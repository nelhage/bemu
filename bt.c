#include "bemu.h"

#include <setjmp.h>

#define BT_STACK_SIZE  (1 << 16)
/* 1MB translation cache */
#define BT_CACHE_SIZE  (1 << 20)

static jmp_buf bt_exit_buf;
uint8_t *bt_stack_base = NULL;
static uint8_t *frag_cache = NULL;
static uint8_t *frag_alloc;
static compiled_frag *frag_hash[256];

/* bt_helper.S */
extern void bt_enter(ccbuff buf) __attribute__((noreturn));
extern void bt_callout(void);
extern void bt_interrupt(void);
extern void bt_start() __attribute__((noreturn));

void bt_translate_and_run(void) __attribute__((noreturn, used));
static void bt_translate_frag(compiled_frag *cfrag, decode_frag *frag);

/*
 * Allocate a compiled_frag out of the frag_cache
 *
 * If may_clear is true, will empty the cache if necessary and will
 * never fail. If `may_clear' is false, it will fail if there is
 * insufficient space in the cache.
 */
static compiled_frag *bt_alloc_cfrag(bool may_clear);
static void bt_clear_cache();
static compiled_frag *bt_find_frag(byteptr PC);
static void bt_insert_frag(compiled_frag *frag);

/* frag cache management */

void bt_clear_cache() {
    LOG("Clearing BT cache",1);
    frag_alloc = frag_cache;
    memset(frag_hash, 0, sizeof frag_hash);
}


compiled_frag *bt_alloc_cfrag(bool may_clear) {
    compiled_frag *frag;
    if(frag_alloc + sizeof *frag > frag_cache + BT_CACHE_SIZE) {
        if(may_clear) {
            bt_clear_cache();
        } else {
            return NULL;
        }
    }

    frag = (compiled_frag*)frag_alloc;
    frag_alloc += sizeof *frag;
    return frag;
}

compiled_frag *bt_find_frag(byteptr PC) {
    compiled_frag *frag = frag_hash[HASH_PC(PC)];
    while(frag) {
        if(frag->start_pc == PC) {
            return frag;
        }
        frag = frag->hash_next;
    }
    return NULL;
}

void bt_insert_frag(compiled_frag *frag) {
    compiled_frag *old = frag_hash[HASH_PC(frag->start_pc)];
    frag_hash[HASH_PC(frag->start_pc)] = frag;
    frag->hash_next = old;
}

/* Actual binary translation */

/*
 * During execution, %ebp points to the base of the regfile
 */

#define LOAD_BETA_REG(buf, breg, x86reg) ({                             \
    uint8_t __reg = (breg);                                             \
    if(__reg == 31) {                                                   \
        X86_XOR_RM32_R32(buf, MOD_REG, x86reg, x86reg);                 \
    } else {                                                            \
        X86_MOV_RM32_R32(buf, MOD_INDIR_DISP8, REG_EBP, x86reg);        \
        X86_DISP8(buf, 4*__reg);                                        \
    }                                                                   \
        });
#define WRITE_BETA_REG(buf, x86reg, breg) ({                            \
    uint8_t __reg = (breg);                                             \
    if(__reg != 31) {                                                   \
        X86_MOV_R32_RM32(buf, x86reg, MOD_INDIR_DISP8, REG_EBP);        \
        X86_DISP8(buf, 4*__reg);                                        \
    }                                                                   \
        });

inline ccbuff bt_translate_arith(ccbuff buf, byteptr pc, bdecode *inst) {
    /* Load %eax with RA, the LHS */
    LOAD_BETA_REG(buf, inst->ra, REG_EAX);
    /* Load %ecx with RB, the RHS */
    LOAD_BETA_REG(buf, inst->rb, REG_ECX);

    switch(inst->opcode) {
    case OP_ADD:
        X86_ADD_RM32_R32(buf, MOD_REG, REG_ECX, REG_EAX);
        break;
    case OP_AND:
        X86_AND_RM32_R32(buf, MOD_REG, REG_ECX, REG_EAX);
        break;
    case OP_OR:
        X86_OR_RM32_R32(buf, MOD_REG, REG_ECX, REG_EAX);
        break;
    case OP_SUB:
        X86_SUB_RM32_R32(buf, MOD_REG, REG_ECX, REG_EAX);
        break;
    case OP_XOR:
        X86_XOR_RM32_R32(buf, MOD_REG, REG_ECX, REG_EAX);
        break;
    case OP_MUL:
        X86_IMUL_RM32_R32(buf, MOD_REG, REG_ECX, REG_EAX);
        break;
    case OP_DIV:
        X86_CDQ(buf);
        X86_IDIV_RM32(buf, MOD_REG, REG_ECX);
        break;
    case OP_SHL:
        X86_SHL_CL_RM32(buf, MOD_REG, REG_EAX);
        break;
    case OP_SHR:
        X86_SHR_CL_RM32(buf, MOD_REG, REG_EAX);
        break;
    case OP_SRA:
        X86_SAR_CL_RM32(buf, MOD_REG, REG_EAX);
        break;
    case OP_CMPLT:
    case OP_CMPEQ:
    case OP_CMPLE:
        /* cmp %ecx, %eax */
        X86_CMP_RM32_R32(buf, MOD_REG, REG_ECX, REG_EAX);
        X86_MOV_IMM32_R32(buf, REG_EAX);
        X86_IMM32(buf, 0x0);
        uint8_t cc = (inst->opcode == OP_CMPLT ? CC_L
                      : (inst->opcode == OP_CMPLE ? CC_LE
                         : CC_Z));
        X86_SETCC_RM8(buf, cc, MOD_REG, REG_EAX);
        break;
    default:
        panic("Unknown arithmetic opcode: 0x%02x\n", inst->opcode);
    }
    /* Load %eax into RC */
    WRITE_BETA_REG(buf, REG_EAX, inst->rc);
    return buf;
}


inline ccbuff bt_translate_arithc(ccbuff buf, byteptr pc, bdecode *inst) {
    uint32_t constant = inst->imm;
    /* Load %eax with RA, the LHS */
    if(inst->ra == 31) {
        X86_XOR_RM32_R32(buf, MOD_REG, REG_EAX, REG_EAX);
    } else {
        X86_MOV_RM32_R32(buf, MOD_INDIR_DISP8, REG_EBP, REG_EAX);
        X86_DISP8(buf, 4*inst->ra);
    }

    switch(inst->opcode) {
    case OP_ADDC:
        X86_ADD_IMM32_RM32(buf, MOD_REG, REG_EAX);
        X86_IMM32(buf, constant);
        break;
    case OP_ANDC:
        X86_AND_IMM32_RM32(buf, MOD_REG, REG_EAX);
        X86_IMM32(buf, constant);
        break;
    case OP_MULC:
        X86_IMUL_IMM32_RM32_R32(buf, MOD_REG, REG_EAX, REG_EAX);
        X86_IMM32(buf, constant);
        break;
    case OP_DIVC:
        X86_CDQ(buf);
        X86_MOV_IMM32_R32(buf, REG_EBX);
        X86_IMM32(buf, constant);
        X86_IDIV_RM32(buf, MOD_REG, REG_EBX);
        break;
    case OP_ORC:
        X86_OR_IMM32_RM32(buf, MOD_REG, REG_EAX);
        X86_IMM32(buf, constant);
        break;
    case OP_SUBC:
        X86_SUB_IMM32_RM32(buf, MOD_REG, REG_EAX);
        X86_IMM32(buf, constant);
        break;
    case OP_XORC:
        X86_XOR_IMM32_RM32(buf, MOD_REG, REG_EAX);
        X86_IMM32(buf, constant);
        break;
    case OP_SHLC:
        X86_SHL_IMM8_RM32(buf, MOD_REG, REG_EAX);
        X86_IMM8(buf, constant & 0x1F);
        break;
    case OP_SHRC:
        X86_SHR_IMM8_RM32(buf, MOD_REG, REG_EAX);
        X86_IMM8(buf, constant & 0x1F);
        break;
    case OP_SRAC:
        X86_SAR_IMM8_RM32(buf, MOD_REG, REG_EAX);
        X86_IMM8(buf, constant & 0x1F);
        break;
    case OP_CMPLTC:
    case OP_CMPEQC:
    case OP_CMPLEC:
        /* cmp $IMM32, %eax */
        X86_CMP_IMM32_RM32(buf, MOD_REG, REG_EAX);
        X86_IMM32(buf, constant);
        X86_MOV_IMM32_R32(buf, REG_EAX);
        X86_IMM32(buf, 0x0);
        uint8_t cc = (inst->opcode == OP_CMPLTC ? CC_L
                      : (inst->opcode == OP_CMPLEC ? CC_LE
                         : CC_Z));
        X86_SETCC_RM8(buf, cc, MOD_REG, REG_EAX);
        break;
    default:
        panic("Unknown constant arithmetic opcode: 0x%02x\n", inst->opcode);
    }
    /* Load %eax into RC */
    WRITE_BETA_REG(buf, REG_EAX, inst->rc);
    return buf;
}

ccbuff bt_translate_other(ccbuff buf, byteptr pc, bdecode *inst) {
    switch(inst->opcode) {
    case OP_ST:
        /* mov regs[RA], %eax
         * add IMM, %eax
         * mov regs[RC], %ecx
         * and $7FFFFFFC, %eax
         * mov %ecx, mem[%eax]
         */
        LOAD_BETA_REG(buf, inst->ra, REG_EAX);

        if(inst->imm) {
            X86_ADD_IMM32_RM32(buf, MOD_REG, REG_EAX);
            X86_IMM32(buf, inst->imm);
        }

        LOAD_BETA_REG(buf, inst->rc, REG_ECX);

        X86_AND_IMM32_RM32(buf, MOD_REG, REG_EAX);
        X86_IMM32(buf, ~(PC_SUPERVISOR | 0x3));

        X86_MOV_R32_RM32(buf, REG_ECX, MOD_INDIR_DISP32, REG_EAX);
        X86_DISP32(buf, beta_mem);
        break;
    case OP_LD:
        /* mov regs[RA], %eax
         * add IMM, %eax
         * and $7FFFFFFC, %eax
         * mov mem[%eax], eax
         * mov $eax, regs[RC]
         */
        LOAD_BETA_REG(buf, inst->ra, REG_EAX);

        if(inst->imm) {
            X86_ADD_IMM32_RM32(buf, MOD_REG, REG_EAX);
            X86_IMM32(buf, inst->imm);
        }

        X86_AND_IMM32_RM32(buf, MOD_REG, REG_EAX);
        X86_IMM32(buf, ~(PC_SUPERVISOR | 0x3));

        X86_MOV_RM32_R32(buf, MOD_INDIR_DISP32, REG_EAX, REG_EAX);
        X86_DISP32(buf, beta_mem);

        WRITE_BETA_REG(buf, REG_EAX, inst->rc);
        break;
    case OP_LDR:
        X86_MOV_RM32_R32(buf, MOD_INDIR, REG_DISP32, REG_EAX);
        X86_DISP32(buf, ((uint32_t)beta_mem) +
                   ((pc + 4 + 4*inst->imm) & ~(PC_SUPERVISOR|0x03)));
        WRITE_BETA_REG(buf, REG_EAX, inst->rc);
        break;
    default:
        panic("Unable to translate opcode: 0x%02x\n", inst->opcode);
    }
    return buf;
}

inline ccbuff bt_translate_inst(ccbuff buf, byteptr pc, bdecode *inst) {
    switch(OP_CLASS(inst->opcode)) {
    case CLASS_ARITH:
        return bt_translate_arith(buf, pc, inst);
    case CLASS_ARITHC:
        return bt_translate_arithc(buf, pc, inst);
    default:
        return bt_translate_other(buf, pc, inst);
    }
}

inline ccbuff bt_translate_prologue(ccbuff buf, byteptr pc) {
    if(!(pc & PC_SUPERVISOR)) {
        X86_TEST_IMM32_RM32(buf, MOD_INDIR, REG_DISP32);
        X86_DISP32(buf, &pending_interrupts);
        X86_IMM32(buf, 0xFFFFFFFF);
        X86_JCC_REL32(buf, CC_NZ);
        X86_REL32(buf, bt_interrupt);
    }
    return buf;
}

inline ccbuff bt_translate_tail(ccbuff buf, byteptr pc, bdecode *inst) {
#define SAVE_PC                                                 \
    if(inst->rc != 31) {                                        \
        X86_MOV_IMM32_RM32(buf, MOD_INDIR_DISP8, REG_EBP);      \
        X86_DISP8(buf, 4*inst->rc);                             \
        X86_IMM32(buf, pc + 4);                                 \
    }

    switch(inst->opcode) {
    case OP_BT:
    case OP_BF:
        /*
         * mov   regs[RA], %ecx
         * mov   (pc+4), %eax
         * mov   %ecx, regs[RC]
         * test  %ecx, %ecx
         * j[n]z .+5
         * mov   (branch pc), %eax
         */
        if(inst->opcode == OP_BF && inst->ra == 31) {
            /* Unconditional branch */
            SAVE_PC;
            X86_MOV_IMM32_R32(buf, REG_EAX);
            X86_IMM32(buf, (pc + 4 + 4*inst->imm) & ~0x03);
        } else {
            LOAD_BETA_REG(buf, inst->ra, REG_ECX);

            X86_MOV_IMM32_R32(buf, REG_EAX);
            X86_IMM32(buf, pc + 4);

            WRITE_BETA_REG(buf, REG_ECX, inst->rc);

            X86_TEST_R32_RM32(buf, REG_ECX, MOD_REG, REG_ECX);

            X86_JCC_REL8(buf, inst->opcode == OP_BT ? CC_Z : CC_NZ);
            X86_DISP8(buf, 5);

            X86_MOV_IMM32_R32(buf, REG_EAX);
            X86_IMM32(buf, (pc + 4 + 4*inst->imm) & ~0x03);
        }
        break;
    case OP_JMP:
        SAVE_PC;
        LOAD_BETA_REG(buf, inst->ra, REG_EAX);
        X86_AND_IMM32_RM32(buf, MOD_REG, REG_EAX);
        X86_IMM32(buf, (pc & PC_SUPERVISOR) | ~(PC_SUPERVISOR|0x3));
        break;
    default:
        /* If we made it here, it's an ILLOP */
        ASSERT(!decode_valid(inst));
        /* XP = PC + 4*/
        X86_MOV_IMM32_RM32(buf, MOD_INDIR_DISP8, REG_EBP);
        X86_DISP8(buf, 4*XP);
        X86_IMM32(buf, pc + 4);

        X86_MOV_IMM32_R32(buf, REG_EAX);
        X86_IMM32(buf, ISR_ILLOP);
    }
    return buf;
}

void bt_translate_frag(compiled_frag *cfrag, decode_frag *frag) {
    ccbuff buf = cfrag->code;
    byteptr pc = frag->start_pc;
    int i;

    LOG("Translating %d instruction%s at 0x%08x",
        frag->ninsts, (frag->tail?" (plus tail)" : ""), frag->start_pc);

    cfrag->start_pc = frag->start_pc;

    buf = bt_translate_prologue(buf, pc);

    for(i = 0; i < frag->ninsts; i++) {
        buf = bt_translate_inst(buf, pc, &frag->insts[i]);
        pc += 4;
    }
    if(frag->tail) {
        buf = bt_translate_tail(buf, pc, &frag->insts[i]);
    } else {
        /*
         * default epilogue -- move emulated PC to %eax and jump to
         * bt_callout
         */
        X86_MOV_IMM32_R32(buf, REG_EAX);
        X86_IMM32(buf, pc);
    }
    X86_JMP_REL32(buf);
    X86_REL32(buf, bt_callout);
}

void bt_run() {
    if(setjmp(bt_exit_buf)) {
        return;
    }

    if(!bt_stack_base) {
        bt_stack_base = mmap(NULL, BT_STACK_SIZE,
                             PROT_READ|PROT_WRITE,
                             MAP_PRIVATE|MAP_ANONYMOUS|MAP_GROWSDOWN, -1, 0);
        if(bt_stack_base == MAP_FAILED) {
            perror("mmap");
            panic("Unable to allocate BT stack!\n");
        }
    }
    bt_stack_base += BT_STACK_SIZE;

    if(!frag_cache) {
        frag_cache = mmap(NULL, BT_CACHE_SIZE,
                          PROT_READ|PROT_WRITE|PROT_EXEC,
                          MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
        if(frag_cache == MAP_FAILED) {
            perror("mmap");
            panic("Could not allocate BT cache!");
        }
        bt_clear_cache();
    }

    bt_start();
}

inline bool bt_can_translate(bdecode *inst) {
    if(!decode_valid(inst)) {
        return 0;
    }
    return OP_CLASS(inst->opcode) == CLASS_ARITH ||
        OP_CLASS(inst->opcode) == CLASS_ARITHC ||
        inst->opcode == OP_ST ||
        inst->opcode == OP_LD ||
        inst->opcode == OP_LDR;
}

void bt_translate_and_run() {
    compiled_frag *cfrag;
    decode_frag frag;
    uint32_t inst;
    uint32_t pc;
    int i = 0;


    while(TRUE) {
        pc = CPU.PC;

        cfrag = bt_find_frag(pc);

        if(!cfrag) {
            frag.start_pc = pc;
            frag.tail = FALSE;
            for(i = 0; i < MAX_FRAG_SIZE; i++) {
                inst = beta_read_mem32(pc);
                decode_op(inst, &frag.insts[i]);
                if(!bt_can_translate(&frag.insts[i])) {
                    if(frag.insts[i].opcode != OP_CALLOUT) {
                        frag.tail = TRUE;
                    }
                    break;
                }
                pc += 4;
            }
            if (i > 0) {
                frag.ninsts = i;
                cfrag = bt_alloc_cfrag(TRUE);
                bt_translate_frag(cfrag, &frag);
                bt_insert_frag(cfrag);
            }
        } else {
            LOG("Cache HIT at pc 0x%08x", pc);
        }

        if(cfrag) {
            bt_enter(cfrag->code);
        } else {
            bcpu_execute_one(&frag.insts[0]);
            if(CPU.halt) {
                longjmp(bt_exit_buf, 1);
            }
        }
    }
}
