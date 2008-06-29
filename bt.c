#include "bemu.h"

#include <setjmp.h>

#define BT_STACK_SIZE  (1 << 16)
/* 1MB translation cache */
#define BT_CACHE_SIZE  (1 << 20)

static jmp_buf bt_exit_buf;
static uint8_t *bt_stack_base = NULL;
static uint8_t *frag_cache = NULL;
static uint8_t *frag_alloc;
static compiled_frag *frag_hash[256];

static void bt_enter(ccbuff buf) __attribute__((noreturn));
static void bt_translate_and_run(void) __attribute__((noreturn));
static void bt_translate_frag(compiled_frag *cfrag, decode_frag *frag);
extern void bt_callout(void);

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
}

compiled_frag *bt_find_frag(byteptr PC) {
    compiled_frag *frag = frag_hash[HASH_PC(PC)];
    while(frag) {
        if(frag->start_pc = PC) {
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

/* During execution, %ebp points to the base of the regfile */
inline ccbuff bt_translate_arith(ccbuff buf, byteptr pc, bdecode *inst) {
    /* Load %eax with RA, the LHS */
    if(inst->ra == 31) {
        X86_XOR_RM32_R32(buf, MOD_REG, REG_EAX, REG_EAX);
    } else {
        X86_MOV_RM32_R32(buf, MOD_INDIR_DISP8, REG_EBP, REG_EAX);
        X86_DISP8(buf, 4*inst->ra);
    }
    /* Load %ecx with RB, the RHS */
    if(inst->rb == 31) {
        X86_XOR_RM32_R32(buf, MOD_REG, REG_ECX, REG_ECX);
    } else {
        X86_MOV_RM32_R32(buf, MOD_INDIR_DISP8, REG_EBP, REG_ECX);
        X86_DISP8(buf, 4*inst->rb);
    }

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
    if(inst->rc != 31) {
        X86_MOV_R32_RM32(buf, REG_EAX, MOD_INDIR_DISP8, REG_EBP);
        X86_DISP8(buf, 4*inst->rc);
    }
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
    if(inst->rc != 31) {
        X86_MOV_R32_RM32(buf, REG_EAX, MOD_INDIR_DISP8, REG_EBP);
        X86_DISP8(buf, 4*inst->rc);
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
         panic("Unable to translate instruction class 0x%x", OP_CLASS(inst->opcode));
    }
}

void bt_translate_frag(compiled_frag *cfrag, decode_frag *frag) {
    LOG("Translating %d instruction(s) at 0x%08x", frag->ninsts, frag->start_pc);

    ccbuff buf = cfrag->code;
    byteptr pc = frag->start_pc;
    cfrag->start_pc = frag->start_pc;
    int i;
    for(i = 0; i < frag->ninsts; i++) {
        buf = bt_translate_inst(buf, pc, &frag->insts[i]);
        pc += 4;
    }
    /*
     * epilogue -- move emulated PC to %eax and jump to bt_callout
     */
    X86_MOV_IMM32_R32(buf, REG_EAX);
    X86_IMM32(buf, pc);
    X86_JMP_REL32(buf);
    X86_IMM32(buf, ((uint8_t*)bt_callout - (buf + 4)));
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

    asm volatile("mov %0, %%esp\n\t"
                 "jmp bt_translate_and_run"
                 : : "g"(bt_stack_base + BT_STACK_SIZE));
}

inline bool bt_can_translate(bdecode *inst) {
    return (OP_CLASS(inst->opcode) == CLASS_ARITH ||
            OP_CLASS(inst->opcode) == CLASS_ARITHC) &&
        decode_valid(inst);
}

void bt_translate_and_run() {
    compiled_frag *cfrag;
    decode_frag frag;
    uint32_t inst;
    uint32_t pc;
    int i = 0;


    while(TRUE) {
        if(CPU.halt) {
            longjmp(bt_exit_buf, 1);
        }
        pc = CPU.PC;

        cfrag = bt_find_frag(pc);

        if(!cfrag) {
            frag.start_pc = pc;

            while(i < MAX_FRAG_SIZE) {
                inst = beta_read_mem32(pc);
                decode_op(inst, &frag.insts[i]);
                if(!bt_can_translate(&frag.insts[i])) {
                    break;
                }
                i++;
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
            bcpu_step_one();
        }
    }
}

void bt_enter(ccbuff buf) {
    asm volatile("mov %0, %%ebp\n\t"
                  "jmp *%1" : : "g"(CPU.regs), "r"(buf));
    while(1); /* Satisfy the compiler that we don't return */
}

#define STRINGIFY(x)  _STRINGIFY(x)
#define _STRINGIFY(x) #x

asm("\n"
    "bt_callout:\n\t"
    "mov %eax, CPU\n\t"
    "mov bt_stack_base, %eax\n\t"
    "addl $" STRINGIFY(BT_STACK_SIZE) ", %eax\n\t"
    "mov %eax, %esp\n\t"
    "jmp bt_translate_and_run\n");
