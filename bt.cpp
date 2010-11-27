#include "bemu.h"

#include <setjmp.h>
#include <signal.h>
#include <sys/ucontext.h>

#define PAGE_SIZE       0x1000
#define PAGE_SHIFT      12

#ifdef __linux__
#include <sys/syscall.h>
#include <asm/ldt.h>
#include <unistd.h>

int modify_ldt(int func, struct user_desc *ptr, unsigned long bytes) {
    return syscall(SYS_modify_ldt, func, ptr, bytes);
}
#elif defined(__APPLE__)
#include <architecture/i386/table.h>
#include <i386/user_ldt.h>
#endif

#define BT_STACK_SIZE  (1 << 16)

/* 1MB translation code cache */
#define BT_CACHE_SIZE  (1 << 20)
#define BT_CACHE_FRAGS (1 << 12)
#define BT_FAULT_COUNT BT_CACHE_FRAGS

static jmp_buf bt_exit_buf;
uint8_t *bt_stack_base = NULL;
static compiled_frag frag_cache[BT_CACHE_FRAGS];
static compiled_frag *frag_alloc;
static uint8_t *frag_code_cache = NULL;
static uint8_t *frag_code_alloc;
static fault_entry fault_table[BT_FAULT_COUNT];
static fault_entry *fault_table_alloc;
compiled_frag *bt_frag_hash[256];

/* bt_helper.S */
extern "C" void bt_enter(ccbuff buf) __attribute__((noreturn));
extern "C" void bt_continue(void);
extern "C" void bt_continue_chain(void);
extern "C" void bt_continue_ic(void);
extern "C" void bt_interrupt(void);

extern "C" void bt_translate_and_run(beta_cpu *cpu, uint32_t used, ccbuff chainptr) __attribute__((noreturn, used, regparm(3)));
static ccbuff bt_translate_frag(compiled_frag *cfrag, decode_frag *frag);

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
    LOG("Clearing BT cache");
    frag_alloc = frag_cache;
    frag_code_alloc = frag_code_cache;
    fault_table_alloc = fault_table;

    memset(bt_frag_hash, 0, sizeof bt_frag_hash);
}


/*
 * We return a compiled_frag with a 'code' pointer into the compiled
 * code cache. `bt_translate_and_run' is responsible for updating
 * `frag_alloc' after it has generated code into this pointer.
 */
compiled_frag *bt_alloc_cfrag(bool may_clear) {
    compiled_frag *frag;
    if(frag_alloc == (frag_cache + BT_CACHE_FRAGS) ||
       frag_code_alloc + CCBUFF_MAX_SIZE >=
       frag_code_cache + BT_CACHE_SIZE) {
        if(may_clear) {
            bt_clear_cache();
        } else {
            return NULL;
        }
    }

    frag = frag_alloc++;
    frag->code = frag_code_alloc;

    return frag;
}

compiled_frag *bt_find_frag(byteptr PC) {
    compiled_frag *frag = bt_frag_hash[HASH_PC(PC)];
    while(frag) {
        if(frag->start_pc == PC) {
            return frag;
        }
        frag = frag->hash_next;
    }
    return NULL;
}

fault_entry *bt_save_fault_entry(X86Assembler *cc, byteptr pc) {
    fault_entry *f = fault_table_alloc++;
    if (f == fault_table + BT_FAULT_COUNT)
        panic("Unable to allocate space for a fault table entry!");
    f->eip = cc->eip();
    f->pc  = pc;
    return f;
}

void bt_insert_frag(compiled_frag *frag) {
    compiled_frag *old = bt_frag_hash[HASH_PC(frag->start_pc)];
    bt_frag_hash[HASH_PC(frag->start_pc)] = frag;
    frag->hash_next = old;
}

static int bt_alloc_segdesc(uint32_t base, uint32_t pages);

#ifdef __linux__
static int bt_alloc_segdesc(uint32_t base, uint32_t pages)
{
    /* FIXME to actually allocate an unused descriptor */
    int segment = 0;
    struct user_desc segdesc = {};
    segdesc.entry_number    = segment;
    segdesc.base_addr       = base;
    segdesc.limit           = pages;
    segdesc.seg_32bit       = 0;
    segdesc.contents        = 0;
    segdesc.read_exec_only  = 0;
    segdesc.limit_in_pages  = 1;
    segdesc.seg_not_present = 0;
    segdesc.useable         = 0;

    if(modify_ldt(1, &segdesc, sizeof(segdesc)) < 0) {
        perror("modify_ldt");
        panic("Unable to modify_ldt to initialize BCPU LDT!");
    }

    return segment;
}
#elif defined(__APPLE__)
static  int bt_alloc_segdesc(uint32_t base, uint32_t pages)
{
    int segment;
    union ldt_entry desc = {
        .data = {
            .limit00 = (pages & 0xffff),
            .base00  = (base  & 0xffff),
            .base16  = (base  >> 16) & 0xff,
            .type    = 0x12,
            .dpl     = 0x3,
            .present = 1,
            .limit16 = (pages >> 16) & 0xff,
            .granular = 1,
            .base24  = base >> 24
        }
    };

    segment = i386_set_ldt(LDT_AUTO_ALLOC, &desc, 1);
    if(segment < 0) {
        perror("i386_set_ldt");
        panic("Unable to initialize the guest CPU LDT!");
    }
    return segment;
}
#endif

int bt_setup_cpu_segment(beta_cpu *cpu) {
    uint32_t pages;

    pages = (((cpu->memsize + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1)) >> PAGE_SHIFT) - 1;
    return bt_alloc_segdesc((uint32_t)cpu->memory, pages);
}

void bt_segv(int signo UNUSED, siginfo_t *info, void *ctx) {
    ucontext_t *uctx = (ucontext_t*)ctx;
    uint8_t *eip = (uint8_t*)uctx->uc_mcontext.gregs[REG_EIP];
    if (eip >= frag_code_cache && eip < frag_code_alloc) {
        fault_entry *f;
        for (f = fault_table; f < fault_table_alloc; f++)
            if (f->eip == eip)
                break;
        if (f != fault_table_alloc) {
            beta_cpu *cpu = (beta_cpu*)uctx->uc_mcontext.gregs[REG_EBP];
            bdecode decode;
            byteptr addr;
            decode_op(cpu->read_mem32(f->pc), &decode);
            switch (decode.opcode) {
            case OP_LD:
            case OP_ST:
                addr = uctx->uc_mcontext.gregs[REG_EAX];
                break;
            case OP_LDR:
                /* Skip the FS prefix, the opcode, and the modrm byte to find
                   the 32-bit literal displacement */
                addr = *(uint32_t*)(eip + 3);
                break;
            default:
                panic("Fault from a non-memory opcode?");
                break;
            }
            panic("Illegal memory reference (PC=%08x) %08x:\n"
                  "  %s", f->pc, addr, pp_decode(&decode));
        }
    }
    panic("[%08x] Segmentation fault", eip)
}

void bt_setup_segv_handler() {
    struct sigaction action = {};
    action.sa_sigaction = bt_segv;
    action.sa_flags     = SA_SIGINFO;

    sigemptyset(&action.sa_mask);
    if(sigaction(SIGSEGV, &action, NULL) < 0) {
        perror("sigaction");
        panic("Unable to set up SEGV signal handler!");
    }
}

/* Actual binary translation */

/*
 * How this all works
 * ==================
 *
 * We translate in units of "fragments", which are `MAX_FRAG_SIZE'
 * instructions, or end at transfer-of-control-flow. Fragments are
 * inserted into the `frag_cache' as they are translated, and entered
 * by a call to bt_enter(frag->code). Frags exit to either
 * `bt_continue' or `bt_continue_chain' (see CHAINING for more
 * information)
 *
 * Register use
 * ------------
 * During execution of a frag, %ebp points to the global beta_cpu
 * being executed. We do no register allocation, and back all of the
 * \Beta's registers with memory. At frag entry and exit (but *not*
 * during frag execution), %eax holds the emulated program counter;
 * `bt_enter' and `bt_continue' in bt_helper.S are responsible for
 * loading/restoring CPU.PC. Other than that, no registers are
 * special-purposed at the moment.
 *
 * Chaining
 * --------
 * For performance, we can "chain" fragments that always follow each
 * other, by overwriting the translated fragment with a jump directly
 * to the next fragment.
 *
 * bt_translate_and_run, the main loop of the bt engine, accepts a
 * `chainptr' argument. If this argument is non-NULL, it points
 * immediately *after* a CALL instruction that should be overwritten
 * by a JMP to the fragment translated from CPU.PC, if appropriate.
 *
 * As mentioned, frags can exit either to bt_continue or
 * bt_continue_chain. Frags exiting to bt_continue_chain by a CALL,
 * which leaves the return address on the stack, where it will be
 * taken as the `chainptr' argument when bt_continue subsequently
 * CALLs bt_translate_and_run. `bt_continue' simply pushes a NULL
 * chainptr and falls through to bt_continue_chain.
 *
 * Inline JMP target caching
 * -------------------------
 * In addition to exits bt_continue and bt_continue_chain, JMP
 * instructions can exit to bt_continue_ic. bt_continue_ic is used to
 * implement an inline cache of JMP targets. bt_continue_ic overwrites
 * the call with a jmp directly to compiled code, but instead of
 * jumping to the start of the ccbuf, it jumps 12 bytes before it, to
 * the following check:
 *
 *  cmp $CFRAG_PC, %eax
 *  jne bt_continue
 *
 * This has the effect that future JMPs to the same target as the
 * first will fall through directly into the ccbuf, instead of having
 * to go through the frag cache. If the JMP target has changed,
 * however, we call back into bt_continue to do the full hash lookup
 * and compilation if necessary.
 *
 * This is implemented by writing the above prefix (the length of
 * which is is defined to be `PC_CHECK_SIZE') before translating every
 * ccbuf, since there is no good way to tell which ccbufs may be
 * targets of indirect jumps. In addition, bt_translate_and_run takes
 * an extra parameter, `exact', which controls whether it should chain
 * to cfrag->code, or `PC_CHECK_SIZE' bytes before
 * it. bt_continue_chain sets the flag, bt_continue_ic does not.
 */

static X86ReferenceIndirect8 bt_register_address(uint8_t reg) {
    return X86Mem(X86EBP, (uint8_t)(offsetof(beta_cpu, regs)
                                    + 4*reg));
}

void bt_load_reg(X86Assembler *cc, uint8_t breg, X86Register reg) {
    if (breg == 31)
        cc->xor_(reg, reg);
    else
        cc->mov(bt_register_address(breg), reg);
}

void bt_store_reg(X86Assembler *cc, X86Register reg, uint8_t breg) {
    if (breg != 31)
        cc->mov(reg, bt_register_address(breg));
}

inline void bt_translate_arith(X86Assembler *buf, byteptr pc UNUSED, bdecode *inst) {
    /* Load %eax with RA, the LHS */
    bt_load_reg(buf, inst->ra, X86EAX);
    /* Load %ecx with RB, the RHS */
    bt_load_reg(buf, inst->rb, X86ECX);

    switch(inst->opcode) {
    case OP_ADD:
        buf->add_(X86ECX, X86EAX);
        break;
    case OP_AND:
        buf->and_(X86ECX, X86EAX);
        break;
    case OP_OR:
        buf->or_(X86ECX, X86EAX);
        break;
    case OP_SUB:
        buf->sub_(X86ECX, X86EAX);
        break;
    case OP_XOR:
        buf->xor_(X86ECX, X86EAX);
        break;
    case OP_MUL:
        X86_IMUL_RM32_R32(buf, MOD_REG, X86_ECX, X86_EAX);
        break;
    case OP_DIV:
        X86_CDQ(buf);
        X86_IDIV_RM32(buf, MOD_REG, X86_ECX);
        break;
    case OP_SHL:
        X86_SHL_CL_RM32(buf, MOD_REG, X86_EAX);
        break;
    case OP_SHR:
        X86_SHR_CL_RM32(buf, MOD_REG, X86_EAX);
        break;
    case OP_SRA:
        X86_SAR_CL_RM32(buf, MOD_REG, X86_EAX);
        break;
    case OP_CMPLT:
    case OP_CMPEQ:
    case OP_CMPLE: {
        /* cmp %ecx, %eax */
        buf->cmp(X86ECX, X86EAX);
        buf->mov(0x00u, X86EAX);
        uint8_t cc = (inst->opcode == OP_CMPLT ? CC_L
                      : (inst->opcode == OP_CMPLE ? CC_LE
                         : CC_Z));
        X86_SETCC_RM8(buf, cc, MOD_REG, X86_EAX);
        break;
    }
    default:
        panic("Unknown arithmetic opcode: 0x%02x\n", inst->opcode);
    }
    /* Load %eax into RC */
    bt_store_reg(buf, X86EAX, inst->rc);
}


inline void bt_translate_arithc(X86Assembler *buf, byteptr pc UNUSED, bdecode *inst) {
    uint32_t constant = inst->imm;
    /* Load %eax with RA, the LHS */
    bt_load_reg(buf, inst->ra, X86EAX);

    switch(inst->opcode) {
    case OP_ADDC:
        buf->add_(constant, X86EAX);
        break;
    case OP_ANDC:
        buf->and_(constant, X86EAX);
        break;
    case OP_MULC:
        X86_IMUL_IMM32_RM32_R32(buf, MOD_REG, X86_EAX, X86_EAX);
        X86_IMM32(buf, constant);
        break;
    case OP_DIVC:
        X86_CDQ(buf);
        X86_MOV_IMM32_R32(buf, X86_EBX);
        X86_IMM32(buf, constant);
        X86_IDIV_RM32(buf, MOD_REG, X86_EBX);
        break;
    case OP_ORC:
        buf->or_(constant, X86EAX);
        break;
    case OP_SUBC:
        buf->sub_(constant, X86EAX);
        break;
    case OP_XORC:
        buf->xor_(constant, X86EAX);
        break;
    case OP_SHLC:
        X86_SHL_IMM8_RM32(buf, MOD_REG, X86_EAX);
        X86_IMM8(buf, constant & 0x1F);
        break;
    case OP_SHRC:
        X86_SHR_IMM8_RM32(buf, MOD_REG, X86_EAX);
        X86_IMM8(buf, constant & 0x1F);
        break;
    case OP_SRAC:
        X86_SAR_IMM8_RM32(buf, MOD_REG, X86_EAX);
        X86_IMM8(buf, constant & 0x1F);
        break;
    case OP_CMPLTC:
    case OP_CMPEQC:
    case OP_CMPLEC: {
        /* cmp $IMM32, %eax */
        buf->cmp(constant, X86EAX);
        buf->mov(0u, X86EAX);

        uint8_t cc = (inst->opcode == OP_CMPLTC ? CC_L
                      : (inst->opcode == OP_CMPLEC ? CC_LE
                         : CC_Z));
        X86_SETCC_RM8(buf, cc, MOD_REG, X86_EAX);
        break;
    }
    default:
        panic("Unknown constant arithmetic opcode: 0x%02x\n", inst->opcode);
    }
    /* Load %eax into RC */
    bt_store_reg(buf, X86EAX, inst->rc);
}

inline void bt_translate_other(X86Assembler *buf, byteptr pc, bdecode *inst) {
    switch(inst->opcode) {
    case OP_ST:
        /* mov regs[RA], %eax
         * add IMM, %eax
         * mov regs[RC], %ecx
         * and $7FFFFFFC, %eax
         * mov %ecx, mem[%eax]
         */
        bt_load_reg(buf, inst->ra, X86EAX);

        if(inst->imm) {
            buf->add_((uint32_t)inst->imm, X86EAX);
        }

        bt_load_reg(buf, inst->rc, X86ECX);

        buf->and_((uint32_t)~(PC_SUPERVISOR | 0x3), X86EAX);

        bt_save_fault_entry(buf, pc);
        buf->byte(PREFIX_SEG_FS);
        buf->mov(X86ECX, X86Mem(X86EAX));

        break;
    case OP_LD:
        /* mov regs[RA], %eax
         * add IMM, %eax
         * and $7FFFFFFC, %eax
         * mov mem[%eax], eax
         * mov $eax, regs[RC]
         */
        bt_load_reg(buf, inst->ra, X86EAX);

        if(inst->imm)
            buf->add_((uint32_t)inst->imm, X86EAX);

        buf->and_((uint32_t)~(PC_SUPERVISOR | 0x3), X86EAX);

        bt_save_fault_entry(buf, pc);
        buf->byte(PREFIX_SEG_FS);
        buf->mov(X86Mem(X86EAX), X86EAX);

        bt_store_reg(buf, X86EAX, inst->rc);
        break;
    case OP_LDR:

        bt_save_fault_entry(buf, pc);
        buf->byte(PREFIX_SEG_FS);
        buf->mov(X86Mem(((pc + 4 + 4*inst->imm) & ~(PC_SUPERVISOR|0x03))),
                 X86EAX);

        bt_store_reg(buf, X86EAX, inst->rc);
        break;
    default:
        panic("Unable to translate opcode: 0x%02x\n", inst->opcode);
    }
}

inline void bt_translate_inst(X86Assembler *pbuf, byteptr pc, bdecode *inst) {
    switch(OP_CLASS(inst->opcode)) {
    case CLASS_ARITH:
        bt_translate_arith(pbuf, pc, inst);
        break;
    case CLASS_ARITHC:
        bt_translate_arithc(pbuf, pc, inst);
        break;
    default:
        bt_translate_other(pbuf, pc, inst);
        break;
    }
    return;
}

/*
 * At the start of every user-mode fragment, we check for pending
 * interrupts and jump out to `bt_interrupt' to handle them if so.
 */
inline void bt_translate_prologue(X86Assembler *buf, byteptr pc) {
    buf->cmp((uint32_t)pc, X86EAX);
    X86_JCC_REL32(buf, CC_NZ);
    X86_REL32(buf, bt_continue);

    if(!(pc & PC_SUPERVISOR)) {
        buf->test((uint32_t)-1,
                  X86Mem(X86EBP, offsetof(beta_cpu, pending_interrupts)));
        X86_JCC_REL32(buf, CC_NZ);
        X86_REL32(buf, bt_interrupt);
    }
}

extern "C" {
    static void bt_step_one(beta_cpu *cpu) __attribute__((used, regparm(1)));
    static void bt_step_one(beta_cpu *cpu) {
        cpu->step_one();
    }

    void bt_process_interrupt(beta_cpu *cpu) __attribute__((used, regparm(1)));
    void bt_process_interrupt(beta_cpu *cpu) {
        cpu->process_interrupt();
    }
};

inline void bt_translate_interp(X86Assembler *buf, byteptr pc) {
    // Align %esp on a 16-byte boundary to placate OS X
    buf->sub_(4u, X86ESP);

    // Save the PC into CPU.PC
    buf->mov(pc, X86Mem(X86EBP, offsetof(beta_cpu, PC)));

    buf->mov(X86EBP, X86EAX);
    // Call bt_step_one
    X86_CALL_REL32(buf);
    X86_REL32(buf, bt_step_one);

    buf->add_(4u, X86ESP);

    // Save CPU.PC back into %eax
    buf->mov(X86Mem(X86EBP, offsetof(beta_cpu, PC)), X86EAX);

    X86_CALL_REL32(buf);
    X86_REL32(buf, bt_continue_chain);
}

inline void bt_translate_tail(X86Assembler *buf, byteptr pc, bdecode *inst) {
#define SAVE_PC                                                 \
    if(inst->rc != 31) {                                        \
        buf->mov(pc + 4, bt_register_address(inst->rc));        \
    }

    if(profile_instructions && inst->opcode != OP_CALLOUT) {
        X86_INC_RM32(buf, MOD_INDIR_DISP32, X86_EBP);
        X86_DISP32(buf, offsetof(beta_cpu, opcode_counts) + inst->opcode * sizeof(uint32_t));
    }

    switch(inst->opcode) {
    case OP_BT:
    case OP_BF:
        if(inst->ra == 31) {
            /* Unconditional branch */
            SAVE_PC;
            X86_MOV_IMM32_R32(buf, X86_EAX);
            if (inst->opcode == OP_BF) {
                X86_IMM32(buf, (pc + 4 + 4*inst->imm) & ~0x03);
            } else {
                X86_IMM32(buf, pc + 4);
            }

            X86_CALL_REL32(buf);
            X86_REL32(buf, bt_continue_chain);
        } else {
            /*
             * cmp   $0, regs[RA]
             * mov   $(pc+4), regs[RC]
             * j[n]z .+10
             * mov   $(pc+4), %eax          ; 5 bytes
             * call  bt_continue_chain      ; 5 bytes
             * mov   $(branch pc), CPU.PC
             * call  bt_continue_chain
             */

            buf->cmp((uint32_t)0, bt_register_address(inst->ra));

            SAVE_PC;

            X86_JCC_REL8(buf, inst->opcode == OP_BT ? CC_NZ : CC_Z);
            X86_DISP8(buf, 10);

            buf->mov(pc + 4, X86EAX);

            X86_CALL_REL32(buf);
            X86_REL32(buf, bt_continue_chain);

            buf->mov((pc + 4 + 4*inst->imm) & ~0x03, X86EAX);

            X86_CALL_REL32(buf);
            X86_REL32(buf, bt_continue_chain);
        }
        break;
    case OP_JMP:
        SAVE_PC;
        bt_load_reg(buf, inst->ra, X86EAX);

        buf->and_((pc & PC_SUPERVISOR) | ~(PC_SUPERVISOR|0x3), X86EAX);

        X86_CALL_REL32(buf);
        X86_REL32(buf, bt_continue_ic);
        break;
    case OP_CALLOUT:
        bt_translate_interp(buf, pc);
        break;
    default:
        /* If we made it here, it's an ILLOP */
        ASSERT(!decode_valid(inst));
        /* XP = PC + 4*/
        buf->mov(pc + 4, bt_register_address(XP));
        buf->mov(ISR_ILLOP, X86EAX);

        X86_CALL_REL32(buf);
        X86_REL32(buf, bt_continue_chain);
    }
}

ccbuff bt_translate_frag(compiled_frag *cfrag, decode_frag *frag) {
    X86Assembler buf(cfrag->code);
    byteptr pc = frag->start_pc;
    int i;

    LOG("Translating %d instruction%s at 0x%08x",
        frag->ninsts, (frag->tail?" (plus tail)" : ""), frag->start_pc);

    cfrag->start_pc = frag->start_pc;

    bt_translate_prologue(&buf, pc);

    for(i = 0; i < frag->ninsts; i++) {
        if (profile_instructions) {
            X86_INC_RM32(&buf, MOD_INDIR_DISP32, X86_EBP);
            X86_DISP32(&buf, offsetof(beta_cpu, opcode_counts) +
                       frag->insts[i].opcode * sizeof(uint32_t));
        }

        bt_translate_inst(&buf, pc, &frag->insts[i]);
        pc += 4;
    }

    /* Update CPU.inst_count */

    buf.add_((uint32_t)(frag->ninsts + (frag->tail ? 1 : 0)),
             X86Mem(X86EBP, offsetof(beta_cpu, inst_count)));

    if(frag->tail) {
        bt_translate_tail(&buf, pc, &frag->insts[i]);
    } else {
        /*
         * default epilogue -- save emulated PC and jump to bt_continue_chain
         */
        buf.mov((uint32_t)pc, X86EAX);

        X86_CALL_REL32(&buf);
        X86_REL32(&buf, bt_continue_chain);
    }
    return buf.eip();
}

void bt_run(beta_cpu *cpu) {
    if(setjmp(bt_exit_buf)) {
        return;
    }

    if(!bt_stack_base) {
        bt_stack_base = (uint8_t*)valloc(BT_STACK_SIZE);
        if(bt_stack_base == NULL) {
            panic("Unable to allocate BT stack!\n");
        }
    }
    bt_stack_base += BT_STACK_SIZE;

    if(!frag_code_cache) {
        frag_code_cache = (uint8_t*)valloc(BT_CACHE_SIZE);
        if(frag_code_cache == NULL) {
            panic("Could not allocate BT cache!");
        }
        if(mprotect(frag_code_cache, BT_CACHE_SIZE, PROT_READ|PROT_WRITE|PROT_EXEC)) {
            perror("mprotect");
            panic("Unable to mprotect() the BT cache!");
        }
        bt_clear_cache();
    }

    cpu->segment = bt_setup_cpu_segment(cpu);
    bt_setup_segv_handler();

    bt_translate_and_run(cpu, 1, NULL);
}

inline bool bt_ends_frag(bdecode *inst) {
    if(!decode_valid(inst)) {
        return 1;
    }
    return inst->opcode == OP_JMP ||
        inst->opcode == OP_BT ||
        inst->opcode == OP_BF ||
        inst->opcode == OP_CALLOUT;
}

void bt_translate_and_run(beta_cpu *cpu, uint32_t exact, ccbuff chainptr) {
    compiled_frag *cfrag;
    decode_frag frag;
    uint32_t inst;
    uint32_t pc;
    int i = 0;

    if(cpu->halt) {
        longjmp(bt_exit_buf, 1);
    }

    pc = cpu->PC;

    cfrag = bt_find_frag(pc);

    if(!cfrag) {
        frag.start_pc = pc;
        frag.tail = false;
        for(i = 0; i < MAX_FRAG_SIZE; i++) {
            inst = cpu->read_mem32(pc);
            decode_op(inst, &frag.insts[i]);
            if(bt_ends_frag(&frag.insts[i])) {
                frag.tail = true;
                break;
            }
            pc += 4;
        }
        frag.ninsts = i;
        cfrag = bt_alloc_cfrag(true);
        if (chainptr == cfrag->code) {
            cfrag->code -= 5;
            chainptr = NULL;
            LOG("Chaining adjacent frags at pc=0x%08x", frag.start_pc);
        }

        frag_code_alloc = bt_translate_frag(cfrag, &frag);
        cfrag->code += PC_CHECK_SIZE;
        LOG("Compiled %d instructions at 0x%08x.", frag.ninsts, frag.start_pc);
#if DEBUG >= 2
        unsigned char *p = cfrag->code;
        printf("code: ");
        while (p < frag_code_alloc)
            printf("%02x ", *p++);
        printf("\n");
#endif

        bt_insert_frag(cfrag);
    } else {
        LOG("Cache HIT at pc 0x%08x", pc);
    }

    if(chainptr) {
        chainptr -= 5;
        X86Assembler cc(chainptr);
        X86_JMP_REL32(&cc);
        X86_REL32(&cc, (exact ? cfrag->code : (cfrag->code - PC_CHECK_SIZE)));
        LOG("Chaining to frag 0x%08x", cfrag->start_pc);
    }

    __asm__("movw %%ax, %%fs\n" :: "a"(cpu->segment<<3|0x7));
    bt_enter(cfrag->code);
}
