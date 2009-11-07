#include "bemu.h"

#include <setjmp.h>
#include <signal.h>
#include <ucontext.h>

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

static jmp_buf bt_exit_buf;
uint8_t *bt_stack_base = NULL;
static compiled_frag frag_cache[BT_CACHE_FRAGS];
static compiled_frag *frag_alloc;
static uint8_t *frag_code_cache = NULL;
static uint8_t *frag_code_alloc;
compiled_frag *bt_frag_hash[256];

/* bt_helper.S */
extern void bt_enter(ccbuff buf) __attribute__((noreturn));
extern void bt_continue(void);
extern void bt_continue_chain(void);
extern void bt_continue_ic(void);
extern void bt_interrupt(void);

void bt_translate_and_run(uint32_t used, ccbuff chainptr) __attribute__((noreturn, used));
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
    struct user_desc segdesc = {
        .entry_number    = segment,
        .base_addr       = base,
        .limit           = pages,
        .seg_32bit       = 0,
        .contents        = 0,
        .read_exec_only  = 0,
        .limit_in_pages  = 1,
        .seg_not_present = 0,
        .useable         = 0,
    };

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

int bt_setup_cpu_segment() {
    uint32_t pages;

    pages = (((CPU.memsize + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1)) >> PAGE_SHIFT) - 1;
    return bt_alloc_segdesc((uint32_t)CPU.memory, pages);
}

void bt_segv(int signal UNUSED, siginfo_t *info UNUSED, void *ctx UNUSED) {
    panic("Illegal memory reference (UNKNOWN ADDRESS)");
}

void bt_setup_segv_handler() {
    struct sigaction action = {
        .sa_sigaction = bt_segv,
        .sa_flags     = SA_SIGINFO
    };
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
 *  cmp $CFAG_PC, %eax
 *  jne bt_continue
 *
 * This has the effect that future JMPs to the same target as the
 * first will fall through directly into the ccbuf, instead of having
 * to go through the frag cache. If the JMP target has changed,
 * however, we call back into bt_continue to do the full hash lookup
 * and compilation if necessary.
 *
 * This is implemented by writing the above 12-byte prefix before
 * translating every ccbuf, since there is no good way to tell which
 * ccbufs may be targets of indirect jumps. In addition,
 * bt_translate_and_run takes an extra parameter, 'exact', which
 * controls whether it should chain to cfrag->code, or 12 bytes before
 * it. bt_continue_chain sets the flag, bt_continue_ic does not.
 */

#define LOAD_BETA_REG(buf, breg, x86reg) ({                             \
    uint8_t __reg = (breg);                                             \
    if(__reg == 31) {                                                   \
        X86_XOR_RM32_R32(buf, MOD_REG, x86reg, x86reg);                 \
    } else {                                                            \
        X86_MOV_RM32_R32(buf, MOD_INDIR_DISP8, X86_EBP, x86reg);        \
        X86_DISP8(buf, offsetof(beta_cpu, regs) + 4*__reg);             \
    }                                                                   \
        })
#define WRITE_BETA_REG(buf, x86reg, breg) ({                            \
    uint8_t __reg = (breg);                                             \
    if(__reg != 31) {                                                   \
        X86_MOV_R32_RM32(buf, x86reg, MOD_INDIR_DISP8, X86_EBP);        \
        X86_DISP8(buf, offsetof(beta_cpu, regs) + 4*__reg);             \
    }                                                                   \
        })

inline void bt_translate_arith(ccbuff *pbuf, byteptr pc UNUSED, bdecode *inst) {
    ccbuff buf = *pbuf;
    /* Load %eax with RA, the LHS */
    LOAD_BETA_REG(buf, inst->ra, X86_EAX);
    /* Load %ecx with RB, the RHS */
    LOAD_BETA_REG(buf, inst->rb, X86_ECX);

    switch(inst->opcode) {
    case OP_ADD:
        X86_ADD_RM32_R32(buf, MOD_REG, X86_ECX, X86_EAX);
        break;
    case OP_AND:
        X86_AND_RM32_R32(buf, MOD_REG, X86_ECX, X86_EAX);
        break;
    case OP_OR:
        X86_OR_RM32_R32(buf, MOD_REG, X86_ECX, X86_EAX);
        break;
    case OP_SUB:
        X86_SUB_RM32_R32(buf, MOD_REG, X86_ECX, X86_EAX);
        break;
    case OP_XOR:
        X86_XOR_RM32_R32(buf, MOD_REG, X86_ECX, X86_EAX);
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
    case OP_CMPLE:
        /* cmp %ecx, %eax */
        X86_CMP_RM32_R32(buf, MOD_REG, X86_ECX, X86_EAX);
        X86_MOV_IMM32_R32(buf, X86_EAX);
        X86_IMM32(buf, 0x0);
        uint8_t cc = (inst->opcode == OP_CMPLT ? CC_L
                      : (inst->opcode == OP_CMPLE ? CC_LE
                         : CC_Z));
        X86_SETCC_RM8(buf, cc, MOD_REG, X86_EAX);
        break;
    default:
        panic("Unknown arithmetic opcode: 0x%02x\n", inst->opcode);
    }
    /* Load %eax into RC */
    WRITE_BETA_REG(buf, X86_EAX, inst->rc);
    *pbuf = buf;
}


inline void bt_translate_arithc(ccbuff *pbuf, byteptr pc UNUSED, bdecode *inst) {
    ccbuff buf = *pbuf;
    uint32_t constant = inst->imm;
    /* Load %eax with RA, the LHS */
    if(inst->ra == 31) {
        X86_XOR_RM32_R32(buf, MOD_REG, X86_EAX, X86_EAX);
    } else {
        X86_MOV_RM32_R32(buf, MOD_INDIR_DISP8, X86_EBP, X86_EAX);
        X86_DISP8(buf, offsetof(beta_cpu, regs) + 4*inst->ra);
    }

    switch(inst->opcode) {
    case OP_ADDC:
        X86_ADD_IMM32_RM32(buf, MOD_REG, X86_EAX);
        X86_IMM32(buf, constant);
        break;
    case OP_ANDC:
        X86_AND_IMM32_RM32(buf, MOD_REG, X86_EAX);
        X86_IMM32(buf, constant);
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
        X86_OR_IMM32_RM32(buf, MOD_REG, X86_EAX);
        X86_IMM32(buf, constant);
        break;
    case OP_SUBC:
        X86_SUB_IMM32_RM32(buf, MOD_REG, X86_EAX);
        X86_IMM32(buf, constant);
        break;
    case OP_XORC:
        X86_XOR_IMM32_RM32(buf, MOD_REG, X86_EAX);
        X86_IMM32(buf, constant);
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
    case OP_CMPLEC:
        /* cmp $IMM32, %eax */
        X86_CMP_IMM32_RM32(buf, MOD_REG, X86_EAX);
        X86_IMM32(buf, constant);
        X86_MOV_IMM32_R32(buf, X86_EAX);
        X86_IMM32(buf, 0x0);
        uint8_t cc = (inst->opcode == OP_CMPLTC ? CC_L
                      : (inst->opcode == OP_CMPLEC ? CC_LE
                         : CC_Z));
        X86_SETCC_RM8(buf, cc, MOD_REG, X86_EAX);
        break;
    default:
        panic("Unknown constant arithmetic opcode: 0x%02x\n", inst->opcode);
    }
    /* Load %eax into RC */
    WRITE_BETA_REG(buf, X86_EAX, inst->rc);
    *pbuf = buf;
}

inline void bt_translate_other(ccbuff *pbuf, byteptr pc, bdecode *inst) {
    ccbuff buf = *pbuf;
    switch(inst->opcode) {
    case OP_ST:
        /* mov regs[RA], %eax
         * add IMM, %eax
         * mov regs[RC], %ecx
         * and $7FFFFFFC, %eax
         * mov %ecx, mem[%eax]
         */
        LOAD_BETA_REG(buf, inst->ra, X86_EAX);

        if(inst->imm) {
            X86_ADD_IMM32_RM32(buf, MOD_REG, X86_EAX);
            X86_IMM32(buf, inst->imm);
        }

        LOAD_BETA_REG(buf, inst->rc, X86_ECX);

        X86_AND_IMM32_RM32(buf, MOD_REG, X86_EAX);
        X86_IMM32(buf, ~(PC_SUPERVISOR | 0x3));

        X86_BYTE(buf, PREFIX_SEG_FS);
        X86_MOV_R32_RM32(buf, X86_ECX, MOD_INDIR, X86_EAX);

        break;
    case OP_LD:
        /* mov regs[RA], %eax
         * add IMM, %eax
         * and $7FFFFFFC, %eax
         * mov mem[%eax], eax
         * mov $eax, regs[RC]
         */
        LOAD_BETA_REG(buf, inst->ra, X86_EAX);

        if(inst->imm) {
            X86_ADD_IMM32_RM32(buf, MOD_REG, X86_EAX);
            X86_IMM32(buf, inst->imm);
        }

        X86_AND_IMM32_RM32(buf, MOD_REG, X86_EAX);
        X86_IMM32(buf, ~(PC_SUPERVISOR | 0x3));

        X86_BYTE(buf, PREFIX_SEG_FS);
        X86_MOV_RM32_R32(buf, MOD_INDIR, X86_EAX, X86_EAX);

        WRITE_BETA_REG(buf, X86_EAX, inst->rc);
        break;
    case OP_LDR:

        X86_BYTE(buf, PREFIX_SEG_FS);
        X86_MOV_RM32_R32(buf, MOD_INDIR, REG_DISP32, X86_EAX);
        X86_DISP32(buf, ((pc + 4 + 4*inst->imm) & ~(PC_SUPERVISOR|0x03)));

        WRITE_BETA_REG(buf, X86_EAX, inst->rc);
        break;
    default:
        panic("Unable to translate opcode: 0x%02x\n", inst->opcode);
    }
    *pbuf = buf;
}

inline void bt_translate_inst(ccbuff *pbuf, byteptr pc, bdecode *inst) {
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
inline void bt_translate_prologue(ccbuff *pbuf, byteptr pc) {
    ccbuff buf = *pbuf;

    X86_CMP_IMM32_RM32(buf, MOD_REG, X86_EAX);
    X86_IMM32(buf, pc);
    X86_JCC_REL32(buf, CC_NZ);
    X86_REL32(buf, bt_continue);

    if(!(pc & PC_SUPERVISOR)) {
        X86_TEST_IMM32_RM32(buf, MOD_INDIR_DISP32, X86_EBP);
        X86_DISP32(buf, offsetof(beta_cpu, pending_interrupts));
        X86_IMM32(buf, 0xFFFFFFFF);
        X86_JCC_REL32(buf, CC_NZ);
        X86_REL32(buf, bt_interrupt);
    }
    *pbuf = buf;
}

inline void bt_translate_interp(ccbuff *pbuf, byteptr pc) {
    // Align %esp on a 16-byte boundary to placate OS X
    X86_SUB_IMM32_RM32(*pbuf, MOD_REG, X86_ESP);
    X86_IMM32(*pbuf, 4);

    // Save the PC into CPU.PC
    X86_MOV_IMM32_RM32(*pbuf, MOD_INDIR_DISP32, X86_EBP);
    X86_DISP32(*pbuf, offsetof(beta_cpu, PC));
    X86_IMM32(*pbuf, pc);

    // Call bt_step_one
    X86_CALL_REL32(*pbuf);
    X86_REL32(*pbuf, bcpu_step_one);

    X86_ADD_IMM32_RM32(*pbuf, MOD_REG, X86_ESP);
    X86_IMM32(*pbuf, 4);

    // Save CPU.PC back into %eax
    X86_MOV_RM32_R32(*pbuf, MOD_INDIR_DISP32, X86_EBP, X86_EAX);
    X86_DISP32(*pbuf, offsetof(beta_cpu, PC));

    X86_CALL_REL32(*pbuf);
    X86_REL32(*pbuf, bt_continue_chain);
}

inline void bt_translate_tail(ccbuff *pbuf, byteptr pc, bdecode *inst) {
    ccbuff buf = *pbuf;
#define SAVE_PC                                                 \
    if(inst->rc != 31) {                                        \
        X86_MOV_IMM32_RM32(buf, MOD_INDIR_DISP8, X86_EBP);      \
        X86_DISP8(buf, offsetof(beta_cpu, regs)+ 4*inst->rc);   \
        X86_IMM32(buf, pc + 4);                                 \
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

            X86_CMP_IMM32_RM32(buf, MOD_INDIR_DISP8, X86_EBP);
            X86_DISP8(buf, offsetof(beta_cpu, regs) + 4 * inst->ra);
            X86_IMM32(buf, 0);

            SAVE_PC;

            X86_JCC_REL8(buf, inst->opcode == OP_BT ? CC_NZ : CC_Z);
            X86_DISP8(buf, 10);

            X86_MOV_IMM32_R32(buf, X86_EAX);
            X86_IMM32(buf, pc + 4);

            X86_CALL_REL32(buf);
            X86_REL32(buf, bt_continue_chain);

            X86_MOV_IMM32_R32(buf, X86_EAX);
            X86_IMM32(buf, (pc + 4 + 4*inst->imm) & ~0x03);

            X86_CALL_REL32(buf);
            X86_REL32(buf, bt_continue_chain);
        }
        break;
    case OP_JMP:
        SAVE_PC;
        LOAD_BETA_REG(buf, inst->ra, X86_EAX);

        X86_AND_IMM32_RM32(buf, MOD_REG, X86_EAX);
        X86_IMM32(buf, (pc & PC_SUPERVISOR) | ~(PC_SUPERVISOR|0x3));

        X86_CALL_REL32(buf);
        X86_REL32(buf, bt_continue_ic);
        break;
    case OP_CALLOUT:
        bt_translate_interp(&buf, pc);
        break;
    default:
        /* If we made it here, it's an ILLOP */
        ASSERT(!decode_valid(inst));
        /* XP = PC + 4*/
        X86_MOV_IMM32_RM32(buf, MOD_INDIR_DISP8, X86_EBP);
        X86_DISP8(buf, offsetof(beta_cpu, regs) + 4*XP);
        X86_IMM32(buf, pc + 4);

        X86_MOV_IMM32_R32(buf, X86_EAX);
        X86_IMM32(buf, ISR_ILLOP);

        X86_CALL_REL32(buf);
        X86_REL32(buf, bt_continue_chain);
    }
    *pbuf = buf;
}

ccbuff bt_translate_frag(compiled_frag *cfrag, decode_frag *frag) {
    ccbuff buf = cfrag->code;
    byteptr pc = frag->start_pc;
    int i;

    LOG("Translating %d instruction%s at 0x%08x",
        frag->ninsts, (frag->tail?" (plus tail)" : ""), frag->start_pc);

    cfrag->start_pc = frag->start_pc;

    bt_translate_prologue(&buf, pc);

    for(i = 0; i < frag->ninsts; i++) {
        bt_translate_inst(&buf, pc, &frag->insts[i]);
        pc += 4;
    }

    /* Update CPU.inst_count */

    X86_ADD_IMM32_RM32(buf, MOD_INDIR_DISP32, X86_EBP);
    X86_DISP32(buf, offsetof(beta_cpu, inst_count));
    X86_IMM32(buf, frag->ninsts + (frag->tail ? 1 : 0));

    if(frag->tail) {
        bt_translate_tail(&buf, pc, &frag->insts[i]);
    } else {
        /*
         * default epilogue -- save emulated PC and jump to bt_continue_chain
         */
        X86_MOV_IMM32_R32(buf, X86_EAX);
        X86_IMM32(buf, pc);

        X86_CALL_REL32(buf);
        X86_REL32(buf, bt_continue_chain);
    }
    return buf;
}

void bt_run() {
    if(setjmp(bt_exit_buf)) {
        return;
    }

    if(!bt_stack_base) {
        bt_stack_base = valloc(BT_STACK_SIZE);
        if(bt_stack_base == NULL) {
            panic("Unable to allocate BT stack!\n");
        }
    }
    bt_stack_base += BT_STACK_SIZE;

    if(!frag_code_cache) {
        frag_code_cache = valloc(BT_CACHE_SIZE);
        if(frag_code_cache == NULL) {
            panic("Could not allocate BT cache!");
        }
        if(mprotect(frag_code_cache, BT_CACHE_SIZE, PROT_READ|PROT_WRITE|PROT_EXEC)) {
            perror("mprotect");
            panic("Unable to mprotect() the BT cache!");
        }
        bt_clear_cache();
    }

    CPU.segment = bt_setup_cpu_segment();
    bt_setup_segv_handler();

    bt_translate_and_run(1, NULL);
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

void bt_translate_and_run(uint32_t exact, ccbuff chainptr) {
    compiled_frag *cfrag;
    decode_frag frag;
    uint32_t inst;
    uint32_t pc;
    int i = 0;

    if(CPU.halt) {
        longjmp(bt_exit_buf, 1);
    }

    pc = CPU.PC;

    cfrag = bt_find_frag(pc);

    if(!cfrag) {
        frag.start_pc = pc;
        frag.tail = FALSE;
        for(i = 0; i < MAX_FRAG_SIZE; i++) {
            inst = beta_read_mem32(&CPU, pc);
            decode_op(inst, &frag.insts[i]);
            if(bt_ends_frag(&frag.insts[i])) {
                frag.tail = TRUE;
                break;
            }
            pc += 4;
        }
        frag.ninsts = i;
        cfrag = bt_alloc_cfrag(TRUE);
        if (chainptr == cfrag->code) {
            cfrag->code -= 5;
            chainptr = NULL;
            LOG("Chaining adjacent frags at pc=0x%08x", frag.start_pc);
        }

        frag_code_alloc = bt_translate_frag(cfrag, &frag);
        cfrag->code += PC_CHECK_SIZE;

        bt_insert_frag(cfrag);
    } else {
        LOG("Cache HIT at pc 0x%08x", pc);
    }

    if(chainptr) {
        chainptr -= 5;
        X86_JMP_REL32(chainptr);
        X86_REL32(chainptr, (exact ? cfrag->code : (cfrag->code - PC_CHECK_SIZE)));
        LOG("Chaining to frag 0x%08x", cfrag->start_pc);
    }

    __asm__("movw %%ax, %%fs\n" :: "a"(CPU.segment<<3|0x4));
    bt_enter(cfrag->code);
}
