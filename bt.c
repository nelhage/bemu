#include "bemu.h"

#include <setjmp.h>

#define BT_STACK_SIZE  65536


jmp_buf bt_exit_buf;
uint8_t *bt_stack_base = NULL;

void bt_enter(ccbuff buf) __attribute__((noreturn));
void bt_translate_and_run(void) __attribute__((noreturn));

/* During execution, %ebp points to the base of the regfile */
inline ccbuff bt_translate_arith(ccbuff buf, uint32_t pb, bdecode *inst) {
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


inline ccbuff bt_translate_arithc(ccbuff buf, uint32_t pb, bdecode *inst) {
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

inline ccbuff bt_translate_inst(ccbuff buf, uint32_t pc, bdecode *inst) {
    switch(OP_CLASS(inst->opcode)) {
    case CLASS_ARITH:
        return bt_translate_arith(buf, pc, inst);
    case CLASS_ARITHC:
        return bt_translate_arithc(buf, pc, inst);
     default:
         panic("Unable to translate instruction class 0x%x", OP_CLASS(inst->opcode));
    }
}

void bt_translate_frag(ccbuff buf, decode_frag frag) {
    uint32_t pc = frag.start_pc;
    int i;
    for(i = 0; i < frag.ninsts; i++) {
        buf = bt_translate_inst(buf, pc, &frag.insts[i]);
        pc += 4;
    }
    /*
     * epilogue -- move emulated PC to CPU.PC and return to
     * bt_translate_and_run
     */
    X86_MOV_IMM32_RM32(buf, MOD_INDIR, REG_DISP32);
    X86_DISP32(buf, &CPU.PC);
    X86_IMM32(buf, pc);
    X86_JMP_REL32(buf);
    X86_IMM32(buf, ((uint8_t*)bt_translate_and_run - (buf + 4)));
}

void bt_run() {
    if(setjmp(bt_exit_buf)) {
        return;
    }

    if(!bt_stack_base) {
        bt_stack_base = mmap(NULL, BT_STACK_SIZE,
                             PROT_READ|PROT_WRITE|PROT_EXEC,
                             MAP_PRIVATE|MAP_ANONYMOUS|MAP_GROWSDOWN, -1, 0);
        if(bt_stack_base == MAP_FAILED) {
            panic("Unable to allocate BT stack!\n");
        }
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
    uint8_t buff[CCBUFF_MAX_SIZE];
    ccbuff tbuff = buff;
    decode_frag frag;
    uint32_t inst;
    uint32_t pc;
    int i = 0;


    while(TRUE) {
        if(CPU.halt) {
            longjmp(bt_exit_buf, 1);
        }

        pc = CPU.PC;

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

        if (i == 0) {
            bcpu_step_one();
        } else {
            frag.ninsts = i;
            bt_translate_frag(tbuff, frag);
            bt_enter(tbuff);
        }
    }
}

void bt_enter(ccbuff buf) {
    asm volatile("mov %0, %%ebp\n\t"
                  "jmp *%1" : : "g"(CPU.regs), "r"(buf));
    while(1); /* Satisfy the compiler that we don't return */
}
