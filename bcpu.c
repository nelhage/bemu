#include "bemu.h"

beta_cpu CPU;
uint32_t *beta_mem;
uint32_t pending_interrupts;

inline void write_reg(beta_reg reg, uint32_t val)
{
    CPU.regs[reg] = val;
}

void bcpu_process_interrupt() {
    uint32_t bit = 0;
    byteptr isr;
    if(pending_interrupts & INT_CLK) {
        bit = INT_CLK;
        isr = ISR_CLK;
    } else if(pending_interrupts & INT_KBD) {
        bit = INT_KBD;
        isr = ISR_KBD;
    } else if(pending_interrupts & INT_MOUSE) {
        bit = INT_MOUSE;
        isr = ISR_MOUSE;
    }
    if(bit) {
        pending_interrupts &= ~bit;
        CPU.regs[XP] = CPU.PC + 4;
        CPU.PC = isr;
    }
}

void bcpu_execute_one(bdecode *decode) {
    uint32_t old_pc;

    CPU.PC += 4;
    /*
     * Enforce R31 is always 0
     */
    CPU.regs[31] = 0;

    switch(decode->opcode) {

#define ARITH(NAME, OP)                                                 \
    case OP_ ## NAME:                                                   \
        write_reg(decode->rc,                                           \
                  CPU.regs[decode->ra] OP CPU.regs[decode->rb]);        \
    break;                                                              \
    case OP_ ## NAME ## C:                                              \
        write_reg(decode->rc,                                           \
                  CPU.regs[decode->ra] OP decode->imm);                 \
    break;
/* signed arithmetic op */
#define ARITHS(NAME, OP)                                                \
    case OP_ ## NAME:                                                   \
        write_reg(decode->rc,                                           \
                  ((int32_t)CPU.regs[decode->ra])                       \
                  OP ((int32_t)CPU.regs[decode->rb]));                  \
    break;                                                              \
    case OP_ ## NAME ## C:                                              \
        write_reg(decode->rc,                                           \
                  ((int32_t)CPU.regs[decode->ra]) OP decode->imm);      \
    break;

        ARITH(ADD, +)
        ARITH(AND, &)
        ARITH(MUL, *)
        ARITH(DIV, /)
        ARITH(OR,  |)
        ARITH(SHL, <<)
        ARITH(SHR, >>)
        ARITHS(SRA, >>)
        ARITH(SUB, -)
        ARITH(XOR, ^)
        ARITHS(CMPEQ, ==)
        ARITHS(CMPLE, <=)
        ARITHS(CMPLT, <)

#undef ARITH
#undef ARITHS

/*
 * Compute the new PC given a requested PC. Ensures that
 * JMP and friends cannot raise the privilege level by
 * setting the supervisor bit
 */
#define JMP(newpc) ((newpc) & (0x7FFFFFFC | (CPU.PC & 0x80000000)))
    case OP_JMP:
        old_pc = CPU.PC;
        CPU.PC = JMP(CPU.regs[decode->ra]);
        write_reg(decode->rc, old_pc);
        break;

    case OP_BT:
        old_pc = CPU.PC;
        if(CPU.regs[decode->ra]) {
            CPU.PC = JMP(CPU.PC + WORD2BYTEADDR(decode->imm));
        }
        write_reg(decode->rc, old_pc);
        break;

    case OP_BF:
        old_pc = CPU.PC;
        if(!CPU.regs[decode->ra]) {
            CPU.PC = JMP(CPU.PC + WORD2BYTEADDR(decode->imm));
        }
        write_reg(decode->rc, old_pc);
        break;
#undef JMP

    case OP_LD:
        LOG("LD from byte %08x", CPU.regs[decode->ra] + decode->imm);
        write_reg(decode->rc, beta_read_mem32(CPU.regs[decode->ra] + decode->imm));
        break;

    case OP_ST:
        LOG("ST to byte %08x", CPU.regs[decode->ra] + decode->imm);
        beta_write_mem32(CPU.regs[decode->ra] + decode->imm, CPU.regs[decode->rc]);
        break;

    case OP_LDR:
        write_reg(decode->rc, beta_read_mem32(CPU.PC + WORD2BYTEADDR(decode->imm)));
        break;

    case OP_CALLOUT:
        switch(decode->imm) {
        case CALL_HALT:
            CPU.halt = 1;
            break;
        case CALL_RDCHR:
            break;
        case CALL_WRCHR:
            beta_wrchr(CPU.regs[0]);
            break;
        default:
            /* Treat an unknown callout as a NOP */
            break;
        }
        break;

    default:
        CPU.regs[XP] = CPU.PC;
        CPU.PC = ISR_ILLOP;
        break;
    }

    if(pending_interrupts && !(CPU.PC & PC_SUPERVISOR)) {
        bcpu_process_interrupt();
    }
}

void bcpu_reset()
{
    CPU.PC = ISR_RESET;
    /* XXX Do we need to zero registers? */
    CPU.regs[31] = 0;
    CPU.halt = 0;
}

/*
 * Advance the \Beta CPU one cycle
 */
void bcpu_step_one()
{
    bdecode decode;
    uint32_t op;

    op = beta_read_mem32(CPU.PC);
    
    decode_op(op, &decode);
    LOG("[PC=%08x bits=%08x] %s", CPU.PC, op, pp_decode(&decode));
    LOG("ra=%08x, rb=%08x, rc=%08x",
        CPU.regs[decode.ra],
        CPU.regs[decode.rb],
        CPU.regs[decode.rc]);

    bcpu_execute_one(&decode);
}
