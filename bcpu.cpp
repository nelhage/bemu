#include "bemu.h"

void beta_cpu::process_interrupt() {
    byteptr  isr = 0;
    if(pending_interrupts & INT_CLK) {
        clear_interrupt(INT_CLK);
        isr = ISR_CLK;
    } else if(pending_interrupts & INT_KBD) {
        isr = ISR_KBD;
    }
    if(isr) {
        regs[XP] = PC + 4;
        PC = isr;
    }
}

void beta_cpu::execute_one(bdecode *decode) {
    uint32_t old_pc;

    PC += 4;
    inst_count++;
    /*
     * Enforce R31 is always 0
     */
    regs[31] = 0;

    opcode_counts[decode->opcode]++;

    switch(decode->opcode) {

#define ARITH(NAME, OP)                                                 \
    case OP_ ## NAME:                                                   \
        write_reg(decode->rc,                                           \
                  regs[decode->ra] OP regs[decode->rb]);                \
    break;                                                              \
    case OP_ ## NAME ## C:                                              \
        write_reg(decode->rc,                                           \
                  regs[decode->ra] OP decode->imm);                     \
    break;
/* signed arithmetic op */
#define ARITHS(NAME, OP)                                                \
    case OP_ ## NAME:                                                   \
        write_reg(decode->rc,                                           \
                  ((int32_t)regs[decode->ra])                           \
                  OP ((int32_t)regs[decode->rb]));                      \
    break;                                                              \
    case OP_ ## NAME ## C:                                              \
        write_reg(decode->rc,                                           \
                  ((int32_t)regs[decode->ra]) OP decode->imm);          \
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
#define JMP(newpc) ((newpc) & (0x7FFFFFFC | (PC & 0x80000000)))
    case OP_JMP:
        old_pc = PC;
        PC = JMP(regs[decode->ra]);
        write_reg(decode->rc, old_pc);
        break;

    case OP_BT:
        old_pc = PC;
        if(regs[decode->ra]) {
            PC = JMP(PC + WORD2BYTEADDR(decode->imm));
        }
        write_reg(decode->rc, old_pc);
        break;

    case OP_BF:
        old_pc = PC;
        if(!regs[decode->ra]) {
            PC = JMP(PC + WORD2BYTEADDR(decode->imm));
        }
        write_reg(decode->rc, old_pc);
        break;
#undef JMP

    case OP_LD:
        LOG("LD from byte %08x", regs[decode->ra] + decode->imm);
        write_reg(decode->rc, read_mem32(regs[decode->ra] + decode->imm));
        break;

    case OP_ST:
        LOG("ST to byte %08x", regs[decode->ra] + decode->imm);
        write_mem32(regs[decode->ra] + decode->imm, regs[decode->rc]);
        break;

    case OP_LDR:
        write_reg(decode->rc, read_mem32(PC + WORD2BYTEADDR(decode->imm)));
        break;

    case OP_CALLOUT:
        if(PC & PC_SUPERVISOR) {
            switch(decode->imm) {
            case CALL_HALT:
                halt = 1;
                break;
            case CALL_RDCHR:
                regs[0] = beta_rdchr();
                break;
            case CALL_WRCHR:
                beta_wrchr(regs[0]);
                break;
            default:
                /* Treat an unknown callout as a NOP */
                break;
            }
            break;
        }
        /* Fall through to ILLOP in user mode */
    default:
        regs[XP] = PC;
        PC = ISR_ILLOP;
        break;
    }

    if(pending_interrupts && !(PC & PC_SUPERVISOR)) {
        process_interrupt();
    }
}

void beta_cpu::reset()
{
    PC = ISR_RESET;
    regs[31] = 0;
    inst_count = 0;
    memset(opcode_counts, 0, sizeof opcode_counts);
    halt = 0;
}

/*
 * Advance the \Beta CPU one cycle
 */
void beta_cpu::step_one()
{
    bdecode decode;
    uint32_t op;

    op = read_mem32(PC);

    decode_op(op, &decode);
    LOG("[PC=%08x bits=%08x] %s", PC, op, pp_decode(&decode));
    LOG("ra=%08x, rb=%08x, rc=%08x",
        regs[decode.ra], regs[decode.rb], regs[decode.rc]);

    execute_one(&decode);
}
