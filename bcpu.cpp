#include "bemu.h"

inline void write_reg(beta_cpu *cpu, beta_reg reg, uint32_t val)
{
    cpu->regs[reg] = val;
}

void bcpu_process_interrupt(beta_cpu *cpu) {
    byteptr  isr = 0;
    if(cpu->pending_interrupts & INT_CLK) {
        clear_interrupt(cpu, INT_CLK);
        isr = ISR_CLK;
    } else if(cpu->pending_interrupts & INT_KBD) {
        isr = ISR_KBD;
    }
    if(isr) {
        cpu->regs[XP] = cpu->PC + 4;
        cpu->PC = isr;
    }
}

void bcpu_execute_one(beta_cpu *cpu, bdecode *decode) {
    uint32_t old_pc;

    cpu->PC += 4;
    cpu->inst_count++;
    /*
     * Enforce R31 is always 0
     */
    cpu->regs[31] = 0;

    cpu->opcode_counts[decode->opcode]++;

    switch(decode->opcode) {

#define ARITH(NAME, OP)                                                 \
    case OP_ ## NAME:                                                   \
        write_reg(cpu, decode->rc,                                      \
                  cpu->regs[decode->ra] OP cpu->regs[decode->rb]);      \
    break;                                                              \
    case OP_ ## NAME ## C:                                              \
        write_reg(cpu, decode->rc,                                      \
                  cpu->regs[decode->ra] OP decode->imm);                \
    break;
/* signed arithmetic op */
#define ARITHS(NAME, OP)                                                \
    case OP_ ## NAME:                                                   \
        write_reg(cpu, decode->rc,                                      \
                  ((int32_t)cpu->regs[decode->ra])                      \
                  OP ((int32_t)cpu->regs[decode->rb]));                 \
    break;                                                              \
    case OP_ ## NAME ## C:                                              \
        write_reg(cpu, decode->rc,                                      \
                  ((int32_t)cpu->regs[decode->ra]) OP decode->imm);     \
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
#define JMP(newpc) ((newpc) & (0x7FFFFFFC | (cpu->PC & 0x80000000)))
    case OP_JMP:
        old_pc = cpu->PC;
        cpu->PC = JMP(cpu->regs[decode->ra]);
        write_reg(cpu, decode->rc, old_pc);
        break;

    case OP_BT:
        old_pc = cpu->PC;
        if(cpu->regs[decode->ra]) {
            cpu->PC = JMP(cpu->PC + WORD2BYTEADDR(decode->imm));
        }
        write_reg(cpu, decode->rc, old_pc);
        break;

    case OP_BF:
        old_pc = cpu->PC;
        if(!cpu->regs[decode->ra]) {
            cpu->PC = JMP(cpu->PC + WORD2BYTEADDR(decode->imm));
        }
        write_reg(cpu, decode->rc, old_pc);
        break;
#undef JMP

    case OP_LD:
        LOG("LD from byte %08x", cpu->regs[decode->ra] + decode->imm);
        write_reg(cpu, decode->rc,
                  beta_read_mem32(cpu, cpu->regs[decode->ra] + decode->imm));
        break;

    case OP_ST:
        LOG("ST to byte %08x", cpu->regs[decode->ra] + decode->imm);
        beta_write_mem32(cpu, cpu->regs[decode->ra] + decode->imm, cpu->regs[decode->rc]);
        break;

    case OP_LDR:
        write_reg(cpu, decode->rc,
                  beta_read_mem32(cpu, cpu->PC + WORD2BYTEADDR(decode->imm)));
        break;

    case OP_CALLOUT:
        if(cpu->PC & PC_SUPERVISOR) {
            switch(decode->imm) {
            case CALL_HALT:
                cpu->halt = 1;
                break;
            case CALL_RDCHR:
                cpu->regs[0] = beta_rdchr();
                break;
            case CALL_WRCHR:
                beta_wrchr(cpu->regs[0]);
                break;
            default:
                /* Treat an unknown callout as a NOP */
                break;
            }
            break;
        }
        /* Fall through to ILLOP in user mode */
    default:
        cpu->regs[XP] = cpu->PC;
        cpu->PC = ISR_ILLOP;
        break;
    }

    if(cpu->pending_interrupts && !(cpu->PC & PC_SUPERVISOR)) {
        bcpu_process_interrupt(cpu);
    }
}

void bcpu_reset(beta_cpu *cpu)
{
    cpu->PC = ISR_RESET;
    cpu->regs[31] = 0;
    cpu->inst_count = 0;
    memset(cpu->opcode_counts, 0, sizeof cpu->opcode_counts);
    cpu->halt = 0;
}

/*
 * Advance the \Beta CPU one cycle
 */
void bcpu_step_one(beta_cpu *cpu)
{
    bdecode decode;
    uint32_t op;

    op = beta_read_mem32(cpu, cpu->PC);

    decode_op(op, &decode);
    LOG("[PC=%08x bits=%08x] %s", cpu->PC, op, pp_decode(&decode));
    LOG("ra=%08x, rb=%08x, rc=%08x",
        cpu->regs[decode.ra],
        cpu->regs[decode.rb],
        cpu->regs[decode.rc]);

    bcpu_execute_one(cpu, &decode);
}
