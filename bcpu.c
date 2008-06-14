#include "bcpu.h"
#include "bdecode.h"

beta_cpu CPU;
uint32_t *beta_mem;

#define PC_SUPERVISOR   0x80000000
#define ISR_RESET       (PC_SUPERVISOR | 0x00000000)
#define ISR_ILLOP       (PC_SUPERVISOR | 0x00000004)

void bcpu_execute_one(bdecode *decode) {
    CPU.PC += 4;
    switch(decode->opcode) {

#define ARITH(NAME, OP)                                              \
    case OP_ ## NAME: CPU.regs[decode->rc] =                         \
        CPU.regs[decode->ra] OP CPU.regs[decode->rb];                \
    break;                                                           \
    case OP_ ## NAME ## C: CPU.regs[decode->rc] =                    \
        CPU.regs[decode->ra] OP decode->imm;                         \
    break;


        ARITH(ADD, +)
        ARITH(AND, &)
        ARITH(MUL, *)
        ARITH(DIV, /)
        ARITH(OR,  |)
        ARITH(SHL, <<)
        ARITH(SHR, >>)
/*
 * We can get an arithmetic shift by forcing the compiler to treat the
 * value as signed
 */
    case OP_SRA:
        CPU.regs[decode->rc] =
            ((int32_t)(CPU.regs[decode->ra])) >> CPU.regs[decode->rb];
        break;

    case OP_SRAC:
        CPU.regs[decode->rc] =
            ((int32_t)(CPU.regs[decode->ra])) >> decode->imm;
        break;

        ARITH(SUB, -)
        ARITH(XOR, ^)
        ARITH(CMPEQ, ==)
        ARITH(CMPLE, <=)
        ARITH(CMPLT, <)

#undef ARITH

/*
 * Compute the new PC given a requested PC. Ensures that
 * JMP and friends cannot raise the privilege level by
 * setting the supervisor bit
 */
#define JMP(newpc) ((newpc) & (0x7FFFFFFF | (CPU.PC & 0x80000000)))
    case OP_JMP:
        CPU.regs[decode->rc] = CPU.PC;
        CPU.PC = JMP(CPU.regs[decode->ra]);
        break;

    case OP_BT:
        CPU.regs[decode->rc] = CPU.PC;
        if(CPU.regs[decode->ra]) {
            CPU.PC = JMP(CPU.PC + WORD2BYTEADDR(decode->imm));
        }
        break;

    case OP_BF:
        CPU.regs[decode->rc] = CPU.PC;
        if(!CPU.regs[decode->ra]) {
            CPU.PC = JMP(CPU.PC + WORD2BYTEADDR(decode->imm));
        }
        break;
#undef JMP

    case OP_LD:
        CPU.regs[decode->rc] =
            beta_mem[BYTE2WORDADDR(CPU.regs[decode->ra] + decode->imm)];
        break;

    case OP_ST:
        beta_mem[BYTE2WORDADDR(CPU.regs[decode->ra] + decode->imm)] =
            CPU.regs[decode->rc];
        break;

    case OP_LDR:
        CPU.regs[decode->rc] =
            beta_mem[BYTE2WORDADDR(CPU.PC + WORD2BYTEADDR(decode->imm))];
        break;

    default:
        CPU.XP = CPU.PC;
        CPU.PC = ISR_ILLOP;
        break;
    }
}

void bcpu_reset()
{
    CPU.PC = ISR_RESET;
    /* XXX Do we need to zero registers? */
}

/*
 * Advance the \Beta CPU one cycle
 */
void bcpu_step_one()
{
    bdecode decode;
    uint32_t op;

    op = beta_mem[BYTE2WORDADDR(CPU.PC)];
    decode_op(op, &decode);
    bcpu_execute_one(&decode);
}
