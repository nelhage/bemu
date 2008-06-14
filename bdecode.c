#include "bdecode.h"


/*
 * Decode the instruction pointed to by `instr' into `decode',
 * advancing `instr' as to point to the next instruction.
 */
void decode_op(uint32_t **instr, bdecode *decode)
{
    uint32_t op = *((*instr)++);
    decode->opcode = BOP_OP(op);
    decode->ra     = BOP_RA(op);
    decode->rb     = BOP_RB(op);
    decode->rc     = BOP_RC(op);
    decode->imm    = BOP_CONST(op);
}

bool op_valid_table[64] = {
    [OP_ADD]   1, [OP_ADDC]   1,
    [OP_AND]   1, [OP_ANDC]   1,
    [OP_MUL]   1, [OP_MULC]   1,
    [OP_DIV]   1, [OP_DIVC]   1,
    [OP_OR]    1, [OP_ORC]    1,
    [OP_SHL]   1, [OP_SHLC]   1,
    [OP_SHR]   1, [OP_SHRC]   1,
    [OP_SRA]   1, [OP_SRAC]   1,
    [OP_SUB]   1, [OP_SUBC]   1,
    [OP_XOR]   1, [OP_XORC]   1,
    [OP_CMPEQ] 1, [OP_CMPEQC] 1,
    [OP_CMPLE] 1, [OP_CMPLEC] 1,
    [OP_CMPLT] 1, [OP_CMPLTC] 1,
    [OP_JMP] 1,
    [OP_BT]  1, [OP_BF] 1,
    [OP_LD]  1, [OP_ST] 1,
    [OP_LDR] 1,
};

bool decode_valid(bdecode *decode)
{
    return op_valid_table[decode->opcode & 0x3F];
}
