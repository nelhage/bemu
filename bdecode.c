#include "bemu.h"

/*
 * Decode a 32-bit \Beta opcode into a bdecode struct
 */
void decode_op(uint32_t op, bdecode *decode)
{
    decode->opcode = BOP_OP(op);
    decode->ra     = BOP_RA(op);
    decode->rb     = BOP_RB(op);
    decode->rc     = BOP_RC(op);
    decode->imm    = BOP_CONST(op);
}

static bool op_valid_table[64] = {
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

char *op_name(beta_op op)
{
    static char opbuf[20];
#define CASE(mnm) case mnm: return #mnm;
    switch(op) {
        CASE(OP_ADD);
        CASE(OP_ADDC);
        CASE(OP_AND);
        CASE(OP_ANDC);
        CASE(OP_MUL);
        CASE(OP_MULC);
        CASE(OP_DIV);
        CASE(OP_DIVC);
        CASE(OP_OR);
        CASE(OP_ORC);
        CASE(OP_SHL);
        CASE(OP_SHLC);
        CASE(OP_SHR);
        CASE(OP_SHRC);
        CASE(OP_SRA);
        CASE(OP_SRAC);
        CASE(OP_SUB);
        CASE(OP_SUBC);
        CASE(OP_XOR);
        CASE(OP_XORC);
        CASE(OP_CMPEQ);
        CASE(OP_CMPEQC);
        CASE(OP_CMPLE);
        CASE(OP_CMPLEC);
        CASE(OP_CMPLT);
        CASE(OP_CMPLTC);
        CASE(OP_JMP);
        CASE(OP_BT);
        CASE(OP_BF);
        CASE(OP_LD);
        CASE(OP_ST);
        CASE(OP_LDR);
    default:
        snprintf(opbuf, sizeof opbuf - 1, "ILLOP(%02x)", op);
        return opbuf;
    }
#undef CASE
}

char *pp_decode(bdecode *decode)
{
    static char buf[1024];
    if(!OP_IMM(decode->opcode)) {
        snprintf(buf, sizeof buf - 1,
                 "%-10s %%r%d, %%r%d, %%r%d",
                 op_name(decode->opcode),
                 decode->ra, decode->rb, decode->rc);
    } else {
        snprintf(buf, sizeof buf - 1,
                 "%-10s %%r%d, $%d, %%r%d",
                 op_name(decode->opcode),
                 decode->ra, (int32_t)decode->imm, decode->rc);
    }
    return buf;
}
