#include "bemu.h"

#define ALL_OPS  OP(OP_ADD) OP(OP_ADDC) OP(OP_AND) OP(OP_ANDC)  \
    OP(OP_MUL) OP(OP_MULC) OP(OP_DIV) OP(OP_DIVC)               \
    OP(OP_OR) OP(OP_ORC) OP(OP_SHL) OP(OP_SHLC)                 \
    OP(OP_SHR) OP(OP_SHRC) OP(OP_SRA) OP(OP_SRAC)               \
    OP(OP_SUB) OP(OP_SUBC) OP(OP_XOR) OP(OP_XORC)               \
    OP(OP_CMPEQ) OP(OP_CMPEQC) OP(OP_CMPLE) OP(OP_CMPLEC)       \
    OP(OP_CMPLT) OP(OP_CMPLTC)                                  \
    OP(OP_JMP) OP(OP_BT) OP(OP_BF)                              \
    OP(OP_LD) OP(OP_ST) OP(OP_LDR)                              \
    OP(OP_CALLOUT)


bool decode_valid(bdecode *decode)
{
    switch (decode->opcode & 0x3F) {
#define OP(c) case (c): return true;
        ALL_OPS
#undef OP
    default: return false;
    }
}

const char *op_name(beta_op op)
{
    static char opbuf[20];
    switch(op) {
#define OP(mnm) case mnm: return #mnm;
        ALL_OPS
#undef OP
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
