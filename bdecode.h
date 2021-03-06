#ifndef __BDECODE_H__
#define __BDECODE_H__

#include <stdint.h>

typedef enum {
    CLASS_ARITH   = 0x2,
    CLASS_ARITHC  = 0x3,
    CLASS_OTHER   = 0x1,
    CLASS_INVALID = 0x0
} op_class;

#define OP_ARITH(op)       (0x20 | (op))
#define OP_ARITHC(op)      (0x30 | (op))

/* Opcodes */

#define OP_ADD          OP_ARITH(0x00)
#define OP_ADDC         OP_ARITHC(0x00)
#define OP_AND          OP_ARITH(0x08)
#define OP_ANDC         OP_ARITHC(0x08)
#define OP_MUL          OP_ARITH(0x02)
#define OP_MULC         OP_ARITHC(0x02)
#define OP_DIV          OP_ARITH(0x03)
#define OP_DIVC         OP_ARITHC(0x03)
#define OP_OR           OP_ARITH(0x09)
#define OP_ORC          OP_ARITHC(0x09)
#define OP_SHL          OP_ARITH(0x0C)
#define OP_SHLC         OP_ARITHC(0x0C)
#define OP_SHR          OP_ARITH(0x0D)
#define OP_SHRC         OP_ARITHC(0x0D)
#define OP_SRA          OP_ARITH(0x0E)
#define OP_SRAC         OP_ARITHC(0x0E)
#define OP_SUB          OP_ARITH(0x01)
#define OP_SUBC         OP_ARITHC(0x01)
#define OP_XOR          OP_ARITH(0x0A)
#define OP_XORC         OP_ARITHC(0x0A)
#define OP_CMPEQ        OP_ARITH(0x04)
#define OP_CMPEQC       OP_ARITHC(0x04)
#define OP_CMPLE        OP_ARITH(0x06)
#define OP_CMPLEC       OP_ARITHC(0x06)
#define OP_CMPLT        OP_ARITH(0x05)
#define OP_CMPLTC       OP_ARITHC(0x05)

#define OP_JMP          0x1B
#define OP_BT           0x1E
#define OP_BF           0x1D

#define OP_LD           0x18
#define OP_ST           0x19
#define OP_LDR          0x1F

#define OP_CALLOUT      0x00

#define CALL_HALT       0x00
#define CALL_RDCHR      0x01
#define CALL_WRCHR      0x02

#define BOP_OP(op)    (((op) >> 26) & 0x3F)
#define BOP_RA(op)    (((op) >> 16) & 0x1F)
#define BOP_RB(op)    (((op) >> 11) & 0x1F)
#define BOP_RC(op)    (((op) >> 21) & 0x1F)
#define BOP_CONST(op) ((int16_t)((op) & 0xFFFF))

typedef uint8_t beta_op;
typedef uint8_t beta_reg;

typedef struct {
    beta_op   opcode;
    beta_reg  ra, rb, rc;
    int32_t   imm;
} bdecode;

/*
 * Opcode classes 0b11 and 0b01 both have an immediate
 * operand. 0b00 and 0b10 do not.
 */
#define OP_IMM(op)   ((op) & 0x10)
#define OP_CLASS(op) ((op) >> 4)

static inline void decode_op(uint32_t instr, bdecode *decode)
    __attribute__ ((always_inline,used));
bool decode_valid(bdecode *decode);
char *pp_decode(bdecode *decode);
const char *op_name(beta_op op);

/*
 * Decode a 32-bit \Beta opcode into a bdecode struct
 */
void decode_op(uint32_t op, bdecode *decode)
{
    decode->opcode = BOP_OP(op);
    decode->ra     = BOP_RA(op);
    decode->rb     = BOP_RB(op);
    decode->rc     = BOP_RC(op);
    decode->imm    = (int32_t)BOP_CONST(op);
}

#endif
