#ifndef __X86_H__
#define __X86_H__

/* compiled code buffer */
typedef uint8_t* ccbuff;

#define MODRM(mod, regop, rm) ({                              \
    uint8_t _mod=(mod), _regop=(regop), _rm=(rm);             \
    ASSERT(!(_mod   & (~0x3)));                               \
    ASSERT(!(_regop & (~0x7)));                               \
    ASSERT(!(_rm    & (~0x7)));                               \
    (_mod << 6)|(_regop << 3)|(_rm);                          \
})

/* Cheat because the encoding is the same... */
#define SIB(scale, index, base) MODRM(scale, index, base)

typedef enum {
    X86_EAX = 0x00,
    X86_ECX = 0x01,
    X86_EDX = 0x02,
    X86_EBX = 0x03,
    X86_ESP = 0x04,
    X86_EBP = 0x05,
    X86_ESI = 0x06,
    X86_EDI = 0x07,
} x86_reg;

class X86Register {
public:
    int val;
    X86Register(int v) : val(v) {};
};
#define R(name, v) static X86Register X86##name(v);
R(EAX, 0x00);
R(ECX, 0x01);
R(EDX, 0x02);
R(EBX, 0x03);
R(ESP, 0x04);
R(EBP, 0x05);
R(ESI, 0x06);
R(EDI, 0x07);

#define PREFIX_LOCK     0xF0
#define PREFIX_REPNZ    0xF2
#define PREFIX_REPZ     0xF3
#define PREFIX_SEG_CS   0x2E
#define PREFIX_SEG_SS   0x36
#define PREFIX_SEG_DS   0x3E
#define PREFIX_SEG_ES   0x26
#define PREFIX_SEG_FS   0x64
#define PREFIX_SEG_GS   0x65
#define PREFIX_OPSIZE   0x66
#define PREFIX_ADDRSIZE 0x67

#define MOD_INDIR        0x0
#define MOD_INDIR_DISP8  0x1
#define MOD_INDIR_DISP32 0x2
#define MOD_REG          0x3

#define SCALE_1          0x0
#define SCALE_2          0x1
#define SCALE_4          0x2
#define SCALE_8          0x3

/* overflow */
#define CC_O  0x0
#define CC_NO 0x1
/* unsigned comparisons */
#define CC_B  0x2
#define CC_AE 0x3
#define CC_BE 0x6
#define CC_A  0x7
/* zero */
#define CC_Z  0x4
#define CC_NZ 0x5
/* sign */
#define CC_S  0x8
#define CC_NS 0x9
/* parity */
#define CC_P  0xA
#define CC_NP 0xB
/* unsigned comparisons */
#define CC_L  0xC
#define CC_GE 0xD
#define CC_LE 0xE
#define CC_G  0xF

/*
 * Using this as a register argument in the r/m field indicates an SIB
 * byte follows (with mod != 3)
 */

#define REG_SIB          X86_ESP

/*
 * With mod = 0, this specifies a 32-bit displacement
 */
#define REG_DISP32       X86_EBP

#define X86_BYTE(cc, b) (cc)->byte(b)

#define X86_4BYTE(cc, val) (cc)->word(val)

/* For readability */
#define X86_IMM32  X86_4BYTE
#define X86_DISP32 X86_4BYTE
#define X86_IMM8   X86_BYTE
#define X86_DISP8  X86_BYTE
#define X86_REL32(cc, label) (cc)->rel32((uint32_t)label)

#define X86_SIB(ptr, scale, index, base) ({     \
            X86_BYTE(ptr, SIB(scale, index, base));     \
        })

#define X86_INC_RM32(ptr, mod, rm) ({           \
    X86_BYTE(ptr, 0xff);                        \
    X86_BYTE(ptr, MODRM(mod, 0, rm));           \
        })

/*
 * We use the AT&T convention of SRC_DST.
 * 
 * These macros output only the opcode and a ModR/M byte; It is the
 * caller's responsibility to follow them with SIB, displacement, and
 * immediate value if appropriate.
 */

#include "opcodes.h"

class X86Assembler {
private:
    ccbuff out;
public:
    X86Assembler(ccbuff buf) : out(buf) {}

    ccbuff eip() {
        return out;
    }

    template <class T>
    void emit(T v) {
        *((T*)out) = v;
        out += sizeof v;
    }

    void byte(uint8_t b) {
        emit(b);
    }

    void address(uint32_t w) {
        emit(w);
    }

    void word(uint32_t w) {
        emit(w);
    }

    void rel32(uint32_t label) {
        word(((uint8_t*)label) - out - 4);
    }

    void modrm(uint8_t mod, uint8_t reg, uint8_t rm) {
        ASSERT(!(mod & (~0x3)));
        ASSERT(!(reg & (~0x7)));
        ASSERT(!(rm  & (~0x7)));
        byte((uint8_t)(mod << 6)|(reg << 3)|(rm));
    }
};

#endif
