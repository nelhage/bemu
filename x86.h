#ifndef __X86_H__
#define __X86_H__

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
    REG_EAX = 0x00,
    REG_ECX = 0x01,
    REG_EDX = 0x02,
    REG_EBX = 0x03,
    REG_ESP = 0x04,
    REG_EBP = 0x05,
    REG_ESI = 0x06,
    REG_EDI = 0x07,
} x86_reg;

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

/*
 * Using this as a register argument in the r/m field indicates an SIB
 * byte follows (with mod != 3)
 */

#define REG_SIB          REG_ESP

#define X86_BYTE(ptr, byte) ({                  \
            uint8_t _byte = (uint8_t)(byte);    \
            uint8_t *_ptr = (uint8_t*)(ptr);    \
            *_ptr = _byte;                      \
            ptr += sizeof _byte;                \
        })

#define X86_4BYTE(ptr, val) ({                    \
            uint32_t _val = (uint32_t)(val);      \
            uint32_t *_ptr = (uint32_t*)(ptr);    \
            *_ptr = _val;                         \
            ptr += sizeof _val;                   \
        })

/* For readability */
#define X86_IMM32  X86_4BYTE
#define X86_DISP32 X86_4BYTE
#define X86_IMM8   X86_BYTE
#define X86_DISP8  X86_BYTE
#define X86_SIB(ptr, scale, index, base) ({     \
            X86_BYTE(ptr, SIB(scale, index, base));     \
        });

/*
 * We use the AT&T convention of SRC_DST.
 * 
 * These macros output only the opcode and a ModR/M byte; It is the
 * caller's responsibility to follow them with SIB, displacement, and
 * immediate value if appropriate.
 */

#include "opcodes.h"

#endif
