#ifndef __X86_H__
#define __X86_H__

/* compiled code buffer */
typedef uint8_t* ccbuff;

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

typedef uint8_t cc_t;

/*
 * Using this as a register argument in the r/m field indicates an SIB
 * byte follows (with mod != 3)
 */

#define REG_SIB          X86ESP.val

/*
 * With mod = 0, this specifies a 32-bit displacement
 */
#define REG_DISP32       X86EBP.val

/* Begin C++ */

template<uint8_t opcode>
struct has_opcode {
    const static uint8_t val = opcode;
};
struct none {};
struct true_type {
    const static bool val = true;
};
struct false_type {
    const static bool val = true;
};
struct no_opcode  {
    const static none val;
};

#include "instructions.h"

class X86Assembler;
class X86Register;
class X86Label8;

#define FOR_EACH_INT_TYPE(X) \
    X(signed short)          \
    X(unsigned short)        \
    X(signed int)            \
    X(unsigned int)          \
    X(signed long)           \
    X(unsigned long)

template<class Inst>
class X86Emitter {
 private:
    static void emit_(X86Assembler *cc, uint32_t lhs, X86Register rhs);
 public:
#define D(int_type) static void emit(X86Assembler *cc, int_type lhs, X86Register rhs);
    FOR_EACH_INT_TYPE(D)
#undef D
    static void emit_imm(X86Assembler *cc, X86Register rhs, none);
    static void emit_imm(X86Assembler *cc, X86Register rhs, uint8_t);

    template <class Mem>
    static void emit(X86Assembler *cc, uint32_t lhs, Mem rhs);
    static void emit(X86Assembler *cc, X86Register lhs, X86Register rhs);
    static void emit_reg(X86Assembler *cc,  X86Register lhs, X86Register rhs, none);
    static void emit_reg(X86Assembler *cc,  X86Register lhs, X86Register rhs, uint8_t);
    template <class Mem>
    static void emit(X86Assembler *cc, X86Register lhs, Mem rhs);
    template <class Mem>
    static void emit(X86Assembler *cc, Mem lhs, X86Register rhs);
};

template<class Inst>
class X86ShiftEmitter {
public:
    template <class Mem>
    static void emit(X86Assembler *cc, X86Register, Mem);
    template <class Mem>
    static void emit(X86Assembler *cc, uint8_t, Mem);
};

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

    void rel32(uintptr_t label) {
        word(((uint8_t*)label) - out - 4);
    }

    void modrm(uint8_t mod, uint8_t reg, uint8_t rm) {
        ASSERT(!(mod & (~0x3)));
        ASSERT(!(reg & (~0x7)));
        ASSERT(!(rm  & (~0x7)));
        byte((uint8_t)(mod << 6)|(reg << 3)|(rm));
    }

    void sib(uint8_t scale, uint8_t index, uint8_t base) {
        modrm(scale, index, base);
    }

#define INSTRUCTION(fn, cls)                                    \
    template <class Tl, class Tr>                               \
    void fn(Tl lhs, Tr rhs) {                                   \
        X86Emitter<X86##cls>::emit(this, lhs, rhs);             \
    }
    INSTRUCTION(test, Test);
    INSTRUCTION(add_, Add);
    INSTRUCTION(mov, Mov);
    INSTRUCTION(cmp, Cmp);
    INSTRUCTION(xor_, Xor);
    INSTRUCTION(or_, Or);
    INSTRUCTION(sub_, Sub);
    INSTRUCTION(lea, Lea);
    INSTRUCTION(and_, And);

#define SHIFT(fn, cls)                                                  \
    template <class Tl, class Tr>                                       \
        void fn(Tl lhs, Tr rhs) {                                       \
        X86ShiftEmitter<X86##cls>::emit(this, lhs, rhs);                \
    }
    SHIFT(shl, SHL);
    SHIFT(shr, SHR);
    SHIFT(sar, SAR);

    template<class Mem> void imul(Mem lhs, X86Register rhs);
    template<class Mem> void imul(uint32_t lhs, Mem rhs, X86Register dst);
    template<class Mem> void idiv(Mem rhs);
    void cdq();

    void call(uintptr_t addr);
    template<class Mem> void call(Mem target);

    template<class Mem> void inc(Mem target);

    void jmp(int8_t off);
    void jmp(X86Label8 &l);
    void jmp(uint8_t *addr);
    template<class Mem> void jmp(Mem target);

    void jcc(cc_t cc, int8_t off);
    void jcc(cc_t cc, uint8_t *addr);
    void jcc(cc_t cc, X86Label8 &l);

    template<class Mem> void setcc(cc_t cc, Mem target);

    void bind(X86Label8 *l);

 private:
    template<class Mem> void call(Mem target, true_type);
    template<class Mem> void call(Mem target, false_type);
};

class X86Label8 {
    enum {
        NEW,
        REFERENCED,
        BOUND
    } state;
    uint8_t *reference;
    int8_t addend;
public:
    X86Label8() : state(NEW) {};
    void referenced(uint8_t *addr, int8_t addend) {
        ASSERT(state == NEW);
        state = REFERENCED;
        reference = addr;
        this->addend = addend;
    }

    void bind(uint8_t *addr) {
        ASSERT(state == REFERENCED);
        state = BOUND;
        *reference = addr - reference + addend;
    }
};

template <class T>
struct is_modrm { const static false_type val; };

class X86Register {
public:
    int val;
    X86Register(int v) : val(v) {};
    void emit(X86Assembler *cc, X86Register reg) {
        cc->modrm(MOD_REG, reg.val, val);
    }
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
template<> struct is_modrm<X86Register> { const static true_type val; };

struct X86ReferenceIndirect {
    X86Register base;
    void emit(X86Assembler *cc, X86Register reg) {
        ASSERT(base.val != REG_DISP32);
        ASSERT(base.val != REG_SIB);
        cc->modrm(MOD_INDIR, reg.val, base.val);
    }
};
template<> struct is_modrm<X86ReferenceIndirect> { const static true_type val; };

struct X86ReferenceIndirect32 {
    X86Register base;
    uint32_t offset;
    void emit(X86Assembler *cc, X86Register reg) {
        ASSERT(base.val != REG_SIB);
        cc->modrm(MOD_INDIR_DISP32, reg.val, base.val);
        cc->word(offset);
    }
};
template<> struct is_modrm<X86ReferenceIndirect32> { const static true_type val; };

struct X86ReferenceIndirect8 {
    X86Register base;
    uint8_t offset;
    void emit(X86Assembler *cc, X86Register reg) {
        ASSERT(base.val != REG_SIB);
        cc->modrm(MOD_INDIR_DISP8, reg.val, base.val);
        cc->byte(offset);
    }
};
template<> struct is_modrm<X86ReferenceIndirect8> { const static true_type val; };

struct X86ReferenceAbs {
    uint32_t address;
    void emit(X86Assembler *cc, X86Register reg) {
        cc->modrm(MOD_INDIR, reg.val, REG_DISP32);
        cc->word(address);
    }
};
template<> struct is_modrm<X86ReferenceAbs> { const static true_type val; };

struct X86ReferenceSIB {
    uint32_t offset;
    X86Register base, index;
    uint8_t scale;
    void emit(X86Assembler *cc, X86Register reg) {
        uint8_t sv;
        switch(scale) {
#define S(n) case n: sv = SCALE_##n
            S(1); S(2); S(4); S(8);
#undef S
        default:
            panic("Illegal scale value: %d", scale);
        }
        cc->modrm(MOD_INDIR_DISP32, reg.val, REG_SIB);
        cc->sib(sv, index.val, base.val);
        cc->word(offset);
    }
};
template<> struct is_modrm<X86ReferenceSIB> { const static true_type val; };

static inline X86ReferenceIndirect X86Mem(X86Register reg) {
    X86ReferenceIndirect r = {reg};
    return r;
}
static inline X86ReferenceIndirect32 X86Mem(X86Register base, uint32_t off) {
    X86ReferenceIndirect32 r = {base, off};
    return r;
}
static inline X86ReferenceIndirect8 X86Mem(X86Register base, uint8_t off) {
    X86ReferenceIndirect8 r = {base, off};
    return r;
}
static inline X86ReferenceAbs X86Mem(uint32_t addr) {
    X86ReferenceAbs r = {addr};
    return r;
}
static inline X86ReferenceSIB X86Mem(uint32_t off, X86Register base, X86Register index, uint8_t scale) {
    X86ReferenceSIB r = {off, base, index, scale};
    return r;
}

template<class Inst>
inline void X86Emitter<Inst>::emit_(X86Assembler *cc, uint32_t lhs, X86Register rhs) {
    emit_imm(cc, rhs, Inst::op_imm_r::val);
    cc->word(lhs);
}

#define D(int_type)                                                     \
    template<class Inst>                                                \
    inline void X86Emitter<Inst>::emit(X86Assembler *cc, int_type lhs,  \
                                       X86Register rhs) {               \
        emit_(cc, (uint32_t)lhs, rhs);                                   \
    }
FOR_EACH_INT_TYPE(D)
#undef D

template<class Inst>
inline void X86Emitter<Inst>::emit_imm(X86Assembler *cc, X86Register rhs, none) {
    cc->byte(Inst::op_imm_rm::val);
    cc->modrm(MOD_REG, Inst::subop_imm_rm::val, rhs.val);
}

template<class Inst>
inline void X86Emitter<Inst>::emit_imm(X86Assembler *cc, X86Register rhs, uint8_t op_imm_r) {
    cc->byte(Inst::op_imm_r::val + rhs.val);
}

template<class Inst>
template<class Mem>
inline void X86Emitter<Inst>::emit(X86Assembler *cc,  uint32_t lhs, Mem rhs) {
    cc->byte(Inst::op_imm_rm::val);
    rhs.emit(cc, X86Register(Inst::subop_imm_rm::val));
    cc->word(lhs);
}

template<class Inst>
inline void X86Emitter<Inst>::emit(X86Assembler *cc,  X86Register lhs, X86Register rhs) {
    emit_reg(cc, lhs, rhs, Inst::op_r_rm::val);
}

template<class Inst>
inline void X86Emitter<Inst>::emit_reg(X86Assembler *cc,  X86Register lhs, X86Register rhs,
                                       none) {
    cc->byte(Inst::op_rm_r::val);
    lhs.emit(cc, rhs);
}

template<class Inst>
inline void X86Emitter<Inst>::emit_reg(X86Assembler *cc,  X86Register lhs, X86Register rhs,
                                       uint8_t) {
    cc->byte(Inst::op_r_rm::val);
    rhs.emit(cc, lhs);
}

/* reg, rm */
template<class Inst>
template<class Mem>
inline void X86Emitter<Inst>::emit(X86Assembler *cc,  X86Register lhs, Mem rhs) {
    cc->byte(Inst::op_r_rm::val);
    rhs.emit(cc, lhs);
}

/* rm, reg */
template<class Inst>
template<class Mem>
inline void X86Emitter<Inst>::emit(X86Assembler *cc,  Mem lhs, X86Register rhs) {
    cc->byte(Inst::op_rm_r::val);
    lhs.emit(cc, rhs);
}

template<class Inst>
template<class Mem>
inline void X86ShiftEmitter<Inst>::emit(X86Assembler *cc, X86Register lhs, Mem rhs) {
    ASSERT(lhs.val == X86ECX.val);
    cc->byte(Inst::op_cl::val);
    rhs.emit(cc, X86Register(Inst::subop_cl::val));
}

template<class Inst>
template<class Mem>
inline void X86ShiftEmitter<Inst>::emit(X86Assembler *cc, uint8_t lhs, Mem rhs) {
    cc->byte(Inst::op_imm::val);
    rhs.emit(cc, X86Register(Inst::subop_imm::val));
    cc->byte(lhs);
}

template <class Mem>
inline void X86Assembler::imul(Mem lhs, X86Register rhs)
{
    byte(0x0f); byte(0xaf);
    lhs.emit(this, rhs);
}

template <class Mem>
inline void X86Assembler::imul(uint32_t lhs, Mem rhs, X86Register dst)
{
    byte(0x69);
    rhs.emit(this, dst);
    word(lhs);
}

template<class Mem>
void X86Assembler::idiv(Mem rhs)
{
    byte(0xf7);
    rhs.emit(this, X86Register(0x7));
}


inline void X86Assembler::cdq()
{
    byte(0x99);
}

inline void X86Assembler::call(uintptr_t addr) {
    byte(0xe8);
    rel32(addr);
}

template<class Mem>
inline void X86Assembler::call(Mem target) {
    call(target, is_modrm<Mem>::val);
}

template<class Mem>
inline void X86Assembler::call(Mem target, true_type) {
    byte(0xff);
    target.emit(this, X86Register(0x2));
}

template<class Mem>
inline void X86Assembler::call(Mem target, false_type) {
    call((uintptr_t)target);
}

template<class Mem>
inline void X86Assembler::inc(Mem target) {
    byte(0xff);
    target.emit(this, X86Register(0x0));
}

inline void X86Assembler::jmp(int8_t off) {
    byte(0xeb);
    byte(off);
}

inline void X86Assembler::jmp(X86Label8 &label) {
    jmp((int8_t)0);
    label.referenced(eip() - 1, -1);
}

inline void X86Assembler::jmp(uint8_t *addr) {
    byte(0xe9);
    rel32((uintptr_t)addr);
}

template<class Mem>
inline void X86Assembler::jmp(Mem target) {
    byte(0xff);
    target.emit(this, X86Register(0x4));
}

inline void X86Assembler::jcc(cc_t cc, int8_t off) {
    byte(0x70 | cc);
    byte(off);
}

inline void X86Assembler::jcc(cc_t cc, uint8_t *addr) {
    byte(0x0f);
    byte(0x80 | cc);
    rel32((uintptr_t)addr);
}

inline void X86Assembler::jcc(cc_t cc, X86Label8 &label) {
    jcc(cc, (int8_t)0);
    label.referenced(eip() - 1, -1);
}

template<class Mem>
inline void X86Assembler::setcc(cc_t cc, Mem target) {
    byte(0x0f);
    byte(0x90 | cc);
    target.emit(this, X86Register(0x0));
}

inline void X86Assembler::bind(X86Label8 *label) {
    label->bind(eip());
}

#endif
