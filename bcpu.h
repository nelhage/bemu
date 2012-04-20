#ifndef __BCPU_H__
#define __BCPU_H__

#include <stdint.h>
#include "bdecode.h"

#define PC_SUPERVISOR   0x80000000
#define ISR_RESET       (PC_SUPERVISOR | 0x00000000)
#define ISR_ILLOP       (PC_SUPERVISOR | 0x00000004)
#define ISR_CLK         (PC_SUPERVISOR | 0x00000008)
#define ISR_KBD         (PC_SUPERVISOR | 0x0000000C)
#define ISR_MOUSE       (PC_SUPERVISOR | 0x00000010)

/* Interrupt flags in 'pending_interrupts' */
#define INT_CLK         0x0001
#define INT_KBD         0x0002
#define INT_MOUSE       0x0004

/*
 * We address memory by words, internally, but the beta uses
 * byte-addressing, even though it only supports aligned access
 */
#define BYTE2WORDADDR(addr) ((addr) >> 2)
#define WORD2BYTEADDR(addr) ((addr) << 2)

/* Typedefs to hopefully make it clear which we're using */
typedef uint32_t byteptr;
typedef uint32_t wordptr;

#define XP 30
#define SP 29
#define BP 27

class beta_cpu {
 public:
    /* This layout is hard-coded into bt.S */
    uint32_t regs[32];
    uint32_t PC;
    uint32_t halt;
    uint32_t *memory;
    uint32_t memsize;
    uint32_t segment;
    uint32_t pending_interrupts;
    uint32_t inst_count;
    uint32_t opcode_counts[256];

    void process_interrupt();
    void reset();
    void execute_one(bdecode *decode);
    void step_one();

    inline uint32_t read_mem32(byteptr addr) {
        if((addr & ~PC_SUPERVISOR) >= memsize) {
            panic("Illegal memory reference %08x", addr);
        }
        return memory[BYTE2WORDADDR(addr & ~PC_SUPERVISOR)];
    }

    inline void write_mem32(byteptr addr, uint32_t val) {
        if((addr & ~PC_SUPERVISOR) >= memsize) {
            panic("Illegal memory write %08x", addr);
        }
        memory[BYTE2WORDADDR(addr & ~PC_SUPERVISOR)] = val;
    }

    inline void set_interrupt(int i) {
        pending_interrupts |= i;
    }

    inline void clear_interrupt(int i) {
        pending_interrupts &= ~i;
    }

 private:
    inline void write_reg(beta_reg reg, uint32_t val)
    {
        regs[reg] = val;
    }
};

#endif
