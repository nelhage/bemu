#ifndef __BCPU_H__
#define __BCPU_H__

#include <stdint.h>
#include "bdecode.h"

#define PC_SUPERVISOR   0x80000000
#define ISR_RESET       (PC_SUPERVISOR | 0x00000000)
#define ISR_ILLOP       (PC_SUPERVISOR | 0x00000004)

#define XP regs[30]

typedef struct {
    uint32_t PC;
    uint32_t regs[32];
    bool     halt;
} beta_cpu;

extern beta_cpu CPU;
extern uint32_t *beta_mem;

/*
 * We address memory by words, internally, but the beta uses
 * byte-addressing, even though it only supports aligned access
 */
#define BYTE2WORDADDR(addr) ((addr) >> 2)
#define WORD2BYTEADDR(addr) ((addr) << 2)

/* Typedefs to hopefully make it clear which we're using */
typedef uint32_t byteptr;
typedef uint32_t wordptr;


void bcpu_execute_one(bdecode *decode);
void bcpu_reset();
void bcpu_step_one();

static uint32_t beta_read_mem32(byteptr addr) __attribute__((always_inline));
static void beta_write_mem32(byteptr addr, uint32_t val) __attribute__((always_inline));

static inline uint32_t beta_read_mem32(byteptr addr) {
    return beta_mem[BYTE2WORDADDR(addr & ~PC_SUPERVISOR)];
}

static inline void beta_write_mem32(byteptr addr, uint32_t val) {
    beta_mem[BYTE2WORDADDR(addr & ~PC_SUPERVISOR)] = val;
}

#endif
