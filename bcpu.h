#ifndef __BCPU_H__
#define __BCPU_H__

#include <stdint.h>
#include "bdecode.h"

#define XP regs[30]

typedef struct {
    uint32_t PC;
    uint32_t regs[32];
} beta_cpu;

extern beta_cpu CPU;
extern uint32_t *beta_mem;

/*
 * We address memory by words, internally, but the beta uses
 * byte-addressing, even though it only supports aligned access
 */
#define BYTE2WORDADDR(addr) ((addr) >> 2)
#define WORD2BYTEADDR(addr) ((addr) << 2)

void bcpu_execute_one(bdecode *decode);
void bcpu_reset();
void bcpu_step_one();


#endif
