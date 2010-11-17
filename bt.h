#ifndef __BT_H__
#define __BT_H__

#define MAX_BYTES_PER_INSTRUCTION       24
#define MAX_FRAG_SIZE                   10
#define PC_CHECK_SIZE                   12
#define CCBUFF_PROLOGUE_SIZE            26
#define CCBUFF_EPILOGUE_SIZE            11
#define CCBUFF_MAX_SIZE                 (MAX_BYTES_PER_INSTRUCTION * \
                                         MAX_FRAG_SIZE               \
                                         + CCBUFF_EPILOGUE_SIZE      \
                                         + CCBUFF_PROLOGUE_SIZE      \
                                         + PC_CHECK_SIZE)
#define PGSIZE                          0x1000

struct decode_frag {
    byteptr start_pc;
    bdecode  insts[MAX_FRAG_SIZE];
    bool     tail;
    uint8_t  ninsts;
};

struct compiled_frag {
    byteptr start_pc;
    compiled_frag *hash_next;
    uint8_t *code;
};

struct fault_entry {
    uint8_t *eip;
    byteptr  pc;
};

#define HASH_PC(pc) (((pc) >> 2) & 0xFF)

void bt_run(beta_cpu *cpu);

#endif
