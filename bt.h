#ifndef __BT_H__
#define __BT_H__

#define MAX_BYTES_PER_INSTRUCTION       24
#define MAX_FRAG_SIZE                   10
#define CCBUFF_PROLOGUE_SIZE            26
#define CCBUFF_EPILOGUE_SIZE            11
#define CCBUFF_MAX_SIZE                 (MAX_BYTES_PER_INSTRUCTION * \
                                         MAX_FRAG_SIZE               \
                                         + CCBUFF_EPILOGUE_SIZE      \
                                         + CCBUFF_PROLOGUE_SIZE)
#define PGSIZE                          0x1000

typedef struct {
    byteptr start_pc;
    bdecode  insts[MAX_FRAG_SIZE];
    bool     tail;
    uint8_t  ninsts;
} decode_frag;

typedef struct compiled_frag {
    byteptr start_pc;
    struct compiled_frag *hash_next;
    uint8_t *code;
} compiled_frag;

#define HASH_PC(pc) (((pc) >> 2) & 0xFF)

void bt_run();

#endif
