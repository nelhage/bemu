#ifndef __BT_H__
#define __BT_H__

/* XXX FIXME */
#define MAX_BYTES_PER_INSTRUCTION       20
#define MAX_FRAG_SIZE                   10
#define CCBUFF_EPILOGUE_SIZE            8
#define CCBUFF_MAX_SIZE                 (MAX_BYTES_PER_INSTRUCTION * \
                                         MAX_FRAG_SIZE               \
                                         + CCBUFF_EPILOGUE_SIZE)

typedef struct {
    byteptr start_pc;
    bdecode  insts[MAX_FRAG_SIZE];
    uint8_t  ninsts;
} decode_frag;

typedef struct compiled_frag {
    uint8_t code[CCBUFF_MAX_SIZE];
    byteptr start_pc;
    struct compiled_frag *hash_next;
} compiled_frag;

#define HASH_PC(pc) (((pc) >> 2) & 0xFF)

void bt_run();

#endif
