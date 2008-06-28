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
    uint32_t start_pc;
    bdecode  insts[MAX_FRAG_SIZE];
    uint8_t  ninsts;
} decode_frag;

void bt_translate_frag(ccbuff buf, decode_frag frag);
void bt_run();

#endif
