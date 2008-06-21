#include "bemu.h"

int main()
{
    uint8_t buf[100];
    uint8_t *ptr = buf;

    X86_ADD_IMM32_MR32(ptr, MOD_REG, REG_EAX);
    X86_4BYTE(ptr, 0x12345678);

    X86_ADD_IMM32_MR32(ptr, MOD_INDIR_DISP32, REG_SIB);

    /* SIB */
    X86_SIB(ptr, SCALE_8, REG_ECX, REG_EDI);
    X86_DISP32(ptr, 0xAAAAAAAA);
    X86_IMM32(ptr, 0xBBBBBBBB);

    return 0;
}
