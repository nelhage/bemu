#include "bemu.h"

int main()
{
    uint8_t buf[100];
    uint8_t *ptr = buf;

    X86_ADD_IMM32_RM32(ptr, MOD_REG, REG_EAX);
    X86_4BYTE(ptr, 0x12345678);

    X86_ADD_IMM32_RM32(ptr, MOD_INDIR_DISP32, REG_SIB);

    /* SIB */
    X86_SIB(ptr, SCALE_8, REG_ECX, REG_EDI);
    X86_DISP32(ptr, 0xAAAAAAAA);
    X86_IMM32(ptr, 0xBBBBBBBB);

    X86_ADD_R32_RM32(ptr, REG_EBP, MOD_REG, REG_ECX);
    X86_ADD_RM32_R32(ptr, MOD_REG, REG_ECX, REG_EBP);

    return 0;
}
