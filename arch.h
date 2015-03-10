#include <stdint.h>

#ifdef __linux__
#include <sys/syscall.h>
#include <asm/ldt.h>
#include <unistd.h>

int modify_ldt(int func, struct user_desc *ptr, unsigned long bytes) {
    return syscall(SYS_modify_ldt, func, ptr, bytes);
}
#elif defined(__APPLE__)
#include <architecture/i386/table.h>
#include <i386/user_ldt.h>
#endif

#include <sys/ucontext.h>
#include <sys/mman.h>

#ifdef __i386__
#ifdef __linux__
static int arch_alloc_segdesc(uintptr_t base, uint32_t pages)
{
    /* FIXME to actually allocate an unused descriptor */
    int segment = 0;
    struct user_desc segdesc = {};
    segdesc.entry_number    = segment;
    segdesc.base_addr       = base;
    segdesc.limit           = pages;
    segdesc.seg_32bit       = 0;
    segdesc.contents        = 0;
    segdesc.read_exec_only  = 0;
    segdesc.limit_in_pages  = 1;
    segdesc.seg_not_present = 0;
    segdesc.useable         = 0;

    if(modify_ldt(1, &segdesc, sizeof(segdesc)) < 0) {
        perror("modify_ldt");
        panic("Unable to modify_ldt to initialize BCPU LDT!");
    }

    return segment;
}
#elif defined(__APPLE__)
static int arch_alloc_segdesc(uintptr_t base, uint32_t pages)
{
    int segment;
    union ldt_entry desc = {
        .data = {
            .limit00 = (pages & 0xffff),
            .base00  = (base  & 0xffff),
            .base16  = (base  >> 16) & 0xff,
            .type    = 0x12,
            .dpl     = 0x3,
            .present = 1,
            .limit16 = (pages >> 16) & 0xff,
            .granular = 1,
            .base24  = base >> 24
        }
    };

    segment = i386_set_ldt(LDT_AUTO_ALLOC, &desc, 1);
    if(segment < 0) {
        perror("i386_set_ldt");
        panic("Unable to initialize the guest CPU LDT!");
    }
    return segment;
}
#endif
#endif

#ifdef __linux__
#ifdef __x86_64__
#define REG(x) REG_##R##x
#else
#define REG(x) REG_##E##x
#endif
static uint8_t *arch_get_ip(ucontext_t *uctx) {
    return (uint8_t*)uctx->uc_mcontext.gregs[REG(IP)];
}
static uint8_t *arch_get_bp(ucontext_t *uctx) {
    return (uint8_t*)uctx->uc_mcontext.gregs[REG(BP)];
}
static uint8_t *arch_get_ax(ucontext_t *uctx) {
    return (uint8_t*)uctx->uc_mcontext.gregs[REG(AX)];
}

static uint8_t *arch_alloc_code_buffer(size_t size) {
    uint8_t *alloc = (uint8_t*)mmap(NULL, size,
                                   PROT_READ|PROT_WRITE|PROT_EXEC,
                                   MAP_PRIVATE|MAP_32BIT|MAP_ANON, -1, 0);
    if (alloc == (uint8_t*)MAP_FAILED)
        return NULL;
    return alloc;
}

#undef REG
#elif defined(__APPLE__)
#ifdef __x86_64__
static uint8_t *arch_get_ip(ucontext_t *uctx) {
    return (uint8_t*)uctx->uc_mcontext->__ss.__rip;
}
static uint8_t *arch_get_bp(ucontext_t *uctx) {
    return (uint8_t*)uctx->uc_mcontext->__ss.__rbp;
}
static uint8_t *arch_get_ax(ucontext_t *uctx) {
    return (uint8_t*)uctx->uc_mcontext->__ss.__rax;
}
extern "C" void bt_continue(void);

static uint8_t *arch_alloc_code_buffer(size_t size) {
    void *hint = (void*)((uintptr_t)bt_continue - (1ul<<32));
    uint8_t *alloc = (uint8_t*)mmap(hint, size,
                                    PROT_READ|PROT_WRITE|PROT_EXEC,
                                    MAP_PRIVATE|MAP_ANON, -1, 0);
    if (alloc == (uint8_t*)MAP_FAILED)
        return NULL;
    if (((uintptr_t)alloc - (uintptr_t)bt_continue) > (uintptr_t)(uint32_t)-1) 
        return NULL;
    return alloc;
}

#endif
#endif
