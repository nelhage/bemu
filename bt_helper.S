.text

#ifdef __APPLE__
#define _(x) _##x
#else
#define _(x) x
#endif

.globl _(bt_continue)
.globl _(bt_continue_chain)
.globl _(bt_continue_ic)
.globl _(bt_enter)
.globl _(bt_interrupt)

_(bt_interrupt):
        mov %eax, 128(%ebp)
        mov _(bt_stack_base), %esp
        movl %ebp, %eax
        call _(bt_process_interrupt)
        xorl %edx, %edx
        xorl %ecx, %ecx
        movl %ebp, %eax
        call _(bt_translate_and_run)

_(bt_continue):
        mov %eax, %ecx

        /* Try a fast-path lookup of PC in the frag hash table. */
        andl  $0x3fc, %ecx
        movl  _(bt_frag_hash)(%ecx), %ecx
        testl %ecx, %ecx                /* non-NULL frag */
        jz miss
        cmpl  %eax, (%ecx)              /* Compare PC */
        jne miss
        movl 8(%ecx), %ecx

        cmp $0, 132(%ebp)               /* Check CPU.halt */
        jnz miss

        jmp *%ecx

miss:
        pushl $0
_(bt_continue_chain):
        mov %eax, 128(%ebp)
        popl %ecx
        movl $1, %edx
        movl %ebp, %eax
        call _(bt_translate_and_run)

_(bt_continue_ic):
        mov %eax, 128(%ebp)
        popl %ecx
        xorl %edx, %edx
        movl %ebp, %eax
        call _(bt_translate_and_run)

_(bt_enter):
        mov 4(%esp), %ecx
        mov _(bt_stack_base), %esp
        subl $8, %esp
        mov $_(CPU), %ebp
        mov 128(%ebp), %eax
        jmp *%ecx
