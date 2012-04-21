.text

#define SP 29
#define BP 27

#ifdef __APPLE__
#define _(x) _##x
#else
#define _(x) x
#endif

.globl _(bt_interp_one)
.globl _(bt_continue)
.globl _(bt_continue_chain)
.globl _(bt_continue_ic)
.globl _(bt_enter)
.globl _(bt_interrupt)

.macro LOAD_CPU pc=1
.if \pc
        mov 128(%ebp), %eax
.endif
        mov (4*SP)(%ebp), %ebx
        mov (4*BP)(%ebp), %edx
        mov (4*0)(%ebp),  %esi
        mov (4*1)(%ebp),  %edi
.endm

.macro SAVE_CPU pc=1
.if \pc
        mov %eax, 128(%ebp)
.endif
        mov %ebx, (4*SP)(%ebp)
        mov %edx, (4*BP)(%ebp)
        mov %esi, (4*0)(%ebp)
        mov %edi, (4*1)(%ebp)
.endm

_(bt_interp_one):
        SAVE_CPU 0
        subl $4, %esp
        movl %ebp, %eax
        call _(bt_step_one)
        addl $4, %esp
        LOAD_CPU
        ret

_(bt_interrupt):
        SAVE_CPU
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
        SAVE_CPU
        popl %ecx
        movl $1, %edx
        movl %ebp, %eax
        call _(bt_translate_and_run)

_(bt_continue_ic):
        SAVE_CPU
        popl %ecx
        xorl %edx, %edx
        movl %ebp, %eax
        call _(bt_translate_and_run)

_(bt_enter):
        mov 4(%esp), %ecx
        mov _(bt_stack_base), %esp
        subl $8, %esp
        mov $_(CPU), %ebp
        LOAD_CPU
        jmp *%ecx
