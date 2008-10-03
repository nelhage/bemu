.text

#ifdef __APPLE__
#define _(x) _##x
#else
#define _(x) x
#endif

.globl _(bt_continue)
.globl _(bt_continue_chain)
.globl _(bt_enter)
.globl _(bt_interrupt)

_(bt_interrupt):
        mov %eax, 128(%ebp)
        mov _(bt_stack_base), %esp
        call _(bcpu_process_interrupt)
        pushl $0
        call _(bt_translate_and_run)

_(bt_continue):
        pushl $0
_(bt_continue_chain):
        mov %eax, 128(%ebp)
        call _(bt_translate_and_run)

_(bt_enter):
        mov 4(%esp), %ecx
        mov _(bt_stack_base), %esp
        mov $_(CPU), %ebp
        mov 128(%ebp), %eax
        jmp *%ecx
