.text
.globl bt_continue
.globl bt_continue_chain
.globl bt_enter
.globl bt_interrupt

bt_interrupt:
        mov %eax, 128(%ebp)
        mov bt_stack_base, %esp
        call bcpu_process_interrupt
        pushl $0
        call bt_translate_and_run

bt_continue:
        pushl $0
bt_continue_chain:
        mov %eax, 128(%ebp)
        call bt_translate_and_run

bt_enter:
        mov 4(%esp), %ecx
        mov bt_stack_base, %esp
        mov $CPU, %ebp
        mov 128(%ebp), %eax
        jmp *%ecx
