.text
.globl bt_callout
.globl bt_start
.globl bt_enter
.globl bt_interrupt

bt_interrupt:
        mov bt_stack_base, %esp
        call bcpu_process_interrupt
        jmp bt_translate_and_run

bt_callout:
        mov %eax, CPU
bt_start:
        mov bt_stack_base, %esp
        jmp bt_translate_and_run

bt_enter:
        mov 4(%esp), %eax
        mov $(CPU + 4), %ebp
        jmp *%eax
