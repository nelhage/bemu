.text
.globl bt_callout
.globl bt_start
.globl bt_enter

bt_callout:
        mov %eax, CPU
bt_start:
        mov bt_stack_base, %esp
        jmp bt_translate_and_run

bt_enter:
        mov 4(%esp), %eax
        mov $(CPU + 4), %ebp
        jmp *%eax
