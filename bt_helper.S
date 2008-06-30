.text
.globl bt_callout
.globl bt_start
.globl bt_enter

bt_callout:
        mov %eax, CPU
        testl $0x80000000, %eax
        jnz bt_start
        testl $0xFFFFFFFF, pending_interrupts
        jz  bt_start
        call bcpu_process_interrupt
bt_start:
        mov bt_stack_base, %esp
        jmp bt_translate_and_run

bt_enter:
        mov 4(%esp), %eax
        mov $(CPU + 4), %ebp
        jmp *%eax
