.text
	.globl main
main:
#On rentre dans la fonction main
	pushq %rbp
	mov %rsp, %rbp
	mov $42, %rax
	mov %rbp, %rsp
	popq %rbp
	ret
.data
