STDOUT = 1
STDIN = 0

.text
.global main
main:
	call start
	pushl $0
	call exit
# TODO
.data
	.comm Assign1_x, 4, 4
	nop
.data
	.comm Assign1_y, 4, 4
	nop
.data
	.comm Assign1_z, 4, 4
	nop
.data
	.comm Assign1_b1, 4, 4
	nop
.data
	.comm Assign1_b2, 4, 4
	nop
.text
start:
# test_files/phase4/basic/assign1.ood:14,5: x
	pushl $5
	popl %eax
	movl %eax, (Assign1_x)
# test_files/phase4/basic/assign1.ood:15,5: y
	pushl (Assign1_x)
	pushl $8
	pushl (Assign1_x)
	popl %ebx
	popl %eax
	subl %ebx, %eax
	pushl %eax
	popl %ebx
	popl %eax
	movl $0, %edx
	mull %ebx
	pushl %eax
	popl %eax
	movl %eax, (Assign1_y)
# test_files/phase4/basic/assign1.ood:17,5: b1
	pushl $1
	popl %eax
	movl %eax, (Assign1_b1)
# test_files/phase4/basic/assign1.ood:18,5: b2
	pushl $0
	popl %eax
	xorl $1, %eax
	pushl %eax
	popl %eax
	movl %eax, (Assign1_b2)
# test_files/phase4/basic/assign1.ood:19,5: b2
	pushl (Assign1_b2)
	popl %eax
	xorl $1, %eax
	pushl %eax
	pushl (Assign1_b1)
	popl %ebx
	popl %eax
	orl %ebx, %eax
	pushl %eax
	popl %eax
	xorl $1, %eax
	pushl %eax
	popl %eax
	movl %eax, (Assign1_b2)
# test_files/phase4/basic/assign1.ood:21,8: .
	pushl (Assign1_y)
	call writeint
	popl %eax
# test_files/phase4/basic/assign1.ood:22,8: .
	pushl (Assign1_x)
	pushl $2
	popl %ebx
	popl %eax
	movl $0, %edx
	divl %ebx
	pushl %eax
	call writeint
	popl %eax
# test_files/phase4/basic/assign1.ood:24,5: y
	pushl (Assign1_y)
	pushl (Assign1_x)
	popl %ebx
	popl %eax
	subl %ebx, %eax
	pushl %eax
	popl %eax
	movl %eax, (Assign1_y)
# test_files/phase4/basic/assign1.ood:25,8: .
	pushl (Assign1_y)
	pushl $4
	popl %ebx
	popl %eax
	addl %ebx, %eax
	pushl %eax
	call writeint
	popl %eax
# test_files/phase4/basic/assign1.ood:26,8: .
	pushl $9
	pushl $5
	popl %ebx
	popl %eax
	subl %ebx, %eax
	pushl %eax
	popl %eax
	negl %eax
	pushl %eax
	call writeint
	popl %eax
# test_files/phase4/basic/assign1.ood:27,8: .
	pushl $5
	pushl $2
	pushl $3
	popl %ebx
	popl %eax
	movl $0, %edx
	mull %ebx
	pushl %eax
	popl %ebx
	popl %eax
	subl %ebx, %eax
	pushl %eax
	pushl $1
	popl %ebx
	popl %eax
	addl %ebx, %eax
	pushl %eax
	call writeint
	popl %eax
	ret


