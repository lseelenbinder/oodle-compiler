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
	.comm breakit_mov, 4, 4
	nop
.data
	.comm breakit_x, 4, 4
	nop
.data
	.comm breakit_y, 4, 4
	nop
.text
start:
# test_files/phase4/basic/breakit.ood:9,5: mov
	pushl $12
# test_files/phase4/basic/breakit.ood:9,27: (
	call readint
	pushl %eax
	popl %ebx
	popl %eax
	movl $0, %edx
	imull %ebx
	pushl %eax
	popl %eax
	movl %eax, (breakit_mov)
# test_files/phase4/basic/breakit.ood:10,8: .
	pushl (breakit_mov)
	call writeint
	popl %ebx
# test_files/phase4/basic/breakit.ood:12,5: mov
	pushl (breakit_mov)
	pushl $1
	popl %eax
	negl %eax
	pushl %eax
	popl %ebx
	popl %eax
	movl $0, %edx
	idivl %ebx
	pushl %eax
	popl %eax
	movl %eax, (breakit_mov)
# test_files/phase4/basic/breakit.ood:13,8: .
	pushl (breakit_mov)
	call writeint
	popl %ebx
# test_files/phase4/basic/breakit.ood:15,5: mov
	pushl (breakit_mov)
	pushl $1
	popl %eax
	negl %eax
	pushl %eax
	popl %ebx
	popl %eax
	movl $0, %edx
	imull %ebx
	pushl %eax
	popl %eax
	movl %eax, (breakit_mov)
# test_files/phase4/basic/breakit.ood:16,8: .
	pushl (breakit_mov)
	call writeint
	popl %ebx
# test_files/phase4/basic/breakit.ood:18,5: if
	pushl $1
	popl %eax
	cmp $1, %eax
	jnz startFalsestart_18
# test_files/phase4/basic/breakit.ood:19,7: x
	pushl $1
	popl %eax
	movl %eax, (breakit_x)
# test_files/phase4/basic/breakit.ood:20,7: loop
startLoopstart_20:
	pushl (breakit_x)
	pushl $3
	popl %ebx
	popl %eax
	cmp %ebx, %eax
	jg nostart_20
	pushl $0
	jmp yesstart_20
nostart_20:
	pushl $1
yesstart_20:
	popl %eax
	xorl $1, %eax
	pushl %eax
	popl %eax
	cmp $1, %eax
	jnz doneLoopstart_20
# test_files/phase4/basic/breakit.ood:21,9: y
	pushl $2
	popl %eax
	movl %eax, (breakit_y)
# test_files/phase4/basic/breakit.ood:22,9: loop
startLoopstart_22:
	pushl (breakit_y)
	pushl $2
	popl %eax
	negl %eax
	pushl %eax
	popl %ebx
	popl %eax
	cmp %ebx, %eax
	jg nostart_22
	pushl $0
	jmp yesstart_22
nostart_22:
	pushl $1
yesstart_22:
	popl %eax
	cmp $1, %eax
	jnz doneLoopstart_22
# test_files/phase4/basic/breakit.ood:23,11: y
	pushl (breakit_y)
	pushl $1
	popl %ebx
	popl %eax
	subl %ebx, %eax
	pushl %eax
	popl %eax
	movl %eax, (breakit_y)
# test_files/phase4/basic/breakit.ood:24,11: if
	pushl (breakit_y)
	pushl $0
	popl %ebx
	popl %eax
	cmp %ebx, %eax
	jge nostart_24
	pushl $0
	jmp yesstart_24
nostart_24:
	pushl $1
yesstart_24:
	popl %eax
	xorl $1, %eax
	pushl %eax
	popl %eax
	cmp $1, %eax
	jnz startFalsestart_24
# test_files/phase4/basic/breakit.ood:25,16: .
	pushl (breakit_y)
	call writeint
	popl %ebx
	jmp endFalsestart_24
startFalsestart_24:
endFalsestart_24:
	jmp startLoopstart_22
doneLoopstart_22:
# test_files/phase4/basic/breakit.ood:28,12: .
	pushl (breakit_x)
	call writeint
	popl %ebx
# test_files/phase4/basic/breakit.ood:29,9: x
	pushl (breakit_x)
	pushl $1
	popl %ebx
	popl %eax
	addl %ebx, %eax
	pushl %eax
	popl %eax
	movl %eax, (breakit_x)
	jmp startLoopstart_20
doneLoopstart_20:
	jmp endFalsestart_18
startFalsestart_18:
endFalsestart_18:
	ret


