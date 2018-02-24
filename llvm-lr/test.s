	.text
	.file	"<string>"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Lcfi0:
	.cfi_def_cfa_offset 16
	movl	$t, %edi
	callq	puts
	movl	$1, %eax
	popq	%rcx
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	t,@object               # @t
	.section	.rodata,"a",@progbits
	.globl	t
t:
	.asciz	"test"
	.size	t, 5


	.section	".note.GNU-stack","",@progbits
