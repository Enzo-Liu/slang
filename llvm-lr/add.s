	.text
	.file	"<string>"
	.globl	add                     # -- Begin function add
	.p2align	4, 0x90
	.type	add,@function
add:                                    # @add
	.cfi_startproc
# BB#0:                                 # %entry
                                        # kill: %ESI<def> %ESI<kill> %RSI<def>
                                        # kill: %EDI<def> %EDI<kill> %RDI<def>
	leal	(%rdi,%rsi), %eax
	retq
.Lfunc_end0:
	.size	add, .Lfunc_end0-add
	.cfi_endproc
                                        # -- End function
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
                                        # kill: %ESI<def> %ESI<kill> %RSI<def>
                                        # kill: %EDI<def> %EDI<kill> %RDI<def>
	leal	(%rdi,%rsi), %eax
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc
                                        # -- End function

	.section	".note.GNU-stack","",@progbits
