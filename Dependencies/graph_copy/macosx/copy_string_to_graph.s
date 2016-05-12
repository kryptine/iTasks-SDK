	.section	__TEXT,__text,regular,pure_instructions
	
	.section	__TEXT,__literal16,16byte_literals
	.align	4
LCPI0_0:
	.quad	-1                      ## 0xffffffffffffffff
	.quad	-2                      ## 0xfffffffffffffffe
LCPI0_1:
	.quad	-3                      ## 0xfffffffffffffffd
	.quad	-4                      ## 0xfffffffffffffffc
LCPI0_2:
	.quad	-2                      ## 0xfffffffffffffffe
	.quad	-3                      ## 0xfffffffffffffffd
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_copy_string_to_graph
	.align	4, 0x90
_copy_string_to_graph:                  ## @copy_string_to_graph
	
## BB#0:
	pushq	%rbp
Ltmp0:
	
Ltmp1:
	
	movq	%rsp, %rbp
Ltmp2:
	
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$160, %rsp
Ltmp3:
	
Ltmp4:
	
Ltmp5:
	
Ltmp6:
	
Ltmp7:
	
	movq	%rcx, -56(%rbp)         ## 8-byte Spill
	movq	%rdx, %r10
	movq	%r10, -64(%rbp)         ## 8-byte Spill
	movq	%rdi, %r12
	addq	$16, %r12
	subq	%rsi, %rdx
	cmpq	$8, %rdx
	jge	LBB0_4
## BB#1:
	movq	%r10, %rbx
	jmp	LBB0_2
LBB0_4:                                 ## %.lr.ph316.preheader
	sarq	$3, %rdx
	leaq	-48(%rbp), %r15
	movq	__ARRAY__@GOTPCREL(%rip), %rdi
	movdqa	LCPI0_2(%rip), %xmm0    ## xmm0 = [18446744073709551614,18446744073709551613]
	movq	dINT@GOTPCREL(%rip), %rax
	addq	$2, %rax
	movq	%rax, -88(%rbp)         ## 8-byte Spill
	movq	CHAR@GOTPCREL(%rip), %rax
	addq	$2, %rax
	movq	%rax, -96(%rbp)         ## 8-byte Spill
	movq	BOOL@GOTPCREL(%rip), %rax
	addq	$2, %rax
	movq	%rax, -104(%rbp)        ## 8-byte Spill
	movq	REAL@GOTPCREL(%rip), %rax
	addq	$2, %rax
	movq	%rax, -112(%rbp)        ## 8-byte Spill
	movq	__STRING__@GOTPCREL(%rip), %rax
	addq	$2, %rax
	movq	%rax, -120(%rbp)        ## 8-byte Spill
	movdqa	LCPI0_0(%rip), %xmm1    ## xmm1 = [18446744073709551615,18446744073709551614]
	movdqa	LCPI0_1(%rip), %xmm2    ## xmm2 = [18446744073709551613,18446744073709551612]
	movq	%r12, %r14
	movq	%r10, %r8
	movq	%r10, %r9
	jmp	LBB0_5
LBB0_15:                                ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$1, %rdx
	jle	LBB0_16
## BB#17:                               ##   in Loop: Header=BB0_5 Depth=1
	addq	$-2, %rdx
	movq	%rax, 8(%rsi)
	addq	$16, %rsi
	addq	$16, %r14
	jmp	LBB0_349
LBB0_20:                                ##   in Loop: Header=BB0_5 Depth=1
	cmpq	-104(%rbp), %rcx        ## 8-byte Folded Reload
	je	LBB0_22
## BB#21:                               ##   in Loop: Header=BB0_5 Depth=1
	cmpq	-112(%rbp), %rcx        ## 8-byte Folded Reload
	je	LBB0_22
## BB#25:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	%r10, %r13
	cmpq	-120(%rbp), %rcx        ## 8-byte Folded Reload
	jne	LBB0_48
## BB#26:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	%rsi, %r10
	movq	8(%r14), %rsi
	leaq	16(%r14), %r12
	leaq	7(%rsi), %rbx
	movq	%rbx, %rax
	shrq	$3, %rax
	addq	$-2, %rdx
	subq	%rax, %rdx
	js	LBB0_27
## BB#28:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	%rdx, -80(%rbp)         ## 8-byte Spill
	movq	%rsi, 8(%r10)
	leaq	16(%r10), %r11
	testq	%rax, %rax
	je	LBB0_47
## BB#29:                               ## %.lr.ph.i.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$32, %rbx
	movl	$0, %edi
	jb	LBB0_40
## BB#30:                               ## %min.iters.checked999
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rax, %r15
	movabsq	$2305843009213693948, %rdx ## imm = 0x1FFFFFFFFFFFFFFC
	andq	%rdx, %r15
	movl	$0, %edi
	je	LBB0_40
## BB#31:                               ## %vector.memcheck1018
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rbx, %rdx
	andq	$-8, %rdx
	leaq	8(%r14,%rdx), %rdi
	cmpq	%rdi, %r11
	ja	LBB0_33
## BB#32:                               ## %vector.memcheck1018
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	8(%r10,%rdx), %rdx
	cmpq	%rdx, %r12
	movl	$0, %edi
	jbe	LBB0_40
LBB0_33:                                ## %vector.body995.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	shrq	$5, %rbx
	leaq	-4(,%rbx,4), %rcx
	movl	%ecx, %edx
	shrl	$2, %edx
	incl	%edx
	testb	$3, %dl
	movl	$0, %edi
	je	LBB0_36
## BB#34:                               ## %vector.body995.prol.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	leal	7(%rsi), %edx
	shrl	$3, %edx
	andl	$12, %edx
	addl	$-4, %edx
	shrl	$2, %edx
	incl	%edx
	andl	$3, %edx
	negq	%rdx
	xorl	%edi, %edi
LBB0_35:                                ## %vector.body995.prol
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movdqu	16(%r14,%rdi,8), %xmm3
	movdqu	32(%r14,%rdi,8), %xmm4
	movdqu	%xmm3, 16(%r10,%rdi,8)
	movdqu	%xmm4, 32(%r10,%rdi,8)
	addq	$4, %rdi
	incq	%rdx
	jne	LBB0_35
LBB0_36:                                ## %vector.body995.preheader.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$12, %rcx
	jb	LBB0_39
## BB#37:                               ## %vector.body995.preheader.split.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	shlq	$2, %rbx
	subq	%rdi, %rbx
	leaq	128(%r10,%rdi,8), %rdx
	leaq	128(%r14,%rdi,8), %rdi
LBB0_38:                                ## %vector.body995
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-112(%rdi), %xmm3
	movups	-96(%rdi), %xmm4
	movups	%xmm3, -112(%rdx)
	movups	%xmm4, -96(%rdx)
	movups	-80(%rdi), %xmm3
	movups	-64(%rdi), %xmm4
	movups	%xmm3, -80(%rdx)
	movups	%xmm4, -64(%rdx)
	movups	-48(%rdi), %xmm3
	movups	-32(%rdi), %xmm4
	movups	%xmm3, -48(%rdx)
	movups	%xmm4, -32(%rdx)
	movdqu	-16(%rdi), %xmm3
	movdqu	(%rdi), %xmm4
	movdqu	%xmm3, -16(%rdx)
	movdqu	%xmm4, (%rdx)
	subq	$-128, %rdx
	subq	$-128, %rdi
	addq	$-16, %rbx
	jne	LBB0_38
LBB0_39:                                ## %middle.block996
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	%r15, %rax
	movq	%r15, %rdi
	je	LBB0_47
LBB0_40:                                ## %.lr.ph.i.preheader1259
                                        ##   in Loop: Header=BB0_5 Depth=1
	movl	%eax, %ebx
	subl	%edi, %ebx
	leaq	-1(%rax), %rdx
	subq	%rdi, %rdx
	testb	$7, %bl
	je	LBB0_41
## BB#42:                               ## %.lr.ph.i.prol.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	addl	$7, %esi
	shrl	$3, %esi
	subl	%edi, %esi
	andl	$7, %esi
	negq	%rsi
	movq	%r10, %rcx
	.align	4, 0x90
LBB0_43:                                ## %.lr.ph.i.prol
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	16(%r14,%rdi,8), %rbx
	movq	%rbx, 16(%rcx,%rdi,8)
	incq	%rdi
	incq	%rsi
	jne	LBB0_43
	jmp	LBB0_44
LBB0_196:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	%r9, %rbx
LBB0_197:                               ## %.thread108
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	-72(%rbp), %rax         ## 8-byte Reload
	leaq	8(%rax), %r15
	leaq	-1(%r13), %rsi
	leaq	-16(%r12,%r13,8), %rdx
	movq	%rdx, -16(%rcx,%r13,8)
	leaq	-24(%r12,%r13,8), %rdx
	movq	%rdx, -24(%rcx,%r13,8)
	cmpl	$4, %r13d
	jb	LBB0_205
## BB#198:                              ## %.lr.ph303.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	-4(%r13), %rdi
	leaq	-3(%r13), %rax
	cmpq	$4, %rax
	jb	LBB0_203
## BB#199:                              ## %min.iters.checked881
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rax, %rdx
	andq	$-4, %rdx
	movq	%rax, %r9
	andq	$-4, %r9
	je	LBB0_203
## BB#200:                              ## %vector.body877.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rax, -128(%rbp)        ## 8-byte Spill
	movq	%rsi, -80(%rbp)         ## 8-byte Spill
	subq	%rdx, %rdi
	movq	-72(%rbp), %rax         ## 8-byte Reload
	leaq	-16(%rax,%r13,8), %r10
	leaq	-32(%r8), %rdx
	movq	%r9, %rsi
	movq	%rsi, -136(%rbp)        ## 8-byte Spill
	leaq	-4(%r13), %r9
	.align	4, 0x90
LBB0_201:                               ## %vector.body877
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movd	%r9, %xmm3
	pshufd	$68, %xmm3, %xmm3       ## xmm3 = xmm3[0,1,0,1]
	paddq	%xmm0, %xmm3
	leaq	8(%r10), %rax
	movd	%r10, %xmm4
	movd	%rax, %xmm5
	punpcklqdq	%xmm5, %xmm4    ## xmm4 = xmm4[0],xmm5[0]
	movd	%xmm3, %rax
	leaq	(%r12,%rax,8), %rax
	movd	%rax, %xmm5
	pshufd	$78, %xmm3, %xmm3       ## xmm3 = xmm3[2,3,0,1]
	movd	%xmm3, %rax
	leaq	(%r12,%rax,8), %rax
	movd	%rax, %xmm3
	punpcklqdq	%xmm5, %xmm3    ## xmm3 = xmm3[0],xmm5[0]
	movdqu	%xmm4, (%rdx)
	movdqu	%xmm3, -16(%rdx)
	addq	$-32, %r10
	addq	$-32, %rdx
	addq	$-4, %r9
	addq	$-4, %rsi
	jne	LBB0_201
## BB#202:                              ## %middle.block878
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	-136(%rbp), %rax        ## 8-byte Reload
	cmpq	%rax, -128(%rbp)        ## 8-byte Folded Reload
	movq	-64(%rbp), %r10         ## 8-byte Reload
	movq	-80(%rbp), %rsi         ## 8-byte Reload
	je	LBB0_205
LBB0_203:                               ## %.lr.ph303.preheader1254
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	-72(%rbp), %rax         ## 8-byte Reload
	leaq	24(%rax,%rdi,8), %rdx
	incq	%rdi
	shlq	$3, %r13
	subq	%r13, %r8
	.align	4, 0x90
LBB0_204:                               ## %.lr.ph303
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	%rdx, (%r8,%rdi,8)
	addq	$-8, %rdx
	decq	%rdi
	jg	LBB0_204
LBB0_205:                               ## %._crit_edge.304
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	(%r12,%rsi,8), %rsi
	addq	$8, %r14
	movq	%r14, %r12
	movq	__ARRAY__@GOTPCREL(%rip), %rdi
	movq	%r11, %rdx
	jmp	LBB0_353
LBB0_322:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	%rsi, %rdi
	subq	%rcx, %rdi
	movq	%rax, -72(%rbp)         ## 8-byte Spill
	leaq	16(%rax), %r11
	cmpq	$2, %rdi
	movq	-64(%rbp), %r10         ## 8-byte Reload
	jl	LBB0_338
## BB#323:                              ##   in Loop: Header=BB0_5 Depth=1
	movl	$1, %eax
	subq	%rdi, %rax
	leaq	(%r8,%rax,8), %rdx
	cmpq	%r9, %rdx
	jae	LBB0_326
## BB#324:                              ##   in Loop: Header=BB0_5 Depth=1
	subq	%rdx, %r9
	sarq	$3, %r9
	subq	%r9, %rbx
	movq	%rdx, %r9
	js	LBB0_325
LBB0_326:                               ## %.thread116
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	-16(%r11,%rdi,8), %rax
	movq	%rax, -16(%rdx,%rdi,8)
	cmpq	$3, %rdi
	jl	LBB0_327
## BB#328:                              ## %.lr.ph305.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rdx, -136(%rbp)        ## 8-byte Spill
	addq	$-3, %rdi
	movq	%rcx, %rax
	movq	%rax, -128(%rbp)        ## 8-byte Spill
	leaq	2(%rax), %rcx
	movq	%rcx, -144(%rbp)        ## 8-byte Spill
	movzbl	%r13b, %r14d
	subq	%r14, %rcx
	cmpq	$-2, %rcx
	movq	$-1, %rdx
	cmovleq	%rdx, %rcx
	leaq	-1(%rcx,%r14), %rcx
	subq	%rax, %rcx
	cmpq	$4, %rcx
	jae	LBB0_330
## BB#329:                              ##   in Loop: Header=BB0_5 Depth=1
	movq	-136(%rbp), %r14        ## 8-byte Reload
	jmp	LBB0_335
LBB0_213:                               ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$2, %rdx
	jle	LBB0_50
## BB#214:                              ##   in Loop: Header=BB0_5 Depth=1
	leaq	-3(%rdx), %rcx
	cmpl	$1, %r11d
	je	LBB0_217
## BB#215:                              ##   in Loop: Header=BB0_5 Depth=1
	testw	%r11w, %r11w
	jne	LBB0_218
## BB#216:                              ##   in Loop: Header=BB0_5 Depth=1
	movq	8(%r14), %rax
	movq	%rax, 8(%rsi)
	movq	16(%r14), %rax
	movq	%rax, 16(%rsi)
	addq	$24, %r14
	addq	$24, %rsi
	movq	%rcx, %rdx
	jmp	LBB0_349
LBB0_224:                               ##   in Loop: Header=BB0_5 Depth=1
	leaq	-256(%r13), %r10
	addq	$254, %rdx
	subq	%r13, %rdx
	js	LBB0_225
## BB#226:                              ##   in Loop: Header=BB0_5 Depth=1
	leaq	24(%rsi), %rax
	movq	%rax, -128(%rbp)        ## 8-byte Spill
	movq	%rax, 16(%rsi)
	testw	%r11w, %r11w
	je	LBB0_227
## BB#246:                              ##   in Loop: Header=BB0_5 Depth=1
	leaq	8(%r14), %r12
	movq	%r10, %r15
	subq	%r11, %r15
	jle	LBB0_265
## BB#247:                              ##   in Loop: Header=BB0_5 Depth=1
	movq	%rdx, -80(%rbp)         ## 8-byte Spill
	cmpq	$4, %r15
	movl	$0, %ebx
	jb	LBB0_258
## BB#248:                              ## %min.iters.checked961
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r15, %rax
	andq	$-4, %rax
	movl	$0, %ebx
	je	LBB0_258
## BB#249:                              ## %vector.memcheck981
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	16(%rsi,%r11,8), %rcx
	movq	%r13, %rdx
	subq	%r11, %rdx
	leaq	-2048(%r14,%rdx,8), %rdx
	cmpq	%rdx, %rcx
	ja	LBB0_251
## BB#250:                              ## %vector.memcheck981
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	-2040(%rsi,%r13,8), %rcx
	leaq	8(%r14), %rdx
	cmpq	%rcx, %rdx
	movl	$0, %ebx
	jbe	LBB0_258
LBB0_251:                               ## %vector.body957.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	-4(%r15), %rcx
	movl	%ecx, %edx
	shrl	$2, %edx
	incl	%edx
	testb	$3, %dl
	movl	$0, %ebx
	je	LBB0_254
## BB#252:                              ## %vector.body957.prol.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	32(%rsi,%r11,8), %rdx
	leal	-260(%r13), %edi
	subl	%r11d, %edi
	shrl	$2, %edi
	incl	%edi
	andl	$3, %edi
	negq	%rdi
	xorl	%ebx, %ebx
LBB0_253:                               ## %vector.body957.prol
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movdqu	8(%r14,%rbx,8), %xmm3
	movdqu	24(%r14,%rbx,8), %xmm4
	movdqu	%xmm3, -16(%rdx,%rbx,8)
	movdqu	%xmm4, (%rdx,%rbx,8)
	addq	$4, %rbx
	incq	%rdi
	jne	LBB0_253
LBB0_254:                               ## %vector.body957.preheader.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$12, %rcx
	jb	LBB0_257
## BB#255:                              ## %vector.body957.preheader.split.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r15, %rdx
	andq	$-4, %rdx
	subq	%rbx, %rdx
	leaq	(%rbx,%r11), %rcx
	leaq	128(%rsi,%rcx,8), %rdi
	leaq	120(%r14,%rbx,8), %rbx
LBB0_256:                               ## %vector.body957
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-112(%rbx), %xmm3
	movups	-96(%rbx), %xmm4
	movups	%xmm3, -112(%rdi)
	movups	%xmm4, -96(%rdi)
	movups	-80(%rbx), %xmm3
	movups	-64(%rbx), %xmm4
	movups	%xmm3, -80(%rdi)
	movups	%xmm4, -64(%rdi)
	movups	-48(%rbx), %xmm3
	movups	-32(%rbx), %xmm4
	movups	%xmm3, -48(%rdi)
	movups	%xmm4, -32(%rdi)
	movdqu	-16(%rbx), %xmm3
	movdqu	(%rbx), %xmm4
	movdqu	%xmm3, -16(%rdi)
	movdqu	%xmm4, (%rdi)
	subq	$-128, %rdi
	subq	$-128, %rbx
	addq	$-16, %rdx
	jne	LBB0_256
LBB0_257:                               ## %middle.block958
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	%rax, %r15
	movq	%rax, %rbx
	je	LBB0_264
LBB0_258:                               ## %.lr.ph.i.81.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	movl	%r13d, %ecx
	subl	%ebx, %ecx
	subl	%r11d, %ecx
	leaq	-257(%r13), %rdx
	subq	%rbx, %rdx
	subq	%r11, %rdx
	testb	$7, %cl
	je	LBB0_261
## BB#259:                              ## %.lr.ph.i.81.prol.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	16(%rsi,%r11,8), %rdi
	movl	%r13d, %ecx
	subl	%ebx, %ecx
	subl	%r11d, %ecx
	andl	$7, %ecx
	negq	%rcx
	.align	4, 0x90
LBB0_260:                               ## %.lr.ph.i.81.prol
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	8(%r14,%rbx,8), %rax
	movq	%rax, (%rdi,%rbx,8)
	incq	%rbx
	incq	%rcx
	jne	LBB0_260
LBB0_261:                               ## %.lr.ph.i.81.preheader.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$7, %rdx
	jb	LBB0_264
## BB#262:                              ## %.lr.ph.i.81.preheader.split.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	subq	%rbx, %r10
	subq	%r11, %r10
	leaq	(%rbx,%r11), %rax
	leaq	72(%rsi,%rax,8), %rdx
	leaq	64(%r14,%rbx,8), %rdi
	.align	4, 0x90
LBB0_263:                               ## %.lr.ph.i.81
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-56(%rdi), %rax
	movq	%rax, -56(%rdx)
	movq	-48(%rdi), %rax
	movq	%rax, -48(%rdx)
	movq	-40(%rdi), %rax
	movq	%rax, -40(%rdx)
	movq	-32(%rdi), %rax
	movq	%rax, -32(%rdx)
	movq	-24(%rdi), %rax
	movq	%rax, -24(%rdx)
	movq	-16(%rdi), %rax
	movq	%rax, -16(%rdx)
	movq	-8(%rdi), %rax
	movq	%rax, -8(%rdx)
	movq	(%rdi), %rax
	movq	%rax, (%rdx)
	addq	$64, %rdx
	addq	$64, %rdi
	addq	$-8, %r10
	jne	LBB0_263
LBB0_264:                               ## %copy.exit82
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	(%r12,%r15,8), %r12
	movq	__ARRAY__@GOTPCREL(%rip), %rdi
	movq	-80(%rbp), %rdx         ## 8-byte Reload
LBB0_265:                               ## %._crit_edge.512
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpl	$2, %r11d
	movq	-64(%rbp), %r10         ## 8-byte Reload
	jb	LBB0_274
## BB#266:                              ##   in Loop: Header=BB0_5 Depth=1
	movl	$1, %eax
	subq	%r11, %rax
	leaq	(%r8,%rax,8), %r14
	cmpq	%r9, %r14
	jae	LBB0_268
## BB#267:                              ##   in Loop: Header=BB0_5 Depth=1
	subq	%r14, %r9
	sarq	$3, %r9
	subq	%r9, %rdx
	movq	%r14, %r9
	js	LBB0_277
LBB0_268:                               ## %.lr.ph302.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rdx, -80(%rbp)         ## 8-byte Spill
	movq	%rsi, -72(%rbp)         ## 8-byte Spill
	leaq	-2(%r11), %rcx
	movl	$1, %ebx
	subq	%r11, %rbx
	cmpq	$-2, %rbx
	movq	$-1, %rax
	cmovleq	%rax, %rbx
	addq	%r11, %rbx
	cmpq	$4, %rbx
	jb	LBB0_275
## BB#269:                              ## %min.iters.checked940
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rbx, %rdx
	andq	$-4, %rdx
	movq	%rbx, %rax
	andq	$-4, %rax
	je	LBB0_275
## BB#270:                              ## %vector.body935.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rax, %r10
	movq	%r14, -136(%rbp)        ## 8-byte Spill
	subq	%rdx, %rcx
	movq	-72(%rbp), %rax         ## 8-byte Reload
	leaq	(%rax,%r11,8), %rsi
	leaq	-16(%r8), %r14
	leaq	-2(%r11), %r15
	movl	$1, %edi
	subq	%r11, %rdi
	cmpq	$-2, %rdi
	movq	$-1, %rax
	cmovleq	%rax, %rdi
	addq	%r11, %rdi
	andq	$-4, %rdi
	movq	-128(%rbp), %rdx        ## 8-byte Reload
	.align	4, 0x90
LBB0_271:                               ## %vector.body935
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movd	%r15, %xmm3
	pshufd	$68, %xmm3, %xmm3       ## xmm3 = xmm3[0,1,0,1]
	paddq	%xmm0, %xmm3
	leaq	8(%rsi), %rax
	movd	%rsi, %xmm4
	movd	%rax, %xmm5
	punpcklqdq	%xmm5, %xmm4    ## xmm4 = xmm4[0],xmm5[0]
	movd	%xmm3, %rax
	leaq	(%rdx,%rax,8), %rax
	movd	%rax, %xmm5
	pshufd	$78, %xmm3, %xmm3       ## xmm3 = xmm3[2,3,0,1]
	movd	%xmm3, %rax
	leaq	(%rdx,%rax,8), %rax
	movd	%rax, %xmm3
	punpcklqdq	%xmm5, %xmm3    ## xmm3 = xmm3[0],xmm5[0]
	movdqu	%xmm4, (%r14)
	movdqu	%xmm3, -16(%r14)
	addq	$-32, %rsi
	addq	$-32, %r14
	addq	$-4, %r15
	addq	$-4, %rdi
	jne	LBB0_271
## BB#272:                              ## %middle.block936
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	%r10, %rbx
	movq	-64(%rbp), %r10         ## 8-byte Reload
	movq	__ARRAY__@GOTPCREL(%rip), %rdi
	movq	-136(%rbp), %r14        ## 8-byte Reload
	je	LBB0_273
LBB0_275:                               ## %.lr.ph302.preheader1257
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	-72(%rbp), %rax         ## 8-byte Reload
	leaq	24(%rax,%rcx,8), %rdx
	incq	%rcx
	shlq	$3, %r11
	subq	%r11, %r8
	.align	4, 0x90
LBB0_276:                               ## %.lr.ph302
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	%rdx, (%r8,%rcx,8)
	decq	%rcx
	addq	$-8, %rdx
	testq	%rcx, %rcx
	jg	LBB0_276
LBB0_273:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	%r14, %r8
	movq	-80(%rbp), %rdx         ## 8-byte Reload
	movq	-72(%rbp), %rsi         ## 8-byte Reload
LBB0_274:                               ## %.thread112
                                        ##   in Loop: Header=BB0_5 Depth=1
	addq	$8, %rsi
	movq	%rsi, %r15
	movq	-128(%rbp), %rax        ## 8-byte Reload
	leaq	-2056(%rax,%r13,8), %rsi
	jmp	LBB0_352
LBB0_22:                                ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$1, %rdx
	jle	LBB0_23
## BB#24:                               ##   in Loop: Header=BB0_5 Depth=1
	addq	$-2, %rdx
LBB0_212:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	8(%r14), %rax
	movq	%rax, 8(%rsi)
	addq	$16, %r14
	addq	$16, %rsi
	jmp	LBB0_349
LBB0_48:                                ##   in Loop: Header=BB0_5 Depth=1
	movq	%rdx, %rbx
	leaq	2(%rdi), %rdx
	cmpq	%rdx, %rcx
	movq	%r13, %r10
	jne	LBB0_181
## BB#49:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	%rbx, %rdx
	cmpq	$2, %rdx
	jle	LBB0_50
## BB#51:                               ##   in Loop: Header=BB0_5 Depth=1
	addq	$-3, %rdx
	movq	8(%r14), %r13
	movq	16(%r14), %rcx
	leaq	24(%r14), %r12
	leaq	(%rcx,%rdi), %rax
	testq	%rcx, %rcx
	cmoveq	%rcx, %rax
	movq	%r13, 8(%rsi)
	movq	%rax, 16(%rsi)
	leaq	24(%rsi), %r15
	testq	%rax, %rax
	je	LBB0_52
## BB#65:                               ##   in Loop: Header=BB0_5 Depth=1
	cmpq	-88(%rbp), %rax         ## 8-byte Folded Reload
	je	LBB0_67
## BB#66:                               ##   in Loop: Header=BB0_5 Depth=1
	cmpq	-112(%rbp), %rax        ## 8-byte Folded Reload
	je	LBB0_67
## BB#90:                               ##   in Loop: Header=BB0_5 Depth=1
	cmpq	-104(%rbp), %rax        ## 8-byte Folded Reload
	jne	LBB0_111
## BB#91:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	%r15, %r11
	movq	%rsi, %rbx
	addq	$7, %r13
	sarq	$3, %r13
	subq	%r13, %rdx
	movq	%rdx, %r15
	js	LBB0_68
## BB#92:                               ##   in Loop: Header=BB0_5 Depth=1
	testq	%r13, %r13
	jle	LBB0_110
## BB#93:                               ## %.lr.ph.i.97.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$4, %r13
	movl	$0, %ecx
	jb	LBB0_104
## BB#94:                               ## %min.iters.checked1095
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r13, %rax
	andq	$-4, %rax
	movl	$0, %ecx
	je	LBB0_104
## BB#95:                               ## %vector.memcheck1114
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	24(%rbx), %rcx
	leaq	16(%r14,%r13,8), %rdx
	cmpq	%rdx, %rcx
	ja	LBB0_97
## BB#96:                               ## %vector.memcheck1114
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	16(%rbx,%r13,8), %rcx
	leaq	24(%r14), %rdx
	cmpq	%rcx, %rdx
	movl	$0, %ecx
	jbe	LBB0_104
LBB0_97:                                ## %vector.body1091.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	-4(%r13), %rcx
	movl	%ecx, %edx
	shrl	$2, %edx
	incl	%edx
	testb	$3, %dl
	movl	$0, %esi
	je	LBB0_100
## BB#98:                               ## %vector.body1091.prol.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	leal	-4(%r13), %edx
	shrl	$2, %edx
	incl	%edx
	andl	$3, %edx
	negq	%rdx
	xorl	%esi, %esi
LBB0_99:                                ## %vector.body1091.prol
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movdqu	24(%r14,%rsi,8), %xmm3
	movdqu	40(%r14,%rsi,8), %xmm4
	movdqu	%xmm3, 24(%rbx,%rsi,8)
	movdqu	%xmm4, 40(%rbx,%rsi,8)
	addq	$4, %rsi
	incq	%rdx
	jne	LBB0_99
LBB0_100:                               ## %vector.body1091.preheader.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$12, %rcx
	jb	LBB0_103
## BB#101:                              ## %vector.body1091.preheader.split.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r13, %rcx
	andq	$-4, %rcx
	subq	%rsi, %rcx
	leaq	136(%rbx,%rsi,8), %rdx
	leaq	136(%r14,%rsi,8), %rsi
LBB0_102:                               ## %vector.body1091
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-112(%rsi), %xmm3
	movups	-96(%rsi), %xmm4
	movups	%xmm3, -112(%rdx)
	movups	%xmm4, -96(%rdx)
	movups	-80(%rsi), %xmm3
	movups	-64(%rsi), %xmm4
	movups	%xmm3, -80(%rdx)
	movups	%xmm4, -64(%rdx)
	movups	-48(%rsi), %xmm3
	movups	-32(%rsi), %xmm4
	movups	%xmm3, -48(%rdx)
	movups	%xmm4, -32(%rdx)
	movdqu	-16(%rsi), %xmm3
	movdqu	(%rsi), %xmm4
	movdqu	%xmm3, -16(%rdx)
	movdqu	%xmm4, (%rdx)
	subq	$-128, %rdx
	subq	$-128, %rsi
	addq	$-16, %rcx
	jne	LBB0_102
LBB0_103:                               ## %middle.block1092
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	%rax, %r13
	movq	%rax, %rcx
	je	LBB0_110
LBB0_104:                               ## %.lr.ph.i.97.preheader1262
                                        ##   in Loop: Header=BB0_5 Depth=1
	movl	%r13d, %edx
	subl	%ecx, %edx
	leaq	-1(%r13), %rax
	subq	%rcx, %rax
	testb	$7, %dl
	je	LBB0_107
## BB#105:                              ## %.lr.ph.i.97.prol.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	movl	%r13d, %edx
	subl	%ecx, %edx
	andl	$7, %edx
	negq	%rdx
LBB0_106:                               ## %.lr.ph.i.97.prol
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	24(%r14,%rcx,8), %rsi
	movq	%rsi, 24(%rbx,%rcx,8)
	incq	%rcx
	incq	%rdx
	jne	LBB0_106
LBB0_107:                               ## %.lr.ph.i.97.preheader1262.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$7, %rax
	jb	LBB0_110
## BB#108:                              ## %.lr.ph.i.97.preheader1262.split.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r13, %rax
	subq	%rcx, %rax
	leaq	80(%rbx,%rcx,8), %rdx
	leaq	80(%r14,%rcx,8), %rcx
LBB0_109:                               ## %.lr.ph.i.97
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-56(%rcx), %rsi
	movq	%rsi, -56(%rdx)
	movq	-48(%rcx), %rsi
	movq	%rsi, -48(%rdx)
	movq	-40(%rcx), %rsi
	movq	%rsi, -40(%rdx)
	movq	-32(%rcx), %rsi
	movq	%rsi, -32(%rdx)
	movq	-24(%rcx), %rsi
	movq	%rsi, -24(%rdx)
	movq	-16(%rcx), %rsi
	movq	%rsi, -16(%rdx)
	movq	-8(%rcx), %rsi
	movq	%rsi, -8(%rdx)
	movq	(%rcx), %rsi
	movq	%rsi, (%rdx)
	addq	$64, %rdx
	addq	$64, %rcx
	addq	$-8, %rax
	jne	LBB0_109
LBB0_110:                               ## %copy.exit98
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	(%r12,%r13,8), %r12
	leaq	(%r11,%r13,8), %rsi
	movq	%r15, %rdx
	jmp	LBB0_350
LBB0_227:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	%rdx, %r11
	movq	%rdi, %r15
	movq	8(%r14), %rax
	movq	%rax, 8(%rsi)
	movq	%rsi, %rbx
	leaq	16(%r14), %rax
	leaq	-257(%r13), %rcx
	cmpl	$258, %r13d             ## imm = 0x102
	jb	LBB0_245
## BB#228:                              ## %.lr.ph.i.85.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$4, %rcx
	movl	$0, %esi
	jb	LBB0_239
## BB#229:                              ## %min.iters.checked902
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rcx, %r10
	andq	$-4, %r10
	movl	$0, %esi
	je	LBB0_239
## BB#230:                              ## %vector.memcheck921
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	-2048(%r14,%r13,8), %rdx
	cmpq	%rdx, -128(%rbp)        ## 8-byte Folded Reload
	ja	LBB0_232
## BB#231:                              ## %vector.memcheck921
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	-2040(%rbx,%r13,8), %rdx
	cmpq	%rdx, %rax
	movl	$0, %esi
	jbe	LBB0_239
LBB0_232:                               ## %vector.body898.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	-261(%r13), %rsi
	movl	%esi, %edx
	shrl	$2, %edx
	incl	%edx
	testb	$3, %dl
	movl	$0, %edi
	je	LBB0_235
## BB#233:                              ## %vector.body898.prol.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	leal	-261(%r13), %edx
	shrl	$2, %edx
	incl	%edx
	andl	$3, %edx
	negq	%rdx
	xorl	%edi, %edi
LBB0_234:                               ## %vector.body898.prol
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movdqu	16(%r14,%rdi,8), %xmm3
	movdqu	32(%r14,%rdi,8), %xmm4
	movdqu	%xmm3, 24(%rbx,%rdi,8)
	movdqu	%xmm4, 40(%rbx,%rdi,8)
	addq	$4, %rdi
	incq	%rdx
	jne	LBB0_234
LBB0_235:                               ## %vector.body898.preheader.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$12, %rsi
	jb	LBB0_238
## BB#236:                              ## %vector.body898.preheader.split.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rcx, %rdx
	andq	$-4, %rdx
	subq	%rdi, %rdx
	leaq	136(%rbx,%rdi,8), %rsi
	leaq	128(%r14,%rdi,8), %rdi
LBB0_237:                               ## %vector.body898
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-112(%rdi), %xmm3
	movups	-96(%rdi), %xmm4
	movups	%xmm3, -112(%rsi)
	movups	%xmm4, -96(%rsi)
	movups	-80(%rdi), %xmm3
	movups	-64(%rdi), %xmm4
	movups	%xmm3, -80(%rsi)
	movups	%xmm4, -64(%rsi)
	movups	-48(%rdi), %xmm3
	movups	-32(%rdi), %xmm4
	movups	%xmm3, -48(%rsi)
	movups	%xmm4, -32(%rsi)
	movdqu	-16(%rdi), %xmm3
	movdqu	(%rdi), %xmm4
	movdqu	%xmm3, -16(%rsi)
	movdqu	%xmm4, (%rsi)
	subq	$-128, %rsi
	subq	$-128, %rdi
	addq	$-16, %rdx
	jne	LBB0_237
LBB0_238:                               ## %middle.block899
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	%r10, %rcx
	movq	%r10, %rsi
	je	LBB0_245
LBB0_239:                               ## %.lr.ph.i.85.preheader1256
                                        ##   in Loop: Header=BB0_5 Depth=1
	leal	7(%r13), %edi
	subl	%esi, %edi
	leaq	-258(%r13), %rdx
	subq	%rsi, %rdx
	testb	$7, %dil
	je	LBB0_242
## BB#240:                              ## %.lr.ph.i.85.prol.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	addl	$7, %r13d
	subl	%esi, %r13d
	andl	$7, %r13d
	negq	%r13
	.align	4, 0x90
LBB0_241:                               ## %.lr.ph.i.85.prol
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	16(%r14,%rsi,8), %rdi
	movq	%rdi, 24(%rbx,%rsi,8)
	incq	%rsi
	incq	%r13
	jne	LBB0_241
LBB0_242:                               ## %.lr.ph.i.85.preheader1256.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$7, %rdx
	jb	LBB0_245
## BB#243:                              ## %.lr.ph.i.85.preheader1256.split.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rcx, %rdx
	subq	%rsi, %rdx
	leaq	80(%rbx,%rsi,8), %rdi
	leaq	72(%r14,%rsi,8), %rsi
	.align	4, 0x90
LBB0_244:                               ## %.lr.ph.i.85
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-56(%rsi), %rbx
	movq	%rbx, -56(%rdi)
	movq	-48(%rsi), %rbx
	movq	%rbx, -48(%rdi)
	movq	-40(%rsi), %rbx
	movq	%rbx, -40(%rdi)
	movq	-32(%rsi), %rbx
	movq	%rbx, -32(%rdi)
	movq	-24(%rsi), %rbx
	movq	%rbx, -24(%rdi)
	movq	-16(%rsi), %rbx
	movq	%rbx, -16(%rdi)
	movq	-8(%rsi), %rbx
	movq	%rbx, -8(%rdi)
	movq	(%rsi), %rbx
	movq	%rbx, (%rdi)
	addq	$64, %rdi
	addq	$64, %rsi
	addq	$-8, %rdx
	jne	LBB0_244
LBB0_245:                               ## %copy.exit86
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	(%rax,%rcx,8), %r12
	movq	-128(%rbp), %rax        ## 8-byte Reload
	leaq	(%rax,%rcx,8), %rsi
	movq	-64(%rbp), %r10         ## 8-byte Reload
	movq	%r15, %rdi
	movq	%r11, %rdx
	jmp	LBB0_350
LBB0_327:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	%rdx, %r8
	jmp	LBB0_338
LBB0_217:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	8(%r14), %rax
	movq	%rax, 16(%rsi)
	addq	$16, %r14
	jmp	LBB0_223
LBB0_218:                               ##   in Loop: Header=BB0_5 Depth=1
	cmpq	%r9, %r8
	ja	LBB0_222
## BB#219:                              ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$3, %rdx
	jle	LBB0_220
## BB#221:                              ##   in Loop: Header=BB0_5 Depth=1
	addq	$-4, %rdx
	addq	$-8, %r9
	movq	%rdx, %rcx
LBB0_222:                               ##   in Loop: Header=BB0_5 Depth=1
	addq	$8, %r14
	leaq	16(%rsi), %rax
	movq	%rax, -8(%r8)
	addq	$-8, %r8
LBB0_223:                               ##   in Loop: Header=BB0_5 Depth=1
	leaq	8(%rsi), %r15
	addq	$24, %rsi
	movq	%rcx, %rdx
	jmp	LBB0_347
LBB0_181:                               ##   in Loop: Header=BB0_5 Depth=1
	leaq	-10(%rdi,%rax), %rax
	movq	%rax, (%r15)
	movq	%rax, (%r14)
	addq	$8, %r14
	movq	%r14, %r12
	movq	%rbx, %rdx
	jmp	LBB0_350
LBB0_330:                               ## %min.iters.checked830
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rcx, %rax
	andq	$-4, %rax
	movq	%rcx, %rdx
	andq	$-4, %rdx
	je	LBB0_331
## BB#332:                              ## %vector.body826.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rdx, -160(%rbp)        ## 8-byte Spill
	movq	%rcx, -152(%rbp)        ## 8-byte Spill
	subq	%rax, %rdi
	movzbl	%r13b, %eax
	movq	%rax, %rcx
	movq	-128(%rbp), %r10        ## 8-byte Reload
	subq	%r10, %rcx
	movq	-72(%rbp), %rdx         ## 8-byte Reload
	leaq	(%rdx,%rcx,8), %r14
	leaq	-3(%rax), %rsi
	subq	%r10, %rsi
	movq	-144(%rbp), %rdx        ## 8-byte Reload
	subq	%rax, %rdx
	cmpq	$-2, %rdx
	movq	$-1, %rcx
	cmovleq	%rcx, %rdx
	leaq	-1(%rdx,%rax), %rdx
	subq	%r10, %rdx
	andq	$-4, %rdx
	xorl	%eax, %eax
	.align	4, 0x90
LBB0_333:                               ## %vector.body826
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movd	%rsi, %xmm3
	pshufd	$68, %xmm3, %xmm3       ## xmm3 = xmm3[0,1,0,1]
	paddq	%xmm0, %xmm3
	leaq	-8(%r14,%rax), %rcx
	movd	%rcx, %xmm4
	leaq	-16(%r14,%rax), %rcx
	movd	%rcx, %xmm5
	punpcklqdq	%xmm4, %xmm5    ## xmm5 = xmm5[0],xmm4[0]
	movd	%xmm3, %rcx
	leaq	(%r11,%rcx,8), %rcx
	movd	%rcx, %xmm4
	pshufd	$78, %xmm3, %xmm3       ## xmm3 = xmm3[2,3,0,1]
	movd	%xmm3, %rcx
	leaq	(%r11,%rcx,8), %rcx
	movd	%rcx, %xmm3
	punpcklqdq	%xmm4, %xmm3    ## xmm3 = xmm3[0],xmm4[0]
	movdqu	%xmm5, -24(%r8,%rax)
	movdqu	%xmm3, -40(%r8,%rax)
	addq	$-32, %rax
	addq	$-4, %rsi
	addq	$-4, %rdx
	jne	LBB0_333
## BB#334:                              ## %middle.block827
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	-160(%rbp), %rax        ## 8-byte Reload
	cmpq	%rax, -152(%rbp)        ## 8-byte Folded Reload
	movq	-64(%rbp), %r10         ## 8-byte Reload
	movq	-80(%rbp), %rsi         ## 8-byte Reload
	movq	-136(%rbp), %r14        ## 8-byte Reload
	je	LBB0_337
	jmp	LBB0_335
LBB0_41:                                ##   in Loop: Header=BB0_5 Depth=1
	movq	%r10, %rcx
LBB0_44:                                ## %.lr.ph.i.preheader1259.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$7, %rdx
	jb	LBB0_47
## BB#45:                               ## %.lr.ph.i.preheader1259.split.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rax, %rdx
	subq	%rdi, %rdx
	leaq	72(%rcx,%rdi,8), %rsi
	leaq	72(%r14,%rdi,8), %rdi
	.align	4, 0x90
LBB0_46:                                ## %.lr.ph.i
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-56(%rdi), %rbx
	movq	%rbx, -56(%rsi)
	movq	-48(%rdi), %rbx
	movq	%rbx, -48(%rsi)
	movq	-40(%rdi), %rbx
	movq	%rbx, -40(%rsi)
	movq	-32(%rdi), %rbx
	movq	%rbx, -32(%rsi)
	movq	-24(%rdi), %rbx
	movq	%rbx, -24(%rsi)
	movq	-16(%rdi), %rbx
	movq	%rbx, -16(%rsi)
	movq	-8(%rdi), %rbx
	movq	%rbx, -8(%rsi)
	movq	(%rdi), %rbx
	movq	%rbx, (%rsi)
	addq	$64, %rsi
	addq	$64, %rdi
	addq	$-8, %rdx
	jne	LBB0_46
LBB0_47:                                ## %copy.exit
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	(%r12,%rax,8), %r12
	leaq	(%r11,%rax,8), %rsi
	movq	%r13, %r10
LBB0_179:                               ## %.loopexit135
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	__ARRAY__@GOTPCREL(%rip), %rdi
	movq	-80(%rbp), %rdx         ## 8-byte Reload
	jmp	LBB0_350
LBB0_52:                                ##   in Loop: Header=BB0_5 Depth=1
	movq	%r10, -64(%rbp)         ## 8-byte Spill
	leaq	(,%r13,8), %rcx
	movq	%r13, %r11
	movq	%r8, %rax
	subq	%rcx, %rax
	movq	%rsi, %r14
	cmpq	%r9, %rax
	jae	LBB0_53
## BB#63:                               ##   in Loop: Header=BB0_5 Depth=1
	subq	%rax, %r9
	sarq	$3, %r9
	subq	%r9, %rdx
	movq	%rdx, %r13
	movq	%rax, %r9
	jns	LBB0_54
	jmp	LBB0_64
LBB0_331:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	-136(%rbp), %r14        ## 8-byte Reload
LBB0_335:                               ## %.lr.ph305.preheader1251
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	-72(%rbp), %rax         ## 8-byte Reload
	leaq	16(%rax,%rdi,8), %rax
	incq	%rdi
	movzbl	%r13b, %ecx
	movq	-128(%rbp), %rdx        ## 8-byte Reload
	subq	%rcx, %rdx
	leaq	(%r8,%rdx,8), %rcx
	.align	4, 0x90
LBB0_336:                               ## %.lr.ph305
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	%rax, (%rcx,%rdi,8)
	decq	%rdi
	addq	$-8, %rax
	testq	%rdi, %rdi
	jg	LBB0_336
LBB0_337:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	%r14, %r8
LBB0_338:                               ## %.loopexit
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	__ARRAY__@GOTPCREL(%rip), %rdi
	leaq	-8(%r11,%rsi,8), %rsi
	movq	%rbx, %rdx
	jmp	LBB0_352
LBB0_67:                                ##   in Loop: Header=BB0_5 Depth=1
	movq	%r15, %r11
	subq	%r13, %rdx
	js	LBB0_68
## BB#71:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	%rsi, %rbx
	movq	%rdx, -80(%rbp)         ## 8-byte Spill
	movq	%rdi, %r15
	testq	%r13, %r13
	jle	LBB0_89
## BB#72:                               ## %.lr.ph.i.101.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$4, %r13
	movl	$0, %ecx
	jb	LBB0_83
## BB#73:                               ## %min.iters.checked1058
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r13, %rax
	andq	$-4, %rax
	movl	$0, %ecx
	je	LBB0_83
## BB#74:                               ## %vector.memcheck1077
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	24(%rbx), %rcx
	leaq	16(%r14,%r13,8), %rdx
	cmpq	%rdx, %rcx
	ja	LBB0_76
## BB#75:                               ## %vector.memcheck1077
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	16(%rbx,%r13,8), %rcx
	leaq	24(%r14), %rdx
	cmpq	%rcx, %rdx
	movl	$0, %ecx
	jbe	LBB0_83
LBB0_76:                                ## %vector.body1054.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	-4(%r13), %rcx
	movl	%ecx, %edx
	shrl	$2, %edx
	incl	%edx
	testb	$3, %dl
	movl	$0, %esi
	je	LBB0_79
## BB#77:                               ## %vector.body1054.prol.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	leal	-4(%r13), %edx
	shrl	$2, %edx
	incl	%edx
	andl	$3, %edx
	negq	%rdx
	xorl	%esi, %esi
LBB0_78:                                ## %vector.body1054.prol
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movdqu	24(%r14,%rsi,8), %xmm3
	movdqu	40(%r14,%rsi,8), %xmm4
	movdqu	%xmm3, 24(%rbx,%rsi,8)
	movdqu	%xmm4, 40(%rbx,%rsi,8)
	addq	$4, %rsi
	incq	%rdx
	jne	LBB0_78
LBB0_79:                                ## %vector.body1054.preheader.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$12, %rcx
	jb	LBB0_82
## BB#80:                               ## %vector.body1054.preheader.split.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r13, %rcx
	andq	$-4, %rcx
	subq	%rsi, %rcx
	leaq	136(%rbx,%rsi,8), %rdx
	leaq	136(%r14,%rsi,8), %rsi
LBB0_81:                                ## %vector.body1054
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-112(%rsi), %xmm3
	movups	-96(%rsi), %xmm4
	movups	%xmm3, -112(%rdx)
	movups	%xmm4, -96(%rdx)
	movups	-80(%rsi), %xmm3
	movups	-64(%rsi), %xmm4
	movups	%xmm3, -80(%rdx)
	movups	%xmm4, -64(%rdx)
	movups	-48(%rsi), %xmm3
	movups	-32(%rsi), %xmm4
	movups	%xmm3, -48(%rdx)
	movups	%xmm4, -32(%rdx)
	movdqu	-16(%rsi), %xmm3
	movdqu	(%rsi), %xmm4
	movdqu	%xmm3, -16(%rdx)
	movdqu	%xmm4, (%rdx)
	subq	$-128, %rdx
	subq	$-128, %rsi
	addq	$-16, %rcx
	jne	LBB0_81
LBB0_82:                                ## %middle.block1055
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	%rax, %r13
	movq	%rax, %rcx
	je	LBB0_89
LBB0_83:                                ## %.lr.ph.i.101.preheader1261
                                        ##   in Loop: Header=BB0_5 Depth=1
	movl	%r13d, %edx
	subl	%ecx, %edx
	leaq	-1(%r13), %rax
	subq	%rcx, %rax
	testb	$7, %dl
	je	LBB0_86
## BB#84:                               ## %.lr.ph.i.101.prol.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	movl	%r13d, %edx
	subl	%ecx, %edx
	andl	$7, %edx
	negq	%rdx
LBB0_85:                                ## %.lr.ph.i.101.prol
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	24(%r14,%rcx,8), %rsi
	movq	%rsi, 24(%rbx,%rcx,8)
	incq	%rcx
	incq	%rdx
	jne	LBB0_85
LBB0_86:                                ## %.lr.ph.i.101.preheader1261.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$7, %rax
	jb	LBB0_89
## BB#87:                               ## %.lr.ph.i.101.preheader1261.split.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r13, %rax
	subq	%rcx, %rax
	leaq	80(%rbx,%rcx,8), %rdx
	leaq	80(%r14,%rcx,8), %rcx
LBB0_88:                                ## %.lr.ph.i.101
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-56(%rcx), %rsi
	movq	%rsi, -56(%rdx)
	movq	-48(%rcx), %rsi
	movq	%rsi, -48(%rdx)
	movq	-40(%rcx), %rsi
	movq	%rsi, -40(%rdx)
	movq	-32(%rcx), %rsi
	movq	%rsi, -32(%rdx)
	movq	-24(%rcx), %rsi
	movq	%rsi, -24(%rdx)
	movq	-16(%rcx), %rsi
	movq	%rsi, -16(%rdx)
	movq	-8(%rcx), %rsi
	movq	%rsi, -8(%rdx)
	movq	(%rcx), %rsi
	movq	%rsi, (%rdx)
	addq	$64, %rdx
	addq	$64, %rcx
	addq	$-8, %rax
	jne	LBB0_88
LBB0_89:                                ## %copy.exit102
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	(%r12,%r13,8), %r12
	leaq	(%r11,%r13,8), %rsi
	movq	%r15, %rdi
	movq	-80(%rbp), %rdx         ## 8-byte Reload
	jmp	LBB0_350
LBB0_53:                                ##   in Loop: Header=BB0_5 Depth=1
	movq	%rdx, %r13
LBB0_54:                                ## %.thread
                                        ##   in Loop: Header=BB0_5 Depth=1
	testq	%r11, %r11
	jle	LBB0_62
## BB#55:                               ## %.lr.ph300.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$2, %r11
	movl	$1, %edx
	cmovlq	%r11, %rdx
	notq	%rdx
	leaq	2(%r11,%rdx), %r10
	cmpq	$4, %r10
	movq	%r11, %rsi
	jb	LBB0_60
## BB#56:                               ## %min.iters.checked1037
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r10, %rdx
	andq	$-4, %rdx
	movq	%r10, %rbx
	andq	$-4, %rbx
	movq	%r11, %rsi
	je	LBB0_60
## BB#57:                               ## %vector.body1032.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rbx, -128(%rbp)        ## 8-byte Spill
	movq	%rcx, -80(%rbp)         ## 8-byte Spill
	movq	%r11, %rsi
	subq	%rdx, %rsi
	movq	%r11, %rdx
	notq	%rdx
	cmpq	$-3, %rdx
	movq	$-2, %rcx
	cmovleq	%rcx, %rdx
	leaq	2(%r11,%rdx), %rdx
	andq	$-4, %rdx
	movq	%r11, %rbx
LBB0_58:                                ## %vector.body1032
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movd	%rbx, %xmm3
	pshufd	$68, %xmm3, %xmm3       ## xmm3 = xmm3[0,1,0,1]
	movdqa	%xmm3, %xmm4
	paddq	%xmm1, %xmm4
	paddq	%xmm2, %xmm3
	movd	%xmm4, %rcx
	leaq	(%r15,%rcx,8), %rdi
	movd	%rdi, %xmm5
	pshufd	$78, %xmm4, %xmm4       ## xmm4 = xmm4[2,3,0,1]
	movd	%xmm4, %rdi
	leaq	(%r15,%rdi,8), %rdi
	movd	%rdi, %xmm4
	punpcklqdq	%xmm5, %xmm4    ## xmm4 = xmm4[0],xmm5[0]
	movd	%xmm3, %rdi
	leaq	(%r15,%rdi,8), %rdi
	movd	%rdi, %xmm5
	pshufd	$78, %xmm3, %xmm3       ## xmm3 = xmm3[2,3,0,1]
	movd	%xmm3, %rdi
	leaq	(%r15,%rdi,8), %rdi
	movd	%rdi, %xmm3
	punpcklqdq	%xmm5, %xmm3    ## xmm3 = xmm3[0],xmm5[0]
	movdqu	%xmm4, -8(%rax,%rcx,8)
	movdqu	%xmm3, -24(%rax,%rcx,8)
	addq	$-4, %rbx
	addq	$-4, %rdx
	jne	LBB0_58
## BB#59:                               ## %middle.block1033
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	-128(%rbp), %r10        ## 8-byte Folded Reload
	movq	__ARRAY__@GOTPCREL(%rip), %rdi
	movq	-80(%rbp), %rcx         ## 8-byte Reload
	je	LBB0_62
LBB0_60:                                ## %.lr.ph300.preheader1260
                                        ##   in Loop: Header=BB0_5 Depth=1
	subq	%rcx, %r8
	leaq	16(%r14,%rsi,8), %rcx
	incq	%rsi
LBB0_61:                                ## %.lr.ph300
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	%rcx, -16(%r8,%rsi,8)
	decq	%rsi
	addq	$-8, %rcx
	cmpq	$1, %rsi
	jg	LBB0_61
LBB0_62:                                ## %._crit_edge.301
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	(%r15,%r11,8), %rsi
	movq	%rax, %r8
	movq	-64(%rbp), %r10         ## 8-byte Reload
	movq	%r13, %rdx
	jmp	LBB0_350
LBB0_111:                               ##   in Loop: Header=BB0_5 Depth=1
	movzwl	(%rax), %ecx
	movzwl	-2(%rax), %eax
	leaq	-256(%rax), %r11
	testq	%rcx, %rcx
	je	LBB0_112
## BB#134:                              ##   in Loop: Header=BB0_5 Depth=1
	movq	%rax, -144(%rbp)        ## 8-byte Spill
	movq	%r11, %rax
	subq	%rcx, %rax
	movq	%rax, -128(%rbp)        ## 8-byte Spill
	jne	LBB0_146
## BB#135:                              ##   in Loop: Header=BB0_5 Depth=1
	movq	%rsi, -72(%rbp)         ## 8-byte Spill
	movq	%r13, %rax
	movq	%rax, -176(%rbp)        ## 8-byte Spill
	movq	%rdx, %r13
	movq	%r10, -64(%rbp)         ## 8-byte Spill
	movq	%rcx, %r11
	movq	%rcx, -136(%rbp)        ## 8-byte Spill
	imulq	%rax, %r11
	leaq	(,%r11,8), %rdx
	movq	%r8, %rax
	subq	%rdx, %rax
	cmpq	%r9, %rax
	jae	LBB0_137
## BB#136:                              ##   in Loop: Header=BB0_5 Depth=1
	subq	%rax, %r9
	sarq	$3, %r9
	subq	%r9, %r13
	movq	%rax, %r9
	js	LBB0_64
LBB0_137:                               ## %.thread104
                                        ##   in Loop: Header=BB0_5 Depth=1
	testq	%r11, %r11
	jle	LBB0_145
## BB#138:                              ## %.lr.ph298.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	-136(%rbp), %rdi        ## 8-byte Reload
	imulq	-176(%rbp), %rdi        ## 8-byte Folded Reload
	cmpq	$2, %rdi
	movl	$1, %edx
	cmovlq	%rdi, %rdx
	movl	$1, %r10d
	subq	%rdx, %r10
	addq	%rdi, %r10
	cmpq	$4, %r10
	movq	%r11, %rsi
	jb	LBB0_143
## BB#139:                              ## %min.iters.checked1170
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r10, %rdx
	andq	$-4, %rdx
	movq	%r10, %rcx
	andq	$-4, %rcx
	movq	%rcx, -80(%rbp)         ## 8-byte Spill
	movq	%r11, %rsi
	je	LBB0_143
## BB#140:                              ## %vector.body1165.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r11, %rsi
	subq	%rdx, %rsi
	movq	%rdi, %rdx
	notq	%rdx
	cmpq	$-3, %rdx
	movq	$-2, %rcx
	cmovleq	%rcx, %rdx
	leaq	2(%rdx,%rdi), %rdx
	andq	$-4, %rdx
LBB0_141:                               ## %vector.body1165
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movd	%rdi, %xmm3
	pshufd	$68, %xmm3, %xmm3       ## xmm3 = xmm3[0,1,0,1]
	movdqa	%xmm3, %xmm4
	paddq	%xmm1, %xmm4
	paddq	%xmm2, %xmm3
	movd	%xmm4, %rcx
	leaq	(%r15,%rcx,8), %rbx
	movd	%rbx, %xmm5
	pshufd	$78, %xmm4, %xmm4       ## xmm4 = xmm4[2,3,0,1]
	movd	%xmm4, %rbx
	leaq	(%r15,%rbx,8), %rbx
	movd	%rbx, %xmm4
	punpcklqdq	%xmm5, %xmm4    ## xmm4 = xmm4[0],xmm5[0]
	movd	%xmm3, %rbx
	leaq	(%r15,%rbx,8), %rbx
	movd	%rbx, %xmm5
	pshufd	$78, %xmm3, %xmm3       ## xmm3 = xmm3[2,3,0,1]
	movd	%xmm3, %rbx
	leaq	(%r15,%rbx,8), %rbx
	movd	%rbx, %xmm3
	punpcklqdq	%xmm5, %xmm3    ## xmm3 = xmm3[0],xmm5[0]
	movdqu	%xmm4, -8(%rax,%rcx,8)
	movdqu	%xmm3, -24(%rax,%rcx,8)
	addq	$-4, %rdi
	addq	$-4, %rdx
	jne	LBB0_141
## BB#142:                              ## %middle.block1166
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	-80(%rbp), %r10         ## 8-byte Folded Reload
	je	LBB0_145
LBB0_143:                               ## %.lr.ph298.preheader1264
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	-136(%rbp), %rcx        ## 8-byte Reload
	imulq	-176(%rbp), %rcx        ## 8-byte Folded Reload
	shlq	$3, %rcx
	subq	%rcx, %r8
	movq	-72(%rbp), %rcx         ## 8-byte Reload
	leaq	16(%rcx,%rsi,8), %rdx
	incq	%rsi
LBB0_144:                               ## %.lr.ph298
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	%rdx, -16(%r8,%rsi,8)
	decq	%rsi
	addq	$-8, %rdx
	cmpq	$1, %rsi
	jg	LBB0_144
LBB0_145:                               ## %._crit_edge.299
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	(%r15,%r11,8), %rsi
	movq	%rax, %r8
	movq	-64(%rbp), %r10         ## 8-byte Reload
	movq	__ARRAY__@GOTPCREL(%rip), %rdi
	movq	%r13, %rdx
	jmp	LBB0_350
LBB0_112:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	%rsi, %rcx
	imulq	%r13, %r11
	subq	%r11, %rdx
	movq	%rdx, -80(%rbp)         ## 8-byte Spill
	js	LBB0_113
## BB#114:                              ##   in Loop: Header=BB0_5 Depth=1
	testq	%r11, %r11
	jle	LBB0_133
## BB#115:                              ## %.lr.ph.i.93.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$4, %r11
	movl	$0, %edi
	jb	LBB0_126
## BB#116:                              ## %min.iters.checked1132
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r11, %rsi
	andq	$-4, %rsi
	movl	$0, %edi
	je	LBB0_126
## BB#117:                              ## %vector.memcheck1151
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	24(%rcx), %rdx
	leaq	16(%r14,%r11,8), %rdi
	cmpq	%rdi, %rdx
	ja	LBB0_119
## BB#118:                              ## %vector.memcheck1151
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	16(%rcx,%r11,8), %rdx
	leaq	24(%r14), %rdi
	cmpq	%rdx, %rdi
	movl	$0, %edi
	jbe	LBB0_126
LBB0_119:                               ## %vector.body1128.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r15, -128(%rbp)        ## 8-byte Spill
	leaq	-4(%r11), %rdi
	movl	%edi, %edx
	shrl	$2, %edx
	incl	%edx
	testb	$3, %dl
	movl	$0, %ebx
	je	LBB0_122
## BB#120:                              ## %vector.body1128.prol.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	leal	-256(%rax), %edx
	imull	%r13d, %edx
	addl	$-4, %edx
	shrl	$2, %edx
	incl	%edx
	andl	$3, %edx
	negq	%rdx
	xorl	%ebx, %ebx
LBB0_121:                               ## %vector.body1128.prol
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movdqu	24(%r14,%rbx,8), %xmm3
	movdqu	40(%r14,%rbx,8), %xmm4
	movdqu	%xmm3, 24(%rcx,%rbx,8)
	movdqu	%xmm4, 40(%rcx,%rbx,8)
	addq	$4, %rbx
	incq	%rdx
	jne	LBB0_121
LBB0_122:                               ## %vector.body1128.preheader.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$12, %rdi
	jb	LBB0_125
## BB#123:                              ## %vector.body1128.preheader.split.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r11, %rdx
	andq	$-4, %rdx
	subq	%rbx, %rdx
	leaq	136(%rcx,%rbx,8), %rdi
	leaq	136(%r14,%rbx,8), %rbx
LBB0_124:                               ## %vector.body1128
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-112(%rbx), %xmm3
	movups	-96(%rbx), %xmm4
	movups	%xmm3, -112(%rdi)
	movups	%xmm4, -96(%rdi)
	movups	-80(%rbx), %xmm3
	movups	-64(%rbx), %xmm4
	movups	%xmm3, -80(%rdi)
	movups	%xmm4, -64(%rdi)
	movups	-48(%rbx), %xmm3
	movups	-32(%rbx), %xmm4
	movups	%xmm3, -48(%rdi)
	movups	%xmm4, -32(%rdi)
	movdqu	-16(%rbx), %xmm3
	movdqu	(%rbx), %xmm4
	movdqu	%xmm3, -16(%rdi)
	movdqu	%xmm4, (%rdi)
	subq	$-128, %rdi
	subq	$-128, %rbx
	addq	$-16, %rdx
	jne	LBB0_124
LBB0_125:                               ## %middle.block1129
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	%rsi, %r11
	movq	%rsi, %rdi
	movq	-128(%rbp), %r15        ## 8-byte Reload
	je	LBB0_133
LBB0_126:                               ## %.lr.ph.i.93.preheader1263
                                        ##   in Loop: Header=BB0_5 Depth=1
	movl	%r11d, %esi
	subl	%edi, %esi
	leaq	-1(%r11), %rdx
	subq	%rdi, %rdx
	testb	$7, %sil
	je	LBB0_127
## BB#128:                              ## %.lr.ph.i.93.prol.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	addl	$-256, %eax
	imull	%eax, %r13d
	subl	%edi, %r13d
	andl	$7, %r13d
	negq	%r13
	movq	%rcx, %rsi
LBB0_129:                               ## %.lr.ph.i.93.prol
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	24(%r14,%rdi,8), %rax
	movq	%rax, 24(%rsi,%rdi,8)
	incq	%rdi
	incq	%r13
	jne	LBB0_129
	jmp	LBB0_130
LBB0_146:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	%rcx, %rax
	movq	%rsi, -72(%rbp)         ## 8-byte Spill
	imulq	%r13, %r11
	subq	%r11, %rdx
	js	LBB0_113
## BB#147:                              ##   in Loop: Header=BB0_5 Depth=1
	movq	%r10, -64(%rbp)         ## 8-byte Spill
	movq	%r13, %rcx
	negq	%rcx
	imulq	%rax, %rcx
	leaq	(%r8,%rcx,8), %rcx
	cmpq	%r9, %rcx
	jae	LBB0_150
## BB#148:                              ##   in Loop: Header=BB0_5 Depth=1
	subq	%rcx, %r9
	sarq	$3, %r9
	subq	%r9, %rdx
	movq	%rcx, %r9
	js	LBB0_149
LBB0_150:                               ## %.thread106
                                        ##   in Loop: Header=BB0_5 Depth=1
	testq	%r13, %r13
	jle	LBB0_151
## BB#152:                              ## %.preheader.lr.ph
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rcx, -320(%rbp)        ## 8-byte Spill
	movq	%rdx, -80(%rbp)         ## 8-byte Spill
	movq	-144(%rbp), %rbx        ## 8-byte Reload
	movq	%rbx, %rcx
	movq	%rax, %r15
	movq	%r15, -136(%rbp)        ## 8-byte Spill
	subq	%r15, %rcx
	movq	%rcx, -216(%rbp)        ## 8-byte Spill
	leaq	(%r14,%rcx,8), %rcx
	leaq	-2048(,%rbx,8), %r11
	leaq	(,%r15,8), %rdx
	movq	%rdx, -184(%rbp)        ## 8-byte Spill
	movq	%r11, %rsi
	subq	%rdx, %rsi
	movq	%rsi, -168(%rbp)        ## 8-byte Spill
	leaq	-1(%r13), %rdx
	imulq	%rdx, %rsi
	leaq	-2024(%rsi,%rcx), %r12
	movq	-72(%rbp), %rsi         ## 8-byte Reload
	leaq	(%rsi,%rbx,8), %rcx
	imulq	%r11, %rdx
	leaq	-2024(%rdx,%rcx), %rax
	movq	%rax, -328(%rbp)        ## 8-byte Spill
	leaq	-254(%rbx), %r10
	subq	%r15, %r10
	testw	%r15w, %r15w
	movq	%r15, %rdi
	movl	$1, %edx
	cmoveq	%rdx, %rdi
	movq	%rdi, -152(%rbp)        ## 8-byte Spill
	movq	-128(%rbp), %rax        ## 8-byte Reload
	leaq	-4(%rax), %rdx
	movq	%rdx, -288(%rbp)        ## 8-byte Spill
	shrl	$2, %edx
	incl	%edx
	movq	%rbx, %rcx
	addq	$-257, %rcx             ## imm = 0xFFFFFFFFFFFFFEFF
	subq	%r15, %rcx
	movq	%rcx, -224(%rbp)        ## 8-byte Spill
	movq	%rax, %rcx
	andq	$-4, %rcx
	movq	%rcx, -240(%rbp)        ## 8-byte Spill
	andl	$3, %edx
	movq	%rdx, -296(%rbp)        ## 8-byte Spill
	andl	$65532, %edi            ## imm = 0xFFFC
	movq	%rdi, -208(%rbp)        ## 8-byte Spill
	movq	%r15, %rcx
	subq	%rdi, %rcx
	movq	%rcx, -248(%rbp)        ## 8-byte Spill
	cmpq	$1, %r15
	movl	$1, %ecx
	cmovaq	%r15, %rcx
	movq	%r13, %rdx
	movq	%r13, -176(%rbp)        ## 8-byte Spill
	imulq	%r15, %rdx
	shlq	$3, %rdx
	subq	%rdx, %r8
	leaq	(%r14,%r10,8), %rdx
	movq	%rdx, -256(%rbp)        ## 8-byte Spill
	andl	$65532, %ecx            ## imm = 0xFFFC
	movq	%rcx, -232(%rbp)        ## 8-byte Spill
	leaq	-2032(%rsi,%rbx,8), %rcx
	movq	%rcx, -272(%rbp)        ## 8-byte Spill
	leal	-260(%rbx), %ecx
	subl	%r15d, %ecx
	shrl	$2, %ecx
	incl	%ecx
	andl	$3, %ecx
	negq	%rcx
	movq	%rcx, -304(%rbp)        ## 8-byte Spill
	andq	$-4, %rax
	movq	%rax, -312(%rbp)        ## 8-byte Spill
	leaq	24(%rsi,%r15,8), %rax
	movq	%rax, -264(%rbp)        ## 8-byte Spill
	leaq	16(%rsi), %rbx
	leaq	24(%rsi), %r13
	leaq	72(%rsi,%r15,8), %rcx
	movq	%rcx, -192(%rbp)        ## 8-byte Spill
	leaq	40(%rsi,%r15,8), %rcx
	movq	%rcx, -160(%rbp)        ## 8-byte Spill
	leaq	24(%r14), %rcx
	movq	%rcx, -280(%rbp)        ## 8-byte Spill
	leaq	136(%r14), %rdx
	movq	%rdx, -200(%rbp)        ## 8-byte Spill
	addq	$80, %r14
	movq	%rax, -144(%rbp)        ## 8-byte Spill
	xorl	%r10d, %r10d
LBB0_153:                               ## %.lr.ph.preheader
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Loop Header: Depth=2
                                        ##       Child Loop BB0_155 Depth 3
                                        ##       Child Loop BB0_158 Depth 3
                                        ##       Child Loop BB0_166 Depth 3
                                        ##       Child Loop BB0_169 Depth 3
                                        ##       Child Loop BB0_173 Depth 3
                                        ##       Child Loop BB0_176 Depth 3
	cmpq	$4, -152(%rbp)          ## 8-byte Folded Reload
	movq	-136(%rbp), %rax        ## 8-byte Reload
	jb	LBB0_157
## BB#154:                              ## %min.iters.checked1231
                                        ##   in Loop: Header=BB0_153 Depth=2
	cmpq	$0, -208(%rbp)          ## 8-byte Folded Reload
	movq	-232(%rbp), %rdx        ## 8-byte Reload
	movq	-136(%rbp), %rax        ## 8-byte Reload
	movq	%rax, %rdi
	je	LBB0_157
LBB0_155:                               ## %vector.body1226
                                        ##   Parent Loop BB0_5 Depth=1
                                        ##     Parent Loop BB0_153 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	movd	%rdi, %xmm3
	pshufd	$68, %xmm3, %xmm3       ## xmm3 = xmm3[0,1,0,1]
	movdqa	%xmm3, %xmm4
	paddq	%xmm1, %xmm4
	paddq	%xmm2, %xmm3
	movd	%xmm4, %rax
	leaq	(%r13,%rax,8), %rsi
	movd	%rsi, %xmm5
	pshufd	$78, %xmm4, %xmm4       ## xmm4 = xmm4[2,3,0,1]
	movd	%xmm4, %rsi
	leaq	(%r13,%rsi,8), %rsi
	movd	%rsi, %xmm4
	punpcklqdq	%xmm5, %xmm4    ## xmm4 = xmm4[0],xmm5[0]
	movd	%xmm3, %rsi
	leaq	(%r13,%rsi,8), %rsi
	movd	%rsi, %xmm5
	pshufd	$78, %xmm3, %xmm3       ## xmm3 = xmm3[2,3,0,1]
	movd	%xmm3, %rsi
	leaq	(%r13,%rsi,8), %rsi
	movd	%rsi, %xmm3
	punpcklqdq	%xmm5, %xmm3    ## xmm3 = xmm3[0],xmm5[0]
	movdqu	%xmm4, -8(%r8,%rax,8)
	movdqu	%xmm3, -24(%r8,%rax,8)
	addq	$-4, %rdi
	addq	$-4, %rdx
	jne	LBB0_155
## BB#156:                              ## %middle.block1227
                                        ##   in Loop: Header=BB0_153 Depth=2
	movq	-208(%rbp), %rax        ## 8-byte Reload
	cmpq	%rax, -152(%rbp)        ## 8-byte Folded Reload
	movq	-248(%rbp), %rax        ## 8-byte Reload
	je	LBB0_159
LBB0_157:                               ## %.lr.ph.preheader1249
                                        ##   in Loop: Header=BB0_153 Depth=2
	leaq	(%rbx,%rax,8), %rdx
	incq	%rax
	.align	4, 0x90
LBB0_158:                               ## %.lr.ph
                                        ##   Parent Loop BB0_5 Depth=1
                                        ##     Parent Loop BB0_153 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	movq	%rdx, -16(%r8,%rax,8)
	decq	%rax
	addq	$-8, %rdx
	cmpq	$1, %rax
	jg	LBB0_158
LBB0_159:                               ## %._crit_edge
                                        ##   in Loop: Header=BB0_153 Depth=2
	movq	%rbx, -72(%rbp)         ## 8-byte Spill
	cmpq	$0, -128(%rbp)          ## 8-byte Folded Reload
	jle	LBB0_177
## BB#160:                              ## %.lr.ph.i.89.preheader
                                        ##   in Loop: Header=BB0_153 Depth=2
	cmpq	$4, -128(%rbp)          ## 8-byte Folded Reload
	movl	$0, %edi
	jb	LBB0_171
## BB#161:                              ## %min.iters.checked1191
                                        ##   in Loop: Header=BB0_153 Depth=2
	cmpq	$0, -240(%rbp)          ## 8-byte Folded Reload
	movl	$0, %edi
	je	LBB0_171
## BB#162:                              ## %vector.memcheck1212
                                        ##   in Loop: Header=BB0_153 Depth=2
	movq	%r11, %rax
	imulq	%r10, %rax
	movq	-168(%rbp), %rdx        ## 8-byte Reload
	imulq	%r10, %rdx
	movq	-264(%rbp), %rsi        ## 8-byte Reload
	leaq	(%rsi,%rax), %rsi
	movq	-256(%rbp), %rdi        ## 8-byte Reload
	leaq	(%rdi,%rdx), %rdi
	cmpq	%rdi, %rsi
	ja	LBB0_164
## BB#163:                              ## %vector.memcheck1212
                                        ##   in Loop: Header=BB0_153 Depth=2
	addq	-272(%rbp), %rax        ## 8-byte Folded Reload
	addq	-280(%rbp), %rdx        ## 8-byte Folded Reload
	cmpq	%rax, %rdx
	movl	$0, %edi
	jbe	LBB0_171
LBB0_164:                               ## %vector.body1187.preheader
                                        ##   in Loop: Header=BB0_153 Depth=2
	cmpq	$0, -296(%rbp)          ## 8-byte Folded Reload
	movl	$0, %eax
	je	LBB0_167
## BB#165:                              ## %vector.body1187.prol.preheader
                                        ##   in Loop: Header=BB0_153 Depth=2
	movq	-184(%rbp), %rax        ## 8-byte Reload
	leaq	(%r13,%rax), %rdx
	movq	-304(%rbp), %rsi        ## 8-byte Reload
	xorl	%eax, %eax
LBB0_166:                               ## %vector.body1187.prol
                                        ##   Parent Loop BB0_5 Depth=1
                                        ##     Parent Loop BB0_153 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	movdqu	(%rcx,%rax,8), %xmm3
	movdqu	16(%rcx,%rax,8), %xmm4
	movdqu	%xmm3, (%rdx,%rax,8)
	movdqu	%xmm4, 16(%rdx,%rax,8)
	addq	$4, %rax
	incq	%rsi
	jne	LBB0_166
LBB0_167:                               ## %vector.body1187.preheader.split
                                        ##   in Loop: Header=BB0_153 Depth=2
	cmpq	$12, -288(%rbp)         ## 8-byte Folded Reload
	jb	LBB0_170
## BB#168:                              ## %vector.body1187.preheader.split.split
                                        ##   in Loop: Header=BB0_153 Depth=2
	movq	-312(%rbp), %rdx        ## 8-byte Reload
	subq	%rax, %rdx
	movq	-192(%rbp), %rsi        ## 8-byte Reload
	leaq	(%rsi,%rax,8), %rdi
	movq	-200(%rbp), %rsi        ## 8-byte Reload
	leaq	(%rsi,%rax,8), %rsi
LBB0_169:                               ## %vector.body1187
                                        ##   Parent Loop BB0_5 Depth=1
                                        ##     Parent Loop BB0_153 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	movups	-112(%rsi), %xmm3
	movups	-96(%rsi), %xmm4
	movups	%xmm3, -48(%rdi)
	movups	%xmm4, -32(%rdi)
	movups	-80(%rsi), %xmm3
	movups	-64(%rsi), %xmm4
	movups	%xmm3, -16(%rdi)
	movups	%xmm4, (%rdi)
	movups	-48(%rsi), %xmm3
	movups	-32(%rsi), %xmm4
	movups	%xmm3, 16(%rdi)
	movups	%xmm4, 32(%rdi)
	movdqu	-16(%rsi), %xmm3
	movdqu	(%rsi), %xmm4
	movdqu	%xmm3, 48(%rdi)
	movdqu	%xmm4, 64(%rdi)
	subq	$-128, %rdi
	subq	$-128, %rsi
	addq	$-16, %rdx
	jne	LBB0_169
LBB0_170:                               ## %middle.block1188
                                        ##   in Loop: Header=BB0_153 Depth=2
	movq	-240(%rbp), %rax        ## 8-byte Reload
	cmpq	%rax, -128(%rbp)        ## 8-byte Folded Reload
	movq	%rax, %rdi
	je	LBB0_177
LBB0_171:                               ## %.lr.ph.i.89.preheader1248
                                        ##   in Loop: Header=BB0_153 Depth=2
	movq	-216(%rbp), %rbx        ## 8-byte Reload
	movl	%ebx, %eax
	subl	%edi, %eax
	movq	-224(%rbp), %rdx        ## 8-byte Reload
	subq	%rdi, %rdx
	testb	$7, %al
	je	LBB0_174
## BB#172:                              ## %.lr.ph.i.89.prol.preheader
                                        ##   in Loop: Header=BB0_153 Depth=2
	movq	-144(%rbp), %rax        ## 8-byte Reload
	leaq	(%rax,%rdi,8), %rsi
	movl	%ebx, %eax
	subl	%edi, %eax
	andl	$7, %eax
	negq	%rax
LBB0_173:                               ## %.lr.ph.i.89.prol
                                        ##   Parent Loop BB0_5 Depth=1
                                        ##     Parent Loop BB0_153 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	movq	(%rcx,%rdi,8), %rbx
	movq	%rbx, (%rsi)
	incq	%rdi
	addq	$8, %rsi
	incq	%rax
	jne	LBB0_173
LBB0_174:                               ## %.lr.ph.i.89.preheader1248.split
                                        ##   in Loop: Header=BB0_153 Depth=2
	cmpq	$7, %rdx
	jb	LBB0_177
## BB#175:                              ## %.lr.ph.i.89.preheader1248.split.split
                                        ##   in Loop: Header=BB0_153 Depth=2
	movq	-128(%rbp), %rdx        ## 8-byte Reload
	subq	%rdi, %rdx
	movq	-160(%rbp), %rax        ## 8-byte Reload
	leaq	(%rax,%rdi,8), %r15
	leaq	(%r14,%rdi,8), %rsi
LBB0_176:                               ## %.lr.ph.i.89
                                        ##   Parent Loop BB0_5 Depth=1
                                        ##     Parent Loop BB0_153 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	movq	-56(%rsi), %rax
	movq	%rax, -16(%r15)
	movq	-48(%rsi), %rax
	movq	%rax, -8(%r15)
	movq	-40(%rsi), %rax
	movq	%rax, (%r15)
	movq	-32(%rsi), %rax
	movq	%rax, 8(%r15)
	movq	-24(%rsi), %rax
	movq	%rax, 16(%r15)
	movq	-16(%rsi), %rax
	movq	%rax, 24(%r15)
	movq	-8(%rsi), %rax
	movq	%rax, 32(%r15)
	movq	(%rsi), %rax
	movq	%rax, 40(%r15)
	addq	$64, %r15
	addq	$64, %rsi
	addq	$-8, %rdx
	jne	LBB0_176
LBB0_177:                               ## %copy.exit90
                                        ##   in Loop: Header=BB0_153 Depth=2
	addq	-184(%rbp), %r8         ## 8-byte Folded Reload
	addq	%r11, %r13
	incq	%r10
	movq	-72(%rbp), %rbx         ## 8-byte Reload
	addq	%r11, %rbx
	movq	-168(%rbp), %rax        ## 8-byte Reload
	addq	%rax, %rcx
	addq	%r11, -192(%rbp)        ## 8-byte Folded Spill
	addq	%rax, -200(%rbp)        ## 8-byte Folded Spill
	addq	%r11, -144(%rbp)        ## 8-byte Folded Spill
	addq	%r11, -160(%rbp)        ## 8-byte Folded Spill
	addq	%rax, %r14
	cmpq	-176(%rbp), %r10        ## 8-byte Folded Reload
	jne	LBB0_153
## BB#178:                              ## %.loopexit135.loopexit
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	-328(%rbp), %rsi        ## 8-byte Reload
	movq	-320(%rbp), %r8         ## 8-byte Reload
	movq	-64(%rbp), %r10         ## 8-byte Reload
	jmp	LBB0_179
LBB0_151:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	%rcx, %r8
	movq	-64(%rbp), %r10         ## 8-byte Reload
	movq	%r15, %rsi
	jmp	LBB0_350
LBB0_127:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	%rcx, %rsi
LBB0_130:                               ## %.lr.ph.i.93.preheader1263.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$7, %rdx
	jb	LBB0_133
## BB#131:                              ## %.lr.ph.i.93.preheader1263.split.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r11, %rax
	subq	%rdi, %rax
	leaq	80(%rsi,%rdi,8), %rdx
	leaq	80(%r14,%rdi,8), %rsi
LBB0_132:                               ## %.lr.ph.i.93
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-56(%rsi), %rdi
	movq	%rdi, -56(%rdx)
	movq	-48(%rsi), %rdi
	movq	%rdi, -48(%rdx)
	movq	-40(%rsi), %rdi
	movq	%rdi, -40(%rdx)
	movq	-32(%rsi), %rdi
	movq	%rdi, -32(%rdx)
	movq	-24(%rsi), %rdi
	movq	%rdi, -24(%rdx)
	movq	-16(%rsi), %rdi
	movq	%rdi, -16(%rdx)
	movq	-8(%rsi), %rdi
	movq	%rdi, -8(%rdx)
	movq	(%rsi), %rdi
	movq	%rdi, (%rdx)
	addq	$64, %rdx
	addq	$64, %rsi
	addq	$-8, %rax
	jne	LBB0_132
LBB0_133:                               ## %copy.exit94
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	(%r12,%r11,8), %r12
	leaq	(%r15,%r11,8), %rsi
	jmp	LBB0_179
	.align	4, 0x90
LBB0_5:                                 ## %.lr.ph316
                                        ## =>This Loop Header: Depth=1
                                        ##     Child Loop BB0_253 Depth 2
                                        ##     Child Loop BB0_256 Depth 2
                                        ##     Child Loop BB0_260 Depth 2
                                        ##     Child Loop BB0_263 Depth 2
                                        ##     Child Loop BB0_271 Depth 2
                                        ##     Child Loop BB0_276 Depth 2
                                        ##     Child Loop BB0_234 Depth 2
                                        ##     Child Loop BB0_237 Depth 2
                                        ##     Child Loop BB0_241 Depth 2
                                        ##     Child Loop BB0_244 Depth 2
                                        ##     Child Loop BB0_201 Depth 2
                                        ##     Child Loop BB0_204 Depth 2
                                        ##     Child Loop BB0_153 Depth 2
                                        ##       Child Loop BB0_155 Depth 3
                                        ##       Child Loop BB0_158 Depth 3
                                        ##       Child Loop BB0_166 Depth 3
                                        ##       Child Loop BB0_169 Depth 3
                                        ##       Child Loop BB0_173 Depth 3
                                        ##       Child Loop BB0_176 Depth 3
                                        ##     Child Loop BB0_141 Depth 2
                                        ##     Child Loop BB0_144 Depth 2
                                        ##     Child Loop BB0_121 Depth 2
                                        ##     Child Loop BB0_124 Depth 2
                                        ##     Child Loop BB0_129 Depth 2
                                        ##     Child Loop BB0_132 Depth 2
                                        ##     Child Loop BB0_99 Depth 2
                                        ##     Child Loop BB0_102 Depth 2
                                        ##     Child Loop BB0_106 Depth 2
                                        ##     Child Loop BB0_109 Depth 2
                                        ##     Child Loop BB0_78 Depth 2
                                        ##     Child Loop BB0_81 Depth 2
                                        ##     Child Loop BB0_85 Depth 2
                                        ##     Child Loop BB0_88 Depth 2
                                        ##     Child Loop BB0_58 Depth 2
                                        ##     Child Loop BB0_61 Depth 2
                                        ##     Child Loop BB0_35 Depth 2
                                        ##     Child Loop BB0_38 Depth 2
                                        ##     Child Loop BB0_43 Depth 2
                                        ##     Child Loop BB0_46 Depth 2
                                        ##     Child Loop BB0_309 Depth 2
                                        ##     Child Loop BB0_312 Depth 2
                                        ##     Child Loop BB0_316 Depth 2
                                        ##     Child Loop BB0_319 Depth 2
                                        ##     Child Loop BB0_333 Depth 2
                                        ##     Child Loop BB0_336 Depth 2
                                        ##     Child Loop BB0_292 Depth 2
                                        ##     Child Loop BB0_295 Depth 2
	movq	(%r14), %rax
	leaq	-1(%rdx), %rbx
	testb	$1, %al
	jne	LBB0_348
## BB#6:                                ##   in Loop: Header=BB0_5 Depth=1
	movq	%rsi, (%r14)
	movq	%rsi, (%r15)
	leaq	(%rax,%rdi), %rcx
	movq	%rcx, (%rsi)
	testb	$2, %cl
	jne	LBB0_7
## BB#278:                              ##   in Loop: Header=BB0_5 Depth=1
	movq	%rsi, %r12
	movslq	-4(%rcx), %r11
	cmpq	$2, %r11
	movl	%r11d, %r13d
	jl	LBB0_341
## BB#279:                              ##   in Loop: Header=BB0_5 Depth=1
	cmpl	$255, %r11d
	jg	LBB0_299
## BB#280:                              ##   in Loop: Header=BB0_5 Depth=1
	subq	%r11, %rbx
	js	LBB0_281
## BB#286:                              ##   in Loop: Header=BB0_5 Depth=1
	movl	$1, %ecx
	subq	%r11, %rcx
	leaq	(%r8,%rcx,8), %rcx
	cmpq	%r9, %rcx
	jae	LBB0_287
## BB#297:                              ##   in Loop: Header=BB0_5 Depth=1
	subq	%rcx, %r9
	sarq	$3, %r9
	subq	%r9, %rbx
	movq	%rbx, %rax
	movq	%rcx, %rbx
	jns	LBB0_288
	jmp	LBB0_298
	.align	4, 0x90
LBB0_348:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	-1(%rax,%r14), %rax
	movq	%rax, (%r15)
	addq	$8, %r14
	movq	%rbx, %rdx
LBB0_349:                               ## %.loopexit135
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r14, %r12
LBB0_350:                               ## %.loopexit135
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	%r10, %r8
	je	LBB0_354
## BB#351:                              ##   in Loop: Header=BB0_5 Depth=1
	movq	(%r8), %r15
	addq	$8, %r8
LBB0_352:                               ## %.backedge
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r9, %rbx
	movq	%r8, %rcx
	jmp	LBB0_353
	.align	4, 0x90
LBB0_7:                                 ##   in Loop: Header=BB0_5 Depth=1
	movzwl	-2(%rcx), %r13d
	cmpq	$2, %r13
	je	LBB0_185
## BB#8:                                ##   in Loop: Header=BB0_5 Depth=1
	movq	%rdx, %rbx
	movzwl	%r13w, %edx
	cmpl	$1, %edx
	je	LBB0_182
## BB#9:                                ##   in Loop: Header=BB0_5 Depth=1
	testw	%r13w, %r13w
	jne	LBB0_192
## BB#10:                               ##   in Loop: Header=BB0_5 Depth=1
	cmpq	-88(%rbp), %rcx         ## 8-byte Folded Reload
	jne	LBB0_18
## BB#11:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	8(%r14), %rax
	cmpq	$32, %rax
	movq	%rbx, %rdx
	ja	LBB0_15
## BB#12:                               ##   in Loop: Header=BB0_5 Depth=1
	shlq	$4, %rax
	addq	small_integers@GOTPCREL(%rip), %rax
	jmp	LBB0_13
LBB0_341:                               ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$2, %rdx
	jle	LBB0_342
## BB#343:                              ##   in Loop: Header=BB0_5 Depth=1
	addq	$-3, %rdx
	addq	$8, %r14
	movq	%r12, %rsi
	testl	%r11d, %r11d
	je	LBB0_344
## BB#345:                              ##   in Loop: Header=BB0_5 Depth=1
	leaq	8(%rsi), %r15
	jmp	LBB0_346
LBB0_299:                               ##   in Loop: Header=BB0_5 Depth=1
	cmpl	$257, %r13d             ## imm = 0x101
	jne	LBB0_300
## BB#339:                              ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$2, %rdx
	movq	%r12, %rsi
	jle	LBB0_50
## BB#340:                              ##   in Loop: Header=BB0_5 Depth=1
	addq	$-3, %rdx
	movq	8(%r14), %rax
	movq	%rax, 8(%rsi)
	addq	$16, %r14
	addq	$24, %rsi
	jmp	LBB0_349
LBB0_185:                               ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$2, %rdx
	jle	LBB0_50
## BB#186:                              ##   in Loop: Header=BB0_5 Depth=1
	cmpq	%r9, %r8
	jbe	LBB0_188
## BB#187:                              ##   in Loop: Header=BB0_5 Depth=1
	addq	$-3, %rdx
	jmp	LBB0_191
LBB0_182:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	%rbx, %rdx
	cmpq	$1, %rdx
	jle	LBB0_23
## BB#183:                              ##   in Loop: Header=BB0_5 Depth=1
	addq	$-2, %rdx
LBB0_184:                               ##   in Loop: Header=BB0_5 Depth=1
	leaq	8(%rsi), %r15
	addq	$8, %r14
	addq	$16, %rsi
	jmp	LBB0_347
LBB0_192:                               ##   in Loop: Header=BB0_5 Depth=1
	cmpl	$255, %r13d
	ja	LBB0_208
## BB#193:                              ##   in Loop: Header=BB0_5 Depth=1
	addq	$-2, %rbx
	subq	%r13, %rbx
	js	LBB0_194
## BB#195:                              ##   in Loop: Header=BB0_5 Depth=1
	movq	%rbx, %r11
	movq	%rsi, -72(%rbp)         ## 8-byte Spill
	leaq	24(%rsi), %r12
	movq	%r12, 16(%rsi)
	movl	$1, %ecx
	subq	%r13, %rcx
	leaq	(%r8,%rcx,8), %rcx
	cmpq	%r9, %rcx
	jae	LBB0_196
## BB#206:                              ##   in Loop: Header=BB0_5 Depth=1
	movq	%r9, %rdx
	subq	%rcx, %rdx
	sarq	$3, %rdx
	subq	%rdx, %r11
	movq	%rcx, %rbx
	jns	LBB0_197
	jmp	LBB0_207
LBB0_344:                               ## %.loopexit135
                                        ##   in Loop: Header=BB0_5 Depth=1
	addq	$24, %rsi
	jmp	LBB0_349
LBB0_18:                                ##   in Loop: Header=BB0_5 Depth=1
	cmpq	-96(%rbp), %rcx         ## 8-byte Folded Reload
	movq	%rbx, %rdx
	jne	LBB0_20
## BB#19:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	8(%r14), %rax
	shlq	$4, %rax
	andl	$4080, %eax             ## imm = 0xFF0
	addq	static_characters@GOTPCREL(%rip), %rax
LBB0_13:                                ## %.loopexit135
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rax, (%r15)
	movq	%rax, (%r14)
	addq	$16, %r14
	jmp	LBB0_349
LBB0_300:                               ##   in Loop: Header=BB0_5 Depth=1
	movzbl	%r11b, %eax
	subq	%rax, %rbx
	js	LBB0_301
## BB#302:                              ##   in Loop: Header=BB0_5 Depth=1
	movq	%rax, -80(%rbp)         ## 8-byte Spill
	movq	%r11, %r15
	sarq	$8, %r15
	leaq	8(%r14), %r10
	testq	%r15, %r15
	jle	LBB0_320
## BB#303:                              ## %.lr.ph.i.77.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$4, %r15
	movl	$0, %esi
	jb	LBB0_314
## BB#304:                              ## %min.iters.checked851
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r15, %rcx
	andq	$-4, %rcx
	movl	$0, %esi
	je	LBB0_314
## BB#305:                              ## %vector.memcheck
                                        ##   in Loop: Header=BB0_5 Depth=1
	movzbl	%r13b, %edx
	movq	%rdx, %rax
	subq	%r15, %rax
	leaq	8(%r12,%rax,8), %rax
	leaq	(%r14,%r15,8), %rsi
	cmpq	%rsi, %rax
	ja	LBB0_307
## BB#306:                              ## %vector.memcheck
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	(%r12,%rdx,8), %rax
	cmpq	%rax, %r10
	movl	$0, %esi
	jbe	LBB0_314
LBB0_307:                               ## %vector.body847.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	-4(%r15), %rax
	movq	%rax, -136(%rbp)        ## 8-byte Spill
	shrl	$2, %eax
	incl	%eax
	testb	$3, %al
	movl	$0, %edx
	je	LBB0_310
## BB#308:                              ## %vector.body847.prol.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	movzbl	%r11b, %eax
	subq	%r15, %rax
	leaq	24(%r12,%rax,8), %rsi
	leal	-4(%r15), %edi
	shrl	$2, %edi
	incl	%edi
	andl	$3, %edi
	negq	%rdi
	xorl	%edx, %edx
	.align	4, 0x90
LBB0_309:                               ## %vector.body847.prol
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movdqu	8(%r14,%rdx,8), %xmm3
	movdqu	24(%r14,%rdx,8), %xmm4
	movdqu	%xmm3, -16(%rsi,%rdx,8)
	movdqu	%xmm4, (%rsi,%rdx,8)
	addq	$4, %rdx
	incq	%rdi
	jne	LBB0_309
LBB0_310:                               ## %vector.body847.preheader.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$12, -136(%rbp)         ## 8-byte Folded Reload
	jb	LBB0_313
## BB#311:                              ## %vector.body847.preheader.split.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r15, %rsi
	andq	$-4, %rsi
	subq	%rdx, %rsi
	movzbl	%r11b, %eax
	addq	%rdx, %rax
	subq	%r15, %rax
	leaq	120(%r12,%rax,8), %rax
	leaq	120(%r14,%rdx,8), %rdx
	.align	4, 0x90
LBB0_312:                               ## %vector.body847
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-112(%rdx), %xmm3
	movups	-96(%rdx), %xmm4
	movups	%xmm3, -112(%rax)
	movups	%xmm4, -96(%rax)
	movups	-80(%rdx), %xmm3
	movups	-64(%rdx), %xmm4
	movups	%xmm3, -80(%rax)
	movups	%xmm4, -64(%rax)
	movups	-48(%rdx), %xmm3
	movups	-32(%rdx), %xmm4
	movups	%xmm3, -48(%rax)
	movups	%xmm4, -32(%rax)
	movdqu	-16(%rdx), %xmm3
	movdqu	(%rdx), %xmm4
	movdqu	%xmm3, -16(%rax)
	movdqu	%xmm4, (%rax)
	subq	$-128, %rax
	subq	$-128, %rdx
	addq	$-16, %rsi
	jne	LBB0_312
LBB0_313:                               ## %middle.block848
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	%rcx, %r15
	movq	%rcx, %rsi
	movq	__ARRAY__@GOTPCREL(%rip), %rdi
	je	LBB0_320
LBB0_314:                               ## %.lr.ph.i.77.preheader1253
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rdi, %r11
	movl	%r15d, %edx
	subl	%esi, %edx
	leaq	-1(%r15), %rax
	subq	%rsi, %rax
	testb	$7, %dl
	je	LBB0_317
## BB#315:                              ## %.lr.ph.i.77.prol.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	movzbl	%r13b, %edx
	subq	%r15, %rdx
	leaq	8(%r12,%rdx,8), %rdx
	movl	%r15d, %edi
	subl	%esi, %edi
	andl	$7, %edi
	negq	%rdi
	.align	4, 0x90
LBB0_316:                               ## %.lr.ph.i.77.prol
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	8(%r14,%rsi,8), %rcx
	movq	%rcx, (%rdx,%rsi,8)
	incq	%rsi
	incq	%rdi
	jne	LBB0_316
LBB0_317:                               ## %.lr.ph.i.77.preheader1253.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$7, %rax
	movq	%r11, %rdi
	jb	LBB0_320
## BB#318:                              ## %.lr.ph.i.77.preheader1253.split.split
                                        ##   in Loop: Header=BB0_5 Depth=1
	movzbl	%r13b, %eax
	leaq	64(%r12,%rax,8), %rax
	leaq	64(%r14,%rsi,8), %rdx
	subq	%r15, %rsi
	.align	4, 0x90
LBB0_319:                               ## %.lr.ph.i.77
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-56(%rdx), %rcx
	movq	%rcx, -56(%rax,%rsi,8)
	movq	-48(%rdx), %rcx
	movq	%rcx, -48(%rax,%rsi,8)
	movq	-40(%rdx), %rcx
	movq	%rcx, -40(%rax,%rsi,8)
	movq	-32(%rdx), %rcx
	movq	%rcx, -32(%rax,%rsi,8)
	movq	-24(%rdx), %rcx
	movq	%rcx, -24(%rax,%rsi,8)
	movq	-16(%rdx), %rcx
	movq	%rcx, -16(%rax,%rsi,8)
	movq	-8(%rdx), %rcx
	movq	%rcx, -8(%rax,%rsi,8)
	movq	(%rdx), %rcx
	movq	%rcx, (%rax,%rsi,8)
	addq	$64, %rdx
	addq	$8, %rsi
	jne	LBB0_319
LBB0_320:                               ## %copy.exit78
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r12, %rax
	movq	%r15, %rcx
	leaq	8(%rax), %r15
	leaq	(%r10,%rcx,8), %r12
	movq	-80(%rbp), %rsi         ## 8-byte Reload
	cmpq	%rcx, %rsi
	jne	LBB0_322
## BB#321:                              ##   in Loop: Header=BB0_5 Depth=1
	leaq	(%r15,%rcx,8), %rsi
	movq	%rbx, %rdx
	movq	-64(%rbp), %r10         ## 8-byte Reload
	jmp	LBB0_350
LBB0_208:                               ##   in Loop: Header=BB0_5 Depth=1
	movzwl	(%rcx), %r11d
	cmpq	$258, %r13              ## imm = 0x102
	movq	%rbx, %rdx
	je	LBB0_213
## BB#209:                              ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$257, %r13              ## imm = 0x101
	jne	LBB0_224
## BB#210:                              ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$1, %rdx
	jle	LBB0_23
## BB#211:                              ##   in Loop: Header=BB0_5 Depth=1
	addq	$-2, %rdx
	testw	%r11w, %r11w
	jne	LBB0_184
	jmp	LBB0_212
LBB0_287:                               ##   in Loop: Header=BB0_5 Depth=1
	movq	%rbx, %rax
	movq	%r9, %rbx
LBB0_288:                               ## %.thread114
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rax, -80(%rbp)         ## 8-byte Spill
	leaq	-1(%r11), %r9
	movq	%r12, -72(%rbp)         ## 8-byte Spill
	leaq	8(%r12), %r15
	leaq	16(%r12), %rdi
	leaq	(%r12,%r11,8), %rdx
	movq	%rdx, -16(%rcx,%r11,8)
	cmpl	$3, %r13d
	jl	LBB0_296
## BB#289:                              ## %.lr.ph306.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	leaq	-3(%r11), %rsi
	leaq	-2(%r11), %rax
	cmpq	$4, %rax
	jb	LBB0_294
## BB#290:                              ## %min.iters.checked
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rax, %rdx
	andq	$-4, %rdx
	movq	%rax, %r10
	andq	$-4, %r10
	je	LBB0_294
## BB#291:                              ## %vector.body.preheader
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%rax, -136(%rbp)        ## 8-byte Spill
	movq	%r9, -128(%rbp)         ## 8-byte Spill
	subq	%rdx, %rsi
	movq	-72(%rbp), %rax         ## 8-byte Reload
	leaq	-16(%rax,%r11,8), %rdx
	leaq	-24(%r8), %r13
	leaq	-3(%r11), %r9
	movq	%r10, %r12
	movq	%r10, -144(%rbp)        ## 8-byte Spill
	.align	4, 0x90
LBB0_292:                               ## %vector.body
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movd	%r9, %xmm3
	pshufd	$68, %xmm3, %xmm3       ## xmm3 = xmm3[0,1,0,1]
	paddq	%xmm0, %xmm3
	leaq	8(%rdx), %r10
	movd	%rdx, %xmm4
	movd	%r10, %xmm5
	punpcklqdq	%xmm5, %xmm4    ## xmm4 = xmm4[0],xmm5[0]
	movd	%xmm3, %rax
	leaq	(%rdi,%rax,8), %rax
	movd	%rax, %xmm5
	pshufd	$78, %xmm3, %xmm3       ## xmm3 = xmm3[2,3,0,1]
	movd	%xmm3, %rax
	leaq	(%rdi,%rax,8), %rax
	movd	%rax, %xmm3
	punpcklqdq	%xmm5, %xmm3    ## xmm3 = xmm3[0],xmm5[0]
	movdqu	%xmm4, (%r13)
	movdqu	%xmm3, -16(%r13)
	addq	$-32, %rdx
	addq	$-32, %r13
	addq	$-4, %r9
	addq	$-4, %r12
	jne	LBB0_292
## BB#293:                              ## %middle.block
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	-144(%rbp), %rax        ## 8-byte Reload
	cmpq	%rax, -136(%rbp)        ## 8-byte Folded Reload
	movq	-128(%rbp), %r9         ## 8-byte Reload
	je	LBB0_296
LBB0_294:                               ## %.lr.ph306.preheader1250
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	-72(%rbp), %rax         ## 8-byte Reload
	leaq	16(%rax,%rsi,8), %rdx
	incq	%rsi
	shlq	$3, %r11
	subq	%r11, %r8
	.align	4, 0x90
LBB0_295:                               ## %.lr.ph306
                                        ##   Parent Loop BB0_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	%rdx, (%r8,%rsi,8)
	addq	$-8, %rdx
	decq	%rsi
	jg	LBB0_295
LBB0_296:                               ## %._crit_edge.307
                                        ##   in Loop: Header=BB0_5 Depth=1
	addq	$8, %r14
	leaq	(%rdi,%r9,8), %rsi
	movq	%r14, %r12
	movq	-64(%rbp), %r10         ## 8-byte Reload
	movq	__ARRAY__@GOTPCREL(%rip), %rdi
	movq	-80(%rbp), %rdx         ## 8-byte Reload
	jmp	LBB0_353
LBB0_188:                               ##   in Loop: Header=BB0_5 Depth=1
	cmpq	$3, %rdx
	jle	LBB0_189
## BB#190:                              ##   in Loop: Header=BB0_5 Depth=1
	addq	$-4, %rdx
	addq	$-8, %r9
LBB0_191:                               ##   in Loop: Header=BB0_5 Depth=1
	leaq	16(%rsi), %rax
	movq	%rax, -8(%r8)
	addq	$-8, %r8
	leaq	8(%rsi), %r15
	addq	$8, %r14
LBB0_346:                               ## %.backedge
                                        ##   in Loop: Header=BB0_5 Depth=1
	addq	$24, %rsi
LBB0_347:                               ## %.backedge
                                        ##   in Loop: Header=BB0_5 Depth=1
	movq	%r9, %rbx
	movq	%r8, %rcx
	movq	%r14, %r12
LBB0_353:                               ## %.backedge
                                        ##   in Loop: Header=BB0_5 Depth=1
	testq	%rdx, %rdx
	movq	%r12, %r14
	movq	%rcx, %r8
	movq	%rbx, %r9
	jg	LBB0_5
LBB0_2:                                 ## %._crit_edge.317
	subq	%rbx, %r10
	leaq	8(%rsi,%r10), %rax
LBB0_3:                                 ## %.thread118
	movq	-56(%rbp), %rcx         ## 8-byte Reload
	movq	%rax, (%rcx)
	incq	%r12
	jmp	LBB0_355
LBB0_354:
	movq	-56(%rbp), %rax         ## 8-byte Reload
	movq	%rsi, (%rax)
	movq	-48(%rbp), %r12
	jmp	LBB0_355
LBB0_342:
	subq	%r9, %r10
	leaq	24(%r12,%r10), %rax
	jmp	LBB0_284
LBB0_23:
	subq	%r9, %r10
	leaq	16(%rsi,%r10), %rax
	jmp	LBB0_284
LBB0_281:
	leaq	(%r12,%r11,8), %rax
	jmp	LBB0_282
LBB0_194:
	leaq	(%rsi,%r13,8), %rax
	subq	%r9, %r10
	leaq	16(%r10,%rax), %rax
	jmp	LBB0_284
LBB0_301:
	leaq	(%r12,%rax,8), %rax
LBB0_282:                               ## %.thread118
	subq	%r9, %r10
	jmp	LBB0_283
LBB0_298:
	leaq	(%r12,%r11,8), %rax
	subq	%rcx, %r10
LBB0_283:                               ## %.thread118
	leaq	8(%r10,%rax), %rax
	jmp	LBB0_284
LBB0_189:
	addq	$-8, %r10
LBB0_50:
	subq	%r9, %r10
	leaq	24(%rsi,%r10), %rax
LBB0_284:                               ## %.thread118
	movq	-56(%rbp), %rcx         ## 8-byte Reload
	movq	%rax, (%rcx)
LBB0_285:                               ## %.thread118
	addq	$9, %r14
	movq	%r14, %r12
LBB0_355:                               ## %.thread118
	movq	%r12, %rax
	addq	$160, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
LBB0_16:
	addq	$8, %r14
	subq	%r9, %r10
	leaq	16(%rsi,%r10), %rax
	movq	-56(%rbp), %rcx         ## 8-byte Reload
	movq	%rax, (%rcx)
	incq	%r14
	movq	%r14, %r12
	jmp	LBB0_355
LBB0_207:
	leaq	(%r12,%r13,8), %rax
	subq	%r9, %r10
	leaq	-8(%r10,%rax), %rax
	jmp	LBB0_284
LBB0_225:
	leaq	(%rsi,%r10,8), %rax
	movq	-64(%rbp), %rcx         ## 8-byte Reload
	subq	%r9, %rcx
	addq	%rax, %rcx
	movq	-56(%rbp), %rax         ## 8-byte Reload
	movq	%rcx, (%rax)
	jmp	LBB0_285
LBB0_27:
	subq	%r9, %r13
	leaq	16(%r10,%r13), %rax
	jmp	LBB0_3
LBB0_325:
	leaq	(%r11,%rsi,8), %rax
	subq	%rdx, %r10
	jmp	LBB0_70
LBB0_220:
	subq	%r9, %r10
	leaq	32(%rsi,%r10), %rax
	jmp	LBB0_284
LBB0_277:
	decq	%r11
	movq	-128(%rbp), %rax        ## 8-byte Reload
	leaq	(%rax,%r11,8), %rax
	leaq	(%rax,%r15,8), %rax
	subq	%r14, %r10
	jmp	LBB0_70
LBB0_68:
	leaq	(%r11,%r13,8), %rax
	jmp	LBB0_69
LBB0_64:
	leaq	(%r15,%r11,8), %rcx
	movq	-64(%rbp), %r13         ## 8-byte Reload
	subq	%rax, %r13
	addq	%rcx, %r13
	movq	-56(%rbp), %rax         ## 8-byte Reload
	movq	%r13, (%rax)
	incq	%r12
	jmp	LBB0_355
LBB0_113:
	leaq	(%r15,%r11,8), %rax
LBB0_69:                                ## %.thread118
	subq	%r9, %r10
LBB0_70:                                ## %.thread118
	addq	%rax, %r10
	movq	-56(%rbp), %rax         ## 8-byte Reload
	movq	%r10, (%rax)
	incq	%r12
	jmp	LBB0_355
LBB0_149:
	movq	-64(%rbp), %r11         ## 8-byte Reload
	subq	%rcx, %r11
	addq	%r11, %r15
	movq	-56(%rbp), %rax         ## 8-byte Reload
	movq	%r15, (%rax)
	incq	%r12
	jmp	LBB0_355
	

	.globl	_remove_forwarding_pointers_from_string
	.align	4, 0x90
_remove_forwarding_pointers_from_string: ## @remove_forwarding_pointers_from_string
	
## BB#0:
	pushq	%rbp
Ltmp8:
	
Ltmp9:
	
	movq	%rsp, %rbp
Ltmp10:
	
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
Ltmp11:
	
Ltmp12:
	
Ltmp13:
	
Ltmp14:
	
	addq	$16, %rdi
	cmpq	%rsi, %rdi
	jae	LBB1_15
## BB#1:
	movq	__ARRAY__@GOTPCREL(%rip), %rax
	movq	REAL@GOTPCREL(%rip), %r12
	addq	$2, %r12
	movq	BOOL@GOTPCREL(%rip), %r11
	addq	$2, %r11
	movq	dINT@GOTPCREL(%rip), %r8
	addq	$2, %r8
	movq	CHAR@GOTPCREL(%rip), %r9
	addq	$2, %r9
	movq	__STRING__@GOTPCREL(%rip), %r10
	addq	$2, %r10
	jmp	LBB1_2
LBB1_17:                                ##   in Loop: Header=BB1_2 Depth=1
	movq	%rdx, %rdi
	jmp	LBB1_14
LBB1_20:                                ##   in Loop: Header=BB1_2 Depth=1
	leaq	(%rdx,%rdi,8), %rdi
	jmp	LBB1_14
LBB1_23:                                ##   in Loop: Header=BB1_2 Depth=1
	movzwl	(%rbx), %r14d
	movzwl	-2(%rbx), %r15d
	movq	$-256, %rbx
	subq	%r14, %rbx
	addq	%r15, %rbx
	imulq	%rdi, %rbx
	leaq	(%rdx,%rbx,8), %rdi
	jmp	LBB1_14
	.align	4, 0x90
LBB1_2:                                 ## %.lr.ph
                                        ## =>This Inner Loop Header: Depth=1
	movq	(%rdi), %rdx
	testb	$1, %dl
	jne	LBB1_13
## BB#3:                                ##   in Loop: Header=BB1_2 Depth=1
	movq	(%rdx), %rdx
	movq	%rdx, %rbx
	subq	%rax, %rbx
	movq	%rbx, (%rdi)
	testb	$2, %dl
	jne	LBB1_4
## BB#26:                               ##   in Loop: Header=BB1_2 Depth=1
	leaq	8(%rdi), %rbx
	movslq	-4(%rdx), %rcx
	movq	%rcx, %rdx
	sarq	$8, %rdx
	cmpq	$255, %rcx
	leaq	8(%rdi,%rdx,8), %rdi
	cmovleq	%rbx, %rdi
	jmp	LBB1_14
	.align	4, 0x90
LBB1_4:                                 ##   in Loop: Header=BB1_2 Depth=1
	movzwl	-2(%rdx), %ebx
	testq	%rbx, %rbx
	je	LBB1_5
## BB#24:                               ##   in Loop: Header=BB1_2 Depth=1
	addq	$8, %rdi
	cmpl	$256, %ebx              ## imm = 0x100
	jb	LBB1_14
## BB#25:                               ##   in Loop: Header=BB1_2 Depth=1
	movzwl	(%rdx), %edx
	addq	$-256, %rbx
	subq	%rdx, %rbx
	leaq	(%rdi,%rbx,8), %rdi
	jmp	LBB1_14
LBB1_5:                                 ##   in Loop: Header=BB1_2 Depth=1
	cmpq	%r12, %rdx
	je	LBB1_9
## BB#6:                                ##   in Loop: Header=BB1_2 Depth=1
	cmpq	%r11, %rdx
	je	LBB1_9
## BB#7:                                ##   in Loop: Header=BB1_2 Depth=1
	cmpq	%r8, %rdx
	je	LBB1_9
## BB#8:                                ##   in Loop: Header=BB1_2 Depth=1
	cmpq	%r9, %rdx
	je	LBB1_9
## BB#10:                               ##   in Loop: Header=BB1_2 Depth=1
	cmpq	%r10, %rdx
	jne	LBB1_12
## BB#11:                               ##   in Loop: Header=BB1_2 Depth=1
	movq	8(%rdi), %rdx
	addq	$7, %rdx
	andq	$-8, %rdx
	leaq	16(%rdi,%rdx), %rdi
	jmp	LBB1_14
LBB1_9:                                 ##   in Loop: Header=BB1_2 Depth=1
	addq	$16, %rdi
	jmp	LBB1_14
LBB1_12:                                ##   in Loop: Header=BB1_2 Depth=1
	leaq	2(%rax), %rbx
	cmpq	%rbx, %rdx
	jne	LBB1_13
## BB#16:                               ##   in Loop: Header=BB1_2 Depth=1
	movq	16(%rdi), %rbx
	leaq	24(%rdi), %rdx
	testq	%rbx, %rbx
	je	LBB1_17
## BB#18:                               ##   in Loop: Header=BB1_2 Depth=1
	movq	8(%rdi), %rdi
	cmpq	%r8, %rbx
	je	LBB1_20
## BB#19:                               ##   in Loop: Header=BB1_2 Depth=1
	cmpq	%r12, %rbx
	je	LBB1_20
## BB#21:                               ##   in Loop: Header=BB1_2 Depth=1
	cmpq	%r11, %rbx
	jne	LBB1_23
## BB#22:                               ##   in Loop: Header=BB1_2 Depth=1
	addq	$7, %rdi
	andq	$-8, %rdi
	addq	%rdi, %rdx
	movq	%rdx, %rdi
	jmp	LBB1_14
	.align	4, 0x90
LBB1_13:                                ##   in Loop: Header=BB1_2 Depth=1
	addq	$8, %rdi
LBB1_14:                                ## %.backedge
                                        ##   in Loop: Header=BB1_2 Depth=1
	cmpq	%rsi, %rdi
	jb	LBB1_2
LBB1_15:                                ## %._crit_edge
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
	


.subsections_via_symbols
