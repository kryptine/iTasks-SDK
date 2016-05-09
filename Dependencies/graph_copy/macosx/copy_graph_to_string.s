	.section	__TEXT,__text,regular,pure_instructions
	
	.globl	_copy_graph_to_string
	.align	4, 0x90
_copy_graph_to_string:                  ## @copy_graph_to_string
	
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
	subq	$176, %rsp
Ltmp3:
	
Ltmp4:
	
Ltmp5:
	
Ltmp6:
	
Ltmp7:
	
	movq	%rdx, -64(%rbp)         ## 8-byte Spill
	movq	%rsi, -72(%rbp)         ## 8-byte Spill
	leaq	16(%rsi), %r12
	xorl	%eax, %eax
	movq	%rax, -56(%rbp)         ## 8-byte Spill
	cmpq	%rdx, %r12
	ja	LBB0_350
## BB#1:
	movq	__STRING__@GOTPCREL(%rip), %rax
	addq	$2, %rax
	movq	%rax, -80(%rbp)         ## 8-byte Spill
	movd	%rax, %xmm0
	movups	%xmm0, (%rsi)
	xorl	%eax, %eax
	movq	%rax, -56(%rbp)         ## 8-byte Spill
	cmpq	%rdx, %r12
	jae	LBB0_350
## BB#2:                                ## %.lr.ph122.preheader
	xorl	%eax, %eax
	movq	%rax, -56(%rbp)         ## 8-byte Spill
	movq	REAL@GOTPCREL(%rip), %rax
	addq	$2, %rax
	movq	%rax, -88(%rbp)         ## 8-byte Spill
	movq	BOOL@GOTPCREL(%rip), %rax
	addq	$2, %rax
	movq	%rax, -96(%rbp)         ## 8-byte Spill
	movq	dINT@GOTPCREL(%rip), %rax
	addq	$2, %rax
	movq	%rax, -104(%rbp)        ## 8-byte Spill
	movq	CHAR@GOTPCREL(%rip), %rax
	addq	$2, %rax
	movq	%rax, -112(%rbp)        ## 8-byte Spill
	movq	%rdx, -48(%rbp)         ## 8-byte Spill
	movq	%rdx, %r13
	jmp	LBB0_3
LBB0_57:                                ##   in Loop: Header=BB0_3 Depth=1
	leaq	(%r9,%r15,8), %r9
	cmpq	-48(%rbp), %r9          ## 8-byte Folded Reload
	ja	LBB0_350
## BB#58:                               ##   in Loop: Header=BB0_3 Depth=1
	testq	%r15, %r15
	jle	LBB0_346
## BB#59:                               ## %.lr.ph.i.83.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rdx, %r10
	movq	%rsi, %r8
	cmpq	$4, %r15
	movl	$0, %esi
	jb	LBB0_70
## BB#60:                               ## %min.iters.checked491
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r15, %rcx
	andq	$-4, %rcx
	movl	$0, %esi
	je	LBB0_70
## BB#61:                               ## %vector.memcheck510
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	24(%r12), %rax
	leaq	16(%rdi,%r15,8), %rdx
	cmpq	%rdx, %rax
	ja	LBB0_63
## BB#62:                               ## %vector.memcheck510
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	16(%r12,%r15,8), %rax
	leaq	24(%rdi), %rdx
	cmpq	%rax, %rdx
	movl	$0, %esi
	jbe	LBB0_70
LBB0_63:                                ## %vector.body487.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-4(%r15), %rsi
	movl	%esi, %eax
	shrl	$2, %eax
	incl	%eax
	testb	$3, %al
	movl	$0, %ebx
	je	LBB0_66
## BB#64:                               ## %vector.body487.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leal	-4(%r15), %eax
	shrl	$2, %eax
	incl	%eax
	andl	$3, %eax
	negq	%rax
	xorl	%ebx, %ebx
LBB0_65:                                ## %vector.body487.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	24(%rdi,%rbx,8), %xmm0
	movups	40(%rdi,%rbx,8), %xmm1
	movups	%xmm0, 24(%r12,%rbx,8)
	movups	%xmm1, 40(%r12,%rbx,8)
	addq	$4, %rbx
	incq	%rax
	jne	LBB0_65
LBB0_66:                                ## %vector.body487.preheader.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$12, %rsi
	jb	LBB0_69
## BB#67:                               ## %vector.body487.preheader.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r15, %rax
	andq	$-4, %rax
	subq	%rbx, %rax
	leaq	136(%r12,%rbx,8), %rdx
	leaq	136(%rdi,%rbx,8), %rsi
LBB0_68:                                ## %vector.body487
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-112(%rsi), %xmm0
	movups	-96(%rsi), %xmm1
	movups	%xmm0, -112(%rdx)
	movups	%xmm1, -96(%rdx)
	movups	-80(%rsi), %xmm0
	movups	-64(%rsi), %xmm1
	movups	%xmm0, -80(%rdx)
	movups	%xmm1, -64(%rdx)
	movups	-48(%rsi), %xmm0
	movups	-32(%rsi), %xmm1
	movups	%xmm0, -48(%rdx)
	movups	%xmm1, -32(%rdx)
	movups	-16(%rsi), %xmm0
	movups	(%rsi), %xmm1
	movups	%xmm0, -16(%rdx)
	movups	%xmm1, (%rdx)
	subq	$-128, %rdx
	subq	$-128, %rsi
	addq	$-16, %rax
	jne	LBB0_68
LBB0_69:                                ## %middle.block488
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%rcx, %r15
	movq	%rcx, %rsi
	je	LBB0_90
LBB0_70:                                ## %.lr.ph.i.83.preheader713
                                        ##   in Loop: Header=BB0_3 Depth=1
	movl	%r15d, %ecx
	subl	%esi, %ecx
	leaq	-1(%r15), %rax
	subq	%rsi, %rax
	testb	$7, %cl
	je	LBB0_73
## BB#71:                               ## %.lr.ph.i.83.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	movl	%r15d, %ecx
	subl	%esi, %ecx
	andl	$7, %ecx
	negq	%rcx
LBB0_72:                                ## %.lr.ph.i.83.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	24(%rdi,%rsi,8), %rdx
	movq	%rdx, 24(%r12,%rsi,8)
	incq	%rsi
	incq	%rcx
	jne	LBB0_72
LBB0_73:                                ## %.lr.ph.i.83.preheader713.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$7, %rax
	jb	LBB0_90
## BB#74:                               ## %.lr.ph.i.83.preheader713.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	subq	%rsi, %r15
	leaq	80(%r12,%rsi,8), %rax
	leaq	80(%rdi,%rsi,8), %rcx
LBB0_75:                                ## %.lr.ph.i.83
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-56(%rcx), %rdx
	movq	%rdx, -56(%rax)
	movq	-48(%rcx), %rdx
	movq	%rdx, -48(%rax)
	movq	-40(%rcx), %rdx
	movq	%rdx, -40(%rax)
	movq	-32(%rcx), %rdx
	movq	%rdx, -32(%rax)
	movq	-24(%rcx), %rdx
	movq	%rdx, -24(%rax)
	movq	-16(%rcx), %rdx
	movq	%rdx, -16(%rax)
	movq	-8(%rcx), %rdx
	movq	%rdx, -8(%rax)
	movq	(%rcx), %rdx
	movq	%rdx, (%rax)
	addq	$64, %rax
	addq	$64, %rcx
	addq	$-8, %r15
	jne	LBB0_75
	jmp	LBB0_90
LBB0_256:                               ## %.lr.ph.i.59.preheader.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	subq	%rcx, %rbx
	subq	%r10, %rbx
	leaq	64(%r12,%rcx,8), %rax
	addq	%r10, %rcx
	leaq	(%r15,%rcx,8), %rcx
	movq	-72(%rbp), %rsi         ## 8-byte Reload
	.align	4, 0x90
LBB0_257:                               ## %.lr.ph.i.59
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-8(%rcx), %rdx
	movq	%rdx, -56(%rax)
	movq	(%rcx), %rdx
	movq	%rdx, -48(%rax)
	movq	8(%rcx), %rdx
	movq	%rdx, -40(%rax)
	movq	16(%rcx), %rdx
	movq	%rdx, -32(%rax)
	movq	24(%rcx), %rdx
	movq	%rdx, -24(%rax)
	movq	32(%rcx), %rdx
	movq	%rdx, -16(%rax)
	movq	40(%rcx), %rdx
	movq	%rdx, -8(%rax)
	movq	48(%rcx), %rdx
	movq	%rdx, (%rax)
	addq	$64, %rax
	addq	$64, %rcx
	addq	$-8, %rbx
	jne	LBB0_257
LBB0_258:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	%r8, %rdx
LBB0_259:                               ## %copy.exit60.thread
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpl	$2, %r10d
	jb	LBB0_180
## BB#260:                              ##   in Loop: Header=BB0_3 Depth=1
	movl	$1, %eax
	subq	%r10, %rax
	leaq	(%r13,%rax,8), %r14
	cmpq	-48(%rbp), %r14         ## 8-byte Folded Reload
	jae	LBB0_262
## BB#261:                              ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%r9, %r14
	movq	%r14, -48(%rbp)         ## 8-byte Spill
	jb	LBB0_350
LBB0_262:                               ## %.lr.ph.i.55.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rdx, %r8
	movq	%rsi, %r12
	cmpq	$4, %r11
	movl	$0, %esi
	jb	LBB0_273
## BB#263:                              ## %min.iters.checked355
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r11, %rbx
	andq	$-4, %rbx
	movl	$0, %esi
	je	LBB0_273
## BB#264:                              ## %vector.memcheck372
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	(,%r10,8), %rdx
	movq	%rdx, %rax
	negq	%rax
	leaq	8(%r13,%rax), %rax
	leaq	-8(%r15,%r11,8), %rsi
	cmpq	%rsi, %rax
	ja	LBB0_266
## BB#265:                              ## %vector.memcheck372
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r11, %rax
	subq	%r10, %rax
	leaq	(%r13,%rax,8), %rax
	cmpq	%rax, %r15
	movl	$0, %esi
	jbe	LBB0_273
LBB0_266:                               ## %vector.body351.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-4(%r11), %rcx
	movl	%ecx, %eax
	shrl	$2, %eax
	incl	%eax
	testb	$3, %al
	movl	$0, %esi
	je	LBB0_269
## BB#267:                              ## %vector.body351.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r13, %rax
	subq	%rdx, %rax
	addq	$24, %rax
	leal	-4(%r11), %edx
	shrl	$2, %edx
	incl	%edx
	andl	$3, %edx
	negq	%rdx
	xorl	%esi, %esi
LBB0_268:                               ## %vector.body351.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	(%r15,%rsi,8), %xmm0
	movups	16(%r15,%rsi,8), %xmm1
	movups	%xmm0, -16(%rax,%rsi,8)
	movups	%xmm1, (%rax,%rsi,8)
	addq	$4, %rsi
	incq	%rdx
	jne	LBB0_268
LBB0_269:                               ## %vector.body351.preheader.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$12, %rcx
	jb	LBB0_272
## BB#270:                              ## %vector.body351.preheader.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r11, %rax
	andq	$-4, %rax
	subq	%rsi, %rax
	leaq	112(%r15,%rsi,8), %rdx
	subq	%r10, %rsi
	leaq	120(%r13,%rsi,8), %rsi
LBB0_271:                               ## %vector.body351
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-112(%rdx), %xmm0
	movups	-96(%rdx), %xmm1
	movups	%xmm0, -112(%rsi)
	movups	%xmm1, -96(%rsi)
	movups	-80(%rdx), %xmm0
	movups	-64(%rdx), %xmm1
	movups	%xmm0, -80(%rsi)
	movups	%xmm1, -64(%rsi)
	movups	-48(%rdx), %xmm0
	movups	-32(%rdx), %xmm1
	movups	%xmm0, -48(%rsi)
	movups	%xmm1, -32(%rsi)
	movups	-16(%rdx), %xmm0
	movups	(%rdx), %xmm1
	movups	%xmm0, -16(%rsi)
	movups	%xmm1, (%rsi)
	subq	$-128, %rsi
	subq	$-128, %rdx
	addq	$-16, %rax
	jne	LBB0_271
LBB0_272:                               ## %middle.block352
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%rbx, %r11
	movq	%rbx, %rsi
	je	LBB0_279
LBB0_273:                               ## %.lr.ph.i.55.preheader710
                                        ##   in Loop: Header=BB0_3 Depth=1
	movl	%r11d, %eax
	subl	%esi, %eax
	leaq	-1(%r11), %rbx
	subq	%rsi, %rbx
	testb	$7, %al
	je	LBB0_276
## BB#274:                              ## %.lr.ph.i.55.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	(,%r10,8), %rdx
	movq	%r13, %rax
	subq	%rdx, %rax
	addq	$8, %rax
	movl	%r11d, %edx
	subl	%esi, %edx
	andl	$7, %edx
	negq	%rdx
	.align	4, 0x90
LBB0_275:                               ## %.lr.ph.i.55.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	(%r15,%rsi,8), %rcx
	movq	%rcx, (%rax,%rsi,8)
	incq	%rsi
	incq	%rdx
	jne	LBB0_275
LBB0_276:                               ## %.lr.ph.i.55.preheader710.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$7, %rbx
	jb	LBB0_279
## BB#277:                              ## %.lr.ph.i.55.preheader710.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	subq	%rsi, %r11
	leaq	56(%r15,%rsi,8), %rax
	subq	%r10, %rsi
	leaq	64(%r13,%rsi,8), %rdx
	.align	4, 0x90
LBB0_278:                               ## %.lr.ph.i.55
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-56(%rax), %rcx
	movq	%rcx, -56(%rdx)
	movq	-48(%rax), %rcx
	movq	%rcx, -48(%rdx)
	movq	-40(%rax), %rcx
	movq	%rcx, -40(%rdx)
	movq	-32(%rax), %rcx
	movq	%rcx, -32(%rdx)
	movq	-24(%rax), %rcx
	movq	%rcx, -24(%rdx)
	movq	-16(%rax), %rcx
	movq	%rcx, -16(%rdx)
	movq	-8(%rax), %rcx
	movq	%rcx, -8(%rdx)
	movq	(%rax), %rcx
	movq	%rcx, (%rdx)
	addq	$64, %rdx
	addq	$64, %rax
	addq	$-8, %r11
	jne	LBB0_278
LBB0_279:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	%r14, %r13
	movq	%r12, %rsi
	movq	%r8, %rdx
	jmp	LBB0_180
LBB0_97:                                ##   in Loop: Header=BB0_3 Depth=1
	movzwl	(%rax), %r14d
	movzwl	-2(%rax), %r11d
	leaq	-256(%r11), %rbx
	testq	%r14, %r14
	je	LBB0_98
## BB#120:                              ##   in Loop: Header=BB0_3 Depth=1
	subq	%r14, %rbx
	jne	LBB0_139
## BB#121:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	%r14, %rbx
	imulq	%r15, %rbx
	leaq	(,%rbx,8), %rax
	movq	%r13, %r10
	subq	%rax, %r10
	cmpq	-48(%rbp), %r10         ## 8-byte Folded Reload
	jae	LBB0_123
## BB#122:                              ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%r9, %r10
	movq	%r10, -48(%rbp)         ## 8-byte Spill
	jb	LBB0_350
LBB0_123:                               ## %.preheader98
                                        ##   in Loop: Header=BB0_3 Depth=1
	testq	%rbx, %rbx
	jle	LBB0_124
## BB#125:                              ## %.lr.ph108.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	imulq	%r15, %r14
	cmpq	$2, %r14
	movl	$1, %eax
	cmovlq	%r14, %rax
	movl	$1, %ecx
	subq	%rax, %rcx
	addq	%r14, %rcx
	cmpq	$4, %rcx
	jb	LBB0_136
## BB#126:                              ## %min.iters.checked603
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rcx, %r11
	andq	$-4, %r11
	movq	%rcx, %r15
	andq	$-4, %r15
	je	LBB0_136
## BB#127:                              ## %vector.body598.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rsi, %r12
	cmpq	$2, %r14
	movl	$1, %eax
	cmovlq	%r14, %rax
	movl	$1, %edx
	subq	%rax, %rdx
	leaq	-4(%rdx,%r14), %rdx
	movq	%rdx, %rax
	shrq	$2, %rax
	btq	$2, %rdx
	movl	$0, %r8d
	jb	LBB0_129
## BB#128:                              ## %vector.body598.prol
                                        ##   in Loop: Header=BB0_3 Depth=1
	movups	8(%rdi,%rbx,8), %xmm0
	movups	-8(%rdi,%rbx,8), %xmm1
	movups	%xmm0, -16(%r10,%rbx,8)
	movups	%xmm1, -32(%r10,%rbx,8)
	movl	$4, %r8d
LBB0_129:                               ## %vector.body598.preheader.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	testq	%rax, %rax
	je	LBB0_132
## BB#130:                              ## %vector.body598.preheader.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r14, %rax
	notq	%rax
	cmpq	$-3, %rax
	movq	$-2, %rdx
	cmovleq	%rdx, %rax
	leaq	2(%rax,%r14), %rax
	andq	$-4, %rax
	subq	%r8, %rax
	movq	%r14, %rdx
	subq	%r8, %rdx
	leaq	8(%rdi,%rdx,8), %rdx
	shlq	$3, %r8
	movq	%r13, %rsi
	subq	%r8, %rsi
	addq	$-16, %rsi
LBB0_131:                               ## %vector.body598
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%rdx), %xmm0
	movups	(%rdx), %xmm1
	movups	%xmm1, (%rsi)
	movups	%xmm0, -16(%rsi)
	movups	-48(%rdx), %xmm0
	movups	-32(%rdx), %xmm1
	movups	%xmm1, -32(%rsi)
	movups	%xmm0, -48(%rsi)
	addq	$-64, %rdx
	addq	$-64, %rsi
	addq	$-8, %rax
	jne	LBB0_131
LBB0_132:                               ## %middle.block599
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%r15, %rcx
	jne	LBB0_135
## BB#133:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	%r10, %r13
	movq	%r12, %rsi
	movq	-64(%rbp), %rdx         ## 8-byte Reload
	jmp	LBB0_346
LBB0_331:                               ## %vector.body199.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r12, -128(%rbp)        ## 8-byte Spill
	movq	%r10, -136(%rbp)        ## 8-byte Spill
	movzbl	%r11b, %eax
	movq	%r8, %rdx
	subq	%rax, %rdx
	cmpq	$-2, %rdx
	movq	$-1, %rsi
	cmovleq	%rsi, %rdx
	leaq	-1(%rdx,%rax), %rdx
	subq	%r15, %rdx
	addq	$-4, %rdx
	movl	%edx, %eax
	shrl	$2, %eax
	incl	%eax
	testb	$3, %al
	movl	$0, %ebx
	je	LBB0_334
## BB#332:                              ## %vector.body199.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-24(%r13), %rsi
	movzbl	%r11b, %eax
	movq	%rdx, %rbx
	movq	%rax, %rdx
	subq	%r15, %rdx
	leaq	-16(%rdi,%rdx,8), %r12
	movq	%r8, %rdx
	subq	%rax, %rdx
	movl	$-1, %eax
	cmovsl	%eax, %edx
	movzbl	%r11b, %eax
	leal	-5(%rdx,%rax), %r10d
	movq	%rbx, %rdx
	subl	%r15d, %r10d
	shrl	$2, %r10d
	incl	%r10d
	andl	$3, %r10d
	negq	%r10
	xorl	%ebx, %ebx
LBB0_333:                               ## %vector.body199.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%r12), %xmm0
	movups	(%r12), %xmm1
	movups	%xmm1, (%rsi)
	movups	%xmm0, -16(%rsi)
	addq	$4, %rbx
	addq	$-32, %rsi
	addq	$-32, %r12
	incq	%r10
	jne	LBB0_333
LBB0_334:                               ## %vector.body199.preheader.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$12, %rdx
	jb	LBB0_337
## BB#335:                              ## %vector.body199.preheader.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	movzbl	%r11b, %edx
	subq	%rdx, %r8
	cmpq	$-2, %r8
	movq	$-1, %rax
	cmovleq	%rax, %r8
	leaq	-1(%r8,%rdx), %rsi
	subq	%r15, %rsi
	andq	$-4, %rsi
	subq	%rbx, %rsi
	leaq	(,%rbx,8), %r10
	movq	%r13, %rax
	subq	%r10, %rax
	addq	$-24, %rax
	addq	%r15, %rbx
	subq	%rbx, %rdx
	leaq	-16(%rdi,%rdx,8), %rdx
LBB0_336:                               ## %vector.body199
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%rdx), %xmm0
	movups	(%rdx), %xmm1
	movups	%xmm1, (%rax)
	movups	%xmm0, -16(%rax)
	movups	-48(%rdx), %xmm0
	movups	-32(%rdx), %xmm1
	movups	%xmm1, -32(%rax)
	movups	%xmm0, -48(%rax)
	movups	-80(%rdx), %xmm0
	movups	-64(%rdx), %xmm1
	movups	%xmm1, -64(%rax)
	movups	%xmm0, -80(%rax)
	movups	-112(%rdx), %xmm0
	movups	-96(%rdx), %xmm1
	movups	%xmm1, -96(%rax)
	movups	%xmm0, -112(%rax)
	addq	$-128, %rax
	addq	$-128, %rdx
	addq	$-16, %rsi
	jne	LBB0_336
LBB0_337:                               ## %middle.block200
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	-120(%rbp), %rax        ## 8-byte Reload
	cmpq	%rax, -128(%rbp)        ## 8-byte Folded Reload
	jne	LBB0_339
## BB#338:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	-136(%rbp), %r13        ## 8-byte Reload
	movq	-72(%rbp), %rsi         ## 8-byte Reload
	movq	-64(%rbp), %rdx         ## 8-byte Reload
	jmp	LBB0_180
LBB0_41:                                ##   in Loop: Header=BB0_3 Depth=1
	movq	%rcx, %r13
	jmp	LBB0_346
LBB0_339:                               ##   in Loop: Header=BB0_3 Depth=1
	subq	%rax, %rcx
	movq	-72(%rbp), %rsi         ## 8-byte Reload
	movq	-136(%rbp), %r10        ## 8-byte Reload
LBB0_340:                               ## %.lr.ph112.preheader704
                                        ##   in Loop: Header=BB0_3 Depth=1
	incq	%rcx
	movzbl	%r14b, %eax
	subq	%rax, %r15
	leaq	(%r13,%r15,8), %rax
	.align	4, 0x90
LBB0_341:                               ## %.lr.ph112
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	8(%rdi,%rcx,8), %rdx
	movq	%rdx, (%rax,%rcx,8)
	decq	%rcx
	testq	%rcx, %rcx
	jg	LBB0_341
## BB#342:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	%r10, %r13
	movq	-64(%rbp), %rdx         ## 8-byte Reload
	jmp	LBB0_180
LBB0_98:                                ##   in Loop: Header=BB0_3 Depth=1
	imulq	%r15, %rbx
	leaq	(%r9,%rbx,8), %r9
	cmpq	-48(%rbp), %r9          ## 8-byte Folded Reload
	ja	LBB0_350
## BB#99:                               ##   in Loop: Header=BB0_3 Depth=1
	testq	%rbx, %rbx
	jle	LBB0_346
## BB#100:                              ## %.lr.ph.i.75.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rdx, %r10
	movq	%rsi, %r14
	cmpq	$4, %rbx
	movl	$0, %esi
	jb	LBB0_112
## BB#101:                              ## %min.iters.checked565
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rbx, %rcx
	andq	$-4, %rcx
	movl	$0, %esi
	je	LBB0_112
## BB#102:                              ## %vector.memcheck584
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	24(%r12), %rax
	leaq	16(%rdi,%rbx,8), %rdx
	cmpq	%rdx, %rax
	ja	LBB0_104
## BB#103:                              ## %vector.memcheck584
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	16(%r12,%rbx,8), %rax
	leaq	24(%rdi), %rdx
	cmpq	%rax, %rdx
	movl	$0, %esi
	jbe	LBB0_112
LBB0_104:                               ## %vector.body561.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-4(%rbx), %rdx
	movl	%edx, %eax
	shrl	$2, %eax
	incl	%eax
	testb	$3, %al
	movl	$0, %esi
	je	LBB0_107
## BB#105:                              ## %vector.body561.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leal	-256(%r11), %eax
	imull	%r15d, %eax
	addl	$-4, %eax
	shrl	$2, %eax
	incl	%eax
	andl	$3, %eax
	negq	%rax
	xorl	%esi, %esi
LBB0_106:                               ## %vector.body561.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	24(%rdi,%rsi,8), %xmm0
	movups	40(%rdi,%rsi,8), %xmm1
	movups	%xmm0, 24(%r12,%rsi,8)
	movups	%xmm1, 40(%r12,%rsi,8)
	addq	$4, %rsi
	incq	%rax
	jne	LBB0_106
LBB0_107:                               ## %vector.body561.preheader.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$12, %rdx
	jb	LBB0_110
## BB#108:                              ## %vector.body561.preheader.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rbx, %rax
	andq	$-4, %rax
	subq	%rsi, %rax
	leaq	136(%r12,%rsi,8), %rdx
	leaq	136(%rdi,%rsi,8), %rsi
LBB0_109:                               ## %vector.body561
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-112(%rsi), %xmm0
	movups	-96(%rsi), %xmm1
	movups	%xmm0, -112(%rdx)
	movups	%xmm1, -96(%rdx)
	movups	-80(%rsi), %xmm0
	movups	-64(%rsi), %xmm1
	movups	%xmm0, -80(%rdx)
	movups	%xmm1, -64(%rdx)
	movups	-48(%rsi), %xmm0
	movups	-32(%rsi), %xmm1
	movups	%xmm0, -48(%rdx)
	movups	%xmm1, -32(%rdx)
	movups	-16(%rsi), %xmm0
	movups	(%rsi), %xmm1
	movups	%xmm0, -16(%rdx)
	movups	%xmm1, (%rdx)
	subq	$-128, %rdx
	subq	$-128, %rsi
	addq	$-16, %rax
	jne	LBB0_109
LBB0_110:                               ## %middle.block562
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%rcx, %rbx
	movq	%rcx, %rsi
	je	LBB0_111
LBB0_112:                               ## %.lr.ph.i.75.preheader717
                                        ##   in Loop: Header=BB0_3 Depth=1
	movl	%ebx, %eax
	subl	%esi, %eax
	leaq	-1(%rbx), %rcx
	subq	%rsi, %rcx
	testb	$7, %al
	je	LBB0_113
## BB#114:                              ## %.lr.ph.i.75.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	addl	$-256, %r11d
	imull	%r11d, %r15d
	subl	%esi, %r15d
	andl	$7, %r15d
	negq	%r15
	movq	%r10, %rdx
LBB0_115:                               ## %.lr.ph.i.75.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	24(%rdi,%rsi,8), %rax
	movq	%rax, 24(%r12,%rsi,8)
	incq	%rsi
	incq	%r15
	jne	LBB0_115
	jmp	LBB0_116
LBB0_139:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	%rbx, %rax
	imulq	%r15, %rax
	leaq	(%r9,%rax,8), %rax
	cmpq	-48(%rbp), %rax         ## 8-byte Folded Reload
	ja	LBB0_350
## BB#140:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	%r15, %rcx
	negq	%rcx
	imulq	%r14, %rcx
	movq	%rcx, -240(%rbp)        ## 8-byte Spill
	leaq	(%r13,%rcx,8), %rcx
	cmpq	-48(%rbp), %rcx         ## 8-byte Folded Reload
	jae	LBB0_142
## BB#141:                              ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%rax, %rcx
	movq	%rcx, -48(%rbp)         ## 8-byte Spill
	jb	LBB0_350
LBB0_142:                               ##   in Loop: Header=BB0_3 Depth=1
	testq	%r15, %r15
	jle	LBB0_143
## BB#144:                              ## %.lr.ph
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rcx, -344(%rbp)        ## 8-byte Spill
	movq	%r11, %rax
	subq	%r14, %rax
	movq	%rax, -216(%rbp)        ## 8-byte Spill
	leaq	(%r12,%rax,8), %rax
	leaq	-2048(,%r11,8), %rcx
	movq	%rcx, -192(%rbp)        ## 8-byte Spill
	leaq	(,%r14,8), %rdx
	movq	%rdx, -168(%rbp)        ## 8-byte Spill
	movq	%rcx, %r8
	subq	%rdx, %r8
	leaq	-1(%r15), %rdx
	imulq	%r8, %rdx
	leaq	-2024(%rdx,%rax), %r9
	leaq	-254(%r11), %rax
	subq	%r14, %rax
	leaq	(%r12,%rax,8), %rax
	movq	%rax, -288(%rbp)        ## 8-byte Spill
	leaq	(,%r15,8), %rax
	movl	$8, %edx
	subq	%rax, %rdx
	imulq	%r14, %rdx
	movq	%rdx, -272(%rbp)        ## 8-byte Spill
	leaq	-4(%rbx), %rax
	movq	%rax, -312(%rbp)        ## 8-byte Spill
	movl	%eax, %edx
	shrl	$2, %edx
	incl	%edx
	movq	%r15, %rax
	imulq	%r14, %rax
	shlq	$3, %rax
	movq	%r13, %r10
	subq	%rax, %r10
	movl	%r14d, %eax
	andl	$3, %eax
	movq	%r14, %rsi
	subq	%rax, %rsi
	movq	%rsi, -120(%rbp)        ## 8-byte Spill
	movq	%r11, %rax
	addq	$-257, %rax             ## imm = 0xFFFFFFFFFFFFFEFF
	subq	%r14, %rax
	movq	%rax, -224(%rbp)        ## 8-byte Spill
	movq	%rbx, %rax
	andq	$-4, %rax
	movq	%rax, -232(%rbp)        ## 8-byte Spill
	andl	$3, %edx
	movq	%rdx, -320(%rbp)        ## 8-byte Spill
	movl	%r14d, %eax
	andl	$3, %eax
	movq	%rax, -208(%rbp)        ## 8-byte Spill
	movq	%r14, %rdx
	subq	%rax, %rdx
	movq	%rdx, -304(%rbp)        ## 8-byte Spill
	leaq	-2032(%rdi,%r11,8), %rax
	movq	%rax, -256(%rbp)        ## 8-byte Spill
	leal	-260(%r11), %eax
	subl	%r14d, %eax
	shrl	$2, %eax
	incl	%eax
	andl	$3, %eax
	negq	%rax
	movq	%rax, -328(%rbp)        ## 8-byte Spill
	movq	%rbx, %rax
	movq	%rbx, -136(%rbp)        ## 8-byte Spill
	andq	$-4, %rax
	movq	%rax, -336(%rbp)        ## 8-byte Spill
	leaq	24(%r12), %rax
	movq	%rax, -264(%rbp)        ## 8-byte Spill
	leaq	72(%r12), %rdx
	movq	%rdx, -176(%rbp)        ## 8-byte Spill
	addq	$40, %r12
	leaq	24(%rdi,%r14,8), %rdx
	movq	%rdx, -296(%rbp)        ## 8-byte Spill
	leaq	24(%rdi), %r11
	movq	%r11, -280(%rbp)        ## 8-byte Spill
	leaq	16(%rdi,%r14,8), %rsi
	movq	%rsi, -248(%rbp)        ## 8-byte Spill
	leaq	80(%rdi), %rsi
	movq	%rsi, -144(%rbp)        ## 8-byte Spill
	leaq	136(%rdi,%r14,8), %rsi
	movq	%rsi, -184(%rbp)        ## 8-byte Spill
	leaq	80(%rdi,%r14,8), %rsi
	movq	%rsi, -160(%rbp)        ## 8-byte Spill
	leaq	-1(%r14), %rsi
	movq	%rsi, -200(%rbp)        ## 8-byte Spill
	movq	%rdx, -152(%rbp)        ## 8-byte Spill
	movq	%rax, %rdi
	xorl	%eax, %eax
LBB0_145:                               ## %.lr.ph.i.71.preheader
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Loop Header: Depth=2
                                        ##       Child Loop BB0_150 Depth 3
                                        ##       Child Loop BB0_154 Depth 3
                                        ##       Child Loop BB0_157 Depth 3
                                        ##       Child Loop BB0_165 Depth 3
                                        ##       Child Loop BB0_168 Depth 3
                                        ##       Child Loop BB0_172 Depth 3
                                        ##       Child Loop BB0_175 Depth 3
	movq	%rax, -128(%rbp)        ## 8-byte Spill
	imulq	%rax, %rcx
	cmpl	$4, %r14d
	movl	$0, %ebx
	jb	LBB0_152
## BB#146:                              ## %min.iters.checked667
                                        ##   in Loop: Header=BB0_145 Depth=2
	movq	-208(%rbp), %rax        ## 8-byte Reload
	movzwl	%ax, %eax
	cmpl	%eax, %r14d
	movl	$0, %ebx
	je	LBB0_152
## BB#147:                              ## %vector.memcheck687
                                        ##   in Loop: Header=BB0_145 Depth=2
	movq	%r14, %rax
	imulq	-128(%rbp), %rax        ## 8-byte Folded Reload
	movq	-240(%rbp), %rdx        ## 8-byte Reload
	leaq	(%rdx,%rax), %rdx
	movq	-248(%rbp), %rsi        ## 8-byte Reload
	leaq	(%rsi,%rcx), %rsi
	leaq	(%r13,%rdx,8), %rdx
	cmpq	%rsi, %rdx
	ja	LBB0_148
## BB#149:                              ## %vector.memcheck687
                                        ##   in Loop: Header=BB0_145 Depth=2
	movq	-280(%rbp), %rdx        ## 8-byte Reload
	leaq	(%rdx,%rcx), %rdx
	leaq	(%r13,%rax,8), %rax
	movq	-272(%rbp), %rsi        ## 8-byte Reload
	leaq	-8(%rsi,%rax), %rax
	cmpq	%rax, %rdx
	movl	$0, %eax
	movl	$0, %ebx
	jbe	LBB0_152
	jmp	LBB0_150
LBB0_148:                               ##   in Loop: Header=BB0_145 Depth=2
	xorl	%eax, %eax
LBB0_150:                               ## %vector.body663
                                        ##   Parent Loop BB0_3 Depth=1
                                        ##     Parent Loop BB0_145 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	movups	(%r11,%rax,8), %xmm0
	movups	16(%r11,%rax,8), %xmm1
	movups	%xmm0, (%r10,%rax,8)
	movups	%xmm1, 16(%r10,%rax,8)
	addq	$4, %rax
	cmpq	%rax, -120(%rbp)        ## 8-byte Folded Reload
	jne	LBB0_150
## BB#151:                              ## %middle.block664
                                        ##   in Loop: Header=BB0_145 Depth=2
	movq	-208(%rbp), %rax        ## 8-byte Reload
	testw	%ax, %ax
	movq	-304(%rbp), %rbx        ## 8-byte Reload
	je	LBB0_158
LBB0_152:                               ## %.lr.ph.i.71.preheader702
                                        ##   in Loop: Header=BB0_145 Depth=2
	movl	%r14d, %edx
	subl	%ebx, %edx
	movq	-200(%rbp), %rax        ## 8-byte Reload
	subq	%rbx, %rax
	testb	$7, %dl
	je	LBB0_155
## BB#153:                              ## %.lr.ph.i.71.prol.preheader
                                        ##   in Loop: Header=BB0_145 Depth=2
	movl	%r14d, %edx
	subl	%ebx, %edx
	andl	$7, %edx
	negq	%rdx
	.align	4, 0x90
LBB0_154:                               ## %.lr.ph.i.71.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ##     Parent Loop BB0_145 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	movq	(%r11,%rbx,8), %rsi
	movq	%rsi, (%r10,%rbx,8)
	incq	%rbx
	incq	%rdx
	jne	LBB0_154
LBB0_155:                               ## %.lr.ph.i.71.preheader702.split
                                        ##   in Loop: Header=BB0_145 Depth=2
	cmpq	$7, %rax
	jb	LBB0_158
## BB#156:                              ## %.lr.ph.i.71.preheader702.split.split
                                        ##   in Loop: Header=BB0_145 Depth=2
	movq	-144(%rbp), %rax        ## 8-byte Reload
	leaq	(%rax,%rbx,8), %rax
	.align	4, 0x90
LBB0_157:                               ## %.lr.ph.i.71
                                        ##   Parent Loop BB0_3 Depth=1
                                        ##     Parent Loop BB0_145 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	movq	-56(%rax), %rdx
	movq	%rdx, (%r10,%rbx,8)
	movq	-48(%rax), %rdx
	movq	%rdx, 8(%r10,%rbx,8)
	movq	-40(%rax), %rdx
	movq	%rdx, 16(%r10,%rbx,8)
	movq	-32(%rax), %rdx
	movq	%rdx, 24(%r10,%rbx,8)
	movq	-24(%rax), %rdx
	movq	%rdx, 32(%r10,%rbx,8)
	movq	-16(%rax), %rdx
	movq	%rdx, 40(%r10,%rbx,8)
	movq	-8(%rax), %rdx
	movq	%rdx, 48(%r10,%rbx,8)
	movq	(%rax), %rdx
	movq	%rdx, 56(%r10,%rbx,8)
	addq	$8, %rbx
	addq	$64, %rax
	cmpq	%rbx, %r14
	jne	LBB0_157
LBB0_158:                               ## %copy.exit72
                                        ##   in Loop: Header=BB0_145 Depth=2
	cmpq	$0, -136(%rbp)          ## 8-byte Folded Reload
	jle	LBB0_176
## BB#159:                              ## %.lr.ph.i.67.preheader
                                        ##   in Loop: Header=BB0_145 Depth=2
	cmpq	$4, -136(%rbp)          ## 8-byte Folded Reload
	movl	$0, %esi
	jb	LBB0_170
## BB#160:                              ## %min.iters.checked628
                                        ##   in Loop: Header=BB0_145 Depth=2
	cmpq	$0, -232(%rbp)          ## 8-byte Folded Reload
	movl	$0, %esi
	je	LBB0_170
## BB#161:                              ## %vector.memcheck649
                                        ##   in Loop: Header=BB0_145 Depth=2
	movq	%r8, %rax
	imulq	-128(%rbp), %rax        ## 8-byte Folded Reload
	movq	-264(%rbp), %rdx        ## 8-byte Reload
	leaq	(%rdx,%rax), %rdx
	movq	-256(%rbp), %rsi        ## 8-byte Reload
	leaq	(%rsi,%rcx), %rsi
	cmpq	%rsi, %rdx
	ja	LBB0_163
## BB#162:                              ## %vector.memcheck649
                                        ##   in Loop: Header=BB0_145 Depth=2
	addq	-288(%rbp), %rax        ## 8-byte Folded Reload
	addq	-296(%rbp), %rcx        ## 8-byte Folded Reload
	cmpq	%rax, %rcx
	movl	$0, %esi
	jbe	LBB0_170
LBB0_163:                               ## %vector.body624.preheader
                                        ##   in Loop: Header=BB0_145 Depth=2
	cmpq	$0, -320(%rbp)          ## 8-byte Folded Reload
	movl	$0, %edx
	je	LBB0_166
## BB#164:                              ## %vector.body624.prol.preheader
                                        ##   in Loop: Header=BB0_145 Depth=2
	movq	-168(%rbp), %rax        ## 8-byte Reload
	leaq	(%r11,%rax), %rax
	movq	-328(%rbp), %rcx        ## 8-byte Reload
	xorl	%edx, %edx
LBB0_165:                               ## %vector.body624.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ##     Parent Loop BB0_145 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	movups	(%rax,%rdx,8), %xmm0
	movups	16(%rax,%rdx,8), %xmm1
	movups	%xmm0, (%rdi,%rdx,8)
	movups	%xmm1, 16(%rdi,%rdx,8)
	addq	$4, %rdx
	incq	%rcx
	jne	LBB0_165
LBB0_166:                               ## %vector.body624.preheader.split
                                        ##   in Loop: Header=BB0_145 Depth=2
	cmpq	$12, -312(%rbp)         ## 8-byte Folded Reload
	jb	LBB0_169
## BB#167:                              ## %vector.body624.preheader.split.split
                                        ##   in Loop: Header=BB0_145 Depth=2
	movq	-336(%rbp), %rax        ## 8-byte Reload
	subq	%rdx, %rax
	movq	-176(%rbp), %rcx        ## 8-byte Reload
	leaq	(%rcx,%rdx,8), %rcx
	movq	-184(%rbp), %rsi        ## 8-byte Reload
	leaq	(%rsi,%rdx,8), %rdx
LBB0_168:                               ## %vector.body624
                                        ##   Parent Loop BB0_3 Depth=1
                                        ##     Parent Loop BB0_145 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	movups	-112(%rdx), %xmm0
	movups	-96(%rdx), %xmm1
	movups	%xmm0, -48(%rcx)
	movups	%xmm1, -32(%rcx)
	movups	-80(%rdx), %xmm0
	movups	-64(%rdx), %xmm1
	movups	%xmm0, -16(%rcx)
	movups	%xmm1, (%rcx)
	movups	-48(%rdx), %xmm0
	movups	-32(%rdx), %xmm1
	movups	%xmm0, 16(%rcx)
	movups	%xmm1, 32(%rcx)
	movups	-16(%rdx), %xmm0
	movups	(%rdx), %xmm1
	movups	%xmm0, 48(%rcx)
	movups	%xmm1, 64(%rcx)
	subq	$-128, %rcx
	subq	$-128, %rdx
	addq	$-16, %rax
	jne	LBB0_168
LBB0_169:                               ## %middle.block625
                                        ##   in Loop: Header=BB0_145 Depth=2
	movq	-232(%rbp), %rax        ## 8-byte Reload
	cmpq	%rax, -136(%rbp)        ## 8-byte Folded Reload
	movq	%rax, %rsi
	je	LBB0_176
LBB0_170:                               ## %.lr.ph.i.67.preheader701
                                        ##   in Loop: Header=BB0_145 Depth=2
	movq	-216(%rbp), %rdx        ## 8-byte Reload
	movl	%edx, %eax
	subl	%esi, %eax
	movq	-224(%rbp), %rcx        ## 8-byte Reload
	subq	%rsi, %rcx
	testb	$7, %al
	je	LBB0_173
## BB#171:                              ## %.lr.ph.i.67.prol.preheader
                                        ##   in Loop: Header=BB0_145 Depth=2
	movq	-152(%rbp), %rax        ## 8-byte Reload
	leaq	(%rax,%rsi,8), %rax
	subl	%esi, %edx
	andl	$7, %edx
	negq	%rdx
	.align	4, 0x90
LBB0_172:                               ## %.lr.ph.i.67.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ##     Parent Loop BB0_145 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	movq	(%rax), %rbx
	movq	%rbx, (%rdi,%rsi,8)
	incq	%rsi
	addq	$8, %rax
	incq	%rdx
	jne	LBB0_172
LBB0_173:                               ## %.lr.ph.i.67.preheader701.split
                                        ##   in Loop: Header=BB0_145 Depth=2
	cmpq	$7, %rcx
	jb	LBB0_176
## BB#174:                              ## %.lr.ph.i.67.preheader701.split.split
                                        ##   in Loop: Header=BB0_145 Depth=2
	movq	-136(%rbp), %rax        ## 8-byte Reload
	subq	%rsi, %rax
	leaq	(%r12,%rsi,8), %rcx
	movq	-160(%rbp), %rdx        ## 8-byte Reload
	leaq	(%rdx,%rsi,8), %rdx
	.align	4, 0x90
LBB0_175:                               ## %.lr.ph.i.67
                                        ##   Parent Loop BB0_3 Depth=1
                                        ##     Parent Loop BB0_145 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	movq	-56(%rdx), %rsi
	movq	%rsi, -16(%rcx)
	movq	-48(%rdx), %rsi
	movq	%rsi, -8(%rcx)
	movq	-40(%rdx), %rsi
	movq	%rsi, (%rcx)
	movq	-32(%rdx), %rsi
	movq	%rsi, 8(%rcx)
	movq	-24(%rdx), %rsi
	movq	%rsi, 16(%rcx)
	movq	-16(%rdx), %rsi
	movq	%rsi, 24(%rcx)
	movq	-8(%rdx), %rsi
	movq	%rsi, 32(%rcx)
	movq	(%rdx), %rsi
	movq	%rsi, 40(%rcx)
	addq	$64, %rcx
	addq	$64, %rdx
	addq	$-8, %rax
	jne	LBB0_175
LBB0_176:                               ## %copy.exit68
                                        ##   in Loop: Header=BB0_145 Depth=2
	movq	-128(%rbp), %rax        ## 8-byte Reload
	incq	%rax
	movq	-192(%rbp), %rcx        ## 8-byte Reload
	addq	%rcx, %r11
	addq	-168(%rbp), %r10        ## 8-byte Folded Reload
	addq	%rcx, -144(%rbp)        ## 8-byte Folded Spill
	addq	%r8, %rdi
	addq	%r8, -176(%rbp)         ## 8-byte Folded Spill
	addq	%rcx, -184(%rbp)        ## 8-byte Folded Spill
	addq	%rcx, -152(%rbp)        ## 8-byte Folded Spill
	addq	%r8, %r12
	addq	%rcx, -160(%rbp)        ## 8-byte Folded Spill
	cmpq	%r15, %rax
	jne	LBB0_145
## BB#177:                              ## %.loopexit.loopexit150
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	-344(%rbp), %r13        ## 8-byte Reload
	movq	-72(%rbp), %rsi         ## 8-byte Reload
	movq	-64(%rbp), %rdx         ## 8-byte Reload
	jmp	LBB0_346
LBB0_51:                                ##   in Loop: Header=BB0_3 Depth=1
	subq	%r11, %r15
	movq	%r12, %rsi
	movq	-64(%rbp), %rdx         ## 8-byte Reload
LBB0_52:                                ## %.lr.ph109.preheader712
                                        ##   in Loop: Header=BB0_3 Depth=1
	subq	%r10, %r13
	addq	$-8, %r13
	.align	4, 0x90
LBB0_53:                                ## %.lr.ph109
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	16(%rdi,%r15,8), %rax
	movq	%rax, (%r13,%r15,8)
	cmpq	$1, %r15
	leaq	-1(%r15), %r15
	jg	LBB0_53
## BB#54:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	%rcx, %r13
	jmp	LBB0_346
LBB0_124:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	%r10, %r13
	jmp	LBB0_346
LBB0_143:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	%rcx, %r13
	jmp	LBB0_346
LBB0_113:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	%r10, %rdx
LBB0_116:                               ## %.lr.ph.i.75.preheader717.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$7, %rcx
	jae	LBB0_118
## BB#117:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	%r14, %rsi
	jmp	LBB0_346
LBB0_118:                               ## %.lr.ph.i.75.preheader717.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rdx, %r10
	subq	%rsi, %rbx
	leaq	80(%r12,%rsi,8), %rax
	leaq	80(%rdi,%rsi,8), %rcx
LBB0_119:                               ## %.lr.ph.i.75
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-56(%rcx), %rdx
	movq	%rdx, -56(%rax)
	movq	-48(%rcx), %rdx
	movq	%rdx, -48(%rax)
	movq	-40(%rcx), %rdx
	movq	%rdx, -40(%rax)
	movq	-32(%rcx), %rdx
	movq	%rdx, -32(%rax)
	movq	-24(%rcx), %rdx
	movq	%rdx, -24(%rax)
	movq	-16(%rcx), %rdx
	movq	%rdx, -16(%rax)
	movq	-8(%rcx), %rdx
	movq	%rdx, -8(%rax)
	movq	(%rcx), %rdx
	movq	%rdx, (%rax)
	addq	$64, %rax
	addq	$64, %rcx
	addq	$-8, %rbx
	jne	LBB0_119
LBB0_111:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	%r14, %rsi
	movq	%r10, %rdx
	jmp	LBB0_346
LBB0_135:                               ##   in Loop: Header=BB0_3 Depth=1
	subq	%r11, %rbx
	movq	%r12, %rsi
	movq	-64(%rbp), %rdx         ## 8-byte Reload
LBB0_136:                               ## %.lr.ph108.preheader719
                                        ##   in Loop: Header=BB0_3 Depth=1
	shlq	$3, %r14
	subq	%r14, %r13
	addq	$-8, %r13
LBB0_137:                               ## %.lr.ph108
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	16(%rdi,%rbx,8), %rax
	movq	%rax, (%r13,%rbx,8)
	cmpq	$1, %rbx
	leaq	-1(%rbx), %rbx
	jg	LBB0_137
## BB#138:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	%r10, %r13
	jmp	LBB0_346
	.align	4, 0x90
LBB0_3:                                 ## %.lr.ph122
                                        ## =>This Loop Header: Depth=1
                                        ##     Child Loop BB0_245 Depth 2
                                        ##     Child Loop BB0_248 Depth 2
                                        ##     Child Loop BB0_253 Depth 2
                                        ##     Child Loop BB0_257 Depth 2
                                        ##     Child Loop BB0_268 Depth 2
                                        ##     Child Loop BB0_271 Depth 2
                                        ##     Child Loop BB0_275 Depth 2
                                        ##     Child Loop BB0_278 Depth 2
                                        ##     Child Loop BB0_224 Depth 2
                                        ##     Child Loop BB0_227 Depth 2
                                        ##     Child Loop BB0_231 Depth 2
                                        ##     Child Loop BB0_234 Depth 2
                                        ##     Child Loop BB0_197 Depth 2
                                        ##     Child Loop BB0_200 Depth 2
                                        ##     Child Loop BB0_204 Depth 2
                                        ##     Child Loop BB0_145 Depth 2
                                        ##       Child Loop BB0_150 Depth 3
                                        ##       Child Loop BB0_154 Depth 3
                                        ##       Child Loop BB0_157 Depth 3
                                        ##       Child Loop BB0_165 Depth 3
                                        ##       Child Loop BB0_168 Depth 3
                                        ##       Child Loop BB0_172 Depth 3
                                        ##       Child Loop BB0_175 Depth 3
                                        ##     Child Loop BB0_131 Depth 2
                                        ##     Child Loop BB0_137 Depth 2
                                        ##     Child Loop BB0_106 Depth 2
                                        ##     Child Loop BB0_109 Depth 2
                                        ##     Child Loop BB0_115 Depth 2
                                        ##     Child Loop BB0_119 Depth 2
                                        ##     Child Loop BB0_85 Depth 2
                                        ##     Child Loop BB0_88 Depth 2
                                        ##     Child Loop BB0_93 Depth 2
                                        ##     Child Loop BB0_96 Depth 2
                                        ##     Child Loop BB0_65 Depth 2
                                        ##     Child Loop BB0_68 Depth 2
                                        ##     Child Loop BB0_72 Depth 2
                                        ##     Child Loop BB0_75 Depth 2
                                        ##     Child Loop BB0_48 Depth 2
                                        ##     Child Loop BB0_53 Depth 2
                                        ##     Child Loop BB0_23 Depth 2
                                        ##     Child Loop BB0_26 Depth 2
                                        ##     Child Loop BB0_30 Depth 2
                                        ##     Child Loop BB0_33 Depth 2
                                        ##     Child Loop BB0_308 Depth 2
                                        ##     Child Loop BB0_311 Depth 2
                                        ##     Child Loop BB0_315 Depth 2
                                        ##     Child Loop BB0_318 Depth 2
                                        ##     Child Loop BB0_333 Depth 2
                                        ##     Child Loop BB0_336 Depth 2
                                        ##     Child Loop BB0_341 Depth 2
                                        ##     Child Loop BB0_292 Depth 2
                                        ##     Child Loop BB0_295 Depth 2
                                        ##     Child Loop BB0_299 Depth 2
	movq	(%rdi), %rcx
	testb	$1, %cl
	jne	LBB0_344
## BB#4:                                ##   in Loop: Header=BB0_3 Depth=1
	leaq	1(%r12), %rax
	movq	%rax, (%rdi)
	movq	%rcx, %rax
	subq	__ARRAY__@GOTPCREL(%rip), %rax
	leaq	8(%r12), %r9
	movq	%rax, (%r12)
	testb	$2, %cl
	jne	LBB0_5
## BB#280:                              ##   in Loop: Header=BB0_3 Depth=1
	movslq	-4(%rcx), %r14
	cmpq	$2, %r14
	movl	%r14d, %r11d
	jl	LBB0_179
## BB#281:                              ##   in Loop: Header=BB0_3 Depth=1
	cmpl	$255, %r11d
	jg	LBB0_300
## BB#282:                              ##   in Loop: Header=BB0_3 Depth=1
	movl	$1, %eax
	subq	%r14, %rax
	leaq	(%r13,%rax,8), %r8
	cmpq	-48(%rbp), %r8          ## 8-byte Folded Reload
	jae	LBB0_284
## BB#283:                              ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%r9, %r8
	movq	%r8, -48(%rbp)          ## 8-byte Spill
	jb	LBB0_350
LBB0_284:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	(%rdi,%r14,8), %rax
	movq	%rax, -16(%r8,%r14,8)
	cmpl	$3, %r11d
	jl	LBB0_205
## BB#285:                              ## %.lr.ph114.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-3(%r14), %rcx
	leaq	-2(%r14), %r10
	cmpq	$4, %r10
	jb	LBB0_298
## BB#286:                              ## %min.iters.checked
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r10, %r11
	andq	$-4, %r11
	je	LBB0_298
## BB#287:                              ## %vector.memcheck
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rdx, %r12
	leaq	(,%r14,8), %rax
	negq	%rax
	leaq	8(%r13,%rax), %rax
	leaq	-8(%rdi,%r14,8), %rdx
	cmpq	%rdx, %rax
	ja	LBB0_290
## BB#288:                              ## %vector.memcheck
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-16(%r13), %rax
	leaq	16(%rdi), %rdx
	cmpq	%rax, %rdx
	ja	LBB0_290
## BB#289:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	%r12, %rdx
	jmp	LBB0_298
	.align	4, 0x90
LBB0_344:                               ##   in Loop: Header=BB0_3 Depth=1
	subq	%r12, %rcx
	movq	%rcx, (%r12)
	addq	$8, %r12
LBB0_345:                               ## %.loopexit
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r12, %r9
LBB0_346:                               ## %.loopexit
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%rdx, %r13
	je	LBB0_347
## BB#348:                              ##   in Loop: Header=BB0_3 Depth=1
	leaq	8(%r13), %r8
	jmp	LBB0_349
	.align	4, 0x90
LBB0_5:                                 ##   in Loop: Header=BB0_3 Depth=1
	movzwl	-2(%rcx), %r14d
	cmpq	$2, %r14
	je	LBB0_182
## BB#6:                                ##   in Loop: Header=BB0_3 Depth=1
	movzwl	%r14w, %eax
	cmpl	$1, %eax
	je	LBB0_180
## BB#7:                                ##   in Loop: Header=BB0_3 Depth=1
	testw	%r14w, %r14w
	jne	LBB0_186
## BB#8:                                ##   in Loop: Header=BB0_3 Depth=1
	cmpq	-88(%rbp), %rcx         ## 8-byte Folded Reload
	je	LBB0_12
## BB#9:                                ##   in Loop: Header=BB0_3 Depth=1
	cmpq	-96(%rbp), %rcx         ## 8-byte Folded Reload
	je	LBB0_12
## BB#10:                               ##   in Loop: Header=BB0_3 Depth=1
	cmpq	-104(%rbp), %rcx        ## 8-byte Folded Reload
	je	LBB0_12
## BB#11:                               ##   in Loop: Header=BB0_3 Depth=1
	cmpq	-112(%rbp), %rcx        ## 8-byte Folded Reload
	je	LBB0_12
## BB#14:                               ##   in Loop: Header=BB0_3 Depth=1
	cmpq	-80(%rbp), %rcx         ## 8-byte Folded Reload
	jne	LBB0_35
## BB#15:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	8(%rdi), %rbx
	leaq	7(%rbx), %rcx
	movq	%rcx, %r10
	shrq	$3, %r10
	leaq	(%r9,%r10,8), %rax
	cmpq	-48(%rbp), %rax         ## 8-byte Folded Reload
	jae	LBB0_350
## BB#16:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	%rdx, %r14
	movq	%rsi, %r8
	leaq	16(%r12), %r9
	movq	%rbx, 8(%r12)
	testq	%r10, %r10
	je	LBB0_34
## BB#17:                               ## %.lr.ph.i.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$32, %rcx
	movl	$0, %esi
	jb	LBB0_28
## BB#18:                               ## %min.iters.checked428
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r10, %r11
	movabsq	$2305843009213693948, %rax ## imm = 0x1FFFFFFFFFFFFFFC
	andq	%rax, %r11
	movl	$0, %esi
	je	LBB0_28
## BB#19:                               ## %vector.memcheck447
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rcx, %rax
	andq	$-8, %rax
	leaq	8(%rdi,%rax), %rdx
	cmpq	%rdx, %r9
	ja	LBB0_21
## BB#20:                               ## %vector.memcheck447
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	8(%r12,%rax), %rax
	leaq	16(%rdi), %rdx
	cmpq	%rax, %rdx
	movl	$0, %esi
	jbe	LBB0_28
LBB0_21:                                ## %vector.body424.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	shrq	$5, %rcx
	leaq	-4(,%rcx,4), %rsi
	movl	%esi, %eax
	shrl	$2, %eax
	incl	%eax
	testb	$3, %al
	movl	$0, %edx
	je	LBB0_24
## BB#22:                               ## %vector.body424.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leal	7(%rbx), %eax
	shrl	$3, %eax
	andl	$12, %eax
	addl	$-4, %eax
	shrl	$2, %eax
	incl	%eax
	andl	$3, %eax
	negq	%rax
	xorl	%edx, %edx
LBB0_23:                                ## %vector.body424.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	16(%rdi,%rdx,8), %xmm0
	movups	32(%rdi,%rdx,8), %xmm1
	movups	%xmm0, 16(%r12,%rdx,8)
	movups	%xmm1, 32(%r12,%rdx,8)
	addq	$4, %rdx
	incq	%rax
	jne	LBB0_23
LBB0_24:                                ## %vector.body424.preheader.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$12, %rsi
	jb	LBB0_27
## BB#25:                               ## %vector.body424.preheader.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	shlq	$2, %rcx
	subq	%rdx, %rcx
	leaq	128(%r12,%rdx,8), %rax
	leaq	128(%rdi,%rdx,8), %rdx
LBB0_26:                                ## %vector.body424
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-112(%rdx), %xmm0
	movups	-96(%rdx), %xmm1
	movups	%xmm0, -112(%rax)
	movups	%xmm1, -96(%rax)
	movups	-80(%rdx), %xmm0
	movups	-64(%rdx), %xmm1
	movups	%xmm0, -80(%rax)
	movups	%xmm1, -64(%rax)
	movups	-48(%rdx), %xmm0
	movups	-32(%rdx), %xmm1
	movups	%xmm0, -48(%rax)
	movups	%xmm1, -32(%rax)
	movups	-16(%rdx), %xmm0
	movups	(%rdx), %xmm1
	movups	%xmm0, -16(%rax)
	movups	%xmm1, (%rax)
	subq	$-128, %rax
	subq	$-128, %rdx
	addq	$-16, %rcx
	jne	LBB0_26
LBB0_27:                                ## %middle.block425
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%r11, %r10
	movq	%r11, %rsi
	je	LBB0_34
LBB0_28:                                ## %.lr.ph.i.preheader711
                                        ##   in Loop: Header=BB0_3 Depth=1
	movl	%r10d, %ecx
	subl	%esi, %ecx
	leaq	-1(%r10), %rax
	subq	%rsi, %rax
	testb	$7, %cl
	je	LBB0_31
## BB#29:                               ## %.lr.ph.i.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	addl	$7, %ebx
	shrl	$3, %ebx
	subl	%esi, %ebx
	andl	$7, %ebx
	negq	%rbx
	.align	4, 0x90
LBB0_30:                                ## %.lr.ph.i.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	16(%rdi,%rsi,8), %rcx
	movq	%rcx, 16(%r12,%rsi,8)
	incq	%rsi
	incq	%rbx
	jne	LBB0_30
LBB0_31:                                ## %.lr.ph.i.preheader711.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$7, %rax
	jb	LBB0_34
## BB#32:                               ## %.lr.ph.i.preheader711.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r10, %rax
	subq	%rsi, %rax
	leaq	72(%r12,%rsi,8), %rcx
	leaq	72(%rdi,%rsi,8), %rdx
	.align	4, 0x90
LBB0_33:                                ## %.lr.ph.i
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-56(%rdx), %rsi
	movq	%rsi, -56(%rcx)
	movq	-48(%rdx), %rsi
	movq	%rsi, -48(%rcx)
	movq	-40(%rdx), %rsi
	movq	%rsi, -40(%rcx)
	movq	-32(%rdx), %rsi
	movq	%rsi, -32(%rcx)
	movq	-24(%rdx), %rsi
	movq	%rsi, -24(%rcx)
	movq	-16(%rdx), %rsi
	movq	%rsi, -16(%rcx)
	movq	-8(%rdx), %rsi
	movq	%rsi, -8(%rcx)
	movq	(%rdx), %rsi
	movq	%rsi, (%rcx)
	addq	$64, %rcx
	addq	$64, %rdx
	addq	$-8, %rax
	jne	LBB0_33
LBB0_34:                                ## %copy.exit
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	(%r9,%r10,8), %r9
	movq	%r8, %rsi
	movq	%r14, %rdx
	jmp	LBB0_346
LBB0_179:                               ##   in Loop: Header=BB0_3 Depth=1
	testl	%r14d, %r14d
	jne	LBB0_180
	jmp	LBB0_346
LBB0_300:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	%r14, %r15
	sarq	$8, %r15
	movzbl	%r14b, %ecx
	leaq	(%r9,%r15,8), %r9
	cmpq	-48(%rbp), %r9          ## 8-byte Folded Reload
	ja	LBB0_350
## BB#301:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	%rsi, %r10
	testq	%r15, %r15
	jle	LBB0_319
## BB#302:                              ## %.lr.ph.i.51.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$4, %r15
	movl	$0, %ebx
	jb	LBB0_313
## BB#303:                              ## %min.iters.checked245
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r15, %r8
	andq	$-4, %r8
	movl	$0, %ebx
	je	LBB0_313
## BB#304:                              ## %vector.memcheck263
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	8(%r12), %rdx
	movzbl	%r11b, %eax
	leaq	(%rdi,%rax,8), %rsi
	cmpq	%rsi, %rdx
	ja	LBB0_306
## BB#305:                              ## %vector.memcheck263
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	(%r12,%r15,8), %rdx
	subq	%r15, %rax
	leaq	8(%rdi,%rax,8), %rax
	cmpq	%rdx, %rax
	movl	$0, %ebx
	jbe	LBB0_313
LBB0_306:                               ## %vector.body241.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-4(%r15), %rsi
	movl	%esi, %eax
	shrl	$2, %eax
	incl	%eax
	testb	$3, %al
	movl	$0, %edx
	je	LBB0_309
## BB#307:                              ## %vector.body241.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	movzbl	%r11b, %eax
	subq	%r15, %rax
	leaq	24(%rdi,%rax,8), %rax
	leal	-4(%r15), %ebx
	shrl	$2, %ebx
	incl	%ebx
	andl	$3, %ebx
	negq	%rbx
	xorl	%edx, %edx
	.align	4, 0x90
LBB0_308:                               ## %vector.body241.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%rax,%rdx,8), %xmm0
	movups	(%rax,%rdx,8), %xmm1
	movups	%xmm0, 8(%r12,%rdx,8)
	movups	%xmm1, 24(%r12,%rdx,8)
	addq	$4, %rdx
	incq	%rbx
	jne	LBB0_308
LBB0_309:                               ## %vector.body241.preheader.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$12, %rsi
	jb	LBB0_312
## BB#310:                              ## %vector.body241.preheader.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r15, %rsi
	andq	$-4, %rsi
	subq	%rdx, %rsi
	leaq	120(%r12,%rdx,8), %rax
	movzbl	%r11b, %ebx
	addq	%rdx, %rbx
	subq	%r15, %rbx
	leaq	120(%rdi,%rbx,8), %rdx
	.align	4, 0x90
LBB0_311:                               ## %vector.body241
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-112(%rdx), %xmm0
	movups	-96(%rdx), %xmm1
	movups	%xmm0, -112(%rax)
	movups	%xmm1, -96(%rax)
	movups	-80(%rdx), %xmm0
	movups	-64(%rdx), %xmm1
	movups	%xmm0, -80(%rax)
	movups	%xmm1, -64(%rax)
	movups	-48(%rdx), %xmm0
	movups	-32(%rdx), %xmm1
	movups	%xmm0, -48(%rax)
	movups	%xmm1, -32(%rax)
	movups	-16(%rdx), %xmm0
	movups	(%rdx), %xmm1
	movups	%xmm0, -16(%rax)
	movups	%xmm1, (%rax)
	subq	$-128, %rax
	subq	$-128, %rdx
	addq	$-16, %rsi
	jne	LBB0_311
LBB0_312:                               ## %middle.block242
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%r8, %r15
	movq	%r8, %rbx
	je	LBB0_319
LBB0_313:                               ## %.lr.ph.i.51.preheader706
                                        ##   in Loop: Header=BB0_3 Depth=1
	movl	%r15d, %eax
	subl	%ebx, %eax
	leaq	-1(%r15), %r8
	subq	%rbx, %r8
	testb	$7, %al
	je	LBB0_316
## BB#314:                              ## %.lr.ph.i.51.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	movzbl	%r11b, %eax
	subq	%r15, %rax
	leaq	8(%rdi,%rax,8), %rax
	movl	%r15d, %edx
	subl	%ebx, %edx
	andl	$7, %edx
	negq	%rdx
	.align	4, 0x90
LBB0_315:                               ## %.lr.ph.i.51.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	(%rax,%rbx,8), %rsi
	movq	%rsi, 8(%r12,%rbx,8)
	incq	%rbx
	incq	%rdx
	jne	LBB0_315
LBB0_316:                               ## %.lr.ph.i.51.preheader706.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$7, %r8
	jb	LBB0_319
## BB#317:                              ## %.lr.ph.i.51.preheader706.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	64(%r12,%rbx,8), %rax
	subq	%r15, %rbx
	movzbl	%r11b, %edx
	leaq	64(%rdi,%rdx,8), %rdx
	.align	4, 0x90
LBB0_318:                               ## %.lr.ph.i.51
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-56(%rdx,%rbx,8), %rsi
	movq	%rsi, -56(%rax)
	movq	-48(%rdx,%rbx,8), %rsi
	movq	%rsi, -48(%rax)
	movq	-40(%rdx,%rbx,8), %rsi
	movq	%rsi, -40(%rax)
	movq	-32(%rdx,%rbx,8), %rsi
	movq	%rsi, -32(%rax)
	movq	-24(%rdx,%rbx,8), %rsi
	movq	%rsi, -24(%rax)
	movq	-16(%rdx,%rbx,8), %rsi
	movq	%rsi, -16(%rax)
	movq	-8(%rdx,%rbx,8), %rsi
	movq	%rsi, -8(%rax)
	movq	(%rdx,%rbx,8), %rsi
	movq	%rsi, (%rax)
	addq	$64, %rax
	addq	$8, %rbx
	jne	LBB0_318
LBB0_319:                               ## %copy.exit52
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%r15, %rcx
	jne	LBB0_321
## BB#320:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	%r10, %rsi
	movq	-64(%rbp), %rdx         ## 8-byte Reload
	jmp	LBB0_346
LBB0_186:                               ##   in Loop: Header=BB0_3 Depth=1
	cmpl	$255, %r14d
	ja	LBB0_206
## BB#187:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	16(%rdi), %r10
	movl	$1, %eax
	subq	%r14, %rax
	leaq	(%r13,%rax,8), %r8
	cmpq	-48(%rbp), %r8          ## 8-byte Folded Reload
	jae	LBB0_189
## BB#188:                              ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%r9, %r8
	movq	%r8, -48(%rbp)          ## 8-byte Spill
	jb	LBB0_350
LBB0_189:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	-16(%r10,%r14,8), %rax
	movq	%rax, -16(%r8,%r14,8)
	movq	-24(%r10,%r14,8), %rax
	movq	%rax, -24(%r8,%r14,8)
	cmpl	$4, %r14d
	jb	LBB0_205
## BB#190:                              ## %.lr.ph110.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-4(%r14), %rcx
	leaq	-3(%r14), %r11
	cmpq	$4, %r11
	jb	LBB0_203
## BB#191:                              ## %min.iters.checked281
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r11, %r15
	andq	$-4, %r15
	je	LBB0_203
## BB#192:                              ## %vector.memcheck298
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	(,%r14,8), %rax
	negq	%rax
	leaq	8(%r13,%rax), %rax
	leaq	-32(%r10,%r14,8), %rdx
	cmpq	%rdx, %rax
	ja	LBB0_195
## BB#193:                              ## %vector.memcheck298
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-24(%r13), %rax
	cmpq	%rax, %r10
	ja	LBB0_195
## BB#194:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	-64(%rbp), %rdx         ## 8-byte Reload
	jmp	LBB0_203
LBB0_206:                               ##   in Loop: Header=BB0_3 Depth=1
	movzwl	(%rcx), %r10d
	cmpq	$258, %r14              ## imm = 0x102
	je	LBB0_209
## BB#207:                              ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$257, %r14              ## imm = 0x101
	jne	LBB0_215
## BB#208:                              ##   in Loop: Header=BB0_3 Depth=1
	testw	%r10w, %r10w
	jne	LBB0_180
LBB0_12:                                ##   in Loop: Header=BB0_3 Depth=1
	cmpq	-48(%rbp), %r9          ## 8-byte Folded Reload
	jae	LBB0_350
## BB#13:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	8(%rdi), %rax
	movq	%rax, 8(%r12)
	addq	$16, %r12
	jmp	LBB0_345
LBB0_321:                               ##   in Loop: Header=BB0_3 Depth=1
	subq	%r15, %rcx
	cmpq	$2, %rcx
	movq	%r10, %rsi
	movq	-64(%rbp), %rdx         ## 8-byte Reload
	jl	LBB0_180
## BB#322:                              ##   in Loop: Header=BB0_3 Depth=1
	movl	$1, %eax
	subq	%rcx, %rax
	leaq	(%r13,%rax,8), %r10
	cmpq	-48(%rbp), %r10         ## 8-byte Folded Reload
	jae	LBB0_324
## BB#323:                              ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%r9, %r10
	movq	%r10, -48(%rbp)         ## 8-byte Spill
	jb	LBB0_350
LBB0_324:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	(%rdi,%rcx,8), %rax
	movq	%rax, -16(%r10,%rcx,8)
	cmpq	$3, %rcx
	jl	LBB0_325
## BB#326:                              ## %.lr.ph112.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	addq	$-3, %rcx
	leaq	2(%r15), %r8
	movzbl	%r11b, %eax
	movq	%r8, %rdx
	subq	%rax, %rdx
	cmpq	$-2, %rdx
	movq	$-1, %rbx
	cmovleq	%rbx, %rdx
	leaq	-1(%rdx,%rax), %r12
	subq	%r15, %r12
	cmpq	$4, %r12
	jb	LBB0_340
## BB#327:                              ## %min.iters.checked203
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r12, %rax
	andq	$-4, %rax
	je	LBB0_340
## BB#328:                              ## %vector.memcheck223
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rax, -120(%rbp)        ## 8-byte Spill
	movzbl	%r11b, %edx
	movq	%r8, %rax
	subq	%rdx, %rax
	cmpq	$-2, %rax
	movq	$-1, %rsi
	cmovleq	%rsi, %rax
	leaq	(%rax,%rdx), %rsi
	movq	%r15, %rbx
	subq	%rsi, %rbx
	leaq	(%r13,%rbx,8), %rsi
	subq	%r15, %rdx
	leaq	-8(%rdi,%rdx,8), %rdx
	cmpq	%rdx, %rsi
	ja	LBB0_331
## BB#329:                              ## %vector.memcheck223
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-16(%r13), %rdx
	shlq	$3, %rax
	negq	%rax
	leaq	8(%rdi,%rax), %rax
	cmpq	%rdx, %rax
	ja	LBB0_331
## BB#330:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	-72(%rbp), %rsi         ## 8-byte Reload
	jmp	LBB0_340
LBB0_35:                                ##   in Loop: Header=BB0_3 Depth=1
	movq	__ARRAY__@GOTPCREL(%rip), %rax
	leaq	2(%rax), %rax
	cmpq	%rax, %rcx
	jne	LBB0_346
## BB#36:                               ##   in Loop: Header=BB0_3 Depth=1
	leaq	24(%r12), %r9
	cmpq	-48(%rbp), %r9          ## 8-byte Folded Reload
	ja	LBB0_350
## BB#37:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	8(%rdi), %r15
	movq	16(%rdi), %rax
	movq	%r15, 8(%r12)
	movq	%rax, %rcx
	subq	__ARRAY__@GOTPCREL(%rip), %rcx
	testq	%rax, %rax
	cmoveq	%rax, %rcx
	movq	%rcx, 16(%r12)
	je	LBB0_38
## BB#55:                               ##   in Loop: Header=BB0_3 Depth=1
	cmpq	-104(%rbp), %rax        ## 8-byte Folded Reload
	je	LBB0_57
## BB#56:                               ##   in Loop: Header=BB0_3 Depth=1
	cmpq	-88(%rbp), %rax         ## 8-byte Folded Reload
	je	LBB0_57
## BB#76:                               ##   in Loop: Header=BB0_3 Depth=1
	cmpq	-96(%rbp), %rax         ## 8-byte Folded Reload
	jne	LBB0_97
## BB#77:                               ##   in Loop: Header=BB0_3 Depth=1
	addq	$7, %r15
	movq	%r15, %rax
	andq	$-8, %rax
	addq	%rax, %r9
	cmpq	-48(%rbp), %r9          ## 8-byte Folded Reload
	ja	LBB0_350
## BB#78:                               ##   in Loop: Header=BB0_3 Depth=1
	sarq	$3, %r15
	testq	%r15, %r15
	jle	LBB0_346
## BB#79:                               ## %.lr.ph.i.79.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rdx, %r10
	movq	%rsi, %r8
	cmpq	$4, %r15
	movl	$0, %esi
	jb	LBB0_91
## BB#80:                               ## %min.iters.checked528
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r15, %rcx
	andq	$-4, %rcx
	movl	$0, %esi
	je	LBB0_91
## BB#81:                               ## %vector.memcheck547
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	24(%r12), %rax
	leaq	16(%rdi,%r15,8), %rdx
	cmpq	%rdx, %rax
	ja	LBB0_83
## BB#82:                               ## %vector.memcheck547
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	16(%r12,%r15,8), %rax
	leaq	24(%rdi), %rdx
	cmpq	%rax, %rdx
	movl	$0, %esi
	jbe	LBB0_91
LBB0_83:                                ## %vector.body524.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-4(%r15), %rsi
	movl	%esi, %eax
	shrl	$2, %eax
	incl	%eax
	testb	$3, %al
	movl	$0, %ebx
	je	LBB0_86
## BB#84:                               ## %vector.body524.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leal	-4(%r15), %eax
	shrl	$2, %eax
	incl	%eax
	andl	$3, %eax
	negq	%rax
	xorl	%ebx, %ebx
LBB0_85:                                ## %vector.body524.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	24(%rdi,%rbx,8), %xmm0
	movups	40(%rdi,%rbx,8), %xmm1
	movups	%xmm0, 24(%r12,%rbx,8)
	movups	%xmm1, 40(%r12,%rbx,8)
	addq	$4, %rbx
	incq	%rax
	jne	LBB0_85
LBB0_86:                                ## %vector.body524.preheader.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$12, %rsi
	jb	LBB0_89
## BB#87:                               ## %vector.body524.preheader.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r15, %rax
	andq	$-4, %rax
	subq	%rbx, %rax
	leaq	136(%r12,%rbx,8), %rdx
	leaq	136(%rdi,%rbx,8), %rsi
LBB0_88:                                ## %vector.body524
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-112(%rsi), %xmm0
	movups	-96(%rsi), %xmm1
	movups	%xmm0, -112(%rdx)
	movups	%xmm1, -96(%rdx)
	movups	-80(%rsi), %xmm0
	movups	-64(%rsi), %xmm1
	movups	%xmm0, -80(%rdx)
	movups	%xmm1, -64(%rdx)
	movups	-48(%rsi), %xmm0
	movups	-32(%rsi), %xmm1
	movups	%xmm0, -48(%rdx)
	movups	%xmm1, -32(%rdx)
	movups	-16(%rsi), %xmm0
	movups	(%rsi), %xmm1
	movups	%xmm0, -16(%rdx)
	movups	%xmm1, (%rdx)
	subq	$-128, %rdx
	subq	$-128, %rsi
	addq	$-16, %rax
	jne	LBB0_88
LBB0_89:                                ## %middle.block525
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%rcx, %r15
	movq	%rcx, %rsi
	je	LBB0_90
LBB0_91:                                ## %.lr.ph.i.79.preheader715
                                        ##   in Loop: Header=BB0_3 Depth=1
	movl	%r15d, %ecx
	subl	%esi, %ecx
	leaq	-1(%r15), %rax
	subq	%rsi, %rax
	testb	$7, %cl
	je	LBB0_94
## BB#92:                               ## %.lr.ph.i.79.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	movl	%r15d, %ecx
	subl	%esi, %ecx
	andl	$7, %ecx
	negq	%rcx
LBB0_93:                                ## %.lr.ph.i.79.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	24(%rdi,%rsi,8), %rdx
	movq	%rdx, 24(%r12,%rsi,8)
	incq	%rsi
	incq	%rcx
	jne	LBB0_93
LBB0_94:                                ## %.lr.ph.i.79.preheader715.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$7, %rax
	jb	LBB0_90
## BB#95:                               ## %.lr.ph.i.79.preheader715.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	subq	%rsi, %r15
	leaq	80(%r12,%rsi,8), %rax
	leaq	80(%rdi,%rsi,8), %rcx
LBB0_96:                                ## %.lr.ph.i.79
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-56(%rcx), %rdx
	movq	%rdx, -56(%rax)
	movq	-48(%rcx), %rdx
	movq	%rdx, -48(%rax)
	movq	-40(%rcx), %rdx
	movq	%rdx, -40(%rax)
	movq	-32(%rcx), %rdx
	movq	%rdx, -32(%rax)
	movq	-24(%rcx), %rdx
	movq	%rdx, -24(%rax)
	movq	-16(%rcx), %rdx
	movq	%rdx, -16(%rax)
	movq	-8(%rcx), %rdx
	movq	%rdx, -8(%rax)
	movq	(%rcx), %rdx
	movq	%rdx, (%rax)
	addq	$64, %rax
	addq	$64, %rcx
	addq	$-8, %r15
	jne	LBB0_96
	jmp	LBB0_90
LBB0_209:                               ##   in Loop: Header=BB0_3 Depth=1
	cmpl	$1, %r10d
	je	LBB0_213
## BB#210:                              ##   in Loop: Header=BB0_3 Depth=1
	testw	%r10w, %r10w
	jne	LBB0_182
## BB#211:                              ##   in Loop: Header=BB0_3 Depth=1
	leaq	24(%r12), %r9
	cmpq	-48(%rbp), %r9          ## 8-byte Folded Reload
	ja	LBB0_350
## BB#212:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	8(%rdi), %rax
	movq	%rax, 8(%r12)
	movq	16(%rdi), %rax
	movq	%rax, 16(%r12)
	jmp	LBB0_346
LBB0_182:                               ##   in Loop: Header=BB0_3 Depth=1
	cmpq	-48(%rbp), %r13         ## 8-byte Folded Reload
	ja	LBB0_185
## BB#183:                              ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%r9, %r13
	jbe	LBB0_350
## BB#184:                              ##   in Loop: Header=BB0_3 Depth=1
	addq	$-8, -48(%rbp)          ## 8-byte Folded Spill
LBB0_185:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	16(%rdi), %rax
	movq	%rax, -8(%r13)
	addq	$-8, %r13
LBB0_180:                               ##   in Loop: Header=BB0_3 Depth=1
	addq	$8, %rdi
LBB0_181:                               ## %copy.exit84.backedge
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r13, %r8
	movq	%rdi, %r13
	jmp	LBB0_349
LBB0_215:                               ##   in Loop: Header=BB0_3 Depth=1
	leaq	-256(%r14), %rbx
	movq	16(%rdi), %r15
	testw	%r10w, %r10w
	je	LBB0_216
## BB#236:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	%rbx, %r8
	subq	%r10, %r8
	jle	LBB0_237
## BB#238:                              ##   in Loop: Header=BB0_3 Depth=1
	leaq	(%r9,%r8,8), %r9
	cmpq	-48(%rbp), %r9          ## 8-byte Folded Reload
	ja	LBB0_350
## BB#239:                              ##   in Loop: Header=BB0_3 Depth=1
	leaq	-1(%r10), %r11
	cmpq	$4, %r8
	movl	$0, %ecx
	jb	LBB0_251
## BB#240:                              ## %min.iters.checked390
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r8, %rsi
	andq	$-4, %rsi
	movl	$0, %ecx
	je	LBB0_251
## BB#241:                              ## %vector.memcheck410
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	8(%r12), %rax
	leaq	-2064(%r15,%r14,8), %rcx
	cmpq	%rcx, %rax
	ja	LBB0_243
## BB#242:                              ## %vector.memcheck410
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r14, %rax
	subq	%r10, %rax
	leaq	-2048(%r12,%rax,8), %rax
	leaq	-8(%r15,%r10,8), %rcx
	cmpq	%rax, %rcx
	movl	$0, %ecx
	jbe	LBB0_251
LBB0_243:                               ## %vector.body386.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rsi, -120(%rbp)        ## 8-byte Spill
	leaq	-4(%r8), %rcx
	movl	%ecx, %eax
	shrl	$2, %eax
	incl	%eax
	testb	$3, %al
	movl	$0, %esi
	je	LBB0_246
## BB#244:                              ## %vector.body386.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	8(%r15,%r10,8), %rax
	leal	-260(%r14), %edx
	subl	%r10d, %edx
	shrl	$2, %edx
	incl	%edx
	andl	$3, %edx
	negq	%rdx
	xorl	%esi, %esi
LBB0_245:                               ## %vector.body386.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%rax,%rsi,8), %xmm0
	movups	(%rax,%rsi,8), %xmm1
	movups	%xmm0, 8(%r12,%rsi,8)
	movups	%xmm1, 24(%r12,%rsi,8)
	addq	$4, %rsi
	incq	%rdx
	jne	LBB0_245
LBB0_246:                               ## %vector.body386.preheader.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$12, %rcx
	jb	LBB0_249
## BB#247:                              ## %vector.body386.preheader.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r8, %rax
	andq	$-4, %rax
	subq	%rsi, %rax
	leaq	120(%r12,%rsi,8), %rcx
	addq	%r10, %rsi
	leaq	104(%r15,%rsi,8), %rdx
LBB0_248:                               ## %vector.body386
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-112(%rdx), %xmm0
	movups	-96(%rdx), %xmm1
	movups	%xmm0, -112(%rcx)
	movups	%xmm1, -96(%rcx)
	movups	-80(%rdx), %xmm0
	movups	-64(%rdx), %xmm1
	movups	%xmm0, -80(%rcx)
	movups	%xmm1, -64(%rcx)
	movups	-48(%rdx), %xmm0
	movups	-32(%rdx), %xmm1
	movups	%xmm0, -48(%rcx)
	movups	%xmm1, -32(%rcx)
	movups	-16(%rdx), %xmm0
	movups	(%rdx), %xmm1
	movups	%xmm0, -16(%rcx)
	movups	%xmm1, (%rcx)
	subq	$-128, %rcx
	subq	$-128, %rdx
	addq	$-16, %rax
	jne	LBB0_248
LBB0_249:                               ## %middle.block387
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	-120(%rbp), %rcx        ## 8-byte Reload
	cmpq	%rcx, %r8
	movq	-64(%rbp), %rdx         ## 8-byte Reload
	jne	LBB0_251
## BB#250:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	-72(%rbp), %rsi         ## 8-byte Reload
	jmp	LBB0_259
LBB0_325:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	%r10, %r13
	jmp	LBB0_180
LBB0_290:                               ## %vector.body.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-6(%r14), %r15
	movl	%r15d, %eax
	shrl	$2, %eax
	incl	%eax
	testb	$3, %al
	movl	$0, %ebx
	je	LBB0_293
## BB#291:                              ## %vector.body.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-24(%r13), %rax
	leaq	-16(%rdi,%r14,8), %rsi
	leal	-6(%r14), %edx
	shrl	$2, %edx
	incl	%edx
	andl	$3, %edx
	negq	%rdx
	xorl	%ebx, %ebx
	.align	4, 0x90
LBB0_292:                               ## %vector.body.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%rsi), %xmm0
	movups	(%rsi), %xmm1
	movups	%xmm1, (%rax)
	movups	%xmm0, -16(%rax)
	addq	$4, %rbx
	addq	$-32, %rax
	addq	$-32, %rsi
	incq	%rdx
	jne	LBB0_292
LBB0_293:                               ## %vector.body.preheader.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$12, %r15
	jb	LBB0_296
## BB#294:                              ## %vector.body.preheader.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r11, %rax
	subq	%rbx, %rax
	leaq	(,%rbx,8), %rsi
	movq	%r13, %rdx
	subq	%rsi, %rdx
	addq	$-24, %rdx
	movq	%r14, %rsi
	subq	%rbx, %rsi
	leaq	-16(%rdi,%rsi,8), %rsi
	.align	4, 0x90
LBB0_295:                               ## %vector.body
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%rsi), %xmm0
	movups	(%rsi), %xmm1
	movups	%xmm1, (%rdx)
	movups	%xmm0, -16(%rdx)
	movups	-48(%rsi), %xmm0
	movups	-32(%rsi), %xmm1
	movups	%xmm1, -32(%rdx)
	movups	%xmm0, -48(%rdx)
	movups	-80(%rsi), %xmm0
	movups	-64(%rsi), %xmm1
	movups	%xmm1, -64(%rdx)
	movups	%xmm0, -80(%rdx)
	movups	-112(%rsi), %xmm0
	movups	-96(%rsi), %xmm1
	movups	%xmm1, -96(%rdx)
	movups	%xmm0, -112(%rdx)
	addq	$-128, %rdx
	addq	$-128, %rsi
	addq	$-16, %rax
	jne	LBB0_295
LBB0_296:                               ## %middle.block
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%r11, %r10
	movq	-72(%rbp), %rsi         ## 8-byte Reload
	movq	%r12, %rdx
	je	LBB0_205
## BB#297:                              ##   in Loop: Header=BB0_3 Depth=1
	subq	%r11, %rcx
LBB0_298:                               ## %.lr.ph114.preheader703
                                        ##   in Loop: Header=BB0_3 Depth=1
	incq	%rcx
	shlq	$3, %r14
	subq	%r14, %r13
	.align	4, 0x90
LBB0_299:                               ## %.lr.ph114
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	8(%rdi,%rcx,8), %rax
	movq	%rax, (%r13,%rcx,8)
	decq	%rcx
	jg	LBB0_299
	jmp	LBB0_205
LBB0_216:                               ##   in Loop: Header=BB0_3 Depth=1
	leaq	(%r9,%rbx,8), %rax
	cmpq	-48(%rbp), %rax         ## 8-byte Folded Reload
	jae	LBB0_350
## BB#217:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	%rdx, %r10
	movq	%rsi, %r8
	movq	8(%rdi), %rax
	movq	%rax, 8(%r12)
	leaq	16(%r12), %rdi
	leaq	-257(%r14), %rbx
	cmpl	$258, %r14d             ## imm = 0x102
	jb	LBB0_235
## BB#218:                              ## %.lr.ph.i.63.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$4, %rbx
	movl	$0, %esi
	jb	LBB0_229
## BB#219:                              ## %min.iters.checked320
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rbx, %rcx
	andq	$-4, %rcx
	movl	$0, %esi
	je	LBB0_229
## BB#220:                              ## %vector.memcheck337
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-2064(%r15,%r14,8), %rax
	cmpq	%rax, %rdi
	ja	LBB0_222
## BB#221:                              ## %vector.memcheck337
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-2048(%r12,%r14,8), %rax
	cmpq	%rax, %r15
	movl	$0, %esi
	jbe	LBB0_229
LBB0_222:                               ## %vector.body316.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-261(%r14), %rdx
	movl	%edx, %eax
	shrl	$2, %eax
	incl	%eax
	testb	$3, %al
	movl	$0, %esi
	je	LBB0_225
## BB#223:                              ## %vector.body316.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leal	-261(%r14), %eax
	shrl	$2, %eax
	incl	%eax
	andl	$3, %eax
	negq	%rax
	xorl	%esi, %esi
LBB0_224:                               ## %vector.body316.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	(%r15,%rsi,8), %xmm0
	movups	16(%r15,%rsi,8), %xmm1
	movups	%xmm0, 16(%r12,%rsi,8)
	movups	%xmm1, 32(%r12,%rsi,8)
	addq	$4, %rsi
	incq	%rax
	jne	LBB0_224
LBB0_225:                               ## %vector.body316.preheader.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$12, %rdx
	jb	LBB0_228
## BB#226:                              ## %vector.body316.preheader.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rbx, %rax
	andq	$-4, %rax
	subq	%rsi, %rax
	leaq	128(%r12,%rsi,8), %rdx
	leaq	112(%r15,%rsi,8), %rsi
LBB0_227:                               ## %vector.body316
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-112(%rsi), %xmm0
	movups	-96(%rsi), %xmm1
	movups	%xmm0, -112(%rdx)
	movups	%xmm1, -96(%rdx)
	movups	-80(%rsi), %xmm0
	movups	-64(%rsi), %xmm1
	movups	%xmm0, -80(%rdx)
	movups	%xmm1, -64(%rdx)
	movups	-48(%rsi), %xmm0
	movups	-32(%rsi), %xmm1
	movups	%xmm0, -48(%rdx)
	movups	%xmm1, -32(%rdx)
	movups	-16(%rsi), %xmm0
	movups	(%rsi), %xmm1
	movups	%xmm0, -16(%rdx)
	movups	%xmm1, (%rdx)
	subq	$-128, %rdx
	subq	$-128, %rsi
	addq	$-16, %rax
	jne	LBB0_227
LBB0_228:                               ## %middle.block317
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%rcx, %rbx
	movq	%rcx, %rsi
	je	LBB0_235
LBB0_229:                               ## %.lr.ph.i.63.preheader709
                                        ##   in Loop: Header=BB0_3 Depth=1
	leal	7(%r14), %ecx
	subl	%esi, %ecx
	leaq	-258(%r14), %rax
	subq	%rsi, %rax
	testb	$7, %cl
	je	LBB0_232
## BB#230:                              ## %.lr.ph.i.63.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	addl	$7, %r14d
	subl	%esi, %r14d
	andl	$7, %r14d
	negq	%r14
LBB0_231:                               ## %.lr.ph.i.63.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	(%r15,%rsi,8), %rcx
	movq	%rcx, 16(%r12,%rsi,8)
	incq	%rsi
	incq	%r14
	jne	LBB0_231
LBB0_232:                               ## %.lr.ph.i.63.preheader709.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$7, %rax
	jb	LBB0_235
## BB#233:                              ## %.lr.ph.i.63.preheader709.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rbx, %rax
	subq	%rsi, %rax
	leaq	72(%r12,%rsi,8), %rcx
	leaq	56(%r15,%rsi,8), %rdx
LBB0_234:                               ## %.lr.ph.i.63
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-56(%rdx), %rsi
	movq	%rsi, -56(%rcx)
	movq	-48(%rdx), %rsi
	movq	%rsi, -48(%rcx)
	movq	-40(%rdx), %rsi
	movq	%rsi, -40(%rcx)
	movq	-32(%rdx), %rsi
	movq	%rsi, -32(%rcx)
	movq	-24(%rdx), %rsi
	movq	%rsi, -24(%rcx)
	movq	-16(%rdx), %rsi
	movq	%rsi, -16(%rcx)
	movq	-8(%rdx), %rsi
	movq	%rsi, -8(%rcx)
	movq	(%rdx), %rsi
	movq	%rsi, (%rcx)
	addq	$64, %rcx
	addq	$64, %rdx
	addq	$-8, %rax
	jne	LBB0_234
LBB0_235:                               ## %copy.exit64
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	(%rdi,%rbx,8), %r9
LBB0_90:                                ##   in Loop: Header=BB0_3 Depth=1
	movq	%r8, %rsi
	movq	%r10, %rdx
	jmp	LBB0_346
LBB0_251:                               ## %.lr.ph.i.59.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rdx, %r8
	movl	%r14d, %eax
	subl	%ecx, %eax
	subl	%r10d, %eax
	leaq	-257(%r14), %rsi
	subq	%rcx, %rsi
	subq	%r10, %rsi
	testb	$7, %al
	je	LBB0_254
## BB#252:                              ## %.lr.ph.i.59.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-8(%r15,%r10,8), %rax
	subl	%ecx, %r14d
	subl	%r10d, %r14d
	andl	$7, %r14d
	negq	%r14
	.align	4, 0x90
LBB0_253:                               ## %.lr.ph.i.59.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	(%rax,%rcx,8), %rdx
	movq	%rdx, 8(%r12,%rcx,8)
	incq	%rcx
	incq	%r14
	jne	LBB0_253
LBB0_254:                               ## %.lr.ph.i.59.preheader.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$7, %rsi
	jae	LBB0_256
## BB#255:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	-72(%rbp), %rsi         ## 8-byte Reload
	jmp	LBB0_258
LBB0_213:                               ##   in Loop: Header=BB0_3 Depth=1
	cmpq	-48(%rbp), %r9          ## 8-byte Folded Reload
	jae	LBB0_350
## BB#214:                              ##   in Loop: Header=BB0_3 Depth=1
	movq	16(%rdi), %rax
	movq	%rax, 8(%r12)
	addq	$16, %r12
	addq	$8, %rdi
	movq	%r12, %r9
	jmp	LBB0_181
LBB0_38:                                ##   in Loop: Header=BB0_3 Depth=1
	leaq	(,%r15,8), %r10
	movq	%r13, %rcx
	subq	%r10, %rcx
	cmpq	-48(%rbp), %rcx         ## 8-byte Folded Reload
	jae	LBB0_40
## BB#39:                               ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%r9, %rcx
	movq	%rcx, -48(%rbp)         ## 8-byte Spill
	jb	LBB0_350
LBB0_40:                                ## %.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	testq	%r15, %r15
	jle	LBB0_41
## BB#42:                               ## %.lr.ph109.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$2, %r15
	movl	$1, %eax
	cmovlq	%r15, %rax
	notq	%rax
	leaq	2(%r15,%rax), %rbx
	cmpq	$4, %rbx
	jb	LBB0_52
## BB#43:                               ## %min.iters.checked466
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rbx, %r11
	andq	$-4, %r11
	movq	%rbx, %r14
	andq	$-4, %r14
	je	LBB0_52
## BB#44:                               ## %vector.body461.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%rsi, %r12
	cmpq	$2, %r15
	movl	$1, %eax
	cmovlq	%r15, %rax
	notq	%rax
	leaq	-2(%r15,%rax), %rdx
	movq	%rdx, %rax
	shrq	$2, %rax
	btq	$2, %rdx
	movl	$0, %r8d
	jb	LBB0_46
## BB#45:                               ## %vector.body461.prol
                                        ##   in Loop: Header=BB0_3 Depth=1
	movups	8(%rdi,%r15,8), %xmm0
	movups	-8(%rdi,%r15,8), %xmm1
	movups	%xmm0, -16(%rcx,%r15,8)
	movups	%xmm1, -32(%rcx,%r15,8)
	movl	$4, %r8d
LBB0_46:                                ## %vector.body461.preheader.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	testq	%rax, %rax
	je	LBB0_49
## BB#47:                               ## %vector.body461.preheader.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r15, %rax
	notq	%rax
	cmpq	$-3, %rax
	movq	$-2, %rdx
	cmovleq	%rdx, %rax
	leaq	2(%r15,%rax), %rax
	andq	$-4, %rax
	subq	%r8, %rax
	movq	%r15, %rdx
	subq	%r8, %rdx
	leaq	8(%rdi,%rdx,8), %rdx
	shlq	$3, %r8
	movq	%r13, %rsi
	subq	%r8, %rsi
	addq	$-16, %rsi
LBB0_48:                                ## %vector.body461
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%rdx), %xmm0
	movups	(%rdx), %xmm1
	movups	%xmm1, (%rsi)
	movups	%xmm0, -16(%rsi)
	movups	-48(%rdx), %xmm0
	movups	-32(%rdx), %xmm1
	movups	%xmm1, -32(%rsi)
	movups	%xmm0, -48(%rsi)
	addq	$-64, %rdx
	addq	$-64, %rsi
	addq	$-8, %rax
	jne	LBB0_48
LBB0_49:                                ## %middle.block462
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%r14, %rbx
	jne	LBB0_51
## BB#50:                               ##   in Loop: Header=BB0_3 Depth=1
	movq	%rcx, %r13
	movq	%r12, %rsi
	movq	-64(%rbp), %rdx         ## 8-byte Reload
	jmp	LBB0_346
LBB0_237:                               ## %.copy.exit60.thread_crit_edge
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-1(%r10), %r11
	jmp	LBB0_259
LBB0_195:                               ## %vector.body277.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-7(%r14), %rdx
	movl	%edx, %eax
	shrl	$2, %eax
	incl	%eax
	testb	$3, %al
	movl	$0, %r12d
	je	LBB0_198
## BB#196:                              ## %vector.body277.prol.preheader
                                        ##   in Loop: Header=BB0_3 Depth=1
	leaq	-32(%r13), %rsi
	leaq	-40(%r10,%r14,8), %rax
	leal	-7(%r14), %ebx
	shrl	$2, %ebx
	incl	%ebx
	andl	$3, %ebx
	negq	%rbx
	xorl	%r12d, %r12d
	.align	4, 0x90
LBB0_197:                               ## %vector.body277.prol
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%rax), %xmm0
	movups	(%rax), %xmm1
	movups	%xmm1, (%rsi)
	movups	%xmm0, -16(%rsi)
	addq	$4, %r12
	addq	$-32, %rsi
	addq	$-32, %rax
	incq	%rbx
	jne	LBB0_197
LBB0_198:                               ## %vector.body277.preheader.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	$12, %rdx
	jb	LBB0_201
## BB#199:                              ## %vector.body277.preheader.split.split
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	%r15, %rax
	subq	%r12, %rax
	leaq	(,%r12,8), %rsi
	movq	%r13, %rdx
	subq	%rsi, %rdx
	addq	$-32, %rdx
	movq	%r14, %rsi
	subq	%r12, %rsi
	leaq	-40(%r10,%rsi,8), %rsi
LBB0_200:                               ## %vector.body277
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%rsi), %xmm0
	movups	(%rsi), %xmm1
	movups	%xmm1, (%rdx)
	movups	%xmm0, -16(%rdx)
	movups	-48(%rsi), %xmm0
	movups	-32(%rsi), %xmm1
	movups	%xmm1, -32(%rdx)
	movups	%xmm0, -48(%rdx)
	movups	-80(%rsi), %xmm0
	movups	-64(%rsi), %xmm1
	movups	%xmm1, -64(%rdx)
	movups	%xmm0, -80(%rdx)
	movups	-112(%rsi), %xmm0
	movups	-96(%rsi), %xmm1
	movups	%xmm1, -96(%rdx)
	movups	%xmm0, -112(%rdx)
	addq	$-128, %rdx
	addq	$-128, %rsi
	addq	$-16, %rax
	jne	LBB0_200
LBB0_201:                               ## %middle.block278
                                        ##   in Loop: Header=BB0_3 Depth=1
	cmpq	%r15, %r11
	movq	-72(%rbp), %rsi         ## 8-byte Reload
	movq	-64(%rbp), %rdx         ## 8-byte Reload
	je	LBB0_205
## BB#202:                              ##   in Loop: Header=BB0_3 Depth=1
	subq	%r15, %rcx
LBB0_203:                               ## %.lr.ph110.preheader707
                                        ##   in Loop: Header=BB0_3 Depth=1
	incq	%rcx
	shlq	$3, %r14
	subq	%r14, %r13
	.align	4, 0x90
LBB0_204:                               ## %.lr.ph110
                                        ##   Parent Loop BB0_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-8(%r10,%rcx,8), %rax
	movq	%rax, (%r13,%rcx,8)
	decq	%rcx
	jg	LBB0_204
LBB0_205:                               ## %._crit_edge
                                        ##   in Loop: Header=BB0_3 Depth=1
	addq	$8, %rdi
	movq	%rdi, %r13
	.align	4, 0x90
LBB0_349:                               ## %copy.exit84.backedge
                                        ##   in Loop: Header=BB0_3 Depth=1
	movq	(%r13), %rdi
	cmpq	-48(%rbp), %r9          ## 8-byte Folded Reload
	movq	%r8, %r13
	movq	%r9, %r12
	jb	LBB0_3
	jmp	LBB0_350
LBB0_347:
	movq	$-16, %rax
	subq	%rsi, %rax
	addq	%r9, %rax
	movq	%rax, 8(%rsi)
	movq	%rsi, -56(%rbp)         ## 8-byte Spill
LBB0_350:                               ## %copy.exit84.thread
	movq	-56(%rbp), %rax         ## 8-byte Reload
	addq	$176, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
	

	.globl	_remove_forwarding_pointers_from_graph
	.align	4, 0x90
_remove_forwarding_pointers_from_graph: ## @remove_forwarding_pointers_from_graph
	
## BB#0:
	pushq	%rbp
Ltmp8:
	
Ltmp9:
	
	movq	%rsp, %rbp
Ltmp10:
	
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
Ltmp11:
	
Ltmp12:
	
Ltmp13:
	
Ltmp14:
	
Ltmp15:
	
	movq	__ARRAY__@GOTPCREL(%rip), %r9
	movq	BOOL@GOTPCREL(%rip), %rax
	addq	$2, %rax
	movq	%rax, -64(%rbp)         ## 8-byte Spill
	movq	dINT@GOTPCREL(%rip), %rax
	addq	$2, %rax
	movq	%rax, -72(%rbp)         ## 8-byte Spill
	movq	REAL@GOTPCREL(%rip), %rax
	addq	$2, %rax
	movq	%rax, -80(%rbp)         ## 8-byte Spill
	movq	%rsi, %r15
	jmp	LBB1_1
	.align	4, 0x90
LBB1_143:                               ##   in Loop: Header=BB1_1 Depth=1
	movq	(%r15), %rdi
	addq	$8, %r15
LBB1_1:                                 ## %.backedge
                                        ## =>This Loop Header: Depth=1
                                        ##     Child Loop BB1_94 Depth 2
                                        ##     Child Loop BB1_97 Depth 2
                                        ##     Child Loop BB1_101 Depth 2
                                        ##     Child Loop BB1_72 Depth 2
                                        ##     Child Loop BB1_75 Depth 2
                                        ##     Child Loop BB1_79 Depth 2
                                        ##     Child Loop BB1_50 Depth 2
                                        ##       Child Loop BB1_55 Depth 3
                                        ##       Child Loop BB1_59 Depth 3
                                        ##       Child Loop BB1_62 Depth 3
                                        ##     Child Loop BB1_37 Depth 2
                                        ##     Child Loop BB1_43 Depth 2
                                        ##     Child Loop BB1_16 Depth 2
                                        ##     Child Loop BB1_22 Depth 2
                                        ##     Child Loop BB1_130 Depth 2
                                        ##     Child Loop BB1_133 Depth 2
                                        ##     Child Loop BB1_139 Depth 2
                                        ##     Child Loop BB1_112 Depth 2
                                        ##     Child Loop BB1_115 Depth 2
                                        ##     Child Loop BB1_119 Depth 2
	movq	(%rdi), %rax
	testb	$1, %al
	je	LBB1_47
## BB#2:                                ##   in Loop: Header=BB1_1 Depth=1
	movq	-1(%rax), %rax
	addq	%r9, %rax
	movq	%rax, (%rdi)
	testb	$2, %al
	jne	LBB1_3
## BB#103:                              ##   in Loop: Header=BB1_1 Depth=1
	movslq	-4(%rax), %r12
	cmpq	$2, %r12
	movl	%r12d, %r10d
	jl	LBB1_141
## BB#104:                              ##   in Loop: Header=BB1_1 Depth=1
	cmpl	$255, %r10d
	jg	LBB1_121
## BB#105:                              ##   in Loop: Header=BB1_1 Depth=1
	movl	$1, %eax
	subq	%r12, %rax
	leaq	(%r15,%rax,8), %r11
	movq	(%rdi,%r12,8), %rax
	movq	%rax, -16(%r11,%r12,8)
	cmpl	$3, %r10d
	jl	LBB1_120
## BB#106:                              ## %.lr.ph37.preheader
                                        ##   in Loop: Header=BB1_1 Depth=1
	leaq	-3(%r12), %rdx
	leaq	-2(%r12), %r10
	cmpq	$4, %r10
	jb	LBB1_118
## BB#107:                              ## %min.iters.checked
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	%r10, %r8
	andq	$-4, %r8
	je	LBB1_118
## BB#108:                              ## %vector.memcheck
                                        ##   in Loop: Header=BB1_1 Depth=1
	leaq	(,%r12,8), %rax
	negq	%rax
	leaq	8(%r15,%rax), %rax
	leaq	-8(%rdi,%r12,8), %rcx
	cmpq	%rcx, %rax
	ja	LBB1_110
## BB#109:                              ## %vector.memcheck
                                        ##   in Loop: Header=BB1_1 Depth=1
	leaq	-16(%r15), %rax
	leaq	16(%rdi), %rcx
	cmpq	%rax, %rcx
	jbe	LBB1_118
LBB1_110:                               ## %vector.body.preheader
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	%r9, %r13
	leaq	-6(%r12), %r9
	movl	%r9d, %eax
	shrl	$2, %eax
	incl	%eax
	testb	$3, %al
	movl	$0, %r14d
	je	LBB1_113
## BB#111:                              ## %vector.body.prol.preheader
                                        ##   in Loop: Header=BB1_1 Depth=1
	leaq	-24(%r15), %rbx
	leaq	-16(%rdi,%r12,8), %rax
	leal	-6(%r12), %ecx
	shrl	$2, %ecx
	incl	%ecx
	andl	$3, %ecx
	negq	%rcx
	xorl	%r14d, %r14d
	.align	4, 0x90
LBB1_112:                               ## %vector.body.prol
                                        ##   Parent Loop BB1_1 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%rax), %xmm0
	movups	(%rax), %xmm1
	movups	%xmm1, (%rbx)
	movups	%xmm0, -16(%rbx)
	addq	$4, %r14
	addq	$-32, %rbx
	addq	$-32, %rax
	incq	%rcx
	jne	LBB1_112
LBB1_113:                               ## %vector.body.preheader.split
                                        ##   in Loop: Header=BB1_1 Depth=1
	cmpq	$12, %r9
	jb	LBB1_116
## BB#114:                              ## %vector.body.preheader.split.split
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	%r8, %rbx
	subq	%r14, %rbx
	leaq	(,%r14,8), %rcx
	movq	%r15, %rax
	subq	%rcx, %rax
	addq	$-24, %rax
	movq	%r12, %rcx
	subq	%r14, %rcx
	leaq	-16(%rdi,%rcx,8), %rcx
	.align	4, 0x90
LBB1_115:                               ## %vector.body
                                        ##   Parent Loop BB1_1 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%rcx), %xmm0
	movups	(%rcx), %xmm1
	movups	%xmm1, (%rax)
	movups	%xmm0, -16(%rax)
	movups	-48(%rcx), %xmm0
	movups	-32(%rcx), %xmm1
	movups	%xmm1, -32(%rax)
	movups	%xmm0, -48(%rax)
	movups	-80(%rcx), %xmm0
	movups	-64(%rcx), %xmm1
	movups	%xmm1, -64(%rax)
	movups	%xmm0, -80(%rax)
	movups	-112(%rcx), %xmm0
	movups	-96(%rcx), %xmm1
	movups	%xmm1, -96(%rax)
	movups	%xmm0, -112(%rax)
	addq	$-128, %rax
	addq	$-128, %rcx
	addq	$-16, %rbx
	jne	LBB1_115
LBB1_116:                               ## %middle.block
                                        ##   in Loop: Header=BB1_1 Depth=1
	cmpq	%r8, %r10
	movq	%r13, %r9
	je	LBB1_120
## BB#117:                              ##   in Loop: Header=BB1_1 Depth=1
	subq	%r8, %rdx
LBB1_118:                               ## %.lr.ph37.preheader279
                                        ##   in Loop: Header=BB1_1 Depth=1
	incq	%rdx
	shlq	$3, %r12
	subq	%r12, %r15
	.align	4, 0x90
LBB1_119:                               ## %.lr.ph37
                                        ##   Parent Loop BB1_1 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	8(%rdi,%rdx,8), %rax
	movq	%rax, (%r15,%rdx,8)
	decq	%rdx
	jg	LBB1_119
LBB1_120:                               ## %._crit_edge.38
                                        ##   in Loop: Header=BB1_1 Depth=1
	addq	$8, %rdi
	movq	(%rdi), %rdi
	movq	%r11, %r15
	jmp	LBB1_1
	.align	4, 0x90
LBB1_3:                                 ##   in Loop: Header=BB1_1 Depth=1
	movzwl	-2(%rax), %r14d
	cmpq	$2, %r14
	je	LBB1_85
## BB#4:                                ##   in Loop: Header=BB1_1 Depth=1
	movzwl	%r14w, %ecx
	cmpl	$1, %ecx
	je	LBB1_142
## BB#5:                                ##   in Loop: Header=BB1_1 Depth=1
	testw	%r14w, %r14w
	jne	LBB1_65
## BB#6:                                ##   in Loop: Header=BB1_1 Depth=1
	leaq	2(%r9), %rcx
	cmpq	%rcx, %rax
	jne	LBB1_47
## BB#7:                                ##   in Loop: Header=BB1_1 Depth=1
	movq	16(%rdi), %rax
	testq	%rax, %rax
	je	LBB1_8
## BB#24:                               ##   in Loop: Header=BB1_1 Depth=1
	cmpq	-64(%rbp), %rax         ## 8-byte Folded Reload
	je	LBB1_47
## BB#25:                               ##   in Loop: Header=BB1_1 Depth=1
	cmpq	-72(%rbp), %rax         ## 8-byte Folded Reload
	je	LBB1_47
## BB#26:                               ##   in Loop: Header=BB1_1 Depth=1
	cmpq	-80(%rbp), %rax         ## 8-byte Folded Reload
	je	LBB1_47
## BB#27:                               ##   in Loop: Header=BB1_1 Depth=1
	movzwl	(%rax), %ecx
	testq	%rcx, %rcx
	je	LBB1_47
## BB#28:                               ##   in Loop: Header=BB1_1 Depth=1
	movzwl	-2(%rax), %eax
	leaq	-256(%rax), %rdx
	movq	8(%rdi), %r14
	cmpq	%rdx, %rcx
	jne	LBB1_45
## BB#29:                               ##   in Loop: Header=BB1_1 Depth=1
	movq	%rcx, %rbx
	imulq	%r14, %rbx
	leaq	(,%rbx,8), %rax
	movq	%r15, %r10
	subq	%rax, %r10
	testq	%rbx, %rbx
	jle	LBB1_30
## BB#31:                               ## %.lr.ph30.preheader
                                        ##   in Loop: Header=BB1_1 Depth=1
	imulq	%r14, %rcx
	cmpq	$2, %rcx
	movl	$1, %eax
	cmovlq	%rcx, %rax
	movl	$1, %r11d
	subq	%rax, %r11
	addq	%rcx, %r11
	cmpq	$4, %r11
	jb	LBB1_42
## BB#32:                               ## %min.iters.checked220
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	%r9, %r13
	movq	%r11, %r8
	andq	$-4, %r8
	movq	%r11, %r9
	andq	$-4, %r9
	je	LBB1_41
## BB#33:                               ## %vector.body215.preheader
                                        ##   in Loop: Header=BB1_1 Depth=1
	cmpq	$2, %rcx
	movl	$1, %eax
	cmovlq	%rcx, %rax
	movl	$1, %edx
	subq	%rax, %rdx
	leaq	-4(%rdx,%rcx), %rdx
	movq	%rdx, %rax
	shrq	$2, %rax
	btq	$2, %rdx
	movl	$0, %r12d
	jb	LBB1_35
## BB#34:                               ## %vector.body215.prol
                                        ##   in Loop: Header=BB1_1 Depth=1
	movups	8(%rdi,%rbx,8), %xmm0
	movups	-8(%rdi,%rbx,8), %xmm1
	movups	%xmm0, -16(%r10,%rbx,8)
	movups	%xmm1, -32(%r10,%rbx,8)
	movl	$4, %r12d
LBB1_35:                                ## %vector.body215.preheader.split
                                        ##   in Loop: Header=BB1_1 Depth=1
	testq	%rax, %rax
	je	LBB1_38
## BB#36:                               ## %vector.body215.preheader.split.split
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	%rcx, %rax
	notq	%rax
	cmpq	$-3, %rax
	movq	$-2, %rdx
	cmovleq	%rdx, %rax
	leaq	2(%rax,%rcx), %r14
	andq	$-4, %r14
	subq	%r12, %r14
	movq	%rcx, %rax
	subq	%r12, %rax
	leaq	8(%rdi,%rax,8), %rax
	shlq	$3, %r12
	movq	%r15, %rdx
	subq	%r12, %rdx
	addq	$-16, %rdx
	.align	4, 0x90
LBB1_37:                                ## %vector.body215
                                        ##   Parent Loop BB1_1 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%rax), %xmm0
	movups	(%rax), %xmm1
	movups	%xmm1, (%rdx)
	movups	%xmm0, -16(%rdx)
	movups	-48(%rax), %xmm0
	movups	-32(%rax), %xmm1
	movups	%xmm1, -32(%rdx)
	movups	%xmm0, -48(%rdx)
	addq	$-64, %rax
	addq	$-64, %rdx
	addq	$-8, %r14
	jne	LBB1_37
LBB1_38:                                ## %middle.block216
                                        ##   in Loop: Header=BB1_1 Depth=1
	cmpq	%r9, %r11
	jne	LBB1_40
## BB#39:                               ##   in Loop: Header=BB1_1 Depth=1
	movq	%r10, %r15
	movq	%r13, %r9
	jmp	LBB1_47
LBB1_141:                               ##   in Loop: Header=BB1_1 Depth=1
	testl	%r10d, %r10d
	jne	LBB1_142
	jmp	LBB1_47
LBB1_121:                               ##   in Loop: Header=BB1_1 Depth=1
	movq	%r12, %r14
	sarq	$8, %r14
	movzbl	%r12b, %edx
	subq	%r14, %rdx
	je	LBB1_47
## BB#122:                              ##   in Loop: Header=BB1_1 Depth=1
	cmpq	$2, %rdx
	jl	LBB1_142
## BB#123:                              ##   in Loop: Header=BB1_1 Depth=1
	movl	$1, %eax
	subq	%rdx, %rax
	leaq	(%r15,%rax,8), %r13
	movq	(%rdi,%rdx,8), %rax
	movq	%rax, -16(%r13,%rdx,8)
	cmpq	$3, %rdx
	jl	LBB1_140
## BB#124:                              ## %.lr.ph35.preheader
                                        ##   in Loop: Header=BB1_1 Depth=1
	addq	$-3, %rdx
	leaq	2(%r14), %r11
	movzbl	%r10b, %eax
	movq	%r11, %rbx
	subq	%rax, %rbx
	cmpq	$-2, %rbx
	movq	$-1, %rcx
	cmovleq	%rcx, %rbx
	leaq	-1(%rbx,%rax), %r8
	subq	%r14, %r8
	cmpq	$4, %r8
	jb	LBB1_138
## BB#125:                              ## %min.iters.checked65
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	%r11, -48(%rbp)         ## 8-byte Spill
	movq	%r8, %r11
	andq	$-4, %r11
	je	LBB1_138
## BB#126:                              ## %vector.memcheck85
                                        ##   in Loop: Header=BB1_1 Depth=1
	movzbl	%r10b, %ebx
	movq	-48(%rbp), %rax         ## 8-byte Reload
	subq	%rbx, %rax
	cmpq	$-2, %rax
	movq	$-1, %rcx
	cmovleq	%rcx, %rax
	leaq	(%rax,%rbx), %r9
	movq	%r14, %rcx
	subq	%r9, %rcx
	leaq	(%r15,%rcx,8), %rcx
	subq	%r14, %rbx
	leaq	-8(%rdi,%rbx,8), %rbx
	cmpq	%rbx, %rcx
	ja	LBB1_128
## BB#127:                              ## %vector.memcheck85
                                        ##   in Loop: Header=BB1_1 Depth=1
	leaq	-16(%r15), %rcx
	shlq	$3, %rax
	negq	%rax
	leaq	8(%rdi,%rax), %rax
	cmpq	%rcx, %rax
	jbe	LBB1_137
LBB1_128:                               ## %vector.body61.preheader
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	%r11, -56(%rbp)         ## 8-byte Spill
	movzbl	%r10b, %eax
	movq	-48(%rbp), %rcx         ## 8-byte Reload
	subq	%rax, %rcx
	cmpq	$-2, %rcx
	movq	$-1, %rbx
	cmovleq	%rbx, %rcx
	leaq	-1(%rcx,%rax), %rcx
	subq	%r14, %rcx
	addq	$-4, %rcx
	movl	%ecx, %eax
	shrl	$2, %eax
	incl	%eax
	testb	$3, %al
	movl	$0, %eax
	je	LBB1_131
## BB#129:                              ## %vector.body61.prol.preheader
                                        ##   in Loop: Header=BB1_1 Depth=1
	leaq	-24(%r15), %rbx
	movzbl	%r10b, %eax
	movq	%rcx, -88(%rbp)         ## 8-byte Spill
	movq	%rax, %rcx
	subq	%r14, %rcx
	leaq	-16(%rdi,%rcx,8), %r9
	movq	-48(%rbp), %rcx         ## 8-byte Reload
	subq	%rax, %rcx
	movl	$-1, %eax
	cmovsl	%eax, %ecx
	movzbl	%r10b, %eax
	leal	-5(%rcx,%rax), %r11d
	movq	-88(%rbp), %rcx         ## 8-byte Reload
	subl	%r14d, %r11d
	shrl	$2, %r11d
	incl	%r11d
	andl	$3, %r11d
	negq	%r11
	xorl	%eax, %eax
	.align	4, 0x90
LBB1_130:                               ## %vector.body61.prol
                                        ##   Parent Loop BB1_1 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%r9), %xmm0
	movups	(%r9), %xmm1
	movups	%xmm1, (%rbx)
	movups	%xmm0, -16(%rbx)
	addq	$4, %rax
	addq	$-32, %rbx
	addq	$-32, %r9
	incq	%r11
	jne	LBB1_130
LBB1_131:                               ## %vector.body61.preheader.split
                                        ##   in Loop: Header=BB1_1 Depth=1
	cmpq	$12, %rcx
	jb	LBB1_134
## BB#132:                              ## %vector.body61.preheader.split.split
                                        ##   in Loop: Header=BB1_1 Depth=1
	movzbl	%r10b, %r11d
	movq	-48(%rbp), %rcx         ## 8-byte Reload
	subq	%r11, %rcx
	cmpq	$-2, %rcx
	movq	$-1, %rbx
	cmovleq	%rbx, %rcx
	leaq	-1(%rcx,%r11), %r9
	subq	%r14, %r9
	andq	$-4, %r9
	subq	%rax, %r9
	leaq	(,%rax,8), %r10
	movq	%r15, %rbx
	subq	%r10, %rbx
	addq	$-24, %rbx
	addq	%r14, %rax
	subq	%rax, %r11
	leaq	-16(%rdi,%r11,8), %rax
	.align	4, 0x90
LBB1_133:                               ## %vector.body61
                                        ##   Parent Loop BB1_1 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%rax), %xmm0
	movups	(%rax), %xmm1
	movups	%xmm1, (%rbx)
	movups	%xmm0, -16(%rbx)
	movups	-48(%rax), %xmm0
	movups	-32(%rax), %xmm1
	movups	%xmm1, -32(%rbx)
	movups	%xmm0, -48(%rbx)
	movups	-80(%rax), %xmm0
	movups	-64(%rax), %xmm1
	movups	%xmm1, -64(%rbx)
	movups	%xmm0, -80(%rbx)
	movups	-112(%rax), %xmm0
	movups	-96(%rax), %xmm1
	movups	%xmm1, -96(%rbx)
	movups	%xmm0, -112(%rbx)
	addq	$-128, %rbx
	addq	$-128, %rax
	addq	$-16, %r9
	jne	LBB1_133
LBB1_134:                               ## %middle.block62
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	-56(%rbp), %rax         ## 8-byte Reload
	cmpq	%rax, %r8
	jne	LBB1_136
## BB#135:                              ##   in Loop: Header=BB1_1 Depth=1
	movq	%r13, %r15
	movq	__ARRAY__@GOTPCREL(%rip), %r9
	jmp	LBB1_142
LBB1_65:                                ##   in Loop: Header=BB1_1 Depth=1
	cmpl	$255, %r14d
	ja	LBB1_81
## BB#66:                               ## %.lr.ph33.preheader
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	16(%rdi), %r11
	movl	$1, %eax
	subq	%r14, %rax
	leaq	(%r15,%rax,8), %r13
	movq	-16(%r11,%r14,8), %rax
	movq	%rax, -16(%r13,%r14,8)
	leaq	-3(%r14), %rcx
	movl	$2, %eax
	subq	%r14, %rax
	cmpq	$-2, %rax
	movq	$-1, %rdx
	cmovleq	%rdx, %rax
	leaq	-1(%rax,%r14), %r10
	cmpq	$4, %r10
	jb	LBB1_78
## BB#67:                               ## %min.iters.checked108
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	%r10, %r8
	andq	$-4, %r8
	je	LBB1_78
## BB#68:                               ## %vector.memcheck128
                                        ##   in Loop: Header=BB1_1 Depth=1
	movl	$2, %eax
	subq	%r14, %rax
	cmpq	$-2, %rax
	movq	$-1, %rdx
	cmovleq	%rdx, %rax
	leaq	(%rax,%r14), %rdx
	shlq	$3, %rdx
	movq	%r15, %rbx
	subq	%rdx, %rbx
	leaq	-24(%r11,%r14,8), %rdx
	cmpq	%rdx, %rbx
	ja	LBB1_70
## BB#69:                               ## %vector.memcheck128
                                        ##   in Loop: Header=BB1_1 Depth=1
	leaq	-16(%r15), %rdx
	shlq	$3, %rax
	negq	%rax
	leaq	-8(%r11,%rax), %rax
	cmpq	%rdx, %rax
	jbe	LBB1_78
LBB1_70:                                ## %vector.body103.preheader
                                        ##   in Loop: Header=BB1_1 Depth=1
	movl	$2, %eax
	subq	%r14, %rax
	cmpq	$-2, %rax
	movq	$-1, %rdx
	cmovleq	%rdx, %rax
	leaq	-5(%rax,%r14), %rbx
	movl	%ebx, %eax
	shrl	$2, %eax
	incl	%eax
	testb	$3, %al
	movl	$0, %r12d
	je	LBB1_73
## BB#71:                               ## %vector.body103.prol.preheader
                                        ##   in Loop: Header=BB1_1 Depth=1
	leaq	-24(%r15), %r9
	leaq	-32(%r11,%r14,8), %rdx
	movl	$2, %eax
	subq	%r14, %rax
	movq	%rbx, %r12
	movl	$-1, %ebx
	cmovsl	%ebx, %eax
	movq	%r12, %rbx
	leal	-5(%rax,%r14), %eax
	shrl	$2, %eax
	incl	%eax
	andl	$3, %eax
	negq	%rax
	xorl	%r12d, %r12d
	.align	4, 0x90
LBB1_72:                                ## %vector.body103.prol
                                        ##   Parent Loop BB1_1 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%rdx), %xmm0
	movups	(%rdx), %xmm1
	movups	%xmm1, (%r9)
	movups	%xmm0, -16(%r9)
	addq	$4, %r12
	addq	$-32, %r9
	addq	$-32, %rdx
	incq	%rax
	jne	LBB1_72
LBB1_73:                                ## %vector.body103.preheader.split
                                        ##   in Loop: Header=BB1_1 Depth=1
	cmpq	$12, %rbx
	jb	LBB1_76
## BB#74:                               ## %vector.body103.preheader.split.split
                                        ##   in Loop: Header=BB1_1 Depth=1
	movl	$2, %eax
	subq	%r14, %rax
	cmpq	$-2, %rax
	movq	$-1, %rdx
	cmovleq	%rdx, %rax
	leaq	-1(%rax,%r14), %rdx
	andq	$-4, %rdx
	subq	%r12, %rdx
	leaq	(,%r12,8), %rbx
	movq	%r15, %rax
	subq	%rbx, %rax
	addq	$-24, %rax
	movq	%r14, %rbx
	subq	%r12, %rbx
	leaq	-32(%r11,%rbx,8), %rbx
	.align	4, 0x90
LBB1_75:                                ## %vector.body103
                                        ##   Parent Loop BB1_1 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%rbx), %xmm0
	movups	(%rbx), %xmm1
	movups	%xmm1, (%rax)
	movups	%xmm0, -16(%rax)
	movups	-48(%rbx), %xmm0
	movups	-32(%rbx), %xmm1
	movups	%xmm1, -32(%rax)
	movups	%xmm0, -48(%rax)
	movups	-80(%rbx), %xmm0
	movups	-64(%rbx), %xmm1
	movups	%xmm1, -64(%rax)
	movups	%xmm0, -80(%rax)
	movups	-112(%rbx), %xmm0
	movups	-96(%rbx), %xmm1
	movups	%xmm1, -96(%rax)
	movups	%xmm0, -112(%rax)
	addq	$-128, %rax
	addq	$-128, %rbx
	addq	$-16, %rdx
	jne	LBB1_75
LBB1_76:                                ## %middle.block104
                                        ##   in Loop: Header=BB1_1 Depth=1
	cmpq	%r8, %r10
	movq	__ARRAY__@GOTPCREL(%rip), %r9
	je	LBB1_80
## BB#77:                               ##   in Loop: Header=BB1_1 Depth=1
	subq	%r8, %rcx
LBB1_78:                                ## %.lr.ph33.preheader282
                                        ##   in Loop: Header=BB1_1 Depth=1
	incq	%rcx
	shlq	$3, %r14
	subq	%r14, %r15
	.align	4, 0x90
LBB1_79:                                ## %.lr.ph33
                                        ##   Parent Loop BB1_1 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-8(%r11,%rcx,8), %rax
	movq	%rax, (%r15,%rcx,8)
	decq	%rcx
	testq	%rcx, %rcx
	jg	LBB1_79
LBB1_80:                                ## %._crit_edge
                                        ##   in Loop: Header=BB1_1 Depth=1
	addq	$8, %rdi
	movq	(%rdi), %rdi
	movq	%r13, %r15
	jmp	LBB1_1
LBB1_81:                                ##   in Loop: Header=BB1_1 Depth=1
	movzwl	(%rax), %r11d
	testq	%r11, %r11
	je	LBB1_47
## BB#82:                               ##   in Loop: Header=BB1_1 Depth=1
	cmpl	$2, %r11d
	jb	LBB1_142
## BB#83:                               ##   in Loop: Header=BB1_1 Depth=1
	cmpl	$2, %r11d
	jne	LBB1_88
## BB#84:                               ##   in Loop: Header=BB1_1 Depth=1
	cmpl	$258, %r14d             ## imm = 0x102
	jne	LBB1_87
LBB1_85:                                ##   in Loop: Header=BB1_1 Depth=1
	movq	16(%rdi), %rax
LBB1_86:                                ## %.loopexit21
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	%rax, -8(%r15)
	addq	$-8, %r15
	jmp	LBB1_142
LBB1_8:                                 ##   in Loop: Header=BB1_1 Depth=1
	movq	8(%rdi), %rcx
	leaq	(,%rcx,8), %r10
	movq	%r15, %r14
	subq	%r10, %r14
	testq	%rcx, %rcx
	jle	LBB1_9
## BB#10:                               ## %.lr.ph31.preheader
                                        ##   in Loop: Header=BB1_1 Depth=1
	cmpq	$2, %rcx
	movl	$1, %eax
	cmovlq	%rcx, %rax
	notq	%rax
	leaq	2(%rcx,%rax), %r11
	cmpq	$4, %r11
	jb	LBB1_21
## BB#11:                               ## %min.iters.checked194
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	%r9, %r13
	movq	%r11, %r8
	andq	$-4, %r8
	movq	%r11, %r9
	andq	$-4, %r9
	je	LBB1_20
## BB#12:                               ## %vector.body189.preheader
                                        ##   in Loop: Header=BB1_1 Depth=1
	cmpq	$2, %rcx
	movl	$1, %eax
	cmovlq	%rcx, %rax
	notq	%rax
	leaq	-2(%rcx,%rax), %rdx
	movq	%rdx, %rax
	shrq	$2, %rax
	btq	$2, %rdx
	movl	$0, %r12d
	jb	LBB1_14
## BB#13:                               ## %vector.body189.prol
                                        ##   in Loop: Header=BB1_1 Depth=1
	movups	8(%rdi,%rcx,8), %xmm0
	movups	-8(%rdi,%rcx,8), %xmm1
	movups	%xmm0, -16(%r14,%rcx,8)
	movups	%xmm1, -32(%r14,%rcx,8)
	movl	$4, %r12d
LBB1_14:                                ## %vector.body189.preheader.split
                                        ##   in Loop: Header=BB1_1 Depth=1
	testq	%rax, %rax
	je	LBB1_17
## BB#15:                               ## %vector.body189.preheader.split.split
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	%rcx, %rax
	notq	%rax
	cmpq	$-3, %rax
	movq	$-2, %rdx
	cmovleq	%rdx, %rax
	leaq	2(%rcx,%rax), %rdx
	andq	$-4, %rdx
	subq	%r12, %rdx
	movq	%rcx, %rax
	subq	%r12, %rax
	leaq	8(%rdi,%rax,8), %rbx
	shlq	$3, %r12
	movq	%r15, %rax
	subq	%r12, %rax
	addq	$-16, %rax
	.align	4, 0x90
LBB1_16:                                ## %vector.body189
                                        ##   Parent Loop BB1_1 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%rbx), %xmm0
	movups	(%rbx), %xmm1
	movups	%xmm1, (%rax)
	movups	%xmm0, -16(%rax)
	movups	-48(%rbx), %xmm0
	movups	-32(%rbx), %xmm1
	movups	%xmm1, -32(%rax)
	movups	%xmm0, -48(%rax)
	addq	$-64, %rbx
	addq	$-64, %rax
	addq	$-8, %rdx
	jne	LBB1_16
LBB1_17:                                ## %middle.block190
                                        ##   in Loop: Header=BB1_1 Depth=1
	cmpq	%r9, %r11
	jne	LBB1_19
## BB#18:                               ##   in Loop: Header=BB1_1 Depth=1
	movq	%r14, %r15
	movq	%r13, %r9
	jmp	LBB1_47
LBB1_88:                                ## %.lr.ph32.preheader
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	16(%rdi), %r14
	movl	$1, %eax
	subq	%r11, %rax
	leaq	(%r15,%rax,8), %r13
	movq	-16(%r14,%r11,8), %rax
	movq	%rax, -16(%r13,%r11,8)
	leaq	-3(%r11), %rdx
	movl	$2, %eax
	subq	%r11, %rax
	cmpq	$-2, %rax
	movq	$-1, %rcx
	cmovleq	%rcx, %rax
	leaq	-1(%rax,%r11), %r10
	cmpq	$4, %r10
	jb	LBB1_100
## BB#89:                               ## %min.iters.checked151
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	%r10, %r8
	andq	$-4, %r8
	je	LBB1_100
## BB#90:                               ## %vector.memcheck171
                                        ##   in Loop: Header=BB1_1 Depth=1
	movl	$2, %eax
	subq	%r11, %rax
	cmpq	$-2, %rax
	movq	$-1, %rcx
	cmovleq	%rcx, %rax
	leaq	(%rax,%r11), %rcx
	shlq	$3, %rcx
	movq	%r15, %rbx
	subq	%rcx, %rbx
	leaq	-24(%r14,%r11,8), %rcx
	cmpq	%rcx, %rbx
	ja	LBB1_92
## BB#91:                               ## %vector.memcheck171
                                        ##   in Loop: Header=BB1_1 Depth=1
	leaq	-16(%r15), %rcx
	shlq	$3, %rax
	negq	%rax
	leaq	-8(%r14,%rax), %rax
	cmpq	%rcx, %rax
	jbe	LBB1_100
LBB1_92:                                ## %vector.body146.preheader
                                        ##   in Loop: Header=BB1_1 Depth=1
	movl	$2, %eax
	subq	%r11, %rax
	cmpq	$-2, %rax
	movq	$-1, %rcx
	cmovleq	%rcx, %rax
	leaq	-5(%rax,%r11), %rbx
	movl	%ebx, %eax
	shrl	$2, %eax
	incl	%eax
	testb	$3, %al
	movl	$0, %r12d
	je	LBB1_95
## BB#93:                               ## %vector.body146.prol.preheader
                                        ##   in Loop: Header=BB1_1 Depth=1
	leaq	-24(%r15), %r9
	leaq	-32(%r14,%r11,8), %rcx
	movl	$2, %eax
	subq	%r11, %rax
	movq	%rbx, %r12
	movl	$-1, %ebx
	cmovsl	%ebx, %eax
	movq	%r12, %rbx
	leal	-5(%rax,%r11), %eax
	shrl	$2, %eax
	incl	%eax
	andl	$3, %eax
	negq	%rax
	xorl	%r12d, %r12d
	.align	4, 0x90
LBB1_94:                                ## %vector.body146.prol
                                        ##   Parent Loop BB1_1 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%rcx), %xmm0
	movups	(%rcx), %xmm1
	movups	%xmm1, (%r9)
	movups	%xmm0, -16(%r9)
	addq	$4, %r12
	addq	$-32, %r9
	addq	$-32, %rcx
	incq	%rax
	jne	LBB1_94
LBB1_95:                                ## %vector.body146.preheader.split
                                        ##   in Loop: Header=BB1_1 Depth=1
	cmpq	$12, %rbx
	jb	LBB1_98
## BB#96:                               ## %vector.body146.preheader.split.split
                                        ##   in Loop: Header=BB1_1 Depth=1
	movl	$2, %eax
	subq	%r11, %rax
	cmpq	$-2, %rax
	movq	$-1, %rcx
	cmovleq	%rcx, %rax
	leaq	-1(%rax,%r11), %rcx
	andq	$-4, %rcx
	subq	%r12, %rcx
	leaq	(,%r12,8), %rbx
	movq	%r15, %rax
	subq	%rbx, %rax
	addq	$-24, %rax
	movq	%r11, %rbx
	subq	%r12, %rbx
	leaq	-32(%r14,%rbx,8), %rbx
	.align	4, 0x90
LBB1_97:                                ## %vector.body146
                                        ##   Parent Loop BB1_1 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movups	-16(%rbx), %xmm0
	movups	(%rbx), %xmm1
	movups	%xmm1, (%rax)
	movups	%xmm0, -16(%rax)
	movups	-48(%rbx), %xmm0
	movups	-32(%rbx), %xmm1
	movups	%xmm1, -32(%rax)
	movups	%xmm0, -48(%rax)
	movups	-80(%rbx), %xmm0
	movups	-64(%rbx), %xmm1
	movups	%xmm1, -64(%rax)
	movups	%xmm0, -80(%rax)
	movups	-112(%rbx), %xmm0
	movups	-96(%rbx), %xmm1
	movups	%xmm1, -96(%rax)
	movups	%xmm0, -112(%rax)
	addq	$-128, %rax
	addq	$-128, %rbx
	addq	$-16, %rcx
	jne	LBB1_97
LBB1_98:                                ## %middle.block147
                                        ##   in Loop: Header=BB1_1 Depth=1
	cmpq	%r8, %r10
	movq	__ARRAY__@GOTPCREL(%rip), %r9
	je	LBB1_102
## BB#99:                               ##   in Loop: Header=BB1_1 Depth=1
	subq	%r8, %rdx
LBB1_100:                               ## %.lr.ph32.preheader284
                                        ##   in Loop: Header=BB1_1 Depth=1
	incq	%rdx
	shlq	$3, %r11
	subq	%r11, %r15
	.align	4, 0x90
LBB1_101:                               ## %.lr.ph32
                                        ##   Parent Loop BB1_1 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-8(%r14,%rdx,8), %rax
	movq	%rax, (%r15,%rdx,8)
	decq	%rdx
	testq	%rdx, %rdx
	jg	LBB1_101
LBB1_102:                               ##   in Loop: Header=BB1_1 Depth=1
	movq	%r13, %r15
	jmp	LBB1_142
LBB1_9:                                 ##   in Loop: Header=BB1_1 Depth=1
	movq	%r14, %r15
	jmp	LBB1_47
LBB1_45:                                ##   in Loop: Header=BB1_1 Depth=1
	movq	%rcx, %rdx
	negq	%rdx
	imulq	%r14, %rdx
	leaq	(%r15,%rdx,8), %rdx
	testq	%r14, %r14
	jle	LBB1_46
## BB#49:                               ## %.lr.ph
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	%rdx, -144(%rbp)        ## 8-byte Spill
	movq	%rsi, -136(%rbp)        ## 8-byte Spill
	movq	%r14, %rdx
	negq	%rdx
	imulq	%rcx, %rdx
	movq	%rdx, -88(%rbp)         ## 8-byte Spill
	leaq	(,%r14,8), %rdx
	movl	$8, %esi
	subq	%rdx, %rsi
	imulq	%rcx, %rsi
	movq	%rsi, -112(%rbp)        ## 8-byte Spill
	leaq	24(%rdi), %r8
	movq	%r8, -120(%rbp)         ## 8-byte Spill
	leaq	-2048(,%rax,8), %rsi
	leaq	16(%rdi,%rcx,8), %rax
	movq	%rax, -96(%rbp)         ## 8-byte Spill
	leaq	-1(%rcx), %r9
	movq	%r9, -104(%rbp)         ## 8-byte Spill
	movl	%ecx, %eax
	andl	$3, %eax
	movq	%rax, -56(%rbp)         ## 8-byte Spill
	movq	%rcx, %rdx
	subq	%rax, %rdx
	movq	%rdx, -128(%rbp)        ## 8-byte Spill
	movq	%r14, %rax
	imulq	%rcx, %rax
	shlq	$3, %rax
	movq	%r15, %rbx
	subq	%rax, %rbx
	leaq	(,%rcx,8), %rax
	movq	%rax, -48(%rbp)         ## 8-byte Spill
	movl	%ecx, %eax
	andl	$3, %eax
	movq	%rcx, %r13
	subq	%rax, %r13
	addq	$80, %rdi
	xorl	%r10d, %r10d
	.align	4, 0x90
LBB1_50:                                ## %.lr.ph.i.preheader
                                        ##   Parent Loop BB1_1 Depth=1
                                        ## =>  This Loop Header: Depth=2
                                        ##       Child Loop BB1_55 Depth 3
                                        ##       Child Loop BB1_59 Depth 3
                                        ##       Child Loop BB1_62 Depth 3
	cmpl	$4, %ecx
	movl	$0, %r12d
	jb	LBB1_57
## BB#51:                               ## %min.iters.checked245
                                        ##   in Loop: Header=BB1_50 Depth=2
	movq	-56(%rbp), %rax         ## 8-byte Reload
	movzwl	%ax, %eax
	cmpl	%eax, %ecx
	movl	$0, %r12d
	je	LBB1_57
## BB#52:                               ## %vector.memcheck264
                                        ##   in Loop: Header=BB1_50 Depth=2
	movq	%r14, %r9
	movq	%rcx, %r14
	imulq	%r10, %r14
	movq	-88(%rbp), %rax         ## 8-byte Reload
	leaq	(%rax,%r14), %r11
	movq	%rsi, %rdx
	imulq	%r10, %rdx
	movq	-96(%rbp), %rax         ## 8-byte Reload
	leaq	(%rax,%rdx), %r12
	leaq	(%r15,%r11,8), %rax
	cmpq	%r12, %rax
	ja	LBB1_53
## BB#54:                               ## %vector.memcheck264
                                        ##   in Loop: Header=BB1_50 Depth=2
	addq	-120(%rbp), %rdx        ## 8-byte Folded Reload
	leaq	(%r15,%r14,8), %rax
	movq	%rsi, %r11
	movq	-112(%rbp), %rsi        ## 8-byte Reload
	leaq	-8(%rsi,%rax), %rax
	movq	%r11, %rsi
	cmpq	%rax, %rdx
	movl	$0, %eax
	movl	$0, %r12d
	movq	%r9, %r14
	movq	-104(%rbp), %r9         ## 8-byte Reload
	jbe	LBB1_57
	jmp	LBB1_55
LBB1_53:                                ##   in Loop: Header=BB1_50 Depth=2
	xorl	%eax, %eax
	movq	%r9, %r14
	movq	-104(%rbp), %r9         ## 8-byte Reload
	.align	4, 0x90
LBB1_55:                                ## %vector.body241
                                        ##   Parent Loop BB1_1 Depth=1
                                        ##     Parent Loop BB1_50 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	movups	(%r8,%rax,8), %xmm0
	movups	16(%r8,%rax,8), %xmm1
	movups	%xmm0, (%rbx,%rax,8)
	movups	%xmm1, 16(%rbx,%rax,8)
	addq	$4, %rax
	cmpq	%rax, %r13
	jne	LBB1_55
## BB#56:                               ## %middle.block242
                                        ##   in Loop: Header=BB1_50 Depth=2
	movq	-56(%rbp), %rax         ## 8-byte Reload
	testw	%ax, %ax
	movq	-128(%rbp), %r12        ## 8-byte Reload
	je	LBB1_63
	.align	4, 0x90
LBB1_57:                                ## %.lr.ph.i.preheader278
                                        ##   in Loop: Header=BB1_50 Depth=2
	movl	%ecx, %eax
	subl	%r12d, %eax
	movq	%r9, %r11
	subq	%r12, %r11
	testb	$7, %al
	je	LBB1_60
## BB#58:                               ## %.lr.ph.i.prol.preheader
                                        ##   in Loop: Header=BB1_50 Depth=2
	movl	%ecx, %eax
	subl	%r12d, %eax
	andl	$7, %eax
	negq	%rax
	.align	4, 0x90
LBB1_59:                                ## %.lr.ph.i.prol
                                        ##   Parent Loop BB1_1 Depth=1
                                        ##     Parent Loop BB1_50 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	movq	(%r8,%r12,8), %rdx
	movq	%rdx, (%rbx,%r12,8)
	incq	%r12
	incq	%rax
	jne	LBB1_59
LBB1_60:                                ## %.lr.ph.i.preheader278.split
                                        ##   in Loop: Header=BB1_50 Depth=2
	cmpq	$7, %r11
	jb	LBB1_63
## BB#61:                               ## %.lr.ph.i.preheader278.split.split
                                        ##   in Loop: Header=BB1_50 Depth=2
	leaq	(%rdi,%r12,8), %rax
	.align	4, 0x90
LBB1_62:                                ## %.lr.ph.i
                                        ##   Parent Loop BB1_1 Depth=1
                                        ##     Parent Loop BB1_50 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	movq	-56(%rax), %rdx
	movq	%rdx, (%rbx,%r12,8)
	movq	-48(%rax), %rdx
	movq	%rdx, 8(%rbx,%r12,8)
	movq	-40(%rax), %rdx
	movq	%rdx, 16(%rbx,%r12,8)
	movq	-32(%rax), %rdx
	movq	%rdx, 24(%rbx,%r12,8)
	movq	-24(%rax), %rdx
	movq	%rdx, 32(%rbx,%r12,8)
	movq	-16(%rax), %rdx
	movq	%rdx, 40(%rbx,%r12,8)
	movq	-8(%rax), %rdx
	movq	%rdx, 48(%rbx,%r12,8)
	movq	(%rax), %rdx
	movq	%rdx, 56(%rbx,%r12,8)
	addq	$8, %r12
	addq	$64, %rax
	cmpq	%r12, %rcx
	jne	LBB1_62
LBB1_63:                                ## %copy.exit
                                        ##   in Loop: Header=BB1_50 Depth=2
	incq	%r10
	addq	%rsi, %r8
	addq	-48(%rbp), %rbx         ## 8-byte Folded Reload
	addq	%rsi, %rdi
	cmpq	%r14, %r10
	jne	LBB1_50
## BB#64:                               ##   in Loop: Header=BB1_1 Depth=1
	movq	-144(%rbp), %r15        ## 8-byte Reload
	movq	__ARRAY__@GOTPCREL(%rip), %r9
	movq	-136(%rbp), %rsi        ## 8-byte Reload
	jmp	LBB1_47
LBB1_87:                                ##   in Loop: Header=BB1_1 Depth=1
	movq	16(%rdi), %rax
	movq	(%rax), %rax
	jmp	LBB1_86
LBB1_30:                                ##   in Loop: Header=BB1_1 Depth=1
	movq	%r10, %r15
	jmp	LBB1_47
LBB1_19:                                ##   in Loop: Header=BB1_1 Depth=1
	subq	%r8, %rcx
LBB1_20:                                ## %.lr.ph31.preheader286
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	%r13, %r9
LBB1_21:                                ## %.lr.ph31.preheader286
                                        ##   in Loop: Header=BB1_1 Depth=1
	subq	%r10, %r15
	addq	$-8, %r15
	.align	4, 0x90
LBB1_22:                                ## %.lr.ph31
                                        ##   Parent Loop BB1_1 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	16(%rdi,%rcx,8), %rax
	movq	%rax, (%r15,%rcx,8)
	cmpq	$1, %rcx
	leaq	-1(%rcx), %rcx
	jg	LBB1_22
## BB#23:                               ##   in Loop: Header=BB1_1 Depth=1
	movq	%r14, %r15
	jmp	LBB1_47
LBB1_46:                                ##   in Loop: Header=BB1_1 Depth=1
	movq	%rdx, %r15
	jmp	LBB1_47
LBB1_136:                               ##   in Loop: Header=BB1_1 Depth=1
	subq	%rax, %rdx
LBB1_137:                               ## %.lr.ph35.preheader280
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	__ARRAY__@GOTPCREL(%rip), %r9
LBB1_138:                               ## %.lr.ph35.preheader280
                                        ##   in Loop: Header=BB1_1 Depth=1
	incq	%rdx
	movzbl	%r12b, %eax
	subq	%rax, %r14
	leaq	(%r15,%r14,8), %rax
	.align	4, 0x90
LBB1_139:                               ## %.lr.ph35
                                        ##   Parent Loop BB1_1 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	8(%rdi,%rdx,8), %rcx
	movq	%rcx, (%rax,%rdx,8)
	decq	%rdx
	testq	%rdx, %rdx
	jg	LBB1_139
LBB1_140:                               ##   in Loop: Header=BB1_1 Depth=1
	movq	%r13, %r15
	.align	4, 0x90
LBB1_142:                               ##   in Loop: Header=BB1_1 Depth=1
	addq	$8, %rdi
	movq	(%rdi), %rdi
	jmp	LBB1_1
LBB1_40:                                ##   in Loop: Header=BB1_1 Depth=1
	subq	%r8, %rbx
LBB1_41:                                ## %.lr.ph30.preheader287
                                        ##   in Loop: Header=BB1_1 Depth=1
	movq	%r13, %r9
LBB1_42:                                ## %.lr.ph30.preheader287
                                        ##   in Loop: Header=BB1_1 Depth=1
	shlq	$3, %rcx
	subq	%rcx, %r15
	addq	$-8, %r15
	.align	4, 0x90
LBB1_43:                                ## %.lr.ph30
                                        ##   Parent Loop BB1_1 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	16(%rdi,%rbx,8), %rax
	movq	%rax, (%r15,%rbx,8)
	cmpq	$1, %rbx
	leaq	-1(%rbx), %rbx
	jg	LBB1_43
## BB#44:                               ##   in Loop: Header=BB1_1 Depth=1
	movq	%r10, %r15
	.align	4, 0x90
LBB1_47:                                ## %.loopexit22
                                        ##   in Loop: Header=BB1_1 Depth=1
	cmpq	%rsi, %r15
	jne	LBB1_143
## BB#48:
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
	


.subsections_via_symbols