
%ifidn __OUTPUT_FORMAT__,obj
section	code	use32 class=code align=256
%elifidn __OUTPUT_FORMAT__,win32
$@feat.00 equ 1
section	.text	code align=256
%else
section	.text	code
%endif
global	_sha256_block_data_order
align	16
_sha256_block_data_order:
L$_sha256_block_data_order_begin:
	push	ebp
	push	ebx
	push	esi
	push	edi
	mov	esi,DWORD [20+esp]
	mov	edi,DWORD [24+esp]
	mov	eax,DWORD [28+esp]
	mov	ebx,esp
	call	L$000pic_point
L$000pic_point:
	pop	ebp
	lea	ebp,[(L$001K256-L$000pic_point)+ebp]
	sub	esp,16
	and	esp,-64
	shl	eax,6
	add	eax,edi
	mov	DWORD [esp],esi
	mov	DWORD [4+esp],edi
	mov	DWORD [8+esp],eax
	mov	DWORD [12+esp],ebx
	jmp	NEAR L$002loop
align	16
L$002loop:
	mov	eax,DWORD [edi]
	mov	ebx,DWORD [4+edi]
	mov	ecx,DWORD [8+edi]
	; bswap eax
	xchg	ah,al
	ror	eax,16
	xchg	ah,al
	mov	edx,DWORD [12+edi]
	; bswap ebx
	xchg	bh,bl
	ror	ebx,16
	xchg	bh,bl
	push	eax
	; bswap ecx
	xchg	ch,cl
	ror	ecx,16
	xchg	ch,cl
	push	ebx
	; bswap edx
	xchg	dh,dl
	ror	edx,16
	xchg	dh,dl
	push	ecx
	push	edx
	mov	eax,DWORD [16+edi]
	mov	ebx,DWORD [20+edi]
	mov	ecx,DWORD [24+edi]
	; bswap eax
	xchg	ah,al
	ror	eax,16
	xchg	ah,al
	mov	edx,DWORD [28+edi]
	; bswap ebx
	xchg	bh,bl
	ror	ebx,16
	xchg	bh,bl
	push	eax
	; bswap ecx
	xchg	ch,cl
	ror	ecx,16
	xchg	ch,cl
	push	ebx
	; bswap edx
	xchg	dh,dl
	ror	edx,16
	xchg	dh,dl
	push	ecx
	push	edx
	mov	eax,DWORD [32+edi]
	mov	ebx,DWORD [36+edi]
	mov	ecx,DWORD [40+edi]
	; bswap eax
	xchg	ah,al
	ror	eax,16
	xchg	ah,al
	mov	edx,DWORD [44+edi]
	; bswap ebx
	xchg	bh,bl
	ror	ebx,16
	xchg	bh,bl
	push	eax
	; bswap ecx
	xchg	ch,cl
	ror	ecx,16
	xchg	ch,cl
	push	ebx
	; bswap edx
	xchg	dh,dl
	ror	edx,16
	xchg	dh,dl
	push	ecx
	push	edx
	mov	eax,DWORD [48+edi]
	mov	ebx,DWORD [52+edi]
	mov	ecx,DWORD [56+edi]
	; bswap eax
	xchg	ah,al
	ror	eax,16
	xchg	ah,al
	mov	edx,DWORD [60+edi]
	; bswap ebx
	xchg	bh,bl
	ror	ebx,16
	xchg	bh,bl
	push	eax
	; bswap ecx
	xchg	ch,cl
	ror	ecx,16
	xchg	ch,cl
	push	ebx
	; bswap edx
	xchg	dh,dl
	ror	edx,16
	xchg	dh,dl
	push	ecx
	push	edx
	add	edi,64
	lea	esp,[esp-36]
	mov	DWORD [104+esp],edi
	mov	eax,DWORD [esi]
	mov	ebx,DWORD [4+esi]
	mov	ecx,DWORD [8+esi]
	mov	edi,DWORD [12+esi]
	mov	DWORD [8+esp],ebx
	xor	ebx,ecx
	mov	DWORD [12+esp],ecx
	mov	DWORD [16+esp],edi
	mov	DWORD [esp],ebx
	mov	edx,DWORD [16+esi]
	mov	ebx,DWORD [20+esi]
	mov	ecx,DWORD [24+esi]
	mov	edi,DWORD [28+esi]
	mov	DWORD [24+esp],ebx
	mov	DWORD [28+esp],ecx
	mov	DWORD [32+esp],edi
align	16
L$00300_15:
	mov	ecx,edx
	mov	esi,DWORD [24+esp]
	ror	ecx,14
	mov	edi,DWORD [28+esp]
	xor	ecx,edx
	xor	esi,edi
	mov	ebx,DWORD [96+esp]
	ror	ecx,5
	and	esi,edx
	mov	DWORD [20+esp],edx
	xor	edx,ecx
	add	ebx,DWORD [32+esp]
	xor	esi,edi
	ror	edx,6
	mov	ecx,eax
	add	ebx,esi
	ror	ecx,9
	add	ebx,edx
	mov	edi,DWORD [8+esp]
	xor	ecx,eax
	mov	DWORD [4+esp],eax
	lea	esp,[esp-4]
	ror	ecx,11
	mov	esi,DWORD [ebp]
	xor	ecx,eax
	mov	edx,DWORD [20+esp]
	xor	eax,edi
	ror	ecx,2
	add	ebx,esi
	mov	DWORD [esp],eax
	add	edx,ebx
	and	eax,DWORD [4+esp]
	add	ebx,ecx
	xor	eax,edi
	add	ebp,4
	add	eax,ebx
	cmp	esi,3248222580
	jne	NEAR L$00300_15
	mov	ecx,DWORD [156+esp]
	jmp	NEAR L$00416_63
align	16
L$00416_63:
	mov	ebx,ecx
	mov	esi,DWORD [104+esp]
	ror	ecx,11
	mov	edi,esi
	ror	esi,2
	xor	ecx,ebx
	shr	ebx,3
	ror	ecx,7
	xor	esi,edi
	xor	ebx,ecx
	ror	esi,17
	add	ebx,DWORD [160+esp]
	shr	edi,10
	add	ebx,DWORD [124+esp]
	mov	ecx,edx
	xor	edi,esi
	mov	esi,DWORD [24+esp]
	ror	ecx,14
	add	ebx,edi
	mov	edi,DWORD [28+esp]
	xor	ecx,edx
	xor	esi,edi
	mov	DWORD [96+esp],ebx
	ror	ecx,5
	and	esi,edx
	mov	DWORD [20+esp],edx
	xor	edx,ecx
	add	ebx,DWORD [32+esp]
	xor	esi,edi
	ror	edx,6
	mov	ecx,eax
	add	ebx,esi
	ror	ecx,9
	add	ebx,edx
	mov	edi,DWORD [8+esp]
	xor	ecx,eax
	mov	DWORD [4+esp],eax
	lea	esp,[esp-4]
	ror	ecx,11
	mov	esi,DWORD [ebp]
	xor	ecx,eax
	mov	edx,DWORD [20+esp]
	xor	eax,edi
	ror	ecx,2
	add	ebx,esi
	mov	DWORD [esp],eax
	add	edx,ebx
	and	eax,DWORD [4+esp]
	add	ebx,ecx
	xor	eax,edi
	mov	ecx,DWORD [156+esp]
	add	ebp,4
	add	eax,ebx
	cmp	esi,3329325298
	jne	NEAR L$00416_63
	mov	esi,DWORD [356+esp]
	mov	ebx,DWORD [8+esp]
	mov	ecx,DWORD [16+esp]
	add	eax,DWORD [esi]
	add	ebx,DWORD [4+esi]
	add	edi,DWORD [8+esi]
	add	ecx,DWORD [12+esi]
	mov	DWORD [esi],eax
	mov	DWORD [4+esi],ebx
	mov	DWORD [8+esi],edi
	mov	DWORD [12+esi],ecx
	mov	eax,DWORD [24+esp]
	mov	ebx,DWORD [28+esp]
	mov	ecx,DWORD [32+esp]
	mov	edi,DWORD [360+esp]
	add	edx,DWORD [16+esi]
	add	eax,DWORD [20+esi]
	add	ebx,DWORD [24+esi]
	add	ecx,DWORD [28+esi]
	mov	DWORD [16+esi],edx
	mov	DWORD [20+esi],eax
	mov	DWORD [24+esi],ebx
	mov	DWORD [28+esi],ecx
	lea	esp,[356+esp]
	sub	ebp,256
	cmp	edi,DWORD [8+esp]
	jb	NEAR L$002loop
	mov	esp,DWORD [12+esp]
	pop	edi
	pop	esi
	pop	ebx
	pop	ebp
	ret
align	64
L$001K256:
dd	1116352408,1899447441,3049323471,3921009573,961987163,1508970993,2453635748,2870763221,3624381080,310598401,607225278,1426881987,1925078388,2162078206,2614888103,3248222580,3835390401,4022224774,264347078,604807628,770255983,1249150122,1555081692,1996064986,2554220882,2821834349,2952996808,3210313671,3336571891,3584528711,113926993,338241895,666307205,773529912,1294757372,1396182291,1695183700,1986661051,2177026350,2456956037,2730485921,2820302411,3259730800,3345764771,3516065817,3600352804,4094571909,275423344,430227734,506948616,659060556,883997877,958139571,1322822218,1537002063,1747873779,1955562222,2024104815,2227730452,2361852424,2428436474,2756734187,3204031479,3329325298
dd	66051,67438087,134810123,202182159
db	83,72,65,50,53,54,32,98,108,111,99,107,32,116,114,97
db	110,115,102,111,114,109,32,102,111,114,32,120,56,54,44,32
db	67,82,89,80,84,79,71,65,77,83,32,98,121,32,60,97
db	112,112,114,111,64,111,112,101,110,115,115,108,46,111,114,103
db	62,0
