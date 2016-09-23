%ifidn __OUTPUT_FORMAT__,obj
section	code	use32 class=code align=64
%elifidn __OUTPUT_FORMAT__,win32
$@feat.00 equ 1
section	.text	code align=64
%else
section	.text	code
%endif
extern	_CAST_S_table0
extern	_CAST_S_table1
extern	_CAST_S_table2
extern	_CAST_S_table3
global	_CAST_encrypt
align	16
_CAST_encrypt:
L$_CAST_encrypt_begin:
	; 
	push	ebp
	push	ebx
	mov	ebx,DWORD [12+esp]
	mov	ebp,DWORD [16+esp]
	push	esi
	push	edi
	; Load the 2 words
	mov	edi,DWORD [ebx]
	mov	esi,DWORD [4+ebx]
	; Get short key flag
	mov	eax,DWORD [128+ebp]
	push	eax
	xor	eax,eax
	; round 0
	mov	edx,DWORD [ebp]
	mov	ecx,DWORD [4+ebp]
	add	edx,esi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	add	ecx,ebx
	xor	edi,ecx
	; round 1
	mov	edx,DWORD [8+ebp]
	mov	ecx,DWORD [12+ebp]
	xor	edx,edi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	xor	ecx,ebx
	xor	esi,ecx
	; round 2
	mov	edx,DWORD [16+ebp]
	mov	ecx,DWORD [20+ebp]
	sub	edx,esi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	sub	ecx,ebx
	xor	edi,ecx
	; round 3
	mov	edx,DWORD [24+ebp]
	mov	ecx,DWORD [28+ebp]
	add	edx,edi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	add	ecx,ebx
	xor	esi,ecx
	; round 4
	mov	edx,DWORD [32+ebp]
	mov	ecx,DWORD [36+ebp]
	xor	edx,esi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	xor	ecx,ebx
	xor	edi,ecx
	; round 5
	mov	edx,DWORD [40+ebp]
	mov	ecx,DWORD [44+ebp]
	sub	edx,edi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	sub	ecx,ebx
	xor	esi,ecx
	; round 6
	mov	edx,DWORD [48+ebp]
	mov	ecx,DWORD [52+ebp]
	add	edx,esi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	add	ecx,ebx
	xor	edi,ecx
	; round 7
	mov	edx,DWORD [56+ebp]
	mov	ecx,DWORD [60+ebp]
	xor	edx,edi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	xor	ecx,ebx
	xor	esi,ecx
	; round 8
	mov	edx,DWORD [64+ebp]
	mov	ecx,DWORD [68+ebp]
	sub	edx,esi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	sub	ecx,ebx
	xor	edi,ecx
	; round 9
	mov	edx,DWORD [72+ebp]
	mov	ecx,DWORD [76+ebp]
	add	edx,edi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	add	ecx,ebx
	xor	esi,ecx
	; round 10
	mov	edx,DWORD [80+ebp]
	mov	ecx,DWORD [84+ebp]
	xor	edx,esi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	xor	ecx,ebx
	xor	edi,ecx
	; round 11
	mov	edx,DWORD [88+ebp]
	mov	ecx,DWORD [92+ebp]
	sub	edx,edi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	sub	ecx,ebx
	xor	esi,ecx
	; test short key flag
	pop	edx
	or	edx,edx
	jnz	NEAR L$000cast_enc_done
	; round 12
	mov	edx,DWORD [96+ebp]
	mov	ecx,DWORD [100+ebp]
	add	edx,esi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	add	ecx,ebx
	xor	edi,ecx
	; round 13
	mov	edx,DWORD [104+ebp]
	mov	ecx,DWORD [108+ebp]
	xor	edx,edi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	xor	ecx,ebx
	xor	esi,ecx
	; round 14
	mov	edx,DWORD [112+ebp]
	mov	ecx,DWORD [116+ebp]
	sub	edx,esi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	sub	ecx,ebx
	xor	edi,ecx
	; round 15
	mov	edx,DWORD [120+ebp]
	mov	ecx,DWORD [124+ebp]
	add	edx,edi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	add	ecx,ebx
	xor	esi,ecx
L$000cast_enc_done:
	nop
	mov	eax,DWORD [20+esp]
	mov	DWORD [4+eax],edi
	mov	DWORD [eax],esi
	pop	edi
	pop	esi
	pop	ebx
	pop	ebp
	ret
extern	_CAST_S_table0
extern	_CAST_S_table1
extern	_CAST_S_table2
extern	_CAST_S_table3
global	_CAST_decrypt
align	16
_CAST_decrypt:
L$_CAST_decrypt_begin:
	; 
	push	ebp
	push	ebx
	mov	ebx,DWORD [12+esp]
	mov	ebp,DWORD [16+esp]
	push	esi
	push	edi
	; Load the 2 words
	mov	edi,DWORD [ebx]
	mov	esi,DWORD [4+ebx]
	; Get short key flag
	mov	eax,DWORD [128+ebp]
	or	eax,eax
	jnz	NEAR L$001cast_dec_skip
	xor	eax,eax
	; round 15
	mov	edx,DWORD [120+ebp]
	mov	ecx,DWORD [124+ebp]
	add	edx,esi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	add	ecx,ebx
	xor	edi,ecx
	; round 14
	mov	edx,DWORD [112+ebp]
	mov	ecx,DWORD [116+ebp]
	sub	edx,edi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	sub	ecx,ebx
	xor	esi,ecx
	; round 13
	mov	edx,DWORD [104+ebp]
	mov	ecx,DWORD [108+ebp]
	xor	edx,esi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	xor	ecx,ebx
	xor	edi,ecx
	; round 12
	mov	edx,DWORD [96+ebp]
	mov	ecx,DWORD [100+ebp]
	add	edx,edi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	add	ecx,ebx
	xor	esi,ecx
L$001cast_dec_skip:
	; round 11
	mov	edx,DWORD [88+ebp]
	mov	ecx,DWORD [92+ebp]
	sub	edx,esi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	sub	ecx,ebx
	xor	edi,ecx
	; round 10
	mov	edx,DWORD [80+ebp]
	mov	ecx,DWORD [84+ebp]
	xor	edx,edi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	xor	ecx,ebx
	xor	esi,ecx
	; round 9
	mov	edx,DWORD [72+ebp]
	mov	ecx,DWORD [76+ebp]
	add	edx,esi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	add	ecx,ebx
	xor	edi,ecx
	; round 8
	mov	edx,DWORD [64+ebp]
	mov	ecx,DWORD [68+ebp]
	sub	edx,edi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	sub	ecx,ebx
	xor	esi,ecx
	; round 7
	mov	edx,DWORD [56+ebp]
	mov	ecx,DWORD [60+ebp]
	xor	edx,esi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	xor	ecx,ebx
	xor	edi,ecx
	; round 6
	mov	edx,DWORD [48+ebp]
	mov	ecx,DWORD [52+ebp]
	add	edx,edi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	add	ecx,ebx
	xor	esi,ecx
	; round 5
	mov	edx,DWORD [40+ebp]
	mov	ecx,DWORD [44+ebp]
	sub	edx,esi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	sub	ecx,ebx
	xor	edi,ecx
	; round 4
	mov	edx,DWORD [32+ebp]
	mov	ecx,DWORD [36+ebp]
	xor	edx,edi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	xor	ecx,ebx
	xor	esi,ecx
	; round 3
	mov	edx,DWORD [24+ebp]
	mov	ecx,DWORD [28+ebp]
	add	edx,esi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	add	ecx,ebx
	xor	edi,ecx
	; round 2
	mov	edx,DWORD [16+ebp]
	mov	ecx,DWORD [20+ebp]
	sub	edx,edi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	sub	ecx,ebx
	xor	esi,ecx
	; round 1
	mov	edx,DWORD [8+ebp]
	mov	ecx,DWORD [12+ebp]
	xor	edx,esi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	add	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	xor	ecx,ebx
	xor	edi,ecx
	; round 0
	mov	edx,DWORD [ebp]
	mov	ecx,DWORD [4+ebp]
	add	edx,edi
	rol	edx,cl
	mov	ebx,edx
	xor	ecx,ecx
	mov	cl,dh
	and	ebx,255
	shr	edx,16
	xor	eax,eax
	mov	al,dh
	and	edx,255
	mov	ecx,DWORD [_CAST_S_table0+ecx*4]
	mov	ebx,DWORD [_CAST_S_table1+ebx*4]
	xor	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table2+eax*4]
	sub	ecx,ebx
	mov	ebx,DWORD [_CAST_S_table3+edx*4]
	add	ecx,ebx
	xor	esi,ecx
	nop
	mov	eax,DWORD [20+esp]
	mov	DWORD [4+eax],edi
	mov	DWORD [eax],esi
	pop	edi
	pop	esi
	pop	ebx
	pop	ebp
	ret
global	_CAST_cbc_encrypt
align	16
_CAST_cbc_encrypt:
L$_CAST_cbc_encrypt_begin:
	; 
	push	ebp
	push	ebx
	push	esi
	push	edi
	mov	ebp,DWORD [28+esp]
	; getting iv ptr from parameter 4
	mov	ebx,DWORD [36+esp]
	mov	esi,DWORD [ebx]
	mov	edi,DWORD [4+ebx]
	push	edi
	push	esi
	push	edi
	push	esi
	mov	ebx,esp
	mov	esi,DWORD [36+esp]
	mov	edi,DWORD [40+esp]
	; getting encrypt flag from parameter 5
	mov	ecx,DWORD [56+esp]
	; get and push parameter 3
	mov	eax,DWORD [48+esp]
	push	eax
	push	ebx
	cmp	ecx,0
	jz	NEAR L$002decrypt
	and	ebp,4294967288
	mov	eax,DWORD [8+esp]
	mov	ebx,DWORD [12+esp]
	jz	NEAR L$003encrypt_finish
L$004encrypt_loop:
	mov	ecx,DWORD [esi]
	mov	edx,DWORD [4+esi]
	xor	eax,ecx
	xor	ebx,edx
	bswap	eax
	bswap	ebx
	mov	DWORD [8+esp],eax
	mov	DWORD [12+esp],ebx
	call	L$_CAST_encrypt_begin
	mov	eax,DWORD [8+esp]
	mov	ebx,DWORD [12+esp]
	bswap	eax
	bswap	ebx
	mov	DWORD [edi],eax
	mov	DWORD [4+edi],ebx
	add	esi,8
	add	edi,8
	sub	ebp,8
	jnz	NEAR L$004encrypt_loop
L$003encrypt_finish:
	mov	ebp,DWORD [52+esp]
	and	ebp,7
	jz	NEAR L$005finish
	call	L$006PIC_point
L$006PIC_point:
	pop	edx
	lea	ecx,[(L$007cbc_enc_jmp_table-L$006PIC_point)+edx]
	mov	ebp,DWORD [ebp*4+ecx]
	add	ebp,edx
	xor	ecx,ecx
	xor	edx,edx
	jmp	ebp
L$008ej7:
	mov	dh,BYTE [6+esi]
	shl	edx,8
L$009ej6:
	mov	dh,BYTE [5+esi]
L$010ej5:
	mov	dl,BYTE [4+esi]
L$011ej4:
	mov	ecx,DWORD [esi]
	jmp	NEAR L$012ejend
L$013ej3:
	mov	ch,BYTE [2+esi]
	shl	ecx,8
L$014ej2:
	mov	ch,BYTE [1+esi]
L$015ej1:
	mov	cl,BYTE [esi]
L$012ejend:
	xor	eax,ecx
	xor	ebx,edx
	bswap	eax
	bswap	ebx
	mov	DWORD [8+esp],eax
	mov	DWORD [12+esp],ebx
	call	L$_CAST_encrypt_begin
	mov	eax,DWORD [8+esp]
	mov	ebx,DWORD [12+esp]
	bswap	eax
	bswap	ebx
	mov	DWORD [edi],eax
	mov	DWORD [4+edi],ebx
	jmp	NEAR L$005finish
L$002decrypt:
	and	ebp,4294967288
	mov	eax,DWORD [16+esp]
	mov	ebx,DWORD [20+esp]
	jz	NEAR L$016decrypt_finish
L$017decrypt_loop:
	mov	eax,DWORD [esi]
	mov	ebx,DWORD [4+esi]
	bswap	eax
	bswap	ebx
	mov	DWORD [8+esp],eax
	mov	DWORD [12+esp],ebx
	call	L$_CAST_decrypt_begin
	mov	eax,DWORD [8+esp]
	mov	ebx,DWORD [12+esp]
	bswap	eax
	bswap	ebx
	mov	ecx,DWORD [16+esp]
	mov	edx,DWORD [20+esp]
	xor	ecx,eax
	xor	edx,ebx
	mov	eax,DWORD [esi]
	mov	ebx,DWORD [4+esi]
	mov	DWORD [edi],ecx
	mov	DWORD [4+edi],edx
	mov	DWORD [16+esp],eax
	mov	DWORD [20+esp],ebx
	add	esi,8
	add	edi,8
	sub	ebp,8
	jnz	NEAR L$017decrypt_loop
L$016decrypt_finish:
	mov	ebp,DWORD [52+esp]
	and	ebp,7
	jz	NEAR L$005finish
	mov	eax,DWORD [esi]
	mov	ebx,DWORD [4+esi]
	bswap	eax
	bswap	ebx
	mov	DWORD [8+esp],eax
	mov	DWORD [12+esp],ebx
	call	L$_CAST_decrypt_begin
	mov	eax,DWORD [8+esp]
	mov	ebx,DWORD [12+esp]
	bswap	eax
	bswap	ebx
	mov	ecx,DWORD [16+esp]
	mov	edx,DWORD [20+esp]
	xor	ecx,eax
	xor	edx,ebx
	mov	eax,DWORD [esi]
	mov	ebx,DWORD [4+esi]
L$018dj7:
	ror	edx,16
	mov	BYTE [6+edi],dl
	shr	edx,16
L$019dj6:
	mov	BYTE [5+edi],dh
L$020dj5:
	mov	BYTE [4+edi],dl
L$021dj4:
	mov	DWORD [edi],ecx
	jmp	NEAR L$022djend
L$023dj3:
	ror	ecx,16
	mov	BYTE [2+edi],cl
	shl	ecx,16
L$024dj2:
	mov	BYTE [1+esi],ch
L$025dj1:
	mov	BYTE [esi],cl
L$022djend:
	jmp	NEAR L$005finish
L$005finish:
	mov	ecx,DWORD [60+esp]
	add	esp,24
	mov	DWORD [ecx],eax
	mov	DWORD [4+ecx],ebx
	pop	edi
	pop	esi
	pop	ebx
	pop	ebp
	ret
align	64
L$007cbc_enc_jmp_table:
dd	0
dd	L$015ej1-L$006PIC_point
dd	L$014ej2-L$006PIC_point
dd	L$013ej3-L$006PIC_point
dd	L$011ej4-L$006PIC_point
dd	L$010ej5-L$006PIC_point
dd	L$009ej6-L$006PIC_point
dd	L$008ej7-L$006PIC_point
align	64
