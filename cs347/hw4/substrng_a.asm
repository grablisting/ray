;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; file: substrng_a.asm
;
; This program searches for a child string within a parent
; string.
;
; Input: A parent string and a child string from the stack.
;
; Output: Returns the starting position of the first match.
; 	Otherwise, returns -1.
;
; Ray Peters
;
; May 21, 2010
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%include "asm_io.inc"
%define child	ebp + 12
%define parent	ebp + 8

segment .data
	NF	equ	0Ah

segment .bss
childSum	resd 1
childLength	resd 1
parentLast	resd 1
parentLength	resd 1
lastPointer	resd 1
firstPointer	resd 1

segment .text
        global  asm_main

asm_main:
        enter   0, 0

	mov	EDX, [child]		;Load child string pointer
	call	string_length_loop	;Find it's last character
	sub	EDX, [child]		;Difference results in string length
	mov	[childLength], EDX	;Store length

	mov	ECX, [childLength]	;Load length of child string
	mov	EDX, [child]		;Load child string pointer
	call	ascii_sum		;Returns ASCII sum in the EAX
	mov	[childSum], EAX		;Store ascii hash value

	mov	EDX, [parent]
	call	string_length_loop
	sub	EDX, [childLength]
	mov	[parentLast], EDX
	
	xor	EBX, EBX
	xor	ECX, ECX
	xor	EDX, EDX
	mov	EBX, [parent]

string_match:
	mov	ECX, [childLength]
	mov	EDX, EBX
	call	ascii_sum

	cmp	EAX, [childSum]
	je	return_hit

	inc	EBX
	cmp	EBX, [parentLast]
	jne	string_match
	
return_miss:
	xor	EAX, EAX
	dec	EAX
	jmp	fin
	
return_hit:
	sub	EDX, [parent]
	sub	EDX, [childLength]
	xor	EAX, EAX
	mov	EAX, EDX
	inc	EAX
	jmp	fin

string_length_loop:
	mov	EAX, [EDX]
	inc	EDX
	cmp	AL, 0
	jne	string_length_loop
	dec	EDX
	ret

ascii_sum:
	push	EBX
	xor	EAX, EAX
	xor	EBX, EBX
	call	ascii_sum_loop
	xor	EBX, EBX
	pop	EBX
	ret

ascii_sum_loop:
	mov	BL, [EDX]
	xchg	AL, BL
	xchg	BL, AL
	add	AX, BX
	inc	EDX
	loop	ascii_sum_loop
	ret
	
fin:
	leave
	ret
