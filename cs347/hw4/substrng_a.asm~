;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; file: substrng_a.asm
;
; This program searches for a child string within a parent string
; by using the sum of it's ASCII characters as hash values.
;
; Input: A child string followed by a parent string from the stack.
;
; Output: If hit, return index of first match, else -1 for miss.
;
; Ray Peters
;
; May 21, 2010
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%include "asm_io.inc"
%define parent	ebp + 8
%define child	ebp + 12

segment .data
	NF	equ	0Ah

segment .bss
childSum	resd 1
childLength	resd 1
exitCond	resd 1

segment .text
        global  asm_main

asm_main:
        enter   0, 0

	mov	EDX, [child]		;Load child string pointer
	call	string_length_loop	;Find location of last character
	sub	EDX, [child]
	mov	[childLength], EDX	;Store length

	mov	ECX, [childLength]	;Set childLength as counter
	mov	EDX, [child]		;Load child string pointer
	call	ascii_sum
	mov	[childSum], EAX		;Store ascii sum as hash value

	mov	EDX, [parent]		;Load the parent string pointer
	call	string_length_loop	;Find location of last character
	sub	EDX, [childLength]	;Determine index of the last feasible match
	inc	EDX			;Add one to match up until the null terminator
	mov	[exitCond], EDX		;Save position for use as maximum iterations
	
	xor	EBX, EBX		;Cleaning house, before the party!
	xor	ECX, ECX
	xor	EDX, EDX
	mov	EBX, [parent]		;Load parent string; start party!

string_match:
	mov	ECX, [childLength]	;Reset ECX counter to childLength
	mov	EDX, EBX		;Set EDX to start from next position
	call	ascii_sum		;Find sum of Parent[EDX..EDX+childLength]

	cmp	EAX, [childSum]		;Compare sum(Parent[EDX..EDX+childLength]) to childSum
	je	return_hit		;If equal, MATCH FOUND!

	inc	EBX
	cmp	EBX, [exitCond]
	jne	string_match		;If not at last possible match position then repeat
	
return_miss:
	xor	EAX, EAX
	dec	EAX			;Return -1 to show no match could be made
	jmp	fin

return_hit:
	sub	EDX, [parent]		;Get index of last char match
	sub	EDX, [childLength]	;Get index of first char match
	xor	EAX, EAX
	mov	EAX, EDX		;Set return value
	inc	EAX			;Add 1 to account for index start at 1
	jmp	fin

;
;string_length_loop
;
;This function scrolls through the EDX until it finds a null terminator.
;The EDX returns the position of the first null terminator's predecessor.
;

string_length_loop:
	mov	EAX, [EDX]		;Load character at EDX
	inc	EDX			;Point to next character
	cmp	AL, 0
	jne	string_length_loop	;Continue while AL != null terminator
	dec	EDX			;Point to predecessor
	ret

;
;ascii_sum
;
;This function uses ECX as a loop counter and the EDX as the string pointer
;The EAX returns the sum of the values of the string between EDX and EDX+ECX.
;

ascii_sum:
	push	EBX			;Save EBX
	xor	EAX, EAX
	xor	EBX, EBX
	call	ascii_sum_loop		;Get the sum
	xor	EBX, EBX
	pop	EBX			;Restore EBX
	ret

ascii_sum_loop:
	mov	BL, [EDX]		;Get a character
	add	AX, BX			;Add the value to the sum
	inc	EDX			;Point to next character
	loop	ascii_sum_loop		;Repeat until ECX is 0
	ret
	
fin:
	leave
	ret
