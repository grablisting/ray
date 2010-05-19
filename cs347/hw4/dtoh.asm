;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; file: dtoh.asm
;
; This program reads in a decimal number and prints out
; the hexidecimal equivalent.
;
; Input: The decimal value between 0 and less than 2^32 - 1
;
; Output: The hexidecimal value of the given decimal.
;
; Ray Peters
;
; May 12, 2010
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%include "asm_io.inc"

two		equ	1
userexit 	db	"Do you want to terminate the program (y/n): ",0
LF		equ	0Ah

segment .data
in_prompt      DB  'Please input a positive number (range 0..1,999,999,999): ',0
out_msg        DB  'The hexidecimal equivalent is: ',0

segment .bss
bignum		resd 1
temp	   resd  1

segment .text
        global  asm_main
asm_main:
        enter   0,0
        pusha

start_dtoh:
	mov	EAX,0
	mov	EBX,0
	mov	ECX,0
	mov	EDX,0
	call	print_nl
	mov	EAX,in_prompt
	call	print_string
	call	read_int
	mov	[bignum],EAX
	mov	dword EBX, 16

divisor_loop:
	cmp	EAX,0
	je	show_result
	mov	EDX,0
	div	EBX
	push	EDX
	add	CL,1
	jmp	divisor_loop

show_result:
	mov	EAX,out_msg
	call	print_string

output:
	cmp	CL,0
	je	done
	pop	EAX
	call	print_hex
	sub	CL,1
	jmp	output

print_hex:
	cmp	EAX,9
	jg	hex_character
	call	print_int
	ret

hex_character:
	sub	EAX,10
	add	EAX,'A'
	call	print_char
	ret

done:
	call	print_nl
	mov	EAX, userexit
	call	print_string
	mov	EAX,0
	call	read_char
	mov	DL,AL
	call	read_char
	cmp	DL,'y'
	je	fin
	cmp	DL,'Y'
	je	fin
	jmp	start_dtoh

fin:
	call	print_nl
        popa
        mov	eax,0            ; return back to C
        leave                     
        ret


