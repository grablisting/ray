;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; file: htod.asm
;
; This program reads in a hexadecimal number and prints out
; the decimal equivalent.
;
; Input: A hexadecimal number (max 4 chars), letters in 
;        uppercase
;
; Output: The decimal value of the hexadecimal number.
;
; Ray Peters
;
; May 12, 2010
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%include "asm_io.inc"


ones		equ	1
sixteens	equ	16*ones
sixteen_sq	equ	16*sixteens
sixteen_cube	equ	16*sixteen_sq
multiplier	db	'1','16','256','4096'
userexit 	db	"Do you want to terminate the program (y/n): ",0
LF		equ	0Ah

segment .data
in_prompt      DB  'Please input a hexadecimal number (4 digits max): ',0
out_msg        DB  'The decimal equivalent is: ',0

segment .bss
hex_num    resb  5
temp	   resd  1

segment .text
        global  asm_main
asm_main:
        enter   0,0
        pusha

start_htod:
	mov	EAX,0
	mov	EBX,0
	mov	ECX,0
	mov	EDX,0
	call	print_nl
	mov	EAX,in_prompt
	call	print_string
	mov	EBX,hex_num

read_loop:
	mov	EAX,0
	mov	AL,0
	call	read_char
	mov	[EBX],AL
	inc	EBX
	add	CL,1
	cmp	AL,LF
	jne	read_loop

	dec	EBX
	sub	CL,1
	mov	byte[EBX],0
	mov	EBX,hex_num

conversion:
	mov	EAX,0
	mov	AL,[EBX]
	cmp	AL,0
	je	decimal_conversion
	cmp	AL,'9'
	jg	check_char
	cmp	AL,'0'
	jge	eval_number

check_char:
	cmp	AL,'F'
	jg	conversion
	jmp	eval_char

eval_char:
	sub	AL,'A'
	add	AL,10
	push	EAX
	inc	EBX
	jmp	conversion

eval_number:
	sub	AL,'0'
	push	EAX
	inc	EBX
	jmp	conversion

decimal_conversion:
	mov	EAX,0
	mov	EBX,0
	jmp	build_num

build_num:
	pop	EBX
	sub	CL,1
	cmp	CL,0
	je	output

	pop	EAX
	imul	EAX,sixteens
	add	EBX,EAX
	sub	CL,1
	cmp	CL,0
	je	output

	pop	EAX
	imul	EAX,sixteen_sq
	add	EBX,EAX
	sub	CL,1
	cmp	CL,0
	je	output

	pop	EAX
	imul	EAX,sixteen_cube
	add	EBX,EAX
	jmp	output

output:
	mov	EAX,out_msg
	call	print_string
	mov	EAX,EBX
	call	print_int

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
	jmp	start_htod

fin:
	call	print_nl
        popa
        mov	eax,0
        leave
        ret


