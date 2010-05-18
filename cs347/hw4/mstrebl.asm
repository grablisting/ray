;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; file: mstrebl.asm
;
; This program reads an input stream and discards both leading
; and duplicate blank characters.
;
; Input: A string with a maximum of 80 characters.
;
; Output: The input string without leading nor duplicate blank characters.
;
; Ray Peters
;
; May 19, 2010
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%include "asm_io.inc"

segment .data
NF			equ	0Ah
MAX_LENGTH		equ	80

segment .bss
input			resd 4 * MAX_LENGTH
output			resd 4 * MAX_LENGTH
total			resw 1

segment .text
        global  asm_main

asm_main:
        enter   0,0
        pusha

start_mstrebl:
	mov	ECX, MAX_LENGTH		;Initialize maximum string length counter
	mov	EBX, input
	mov	EAX, 0		

input_loop:
	call	read_char
	mov	[EBX], AL
	inc	EBX
	cmp	AL, NF
	je	init_clean
	loop	input_loop

init_clean:
	mov	EAX, MAX_LENGTH
	xchg	ECX, EAX
	sub	ECX, EAX
	mov	[total], ECX

	mov	EBX,input
	mov	EDX,output

skip_blanks:
	call	get_next_char
	cmp	AL, 33
	jl	skip_blanks
	jmp	clean_input

clean_input:
	call	save_this_char
	call	get_next_char
	cmp	AL, 33
	jl	save_a_space
	loop	clean_input

	mov	EDX, output
	jmp	fin

save_a_space:
	call	save_this_char
	call	get_next_char
	cmp	AL, 33
	jge	clean_input
	jmp	skip_blanks

get_next_char:
	mov	AL, [EBX]
	inc	EBX
	ret

save_this_char:
	mov	[EDX], AL
	inc	EDX
	ret

fin:
	mov	AL, [EDX]
	inc	EDX
	call	print_char
	cmp	AL, NF
	jne	fin

        popa
        mov     EAX, 0
        leave
        ret




