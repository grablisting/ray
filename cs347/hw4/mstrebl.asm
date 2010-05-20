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
NF		equ	0Ah
MAX_LENGTH	equ	80

segment .bss
total		resd 	1
input		resw	80
output		resw	80

segment .text
        global  asm_main
	;extern	strebl

asm_main:
        enter   0,0
        pusha

start_mstrebl:
	mov	ECX, MAX_LENGTH
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
	sub	EAX, ECX
	xchg	ECX, EAX
	add	ECX, ECX

	mov	EAX, 0
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
	jmp	fin

get_next_char:
	cmp	ECX,0
	jle	fin
	mov	AL, [EBX]
	inc	EBX
	dec	ECX
	ret

save_a_space:
	call	save_this_char
	call	get_next_char
	cmp	AL, 33
	jge	clean_input
	jmp	skip_blanks

save_this_char:
	mov	[EDX], AL
	inc	EDX
	cmp	AL,NF
	je	fin
	ret

fin:
	mov	EAX, output
	call	print_string

        popa
        mov     EAX, 0
	leave




