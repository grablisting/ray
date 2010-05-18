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
NEWLINE_FLAG		equ	0Ah
MAX_LENGTH		equ	80

characters_left_msg	db	", characters left ",0
input_loop_msg		db	"Entering input_loop",0
clean_input_msg		db	"Entering clean_input",0
skip_blanks_msg		db	"Entering skip_blanks",0
read_printable_msg	db	"Entering read_printable on ",0
fin_msg			db	"Entering fin",0

segment .bss
input			resd 1
output			resd 1

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
	cmp	AL, NEWLINE_FLAG
	je	clean_input
	loop	input_loop

clean_input:
	mov	EAX, 0
	mov	ECX, EBX
	mov	EBX, input
	mov	EDX, output
	call	print_nl
	call	print_nl
	jmp	skip_blanks

skip_blanks:
	call	get_next		;Get the next character
	cmp	AL, 33			;If > 32, it is printable
	jge	print_printable		;If true, print it!
	jmp	skip_blanks		;Otherwise, throw it out and continue
	
print_space:
	call	print_input		;Display current character
	call	get_next		;Get the next one
	cmp	AL, 33			;If < 33, it is not printable
	jl	skip_blanks		;If true, skip blanks
	jmp	print_printable

print_printable:
	call	print_input		;Display current character
	call	get_next		;Get the next one
	cmp	AL, 33
	jl	print_space
	jmp	print_printable		;Otherwise, continue reading_printable

get_next:
	cmp	EBX, ECX
	je	fin
	mov	AL, [EBX]		;Load a character
	inc 	EBX			;Point at the next character
	ret

print_input:
	call	print_char
	mov	[EDX], AL
	inc	EDX
	cmp	AL, 0
	je	fin
	ret

print_output:
	cmp	AL, NEWLINE_FLAG
	je	fin
	mov	AL, [EDX]
	inc	EDX
	call	print_char
	jmp	print_output

fin:
        popa
        mov     EAX, 0
        leave
        ret




