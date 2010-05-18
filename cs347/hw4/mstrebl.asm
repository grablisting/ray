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
read_space_msg		db	"a space",0
read_printable_msg	db	"Entering read_printable on ",0
fin_msg			db	"Entering fin",0

segment .bss
input			resd 1

segment .text
        global  asm_main

asm_main:
        enter   0,0
        pusha

start_mstrebl:
	mov	ECX, MAX_LENGTH		;Initialize maximum string length counter
	mov	EBX, input		

input_loop:
	call	read_char
	mov	[EBX], AL
	inc	EBX
	cmp	AL, NEWLINE_FLAG
	je	clean_input
	loop	input_loop

clean_input:
	push	EAX
	mov	EAX, clean_input_msg
	call	print_string
	call	print_nl
	pop	EAX

	mov	EAX, MAX_LENGTH		;Load max_length allowable
	sub	EAX, ECX		;Subtract unused positions
	call	print_int
	call	print_nl
	mov	ECX, EAX		;Store number of used positions
	mov	EBX, input		;Load a string location
	jmp	skip_blanks

get_next:
	dec	ECX			;Record retrieval
	mov	AL, [EBX]		;Otherwise, load a character
	inc 	EBX			;Point at the next character
	ret

skip_blanks:
	push	EAX
	mov	EAX, skip_blanks_msg
	call	print_string
	call	print_nl
	pop	EAX

	call	get_next		;Get the next character
	cmp	AL, 33			;If > 32, it is printable
	jge	read_printable		;If true, print it!
	jmp	skip_blanks		;Otherwise, throw it out and continue
	
read_space:
	push	EAX
	mov	EAX, read_space_msg
	call	print_string
	pop	EAX

	call	print_input		;Display current character
	call	get_next		;Get the next one
	cmp	AL, 33			;If < 33, it is not printable
	jl	skip_blanks		;If true, skip blanks

read_printable:
	push	EAX
	mov	EAX, read_printable_msg
	call	print_string
	pop	EAX

	cmp	AL, 33			;If < 33, it is not printable
	jl	read_space		;If true, print it!
	call	print_input		;Display current character
	call	get_next		;Get the next one
	jmp	read_printable		;Otherwise, continue reading_printable

print_input:
	call	print_char
	push	EAX
	mov	EAX,characters_left_msg
	call	print_string
	xchg	ECX, EAX
	call	print_int
	xchg	ECX, EAX
	pop	EAX
	call	print_nl
	cmp	ECX, 0
	je	fin
	ret

fin:
	dump_regs 1
        popa
        mov     EAX, 0
        leave
        ret




