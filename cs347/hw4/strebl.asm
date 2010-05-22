


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; file: strebl.asm
;
; This program takes a string then removes leading
; and duplicate blank characters.
;
; Input: A string length and then a string pointer on the stack.
;
; Output: Replaces dirty string string with the new, clean string.
;
; Ray Peters
;
; May 21, 2010
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%include "asm_io.inc"
%define	input	ebp + 8			;Useful values
%define length	ebp + 12

segment .data
	NF	equ	0Ah

segment .bss
	temp	resb	80

segment .text
	global	strebl

strebl:
        enter   0, 0

        mov     ECX, [length]		;Retrieve length of input
	mov	EAX, 0
	mov	EBX, [input]		;Retrieve location of input
	mov	EDX, temp		;Load temp string, start skip_blanks

skip_blanks:
	call	get_next_char		;Get a character
	cmp	AL, 33			;See if it's printable
	jl	skip_blanks		;If false, repeat
	jmp	clean_input

clean_input:
	call	save_this_char		;Add character to temp string
	call	get_next_char		;Get a character
	cmp	AL, 33			;See if it's printable
	jl	save_a_space		;If false, jmp
	loop	clean_input		;Otherwise, repeat
	jmp	fin

get_next_char:
	cmp	ECX, 0
	jle	fin			;If remaining chars = 0, exit
	mov	AL, [EBX]		;Otherwise, get a character
	inc	EBX			;Point to next character
	dec	ECX			;Decrement remaining characters
	ret

save_a_space:
	call	save_this_char		;Add character to temp string
	call	get_next_char		;Get a character
	cmp	AL, 33			;See if it's printable
	jge	clean_input		;If it's printable, go save it
	jmp	skip_blanks		;Else, start skipping blanks

save_this_char:
	mov	[EDX], AL		;Save value to memory
	inc	EDX
	cmp	AL, NF			;See if it was a null terminator
	je	clean_up		;If true, don't return
	ret

clean_up:
	mov	EBX, [input]		;Load input string
	mov	EDX, temp		;Load temp (output) string
	sub	[length], ECX		;Save length input - length out
	mov	ECX, [length]		;Load value as counter

copy:
	mov	AL, [EDX]		;Get character from output
	mov	[EBX], AL		;Replace character from input
	inc	EBX
	inc	EDX			;Increment indexes
	loop	copy			;Continue until ECX = 0

fin:
	leave
	ret



