;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; file: strebl.asm
;
; This program takes a string then removes leading
; and duplicate blank characters.
;
; Input: A string length and then a string pointer on the stack.
;
; Output: Replaces the old string with the new string.
;
; Ray Peters
;
; May 21, 2010
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%include "asm_io.inc"
%define	input	ebp + 8
%define length	ebp + 12

segment .data	
	NF	equ	0Ah

segment .bss
	temp	resb	80

segment .text
	global	strebl

strebl:
        enter   0, 0
        mov     ECX, [length]
	mov	EAX, 0
	mov	EBX, [input]
	mov	EDX, temp

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
	cmp	AL, NF
	je	clean_up
	ret

clean_up:
	mov	EBX, [input]
	mov	EDX, temp
	sub	[length], ECX
	mov	ECX, [length]

copy:
	mov	AL, [EDX]
	call	print_char
	mov	[EBX], AL
	inc	EBX
	inc	EDX
	loop	copy

fin:
	leave
	ret



