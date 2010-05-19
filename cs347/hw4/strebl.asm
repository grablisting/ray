
%include "asm_io.inc"

segment .data
segment .text
	global	strebl

strebl:
	enter	0,0

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
