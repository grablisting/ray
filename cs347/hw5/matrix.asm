


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; file: matrix.asm
;
; This program works with matrices for fun.
;
; Input: A user input matrix.
;
; Output: Prints the matrix to the screen.
;
; Ray Peters
;
; May 26, 2010
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%include "asm_io.inc"

segment .data
	NF		equ	0Ah
	hello		db	"Hello there!", NF, 0
	rowmsg		db	"Number of rows: ", 0
	colmsg		db	"Number of columns: ", 0

	cbefore		db	"Coord (", 0
	cmid		db	", ", 0
	cafter		db	")", 0

	endmsg		db	NF, "Here's your matrix: ", NF, 0
	transmsg	db	NF, "Here's your transposition: ", NF, 0

segment .bss
	rows	resb	1		;Spaces for the number of rows
	cols	resb	1		;Space for number of columns
	input	resb	100		;Make space for user input

segment .text
        global  asm_main, transpose_matrix

asm_main:
        enter   0,0
        pusha
	XOR EAX, EAX
	XOR EBX, EBX
	XOR ECX, ECX
	XOR EDX, EDX

	call	new_matrix
	call	print_matrix

	mov	EAX, cols
	push	EAX
	mov	EAX, rows
	push	EAX
	mov	EAX, input
	push	EAX
	call	transpose_matrix
	pop	EAX
	call	print_matrix
	add	ESP, 12
	jmp	fin


new_matrix:
	mov	EAX, rowmsg
	;call	print_string
	XOR	EAX, EAX
	call	read_int
	mov	[rows], AL
	mov	EAX, rows

	mov	EAX, colmsg
	;call	print_string
	XOR	EAX, EAX
	call	read_int
	mov	[cols], AL
	mov	EAX, cols

	call	read_matrix
	ret

read_matrix:
	call	clear_regs
	mov	DL, 1
	mov	EBX, input
	call	get_row
	ret

get_row:
	mov	CL, 1
	call	get_col
	inc	DL
	cmp	DL, [rows]
	jle	get_row
	ret

get_col:
	;call	print_coordinates
	call	read_int
	mov	[EBX], AL
	inc	EBX
	inc	CL
	cmp	CL, [cols]
	jle	get_col
	ret
	
print_coordinates:
	mov	EAX, cbefore
	call	print_string
	XOR	EAX, EAX
	mov	AL, DL
	call	print_int
	mov	EAX, cmid
	call	print_string
	XOR	EAX, EAX
	mov	AL, CL
	call	print_int
	mov	EAX, cafter
	call	print_string
	XOR	EAX, EAX
	ret

print_matrix:
	mov	EAX, endmsg
	call	print_string
	call	clear_regs
	mov	EBX, input
	mov	CL, 1
	mov	DL, 1
	call	print_matrix_loop
	ret
	
print_matrix_loop:
	XOR	EAX, EAX
	mov	AL, [EBX]
	call	print_int
	mov	EAX, ' '
	call	print_char
	inc	EBX

	inc	CL
	cmp	CL, [cols]
	jle	print_matrix_loop

	call	print_nl
	inc	DL
	mov	CL, 1
	cmp	DL, [rows]
	jle	print_matrix_loop
	ret

clear_regs:
	XOR	EAX, EAX
	XOR	EBX, EBX
	XOR	ECX, ECX
	XOR	EDX, EDX
	ret

fin:
	call	print_nl
	call	print_nl
	;dump_regs 5

        popa
        mov     EAX, 0
	leave
	ret



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; procedure: transposition
;
; This program works with matrices for fun.
;
; Input: A user input matrix.
;
; Output: Prints the matrix to the screen.
;
; Ray Peters
;
; May 26, 2010
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%define	dims	ebp + 12
%define	matrix	ebp + 16

segment .bss
	trows	resb	1
	tcols	resb	1
	trans	resb	100	;Space for new matrix

segment .text
transpose_matrix:
	enter	0, 0

	xor	EAX, EAX
	xor	EBX, EBX
	xor	ECX, ECX
	xor	EDX, EDX

	mov	EDX, [dims]
	mov	AL, [EDX]
	call	print_int
	call	print_nl
	mov	[tcols], AL
	inc	EDX
	mov	AL, [EDX]
	call	print_int
	call	print_nl
	mov	[trows], AL
	
	mov	EBX, trans
	mov	EDX, [matrix]
	mov	CL, AL

invert_matrix_start:
	push	ECX
	call	print_nl
	mov	CL, [tcols]

invert_matrix_loop:
	mov	AL, [EDX]
	mov	[EBX], AL
	mov	AL, [trows]
	call	print_int
	call	print_nl
	add	DL, AL
	inc	EBX

	loop	invert_matrix_loop

	;mov	EDX, [matrix]
	;inc	EDX
	pop	ECX
	loop	invert_matrix_start
	

	leave
	mov	EAX, trans
	ret

