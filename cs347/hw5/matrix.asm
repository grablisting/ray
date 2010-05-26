


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
	input	resb	120		;Make space for user input

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

	call	pack_matrix
	call	transpose_matrix
	add	esp, 12
	jmp	fin

pack_matrix:
	mov	EAX, rows
	push	EAX
	mov	EAX, cols
	push	EAX
	mov	EAX, input
	push	EAX
	ret

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
	call	transpose_matrix
	call	print_nl
	call	print_nl

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

%define	rows	[esp + 16]
%define	cols	[esp + 12]
%define	matrix	[esp + 8]

segment .text
transpose_matrix:
	mov	EAX, hello
	call	print_string
	call	print_nl
	ret


