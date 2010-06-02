


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
	cbefore		db	"[", 0
	cmid		db	"][", 0
	cafter		db	"]", 0

segment .bss
	input	resd	102		;Make space for user input
	trans	resd	102		;Space for new matrix
	maxval	resd	1
	max_x	resd	1
	max_y	resd	1

segment .text
        global  asm_main, clear_regs, transpose_matrix, find_max, print_matrix

asm_main:
        enter   0,0
        pusha


	call	clear_regs		;Clean house before party
	call	make_matrix
	mov	EBX, input
	push	EBX			;Print_matrix uses address
	call	print_matrix		;in EBX as matrix to print
	add	ESP, 4

	mov	EAX, input		;Push address for user's matrix
	push	EAX
	mov	EAX, trans		;Push address for new matrix
	push	EAX

	call	transpose_matrix
	add	ESP, 8			;Clean up the stack pointer
	mov	EAX, trans
	push	EAX
	call	print_matrix		;Print result
	add	ESP, 4			;Clean up the stack pointer


	jmp	fin

make_matrix:
	call	print_nl
	mov	EBX, input		
	XOR	EAX, EAX
	call	read_int		;Get first character for rows
	mov	[EBX], EAX
	add	EBX, 4

	XOR	EAX, EAX
	call	read_int		;Get next character for columns
	mov	[EBX], EAX
	add	EBX, 4
	mov	DL, 0			;Set row position to 1	

	call	read_row		;Fill the table with everything else
	ret

read_row:
	mov	CL, 0			;Set column position to 1
	call	get_col			;Get next column
	inc	DL			;Increment row position
	cmp	DL, [input]		;Check exit condition
	jl	read_row
	ret

get_col:
	xor	EAX, EAX
	call	read_int		;Get an integer
	mov	[EBX], EAX		;Save it
	add	EBX, 4
	inc	CL
	cmp	CL, [input + 4]		;Check exit condition
	jl	get_col
	ret

;
;print_coordinates
;
;This function prints out the matrix coordinates in a nice format.
;
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

fin:
	call	print_nl
        popa
        mov     EAX, 0
	leave
	ret



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;print_matrix
;
;This function will print out the presumed matrix
;loaded into the EBX register.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
segment .text
print_matrix:
	enter	4, 0

	call	clear_regs
	mov	EBX, [EBP + 8]		;Copy the EDX
	mov	ECX, [EBX]
	add	EBX, 4
	mov	EAX, [EBX]
	mov	[EBP - 4], EAX
	add	EBX, 4
	mov	DL, 1

print_matrix_loop:
	XOR	EAX, EAX
	mov	EAX, [EBX]
	add	EBX, 4
	call	print_int
	mov	EAX, ' '
	call	print_char
	inc	DL
	cmp	DL, [EBP - 4]
	jle	print_matrix_loop

	call	print_nl
	mov	DL, 1
	loop	print_matrix_loop

end:
	leave
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Function: clear_regs
; 
; Zeros out all registers.
;
; Ray Peters
;
; May 28, 2010
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

segment .text
clear_regs:
	XOR	EAX, EAX
	XOR	EBX, EBX
	XOR	ECX, ECX
	XOR	EDX, EDX
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Function: transpose_matrix
;
; This program transposes a matrix supposed on the stack.
;
; Input: A pointer to a matrix, and space for a transposed
; matrix from the stack
;
; Output: Builds transposed matrix at the location specified.
;
; Ray Peters
;
; May 28, 2010
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%define matrix	ebp + 12
%define	trans	ebp + 8
%define	cols	ebp - 4
%define	rows	ebp - 8
%define	ctr	ebp - 12

segment .text
transpose_matrix:
	enter	12, 0			;Make space for 2 counters
	call	clear_regs
	call	print_nl

	mov	EBX, [matrix]		;Load provided matrix
	mov	EDX, [trans]		;Load address for new matrix

	mov	EAX, [EBX]
	mov	[rows], EAX		;Initialize row counter
	add	EBX, 4

	xor	EAX, EAX
	mov	EAX, [EBX]
	mov	[cols], EAX
	add	EBX, 4

	mov	EAX, [cols]
	mov	dword[EDX], EAX
	add	EDX, 4

	mov	EAX, [rows]
	mov	dword[EDX], EAX

	mov	dword[ctr], 0

newrow:
	mov	EDX, [trans]
	add	EDX, 8
	mov	EAX, [ctr]
	imul	EAX, 4
	add	EDX, EAX
	mov	ECX, [cols]
	call	newcol
	inc	dword [ctr]
	mov	EAX, [rows]
	cmp	[ctr], EAX
	jl	newrow
	jmp	exit

newcol:
	mov	EAX, [EBX]
	add	EBX, 4
	mov	[EDX], EAX
	mov	EAX, [rows]
	imul	EAX, 4
	add	EDX, EAX
	loop	newcol
	ret

exit:
	call	print_nl
	leave
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Function: find_max
;
; This function finds the maximum value of an array.
;
; Input: A pointer from the stack.
;
; Output: Displays the maximum element and it's indeces. 
;
; Ray Peters
;
; May 28, 2010
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%define	getmax	ebp + 4

segment .text
find_max:
	enter	0, 0
	call	clear_regs

leave:
	call	print_nl
	leave
	ret



