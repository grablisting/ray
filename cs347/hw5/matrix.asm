


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
	cbefore		db	"Coord [", 0
	cmid		db	"][", 0
	cafter		db	"]", 0

segment .bss
	input	resb	102		;Make space for user input
	trans	resb	102		;Space for new matrix

segment .text
        global  asm_main, clear_regs, transpose_matrix, find_max

asm_main:
        enter   0,0
        pusha
	call	clear_regs		;Clean house before party

	call	make_matrix
	mov	EBX, input		;Print_matrix uses address
	call	print_matrix		;in EBX as matrix to print

	mov	EAX, input		;Push address for user's matrix
	push	EAX
	mov	EAX, trans		;Push address for new matrix
	push	EAX

	call	transpose_matrix
	add	ESP, 8			;Clean up the stack pointer
	mov	EBX, trans
	call	print_matrix		;Print result

	jmp	fin

make_matrix:
	call	print_nl
	mov	EBX, input		
	XOR	EAX, EAX
	call	read_int		;Get first character for rows
	mov	[EBX], AL
	inc	EBX

	XOR	EAX, EAX
	call	read_int		;Get next character for columns
	mov	[EBX], AL
	inc	EBX
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
	call	read_int		;Get an integer
	mov	[EBX], AL		;Save it
	inc	EBX
	inc	CL
	cmp	CL, [input + 1]		;Check exit condition
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

;
;print_matrix
;
;This function will print out the presumed matrix
;loaded into the EBX register.
;
print_matrix:
	mov	EDX, EBX		;Copy the EDX
	mov	AL, 1
	mov	[EBP - 4], AL
	inc	EBX
	mov	CL, 1
	inc	EBX
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
	cmp	CL, [EDX + 1]
	jle	print_matrix_loop
	call	print_nl
	mov	CL, 1
	mov	AL, [EBP - 4]
	inc	AL
	mov	[EBP - 4], AL
	cmp	AL, [EDX]
	jle	print_matrix_loop
	ret

fin:
	call	print_nl
        popa
        mov     EAX, 0
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

segment .text
transpose_matrix:
	enter	8, 0			;Make space for 2 counters
	call	clear_regs

	mov	EDX, [trans]		;Load address for new matrix
	mov	EBX, [matrix]		;Load provided matrix

	mov	AL, [EBX]
	mov	[rows], byte 1		;Initialize row counter
	inc	EBX
	mov	CL, [EBX]
	mov	[cols], byte 1		;Initialize column counter
	inc	EBX

	mov	[EDX], CL		;Save column total as row total
	inc	EDX
	mov	[EDX], AL		;Save row total as column total
	inc	EDX

	xor	EAX, EAX
	xor	ECX, ECX

outer:
	mov	EBX, [matrix]		;reset input pointer
	inc	byte[cols]		;point to next column
	add	EBX, [cols]		;Add offset to input pointer
	mov	CL, 1			;Reset col counter

trans_loop:
	mov	AL, [EBX]		;Get a character
	mov	[EDX], AL		;Save a character
	inc	EDX
	call	inc_origin
	mov	EAX, [trans]
	mov	AL, [EAX]
	inc	CL			;Check to see if we've read enough
	cmp	CL, AL			;characters for the current row
	jle	trans_loop		;If false, continue reading

	inc	byte[rows]		;Point to next row
	mov	EAX, [trans]		;Load number of rows we need
	mov	AL, [EAX]
	cmp	[rows], AL		;Exit when we have the last row
	jle	outer
	jmp	exit

inc_origin:
	push	ECX			;Save state of ECX counter
	xor	EAX, EAX
	xor	ECX, ECX
	mov	ECX, [trans]
	mov	AL, [ECX]		;Offset the input pointer by the number
	add	EBX, EAX		;of rows in the new array
	pop	ECX			;Restore ECX
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

	mov	EDX, [getmax]
	push	EAX			;Placeholder containers
	push	EAX
	mov	BL, 0
	mov	CL, 10

scan:
	xor	EAX, EAX
	mov	AL, [EDX]		;Load character
	inc	EDX		
	cmp	AL, BL			;Compare to current maximum
	jg	newmax			;If greater than the greatest...
	loop	scan			;Loop
	jmp	done

newmax:
	pop	BL			;Get rid of old max
	pop	BL
	push	CL			;Store location
	push	AL			;Store value
	mov	BL, AL			;Save value for comparison
	jmp	loop			; Return

done:
	pop	EAX			;Get the values
	call	print_int
	pop	EAX
	xor	EDX, EDX		;Display them to users
	idiv	EAX, EDX
	call	print_int

leave:
	call	print_nl
	leave
	ret
