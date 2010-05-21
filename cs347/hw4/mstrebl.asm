;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; file: mstrebl.asm
;
; This program reads an input stream and then calls a strip blanks
; procedure.
;
; Input: A string with a maximum of 80 characters.
;
; Output: Displays the new string without leading or duplicate
; 	blank characters.
;
; Ray Peters
;
; May 21, 2010
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%include "asm_io.inc"

segment .data
	NF	equ	0Ah
	MAX	equ	80

segment .bss
	input	resb	80

segment .text
        global  asm_main
	extern	strebl

asm_main:
        enter   0,0
        pusha

start_mstrebl:
	mov	ECX, MAX
	mov	EBX, input
	mov	EAX, 0

input_loop:
	call	read_char
	mov	[EBX], AL
	inc	EBX
	cmp	AL, NF
	je	clean_input
	loop	input_loop

clean_input:
	mov	EAX, MAX
	sub	EAX, ECX
	add	EAX, EAX
	push	EAX

	mov	EAX, dword input
	push	EAX

	call	strebl
	add	esp, 8

fin:
        popa
        mov     EAX, 0
	leave
	ret




