


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; file: mstrebl.asm
;
; This program reads and cleans an input string from the user.
;
; Input: A string with a maximum of 80 characters.
;
; Output: A string without leading or duplicate blank characters.
;
; Ray Peters
;
; May 21, 2010
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%include "asm_io.inc"

segment .data
	NF	equ	0Ah

segment .bss
	input	resb	80		;Make space for 80 characters

segment .text
        global  asm_main
	extern	strebl

asm_main:
        enter   0,0
        pusha

start_mstrebl:
	mov	ECX, 80
	mov	EBX, input
	mov	EAX, 0

;
;input_loop collects and stores characters to the input string
;until it finds a Newline Flag or it collects 80 characters.
;
input_loop:
	call	read_char
	mov	[EBX], AL
	inc	EBX
	cmp	AL, NF
	je	clean_input
	loop	input_loop

clean_input:
	mov	EAX, 80
	sub	EAX, ECX		;Find length of the string
	add	EAX, EAX		;Doubling the value for use as counter
	push	EAX			;Save to stack

	mov	EAX, dword input	;Get the location of the input string
	push	EAX			;Save to stack

	call	strebl			;Call string cleaning procedure
	add	esp, 8			;Clean up the stack

	mov	EAX, input		;Load result of procedure
	call	print_string		;Display to user
	call	print_nl

fin:
        popa
        mov     EAX, 0
	leave
	ret




