;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; file: encrypt.asm
;
; This program reads a string and then encrypts all numeric
; values against an Encryption Table.
;
; Input: An alphanumeric string.
;
;
; Output: The same string with numbers encrypted string.
;
; Ray Peters
;
; May 12, 2010
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%include "asm_io.inc"
segment .data

LF			equ	0Ah
ENCRYPTION_KEY db 	'4','6','9','5','0','3','1','8','7','2'
userenter db		"Enter a string to encrypt: ", 0
userexit db		"Terminate? (y/n) ", 0

segment .bss
input	resd 1

segment .text
        global  asm_main

asm_main:
        enter   0,0
        pusha

start_encrypt:
	mov	EAX, userenter
	call	print_string
	mov	EBX, input
read_loop:
	mov	AL,0
	call	read_char
	mov	[EBX], AL
	inc	EBX
	cmp	AL, LF
	jne	read_loop

	dec	EBX
	mov	byte[EBX], 0
	mov	EBX, input

process_char:
	mov	AL,0
	mov	AL,[EBX]
	cmp	AL,0
	je	done
	cmp	AL,'0'
	jl	print_encrypted_char
	cmp	AL,'9'
	jg	print_encrypted_char
	sub	AL,'0'

convert_num:
	mov	DL,AL
	mov	EAX,ENCRYPTION_KEY
	add	AL,DL
	mov	AL,[EAX]

	call	print_char
	inc	EBX
	jmp	process_char

print_encrypted_char:
	call	print_char
	inc	EBX
	jmp	process_char

done:
	call	print_nl
	mov	EAX, userexit
	call	print_string
	mov	EAX, 0

	call	read_char
	mov	DL,AL
	call	read_char
	cmp	DL,'y'
	je	fin
	cmp	DL,'Y'
	je	fin
	jmp	start_encrypt

fin:
	call	print_nl
        popa
        mov     eax, 0
        leave                     
        ret


