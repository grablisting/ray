;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; file: mstrebl.asm
;
; This program reads an input stream and discards both leading
; and duplicate blank characters.
;
; Input: A string with a maximum of 80 characters.
;
; Output: The input string without leading nor duplicate blank characters.
;
; Ray Peters
;
; May 19, 2010
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%include "asm_io.inc"
segment .data

NEWLINE_FLAG    equ    0Ah
MAX_LENGTH    equ    80
BAD_INPUT    db    "            Hello     there:",0

segment .bss
input        resd 1

segment .text
        global  asm_main

asm_main:
        enter   0,0
        pusha

start_mstrebl:
    mov    ECX, MAX_LENGTH
    ;mov    EBX, BAD_INPUT
    jmp    clean_input

input_loop:
    call    read_char
    mov    [EBX], AL
    inc    EBX
    cmp    AL, NEWLINE_FLAG
    je    clean_input
    loop    input_loop

clean_input:
    mov    EBX, BAD_INPUT
    mov    EAX, MAX_LENGTH
    sub    EAX, ECX
    mov    ECX, EAX
    jmp    skip_blanks

get_next:
    cmp    ECX, 0
    je    fin
    mov    AL, [EBX]
    cmp    AL, NEWLINE_FLAG
    je    fin
    inc    EBX
    dec    ECX
    ret

skip_blanks:
    call    get_next
    cmp    AL, 32
    jg    read_printable
    jmp    skip_blanks
    
read_space:
    call    print_input
    call    get_next
    cmp    AL, 33
    jl    skip_blanks

read_printable:
    call    print_input
    call    get_next
    cmp    AL, 33
    jl    read_space
    jmp    read_printable
    

print_input:
    call    print_char
    ret
    
fin:
    call    print_nl
        popa
        mov     EAX, 0
        leave                     
        ret