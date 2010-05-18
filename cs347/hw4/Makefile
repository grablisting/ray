#
# Linux makefile
# Use with make 
#

.SUFFIXES:
.SUFFIXES: .o .asm .cpp .c

AS=nasm
ASFLAGS= -f elf
PROGRAMS= 
CFLAGS= -Wall -m32 driver.c asm_io.o
CC=gcc

.asm.o:
	$(AS) $(ASFLAGS) $*.asm

all:
	make mstrebl

mstrebl : mstrebl.o
	gcc $(CFLAGS) -o $@ $?

clean :
	rm *.o
