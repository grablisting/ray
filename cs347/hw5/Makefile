
.SUFFIXES:
.SUFFIXES: .o .asm .cpp .c

AS=nasm
ASFLAGS= -f elf
CFLAGS= -Wall -m32
CC=gcc

.asm.o:
	$(AS) $(ASFLAGS) $*.asm

.c.o:
	$(CC) -c $(CFLAGS) $*.c

all: matrix

matrix: driver.o asm_io.o matrix.o
	gcc $(CFLAGS) -omatrix driver.o asm_io.o matrix.o

asm_io.o : asm_io.asm
	$(AS) $(ASFLAGS) -d ELF_TYPE asm_io.asm

clean:
	rm *.o
