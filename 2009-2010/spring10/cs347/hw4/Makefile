
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

all: mstrebl asm_io.o substrng_c

substrng_c: substrng_c.o asm_io.o substrng_a.o
	gcc $(CFLAGS) -osubstrng_c substrng_c.o asm_io.o substrng_a.o

mstrebl: driver.o asm_io.o mstrebl.o strebl.o
	gcc $(CFLAGS) -omstrebl driver.o asm_io.o mstrebl.o strebl.o

asm_io.o : asm_io.asm
	$(AS) $(ASFLAGS) -d ELF_TYPE asm_io.asm

clean:
	rm *.o
