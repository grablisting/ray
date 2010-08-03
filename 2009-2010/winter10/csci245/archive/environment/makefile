CC=g++
SRC_FILES=main.cpp cons.cpp functions.cpp number.cpp symbol.cpp sexpr.cpp environment.cpp
OBJ_FILES=$(patsubst %.cpp, %.o, ${SRC_FILES})
DEP_FILES=$(patsubst %.cpp, %.dep, ${SRC_FILES})
VPATH = src
CFLAGS  = -c -g 
LDFLAGS = -g -L. -lbignum

main: ${OBJ_FILES} libbignum.a
	${CC} ${LDFLAGS} -o main.exe ${OBJ_FILES}
%.o:%.cpp
	${CC} ${CFLAGS} -o $@ $<

libbignum.a: bignum.o
	ar cr libbignum.a bignum.o

clean:
	rm *.o main.exe *.dep
include ${DEP_FILES}

%.dep: %.cpp
	@set -e; rm -f $@; \
	 ${CC} -MM $(CFLAGS) $< > $@.$$$$; \
	 sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	 rm -f $@.$$$$