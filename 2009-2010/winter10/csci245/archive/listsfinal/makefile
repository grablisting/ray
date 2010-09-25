CC=g++
SRC_FILES=lists.cpp functions.cpp cons.cpp number.cpp symbol.cpp
OBJ_FILES=$(patsubst %.cpp, %.o, ${SRC_FILES})
DEP_FILES=$(patsubst %.cpp, %.dep, ${SRC_FILES})
VPATH = src
CFLAGS  = -c -g 
LDFLAGS = -g

appexp: ${OBJ_FILES}
	${CC} ${LDFLAGS} -o lists.exe ${OBJ_FILES}
%.o:%.cpp
	${CC} ${CFLAGS} -o $@ $<
clean:
	rm *.o lists.exe *.dep
include ${DEP_FILES}

%.dep: %.cpp
	@set -e; rm -f $@; \
	 ${CC} -MM $(CFLAGS) $< > $@.$$$$; \
	 sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	 rm -f $@.$$$$