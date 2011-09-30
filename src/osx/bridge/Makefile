CFLAGS=-Wall
OBJ=foundation.o appkit.o GlopApp.o

all: glop-bridge.dylib

glop-bridge.dylib: ${OBJ}
	ld -dylib -o glop-bridge.dylib ${OBJ} -lc -framework AppKit 

clean:
	rm -f *.dylib *.o
