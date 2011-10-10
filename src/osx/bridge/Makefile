CFLAGS=-Wall
OBJ=foundation.o appkit.o GlopApp.o GlopView.o

all: glop-bridge.dylib

glop-bridge.dylib: ${OBJ}
	ld -dylib -o glop-bridge.dylib ${OBJ} -lc -framework AppKit,Carbon

clean:
	rm -f *.dylib *.o
