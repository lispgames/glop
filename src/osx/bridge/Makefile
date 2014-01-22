CFLAGS=-Wall
OBJ=foundation.o appkit.o GlopApp.o GlopView.o

all: glop-bridge.dylib

glop-bridge.dylib: ${OBJ}
	ld -dylib -o glop-bridge.dylib /usr/lib/dylib1.o ${OBJ} -lc -framework AppKit,Carbon,CoreServices

clean:
	rm -f *.dylib *.o
