CFLAGS=-Wall
OBJ=foundation.o appkit.o GlopApp.o GlopWindowResponder.o GlopGLView.o

all: glop-bridge.dylib

glop-bridge.dylib: ${OBJ}
	ld -dylib -o glop-bridge.dylib ${OBJ} -lc -framework AppKit,Carbon,OpenGL

clean:
	rm -f *.dylib *.o
