all: bridge.dylib

bridge.dylib: bridge.o
	ld -dylib -o bridge.dylib bridge.o -lc -framework AppKit 

clean:
	rm -f bridge*.dylib *.o
