

hello: hello-world.o main.c
	gcc main.c hello-world.o -o $@

hello-world.o: hello-world.txt
	objcopy --input binary --output elf64-x86-64 --binary-architecture i386 $< $@

