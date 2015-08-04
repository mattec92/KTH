FLAGS = -W -g 

all:    utfil.exe

utfil.exe: nod.o program.o
	gcc $(FLAGS) -o utfil.exe nod.o program.o

program.o: program.c
	gcc $(FLAGS) -c program.c

nod.o:	nod.c nod.h
	gcc $(FLAGS) -c nod.c

clean:
	touch dumt.o utfil.exe
	rm *.o utfil.exe

