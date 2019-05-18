# Makefile for the oz emulator

HDR =		oz.h std.h missing.h
SRC = 		oz.c poz.c loz.c
OBJ = 		oz.o poz.o loz.o
CC =	 	gcc
# We need -ffloat-store to help ensure consistent results for 
# floating point operations on different architectures and with 
# different optimization levels.
CFLAGS =	-Wall -ffloat-store -g -O0
LINKFLAGS =	-g

oz:		$(OBJ)
		$(CC) $(CFLAGS) $(LINKFLAGS) -o oz $(OBJ) -lm

poz.c poz.h:	poz.y
		bison -p ozyy -d poz.y -o poz.c

loz.c:		loz.l poz.h
		flex -s -t -Pozyy loz.l > loz.c

clean:
		-/bin/rm -f $(OBJ) poz.c poz.h loz.c
clobber: clean
		-/bin/rm -f oz

$(OBJ):		$(HDR)
