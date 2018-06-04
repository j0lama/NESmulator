CC				= gcc
OBJFLAGS		= -c -Wall -pedantic -ansi -g -std=c99 -D_DEBUG
all: NESmulator

NESmulator: NESmulator.c cpu.c registers.c cart.c
	@echo "Creando objetos para $@... "
	$(CC) $(OBJFLAGS) $^
	@echo "Creando ejecutable $@..."
	$(CC) $(^:.c=.o) -o $@
	@rm -f *.o
	@echo == OK ==

clean:
	@echo "Borrando objetos, ejecutables, etc."
	@rm -fr Doxyfile latex html
	@rm -f *~ *.o NESmulator

