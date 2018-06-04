#include <stdio.h>
#include <stdlib.h>

#include "cpu.h"
#include "cart.h"

int main(int argc, char const *argv[])
{
	unsigned char * code;
	char string[10];
	const instruction * ins;
	int length = 0;
	nes_cart * cart;


	cart = load_cart(argv[1]);
	if(cart == NULL)
		return 1;
	code = cart->prg_rom + 0x0000FFFC;
	while(1)
	{
		scanf("%s", string);
		printf("PC: 0x%x\n", (unsigned int)code);
		printf("Readed: 0x%x\n", *code);
		ins = getInstruction(*code);
		if(ins == NULL)
		{
			printf("Invalid operation\n");
			code += 1;
		}
		else
		{
			printOpcode(ins);
			length = getInstructionLength(ins);
		}
		code += length;
	}
	return 0;
}
