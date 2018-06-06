#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cpu.h"
#include "cart.h"

int main(int argc, char const *argv[])
{
	char string[20];
	nes_cart * cart;
	cpu * cpu;


	cart = load_cart(argv[1]);
	if(cart == NULL)
		return 1;

	/*CPU initialization*/
	cpu = initCPU(cart->prg_rom);
	/*Reset the PC*/
	resetCPU(cpu);

	while(1)
	{
		printf("> ");
		fgets(string, 20, stdin);
		if(strcmp(string, "pc\n") == 0)
		{
			printPCMemory(cpu);
		}
		else if(strcmp(string, "r\n") == 0)
		{
			printRegisters(cpu);
		}
		else if(strcmp(string, "n\n") == 0)
		{
			/*Execute next instruction*/
			cpu_execute(cpu);
		}
		else if(strcmp(string, "q\n") == 0)
		{
			freeCPU(cpu);
			freeCart(cart);
			break;
		}
		else
		{
			printf("Commands:\n");
			printf("\tpc:\tprint pc memory\n");
			printf("\tr:\tprint registers\n");
			printf("\tn:\texecute next instruction\n");
			printf("\tq:\texit\n");
		}
	}
	return 0;
}
