#include <stdio.h>
#include <stdlib.h>

#include "cpu.h"
#include "cart.h"

int main(int argc, char const *argv[])
{
	char string[10];
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
		scanf("%s", string);

		/*Execute next instruction*/
		cpu_execute(cpu);
	}
	return 0;
}
