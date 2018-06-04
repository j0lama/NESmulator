#include <stdio.h>
#include <stdlib.h>

#include "cpu.h"
#include "cart.h"

int main(int argc, char const *argv[])
{
	uint8_t code;
	char string[10];
	const instruction * ins;
	int length = 0;
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
		code = cpu_read(cpu, (cpu->regs).PC);
		ins = getInstruction(code);
		if(ins == NULL)
		{
			printf("Invalid operation\n");
			code += 1;
		}
		else
		{
			#ifdef _DEBUG
			printRegisters(cpu);
			printOpcode(ins);
			#endif
			length = getInstructionLength(ins);
		}
		(cpu->regs).PC += length;
	}
	return 0;
}
