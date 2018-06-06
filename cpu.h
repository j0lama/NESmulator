#include <stdio.h>
#include <stdint.h>

#include "registers.h"

typedef struct _cpu
{
	registers regs;
	uint8_t * ram;
	uint8_t * prg_rom;
} cpu;

typedef struct _instruction instruction;

cpu * initCPU(uint8_t * prg);
void cpu_execute(cpu * cpu);
void resetCPU(cpu * cpu);
void freeCPU(cpu * cpu);

void printPCMemory(cpu * cpu);
void printRegisters(cpu * cpu);
void printMemory(cpu * cpu, uint16_t addr);
