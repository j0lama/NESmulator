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
uint8_t cpu_read(cpu * cpu, uint16_t addr);
void cpu_write(cpu * cpu, uint16_t addr, uint8_t value);
void cpu_execute(cpu * cpu);
void resetCPU(cpu * cpu);
void freeCPU(cpu * cpu);
void printRegisters(cpu * cpu);
