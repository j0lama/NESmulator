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

const instruction * getInstruction(uint8_t opcode);
int getInstructionLength(const instruction * ins);
void printOpcode(const instruction * ins);

cpu * initCPU(uint8_t * prg);
uint8_t cpu_read(cpu * cpu, uint16_t addr);
void cpu_write(cpu * cpu, uint16_t addr, uint8_t value);
void resetCPU(cpu * cpu);
void freeCPU(cpu * cpu);
void printRegisters(cpu * cpu);
