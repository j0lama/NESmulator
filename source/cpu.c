#include <stdio.h>
#include <stdlib.h>
#include "cpu.h"


struct _instruction
{
	char * signature;
	char * description;
	int length;
	uint8_t code;
	void (*execute) (cpu * cpu);
	uint8_t (*addresing) (cpu * cpu);
	uint8_t cycles;
};


#define OPNUM 151

#define RAM_SIZE 0x8000

/*Private functions*/
void printRegisters(cpu * cpu);
const instruction * getInstruction(uint8_t opcode);
uint8_t cpu_read(cpu * cpu, uint16_t addr);
void cpu_write(cpu * cpu, uint16_t addr, uint8_t value);
void printRegisters(cpu * cpu);

/*Instruction functions*/
void tay(cpu * cpu);
void tax(cpu * cpu);
void tsx(cpu * cpu);
void tya(cpu * cpu);
void txa(cpu * cpu);
void txs(cpu * cpu);
void lda_nn(cpu * cpu);
void ldx_nn(cpu * cpu);
void ldy_nn(cpu * cpu);


/*INSTRUCCIONES A REVISAR EL FUNCIONAMIENTO*/




/*Opcodes*/
/*http://problemkaputt.de/everynes.htm#cpu65xxmicroprocessor*/

const instruction instructions[256] = {

	{"BRK", "Force Break", 1, brk, index_implied, 7},
	{"ORA", "'OR' Memory with accumulator", 2, ora, index_ind_x, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"ORA", "'OR' Memory with accumulator", 2, ora, index_zp, 3},
	{"ASL", "Shift Left Memory One Bit (Memory or Accumulator)", 2, asl, index_zp, 5},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"PHP", "Push Processor Status on Stack", 1, php, index_implied, 3},
	{"ORA", "'OR' Memory with accumulator", 2, ora, index_imm, 2},
	{"ASL", "Shift Left Memory One Bit (Memory or Accumulator)", 1, asl, index_accum, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"ORA", "'OR' Memory with accumulator", 3, ora, index_abs, 4},
	{"ASL", "Shift Left Memory One Bit (Memory or Accumulator)", 3, asl, index_abs, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},

	{"BPL", "Branch on Result Plus", 2, bpl, index_relative, 2},
	{"ORA", "'OR' Memory with accumulator", 2, ora, index_ind_y, 5},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"ORA", "'OR' Memory with accumulator", 2, ora, index_zp_x, 4},
	{"ASL", "Shift Left Memory One Bit (Memory or Accumulator)", 2, asl, index_zp_x, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"CLC", "Clear Carry Flag", 1, clc, index_implied, 2},
	{"ORA", "'OR' Memory with accumulator", 3, ora, index_abs_y, 4},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"ORA", "'OR' Memory with accumulator", 3, ora, index_abs_x, 4},
	{"ASL", "Shift Left Memory One Bit (Memory or Accumulator)", 3, asl, index_abs_x, 7},
	{"none", "Not implemented", 1, none, index_implied, 2},

	{"JSR", "Jump to New Location Saving Return Address", 3, jsr, index_abs, 6},
	{"AND", "'AND' Memory with Accumulator", 2, and, index_ind_x, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"BIT", "Test Bits in Memory with Accumulator", 2, bit, index_zp, 3},
	{"AND", "'AND' Memory with Accumulator", 2, and, index_zp, 3},
	{"ROL", "Rotate One Bit Left (Memory or Accumulator)", 2, rol, index_zp, 5},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"PLP", "Pull Processor Status from Stack", 1, plp, index_implied, 4},
	{"AND", "'AND' Memory with Accumulator", 2, and, index_imm, 2},
	{"ROL", "Rotate One Bit Left (Memory or Accumulator)", 1, rol, index_accum, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"BIT", "Test Bits in Memory with Accumulator", 3, bit, index_abs, 4},
	{"AND", "'AND' Memory with Accumulator", 3, and, index_abs, 4},
	{"ROL", "Rotate One Bit Left (Memory or Accumulator)", 3, rol, index_abs, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},

	{"BMI", "Branch on Result Minus", 2, bmi, index_relative, 2},
	{"AND", "'AND' Memory with Accumulator", 2, and, index_ind_y, 5},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"AND", "'AND' Memory with Accumulator", 2, and, index_zp_x, 4},
	{"ROL", "Rotate One Bit Left (Memory or Accumulator)", 2, rol, index_zp_x, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"SEC", "Set Carry Flag", 1, sec, index_implied, 2},
	{"AND", "'AND' Memory with Accumulator", 3, and, index_abs_y, 4},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"AND", "'AND' Memory with Accumulator", 3, and, index_abs_x, 4},
	{"ROL", "Rotate One Bit Left (Memory or Accumulator)", 3, rol, index_abs_x, 7},
	{"none", "Not implemented", 1, none, index_implied, 2},

	{"RTI", "Return from Interrupt", 1, rti, index_implied, 6},
	{"EOR", "'Exclusive-OR' Memory with Accumulator", 2, eor, index_ind_x, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"EOR", "'Exclusive-OR' Memory with Accumulator", 2, eor, index_zp, 3},
	{"LSR", "Shift One Bit Right (Memory or Accumulator)", 2, lsr, index_zp, 5},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"PHA", "Push Accumulator on Stack", 1, pha, index_implied, 3},
	{"EOR", "'Exclusive-OR' Memory with Accumulator", 2, eor, index_imm, 2},
	{"LSR", "Shift One Bit Right (Memory or Accumulator)", 1, lsr, index_accum, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"JMP", "Jump to New Location", 3, jmp, index_abs, 3},
	{"EOR", "'Exclusive-OR' Memory with Accumulator", 3, eor, index_abs, 4},
	{"LSR", "Shift One Bit Right (Memory or Accumulator)", 3, lsr, index_abs, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},

	{"BVC", "Branch on Overflow Clear", 2, bvc, index_relative, 2},
	{"EOR", "'Exclusive-OR' Memory with Accumulator", 2, eor, index_ind_y, 5},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"EOR", "'Exclusive-OR' Memory with Accumulator", 2, eor, index_zp_x, 4},
	{"LSR", "Shift One Bit Right (Memory or Accumulator)", 2, lsr, index_zp_x, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"CLI", "Clear Input Disable Bit", 1, cli, index_implied, 2},
	{"EOR", "'Exclusive-OR' Memory with Accumulator", 3, eor, index_abs_y, 4},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"EOR", "'Exclusive-OR' Memory with Accumulator", 3, eor, index_abs_x, 4},
	{"LSR", "Shift One Bit Right (Memory or Accumulator)", 3, lsr, index_abs_x, 7},
	{"none", "Not implemented", 1, none, index_implied, 2},

	{"RTS", "Return from Subroutine", 1, rts, index_implied, 6},
	{"ADC", "Add Memory to Accumulator with Carry", 2, adc, index_ind_x, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"ADC", "Add Memory to Accumulator with Carry", 2, adc, index_zp, 3},
	{"ROR", "Rotate One Bit Right (Memory or Accumulator)", 2, ror, index_zp, 5},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"PLA", "Pull Accumulator from Stack", 1, pla, index_implied, 4},
	{"ADC", "Add Memory to Accumulator with Carry", 2, adc, index_imm, 2},
	{"ROR", "Rotate One Bit Right (Memory or Accumulator)", 1, ror, index_accum, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"JMP", "Jump to New Location", 3, jmp, index_indirect, 5},
	{"ADC", "Add Memory to Accumulator with Carry", 3, adc, index_abs, 4},
	{"ROR", "Rotate One Bit Right (Memory or Accumulator)", 3, ror, index_abs, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},
	
	{"BVS", "Branch on Overflow Set", 2, bvs, index_relative, 2},
	{"ADC", "Add Memory to Accumulator with Carry", 2, adc, index_ind_y, 5},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"ADC", "Add Memory to Accumulator with Carry", 2, adc, index_zp_x, 4},
	{"ROR", "Rotate One Bit Right (Memory or Accumulator)", 2, ror, index_zp_x, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"SEI", "Set Interrupt Disable Status", 1, sei, index_implied, 2},
	{"ADC", "Add Memory to Accumulator with Carry", 3, adc, index_abs_y, 4},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"ADC", "Add Memory to Accumulator with Carry", 3, adc, index_abs_x, 4},
	{"ROR", "Rotate One Bit Right (Memory or Accumulator)", 3, ror, index_abs_x, 7},
	{"none", "Not implemented", 1, none, index_implied, 2},

	{"none", "Not implemented", 1, none, index_implied, 2},
	{"STA", "Store Accumulator in Memory", 2, sta, index_ind_x, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"STY", "Store Index Y in Memory", 2, sty, index_zp, 3},
	{"STA", "Store Accumulator in Memory", 2, sta, index_zp, 3},
	{"STX", "Store Index X in Memory", 2, stx, index_zp, 3},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"DEY", "Decrement Index Y by One", 1, dey, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"TXA", "Transfer Index X to Accumulator", 1, txa, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"STY", "Store Index Y in Memory", 3, sty, index_abs, 4},
	{"STA", "Store Accumulator in Memory", 3, sta, index_abs, 4},
	{"STX", "Store Index X in Memory", 3, stx, index_abs, 4},
	{"none", "Not implemented", 1, none, index_implied, 2},

	{"BCC", "Branch on Carry Clear", 2, bcc, index_relative, 2},
	{"STA", "Store Accumulator in Memory", 2, sta, index_ind_y, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"STY", "Store Index Y in Memory", 2, sty, index_zp_x, 4},
	{"STA", "Store Accumulator in Memory", 2, sta, index_zp_x, 4},
	{"STX", "Store Index X in Memory", 2, stx, index_zp_y, 4},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"TYA", "Transfer Index Y to Accumulator", 1, tya, index_implied, 2},
	{"STA", "Store Accumulator in Memory", 3, sta, index_abs_y, 5},
	{"TXS", "Transfer Index X to Stack Register", 1, txs, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"STA", "Store Accumulator in Memory", 3, sta, index_abs_x, 5},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	
	{"LDY", "Load Index Y with Memory", 2, ldy, index_imm, 2},
	{"LDA", "Load Accumulator with Memory", 2, lda, index_ind_x, 6},
	{"LDX", "Load Index X with Memory", 2, ldx, index_imm, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"LDY", "Load Index Y with Memory", 2, ldy, index_zp, 3},
	{"LDA", "Load Accumulator with Memory", 2, lda, index_zp, 3},
	{"LDX", "Load Index X with Memory", 2, ldx, index_zp, 3},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"TAY", "Transfer Accumulator to Index Y", 1, tay, index_implied, 2},
	{"LDA", "Load Accumulator with Memory", 2, lda, index_imm, 2},
	{"TAX", "Transfer Accumulator to Index X", 1, tax, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"LDY", "Load Index Y with Memory", 3, ldy, index_abs, 4},
	{"LDA", "Load Accumulator with Memory", 3, lda, index_abs, 4},
	{"LDX", "Load Index X with Memory", 3, ldx, index_abs, 4},
	{"none", "Not implemented", 1, none, index_implied, 2},

	{"BCS", "Branch on Carry Set", 2, bcs, index_relative, 2},
	{"LDA", "Load Accumulator with Memory", 2, lda, index_ind_y, 5},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"LDY", "Load Index Y with Memory", 2, ldy, index_zp_x, 4},
	{"LDA", "Load Accumulator with Memory", 2, lda, index_zp_x, 4},
	{"LDX", "Load Index X with Memory", 2, ldx, index_zp_y, 4},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"CLV", "Clear Overflow Flag", 1, clv, index_implied, 2},
	{"LDA", "Load Accumulator with Memory", 3, lda, index_abs_y, 4},
	{"TSX", "Transfer Stack Pointer to Index X", 1, tsx, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"LDY", "Load Index Y with Memory", 3, ldy, index_abs_x 4},
	{"LDA", "Load Accumulator with Memory", 3, lda, index_abs_x, 4},
	{"LDX", "Load Index X with Memory", 3, ldx, index_abs_y, 4},
	{"none", "Not implemented", 1, none, index_implied, 2},
	
	{"CPY", "Compare Memory and Index Y", 2, cpy, index_imm, 2},
	{"CMP", "Compare Memory and Accumulator", 2, cmp, index_ind_x, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"CPY", "Compare Memory and Index Y", 2, cpy, index_zp, 3},
	{"CMP", "Compare Memory and Accumulator", 2, cmp, index_zp, 3},
	{"DEC", "Decrement Memory by One", 2, dec, index_zp, 5},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"INY", "Increment Index Y by One", 1, iny, index_implied, 2},
	{"CMP", "Compare Memory and Accumulator", 2, cmp, index_imm, 2},
	{"DEX", "Decrement Index X by One", 1, dex, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"CPY", "Compare Memory and Index Y", 3, cpy, index_abs, 4},
	{"CMP", "Compare Memory and Accumulator", 3, cmp, index_abs, 4},
	{"DEC", "Decrement Memory by One", 3, dec, index_abs, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},

	{"BNE", "Branch on Result not Zero", 2, bne, index_relative, 2},
	{"CMP", "Compare Memory and Accumulator", 2, cmp, index_ind_y, 5},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"CMP", "Compare Memory and Accumulator", 2, cmp, index_zp_x, 4},
	{"DEC", "Decrement Memory by One", 2, dec, index_zp_x, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"CLD", "Clear Decimal Mode", 1, cld, index_implied, 2},
	{"CMP", "Compare Memory and Accumulator", 3, cmp, index_abs_y, 4},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"CMP", "Compare Memory and Accumulator", 3, cmp, index_abs_x, 4},
	{"DEC", "Decrement Memory by One", 3, dec, index_abs_x, 7},
	{"none", "Not implemented", 1, none, index_implied, 2},

	{"CPX", "Compare Memory and Index X", 2, cpx, index_imm, 2},
	{"SBC", "Subtract Memory from Accumulator with Borrow", 2, sbc, index_ind_x, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"CPX", "Compare Memory and Index X", 2, cpx, index_zp, 3},
	{"SBC", "Subtract Memory from Accumulator with Borrow", 2, sbc, index_zp, 3},
	{"INC", "Increment Memory by One", 2, inc, index_zp, 5},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"INX", "Increment Index X by One", 1, inx, index_implied, 2},
	{"SBC", "Subtract Memory from Accumulator with Borrow", 2, sbc, index_imm, 2},
	{"NOP", "No Operation", 1, nop, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"CPX", "Compare Memory and Index X", 3, cpx, index_abs, 4},
	{"SBC", "Subtract Memory from Accumulator with Borrow", 3, sbc, index_abs, 4},
	{"INC", "Increment Memory by One", 3, inc, index_abs, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},

	{"BEQ", "Branch on Result Zero", 2, beq, index_relative, 2},
	{"SBC", "Subtract Memory from Accumulator with Borrow", 2, sbc, index_ind_y, 5},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"SBC", "Subtract Memory from Accumulator with Borrow", 2, sbc, index_zp_x, 4},
	{"INC", "Increment Memory by One", 2, inc, index_zp_x, 6},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"SED", "Set Decimal Mode", 1, sed, index_implied, 2},
	{"SBC", "Subtract Memory from Accumulator with Borrow", 3, sbc, index_abs_y, 4},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"none", "Not implemented", 1, none, index_implied, 2},
	{"SBC", "Subtract Memory from Accumulator with Borrow", 3, sbc, index_abs_x, 4},
	{"INC", "Increment Memory by One", 3, inc, index_abs_x, 7},
	{"none", "Not implemented", 1, none, index_implied, 2},
};

/*****************************/
/* Addressing Mode functions */
/*****************************/

uint8_t index_implied(cpu * cpu)
{
	return 0;
} 

uint8_t index_imm(cpu * cpu)
{
	return 0;
} 

uint8_t index_zp(cpu * cpu)
{
	return 0;
} 

uint8_t index_zp_x(cpu * cpu)
{
	return 0;
} 

uint8_t index_zp_y(cpu * cpu)
{
	return 0;
} 

uint8_t index_relative(cpu * cpu)
{
	return 0;
} 

uint8_t index_abs(cpu * cpu)
{
	return 0;
} 

uint8_t index_abs_x(cpu * cpu)
{
	return 0;
} 

uint8_t index_abs_y(cpu * cpu)
{
	return 0;
} 

uint8_t index_indirect(cpu * cpu)
{
	return 0;
} 

uint8_t index_ind_x(cpu * cpu)
{
	return 0;
} 

uint8_t index_ind_y(cpu * cpu)
{
	return 0;
} 


void carrycalc(cpu, value)
{
	if(value > 255)
		FLAG_SET(cpu->regs, FLAG_CARRY);
	return;
}

void zerocalc(cpu, value)
{
	if((value & 0x00FF) == 0)
		FLAG_SET(cpu->regs, FLAG_ZERO)
	return;
}

void overflowcalc(cpu, value)
{
	return;
}

void signcalc(cpu * cpu, uint8_t value)
{
	/*MSB is 1*/
	if(value & 0x80)
		FLAG_SET(cpu->regs, FLAG_NEGATIVE)
	return;
}
void saveToA(cpu, value)
{
	(cpu->regs).A = value & 0x00FF;
	return;
}

/* Addressing Mode functions */
uint8_t index_implied(cpu * cpu)
{
	return 0;
} 

const instruction * getInstruction(uint8_t opcode)
{
	int i = 0;
	for(i = 0; i < OPNUM; i++)
	{
		if(instructions[i].code == opcode)
			return &instructions[i];
	}
	return NULL;
}

void printOpcode(const instruction * ins)
{
	printf("Hex:\t0x%x\n", ins->code);
	printf("Instruction:\t%s or %s\n", ins->signature, ins->auxSignature);
	printf("Length:\t%d\n", ins->length);
}

cpu * initCPU(uint8_t * prg)
{
	cpu * cpu;
	cpu = calloc(sizeof(cpu), 1);

	/*Initialize regs*/
	(cpu->regs).PC = 0x0000;
	(cpu->regs).S = 0xFD;
	(cpu->regs).A = 0;
	(cpu->regs).X = 0;
	(cpu->regs).Y = 0;
	(cpu->regs).P = 0x24;

	cpu->ram = calloc(RAM_SIZE, 1);
	cpu->prg_rom = prg;

	return cpu;
}

uint8_t cpu_read(cpu * cpu, uint16_t addr)
{
	switch(addr)
	{
		case 0x0000 ... 0x7FFF:
			return cpu->ram[addr];
		case 0x8000 ... 0xFFFF:
			return cpu->prg_rom[addr & 0x7FFF];
		default:
			printf("Invalid address: 0x%x\n", addr);
			break;
	}
	return 0;
}

void cpu_write(cpu * cpu, uint16_t addr, uint8_t value)
{
	switch(addr)
	{
		case 0x0000 ... 0x7FFF:
			cpu->ram[addr] = value;
		case 0x8000 ... 0xFFFF:
			cpu->prg_rom[addr & 0x7FFF] = value;
		default:
			printf("Invalid address: 0x%x\n", addr);
			break;
	}
	return;
}

void cpu_execute(cpu * cpu)
{
	uint8_t code;
	uint8_t extra_cycle_addressing = 0;
	uint8_t extra_cycle_execution = 0;
	uint8_t cycles = 0;
	const instruction * ins;

	/*Read instruction code*/
	code = cpu_read(cpu, (cpu->regs).PC);

	/*Get the detailed instruction*/
	ins = instructions[code];

	#ifdef _DEBUG
	printOpcode(ins);
	#endif

	/*Executing addresing mode function*/
	extra_cycle_addressing = ins->addresing(cpu);

	/*Executing instruction*/
	extra_cycle_execution = ins->execute(cpu);

	/*Next instruction*/
	(cpu->regs).PC += ins->length;

	/*Clock implementation*/
	cycles = ins->cycles + extra_cycle_addressing + extra_cycle_execution;
	return;
}

void resetCPU(cpu * cpu)
{
	(cpu->regs).PC = cpu_read(cpu, 0xFFFC); /*Parte baja de PC*/
	(cpu->regs).PC |= cpu_read(cpu, 0xFFFD) << 8; /*Parte alta de PC*/
}

void freeCPU(cpu * cpu)
{
	if(cpu == NULL)
		return;
	free(cpu->ram);
	free(cpu);
	return;
}

void printRegisters(cpu * cpu)
{
	printf("PC:\t0x%x\n", (cpu->regs).PC);
	printf("S:\t0x%x\n", (cpu->regs).S);
	printf("A:\t0x%x\n", (cpu->regs).A);
	printf("X:\t0x%x\n", (cpu->regs).X);
	printf("Y:\t0x%x\n", (cpu->regs).Y);
	printf("P:\t0x%x\n", (cpu->regs).P);
}

void printPCMemory(cpu * cpu)
{
	int i = 0;
	printf("PC Memory: ");
	for(i = 0; i < 16; i++)
		printf("%x ", cpu_read(cpu, (cpu->regs).PC+i));
	printf("\n");
	return;
}

void printMemory(cpu * cpu, uint16_t addr)
{
	int i = 0;
	printf("Memory: ");
	for(i = 0; i < 16; i++)
		printf("%x ", cpu_read(cpu, addr + i));
	printf("\n");
	return;
}


/*Instruction functions*/

void tay(cpu * cpu)
{
	/*Clk = 2*/
	(cpu->regs).Y = (cpu->regs).A;
	return;
}

void tax(cpu * cpu)
{
	/*Clk = 2*/
	(cpu->regs).X = (cpu->regs).A;
	return;
}

void tsx(cpu * cpu)
{
	/*Cpu = 2*/
	(cpu->regs).X = (cpu->regs).S;
	return;
}

void tya(cpu * cpu)
{
	/*Clk = 2*/
	(cpu->regs).A = (cpu->regs).Y;
	return;
}

void txa(cpu * cpu)
{
	/*Clk = 2*/
	(cpu->regs).A = (cpu->regs).X;
	return;
}

void txs(cpu * cpu)
{
	/*Clk = 2*/
	(cpu->regs).S = (cpu->regs).X;
	return;
}

void lda_nn(cpu * cpu)
{
	/*Clk = 2*/
	uint8_t value;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	(cpu->regs).A = value; /*Inmediate Addressing*/
	return;
}

void ldx_nn(cpu * cpu)
{
	/*Clk = 2*/
	uint8_t value;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	(cpu->regs).X = value; /*Inmediate Addressing*/
	return;
}

void ldy_nn(cpu * cpu)
{
	/*Clk = 2*/
	uint8_t value;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	(cpu->regs).Y = value; /*Inmediate Addressing*/
	return;
}

void lda__nn(cpu * cpu)
{
	/*Clk = 3*/
	uint8_t value;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	(cpu->regs).A = cpu_read(cpu, value); /*Zero Page Addressing*/
	return;
}

void lda__nn_X(cpu * cpu)
{
	/*Clk = 4*/
	uint8_t value;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	(cpu->regs).A = cpu_read(cpu, value + (cpu->regs).X); /*Zero Page Addressing*/
	return;
}

void lda__nnnn(cpu * cpu)
{
	/*Clk = 4*/
	uint8_t valueLow, valueHigh;
	uint16_t addr = 0;
	valueLow = cpu_read(cpu, (cpu->regs).PC + 1);
	valueHigh = cpu_read(cpu, (cpu->regs).PC + 2);
	addr = valueLow;
	addr |= valueHigh << 8;
	(cpu->regs).A = cpu_read(cpu, addr); /*Absolute Addressing*/
	return;
}

void lda__nnnn_X(cpu * cpu)
{
	/*Clk = 5*/
	uint8_t valueLow, valueHigh;
	uint16_t addr = 0;
	valueLow = cpu_read(cpu, (cpu->regs).PC + 1);
	valueHigh = cpu_read(cpu, (cpu->regs).PC + 2);
	addr = valueLow;
	addr |= valueHigh << 8;
	(cpu->regs).A = cpu_read(cpu, addr + (cpu->regs).X); /*Absolute Addressing*/
	return;
}

void lda__nnnn_Y(cpu * cpu)
{
	/*Clk = 5*/
	uint8_t valueLow, valueHigh;
	uint16_t addr = 0;
	valueLow = cpu_read(cpu, (cpu->regs).PC + 1);
	valueHigh = cpu_read(cpu, (cpu->regs).PC + 2);
	addr = valueLow;
	addr |= valueHigh << 8;
	(cpu->regs).A = cpu_read(cpu, addr + (cpu->regs).Y); /*Absolute Addressing*/
	return;
}

void lda___nn_X(cpu * cpu)
{
	/*Clk = 6*/
	uint8_t value, valueLow, valueHigh;
	uint16_t addr = 0;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	valueLow = cpu_read(cpu, value + (cpu->regs).X);
	valueHigh = cpu_read(cpu, value + (cpu->regs).X + 1);
	addr = valueLow;
	addr |= valueHigh << 8;
	(cpu->regs).A = cpu_read(cpu, addr); /*Indirect Absolute Addressing*/
	return;
}

void lda___nn_Y(cpu * cpu)
{
	/*Clk = 6*/
	uint8_t valueLow, valueHigh;
	uint16_t addr = 0;
	valueLow = cpu_read(cpu, (cpu->regs).PC + 1);
	valueHigh = cpu_read(cpu, (cpu->regs).PC + 2);
	addr = valueLow;
	addr |= valueHigh << 8;
	(cpu->regs).A = cpu_read(cpu, addr + (cpu->regs).Y); /*Indirect Absolute Addressing*/
	return;
}

void ldx__nn(cpu * cpu)
{
	/*Clk = 3*/
	uint8_t value;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	(cpu->regs).X = cpu_read(cpu, value); /*Zero Page Addressing*/
	return;
}

void ldx__nn_Y(cpu * cpu)
{
	/*Clk = 4*/
	uint8_t value;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	(cpu->regs).X = cpu_read(cpu, value + (cpu->regs).Y); /*Zero Page Addressing*/
	return;
}

void ldx__nnnn(cpu * cpu)
{
	/*Clk = 4*/
	uint8_t valueLow, valueHigh;
	uint16_t addr = 0;
	valueLow = cpu_read(cpu, (cpu->regs).PC + 1);
	valueHigh = cpu_read(cpu, (cpu->regs).PC + 2);
	addr = valueLow;
	addr |= valueHigh << 8;
	(cpu->regs).X = cpu_read(cpu, addr); /*Absolute Addressing*/
	return;
}

void ldx__nnnn_Y(cpu * cpu)
{
	/*Clk = 4*/
	uint8_t valueLow, valueHigh;
	uint16_t addr = 0;
	valueLow = cpu_read(cpu, (cpu->regs).PC + 1);
	valueHigh = cpu_read(cpu, (cpu->regs).PC + 2);
	addr = valueLow;
	addr |= valueHigh << 8;
	(cpu->regs).X = cpu_read(cpu, addr + (cpu->regs).Y); /*Absolute Addressing*/
	return;
}

void ldy__nn(cpu * cpu)
{
	/*Clk = 3*/
	uint8_t value;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	(cpu->regs).Y = cpu_read(cpu, value); /*Zero Page Addressing*/
	return;
}

void ldy__nn_X(cpu * cpu)
{
	/*Clk = 4*/
	uint8_t value;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	(cpu->regs).Y = cpu_read(cpu, value + (cpu->regs).X); /*Zero Page Addressing*/
	return;
}

void ldy__nnnn(cpu * cpu)
{
	/*Clk = 4*/
	uint8_t valueLow, valueHigh;
	uint16_t addr = 0;
	valueLow = cpu_read(cpu, (cpu->regs).PC + 1);
	valueHigh = cpu_read(cpu, (cpu->regs).PC + 2);
	addr = valueLow;
	addr |= valueHigh << 8;
	(cpu->regs).Y = cpu_read(cpu, addr); /*Absolute Addressing*/
	return;
}

void ldy__nnnn_X(cpu * cpu)
{
	/*Clk = 4*/
	uint8_t valueLow, valueHigh;
	uint16_t addr = 0;
	valueLow = cpu_read(cpu, (cpu->regs).PC + 1);
	valueHigh = cpu_read(cpu, (cpu->regs).PC + 2);
	addr = valueLow;
	addr |= valueHigh << 8;
	(cpu->regs).Y = cpu_read(cpu, addr + (cpu->regs).X); /*Absolute Addressing*/
	return;
}

void sta__nn(cpu * cpu)
{
	/*Clk = 3*/
	uint8_t value;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	cpu_write(cpu, value, (cpu->regs).A);
	return;
}

void sta__nn_X(cpu * cpu)
{
	/*Clk = 4*/
	uint8_t value;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	cpu_write(cpu, value + (cpu->regs).X, (cpu->regs).A);
	return;
}

void sta__nnnn(cpu * cpu) //[nnnn]=A Clk=4
{
	/*Clk = 4*/
	uint8_t valueLow, valueHigh;
	uint16_t addr = 0;
	valueLow = cpu_read(cpu, (cpu->regs).PC + 1);
	valueHigh = cpu_read(cpu, (cpu->regs).PC + 2);
	addr = valueLow;
	addr |= valueHigh << 8;
	cpu_write(cpu, addr, (cpu->regs).A);
	return;
}

void sta__nnnn_X(cpu * cpu) //[nnnn+X]=A Clk=5
{
	/*Clk = 5*/
	uint8_t valueLow, valueHigh;
	uint16_t addr = 0;
	valueLow = cpu_read(cpu, (cpu->regs).PC + 1);
	valueHigh = cpu_read(cpu, (cpu->regs).PC + 2);
	addr = valueLow;
	addr |= valueHigh << 8;
	cpu_write(cpu, addr + (cpu->regs).X, (cpu->regs).A);
	return;
}

void sta__nnnn_Y(cpu * cpu) //[nnnn+Y]=A Clk=5
{
	/*Clk = 5*/
	uint8_t valueLow, valueHigh;
	uint16_t addr = 0;
	valueLow = cpu_read(cpu, (cpu->regs).PC + 1);
	valueHigh = cpu_read(cpu, (cpu->regs).PC + 2);
	addr = valueLow;
	addr |= valueHigh << 8;
	cpu_write(cpu, addr + (cpu->regs).Y, (cpu->regs).A);
	return;
}

void sta___nn_X(cpu * cpu) //[WORD[nn+x]]=A Clk=6
{
	/*Clk = 6*/
	uint8_t value, valueLow, valueHigh;
	uint16_t addr = 0;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	valueLow = cpu_read(cpu, value + (cpu->regs).X);
	valueHigh = cpu_read(cpu, value + (cpu->regs).X + 1);
	addr = valueLow;
	addr |= valueHigh << 8;
	cpu_write(cpu, addr, (cpu->regs).A);
	return;
}

void sta___nn_Y(cpu * cpu) //[WORD[nn]+y]=A Clk=6
{
	/*Clk = 6*/
	uint8_t value, valueLow, valueHigh;
	uint16_t addr = 0;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	valueLow = cpu_read(cpu, value);
	valueHigh = cpu_read(cpu, value + 1);
	addr = valueLow;
	addr |= valueHigh << 8;
	cpu_write(cpu, addr + (cpu->regs).Y, (cpu->regs).A);
	return;
}

void stx__nn(cpu * cpu) //[nn]=X Clk=3
{
	/*Clk = 3*/
	uint8_t value;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	cpu_write(cpu, value, (cpu->regs).X);
	return;
}

void stx__nn_Y(cpu * cpu) //[nn+Y]=X Clk=4
{
	/*Clk = 4*/
	uint8_t value;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	cpu_write(cpu, value + (cpu->regs).Y, (cpu->regs).X);
	return;
}

void stx__nnnn(cpu * cpu) //[nnnn]=X Clk=4
{
	/*Clk = 4*/
	uint8_t valueLow, valueHigh;
	uint16_t addr = 0;
	valueLow = cpu_read(cpu, (cpu->regs).PC + 1);
	valueHigh = cpu_read(cpu, (cpu->regs).PC + 2);
	addr = valueLow;
	addr |= valueHigh << 8;
	cpu_write(cpu, addr, (cpu->regs).X);
	return;
}

void sty__nn(cpu * cpu) //[nn]=Y Clk=3
{
	/*Clk = 3*/
	uint8_t value;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	cpu_write(cpu, value, (cpu->regs).Y);
	return;
}

void sty__nn_X(cpu * cpu) //[nn+X]=Y Clk=4
{
	/*Clk = 4*/
	uint8_t value;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	cpu_write(cpu, value + (cpu->regs).X, (cpu->regs).Y);
	return;
}

void sty__nnnn(cpu * cpu) //[nnnn]=Y Clk=4
{
	/*Clk = 4*/
	uint8_t valueLow, valueHigh;
	uint16_t addr = 0;
	valueLow = cpu_read(cpu, (cpu->regs).PC + 1);
	valueHigh = cpu_read(cpu, (cpu->regs).PC + 2);
	addr = valueLow;
	addr |= valueHigh << 8;
	cpu_write(cpu, addr, (cpu->regs).Y);
	return;
}

void pha(cpu * cpu) //[S]=A, S=S-1 Clk=3
{
	/*Clk = 3*/
	cpu_write(cpu, 0x100 + (cpu->regs).S, (cpu->regs).A);
	(cpu->regs).S -= 1;
	return;
}

void php(cpu * cpu) //[S]=P, S=S-1 (flags) Clk=3
{
	/*Clk = 3*/
	cpu_write(cpu, 0x100 + (cpu->regs).S, (cpu->regs).P);
	(cpu->regs).S -= 1;
	return;
}

void pla(cpu * cpu) //S=S+1, A=[S] Clk=4
{
	/*Clk = 4*/
	(cpu->regs).S += 1;
	(cpu->regs).A = cpu_read(cpu, 0x100 + (cpu->regs).S);
	return;
}

void plp(cpu * cpu) //S=S+1, P=[S] (flags) Clk= 4
{
	/*Clk = 4*/
	(cpu->regs).S += 1;
	(cpu->regs).P = cpu_read(cpu, 0x100 + (cpu->regs).S);
	return;
}

void adc_nn(cpu * cpu) //A=A+C+nn Clk=2
{
	/*Clk = 2*/
	uint16_t value;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	
	if(FLAG_IS_CARRY(cpu->regs))
		value = value + 1;

	value += (cpu->regs).A;

	carrycalc(cpu, value);
	zerocalc(cpu, value);
	overflowcalc(cpu, value);
	signcalc(cpu, value);
	saveToA(cpu, value);
	return;
}

void adc__nn(cpu * cpu) //A=A+C+[nn] Clk=3
{
	/*Clk = 3*/
	uint8_t value;
	value = cpu_read(cpu, (cpu->regs).PC + 1);
	if(FLAG_IS_CARRY(cpu->regs))
		(cpu->regs).A += 1 + cpu_read(cpu, value);
		return;
	(cpu->regs).A += cpu_read(cpu, value);
	return;
}

void adc__nn_X(cpu * cpu); //A=A+C+[nn+X] Clk=4
void adc__nnnn(cpu * cpu); //A=A+C+[nnnn] Clk=4
void adc__nnnn_X(cpu * cpu); //A=A+C+[nnnn+X] Clk=4*
void adc__nnnn_Y(cpu * cpu); //A=A+C+[nnnn+Y] Clk=4*
void adc___nn_X(cpu * cpu); //A=A+C+[word[nn+X]] Clk=6
void adc___nn_Y(cpu * cpu); //A=A+C+[word[nn]+Y] Clk=5*


void sbc_nn(cpu * cpu) //A=A+C-1-nn Clk=2
{
	uint8_t value;
	value = cpu_read(cpu, (cpu->regs).PC + 1);

}
void sbc__nn(cpu * cpu);
void sbc__nn_X(cpu * cpu);
void sbc__nnnn(cpu * cpu);
void sbc__nnnn_X(cpu * cpu);
void sbc__nnnn_Y(cpu * cpu);
void sbc___nn_X(cpu * cpu);
void sbc___nn_Y(cpu * cpu);


	/*{"SBC nn", "SBC A,[nn]", 2, 0xE5, NULL}, //A=A+C-1-[nn] Clk=3
	{"SBC nn,X", "SBC A,[nn+X]", 2, 0xF5, NULL}, //A=A+C-1-[nn+X] Clk=4
	{"SBC nnnn", "SBC A,[nnnn]", 3, 0xED, NULL}, //A=A+C-1-[nnnn] Clk=4
	{"SBC nnnn,X", "SBC A,[nnnn+X]", 3, 0xFD, NULL}, //A=A+C-1-[nnnn+X] Clk=4*
	{"SBC nnnn,Y", "SBC A,[nnnn+Y]", 3, 0xF9, NULL}, //A=A+C-1-[nnnn+Y] Clk=4*
	{"SBC (nn,X)", "SBC A,[[nn+X]]", 2, 0xE1, NULL}, //A=A+C-1-[word[nn+X]] Clk=6
	{"SBC (nn),Y", "SBC A,[[nn]+Y]", 2, 0xF1, NULL}, //A=A+C-1-[word[nn]+Y] Clk=5*/


