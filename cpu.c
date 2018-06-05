#include <stdlib.h>
#include "cpu.h"


struct _instruction
{
	char * signature;
	char * auxSignature;
	int length;
	uint8_t code;
	void (*execute) (cpu * cpu);
};


#define OPNUM 151

#define RAM_SIZE 0x800

/*Opcodes*/
/*http://problemkaputt.de/everynes.htm#cpu65xxmicroprocessor*/


const instruction instructions[256] = {
	/////////////////////////////////////
	//CPU Memory and Register Transfers//
	/////////////////////////////////////
	//Register/Immeditate to Register Transfer
	{"TAY", "MOV Y,A", 1, 0xA8, NULL}, //Y=A Clk=2
	{"TAX", "MOV X,A", 1, 0xAA, NULL}, //X=A Clk=2
	{"TSX", "MOV X,S", 1, 0xBA, NULL}, //X=S Clk=2
	{"TYA", "MOV A,Y", 1, 0x98, NULL}, //A=Y Clk=2
	{"TXA", "MOV A,X", 1, 0x8A, NULL}, //A=X Clk=2
	{"TXS", "MOV S,X", 1, 0x9A, NULL}, //S=X Clk=2
	{"LDA #nn", "MOV A,nn", 2, 0xA9, NULL}, //A=nn Clk=2
	{"LDX #nn", "MOV X,nn", 2, 0xA2, NULL}, //X=nn Clk=2
	{"LDY #nn", "MOV Y,nn", 2, 0xA0, NULL}, //Y=nn Clk=2

	//Load Register from Memory
	//* Add one cycle if indexing crosses a page boundary.
	{"LDA nn", "MOV A,[nn]", 2, 0xA5, NULL}, //A=[nn] Clk=3
	{"LDA nn,X", "MOV A,[nn+X]", 2, 0xB5, NULL}, //A=[nn+X] Clk=4
	{"LDA nnnn", "MOV A,[nnnn]", 3, 0xAD, NULL}, //A=[nnnn] Clk=4
	{"LDA nnnn,X", "MOV A,[nnnn+X]", 3, 0xBD, NULL}, //A=[nnnn+X] Clk=5
	{"LDA nnnn,Y", "MOV A,[nnnn+Y]", 3, 0xB9, NULL}, //A=[nnnn+Y] Clk=5
	{"LDA (nn,X)", "MOV A,[[nn+X]]", 2, 0xA1, NULL}, //=[WORD[nn+X]] Clk=6
	{"LDA (nn),Y", "MOV A,[[nn]+Y]", 2, 0xB1, NULL}, //A=[WORD[nn]+Y] Clk=6
	{"LDX nn", "MOV X,[nn]", 2, 0xA6, NULL}, //X=[nn] Clk=3
	{"LDX nn,Y", "MOV X,[nn+Y]", 2, 0xB6, NULL}, //X=[nn+Y] Clk=4
	{"LDX nnnn", "MOV X,[nnnn]", 3, 0xAE, NULL}, //X=[nnnn] Clk=4
	{"LDX nnnn,Y", "MOV X,[nnnn+Y]", 3, 0xBE, NULL}, //X=[nnnn+Y] Clk=4*
	{"LDY nn", "MOV Y,[nn]", 2, 0xA4, NULL}, //Y=[nn] Clk=3
	{"LDY nn,X", "MOV Y,[nn+X]", 2, 0xB4, NULL}, //Y=[nn+X] Clk=4
	{"LDY nnnn", "MOV Y,[nnnn]", 3, 0xAC, NULL}, //Y=[nnnn] Clk=4
	{"LDY nnnn,X", "MOV Y,[nnnn+X]", 3, 0xBC, NULL}, //;Y=[nnnn+X] Clk=4*

	//Store Register in Memory
	{"STA nn", "MOV [nn],A", 2, 0x85, NULL}, //[nn]=A Clk=3
	{"STA nn,X", "MOV [nn+X],A", 2, 0x95, NULL}, //[nn+X]=A Clk=4
	{"STA nnnn", "MOV [nnnn],A", 3, 0x8D, NULL}, //[nnnn]=A Clk=4
	{"STA nnnn,X", "MOV [nnnn+X],A", 3, 0x9D, NULL}, //[nnnn+X]=A Clk=5
	{"STA nnnn,Y", "MOV [nnnn+Y],A", 3, 0x99, NULL}, //[nnnn+Y]=A Clk=5
	{"STA (nn,X)", "MOV [[nn+x]],A", 2, 0x81, NULL}, //[WORD[nn+x]]=A Clk=6
	{"STA (nn),Y", "MOV [[nn]+y],A", 2, 0x91, NULL}, //[WORD[nn]+y]=A Clk=6
	{"STX nn", "MOV [nn],X", 2, 0x86, NULL}, //[nn]=X Clk=3
	{"STX nn,Y", "MOV [nn+Y],X", 2, 0x96, NULL}, //[nn+Y]=X Clk=4
	{"STX nnnn", "MOV [nnnn],X", 3, 0x8E, NULL}, //[nnnn]=X Clk=4
	{"STY nn", "MOV [nn],Y", 2, 0x84, NULL}, //[nn]=Y Clk=3
	{"STY nn,X", "MOV [nn+X],Y", 2, 0x94, NULL}, //[nn+X]=Y Clk=4
	{"STY nnnn", "MOV [nnnn],Y", 3, 0x8C, NULL}, //[nnnn]=Y Clk=4

	//Push/Pull
	//Notes: PLA sets Z and N according to content of A. The B-flag and unused flags cannot be changed by PLP, these flags are always written as "1" by PHP.
	{"PHA", "PUSH A", 1, 0x48, NULL}, //[S]=A, S=S-1 Clk=3
	{"PHP", "PUSH P", 1, 0x08, NULL}, //[S]=P, S=S-1 (flags) Clk=3
	{"PLA", "POP  A", 1, 0x68, NULL}, //S=S+1, A=[S] Clk=4
	{"PLP", "POP  P", 1, 0x28, NULL}, //S=S+1, P=[S] (flags) Clk= 4

	/////////////////////////////////////
	//CPU Arithmetic/Logical Operations//
	/////////////////////////////////////
	//Add memory to accumulator with carry
	//* Add one cycle if indexing crosses a page boundary.
	{"ADC #nn", "ADC A,nn", 2, 0x69, NULL}, //A=A+C+nn Clk=2
	{"ADC nn", "ADC A,[nn]", 2, 0x65, NULL}, //A=A+C+[nn] Clk=3
	{"ADC nn,X", "ADC A,[nn+X]", 2, 0x75, NULL}, //A=A+C+[nn+X] Clk=4
	{"ADC nnnn", "ADC A,[nnnn]", 3, 0x6D, NULL}, //A=A+C+[nnnn] Clk=4
	{"ADC nnnn,X", "ADC A,[nnnn+X]", 3, 0x7D, NULL}, //A=A+C+[nnnn+X] Clk=4*
	{"ADC nnnn,Y", "ADC A,[nnnn+Y]", 3, 0x79, NULL}, //A=A+C+[nnnn+Y] Clk=4*
	{"ADC (nn,X)", "ADC A,[[nn+X]]", 2, 0x61, NULL}, //A=A+C+[word[nn+X]] Clk=6
	{"ADC (nn),Y", "ADC A,[[nn]+Y]", 2, 0x71, NULL}, //A=A+C+[word[nn]+Y] Clk=5*

	//Subtract memory from accumulator with borrow
	//* Add one cycle if indexing crosses a page boundary.
	//Note: Compared with normal 80x86 and Z80 CPUs, incoming and resulting Carry Flag are reversed.
	{"SBC #nn", "SBC A,nn", 2, 0xE9, NULL}, //A=A+C-1-nn Clk=2
	{"SBC nn", "SBC A,[nn]", 2, 0xE5, NULL}, //A=A+C-1-[nn] Clk=3
	{"SBC nn,X", "SBC A,[nn+X]", 2, 0xF5, NULL}, //A=A+C-1-[nn+X] Clk=4
	{"SBC nnnn", "SBC A,[nnnn]", 3, 0xED, NULL}, //A=A+C-1-[nnnn] Clk=4
	{"SBC nnnn,X", "SBC A,[nnnn+X]", 3, 0xFD, NULL}, //A=A+C-1-[nnnn+X] Clk=4*
	{"SBC nnnn,Y", "SBC A,[nnnn+Y]", 3, 0xF9, NULL}, //A=A+C-1-[nnnn+Y] Clk=4*
	{"SBC (nn,X)", "SBC A,[[nn+X]]", 2, 0xE1, NULL}, //A=A+C-1-[word[nn+X]] Clk=6
	{"SBC (nn),Y", "SBC A,[[nn]+Y]", 2, 0xF1, NULL}, //A=A+C-1-[word[nn]+Y] Clk=5*

	//Logical AND memory with accumulator
	//* Add one cycle if indexing crosses a page boundary.
	{"AND #nn", "AND A,nn", 2, 0x29, NULL}, //A=A AND nn Clk=2
	{"AND nn", "AND A,[nn]", 2, 0x25, NULL}, //A=A AND [nn] Clk=3
	{"AND nn,X", "AND A,[nn+X]", 2, 0x35, NULL}, //A=A AND [nn+X] Clk=4
	{"AND nnnn", "AND A,[nnnn]", 3, 0x2D, NULL}, //A=A AND [nnnn] Clk=4
	{"AND nnnn,X", "AND A,[nnnn+X]", 3, 0x3D, NULL}, //A=A AND [nnnn+X] Clk=4*
	{"AND nnnn,Y", "AND A,[nnnn+Y]", 3, 0x39, NULL}, //A=A AND [nnnn+Y] Clk=4*
	{"AND (nn,X)", "AND A,[[nn+X]]", 2, 0x21, NULL}, //A=A AND [word[nn+X]] Clk=6
	{"AND (nn),Y", "AND A,[[nn]+Y]", 2, 0x31, NULL}, //A=A AND [word[nn]+Y] Clk=5*

	//Exclusive-OR memory with accumulator
	//* Add one cycle if indexing crosses a page boundary.
	{"EOR #nn", "XOR A,nn", 2, 0x49, NULL}, //A=A XOR nn Clk=2
	{"EOR nn", "XOR A,[nn]", 2, 0x45, NULL}, //A=A XOR [nn] Clk=3
	{"EOR nn,X", "XOR A,[nn+X]", 2, 0x55, NULL}, //A=A XOR [nn+X] Clk=4
	{"EOR nnnn", "XOR A,[nnnn]", 3, 0x4D, NULL}, //A=A XOR [nnnn] Clk=4
	{"EOR nnnn,X", "XOR A,[nnnn+X]", 3, 0x5D, NULL}, //A=A XOR [nnnn+X] Clk=4*
	{"EOR nnnn,Y", "XOR A,[nnnn+Y]", 3, 0x59, NULL}, //A=A XOR [nnnn+Y] Clk=4*
	{"EOR (nn,X)", "XOR A,[[nn+X]]", 2, 0x41, NULL}, //A=A XOR [word[nn+X]] Clk=6
	{"EOR (nn),Y", "XOR A,[[nn]+Y]", 2, 0x51, NULL}, //A=A XOR [word[nn]+Y] Clk=5*

	//Logical OR memory with accumulator
	//* Add one cycle if indexing crosses a page boundary.
	{"ORA #nn", "OR  A,nn", 2, 0x09, NULL}, //A=A OR nn Clk=2
	{"ORA nn", "OR  A,[nn]", 2, 0x05, NULL}, //A=A OR [nn] Clk=3
	{"ORA nn,X", "OR  A,[nn+X]", 2, 0x15, NULL}, //A=A OR [nn+X] Clk=4
	{"ORA nnnn", "OR  A,[nnnn]", 3, 0x0D, NULL}, //A=A OR [nnnn] Clk=4
	{"ORA nnnn,X", "OR  A,[nnnn+X]", 3, 0x1D, NULL}, //A=A OR [nnnn+X] Clk=4*
	{"ORA nnnn,Y", "OR  A,[nnnn+Y]", 3, 0x19, NULL}, //A=A OR [nnnn+Y] Clk=4*
	{"ORA (nn,X)", "OR  A,[[nn+X]]", 2, 0x01, NULL}, //A=A OR [word[nn+X]] Clk=6
	{"ORA (nn),Y", "OR  A,[[nn]+Y]", 2, 0x11, NULL}, //A=A OR [word[nn]+Y] Clk=5*

	//Compare
	//* Add one cycle if indexing crosses a page boundary.
	//Note: Compared with normal 80x86 and Z80 CPUs, resulting Carry Flag is reversed.
	{"CMP #nn", "CMP A,nn", 2, 0xC9, NULL}, //A-nn Clk=2
	{"CMP nn", "CMP A,[nn]", 2, 0xC5, NULL}, //A-[nn] Clk=3
	{"CMP nn,X", "CMP A,[nn+X]", 2, 0xD5, NULL}, //A-[nn+X] Clk=4
	{"CMP nnnn", "CMP A,[nnnn]", 3, 0xCD, NULL}, //A-[nnnn] Clk=4
	{"CMP nnnn,X", "CMP A,[nnnn+X]", 3, 0xDD, NULL}, //A-[nnnn+X] Clk=4*
	{"CMP nnnn,Y", "CMP A,[nnnn+Y]", 3, 0xD9, NULL}, //A-[nnnn+Y] Clk=4*
	{"CMP (nn,X)", "CMP A,[[nn+X]]", 2, 0xC1, NULL}, //A-[word[nn+X]] Clk=6
	{"CMP (nn),Y", "CMP A,[[nn]+Y]", 2, 0xD1, NULL}, //A-[word[nn]+Y] Clk=5*
	{"CPX #nn", "CMP X,nn", 2, 0xE0, NULL}, //X-nn Clk=2
	{"CPX nn", "CMP X,[nn]", 2, 0xE4, NULL}, //X-[nn] Clk=3
	{"CPX nnnn", "CMP X,[nnnn]", 3, 0xEC, NULL}, //X-[nnnn] Clk=4
	{"CPY #nn", "CMP Y,nn", 2, 0xC0, NULL}, //Y-nn Clk=2
	{"CPY nn", "CMP Y,[nn]", 2, 0xC4, NULL}, //Y-[nn] Clk=3
	{"CPY nnnn", "CMP Y,[nnnn]", 3, 0xCC, NULL}, //Y-[nnnn] Clk=4

	//Bit Test
	//Flags are set as so: Z=((A AND [addr])=00h), N=[addr].Bit7, V=[addr].Bit6. Note that N and V are affected only by [addr] (not by A).
	{"BIT nn", "TEST A,[nn]", 2, 0x24, NULL}, //test and set flags Clk=3
	{"BIT nnnn", "TEST A,[nnnn]", 3, 0x2C, NULL}, //test and set flags Clk=4

	//Increment by one
	{"INC nn", "INC [nn]", 2, 0xE6, NULL}, //[nn]=[nn]+1 Clk=5
	{"INC nn,X", "INC [nn+X]", 2, 0xF6, NULL}, //[nn+X]=[nn+X]+1 Clk=6
	{"INC nnnn", "INC [nnnn]", 3, 0xEE, NULL}, //[nnnn]=[nnnn]+1 Clk=6
	{"INC nnnn,X", "INC [nnnn+X]", 3, 0xFE, NULL}, //[nnnn+X]=[nnnn+X]+1 Clk=7
	{"INX", "INC X", 1, 0xE8, NULL}, //X=X+1 Clk=2
	{"INY", "INC Y", 1, 0xC8, NULL}, //Y=Y+1 Clk=2

	//Decrement by one
	{"DEC nn", "DEC [nn]", 2, 0xC6, NULL}, //[nn]=[nn]-1 Clk=5
	{"DEC nn,X", "DEC [nn+X]", 2, 0xD6, NULL}, //[nn+X]=[nn+X]-1 Clk=6
	{"DEC nnnn", "DEC [nnnn]", 3, 0xCE, NULL}, //[nnnn]=[nnnn]-1 Clk=6
	{"DEC nnnn,X", "DEC [nnnn+X]", 3, 0xDE, NULL}, //[nnnn+X]=[nnnn+X]-1 Clk=7
	{"DEX", "DEC X", 1, 0xCA, NULL}, //X=X-1 Clk=2
	{"DEY", "DEC Y", 1, 0x88, NULL}, //Y=Y-1 Clk=2

	/////////////////////////////////////
	//CPU Rotate and Shift Instructions//
	/////////////////////////////////////
	//ROR instruction is available on MCS650X microprocessors after June, 1976.
	//ROL and ROR rotate an 8bit value through carry (rotates 9bits in total).
	//Shift Left Logical/Arithmetic
	{"ASL A", "SHL A", 1, 0x0A, NULL}, //SHL A Clk=2
	{"ASL nn", "SHL [nn]", 2, 0x06, NULL}, //SHL [nn] Clk=5
	{"ASL nn,X", "SHL [nn+X]", 2, 0x16, NULL}, //SHL [nn+X] Clk=6
	{"ASL nnnn", "SHL [nnnn]", 3, 0x0E, NULL}, //SHL [nnnn] Clk=6
	{"ASL nnnn,X", "SHL [nnnn+X]", 3, 0x1E, NULL}, //SHL [nnnn+X] Clk=7

	//Shift Right Logical
	{"LSR A", "SHR A", 1, 0x4A, NULL}, //SHR A Clk=2
	{"LSR nn", "SHR [nn]", 2, 0x46, NULL}, //SHR [nn] Clk=5
	{"LSR nn,X", "SHR [nn+X]", 2, 0x56, NULL}, //SHR [nn+X] Clk=6
	{"LSR nnnn", "SHR [nnnn]", 3, 0x4E, NULL}, //SHR [nnnn] Clk=6
	{"LSR nnnn,X", "SHR [nnnn+X]", 3, 0x5E, NULL}, //SHR [nnnn+X] Clk=7

	//Rotate Left through Carry
	{"ROL A", "RCL A", 1, 0x2A, NULL}, //RCL A Clk=2
	{"ROL nn", "RCL [nn]", 2, 0x26, NULL}, //RCL [nn] Clk=5
	{"ROL nn,X", "RCL [nn+X]", 2, 0x36, NULL}, //RCL [nn+X] Clk=6
	{"ROL nnnn", "RCL [nnnn]", 3, 0x2E, NULL}, //RCL [nnnn] Clk=6
	{"OL nnnn,X", "RCL [nnnn+X]", 3, 0x3E, NULL}, //RCL [nnnn+X] Clk=7

	//Rotate Right through Carry
	{"ROR A", "RCR A", 1, 0x6A, NULL}, //RCR A Clk=2
	{"ROR nn", "RCR [nn]", 2, 0x66, NULL}, //RCR [nn] Clk=5
	{"ROR nn,X", "RCR [nn+X]", 2, 0x76, NULL}, //RCR [nn+X] Clk=6
	{"ROR nnnn", "RCR [nnnn]", 3, 0x6E, NULL}, //RCR [nnnn] Clk=6
	{"ROR nnnn,X", "RCR [nnnn+X]", 3, 0x7E, NULL}, //RCR [nnnn+X] Clk=7

	/////////////////////////////////////
	//CPU Jump and Control Instructions//
	/////////////////////////////////////
	//Normal Jumps & Subroutine Calls/Returns
	//Note: RTI cannot modify the B-Flag or the unused flag.
	//Glitch: For JMP [nnnn] the operand word cannot cross page boundaries, ie. JMP [03FFh] would fetch the MSB from [0300h] instead of [0400h]. Very simple workaround would be to place a ALIGN 2 before the data word.
	{"JMP nnnn", "JMP nnnn", 3, 0x4C, NULL}, //PC=nnnn Clk=3
	{"JMP (nnnn)", "JMP [nnnn]", 3, 0x6C, NULL}, //PC=WORD[nnnn] Clk=5
	{"JSR nnnn", "CALL nnnn", 3, 0x20, NULL}, //[S]=PC+2,PC=nnnn Clk = 6
	{"RTI", "RETI ;(from BRK/IRQ/NMI)", 1, 0x40, NULL}, //P=[S], PC=[S] Clk=6
	{"RTS", "RET ;(from CALL)", 1, 0x60, NULL}, //PC=[S]+1 Clk=6

	//Conditional Branches (conditional jump to PC=PC+/-dd)
	//** The execution time is 2 cycles if the condition is false (no branch executed). Otherwise, 3 cycles if the destination is in the same memory page, or 4 cycles if it crosses a page boundary (see below for exact info).
	//Note: After subtractions (SBC or CMP) carry=set indicates above-or-equal, unlike as for 80x86 and Z80 CPUs.
	{"BPL nnn", "JNS nnn", 2, 0x10, NULL}, //N=0 plus/positive Clk=2**
	{"BMI nnn", "JS  nnn", 2, 0x30, NULL}, //N=1 minus/negative/signed Clk=2**
	{"BVC nnn", "JNO nnn", 2, 0x50, NULL}, //V=0 no overflow Clk=2**
	{"BVS nnn", "JO  nnn", 2, 0x70, NULL}, //V=1 overflow Clk=2**
	{"BCC/BLT nnn", "JNC/JB  nnn", 2, 0x90, NULL}, //C=0 less/below/no carry Clk=2**
	{"BCS/BGE nnn", "JC/JAE  nnn", 2, 0xB0, NULL}, //C=1 above/greater/equal/carry Clk=2**
	{"BNE/BZC nnn", "JNZ/JNE nnn", 2, 0xD0, NULL}, //Z=0 not zero/not equal Clk=2**
	{"BEQ/BZS nnn", "JZ/JE   nnn", 2, 0xF0, NULL}, //Z=1 zero/equal Clk=2**

	//Interrupts, Exceptions, Breakpoints
	//Notes: IRQs can be disabled by setting the I-flag. BRK command, /NMI signal, and /RESET signal cannot be masked by setting I.
	//BRK/IRQ/NMI first change the B-flag, then write P to stack, and then set the I-flag, the D-flag is NOT changed and should be cleared by software.
	//The same vector is shared for BRK and IRQ, software can separate between BRK and IRQ by examining the pushed B-flag only.
	//The RTI opcode can be used to return from BRK/IRQ/NMI, note that using the return address from BRK skips one dummy/parameter byte following after the BRK opcode.
	//Software or hardware must take care to acknowledge or reset /IRQ or /NMI signals after processing it.
	//	IRQs are executed whenever "/IRQ=LOW AND I=0".
	//	NMIs are executed whenever "/NMI changes from HIGH to LOW".
	//If /IRQ is kept LOW then same (old) interrupt is executed again as soon as setting I=0. If /NMI is kept LOW then no further NMIs can be executed.
	{"BRK", "Force Break", 1, 0x00, NULL}, //B=1,[S]=PC+1,[S]=P,I=1,PC=[FFFE] Clk=7
	//  --        ---1--  7   /IRQ  Interrupt   B=0,[S]=PC,  [S]=P,I=1,PC=[FFFE]
  	//  --        ---1--  7   /NMI  NMI         B=0,[S]=PC,  [S]=P,I=1,PC=[FFFA]
  	//  --        ---1-- T+6? /RESET Reset      B=1,S=S-3,         I=1,PC=[FFFC]

	//CPU Control
	{"CLC", "CLC", 1, 0x18, NULL}, //Clear carry flag C=0 Clk=2
	{"CLI", "EI", 1, 0x58, NULL}, //Clear interrupt disable bit I=0 Clk=2
	{"CLD", "CLD", 1, 0xD8, NULL}, //Clear decimal mode D=0 Clk=2
	{"CLV", "CLV", 1, 0xB8, NULL}, //Clear overflow flag V=0 Clk=2
	{"SEC", "STC", 1, 0x38, NULL}, //Set carry flag C=1 Clk=2
	{"SEI", "DI", 1, 0x78, NULL}, //Set interrupt disable bit I=1 Clk=2
	{"SED", "STD", 1, 0xF8, NULL}, //Set decimal mode D=1

	//No Operation
	{"NOP", "NOP",1, 0xEA, NULL} //No operation Clk=2

	//Illegal OPCodes
	//TODO
};

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
	printf("Signature:\t%s\n", ins->signature);
	printf("80x86:\t\t%s\n", ins->auxSignature);
	printf("Length:\t\t%d\n", ins->length);
	printf("Code:\t\t0x%x\n\n", ins->code);
}

cpu * initCPU(uint8_t * prg)
{
	cpu * cpu;
	cpu = calloc(sizeof(cpu), 1);

	/*Initialize regs*/
	(cpu->regs).PC = 0xC000;
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
		case 0x0000 ... 0x07FF:
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
		case 0x0000 ... 0x07FF:
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
	const instruction * ins;

	/*Read instruction*/
	code = cpu_read(cpu, (cpu->regs).PC);
	/*Get the detailed instruction*/
	ins = getInstruction(code);

	if(ins == NULL)
	{
		printf("Invalid operation\n");
		(cpu->regs).PC += 1;
	}
	else
	{
		#ifdef _DEBUG
		printRegisters(cpu);
		printOpcode(ins);
		#endif

		/*Exectue function*/
		if(ins->execute == NULL)
			printf("Unimplemented function\n\n");
		else
			ins->execute(cpu);

		/*Next instruction*/
		(cpu->regs).PC += ins->length;
	}
	return;
}

void resetCPU(cpu * cpu)
{
	(cpu->regs).PC = cpu_read(cpu, 0xFFFC); /*Parte baja de PC*/
	(cpu->regs).PC |= cpu_read(cpu, 0xFFFD) << 8; /*Parte añta de PC*/
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
	printf("\nRegs\n");
	printf("PC:\t0x%x\n", (cpu->regs).PC);
	printf("S:\t0x%x\n", (cpu->regs).S);
	printf("A:\t0x%x\n", (cpu->regs).A);
	printf("X:\t0x%x\n", (cpu->regs).X);
	printf("Y:\t0x%x\n", (cpu->regs).Y);
	printf("P:\t0x%x\n\n", (cpu->regs).P);
}









