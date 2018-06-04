#include <stdio.h>

typedef struct _instruction instruction;

const instruction * getInstruction(unsigned char opcode);
int getInstructionLength(const instruction * ins);
void printOpcode(const instruction * ins);
