typedef struct _registers
{
	unsigned short PC; /*Program Counter*/
	unsigned char S; /*Stack*/
	unsigned char A; 
	unsigned char X;
	unsigned char Y;
	unsigned char P;
} registers;


#define FLAG_CARRY (1)
#define FLAG_ZERO (1 << 1)
#define FLAG_INTERRUPT (1 << 2)
#define FLAG_DECIMALMODE (1 << 3)
#define FLAG_BREAK (1 << 4)
#define FLAG_OVERFLOW (1 << 6)
#define FLAG_NEGATIVE (1 << 7)

#define FLAG_IS_CARRY(registers) (registers.P & FLAG_CARRY)
#define FLAG_IS_ZERO(registers) (registers.P & FLAG_ZERO)
#define FLAG_IS_OVERFLOW(registers) (registers.P & FLAG_OVERFLOW)
#define FLAG_IS_NEGATIVE(registers) (registers.P & FLAG_NEGATIVE)

#define FLAG_IS_SET(registers, flag) (registers.P & (flag))
#define FLAG_SET(registers, flag) (registers.P |= (flag))
#define FLAG_CLEAR(registers, flag) (registers.P &= ~(flag))
