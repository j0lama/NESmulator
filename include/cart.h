#ifndef CART_H
#define CART_H

#include <stdint.h>

struct _nes_cart
{
	size_t prg_size;
	size_t chr_size;
	int region;
	int mapper;
	int battery;
	int mirroring;
	int multiplier;
	uint8_t * prg_rom;
	uint8_t * chr_rom;
};

typedef struct _nes_cart nes_cart;

nes_cart * load_cart(const char * path);
void freeCart(nes_cart * cart);

#endif
