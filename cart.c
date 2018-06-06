#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cart.h"

/*Based on ChickenNES Emulator*/

typedef struct {
	uint8_t NES[4];
	uint8_t prg_size;
	uint8_t chr_size;
	uint8_t rom_info;
	uint8_t rom_info2;
	uint8_t region;
	uint8_t ines_reserved[7];
} iNES;

nes_cart * load_cart(const char * path)
{
	FILE * file;
	iNES header;
	nes_cart * cart;

	file = fopen(path, "rb");
	if(file == NULL)
	{
		printf("Invalid path\n");
		return NULL;
	}

	/*Reading header*/
	fread(&header, 1, sizeof(header), file);

	if(memcmp(header.NES, "NES\x1A", 4))
	{
		printf("Invalid iNES rom image.\n");
		return NULL;
	}

	cart = calloc(sizeof(*cart), 1);
	cart->prg_size = header.prg_size * 0x4000;
	cart->chr_size = header.chr_size * 0x2000;
	cart->mapper = (header.rom_info >> 4) + (header.rom_info2 & 0xf0);
	cart->region = header.region & 0x01;
	cart->battery = header.rom_info & 0x02;
	//16 for PAL, 15 for NTSC
	cart->multiplier = cart->region ? 16 : 15;

	#ifdef _DEBUG
	printf("PRG size:\t0x%X\n", (unsigned int) cart->prg_size);
	printf("CHR size:\t0x%X\n", (unsigned int) cart->chr_size);
	printf("Region:\t%s\n", cart->region ? "PAL" : "NTSC");
	printf("Mapper:\t%i\n",  cart->mapper);
	if (cart->battery)
		printf("Battery Save\n");
	if (header.rom_info & 0x1)
		printf("Mirroring:\tVertical\n");
	else if (header.rom_info & 0x8)
		printf("Mirroring:\tFour Screen\n");
	else
		printf("Mirroring:\tHorizontal\n");
	#endif

	/*Allocating memory*/
	if (cart->prg_size == 0x4000)
	{
		cart->prg_rom = malloc(0x8000);
	} 
	else
	{
		cart->prg_rom = malloc(cart->prg_size);
	}
	 /*Copying memory*/
	if (cart->prg_size >= 0x8000)
	{
		fread(cart->prg_rom, cart->prg_size, 1, file);
	}
	else
	{
		fread(cart->prg_rom, cart->prg_size, 1, file);
		memcpy(&cart->prg_rom[0x4000], cart->prg_rom, 0x4000);
	}

	for (int i = 0; i < cart->prg_size / 0x4000; i++) {
		fprintf(stderr, "Time %i\n", i);
		cart->prg_pages[i] = &cart->prg_rom[0x4000 * i];
	}
	fprintf(stderr, "0 %x\n", cart->prg_pages[0][0]);

	//if CHR rom is present, allocate memory for it
	if (cart->chr_size > 0)
	{
		cart->chr_rom = malloc(cart->chr_size);
		fread(cart->chr_rom, cart->chr_size, 1, file);
	}

	fclose(file);
	return cart;
}
