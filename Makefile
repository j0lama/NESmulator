# Author: j0lama

# COMPILER
CC = gcc

#FLAGS
CFLAGS = -c -Wall -ansi -g -std=c99 -D_DEBUG

TARGET = NESmulator

# PATHS
SOURCE_DIRECTORY = source/
INCLUDE_DIRECTORY = include/
OBJECT_DIRECTORY = objects/
BUILD_DIRECTORY = build/

CFILES = $(wildcard $(SOURCE_DIRECTORY)*.c)
OBJECTS = $(patsubst $(SOURCE_DIRECTORY)%.c, $(OBJECT_DIRECTORY)%.o, $(CFILES))

all: $(OBJECT_DIRECTORY) $(TARGET)

$(TARGET): $(OBJECTS)
	@echo "Building $@..."
	@$(CC) $(OBJECTS) -o $@

$(OBJECT_DIRECTORY):
	@mkdir $(OBJECT_DIRECTORY)

$(OBJECT_DIRECTORY)%.o: $(SOURCE_DIRECTORY)%.c
	@echo "Building $@..."
	@$(CC) -I $(INCLUDE_DIRECTORY) $(CFLAGS) $< -o $@

.PHONY: clean
clean:
	@echo "Removing objects files"
	@rm -rf $(OBJECT_DIRECTORY) $(TARGET)