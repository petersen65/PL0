# Makefile

# Compiler
CC = pl0

# Directory for binaries
BIN = build

# Directory for source files
SRC = examples

# All source files
SRCS = $(wildcard $(SRC)/*.pl0)

# All object files
OBJS = $(patsubst $(SRC)/%.pl0,$(BIN)/%,$(SRCS))

# Default target
all: $(OBJS)

# Rule for building a single object file
$(BIN)/%: $(SRC)/%.pl0
    $(CC) $< -o $@

# Rule for cleaning up object files
clean:
    rm -f $(BIN)/*
