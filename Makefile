# Makefile for a Haskell project with source files in /src

# Compiler and options
GHC = ghc
GHCFLAGS = -package parsec -package random -i./src

# Source directory and files
SRCDIR = src
SRCS = $(wildcard $(SRCDIR)/*.hs)
OBJS = $(patsubst $(SRCDIR)/%.hs, %.o, $(SRCS))

# Main target
all: flp22-fun

# Build executable
flp22-fun: $(OBJS)
	$(GHC) $(GHCFLAGS) -o $@ $^

# Build object files
%.o: $(SRCDIR)/%.hs
	$(GHC) $(GHCFLAGS) -c $< -o $@

# Clean up
clean:
	rm -f *.o *.hi flp22-fun $(SRCDIR)/*.o $(SRCDIR)/*.hi

.PHONY: all clean
