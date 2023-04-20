# Compiler and options
GHC = ghc
GHCFLAGS = -Wall -XInstanceSigs -package parsec -package random -i./src

# Source directory and files
SRCDIR = src

all: 
	ghc $(GHCFLAGS) -o flp22-fun ./src/*.hs

# Clean up
clean:
	rm -f *.o *.hi flp22-fun $(SRCDIR)/*.o $(SRCDIR)/*.hi

.PHONY: all clean
