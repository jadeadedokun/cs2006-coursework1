# Defines variables for the compiler, flags and the executable file
CC = ghc
CFLAGS = -Wall -Wextra -g
EXEC = calc

all: build run

clean:
	rm -f *.hi *.o $(EXEC)

build: *.hs
	$(CC) $(CFLAGS) -o $(EXEC) *.hs

run: $(EXEC)
	./$(EXEC)
