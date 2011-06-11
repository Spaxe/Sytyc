# Builds Sytyc for Linux and Mac OS X Systems
FLAGS = --make -O3 -H14m
CC = ghc
SRC = src
BIN = index problem problem_list faq feedback
.SUFFIXES:
.SUFFIXES: .hs

all: $(BIN)

install:
	mv $(BIN) ../

.hs:
	$(CC) $(FLAGS) $<

clean:
	rm *.o *.hi $(BIN) 
