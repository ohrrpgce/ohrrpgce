CC=gcc
CXX=g++

all: CFLAGS+=-O2 -g -Wall
all: CXXFLAGS+=-O2 -g -Wall -Wno-non-virtual-dtor
all: blit.o audwrap.o

blit.o: ../blit.c
	$(CC) -c -o $@ $< $(CFLAGS)

audwrap.o: ../audwrap/audwrap.cpp
	$(CC) -c -o $@ $< $(CXXFLAGS)

clean:
	@$(RM) blit.o

.PHONY: all clean
