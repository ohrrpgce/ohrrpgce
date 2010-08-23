CC=gcc
CXX=g++

all: CFLAGS+=-O2 -g -Wall
all: CXXFLAGS+=-O2 -g -Wall -Wno-non-virtual-dtor
all: blit.o audwrap.o base64.o

blit.o: ../blit.c
	$(CC) -c -o $@ $< $(CFLAGS)

base64.o: ../base64.c
	$(CC) -c -o $@ $< $(CFLAGS)

audwrap.o: ../audwrap/audwrap.cpp
	$(CC) -c -o $@ $< $(CXXFLAGS)

clean:
	@$(RM) blit.o

.PHONY: all clean
