CC=gcc
CXX=g++

all: CFLAGS+=-O2 -g -Wall --std=c99
all: CXXFLAGS+=-O2 -g -Wall -Wno-non-virtual-dtor
all: array.o blit.o audwrap.o base64.o utf8toisolat1.o

array.o base64.o blit.o: ../%.c
	$(CC) -c -o $@ $< $(CFLAGS)

#utf8toisolat1.o: utf8toisolat1.c
#	$(CC) -c -o $@ $< $(CFLAGS)

audwrap.o: ../audwrap/audwrap.cpp
	$(CXX) -c -o $@ $< $(CXXFLAGS)

clean:
	@$(RM) blit.o

.PHONY: all clean
