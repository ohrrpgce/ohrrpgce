OUTPUT=libaudwrap.a

all: audwrap

audwrap: CXXFLAGS+=-DAUDIERE_EXPORTS -O3 -Wall -Wno-non-virtual-dtor
audwrap: $(OUTPUT)

$(OUTPUT): audwrap.o
	$(CXX) -shared -Wall -o $(OUTPUT) audwrap.o -L./ -laudiere -lvorbisfile -lvorbis -logg -lrt -lpthread

clean:
	@$(RM) audwrap.o

.PHONY: all audwrap clean relink
