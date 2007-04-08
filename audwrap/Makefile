all: audwrap

audwrap: libaudwrap.a
clean:
	$(RM) libaudwrap.a *.o

libaudwrap.a: CXXFLAGS+=-DAUDWRAP_EXPORTS -O
libaudwrap.a: audwrap.o
	$(CXX) -shared -o libaudwrap.a audwrap.o -laudiere -lpthread -lvorbisfile -lvorbis -logg -lrt $(CXXFLAGS)

.PHONY: all audwrap clean
