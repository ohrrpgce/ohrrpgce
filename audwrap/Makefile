OUTPUT=libaudwrap.a
#HOST_PLATFORM = ${shell $(CXX) -dumpmachine}
#ifeq ($(HOST_PLATFORM), mingw32)  ...

all: audwrap

audwrap: CXXFLAGS+=-O2 -g -Wall -Wno-non-virtual-dtor
audwrap: $(OUTPUT)

$(OUTPUT): audwrap.o
	$(AR) rcs $(OUTPUT) audwrap.o

clean:
	@$(RM) audwrap.o $(OUTPUT)

.PHONY: all audwrap clean relink
