OUTPUT=libgfx_sdl++.a
#HOST_PLATFORM = ${shell $(CXX) -dumpmachine}
#ifeq ($(HOST_PLATFORM), mingw32)  ...
COMMON=../../gfx_common
OBJ=gfx_sdl.o Input.o Video.o Window.o gfx_osmouse.o sdl2fb.o $(COMMON)/_tstring.o

all: gfx_sdl++

gfx_sdl++: CXXFLAGS+=-Wall `sdl-config --cflags` -I../.. -I$(COMMON)
gfx_sdl++: $(OUTPUT)

$(OUTPUT): $(OBJ)
	$(AR) rcs $(OUTPUT) $(OBJ)

clean:
	@$(RM) $(OBJ)
