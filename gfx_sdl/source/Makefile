CXX=g++
HOST_PLATFORM = ${shell $(CXX) -dumpmachine}
COMMON=../../gfx_common
OBJ=gfx_sdl.o gfx_sdl.old.o Input.o Video.o Window.o gfx_osmouse.o sdl2fb.o $(COMMON)/_tstring.o
ifeq ($(HOST_PLATFORM), mingw32)
 DLL_EXT=.dll
else
 DLL_EXT=.so
endif

CXXFLAGS+=-Wall `sdl-config --cflags` -I../.. -I$(COMMON) -g

all: gfx_sdl++

gfx_sdl++: old-dynamic

old-static: OBJ+=static_link.old.o gfx_sdl.old.o
old-static: OUTPUT=libgfx_sdl++_old.a
old-static: static_link.old.o gfx_sdl.old.o base
	$(AR) rcs $(OUTPUT) $(OBJ)

old-dynamic: base
	$(CXX) --shared -o gfx_sdl$(DLL_EXT) $(OBJ) `sdl-config --libs`

new: OUTPUT=libgfx_sdl++_new.a
new: base
	$(AR) rcs $(OUTPUT) $(OBJ)

base: $(OBJ)

clean:
	@$(RM) $(OBJ)
