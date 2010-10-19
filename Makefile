WARNING="!!! This Makefile is not the recommended way to build the OHRRPGCE, and is poorly tested!" \
	"makegame.sh, makeedit.sh or makegame.bat, makeedit.bat are recommended instead !!!"

# See also makehspeak.bat, makeutil.bat/sh, makereload.bat/sh; this Makefile doesn't build those utilities

# Example use:
#  make game
#  make OHRGFX=sdl+alleg OHRMUSIC=native2 edit
# Append as many graphics backends as you want together with +. Specify at most one music backend.
# make clean before changing backends


FBC=fbc -lang deprecated

#a C compiler is not required under windows
CC=gcc -m32
MAKE=make

#msys doesn't include uname
HOST_PLATFORM = ${shell $(CC) -dumpmachine}
ifeq ($(HOST_PLATFORM), mingw32)
	win32=1
else
	unix=1
ifeq (darwin,$(findstring darwin,$(HOST_PLATFORM)))
	mac=1
endif
endif

#don't use the built in compiler rules, they don't apply to FB
.SUFFIXES:

#Mac-only
FRAMEWORKS_PATH:=$(HOME)/Library/Frameworks/

libpaths=/usr/lib/

FBFLAGS+=-mt
FBFLAGS+=-g -exx
CFLAGS+=-O2

ifdef win32
	FBFLAGS+=-s gui
#libfbgfx always needed, because of display_help_string!
	libraries=fbgfx
#libraries+= gdi32 winmm msvcrt kernel32 user32
	common_objects+=win32\blit.o win32\base64.o
	game_exe:=game.exe
	edit_exe:=custom.exe
	verprint_exe:=verprint.exe
	game_flags+=gicon.rc
	edit_flags+=cicon.rc
endif

ifdef unix
	common_objects+=blit.o base64.o
ifndef mac
	libraries+= X11 Xext Xpm Xrandr Xrender pthread
else
#	libraries+= objc gcc_s.10.5 crt1.10.5.o
endif
	game_exe:=ohrrpgce-game
	edit_exe:=ohrrpgce-custom
	verprint_exe:=verprint
endif

ifndef OHRMUSIC
	OHRMUSIC:=sdl
endif

ifndef OHRGFX
ifdef mac
	OHRGFX:=sdl
else
ifdef unix
	OHRGFX:=sdl+fb
else
	OHRGFX:=directx+sdl+fb
endif
endif
endif

#Note: all these libraries are specified so that things will link even if fbc is compiled without objinfo,
#which stops #inclib directives from working

ifeq ($(findstring fb,$(OHRGFX)), fb)
	common_modules+= gfx_fb
	libraries+= fbgfx
ifdef mac
$(error gfx_fb not supported on Mac)
endif
endif

ifeq ($(findstring alleg,$(OHRGFX)), alleg)
	common_modules+= gfx_alleg
	libraries+= alleg
ifdef mac
$(error gfx_alleg not supported on Mac)
endif
endif

ifeq ($(findstring sdl,$(OHRGFX)), sdl)
	common_modules+= gfx_sdl
	linksdl=1
#	libraries+= SDL SDLmain
endif

ifeq ($(findstring directx,$(OHRGFX)), directx)
#nothing needed
ifdef unix
$(error gfx_directx only supported on Windows)
endif
endif

ifeq ($(findstring sdlpp,$(OHRGFX)), sdlpp)
#nothing needed
endif


ifeq "$(OHRMUSIC)" "native"
	common_modules+= music_native
	common_objects+=win32\audwrap.o
	libraries+= audiere
	libpath+= audwrap
else
ifeq "$(OHRMUSIC)" "native2"
	common_modules+= music_native2
	common_objects+=win32\audwrap.o
	libraries+= audiere
	libpath+= audwrap
else
ifeq "$(OHRMUSIC)" "sdl"
	common_modules+= music_sdl sdl_lumprwops
	linksdl=1
ifndef mac
	libraries+= SDL_mixer
endif
endif
ifeq "$(OHRMUSIC)" "silence"
	common_modules+= music_silence
endif
endif
endif


common_modules+=allmodex backends lumpfile compat bam2mid common browse util loading reload reloadext slices
common_objects+=$(addsuffix .o,$(common_modules))
common_sources:=$(addsuffix .bas,$(common_modules))

game_modules:=game bmod bmodsubs menustuf moresubs yetmore yetmore2 savegame hsinterpreter 
game_objects:=$(addsuffix .o,$(game_modules))  
game_sources:=$(addsuffix .bas,$(game_modules))

edit_modules:=custom customsubs drawing subs subs2 mapsubs flexmenu menus sliceedit reloadedit editedit
edit_objects:=$(addsuffix .o,$(edit_modules))
edit_sources:=$(addsuffix .bas,$(edit_modules))

main_modules:=game.o custom.o reload2xml.o xml2reload.o reloadtest.o reloadutil.o

reload_objects:=reload.o reloadext.o lumpfile.o util.o base64.o blit.o

#Sadly, we make every source file depend on every include
includes:=${shell echo *.bi}

#The following common modules need to be rebuilt for Game or Custom, because
#they depend on IS_GAME/IS_EDIT or g/cver.txt:
semicommon_modules:=backends browse common allmodex slices compat music_native music_native2
semicommon_objects+=$(addsuffix .o,$(semicommon_modules))
semicommon_sources+=$(addsuffix .bas,$(semicommon_modules))

#semicommon_sources:=$(filter $(semicommon_sources),$(common_sources))
#common_objects:=$(filter-out $(semicommon_objects),$(common_objects))

#c/gver.txt are phony to force recreating them
.PHONY: all game edit clean spotless gver.txt cver.txt bam2mid 

libraries:=$(addprefix -l , $(libraries))
libpaths :=$(addprefix -p , $(libpaths))

ifdef linksdl
#actually includes lib path
#	libraries+=${shell sdl-config --libs}
#	libraries:=$(patsubst -Wl,-Wl ,$(libraries))
ifdef mac
	libraries+= -Wl -framework,SDL_mixer -Wl -framework,SDL -Wl -framework,Cocoa -Wl -F$(FRAMEWORKS_PATH)
	common_objects+= SDLMain.o
	FBFLAGS+= -entry SDL_main
	CFLAGS+=${shell if [ `which sdl-confijg` ] ; then sdl-config --cflags; else echo -I$(FRAMEWORKS_PATH)/SDL.framework/Headers; fi}
else
	libraries+= -l SDL -l SDLmain
endif

endif

ifdef mac
	FBFLAGS+= -Wl -macosx_version_min,10.4
endif


all: game edit

game:
	@$(MAKE) $(game_exe) --no-print-directory || echo $(WARNING)

edit:
	@$(MAKE) $(edit_exe) --no-print-directory || echo $(WARNING)

$(game_exe): FBFLAGS+=-d IS_GAME
$(game_exe): gver.txt game_compiled_objs $(common_objects) $(game_objects)
	@echo Linking Game...
	$(FBC) -x $(game_exe) -m game $(FBFLAGS) $(game_flags) $(game_objects) $(common_objects) $(libpaths) $(libraries)


$(edit_exe): FBFLAGS+=-d IS_CUSTOM
$(edit_exe): cver.txt custom_compiled_objs $(common_objects) $(edit_objects)
	@echo Linking Custom...
	$(FBC) -x $(edit_exe) -m custom $(FBFLAGS) $(edit_flags) $(edit_objects) $(common_objects) $(libpaths) $(libraries)


bam2mid: FBFLAGS:=
bam2mid: bam2mid.bas
	@echo Compiling Bam2Midi...
	$(FBC) bam2mid.bas $(FBFLAGS)


#reload2xml xml2reload reloadtest reloadutil

#-exx causes fbc to throw an error on OPEN CONS, even though it works without error checking!
reload: FBFLAGS:=-g
reload: gver.txt reload_compiled_objs $(reload_objects) reload2xml.o xml2reload.o reloadtest.o reloadutil.o
	$(FBC) $(FBFLAGS) reloadtest.o $(reload_objects) $(libpaths) $(libraries)
	$(FBC) $(FBFLAGS) reloadutil.o $(reload_objects) $(libpaths) $(libraries)
	$(FBC) $(FBFLAGS) reload2xml.o $(reload_objects) $(libpaths) $(libraries)
	$(FBC) $(FBFLAGS) xml2reload.o $(reload_objects) $(libpaths) $(libraries) -l xml2

clean:
	@echo Removing compilation files...
	@rm -f *.o
	@rm -f gver.txt cver.txt
	@rm -f $(verprint_exe)
	@rm -f game_compiled_objs custom_compiled_objs

spotless: clean
	@rm -f $(game_exe)
	@rm -f $(edit_exe)

gver.txt cver.txt: $(verprint_exe)
	./$(verprint_exe) $(OHRGFX) $(OHRMUSIC)

$(verprint_exe): verprint.bas
	$(FBC) -m verprint verprint.bas -g

game_compiled_objs:
	@if [ -e custom_compiled_objs ] ; then rm -f custom_compiled_objs $(semicommon_objects); fi
	@if [ -e reload_compiled_objs ] ; then rm -f reload_compiled_objs $(common_objects); fi
	@echo . > game_compiled_objs

custom_compiled_objs:
	@if [ -e game_compiled_objs ] ; then rm -f game_compiled_objs $(semicommon_objects); fi
	@if [ -e reload_compiled_objs ] ; then rm -f reload_compiled_objs $(common_objects); fi
	@echo . > custom_compiled_objs

reload_compiled_objs:
	@if [ -e custom_compiled_objs ] ; then rm -f custom_compiled_objs $(common_objects); fi
	@if [ -e game_compiled_objs ] ; then rm -f game_compiled_objs $(common_objects); fi
	@echo . > reload_compiled_objs

#these need the program specific -m flag
$(main_modules): %.o: %.bas $(includes)
	$(FBC) -c $< -m $* $(FBFLAGS)

%.o: %.bas $(includes)
	$(FBC) -c $< $(FBFLAGS)

%.o: mac/%.m
	$(CC) -c $< -o $@ $(CFLAGS)

$(semicommon_objects) game.o custom.o: codename.txt

#unix only; run make in win32/ on windows
blit.o: blit.c
	$(CC) -c -g -O3 $<

base64.o: base64.c
	$(CC) -c -g -O3 $< --std=c99

