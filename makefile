#for consistency (yes, this even works on windows)
SHELL = /bin/sh

#don't use the built in compiler rules, they don't apply to FB
.SUFFIXES:

common_modules:=allmodex compat bam2mid gfx_$(OHRGFX) music_$(OHRMUSIC) common util loading
common_objects:=$(addsuffix .o,$(common_modules))
common_sources:=$(addsuffix .bas,$(common_modules))
common_deps   :=$(addsuffix .d,$(common_modules))

game_modules:=game bmod bmodsubs menustuf moresubs yetmore yetmore2
game_objects:=$(addsuffix .o,$(game_modules))  
game_sources:=$(addsuffix .bas,$(game_modules))
game_deps   :=$(addsuffix .d,$(game_modules))

edit_modules:=custom drawing subs subs2 subs3 mapsubs flexmenu menus
edit_objects:=$(addsuffix .o,$(edit_modules))
edit_sources:=$(addsuffix .bas,$(edit_modules))
edit_deps   :=$(addsuffix .d,$(edit_modules))

game_exe:=game.exe
edit_exe:=custom.exe
compiler=fbc

include config.mak

%.d:
	@touch $@

include $(common_deps) $(edit_deps) $(game_deps)


libraries=fbmt gcc
libpaths=

ifdef win32
libraries+= gdi32 winmm msvcrt kernel32 user32
else
ifdef linux
libraries+= X11 Xext Xpm Xrandr Xrender pthread
endif
endif



ifeq "$(OHRGFX)" "fb"
libraries+= fbgfx opengl32
else
ifeq "$(OHRGFX)" "alleg"
libraries+= alleg
else
ifeq "$(OHRGFX)" "sdl"
libraries+= sdl
endif
endif
endif

ifeq "$(OHRMUSIC)" "native"
libraries+= audwrap
libpath+= audwrap
else
ifeq "$(OHRMUSIC)" "native2"
libraries+= audwrap
libpath+= audwrap
else
ifeq "$(OHRMUSIC)" "sdl"
	libraries+= sdl_mixer
endif
endif
endif

.PHONY: all game edit clean $(common_sources) gver.txt cver.txt bam2mid

libraries:=$(addprefix -l , $(libraries))
libpaths :=$(addprefix -p , $(libpaths))

all: game edit bam2mid


game : target=game
game : $(game_exe)


$(game_exe): CFLAGS+=-d IS_GAME
$(game_exe): $(common_objects) $(game_objects)
	@echo Compiling GAME...
	$(compiler) -s gui -x $(game_exe) -m game  $(CFLAGS) gicon.rc $(game_objects) $(common_objects) $(libpaths) $(libraries)

edit : target=custom
edit : $(edit_exe)


$(edit_exe): CFLAGS+=-d IS_CUSTOM
$(edit_exe): $(common_objects) $(edit_objects)
	@echo Compiling CUSTOM...
	$(compiler) -s gui -x $(edit_exe) -m custom $(CFLAGS) cicon.rc $(edit_objects) $(common_objects) $(libpaths) $(libraries)

depend: makedep
	@echo Erasing and regenerating dependancy files
	./makedep $(common_sources) $(edit_sources) $(game_sources)

makedep: makedep.exe

makedep.exe: makedep.bas
	@echo Compiling makedep
	$(compiler) makedep.bas $(CFLAGS)

bam2mid: bam2mid.bas
	@echo Compiling Bam2Midi...
	$(compiler) bam2mid.bas $(CFLAGS)


clean:
	@echo Removing compilation files...
	@-rm *.o
	@-rm *.d
	@-rm $(game_exe)
	@-rm $(edit_exe)

gver.txt cver.txt:
	verprint

%.o:%.bas
	$(compiler) -c $< $(CFLAGS)

$(common_objects): %.o : %.bas
	$(compiler) -c $< $(CFLAGS)
	
#for some reason, make tries to compile the headers otherwise...
%.bi: ;


#these need the program specific -m flag
game.o: game.bas
	$(compiler) -c $< -m game $(CFLAGS)

custom.o: custom.bas
	$(compiler) -c $< -m custom $(CFLAGS)
