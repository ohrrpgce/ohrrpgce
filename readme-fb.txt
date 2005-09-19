-----------------------------------------------------------
OHRRPGCE FreeBasic version

2005-09-19 - Filled in more functions and updated from QB.
2005-09-14 - Combat bug fix.
2005-09-11 - Initial release.
-----------------------------------------------------------
RANDOM STUFF ABOUT THE FREEBASIC VERSION IN THE 
ORDER I THINK OF IT.

Not Yet Done
------------
- Drives. The drive list is faked, and there's no way to 
change drives yet, so run it from the same drive as your
game files.
- Fullscreen. Easy to enable in code, but I've left it in
a window for now thinking it will be easier to debug. It
can be manually changed by Alt-Enter. (Also, the underlying
console window can be removed with the -s gui compiler flag.)
- Sound. Probably going to be quite tough.
- Joystick and Mouse. Haven't even looked at it.

New Files
---------
readme-fb.txt - this file.
allmodex.bas - the main new source file for this version.
amx_state.txt - notes about where I am with the allmodex
	functions.
fbgame.txt - description of the conversion process from QB.
	the QB build on which this is based was taken from
	svn on 2005-08-28. I think.
fontdata.bi - the font data from ohrrpgce.fnt converted into
	an array declaration using bin2bas.
gicon.rc - resource file for the icon.
makegame.bat - compile command.

also

env-set.bat - is bound to be wrong for anyone else's system.

Other Stuff
-----------
I've been focussing on Windows, but the source should also 
build fine in the DOS version of FreeBasic. Linux will need
some code changes relating to file access.

I have made no attempt to port custom.exe, and imagine that
some of the allmodex functions will need revising to support
it. I see the editor as a different class of problem, which
is more amenable to other solutions. It should be possible
to write an editor from scratch knowing only the .rpg file
specification. Game.exe, on the other hand, needs to copy
the internal logic exactly, and so a direct port makes more
sense.

I can only hope I removed all my swearing from the comments.

This file should probably be replaced with something more 
sensible in future versions.

I don't really know where I want problems reported. I'll think
of something later.