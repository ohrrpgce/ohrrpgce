'OHRRPGCE RELUMP - RPG File relumping utility
'(C) Copyright 2006 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
' Compile with makeutil.sh or makeutil.bat

#ifdef LANG_DEPRECATED
 #define __langtok #lang
 __langtok "deprecated"
 OPTION STATIC
 OPTION EXPLICIT
#endif

#include "config.bi"
#include "util.bi"
#include "const.bi"
#include "file.bi"
#include "lumpfile.bi"
#include "common_base.bi"

DIM olddir as string = curdir

IF COMMAND = "" THEN
 PRINT "O.H.R.RPG.C.E. lumping utility"
 PRINT ""
 PRINT "syntax:"
 PRINT "relump folder filename"
 PRINT ""
 PRINT "A utility to package the contents of a folder into an OHRRPGCE"
 PRINT "lumpfile, such as an .RPG file"
 PRINT ""
 PRINT "Windows users can drag-and-drop their rpgdir folder onto this program"
 PRINT "to relump it."
 PRINT ""
 PRINT "[Press a Key]"
 DIM dummy as string = readkey()
 fatalerror ""
END IF


DIM src as string = COMMAND(1)
DIM dest as string = COMMAND(2)

IF RIGHT(src,1)=SLASH THEN src=LEFT(src,LEN(src)-1)

IF NOT isdir(src) THEN
  IF isfile(src) THEN fatalerror src + "' is a file, not a folder"
  fatalerror "rpgdir folder `" + src + "' was not found"
END IF

IF dest = "" THEN
 IF RIGHT(src,7) = ".rpgdir" THEN
  dest = trimextension(src) + ".rpg"
 ELSE
  fatalerror "please specify an output folder"
 END IF
END IF

PRINT "From " + src + " to " + dest

IF isdir(dest) THEN
 fatalerror "destination file " + dest + " already exists as a folder."
ELSEIF isfile(dest) THEN
 PRINT "destination file " + dest + " already exists. Replace it? (y/n)"
 DIM w as string
 w = readkey
 IF w <> "Y" AND w <> "y" THEN SYSTEM
END IF

set_tmpdir
IF NOT isdir(tmpdir) THEN
 IF makedir(tmpdir) <> 0 THEN fatalerror "Unable to create temp directory " & tmpdir
END IF

'--build the list of files to lump
REDIM filelist() as string
findfiles src, ALLFILES, fileTypefile, NO, filelist()
fixlumporder filelist()
'---relump data into lumpfile package---
lumpfiles filelist(), dest, src + SLASH

safekill tmpdir
