'OHRRPGCE UNLUMP - RPG File unlumping utility
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
' Compile with makeutil.sh or makeutil.bat
'

#include "config.bi"
#include "util.bi"
#include "const.bi"
#include "lumpfile.bi"
#include "common_base.bi"

'basic subs and functions
DECLARE FUNCTION editstr (stri as string, key as string, byref cur as integer, byref max as integer, byref number as integer) as string
DECLARE SUB fatalcleanup ()

cleanup_function = @fatalcleanup

DIM SHARED createddir as integer = 0
DIM SHARED dest as string
DIM SHARED olddir as string
DIM SHARED verbose as bool = NO
DIM SHARED recover as bool = NO

DIM cur as integer = 0

olddir = curdir


'------------------------------------------------------------------------------
' Process command line

SUB showusage ()
 PRINT "O.H.R.RPG.C.E. unlumping utility"
 PRINT ""
 PRINT "A utility to extract the contents of an RPG file or other lumped"
 PRINT "file to a directory so that advanced users can hack the delicious"
 PRINT "morsels inside."
 PRINT ""
 PRINT "syntax:"
 PRINT "  unlump [-v] [--recover] filename.rpg [directory]"
 PRINT ""
 PRINT "  -v:        Print verbose messages"
 PRINT "  --recover: Try to extract damaged lumped files (experimental)"
 PRINT ""
 PRINT "You can drag-and-drop a file onto this program to unlump it."
 PRINT "The output directory defaults to 'filename.rpgdir' or 'filename.unlmp'."
 PRINT ""
 PRINT "[Press a Key]"
 readkey
 fatalerror ""
END SUB

FUNCTION unlump_setoption(opt as string, arg as string) as integer
 IF opt = "v" THEN
  verbose = YES
  RETURN 1
 ELSEIF opt = "help" or opt = "?" or opt = "h" THEN
  showusage
 ELSEIF opt = "recover" THEN
  recover = YES
  RETURN 1
 END IF
END FUNCTION


DIM cmdline_args() as string
processcommandline cmdline_args(), @unlump_setoption

DIM numargs as integer = UBOUND(cmdline_args) + 1
IF numargs < 1 OR numargs > 2 THEN showusage

DIM lumped as string = cmdline_args(0)

'check whether it is an RPG file (assume all RPG files contain BROWSE.TXT)
DIM isrpg as bool = islumpfile(lumped, "browse.txt")

IF numargs >= 2 THEN
 dest = cmdline_args(1)
ELSE
 IF isrpg THEN
  dest = trimextension(lumped) + ".rpgdir"
 ELSE
  dest = trimextension(lumped) + ".unlmp"
 END IF
END IF


'------------------------------------------------------------------------------
' Setup

IF NOT isfile(lumped) THEN fatalerror "lump file `" + lumped + "' was not found"

PRINT "From " + lumped + " to " + dest

'--Get old-style game (only matters for ancient RPG files that are missing the archinym.lmp)
dim game as string
game = trimextension(trimpath(lumped))

IF isdir(dest) THEN
 PRINT "Destination directory `" + dest + "' already exists. Delete it? (y/n)"
 DIM w as string = readkey
 IF w <> "Y" AND w <> "y" THEN SYSTEM
 killdir dest
ELSEIF isfile(dest) THEN
 fatalerror "destination directory `" + dest + "' already exists as a file"
END IF
makedir dest
createddir = -1

IF NOT isdir(dest) THEN fatalerror "unable to create destination directory `" + dest + "'"

IF recover THEN
 recover_lumped_file lumped, dest + SLASH
 SYSTEM
END IF

IF NOT isrpg THEN
 unlump lumped, dest + SLASH
 CHDIR olddir
 PRINT "Done."
 SYSTEM
END IF
 
unlumpfile lumped, "archinym.lmp", dest + SLASH

'--set game according to the archinym
DIM fh as integer
IF OPENFILE(dest + SLASH + "archinym.lmp", FOR_INPUT, fh) = 0 THEN
 DIM a as string
 LINE INPUT #fh, a
 CLOSE #fh
 IF LEN(a) <= 8 THEN
  game = LCASE(a)
 END IF
 killfile dest + SLASH + "archinym.lmp"
END IF

unlumpfile lumped, game + ".gen", dest + SLASH
DIM SHARED gen(360) as integer
xbload dest + SLASH + game + ".gen", gen(), "unable to open general data"

killfile dest + SLASH + game + ".gen"

unlump lumped, dest + SLASH, YES, verbose

CHDIR olddir
PRINT "Done."
SYSTEM


'------------------------------------------------------------------------------


FUNCTION editstr (stri as string, key as string, byref cur as integer, byref max as integer, byref number as integer) as string

DIM pre as string = LEFT(stri, cur)
DIM post as string = RIGHT(stri, LEN(stri) - cur)

SELECT CASE key
 CASE CHR(8)
  'backspace
  IF LEN(pre) > 0 THEN pre = LEFT(pre, LEN(pre) - 1): cur = cur - 1
 CASE CHR(0) + CHR(83)
  'delete
  IF LEN(post) > 0 THEN post = RIGHT(post, LEN(post) - 1)
 CASE ELSE
  IF LEN(key) > 0 THEN
   IF (ASC(key) >= 32 AND ASC(key) < 127 AND key <> "," AND key <> "~" AND number = 0) OR (ASC(key) >= 48 AND ASC(key) <= 57 AND number) THEN
    IF LEN(post) = 0 AND LEN(pre) < max THEN post = " "
    IF LEN(post) > 0 THEN
     MID(post, 1, 1) = key
     cur = bound(cur + 1, 0, LEN(pre + post))
    END IF
   END IF
  END IF
END SELECT

RETURN pre + post

END FUNCTION

SUB fatalcleanup ()
 'RMDIR does not work unless isdir is called first. If I tried to figure out why, my brain would explode
 isdir(dest)
 IF createddir THEN killdir dest
 SYSTEM
END SUB
