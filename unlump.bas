'OHRRPGCE UNLUMP - RPG File unlumping utility
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
'
' Compile with 'scons unlump'
'

#include "config.bi"
#include "util.bi"
#include "const.bi"
#include "lumpfile.bi"
#include "common_base.bi"

'basic subs and functions
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

unlump lumped, dest + SLASH, YES, verbose

PRINT "Done."
SYSTEM


'------------------------------------------------------------------------------

SUB fatalcleanup ()
 'RMDIR does not work unless isdir is called first. If I tried to figure out why, my brain would explode
 isdir(dest)
 IF createddir THEN killdir dest
 SYSTEM
END SUB
