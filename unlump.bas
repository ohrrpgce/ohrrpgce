'OHRRPGCE UNLUMP - RPG File unlumping utility
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
' Compile with makeutil.sh or makeutil.bat
'
'basic subs and functions
DECLARE FUNCTION editstr (stri as string, key as string, byref cur as integer, byref max as integer, byref number as integer) as string
DECLARE SUB fatalcleanup ()
DECLARE FUNCTION checkpassword (pass as string) as integer

#include "config.bi"
#include "util.bi"
#include "const.bi"
#include "lumpfile.bi"
#include "common_base.bi"

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
 PRINT "If a password is required, you will be prompted to enter it."
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
 DIM dummy as string = readkey()
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
 IF LEN(rightafter(lumped, ".")) = LEN(lumped) - 1 THEN fatalerror "please specify an output directory"
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
 ' We skip checking the password, but who cares?
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
IF isfile(dest + SLASH + "archinym.lmp") THEN
 DIM fh as integer = FREEFILE
 OPEN dest + SLASH + "archinym.lmp" FOR INPUT AS #fh
 DIM a as string
 LINE INPUT #fh, a
 CLOSE #fh
 IF LEN(a) <= 8 THEN
  game = a
 END IF
 killfile dest + SLASH + "archinym.lmp"
END IF

unlumpfile lumped, game + ".gen", dest + SLASH
DIM SHARED gen(360) as integer
xbload dest + SLASH + LCASE(game) + ".gen", gen(), "unable to open general data"

killfile dest + SLASH + game + ".gen"

DIM passokay as integer = -1

IF checkpassword("") = 0 THEN
 passokay = 0
 '-----get inputed password-----
 print "Password Required"
 DIM pas as string = ""
 DIM w as string
 DO
  w = readkey
  IF w = CHR(13) THEN
   PRINT ""
   IF checkpassword(pas) = 0 THEN fatalerror "password mismatch"
   passokay = -1
   EXIT DO
  END IF
  LOCATE , 1
  FOR i as integer = 1 TO LEN(pas)
   PRINT " "; 
  NEXT i
  pas = editstr(pas, w, cur, 17, 0)
  LOCATE , 1
  FOR i as integer = 1 TO LEN(pas)
   PRINT "*"; 
  NEXT i
  sleep 80,1
 LOOP
END IF

IF passokay THEN
 unlump lumped, dest + SLASH, YES, verbose
END IF

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

FUNCTION passwordhash (p as string) as ushort
 'Just a simple stupid 9-bit hash.
 'The idea is just to make the password unretrieveable, without using a cryptographic hash.
 IF p = "" THEN RETURN 0
 DIM hash as ushort
 FOR i as integer = 0 TO LEN(p) - 1
  hash = hash * 3 + p[i] * 31
 NEXT
 RETURN (hash AND 511) OR 512  'Never return 0
END FUNCTION

'Read old-old-old password (very similar to PW3)
FUNCTION read_PW1_password () as string
 DIM rpas as string
 FOR i as integer = 1 TO gen(genPW1Length)
  IF gen(4 + i) >= 0 AND gen(4 + i) <= 255 THEN rpas = rpas + CHR(loopvar(gen(4 + i), 0, 255, gen(genPW1Offset) * -1))
 NEXT i
 RETURN rpas
END FUNCTION

'Read old-old scattertable password format
FUNCTION read_PW2_password () as string
 DIM stray(10) as integer
 DIM pass as string = STRING(20, "!")

 FOR i as integer = 0 TO gen(genPW2Length)
  setbit stray(), 0, i, readbit(gen(), 200 - 1, gen(200 + i))
 NEXT i

 array2str stray(), 0, pass
 pass = LEFT(pass, INT((gen(genPW2Length) + 1) / 8))

 RETURN rotascii(pass, gen(genPW2Offset) * -1)
END FUNCTION

FUNCTION read_PW3_password () as string
 '--read a 17-byte string from GEN at word offset 7
 '--(Note that array2str uses the byte offset not the word offset)
 DIM pass as STRING
 pass = STRING(17, 0)
 array2str gen(), 14, pass

 '--reverse ascii rotation / weak obfuscation
 pass = rotascii(pass, gen(genPW3Rot) * -1)

 '-- discard ascii chars lower than 32
 DIM pass2 as string = ""
 FOR i as integer = 1 TO 17
  DIM c as string = MID(pass, i, 1)
  IF ASC(c) >= 32 THEN pass2 += c
 NEXT i

 RETURN pass2
END FUNCTION

'Return true if it passes.
'Supports all password formats, because this is called before upgrade
FUNCTION checkpassword (pass as string) as integer
 IF gen(genPassVersion) > 257 THEN
  'Please let this never happen
  RETURN NO
 ELSEIF gen(genPassVersion) = 257 THEN
  RETURN (passwordhash(pass) = gen(genPW4Hash))
 ELSEIF gen(genPassVersion) = 256 THEN
  '--new format password
  RETURN (pass = read_PW3_password)
 ELSEIF gen(genVersion) >= 3 THEN
  '--old scattertable format
  RETURN (pass = read_PW2_password)
 ELSE
  '--ancient format
  RETURN (pass = read_PW1_password)
 END IF
END FUNCTION
