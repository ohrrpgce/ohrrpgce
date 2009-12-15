'OHRRPGCE VERPRINT - Crude version printing utility used by compile.bat
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
DECLARE FUNCTION get_date_tag () AS STRING
DECLARE FUNCTION get_svn_rev () AS STRING
DECLARE SUB split (BYVAL z AS STRING, ret() AS STRING, sep AS STRING = CHR(10))
DEFINT A-Z
'$DYNAMIC

datetag$ = get_date_tag()
svnrev$ = get_svn_rev()

OPEN "codename.txt" FOR INPUT AS #1
INPUT #1, codename$
CLOSE #1
codename$ = LEFT$(codename$, 15)

PRINT "Version ID " + datetag$
PRINT "Codename " + codename$

DIM gfxmods() AS STRING

split(command(1), gfxmods(), "+")

long_version$ = "CONST long_version as string = " + CHR$(34) + "OHRRPGCE " + codename$ + " " + datetag$ + "." + svnrev$ + " gfx_" + command(1) + "/music_" + command(2) + " " + __FB_SIGNATURE__ + " (" + __FB_BUILD_DATE__ + ")" + CHR$(34)

OPEN "cver.txt" FOR OUTPUT AS #1
FOR i AS INTEGER = 0 TO UBOUND(gfxmods)
  a$ = "#DEFINE GFX_" + UCASE$(gfxmods(i)) + "_BACKEND"
  PRINT #1, a$
NEXT
a$ = "#DEFINE MUSIC_" + UCASE$(command(2)) + "_BACKEND"
PRINT #1, a$
a$ = "#DEFINE MUSIC_BACKEND """ + LCASE$(command(2)) + """"
PRINT #1, a$
a$ = "CONST version as string = " + CHR$(34) + "OHRRPGCE " + codename$ + " " + datetag$ + CHR$(34)
PRINT #1, a$
a$ = "CONST version_code as string = " + CHR$(34) + "OHRRPGCE Editor version " + codename$ + CHR$(34)
PRINT #1, a$
a$ = "CONST version_build as string = " + CHR$(34) + datetag$ + " gfx_" + command(1) + " music_" + command(2) + CHR$(34)
PRINT #1, a$
PRINT #1, long_version$
CLOSE #1

OPEN "gver.txt" FOR OUTPUT AS #1
FOR i AS INTEGER = 0 TO UBOUND(gfxmods)
  a$ = "#DEFINE GFX_" + UCASE$(gfxmods(i)) + "_BACKEND"
  PRINT #1, a$
NEXT
a$ = "#DEFINE MUSIC_" + UCASE$(command(2)) + "_BACKEND"
PRINT #1, a$
a$ = "#DEFINE MUSIC_BACKEND """ + LCASE$(command(2)) + """"
PRINT #1, a$
a$ = "CONST version as string = " + CHR$(34) + "OHRRPGCE " + codename$ + " " + datetag$ + CHR$(34)
PRINT #1, a$
PRINT #1, long_version$
CLOSE #1

OPEN "iver.txt" FOR OUTPUT AS #1

a$ = "AppVerName=" + "OHRRPGCE (" + codename$ + ") " + datetag$
PRINT #1, a$
a$ = "VersionInfoVersion=" + MID$(datetag$, 1, 4) + "." + MID$(datetag$, 5, 2) + "." + MID$(datetag$, 7, 2) + "." + svnrev$
PRINT #1, a$

CLOSE #1

OPEN "distver.bat" FOR OUTPUT AS #1

a$ = "@ECHO OFF"
PRINT #1, a$
a$ = "SET OHRVERCODE=" + codename$
PRINT #1, a$
a$ = "SET OHRVERDATE=" + MID$(datetag$, 1, 4) + "-" + MID$(datetag$, 5, 2) + "-" + MID$(datetag$, 7, 2)
PRINT #1, a$

CLOSE #1

REM $STATIC
FUNCTION get_date_tag() AS STRING
DIM s AS STRING

'-- use the current date as a fallback in case svn info fails
get_date_tag = MID$(DATE$, 7, 4) & MID$(DATE$, 1, 2) & MID$(DATE$, 4, 2)

KILL "svninfo.tmp"
SHELL "svn info > svninfo.tmp"
fh = FREEFILE
OPEN "svninfo.tmp" FOR INPUT AS #fh
DO WHILE NOT EOF(fh)
 LINE INPUT #fh, s
 IF LEFT$(s, 19) = "Last Changed Date: " THEN
   get_date_tag = MID$(s, 20, 4) & MID$(s, 25, 2) & MID$(s, 28, 2)
 END IF
LOOP
CLOSE #fh
KILL "svninfo.tmp"
END FUNCTION

FUNCTION get_svn_rev() AS STRING
DIM s AS STRING

'-- default in case svn info fails
get_svn_rev = "0"

KILL "svninfo.tmp"
SHELL "svn info > svninfo.tmp"
fh = FREEFILE
OPEN "svninfo.tmp" FOR INPUT AS #fh
DO WHILE NOT EOF(fh)
 LINE INPUT #fh, s
 IF LEFT$(s, 10) = "Revision: " THEN
   get_svn_rev = MID$(s, 11)
 END IF
LOOP
CLOSE #fh
KILL "svninfo.tmp"
END FUNCTION

'copied from util.bas. Bad things might happen if it were used directly?
SUB split(byval z as string, ret() as string, sep as string = chr(10))
 redim ret(0)
 dim as integer i = 0, i2 = 1, j = 0
 dim as string in = z
 i = instr(i2, in, sep)
 if i = 0 then
  ret(0) = in
  exit sub
 end if
 do
  redim preserve ret(j) 
  if i = 0 then 
   ret(j) = mid(in, i2)
   exit do
  else
   ret(j) = mid(in, i2, i - i2)
  end if
  i2 = i + 1
  i = instr(i2, in, sep)
  j+=1
 loop
end sub
