'OHRRPGCE VERPRINT - Crude version printing utility used by compile.bat
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
DECLARE FUNCTION get_date_tag () AS STRING
DEFINT A-Z
'$DYNAMIC

datetag$ = get_date_tag()

OPEN "codename.txt" FOR INPUT AS #1
INPUT #1, codename$
CLOSE #1
codename$ = LEFT$(codename$, 15)

PRINT "Version ID " + datetag$
PRINT "Codename " + codename$

OPEN "cver.txt" FOR OUTPUT AS #1
a$ = "#DEFINE GFX_" + UCASE$(command(1)) + "_BACKEND"
PRINT #1, a$
a$ = "#DEFINE MUSIC_" + UCASE$(command(2)) + "_BACKEND"
PRINT #1, a$
a$ = "CONST version$ = " + CHR$(34) + "OHRRPGCE " + codename$ + " " + datetag$ + " " + command(1) + "/" + command(2) + CHR$(34)
PRINT #1, a$
a$ = "CONST version_code$ = " + CHR$(34) + "OHRRPGCE Editor version " + codename$ + CHR$(34)
PRINT #1, a$
a$ = "CONST version_build$ = " + CHR$(34) + "build:" + datetag$ + " gfx_" + command(1) + " music_" + command(2) + CHR$(34)
PRINT #1, a$
CLOSE #1

OPEN "gver.txt" FOR OUTPUT AS #1
a$ = "#DEFINE GFX_" + UCASE$(command(1)) + "_BACKEND"
PRINT #1, a$
a$ = "#DEFINE MUSIC_" + UCASE$(command(2)) + "_BACKEND"
PRINT #1, a$
a$ = "CONST version$ = " + CHR$(34) + "OHRRPGCE " + codename$ + " " + datetag$ + " " + command(1) + "/" + command(2) + CHR$(34)
PRINT #1, a$
CLOSE #1

OPEN "iver.txt" FOR OUTPUT AS #1

a$ = "AppVerName=" + "OHRRPGCE (" + codename$ + ") " + datetag$
PRINT #1, a$
a$ = "VersionInfoVersion=" + MID$(datetag$, 1, 4) + "." + MID$(datetag$, 5, 2) + "." + MID$(datetag$, 7, 2) + ".0"
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

