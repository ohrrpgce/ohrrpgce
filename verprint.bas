'OHRRPGCE VERPRINT - Crude version printing utility used by compile.bat
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
DECLARE FUNCTION datetag$ ()
DEFINT A-Z
'$DYNAMIC


OPEN "codename.txt" FOR INPUT AS #1
INPUT #1, codename$
CLOSE #1
codename$ = LEFT$(codename$, 15)

PRINT "Version ID " + datetag$
PRINT "Codename " + codename$

OPEN "cver.txt" FOR OUTPUT AS #1
a$ = "version$ = " + CHR$(34) + "OHRRPGCE " + codename$ + " " + datetag$ + " " + command(1) + "/" + command(2) + CHR$(34)
PRINT #1, a$
a$ = "version_code$ = " + CHR$(34) + "OHRRPGCE Editor version " + codename$ + CHR$(34)
PRINT #1, a$
a$ = "version_build$ = " + CHR$(34) + "build:" + datetag$ + " gfx_" + command(1) + " music_" + command(2) + CHR$(34)
PRINT #1, a$
CLOSE #1

OPEN "gver.txt" FOR OUTPUT AS #1
a$ = "version$ = " + CHR$(34) + "OHRRPGCE " + codename$ + " " + datetag$ + " " + command(1) + "/" + command(2) + CHR$(34)
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
FUNCTION datetag$
datetag$ = MID$(DATE$, 7, 4) + MID$(DATE$, 1, 2) + MID$(DATE$, 4, 2)
END FUNCTION

