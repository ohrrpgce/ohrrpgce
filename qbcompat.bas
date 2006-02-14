'OHRRPGCE GAME - Compatibility functions, QuickBasic version
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z
'$INCLUDE: 'compat.bi'
'$INCLUDE: 'allmodex.bi'

COMMON SHARED workingdir$, version$, game$

DECLARE SUB fatalerror (e$)
DECLARE FUNCTION small% (n1%, n2%)
DECLARE SUB debug (m$)

FUNCTION canplay (file$)
	canplay = 0
	ext$ = LCASE$(MID$(file$, INSTR(file$, ".")))
	IF ext$ = ".bam" THEN canplay = 1
END FUNCTION

SUB crashexplain
	PRINT "Please report this exact error message to ohrrpgce@HamsterRepublic.com"
	PRINT "Be sure to describe in detail what you were doing when it happened"
	PRINT
	PRINT version$
	PRINT "Memory Info:"; SETMEM(0); FRE(-1); FRE(-2); FRE(0)
	PRINT "Executable: "; progdir$ + exename$ + ".exe"
	PRINT "RPG file: "; sourcerpg$
END SUB

SUB defseg (vbl AS INTEGER)
	DEF SEG = vbl
END SUB

SUB fbdim (v%)
'dummy sub for compatibility
END SUB

FUNCTION getcommandline$
	'---RELOAD COMMAND LINE FROM TEMP FILE---
	fh = FREEFILE
'       OPEN tmpdir$ + "ohrcline.tmp" FOR INPUT AS #fh
	OPEN "ohrcline.tmp" FOR INPUT AS #fh
	LINE INPUT #fh, cline$
	CLOSE #fh
	getcommandline = cline$
END FUNCTION

SUB getdefaultfont (font() AS INTEGER)
	IF isfile(progdir$ + "ohrrpgce.fnt" + CHR$(0)) THEN
		DEF SEG = VARSEG(font(0)): BLOAD progdir$ + "ohrrpgce.fnt", VARPTR(font(0))
	ELSE
		'--load the ROM font
		regs.ax = &H1130
		regs.bx = &H300
		CALL interruptx(&H10, regs, regs)
		'off9 = regs.bx: seg9 = regs.es
		DEF SEG = regs.es
		FOR i = 1 TO 255
			FOR j = 0 TO 7
				b = PEEK(regs.bp + (8 * i) + j)
				FOR k = 0 TO 7
				setbit font(), i * 4, (7 - k) * 8 + j, (b AND 2 ^ k)
				NEXT k
			NEXT j
		NEXT i
	END IF
END SUB

SUB playsongnum (songnum%)
	IF songnum > 99 THEN
		songfile$ = workingdir$ + SLASH + "song" + LTRIM$(STR$(songnum)) + ".bam"
	ELSE
		songfile$ = game$ + "." + LTRIM$(STR$(songnum))
	END IF
	IF isfile(songfile$ + CHR$(0)) THEN loadsong songfile$ + CHR$(0)
END SUB

SUB romfontchar (font%(), char%)
	regs.ax = &H1130
	regs.bx = &H300
	CALL interruptx(&H10, regs, regs)
	off9 = regs.bx: seg9 = regs.es
	DEF SEG = regs.es
	'FOR i = 1 TO 255
	FOR j = 0 TO 7
		b = PEEK(regs.bp + (8 * char) + j)
		FOR k = 0 TO 7
			setbit font(), char * 4, (7 - k) * 8 + j, (b AND 2 ^ k)
		NEXT k
	NEXT j
	'NEXT i
END SUB

SUB storecommandline
'The command line must be written to a file so it can be re-read after the CLEAR statement nukes it
	'---WRITE COMMAND-LINE ARGS TO A TEMP FILE---
	fh = FREEFILE
	'No access to tmpdir here, use current dir for now
'       OPEN tmpdir$ + "ohrcline.tmp" FOR OUTPUT AS #fh
	OPEN "ohrcline.tmp" FOR OUTPUT AS #fh
	PRINT #fh, COMMAND$
	CLOSE #fh
END SUB

SUB togglewindowed
'Dummy sub
END SUB

SUB xbload (f$, array(), e$)
	IF isfile(f$ + CHR$(0)) THEN
		DEF SEG = VARSEG(array(0)): BLOAD f$, VARPTR(array(0))
	ELSE
		fatalerror e$
	END IF
END SUB

SUB xbsave (f$, array%(), bsize%)
	DEF SEG = VARSEG(array(0))
	BSAVE f$, VARPTR(array(0)), bsize
END SUB

SUB makedir (dirname$)
    MKDIR dirname$
END SUB

SUB windowtitle (title AS STRING)
    'does nothing in QB version
END SUB
