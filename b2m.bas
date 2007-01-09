'OHRRPGCE GAME - Main module
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z

'extern
declare sub bam2mid(infile as string, outfile as string, useOHRm as integer)
'subs and functions
declare sub debug(s$)
declare function isfile(n$) as integer

IF command$ = "" THEN
	PRINT "Usage: b2m filename"
	PRINT "Converts a BAM file into MIDI format"
ELSE
        bam2mid command$, "", 0
END IF

SUB debug (s$)
	print s$
END SUB

FUNCTION isfile (n$) as integer
	dim f as integer
	f = freefile
	open n$ for input as #f
	if err > 0 then
		isfile = 0
	else
		close #f
		isfile = 1
	end if
END FUNCTION
