'OHRRPGCE GAME - Compatibility functions, FreeBasic version
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
option explicit

#include compat.bi
#include allmodex.bi
#include cglobals.bi
#include fontdata.bi

dim shared seg as integer ptr

DECLARE SUB fatalerror (e$)
DECLARE FUNCTION small% (n1%, n2%)

SUB dummyclear(arg1%,arg2%,arg3%) 'dummy sub for compatibility
END SUB

SUB getdefaultfont(font() as integer)
	dim i as integer
	
	for i = 0 to 1023
		font(i) = font_data(i)
	next
END SUB

SUB xbload (f$, array(), e$)

	IF isfile(f$) THEN
		DIM ff%, byt as UByte, seg AS Short, offset AS Short, length AS Short
		dim ilength as integer
		dim i as integer

		ff = FreeFile
		OPEN f$ FOR BINARY AS #ff
		GET #ff,, byt 'Magic number, always 253
		IF byt <> 253 THEN fatalerror e$
		GET #ff,, seg 'Segment, no use anymore
		GET #ff,, offset 'Offset into the array, not used now
		GET #ff,, length 'Length
		'length is in bytes, so divide by 2, and subtract 1 because 0-based
		ilength = (length / 2) - 1
		
		dim buf(ilength) as short
		
		GET #ff,, buf()
		CLOSE #ff

		for i = 0 to small(ilength, ubound(array))
			array(i) = buf(i)	
		next i
		
		ELSE
		fatalerror e$
	END IF

END SUB

SUB xbsave (f$, array%(), bsize%)

	DIM ff%, byt as UByte, seg AS uShort, offset AS Short, length AS Short
	dim ilength as integer
	dim i as integer

	seg = &h9999
	offset = 0
	ilength = (bsize \ 2) - 1
	length = bsize	'bsize is in bytes
	byt = 253
	
	'copy array to shorts
	DIM buf(ilength) as short
	for i = 0 to small(ilength, ubound(array))
		buf(i) = array(i)
	next		
	
	ff = FreeFile
	OPEN f$ FOR BINARY AS #ff
	PUT #ff, , byt				'Magic number
	PUT #ff, , seg				'segment - obsolete
	PUT #ff, , offset			'offset - obsolete
	PUT #ff, , length			'size in bytes
	
	PUT #ff,, buf()
	CLOSE #ff
		
END SUB

SUB crashexplain()
	PRINT "Please report this exact error message to ohrrpgce@HamsterRepublic.com"
	PRINT "Be sure to describe in detail what you were doing when it happened"
	PRINT
	PRINT version$
	PRINT "Memory Info:"; FRE(0)
'#ifndef __FB_LINUX__
'	PRINT "Executable: "; progdir$ + exename$ + ".EXE"
'#else
'	PRINT "Executable: "; progdir$ + exename$
'#endif	
'	PRINT "RPG file: "; sourcerpg$
END SUB

'replacements for def seg and peek, use seg shared ptr 
'assumes def seg will always be used to point to an integer and
'that integers are only holding 2 bytes of data
sub defseg(byref var as integer)
	seg = @var
end sub

function xpeek(byval idx as integer) as integer
	dim as ubyte bval
	dim as integer hilow
	
	hilow = idx mod 2
	idx = idx \ 2
	
	if hilow = 0 then
		bval = seg[idx] and &hff
	else
		bval = (seg[idx] and &hff00) shr 8
	end if
	xpeek = bval
end function

sub xpoke(byval idx as integer, byval v as integer)
	dim as integer bval
	dim as integer hilow
	dim as integer newval
	
	hilow = idx mod 2
	idx = idx \ 2
	
	bval = v and &hff
	if hilow = 0 then
		newval = seg[idx] and &hff00
		seg[idx] = newval or bval
	else
		newval = seg[idx] and &hff
		seg[idx] = newval or (bval shl 8)
	end if
end sub