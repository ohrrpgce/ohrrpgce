'OHRRPGCE RELUMP - RPG File relumping utility
'(C) Copyright 2006 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
' To compile:
'        fbc relump.bas util.bas
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE FUNCTION readkey$ ()
DECLARE FUNCTION editstr$ (stri$, key$, cur%, max%, number%)
DECLARE SUB fatalerror (e$)
DECLARE FUNCTION rightafter$ (s$, d$)
DECLARE SUB forcewd (wd$)
DECLARE FUNCTION getcurdir$ ()
DECLARE SUB xbload (f$, array%(), e$)
DECLARE SUB readscatter (s$, lhold%, array%(), start%)
DECLARE FUNCTION rotascii$ (s$, o%)
DECLARE SUB fixorder (f$)

'assembly subs and functions
DECLARE SUB setwait (b(), BYVAL t)
DECLARE SUB dowait ()
DECLARE SUB setbit (b(), BYVAL w, BYVAL b, BYVAL v)
DECLARE FUNCTION readbit (b(), BYVAL w, BYVAL b)
DECLARE SUB copyfile (s$, d$, buf())
DECLARE SUB findfiles (fmask$, BYVAL attrib, outfile$, buf())
DECLARE SUB lumpfiles (listf$, lump$, path$, buffer())
DECLARE SUB array2str (arr(), BYVAL o, s$)
DECLARE SUB str2array (s$, arr(), BYVAL o)
DECLARE SUB getstring (path$)
DECLARE FUNCTION rpathlength ()
DECLARE FUNCTION envlength (e$)
DECLARE FUNCTION drivelist (drbuf())
DECLARE FUNCTION isfile (n$)
DECLARE FUNCTION isdir (dir$)
DECLARE FUNCTION isremovable (BYVAL d)
DECLARE FUNCTION isvirtual (BYVAL d)
DECLARE FUNCTION hasmedia (BYVAL d)

'slight hackery to get more versatile read function
declare function fget alias "fb_FileGet" ( byval fnum as integer, byval pos as integer = 0, byval dst as any ptr, byval bytes as uinteger ) as integer
declare function fput alias "fb_FilePut" ( byval fnum as integer, byval pos as integer = 0, byval src as any ptr, byval bytes as uinteger ) as integer

#include "util.bi"

#IFDEF __FB_LINUX__
 CONST SLASH = "/"
 CONST ALLFILES = "*"
#ELSE
 CONST SLASH = "\"
 CONST ALLFILES = "*.*"
#ENDIF

COMMON SHARED buffer() AS INTEGER
DIM buffer(32767) AS INTEGER

olddir$ = getcurdir

IF COMMAND$ = "" THEN
 PRINT "O.H.R.RPG.C.E. game relumping utility"
 PRINT ""
 PRINT "syntax:"
 PRINT "relump folder filename.rpg"
 PRINT ""
 PRINT "A utility to package the contents of an RPG folder back into"
 PRINT "a RPG format lumpfile."
 PRINT "If a password is required, you will be prompted to enter it."
 PRINT ""
 PRINT "Windows users can drag-and-drop their rpgdir file onto this program"
 PRINT "to relump it."
 PRINT ""
 PRINT "[Press a Key]"
 dummy$ = readkey$()
 fatalerror ""
END IF

src$ = COMMAND$(1)
dest$ = COMMAND$(2)
IF dest$ = "" THEN
 dest$ = trimextension$(src$) + ".rpg"
 IF dest$ = "" THEN fatalerror "please specify an output folder"
END IF

IF NOT isdir(src$) THEN
  IF isfile(src$) THEN fatalerror src$ + "' is a file, not a folder"
  fatalerror "rpgdir folder `" + src$ + "' was not found"
END IF

PRINT "From " + src$ + " to " + dest$

IF isfile(dest$) THEN
 PRINT "destination file " + dest$ + " already exists. Replace it? (y/n)"
 w$ = readkey
 IF w$ <> "Y" AND w$ <> "y" THEN SYSTEM
END IF

IF isdir(dest$) THEN fatalerror "destination file " + dest$ + " already exists as a folder."

'--set game$ according to the archinym
IF isfile(src$ + SLASH + "archinym.lmp") THEN
 fh = FREEFILE
 OPEN src$ + SLASH + "archinym.lmp" FOR INPUT AS #fh
 LINE INPUT #fh, a$
 CLOSE #fh
 IF LEN(a$) <= 8 THEN
  game$ = a$
 END IF
ELSE
 PRINT "WARNING: " + src$ + SLASH + "archinym.lmp is missing."
 game$ ="ohrrpgce"
END IF

xbload src$ + SLASH + game$ + ".gen", buffer(), "unable to open general data"

passokay = -1

IF buffer(94) > -1 THEN
 passokay = 0
 '----load password-----
 'Note that this is still using the old 2nd-style password format, not the
 'newer simpler 3rd-style password format. This is okay for now, since
 'CUSTOM writes both 2nd and 3rd style passwords, but supporting 3rd-style
 'here also would be desireable
 rpas$ = ""
 readscatter rpas$, buffer(94), buffer(), 200
 rpas$ = rotascii(rpas$, buffer(93) * -1)
 'PRINT rpas$
 '-----get inputed password-----
 print "Password Required"
 pas$ = ""
 DO
  w$ = readkey$
  IF w$ = CHR$(13) THEN
   PRINT ""
   IF pas$ <> rpas$ THEN fatalerror "password mismatch"
   passokay = -1
   EXIT DO
  END IF
  LOCATE , 1: FOR i = 1 TO LEN(pas$): PRINT " "; : NEXT i
  cur = 0
  pas$ = editstr(pas$, w$, cur, 17, 0)
  LOCATE , 1: FOR i = 1 TO LEN(pas$): PRINT "*"; : NEXT i
  sleep 80,1
 LOOP
END IF

IF passokay THEN
 '--build the list of files to lump
 findfiles src$ + SLASH + ALLFILES, 0, "temp.lst", buffer()
 fixorder "temp.lst"
 '---relump data into lumpfile package---
 lumpfiles "temp.lst", dest$, src$ + SLASH, buffer()
 KILL "temp.lst"
END IF

SYSTEM

REM $STATIC
FUNCTION editstr$ (stri$, key$, cur, max, number)

pre$ = LEFT$(stri$, cur)
post$ = RIGHT$(stri$, LEN(stri$) - cur)

SELECT CASE key$
 CASE CHR$(8)
  'backspace
  IF LEN(pre$) > 0 THEN pre$ = LEFT$(pre$, LEN(pre$) - 1): cur = cur - 1
 CASE CHR$(0) + CHR$(83)
  'delete
  IF LEN(post$) > 0 THEN post$ = RIGHT$(post$, LEN(post$) - 1)
 CASE ELSE
  IF LEN(key$) > 0 THEN
   IF (ASC(key$) >= 32 AND ASC(key$) < 127 AND key$ <> "," AND key$ <> "~" AND number = 0) OR (ASC(key$) >= 48 AND ASC(key$) <= 57 AND number) THEN
    IF LEN(post$) = 0 AND LEN(pre$) < max THEN post$ = " "
    IF LEN(post$) > 0 THEN
     MID$(post$, 1, 1) = key$
     cur = bound(cur + 1, 0, LEN(pre$ + post$))
    END IF
   END IF
  END IF
END SELECT

editstr$ = pre$ + post$


END FUNCTION

SUB fatalerror (e$)

IF e$ <> "" THEN PRINT "ERROR: " + e$
SYSTEM

END SUB

SUB forcewd (wd$)

CHDIR wd$

END SUB

FUNCTION getcurdir$

getcurdir$ = curdir

END FUNCTION

FUNCTION readkey$

w$ = ""
WHILE w$ = ""
 w$ = INKEY$
WEND

readkey$ = w$

END FUNCTION

SUB readscatter (s$, lhold, array(), start)
DIM stray(10)
s$ = STRING$(20, "!")

FOR i = 0 TO lhold
 setbit stray(), 0, i, readbit(array(), start - 1, array(start + i))
NEXT i

array2str stray(), 0, s$
s$ = LEFT$(s$, INT((lhold + 1) / 8))

END SUB

FUNCTION rightafter$ (s$, d$)

rightafter$ = ""
result$ = ""

FOR i = LEN(s$) TO 1 STEP -1
 IF MID$(s$, i, 1) = d$ THEN
  rightafter$ = result$
  EXIT FOR
 END IF
 result$ = MID$(s$, i, 1) + result$
NEXT i

END FUNCTION

FUNCTION rotascii$ (s$, o)

temp$ = ""

FOR i = 1 TO LEN(s$)
 temp$ = temp$ + CHR$(loopvar(ASC(MID$(s$, i, 1)), 0, 255, o))
NEXT i

rotascii$ = temp$

END FUNCTION

SUB xbload (f$, array(), e$)

    ' Linux compat demands lowercase lump names
    f$ = lcase(f$)

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

FUNCTION readbit (bb() as integer, BYVAL w as integer, BYVAL b as integer)  as integer
	dim mask as uinteger
	dim woff as integer
	dim wb as integer

	woff = w + (b \ 16)
	wb = b mod 16

	mask = 1 shl wb

	if (bb(woff) and mask) then
		readbit = 1
	else
		readbit = 0
	end if
end FUNCTION

SUB setbit (bb() as integer, BYVAL w as integer, BYVAL b as integer, BYVAL v as integer)
	dim mask as uinteger
	dim woff as integer
	dim wb as integer

	woff = w + (b \ 16)
	wb = b mod 16

	if woff > ubound(bb) then
		exit sub
	end if

	mask = 1 shl wb
	if v = 1 then
		bb(woff) = bb(woff) or mask
	else
		mask = not mask
		bb(woff) = bb(woff) and mask
	end if
end SUB

SUB array2str (arr() AS integer, BYVAL o AS integer, s$)
'String s$ is already filled out with spaces to the requisite size
'o is the offset in bytes from the start of the buffer
'the buffer will be packed 2 bytes to an int, for compatibility, even
'though FB ints are 4 bytes long  ** leave like this? not really wise
	DIM i AS Integer
	dim bi as integer
	dim bp as integer ptr
	dim toggle as integer

	bp = @arr(0)
	bi = o \ 2 'offset is in bytes
	toggle = o mod 2

	for i = 0 to len(s$) - 1
		if toggle = 0 then
			s$[i] = bp[bi] and &hff
			toggle = 1
		else
			s$[i] = (bp[bi] and &hff00) shr 8
			toggle = 0
			bi = bi + 1
		end if
	next

END SUB

FUNCTION isfile (n$) as integer
    ' I'm assuming we don't count directories as files
	'return dir$(n$) <> ""
    return dir$(n$, 255 xor 16) <> ""
END FUNCTION

FUNCTION isdir (sDir$) as integer
#IFDEF __FB_LINUX__
	'Special hack for broken Linux dir$() behavior
	isdir = 0
	SHELL "if [ -d """ + sDir$ + """ ] ; then echo dir ; fi > isdirhack.tmp"
	DIM AS INTEGER fh
	fh = FREEFILE
	OPEN "isdirhack.tmp" FOR INPUT AS #fh
	DIM s$
	LINE INPUT #fh, s$
	IF TRIM$(s$) = "dir" THEN isdir = -1
	CLOSE #fh
	KILL "isdirhack.tmp"
#ELSE
	'Windows just uses dir
	dim ret as integer = dir$(sDir$, 55) <> "" AND dir$(sDir$, 39) = ""
	return ret
#ENDIF
END FUNCTION

function matchmask(match as string, mask as string) as integer
	dim i as integer
	dim m as integer
	dim si as integer, sm as integer

	'special cases
	if mask = "" then
		matchmask = 1
		exit function
	end if

	i = 0
	m = 0
	while (i < len(match)) and (m < len(mask)) and (mask[m] <> asc("*"))
		if (match[i] <> mask[m]) and (mask[m] <> asc("?")) then
			matchmask = 0
			exit function
		end if
		i = i+1
		m = m+1
	wend

	if (m >= len(mask)) and (i < len(match)) then
		matchmask = 0
		exit function
	end if

	while i < len(match)
		if m >= len(mask) then
			'run out of mask with string left over, rewind
			i = si + 1 ' si will always be set by now because of *
			si = i
			m = sm
		else
			if mask[m] = asc("*") then
				m = m + 1
				if m >= len(mask) then
					'* eats the rest of the string
					matchmask = 1
					exit function
				end if
				i = i + 1
				'store the positions in case we need to rewind
				sm = m
				si = i
			else
				if (mask[m] = match[i]) or (mask[m] = asc("?")) then
					'ok, next
					m = m + 1
					i = i + 1
				else
					'mismatch, rewind to last * positions, inc i and try again
					m = sm
					i = si + 1
					si = i
				end if
			end if
		end if
	wend

  	while (m < len(mask)) and (mask[m] = asc("*"))
  		m = m + 1
  	wend

  	if m < len(mask) then
		matchmask = 0
	else
		matchmask = 1
	end if

end function

SUB fixorder (f$)
copyfile f$, "fixorder.tmp", buffer()

ofh = FREEFILE
OPEN f$ FOR OUTPUT AS #ofh

ifh = FREEFILE
OPEN "fixorder.tmp" FOR INPUT AS #ifh

'--first output the archinym.lmp and browse.txt files
WHILE NOT EOF(ifh)
 LINE INPUT #ifh, a$
 b$ = LCASE$(a$)
 IF b$ = "archinym.lmp" OR b$ = "browse.txt" THEN
  PRINT #ofh, a$
 END IF
WEND

'--close and re-open
CLOSE #ifh
OPEN "fixorder.tmp" FOR INPUT AS #ifh

'--output the other files, excluding illegal files
WHILE NOT EOF(ifh)
 LINE INPUT #ifh, a$
 b$ = LCASE$(a$)
 SELECT CASE b$
  CASE "archinym.lmp", "browse.txt", "scripts.txt", "hs"
   '--do nothing
  CASE ELSE
   '--check extenstion
   c$ = RIGHT$(b$, 4)
   SELECT CASE c$
    CASE ".tmp", ".hsx", ".hsz"
     '--do nothing
    CASE ELSE
     '--output all other names
     PRINT #ofh, a$
   END SELECT
 END SELECT
WEND
CLOSE #ifh
CLOSE #ofh
KILL "fixorder.tmp"
END SUB

SUB copyfile (s$, d$, buf() as integer)
	dim bufr as ubyte ptr
	dim as integer fi, fo, size, csize

	fi = freefile
	open s$ for binary access read as #fi
	if err <> 0 then
		exit sub
	end if

	fo = freefile
	open d$ for binary access write as #fo
	if err <> 0 then
		close #fi
		exit sub
	end if

	size = lof(fi)

	if size < 16000 then
		bufr = allocate(size)
		'copy a chunk of file
		fget(fi, , bufr, size)
		fput(fo, , bufr, size)
	else
		bufr = allocate(16000)

		'write lump
		while size > 0
			if size > 16000 then
				csize = 16000
			else
				csize = size
			end if
			'copy a chunk of file
			fget(fi, , bufr, csize)
			fput(fo, , bufr, csize)
			size = size - csize
		wend
	end if

	deallocate bufr
	close #fi
	close #fo

end SUB

SUB findfiles (fmask$, BYVAL attrib, outfile$, buf())
    ' attrib 0: all files 'cept folders, attrib 16: folders only
#ifdef __FB_LINUX__
        'this is pretty hacky, but works around the lack of DOS-style attributes, and the apparent uselessness of DIR$
	DIM grep$
	grep$ = "-v '/$'"
	IF attrib AND 16 THEN grep$ = "'/$'"
	DIM i%
	FOR i = LEN(fmask$) TO 1 STEP -1
		IF MID$(fmask$, i, 1) = CHR$(34) THEN fmask$ = LEFT$(fmask$, i - 1) + SLASH + CHR$(34) + RIGHT$(fmask$, LEN(fmask$) - i)
	NEXT i
	i = INSTR(fmask$, "*")
	IF i THEN
		fmask$ = CHR$(34) + LEFT$(fmask$, i - 1) + CHR$(34) + RIGHT$(fmask$, LEN(fmask$) - i + 1)
	ELSE
		fmask$ = CHR$(34) + fmask$ + CHR$(34)
	END IF
	SHELL "ls -d1p " + fmask$ + "|grep "+ grep$ + ">" + outfile$ + ".tmp"
	DIM AS INTEGER f1, f2
	f1 = FreeFile
	OPEN outfile$ + ".tmp" FOR INPUT AS #f1
	f2 = FreeFile
	OPEN outfile$ FOR OUTPUT AS #f2
	DIM s$
	DO UNTIL EOF(f1)
		LINE INPUT #f1, s$
		IF RIGHT$(s$, 1) = "/" THEN s$ = LEFT$(s$, LEN(s$) - 1)
		DO WHILE INSTR(s$, "/")
			s$ = RIGHT$(s$, LEN(s$) - INSTR(s$, "/"))
		LOOP
		PRINT #f2, s$
	LOOP
	CLOSE #f1
	CLOSE #f2
	KILL outfile$ + ".tmp"
#else
    DIM a$, i%, folder$
	if attrib = 0 then attrib = 255 xor 16

	FOR i = LEN(fmask$) TO 1 STEP -1
        IF MID$(fmask$, i, 1) = SLASH THEN folder$ = MID$(fmask$, 1, i): EXIT FOR
    NEXT

	dim tempf%, realf%
	tempf = FreeFile
	OPEN outfile$ + ".tmp" FOR OUTPUT AS #tempf
	a$ = DIR$(fmask$, attrib)
	if a$ = "" then
		close #tempf
		exit sub
	end if
	DO UNTIL a$ = ""
		PRINT #tempf, a$
		a$ = DIR$("", attrib)
	LOOP
	CLOSE #tempf
    OPEN outfile$ + ".tmp" FOR INPUT AS #tempf
    realf = FREEFILE
    OPEN outfile$ FOR OUTPUT AS #realf
    DO UNTIL EOF(tempf)
        LINE INPUT #tempf, a$
        IF attrib = 16 THEN
            'alright, we want directories, but DIR$ is too broken to give them to us
            'files with attribute 0 appear in the list, so single those out
            IF DIR$(folder$ + a$, 255 xor 16) = "" THEN PRINT #realf, a$
        ELSE
            PRINT #realf, a$
        END IF
    LOOP
    CLOSE #tempf
    CLOSE #realf
    KILL outfile$ + ".tmp"
#endif
END SUB

SUB lumpfiles (listf$, lump$, path$, buffer())
	dim as integer lf, fl, tl	'lumpfile, filelist, tolump

	dim dat as ubyte
	dim size as integer
	dim lname as string
	dim bufr as ubyte ptr
	dim csize as integer
	dim as integer i, t, textsize(1)

	fl = freefile
	open listf$ for input as #fl
	if err <> 0 then
		PRINT "Filed to open listfile " + listf$
		exit sub
	end if

	lf = freefile
	open lump$ for binary access write as #lf
	if err <> 0 then
		PRINT "Could not open file " + lump$
		close #fl
		exit sub
	end if

	bufr = allocate(16000)

	'get file to lump
	do until eof(fl)
		line input #fl, lname
		
		'validate that lumpname is 8.3 or ignore the file
		textsize(0) = 0
		textsize(1) = 0
		t = 0
		for i = 0 to len(lname)-1
			if lname[i] = asc(".") then t = 1
			textsize(t) += 1
		next
		'note extension includes the "." so can be 4 chars
		if textsize(0) > 8 or textsize(1) > 4 then
			PRINT "name too long: " + lname
			PRINT " name = " + str(textsize(0)) + ", ext = " + str(textsize(1))
			continue do
		end if

		tl = freefile
		open path$ + lname for binary access read as #tl
		if err <> 0 then
			PRINT "failed to open " + path$ + lname
			continue do
		end if

		'write lump name (seems to need to be upper-case, at least
		'for any files opened with unlumpone in the QB version)
		put #lf, , ucase(lname)
		dat = 0
		put #lf, , dat

		'write lump size - byte order = 3,4,1,2 I think
		size = lof(tl)
		dat = (size and &hff0000) shr 16
		put #lf, , dat
		dat = (size and &hff000000) shr 24
		put #lf, , dat
		dat = size and &hff
		put #lf, , dat
		dat = (size and &hff00) shr 8
		put #lf, , dat

		'write lump
		while size > 0
			if size > 16000 then
				csize = 16000
			else
				csize = size
			end if
			'copy a chunk of file
			fget(tl, , bufr, csize)
			fput(lf, , bufr, csize)
			size = size - csize
		wend

		close #tl
	loop

	close #lf
	close #fl

	deallocate bufr
END SUB
