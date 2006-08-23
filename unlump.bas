'OHRRPGCE UNLUMP - RPG File unlumping utility
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
' To compile:
'        fbc unlump.bas util.bas
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE FUNCTION readkey$ ()
DECLARE FUNCTION editstr$ (stri$, key$, cur%, max%, number%)
DECLARE SUB fatalerror (e$)
DECLARE FUNCTION rightafter$ (s$, d$)
DECLARE SUB xbload (f$, array%(), e$)
DECLARE SUB readscatter (s$, lhold%, array%(), start%)
DECLARE FUNCTION rotascii$ (s$, o%)
'DECLARE FUNCTION readpassword$ ()

'assembly subs and functions
DECLARE SUB setwait (b(), BYVAL t)
DECLARE SUB dowait ()
DECLARE SUB setbit (b(), BYVAL w, BYVAL b, BYVAL v)
DECLARE FUNCTION readbit (b(), BYVAL w, BYVAL b)
DECLARE SUB copyfile (s$, d$, buf())
DECLARE SUB findfiles (fmask$, BYVAL attrib, outfile$, buf())
DECLARE SUB lumpfiles (listf$, lump$, path$, buffer())
DECLARE SUB unlump (lump$, ulpath$, buffer())
DECLARE SUB unlumpfile (lump$, fmask$, path$, buf())
DECLARE FUNCTION islumpfile (lump$, fmask$)
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

'$INCLUDE: 'util.bi'

CONST true = -1
CONST false = 0

DIM buffer(16383)

DIM SHARED createddir = false, dest$, olddir$

olddir$ = curdir

IF COMMAND$ = "" THEN
 PRINT "O.H.R.RPG.C.E. game unlumping utility"
 PRINT ""
 PRINT "syntax:"
 PRINT "unlump filename.rpg directory"
 PRINT ""
 PRINT "A utility to extract the contents of an RPG file or other lumped
 PRINT "to a directory so that advanced users can hack the delicious"
 PRINT "morsels inside."
 PRINT "If a password is required, you will be prompted to enter it."
 PRINT ""
 PRINT "Windows users can drag-and-drop their RPG file onto this program"
 PRINT "to unlump it."
 PRINT ""
 PRINT "[Press a Key]"
 dummy$ = readkey$()
 fatalerror ""
END IF

lump$ = COMMAND$(1)
dest$ = COMMAND$(2)

'check whether it is an RPG file (assume all RPG files contain BROWSE.TXT)
isrpg = islumpfile(lump$, "browse.txt")

IF dest$ = "" THEN
 IF isrpg THEN
  dest$ = trimextension$(lump$) + ".rpgdir"
 ELSE
  dest$ = trimextension$(lump$) + ".unlmp"
 END IF
 IF LEN(rightafter(lump$, ".")) > 3 OR dest$ = "" THEN fatalerror "please specify an output directory"
END IF

IF NOT isfile(lump$) THEN fatalerror "lump file `" + lump$ + "' was not found"

PRINT "From " + lump$ + " to " + dest$

'--Get old-style game$ (only matters for ancient RPG files that are missing the archinym.lmp)
game$ = rightafter(lump$, "\")
IF game$ = "" THEN game$ = lump$
IF INSTR(game$, ".") THEN game$ = trimextension$(game$)

IF isfile(dest$) THEN fatalerror "destination directory `" + dest$ + "' already exists as a file"

IF isdir(dest$) THEN
 PRINT "destination directory `" + dest$ + "' already exists. use it anyway? (y/n)"
 w$ = readkey
 IF w$ <> "Y" AND w$ <> "y" THEN SYSTEM
ELSE
 MKDIR dest$
 createddir = true
END IF

IF NOT isdir(dest$) THEN fatalerror "unable to create destination directory `" + dest$ + "'"

IF NOT isrpg THEN
 unlump lump$, dest$ + "\", buffer()
 CHDIR olddir$
 SYSTEM
END IF
 
unlumpfile lump$, "archinym.lmp", dest$ + "\", buffer()

'--set game$ according to the archinym
IF isfile(dest$ + "\archinym.lmp") THEN
 fh = FREEFILE
 OPEN dest$ + "\archinym.lmp" FOR INPUT AS #fh
 LINE INPUT #fh, a$
 CLOSE #fh
 IF LEN(a$) <= 8 THEN
  game$ = a$
 END IF
 KILL dest$ + "\archinym.lmp"
END IF

unlumpfile lump$, game$ + ".gen", dest$ + "\", buffer()
xbload dest$ + "\" + game$ + ".gen", buffer(), "unable to open general data"

KILL dest$ + "\" + game$ + ".gen"

passokay = true

IF buffer(94) > -1 THEN
 passokay = false
 '----load password-----
 'Note that this is still using the old 2nd-style password format, not the
 'newer simpler 3rd-style password format. This is okay for now, since
 'CUSTOM writes both 2nd and 3rd style passwords, but supporting 3rd-style
 'here also would be desireable
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
   passokay = true
   EXIT DO
  END IF
  LOCATE , 1: FOR i = 1 TO LEN(pas$): PRINT " "; : NEXT i
  pas$ = editstr(pas$, w$, cur, 17, false)
  LOCATE , 1: FOR i = 1 TO LEN(pas$): PRINT "*"; : NEXT i
  sleep 80,1
 LOOP
END IF

IF passokay THEN
 REDIM buffer(32767)
 unlump lump$, dest$ + "\", buffer()
END IF

CHDIR olddir$

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

'RMDIR does not work unless isdir$ is called first. If I tried to figure out why, my brain would explode
isdir$(dest$)
IF createddir THEN RMDIR dest$
SYSTEM

END SUB

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
new$ = ""

FOR i = LEN(s$) TO 1 STEP -1
 IF MID$(s$, i, 1) = d$ THEN
  rightafter$ = new$
  EXIT FOR
 END IF
 new$ = MID$(s$, i, 1) + new$
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
	isdir = NOT (dir$(sDir$, 16) = "")
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

SUB unlumpfile (lump$, fmask$, path$, buf() as integer)
	dim lf as integer
	dim dat as ubyte
	dim size as integer
	dim maxsize as integer
	dim lname as string
	dim i as integer
	dim bufr as ubyte ptr
	dim nowildcards as integer = 0

	lf = freefile
	open lump$ for binary access read as #lf
	if err > 0 then
		'debug "Could not open file " + lump$
		exit sub
	end if
	maxsize = LOF(lf)

	bufr = allocate(16383)
	if bufr = null then
		close #lf
		exit sub
	end if

	'should make browsing a bit faster
	if len(fmask$) > 0 then
		if instr(fmask$, "*") = 0 and instr(fmask$, "?") = 0 then
			nowildcards = -1
		end if
	end if

	get #lf, , dat	'read first byte
	while not eof(lf)
		'get lump name
		lname = ""
		i = 0
		while not eof(lf) and dat <> 0 and i < 64
			lname = lname + chr$(dat)
			get #lf, , dat
			i += 1
		wend
		if i > 50 then 'corrupt file, really if i > 12
			'debug "corrupt lump file: lump name too long"
			exit while
		end if
		'force to lower-case
		lname = lcase(lname)
		'debug "lump name " + lname

		if instr(lname, "\") or instr(lname, "/") then
			'debug "unsafe lump name " + str$(lname)
			exit while
		end if

		if not eof(lf) then
			'get lump size - byte order = 3,4,1,2 I think
			get #lf, , dat
			size = (dat shl 16)
			get #lf, , dat
			size = size or (dat shl 24)
			get #lf, , dat
			size = size or dat
			get #lf, , dat
			size = size or (dat shl 8)
			if size > maxsize then
				'debug "corrupt lump size" + str$(size) + " exceeds source size" + str$(maxsize)
				exit while
			end if

			'debug "lump size " + str$(size)

			'do we want this file?
			if matchmask(lname, lcase$(fmask$)) then
				'write yon file
				dim of as integer
				dim csize as integer
				dim osize as integer
				
				osize = size

				of = freefile
				open path$ + lname for binary access write as #of
				if err > 0 then
					'debug "Could not open file " + path$ + lname
					exit while
				end if

				'copy the data
				do while size > 0
					if size > 16383 then
						csize = 16383
					else
						csize = size
					end if
					'copy a chunk of file
					get #lf, , *bufr, csize
					put #of, , *bufr, csize
					size -= csize
					if size > osize then size = 0
				loop

				close #of

				'early out if we're only looking for one file
				if nowildcards then exit while
			else
				'skip to next name
				i = seek(lf)
				i = i + size
				seek #lf, i
			end if

			if not eof(lf) then
				get #lf, , dat
			end if
		end if
	wend

	deallocate bufr
	close #lf

end SUB

SUB unlump (lump$, ulpath$, buffer() as integer)
	unlumpfile(lump$, "", ulpath$, buffer())
end SUB

FUNCTION islumpfile (lump$, fmask$)
	dim lf as integer
	dim dat as ubyte
	dim size as integer
	dim maxsize as integer
	dim lname as string
	dim i as integer

    islumpfile = 0

	lf = freefile
	open lump$ for binary access read as #lf
	if err > 0 then
		'debug "Could not open file " + lump$
		exit function
	end if
	maxsize = LOF(lf)

    get #lf, , dat	'read first byte
	while not eof(lf)
		'get lump name
		lname = ""
		i = 0
		while not eof(lf) and dat <> 0 and i < 64
			lname = lname + chr$(dat)
			get #lf, , dat
			i += 1
		wend
		if i > 50 then 'corrupt file, really if i > 12
			'debug "corrupt lump file: lump name too long"
			exit while
		end if
		'force to lower-case
		lname = lcase(lname)
		'debug "lump name " + lname

		if instr(lname, "\") or instr(lname, "/") then
			'debug "unsafe lump name " + str$(lname)
			exit while
		end if

		if not eof(lf) then
			'get lump size - byte order = 3,4,1,2 I think
			get #lf, , dat
			size = (dat shl 16)
			get #lf, , dat
			size = size or (dat shl 24)
			get #lf, , dat
			size = size or dat
			get #lf, , dat
			size = size or (dat shl 8)
			if size > maxsize then
				'debug "corrupt lump size" + str$(size) + " exceeds source size" + str$(maxsize)
				exit while
			end if

			'do we want this file?
			if matchmask(lname, lcase$(fmask$)) then
                islumpfile = -1
                exit function
			else
				'skip to next name
				seek #lf, seek(lf) + size
			end if

			if not eof(lf) then
				get #lf, , dat
			end if
		end if
	wend

	close #lf
end FUNCTION

'FUNCTION readpassword$
'
''--read a 17-byte string from GEN at word offset 7
''--(Note that array2str uses the byte offset not the word offset)
's$ = STRING$(17, 0)
'array2str general(), 14, s$
'
''--reverse ascii rotation / weak obfuscation
's$ = rotascii(s$, general(6) * -1)
'
''-- discard ascii chars lower than 32
'p$ = ""
'FOR i = 1 TO 17
' c$ = MID$(s$, i, 1)
' IF ASC(c$) >= 32 THEN p$ = p$ + c$
'NEXT i
'
'readpassword$ = p$
'
'END FUNCTION
