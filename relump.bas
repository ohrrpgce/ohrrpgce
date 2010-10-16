'OHRRPGCE RELUMP - RPG File relumping utility
'(C) Copyright 2006 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
' To compile:
'        fbc -lang deprecated relump.bas util.bas
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE FUNCTION readkey$ ()
DECLARE FUNCTION editstr$ (stri$, key$, cur%, max%, number%)
DECLARE SUB fatalerror (e$)
DECLARE FUNCTION rightafter$ (s$, d$)
DECLARE FUNCTION getcurdir$ ()
DECLARE SUB fixorder (f$)
DECLARE SUB lumpfiles (listf$, lump$, path$)

'slight hackery to get more versatile read function
declare function fget alias "fb_FileGet" ( byval fnum as integer, byval pos as integer = 0, byval dst as any ptr, byval bytes as uinteger ) as integer
declare function fput alias "fb_FilePut" ( byval fnum as integer, byval pos as integer = 0, byval src as any ptr, byval bytes as uinteger ) as integer

#include "compat.bi"
#include "util.bi"
#include "const.bi"
#include "file.bi"

olddir$ = getcurdir

IF COMMAND$ = "" THEN
 PRINT "O.H.R.RPG.C.E. lumping utility"
 PRINT ""
 PRINT "syntax:"
 PRINT "relump folder filename"
 PRINT ""
 PRINT "A utility to package the contents of a folder into an OHRRPGCE"
 PRINT "lumpfile, such as an .RPG file"
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

IF RIGHT(src$,1)=SLASH THEN src$=LEFT(src$,LEN(src$)-1)

IF NOT isdir(src$) THEN
  IF isfile(src$) THEN fatalerror src$ + "' is a file, not a folder"
  fatalerror "rpgdir folder `" + src$ + "' was not found"
END IF

IF dest$ = "" THEN
 IF RIGHT(src$,7) = ".rpgdir" THEN
  dest$ = trimextension$(src$) + ".rpg"
 ELSE
  fatalerror "please specify an output folder"
 END IF
END IF

PRINT "From " + src$ + " to " + dest$

IF isfile(dest$) THEN
 PRINT "destination file " + dest$ + " already exists. Replace it? (y/n)"
 w$ = readkey
 IF w$ <> "Y" AND w$ <> "y" THEN SYSTEM
END IF

IF isdir(dest$) THEN fatalerror "destination file " + dest$ + " already exists as a folder."

'--build the list of files to lump
findfiles src$, ALLFILES, fileTypefile, NO, "temp.lst"
fixorder "temp.lst"
'---relump data into lumpfile package---
lumpfiles "temp.lst", dest$, src$ + SLASH
KILL "temp.lst"

SYSTEM

REM $STATIC

SUB fatalerror (e$)

IF e$ <> "" THEN PRINT "ERROR: " + e$
SYSTEM

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
filecopy f$, "fixorder.tmp"

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
   'scripts.txt and hs won't be located here anymore, but check them anyway
   'in case we are reluping a crashed playing.tmp from an old version
  CASE ELSE
   '--check extenstion
   c$ = RIGHT$(b$, 4)
   SELECT CASE c$
    CASE ".tmp", ".hsx", ".hsz"
     '--do nothing 
     'hsx and hsz script files shouldn't appear here anymore, but check for them anyway
     'in case we are reluping a crashed playing.tmp from an old version
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

SUB lumpfiles (listf$, lump$, path$)
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
