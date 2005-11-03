'' FBOHR COMPATIBILITY FUNCTIONS
'' GPL and stuff. See LICENSE.txt.
'
#define DEMACRO
#define USE_ALLEGRO 0
'$include: 'compat.bi'
'$include: 'allmodex.bi'
'$include: 'gglobals.bi'

option explicit

declare function matchmask(match as string, mask as string) as integer
declare function calcblock(byval x as integer, byval y as integer, byval t as integer) as integer
declare sub bamconvert(bamfile as string, songfile as string)

'slight hackery to get more versatile read function
declare function fget alias "fb_FileGet" ( byval fnum as integer, byval pos as integer = 0, byval dst as any ptr, byval bytes as uinteger ) as integer
declare function fput alias "fb_FilePut" ( byval fnum as integer, byval pos as integer = 0, byval src as any ptr, byval bytes as uinteger ) as integer
declare function smouse alias "fb_SetMouse" ( byval x as integer = -1, byval y as integer = -1, byval cursor as integer = -1 ) as integer

'extern
declare sub debug(s$)
declare sub fatalerror(e$)
declare sub bam2mid(infile as string, outfile as string)

dim shared path as string
dim shared vispage as integer
dim shared wrkpage as integer

dim shared bptr as integer ptr	' buffer 
dim shared bsize as integer
dim shared bpage as integer

dim shared bordertile as integer
dim shared aptr as integer ptr	' array ptr
dim shared pptr as integer ptr	' pass ptr
dim shared maptop as integer
dim shared maplines as integer
dim shared map_x as integer
dim shared map_y as integer

dim shared anim1 as integer
dim shared anim2 as integer

dim shared waittime as single
dim shared keybd(0 to 255) as integer
dim shared keytime(0 to 255) as integer

dim shared stacktop as ubyte ptr
dim shared stackptr as ubyte ptr
dim shared stacksize as integer

dim shared mouse_xmin as integer
dim shared mouse_xmax as integer
dim shared mouse_ymin as integer
dim shared mouse_ymax as integer

dim shared textfg as integer
dim shared textbg as integer

dim shared music_on as integer = 0
dim shared music_vol as integer
dim shared music_paused as integer
dim shared orig_vol as integer = -1

dim shared fontdata(0 to 2048-1) as ubyte





SUB setmapdata (array() as integer, pas() as integer, BYVAL t as integer, BYVAL b as integer)
'I think this is a setup routine like setpicstuf
't and b are top and bottom margins
	map_x = array(0)
	map_y = array(1)
	aptr = @array(2)
	pptr = @pas(2)
	maptop = t
	maplines = 200 - t - b
end SUB

SUB setmapblock (BYVAL x as integer, BYVAL y as integer, BYVAL v as integer)
	dim index as integer
	dim hilow as integer
	
	index = (map_x * y) + x	'raw byte offset
	hilow = index mod 2		'which byte in word
	index = index shr 1 	'divide by 2
	
	if hilow > 0 then
		'delete original value
		aptr[index] = aptr[index] and &hff 
		'set new value
		aptr[index] = aptr[index] or ((v and &hff) shl 8)
	else
		'delete original value
		aptr[index] = aptr[index] and &hff00
		'set new value
		aptr[index] = aptr[index] or (v and &hff)
	end if

end SUB

FUNCTION readmapblock (BYVAL x as integer, BYVAL y as integer) as integer
	dim block as integer
	dim index as integer
	dim hilow as integer
	
	index = (map_x * y) + x	'raw byte offset
	hilow = index mod 2		'which byte in word
	index = index shr 1 	'divide by 2
	
	if hilow > 0 then
		block = (aptr[index] and &hff00) shr 8
	else
		block = aptr[index] and &hff
	end if
	
	readmapblock = block
end FUNCTION


SUB setanim (BYVAL cycle1 as integer, BYVAL cycle2 as integer)
	anim1 = cycle1
	anim2 = cycle2
end SUB

SUB setoutside (BYVAL defaulttile as integer)
	bordertile = defaulttile
end SUB

SUB interruptx (intnum as integer,inreg AS RegType, outreg AS RegType) 'not required
end SUB

FUNCTION Keyseg () as integer	'not required
	keyseg = 0
end FUNCTION

FUNCTION keyoff () as integer	'not required
	keyoff = 0
end FUNCTION

FUNCTION keyval (BYVAL a as integer) as integer
	keyval = keybd(a)
end FUNCTION

FUNCTION getkey () as integer
	dim i as integer, key as integer
	while inkey$ <> "": wend
	
	key = 0

	do
		'I think this wants a scancode, and the only way I can see is to check
		'them all
		for i=0 to &h80
			if multikey(i) then
				key = i
				exit for
			end if
		next
		sleep 50
	loop while key = 0
	
	getkey = key
end FUNCTION

SUB setkeys ()
'Quite nasty. Moved all this functionality from keyval() because this
'is where it seems to happen in the original.
'The keytime array is used to store the repeat delay timeout so that a 
'key "event" will only fire once every now and again rather than every
'time this function is called while the key is down. This probably doesn't
'need to be per key, but it mostly works, so I'll leave it be.
'Actual key state goes in keybd array for retrieval via keyval().
	dim ktime as integer
	dim a as integer
	
	ktime = int(timer() * 1000)
	
	'set key state for every key
	'highest scancode in fbgfx.bi is &h79, no point overdoing it
	for a = 0 to &h80 
		keybd(a) = 0 'default to not pressed
		if multikey(a) < 0 then
			'key is down
			if ktime > keytime(a) then
				'ok to fire a key event
				keybd(a) = 2
				if ktime > keytime(a) + 1000 then
					keytime(a) = ktime + 200
				else
					keytime(a) = ktime + 50
				end if
			else
				'NOTE there are 86,400,000 thousandths in a day
				if keytime(a) > 80000000 and ktime < 1800000 then
					'check wrap over midnight (stupid timer)
					keytime(a) = keytime(a) - 86400000
					if ktime > keytime(a) then
						keybd(a) = 2
						if keytime(a) = -1 or ktime > keytime(a) + 1000 then
							keytime(a) = ktime + 200
						else
							keytime(a) = ktime + 50
						end if
					end if
				end if
			end if
		else
			keytime(a) = -1
		end if
	next
end SUB

SUB setwait (b() as integer, BYVAL t as integer)
't is a value in milliseconds which, in the original, is used to set the event 
'frequency and is also used to set the wait time, but the resolution of the 
'dos timer means that the latter is always truncated to the last multiple of
'55 milliseconds.
	dim millis as integer
	dim secs as single
	millis = (t \ 55) * 55
	
	secs = millis / 1000
	waittime = timer + secs
end SUB

SUB dowait ()
'wait until alarm time set in setwait()
'In freebasic, sleep is in 1000ths, and a value of less than 100 will not 
'be exited by a keypress, so sleep for 5ms until timer > waittime.
	do while timer <= waittime
		sleep 5 'is this worth it?
	loop
end SUB


SUB textcolor (BYVAL f as integer, BYVAL b as integer)
	textfg = f
	textbg = b
end SUB

SUB setfont (f() as integer) 
' no bounds checking, probably not smart
' 2 characters per int
	dim i as integer
	dim j as integer
	dim t as integer
	j = 0
	t = 0
	for i = 0 to (256 * 8) - 1
		if t = 0 then
			fontdata(i) = f(j) and &hff
			t = 1
		else
			fontdata(i) = (f(j) and &hff00) shr 8
			t = 0
			j = j + 1
		end if
	next
end SUB

SUB setbit (bb() as integer, BYVAL w as integer, BYVAL b as integer, BYVAL v as integer)
	dim mask as uinteger
	dim woff as integer
	dim wb as integer
		
	woff = w + (b \ 16)
	wb = b mod 16

	if woff > ubound(bb) then
		debug "setbit overflow: ub " + str$(ubound(bb)) + ", w " + str$(w) + ", b " + str$(b)
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

SUB findfiles (fmask$, BYVAL attrib, outfile$, buf())
	dim ff%
	ff = FreeFile
	OPEN outfile$ FOR OUTPUT as #ff
	dim a$
	a$ = DIR$(fmask$, attrib)
	if a$ = "" then exit sub
	DO UNTIL a$ = ""
		PRINT #ff,a$
		a$ = DIR$("", attrib)
	LOOP
	CLOSE #ff
END SUB

SUB unlump (lump$, ulpath$, buffer() as integer)
	unlumpfile(lump$, "*.*", ulpath$, buffer())
end SUB

SUB unlumpfile (lump$, fmask$, path$, buf() as integer)
	dim lf as integer
	dim dat as ubyte
	dim size as integer
	dim lname as string
	dim i as integer
	
	lf = freefile
	open lump$ for binary access read as #lf
	if err > 0 then
		'debug "Could not open file " + lump$
		exit sub
	end if
	
	get #lf, , dat	'read first byte
	while not eof(lf)
		'get lump name
		lname = ""
		while not eof(lf) and dat <> 0
			lname = lname + chr$(dat)
			get #lf, , dat
		wend
		'debug "lump name " + lname
		
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
			
			'debug "lump size " + str$(size)
					
			'do we want this file?	
			if matchmask(ucase$(lname), ucase$(fmask$)) then
				'write yon file
				dim of as integer
				redim bufr(0) as ubyte
				dim csize as integer
				
				of = freefile
				open path$ + lname for binary access write as #of
				if err > 0 then
					'debug "Could not open file " + path$ + lname
					exit sub
				end if
				
				'copy the data
				while size > 0
					if size > 8192 then
						csize = 8192
					else
						csize = size
					end if
					redim bufr(csize - 1)
					'copy a chunk of file
					get #lf, , bufr()
					put #of, , bufr()
					size = size - csize
				wend
				
				erase bufr
				close #of
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
	
	close #lf
	
end SUB

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

FUNCTION pathlength () as integer
	path = curdir$
	pathlength = len(path)
end FUNCTION

SUB getstring (p$)
	p$ = path
end SUB

FUNCTION drivelist (d() as integer) as integer
	'faked, needs work (not linux compatible, either, but later, later)
	d(0) = 3
	d(1) = 4 
	d(2) = 5
	drivelist = 3
end FUNCTION

FUNCTION rpathlength () as integer
	path = exepath
    rpathlength = len(path)
end FUNCTION

FUNCTION exenamelength () as integer
	path = command$(0)
	exenamelength = len(path)
end FUNCTION

SUB setdrive (BYVAL n as integer)
end SUB

FUNCTION envlength (e$) as integer
	path = environ$(e$)
	envlength = len(path)
end FUNCTION

FUNCTION isdir (sDir$) as integer
	isdir = NOT (dir$(sDir$, 16) = "")
END FUNCTION

FUNCTION isremovable (BYVAL d) as integer
	isremovable = 0
end FUNCTION

FUNCTION hasmedia (BYVAL d as integer) as integer
	hasmedia = 0
end FUNCTION

FUNCTION LongNameLength (filename$) as integer
	path = filename$
	longnamelength = len(path)
end FUNCTION



SUB copyfile (s$, d$, buf() as integer)
'this is only called from the obsolete function unlumpone() in moresubs.bas
'which itself is never called, so this can be removed when unlumpone() is.
end SUB

FUNCTION setmouse (mbuf() as integer) as integer
'don't think this does much except says whether there is a mouse
'no idea what the parameter is for
	dim mousebuf(0 to 3) as integer
	readmouse(mousebuf())
	if (mousebuf(2) = -1) then	'no mouse if button = -1
		setmouse = 0
	end if
	setmouse = 1
end FUNCTION

SUB readmouse (mbuf() as integer)
	dim as integer mx, my, mw, mb
	static lastx as integer = 0
	static lasty as integer = 0
	
	getmouse(mx, my, mw, mb)
	if (mx = -1) then mx = lastx
	if (my = -1) then my = lasty
	if (mx > mouse_xmax) then mx = mouse_xmax
	if (mx < mouse_xmin) then mx = mouse_xmin
	if (my > mouse_ymax) then my = mouse_ymax
	if (my < mouse_ymin) then my = mouse_ymin
	
	lastx = mx
	lasty = my
	
	mbuf(0) = mx
	mbuf(1) = my
	mbuf(2) = mb
	mbuf(3) = mw 'not supported at the moment, but shouldn't hurt
end SUB

SUB movemouse (BYVAL x as integer, BYVAL y as integer)
	smouse(x, y)
end SUB

SUB mouserect (BYVAL xmin, BYVAL xmax, BYVAL ymin, BYVAL ymax)
	mouse_xmin = xmin
	mouse_xmax = xmax
	mouse_ymin = ymin
	mouse_ymax = ymax
end sub

FUNCTION readjoy (joybuf() as integer, BYVAL jnum as integer) as integer
'would be easy if I knew what was going where in the buffer
	readjoy = 0
end FUNCTION

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

SUB str2array (s$, arr() as integer, BYVAL o as integer)
'strangely enough, this does the opposite of the above
	DIM i AS Integer
	dim bi as integer
	dim bp as integer ptr
	dim toggle as integer
	
	bp = @arr(0)
	bi = o \ 2 'offset is in bytes
	toggle = o mod 2
	
	'debug "String is " + str$(len(s$)) + " chars"
	for i = 0 to len(s$) - 1
		if toggle = 0 then
			bp[bi] = s$[i] and &hff
			toggle = 1
		else
			bp[bi] = bp[bi] or (s$[i] shl 8)
			'check sign
			if (bp[bi] and &h8000) > 0 then
				bp[bi] = bp[bi] or &hffff0000 'make -ve
			end if
			toggle = 0
			bi = bi + 1
		end if
	next
end SUB

SUB setupstack (buffer() as integer, BYVAL size as integer, file$)
'Currently, stack is always 1024, and blocks of 512 are written out to file$
'whenever it gets too big. Likewise, the passed is never used for anything else.
'For simlpicity, I've decided to allocate a larger stack in memory and ignore 
'the parameters.
	stacktop = allocate(32768) '32k
	if (stacktop = 0) then
		'oh dear
		debug "Not enough memory for stack"
		exit sub
	end if
	stackptr = stacktop
	stacksize = 32768
end SUB

SUB pushw (BYVAL word as integer)
'not sure about the byte order, but it shouldn't matter as long as I undo it
'the same way.
'check bounds to stop overflow - currently it will still break since there's
'no error handling, but at least it won't scribble.
	if stackptr - stacktop < stacksize - 2 and stackptr >= stacktop then
		*stackptr = word and &hff
		stackptr = stackptr + 1
		*stackptr = (word and &hff00) shr 8
		stackptr = stackptr + 1
	else
		debug "overflow"
	end if
end SUB

FUNCTION popw () as integer
	dim pw as integer
	
	if (stackptr > stacktop) then
		stackptr = stackptr - 1
		pw = *stackptr shl 8
		stackptr = stackptr - 1
		pw = pw or (*stackptr)
		'sign
		if pw and &h8000 then
			pw = pw or &hffff0000
		end if
	else
		pw = 0
		'debug "underflow"
	end if
	
	popw = pw	
end FUNCTION

SUB releasestack ()
	if stacksize > 0 then
		deallocate stacktop
		stacksize = -1
	end if
end SUB

FUNCTION stackpos () as integer
	stackpos = stackptr - stacktop
end FUNCTION

'private functions
function matchmask(match as string, mask as string) as integer
	dim i as integer
	dim m as integer
	dim si as integer, sm as integer
	
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

function calcblock(byval x as integer, byval y as integer, byval t as integer) as integer
'returns -1 if overlay
	dim block as integer
	dim tptr as integer ptr
	dim over as integer
	
	'check bounds
	if bordertile = -1 then
		'wrap
		while y < 0
			y = y + map_y
		wend
		while y >= map_y
			y = y - map_y
		wend
		while x < 0
			x = x + map_x
		wend
		while x >= map_x
			x = x - map_x
		wend
	else
		if (y < 0) or (y >= map_y) then 
			calcblock = bordertile
			exit function
		end if
		if (x < 0) or (x >= map_x) then
			calcblock = bordertile
			exit function
		end if
	end if
	
	block = readmapblock(x, y)
	
	'check overlay (??)
	if t > 0 then
		'cheat massively by switching array pointers and recycling readmapblock
		tptr = aptr
		aptr = pptr
		over = readmapblock(x, y)
		over = (over and 128) + t 'whuh?
		if (over <> 130) and (over <> 1) then
			block = -1
		end if
		aptr = tptr	'restore pointer
	end if
	
	calcblock = block
end function

'----------------------------------------------------------------------
'Stub functions which aren't used in game.exe, but are declared in 
'allmodex.bi for custom.exe.
'----------------------------------------------------------------------
SUB setpassblock (BYVAL x, BYVAL y, BYVAL v)
END SUB

FUNCTION readpassblock (BYVAL x, BYVAL y)
	readpassblock = 0
END FUNCTION

SUB bitmap2page (temp(), bmp$, BYVAL p)
END SUB

SUB lumpfiles (listf$, lump$, path$, buffer())
END SUB

FUNCTION isvirtual (BYVAL d)
	isvirtual = 0
END FUNCTION

SUB loadbmp (f$, BYVAL x, BYVAL y, buf(), BYVAL p)
END SUB

SUB getbmppal (f$, mpal(), pal(), BYVAL o)
END SUB

FUNCTION bmpinfo (f$, dat())
	bmpinfo = 0
END FUNCTION

''-----------------------------------------------------------------------
'' Compatibility stuff that should probably go in another file
''-----------------------------------------------------------------------
function xstr$(x as integer)
	if x >= 0 then
		xstr$ = " " + str$(x)
	else
		xstr$ = str$(x)
	end if
end function

function xstr$(x as single)
	if x >= 0 then
		xstr$ = " " + str$(x)
	else
		xstr$ = str$(x)
	end if
end function

function xstr$(x as double)
	if x >= 0 then
		xstr$ = " " + str$(x)
	else
		xstr$ = str$(x)
	end if
end function

sub bamconvert(bamfile as string, songfile as string)
'note, songfile has no extension, so this sub can add whichever it likes
	bam2mid(bamfile, songfile + ".mid")
	'exec("tools/bam2mid.exe", bamfile + " " + songfile + ".mid")
end sub

'I moved this bit down here for a specific reason:
'if any of the procs above require one of the extra libs
'(which they shouldn't, since I sorted them all out),
'it'll bomb because the extra libs aren't included until now
'
'It's a quality thing.

#if not USE_ALLEGRO=1
#include amx_sdl.bas
#else
#include amx_aleg.bas
#endif