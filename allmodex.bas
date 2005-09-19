''
'$include: 'allmodex.bi'
'$include: 'fbgfx.bi'

option explicit

declare function matchmask(match as string, mask as string) as integer
declare function calcblock(byval x as integer, byval y as integer, byval t as integer) as integer

'slight hackery to get more versatile read function
declare function fget alias "fb_FileGet" ( byval fnum as integer, byval pos as integer = 0, byval dst as any ptr, byval bytes as uinteger ) as integer
declare function fput alias "fb_FilePut" ( byval fnum as integer, byval pos as integer = 0, byval src as any ptr, byval bytes as uinteger ) as integer
'extern
declare sub debug(s$)
declare sub fatalerror(e$)

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
dim shared mapx as integer
dim shared mapy as integer

dim shared anim1 as integer
dim shared anim2 as integer

dim shared waittime as integer
dim shared keybd(0 to 255) as integer

dim shared stacktop as ubyte ptr
dim shared stackptr as ubyte ptr
dim shared stacksize as integer

'This is the fontdata() array, initialised with the contents of ohrrpgce.fnt
'$include: 'fontdata.bi'

sub setmodex()
	dim i as integer
	
	'screen 13, , 4, 1 for fullscreen
	screen 13, ,4
	screenset 0, 0
	vispage = 0
	wrkpage = 0
	
	'init vars
	for i = 0 to 255
		keybd(i) = -1
	next
	stacksize = -1
end sub

sub restoremode() ' not needed
	releasestack
end sub

SUB copypage (BYVAL page1 as integer, BYVAL page2 as integer)
	screencopy page1, page2
end sub

SUB clearpage (BYVAL page as integer)
	screenset page
	cls
	wrkpage = page
end SUB

SUB setvispage (BYVAL page as integer)
	screenset , page
	vispage = page
end SUB

sub setpal(pal() as integer)
	dim intpal(0 to 255) as integer
	dim p as integer
	dim i as integer
	
	p = 0 ' is it actually base 0?
	for i = 0 to 255
		intpal(i) = pal(p) or (pal(p+1) shl 8) or (pal(p+2) shl 16)
		p = p + 3
	next i
		
	palette using intpal
end sub

SUB fadeto (palbuff() as integer, BYVAL red as integer, BYVAL green as integer, BYVAL blue as integer)
	dim pal(255) as integer
	dim i as integer
	dim j as integer
	dim hue as integer
	
	palette get using pal
	
	'max of 64 steps
	for i = 0 to 63
		for j = 0 to 255
			'red
			hue = pal(j) and &hff
			pal(j) = pal(j) and &hffff00 'clear
			if hue > red then
				hue = hue - 1
			end if
			if hue < red then
				hue = hue + 1
			end if
			pal(j) = pal(j) or hue
			'green
			hue = (pal(j) and &hff00) shr 8
			pal(j) = pal(j) and &hff00ff 'clear
			if hue > green then
				hue = hue - 1
			end if
			if hue < green then
				hue = hue + 1
			end if
			pal(j) = pal(j) or (hue shl 8)
			'blue
			hue = (pal(j) and &hff0000) shr 16
			pal(j) = pal(j) and &h00ffff 'clear
			if hue > blue then
				hue = hue - 1
			end if
			if hue < blue then
				hue = hue + 1
			end if
			pal(j) = pal(j) or (hue shl 16)
		next
		palette using pal
		sleep 20 'how long?
	next
	
end SUB

SUB fadetopal (pal() as integer, palbuff() as integer)
	dim intpal(255) as integer
	dim i as integer
	dim j as integer
	dim hue as integer
	dim p as integer	'index to passed palette, which has separate r, g, b
	
	palette get using intpal
	
	'max of 64 steps
	for i = 0 to 63
		p = 0
		for j = 0 to 255
			'red
			hue = intpal(j) and &hff
			intpal(j) = intpal(j) and &hffff00 'clear
			if hue > pal(p) then
				hue = hue - 1
			end if
			if hue < pal(p) then
				hue = hue + 1
			end if
			intpal(j) = intpal(j) or hue
			p = p + 1
			'green
			hue = (intpal(j) and &hff00) shr 8
			intpal(j) = intpal(j) and &hff00ff 'clear
			if hue > pal(p) then
				hue = hue - 1
			end if
			if hue < pal(p) then
				hue = hue + 1
			end if
			intpal(j) = intpal(j) or (hue shl 8)
			p = p + 1
			'blue
			hue = (intpal(j) and &hff0000) shr 16
			intpal(j) = intpal(j) and &h00ffff 'clear
			if hue > pal(p) then
				hue = hue - 1
			end if
			if hue < pal(p) then
				hue = hue + 1
			end if
			intpal(j) = intpal(j) or (hue shl 16)
			p = p + 1
		next
		palette using intpal
		sleep 20 'how long?
	next
end SUB

SUB setmapdata (array() as integer, pas() as integer, BYVAL t as integer, BYVAL b as integer)
'I think this is a setup routine like setpicstuf
't and b are top and bottom margins
	mapx = array(0)
	mapy = array(1)
	aptr = @array(2)
	pptr = @pas(2)
	maptop = t
	maplines = 200 - t - b
end SUB

SUB setmapblock (BYVAL x as integer, BYVAL y as integer, BYVAL v as integer)
	dim index as integer
	dim hilow as integer
	
	index = (mapx * y) + x	'raw byte offset
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
	
	index = (mapx * y) + x	'raw byte offset
	hilow = index mod 2		'which byte in word
	index = index shr 1 	'divide by 2
	
	if hilow > 0 then
		block = (aptr[index] and &hff00) shr 8
	else
		block = aptr[index] and &hff
	end if
	
	readmapblock = block
end FUNCTION

SUB drawmap (BYVAL x, BYVAL y as integer, BYVAL t as integer, BYVAL p as integer)
	dim sptr as ubyte ptr
	dim plane as integer
		
	dim ypos as integer
	dim xpos as integer
	dim xstart as integer
	dim yoff as integer
	dim xoff as integer
	dim calc as integer
	dim ty as integer
	dim tx as integer
	dim tbuf as any ptr
	dim tpx as integer
	dim tpy as integer
	dim todraw as integer
	dim tpage as integer
		
	if wrkpage <> p then
		screenset p
		wrkpage = p
	end if

	'set viewport to allow for top and bottom bars
	view screen (0, maptop) - (319, maptop + maplines - 1)
	
	'copied from the asm
	ypos = y \ 20	
	calc = y mod 20
	if calc < 0 then  	'adjust for negative coords
		calc = calc + 20
		ypos = ypos - 1
	end if
	yoff = -calc
	
	xpos = x \ 20
	calc = x mod 20
	if calc < 0 then
		calc = calc + 20
		xpos = xpos - 1
	end if
	xoff = -calc
	xstart = xpos
		
	'create tile buffer
	tbuf = imagecreate(40,40)
	tpage = 3
	
	'screen is 16 * 10 tiles, which means we need to draw 17x11
	'to allow for partial tiles	
	ty = yoff
	while ty < 200
		tx = xoff
		xpos = xstart
		while tx < 320
			todraw = calcblock(xpos, ypos, t)
			if (todraw >= 160) then
				if (todraw > 207) then
					todraw = todraw - 207 + anim2
				else
					todraw = todraw - 160 + anim1
				end if
			end if

			'get the tile
			if (todraw >= 0) then
				tpx = (todraw mod 16) * 20
				tpy = (todraw \ 16) * 20
				'page 3 is the tileset page (#define??)
				'get and put don't take a page argument, so I'll
				'have to toggle the work page, not sure that's efficient
				screenset 3
				get (tpx, tpy)-(tpx+20-1,tpy+20-1), tbuf
									
				'draw it on the map
				screenset p
				put (tx, ty), tbuf, PSET
			end if
			
			tx = tx + 20
			xpos = xpos + 1
		wend
		ty = ty + 20
		ypos = ypos + 1
	wend
		
	imagedestroy tbuf	
	'reset viewport
	view screen (0, 0) - (319, 199)
end SUB

SUB setanim (BYVAL cycle1 as integer, BYVAL cycle2 as integer)
	anim1 = cycle1
	anim2 = cycle2
end SUB

SUB setoutside (BYVAL defaulttile as integer)
	bordertile = defaulttile
end SUB

SUB drawsprite (pic() as integer, BYVAL picoff as integer, pal() as integer, BYVAL po as integer, BYVAL x as integer, BYVAL y as integer, BYVAL page as integer)
'draw sprite from pic(picoff) onto page using pal() starting at po
	dim sw as integer
	dim sh as integer
	dim hspr as ubyte ptr
	dim dspr as ubyte ptr
	dim hmsk as ubyte ptr
	dim dmsk as ubyte ptr
	dim nib as integer
	dim i as integer
	dim spix as integer
	dim pix as integer
	dim mask as integer
	dim row as integer
	
	if wrkpage <> page then
		screenset page
		wrkpage = page
	end if
	
	sw = pic(picoff)
	sh = pic(picoff+1)
	picoff = picoff + 2
	
	'create sprite
	hspr = imagecreate(sw, sh)
	dspr = hspr + 4	'jump to data (hope this works sensibly)
	hmsk = imagecreate(sw, sh)
	dmsk = hmsk + 4
	
	'now do the pixels
	'pixels are in columns, so this might not be the best way to do it
	'maybe just drawing straight to the screen would be easier
	nib = 0
	row = 0
	for i = 0 to (sw * sh) - 1
		select case nib			' 2 bytes = 4 nibbles in each int
			case 0
				spix = (pic(picoff) and &hf000) shr 12
			case 1
				spix = (pic(picoff) and &h0f00) shr 8
			case 2
				spix = (pic(picoff) and &hf0) shr 4
			case 3
				spix = pic(picoff) and &h0f
				picoff = picoff + 1
		end select
		if spix = 0 then 
			pix = 0					' transparent (hope 0 is never valid)
			mask = &hff
		else
			'palettes are interleaved like everything else
			pix = pal((po + spix) \ 2)	' get color from palette
			if (po + spix) mod 2 = 1 then
				pix = (pix and &hff00) shr 8
			else
				pix = pix and &hff
			end if
			mask = 0
		end if
		*dspr = pix				' set image pixel
		dspr = dspr + sw
		*dmsk = mask
		dmsk = dmsk + sw
		row = row + 1
		if (row >= sh) then 	'ugh
			dspr = dspr - (sw * sh)
			dspr = dspr + 1
			dmsk = dmsk - (sw * sh)
			dmsk = dmsk + 1
			row = 0
		end if
		nib = nib + 1
		nib = nib and 3	'= mod 4, but possibly more efficient
	next
	
	'now draw the image and mask
	put (x, y), hmsk, AND
	put (x, y), hspr, XOR
	imagedestroy hspr
	imagedestroy hmsk
end SUB

SUB wardsprite (pic() as integer, BYVAL picoff as integer, pal() as integer, BYVAL po as integer, BYVAL x as integer, BYVAL y as integer, BYVAL page as integer)
'I think this just draws the sprite mirrored
'are the coords top left or top right, though?
	dim sw as integer
	dim sh as integer
	dim hspr as ubyte ptr
	dim dspr as ubyte ptr
	dim hmsk as ubyte ptr
	dim dmsk as ubyte ptr
	dim nib as integer
	dim i as integer
	dim spix as integer
	dim pix as integer
	dim mask as integer
	dim row as integer
	
	if wrkpage <> page then
		screenset page
		wrkpage = page
	end if
	
	sw = pic(picoff)
	sh = pic(picoff+1)
	picoff = picoff + 2
	
	'create sprite
	hspr = imagecreate(sw, sh)
	dspr = hspr + 4	'jump to data (hope this works sensibly)
	dspr = dspr + sh - 1 'jump to last column
	hmsk = imagecreate(sw, sh)
	dmsk = hmsk + 4
	dmsk = dmsk + sh - 1 'jump to last column
	
	'now do the pixels
	'pixels are in columns, so this might not be the best way to do it
	'maybe just drawing straight to the screen would be easier
	nib = 0
	row = 0
	for i = 0 to (sw * sh) - 1
		select case nib			' 2 bytes = 4 nibbles in each int
			case 0
				spix = (pic(picoff) and &hf000) shr 12
			case 1
				spix = (pic(picoff) and &h0f00) shr 8
			case 2
				spix = (pic(picoff) and &hf0) shr 4
			case 3
				spix = pic(picoff) and &h0f
				picoff = picoff + 1
		end select
		if spix = 0 then 
			pix = 0					' transparent (hope 0 is never valid)
			mask = &hff
		else
			'palettes are interleaved like everything else
			pix = pal((po + spix) \ 2)	' get color from palette
			if (po + spix) mod 2 = 1 then
				pix = (pix and &hff00) shr 8
			else
				pix = pix and &hff
			end if
			mask = 0
		end if
		*dspr = pix				' set image pixel
		dspr = dspr + sw
		*dmsk = mask
		dmsk = dmsk + sw
		row = row + 1
		if (row >= sh) then 	'ugh
			dspr = dspr - (sw * sh)
			dspr = dspr - 1		' right to left for wardsprite
			dmsk = dmsk - (sw * sh)
			dmsk = dmsk - 1		' right to left
			row = 0
		end if
		nib = nib + 1
		nib = nib and 3	'= mod 4, but possibly more efficient
	next
	
	'now draw the image and mask
	put (x, y), hmsk, AND
	put (x, y), hspr, XOR
	imagedestroy hspr
	imagedestroy hmsk
end SUB

SUB stosprite (pic() as integer, BYVAL picoff as integer, BYVAL x as integer, BYVAL y as integer, BYVAL page as integer)
'I'm guessing this is the opposite of loadsprite, ie store raw sprite data in screen p 
'starting at x, y. The offsets here do actually seem to be in words, not bytes.
	dim i as integer
	dim p as integer
	dim toggle as integer
	dim sbytes as integer
	dim sptr as ubyte ptr
	dim h as integer
	dim w as integer

	if wrkpage <> page then
		screenset page
		wrkpage = page
	end if
	
	p = picoff
	h = pic(p)
	w = pic(p + 1)
	p = p + 2
	sbytes = (w * h) \ 2 	'only 4 bits per pixel
	
	screenlock
	sptr = screenptr + (320 * y) + x
	
	'copy to passed int buffer, with 2 bytes per int as usual
	toggle = 0
	for i = 0 to sbytes - 1
		if toggle = 0 then
			*sptr = (pic(p) and &hff00) shr 8
			toggle = 1
		else
			*sptr = pic(p) and &hff
			toggle = 0
			p = p + 1
		end if
		sptr = sptr + 1
	next
	
	screenunlock	
end SUB

SUB loadsprite (pic() as integer, BYVAL picoff as integer, BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL page as integer)
'reads sprite from given page into pic(), starting at picoff
'I'm not really sure I have understood this right
	dim i as integer
	dim p as integer
	dim toggle as integer
	dim sbytes as integer
	dim sptr as ubyte ptr
	dim temp as integer

	if wrkpage <> page then
		screenset page
		wrkpage = page
	end if
	
	sbytes = (w * h) \ 2 	'only 4 bits per pixel
	
	screenlock
	sptr = screenptr + (320 * y) + x
	
	'copy to passed int buffer, with 2 bytes per int as usual
	toggle = 0
	p = picoff
	pic(p) = w			'these are 4byte ints, not compat w. orig.
	pic(p+1) = h
	p = p + 2	
	for i = 0 to sbytes - 1
		temp = *sptr
		if toggle = 0 then
			pic(p) = temp shl 8
			toggle = 1
		else
			pic(p) = pic(p) or temp
			toggle = 0
			p = p + 1
		end if
		sptr = sptr + 1
	next
	
	screenunlock
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
'Much more complicated version of this function (which used to be a thin
'wrapper around multikey()) now checks against an array of times to see
'it's time for the key to repeat yet.
'It would be nice to make changes in game.bas and elsewhere so this can 
'revert to the simpler form, but at the moment it is required or the
'context menu never shows.
	dim ktime as integer
	
	keyval = 0
	if multikey(a) < 0 then
		'NOTE there are 86,400,000 thousandths in a day
		ktime = int(timer() * 1000)
		if ktime > keybd(a) then
			keyval = 2
			if ktime > keybd(a) + 1000 then
				keybd(a) = ktime + 200
			else
				keybd(a) = ktime + 50
			end if
		else
			if keybd(a) > 80000000 and ktime < 1800000 then
				keybd(a) = keybd(a) - 86400000
				if ktime > keybd(a) then
					keyval = 2
					if keybd(a) = -1 or ktime > keybd(a) + 1000 then
						keybd(a) = ktime + 200
					else
						keybd(a) = ktime + 50
					end if
				end if
			end if
		end if
	else
		keybd(a) = -1
	end if
end FUNCTION

FUNCTION igetkey () as integer
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
	
	igetkey = key
end FUNCTION

SUB setkeys ()
end SUB

SUB putpixel (BYVAL x as integer, BYVAL y as integer, BYVAL c as integer, BYVAL p as integer)
	if wrkpage <> p then
		screenset p
		wrkpage = p
	end if
	
	' lock the screen first?
	pset (x, y), c	
	
end SUB

FUNCTION readpixel (BYVAL x as integer, BYVAL y as integer, BYVAL p as integer) as integer
	if wrkpage <> p then
		screenset p
		wrkpage = p
	end if
	
	readpixel = point (x, y)	
end FUNCTION

SUB rectangle (BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL c as integer, BYVAL p as integer)
	dim hh as string
	dim ww as string
	if wrkpage <> p then
		screenset p
		wrkpage = p
	end if
	
	line (x, y) - (x+w-1, y+h-1), c, BF
	
end SUB

SUB fuzzyrect (BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL c as integer, BYVAL p as integer)
	dim i as integer
	
	if wrkpage <> p then
		screenset p
		wrkpage = p
	end if

	'can't see how to do this with fills, so it'll have to be lines
	'unless I draw direct to the buffer
	for i = y to y+h-1 step 2
		line (x, i) - (x+w-1, i), c, , &h5555
	next

	for i = y+1 to y+h-1 step 2
		line (x, i) - (x+w-1, i), c, , &haaaa
	next
end SUB

SUB drawline (BYVAL x1 as integer, BYVAL y1 as integer, BYVAL x2 as integer, BYVAL y2 as integer, BYVAL c as integer, BYVAL p as integer)
	if wrkpage <> p then
		screenset p
		wrkpage = p
	end if
	
	line (x1, y1) - (x2, y2), c
end SUB

SUB paintat (BYVAL x as integer, BYVAL y as integer, BYVAL c as integer, BYVAL page as integer, buf() as integer, BYVAL max as integer)
	if wrkpage <> page then
		screenset page
		wrkpage = page
	end if
	
	paint (x, y), c
end SUB

SUB storepage (fil$, BYVAL i as integer, BYVAL p as integer)
'saves a screen page to a file
	dim f as integer
	dim idx as integer
	dim bi as integer
	dim ub as ubyte
	dim sptr as ubyte ptr
	dim plane as integer
	
	if wrkpage <> p then
		screenset p
		wrkpage = p
	end if
	
	f = freefile
	open fil$ for binary access read write as #f
	if err > 0 then
		'debug "Couldn't open " + fil$
		exit sub
	end if
	
	'skip to index
	seek #f, (i*64000) + 1 'will this work with write access?
	
	screenlock
	
	'modex format, 4 planes
	for plane = 0 to 3
		sptr = screenptr() + plane
		
		for idx = 0 to (16000 - 1) '1/4 of a screenfull
			ub = *sptr
			put #f, , ub
			sptr = sptr + 4
		next
	next
	
	screenunlock
	close #f		
end SUB

SUB loadpage (fil$, BYVAL i as integer, BYVAL p as integer)
'loads a whole page from a file 
	dim f as integer
	dim idx as integer
	dim bi as integer
	dim ub as ubyte
	dim sptr as ubyte ptr
	dim plane as integer
	
	if wrkpage <> p then
		screenset p
		wrkpage = p
	end if
	
	f = freefile
	open fil$ for binary access read as #f
	if err > 0 then
		'debug "Couldn't open " + fil$
		exit sub
	end if
	
	'skip to index
	seek #f, (i*64000) + 1
	
	screenlock
	
	'modex format, 4 planes
	for plane = 0 to 3
		sptr = screenptr() + plane
		
		for idx = 0 to (16000 - 1) '1/4 of a screenfull
			get #f, , ub
			*sptr = ub
			sptr = sptr + 4
		next
	next
	
	screenunlock
	close #f
	
end SUB

SUB setdiskpages (buf() as integer, BYVAL h as integer, BYVAL l as integer)
'sets up buffer (not used) and page size in lines for the page functions above
'at the moment I have ignored this, and handled whole pages only, I'll have
'to check whether partial pages are used anywhere
'No, doesn't look like it is ever less than a whole page, starting at line 0.
end SUB

SUB setwait (b() as integer, BYVAL t as integer)
'b() is actually the destination, where the time gets written to, but is
'never accessed outside of here, so I'll ignore it. t is the delay in 
'1/1024ths of a second. 
	waittime = t
end SUB

SUB dowait ()
'wait for as long as defined in setwait()
'In freebasic, sleep is in 1000ths, and a value of less than 100 will not 
'be exited by a keypress, so break the wait time up into 50s.
	dim i as integer
	
	i = waittime
	while i > 0
		if i > 50 then
			sleep 50
			i = i - 50
		else
			sleep i
			i = 0
		end if
	wend
end SUB

SUB printstr (s$, BYVAL x as integer, BYVAL y as integer, BYVAL p as integer)
	dim row as integer
	dim col as integer
	dim si as integer 'screen index
	dim pscr as ubyte ptr
	dim ch as integer 'character
	dim fi as integer 'font index
	dim cc as integer 'char column
	dim fcol as ubyte 'font color
	dim pix as integer
	dim bval as integer
	dim tbyte as ubyte
	
	if wrkpage <> p then
		screenset p
		wrkpage = p
	end if
	
	'is it actually faster to use a direct buffer write, or would pset be
	'sufficiently quick?
	col = x
	fcol = (color() and &hff) 'fg color - should I store text color separately?
	screenlock
	pscr = screenptr
	for ch = 0 to len(s$) - 1
		fi = s$[ch] * 8	'index to fontdata
		for cc = 0 to 7
			si = (y * 320) + col
			if (fontdata(fi) > 0) then
				tbyte = 1
				for pix = 0 to 7
					bval = fontdata(fi) and tbyte
					if bval > 0 then
						pscr[si] = fcol
					end if
					si = si + 320
					tbyte = tbyte shl 1
				next
			end if
			col = col + 1
			fi = fi + 1
		next
	next
	screenunlock
end SUB

SUB textcolor (BYVAL f as integer, BYVAL b as integer)
	color f, b
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

SUB storeset (fil$, BYVAL i as integer, BYVAL l as integer)
' i = index, l = line (only if reading from screen buffer)
	dim f as integer
	dim idx as integer
	dim bi as integer
	dim ub as ubyte
	dim toggle as integer
	dim sptr as ubyte ptr
	
	f = freefile
	open fil$ for binary access read write as #f
	if err > 0 then
		'debug "Couldn't open " + fil$
		exit sub
	end if
	
	seek #f, (i*bsize) + 1 'does this work properly with write?
	'this is a horrible hack to get 2 bytes per integer, even though
	'they are 4 bytes long in FB
	bi = 0
	toggle = 0
	if bpage >= 0 then
		'read from screen
		screenlock
		sptr = screenptr + (320 * l)
		fput(f, ,sptr, bsize)
		screenunlock
		'do I need to bother with buffer?
	else
		'debug "buffer size to read = " + str$(bsize)
		for idx = 0 to bsize - 1 ' this will be slow
			if toggle = 0 then
				ub = bptr[bi] and &hff
				toggle = 1
			else
				ub = (bptr[bi] and &hff00) shr 8
				toggle = 0
				bi = bi + 1
			end if
			put #f, , ub
		next
	end if
			
	close #f
	
end SUB

SUB loadset (fil$, BYVAL i as integer, BYVAL l as integer)
' i = index, l = line (only if reading to screen buffer)
	dim f as integer
	dim idx as integer
	dim bi as integer
	dim ub as ubyte
	dim toggle as integer
	dim sptr as ubyte ptr
	
	f = freefile
	open fil$ for binary access read as #f
	if err > 0 then
		'debug "Couldn't open " + fil$
		exit sub
	end if
	
	seek #f, (i*bsize) + 1
	'this is a horrible hack to get 2 bytes per integer, even though
	'they are 4 bytes long in FB
	bi = 0
	toggle = 0
	if bpage >= 0 then
		'read to screen
		screenlock
		sptr = screenptr + (320 * l)
		fget(f, ,sptr, bsize)
		screenunlock
		'do I need to bother with buffer?
	else
		'debug "buffer size to read = " + str$(bsize)
		for idx = 0 to bsize - 1 ' this will be slow
			get #f, , ub
			if toggle = 0 then
				bptr[bi] = ub
				toggle = 1
			else
				bptr[bi] = bptr[bi] or (ub shl 8)
				'check sign
				if (bptr[bi] and &h8000) > 0 then
					bptr[bi] = bptr[bi] or &hffff0000 'make -ve
				end if
				toggle = 0
				bi = bi + 1
			end if
		next
	end if
			
	close #f
end SUB

SUB setpicstuf (buf() as integer, BYVAL b as integer, BYVAL p as integer)
	if p >= 0 then
		if wrkpage <> p then
			screenset p
			wrkpage = p
		end if
	end if
	
	bptr = @buf(0) 'doesn't really work well with FB
	bsize = b
	bpage = p
end SUB

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

SUB setupmusic (mbuf() as integer)
end SUB

SUB closemusic ()
end SUB

SUB loadsong (f$)
end SUB

SUB stopsong ()
end SUB

SUB resumesong ()
end SUB

SUB fademusic (BYVAL vol as integer)
end SUB

FUNCTION getfmvol () as integer
	getfmvol = 0
end FUNCTION

SUB setfmvol (BYVAL vol as integer)
end SUB

SUB copyfile (s$, d$, buf() as integer)
'this is only called from the obsolete function unlumpone() in moresubs.bas
'which itself is never called, so this can be removed when unlumpone() is.
end SUB

SUB screenshot (f$, BYVAL p as integer, maspal() as integer, buf() as integer)
	bsave f$, 0
end SUB

FUNCTION isetmouse (mbuf() as integer) as integer
	isetmouse = 0
end FUNCTION

SUB readmouse (mbuf() as integer)
end SUB

SUB movemouse (BYVAL x as integer, BYVAL y as integer)
end SUB

SUB mouserect (BYVAL xmin, BYVAL xmax, BYVAL ymin, BYVAL ymax)
end sub

FUNCTION readjoy (joybuf() as integer, BYVAL jnum as integer) as integer
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
			y = y + mapy
		wend
		while y >= mapy
			y = y - mapy
		wend
		while x < 0
			x = x + mapx
		wend
		while x >= mapx
			x = x - mapx
		wend
	else
		if (y < 0) or (y >= mapy) then 
			calcblock = bordertile
			exit function
		end if
		if (x < 0) or (x >= mapx) then
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