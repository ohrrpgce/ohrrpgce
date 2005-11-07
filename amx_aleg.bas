'' FBOHR COMPATIBILITY FUNCTIONS - Allegro version
'' GPL and stuff. See LICENSE.txt.
'

'$include: 'allegro.bi'
dim shared music_song as MIDI ptr = NULL

dim shared back_buffer as BITMAP ptr

sub setmodex()
	allegro_init
	
	set_gfx_mode(GFX_AUTODETECT,640,400,0,0)
	set_display_switch_mode(SWITCH_PAUSE) 'needs to be set, since the default in windows is
	                                      'SWITCH_AMNESIA, which clears all the surfaces if
	                                      'the user switches away! Ow!
	back_buffer = create_bitmap(320,200)
	back_buffer = null '(fer testin)
	if not back_buffer then
		'erm, that's not good.
		fatalerror "Could not create screen."
	end if
	acquire_bitmap(back_buffer)
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
		
	'palette using intpal
end sub

SUB fadeto (palbuff() as integer, BYVAL red as integer, BYVAL green as integer, BYVAL blue as integer)
	dim pal(255) as integer
	dim i as integer
	dim j as integer
	dim hue as integer
	
	'palette get using pal
	
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
	'	palette using pal
		sleep 15 'how long?
	next
	
end SUB

SUB fadetopal (pal() as integer, palbuff() as integer)
	dim intpal(255) as integer
	dim i as integer
	dim j as integer
	dim hue as integer
	dim p as integer	'index to passed palette, which has separate r, g, b
	
	'palette get using intpal
	
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
	'	palette using intpal
		sleep 15 'how long?
	next
end SUB


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
	'view screen (0, maptop) - (319, maptop + maplines - 1)
	
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
					todraw = todraw - 208 + anim2
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
	'view screen (0, 0) - (319, 199)
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
	dspr = dspr + sw - 1 'jump to last column
	hmsk = imagecreate(sw, sh)
	dmsk = hmsk + 4
	dmsk = dmsk + sw - 1 'jump to last column
	
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
	
'	line (x, y) - (x+w-1, y+h-1), c, BF
	
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
		'line (x, i) - (x+w-1, i), c, , &h5555
	next

	for i = y+1 to y+h-1 step 2
		'line (x, i) - (x+w-1, i), c, , &haaaa
	next
end SUB

SUB drawline (BYVAL x1 as integer, BYVAL y1 as integer, BYVAL x2 as integer, BYVAL y2 as integer, BYVAL c as integer, BYVAL p as integer)
	if wrkpage <> p then
		screenset p
		wrkpage = p
	end if
	
	'line (x1, y1) - (x2, y2), c
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

SUB printstr (s$, BYVAL x as integer, BYVAL y as integer, BYVAL p as integer)
	dim row as integer
	dim col as integer
	dim si as integer 'screen index
	dim pscr as ubyte ptr
	dim ch as integer 'character
	dim fi as integer 'font index
	dim cc as integer 'char column
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
						pscr[si] = textfg
					else
						if textbg > 0 then
							pscr[si] = textbg
						end if
					end if
					si = si + 320
					tbyte = tbyte shl 1
				next
			else
				if textbg > 0 then
					for pix = 0 to 7
						pscr[si] = textbg
						si = si + 320
					next
				end if
			end if
			col = col + 1
			fi = fi + 1
		next
	next
	screenunlock
end SUB

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


SUB setupmusic (mbuf() as integer)
	dim version as uinteger
	if music_on = 0 then
		
		install_sound(DIGI_NONE, -1,"")
		
		
		
		
		music_vol = 8
		music_on = 1
		music_paused = 0
	end if	
end SUB

SUB closemusic ()
	if music_on = 1 then
		
		
		if music_song <> 0 then
			
			music_song = 0
			music_paused = 0
		end if
		
		'Mix_CloseAudio
		'SDL_Quit
		music_on = 0
	end if
end SUB

SUB loadsong (f$)
'would be nice if we had a routine that took the number as a param
'instead of the name, maybe abstract one into compat.bas?
	if music_on = 1 then
		dim exten as string
		dim dotpos as integer
		dim i as integer
		dim songname as string = ""
		dim found as integer = 0
		dim pass as integer = 0
		dim bamfile as string
		
		bamfile = rtrim$(f$) 	'lose the null terminator
		
		'get the extension (ie. song number)
		dotpos = 0
		do 
			i = instr(dotpos + 1, bamfile, ".")
			if i <> 0 then
				dotpos = i
			end if
		loop until i = 0
		exten = mid$(bamfile, dotpos + 1)
		
		're-ordered filename without extension
		songname = workingdir$ + PATH_SEP + "song" + exten
		
		'stop current song
		if music_song <> 0 then
			'Mix_FreeMusic(music_song)
			music_song = 0
			music_paused = 0
		end if

		'find song
		pass = 0
		while pass < 2 and found = 0
			if isfile(songname + ".xm") then
				songname = songname + ".xm"
				found = 1
			elseif isfile(songname + ".it") then
				songname = songname + ".it"
				found = 1
			elseif isfile(songname + ".mod") then
				songname = songname + ".mod"
				found = 1
			elseif isfile(songname + ".ogg") then
				songname = songname + ".ogg"
				found = 1
			elseif isfile(songname + ".mid") then
				songname = songname + ".mid"
				found = 1
			end if
			
			if found = 0 then
				bamconvert(bamfile, songname)
			end if
			pass = pass + 1
		wend
		
		if (found = 1) then
			'music_song = Mix_LoadMUS(songname)
			if music_song = 0 then
				debug "Could not load song " + songname
				exit sub
			end if
			
			'Mix_PlayMusic(music_song, -1)			
			music_paused = 0

			if orig_vol = -1 then
				'orig_vol = Mix_VolumeMusic(-1)
			end if
						
			'dim realvol as single
			'realvol = music_vol / 15
			'FMOD_Channel_SetVolume(fmod_channel, realvol)
			if music_vol = 0 then
				'Mix_VolumeMusic(0)
			else
				'add a small adjustment because 15 doesn't go into 128
				'Mix_VolumeMusic((music_vol * 8) + 8)
			end if
		end if
	end if
end SUB

SUB stopsong ()
	if music_on = 1 then
		if music_song > 0 then
			if music_paused = 0 then
				'Mix_PauseMusic
				music_paused = 1
			end if
		end if
	end if
end SUB

SUB resumesong ()
	if music_on = 1 then
		if music_song > 0 then
			'Mix_ResumeMusic
			music_paused = 0
		end if
	end if
end SUB

SUB fademusic (BYVAL vol as integer)
'Unlike the original version, this will pause everything else while it
'fades, so make sure it doesn't take too long
	dim vstep as integer = 1
	dim i as integer
	
	if music_vol > vol then vstep = -1
	for i = music_vol to vol step vstep
		setfmvol(i)
		sleep 10
	next
	
end SUB

FUNCTION getfmvol () as integer
	getfmvol = music_vol
end FUNCTION

SUB setfmvol (BYVAL vol as integer)
	music_vol = vol
	if music_on = 1 then
		if music_vol = 0 then
			'Mix_VolumeMusic(0)
		else
			'add a small adjustment because 15 doesn't go into 128
			'Mix_VolumeMusic((music_vol * 8) + 8)
		end if
	end if
end SUB

SUB screenshot (f$, BYVAL p as integer, maspal() as integer, buf() as integer)
	'bsave f$, 0
end SUB
