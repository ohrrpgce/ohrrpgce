'' FBOHR COMPATIBILITY FUNCTIONS
'' GPL and stuff. See LICENSE.txt.
'
#define DEMACRO

#ifdef __FB_WIN32__
'# include "windows.bi"
'it was too awful (collision-wise) to include all of windows.bi
# undef point
# define _X86_
# include "win/windef.bi"
# include "win/winbase.bi"
# undef max
# undef min
# undef getcommandline
# undef copyfile
#endif

#include "compat.bi"
#include "common.bi"
#include "allmodex.bi"
#include "gfx.bi"
#include "music.bi"
#include "bitmap.bi"
#include "util.bi"
#include "const.bi"

option explicit

#define NULL 0
'a few key constants borrowed from fbgfx.bi, they should all be defined
'in a separate .bi file really, but this will do for now
#define SC_CONTROL		&h1D
#define SC_LSHIFT		&h2A
#define SC_RSHIFT		&h36
#define SC_ALT			&h38


type node 	'only used for floodfill
	x as integer
	y as integer
	nextnode as node ptr
end type

'add page? or assume workpage? (all pages for clip?)
declare SUB drawspritex (pic() as integer, BYVAL picoff as integer, pal() as integer, BYVAL po as integer, BYVAL x as integer, BYVAL y as integer, BYVAL page as integer, byval scale as integer=1, BYVAL trans as integer = -1)
declare sub setclip(l as integer=0, t as integer=0, r as integer=319, b as integer=199)
declare sub drawohr(byref spr as frame, x as integer, y as integer, scale as integer=1, trans as integer = -1)
declare sub grabrect(page as integer, x as integer, y as integer, w as integer, h as integer, ibuf as ubyte ptr, tbuf as ubyte ptr = 0)
declare SUB loadbmp4(byval bf as integer, byval iw as integer, byval ih as integer, byval maxw as integer, byval maxh as integer, byval sbase as ubyte ptr)
declare SUB loadbmprle4(byval bf as integer, byval iw as integer, byval ih as integer, byval maxw as integer, byval maxh as integer, byval sbase as ubyte ptr)

'used for map and pass
DECLARE SUB setblock (BYVAL x as integer, BYVAL y as integer, BYVAL v as integer, byval l as integer, BYVAL mp as integer ptr)
DECLARE FUNCTION readblock (BYVAL x as integer, BYVAL y as integer, byval l as integer, BYVAL mp as integer ptr) as integer

declare function matchmask(match as string, mask as string) as integer
declare function calcblock(byval x as integer, byval y as integer, byval l as integer, byval t as integer) as integer

'slight hackery to get more versatile read function
declare function fget alias "fb_FileGet" ( byval fnum as integer, byval pos as integer = 0, byval dst as any ptr, byval bytes as uinteger ) as integer
declare function fput alias "fb_FilePut" ( byval fnum as integer, byval pos as integer = 0, byval src as any ptr, byval bytes as uinteger ) as integer


#if __FB_VERSION__ > "0.16"
#define threadbs any ptr
#else
#define threadbs integer
#endif

declare sub pollingthread(byval as threadbs)

dim shared vispage as integer
dim shared wrkpage as integer
dim shared spage(0 to 3) as ubyte ptr

dim shared bptr as integer ptr	' buffer
dim shared bsize as integer
dim shared bpage as integer

dim shared bordertile as integer
dim shared mptr as integer ptr	' map ptr
dim shared pptr as integer ptr	' pass ptr
dim shared maptop as integer
dim shared maplines as integer
dim shared map_x as integer
dim shared map_y as integer

dim shared anim1 as integer
dim shared anim2 as integer

dim shared waittime as double
dim shared waitset as integer

dim shared keybd(-1 to 255) as integer  'keyval array
dim shared keybdstate(127) as integer  '"real"time array
dim shared keysteps(127) as integer

dim shared keybdmutex as intptr  'controls access to keybdstate(), mouseflags and mouselastflags
dim shared keybdthread as intptr   'id of the polling thread
dim shared endpollthread as integer  'signal the polling thread to quit

dim shared stackbottom as ubyte ptr
dim shared stackptr as ubyte ptr
dim shared stacksize as integer

dim shared mouse_xmin as integer
dim shared mouse_xmax as integer
dim shared mouse_ymin as integer
dim shared mouse_ymax as integer
dim shared mouseflags as integer
dim shared mouselastflags as integer

dim shared textfg as integer
dim shared textbg as integer

dim shared fontdata as ubyte ptr

dim shared as integer clipl, clipt, clipr, clipb

dim shared intpal(0 to 255) as RGBcolor	'current palette
dim shared updatepal as integer  'setpal called, load new palette at next setvispage

'global sprite buffer, to allow reuse without allocate/deallocate
dim shared tbuf as frame ptr = null

sub setmodex()
	dim i as integer

	'initialise software gfx
	for i = 0 to 3
		spage(i) = callocate(320 * 200)
	next
	setclip

	gfx_init
	vispage = 0
	wrkpage = 0

	'init vars
	stacksize = -1
	for i = 0 to 127
		keybd(i) = 0
		keybdstate(i) = 0
 		keysteps(i) = -1
	next
	endpollthread = 0
	mouselastflags = 0
	mouseflags = 0

	keybdmutex = mutexcreate
	keybdthread = threadcreate (@pollingthread)

	io_init
	mouserect(0,319,0,199)
end sub

sub restoremode()
	dim i as integer

	gfx_close
	'clean up io stuff
	endpollthread = 1
	threadwait keybdthread
	mutexdestroy keybdmutex

	'clear up software gfx
	for i = 0 to 3
		deallocate(spage(i))
	next

	'clean up tile buffer
	if tbuf <> null then
		'mask should always be null, but no harm in future-proofing
		if tbuf->mask <> null then	deallocate tbuf->mask
		if tbuf->image <> null then	deallocate tbuf->image
		deallocate tbuf
		tbuf = null
	end if
	releasestack
end sub

SUB copypage (BYVAL page1 as integer, BYVAL page2 as integer)
	dim i as integer
	'inefficient, could be improved with memcpy
	for i = 0 to (320 * 200) - 1
		spage(page2)[i] = spage(page1)[i]
	next
end sub

SUB copypage (BYVAL page1 as integer, page2() as ubyte)
	if ubound(page2) < (320*200) - 1 then
		debug "page2 buffer too small " & ubound(page2)
	end if
	dim i as integer
	for i = 0 to (320 * 200) - 1
		page2(i) = spage(page1)[i]
	next
END SUB

SUB copypage (page1() as ubyte, BYVAL page2 as integer)
	if ubound(page1) < (320*200) - 1 then
		debug "page1 buffer too small " & ubound(page1)
	end if
	dim i as integer
	for i = 0 to (320 * 200) - 1
		spage(page2)[i] = page1(i)
	next
END SUB

SUB clearpage (BYVAL page as integer)
	dim i as integer

	'inefficient, could be improved with memcpy
	for i = 0 to (320 * 200) - 1
		spage(page)[i] = 0
	next
	wrkpage = page
end SUB

SUB setvispage (BYVAL page as integer)
  'the fb backend may freeze up if they collide with the polling thread (why???)
	mutexlock keybdmutex
	if updatepal then
		gfx_setpal(intpal())
		updatepal = 0
	end if	
	gfx_showpage(spage(page))
	mutexunlock keybdmutex

	vispage = page
end SUB

sub setpal(pal() as RGBcolor)
	memcpy(@intpal(0), @pal(0), 256 * SIZEOF(RGBcolor))

	updatepal = -1
end sub

SUB fadeto (BYVAL red as integer, BYVAL green as integer, BYVAL blue as integer)
	dim i as integer
	dim j as integer
	dim diff as integer

	if updatepal then
		mutexlock keybdmutex
		gfx_setpal(intpal())
		mutexunlock keybdmutex
		updatepal = 0
	end if

	for i = 1 to 32
		for j = 0 to 255
			'red
			diff = intpal(j).r - red
			if diff > 0 then
				intpal(j).r -= iif(diff >= 8, 8, diff) 
			elseif diff < 0 then
				intpal(j).r -= iif(diff <= -8, -8, diff) 
			end if
			'green
			diff = intpal(j).g - green
			if diff > 0 then
				intpal(j).g -= iif(diff >= 8, 8, diff) 
			elseif diff < 0 then
				intpal(j).g -= iif(diff <= -8, -8, diff) 
			end if
			'blue
			diff = intpal(j).b - blue
			if diff > 0 then
				intpal(j).b -= iif(diff >= 8, 8, diff) 
			elseif diff < 0 then
				intpal(j).b -= iif(diff <= -8, -8, diff) 
			end if
		next
		mutexlock keybdmutex
		gfx_setpal(intpal())
		mutexunlock keybdmutex
        sleep 15 'how long?
	next

	'Make sure the palette gets set on the final pass
end SUB

SUB fadetopal (pal() as RGBcolor)
	dim i as integer
	dim j as integer
	dim diff as integer

	if updatepal then
		mutexlock keybdmutex
		gfx_setpal(intpal())
		mutexunlock keybdmutex
		updatepal = 0
	end if

	for i = 1 to 32
		for j = 0 to 255
			'red
			diff = intpal(j).r - pal(j).r
			if diff > 0 then
				intpal(j).r -= iif(diff >= 8, 8, diff) 
			elseif diff < 0 then
				intpal(j).r -= iif(diff <= -8, -8, diff) 
			end if
			'green
			diff = intpal(j).g - pal(j).g
			if diff > 0 then
				intpal(j).g -= iif(diff >= 8, 8, diff) 
			elseif diff < 0 then
				intpal(j).g -= iif(diff <= -8, -8, diff) 
			end if
			'blue
				diff = intpal(j).b - pal(j).b
			if diff > 0 then
				intpal(j).b -= iif(diff >= 8, 8, diff) 
			elseif diff < 0 then
				intpal(j).b -= iif(diff <= -8, -8, diff) 
			end if
		next
		mutexlock keybdmutex
		gfx_setpal(intpal())
		mutexunlock keybdmutex
	sleep 15 'how long?
	next
end SUB

SUB setmapdata (array() as integer, pas() as integer, BYVAL t as integer, BYVAL b as integer)
'I think this is a setup routine like setpicstuf
't and b are top and bottom margins
	map_x = array(0)
	map_y = array(1)
	mptr = @array(2)
	pptr = @pas(2)
	maptop = t
	maplines = 200 - t - b
end SUB

SUB setmapblock (BYVAL x as integer, BYVAL y as integer, BYVAL l as integer, BYVAL v as integer)
	setblock(x, y, v, l, mptr)
end sub

FUNCTION readmapblock (BYVAL x as integer, BYVAL y as integer, byval l as integer) as integer
	return readblock(x, y, l, mptr)
end function

SUB setpassblock (BYVAL x as integer, BYVAL y as integer, BYVAL v as integer)
	setblock(x, y, v, 0, pptr)
END SUB

FUNCTION readpassblock (BYVAL x as integer, BYVAL y as integer)
	return readblock(x, y, 0, pptr)
END FUNCTION

SUB setblock (BYVAL x as integer, BYVAL y as integer, BYVAL v as integer, BYVAL l as integer, BYVAL mp as integer ptr)
	dim index as integer
	dim hilow as integer

	index = (map_x * map_y * l) + (map_x * y) + x	'raw byte offset
	hilow = index mod 2		'which byte in word
	index = index shr 1 	'divide by 2

	if hilow > 0 then
		'delete original value
		mp[index] = mp[index] and &hff
		'set new value
		mp[index] = mp[index] or ((v and &hff) shl 8)
	else
		'delete original value
		mp[index] = mp[index] and &hff00
		'set new value
		mp[index] = mp[index] or (v and &hff)
	end if

end SUB

FUNCTION readblock (BYVAL x as integer, BYVAL y as integer, BYVAL l as integer, BYVAL mp as integer ptr) as integer
	dim block as integer
	dim index as integer
	dim hilow as integer

	index = (map_x * map_y * l) + (map_x * y) + x	'raw byte offset
	hilow = index mod 2		'which byte in word
	index = index shr 1 	'divide by 2

	if hilow > 0 then
		block = (mp[index] and &hff00) shr 8
	else
		block = mp[index] and &hff
	end if

	readblock = block
end FUNCTION

SUB drawmap (BYVAL x, BYVAL y as integer, BYVAL l as integer, BYVAL t as integer, BYVAL p as integer, byval trans as integer = 0)
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
	dim tpx as integer
	dim tpy as integer
	dim todraw as integer
	dim tpage as integer
	'this is static to allow optimised reuse
	static lasttile as integer
	
	if wrkpage <> p then
		wrkpage = p
	end if

	'set viewport to allow for top and bottom bars
	setclip(0, maptop, 319, maptop + maplines - 1)

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

	if tbuf = null then
		'create tile buffer
		tbuf = callocate(sizeof(frame))
		tbuf->w = 20
		tbuf->h = 20
		tbuf->mask = callocate(20 * 20)
		tbuf->image = callocate(20 * 20)
	end if
	
	'debug trans & " " & tbuf->mask
	'force it to be cleared for each redraw
	lasttile = -1

	tpage = 3

	'screen is 16 * 10 tiles, which means we need to draw 17x11
	'to allow for partial tiles
	ty = yoff
	while ty < 200
		tx = xoff
		xpos = xstart
		while tx < 320
			todraw = calcblock(xpos, ypos, l, t)
			if (todraw >= 160) then
				if (todraw > 207) then
					todraw = (todraw - 48 + anim2) MOD 160
				else
					todraw = (todraw + anim1) MOD 160
				end if
			end if

			'get the tile
			if (todraw >= 0) then
				if todraw <> lasttile then
					tpx = (todraw mod 16) * 20
					tpy = (todraw \ 16) * 20
					'page 3 is the tileset page (#define??)
					'get and put don't take a page argument, so I'll
					'have to toggle the work page, not sure that's efficient
					grabrect(3, tpx, tpy, 20, 20, tbuf->image, tbuf->mask)
				end if

				'draw it on the map
				drawohr(*tbuf, tx, ty,,trans)
				lasttile = todraw
			end if

			tx = tx + 20
			xpos = xpos + 1
		wend
		ty = ty + 20
		ypos = ypos + 1
	wend

	'reset viewport
	setclip
end SUB

SUB setanim (BYVAL cycle1 as integer, BYVAL cycle2 as integer)
	anim1 = cycle1
	anim2 = cycle2
end SUB

SUB setoutside (BYVAL defaulttile as integer)
	bordertile = defaulttile
end SUB

SUB drawsprite (pic() as integer, BYVAL picoff as integer, pal() as integer, BYVAL po as integer, BYVAL x as integer, BYVAL y as integer, BYVAL page as integer, BYVAL trans = -1)
'draw sprite from pic(picoff) onto page using pal() starting at po
	drawspritex(pic(), picoff, pal(), po, x, y, page, 1, trans)
end sub

SUB bigsprite (pic(), pal(), BYVAL p, BYVAL x, BYVAL y, BYVAL page, BYVAL trans = -1)
	drawspritex(pic(), 0, pal(), p, x, y, page, 2, trans)
END SUB

SUB hugesprite (pic(), pal(), BYVAL p, BYVAL x, BYVAL y, BYVAL page, BYVAL trans = -1)
	drawspritex(pic(), 0, pal(), p, x, y, page, 4, trans)
END SUB

SUB drawspritex (pic() as integer, BYVAL picoff as integer, pal() as integer, BYVAL po as integer, BYVAL x as integer, BYVAL y as integer, BYVAL page as integer, byval scale as integer, byval trans as integer = -1)
'draw sprite scaled, used for drawsprite(x1), bigsprite(x2) and hugesprite(x4)
	dim sw as integer
	dim sh as integer
	dim hspr as frame
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
		wrkpage = page
	end if

	sw = pic(picoff)
	sh = pic(picoff+1)
	picoff = picoff + 2

	'create sprite
	hspr.w = sw
	hspr.h = sh
	hspr.image = allocate(sw * sh)
	hspr.mask = allocate(sw * sh)
	dspr = hspr.image
	dmsk = hspr.mask

	'now do the pixels
	'pixels are in columns, so this might not be the best way to do it
	'maybe just drawing straight to the screen would be easier
	nib = 0
	row = 0
	for i = 0 to (sw * sh) - 1
		select case as const nib 			' 2 bytes = 4 nibbles in each int
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
		if spix = 0 and trans then
			pix = 0					' transparent (hope 0 is never valid)
			mask = &hff
		else
			'palettes are interleaved like everything else
			pix = pal(int((po + spix) / 2))	' get color from palette
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
	'now draw the image
	drawohr(hspr,x,y, scale)

	deallocate(hspr.image)
	deallocate(hspr.mask)
end SUB

SUB wardsprite (pic() as integer, BYVAL picoff as integer, pal() as integer, BYVAL po as integer, BYVAL x as integer, BYVAL y as integer, BYVAL page as integer, BYVAL trans = -1)
'I think this just draws the sprite mirrored
'are the coords top left or top right, though?
	dim sw as integer
	dim sh as integer
	dim hspr as frame
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
	hspr.w = sw
	hspr.h = sh
	hspr.image = allocate(sw * sh)
	hspr.mask = allocate(sw * sh)
	dspr = hspr.image
	dmsk = hspr.mask
	dspr = dspr + sw - 1 'jump to last column
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
		if spix = 0 and trans then
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

	'now draw the image
	drawohr(hspr,x,y)
	deallocate(hspr.image)
	deallocate(hspr.mask)
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
		wrkpage = page
	end if

	p = picoff
	h = pic(p)
	w = pic(p + 1)
	p = p + 2
	sbytes = ((w * h) + 1) \ 2 	'only 4 bits per pixel

	sptr = spage(page)
	sptr = sptr + (320 * y) + x

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
		wrkpage = page
	end if

	sbytes = ((w * h) + 1) \ 2 	'only 4 bits per pixel

	sptr = spage(page)
	sptr = sptr + (320 * y) + x

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

end SUB

SUB getsprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL page)
'This seems to convert a normal graphic into a sprite, storing the result in pic() at picoff
	dim as ubyte ptr sbase, sptr
	dim nyb as integer = 0
	dim p as integer = 0
	dim as integer sw, sh

	'store width and height
	p = picoff
	pic(p) = w
	p += 1
	pic(p) = h
	p += 1

	'find start of image
	sbase = spage(page)
	sbase = sbase + (y * 320) + x
	'pixels are stored in columns for the sprites (argh)
	for sh = 0 to w - 1
		sptr = sbase
		for sw = 0 to h - 1
			select case nyb
				case 0
					pic(p) = (*sptr and &h0f) shl 12
				case 1
					pic(p) = pic(p) or ((*sptr and &h0f) shl 8)
				case 2
					pic(p) = pic(p) or ((*sptr and &h0f) shl 4)
				case 3
					pic(p) = pic(p) or (*sptr and &h0f)
					p += 1
			end select
			sptr += 320
			nyb += 1
			nyb = nyb and &h03
		next
		sbase = sbase + 1 'next col
	next

END SUB

FUNCTION keyval (BYVAL a as integer) as integer
	keyval = keybd(a)
end FUNCTION

FUNCTION waitforanykey (modkeys=-1) as integer
	dim i as integer
	setkeys
	do
		setwait 100
		setkeys
		for i = 1 to &h7f
			if not modkeys and (i=29 or i=56 or i=42 or i=54) then continue for
			if keyval(i) > 1 then return i
		next i
		dowait
	loop
	return 0
end FUNCTION

FUNCTION getkey () as integer
	dim i as integer, key as integer
	key = 0

	setkeys
	do
		io_pollkeyevents()
		setkeys
		'keybd(0) may contain garbage (but in assembly, keyval(0) contains last key pressed)
		for i=1 to &h7f
			if keyval(i) > 1 then
				key = i
				exit do
			end if
		next
		sleep 50
	loop while key = 0

	getkey = key
end FUNCTION

'FIXME DELETEME
'--This code is for screen page debugging, and will be removed in the future!
DECLARE SUB debug_screen_page(p AS INTEGER)
SUB debug_screen_page(p AS INTEGER)
	dim caption as string
	dim k as integer
	copypage p, vpage
	caption = "Screen Page: "
	IF p = dpage THEN
		caption = caption & "drawing page"
	else
	caption = caption & p
	end if
	edgeprint caption, 0, 0, uilook(uiText), vpage
	edgeprint "B:blank, W:whiteout, N:Nukepicstuf", 0, 190, uilook(uiText), vpage
	setvispage vpage
	k = waitforanykey(NO)
	if k = 48 then clearpage p
	if k = 17 then rectangle 0, 0, 320, 200, 15, p
	if k = 49 then
		dim x as integer
		dim c as integer
		c = 1
		for x = 2 to 319
			rectangle x, 0, 1, 200, c, p
			c = (c + 1) MOD 255
		next x
	end if
	clearkey (k)
END SUB

SUB setkeys ()
'Quite nasty. Moved all this functionality from keyval() because this
'is where it seems to happen in the original.
'I have rewritten this to use steps (frames based on the 55ms DOS timer)
'rather than raw time. It makes the maths a bit simpler. The way the
'rest of the code is structured means we need to emulate the original
'functionality of clearing the event until a repeat fires. I do this
'by stalling for 3 steps on a new keypress and 1 step on a repeat.
'1 step means the event will fire once per step, but won't fire many
'times in one frame (which is a problem, setkeys() is often called
'more than once per frame, particularly when new screens are brought
'up). - sb 2006-01-27

'Actual key state goes in keybd array for retrieval via keyval().

'In the asm version, setkeys copies over the real key state array
'(which is built using an interrupt handler) to the state array used
'by keyval and then reduces new key presses to held keys, all of
'which now happens in the backend, which may rely on a polling thread
'or keyboard event callback as needed. - tmc
	dim a as integer
	mutexlock keybdmutex
	for a = 0 to &h7f
		keybd(a) = keybdstate(a)
		if keysteps(a) > 0 then
			keysteps(a) -= 1
		end if
		keybdstate(a) = keybdstate(a) and 1
	next
	'Check to see if the operating system has received a request
	'to close the window (clicking the X) and set the magic keyboard
	'index -1 if so.
	IF INKEY$ = CHR$(255) + "k" THEN
		keybd(-1) = 1
	ELSE
		keybd(-1) = 0
	END IF

	mutexunlock keybdmutex

	'FIXME DELETEME
	'--This code is for screen page debugging, and will be removed in the future!
	if keyval(70) > 0 then 'Scroll-lock
		clearkey(70)
		if keyval(3) > 1 then clearkey(3) : debug_screen_page 2
		if keyval(4) > 1 then clearkey(4) : debug_screen_page 3
	end if
end SUB

SUB clearkey(byval k as integer)
	keybd(k) = 0
end sub

sub pollingthread(byval unused as threadbs)
	dim as integer a, dummy, buttons

	while endpollthread = 0
		mutexlock keybdmutex

		io_updatekeys keybdstate()
		'set key state for every key
		'highest scancode in fbgfx.bi is &h79, no point overdoing it
		for a = 0 to &h7f
			if keybdstate(a) and 4 then
				'decide whether to fire a new key event, otherwise the keystate is preserved as 1
				if keysteps(a) <= 0 then
					if keysteps(a) = -1 then
						'this is a new keypress
						keysteps(a) = 7
					else
						keysteps(a) = 1
					end if
					keybdstate(a) = 3
				else
					keybdstate(a) = keybdstate(a) and 3
				end if
			else
				keybdstate(a) = keybdstate(a) and 2 'no longer pressed, but was seen
				keysteps(a) = -1 '-1 means it's a new press next time
			end if
		next
		io_getmouse dummy, dummy, dummy, buttons
		mouseflags = mouseflags or (buttons and not mouselastflags)
		mouselastflags = buttons
		mutexunlock keybdmutex

		sleep 25
	wend
end sub

SUB putpixel (BYVAL x as integer, BYVAL y as integer, BYVAL c as integer, BYVAL p as integer)
	if wrkpage <> p then
		wrkpage = p
	end if

	'wrap if x is too high
	if x >= 320 then
		y = y + (x \ 320)
		x = x mod 320
	end if

	if y >= 200 or y < 0 or x < 0 then
		debug "attempt to putpixel off-screen " & x & "," & y & "=" & c & " on page " & p
		exit sub
	end if

	spage(p)[y*320 + x] = c
end SUB

FUNCTION readpixel (BYVAL x as integer, BYVAL y as integer, BYVAL p as integer) as integer
	if wrkpage <> p then
		wrkpage = p
	end if

	'wrap if x is too high
	if x >= 320 then
		y = y + (x \ 320)
		x = x mod 320
	end if

	if y >= 200 or y < 0 or x < 0 then
		debug "attempt to readpixel off-screen " & x & "," & y & " on page " & p
		return 0
	end if

	readpixel = spage(p)[y*320 + x]
end FUNCTION

SUB drawbox (BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL c as integer, BYVAL p as integer)
	dim sptr as ubyte ptr
	dim i as unsigned integer
    dim j as integer, temp, tw

	if wrkpage <> p then
		wrkpage = p
	end if

    if w < 0 then x = x + w + 1: w = -w
    if h < 0 then y = y + h + 1: h = -h

	'clip
	if x + w > clipr then w = (clipr - x) + 1
	if y + h > clipb then h = (clipb - y) + 1
	if x < clipl then w -= (clipl - x) : x = clipl
	if y < clipt then h -= (clipt - y) : y = clipt

	'draw
    j = 321 - w
    i = c shl 24 or c shl 16 or c shl 8 or c

	sptr = spage(p) + (y*320) + x

    if h >= 1 then
        'draw the top
        temp = w
        while temp and 3
            *sptr = c
            sptr += 1
            temp -= 1
        wend
        while temp
            *(cast(unsigned integer ptr, sptr)) = i
            sptr += 4
            temp -= 4
        wend
    end if
    sptr -= 1
    if h >= 3 then
        'draw the sides
        temp = h - 2
        w -= 1
        while temp
            sptr += j
            *sptr = c
            sptr += w
            *sptr = c
            temp -= 1
        wend
        w += 1
    end if
    if h >= 2 then
        'draw the bottom
        sptr += j

        while w and 3
            *sptr = c
            sptr += 1
            w -= 1
        wend
        while w
            *cast(unsigned integer ptr, sptr) = i
            sptr += 4
            w -= 4
        wend
    end if

end SUB

SUB rectangle (BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL c as integer, BYVAL p as integer)
	dim sptr as ubyte ptr
	dim i as integer

	if wrkpage <> p then
		wrkpage = p
	end if

	'clip
	if x + w > clipr then w = (clipr - x) + 1
	if y + h > clipb then h = (clipb - y) + 1
	if x < clipl then w = w - ABS(x - clipl) : x = clipl + 1
	if y < clipt then h = h - ABS(y - clipt) : y = clipt + 1
	'repeat w/h clipping because x,y might have just changed 
	if x + w > clipr then w = (clipr - x) + 1
	if y + h > clipb then h = (clipb - y) + 1

	if w <= 0 or h <= 0 then exit sub
	'draw
	sptr = spage(p) + (y*320) + x
	while h > 0
		for i = 0 to w-1
			sptr[i] = c
		next
		h -= 1
		sptr += 320
	wend
'	line (x, y) - (x+w-1, y+h-1), c, BF

end SUB

SUB fuzzyrect (BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL c as integer, BYVAL p as integer)
	dim sptr as ubyte ptr
	dim i as integer
	dim tog as integer 'pattern toggle

	if wrkpage <> p then
		wrkpage = p
	end if

	'clip
	if x + w > clipr then w = (clipr - x) + 1
	if y + h > clipb then h = (clipb - y) + 1
	if x < clipl then w = w - ABS(x - clipl) : x = clipl
	if y < clipt then h = h - ABS(y - clipt) : y = clipt
	'repeat w/h clipping because x,y might have just changed 
	if x + w > clipr then w = (clipr - x) + 1
	if y + h > clipb then h = (clipb - y) + 1

	'draw
	sptr = spage(p) + (y*320) + x
	while h > 0
		tog = h mod 2
		for i = 0 to w-1
			if tog = 0 then
				sptr[i] = c
				tog = 1
			else
				tog = 0
			end if
		next
		h -= 1
		sptr += 320
	wend

end SUB

SUB drawline (BYVAL x1 as integer, BYVAL y1 as integer, BYVAL x2 as integer, BYVAL y2 as integer, BYVAL c as integer, BYVAL p as integer)
'uses Bresenham's run-length slice algorithm
  	dim as integer xdiff,ydiff
  	dim as integer xdirection 	'direction of X travel from top to bottom point (1 or -1)
  	dim as integer minlength  	'minimum length of a line strip
  	dim as integer startLength 	'length of start strip (approx half 'minLength' to balance line)
  	dim as integer runLength  	'current run-length to be used (minLength or minLength+1)
  	dim as integer endLength   	'length of end of line strip (usually same as startLength)

  	dim as integer instep		'xdirection or 320 (inner loop)
	dim as integer outstep		'xdirection or 320 (outer loop)
	dim as integer shortaxis	'outer loop control
	dim as integer longaxis

  	dim as integer errorterm   	'when to draw an extra pixel
  	dim as integer erroradd 		'add to errorTerm for each strip drawn
  	dim as integer errorsub 		'subtract from errorterm when triggered

  	dim as integer i,j
  	dim sptr as ubyte ptr

'Macro to simplify code
#define DRAW_SLICE(a) for i=0 to a-1: *sptr = c: sptr += instep: next

	if wrkpage <> p then
		wrkpage = p
	end if

  	if (y1>y2) then
  		'swap ends, we only draw downwards
    	i=y1: y1=y2: y2=i
    	i=x1: x1=x2: x2=i
    end if

    'point to start
    sptr = spage(p) + y1*320 + x1

  	xdiff=x2-x1
  	ydiff=y2-y1

  	if (xDiff<0) then
  		'right to left
    	xdiff=-xdiff
    	xdirection=-1
  	else
    	xdirection=1
    end if

	'special case for vertical
  	if (xdiff = 0) then
  		instep = 320
  		DRAW_SLICE(ydiff+1)
    	exit sub
  	end if

	'and for horizontal
  	if (ydiff = 0) then
  		instep = xdirection
  		DRAW_SLICE(xdiff+1)
    	exit sub
  	end if

  	'and also for pure diagonals
  	if xdiff = ydiff then
  		instep = 320 + xdirection
  		DRAW_SLICE(ydiff+1)
    	exit sub
  	end if

	'now the actual bresenham
  	if xdiff > ydiff then
  		longaxis = xdiff
    	shortaxis = ydiff

    	instep = xdirection
    	outstep = 320
  	else
		'other way round, draw vertical slices
		longaxis = ydiff
		shortaxis = xdiff

		instep = 320
		outstep = xdirection
	end if

	'calculate stuff
    minlength = longaxis \ shortaxis
	erroradd = (longaxis mod shortaxis) * 2
	errorsub = shortaxis * 2

	'errorTerm must be initialized properly since first pixel
	'is about in the center of a strip ... not the start
	errorterm = (erroradd \ 2) - errorsub

	startLength = (minLength \ 2) + 1
	endLength = startlength 'half +1 of normal strip length

	'If the minimum strip length is even
	if (minLength and 1) <> 0 then
  		errorterm += shortaxis 'adjust errorTerm
	else
		'If the line had no remainder (x&yDiff divided evenly)
  		if erroradd = 0 then
			startLength -= 1 'leave out extra start pixel
		end if
	end if

	'draw the start strip
	DRAW_SLICE(startlength)
	sptr += outstep

	'draw the middle strips
	for j = 1 to shortaxis-1
      	runLength = minLength
  		errorTerm += erroradd

  		if errorTerm > 0 then
  			errorTerm -= errorsub
			runLength += 1
  		end if

  		DRAW_SLICE(runlength)
  		sptr += outstep
	next

	DRAW_SLICE(endlength)
end SUB

SUB paintat (BYVAL x as integer, BYVAL y as integer, BYVAL c as integer, BYVAL page as integer, buf() as integer, BYVAL max as integer)
'I'm not really sure what this does, I assume it's a floodfill, but then what are buf and max for?
'Uses putpixel and readpixel, so could probably be sped up with direct access. Also ignores clipping
'at the moment, which is possibly foolish
	dim tcol as integer
	dim queue as node ptr = null
	dim tail as node ptr = null
	dim as integer w, e		'x coords west and east
	dim i as integer
	dim tnode as node ptr = null

	if wrkpage <> page then
		wrkpage = page
	end if

	tcol = readpixel(x, y, page)	'get target colour

	'prevent infinite loop if you fill with the same colour
	if tcol = c then exit sub

	queue = allocate(sizeof(node))
	queue->x = x
	queue->y = y
	queue->nextnode = null
	tail = queue

	do
		if readpixel(queue->x, queue->y, page) = tcol then
			putpixel(queue->x, queue->y, c, page) 'change color
			w = queue->x
			e = queue->x
			'find western limit
			while w > 0 and readpixel(w-1, queue->y, page) = tcol
				w = w-1
				putpixel(w, queue->y, c, page) 'change
			wend
			'find eastern limit
			while e < 319 and readpixel(e+1, queue->y, page) = tcol
				e = e+1
				putpixel(e, queue->y, c, page)
			wend
			'add bordering nodes
			for i = w to e
				if queue->y > 0 then
					'north
					if readpixel(i, queue->y-1, page) = tcol then
						tail->nextnode = allocate(sizeof(node))
						tail = tail->nextnode
						tail->x = i
						tail->y = queue->y-1
						tail->nextnode = null
					end if
				end if
				if queue->y < 199 then
					'south
					if readpixel(i, queue->y+1, page) = tcol then
						tail->nextnode = allocate(sizeof(node))
						tail = tail->nextnode
						tail->x = i
						tail->y = queue->y+1
						tail->nextnode = null
					end if
				end if
			next
		end if

		'advance queue pointer, and delete behind us
		tnode = queue
		queue = queue->nextnode
		deallocate(tnode)

	loop while queue <> null
	'should only exit when queue has caught up with tail

end SUB

SUB storepage (fil$, BYVAL i as integer, BYVAL p as integer)
'saves a screen page to a file
	dim f as integer
	dim idx as integer
	dim bi as integer
	dim ub as ubyte
	dim sptr as ubyte ptr
	dim scrnbase as ubyte ptr
	dim plane as integer

	if wrkpage <> p then
		wrkpage = p
	end if

	if NOT fileiswriteable(fil$) then exit sub
	f = freefile
	open fil$ for binary access read write as #f

	'skip to index
	seek #f, (i*64000) + 1 'will this work with write access?

	'modex format, 4 planes
	scrnbase = spage(p)
	for plane = 0 to 3
		sptr = scrnbase + plane

		for idx = 0 to (16000 - 1) '1/4 of a screenfull
			ub = *sptr
			put #f, , ub
			sptr = sptr + 4
		next
	next

	close #f
end SUB

SUB loadpage (fil$, BYVAL i as integer, BYVAL p as integer)
'loads a whole page from a file
	dim f as integer
	dim idx as integer
	dim bi as integer
	dim ub as ubyte
	dim sptr as ubyte ptr
	dim scrnbase as ubyte ptr
	dim plane as integer

	if wrkpage <> p then
		wrkpage = p
	end if

	if NOT fileisreadable(fil$) then exit sub
	f = freefile
	open fil$ for binary access read as #f

	'skip to index
	seek #f, (i*64000) + 1

	'modex format, 4 planes
	scrnbase = spage(p)
	for plane = 0 to 3
		sptr = scrnbase + plane

		for idx = 0 to (16000 - 1) '1/4 of a screenfull
			get #f, , ub
			*sptr = ub
			sptr = sptr + 4
		next
	next

	close #f

end SUB

SUB setwait (BYVAL t as integer)
't is a value in milliseconds which, in the original, is used to set the event
'frequency and is also used to set the wait time, but the resolution of the
'dos timer means that the latter is always truncated to the last multiple of
'55 milliseconds. We won't do this anymore.
	waittime = timer + t / 1000
	waitset = 1
end SUB

SUB dowait ()
'wait until alarm time set in setwait()
'In freebasic, sleep is in 1000ths, and a value of less than 100 will not
'be exited by a keypress, so sleep for 5ms until timer > waittime.
	dim i as integer
	do while timer <= waittime
		sleep 5 'is this worth it?
	loop
	if waitset = 1 then
		waitset = 0
	else
		debug "dowait called without setwait"
	end if
end SUB

SUB printstr (s$, BYVAL x as integer, BYVAL y as integer, BYVAL p as integer)
	dim col as integer
	dim si as integer 'screen index
	dim pscr as ubyte ptr
	dim ch as integer 'character
	dim fi as integer 'font index
	dim cc as integer 'char column
	dim pix as integer
	dim bval as integer
	dim tbyte as ubyte
	dim fstep as integer
	dim maxrow as integer
	dim minrow as integer

	if wrkpage <> p then
		wrkpage = p
	end if

	'check bounds
	if y < -7 or y > 199 then exit sub
	if x > 319 then exit sub

	'only draw rows that are on the screen
	maxrow = 199 - y
	if maxrow > 7 then
		maxrow = 7
	end if
	minrow = 0
	if y < 0 then
		minrow = -y
		y = 0
	end if

	'is it actually faster to use a direct buffer write, or would pset be
	'sufficiently quick?
	col = x
	pscr = spage(p)
	for ch = 0 to len(s$) - 1
		'find fontdata index, bearing in mind that the data is stored
		'2-bytes at a time in 4-byte integers, due to QB->FB quirks,
		'and fontdata itself is a byte pointer. Because there are
		'always 8 bytes per character, we will always use exactly 4
		'ints, or 16 bytes, making the initial calc pretty simple.
		fi = (s$[ch] * 16)
		'fi = s$[ch] * 8	'index to fontdata
		fstep = 1 'used because our indexing is messed up, see above
		for cc = 0 to 7
			if col >= 0 then
				si = (y * 320) + col
				if (fontdata[fi] > 0) then
					tbyte = 1 shl minrow
					for pix = minrow to maxrow
						bval = fontdata[fi] and tbyte
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
						for pix = minrow to maxrow
							pscr[si] = textbg
							si = si + 320
						next
					end if
				end if
			end if
			col = col + 1
			if col >= 320 THEN exit SUB
			fi = fi + fstep
			fstep = iif(fstep = 1, 3, 1) 'uneven steps due to 2->4 byte thunk
		next
	next
end SUB

SUB textcolor (BYVAL f as integer, BYVAL b as integer)
	textfg = f
	textbg = b
end SUB

SUB setfont (f() as integer)
	fontdata = cast(ubyte ptr, @f(0))
end SUB

SUB setbit (bb() as integer, BYVAL w as integer, BYVAL b as integer, BYVAL v as integer)
	dim mask as uinteger
	dim woff as integer
	dim wb as integer

	woff = w + (b \ 16)
	wb = b mod 16

	if woff > ubound(bb) then
		debug "setbit overflow: ub " & ubound(bb) & ", w " & w & ", b " & b & ", v " & v
		exit sub
	end if

	mask = 1 shl wb
	if v then
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
	if woff > ubound(bb) then
		debug "readbit overflow: ub " & ubound(bb) & ", w " & w & ", b " & b
		return 0
	end if
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

	if NOT fileiswriteable(fil$) then exit sub
	f = freefile
	open fil$ for binary access read write as #f

	seek #f, (i*bsize) + 1 'does this work properly with write?
	'this is a horrible hack to get 2 bytes per integer, even though
	'they are 4 bytes long in FB
	bi = 0
	toggle = 0
	if bpage >= 0 then
		'read from screen
		sptr = spage(wrkpage)
		sptr = sptr + (320 * l)
		fput(f, ,sptr, bsize)
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

	if NOT fileisreadable(fil$) then exit sub
	f = freefile
	open fil$ for binary access read as #f

	seek #f, (i*bsize) + 1
	'this is a horrible hack to get 2 bytes per integer, even though
	'they are 4 bytes long in FB
	bi = 0
	toggle = 0
	if bpage >= 0 then
		'read to screen
		sptr = spage(wrkpage)
		sptr = sptr + (320 * l)
		fget(f, ,sptr, bsize)
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
			wrkpage = p
		end if
	end if

	bptr = @buf(0) 'doesn't really work well with FB
	bsize = b
	bpage = p
end SUB

SUB fixspriterecord (buf() as integer, w as integer, h as integer)
 ' Fix a sprite record that was loaded with loadrecord so that it can be drawn with drawsprite
 DIM AS INTEGER i, j, n, size
 DIM nibble(3) AS INTEGER
 
 'calculate array size
 size = w * h \ 4
 DIM tmpbuf(size)
 
 'move data to a temporary buffer
 FOR i = 0 TO size - 1
  tmpbuf(i) = buf(i)
 NEXT i
 
 'store witdth and height
 buf(0) = w
 buf(1) = h

 'copy data back to array, compensating for mode-x planes
 FOR i = 0 TO size - 1
  n = tmpbuf(i)
  FOR j = 0 TO 3
   nibble(j) = (n \ 2 ^ (j * 4)) AND 15
  NEXT j
  n = nibble(1) * 4096 + nibble(0) * 256 + nibble(3) * 16 + nibble(2)
  buf(2 + i) = n
 NEXT i
 
END SUB

FUNCTION loadrecord (buf() as integer, fh as integer, recordsize as integer, record as integer = -1)
'common sense alternative to loadset, setpicstuf
'loads 16bit records in an array
'buf() = buffer to load shorts into, starting at buf(0)
'fh = open file handle
'recordsize = record size in shorts (not bytes)
'record = record number, defaults to read from current file position
'returns 1 if successful, 0 if failure (eg. file too short)
 dim idx as integer
 if recordsize <= 0 then return 0
 if ubound(buf) < recordsize - 1 then
  debug "loadrecord: " & recordsize & " ints will not fit in " & ubound(buf) + 1 & " element array"
 end if
 dim readbuf(recordsize - 1) as short

 if record <> -1 then
  seek #fh, recordsize * 2 * record + 1
 end if
 if seek(fh) + 2 * recordsize > lof(fh) + 1 then return 0
 get #fh, , readbuf()
 for idx = 0 to recordsize - 1
  buf(idx) = readbuf(idx)
 next
 loadrecord = 1
END FUNCTION

FUNCTION loadrecord (buf() as integer, filen$, recordsize as integer, record as integer = 0)
'wrapper for above
	dim f as integer
	dim i as integer

  if recordsize <= 0 then return 0

	if NOT fileisreadable(filen$) then
		debug "File not found loading record " & record & " from " & filen$
		for i = 0 to recordsize - 1
			buf(i) = 0
		next
		return 0
	end if
	f = freefile
	open filen$ for binary access read as #f

	loadrecord = loadrecord (buf(), f, recordsize, record)
	close #f
END FUNCTION

SUB storerecord (buf() as integer, fh as integer, recordsize as integer, record as integer = -1)
'same as loadrecord
	dim idx as integer
	dim writebuf(recordsize - 1) as short

	if record <> -1 then
		seek #fh, recordsize * 2 * record + 1
	end if
	for idx = 0 to recordsize - 1
		writebuf(idx) = buf(idx)
	next
	put #fh, , writebuf()
end SUB

SUB storerecord (buf() as integer, filen$, recordsize as integer, record as integer = 0)
'wrapper for above
	dim f as integer

	if NOT fileiswriteable(filen$) then exit sub
	f = freefile
	open filen$ for binary access read write as #f

	storerecord buf(), f, recordsize, record
	close #f
END SUB

SUB findfiles (fmask$, BYVAL attrib, outfile$, buf())
  findfiles fmask$, attrib, outfile$
end sub

SUB findfiles (fmask$, BYVAL attrib, outfile$)
    ' attrib 0: all files 'cept folders, attrib 16: folders only
#ifdef __FB_LINUX__
        'this is pretty hacky, but works around the lack of DOS-style attributes, and the apparent uselessness of DIR$
	DIM grep$, shellout$
  shellout$ = "/tmp/ohrrpgce-findfiles-" + STR$(RND * 10000) + ".tmp"
	grep$ = "-v '/$'"
	IF attrib AND 16 THEN grep$ = "'/$'"
	DIM i%
	FOR i = LEN(fmask$) TO 1 STEP -1
		IF MID$(fmask$, i, 1) = CHR$(34) THEN fmask$ = LEFT$(fmask$, i - 1) + "\" + CHR$(34) + RIGHT$(fmask$, LEN(fmask$) - i)
	NEXT i
	i = INSTR(fmask$, "*")
	IF i THEN
		fmask$ = CHR$(34) + LEFT$(fmask$, i - 1) + CHR$(34) + RIGHT$(fmask$, LEN(fmask$) - i + 1)
	ELSE
		fmask$ = CHR$(34) + fmask$ + CHR$(34)
	END IF
	SHELL "ls -d1p " + fmask$ + " 2>/dev/null |grep "+ grep$ + ">" + shellout$ + " 2>&1"
	DIM AS INTEGER f1, f2
	f1 = FreeFile
	OPEN shellout$ FOR INPUT AS #f1
	f2 = FreeFile
	OPEN outfile$ FOR OUTPUT AS #f2
	DIM s$
	DO UNTIL EOF(f1)
		LINE INPUT #f1, s$
		IF s$ = "/dev/" OR s$ = "/proc/" OR s$ = "/sys/" THEN CONTINUE DO
		IF RIGHT$(s$, 1) = "/" THEN s$ = LEFT$(s$, LEN(s$) - 1)
		DO WHILE INSTR(s$, "/")
			s$ = RIGHT$(s$, LEN(s$) - INSTR(s$, "/"))
		LOOP
		PRINT #f2, s$
	LOOP
	CLOSE #f1
	CLOSE #f2
	KILL shellout$
#else
	DIM a$, i%, folder$
	if attrib = 0 then attrib = 255 xor 16
	if attrib = 16 then attrib = 55 '*sigh*
	FOR i = LEN(fmask$) TO 1 STEP -1
		IF MID$(fmask$, i, 1) = "\" THEN folder$ = MID$(fmask$, 1, i): EXIT FOR
	NEXT

	dim tempf%, realf%
	tempf = FreeFile
	a$ = DIR$(fmask$, attrib)
	if a$ = "" then
		'create an empty file
		OPEN outfile$ FOR OUTPUT AS #tempf
		close #tempf
		exit sub
	end if
	OPEN outfile$ + ".tmp" FOR OUTPUT AS #tempf
	DO UNTIL a$ = ""
		PRINT #tempf, a$
		a$ = DIR$ '("", attrib)
	LOOP
	CLOSE #tempf
	OPEN outfile$ + ".tmp" FOR INPUT AS #tempf
	realf = FREEFILE
	OPEN outfile$ FOR OUTPUT AS #realf
	DO UNTIL EOF(tempf)
	LINE INPUT #tempf, a$
        IF attrib = 55 THEN
		'alright, we want directories, but DIR$ is too broken to give them to us
		'files with attribute 0 appear in the list, so single those out
		IF DIR$(folder$ + a$, 55) <> "" AND DIR$(folder$ + a$, 39) = "" THEN PRINT #realf, a$
	ELSE
		PRINT #realf, a$
	END IF
	LOOP
	CLOSE #tempf
	CLOSE #realf
	KILL outfile$ + ".tmp"
#endif
END SUB

SUB unlump (lump$, ulpath$, buffer() as integer)
	unlumpfile(lump$, "", ulpath$)
end SUB

SUB unlump (lump$, ulpath$)
	unlumpfile(lump$, "", ulpath$)
end SUB

SUB unlumpfile (lump$, fmask$, path$, buf() as integer)
	unlumpfile(lump$, fmask$, path$)
end sub

SUB unlumpfile (lump$, fmask$, path$)
	dim lf as integer
	dim dat as ubyte
	dim size as integer
	dim maxsize as integer
	dim lname as string
	dim i as integer
	dim bufr as ubyte ptr
	dim nowildcards as integer = 0

	if NOT fileisreadable(lump$) then exit sub
	lf = freefile
	open lump$ for binary access read as #lf
	maxsize = LOF(lf)

	if len(path$) > 0 and right(path$, 1) <> SLASH then path$ = path$ & SLASH

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
			debug "corrupt lump file: lump name too long"
			exit while
		end if
		'force to lower-case
		lname = lcase(lname)
		'debug "lump name " + lname

		if instr(lname, "\") or instr(lname, "/") then
			debug "unsafe lump name " + str$(lname)
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
				debug "corrupt lump size" + str$(size) + " exceeds source size" + str$(maxsize)
				exit while
			end if

			'debug "lump size " + str$(size)

			'do we want this file?
			if matchmask(lname, lcase$(fmask$)) then
				'write yon file
				dim of as integer
				dim csize as integer

				if NOT fileiswriteable(path$ + lname) then exit while
				of = freefile
				open path$ + lname for binary access write as #of

				'copy the data
				while size > 0
					if size > 16383 then
						csize = 16383
					else
						csize = size
					end if
					'copy a chunk of file
					fget lf, , bufr, csize
					fput of, , bufr, csize
					size = size - csize
				wend

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

FUNCTION islumpfile (lump$, fmask$)
	dim lf as integer
	dim dat as ubyte
	dim size as integer
	dim maxsize as integer
	dim lname as string
	dim i as integer

	islumpfile = 0

	if NOT fileisreadable(lump$) then exit function
	lf = freefile
	open lump$ for binary access read as #lf
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
			debug "corrupt lump file: lump name too long"
			exit while
		end if
		'force to lower-case
		lname = lcase(lname)
		'debug "lump name " + lname

		if instr(lname, "\") or instr(lname, "/") then
			debug "unsafe lump name " + str$(lname)
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
				debug "corrupt lump size" + str$(size) + " exceeds source size" + str$(maxsize)
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

SUB lumpfiles (listf$, lump$, path$, buffer())
  lumpfiles(listf$, lump$, path$)
end sub

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
		exit sub
	end if

	lf = freefile
	open lump$ for binary access write as #lf
	if err <> 0 then
		'debug "Could not open file " + lump$
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
			debug "name too long: " + lname
			debug " name = " + str(textsize(0)) + ", ext = " + str(textsize(1))
			continue do
		end if

		tl = freefile
		open path$ + lname for binary access read as #tl
		if err <> 0 then
			'debug "failed to open " + path$ + lname
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

FUNCTION isfile (n$) as integer
	' directories don't count as files
	' this is a simple wrapper for fileisreadable
	if n$ = "" then return 0
	return fileisreadable(n$)
END FUNCTION

FUNCTION isdir (sDir$) as integer
#IFDEF __FB_LINUX__
	'Special hack for broken Linux dir$() behavior
	isdir = 0
	sDir$ = escape_string(sDir$, """`\$")
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

FUNCTION drivelist (drives$()) as integer
#ifdef __FB_LINUX__
	' on Linux there is only one drive, the root /
	drivelist = 0
#else
	dim drivebuf as zstring * 1000
	dim drivebptr as zstring ptr
	dim as integer zslen, i

	zslen = GetLogicalDriveStrings(999, drivebuf)

	drivebptr = @drivebuf
	while drivebptr < @drivebuf + zslen
		drives$(i) = *drivebptr
		drivebptr += len(drives$(i)) + 1
		i += 1
	wend

	drivelist = i
#endif
end FUNCTION

FUNCTION drivelabel (drive$) as string
#ifdef __FB_WIN32__
	dim tmpname as zstring * 256
	if GetVolumeInformation(drive$, tmpname, 255, NULL, NULL, NULL, NULL, 0) = 0 then
		drivelabel = "<not ready>"
	else
		drivelabel = tmpname
	end if
#else
	drivelabel = ""
#endif
END FUNCTION

FUNCTION isremovable (drive$) as integer
#ifdef __FB_WIN32__
	isremovable = GetDriveType(drive$) = DRIVE_REMOVABLE
#else
	isremovable = 0
#endif
end FUNCTION

FUNCTION hasmedia (drive$) as integer
#ifdef __FB_WIN32__
	hasmedia = GetVolumeInformation(drive$, NULL, 0, NULL, NULL, NULL, NULL, 0)
#else
	hasmedia = 0
#endif
end FUNCTION

SUB setupmusic
	music_init
	sound_init
end SUB

SUB closemusic ()
	music_close
	sound_close
end SUB

SUB loadsong (f$)
	'check for extension
	dim ext as string
	dim songname as string
	dim songtype as integer

	songname = f$
	songtype = getmusictype(f$)

  music_play(songname, songtype)

end SUB

SUB pausesong ()
	music_pause()
end SUB

SUB resumesong ()
	music_resume
end SUB

SUB fademusic (BYVAL vol as integer)
	music_fade(vol)
end SUB

FUNCTION getfmvol () as integer
	getfmvol = music_getvolume
end FUNCTION

SUB setfmvol (BYVAL vol as integer)
	music_setvolume(vol)
end SUB

SUB copyfile (s$, d$)
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

SUB screenshot (f$, BYVAL p as integer, maspal() as RGBcolor)
'Not sure whether this should be in here or in gfx. Possibly both?
'	bsave f$, 0

	'try external first
	if gfx_screenshot(f$, p) = 0 then
		'otherwise save it ourselves
		dim header as BITMAPFILEHEADER
		dim info as BITMAPINFOHEADER
		dim argb as RGBQUAD

		dim as integer of, w, h, i, bfSize, biSizeImage, bfOffBits, biClrUsed, pitch
		dim as ubyte ptr s

		w = 320
		h = 200
		s = spage(p)
		pitch = w

		biSizeImage = w * h
		bfOffBits = 54 + 1024
		bfSize = bfOffBits + biSizeImage
		biClrUsed = 256

		header.bfType = 19778
		header.bfSize = bfSize
		header.bfReserved1 = 0
		header.bfReserved2 = 0
		header.bfOffBits = bfOffBits

		info.biSize = 40
		info.biWidth = w
		info.biHeight = h
		info.biPlanes = 1
		info.biBitCount = 8
		info.biCompression = 0
		info.biSizeImage = biSizeImage
		info.biXPelsPerMeter = &hB12
		info.biYPelsPerMeter = &hB12
		info.biClrUsed = biClrUsed
		info.biClrImportant = biClrUsed

		if NOT fileiswriteable(f$) then exit sub
		of = freefile
		open f$ for binary access write as #of

		put #of, , header
		put #of, , info

		for i = 0 to 255
			argb.rgbRed = maspal(i).r
			argb.rgbGreen = maspal(i).g
			argb.rgbBlue = maspal(i).b
			put #of, , argb
		next

		s += (h - 1) * pitch
		while h > 0
			fput(of, , s, pitch)
			s -= pitch
			h -= 1
		wend

		close #of
	end if
end SUB

FUNCTION setmouse (mbuf() as integer) as integer
'don't think this does much except says whether there is a mouse
'no idea what the parameter is for
	if io_enablemouse <> 0 then
		setmouse = 0
		exit function
	end if
	setmouse = 1
end FUNCTION

SUB readmouse (mbuf() as integer)
	dim as integer mx, my, mw, mb, mc

	io_getmouse(mx, my, mw, mb)
	'gfx_fb/sdl/alleg return last onscreen position when the mouse is offscreen
	'gfx_fb: If you release a mouse button offscreen, it becomes stuck (FB bug)
	'        wheel scrolls offscreen are registered when you move back onscreen
	'gfx_alleg: button state continues to work offscreen but wheel scrolls are not registered
	'gfx_sdl: button state works offscreen. wheel state unavailable

	if (mx > mouse_xmax) then mx = mouse_xmax
	if (mx < mouse_xmin) then mx = mouse_xmin
	if (my > mouse_ymax) then my = mouse_ymax
	if (my < mouse_ymin) then my = mouse_ymin

	mutexlock keybdmutex   'is this necessary?
	mc = mouseflags or (mb and not mouselastflags)
	mouselastflags = mb
	mouseflags = 0

	mutexunlock keybdmutex

	mbuf(0) = mx
	mbuf(1) = my
	mbuf(2) = mb or mc   'current button state bits, plus missed clicks since last call
	mbuf(3) = mc         '1 if new (left?) click since last call to readmouse
end SUB

SUB movemouse (BYVAL x as integer, BYVAL y as integer)
	io_setmouse(x, y)
end SUB

SUB mouserect (BYVAL xmin, BYVAL xmax, BYVAL ymin, BYVAL ymax)
	mouse_xmin = xmin
	mouse_xmax = xmax
	mouse_ymin = ymin
	mouse_ymax = ymax
	io_mouserect(xmin, xmax, ymin, ymax)
end sub

FUNCTION readjoy (joybuf() as integer, BYVAL jnum as integer) as integer
'Return 0 if joystick is not present, or -1 (true) if joystick is present
'jnum is the joystick to read (QB implementation supports 0 and 1)
'joybuf(0) = Analog X axis
'joybuf(1) = Analog Y axis
'joybuf(2) = button 1: 0=pressed nonzero=not pressed
'joybuf(3) = button 2: 0=pressed nonzero=not pressed
'Other values in joybuf() should be preserved.
'If X and Y axis are not analog,
'  upward motion when joybuf(0) < joybuf(9)
'  down motion when joybuf(0) > joybuf(10)
'  left motion when joybuf(1) < joybuf(11)
'  right motion when joybuf(1) > joybuf(12)
	readjoy = io_readjoy(joybuf(), jnum)
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
			bp[bi] = (bp[bi] and &hff) or (s$[i] shl 8)
			'check sign
			if (bp[bi] and &h8000) > 0 then
				bp[bi] = bp[bi] or &hffff0000 'make -ve
			end if
			toggle = 0
			bi = bi + 1
		end if
	next
end SUB

SUB setupstack ()
	stackbottom = allocate(32768)
	if (stackbottom = 0) then
		'oh dear
		debug "Not enough memory for stack"
		exit sub
	end if
	stackptr = stackbottom
	stacksize = 32768
end SUB

SUB pushw (BYVAL word as integer)
	if stackptr - stackbottom > stacksize - 2 then
		dim newptr as ubyte ptr
		newptr = reallocate(stackbottom, stacksize + 32768)
		if newptr = 0 then
			debug "stack: out of memory"
			exit sub
		end if
		stacksize += 32768
		stackptr += newptr - stackbottom
		stackbottom = newptr
	end if
	*cast(short ptr, stackptr) = word
	stackptr += 2
end SUB

FUNCTION popw () as integer
	dim pw as short

	if (stackptr >= stackbottom + 2) then
		stackptr -= 2
		pw = *cast(short ptr, stackptr)
	else
		pw = 0
		debug "underflow"
	end if

	popw = pw
end FUNCTION

SUB pushdw (BYVAL dword as integer)
	if stackptr - stackbottom > stacksize - 4 then
		dim newptr as ubyte ptr
		newptr = reallocate(stackbottom, stacksize + 32768)
		if newptr = 0 then
			debug "stack: out of memory"
			exit sub
		end if
		stacksize += 32768
		stackptr += newptr - stackbottom
		stackbottom = newptr
	end if
	*cast(integer ptr, stackptr) = dword
	stackptr += 4
end SUB

FUNCTION popdw () as integer
	dim pdw as integer

	if (stackptr >= stackbottom - 4) then
		stackptr -= 4
		pdw = *cast(integer ptr, stackptr)
	else
		pdw = 0
		debug "underflow"
	end if

	popdw = pdw
end FUNCTION

SUB releasestack ()
	if stacksize > 0 then
		deallocate stackbottom
		stacksize = -1
	end if
end SUB

FUNCTION stackpos () as integer
	stackpos = stackptr - stackbottom
end FUNCTION

'read an int from the stack relative to current position (eg -1 is last word pushed - off should be negative)
FUNCTION readstackdw (BYVAL off as integer) as integer
	if stackptr + off * 4 >= stackbottom then
		readstackdw = *cptr(integer ptr, stackptr + off * 4)
	end if
END FUNCTION

'private functions
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

function calcblock(byval x as integer, byval y as integer, byval l as integer, byval t as integer) as integer
'returns -1 to draw no tile
't = 1 : draw non overhead tiles only (to avoid double draw)
't = 2 : draw overhead tiles only
	dim block as integer

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
		if (y < 0) or (y >= map_y) or (x < 0) or (x >= map_x) then
			if l = 0 and t <= 1 then
				'only draw the border tile once!
				return bordertile
			else
				return -1
			end if
		end if
	end if

	block = readmapblock(x, y, l)

	if block = 0 and l > 0 then
		return -1
	end if

	if t > 0 then
		if ((readpassblock(x, y) and 128) <> 0) xor (t = 2) then
			block = -1
		end if
	end if

	return block
end function

'----------------------------------------------------------------------
'Bitmap import functions - other formats are probably quite simple
'with Allegro or SDL or FreeImage, but we'll stick to this for now.
'----------------------------------------------------------------------
SUB bitmap2page (pal() as RGBcolor, bmp$, BYVAL p)
'loads the 24- or 8-bit bitmap bmp$ into page p with palette pal()
'I'm pretty sure this is only ever called with 320x200 pics, but I
'have tried to generalise it to cope with any size.
	dim header as BITMAPFILEHEADER
	dim info as BITMAPINFOHEADER
	dim pix as RGBTRIPLE
	dim pix8 as UByte
	dim bf as integer
	dim as integer w, h, maxw, maxh
	dim as ubyte ptr sptr, sbase
	dim ub as ubyte
	dim pad as integer

	if NOT fileisreadable(bmp$) then exit sub
	bf = freefile
	open bmp$ for binary access read as #bf

	get #bf, , header
	if header.bfType <> 19778 then
		'not a bitmap
		close #bf
		exit sub
	end if

	get #bf, , info

	if info.biBitCount <> 24 AND info.biBitCount <> 8 then
		close #bf
		exit sub
	end if

	sbase = spage(p)


	'navigate to the beginning of the bitmap data
	seek #bf, header.bfOffBits + 1


	IF info.biBitCount = 24 THEN
		'data lines are padded to 32-bit boundaries
		pad = 4 - ((info.biWidth * 3) mod 4)
		if pad = 4 then	pad = 0
		'crop images larger than screen
		maxw = info.biWidth - 1
		if maxw > 319 then
			maxw = 319
			pad = pad + ((info.biWidth - 320) * 3)
		end if
		maxh = info.biHeight - 1
		if maxh > 199 then
			maxh = 199
		end if
		for h = info.biHeight - 1 to 0 step -1
			if h > maxh then
				for w = 0 to maxw
					'read the data
					get #bf, , pix
				next
			else
				sptr = sbase + (h * 320)
				for w = 0 to maxw
					'read the data
					get #bf, , pix
					*sptr = nearcolor(pal(), pix.rgbtRed, pix.rgbtGreen, pix.rgbtBlue)
					sptr += 1
				next
			end if

			'padding to dword boundary, plus excess pixels
			for w = 0 to pad-1
				get #bf, , ub
			next
		next
	ELSEIF info.biBitCount = 8 THEN
		'data lines are padded to 32-bit boundaries
		pad = 4 - (info.biWidth mod 4)
		if pad = 4 then	pad = 0
		'crop images larger than screen
		maxw = info.biWidth - 1
		if maxw > 319 then
			maxw = 319
			pad = pad + ((info.biWidth - 320) * 3)
		end if
		maxh = info.biHeight - 1
		if maxh > 199 then
			maxh = 199
		end if
		for h = info.biHeight - 1 to 0 step -1
			if h > maxh then
				for w = 0 to maxw
					'read the data
					get #bf, , pix
				next
			else
				sptr = sbase + (h * 320)
				for w = 0 to maxw
					'read the data
					get #bf, , pix8 'assume they know what they're doing
					*sptr = pix8
					sptr += 1
				next
			end if

			'padding to dword boundary, plus excess pixels
			for w = 0 to pad-1
				get #bf, , ub
			next
		next
	END IF

	close #bf
END SUB

SUB bitmap2pal (bmp$, pal() as RGBcolor)
'loads the 24-bit 16x16 palette bitmap bmp$ into palette pal()
'so, pixel (0,0) holds colour 0, (0,1) has colour 16, and (15,15) has colour 255
	dim header as BITMAPFILEHEADER
	dim info as BITMAPINFOHEADER
	dim col as RGBTRIPLE
	dim bf as integer
	dim as integer w, h 

	if NOT fileisreadable(bmp$) then exit sub
	bf = freefile
	open bmp$ for binary access read as #bf

	get #bf, , header
	if header.bfType <> 19778 then
		'not a bitmap
		close #bf
		exit sub
	end if

	get #bf, , info

	if info.biBitCount <> 24 OR info.biWidth <> 16 OR info.biHeight <> 16 then
		close #bf
		exit sub
	end if

	'navigate to the beginning of the bitmap data
	seek #bf, header.bfOffBits + 1

	for h = 15 to 0 step -1
		for w = 0 to 15
			'read the data
			get #bf, , col
			pal(h * 16 + w).r = col.rgbtRed
			pal(h * 16 + w).g = col.rgbtGreen
			pal(h * 16 + w).b = col.rgbtBlue
		next
	next

	close #bf
END SUB

SUB loadbmp (f$, BYVAL x, BYVAL y, BYVAL p)
'loads the 4-bit bitmap f$ into page p at x, y
'sets palette to match file???
	dim header as BITMAPFILEHEADER
	dim info as BITMAPINFOHEADER
	dim bf as integer
	dim as integer maxw, maxh
	dim sbase as ubyte ptr
	dim i as integer
	dim col as RGBQUAD

	if NOT fileisreadable(f$) then exit sub
	bf = freefile
	open f$ for binary access read as #bf

	get #bf, , header
	if header.bfType <> 19778 then
		'not a bitmap
		close #bf
		exit sub
	end if

	get #bf, , info

	if info.biBitCount <> 4 then
		close #bf
		exit sub
	end if

	'use header offset to get to data
	seek #bf, header.bfOffBits + 1

	sbase = spage(p) + (y * 320) + x

	'crop images larger than screen
	maxw = info.biWidth - 1
	if maxw > 319 - x then	maxw = 319 - x
	maxh = info.biHeight - 1
	if maxh > 199 - y then 	maxh = 199 - y

	'call one of two loaders depending on compression
	if info.biCompression = BI_RGB then
		loadbmp4(bf, info.biWidth, info.biHeight, maxw, maxh, sbase)
	elseif info.biCompression = BI_RLE4 then
		loadbmprle4(bf, info.biWidth, info.biHeight, maxw, maxh, sbase)
	end if

	close #bf
END SUB

SUB loadbmp4(byval bf as integer, byval iw as integer, byval ih as integer, byval maxw as integer, byval maxh as integer, byval sbase as ubyte ptr)
'takes an open file handle and a screen pointer, should only be called within loadbmp
	dim pix as ubyte
	dim ub as ubyte
	dim linelen as integer
	dim toggle as integer
	dim bcount as integer
	dim as integer w, h
	dim sptr as ubyte ptr

	linelen = (iw + 1) \ 2 	'num of bytes
	linelen = ((linelen + 3) \ 4) * 4 	'nearest dword bound

	for h = ih - 1 to 0 step -1
		bcount = 0
		toggle = 0
		if h > maxh then
			for w = 0 to maxw
				if toggle = 0 then
					'read the data
					get #bf, , pix
					toggle = 1
					bcount += 1
				else
					toggle = 0
				end if
			next
		else
			sptr = sbase + (h * 320)
			for w = 0 to maxw
				if toggle = 0 then
					'read the data
					get #bf, , pix
					*sptr = (pix and &hf0) shr 4
					sptr += 1
					toggle = 1
					bcount += 1
				else
					'2nd nybble in byte
					*sptr = pix and &h0f
					sptr += 1
					toggle = 0
				end if
			next
		end if

		'padding to dword boundary, plus excess pixels
		while bcount < linelen
			get #bf, , ub
			bcount += 1
		wend
	next
END SUB

SUB loadbmprle4(byval bf as integer, byval iw as integer, byval ih as integer, byval maxw as integer, byval maxh as integer, byval sbase as ubyte ptr)
'takes an open file handle and a screen pointer, should only be called within loadbmp
	dim pix as ubyte
	dim ub as ubyte
	dim toggle as integer
	dim as integer w, h
	dim sptr as ubyte ptr
	dim i as integer
	dim as ubyte bval, v1, v2

	w = 0
	h = ih -1

	'read bytes until we're done
	while not eof(bf)
		'get command byte
		get #bf, , ub
		select case ub
			case 0	'special, check next byte
				get #bf, , ub
				select case ub
					case 0		'end of line
						w = 0
						h -= 1
					case 1		'end of bitmap
						exit while
					case 2 		'delta (how can this ever be used?)
						get #bf, , ub
						w = w + ub
						get #bf, , ub
						h = h + ub
					case else	'absolute mode
						toggle = 0
						for i = 1 to ub
							if toggle = 0 then
								get #bf, , pix
								toggle = 1
								bval = (pix and &hf0) shr 4
							else
								toggle = 0
								bval = pix and &h0f
							end if
							if h <= maxh and w <= maxw then
								sptr = sbase + (h * 320) + w
								*sptr = bval
							end if
							w += 1
						next
						if (ub + 1) mod 4 > 1 then	'is this right?
							get #bf, , ub 'pad to word bound
						end if
				end select
			case else	'run-length
				get #bf, , pix	'2 colours
				v1 = (pix and &hf0) shr 4
				v2 = pix and &h0f

				toggle = 0
				for i = 1 to ub
					if toggle = 0 then
						toggle = 1
						bval = v1
					else
						toggle = 0
						bval = v2
					end if
					if h <= maxh and w <= maxw then
						sptr = sbase + (h * 320) + w
						*sptr = bval
					end if
					w += 1
				next
		end select
	wend

end sub

FUNCTION loadbmppal (f$, pal() as RGBcolor)
'loads the palette of a 4-bit or 8-bit bmp into pal
'returns the number of bits
	dim header as BITMAPFILEHEADER
	dim info as BITMAPINFOHEADER
	dim col as RGBQUAD
	dim bf as integer
	dim i as integer

	if NOT fileisreadable(f$) then exit function
	bf = freefile
	open f$ for binary access read as #bf

	get #bf, , header
	if header.bfType <> 19778 then
		'not a bitmap
		close #bf
		exit function
	end if

	get #bf, , info

	loadbmppal = info.biBitCount

	if info.biBitCount = 4 or info.biBitCount = 8 then
		for i = 0 to (1 shl info.biBitCount) - 1
			get #bf, , col
			pal(i).r = col.rgbRed
			pal(i).g = col.rgbGreen
			pal(i).b = col.rgbBlue
		next
	end if
	close #bf
END FUNCTION

SUB convertbmppal (f$, mpal() as RGBcolor, pal(), BYVAL o)
'find the nearest match palette mapping from a 4/8 bit bmp f$ to
'the master palette mpal(), and store it in pal() starting at offset o
'for 4 bit bmps, pal() is a 2 bytes per int packed format used for
'sprite palettes, for 8bit bmps it is a simple array
	dim col8 as integer
	dim i as integer
	dim p as integer
	dim toggle as integer
	dim bitdepth as integer
	dim cols(255) as RGBcolor

	bitdepth = loadbmppal(f$, cols())

	if bitdepth = 4 then
		'read and translate the 16 colour entries
		p = o
		toggle = p mod 2
		for i = 0 to 15
			col8 = nearcolor(mpal(), cols(i).r, cols(i).g, cols(i).b)
			if toggle = 0 then
				pal(p) = col8
				toggle = 1
			else
				pal(p) = pal(p) or (col8 shl 8)
				toggle = 0
				p += 1
			end if
		next
	elseif bitdepth = 8 then
		for i = 0 to 255
			pal(o + i) = nearcolor(mpal(), cols(i).r, cols(i).g, cols(i).b)
		next
	end if
END SUB

FUNCTION bmpinfo (f$, dat())
	dim header as BITMAPFILEHEADER
	dim info as BITMAPINFOHEADER
	dim bf as integer

	if NOT fileisreadable(f$) then return 0
	bf = freefile
	open f$ for binary access read as #bf

	get #bf, , header
	if header.bfType <> 19778 then
		'not a bitmap
		bmpinfo = 0
		close #bf
		exit function
	end if

	get #bf, , info

	'only these 4 fields are returned by the asm
	dat(0) = info.biBitCount
	dat(1) = info.biWidth
	dat(2) = info.biHeight
	'seems to be a gap here, or all 4 bytes of height are returned
	'but I doubt this will be relevant anyway
	dat(3) = 0
	dat(4) = info.biCompression
	'code doesn't actually seem to use anything higher than 2 anway

	close #bf

	bmpinfo = -1
END FUNCTION

function nearcolor(pal() as RGBcolor, byval red as ubyte, byval green as ubyte, byval blue as ubyte) as ubyte
'figure out nearest palette colour
	dim as integer i, diff, best, save, rdif, bdif, gdif

	best = 1000000
	save = 0
	for i = 0 to 255
		rdif = red - pal(i).r
		gdif = green - pal(i).g
		bdif = blue - pal(i).b
		'diff = abs(rdif) + abs(gdif) + abs(bdif)
		diff = rdif^2 + gdif^2 + bdif^2
		if diff = 0 then
			'early out on direct hit
			save = i
			exit for
		end if
		if diff < best then
			save = i
			best = diff
		end if
	next

	nearcolor = save
end function


'-------------- Software GFX mode routines -----------------
sub setclip(l as integer, t as integer, r as integer, b as integer)
	clipl = l
	clipt = t
	clipr = r
	clipb = b
end sub

sub drawohr(byref spr as frame, x as integer, y as integer, scale as integer, trans as integer = -1)
	dim sptr as ubyte ptr
	dim as integer tx, ty
	dim as integer i, j, pix, spix

	'assume wrkpage
	sptr = spage(wrkpage)

	if scale = 0 then scale = 1

	'checking the clip region should really be outside the loop,
	'I think, but we'll see how this works
	ty = y
	for i = 0 to (spr.h * scale) - 1
		tx = x
		for j = 0 to (spr.w * scale) - 1
			'check bounds
			if not (tx < clipl or tx > clipr or ty < clipt or ty > clipb) then
				'ok to draw pixel
				pix = (ty * 320) + tx
				spix = ((i \ scale) * spr.w) + (j \ scale)
				'check mask
				if spr.mask <> 0 and trans then
					'not really sure whether to leave the masks like
					'this or change them above, this is the wrong
					'way round, really. perhaps.
					if spr.mask[spix] = 0 then
						sptr[pix] = spr.image[spix]
					end if
				else
					sptr[pix] = spr.image[spix]
				end if
			end if
			tx += 1
		next
		ty += 1
	next

end sub

sub grabrect(page as integer, x as integer, y as integer, w as integer, h as integer, ibuf as ubyte ptr, tbuf as ubyte ptr = 0)
'ibuf should be pre-allocated
	dim sptr as ubyte ptr
	dim as integer i, j, px, py, l

	if ibuf = null then exit sub

	sptr = spage(page)

	py = y
	for i = 0 to h-1
		px = x
		for j = 0 to w-1
			l = i * w + j
			'ignore clip rect, but check screen bounds
			if not (px < 0 or px > 319 or py < 0 or py > 199) then
				ibuf[l] = sptr[(py * 320) + px]
				if tbuf then
					if ibuf[l] = 0 then tbuf[l] = 1 else tbuf[l] = 0
				end if
			else
				ibuf[l] = 0
				tbuf[l] = 0
			end if
			px += 1
		next
		py += 1
	next

end sub



#DEFINE ID(a,b,c,d) asc(a) SHL 0 + asc(b) SHL 8 + asc(c) SHL 16 + asc(d) SHL 24
function isawav(fi$) as integer
  if not isfile(fi$) then return 0 'duhhhhhh

  dim _RIFF as integer = ID("R","I","F","F") 'these are the "signatures" of a
  dim _WAVE as integer = ID("W","A","V","E") 'wave file. RIFF is the format,
  dim _fmt_ as integer = ID("f","m","t"," ") 'WAVE is the type, and fmt_ and
  dim _data as integer = ID("d","a","t","a") 'data are the chunks

  dim chnk_ID as integer
  dim chnk_size as integer
  dim f as integer = freefile
  open fi$ for binary as #f

  get #f,,chnk_ID
  if chnk_ID <> _RIFF then return 0 'not even a RIFF file

  get #f,,chnk_size 'don't care

  get #f,,chnk_ID

  if chnk_ID <> _WAVE then return 0 'not a WAVE file, pffft

  'is this good enough? meh, sure.
  close #f
  return 1

end function


SUB setupsound ()
	setupmusic
end SUB

SUB closesound ()
 	closemusic
end SUB

SUB playsfx (BYVAL num, BYVAL l=0)
  sound_play(num,l)
end sub

SUB stopsfx (BYVAL num)
  sound_stop (num)
end sub

SUB pausesfx (BYVAL num)
  sound_pause(num)
end sub

SUB freesfx (BYVAL num)
  sound_free(num)
end sub

Function sfxisplaying(BYVAL num)
  return sound_playing(num)
end Function

Function fileisreadable(f$)
	dim fh as integer, err_code as integer
	fh = freefile
	err_code = open(f$ for binary access read as #fh)
	if err_code = 2 then
		'debug f$ & " unreadable (ignored)"
		return 0
	elseif err_code <> 0 then
		debug "Error " & err_code & " reading " & f$
		return 0
	end if
	close #fh
	return -1
end Function

Function fileiswriteable(f$)
	dim fh as integer
	fh = freefile
	if open (f$ for binary access read write as #fh) = 2 then
		'debug f$ & " unreadable (ignored)"
		return 0 
	end if
	close #fh
	return -1
end Function

FUNCTION getmusictype (file$)

	if file$ = "" then
	  'no further checking for blank names
	  return 0
	end if

	if isdir(file$) OR right(file$, 1) = SLASH then
	  'no further checking if this is a directory
	  return 0
	end if

	DIM ext$, chk
	ext$ = lcase(justextension(file$))

	'special case
	if str(cint(ext$)) = ext$ then return FORMAT_BAM

	SELECT CASE ext$
	CASE "bam"
		chk = FORMAT_BAM
	CASE "mid"
		chk = FORMAT_MIDI
	CASE "xm"
		chk = FORMAT_XM
	CASE "it"
	  chk = FORMAT_IT
	CASE "wav"
	  chk = FORMAT_WAV
	CASE "ogg"
	  chk = FORMAT_OGG
	CASE "mp3"
	  chk = FORMAT_MP3
	CASE "s3m"
	  chk = FORMAT_S3M
	CASE "mod"
	  chk = FORMAT_MOD
	CASE ELSE
	  debug "unknown format: " & file$ & " - " & ext$
	  chk = 0
	END SELECT

  return chk
END FUNCTION

'This should be replaced with a real hash

type SpriteCache
	s as string
	p as frame ptr
end type


redim shared sprcache(50) as SpriteCache

'Private
' unconditionally frees a sprite from memory.
' takes a pointer to a pointer so that the pointer can also be nulled, so it
' will not be used again accidentally.
sub sprite_delete(byval f as frame ptr ptr)
	if f = 0 then exit sub
	if *f = 0 then exit sub
	with **f
		if .image <> 0 then deallocate(.image)
		.image = 0
		if .mask <> 0 then deallocate(.mask)
		.mask = 0
	end with
	deallocate(*f)
	*f = 0
end sub

'Private:
' looks for a key in the cache. Will return the pointer associated with it.
function sprite_find_cache(byval s as string) as frame ptr
	dim i as integer
	for i = 0 to ubound(sprcache)
		if sprcache(i).s = s then return sprcache(i).p
	next
	return 0
end function

'Completely empty the sprite cache
sub sprite_empty_cache()
	dim i as integer
	for i = 0 to ubound(sprcache)
		with sprcache(i)
			'if .p <> 0 or .s <> "" then debug i & ": " & .p & ", " & .s
			if .p <> 0 then
				sprite_delete(@.p)
				.s = ""
			end if
		end with
	next
end sub

'Private:
' adds a frame to the cache with a given key. Sets the ->refcount member to 1.
' will overwrite older sprites with refcounts of 0 or less.
' if the key already exists, will update it to this pointer.
' will also expand the cache if it is full of referenced sprites.
sub sprite_add_cache(byval s as string, byval p as frame ptr, byval fr as integer = 0)
	if p = 0 then exit sub
	dim as integer i, sec = -1
	for i = fr to ubound(sprcache)
		with sprcache(i)
			if .s = "" then
				.s = s
				.p = p
				p->refcount = 1
				exit sub
			elseif .s = s then
				.p->refcount = 0
				.p = p
				.p->refcount = 1
				exit sub
			elseif .p->refcount <= 0 then
				sec = i
			end if
		end with
	next
	
	if sec > -1 then
		sprite_delete(@sprcache(sec).p)
		sprcache(sec).s = s
		sprcache(sec).p = p
		p->refcount = 1
		exit sub
	end if

	'no room? pah.
	redim preserve sprcache(ubound(sprcache) * 1.3 + 5)
	
	sprite_add_cache(s, p, i)
end sub

'Public:
' loads a sprite from a file. It expects 4-bit pixels, stored in columns (2/byte).
' it will return a pointer to the first frame (of num frames), and subsequent frames
' will be immediately after it in memory.
function sprite_load(byval fi as string, byval rec as integer, byval num as integer, byval wid as integer, byval hei as integer) as frame ptr
	
	dim ret as frame ptr
	dim hashstring as string = trimpath(fi) & "#" & rec 'we assume that all sprites in the same file are the same size
	'debug "Loading: " & hashstring
	
	ret = sprite_find_cache(hashstring)
	
	if ret then
		ret->refcount += 1
		'debug("Pulled cached copy: " & hashstring & "(" & ret->refcount & ")")
		return ret
	end if
	
	'debug "Must load from disk"
	
	'first, we do a bit of math:
	dim frsize as integer = wid * hei / 2
	dim recsize as integer = frsize * num
	
	'make sure the file is real
	if not isfile(fi) then return 0
	
	'now, we can load the sprite
	dim f as integer = freefile
	
	'open() returns 0 for success
	if open(fi for binary as #f) then return 0
	
	'if we get here, we can assume that all's well, and allocate the memory
	ret = callocate(sizeof(frame) * num)
	
	'no memory? shucks.
	if ret = 0 then
		close #f
		return 0
	end if
	
	'find the right sprite (remember, it's base-1)
	seek #f, recsize * rec + 1
	
	dim i as integer, x as integer, y as integer, z as ubyte
	
	#define SPOS (y * wid + x)
	
	for i = 0 to num - 1
		with ret[i]
			'each frame has two bitmaps: the image and the mask
			.w = wid
			.h = hei
			
			'although it's a four-bit sprite, it IS an 8-bit bitmap.
			.image = allocate(wid * hei)
			.mask = allocate(wid * hei)
			
			for x = 0 to wid - 1
				for y = 0 to hei - 1
					'pull up two pixels
					get #f,,z
					
					'the high nybble is the first pixel
					.image[SPOS] = (z SHR 4)
					if .image[SPOS] = 0 then .mask[SPOS] = &hFF else .mask[SPOS] = 0
					
					y+=1
					
					'and the low nybble is the second one
					.image[SPOS] = z AND 15
					if .image[SPOS] = 0 then .mask[SPOS] = &hFF else .mask[SPOS] = 0
					
					'it is worth mentioning that sprites are stored in columns, not rows
				next
			next
		end with
	next
	
	#undef SPOS
	
	close #f
	
	sprite_add_cache(hashstring, ret)
	return ret
end function

'Public:
' Releases a reference to a sprite. Decrements the refcount, and nulls the pointer.
' Although the pointer is still valid, the caller can no longer use it without
' breaking the reference system
sub sprite_unload(byval p as frame ptr ptr)
	if p = 0 then exit sub
	if *p = 0 then exit sub
	if (*p)->refcount = -1 then
		sprite_delete(p)
	else
		(*p)->refcount -= 1
		*p = 0
	end if
end sub

function sprite_duplicate(byval p as frame ptr, byval clr as integer = 0) as frame ptr
	dim ret as frame ptr, i as integer
	
	if p = 0 then return 0
	
	ret = allocate(sizeof(frame))
	
	if ret = 0 then return 0
	
	ret->w = p->w
	ret->h = p->h
	ret->refcount = -1 'that is, it's not refcounted
	if p->image then
		ret->image = callocate(ret->w * ret->h)
		if clr = 0 then
			for i = 0 to ret->w * ret->h - 1
				ret->image[i] = p->image[i]
			next
		end if
	end if
	if p->mask then
		ret->mask = callocate(ret->w * ret->h)
		if clr = 0 then
			for i = 0 to ret->w * ret->h - 1
				ret->mask[i] = p->mask[i]
			next
		end if
	end if
	
	return ret
	
end function

'Public:
' draws a sprite to a page. scale must be greater than 1. if trans is false, the
' mask will be wholly ignored (and, trans will be forced to false if the mask
' doesn't even exist)
sub sprite_draw(byval spr as frame ptr, Byval pal as Palette16 ptr, Byval x as integer, Byval y as integer, Byval scale as integer = 1, Byval trans as integer = -1, byval page as integer = wrkpage)
	dim sptr as ubyte ptr
	dim as integer tx, ty
	dim as integer sx, sy, pix, spix

	if spr = 0 then
		debug "trying to draw null sprite"
		exit sub
	end if
	
	'assume wrkpage
	sptr = spage(page)

	if scale = 0 then scale = 1
	
	if spr->mask = 0 then trans = 0 'no mask? no transparency.
	
	if trans then trans = -1
	
	dim as integer sxfrom, sxto, syfrom, syto
	
	sxfrom = large(clipl, x)
	sxto = small(clipr, x + (spr->w * scale) - 1)
	
	syfrom = large(clipt, y)
	syto = small(clipb, y + (spr->h * scale) - 1)
	
	With *spr
		'ty = syfrom
		if not trans then
			for ty = syfrom to syto
				'tx = sxfrom
				for tx = sxfrom to sxto
					'figure out where to put the pixel
					pix = (ty * 320) + tx
					'and where to get the pixel from
					spix = (int((ty-syfrom) / scale) * .w) + int((tx-sxfrom) / scale)
					'check mask
					
					if pal <> 0 then
						sptr[pix] = pal->col(.image[spix])
					else
						sptr[pix] = .image[spix]
					end if
				next
			next
		else
			for ty = syfrom to syto
				'tx = sxfrom
				for tx = sxfrom to sxto
					'figure out where to put the pixel
					pix = (ty * 320) + tx
					'and where to get the pixel from
					spix = (((ty-y) \ scale) * .w) + ((tx-x) \ scale)
					
					'check mask
					if .mask[spix] = 0 then
						if pal <> 0 then
							sptr[pix] = pal->col(.image[spix])
						else
							sptr[pix] = .image[spix]
						end if
					end if
					
				next
			next
		end if

	End With
end sub

'Public:
' returns a sprite in the midst of a given fade out. amnt is expected to be the number
' of ticks left in the fade out. style is the specific transition.
function sprite_dissolve(byval spr as frame ptr, byval tim as integer, byval p as integer, byval style as integer = 0, byval direct as integer = 0) as frame ptr

	dim cpy as frame ptr
	
	if spr = 0 then return 0
	
	cpy = sprite_duplicate(spr)
	
	if cpy = 0 then return 0
	
	dim as integer i, j, sx, sy, tog

	select case style
		case 0 'scattered pixel dissolve
			dim t as integer = spr->w / tim * p
			randomize 1 ' use the same random seed for each frame
			for i = 0 to t - 1
				for sy = 0 to spr->h - 1
					sx = int(rnd * spr->w)
					cpy->mask[sy * spr->w + sx] = &hff
				next sy
			next i
			randomize timer 're-seed random

		case 1 'crossfade
			if p > tim / 2 then
				dim m as integer = spr->w * spr->h / tim * (p - tim / 2) * 2
				sx = 0
				sy = 0
				tog = 0
				for i = 0 to spr->w * spr->h - 1
					sx += 1
					tog = tog xor 1
					if sx >= spr->w then
						sy += 1
						sx = 0
						tog = tog xor 1
					end if
					cpy->mask[i] = large(cint(cpy->mask[i]), &hff * tog)
				next
				sx = 0
				sy = 0
				tog = 1
				for i = 0 to m - 1
					sx += 1
					tog = tog xor 1
					if sx >= spr->w then
						sy += 1
						sx = 0
						tog = tog xor 1
					end if
					cpy->mask[i] = large(cint(cpy->mask[i]), &hff * tog)
				next
			else
				dim m as integer = spr->w * spr->h / tim * p * 2
				sx = 0
				sy = 0
				for i = 0 to m - 1
					sx += 1
					tog = tog xor 1
					if sx >= spr->w then
						sy += 1
						sx = 0
						tog = tog xor 1
					end if
					cpy->mask[i] = large(cint(cpy->mask[i]), &hff * tog)
				next
			end if
		case 2 'diagonal vanish
			i = spr->w / tim * p * 2
			j = i
			for sy = 0 to i
				j = i - sy
				if sy >= spr->h then exit for
				for sx = 0 to j
					if sx >= spr->w then exit for
					cpy->mask[sy * spr->w + sx] = &hff
				next
			next
		case 3 'sink into ground
			dim t as integer = spr->h / tim * p
			'debug str(t)
			for sy = spr->h - 1 to t step -1
				for sx = 0 to spr->w - 1
					dim s as integer = (sy-t) * spr->w + sx
					if s < 0 or s > spr->w * spr->h - 1 then
						debug "!!! " & s
					end if
					cpy->image[sy * spr->w + sx] = spr->image[s]
					cpy->mask[sy * spr->w + sx] = spr->mask[s]
				next
			next
			for i = 0 to t - 1
				for sx = 0 to spr->w - 1
					cpy->mask[i * spr->w + sx] = &hff
				next
			next
	end select
	
	if direct then
		deallocate(spr->image)
		deallocate(spr->mask)
		spr->image = cpy->image
		spr->mask = cpy->mask
		cpy->image = 0
		cpy->mask = 0
		sprite_delete(@cpy)
		
		return spr
	else
		return cpy
	end if
end function

'Public:
' returns a copy of the sprite flipped horizontally. The new sprite is not
' cached, and is not ref-counted.
function sprite_flip_horiz(byval spr as frame ptr, byval direct as integer = 0) as frame ptr
	dim ret as frame ptr
	
	if spr = 0 then return 0
	
	ret = sprite_duplicate(spr)
	
	if ret = 0 then return 0
	
	dim as integer x, y
	
	for y = 0 to spr->h - 1
		for x = 0 to spr->w - 1 
			ret->image[y * spr->w + x] = spr->image[y * spr->w + (spr->w - x - 1)]
			ret->mask[y * spr->w + x] = spr->mask[y * spr->w + (spr->w - x - 1)]
		next
	next
	
	if direct then
		deallocate(spr->image)
		deallocate(spr->mask)
		spr->image = ret->image
		spr->mask = ret->mask
		ret->image = 0
		ret->mask = 0
		sprite_delete(@ret)
		return spr
	else
		return ret
	end if
end function

'Public:
' returns a copy of the sprite flipped vertically. The new sprite is not cached,
' and is not ref-counted.
function sprite_flip_vert(byval spr as frame ptr, byval direct as integer = 0) as frame ptr
	dim ret as frame ptr
	
	if spr = 0 then return 0
	
	ret = sprite_duplicate(spr)
	
	if ret = 0 then return 0
	
	dim as integer x, y
	
	for y = 0 to spr->h - 1
		for x = 0 to spr->w - 1
			ret->image[y * spr->w + x] = spr->image[(spr->h - y - 1) * spr->w + x]
			ret->mask[y * spr->w + x] = spr->mask[(spr->h - y - 1) * spr->w + x]
		next
	next
	
	if direct then
		deallocate(spr->image)
		deallocate(spr->mask)
		spr->image = ret->image
		spr->mask = ret->mask
		ret->image = 0
		ret->mask = 0
		sprite_delete(@ret)
		return spr
	else
		return ret
	end if
end function

sub sprite_clear(byval spr as frame ptr)
	dim as integer i
	
	for i = 0 to spr->w * spr->h - 1
		spr->image[i] = 0
		spr->mask[i] = 0
	next
end sub

' function sprite_scroll(byval spr as frame ptr, byval h as integer = 0, byval v as integer = 0, byval wrap as integer = 0, byval direct as integer = 0) as frame ptr

' 	dim ret as frame ptr, x as integer, y as integer
' 	
' 	ret = sprite_clear(spr, -1)
' 	
' 	'first scroll horizontally
' 	
' 	if h <> 0 then
' 		if h > 0 then
' 			for y = 0 to spr->h - 1
' 				for x = spr->w - 1 to h step -1
' 					ret->image[y * spr->h + x] = spr->image[y * spr->h - h + x]
' 					ret->mask[y * spr->h + x] = spr->mask[y * spr->h - h + x]
' 				next
' 			next
' 			if wrap then
' 				for y = 0 to spr->h - 1
' 					for x = 0 to h - 1
' 						ret->image[y * spr->h + x] = spr->image[y * spr->h + (x + spr->w - h)]
' 						ret->mask[y * spr->h + x] = spr->mask[y * spr->h + (x + spr->w - h)]
' 					next
' 				next
' 			end if
' 		else if h < 0 then
' 			for y = 0 to spr->h - 1
' 				for x = 0 to abs(h) - 1
' 					ret->image[y * spr->h + x] = spr->image[y * spr->h - h + x]
' 					ret->mask[y * spr->h + x] = spr->mask[y * spr->h - h + x]
' 				next
' 			next
' 			if wrap then
' 				for y = 0 to spr->h - 1
' 					for x = abs(h) to spr->w - 1
' 						ret->image[y * spr->h - h + x] = spr->image[y * spr->h + x]
' 						ret->mask[y * spr->h - h + x] = spr->mask[y * spr->h + x]
' 					next
' 				next
' 			end if
' 		end if
' 	end if
' 	
' 	'then scroll vertically
' 	
' 	if v <> 0 then
' 	
' 	end if
' 	
' 	if direct then
' 		deallocate(spr->image)
' 		deallocate(spr->mask)
' 		spr->image = ret->image
' 		spr->mask = ret->mask
' 		ret->image = 0
' 		ret->mask = 0
' 		sprite_delete(@ret)
' 		return spr
' 	else
' 		return ret
' 	end if
' end function

'This should be replaced with a real hash

type Palette16Cache
	s as string
	p as palette16 ptr
end type


redim shared palcache(50) as Palette16Cache

sub Palette16_delete(byval f as Palette16 ptr ptr)
	if f = 0 then exit sub
	if *f = 0 then exit sub
	deallocate(*f)
	*f = 0
end sub

'Completely empty the palette16 cache
sub Palette16_empty_cache()
	dim i as integer
	for i = 0 to ubound(palcache)
		with palcache(i)
			'if .p <> 0 or .s <> "" then debug i & ": " & .p & ", " & .s
			if .p <> 0 then
				Palette16_delete(@.p)
				.s = ""
			end if
		end with
	next
end sub

function Palette16_find_cache(byval s as string) as Palette16 ptr
	dim i as integer
	for i = 0 to ubound(palcache)
		if palcache(i).s = s then return palcache(i).p
	next
end function

sub Palette16_add_cache(byval s as string, byval p as Palette16 ptr, byval fr as integer = 0)
	if p = 0 then exit sub
	dim as integer i, sec = -1
	for i = fr to ubound(palcache)
		with palcache(i)
			if .s = "" then
				.s = s
				.p = p
				p->refcount = 1
				exit sub
			elseif .p->refcount <= 0 then
				sec = i
			end if
		end with
	next
	
	if sec > 0 then
		Palette16_delete(@palcache(sec).p)
		palcache(sec).s = s
		palcache(sec).p = p
		p->refcount = 1
		exit sub
	end if
	
	'no room? pah.
	redim preserve palcache(ubound(palcache) * 1.3 + 5)
	
	Palette16_add_cache(s, p, i)
end sub





function palette16_load(byval fil as string, byval num as integer, byval autotype as integer = 0, byval spr as integer = 0) as palette16 ptr
	dim f as integer, ret as palette16 ptr
	dim hashstring as string
	if num > -1 then
		hashstring = trimpath(fil) & "#" & num & ":0"
	else
		num = getdefaultpal(autotype, spr)
		if num <> -1 then
			hashstring = trimpath(fil) & "#" & num & ":" & spr
		else
			return 0
		end if
	end if
	
	'debug "Loading: " & hashstring
	ret = palette16_find_cache(hashstring)
	
	if ret <> 0 then
		ret->refcount += 1
		return ret
	end if
	
	if not isfile(fil) then return 0
	
	f = freefile
	
	if open(fil for binary as #f) then return 0
	
	
	dim mag as short
	
	get #f, 1, mag
	
	if mag = 4444 then
		get #f,,mag
		if num > mag then
			close #f
			return 0
		end if
		
		seek #f, 17 + 16 * num
	else
		seek #f, 8 + 16 * num
	end if
	
	ret = callocate(sizeof(palette16))
	
	if ret = 0 then
		close #f
		debug "Could not create palette, no memory"
		return 0
	end if
	
	'see, it's "mag"ic, since it's used for so many things
	for mag = 0 to 15
		get #f,, ret->col(mag)
	next
	
	close #f
	
	palette16_add_cache(hashstring, ret)
	
	'dim d as string
	'd = hex(ret->col(0))
	'for mag = 1 to 15
	'	d &= "," & hex(ret->col(mag))
	'next
	
	'debug d
	
	return ret
	
end function

sub palette16_unload(byval p as palette16 ptr ptr)
	if p = 0 then exit sub
	if *p = 0 then exit sub
	(*p)->refcount -= 1
	'debug "unloading palette (" & ((*p)->refcount) & " more copies!)"
	*p = 0
end sub
