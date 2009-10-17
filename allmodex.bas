'' FBOHR COMPATIBILITY FUNCTIONS
'' GPL and stuff. See LICENSE.txt.
'
#include "compat.bi"
#ifdef __FB_WIN32__
include_windows_bi()
#endif
#include "common.bi"
#include "allmodex.bi"
#include "gfx.bi"
#include "music.bi"
#include "bitmap.bi"
#include "util.bi"
#include "const.bi"
#include "uiconst.bi"

option explicit

#define NULL 0

'Note: While this works (at last check), it's not used anywhere, and you most probably do not need it
Const NOREFC = -1234

type node 	'only used for floodfill
	x as integer
	y as integer
	nextnode as node ptr
end type

declare sub drawohr(byref spr as frame, byval pal as Palette16 ptr = null, byval x as integer, byval y as integer, byval scale as integer = 1, byval trans as integer = -1, byval page as integer)
declare sub grabrect(page as integer, x as integer, y as integer, w as integer, h as integer, ibuf as ubyte ptr, tbuf as ubyte ptr = 0)
declare SUB loadbmp4(byval bf as integer, byval iw as integer, byval ih as integer, byval maxw as integer, byval maxh as integer, byval sbase as ubyte ptr)
declare SUB loadbmprle4(byval bf as integer, byval iw as integer, byval ih as integer, byval maxw as integer, byval maxh as integer, byval sbase as ubyte ptr)

'used for map and pass
DECLARE SUB setblock (BYVAL x as integer, BYVAL y as integer, BYVAL v as integer, byval l as integer, BYVAL mp as integer ptr)
DECLARE FUNCTION readblock (BYVAL x as integer, BYVAL y as integer, byval l as integer, BYVAL mp as integer ptr) as integer

declare function matchmask(match as string, mask as string) as integer
declare function calcblock(byval x as integer, byval y as integer, byval l as integer, byval t as integer) as integer

'internal allmodex use only sprite functions
declare sub sprite_freemem(byval f as frame ptr)
declare sub Palette16_delete(byval f as Palette16 ptr ptr)

'slight hackery to get more versatile read function
declare function fget alias "fb_FileGet" ( byval fnum as integer, byval pos as integer = 0, byval dst as any ptr, byval bytes as uinteger ) as integer
declare function fput alias "fb_FilePut" ( byval fnum as integer, byval pos as integer = 0, byval src as any ptr, byval bytes as uinteger ) as integer


#if __FB_VERSION__ > "0.16"
#define threadbs any ptr
#else
#define threadbs integer
#endif

declare sub pollingthread(byval as threadbs)

'global
dim vpages(0 to 15) as Frame ptr  'up to 6 used at once, last I counted

'module shared
dim shared wrkpage as integer  'used to track which page the clips are for; also used by some legacy modex functions

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
dim shared flagtime as double = 0.0
dim shared waitset as integer

dim shared keybd(-1 to 127) as integer  'keyval array
dim shared keybdstate(127) as integer  '"real"time array
dim shared keysteps(127) as integer
dim shared keyrepeatwait as integer = 8
dim shared keyrepeatrate as integer = 1
dim shared diagonalhack as integer

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
dim shared fonts(2) as Font

dim shared as integer clipl, clipt, clipr, clipb 'these clip to the current wrkpage and must be reset whenever that changes

dim shared intpal(0 to 255) as RGBcolor	'current palette
dim shared updatepal as integer  'setpal called, load new palette at next setvispage

dim shared fpsframes as integer = 0
dim shared fpstime as double = 0.0
dim shared fpsstring as string
dim shared showfps as integer = 0

sub setmodex()
	dim i as integer

	'initialise software gfx
	for i as integer = 0 to 3
		vpages(i) = sprite_new(320, 200, , YES)
	next
	'other vpages slots are for temporary pages

	gfx_init
	setclip , , , , 0

	'init vars
	stacksize = -1
	for i = 0 to 127
		keybd(i) = 0
		keybdstate(i) = 0
 		keysteps(i) = 0
	next
	endpollthread = 0
	mouselastflags = 0
	mouseflags = 0

	keybdmutex = mutexcreate
	keybdthread = threadcreate(@pollingthread)

	io_init
	mouserect(0,319,0,199)

	fpstime = TIMER
	fpsframes = 0
	fpsstring = ""
end sub

sub restoremode()
	gfx_close
	'clean up io stuff
	endpollthread = 1
	threadwait keybdthread
	mutexdestroy keybdmutex

	'clear up software gfx
	for i as integer = 0 to ubound(vpages)
		sprite_unload(@vpages(i))
	next

	releasestack
end sub

SUB freepage (byval page as integer)
	if page < 0 orelse page > ubound(vpages) orelse vpages(page) = NULL then
		debug "Tried to free unallocated/invalid page " & page
		exit sub
	end if

	sprite_unload(@vpages(page))
	if wrkpage = page then
		setclip , , , , 0
	end if
END SUB

FUNCTION registerpage (byval spr as Frame ptr) as integer
	for i as integer = 0 to ubound(vpages)
		if vpages(i) = NULL then
			vpages(i) = spr
			if spr->refcount <> NOREFC then	spr->refcount += 1
			return i
		end if
	next

	fatalerror "Max number of video pages exceeded"
END FUNCTION

FUNCTION allocatepage() as integer
	dim fr as Frame ptr = sprite_new(320, 200, , YES)

	dim ret as integer = registerpage(fr)
	sprite_unload(@fr) 'we're not hanging onto it, vpages() is
	
	return ret
END FUNCTION

'TODO: this sub has not been modified for frame pitch yet, because of a catch-22 situation
'anyway, I'd rather get rid of all those ugly arguments
SUB copypage (BYVAL page1 as integer, BYVAL page2 as integer, BYVAL y as integer = 0, BYVAL top as integer = 0, BYVAL bottom as integer = 199)
	if vpages(page1)->w <> vpages(page2)->w then
		debug "bad page copy"
		exit sub
	end if
	top = bound(top, 0, vpages(page2)->h - 1)
	y = bound(y, 0, vpages(page1)->h - 1)
	dim size as integer
	size = vpages(page1)->w * bound(bottom - top + 1, 0, vpages(page1)->h - large(y, top))
	memmove(vpages(page2)->image + vpages(page2)->w * top, vpages(page1)->image + vpages(page1)->w * y, size)
	'video pages do not use masks
end sub

'want to get rid of those ugly arguments
'TODO: delete this sub; not updated for pitch either
SUB clearpage (BYVAL page as integer, BYVAL colour as integer = -1, BYVAL top as integer = 0, BYVAL bottom as integer = 199)
	if colour = -1 then colour = uilook(uiBackground)
	top = bound(top, 0, vpages(page)->h - 1)
	bottom = bound(bottom, top, vpages(page)->h - 1)
	memset(vpages(page)->image + vpages(page)->w * top, colour, vpages(page)->w * (bottom - top + 1))
end SUB

SUB setvispage (BYVAL page as integer)
	fpsframes += 1
	if timer > fpstime + 1 then
		fpsstring = "fps:" & INT(10 * fpsframes / (timer - fpstime)) / 10
		fpstime = timer
		fpsframes = 0
	end if
	if showfps then
		edgeprint fpsstring, 255, 190, uilook(uiText), page
	end if

	'the fb backend may freeze up if they collide with the polling thread (why???)
	mutexlock keybdmutex
	if updatepal then
		gfx_setpal(intpal())
		updatepal = 0
	end if	
	gfx_showpage(vpages(page)->image)
	mutexunlock keybdmutex
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

#define POINT_CLIPPED(x, y) ((x) < clipl orelse (x) > clipr orelse (y) < clipt orelse (y) > clipb)

#define PAGEPIXEL(x, y, p) vpages(p)->image[vpages(p)->pitch * (y) + (x)]
#define FRAMEPIXEL(x, y, fr) fr->image[fr->pitch * (y) + (x)]

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

FUNCTION readpassblock (BYVAL x as integer, BYVAL y as integer) as integer
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

SUB drawmap (BYVAL x as integer, BYVAL y as integer, BYVAL l as integer, BYVAL t as integer, BYVAL tileset as TilesetData ptr, BYVAL p as integer, byval trans as integer = 0)
	'overrides setanim
	anim1 = tileset->tastuf(0) + tileset->anim(0).cycle
	anim2 = tileset->tastuf(20) + tileset->anim(1).cycle
	drawmap x, y, l, t, tileset->spr, p, trans
END SUB

SUB drawmap (BYVAL x as integer, BYVAL y as integer, BYVAL l as integer, BYVAL t as integer, BYVAL tilesetsprite as Frame ptr, BYVAL p as integer, byval trans as integer = 0)
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
	dim todraw as integer
	dim tileframe as frame
	
	if wrkpage <> p then
		setclip , , , , p
	end if

	'set viewport to allow for top and bottom bars (TODO: remove this)
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

	'debug trans

	tileframe.w = 20
	tileframe.h = 20
	tileframe.pitch = 20

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
				tileframe.image = tilesetsprite->image + todraw * 20 * 20
				if tilesetsprite->mask then 'just in case it happens some day
					tileframe.mask = tilesetsprite->mask + todraw * 20 * 20
				else
					tileframe.mask = NULL
				end if

				'draw it on the map
				drawohr(tileframe, , tx, ty, , trans, p)
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
	dim hspr as frame ptr
	dim dspr as ubyte ptr
	dim nib as integer
	dim i as integer
	dim spix as integer
	dim pix as integer
	dim row as integer

	if wrkpage <> page then
		setclip , , , , page
	end if

	sw = pic(picoff)
	sh = pic(picoff+1)
	picoff = picoff + 2

	hspr = sprite_new(sw, sh)
	dspr = hspr->image

	'now do the pixels
	'pixels are in columns, so this might not be the best way to do it
	'maybe just drawing straight to the screen would be easier
	nib = 0
	row = 0
	for i = 0 to (sw * sh) - 1
		select case nib 			' 2 bytes = 4 nibbles in each int
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
			pix = 0					' transparent
		else
			'palettes are interleaved like everything else
			pix = pal((po + spix) \ 2)	' get color from palette
			if (po + spix) mod 2 = 1 then
				pix = (pix and &hff00) shr 8
			else
				pix = pix and &hff
			end if
		end if
		*dspr = pix				' set image pixel
		dspr = dspr + sw
		row = row + 1
		if (row >= sh) then 	'ugh
			dspr = dspr - (sw * sh)
			dspr = dspr + 1
			row = 0
		end if
		nib = nib + 1
		nib = nib and 3
	next
	'now draw the image
	drawohr(*hspr, , x, y, scale, trans, page)
	'what a waste
	sprite_unload(@hspr)
end SUB

SUB wardsprite (pic() as integer, BYVAL picoff as integer, pal() as integer, BYVAL po as integer, BYVAL x as integer, BYVAL y as integer, BYVAL page as integer, BYVAL trans = -1)
'this just draws the sprite mirrored
'the coords are still top-left
	dim sw as integer
	dim sh as integer
	dim hspr as frame ptr
	dim dspr as ubyte ptr
	dim nib as integer
	dim i as integer
	dim spix as integer
	dim pix as integer
	dim row as integer

	if wrkpage <> page then
		setclip , , , , page
	end if

	sw = pic(picoff)
	sh = pic(picoff+1)
	picoff = picoff + 2

	hspr = sprite_new(sw, sh)
	dspr = hspr->image
	dspr = dspr + sw - 1 'jump to last column

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
			pix = 0					' transparent
		else
			'palettes are interleaved like everything else
			pix = pal((po + spix) \ 2)	' get color from palette
			if (po + spix) mod 2 = 1 then
				pix = (pix and &hff00) shr 8
			else
				pix = pix and &hff
			end if
		end if
		*dspr = pix				' set image pixel
		dspr = dspr + sw
		row = row + 1
		if (row >= sh) then 	'ugh
			dspr = dspr - (sw * sh)
			dspr = dspr - 1		' right to left for wardsprite
			row = 0
		end if
		nib = nib + 1
		nib = nib and 3
	next

	'now draw the image
	drawohr(*hspr, , x, y, , trans, page)

	sprite_unload(@hspr)
end SUB

SUB stosprite (pic() as integer, BYVAL picoff as integer, BYVAL x as integer, BYVAL y as integer, BYVAL page as integer)
'This is the opposite of loadsprite, ie store raw sprite data in screen p
'starting at x, y.
	dim i as integer
	dim poff as integer
	dim toggle as integer
	dim sbytes as integer
	dim h as integer
	dim w as integer

	if wrkpage <> page then
		setclip , , , , page
	end if

	poff = picoff
	h = pic(poff)
	w = pic(poff + 1)
	poff += 2
	sbytes = ((w * h) + 1) \ 2 	'only 4 bits per pixel

	y += x \ 320
	x = x mod 320

	'copy from passed int buffer, with 2 bytes per int as usual
	toggle = 0
	for i = 0 to sbytes - 1
		if toggle = 0 then
			PAGEPIXEL(x, y, page) = (pic(poff) and &hff00) shr 8
			toggle = 1
		else
			PAGEPIXEL(x, y, page) = pic(poff) and &hff
			toggle = 0
			poff += 1
		end if
		x += 1
		if x = 320 then y += 1: x = 0
	next

end SUB

SUB loadsprite (pic() as integer, BYVAL picoff as integer, BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL page as integer)
'reads sprite from given page into pic(), starting at picoff
	dim i as integer
	dim poff as integer
	dim toggle as integer
	dim sbytes as integer
	dim temp as integer

	if wrkpage <> page then
		setclip , , , , page
	end if

	sbytes = ((w * h) + 1) \ 2 	'only 4 bits per pixel

	y += x \ 320
	x = x mod 320

	'copy to passed int buffer, with 2 bytes per int as usual
	toggle = 0
	poff = picoff
	pic(poff) = w			'these are 4byte ints, not compat w. orig.
	pic(poff+1) = h
	poff += 2
	for i = 0 to sbytes - 1
		temp = PAGEPIXEL(x, y, page)
		if toggle = 0 then
			pic(poff) = temp shl 8
		else
			pic(poff) = pic(poff) or temp
			poff += 1
		end if
		toggle xor= 1
		x += 1
		if x = 320 then y += 1: x = 0
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
	sbase = vpages(page)->image + (vpages(page)->pitch * y) + x

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

'FIXME DELETEME
'--This code is for screen page debugging, and will be removed in the future!  (FIXME: this is getting old)
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

FUNCTION keyval (BYVAL a as integer, BYVAL rwait as integer = 0, BYVAL rrate as integer = 0) as integer
'except for special keys (like -1), each key reports 3 bits:
'
'bit 0: key was down at the last setkeys call
'bit 1: keypress event (either new keypress, or key-repeat) during last setkey-setkey interval
'bit 2: new keypress during last setkey-setkey interval

	DIM result as integer = keybd(a)
	IF a >= 0 THEN
		IF rwait = 0 THEN rwait = keyrepeatwait
		IF rrate = 0 THEN rrate = keyrepeatrate

		'awful hack to avoid arrow keys firely alternatively with rrate > 1
		DIM arrowkey as integer = 0
		IF a = scLeft OR a = scRight OR a = scUp OR a = scDown THEN arrowkey = -1
		IF arrowkey AND diagonalhack <> -1 THEN RETURN (result AND 5) OR (diagonalhack AND keybd(a) > 0)
		IF keysteps(a) >= rwait THEN
			IF ((keysteps(a) - rwait - 1) MOD rrate = 0) THEN result OR= 2
			IF arrowkey THEN diagonalhack = result AND 2
		END IF
	END IF
	RETURN result
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
	dim as integer joybutton = 0, joyx = 0, joyy = 0, sleepjoy = 3
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
		if sleepjoy > 0 then
			sleepjoy -= 1
		elseif io_readjoysane(0, joybutton, joyx, joyy) then
			for i = 16 to 1 step -1
				if joybutton and (i ^ 2) then key = 127 + i
			next i
		end if
		sleep 50
	loop while key = 0

	getkey = key
end FUNCTION

SUB setkeyrepeat (rwait as integer = 8, rrate as integer = 1)
	keyrepeatwait = rwait
	keyrepeatrate = rrate
END SUB

SUB setkeys ()
'Updates the keybd array (which keyval() wraps) to reflect new keypresses
'since the last call, also clears all keypress events (except key-is-down)
'
'keysteps is the number of setkeys calls that a key has been down, used
'for flexible key-repeat
'
'Note that currently key repeat events are triggered every 25ms, not every
'setkeys call

	dim a as integer
	mutexlock keybdmutex
	for a = 0 to &h7f
		keybd(a) = keybdstate(a)
		if keybd(a) and 1 then
			keysteps(a) += 1
		end if
		keybdstate(a) = keybdstate(a) and 1
	next
	'Check to see if the operating system has received a request
	'to close the window (clicking the X) and set the magic keyboard
	'index -1 if so. It can only be unset with clearkey.
	IF INKEY$ = CHR$(255) + "k" THEN
		keybd(-1) = 1
	'ELSE
		'keybd(-1) = 0
	END IF
	if keybd(scPageup) > 0 and keybd(scPagedown) > 0 and keybd(scEsc) > 1 then keybd(-1) = 1

#ifdef IS_CUSTOM
	if keybd(-1) then keybd(scEsc) = 7
#endif

	mutexunlock keybdmutex

	'reset arrow key fire state
	diagonalhack = -1

	if keyval(scCtrl) > 0 and keyval(scTilde) and 4 then
		showfps xor= 1
	end if

	'FIXME DELETEME
	'--This code is for screen page debugging, and will be removed in the future!
	if keyval(scScrollLock) > 0 then 'Scroll-lock
		clearkey(scScrollLock)
		if keyval(sc2) > 1 then clearkey(sc2) : debug_screen_page 2
		if keyval(sc3) > 1 then clearkey(sc3) : debug_screen_page 3
	end if
end SUB

SUB clearkey(byval k as integer)
	keybd(k) = 0
	if k >= 0 then
		keysteps(k) = 1
	end if
end sub

sub pollingthread(byval unused as threadbs)
	dim as integer a, dummy, buttons

	while endpollthread = 0
		mutexlock keybdmutex

		io_updatekeys keybdstate()
		'set key state for every key
		'highest scancode in fbgfx.bi is &h79, no point overdoing it
		for a = 0 to &h7f
			if keybdstate(a) and 8 then
				'clear the bit that io_updatekeys sets
				keybdstate(a) = keybdstate(a) and 7

				'decide whether to fire a new key event, otherwise the keystate is preserved
				if keysteps(a) = 0 then
					'this is a new keypress
					keybdstate(a) = keybdstate(a) or 6 'key was triggered, new keypress
				end if
				keybdstate(a) = keybdstate(a) or 1 'key is pressed
			else
				keybdstate(a) = keybdstate(a) and 6 'no longer pressed, but was seen
				keysteps(a) = 0 '0 means it's a new press next time
			end if
		next
		io_getmouse dummy, dummy, dummy, buttons
		mouseflags = mouseflags or (buttons and not mouselastflags)
		mouselastflags = buttons
		mutexunlock keybdmutex

		sleep 25
	wend
end sub

SUB putpixel (byval spr as Frame ptr, byval x as integer, byval y as integer, byval c as integer)
	if x < 0 orelse x >= spr->w orelse y < 0 orelse y >= spr->h then
		exit sub
	end if

	FRAMEPIXEL(x, y, spr) = c
end SUB

SUB putpixel (BYVAL x as integer, BYVAL y as integer, BYVAL c as integer, BYVAL p as integer)
	if wrkpage <> p then
		setclip , , , , p
	end if

	if POINT_CLIPPED(x, y) then
		'debug "attempt to putpixel off-screen " & x & "," & y & "=" & c & " on page " & p
		exit sub
	end if

	PAGEPIXEL(x, y, p) = c
end SUB

FUNCTION readpixel (byval spr as Frame ptr, byval x as integer, byval y as integer) as integer
	if x < 0 orelse x >= spr->w orelse y < 0 orelse y >= spr->h then
		exit function
	end if

	return FRAMEPIXEL(x, y, spr)
end FUNCTION

FUNCTION readpixel (BYVAL x as integer, BYVAL y as integer, BYVAL p as integer) as integer
	if wrkpage <> p then
		setclip , , , , p
	end if

	if POINT_CLIPPED(x, y) then
		debug "attempt to readpixel off-screen " & x & "," & y & " on page " & p
		return 0
	end if

	return PAGEPIXEL(x, y, p)
end FUNCTION

SUB drawbox (BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL c as integer, BYVAL p as integer)
	if wrkpage <> p then
		setclip , , , , p
	end if

	if w < 0 then x = x + w + 1: w = -w
	if h < 0 then y = y + h + 1: h = -h

	'clip
	if x + w > clipr then w = (clipr - x) + 1
	if y + h > clipb then h = (clipb - y) + 1
	if x < clipl then w -= (clipl - x) : x = clipl
	if y < clipt then h -= (clipt - y) : y = clipt

	if w <= 0 or h <= 0 then exit sub

	dim sptr as ubyte ptr = vpages(p)->image + (y * vpages(p)->pitch) + x
	if h >= 1 then
		'draw the top
		memset(sptr, c, w)
		sptr += vpages(p)->pitch
	end if
	if h >= 3 then
		'draw the sides
		for i as integer = h - 3 to 0 step -1
			sptr[0] = c
			sptr[w - 1] = c
			sptr += vpages(p)->pitch
		next
	end if
	if h >= 2 then
		'draw the bottom
		memset(sptr, c, w)
	end if
end SUB

SUB rectangle (BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL c as integer, BYVAL p as integer)
	if wrkpage <> p then
		setclip , , , , p
	end if

	if w < 0 then x = x + w + 1: w = -w
	if h < 0 then y = y + h + 1: h = -h

	'clip
	if x + w > clipr then w = (clipr - x) + 1
	if y + h > clipb then h = (clipb - y) + 1
	if x < clipl then w -= (clipl - x) : x = clipl
	if y < clipt then h -= (clipt - y) : y = clipt

	if w <= 0 or h <= 0 then exit sub

	dim sptr as ubyte ptr = vpages(p)->image + (y * vpages(p)->pitch) + x
	while h > 0
		memset(sptr, c, w)
		sptr += vpages(p)->pitch
		h -= 1
	wend
end SUB

SUB fuzzyrect (BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL c as integer, BYVAL p as integer)
	if wrkpage <> p then
		setclip , , , , p
	end if

	if w < 0 then x = x + w + 1: w = -w
	if h < 0 then y = y + h + 1: h = -h

	'clip
	if x + w > clipr then w = (clipr - x) + 1
	if y + h > clipb then h = (clipb - y) + 1
	if x < clipl then w -= (clipl - x) : x = clipl
	if y < clipt then h -= (clipt - y) : y = clipt

	if w <= 0 or h <= 0 then exit sub

	dim sptr as ubyte ptr = vpages(p)->image + (y * vpages(p)->pitch) + x
	while h > 0
		for i as integer = h mod 2 to w-1 step 2
			sptr[i] = c
		next
		h -= 1
		sptr += vpages(p)->pitch
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
  	dim as integer erroradd 	'add to errorTerm for each strip drawn
  	dim as integer errorsub 	'subtract from errorterm when triggered

  	dim as integer i,j
  	dim sptr as ubyte ptr

'Macro to simplify code
#define DRAW_SLICE(a) for i=0 to a-1: *sptr = c: sptr += instep: next

	if wrkpage <> p then
		setclip , , , , p
	end if

	if POINT_CLIPPED(x1, y1) orelse POINT_CLIPPED(x2, y2) then
		debug "drawline: outside clipping"
		exit sub
	end if

  	if y1 > y2 then
  		'swap ends, we only draw downwards
		i=y1: y1=y2: y2=i
		i=x1: x1=x2: x2=i
	end if

	'point to start
	sptr = vpages(p)->image + (y1 * vpages(p)->pitch) + x1

  	xdiff = x2 - x1
  	ydiff = y2 - y1

  	if xDiff < 0 then
  		'right to left
		xdiff = -xdiff
		xdirection = -1
  	else
		xdirection = 1
	end if

	'special case for vertical
  	if xdiff = 0 then
  		instep = vpages(p)->pitch
  		DRAW_SLICE(ydiff+1)
		exit sub
  	end if

	'and for horizontal
  	if ydiff = 0 then
  		instep = xdirection
  		DRAW_SLICE(xdiff+1)
		exit sub
  	end if

  	'and also for pure diagonals
  	if xdiff = ydiff then
  		instep = vpages(p)->pitch + xdirection
  		DRAW_SLICE(ydiff+1)
		exit sub
  	end if

	'now the actual bresenham
  	if xdiff > ydiff then
  		longaxis = xdiff
		shortaxis = ydiff

		instep = xdirection
		outstep = vpages(p)->pitch
  	else
		'other way round, draw vertical slices
		longaxis = ydiff
		shortaxis = xdiff

		instep = vpages(p)->pitch
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

SUB paintat (BYVAL x as integer, BYVAL y as integer, BYVAL c as integer, BYVAL page as integer)
'a floodfill.
	dim tcol as integer
	dim queue as node ptr = null
	dim tail as node ptr = null
	dim as integer w, e		'x coords west and east
	dim i as integer
	dim tnode as node ptr = null

	if wrkpage <> page then
		setclip , , , , page
	end if

	if POINT_CLIPPED(x, y) then exit sub

	tcol = readpixel(x, y, page)	'get target colour

	'prevent infinite loop if you fill with the same colour
	if tcol = c then exit sub

	queue = callocate(sizeof(node))
	queue->x = x
	queue->y = y
	queue->nextnode = null
	tail = queue

	'we only let coordinates within the clip bounds get onto the queue, so there's no need to check them

	do
		if PAGEPIXEL(queue->x, queue->y, page) = tcol then
			PAGEPIXEL(queue->x, queue->y, page) = c
			w = queue->x
			e = queue->x
			'find western limit
			while w > clipl and PAGEPIXEL(w-1, queue->y, page) = tcol
				w -= 1
				PAGEPIXEL(w, queue->y, page) = c
			wend
			'find eastern limit
			while e < clipr and PAGEPIXEL(e+1, queue->y, page) = tcol
				e += 1
				PAGEPIXEL(e, queue->y, page) = c
			wend
			'add bordering nodes
			for i = w to e
				if queue->y > clipt then
					'north
					if PAGEPIXEL(i, queue->y-1, page) = tcol then
						tail->nextnode = callocate(sizeof(node))
						tail = tail->nextnode
						tail->x = i
						tail->y = queue->y-1
						tail->nextnode = null
					end if
				end if
				if queue->y < clipb then
					'south
					if PAGEPIXEL(i, queue->y+1, page) = tcol then
						tail->nextnode = callocate(sizeof(node))
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
'saves a screen page to a file. Doesn't support non-320x200 pages
	dim f as integer
	dim idx as integer
	dim bi as integer
	dim ub as ubyte
	dim sptr as ubyte ptr
	dim scrnbase as ubyte ptr
	dim plane as integer

	if wrkpage <> p then
		setclip , , , , p
	end if

	if NOT fileiswriteable(fil$) then exit sub
	f = freefile
	open fil$ for binary access read write as #f

	'skip to index
	seek #f, (i*64000) + 1 'will this work with write access?

	'modex format, 4 planes
	scrnbase = vpages(p)->image
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
'loads a whole page from a file. Doesn't support non-320x200 pages
	dim f as integer
	dim idx as integer
	dim bi as integer
	dim ub as ubyte
	dim sptr as ubyte ptr
	dim scrnbase as ubyte ptr
	dim plane as integer

	if wrkpage <> p then
		setclip , , , , p
	end if

	if NOT fileisreadable(fil$) then exit sub
	f = freefile
	open fil$ for binary access read as #f

	if lof(f) < (i + 1) * 64000 then
		debug "loadpage: wanted page " & i & "; " & fil$ & " is only " & lof(f) & " bytes"
		clearpage p
		close #f
		exit sub
	end if

	'skip to index
	seek #f, (i*64000) + 1

	'modex format, 4 planes
	scrnbase = vpages(p)->image
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

SUB setwait (BYVAL t as integer, BYVAL flagt as integer = 0)
't is a value in milliseconds which, in the original, is used to set the event
'frequency and is also used to set the wait time, but the resolution of the
'dos timer means that the latter is always truncated to the last multiple of
'55 milliseconds. We won't do this anymore. Try to make the target framerate.
	waittime = bound(waittime + t / 1000, timer + 0.017, timer + t / 667)
	if timer > flagtime then
		flagtime = bound(flagtime + flagt / 1000, timer + 0.017, timer + flagt / 667)
	end if
	if flagt = 0 then
		flagt = t
	end if
	waitset = 1
end SUB

FUNCTION dowait () as integer
'wait until alarm time set in setwait()
'returns true if the flag time has passed (since the last time it was passed)
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
	return timer >= flagtime
end FUNCTION

'FIXME: sprite pitch and so on!
SUB printstr (s as string, BYVAL startx as integer, BYVAL y as integer, BYREF f as Font, BYREF pal as Palette16, BYVAL p as integer)
	if wrkpage <> p then
		setclip , , , , p
	end if

	startx += f.offset.x
	y += f.offset.y

	'check bounds skipped because this is now quite hard to tell (checked in drawohr)

	dim as Frame charframe
	charframe.mask = NULL

	'decide whether to draw a solid background or not
	dim as integer trans_type = -1
	if pal.col(0) > 0  then
		trans_type = 0
	end if

	dim x as integer

	for layer as integer = 0 to 1
		if f.sprite(layer) = NULL then continue for

		x = startx

		'charframe.w = f.sprite(layer)->w
		'charframe.h = f.sprite(layer)->h \ 256
		for ch as integer = 0 to len(s) - 1
			with f.sprite(layer)->chdata(s[ch])
				charframe.image = f.sprite(layer)->spr.image + .offset
				charframe.w = .w
				charframe.h = .h
				charframe.pitch = .w
				drawohr(charframe, @pal, x + .offx, y + .offy, 1, trans_type, p)

				'print one character past the end of the line
				'(I think this is a reasonable approximation)
				if x > clipr then
					continue for, for
				end if
				'note: do not use .w, that's just the width of the sprite
				x += f.w(s[ch])
			end with
		next
	next
end SUB

'the old printstr
SUB printstr (s as string, BYVAL x as integer, BYVAL y as integer, BYVAL p as integer)
	dim fontpal as Palette16

	fontpal.col(0) = textbg
	fontpal.col(1) = textfg

	printstr (s, x, y, fonts(0), fontpal, p)
end SUB

SUB edgeprint (s as string, BYVAL x as integer, BYVAL y as integer, BYVAL c as integer, BYVAL p as integer)
	static fontpal as Palette16

	fontpal.col(0) = 0
	fontpal.col(1) = c
	fontpal.col(2) = uilook(uiOutline)

	'preserve the old behaviour
	textfg = c
	textbg = 0

	printstr (s, x, y, fonts(1), fontpal, p)
END SUB

SUB textcolor (BYVAL f as integer, BYVAL b as integer)
	textfg = f
	textbg = b
end SUB

'TODO/FIXME: need to use sprite_* functions PROPERLY to handle Frame stuff
SUB font_unload (byval font as Font ptr)
	if font = null then exit sub

	'look! polymorphism! definitely not hackery! yeah... look it up sometime.
	sprite_unload cast(Frame ptr ptr, @font->sprite(0))
	sprite_unload cast(Frame ptr ptr, @font->sprite(1))
end SUB

'TODO/FIXME: need to use sprite_* functions to handle Frame stuff
SUB font_create_edged (byval font as Font ptr, byval basefont as Font ptr)
	if basefont = null then
		debug "createedgefont wasn't passed a font!"
		exit sub
	end if
	if basefont->sprite(1) = null then
		debug "createedgefont was passed a blank font!"
		exit sub
	end if

	if font = null then exit sub
		'font = callocate(sizeof(Font))
	font_unload font

	font->sprite(0) = callocate(sizeof(FontLayer))
	font->sprite(1) = basefont->sprite(1)
	font->sprite(1)->spr.refcount += 1

	dim size as integer
	'since you can only WITH one thing at a time
	dim bchr as FontChar ptr
	bchr = @basefont->sprite(1)->chdata(0)

	dim as integer ch

	for ch = 0 to 255
		font->w(ch) = basefont->w(ch)

		with font->sprite(0)->chdata(ch)
			.offset = size
			.offx = bchr->offx - 1
			.offy = bchr->offy - 1
			.w = bchr->w + 2
			.h = bchr->h + 2
			size += .w * .h
		end with
		bchr += 1
	next
			

	with font->sprite(0)->spr
		.w = size  'garbage
		.h = 1
		.pitch = size 'more garbage, not sure whether there's a sensible value
		.refcount = 2  '1  'NOREFC  '?????
		.arrayelem = 1 ' ??????
		.mask = null
		.image = callocate(size)
	end with
	font->h = basefont->h  '+ 2
	font->offset = basefont->offset
	font->cols = basefont->cols + 1


	'dim as ubyte ptr maskp = basefont->sprite(0)->mask
	dim as ubyte ptr sptr
	dim as ubyte ptr srcptr = font->sprite(1)->spr.image
	dim as integer x, y

	for ch = 0 to 255
		with font->sprite(0)->chdata(ch)
			sptr = font->sprite(0)->spr.image + .offset + .w + 1
			for y = 1 to .h - 2
				for x = 1 to .w - 2
					if *srcptr then
						sptr[-.w + 0] = font->cols
						sptr[  0 - 1] = font->cols
						sptr[  0 + 1] = font->cols
						sptr[ .w + 0] = font->cols
					end if
					'if *sptr = 0 then *maskp = 0 else *maskp = &hff
					sptr += 1
					srcptr += 1
					'maskp += 8
				next
				sptr += 2
			next
		end with
	next
end SUB

'TODO/FIXME: need to use sprite_* functions to handle Frame stuff (and some dodgy non-pitch-aware stuff here)
SUB font_create_shadowed (byval font as Font ptr, byval basefont as Font ptr, byval xdrop as integer = 1, byval ydrop as integer = 1)
	if basefont = null then
		debug "createshadowfont wasn't passed a font!"
		exit sub
	end if
	if basefont->sprite(1) = null then
		debug "createshadowfont was passed a blank font!"
		exit sub
	end if

	if font = null then exit sub
	font_unload font

	memcpy(font, basefont, sizeof(Font))

	font->sprite(0) = callocate(sizeof(FontLayer))
	font->sprite(1)->spr.refcount += 1
	font->cols += 1

	'wish I could call sprite_duplicate. A little OO would fix that.
	memcpy(font->sprite(0), font->sprite(1), sizeof(FontLayer))

	for ch as integer = 0 to 255
		with font->sprite(0)->chdata(ch)
			.offx += xdrop
			.offy += ydrop
		end with
	next
			
	with font->sprite(0)->spr	
		.image = allocate(.w * .h)
		memcpy(.image, font->sprite(1)->spr.image, .w * .h)
		if font->sprite(1)->spr.mask then
			.mask = allocate(.w * .h)
			memcpy(.mask, font->sprite(1)->spr.mask, .w * .h)
		end if
		.refcount = 1

		for i as integer = 0 to .w * .h - 1
			if .image[i] then
				.image[i] = font->cols
			end if
		next
	end with
end SUB

'TODO/FIXME: need to use sprite_* functions to handle Frame stuff
sub font_loadold1bit (byval font as Font ptr, byval fontdata as ubyte ptr)
	if font = null then exit sub
	font_unload font

	font->sprite(1) = callocate(sizeof(FontLayer))
	with font->sprite(1)->spr
		.w = 8
		.pitch = 8
		.h = 256 * 8
		.refcount = 1   'NOREFC
		'font->mask = allocate(256 * 8 * 8)
		.mask = null
		.image = allocate(256 * 8 * 8)
	end with
	font->h = 8
	font->offset.x = 0
	font->offset.y = 0
	font->cols = 1

	'dim as ubyte ptr maskp = font->mask
	dim as ubyte ptr sptr = font->sprite(1)->spr.image

	dim as integer ch, x, y
	dim as integer fi 'font index
	dim as integer fstep

	for ch = 0 to 255
		font->w(ch) = 8
		with font->sprite(1)->chdata(ch)
			.w = 8
			.h = 8
			.offset = 64 * ch
		end with

		'find fontdata index, bearing in mind that the data is stored
		'2-bytes at a time in 4-byte integers, due to QB->FB quirks,
		'and fontdata itself is a byte pointer. Because there are
		'always 8 bytes per character, we will always use exactly 4
		'ints, or 16 bytes, making the initial calc pretty simple.
		fi = ch * 16
		'fi = ch * 8	'index to fontdata
		fstep = 1 'used because our indexing is messed up, see above
		for x = 0 to 7
			for y = 0 to 7
				*sptr = (fontdata[fi] shr y) and 1
				'if *sptr = 0 then *maskp = 0 else *maskp = &hff
				sptr += 8
				'maskp += 8
			next
			fi = fi + fstep
			fstep = iif(fstep = 1, 3, 1) 'uneven steps due to 2->4 byte thunk
			sptr += 1 - 8 * 8
			'maskp += 1 - 8 * 8
		next
		sptr += 8 * 8 - 8
		'maskp += 8 * 8 - 8
	next
end SUB

'This sub is for testing purposes only, and will be removed unless this happens to become
'the adopted font format. Includes hardcoded values
'TODO/FIXME: need to use sprite_* functions to handle Frame stuff (plus pitch-awareness)
'FIXME: setclip?
SUB font_loadbmps (byval font as Font ptr, byval fallback as Font ptr = null)
	font_unload font
	if font = null then exit sub

	font->sprite(0) = null
	font->sprite(1) = callocate(sizeof(FontLayer))
	'these are hardcoded
	font->h = 6
	font->offset.x = 0
	font->offset.y = 0
	font->cols = 1

	dim as integer bmpstuff(4)
	dim as ubyte ptr image = allocate(4096)
	dim as ubyte ptr sptr
	dim as integer size = 0
	dim as integer i, x, y
	dim f as string
	dim bchr as FontChar ptr
	bchr = @fallback->sprite(1)->chdata(0)


	for i = 0 to 255
		with font->sprite(1)->chdata(i)
			f = "testfont" & SLASH & i & ".bmp"
			if isfile(f) then
				bmpinfo(f, bmpstuff())

				bitmap2page(master(), f, 2)

				.offset = size
				.offx = 0
				.offy = 0
				.w = bmpstuff(1)
				.h = bmpstuff(2)
				font->w(i) = .w
				size += .w * .h
				image = reallocate(image, size)
				sptr = image + .offset
				for y = 0 to .h - 1
					for x = 0 to .w - 1
						'bitmap2page to blame
						*sptr = vpages(2)->image[y * vpages(2)->pitch + x]
						sptr += 1
					next
				next
			else
				if iif(fallback = null, YES, fallback->sprite(1) = null) then
					debug "font_loadbmps: fallback font not provided"
					deallocate(image)
					exit sub
				end if

				.offset = size
				.offx = bchr->offx
				.offy = bchr->offy
				.w = bchr->w
				.h = bchr->h
				font->w(i) = .w
				size += .w * .h
				image = reallocate(image, size)
				memcpy(image + .offset, fallback->sprite(1)->spr.image + bchr->offset, .w * .h)
			end if
		end with

		bchr += 1
	next

	with font->sprite(1)->spr
		.w = size  'garbage
		.h = 1
		.pitch = size 'more garbage
		.refcount = 1   'NOREFC
		.mask = null
		.image = image
	end with
end SUB

SUB setfont (f() as integer)
	'uncomment to try out a variable width font
	'font_loadold1bit(@fonts(2), cast(ubyte ptr, @f(0)))
	'font_loadbmps(@fonts(0), @fonts(2))
	
	'comment to try out a variable width font
	font_loadold1bit(@fonts(0), cast(ubyte ptr, @f(0)))

	font_create_edged(@fonts(1), @fonts(0))
	'font_create_shadowed(@fonts(1), @fonts(0))
	'more hardcoded stuff
	fonts(1).offset.x = 1
	fonts(1).offset.y = 1
	fonts(1).h += 2
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
		sptr = vpages(wrkpage)->image
		sptr = sptr + (vpages(wrkpage)->pitch * l)
		idx = bsize
		while idx > vpages(wrkpage)->w
		      fput(f, , sptr, vpages(wrkpage)->w)
		      idx -= vpages(wrkpage)->w
		      sptr += vpages(wrkpage)->pitch
		wend
		fput(f, , sptr, idx)
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
		sptr = vpages(wrkpage)->image
		sptr = sptr + (vpages(wrkpage)->pitch * l)
		idx = bsize
		while idx > vpages(wrkpage)->w
		      fget(f, , sptr, vpages(wrkpage)->w)
		      idx -= vpages(wrkpage)->w
		      sptr += vpages(wrkpage)->pitch
		wend
		fget(f, , sptr, idx)
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
			setclip , , , , p
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

FUNCTION loadrecord (buf() as integer, fh as integer, recordsize as integer, record as integer = -1) as integer
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
		'continue, fit in as much as possible
	end if
	dim readbuf(recordsize - 1) as short

	if record <> -1 then
		seek #fh, recordsize * 2 * record + 1
	end if
	if seek(fh) + 2 * recordsize > lof(fh) + 1 then return 0
	get #fh, , readbuf()
	for idx = 0 to small(recordsize - 1, ubound(buf))
		buf(idx) = readbuf(idx)
	next
	loadrecord = 1
END FUNCTION

FUNCTION loadrecord (buf() as integer, filen$, recordsize as integer, record as integer = 0) as integer
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
	if ubound(buf) < recordsize - 1 then
		debug "storerecord: array has only " & ubound(buf) + 1 & " elements, record is " & recordsize & " ints"
		'continue, write as much as possible
	end if

	dim idx as integer
	dim writebuf(recordsize - 1) as short

	if record <> -1 then
		seek #fh, recordsize * 2 * record + 1
	end if
	for idx = 0 to small(recordsize - 1, ubound(buf))
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

SUB unlump (lump$, ulpath$)
	unlumpfile(lump$, "", ulpath$)
end SUB

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

	bufr = callocate(16383)
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

FUNCTION islumpfile (lump$, fmask$) as integer
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

	bufr = callocate(16000)

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
	sDir$ = escape_string(sDir$, """`\$")
	isdir = SHELL("[ -d """ + sDir$ + """ ]") = 0
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

SUB resetsfx ()
	sound_reset
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
		bufr = callocate(size)
		'copy a chunk of file
		fget(fi, , bufr, size)
		fput(fo, , bufr, size)
	else
		bufr = callocate(16000)

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
	'try external first
	if gfx_screenshot(f$, p) = 0 then
		'otherwise save it ourselves
		sprite_export_bmp8(f$, vpages(p), maspal())
	end if
END SUB

SUB sprite_export_bmp8 (f$, fr as Frame Ptr, maspal() as RGBcolor)
'Not sure whether this should be in here or in gfx. Possibly both?
'	bsave f$, 0

		dim header as BITMAPFILEHEADER
		dim info as BITMAPINFOHEADER
		dim argb as RGBQUAD

		dim as integer of, h, i, bfSize, biSizeImage, bfOffBits, biClrUsed
		dim as ubyte ptr s

		biSizeImage = fr->w * fr->h
		bfOffBits = 54 + 1024
		bfSize = bfOffBits + biSizeImage
		biClrUsed = 256

		header.bfType = 19778
		header.bfSize = bfSize
		header.bfReserved1 = 0
		header.bfReserved2 = 0
		header.bfOffBits = bfOffBits

		info.biSize = 40
		info.biWidth = fr->w
		info.biHeight = fr->h
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

		h = fr->h
		s = fr->image + (h - 1) * fr->pitch
		while h > 0
			fput(of, , s, fr->w)
			s -= fr->pitch
			h -= 1
		wend

		close #of
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
	stackbottom = callocate(32768)
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
'This is normally, but not always, called with 320x200 pics
'TODO/FIXME: Haven't fully translated to new Frame vpages yet, because this
'should just wrap around a bmp->frame loader
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

	sbase = vpages(p)->image


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
'TODO/FIXME: Haven't fully translated to new Frame vpages yet, because this
'should just wrap around a bmp->frame loader
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

	sbase = vpages(p)->image + (y * 320) + x

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
'TODO/FIXME: see loadbmp
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
'TODO/FIXME: see loadbmp
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

FUNCTION loadbmppal (f$, pal() as RGBcolor) as integer
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

FUNCTION bmpinfo (f$, dat()) as integer
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
'Set the bounds used by various (not quite all?) video page drawing functions.
'setclip must be called to reset the clip bounds whenever the wrkpage changes, to ensure
'that they are valid (the video page dimensions might differ).
'Aside from tracking which page the clips are for, some legacy code actually uses wrkpage,
'these should be removed.
sub setclip(byval l as integer = 0, byval t as integer = 0, byval r as integer = 9999, byval b as integer = 9999, byval page as integer = -1)
	if page <> -1 then wrkpage = page
	with *vpages(wrkpage)
		clipl = bound(l, 0, .w) '.w valid, prevents any drawing
		clipt = bound(t, 0, .h)
		clipr = bound(r, 0, .w - 1)
		clipb = bound(b, 0, .h - 1)
	end with
end sub

'trans: draw transparently, either using ->mask if available, or otherwise use colour 0 as transparent
sub drawohr(byref spr as frame, byval pal as Palette16 ptr = null, byval x as integer, byval y as integer, byval scale as integer = 1, byval trans as integer = -1, byval page as integer)
	dim as integer i, j
	dim as ubyte ptr maskp, srcp
	dim as ubyte ptr sptr
	dim as integer srclineinc, destlineinc
	dim as integer startx, starty, endx, endy

	if page <> wrkpage then
		setclip , , , , page
	end if

	if scale <> 1 then
		' isn't code duplication convenient?
		sprite_draw @spr, NULL, x, y, scale, trans, page
		exit sub
	end if

	srcp = spr.image

	maskp = spr.mask
	if maskp = 0 then
		maskp = srcp
		'we could add an optimised version for this case, which is the 99% case
	end if

	startx = x
	endx = x + spr.w - 1
	starty = y
	endy = y + spr.h - 1

	srclineinc = 0

	if startx < clipl then
		srclineinc = (clipl - startx)
		srcp += srclineinc
		maskp += srclineinc
		startx = clipl
	end if

	if starty < clipt then
		srcp += (clipt - starty) * spr.pitch
		maskp += (clipt - starty) * spr.pitch
		starty = clipt
	end if

	if endx > clipr then
		srclineinc += endx - clipr
		endx = clipr
	end if

	if endy > clipb then
		endy = clipb
	end if

	if starty > endy or startx > endx then exit sub

	'above we calculate how many bytes of pixels should be skipped at the beginning+end
	'of each source line, now add on number of bytes skipped due to pitch
	srclineinc += spr.pitch - spr.w

	sptr = vpages(page)->image + startx + starty * vpages(page)->pitch
	destlineinc = vpages(page)->pitch - (endx - startx + 1)

	if scale = 1 then
		if pal <> 0 and trans <> 0 then
			for i = starty to endy
				'loop unrolling copied from below, but not nearly as effective
				for j = endx - startx to 3 step -4
					if maskp[0] then sptr[0] = pal->col(srcp[0])
					if maskp[1] then sptr[1] = pal->col(srcp[1])
					if maskp[2] then sptr[2] = pal->col(srcp[2])
					if maskp[3] then sptr[3] = pal->col(srcp[3])
					maskp += 4
					srcp += 4
					sptr += 4
				next
				while j >= 0
					if maskp[0] then sptr[0] = pal->col(srcp[0])
					maskp += 1
					srcp += 1
					sptr += 1
					j -= 1
				wend

				sptr += destlineinc
				maskp += srclineinc
				srcp += srclineinc
			next
		elseif pal <> 0 and trans = 0 then
			for i = starty to endy
				'loop unrolling blindly copied from below
				for j = endx - startx to 3 step -4
					sptr[0] = pal->col(srcp[0])
					sptr[1] = pal->col(srcp[1])
					sptr[2] = pal->col(srcp[2])
					sptr[3] = pal->col(srcp[3])
					srcp += 4
					sptr += 4
				next
				while j >= 0
					sptr[0] = pal->col(srcp[0])
					srcp += 1
					sptr += 1
					j -= 1
				wend

				sptr += destlineinc
				srcp += srclineinc
			next
		elseif trans = 0 then 'and pal = 0
			for i = starty to endy
				memcpy(sptr, srcp, endx - startx + 1)
				srcp += spr.pitch
				sptr += vpages(page)->pitch
			next
		else 'pal = 0 and trans <> 0
			for i = starty to endy
				'a little loop unrolling
				for j = endx - startx to 3 step -4
					'the following line is surprisingly slow
					'*cast(integer ptr, sptr) = (*cast(integer ptr, srcp) and *cast(integer ptr, maskp)) or (*cast(integer ptr, sptr) and not *cast(integer ptr, maskp))
					if maskp[0] then sptr[0] = srcp[0]
					if maskp[1] then sptr[1] = srcp[1]
					if maskp[2] then sptr[2] = srcp[2]
					if maskp[3] then sptr[3] = srcp[3]
					maskp += 4
					srcp += 4
					sptr += 4
				next
				while j >= 0
					if *maskp then *sptr = *srcp
					maskp += 1
					srcp += 1
					sptr += 1
					j -= 1
				wend

				sptr += destlineinc
				maskp += srclineinc
				srcp += srclineinc
			next
		end if
	else
		'see above
	end if

end sub

sub unloadtileset(byref tileset as Frame ptr)
	'tilesets are never cached at the moment
	sprite_unload @tileset
end sub

sub loadtileset(byref tileset as Frame ptr, byval page as integer)
'TODO/FIXME: Haven't fully translated to new Frame vpages yet, because this
'should just load from a file or 320x200 Frame, not a video page
'TODO: cache tilesets like everything else
	if tileset = null then
		tileset = sprite_new(20, 20 * 160)
	end if

	dim as ubyte ptr sptr = tileset->image
	dim as ubyte ptr srcp
	dim tilex, tiley, px, py

	for tiley = 0 to 9
		for tilex = 0 to 15
			srcp = vpages(page)->image + tilex * 20 + tiley * 320 * 20
			for py = 0 to 19
				for px = 0 to 19
					*sptr = *srcp
					sptr += 1
					srcp += 1
				next
				srcp += 320 - 20
			next
		next
	next
end sub

/'
sub grabrect(page as integer, x as integer, y as integer, w as integer, h as integer, ibuf as ubyte ptr, tbuf as ubyte ptr = 0)
'this isn't used anywhere anymore, was used to grab tiles from the tileset videopage before loadtileset
'maybe some possible future use?
'ibuf should be pre-allocated
	dim sptr as ubyte ptr
	dim as integer i, j, px, py, l

	if ibuf = null then exit sub

	sptr = vpages(page)->image

	py = y
	for i = 0 to h-1
		px = x
		for j = 0 to w-1
			l = i * w + j
			'ignore clip rect, but check screen bounds
			if not (px < 0 or px >= vpages(page)->w or py < 0 or py >= vpages(page)->h) then
				ibuf[l] = sptr[(py * vpages(page)->pitch) + px]
				if tbuf then
					if ibuf[l] = 0 then tbuf[l] = &hff else tbuf[l] = 0
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
'/


#DEFINE ID(a,b,c,d) asc(a) SHL 0 + asc(b) SHL 8 + asc(c) SHL 16 + asc(d) SHL 24
function isawav(fi$) as integer
  if not isfile(fi$) then return 0 'duhhhhhh

  dim _RIFF as integer = ID("R","I","F","F") 'these are the "signatures" of a
  dim _WAVE as integer = ID("W","A","V","E") 'wave file. RIFF is the format,
  dim _fmt_ as integer = ID("f","m","t"," ") 'WAVE is the type, and fmt_ and
  dim _data as integer = ID("d","a","t","a") 'data are the chunks
#UNDEF ID

  dim chnk_ID as integer
  dim chnk_size as integer
  dim f as integer = freefile
  open fi$ for binary as #f

  get #f,,chnk_ID
  if chnk_ID <> _RIFF then
		close #f
		return 0 'not even a RIFF file
	end if

  get #f,,chnk_size 'don't care

  get #f,,chnk_ID

  if chnk_ID <> _WAVE then
		close #f
		return 0 'not a WAVE file, pffft
	end if

  'is this good enough? meh, sure.
  close #f
  return 1

end function

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

Function sfxisplaying(BYVAL num) as integer
  return sound_playing(num)
end Function

Function fileisreadable(f$) as integer
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

Function fileiswriteable(f$) as integer
	dim fh as integer
	fh = freefile
	if open (f$ for binary access read write as #fh) = 2 then
		'debug f$ & " unreadable (ignored)"
		return 0 
	end if
	close #fh
	return -1
end Function

FUNCTION getmusictype (file$) as integer

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
'Really? How likely is loading graphics to be time-critical to the microsecond?
type SpriteCache
	key as integer
	p as frame ptr
end type
redim shared sprcache(60) as SpriteCache

' seeks a frame, and removes it from the cache. It does not free it or
' do any sort of memory management, it just removes it from the cache.
private sub sprite_remove_cache(byval spr as frame ptr)
	dim i as integer
	for i = 0 to ubound(sprcache)
		if sprcache(i).p = spr then
			sprcache(i).key = 0
			sprcache(i).p = 0
			exit sub
		end if
	next
end sub

' looks for a key in the cache. Will return the pointer associated with it.
private function sprite_find_cache(byval key as integer) as frame ptr
	dim i as integer
	for i = 0 to ubound(sprcache)
		if sprcache(i).key = key then return sprcache(i).p
	next
	return 0
end function

'Completely empty the sprite cache
sub sprite_empty_cache()
	dim i as integer
	for i = 0 to ubound(sprcache)
		with sprcache(i)
			if .p <> 0 then
				debug "warning: leaked sprite: " & .key & " with " & .p->refcount & " references"
				sprite_unload @.p
			elseif .key <> 0 then
				debug "warning: phantom cached sprite " & .key
			end if
			.key = 0
		end with
	next
end sub

sub sprite_debug_cache()
	debug "==sprcache=="
	for i as integer = 0 to ubound(sprcache)
		with sprcache(i)
			debug i & " " & .key & " " & sprite_describe(.p)
		end with
	next i
end sub

' adds a frame to the cache with a given key.
' will overwrite older sprites with refcounts of 0 or less.
' if the key already exists, will update it to this pointer.
' will also expand the cache if it is full of referenced sprites.
private sub sprite_add_cache(byval key as integer, byval p as frame ptr)
	if p = 0 then exit sub
	dim as integer i, sec = -1

	'the cache counts as a reference, but only to the head element of an array!!
	p->cached = 1
	p->refcount += 1

	for i = 0 to ubound(sprcache)
		with sprcache(i)
			'we don't check whether the key is equal; we can't reliably
			'find that case without checking everything, and it should never happen
			if .key = 0 then
				.key = key
				.p = p
				exit sub
			elseif .p->refcount <= 0 then
				sec = i
			end if
		end with
	next

	if sec > -1 then
		'NOTE: currently, sprites are always freed as soon as all references are unloaded, so this never happens
		'NOTE: we can not call sprite_unload from in here
		'NOTE: the following is no longer correct! sprite_unload has lots of extra logic which can't be skipped
		sprite_freemem(sprcache(sec).p)
		sprcache(sec).key = key
		sprcache(sec).p = p
		exit sub
	end if

	'no room? pah.
	redim preserve sprcache(ubound(sprcache) * 1.3 + 5)
	sprcache(i).key = key
	sprcache(i).p = p	
end sub

function sprite_new(byval w as integer, byval h as integer, byval frames as integer = 1, byval clr as integer = NO, byval wantmask as integer = NO) as Frame ptr
	dim ret as frame ptr
	'this hack was Mike's idea, not mine!
	ret = callocate(sizeof(Frame) * frames)

	'no memory? shucks.
	if ret = 0 then
		debug "Could not create sprite frames, no memory"
		return 0
	end if

	dim as integer i, j
	for i = 0 to frames - 1
		with ret[i]
			'the caller to sprite_new is considered to have a ref to the head; and the head to have a ref to each other elem
			'so set each refcount to 1
			.refcount = 1
			.arraylen = frames
			if i > 0 then .arrayelem = 1
			.w = w
			.h = h
			.pitch = w '+ 10  'test pitch conversion work
			.mask = NULL
			if clr then
				.image = callocate(.pitch * h)
				if wantmask then .mask = callocate(.pitch * h)
			else
				.image = allocate(.pitch * h)
				if wantmask then .mask = allocate(.pitch * h)
			end if

			if .image = 0 or (.mask = 0 and wantmask <> 0) then
				debug "Could not allocate sprite frames, no memory"
				'well, I don't really see the point freeing memory, but who knows...
				sprite_freemem(ret)
				return 0
			end if
		end with
	next
	return ret
end function

'create a frame which is a view onto part of a larger frame
function sprite_new_view(byval spr as Frame ptr, byval x as integer, byval y as integer, byval w as integer, byval h as integer) as Frame ptr
	dim ret as frame ptr = callocate(sizeof(Frame))

	if ret = 0 then
		debug "Could not create sprite view, no memory"
		return 0
	end if

	with *ret
		.w = bound(w, 1, spr->w - x)
		.h = bound(h, 1, spr->h - y)
		.pitch = spr->pitch
		.image = spr->image + .pitch * y + x
		if spr->mask then
			.mask = spr->mask + .pitch * y + x
		end if
		.refcount = 1
		.arraylen = 1 'at the moment not actually used anywhere on sprites with isview = 1
		.isview = 1
		'we point .base at the 'root' frame which really owns these pixel buffer(s)
		if spr->isview then
			.base = spr->base
		else
			.base = spr
		end if
		if .base->refcount <> NOREFC then .base->refcount += 1
	end with
	return ret
end function

' unconditionally frees a sprite from memory. 
' You should never need to call this: use sprite_unload
' Should only be called on the head of an array (and not a view, obv)!
' Warning: not all code calls sprite_freemem to free sprites! Grrr!
private sub sprite_freemem(byval f as frame ptr)
	if f = 0 then exit sub
	if f->arrayelem then debug "can't free arrayelem!": exit sub
	for i as integer = 0 to f->arraylen - 1
		deallocate(f[i].image)
		deallocate(f[i].mask)
	next
	deallocate(f)
end sub

'Public:
' Loads a 4-bit sprite (stored in columns (2/byte)) from one of the .pt? files, with caching.
' It will return a pointer to the first frame, and subsequent frames
' will be immediately after it in memory. (This is a hack, and will probably be removed)
function sprite_load(byval ptno as integer, byval rec as integer) as frame ptr
	dim ret as frame ptr
	dim key as integer = ptno * 1000000 + rec
	
	ret = sprite_find_cache(key)
	
	'AKA  if ret then
	if sprite_is_valid(ret) then
		ret->refcount += 1
		return ret
	end if

	with sprite_sizes(ptno)
		ret = sprite_load(game + ".pt" & ptno, rec, .frames, .size.x, .size.y)
		if ret = 0 then return 0
	end with

	sprite_add_cache(key, ret)
	return ret
end function

' You can use this to load a .pt?-format 4-bit sprite from some non-standard location.
' No code does this. Does not use a cache.
' It will return a pointer to the first frame (of num frames), and subsequent frames
' will be immediately after it in memory. (This is a hack, and will probably be removed)
function sprite_load(byval fi as string, byval rec as integer, byval num as integer, byval wid as integer, byval hei as integer) as frame ptr
	dim ret as frame ptr
	
	'first, we do a bit of math:
	dim frsize as integer = wid * hei / 2
	dim recsize as integer = frsize * num
	
	'make sure the file is real
	if not isfile(fi) then return 0
	
	'now, we can load the sprite
	dim f as integer = freefile
	
	'open() returns 0 for success
	if open(fi for binary as #f) then
		debug "sprites: could not open " & fi
		return 0
	end if
	
	'if we get here, we can assume that all's well, and allocate the memory
	ret = sprite_new(wid, hei, num)
	
	if ret = 0 then
		close #f
		return 0
	end if
	
	'find the right sprite (remember, it's base-1)
	seek #f, recsize * rec + 1
	
	dim i as integer, x as integer, y as integer, z as ubyte
	
	for i = 0 to num - 1
		with ret[i]
			'although it's a four-bit sprite, it IS an 8-bit bitmap.
			
			for x = 0 to wid - 1
				for y = 0 to hei - 1
					'pull up two pixels
					get #f,,z
					
					'the high nybble is the first pixel
					.image[y * wid + x] = (z SHR 4)
					
					y+=1
					
					'and the low nybble is the second one
					.image[y * wid + x] = z AND 15
					
					'it is worth mentioning that sprites are stored in columns, not rows
				next
			next
		end with
	next
	
	close #f

	return ret
end function

'Public:
' Releases a reference to a sprite and nulls the pointer.
' If it is refcounted, decrements the refcount, otherwise it is freed immediately.
' A note on frame arrays: you may pass around pointers to frames in it (call sprite_reference
' on them) and then unload them, but no memory will be freed until the head pointer refcount reaches 0.
' The head element will have 1 extra refcount if the frame array is in the cache. Each of the non-head
' elements also have 1 refcount, indicating that they are 'in use' by the head element,
' but this is just for feel-good book keeping
sub sprite_unload(byval p as frame ptr ptr)
	if p = 0 then exit sub
	if *p = 0 then exit sub
	with **p
		if .refcount <> NOREFC then
			.refcount -= 1
			if .refcount < 0 then debug sprite_describe(*p) & " has refcount " & .refcount
		end if
		'if cached, can free two references at once
		if (.refcount - .cached) <= 0 then
			if .arrayelem then
				'this should not happen, because each arrayelem gets an extra refcount
				debug "arrayelem with refcount = " & .refcount
				exit sub
			end if
			if .isview then
				sprite_unload @.base
				deallocate(*p)
			else
				'current policy is to unload all sprite as soon as they are unused, but this could be changed easily.
				if .cached then sprite_remove_cache(*p)
				for i as integer = 1 to .arraylen - 1
					if (*p)[i].refcount <> 1 then
						debug sprite_describe(*p) & " freed with refcount " & .refcount
					end if
				next
				sprite_freemem(*p)
			end if
		end if
	end with
	*p = 0
end sub

function sprite_describe(byval p as frame ptr) as string
	if p = 0 then return "'(null)'"
	return "'(0x" & hex(p) & ") " & p->arraylen & "x" & p->w & "x" & p->h & " img=0x" & hex(p->image) _
	       & " msk=0x" & hex(p->mask) & " pitch=" & p->pitch & " cached=" & p->cached & " aelem=" _
	       & p->arrayelem & " view=" & p->isview & " base=0x" & hex(p->base) & " refc=" & p->refcount & "'"
end function

'this is mostly just a gimmick
function sprite_is_valid(byval p as frame ptr) as integer
	if p = 0 then return 0
	dim ret = -1
	
	if p->refcount <> NOREFC and p->refcount <= 0 then ret = 0
	
	'this is an arbitrary test, and in theory, could cause a false-negative, but I can't concieve of 100 thousand references to the same sprite.
	if p->refcount > 100000 then ret = 0
	
	if p->w < 0 or p->h < 0 then ret = 0
	if p->pitch < p->w then ret = 0
	
	if p->image = 0 then ret = 0
	
	if p->mask = &hBAADF00D or p->image = &hBAADF00D then ret = 0
	if p->mask = &hFEEEFEEE or p->image = &hFEEEFEEE then ret = 0
	
	if ret = 0 then
		debug "Invalid sprite " & sprite_describe(p)
		'if we get here, we are probably doomed, but this might be a recovery
		sprite_remove_cache(p)
	end if
	return ret
end function

'for a copy you intend to modify. Otherwise use sprite_reference
'note: does not copy frame arrays, only single frames
function sprite_duplicate(byval p as frame ptr, byval clr as integer = 0, byval addmask as integer = 0) as frame ptr
	dim ret as frame ptr, i as integer
	
	if p = 0 then return 0
	
	ret = callocate(sizeof(frame))
	
	if ret = 0 then return 0
	
	ret->w = p->w
	ret->h = p->h
	ret->pitch = p->w
	ret->refcount = 1
	ret->image = 0
	ret->mask = 0
	ret->arraylen = 1

	if p->image then
		if clr = 0 then
			ret->image = allocate(ret->w * ret->h)
			if p->w = p->pitch then
				'a little optimisation (we know ret->w == ret->pitch)
				memcpy(ret->image, p->image, ret->w * ret->h)
			else
				for i = 0 to ret->h - 1
					memcpy(ret->image + i * ret->pitch, p->image + i * p->pitch, ret->w)
				next
			end if
		else
			ret->image = callocate(ret->w * ret->h)
		end if
	end if
	if p->mask then
		if clr = 0 then
			ret->mask = allocate(ret->w * ret->h)
			if p->w = p->pitch then
				'a little optimisation (we know ret->w == ret->pitch)
				memcpy(ret->mask, p->mask, ret->w * ret->h)
			else
				for i = 0 to ret->h - 1
					memcpy(ret->mask + i * ret->pitch, p->mask + i * p->pitch, ret->w)
				next
			end if
		else
			ret->mask = callocate(ret->w * ret->h)
		end if
	elseif addmask then
		if clr = 0 then
			ret->mask = allocate(ret->w * ret->h)
			'we can just copy .image in one go, since we just ensured it's contiguous
			memcpy(ret->mask, ret->image, ret->w * ret->h)
		else
			ret->mask = callocate(ret->w * ret->h)
		end if
	end if
	
	return ret
end function

function sprite_reference(byval p as frame ptr) as frame ptr
	if p = 0 then return 0
	if p->refcount = NOREFC then
		debug "tried to reference a non-refcounted sprite!"
	else
		p->refcount += 1
	end if
	return p
end function

'Public:
' draws a sprite to a page. scale must be greater than or equal to 1. if trans is false, the
' mask will be wholly ignored. Just like drawohr, masks are optional, otherwise use colourkey 0
sub sprite_draw(byval spr as frame ptr, Byval pal as Palette16 ptr, Byval x as integer, Byval y as integer, Byval scale as integer = 1, Byval trans as integer = -1, byval page as integer)
	dim sptr as ubyte ptr
	dim mptr as ubyte ptr
	dim as integer tx, ty
	dim as integer sx, sy, pix, spix

	if spr = 0 then
		debug "trying to draw null sprite"
		exit sub
	end if

	if scale = 1 then
		drawohr *spr, pal, x, y, scale, trans, page
		exit sub
	end if

	if page <> wrkpage then
		setclip , , , , page
	end if

	sptr = vpages(page)->image

	mptr = spr->mask
	if spr->mask = 0 then
		mptr = spr->image
	end if
	
	dim as integer sxfrom, sxto, syfrom, syto
	
	sxfrom = large(clipl, x)
	sxto = small(clipr, x + (spr->w * scale) - 1)
	
	syfrom = large(clipt, y)
	syto = small(clipb, y + (spr->h * scale) - 1)
	
	With *spr
		'ty = syfrom
		if trans <> 0 then
			for ty = syfrom to syto
				'tx = sxfrom
				for tx = sxfrom to sxto
					'figure out where to put the pixel
					pix = (ty * vpages(page)->pitch) + tx
					'and where to get the pixel from
					spix = (((ty-syfrom) \ scale) * .pitch) + ((tx-sxfrom) \ scale)
					
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
					pix = (ty * vpages(page)->pitch) + tx
					'and where to get the pixel from
					spix = (((ty-y) \ scale) * .pitch) + ((tx-x) \ scale)
					
					'check mask
					if mptr[spix] then
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
' Returns a (copy of the) sprite (any bitdepth) in the midst of a given fade out.
' tlength is the desired length of the transition (in any time units you please),
' t is the number of elasped time units. style is the specific transition.
function sprite_dissolved(byval spr as frame ptr, byval tlength as integer, byval t as integer, byval style as integer) as frame ptr
	if t > tlength then return sprite_duplicate(spr, YES)

	'by default, sprites use colourkey transparency instead of masks.
	'We could easily not use a mask here, but by using one, this function can be called on 8-bit graphics
	'too; just in case you ever want to fade out a backdrop or something?
	dim cpy as frame ptr
	cpy = sprite_duplicate(spr, 0, 1)
	if cpy = 0 then return 0
	
	dim as integer i, j, sx, sy, tog

	select case style
		case 0 'scattered pixel dissolve
			randomize 1, 2 ' use the same random seed for each frame (fast PRNG)

			dim cutoff as unsigned integer = 2 ^ 30 * t / (tlength - 0.5)
			'some random randomness
			dim randomness(cpy->w + 15) as unsigned integer
			for i = 0 to cpy->w + 15
				randomness(i) = int(rnd * (2 ^ 30))
			next

			for sy = 0 to cpy->h - 1
				dim mptr as ubyte ptr = @cpy->mask[sy * cpy->pitch]
				dim key as unsigned integer = int(rnd * (2 ^ 30))
				dim shift as integer = int(rnd * 16)
				for sx = 0 to cpy->w - 1
					'What we would ideally want is a new randomness buffer for each line.
					'You can simulate this by xoring with key; however this results in artifacts.
					'So we try a little more mixing.
					'if (randomness(sx) xor key) < cutoff then
					if (randomness(sx + shift) xor key) < cutoff then
						mptr[sx] = 0
					end if
				next
			next
			randomize timer, 3 're-seed random (MT PRNG)

		case 1 'crossfade
			'interesting idea: could maybe replace all this with calls to generalised fuzzyrect
			dim m as integer = cpy->w * cpy->h * t * 2 / tlength
			dim mptr as ubyte ptr
			dim xoroff as integer = 0
			if t > tlength / 2 then
				'after halfway mark: checker whole sprite, then checker the remaining (with tog xor'd 1)
				for sy = 0 to cpy->h - 1
					mptr = cpy->mask + sy * cpy->pitch
					tog = sy and 1
					for sx = 0 to cpy->w - 1
						tog = tog xor 1
						if tog then mptr[sx] = 0
					next
				next
				xoroff = 1
				m = cpy->w * cpy->h * (t - tlength / 2) * 2 / tlength
			end if
			'checker the first m pixels of the sprite
			for sy = 0 to cpy->h - 1
				mptr = cpy->mask + sy * cpy->pitch
				tog = (sy and 1) xor xoroff
				for sx = 0 to cpy->w - 1
					tog = tog xor 1
					if tog then mptr[sx] = 0
					m -= 1
					if m <= 0 then exit for, for
				next
			next
		case 2 'diagonal vanish
			i = cpy->w * t * 2 / tlength
			j = i
			for sy = 0 to i
				j = i - sy
				if sy >= cpy->h then exit for
				for sx = 0 to j
					if sx >= cpy->w then exit for
					cpy->mask[sy * cpy->pitch + sx] = 0
				next
			next
		case 3 'sink into ground
			dim fall as integer = cpy->h * t / tlength
			for sy = cpy->h - 1 to 0 step -1
				if sy < fall then 
					memset(cpy->mask + sy * cpy->pitch, 0, cpy->w)
				else
					memcpy(cpy->image + sy * cpy->pitch, cpy->image + (sy - fall) * cpy->pitch, cpy->w)
					memcpy(cpy->mask + sy * cpy->pitch, cpy->mask + (sy - fall) * cpy->pitch, cpy->w)
				end if
			next
		case 4 'squash
			for i = cpy->h - 1 to 0 step -1
				dim desty as integer = cpy->h * (t / tlength) + i * (1 - t / tlength)
				desty = bound(desty, 0, cpy->h - 1)
				if desty > i then
					memcpy(cpy->image + desty * cpy->pitch, cpy->image + i * cpy->pitch, cpy->w)
					memcpy(cpy->mask + desty * cpy->pitch, cpy->mask + i * cpy->pitch, cpy->w)
					memset(cpy->mask + i * cpy->pitch, 0, cpy->w)
				end if
			next
		case 5 'melt
			'height and meltmap are fixed point, with 8 bit fractional parts
			'(an attempt to speed up this dissolve, which is 10x slower than most of the others!)
			'the current height of each column above the base of the frame
			dim height(-1 to cpy->w) as integer
			dim meltmap(cpy->h - 1) as integer
			
			for i = 0 to cpy->h - 1
				'Gompertz sigmoid function, exp(-exp(-x))
				'this is very close to 1 when k <= -1.5
				'and very close to 0 when k >= 1.5
				'so decreases down to 0 with increasing i (height) and t
				'meltmap(i) = exp(-exp(-7 + 8.5*(t/tlength) + (-cpy->h + i))) * 256
				meltmap(i) = exp(-exp(-8 + 10*(t/tlength) + 6*(i/cpy->h))) * 256
			next

			dim poffset as integer = (cpy->h - 1) * cpy->pitch
			dim destoff as integer

			for sy = cpy->h - 1 to 0 step -1
				for sx = 0 to cpy->w - 1
					destoff = (cpy->h - 1 - (height(sx) shr 8)) * cpy->pitch + sx

					'if sx = 24 then
						'debug sy & " mask=" & cpy->mask[poffset + sx] & " h=" & height(sx)/256 & " dest=" & (destoff\cpy->pitch) & "   " & t/tlength
					'end if

					'potentially destoff = poffset + sx
					dim temp as integer = cpy->mask[poffset + sx]
					cpy->mask[poffset + sx] = 0
					cpy->image[destoff] = cpy->image[poffset + sx]
					cpy->mask[destoff] = temp

					if temp then
						height(sx) += meltmap(height(sx) shr 8)
					else
						'empty spaces melt quicker, for flop down of hanging swords,etc
						'height(sx) += meltmap(height(sx)) * (1 - t/tlength)
						'height(sx) += meltmap((height(sx) shr 8) + 16)
						height(sx) += meltmap(sy)
					end if
				next
				poffset -= cpy->pitch

				'mix together adjacent heights so that hanging pieces don't easily disconnect
				height(-1) = height(0)
				height(cpy->w) = height(cpy->w - 1)
				for sx = (sy mod 3) to cpy->w - 1 step 3
					height(sx) =  height(sx - 1) \ 4 + height(sx) \ 2 + height(sx + 1) \ 4
				next
			next
		case 6 'vapourise
			'vapoury is the bottommost vapourised row
			dim vapoury as integer = (cpy->h - 1) * (t / tlength)
			dim vspeed as integer = large(cpy->h / tlength, 1)
			for sx = 0 to cpy->w - 1
				dim chunklength as integer = rnd * (vspeed + 5)
				for i = -2 to 9999
					if rnd < 0.3 then exit for
				next

				dim fragy as integer = large(vapoury - large(i, 0) - (chunklength - 1), 0)
				'position to copy fragment from
				dim chunkoffset as integer = large(vapoury - (chunklength - 1), 0) * cpy->pitch + sx

				dim poffset as integer = sx
				for sy = 0 to vapoury
					if sy >= fragy and chunklength <> 0 then
						cpy->image[poffset] = cpy->image[chunkoffset]
						cpy->mask[poffset] = cpy->mask[chunkoffset]
						chunkoffset += cpy->pitch
						chunklength -= 1
					else
						cpy->mask[poffset] = 0
					end if
					poffset += cpy->pitch
				next
			next
		case 7 'phase out
			dim fall as integer = 1 + (cpy->h - 2) * (t / tlength)  'range 1 to cpy->h-1
			'blank out top of sprite
			for sy = 0 to fall - 2
				memset(cpy->mask + sy * cpy->pitch, 0, cpy->w)
			next

			for sx = 0 to cpy->w - 1
				dim poffset as integer = sx + fall * cpy->pitch

				'we stretch the two pixels at the vapour-front up some factor
				dim beamc1 as integer = -1
				dim beamc2 as integer = -1
				if cpy->mask[poffset] then beamc1 = cpy->image[poffset]
				if cpy->mask[poffset - cpy->pitch] then beamc2 = cpy->image[poffset - cpy->pitch]

				if beamc1 = -1 then continue for
				for sy = fall to large(fall - 10, 0) step -1
					cpy->image[poffset] = beamc1
					cpy->mask[poffset] = 1
					poffset -= cpy->pitch
				next
				if beamc2 = -1 then continue for
				for sy = sy to large(sy - 10, 0) step -1
					cpy->image[poffset] = beamc2
					cpy->mask[poffset] = 1
					poffset -= cpy->pitch
				next
			next
	end select

	return cpy
end function

'Used by sprite_flip_horiz and sprite_flip_vert
private sub flip_image(byval pixels as ubyte ptr, byval d1len as integer, byval d1stride as integer, byval d2len as integer, byval d2stride as integer)
	for x1 as integer = 0 to d1len - 1
		dim as ubyte ptr pixelp = pixels + x1 * d1stride
		for offset as integer = d2len - 1 to 0 step -2 * d2stride
			dim as ubyte temp = pixelp[0]
			pixelp[0] = pixelp[offset]
			pixelp[offset] = temp
			pixelp += d2stride
		next
	next
end sub

'Public:
' flips a sprite horizontally. In place: you are only allowed to do this on sprites with no other references
sub sprite_flip_horiz(byval spr as frame ptr)
	if spr = 0 then exit sub
	
	if spr->refcount > 1 then
		debug "illegal hflip on " & sprite_describe(spr)
		exit sub
	end if

	flip_image(spr->image, spr->h, spr->pitch, spr->w, 1)
	if spr->mask then
		flip_image(spr->mask, spr->h, spr->pitch, spr->w, 1)
	end if
end sub

'Public:
' returns a copy of the sprite flipped vertically. See sprite_flip_horiz for documentation
sub sprite_flip_vert(byval spr as frame ptr)
	if spr = 0 then exit sub
	
	if spr->refcount > 1 then
		debug "illegal vflip on " & sprite_describe(spr)
		exit sub
	end if

	flip_image(spr->image, spr->w, 1, spr->h, spr->pitch)
	if spr->mask then
		flip_image(spr->mask, spr->w, 1, spr->h, spr->pitch)
	end if
end sub

'Note that we clear masks to transparent! I'm not sure if this is best (not currently used anywhere), but notice that
'sprite_duplicate with clr=1 does the same
sub sprite_clear(byval spr as frame ptr)
	if spr->image then
		if spr->w = spr->pitch then
			memset(spr->image, 0, spr->w * spr->h)
		else
			for i as integer = 0 to spr->h - 1
				memset(spr->image + i * spr->pitch, 0, spr->w)
			next
		end if
	end if
	if spr->mask then
		if spr->w = spr->pitch then
			memset(spr->mask, 0, spr->w * spr->h)
		else
			for i as integer = 0 to spr->h - 1
				memset(spr->mask + i * spr->pitch, 0, spr->w)
			next
		end if
	end if
end sub

'Warning: this code is rotting; don't assume ->mask is used, etc. Anyway the whole thing should be replaced with a memmove call or two.
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
'Unlike the sprite cache, palettes aren't uncached when they hit 0 references
sub Palette16_empty_cache()
	dim i as integer
	for i = 0 to ubound(palcache)
		with palcache(i)
			if .p <> 0 then
				'debug "warning: leaked palette: " & .s & " with " & .p->refcount & " references"
				Palette16_delete(@.p)
			'elseif .s <> "" then
				'debug "warning: phantom cached palette " & .s
			end if
			.s = ""
		end with
	next
end sub

function Palette16_find_cache(byval s as string) as Palette16Cache ptr
	dim i as integer
	for i = 0 to ubound(palcache)
		if palcache(i).s = s then return @palcache(i)
	next
	return NULL
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

function palette16_load(byval num as integer, byval autotype as integer = 0, byval spr as integer = 0) as palette16 ptr
	dim as Palette16 ptr ret = palette16_load(game + ".pal", num, autotype, spr)
	if ret = 0 then
		if num >= 0 then
			' Only bother to warn if a specific palette failed to load.
			' Avoids debug noise when default palette load fails because of a non-existant defpal file
			debug "failed to load palette " & num
		end if
	end if
	return ret
end function

function palette16_load(byval fil as string, byval num as integer, byval autotype as integer = 0, byval spr as integer = 0) as palette16 ptr
	dim f as integer, ret as palette16 ptr
	dim hashstring as string
	dim cache as Palette16Cache ptr
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
	cache = palette16_find_cache(hashstring)
	
	if cache <> 0 then
		cache->p->refcount += 1
		return cache->p
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

'update a .pal-loaded palette even while in use elsewhere. Notice that sprites don't have anything like this.
sub Palette16_update_cache(fil as string, byval num as integer)
	dim oldpal as Palette16 ptr
	dim hashstring as string
	dim cache as Palette16Cache ptr

	hashstring = trimpath(fil) & "#" & num & ":0"
	cache = Palette16_find_cache(hashstring)

	if cache then
		oldpal = cache->p

		'force a reload, creating a temporary new palette
		cache->s = ""
		cache->p = NULL
		palette16_load(num)
		cache = Palette16_find_cache(hashstring)

		'copy to old palette structure
		dim as integer oldrefcount = oldpal->refcount
		memcpy(oldpal, cache->p, sizeof(Palette16))
		oldpal->refcount = oldrefcount
		'this sub is silly
		Palette16_delete(@cache->p)
		cache->p = oldpal
	end if
end sub
