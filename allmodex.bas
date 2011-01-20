'The OHRRPGCE graphics, audio and user input library!
'Please read LICENSE.txt for GNU GPL license details and disclaimer of liability

#include "compat.bi"
#include "crt/limits.bi"
#include "common.bi"
#include "allmodex.bi"
#include "gfx.bi"
#include "music.bi"
#include "util.bi"
#include "const.bi"
#include "uiconst.bi"

#ifdef IS_GAME
declare sub exitprogram (byval needfade as integer, byval errorout as integer = NO)
#endif

option explicit


'Note: While non-refcounted frames work (at last check), it's not used anywhere, and you most probably do not need it
'NOREFC is also used to indicate uncached Palette16's. Note Palette16's are NOT refcounted in the way as frames
const NOREFC = -1234
const FREEDREFC = -4321

type node 	'only used for floodfill
	x as integer
	y as integer
	nextnode as node ptr
end type

declare sub drawohr(byval src as Frame ptr, byval dest as Frame ptr, byval pal as Palette16 ptr = null, byval x as integer, byval y as integer, byval trans as integer = -1)
declare sub grabrect(page as integer, x as integer, y as integer, w as integer, h as integer, ibuf as ubyte ptr, tbuf as ubyte ptr = 0)
declare function write_bmp_header(f as string, byval w as integer, byval h as integer, byval bitdepth as integer) as integer
declare sub loadbmp24(byval bf as integer, byval fr as Frame ptr, pal() as RGBcolor)
declare sub loadbmp8(byval bf as integer, byval fr as Frame ptr)
declare sub loadbmp4(byval bf as integer, byval fr as Frame ptr)
declare sub loadbmprle4(byval bf as integer, byval fr as Frame ptr)

declare sub snapshot_check

declare function calcblock(tmap as TileMap, byval x as integer, byval y as integer, byval overheadmode as integer, pmapptr as TileMap ptr) as integer

declare sub font_unload(byval font as Font ptr)

'slight hackery to get more versatile read function
declare function fget alias "fb_FileGet" ( byval fnum as integer, byval pos as integer = 0, byval dst as any ptr, byval bytes as uinteger ) as integer
declare function fput alias "fb_FilePut" ( byval fnum as integer, byval pos as integer = 0, byval src as any ptr, byval bytes as uinteger ) as integer


declare sub pollingthread(byval as any ptr)

'global
dim vpages() as Frame ptr
dim vpagesp as Frame ptr ptr  'points to vpages(0) for debugging: fbc outputs typeless debugging symbol

'Convert scancodes to text; Enter does not insert newline!
'This array is a global instead of an internal detail because it's used by charpicker and the font editor
'to work out key mapping for the extended characters. Would be nice if it weren't needed.
'key2text(0,*): no modifiers
'key2text(1,*): shift
'key2text(2,*): alt
'key2text(3,*): alt+shift
dim key2text(3,53) as string*1 => { _
	{"", "", "1","2","3","4","5","6","7","8","9","0","-","=","","","q","w","e","r","t","y","u","i","o","p","[","]","","","a","s","d","f","g","h","j","k","l",";","'","`","","\","z","x","c","v","b","n","m",",",".","/"}, _
	{"", "", "!","@","#","$","%","^","&","*","(",")","_","+","","","Q","W","E","R","T","Y","U","I","O","P","{","}","","","A","S","D","F","G","H","J","K","L",":","""","~","","|","Z","X","C","V","B","N","M","<",">","?"}, _
	{"", "", !"\130",!"\131",!"\132",!"\133",!"\134",!"\135",!"\136",!"\137",!"\138",!"\139",!"\140",!"\141","","",!"\142",!"\143",!"\144",!"\145",!"\146",!"\147",!"\148",!"\149",!"\150",!"\151",!"\152",!"\153","","",!"\154",!"\155",!"\156",!"\157",!"\158",!"\159",!"\160",!"\161",!"\162",!"\163",!"\164",!"\165","",!"\166",!"\167",!"\168",!"\169",!"\170",!"\171",!"\172",!"\173",!"\174",!"\175",!"\176"}, _
	{"", "", !"\177",!"\178",!"\179",!"\180",!"\181",!"\182",!"\183",!"\184",!"\185",!"\186",!"\187",!"\188","","",!"\189",!"\190",!"\191",!"\192",!"\193",!"\194",!"\195",!"\196",!"\197",!"\198",!"\199",!"\200","","",!"\201",!"\202",!"\203",!"\204",!"\205",!"\206",!"\207",!"\208",!"\209",!"\210",!"\211",!"\212","",!"\213",!"\214",!"\215",!"\216",!"\217",!"\218",!"\219",!"\220",!"\221",!"\222",!"\223"} _
}

'module shared
dim shared wrkpage as integer  'used by some legacy modex functions. Usually points at clippedframe
dim shared clippedframe as Frame ptr  'used to track which Frame the clips are set for

'temporary exploratory variable resolution stuff
dim shared windowsize as XYPair = (320, 200)
dim shared variablerez as integer = NO
dim shared minwinsize as XYPair
dim shared forcevispageresize as integer = NO  'for mischief!

'storeset/loadset stuff
dim shared bptr as integer ptr	' buffer
dim shared bsize as integer  'record size, in BYTES
dim shared bpage as integer  'Ye Olde Garbage

dim shared bordertile as integer

dim shared anim1 as integer
dim shared anim2 as integer

dim shared waittime as double
dim shared flagtime as double = 0.0
dim shared waitset as integer

dim shared keybd(-1 to 127) as integer  'keyval array
dim shared keysteps(127) as integer
dim shared keyrepeatwait as integer = 8
dim shared keyrepeatrate as integer = 1
dim shared diagonalhack as integer

dim shared closerequest as integer = 0

dim shared keybdmutex as any ptr  'controls access to keybdstate(), mouseflags, mouselastflags, and various backend functions
dim shared keybdthread as any ptr   'id of the polling thread
dim shared endpollthread as integer  'signal the polling thread to quit
dim shared keybdstate(127) as integer  '"real"time keyboard array
dim shared mouseflags as integer
dim shared mouselastflags as integer

'State saved from one readmouse call to the next
dim shared mouse_clickstart as XYPair
dim shared mouse_dragmask as integer

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

MAKETYPE_DoubleList(SpriteCacheEntry)
MAKETYPE_DListItem(SpriteCacheEntry)
'WARNING: don't add strings to this
type SpriteCacheEntry
	'cachelist used only if object is a member of sprcacheB
	cacheB as DListItem(SpriteCacheEntry)
	hashed as HashedItem
	p as frame ptr
	cost as integer
	Bcached as integer
end type

dim shared sprcache as HashTable
dim shared sprcacheB as DoubleList(SpriteCacheEntry)
dim shared sprcacheB_used as integer  'number of slots full
'dim shared as integer cachehit, cachemiss

dim shared mouse_grab_requested as integer = 0
dim shared mouse_grab_overridden as integer = 0
dim shared remember_mouse_grab(3) as integer = {-1, -1, -1, -1}

dim shared remember_title as string

sub modex_init()
	'initialise software gfx library

	redim vpages(3)
	vpagesp = @vpages(0)
	for i as integer = 0 to 3
		vpages(i) = frame_new(320, 200, , YES)
	next
	'other vpages slots are for temporary pages

	setclip , , , , 0

	hash_construct(sprcache, offsetof(SpriteCacheEntry, hashed))
	dlist_construct(sprcacheB.generic, offsetof(SpriteCacheEntry, cacheB))
	sprcacheB_used = 0
end sub

sub setmodex()
	modex_init()

	'initialise graphics and io

	gfx_backend_init(@post_terminate_signal, "FB_PROGRAM_ICON")

	'init vars
	for i as integer = 0 to 127
		keybd(i) = 0
		keybdstate(i) = 0
 		keysteps(i) = 0
	next
	endpollthread = 0
	mouselastflags = 0
	mouseflags = 0

	keybdmutex = mutexcreate
	if wantpollingthread then
		keybdthread = threadcreate(@pollingthread)
	end if

	io_init()
	'mouserect(-1,-1,-1,-1)

	fpstime = TIMER
	fpsframes = 0
	fpsstring = ""
end sub

sub modex_quit()
	'clean up software gfx library

	for i as integer = 0 to ubound(vpages)
		frame_unload(@vpages(i))
	next
	for i as integer = 0 to ubound(fonts)
		font_unload(@fonts(i))
	next

	hash_destruct(sprcache)
	'debug "cachehit = " & cachehit & " mis == " & cachemiss

	releasestack
end sub

sub restoremode()
	'clean up io stuff
	if keybdthread then
		endpollthread = 1
		threadwait keybdthread
		keybdthread = 0
	end if
	mutexdestroy keybdmutex

	gfx_close()

        modex_quit
end sub

SUB settemporarywindowtitle (title as string)
	'just like setwindowtitle but does not memorize the title
	mutexlock keybdmutex
	gfx_windowtitle(title)
	mutexunlock keybdmutex
END SUB

SUB setwindowtitle (title as string)
	remember_title = title
	mutexlock keybdmutex
	gfx_windowtitle(title)
	mutexunlock keybdmutex
END SUB

SUB freepage (byval page as integer)
	if page < 0 orelse page > ubound(vpages) orelse vpages(page) = NULL then
		debug "Tried to free unallocated/invalid page " & page
		exit sub
	end if

	if page = wrkpage then wrkpage = 0 'no setclip call: legacy wrkpage code doesn't even use clips anyway
	frame_unload(@vpages(page))
END SUB

FUNCTION registerpage (byval spr as Frame ptr) as integer
	if spr->refcount <> NOREFC then	spr->refcount += 1
	for i as integer = 0 to ubound(vpages)
		if vpages(i) = NULL then
			vpages(i) = spr
			return i
		end if
	next

	redim preserve vpages(ubound(vpages) + 1)
	vpagesp = @vpages(0)
	vpages(ubound(vpages)) = spr
	return ubound(vpages)
END FUNCTION

FUNCTION allocatepage(BYVAL w as integer = 320, BYVAL h as integer = 200) as integer
	dim fr as Frame ptr = frame_new(w, h, , YES)

	dim ret as integer = registerpage(fr)
	frame_unload(@fr) 'we're not hanging onto it, vpages() is
	
	return ret
END FUNCTION

'creates a copy of a page, registering it (must be freed)
FUNCTION duplicatepage (BYVAL page as integer) as integer
	dim fr as Frame ptr = frame_duplicate(vpages(page))
	dim ret as integer = registerpage(fr)
	frame_unload(@fr) 'we're not hanging onto it, vpages() is
	return ret
END FUNCTION

'copy page1 to page2
'should copying to a page of different size resize that page?
SUB copypage (BYVAL page1 as integer, BYVAL page2 as integer)
	'if vpages(page1)->w <> vpages(page2)->w or vpages(page1)->h <> vpages(page2)->h then
	'	debug "warning, copied to page of unequal size"
	'end if
	frame_draw vpages(page1), , 0, 0, , NO, vpages(page2)
end sub

SUB clearpage (BYVAL page as integer, BYVAL colour as integer = -1)
	if colour = -1 then colour = uilook(uiBackground)
	frame_clear vpages(page), colour
end SUB

'TEMPORARY
'resizes a page to match the 'window size' (which is a fake, currently) - if so, the page is erased
'returns whether the page size was changed
FUNCTION updatepagesize (BYVAL page as integer) as integer
	gfx_getresize(windowsize)
	if vpages(page)->w = windowsize.w and vpages(page)->h = windowsize.h then return NO
	frame_unload @vpages(page)
	vpages(page) = frame_new(windowsize.w, windowsize.h, , YES)
	return YES
end FUNCTION

'TEMPORARY
SUB unlockresolution (BYVAL min_w as integer = -1, BYVAL min_h as integer = -1)
	variablerez = YES
	minwinsize.w = iif(min_w = -1, 320, min_w)
	minwinsize.h = iif(min_h = -1, 200, min_h)
	windowsize.w = large(windowsize.w, minwinsize.w)
	windowsize.h = large(windowsize.h, minwinsize.h)
end SUB

'TEMPORARY
SUB setresolution (BYVAL w as integer, BYVAL h as integer)
	windowsize.w = large(w, minwinsize.w)
	windowsize.h = large(h, minwinsize.h)
end SUB

'TEMPORARY
SUB resetresolution ()
	variablerez = NO
	windowsize = Type(320, 200)
	for i as integer = 0 to ubound(vpages)
		if vpages(i) then updatepagesize i
	next
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

	'the fb backend may freeze up if it collides with the polling thread
	mutexlock keybdmutex
	if updatepal then
		gfx_setpal(@intpal(0))
		updatepal = 0
	end if
	with *vpages(page)
		if .w = windowsize.w and .h = windowsize.h then
			gfx_showpage(.image, .w, .h)
		else
			dim tpage as integer
			tpage = allocatepage(windowsize.w, windowsize.h)
			frame_draw vpages(page), NULL, 0, 0, 1, 0, tpage
			gfx_showpage(vpages(tpage)->image, windowsize.w, windowsize.h)
			freepage tpage
		end if
	end with
	mutexunlock keybdmutex

	'for having fun
	if forcevispageresize then updatepagesize page
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
		gfx_setpal(@intpal(0))
		mutexunlock keybdmutex
		updatepal = 0
	end if

	for i = 1 to 32
		setwait 17 ' aim to complete fade in 550ms
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
		gfx_setpal(@intpal(0))
		mutexunlock keybdmutex
		dowait
	next
	'Make sure the palette gets set on the final pass

	'This function was probably called in the middle of timed loop, call
	'setwait to avoid "dowait without setwait" warnings
	setwait 0
end SUB

SUB fadetopal (pal() as RGBcolor)
	dim i as integer
	dim j as integer
	dim diff as integer

	if updatepal then
		mutexlock keybdmutex
		gfx_setpal(@intpal(0))
		mutexunlock keybdmutex
		updatepal = 0
	end if

	for i = 1 to 32
		setwait 17 ' aim to complete fade in 550ms
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
		gfx_setpal(@intpal(0))
		mutexunlock keybdmutex
		dowait
	next

	'This function was probably called in the middle of timed loop, call
	'setwait to avoid "dowait without setwait" warnings
	setwait 0
end SUB

#define POINT_CLIPPED(x, y) ((x) < clipl orelse (x) > clipr orelse (y) < clipt orelse (y) > clipb)

#define PAGEPIXEL(x, y, p) vpages(p)->image[vpages(p)->pitch * (y) + (x)]
#define FRAMEPIXEL(x, y, fr) fr->image[fr->pitch * (y) + (x)]

FUNCTION readblock (map as TileMap, BYVAL x as integer, BYVAL y as integer) as integer
	if x < 0 OR x >= map.wide OR y < 0 OR y >= map.high then
		debug "illegal readblock call " & x & " " & y
		exit function
	end if
	return map.data[x + y * map.wide]
END FUNCTION

SUB writeblock (map as TileMap, BYVAL x as integer, BYVAL y as integer, BYVAL v as integer)
	if x < 0 OR x >= map.wide OR y < 0 OR y >= map.high then
		debug "illegal writeblock call " & x & " " & y
		exit sub
	end if
	map.data[x + y * map.wide] = v
END SUB

SUB drawmap (tmap as TileMap, BYVAL x as integer, BYVAL y as integer, BYVAL tileset as TilesetData ptr, BYVAL p as integer, BYVAL trans as integer = 0, BYVAL overheadmode as integer = 0, BYVAL pmapptr as TileMap ptr = NULL, BYVAL ystart as integer = 0, BYVAL yheight as integer = -1)
	'overrides setanim
	anim1 = tileset->tastuf(0) + tileset->anim(0).cycle
	anim2 = tileset->tastuf(20) + tileset->anim(1).cycle
	drawmap tmap, x, y, tileset->spr, p, trans, overheadmode, pmapptr, ystart, yheight
END SUB

SUB drawmap (tmap as TileMap, BYVAL x as integer, BYVAL y as integer, BYVAL tilesetsprite as Frame ptr, BYVAL p as integer, BYVAL trans as integer = 0, BYVAL overheadmode as integer = 0, BYVAL pmapptr as TileMap ptr = NULL, BYVAL ystart as integer = 0, BYVAL yheight as integer = -1, BYVAL largetileset as integer = NO)
'ystart is the distance from the top to start drawing, yheight the number of lines. yheight=-1 indicates extend to bottom of screen
'There are no options in the X direction because they've never been used, and I don't forsee them being (can use Frames or slices instead)
	dim mapview as Frame ptr
	mapview = frame_new_view(vpages(p), 0, ystart, vpages(p)->w, iif(yheight = -1, vpages(p)->h, yheight))
	drawmap tmap, x, y, tilesetsprite, mapview, trans, overheadmode, pmapptr, largetileset
	frame_unload @mapview
END SUB

SUB drawmap (tmap as TileMap, BYVAL x as integer, BYVAL y as integer, BYVAL tilesetsprite as Frame ptr, BYVAL dest as Frame ptr, BYVAL trans as integer = 0, BYVAL overheadmode as integer = 0, BYVAL pmapptr as TileMap ptr = NULL, BYVAL largetileset as integer = NO)
'This version of drawmap paints over the entire dest Frame given to it
'overheadmode = 0 : draw all tiles normally
'overheadmode = 1 : draw non overhead tiles only (to avoid double draw)
'overheadmode = 2 : draw overhead tiles only
'largetileset : A hack which disables tile animation, instead using tilesets with 256 tiles

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

	if clippedframe <> dest then
		setclip , , , , dest
	end if

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

	tileframe.w = 20
	tileframe.h = 20
	tileframe.pitch = 20

	ty = yoff
	while ty < dest->h
		tx = xoff
		xpos = xstart
		while tx < dest->w
			todraw = calcblock(tmap, xpos, ypos, overheadmode, pmapptr)
			if (largetileset = NO ANDALSO todraw >= 160) then
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
				drawohr(@tileframe, dest, , tx, ty, trans)
			end if

			tx = tx + 20
			xpos = xpos + 1
		wend
		ty = ty + 20
		ypos = ypos + 1
	wend
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

FUNCTION palette16_new_from_buffer(pal() as integer, BYVAL po as integer) as Palette16 ptr
	dim ret as Palette16 ptr
	dim word as integer
	ret = allocate(sizeof(Palette16))

	for i as integer = 0 to 15
		'palettes are interleaved like everything else
		word = pal((po + i) \ 2)	' get color from palette
		if (po + i) mod 2 = 1 then
			ret->col(i) = (word and &hff00) shr 8
		else
			ret->col(i) = word and &hff
		end if
	next
	return ret
END FUNCTION

FUNCTION frame_new_from_buffer(pic() as integer, BYVAL picoff as integer) as Frame ptr
	dim sw as integer
	dim sh as integer
	dim hspr as frame ptr
	dim dspr as ubyte ptr
	dim nib as integer
	dim i as integer
	dim spix as integer
	dim row as integer

	sw = pic(picoff)
	sh = pic(picoff+1)
	picoff = picoff + 2

	hspr = frame_new(sw, sh)
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
		*dspr = spix				' set image pixel
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
	return hspr
END FUNCTION

SUB drawspritex (pic() as integer, BYVAL picoff as integer, pal() as integer, BYVAL po as integer, BYVAL x as integer, BYVAL y as integer, BYVAL page as integer, byval scale as integer, byval trans as integer = -1)
'draw sprite scaled, used for drawsprite(x1), bigsprite(x2) and hugesprite(x4)
	if clippedframe <> vpages(page) then
		setclip , , , , page
	end if
	
	'convert the buffer into a Frame
	dim hspr as frame ptr
	dim hpal as palette16 ptr
	hspr = frame_new_from_buffer(pic(), picoff)
	hpal = palette16_new_from_buffer(pal(), po)
	
	'now draw the image
	frame_draw(hspr, hpal, x, y, scale, trans, page)
	'what a waste
	frame_unload(@hspr)
	deallocate(hpal)
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

	if clippedframe <> vpages(page) then
		setclip , , , , page
	end if

	sw = pic(picoff)
	sh = pic(picoff+1)
	picoff = picoff + 2

	hspr = frame_new(sw, sh)
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
	frame_draw(hspr, NULL, x, y, , trans, page)

	frame_unload(@hspr)
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

	if clippedframe <> vpages(page) then
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

	if clippedframe <> vpages(page) then
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
'This reads a rectangular region of a screen page into sprite buffer array pic() at picoff
'It assumes that all the pixels it encounters will be colors 0-15 of the master palette
'even though those colors will certainly be mapped to some other 16 color palette when drawn
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
			sptr += vpages(page)->pitch
			nyb += 1
			nyb = nyb and &h03
		next
		sbase = sbase + 1 'next col
	next

END SUB

FUNCTION keyval (BYVAL a as integer, BYVAL rwait as integer = 0, BYVAL rrate as integer = 0) as integer
'except for special keys (like -1), each key reports 3 bits:
'
'bit 0: key was down at the last setkeys call
'bit 1: keypress event (either new keypress, or key-repeat) during last setkey-setkey interval
'bit 2: new keypress during last setkey-setkey interval
'
'Note: Alt/Ctrl keys may behave strangely with gfx_fb (and old gfx_directx):
'You won't see Left/Right keypresses even when scAlt/scCtrl is pressed, so do not
'check "keyval(scLeftAlt) > 0 OR keyval(scRightAlt) > 0" instead of "keyval(scAlt) > 0"

	DIM result as integer = keybd(a)
	IF a >= 0 THEN
		IF rwait = 0 THEN rwait = keyrepeatwait
		IF rrate = 0 THEN rrate = keyrepeatrate

		'awful hack to avoid arrow keys firely alternatively with rrate > 1
		DIM arrowkey as integer = 0
		IF a = scLeft OR a = scRight OR a = scUp OR a = scDown THEN arrowkey = -1
		IF arrowkey AND diagonalhack <> -1 THEN RETURN (result AND 5) OR (diagonalhack AND keybd(a) > 0)
		IF keysteps(a) >= rwait THEN
			'Don't fire repeat presses for special toggle keys (note: these aren't actually
			'toggle keys in all backends, eg. gfx_fb)
			IF a <> scNumlock AND a <> scCapslock AND a <> scScrolllock THEN
				IF ((keysteps(a) - rwait - 1) MOD rrate = 0) THEN result OR= 2
			END IF
			IF arrowkey THEN diagonalhack = result AND 2
		END IF
	END IF
	RETURN result
end FUNCTION

FUNCTION getinputtext () as string
	dim ret as string
	dim shift as integer = 0

	if keyval(scCtrl) > 0 then return ""

	if keyval(scRightShift) > 0 or keyval(scLeftShift) > 0 then shift = 1

	'ALT to enter extended characters (128 and up)
	if keyval(scAlt) then shift += 2

	for i as integer = 0 to 53
		dim effective_shift as integer = shift
		if shift <= 1 andalso keyval(scCapsLock) > 0 then
			select case i
				case scQ to scP, scA to scL, scZ to scM
					effective_shift xor= 1
			end select
		end if
		if keyval(i) > 1 then
			ret &= key2text(effective_shift, i)
		end if
	next i

	'Space missing from key2text
	if keyval(scSpace) > 1 then ret &= " "

	return ret
end FUNCTION

'one of waitforanykey and getkey must go
FUNCTION waitforanykey (modkeys=-1) as integer
	dim i as integer
	setkeys
	do
		setwait 100
		io_pollkeyevents()
		setkeys
		for i = 1 to &h7f
			if not modkeys and (i=29 or i=56 or i=42 or i=54) then continue for  'what's the reason for this again? If I knew, I'd delete this function
			if keyval(i) > 1 then return i
		next i
		dowait
	loop
	return 0
end FUNCTION

'one of waitforanykey and getkey must go
FUNCTION getkey () as integer
	dim i as integer, key as integer
	dim as integer joybutton = 0, joyx = 0, joyy = 0, sleepjoy = 3
	key = 0

	setkeys
	do
		setwait 50
		io_pollkeyevents()
		setkeys
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
		dowait
	loop while key = 0
	'prevent crazy fast pseudo-keyrepeat
	sleep 25

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
	io_keybits(@keybd(0))
	mutexunlock keybdmutex

	'DELETEME: This is a temporary fix for gfx_directx not knowing about scShift
	'(or any other of the new scancodes, but none of the rest matter much (maybe
	'scPause) since there are no games that use them).
	if (keybd(scLeftShift) or keybd(scRightShift)) <> keybd(scShift) then
		keybd(scShift) = keybd(scLeftShift) or keybd(scRightShift)
	end if

	for a = 0 to &h7f
		if (keybd(a) and 4) or (keybd(a) and 1) = 0 then  'I am also confused
			keysteps(a) = 0
		end if
		if keybd(a) and 1 then
			keysteps(a) += 1
		end if
	next

	'Check to see if the operating system has received a request
	'to close the window (clicking the X) and set the magic keyboard
	'index -1 if so. It can only be unset with clearkey.
	IF closerequest THEN
		closerequest = 0
		keybd(-1) = 1
	'ELSE
		'keybd(-1) = 0
	END IF
	if keybd(scPageup) > 0 and keybd(scPagedown) > 0 and keybd(scEsc) > 1 then keybd(-1) = 1

#ifdef IS_CUSTOM
	if keybd(-1) then keybd(scEsc) = 7
#elseif defined(IS_GAME)
	'Quick abort (could probably do better, just moving this here for now)
	IF keyval(-1) THEN
		'uncomment for slice debugging
		'DestroyGameSlices YES
		exitprogram 0
	END IF
#endif

	snapshot_check

	'reset arrow key fire state
	diagonalhack = -1

	if keyval(scCtrl) > 0 and keyval(scTilde) and 4 then
		showfps xor= 1
	end if

	'some debug keys for working on resolution independence
	if (keyval(scLeftShift) > 0 or keyval(scRightShift) > 0) and keyval(sc1) > 0 then
		if variablerez then
			if keyval(scRightBrace) > 1 then
				windowsize.w += 10
				windowsize.h += 10
			end if
			if keyval(scLeftBrace) > 1 then
				windowsize.w -= 10
				windowsize.h -= 10
				windowsize.w = large(windowsize.w, minwinsize.w)
				windowsize.h = large(windowsize.h, minwinsize.h)
			end if
		end if
		if keyval(scR) > 1 then
			variablerez xor= 1
			gfx_setresizable(variablerez)
			if forcevispageresize = NO then
				forcevispageresize = YES
			else
				forcevispageresize = NO
				resetresolution
			end if
		end if
	end if

	if mouse_grab_requested then
		if keyval(scScrollLock) > 1 then
			clearkey(scScrollLock)
			mouserect -1, -1, -1, -1
			mouse_grab_requested = -1
			mouse_grab_overridden = -1
		end if
	end if

end SUB

SUB clearkey(byval k as integer)
	keybd(k) = 0
	if k >= 0 then
		keysteps(k) = 1
	end if
end sub

'Set keyval(-1) on. So ugly
SUB setquitflag ()
	keybd(-1) = 1
END SUB

'these are wrappers provided by the polling thread
SUB io_amx_keybits cdecl (keybdarray as integer ptr)
	for a as integer = 0 to &h7f
		keybdarray[a] = keybdstate(a)
		keybdstate(a) = keybdstate(a) and 1
	next
END SUB

SUB io_amx_mousebits cdecl (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer, byref mclicks as integer)
	'get the mouse state one last time, for good measure
	io_getmouse(mx, my, mwheel, mbuttons)
	mclicks = mouseflags or (mbuttons and not mouselastflags)
	mouselastflags = mbuttons
	mouseflags = 0
	mbuttons = mbuttons or mclicks
END SUB

sub pollingthread(byval unused as any ptr)
	dim as integer a, dummy, buttons

	while endpollthread = 0
		mutexlock keybdmutex

		io_updatekeys(@keybdstate(0))
		'set key state for every key
		'highest scancode in fbgfx.bi is &h79, no point overdoing it
		for a = 0 to &h7f
			if keybdstate(a) and 8 then
				'decide whether to fire a new key event, otherwise the keystate is preserved
				if (keybdstate(a) and 1) = 0 then
					'this is a new keypress
					keybdstate(a) = keybdstate(a) or 6 'key was triggered, new keypress
				end if
			end if
			'move the bit (clearing it) that io_updatekeys sets from 8 to 1
			keybdstate(a) = (keybdstate(a) and 6) or ((keybdstate(a) shr 3) and 1)
		next

		io_getmouse(dummy, dummy, dummy, buttons)
		mouseflags = mouseflags or (buttons and not mouselastflags)
		mouselastflags = buttons

		mutexunlock keybdmutex

		'25ms was found to be sufficient
		sleep 25
	wend
end sub

sub post_terminate_signal cdecl ()
	'in future, we might like to do something here about infinite loops and bug 233, etc
	closerequest = 1
end sub

'No clipping!!
SUB putpixel (byval spr as Frame ptr, byval x as integer, byval y as integer, byval c as integer)
	if x < 0 orelse x >= spr->w orelse y < 0 orelse y >= spr->h then
		exit sub
	end if

	FRAMEPIXEL(x, y, spr) = c
end SUB

SUB putpixel (BYVAL x as integer, BYVAL y as integer, BYVAL c as integer, BYVAL p as integer)
	if clippedframe <> vpages(p) then
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
	if clippedframe <> vpages(p) then
		setclip , , , , p
	end if

	if POINT_CLIPPED(x, y) then
		debug "attempt to readpixel off-screen " & x & "," & y & " on page " & p
		return 0
	end if

	return PAGEPIXEL(x, y, p)
end FUNCTION

SUB drawbox (BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL col as integer, BYVAL thick as integer = 1, BYVAL p as integer)
	drawbox vpages(p), x, y, w, h, col, thick
END SUB

'Draw a hollow box, with given edge thickness
SUB drawbox (BYVAL dest as Frame ptr, BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL col as integer, BYVAL thick as integer = 1)
	if w < 0 then x = x + w + 1: w = -w
	if h < 0 then y = y + h + 1: h = -h

	if w = 0 or h = 0 then exit sub

        dim as integer thickx = small(thick, w), thicky = small(thick, h)

	rectangle dest, x, y, w, thicky, col
	IF h > thicky THEN
		rectangle dest, x, y + h - thicky, w, thicky, col
	END IF
	rectangle dest, x, y, thickx, h, col
	IF w > thickx THEN
		rectangle dest, x + w - thickx, y, thickx, h, col
	END IF
END SUB

SUB rectangle (BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL c as integer, BYVAL p as integer)
	rectangle vpages(p), x, y, w, h, c
END SUB

SUB rectangle (BYVAL fr as Frame Ptr, BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL c as integer)
	if clippedframe <> fr then
		setclip , , , , fr
	end if

	if fr = 0 then debug "rectangle null ptr": exit sub

	if w < 0 then x = x + w + 1: w = -w
	if h < 0 then y = y + h + 1: h = -h

	'clip
	if x + w > clipr then w = (clipr - x) + 1
	if y + h > clipb then h = (clipb - y) + 1
	if x < clipl then w -= (clipl - x) : x = clipl
	if y < clipt then h -= (clipt - y) : y = clipt

	if w <= 0 or h <= 0 then exit sub

	dim sptr as ubyte ptr = fr->image + (y * fr->pitch) + x
	while h > 0
		memset(sptr, c, w)
		sptr += fr->pitch
		h -= 1
	wend
END SUB

SUB fuzzyrect (BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL c as integer, BYVAL p as integer, BYVAL fuzzfactor as integer = 50)
	fuzzyrect vpages(p), x, y, w, h, c, fuzzfactor
END SUB

SUB fuzzyrect (BYVAL fr as Frame Ptr, BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL c as integer, BYVAL fuzzfactor as integer = 50)
	'How many magic constants could you wish for?
	'These were half generated via magic formulas, and half hand picked (with magic criteria)
	static grain_table(50) as integer = {_
	                    50, 46, 42, 38, 38, 40, 41, 39, 26, 38, 30, 36, _
	                    42, 31, 39, 38, 41, 26, 27, 28, 40, 35, 35, 31, _
	                    39, 50, 41, 30, 29, 28, 45, 37, 24, 43, 23, 42, _
	                    21, 28, 11, 16, 20, 22, 18, 17, 19, 32, 17, 16, _
	                    15, 14, 50}

	if clippedframe <> fr then
		setclip , , , , fr
	end if

        if fuzzfactor <= 0 then fuzzfactor = 1
        dim grain as integer
        dim r as integer = 0
	dim startr as integer = 0

	if fuzzfactor <= 50 then grain = grain_table(fuzzfactor) else grain = grain_table(100 - fuzzfactor)
	'if w = 99 then grain = h mod 100  'for hand picking

	if w < 0 then x = x + w + 1: w = -w
	if h < 0 then y = y + h + 1: h = -h

	'clip
	if x + w > clipr then w = (clipr - x) + 1
	if y + h > clipb then h = (clipb - y) + 1
	if x < clipl then
		startr += (clipl - x) * fuzzfactor
		w -= (clipl - x)
		x = clipl
	end if
	if y < clipt then
		startr += (clipt - y) * grain
		h -= (clipt - y)
		y = clipt
	end if

	if w <= 0 or h <= 0 then exit sub

	dim sptr as ubyte ptr = fr->image + (y * fr->pitch) + x
	while h > 0
		startr = (startr + grain) mod 100
		r = startr
                for i as integer = 0 to w-1
			r += fuzzfactor
			if r >= 100 then
				sptr[i] = c
				r -= 100
			end if
		next
		h -= 1
		sptr += fr->pitch
	wend
end SUB

SUB drawline (BYVAL x1 as integer, BYVAL y1 as integer, BYVAL x2 as integer, BYVAL y2 as integer, BYVAL c as integer, BYVAL p as integer)
	drawline vpages(p), x1, y1, x2, y2, c
END SUB

SUB drawline (BYVAL dest as Frame ptr, BYVAL x1 as integer, BYVAL y1 as integer, BYVAL x2 as integer, BYVAL y2 as integer, BYVAL c as integer)
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

	if clippedframe <> dest then
		setclip , , , , dest
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
	sptr = dest->image + (y1 * dest->pitch) + x1

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
  		instep = dest->pitch
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
  		instep = dest->pitch + xdirection
  		DRAW_SLICE(ydiff+1)
		exit sub
  	end if

	'now the actual bresenham
  	if xdiff > ydiff then
  		longaxis = xdiff
		shortaxis = ydiff

		instep = xdirection
		outstep = dest->pitch
  	else
		'other way round, draw vertical slices
		longaxis = ydiff
		shortaxis = xdiff

		instep = dest->pitch
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

SUB paintat (BYVAL dest as Frame ptr, BYVAL x as integer, BYVAL y as integer, BYVAL c as integer)
'a floodfill.
	dim tcol as integer
	dim queue as node ptr = null
	dim tail as node ptr = null
	dim as integer w, e		'x coords west and east
	dim i as integer
	dim tnode as node ptr = null

	if clippedframe <> dest then
		setclip , , , , dest
	end if

	if POINT_CLIPPED(x, y) then exit sub

	tcol = readpixel(dest, x, y)	'get target colour

	'prevent infinite loop if you fill with the same colour
	if tcol = c then exit sub

	queue = callocate(sizeof(node))
	queue->x = x
	queue->y = y
	queue->nextnode = null
	tail = queue

	'we only let coordinates within the clip bounds get onto the queue, so there's no need to check them

	do
		if FRAMEPIXEL(queue->x, queue->y, dest) = tcol then
			FRAMEPIXEL(queue->x, queue->y, dest) = c
			w = queue->x
			e = queue->x
			'find western limit
			while w > clipl and FRAMEPIXEL(w-1, queue->y, dest) = tcol
				w -= 1
				FRAMEPIXEL(w, queue->y, dest) = c
			wend
			'find eastern limit
			while e < clipr and FRAMEPIXEL(e+1, queue->y, dest) = tcol
				e += 1
				FRAMEPIXEL(e, queue->y, dest) = c
			wend
			'add bordering nodes
			for i = w to e
				if queue->y > clipt then
					'north
					if FRAMEPIXEL(i, queue->y-1, dest) = tcol then
						tail->nextnode = callocate(sizeof(node))
						tail = tail->nextnode
						tail->x = i
						tail->y = queue->y-1
						tail->nextnode = null
					end if
				end if
				if queue->y < clipb then
					'south
					if FRAMEPIXEL(i, queue->y+1, dest) = tcol then
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

SUB ellipse (BYVAL fr as Frame ptr, BYVAL x as double, BYVAL y as double, BYVAL radius as double, BYVAL col as integer, BYVAL semiminor as double = 0.0, BYVAL angle as double = 0.0)
'radius is the semimajor axis if the ellipse is not a circle
'angle is the angle of the semimajor axis to the x axis, in radians counter-clockwise

	if clippedframe <> fr then
		setclip , , , , fr
	end if

	'x,y is the pixel to centre the ellipse at - that is, the centre of that pixel, so add half a pixel to
	'radius to put the perimeter halfway between two pixels
	x += 0.5
	y += 0.5
	radius += 0.5
	if semiminor = 0.0 then semiminor = radius

	dim as double ypart
	ypart = fmod(y, 1.0) - 0.5  'Here we add in the fact that we test for intercepts with a line offset 0.5 pixels

	dim as double sin_2, cos_2, sincos
	sin_2 = sin(-angle) ^ 2
	cos_2 = cos(-angle) ^ 2
	sincos = sin(-angle) * cos(-angle)

	'Coefficients of the general conic quadratic equation Ax^2 + Bxy + Cy^2 + Dx + Ey + F = 0  (D,E = 0)
	'Aprime, Cprime are of the unrotated version
	dim as double Aprime, Cprime
	Aprime = 1.0 / radius ^ 2
	Cprime = 1.0 / semiminor ^ 2

	dim as double A, B, C, F
	A = Aprime * cos_2 + Cprime * sin_2
	B = 2 * (Cprime - Aprime) * sincos
	C = Aprime * sin_2 + Cprime * cos_2
	F = -1.0

	dim as integer xstart = 999999, xend = -999999, lastxstart = 999999, lastxend = -999999, xs, yi, ys, maxr = radius + 1

	for yi = maxr to -maxr step -1
		'Draw the circle as two semi-circles, so that we can approach the from
		'Note yi is cartesian coordinates, with the centre of the ellipsis at the origin, NOT screen coordinates!
		'xs, ys are in screen coordinates
		ys = int(y) - yi

		'Fix y (scanline) and solve for x using quadratic formula (coefficients:)
		dim as double qf_a, qf_b, qf_c
		qf_a = A
		qf_b = B * (yi + ypart)
		qf_c = C * (yi + ypart) ^ 2 + F

		dim as double discrim
		discrim = qf_b^2 - 4.0 * qf_a * qf_c
		if discrim >= 0.0 then
			discrim = sqr(discrim)

                        'This algorithm is very sensitive to which way XXX.5 is rounded (normally towards even)...
                        xstart = -int(-(x + (-qf_b - discrim) / (2.0 * qf_a) - 0.5))  'ceil(x-0.5), ie. round 0.5 down
			xend = int(x + (-qf_b + discrim) / (2.0 * qf_a) - 0.5)  'floor(x-0.5), ie. round 0.5 up, and subtract 1

			if xstart > xend then  'No pixel centres on this scanline lie inside the ellipse
				if lastxstart <> 999999 then
					xend = xstart  'We've already started drawing, so must draw at least one pixel
				end if
			end if
		end if

		'Reconsider the previous scanline
		for xs = lastxstart to xstart - 1
			putpixel(fr, xs, ys - 1, col)
		next
		for xs = xend + 1 to lastxend
			putpixel(fr, xs, ys - 1, col)
		next

		dim canskip as integer = YES
		for xs = xstart to xend
			putpixel(fr, xs, ys, col)
			if canskip andalso xs >= lastxstart - 1 then
				'Draw the bare minimum number of pixels (some of these might be needed, but won't know until next scanline)
				xs = small(xend - 1, lastxend)
				canskip = NO  'Skipping more than once causes infinite loops
			end if
		next
		lastxstart = xstart
		lastxend = xend
		if discrim >= 0 then xend = xstart - 1  'To draw the last scanline, in the next loop
	next
END SUB

'Replaces one colour with another within a rectangular region.
'Specifying the region is optional
SUB replacecolor (BYVAL fr as Frame ptr, BYVAL c_old as integer, BYVAL c_new as integer, BYVAL x as integer = -1, BYVAL y as integer = -1, BYVAL w as integer = -1, BYVAL h as integer = -1)
	if clippedframe <> fr then
		setclip , , , , fr
	end if

	if (x and y and w and h) = -1 then
		'Default to whole clipped region
		x = clipl
		y = clipt
		w = clipr - clipl + 1
		h = clipb - clipt + 1
	else
		if w < 0 then x = x + w + 1: w = -w
		if h < 0 then y = y + h + 1: h = -h

		'clip
		if x + w > clipr then w = (clipr - x) + 1
		if y + h > clipb then h = (clipb - y) + 1
		if x < clipl then w -= (clipl - x) : x = clipl
		if y < clipt then h -= (clipt - y) : y = clipt
	end if

	if w <= 0 or h <= 0 then exit sub

	dim as integer xi, yi
	for yi = y to y + h - 1
		dim sptr as ubyte ptr = fr->image + (yi * fr->pitch)
		for xi = x to x + w - 1
			if sptr[xi] = c_old then sptr[xi] = c_new
		next
	next
END SUB

SUB storemxs (fil as string, BYVAL record as integer, BYVAL fr as Frame ptr)
'saves a screen page to a file. Doesn't support non-320x200 pages
	dim f as integer
	dim as integer x, y
	dim sptr as ubyte ptr
	dim plane as integer

	if NOT fileiswriteable(fil) then exit sub
	f = freefile
	open fil for binary access read write as #f

	'skip to index
	seek #f, (record*64000) + 1 'will this work with write access?

	'modex format, 4 planes
	for plane = 0 to 3
		for y = 0 to 199
			sptr = fr->image + fr->pitch * y + plane

			for x = 0 to (80 - 1) '1/4 of a row
				put #f, , *sptr
				sptr = sptr + 4
			next
		next
	next

	close #f
end SUB

FUNCTION loadmxs (fil as string, BYVAL record as integer, BYVAL dest as Frame ptr = NULL) as Frame ptr
'loads a 320x200 mode X format page from a file.
'You may optionally pass in existing frame to load into.
	dim f as integer
	dim as integer x, y
	dim sptr as ubyte ptr
	dim plane as integer

	if NOT fileisreadable(fil) then return 0
	f = freefile
	open fil for binary access read as #f

	if lof(f) < (record + 1) * 64000 then
		debug "loadpage: wanted page " & record & "; " & fil & " is only " & lof(f) & " bytes"
		close #f
		return dest
	end if

	'skip to index
	seek #f, (record*64000) + 1

	if dest = NULL then
		dest = frame_new(320, 200)
	end if

	'modex format, 4 planes
	for plane = 0 to 3
		for y = 0 to 199
			sptr = dest->image + dest->pitch * y + plane

			for x = 0 to (80 - 1) '1/4 of a row
				get #f, , *sptr
				sptr = sptr + 4
			next
		next
	next

	close #f
	return dest
end FUNCTION

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
		i = bound((waittime - timer) * 1000, 1, 5)
		sleep i
		io_waitprocessing()
	loop
	if waitset = 1 then
		waitset = 0
	else
		debug "dowait called without setwait"
	end if
	return timer >= flagtime
end FUNCTION

'FIXME: sprite pitch and so on!
SUB printstr (BYVAL dest as Frame ptr, s as string, BYVAL startx as integer, BYVAL y as integer, BYVAL startfont as Font ptr, BYVAL pal as Palette16 ptr, BYVAL withtags as integer)
	if clippedframe <> dest then
		setclip , , , , dest
	end if

	startx += startfont->offset.x
	y += startfont->offset.y

	'check bounds skipped because this is now quite hard to tell (checked in drawohr)

	dim as Frame charframe
	charframe.mask = NULL

	'decide whether to draw a solid background or not
	dim as integer trans_type = -1
	if pal->col(0) > 0  then
		trans_type = 0
	end if

	'We have to process both layers, even if this font has only one layer, incase it switches to a font that has two!
	for layer as integer = 0 to 1
		'Make a local copy of the palette, for modifications
		dim localpal as Palette16 = *pal

		dim thefont as Font ptr = startfont

		dim x as integer = startx
		'dim y as integer = starty

		for ch as integer = 0 to len(s) - 1
			if withtags ANDALSO memcmp(@s[ch], @"${", 2) = 0 then
				dim closebrace as integer = instr(ch + 1 + 4, s, "}")
				if closebrace then
					dim action as byte = toupper(s[ch + 2])
					dim arg as string = mid(s, ch + 1 + 3, closebrace - (ch + 1) - 3)
					dim intarg as integer = str2int(arg)

					if action = asc("K") then
						if intarg = -1 then intarg = pal->col(1)
						localpal.col(1) = intarg
					elseif action = asc("F") then
						'Let's preserve the position offset when changing fonts. That way, plain text in
						'the middle of edgetext is also offset +1,+1, so that it lines up visually with it
						'x += fonts(intarg).offset.x - thefont->offset.x
						'y += fonts(intarg).offset.y - thefont->offset.y
						if intarg = -1 then
							thefont = startfont
						else
							thefont = @fonts(intarg)
						end if
					else
						goto badaction
					end if
					ch = closebrace - 1
					continue for
				end if
			end if
			badaction:

			if thefont->sprite(layer) <> NULL then
				with thefont->sprite(layer)->chdata(s[ch])
					charframe.image = thefont->sprite(layer)->spr.image + .offset
					charframe.w = .w
					charframe.h = .h
					charframe.pitch = .w
					drawohr(@charframe, dest, @localpal, x + .offx, y + .offy, trans_type)
				end with
			end if

			'print one character past the end of the line
			'(I think this is a reasonable approximation)
			if x > clipr then
				continue for, for
			end if
			'note: do not use charframe.w, that's just the width of the sprite
			x += thefont->w(s[ch])
		next
	next
end SUB

'the old printstr
SUB printstr (s as string, BYVAL x as integer, BYVAL y as integer, BYVAL p as integer, BYVAL withtags as integer = NO)
	dim fontpal as Palette16

	fontpal.col(0) = textbg
	fontpal.col(1) = textfg

	printstr (vpages(p), s, x, y, @fonts(0), @fontpal, withtags)
end SUB

SUB edgeprint (s as string, BYVAL x as integer, BYVAL y as integer, BYVAL c as integer, BYVAL p as integer, BYVAL withtags as integer = NO)
	static fontpal as Palette16

	fontpal.col(0) = 0
	fontpal.col(1) = c
	fontpal.col(2) = uilook(uiOutline)

	'preserve the old behaviour (edgeprint used to call textcolor)
	textfg = c
	textbg = 0

	printstr (vpages(p), s, x, y, @fonts(1), @fontpal, withtags)
END SUB

SUB textcolor (BYVAL f as integer, BYVAL b as integer)
	textfg = f
	textbg = b
end SUB

'TODO/FIXME: need to use frame_* functions PROPERLY to handle Frame stuff
'In fact, this SUB is basically broken and does not work
SUB font_unload (byval font as Font ptr)
	if font = null then exit sub

	'look! polymorphism! definitely not hackery! yeah... look it up sometime.
	frame_unload cast(Frame ptr ptr, @font->sprite(0))
	frame_unload cast(Frame ptr ptr, @font->sprite(1))

	'delete font
end SUB

'TODO/FIXME: need to use frame_* functions to handle Frame stuff
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
	'font = new Font
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
		.refcount = 1  'NOREFC  '?????
		'.arrayelem = 1 ' ??????
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

'TODO/FIXME: need to use frame_* functions to handle Frame stuff (and some dodgy non-pitch-aware stuff here)
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

	'wish I could call frame_duplicate. A little OO would fix that.
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

'TODO/FIXME: need to use frame_* functions to handle Frame stuff
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
'TODO/FIXME: need to use frame_* functions to handle Frame stuff (plus pitch-awareness)
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

	dim as ubyte ptr image = allocate(4096)
	dim as ubyte ptr sptr
	dim as integer size = 0
	dim as integer i, x, y
	dim f as string
	dim tempfr as Frame ptr
	dim bchr as FontChar ptr
	bchr = @fallback->sprite(1)->chdata(0)


	for i = 0 to 255
		with font->sprite(1)->chdata(i)
			f = "testfont" & SLASH & i & ".bmp"
			if isfile(f) then
				'FIXME: awful stuff
				tempfr = frame_import_bmp_raw(f)  ', master())

				.offset = size
				.offx = 0
				.offy = 0
				.w = tempfr->w
				.h = tempfr->h
				font->w(i) = .w
				size += .w * .h
				image = reallocate(image, size)
				sptr = image + .offset
				memcpy(sptr, tempfr->image, .w * .h)
				frame_unload @tempfr
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

SUB storeset (fil as string, BYVAL i as integer, BYVAL l as integer)
' i = index, l = line (only if reading from screen buffer)
	dim f as integer
	dim idx as integer
	dim bi as integer
	dim ub as ubyte
	dim toggle as integer
	dim sptr as ubyte ptr

	if NOT fileiswriteable(fil) then exit sub
	f = freefile
	open fil for binary access read write as #f

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

SUB loadset (fil as string, BYVAL i as integer, BYVAL l as integer)
' i = index, l = line (only if reading to screen buffer)
	dim f as integer
	dim idx as integer
	dim bi as integer
	dim ub as ubyte
	dim toggle as integer
	dim sptr as ubyte ptr

	if NOT fileisreadable(fil) then exit sub
	f = freefile
	open fil for binary access read as #f

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

'b is in BYTES
SUB setpicstuf (buf() as integer, BYVAL b as integer, BYVAL p as integer)
	if p >= 0 then
		if clippedframe <> vpages(p) then
			setclip , , , , p
		end if
	end if

	bptr = @buf(0) 'doesn't really work well with FB
	bsize = b
	bpage = p
end SUB

'--------------------- Audio routines ----------------------

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

FUNCTION get_music_volume () as single
	return music_getvolume
end FUNCTION

SUB set_music_volume (BYVAL vol as single)
	music_setvolume(vol)
end SUB



SUB screenshot (f$)
	'try external first
	if gfx_screenshot(f$) = 0 then
		'otherwise save it ourselves
		frame_export_bmp8(f$ + ".bmp", vpages(vpage), intpal())
	end if
END SUB

sub snapshot_check
'The best of both worlds. Holding down F12 takes a screenshot each frame, however besides
'the first, they're saved to the temporary directory until key repeat kicks in, and then
'moved, to prevent littering
'NOTE: global variables like tmpdir can change between calls, have to be lenient
	static as string*4 image_exts(3) => {".bmp", ".png", ".jpg", ".dds"}
	'dynamic static array. Redim before use.
	static as string backlog()
	redim preserve backlog(ubound(backlog))
	static as integer backlog_num

	dim as integer n, i

	if keyval(scF12) = 0 then
		'delete the backlog
		for n = 1 to ubound(backlog)
			'debug "killing " & backlog(n)
			safekill backlog(n)
		next
		redim backlog(0)
		backlog_num = 0
	else
		dim as string shot
		dim as string gamename = trimextension(trimpath(sourcerpg))
		if gamename = "" then
			gamename = "ohrrpgce"
		end if

		for n = backlog_num to 9999
			shot = gamename + right("000" & n, 4)
			'checking curdir, which is export directory
			for i = 0 to ubound(image_exts)
				if isfile(shot + image_exts(i)) then continue for, for
			next
			exit for
		next
		backlog_num = n + 1		

		if keyval(scF12) = 1 then
			shot = tmpdir + shot
			screenshot shot
			for i = 0 to ubound(image_exts)
				if isfile(shot + image_exts(i)) then str_array_append(backlog(), shot + image_exts(i))
			next
		else
			screenshot shot
			'move our backlog of screenshots to the visible location
			for n = 1 to ubound(backlog)
				'debug "moving " & backlog(n) & " to " & curdir + slash + trimpath(backlog(n))
				name backlog(n), curdir + slash + trimpath(backlog(n))
			next
			redim backlog(0)
		end if
		'debug "screen " & shot
	end if
end sub

FUNCTION havemouse() as integer
'atm, all backends support the mouse, or don't know
	 return -1
end FUNCTION

SUB hidemousecursor ()
	io_setmousevisibility(0)
end SUB

SUB unhidemousecursor ()
	io_setmousevisibility(-1)
	io_mouserect(-1, -1, -1, -1)
end SUB

FUNCTION readmouse () as MouseInfo
	dim info as MouseInfo

	mutexlock keybdmutex   'is this necessary?
	io_mousebits(info.x, info.y, info.wheel, info.buttons, info.clicks)
	mutexunlock keybdmutex

	'gfx_fb/sdl/alleg return last onscreen position when the mouse is offscreen
	'gfx_fb: If you release a mouse button offscreen, it becomes stuck (FB bug)
	'        wheel scrolls offscreen are registered when you move back onscreen
	'gfx_alleg: button state continues to work offscreen but wheel scrolls are not registered
	'gfx_sdl: button state works offscreen. wheel state not implemented yet

	if mouse_dragmask then
		'Test whether drag ended
		if (info.clicks and mouse_dragmask) orelse (info.buttons and mouse_dragmask) = 0 then
			mouse_dragmask = 0
			mouse_clickstart = TYPE<XYPair>(0, 0)
		end if
	end if

	if mouse_dragmask = 0 then
		'Dragging is only tracked for a single button at a time, and clickstart is not updated
		'while dragging either. So we may now test for new drags or clicks.
		for i as integer = 0 to 2
			dim mask as integer = 2 ^ i
			if info.clicks and mask then
				'Do not flag as dragging until the second tick
				mouse_clickstart = TYPE<XYPair>(info.x, info.y)
			elseif info.buttons and mask then
				'left mouse button down, but no new click this tick
				mouse_dragmask = mask
				exit for
			end if
		next
	end if

	info.dragging = mouse_dragmask
	info.clickstart = mouse_clickstart

	if info.clicks <> 0 then
		if mouse_grab_requested andalso mouse_grab_overridden then
			mouserect remember_mouse_grab(0), remember_mouse_grab(1), remember_mouse_grab(2), remember_mouse_grab(3)
		end if
	end if

	return info
end FUNCTION

SUB movemouse (BYVAL x as integer, BYVAL y as integer)
	io_setmouse(x, y)
end SUB

SUB mouserect (BYVAL xmin, BYVAL xmax, BYVAL ymin, BYVAL ymax)
	if gfxbackend = "fb" or gfxbackend = "sdl" then
		if xmin = -1 and xmax = -1 and ymin = -1 and ymax = -1 then
			mouse_grab_requested = 0
			settemporarywindowtitle remember_title
		else
			remember_mouse_grab(0) = xmin
			remember_mouse_grab(1) = xmax
			remember_mouse_grab(2) = ymin
			remember_mouse_grab(3) = ymax
			mouse_grab_requested = -1
			mouse_grab_overridden = 0
			settemporarywindowtitle remember_title & " (ScrlLock to free mouse)"
		end if
	end if
	mutexlock keybdmutex
	io_mouserect(xmin, xmax, ymin, ymax)
	mutexunlock keybdmutex
end sub

FUNCTION readjoy (joybuf() as integer, BYVAL jnum as integer) as integer
'Return 0 if joystick is not present, or -1 (true) if joystick is present
'jnum is the joystick to read (QB implementation supports 0 and 1)
'joybuf(0) = Analog X axis (scaled to -100 to 100)
'joybuf(1) = Analog Y axis
'joybuf(2) = button 1: 0=pressed nonzero=not pressed
'joybuf(3) = button 2: 0=pressed nonzero=not pressed
'Other values in joybuf() should be preserved.
'If X and Y axis are not analog,
'  upward motion when joybuf(0) < joybuf(9)
'  down motion when joybuf(0) > joybuf(10)
'  left motion when joybuf(1) < joybuf(11)
'  right motion when joybuf(1) > joybuf(12)
	dim as integer buttons, x, y
	if io_readjoysane(jnum, buttons, x, y) = 0 then return 0

	joybuf(0) = x
	joybuf(1) = y
	joybuf(2) = (buttons AND 1) = 0 '0 = pressed, not 0 = unpressed (why???)
	joybuf(3) = (buttons AND 2) = 0 'ditto
	return -1
end FUNCTION

FUNCTION readjoy (byval joynum as integer, byref buttons as integer, byref x as integer, byref y as integer) as integer
	return io_readjoysane(joynum, buttons, x, y)
end FUNCTION

function calcblock(tmap as TileMap, byval x as integer, byval y as integer, byval overheadmode as integer, pmapptr as TileMap ptr) as integer
'returns -1 to draw no tile
'overheadmode = 1 : draw non overhead tiles only (to avoid double draw)
'overheadmode = 2 : draw overhead tiles only
	dim block as integer

	'check bounds
	if bordertile = -1 then
		'wrap
		while y < 0
			y = y + tmap.high
		wend
		while y >= tmap.high
			y = y - tmap.high
		wend
		while x < 0
			x = x + tmap.wide
		wend
		while x >= tmap.wide
			x = x - tmap.wide
		wend
	else
		if (y < 0) or (y >= tmap.high) or (x < 0) or (x >= tmap.wide) then
			if tmap.layernum = 0 and overheadmode <= 1 then
				'only draw the border tile once!
				return bordertile
			else
				return -1
			end if
		end if
	end if

	block = readblock(tmap, x, y)

	if block = 0 and tmap.layernum > 0 then  'This could be an argument, maybe we could get rid of layernum
		return -1
	end if

	if overheadmode > 0 then
		if x >= pmapptr->wide or y >= pmapptr->high or pmapptr = NULL then
			if overheadmode = 2 then block = -1
		elseif ((readblock(*pmapptr, x, y) and 128) <> 0) xor (overheadmode = 2) then
			block = -1
		end if
	end if

	return block
end function

'----------------------------------------------------------------------
'BMP functions - other formats are probably quite simple
'with Allegro or SDL or FreeImage, but we'll stick to this for now.
'----------------------------------------------------------------------

SUB frame_export_bmp8 (f as string, byval fr as Frame Ptr, maspal() as RGBcolor)
	dim argb as RGBQUAD
	dim as integer of, y, i, skipbytes
	dim as ubyte ptr sptr

	of = write_bmp_header(f, fr->w, fr->h, 8)
	if of = -1 then exit sub

	for i = 0 to 255
		argb.rgbRed = maspal(i).r
		argb.rgbGreen = maspal(i).g
		argb.rgbBlue = maspal(i).b
		put #of, , argb
	next

	skipbytes = 4 - (fr->w mod 4)
	if skipbytes = 4 then skipbytes = 0
	sptr = fr->image + (fr->h - 1) * fr->pitch
	for y = fr->h - 1 to 0 step -1
		'put is possibly the most screwed up FB builtin; the use of the fput wrapper soothes the soul
		fput(of, , sptr, fr->w) 'equivalent to "put #of, , *sptr, fr->w"
		sptr -= fr->pitch
		'write some interesting dummy data
		fput(of, , fr->image, skipbytes)
	next

	close #of
end SUB

SUB frame_export_bmp4 (f as string, byval fr as Frame Ptr, maspal() as RGBcolor, byval pal as Palette16 ptr)
	dim argb as RGBQUAD
	dim as integer of, x, y, i, skipbytes
	dim as ubyte ptr sptr
	dim as ubyte pix

	of = write_bmp_header(f, fr->w, fr->h, 4)
	if of = -1 then exit sub

	for i = 0 to 15
		argb.rgbRed = maspal(pal->col(i)).r
		argb.rgbGreen = maspal(pal->col(i)).g
		argb.rgbBlue = maspal(pal->col(i)).b
		put #of, , argb
	next

	skipbytes = 4 - ((fr->w / 2) mod 4)
	if skipbytes = 4 then skipbytes = 0
	sptr = fr->image + (fr->h - 1) * fr->pitch
	for y = fr->h - 1 to 0 step -1
		for x = 0 to fr->w - 1
			if (x and 1) = 0 then
				pix = sptr[x] shl 4
			else
				pix or= sptr[x]
				put #of, , pix
			end if
		next
		if fr->w mod 2 then
			put #of, , pix
		end if
		sptr -= fr->pitch
		'write some interesting dummy data
		fput(of, , fr->image, skipbytes)
	next

	close #of
end SUB

'Creates a new file and writes the bmp headers to it.
'Returns a file handle, or -1 on error.
private function write_bmp_header(f as string, byval w as integer, byval h as integer, byval bitdepth as integer) as integer
	dim header as BITMAPFILEHEADER
	dim info as BITMAPINFOHEADER

	dim as integer of, imagesize, imageoff

	imagesize = ((w * bitdepth + 31) \ 32) * 4 * h

	imageoff = 54 + (1 shl bitdepth) * 4

	header.bfType = 19778
	header.bfSize = imageoff + imagesize
	header.bfReserved1 = 0
	header.bfReserved2 = 0
	header.bfOffBits = imageoff

	info.biSize = 40
	info.biWidth = w
	info.biHeight = h
	info.biPlanes = 1
	info.biBitCount = bitdepth
	info.biCompression = 0
	info.biSizeImage = imagesize
	info.biXPelsPerMeter = &hB12
	info.biYPelsPerMeter = &hB12
	info.biClrUsed = 0
	info.biClrImportant = 0

	if NOT fileiswriteable(f$) then return -1
	safekill f$
	of = freefile
	open f$ for binary access write as #of

	put #of, , header
	put #of, , info

	return of
end function

FUNCTION frame_import_bmp24(bmp as string, pal() as RGBcolor) as Frame ptr
'loads the 24-bit bitmap bmp$, mapped to palette pal()
	dim header as BITMAPFILEHEADER
	dim info as BITMAPINFOHEADER
	dim bf as integer
	dim ret as Frame ptr

	if NOT fileisreadable(bmp) then return 0
	bf = freefile
	open bmp for binary access read as #bf

	get #bf, , header
	if header.bfType <> 19778 then
		'not a bitmap
		close #bf
		return 0
	end if

	get #bf, , info

	if info.biBitCount <> 24 then
		close #bf
		return 0
	end if

	'navigate to the beginning of the bitmap data
	seek #bf, header.bfOffBits + 1

	ret = frame_new(info.biWidth, info.biHeight)

	loadbmp24(bf, ret, pal())

	close #bf
	return ret
END FUNCTION

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

FUNCTION frame_import_bmp_raw(bmp as string) as Frame ptr
'load a 4- or 8-bit .BMP, ignoring the palette
	dim header as BITMAPFILEHEADER
	dim info as BITMAPINFOHEADER
	dim bf as integer
	dim ret as frame ptr

	if NOT fileisreadable(bmp) then return 0
	bf = freefile
	open bmp for binary access read as #bf

	get #bf, , header
	if header.bfType <> 19778 then
		'not a bitmap
		close #bf
		return 0
	end if

	get #bf, , info

	if info.biBitCount <> 4 and info.biBitCount <> 8 then
		close #bf
		return 0
	end if

	'use header offset to get to data
	seek #bf, header.bfOffBits + 1

	ret = frame_new(info.biWidth, info.biHeight, , 1)

	if info.biBitCount = 4 then
		'call one of two loaders depending on compression
		if info.biCompression = BI_RGB then
			loadbmp4(bf, ret)
		elseif info.biCompression = BI_RLE4 then
			loadbmprle4(bf, ret)
		end if
	else
		loadbmp8(bf, ret)
	end if

	close #bf
	return ret
END FUNCTION

PRIVATE SUB loadbmp24(byval bf as integer, byval fr as Frame ptr, pal() as RGBcolor)
'takes an open file handle, an already size Frame pointer, and a 256 colour palette to map to
	dim pix as RGBTRIPLE
	dim ub as ubyte
	dim as integer w, h
	dim sptr as ubyte ptr
	dim pad as integer

	'data lines are padded to 32-bit boundaries
	pad = 4 - ((fr->w * 3) mod 4)
	if pad = 4 then	pad = 0

	for h = fr->h - 1 to 0 step -1
		sptr = fr->image + h * fr->pitch
		for w = 0 to fr->w - 1
			'read the data
			get #bf, , pix
			*sptr = nearcolor(pal(), pix.rgbtRed, pix.rgbtGreen, pix.rgbtBlue)
			sptr += 1
		next
			'padding to dword boundary
		for w = 0 to pad-1
			get #bf, , ub
		next
	next
END SUB

PRIVATE SUB loadbmp8(byval bf as integer, byval fr as Frame ptr)
'takes an open file handle and an already size Frame pointer, should only be called within loadbmp
	dim ub as ubyte
	dim as integer w, h
	dim sptr as ubyte ptr
	dim pad as integer

	pad = 4 - (fr->w mod 4)
	if pad = 4 then	pad = 0

	for h = fr->h - 1 to 0 step -1
		sptr = fr->image + h * fr->pitch
		for w = 0 to fr->w - 1
			'read the data
			get #bf, , ub
			*sptr = ub
			sptr += 1
		next

		'padding to dword boundary
		for w = 0 to pad-1
			get #bf, , ub
		next
	next
END SUB

PRIVATE SUB loadbmp4(byval bf as integer, byval fr as Frame ptr)
'takes an open file handle and an already size Frame pointer, should only be called within loadbmp
	dim ub as ubyte
	dim as integer w, h
	dim sptr as ubyte ptr
	dim pad as integer

	pad = 4 - ((fr->w / 2) mod 4)
	if pad = 4 then	pad = 0

	for h = fr->h - 1 to 0 step -1
		sptr = fr->image + h * fr->pitch
		for w = 0 to fr->w - 1
			if (w and 1) = 0 then
				'read the data
				get #bf, , ub
				*sptr = (ub and &hf0) shr 4
			else
				'2nd nybble in byte
				*sptr = ub and &h0f
			end if
			sptr += 1
		next

		'padding to dword boundary
		for w = 0 to pad - 1
			get #bf, , ub
		next
	next
END SUB

PRIVATE SUB loadbmprle4(byval bf as integer, byval fr as Frame ptr)
'takes an open file handle and an already size Frame pointer, should only be called within loadbmp
	dim pix as ubyte
	dim ub as ubyte
	dim as integer w, h
	dim i as integer
	dim as ubyte bval, v1, v2

	w = 0
	h = fr->h - 1

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
						for i = 1 to ub
							if i and 1 then
								get #bf, , pix
								bval = (pix and &hf0) shr 4
							else
								bval = pix and &h0f
							end if
							fr->image[h * fr->pitch + w] = bval
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

				for i = 1 to ub
					if i and 1 then
						bval = v1
					else
						bval = v2
					end if
					fr->image[h * fr->pitch + w] = bval
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

FUNCTION bmpinfo (f$, byref dat as BITMAPINFOHEADER) as integer
	dim header as BITMAPFILEHEADER
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

	get #bf, , dat
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

'NOTE: there is only one set of clipping values, shared globally for
'all drawing operations... this is probably a bad thing, but that is how
'it works. The frame or page argument to setclip() is used to determine
'the allowed range of clipping values.

'Set the bounds used by various (not quite all?) video page drawing functions.
'setclip must be called to reset the clip bounds whenever the wrkpage changes, to ensure
'that they are valid (the video page dimensions might differ).
'Aside from tracking which page the clips are for, some legacy code actually uses wrkpage,
'these should be removed.
sub setclip(byval l as integer = 0, byval t as integer = 0, byval r as integer = 9999, byval b as integer = 9999, byval page as integer)
	wrkpage = page
	setclip l, t, r, b, vpages(wrkpage)
end sub

'more modern version
'would call this directly everywhere, but don't want to break that edge case that actually needs wrkpage set
sub setclip(byval l as integer = 0, byval t as integer = 0, byval r as integer = 9999, byval b as integer = 9999, byval fr as Frame ptr = 0)
	if fr <> 0 then clippedframe = fr
	with *clippedframe
		clipl = bound(l, 0, .w) '.w valid, prevents any drawing
		clipt = bound(t, 0, .h)
		clipr = bound(r, 0, .w - 1)
		clipb = bound(b, 0, .h - 1)
	end with
end sub

'Shrinks clipping area, never grows it
sub shrinkclip(byval l as integer = 0, byval t as integer = 0, byval r as integer = 9999, byval b as integer = 9999, byval fr as Frame ptr)
	clippedframe = fr
	with *clippedframe
		clipl = bound(large(clipl, l), 0, .w) '.w valid, prevents any drawing
		clipt = bound(large(clipt, t), 0, .h)
		clipr = bound(small(clipr, r), 0, .w - 1)
		clipb = bound(small(clipb, b), 0, .h - 1)
	end with
end sub

'trans: draw transparently, either using ->mask if available, or otherwise use colour 0 as transparent
'warning! Make sure setclip has been called before calling this
sub drawohr(byval src as Frame ptr, byval dest as Frame ptr, byval pal as Palette16 ptr = null, byval x as integer, byval y as integer, byval trans as integer = -1)
	dim as integer startx, starty, endx, endy
	dim as integer srcoffset

	startx = x
	endx = x + src->w - 1
	starty = y
	endy = y + src->h - 1

	if startx < clipl then
		srcoffset = (clipl - startx)
		startx = clipl
	end if

	if starty < clipt then
		srcoffset += (clipt - starty) * src->pitch
		starty = clipt
	end if

	if endx > clipr then
		endx = clipr
	end if

	if endy > clipb then
		endy = clipb
	end if

	if starty > endy or startx > endx then exit sub

	blitohr(src, dest, pal, srcoffset, startx, starty, endx, endy, trans)
end sub

function frame_to_tileset(byval spr as Frame ptr) as Frame ptr
	dim tileset as Frame ptr
	tileset = frame_new(20, 20 * 160)

	dim as ubyte ptr sptr = tileset->image
	dim as ubyte ptr srcp
	dim tilex, tiley, px, py

	for tiley = 0 to 9
		for tilex = 0 to 15
			srcp = spr->image + tilex * 20 + tiley * 320 * 20
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
	return tileset
end function

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

FUNCTION getmusictype (file as string) as integer

	if file = "" then
	  'no further checking for blank names
	  return 0
	end if

	if isdir(file) OR right(file, 1) = SLASH then
	  'no further checking if this is a directory
	  return 0
	end if

	DIM ext as string, chk as integer
	ext = lcase(justextension(file))

	'special case
	if str(cint(ext)) = ext then return FORMAT_BAM

	SELECT CASE ext
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
	  debug "unknown format: " & file & " - " & ext
	  chk = 0
	END SELECT

  return chk
END FUNCTION

'not to be used outside of the sprite functions
declare sub frame_freemem(byval f as frame ptr)
declare sub Palette16_delete(byval f as Palette16 ptr ptr)
declare sub spriteset_freemem(byval sprset as SpriteSet ptr)

'The sprite cache holds Frame ptrs, which may also be Frame arrays and SpriteSets. Since
'each SpriteSet is associated with a unique Frame array, we don't need a separate cache
'for SpriteSets. SpriteSet data can be loaded after and linked to the cached Frame array
'if it was initially not loaded as a SpriteSet.

'The sprite cache, which is a HashTable (sprcache) containing all loaded sprites, is split in
'two: the A cache containing currently in-use sprites (which is not explicitly tracked), and
'the B cache holding those not in use, which is a LRU list 'sprcacheB' which holds a maximum
'of SPRCACHEB_SZ entries.
'The number/size of in-use sprites is not limited, and does not count towards the B cache
'unless COMBINED_SPRCACHE_LIMIT is defined. It should be left undefined when memory usage
'is not actually important.

'I couldn't find any algorithms for inequal cost caching so invented my own: sprite size is
'measured in 'base size' units, and instead of being added to the head of the LRU list,
'sprites are moved a number of places back from the head equal to their size. This is probably
'an unnecessary complication over LRU, but it's fun.

CONST SPRCACHE_BASE_SZ = 4096  'bytes
CONST SPRCACHEB_SZ = 256  'in SPRITE_BASE_SZ units
'#DEFINE COMBINED_SPRCACHE_LIMIT 1


' removes a sprite from the cache, and frees it.
private sub sprite_remove_cache(byval entry as SpriteCacheEntry ptr)
	if entry->p->refcount <> 1 then
		debug "error: invalidly uncaching sprite " & entry->hashed.hash & " " & frame_describe(entry->p)
	end if
	dlist_remove(sprcacheB.generic, entry)
	hash_remove(sprcache, entry)
	entry->p->cacheentry = NULL  'help to detect double free
	frame_freemem(entry->p)
	#ifdef COMBINED_SPRCACHE_LIMIT
		sprcacheB_used -= entry->cost
	#else
		if entry->Bcached then
			sprcacheB_used -= entry->cost
		end if
	#endif
	deallocate(entry)
end sub

'Free some sprites from the end of the B cache
'Returns true if enough space was freed
private function sprite_cacheB_shrink(byval amount as integer) as integer
	sprite_cacheB_shrink = (amount <= SPRCACHEB_SZ)
	if sprcacheB_used + amount <= SPRCACHEB_SZ then exit function

	dim as SpriteCacheEntry ptr pt, prevpt
	pt = sprcacheB.last
	while pt
		prevpt = pt->cacheB.prev
		sprite_remove_cache(pt)
		if sprcacheB_used + amount <= SPRCACHEB_SZ then exit function
		pt = prevpt
	wend
end function

sub sprite_purge_cache(byval minkey as integer, byval maxkey as integer, leakmsg as string, byval freeleaks as integer = NO)
	dim iterstate as integer = 0
	dim as SpriteCacheEntry ptr pt, nextpt

	nextpt = NULL
	pt = hash_iter(sprcache, iterstate, nextpt)
	while pt
		nextpt = hash_iter(sprcache, iterstate, nextpt)
		'recall that the cache counts as a reference
		if pt->p->refcount <> 1 then
			debug "warning: " & leakmsg & pt->hashed.hash & " with " & pt->p->refcount & " references"
			if freeleaks then sprite_remove_cache(pt)
		else
			sprite_remove_cache(pt)
		end if
		pt = nextpt
	wend
end sub

'Attempt to completely empty the sprite cache, detecting memory leaks
sub sprite_empty_cache()
	sprite_purge_cache(INT_MIN, INT_MAX, "leaked sprite ")
	if sprcacheB_used <> 0 or sprcache.numitems <> 0 then
		debug "sprite_empty_cache: corruption: sprcacheB_used=" & sprcacheB_used & " items=" & sprcache.numitems
	end if
end sub

'removes all tilesets from the cache
sub tileset_empty_cache()
	sprite_purge_cache (100000000, 110000000, "could not purge tileset ")
end sub

sub sprite_debug_cache()
	debug "==sprcache=="
	dim iterstate as integer = 0
	dim pt as SpriteCacheEntry ptr = NULL

	while hash_iter(sprcache, iterstate, pt)
		debug pt->hashed.hash & " cost=" & pt->cost & " : " & frame_describe(pt->p)
	wend

	debug "==sprcacheB== (used units = " & sprcacheB_used & "/" & SPRCACHEB_SZ & ")"
	pt = sprcacheB.first
	while pt
		debug pt->hashed.hash & " cost=" & pt->cost & " : " & frame_describe(pt->p)
		pt = pt->cacheB.next
	wend
end sub

'a sprite has no references, move it to the B cache
private sub sprite_to_B_cache(byval entry as SpriteCacheEntry ptr)
	dim pt as SpriteCacheEntry ptr

	if sprite_cacheB_shrink(entry->cost) = NO then
		'fringe case: bigger than the max cache size
		sprite_remove_cache(entry)
		exit sub
	end if

	'apply size penalty
	pt = sprcacheB.first
	dim tobepaid as integer = entry->cost
	while pt
		tobepaid -= pt->cost
		if tobepaid <= 0 then exit while
		pt = pt->cacheB.next
	wend
	dlist_insertat(sprcacheB.generic, pt, entry)
	entry->Bcached = YES
	#ifndef COMBINED_SPRCACHE_LIMIT
		sprcacheB_used += entry->cost
	#endif
end sub

' move a sprite out of the B cache
private sub sprite_from_B_cache(byval entry as SpriteCacheEntry ptr)
	dlist_remove(sprcacheB.generic, entry)
	entry->Bcached = NO
	#ifndef COMBINED_SPRCACHE_LIMIT
		sprcacheB_used -= entry->cost
	#endif
end sub

' search cache, update as required if found
private function sprite_fetch_from_cache(byval key as integer) as Frame ptr
	dim entry as SpriteCacheEntry ptr
	
	entry = hash_find(sprcache, key)

	if entry then
		'cachehit += 1
		if entry->Bcached then
			sprite_from_B_cache(entry)
		end if
		entry->p->refcount += 1
		return entry->p
	end if
	return NULL
end function

' adds a newly loaded frame to the cache with a given key
private sub sprite_add_cache(byval key as integer, byval p as frame ptr)
	if p = 0 then exit sub

	dim entry as SpriteCacheEntry ptr
	entry = callocate(sizeof(SpriteCacheEntry))

	entry->hashed.hash = key
	entry->p = p
	entry->cost = (p->w * p->h * p->arraylen) \ SPRCACHE_BASE_SZ + 1
	'leave entry->cacheB unlinked
	entry->Bcached = NO

	'the cache counts as a reference, but only to the head element of an array!!
	p->cached = 1
	p->refcount += 1
	p->cacheentry = entry
	hash_add(sprcache, entry)

	#ifdef COMBINED_SPRCACHE_LIMIT
		sprcacheB_used += entry->cost
	#endif
end sub

function frame_new(byval w as integer, byval h as integer, byval frames as integer = 1, byval clr as integer = NO, byval wantmask as integer = NO) as Frame ptr
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
			'the caller to frame_new is considered to have a ref to the head; and the head to have a ref to each other elem
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
				frame_freemem(ret)
				return 0
			end if
		end with
	next
	return ret
end function

'Create a frame which is a view onto part of a larger frame
'Can return a zero-size view. Seems to work, but not yet sure that all operations will work correctly on such a frame.
function frame_new_view(byval spr as Frame ptr, byval x as integer, byval y as integer, byval w as integer, byval h as integer) as Frame ptr
	dim ret as frame ptr = callocate(sizeof(Frame))

	if ret = 0 then
		debug "Could not create sprite view, no memory"
		return 0
	end if

	if x < 0 then w -= -x: x = 0
	if y < 0 then h -= -y: y = 0
	with *ret
		.w = bound(w, 0, spr->w - x)
		.h = bound(h, 0, spr->h - y)
		if x >= spr->w or y >= spr->h or .w = 0 or .h = 0 then
			'this might help to keep things slightly saner
			.w = 0
			.h = 0
		end if
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
' You should never need to call this: use frame_unload
' Should only be called on the head of an array (and not a view, obv)!
' Warning: not all code calls frame_freemem to free sprites! Grrr!
private sub frame_freemem(byval f as frame ptr)
	if f = 0 then exit sub
	if f->arrayelem then debug "can't free arrayelem!": exit sub
	for i as integer = 0 to f->arraylen - 1
		deallocate(f[i].image)
		deallocate(f[i].mask)
		f[i].image = NULL
		f[i].mask = NULL
		f[i].refcount = FREEDREFC  'help to detect double free
	next
	'spriteset_freemem also calls frame_freemem
	if f->sprset then
		f->sprset->frames = NULL
		spriteset_freemem f->sprset
	end if
	deallocate(f)
end sub

'Public:
' Loads a 4-bit sprite (stored in columns (2/byte)) from one of the .pt? files, with caching.
' It will return a pointer to the first frame, and subsequent frames
' will be immediately after it in memory. (This is a hack, and will probably be removed)
function frame_load(byval ptno as integer, byval rec as integer) as frame ptr
	dim ret as Frame ptr
	dim key as integer = ptno * 1000000 + rec

	if ptno < 0 or rec < 0 then
		debug "frame_load: invalid ptno=" & ptno & " and rec=" & rec
		return 0
	end if

	ret = sprite_fetch_from_cache(key)
	if ret then return ret

	with sprite_sizes(ptno)
		'debug "loading " & ptno & "  " & rec
		'cachemiss += 1
		ret = frame_load(game + ".pt" & ptno, rec, .frames, .size.w, .size.h)
		if ret = 0 then return 0
	end with

	sprite_add_cache(key, ret)
	return ret
end function

function tileset_load(byval num as integer) as Frame ptr
	dim ret as Frame ptr
	dim key as integer = 100000000 + num

	ret = sprite_fetch_from_cache(key)
	if ret then return ret

	'debug "loading tileset" & ptno & "  " & rec
	'cachemiss += 1

	dim mxs as Frame ptr
	mxs = loadmxs(game + ".til", num)
	if mxs = NULL then return NULL
	ret = frame_to_tileset(mxs)
	frame_unload @mxs

	if ret then sprite_add_cache(key, ret)
	return ret
end function

' You can use this to load a .pt?-format 4-bit sprite from some non-standard location.
' No code does this. Does not use a cache.
' It will return a pointer to the first frame (of num frames), and subsequent frames
' will be immediately after it in memory. (This is a hack, and will probably be removed)
function frame_load(fi as string, byval rec as integer, byval num as integer, byval wid as integer, byval hei as integer) as frame ptr
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
	ret = frame_new(wid, hei, num)
	
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
' A note on frame arrays: you may pass around pointers to frames in it (call frame_reference
' on them) and then unload them, but no memory will be freed until the head pointer refcount reaches 0.
' The head element will have 1 extra refcount if the frame array is in the cache. Each of the non-head
' elements also have 1 refcount, indicating that they are 'in use' by the head element,
' but this is just for feel-good book keeping
sub frame_unload(byval p as frame ptr ptr)
	if p = 0 then exit sub
	if *p = 0 then exit sub

	if clippedframe = *p then clippedframe = 0
	with **p
		if .refcount <> NOREFC then
			if .refcount = FREEDREFC then
				debug frame_describe(*p) & " already freed!"
				*p = 0
				exit sub
			end if
			.refcount -= 1
			if .refcount < 0 then debug frame_describe(*p) & " has refcount " & .refcount
		end if
		'if cached, can free two references at once
		if (.refcount - .cached) <= 0 then
			if .arrayelem then
				'this should not happen, because each arrayelem gets an extra refcount
				debug "arrayelem with refcount = " & .refcount
				exit sub
			end if
			if .isview then
				frame_unload @.base
				deallocate(*p)
			else
				for i as integer = 1 to .arraylen - 1
					if (*p)[i].refcount <> 1 then
						debug frame_describe(*p + i) & " array elem freed with bad refcount"
					end if
				next
				if .cached then sprite_to_B_cache((*p)->cacheentry) else frame_freemem(*p)
			end if
		end if
	end with
	*p = 0
end sub

function frame_describe(byval p as frame ptr) as string
	if p = 0 then return "'(null)'"
	dim temp as string
	if p->sprset then
		temp = "spriteset:<" & p->sprset->numframes & " frames: 0x" & hex(p->sprset->frames) _
		       & "," & p->sprset->numanimations & " animations: 0x" & hex(p->sprset->animations) & ">"
	end if
	return "'(0x" & hex(p) & ") " & p->arraylen & "x" & p->w & "x" & p->h & " img=0x" & hex(p->image) _
	       & " msk=0x" & hex(p->mask) & " pitch=" & p->pitch & " cached=" & p->cached & " aelem=" _
	       & p->arrayelem & " view=" & p->isview & " base=0x" & hex(p->base) & " refc=" & p->refcount & "' " _
	       & temp
end function

'this is mostly just a gimmick
function frame_is_valid(byval p as frame ptr) as integer
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
		debug "Invalid sprite " & frame_describe(p)
		'if we get here, we are probably doomed, but this might be a recovery
		if p->cacheentry then sprite_remove_cache(p->cacheentry)
	end if
	return ret
end function

'for a copy you intend to modify. Otherwise use frame_reference
'note: does not copy frame arrays, only single frames
function frame_duplicate(byval p as frame ptr, byval clr as integer = 0, byval addmask as integer = 0) as frame ptr
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

function frame_reference(byval p as frame ptr) as frame ptr
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
sub frame_draw(byval src as frame ptr, Byval pal as Palette16 ptr = NULL, Byval x as integer, Byval y as integer, Byval scale as integer = 1, Byval trans as integer = -1, byval page as integer)
	if src = 0 then
		debug "trying to draw null frame"
		exit sub
	end if

	frame_draw src, pal, x, y, scale, trans, vpages(page)
end sub

sub frame_draw(byval src as Frame ptr, Byval pal as Palette16 ptr = NULL, Byval x as integer, Byval y as integer, Byval scale as integer = 1, Byval trans as integer = -1, byval dest as Frame ptr)
	if dest <> clippedframe then
		setclip , , , , dest
	end if

	if scale = 1 then
		drawohr src, dest, pal, x, y, trans
		exit sub
	end if

	dim as integer sxfrom, sxto, syfrom, syto
	
	sxfrom = large(clipl, x)
	sxto = small(clipr, x + (src->w * scale) - 1)
	
	syfrom = large(clipt, y)
	syto = small(clipb, y + (src->h * scale) - 1)
	
	blitohrscaled (src, dest, pal, x, y, sxfrom, syfrom, sxto, syto, trans, scale)
end sub

'Public:
' Returns a (copy of the) sprite (any bitdepth) in the midst of a given fade out.
' tlength is the desired length of the transition (in any time units you please),
' t is the number of elasped time units. style is the specific transition.
function frame_dissolved(byval spr as frame ptr, byval tlength as integer, byval t as integer, byval style as integer) as frame ptr
	if t > tlength then return frame_duplicate(spr, YES)

	'by default, sprites use colourkey transparency instead of masks.
	'We could easily not use a mask here, but by using one, this function can be called on 8-bit graphics
	'too; just in case you ever want to fade out a backdrop or something?
	dim startblank as integer = (style = 8 or style = 9)
	dim cpy as frame ptr
	cpy = frame_duplicate(spr, startblank, 1)
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
			dim vspeed as integer = large(cint(cpy->h / tlength), 1)
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
		case 8 'squeeze (horizontal squash)
			dim destx(spr->w - 1) as integer
			for sx = 0 to spr->w - 1
				destx(sx) = sx * (1 - t / tlength) + 0.5 * (spr->w - 1) * (t / tlength)
			next
			for sy = 0 to spr->h - 1
				dim destimage as ubyte ptr = cpy->image + sy * cpy->pitch
				dim destmask as ubyte ptr = cpy->mask + sy * cpy->pitch
				dim srcmask as ubyte ptr = iif(spr->mask, spr->mask, spr->image)
				dim poffset as integer = sy * cpy->pitch
				for sx = 0 to spr->w - 1
					destimage[destx(sx)] = spr->image[poffset]
					destmask[destx(sx)] = srcmask[poffset]
					poffset += 1
				next
			next
		case 9 'shrink (horizontal+vertical squash)
			dim destx(spr->w - 1) as integer
			for sx = 0 to spr->w - 1
				destx(sx) = sx * (1 - t / tlength) + 0.5 * (spr->w - 1) * (t / tlength)
			next
			for sy = 0 to spr->h - 1
				dim desty as integer = sy * (1 - t / tlength) + (spr->h - 1) * (t / tlength)
				dim destimage as ubyte ptr = cpy->image + desty * cpy->pitch
				dim destmask as ubyte ptr = cpy->mask + desty * cpy->pitch
				dim srcmask as ubyte ptr = iif(spr->mask, spr->mask, spr->image)
				dim poffset as integer = sy * cpy->pitch
				for sx = 0 to spr->w - 1
					destimage[destx(sx)] = spr->image[poffset]
					destmask[destx(sx)] = srcmask[poffset]
					poffset += 1
				next
			next
		case 10 'flicker
			dim state as integer = 0
			dim ctr as integer  'percent
			for i = 0 to t
				dim cutoff as integer = 60 * (1 - i / tlength) + 25 * (i / tlength)
				dim inc as integer = 60 * i / tlength
				ctr += inc
				if ctr > cutoff then
					i += ctr \ cutoff  'length of gaps increases
					if i > t then state = 1
					ctr = ctr mod 100
				end if
			next
			if state then frame_clear(cpy)
	end select

	return cpy
end function

function default_dissolve_time(byval style as integer, byval w as integer, byval h as integer) as integer
	'squash, vapourise, phase out, squeeze
	if style = 4 or style = 6 or style = 7 or style = 8 or style = 9 then
		return w / 5
	else
		return w / 2
	end if
end function

'Used by frame_flip_horiz and frame_flip_vert
private sub flip_image(byval pixels as ubyte ptr, byval d1len as integer, byval d1stride as integer, byval d2len as integer, byval d2stride as integer)
	for x1 as integer = 0 to d1len - 1
		dim as ubyte ptr pixelp = pixels + x1 * d1stride
		for offset as integer = (d2len - 1) * d2stride to 0 step -2 * d2stride
			dim as ubyte temp = pixelp[0]
			pixelp[0] = pixelp[offset]
			pixelp[offset] = temp
			pixelp += d2stride
		next
	next
end sub

'not-in-place isometric transformation of a pixel buffer
'dimensions/strides of source is taken from src, but srcpixels specifies the actual pixel buffer
'destorigin points to the pixel in the destination buffer where the pixel at the (top left) origin should be put
private sub transform_image(byval src as Frame ptr, byval srcpixels as ubyte ptr, byval destorigin as ubyte ptr, byval d1stride as integer, byval d2stride as integer)
	for y as integer = 0 to src->h - 1
		dim as ubyte ptr sptr = srcpixels + y * src->pitch
		dim as ubyte ptr dptr = destorigin + y * d1stride
		for x as integer = 0 to src->w - 1
			*dptr = sptr[x]
			dptr += d2stride
		next
	next
end sub

'Public:
' flips a sprite horizontally. In place: you are only allowed to do this on sprites with no other references
sub frame_flip_horiz(byval spr as frame ptr)
	if spr = 0 then exit sub
	
	if spr->refcount > 1 then
		debug "illegal hflip on " & frame_describe(spr)
		exit sub
	end if

	flip_image(spr->image, spr->h, spr->pitch, spr->w, 1)
	if spr->mask then
		flip_image(spr->mask, spr->h, spr->pitch, spr->w, 1)
	end if
end sub

'Public:
' flips a sprite vertically. In place: you are only allowed to do this on sprites with no other references
sub frame_flip_vert(byval spr as frame ptr)
	if spr = 0 then exit sub
	
	if spr->refcount > 1 then
		debug "illegal vflip on " & frame_describe(spr)
		exit sub
	end if

	flip_image(spr->image, spr->w, 1, spr->h, spr->pitch)
	if spr->mask then
		flip_image(spr->mask, spr->w, 1, spr->h, spr->pitch)
	end if
end sub

'90 degree (anticlockwise) rotation. Unlike flipping functions, non-destructive!
function frame_rotated_90(byval spr as Frame ptr) as Frame ptr
	if spr = 0 then return NULL

	dim ret as Frame ptr = frame_new(spr->h, spr->w, 1, (spr->mask <> NULL))

	'top left corner transformed to bottom left corner
	transform_image(spr, spr->image, ret->image + ret->pitch * (ret->h - 1), 1, -ret->pitch)

	if spr->mask <> NULL then
		transform_image(spr, spr->mask, ret->mask + ret->pitch * (ret->h - 1), 1, -ret->pitch)
	end if

	return ret
end function

'270 degree (anticlockwise) rotation. Unlike flipping functions, non-destructive!
function frame_rotated_270(byval spr as Frame ptr) as Frame ptr
	if spr = 0 then return NULL

	dim ret as Frame ptr = frame_new(spr->h, spr->w, 1, (spr->mask <> NULL))

	'top left corner transformed to top right corner
	transform_image(spr, spr->image, ret->image + (ret->w - 1), -1, ret->pitch)

	if spr->mask <> NULL then
		transform_image(spr, spr->mask, ret->mask + (ret->w - 1), -1, ret->pitch)
	end if

	return ret
end function

'Note that we clear masks to transparent! I'm not sure if this is best (not currently used anywhere), but notice that
'frame_duplicate with clr=1 does the same
sub frame_clear(byval spr as frame ptr, byval colour as integer = 0)
	if spr->image then
		if spr->w = spr->pitch then
			memset(spr->image, colour, spr->w * spr->h)
		else
			for i as integer = 0 to spr->h - 1
				memset(spr->image + i * spr->pitch, colour, spr->w)
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
' function frame_scroll(byval spr as frame ptr, byval h as integer = 0, byval v as integer = 0, byval wrap as integer = 0, byval direct as integer = 0) as frame ptr

' 	dim ret as frame ptr, x as integer, y as integer
' 	
' 	ret = frame_clear(spr, -1)
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
'Note that the palette cache works completely differently to the sprite cache,
'and the palette refcounting system too!

type Palette16Cache
	s as string
	p as palette16 ptr
end type


redim shared palcache(50) as Palette16Cache

sub Palette16_delete(byval f as Palette16 ptr ptr)
	if f = 0 then exit sub
	if *f = 0 then exit sub
	(*f)->refcount = FREEDREFC  'help detect double frees
	deallocate(*f)
	*f = 0
end sub

'Completely empty the palette16 cache
'palettes aren't uncached either when they hit 0 references
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

function Palette16_find_cache(s as string) as Palette16Cache ptr
	dim i as integer
	for i = 0 to ubound(palcache)
		if palcache(i).s = s then return @palcache(i)
	next
	return NULL
end function

sub Palette16_add_cache(s as string, byval p as Palette16 ptr, byval fr as integer = 0)
	if p = 0 then exit sub
	if p->refcount = NOREFC then
		'sanity check
		debug "Tried to add a non-refcounted Palette16 to the palette cache!"
		exit sub
	end if

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

function palette16_new() as palette16 ptr
	dim ret as palette16 ptr
	'--create a new palette which is not connected to any data file
	ret = callocate(sizeof(palette16))
	'--noncached palettes should be deleted when they are unloaded
	ret->refcount = NOREFC
	return ret
end function

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

function palette16_load(fil as string, byval num as integer, byval autotype as integer = 0, byval spr as integer = 0) as palette16 ptr
	dim f as integer, ret as palette16 ptr
	dim hashstring as string
	dim cache as Palette16Cache ptr
	if num > -1 then
		hashstring = trimpath(fil) & "#" & num
	else
		num = getdefaultpal(autotype, spr)
		if num <> -1 then
			hashstring = trimpath(fil) & "#" & num
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
	if (*p)->refcount = NOREFC then
		'--noncached palettes should be deleted when they are unloaded
		Palette16_delete(p)
	else
		(*p)->refcount -= 1
		'debug "unloading palette (" & ((*p)->refcount) & " more copies!)"
	end if
	*p = 0
end sub

'update a .pal-loaded palette even while in use elsewhere. Notice that sprites don't have anything like this.
sub Palette16_update_cache(fil as string, byval num as integer)
	dim oldpal as Palette16 ptr
	dim hashstring as string
	dim cache as Palette16Cache ptr

	hashstring = trimpath(fil) & "#" & num
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

'-------------- SpriteSet and SpriteState routines -----------------

function spriteset_load_from_pt(byval ptno as integer, byval rec as integer) as SpriteSet ptr
	dim frameset as Frame ptr
	frameset = frame_load(ptno, rec)
	if frameset = NULL then return NULL

	if frameset->sprset = NULL then
		'this Frame array was previously loaded using frame_load; add SpriteSet data
		frameset->sprset = new SpriteSet
		with *frameset->sprset
			.numframes = sprite_sizes(ptno).frames
			.frames = frameset
			'TODO: should pt? records have default animations?
		end with
	end if

	return frameset->sprset
end function

private sub spriteset_freemem(byval sprset as SpriteSet ptr)
	'frame_freemem also calls spriteset_freemem
	if sprset->frames then
		sprset->frames->sprset = NULL
		frame_freemem sprset->frames
	end if
	deallocate sprset->animations
	delete sprset
end sub

sub spriteset_unload(byref ss as SpriteSet ptr)
	'a SpriteSet and its Frame array are never unloaded separately;
	'frame_unload is responsible for all refcounting and unloading
	if ss = NULL then exit sub
	dim temp as Frame ptr = ss->frames
	frame_unload @temp
	ss = NULL
end sub

function sprite_load(byval ptno as integer, byval rec as integer, byval palno as integer = -1) as SpriteState ptr
	dim sprset as SpriteSet ptr = spriteset_load_from_pt(ptno, rec)
	if sprset = NULL then return NULL

	dim ret as SpriteState ptr
	ret = allocate(sizeof(SpriteState))
	with *ret
		.set = sprset
		.pal = palette16_load(palno, ptno, rec)
		.frame_id = 0
		.curframe = @sprset->frames[.frame_id]
	end with
	return ret
end function

sub sprite_unload(byval spr as SpriteState ptr ptr)
	if spr = NULL then exit sub
	if *spr = NULL then exit sub
	spriteset_unload((*spr)->set)
	palette16_unload(@(*spr)->pal)
	deallocate *spr
	*spr = NULL
end sub

'loop is number of times to play, or <=0 for infinite
sub sprite_play_animation(spr as SpriteState ptr, anim_name as string, loopcount as integer = 1)
	spr->anim_wait = 0
	spr->anim_step = 0
	spr->anim_loop = loopcount - 1
	dim animp as Animation ptr = spr->set->animations
	for i as integer = 0 to spr->set->numanimations - 1
		if animp->name = anim_name then
			spr->anim = animp
			exit sub
		end if
		animp += 1
	next
	debug "Could not find animation '" & anim_name & "'"
end sub

sub sprite_animate(spr as SpriteState ptr)
	with *spr
		if .anim = NULL then exit sub

		dim looplimit as integer = 40
		do
			if .anim_step >= .anim->numitems then
				if .anim_loop = 0 then 
					.anim = NULL
					exit sub
				end if
				if .anim_loop > 0 then .anim_loop -= 1
				.anim_step = 0
			end if

			dim op as AnimationOp ptr = @.anim->ops[.anim_step]
			select case op->type
				case animOpWait
					.anim_wait += 1
					if .anim_wait > op->arg1 then
						.anim_wait = 0
					else
						exit do
					end if
				case animOpFrame
					if op->arg1 >= .set->numframes then
						debug "Animation '" & .anim->name & "': illegal frame number " & op->arg1
						.anim = NULL
						exit sub
					end if
					.frame_id = op->arg1
					.curframe = @.set->frames[.frame_id]
				case animOpSetOffset
					.offset.x = op->arg1
					.offset.y = op->arg2
				case animOpRelOffset
					.offset.x += op->arg1
					.offset.y += op->arg2
				case else
					debug "bad animation opcode " & op->type & " in '" & .anim->name & "'"
					.anim = NULL
					exit sub
			end select
			.anim_step += 1

			looplimit -= 1
			if looplimit = 0 then .anim = NULL: exit do
		loop
	end with
end sub

sub sprite_draw(spr as SpriteState ptr, byval x as integer, byval y as integer, byval scale as integer = 1, byval trans as integer = -1, byval page as integer)
	dim as integer realx, realy
	realx = x + spr->curframe->offset.x + spr->offset.x
	realy = y + spr->curframe->offset.y + spr->offset.y
	frame_draw(spr->curframe, spr->pal, realx, realy, scale, trans, page)
end sub
