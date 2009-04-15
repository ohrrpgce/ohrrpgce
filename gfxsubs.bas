''
'' gfxsubs.bas - Graphics utility functions to supplement all graphics backends
''
'' Please read LICENSE.txt for GPL License details and disclaimer of liability
''

#include "gfx.bi"
#include once "crt.bi"


sub smoothzoomblit_8bit(byval rptr as ubyte ptr, byval dptr as ubyte ptr, byval zoom as integer, byval smooth as integer)
'rptr: source 320x200 buffer paletted 8 bit
'dptr: destination scaled buffer also 8 bit

	dim sptr as ubyte ptr
	dim as integer mult = 1
	dim as integer i, j
	dim as integer fx, fy, p0, p1, p2, p3, p4, pstep'for 2x/3x filtering
	dim as integer wide = 320 * zoom, high = 200 * zoom


	sptr = dptr

	if zoom = 1 then
		memcpy sptr, rptr, wide * high
	else
		for i = 2 to zoom
			mult = mult shl 8 + 1
		next

		for j = 0 to 200 - 1
			for i = 320 / 4 - 1 to 0 step -1
				'could just multiple by &h1010101, but FB produces truly daft code for multiplication by constants
				*cast(integer ptr, sptr) = rptr[0] * mult
				sptr += zoom
				*cast(integer ptr, sptr) = rptr[1] * mult
				sptr += zoom
				*cast(integer ptr, sptr) = rptr[2] * mult
				sptr += zoom
				*cast(integer ptr, sptr) = rptr[3] * mult
				sptr += zoom
				rptr += 4
			next
			'repeat row zoom times
			for i = 2 to zoom
				memcpy sptr, sptr - wide, wide
				sptr += wide
			next
		next
	end if

	if smooth = 1 and (zoom = 2 or zoom = 3) then
		if zoom = 3 then pstep = 1 else pstep = 2
		sptr = dptr
		dim as ubyte ptr sptr1, sptr2, sptr3
		for fy = 1 to (high - 2) step pstep
			sptr1 = sptr + 1  '(1,0)
			sptr2 = sptr1 + wide '(1,1)
			sptr3 = sptr2 + wide '(1,2)
			for fx = (wide - 2) to 1 step -1
				'p0=point(fx,fy)
				'p1=point(fx-1,fy-1)'nw
				'p2=point(fx+1,fy-1)'ne
				'p3=point(fx+1,fy+1)'se
				'p4=point(fx-1,fy+1)'sw
				'if p1 = p3 then p0 = p1
				'if p2 = p4 then p0 = p2
				if sptr1[1] = sptr3[-1] then
					sptr2[0] = sptr1[1]
				else
					if sptr1[-1] = sptr3[1] then sptr2[0] = sptr1[-1]
				end if
				'pset(fx,fy),p0
				sptr1 += 1
				sptr2 += 1
				sptr3 += 1
			next
			sptr += wide
		next
	end if


end sub

sub smoothzoomblit_32bit(byval rptr as ubyte ptr, byval dptr as ubyte ptr, byval zoom as integer, byval smooth as integer, byval pal as integer ptr)
'rptr: source 320x200 buffer paletted 8 bit
'dptr: destination scaled buffer 32 bit

	dim sptr as integer ptr
	dim as integer i, j
	dim as integer pixel
	dim as integer fx, fy, p0, p1, p2, p3, p4, pstep'for 2x/3x filtering
	dim as integer wide = 320 * zoom, high = 200 * zoom

	sptr = cast(integer ptr, dptr)

	for j = 0 to 200 - 1
		'repeat row zoom times
		for i = 0 to 320 - 1
			'get colour
			pixel = pal[*rptr]
			'zoom sptrs for each rptr
			for j = zoom to 1 step -1
				*sptr = pixel
				sptr += 1
			next
			rptr += 1
		next
		for i = 2 to zoom
			memcpy sptr, sptr - wide, 4 * wide
			sptr += wide
		next
	next
	if smooth = 1 and (zoom = 2 or zoom = 3) then
		'this is duplicated from the 8-bit smoothing code because there is no
		'efficient way to write this code as a function that would accept both ubyte ptr and integer ptr
		'added for 2x/3x filtering
		if zoom = 3 then pstep = 1 else pstep = 2
		sptr = cast(integer ptr, dptr)
		dim as integer ptr sptr1, sptr2, sptr3
		for fy = 1 to (high - 2) step pstep
			sptr1 = sptr + 1  '(1,0)
			sptr2 = sptr1 + wide '(1,1)
			sptr3 = sptr2 + wide '(1,2)
			for fx = (wide - 2) to 1 step -1
				'p0=point(fx,fy)
				'p1=point(fx-1,fy-1)'nw
				'p2=point(fx+1,fy-1)'ne
				'p3=point(fx+1,fy+1)'se
				'p4=point(fx-1,fy+1)'sw
				'if p1 = p3 then p0 = p1
				'if p2 = p4 then p0 = p2
				if sptr1[1] = sptr3[-1] then
					sptr2[0] = sptr1[1]
				else
					if sptr1[-1] = sptr3[1] then sptr2[0] = sptr1[-1]
				end if
				'pset(fx,fy),p0
				sptr1 += 1
				sptr2 += 1
				sptr3 += 1
			next
			sptr += wide
		next
	end if
end sub

sub smoothzoomblit_anybit(byval rptr as ubyte ptr, byval dptr as ubyte ptr, byval zoom as integer, byval smooth as integer, byval bpp as integer, byval pitch as integer)
'moved from gfx_sdl, it might be needed, probably just replace with 15/16 bit version of above
'rptr: source pitchx200 buffer variable bbp
'dptr: destination scaled buffer variable bbp

	DIM AS INTEGER x, y, b, z
	DIM zoombuffer(pitch) AS ubyte

	FOR y = 199 TO 0 STEP -1
		FOR x = 0 TO 319
			FOR z = 0 TO zoom - 1
				FOR b = 0 TO bpp - 1
					zoombuffer((x * zoom + z) * bpp + b) = rptr[y * pitch + (x * bpp + b)]
				NEXT
			NEXT
		NEXT
		FOR z = 0 TO zoom - 1
			memcpy dptr + (y * zoom + z) * pitch, @zoombuffer(0), pitch
		NEXT
	NEXT
end sub
