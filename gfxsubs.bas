''
'' gfxsubs.bas - Graphics utility functions to supplement all graphics backends
''
'' Please read LICENSE.txt for GPL License details and disclaimer of liability
''

#include "gfx.bi"
#include once "crt.bi"


sub smoothzoomblit_8_to_8bit(byval rptr as ubyte ptr, byval dptr as ubyte ptr, byval w as integer, byval h as integer, byval pitch as integer, byval zoom as integer, byval smooth as integer)
'rptr: source w x h buffer paletted 8 bit
'dptr: destination scaled buffer pitch x h*zoom also 8 bit
'supports zoom 1 to 4

	dim sptr as ubyte ptr
	dim as integer mult = 1
	dim as integer i, j
	dim as integer fx, fy, p0, p1, p2, p3, p4, pstep'for 2x/3x filtering
	dim as integer wide = w * zoom, high = h * zoom


	sptr = dptr

	if zoom = 1 then
		for i = 0 to h - 1 
			memcpy(sptr, rptr, w)
			rptr += pitch
			sptr += w
		next
	else
		for i = 2 to zoom
			mult = mult shl 8 + 1
		next

		for j = 0 to h - 1
			for i = w \ 4 - 1 to 0 step -1
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
			for i = (w mod 4) - 1 to 0 step -1
				*cast(integer ptr, sptr) = rptr[0] * mult
				sptr += zoom
				rptr += 1
			next
			sptr += pitch - wide
			'repeat row zoom times
			for i = 2 to zoom
				memcpy sptr, sptr - pitch, wide
				sptr += pitch
			next
		next
	end if

	if smooth = 1 and (zoom = 2 or zoom = 3) then
		if zoom = 3 then pstep = 1 else pstep = 2
		dim as ubyte ptr sptr1, sptr2, sptr3
		for fy = 1 to (high - 2) step pstep
			sptr1 = dptr + pitch * (fy - 1) + 1  '(1,0)
			sptr2 = sptr1 + pitch '(1,1)
			sptr3 = sptr2 + pitch '(1,2)
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
		next
	end if


end sub

sub smoothzoomblit_8_to_32bit(byval rptr as ubyte ptr, byval dptr as ubyte ptr, byval w as integer, byval h as integer, byval pitch as integer, byval zoom as integer, byval smooth as integer, byval pal as integer ptr)
'rptr: source w x h buffer paletted 8 bit
'dptr: destination scaled buffer pitch x h*zoom 32 bit (so pitch is in pixels, not bytes)
'supports zoom 1 to 4

	dim sptr as integer ptr
	dim as integer i, j
	dim as integer pixel
	dim as integer fx, fy, p0, p1, p2, p3, p4, pstep'for 2x/3x filtering
	dim as integer wide = w * zoom, high = h * zoom

	sptr = cast(integer ptr, dptr)

	for j = 0 to h - 1
		'repeat row zoom times
		for i = 0 to w - 1
			'get colour
			pixel = pal[*rptr]
			'zoom sptrs for each rptr
			for j = zoom to 1 step -1
				*sptr = pixel
				sptr += 1
			next
			rptr += 1
		next

		sptr += pitch - wide
		'repeat row zoom times
		for i = 2 to zoom
			memcpy sptr, sptr - pitch, 4 * wide
			sptr += pitch
		next
	next
	if smooth = 1 and (zoom = 2 or zoom = 3) then
		'this is duplicated from the 8-bit smoothing code because there is no
		'efficient way to write this code as a function that would accept both ubyte ptr and integer ptr
		'added for 2x/3x filtering
		if zoom = 3 then pstep = 1 else pstep = 2
		dim as integer ptr sptr1, sptr2, sptr3
		for fy = 1 to (high - 2) step pstep
			sptr1 = cast(integer ptr, dptr) + pitch * (fy - 1) + 1  '(1,0)
			sptr2 = sptr1 + pitch '(1,1)
			sptr3 = sptr2 + pitch '(1,2)
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
		next
	end if
end sub

/'
sub smoothzoomblit_anybit(byval rptr as ubyte ptr, byval dptr as ubyte ptr, byval w as integer, byval h as integer, byval pitch as integer, byval zoom as integer, byval smooth as integer, byval bpp as integer, byval pitch as integer)
'moved from gfx_sdl, it might be needed, probably just replace with 15/16 bit version of above
'no smoothing done yet
'rptr: source pitch x h buffer variable bbp
'dptr: destination scaled buffer variable bbp

	DIM AS INTEGER x, y, b, z
	DIM sptr as integer ptr

	sptr = dptr

	FOR y = h - 1 TO 0 STEP -1
		FOR x = 0 TO w - 1
			FOR z = 0 TO zoom - 1
				*sptr = *rptr
				sptr += bpp
			NEXT
			rptr += bpp
		NEXT

		sptr += pitch - x * zoom
		'repeat row zoom times
		for i = 2 to zoom
			memcpy sptr, sptr - pitch, 4 * wide
			sptr += pitch
		NEXT
	NEXT
end sub
'/