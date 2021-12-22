'Base64 encoder/decoder
'(C) Copyright 2021 Joe King, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#include once "base64.bi"

function base64encode( decoded as const string ) as string

	dim as string encoded
	dim as integer six, n, a, b, c, k, lendec, padding, offset

	static chars as string * 64 = _
	       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" _
	       "abcdefghijklmnopqrstuvwxyz" _
	       "0123456789+/"

	lendec = len( decoded )
	encoded = string( ( (lendec + 2) \ 3 ) * 4 , "=" )
	' Uncomment to omit padding
	'encoded = space( ( lendec * 4 + 2 ) \ 3 )
	for n = 0 to lendec - 1 step 3
		a = decoded[n]
		b = decoded[n + 1]
		c = decoded[n + 2]
		padding = n + 3 - lendec
		if padding < 0 then padding = 0
		for k = 0 to 3 - padding
			select case k
				case 0: six = ( ( &b11111100 and a ) shr 2 )
				case 1: six = ( ( &b00000011 and a ) shl 4 ) or ( ( b and &b11110000 ) shr 4 )
				case 2: six = ( ( &b00001111 and b ) shl 2 ) or ( ( c and &b11000000 ) shr 6 )
				case 3: six = ( ( &b00111111 and c ) )
			end select
			encoded[offset] = chars[six]
			offset += 1
		next k
	next n

	return encoded

end function

' Accepts any padding character, not just '=', and also unpadded base64 strings.
' Returns "" if the input is invalid: contains any invalid character anywhere except the end.
' Newlines must be stripped by the caller.
function base64decode( encoded as const string ) as string

	dim as string decoded
	dim as integer eight, n, k, vals(3), lenenc, offset, byt

	lenenc = len( encoded )
	' if right( encoded, 1 ) = "=" then lenenc -= 1
	' if right( encoded, 2 ) = "==" then lenenc -= 1
	' May overestimate the length by 1 or 2, trimmed after
	decoded = space( (3 * lenenc) \ 4 )

	for n = 0 to lenenc - 1 step 4
		for k = 0 to 3
			vals(k) = -1
			if n + k >= lenenc then exit for
			byt = encoded[n + k]
			select case byt
				case asc("A") to asc("Z"): vals(k) = byt - asc("A")
				case asc("a") to asc("z"): vals(k) = byt - asc("a") + 26
				case asc("0") to asc("9"): vals(k) = byt - asc("0") + 52
				case asc("+"):             vals(k) = 62
				case asc("/"):             vals(k) = 63
			end select
		next k
		if ( vals(0) = -1 ) or ( vals(1) = -1 ) then return ""

		for k = 0 to 2
			select case k
				case 0: eight = ( ( &b111111 and vals(0) ) shl 2 ) or ( ( &b110000 and vals(1) ) shr 4 )
				case 1: if vals(2) = -1 then exit for, for
					eight = ( ( &b001111 and vals(1) ) shl 4 ) or ( ( &b111100 and vals(2) ) shr 2 )
				case 2: if vals(3) = -1 then exit for, for
					eight = ( ( &b000011 and vals(2) ) shl 6 ) or ( ( &b111111 and vals(3) ) shr 0 )
			end select
			decoded[offset] = eight
			offset += 1
		next k
	next n
	' Was an invalid character encountered somewhere other than the end?
	if offset < len( decoded ) - 2 then return ""

	return left( decoded, offset )

end function
