' OHRRPGCE - Minimal set of wchar_t <-> UTF8 Unicode routines
' See unicode.c for license information

#pragma once

'#include once "crt/stdint.bi"
#include once "crt/wchar.bi"

extern "C"

declare function utf8_length(byval s as const ubyte ptr) as long
declare function utf8_decode(byval input as const ubyte ptr, byval length as long ptr = NULL) as wstring ptr
declare function utf8_offset(byval s as const zstring ptr, byval charnum as long) as long
declare function utf8_charnum(byval s as const zstring ptr, byval offset as long) as long
declare function utf8_charlen(byval ch as ulong) as long
declare function utf8_encode_char(byval dest as zstring ptr, byval ch as ulong) as long
declare function utf8_encode(byval input as const wstring ptr, byval input_len as long, byval length as long ptr) as zstring ptr
declare function partially_normalise_unicode(byval input as const wstring ptr, byval output as wstring ptr, byval outsize as long) as long
declare function wstring_to_latin1(byval input as const wstring ptr, byval output as ubyte ptr, byval outsize as long) as long

end extern
