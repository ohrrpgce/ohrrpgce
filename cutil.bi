'Header for misc functions implemented in C files

#ifndef CUTIL_BI
#define CUTIL_BI

extern "C"

' For size_t
#include "crt/stddef.bi"

'In miscc.c
declare function get_sys_err_string() as zstring ptr

declare function stringhash(byval strp as zstring ptr, byval leng as integer) as unsigned integer


'In base64.c
declare function isbase64 (byval ch as byte) as bool

declare sub base64_encode (byval in as byte ptr, byval inlen as size_t, _
                           byval out as byte ptr, byval outlen as size_t)

declare function base64_encode_alloc (byval in as byte ptr, byval inlen as size_t, _
                                      byval out as byte ptr ptr) as size_t

declare function base64_decode (byval in as byte ptr, byval inlen as size_t, _
                                byval out as byte ptr, byval outlen as size_t ptr) as bool

declare function base64_decode_alloc (byval in as byte ptr, byval inlen as size_t, _
                                      byval out as byte ptr ptr, byval outlen as size_t ptr) as bool


'In unicode.c
declare function utf8_length(byval in as ubyte ptr) as ssize_t

declare function utf8_decode(byval in as ubyte ptr, byval length as ssize_t ptr = NULL) as wstring ptr

declare sub wstring_to_latin1(byval input as wstring ptr, byval output as ubyte ptr, byval outsize as ssize_t)

end extern

#endif