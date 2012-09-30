'Header for misc functions implemented in C files

#ifndef CUTIL_BI
#define CUTIL_BI

extern "C"

#ifndef size_t
type size_t as integer
#endif


'In blit.c
declare function get_sys_err_string() as zstring ptr


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
declare function utf8_length(byval in as byte ptr) as integer

declare function utf8_decode(byval in as byte ptr, byval length as integer ptr = NULL) as wstring ptr

declare sub wstring_to_latin1(byval input as wstring ptr, byval output as byte ptr, byval outsize as integer)

end extern

#endif