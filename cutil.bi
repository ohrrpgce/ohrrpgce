'Header for misc functions implemented in C files

#ifndef CUTIL_BI
#define CUTIL_BI

extern "C"

'type integer as size_t
type bool as integer


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

end extern

#endif