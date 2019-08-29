'Header for misc functions implemented in C files
'FIXME: why don't we just split this in two?

#ifndef CUTIL_BI
#define CUTIL_BI

extern "C"

' For size_t
#include "crt/stddef.bi"

'In miscc.c
declare function get_sys_err_string() as zstring ptr

declare function stringhash(byval strp as zstring ptr, byval leng as integer) as unsigned integer

declare function strprintf (byval fmtstr as zstring ptr, ...) as string

declare sub init_crt ()

declare sub disable_extended_precision ()

declare function hook_fb_End() as boolint
declare sub unhook_fb_End()


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
