#pragma once

'#include once "crt/stdint.bi"

extern "C"

#define SHA1_H

type SHA1_CTX
	state(0 to 4) as ulong
	count(0 to 1) as ulong
	buffer(0 to 63) as ubyte
end type

type SHA160 as zstring * 20

declare sub SHA1Transform(byval state as ulong ptr, byval buffer as const ubyte ptr)
declare sub SHA1Init(byval context as SHA1_CTX ptr)
declare sub SHA1Update(byval context as SHA1_CTX ptr, byval data as const ubyte ptr, byval len as ulong)
declare sub SHA1Final(byval digest as SHA160 ptr, byval context as SHA1_CTX ptr)
declare sub SHA1(byval hash_out as SHA160 ptr, byval str as const zstring ptr, byval len as long)
declare function int64_to_bigendian(byval v as ulongint) as ulongint
declare function SHA1_64(byval str as const zstring ptr, byval len as long) as ulongint

end extern
