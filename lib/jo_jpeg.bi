#pragma once

extern "C"

type jo_write_func as sub(byval context as any ptr, byval data as const any ptr, byval size as long)

declare function jo_write_jpg(byval filename as const zstring ptr, byval data as const any ptr, byval width as long, byval height as long, byval comp as long, byval quality as long) as bool
declare function jo_write_jpg_to_func(byval func as jo_write_func, byval context as any ptr, byval data as const any ptr, byval width as long, byval height as long, byval comp as long, byval quality as long) as bool

end extern
