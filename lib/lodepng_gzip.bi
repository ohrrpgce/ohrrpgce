#pragma once

extern "C"

#define LODEPNG_GZIP_H

enum
	GZ_SUCCESS
	GZ_BADCALL = -1
	GZ_CORRUPT = -2
	GZ_UNSUPPORTED = -3
end enum

declare function compress_gzip(byval in as const ubyte ptr, byval insize as size_t, byval outp as ubyte ptr ptr, byval outsizep as size_t ptr, byval maxcomp as long = 0) as long
declare function decompress_gzip(byval in as const ubyte ptr, byval insize as size_t, byval outp as ubyte ptr ptr, byval outsizep as size_t ptr) as long
declare function gzip_error_text(byval code as long) as const zstring ptr

end extern
