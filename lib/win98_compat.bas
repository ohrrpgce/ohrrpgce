'Public domain.
'These wrappers are required when compiling with MinGW (or theoretically VC++)
'in order to run with msvcrt.dll from older Windows versions like 98.
'MinGW-w64 actually doesn't need this, it doesn't alias localtime to localtime64, etc.

#include "crt/sys/stat.bi"
#include "crt/time.bi"
#include "crt/string.bi"

extern "C"

'FB header's only have the older 32 bit time_t and time functions,
'unlike VC++ and MinGW, which alias time_t and the time functions
'to their 64-bit versions. MingGW-w64 doesn't do this, oddly.

type time64_t as ulongint

function _localtime64  (t as time64_t ptr) as tm ptr
	dim time32 as time_t = *t
	return localtime(@time32)
end function

function _time64 (tloc as time64_t ptr) as time64_t
	dim time32 as time_t = time_()
	if tloc then *tloc = time32
	return time32
end function

type _stat64_t
	st_dev as _dev_t
	st_ino as _ino_t
	st_mode as _mode_t
	st_nlink as short
	st_uid as short
	st_gid as short
	st_rdev as _dev_t
	st_size as _off_t
	st_atime as time64_t
	st_mtime as time64_t
	st_ctime as time64_t
end type

function _stat64(byval fname as zstring ptr, byval s64 as _stat64_t ptr) as long
	dim s32 as _stat
	dim ret as long = _stat(fname, @s32)
	memcpy s64, @s32, offsetof(_stat, st_atime)
	s64->st_atime = s32.st_atime
	s64->st_mtime = s32.st_mtime
	s64->st_ctime = s32.st_ctime
	return ret
end function

end extern
