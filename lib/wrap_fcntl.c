/* This file is only used on GNU/Linux (ie glibc) when compiling with portable=1,
   for compatibility with glibc < 2.28 when compiling with glibc >= 2.28 (2018-08-01).
   fcntl and fcntl64 (used in libfb) are redirected to __wrap_fcntl by ld (see SConscript).

   Placed in the public domain.
*/

#include "../fb/fb_config.h"  // defines HOST_64BIT
#include <fcntl.h>
#include <stdarg.h>

#ifdef HOST_64BIT
// x86_64 glibc... I'm guessing ARM64, etc, has the same versioned fcntl symbols
asm (".symver fcntl, fcntl@GLIBC_2.2.5");
#else
// x86 glibc... I'm guessing ARM32, etc, has the same versioned fcntl symbols
asm (".symver fcntl, fcntl@GLIBC_2.0");
#endif

int __wrap_fcntl(int fd, int cmd, ...)
{
    // fcntl has 2 or 3 args, and I don't know whether it's safe to 
    // just define it with 3... glibc itself always seems to access that arg
    // as a pointer using va_arg, although the man page says it can be an int!
    va_list va;
    va_start(va, cmd);
    return fcntl(fd, cmd, va_arg(va, void*));
    va_end(va);
}

// fcntl64 was only added in glibc 2.28. It is used on a 32-bit system only if
// you #define _FILE_OFFSET_BITS 64 (which libfb does do). Unlike other 64-bit
// variant functions there are no off_t's involved; fcntl64 was added to fix
// some problem with large files.
// (See https://savannah.gnu.org/forum/forum.php?forum_id=9205)
// So... I assume we can just map fcntl64 to fcntl - of course, we can't call
// fcntl64.
// Note that fcntl64 only appears if libfb was compiled against glibc 2.28+.

int __wrap_fcntl64(int fd, int cmd, ...)
{
    va_list va;
    va_start(va, cmd);
    return fcntl(fd, cmd, va_arg(va, void*));
    va_end(va);
}
