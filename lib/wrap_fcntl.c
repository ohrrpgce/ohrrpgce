/* This file is only used on GNU/Linux (ie glibc) when compiling with portable=1,
   in order to produce binaries that don't depend on functions only present
   in newer glibc versions.
   Functions like fcntl are redirected by ld to __wrap_fcntl etc (see
   SConscript), defined in this file, which are wrappers around a shadowed but
   still present symbol in libc.so or libm.so.

   See https://rpg.hamsterrepublic.com/ohrrpgce/Portable_GNU-Linux_binaries
   for more info and instructions for updating.

   Placed in the public domain.
*/

#include "../fb/fb_config.h"  // defines HOST_64BIT
#include <fcntl.h>
#include <stdarg.h>
#include <math.h>

#ifdef HOST_64BIT
// x86_64 glibc... I'm guessing ARM64, etc, has the same versioned symbols
asm (".symver fcntl, fcntl@GLIBC_2.2.5");
asm (".symver pow, pow@GLIBC_2.2.5");
asm (".symver exp, exp@GLIBC_2.2.5");
asm (".symver log, log@GLIBC_2.2.5");
#else
// x86 glibc... I'm guessing ARM32, etc, has the same versioned symbols
asm (".symver fcntl, fcntl@GLIBC_2.0");
asm (".symver pow, pow@GLIBC_2.0");
asm (".symver exp, exp@GLIBC_2.0");
asm (".symver log, log@GLIBC_2.0");
#endif

// fcntl and fcntl64 are used in libfb.
// If libfb was compiled against >= 2.28 we need to wrap fcntl64, otherwise fcntl.

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

// fcntl64 was only added in glibc 2.28 (2018-08-01). It is used on a 32-bit
// system only if you #define _FILE_OFFSET_BITS 64 (which libfb does do). Unlike
// other 64-bit variant functions there are no off_t's involved; fcntl64 was
// added to fix some problem with large files.
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

// I couldn't figure out what has changed in pow, exp, log in glibc 2.29.
// Interestingly despite compiling with -fno-omit-frame-pointer, GCC
// optimises the following to a jmp anyway.

double __wrap_pow(double x, double y)
{
    return pow(x, y);
}

double __wrap_exp(double x)
{
    return exp(x);
}

double __wrap_log(double x)
{
    return log(x);
}
