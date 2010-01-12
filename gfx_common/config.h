#ifndef CONFIG_H
#define CONFIG_H

/* Cross-platform workarounds */

#ifndef _MSC_VER

#ifndef __cdecl
// #define __cdecl __attribute__((__cdecl__)) 
 #define __cdecl
#endif

/* replacements for Microsoft extensions (no guarantees about correctness) */

#define memcpy_s(dest, destsize, src, count)  memcpy(dest, src, count)
#define strcpy_s(dest, destsize, src)  strcpy(dest, src)
#define wcstombs_s(pReturnValue, mbstr, sizeInBytes, wcstr, count) \
  ((*(pReturnValue) = wcstombs(mbstr, wcstr, count), (*(int *)(pReturnValue) == -1) ? EINVAL : 0))
#define mbstowcs_s(pReturnValue, wcstr, sizeInWords, mbstr, count) \
  ((*(pReturnValue) = mbstowcs(wcstr, mbstr, count), (*(int *)(pReturnValue) == -1) ? EINVAL : 0)) 

#endif

#endif
