'OHHRPGCE COMMON - Lumped file format routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#ifndef LUMPFILE_BI
#define LUMPFILE_BI

declare function loadrecord overload (buf() as integer, fh as integer, recordsize as integer, record as integer = -1) as integer
declare function loadrecord overload (buf() as integer, filename as string, recordsize as integer, record as integer = 0) as integer
declare sub storerecord overload (buf() as integer, fh as integer, recordsize as integer, record as integer = -1)
declare sub storerecord overload (buf() as integer, filename as string, recordsize as integer, record as integer = 0)

declare sub lumpfiles (listf as string, lump as string, path as string)
declare sub unlump(lump as string, ulpath as string)
declare sub unlumpfile(lump as string, fmask as string, path as string)
declare function islumpfile (lump as string, fmask as string) as integer

#endif
