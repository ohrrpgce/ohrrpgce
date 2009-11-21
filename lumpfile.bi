'OHHRPGCE COMMON - Lumped file format routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#ifndef LUMPFILE_BI
#define LUMPFILE_BI


enum Lumptype
	LT_FILE
	LT_LUMP
end enum

type Lump
	lumpname as string
	'usual FB start-from-one offset of start of data for this lump
	offset as integer
	length as integer
	unlumpmark as integer

	bucket_chain as Lump ptr
	'used to iterate over lumps in order they are in the file (or whatever else you want)
	next as Lump ptr
end type

type LumpIndex
	numlumps as integer
	tablesize as integer
	table as Lump ptr ptr
	first as Lump ptr
	last as Lump ptr
	'if non-zero, handle of open file
	fhandle as integer
end type

declare sub destruct_lumpindex(byref this as LumpIndex)
declare function lumpindex_findlump(byref this as LumpIndex, lumpname as string) as Lump ptr
declare sub lumpindex_debug(byref this as LumpIndex)

declare function loadrecord overload (buf() as integer, fh as integer, recordsize as integer, record as integer = -1) as integer
declare function loadrecord overload (buf() as integer, filename as string, recordsize as integer, record as integer = 0) as integer
declare sub storerecord overload (buf() as integer, fh as integer, recordsize as integer, record as integer = -1)
declare sub storerecord overload (buf() as integer, filename as string, recordsize as integer, record as integer = 0)

declare function indexlumpfile (lumpfile as string, byval keepopen as integer = YES) as LumpIndex ptr
declare sub lumpfiles (listf as string, lump as string, path as string)
declare sub unlump(lump as string, ulpath as string)
declare sub unlumpfile(lump as string, fmask as string, path as string)
declare function islumpfile (lump as string, fmask as string) as integer

#endif
