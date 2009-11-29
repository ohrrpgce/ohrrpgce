'OHHRPGCE COMMON - Lumped file format routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#ifndef LUMPFILE_BI
#define LUMPFILE_BI

#include "const.bi"

enum Lumptype
	LT_LUMPED
	LT_FILE
	'LT_FIXEDREC
	'LT_DIRTYMAP
	'LT_SUBLUMP
	LT_NUM
end enum

'forward declarations
type LumpPtr as Lump ptr
type LumpIndexPtr as LumpIndex ptr

type LumpedLump
	type as Lumptype
	lumpname as string
	length as integer
	bucket_chain as LumpPtr
	next as LumpPtr
	index as LumpIndexPtr

	'usual FB start-from-one offset of start of data for this lump
	offset as integer
end type

type FileLump
	type as Lumptype
	lumpname as string
	length as integer
	bucket_chain as LumpPtr
	next as LumpPtr
	index as LumpIndexPtr

	'fhandle as integer
end type

type Lump
	type as Lumptype

	lumpname as string
	length as integer

	bucket_chain as LumpPtr
	'used to iterate over lumps in order they are in the file (or whatever else you want)
	next as LumpPtr

	index as LumpIndexPtr
end type

/'
type LumpUnion
	base as BaseLump
	union 
		lumped as LumpedLump
		file as FileLump
	end union
end type
'/

type LumpIndex
	numlumps as integer
	tablesize as integer
	table as Lump ptr ptr
	first as Lump ptr
	last as Lump ptr

	'if non-zero, handle of open file
	fhandle as integer

	'if non-empty, directory containing FileLumps
	unlumpeddir as string
end type


'----------------------------------------------------------------------
'                        Lump VTable/methods


type FnLumpDestruct as sub (byref as Lump)
type FnLumpWriteToFile as sub (byref as Lump, byval as integer, byval as integer)
type FnLumpWriteChanges as sub (byref as Lump, byval as integer, byval as integer)
type FnLumpRead as function (byref as any, byval position as integer, byval bufr as any ptr, byval size as integer) as integer

type LumpVTable_t
	destruct    as FnLumpDestruct
	writetofile  as FnLumpWriteToFile
	writechanges as FnLumpWriteChanges
	read         as FnLumpRead
end type

'Upcast Lump-subclass Method Ptr
'We can't concatenate an underscore between class and method because when the FB preprocessor
'and underscores mix, massive explosions rupture your body! You die.
#define ULMP(class, method) cast(FnLump##method, @class##method)

'Because FB doesn't allow casts in array initialisers! Claims they are not constant expressions!
#define LMPVTAB(classid, destructFn, writeoutFn, writechangesFn, readFn) _
	lumpvtable(classid) = type(destructFn, writeoutFn, writechangesFn, readFn)


'----------------------------------------------------------------------
'                           Main Interface


declare sub destruct_LumpIndex(byref this as LumpIndex)
declare function LumpIndex_findlump(byref this as LumpIndex, lumpname as string) as Lump ptr
declare sub LumpIndex_debug(byref this as LumpIndex)

declare sub Lump_unlumpfile(byref this as Lump, whereto as string)
declare function Lump_read(byref this as Lump, byval position as integer, byval bufr as any ptr, byval size as integer) as integer

declare function loadrecord overload (buf() as integer, fh as integer, recordsize as integer, record as integer = -1) as integer
declare function loadrecord overload (buf() as integer, filename as string, recordsize as integer, record as integer = 0) as integer
declare sub storerecord overload (buf() as integer, fh as integer, recordsize as integer, record as integer = -1)
declare sub storerecord overload (buf() as integer, filename as string, recordsize as integer, record as integer = 0)

declare function indexunlumpeddir (whichdir as string) as LumpIndex ptr
declare function indexlumpfile (lumpfile as string, byval keepopen as integer = YES) as LumpIndex ptr
declare sub lumpfiles (listf as string, lump as string, path as string)
declare sub unlump(lump as string, ulpath as string)
declare sub unlumpfile(lump as string, fmask as string, path as string)
declare function islumpfile (lump as string, fmask as string) as integer


'----------------------------------------------------------------------
'                         Lump FileWrapper
'Here is a C stdio-style wrapper around a Lump plus file position, since
'all of FB_FILE, SDL_RWops and Audiere's File need such a wrapper

type FileWrapper
	lump as Lump ptr
	'index as LumpIndex ptr
	pos as integer
	' for FileLumps?
	'fhandle as integer
end type

declare function FileWrapper_open(byval lump as Lump ptr) as FileWrapper ptr
declare sub FileWrapper_close(byref this as FileWrapper)
'returns final position in lump. whence is one of SEEK_SET, SEEK_CUR, or SEEK_END from stdio.h
declare function FileWrapper_seek(byref this as FileWrapper, byval offset as integer, byval whence as integer) as integer
'returns number of records (size bytes), out of maxnum requested, successfully read
declare function FileWrapper_read(byref this as FileWrapper, byval bufr as any ptr, byval size as integer, byval maxnum as integer) as integer
'declare function FileWrapper_write(byref this as FileWrapper, byval bufr as any ptr, byval size as integer) as integer
'don't need tell: call seek

#endif
