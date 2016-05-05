'OHHRPGCE COMMON - Lumped file format routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#ifndef LUMPFILE_BI
#define LUMPFILE_BI

#include "util.bi"
#include "const.bi"
#include "os.bi"

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

MAKETYPE_DListItem(Lump)
MAKETYPE_DoubleList(Lump)

type LumpedLump
	type as Lumptype
	lumpname as string
	length as integer
	bucket_chain as LumpPtr
	seq as DListItem(Lump)
	index as LumpIndexPtr
	opencount as integer 'refcount

	'usual FB start-from-one offset of start of data for this lump
	offset as integer
end type

type FileLump
	type as Lumptype
	lumpname as string
	length as integer
	bucket_chain as LumpPtr
	seq as DListItem(Lump)
	index as LumpIndexPtr
	opencount as integer 'refcount

	fhandle as integer
	'if empty, file name is index->unlumpeddir + lumpname (FIXME: stupid)
	filename as string
	'temp file lumps are automatically deleted when their refcount hits 0, and the file removed
	istemp:1 as integer
end type

type Lump
	type as Lumptype

	lumpname as string
	length as integer

	bucket_chain as LumpPtr
	'used to iterate over lumps in order they are in the file (or whatever else you want)
	seq as DListItem(Lump)

	index as LumpIndexPtr

	opencount as integer 'refcount
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
	tablesize as integer
	table as Lump ptr ptr
	lumps as DoubleList(Lump)

	'if non-zero, handle of open file
	fhandle as integer

	'if non-empty, directory containing FileLumps
	unlumpeddir as string
end type


'----------------------------------------------------------------------
'                        Lump VTable/methods


type FnLumpDestruct as sub (byref as Lump)
type FnLumpOpen as sub (byref as Lump)
type FnLumpClose as sub (byref as Lump)
type FnLumpWriteToFile as sub (byref as Lump, byval as integer, byval as integer)
type FnLumpWriteChanges as sub (byref as Lump, byval as integer, byval as integer)
type FnLumpRead as function (byref as any, byval position as integer, byval bufr as any ptr, byval size as integer) as integer

type LumpVTable_t
	destruct     as FnLumpDestruct
	open         as FnLumpOpen
	close        as FnLumpClose
	writetofile  as FnLumpWriteToFile
	writechanges as FnLumpWriteChanges
	read         as FnLumpRead
end type

'(recall NULL is define'd as 0)
#define _PRE0 NULL

'Upcast Lump-subclass Method Ptr
'We can't concatenate an underscore between class and method because when the FB preprocessor
'and underscores mix, massive explosions rupture your body! You die.
#define _PREULMP(class, method) cast(FnLump##method, @class##method)

'Quick Lump-subclass Method Ptr
#define _PREQLMP(method) cast(FnLump##method, @_CONCAT(CURLUMPCLASS,method))

'Because FB doesn't allow casts in array initialisers! Claims they are not constant expressions!
#macro LMPVTAB(classid, classname, destructFn, openFn, closeFn, writeoutFn, writechangesFn, readFn)
	#define CURLUMPCLASS classname
	lumpvtable(classid) = type(_PRE##destructFn, _PRE##openFn, _PRE##closeFn, _PRE##writeoutFn, _PRE##writechangesFn, _PRE##readFn)
	#undef CURLUMPCLASS
#endmacro


'----------------------------------------------------------------------
'                           Main Interface


declare sub destruct_LumpIndex(byref this as LumpIndex)
declare function LumpIndex_findlump(byref this as LumpIndex, lumpname as string) as Lump ptr
declare sub LumpIndex_debug(byref this as LumpIndex)

declare sub Lump_open(byref this as Lump)
declare sub Lump_close(byref this as Lump)
declare function Lump_unlumpfile(byref this as Lump, whereto as string) as integer
declare function Lump_read(byref this as Lump, byval position as integer, byval bufr as any ptr, byval size as integer) as integer

declare function FileLump_tempfromlump(byref lmp as Lump) as FileLump ptr 
declare function loadrecord overload (buf() as integer, fh as integer, recordsize as integer, record as integer = -1, context as string = "") as bool
declare function loadrecord overload (buf() as integer, filename as string, recordsize as integer, record as integer = 0, expectfile as bool = YES) as bool
declare sub storerecord overload (buf() as integer, fh as integer, recordsize as integer, record as integer = -1)
declare sub storerecord overload (buf() as integer, filename as string, recordsize as integer, record as integer = 0)
declare function compare_files_by_record (differences() as integer, leftfile as string, rightfile as string, byval recordsize as integer, byval maskarray as integer ptr = NULL) as integer

declare function indexunlumpeddir (whichdir as string) as LumpIndex ptr
declare function indexlumpfile (lumpfile as string, byval keepopen as integer = YES) as LumpIndex ptr
declare function lumpfiles (filelist() as string, lumpfile as string, path as string) as string
declare sub recover_lumped_file(lumpfile as string, destpath as string = "")
declare sub unlump(lump as string, ulpath as string, showerrors as bool = YES, verbose as bool = NO)
declare sub unlumpfile(lump as string, fmask as string, path as string, showerrors as bool = YES, verbose as bool = NO)
declare sub copylump(package as string, lump as string, dest as string, byval ignoremissing as integer = NO)
declare function islumpfile (lump as string, fmask as string) as bool
declare sub fixlumporder (filelist() as string)


'----------------------------------------------------------------------
'                   Tail-buffered binary output file

'WARNING: don't add strings to this
type BufferedFile
	fh as uinteger       'FB file handle
	pos as uinteger      '0-based write position; fh's position is NOT used
	len as uinteger      'total length of the file
	buf as ubyte ptr     'of size BF_BUFSIZE
	bufStart as uinteger 'offset of the buffer in the file. The buffer always extends to the end
	filename as zstring ptr  'manually allocated!
end type

#define BF_BUFSIZE   (64 * 1024)

declare function Buffered_open(filename as string) as BufferedFile ptr
declare sub Buffered_close(byval bfile as BufferedFile ptr)
declare sub Buffered_seek(byval bfile as BufferedFile ptr, byval offset as unsigned integer)
declare function Buffered_tell(byval bfile as BufferedFile ptr) as integer
declare sub Buffered_write(byval bfile as BufferedFile ptr, byval databuf as any ptr, byval amount as integer)
declare sub Buffered_putc(byval bfile as BufferedFile ptr, byval datum as ubyte)


'----------------------------------------------------------------------
'                       filelayer.cpp stuff


' NOTE: Duplicated in filelayer.h
enum OPENBits
	' FOR RANDOM (fixed sized records) not supported. Use load/storerecord() instead.
	FOR_BINARY =        &h0010000  ' default
	FOR_INPUT =         &h0020000
	FOR_OUTPUT =        &h0040000
	FOR_APPEND =        &h0080000
	FOR_MASK =          &h00F0000
	' For files, ACCESS ANY means try READ_WRITE, failing that use READ.
	' Which sounds like a misfeature to me, so let's default to ACCESS_READ_WRITE instead.
	ACCESS_ANY =        &h0100000
	ACCESS_READ =       &h0200000
	ACCESS_WRITE =      &h0400000
	ACCESS_READ_WRITE = &h0800000  ' default
	ACCESS_MASK =       &h0F00000
	' Not implemented yet for hooked files, so no point using these
	'ENCODING_ASCII =   &h1000000  ' default
	'ENCODING_UTF8 =    &h2000000
	'ENCODING_UTF16 =   &h4000000
	'ENCODING_UTF32 =   &h8000000
	'ENCODING_MASK =    &hF000000
	' LOCK not supported... in fact it's not even properly supported by FB!
	' However it could be added (since we already have file locking implemented in os.bi)
	' if it were useful.
	' LEN (record length) not supported.
end enum

type FnStringPredicate as function (filename as string) as boolint
type FnOpenCallback as function (filename as string, writable as boolint) as boolint

extern "C"

'Replacement for OPEN (and FREEFILE) which is used to hook accesses to lumps, and send messages from Custom
'to a spawned instance of Game when a modified file is closed.
'Sets fh to a FREEFILE file number (initial value ignored).
'Returns 0 on success, 1 on error, 2 if file doesn't exist.
'Example:
'  OPEN file FOR BINARY ACCESS READ as #fh
'becomes:
'  OPENFILE(file, FOR_BINARY + ACCESS_READ, fh)
'All access flags are optional; you can pass 0.
declare function OPENFILE(filename as string, open_bits as OPENBits, byref fh as integer) as integer

declare sub send_lump_modified_msg(byval filename as zstring ptr)
declare sub set_OPEN_hook(lumpfile_filter as FnOpenCallback, lump_writes_allowed as boolint, channel as IPCChannel ptr)
declare sub clear_OPEN_hook()

end extern

declare function inworkingdir(filename as string, writable as boolint) as boolint
declare function channel_wait_for_msg(byref channel as IPCChannel, wait_for_prefix as string, line_in as string = "", timeout_ms as integer = 500) as integer

#endif
