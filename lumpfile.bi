'OHRRPGCE - Lumped file format routines
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#ifndef LUMPFILE_BI
#define LUMPFILE_BI

#include "util.bi"
#include "const.bi"
#include "os.bi"


' Lump/FileLump/LumpedLump abstraction layer.
' Sadly not used, and probably never will be, because filelayer.cpp took a different
' approach which avoids modifying all code that uses FB files.

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
' (Not used!)

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
declare function loadrecord overload (buf() as integer, fh as integer, recordsize as integer, record as integer = -1, expect_exists as bool = YES, partial_retval as bool = NO) as bool
declare function loadrecord overload (buf() as integer, filename as string, recordsize as integer, record as integer = 0, expect_exists as bool = YES, partial_retval as bool = NO) as bool
declare sub storerecord overload (buf() as integer, fh as integer, recordsize as integer, record as integer = -1)
declare sub storerecord overload (buf() as integer, filename as string, recordsize as integer, record as integer = 0)
declare function compare_files_by_record (differences() as integer, leftfile as string, rightfile as string, byval recordsize as integer, byval maskarray as bool ptr = NULL) as bool

declare function indexunlumpeddir (whichdir as string) as LumpIndex ptr
declare function indexlumpfile (lumpfile as string, byval keepopen as bool = YES) as LumpIndex ptr
declare function lumpfiles (filelist() as string, lumpfile as string, path as string) as string
declare sub recover_lumped_file(lumpfile as string, destpath as string = "")
declare function unlump(lump as string, ulpath as string, showerrors as bool = YES, verbose as bool = NO) as bool
declare sub unlumpfile(lump as string, fmask as string, path as string, showerrors as bool = YES, verbose as bool = NO)
declare sub copylump(package as string, lump as string, dest as string, byval ignoremissing as bool = NO)
declare function islumpfile (lump as string, fmask as string) as bool
declare sub fixlumporder (filelist() as string)


'----------------------------------------------------------------------
'                         Lump FileWrapper
'Here is a C stdio-style wrapper around a Lump plus file position, since
'all of FB_FILE, SDL_RWops and Audiere's File need such a wrapper

'WARNING: don't add strings to this
type FileWrapper
	lump as Lump ptr
	'index as LumpIndex ptr
	pos as integer
	' for FileLumps?
	'fhandle as integer
end type

extern "C"

declare function FileWrapper_open(byval lump as Lump ptr) as FileWrapper ptr
declare sub FileWrapper_close(byref this as FileWrapper)
'returns final position in lump. whence is one of SEEK_SET, SEEK_CUR, or SEEK_END from stdio.h
declare function FileWrapper_seek(byref this as FileWrapper, byval offset as integer, byval whence as integer) as integer
'returns number of records (size bytes), out of maxnum requested, successfully read
declare function FileWrapper_read(byref this as FileWrapper, byval bufr as any ptr, byval size as integer, byval maxnum as integer) as integer
'declare function FileWrapper_write(byref this as FileWrapper, byval bufr as any ptr, byval size as integer, byval maxnum as integer) as integer
'don't need tell: call seek

end extern


'----------------------------------------------------------------------
'                   Access to embedded data files
' Abstraction layer for reading data files either  embedded into the exe or external.

extern "C"

type EmbeddedFileInfo
	path as const zstring ptr
	data as const zstring ptr
	length as long
end type

extern embedded_files_table as EmbeddedFileInfo ptr

declare sub list_embedded_files()
declare function find_embedded_file(byval path as const zstring ptr) as EmbeddedFileInfo ptr

type VFile as VFileFwd

declare function vfopen(byval path as const zstring ptr, byval mode as const zstring ptr) as VFile ptr
declare sub vfclose(byval file as VFile ptr)
declare function vfread(byval ptr as any ptr, byval size as uint32, byval nmemb as uint32, byval file as VFile ptr) as uint32
declare function vfgetc(file as VFile ptr) as integer
declare function vfwrite(byval ptr as const any ptr, byval size as uint32, byval nmemb as uint32, byval file as VFile ptr) as uint32
declare function vfseek(byval file as VFile ptr, byval offset as ssize_t, byval whence as long) as size_t
declare function vftell(byval file as VFile ptr) as size_t

end extern


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
	OR_ERROR =          &h0000001  ' Show an error message (showerror) if the file can't be opened
	                               ' NOTE: You must add ACCESS_READ to get an error if it doesn't exist
	' FOR RANDOM (fixed sized records) not supported. Use load/storerecord() instead.
	FOR_BINARY =        &h0010000  ' default
	FOR_INPUT =         &h0020000  ' Text files only! Reading only, not opened if doesn't exist.
	FOR_OUTPUT =        &h0040000  ' Text files only! Write only. File created, or truncated if exists.
	FOR_APPEND =        &h0080000  ' Text files only! Write only. All writes always happen at the end
	FOR_BITMASK =       &h00F0000
	' ACCESS flags can only be used with FOR_BINARY
	' FB's OPEN defaults to ACCESS ANY.
	' Which sounds like a misfeature to me, so let's default to ACCESS_READ_WRITE instead.
	ACCESS_ANY =        &h0100000  ' Create if needed, otherwise open for read+write, or read-only if that fails
	ACCESS_READ =       &h0200000  ' Read only. Not created if doesn't exist.
	ACCESS_WRITE =      &h0400000  ' Write only. Create if needed, truncate to zero length otherwise.
	ACCESS_READ_WRITE = &h0800000  ' [Default] Read+Write. Create if needed, does not truncate
        'ACCESS_RW_NO_CREATE=&h0A00000  ' Not implemented
	ACCESS_BITMASK =    &h0F00000
	' Not implemented yet for hooked files, so no point using these
	'ENCODING_ASCII =   &h1000000  ' default
	'ENCODING_UTF8 =    &h2000000
	'ENCODING_UTF16 =   &h4000000
	'ENCODING_UTF32 =   &h8000000
	'ENCODING_BITMASK = &hF000000
	' LOCK not supported... in fact it's not even properly supported by FB!
	' However it could be added (since we already have file locking implemented in os.bi)
	' if it were useful.
	' LEN (record length) not supported.
end enum

Enum FilterActionEnum Explicit
	hook = 1      ' Open and hook it
	dont_hook = 2 ' Open but don't hook it
	deny = 3      ' Don't open the file, return illegal function call error
	hide = 4      ' Don't open the file, return file not found
End Enum

type FnOpenCallback as function (filename as string, writable as boolint, writes_allowed as boolint) as FilterActionEnum

extern "C"

' Error codes returned by FB runtime functions
Enum 'FBErrorEnum
	fberrOK = 0
	fberrILLEGAL_CALL = 1
	fberrNOTFOUND = 2
	fberrFILEIO = 3
	fberrOUTOFMEM = 4
	fberrILLEGALRESUME = 5
	fberrOUTOFBOUNDS = 6
	fberrNULLPTR = 7
	fberrNOPRIVILEGES = 8
	fberrSIGINT = 9
	fberrSIGILL = 10
	fberrSIGFPE = 11
	fberrSIGSEGV = 12
	fberrSIGTERM = 13
	fberrSIGABRT = 14
	fberrSIGQUIT = 15
	fberrRETURNWITHOUTGOSUB = 16
	fberrENDOFFILE = 17
End Enum
Type FBErrorEnum as integer  'For compatibility with C

'Replacement for OPEN (and FREEFILE) which is used to hook accesses to lumps, and send messages from Custom
'to a spawned instance of Game when a modified file is closed.
'Sets fh to a FREEFILE file number (initial value ignored).
'Returns 0 on success, 1 on error, 2 if file doesn't exist and didn't open for writing.
'Example:
'  OPEN file FOR BINARY ACCESS READ as #fh
'becomes:
'  OPENFILE(file, FOR_BINARY + ACCESS_READ, fh)
'All access flags are optional; you can pass 0.
declare function OPENFILE(filename as string, open_bits as OPENBits, byref fh as integer) as FBErrorEnum

'Replacement for CLOSE which temporarily leaves the file open so it can be reused by OPENFILE
'Call close_lazy_files to ensure closed.
'It's OK to lazyclose files opened for writing (they will flushed, unlocked if needed, and
'lump modified messages sent).
'After you've called lazyclose it's an error to call close (because the file number might already
'be closed and reused!)
declare function lazyclose(fh as integer) as FBErrorEnum

'Really close all files that are lazyclose'd.
'Not necessary before quitting, because FB closes all files itself... but it seems
'"double close" warnings are possible if you don't call this.
declare sub close_lazy_files()

declare sub send_lump_modified_msg(byval filename as zstring ptr)
declare sub set_OPEN_hook(lumpfile_filter as FnOpenCallback, lump_writes_allowed as boolint = YES, lazyclose_allowed as boolint = YES, channel as IPCChannel ptr = NULL)
declare sub clear_OPEN_hook()
declare function get_fb_filename(fnum as integer) as string

declare sub log_openfile(filename as zstring ptr)
declare function read_recent_files_list(idx as integer, byref filename as zstring ptr, byref opentime as double) as boolint

end extern

'This is always true except in Game when live previewing (running off Custom's workingdir)
extern can_write_to_workingdir as bool

declare function inworkingdir (filename as string, writable as boolint, writes_allowed as boolint) as FilterActionEnum
declare function hook_all_files(filename as string, writable as boolint, writes_allowed as boolint) as FilterActionEnum

declare function channel_wait_for_msg(byref channel as IPCChannel, wait_for_prefix as string, line_in as string = "", timeout_ms as integer = 500) as integer

#endif
