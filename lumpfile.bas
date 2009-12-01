'OHRRPGCE COMMON - Lumped file format routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#include "allmodex.bi"
#include "compat.bi"
#include "common.bi"
#include "lumpfile.bi"
#include "lumpfilewrapper.bi"

'slight hackery to get more versatile read function
declare function fget alias "fb_FileGet" ( byval fnum as integer, byval pos as integer = 0, byval dst as any ptr, byval bytes as uinteger ) as integer
declare function fput alias "fb_FilePut" ( byval fnum as integer, byval pos as integer = 0, byval src as any ptr, byval bytes as uinteger ) as integer
declare function fgetiob alias "fb_FileGetIOB" ( byval fnum as integer, byval pos as integer = 0, byval dst as any ptr, byval bytes as uinteger, byval bytesread as uinteger ptr ) as integer

option explicit


declare function matchmask(match as string, mask as string) as integer

declare sub LumpedLump_writetofile(byref this as LumpedLump, byval fileno as integer, byval position as integer)
declare function LumpedLump_read(byref this as LumpedLump, byval position as integer, byval bufr as any ptr, byval size as integer) as integer
declare sub FileLump_writetofile(byref this as FileLump, byval fileno as integer, byval position as integer)
declare function FileLump_read(byref this as FileLump, byval position as integer, byval bufr as any ptr, byval size as integer) as integer

dim shared lumpvtable(LT_NUM - 1) as LumpVTable_t
LMPVTAB(LT_LUMPED,   NULL, ULMP(LumpedLump_,writetofile), NULL, ULMP(LumpedLump_,read))
LMPVTAB(LT_FILE,     NULL, ULMP(FileLump_,writetofile),   NULL, ULMP(FileLump_,read))


'----------------------------------------------------------------------
'                          LumpIndex class


sub construct_LumpIndex(byref this as LumpIndex)
	this.numlumps = 0
	this.tablesize = 512
	this.first = NULL
	this.last = NULL
	this.table = callocate(sizeof(any ptr) * this.tablesize)
	this.fhandle = 0
	this.unlumpeddir = ""
end sub

sub destruct_LumpIndex(byref this as LumpIndex)
	dim as Lump ptr lmp, temp

	if this.fhandle then close this.fhandle

	lmp = this.first
	while lmp
		temp = lmp->next
		'lumpvtable(lump->type).destruct(*lump, whereto)
		deallocate(lmp)
		lmp = temp
	wend

	deallocate(this.table)
end sub

sub LumpIndex_addlump(byref this as LumpIndex, byval lump as Lump ptr)
	dim hash as unsigned integer
	dim lmpp as Lump ptr ptr
	
	hash = strhash(lump->lumpname)
	lmpp = @this.table[hash mod this.tablesize]
	while *lmpp
		lmpp = @(*lmpp)->bucket_chain
	wend
	*lmpp = lump

	lump->index = @this

	this.numlumps += 1
	if this.first = NULL then
		this.first = lump
		this.last = lump
	else
		this.last->next = lump
		this.last = lump
	end if
end sub

'case sensitive
function LumpIndex_findlump(byref this as LumpIndex, lumpname as string) as Lump ptr
	dim hash as unsigned integer
	dim lmp as Lump ptr

	hash = strhash(lumpname)
	lmp = this.table[hash mod this.tablesize]
	while lmp
		if lmp->lumpname = lumpname then return lmp
		lmp = lmp->bucket_chain
	wend

	return NULL
end function

'verify integrity and print contents
sub LumpIndex_debug(byref this as LumpIndex)
	dim lmp as Lump ptr
	dim lumped as LumpedLump ptr
	dim numlumps as integer = 0

	debug "===LumpIndex_debug==="

	for i as integer = 0 to this.tablesize - 1
		lmp = this.table[i]
		while lmp
			numlumps += 1
			if (strhash(lmp->lumpname) mod this.tablesize) <> i then
				debug "LumpIndex_debug error: lump in wrong bucket"
			end if
			lmp = lmp->bucket_chain
		wend
	next

	if numlumps <> this.numlumps then
		debug "error: " & numlumps & " lumps in table, " & this.numlumps & " recorded"
	end if

	numlumps = 0
	lmp = this.first
	do
		numlumps += 1
		debug lmp->lumpname
		if lmp->type = LT_LUMPED then
			lumped = cast(LumpedLump ptr, lmp)
			debug "  at " & lumped->offset & " len " & lumped->length
		elseif lmp->type = LT_FILE then
			debug "  in " + this.unlumpeddir
		end if
		if lmp->next = NULL then
			if lmp <> this.last then
				debug "error: ->last corrupt"
			end if
			exit do
		end if
		lmp = lmp->next
	loop

	if numlumps <> this.numlumps then
		debug "error: " & numlumps & " lumps chained, " & this.numlumps & " recorded"
	end if

	debug "====================="
end sub


'----------------------------------------------------------------------
'                           Lump base class


sub Lump_unlumpfile(byref this as Lump, whereto as string)
	dim dest as string
	dim of as integer

	if @this = 0 then
		debug "Null lump error"
		exit sub
	end if
	if lumpvtable(this.type).writetofile = 0 then
		debug "lump writetofile method not supported"
		exit sub
	end if

	dest = whereto + this.lumpname
	if fileiswriteable(dest) = 0 then
		debug "Could not unlump to " + dest
		exit sub
	end if
	of = freefile
	open dest for binary access write as #of
	lumpvtable(this.type).writetofile(this, of, 1)
	close of
end sub

function Lump_read(byref this as Lump, byval position as integer, byval bufr as any ptr, byval size as integer) as integer
	if @this = 0 then
		debug "Null lump error"
		return 0
	end if
	return lumpvtable(this.type).read(this, position, bufr, size)
end function


'----------------------------------------------------------------------
'                           16-bit records
'Why is this here? Because according to the Plan, binsize expanding may be
'handled transparently by the Lump object rather than actually occurring


function loadrecord (buf() as integer, fh as integer, recordsize as integer, record as integer = -1) as integer
'common sense alternative to loadset, setpicstuf
'loads 16bit records in an array
'buf() = buffer to load shorts into, starting at buf(0)
'fh = open file handle
'recordsize = record size in shorts (not bytes)
'record = record number, defaults to read from current file position
'returns 1 if successful, 0 if failure (eg. file too short)
	dim idx as integer
	if recordsize <= 0 then return 0
	if ubound(buf) < recordsize - 1 then
		debug "loadrecord: " & recordsize & " ints will not fit in " & ubound(buf) + 1 & " element array"
		'continue, fit in as much as possible
	end if
	dim readbuf(recordsize - 1) as short

	if record <> -1 then
		seek #fh, recordsize * 2 * record + 1
	end if
	if seek(fh) + 2 * recordsize > lof(fh) + 1 then return 0
	get #fh, , readbuf()
	for idx = 0 to small(recordsize - 1, ubound(buf))
		buf(idx) = readbuf(idx)
	next
	loadrecord = 1
end function

function loadrecord (buf() as integer, filen as string, recordsize as integer, record as integer = 0) as integer
'wrapper for above
	dim f as integer
	dim i as integer

	if recordsize <= 0 then return 0

	if NOT fileisreadable(filen) then
		debug "File not found loading record " & record & " from " & filen
		for i = 0 to recordsize - 1
			buf(i) = 0
		next
		return 0
	end if
	f = freefile
	open filen for binary access read as #f

	loadrecord = loadrecord (buf(), f, recordsize, record)
	close #f
end function

sub storerecord (buf() as integer, fh as integer, recordsize as integer, record as integer = -1)
'same as loadrecord
	if ubound(buf) < recordsize - 1 then
		debug "storerecord: array has only " & ubound(buf) + 1 & " elements, record is " & recordsize & " ints"
		'continue, write as much as possible
	end if

	dim idx as integer
	dim writebuf(recordsize - 1) as short

	if record <> -1 then
		seek #fh, recordsize * 2 * record + 1
	end if
	for idx = 0 to small(recordsize - 1, ubound(buf))
		writebuf(idx) = buf(idx)
	next
	put #fh, , writebuf()
end sub

sub storerecord (buf() as integer, filen as string, recordsize as integer, record as integer = 0)
'wrapper for above
	dim f as integer

	if NOT fileiswriteable(filen) then exit sub
	f = freefile
	open filen for binary access read write as #f

	storerecord buf(), f, recordsize, record
	close #f
end sub


'----------------------------------------------------------------------
'                           FileLump class


sub FileLump_writetofile(byref this as FileLump, byval fileno as integer, byval position as integer)
	'unfinished
end sub

/'
sub FileLump_unlumpfile(byref this as FileLump, whereto as string)
	filecopy this.index->unlumpeddir + this.lumpname, whereto + this.lumpname
end sub
'/

function FileLump_read(byref this as FileLump, byval position as integer, byval bufr as any ptr, byval size as integer) as integer
	'unfinished
	return 0
end function

function indexunlumpeddir (whichdir as string) as LumpIndex ptr
	dim index as LumpIndex ptr
	dim fh as integer
	dim filename as string

	index = callocate(sizeof(LumpIndex))
	construct_LumpIndex(*index)
	index->unlumpeddir = whichdir

	'ideally findfiles would have an overload to return files in an array, but that's a nontrivial project
	findfiles whichdir + ALLFILES, 0, "filelist.tmp"
	fh = freefile
	open "filelist.tmp" for input as #fh
	do until eof(fh)
		line input #fh, filename
		
		dim lmp as FileLump ptr
		lmp = callocate(sizeof(FileLump))
		lmp->type = LT_FILE
		lmp->lumpname = filename
		LumpIndex_addlump(*index, cast(Lump ptr, lmp))
	loop
	close fh
	kill "filelist.tmp"

	return index
end function


'----------------------------------------------------------------------
'                           LumpedLump class


sub LumpedLump_writetofile(byref this as LumpedLump, byval fileno as integer, byval position as integer)
	dim bufr as byte ptr
	dim size as integer

	if this.index->fhandle = 0 then
		debug "lumped file not open"
		exit sub
	end if

	bufr = allocate(32768)

	'copy the data
	seek this.index->fhandle, this.offset
	seek fileno, position
	size = this.length
	while size > 0
		fget this.index->fhandle, , bufr, small(size, 32768)
		fput fileno, , bufr, small(size, 32768)
		size -= 32768
	wend
	deallocate(bufr)
end sub

function LumpedLump_read(byref this as LumpedLump, byval position as integer, byval bufr as any ptr, byval size as integer) as integer
	dim amount as unsigned integer
	fgetiob(this.index->fhandle, this.offset + position, bufr, small(size, this.length - position), @amount)
	return amount
end function

function indexlumpfile (lumpfile as string, byval keepopen as integer = YES) as LumpIndex ptr
	dim index as LumpIndex ptr
	dim lf as integer
	dim dat as ubyte
	dim size as integer
	dim maxsize as integer
	dim lname as string
	dim i as integer

	if NOT fileisreadable(lumpfile) then
		debug "indexlumpfile: could not read " + lumpfile
		return NULL
	end if

	index = callocate(sizeof(LumpIndex))
	construct_LumpIndex(*index)

	lf = freefile
	open lumpfile for binary access read lock write as #lf
	if keepopen then index->fhandle = lf
	maxsize = lof(lf)

	do
		'get lump name
		lname = ""
		i = 0
		while not eof(lf) and i < 50
			get #lf, , dat
			if dat = 0 then exit while
			lname = lname + chr(dat)
			i += 1
		wend
		if i >= 50 then 'corrupt file, really if i > 12
			debug "corrupt lump file " + lumpfile + " : lump name too long"
			exit do
		end if
		if eof(lf) then
			debug "corrupt lump file " + lumpfile + " : garbage"
			exit do
		end if

		lname = lcase(lname)
		'debug "lump name <" + lname + ">"

		'if instr(lname, "\") or instr(lname, "/") then
		'	debug "lump file " + lumpfile + " : unsafe lump name " + lname
		'	exit do
		'end if

		'get lump size - byte order = 3,4,1,2
		fget lf, , cast(short ptr, @size) + 1, 2
		fget lf, , cast(short ptr, @size), 2
		'debug "lump size " + str(size)

		if size + seek(lf) > maxsize + 1 then
			debug lumpfile + ": corrupt lump size " & size & " exceeds source size " & maxsize
			exit do
		end if

		dim lmp as LumpedLump ptr
		lmp = callocate(sizeof(LumpedLump))
		lmp->type = LT_LUMPED
		lmp->lumpname = lname
		lmp->offset = seek(lf)
		lmp->length = size
		LumpIndex_addlump(*index, cast(Lump ptr, lmp))

		seek #lf, seek(lf) + size
		if eof(lf) then
			'success
			if keepopen = 0 then close lf
			return index
		end if
	loop
	'while loop exits on error

	'closes file
	destruct_LumpIndex(*index)
	deallocate(index)
	return NULL
end function


'----------------------------------------------------------------------
'                         Lump FileWrapper

extern "C"

function FileWrapper_open(byval lump as Lump ptr) as FileWrapper ptr
	dim filew as FileWrapper ptr
	filew = callocate(sizeof(FileWrapper))
	filew->lump = lump
	'filew->index = index
	filew->pos = 0
	
	return filew
end function

sub FileWrapper_close(byref this as FileWrapper)
	deallocate(@this)
end sub

function FileWrapper_seek(byref this as FileWrapper, byval offset as integer, byval whence as integer) as integer
	if whence = SEEK_SET then
		this.pos = offset
	elseif whence = SEEK_CUR then
		this.pos += offset
	elseif whence = SEEK_END then
		this.pos = this.lump->length + offset
	end if
	this.pos = bound(this.pos, 0, this.lump->length)
	return this.pos
end function

function FileWrapper_read(byref this as FileWrapper, byval bufr as any ptr, byval size as integer, byval maxnum as integer) as integer
	dim ret as integer
	if size <= 0 or maxnum <= 0 then
		return 0
	end if
	ret = Lump_read(*this.lump, this.pos, bufr, size * maxnum)
	this.pos = bound(this.pos + ret, 0, this.lump->length)
	return ret \ size
end function

end extern

'----------------------------------------------------------------------
'                           Lumped files


sub unlump (lumpfile as string, ulpath as string)
	unlumpfile(lumpfile, "", ulpath)
end sub

sub unlumpfile (lumpfile as string, fmask as string, path as string)
	dim lf as integer
	dim dat as ubyte
	dim size as integer
	dim maxsize as integer
	dim lname as string
	dim i as integer
	dim bufr as ubyte ptr
	dim nowildcards as integer = 0

	if NOT fileisreadable(lumpfile) then exit sub
	lf = freefile
	open lumpfile for binary access read as #lf
	maxsize = LOF(lf)

	if len(path) > 0 and right(path, 1) <> SLASH then path = path & SLASH

	bufr = callocate(16383)
	if bufr = null then
		close #lf
		exit sub
	end if

	'should make browsing a bit faster
	if len(fmask) > 0 then
		if instr(fmask, "*") = 0 and instr(fmask, "?") = 0 then
			nowildcards = -1
		end if
	end if

	get #lf, , dat	'read first byte
	while not eof(lf)
		'get lump name
		lname = ""
		i = 0
		while not eof(lf) and dat <> 0 and i < 64
			lname = lname + chr(dat)
			get #lf, , dat
			i += 1
		wend
		if i > 50 then 'corrupt file, really if i > 12
			debug "corrupt lump file " + lumpfile + " : lump name too long"
			exit while
		end if
		'force to lower-case
		lname = lcase(lname)
		'debug "lump name " + lname

		if instr(lname, "\") or instr(lname, "/") then
			debug "lump file " + lumpfile + " : unsafe lump name " + lname
			exit while
		end if

		if not eof(lf) then
			'get lump size - byte order = 3,4,1,2 I think
			get #lf, , dat
			size = (dat shl 16)
			get #lf, , dat
			size = size or (dat shl 24)
			get #lf, , dat
			size = size or dat
			get #lf, , dat
			size = size or (dat shl 8)
			if size > maxsize then
				debug lumpfile + ": corrupt lump size " & size & " exceeds source size " & maxsize
				exit while
			end if

			'debug "lump size " + str(size)

			'do we want this file?
			if matchmask(lname, lcase(fmask)) then
				'write yon file
				dim of as integer
				dim csize as integer

				if NOT fileiswriteable(path + lname) then exit while
				of = freefile
				open path + lname for binary access write as #of

				'copy the data
				while size > 0
					if size > 16383 then
						csize = 16383
					else
						csize = size
					end if
					'copy a chunk of file
					fget lf, , bufr, csize
					fput of, , bufr, csize
					size = size - csize
				wend

				close #of

				'early out if we're only looking for one file
				if nowildcards then exit while
			else
				'skip to next name
				i = seek(lf)
				i = i + size
				seek #lf, i
			end if

			if not eof(lf) then
				get #lf, , dat
			end if
		end if
	wend

	deallocate bufr
	close #lf

end sub

function islumpfile (lumpfile as string, fmask as string) as integer
	dim lf as integer
	dim dat as ubyte
	dim size as integer
	dim maxsize as integer
	dim lname as string
	dim i as integer

	islumpfile = 0

	if NOT fileisreadable(lumpfile) then exit function
	lf = freefile
	open lumpfile for binary access read as #lf
	maxsize = LOF(lf)

	get #lf, , dat	'read first byte
	while not eof(lf)
		'get lump name
		lname = ""
		i = 0
		while not eof(lf) and dat <> 0 and i < 64
			lname = lname + chr(dat)
			get #lf, , dat
			i += 1
		wend
		if i > 50 then 'corrupt file, really if i > 12
			debug "corrupt lump file " + lumpfile + " : lump name too long"
			exit while
		end if
		'force to lower-case
		lname = lcase(lname)
		'debug "lump name " + lname

		if instr(lname, "\") or instr(lname, "/") then
			debug "lump file " + lumpfile + " : unsafe lump name " + lname
			exit while
		end if

		if not eof(lf) then
			'get lump size - byte order = 3,4,1,2 I think
			get #lf, , dat
			size = (dat shl 16)
			get #lf, , dat
			size = size or (dat shl 24)
			get #lf, , dat
			size = size or dat
			get #lf, , dat
			size = size or (dat shl 8)
			if size > maxsize then
				debug lumpfile + ": corrupt lump size " & size & " exceeds source size " & maxsize
				exit while
			end if

			'do we want this file?
			if matchmask(lname, lcase(fmask)) then
				islumpfile = -1
				exit function
			else
				'skip to next name
				seek #lf, seek(lf) + size
			end if

			if not eof(lf) then
				get #lf, , dat
			end if
		end if
	wend

	close #lf
end function

sub lumpfiles (listf as string, lumpfile as string, path as string)
	dim as integer lf, fl, tl	'lumpfile, filelist, tolump

	dim dat as ubyte
	dim size as integer
	dim lname as string
	dim bufr as ubyte ptr
	dim csize as integer
	dim as integer i, t, textsize(1)

	fl = freefile
	open listf for input as #fl
	if err <> 0 then
		exit sub
	end if

	lf = freefile
	open lumpfile for binary access write as #lf
	if err <> 0 then
		'debug "Could not open file " + lumpfile
		close #fl
		exit sub
	end if

	bufr = callocate(16000)

	'get file to lump
	do until eof(fl)
		line input #fl, lname

		'validate that lumpname is 8.3 or ignore the file
		textsize(0) = 0
		textsize(1) = 0
		t = 0
		for i = 0 to len(lname)-1
			if lname[i] = asc(".") then t = 1
			textsize(t) += 1
		next
		'note extension includes the "." so can be 4 chars
		if textsize(0) > 8 or textsize(1) > 4 then
			debug "name too long: " + lname
			debug " name = " + str(textsize(0)) + ", ext = " + str(textsize(1))
			continue do
		end if

		tl = freefile
		open path + lname for binary access read as #tl
		if err <> 0 then
			'debug "failed to open " + path + lname
			continue do
		end if

		'write lump name (seems to need to be upper-case, at least
		'for any files opened with unlumpone in the QB version)
		put #lf, , ucase(lname)
		dat = 0
		put #lf, , dat

		'write lump size - byte order = 3,4,1,2 I think
		size = lof(tl)
		dat = (size and &hff0000) shr 16
		put #lf, , dat
		dat = (size and &hff000000) shr 24
		put #lf, , dat
		dat = size and &hff
		put #lf, , dat
		dat = (size and &hff00) shr 8
		put #lf, , dat

		'write lump
		while size > 0
			if size > 16000 then
				csize = 16000
			else
				csize = size
			end if
			'copy a chunk of file
			fget(tl, , bufr, csize)
			fput(lf, , bufr, csize)
			size = size - csize
		wend

		close #tl
	loop

	close #lf
	close #fl

	deallocate bufr
end sub

function matchmask(match as string, mask as string) as integer
	dim i as integer
	dim m as integer
	dim si as integer, sm as integer

	'special cases
	if mask = "" then
		matchmask = 1
		exit function
	end if

	i = 0
	m = 0
	while (i < len(match)) and (m < len(mask)) and (mask[m] <> asc("*"))
		if (match[i] <> mask[m]) and (mask[m] <> asc("?")) then
			matchmask = 0
			exit function
		end if
		i = i+1
		m = m+1
	wend

	if (m >= len(mask)) and (i < len(match)) then
		matchmask = 0
		exit function
	end if

	while i < len(match)
		if m >= len(mask) then
			'run out of mask with string left over, rewind
			i = si + 1 ' si will always be set by now because of *
			si = i
			m = sm
		else
			if mask[m] = asc("*") then
				m = m + 1
				if m >= len(mask) then
					'* eats the rest of the string
					matchmask = 1
					exit function
				end if
				i = i + 1
				'store the positions in case we need to rewind
				sm = m
				si = i
			else
				if (mask[m] = match[i]) or (mask[m] = asc("?")) then
					'ok, next
					m = m + 1
					i = i + 1
				else
					'mismatch, rewind to last * positions, inc i and try again
					m = sm
					i = si + 1
					si = i
				end if
			end if
		end if
	wend

  	while (m < len(mask)) and (mask[m] = asc("*"))
  		m = m + 1
  	wend

  	if m < len(mask) then
		matchmask = 0
	else
		matchmask = 1
	end if

end function
