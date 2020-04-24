' This is a helper for the rename tests in filetest.bas.
' This program opens a file with a certain access mode, possibly writes to it,
' waits a while, then closes it.

#include "config.bi"
#ifdef __FB_WIN32__
	include_windows_bi()
#endif
#include "common_base.bi"
#include "util.bi"
#include "lumpfile.bi"

dim filename as string = command(1)
dim waitms as integer = valint(command(2))
dim readonly as bool
dim sharedelete as bool
dim lockopen as bool
dim quiet as bool

select case command(3)
#ifdef __FB_WIN32__
        case "-sharedelete": sharedelete = YES
        case "-lock":        lockopen = YES
#else
        case "-sharedelete": readonly = YES
        case "-lock":        readonly = YES
#endif
        case "-readonly":    readonly = YES
        case "-write":       readonly = NO
	case else
	        print "Usage: filetest_helper filename wait_ms -write|-readonly|-sharedelete|-lock [-q]"
        	end 1
end select

quiet = (command(4) = "-q")

dim outtext as string = "filetest_helper"

if sharedelete or lockopen then
#ifdef __FB_WIN32__
        dim modeinfo as string = iif(sharedelete, " with FILE_SHARE_DELETE", " without sharing")
        dim hdl as HANDLE
        dim flags as integer = 0
        if lockopen = NO then flags = FILE_SHARE_READ + FILE_SHARE_WRITE + FILE_SHARE_DELETE
        hdl = CreateFile(strptr(filename), GENERIC_READ, flags, _
                         NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL)
        if hdl = 0 then
                print "Couldn't open " & filename & modeinfo & " : " & *win_error_str()
                end 1
        end if
        if quiet = NO then print "filetest_helper: Opened " & filename & modeinfo

        /'
        dim written as integer
	if WriteFile(hdl, strptr(outtext), len(outtext), @written, NULL) = 0 then
                print "WriteFile failed: " & *win_error_str()
        end if
        FlushFileBuffers(hdl)
        '/

        touchfile "_syncfile.tmp"

        sleep waitms

        CloseHandle(hdl)
#endif
else
        dim modeinfo as string = iif(readonly, " (readonly)", " (writing)")
        dim fh as integer
        if openfile(filename, for_binary + iif(readonly, access_read, 0), fh) then
                print "filetest_helper: Couldn't open " & filename & modeinfo
                end 1
        end if
        if quiet = NO then print "filetest_helper: Opened " & filename & modeinfo

        if readonly = NO then
                print #fh, outtext
                fflush(cast(FILE Ptr, FileAttr(fh, fbFileAttrHandle)))
        end if

        touchfile "_syncfile.tmp"

        sleep waitms

        close(fh)
end if

if quiet = NO then print "filetest_helper: Done"
