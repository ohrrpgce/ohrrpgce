'OHRRPGCE - a RELOAD test utility, or compare two RELOAD files
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
'
'Compile with "scons reloadutil"

#include "reload.bi"
#include "reloadext.bi"

dim shared as double startTime, endTime


function timedisp(s as double, e as double) as string
	dim dif as double = e - s
	
	if dif > 1 then
		return int(dif) & "s"
	elseif dif > 0.001 then
		return int(dif * 1000) & "ms"
	elseif dif > 0.000001 then
		return int(dif * 1000000) & "us"
	else
		return int(dif * 1000000000) & "ns"
	end if
end function

function verboseload(filename as string) as Reload.Docptr
	dim doc as Reload.Docptr
	
	print "Loading " & filename & "..."
	
	startTime = timer
	
	doc = Reload.LoadDocument(filename, Reload.optNoDelay)
	
	endTime = timer
	
	if doc = null then
		print "Error loading " & filename
		end
	end if
	
	print "Loaded in " & timedisp(startTime, endTime)
	return doc
end function


dim as string filename1, filename2
dim pedantic as bool = NO

dim i as integer = 1

while command(i) <> ""
	if command(i) = "--pedantic" then
		pedantic = YES
	elseif filename1 = "" then
		filename1 = command(i)
	elseif filename2 = "" then
		filename2 = command(i)
	else
		print "Don't know what to do with " & command(i) & "..."
	end if
	i += 1
wend

if filename1 = "" then
	print "Usage:"
	print "Time RELOAD load/save: " & command(0) & " filename.rld"
	print "Compare two documents: " & command(0) & " [--pedantic] filename1.rld filename2.rld"
	print "Use --pedantic to count 8 and ""8"" or Null and """" as inequal."
	end
end if

dim as Reload.Docptr doc1, doc2

doc1 = verboseload(filename1)

if filename2 <> "" then
	' Compare docs
	doc2 = verboseload(filename2)

	if Reload.Ext.CompareNodes(Reload.DocumentRoot(doc1), Reload.DocumentRoot(doc2), pedantic, YES) then
		print "* Documents compare " & iif(pedantic, "exactly ", "effectively ") & "equal. *"
	end if
else
	' Just timing
	tmpdir = get_tmpdir()
	dim outfname as string = tmpdir + SLASH + "reloadutil.reload.tmp"

	startTime = timer
	Reload.SerializeBin(outfname, doc1)
	endTime = timer
	print "Serialized in " & timedisp(startTime, endTime)

	killfile outfname
end if

startTime = timer
Reload.FreeDocument(doc1)
endTime = timer
print "Freed " & filename1 & " in " & timedisp(startTime, endTime)

if doc2 then
	startTime = timer
	Reload.FreeDocument(doc2)
	endTime = timer
	print "Freed " & filename2 & " in " & timedisp(startTime, endTime)
end if
