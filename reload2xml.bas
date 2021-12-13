'OHRRPGCE - reload2xml utility
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
'
'Compile with "scons reload2xml"

#include "util.bi"
#include "reload.bi"

dim as string filename, outfile
dim as integer validargs = NO, debugging = NO, shortform = NO, i = 1

while command(i) <> ""
	if command(i) = "--debug" then
		debugging = YES
	elseif command(i) = "--short" then
		shortform = YES
	elseif filename = "" then
		filename = command(i)
		validargs = YES
	elseif outfile = "" then
		outfile = command(i)
	else
		validargs = NO
	end if
	i += 1
wend

if len(filename) = 0 orelse isfile(filename) = NO orelse validargs = NO then
	print "Convert a RELOAD file into XML. Specify - as outfile to print to console."
	print "Add --debug to print strings unambiguously."
	print "Add --short to leave out long strings and binary data."
	print ""
	print "Usage: reload2xml [--debug] [--short] reloadfilename filename.xml"
	print "   or: reload2xml [--debug] [--short] reloadfilename - > filename.xml"
	end
end if

if outfile = "" then outfile = "-"

dim as double startTime = timer, realStart = timer

dim outstream as integer
outstream = freefile
if outfile = "-" then
	open cons for output as outstream
else
	if open(outfile for output as outstream) then
		print "Could not open " & outfile
		end
	end if
end if

dim doc as Reload.DocPtr
doc = Reload.LoadDocument(filename, Reload.optNoDelay)

if outfile <> "-" then print "Loaded RELOAD document in " & int((timer - starttime) * 1000) & " ms"
starttime = timer

Reload.SerializeXML(doc, outstream, debugging, shortform)

if outfile <> "-" then print "Serialized XML document in " & int((timer - starttime) * 1000) & " ms"
starttime = timer

Reload.FreeDocument(doc)

if outfile <> "-" then
	print "Tore down memory in " & int((timer - starttime) * 1000) & " ms"
	print "Finished in " & int((timer - realStart) * 1000) & " ms"
end if
