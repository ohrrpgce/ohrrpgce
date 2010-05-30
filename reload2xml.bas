#include "reload.bi"

dim as string filename, outfile
filename = command(1)
outfile = command(2)

if isfile(filename) = 0 or outfile = "" then
	print "Convert a RELOAD file into XML. Specify - as outfile to print to console"
	print ""
	print "Usage: reload2xml reloadfilename filename.xml"
	print "   or: reload2xml reloadfilename - > filename.xml"
	end
end if

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
doc = Reload.LoadDocument(filename)

if outfile <> "-" then print "Loaded RELOAD document in " & int((timer - starttime) * 1000) & " ms"
starttime = timer

Reload.SerializeXML(doc, outstream)

if outfile <> "-" then print "Serialized XML document in " & int((timer - starttime) * 1000) & " ms"
starttime = timer

Reload.FreeDocument(doc)

if outfile <> "-" then
	print "Tore down memory in " & int((timer - starttime) * 1000) & " ms"
	print "Finished in " & int((timer - realStart) * 1000) & " ms"
end if
