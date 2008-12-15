
#include "reload.bi"

Using Reload

dim doc as DocPtr

dim as double startTime = Timer, realStart = Timer

doc = LoadDocument("test2.rld")

print "Loaded document in " & int((timer - starttime) * 1000) & " ms"
starttime = timer

if doc = null then
	print "Error loading document!"
else
	serializeXML(doc)
	starttime = timer
	FreeDocument(doc)
	print "Tore down memory in " & int((timer - starttime) * 1000) & " ms"
	
	print "Finished in " & int((timer - realStart) * 1000) & " ms"
end if
