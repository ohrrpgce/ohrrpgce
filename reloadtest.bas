
#include "reload.bi"

Using Reload

dim doc as DocPtr

doc = LoadDocument("test.rld")

if doc = null then
	print "Error loading document!"
else
	serializeXML(doc)
	FreeDocument(doc)
end if
