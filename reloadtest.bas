
#include "reload.bi"

Using Reload

DECLARE sub dumpDocument overload(doc as DocPtr)
DECLARE sub dumpDocument(nod as NodePtr, ind as integer)



dim doc as DocPtr

print "Loading document... ";

doc = LoadDocument("test.rld")

if doc = null then
	print "Error!"
	end 1
end if

print "okay"

serializeXML(doc)



print "Freeing document... ";
FreeDocument(doc)

print "okay"


end 0

