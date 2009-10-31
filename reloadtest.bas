
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

print "Running RPath query... "

dim nodes as NodeSetPtr = RPathQuery("bar", doc->root)

if nodes = null then
	print "Error!"
else
	print "Found " & nodes->numNodes & " nodes!"
	for i as integer = 0 to nodes->numNodes - 1
		serializeXML(nodes->nodes[i])
	next
	
	
end if




print "Freeing document... ";
FreeDocument(doc)

print "okay"


end 0

