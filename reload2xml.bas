#include "reload.bi"

DIM filename AS STRING
filename = COMMAND(1)

IF NOT isfile(filename) THEN
 PRINT "Convert a RELOAD file into XML and dump it to the standard output."
 PRINT ""
 PRINT "Usage: reload2xml reloadfilename"
 PRINT "       reload2xml reloadfilename > filename.xml"
 SYSTEM
END IF

DIM doc AS Reload.DocPtr
doc = Reload.LoadDocument(filename)
Reload.SerializeXML(doc)
Reload.FreeDocument(doc)
