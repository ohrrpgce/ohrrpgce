'OHRRPGCE COMMON - XML related functions
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

#include "libxml/tree.bi"
#include "libxml/parser.bi"

#include "xml.bi"

sub compileTextbox(byref txt as Textbox)
	dim as xmlDocPtr doc = 0
	dim as xmlNodePtr root = 0
	
	doc = xmlNewDoc("1.0")
	root = xmlNewNode(0, "textbox")
	
	xmlDocSetRootElement(doc, root)
	
	xmlSaveFormatFileEnc("-", doc, "UTF-8", 1)
	xmlFreeDoc(doc)
	xmlCleanupParser()
	xmlMemoryDump()
	
end sub