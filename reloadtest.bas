
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


dumpDocument(doc)



print "Freeing document... ";
FreeDocument(doc)

print "okay"


end 0


sub dumpDocument(doc as DocPtr)
	dumpDocument(doc->root, 0)
end sub

sub dumpDocument(nod as NodePtr, ind as integer)
	print string(ind, "	");
	select case nod->nodeType
		case rltNull
			print "<" & nod->name & " />"
			exit sub
		case rltInt
			if nod->name <> "" then print "<" & nod->name & ">";
			print "" & nod->num;
			if nod->name <> "" then print "</" & nod->name & ">";
			print " <!--i-->"
		case rltFloat
			if nod->name <> "" then print "<" & nod->name & ">";
			print "" & nod->flo;
			if nod->name <> "" then print "</" & nod->name & ">";
			print " <!--f-->"
		case rltString
			if nod->name <> "" then print "<" & nod->name & ">";
			print nod->str;
			if nod->name <> "" then print "</" & nod->name & ">";
			print " <!--s-->"
		case rltChildren
			print "<" & nod->name & ">"
			dim nod2 as NodePtr = nod->children
			'print "# = " & nod->numChildren
			for i as integer = 0 to nod->numChildren - 1
				'print i
				dumpDocument(nod2, ind + 1)
				nod2 = nod2->nextSib
			next
			print string(ind, "	") & "</" & nod->name & ">"
		case else
			print "What did I forget? " & nod->NodeType
	end select
end sub