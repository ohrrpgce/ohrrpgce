
#include "reload.bi"

Using Reload

DECLARE sub dumpDocument overload(doc as DocPtr)
DECLARE sub dumpDocument(nod as NodePtr, ind as integer)

TYPE testPtr as function() as integer

dim shared pauseTime as double

Randomize

sub doTest(t as string, theTest as testPtr)
	static num as integer = 0
	
	num += 1
	
	print "Test #" & num & ": " & t & "... ";
	
	dim as double start, finish, diff
	dim as integer ret
	
	pauseTime = 0
	
	start = timer
	
	ret = theTest()
	
	finish = timer - pauseTime
	
	diff = finish - start
	
	do while diff < 0
		diff += 86400
	loop
	
	'diff *= 1000000
	
	if ret then
		print "FAIL"
		end num
	else
		print "Pass"
	end if
	
	if(diff < 1) then
		diff *= 1000
		if(diff < 1) then
			diff *= 1000
			print "Took " & int(diff) & !" \u03BCs "
		else
			print "Took " & int(diff) & " ms "
		end if
	else
		print "Took " & int(diff) & " s "
	end if
	
end sub

#define pass return 0
#define fail return 1

#macro startTest(t)
	Declare Function t##_TEST() as integer
	doTest(#t, @t##_TEST)
	function t##_TEST() as integer
#endmacro
#define endTest pass : end Function


function ask(q as string) as integer
	dim ret as string, r as integer
	
	dim as double s, f, d
	
	s = timer
	
	q = q & " (y/n)"
	
	again:
	print q
	ret = input(1)
	
	if lcase(ret) <> "y" and lcase(ret) <> "n" then goto again
	
	r = lcase(ret) = "y"
	
	f = timer
	
	d = s - f
	
	do while d < 0
		d += 86400
	loop
	
	pauseTime += d
	
	return r
end function


dim shared doc as DocPtr, doc2 as DocPtr

#if 0
startTest(testStringTables)
	TestStringTables()
endTest
#endif

startTest(createDocument)
	
	doc = CreateDocument()
	
	if doc = null then fail
endTest

startTest(addRootNode)
	dim nod as NodePtr = CreateNode(doc, "root")
	
	if nod = 0 then fail
	
	SetRootNode(doc, nod)
	
	if DocumentRoot(doc) <> nod then fail
endTest

startTest(addSmallInteger)
	dim nod2 as NodePtr = CreateNode(doc, "smallInt")
	
	if nod2 = 0 then fail
	
	dim i as integer = int(Rnd * 20)
	SetContent(nod2, i)
	
	if NodeType(nod2) <> rltInt then fail
	if GetInteger(nod2) <> i then fail
	
	AddChild(DocumentRoot(doc), nod2)
	
	if NumChildren(DocumentRoot(doc)) <> 1 then fail
	
endTest

startTest(addLargeInteger)
	dim nod2 as NodePtr = CreateNode(doc, "largeInt")
	
	if nod2 = 0 then fail
	
	dim i as LongInt = int(Rnd * 200000000000) + 5000000000 'we want something > 32 bits ;)
	SetContent(nod2, i)
	
	if NodeType(nod2) <> rltInt then fail
	if GetInteger(nod2) <> i then fail
	
	AddChild(DocumentRoot(doc), nod2)
	
	if NumChildren(DocumentRoot(doc)) <> 2 then fail
	
endTest

startTest(addSmallNegativeInteger)
	dim nod2 as NodePtr = CreateNode(doc, "smallNegativeInt")
	
	if nod2 = 0 then fail
	
	dim i as integer = int(Rnd * 20) - 40
	SetContent(nod2, i)
	
	if NodeType(nod2) <> rltInt then fail
	if GetInteger(nod2) <> i then fail
	
	AddChild(DocumentRoot(doc), nod2)
	
	if NumChildren(DocumentRoot(doc)) <> 3 then fail
	
endTest

startTest(addLargeNegativeInteger)
	dim nod2 as NodePtr = CreateNode(doc, "largeNegativeInt")
	
	if nod2 = 0 then fail
	
	dim i as LongInt = int(Rnd * 200000000000) - 500000000000 'we want something > 32 bits ;)
	SetContent(nod2, i)
	
	if NodeType(nod2) <> rltInt then fail
	if GetInteger(nod2) <> i then fail
	
	AddChild(DocumentRoot(doc), nod2)
	
	if NumChildren(DocumentRoot(doc)) <> 4 then fail
	
endTest

startTest(addFloat)
	dim nod2 as NodePtr = CreateNode(doc, "floatingPoint")
	
	if nod2 = 0 then fail
	
	dim i as Double = Rnd * 20000000
	SetContent(nod2, i)
	
	if NodeType(nod2) <> rltFloat then fail
	if GetFloat(nod2) <> i then fail
	
	AddChild(DocumentRoot(doc), nod2)
	
	if NumChildren(DocumentRoot(doc)) <> 5 then fail
	
endTest

startTest(addString)
	dim nod2 as NodePtr = CreateNode(doc, "string")
	
	if nod2 = 0 then fail
	
	dim i as String = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
	SetContent(nod2, i)
	
	if NodeType(nod2) <> rltString then fail
	if GetString(nod2) <> i then fail
	
	AddChild(DocumentRoot(doc), nod2)
	
	if NumChildren(DocumentRoot(doc)) <> 6 then fail
	
endTest

startTest(addEmpty)
	dim nod2 as NodePtr = CreateNode(doc, "empty")
	
	if nod2 = 0 then fail
	
	if NodeType(nod2) <> rltNull then fail
	
	AddChild(DocumentRoot(doc), nod2)
	
	if NumChildren(DocumentRoot(doc)) <> 7 then fail
endTest

startTest(addNested)
	dim nod as NodePtr = CreateNode(doc, "nestedTop")
	
	if nod = 0 then fail
	
	AddChild(DocumentRoot(doc), nod)
	
	dim nod2 as NodePtr
	
	for i as integer = 0 to int(Rnd * 7) + 3
		nod2 = CreateNode(doc, "level" & i)
		if nod2 = 0 then fail
		AddChild(nod, nod2)
		nod = nod2
	next
	
	if NumChildren(DocumentRoot(doc)) <> 8 then fail
endTest

startTest(helperFunctions)
	dim nod as nodeptr = SetChildNode(DocumentRoot(doc), "helper")
	SetChildNode(nod, "int", 12345)
	SetChildNode(nod, "float", 1234.5678)
	SetChildNode(nod, "string", "1 2 3 4 5 6 7 8 9 0")
	SetChildNode(nod, "null")
	
	if GetChildNodeInt(nod, "int") <> 12345 then fail
	if GetChildNodeFloat(nod, "float") <> 1234.5678 then fail
	if GetChildNodeStr(nod, "string") <> "1 2 3 4 5 6 7 8 9 0" then fail
	if not GetChildNodeExists(nod, "null") then fail
	if not GetChildNodeBool(nod, "int") then fail
endTest

startTest(serializeXML)
	print
	
	serializeXML(doc)
	
	'if 0 = ask("Did this render correctly?") then fail
endTest

startTest(writeFile)
	SerializeBin("unittest.rld", doc)
	
	if dir("unittest.rld") = "" then fail
endTest

startTest(loadFile)
	doc2 = LoadDocument("unittest.rld")
	
	if doc2 = null then fail
endTest

function comparenode(nod1 as nodeptr, nod2 as nodeptr) as integer
	if NodeName(nod1) <> NodeName(nod2) then
		print "Names of nodes differ! " & NodeName(nod1) & " vs" & NodeName(nod2)
		return 1
	end if
	
	if NodeType(nod1) <> NodeType(nod2) then
		print "Types of node " & NodeName(nod1) & " differ! " & NodeType(nod1) & " vs" & NodeType(nod2)
		return 1
	end if
	
	select case NodeType(nod1)
		case rltNull
		case rltInt
			if GetInteger(nod1) <> GetInteger(nod2) then
				print "Value of node " & NodeName(nod1) & " differ! " & GetInteger(nod1) & " vs " & GetInteger(nod2)
				return 1
			end if
		case rltFloat
			if GetFloat(nod1) <> GetFloat(nod2) then
				print "Value of node " & NodeName(nod1) & " differ! " & GetFloat(nod1) & " vs " & GetFloat(nod2)
				return 1
			end if
		case rltString
			if GetString(nod1) <> GetString(nod2) then
				print "Value of node " & NodeName(nod1) & " differ! " & GetString(nod1) & " vs " & GetString(nod2)
				return 1
			end if
	end select
	
	if NumChildren(nod1) <> NumChildren(nod2) then
		print "Number of children on node " & NodeName(nod1) & " differ! " & NumChildren(nod1) & " vs " & NumChildren(nod2)
		return 1
	end if
	
	dim numkids as integer = NumChildren(nod1)
	
	nod1 = FirstChild(nod1)
	nod2 = FirstChild(nod2)
	for i as integer = 0 to numkids - 1
		if comparenode(nod1, nod2) then
			print "Comparison of children failed"
			return 1
		end if
		
		nod1 = NextSibling(nod1)
		nod2 = NextSibling(nod2)
	next
	
	'I GUESS they're the same...
	return 0
end function

startTest(compareDocuments)
	if comparenode(DocumentRoot(doc), DocumentRoot(doc2)) then fail
endTest


startTest(freeDocument)
	FreeDocument(doc)
	doc = 0
	FreeDocument(doc2)
	doc2 = 0
	pass
endTest
