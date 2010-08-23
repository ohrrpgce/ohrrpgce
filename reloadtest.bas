
#include "reload.bi"
#include "reloadext.bi"

Using Reload
Using Reload.Ext

TYPE testPtr as function() as integer

dim shared pauseTime as double

Randomize

sub doTest(t as string, byval theTest as testPtr)
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

#if 0
startTest(bitsetTest)
	doc = CreateDocument()
	if doc = null then fail
	
	dim nod as NodePtr = CreateNode(doc, "root")
	if nod = null then fail
	
	SetRootNode(doc, nod)
	
	CreateBitset(nod)
	
	SerializeBin("test.rld", doc)
	
	FreeDocument(doc)
	
	doc = null
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

startTest(addBitset)
	dim nod2 as NodePtr = CreateNode(doc, "bitset")
	
	if nod2 = 0 then fail
	
	SetContent(nod2, " ") 'this is the brute force method...
	
	if GetBitset(nod2, 5) = 0 then fail

	CreateBitset(nod2)
	
	if GetBitset(nod2, 5) <> 0 then fail
	
	SetBitset(nod2, 10, 1)
	
	if GetBitset(nod2, 10) = 0 then fail
	
	AddChild(DocumentRoot(doc), nod2)
	
	if NumChildren(DocumentRoot(doc)) <> 7 then fail
	
endTest

startTest(addEmpty)
	dim nod2 as NodePtr = CreateNode(doc, "empty")
	
	if nod2 = 0 then fail
	
	if NodeType(nod2) <> rltNull then fail
	
	AddChild(DocumentRoot(doc), nod2)
	
	if NumChildren(DocumentRoot(doc)) <> 8 then fail
endTest

startTest(addNested)
	dim nod as NodePtr = CreateNode(doc, "nestedTop")
	
	if nod = 0 then fail
	
	AddChild(DocumentRoot(doc), nod)
	
	dim nod2 as NodePtr
	
	for i as integer = 0 to int(Rnd * 7) + 3
		nod2 = CreateNode(doc, "level" & i)
		SetContent(nod2, 42 + i)
		if nod2 = 0 then fail
		AddChild(nod, nod2)
		nod = nod2
	next
	
	if NumChildren(DocumentRoot(doc)) <> 9 then fail
endTest

startTest(helperFunctions)
	dim nod as nodeptr = SetChildNode(DocumentRoot(doc), "helper")
	SetContent(nod, 1)

	SetChildNode(nod, "int", 12345)
	SetChildNode(nod, "float", 1234.5678)
	SetChildNode(nod, "string", "1 2 3 4 5 6 7 8 9 0")
	SetChildNode(nod, "null")
	
	if GetChildNodeInt(nod, "int") <> 12345 then fail
	if GetChildNodeFloat(nod, "float") <> 1234.5678 then fail
	if GetChildNodeStr(nod, "string") <> "1 2 3 4 5 6 7 8 9 0" then fail
	if not GetChildNodeExists(nod, "null") then fail
	if not GetChildNodeBool(nod, "int") then fail
	
	AppendChildNode(nod, "appended", 54321)
	AppendChildNode(nod, "appended", 43.21)
	AppendChildNode(nod, "@attr1", "fish")
	AppendChildNode(nod, "appended", "A B C D E F G H I J")
	AppendChildNode(nod, "appended")
endTest

startTest(bitsetArray)
	dim nod2 as nodeptr = CreateNode(doc, "bitsettest")
	
	dim bs(100) as integer, bs2(100) as integer
	
	for i as integer = 0 to 99
		bs(i) = int(rnd * 65535) 'add some random bits
	next
	
	SaveBitsetArray(nod2, bs(), 100)
	
	LoadBitsetArray(nod2, bs2(), 100)
	
	for i as integer = 0 to 99
		if bs(i) <> bs2(i) then fail
	next
	
endTest

startTest(testNodeByPath)
	dim nod1 as NodePtr
	dim nod2 as NodePtr
	nod1 = SetChildNode(DocumentRoot(doc), "party")
	AppendChildNode(nod1, "slot", 0)
	AppendChildNode(nod1, "slot", 0)
	AppendChildNode(nod1, "slot", 0)
	nod1 = AppendChildNode(nod1, "slot", 3)
	nod1 = SetChildNode(nod1, "stats")
	nod2 = AppendChildNode(nod1, "stat", 0)
	AppendChildNode(nod1, "stat", 1)
	SetChildNode(nod2, "max", 100)
	SetChildNode(nod2, "cur", 95)
	
	nod1 = NodeByPath(doc, "/party/slot[3]/stats/stat[0]/max")
	if nod1 = null then fail
	if NodeName(nod1) <> "max" then fail
	if GetInteger(nod1) <> 100 then fail
endTest

startTest(serializeXML)
	print
	
	dim fh as integer
	open cons for output as fh
	serializeXML(doc, fh)
	close fh
	
	'if 0 = ask("Did this render correctly?") then fail
endTest

startTest(writeFile)
	SerializeBin("unittest.rld", doc)
	
	if dir("unittest.rld") = "" then fail
endTest

startTest(compareDocumentsNoDelay)
	doc2 = LoadDocument("unittest.rld", optNoDelay)
	
	if doc2 = null then fail
	
	if CompareNodes(DocumentRoot(doc), DocumentRoot(doc2)) then fail
endTest

startTest(freeDocumentNoDelay)
	FreeDocument(doc2)
	doc2 = 0
	pass
endTest

startTest(compareDocumentsDelay)
	doc2 = LoadDocument("unittest.rld")
	
	if doc2 = null then fail
	
	if CompareNodes(DocumentRoot(doc), DocumentRoot(doc2)) then fail
endTest

startTest(freeDocumentDelay)
	FreeDocument(doc2)
	doc2 = 0
	pass
endTest

startTest(cleanup)
	FreeDocument(doc)
	doc = 0
	pass
endTest
