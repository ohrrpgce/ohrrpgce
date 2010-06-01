#include "reload.bi"

function timedisp(s as double, e as double) as string
	dim dif as double = e - s
	
	if dif > 1 then
		return int(dif) & "s"
	elseif dif > 0.001 then
		return int(dif * 1000) & "ms"
	elseif dif > 0.000001 then
		return int(dif * 1000000) & "us"
	else
		return int(dif * 1000000000) & "ns"
	end if
end function



dim filename as string

dim i as integer = 1

while command(i) <> ""
	if filename = "" then
		filename = command(i)
	else
		print "Don't know what to do with " & command(i) & "..."
	end if
	i += 1
wend

if filename = "" then
	print "Usage:"
	print command(0) & " filename.rld"
	end
end if

dim doc as Reload.Docptr

dim as double startTime, endTime

print "Loading " & filename & "..."

startTime = timer

doc = Reload.LoadDocument(filename, Reload.optNoDelay)

endTime = timer

if doc = null then
	print "Error loading " & filename
	end
end if

print "Loaded in " & timedisp(startTime, endTime)

startTime = timer

Reload.FreeDocument(doc)

endTime = timer

print "Freed in " & timedisp(startTime, endTime)

