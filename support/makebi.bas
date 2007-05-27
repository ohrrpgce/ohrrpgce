option explicit

if command(1) = "" then 
  print "Usage: makebi file.bas <output.bi>"
  end
end if

dim file as string = command(1)

dim outp as string = command(2)

dim root as string = ucase(mid(file,1,instr(file,".") - 1))

if outp = "" then outp = lcase(root) + ".bi"

open "bihead.txt" for input as #1

dim head as string, tmp as string

do until eof(1)
  line input #1, tmp
  if instr(tmp, "{FILE}") then tmp = left(tmp, instr(tmp, "{FILE}") - 1) + outp + mid(tmp, instr(tmp, "{FILE}") + 6)
  if instr(tmp, "{SRC}") then tmp = left(tmp, instr(tmp, "{SRC}") - 1) + file + mid(tmp, instr(tmp, "{SRC}") + 5)
  head += tmp + chr(13) + chr(10)
loop
close #1

open file for input as #1
open outp for output as #2

print #2, head
print #2, "#IFNDEF " & root & "_BI"
print #2, "#DEFINE " & root & "_BI"
print #2,

dim l as string
do until eof(1)
  line input #1, l
  l = lcase(ltrim(l))
  
  if left(l,1) = "'" or left(l,4) = "rem " then
    ' ignore
  elseif left(l,8) = "private " then
    ' ugh, also ignore
  elseif left(l,7) = "public " then
    l = mid(l,8) 'remove the 'public ' bit
    goto still 'cuz we still want to process this line
  elseif left(l,4) = "sub " or left(l,9) = "function " then
  
  still: 'there is a valid reason for this.
    
    dim ret as string
    
    ret = "declare "
    
    ret += l
    
    print #2, ret
  
  end if
  

loop

print #2,
print #2, "#ENDIF"

close #1
close #2