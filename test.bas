SUB test(filename as string)
 DIM chunk1 as string = STRING(10, 0)
 DIM chunk2 as string = STRING(10, 0)

 DIM size as integer
 DIM fh as integer

 fh = FREEFILE
 IF OPEN(filename FOR BINARY AS #fh) THEN PRINT filename & " [CANNOT BE OPENED]" : EXIT SUB
  '--Read the first 10 bytes
  size = LOF(fh)
  GET #fh, , chunk1
 CLOSE #fh

 IF size >= 4096 THEN EXIT SUB

 fh = FREEFILE
 IF OPEN(filename FOR BINARY ACCESS READ AS #fh) THEN PRINT filename & " [CANNOT BE OPENED ACCESS READ]" : EXIT SUB
  '--Read the first 10 bytes
  GET #fh, , chunk2
 CLOSE #fh

 DIM info as string = ""

 IF chunk1 <> chunk2 THEN
  info = "[BAD!]"
 END IF

 PRINT filename & " " & size & " " & info

END SUB

test COMMAND
