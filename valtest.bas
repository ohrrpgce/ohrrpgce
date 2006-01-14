DECLARE FUNCTION str2int% (stri$)
DEFINT A-Z

PRINT "Testing VAL()..."
FOR i& = 32760 TO 32770
 s$ = STR$(i&)
 PRINT s$; VAL(s$); str2int(s$)
NEXT i&

FUNCTION str2int (stri$)

n& = 0
s$ = LTRIM$(stri$)
sign = 1

FOR i = 1 TO LEN(s$)
 c$ = MID$(s$, i, 1)
 IF c$ = "-" AND i = 1 THEN sign = -1
 c = ASC(c$) - 48
 IF c >= 0 AND c <= 9 THEN
  n& = n& * 10 + (c * sign)
 END IF
NEXT i

IF n& > 32767 THEN n& = 32767
IF n& < -32768 THEN n& = -32768

str2int = n&

END FUNCTION

