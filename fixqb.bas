' fixqb - Fixes ancient QB code

Enum TokenType
	t_undef
	t_eof
	t_eol
	t_comment
	t_string
	t_identifier
	t_symbol
	t_whitespace
	t_integer
	t_double
	t_k_enum
	t_k_type
	t_k_as
	t_k_dim
	t_k_shared
	t_k_function
	t_k_sub
	t_k_end
	t_k_exit
	t_k_if
	t_k_for
	t_k_while
	t_k_do
	t_k_next
	t_k_declare
	t_k_loop
	t_k_else
	t_k_elseif
	t_k_then
	t_k_return
	t_k_wend
	t_k_select
	t_k_case
	t_k_and
	t_k_or
	t_k_xor
	t_k_to
	t_k_until
	t_k_binary
	t_k_random
	t_k_input
	t_k_output
	t_k_goto
	t_k_print
	t_k_byval
	t_k_byref
End Enum

Type Token
	sval as string
	ival as integer
	dval as double
	ttype as TokenType
End Type

dim shared ungottenchar as integer = 0

function isNumber(c as integer) as integer
	if c >= asc("0") and c <= asc("9") then return -1
	return 0
end function

function isAlpha(c as integer) as integer
	if (c >= asc("a") and c <= asc("z")) or (c >= asc("A") and c <= asc("Z")) then return -1
	return 0
end function

function isAlphaNum(c as integer) as integer
	return isNumber(c) or isAlpha(c)
end function

function isWhitespace(c as integer) as integer
	return c = asc(" ") or c = asc("	")
end function

'reads a single character in from the specified file
'if the end of file is reached, returns ""
function getchar(f as integer) as integer
	if ungottenchar <> 0 then
		dim ret as integer = ungottenchar
		ungottenchar = 0
		return ret
	else
		if eof(f) then return -1
		
		dim b as ubyte
		
		get #f, , b
		
		return b
	end if
end function

'pushes a character "back" into the file
'do note that you can't unget more than one character...
sub ungetchar(c as integer)
	ungottenchar = c
end sub

sub readtoken(f as integer, byref tok as Token)
	dim ret as string
	dim c as integer
	
	tok.ttype = t_undef
	
	c = getchar(f)
	if c = -1 then
		tok.ttype = t_eof
		exit sub
	end if
	do while c <> -1
		if isWhitespace(c) then 'the second one is a tab, by the way
			tok.ttype = t_whitespace
			ret = chr(c)
			c = getchar(f)
			do while isWhitespace(c)
				ret &= chr(c)
				c = getchar(f)
			loop
			ungetchar(c)
			
			tok.sval = ret
			exit do
		elseif isNumber(c) then
			ret = chr(c)
actuallyanumber:
			tok.ttype = t_integer
			
			c = getchar(f)
			
			'the following rules should be equivalent to the following regular expression:
			'\d+(?:\.\d+)?(?:[dDeE]?(?:[+-]?\d+)?)?[!#fF]
			do while isNumber(c)
				ret &= chr(c)
				c = getchar(f)
			loop
			if c = asc(".") then
				tok.ttype = t_double
				ret &= "."
				c = getchar(f)
				do while isNumber(c)
					ret &= chr(c)
					c = getchar(f)
				loop
			end if
			
			if lcase(chr(c)) = "d" or lcase(chr(c)) = "e" then
				tok.ttype = t_double
				ret &= chr(c)
				c = getchar(f)
			end if
			if c = asc("+") or c = asc("-") then
				tok.ttype = t_double
				ret &= chr(c)
				do while isNumber(c)
					ret &= chr(c)
					c = getchar(f)
				loop
			end if
			if c = asc("!") or c = asc("#") or c = asc("f") or c = asc("F") then
				tok.ttype = t_double
				ret &= chr(c)
				c = getchar(f)
			end if
			
			ungetchar(c)
			
			if tok.ttype = t_double then
				tok.dval = cdbl(ret)
			else
				tok.ival = cint(ret)
			end if
			exit do
		elseif c = asc("'") then
			tok.ttype = t_comment
			ret = ""
			c = getchar(f)
			do while c <> 10 and c <> 13 and c <> -1
				ret &= chr(c)
				c = getchar(f)
			loop
			
			dim p as integer = getchar(f)
			
			if p <> 10 then
				ungetchar(p)
			end if
			
			tok.sval = ret
			exit do
		elseif c = asc("!") or c = asc("@") or c = asc("#") or c = asc("$") or c = asc("%") _
			or c = asc("^") or c = asc("&") or c = asc("*") or c = asc("(") or c = asc(")") _
			or c = asc("{") or c = asc("}") or c = asc("-") or c = asc("+") or c = asc("=") _
			or c = asc("`") or c = asc("[") or c = asc("]") or c = asc(".") or c = asc(",") _
			or c = asc("<") or c = asc(">") or c = asc(":") or c = asc(";") or c = asc("_") _
			or c = asc("\") or c = asc("/") then
			
			ret = chr(c)
			
			tok.ttype = t_symbol
			dim p as integer = getchar(f)
			ungetchar(p)
			
			'bloody unary minus
			if c = asc("-") and isNumber(p) then
				goto actuallyanumber
			end if
			
			if _
				(c = asc(">") and p = asc("=")) or _
				(c = asc("<") and p = asc("=")) or _
				(c = asc("&") and p = asc("=")) or _
				(c = asc("+") and p = asc("=")) or _
				(c = asc("-") and p = asc("=")) or _
				(c = asc("*") and p = asc("=")) or _
				(c = asc("/") and p = asc("=")) _
			then
				
				'consume the peek char
				getchar(f)
				ret &= chr(p)
			end if
			
			tok.sval = ret
			exit do
		elseif c = asc("""") then
			'string!
			tok.ttype = t_string
			ret = ""
			c = getchar(f)
			do until c = asc("""") or c = -1
				'print chr(c);
				ret &= chr(c)
				c = getchar(f)
			loop
			'print
			if c = -1 then
				print "Unexpected end of file in string"
				end
			end if
			tok.sval = ret
			exit do
		elseif isAlpha(c) then
			'first, let's read it in. Then we'll decide what it is...
			ret = chr(c)
			c = getchar(f)
			do while isAlphaNum(c) or c = asc("_")
				ret &= chr(c)
				c = getchar(f)
			loop
			ungetchar(c)
			
			select case lcase(ret)
				case "if"
					tok.ttype = t_k_if
				case "else"
					tok.ttype = t_k_else
				case "elseif"
					tok.ttype = t_k_elseif
				case "end"
					tok.ttype = t_k_end
				case "exit"
					tok.ttype = t_k_exit
				case "do"
					tok.ttype = t_k_do
				case "while"
					tok.ttype = t_k_while
				case "wend"
					tok.ttype = t_k_wend
				case "dim"
					tok.ttype = t_k_dim
				case "declare"
					tok.ttype = t_k_declare
				case "shared"
					tok.ttype = t_k_shared
				case "next"
					tok.ttype = t_k_next
				case "then"
					tok.ttype = t_k_then
				case "return"
					tok.ttype = t_k_return
				case "as"
					tok.ttype = t_k_as
				case "sub"
					tok.ttype = t_k_sub
				case "function"
					tok.ttype = t_k_function
				case "type"
					tok.ttype = t_k_type
				case "enum"
					tok.ttype = t_k_enum
				case "select"
					tok.ttype = t_k_select
				case "case"
					tok.ttype = t_k_case
				case "and"
					tok.ttype = t_k_and
				case "or"
					tok.ttype = t_k_or
				case "xor"
					tok.ttype = t_k_xor
				case "for"
					tok.ttype = t_k_for
				case "to"
					tok.ttype = t_k_to
				case "until"
					tok.ttype = t_k_until
				case "binary"
					tok.ttype = t_k_binary
				case "random"
					tok.ttype = t_k_random
				case "input"
					tok.ttype = t_k_input
				case "output"
					tok.ttype = t_k_output
				case "goto"
					tok.ttype = t_k_goto
				case "print"
					tok.ttype = t_k_print
				case "loop"
					tok.ttype = t_k_loop
				case "byval"
					tok.ttype = t_k_byval
				case "byref"
					tok.ttype = t_k_byref
				case else
					tok.ttype = t_identifier
					tok.sval = ret
			end select
			
			
			exit do
		elseif c = 13 or c = 10 then
			tok.ttype = t_eol
			dim p as integer = getchar(f)
			if p <> 10 then ungetchar(p)
			exit do
			
		else 'anything else...
			print "OMGWAT: " & chr(c) & " (" & c & ")"
			end
			c = getchar(f)
			
		end if
	loop
	
end sub


dim infile as string = command(1)

if infile = "" then
	print "Yo, pass me a file name, plskthx."
	end
end if



'so, like, basically, this is a basic parser. Except, we're not executing anything, just fixing
'symbols and the like.

'here-a-we-go!
dim f as integer = freefile

open infile for binary as #f

dim tok as token

do until tok.ttype = t_eof
	readtoken(f, tok)

	select case tok.ttype
		case t_undef
			'print "Undefined..."
		case t_eof
			'print "End of File!"
			exit do
		case t_eol
			'print "End of Line!"
			print
		case t_comment
			print "'" & tok.sval
		case t_integer
			print str(tok.ival);
		case t_double
			print str(tok.dval);
		case t_string
			print """" & tok.sval & """";
		case t_symbol
			print tok.sval;
		case t_identifier
			print tok.sval;
		case t_k_if
			print "IF";
		case t_k_then
			print "THEN";
		case t_k_else
			print "ELSE";
		case t_k_elseif
			print "ELSEIF";
		case t_k_enum
			print "ENUM";
		case t_k_type
			print "TYPE";
		case t_k_as
			print "AS";
		case t_k_dim
			print "DIM";
		case t_k_shared
			print "SHARED";
		case t_k_function
			print "FUNCTION";
		case t_k_sub
			print "SUB";
		case t_k_end
			print "END";
		case t_k_exit
			print "EXIT";
		case t_k_for
			print "FOR";
		case t_k_while
			print "WHILE";
		case t_k_do
			print "DO";
		case t_k_next
			print "NEXT";
		case t_k_declare
			print "DECLARE";
		case t_k_loop
			print "LOOP";
		case t_k_return
			print "RETURN";
		case t_k_wend
			print "WEND";
		case t_k_select
			print "SELECT";
		case t_k_case
			print "CASE";
		case t_k_and
			print "AND";
		case t_k_or
			print "OR";
		case t_k_xor
			print "XOR";
		case t_k_for
			print "FOR";
		case t_k_to
			print "TO";
		case t_k_until
			print "UNTIL";
		case t_k_binary
			print "BINARY";
		case t_k_random
			print "RANDOM";
		case t_k_input
			print "INPUT";
		case t_k_output
			print "OUTPUT";
		case t_k_goto
			print "GOTO";
		case t_k_print
			print "PRINT";
		case t_k_byval
			print "BYVAL";
		case t_k_byref
			print "BYREF";
		case t_whitespace
			print tok.sval;
		case else
			print "Unknown type: " & tok.ttype
			end
	end select
loop

close #f
