--GENERAL PURPOSE SPIFFY routines for HamsterSpeak
--(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
--Please read LICENSE.txt for GPL License details and disclaimer of liability
---------------------------------------------------------------------------
-- All of the routines in this file are general-purpose routines that
-- are used by HSPEAK.EX but dont do anything HamsterSpeak-specific
---------------------------------------------------------------------------

include misc.e
include file.e
include get.e

---------------------------------------------------------------------------

---constants---

constant true=1
constant false=0

global constant stdin=0
global constant stdout=1
global constant failure=-1

--globals--
global integer PATH_SLASH

ifdef WINDOWS then
  PATH_SLASH='\\'
elsedef
  PATH_SLASH='/'
end ifdef

--redundant to the standard upper() and lower() functions I know, but I
--use them constantly, and almost never use the other wildcard functs.
--this avoids including wildcard.e

-- convert atom or sequence to lower case
global function hs_lower(object x)
    return x + (x >= 'A' and x <= 'Z') * ('a' - 'A')
end function

-- convert atom or sequence to upper case
global function hs_upper(object x)
    return x - (x >= 'a' and x <= 'z') * ('a' - 'A')
end function

-- convert unprintable characters to \x escape codes
global function escape_string(sequence s)
  sequence result
  result=""
  for i=1 to length(s) do
    if s[i]=10 then
      result&="\\n"
    elsif s[i]<32 or s[i]>=127 then
      result&=sprintf("\\x%02x",{s[i]})
    else
      result&={s[i]}
    end if
  end for
  return result
end function

---convert a delimited string into a sequence of strings---
global function explode(sequence s,sequence delim)
  sequence result
  integer pos
  result={}
  pos=match(delim,s)
  if pos then
    while pos do
      result=append(result,s[1..pos-1])
      s=s[pos+length(delim)..length(s)]
      pos=match(delim,s)
    end while
  end if
  result=append(result,s)
  return result
end function

--merge a sequence of strings into a single delimited string--
global function implode (sequence s,sequence glue)
  sequence output
  output={}
  if length(s) then
    output=s[1]
    for i=2 to length(s) do
      output &= glue & s[i]
    end for
  end if
  return output
end function

---exclude specified chars from a string sequence---
global function exclude(sequence s,sequence c)
  sequence result
  result={}
  for i=1 to length(s) do
    if not find(s[i],c) then
      result=append(result,s[i])
    end if
  end for
  return(result)
end function

--wraps value() and returns a default if it fails
global function string_to_object(sequence s,object default)
  object result
  result=value(s)
  if result[1]!=GET_SUCCESS then
    result[2]=default
  end if
  return result[2]
end function

---collapse reundant elements of a sequence---
global function redundx(sequence s,object c)
 sequence n
 n=""
 for i=1 to length(s) do
  if compare(s[i],c)=0 then
   if i<length(s) then
    if compare(s[i+1],c)!=0 then
     n=append(n,s[i])
    end if
   else
    n=append(n,s[i])
   end if
  else
   n=append(n,s[i])
  end if
 end for
 return n
end function

---collapse all redundant elements of a sequence---
global function redundall(sequence s)
 sequence n
 n=""
 for i=1 to length(s) do
  if i<length(s) then
   if compare(s[i+1],s[i])!=0 then
    n=append(n,s[i])
   end if
  else
   n=append(n,s[i])
  end if
 end for
 return n
end function

--returns a column of a sequence of sequences--
global function column(sequence s,integer column)
  for i=1 to length(s) do
    s[i]=s[i][column]
  end for
  return s
end function

--equivalent to 'find(it,column(s,column))'--
global function find_in_column(object it,sequence s,integer column)
  for i=1 to length(s) do
    if equal(s[i][column],it) then
      return i
    end if
  end for
  return 0
end function

--substitute all instances of one object in a sequence with another object--
global function substitute(sequence seq, object old, object new)
  for i=1 to length(seq) do
    if compare(seq[i],old)=0 then
      seq[i]=new
    end if
  end for
  return seq
end function

--substitute all instances of a slice--
global function substring_replace(sequence seq,sequence old, sequence new)
  integer at
  integer start
  at=match(old,seq)
  start=1
  while at>=start do
    seq=seq[1..at-1] & new & seq[at+length(old)..length(seq)]
    start=at+length(new)
    at=match(old,repeat(0,start-1) & seq[start..length(seq)])
  end while
  return(seq)
end function

--count the number of occurrences of an object in a sequence--
global function count(object it, sequence s)
  integer cnt
  integer i
  cnt=0
  i=find(it,s)
  while i do
    cnt+=1
    i=find_from(it,s,i+1)
  end while
  return cnt
end function

--find from the end of a sequence--
global function find_rev(object it, sequence s)
  for i=length(s) to 1 by -1 do
    if equal(s[i],it) then
      return i
    end if
  end for
  return 0
end function

--normalize a pathname to use forward slashes--
global function filenamix(sequence s)
  return substitute(s,'\\','/')
end function

--normalize a pathname to use back slashes--
global function filenamos(sequence s)
  return substitute(s,'/','\\')
end function

--normalise a pathname to use the platform specific path delimiter--
global function normalize_filename(sequence s)
  ifdef WINDOWS then
    return filenamos(s)
  elsedef
    return filenamix(s)
  end ifdef
end function

--extract only portion of a string after the last of a delimiter--
global function get_suffix(sequence s,sequence delim)
  sequence broken
  broken=explode(s,delim)
  return broken[length(broken)]
end function

--extract all of a string except what follows the last of a delimiter--
global function without_suffix(sequence s,sequence delim)
  sequence broken
  broken=explode(s,delim)
  if length(broken)>1 then
    return implode(broken[1..length(broken)-1],delim)&delim
  else
    return ""
  end if
end function

--exclude the path from a fully qualified filename--
global function file_only(sequence filename)
  return get_suffix(filenamix(filename),"/")
end function

--return only the path from a fully qualified filename--
global function path_only(sequence filename)
  return without_suffix(filenamix(filename),"/")
end function

--return only the extension of a filename--
global function extension_only(sequence filename)
  return get_suffix(filename,".")
end function

--exclude the extension from a filename--
global function without_extension(sequence filename)
  sequence result
  result=without_suffix(filename,".")
  if length(result) then
    return result
  else
    return filename&"."
  end if
end function

--returns a filename with the extension changed--
global function alter_extension(sequence filename,sequence newext)
  return without_extension(filename) & newext
end function

--return the larger of two integers--
global function large(integer n1,integer n2)
  if n1>n2 then
    return n1
  else
    return n2
  end if
end function

--return the smaller of two integers--
global function small(integer n1,integer n2)
  if n1<n2 then
    return n1
  else
    return n2
  end if
end function

--delete the first element of a sequence--
global function decapitate(sequence s)
  if length(s) then
    return s[2..length(s)]
  else
    return s
  end if
end function

--verify that a file exists--
global function file_exists(sequence f)
  integer fh
  integer result
  fh=open(f,"rb")
  if fh=-1 then
    result=false
  else
    close(fh)
    result=true
  end if
  return(result)
end function

--insert an object into a sequence--
global function insert_element(sequence s,object ob,integer index)
  if index=1 then
    return(prepend(s,ob))
  elsif index=length(s)+1 then
    return(append(s,ob))
  else
    return(append(s[1..index-1],ob)&s[index..length(s)])
  end if
end function

--insert a sequence into a sequence--
global function insert_sequence(sequence s,sequence new,integer index)
  if index=1 then
    return(new&s)
  elsif index=length(s)+1 then
    return(s&new)
  else
    return(s[1..index-1]&new&s[index..length(s)])
  end if
end function

--delete an element of a sequence
global function delete_element(sequence s,integer index)
  return(s[1..index-1]&s[index+1..length(s)])
end function

--delete a slice from a sequence--
global function delete_slice(sequence s,integer first,integer last)
  return(s[1..first-1]&s[last+1..length(s)])
end function

--replace a slice with a sequence--
global function replace_slice(sequence s,integer first,integer last,sequence new)
  return(s[1..first-1]&new&s[last+1..length(s)])
end function

--trim whitespace (tabs and spaces) from beginning and end--
global function trim_whitespace(sequence s)
  while length(s) and (s[1]=' ' or s[1]='\t') do
    s=s[2..length(s)]
  end while
  while length(s) and (s[length(s)]=' ' or s[length(s)]='\t') do
    s=s[1..length(s)-1]
  end while
  return s
end function

--remove any of the specified char from the end of a string
global function trim_tail(sequence s,integer tail)
  while length(s) and s[length(s)]=tail do
    s=s[1..length(s)-1]
  end while
  return(s)
end function

--create a blank alpha-tree
global function alpha_tree_create()
  --make space for all printable chars
  sequence tree
  tree=repeat({{},{}},95)
  return(tree)
end function

--stores text into a alphabet tree
global function alpha_tree_insert(sequence tree,sequence string,object data)
  integer firstchar
  firstchar=string[1]-31
  if firstchar<1 or firstchar>95 then
    --unprintables get filed at the end with ~
    firstchar=95
  end if
  tree[firstchar][1]=append(tree[firstchar][1],string)
  tree[firstchar][2]=append(tree[firstchar][2],data)
  return(tree)
end function

global function alpha_tree_mass_insert(sequence tree,sequence mass)
  --{{string,data},{string,data}}
  integer firstchar
  for i=1 to length(mass) do
    firstchar=mass[i][1][1]-31
    if firstchar<1 or firstchar>95 then
      firstchar=95
    end if
    tree[firstchar][1]=append(tree[firstchar][1],mass[i][1])
    tree[firstchar][2]=append(tree[firstchar][2],mass[i][2])
  end for
  return(tree)
end function

--change the data for an existing string
global function alpha_tree_set_data(sequence tree,sequence string,object data)
  integer at
  integer firstchar
  firstchar=string[1]-31
  if firstchar<1 or firstchar>95 then
    firstchar=95
  end if
  at=find(string,tree[firstchar][1])
  tree[at][2]=data
  return(tree)
end function

--returns true if a gives string is in an alpha-tree--
global function alpha_tree_seek(sequence tree,sequence string)
  integer firstchar
  if length(string) then
    firstchar=string[1]-31
    if firstchar<1 or firstchar>95 then
      firstchar=95
    end if
    return(find(string,tree[firstchar][1]))
  else
    return(false)
  end if
end function

--returns the data associated with a string in a tree--
global function alpha_tree_data(sequence tree,sequence string,object default)
  integer at
  integer firstchar
  if length(string)=0 then
    return(default)
  end if
  firstchar=string[1]-31
  if firstchar<1 or firstchar>95 then
    firstchar=95
  end if
  at=find(string,tree[firstchar][1])
  if at then
    return(tree[firstchar][2][at])
  else
    return(default)
  end if
end function

global type int32(object o)
  --32-bit signed int (Euphoria deals in 31-bit ints)
  if integer(o) or (atom(o) and floor(o)=o and o>=-2147483648 and o<=2147483647) then
    return(true)
  end if
  return(false)
end type

global function string_is_int32(sequence s)
  object o
  o=value(s)
  if o[1]=GET_SUCCESS then
    --translated to object
    if int32(o[2]) then
      --object is an integer
      --if compare(s,sprintf("%d",{o[2]}))=0 then
        --strict checking is disabled because 7 = 007
        --is exact match
        return(true)
      --end if
    end if
  end if
  return(false)
end function

---------------------------------------------------------------------------

--returns a number as a four-byte sequence in absurd byte order {3,4,1,2}
function absurd_byte_order(integer n)
  integer b1,b2,b3,b4
  b1=and_bits(n,#FF)
  b2=and_bits(n,#FF00)/#100
  b3=and_bits(n,#FF0000)/#10000
  b4=and_bits(n,#FF000000)/#1000000
  return{b3,b4,b1,b2}
end function

---------------------------------------------------------------------------

global function write_lump(integer filehandle,sequence name,sequence data)
  if length(name)>50 then
    --fail if name is too long
    return(false)
  end if
  puts(filehandle,hs_upper(name)&0)
  puts(filehandle,absurd_byte_order(length(data)))
  puts(filehandle,data)
  return(true)
end function

---------------------------------------------------------------------------

--alternative to write_lump. Call lumphandle=begin_lump(...), then write
--data to the file and call end_lump(lumphandle)
global function begin_lump(integer filehandle,sequence name)
  sequence handle
  if length(name)>50 then
    --fail if name is too long
    return(false)
  end if
  puts(filehandle,hs_upper(name)&0)
  handle={filehandle,where(filehandle)}
  puts(filehandle,{0,0,0,0})
  return(handle)
end function

---------------------------------------------------------------------------

global function end_lump(sequence lumphandle)
  integer curpos
  curpos=where(lumphandle[1])
  if seek(lumphandle[1],lumphandle[2]) then
    return false
  end if
  puts(lumphandle[1],absurd_byte_order(curpos-lumphandle[2]-4))
  if seek(lumphandle[1],curpos) then
    return false
  end if
  return(true)
end function

---------------------------------------------------------------------------

--returns the first x characters, avoiding word break.
--this is used to do the word-wrapping of long-lines
global function before_wrap_point(sequence string)
  sequence line,word,result
  integer size
  size=79
  result=string
  line=explode(string,"\n")
  if length(line) then
    word=explode(line[1]," ")
    result=""
    if length(word) then
      while length(word) do
        if length(result & word[1]) <= size then
          result &= word[1]
          word=decapitate(word)
          if length(word) then
            result &= " "
          end if
        else
          if length(word[1])>size then
            result &= word[1][1..large(0,size-length(result))]
          end if
          exit
        end if
      end while
    else
      result&=line[1]
    end if
  end if
  return result
end function

---------------------------------------------------------------------------

--returns the remainder of a string after word wrapping one line
--this is used to do the word-wrapping of long-lines
global function after_wrap_point(sequence string)
  sequence result
  result=string[large(length(before_wrap_point(string)),1)+1..length(string)]
  if length(result) then
    if result[1]='\n' then
      result=decapitate(result)
    end if
  end if
  return result
end function

---------------------------------------------------------------------------

--replace the end of a string with ellipsis if too long
global function shorten_string(sequence s, integer len)
  integer quotes
  quotes=0
  if length(s)>0 and s[1]='"' and s[$]='"' then
    quotes=1
    s=s[2..$-1]
    len-=2
  end if
  if length(s)>len then
    s=s[1..len-3]&"..."
  end if
  if quotes then
    return("\""&s&"\"")
  else
    return(s)
  end if
end function
