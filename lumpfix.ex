--OHRRPGCE LUMPFIX - Utility to repair damage to RPG format files
--(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
--Please read LICENSE.txt for GPL License details and disclaimer of liability
--See README.txt for code docs

include lump.e
include spiffy.e
include graphics.e
include wildcard.e

---------------------------------------------------------------------------

constant true=1
constant false=0
constant end_of_file=-1

constant LUMP_NAME=1
constant LUMP_DATA=2

---------------------------------------------------------------------------

sequence prog_dir
sequence lump_filename
sequence simple_name
sequence lump_data
integer corrupt          corrupt=false
sequence lump_list       lump_list={{},{}}     
integer recover_count    recover_count=0

---------------------------------------------------------------------------

procedure nl_printf(integer dest,sequence s,sequence printf_args)
  sequence pos
  pos=get_position()
  if pos[2]>1 then
    printf(dest,"\n",{})
  end if
  printf(dest,s,printf_args)
end procedure

---------------------------------------------------------------------------

procedure init()
  sequence args
  args=command_line()
  prog_dir=path_only(args[2])
  if length(args)>=3 then
    lump_filename=args[3]
    simple_name=upper(trim_tail(without_extension(file_only(lump_filename)),'.'))
  else  
    nl_printf(stdout,"LUMPFIX - (C)2001 James Paige / Hamster Republic Productions\n",{})  
    printf(stdout,"A utility that attempts to repair corrupted lumpfiles\n",{})  
    printf(stdout,"usage:  LUMPFIX FILENAME.RPG\n",{})  
    abort(0)
  end if
  if not file_exists(lump_filename) then
    nl_printf(stdout,"Lumpfile %s not found\n",{lump_filename})
    abort(1)
  end if
end procedure

---------------------------------------------------------------------------

procedure read_file()
  integer fh
  integer char
  nl_printf(stdout,"Reading Lumpfile %s\n",{lump_filename})
  lump_data={}
  fh=open(lump_filename,"rb")
  if fh=failure then
    nl_printf(stdout,"file open failed\n",{})
    abort(1)
  else
    while true do
      char=getc(fh)
      if char=end_of_file then
        exit
      end if
      lump_data&=char
    end while
    close(fh)
  end if
  nl_printf(stdout,"read %d bytes\n",{length(lump_data)})
end procedure

---------------------------------------------------------------------------

procedure write_fixed_lumpfile()
  sequence outfile
  integer fh
  outfile=alter_extension(lump_filename,"FIX")
  if length(lump_list[LUMP_NAME]) then
    nl_printf(stdout,"%s is corrupt\nwriting fixed file %s\n",{lump_filename,outfile})
    fh=open(outfile,"wb")
    if fh=failure then
      nl_printf(stdout,"failed to open %s\n",{outfile})
    else
      for i=1 to length(lump_list[LUMP_NAME]) do
        if not write_lump(fh,lump_list[LUMP_NAME][i],lump_list[LUMP_DATA][i]) then
          nl_printf(stdout,"failed to write lump %s\n",{lump_list[LUMP_NAME][i]})
        end if
      end for
      close(fh)
      nl_printf(stdout,"wrote %d lumps\n",{length(lump_list[LUMP_NAME])})
      nl_printf(stdout,"recovered %d lost lumps\n",{recover_count})
      nl_printf(stdout,"it is possible that other lumps were unrecoverable\n",{})
    end if
  else  
    nl_printf(stdout,"No valid lumps found.\nIt is unlikely that %s was even a lumpfile in the first place\n",{lump_filename})
  end if  
end procedure

---------------------------------------------------------------------------

function get_filename(integer ptr)
  sequence result
  integer b
  result=""
  for i=ptr to ptr+11 do
    if i>length(lump_data) then
      nl_printf(stdout,"file ended in the middle of a lumpname\n",{})
      abort(1)
    end if
    b=lump_data[i]
    if b=0 then
      --found zero-terminator, break the loop
      exit
    elsif b<33 or b>126 or find(b,"\"*/\\<>?|") or (b='.' and (find('.',result) or length(result)>8)) then
      nl_printf(stdout,"found illegal char %d (%s) in filename \"%s\"\n",{b,b,result})
      return("")
    else
      result&=b  
    end if
  end for
  if find(result,lump_list[LUMP_NAME]) then
    nl_printf(stdout,"detected repeated lump name\n",{})
  end if
  return(result)
end function

---------------------------------------------------------------------------

function try_get_name(integer ptr)
  sequence result
  integer b
  result=""
  for i=ptr to ptr+11 do
    if i>length(lump_data) then
      nl_printf(stdout,"file ended while seeking a lumpname\n",{})
      write_fixed_lumpfile()
      abort(1)
    end if
    b=lump_data[i]
    if b=0 then
      --found zero-terminator, break the loop
      exit
    elsif b<33 or b>126 or find(b,"\"*/\\<>?|") or (b='.' and (find('.',result) or length(result)>8)) then
      --found illegal char
      return("")
    elsif length(result)>=8 and not find('.',result) and b!='.' then
      --no . found
      return("")
    else
      result&=b  
    end if
  end for
  if find(result,lump_list[LUMP_NAME]) then
    --detected repeated lump name
    return("")
  end if
  --check for valid RPG file lumps
  if wildcard_match(simple_name&".*",upper(result))
  or compare("ARCHINYM.LMP",upper(result))=0
  or compare("BROWSE.TXT",upper(result))=0
  or compare("PLOTSCR.LST",upper(result))=0
  or compare("DEFPASS.BIN",upper(result))=0
    then
    return(result)
  end if
  return("")
end function

---------------------------------------------------------------------------

function quadbyte_to_dword(sequence data)
  integer result
  result=data[3]+data[4]*#100+data[1]*#10000+data[2]*#1000000
  return(result)
end function

---------------------------------------------------------------------------

function get_size(integer ptr)
  sequence quad
  integer size
  if ptr+3>length(lump_data) then
    nl_printf(stdout,"file ends in size entry\n",{})
    write_fixed_lumpfile()
    abort(1)
  end if
  quad=lump_data[ptr..ptr+3]
  size=quadbyte_to_dword(quad)
  if size>length(lump_data)-(ptr+3) then
    nl_printf(stdout,"not enough room left in file for %d byte lump\n",{size})
    return(-1)
  end if
  return(size)
end function

---------------------------------------------------------------------------

function seek_possible_name(integer ptr)
  sequence name
  integer progress
  progress=0
  corrupt=true
  nl_printf(stdout,"Lump corruption detected. Attempting to re-track data",{})
  while ptr<=length(lump_data) do
    name=try_get_name(ptr)
    if length(name) then
      nl_printf(stdout,"found possible filename candidate %s\n",{name})
      return(ptr)
    else
      ptr+=1  
    end if
    progress+=1
    if progress>=1000 then
      progress=0
      puts(stdout,".")
    end if
  end while
  return(ptr)
end function

---------------------------------------------------------------------------

function read_archinym(sequence data)
  sequence result
  sequence broken
  result=""
  broken=explode(data,"\n")
  result=upper(trim_tail(broken[1],13))
  nl_printf(stdout,"Found Archinym \"%s\"\n",{result})
  return(result)
end function

---------------------------------------------------------------------------

procedure try_unlump(integer ptr)
  sequence name
  integer size
  while ptr<=length(lump_data) do
    name=get_filename(ptr)
    if length(name) then
      ptr+=length(name)+1
      nl_printf(stdout,"lump %-12s",{name})  
      size=get_size(ptr)
      if size>=0 then
        printf(stdout," size %d\n",{size})  
        ptr+=4
        --get lump data
        lump_list[LUMP_NAME]=append(lump_list[LUMP_NAME],name)
        lump_list[LUMP_DATA]=append(lump_list[LUMP_DATA],lump_data[ptr..ptr+size-1])
        if compare(upper(name),"ARCHINYM.LMP")=0 then
          simple_name=read_archinym(lump_list[LUMP_DATA][length(lump_list[LUMP_DATA])])
        end if
        if corrupt then
          recover_count+=1
        end if
        ptr+=size
      else
        nl_printf(stdout,"invalid filesize %d\n",{size})  
        ptr=seek_possible_name(ptr)
      end if  
    else
      nl_printf(stdout,"failed to get filename\n",{})
      ptr=seek_possible_name(ptr)
    end if
  end while
  if corrupt then
    write_fixed_lumpfile()
  else  
    nl_printf(stdout,"no file corruption was detected\n",{})
  end if
end procedure

---------------------------------------------------------------------------

init()
read_file()
try_unlump(1)


