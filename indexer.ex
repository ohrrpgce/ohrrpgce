--OHRRPGCE INDEXER - Utility to extract index from plotdictionary.html
--(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
--Please read LICENSE.txt for GPL License details and disclaimer of liability
include wildcard.e
include sort.e

integer fh
object line
integer at
sequence short,long
sequence list
sequence inputfile,outputfile
integer mode

inputfile="plotdictionary.html"
outputfile="plotdict.txt"

list={}
mode=0

fh=open(inputfile,"r")

if fh!=-1 then
 printf(1,"Opened %s for read\n",{inputfile})  
 while 1 do
  line=gets(fh)
  if sequence(line) then

    if mode=0 then
      at=match("<a name=\"about-",line)
      if at=1 then
        printf(1,"/",{})
        short=""
        long=""
        for i=16 to length(line) do
          if line[i]='\"' then
            exit
          end if
          short&=line[i]
        end for
        mode=1
      end if
    elsif mode=1 then
      at=match("<b><font color=\"yellow\">",line)
      if at=1 then
        printf(1,"\\",{})
        for i=25 to length(line) do
          if line[i]='<' then
            exit
          end if
          long&=line[i]
        end for
        list=append(list,{lower(long),long,short})
        mode=0
      end if
    end if
  else
    exit    
  end if
 end while
 printf(1,"Closed %s\n",{inputfile})
close(fh)

 list=sort(list)

 fh=open(outputfile,"w")

 if fh!=-1 then
   for i=1 to length(list) do
     if length(list[i][1]) then
       printf(fh,"<a href=\"#about-%s\">%s</a><br>\n",{list[i][3],list[i][2]})
     end if
   end for
   close(fh)
 else
   printf(1,"Cant open %s for write\n",{outputfile})  
 end if

else
  printf(1,"Cant open %s for read\n",{inputfile})  
end if

