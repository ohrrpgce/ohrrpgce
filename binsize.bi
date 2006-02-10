'OHRRPGCE - Extendable recordsize arrays for use with binsize.bin
'Please read LICENSE.txt for GPL License details and disclaimer of liability

'GAME and CUSTOM share this file to keep everything in one place
'See binsize.bin documentation page on the wiki for more info

DIM defbinsize(1), curbinsize(1)

RESTORE defbinsizes
defbinsizes:
FOR i = 0 TO 1
 READ size%
 defbinsize(i) = size%
NEXT
DATA 0, 64

'-- current record size data necessary when declaring arrays
RESTORE curbinsizes
curbinsizes:
FOR i = 0 TO 1
 READ size%
 curbinsize(i) = size%
NEXT
DATA 120, 84
