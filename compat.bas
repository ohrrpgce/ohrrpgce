'OHRRPGCE GAME - Compatibility functions, FreeBasic version
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

#include compat.bi
#include allmodex.bi
#include fontdata.bi

sub getdefaultfont(font() as integer)
	dim i as integer
	
	for i = 0 to 1023
		font(i) = font_data(i)
	next
end sub