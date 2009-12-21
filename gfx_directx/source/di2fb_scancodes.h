int di2fb(int diScancode)
{
	if(diScancode == 0xB8) //Right ALT, no Right ALT in fb
		return 0x38;
	else if(diScancode == 0x9D) //Right CNTRL, no Right CNTRL in fb
		return 0x1D;
	else if(diScancode < 0x47)
		return diScancode;
	else if(diScancode > 0xC6 && diScancode < 0xDE)
		return diScancode - 0x80;
	else if(diScancode == 0x4E || diScancode == 0x57 || diScancode == 0x58) //Numeric pad Plus, F11, F12
		return diScancode;
	else if(diScancode == 0x9C) //'enter' on numeric pad maps to 'enter' on main keyboard
		return 0x1C;
	else if(false) //I don't know what to check for
		return 0x64;
	return 0;
}

int fb2di(int fbScancode)
{
	if(fbScancode < 0x47)
		return fbScancode;
	else if(fbScancode == 0x4E || fbScancode == 0x57 || fbScancode == 0x58) //Numeric pad Plus, F11, F12
		return fbScancode;
	else if(fbScancode == 0x64) //I don't know what this is
		return 0;
	else if(fbScancode > 0x5E)
		return 0;
	return fbScancode + 0x80;
}