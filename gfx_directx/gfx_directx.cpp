#include "gfx_directx.new.h"
#include "gfx_directx.h"
#include "gfx_msg.h"

#include <string>

void OnCriticalError(const char* szError) {}
void SendDebugString(const char* szMessage) {}

int gfx_init(void (__cdecl *terminate_signal_handler)(void) , const char* windowicon, char* info_buffer, int info_buffer_size)
{
	GFX_INIT gfxInit = {"DirectX Backend", windowicon, terminate_signal_handler, OnCriticalError, SendDebugString};
	if(gfx_Initialize(&gfxInit) == 0)
	{
		strcpy(info_buffer, "Backend failed!");
		return 0;
	}
	strcpy(info_buffer, "Backend success!");
	return 1;
}

void gfx_close()
{
	gfx_Shutdown();
}

int gfx_getversion()
{
	return gfx_GetVersion();
}

void gfx_showpage(unsigned char *raw, int w, int h)
{
	gfx_Present(raw, w, h, 0);
	gfx_PumpMessages();
}

void gfx_setpal(unsigned int *pal)
{
	gfx_Present(0, 0, 0, pal);
	gfx_PumpMessages();
}

int gfx_screenshot(const char* fname)
{
	return gfx_ScreenShot(fname);
}

void gfx_setwindowed(int iswindow)
{
	gfx_SendMessage(OM_GFX_SETWINDOWED, iswindow, 0);
}

void gfx_windowtitle(const char* title)
{
	gfx_SetWindowTitle(title);
}

WindowState* gfx_getwindowstate()
{//there isn't an equivalent message in the new backend
	return 0;
}

int gfx_setoption(const char* opt, const char* arg)
{
	if(!opt || !arg)
		return 0;
	if(::strcmp(opt, "w") == 0 || ::strcmp(opt, "width") == 0)
		gfx_SendMessage(OM_GFX_SETWIDTH, ::atoi(arg), 0);
	else if(::strcmp(opt, "h") == 0 || ::strcmp(opt, "height") == 0)
		gfx_SendMessage(OM_GFX_SETHEIGHT, ::atoi(arg), 0);
	else if(::strcmp(opt, "f") == 0 || ::strcmp(opt, "fullscreen") == 0)
	{
		if(*arg == '0')
			gfx_SendMessage(OM_GFX_SETWINDOWED, 0, 0);
		else
			gfx_SendMessage(OM_GFX_SETWINDOWED, 1, 0);
	}
	else if(::strcmp(opt, "v") == 0 || ::strcmp(opt, "vsync") == 0)
	{
		if(*arg == '0')
			gfx_SendMessage(OM_GFX_SETVSYNC, 0, 0);
		else
			gfx_SendMessage(OM_GFX_SETVSYNC, 1, 0);
	}
	else if(::strcmp(opt, "a") == 0 || ::strcmp(opt, "aspect") == 0)
	{
		if(*arg == '0')
			gfx_SendMessage(OM_GFX_SETARP, 0, 0);
		else
			gfx_SendMessage(OM_GFX_SETARP, 1, 0);
	}
	else if(::strcmp(opt, "s") == 0 || ::strcmp(opt, "smooth") == 0)
	{
		if(*arg == '0')
			gfx_SendMessage(OM_GFX_SETSMOOTH, 0, 0);
		else
			gfx_SendMessage(OM_GFX_SETSMOOTH, 1, 0);
	}
	else if(::strcmp(opt, "ss") == 0 || ::strcmp(opt, "screenshot") == 0)
	{
		if(*arg == 'j')
			gfx_SendMessage(OM_GFX_SETSSFORMAT, 1, 0);
		else if(*arg == 'b')
			gfx_SendMessage(OM_GFX_SETSSFORMAT, 2, 0);
		else if(*arg == 'p')
			gfx_SendMessage(OM_GFX_SETSSFORMAT, 3, 0);
		else if(*arg == 'd')
			gfx_SendMessage(OM_GFX_SETSSFORMAT, 4, 0);
		else
			gfx_SendMessage(OM_GFX_SETSSFORMAT, 0, 0);
	}
	else
		return 0;
	return 2;
}

const char* gfx_describe_options()
{
	return "-w -width [x]  sets the width of the client area\n" \
		"-h -height [x]  sets the height of the client area\n" \
		"-f -fullscreen [0* | 1]  toggles fullscreen on startup\n" \
		"    the above may NOT be called before width and height\n" \
		"-v -vsync [0 | 1*]  toggles vsync\n" \
		"-a -aspect [0 | 1*]  toggles aspect ratio preservation\n" \
		"-s -smooth [0* | 1]  toggles smooth linear interpolation of display\n" \
		"-ss -screenshot [jpg | bmp | png* | dds | ohr]\n" \
		"     the above sets the screen shot format";
}

void io_init()
{//there isn't an equivalent message in the new backend
}

void io_pollkeyevents()
{
	gfx_Present(0,0,0,0);
	gfx_PumpMessages();
}

void io_updatekeys(int *keybd)
{
	gfx_GetKeyboard(keybd);
}

int io_setmousevisibility(int visible)
{
	if(visible == 0)
		gfx_HideCursor();
	else
		gfx_ShowCursor();
	return 1;
}

void io_getmouse(int& mx, int& my, int& mwheel, int& mbuttons)
{
	gfx_GetMouse(mx, my, mwheel, mbuttons);
}

void io_setmouse(int x, int y)
{
	gfx_SetMouse(x, y);
}

void io_mouserect(int xmin, int xmax, int ymin, int ymax)
{
	gfx_ClipCursor(xmin, ymin, xmax, ymax);
}

int io_readjoysane(int joynum, int& button, int& x, int& y)
{
	return gfx_GetJoystick(joynum, x, y, button);
}