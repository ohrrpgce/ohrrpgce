#include "gfx.new.h"
#include "gfx.old.h"
#include "gfx_msg.h"

#include <string>

void OnCriticalError(const char* szError) {}
void SendDebugString(const char* szMessage) {}
int DefMsgProc(unsigned int msg, unsigned int dwParam, void *pvParam) {return 0;}

int gfx_init(void (__cdecl *terminate_signal_handler)(void) , const char* windowicon, char* info_buffer, int info_buffer_size)
{
	GFX_INIT gfxInit = {"SDL Backend", windowicon, terminate_signal_handler, OnCriticalError, SendDebugString, DefMsgProc};
	return gfx_Initialize(&gfxInit);
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
	if(::strcmp(opt, "z") == 0 || ::strcmp(opt, "zoom") == 0)
		gfx_SendMessage(OM_GFX_SETZOOM, ::atoi(arg), 0);
	else if(::strcmp(opt, "s") == 0 || ::strcmp(opt, "smooth") == 0)
	{
		gfx_SendMessage(OM_GFX_SETSMOOTH, ::atoi(arg), 0);
		return 1;
	}
	else
		return 0;
	return 2;
}

const char* gfx_describe_options()
{
	return "-z -zoom [1|2|3|4]  Scale screen to 1,2,3 or 4x normal size (2x default)\n" \
		   "-s -smooth          Enable smoothing filter for zoom modes (default off)";
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
	return gfx_AcquireMouse(visible == 0 ? 1 : 0);
}

void io_getmouse(int& mx, int& my, int& mwheel, int& mbuttons)
{
	gfx_GetMousePosition(mx, my, mwheel, mbuttons);
}

void io_setmouse(int x, int y)
{
	gfx_SetMousePosition(x, y);
}

void io_mouserect(int xmin, int xmax, int ymin, int ymax)
{//there isn't an equivalent message in the new backend
}

int io_readjoysane(int joynum, int& button, int& x, int& y)
{
	return gfx_GetJoystickPosition(joynum, x, y, button);
}