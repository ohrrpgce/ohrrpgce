#include "gfx_directx.new.h"
#include "gfx_directx.h"

void OnCriticalError(const char* szError) {}
void SendDebugString(const char* szMessage) {}

int gfx_init(void (__cdecl *terminate_signal_handler)(void) , const char* windowicon, char* info_buffer, int info_buffer_size)
{
	GFX_INIT gfxInit = {"DirectX Backend", windowicon, terminate_signal_handler, OnCriticalError, SendDebugString};
	return gfx_Initialize(&gfxInit);
}

void gfx_close()
{
	gfx_Close();
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
{//there isn't an equivalent message in the new backend
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
{//this is replaced with the preference structure
	return 0;
}

const char* gfx_describe_options()
{//this is replaced with the preference structure
	return "Command line options have been deprecated for this backend.";
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