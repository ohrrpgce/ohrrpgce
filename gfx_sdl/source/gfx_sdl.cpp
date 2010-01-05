#include "gfx_sdl.h"
#include "Graphics.h"
#include "Input.h"
#include "Window.h"
#include "_tstring.h"
using namespace gfx;

Window g_Window;
SDL g_Graphics;
Input g_Input;

int OHREventFilter(const SDL_Event* pEvent);

int gfx_Initialize(const GFX_INIT *pCreationData)
{//need to capture creation data
	if(0 == g_Graphics.Initialize())
		return 0;
	g_Window.SetEventFilter((SDL_EventFilter*)OHREventFilter);
	g_Window.SetGraphics(&g_Graphics);
	g_Window.SetInput(&g_Input);
	return 1;
}

void gfx_Shutdown()
{
	while(g_Window.PumpMessages());
	g_Graphics.Shutdown();
}

int gfx_SendMessage(unsigned int msg, unsigned int dwParam, void* pvParam)
{//need to capture understood messages
	return 0;
}

int gfx_GetVersion()
{//need to increment to 2
	return 1;
}

void gfx_PumpMessages()
{
	while(g_Window.PumpMessages());
}

void gfx_Present(unsigned char *pSurface, int nWidth, int nHeight, unsigned int *pPalette)
{
	g_Graphics.Present(pSurface, nWidth, nHeight, &Palette<Uint32>((Uint32*)pPalette, 256));
}

int gfx_ScreenShot(const char* szFileName)
{
	return g_Graphics.ScreenShot(szFileName);
}

void gfx_SetWindowTitle(const char* szTitle)
{//need to add "Press scroll lock to lock/free mouse" when appropriate
	g_Window.SetWindowTitle(szTitle);
}

const char* gfx_GetWindowTitle()
{//need to return actual title, without appended "Press scroll lock..." message
	return "";
}

int gfx_AcquireKeyboard(int bEnable)
{//needs work
	return 0;
}

int gfx_AcquireMouse(int bEnable)
{//needs work
	return 0;
}

int gfx_AcquireJoystick(int bEnable, int nDevice)
{//needs work
	return 0;
}

int gfx_GetKeyboard(int *pKeyboard)
{//needs work
	return 0;
}

int gfx_GetMouseMovement(int& dx, int& dy, int& dWheel, int& buttons)
{//needs work
	return 0;
}

int gfx_GetMousePosition(int& x, int& y, int& wheel, int& buttons)
{//needs work
	return 0;
}

int gfx_SetMousePosition(int x, int y)
{//needs work
	return 0;
}

int gfx_GetJoystickMovement(int nDevice, int& dx, int& dy, int& buttons)
{//needs work
	return 0;
}

int gfx_GetJoystickPosition(int nDevice, int& x, int& y, int& buttons)
{//needs work
	return 0;
}

int gfx_SetJoystickPosition(int nDevice, int x, int y)
{//needs work
	return 0;
}

int OHREventFilter(const SDL_Event *pEvent)
{//needs work
	switch(pEvent->type)
	{
	case SDL_QUIT: //needs to post the termination signal
		break;
	default:
		return 1;
	}
	return 0;
}