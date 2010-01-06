#include "gfx_sdl.h"
#include "Video.h"
#include "Input.h"
#include "Window.h"
#include "_tstring.h"
using namespace gfx;

Window g_Window;
Video g_Video;
Input g_Input;
struct gfx_BackendState
{
	tstring szWindowTitle;
	tstring szWindowIcon;
	void (__cdecl *PostTerminateSignal)(void);
	void (__cdecl *OnCriticalError)(const char* szError);
	void (__cdecl *SendDebugString)(const char* szMessage);
	int (__cdecl *DefGfxMessageProc)(unsigned int msg, unsigned int dwParam, void* pvParam);
} g_State;

int OHREventFilter(const SDL_Event* pEvent);
int OHREventProc(const SDL_Event* pEvent);

int gfx_Initialize(const GFX_INIT *pCreationData)
{
	if(!pCreationData)
		return 0;
	g_State.szWindowTitle = pCreationData->szInitWindowTitle;
	g_State.szWindowIcon = pCreationData->szWindowIcon;
	g_State.PostTerminateSignal = pCreationData->PostTerminateSignal;
	g_State.OnCriticalError = pCreationData->OnCriticalError;
	g_State.SendDebugString = pCreationData->SendDebugString;
	g_State.DefGfxMessageProc = pCreationData->DefGfxMessageProc;

	if(g_State.PostTerminateSignal == 0 || g_State.OnCriticalError == 0 || g_State.SendDebugString == 0 || g_State.DefGfxMessageProc == 0)
		return 0;
	if(0 == g_Video.Initialize())
	{
		g_Video.Shutdown();
		return 0;
	}
	g_Window.SetEventFilter((SDL_EventFilter)OHREventFilter);
	g_Window.SetEventProc((SDL_EventProc)OHREventProc);
	g_Window.PumpMessages();
	return 1;
}

void gfx_Shutdown()
{
	g_Window.PumpMessages();
	g_Video.Shutdown();
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
{//need to poll input devices, too
	g_Window.PumpMessages();
}

void gfx_Present(unsigned char *pSurface, int nWidth, int nHeight, unsigned int *pPalette)
{
	g_Video.Present(pSurface, nWidth, nHeight, &Palette<Uint32>((Uint32*)pPalette, 256));
}

int gfx_ScreenShot(const char* szFileName)
{
	return g_Video.ScreenShot(szFileName);
}

void gfx_SetWindowTitle(const char* szTitle)
{//need to adjust conditions for "Press scroll lock to lock/free mouse"
	g_State.szWindowTitle = szTitle;
	tstring str = g_State.szWindowTitle;
	if(false)
		str += T_TEXT("Press Scroll Lock to free the mouse...");
	else if(false)
		str += T_TEXT("Press Scroll Lock to lock the mouse...");
	g_Window.SetWindowTitle(str);
}

const char* gfx_GetWindowTitle()
{//static char buffer won't die off
	static char buffer[256] = "";
	return TcharToChar(buffer, 256, g_State.szWindowTitle.c_str(), 0);
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

//executes as a filter for the sdl message proc (before OHREventProc())
int OHREventFilter(const SDL_Event *pEvent)
{//needs work
	switch(pEvent->type)
	{
	case SDL_QUIT:
		g_State.PostTerminateSignal();
		break;
	default:
		return 1;
	}
	return 0;
}

//executes as the main sdl message proc (after the filter OHREventFilter())
int OHREventProc(const SDL_Event* pEvent)
{//needs work
	switch(pEvent->type)
	{
	default:
		return 1;
	}
	return 0;
}