#include "gfx.new.h"
#include "Video.h"
#include "Input.h"
#include "Window.h"
#include "gfx_osmouse.h"
#include "_tstring.h"
#include "sdl2fb.h"
#include "gfx_msg.h"
using namespace gfx;

#pragma comment(lib, "SDL.lib")

Window g_Window;
Video g_Video;
Input g_Input;
OSMouse g_OSMouse;
struct gfx_BackendState
{
	tstring szWindowTitle;
	tstring szWindowIcon;
	void (__cdecl *PostTerminateSignal)(void);
	void (__cdecl *OnCriticalError)(const char* szError);
	void (__cdecl *SendDebugString)(const char* szMessage);
	int (__cdecl *DefGfxMessageProc)(unsigned int msg, unsigned int dwParam, void* pvParam);
	bool bBlockOhrMouseInput; //toggles ohr mouse control
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
	SDL_QuitEvent qe;
	qe.type = SDL_QUIT;
	g_Window.PushEvent((SDL_Event*)&qe);
	g_Window.PumpMessages();

	g_Video.Shutdown();
}

int gfx_SendMessage(unsigned int msg, unsigned int dwParam, void* pvParam)
{//need to finish setting/getting bit depth
	switch(msg)
	{
	case OM_GFX_SETWIDTH:
		g_Video.SetResolution(dwParam, g_Video.GetResolution().h);
		break;
	case OM_GFX_SETHEIGHT:
		g_Video.SetResolution(g_Video.GetResolution().w, dwParam);
		break;
	case OM_GFX_SETCLIENTAREA:
		g_Video.SetResolution(dwParam, (int)pvParam);
		break;
	case OM_GFX_SETARP:
		g_Video.SetAspectRatioPreservation(dwParam != 0);
		break;
	case OM_GFX_SETWINDOWED:
		g_Video.SetView(dwParam != 0);
		break;
	case OM_GFX_SETSMOOTH:
		g_Video.SetSmooth(dwParam != 0);
		break;
	case OM_GFX_SETZOOM:
		g_Video.SetResolution(320 * dwParam, 200 & dwParam);
		break;
	case OM_GFX_SETBITDEPTH:
		break;

	case OM_GFX_GETWIDTH:
		return g_Video.GetResolution().w;
	case OM_GFX_GETHEIGHT:
		return g_Video.GetResolution().h;
	case OM_GFX_GETCLIENTAREA:
		return (g_Video.GetResolution().w << 16) | g_Video.GetResolution().h;
	case OM_GFX_GETARP:
		return g_Video.IsAspectRatioPreserved() ? 1 : 0;
	case OM_GFX_GETWINDOWED:
		return g_Video.IsViewFullscreen() ? 0 : 1;
	case OM_GFX_GETSMOOTH:
		return g_Video.IsSmooth() ? 1 : 0;
	case OM_GFX_GETZOOM:
		return g_Video.GetResolution().w / 320;
	case OM_GFX_GETBITDEPTH:
		return 32;
	default:
		return g_State.DefGfxMessageProc(msg, dwParam, pvParam);
	}
	return 1;
}

int gfx_GetVersion()
{//need to support v1 and v2
	return 1;
}

void gfx_PumpMessages()
{
	g_Window.PumpMessages();
	if(SDL_GetAppState() & SDL_APPINPUTFOCUS)
	{
		g_Input.PollKeyboard();
		if(!g_State.bBlockOhrMouseInput)
			g_Input.PollMouse();
	}
}

void gfx_Present(unsigned char *pSurface, int nWidth, int nHeight, unsigned int *pPalette)
{
	g_Video.Present(pSurface, nWidth, nHeight, (pPalette ? &Palette<Uint32>((Uint32*)pPalette, 256) : NULL));
}

int gfx_ScreenShot(const char* szFileName)
{
	return g_Video.ScreenShot(szFileName);
}

void gfx_SetWindowTitle(const char* szTitle)
{
	g_State.szWindowTitle = szTitle;
	tstring str = g_State.szWindowTitle;
	if(!g_State.bBlockOhrMouseInput && g_OSMouse.IsOHRMouseActive())
		str += T_TEXT("Press Scroll Lock to free the mouse...");
	else if(g_State.bBlockOhrMouseInput && g_OSMouse.IsOHRMouseActive())
		str += T_TEXT("Press Scroll Lock to lock the mouse...");
	g_Window.SetWindowTitle(str);
}

const char* gfx_GetWindowTitle()
{//static char buffer won't die off
	static char buffer[256] = "";
	return TcharToChar(buffer, 256, g_State.szWindowTitle.c_str(), 0);
}

int gfx_AcquireKeyboard(int bEnable)
{//acquired by default
	return 1;
}

int gfx_AcquireMouse(int bEnable)
{
	g_OSMouse.OHRMouseActive(bEnable != 0 ? true : false);
	char buffer[256] = "";
	gfx_SetWindowTitle(TcharToChar(buffer, 256, g_State.szWindowTitle.c_str(), 0));
	return 1;
}

int gfx_AcquireJoystick(int bEnable, int nDevice)
{//needs work
	return 0;
}

int gfx_GetKeyboard(int *pKeyboard)
{
	for(int i = 0; i < 128; i++)
		pKeyboard[i] = 0;
	Uint8* pSdlKeys = g_Input.GetKeyboardState();
	for(int i = 0; i < 322; i++)
		if(pSdlKeys[i] != 0)
			pKeyboard[sdl2fb(i)] |= 0x8; //sdl2fb() converts sdl keysym into fb scancode
	return 0;
}

int gfx_GetMouseMovement(int& dx, int& dy, int& dWheel, int& buttons)
{
	dx = g_Input.GetMouseXChange();
	dy = g_Input.GetMouseYChange();
	dWheel = g_Input.GetMouseWheelChange(); //not meaningful; wheel change is always 0
	buttons = 0x0;
	buttons |= (g_Input.IsMouseLButtonDown() ? 0x1 : 0);
	buttons |= (g_Input.IsMouseRButtonDown() ? 0x2 : 0);
	buttons |= (g_Input.IsMouseMButtonDown() ? 0x4 : 0);
	return 1;
}

int gfx_GetMousePosition(int& x, int& y, int& wheel, int& buttons)
{
	x = g_Input.GetMouseX();
	y = g_Input.GetMouseY();
	wheel = g_Input.GetMouseWheel();
	buttons = 0x0;
	buttons |= (g_Input.IsMouseLButtonDown() ? 0x1 : 0);
	buttons |= (g_Input.IsMouseRButtonDown() ? 0x2 : 0);
	buttons |= (g_Input.IsMouseMButtonDown() ? 0x4 : 0);
	return 1;
}

int gfx_SetMousePosition(int x, int y)
{
	g_Input.SetMouseX(x);
	g_Input.SetMouseY(y);
	return 1;
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
{
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
{
	switch(pEvent->type)
	{
	case SDL_MOUSEBUTTONDOWN:
		{
			if(SDL_GetAppState() & SDL_APPINPUTFOCUS)
			{
				if(pEvent->button.button == SDL_BUTTON_WHEELUP)
					g_Input.SetMouseWheel(g_Input.GetMouseWheel() + 1);
				else if(pEvent->button.button == SDL_BUTTON_WHEELDOWN)
					g_Input.SetMouseWheel(g_Input.GetMouseWheel() - 1);
			}
		} break;
	case SDL_KEYDOWN:
		{
			if(SDL_GetAppState() & SDL_APPINPUTFOCUS)
			{
				if(pEvent->key.state == SDL_PRESSED) //does this check against the previous state of the key?
				{
					if(pEvent->key.keysym.mod & KMOD_ALT) //alt key down
					{
						if(pEvent->key.keysym.sym == SDLK_RETURN) //alt-enter
							g_Video.SetView(g_Video.IsViewFullscreen());
						g_OSMouse.Fullscreen(g_Video.IsViewFullscreen());
					}
					if(pEvent->key.keysym.sym == SDLK_SCROLLOCK) //scroll lock
					{
						g_State.bBlockOhrMouseInput = !g_State.bBlockOhrMouseInput;
						if(g_State.bBlockOhrMouseInput)
							g_OSMouse.Push_State();
						else
							g_OSMouse.Pop_State();
						char buffer[256] = "";
						gfx_SetWindowTitle(TcharToChar(buffer, 256, g_State.szWindowTitle.c_str(), 0));
					}
				}
			}
		} break;
	case SDL_VIDEORESIZE:
		{//needs work
		} break;
	default:
		return 1;
	}
	return 0;
}
