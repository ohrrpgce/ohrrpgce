//gfx_directx.new.h
//by Jay Tennant 12/29/09
//restructure of backend interfaces, applying to directx backend

#ifndef GFX_DIRECTX_NEW_H
#define GFX_DIRECTX_NEW_H

#define DLLEXPORT __declspec(dllexport)

#include "../errorlevel.h"

#ifdef __cplusplus
extern "C"
{
#endif

struct WindowState
{
	int structsize;    // Number of members
	int focused;
	int minimised;
	int fullscreen;
	int user_toggled_fullscreen;
};
#define WINDOWSTATE_SZ 5


struct GfxInitData
{
	int structsize;    // Number of members
	const char* windowtitle;
	const char* windowicon;
	void (__cdecl *PostTerminateSignal)(void);
	void (__cdecl *DebugMsg)(ErrorLevel errlvl, const char* message);
};
#define GFXINITDATA_SZ 5

/////////////////////////////////////////////////////////////////////////////////////////////
//basic backend functions
DLLEXPORT int gfx_Initialize(const GfxInitData* pCreationData); //initializes the backend; if failed, returns 0
DLLEXPORT void gfx_Shutdown(); //shuts down the backend--does not post the termination signal

DLLEXPORT int gfx_SendMessage(unsigned int msg, unsigned int dwParam, void* pvParam); //sends a message to the backend; return value depends on message sent

DLLEXPORT int gfx_GetVersion(); //returns the backend version

/////////////////////////////////////////////////////////////////////////////////////////////
//graphical functions
//presents a surface from ohr to the backend's backbuffer, converting it with the palette supplied;
//if pSurface == NULL, a maintained copy of the surface will be used
//if pPalette == NULL, a maintained copy of the palette will be used
DLLEXPORT void gfx_Present(unsigned char *pSurface, int nWidth, int nHeight, unsigned int *pPalette);

DLLEXPORT int gfx_ScreenShot(const char* szFileName); //takes a screenshot; if failed, returns 0

/////////////////////////////////////////////////////////////////////////////////////////////
//messaging functions
DLLEXPORT void gfx_PumpMessages(); //pumps the backend's message queues and polls input

DLLEXPORT void gfx_SetWindowTitle(const char* szTitle); //sets the window title; the backend may add messages to the window title to describe further option
DLLEXPORT const char* gfx_GetWindowTitle(); //returns the window title without the backend's possible additions
DLLEXPORT void gfx_GetWindowState(WindowState *pState); //returns window information

DLLEXPORT void gfx_ClipCursor(int left, int top, int right, int bottom); //clips the os cursor to the ohr rectangle, which is scaled to the client area; passing a negative for any value disables the clip

/////////////////////////////////////////////////////////////////////////////////////////////
//input functions
DLLEXPORT int gfx_GetKeyboard(int *pKeyboard); //gets the keyboard state in a format the engine understands; returns 0 on failure
DLLEXPORT void gfx_GetText(wchar_t *pBuffer, int buffenLen); //gets the textual input since the last call, stores it in a buffer which can hold len-1 characters

DLLEXPORT int gfx_GetMouse(int& x, int& y, int& wheel, int& buttons); //gets the mouse position and button state; returns 0 on failure
DLLEXPORT int gfx_SetMouse(int x, int y); //sets the mouse position; returns 0 on failure

DLLEXPORT int gfx_GetJoystick(int nDevice, int& x, int& y, int& buttons); //gets the indexed joystick position and button state; returns 0 on failure
DLLEXPORT int gfx_SetJoystick(int nDevice, int x, int y); //sets the indexed joystick position; returns 0 on failure
DLLEXPORT int gfx_GetJoystickCount(); //returns the number of joysticks attached to the system

#ifdef __cplusplus
} //extern "C"
#endif

#endif
