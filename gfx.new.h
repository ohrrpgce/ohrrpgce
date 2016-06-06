//new backend interfaces--proposal
//started 12/22/09

#ifndef GFX_NEW_H
#define GFX_NEW_H

#include "gfx_common/config.h"

#ifdef __cplusplus
extern "C"
{
#endif

struct GFX_INIT
{
	char* szInitWindowTitle;
	char* szWindowIcon;
	void (__cdecl *PostTerminateSignal)(void);
	void (__cdecl *OnCriticalError)(const char* szError);
	void (__cdecl *SendDebugString)(const char* szMessage);
};

enum CursorVisibility {
	CV_Hidden = 0,   // (cursorHidden)  Cursor always hidden
	CV_Visible = -1, // (cursorVisible) Cursor always shown, except on touch screens
	CV_Default = -2  // (cursorDefault) Cursor shown when windowed, hidden in fullscreen
};

/////////////////////////////////////////////////////////////////////////////////////////////
//basic backend functions
int gfx_Initialize(const GFX_INIT* pCreationData); //initializes the backend; if failed, returns 0
void gfx_Shutdown(); //shuts down the backend--does not post the termination signal

int gfx_SendMessage(unsigned int msg, unsigned int dwParam, void* pvParam); //sends a message to the backend; return value depends on message sent

int gfx_GetVersion(); //returns the backend version

/////////////////////////////////////////////////////////////////////////////////////////////
//graphical functions
//presents a surface from ohr to the backend's backbuffer, converting it with the palette supplied;
//if pSurface == NULL, a maintained copy of the surface will be used
//if pPalette == NULL, a maintained copy of the palette will be used
void gfx_Present(unsigned char *pSurface, int nWidth, int nHeight, unsigned int *pPalette);

int gfx_ScreenShot(const char* szFileName); //takes a screenshot; if failed, returns 0

/////////////////////////////////////////////////////////////////////////////////////////////
//messaging functions
void gfx_PumpMessages(); //pumps the backend's message queues and polls input

void gfx_SetWindowTitle(const char* szTitle); //sets the window title; the backend may add messages to the window title to describe further option
const char* gfx_GetWindowTitle(); //returns the window title without the backend's possible additions

void gfx_SetCursorVisibility(CursorVisibility visibility); //visibility of the OS cursor over the client area
void gfx_ClipCursor(int left, int top, int right, int bottom); //clips the os cursor to the ohr rectangle, which is scaled to the client area; passing a negative for any value disables the clip

/////////////////////////////////////////////////////////////////////////////////////////////
//input functions
int gfx_GetKeyboard(int *pKeyboard); //gets the keyboard state in a format the engine understands; returns 0 on failure

int gfx_GetMouse(int& x, int& y, int& wheel, int& buttons); //gets the mouse position and button state; returns 0 on failure
int gfx_SetMouse(int x, int y); //sets the mouse position; returns 0 on failure

int gfx_GetJoystick(int nDevice, int& x, int& y, int& buttons); //gets the indexed joystick position and button state; returns 0 on failure
int gfx_SetJoystick(int nDevice, int x, int y); //sets the indexed joystick position; returns 0 on failure
int gfx_GetJoystickCount(); //returns the number of joysticks attached to the system

#ifdef __cplusplus
} //extern "C"
#endif

#endif //GFX_NEW_H
