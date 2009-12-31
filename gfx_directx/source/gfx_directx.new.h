//gfx_directx.new.h
//by Jay Tennant 12/29/09
//restructure of backend interfaces, applying to directx backend

#ifndef GFX_DIRECTX_NEW_H
#define GFX_DIRECTX_NEW_H

#define DLLEXPORT __declspec(dllexport)

#ifdef __cplusplus
extern "C"
{
#endif

struct GFX_INIT
{
	const char* szInitWindowTitle;
	const char* szWindowIcon;
	void (__cdecl *PostTerminateSignal)(void);
	void (__cdecl *OnCriticalError)(const char* szError);
	void (__cdecl *SendDebugString)(const char* szMessage);
	int (__cdecl *DefGfxMessageProc)(unsigned int msg, unsigned int dwParam, void* pvParam);
};

struct GFX_PREFERENCES
{
	int top; //window top
	int left; //window left
	int width; //client width
	int height; //client height
	int bAspectRatioPreservation; //0 = false; if true, aspect ratio preservation is enabled
	int bFullscreen; //0 = false; if true, backend preference is fullscreen
	int bSmooth; //0 = false; if true, backend preference is smooth linear interpolation
	int bVsync; //0 = false; if true, backend preference is vsync enabled
	int nScreenshotFormat; //0 = ohr bmp, 1 = jpg, 2 = bmp, 3 = png, 4 = dds
};

DLLEXPORT int gfx_Initialize(const GFX_INIT* pCreationData); //initializes the backend; if failed, returns 0
DLLEXPORT void gfx_Close(); //closes the backend--does not post the termination signal

DLLEXPORT void gfx_SetPreferences(const GFX_PREFERENCES* pPreferences); //sets the preferences for a backend
DLLEXPORT void gfx_GetPreferences(GFX_PREFERENCES* pPreferences); //gets the preferences of a backend

DLLEXPORT int gfx_SendMessage(unsigned int msg, unsigned int dwParam, void* pvParam); //sends a message to the backend; return value depends on message sent

DLLEXPORT int gfx_GetVersion(); //returns the backend version

DLLEXPORT void gfx_PumpMessages(); //pumps the backend's message queues and polls input

//presents a surface from ohr to the backend's backbuffer, converting it with the palette supplied;
//if pSurface == NULL, a maintained copy of the surface will be used
//if pPalette == NULL, a maintained copy of the palette will be used
DLLEXPORT void gfx_Present(unsigned char *pSurface, int nWidth, int nHeight, unsigned int *pPalette);

DLLEXPORT int gfx_ScreenShot(const char* szFileName); //takes a screenshot; if failed, returns 0

DLLEXPORT void gfx_SetWindowTitle(const char* szTitle); //sets the window title; the backend may add messages to the window title to describe further option
DLLEXPORT const char* gfx_GetWindowTitle(); //returns the window title without the backend's possible additions

DLLEXPORT int gfx_AcquireKeyboard(int bEnable); //alerts backend of the engine's request for keyboard input; if bEnable == 0, the keyboard is freed; returns 0 on failure
DLLEXPORT int gfx_AcquireMouse(int bEnable); //alerts backend of the engine's request for mouse input; if bEnable == 0, the mouse is freed; returns 0 on failure
//alerts backend of the engine's request for an indexed joystick;
//the backend may allow a user to order the input devices as he/she sees fit;
//if bEnable == 0, the joystick is freed;
//returns 0 on failure;
DLLEXPORT int gfx_AcquireJoystick(int bEnable, int nDevice);

DLLEXPORT int gfx_GetKeyboard(int *pKeyboard); //gets the keyboard state in a format the engine understands; returns 0 on failure

DLLEXPORT int gfx_GetMouseMovement(int& dx, int& dy, int& dWheel, int& buttons); //gets the mouse movement since the last input poll and the button state; returns 0 on failure
DLLEXPORT int gfx_GetMousePosition(int& x, int& y, int& wheel, int& buttons); //gets the mouse position and button state; returns 0 on failure
DLLEXPORT int gfx_SetMousePosition(int x, int y); //sets the mouse position; returns 0 on failure

DLLEXPORT int gfx_GetJoystickMovement(int nDevice, int& dx, int& dy, int& buttons); //gets the indexed joystick movement since last input poll and button state; returns 0 on failure
DLLEXPORT int gfx_GetJoystickPosition(int nDevice, int& x, int& y, int& buttons); //gets the indexed joystick position and button state; returns 0 on failure
DLLEXPORT int gfx_SetJoystickPosition(int nDevice, int x, int y); //sets the indexed joystick position; returns 0 on failure

#ifdef __cplusplus
} //extern "C"
#endif

#endif