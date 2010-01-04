//new backend interfaces--proposal
//started 12/22/09

#ifndef GFX_NEW_H
#define GFX_NEW_H

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
	int (__cdecl *DefGfxMessageProc)(unsigned int msg, unsigned int dwParam, void* pvParam);
};

int gfx_Initialize(const GFX_INIT* pCreationData); //initializes the backend; if failed, returns 0
void gfx_Close(); //closes the backend--does not post the termination signal

int gfx_SendMessage(unsigned int msg, unsigned int dwParam, void* pvParam); //sends a message to the backend; return value depends on message sent

int gfx_GetVersion(); //returns the backend version

void gfx_PumpMessages(); //pumps the backend's message queues and polls input

//presents a surface from ohr to the backend's backbuffer, converting it with the palette supplied;
//if pSurface == NULL, a maintained copy of the surface will be used
//if pPalette == NULL, a maintained copy of the palette will be used
void gfx_Present(unsigned char *pSurface, int nWidth, int nHeight, unsigned int *pPalette);

int gfx_ScreenShot(const char* szFileName); //takes a screenshot; if failed, returns 0

void gfx_SetWindowTitle(const char* szTitle); //sets the window title; the backend may add messages to the window title to describe further option
const char* gfx_GetWindowTitle(); //returns the window title without the backend's possible additions

int gfx_AcquireKeyboard(int bEnable); //alerts backend of the engine's request for keyboard input; if bEnable == 0, the keyboard is freed; returns 0 on failure
int gfx_AcquireMouse(int bEnable); //alerts backend of the engine's request for mouse input; if bEnable == 0, the mouse is freed; returns 0 on failure
//alerts backend of the engine's request for an indexed joystick;
//the backend may allow a user to order the input devices as he/she sees fit;
//if bEnable == 0, the joystick is freed;
//returns 0 on failure;
int gfx_AcquireJoystick(int bEnable, int nDevice);

int gfx_GetKeyboard(int *pKeyboard); //gets the keyboard state in a format the engine understands; returns 0 on failure

int gfx_GetMouseMovement(int& dx, int& dy, int& dWheel, int& buttons); //gets the mouse movement since the last input poll and the button state; returns 0 on failure
int gfx_GetMousePosition(int& x, int& y, int& wheel, int& buttons); //gets the mouse position and button state; returns 0 on failure
int gfx_SetMousePosition(int x, int y); //sets the mouse position; returns 0 on failure

int gfx_GetJoystickMovement(int nDevice, int& dx, int& dy, int& buttons); //gets the indexed joystick movement since last input poll and button state; returns 0 on failure
int gfx_GetJoystickPosition(int nDevice, int& x, int& y, int& buttons); //gets the indexed joystick position and button state; returns 0 on failure
int gfx_SetJoystickPosition(int nDevice, int x, int y); //sets the indexed joystick position; returns 0 on failure

#ifdef __cplusplus
} //extern "C"
#endif

#endif //GFX_NEW_H