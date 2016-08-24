' New backend interfaces (proposal, not used yet)
' Also, gfx.new_x.bi contains function declarations instead of
' function pointer declarations.

'gfx_GetVersion() and gfx_getversion(), and gfx_ScreenShot() and gfx_screenshot()
'collide, so are disabled for now

extern "C"

type GfxInitData
	structsize as integer    ' Number of members
	windowtitle as zstring ptr
	windowicon as zstring ptr
	PostTerminateSignal as sub cdecl()
	DebugMsg as sub cdecl(errlvl as integer, byval message as const zstring ptr)
end type
#define GFXINITDATA_SZ 5

extern gfx_Initialize as function (byval pCreationData as const GfxInitData ptr) as integer 'initializes the backend; if failed, returns 0
extern gfx_Shutdown as sub () 'shuts down the backend--does not post the termination signal

extern gfx_SendMessage as function (byval msg as unsigned integer, byval dwParam as unsigned integer, byval pvParam as Any ptr) as integer 'sends a message to the backend; return value depends on message sent

'extern gfx_GetVersion as function () as integer 'returns the backend version

extern gfx_PumpMessages as sub () 'pumps the backend's message queues and polls input

'presents a surface from ohr to the backend's backbuffer, converting it with the palette supplied;
'if pSurface == NULL, a maintained copy of the surface will be used
'if pPalette == NULL, a maintained copy of the palette will be used
'extern gfx_Present as sub (byval pSurface as ubyte ptr, byval nWidth as integer, byval nHeight as integer, byval pPalette as RGBcolor ptr)

'extern gfx_ScreenShot as function (byval szFileName as const zstring ptr) as integer 'takes a screenshot; if failed, returns 0

extern gfx_SetWindowTitle as sub (byval szTitleconst as const zstring ptr) 'sets the window title; the backend may add messages to the window title to describe further option
extern gfx_GetWindowTitle as function () as const zstring ptr 'returns the window title without the backend's possible additions
'extern gfx_GetWindowState as sub (byval nID as integer, byval pState as WindowState ptr) 'return information for the specified window

extern gfx_AcquireKeyboard as function (byval bEnable as integer) as integer 'alerts backend of the engine's request for keyboard input; if bEnable == 0, the keyboard is freed; returns 0 on failure
extern gfx_AcquireMouse as function (byval bEnable as integer) as integer 'alerts backend of the engine's request for mouse input; if bEnable == 0, the mouse is freed; returns 0 on failure
'alerts backend of the engine's request for an indexed joystick;
'the backend may allow a user to order the input devices as he/she sees fit;
'if bEnable == 0, the joystick is freed;
'returns 0 on failure;
extern gfx_AcquireJoystick as function (byval bEnable as integer, byval nDevice as integer) as integer
extern gfx_AcquireTextInput as function (byval bEnabled as integer) as integer  'sets whether text input translation is enabled, returns 0 on failure

extern gfx_GetKeyboard as function (byval pKeyboard as integer ptr) as integer 'gets the keyboard state in a format the engine understands; returns 0 on failure
extern gfx_GetText as sub (byval pBuffer as wstring ptr, byval buffenLen as integer)  'gets the textual input since the last call, stores it in a buffer which can hold len-1 characters

extern gfx_GetMouseMovement as function (byref dx as integer, byref dy as integer, byref dWheel as integer, byref buttons as integer) as integer 'gets the mouse movement since the last input poll and the button state; returns 0 on failure
extern gfx_GetMousePosition as function (byref x as integer, byref y as integer, byref wheel as integer, byref buttons as integer) as integer 'gets the mouse position and button state; returns 0 on failure
extern gfx_SetMousePosition as function (byval x as integer, byval y as integer) as integer 'sets the mouse position; returns 0 on failure

extern gfx_GetJoystickMovement as function (byval nDevice as integer, byref dx as integer, byref dy as integer, byref buttons as integer) as integer 'gets the indexed joystick movement since last input poll and button state; returns 0 on failure
extern gfx_GetJoystickPosition as function (byval nDevice as integer, byref x as integer, byref y as integer, byref buttons as integer) as integer 'gets the indexed joystick position and button state; returns 0 on failure
extern gfx_SetJoystickPosition as function (byval nDevice as integer, byval x as integer, byval y as integer) as integer 'sets the indexed joystick position; returns 0 on failure

end extern
