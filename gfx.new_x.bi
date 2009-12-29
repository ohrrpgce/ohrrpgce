'new backend interfaces--proposal
'started 12/22/09

extern "C"

type GFX_INIT
	szInitWindowTitle as zstring ptr
	szWindowIcon as zstring ptr
	PostTerminateSignal as sub cdecl()
	OnCriticalError as sub cdecl(byval szError as zstring ptr)
	end type

type GFX_PREFERENCES
	top as integer 'window top
	left as integer 'window left
	width as integer 'client width
	height as integer 'client height
	bAspectRatioPreservation as integer '0 = false; if true, aspect ratio preservation is enabled
	bFullscreen as integer '0 = false; if true, backend preference is fullscreen
	bSmooth as integer '0 = false; if true, backend preference is smooth linear interpolation
	bVsync as integer '0 = false; if true, backend preference is vsync enabled
	nScreenshotFormat as integer '0 = ohr bmp, 1 = jpg, 2 = bmp, 3 = png, 4 = dds
	end type

DECLARE FUNCTION gfx_x_Initialize (byval pCreationData as const GFX_INIT ptr) as integer 'initializes the backend; if failed, returns 0
DECLARE SUB gfx_x_Close () 'closes the backend--does not post the termination signal

DECLARE SUB gfx_x_SetPreferences (byval pPreferences as const GFX_PREFERENCES ptr) 'sets the preferences for a backend
DECLARE SUB gfx_x_GetPreferences (byval pPreferences as GFX_PREFERENCES ptr) 'gets the preferences of a backend

DECLARE FUNCTION gfx_x_GetVersion () as integer 'returns the backend version

DECLARE SUB gfx_x_PumpMessages () 'pumps the backend's message queues and polls input

'presents a surface from ohr to the backend's backbuffer, converting it with the palette supplied;
'if pSurface == NULL, a maintained copy of the surface will be used
'if pPalette == NULL, a maintained copy of the palette will be used
DECLARE SUB gfx_x_Present (byval pSurface as ubyte ptr, byval nWidth as integer, byval nHeight as integer, byval pPalette as RGBcolor ptr)

DECLARE FUNCTION gfx_x_ScreenShot (byval szFileName as const zstring ptr) as integer 'takes a screenshot; if failed, returns 0

DECLARE SUB gfx_x_SetWindowTitle (byval szTitleconst as const zstring ptr) 'sets the window title; the backend may add messages to the window title to describe further option
DECLARE FUNCTION gfx_x_GetWindowTitle () as const zstring ptr 'returns the window title without the backend's possible additions

DECLARE FUNCTION gfx_x_AcquireKeyboard (byval bEnable as integer) as integer 'alerts backend of the engine's request for keyboard input; if bEnable == 0, the keyboard is freed; returns 0 on failure
DECLARE FUNCTION gfx_x_AcquireMouse (byval bEnable as integer) as integer 'alerts backend of the engine's request for mouse input; if bEnable == 0, the mouse is freed; returns 0 on failure
'alerts backend of the engine's request for an indexed joystick;
'the backend may allow a user to order the input devices as he/she sees fit;
'if bEnable == 0, the joystick is freed;
'returns 0 on failure;
DECLARE FUNCTION gfx_x_AcquireJoystick (byval bEnable as integer, byval nDevice as integer) as integer

DECLARE FUNCTION gfx_x_GetKeyboard (byval pKeyboard as integer ptr) as integer 'gets the keyboard state in a format the engine understands; returns 0 on failure

DECLARE FUNCTION gfx_x_GetMouseMovement (byref dx as integer, byref dy as integer, byref dWheel as integer, byref buttons as integer) as integer 'gets the mouse movement since the last input poll and the button state; returns 0 on failure
DECLARE FUNCTION gfx_x_GetMousePosition (byref x as integer, byref y as integer, byref wheel as integer, byref buttons as integer) as integer 'gets the mouse position and button state; returns 0 on failure
DECLARE FUNCTION gfx_x_SetMousePosition (byval x as integer, byval y as integer) as integer 'sets the mouse position; returns 0 on failure

DECLARE FUNCTION gfx_x_GetJoystickMovement (byval nDevice as integer, byref dx as integer, byref dy as integer, byref buttons as integer) as integer 'gets the indexed joystick movement since last input poll and button state; returns 0 on failure
DECLARE FUNCTION gfx_x_GetJoystickPosition (byval nDevice as integer, byref x as integer, byref y as integer, byref buttons as integer) as integer 'gets the indexed joystick position and button state; returns 0 on failure
DECLARE FUNCTION gfx_x_SetJoystickPosition (byval nDevice as integer, byval x as integer, byval y as integer) as integer 'sets the indexed joystick position; returns 0 on failure

end extern