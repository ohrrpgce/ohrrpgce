' New backend interfaces (proposal, not used yet)
' This file should not normally be included; it's just a template of the
' necessary functions to define in a shared-library gfx backend, but doesn't
' do the necessary dllexporting.
' Include gfx.new.bi instead.

extern "C"

type GfxInitData
	structsize as integer    ' Number of members
	windowtitle as zstring ptr
	windowicon as zstring ptr
	PostTerminateSignal as sub cdecl()
	DebugMsg as sub cdecl(errlvl as integer, byval message as const zstring ptr)
end type
#define GFXINITDATA_SZ 5

#ifdef GFX_EXTERNAL_DLL

DECLARE FUNCTION gfx_Initialize (byval pCreationData as const GfxInitData ptr) as integer 'initializes the backend; if failed, returns 0
DECLARE SUB gfx_Shutdown () 'shuts down the backend--does not post the termination signal

DECLARE FUNCTION gfx_SendMessage (byval msg as unsigned integer, byval dwParam as unsigned integer, byval pvParam as Any ptr) as integer 'sends a message to the backend; return value depends on message sent

DECLARE FUNCTION gfx_GetVersion () as integer 'returns the backend version

DECLARE SUB gfx_PumpMessages () 'pumps the backend's message queues and polls input

'presents a surface from ohr to the backend's backbuffer, converting it with the palette supplied;
'if pSurface == NULL, a maintained copy of the surface will be used
'if pPalette == NULL, a maintained copy of the palette will be used
DECLARE SUB gfx_Present (byval pSurface as ubyte ptr, byval nWidth as integer, byval nHeight as integer, byval pPalette as RGBcolor ptr)

DECLARE FUNCTION gfx_ScreenShot (byval szFileName as const zstring ptr) as integer 'takes a screenshot; if failed, returns 0

DECLARE SUB gfx_SetWindowTitle (byval szTitleconst as const zstring ptr) 'sets the window title; the backend may add messages to the window title to describe further option
DECLARE FUNCTION gfx_GetWindowTitle () as const zstring ptr 'returns the window title without the backend's possible additions

DECLARE FUNCTION gfx_AcquireKeyboard (byval bEnable as integer) as integer 'alerts backend of the engine's request for keyboard input; if bEnable == 0, the keyboard is freed; returns 0 on failure
DECLARE FUNCTION gfx_AcquireMouse (byval bEnable as integer) as integer 'alerts backend of the engine's request for mouse input; if bEnable == 0, the mouse is freed; returns 0 on failure
'alerts backend of the engine's request for an indexed joystick;
'the backend may allow a user to order the input devices as he/she sees fit;
'if bEnable == 0, the joystick is freed;
'returns 0 on failure;
DECLARE FUNCTION gfx_AcquireJoystick (byval bEnable as integer, byval nDevice as integer) as integer

DECLARE FUNCTION gfx_GetKeyboard (byval pKeyboard as integer ptr) as integer 'gets the keyboard state in a format the engine understands; returns 0 on failure

DECLARE FUNCTION gfx_GetMouseMovement (byref dx as integer, byref dy as integer, byref dWheel as integer, byref buttons as integer) as integer 'gets the mouse movement since the last input poll and the button state; returns 0 on failure
DECLARE FUNCTION gfx_GetMousePosition (byref x as integer, byref y as integer, byref wheel as integer, byref buttons as integer) as integer 'gets the mouse position and button state; returns 0 on failure
DECLARE FUNCTION gfx_SetMousePosition (byval x as integer, byval y as integer) as integer 'sets the mouse position; returns 0 on failure

DECLARE FUNCTION gfx_GetJoystickMovement (byval nDevice as integer, byref dx as integer, byref dy as integer, byref buttons as integer) as integer 'gets the indexed joystick movement since last input poll and button state; returns 0 on failure
DECLARE FUNCTION gfx_GetJoystickPosition (byval nDevice as integer, byref x as integer, byref y as integer, byref buttons as integer) as integer 'gets the indexed joystick position and button state; returns 0 on failure
DECLARE FUNCTION gfx_SetJoystickPosition (byval nDevice as integer, byval x as integer, byval y as integer) as integer 'sets the indexed joystick position; returns 0 on failure

#endif

end extern