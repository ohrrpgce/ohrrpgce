'OHRRPGCE COMMON - Runtime backend loading routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#include "config.bi"
#include "ver.txt"
#include "common.bi"
#include "gfx.bi"
#include "music.bi"
#include "gfx.new.bi"
#include "gfx_newRenderPlan.bi"

extern "C"

#ifdef __FB_DARWIN__
	type OSType as integer
	'From CoreServices (Gestalt.h)
	declare function Gestalt (byval selector as OSType, byval reponse as integer ptr) as integer
#endif

dim gfx_Initialize as function (byval pCreationData as const GfxInitData ptr) as integer
dim gfx_Shutdown as sub ()
dim gfx_SendMessage as function (byval msg as unsigned integer, byval dwParam as unsigned integer, byval pvParam as Any ptr) as integer
'dim gfx_GetVersion as function () as integer
dim gfx_PumpMessages as sub ()
'dim gfx_Present as sub (byval pSurface as ubyte ptr, byval nWidth as integer, byval nHeight as integer, byval pPalette as RGBcolor ptr)
'dim gfx_ScreenShot as function (byval szFileName as const zstring ptr) as integer
dim gfx_SetWindowTitle as sub (byval szTitleconst as const zstring ptr)
dim gfx_GetWindowTitle as function () as const zstring ptr
'dim gfx_GetWindowState as sub (byval nID as integer, byval pState as WindowState ptr)
dim gfx_AcquireKeyboard as function (byval bEnable as integer) as integer
dim gfx_AcquireMouse as function (byval bEnable as integer) as integer
dim gfx_AcquireJoystick as function (byval bEnable as integer, byval nDevice as integer) as integer
dim gfx_AcquireTextInput as function (byval bEnable as integer) as integer
dim gfx_GetKeyboard as function (byval pKeyboard as integer ptr) as integer
dim gfx_GetText as sub (byval pBuffer as wstring ptr, byval buffenLen as integer)
dim gfx_GetMouseMovement as function (byref dx as integer, byref dy as integer, byref dWheel as integer, byref buttons as integer) as integer
dim gfx_GetMousePosition as function (byref x as integer, byref y as integer, byref wheel as integer, byref buttons as integer) as integer
dim gfx_SetMousePosition as function (byval x as integer, byval y as integer) as integer
dim gfx_GetJoystickMovement as function (byval nDevice as integer, byref dx as integer, byref dy as integer, byref buttons as integer) as integer
dim gfx_GetJoystickPosition as function (byval nDevice as integer, byref x as integer, byref y as integer, byref buttons as integer) as integer
dim gfx_SetJoystickPosition as function (byval nDevice as integer, byval x as integer, byval y as integer) as integer

'Old graphics backend function pointers

dim gfx_init as function (byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr, byval info_buffer as zstring ptr, byval info_buffer_size as integer) as integer
dim gfx_close as sub ()
dim gfx_setdebugfunc as sub (byval debugc as sub cdecl (byval errorlevel as ErrorLevelEnum, byval message as zstring ptr))
dim gfx_getversion as function () as integer
dim gfx_showpage as sub (byval raw as ubyte ptr, byval w as integer, byval h as integer)
dim gfx_setpal as sub (byval pal as RGBcolor ptr)
dim gfx_screenshot as function (byval fname as zstring ptr) as integer
dim gfx_setwindowed as sub (byval iswindow as integer)
dim gfx_windowtitle as sub (byval title as zstring ptr)
dim gfx_getwindowstate as function () as WindowState ptr
dim gfx_get_screen_size as sub (wide as integer ptr, high as integer ptr)

dim gfx_supports_variable_resolution as function () as bool
dim gfx_get_resize as function (byref ret as XYPair) as bool
dim gfx_set_resizable as function (enable as bool, min_width as integer, min_height as integer) as bool
dim gfx_recenter_window_hint as sub ()
dim gfx_vsync_supported as function () as bool
dim gfx_setoption as function (byval opt as zstring ptr, byval arg as zstring ptr) as integer
dim gfx_describe_options as function () as zstring ptr
dim gfx_printchar as sub (byval ch as integer, byval x as integer, byval y as integer, byval col as integer)
dim gfx_get_safe_zone_margin as function () as single
dim gfx_set_safe_zone_margin as sub (byval margin as single)
dim gfx_supports_safe_zone_margin as function () as bool
dim gfx_ouya_purchase_request as sub(dev_id as string, identifier as string, key_der as string)
dim gfx_ouya_purchase_is_ready as function() as bool
dim gfx_ouya_purchase_succeeded as function() as bool
dim gfx_ouya_receipts_request as sub (dev_id as string, key_der as string)
dim gfx_ouya_receipts_are_ready as function () as bool
dim gfx_ouya_receipts_result as function () as string
dim io_init as sub ()
dim io_pollkeyevents as sub ()
dim io_waitprocessing as sub ()
dim io_keybits as sub (byval keybdarray as integer ptr)
dim io_updatekeys as sub (byval keybd as integer ptr)
dim io_enable_textinput as sub (byval enable as integer)
dim io_textinput as sub (byval buf as wstring ptr, byval bufsize as integer)
dim io_show_virtual_keyboard as sub ()
dim io_hide_virtual_keyboard as sub ()
dim io_show_virtual_gamepad as sub ()
dim io_hide_virtual_gamepad as sub ()
dim io_remap_android_gamepad as sub (byval player as integer, gp as GamePadMap)
dim io_remap_touchscreen_button as sub (byval button_id as integer, byval ohr_scancode as integer)
dim io_running_on_console as function () as bool
dim io_running_on_ouya as function () as bool
dim io_mousebits as sub (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer, byref mclicks as integer)
dim io_setmousevisibility as sub (byval visible as integer)
dim io_getmouse as sub (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer)
dim io_setmouse as sub (byval x as integer, byval y as integer)
dim io_mouserect as sub (byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
dim io_readjoysane as function (byval as integer, byref as integer, byref as integer, byref as integer) as integer


'New Surface-based graphics backend function pointers

dim gfx_surfaceCreate as function ( byval width as integer, byval height as integer, byval format as SurfaceFormat, byval usage as SurfaceUsage, byval ppSurfaceOut as Surface ptr ptr) as integer
dim gfx_surfaceDestroy as function ( byval pSurfaceIn as Surface ptr ) as integer
dim gfx_surfaceUpdate as function ( byval pSurfaceIn as Surface ptr ) as integer
dim gfx_surfaceGetData as function ( byval pSurfaceIn as Surface ptr ) as integer
dim gfx_surfaceFill as function ( byval fillColor as integer, byval pRect as SurfaceRect ptr, byval pSurfaceIn as Surface ptr ) as integer
dim gfx_surfaceStretch as function ( byval pRectSrc as SurfaceRect ptr, byval pSurfaceSrc as Surface ptr, byval pPalette as BackendPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
dim gfx_surfaceCopy as function ( byval pRectSrc as SurfaceRect ptr, byval pSurfaceSrc as Surface ptr, byval pPalette as BackendPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

dim gfx_paletteCreate as function ( byval ppPaletteOut as BackendPalette ptr ptr) as integer
dim gfx_paletteDestroy as function ( byval pPaletteIn as BackendPalette ptr ) as integer
dim gfx_paletteUpdate as function ( byval pPaletteIn as BackendPalette ptr ) as integer

dim gfx_renderQuadColor as function ( byval pQuad as VertexPC ptr, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
dim gfx_renderQuadTexture as function ( byval pQuad as VertexPT ptr, byval pTexture as Surface ptr, byval pPalette as BackendPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
dim gfx_renderQuadTextureColor as function ( byval pQuad as VertexPTC ptr, byval pTexture as Surface ptr, byval pPalette as BackendPalette ptr, byval bUseColorKey0 as integer, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

dim gfx_renderTriangleColor as function ( byval pTriangle as VertexPC ptr, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
dim gfx_renderTriangleTexture as function ( byval pTriangle as VertexPT ptr, byval pTexture as Surface ptr, byval pPalette as BackendPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
dim gfx_renderTriangleTextureColor as function ( byval pTriangle as VertexPTC ptr, byval pTexture as Surface ptr, byval pPalette as BackendPalette ptr, byval bUseColorKey0 as integer, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

dim gfx_present as function ( byval pSurfaceIn as Surface ptr, byval pPalette as BackendPalette ptr ) as integer



declare function gfx_alleg_setprocptrs() as integer
declare function gfx_fb_setprocptrs() as integer
declare function gfx_sdl_setprocptrs() as integer
declare function gfx_console_setprocptrs() as integer
'declare function gfx_sdlpp_setprocptrs() as integer

type GfxBackendStuff
	'FB doesn't allow initialising UDTs containing var-length strings
	name as string * 7
	libname as string * 15
	load as function () as integer
	wantpolling as integer  'run the polling thread?
	dylib as any ptr  'handle on a loaded library
end type

#ifdef GFX_ALLEG_BACKEND
dim shared as GfxBackendStuff alleg_stuff = ("alleg", "", @gfx_alleg_setprocptrs, YES)
#endif
#ifdef GFX_DIRECTX_BACKEND
dim shared as GfxBackendStuff directx_stuff = ("directx", "", NULL)  'work out wantpolling when loading
#endif
#ifdef GFX_FB_BACKEND
dim shared as GfxBackendStuff fb_stuff = ("fb", "", @gfx_fb_setprocptrs, YES)
#endif
#ifdef GFX_SDL_BACKEND
dim shared as GfxBackendStuff sdl_stuff = ("sdl", "", @gfx_sdl_setprocptrs, NO)
#endif
#ifdef GFX_CONSOLE_BACKEND
dim shared as GfxBackendStuff console_stuff = ("console", "", @gfx_console_setprocptrs, NO)
#endif
#ifdef GFX_SDLPP_BACKEND
dim shared as GfxBackendStuff sdlpp_stuff = ("sdl++", "gfx_sdl", NULL)
'dim shared as GfxBackendStuff sdlpp_stuff = ("sdl++", "", @gfx_sdlpp_setprocptrs)
#endif


'you can't initialise arrays with addresses because FB considers them nonconstant!!
'plus the extern block nonsense, oh what a mess
end extern
dim shared gfx_choices() as GfxBackendStuff ptr

'sets up pointers to *_stuff variables, in some build-dependent order
GFX_CHOICES_INIT
extern "C"

declare function gfx_load(byval onlyfirst as integer = NO) as bool
declare sub unload_backend(which as GFxBackendStuff ptr)
declare sub default_gfx_render_procs()

dim shared currentgfxbackend as GfxBackendStuff ptr = NULL
dim shared queue_error as string  'queue up errors until it's possible to actually display them (TODO: not implemented)
dim wantpollingthread as bool
dim as string gfxbackend, musicbackend
dim as string gfxbackendinfo, musicbackendinfo
dim as string systeminfo

dim allegro_initialised as bool = NO

sub gfx_dummy_get_screen_size(wide as integer ptr, high as integer ptr) : *wide = 0 : *high = 0 : end sub
function gfx_dummy_supports_variable_resolution() as bool : return NO : end function
function gfx_dummy_get_resize(byref ret as XYPair) as bool : return NO : end function
function gfx_dummy_set_resizable(enable as bool, min_width as integer, min_height as integer) as bool : return NO : end function
sub gfx_dummy_recenter_window_hint() : end sub
function gfx_dummy_vsync_supported_false() as bool : return NO : end function
function gfx_dummy_vsync_supported_true() as bool : return YES : end function
function gfx_dummy_get_safe_zone_margin() as single : return 0.0 : end function
sub gfx_dummy_set_safe_zone_margin(byval margin as single) : end sub
function gfx_dummy_supports_safe_zone_margin() as bool : return NO : end function
sub gfx_dummy_ouya_purchase_request(dev_id as string, identifier as string, key_der as string) : end sub
function gfx_dummy_ouya_purchase_is_ready() as bool : return YES : end function 'returns YES because we don't want to wait for the timeout
function gfx_dummy_ouya_purchase_succeeded() as bool : return NO : end function
sub gfx_dummy_ouya_receipts_request(dev_id as string, key_der as string) : end sub
function gfx_dummy_ouya_receipts_are_ready() as bool : return YES : end function 'returns YES because we don't want to wait for the timeout
function gfx_dummy_ouya_receipts_result() as string : return "" : end function

sub io_dummy_waitprocessing() : end sub
sub io_dummy_pollkeyevents() : end sub
sub io_dummy_updatekeys(byval keybd as integer ptr) : end sub
sub io_dummy_mousebits(byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer, byref mclicks as integer) : end sub
sub io_dummy_getmouse(byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer) : end sub
sub io_dummy_enable_textinput(byval enable as integer) : end sub
sub io_dummy_show_virtual_keyboard() : end sub
sub io_dummy_hide_virtual_keyboard() : end sub
sub io_dummy_show_virtual_gamepad() : end sub
sub io_dummy_hide_virtual_gamepad() : end sub
sub io_dummy_remap_android_gamepad(byval player as integer, gp as GamePadMap) : end sub
sub io_dummy_remap_touchscreen_button(byval button_id as integer, byval ohr_scancode as integer) : end sub
function io_dummy_running_on_console() as bool : return NO : end function
function io_dummy_running_on_ouya() as bool : return NO : end function

'Some parts of the API (function pointers) are optional in all gfx backends.
'Those are set to defaults, most of which do nothing.
'In addition other functions are only allowed to be missing when loading old dynamic
'libraries from before they existed; handled in gfx_load_library[_new]
private sub set_default_gfx_function_ptrs
	default_gfx_render_procs()
	gfx_getversion = NULL
	gfx_setdebugfunc = NULL
	gfx_get_screen_size = @gfx_dummy_get_screen_size
	gfx_supports_variable_resolution = @gfx_dummy_supports_variable_resolution
	gfx_get_resize = @gfx_dummy_get_resize
	gfx_set_resizable = @gfx_dummy_set_resizable
	gfx_recenter_window_hint = @gfx_dummy_recenter_window_hint
	gfx_vsync_supported = @gfx_dummy_vsync_supported_false
	gfx_printchar = NULL
	gfx_set_safe_zone_margin = @gfx_dummy_set_safe_zone_margin
	gfx_get_safe_zone_margin = @gfx_dummy_get_safe_zone_margin
	gfx_supports_safe_zone_margin = @gfx_dummy_supports_safe_zone_margin
	gfx_ouya_purchase_request = @gfx_dummy_ouya_purchase_request
	gfx_ouya_purchase_is_ready = @gfx_dummy_ouya_purchase_is_ready
	gfx_ouya_purchase_succeeded = @gfx_dummy_ouya_purchase_succeeded
	gfx_ouya_receipts_request = @gfx_dummy_ouya_receipts_request
	gfx_ouya_receipts_are_ready = @gfx_dummy_ouya_receipts_are_ready
	gfx_ouya_receipts_result = @gfx_dummy_ouya_receipts_result
	io_pollkeyevents = @io_dummy_pollkeyevents
	io_waitprocessing = @io_dummy_waitprocessing
	io_keybits = @io_amx_keybits   'Special handling when missing, see gfx_load_library
	io_updatekeys = @io_dummy_updatekeys
	io_enable_textinput = @io_dummy_enable_textinput
	io_textinput = NULL
	io_show_virtual_keyboard = @io_dummy_show_virtual_keyboard
	io_hide_virtual_keyboard = @io_dummy_hide_virtual_keyboard
	io_show_virtual_gamepad = @io_dummy_show_virtual_gamepad
	io_hide_virtual_gamepad = @io_dummy_hide_virtual_gamepad
	io_remap_android_gamepad = @io_dummy_remap_android_gamepad
	io_remap_touchscreen_button = @io_dummy_remap_touchscreen_button
	io_running_on_console = @io_dummy_running_on_console
	io_running_on_ouya = @io_dummy_running_on_ouya
	io_mousebits = @io_amx_mousebits   'Special handling when missing, see gfx_load_library
	io_getmouse = @io_dummy_getmouse
end sub

private function hTRYLOAD(byval hFile as any ptr, byval procedure as any ptr ptr, funcname as string) as bool
	dim tempptr as any ptr = dylibsymbol(hfile, funcname)
	if tempptr <> NULL then *procedure = tempptr
	'Otherwise leave default value of procedure intact
	return tempptr <> NULL
end function
#define TRYLOAD(procedure) hTRYLOAD(hFile, @procedure, #procedure)

#macro MUSTLOAD(procedure)
	procedure = dylibsymbol(hfile, #procedure)
	if procedure = NULL then
		debug filename & " - Could not load required procedure " & #procedure
		dylibfree(hFile)
		return 0
	end if
#endmacro

'Load a dynamically linked gfx backend. Returns true on success
private function gfx_load_library(byval backendinfo as GfxBackendStuff ptr, filename as string) as bool
	dim hFile as any ptr = backendinfo->dylib
	dim needpolling as integer = NO
	if hFile <> NULL then return YES

	IF backendinfo->name = "directx" THEN
		'override default. TODO: move into gfx_directx
		gfx_vsync_supported = @gfx_dummy_vsync_supported_true
	END IF

	hFile = dylibload(filename)
	if hFile = NULL then return NO

	TRYLOAD(gfx_getversion)
	dim as integer apiver = 0
	if gfx_getversion <> NULL then apiver = gfx_getversion()
	if (apiver and 1) = 0 then
		queue_error = "gfx_version: does not support v1--reports bitfield " & apiver
		debug(queue_error)
		dylibfree(hFile)
		hFile = NULL
		return NO
	end if

	MUSTLOAD(gfx_init)
	MUSTLOAD(gfx_close)
	TRYLOAD (gfx_setdebugfunc)
	'gfx_getversion already loaded
	MUSTLOAD(gfx_showpage)
	MUSTLOAD(gfx_setpal)
	MUSTLOAD(gfx_screenshot)
	MUSTLOAD(gfx_setwindowed)
	MUSTLOAD(gfx_windowtitle)
	MUSTLOAD(gfx_getwindowstate)
	TRYLOAD (gfx_get_screen_size)
	TRYLOAD (gfx_supports_variable_resolution)
	TRYLOAD (gfx_get_resize)
	TRYLOAD (gfx_set_resizable)
	TRYLOAD (gfx_recenter_window_hint)
	MUSTLOAD(gfx_setoption)
	MUSTLOAD(gfx_describe_options)
	TRYLOAD (gfx_printchar)
	TRYLOAD (gfx_get_safe_zone_margin)
	TRYLOAD (gfx_set_safe_zone_margin)
	TRYLOAD (gfx_supports_safe_zone_margin)
	TRYLOAD (gfx_ouya_purchase_request)
	TRYLOAD (gfx_ouya_purchase_is_ready)
	TRYLOAD (gfx_ouya_purchase_succeeded)
	TRYLOAD (gfx_ouya_receipts_request)
	TRYLOAD (gfx_ouya_receipts_are_ready)
	TRYLOAD (gfx_ouya_receipts_result)

	'New rendering API (FIXME: complete this)
	TRYLOAD (gfx_present)
	'End of new API

	MUSTLOAD(io_init)
	TRYLOAD (io_pollkeyevents)
	TRYLOAD (io_waitprocessing)
	if TRYLOAD(io_keybits) = NO then
		needpolling = YES
	end if
	TRYLOAD (io_updatekeys)
	TRYLOAD (io_enable_textinput)
	TRYLOAD (io_textinput)
	TRYLOAD (io_show_virtual_keyboard)
	TRYLOAD (io_hide_virtual_keyboard)
	TRYLOAD (io_show_virtual_gamepad)
	TRYLOAD (io_hide_virtual_gamepad)
	TRYLOAD (io_remap_android_gamepad)
	TRYLOAD (io_remap_touchscreen_button)
	TRYLOAD (io_running_on_console)
	TRYLOAD (io_running_on_ouya)
	if TRYLOAD(io_mousebits) = NO then
		needpolling = YES
	end if
	MUSTLOAD(io_setmousevisibility)
	TRYLOAD (io_getmouse)
	MUSTLOAD(io_setmouse)
	MUSTLOAD(io_mouserect)
	MUSTLOAD(io_readjoysane)

	backendinfo->dylib = hFile
	backendinfo->wantpolling = needpolling
	return YES
end function

'Loads dynamic library graphics backends' procs into memory - new interface.
'Returns true on success
'filename is the name of the file, ie. "gfx_directx.dll" 
'backendinfo is modified with relevant data
private function gfx_load_library_new(byval backendinfo as GfxBackendStuff ptr, filename as string) as bool
	Dim hFile As any ptr
	hFile = dylibload(filename)
	If hFile = NULL Then Return NO

	If TRYLOAD(gfx_GetVersion) = NO Then
		'gfx_GetVersion and gfx_getversion are the same variable, but different functions in hFile
		MUSTLOAD(gfx_getversion)
	End If

	Dim apiVersion As Integer
	apiVersion = gfx_GetVersion()
	If (apiVersion and 2) = 0 Then
		queue_error = filename + " backend does not support v2--reports bitfield " & apiVersion
		debug(queue_error)
		dylibfree(hFile)
		Return NO
	End If

	'backend checks out ok; start loading functions
	MUSTLOAD(gfx_Initialize)
	MUSTLOAD(gfx_Shutdown)
	MUSTLOAD(gfx_SendMessage)
	MUSTLOAD(gfx_PumpMessages)
	'MUSTLOAD(gfx_Present)
	MUSTLOAD(gfx_ScreenShot)
	MUSTLOAD(gfx_SetWindowTitle)
	MUSTLOAD(gfx_GetWindowTitle)
	'MUSTLOAD(gfx_GetWindowState)
	MUSTLOAD(gfx_AcquireKeyboard)
	MUSTLOAD(gfx_AcquireMouse)
	MUSTLOAD(gfx_AcquireJoystick)
	MUSTLOAD(gfx_AcquireTextInput)
	MUSTLOAD(gfx_GetKeyboard)
	MUSTLOAD(gfx_GetText)
	MUSTLOAD(gfx_GetMouseMovement)
	MUSTLOAD(gfx_GetMousePosition)
	MUSTLOAD(gfx_SetMousePosition)
	MUSTLOAD(gfx_GetJoystickMovement)
	MUSTLOAD(gfx_GetJoystickPosition)
	MUSTLOAD(gfx_SetJoystickPosition)

	'success
	backendinfo->dylib = hFile
	backendinfo->wantpolling = NO

	Return YES
End Function

private sub default_gfx_render_procs()
	gfx_surfaceCreate = @gfx_surfaceCreate_SW
	gfx_surfaceDestroy = @gfx_surfaceDestroy_SW
	gfx_surfaceUpdate = @gfx_surfaceUpdate_SW
	gfx_surfaceGetData = @gfx_surfaceGetData_SW
	gfx_surfaceFill = @gfx_surfaceFill_SW
	gfx_surfaceStretch = @gfx_surfaceStretch_SW
	gfx_surfaceCopy = @gfx_surfaceCopy_SW
	gfx_paletteCreate = @gfx_paletteCreate_SW
	gfx_paletteDestroy = @gfx_paletteDestroy_SW
	gfx_paletteUpdate = @gfx_paletteUpdate_SW
	gfx_renderQuadColor = @gfx_renderQuadColor_SW
	gfx_renderQuadTexture = @gfx_renderQuadTexture_SW
	gfx_renderQuadTextureColor = @gfx_renderQuadTextureColor_SW
	gfx_renderTriangleColor = @gfx_renderTriangleColor_SW
	gfx_renderTriangleTexture = @gfx_renderTriangleTexture_SW
	gfx_renderTriangleTextureColor = @gfx_renderTriangleTextureColor_SW
	gfx_present = @gfx_present_SW
end sub

private sub prefer_backend(b as GfxBackendStuff ptr)
	for i as integer = ubound(gfx_choices) - 1 to 0 step -1
		if gfx_choices(i + 1) = b then swap gfx_choices(i), gfx_choices(i + 1)
	next
end sub

function backends_setoption(opt as string, arg as string) as integer
	'general backend options
	'gfx should be the first option
	if opt = "gfx" then 
		dim unsupported as integer = NO
		dim failed as integer = NO
		if arg = "alleg" or arg = "allegro" then
			#ifdef GFX_ALLEG_BACKEND
				prefer_backend(@alleg_stuff)
				if gfx_load(YES) then return 2
				failed = YES
			#endif
			unsupported = YES
		elseif arg = "directx" then
			#ifdef GFX_DIRECTX_BACKEND
				prefer_backend(@directx_stuff)
				if gfx_load(YES) then return 2
				failed = YES
			#endif
			unsupported = YES
		elseif arg = "fb" then
			#ifdef GFX_FB_BACKEND
				prefer_backend(@fb_stuff)
				if gfx_load(YES) then return 2
				failed = YES
			#endif
			unsupported = YES
		elseif arg = "sdl" then
			#ifdef GFX_SDL_BACKEND
				prefer_backend(@sdl_stuff)
				if gfx_load(YES) then return 2
				failed = YES
			#endif
			unsupported = YES
		elseif arg = "console" then
			#ifdef GFX_CONSOLE_BACKEND
				prefer_backend(@console_stuff)
				if gfx_load(YES) then return 2
				failed = YES
			#endif
			unsupported = YES
		elseif arg = "sdlpp" or arg = "sdl++" then
			#ifdef GFX_SDLPP_BACKEND
				prefer_backend(@sdlpp_stuff)
				if gfx_load(YES) then return 2
				failed = YES
			#endif
			unsupported = YES
		else
			display_help_string """" + arg + """ is not a valid graphics backend"
		end if
		if failed then
			display_help_string "gfx_" + arg + " could not be loaded!"
		elseif unsupported then
			display_help_string "gfx_" + arg + " support is not enabled in this build"
		end if
		return 2
	else
		'after any -gfx is processed, should load the backend to send it the remain options
		gfx_load
		if opt = "w" or opt = "windowed" then
			gfx_setwindowed(1)
			return 1
		elseif opt = "f" or opt = "fullscreen" then
			gfx_setwindowed(0)
			return 1
		end if
	end if
	return 0
end function

'Returns true on success
private function load_backend(which as GFxBackendStuff ptr) as bool
	if currentgfxbackend = which then return YES
	if currentgfxbackend <> NULL then
		unload_backend(currentgfxbackend)
		currentgfxbackend = NULL
	end if

	set_default_gfx_function_ptrs()

	if which->load = NULL then
		'Dynamically linked
		dim filename as string = which->libname
		if filename = "" then filename = "gfx_" + which->name
#ifdef __FB_WIN32__
		filename += ".dll"
#else
		filename += ".so"   'try other paths?
#endif
		if gfx_load_library(which, filename) = NO then return NO
	else
		'Statically linked
		if which->load() = 0 then return NO
	end if

	if gfx_setdebugfunc then
		gfx_setdebugfunc(@debugc)
	end if

	'FIXME: in the Android port, gfxbackend takes the value "sd"!!

	currentgfxbackend = which
	gfxbackendinfo = "gfx_" + which->name
	gfxbackend = which->name
	wantpollingthread = which->wantpolling
	return YES
end function

private sub unload_backend(which as GFxBackendStuff ptr)
	if which->dylib then
		dylibfree(which->dylib)
		which->dylib = NULL
	end if
end sub

'onlyfirst: only try the most prefered. Returns true on success
private function gfx_load(byval onlyfirst as bool) as bool
	if currentgfxbackend <> NULL then return YES 'hmm
	for i as integer = 0 to ubound(gfx_choices)
		if load_backend(gfx_choices(i)) then return YES
		if onlyfirst then return NO
	next
	display_help_string "Could not load any graphic backend! (Who forgot to compile without at least gfx_fb?)"
	return NO
end function

end extern

' Try to init gfx backends in order of preference until one works.
sub init_gfx_backend()
	for i as integer = 0 to ubound(gfx_choices)
		with *gfx_choices(i)
			if load_backend(gfx_choices(i)) then
				dim info_buffer as zstring * 512
				debuginfo "Initialising gfx_" + .name + "..."
				if gfx_init(@post_terminate_signal, "FB_PROGRAM_ICON", @info_buffer, 511) = 0 then
					unload_backend(gfx_choices(i))
					currentgfxbackend = NULL
					'TODO: what about the polling thread?
					queue_error = info_buffer
					debug queue_error
				else
					if len(info_buffer) then
						gfxbackendinfo += " """ + info_buffer + """"
						debuginfo gfxbackendinfo
					end if
					exit sub
				end if
			end if
		end with
	next

	display_help_string "No working graphics backend!"
	system 1
end sub

' Load gfxbackendinfo, musicbackendinfo, systeminfo
sub read_backend_info()
	'gfx backend not selected yet.

	'initialise the music backend name because it's static, yet music_init
	'might not be called until Import Music menu
	musicbackend = MUSIC_BACKEND
	'musicbackendinfo = "music_" + MUSIC_BACKEND
	musicbackendinfo = music_get_info()

	#ifdef __FB_DARWIN__
		dim as integer response
		'Note that we have to give the OSTypes backwards because we're little-endian
		Gestalt(*cast(integer ptr, @"1sys"), @response)  'gestaltSystemVersionMajor
		systeminfo = "Mac OS " & response & "."
		Gestalt(*cast(integer ptr, @"2sys"), @response)  'gestaltSystemVersionMinor
		systeminfo &= response & "."
		Gestalt(*cast(integer ptr, @"3sys"), @response)  'gestaltSystemVersionBugFix
		systeminfo &= response
	#endif

	#ifdef __FB_WIN32__
		systeminfo = get_windows_version()
	#endif
end sub