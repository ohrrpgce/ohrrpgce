'OHRRPGCE COMMON - Runtime backend loading routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#ifdef LANG_DEPRECATED
 #define __langtok #lang
 __langtok "deprecated"
 OPTION STATIC
 OPTION EXPLICIT
#endif

#include "config.bi"
#include "ver.txt"
#include "common.bi"
#include "gfx.bi"
#include "music.bi"
#include "gfx.new.bi"
#include "gfx_newRenderPlan.bi"

extern "C"

dim gfx_Initialize as function (byval pCreationData as const GFX_INIT ptr) as integer
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
dim gfx_getresize as function (byref ret as XYPair) as integer
dim gfx_setresizable as sub (byval able as integer)
dim gfx_setoption as function (byval opt as zstring ptr, byval arg as zstring ptr) as integer
dim gfx_describe_options as function () as zstring ptr
dim gfx_printchar as sub (byval ch as integer, byval x as integer, byval y as integer)
dim io_init as sub ()
dim io_pollkeyevents as sub ()
dim io_waitprocessing as sub ()
dim io_keybits as sub (byval keybdarray as integer ptr)
dim io_updatekeys as sub (byval keybd as integer ptr)
dim io_enable_textinput as sub (byval enable as integer)
dim io_textinput as sub (byval buf as wstring ptr, byval bufsize as integer)
dim io_show_virtual_keyboard as sub ()
dim io_hide_virtual_keyboard as sub ()
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

declare function gfx_load(byval onlyfirst as integer = NO) as integer
declare sub unload_backend(which as GFxBackendStuff ptr)

dim shared currentgfxbackend as GfxBackendStuff ptr = NULL
dim shared queue_error as string  'queue up errors until it's possible to actually display them (TODO: not implemented)
dim wantpollingthread as integer
dim as string gfxbackend, musicbackend
dim as string gfxbackendinfo, musicbackendinfo
dim as string systeminfo

function gfx_dummy_getresize(byref ret as XYPair) as integer : return NO : end function
sub gfx_dummy_setresizable(byval able as integer) : end sub
sub io_dummy_waitprocessing() : end sub
sub io_dummy_pollkeyevents() : end sub
sub io_dummy_keybits(byval keybdarray as integer ptr) : end sub
sub io_dummy_updatekeys(byval keybd as integer ptr) : end sub
sub io_dummy_mousebits(byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer, byref mclicks as integer) : end sub
sub io_dummy_getmouse(byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer) : end sub
sub io_dummy_enable_textinput(byval enable as integer) : end sub
sub io_dummy_show_virtual_keyboard() : end sub
sub io_dummy_hide_virtual_keyboard() : end sub

function gfx_load_library(byval backendinfo as GfxBackendStuff ptr, filename as string) as integer
	dim hFile as any ptr = backendinfo->dylib
	dim needpolling as integer = NO
	if hFile <> 0 then return 1

	hFile = dylibload(filename)
	if hFile = 0 then return 0

	gfx_getversion = dylibsymbol(hFile, "gfx_getversion")
	dim as integer apiver = 0
	if gfx_getversion <> NULL then apiver = gfx_getversion()
	if (apiver or 1) = 0 then 'adjusted to bitwise OR-ing (FIXME)
		queue_error = "gfx_version: does not support v1--reports v" & apiver
		debug(queue_error)
		dylibfree(hFile)
		hFile = NULL
		return 0
	end if

	gfx_init = dylibsymbol(hFile, "gfx_init")
	gfx_close = dylibsymbol(hFile, "gfx_close")
	gfx_setdebugfunc = dylibsymbol(hFile, "gfx_setdebugfunc")
	gfx_showpage = dylibsymbol(hFile, "gfx_showpage")
	gfx_setpal = dylibsymbol(hFile, "gfx_setpal")
	gfx_screenshot = dylibsymbol(hFile, "gfx_screenshot")
	gfx_setwindowed = dylibsymbol(hFile, "gfx_setwindowed")
	gfx_windowtitle = dylibsymbol(hFile, "gfx_windowtitle")
	gfx_getwindowstate = dylibsymbol(hFile, "gfx_getwindowstate")
	gfx_setoption = dylibsymbol(hFile, "gfx_setoption")
	gfx_describe_options = dylibsymbol(hFile, "gfx_describe_options")
	gfx_printchar = dylibsymbol(hFile, "gfx_printchar") 'allowed to be NULL

#ifdef USE_RASTERIZER
	'New rendering API (FIXME: complete this)
	gfx_present = dylibsymbol(hFile, "gfx_present")
	if gfx_present = NULL then gfx_present = @gfx_present_SW
#endif

	io_init = dylibsymbol(hFile, "io_init")

	io_pollkeyevents = dylibsymbol(hFile, "io_pollkeyevents")
	if io_pollkeyevents = NULL then io_pollkeyevents = @io_dummy_pollkeyevents
	io_waitprocessing = dylibsymbol(hFile, "io_waitprocessing")
	if io_waitprocessing = NULL then io_waitprocessing = @io_dummy_waitprocessing

	io_keybits = dylibsymbol(hFile, "io_keybits")
	if io_keybits = NULL then
		io_keybits = @io_amx_keybits
		needpolling = YES
	end if
	io_updatekeys = dylibsymbol(hFile, "io_updatekeys")
	if io_updatekeys = NULL then io_updatekeys = @io_dummy_updatekeys

	io_enable_textinput = dylibsymbol(hFile, "io_enable_textinput")
	if io_enable_textinput = NULL then io_enable_textinput = @io_dummy_enable_textinput

	io_textinput = dylibsymbol(hFile, "io_textinput")
	'io_textinput is allowed to be NULL

	io_show_virtual_keyboard = dylibsymbol(hFile, "io_show_virtual_keyboard")
	if io_show_virtual_keyboard = NULL then io_show_virtual_keyboard = @io_dummy_show_virtual_keyboard
	io_hide_virtual_keyboard = dylibsymbol(hFile, "io_hide_virtual_keyboard")
	if io_hide_virtual_keyboard = NULL then io_hide_virtual_keyboard = @io_dummy_hide_virtual_keyboard

	io_mousebits = dylibsymbol(hFile, "io_mousebits")
	if io_mousebits = NULL then
		io_mousebits = @io_amx_mousebits
		needpolling = YES
	end if
	io_getmouse = dylibsymbol(hFile, "io_getmouse")
	if io_getmouse = NULL then io_getmouse = @io_dummy_getmouse

	io_setmousevisibility = dylibsymbol(hFile, "io_setmousevisibility")
	io_setmouse = dylibsymbol(hFile, "io_setmouse")
	io_mouserect = dylibsymbol(hFile, "io_mouserect")
	io_readjoysane = dylibsymbol(hFile, "io_readjoysane")
	
	backendinfo->dylib = hFile
	backendinfo->wantpolling = needpolling
	return 1
end function

'loads dynamic library graphics backends' procs into memory - new interface
'filename is the name of the file, ie. "gfx_directx.dll" 
'backendinfo is modified with relevant data
function gfx_load_library_new(byval backendinfo as GfxBackendStuff ptr, filename as string) as integer
	Dim hFile As any ptr
	hFile = dylibload(filename)
	If hFile = NULL Then Return 0

	gfx_GetVersion = dylibsymbol(hFile, "gfx_GetVersion")
	If gfx_GetVersion = NULL Then
		gfx_GetVersion = dylibsymbol(hFile, "gfx_getversion")
		If gfx_GetVersion = NULL Then
			dylibfree(hFile)
			Return 0
		End If
	End If

	Dim apiVersion As Integer
	apiVersion = gfx_GetVersion()
	If (apiVersion Or 2) = 0 Then
		queue_error = filename + " backend does not support v2--reports v" & apiVersion
		debug(queue_error)
		dylibfree(hFile)
		Return 0
	End If

	'backend checks out ok; start loading functions
	gfx_Initialize = dylibsymbol(hFile, "gfx_Initialize")
	gfx_Shutdown = dylibsymbol(hFile, "gfx_Shutdown")
	gfx_SendMessage = dylibsymbol(hFile, "gfx_SendMessage")
	gfx_PumpMessages = dylibsymbol(hFile, "gfx_PumpMessages")
	'gfx_Present = dylibsymbol(hFile, "gfx_Present")
	gfx_ScreenShot = dylibsymbol(hFile, "gfx_ScreenShot")
	gfx_SetWindowTitle = dylibsymbol(hFile, "gfx_SetWindowTitle")
	gfx_GetWindowTitle = dylibsymbol(hFile, "gfx_GetWindowTitle")
	'gfx_GetWindowState = dylibsymbol(hFile, "gfx_GetWindowState")
	gfx_AcquireKeyboard = dylibsymbol(hFile, "gfx_AcquireKeyboard")
	gfx_AcquireMouse = dylibsymbol(hFile, "gfx_AcquireMouse")
	gfx_AcquireJoystick = dylibsymbol(hFile, "gfx_AcquireJoystick")
	gfx_AcquireTextInput = dylibsymbol(hFile, "gfx_AcquireTextInput")
	gfx_GetKeyboard = dylibsymbol(hFile, "gfx_GetKeyboard")
	gfx_GetText = dylibsymbol(hFile, "gfx_GetText")
	gfx_GetMouseMovement = dylibsymbol(hFile, "gfx_GetMouseMovement")
	gfx_GetMousePosition = dylibsymbol(hFile, "gfx_GetMousePosition")
	gfx_SetMousePosition = dylibsymbol(hFile, "gfx_SetMousePosition")
	gfx_GetJoystickMovement = dylibsymbol(hFile, "gfx_GetJoystickMovement")
	gfx_GetJoystickPosition = dylibsymbol(hFile, "gfx_GetJoystickPosition")
	gfx_SetJoystickPosition = dylibsymbol(hFile, "gfx_SetJoystickPosition")

	'success
	backendinfo->dylib = hFile
	backendinfo->wantpolling = NO

	Return 1
End Function

Sub default_gfx_render_procs()
#IFDEF USE_RASTERIZER
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
#ENDIF
end sub

sub prefer_backend(b as GfxBackendStuff ptr)
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

function load_backend(which as GFxBackendStuff ptr) as integer
	if currentgfxbackend = which then return 1
	if currentgfxbackend <> NULL then
		unload_backend(currentgfxbackend)
		currentgfxbackend = NULL
	end if

	'Set up default function pointers
	default_gfx_render_procs()
	gfx_setdebugfunc = NULL
	Gfx_getresize = @gfx_dummy_getresize
	Gfx_setresizable = @gfx_dummy_setresizable
	Io_textinput = NULL
	Io_enable_textinput = @io_dummy_enable_textinput
	Gfx_printchar = NULL

	if which->load = NULL then
		dim filename as string = which->libname
		if filename = "" then filename = "gfx_" + which->name
#ifdef __FB_WIN32__
		filename += ".dll"
#else
		filename += ".so"   'try other paths?
#endif
		if gfx_load_library(which, filename) = 0 then return 0
	else
		if which->load() = 0 then return 0
	end if

	if gfx_setdebugfunc then
		gfx_setdebugfunc(@debugc)
	end if

	currentgfxbackend = which
	gfxbackendinfo = "gfx_" + which->name
	gfxbackend = which->name
	wantpollingthread = which->wantpolling
	return 1
end function

sub unload_backend(which as GFxBackendStuff ptr)
	if which->dylib then
		dylibfree(which->dylib)
		which->dylib = NULL
	end if
end sub

'onlyfirst: only try the most prefered. Returns 1 on success
function gfx_load(byval onlyfirst as integer) as integer
	if currentgfxbackend <> NULL then return 1 'hmm
	for i as integer = 0 to ubound(gfx_choices)
		if load_backend(gfx_choices(i)) then return 1
		if onlyfirst then return 0
	next
	display_help_string "Could not load any graphic backend! (Who forgot to compile without at least gfx_fb?)"
	return 0
end function

sub gfx_backend_init(byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr)
	for i as integer = 0 to ubound(gfx_choices)
		with *gfx_choices(i)
			if load_backend(gfx_choices(i)) then
				dim info_buffer as zstring * 512
				debuginfo "Initialising gfx_" + .name + "..."
				if gfx_init(terminate_signal_handler, windowicon, @info_buffer, 511) = 0 then 
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

end extern

'initialise the music backend name because it's static, yet music_init
'might not be called until Import Music menu
musicbackend = MUSIC_BACKEND
'musicbackendinfo = "music_" + MUSIC_BACKEND
musicbackendinfo = music_get_info()

#ifdef __FB_DARWIN__
type OSType as integer
extern "C"
 'From CoreServices (Gestalt.h)
 declare function Gestalt (byval selector as OSType, byval reponse as integer ptr) as integer
end extern

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
