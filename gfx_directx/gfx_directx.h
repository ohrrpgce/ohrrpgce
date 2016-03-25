//gfx_directx.h
//started 10/22/09
//provides directx 9.0c shader enabled graphics backend for the ohrrpgce

#pragma once

#include "DllFunctionInterface.h"

struct WindowState
{
	int structsize;
	int focused;
	int minimised;
	int fullscreen;
	int user_toggled_fullscreen;
};
#define WINDOWSTATE_SZ 5

struct GFX_INIT
{
	int nSize;
	const char* szInitWindowTitle;
	const char* szWindowIcon;
	void (__cdecl *PostTerminateSignal)(void);
	void (__cdecl *OnCriticalError)(const char* szError);
	void (__cdecl *SendDebugString)(const char* szMessage);
};
#include "stdint.h"

//surfaces
enum SurfaceFormat
{
	SF_8bit = 0,
	SF_32bit = 1,
};
enum SurfaceUsage
{
	SU_Source = 0,
	SU_RenderTarget = 1,
	//SU_Backbuffer = 2,
};
struct Surface
{
	void* handle;
	uint32_t width;
	uint32_t height;
	SurfaceFormat format;
	SurfaceUsage usage;
	union
	{
		void* pRawData;
		uint32_t* pColorData;
		uint8_t* pPaletteData;
	};
};
struct SurfaceRect
{
	int32_t left, top, right, bottom;
};

//palettes
struct Palette
{
	void* handle;
	uint32_t p[256];
};


DFI_CLASS_BEGIN( DllBackend );


#ifndef DFI_IMPORT
#if 0 //set to 1 if using stdcall calling methods, instead of cdecl
#pragma comment(linker, "/export:gfx_init=_gfx_init@16")
#pragma comment(linker, "/export:gfx_close=_gfx_close@0")
#pragma comment(linker, "/export:gfx_getversion=_gfx_getversion@0")
#pragma comment(linker, "/export:gfx_showpage=_gfx_showpage@12")
#pragma comment(linker, "/export:gfx_setpal=_gfx_setpal@4")
#pragma comment(linker, "/export:gfx_screenshot=_gfx_screenshot@4")
#pragma comment(linker, "/export:gfx_setwindowed=_gfx_setwindowed@4")
#pragma comment(linker, "/export:gfx_windowtitle=_gfx_windowtitle@4")
#pragma comment(linker, "/export:gfx_getwindowstate=_gfx_getwindowstate@0")
#pragma comment(linker, "/export:gfx_setoption=_gfx_setoption@8")
#pragma comment(linker, "/export:gfx_describe_options=_gfx_describe_options@0")
#pragma comment(linker, "/export:io_init=_io_init@0")
#pragma comment(linker, "/export:io_pollkeyevents=_io_pollkeyevents@0")
#pragma comment(linker, "/export:io_keybits=_io_keybits@4")
#pragma comment(linker, "/export:io_setmousevisibility=_io_setmousevisibility@4")
#pragma comment(linker, "/export:io_getmouse=_io_getmouse@16")
#pragma comment(linker, "/export:io_setmouse=_io_setmouse@8")
#pragma comment(linker, "/export:io_mouserect=_io_mouserect@16")
#pragma comment(linker, "/export:io_readjoysane=_io_readjoysane@16")
#endif
#endif //DFI_IMPORT

//terminate_signal_handler is a pointer to post_terminate_signal, for dynamically linked graphics backends.
//windowicon is platform specific: name of the icon resource on Windows, no meaning yet elsewhere
DFI_DECLARE_CDECL( int, gfx_init, void (__cdecl *terminate_signal_handler)(void) , const char* windowicon, char* info_buffer, int info_buffer_size );
DFI_DECLARE_CDECL( void, gfx_close );

DFI_DECLARE_CDECL( int, gfx_getversion );

DFI_DECLARE_CDECL( void, gfx_showpage, unsigned char *raw, int w, int h ); //the main event
DFI_DECLARE_CDECL( void, gfx_showpage32, unsigned int *raw, int w, int h ); //32bit main event
DFI_DECLARE_CDECL( int, gfx_present, Surface* pSurfaceIn, Palette* palette ); //new interface for presentation
DFI_DECLARE_CDECL( void, gfx_setpal, unsigned int *pal ); //set colour palette, DWORD where colors ordered b,g,r,a
DFI_DECLARE_CDECL( int, gfx_screenshot, const char* fname );
DFI_DECLARE_CDECL( void, gfx_setwindowed, int iswindow );
DFI_DECLARE_CDECL( void, gfx_windowtitle, const char* title );
DFI_DECLARE_CDECL( WindowState*, gfx_getwindowstate );

//gfx_setoption recieves an option name and the following option which may or may not be a related argument
//returns 0 if unrecognised, 1 if recognised but arg is ignored, 2 if arg is gobbled
DFI_DECLARE_CDECL( int, gfx_setoption, const char* opt, const char* arg );
DFI_DECLARE_CDECL( const char*, gfx_describe_options );

DFI_DECLARE_CDECL( void, io_init );
DFI_DECLARE_CDECL( void, io_pollkeyevents );

//DFI_DECLARE_CDECL( void, io_waitprocessing );

//DFI_DECLARE_CDECL( void, io_keybits, int* keybdarray );
DFI_DECLARE_CDECL( void, io_keybits, int *keybd );
DFI_DECLARE_CDECL( void, io_textinput, wchar_t* buffer, int bufferLen );
//DFI_DECLARE_CDECL( void, io_mousebits, int& mx, int& my, int& mwheel, int& mbuttons, int& mclicks );
DFI_DECLARE_CDECL( int, io_setmousevisibility, int visible );
DFI_DECLARE_CDECL( void, io_getmouse, int& mx, int& my, int& mwheel, int& mbuttons );
DFI_DECLARE_CDECL( void, io_setmouse, int x, int y );
DFI_DECLARE_CDECL( void, io_mouserect, int xmin, int xmax, int ymin, int ymax );
DFI_DECLARE_CDECL( int, io_readjoysane, int, int&, int&, int& );


/////////////////////////////////////////////////////////////////////////////////////////////
//Interfaces 2.0?


/////////////////////////////////////////////////////////////////////////////////////////////
//basic backend functions
DFI_DECLARE_CDECL( int, gfx_Initialize, const GFX_INIT* pCreationData ); //initializes the backend; if failed, returns 0
DFI_DECLARE_CDECL( void, gfx_Shutdown ); //shuts down the backend--does not post the termination signal

DFI_DECLARE_CDECL( int, gfx_SendMessage, unsigned int msg, unsigned int dwParam, void* pvParam ); //sends a message to the backend; return value depends on message sent

DFI_DECLARE_CDECL( int, gfx_GetVersion ); //returns the backend version

/////////////////////////////////////////////////////////////////////////////////////////////
//graphical functions
//presents a surface from ohr to the backend's backbuffer, converting it with the palette supplied;
//if pSurface == NULL, a maintained copy of the surface will be used
//if pPalette == NULL, a maintained copy of the palette will be used
DFI_DECLARE_CDECL( void, gfx_PresentOld, unsigned char *pSurface, int nWidth, int nHeight, unsigned int *pPalette );

DFI_DECLARE_CDECL( int, gfx_ScreenShot, const char* szFileName ); //takes a screenshot; if failed, returns 0

/////////////////////////////////////////////////////////////////////////////////////////////
//messaging functions
DFI_DECLARE_CDECL( void, gfx_PumpMessages ); //pumps the backend's message queues and polls input

DFI_DECLARE_CDECL( void, gfx_SetWindowTitle, const char* szTitle ); //sets the window title; the backend may add messages to the window title to describe further option
DFI_DECLARE_CDECL( const char*, gfx_GetWindowTitle ); //returns the window title without the backend's possible additions
DFI_DECLARE_CDECL( void, gfx_GetWindowState, int nID, WindowState *pState ); //returns window information

DFI_DECLARE_CDECL( void, gfx_ShowCursor ); //sets the os cursor to display over the client area
DFI_DECLARE_CDECL( void, gfx_HideCursor ); //sets the os cursor to hide while over the client area
DFI_DECLARE_CDECL( void, gfx_ClipCursor, int left, int top, int right, int bottom ); //clips the os cursor to the ohr rectangle, which is scaled to the client area; passing a negative for any value disables the clip

/////////////////////////////////////////////////////////////////////////////////////////////
//input functions
DFI_DECLARE_CDECL( int, gfx_GetKeyboard, int *pKeyboard ); //gets the keyboard state in a format the engine understands; returns 0 on failure
DFI_DECLARE_CDECL( void, gfx_GetText, wchar_t *pBuffer, int bufferLen ); //gets the textual input since the last call, stores it in a buffer which can hold len-1 characters

DFI_DECLARE_CDECL( int, gfx_GetMouse, int& x, int& y, int& wheel, int& buttons ); //gets the mouse position and button state; returns 0 on failure
DFI_DECLARE_CDECL( int, gfx_SetMouse, int x, int y); //sets the mouse position; returns 0 on failure

DFI_DECLARE_CDECL( int, gfx_GetJoystick, int nDevice, int& x, int& y, int& buttons ); //gets the indexed joystick position and button state; returns 0 on failure
DFI_DECLARE_CDECL( int, gfx_SetJoystick, int nDevice, int x, int y ); //sets the indexed joystick position; returns 0 on failure
DFI_DECLARE_CDECL( int, gfx_GetJoystickCount ); //returns the number of joysticks attached to the system


DFI_CLASS_END( DllBackend, TEXT("gfx_directx.dll") );
