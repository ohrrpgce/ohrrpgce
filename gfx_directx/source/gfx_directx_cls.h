//gfx_directx_cls.h
//by Jay Tennant 10/29/09
//does everything gfx_directx.h was to do, except in classes for easier management

#ifndef GFX_DIRECTX_CLS_H
#define GFX_DIRECTX_CLS_H

#include <windows.h>
#include <d3d9.h>
#include <d3dx9.h>
#include "smartptr.h"
#include <string>
#include <tchar.h>
#include "_tstring.h"

#include "gfx_directx_TESTAPPconst.h"
#include "gfx_directx_cls_window.h"
#include "gfx_directx_cls_hpcounter.h"
#include "gfx_directx_cls_palette.h"
//#include "gfx_directx_cls_font.h"
#include "gfx_directx_cls_midsurface.h"
//#include "gfx_directx_cls_quad.h"
//#include "gfx_directx_cls_d3d9caps.h"
//#include "gfx_directx_cls_fps.h"

namespace gfx
{
	enum DirectX_ErrorCode
	{
		DX_OK = 0,
		DX_LibrariesMissing,
		DX_NotInitialized,
		DX_Create_ParamsNotValid,
		DX_Create_D3D,
		DX_Create_D3DDevice,
		DX_Create_Fps,
		DX_Create_Texture,
		DX_Create_Quad,
		DX_Palette_ParamNotValid,
		DX_ScreenShot_ParamNotValid,
		DX_ScreenShot_Surface,
		DX_ScreenShot_GetFrontBuffer,
		DX_ScreenShot_Save,
		DX_Resolution_Invalid,
		DX_ShowPage_Begin,
		DX_ShowPage_Clear,
		DX_ShowPage_GetBackBuffer,
		DX_ShowPage_StretchRect,
		DX_ShowPage_End,
		DX_ShowPage_Present,
		DX_FORCE_DWORD = 0xffffffff, //not used, just forces 32bit compile
	};

	typedef IDirect3D9* (__stdcall *D3D_CREATE_CALL)(UINT d3d_sdk_version);
	typedef HRESULT (__stdcall *D3DX_SAVESURFACE_CALL)(LPCTSTR pDestFile, D3DXIMAGE_FILEFORMAT DestFormat, LPDIRECT3DSURFACE9 pSrcSurface, 
		CONST PALETTEENTRY * pSrcPalette, CONST RECT * pSrcRect);

	class DirectX
	{
	protected:
		HMODULE m_hD3d9;
		HMODULE m_hD3dx9;
		D3D_CREATE_CALL Direct3DCreate9_call;
		D3DX_SAVESURFACE_CALL D3DXSaveSurfaceToFile_call;
		bool m_bLibrariesLoaded;
		SmartPtr<IDirect3D9> m_d3d;
		SmartPtr<IDirect3DDevice9> m_d3ddev;
		D3DPRESENT_PARAMETERS m_d3dpp;
		//Dx9Caps m_caps;
		D3DXIMAGE_FILEFORMAT m_saveFormat;
		//Quad m_quad; //quad that covers the screen at drawing time
		MidSurface m_surface; //surface that receives ohr data
		//FPSDisplay m_fps; //displays frames per second
		Window* m_pWindow;
		RECT m_rWindowedMode; //position and dimensions in windowed mode
		RECT m_rFullscreenMode; //position and dimensions in fullscreen mode
		bool m_bInitialized;
		//bool m_bShowFps;
		bool m_bVSync;
		bool m_bSmoothDraw; //determines whether texture has smooth linear interpolation
		bool m_bPreserveAspectRatio; //determines whether the aspect ratio is preserved no matter the screen resolution
		HPCounter m_timer; //fps timer
		Palette<UINT> m_palette; //palette information
		DirectX_ErrorCode m_lastErrorCode; //last error code
		TCHAR m_lastErrorMessage[256]; //last error message
		tstring m_szModuleName;

		DirectX_ErrorCode Report(DirectX_ErrorCode error); //generates error message and stores error code
		RECT CalculateAspectRatio(UINT srcWidth, UINT srcHeight, UINT destWidth, UINT destHeight);
	public:
		DirectX();
		virtual ~DirectX();

		DirectX_ErrorCode Initialize(Window *pWin, const TCHAR* szModuleName/*, bool bEnableFpsDisplay = true*/); //starts up the engine
		DirectX_ErrorCode Shutdown(); //shuts down the engine
		DirectX_ErrorCode ShowPage(unsigned char *pRawPage, UINT width, UINT height); //draws the raw page (array of indices into graphics palette)
		DirectX_ErrorCode SetPalette(Palette<UINT>* pPalette); //sets the graphics palette by copying
		DirectX_ErrorCode ScreenShot(TCHAR* strName); //gets a screenshot, appending the correct format image to the end of the name
		DirectX_ErrorCode SetView(bool bWindowed); //sets view to either windowed or fullscreen
		DirectX_ErrorCode SetResolution(UINT width, UINT height);
		void SetVSync(bool bEnableVSync); //enables vsync, which is enabled by default
		//void SetFps(bool bEnableFpsDisplay); //enables fps display, which is enabled by default
		DirectX_ErrorCode SetSmooth(bool bSmoothDraw); //enables linear interpolation used on texture drawing
		void SetAspectRatioPreservation(bool bPreserve); //enables aspect ratio preservation through all screen resolutions
		void SetImageFileFormat(D3DXIMAGE_FILEFORMAT format); //sets the image file format of any screenshots

		RECT GetResolution(); //returns active resolution
		Palette<UINT> GetPalette(); //returns a reference of the palette, non-deletable
		bool IsVsyncEnabled(); //returns true if vsync is enabled
		//bool IsFpsEnabled(); //returns true if fps are displayed
		bool IsViewFullscreen(); //returns true if view is fullscreen
		bool IsSmooth(); //returns true if linear interpolation is used on the texture
		bool IsAspectRatioPreserved(); //returns true if aspect ratio is preserved
		bool IsScreenShotsActive(); //returns true if screen shot library was loaded
		D3DXIMAGE_FILEFORMAT GetImageFileFormat(); //returns image file format of screenshots
		DirectX_ErrorCode GetLastErrorCode(); //returns last error code
		TCHAR* GetLastErrorMessage(); //gets last error message
		//const TCHAR* GetCapsMessage(); //gets d3d9's caps in message layout (app-specific critical systems polled)

		void OnLostDevice();
		void OnResetDevice();
	};
}
#endif