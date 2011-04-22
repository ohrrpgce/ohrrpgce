//gfx_directx_cls_d3d.h
//by Jay Tennant 10/29/09
//does everything gfx_directx.h was to do, except in classes for easier management

#ifndef GFX_DIRECTX_CLS_D3D_H
#define GFX_DIRECTX_CLS_D3D_H

#include <windows.h>
#include <d3d9.h>
#include <d3dx9.h>
#include "smartptr.h"
#include <string>
#include <tchar.h>
#include "Tstring.h"

#include "window.h"
#include "palette.h"
#include "midsurface.h"
#include "DllFunctionInterface.h"


namespace gfx
{
	//d3d9.dll Direct3DCreate9() library manager class
	_DFI_IMPORT_CLASS_SHELL_BEGIN(DXCreate);
	_DFI_IMPORT_CLASS_DECLARE(IDirect3D9*, __stdcall, Direct3DCreate9, UINT sdkVersion);
	_DFI_IMPORT_CLASS_SHELL_END(DXCreate, TEXT("d3d9.dll"));

	//d3dx9_24.dll D3DXSaveSurfaceToFile() library manager class
	_DFI_IMPORT_CLASS_SHELL_BEGIN(DXScreenShot);
	_DFI_IMPORT_CLASS_DECLARE(HRESULT, __stdcall, D3DXSaveSurfaceToFileA, LPCSTR pDestFile, 
																		  D3DXIMAGE_FILEFORMAT DestFormat, 
																		  LPDIRECT3DSURFACE9 pSrcSurface, 
																		  CONST PALETTEENTRY* pSrcPalette, 
																		  CONST RECT* pSrcRect);
	_DFI_IMPORT_CLASS_DECLARE(HRESULT, __stdcall, D3DXSaveSurfaceToFileW, LPCWSTR pDestFile, 
																		  D3DXIMAGE_FILEFORMAT DestFormat, 
																		  LPDIRECT3DSURFACE9 pSrcSurface, 
																		  CONST PALETTEENTRY* pSrcPalette, 
																		  CONST RECT* pSrcRect);
	_DFI_IMPORT_CLASS_SHELL_END(DXScreenShot, TEXT("d3dx9_24.dll"));

	//main class; the previous libraries are included in this class
	class D3D : protected DXCreate, protected DXScreenShot
	{
	protected:
		SmartPtr<IDirect3D9> m_d3d;
		SmartPtr<IDirect3DDevice9> m_d3ddev;
		D3DPRESENT_PARAMETERS m_d3dpp;
		D3DXIMAGE_FILEFORMAT m_saveFormat;
		MidSurface m_surface; //surface that receives ohr data
		Window* m_pWindow;
		RECT m_rWindowedMode; //position and dimensions in windowed mode
		RECT m_rFullscreenMode; //position and dimensions in fullscreen mode
		BOOL m_bInitialized;
		BOOL m_bVSync;
		BOOL m_bSmoothDraw; //determines whether texture has smooth linear interpolation
		BOOL m_bPreserveAspectRatio; //determines whether the aspect ratio is preserved no matter the screen resolution
		struct Image
		{
			Image() : pSurface(NULL), width(0), height(0){}
			~Image() {Free(); palette.Free();}
			void AllocateSurface(UINT nWidth, UINT nHeight)
			{
				Free();
				if(nWidth == 0 || nHeight == 0)
					return;
				width = nWidth;
				height = nHeight;
				pSurface = new BYTE[width * height];
			}
			void Free()
			{
				if(pSurface != NULL) 
					delete [] pSurface; 
				pSurface = NULL; 
			}
			BYTE *pSurface;
			UINT width;
			UINT height;
			Palette<UINT> palette;
		} m_image;
		Tstring m_szModuleName;

		RECT CalculateAspectRatio(UINT srcWidth, UINT srcHeight, UINT destWidth, UINT destHeight);
	public:
		D3D();
		virtual ~D3D();

		HRESULT Initialize(Window *pWin, LPCTSTR szModuleName); //starts up the engine
		HRESULT Shutdown(); //shuts down the engine
		//HRESULT ShowPage(unsigned char *pRawPage, UINT width, UINT height); //draws the raw page (array of indices into graphics palette)
		//HRESULT SetPalette(Palette<UINT>* pPalette); //sets the graphics palette by copying
		HRESULT Present(unsigned char *pRawPage, UINT width, UINT height, Palette<UINT> *pPalette); //draws the raw page (array of indices into palette), and sets the palette; if pPalette is NULL, the page is presented with the previous palette; if pRawpage is NULL, the page is presented with the new palette; if both are NULL, the image is presented again
		HRESULT ScreenShot(LPCTSTR strName); //gets a screenshot, appending the correct format image to the end of the name
		void OnLostDevice();
		void OnResetDevice();

		//option setting
		HRESULT SetViewFullscreen(BOOL bFullscreen); //sets view to fullscreen if true
		HRESULT SetResolution(LPCRECT pRect); //sets the dimensions of the backbuffer
		HRESULT SetVsyncEnabled(BOOL bVsync); //enables vsync if true
		void SetSmooth(BOOL bSmoothDraw); //enables linear interpolation used on texture drawing
		void SetAspectRatioPreservation(BOOL bPreserve); //enables aspect ratio preservation through all screen resolutions
		void SetImageFileFormat(D3DXIMAGE_FILEFORMAT format); //sets the image file format of any screenshots

		//info
		RECT GetResolution(); //returns active resolution
		Palette<UINT> GetPalette(); //returns a reference of the palette, non-deletable
		BOOL IsVsyncEnabled(); //returns true if vsync is enabled
		BOOL IsViewFullscreen(); //returns true if view is fullscreen
		BOOL IsSmooth(); //returns true if linear interpolation is used on the texture
		BOOL IsAspectRatioPreserved(); //returns true if aspect ratio is preserved
		BOOL IsScreenShotsActive(); //returns true if screen shot library was loaded
		D3DXIMAGE_FILEFORMAT GetImageFileFormat(); //returns image file format of screenshots
	};
}
#endif