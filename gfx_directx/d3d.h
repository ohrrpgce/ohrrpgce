//d3d.h
//by Jay Tennant 10/29/09; updated 4/21/11
//manages the directx object and presentation

#pragma once

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
	_DFI_IMPORT_CLASS_SHELL_END(DXCreate);

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
	_DFI_IMPORT_CLASS_SHELL_END(DXScreenShot);

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
			~Image() {free(); palette.free();}
			void allocateSurface(UINT nWidth, UINT nHeight)
			{
				free();
				if(nWidth == 0 || nHeight == 0)
					return;
				width = nWidth;
				height = nHeight;
				pSurface = new BYTE[width * height];
			}
			void free()
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

		RECT calculateAspectRatio(UINT srcWidth, UINT srcHeight, UINT destWidth, UINT destHeight);
	public:
		D3D();
		virtual ~D3D();

		HRESULT initialize(Window *pWin, Tstring* pStrResult = NULL); //starts up the engine
		HRESULT shutdown(); //shuts down the engine
		//HRESULT showPage(unsigned char *pRawPage, UINT width, UINT height); //draws the raw page (array of indices into graphics palette)
		//HRESULT setPalette(Palette<UINT>* pPalette); //sets the graphics palette by copying
		HRESULT present(unsigned char *pRawPage, UINT width, UINT height, Palette<UINT> *pPalette); //draws the raw page (array of indices into palette), and sets the palette; if pPalette is NULL, the page is presented with the previous palette; if pRawpage is NULL, the page is presented with the new palette; if both are NULL, the image is presented again
		HRESULT present32(unsigned int *pRawPage, UINT width, UINT height);
		HRESULT screenShot(LPCTSTR strName); //gets a screenshot, appending the correct format image to the end of the name
		void onLostDevice();
		void onResetDevice();

		//option setting
		HRESULT setViewFullscreen(BOOL bFullscreen); //sets view to fullscreen if true
		HRESULT setResolution(LPCRECT pRect); //sets the dimensions of the backbuffer
		HRESULT setVsyncEnabled(BOOL bVsync); //enables vsync if true
		void setSmooth(BOOL bSmoothDraw); //enables linear interpolation used on texture drawing
		void setAspectRatioPreservation(BOOL bPreserve); //enables aspect ratio preservation through all screen resolutions
		void setImageFileFormat(D3DXIMAGE_FILEFORMAT format); //sets the image file format of any screenshots

		//info
		RECT getResolution(); //returns active resolution
		Palette<UINT> getPalette(); //returns a reference of the palette, non-deletable
		BOOL isVsyncEnabled(); //returns true if vsync is enabled
		BOOL isViewFullscreen(); //returns true if view is fullscreen
		BOOL isSmooth(); //returns true if linear interpolation is used on the texture
		BOOL isAspectRatioPreserved(); //returns true if aspect ratio is preserved
		BOOL isScreenShotsActive(); //returns true if screen shot library was loaded
		D3DXIMAGE_FILEFORMAT getImageFileFormat(); //returns image file format of screenshots
	};
}
