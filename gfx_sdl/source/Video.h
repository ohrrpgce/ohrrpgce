//Video.h
//started 1/5/10
//sdl graphics interface

#ifndef GFX_VIDEO_H
#define GFX_VIDEO_H

#include "SDL.h"
#include "gfx_palette.h"
#include "_tstring.h"

namespace gfx
{
	struct Resolution { Uint32 w, h; };

	class Video
	{
	protected:
		SDL_Surface *m_pFrontBuffer;
		SDL_Surface *m_pBackBuffer;
		struct Image
		{
			Image() : pSurface(0), width(0), height(0){}
			~Image() {Free(); palette.Free();}
			void AllocateSurface(Uint32 nWidth, Uint32 nHeight)
			{
				Free();
				if(nWidth == 0 || nHeight == 0)
					return;
				width = nWidth;
				height = nHeight;
				pSurface = new Uint8[width * height];
			}
			void Free()
			{
				if(pSurface != NULL) 
					delete [] pSurface; 
				pSurface = NULL; 
			}
			Uint8 *pSurface;
			Uint32 width;
			Uint32 height;
			Palette<Uint32> palette;
		} m_image;
		bool m_bFullscreen;
		bool m_bSmooth;
		bool m_bARP; //aspect ratio preservation
		bool m_bInitialized;
	public:
		Video();
		virtual ~Video();

		int Initialize();
		void Shutdown();
		int Present(Uint8 *pRawPage, Uint32 width, Uint32 height, Palette<Uint32> *pPalette);
		int ScreenShot(const tstring& strName);
		int SetView(bool bWindowed); //if true, sets to windowed mode; else, sets to fullscreen mode
		int SetResolution(Uint32 width, Uint32 height);
		void SetSmooth(bool bSmooth); //if true, enables the smoothing algorithm
		void SetAspectRatioPreservation(bool bEnable);

		Palette<Uint32> GetPalette();
		Resolution GetResolution();
		bool IsViewFullscreen();
		bool IsSmooth();
		bool IsAspectRatioPreserved();
	};
}

#endif