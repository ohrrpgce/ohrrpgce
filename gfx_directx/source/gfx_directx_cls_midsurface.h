//gfx_directx_cls_midsurface.h, formerly *_midtexture.h
//by Jay Tennant 10/30/09
//manages dynamic texture (not render target) for copying from system to video memory

#ifndef GFX_DIRECTX_CLS_MIDTEXTURE_H
#define GFX_DIRECTX_CLS_MIDTEXTURE_H

#include <windows.h>
#include <d3d9.h>
#include <d3dx9.h>
#include "smartptr.h"

#include "gfx_directx_TESTAPPconst.h"
#include "gfx_directx_cls_palette.h"

namespace gfx
{
	//class MidTexture
	//{
	//protected:
	//	SmartPtr<IDirect3DDevice9> m_d3ddev;
	//	SmartPtr<IDirect3DTexture9> m_texture;
	//	SIZE m_dimensions;
	//	bool m_bInitialized;
	//	D3DFORMAT m_format;
	//public:
	//	MidTexture();
	//	virtual ~MidTexture();

	//	int Initialize(IDirect3DDevice9* d3ddev, UINT width, UINT height, D3DFORMAT surfaceFormat = D3DFMT_A8R8G8B8);
	//	void CopySystemPage(UCHAR *pRawPage, UINT width, UINT height, Palette<UINT> *pPalette); //copies system page into texture converting using the palette
	//	void CopyMidTexture(MidTexture *pTex); //copies another MidTexture; should generalize to Texture class, of which this inherits

	//	D3DFORMAT GetFormat();
	//	SIZE GetDimensions();
	//	IDirect3DTexture9* GetTexture();

	//	void OnLostDevice();
	//	void OnResetDevice();
	//};

	class MidSurface //replacing the MidTexture class
	{
	protected:
		SmartPtr<IDirect3DDevice9> m_d3ddev;
		SmartPtr<IDirect3DSurface9> m_surface;
		SIZE m_dimensions;
		bool m_bInitialized;
		D3DFORMAT m_format;
	public:
		MidSurface();
		virtual ~MidSurface();

		int Initialize(IDirect3DDevice9* d3ddev, UINT width, UINT height, D3DFORMAT surfaceFormat = D3DFMT_A8R8G8B8);
		void CopySystemPage(UCHAR *pRawPage, UINT width, UINT height, Palette<UINT> *pPalette); //copies system page into texture converting using the palette

		D3DFORMAT GetFormat();
		SIZE GetDimensions();
		IDirect3DSurface9* GetSurface();

		void OnLostDevice();
		void OnResetDevice();
	};

}

#endif