//midsurface.h
//by Jay Tennant 10/30/09; updated 4/21/11
//manages dynamic surface for copying from system to video memory

#pragma once

#include <windows.h>
#include <d3d9.h>
#include <d3dx9.h>
#include "smartptr.h"

#include "palette.h"

namespace gfx
{
	class MidSurface
	{
	protected:
		SmartPtr<IDirect3DDevice9> m_d3ddev;
		SmartPtr<IDirect3DSurface9> m_surface;
		SIZE m_dimensions;
		bool m_bInitialized;
		D3DFORMAT m_format;
	public:
		MidSurface();
		~MidSurface();

		HRESULT initialize(IDirect3DDevice9* d3ddev, UINT width, UINT height, D3DFORMAT surfaceFormat = D3DFMT_A8R8G8B8);
		void copySystemPage(UCHAR *pRawPage, UINT width, UINT height, Palette<UINT> *pPalette); //copies system page into texture converting using the palette
		void copySystemPage32(UINT *pRawPage, UINT width, UINT height);

		D3DFORMAT getFormat();
		SIZE getDimensions();
		IDirect3DSurface9* getSurface();

		void onLostDevice();
		void onResetDevice();
	};
}