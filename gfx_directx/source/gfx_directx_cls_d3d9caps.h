//gfx_directx_cls_d3d9caps.h
//by Jay Tennant 11/30/09
//manages d3d9 caps

#ifndef GFX_DIRECTX_CLS_D3D9CAPS
#define GFX_DIRECTX_CLS_D3D9CAPS

#include <d3d9.h>
#include <tchar.h>

namespace gfx
{
	class Dx9Caps
	{
		D3DCAPS9 m_caps;
		TCHAR m_output[512];
	public:
		Dx9Caps();
		int Test(IDirect3DDevice9 *pd3ddevice);
		const TCHAR* GetOutput() const;
		const D3DCAPS9* GetCaps() const;
	};
}

#endif