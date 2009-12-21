#include "gfx_directx_cls_d3d9caps.h"
using namespace gfx;

Dx9Caps::Dx9Caps()
{
	::ZeroMemory(m_output, sizeof(m_output));
	::ZeroMemory(&m_caps, sizeof(m_caps));
}

int Dx9Caps::Test(IDirect3DDevice9 *pd3ddevice)
{
	if(!pd3ddevice)
		return -1;
	if(D3D_OK != pd3ddevice->GetDeviceCaps(&m_caps))
		return -1;
	::ZeroMemory(m_output, sizeof(m_output));
	::_tcscat_s<sizeof(m_output) / sizeof(TCHAR)>(m_output, TEXT("Dynamic Texture Support:    "));
	::_tcscat_s<sizeof(m_output) / sizeof(TCHAR)>(m_output, (m_caps.Caps2 & D3DCAPS2_DYNAMICTEXTURES) ? TEXT("Yes") : TEXT("No"));
	::_tcscat_s<sizeof(m_output) / sizeof(TCHAR)>(m_output, TEXT("\r\nTexture Alpha Support:      "));
	::_tcscat_s<sizeof(m_output) / sizeof(TCHAR)>(m_output, (m_caps.TextureCaps & D3DPTEXTURECAPS_ALPHA) ? TEXT("Yes") : TEXT("No"));
	::_tcscat_s<sizeof(m_output) / sizeof(TCHAR)>(m_output, TEXT("\r\nTexture must be Power of 2: "));
	::_tcscat_s<sizeof(m_output) / sizeof(TCHAR)>(m_output, (m_caps.TextureCaps & D3DPTEXTURECAPS_POW2) ? TEXT("Yes") : TEXT("No"));
	::_tcscat_s<sizeof(m_output) / sizeof(TCHAR)>(m_output, TEXT("\r\nTexture must be Square:     "));
	::_tcscat_s<sizeof(m_output) / sizeof(TCHAR)>(m_output, (m_caps.TextureCaps & D3DPTEXTURECAPS_SQUAREONLY) ? TEXT("Yes") : TEXT("No"));
	::_tcscat_s<sizeof(m_output) / sizeof(TCHAR)>(m_output, TEXT("\r\nTexture can be clamped:     "));
	::_tcscat_s<sizeof(m_output) / sizeof(TCHAR)>(m_output, (m_caps.TextureAddressCaps  & D3DPTADDRESSCAPS_CLAMP) ? TEXT("Yes") : TEXT("No"));
	TCHAR buffer[16] = TEXT("");
	::_itot_s<sizeof(buffer)/sizeof(TCHAR)>(m_caps.VertexShaderVersion & 0xffff, buffer, 16);
	::_tcscat_s<sizeof(m_output) / sizeof(TCHAR)>(m_output, TEXT("\r\nVertex Shader version:      "));
	::_tcscat_s<sizeof(m_output) / sizeof(TCHAR)>(m_output, buffer);
	::_itot_s<sizeof(buffer)/sizeof(TCHAR)>(m_caps.PixelShaderVersion & 0xffff, buffer, 16);
	::_tcscat_s<sizeof(m_output) / sizeof(TCHAR)>(m_output, TEXT("\r\nPixel Shader version:       "));
	::_tcscat_s<sizeof(m_output) / sizeof(TCHAR)>(m_output, buffer);
	return 0;
}

const TCHAR* Dx9Caps::GetOutput() const
{
	return m_output;
}

const D3DCAPS9* Dx9Caps::GetCaps() const
{
	return &m_caps;
}