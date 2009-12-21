#include "gfx_directx_cls_midsurface.h"
using namespace gfx;

//MidTexture::MidTexture() : m_bInitialized(false), m_format(D3DFMT_A8R8G8B8)
//{
//	::ZeroMemory(&m_dimensions, sizeof(m_dimensions));
//}
//
//MidTexture::~MidTexture()
//{
//	m_bInitialized = false;
//	m_texture = NULL;
//	m_d3ddev = NULL;
//}
//
//int MidTexture::Initialize(IDirect3DDevice9* d3ddev, UINT width, UINT height, D3DFORMAT surfaceFormat)
//{
//	m_bInitialized = false;
//	m_texture = NULL;
//	m_d3ddev = NULL;
//
//	if(d3ddev == NULL || width == 0 || height == 0)
//		return -1;
//	m_d3ddev = d3ddev;
//	m_dimensions.cx = 1;
//	m_dimensions.cy = 1;
//	D3DCAPS9 caps;
//	m_d3ddev->GetDeviceCaps(&caps);
//	if(caps.TextureCaps & D3DPTEXTURECAPS_POW2)
//	{
//		while(m_dimensions.cx < width)
//			m_dimensions.cx <<= 1;
//		while(m_dimensions.cy < height)
//			m_dimensions.cy <<= 1;
//	}
//	else
//	{
//		m_dimensions.cx = width;
//		m_dimensions.cy = height;
//	}
//	if(caps.TextureCaps & D3DPTEXTURECAPS_SQUAREONLY)
//	{
//		if(width > height)
//			height = width;
//		else
//			width = height;
//	}
//	m_format = surfaceFormat;
//	m_d3ddev->CreateTexture(m_dimensions.cx, m_dimensions.cy, 0, D3DUSAGE_DYNAMIC, m_format, D3DPOOL_DEFAULT, &m_texture, 0);
//	//::D3DXCreateTexture(m_d3ddev, m_dimensions.cx, m_dimensions.cy, 0, D3DUSAGE_DYNAMIC, m_format, D3DPOOL_DEFAULT, &m_texture);
//	if(m_texture == NULL)
//		return -1;
//	m_bInitialized = true;
//	return 0;
//}
//
//void MidTexture::CopySystemPage(UCHAR *pRawPage, UINT width, UINT height, gfx::Palette<UINT> *pPalette)
//{//specific to ohr; can't be reused elsewhere much
//	if(!m_bInitialized)
//		return;
//	if(!pRawPage || !pPalette)
//		return;
//
//	D3DLOCKED_RECT lr;
//	HRESULT hr = m_texture->LockRect(0, &lr, 0, 0);
//	UINT* pData = (UINT*)lr.pBits;
//	for(UINT i = 0; i < height && i < m_dimensions.cy; i++)
//		for(UINT j = 0; j < width && j < m_dimensions.cx; j++)
//			pData[i * lr.Pitch / 4 + j] = (*pPalette)[pRawPage[i * width + j]] | 0xff000000;
//	hr = m_texture->UnlockRect(0);
//}
//
//void MidTexture::CopyMidTexture(gfx::MidTexture *pTex)
//{//assuming formats are identical and UINT, which could go very, very wrong
//	if(!m_bInitialized)
//		return;
//	if(!pTex)
//		return;
//	if(pTex->GetTexture() == NULL)
//		return;
//
//	UINT w1 = m_dimensions.cx, 
//		 w2 = pTex->GetDimensions().cx, 
//		 h1 = m_dimensions.cy, 
//		 h2 = pTex->GetDimensions().cy;
//
//	D3DLOCKED_RECT tex1, tex2;
//	m_texture->LockRect(0, &tex1, 0, 0);
//	pTex->GetTexture()->LockRect(0, &tex2, 0, 0);
//	UINT *p1 = (UINT*)tex1.pBits,
//		 *p2 = (UINT*)tex2.pBits;
//	for(UINT i = 0; i < h1 && i < h2; i++)
//		for(UINT j = 0; j < w1 && j < w2; j++)
//			p1[i * tex1.Pitch / 4 + j] = p2[i * tex2.Pitch / 4 + j];
//	pTex->GetTexture()->UnlockRect(0);
//	m_texture->UnlockRect(0);
//}
//
//D3DFORMAT MidTexture::GetFormat()
//{
//	return m_format;
//}
//
//SIZE MidTexture::GetDimensions()
//{
//	return m_dimensions;
//}
//
//IDirect3DTexture9* MidTexture::GetTexture()
//{
//	return m_texture;
//}
//
//void MidTexture::OnLostDevice()
//{
//	m_texture = NULL;
//}
//
//void MidTexture::OnResetDevice()
//{
//	Initialize(m_d3ddev, m_dimensions.cx, m_dimensions.cy, m_format);
//}




MidSurface::MidSurface() : m_bInitialized(false), m_format(D3DFMT_A8R8G8B8)
{
	::ZeroMemory(&m_dimensions, sizeof(m_dimensions));
}

MidSurface::~MidSurface()
{
	m_bInitialized = false;
	m_surface = NULL;
	m_d3ddev = NULL;
}

int MidSurface::Initialize(IDirect3DDevice9* d3ddev, UINT width, UINT height, D3DFORMAT surfaceFormat)
{
	m_bInitialized = false;
	m_surface = NULL;
	m_d3ddev = NULL;

	if(d3ddev == NULL || width == 0 || height == 0)
		return -1;
	m_d3ddev = d3ddev;
	m_dimensions.cx = 1;
	m_dimensions.cy = 1;
	D3DCAPS9 caps;
	m_d3ddev->GetDeviceCaps(&caps);
	if(caps.TextureCaps & D3DPTEXTURECAPS_POW2)
	{//these tests might not be necessary now that we're not using textures
		while(m_dimensions.cx < width)
			m_dimensions.cx <<= 1;
		while(m_dimensions.cy < height)
			m_dimensions.cy <<= 1;
	}
	else
	{
		m_dimensions.cx = width;
		m_dimensions.cy = height;
	}
	if(caps.TextureCaps & D3DPTEXTURECAPS_SQUAREONLY)
	{
		if(m_dimensions.cx > m_dimensions.cy)
			m_dimensions.cy = m_dimensions.cx;
		else
			m_dimensions.cx = m_dimensions.cy;
	}
	m_format = surfaceFormat;
	m_d3ddev->CreateOffscreenPlainSurface(m_dimensions.cx, m_dimensions.cy, m_format, D3DPOOL_DEFAULT, &m_surface, 0);
	if(m_surface == NULL)
		return -1;
	m_bInitialized = true;
	return 0;
}

void MidSurface::CopySystemPage(UCHAR *pRawPage, UINT width, UINT height, gfx::Palette<UINT> *pPalette)
{//specific to ohr; can't be reused elsewhere much
	if(!m_bInitialized)
		return;
	if(!pRawPage || !pPalette)
		return;

	TEST_ONLY_BLOCK(
		static bool bRunOnce = false;
		if(!bRunOnce)
		{
			TCHAR strInfoBuffer[128] = TEXT("");
			::_stprintf_s<128>(strInfoBuffer, TEXT("Width: %d, Height: %d"), width, height);
			::MessageBox(0, strInfoBuffer, TEXT("Run Once"), MB_OK);
			bRunOnce = true;
		}
	);

	D3DLOCKED_RECT lr;
	HRESULT hr = m_surface->LockRect(&lr, 0, 0);
	UINT* pData = (UINT*)lr.pBits;
	for(UINT i = 0; i < height && i < m_dimensions.cy; i++)
		for(UINT j = 0; j < width && j < m_dimensions.cx; j++)
			pData[i * lr.Pitch / 4 + j] = (*pPalette)[pRawPage[i * width + j]] | 0xff000000;
	hr = m_surface->UnlockRect();
}

D3DFORMAT MidSurface::GetFormat()
{
	return m_format;
}

SIZE MidSurface::GetDimensions()
{
	return m_dimensions;
}

IDirect3DSurface9* MidSurface::GetSurface()
{
	return m_surface;
}

void MidSurface::OnLostDevice()
{
	m_surface = NULL;
}

void MidSurface::OnResetDevice()
{
	Initialize(m_d3ddev, m_dimensions.cx, m_dimensions.cy, m_format);
}