#include "midsurface.h"
using namespace gfx;

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

HRESULT MidSurface::initialize(IDirect3DDevice9* d3ddev, UINT width, UINT height, D3DFORMAT surfaceFormat)
{
	m_bInitialized = false;
	m_surface = NULL;
	m_d3ddev = NULL;

	if(d3ddev == NULL)
		return E_POINTER;
	if(width == 0 || height == 0)
		return E_INVALIDARG;

	m_d3ddev = d3ddev;
	m_dimensions.cx = width;
	m_dimensions.cy = height;
	m_format = surfaceFormat;

	HRESULT hr = m_d3ddev->CreateOffscreenPlainSurface(m_dimensions.cx, m_dimensions.cy, m_format, D3DPOOL_DEFAULT, &m_surface, 0);
	if(FAILED(hr))
		return hr;

	m_bInitialized = true;
	return hr;
}

void MidSurface::copySystemPage(UCHAR *pRawPage, UINT width, UINT height, gfx::Palette<UINT> *pPalette)
{//specific to ohr; can't be reused elsewhere much
	if(!m_bInitialized)
		return;
	if(!pRawPage || !pPalette)
		return;
	if(m_surface == NULL)
		return;

	D3DLOCKED_RECT lr;
	HRESULT hr = m_surface->LockRect(&lr, 0, 0);
	UINT* pData = (UINT*)lr.pBits;
	for(UINT i = 0; i < height && i < (UINT)m_dimensions.cy; i++)
		for(UINT j = 0; j < width && j < (UINT)m_dimensions.cx; j++)
			pData[i * lr.Pitch / 4 + j] = (*pPalette)[pRawPage[i * width + j]] | 0xff000000;
	hr = m_surface->UnlockRect();
}

void MidSurface::copySystemPage32(UINT *pRawPage, UINT width, UINT height)
{//specific to ohr; can't be reused elsewhere much
	if(!m_bInitialized)
		return;
	if(m_surface == NULL)
		return;

	D3DLOCKED_RECT lr;
	HRESULT hr = m_surface->LockRect(&lr, 0, 0);
	UINT* pData = (UINT*)lr.pBits;
	for(UINT i = 0; i < height && i < (UINT)m_dimensions.cy; i++)
		for(UINT j = 0; j < width && j < (UINT)m_dimensions.cx; j++)
			pData[i * lr.Pitch / 4 + j] = pRawPage[i * width + j];
	hr = m_surface->UnlockRect();
}

D3DFORMAT MidSurface::getFormat()
{
	return m_format;
}

SIZE MidSurface::getDimensions()
{
	return m_dimensions;
}

IDirect3DSurface9* MidSurface::getSurface()
{
	return m_surface;
}

void MidSurface::onLostDevice()
{
	m_surface = NULL;
}

void MidSurface::onResetDevice()
{
	initialize(m_d3ddev, m_dimensions.cx, m_dimensions.cy, m_format);
}