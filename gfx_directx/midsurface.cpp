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

int MidSurface::Initialize(IDirect3DDevice9* d3ddev, UINT width, UINT height, D3DFORMAT surfaceFormat)
{
	m_bInitialized = false;
	m_surface = NULL;
	m_d3ddev = NULL;

	if(d3ddev == NULL || width == 0 || height == 0)
		return -1;
	m_d3ddev = d3ddev;
	m_dimensions.cx = width;
	m_dimensions.cy = height;
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
	for(UINT i = 0; i < height && i < (UINT)m_dimensions.cy; i++)
		for(UINT j = 0; j < width && j < (UINT)m_dimensions.cx; j++)
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