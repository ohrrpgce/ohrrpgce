#include <cstring>
#include "Video.h"
using namespace gfx;

Video::Video() : m_pBackBuffer(0), m_pSurface(0), m_bFullscreen(false), m_bSmooth(false), m_bARP(false), m_bInitialized(false)
{
	Resolution tmp = {0,0};
	m_resolution = tmp;
	if(::SDL_WasInit(SDL_INIT_EVERYTHING) == 0)
		::SDL_Init(SDL_INIT_VIDEO);
	else
		::SDL_InitSubSystem(SDL_INIT_VIDEO);
}

Video::~Video()
{
	Shutdown();
	::SDL_QuitSubSystem(SDL_INIT_VIDEO);
	if(::SDL_WasInit(SDL_INIT_EVERYTHING))
		::SDL_Quit();
}

int Video::Initialize()
{
	Shutdown();

	Resolution tmp = {800, 600};
	m_resolution = tmp;
	m_pBackBuffer = ::SDL_SetVideoMode(m_resolution.w, m_resolution.h, 32, /*SDL_RESIZABLE |*/ SDL_HWSURFACE); //experimenting
	if(m_pBackBuffer == NULL)
		return 0;
	m_bInitialized = true;
	return 1;
}

void Video::Shutdown()
{
	m_bInitialized = false;
	if(m_pBackBuffer != NULL)
		::SDL_FreeSurface(m_pBackBuffer);
	m_pBackBuffer = NULL;
}

void Video::StretchCopy()
{
	::SDL_LockSurface(m_pBackBuffer);

	for(Uint32 h = 0; h < m_image.height; h++)
		for(Uint32 w = 0; w < m_image.width; w++)
			((Uint32*)m_pBackBuffer->pixels)[h * m_pBackBuffer->pitch / 4 + w] = m_image.palette[m_image.pSurface[h * 200 + w]];

	::SDL_UnlockSurface(m_pBackBuffer);
}

int Video::Present(Uint8 *pRawPage, Uint32 width, Uint32 height, gfx::Palette<Uint32> *pPalette)
{
	if(!m_bInitialized)
		return 0;
	if(pRawPage != 0)
	{
		if(m_image.pSurface == NULL)
		{
			m_image.AllocateSurface(width, height);
			if(m_image.pSurface == NULL)
				return 0;
		}
		::memcpy_s((void*)m_image.pSurface, m_image.width * m_image.height, (void*)pRawPage, width * height);
	}
	if(pPalette != 0)
		m_image.palette = *pPalette;
	if(m_image.pSurface == NULL)
		return 0;

	StretchCopy();

	::SDL_Flip(m_pBackBuffer);
	return 1;
}

int Video::ScreenShot(const tstring &strName)
{
	if(!m_bInitialized)
		return 0;
	return 0;
}

int Video::SetView(bool bWindowed)
{
	if(!m_bInitialized)
		return 0;
	return 0;
}

int Video::SetResolution(Uint32 width, Uint32 height)
{//needs work
	if(!m_bInitialized)
		return 0;
	m_resolution.w = width;
	m_resolution.h = height;
	return 0;
}

void Video::SetSmooth(bool bSmooth)
{
}

void Video::SetAspectRatioPreservation(bool bEnable)
{
}

Palette<Uint32> Video::GetPalette()
{
	return m_image.palette;
}

Resolution Video::GetResolution()
{
	return m_resolution;
}

bool Video::IsViewFullscreen()
{
	return m_bFullscreen;
}

bool Video::IsSmooth()
{
	return m_bSmooth;
}

bool Video::IsAspectRatioPreserved()
{
	return m_bARP;
}
