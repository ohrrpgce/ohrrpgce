#define DFI_UNIQUE
#include "debugmsg.h"
#include "d3d.h"
using namespace gfx;

D3D::D3D() 
: m_pWindow(NULL), m_bInitialized(FALSE), m_bVSync(TRUE), m_bSmoothDraw(FALSE),
  m_bPreserveAspectRatio(TRUE), m_saveFormat(D3DXIFF_PNG),
  DXCreate(TEXT("d3d9.dll")),
  DXScreenShot(TEXT("d3dx9_24.dll"))
{
	::ZeroMemory(&m_d3dpp, sizeof(m_d3dpp));

	Init_Direct3DCreate9();
	Init_D3DXSaveSurfaceToFileA();
	Init_D3DXSaveSurfaceToFileW();
}

D3D::~D3D()
{
	shutdown();
}

RECT D3D::calculateAspectRatio(UINT srcWidth, UINT srcHeight, UINT destWidth, UINT destHeight)
{
	float destAspect = (float)destWidth / (float)destHeight;
	float srcAspect = (float)srcWidth / (float)srcHeight;
	float xScale, yScale;
	if(srcAspect < destAspect)
	{
		yScale = 1.0f;
		xScale = srcAspect / destAspect;
	}
	else
	{
		xScale = 1.0f;
		yScale = destAspect / srcAspect;
	}
	float corners[2][2] = {{-xScale, yScale}, {xScale, -yScale}};
	LONG xCenter = destWidth / 2;
	LONG yCenter = destHeight / 2;
	RECT r = {0};
	r.left = xCenter + (LONG)(corners[0][0] * (float)destWidth / 2.0f);
	r.bottom = yCenter + (LONG)(corners[0][1] * (float)destHeight / 2.0f);
	r.right = xCenter + (LONG)(corners[1][0] * (float)destWidth / 2.0f);
	r.top = yCenter + (LONG)(corners[1][1] * (float)destHeight / 2.0f);
	return r;
}

RECT D3D::getImageRect()
{
	SIZE clientSize = m_pWindow->getClientSize();
	if(m_bPreserveAspectRatio)
	{
		SIZE imageSize = m_surface.getDimensions();
		return calculateAspectRatio(imageSize.cx, imageSize.cy, clientSize.cx, clientSize.cy);
	}
	else
	{
		RECT rImage = {0, 0, clientSize.cx, clientSize.cy};
		return rImage;
	}
}

SIZE D3D::getImageResolution()
{
	return m_surface.getDimensions();
}

HRESULT D3D::initialize(gfx::Window *pWin)
{
	HRESULT hr = S_OK;
	if(!pWin)
		return E_POINTER;
	shutdown();

	m_pWindow = pWin;
	m_sizeWindowed = m_pWindow->getClientSize();

	RECT rDesktop;
	::GetWindowRect(::GetDesktopWindow(), &rDesktop);
	m_sizeFullscreen.cx = rDesktop.right;
	m_sizeFullscreen.cy = rDesktop.bottom;

	::ZeroMemory(&m_d3dpp, sizeof(m_d3dpp));
	m_d3dpp.BackBufferFormat		= D3DFMT_X8R8G8B8;
	m_d3dpp.BackBufferWidth			= m_sizeWindowed.cx;
	m_d3dpp.BackBufferHeight		= m_sizeWindowed.cy;
	m_d3dpp.hDeviceWindow			= m_pWindow->getWindowHandle();
	if(!m_bVSync)
		m_d3dpp.PresentationInterval= D3DPRESENT_INTERVAL_IMMEDIATE;
	m_d3dpp.SwapEffect				= D3DSWAPEFFECT_DISCARD;
	m_d3dpp.Windowed				= TRUE;

	if(Direct3DCreate9 == NULL)
	{
		Debug(errError, "Direct3DCreate9() failed to load! Possibly d3d9.dll missing.");
		return E_FAIL;
	}
	m_d3d.Attach(Direct3DCreate9(D3D_SDK_VERSION));
	if(m_d3d == NULL)
	{
		Debug(errError, "IDirect3D9 object failed to be created! (Header and runtime version mismatch?)");
		return E_FAIL;
	}

	D3DADAPTER_IDENTIFIER9 adapterID;
	hr = m_d3d->GetAdapterIdentifier(D3DADAPTER_DEFAULT, 0, &adapterID);
	if(FAILED(hr)) {
		Debug(errError, "Unable to query adapter information! Error %s", HRESULTString(hr));
	}
	else
	{
		Debug(errInfo, "Adapter: %s", adapterID.Description);
		Debug(errInfo, "Driver: %s", adapterID.Driver);
	}

	hr = m_d3d->CreateDevice(D3DADAPTER_DEFAULT, 
							 D3DDEVTYPE_HAL, 
							 m_pWindow->getWindowHandle(), 
							 D3DCREATE_HARDWARE_VERTEXPROCESSING | D3DCREATE_FPU_PRESERVE | D3DCREATE_PUREDEVICE | D3DCREATE_NOWINDOWCHANGES, 
							 &m_d3dpp, 
							 &m_d3ddev);
	if(FAILED(hr))
	{
		hr = m_d3d->CreateDevice(D3DADAPTER_DEFAULT, 
								 D3DDEVTYPE_HAL, 
								 m_pWindow->getWindowHandle(), 
								 D3DCREATE_SOFTWARE_VERTEXPROCESSING | D3DCREATE_FPU_PRESERVE | D3DCREATE_NOWINDOWCHANGES, 
								 &m_d3dpp, 
								 &m_d3ddev);
		if(FAILED(hr))
		{
			Debug(errError, "IDirect3DDevice9 object failed to be created! Possibly lack of hardware support. Error %s", HRESULTString(hr));
			return hr;
		}
		else
			Debug(errInfo, "IDirect3DDevice9 object created as software device.");
	}
	else
		Debug(errInfo, "IDirect3DDevice9 object created as hardware device.");
	if(FAILED(m_surface.initialize(m_d3ddev, 320, 200)))
	{
		Debug(errError, "IDirect3DSurface9 object failed to be created! Error %s", HRESULTString(hr));
		return E_FAIL;
	}
	m_bInitialized = TRUE;
	//hr = m_d3ddev->Clear(0, 0, D3DCLEAR_TARGET, 0x0, 1.0f, 0);
	SmartPtr<IDirect3DSurface9> backBuffer;
	m_d3ddev->GetBackBuffer(0, 0, D3DBACKBUFFER_TYPE_MONO, &backBuffer);
	hr = m_d3ddev->ColorFill(backBuffer, NULL, 0x0);
	backBuffer = NULL;
	hr = m_d3ddev->Present(NULL, NULL, NULL, NULL);

	if(D3DXSaveSurfaceToFile == NULL)
		// Not an interesting error
		Debug(errInfo, "D3DXSaveSurfaceToFile() failed to load. Probably lacking d3dx_24.dll.");
	else
		Debug(errInfo, "D3DXSaveSurfaceToFile() successfully loaded.");

	return S_OK;
}

HRESULT D3D::shutdown()
{
	m_bInitialized = FALSE;
	m_surface.initialize(0,0,0);
	m_d3ddev = NULL;
	m_d3d = NULL;
	return S_OK;
}

//HRESULT D3D::ShowPage(unsigned char *pRawPage, UINT width, UINT height)
//{
//	if(!m_bInitialized)
//		return E_FAIL;
//	HRESULT hrCoopLevel = m_d3ddev->TestCooperativeLevel();
//	if(hrCoopLevel == D3DERR_DEVICELOST)
//	{
//		OnLostDevice();
//		return S_OK;
//	}
//	else if(hrCoopLevel == D3DERR_DEVICENOTRESET)
//	{
//		OnResetDevice();
//		return S_OK;
//	}
//	else if(hrCoopLevel == D3DERR_DRIVERINTERNALERROR)
//	{
//		if(IDYES == ::MessageBox(0, TEXT("Internal driver failure! Attempt to recover?"), TEXT("Critical Failure"), MB_ICONEXCLAMATION | MB_YESNO))
//			return Shutdown();
//		Shutdown();
//		return initialize(m_pWindow);
//	}
//	if(pRawPage != NULL)
//	{
//		if(m_image.pSurface == NULL)
//			m_image.AllocateSurface(width, height);
//		if(m_image.pSurface != NULL)
//		{
//			for(UINT i = 0; i < width * height; i++)
//				m_image.pSurface[i] = pRawPage[i];
//		}
//	}
//	m_surface.CopySystemPage(m_image.pSurface, m_image.width, m_image.height, &m_image.palette);
//
//	//Clear needs to happen outside of BeginScene()/EndScene() block
//	HRESULT hr = S_OK;
//	hr = m_d3ddev->Clear(0, 0, D3DCLEAR_TARGET, 0xff000000, 1.0f, 0);
//	if(FAILED(hr))
//		return hr;
//
//	RECT rAspectRatio = {0};
//	if(m_bPreserveAspectRatio)
//		rAspectRatio = CalculateAspectRatio(m_surface.GetDimensions().cx, m_surface.GetDimensions().cy, m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight);
//	SmartPtr<IDirect3DSurface9> pBackBuffer;
//	hr = m_d3ddev->GetBackBuffer(0, 0, D3DBACKBUFFER_TYPE_MONO, &pBackBuffer);
//	if(FAILED(hr))
//		return hr;
//	hr = m_d3ddev->StretchRect(m_surface.GetSurface(), 0, pBackBuffer, (m_bPreserveAspectRatio ? &rAspectRatio : NULL), (m_bSmoothDraw ? D3DTEXF_LINEAR : D3DTEXF_POINT));
//	if(FAILED(hr))
//		return hr;
//
//	hr = m_d3ddev->Present(0,0,0,0);
//	return hr;
//}
//
//HRESULT D3D::SetPalette(gfx::Palette<UINT> *pPalette)
//{
//	if(pPalette == NULL)
//		return E_POINTER;
//	m_image.palette = *pPalette;
//	return S_OK;
//}

HRESULT D3D::present(unsigned char *pRawPage, UINT width, UINT height, gfx::Palette<UINT> *pPalette)
{
	if(!m_bInitialized)
		return E_FAIL;

	//palette setting
	if(pPalette != NULL)
		m_image.palette = *pPalette;

	//page copy
	if(pRawPage != NULL)
	{
		if(m_image.pSurface == NULL)
			m_image.allocateSurface(width, height);
		if(m_image.pSurface != NULL)
		{
			for(UINT i = 0; i < width * height; i++)
				m_image.pSurface[i] = pRawPage[i];
		}
	}
	m_surface.copySystemPage(m_image.pSurface, m_image.width, m_image.height, &m_image.palette);

	//coop-level test
	HRESULT hrCoopLevel = m_d3ddev->TestCooperativeLevel();
	if(hrCoopLevel == D3DERR_DEVICELOST)
	{
		onLostDevice();
		return S_OK;
	}
	else if(hrCoopLevel == D3DERR_DEVICENOTRESET)
	{
		onResetDevice();
		return S_OK;
	}
	else if(hrCoopLevel == D3DERR_DRIVERINTERNALERROR)
	{
		if(IDNO == ::MessageBox(0, TEXT("Internal driver failure! Attempt to recover?"), TEXT("Critical Failure"), MB_ICONEXCLAMATION | MB_YESNO))
			return shutdown();
		shutdown();
		return initialize(m_pWindow);
	}

	//present
	HRESULT hr = S_OK;
	//doesn't work; apparently calling only Clear(), no BeginScene()/EndScene() pair, then Present() without any additional
	//rendering causes Clear() to stop functioning correctly. Oi!
	//hr = m_d3ddev->Clear(0, 0, D3DCLEAR_TARGET, 0x0, 1.0f, 0);

	RECT rAspectRatio = {0};
	if(m_bPreserveAspectRatio)
		rAspectRatio = calculateAspectRatio(m_surface.getDimensions().cx, m_surface.getDimensions().cy, m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight);

	SmartPtr<IDirect3DSurface9> pBackBuffer;
	hr = m_d3ddev->GetBackBuffer(0, 0, D3DBACKBUFFER_TYPE_MONO, &pBackBuffer);
	if(FAILED(hr))
		return hr;

	hr = m_d3ddev->ColorFill(pBackBuffer, NULL, 0x0);
	if(FAILED(hr))
		return hr;

	// srcSurface, srcRect, destSurface, destRect
	hr = m_d3ddev->StretchRect(m_surface.getSurface(), 0, pBackBuffer, (m_bPreserveAspectRatio ? &rAspectRatio : NULL), (m_bSmoothDraw ? D3DTEXF_LINEAR : D3DTEXF_POINT));
	if(FAILED(hr))
		return hr;

	hr = m_d3ddev->Present(0,0,0,0);
	return hr;
}

HRESULT D3D::present32(unsigned int *pRawPage, UINT width, UINT height)
{
	if(!m_bInitialized)
		return E_FAIL;

	//page copy
	m_surface.copySystemPage32(pRawPage, width, height);

	//coop-level test
	HRESULT hrCoopLevel = m_d3ddev->TestCooperativeLevel();
	if(hrCoopLevel == D3DERR_DEVICELOST)
	{
		onLostDevice();
		return S_OK;
	}
	else if(hrCoopLevel == D3DERR_DEVICENOTRESET)
	{
		onResetDevice();
		return S_OK;
	}
	else if(hrCoopLevel == D3DERR_DRIVERINTERNALERROR)
	{
		if(IDNO == ::MessageBox(0, TEXT("Internal driver failure! Attempt to recover?"), TEXT("Critical Failure"), MB_ICONEXCLAMATION | MB_YESNO))
			return shutdown();
		shutdown();
		return initialize(m_pWindow);
	}

	//present
	HRESULT hr = S_OK;
	//doesn't work; apparently calling only Clear(), no BeginScene()/EndScene() pair, then Present() without any additional
	//rendering causes Clear() to stop functioning correctly. Oi!
	//hr = m_d3ddev->Clear(0, 0, D3DCLEAR_TARGET, 0x0, 1.0f, 0);

	RECT rAspectRatio = {0};
	if(m_bPreserveAspectRatio)
		rAspectRatio = calculateAspectRatio(m_surface.getDimensions().cx, m_surface.getDimensions().cy, m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight);

	SmartPtr<IDirect3DSurface9> pBackBuffer;
	hr = m_d3ddev->GetBackBuffer(0, 0, D3DBACKBUFFER_TYPE_MONO, &pBackBuffer);
	if(FAILED(hr))
		return hr;

	hr = m_d3ddev->ColorFill(pBackBuffer, NULL, 0x0);
	if(FAILED(hr))
		return hr;

	hr = m_d3ddev->StretchRect(m_surface.getSurface(), 0, pBackBuffer, (m_bPreserveAspectRatio ? &rAspectRatio : NULL), (m_bSmoothDraw ? D3DTEXF_LINEAR : D3DTEXF_POINT));
	if(FAILED(hr))
		return hr;

	hr = m_d3ddev->Present(0,0,0,0);
	return hr;
}

HRESULT D3D::screenShot(LPCTSTR strName)
{
	if(!m_bInitialized)
		return E_FAIL;
	if(D3DXSaveSurfaceToFile == NULL)
		return E_FAIL;
	if(strName == NULL)
		return E_POINTER;
	if(m_saveFormat == D3DXIFF_FORCE_DWORD)
		return E_FAIL;

	HRESULT hr = S_OK;
	SmartPtr<IDirect3DSurface9> pSurface;
	if(m_d3dpp.Windowed)
	{//have to use desktop size
		HWND hDesktop = ::GetDesktopWindow();
		RECT rDesktop;
		::GetWindowRect(hDesktop, &rDesktop);
		hr = m_d3ddev->CreateOffscreenPlainSurface(rDesktop.right, rDesktop.bottom, D3DFMT_A8R8G8B8, D3DPOOL_SCRATCH, &pSurface, 0);
		if(FAILED(hr))
			return hr;
	}
	else
	{
		hr = m_d3ddev->CreateOffscreenPlainSurface(m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight, D3DFMT_A8R8G8B8, D3DPOOL_SCRATCH, &pSurface, 0);
		if(FAILED(hr))
			return hr;
	}
	hr = m_d3ddev->GetFrontBufferData(0, pSurface);
	if(FAILED(hr))
		return hr;

	RECT rImage = {0,0, m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight};
	if(m_bPreserveAspectRatio)
		rImage = calculateAspectRatio(m_surface.getDimensions().cx, m_surface.getDimensions().cy, m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight);
	if(m_d3dpp.Windowed)
	{
		POINT rpTopLeft = {0,0};
		::ClientToScreen(m_pWindow->getWindowHandle(), &rpTopLeft);
		rImage.left += rpTopLeft.x;
		rImage.right += rpTopLeft.x;
		rImage.top += rpTopLeft.y;
		rImage.bottom += rpTopLeft.y;
	}

	hr = D3DXSaveSurfaceToFile( strName, m_saveFormat, pSurface, 0, &rImage );
	
	return hr;
}

void D3D::onLostDevice()
{
	if(!m_bInitialized)
		return;
	m_surface.onLostDevice();
}

void D3D::onResetDevice()
{
	if(!m_bInitialized)
		return;
	m_d3ddev->Reset(&m_d3dpp);
	m_surface.onResetDevice();
}

HRESULT D3D::setViewFullscreen(BOOL bFullscreen)
{
	if(bFullscreen != m_d3dpp.Windowed)
		return S_OK;

	m_d3dpp.Windowed = !bFullscreen;

	if(m_d3dpp.Windowed)
	{
		m_d3dpp.BackBufferWidth		= m_sizeWindowed.cx;
		m_d3dpp.BackBufferHeight	= m_sizeWindowed.cy;
		//m_d3dpp.SwapEffect			= D3DSWAPEFFECT_COPY;
	}
	else
	{
		m_d3dpp.BackBufferWidth		= m_sizeFullscreen.cx;
		m_d3dpp.BackBufferHeight	= m_sizeFullscreen.cy;
		//m_d3dpp.SwapEffect			= D3DSWAPEFFECT_DISCARD;
		m_pWindow->setWindowSize(m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight);
		m_pWindow->setWindowPosition(0,0);
	}

	if(!m_bInitialized)
		return E_FAIL;

	if(m_d3dpp.Windowed)
	{
		m_pWindow->setClientSize(m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight);
		m_pWindow->centerWindow();
	}
	else
	{
		onLostDevice();
		onResetDevice();
	}
	return S_OK;
}

HRESULT D3D::setResolution(SIZE size)
{
	if(m_d3dpp.Windowed)
	{
		m_sizeWindowed = size;
		m_d3dpp.BackBufferWidth	= size.cx;
		m_d3dpp.BackBufferHeight = size.cy;
		//intentionally not setting these--otherwise will enter recursive pit of despair, abandon all hope!
		//m_pWindow->SetClientSize(m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight);
		//m_pWindow->CenterWindow();
		onLostDevice();
		onResetDevice();
	}
	return S_OK;
}

HRESULT D3D::setVsyncEnabled(BOOL bVsync)
{
	if(bVsync == m_bVSync)
		return S_OK;
	m_bVSync = bVsync;

	if(m_bVSync)
	{
		m_d3dpp.PresentationInterval = 0;
	}
	else
	{
		m_d3dpp.PresentationInterval = D3DPRESENT_INTERVAL_IMMEDIATE;
	}

	onLostDevice();
	onResetDevice();

	return S_OK;
}

void D3D::setSmooth(BOOL bSmoothDraw)
{
	m_bSmoothDraw = bSmoothDraw;
}

void D3D::setAspectRatioPreservation(BOOL bPreserve)
{
	m_bPreserveAspectRatio = bPreserve;
}

void D3D::setImageFileFormat(D3DXIMAGE_FILEFORMAT format)
{
	m_saveFormat = format;
}

SIZE D3D::getResolution()
{
	// This is currently equal to the size of the client area.
	if(m_d3dpp.Windowed)
		return m_sizeWindowed;
	return m_sizeFullscreen;
}

Palette<UINT> D3D::getPalette()
{
	return m_image.palette;
}

BOOL D3D::isVsyncEnabled()
{
	return m_bVSync;
}

BOOL D3D::isViewFullscreen()
{
	return !m_d3dpp.Windowed;
}

BOOL D3D::isSmooth()
{
	return m_bSmoothDraw;
}

BOOL D3D::isAspectRatioPreserved()
{
	return m_bPreserveAspectRatio;
}

BOOL D3D::isScreenShotsActive()
{
	return (D3DXSaveSurfaceToFile != NULL);
}

D3DXIMAGE_FILEFORMAT D3D::getImageFileFormat()
{
	return m_saveFormat;
}
