#include "gfx_directx_cls.h"
using namespace gfx;

DirectX::DirectX() 
: m_pWindow(NULL), m_bInitialized(false), /*m_bShowFps(false),*/ m_bVSync(true), m_bSmoothDraw(false),
  m_bPreserveAspectRatio(true), m_saveFormat(D3DXIFF_JPG),
  m_hD3d9(NULL), m_hD3dx9(NULL), m_bLibrariesLoaded(false)
{
	::ZeroMemory(&m_d3dpp, sizeof(m_d3dpp));
	Report(DX_NotInitialized);
	Direct3DCreate9_call = NULL;
	D3DXSaveSurfaceToFile_call = NULL;

	m_hD3d9 = ::LoadLibrary(TEXT("d3d9.dll"));
	if(m_hD3d9 == NULL)
		return;
	Direct3DCreate9_call = (D3D_CREATE_CALL)::GetProcAddress(m_hD3d9, "Direct3DCreate9");
	if(Direct3DCreate9_call == NULL)
	{
		::MessageBox(0, TEXT("Failed to load d3d9.dll"), TEXT("Error"), MB_OK | MB_ICONEXCLAMATION);
		return;
	}

	m_bLibrariesLoaded = true; //moved here because screenshots will always work, whether d3dx9_*.dll is found or not

	m_hD3dx9 = ::LoadLibrary(TEXT("d3dx9_24.dll")); //d3dx9_24.dll is the earliest dx runtime released as a dll
	if(m_hD3dx9 != NULL)
	{
#ifdef _UNICODE
		D3DXSaveSurfaceToFile_call = (D3DX_SAVESURFACE_CALL)::GetProcAddress(m_hD3dx9, "D3DXSaveSurfaceToFileW");
#else
		D3DXSaveSurfaceToFile_call = (D3DX_SAVESURFACE_CALL)::GetProcAddress(m_hD3dx9, "D3DXSaveSurfaceToFileA");
#endif
	}
}

DirectX::~DirectX()
{
	Shutdown();
	if(m_hD3dx9)
		::FreeLibrary(m_hD3dx9);
	if(m_hD3d9)
		::FreeLibrary(m_hD3d9);
}

DirectX_ErrorCode DirectX::Report(gfx::DirectX_ErrorCode error)
{
	m_lastErrorCode = error;
	switch(m_lastErrorCode)
	{
	case DX_OK:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_OK: No Errors"));
		} break;
	case DX_LibrariesMissing:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_LibrariesMissing: Either d3d9.dll or d3dx9.dll is missing from the system. Update the directx runtime!"));
		} break;
	case DX_NotInitialized:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_NotInitialized: The gfx::DirectX object was not properly initialized."));
		} break;
	case DX_Create_ParamsNotValid:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_Create_ParamsNotValid: The parameters sent to gfx::DirectX::Initialize() were not valid."));
		} break;
	case DX_Create_D3D:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_Create_D3D: An error occurred in creating the Direct3D object."));
		} break;
	case DX_Create_D3DDevice:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_Create_D3DDevice: An error occurred in creating the Direct3D device."));
		} break;
	case DX_Create_Fps:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_Create_Fps: An error occurred in creating the frames/second display."));
		} break;
	case DX_Create_Texture:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_Create_Texture: An error occurred in creating the texture that receives the ohr render."));
		} break;
	case DX_Create_Quad:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_Create_Quad: An error occurred in creating the quadrolateral that covers the screen with the ohr render."));
		} break;
	case DX_Palette_ParamNotValid:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_Palette_ParamNotValid: The parameter sent to gfx::DirectX::SetPalette() was not valid."));
		} break;
	case DX_ScreenShot_ParamNotValid:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_ScreenShot_ParamNotValid: The parameter sent to gfx::DirectX::ScreenShot() was not valid."));
		} break;
	case DX_ScreenShot_Surface:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_ScreenShot_Surface: An error occurred in allocating a surface to receive the front buffer's data."));
		} break;
	case DX_ScreenShot_GetFrontBuffer:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_ScreenShot_GetFrontBuffer: An error occurred in copying the front buffer contents to a surface."));
		} break;
	case DX_ScreenShot_Save:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_ScreenShot_Save: An error occurred in saving the screenshot."));
		} break;
	case DX_Resolution_Invalid:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_Resolution_Invalid: The resolution requested is invalid."));
		} break;
	case DX_ShowPage_Begin:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_ShowPage_Begin: The scene failed to begin on the device."));
		} break;
	case DX_ShowPage_Clear:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_ShowPage_Clear: The scene failed to clear on the device."));
		} break;
	case DX_ShowPage_GetBackBuffer:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_ShowPage_GetBackBuffer: The device failed to retrieve the back buffer."));
		} break;
	case DX_ShowPage_StretchRect:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_ShowPage_StretchRect: The device failed to stretch the image across the back buffer."));
		} break;
	case DX_ShowPage_End:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_ShowPage_End: The scene failed to end on the device."));
		} break;
	case DX_ShowPage_Present:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("DX_ShowPage_Present: The device failed to present the backbuffer."));
		} break;
	default:
		{
			::_tcscpy_s<256>(m_lastErrorMessage, TEXT("Unknown error!"));
		} break;
	}
	return m_lastErrorCode;
}

RECT DirectX::CalculateAspectRatio(UINT srcWidth, UINT srcHeight, UINT destWidth, UINT destHeight)
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

DirectX_ErrorCode DirectX::Initialize(gfx::Window *pWin, const TCHAR* szModuleName/*, bool b*/)
{
	if(!m_bLibrariesLoaded)
		return Report(DX_LibrariesMissing);
	if(!pWin)
		return Report(DX_Create_ParamsNotValid);
	Shutdown();
	if(szModuleName != NULL)
		m_szModuleName = szModuleName;
	else
		m_szModuleName = TEXT("");
	m_bInitialized = false;
	//m_bShowFps = b;

	m_pWindow = pWin;
	m_rWindowedMode = m_pWindow->GetWindowSize();
	m_rWindowedMode.right = m_pWindow->GetClientSize().cx;
	m_rWindowedMode.bottom = m_pWindow->GetClientSize().cy;
	RECT rTmp;
	::GetWindowRect(::GetDesktopWindow(), &rTmp);
	m_rFullscreenMode = rTmp;

	::ZeroMemory(&m_d3dpp, sizeof(m_d3dpp));
	m_d3dpp.BackBufferFormat		= D3DFMT_X8R8G8B8;
	m_d3dpp.BackBufferWidth			= m_rWindowedMode.right;
	m_d3dpp.BackBufferHeight		= m_rWindowedMode.bottom;
	m_d3dpp.hDeviceWindow			= m_pWindow->GetWindowHandle();
	if(!m_bVSync)
		m_d3dpp.PresentationInterval= D3DPRESENT_INTERVAL_IMMEDIATE;
	m_d3dpp.SwapEffect				= D3DSWAPEFFECT_DISCARD;
	m_d3dpp.Windowed				= true;

	m_d3d.Attach(Direct3DCreate9_call(D3D_SDK_VERSION));
	if(m_d3d == NULL)
		return Report(DX_Create_D3D);
	m_d3d->CreateDevice(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, m_pWindow->GetWindowHandle(), D3DCREATE_HARDWARE_VERTEXPROCESSING, &m_d3dpp, &m_d3ddev);
	if(m_d3ddev == NULL)
		return Report(DX_Create_D3DDevice);
	//m_caps.Test(m_d3ddev);

	//if(S_OK != m_fps.Initialize(m_pWindow->GetWindowHandle(), m_d3ddev))
	//	return Report(DX_Create_Fps);
	if(S_OK != m_surface.Initialize(m_d3ddev, 320, 200))
		return Report(DX_Create_Texture);
	//if(S_OK != m_quad.Initialize(m_d3ddev, m_surface.GetDimensions().cx, m_surface.GetDimensions().cy))
	//	return Report(DX_Create_Quad);
	//if(m_bPreserveAspectRatio)
	//	m_quad.AspectPadding(m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight);
	m_bInitialized = true;
	return Report(DX_OK);
}

DirectX_ErrorCode DirectX::Shutdown()
{
	Report(DX_NotInitialized);
	m_bInitialized = false;
	m_szModuleName.clear();
	//m_fps.Initialize(0,0);
	m_surface.Initialize(0,0,0);
	//m_quad.Initialize(0,0,0);
	m_d3ddev = NULL;
	m_d3d = NULL;
	return DX_OK;
}

DirectX_ErrorCode DirectX::ShowPage(unsigned char *pRawPage, UINT width, UINT height)
{
	if(!m_bInitialized)
		return DX_NotInitialized;
	HRESULT hrCoopLevel = m_d3ddev->TestCooperativeLevel();
	if(hrCoopLevel == D3DERR_DEVICELOST)
	{
		OnLostDevice();
		return DX_OK;
	}
	else if(hrCoopLevel == D3DERR_DEVICENOTRESET)
	{
		OnResetDevice();
		return DX_OK;
	}
	else if(hrCoopLevel == D3DERR_DRIVERINTERNALERROR)
	{
		if(IDYES == ::MessageBox(0, TEXT("Internal driver failure! Attempt to recover?"), TEXT("Critical Failure"), MB_ICONEXCLAMATION | MB_YESNO))
			return Shutdown();
		tstring szModule = m_szModuleName;
		Shutdown();
		return Initialize(m_pWindow, (szModule == TEXT("") ? NULL : szModule.c_str())/*, false*/);
	}
	m_surface.CopySystemPage(pRawPage, width, height, &m_palette);

	//m_fps.IncrementFrameCount();

	//Clear needs to happen outside of BeginScene()/EndScene() block
	TEST_HR(m_d3ddev->Clear(0, 0, D3DCLEAR_TARGET, 0xff000000, 1.0f, 0), DX_ShowPage_Clear);

	//StretchRect() needs to happen outside of BeginScene()/EndScene() block
	//RECT /*rTexCoords = {0},*/ rAspectRatio = {0};
	//rTexCoords.right = 320;
	//rTexCoords.bottom = 200;
	//rAspectRatio.right = m_d3dpp.BackBufferWidth;
	//rAspectRatio.bottom = m_d3dpp.BackBufferHeight;
	//m_quad.CalculateRect(&rAspectRatio);
	RECT rAspectRatio = {0};
	if(m_bPreserveAspectRatio)
		rAspectRatio = CalculateAspectRatio(m_surface.GetDimensions().cx, m_surface.GetDimensions().cy, m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight);
	SmartPtr<IDirect3DSurface9> pBackBuffer;
	TEST_HR(m_d3ddev->GetBackBuffer(0, 0, D3DBACKBUFFER_TYPE_MONO, &pBackBuffer), DX_ShowPage_GetBackBuffer);
	TEST_HR(m_d3ddev->StretchRect(m_surface.GetSurface(), /*&rTexCoords*/0, pBackBuffer, (m_bPreserveAspectRatio ? &rAspectRatio : NULL), (m_bSmoothDraw ? D3DTEXF_LINEAR : D3DTEXF_POINT)), DX_ShowPage_StretchRect);

	//if(m_bShowFps)
	//{//this is the only object that requires BeginScene()/EndScene()
	//	TEST_HR(m_d3ddev->BeginScene(), DX_ShowPage_Begin);
	//	m_fps.DrawFps();
	//	TEST_HR(m_d3ddev->EndScene(), DX_ShowPage_End);
	//}
	TEST_HR(m_d3ddev->Present(0,0,0,0), DX_ShowPage_Present);
	return DX_OK;
}

DirectX_ErrorCode DirectX::SetPalette(gfx::Palette<UINT> *pPalette)
{
	if(pPalette == NULL)
		return Report(DX_Palette_ParamNotValid);
	m_palette = *pPalette;
	return DX_OK;
}

DirectX_ErrorCode DirectX::ScreenShot(TCHAR *strName)
{
	if(!m_bInitialized)
		return DX_NotInitialized;
	if(D3DXSaveSurfaceToFile_call == NULL)
		return DX_LibrariesMissing;
	if(strName == NULL)
		return Report(DX_ScreenShot_ParamNotValid);
	if(m_saveFormat == D3DXIFF_FORCE_DWORD)
		return Report(DX_ScreenShot_Surface);
	SmartPtr<IDirect3DSurface9> pSurface;
	if(m_d3dpp.Windowed)
	{//have to use desktop size
		HWND hDesktop = ::GetDesktopWindow();
		RECT rDesktop;
		::GetWindowRect(hDesktop, &rDesktop);
		TEST_HR(m_d3ddev->CreateOffscreenPlainSurface(rDesktop.right, rDesktop.bottom, D3DFMT_A8R8G8B8, D3DPOOL_SCRATCH, &pSurface, 0), DX_ScreenShot_Surface);
	}
	else
		TEST_HR(m_d3ddev->CreateOffscreenPlainSurface(m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight, D3DFMT_A8R8G8B8, D3DPOOL_SCRATCH, &pSurface, 0), DX_ScreenShot_Surface);
	if(pSurface == NULL)
		return Report(DX_ScreenShot_Surface);
	TEST_HR(m_d3ddev->GetFrontBufferData(0, pSurface), DX_ScreenShot_GetFrontBuffer);

	RECT rImage = {0,0, m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight};
	if(m_bPreserveAspectRatio)
		rImage = CalculateAspectRatio(m_surface.GetDimensions().cx, m_surface.GetDimensions().cy, m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight);
	if(m_d3dpp.Windowed)
	{
		POINT rpTopLeft = {0,0};
		//POINT rpBottomRight = {m_pWindow->GetClientSize().cx, m_pWindow->GetClientSize().cy};
		::ClientToScreen(m_pWindow->GetWindowHandle(), &rpTopLeft);
		//::ClientToScreen(m_pWindow->GetWindowHandle(), &rpBottomRight);
		//RECT rCapture = {rpTopLeft.x, rpTopLeft.y, rpBottomRight.x, rpBottomRight.y};
		rImage.left += rpTopLeft.x;
		rImage.right += rpTopLeft.x;
		rImage.top += rpTopLeft.y;
		rImage.bottom += rpTopLeft.y;
		//if(D3D_OK != ::D3DXSaveSurfaceToFile(strName, m_saveFormat, pSurface, 0, &rImage))
		//	return Report(DX_ScreenShot_Save);
	}
	//else
	//{
	//	//RECT rCapture = m_pWindow->GetWindowSize();
	//	//if(m_bPreserveAspectRatio)
	//	//	m_quad.CalculateRect(&rCapture);
	//}
	if(D3D_OK != D3DXSaveSurfaceToFile_call(strName, m_saveFormat, pSurface, 0, &rImage))
		return Report(DX_ScreenShot_Save);
	return DX_OK;
}

DirectX_ErrorCode DirectX::SetView(bool bWindowed)
{
	if(bWindowed == (TRUE == m_d3dpp.Windowed))
		return DX_OK;
	m_d3dpp.Windowed = bWindowed;
	if(m_d3dpp.Windowed)
	{
		m_d3dpp.BackBufferWidth		= m_rWindowedMode.right;
		m_d3dpp.BackBufferHeight	= m_rWindowedMode.bottom;
	}
	else
	{
		m_d3dpp.BackBufferWidth		= m_rFullscreenMode.right;
		m_d3dpp.BackBufferHeight	= m_rFullscreenMode.bottom;
		m_pWindow->SetWindowSize(m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight);
		m_pWindow->SetWindowPosition(0,0);
	}
	if(!m_bInitialized)
		return DX_NotInitialized;
	//if(m_bPreserveAspectRatio)
	//	m_quad.AspectPadding(m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight);
	if(m_d3dpp.Windowed)
	{
		m_pWindow->SetClientSize(m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight);
		m_pWindow->CenterWindow();
	}
	else
	{
		OnLostDevice();
		OnResetDevice();
	}
	return DX_OK;
}

DirectX_ErrorCode DirectX::SetResolution(UINT width, UINT height)
{
	if(m_d3dpp.Windowed == TRUE)
	{
		m_rWindowedMode.right = width;
		m_rWindowedMode.bottom = height;
		m_d3dpp.BackBufferWidth		= m_rWindowedMode.right;
		m_d3dpp.BackBufferHeight	= m_rWindowedMode.bottom;
		//intentionally not setting these--otherwise will enter recursive pit of despair, abandon all hope!
		//m_pWindow->SetClientSize(m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight);
		//m_pWindow->CenterWindow();
		//if(m_bPreserveAspectRatio)
		//	m_quad.AspectPadding(m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight);
		OnLostDevice();
		OnResetDevice();
	}
	return DX_OK;
}

void DirectX::SetVSync(bool bEnableVSync)
{
	if(bEnableVSync == m_bVSync)
		return;
	m_bVSync = bEnableVSync;
	if(m_bVSync)
	{
		m_d3dpp.PresentationInterval = 0;
	}
	else
	{
		m_d3dpp.PresentationInterval = D3DPRESENT_INTERVAL_IMMEDIATE;
	}
	OnLostDevice();
	OnResetDevice();
}

//void DirectX::SetFps(bool bEnableFpsDisplay)
//{
//	m_bShowFps = bEnableFpsDisplay;
//}

DirectX_ErrorCode DirectX::SetSmooth(bool bSmoothDraw)
{
	m_bSmoothDraw = bSmoothDraw;
	return DX_OK;
}

void DirectX::SetAspectRatioPreservation(bool bPreserve)
{
	m_bPreserveAspectRatio = bPreserve;
	//if(m_bPreserveAspectRatio)
	//	m_quad.AspectPadding(m_d3dpp.BackBufferWidth, m_d3dpp.BackBufferHeight);
	//else
	//	m_quad.AspectPadding(320, 200);
}

void DirectX::SetImageFileFormat(D3DXIMAGE_FILEFORMAT format)
{
	m_saveFormat = format;
}

RECT DirectX::GetResolution()
{
	if(m_d3dpp.Windowed)
		return m_rWindowedMode;
	return m_rFullscreenMode;
}

Palette<UINT> DirectX::GetPalette()
{
	return m_palette;
}

bool DirectX::IsVsyncEnabled()
{
	return m_bVSync;
}

//bool DirectX::IsFpsEnabled()
//{
//	return m_bShowFps;
//}

bool DirectX::IsViewFullscreen()
{
	return !m_d3dpp.Windowed;
}

bool DirectX::IsSmooth()
{
	return m_bSmoothDraw;
}

bool DirectX::IsAspectRatioPreserved()
{
	return m_bPreserveAspectRatio;
}

bool DirectX::IsScreenShotsActive()
{
	return (D3DXSaveSurfaceToFile_call != NULL);
}

D3DXIMAGE_FILEFORMAT DirectX::GetImageFileFormat()
{
	return m_saveFormat;
}

DirectX_ErrorCode DirectX::GetLastErrorCode()
{
	return m_lastErrorCode;
}

TCHAR* DirectX::GetLastErrorMessage()
{
	return m_lastErrorMessage;
}

//const TCHAR* DirectX::GetCapsMessage()
//{
//	return m_caps.GetOutput();
//}

void DirectX::OnLostDevice()
{
	if(!m_bInitialized)
		return;
	//m_fps.OnLostDevice();
	//m_quad.OnLostDevice();
	m_surface.OnLostDevice();
}

void DirectX::OnResetDevice()
{
	if(!m_bInitialized)
		return;
	m_d3ddev->Reset(&m_d3dpp);
	m_surface.OnResetDevice();
	//m_quad.OnResetDevice();
	//m_fps.OnResetDevice();
}
