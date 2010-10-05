#include "gfx_directx_cls_joystick.h"
using namespace gfx;

Joystick::Joystick() : m_hLibrary(NULL), m_hWnd(NULL)
{
	m_hLibrary = LoadLibrary(TEXT("dinput8.dll"));
}

Joystick::~Joystick()
{
	Shutdown();

	if(m_hLibrary)
		FreeLibrary(m_hLibrary);
	m_hLibrary = NULL;
}

HRESULT Joystick::Initialize(HINSTANCE hInstance, HWND hWnd)
{
	Shutdown();

	HRESULT hr = S_OK;
	hr = CoCreateInstance( __uuidof(diClsid)/*CLSID_DirectInput8*/, NULL, CLSCTX_INPROC_SERVER, __uuidof(diIid)/*IID_IDirectInput8*/, (void**)&m_dinput );
	if(FAILED(hr))
		return hr;

	hr = m_dinput->Initialize(hInstance, DIRECTINPUT_VERSION);
	if(FAILED(hr))
		return hr;

	m_hWnd = hWnd;

	RefreshEnumeration();
	return hr;
}

void Joystick::Shutdown()
{
	m_hWnd = NULL;
	m_devices.clear();
	m_dinput = NULL;
}

void Joystick::RefreshEnumeration()
{ //refresh and initialize attached devices (unless they are already attached & initialized; --placeholder--
}

BOOL Joystick::GetState(int &nDevice, int &buttons, int &xPos, int &yPos)
{
	if(nDevice > m_devices.size() || nDevice < 0)
		return FALSE;

	buttons = m_devices[nDevice].nButtons;
	xPos = m_devices[nDevice].xPos;
	yPos = m_devices[nDevice].yPos;
	return TRUE;
}

void Joystick::Poll()
{
	HRESULT hr = S_OK;
	for(UINT i = 0; i < m_devices.size(); i++)
	{
		hr = m_devices[i].pDevice->Poll();
		switch(hr)
		{
		case DIERR_NOTACQUIRED:
			m_devices[i].pDevice->Acquire();
			break;
		case DIERR_NOTINITIALIZED:
		case DIERR_INPUTLOST:
			RefreshEnumeration();
			return;
		default:
			//fill in state; --placeholder--
			m_devices[i].nButtons = 0;
		}
	}
}