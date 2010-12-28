//BackendDebugger.h
//by Jay Tennant 12/12/10
//defines backend debugger as an accessor

#pragma once

#include <windows.h>
#include "gfx_directx_cls_window.h"
#include "gfx_directx_cls_d3d.h"
#include "gfx_directx_cls_keyboard.h"
#include "gfx_directx_cls_mouse.h"
#include "gfx_directx_cls_joystick.h"
#include "defptr.h"

class IAppHook
{
public:
	enum StatusCode
	{
		SC_OK = 0,
		SC_INACTIVE = 1,
		SC_ERROR = 2,
	};
	virtual void SendDebugString(LPCSTR szMessage) {}
	virtual void HookWindowMsg(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {}
	virtual UINT CreateStatusListener() {return 0;}
	virtual void DestroyStatusListener(UINT id) {}
	virtual void SendStatus(UINT id, StatusCode sc, const char* szErrMsg) {}
};

//there isn't any reference counting on this object, and the interface is
//implemented already, unlike most interface dev--it's just not practical here
class IBackend
{
protected:
	DefPtr<IAppHook> m_hook;
public:
	//interface information
	virtual UINT GetVersion() const {return 1;} //returns the current version; any interface at or below this number may be successfully queried
	virtual HRESULT QueryVersion(UINT version, void** pInterface) //queries for a version of the interface available
	{
		if(::IsBadWritePtr(pInterface, sizeof(void*)))
			return E_POINTER;
		if(version > 1 || version == 0)
			return E_NOINTERFACE;
		
		*pInterface = (void*)this;
		return S_OK;
	}

	//app interfacing
	virtual HRESULT SendHook( IAppHook* pHook )
	{
		m_hook = pHook;
		return S_OK;
	}
};