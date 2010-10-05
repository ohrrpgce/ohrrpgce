//gfx_directx_cls_joystick.h
//by Jay Tennant 10/5/10
//manages joystick input through directinput

#ifndef GFX_DIRECTX_CLS_JOYSTICK_H
#define GFX_DIRECTX_CLS_JOYSTICK_H

#include <windows.h>
#define DIRECTINPUT_VERSION 0x0800
#include <dinput.h>
#include "smartptr.h"
#include <vector>

namespace gfx
{
	class Joystick
	{
	protected:
		struct DECLSPEC_UUID("{25E609E4-B259-11CF-BFC7-444553540000}") diClsid;
#ifdef _UNICODE
		struct DECLSPEC_UUID("{BF798031-483A-4DA2-AA99-5D64ED369700}") diIid;
#else
		struct DECLSPEC_UUID("{BF798030-483A-4DA2-AA99-5D64ED369700}") diIid;
#endif
		struct Device
		{ //need to add other pertinent data
			Device() : nButtons(0), xPos(0), yPos(0) {}
			~Device() {pDevice = NULL;}
			SmartPtr<IDirectInputDevice8> pDevice;
			int nButtons;
			int xPos;
			int yPos;
		};
	protected:
		HMODULE m_hLibrary;
		HWND m_hWnd;

		SmartPtr<IDirectInput8> m_dinput;
		std::vector<Device> m_devices;
	public:
		Joystick();
		~Joystick();

		HRESULT Initialize(HINSTANCE hInstance, HWND hWnd);
		void Shutdown();

		void RefreshEnumeration(); //refreshes the device list
		BOOL GetState(int& nDevice, int& buttons, int& xPos, int& yPos);
		void Poll();
	};
}

#endif