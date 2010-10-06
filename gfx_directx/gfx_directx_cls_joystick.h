//gfx_directx_cls_joystick.h
//by Jay Tennant 10/5/10
//manages joystick input through directinput

#ifndef GFX_DIRECTX_CLS_JOYSTICK_H
#define GFX_DIRECTX_CLS_JOYSTICK_H

#include <windows.h>
#define DIRECTINPUT_VERSION 0x0800
#include <dinput.h>
#include "smartptr.h"
#include <list>

namespace gfx
{
	class Joystick
	{
	protected:
		struct Device
		{ //need to add other pertinent data
			Device() : nButtons(0), xPos(0), yPos(0), bNewDevice(true) {}
			~Device() {pDevice = NULL;}
			SmartPtr<IDirectInputDevice8> pDevice;
			DIDEVICEINSTANCE info;
			int nButtons;
			int xPos;
			int yPos;
			bool bNewDevice;
		};

		static BOOL EnumDevices(LPCDIDEVICEINSTANCE lpddi, LPVOID pvRef);
		static BOOL EnumDeviceObjects(LPCDIDEVICEOBJECTINSTANCE lpddoi, LPVOID pvRef);
	protected:
		HMODULE m_hLibrary;
		HWND m_hWnd;

		SmartPtr<IDirectInput8> m_dinput;
		std::list<Device> m_devices;

		void FilterAttachedDevices(); //cleans list so only attached devices are in list
		void ConfigNewDevices(); //sets data format, and initial button mappings for new devices
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