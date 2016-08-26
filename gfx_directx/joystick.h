//joystick.h
//by Jay Tennant 10/5/10; updated 4/21/11
//manages joystick input through directinput

#pragma once

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
			Device() : nButtons(0), xPos(0), yPos(0), bNewDevice(true), bRefreshed(true) {}
			~Device() {pDevice = NULL;}
			SmartPtr<IDirectInputDevice8> pDevice;
			DIDEVICEINSTANCE info;
			int nButtons;
			int xPos;
			int yPos;
			bool bNewDevice;
			bool bRefreshed;
		};

		static BOOL __stdcall EnumDevices(LPCDIDEVICEINSTANCE lpddi, LPVOID pvRef);
		static BOOL __stdcall EnumDeviceObjects(LPCDIDEVICEOBJECTINSTANCE lpddoi, LPVOID pvRef);
	protected:
		HWND m_hWnd;
		BOOL m_bRefreshRequest;

		SmartPtr<IDirectInput8> m_dinput;
		std::list<Device> m_devices;

		void filterAttachedDevices(); //cleans list so only attached devices are in list
		void configNewDevices(); //sets data format, and initial button mappings for new devices
	public:
		Joystick();
		~Joystick();

		HRESULT initialize(HINSTANCE hInstance, HWND hWnd);
		void shutdown();

		void refreshEnumeration(); //refreshes the device list
		void delayedRefreshEnumeration() { m_bRefreshRequest = TRUE; }
		UINT getJoystickCount();
		BOOL getState(int& nDevice, int& buttons, int& xPos, int& yPos);
		void poll();
	};
}
