//gfx_directx_cls_dinput.h
//by Jay Tennant 11/2/09
//manages general direct input

#ifndef GFX_DIRECTX_CLS_DINPUT_H
#define GFX_DIRECTX_CLS_DINPUT_H

#include "gfx_directx_cls_window.h"
#include <dinput.h>
#include "smartptr.h"

namespace gfx
{
	class DirectInput
	{
	protected:
		SmartPtr<IDirectInput8> m_dinput;
		SmartPtr<IDirectInputDevice8> m_keyboard;
		SmartPtr<IDirectInputDevice8> m_mouse;
		SmartPtr<IDirectInputDevice8> m_joystick;
		BYTE m_keyboardState[256];
		DIMOUSESTATE2 m_mouseState; //updates change in values to itself
		DIMOUSESTATE2 m_mouseStateChange; //change in values
		bool m_bInitialized;
		RECT m_rClip;
		bool m_bClipping;
	public:
		DirectInput();
		virtual ~DirectInput();

		int Initialize(Window* pWindow);
		void Shutdown();
		void Poll();
		void PollKeyboard();
		void PollMouse();
		void PollJoystick();

		BYTE* GetKeyboardState(); //returns 256 byte array of keyboard state
		bool IsKeyDown(int vkCode); //returns true if vkCode key is down
		bool IsKeyUp(int vkCode); //returns true if vkCode key is up

		LONG GetMouseX();
		LONG GetMouseY();
		LONG GetMouseWheel();
		void SetMouseX(LONG xPos);
		void SetMouseY(LONG yPos);
		void SetMouseWheel(LONG zPos);
		LONG GetMouseXChange(); //returns change in movement along x-axis
		LONG GetMouseYChange(); //returns change in movement along y-axis
		LONG GetMouseWheelChange(); //returns change in wheel movement
		BYTE* GetMouseButtonState(); //returns 8 byte array of mouse button state
		bool IsMouseLButtonDown(); //returns true if mouse left button is down
		bool IsMouseLButtonUp(); //returns true if mouse left button is up
		bool IsMouseRButtonDown(); //returns true if mouse right button is down
		bool IsMouseRButtonUp(); //returns true if mouse right button is up
		bool IsMouseMButtonDown(); //returns true if mouse middle button is down
		bool IsMouseMButtonUp(); //returns true if mouse middle button is up
		bool IsMouseXButtonDown(int n); //returns true if mouse 'n - 1' button is down
		bool IsMouseXButtonUp(int n); //returns true if mouse 'n - 1' button is up

		void ClipMouseMovement(int left, int top, int right, int bottom); //restricts mouse.X and mouse.Y to the rectangle; an invalid rectangle disables clipping
		void ClipMouseMovement(RECT* r);
		RECT GetMouseClip(); //returns the clipped rectangle the mouse is restricted to
	};
}

#endif