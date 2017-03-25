//Input.h
//started 1/5/10
//manages input

#ifndef GFX_INPUT_H
#define GFX_INPUT_H

#include "SDL.h"

namespace gfx
{
	struct RECT
	{
		int left, top, right, bottom;
	};

	class Input
	{
	protected:
		Uint8 m_keyboardState[322];
		struct Mouse
		{
			int x;
			int y;
			int wheel;
			Uint8 buttonState[8]; //support for 8 buttons
		} m_mousePosition, m_mouseChange; //m_mouseChange is used for button state
		bool m_bClipped; //if true, mouse position is clipped to a specified range (not the os cursor)
		gfx::RECT m_rClip;
	public:
		Input();
		virtual ~Input();

		void Poll();
		void PollKeyboard(); //updates key state
		void PollMouse(); //updates mouse position and button state
		void PollJoystick(); //updates joystick position and button state

		Uint8* GetKeyboardState(); //returns 322 byte array of keyboard state
		bool IsKeyDown(int sdlCode); //returns true if sdlCode key is down
		bool IsKeyUp(int sdlCode); //returns true if sdlCode key is up

		int GetMouseX();
		int GetMouseY();
		int GetMouseWheel();
		void SetMouseX(int xPos);
		void SetMouseY(int yPos);
		void SetMouseWheel(int zPos);
		int GetMouseXChange(); //returns change in movement along x-axis
		int GetMouseYChange(); //returns change in movement along y-axis
		int GetMouseWheelChange(); //returns change in wheel movement
		Uint8* GetMouseButtonState(); //returns 8 byte array of mouse button state
		bool IsMouseLButtonDown(); //returns true if mouse left button is down
		bool IsMouseLButtonUp(); //returns true if mouse left button is up
		bool IsMouseRButtonDown(); //returns true if mouse right button is down
		bool IsMouseRButtonUp(); //returns true if mouse right button is up
		bool IsMouseMButtonDown(); //returns true if mouse middle button is down
		bool IsMouseMButtonUp(); //returns true if mouse middle button is up
		bool IsMouseXButtonDown(int n); //returns true if mouse 'n - 1' button is down
		bool IsMouseXButtonUp(int n); //returns true if mouse 'n - 1' button is up

		//restricts mouse.X and mouse.Y to the rectangle; an invalid rectangle disables clipping;
		//this does not affect the os mouse cursor
		void ClipMouseMovement(int left, int top, int right, int bottom); 
	};
}

#endif
