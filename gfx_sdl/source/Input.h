//Input.h
//started 1/5/10
//manages input

#ifndef INPUT_H
#define INPUT_H

#include "SDL.h"

namespace gfx
{
	enum InputMessage
	{
		IM_KEYDOWN,
		IM_KEYUP,
		IM_MOUSEMOVE,
		IM_MOUSECLICK,
		IM_JOYMOVE,
		IM_JOYCLICK,
	};

	class Input
	{
	protected:
	public:
		Input();
		virtual ~Input();

		void SendMessage(InputMessage msg, Uint32 dwParam, Uint32 dwParam2);

		void Poll();
		void PollKeyboard(); //updates key state
		void PollMouse(); //updates mouse position and button state
		void PollJoystick(); //updates joystick position and button state

		Uint8* GetKeyboardState(); //returns 256 byte array of keyboard state
		bool IsKeyDown(int sdlCode); //returns true if sdlCode key is down
		bool IsKeyUp(int sdlCode); //returns true if sdlCode key is up

		int GetMouseX();
		int GetMouseY();
		int GetMouseWheel();
		void SetMouseX(LONG xPos);
		void SetMouseY(LONG yPos);
		void SetMouseWheel(LONG zPos);
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