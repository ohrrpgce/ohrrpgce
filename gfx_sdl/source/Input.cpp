#include "Input.h"
using namespace gfx;

Input::Input() : m_bClipped(false)
{
	::memset((void*)m_keyboardState, 0, sizeof(m_keyboardState));
	::memset((void*)&m_mousePosition, 0, sizeof(m_mousePosition));
	::memset((void*)&m_mouseChange, 0, sizeof(m_mouseChange));
	RECT rTmp = {0,0,0,0};
	m_rClip = rTmp;
}

Input::~Input()
{
}

void Input::Poll()
{
	PollKeyboard();
	PollMouse();
	PollJoystick();
}

void Input::PollKeyboard()
{
	int nKeys = 0;
	Uint8 *pKeys = ::SDL_GetKeyState(&nKeys);
	for(int i = 0; i < nKeys && i < 322; i++)
		m_keyboardState[i] = pKeys[i];
}

void Input::PollMouse()
{
	Uint8 buttons = ::SDL_GetRelativeMouseState(&m_mouseChange.x, &m_mouseChange.y);
	for(int i = 0; i < 8; i++)
		m_mouseChange.buttonState[i] = buttons & (1 << i);

	m_mousePosition.x += m_mouseChange.x;
	m_mousePosition.y += m_mouseChange.y;
	//m_mousePosition.wheel += m_mouseChange.wheel; //implemented in sdl proc in gfx_sdl.cpp, not in this input module

	if(m_bClipped)
	{
		if(m_mousePosition.x < m_rClip.left) 
			m_mousePosition.x = m_rClip.left;
		else if(m_mousePosition.x > m_rClip.right) 
			m_mousePosition.x = m_rClip.right;
		if(m_mousePosition.y < m_rClip.top) 
			m_mousePosition.y = m_rClip.top;
		else if(m_mousePosition.y > m_rClip.bottom) 
			m_mousePosition.y = m_rClip.bottom;
	}
}

void Input::PollJoystick()
{
}

Uint8* Input::GetKeyboardState()
{
	return m_keyboardState;
}

bool Input::IsKeyDown(int sdlCode)
{
	return (m_keyboardState[sdlCode] != 0 ? true : false);
}

bool Input::IsKeyUp(int sdlCode)
{
	return !IsKeyDown(sdlCode);
}

int Input::GetMouseX()
{
	return m_mousePosition.x;
}

int Input::GetMouseY()
{
	return m_mousePosition.y;
}

int Input::GetMouseWheel()
{
	return m_mousePosition.wheel;
}

void Input::SetMouseX(int xPos)
{
	m_mousePosition.x = xPos;
}

void Input::SetMouseY(int yPos)
{
	m_mousePosition.y = yPos;
}

void Input::SetMouseWheel(int zPos)
{
	m_mousePosition.wheel = zPos;
}

int Input::GetMouseXChange()
{
	return m_mouseChange.x;
}

int Input::GetMouseYChange()
{
	return m_mouseChange.y;
}

int Input::GetMouseWheelChange()
{
	return m_mouseChange.wheel;
}

Uint8* Input::GetMouseButtonState()
{
	return m_mouseChange.buttonState;
}

bool Input::IsMouseLButtonDown()
{
	return (m_mouseChange.buttonState[SDL_BUTTON(SDL_BUTTON_LEFT)] != 0 ? true : false);
}

bool Input::IsMouseLButtonUp()
{
	return !IsMouseLButtonDown();
}

bool Input::IsMouseRButtonDown()
{
	return (m_mouseChange.buttonState[SDL_BUTTON(SDL_BUTTON_RIGHT)] != 0 ? true : false);
}

bool Input::IsMouseRButtonUp()
{
	return !IsMouseRButtonDown();
}

bool Input::IsMouseMButtonDown()
{
	return (m_mouseChange.buttonState[SDL_BUTTON(SDL_BUTTON_MIDDLE)] != 0 ? true : false);
}

bool Input::IsMouseMButtonUp()
{
	return !IsMouseMButtonDown();
}

bool Input::IsMouseXButtonDown(int n)
{
	if(n < 0 || n >= 8)
		return false;
	return (m_mouseChange.buttonState[SDL_BUTTON(n)] != 0 ? true : false);
}

bool Input::IsMouseXButtonUp(int n)
{
	return !IsMouseXButtonDown(n);
}

void Input::ClipMouseMovement(int left, int top, int right, int bottom)
{
	if(left >= right || top >= bottom)
	{
		m_bClipped = false;
		return;
	}
	m_bClipped = true;
	RECT rTmp = {left, top, right, bottom};
	m_rClip = rTmp;
}
