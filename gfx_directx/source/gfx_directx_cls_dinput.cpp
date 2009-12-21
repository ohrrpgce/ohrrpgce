#include "gfx_directx_cls_dinput.h"
using namespace gfx;

DirectInput::DirectInput() : m_bInitialized(false), m_bClipping(false)
{
	::ZeroMemory(&m_mouseState, sizeof(m_mouseState));
	::ZeroMemory(&m_mouseStateChange, sizeof(m_mouseStateChange));
	::ZeroMemory(&m_rClip, sizeof(m_rClip));
	::ZeroMemory(m_keyboardState, sizeof(m_keyboardState));
}

DirectInput::~DirectInput()
{
	Shutdown();
}

int DirectInput::Initialize(gfx::Window *pWindow)
{
	Shutdown();
	if(pWindow == NULL)
		return -1;

	::DirectInput8Create(pWindow->GetAppHandle(), DIRECTINPUT_VERSION, IID_IDirectInput8, (void**)&m_dinput, NULL);
	if(m_dinput == NULL)
		return -1;
	m_dinput->CreateDevice(GUID_SysKeyboard, &m_keyboard, 0);
	if(m_keyboard == NULL)
		return -1;
	m_keyboard->SetDataFormat(&c_dfDIKeyboard);
	m_keyboard->SetCooperativeLevel(pWindow->GetWindowHandle(), DISCL_FOREGROUND | DISCL_NONEXCLUSIVE);

	m_dinput->CreateDevice(GUID_SysMouse, &m_mouse, 0);
	if(m_mouse == NULL)
		return -1;
	m_mouse->SetDataFormat(&c_dfDIMouse2);
	m_mouse->SetCooperativeLevel(pWindow->GetWindowHandle(), DISCL_FOREGROUND | DISCL_NONEXCLUSIVE);

	m_keyboard->Acquire();
	m_mouse->Acquire();

	m_bInitialized = true;
	return 0;
}

void DirectInput::Shutdown()
{
	m_bInitialized = false;
	::ZeroMemory(&m_mouseState, sizeof(m_mouseState));
	::ZeroMemory(&m_mouseStateChange, sizeof(m_mouseStateChange));
	::ZeroMemory(m_keyboardState, sizeof(m_keyboardState));
	if(m_joystick != NULL)
		m_joystick->Unacquire();
	if(m_mouse != NULL)
		m_mouse->Unacquire();
	if(m_keyboard != NULL)
		m_keyboard->Unacquire();
	m_joystick = NULL;
	m_mouse = NULL;
	m_keyboard = NULL;
	m_dinput = NULL;
}

void DirectInput::Poll()
{
	PollKeyboard();
	PollMouse();
	PollJoystick();
}

void DirectInput::PollKeyboard()
{
	if(!m_bInitialized)
		return;
	if(FAILED(m_keyboard->GetDeviceState(sizeof(m_keyboardState), (void**)&m_keyboardState)))
	{
		::ZeroMemory(&m_keyboardState, sizeof(m_keyboardState));
		m_keyboard->Acquire();
	}
}

void DirectInput::PollMouse()
{
	if(!m_bInitialized)
		return;
	if(FAILED(m_mouse->GetDeviceState(sizeof(m_mouseStateChange), (void**)&m_mouseStateChange)))
	{
		::ZeroMemory(&m_mouseStateChange, sizeof(m_mouseStateChange));
		m_mouse->Acquire();
	}

	m_mouseState.lX += m_mouseStateChange.lX;
	m_mouseState.lY += m_mouseStateChange.lY;
	m_mouseState.lZ += m_mouseStateChange.lZ;
	//buttons are not updated; m_mouseStateChange is referenced directly

	if(m_bClipping)
	{
		if(m_mouseState.lX < m_rClip.left)
			m_mouseState.lX = m_rClip.left;
		else if(m_mouseState.lX > m_rClip.right)
			m_mouseState.lX = m_rClip.right;
		if(m_mouseState.lY < m_rClip.top)
			m_mouseState.lY = m_rClip.top;
		else if(m_mouseState.lY > m_rClip.bottom)
			m_mouseState.lY = m_rClip.bottom;
	}
}

void DirectInput::PollJoystick()
{
	if(!m_bInitialized)
		return;
}

BYTE* DirectInput::GetKeyboardState()
{
	return m_keyboardState;
}

bool DirectInput::IsKeyDown(int vkCode)
{
	if(vkCode > 255)
		return false;
	return (0 != (m_keyboardState[vkCode] & 0x80));
}

bool DirectInput::IsKeyUp(int vkCode)
{//by default, any key value above 255 is considered being "up"
	return !IsKeyDown(vkCode);
}

LONG DirectInput::GetMouseX()
{
	return m_mouseState.lX;
}

LONG DirectInput::GetMouseY()
{
	return m_mouseState.lY;
}

LONG DirectInput::GetMouseWheel()
{
	return m_mouseState.lZ;
}

void DirectInput::SetMouseX(LONG xPos)
{
	m_mouseState.lX = xPos;
}

void DirectInput::SetMouseY(LONG yPos)
{
	m_mouseState.lY = yPos;
}

void DirectInput::SetMouseWheel(LONG zPos)
{
	m_mouseState.lZ = zPos;
}

LONG DirectInput::GetMouseXChange()
{
	return m_mouseStateChange.lX;
}

LONG DirectInput::GetMouseYChange()
{
	return m_mouseStateChange.lY;
}

LONG DirectInput::GetMouseWheelChange()
{
	return m_mouseStateChange.lZ;
}

BYTE* DirectInput::GetMouseButtonState()
{//intentionally accessing this one's button states as it updates every poll
	return m_mouseStateChange.rgbButtons;
}

bool DirectInput::IsMouseLButtonDown()
{
	return (0 != (m_mouseStateChange.rgbButtons[0] & 0x80));
}

bool DirectInput::IsMouseLButtonUp()
{
	return !IsMouseLButtonDown();
}

bool DirectInput::IsMouseRButtonDown()
{
	return (0 != (m_mouseStateChange.rgbButtons[1] & 0x80));
}

bool DirectInput::IsMouseRButtonUp()
{
	return !IsMouseRButtonDown();
}

bool DirectInput::IsMouseMButtonDown()
{
	return (0 != (m_mouseStateChange.rgbButtons[2] & 0x80));
}

bool DirectInput::IsMouseMButtonUp()
{
	return !IsMouseMButtonDown();
}

bool DirectInput::IsMouseXButtonDown(int n)
{//n - 1
	if(n > 8 || n < 1)
		return false;
	return (0 != (m_mouseStateChange.rgbButtons[n - 1] & 0x80));
}

bool DirectInput::IsMouseXButtonUp(int n)
{
	return !IsMouseXButtonDown(n);
}

void DirectInput::ClipMouseMovement(int left, int top, int right, int bottom)
{
	RECT r = {left, top, right, bottom};
	ClipMouseMovement(&r);
}

void DirectInput::ClipMouseMovement(RECT *r)
{
	if(!r)
	{
		m_bClipping = false;
		return;
	}
	else if(r->left >= r->right || r->top >= r->bottom)
	{
		m_bClipping = false;
		return;
	}
	m_rClip = *r;
	m_bClipping = true;
}

RECT DirectInput::GetMouseClip()
{
	return m_rClip;
}