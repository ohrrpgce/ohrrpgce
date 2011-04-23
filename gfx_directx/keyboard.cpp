#include "keyboard.h"
#include "..\\scancodes.h"
using namespace gfx;

#define VK_NUMPAD_ENTER 0xCA //reserved in windows, but recommended on a site

Keyboard::Keyboard() : m_scLShift(0)
{
	ZeroMemory(m_scancodes, sizeof(m_scancodes));
	ZeroMemory(m_virtualKeys, sizeof(m_virtualKeys));
	m_scLShift = MapVirtualKey(VK_SHIFT, MAPVK_VK_TO_VSC);
}

//void Keyboard::Poll()
//{
//	::GetKeyboardState(m_virtualKeys);
//	for(UINT i = 0; i < 256; i++)
//		m_scancodes[ c_vk2fb[i] ] = (m_virtualKeys[i] & 0x80) ? 0x8 : 0x0;
//	m_scancodes[ c_vk2fb[VK_CAPITAL] ] = (m_virtualKeys[VK_CAPITAL] & 0x1) ? 0x8 : 0x0;
//	m_scancodes[ c_vk2fb[VK_NUMLOCK] ] = (m_virtualKeys[VK_NUMLOCK] & 0x1) ? 0x8 : 0x0;
//	m_scancodes[ c_vk2fb[VK_SCROLL] ] = (m_virtualKeys[VK_SCROLL] & 0x1) ? 0x8 : 0x0;
//}

bool Keyboard::processMessage(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	switch(msg)
	{
	case WM_SYSKEYDOWN:
	case WM_KEYDOWN:
		{
			if(wParam == VK_NUMLOCK || wParam == VK_SCROLL || wParam == VK_CAPITAL)
			{
				break;
			}
			else if(wParam == VK_SHIFT)
			{//to distinguish between left and right
				if(m_scLShift == (HIWORD(lParam) & 0xff))
				{
					m_virtualKeys[VK_LSHIFT] = 0x80;
					m_scancodes[ c_vk2fb[VK_LSHIFT] ] /*|= 0x3;*/  = KB_CREATE_KEYPRESS();
				}
				else
				{
					m_virtualKeys[VK_RSHIFT] = 0x80;
					m_scancodes[ c_vk2fb[VK_RSHIFT] ] /*|= 0x3;*/  = KB_CREATE_KEYPRESS();
				}
				if(m_virtualKeys[VK_LSHIFT] ^ m_virtualKeys[VK_RSHIFT])
				{
					m_virtualKeys[VK_SHIFT] = 0x80;
					m_scancodes[ c_vk2fb[VK_SHIFT] ] /*|= 0x3;*/  = KB_CREATE_KEYPRESS();
				}
			}
			else if(wParam == VK_CONTROL)
			{//to distinguish between left and right
				if(HIWORD(lParam) & 0x100) //extended key, right control
				{
					m_virtualKeys[VK_RCONTROL] = 0x80;
					m_scancodes[ c_vk2fb[VK_RCONTROL] ] /*|= 0x3;*/  = KB_CREATE_KEYPRESS();
				}
				else
				{
					m_virtualKeys[VK_LCONTROL] = 0x80;
					m_scancodes[ c_vk2fb[VK_LCONTROL] ] /*|= 0x3;*/  = KB_CREATE_KEYPRESS();
				}
				if(m_virtualKeys[VK_LCONTROL] ^ m_virtualKeys[VK_RCONTROL])
				{
					m_virtualKeys[VK_CONTROL] = 0x80;
					m_scancodes[ c_vk2fb[VK_CONTROL] ] /*|= 0x3;*/  = KB_CREATE_KEYPRESS();
				}
			}
			else if(wParam == VK_MENU)
			{//to distinguish between left and right
				if(HIWORD(lParam) & 0x100) //extended key, right alt
				{
					m_virtualKeys[VK_RMENU] = 0x80;
					m_scancodes[ c_vk2fb[VK_RMENU] ] /*|= 0x3;*/  = KB_CREATE_KEYPRESS();
				}
				else
				{
					m_virtualKeys[VK_LMENU] = 0x80;
					m_scancodes[ c_vk2fb[VK_LMENU] ] /*|= 0x3;*/  = KB_CREATE_KEYPRESS();
				}
				if(m_virtualKeys[VK_LMENU] ^ m_virtualKeys[VK_RMENU])
				{
					m_virtualKeys[VK_MENU] = 0x80;
					m_scancodes[ c_vk2fb[VK_MENU] ] /*|= 0x3;*/  = KB_CREATE_KEYPRESS();
				}
			}
			else if(wParam == VK_RETURN)
			{
				if(HIWORD(lParam) & 0x100) //extended key, numpad enter
				{
					m_virtualKeys[VK_NUMPAD_ENTER] == 0x80;
					m_scancodes[ c_vk2fb[VK_NUMPAD_ENTER] ] = KB_CREATE_KEYPRESS();
				}
				else
				{
					m_virtualKeys[VK_RETURN] == 0x80;
					m_scancodes[ c_vk2fb[VK_RETURN] ] = KB_CREATE_KEYPRESS();
				}
			}
			else
			{
				m_virtualKeys[wParam] = 0x80;
				m_scancodes[ c_vk2fb[wParam] ] /*|= 0x3;*/  = KB_CREATE_KEYPRESS();
			}
		} break;
	case WM_SYSKEYUP:
	case WM_KEYUP:
		{
			if(wParam == VK_NUMLOCK || wParam == VK_SCROLL || wParam == VK_CAPITAL)
			{
				if(m_virtualKeys[wParam] == 0x80)
				{
					m_virtualKeys[wParam] = 0x0;
					m_scancodes[ c_vk2fb[wParam] ] /*&= 0x2;*/  = KB_CREATE_KEYRELEASE();
				}
				else
				{
					m_virtualKeys[wParam] = 0x80;
					m_scancodes[ c_vk2fb[wParam] ] /*|= 0x3;*/  = KB_CREATE_KEYPRESS();
				}
			}
			else if(wParam == VK_SHIFT)
			{//to distinguish between left and right
				if(m_scLShift == (HIWORD(lParam) & 0xff))
				{
					m_virtualKeys[VK_LSHIFT] = 0x0;
					m_scancodes[ c_vk2fb[VK_LSHIFT] ] /*&= 0x2;*/  = KB_CREATE_KEYRELEASE();
				}
				else
				{
					m_virtualKeys[VK_RSHIFT] = 0x0;
					m_scancodes[ c_vk2fb[VK_RSHIFT] ] /*&= 0x2;*/  = KB_CREATE_KEYRELEASE();
				}
				if(!(m_virtualKeys[VK_LSHIFT] || m_virtualKeys[VK_RSHIFT]))
				{
					m_virtualKeys[VK_SHIFT] = 0x0;
					m_scancodes[ c_vk2fb[VK_SHIFT] ] /*&= 0x2;*/  = KB_CREATE_KEYRELEASE();
				}
			}
			else if(wParam == VK_CONTROL)
			{//to distinguish between left and right
				if(HIWORD(lParam) & 0x100) //extended key, right control
				{
					m_virtualKeys[VK_RCONTROL] = 0x0;
					m_scancodes[ c_vk2fb[VK_RCONTROL] ] /*&= 0x2;*/  = KB_CREATE_KEYRELEASE();
				}
				else
				{
					m_virtualKeys[VK_LCONTROL] = 0x0;
					m_scancodes[ c_vk2fb[VK_LCONTROL] ] /*&= 0x2;*/  = KB_CREATE_KEYRELEASE();
				}
				if(!(m_virtualKeys[VK_LCONTROL] || m_virtualKeys[VK_RCONTROL]))
				{
					m_virtualKeys[VK_CONTROL] = 0x0;
					m_scancodes[ c_vk2fb[VK_CONTROL] ] /*&= 0x2;*/  = KB_CREATE_KEYRELEASE();
				}
			}
			else if(wParam == VK_MENU)
			{//to distinguish between left and right
				if(HIWORD(lParam) & 0x100) //extended key, right alt
				{
					m_virtualKeys[VK_RMENU] = 0x0;
					m_scancodes[ c_vk2fb[VK_RMENU] ] /*&= 0x2;*/  = KB_CREATE_KEYRELEASE();
				}
				else
				{
					m_virtualKeys[VK_LMENU] = 0x0;
					m_scancodes[ c_vk2fb[VK_LMENU] ] /*&= 0x2;*/  = KB_CREATE_KEYRELEASE();
				}
				if(!(m_virtualKeys[VK_LMENU] || m_virtualKeys[VK_RMENU]))
				{
					m_virtualKeys[VK_MENU] = 0x0;
					m_scancodes[ c_vk2fb[VK_MENU] ] /*&= 0x2;*/  = KB_CREATE_KEYRELEASE();
				}
			}
			else if(wParam == VK_RETURN)
			{
				if(HIWORD(lParam) & 0x100) //extended key, numpad enter
				{
					m_virtualKeys[VK_NUMPAD_ENTER] == 0x0;
					m_scancodes[ c_vk2fb[VK_NUMPAD_ENTER] ] = KB_CREATE_KEYRELEASE();
				}
				else
				{
					m_virtualKeys[VK_RETURN] == 0x0;
					m_scancodes[ c_vk2fb[VK_RETURN] ] = KB_CREATE_KEYRELEASE();
				}
			}
			else
			{
				m_virtualKeys[wParam] = 0x0;
				m_scancodes[ c_vk2fb[wParam] ] /*&= 0x2;*/  = KB_CREATE_KEYRELEASE();
			}
		} break;
	default:
		return false;
	}
	return true;
}

const int Keyboard::c_vk2fb[256] = {
	0,
	0,
	0,
	SC_NUMLOCK, //control break process
	0,
	0,
	0,
	0,
	SC_BACKSPACE,
	SC_TAB,
	0,
	0,
	0,
	SC_ENTER,
	0,
	0,

	SC_SHIFT, //generic shift key
	SC_CTRL, //generic control key
	SC_ALT, //generic alt key
	SC_PAUSE,
	SC_CAPSLOCK,
	0,
	0,
	0,
	0,
	0,
	0,
	SC_ESC,
	0,
	0,
	0,
	0,

	SC_SPACE,
	SC_PAGEUP,
	SC_PAGEDOWN,
	SC_END,
	SC_HOME,
	SC_LEFT,
	SC_UP,
	SC_RIGHT,
	SC_DOWN,
	0,
	0,
	0,
	SC_PRINTSCREEN,
	SC_INSERT,
	SC_DELETE,
	SC_F1, //help key

	SC_0,
	SC_1,
	SC_2,
	SC_3,
	SC_4,
	SC_5,
	SC_6,
	SC_7,
	SC_8,
	SC_9,
	0,
	0,
	0,
	0,
	0,
	0,

	0,
	SC_A,
	SC_B,
	SC_C,
	SC_D,
	SC_E,
	SC_F,
	SC_G,
	SC_H,
	SC_I,
	SC_J,
	SC_K,
	SC_L,
	SC_M,
	SC_N,
	SC_O,

	SC_P,
	SC_Q,
	SC_R,
	SC_S,
	SC_T,
	SC_U,
	SC_V,
	SC_W,
	SC_X,
	SC_Y,
	SC_Z,
	SC_LEFTWINLOGO,
	SC_RIGHTWINLOGO,
	SC_CONTEXT, //applications key
	0,
	0,

	//number pad keys
	SC_NUMPAD0,
	SC_NUMPAD1,
	SC_NUMPAD2,
	SC_NUMPAD3,
	SC_NUMPAD4,
	SC_NUMPAD5,
	SC_NUMPAD6,
	SC_NUMPAD7,
	SC_NUMPAD8,
	SC_NUMPAD9,
	SC_NUMPADASTERIX,
	SC_NUMPADPLUS,
	0, //separator key?
	SC_NUMPADMINUS,
	SC_NUMPADPERIOD,
	SC_NUMPADSLASH,

	SC_F1,
	SC_F2,
	SC_F3,
	SC_F4,
	SC_F5,
	SC_F6,
	SC_F7,
	SC_F8,
	SC_F9,
	SC_F10,
	SC_F11,
	SC_F12,
	SC_F13,
	SC_F14,
	SC_F15,
	0,

	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,

	SC_NUMLOCK,
	SC_SCROLLLOCK,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,

	SC_LEFTSHIFT,
	SC_RIGHTSHIFT,
	SC_LEFTCTRL,
	SC_RIGHTCTRL,
	SC_LEFTALT,
	SC_RIGHTALT,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,

	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	SC_SEMICOLON,
	SC_EQUALS,
	SC_COMMA,
	SC_MINUS,
	SC_PERIOD,
	SC_SLASH,

	SC_TILDE,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	SC_NUMPADENTER, //this is supposed to be reserved, but it was recommended to change here at 0xCA; it still doesn't work, though
	0,
	0,
	0,
	0,
	0,

	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	SC_LEFTBRACKET,
	SC_BACKSLASH,
	SC_RIGHTBRACKET,
	SC_QUOTE,
	0,

	0,
	0,
	SC_BACKSLASH, //angle bracket or backslash key
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,

	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
};
