#include "gfx_directx_cls_keyboard.h"
#include "..\\scancodes.h"
using namespace gfx;

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

bool Keyboard::ProcessMessage(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
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
				if(m_scLShift == (HIWORD(lParam) & 0x7f))
				{
					m_virtualKeys[VK_LSHIFT] = 0x80;
					m_scancodes[ c_vk2fb[VK_LSHIFT] ] |= 0x3;
				}
				else
				{
					m_virtualKeys[VK_RSHIFT] = 0x80;
					m_scancodes[ c_vk2fb[VK_RSHIFT] ] |= 0x3;
				}
			}
			else
			{
				m_virtualKeys[wParam] = 0x80;
				m_scancodes[ c_vk2fb[wParam] ] |= 0x3;
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
					m_scancodes[ c_vk2fb[wParam] ] &= 0x2;
				}
				else
				{
					m_virtualKeys[wParam] = 0x80;
					m_scancodes[ c_vk2fb[wParam] ] |= 0x3;
				}
			}
			else if(wParam == VK_SHIFT)
			{//to distinguish between left and right
				if(m_scLShift == (HIWORD(lParam) & 0x7f))
				{
					m_virtualKeys[VK_LSHIFT] = 0x0;
					m_scancodes[ c_vk2fb[VK_LSHIFT] ] &= 0x2;
				}
				else
				{
					m_virtualKeys[VK_RSHIFT] = 0x0;
					m_scancodes[ c_vk2fb[VK_RSHIFT] ] &= 0x2;
				}
			}
			else
			{
				m_virtualKeys[wParam] = 0x0;
				m_scancodes[ c_vk2fb[wParam] ] &= 0x2;
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

	0, //generic shift key--what to do? Nothing: left and right are accounted for as special cases
	SC_CTRL, //generic control key--what to do? fb doesn't distinguish between the left and right, so pass it along
	SC_ALT, //generic alt key--what to do? fb doesn't distinguish between the left and right, so pass it along
	SC_NUMLOCK, //pause key
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
	0, //print screen
	SC_INSERT,
	SC_DELETE,
	0,

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
	SC_ASTERIX,
	SC_PLUS,
	0, //separator key?
	SC_MINUS,
	SC_PERIOD, //decimal--should it do nothing?
	SC_SLASH,

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
	SC_CTRL,
	SC_CTRL,
	SC_ALT,
	SC_ALT,
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
	SC_LEFTBRACKET,
	SC_BACKSLASH,
	SC_RIGHTBRACKET,
	SC_QUOTE,
	0,

	0,
	0,
	0, //angle bracket or backslash key
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
