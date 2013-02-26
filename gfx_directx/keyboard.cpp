#include "keyboard.h"
#include "..\\scancodes.h"
using namespace gfx;

#define VK_NUMPAD_ENTER 0xCA //reserved in windows, but recommended on a site

Keyboard::Keyboard() : m_scLShift(0), m_bActive(true)
{
	ZeroMemory(m_scancodes, sizeof(m_scancodes));
	ZeroMemory(m_virtualKeys, sizeof(m_virtualKeys));
	m_scLShift = MapVirtualKey(VK_SHIFT, MAPVK_VK_TO_VSC);
}

void Keyboard::getOHRScans(int *pScancodes)
{
	//workaround to get the second of two shift keys to release
	if(!(GetAsyncKeyState(VK_LSHIFT) & 0x8000)) //most significant bit, 0x8000, is current state
	{
		m_virtualKeys[VK_LSHIFT] = 0x0;
		KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_LSHIFT] ]);
	}
	if(!(GetAsyncKeyState(VK_RSHIFT) & 0x8000)) //most significant bit, 0x8000, is current state
	{
		m_virtualKeys[VK_RSHIFT] = 0x0;
		KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_RSHIFT] ]);
	}
	if(!(m_virtualKeys[VK_LSHIFT] || m_virtualKeys[VK_RSHIFT]))
	{
		m_virtualKeys[VK_SHIFT] = 0x0;
		KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_SHIFT] ]);
	}

	//printscreen workaround
	if((GetAsyncKeyState(VK_SNAPSHOT) & 0x8000)) //most significant bit, 0x8000, is current state
	{
		if(m_virtualKeys[VK_SNAPSHOT] == 0x0)
		{
			m_virtualKeys[VK_SNAPSHOT] = 0x80;
			KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_SNAPSHOT] ]);
		}
	}
	else
	{
		if(m_virtualKeys[VK_SNAPSHOT] == 0x80)
		{
			m_virtualKeys[VK_SNAPSHOT] = 0x0;
			KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_SNAPSHOT] ]);
		}
	}

	//obtain toggle state every loop
	if(GetKeyState(VK_NUMLOCK) & 0x1) //least significant bit, 0x1, is current toggled state
	{
		if(m_virtualKeys[VK_NUMLOCK] == 0x0)
		{
			m_virtualKeys[VK_NUMLOCK] = 0x80;
			KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_NUMLOCK] ]);
		}
	}
	else
	{
		if(m_virtualKeys[VK_NUMLOCK] == 0x80)
		{
			m_virtualKeys[VK_NUMLOCK] = 0x0;
			KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_NUMLOCK] ]);
		}
	}
	if(GetKeyState(VK_SCROLL) & 0x1) //least significant bit, 0x1, is current toggled state
	{
		if(m_virtualKeys[VK_SCROLL] == 0x0)
		{
			m_virtualKeys[VK_SCROLL] = 0x80;
			KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_SCROLL] ]);
		}
	}
	else
	{
		if(m_virtualKeys[VK_SCROLL] == 0x80)
		{
			m_virtualKeys[VK_SCROLL] = 0x0;
			KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_SCROLL] ]);
		}
	}
	if(GetKeyState(VK_CAPITAL) & 0x1) //least significant bit, 0x1, is current toggled state
	{
		if(m_virtualKeys[VK_CAPITAL] == 0x0)
		{
			m_virtualKeys[VK_CAPITAL] = 0x80;
			KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_CAPITAL] ]);
		}
	}
	else
	{
		if(m_virtualKeys[VK_CAPITAL] == 0x80)
		{
			m_virtualKeys[VK_CAPITAL] = 0x0;
			KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_CAPITAL] ]);
		}
	}

	for(UINT i = 0; i < 128; i++) 
	{
		pScancodes[i] = m_scancodes[i];
		KB_CONSUME_EVENT(m_scancodes[i]);
	}
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

#define KB_BREAK_IF_SET(vkEntry) if(vkEntry & 0x80) break

bool Keyboard::processMessage(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	if(!m_bActive && msg != WM_ACTIVATE)
		return false;

	switch(msg)
	{
	case WM_CHAR:
		{
			m_textInput += WCHAR(wParam);
		} break;
	case WM_ACTIVATE:
		{
			if(LOWORD(wParam) == WA_INACTIVE)
			{
				m_bActive = false;
				for(UINT i = 0; i < 256; i++)
					if(m_virtualKeys[i] != 0x0)
						if(i != VK_NUMLOCK && i != VK_SCROLL && i != VK_CAPITAL)
						{
							m_virtualKeys[i] = 0x0;
							KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[i] ]);
						}
			}
			else
			{
				m_bActive = true;
			}
		} break;
	case WM_SYSKEYDOWN:
	case WM_KEYDOWN:
		{
			////prevents multiple keydown messages for held keys; this is not the way--KB_BREAK_IF_SET() fixes this
			//if(HIWORD(lParam) & 0x4000)
			//	break;
			switch(wParam)
			{
			case VK_NUMLOCK:
			case VK_SCROLL:
			case VK_CAPITAL:
				break;
			case VK_SHIFT:
				{//to distinguish between left and right
					if(HIWORD(lParam) & 0x100) //extended key, fake shifts: http://www.win.tue.nl/~aeb/linux/kbd/scancodes-1.html#fakeshifts
						break; //we don't want fake shifts
					if(m_scLShift == (HIWORD(lParam) & 0xff))
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_LSHIFT]);
						m_virtualKeys[VK_LSHIFT] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_LSHIFT] ]);
					}
					else
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_RSHIFT]);
						m_virtualKeys[VK_RSHIFT] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_RSHIFT] ]);
					}
					if(m_virtualKeys[VK_SHIFT] == 0x0)//if(m_virtualKeys[VK_LSHIFT] || m_virtualKeys[VK_RSHIFT])
					{
						m_virtualKeys[VK_SHIFT] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_SHIFT] ]);
					}
				} break;
			case VK_CONTROL:
				{//to distinguish between left and right
					if(HIWORD(lParam) & 0x100) //extended key, right control
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_RCONTROL]);
						m_virtualKeys[VK_RCONTROL] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_RCONTROL] ]);
					}
					else
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_LCONTROL]);
						m_virtualKeys[VK_LCONTROL] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_LCONTROL] ]);
					}
					if(m_virtualKeys[VK_CONTROL] == 0x0)//if(m_virtualKeys[VK_LCONTROL] || m_virtualKeys[VK_RCONTROL])
					{
						m_virtualKeys[VK_CONTROL] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_CONTROL] ]);
					}
				} break;
			case VK_MENU:
				{//to distinguish between left and right
					if(HIWORD(lParam) & 0x100) //extended key, right alt
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_RMENU]);
						m_virtualKeys[VK_RMENU] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_RMENU] ]);
					}
					else
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_LMENU]);
						m_virtualKeys[VK_LMENU] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_LMENU] ]);
					}
					if(m_virtualKeys[VK_MENU] == 0x0)//if(m_virtualKeys[VK_LMENU] || m_virtualKeys[VK_RMENU])
					{
						m_virtualKeys[VK_MENU] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_MENU] ]);
					}
				} break;
			case VK_RETURN:
				{
					if(HIWORD(lParam) & 0x100) //extended key, numpad enter
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_NUMPAD_ENTER]);
						m_virtualKeys[VK_NUMPAD_ENTER] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_NUMPAD_ENTER] ]);
					}
					else
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_RETURN]);
						m_virtualKeys[VK_RETURN] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_RETURN] ]);
					}
				} break;
			case VK_HOME:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 7 key
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_HOME]);
						m_virtualKeys[VK_HOME] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_HOME] ]);
					}
					else
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_NUMPAD7]);
						m_virtualKeys[VK_NUMPAD7] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_NUMPAD7] ]);
					}
				} break;
			case VK_UP:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 8 key
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_UP]);
						m_virtualKeys[VK_UP] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_UP] ]);
					}
					else
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_NUMPAD8]);
						m_virtualKeys[VK_NUMPAD8] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_NUMPAD8] ]);
					}
				} break;
			case VK_PRIOR:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 9 key
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_PRIOR]);
						m_virtualKeys[VK_PRIOR] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_PRIOR] ]);
					}
					else
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_NUMPAD9]);
						m_virtualKeys[VK_NUMPAD9] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_NUMPAD9] ]);
					}
				} break;
			case VK_LEFT:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 4 key
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_LEFT]);
						m_virtualKeys[VK_LEFT] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_LEFT] ]);
					}
					else
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_NUMPAD4]);
						m_virtualKeys[VK_NUMPAD4] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_NUMPAD4] ]);
					}
				} break;
			case VK_CLEAR:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 5 key
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_CLEAR]);
						m_virtualKeys[VK_CLEAR] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_CLEAR] ]);
					}
					else
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_NUMPAD5]);
						m_virtualKeys[VK_NUMPAD5] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_NUMPAD5] ]);
					}
				} break;
			case VK_RIGHT:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 6 key
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_RIGHT]);
						m_virtualKeys[VK_RIGHT] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_RIGHT] ]);
					}
					else
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_NUMPAD6]);
						m_virtualKeys[VK_NUMPAD6] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_NUMPAD6] ]);
					}
				} break;
			case VK_END:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 1 key
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_END]);
						m_virtualKeys[VK_END] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_END] ]);
					}
					else
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_NUMPAD1]);
						m_virtualKeys[VK_NUMPAD1] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_NUMPAD1] ]);
					}
				} break;
			case VK_DOWN:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 2 key
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_DOWN]);
						m_virtualKeys[VK_DOWN] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_DOWN] ]);
					}
					else
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_NUMPAD2]);
						m_virtualKeys[VK_NUMPAD2] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_NUMPAD2] ]);
					}
				} break;
			case VK_NEXT:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 3 key
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_NEXT]);
						m_virtualKeys[VK_NEXT] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_NEXT] ]);
					}
					else
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_NUMPAD3]);
						m_virtualKeys[VK_NUMPAD3] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_NUMPAD3] ]);
					}
				} break;
			case VK_INSERT:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 0 key
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_INSERT]);
						m_virtualKeys[VK_INSERT] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_INSERT] ]);
					}
					else
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_NUMPAD0]);
						m_virtualKeys[VK_NUMPAD0] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_NUMPAD0] ]);
					}
				} break;
			case VK_DELETE:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad decimal key
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_DELETE]);
						m_virtualKeys[VK_DELETE] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_DELETE] ]);
					}
					else
					{
						KB_BREAK_IF_SET(m_virtualKeys[VK_DECIMAL]);
						m_virtualKeys[VK_DECIMAL] = 0x80;
						KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[VK_DECIMAL] ]);
					}
				} break;
			default:
				{
					KB_BREAK_IF_SET(m_virtualKeys[wParam]);
					m_virtualKeys[wParam] = 0x80;
					KB_CREATE_KEYPRESS(m_scancodes[ c_vk2fb[wParam] ]);
				}
			}
		} break;
	case WM_SYSKEYUP:
	case WM_KEYUP:
		{
			switch(wParam)
			{
			case VK_NUMLOCK:
			case VK_SCROLL:
			case VK_CAPITAL:
				break;//these keys are now caught every time getOHRScans() is called
			case VK_SHIFT:
				{//to distinguish between left and right
					if(HIWORD(lParam) & 0x100) //extended key, fake shifts: http://www.win.tue.nl/~aeb/linux/kbd/scancodes-1.html#fakeshifts
						break; //we don't want fake shifts
					if(m_scLShift == (HIWORD(lParam) & 0xff))
					{
						m_virtualKeys[VK_LSHIFT] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_LSHIFT] ]);
					}
					else
					{
						m_virtualKeys[VK_RSHIFT] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_RSHIFT] ]);
					}
					if(!(m_virtualKeys[VK_LSHIFT] || m_virtualKeys[VK_RSHIFT]))
					{
						m_virtualKeys[VK_SHIFT] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_SHIFT] ]);
					}
				} break;
			case VK_CONTROL:
				{//to distinguish between left and right
					if(HIWORD(lParam) & 0x100) //extended key, right control
					{
						m_virtualKeys[VK_RCONTROL] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_RCONTROL] ]);
					}
					else
					{
						m_virtualKeys[VK_LCONTROL] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_LCONTROL] ]);
					}
					if(!(m_virtualKeys[VK_LCONTROL] || m_virtualKeys[VK_RCONTROL]))
					{
						m_virtualKeys[VK_CONTROL] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_CONTROL] ]);
					}
				} break;
			case VK_MENU:
				{//to distinguish between left and right
					if(HIWORD(lParam) & 0x100) //extended key, right alt
					{
						m_virtualKeys[VK_RMENU] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_RMENU] ]);
					}
					else
					{
						m_virtualKeys[VK_LMENU] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_LMENU] ]);
					}
					if(!(m_virtualKeys[VK_LMENU] || m_virtualKeys[VK_RMENU]))
					{
						m_virtualKeys[VK_MENU] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_MENU] ]);
					}
				} break;
			case VK_RETURN:
				{
					if(HIWORD(lParam) & 0x100) //extended key, numpad enter
					{
						m_virtualKeys[VK_NUMPAD_ENTER] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_NUMPAD_ENTER] ]);
					}
					else
					{
						m_virtualKeys[VK_RETURN] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_RETURN] ]);
					}
				} break;
			case VK_HOME:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 7 key
					{
						m_virtualKeys[VK_HOME] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_HOME] ]);
					}
					else
					{
						m_virtualKeys[VK_NUMPAD7] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_NUMPAD7] ]);
					}
				} break;
			case VK_UP:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 8 key
					{
						m_virtualKeys[VK_UP] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_UP] ]);
					}
					else
					{
						m_virtualKeys[VK_NUMPAD8] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_NUMPAD8] ]);
					}
				} break;
			case VK_PRIOR:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 9 key
					{
						m_virtualKeys[VK_PRIOR] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_PRIOR] ]);
					}
					else
					{
						m_virtualKeys[VK_NUMPAD9] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_NUMPAD9] ]);
					}
				} break;
			case VK_LEFT:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 4 key
					{
						m_virtualKeys[VK_LEFT] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_LEFT] ]);
					}
					else
					{
						m_virtualKeys[VK_NUMPAD4] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_NUMPAD4] ]);
					}
				} break;
			case VK_CLEAR:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 5 key
					{
						m_virtualKeys[VK_CLEAR] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_CLEAR] ]);
					}
					else
					{
						m_virtualKeys[VK_NUMPAD5] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_NUMPAD5] ]);
					}
				} break;
			case VK_RIGHT:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 6 key
					{
						m_virtualKeys[VK_RIGHT] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_RIGHT] ]);
					}
					else
					{
						m_virtualKeys[VK_NUMPAD6] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_NUMPAD6] ]);
					}
				} break;
			case VK_END:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 1 key
					{
						m_virtualKeys[VK_END] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_END] ]);
					}
					else
					{
						m_virtualKeys[VK_NUMPAD1] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_NUMPAD1] ]);
					}
				} break;
			case VK_DOWN:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 2 key
					{
						m_virtualKeys[VK_DOWN] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_DOWN] ]);
					}
					else
					{
						m_virtualKeys[VK_NUMPAD2] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_NUMPAD2] ]);
					}
				} break;
			case VK_NEXT:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 3 key
					{
						m_virtualKeys[VK_NEXT] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_NEXT] ]);
					}
					else
					{
						m_virtualKeys[VK_NUMPAD3] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_NUMPAD3] ]);
					}
				} break;
			case VK_INSERT:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad 0 key
					{
						m_virtualKeys[VK_INSERT] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_INSERT] ]);
					}
					else
					{
						m_virtualKeys[VK_NUMPAD0] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_NUMPAD0] ]);
					}
				} break;
			case VK_DELETE:
				{
					if(HIWORD(lParam) & 0x100) //extended key, not NumPad decimal key
					{
						m_virtualKeys[VK_DELETE] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_DELETE] ]);
					}
					else
					{
						m_virtualKeys[VK_DECIMAL] = 0x0;
						KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[VK_DECIMAL] ]);
					}
				} break;
			default:
				{
					m_virtualKeys[wParam] = 0x0;
					KB_CREATE_KEYRELEASE(m_scancodes[ c_vk2fb[wParam] ]);
				}
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
	SC_NUMPADASTERISK,
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
	SC_NUMPADENTER, //this is supposed to be reserved, but it was recommended to change here at 0xCA
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
