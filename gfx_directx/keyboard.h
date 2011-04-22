//keyboard.h
//by Jay Tennant 9/25/10; updated 4/21/11
//manages keyboard input processing from window messages, converting to ohr-specific scancodes

#pragma once

#include <windows.h>

#define KB_STATE 0x2 //are these values right? are they reversed?
#define KB_EVENT 0x1

#define KB_CONSUME_EVENT(bits)		(bits & (~KB_EVENT))
#define KB_CREATE_KEYPRESS()		(KB_STATE | KB_EVENT)
#define KB_CREATE_KEYRELEASE()		(KB_EVENT)

#define KB_IS_KEY_DOWN(bits)		( ((bits & KB_STATE) != 0) ? true : false )
#define KB_IS_NEW_EVENT(bits)		( ((bits & KB_EVENT) != 0) ? true : false )

#define KB_IS_NEW_KEYPRESS(bits)	(KB_IS_KEY_DOWN(bits) && KB_IS_NEW_EVENT(bits))
#define KB_IS_OLD_KEYPRESS(bits)	(KB_IS_KEY_DOWN(bits) && !KB_IS_NEW_EVENT(bits))
#define KB_IS_NEW_KEYRELEASE(bits)	(!KB_IS_KEYDOWN(bits) && KB_IS_NEW_EVENT(bits))
#define KB_IS_OLD_KEYRELEASE(bits)	(!KB_IS_KEYDOWN(bits) && !KB_IS_NEW_EVENT(bits))

namespace gfx
{
	class Keyboard
	{
	protected:
		static const int c_vk2fb[256];
		int m_scancodes[128];
		BYTE m_virtualKeys[256];
		UINT m_scLShift;
		UINT m_scLCtrl;
		UINT m_scLAlt;
	public:
		Keyboard();

		void getOHRScans(int* pScancodes) {
			for(UINT i = 0; i < 128; i++) {
				//pScancodes[i] |= m_scancodes[i];
				pScancodes[i] = m_scancodes[i];
				m_scancodes[i] /*&= 0x1;*/  = KB_CONSUME_EVENT(m_scancodes[i]);
			}
		}
		void getVirtualKeys(BYTE* pVirtualKeys) const {memcpy((void*)pVirtualKeys, (void*)m_virtualKeys, sizeof(m_virtualKeys));}

		//void Poll();
		bool processMessage(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
	};
}