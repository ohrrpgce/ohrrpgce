//keyboard.h
//by Jay Tennant 9/25/10; updated 4/21/11
//manages keyboard input processing from window messages, converting to ohr-specific scancodes

#pragma once

#include <windows.h>

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
				pScancodes[i] |= m_scancodes[i];
				m_scancodes[i] &= 0x1;
			}
		}
		void getVirtualKeys(BYTE* pVirtualKeys) const {memcpy((void*)pVirtualKeys, (void*)m_virtualKeys, sizeof(m_virtualKeys));}

		//void Poll();
		bool processMessage(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
	};
}