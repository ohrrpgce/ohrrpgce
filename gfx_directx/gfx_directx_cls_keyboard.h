//gfx_directx_cls_keyboard.h
//by Jay Tennant 9/25/10
//manages keyboard input processing from window messages, converting to ohr-specific scancodes

#ifndef GFX_DIRECTX_CLS_KEYBOARD_H
#define GFX_DIRECTX_CLS_KEYBOARD_H

#include <windows.h>

namespace gfx
{
	class Keyboard
	{
	protected:
		static const int c_vk2fb[256];
		int m_scancodes[128];
		BYTE m_virtualKeys[256];
	public:
		Keyboard();

		void GetOHRScans(int* pScancodes) const {for(UINT i = 0; i < 128; i++) pScancodes[i] |= m_scancodes[i];}
		void GetVirtualKeys(BYTE* pVirtualKeys) const {memcpy((void*)pVirtualKeys, (void*)m_virtualKeys, sizeof(m_virtualKeys));}

		bool ProcessMessage(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
	};
}

#endif