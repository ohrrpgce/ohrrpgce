//gfx_directx_cls_mouse.h
//by Jay Tennant 9/22/10
//new manager for windows mouse

#ifndef GFX_DIRECTX_CLS_MOUSE_H
#define GFX_DIRECTX_CLS_MOUSE_H

#include <windows.h>
#include <stack>

namespace gfx
{
	class Mouse
	{
	public:
		class Buttons
		{
		private:
			unsigned int btns;
		public:
			Buttons() : btns(0) {}
			unsigned int GetData() const {return btns;}
			bool IsLeftDown() const {return (btns & 0x1) ? true : false;}
			bool IsRightDown() const {return (btns & 0x2) ? true : false;}
			bool IsMiddleDown() const {return (btns & 0x4) ? true : false;}
			bool IsAnyDown() const {return (btns & 0x7) ? true : false;}
			void SetLeftDown() {btns |= 0x1;}
			void SetLeftUp() {btns &= 0xfffffffe;}
			void SetRightDown() {btns |= 0x2;}
			void SetRightUp() {btns &= 0xfffffffd;}
			void SetMiddleDown() {btns |= 0x4;}
			void SetMiddleUp() {btns &= 0xfffffffb;}
			void SetAllDown() {btns |= 0x7;}
			void SetAllUp() {btns = 0x0;}
		};

		enum InputState
		{
			IS_DEAD,
			IS_LIVE,
		};
		enum VideoMode
		{
			VM_WINDOWED,
			VM_FULLSCREEN,
		};

	protected:
		struct LiveState
		{
			VideoMode mode;
			InputState state;
			bool bCursorVisible;
			bool bClipEnable;
			RECT rClip;
		};

	protected:
		POINT m_cursorPos;
		long m_wheel;
		Buttons m_buttons;
		std::stack<LiveState> m_liveState;
	public:
		Mouse();

		const POINT& GetCursorPos() const {return m_cursorPos;}
		long GetWheel() const {return m_wheel;}
		Buttons GetButtonState() const {return m_buttons;}
		bool IsInputLive() const {return m_liveState.top().state == IS_LIVE;}

		bool ProcessMessage(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
		void SetInputState(InputState state);
		void SetVideoMode(VideoMode mode);
		void PushState();
		void PopState();
	};
}

#endif