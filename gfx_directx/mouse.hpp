//mouse.h
//by Jay Tennant 9/22/10; updated 4/21/11
//manages mouse input processing through window messages

#pragma once

#include <windows.h>
#include <stack>

#include "d3d.hpp"
#include "../gfx.h" // for CursorVisibility

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
			unsigned int getData() const {return btns;}
			bool isLeftDown() const {return (btns & 0x1) ? true : false;}
			bool isRightDown() const {return (btns & 0x2) ? true : false;}
			bool isMiddleDown() const {return (btns & 0x4) ? true : false;}
			bool isAnyDown() const {return (btns & 0x7) ? true : false;}
			void setLeftDown() {btns |= 0x1;}
			void setLeftUp() {btns &= 0xfffffffe;}
			void setRightDown() {btns |= 0x2;}
			void setRightUp() {btns &= 0xfffffffd;}
			void setMiddleDown() {btns |= 0x4;}
			void setMiddleUp() {btns &= 0xfffffffb;}
			void setAllDown() {btns |= 0x7;}
			void setAllUp() {btns = 0x0;}
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
		enum ClipState
		{
			CS_OFF,
			CS_ON,
		};

	protected:
		struct State
		{
			VideoMode mode;
			CursorVisibility visibility;  // The visibility requested by the engine
			bool bCursorVisible;  // Whether the cursor is actually currently visible
			bool bOverClient;  // Whether the mouse is over the client area (where visibility takes effect)
			ClipState clipped;
			RECT rClippedArea;
			ClipState buttonClipped;
			RECT rButtonClippedArea;
		};

	protected:
		HWND m_hWnd;
		D3D *m_pDirectX;  // pointer to g_DirectX
		POINT m_cursorPos;  // position in engine coords, clamped to valid range.
		long m_wheel;
		Buttons m_buttons;
		std::stack<InputState> m_inputState;
		State m_state;

		RECT ScaleRectClient(const RECT& rSrc);
		void updateCursorVisibility();
		void updatePosition();
		void startClickInducedClipping();
		void endClickInducedClipping();

	public:
		Mouse();

		void initialize(D3D *pDirectX);

		const POINT& getCursorPos() const {return m_cursorPos;}
		long getWheel() const {return m_wheel;}
		Buttons getButtonState() const {return m_buttons;}
		bool isInputLive() const {return m_inputState.top() == IS_LIVE;}
		bool isVideoFullscreen() const {return m_state.mode == VM_FULLSCREEN;}
		bool isCursorVisible() const {return m_state.bCursorVisible;}
		bool isClippedCursor() const {return m_state.clipped == CS_ON;}

		bool processMessage(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
		void setInputState(InputState state);
		void setVideoMode(VideoMode mode);
		void setCursorVisibility(CursorVisibility visibility);
		void setClipState(ClipState state);
		void setClippingRect(RECT* pRect);
		// Set cursor position in engine coords. Returns TRUE on scuess
		int setPosition(int x, int y);
		void updateClippingRect(); //call whenever the window size changes
		void pushState(InputState state);
		void popState();
	};
}
