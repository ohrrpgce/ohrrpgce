//mouse.h
//by Jay Tennant 9/22/10; updated 4/21/11
//manages mouse input processing through window messages

#pragma once

#include <windows.h>
#include <stack>

#include "../gfx_common/gfx.h" // for CursorVisibility

namespace gfx
{
	//class Mouse
	//{
	//public:
	//	class Buttons
	//	{
	//	private:
	//		unsigned int btns;
	//	public:
	//		Buttons() : btns(0) {}
	//		unsigned int GetData() const {return btns;}
	//		bool IsLeftDown() const {return (btns & 0x1) ? true : false;}
	//		bool IsRightDown() const {return (btns & 0x2) ? true : false;}
	//		bool IsMiddleDown() const {return (btns & 0x4) ? true : false;}
	//		bool IsAnyDown() const {return (btns & 0x7) ? true : false;}
	//		void SetLeftDown() {btns |= 0x1;}
	//		void SetLeftUp() {btns &= 0xfffffffe;}
	//		void SetRightDown() {btns |= 0x2;}
	//		void SetRightUp() {btns &= 0xfffffffd;}
	//		void SetMiddleDown() {btns |= 0x4;}
	//		void SetMiddleUp() {btns &= 0xfffffffb;}
	//		void SetAllDown() {btns |= 0x7;}
	//		void SetAllUp() {btns = 0x0;}
	//	};

	//	enum InputState
	//	{
	//		IS_DEAD,
	//		IS_LIVE,
	//	};
	//	enum VideoMode
	//	{
	//		VM_WINDOWED,
	//		VM_FULLSCREEN,
	//	};

	//protected:
	//	struct LiveState
	//	{
	//		VideoMode mode;
	//		InputState state;
	//		bool bCursorVisible;
	//		bool bClipEnable;
	//		RECT rClip;
	//	};

	//protected:
	//	POINT m_cursorPos;
	//	long m_wheel;
	//	Buttons m_buttons;
	//	std::stack<LiveState> m_liveState;
	//public:
	//	Mouse();

	//	const POINT& GetCursorPos() const {return m_cursorPos;}
	//	long GetWheel() const {return m_wheel;}
	//	Buttons GetButtonState() const {return m_buttons;}
	//	bool IsInputLive() const {return m_liveState.top().state == IS_LIVE;}

	//	bool ProcessMessage(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
	//	void SetInputState(InputState state);
	//	void SetVideoMode(VideoMode mode);
	//	void PushState();
	//	void PopState();
	//};

	class Mouse2
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
		POINT m_cursorPos;
		long m_wheel;
		Buttons m_buttons;
		std::stack<InputState> m_inputState;
		State m_state;

		void updateCursorVisibility();
		void updatePosition();

	public:
		Mouse2();

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
		// Set cursor position in game coords. Returns TRUE on scuess
		int setPosition(int x, int y);
		void updateClippingRect(); //call whenever the window size changes
		void pushState(InputState state);
		void popState();
	};
}
