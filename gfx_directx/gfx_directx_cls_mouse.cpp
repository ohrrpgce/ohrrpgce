#include "gfx_directx_cls_mouse.h"
using namespace gfx;

Mouse::Mouse() : m_wheel(0)
{
	ZeroMemory(&m_cursorPos, sizeof(m_cursorPos));
	LiveState ns;
	ns.bClipEnable = false;
	ns.bCursorVisible = true;
	ns.mode = VM_WINDOWED;
	ns.state = IS_DEAD;
	m_liveState.push(ns);
}

bool Mouse::ProcessMessage(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	if(m_liveState.top().state == IS_DEAD)
		return false;

	switch(msg)
	{
	case WM_NCMOUSEMOVE:
	case WM_MOUSEMOVE:
		{
			::GetCursorPos(&m_cursorPos);
			ScreenToClient(hWnd, &m_cursorPos);
			RECT *clientRect = &m_liveState.top().rClip;
			GetClientRect(hWnd, clientRect);

			if(m_cursorPos.x < clientRect->left) m_cursorPos.x = 0;
			else if(m_cursorPos.x > clientRect->right) m_cursorPos.x = clientRect->right;
			if(m_cursorPos.y < clientRect->top) m_cursorPos.y = 0;
			else if(m_cursorPos.y > clientRect->bottom) m_cursorPos.y = clientRect->bottom;

			m_cursorPos.x = (LONG)(320.0f * (float)m_cursorPos.x / (float)clientRect->right);
			m_cursorPos.y = (LONG)(200.0f * (float)m_cursorPos.y / (float)clientRect->bottom);
		} break;
	case WM_NCLBUTTONDOWN:
	case WM_LBUTTONDOWN:
		{
			if(!m_buttons.IsLeftDown())
			{
				m_buttons.SetLeftDown();
				if(m_liveState.top().mode == VM_WINDOWED)
				{
					RECT* pr = &m_liveState.top().rClip;
					//GetClientRect(hWnd, pr);
					GetWindowRect(hWnd, pr);
					ClipCursor(pr);
					m_liveState.top().bClipEnable = true;
				}
			}
		} break;
	case WM_NCLBUTTONUP:
	case WM_LBUTTONUP:
		{
			if(m_buttons.IsLeftDown())
			{
				m_buttons.SetLeftUp();
				if(!m_buttons.IsAnyDown() && m_liveState.top().bClipEnable)
				{
					ClipCursor(NULL);
					m_liveState.top().bClipEnable = false;
				}
			}
		} break;
	case WM_NCRBUTTONDOWN:
	case WM_RBUTTONDOWN:
		{
			if(!m_buttons.IsRightDown())
			{
				m_buttons.SetRightDown();
				if(m_liveState.top().mode == VM_WINDOWED)
				{
					RECT* pr = &m_liveState.top().rClip;
					//GetClientRect(hWnd, pr);
					GetWindowRect(hWnd, pr);
					ClipCursor(pr);
					m_liveState.top().bClipEnable = true;
				}
			}
		} break;
	case WM_NCRBUTTONUP:
	case WM_RBUTTONUP:
		{
			if(m_buttons.IsRightDown())
			{
				m_buttons.SetRightUp();
				if(!m_buttons.IsAnyDown() && m_liveState.top().bClipEnable)
				{
					ClipCursor(NULL);
					m_liveState.top().bClipEnable = false;
				}
			}
		} break;
	case WM_NCMBUTTONDOWN:
	case WM_MBUTTONDOWN:
		{
			if(!m_buttons.IsMiddleDown())
			{
				m_buttons.SetMiddleDown();
				if(m_liveState.top().mode == VM_WINDOWED)
				{
					RECT* pr = &m_liveState.top().rClip;
					//GetClientRect(hWnd, pr);
					GetWindowRect(hWnd, pr);
					ClipCursor(pr);
					m_liveState.top().bClipEnable = true;
				}
			}
		} break;
	case WM_NCMBUTTONUP:
	case WM_MBUTTONUP:
		{
			if(m_buttons.IsMiddleDown())
			{
				m_buttons.SetMiddleUp();
				if(!m_buttons.IsAnyDown() && m_liveState.top().bClipEnable)
				{
					ClipCursor(NULL);
					m_liveState.top().bClipEnable = false;
				}
			}
		} break;
	case WM_MOUSEWHEEL:
		{
			m_wheel += GET_WHEEL_DELTA_WPARAM(wParam);
		} break;
	default:
		return false;
	}

	GetWindowRect(hWnd, &m_liveState.top().rClip);
	return true;
}

void Mouse::SetInputState(InputState state)
{
	if(m_liveState.top().state != state)
	{
		LiveState &ls = m_liveState.top();
		ls.state = state;
		if(ls.state == IS_LIVE)
		{
			if(ls.bCursorVisible)
			{
				ShowCursor(FALSE);
				ls.bCursorVisible = false;
			}
			if(ls.mode == VM_WINDOWED)
			{
				if(ls.bClipEnable)
					ClipCursor(&ls.rClip);
			}
			else
				ClipCursor(NULL);
		}
		else
		{
			if(ls.bClipEnable == true)
			{
				ClipCursor(NULL);
				ls.bClipEnable = false;
			}
			if(ls.mode == VM_WINDOWED)
			{
				if(!ls.bCursorVisible)
				{
					ShowCursor(TRUE);
					ls.bCursorVisible = true;
				}
			}
		}
	}
}

void Mouse::SetVideoMode(VideoMode mode)
{
	if(m_liveState.top().mode != mode)
	{
		LiveState &ls = m_liveState.top();
		ls.mode = mode;
		if(ls.mode == VM_WINDOWED)
		{
			if(ls.bClipEnable)
				ClipCursor(&ls.rClip);
			if(ls.state == IS_DEAD)
			{
				if(!ls.bCursorVisible)
				{
					ShowCursor(TRUE);
					ls.bCursorVisible = true;
				}
			}
			else
			{
				if(ls.bCursorVisible)
				{
					ShowCursor(FALSE);
					ls.bCursorVisible = false;
				}
			}
		}
		else
		{
			if(ls.bClipEnable)
			{
				ClipCursor(NULL);
			}
			if(ls.bCursorVisible)
			{
				ShowCursor(FALSE);
				ls.bCursorVisible = false;
			}
		}
	}
}

void Mouse::PushState()
{
	LiveState ns(m_liveState.top());
	m_liveState.push(ns);
}

void Mouse::PopState()
{
	if(m_liveState.size() == 1)
		return;
	LiveState os(m_liveState.top());
	m_liveState.pop();
	LiveState &ls = m_liveState.top();

	ls.rClip = os.rClip;
	ls.bClipEnable = os.bClipEnable;
	ls.bCursorVisible = os.bCursorVisible;

	if(ls.state != os.state)
	{
		ls.state = os.state;
		if(ls.state == IS_LIVE)
			SetInputState(IS_DEAD);
		else
			SetInputState(IS_LIVE);
	}
	if(ls.mode != os.mode)
	{
		ls.mode = os.mode;
		if(ls.mode == VM_WINDOWED)
			SetVideoMode(VM_FULLSCREEN);
		else
			SetVideoMode(VM_WINDOWED);
	}
}

//////////////////////////////
//rewrite of Mouse

Mouse2::Mouse2() : m_wheel(0)
{
	ZeroMemory(&m_cursorPos, sizeof(m_cursorPos));
	LiveState ns;
	ns.buttonClipped = CS_OFF;
	ZeroMemory(&ns.rButtonClippedArea, sizeof(ns.rButtonClippedArea));
	ZeroMemory(&ns.rClippedArea, sizeof(ns.rClippedArea));
	ns.clipped = CS_OFF;
	ns.visibility = CV_SHOW;
	ns.mode = VM_WINDOWED;
	ns.state = IS_DEAD;
	m_liveState.push(ns);
}

bool Mouse2::ProcessMessage(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{//needs work
}

void Mouse2::SetInputState(InputState state)
{//needs work
}

void Mouse2::SetVideoMode(VideoMode mode)
{//needs finishing
	if(m_liveState.top().mode == mode)
		return;
	LiveState &ls = m_liveState.top();
}

void Mouse2::SetCursorVisibility(CursorVisibility visibility)
{
	if(m_liveState.top().visibility == visibility)
		return;
	LiveState &ls = m_liveState.top();
	ls.visibility = visibility;
	if(ls.visibility == CV_SHOW)
		if(ls.mode == VM_FULLSCREEN)
			return;
		else
			ShowCursor(TRUE);
	else
		if(ls.mode == VM_FULLSCREEN)
			return;
		else
			ShowCursor(FALSE);
}

void Mouse2::SetClipState(ClipState state)
{
	if(m_liveState.top().clipped == state)
		return;
	LiveState &ls = m_liveState.top();
	ls.clipped = state;
	if(ls.clipped == CS_OFF)
		if(ls.buttonClipped == CS_OFF)
			ClipCursor(NULL);
		else
			ClipCursor(&ls.rButtonClippedArea);
	else
		ClipCursor(&ls.rClippedArea);
}

void Mouse2::SetClippingRect(RECT *pRect)
{
	if(pRect == NULL)
		return;
	m_liveState.top().rClippedArea = *pRect;
	if(m_liveState.top().clipped == CS_ON)
		ClipCursor(&m_liveState.top().rClippedArea);
}

void Mouse2::PushState()
{//needs review
	LiveState ns(m_liveState.top());
	m_liveState.push(ns);
}

void Mouse2::PopState()
{//needs work
}