#include "mouse.h"

using namespace gfx;

//Mouse::Mouse() : m_wheel(0)
//{
//	ZeroMemory(&m_cursorPos, sizeof(m_cursorPos));
//	LiveState ns;
//	ns.bClipEnable = false;
//	ns.bCursorVisible = true;
//	ns.mode = VM_WINDOWED;
//	ns.state = IS_DEAD;
//	m_liveState.push(ns);
//}
//
//bool Mouse::ProcessMessage(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
//{
//	if(m_liveState.top().state == IS_DEAD)
//		return false;
//
//	switch(msg)
//	{
//	case WM_NCMOUSEMOVE:
//	case WM_MOUSEMOVE:
//		{
//			::GetCursorPos(&m_cursorPos);
//			ScreenToClient(hWnd, &m_cursorPos);
//			RECT *clientRect = &m_liveState.top().rClip;
//			GetClientRect(hWnd, clientRect);
//
//			if(m_cursorPos.x < clientRect->left) m_cursorPos.x = 0;
//			else if(m_cursorPos.x > clientRect->right) m_cursorPos.x = clientRect->right;
//			if(m_cursorPos.y < clientRect->top) m_cursorPos.y = 0;
//			else if(m_cursorPos.y > clientRect->bottom) m_cursorPos.y = clientRect->bottom;
//
//			m_cursorPos.x = (LONG)(320.0f * (float)m_cursorPos.x / (float)clientRect->right);
//			m_cursorPos.y = (LONG)(200.0f * (float)m_cursorPos.y / (float)clientRect->bottom);
//		} break;
//	case WM_NCLBUTTONDOWN:
//	case WM_LBUTTONDOWN:
//		{
//			if(!m_buttons.IsLeftDown())
//			{
//				m_buttons.SetLeftDown();
//				if(m_liveState.top().mode == VM_WINDOWED)
//				{
//					RECT* pr = &m_liveState.top().rClip;
//					//GetClientRect(hWnd, pr);
//					GetWindowRect(hWnd, pr);
//					ClipCursor(pr);
//					m_liveState.top().bClipEnable = true;
//				}
//			}
//		} break;
//	case WM_NCLBUTTONUP:
//	case WM_LBUTTONUP:
//		{
//			if(m_buttons.IsLeftDown())
//			{
//				m_buttons.SetLeftUp();
//				if(!m_buttons.IsAnyDown() && m_liveState.top().bClipEnable)
//				{
//					ClipCursor(NULL);
//					m_liveState.top().bClipEnable = false;
//				}
//			}
//		} break;
//	case WM_NCRBUTTONDOWN:
//	case WM_RBUTTONDOWN:
//		{
//			if(!m_buttons.IsRightDown())
//			{
//				m_buttons.SetRightDown();
//				if(m_liveState.top().mode == VM_WINDOWED)
//				{
//					RECT* pr = &m_liveState.top().rClip;
//					//GetClientRect(hWnd, pr);
//					GetWindowRect(hWnd, pr);
//					ClipCursor(pr);
//					m_liveState.top().bClipEnable = true;
//				}
//			}
//		} break;
//	case WM_NCRBUTTONUP:
//	case WM_RBUTTONUP:
//		{
//			if(m_buttons.IsRightDown())
//			{
//				m_buttons.SetRightUp();
//				if(!m_buttons.IsAnyDown() && m_liveState.top().bClipEnable)
//				{
//					ClipCursor(NULL);
//					m_liveState.top().bClipEnable = false;
//				}
//			}
//		} break;
//	case WM_NCMBUTTONDOWN:
//	case WM_MBUTTONDOWN:
//		{
//			if(!m_buttons.IsMiddleDown())
//			{
//				m_buttons.SetMiddleDown();
//				if(m_liveState.top().mode == VM_WINDOWED)
//				{
//					RECT* pr = &m_liveState.top().rClip;
//					//GetClientRect(hWnd, pr);
//					GetWindowRect(hWnd, pr);
//					ClipCursor(pr);
//					m_liveState.top().bClipEnable = true;
//				}
//			}
//		} break;
//	case WM_NCMBUTTONUP:
//	case WM_MBUTTONUP:
//		{
//			if(m_buttons.IsMiddleDown())
//			{
//				m_buttons.SetMiddleUp();
//				if(!m_buttons.IsAnyDown() && m_liveState.top().bClipEnable)
//				{
//					ClipCursor(NULL);
//					m_liveState.top().bClipEnable = false;
//				}
//			}
//		} break;
//	case WM_MOUSEWHEEL:
//		{
//			m_wheel += GET_WHEEL_DELTA_WPARAM(wParam);
//		} break;
//	default:
//		return false;
//	}
//
//	GetWindowRect(hWnd, &m_liveState.top().rClip);
//	return true;
//}
//
//void Mouse::SetInputState(InputState state)
//{
//	if(m_liveState.top().state != state)
//	{
//		LiveState &ls = m_liveState.top();
//		ls.state = state;
//		if(ls.state == IS_LIVE)
//		{
//			if(ls.bCursorVisible)
//			{
//				ShowCursor(FALSE);
//				ls.bCursorVisible = false;
//			}
//			if(ls.mode == VM_WINDOWED)
//			{
//				if(ls.bClipEnable)
//					ClipCursor(&ls.rClip);
//			}
//			else
//				ClipCursor(NULL);
//		}
//		else
//		{
//			if(ls.bClipEnable == true)
//			{
//				ClipCursor(NULL);
//				ls.bClipEnable = false;
//			}
//			if(ls.mode == VM_WINDOWED)
//			{
//				if(!ls.bCursorVisible)
//				{
//					ShowCursor(TRUE);
//					ls.bCursorVisible = true;
//				}
//			}
//		}
//	}
//}
//
//void Mouse::SetVideoMode(VideoMode mode)
//{
//	if(m_liveState.top().mode != mode)
//	{
//		LiveState &ls = m_liveState.top();
//		ls.mode = mode;
//		if(ls.mode == VM_WINDOWED)
//		{
//			if(ls.bClipEnable)
//				ClipCursor(&ls.rClip);
//			if(ls.state == IS_DEAD)
//			{
//				if(!ls.bCursorVisible)
//				{
//					ShowCursor(TRUE);
//					ls.bCursorVisible = true;
//				}
//			}
//			else
//			{
//				if(ls.bCursorVisible)
//				{
//					ShowCursor(FALSE);
//					ls.bCursorVisible = false;
//				}
//			}
//		}
//		else
//		{
//			if(ls.bClipEnable)
//			{
//				ClipCursor(NULL);
//			}
//			if(ls.bCursorVisible)
//			{
//				ShowCursor(FALSE);
//				ls.bCursorVisible = false;
//			}
//		}
//	}
//}
//
//void Mouse::PushState()
//{
//	LiveState ns(m_liveState.top());
//	m_liveState.push(ns);
//}
//
//void Mouse::PopState()
//{
//	if(m_liveState.size() == 1)
//		return;
//	LiveState os(m_liveState.top());
//	m_liveState.pop();
//	LiveState &ls = m_liveState.top();
//
//	ls.rClip = os.rClip;
//	ls.bClipEnable = os.bClipEnable;
//	ls.bCursorVisible = os.bCursorVisible;
//
//	if(ls.state != os.state)
//	{
//		ls.state = os.state;
//		if(ls.state == IS_LIVE)
//			SetInputState(IS_DEAD);
//		else
//			SetInputState(IS_LIVE);
//	}
//	if(ls.mode != os.mode)
//	{
//		ls.mode = os.mode;
//		if(ls.mode == VM_WINDOWED)
//			SetVideoMode(VM_FULLSCREEN);
//		else
//			SetVideoMode(VM_WINDOWED);
//	}
//}
//
//////////////////////////////
//rewrite of Mouse

//scales rect from range inside (0,0,319,199) to the window's client rect
RECT ScaleRectClient(HWND hWnd, const RECT& rSrc)
{
	RECT rWin;
	//GetWindowRect(hWnd, &rWin);
	GetClientRect(hWnd, &rWin);
	POINT pnt1 = {1,1}, pnt2 = {rWin.right-1,rWin.bottom-1};
	ClientToScreen(hWnd, &pnt1);
	//ScreenToClient(hWnd, &pnt2);

	FLOAT l,t,r,b;
	l = (FLOAT)rSrc.left / 319.0f * (FLOAT)pnt2.x + (FLOAT)pnt1.x;
	t = (FLOAT)rSrc.top / 199.0f * (FLOAT)pnt2.y + (FLOAT)pnt1.y;
	r = (FLOAT)rSrc.right / 319.0f * (FLOAT)pnt2.x + (FLOAT)pnt1.x;
	b = (FLOAT)rSrc.bottom / 199.0f * (FLOAT)pnt2.y + (FLOAT)pnt1.y;

	rWin.left = (LONG)l;
	rWin.top = (LONG)t;
	rWin.right = (LONG)r;
	rWin.bottom = (LONG)b;
	return rWin;
}
//scales rect from range inside (0,0,319,199) to the window rect
RECT ScaleRectWindow(HWND hWnd, const RECT& rSrc)
{
	return ScaleRectClient(hWnd, rSrc); //this is better anyways

	//RECT rWin;
	//GetWindowRect(hWnd, &rWin);
	//FLOAT l,t,r,b;

	//l = (FLOAT)rSrc.left / 319.0f * (FLOAT)rWin.left;
	//t = (FLOAT)rSrc.top / 199.0f * (FLOAT)rWin.top;
	//r = (FLOAT)rSrc.right / 319.0f * (FLOAT)rWin.right;
	//b = (FLOAT)rSrc.bottom / 199.0f * (FLOAT)rWin.bottom;
	//
	//rWin.left = (LONG)l;
	//rWin.top = (LONG)t;
	//rWin.right = (LONG)r;
	//rWin.bottom = (LONG)b;
	//return rWin;
}

Mouse2::Mouse2() : m_wheel(0), m_hWnd(NULL), m_pDirectX(NULL)
{
	ZeroMemory(&m_cursorPos, sizeof(m_cursorPos));
	State ns;
	RECT r = {0,0,319,199};
	ns.rButtonClippedArea = r;
	ns.rClippedArea = r;
	ns.buttonClipped = CS_OFF;
	ns.clipped = CS_OFF;
	ns.visibility = CV_Default;
	// We will immediately receive a message to let us know if the mouse starts over the window
	ns.bOverClient = false;
	ns.bCursorVisible = true;
	ns.mode = VM_WINDOWED;
	m_state = ns;
	m_inputState.push(IS_LIVE);
}

void Mouse2::initialize(gfx::D3D *pDirectX)
{
	m_pDirectX = pDirectX;
}

bool Mouse2::processMessage(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{//if m_state.clipped == CS_ON, mouse clicks do not engage the m_state.buttonClipped
	m_hWnd = hWnd;
	if(m_inputState.top() == IS_DEAD)
		return false;

	static bool bWasOverClient = true, bWasHidingCursor = true;
	switch(msg)
	{
	case WM_NCHITTEST:
		{
			// Test whether the mouse is over the client area of the window
			// FIXME: it appears that it's wrong to do anything (like call ShowCursor) in response to this,
			// because this message can be recieved as a query rather than a mouse movement?
			// Should react to WM_NCMOUSEMOVE and WM_MOUSEMOVE instead?

			//if(!(m_state.mode == VM_WINDOWED && m_state.clipped == CS_ON && m_inputState.top() == IS_LIVE))
			//{

				if(HTCLIENT == DefWindowProc(hWnd, msg, wParam, lParam))
					m_state.bOverClient = true;
				else
					m_state.bOverClient = false;
				updateCursorVisibility();
			//}
			return false;
		}
	case WM_NCMOUSEMOVE:
		{
			if(m_state.mode == VM_WINDOWED)
			{
				//if(m_state.clipped == CS_ON)
				//{
				//	if(m_inputState.top() == IS_DEAD)
				//		return false;
				//}
				//else
					return false;
			}
		}
	case WM_MOUSEMOVE:
		{
			updatePosition();
		} break;
	case WM_NCLBUTTONDOWN:
		{
			if(m_state.mode == VM_WINDOWED)
			{
				//if(m_state.clipped == CS_ON)
				//{
				//	if(m_inputState.top() == IS_DEAD)
				//		return false;
				//}
				//else
					return false;
			}
		}
	case WM_LBUTTONDOWN:
		{
			if(!m_buttons.isLeftDown())
			{
				m_buttons.setLeftDown();
				if(m_state.mode == VM_WINDOWED && m_state.clipped == CS_OFF)
				{
					m_state.buttonClipped = CS_ON;
					ClipCursor(&ScaleRectWindow(hWnd, m_state.rButtonClippedArea));
				}
			}
		} break;
	case WM_NCLBUTTONUP:
		{
			if(m_state.mode == VM_WINDOWED)
			{
				//if(m_state.clipped == CS_ON)
				//{
				//	if(m_inputState.top() == IS_DEAD)
				//		return false;
				//}
				//else
					return false;
			}
		}
	case WM_LBUTTONUP:
		{
			if(m_buttons.isLeftDown())
			{
				m_buttons.setLeftUp();
				if(!m_buttons.isAnyDown() && m_state.clipped == CS_OFF && m_state.buttonClipped == CS_ON)
				{
					m_state.buttonClipped = CS_OFF;
					ClipCursor(NULL);
				}
			}
		} break;
	case WM_NCRBUTTONDOWN:
		{
			if(m_state.mode == VM_WINDOWED)
			{
				//if(m_state.clipped == CS_ON)
				//{
				//	if(m_inputState.top() == IS_DEAD)
				//		return false;
				//}
				//else
					return false;
			}
		}
	case WM_RBUTTONDOWN:
		{
			if(!m_buttons.isRightDown())
			{
				m_buttons.setRightDown();
				if(m_state.mode == VM_WINDOWED && m_state.clipped == CS_OFF)
				{
					m_state.buttonClipped = CS_ON;
					ClipCursor(&ScaleRectWindow(hWnd, m_state.rButtonClippedArea));
				}
			}
		} break;
	case WM_NCRBUTTONUP:
		{
			if(m_state.mode == VM_WINDOWED)
			{
				//if(m_state.clipped == CS_ON)
				//{
				//	if(m_inputState.top() == IS_DEAD)
				//		return false;
				//}
				//else
					return false;
			}
		}
	case WM_RBUTTONUP:
		{
			if(m_buttons.isRightDown())
			{
				m_buttons.setRightUp();
				if(!m_buttons.isAnyDown() && m_state.clipped == CS_OFF && m_state.buttonClipped == CS_ON)
				{
					m_state.buttonClipped = CS_OFF;
					ClipCursor(NULL);
				}
			}
		} break;
	case WM_NCMBUTTONDOWN:
		{
			if(m_state.mode == VM_WINDOWED)
			{
				//if(m_state.clipped == CS_ON)
				//{
				//	if(m_inputState.top() == IS_DEAD)
				//		return false;
				//}
				//else
					return false;
			}
		}
	case WM_MBUTTONDOWN:
		{
			if(!m_buttons.isMiddleDown())
			{
				m_buttons.setMiddleDown();
				if(m_state.mode == VM_WINDOWED && m_state.clipped == CS_OFF)
				{
					m_state.buttonClipped = CS_ON;
					ClipCursor(&ScaleRectWindow(hWnd, m_state.rButtonClippedArea));
				}
			}
		} break;
	case WM_NCMBUTTONUP:
		{
			if(m_state.mode == VM_WINDOWED)
			{
				//if(m_state.clipped == CS_ON)
				//{
				//	if(m_inputState.top() == IS_DEAD)
				//		return false;
				//}
				//else
					return false;
			}
		}
	case WM_MBUTTONUP:
		{
			if(m_buttons.isMiddleDown())
			{
				m_buttons.setMiddleUp();
				if(!m_buttons.isAnyDown() && m_state.clipped == CS_OFF && m_state.buttonClipped == CS_ON)
				{
					m_state.buttonClipped = CS_OFF;
					ClipCursor(NULL);
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

	return true;
}

void Mouse2::setInputState(InputState state)
{//this should be the ultimate adjuster!
	if(m_inputState.top() == state)
		return;
	m_inputState.top() = state;
	if(state == IS_LIVE)
	{
		if(m_state.mode == VM_FULLSCREEN)
		{
			if(m_state.clipped == CS_ON)
				ClipCursor(&ScaleRectClient(m_hWnd, m_state.rClippedArea));
			else
			{
				RECT r = {0,0,319,199};
				ClipCursor(&ScaleRectClient(m_hWnd, r));
			}
		}
		else
		{
			if(m_state.clipped == CS_ON)
				ClipCursor(&ScaleRectWindow(m_hWnd, m_state.rClippedArea));
		}
	}
	else
	{
		if(m_state.mode == VM_FULLSCREEN)
		{
			RECT r = {0,0,319,199};
			ClipCursor(&ScaleRectClient(m_hWnd, r));
		}
		else
		{
			ClipCursor(NULL);
		}
	}
	updateCursorVisibility();
}

void Mouse2::setVideoMode(VideoMode mode)
{//VideoMode has precedence over InputState for the cursor visibility and certain clipping
	if(m_state.mode == mode)
		return;
	m_state.mode = mode;
	if(m_state.mode == VM_FULLSCREEN)
	{
		if(m_inputState.top() == IS_DEAD)
		{
			RECT r = {0,0,319,199};
			ClipCursor(&ScaleRectClient(m_hWnd, r));
		}
		else
		{
			if(m_state.clipped == CS_ON)
				ClipCursor(&ScaleRectClient(m_hWnd, m_state.rClippedArea));
			else
			{
				RECT r = {0,0,319,199};
				ClipCursor(&ScaleRectClient(m_hWnd, r));
			}
		}
	}
	else
	{
		if(m_inputState.top() == IS_DEAD)
		{
			ClipCursor(NULL);
		}
		else
		{
			if(m_state.clipped == CS_ON)
				ClipCursor(&ScaleRectWindow(m_hWnd, m_state.rClippedArea));
			else
				ClipCursor(NULL);
		}
	}
	updateCursorVisibility();
}

LONG _round(double v)
{
	return (LONG)floor(v + .5);
}

// WM_MOUSEMOVE event
void Mouse2::updatePosition()
{
	POINT pos;
	::GetCursorPos(&pos);
	ScreenToClient(m_hWnd, &pos);

	// In client coordinates: window top-left is at 0,0
	RECT rImage = m_pDirectX->getImageRect();
	int imWidth = rImage.right - rImage.left;
	int imHeight = rImage.bottom - rImage.top;
	SIZE rGameRes = m_pDirectX->getImageResolution();

	m_cursorPos.x = (LONG)((float)(pos.x - rImage.left) * rGameRes.cx / imWidth);
	m_cursorPos.y = (LONG)((float)(pos.y - rImage.top) * rGameRes.cy / imHeight);

	m_cursorPos.x = min(max(m_cursorPos.x, 0), rGameRes.cx - 1);
	m_cursorPos.y = min(max(m_cursorPos.y, 0), rGameRes.cy - 1);
}

// client changes
int Mouse2::setPosition(int x, int y)
{
	DWORD xPos, yPos;
	RECT rDesktop;

	// In client coordinates: window top-left is at 0,0
	RECT rImage = m_pDirectX->getImageRect();
	int imWidth = rImage.right - rImage.left;
	int imHeight = rImage.bottom - rImage.top;
	SIZE rGameRes = m_pDirectX->getImageResolution();

	// Translate to desktop coordinates
	// add 0.5 to put the mouse cursor at the centre of the scaled pixel
	POINT pos;
	pos.x = (LONG)(rImage.left + (float)(x + 0.5) * imWidth / rGameRes.cx);
	pos.y = (LONG)(rImage.top + (float)(y + 0.5) * imHeight / rGameRes.cy);
	ClientToScreen(m_hWnd, &pos);

	//it was recommended not to use mouse_event; but if we need to, we could go back to it
	//mouse_event(MOUSEEVENTF_ABSOLUTE | MOUSEEVENTF_MOVE, xPos, yPos, 0, NULL);

	GetWindowRect(GetDesktopWindow(), &rDesktop);
	xPos = _round( (float)pos.x / (float)(rDesktop.right - 1) * 65535.0f );
	yPos = _round( (float)pos.y / (float)(rDesktop.bottom - 1) * 65535.0f );

	INPUT mouseEvent = { INPUT_MOUSE };
	mouseEvent.mi.dx = xPos;
	mouseEvent.mi.dy = yPos;
	mouseEvent.mi.dwFlags = MOUSEEVENTF_ABSOLUTE | MOUSEEVENTF_MOVE;
	if(0 == SendInput( 1, &mouseEvent, sizeof(mouseEvent) ))
		return FALSE;
	return TRUE;
}

// Decide whether the cursor should be visible, and enact.
void Mouse2::updateCursorVisibility()
{
	bool bCurrent = m_state.bCursorVisible;
	bool bNew;
	if(m_inputState.top() == IS_DEAD)
	{
		if(m_state.mode == VM_FULLSCREEN)
			bNew = false;
		else
			bNew = true;
	}
	else
	{
		if(!m_state.bOverClient)
			bNew = m_state.mode == VM_WINDOWED;
		else
		{
			if(m_state.visibility == CV_Visible)
				bNew = true;
			else if(m_state.visibility == CV_Hidden)
				bNew = false;
			else  // CV_Default
				bNew = m_state.mode == VM_WINDOWED;
		}
	}
	// ShowCursor increments or decrements an internal counter;
	// the cursor is hidden when the counter reaches zero.
	if (bNew && !bCurrent)
		ShowCursor(TRUE);
	else if (!bNew && bCurrent)
		ShowCursor(FALSE);
	m_state.bCursorVisible = bNew;
}

void Mouse2::setCursorVisibility(CursorVisibility visibility)
{
	if(m_state.visibility == visibility)
		return;
	m_state.visibility = visibility;
	if(m_inputState.top() == IS_DEAD)
		return;
	updateCursorVisibility();
}

void Mouse2::setClipState(ClipState state)
{
	if(m_state.clipped == state)
		return;
	m_state.clipped = state;
	if(m_inputState.top() == IS_DEAD)
		return;
	if(m_state.mode == VM_FULLSCREEN)
	{
		if(m_state.clipped == CS_ON)
		{
			ClipCursor(&ScaleRectClient(m_hWnd, m_state.rClippedArea));
		}
		else
		{
			RECT r = {0,0,319,199};
			ClipCursor(&ScaleRectClient(m_hWnd, r));
		}
	}
	else
	{
		if(m_state.clipped == CS_ON)
		{
			ClipCursor(&ScaleRectWindow(m_hWnd, m_state.rClippedArea));
		}
		else
		{
			ClipCursor(NULL);
		}
	}
}

void Mouse2::setClippingRect(RECT *pRect)
{
	if(pRect == NULL)
		return;
	m_state.rClippedArea = *pRect;
	m_state.rClippedArea.left = (m_state.rClippedArea.left > 0) ? ((m_state.rClippedArea.left < 319) ? m_state.rClippedArea.left : 319) : 0;
	m_state.rClippedArea.top = (m_state.rClippedArea.top > 0) ? ((m_state.rClippedArea.top < 199) ? m_state.rClippedArea.top : 199) : 0;
	m_state.rClippedArea.right = (m_state.rClippedArea.right > 0) ? ((m_state.rClippedArea.right < 319) ? m_state.rClippedArea.right : 319) : 0;
	m_state.rClippedArea.bottom = (m_state.rClippedArea.bottom > 0) ? ((m_state.rClippedArea.bottom < 199) ? m_state.rClippedArea.bottom : 199) : 0;
	if(m_inputState.top() == IS_DEAD)
		return;
	if(m_state.mode == VM_FULLSCREEN)
	{
		if(m_state.clipped == CS_ON)
			ClipCursor(&ScaleRectClient(m_hWnd, m_state.rClippedArea));
		else
		{
			RECT r = {0,0,319,199};
			ClipCursor(&ScaleRectClient(m_hWnd, r));
		}
	}
	else
	{
		if(m_state.clipped == CS_ON)
			ClipCursor(&ScaleRectWindow(m_hWnd, m_state.rClippedArea));
	}
}

void Mouse2::updateClippingRect()
{
	if(m_state.mode == VM_FULLSCREEN)
	{
		if(m_inputState.top() == IS_LIVE)
		{
			if(m_state.clipped == CS_ON)
				ClipCursor(&ScaleRectClient(m_hWnd, m_state.rClippedArea));
			else
			{
				RECT r = {0,0,319,199};
				ClipCursor(&ScaleRectClient(m_hWnd, r));
			}
		}
		else
		{
			RECT r = {0,0,319,199};
			ClipCursor(&ScaleRectClient(m_hWnd, r));
		}
	}
	else
	{
		if(m_inputState.top() == IS_LIVE)
			if(m_state.clipped == CS_ON)
				ClipCursor(&ScaleRectWindow(m_hWnd, m_state.rClippedArea));
	}
}

void Mouse2::pushState(InputState state)
{
	InputState prevState = m_inputState.top();
	m_inputState.push(state);
	if(prevState != state)
	{
		if(state == IS_DEAD)
		{
			m_inputState.top() = IS_LIVE;
			setInputState(IS_DEAD);
		}
		else
		{
			m_inputState.top() = IS_DEAD;
			setInputState(IS_LIVE);
		}
	}
}

void Mouse2::popState()
{
	if(m_inputState.size() == 1)
		return;
	InputState prevState = m_inputState.top();
	m_inputState.pop();
	if(prevState != m_inputState.top())
	{
		if(m_inputState.top() == IS_DEAD)
		{
			m_inputState.top() = IS_LIVE;
			setInputState(IS_DEAD);
		}
		else
		{
			m_inputState.top() = IS_DEAD;
			setInputState(IS_LIVE);
		}
	}
}
