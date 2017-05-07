#include "mouse.hpp"

using namespace gfx;

Mouse::Mouse() : m_wheel(0), m_hWnd(NULL), m_pDirectX(NULL)
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

void Mouse::initialize(gfx::D3D *pDirectX)
{
	m_pDirectX = pDirectX;
}

void Mouse::startClickInducedClipping()
{
	if(m_state.mode == VM_WINDOWED && m_state.clipped == CS_OFF)
	{
		m_state.buttonClipped = CS_ON;
		ClipCursor(&ScaleRectClient(m_state.rButtonClippedArea));
	}
}

void Mouse::endClickInducedClipping()
{
	if(!m_buttons.isAnyDown() && m_state.clipped == CS_OFF && m_state.buttonClipped == CS_ON)
	{
		m_state.buttonClipped = CS_OFF;
		ClipCursor(NULL);
	}
}

bool Mouse::processMessage(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{//if m_state.clipped == CS_ON, mouse clicks do not engage the m_state.buttonClipped
	m_hWnd = hWnd;
	if(m_inputState.top() == IS_DEAD)
		return false;

	// I don't know whether it is actually necessary to ignore non-client area click events
	// (e.g. WM_NCLBUTTONDOWN) while windowed; preserving previous logic

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
			if(m_state.mode == VM_WINDOWED)
					return false;
	case WM_MOUSEMOVE:
		{
			updatePosition();
		} break;
	case WM_NCLBUTTONDOWN:
			if(m_state.mode == VM_WINDOWED)
					return false;
	case WM_LBUTTONDOWN:
		{
			if(!m_buttons.isLeftDown())
			{
				m_buttons.setLeftDown();
				startClickInducedClipping();
			}
		} break;
	case WM_NCLBUTTONUP:
			if(m_state.mode == VM_WINDOWED)
					return false;
	case WM_LBUTTONUP:
		{
			if(m_buttons.isLeftDown())
			{
				m_buttons.setLeftUp();
				endClickInducedClipping();
			}
		} break;
	case WM_NCRBUTTONDOWN:
			if(m_state.mode == VM_WINDOWED)
					return false;
	case WM_RBUTTONDOWN:
		{
			if(!m_buttons.isRightDown())
			{
				m_buttons.setRightDown();
				startClickInducedClipping();
			}
		} break;
	case WM_NCRBUTTONUP:
			if(m_state.mode == VM_WINDOWED)
					return false;
	case WM_RBUTTONUP:
		{
			if(m_buttons.isRightDown())
			{
				m_buttons.setRightUp();
				endClickInducedClipping();
			}
		} break;
	case WM_NCMBUTTONDOWN:
			if(m_state.mode == VM_WINDOWED)
					return false;
	case WM_MBUTTONDOWN:
		{
			if(!m_buttons.isMiddleDown())
			{
				m_buttons.setMiddleDown();
				startClickInducedClipping();
			}
		} break;
	case WM_NCMBUTTONUP:
			if(m_state.mode == VM_WINDOWED)
					return false;
	case WM_MBUTTONUP:
		{
			if(m_buttons.isMiddleDown())
			{
				m_buttons.setMiddleUp();
				endClickInducedClipping();
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

void Mouse::setInputState(InputState state)
{//this should be the ultimate adjuster!
	if(m_inputState.top() == state)
		return;
	m_inputState.top() = state;
	if(state == IS_LIVE)
	{
		if(m_state.mode == VM_FULLSCREEN)
		{
			if(m_state.clipped == CS_ON)
				ClipCursor(&ScaleRectClient(m_state.rClippedArea));
			else
			{
				RECT r = {0,0,319,199};
				ClipCursor(&ScaleRectClient(r));
			}
		}
		else
		{
			if(m_state.clipped == CS_ON)
				ClipCursor(&ScaleRectClient(m_state.rClippedArea));
		}
	}
	else
	{
		if(m_state.mode == VM_FULLSCREEN)
		{
			RECT r = {0,0,319,199};
			ClipCursor(&ScaleRectClient(r));
		}
		else
		{
			ClipCursor(NULL);
		}
	}
	updateCursorVisibility();
}

// Called when (possibly) switching between windowed/fullscreen
void Mouse::setVideoMode(VideoMode mode)
{//VideoMode has precedence over InputState for the cursor visibility and certain clipping
	if(m_state.mode == mode)
		return;
	m_state.mode = mode;
	if(m_state.mode == VM_FULLSCREEN)
	{
		if(m_inputState.top() == IS_DEAD)
		{
			RECT r = {0,0,319,199};
			ClipCursor(&ScaleRectClient(r));
		}
		else
		{
			if(m_state.clipped == CS_ON)
				ClipCursor(&ScaleRectClient(m_state.rClippedArea));
			else
			{
				RECT r = {0,0,319,199};
				ClipCursor(&ScaleRectClient(r));
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
				ClipCursor(&ScaleRectClient(m_state.rClippedArea));
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

// WM_MOUSEMOVE event, convert mouse position to engine coords
void Mouse::updatePosition()
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

// Client changes the mouse cursor position
int Mouse::setPosition(int x, int y)
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

//scales rect from range inside (0,0,319,199) to the window's client rect
RECT Mouse::ScaleRectClient(const RECT& rSrc)
{
	RECT rWin;
	//GetWindowRect(m_hWnd, &rWin);
	GetClientRect(m_hWnd, &rWin);
	POINT pnt1 = {1,1}, pnt2 = {rWin.right-1,rWin.bottom-1};
	ClientToScreen(m_hWnd, &pnt1);
	//ScreenToClient(m_hWnd, &pnt2);

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

// Decide whether the cursor should be visible, and enact.
void Mouse::updateCursorVisibility()
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

void Mouse::setCursorVisibility(CursorVisibility visibility)
{
	if(m_state.visibility == visibility)
		return;
	m_state.visibility = visibility;
	if(m_inputState.top() == IS_DEAD)
		return;
	updateCursorVisibility();
}

void Mouse::setClipState(ClipState state)
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
			ClipCursor(&ScaleRectClient(m_state.rClippedArea));
		}
		else
		{
			RECT r = {0,0,319,199};
			ClipCursor(&ScaleRectClient(r));
		}
	}
	else
	{
		if(m_state.clipped == CS_ON)
		{
			ClipCursor(&ScaleRectClient(m_state.rClippedArea));
		}
		else
		{
			ClipCursor(NULL);
		}
	}
}

void Mouse::setClippingRect(RECT *pRect)
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
			ClipCursor(&ScaleRectClient(m_state.rClippedArea));
		else
		{
			RECT r = {0,0,319,199};
			ClipCursor(&ScaleRectClient(r));
		}
	}
	else
	{
		if(m_state.clipped == CS_ON)
			ClipCursor(&ScaleRectClient(m_state.rClippedArea));
	}
}

void Mouse::updateClippingRect()
{
	if(m_state.mode == VM_FULLSCREEN)
	{
		if(m_inputState.top() == IS_LIVE)
		{
			if(m_state.clipped == CS_ON)
				ClipCursor(&ScaleRectClient(m_state.rClippedArea));
			else
			{
				RECT r = {0,0,319,199};
				ClipCursor(&ScaleRectClient(r));
			}
		}
		else
		{
			RECT r = {0,0,319,199};
			ClipCursor(&ScaleRectClient(r));
		}
	}
	else
	{
		if(m_inputState.top() == IS_LIVE)
			if(m_state.clipped == CS_ON)
				ClipCursor(&ScaleRectClient(m_state.rClippedArea));
	}
}

// This is called when some on-going condition (e.g. losing focus) causes the mouse state
// to be forced to dead/alive (in practice always called with IS_DEAD).
void Mouse::pushState(InputState state)
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

// Called when a condition that changed mouse state ends.
void Mouse::popState()
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
