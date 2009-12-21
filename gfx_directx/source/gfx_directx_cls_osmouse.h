//gfx_directx_cls_osmouse.h
//by Jay Tennant 12/4/09
//manages os mouse state for clip cursor, visibility
//the push count is in place to cover in case the window loses focus, has a menu accessed, etc.
//if push count is 0, OSMouse will carry out activity; if push count != 0, OSMouse will suspend mouse control activity

#ifndef GFX_DIRECTX_CLS_OSMOUSE_H
#define GFX_DIRECTX_CLS_OSMOUSE_H

#include <windows.h>

namespace gfx
{
	class OSMouse
	{
	protected:
		HWND m_hWnd;
		RECT m_rClip;
		RECT m_rPrevClip;
		bool m_bInitialized;
		bool m_bOHRMouseActive;
		bool m_bFullscreen;
		bool m_bHidden;
		UINT m_nPushCount;
	public:
		OSMouse();
		virtual ~OSMouse();

		int Initialize(HWND hWnd); //initializes and sets window handle that os mouse will be tied to
		void Free(); //"un-initializes" OSMouse

		void Push_State(); //increments the push count on the os mouse state by 1
		void Pop_State(); //decrements the push count on the os mouse state by 1

		void OHRMouseActive(bool bActive); //sets whether ohr mouse is active
		void Fullscreen(bool bFullscreen); //sets whether d3d is in fullscreen
		void UpdateClientRect(); //updates the clip cursor position

		bool IsOHRMouseActive();
		bool IsFullscreen();
		UINT GetPushCount();
	};
}

#endif