//gfx_directx_cls_font.h
//by Jay Tennant 10/30/09
//manages font interface

#ifndef GFX_DIRECTX_CLS_FONT_H
#define GFX_DIRECTX_CLS_FONT_H


#include <windows.h>
#include <d3d9.h>
#include <d3dx9.h>
#include "smartptr.h"

namespace gfx
{
	class Font
	{
	protected:
		SmartPtr<IDirect3DDevice9> m_d3ddev;
		SmartPtr<ID3DXFont> m_font;
		D3DXFONT_DESC m_fontDesc;
		HWND m_hWnd;
		RECT m_rBox;
		DWORD m_dwStyle; //centered, left, right, clip, etc.
		DWORD m_dwColor;
		bool m_bInitialized;
	public:
		Font();
		virtual ~Font();

		int Initialize(HWND hWnd, IDirect3DDevice9* d3ddev, TCHAR* strFontName, float percentWidthToWindow, float percentHeightToWindow);
		int Initialize(HWND hWnd, IDirect3DDevice9* d3ddev, D3DXFONT_DESC* pFontDesc);
		void PrintA(char* strMessage);
		void PrintW(wchar_t* strMessage);
		void Print(TCHAR* strMessage); //resolves to PrintA if ansi, PrintW if unicode
		void SetBox(RECT* rBox); //sets font box, where text is inside of box
		void SetBox(int left, int top, int right, int bottom); //sets font box, where text is inside of box
		void SetStyle(DWORD dwStyle); //sets font style, ie. DT_CENTER, DT_LEFT, DT_RIGHT, DT_NOCLIP, etc.
		void SetColor(DWORD argb);

		DWORD GetColor(); //returns argb color
		DWORD GetStyle(); //returns style, ie. DT_CENTER, DT_NOCLIP, etc.
		RECT GetBox(); //returns font box, where text is drawn
		RECT& GetBoxRef(); //returns reference to font box, where text is drawn
		D3DXFONT_DESC& GetDescriptionRef(); //returns font description reference

		void OnLostDevice();
		void OnResetDevice();
	};
}

#endif