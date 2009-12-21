#include "gfx_directx_cls_font.h"
using namespace gfx;

Font::Font() : m_dwStyle(0)
{
	::ZeroMemory(&m_rBox, sizeof(m_rBox));
	::ZeroMemory(&m_fontDesc, sizeof(m_fontDesc));
}

Font::~Font()
{
	m_font = NULL;
	m_d3ddev = NULL;
}

int Font::Initialize(HWND hWnd, IDirect3DDevice9 *d3ddev, TCHAR *strFontName, float percentWidthToWindow, float percentHeightToWindow)
{//hardcoded, need to adjust; see msdn LOGFONT structure for more info
	D3DXFONT_DESC fd = {
		20, //height
		0, //width, setting 0 enables default aspect ratio
		FW_DONTCARE, //weight
		D3DX_DEFAULT, //miplevels
		0, //italic (true/false)
		DEFAULT_CHARSET, //character set
		OUT_DEFAULT_PRECIS, //output precision
		DEFAULT_QUALITY, //quality
		DEFAULT_PITCH | FF_DONTCARE, //pitch and family
		TEXT("Times New Roman"), //face name
	};
	return Initialize(hWnd, d3ddev, &fd);
}

int Font::Initialize(HWND hWnd, IDirect3DDevice9 *d3ddev, D3DXFONT_DESC *pFontDesc)
{
	m_bInitialized = false;
	m_font = NULL;
	m_d3ddev = NULL;
	if(!hWnd || !d3ddev || !pFontDesc)
		return -1;
	m_hWnd = hWnd;
	m_d3ddev = d3ddev;
	m_fontDesc = *pFontDesc;
	::D3DXCreateFontIndirect(m_d3ddev, &m_fontDesc, &m_font);
	m_bInitialized = true;
	return 0;
}

void Font::PrintA(char *strMessage)
{
	if(!m_bInitialized)
		return;
	if(strMessage == NULL)
		return;
	m_font->DrawTextA(0, strMessage, -1, &m_rBox, m_dwStyle, m_dwColor);
}

void Font::PrintW(wchar_t *strMessage)
{
	if(!m_bInitialized)
		return;
	if(strMessage == NULL)
		return;
	m_font->DrawTextW(0, strMessage, -1, &m_rBox, m_dwStyle, m_dwColor);
}

void Font::Print(TCHAR *strMessage)
{
	if(strMessage == NULL)
		return;
#ifdef _UNICODE
	PrintW(strMessage);
#else
	PrintA(strMessage);
#endif
}

void Font::SetBox(RECT *rBox)
{
	if(rBox)
		m_rBox = *rBox;
}

void Font::SetBox(int left, int top, int right, int bottom)
{
	RECT r = {left, top, right, bottom};
	SetBox(&r);
}

void Font::SetStyle(DWORD dwStyle)
{
	m_dwStyle = dwStyle;
}

void Font::SetColor(DWORD argb)
{
	m_dwColor = argb;
}

DWORD Font::GetColor()
{
	return m_dwColor;
}

DWORD Font::GetStyle()
{
	return m_dwStyle;
}

RECT Font::GetBox()
{
	return m_rBox;
}

RECT& Font::GetBoxRef()
{
	return m_rBox;
}

D3DXFONT_DESC& Font::GetDescriptionRef()
{
	return m_fontDesc;
}

void Font::OnLostDevice()
{
	if(m_font != NULL)
		m_font->OnLostDevice();
}

void Font::OnResetDevice()
{
	if(m_font != NULL)
		m_font->OnResetDevice();
}