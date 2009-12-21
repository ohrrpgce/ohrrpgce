#include "gfx_directx_cls_fps.h"
using namespace gfx;

FPSDisplay::FPSDisplay() : m_nFrameCount(0), m_nFramePerSecond(0)
{
}

FPSDisplay::~FPSDisplay()
{
	m_font.Initialize(0,0,0);
}

int FPSDisplay::Initialize(HWND hWnd, IDirect3DDevice9 *d3ddev)
{
	m_font.SetBox(0,0,100,100);
	m_font.SetStyle(DT_NOCLIP);
	m_font.SetColor(0xffffffff);
	return m_font.Initialize(hWnd, d3ddev, TEXT("Times New Roman"), 1.0f, 1.0f);

}

void FPSDisplay::IncrementFrameCount()
{
	m_nFrameCount++;
	if(m_counter.GetTimeDifferential() > 1000)
	{
		m_nFramePerSecond = m_nFrameCount;
		m_nFrameCount = 0;
		m_counter.Update();
	}
}

void FPSDisplay::DrawFps()
{
	char str[64] = "";
	::sprintf_s<64>(str, "Avg FPS: %d", m_nFramePerSecond);
	m_font.PrintA(str);
}

void FPSDisplay::OnLostDevice()
{
	m_font.OnLostDevice();
}

void FPSDisplay::OnResetDevice()
{
	m_font.OnResetDevice();
}