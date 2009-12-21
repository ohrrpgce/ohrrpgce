#include "gfx_directx_cls_quad.h"
using namespace gfx;

Quad::Quad()
{
	::ZeroMemory(m_vertices, sizeof(m_vertices));
}

Quad::~Quad()
{
	m_vBuffer = NULL;
	m_vertDecl = NULL;
	m_d3ddev = NULL;
}

int Quad::Initialize(IDirect3DDevice9 *d3ddev, int width, int height)
{
	m_vertDecl = NULL;
	m_d3ddev = d3ddev;

	if(m_d3ddev == NULL)
		return -1;

	D3DVERTEXELEMENT9 vElem[] =
	{
		{0, 0, D3DDECLTYPE_FLOAT4, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0},
		{0, 16, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0},
		D3DDECL_END()
	};

	m_d3ddev->CreateVertexDeclaration(vElem, &m_vertDecl);
	if(m_vertDecl == NULL)
		return -1;
	float fw = 320.0f / (float)width;
	float fh = 200.0f / (float)height;
	VertexData vd[4] =
	{
		{-1,1,0,1,0,0},
		{1,1,0,1,fw,0},
		{-1,-1,0,1,0,fh},
		{1,-1,0,1,fw,fh},
	};
	::memcpy_s((void*)m_vertices, sizeof(m_vertices), (void*)vd, sizeof(vd));
	OnResetDevice();
	if(m_vBuffer == NULL)
		return -1;
	return 0;
}

void Quad::AspectPadding(int screenWidth, int screenHeight)
{
	float scrnAspect = (float)screenWidth / (float)screenHeight;
	float frameAspect = 320.0f / 200.0f;
	float xScale, yScale;
	if(frameAspect < scrnAspect)
	{
		yScale = 1.0f;
		xScale = frameAspect / scrnAspect;
	}
	else
	{
		xScale = 1.0f;
		yScale = scrnAspect / frameAspect;
	}
	m_vertices[0].x = -xScale;
	m_vertices[0].y = yScale;
	m_vertices[1].x = xScale;
	m_vertices[1].y = yScale;
	m_vertices[2].x = -xScale;
	m_vertices[2].y = -yScale;
	m_vertices[3].x = xScale;
	m_vertices[3].y = -yScale;
	VertexData *pData;
	m_vBuffer->Lock(0, 0, (void**)&pData, 0);
	::memcpy_s((void*)pData, sizeof(m_vertices), (void*)m_vertices, sizeof(m_vertices));
	m_vBuffer->Unlock();
}

void Quad::DrawQuad()
{
	if(m_d3ddev == NULL)
		return;
	HRESULT hr = S_OK;
	hr = m_d3ddev->DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);
}

void Quad::PrepareQuad()
{
	if(m_d3ddev == NULL || m_vertDecl == NULL)
		return;
	HRESULT hr = m_d3ddev->SetVertexDeclaration(m_vertDecl);
	hr = m_d3ddev->SetStreamSource(0, m_vBuffer, 0, sizeof(VertexData));
}

void Quad::CalculateRect(RECT *pRect)
{
	if(pRect == NULL)
		return;
	LONG w = pRect->right - pRect->left;
	LONG h = pRect->bottom - pRect->top;
	LONG xCenter = (pRect->left + pRect->right) / 2;
	LONG yCenter = (pRect->top + pRect->bottom) / 2;
	RECT r = {0};
	r.left = xCenter + (LONG)(m_vertices[0].x * (float)w / 2.0f);
	r.bottom = yCenter + (LONG)(m_vertices[0].y * (float)h / 2.0f);
	r.right = xCenter + (LONG)(m_vertices[3].x * (float)w / 2.0f);
	r.top = yCenter + (LONG)(m_vertices[3].y * (float)h / 2.0f);
	*pRect = r;
}

void Quad::OnLostDevice()
{
	m_vBuffer = NULL;
}

void Quad::OnResetDevice()
{
	m_d3ddev->CreateVertexBuffer(sizeof(m_vertices), D3DUSAGE_DYNAMIC | D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &m_vBuffer, 0);
	if(m_vBuffer == NULL)
		return;
	VertexData *pData;
	m_vBuffer->Lock(0, 0, (void**)&pData, 0);
	::memcpy_s((void*)pData, sizeof(m_vertices), (void*)m_vertices, sizeof(m_vertices));
	m_vBuffer->Unlock();
}