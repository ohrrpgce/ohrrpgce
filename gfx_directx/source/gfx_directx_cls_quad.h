//gfx_directx_cls_quad.h
//by Jay Tennant 10/30/09
//manages quad development and drawing

#ifndef GFX_DIRECTX_CLS_QUAD_H
#define GFX_DIRECTX_CLS_QUAD_H

#include <windows.h>
#include <d3d9.h>
#include <d3dx9.h>
#include "smartptr.h"

namespace gfx
{
	class Quad
	{
	protected:
		SmartPtr<IDirect3DDevice9> m_d3ddev;
		SmartPtr<IDirect3DVertexDeclaration9> m_vertDecl;
		SmartPtr<IDirect3DVertexBuffer9> m_vBuffer;
		struct VertexData
		{
			float x,y,z,w; //homogenous screen space
			float u,v; //texture coordinates
		} m_vertices[4];
	public:
		Quad();
		virtual ~Quad();

		int Initialize(IDirect3DDevice9* d3ddev, int width, int height);
		void AspectPadding(int screenWidth, int screenHeight);
		void DrawQuad();
		void PrepareQuad();
		void CalculateRect(RECT *pRect); //calculates rect that image is drawn within; rect specifies client rect in screen space (or in client space)

		void OnLostDevice();
		void OnResetDevice();
	};
}

#endif