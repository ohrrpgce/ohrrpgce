#include "guiButton.h"

CGuiButton::CGuiButton()
: CGuiObject()
{
}

CGuiButton::~CGuiButton()
{
}

int CGuiButton::proc(unsigned int msg, unsigned int param1, void *param2)
{
	switch(msg)
	{
	case GUI_CREATE:
		{
			CGuiObject::proc( msg, param1, param2 );

			//VertexData v[] = {
			//	CVertexData(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0xaa3355ee),
			//	CVertexData(1.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0xaa3355ee),
			//	CVertexData(1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0xaa2244aa),
			//	CVertexData(0.0f, 1.0f, 0.0f, 0.0f, 1.0f, 0xaa2244aa),
			//};
			//graphic()->vertexCreate( 4, GRU_dynamic, v, &m_vb );

			//GuiCreationParams* pgcp = reinterpret_cast<GuiCreationParams*>(param2);
			//if(pgcp != 0)
			//{
			//	//if(pgcp->dwFlags & GBS_TEXT) //this should always be the case
			//	{
					GuiCreationParams cp = {0};
					cp.text = text().c_str();
					cp.dwFlags = GTS_HALIGN_CENTER | GTS_CLIP_TEXT;
					//cp.rect.left = 0;
					//cp.rect.top = 0;
					//cp.rect.right = aabb().right-aabb().left;
					//cp.rect.bottom = aabb().bottom-aabb().top;
					cp.x = 0;
					cp.y = 0;
					cp.width = aabb().right - aabb().left;
					cp.height = aabb().bottom - aabb().top;
					createChild( "text", &cp );
			//	}
			//}
		} break;
	case GUI_DESTROY:
		{
			//graphic()->vertexDestroy( m_vb );
			//m_vb = 0;

			CGuiObject::proc(msg, param1, param2);
		} break;
	case GUI_PAINT:
		{
			preparePaint();

			if( isMouseOver() )
			{
				//for(int i = 0; i < m_vb->length; i++)
				//	m_vb->pData.vertex[i].c = (m_vb->pData.vertex[i].c & 0x00ffffff) | 0xff000000;
			}
			else
			{
				//for(int i = 0; i < m_vb->length; i++)
				//	m_vb->pData.vertex[i].c = (m_vb->pData.vertex[i].c & 0x00ffffff) | 0xaa000000;
			}
			//m_vb->pData.vertex[0].x = (float)aabb().left;
			//m_vb->pData.vertex[0].y = (float)aabb().top;
			//m_vb->pData.vertex[1].x = (float)aabb().right;
			//m_vb->pData.vertex[1].y = (float)aabb().top;
			//m_vb->pData.vertex[2].x = (float)aabb().right;
			//m_vb->pData.vertex[2].y = (float)aabb().bottom;
			//m_vb->pData.vertex[3].x = (float)aabb().left;
			//m_vb->pData.vertex[3].y = (float)aabb().bottom;

			//graphic()->vertexUpdate( 0, 0, m_vb );

			//memcpy( m_cb->pData.cb, reinterpret_cast<Matrix*>(param2)->m, m_cb->stepSize * m_cb->length );

			//graphic()->setVertexBuffer( m_vb );
			//graphic()->setIndexBuffer( m_ib );
			//graphic()->setConstantBuffer( m_cb );
			//graphic()->setShader( S_simpleColor );
			//graphic()->render();

			CGuiObject::proc(msg, param1, param2);
		} break;
	case GUI_SIZE:
		{
			CGuiObject::proc(msg, param1, param2);
			sendMessage( childID(0), msg, param1, param2 ); //force text to be the same as the button
		} break;
	case GUI_MOUSE_LCLICK:
		sendMessage( parentID(), GUI_COMMAND, commandValue(), (void*)thisID() );
		CGuiObject::proc(msg, param1, param2);
		break;
	case GUI_FOCUS:
		return sendMessage( parentID(), msg, param1, param2 );
	case GUI_IS_MOVABLE_BY_MOUSE:
		return 0;
	case GUI_SET_TEXT:
		return sendMessage( childID(0), msg, param1, param2 );
	case GUI_GET_TEXT:
		return sendMessage( childID(0), msg, param1, param2 );
	default:
		return CGuiObject::proc(msg, param1, param2);
	}
	return 0;
}

CGuiButtonFactory::CGuiButtonFactory()
: m_manager(0)/*, m_ib(0), m_cb(0)*/
{
}

CGuiButtonFactory::~CGuiButtonFactory()
{
	m_control.clear();

	if(m_manager != 0)
	{
		//m_manager->graphic()->indexDestroy( m_ib );
		//m_ib = 0;
		//m_manager->graphic()->constantDestroy( m_cb );
		//m_cb = 0;
	}

	m_manager = 0;
}

GuiObject* CGuiButtonFactory::createInstance()
{
	m_control.push_back( CGuiButton( m_ib, m_cb ) );
	return (GuiObject*)&m_control.back();
}

void CGuiButtonFactory::destroyInstance(GuiObject *pObject)
{
	if(pObject == 0)
		return;
	for(std::list<CGuiButton>::iterator i = m_control.begin(); i != m_control.end(); i++)
	{
		if( pObject == (GuiObject*)&(*i) )
		{
			m_control.erase( i );
			break;
		}
	}
}

void CGuiButtonFactory::notifyRegister(GuiManager *pManager)
{
	m_manager = pManager;
	if(m_manager == 0)
		return;

	//IndexData id[6] = {
	//	0, 1, 2,
	//	2, 3, 0,
	//};

	//m_manager->graphic()->indexCreate( 6, GRU_immutable, id, &m_ib );
	//m_manager->graphic()->constantCreate( SIR_wvp, 0, &m_cb );
}

void CGuiButtonFactory::notifyUnregister()
{
	m_control.clear();

	if(m_manager != 0)
	{
		//m_manager->graphic()->indexDestroy( m_ib );
		//m_ib = 0;
		//m_manager->graphic()->constantDestroy( m_cb );
		//m_cb = 0;
	}

	m_manager = 0;
}

const char* CGuiButtonFactory::szClass()
{
	return "button";
}