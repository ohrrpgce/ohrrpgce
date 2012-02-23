#include "guiBase.h"

GuiManager::GuiManager()
: m_objectIDGeneratorCounter(1), m_focusedObject(GUIC_INVALID), m_movingObject(GUIC_INVALID)
{
	memset( &m_mouseSnapshot, 0, sizeof(m_mouseSnapshot) );
	memset( &m_keyboardSnapshot, 0, sizeof(m_keyboardSnapshot) );
}

GuiManager::~GuiManager()
{
	shutdown();
}

void GuiManager::postInputMessage( unsigned int msg, unsigned int param1, void* param2 )
{

	switch( msg ) {
		case GUI_MOUSE_LBUTTON_DOWN:
			{
				//copy the mouse state of param2 to the snapshot

				//if button already down,
					//send input to focused control
				//else
					//set the focus control to the one that passes the HITTEST
					//if that control can move
						//set it as the moving object
			}
			break;
		case GUI_MOUSE_LBUTTON_UP:
			{
				//copy the mouse state of param2 to the snapshot

				//send the notification to the focus control
				//if there was a moving object
					//release the moving object

				//if the release of the button was over the focused control (HITTEST),
				//and the pressing of the button was over the focused control,
					//emit a CLICK message to that control
			}
			break;
		case GUI_MOUSE_RBUTTON_DOWN:
			{
				//copy the mouse state of param2 to the snapshot

				//send input to focused control
			}
			break;
		case GUI_MOUSE_RBUTTON_UP:
			{
				//copy the mouse state of param2 to the snapshot

				//send input to focused control

				//if the release of the button was over the focused control (HITTEST),
				//and the pressing of the button was over the focused control,
					//emit a CLICK message to that control
			}
			break;
		case GUI_MOUSE_MBUTTON_DOWN:
			{
				//copy the mouse state of param2 to the snapshot

				//send input to focused control
			}
			break;
		case GUI_MOUSE_MBUTTON_UP:
			{
				//copy the mouse state of param2 to the snapshot

				//send input to focused control

				//if the release of the button was over the focused control (HITTEST),
				//and the pressing of the button was over the focused control,
					//emit a CLICK message to that control
			}
			break;
		case GUI_MOUSE_MOVE:
			{
				//copy the mouse state of param2 to the snapshot

				//if there is a moving object
					//send the message to that control
				//else
					//send the message to the control that passes the HITTEST and
					//the control that previously received the MOUSE_MOVE message
					//if the control that passes the HITTEST is not the same as the
					//previous control that received the MOUSE_MOVE message
						//emit a MOUSE_OVER message, with a 'true' value, to the control that passes the HITTEST
						//emit a MOUSE_OVER message, with a 'false' value, to the previous control that received MOUSE_MOVE messages
			}
			break;
		case GUI_KEY_DOWN:
			{
				//issue the message to the focused control
			}
			break;
		case GUI_KEY_UP:
			{
				//issue the message to the focused control
				//also emit a TOUCH message to that control
			}
			break;
		case GUI_CHAR:
			{
				//issue the message to the focused control
			}
			break;
		//the next few messages generally are only if emulated click's and touch's are sent
		case GUI_MOUSE_LBUTTON_CLICK:
		case GUI_MOUSE_RBUTTON_CLICK:
		case GUI_MOUSE_MBUTTON_CLICK:
		case GUI_KEY_TOUCH:
			{
				//issue the message to the focused control
			}
			break;
	}

	//int mouseCoord[2] = {0,0};
	//int depth = 0, depthTest = 0;

	////if moving still, that object receives mouse data
	//if( m_movingObject != GUIC_INVALID )
	//{
	//	depth = sendMessage( m_movingObject, GUI_GET_ZORDER, 0, 0 );
	//}
	////else find the object depth that would receive mouse data, exception being mouse-over messages, which goes to everything
	//else
	//{
	//	for(ObjectMap::iterator i = m_object.begin(); i != m_object.end(); i++)
	//	{
	//		depthTest = sendMessage( i->first, GUI_HITTEST, depth, mouseCoord );
	//		if(depthTest >= depth)
	//		{
	//			depth = depthTest;
	//		}
	//	}
	//}

	////post mouse over messages to all objects, and mouse button messages only to those that pass the hittest
	//Message msg = { GUIC_INVALID, GUI_MOUSE_OVER, 0, &m_mouseSnapshot, false };
	//for(ObjectMap::iterator i = m_object.begin(); i != m_object.end(); i++)
	//{
	//	if( (sendMessage( i->first, GUI_HITTEST, depth, mouseCoord ) && m_movingObject == GUIC_INVALID) || m_movingObject == i->first )
	//	{
	//		msg.param1 = 1;
	//		msg.controlID = i->first;
	//		postMessage( &msg );

	//		Message buttonNotification = { i->first, 0, 0, msg.param2, false };
	//		if(m_input->mouse().bLeftDown) //mouse left button down
	//		{
	//			buttonNotification.msg = GUI_MOUSE_LBUTTON_DOWN;

	//			if(m_mouseSnapshot.bLeftDown) //mouse left button held
	//				buttonNotification.param1 = GUIC_BUTTON_DOWN;
	//			else //newly pressed
	//			{
	//				focus( i->first );
	//				buttonNotification.param1 = GUIC_BUTTON_UP;
	//				if( sendMessage( i->first, GUI_IS_MOVABLE_BY_MOUSE, 0, 0 ) == 1 )
	//					m_movingObject = i->first;
	//			}

	//			postMessage( &buttonNotification );
	//			if(m_movingObject != GUIC_INVALID)
	//			{
	//				Message moveNote;
	//				moveNote.controlID = i->first;
	//				moveNote.msg = GUI_MOVE;
	//				moveNote.param1 = 0;
	//				allocateMessageSpace( 2*sizeof(long), &moveNote );
	//				long d[2] = {mouseCoord[0] - m_mouseSnapshot.px, mouseCoord[1] - m_mouseSnapshot.py};
	//				GuiRect parentRect = {0}, previousRect = {0};
	//				unsigned int parentID = sendMessage( i->first, GUI_GET_PARENT, 0, 0 );
	//				if( parentID != GUIC_INVALID )
	//				{
	//					sendMessage( parentID, GUI_GET_ABS_POSITION, 0, &parentRect );
	//					d[0] += parentRect.left;
	//					d[1] += parentRect.top;
	//				}
	//				sendMessage( i->first, GUI_GET_REL_POSITION, 0, &previousRect );
	//				d[0] += previousRect.left;
	//				d[1] += previousRect.top;

	//				memcpy( moveNote.param2, d, sizeof(d) );
	//				postMessage( &moveNote );
	//			}
	//		}
	//		else if(m_mouseSnapshot.bLeftDown) //mouse left button release
	//		{
	//			buttonNotification.msg = GUI_MOUSE_LBUTTON_UP;
	//			buttonNotification.param1 = GUIC_BUTTON_DOWN;

	//			postMessage( &buttonNotification );
	//			if(m_movingObject != GUIC_INVALID)
	//			{
	//				Message moveNote;
	//				moveNote.controlID = i->first;
	//				moveNote.msg = GUI_MOVE;
	//				moveNote.param1 = 0;
	//				allocateMessageSpace( 2*sizeof(long), &moveNote );
	//				long d[2] = {mouseCoord[0] - m_mouseSnapshot.px, mouseCoord[1] - m_mouseSnapshot.py};
	//				GuiRect parentRect = {0}, previousRect = {0};
	//				unsigned int parentID = sendMessage( i->first, GUI_GET_PARENT, 0, 0 );
	//				if( parentID != GUIC_INVALID )
	//				{
	//					sendMessage( parentID, GUI_GET_ABS_POSITION, 0, &parentRect );
	//					d[0] += parentRect.left;
	//					d[1] += parentRect.top;
	//				}
	//				sendMessage( i->first, GUI_GET_REL_POSITION, 0, &previousRect );
	//				d[0] += previousRect.left;
	//				d[1] += previousRect.top;

	//				memcpy( moveNote.param2, d, sizeof(d) );
	//				postMessage( &moveNote );
	//				m_movingObject = GUIC_INVALID;
	//			}
	//		}
	//		if(m_input->mouse().bRightDown) //mouse right button down
	//		{
	//			buttonNotification.msg = GUI_MOUSE_RBUTTON_DOWN;

	//			if(m_mouseSnapshot.bRightDown) //mouse right button held
	//				buttonNotification.param1 = GUIC_BUTTON_DOWN;
	//			else //newly pressed
	//			{
	//				focus( i->first );
	//				buttonNotification.param1 = GUIC_BUTTON_UP;
	//			}

	//			postMessage( &buttonNotification );
	//		}
	//		else if(m_mouseSnapshot.bRightDown) //mouse right button release
	//		{
	//			buttonNotification.msg = GUI_MOUSE_RBUTTON_UP;
	//			buttonNotification.param1 = GUIC_BUTTON_DOWN;

	//			postMessage( &buttonNotification );
	//		}
	//		if(m_input->mouse().bMiddleDown) //mouse middle button down
	//		{
	//			buttonNotification.msg = GUI_MOUSE_MBUTTON_DOWN;

	//			if(m_mouseSnapshot.bMiddleDown) //mouse middle button held
	//				buttonNotification.param1 = GUIC_BUTTON_DOWN;
	//			else //newly pressed
	//			{
	//				focus( i->first );
	//				buttonNotification.param1 = GUIC_BUTTON_UP;
	//			}

	//			postMessage( &buttonNotification );
	//		}
	//		else if(m_mouseSnapshot.bMiddleDown) //mouse middle button release
	//		{
	//			buttonNotification.msg = GUI_MOUSE_MBUTTON_UP;
	//			buttonNotification.param1 = GUIC_BUTTON_DOWN;

	//			postMessage( &buttonNotification );
	//		}
	//	}
	//	else
	//	{
	//		msg.param1 = 0;
	//		msg.controlID = i->first;
	//		postMessage( &msg );
	//	}
	//}

	////get the keyboard context
	//GuiKey key;
	//key.shift = m_input->keyboard().keys[ IS_SHIFT ];
	//key.ctrl = m_input->keyboard().keys[ IS_CONTROL ];
	//key.alt = m_input->keyboard().keys[ IS_ALT ];

	////intentionally starting with 1, because 0 is IS_INVALID
	//for(unsigned int i = 1; i < IS_SCANCODE_COUNT; i++)
	//{
	//	if( m_input->keyboard().keys[i] != 0 ) //key is down
	//	{
	//		if( m_keyboardSnapshot.keys[i] != 0 ) //key was down
	//		{
	//			key.keyDown = true;
	//			if( i == IS_CAPS_LOCK || i == IS_NUM_LOCK || i == IS_SCROLL_LOCK )
	//				continue; //to avoid multiple toggle key notifications
	//		}
	//		else //key was up
	//		{
	//			key.keyDown = false;
	//		}
	//		key.scancode = i;
	//		Message msg = { m_focusedObject, GUI_KEY_DOWN, (unsigned int)key, 0, false };
	//		postMessage( &msg );
	//	}
	//	else //key is up
	//	{
	//		if( m_keyboardSnapshot.keys[i] != 0 ) //key was down
	//		{
	//			key.keyDown = true;
	//			key.scancode = i;
	//			Message msg = { m_focusedObject, GUI_KEY_UP, (unsigned int)key, 0, false };
	//			postMessage( &msg );
	//		}
	//	}
	//}

	////update the state of the mouse and keyboard snapshots
	//m_mouseSnapshot = m_input->mouse();
	//m_keyboardSnapshot = m_input->keyboard();
}

void GuiManager::dispatchMessage(GuiManager::Message *pMsg)
{
	if(pMsg == 0)
		return;

	sendMessage(pMsg->controlID, pMsg->msg, pMsg->param1, pMsg->param2);
	if(pMsg->allocated)
		freeMessageSpace(pMsg);
}

bool GuiManager::initialize()
{
	shutdown();
	return true;
}

void GuiManager::shutdown()
{
	while(!m_message.empty())
		dispatchMessage( &m_message.front() );
	while(!m_object.empty())
		destroyObject( m_object.begin()->first );
	m_objectUnparented.clear(); //shouldn't be necessary, but...
	while(!m_objectFactory.empty())
		unregisterObjectFactory( m_objectFactory.begin()->first.c_str() );

	memset( &m_mouseSnapshot, 0, sizeof(m_mouseSnapshot) );
	memset( &m_keyboardSnapshot, 0, sizeof(m_keyboardSnapshot) );
	m_objectIDGeneratorCounter = 1;
}

bool GuiManager::registerObjectFactory(GuiObjectFactory* pFactory)
{
	if(pFactory == 0)
		return false;

	char buffer[256] = "";
	for(int i = 0; i < sizeof(buffer) / sizeof(buffer[0]); i++)
	{
		buffer[i] = toupper(pFactory->szClass()[i]);
		if(pFactory->szClass()[i] == 0)
			break;
	}

	if(strlen(buffer) == 0)
		return false;

	ObjectFactoryMapInsertionInput in( buffer, pFactory );
	ObjectFactoryMapInsertionResult out = m_objectFactory.insert( in );

	if(out.second)
		pFactory->notifyRegister(this);

	return out.second;
}

void GuiManager::unregisterObjectFactory(const char *szClass)
{
	if(szClass == 0)
		return;

	char buffer[256] = "";
	for(int i = 0; i < sizeof(buffer) / sizeof(buffer[0]); i++)
	{
		buffer[i] = toupper(szClass[i]);
		if(szClass[i] == 0)
			break;
	}

	if(strlen(buffer) == 0)
		return;

	ObjectFactoryMap::iterator iter = m_objectFactory.find( buffer );
	if(iter == m_objectFactory.end())
		return;

	iter->second->notifyUnregister();
	m_objectFactory.erase( iter );
}

unsigned int GuiManager::createObject(const char *szClass, GuiCreationParams *params)
{
	if(szClass == 0 || params == 0)
		return GUIC_INVALID;

	char buffer[256] = "";
	for(int i = 0; i < sizeof(buffer) / sizeof(buffer[0]); i++)
	{
		buffer[i] = toupper(szClass[i]);
		if(szClass[i] == 0)
			break;
	}

	if(strlen(buffer) == 0)
		return GUIC_INVALID;

	ObjectFactoryMap::iterator iFactory = m_objectFactory.find( buffer );
	if(iFactory == m_objectFactory.end())
		return GUIC_INVALID;

	GuiCreationParams cp = *params;
	cp.manager = this;
	cp.thisID = generateID();

	GuiObject* p = iFactory->second->createInstance();
	if(p == 0)
		return GUIC_INVALID;

	int zOrder = 0;
	if(cp.parentID != 0 && cp.parentID != GUIC_INVALID)
		zOrder = sendMessage(cp.parentID, GUI_GET_ZORDER, 0, 0);
	cp.zOrder = zOrder + 1;

	ObjectAndFactoryTrace pft = { p, iFactory->second };
	ObjectMapInsertionInput in( cp.thisID, pft );
	ObjectMapInsertionResult out = m_object.insert( in );
	if(!out.second)
	{
		iFactory->second->destroyInstance(p);
		return GUIC_INVALID;
	}

	if( p->proc(GUI_CREATE, 0, (void*)&cp) != 0 )
	{
		p->proc(GUI_DESTROY, 0, 0);
		iFactory->second->destroyInstance(p);
		m_object.erase( out.first );
		return GUIC_INVALID;
	}

	if( cp.parentID == GUIC_INVALID || cp.parentID == 0 ) //unparented object
	{
		out = m_objectUnparented.insert( in );
		focus( cp.thisID );
	}
	return cp.thisID;
}

void GuiManager::destroyObject(unsigned int objectID)
{
	ObjectMap::iterator iter = m_object.find( objectID );
	if(iter == m_object.end())
		return;

	if(m_focusedObject == objectID)
		m_focusedObject = GUIC_INVALID;

	iter->second.object->proc( GUI_DESTROY, 0, 0 );
	iter->second.factory->destroyInstance( iter->second.object );
	m_object.erase( iter );

	iter = m_objectUnparented.find( objectID );
	if(iter == m_objectUnparented.end())
		return;

	m_objectUnparented.erase( iter );
}

void GuiManager::pumpMessages()
{
	while(!m_message.empty())
	{
		dispatchMessage( &m_message.front() );
		m_message.pop();
	}
}

int GuiManager::sendMessage(unsigned int id, unsigned int msg, unsigned int param1, void *param2)
{
	ObjectMap::iterator iter = m_object.find( id );
	if(iter == m_object.end())
		return GUIC_INVALID;

	return iter->second.object->proc( msg, param1, param2 );
}

void GuiManager::postMessage(GuiManager::Message *pMsg)
{
	if(pMsg == 0)
		return;

	m_message.push( *pMsg );
}

unsigned int GuiManager::focus(unsigned int id)
{
	unsigned int focusNext = sendMessage( id, GUI_FOCUS, GUIC_FOCUS_GAIN, 0 );
	if(focusNext != m_focusedObject)
	{
		sendMessage( m_focusedObject, GUI_FOCUS, GUIC_FOCUS_LOSE, 0 );
		m_focusedObject = focusNext;
	}
	return m_focusedObject;
}

void GuiManager::allocateMessageSpace(unsigned int numBytes, GuiManager::Message *pMsg)
{
	if(numBytes == 0 || pMsg == 0)
		return;

	pMsg->param2 = new char[numBytes];
	if(pMsg->param2 != 0)
		pMsg->allocated = true;
}

void GuiManager::freeMessageSpace(GuiManager::Message *pMsg)
{
	if(pMsg == 0 || !pMsg->allocated)
		return;

	delete [] reinterpret_cast<char*>(pMsg->param2);
	pMsg->param2 = 0;
}



CGuiObject::CGuiObject()
: m_manager(0), m_thisID(GUIC_INVALID), m_parentID(GUIC_INVALID), m_zOrder(0), m_commandValue(0), m_text(""), 
m_mouseLastXCoord(0), m_mouseLastYCoord(0), m_mouseLButtonDownStartedHere(false), m_mouseRButtonDownStartedHere(false), m_mouseMButtonDownStartedHere(false), m_isMouseOver(false), 
m_isFocused(false), m_lastKeyDepressed(GUIC_INVALID), m_isClippingEnabled(true), m_isPaintPrepared(false)
{
	memset( &m_aabb, 0, sizeof(m_aabb) );
	memset( &m_relativeAabb, 0, sizeof(m_relativeAabb) );
	
	memcpy( m_acceleratorTable, m_defAcceleratorTable, sizeof(m_acceleratorTable) );
	m_mouseHoverDuration.setLength(-1);
}

CGuiObject::~CGuiObject()
{
	for(std::list<unsigned int>::iterator i = m_child.begin(); i != m_child.end(); i++)
		m_manager->destroyObject( *i );
	m_child.clear();
}

char CGuiObject::translateKeypress( GuiKey& k )
{//needs work
	if(k.ctrl || k.alt)
		return GUIC_INVALID;
	if( k.shift )
	{
		return m_acceleratorTable[ IS_SCANCODE_COUNT * 1 + k.scancode ];
	}
	else
	{
		return m_acceleratorTable[ IS_SCANCODE_COUNT * 0 + k.scancode ];
	}
	return GUIC_INVALID;
}

unsigned int CGuiObject::createChild(const char *szClass, GuiCreationParams *params)
{
	params->parentID = m_thisID;
	params->absX = m_aabb.left;
	params->absY = m_aabb.top;
	unsigned int c = m_manager->createObject( szClass, params );
	if(c == GUIC_INVALID)
		return GUIC_INVALID;

	m_child.push_back(c);
	return c;
}

void CGuiObject::destroyChild(unsigned int childID)
{
	for(std::list<unsigned int>::iterator i = m_child.begin(); i != m_child.end(); i++)
		if(*i == childID)
		{
			m_child.erase(i);
			break;
		}
}

void CGuiObject::preparePaint()
{
	if( !m_isPaintPrepared )
	{
		if(m_isClippingEnabled)
			m_manager->pushClipRegion( &m_aabb );
	}
	m_isPaintPrepared = true;
}

void CGuiObject::cleanupPaint()
{
	if( m_isPaintPrepared )
	{
		if(m_isClippingEnabled)
			m_manager->popClipRegion();
	}
	m_isPaintPrepared = false;
}

int CGuiObject::proc(unsigned int msg, unsigned int param1, void *param2)
{
	switch(msg)
	{
	case GUI_HITTEST:
		{
			if(param1 <= m_zOrder)
			{
				long* pos = reinterpret_cast<long*>(param2);
				if( pos[0] >= m_aabb.left && pos[0] <= m_aabb.right && pos[1] >= m_aabb.top && pos[1] <= m_aabb.bottom )
					return m_zOrder;
			}
		} break;
	case GUI_CREATE:
		{
			GuiCreationParams cp = *reinterpret_cast<GuiCreationParams*>(param2);
			m_manager = cp.manager;
			m_thisID = cp.thisID;
			m_parentID = cp.parentID;
			m_zOrder = cp.zOrder;
			m_commandValue = cp.command;
			//m_relativeAabb = cp.rect;
			m_relativeAabb.left = cp.x;
			m_relativeAabb.top = cp.y;
			m_relativeAabb.right = cp.x + cp.width;
			m_relativeAabb.bottom = cp.y + cp.height;
			m_aabb.left = m_relativeAabb.left + cp.absX;
			m_aabb.top = m_relativeAabb.top + cp.absY;
			m_aabb.right = m_relativeAabb.right + cp.absX;
			m_aabb.bottom = m_relativeAabb.bottom + cp.absY;

			if(cp.text != 0)
				m_text = cp.text;
			m_mouseLButtonDownStartedHere = false;
			m_mouseRButtonDownStartedHere = false;
			m_mouseMButtonDownStartedHere = false;
			m_isMouseOver = false;
			m_isFocused = false;
			m_lastKeyDepressed = GUIC_INVALID;
			m_manager->timeKeeper()->addCallback( &m_keypressCounter );
			m_manager->timeKeeper()->addCallback( &m_mouseHoverDuration );
		} break;
	case GUI_DESTROY:
		{
			for(std::list<unsigned int>::const_iterator i = m_child.begin(); i != m_child.end(); i++)
				m_manager->destroyObject( *i );
			m_child.clear();

			m_manager->timeKeeper()->removeCallback( &m_keypressCounter );
			m_manager->timeKeeper()->removeCallback( &m_mouseHoverDuration );
			m_text = "";
			m_commandValue = 0;
			m_zOrder = 0;
			m_parentID = GUIC_INVALID;
			m_thisID = GUIC_INVALID;
			m_manager = 0;
		} break;
	case GUI_PAINT:
		{
			preparePaint();
			for(std::list<unsigned int>::const_iterator i = m_child.begin(); i != m_child.end(); i++)
			{
				m_manager->sendMessage( *i, msg, param1, param2 );
			}
			cleanupPaint();
		} break;
	case GUI_MOVE:
		{
			long dx = reinterpret_cast<long*>(param2)[0] - m_relativeAabb.left;
			long dy = reinterpret_cast<long*>(param2)[1] - m_relativeAabb.top;
			m_relativeAabb.left += dx;
			m_relativeAabb.top += dy;
			m_relativeAabb.right += dx;
			m_relativeAabb.bottom += dy;
			GuiRect parentRect = {0};
			m_manager->sendMessage( m_parentID, GUI_GET_ABS_POSITION, 0, &parentRect ); 
			m_aabb.left = m_relativeAabb.left + parentRect.left;
			m_aabb.top = m_relativeAabb.top + parentRect.top;
			m_aabb.right = m_relativeAabb.right + parentRect.left;
			m_aabb.bottom = m_relativeAabb.bottom + parentRect.top;

			long thisCorner[2] = { m_aabb.left, m_aabb.top };
			for(std::list<unsigned int>::const_iterator i = m_child.begin(); i != m_child.end(); i++)
				m_manager->sendMessage( *i, GUI_MOVE_FROM_PARENT, 0, thisCorner );
		} break;
	case GUI_MOVE_FROM_PARENT:
		{
			m_aabb.left = m_relativeAabb.left + reinterpret_cast<long*>(param2)[0];
			m_aabb.top = m_relativeAabb.top + reinterpret_cast<long*>(param2)[1];
			m_aabb.right = m_relativeAabb.right + reinterpret_cast<long*>(param2)[0];
			m_aabb.bottom = m_relativeAabb.bottom + reinterpret_cast<long*>(param2)[1];
			long thisCorner[2] = { m_aabb.left, m_aabb.top };
			for(std::list<unsigned int>::const_iterator i = m_child.begin(); i != m_child.end(); i++)
				m_manager->sendMessage( *i, GUI_MOVE_FROM_PARENT, 0, thisCorner );
		} break;
	case GUI_SIZE:
		{
			uint2 newSize = *reinterpret_cast<uint2*>(param2);
			m_relativeAabb.right = m_relativeAabb.left + newSize.x;
			m_relativeAabb.bottom = m_relativeAabb.top + newSize.y;
			m_aabb.right = m_aabb.left + newSize.x;
			m_aabb.bottom = m_aabb.top + newSize.y;
		} break;
	case GUI_CLIENT_RESIZE:
		{
		} break;
	case GUI_MOUSE_OVER:
		{
			if(m_mouseLButtonDownStartedHere && param1 == 0 && reinterpret_cast<Mouse*>(param2)->bLeftDown == false) //check whether the left button was released when not over this control
				m_mouseLButtonDownStartedHere = false;
			if(m_mouseRButtonDownStartedHere && param1 == 0 && reinterpret_cast<Mouse*>(param2)->bLeftDown == false) //check whether the right button was released when not over this control
				m_mouseRButtonDownStartedHere = false;
			if(m_mouseMButtonDownStartedHere && param1 == 0 && reinterpret_cast<Mouse*>(param2)->bLeftDown == false) //check whether the middle button was released when not over this control
				m_mouseMButtonDownStartedHere = false;
			if(param1 == 0) //mouse not over
			{
				m_isMouseOver = false;
				m_mouseHoverDuration.reset();
			}
			else
			{
				m_isMouseOver = true;
				if( m_mouseLastXCoord != reinterpret_cast<Mouse*>(param2)->px || m_mouseLastYCoord != reinterpret_cast<Mouse*>(param2)->py )
				{
					unsigned int delta = 0;
					GUI_MOUSE_MOVE_PACK( delta, reinterpret_cast<Mouse*>(param2)->py - m_mouseLastYCoord, reinterpret_cast<Mouse*>(param2)->px - m_mouseLastXCoord );
					m_mouseLastXCoord = reinterpret_cast<Mouse*>(param2)->px;
					m_mouseLastYCoord = reinterpret_cast<Mouse*>(param2)->py;
					sendMessage( m_thisID, GUI_MOUSE_MOVE, delta, param2 );
					m_mouseHoverDuration.reset();
				}
				else
				{
					if(m_mouseHoverDuration.getTimeCounting() != 0)
						sendMessage( m_thisID, GUI_MOUSE_HOVER, m_mouseHoverDuration.getTimeCounting(), param2 );
				}
			}
		} break;
	case GUI_MOUSE_MOVE:
		{
		} break;
	case GUI_MOUSE_HOVER:
		{
		} break;
	case GUI_MOUSE_LBUTTON_DOWN:
		{
			if(param1 == GUIC_BUTTON_UP) //button was up before, mouse down was first made here
				m_mouseLButtonDownStartedHere = true;
		} break;
	case GUI_MOUSE_LBUTTON_UP:
		{
			if(m_mouseLButtonDownStartedHere)
				m_manager->sendMessage( m_thisID, GUI_MOUSE_LCLICK, 0, param2 );
			m_mouseLButtonDownStartedHere = false;
		} break;
	case GUI_MOUSE_LCLICK:
		{
		} break;
	case GUI_MOUSE_RBUTTON_DOWN:
		{
			if(param1 == GUIC_BUTTON_UP) //button was up before, mouse down was first made here
				m_mouseRButtonDownStartedHere = true;
		} break;
	case GUI_MOUSE_RBUTTON_UP:
		{
			if(m_mouseRButtonDownStartedHere)
				m_manager->sendMessage( m_thisID, GUI_MOUSE_LCLICK, 0, param2 );
			m_mouseRButtonDownStartedHere = false;
		} break;
	case GUI_MOUSE_RCLICK:
		{
		} break;
	case GUI_MOUSE_MBUTTON_DOWN:
		{
			if(param1 == GUIC_BUTTON_UP) //button was up before, mouse down was first made here
				m_mouseMButtonDownStartedHere = true;
		} break;
	case GUI_MOUSE_MBUTTON_UP:
		{
			if(m_mouseMButtonDownStartedHere)
				m_manager->sendMessage( m_thisID, GUI_MOUSE_LCLICK, 0, param2 );
			m_mouseMButtonDownStartedHere = false;
		} break;
	case GUI_MOUSE_MCLICK:
		{
		} break;
	case GUI_KEY_DOWN:
		{
			GuiKey key(param1);
			if( !key.keyDown )
			{
				m_keypressCounter.setLength(400);
				m_keypressCounter.reset();
				m_lastKeyDepressed = key.scancode;
				char ch = translateKeypress( key );
				if(ch != GUIC_INVALID)
					sendMessage( m_thisID, GUI_CHAR, ch, 0 );
			}
			if( key.scancode == m_lastKeyDepressed && m_keypressCounter.getTimeRemaining() == 0 )
			{
				m_keypressCounter.setLength(40);
				m_keypressCounter.reset();
				char ch = translateKeypress( key );
				if(ch != GUIC_INVALID)
					sendMessage( m_thisID, GUI_CHAR, ch, 0 );
			}
		} break;
	case GUI_KEY_UP:
		{
			GuiKey key(param1);
			sendMessage( m_thisID, GUI_KEY_TOUCH, param1, param2 );
			if( m_lastKeyDepressed == key.scancode )
				m_lastKeyDepressed = GUIC_INVALID;
		} break;
	case GUI_KEY_TOUCH:
		{
		} break;
	case GUI_CHAR:
		{
		} break;
	case GUI_FOCUS:
		{
			if(param1 == GUIC_FOCUS_GAIN)
			{
				m_isFocused = true;
				return m_thisID;
			}
			else
				m_isFocused = false;
		} break;
	case GUI_IS_MOVABLE_BY_MOUSE:
		{
			return 1;
		} break;
	case GUI_GET_ABS_POSITION:
		{
			memcpy( param2, &m_aabb, sizeof(m_aabb) );
		} break;
	case GUI_GET_REL_POSITION:
		{
			memcpy( param2, &m_relativeAabb, sizeof(m_relativeAabb) );
		} break;
	case GUI_GET_SIZE:
		{
			uint2 objectSize( m_aabb.right - m_aabb.left, m_aabb.bottom - m_aabb.top );
			memcpy( param2, &objectSize, sizeof(objectSize) );
		} break;
	case GUI_GET_TEXT:
		{
			char* p = reinterpret_cast<char*>(param2);
			unsigned int counter = 0;
			for(unsigned int i = 0; i < param1 && i < m_text.size(); i++, counter++)
				p[i] = m_text[i];
			return counter;
		} break;
	case GUI_SET_TEXT:
		{
			char* p = reinterpret_cast<char*>(param2);
			if(param1 == GUIC_INVALID)
			{
				m_text = p;
				return strlen(p);
			}
			else
			{
				if( p[param1] == 0 )
					m_text.resize( param1 );
				else
					m_text.resize( param1 + 1 );
				for(unsigned int i = 0; i < param1; i++)
					m_text[i] = p[i];
				m_text.at( m_text.size()-1 ) = 0; //null terminator guaranteed
				return param1;
			}
		} break;
	case GUI_GET_ZORDER:
		{
			return m_zOrder;
		} break;
	case GUI_SET_ZORDER:
		{
			m_zOrder = param1;
			for(std::list<unsigned int>::const_iterator i = m_child.begin(); i != m_child.end(); i++)
				m_manager->sendMessage( *i, msg, m_zOrder+1, param2 );
		} break;
	case GUI_GET_TREEDEPTH:
		{
			int depth = m_zOrder, depthTest = m_zOrder;
			for(std::list<unsigned int>::const_iterator i = m_child.begin(); i != m_child.end(); i++)
			{
				depthTest = m_manager->sendMessage( *i, msg, param1, param2 );
				if(depthTest > depth)
					depth = depthTest;
			}
			return depth;
		} break;
	case GUI_GET_CHILDCOUNT:
		{
			return m_child.size();
		} break;
	case GUI_GET_CHILD:
		{
			if(param1 >= m_child.size())
				return GUIC_INVALID;
			std::list<unsigned int>::const_iterator iter = m_child.begin();
			for(unsigned int i = 0; i < param1; i++)
				iter++;
			return *iter;
		} break;
	case GUI_GET_PARENT:
		{
			return m_parentID;
		} break;
	case GUI_COMMAND:
		{
		} break;
	default:
		return GUIC_INVALID;
	}
	return 0;
}




const char CGuiObject::m_defAcceleratorTable[ IS_SCANCODE_COUNT * 2 ] = {
	//no modifiers
	GUIC_INVALID, //IS_INVALID,
	GUIC_INVALID, //IS_ESCAPE,
	GUIC_INVALID, //IS_F1,
	GUIC_INVALID, //IS_F2,
	GUIC_INVALID, //IS_F3,
	GUIC_INVALID, //IS_F4,
	GUIC_INVALID, //IS_F5,
	GUIC_INVALID, //IS_F6,
	GUIC_INVALID, //IS_F7,
	GUIC_INVALID, //IS_F8,
	GUIC_INVALID, //IS_F9,
	GUIC_INVALID, //IS_F10,
	GUIC_INVALID, //IS_F11,
	GUIC_INVALID, //IS_F12,
	GUIC_INVALID, //IS_PRINT_SCREEN,
	GUIC_INVALID, //IS_SCROLL_LOCK,
	GUIC_INVALID, //IS_PAUSE,
	'`', //IS_TILDE,
	'1', //IS_1,
	'2', //IS_2,
	'3', //IS_3,
	'4', //IS_4,
	'5', //IS_5,
	'6', //IS_6,
	'7', //IS_7,
	'8', //IS_8,
	'9', //IS_9,
	'0', //IS_0,
	'-', //IS_MINUS,
	'=', //IS_EQUALS,
	'\b', //IS_BACKSPACE,
	'\t', //IS_TAB,
	'q', //IS_Q,
	'w', //IS_W,
	'e', //IS_E,
	'r', //IS_R,
	't', //IS_T,
	'y', //IS_Y,
	'u', //IS_U,
	'i', //IS_I,
	'o', //IS_O,
	'p', //IS_P,
	'[', //IS_LEFT_BRACKET,
	']', //IS_RIGHT_BRACKET,
	'\\', //IS_BACK_SLASH,
	GUIC_INVALID, //IS_CAPS_LOCK,
	'a', //IS_A,
	's', //IS_S,
	'd', //IS_D,
	'f', //IS_F,
	'g', //IS_G,
	'h', //IS_H,
	'j', //IS_J,
	'k', //IS_K,
	'l', //IS_L,
	';', //IS_SEMICOLON,
	'\'', //IS_APOSTROPHE,
	'\n', //IS_ENTER,
	GUIC_INVALID, //IS_LSHIFT,
	'z', //IS_Z,
	'x', //IS_X,
	'c', //IS_C,
	'v', //IS_V,
	'b', //IS_B,
	'n', //IS_N,
	'm', //IS_M,
	',', //IS_COMMA,
	'.', //IS_PERIOD,
	'/', //IS_FORWARD_SLASH,
	GUIC_INVALID, //IS_RSHIFT,
	GUIC_INVALID, //IS_LCONTROL,
	GUIC_INVALID, //IS_LWIN,
	GUIC_INVALID, //IS_LALT,
	' ', //IS_SPACE,
	GUIC_INVALID, //IS_RALT,
	GUIC_INVALID, //IS_RWIN,
	GUIC_INVALID, //IS_CONTEXT,
	GUIC_INVALID, //IS_RCONTROL,
	GUIC_INVALID, //IS_INSERT,
	GUIC_INVALID, //IS_DELETE,
	GUIC_INVALID, //IS_HOME,
	GUIC_INVALID, //IS_END,
	GUIC_INVALID, //IS_NEXT,
	GUIC_INVALID, //IS_PREVIOUS,
	GUIC_INVALID, //IS_UP,
	GUIC_INVALID, //IS_LEFT,
	GUIC_INVALID, //IS_DOWN,
	GUIC_INVALID, //IS_RIGHT,
	GUIC_INVALID, //IS_NUM_LOCK,
	'/', //IS_NUM_DIVIDE,
	'*', //IS_NUM_MULTIPLY,
	'-', //IS_NUM_SUBTRACT,
	'7', //IS_NUM_7,
	'8', //IS_NUM_8,
	'9', //IS_NUM_9,
	'+', //IS_NUM_ADD,
	'4', //IS_NUM_4,
	'5', //IS_NUM_5,
	'6', //IS_NUM_6,
	'1', //IS_NUM_1,
	'2', //IS_NUM_2,
	'3', //IS_NUM_3,
	'\n', //IS_NUM_ENTER,
	'0', //IS_NUM_0,
	'.', //IS_NUM_DECIMAL,
	GUIC_INVALID, //IS_SHIFT, //generic virtual key state
	GUIC_INVALID, //IS_CONTROL, //generic virtual key state
	GUIC_INVALID, //IS_ALT, //generic virtual key state



	//Shift down
	GUIC_INVALID, //IS_INVALID,
	GUIC_INVALID, //IS_ESCAPE,
	GUIC_INVALID, //IS_F1,
	GUIC_INVALID, //IS_F2,
	GUIC_INVALID, //IS_F3,
	GUIC_INVALID, //IS_F4,
	GUIC_INVALID, //IS_F5,
	GUIC_INVALID, //IS_F6,
	GUIC_INVALID, //IS_F7,
	GUIC_INVALID, //IS_F8,
	GUIC_INVALID, //IS_F9,
	GUIC_INVALID, //IS_F10,
	GUIC_INVALID, //IS_F11,
	GUIC_INVALID, //IS_F12,
	GUIC_INVALID, //IS_PRINT_SCREEN,
	GUIC_INVALID, //IS_SCROLL_LOCK,
	GUIC_INVALID, //IS_PAUSE,
	'~', //IS_TILDE,
	'!', //IS_1,
	'@', //IS_2,
	'#', //IS_3,
	'$', //IS_4,
	'%', //IS_5,
	'^', //IS_6,
	'&', //IS_7,
	'*', //IS_8,
	'(', //IS_9,
	')', //IS_0,
	'_', //IS_MINUS,
	'+', //IS_EQUALS,
	'\b', //IS_BACKSPACE,
	'\t', //IS_TAB,
	'Q', //IS_Q,
	'W', //IS_W,
	'E', //IS_E,
	'R', //IS_R,
	'T', //IS_T,
	'Y', //IS_Y,
	'U', //IS_U,
	'I', //IS_I,
	'O', //IS_O,
	'P', //IS_P,
	'{', //IS_LEFT_BRACKET,
	'}', //IS_RIGHT_BRACKET,
	'|', //IS_BACK_SLASH,
	GUIC_INVALID, //IS_CAPS_LOCK,
	'A', //IS_A,
	'S', //IS_S,
	'D', //IS_D,
	'F', //IS_F,
	'G', //IS_G,
	'H', //IS_H,
	'J', //IS_J,
	'K', //IS_K,
	'L', //IS_L,
	':', //IS_SEMICOLON,
	'\"', //IS_APOSTROPHE,
	'\n', //IS_ENTER,
	GUIC_INVALID, //IS_LSHIFT,
	'Z', //IS_Z,
	'X', //IS_X,
	'C', //IS_C,
	'V', //IS_V,
	'B', //IS_B,
	'N', //IS_N,
	'M', //IS_M,
	'<', //IS_COMMA,
	'>', //IS_PERIOD,
	'?', //IS_FORWARD_SLASH,
	GUIC_INVALID, //IS_RSHIFT,
	GUIC_INVALID, //IS_LCONTROL,
	GUIC_INVALID, //IS_LWIN,
	GUIC_INVALID, //IS_LALT,
	' ', //IS_SPACE,
	GUIC_INVALID, //IS_RALT,
	GUIC_INVALID, //IS_RWIN,
	GUIC_INVALID, //IS_CONTEXT,
	GUIC_INVALID, //IS_RCONTROL,
	GUIC_INVALID, //IS_INSERT,
	GUIC_INVALID, //IS_DELETE,
	GUIC_INVALID, //IS_HOME,
	GUIC_INVALID, //IS_END,
	GUIC_INVALID, //IS_NEXT,
	GUIC_INVALID, //IS_PREVIOUS,
	GUIC_INVALID, //IS_UP,
	GUIC_INVALID, //IS_LEFT,
	GUIC_INVALID, //IS_DOWN,
	GUIC_INVALID, //IS_RIGHT,
	GUIC_INVALID, //IS_NUM_LOCK,
	'/', //IS_NUM_DIVIDE,
	'*', //IS_NUM_MULTIPLY,
	'-', //IS_NUM_SUBTRACT,
	'7', //IS_NUM_7,
	'8', //IS_NUM_8,
	'9', //IS_NUM_9,
	'+', //IS_NUM_ADD,
	'4', //IS_NUM_4,
	'5', //IS_NUM_5,
	'6', //IS_NUM_6,
	'1', //IS_NUM_1,
	'2', //IS_NUM_2,
	'3', //IS_NUM_3,
	'\n', //IS_NUM_ENTER,
	'0', //IS_NUM_0,
	'.', //IS_NUM_DECIMAL,
	GUIC_INVALID, //IS_SHIFT, //generic virtual key state
	GUIC_INVALID, //IS_CONTROL, //generic virtual key state
	GUIC_INVALID, //IS_ALT, //generic virtual key state
};
