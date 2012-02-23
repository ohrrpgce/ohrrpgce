//guiBase.h
//by Jay Tennant 11/3/11
//manager and framework for GuiObject's, including an implemented CGuiObject base

#pragma once

#include <map>
#include <list>
#include <queue>
#include <stack>
#include <string>

#include "gui.h"

#define GUI_MAKE_DWORD( hiword, loword ) \
	( ( ((unsigned int)(hiword) & 0xffff) << 16 ) | ((unsigned int)(loword) & 0xffff) )
#define GUI_LOWORD( dWord ) \
	((unsigned short)( (unsigned int)(dWord) & 0xffff ))
#define GUI_HIWORD( dWord ) \
	((unsigned short)( ((unsigned int)(dWord) & 0xffff0000) >> 16 ))

//these should be contained within a packer/unpacker object, like GuiKey, instead of defined like this
#define GUI_MOUSE_MOVE_PACK( rparam, x, y ) \
	rparam = GUI_MAKE_DWORD(y, x)
#define GUI_MOUSE_MOVE_UNPACK( param, rx, ry ) \
	rx = (short)GUI_LOWORD(param); ry = (short)GUI_HIWORD(param)

#define GUI_MOUSE_MOVE_PACKUP( x, y ) \
	GUI_MAKE_DWORD(y, x)
#define GUI_MOUSE_MOVE_UNPACK_X( param ) \
	(short)GUI_LOWORD(param)
#define GUI_MOUSE_MOVE_UNPACK_Y( param ) \
	(short)GUI_HIWORD(param)

struct GuiKey //packs and unpacks key context and scancode into a dword; used by GUI_KEY_DOWN, GUI_KEY_UP, GUI_KEY_TOUCH
{
	bool previouslyKeyDown, shift, ctrl, alt;
	unsigned int scancode;
	GuiKey(const GuiKey& c) : keyDown(c.keyDown), shift(c.shift), ctrl(c.ctrl), alt(c.alt), scancode(c.scancode) {}
	GuiKey(unsigned int dWord = 0) { 
		unsigned int l = GUI_LOWORD( dWord ); 
		keyDown = (GUIC_KEY_DOWN == (l & GUIC_KEY_STATE)); 
		shift = (GUIC_KEY_DOWN == ((l & GUIC_KEY_SHIFT) >> 1)); 
		ctrl = (GUIC_KEY_DOWN == ((l & GUIC_KEY_CTRL) >> 2));
		alt = (GUIC_KEY_DOWN == ((l & GUIC_KEY_ALT) >> 3));
		scancode = GUI_HIWORD( dWord );
	}
	operator unsigned int() {
		unsigned int l = 0x0;
		l |= keyDown ? GUIC_KEY_STATE : 0;
		l |= shift ? GUIC_KEY_SHIFT : 0;
		l |= ctrl ? GUIC_KEY_CTRL : 0;
		l |= alt ? GUIC_KEY_ALT : 0;
		return GUI_MAKE_DWORD( scancode, l );
	}
};

struct GuiObject
{
	virtual int proc(unsigned int msg, unsigned int param1, void* param2) = 0;
};

struct GuiRect
{
	long left, top, right, bottom;
};

class GuiManager;
struct GuiCreationParams
{
	GuiManager* manager; //auto-filled
	unsigned int thisID; //auto-filled
	int zOrder; //auto-filled
	long absX, absY; //auto-filled, absolute coordinate of upper-left corner of parent
	unsigned int parentID; //id of parent; if 0, this is an "unparented" object
	unsigned int command; //command notification
	long x, y; //position of upper-left corner
	long width, height; //area dimension
	const char* text; //text to appear as "button text", initial text in edit control, etc.
	unsigned int dwFlags; //creation flags
	void* pExtra; //extra data, specific to a control
};

struct GuiObjectFactory
{
	virtual GuiObject* createInstance() = 0;
	virtual void destroyInstance(GuiObject* pObject) = 0;
	virtual void notifyRegister(GuiManager* pManager) = 0;
	virtual void notifyUnregister() = 0;
	virtual const char* szClass() = 0;
};

class GuiManager
{
private:
	typedef std::map<std::string, GuiObjectFactory*> ObjectFactoryMap;
	typedef std::pair<std::string, GuiObjectFactory*> ObjectFactoryMapInsertionInput;
	typedef std::pair<ObjectFactoryMap::iterator, bool> ObjectFactoryMapInsertionResult;

	struct ObjectAndFactoryTrace { GuiObject* object; GuiObjectFactory* factory; };

	typedef std::map<unsigned int, ObjectAndFactoryTrace> ObjectMap;
	typedef std::pair<unsigned int, ObjectAndFactoryTrace> ObjectMapInsertionInput;
	typedef std::pair<ObjectMap::iterator, bool> ObjectMapInsertionResult;
	
	struct Message {unsigned int controlID; unsigned int msg; unsigned int param1; void* param2; bool allocated;};

	Mouse m_mouseSnapshot;
	Keyboard m_keyboardSnapshot;

	ObjectFactoryMap m_objectFactory;
	ObjectMap m_object;
	ObjectMap m_objectUnparented; //unparented objects; receive GUI_PAINT messages, etc.
	unsigned int m_focusedObject; //object receiving keyboard focus
	unsigned int m_movingObject; //object being moved
	std::queue<Message> m_message; //all queued messages, for all controls
	std::queue<Message> m_inputMessage; //input messages posted to the GuiManager
	unsigned int m_objectIDGeneratorCounter;

	unsigned int generateID() {unsigned int n = m_objectIDGeneratorCounter; m_objectIDGeneratorCounter++; return n;}

	void dispatchMessage(Message* pMsg); //dispatches one message, freeing the memory automatically if allocated
public:
	GuiManager();
	~GuiManager();

	//manager stuff
	bool initialize();
	void shutdown();

	//object stuff
	bool registerObjectFactory( GuiObjectFactory* pFactory );
	void unregisterObjectFactory( const char* szClass );
	unsigned int createObject( const char* szClass, GuiCreationParams* params );
	void destroyObject( unsigned int objectID );

	//message related
	void pumpMessages();
	int sendMessage( unsigned int id, unsigned int msg, unsigned int param1, void* param2 );
	void postMessage( Message* pMsg );
	void postMessage( unsigned int id, unsigned int msg, unsigned int param1, void* param2 );
	void postInputMessage( unsigned int msg, unsigned int param1, void* param2 ); //for posting input messages (synthesized or otherwise) to the GuiManager
	unsigned int focus( unsigned int id );
	void allocateMessageSpace( unsigned int numBytes, Message* pMsg );
	void freeMessageSpace( Message* pMsg );
};

class CGuiObject : public GuiObject
{
private:
	GuiManager* m_manager;
	unsigned int m_thisID;
	unsigned int m_parentID;
	unsigned int m_zOrder;
	unsigned int m_commandValue;
	GuiRect m_aabb;
	GuiRect m_relativeAabb;
	std::string m_text;
	bool m_mouseLButtonDownStartedHere;
	bool m_mouseRButtonDownStartedHere;
	bool m_mouseMButtonDownStartedHere;
	bool m_isMouseOver;
	bool m_isFocused;
	unsigned int m_lastKeyDepressed;
	int m_mouseLastXCoord;
	int m_mouseLastYCoord;
	std::list<unsigned int> m_child;
public:
	CGuiObject();
	virtual ~CGuiObject();

	unsigned int createChild(const char* szClass, GuiCreationParams* params); //creates the child, adds it to the managed child list, and returns the ID of the child
	void destroyChild(unsigned int childID); //destroys the child; children are automatically destroyed when the proc() receives the GUI_DESTROY message

	int sendMessage(unsigned int id, unsigned int msg, unsigned int param1, void* param2) {return m_manager->sendMessage( id, msg, param1, param2 );}
	GuiManager* manager() {return m_manager;}
	unsigned int thisID() const {return m_thisID;}
	unsigned int parentID() const {return m_parentID;}
	unsigned int zOrder() const {return m_zOrder;}
	unsigned int commandValue() const {return m_commandValue;}
	const GuiRect& aabb() const {return m_aabb;}
	const GuiRect& relativeAabb() const {return m_relativeAabb;}
	const std::string& text() const {return m_text;}
	bool isMouseOver() const {return m_isMouseOver;}
	bool isMouseLButtonDownStartedHere() const {return m_mouseLButtonDownStartedHere;}
	bool isMouseMButtonDownStartedHere() const {return m_mouseMButtonDownStartedHere;}
	bool isMouseRButtonDownStartedHere() const {return m_mouseRButtonDownStartedHere;}
	bool isFocused() const {return m_isFocused;}
	unsigned int childCount() const {return m_child.size();}
	unsigned int childID(unsigned int index) const {if(index >= m_child.size()) return GUIC_INVALID; std::list<unsigned int>::const_iterator iter = m_child.begin(); for(int i = 0; i != index; i++) iter++; return *iter;}

	//override
	virtual int proc(unsigned int msg, unsigned int param1, void* param2);
};
