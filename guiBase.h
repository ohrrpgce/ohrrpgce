//guiBase.h
//by Jay Tennant 11/3/11
//manager and framework for GuiObject's, including an implemented CGuiObject base

#pragma once

#include <map>
#include <list>
#include <queue>
#include <stack>
#include <string>

//#include "math.h"
//#include "timekeeper.h"

enum GUI_MESSAGES
{
	GUI_HITTEST,				//param1 is depth value, param2 is long[2] with x,y mouse coordinates in absolute position, returns it's depth if it is >= param1 and x,y coordinates transformed to the object still pass
	GUI_CREATE,					//param2 is creationParams, returns 0 on success, or a value for error
	GUI_DESTROY,				//no params
	GUI_PAINT,					//param2 is GuiPaint struct
	GUI_MOVE,					//param2 is long[2] with x,y coordinates of location to move to, relative to parent
	GUI_MOVE_FROM_PARENT,		//param2 is long[2] with x,y coordinates of parent's absolute upper-left corner
	GUI_SIZE,					//param2 is uint2 ptr with x,y dimensions of new object size
	GUI_CLIENT_RESIZE,			//param2 is uint2 ptr with x,y dimensions of new client (backbuffer) size

	GUI_MOUSE_OVER,				//param1 is true if mouse is over, false if it is not, param2 is const Mouse struct ptr
	GUI_MOUSE_MOVE,				//param1 is amount moved, loword being signed x coord, hiword being signed y coord, param2 is const Mouse struct ptr
	GUI_MOUSE_HOVER,			//param1 is duration, param2 is const Mouse struct ptr
	GUI_MOUSE_LBUTTON_DOWN,		//param1 is button code (GUIC_BUTTON_*), param2 is const Mouse struct ptr
	GUI_MOUSE_LBUTTON_UP,		//param1 is button code (GUIC_BUTTON_*), param2 is const Mouse struct ptr
	GUI_MOUSE_LCLICK,			//param2 is const Mouse struct ptr
	GUI_MOUSE_RBUTTON_DOWN,		//param1 is button code (GUIC_BUTTON_*), param2 is const Mouse struct ptr
	GUI_MOUSE_RBUTTON_UP,		//param1 is button code (GUIC_BUTTON_*), param2 is const Mouse struct ptr
	GUI_MOUSE_RCLICK,			//param2 is const Mouse struct ptr
	GUI_MOUSE_MBUTTON_DOWN,		//param1 is button code (GUIC_BUTTON_*), param2 is const Mouse struct ptr
	GUI_MOUSE_MBUTTON_UP,		//param1 is button code (GUIC_BUTTON_*), param2 is const Mouse struct ptr
	GUI_MOUSE_MCLICK,			//param2 is const Mouse struct ptr
	GUI_KEY_DOWN,				//param1 is the combination of key codes (GUIC_KEY_*) in the loword, InputScancode in the hiword (hint: use GuiKey), param2 is the InputKey code
	GUI_KEY_UP,					//param1 is the combination of key codes (GUIC_KEY_*) in the loword, InputScancode in the hiword (hint: use GuiKey), param2 is the InputKey code
	GUI_KEY_TOUCH,				//param1 is the combination of key codes (GUIC_KEY_*) in the loword, InputScancode in the hiword (hint: use GuiKey), param2 is the InputKey code
	GUI_CHAR,					//param1 is the char

	GUI_FOCUS,					//param1 is the focus code
	GUI_IS_MOVABLE_BY_MOUSE,	//returns 1 for true, 0 for false; default value is true
	GUI_GET_ABS_POSITION,		//param2 is GuiRect* to receive the absolute position of the object
	GUI_GET_REL_POSITION,		//param2 is GuiRect* to receive the position of the object relative to the parent
	GUI_GET_SIZE,				//param2 is uint2 ptr to receive the dimensions of the object
	GUI_GET_TEXT,				//param1 specifies how large the buffer of param2 is, param2 is a char buffer that receives the text, returns the number of characters copied
	GUI_SET_TEXT,				//param1 is how many characters to copy or -1 for the whole string, param2 is a char buffer to be copied from (if param1 is -1, the char buffer must be null terminated)
	GUI_GET_ZORDER,				//returns the z-order or z-order code
	GUI_SET_ZORDER,				//param1 is the z-order or z-order code; child objects should be updated, too
	GUI_GET_TREEDEPTH,			//returns the deepest child's z-order
	GUI_GET_CHILDCOUNT,			//returns the number of children
	GUI_GET_CHILD,				//param1 is the index into the nth child, returns the ID of the child
	GUI_GET_PARENT,				//returns the parent ID
	//GUI_IS_DESCENDANT,			//param1 is the ID of the possible anscestor
	//GUI_IS_ANSCESTOR,			//param1 is the ID of the possible descendant
	GUI_COMMAND,				//param1 is the notification code (dependent on control type), param2 is the control ID that sent this message
};

enum GUI_CODES
{
	GUIC_INVALID = -1,
	GUIC_BUTTON_UP = 0,
	GUIC_BUTTON_DOWN = 1,
	GUIC_KEY_UP = 0,
	GUIC_KEY_DOWN = 1,
	GUIC_KEY_STATE = 0x1,
	GUIC_KEY_SHIFT = 0x2,
	GUIC_KEY_CTRL = 0x4,
	GUIC_KEY_ALT = 0x8,
	//GUIC_KEY_CAPS_LOCK = 0x10,
	//GUIC_KEY_NUM_LOCK = 0x20,
	//GUIC_KEY_SCROLL_LOCK = 0x40,
	GUIC_FOCUS_LOSE = 0,
	GUIC_FOCUS_GAIN = 1,
	//GUIC_ZORDER_TOPMOST = -1,
	//GUIC_ZORDER_BOTTOMMOST = -2,
	//GUIC_ZORDER_TOP = -3,
	//GUIC_ZORDER_BOTTOM = -4,
};

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

//struct GuiPaint
//{
//	Matrix transform;
//};

class GuiManager;
struct GuiCreationParams
{
	GuiManager* manager; //auto-filled
	unsigned int thisID; //auto-filled
	int zOrder; //auto-filled
	long absX, absY; //auto-filled, absolute coordinate of upper-left corner of parent
	unsigned int parentID; //id of parent; if 0, this is an "unparented" object
	unsigned int command; //command notification
	//GuiRect rect; //bounding box, relative to parent in normalized coordinates
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

	//TimeKeeper m_tk;
	//Mouse m_mouseSnapshot;
	//Keyboard m_keyboardSnapshot;

	ObjectFactoryMap m_objectFactory;
	ObjectMap m_object;
	ObjectMap m_objectUnparented; //unparented objects; receive GUI_PAINT messages, etc.
	unsigned int m_focusedObject; //object receiving keyboard focus
	unsigned int m_movingObject; //object being moved
	std::queue<Message> m_message;
	unsigned int m_objectIDGeneratorCounter;
	static const GraphicRenderStateBlock m_rsBlock;
	std::stack< GuiRect > m_clipRegion; //accumulative clipping region
	//uint2 m_clientSize; //size of client, used for GUI_CLIENT_RESIZE

	unsigned int generateID() {unsigned int n = m_objectIDGeneratorCounter; m_objectIDGeneratorCounter++; return n;}

	void queryInput(); //checks the mouse, creates messages from that
	//void clientSizeTest(); //compares the backbuffer size to the recorded client size; if it is different, a GUI_CLIENT_RESIZE message is posted to all controls; contemplating reducing this only to unparented controls
	void dispatchMessage(Message* pMsg); //dispatches one message, freeing the memory automatically if allocated
public:
	GuiManager();
	~GuiManager();

	////simple accessors
	//TimeKeeper* timeKeeper() {return m_tk;}

	//manager stuff
	bool initialize();
	void shutdown();
	bool registerObjectFactory( GuiObjectFactory* pFactory );
	void unregisterObjectFactory( const char* szClass );

	unsigned int createObject( const char* szClass, GuiCreationParams* params );
	void destroyObject( unsigned int objectID );
	//GuiRect calculateRelativeArea( unsigned int parentID, const GuiRect* pArea ); //calculates the width and height factors relative to the parent, to match the backbuffer area; useful for maintaining the same visual proportions through all nested objects
	void pushClipRegion( const GuiRect* pRegion ); //pushes a clipping region onto an accumulative stack
	void popClipRegion();
	void getClipRegion(GuiRect* pOut); //gets a copy of the clip region

	//message related
	void pumpMessages();
	int sendMessage( unsigned int id, unsigned int msg, unsigned int param1, void* param2 );
	void postMessage( Message* pMsg );
	void postMessage( unsigned int id, unsigned int msg, unsigned int param1, void* param2 );
	unsigned int focus( unsigned int id );
	void allocateMessageSpace( unsigned int numBytes, Message* pMsg );
	void freeMessageSpace( Message* pMsg );

	////rendering
	//void render( /*const Matrix* pWVP = 0, const GuiRect* pClippingRegion = 0*/ );
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
	bool m_isClippingEnabled;
	//bool m_isPaintPrepared;
	unsigned int m_lastKeyDepressed;
	int m_mouseLastXCoord;
	int m_mouseLastYCoord;
	TKCounter m_mouseHoverDuration;
	TKCounter m_keypressCounter;
	std::list<unsigned int> m_child;
	char translateKeypress( GuiKey& k );
	char m_acceleratorTable[ IS_SCANCODE_COUNT * 2 ];
	static const char m_defAcceleratorTable[ IS_SCANCODE_COUNT * 2 ];
public:
	CGuiObject();
	virtual ~CGuiObject();

	unsigned int createChild(const char* szClass, GuiCreationParams* params); //creates the child, adds it to the managed child list, and returns the ID of the child
	void destroyChild(unsigned int childID); //destroys the child; children are automatically destroyed when the proc() receives the GUI_DESTROY message
	//void preparePaint(); //pushes the clipping region, if clipping is enabled
	//void cleanupPaint(); //pops the clipping region, if clipping is enabled

	int sendMessage(unsigned int id, unsigned int msg, unsigned int param1, void* param2) {return m_manager->sendMessage( id, msg, param1, param2 );}
	//bool getKeyState( unsigned int sc ) {return m_manager->input()->keyboard().keys[sc];}
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
