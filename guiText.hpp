//guiText.h
//by Jay Tennant 11/2/11
//implements GUI text render control

#pragma once

#include "guiBase.h"
#include "font2.h"

enum GUI_TEXT_STYLE
{
	GTS_ALLOW_TEXT_CODES = 0x1, //allows text codes to be used
	GTS_SELECTABLE = 0x2, //allows text to be selectable, receive focus
	GTS_SHOW_CARET = 0x4, //shows caret when text is focused
	GTS_MULTILINE = 0x8, //allows for multiple lines, and therefore embedded alignments in strings
	GTS_AUTOWRAP = 0x10 | GTS_MULTILINE, //allows for autowrapping of text; multiline is implied
	GTS_CLIP_TEXT = 0x20, //clips text to region
	GTS_VALIGN_TOP = 0x0, //aligns the text vertically to the top; this is the default value
	GTS_VALIGN_MIDDLE = 0x40, //aligns the text vertically to the middle
	GTS_VALIGN_BOTTOM = 0x80, //aligns the text vertically to the bottom
	GTS_HALIGN_LEFT = 0x0, //aligns the text horizontally to the left; this is the default value
	GTS_HALIGN_CENTER = 0x100, //aligns the text horizontally to the center
	GTS_HALIGN_RIGHT = 0x200, //aligns the text horizontally to the right
};

struct GuiTextCreate
{
	unsigned int defaultColor, defaultWidth, defaultHeight;
};

class CGuiText : public CGuiObject
{
private:
	struct CharCode
	{
		unsigned int c; //if not a code, a letter; otherwise, a color
		unsigned int code; //non-zero means a code, like newline, tab, color change
	};
	struct CharRegion
	{
		unsigned int color; //color of paintable char
		int width; //width of paintable char, counting from region.left
		int height; //height of paintable char, counting back from region.bottom
		GuiRect region; //max dimensions of char region
		FontRect texCoord; //tex coords of paintable font char
		VertexData v[4]; //vertices containing all paint information; filled by finalize()
		void finalize();
	};
	//ConstantBuffer* m_cb; //wvp matrix; this is created by factory
	//VertexBuffer* m_vb; //max 256*4 vertices (256 letters max rendered at a time)
	//IndexBuffer* m_ib; //max 256*2*3 indices (256 letters max rendered at a time); this is created by factory
	//FontObject* m_font; //maintains font surface, texture lookups; this is created by factory
	std::list<CharCode> m_text; //individual letters or codes
	std::list<CharRegion> m_paintable; //individual paintable char's
	std::stack<unsigned int> m_color; //color stack; if empty, m_defaultColor is used; limited to 64 nested colors
	std::stack<int> m_height; //height stack; if empty, m_defaultHeight is used; limited to 64 nested heights
	std::stack<int> m_width; //width stack; if empty, m_defaultWidth is used; limited to 64 nested widths
	std::stack<unsigned int> m_hAlign; //horizontal alignment stack; if empty, m_defaultHAlign is used; limited to 64 nested alignments

	unsigned int m_defaultColor; //default color when the color stack is empty
	unsigned int m_defaultHeight;
	unsigned int m_defaultWidth;
	unsigned int m_defaultHAlign;
	unsigned int m_defaultVAlign;
	unsigned int m_alphaOverride; //an overriding alpha value for the color; if the color has a lower alpha value, that color is used instead of alphaOverride
	bool m_isMultiline; //if true, the text is multiline, so carriage returns allowed
	bool m_isAutoWrap; //if true (and m_isMultiline is true), autowraps text
	bool m_isClipping; //if true, clips text to the textbox region
	bool m_allowsGuiTextCode; //if true, text codes can be embedded in the strings, like "Hello <color=ffffff00>Johnny</color>!"
	bool m_allowsCaret; //if true, a caret is allowable
	bool m_isSelectable; //if false, all selection/focus messages are forwarded to the parent; the hittest will always fail
	TKCounter m_caretFlash; //blinking caret counter

protected:
	//extracting string into control
	bool isHexNumber(const char* szText, int numDigits = 8) const;
	unsigned int atohex(const char* szText) const;
	void extract(const char* szText);
	unsigned int textToCode(CharCode& ccOut, const char* p) const;

	//serialize control into string
	void serialize(std::string& strOut) const;
	void codeToText(std::string& strOut, const CharCode& cc) const;

	bool newLine( unsigned int numPaintableOnLine, unsigned int maxLineWidth, unsigned int maxLineHeight );
	void updateRegions();
public:
	CGuiText(FontObject* pFont, ConstantBuffer* cbuffer, IndexBuffer* ibuffer);
	virtual ~CGuiText();

	//override
	virtual int proc(unsigned int msg, unsigned int param1, void* param2);
};

class CGuiTextFactory : public GuiObjectFactory
{
private:
	std::list<CGuiText> m_control;
	GuiManager* m_manager;
	IndexBuffer* m_ib;
	ConstantBuffer* m_cb;
	FontObject m_font;
public:
	CGuiTextFactory();
	virtual ~CGuiTextFactory();

	//overrides
	virtual GuiObject* createInstance();
	virtual void destroyInstance(GuiObject* pObject);
	virtual void notifyRegister(GuiManager* pManager);
	virtual void notifyUnregister();
	virtual const char* szClass();
};