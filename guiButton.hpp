//guiButton.h
//by Jay Tennant 11/3/11
//implements GUI button control, with a text control on top of it

#pragma once

#include "guiBase.h"
#include "guiText.h"

enum GUI_BUTTON_STYLE
{
	GBS_DEFAULT = 0x0, //default button creation, creates one with text centered
	//GBS_3STATE = 0x1, //3-state button
	//GBS_CHECKBOX = 0x2, //check box
	//GBS_RADIO = 0x4, //radio button
	//GBS_PUSH = 0x8, //push-button, or push-like if combined with 3STATE, CHECKBOX, or RADIO
	//GBS_GROUPBOX = 0x10, //group box, meant to encapsulate mutually exclusive radio buttons
	//GBS_TEXT = 0,//0x20, //uses text
	//GBS_TEXT_LEFT = 0x40 | GBS_TEXT, //puts the text on the left of the button, or aligns it left
	//GBS_TEXT_RIGHT = 0x80 | GBS_TEXT, //puts the text on the right of the button, or aligns it right
	//GBS_TEXT_CENTER = 0x100 | GBS_TEXT, //aligns the text in the center if push-button, does not specify which side to put the text if not push-button
	//GBS_TEXT_TOP = 0x200 | GBS_TEXT, //aligns the text at the top if push-button, or group box
	//GBS_TEXT_BOTTOM = 0x400 | GBS_TEXT, //aligns the text at the bottom if push-button, or group box
	//GBS_TEXT_MIDDLE = 0x800 | GBS_TEXT, //aligns the text in the middle if push-button, does not mean anything otherwise
	//GBS_TEXT_MULTILINE = 0x1000 | GBS_TEXT, //allows the text to be multiline
};

class CGuiButton : public CGuiObject
{
public:
	CGuiButton();
	virtual ~CGuiButton();

	virtual int proc(unsigned int msg, unsigned int param1, void* param2);
};

class CGuiButtonFactory : public GuiObjectFactory
{
private:
	std::list<CGuiButton> m_control;
	GuiManager* m_manager;
public:
	CGuiButtonFactory();
	virtual ~CGuiButtonFactory();

	virtual GuiObject* createInstance();
	virtual void destroyInstance(GuiObject* pObject);
	virtual void notifyRegister(GuiManager* pManager);
	virtual void notifyUnregister();
	virtual const char* szClass();
};
