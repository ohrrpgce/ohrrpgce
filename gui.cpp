#include "gui.h"
#include "guiBase.h"

GuiManager g_guiManager;
GuiMouse g_guiMouse;
GuiKeyboard g_guiKeyboard;

void guiStart() {
	if( g_guiManager.initialize() ) {
		//register GuiObjectFactory's to the manager
	}
};

void guiStop() {
	g_guiManager.shutdown();
}

int guiSendMessage( unsigned int id, unsigned int msg, unsigned int param1, void* param2 ) {
	return g_guiManager.sendMessage( id, msg, param1, param2 );
}

void guiPostMessage( unsigned int id, unsigned int msg, unsigned int param1, void* param2 ) {
	g_guiManager.postMessage( id, msg, param1, param2 );
}

void guiPumpMessages() {
	g_guiManager.pumpMessages();
}

void guiSynthesizeInputMessage( GuiMouse& mouse, GuiKeyboard& kb ) {
	//check mouse changes

	//left mouse button state
	if( mouse.leftButton != 0 ) {
		if( g_guiMouse.leftButton != 0 )
			guiInputMessage( GUI_MOUSE_LBUTTON_DOWN, GUIC_BUTTON_DOWN, (void*)&mouse );
		else
			guiInputMessage( GUI_MOUSE_LBUTTON_DOWN, GUIC_BUTTON_UP, (void*)&mouse );
	}
	else if( g_guiMouse.leftButton != mouse.leftButton ) {
		//if( g_guiMouse.leftButton != 0 ) //implied logic
			guiInputMessage( GUI_MOUSE_LBUTTON_UP, GUIC_BUTTON_DOWN, (void*)&mouse );
	}

	//right mouse button state
	if( mouse.rightButton != 0 ) {
		if( g_guiMouse.rightButton != 0 )
			guiInputMessage( GUI_MOUSE_RBUTTON_DOWN, GUIC_BUTTON_DOWN, (void*)&mouse );
		else
			guiInputMessage( GUI_MOUSE_RBUTTON_DOWN, GUIC_BUTTON_UP, (void*)&mouse );
	}
	else if( g_guiMouse.rightButton != mouse.rightButton ) {
		//if( g_guiMouse.rightButton != 0 ) //implied logic
			guiInputMessage( GUI_MOUSE_RBUTTON_UP, GUIC_BUTTON_DOWN, (void*)&mouse );
	}

	//middle mouse button state
	if( mouse.middleButton != 0 ) {
		if( g_guiMouse.middleButton != 0 )
			guiInputMessage( GUI_MOUSE_MBUTTON_DOWN, GUIC_BUTTON_DOWN, (void*)&mouse );
		else
			guiInputMessage( GUI_MOUSE_MBUTTON_DOWN, GUIC_BUTTON_UP, (void*)&mouse );
	}
	else if( g_guiMouse.middleButton != mouse.middleButton ) {
		//if( g_guiMouse.middleButton != 0 ) //implied logic
			guiInputMessage( GUI_MOUSE_MBUTTON_UP, GUIC_BUTTON_DOWN, (void*)&mouse );
	}

	//mouse position (wheel included)
	if( g_guiMouse.x != mouse.x || g_guiMouse.y != mouse.y || g_guiMouse.wheel != mouse.wheel ) {
		int dx = mouse.x - g_guiMouse.x;
		int dy = mouse.y - g_guiMouse.y;
		guiInputMessage( GUI_MOUSE_MOVE, GUI_MOUSE_MOVE_PACKUP( dx, dy ), 0 );
	}

	g_guiMouse = mouse;

	//check for keyboard changes

	//individual key state
	for( int i = 0; i < sizeof(g_guiKeyboard.key) / sizeof(g_guiKeyboard.key[0]); i++ ) {
		GuiKey k;
		//need to check alt, shift, and ctrl contexts, setting those as well
		k.scancode = i;
		if( kb.key[i] != 0 ) {
			if( g_guiKeyboard.key[i] != 0 ) {
				k.previouslyKeyDown = true;
				guiInputMessage( GUI_KEY_DOWN, k, 0 );
			}
			else {
				k.previouslyKeyDown = false;
				guiInputMessage( GUI_KEY_DOWN, k, 0 );
			}
		}
		else if( g_guiKeyboard.key[i] != kb.key[i] ) {
			//if( g_guiKeyboard.key[i] != 0 ) //implied logic
				k.previouslyKeyDown = true;
				guiInputMessage( GUI_KEY_UP, k, 0 );
		}
	}

	//translator, such as converting a 'k' key to the character 'k', and SHIFT-'k' to character 'K'
	//unfinished!
	//for each key, generate a possible GUI_CHAR message
		//mind the shift state
		//may as well check the ALT state, too
		//guiInputMessage( GUI_CHAR, theCharacter, 0 );
}

void guiInputMessage( unsigned int msg, unsigned int param1, void* param2 ) {
	g_guiManager.postInputMessage( msg, param1, param2 );
}