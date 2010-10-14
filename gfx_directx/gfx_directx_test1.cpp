#include "gfx_directx_TESTAPPconst.h"

#if TESTAPP

#include "gfx_directx.h"
#include <windows.h>

#include "fb_scancodes.h"

BYTE g_frameTest[320 * 200];
UINT g_paletteTest[256];
int g_keys[128];

bool g_bQuit;
void __cdecl RequestQuit()
{
	g_bQuit = true;
}

int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, LPSTR lpCmdLine, int nShowCmd)
{
	for(UINT i = 0; i < 320 * 200; i++)
		g_frameTest[i] = i % 256;
	for(UINT i = 0; i < 256; i++)
		g_paletteTest[i] = 0xff000000 + (i << 16) + (i << 8) + i;
	g_paletteTest[0xff] = 0xff00ff00;
	g_paletteTest[0xee] = 0xffff0000;
	g_paletteTest[0xdd] = 0xff0000ff;

	char szInfoBuffer[256] = "";
	gfx_init(RequestQuit, 0, szInfoBuffer, sizeof(szInfoBuffer));
	gfx_windowtitle("DirectX Backend Test App");
	gfx_setpal(g_paletteTest);
	io_init();
	::MessageBox(0, TEXT("Use left and right to change scroll speed.") \
					TEXT("\r\nUse 'S' to take a screenshot.") \
					TEXT("\r\nUse 'D' to get debug local d3d9 capabilities.") \
					TEXT("\r\nUse 'L' to get an error log (if you hear beeping, use this)."), 
					TEXT("TestApp Message"), MB_OK);

	UINT j = 0;
	int x,y,buttons;
	io_setmousevisibility(FALSE);
	while(!g_bQuit)
	{
		for(UINT i = 0; i < 320 * 200; i++)
			g_frameTest[i] = (i+j) % 256;
		j++;
		j %= 256;

		gfx_showpage(g_frameTest, 320, 200);
		//io_waitprocessing();
		::ZeroMemory(g_keys, sizeof(g_keys));
		io_updatekeys(g_keys);
		buttons = 0; x = 0; y = 0;
		io_readjoysane(0, buttons, x, y);
		if(g_keys[SC_LEFT] || buttons & 0x1)
			j++;
		j += x / 10;
		j -= y / 10;
		if(g_keys[SC_RIGHT] || buttons & 0x2)
			j--;
		if(g_keys[SC_ESCAPE])
			g_bQuit = true;
		if(g_keys[SC_S])
			gfx_screenshot("testscreen2");
	}

	gfx_close();
	return 0;
};

#endif //TESTAPP