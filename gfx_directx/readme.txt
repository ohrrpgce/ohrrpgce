to build the dll:
-create an empty win32 .dll project
-add all gfx_directx source code to project
-add the resource script "gfx_directx.rc" to the project
-make sure the defined constant "TESTAPP" is 0 in gfx_directx_TESTAPPconst.h

to run the test app, modify the above by:
-instead, create an empty win32 .exe project
-set the defined constant TESTAPP in gfx_directx_TESTAPPconst.h to a non-zero value

The binary was compiled for 32-bit systems using Visual Studio 2008 SP1, Unicode character set. The msvc runtime is officially statically linked, and the d3dx library is officially dynamically linked. The official version is x.y.1.