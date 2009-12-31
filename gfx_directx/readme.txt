to build the dll:
-create an empty win32 .dll project
-add all "root\source" code to project _EXCEPT_ gfx_directx.old.cpp
-add the contents of "root\resources" to the working directory
-add the resource script "root\resources\gfx_directx.rc" to the project

to run the test app, modify the above by:
-instead, create an empty win32 .exe project
-set the macro TESTAPP in gfx_directx_TESTAPPconst.h to the value 1

The binary was compiled for 32-bit systems using Visual Studio 2008 SP1, Unicode character set. The msvc runtime is officially statically linked, and the d3dx library is officially dynamically linked. The official version is x.y.1.