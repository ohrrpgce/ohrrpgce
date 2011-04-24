to build the dll:
-create an empty win32 .dll project
-add all gfx_directx source code to project, minus gfx_directx_test1.cpp
-add the resource script "gfx_directx.rc" to the project

to build the test app:
-create an empty win32 .exe project
-add gfx_directx_test1.cpp to project

The binary was compiled for 32-bit systems using Visual Studio 2008 SP1, Unicode character set. The msvc runtime is officially statically linked, and the d3dx library is officially dynamically linked. The official version is x.y.1.