Using Visual Studio:

to build the dll:
-create an empty win32 .dll project
-add all gfx_directx source code to project, minus gfx_directx_test1.cpp
-add the resource script "gfx_directx.rc" to the project

to build the test app:
-create an empty win32 .exe project
-add gfx_directx_test1.cpp to project

Using SConscript:

Run "scons gfx_directx.dll gfx_directx_test1.exe"
The resultant files are created in the 'wip' directory, not the 'gfx_directx' directory.
You can append "debug=1" to the commandline for a debugging build, though it is probably best to compile and debug from within VS.


The binary was compiled for 32-bit systems using Visual Studio 2008 SP1, Unicode character set. The msvc runtime (msvcp90.lib) is officially statically linked, and the d3dx library is officially dynamically linked. The official version is x.y.1.
