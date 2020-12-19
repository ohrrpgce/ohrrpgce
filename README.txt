-----------------------------------------------------------------------------
   Official Hamster Republic RPG Construction Engine    (O.H.R.RPG.C.E)
                                    http://HamsterRepublic.com/ohrrpgce/
-----------------------------------------------------------------------------

Welcome to the OHRRPGCE Source Code! This file gives some basic instructions
for compiling the OHRRPGCE, some hints for hacking improvements, and some
history of the code.
-----------------------------------------------------------------------------
We strongly recommend reading http://rpg.HamsterRepublic.com/ohrrpgce/Source
for more detailed up-to-date compiling instructions and documentation of the
source code, especially http://rpg.HamsterRepublic.com/ohrrpgce/Compiling

Please see whatsnew.txt for the changelog.
-----------------------------------------------------------------------------
LICENSE:
  This program is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation; either version 2 of the License, or (at your
  option) any later version. See LICENSE.txt for the full text of the GPL.

  Any .RPG files you create are yours to distribute as you please; the GPL
  does not apply to games made with the engine.
-----------------------------------------------------------------------------
WINDOWS/LINUX/UNIX:
  Recommend FreeBasic version 1.05.0
  Requires minimum FreeBasic version 1.00.0
  Download FreeBasic from http://freebasic.net/
   http://rpg.hamsterrepublic.com/ohrrpgce/Getting_FreeBasic
  Compiling requires Python and Scons
  Building HSpeak requires Euphoria v4.0+: http://www.OpenEuphoria.com/
  (Compiling x86_64 versions of HSpeak requires Euphoria 4.1)
  Compiling gfx_directx.dll (optional) requires Visual C++ and a DirectX SDK.
  (See http://rpg.hamsterrepublic.com/ohrrpgce/Compiling#gfx_directx)
  x86, x86_64, and ARM on Win32, GNU/Linux, OSX and Android are tested.
-----------------------------------------------------------------------------
MAC OS X:
  Compiling requires Python, Scons, and a fork of FreeBasic 1.06+
  Read http://rpg.hamsterrepublic.com/ohrrpgce/Compiling_in_Mac_OS_X
-----------------------------------------------------------------------------
ANDROID:
  Requires a fork of FreeBasic 1.06+
  Requires sdl-android (ohrrpgce fork)
  See http://rpg.hamsterrepublic.com/ohrrpgce/Compiling_for_Android for info
-----------------------------------------------------------------------------
INSTRUCTIONS
  Unpack the source code to a new folder
  Install FreeBasic
  Install Python if needed
  Install SConscript
  Run "scons --help" to see options
  Run "scons" to compile Game and Custom
   (32 bit) or "scons arch=64" (64 bit)
  (Unix only) You can run "scons install" to install system-wide or as part
  of creating a package. You can run linux/ohrrpgce.py to create a .deb file.
-----------------------------------------------------------------------------
MAKING IMPROVEMENTS
  There are still lots of bugs that need to be fixed. For a list of known
  bugs, visit http://HamsterRepublic.com/ohrrpgce/buglist.php
  We also welcome code-cleanup and new features as long as you don't break
  compatibility with existing games. If you want to submit a improvement
  see http://rpg.hamsterrepublic.com/ohrrpgce/Source#Submitting_Improvements
-----------------------------------------------------------------------------
WHAT IF YOU WANT TO BREAK COMPATIBILITY?
  We will not accept patches that intentionally break compatibility from the
  official version, but you are welcome to make your own "fork" of the
  OHRRPGCE code as long as you obey the conditions of the GPL license. This
  means that if you make your own personal version of the OHRRPGCE based on
  our source code, you have to give others the same freedoms we gave to you.
  If you want to learn more about how the GPL license works, just mail us,
  and we would be happy to elaborate. You can also find the full text of the
  license in LICENSE.txt
-----------------------------------------------------------------------------
HISTORY
  The OHRRPGCE's source code has a long history! The first version was
  written back in 1996 using QuickBasic 4.5 and some ASM libraries on
  a 486 DOS computer.

  The code was very messy, but gradually, patiently, over many years
  it has been incrementally cleaned up. Some of it is in very good shape
  while other parts of the code still show the legacy of technical debt.
  On the whole, we the developers are quite proud of how this codebase has
  grown from something promitive into something-- well, not modern, but
  certainly lovingly crafted!

  After releasing the source code under the GPL, it turns out that several
  people did in fact have time and motivation to improve the code. These new
  developers were a blessing, and have revitalized OHRRPGCE development.
  Although some questionable code still exists, it is always getting better.
  The OHRRPGCE has become a joyous exercise in incrementally transforming a
  BIG BALL OF MUD into well-structured code, while maintaining backwards
  compatibility all along the way.

  The code base is big and varied, but we hope that doesn't discourage you.
  Play around with the code, have fun, communicate with the other developers
  whenever you have questions, and if you make any improvements that you
  would like us to merge back into the official version, submit patches/pull
  requests; see
  http://rpg.hamsterrepublic.com/ohrrpgce/Source#Submitting_Improvements
-----------------------------------------------------------------------------
James Paige - Bob@HamsterRepublic.com - http://HamsterRepublic.com/ohrrpgce/
-----------------------------------------------------------------------------
Do you want to keep up with the latest development progress? Subscribe to
the Developer's Mailing list:
http://lists.motherhamster.org/listinfo.cgi/ohrrpgce-motherhamster.org
-----------------------------------------------------------------------------
