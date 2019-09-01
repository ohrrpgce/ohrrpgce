#!/usr/bin/env python
"""Main scons build script for OHRRPGCE
Run "scons -h" to print help (and "scons -H" for options to scons itself).

cf. SConstruct, ohrbuild.py
"""
import sys
import os
import platform
import shutil
import shlex
import itertools
import re
from ohrbuild import get_command_output
import ohrbuild

FBFLAGS = ['-mt'] #, '-showincludes']
# Flags used when compiling C and C++ modules, but NOT -gen gcc or euc generated
# C sources (except on Android...). Not used for linking.
CFLAGS = ['-Wall']
# Flags used when compiling -gen gcc generated C sources.
# Not for euc, and not using on Android (because of inflexible build system).
GENGCC_CFLAGS = []
# TRUE_CFLAGS apply only to normal .c sources, NOT to C++ or those generated via gengcc=1 or euc.
# Use gnu99 dialect instead of c99. c99 causes GCC to define __STRICT_ANSI__
# which causes types like off_t and off64_t to be renamed to _off_t and _off64_t
# under MinGW. (See bug 951)
TRUE_CFLAGS = ['--std=gnu11']
# Flags used only for C++ (in addition to CFLAGS)
# Can add -fno-exceptions, but only removes ~2KB
CXXFLAGS = '--std=c++0x -Wno-non-virtual-dtor'.split()
# CXXLINKFLAGS are used when linking with g++
CXXLINKFLAGS = []
# FBLINKFLAGS are passed to fbc when linking with fbc
FBLINKFLAGS = []
# FBLINKERFLAGS are passed to the linker (with -Wl) when linking with fbc
FBLINKERFLAGS = []


release = int (ARGUMENTS.get ('release', False))
verbose = int (ARGUMENTS.get ('v', False))
if verbose:
    FBFLAGS += ['-v']
if 'FBFLAGS' in os.environ:
    FBFLAGS += shlex.split (os.environ['FBFLAGS'])
fbc = ARGUMENTS.get ('fbc','fbc')
fbc = os.path.expanduser (fbc)  # expand ~
gengcc = int (ARGUMENTS.get ('gengcc', True if release else False))
linkgcc = int (ARGUMENTS.get ('linkgcc', True))   # link using g++ instead of fbc?
envextra = {}
FRAMEWORKS_PATH = os.path.expanduser("~/Library/Frameworks")  # Frameworks search path in addition to the default /Library/Frameworks
destdir = ARGUMENTS.get ('destdir', '')
prefix =  ARGUMENTS.get ('prefix', '/usr')
dry_run = int(ARGUMENTS.get ('dry_run', '0'))  # Only used by uninstall

base_libraries = []  # libraries shared by all utilities (except bam2mid)

# Set default value for -j/--jobs option
try:
    import multiprocessing  # Python 2.6+
    SetOption('num_jobs', multiprocessing.cpu_count())
except (ImportError, NotImplementedError):
    pass

################ Decide the target/OS and cpu arch

# Note: default_arch will be one of x86, x86_64, arm, aarch64, not more specific.
# default_target will be one of win32, dos, linux, freebsd, darwin, etc
fbc_binary, fbcversion, fullfbcversion, default_target, default_arch = ohrbuild.get_fb_info(fbc)
if verbose:
    print "Using fbc", fbc_binary #, " version:", fbcversion

win32 = False
unix = False  # True on mac and android
mac = False
android = False
android_source = False
win95 = int(ARGUMENTS.get ('win95', '1'))
glibc = False  # Computed below; can also be overridden by glibc=1 cmdline argument
target = ARGUMENTS.get ('target', None)
arch = ARGUMENTS.get ('arch', None)  # default decided below

if 'android-source' in ARGUMENTS:
    # Produce .c files, and also an executable, which is an unwanted side product
    # (We could do with build targets for compiling to .asm/.c but not assembling+linking)
    FBFLAGS += ["-r"]
    if target:
        print "Don't use 'target' and 'android-source' together. Use only 'target' for real cross-compiling."
        print "You can use arch=arm|arm64|x86|x86_64|etc however."
        Exit(1)
    target = 'android'
    default_arch = 'arm'
    android_source = True
    linkgcc = False

if not target:
    target = default_target

# Must check android before linux, because of 'arm-linux-androideabi'
if 'android' in target:
    android = True
elif 'win32' in target or 'windows' in target or 'mingw' in target:
    win32 = True
elif 'darwin' in target or 'mac' in target:
    mac = True
elif 'linux' in target or 'bsd' in target or 'unix' in target:
    unix = True
    if 'linux' in target:
        glibc = True
else:
    print "!! WARNING: target '%s' not recognised!" % target

exe_suffix = ''
if win32:
    exe_suffix = '.exe'
    # Force use of gcc instead of MSVC++, so compiler flags are understood
    envextra = {'tools': ['mingw']}
else:
    unix = True

target_prefix = ''  # prepended to gcc, etc.
if target.count('-') >= 2:
    target_prefix = target + '-'

if arch == '32':
    if android:
        arch = 'armeabi'  # This might be obsolete?
    elif 'x86' in default_arch:
        arch = 'x86'
    else:
        arch = 'armv7-a'
if arch == '64':
    if 'x86' in default_arch:
        arch = 'x86_64'
    else:
        arch = 'aarch64'
if arch in ('armeabi', 'androideabi'):
    # armeabi is an android abi name. ARM EABI is a family of ABIs.
    # For example, in debian ARM EABI (called armel) allows armv4t+.
    arch = 'armv5te'
if arch in ('arm', 'armv7a', 'armeabi-v7a'):
    # Again, armeabi-v7a is an android abi.
    arch = 'armv7-a'
if arch in ('arm64', 'aarch64', 'arm64-v8a'):
    # arm64-v8a is an android abi. aarch64 is the arch name recognised by FB.
    arch = 'aarch64'
if not arch:
    if target_prefix:
        # The arch is implied in the target triple. Let fbc handle it, parsing the
        # triple is too much work
        arch = '(see target)'
    elif android:
        # There are 4 ARM ABIs used on Android
        # armeabi - ARMV5TE and later. All floating point is done by library calls
        #           (aka androideabi, as it is slightly more specific than the ARM EABI)
        # armeabi-v7a - ARM V7 and later, has hardware floating point (VFP)
        # armeabi-v7a-hard - not a real ABI. armeabi-v7a with faster passing convention
        #           for floating point values. Not binary compatible with armeabi, support
        #           was dropped in later Android NDK versions.
        # arm64-v8a
        # See https://developer.android.com/ndk/guides/abis.html for more
        arch = 'armv5te'
    elif mac:
        # Default to 32 bit, because bundle-apps.sh only supports that currently
        arch = 'x86'
    else:
        arch = default_arch

################ Other commandline arguments

if int (ARGUMENTS.get ('asm', False)):
    FBFLAGS += ["-R", "-RR", "-g"]

# glibc=0|1 overrides automatic detection
glibc = int (ARGUMENTS.get ('glibc', glibc))
if glibc:
    CFLAGS += ["-DHAVE_GLIBC"]

pdb = int(ARGUMENTS.get('pdb', 0))
if pdb:
    # fbc -gen gas outputs STABS debug info, gcc outputs DWARF; cv2pdb requires DWARF
    gengcc = True
    if not win32:
        print "pdb=1 only makes sense when targeting Windows"
        Exit(1)

# There are five levels of debug here: 0, 1, 2, 3, 4 (ugh!). See the help.
if release:
    debug = 0
else:
    debug = 2  # Default to happy medium
if 'debug' in ARGUMENTS:
    debug = int (ARGUMENTS['debug'])
optimisations = (debug < 3)    # compile with C/C++/FB optimisations?
FB_exx = (debug in (2,3))     # compile with -exx?
if debug >= 1 or pdb:
    # If debug=0 and pdb, then the debug info gets stripped later
    FBFLAGS.append ('-g')
    CFLAGS.append ('-g')
# Note: fbc includes symbols (but not debug info) in .o files even without -g,
# but strips everything if -g not passed during linking; with linkgcc we need to strip.
GCC_strip = (debug == 0 and pdb == 0)  # (linkgcc only) strip debug info and unwanted symbols?
if ARGUMENTS.get('lto'):
    # Only use LTO on gengcc .C files. GCC throws errors if you try to use LTO
    # across C/FB, after saying declarations don't match.
    #CFLAGS.append('-flto')
    GENGCC_CFLAGS.append('-flto')
    CXXLINKFLAGS.append('-flto')

# Make sure we can print stack traces
# Also -O2 plus profiling crashes for me due to mandatory frame pointers being omitted.
CFLAGS.append('-fno-omit-frame-pointer')
GENGCC_CFLAGS.append('-fno-omit-frame-pointer')

portable = False
if release and unix and not mac and not android:
    portable = True
portable = int (ARGUMENTS.get ('portable', portable))
profile = int (ARGUMENTS.get ('profile', 0))
if profile:
    FBFLAGS.append ('-profile')
    CFLAGS.append ('-pg')
if int (ARGUMENTS.get ('valgrind', 0)):
    #-exx under valgrind is nearly redundant, and really slow
    FB_exx = False
    # This changes memory layout of vectors to be friendlier to valgrind
    CFLAGS.append ('-DVALGRIND_ARRAYS')
asan = int (ARGUMENTS.get ('asan', 0))
if asan:
    # AddressSanitizer is supported by both gcc & clang. They are responsible for linking runtime library
    assert linkgcc, "linkgcc=0 asan=1 combination not supported."
    CFLAGS.append ('-fsanitize=address')
    CXXLINKFLAGS.append ('-fsanitize=address')
    base_libraries.append ('dl')
    # Also, compile FB to C by default, unless overridden with gengcc=0.
    if int (ARGUMENTS.get ('gengcc', 1)):
        gengcc = True
        FB_exx = False  # Superceded by AddressSanitizer
if FB_exx:
    FBFLAGS.append ('-exx')
if optimisations:
    CFLAGS.append ('-O3')
    CXXLINKFLAGS.append ('-O2')  # For LTO
    # FB optimisation flag currently does pretty much nothing unless using -gen gcc
    FBFLAGS += ["-O", "2"]
else:
    CFLAGS.append ('-O0')

# Backend selection.
if 'gfx' in ARGUMENTS:
    gfx = ARGUMENTS['gfx']
elif 'OHRGFX' in os.environ:
    gfx = os.environ['OHRGFX']
elif mac:
    gfx = 'sdl'
elif android:
    gfx = 'sdl'
elif win32:
    gfx = 'directx+sdl+fb'
else: # unix
    gfx = 'sdl+fb'
gfx = [g.lower() for g in gfx.split("+")]
if 'music' in ARGUMENTS:
    music = ARGUMENTS['music']
elif 'OHRMUSIC' in os.environ:
    music = os.environ['OHRMUSIC']
elif 'sdl2' in gfx:
    music = 'sdl2'
else:
    music = 'sdl'
music = [music.lower()]

# You can link both gfx_sdl and gfx_sdl2, but one of SDL 1.2, SDL 2 will
# be partially shadowed by the other and will crash. Need to use dynamic linking. WIP.
if 'sdl' in music+gfx and 'sdl2' in music+gfx:
    print "Can't link both sdl and sdl2 music or graphics backends at same time"
    Exit(1)


################ create base environment


env = Environment (CFLAGS = [],
                   CXXFLAGS = [],
                   FBC = fbc,
                   VAR_PREFIX = '',
                   **envextra)

# Shocked that scons doesn't provide $HOME
# $DISPLAY is need for both gfx_sdl and gfx_fb (when running tests)
for var in 'PATH', 'DISPLAY', 'HOME', 'EUDIR', 'GCC', 'AS', 'CC', 'CXX':
    if var in os.environ:
        env['ENV'][var] = os.environ[var]

def findtool(envvar, toolname):
    if os.environ.get (envvar):
        ret = os.environ.get (envvar)
    elif WhereIs (target_prefix + toolname):
        ret = target_prefix + toolname
    else:
        ret = toolname
    # standalone builds of FB on Windows do not search $PATH for binaries,
    # so we have to do so for it!
    if win32:
        ret = WhereIs (ret)
    return ret


# If you want to use a different C/C++ compiler do "CC=... CXX=... scons ...".
# If using gengcc=1, CC will not be used by fbc, set GCC envar instead.
GCC = findtool ('GCC', "gcc")
CC = findtool ('CC', "gcc")
CXX = findtool ('CXX', "g++")
MAKE = findtool ('MAKE', 'make')
if not MAKE and win32:
    MAKE = findtool ('MAKE', 'mingw32-make')

clang = False
if CC:
    try:
        clang = 'clang' in CC or 'clang' in os.readlink(WhereIs(CC))
    except OSError:
        pass # readlink throws an error if the arg isn't a symlink
    except AttributeError:
        pass # readlink does not exist at all on Windows
    if not clang and 'GCC' not in os.environ:
        # fbc does not support -gen gcc using clang
        env['ENV']['GCC'] = CC  # fbc only checks GCC variable, not CC
        GCC = CC
    env.Replace (CC = CC)
if CXX:
    env.Replace (CXX = CXX)

#gcc = env['ENV'].get('GCC', env['ENV'].get('CC', 'gcc'))
#gcc = CC or WhereIs(target + "-gcc") or WhereIs("gcc")

fullgccversion = get_command_output(GCC, "-dumpversion")
gccversion = int(fullgccversion.replace('.', ''))  # Convert e.g. 4.9.2 to 492

EUC = WhereIs("euc")  # Euphoria compiler


################ Define Builders and Scanners for FreeBASIC and ReloadBasic

builddir = Dir('.').abspath + os.path.sep
rootdir = Dir('#').abspath + os.path.sep

def prefix_targets(target, source, env):
    target = [File(env['VAR_PREFIX'] + str(a)) for a in target]
    return target, source

def translate_rb(source):
    if source.endswith('.rbas'):
        return env.RB(source)
    return File(source)


if portable and unix and not mac:
    # Only implemented on GNU
    def check_lib_reqs(source, target, env):
        for targ in target:
            ohrbuild.check_lib_requirements(str(targ))
    check_binary = Action(check_lib_reqs, None)  # Action wrapper which prints nothing
else:
    check_binary = None


#variant_baso creates Nodes/object files with filename prefixed with VAR_PREFIX environment variable
variant_baso = Builder (action = '$FBC -c $SOURCE -o $TARGET $FBFLAGS',
                        suffix = '.o', src_suffix = '.bas', single_source = True, emitter = prefix_targets,
                        source_factory = translate_rb)
baso = Builder (action = '$FBC -c $SOURCE -o $TARGET $FBFLAGS',
                suffix = '.o', src_suffix = '.bas', single_source = True, source_factory = translate_rb)
basmaino = Builder (action = '$FBC -c $SOURCE -o $TARGET -m ${SOURCE.filebase} $FBFLAGS',
                    suffix = '.o', src_suffix = '.bas', single_source = True,
                    source_factory = translate_rb)

# Only used when linking with fbc.
# Because fbc ignores all but the last -Wl flag, have to concatenate them.
basexe = Builder (action = ['$FBC $FBFLAGS -x $TARGET $SOURCES $FBLINKFLAGS ${FBLINKERFLAGS and "-Wl " + ",".join(FBLINKERFLAGS)}',
                            check_binary],
                  suffix = exe_suffix, src_suffix = '.bas', source_factory = translate_rb)

# Not used; use asm=1
basasm = Builder (action = '$FBC -c $SOURCE -o $TARGET $FBFLAGS -r -g',
                suffix = '.asm', src_suffix = '.bas', single_source = True,
                emitter = prefix_targets, source_factory = translate_rb)

# Surely there's a simpler way to do this
def depend_on_reloadbasic_py(target, source, env):
    return (target, source + ['reloadbasic/reloadbasic.py'])

rbasic_builder = Builder (action = [[File('reloadbasic/reloadbasic.py'), '--careful', '$SOURCE', '-o', '$TARGET']],
                          suffix = '.rbas.bas', src_suffix = '.rbas', emitter = depend_on_reloadbasic_py)

# windres is part of mingw, and this is only used with linkgcc anyway.
# FB includes GoRC.exe, but finding that file is too much trouble...
rc_builder = Builder (action = target_prefix + 'windres --input $SOURCE --output $TARGET',
                      suffix = '.obj', src_suffix = '.rc')

bas_scanner = Scanner (function = ohrbuild.basfile_scan,
                       skeys = ['.bas', '.bi'], recursive = True)
hss_scanner = Scanner (function = ohrbuild.hssfile_scan,
                       skeys = ['.hss', '.hsi', '.hsd'], recursive = True)

env['BUILDERS']['Object'].add_action ('.bas', '$FBC -c $SOURCE -o $TARGET $FBFLAGS')
# These are needed for Object() auto-dependency detection
SourceFileScanner.add_scanner ('.bas', bas_scanner)
SourceFileScanner.add_scanner ('.bi', bas_scanner)
SourceFileScanner.add_scanner ('.hss', hss_scanner)

env.Append (BUILDERS = {'BASEXE':basexe, 'BASO':baso, 'BASMAINO':basmaino, 'VARIANT_BASO':variant_baso,
                        'RB':rbasic_builder, 'RC':rc_builder, 'ASM':basasm},
            SCANNERS = [bas_scanner, hss_scanner])


################ Find fbc and get fbcinfo fbcversion

# Headers in fb/ depend on this define
CFLAGS += ['-DFBCVERSION=%d' % fbcversion]

# FB 0.91 added a multithreaded version of libfbgfx
if fbcversion >= 910:
    libfbgfx = 'fbgfxmt'
else:
    libfbgfx = 'fbgfx'


################ Mac SDKs

if mac:
    macsdk = ARGUMENTS.get ('macsdk', '')
    macSDKpath = ''
    if os.path.isdir(FRAMEWORKS_PATH):
        FBLINKERFLAGS += ['-F', FRAMEWORKS_PATH]
        CXXLINKFLAGS += ['-F', FRAMEWORKS_PATH]
    # OS 10.4 is the minimum version supported by SDL 1.2.14 on x86 (README.MacOSX
    # in the SDL source tree seems to be out of date, it doesn't even mention x86_64)
    # and OS 10.6 is the minimum for x86_64. 10.6 was released 2009
    # (See https://playcontrol.net/ewing/jibberjabber/big_behind-the-scenes_chang.html)
    # Note: if we wanted to build a x86/x86_64 fat binary that runs on < 10.6, we need
    # to add LSMinimumSystemVersionByArchitecture to the plist, see above link.
    # Our FB Mac fork also currently targets OS 10.4.
    macosx_version_min = '10.4'
    if 'sdl2' in gfx+music:
        # The minimum target supported by SDL 2 is 10.5 for x86 and 10.6 for x86_64
        # (although OS 10.5 is the first to support x86_64 Cocoa apps),
        # requires SDK 10.7+ to compile.
        macosx_version_min = '10.5'
    if arch == 'x86_64':
        macosx_version_min = '10.6'  # Both SDL 1.2 & 2.0
    if macsdk:
        if macsdk == '10.4':
            # 10.4 has a different naming scheme
            macSDKpath = 'MacOSX10.4u.sdk'
        else:
            macSDKpath = 'MacOSX' + macsdk + '.sdk'
        macSDKpath = '/Developer/SDKs/' + macSDKpath
        if not os.path.isdir(macSDKpath):
            raise Exception('Mac SDK ' + macsdk + ' not installed: ' + macSDKpath + ' is missing')
        macosx_version_min = macsdk
    FBLINKERFLAGS += ['-mmacosx-version-min=' + macosx_version_min]
    CFLAGS += ['-mmacosx-version-min=' + macosx_version_min]


################ Cross-compiling and arch-specific stuff

if target:
    FBFLAGS += ['-target', target]

NO_PIE = '-no-pie'
if android:
    # Android 5.0+ will only run PIE exes, for security reasons (ASLR).
    # However, only Android 4.1+ (APP_PLATFORM android-16) support  PIE exes!
    # This only matters for compiling test cases.
    # A workaround is to use a tool to load a PIE executable as a library
    # and run it on older Android:
    #https://chromium.googlesource.com/chromium/src/+/32352ad08ee673a4d43e8593ce988b224f6482d3/tools/android/run_pie/run_pie.c
    CXXLINKFLAGS += ["-pie"]
elif mac:
    # (This is old logic, probably it should be merged into the branch below,
    # but I don't want to worry about breaking mac builds right now)
    # -no_pie (no position-independent execution) fixes a warning
    CXXLINKFLAGS += ['-Wl,-no_pie']
    NO_PIE = '-no_pie'
elif not win32:
    # Recent versions of some linux distros, such as debian and arch, config
    # GCC to default to PIE on non-x86, but our linkgcc code isn't written
    # to support PIE, causing ld 'relocation' errors. Simplest solution is
    # to disable PIE.
    if not clang and gccversion < 500:
        # gcc 4.9 apparently doesn't have -nopie, so I assume it was added in 5.x
        NO_PIE = None
    elif clang or gccversion < 540:
        # -no-pie was added in gcc 6.
        # But on Ubuntu 16.04 -no-pie exists in gcc 5.4
        # -no-pie was added to clang in July 2017, which I think is clang 5.0
        # Recent clang accepts both, recent gcc only accepts -no-pie
        NO_PIE = '-nopie'
    if NO_PIE:
        # -no-pie is a linker flag, I think the compiler flag is actually
        # -fno-pie (or -fno-PIE?) but I assume the former implies the latter
        CFLAGS += [NO_PIE]
        GENGCC_CFLAGS += [NO_PIE]
        # -no_pie is only needed when linking using gcc, not with linkgcc=0,
        # since apparently it's gcc, not ld, which is defaulting to PIE
        CXXLINKFLAGS += [NO_PIE]


# We set gengcc=True if FB will default to it; we need to know whether it's used
if arch != 'x86' and 'mingw32' not in target:
    gengcc = True
if mac and fbcversion > 220:
    gengcc = True

if arch == 'armv5te':
    # FB puts libraries in 'arm' folder
    FBFLAGS += ["-arch", arch]
elif arch == 'armv7-a':
    FBFLAGS += ["-arch", arch]
elif arch == 'aarch64':
    FBFLAGS += ["-arch", arch]
elif arch == 'x86':
    FBFLAGS += ["-arch", "686"]  # "x86" alias not recognised by FB yet
    CFLAGS.append ('-m32')
    if not clang:
        # Recent versions of GCC default to assuming the stack is kept 16-byte aligned
        # (which is a recent change in the Linux x86 ABI) but fbc's GAS backend is not yet updated for that
        # I don't know what clang does, but it doesn't support this commandline option.
        CFLAGS.append ('-mpreferred-stack-boundary=2')
    # gcc -m32 on x86_64 defaults to enabling SSE and SSE2, so disable that,
    # except on Intel Macs, where it is both always present, and required by system headers
    if not mac:
        CFLAGS.append ('-mno-sse')
elif arch == 'x86_64':
    FBFLAGS += ["-arch", arch]
    CFLAGS.append ('-m64')
    # This also causes FB to default to -gen gcc, as -gen gas not supported
    # (therefore we don't need to pass -mpreferred-stack-boundary=2)
elif arch == '(see target)':
    pass  # We let fbc figure it out from the target
elif arch == default_arch:
    # This happens on 32bit arm platforms, where default_arch == 'arm'.
    # We don't need to know a more specific CPU arch, we only need that
    # for the ABI when compiling for Android.
    pass
else:
    print "Error: Unknown architecture %s" % arch
    Exit(1)

# If cross compiling, do a sanity test
if not android_source:
    gcctarget = get_command_output(GCC, "-dumpmachine")
    print "Using target:", target, " arch:", arch, " gcc:", GCC, " cc:", CC, " gcctarget:", gcctarget, " gccversion:", gccversion, " fbcversion:", fbcversion
    # If it contains two dashes it looks like a target triple
    if target_prefix and target_prefix != gcctarget + '-':
        print "Error: This GCC doesn't target " + target_prefix
        print ("You need to either pass 'target' as a target triple (e.g. target=arm-linux-androideabi) and "
               "ensure that the toolchain executables (e.g. arm-linux-androideabi-gcc) "
               "are in your PATH, or otherwise set CC, CXX, and AS environmental variables.")
        Exit(1)

if gengcc:
    FBFLAGS += ["-gen", "gcc"]
    # -exx especially results in a lot of labelled goto use, which confuses gcc 4.8+, which tries harder to throw this warning.
    # (This flag only recognised by recent gcc)
    if gccversion >= 480:
        GENGCC_CFLAGS.append ('-Wno-maybe-uninitialized')
        # (The following is not in gcc 4.2)
        # Ignore warnings due to using an array lbound > 0
        GENGCC_CFLAGS.append ('-Wno-array-bounds')
    if gccversion >= 900:
        # Workaround an error. See https://sourceforge.net/p/fbc/bugs/904/
        GENGCC_CFLAGS.append ('-Wno-format')
    # Ignore annoying warning which is an fbc bug
    GENGCC_CFLAGS.append ('-Wno-missing-braces')
    if asan:
        # Use AddressSanitizer in C files produced by fbc
        GENGCC_CFLAGS.append ('-fsanitize=address')
    if len(GENGCC_CFLAGS):
        # NOTE: You can only pass -Wc (which passes flags on to gcc) once to fbc; the last -Wc overrides others!
        # NOTE: GENGCC_CFLAGS isn't used on android
        FBFLAGS += ["-Wc", ','.join (GENGCC_CFLAGS)]

if mac:
    # Doesn't have --gc-sections. This is similar, but more aggressive than --gc-sections
    CXXLINKFLAGS += ['-Wl,-dead_strip']
else:
    # --gc-sections decreases filesize, but unfortunately doesn't remove symbols for dropped sections,
    # not even with -flto or --strip-discarded!
    CXXLINKFLAGS += ['-Wl,--gc-sections']


################ A bunch of stuff for linking

if linkgcc:
    # Link using g++ instead of fbc; this makes it easy to link correct C++ libraries, but harder to link FB

    # Find the directory where the FB libraries are kept.
    if fbcversion >= 1030:
        # Take the last line, in case -v is in FBFLAGS
        libpath = get_command_output (fbc, ["-print", "fblibdir"] + FBFLAGS).split('\n')[-1]
        checkfile = os.path.join (libpath, 'fbrt0.o')
        if not os.path.isfile (checkfile):
            print "Error: This installation of FreeBASIC doesn't support this target-arch combination;\n" + repr(checkfile) + " is missing."
            Exit(1)
    else:
        # Manually determine library location (TODO: delete this if certainly not supporting FB 1.02 any more)
        fbc_path = os.path.dirname(os.path.realpath(fbc_binary))
        fblibpaths = [[fbc_path, '..', 'lib', 'freebasic'],  # Normal
                      [fbc_path, 'lib'],   # Standalone
                      [fbc_path, '..', 'lib'],
                      ['/usr/share/freebasic/lib'],
                      ['/usr/local/lib/freebasic']]
        # For each of the above possible library paths, check four possible target subdirectories:
        # (FB changes where the libraries are stored every other month)
        targetdirs = [ [], [target + '-' + arch], [arch + '-' + target], [target] ]

        for path, targetdir in itertools.product(fblibpaths, targetdirs):
            libpath = os.path.join(*(path + targetdir))
            print "Looking for FB libs in", libpath
            if os.path.isfile(os.path.join(libpath, 'fbrt0.o')):
                break
        else:
            raise Exception("Couldn't find the FreeBASIC lib directory")

    # This causes ld to recursively search the dependencies of linked dynamic libraries
    # for more dependencies (specifically SDL on X11, etc)
    # Usually the default, but overridden on some distros. Don't know whether GOLD ld supports this.
    if not mac:
        CXXLINKFLAGS += ['-Wl,--add-needed']

    # Passing this -L option straight to the linker is necessary, otherwise gcc gives it
    # priority over the default library paths, which on Windows means using FB's old mingw libraries
    CXXLINKFLAGS += ['-Wl,-L' + libpath, os.path.join(libpath, 'fbrt0.o'), '-lfbmt']
    if verbose:
        CXXLINKFLAGS += ['-v']
    if GCC_strip:
        # Strip debug info but leave in the function (and unwanted global) symbols.
        # Result is about 600KB larger than a full strip, and after running
        # strip_unwanted_syms below, down to 280KB.
        CXXLINKFLAGS += ['-Wl,-S']
    if win32:
        # win32\ld_opt_hack.txt contains --stack option which can't be passed using -Wl
        CXXLINKFLAGS += ['-static-libgcc', '-static-libstdc++', '-Wl,@win32/ld_opt_hack.txt']
    else:
        if 'fb' in gfx:
            # Program icon required by fbgfx, but we only provide it on Windows,
            # because on X11 need to provide it as an XPM instead
            CXXLINKFLAGS += ['linux/fb_icon.c']
        # Android doesn't have ncurses, and libpthread is part of libc
        if not android:
            # The following are required by libfb (not libfbgfx)
            CXXLINKFLAGS += ['-lpthread']
            if portable and not mac:
                #CXXLINKFLAGS += ['/usr/lib/libncurses.5.dylib']  # Mac: -l: syntax not supported
                # FIXME: linking against this older version when libfb was compiled
                # against a newer version seems to cause problems: in programs like unlump
                # waiting for a keypress doesn't work.
                CXXLINKFLAGS += ['-l:libncurses.so.5']
            else:
                CXXLINKFLAGS += ['-lncurses']  # would be libncurses.so.6 since ~2015

    if mac:
        if fbcversion <= 220:
            # The old port of FB v0.22 to mac requires this extra file (it was a kludge)
            CXXLINKFLAGS += [os.path.join(libpath, 'operatornew.o')]
        if macSDKpath:
            CXXLINKFLAGS += ["-isysroot", macSDKpath]  # "-static-libgcc", '-weak-lSystem']

    def compile_main_module(target, source, env):
        """
        This is the emitter for BASEXE when using linkgcc: it compiles sources if needed, where
        the first specified module is the main module (-m flag), and rest are regular modules.
        """
        def to_o((i, obj)):
            if str(obj).endswith('.bas'):
                if i == 0:
                    return env.BASMAINO (obj)
                else:
                    return env.BASO (obj)
            return obj
        return target, map(to_o, enumerate(source))

    if pdb:
        # Note: to run cv2pdb you need Visual Studio or Visual C++ Build Tools installed,
        # but not necessarily in PATH. (Only a few dlls and mspdbsrv.exe actually needed.)
        # By default cv2pdb modifies the exe in-place, stripping DWARF debug info,
        # pass NUL as second argument to throw away the stripped copy.
        handle_symbols = os.path.join('support', 'cv2pdb') + ' $TARGET '
        # Actually, we need to always strip the debug info, because cv2pdb puts the GUID
        # of the .pdb in the exe at the same time, without which the .pdb doesn't work.
        # It would be possible to modify cv2pdb to add the GUID without stripping
        # (by modifying PEImage::replaceDebugSection())
        strip = True  #debug > 0
        if strip == False:
            # Do not strip
            handle_symbols += 'NUL'
        else:
            handle_symbols += '$TARGET'
        handle_symbols += ' win32/${TARGET.filebase}.pdb'
        if not sys.platform.startswith('win'):
            handle_symbols = 'WINEDEBUG=fixme-all wine ' + handle_symbols
            # If cv2pdb fails (because Visual Studio is missing) continue without error
            handle_symbols += " || true"
        else:
            handle_symbols += " || exit /b 0"   # aka " || true"
    else:
        # Untested on mac. And I would guess not needed, due to -dead_strip
        if GCC_strip and not mac:
            # This strips ~330KB from each of Game and Custom, leaving ~280KB of symbols
            def strip_unwanted_syms(source, target, env):
                # source are the source objects for the executable and target is the exe
                ohrbuild.strip_nonfunction_symbols(target[0].path, target_prefix, builddir, env)
            handle_symbols = Action(strip_unwanted_syms, None)  # Action wrapper to print nothing
        else:
            handle_symbols = None

    if mac:
        # -( -) not supported
        basexe_gcc_action = '$CXX $CXXFLAGS -o $TARGET $SOURCES $CXXLINKFLAGS'
    else:
        basexe_gcc_action = '$CXX $CXXFLAGS -o $TARGET $SOURCES "-Wl,-(" $CXXLINKFLAGS "-Wl,-)"'

    basexe_gcc = Builder (action = [basexe_gcc_action, check_binary, handle_symbols], suffix = exe_suffix,
                          src_suffix = '.bas', emitter = compile_main_module)

    env['BUILDERS']['BASEXE'] = basexe_gcc

if not linkgcc:
    if fbcversion >= 1060:
        # Ignore #inclib directives (specifically, so we can include modplug.bi)
        FBLINKFLAGS += ['-noobjinfo']
    if win32:
        # Link statically
        FBLINKFLAGS += ['-l', ':libstdc++.a']  # Yes, fbc accepts this argument form
    else:
        FBLINKFLAGS += ['-l','stdc++'] #, '-l','gcc_s']
    if mac and fbcversion > 220:
        # libgcc_eh (a C++ helper library) is only needed when linking/compiling with old versions of Apple g++
        # including v4.2.1; for most compiler versions and configuration I tried it is unneeded
        # (Normally fbc links with gcc_eh if required, I wonder what goes wrong here?)
        FBLINKFLAGS += ['-l','gcc_eh']
    if portable:
        # TODO: force link to libncurses.so.5 or libtinfo.so.5
        print "WARNING: can't force libtinfo.so.5\n"

if portable and (unix and not mac):
    # For compatibility with libstdc++ before GCC 5
    # See https://bugzilla.mozilla.org/show_bug.cgi?id=1153109
    # and https://gcc.gnu.org/onlinedocs/libstdc%2B%2B/manual/using_dual_abi.html
    CXXFLAGS.append ("-D_GLIBCXX_USE_CXX11_ABI=0")
    if glibc:
        # For compatibility with glibc < 2.28 when linking with glibc >= 2.28 (2018-08-01)
        # This redirects fcntl (used in libfb) to __wrap_fcntl, defined in lib/wrap_fcntl.c
        # If libfb was compiled against >= 2.28 we need to wrap fcntl64, otherwise fcntl.
        CXXFLAGS.append ("-Wl,--wrap=fcntl")
        CXXFLAGS.append ("-Wl,--wrap=fcntl64")

# As long as exceptions aren't used anywhere and don't have to be propagated between libraries,
# we can link libgcc_s statically, which avoids one more thing that might be incompatible
# (although I haven't seen any problems yet). I think we can use
# -static-libgcc with exceptions, provided we link with g++?
# NOTE: libgcc_s.so still appears in ldd output, but it's no longer listed in objdump -p
# dependencies... hmmm...
# if unix:
#     CXXLINKFLAGS += ['-static-libgcc']

if android_source:
    with open(rootdir + 'android/extraconfig.cfg', 'w+') as fil:
        # Unfortunately the commandergenius port only has a single CFLAGS,
        # which gets used for handwritten C, generated-from-FB C, and C++.
        # It would be better to change that.
        NDK_CFLAGS = CFLAGS[:]
        NDK_CFLAGS.append('--std=c99')  # Needed for compiling array.c, blit.c
        NDK_CFLAGS += ('-Wno-unused-label -Wno-unused-but-set-variable -Wno-maybe-uninitialized '
                       '-Wno-unused-variable -Wno-unused-function -Wno-missing-braces'.split())
        if arch in ('x86', 'x86_64'):
            NDK_CFLAGS.append("-masm=intel")  # for fbc's generated inline assembly
        fil.write('AppCflags="%s"\n' % ' '.join(NDK_CFLAGS))
        fil.write('AppCppflags="%s"\n' % ' '.join(CXXFLAGS))
        if arch == 'armv5te':
            abi = 'armeabi'
        elif arch == 'armv7-a':
            abi = 'armeabi-v7a'
        elif arch == 'aarch64':
            abi = 'arm64-v8a'
            # TODO: To support both 32 and 64 bit ARM apparently need this:
            #abi = 'arm64-v8a,armeabi'
            # to set APP_ABI in project/jni/Settings.mk to that value, but that
            # won't work, need to separately cross-compile FB to C for each arch.
        else:
            abi = arch
        fil.write('MultiABI="%s"\n' % abi)


# With the exception of base_libraries, now have determined all shared variables
# so put them in the shared Environment env. After this point need to modify one of
# the specific Environments.

env['FBFLAGS'] = FBFLAGS
env['CFLAGS'] += CFLAGS + TRUE_CFLAGS
env['CXXFLAGS'] += CFLAGS + CXXFLAGS
env['CXXLINKFLAGS'] = CXXLINKFLAGS
env['FBLINKFLAGS'] = FBLINKFLAGS
env['FBLINKERFLAGS'] = FBLINKERFLAGS

# These no longer have any effect.
del FBFLAGS, TRUE_CFLAGS, GENGCC_CFLAGS, CFLAGS, CXXFLAGS
del CXXLINKFLAGS, FBLINKFLAGS, FBLINKERFLAGS

################ Program-specific stuff starts here

# We have five environments:
# env             : Used for utilities
# +-> commonenv   : Not used directly, but common configuration shared by gameenv and editenv
#     +-> gameenv : For Game
#     +-> editenv : For Custom
# w32_env         : For gfx_directx (completely separate; uses Visual C++)

# Make a base environment for Game and Custom (other utilities use env)
commonenv = env.Clone ()

# Added to env and commonenv
base_modules = []   # modules (any language) shared by all executables (except bam2mid)
#base_libraries defined above

# Added to gameenv and editenv
shared_modules = []  # FB/RB modules shared by, but with separate builds, for Game and Custom

# Added to commonenv
common_modules = []  # other modules (in any language) shared by Game and Custom; only built once
common_libraries = []
common_libpaths = []


################ gfx and music backend modules and libraries

# OS-specific libraries and options for each backend are added below.

gfx_map = {'fb': {'shared_modules': 'gfx_fb.bas', 'common_libraries': libfbgfx},
           'alleg' : {'shared_modules': 'gfx_alleg.bas', 'common_libraries': 'alleg'},
           'sdl' : {'shared_modules': 'gfx_sdl.bas', 'common_libraries': 'SDL'},
           'sdl2' : {'shared_modules': 'gfx_sdl2.bas', 'common_libraries': 'SDL2'},
           'console' : {'shared_modules': 'gfx_console.bas', 'common_modules': 'curses_wrap.c'},
           'directx' : {}, # nothing needed
           'sdlpp': {}     # nothing needed
           }

music_map = {'native':
                 {'shared_modules': 'music_native.bas music_audiere.bas',
                  'common_modules': os.path.join ('audwrap','audwrap.cpp'),
                  'common_libraries': 'audiere', 'common_libpaths': '.'},
             'native2':
                 {'shared_modules': 'music_native2.bas music_audiere.bas',
                  'common_modules': os.path.join ('audwrap','audwrap.cpp'),
                  'common_libraries': 'audiere', 'common_libpaths': '.'},
             'sdl':
                 {'shared_modules': 'music_sdl.bas sdl_lumprwops.bas',
                  'common_libraries': 'SDL SDL_mixer'},
             'sdl2':
                 {'shared_modules': 'music_sdl.bas',
                  'common_libraries': 'SDL2 SDL2_mixer',
                  'fb_defines': '-d SDL_MIXER2'},
             'allegro':
                 {'shared_modules': 'music_allegro.bas',
                  'common_libraries': 'alleg'},
             'silence':
                 {'shared_modules': 'music_silence.bas'}
            }

fb_defines = []

for k in gfx:
    for k2, v2 in gfx_map[k].items ():
        globals()[k2] += v2.split (' ')

for k in music:
    for k2, v2 in music_map[k].items ():
        globals()[k2] += v2.split (' ')

commonenv['FBFLAGS'] += fb_defines
del fb_defines

################ OS-specific modules and libraries

# This module is OS-specific but shared by Windows (winsock) and Unix. A web port probably won't use it.
base_modules += ['os_sockets.c']

if win32:
    base_modules += ['os_windows.bas', 'os_windows2.c', 'lib/win98_compat.bas',
                     'lib/msvcrt_compat.c', 'gfx_common/win_error.c']
    # winmm needed for MIDI, used by music backends but also by miditest
    # psapi.dll needed just for get_process_path() and memory_usage(). Not present on Win98 unfortunately,
    # so now we dynamically link it.
    # ole32.dll and shell32.dll needed just for open_document()
    # advapi32 is needed by libfb[mt]
    # Strangely advapi32 and shell32 are automatically added by ld when using linkgcc=1 but not linkgcc=0
    base_libraries += ['winmm', 'ole32', 'gdi32', 'shell32', 'advapi32', 'wsock32' if win95 else 'ws2_32']
    if win95:
        env['CFLAGS'] += ['-D', 'USE_WINSOCK1']
    common_libraries += [libfbgfx]
    commonenv['FBFLAGS'] += ['-s','gui']  # Change to -s console to see 'print' statements in the console!
    commonenv['CXXLINKFLAGS'] += ['-lgdi32', '-Wl,--subsystem,windows']
    #env['CXXLINKFLAGS'] += ['win32/CrashRpt1403.lib']  # If not linking the .dll w/ LoadLibrary
    env['CFLAGS'] += ['-I', 'win32/include']
    if 'sdl' in gfx or 'fb' in gfx:
        common_modules += ['lib/SDL/SDL_windowsclipboard.c', 'gfx_common/ohrstring.cpp']
    if 'console' in gfx:
        common_libraries += ['pdcurses']
    # if 'sdl' in music:
    #     # libvorbisfile is linked into SDL_mixer.dll which has been compiled to export its symbols
    #     commonenv['FBFLAGS'] += ['-d', 'HAVE_VORBISFILE']
elif mac:
    base_modules += ['os_unix.c', 'os_unix2.bas']
    common_modules += ['os_unix_wm.c']
    common_libraries += ['Cocoa']  # For CoreServices
    if 'sdl' in gfx:
        common_modules += ['mac/SDLmain.m', 'lib/SDL/SDL_cocoaclipboard.m']
        commonenv['FBFLAGS'] += ['-entry', 'SDL_main']
        if env.WhereIs('sdl-config'):
            commonenv.ParseConfig('sdl-config --cflags')
        else:
            commonenv['CFLAGS'] += ["-I", "/Library/Frameworks/SDL.framework/Headers", "-I", FRAMEWORKS_PATH + "/SDL.framework/Headers"]
    if 'sdl2' in gfx:
        # SDL2 does not have SDLmain
        if env.WhereIs('sdl2-config'):
            commonenv.ParseConfig('sdl2-config --cflags')
        else:
            commonenv['CFLAGS'] += ["-I", "/Library/Frameworks/SDL2.framework/Headers", "-I", FRAMEWORKS_PATH + "/SDL2.framework/Headers"]
    # if 'sdl' in music:
    #     # libvorbisfile is linked into SDL_mixer.framework which has been compiled to export its symbols
    #     commonenv['FBFLAGS'] += ['-d', 'HAVE_VORBISFILE']

elif android:
    # liblog for __android_log_print/write
    base_libraries += ['log']
    base_modules += ['os_unix.c', 'os_unix2.bas']
    common_modules += ['os_unix_wm.c']
elif unix:  # Unix+X11 systems: Linux & BSD
    base_modules += ['os_unix.c', 'os_unix2.bas']
    common_modules += ['os_unix_wm.c', 'lib/x11_printerror.c']
    if portable:
        # To support old libstdc++.so versions
        base_modules += ['lib/stdc++compat.cpp']
        if glibc:
            base_modules += ['lib/wrap_fcntl.c']
    if 'sdl' in gfx or 'fb' in gfx:
        common_modules += ['lib/SDL/SDL_x11clipboard.c', 'lib/SDL/SDL_x11events.c']
    if gfx != ['console']:
        # All graphical gfx backends need the X11 libs
        common_libraries += 'X11 Xext Xpm Xrandr Xrender'.split (' ')
    # common_libraries += ['vorbisfile']
    # commonenv['FBFLAGS'] += ['-d','HAVE_VORBISFILE']


################ Add the libraries to env and commonenv

if win32:
    # win32/ contains .a and .dll.a files
    env['FBLINKFLAGS'] += ['-p', 'win32']
    env['CXXLINKFLAGS'] += ['-L', 'win32']
    common_libpaths += ['win32']

commonenv['CXXLINKFLAGS'] += ['-L' + path for path in common_libpaths]
commonenv['FBLINKFLAGS'] += Flatten ([['-p', v] for v in common_libpaths])

for lib in base_libraries:
    env['CXXLINKFLAGS'] += ['-l' + lib]
    env['FBLINKFLAGS'] += ['-l', lib]

for lib in base_libraries + common_libraries:
    if mac and lib in ('SDL', 'SDL_mixer', 'SDL2', 'SDL2_mixer', 'Cocoa'):
        # Use frameworks rather than normal unix libraries
        # (Note: linkgcc=0 does not work on Mac because the #inclib "SDL" in the
        # SDL headers causes fbc to pass -lSDL to the linker, which can't be
        # found (even if we add the framework path, because it's not called libSDL.dylib))
        commonenv['CXXLINKFLAGS'] += ['-framework', lib]
        commonenv['FBLINKERFLAGS'] += ['-framework', lib]
    else:
        commonenv['CXXLINKFLAGS'] += ['-l' + lib]
        commonenv['FBLINKFLAGS'] += ['-l', lib]


################ Modules

# The following are linked into all executables, except miditest.
base_modules +=   ['util.bas',
                   'blit.c',
                   'base64.c',
                   'unicode.c',
                   'array.c',
                   'miscc.c',
                   'fb/error.c',
                   'lib/sha1.c',
                   'lib/lodepng.c',  # Only for lodepng_gzip.c
                   'lib/lodepng_gzip.c',  # Only for filetest
                   'filelayer.cpp',
                   'lumpfile.bas',
                   'vector.bas']

# Modules shared by the reload utilities, additional to base_modules
reload_modules =  ['reload.bas',
                   'reloadext.bas']

# The following are built twice, for Game and Custom, so may use #ifdef to change behaviour
# (.bas files only) 
shared_modules += ['allmodex',
                   'audiofile',
                   'backends',
                   'misc',
                   'bam2mid',
                   'common.rbas',
                   'common_menus',
                   'bcommon',
                   'menus',
                   'browse',
                   'loading.rbas',
                   'reload',
                   'reloadext',
                   'sliceedit',
                   'slices',
                   'plankmenu.bas',
                   'thingbrowser']

# (.bas files only) 
edit_modules = ['custom',
                'customsubs.rbas',
                'specialslices',
                'drawing',
                'textboxedit',
                'scriptedit',
                'subs4',
                'mapsubs',
                'attackedit',
                'audioedit',
                'enemyedit',
                'fontedit',
                'formationedit',
                'globalstredit',
                'heroedit.rbas',
                'menuedit',
                'itemedit',
                'shopedit',
                'reloadedit',
                'editedit',
                'editrunner',
                'distribmenu']

# (.bas files only) 
game_modules = ['game',
                'bmod.rbas',
                'bmodsubs',
                'menustuf.rbas',
                'moresubs.rbas',
                'scriptcommands',
                'yetmore2',
                'walkabouts',
                'savegame.rbas',
                'scripting',
                'oldhsinterpreter',
                'purchase.rbas',
                'pathfinding.bas']

# The following are built only once and linked into Game and Custom
common_modules += ['rasterizer.cpp',
                   'matrixMath.cpp',
                   'rotozoom.c',
                   'surface.cpp',
                   'lib/gif.cpp',
                   'lib/ujpeg.c']



################ ver.txt (version info) build rule

archinfo = arch
if arch == '(see target)':
    archinfo = target
def version_info(source, target, env):
    ohrbuild.verprint (gfx, music, fbc, archinfo, fullgccversion, asan, portable, pdb, builddir, rootdir)
VERPRINT = env.Command (target = ['#/ver.txt', '#/iver.txt', '#/distver.bat'],
                        source = ['codename.txt'], 
                        action = env.Action(version_info, "Generating ver.txt"))
AlwaysBuild(VERPRINT)


################ Generate object file Nodes

# Note that base_objects are not built in commonenv!
base_objects = Flatten([env.Object(a) for a in base_modules])  # concatenate NodeLists
common_objects = base_objects + Flatten ([commonenv.Object(a) for a in common_modules])
# Plus unique module included by utilities but not Game or Custom
base_objects.extend (env.Object ('common_base.bas'))

gameenv = commonenv.Clone (VAR_PREFIX = 'game-', FBFLAGS = commonenv['FBFLAGS'] + \
                      ['-d','IS_GAME', '-m','game'])
editenv = commonenv.Clone (VAR_PREFIX = 'edit-', FBFLAGS = commonenv['FBFLAGS'] + \
                      ['-d','IS_CUSTOM', '-m','custom'])

#now... GAME and CUSTOM

gamesrc = common_objects[:]
for item in game_modules:
    gamesrc.extend (gameenv.BASO (item))
for item in shared_modules:
    gamesrc.extend (gameenv.VARIANT_BASO (item))

editsrc = common_objects[:]
for item in edit_modules:
    editsrc.extend (editenv.BASO (item))
for item in shared_modules:
    editsrc.extend (editenv.VARIANT_BASO (item))

# Sort RB modules to the front so they get built first, to avoid bottlenecks
gamesrc.sort (key = lambda node: 0 if '.rbas' in node.path else 1)
editsrc.sort (key = lambda node: 0 if '.rbas' in node.path else 1)

# For reload utilities
reload_objects = base_objects + Flatten ([env.Object(a) for a in reload_modules])

# For utiltest
base_objects_without_util = [a for a in base_objects if str(a) != 'util.o']


################ Executable definitions
# Executables are explicitly placed in rootdir, otherwise they would go in build/

if win32:
    gamename = 'game'
    editname = 'custom'
    # The .rc file includes game.ico or custom.ico and is compiled to an .o file
    # (If linkgcc=0, could just pass the .rc to fbc)
    gamesrc += Depends(gameenv.RC('gicon.o', 'gicon.rc'), 'game.ico')
    editsrc += Depends(editenv.RC('cicon.o', 'cicon.rc'), 'custom.ico')
else:
    gamename = 'ohrrpgce-game'
    editname = 'ohrrpgce-custom'

def env_exe(name, **kwargs):
    ret = env.BASEXE (rootdir + name, **kwargs)
    Alias (name, ret)
    return ret

GAME = gameenv.BASEXE   (rootdir + gamename, source = gamesrc)
CUSTOM = editenv.BASEXE (rootdir + editname, source = editsrc)
GAME = GAME[0]  # first element of NodeList is the executable
CUSTOM = CUSTOM[0]
env_exe ('bam2mid', source = ['bam2mid.bas'] + base_objects)
env_exe ('miditest')
env_exe ('unlump', source = ['unlump.bas'] + base_objects)
env_exe ('relump', source = ['relump.bas'] + base_objects)
env_exe ('dumpohrkey', source = ['dumpohrkey.bas'] + base_objects)

# Put this into a function so that we only call get_euphoria_version() when compiling
def compile_hspeak(target, source, env):
    if not EUC:
        print "Euphoria is required to compile HSpeak but is not installed (euc is not in the PATH)"
        Exit(1)
    hspeak_builddir = builddir + "hspeak"
    euc_extra_args = ''
    # Work around Euphoria bug (in 4.0/4.1), where $EUDIR is ignored if another
    # copy of Euphoria is installed system-wide
    if 'EUDIR' in env['ENV']:
        euc_extra_args += ' -eudir ' + env['ENV']['EUDIR']
    if ohrbuild.get_euphoria_version() >= 40100 and NO_PIE and not mac:
        # On some systems (not including mac) gcc defaults to building PIE
        # executables, but the linux euphoria 4.1.0 builds aren't built for PIE/PIC,
        # resulting in a "recompile with -fPIC" error.
        # But the -extra-lflags option is new in Eu 4.1.
        euc_extra_args += ' -extra-lflags ' + NO_PIE

    actions = [
        # maxsize: cause euc to split hspeak.exw to multiple .c files
        "euc -con -gcc hspeak.exw -verbose -maxsize 5000 -makefile -build-dir %s " % hspeak_builddir + euc_extra_args,
        "%s -j%d -C %s -f hspeak.mak" % (MAKE, GetOption('num_jobs'), hspeak_builddir)
    ]
    Action(actions)(target, source, env)

# HSpeak is built by translating to C, generating a Makefile, and running make.
HSPEAK = env.Command (rootdir + 'hspeak', source = ['hspeak.exw', 'hsspiffy.e'] + Glob('euphoria/*.e'),
                      action = Action(compile_hspeak, "Compiling hspeak"))

RELOADTEST = env_exe ('reloadtest', source = ['reloadtest.bas'] + reload_objects)
x2rsrc = ['xml2reload.bas'] + reload_objects
if win32:
    # Hack around our provided libxml2.a lacking a function. (Was less work than recompiling)
    x2rsrc.append (env.Object('win32/utf8toisolat1.c'))
XML2RELOAD = env_exe ('xml2reload', source = x2rsrc, FBLINKFLAGS = env['FBLINKFLAGS'] + ['-l','xml2'], CXXLINKFLAGS = env['CXXLINKFLAGS'] + ['-lxml2'])
RELOAD2XML = env_exe ('reload2xml', source = ['reload2xml.bas'] + reload_objects)
RELOADUTIL = env_exe ('reloadutil', source = ['reloadutil.bas'] + reload_objects)
RBTEST = env_exe ('rbtest', source = [env.RB('rbtest.rbas'), env.RB('rbtest2.rbas')] + reload_objects)
VECTORTEST = env_exe ('vectortest', source = ['vectortest.bas'] + base_objects)
# Compile util.bas as a main module to utiltest.o to prevent its linkage in other binaries
UTILTEST = env_exe ('utiltest', source = env.BASMAINO('utiltest.o', 'util.bas') + base_objects_without_util)
FILETEST = env_exe ('filetest', source = ['filetest.bas'] + base_objects)
env_exe ('slice2bas', source = ['slice2bas.bas'] + reload_objects)

Alias ('reload', [RELOADUTIL, RELOAD2XML, XML2RELOAD, RELOADTEST, RBTEST])

if android_source:
    # android_source is a hack:
    # Don't produce any .o files, just produce and copy .c/.cpp files to a directory for sdl-android's build system
    srcs, actions = ohrbuild.android_source_actions (gamesrc, rootdir, rootdir + 'android/tmp')
    Alias('game', source = srcs, action = actions)
    srcs, actions = ohrbuild.android_source_actions (editsrc, rootdir, rootdir + 'android/tmp')
    Alias('custom', source = srcs, action = actions)
    if 'game' not in COMMAND_LINE_TARGETS and 'custom' not in COMMAND_LINE_TARGETS:
        raise Exception("Specify either 'game' or 'custom' as a target with android-source=1")
else:
    Alias('game', GAME)
    Alias('custom', CUSTOM)

# building gfx_directx.dll (can't crosscompile)
if platform.system () == 'Windows':
    directx_sources = ['d3d.cpp', 'didf.cpp', 'gfx_directx.cpp', 'joystick.cpp', 'keyboard.cpp',
                       'midsurface.cpp', 'mouse.cpp', 'window.cpp']
    directx_sources = [os.path.join('gfx_directx', f) for f in directx_sources]
    directx_sources += ['gfx_common/ohrstring.cpp', 'gfx_common/win_error.c',
                        'lib/msvcrt_compat.c', 'lib/SDL/SDL_windowsclipboard.c']

    # Create environment for compiling gfx_directx.dll
    # $OBJPREFIX is prefixed to the name of each object file, to ensure there are no clashes
    w32_env = Environment (OBJPREFIX = 'gfx_directx-')
    w32_env['ENV']['PATH'] = os.environ['PATH']
    if "Include" in os.environ:
        w32_env.Append(CPPPATH = os.environ['Include'].split(';'))
    if "Lib" in os.environ:
        w32_env.Append(LIBPATH = os.environ['Lib'].split(';'))
    if 'DXSDK_DIR' in os.environ:
        w32_env.Append(CPPPATH = [os.path.join(os.environ['DXSDK_DIR'], 'Include')])
        w32_env.Append(LIBPATH = [os.path.join(os.environ['DXSDK_DIR'], 'Lib', 'x86')])
    w32_env.Append(CPPPATH = "gfx_common")

    if profile:
        # Profile using MicroProfiler, which uses instrumentation (counting function calls)
        # There are many other available profilers based on either instrumentation or
        # statistical sampling (like gprof), so this can be easily adapted.
        dllpath = WhereIs("micro-profiler.dll", os.environ['PATH'], "dll")

        if not dllpath:
            # MicroProfiler is MIT licensed, but you need to install it using
            # regsvr32 (with admin privileges) for it to work, so there's little
            # benefit to distributing the library ourselves.
            print "MicroProfiler is not installed. You can install it from"
            print "https://visualstudiogallery.msdn.microsoft.com/800cc437-8cb9-463f-9382-26bedff7cdf0"
            Exit(1)

        # if optimisations == False:
        #     # MidSurface::copySystemPage() and Palette::operator[] are extremely slow when
        #     # profiled without optimisation, so don't instrument MidSurface.
        #     w32_no_profile_env = w32_env.Clone ()
        #     midsurface = os.path.join ('gfx_directx', 'midsurface.cpp')
        #     directx_sources.remove (midsurface)
        #     directx_sources.append (w32_no_profile_env.Object (midsurface))

        MPpath = os.path.dirname(dllpath) + os.path.sep
        directx_sources.append (w32_env.Object('micro-profiler.initalizer.obj', MPpath + 'micro-profiler.initializer.cpp'))
        w32_env.Append (LIBPATH = MPpath)
        # Call _penter and _pexit in every function.
        w32_env.Append (CPPFLAGS = ['/Gh', '/GH'])

    RESFILE = w32_env.RES ('gfx_directx/gfx_directx.res', source = 'gfx_directx/gfx_directx.rc')
    Depends (RESFILE, ['gfx_directx/help.txt', 'gfx_directx/Ohrrpgce.bmp'])
    directx_sources.append (RESFILE)

    # Enable exceptions, most warnings, treat .c files are C++, unicode/wide strings
    w32_env.Append (CPPFLAGS = ['/EHsc', '/W3', '/TP'], CPPDEFINES = ['UNICODE', '_UNICODE', 'FBCVERSION=%d' % fbcversion])

    if profile:
        # debug info, static link VC9.0 runtime lib, but no link-time code-gen
        # as it inlines too many functions, which don't get instrumented
        w32_env.Append (CPPFLAGS = ['/Zi', '/MT'], LINKFLAGS = ['/DEBUG'])
        # Optimise for space (/O1) to inline trivial functions, otherwise takes seconds per frame
        w32_env.Append (CPPFLAGS = ['/O2' if optimisations else '/O1'])
    elif optimisations == False:
        # debug info, runtime error checking, static link debugging VC9.0 runtime lib, no optimisation
        w32_env.Append (CPPFLAGS = ['/Zi', '/RTC1', '/MTd', '/Od'], LINKFLAGS = ['/DEBUG'])
    else:
        # static link VC9.0 runtime lib, optimise, whole-program optimisation
        w32_env.Append (CPPFLAGS = ['/MT', '/O2', '/GL'], LINKFLAGS = ['/LTCG'])

    #if pdb:  # I see no reason not to build a .pdb
    if True:
        # /OPT:REF enables dead code removal (trimming 110KB) which is disabled by default by /DEBUG,
        # while /OPT:NOICF disables identical-function-folding, which is confusing while debugging
        w32_env.Append (CPPFLAGS = ['/Zi'],
                        LINKFLAGS = ['/DEBUG', '/PDB:' + rootdir + 'win32/gfx_directx.pdb',
                                     '/OPT:REF', '/OPT:NOICF'])

    w32_env.SharedLibrary (rootdir + 'gfx_directx.dll', source = directx_sources,
                          LIBS = ['user32', 'ole32', 'gdi32'])
    TEST = w32_env.Program (rootdir + 'gfx_directx_test1.exe', source = ['gfx_directx/gfx_directx_test1.cpp'],
                            LIBS = ['user32'])
    Alias ('gfx_directx_test', TEST)


################ Non-file/action targets

def Phony(name, source, action, message = None):
    """Define a target which performs some action (e.g. a Python function) unconditionally"""
    if message:
        action = env.Action(action, message)
    node = env.Alias(name, source = source, action = action)
    AlwaysBuild(node)  # Run even if there happens to be a file of the same name
    return node

def RPGWithScripts(rpg, main_script):
    """Construct an (Action) node for an .rpg, which updates it by re-importing
    an .hss if it (or any included script file) has been modified."""
    sources = [main_script, "plotscr.hsd"]
    if EUC:
        # Only include hspeak as dependency if Euphoria is installed, otherwise can't run tests
        sources += [HSPEAK]
    action = env.Action(CUSTOM.abspath + ' --nowait ' + rpg + ' ' + main_script)
    # Prepending # means relative to rootdir, otherwise this a rule to build a file in build/
    node = env.Command('#' + rpg, source = sources, action = action)
    Precious(node)  # Don't delete the .rpg before "rebuilding" it
    NoClean(node)   # Don't delete the .rpg with -c
    # Note: the following Ignore does NOT work if the .hss file manually includes plotscr.hsd/scancode.hsi!
    Ignore(node, [CUSTOM, "plotscr.hsd", "scancode.hsi"])  # Don't reimport just because these changed...
    Requires(node, CUSTOM)  # ...but do rebuild Custom before reimporting (because of maxScriptCmdID, etc, checks)
    # Note: unfortunately this Requires causes scons to make sure CUSTOM is
    # up to date even if it's not called; I don't know how to avoid that.
    SideEffect (Alias ('c_debug.txt'), node)  # Prevent more than one copy of Custom from running at once
    return node

T = 'testgame/'
AUTOTEST = Phony ('autotest_rpg',
                  source = [GAME, RPGWithScripts(T+'autotest.rpg', T+'autotest.hss')],
                  action =
                  [GAME.abspath + ' --log . --runfast testgame/autotest.rpg -z 2',
                   'grep -q "TRACE: TESTS SUCCEEDED" g_debug.txt'])
env.Alias ('autotest', source = AUTOTEST)
INTERTEST = Phony ('interactivetest',
                   source = [GAME, RPGWithScripts(T+'interactivetest.rpg', T+'interactivetest.hss')],
                   action =
                   [GAME.abspath + ' --log . --runfast testgame/interactivetest.rpg -z 2'
                    ' --replayinput testgame/interactivetest.ohrkey',
                    'grep -q "TRACE: TESTS SUCCEEDED" g_debug.txt'])
# This prevents more than one copy of Game from being run at once
# (doesn't matter where g_debug.txt is actually placed).
# The Alias prevents scons . from running the tests.
SideEffect (Alias ('g_debug.txt'), [AUTOTEST, INTERTEST])

HSPEAKTEST = Phony ('hspeaktest', source = HSPEAK, action =
                    [rootdir + 'hspeaktest.py testgame/parser_tests.hss'])

# Note: does not include hspeaktest, because it fails, and Euphoria may not be installed
tests = [exe.abspath for exe in Flatten([RELOADTEST, RBTEST, VECTORTEST, UTILTEST, FILETEST])]
TESTS = Phony ('test', source = tests + [AUTOTEST, INTERTEST], action = tests)
Alias ('tests', TESTS)

def packager(target, source, env):
    action = str(target[0])  # eg 'install'
    if mac or android or not unix:
        print "The '%s' action is only implemented on Unix systems." % action
        return 1
    if action == 'install' and dry_run:
        print "dry_run option not implemented for 'install' action"
        return 1
    sys.path += ['linux']
    import ohrrpgce
    getattr(ohrrpgce, action)(destdir, prefix, dry_run = dry_run)

Phony ('install', source = [GAME, CUSTOM, HSPEAK], action = packager, message = "Installing...")
Phony ('uninstall', source = [], action = packager, message = "Uninstalling..." + (dry_run and " (dry run)" or ""))

Default (GAME)
Default (CUSTOM)

#print [str(a) for a in FindSourceFiles(GAME)]

Help ("""
Usage:  scons [SCons options] [options] [targets]

Options:
  gfx=BACKENDS        Graphics backends, concatenated with +. Options:
                        """ + " ".join (gfx_map.keys ()) + """
                      At runtime, backends are tried in the order specified.
                      Current (default) value: """ + "+".join (gfx) + """
  music=BACKEND       Music backend. Options:
                        """ + " ".join (music_map.keys ()) + """
                      Current (default) value: """ + "+".join (music) + """
  release=1           Sets the default settings used for releases, including
                      nightly builds (which you can override):
                      Equivalent to debug=0 gengcc=1, and also portable=1
                      on Unix (except Android and Mac)
  gengcc=1            Compile using GCC emitter (faster binaries, longer compile
                      times, and some extra warnings). This is always used
                      everywhere except x86 Windows/Linux/BSD.
  debug=0|1|2|3|4     Debug level:
                                  -exx |     debug info      | optimisation
                                 ------+---------------------+--------------
                       debug=0:    no  |    minimal syms     |    yes   <--Releases
                       debug=1:    no  |        yes          |    yes
                       debug=2:    yes |        yes          |    yes   <--Default
                       debug=3:    yes |        yes          |    no
                       debug=4:    no  |        yes          |    no
                      (pdb=1:          always stripped to pdb         )
                      -exx builds have array, pointer and file error checking
                        (they abort immediately on errors!), and are slow.
                      debug info: "minimal syms" means: only function
                        symbols present (to allow basic stacktraces); adds ~300KB.
                        (Note: if gengcc=0, then debug=0 is completely unstripped)
  pdb=1               (Windows only.) Produce .pdb debug info files, for CrashRpt
                      and BreakPad crash analysis. .pdb files are put in win32/.
                      Visual Studio or Visual C++ Build Tools must be installed.
                      Forces gengcc=1. Doesn't support linkgcc=0.
                      Requires wine if cross-compiling to Windows.
  lto=1               Do link-time optimisation, for a faster, smaller build
                      (about 2-300KB for Game/Custom) but longer compile time.
                      Use with gengcc=1.
  win95=0             (Windows only) Link to Winsock 2 instead of 1. Use win95=0
                      and mingw-w64 (not mingw) to get support for IPv6.
  valgrind=1          Recommended when using valgrind (also turns off -exx).
  asan=1              Use AddressSanitizer. Unless overridden with gengcc=0 also
                      disables -exx and uses GCC emitter.
  profile=1           Profiling build using gprof (executables) or MicroProfiler
                      (gfx_directx.dll/gfx_directx_test1.exe).
  asm=1               Produce .asm or .c files in build/ while compiling.
  fbc=PATH            Point to a different version of fbc.
  macsdk=version      Compile against a Mac OS X SDK instead of using the system
                      headers and libraries. Specify the SDK version, e.g. 10.4.
                      You'll need the relevant SDK installed in /Developer/SDKs
                      and may want to use a copy of FB built against that SDK.
                      Also sets macosx-version-min (defaults to 10.4).
  prefix=PATH         For 'install' and 'uninstall' actions. Default: '/usr'
  destdir=PATH        For 'install' and 'uninstall' actions. Use if you want to
                      install into a staging area, for a package creation tool.
                      Default: ''
  dry_run=1           For 'uninstall' only. Print files that would be deleted.
  v=1                 Be verbose.

Experimental options:
  linkgcc=0           Link using fbc instead of g++
  android-source=1    Used as part of the Android build process for Game/Custom.
                      (See wiki for explanation.) Note: defaults to the original
                      armeabi ABI, which is becoming obsolete, c.f. 'arch='.
  glibc=0|1           Override automatic detection (just checks for Linux).
  target=...          Set cross-compiling target. Passed through to fbc. Either
                      a toolchain prefix triplet such as arm-linux-androideabi
                      (will be prefixed to names of tools like gcc/as/ld/, e.g.
                      arm-linux-androideabi-gcc) or a target name supported by
                      fbc (e.g. darwin, android) or a platform-cpu pair (e.g.
                      linux-arm). Current (default) value: """ + target + """
  arch=ARCH           Specify target CPU type. Overrides 'target'. Options
                      include:
                       x86, x86_64        x86 Desktop PCs, Android devices.
                       arm or armeabi     Older 32-bit ARM devices w/o FPUs.
                           or arm5vte     (Android default.)
                       armv7-a            Newer 32-bit ARM devices w/ FPUs,
                                          like RPi2+.
                       arm64 or aarch64   64-bit ARM devices.
                         or arm64-v8a
                       32 or 64           32 or 64 bit variant of the default
                                          arch (x86 or ARM).
                      Current (default) value: """ + arch + """
  portable=1          (For Linux) Try to build portable binaries, and test them.

The following environmental variables are also important:
  FBFLAGS             Pass more flags to fbc
  fbc                 Override FB compiler
  AS, CC, CXX         Override assembler/compiler. Should be set when
                      crosscompiling unless target=... is given instead.
  GCC                 Used only to compile C code generated from FB code
                      (when using gengcc=1). Must not be clang!
                      CC used by default, unless CC appears to be clang.
  OHRGFX, OHRMUSIC    Specify default gfx, music backends
  DXSDK_DIR, Lib,
     Include          For compiling gfx_directx.dll
  EUDIR               Override location of the Euphoria installation, for
                      compiling hspeak (not needed if installed system-wide)

Targets (executables to build):
  """ + gamename + """ (or game)
  """ + editname + """ (or custom)
  gfx_directx.dll
  unlump
  relump
  hspeak              HamsterSpeak compiler (note: arch and target ignored)
  dumpohrkey          Convert .ohrkeys to text
  bam2mid             Convert .bam to .mid
  slice2bas           For embedding .slice files
  reload2xml
  xml2reload          Requires libxml2 to build.
  reloadutil          To compare two .reload documents, or time load time
 Automated tests (executables; use "test" target to build and run):
  reloadtest
  utiltest
  filetest
  vectortest
  rbtest
 Non-default automated test targets (not run by "scons test"):
  hspeaktest
 Nonautomated test programs:
  gfx_directx_test    gfx_directx.dll test
  miditest
Other targets/actions:
  install             (Unix only.) Install the OHRRPGCE. Uses prefix and destdir
                      args.
                      Installs files into ${destdir}${prefix}/games and
                      ${destdir}${prefix}/share
  uninstall           (Unix only.) Removes 'install'ed files. Uses prefix,
                      destdir, dry_run args (must be same as when installing).
  reload              Compile all RELOAD utilities.
  autotest            Runs autotest.rpg. See autotest.py for a better tool to
                      check differences.
  interactivetest     Runs interactivetest.rpg with recorded input.
  test (or tests)     Compile and run all automated tests, including
                      autotest.rpg.
  .                   Compile everything (but doesn't run tests)

With no targets specified, compiles game and custom.

Examples:
 Do a debug build of Game and Custom:
  scons
 Do a release build (same as official releases) of everything and run tests:
  scons release=1 . test
 Specifying graphics and music backends for a debug build of Game:
  scons gfx=sdl+fb music=native game
 Compile one file at a time, to avoid mixed-up error messages:
  scons -j1
 Create a fully optimised 64 bit build with debug symbols:
  scons arch=64 release=1 debug=1 .
 Compile and install (Unix only):
  sudo scons install prefix=/usr/local
""")
