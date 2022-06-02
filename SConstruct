# -*- mode:python -*-
""" FOR USAGE INFORMATION RUN
 scons --help
"""

import os
import platform
import atexit
import ohrbuild

# Avoid warning when calling CacheDir if Visual Studio isn't installed
if platform.system() == 'Windows':
   env = Environment(tools = ['mingw'])
else:
   env = Environment()

# Use a cache for object files
# SCons has no builtin pruning of or limit on the cache, it just grows forever, so do it ourselves
cache_dir = 'build/cache'
cache_size_limit = float(os.environ.get('SCONS_CACHE_SIZE', 100)) * 1024*1024
if cache_size_limit:
   ohrbuild.init_cache_dir(cache_dir)  # This is completely optional
   env.CacheDir(cache_dir)
   atexit.register(ohrbuild.prune_cache_dir, cache_dir, cache_size_limit)

SConscript('SConscript', variant_dir = 'build/', duplicate = False)
