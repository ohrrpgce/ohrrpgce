This directory contains headers taken from FreeBASIC's rtlib, version
1.06 (20160504). See license.txt for their license.

fb_config.h is very slightly modified.
fb_stub.h is a heavily modified, shortened version of fb.h which
doesn't depend on any of the platform-specific headers.

These headers have been largely stable over the last 9 years, however
I only guarantee the parts that we actually use to be correct, for FB
versions between 0.22 and 1.06 inclusive.  Of course we want to rely
on as little as possible to minimise the chance of breakage in future
FB versions. Do not assume these headers are correct for future FB
versions without diff'ing them! It will be necessary to check the git
log of fb.h for changes rather than doing a diff.
