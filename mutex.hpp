/* OHRRPGCE - mutex helper class
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
 * Please read LICENSE.txt for GPL License details and disclaimer of liability
 */

#ifndef MUTEX_HPP
#define MUTEX_HPP

// Use FB's mutexes instead of C++'s to avoid pulling in more of the C++ runtime library
// You must include fb/fb_stub.h before this or any other include!
// FB mutexes are NOT recursive: they can't be locked more than once, although
// the internal global mutexes used by the rtlib and gfxlib are recursive.

class mutex {
	FBMUTEX *fbmutex;
	bool permanent;
public:
	// Unfortunately we have a frightening amount of module-level constructor code,
	// so we can't rely on the mutex constructor getting called first.
	// However, only one thread will exist, and fb_MutexLock/Unlock fails silently
	// on an uninitialised mutex, so there's no problem.

	// Permanent flag is used for global mutexes which we don't want to risk
	// getting destroyed early during program shutdown
	mutex(bool _permanent = false) : permanent(_permanent)
		      { fbmutex = fb_MutexCreate(); }
	~mutex()      { if (!permanent) fb_MutexDestroy(fbmutex); }
	void lock()   { fb_MutexLock(fbmutex); }
	void unlock() { fb_MutexUnlock(fbmutex); }
};

#endif
