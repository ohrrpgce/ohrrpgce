//gfx_directx_hpcounter.h
//by Jay Tennant 11/2/09
//manages high performance counter queries

#ifndef GFX_DIRECTX_HPCOUNTER_H
#define GFX_DIRECTX_HPCOUNTER_H

#include <windows.h>

namespace gfx
{
	class HPCounter
	{
	protected:
		LARGE_INTEGER m_time; //last updated time
		LARGE_INTEGER m_timeDelta; //change in time
		static void Initialize(); //initializes the performance frequency if not initialized
		static LARGE_INTEGER s_frequency;
		static bool s_bInitialized;
	public:
		HPCounter();
		virtual ~HPCounter();

		void Update(); //updates base time to which GetTimeDifferential() figures against
		UINT GetTimeDifferential(); //gets time differential in milliseconds (time since last Update() call)
		UINT GetTimeDifferentialHiRes(); //gets time differential in microseconds (time since last Update() call)
		LARGE_INTEGER GetTime(); //gets the current performance counter read
		static LARGE_INTEGER GetFrequency(); //returns performance frequency
	};
}

#endif