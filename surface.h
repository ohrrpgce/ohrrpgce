//surface.h
//by Jay Tennant 5/31/11
//defines a surface

#pragma once

#include <stdint.h>

enum SurfaceFormat {
	SFMT_P8 = 0,
	SFMT_A8R8G8B8 = 1,
	SFMT_FORCEDWORD = 0xffffffff,
};
enum SurfaceUsage {
	SUSE_REGULAR = 0,
	SUSE_RENDERTARGET = 1,
	SUSE_SWAPCHAIN = 2,
	SUSE_FORCEDWORD = 0xffffffff,
};

typedef uint32_t SurfaceData32;
typedef uint8_t SurfaceData8;

struct Surface
{
	uint32_t width;
	uint32_t height;
	SurfaceFormat format;
	SurfaceUsage usage;
	union
	{
		void* pRawData;
		SurfaceData32* pColorData;
		SurfaceData8* pPaletteData;
	};
};
