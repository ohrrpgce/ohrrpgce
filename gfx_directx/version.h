//gfx_directx_version.h
//by Jay Tennant 12/30/09
//backend version

#ifndef GFX_DIRECTX_VERSION_H
#define GFX_DIRECTX_VERSION_H

#define DX_BUILD_DIRECTX_DYNAMIC TRUE //can only be TRUE or FALSE, or 1 or 0 respectively
#define DX_BUILD_MSVC_DYNAMIC FALSE //can only be TRUE or FALSE, or 1 or 0 respectively
#define DX_VERSION_MAJOR 1
#define DX_VERSION_MINOR 20
#define DX_VERSION_BUILD ((DX_BUILD_MSVC_DYNAMIC << 1) + DX_BUILD_DIRECTX_DYNAMIC)
#define DX_BACKEND_VERSION ((DX_VERSION_MAJOR << 16) + (DX_VERSION_MINOR << 8) + DX_VERSION_BUILD)

#endif
