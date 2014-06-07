'Compression enum
#define BI_RGB 0
#define BI_RLE8 1
#define BI_RLE4 2
#define BI_BITFIELDS 3
#define BI_JPEG 4
#define BI_PNG 5

type BITMAP
	bmType as LONG
	bmWidth as LONG
	bmHeight as LONG
	bmWidthBytes as LONG
	bmPlanes as short
	bmBitsPixel as short
	bmBits as any ptr
end type

type PBITMAP as BITMAP ptr
type LPBITMAP as BITMAP ptr

type BITMAPCOREHEADER
	bcSize as integer
	bcWidth as short
	bcHeight as short
	bcPlanes as short
	bcBitCount as short
end type

type LPBITMAPCOREHEADER as BITMAPCOREHEADER ptr
type PBITMAPCOREHEADER as BITMAPCOREHEADER ptr

type RGBTRIPLE field=1
	rgbtBlue as BYTE
	rgbtGreen as BYTE
	rgbtRed as BYTE
end type

type LPRGBTRIPLE as RGBTRIPLE ptr

type BITMAPFILEHEADER field=2
	bfType as short
	bfSize as integer
	bfReserved1 as short
	bfReserved2 as short
	bfOffBits as integer
end type

type LPBITMAPFILEHEADER as BITMAPFILEHEADER ptr
type PBITMAPFILEHEADER as BITMAPFILEHEADER ptr

type BITMAPCOREINFO
	bmciHeader as BITMAPCOREHEADER
	bmciColors(0 to 1-1) as RGBTRIPLE
end type

type LPBITMAPCOREINFO as BITMAPCOREINFO ptr
type PBITMAPCOREINFO as BITMAPCOREINFO ptr

type BITMAPINFOHEADER field = 1
	' These first fields are BMP V3 members
	biSize as integer
	biWidth as LONG
	biHeight as LONG
	biPlanes as short
	biBitCount as short
	biCompression as integer
	biSizeImage as integer
	biXPelsPerMeter as LONG
	biYPelsPerMeter as LONG
	biClrUsed as integer
	biClrImportant as integer

	' The following are the first of the additional BMPV V4 data fields
	' I've omitted colour space and gamma stuff
	biRedMask as integer
	biGreenMask as integer
	biBlueMask as integer
	biAlphaMask as integer
end type

type LPBITMAPINFOHEADER as BITMAPINFOHEADER ptr
type PBITMAPINFOHEADER as BITMAPINFOHEADER ptr

type RGBQUAD field = 1
	rgbBlue as BYTE
	rgbGreen as BYTE
	rgbRed as BYTE
	rgbReserved as BYTE
end type

type LPRGBQUAD as RGBQUAD ptr

type BITMAPINFO
	bmiHeader as BITMAPINFOHEADER
	bmiColors(0 to 1-1) as RGBQUAD
end type

type LPBITMAPINFO as BITMAPINFO ptr
type PBITMAPINFO as BITMAPINFO ptr
