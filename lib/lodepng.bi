#pragma once

#include once "crt/string.bi"

extern "C"

#define LODEPNG_H
extern LODEPNG_VERSION_STRING as const zstring ptr
#define LODEPNG_COMPILE_ZLIB
#define LODEPNG_COMPILE_PNG
#define LODEPNG_COMPILE_DECODER
#define LODEPNG_COMPILE_ENCODER
#define LODEPNG_COMPILE_DISK
#define LODEPNG_COMPILE_ANCILLARY_CHUNKS
#define LODEPNG_COMPILE_ERROR_TEXT
#define LODEPNG_COMPILE_ALLOCATORS

type LodePNGColorType as long
enum
	LCT_GREY = 0
	LCT_RGB = 2
	LCT_PALETTE = 3
	LCT_GREY_ALPHA = 4
	LCT_RGBA = 6
end enum

declare function lodepng_decode_memory(byval out as ubyte ptr ptr, byval w as ulong ptr, byval h as ulong ptr, byval in as const ubyte ptr, byval insize as size_t, byval colortype as LodePNGColorType, byval bitdepth as ulong) as ulong
declare function lodepng_decode32(byval out as ubyte ptr ptr, byval w as ulong ptr, byval h as ulong ptr, byval in as const ubyte ptr, byval insize as size_t) as ulong
declare function lodepng_decode24(byval out as ubyte ptr ptr, byval w as ulong ptr, byval h as ulong ptr, byval in as const ubyte ptr, byval insize as size_t) as ulong
declare function lodepng_decode_file(byval out as ubyte ptr ptr, byval w as ulong ptr, byval h as ulong ptr, byval filename as const zstring ptr, byval colortype as LodePNGColorType, byval bitdepth as ulong) as ulong
declare function lodepng_decode32_file(byval out as ubyte ptr ptr, byval w as ulong ptr, byval h as ulong ptr, byval filename as const zstring ptr) as ulong
declare function lodepng_decode24_file(byval out as ubyte ptr ptr, byval w as ulong ptr, byval h as ulong ptr, byval filename as const zstring ptr) as ulong
declare function lodepng_encode_memory(byval out as ubyte ptr ptr, byval outsize as size_t ptr, byval image as const ubyte ptr, byval w as ulong, byval h as ulong, byval colortype as LodePNGColorType, byval bitdepth as ulong) as ulong
declare function lodepng_encode32(byval out as ubyte ptr ptr, byval outsize as size_t ptr, byval image as const ubyte ptr, byval w as ulong, byval h as ulong) as ulong
declare function lodepng_encode24(byval out as ubyte ptr ptr, byval outsize as size_t ptr, byval image as const ubyte ptr, byval w as ulong, byval h as ulong) as ulong
declare function lodepng_encode_file(byval filename as const zstring ptr, byval image as const ubyte ptr, byval w as ulong, byval h as ulong, byval colortype as LodePNGColorType, byval bitdepth as ulong) as ulong
declare function lodepng_encode32_file(byval filename as const zstring ptr, byval image as const ubyte ptr, byval w as ulong, byval h as ulong) as ulong
declare function lodepng_encode24_file(byval filename as const zstring ptr, byval image as const ubyte ptr, byval w as ulong, byval h as ulong) as ulong
declare function lodepng_error_text(byval code as ulong) as const zstring ptr

type LodePNGDecompressSettings
	ignore_adler32 as ulong
	custom_zlib as function(byval as ubyte ptr ptr, byval as size_t ptr, byval as const ubyte ptr, byval as size_t, byval as const LodePNGDecompressSettings ptr) as ulong
	custom_inflate as function(byval as ubyte ptr ptr, byval as size_t ptr, byval as const ubyte ptr, byval as size_t, byval as const LodePNGDecompressSettings ptr) as ulong
	custom_context as const any ptr
end type

extern lodepng_default_decompress_settings as const LodePNGDecompressSettings
declare sub lodepng_decompress_settings_init(byval settings as LodePNGDecompressSettings ptr)

type LodePNGCompressSettings
	btype as ulong
	use_lz77 as ulong
	windowsize as ulong
	minmatch as ulong
	nicematch as ulong
	lazymatching as ulong
	custom_zlib as function(byval as ubyte ptr ptr, byval as size_t ptr, byval as const ubyte ptr, byval as size_t, byval as const LodePNGCompressSettings ptr) as ulong
	custom_deflate as function(byval as ubyte ptr ptr, byval as size_t ptr, byval as const ubyte ptr, byval as size_t, byval as const LodePNGCompressSettings ptr) as ulong
	custom_context as const any ptr
end type

extern lodepng_default_compress_settings as const LodePNGCompressSettings
declare sub lodepng_compress_settings_init(byval settings as LodePNGCompressSettings ptr)

type LodePNGColorMode
	colortype as LodePNGColorType
	bitdepth as ulong
	palette as ubyte ptr
	palettesize as size_t
	key_defined as ulong
	key_r as ulong
	key_g as ulong
	key_b as ulong
end type

declare sub lodepng_color_mode_init(byval info as LodePNGColorMode ptr)
declare sub lodepng_color_mode_cleanup(byval info as LodePNGColorMode ptr)
declare function lodepng_color_mode_copy(byval dest as LodePNGColorMode ptr, byval source as const LodePNGColorMode ptr) as ulong
declare function lodepng_color_mode_make(byval colortype as LodePNGColorType, byval bitdepth as ulong) as LodePNGColorMode
declare sub lodepng_palette_clear(byval info as LodePNGColorMode ptr)
declare function lodepng_palette_add(byval info as LodePNGColorMode ptr, byval r as ubyte, byval g as ubyte, byval b as ubyte, byval a as ubyte) as ulong
declare function lodepng_get_bpp(byval info as const LodePNGColorMode ptr) as ulong
declare function lodepng_get_channels(byval info as const LodePNGColorMode ptr) as ulong
declare function lodepng_is_greyscale_type(byval info as const LodePNGColorMode ptr) as ulong
declare function lodepng_is_alpha_type(byval info as const LodePNGColorMode ptr) as ulong
declare function lodepng_is_palette_type(byval info as const LodePNGColorMode ptr) as ulong
declare function lodepng_has_palette_alpha(byval info as const LodePNGColorMode ptr) as ulong
declare function lodepng_can_have_alpha(byval info as const LodePNGColorMode ptr) as ulong
declare function lodepng_get_raw_size(byval w as ulong, byval h as ulong, byval color as const LodePNGColorMode ptr) as size_t

type LodePNGTime
	year as ulong
	month as ulong
	day as ulong
	hour as ulong
	minute as ulong
	second as ulong
end type

type LodePNGInfo
	compression_method as ulong
	filter_method as ulong
	interlace_method as ulong
	color as LodePNGColorMode
	background_defined as ulong
	background_r as ulong
	background_g as ulong
	background_b as ulong
	text_num as size_t
	text_keys as zstring ptr ptr
	text_strings as zstring ptr ptr
	itext_num as size_t
	itext_keys as zstring ptr ptr
	itext_langtags as zstring ptr ptr
	itext_transkeys as zstring ptr ptr
	itext_strings as zstring ptr ptr
	time_defined as ulong
	time as LodePNGTime
	phys_defined as ulong
	phys_x as ulong
	phys_y as ulong
	phys_unit as ulong
	gama_defined as ulong
	gama_gamma as ulong
	chrm_defined as ulong
	chrm_white_x as ulong
	chrm_white_y as ulong
	chrm_red_x as ulong
	chrm_red_y as ulong
	chrm_green_x as ulong
	chrm_green_y as ulong
	chrm_blue_x as ulong
	chrm_blue_y as ulong
	srgb_defined as ulong
	srgb_intent as ulong
	iccp_defined as ulong
	iccp_name as zstring ptr
	iccp_profile as ubyte ptr
	iccp_profile_size as ulong
	unknown_chunks_data(0 to 2) as ubyte ptr
	unknown_chunks_size(0 to 2) as size_t
end type

declare sub lodepng_info_init(byval info as LodePNGInfo ptr)
declare sub lodepng_info_cleanup(byval info as LodePNGInfo ptr)
declare function lodepng_info_copy(byval dest as LodePNGInfo ptr, byval source as const LodePNGInfo ptr) as ulong
declare function lodepng_add_text(byval info as LodePNGInfo ptr, byval key as const zstring ptr, byval str as const zstring ptr) as ulong
declare sub lodepng_clear_text(byval info as LodePNGInfo ptr)
declare function lodepng_add_itext(byval info as LodePNGInfo ptr, byval key as const zstring ptr, byval langtag as const zstring ptr, byval transkey as const zstring ptr, byval str as const zstring ptr) as ulong
declare sub lodepng_clear_itext(byval info as LodePNGInfo ptr)
declare function lodepng_set_icc(byval info as LodePNGInfo ptr, byval name as const zstring ptr, byval profile as const ubyte ptr, byval profile_size as ulong) as ulong
declare sub lodepng_clear_icc(byval info as LodePNGInfo ptr)
declare function lodepng_convert(byval out as ubyte ptr, byval in as const ubyte ptr, byval mode_out as const LodePNGColorMode ptr, byval mode_in as const LodePNGColorMode ptr, byval w as ulong, byval h as ulong) as ulong

type LodePNGDecoderSettings
	zlibsettings as LodePNGDecompressSettings
	ignore_crc as ulong
	ignore_critical as ulong
	ignore_end as ulong
	color_convert as ulong
	read_text_chunks as ulong
	remember_unknown_chunks as ulong
end type

declare sub lodepng_decoder_settings_init(byval settings as LodePNGDecoderSettings ptr)

type LodePNGFilterStrategy as long
enum
	LFS_ZERO
	LFS_MINSUM
	LFS_ENTROPY
	LFS_BRUTE_FORCE
	LFS_PREDEFINED
end enum

type LodePNGColorProfile
	colored as ulong
	key as ulong
	key_r as ushort
	key_g as ushort
	key_b as ushort
	alpha as ulong
	numcolors as ulong
	palette(0 to 1023) as ubyte
	bits as ulong
	numpixels as size_t
end type

declare sub lodepng_color_profile_init(byval profile as LodePNGColorProfile ptr)
declare function lodepng_get_color_profile(byval profile as LodePNGColorProfile ptr, byval image as const ubyte ptr, byval w as ulong, byval h as ulong, byval mode_in as const LodePNGColorMode ptr) as ulong
declare function lodepng_auto_choose_color(byval mode_out as LodePNGColorMode ptr, byval image as const ubyte ptr, byval w as ulong, byval h as ulong, byval mode_in as const LodePNGColorMode ptr) as ulong

type LodePNGEncoderSettings
	zlibsettings as LodePNGCompressSettings
	auto_convert as ulong
	filter_palette_zero as ulong
	filter_strategy as LodePNGFilterStrategy
	predefined_filters as const ubyte ptr
	force_palette as ulong
	add_id as ulong
	text_compression as ulong
end type

declare sub lodepng_encoder_settings_init(byval settings as LodePNGEncoderSettings ptr)

type LodePNGState
	decoder as LodePNGDecoderSettings
	encoder as LodePNGEncoderSettings
	info_raw as LodePNGColorMode
	info_png as LodePNGInfo
	error as ulong
end type

declare sub lodepng_state_init(byval state as LodePNGState ptr)
declare sub lodepng_state_cleanup(byval state as LodePNGState ptr)
declare sub lodepng_state_copy(byval dest as LodePNGState ptr, byval source as const LodePNGState ptr)
declare function lodepng_decode(byval out as ubyte ptr ptr, byval w as ulong ptr, byval h as ulong ptr, byval state as LodePNGState ptr, byval in as const ubyte ptr, byval insize as size_t) as ulong
declare function lodepng_inspect(byval w as ulong ptr, byval h as ulong ptr, byval state as LodePNGState ptr, byval in as const ubyte ptr, byval insize as size_t) as ulong
declare function lodepng_inspect_chunk(byval state as LodePNGState ptr, byval pos as size_t, byval in as const ubyte ptr, byval insize as size_t) as ulong
declare function lodepng_encode(byval out as ubyte ptr ptr, byval outsize as size_t ptr, byval image as const ubyte ptr, byval w as ulong, byval h as ulong, byval state as LodePNGState ptr) as ulong
declare function lodepng_chunk_length(byval chunk as const ubyte ptr) as ulong
declare sub lodepng_chunk_type(byval type as zstring ptr, byval chunk as const ubyte ptr)
declare function lodepng_chunk_type_equals(byval chunk as const ubyte ptr, byval type as const zstring ptr) as ubyte
declare function lodepng_chunk_ancillary(byval chunk as const ubyte ptr) as ubyte
declare function lodepng_chunk_private(byval chunk as const ubyte ptr) as ubyte
declare function lodepng_chunk_safetocopy(byval chunk as const ubyte ptr) as ubyte
declare function lodepng_chunk_data(byval chunk as ubyte ptr) as ubyte ptr
declare function lodepng_chunk_data_const(byval chunk as const ubyte ptr) as const ubyte ptr
declare function lodepng_chunk_check_crc(byval chunk as const ubyte ptr) as ulong
declare sub lodepng_chunk_generate_crc(byval chunk as ubyte ptr)
declare function lodepng_chunk_next(byval chunk as ubyte ptr) as ubyte ptr
declare function lodepng_chunk_next_const(byval chunk as const ubyte ptr) as const ubyte ptr
declare function lodepng_chunk_find(byval chunk as ubyte ptr, byval end as const ubyte ptr, byval type as const zstring ptr) as ubyte ptr
declare function lodepng_chunk_find_const(byval chunk as const ubyte ptr, byval end as const ubyte ptr, byval type as const zstring ptr) as const ubyte ptr
declare function lodepng_chunk_append(byval out as ubyte ptr ptr, byval outlength as size_t ptr, byval chunk as const ubyte ptr) as ulong
declare function lodepng_chunk_create(byval out as ubyte ptr ptr, byval outlength as size_t ptr, byval length as ulong, byval type as const zstring ptr, byval data as const ubyte ptr) as ulong
declare function lodepng_crc32(byval buf as const ubyte ptr, byval len as size_t) as ulong
declare function lodepng_inflate(byval out as ubyte ptr ptr, byval outsize as size_t ptr, byval in as const ubyte ptr, byval insize as size_t, byval settings as const LodePNGDecompressSettings ptr) as ulong
declare function lodepng_zlib_decompress(byval out as ubyte ptr ptr, byval outsize as size_t ptr, byval in as const ubyte ptr, byval insize as size_t, byval settings as const LodePNGDecompressSettings ptr) as ulong
declare function lodepng_zlib_compress(byval out as ubyte ptr ptr, byval outsize as size_t ptr, byval in as const ubyte ptr, byval insize as size_t, byval settings as const LodePNGCompressSettings ptr) as ulong
declare function lodepng_huffman_code_lengths(byval lengths as ulong ptr, byval frequencies as const ulong ptr, byval numcodes as size_t, byval maxbitlen as ulong) as ulong
declare function lodepng_deflate(byval out as ubyte ptr ptr, byval outsize as size_t ptr, byval in as const ubyte ptr, byval insize as size_t, byval settings as const LodePNGCompressSettings ptr) as ulong
declare function lodepng_load_file(byval out as ubyte ptr ptr, byval outsize as size_t ptr, byval filename as const zstring ptr) as ulong
declare function lodepng_save_file(byval buffer as const ubyte ptr, byval buffersize as size_t, byval filename as const zstring ptr) as ulong

end extern
