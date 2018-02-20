#pragma once

'' The following symbols have been renamed:
''     constant MAD_VERSION => MAD_VERSION_
''     constant MAD_AUTHOR => MAD_AUTHOR_

#include once "crt/long.bi"

extern "C"

#define FPM_DEFAULT
const SIZEOF_INT = 4
const SIZEOF_LONG = 8
const SIZEOF_LONG_LONG = 8
#define LIBMAD_VERSION_H
const MAD_VERSION_MAJOR = 0
const MAD_VERSION_MINOR = 15
const MAD_VERSION_PATCH = 1
#define MAD_VERSION_EXTRA " (beta)"
#define MAD_VERSION_STRINGIZE(str) #str
#define MAD_VERSION_STRING(num) MAD_VERSION_STRINGIZE(num)
#define MAD_VERSION_ MAD_VERSION_STRING(MAD_VERSION_MAJOR) "." MAD_VERSION_STRING(MAD_VERSION_MINOR) "." MAD_VERSION_STRING(MAD_VERSION_PATCH) MAD_VERSION_EXTRA
#define MAD_PUBLISHYEAR "2000-2004"
#define MAD_AUTHOR_ "Underbit Technologies, Inc."
#define MAD_EMAIL "info@underbit.com"

extern mad_version as const zstring ptr
extern mad_copyright as const zstring ptr
extern mad_author as const zstring ptr
extern mad_build as const zstring ptr
#define LIBMAD_FIXED_H

type mad_fixed_t as long
type mad_fixed64hi_t as long
type mad_fixed64lo_t as ulong
type mad_fixed64_t as longint
type mad_sample_t as mad_fixed_t

const MAD_F_FRACBITS = 28
#define MAD_F(x) cast(mad_fixed_t, x##L)
const MAD_F_MIN = cast(mad_fixed_t, -cast(clong, &h80000000))
#define MAD_F_MAX cast(mad_fixed_t, +cast(clong, &h7fffffff))
#define MAD_F_ONE MAD_F(&h10000000)
#define mad_f_tofixed(x) cast(mad_fixed_t, ((x) * cdbl(cast(clong, 1) shl MAD_F_FRACBITS)) + 0.5)
#define mad_f_todouble(x) cdbl((x) / cdbl(cast(clong, 1) shl MAD_F_FRACBITS))
#define mad_f_intpart(x) ((x) shr MAD_F_FRACBITS)
#define mad_f_fracpart(x) ((x) and ((cast(clong, 1) shl MAD_F_FRACBITS) - 1))
#define mad_f_fromint(x) ((x) shl MAD_F_FRACBITS)
#define mad_f_add(x, y) ((x) + (y))
#define mad_f_sub(x, y) ((x) - (y))
#define mad_f_mul(x, y) ((((x) + (cast(clong, 1) shl 11)) shr 12) * (((y) + (cast(clong, 1) shl 15)) shr 16))
#define MAD_F_ML0(hi, lo, x, y) scope : (lo) = mad_f_mul((x), (y)) : end scope
#define MAD_F_MLA(hi, lo, x, y) scope : (lo) += mad_f_mul((x), (y)) : end scope
#define MAD_F_MLN(hi, lo) scope : (lo) = -(lo) : end scope
#macro MAD_F_MLZ(hi, lo)
	scope
		cast(any, (hi))
		cast(mad_fixed_t, (lo))
	end scope
#endmacro
#define mad_f_scale64(hi, lo) cast(mad_fixed_t, ((hi) shl (32 - MAD_F_SCALEBITS)) or ((lo) shr MAD_F_SCALEBITS))
const MAD_F_SCALEBITS = MAD_F_FRACBITS
declare function mad_f_abs(byval as mad_fixed_t) as mad_fixed_t
declare function mad_f_div(byval as mad_fixed_t, byval as mad_fixed_t) as mad_fixed_t
#define LIBMAD_BIT_H

type mad_bitptr
	byte as const ubyte ptr
	cache as ushort
	left as ushort
end type

declare sub mad_bit_init(byval as mad_bitptr ptr, byval as const ubyte ptr)
#define mad_bit_finish(bitptr)
declare function mad_bit_length(byval as const mad_bitptr ptr, byval as const mad_bitptr ptr) as ulong
#define mad_bit_bitsleft(bitptr) (bitptr)->left
declare function mad_bit_nextbyte(byval as const mad_bitptr ptr) as const ubyte ptr
declare sub mad_bit_skip(byval as mad_bitptr ptr, byval as ulong)
declare function mad_bit_read(byval as mad_bitptr ptr, byval as ulong) as culong
declare sub mad_bit_write(byval as mad_bitptr ptr, byval as ulong, byval as culong)
declare function mad_bit_crc(byval as mad_bitptr, byval as ulong, byval as ushort) as ushort
#define LIBMAD_TIMER_H

type mad_timer_t
	seconds as clong
	fraction as culong
end type

extern mad_timer_zero as const mad_timer_t
const MAD_TIMER_RESOLUTION = cast(culong, 352800000)

type mad_units as long
enum
	MAD_UNITS_HOURS = -2
	MAD_UNITS_MINUTES = -1
	MAD_UNITS_SECONDS = 0
	MAD_UNITS_DECISECONDS = 10
	MAD_UNITS_CENTISECONDS = 100
	MAD_UNITS_MILLISECONDS = 1000
	MAD_UNITS_8000_HZ = 8000
	MAD_UNITS_11025_HZ = 11025
	MAD_UNITS_12000_HZ = 12000
	MAD_UNITS_16000_HZ = 16000
	MAD_UNITS_22050_HZ = 22050
	MAD_UNITS_24000_HZ = 24000
	MAD_UNITS_32000_HZ = 32000
	MAD_UNITS_44100_HZ = 44100
	MAD_UNITS_48000_HZ = 48000
	MAD_UNITS_24_FPS = 24
	MAD_UNITS_25_FPS = 25
	MAD_UNITS_30_FPS = 30
	MAD_UNITS_48_FPS = 48
	MAD_UNITS_50_FPS = 50
	MAD_UNITS_60_FPS = 60
	MAD_UNITS_75_FPS = 75
	MAD_UNITS_23_976_FPS = -24
	MAD_UNITS_24_975_FPS = -25
	MAD_UNITS_29_97_FPS = -30
	MAD_UNITS_47_952_FPS = -48
	MAD_UNITS_49_95_FPS = -50
	MAD_UNITS_59_94_FPS = -60
end enum

#define mad_timer_reset(timer) (*(timer) = mad_timer_zero)
declare function mad_timer_compare(byval as mad_timer_t, byval as mad_timer_t) as long
#define mad_timer_sign(timer) mad_timer_compare((timer), mad_timer_zero)
declare sub mad_timer_negate(byval as mad_timer_t ptr)
declare function mad_timer_abs(byval as mad_timer_t) as mad_timer_t
declare sub mad_timer_set(byval as mad_timer_t ptr, byval as culong, byval as culong, byval as culong)
declare sub mad_timer_add(byval as mad_timer_t ptr, byval as mad_timer_t)
declare sub mad_timer_multiply(byval as mad_timer_t ptr, byval as clong)
declare function mad_timer_count(byval as mad_timer_t, byval as mad_units) as clong
declare function mad_timer_fraction(byval as mad_timer_t, byval as culong) as culong
declare sub mad_timer_string(byval as mad_timer_t, byval as zstring ptr, byval as const zstring ptr, byval as mad_units, byval as mad_units, byval as culong)

#define LIBMAD_STREAM_H
const MAD_BUFFER_GUARD = 8
const MAD_BUFFER_MDLEN = (511 + 2048) + MAD_BUFFER_GUARD

type mad_error as long
enum
	MAD_ERROR_NONE = &h0000
	MAD_ERROR_BUFLEN = &h0001
	MAD_ERROR_BUFPTR = &h0002
	MAD_ERROR_NOMEM = &h0031
	MAD_ERROR_LOSTSYNC = &h0101
	MAD_ERROR_BADLAYER = &h0102
	MAD_ERROR_BADBITRATE = &h0103
	MAD_ERROR_BADSAMPLERATE = &h0104
	MAD_ERROR_BADEMPHASIS = &h0105
	MAD_ERROR_BADCRC = &h0201
	MAD_ERROR_BADBITALLOC = &h0211
	MAD_ERROR_BADSCALEFACTOR = &h0221
	MAD_ERROR_BADMODE = &h0222
	MAD_ERROR_BADFRAMELEN = &h0231
	MAD_ERROR_BADBIGVALUES = &h0232
	MAD_ERROR_BADBLOCKTYPE = &h0233
	MAD_ERROR_BADSCFSI = &h0234
	MAD_ERROR_BADDATAPTR = &h0235
	MAD_ERROR_BADPART3LEN = &h0236
	MAD_ERROR_BADHUFFTABLE = &h0237
	MAD_ERROR_BADHUFFDATA = &h0238
	MAD_ERROR_BADSTEREO = &h0239
end enum

#define MAD_RECOVERABLE(error) ((error) and &hff00)

type mad_stream
	buffer as const ubyte ptr
	bufend as const ubyte ptr
	skiplen as culong
	sync as long
	freerate as culong
	this_frame as const ubyte ptr
	next_frame as const ubyte ptr
	ptr as mad_bitptr
	anc_ptr as mad_bitptr
	anc_bitlen as ulong
	main_data as ubyte ptr
	md_len as ulong
	options as long
	error as mad_error
end type

enum
	MAD_OPTION_IGNORECRC = &h0001
	MAD_OPTION_HALFSAMPLERATE = &h0002
end enum

declare sub mad_stream_init(byval as mad_stream ptr)
declare sub mad_stream_finish(byval as mad_stream ptr)
#define mad_stream_options(stream, opts) (stream)->options = (opts)
declare sub mad_stream_buffer(byval as mad_stream ptr, byval as const ubyte ptr, byval as culong)
declare sub mad_stream_skip(byval as mad_stream ptr, byval as culong)
declare function mad_stream_sync(byval as mad_stream ptr) as long
declare function mad_stream_errorstr(byval as const mad_stream ptr) as const zstring ptr
#define LIBMAD_FRAME_H

type mad_layer as long
enum
	MAD_LAYER_I = 1
	MAD_LAYER_II = 2
	MAD_LAYER_III = 3
end enum

type mad_mode as long
enum
	MAD_MODE_SINGLE_CHANNEL = 0
	MAD_MODE_DUAL_CHANNEL = 1
	MAD_MODE_JOINT_STEREO = 2
	MAD_MODE_STEREO = 3
end enum

type mad_emphasis as long
enum
	MAD_EMPHASIS_NONE = 0
	MAD_EMPHASIS_50_15_US = 1
	MAD_EMPHASIS_CCITT_J_17 = 3
	MAD_EMPHASIS_RESERVED = 2
end enum

type mad_header
	layer as mad_layer
	mode as mad_mode
	mode_extension as long
	emphasis as mad_emphasis
	bitrate as culong
	samplerate as ulong
	crc_check as ushort
	crc_target as ushort
	flags as long
	private_bits as long
	duration as mad_timer_t
end type

type mad_frame
	header as mad_header
	options as long
	sbsample(0 to 1, 0 to 35, 0 to 31) as mad_fixed_t
	overlap as mad_fixed_t ptr
end type

#define MAD_NCHANNELS(header) iif((header)->mode, 2, 1)
#define MAD_NSBSAMPLES(header) iif((header)->layer = MAD_LAYER_I, 12, iif(((header)->layer = MAD_LAYER_III) andalso ((header)->flags and MAD_FLAG_LSF_EXT), 18, 36))

enum
	MAD_FLAG_NPRIVATE_III = &h0007
	MAD_FLAG_INCOMPLETE = &h0008
	MAD_FLAG_PROTECTION = &h0010
	MAD_FLAG_COPYRIGHT = &h0020
	MAD_FLAG_ORIGINAL = &h0040
	MAD_FLAG_PADDING = &h0080
	MAD_FLAG_I_STEREO = &h0100
	MAD_FLAG_MS_STEREO = &h0200
	MAD_FLAG_FREEFORMAT = &h0400
	MAD_FLAG_LSF_EXT = &h1000
	MAD_FLAG_MC_EXT = &h2000
	MAD_FLAG_MPEG_2_5_EXT = &h4000
end enum

enum
	MAD_PRIVATE_HEADER = &h0100
	MAD_PRIVATE_III = &h001f
end enum

declare sub mad_header_init(byval as mad_header ptr)
#define mad_header_finish(header)
declare function mad_header_decode(byval as mad_header ptr, byval as mad_stream ptr) as long
declare sub mad_frame_init(byval as mad_frame ptr)
declare sub mad_frame_finish(byval as mad_frame ptr)
declare function mad_frame_decode(byval as mad_frame ptr, byval as mad_stream ptr) as long
declare sub mad_frame_mute(byval as mad_frame ptr)
#define LIBMAD_SYNTH_H

type mad_pcm
	samplerate as ulong
	channels as ushort
	length as ushort
	samples(0 to 1, 0 to 1151) as mad_fixed_t
end type

type mad_synth
	filter(0 to 1, 0 to 1, 0 to 1, 0 to 15, 0 to 7) as mad_fixed_t
	phase as ulong
	pcm as mad_pcm
end type

enum
	MAD_PCM_CHANNEL_SINGLE = 0
end enum

enum
	MAD_PCM_CHANNEL_DUAL_1 = 0
	MAD_PCM_CHANNEL_DUAL_2 = 1
end enum

enum
	MAD_PCM_CHANNEL_STEREO_LEFT = 0
	MAD_PCM_CHANNEL_STEREO_RIGHT = 1
end enum

declare sub mad_synth_init(byval as mad_synth ptr)
#define mad_synth_finish(synth)
declare sub mad_synth_mute(byval as mad_synth ptr)
declare sub mad_synth_frame(byval as mad_synth ptr, byval as const mad_frame ptr)
#define LIBMAD_DECODER_H

type mad_decoder_mode as long
enum
	MAD_DECODER_MODE_SYNC = 0
	MAD_DECODER_MODE_ASYNC
end enum

type mad_flow as long
enum
	MAD_FLOW_CONTINUE = &h0000
	MAD_FLOW_STOP = &h0010
	MAD_FLOW_BREAK = &h0011
	MAD_FLOW_IGNORE = &h0020
end enum

type mad_decoder_async
	pid as clong
	in as long
	out as long
end type

type mad_decoder__0
	stream as mad_stream
	frame as mad_frame
	synth as mad_synth
end type

type mad_decoder
	mode as mad_decoder_mode
	options as long
	async as mad_decoder_async
	sync as mad_decoder__0 ptr
	cb_data as any ptr
	input_func as function(byval as any ptr, byval as mad_stream ptr) as mad_flow
	header_func as function(byval as any ptr, byval as const mad_header ptr) as mad_flow
	filter_func as function(byval as any ptr, byval as const mad_stream ptr, byval as mad_frame ptr) as mad_flow
	output_func as function(byval as any ptr, byval as const mad_header ptr, byval as mad_pcm ptr) as mad_flow
	error_func as function(byval as any ptr, byval as mad_stream ptr, byval as mad_frame ptr) as mad_flow
	message_func as function(byval as any ptr, byval as any ptr, byval as ulong ptr) as mad_flow
end type

declare sub mad_decoder_init(byval as mad_decoder ptr, byval as any ptr, byval as function(byval as any ptr, byval as mad_stream ptr) as mad_flow, byval as function(byval as any ptr, byval as const mad_header ptr) as mad_flow, byval as function(byval as any ptr, byval as const mad_stream ptr, byval as mad_frame ptr) as mad_flow, byval as function(byval as any ptr, byval as const mad_header ptr, byval as mad_pcm ptr) as mad_flow, byval as function(byval as any ptr, byval as mad_stream ptr, byval as mad_frame ptr) as mad_flow, byval as function(byval as any ptr, byval as any ptr, byval as ulong ptr) as mad_flow)
declare function mad_decoder_finish(byval as mad_decoder ptr) as long
#define mad_decoder_options(decoder, opts) (decoder)->options = (opts)
declare function mad_decoder_run(byval as mad_decoder ptr, byval as mad_decoder_mode) as long
declare function mad_decoder_message(byval as mad_decoder ptr, byval as any ptr, byval as ulong ptr) as long

end extern
