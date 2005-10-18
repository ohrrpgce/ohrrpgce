''
''
'' fmod -- header translated with help of SWIG FB wrapper
''
'' NOTICE: This file is part of the FreeBASIC Compiler package and can't
''         be included in other distributions without authorization.
''
''
#ifndef __fmod_bi__
#define __fmod_bi__

'$inclib: "fmodex"

#define FMOD_VERSION &h00040100

type FMOD_BOOL as integer
type FMOD_SYSTEM as any
type FMOD_SOUND as any
type FMOD_CHANNEL as any
type FMOD_CHANNELGROUP as any
type FMOD_DSP as any
type FMOD_POLYGON as any
type FMOD_GEOMETRY as any
type FMOD_SYNCPOINT as any
type FMOD_MODE as uinteger
type FMOD_TIMEUNIT as uinteger
type FMOD_INITFLAGS as uinteger
type FMOD_CAPS as uinteger

'these are actually in fmod_dsp.h, which I haven't converted
type FMOD_DSP_DESCRIPTION as integer
type FMOD_DSP_TYPE as integer

type FMOD_VECTOR
	x as single
	y as single
	z as single
end type

enum FMOD_RESULT
	FMOD_OK
	FMOD_ERR_ALREADYLOCKED
	FMOD_ERR_BADCOMMAND
	FMOD_ERR_CDDA_DRIVERS
	FMOD_ERR_CDDA_INIT
	FMOD_ERR_CDDA_INVALID_DEVICE
	FMOD_ERR_CDDA_NOAUDIO
	FMOD_ERR_CDDA_NODEVICES
	FMOD_ERR_CDDA_NODISC
	FMOD_ERR_CDDA_READ
	FMOD_ERR_CHANNEL_ALLOC
	FMOD_ERR_CHANNEL_STOLEN
	FMOD_ERR_COM
	FMOD_ERR_DMA
	FMOD_ERR_DSP_CONNECTION
	FMOD_ERR_DSP_FORMAT
	FMOD_ERR_DSP_NOTFOUND
	FMOD_ERR_DSP_RUNNING
	FMOD_ERR_DSP_TOOMANYCONNECTIONS
	FMOD_ERR_FILE_BAD
	FMOD_ERR_FILE_COULDNOTSEEK
	FMOD_ERR_FILE_EOF
	FMOD_ERR_FILE_NOTFOUND
	FMOD_ERR_FORMAT
	FMOD_ERR_HTTP
	FMOD_ERR_HTTP_ACCESS
	FMOD_ERR_HTTP_PROXY_AUTH
	FMOD_ERR_HTTP_SERVER_ERROR
	FMOD_ERR_HTTP_TIMEOUT
	FMOD_ERR_INITIALIZATION
	FMOD_ERR_INITIALIZED
	FMOD_ERR_INTERNAL
	FMOD_ERR_INVALID_HANDLE
	FMOD_ERR_INVALID_PARAM
	FMOD_ERR_IRX
	FMOD_ERR_MEMORY
	FMOD_ERR_MEMORY_IOP
	FMOD_ERR_MEMORY_SRAM
	FMOD_ERR_NEEDSOFTWARE
	FMOD_ERR_NET_CONNECT
	FMOD_ERR_NET_SOCKET_ERROR
	FMOD_ERR_NET_URL
	FMOD_ERR_NOTREADY
	FMOD_ERR_OUTPUT_ALLOCATED
	FMOD_ERR_OUTPUT_CREATEBUFFER
	FMOD_ERR_OUTPUT_DRIVERCALL
	FMOD_ERR_OUTPUT_FORMAT
	FMOD_ERR_OUTPUT_INIT
	FMOD_ERR_OUTPUT_NOHARDWARE
	FMOD_ERR_OUTPUT_NOSOFTWARE
	FMOD_ERR_PAN
	FMOD_ERR_PLUGIN
	FMOD_ERR_PLUGIN_MISSING
	FMOD_ERR_PLUGIN_RESOURCE
	FMOD_ERR_RECORD
	FMOD_ERR_REVERB_INSTANCE
	FMOD_ERR_SUBSOUND_ALLOCATED
	FMOD_ERR_TAGNOTFOUND
	FMOD_ERR_TOOMANYCHANNELS
	FMOD_ERR_UNIMPLEMENTED
	FMOD_ERR_UNINITIALIZED
	FMOD_ERR_UNSUPPORTED
	FMOD_ERR_VERSION
	FMOD_RESULT_FORCEINT = 65536
end enum


enum FMOD_OUTPUTTYPE
	FMOD_OUTPUTTYPE_AUTODETECT
	FMOD_OUTPUTTYPE_UNKNOWN
	FMOD_OUTPUTTYPE_NOSOUND
	FMOD_OUTPUTTYPE_WAVWRITER
	FMOD_OUTPUTTYPE_DSOUND
	FMOD_OUTPUTTYPE_WINMM
	FMOD_OUTPUTTYPE_ASIO
	FMOD_OUTPUTTYPE_OSS
	FMOD_OUTPUTTYPE_ALSA
	FMOD_OUTPUTTYPE_ESD
	FMOD_OUTPUTTYPE_SOUNDMANAGER
	FMOD_OUTPUTTYPE_COREAUDIO
	FMOD_OUTPUTTYPE_XBOX
	FMOD_OUTPUTTYPE_PS2
	FMOD_OUTPUTTYPE_GC
	FMOD_OUTPUTTYPE_XBOX360
	FMOD_OUTPUTTYPE_PSP
	FMOD_OUTPUTTYPE_MAX
	FMOD_OUTPUTTYPE_FORCEINT = 65536
end enum


#define FMOD_CAPS_NONE &h00000000
#define FMOD_CAPS_HARDWARE &h00000001
#define FMOD_CAPS_HARDWARE_EMULATED &h00000002
#define FMOD_CAPS_OUTPUT_MULTICHANNEL &h00000004
#define FMOD_CAPS_OUTPUT_FORMAT_PCM8 &h00000008
#define FMOD_CAPS_OUTPUT_FORMAT_PCM16 &h00000010
#define FMOD_CAPS_OUTPUT_FORMAT_PCM24 &h00000020
#define FMOD_CAPS_OUTPUT_FORMAT_PCM32 &h00000040
#define FMOD_CAPS_OUTPUT_FORMAT_PCMFLOAT &h00000080
#define FMOD_CAPS_REVERB_EAX2 &h00000100
#define FMOD_CAPS_REVERB_EAX3 &h00000200
#define FMOD_CAPS_REVERB_EAX4 &h00000400
#define FMOD_CAPS_REVERB_I3DL2 &h00000800
#define FMOD_CAPS_REVERB_LIMITED &h00001000

enum FMOD_SPEAKERMODE
	FMOD_SPEAKERMODE_RAW
	FMOD_SPEAKERMODE_MONO
	FMOD_SPEAKERMODE_STEREO
	FMOD_SPEAKERMODE_4POINT1
	FMOD_SPEAKERMODE_5POINT1
	FMOD_SPEAKERMODE_7POINT1
	FMOD_SPEAKERMODE_PROLOGIC
	FMOD_SPEAKERMODE_MAX
	FMOD_SPEAKERMODE_FORCEINT = 65536
end enum


enum FMOD_SPEAKER
	FMOD_SPEAKER_FRONT_LEFT
	FMOD_SPEAKER_FRONT_RIGHT
	FMOD_SPEAKER_FRONT_CENTER
	FMOD_SPEAKER_LOW_FREQUENCY
	FMOD_SPEAKER_BACK_LEFT
	FMOD_SPEAKER_BACK_RIGHT
	FMOD_SPEAKER_SIDE_LEFT
	FMOD_SPEAKER_SIDE_RIGHT
	FMOD_SPEAKER_MAX
	FMOD_SPEAKER_FORCEINT = 65536
end enum


enum FMOD_PLUGINTYPE
	FMOD_PLUGINTYPE_OUTPUT
	FMOD_PLUGINTYPE_CODEC
	FMOD_PLUGINTYPE_DSP
	FMOD_PLUGINTYPE_MAX
	FMOD_PLUGINTYPE_FORCEINT = 65536
end enum


#define FMOD_INIT_NORMAL &h00000000
#define FMOD_INIT_NONREALTIME &h00000001
#define FMOD_INIT_3D_RIGHTHANDED &h00000002
#define FMOD_INIT_DISABLESOFTWARE &h00000004
#define FMOD_INIT_DSOUND_DEFERRED &h00000100
#define FMOD_INIT_DSOUND_HRTFNONE &h00000200
#define FMOD_INIT_DSOUND_HRTFLIGHT &h00000400
#define FMOD_INIT_DSOUND_HRTFFULL &h00000800
#define FMOD_INIT_PS2_DISABLECORE0REVERB &h00010000
#define FMOD_INIT_PS2_DISABLECORE1REVERB &h00020000
#define FMOD_INIT_XBOX_REMOVEHEADROOM &h00100000

enum FMOD_SOUND_TYPE
	FMOD_SOUND_TYPE_UNKNOWN
	FMOD_SOUND_TYPE_AAC
	FMOD_SOUND_TYPE_AIFF
	FMOD_SOUND_TYPE_ASF
	FMOD_SOUND_TYPE_CDDA
	FMOD_SOUND_TYPE_DLS
	FMOD_SOUND_TYPE_FLAC
	FMOD_SOUND_TYPE_FSB
	FMOD_SOUND_TYPE_GCADPCM
	FMOD_SOUND_TYPE_IT
	FMOD_SOUND_TYPE_MIDI
	FMOD_SOUND_TYPE_MOD
	FMOD_SOUND_TYPE_MPEG
	FMOD_SOUND_TYPE_OGGVORBIS
	FMOD_SOUND_TYPE_PLAYLIST
	FMOD_SOUND_TYPE_RAW
	FMOD_SOUND_TYPE_S3M
	FMOD_SOUND_TYPE_SF2
	FMOD_SOUND_TYPE_USER
	FMOD_SOUND_TYPE_WAV
	FMOD_SOUND_TYPE_XM
	FMOD_SOUND_TYPE_XMA
	FMOD_SOUND_TYPE_MAX
	FMOD_SOUND_TYPE_FORCEINT = 65536
end enum


enum FMOD_SOUND_FORMAT
	FMOD_SOUND_FORMAT_NONE
	FMOD_SOUND_FORMAT_PCM8
	FMOD_SOUND_FORMAT_PCM16
	FMOD_SOUND_FORMAT_PCM24
	FMOD_SOUND_FORMAT_PCM32
	FMOD_SOUND_FORMAT_PCMFLOAT
	FMOD_SOUND_FORMAT_GCADPCM
	FMOD_SOUND_FORMAT_XADPCM
	FMOD_SOUND_FORMAT_XMA
	FMOD_SOUND_FORMAT_VAG
	FMOD_SOUND_FORMAT_MAX
	FMOD_SOUND_FORMAT_FORCEINT = 65536
end enum


#define FMOD_DEFAULT &h00000000
#define FMOD_LOOP_OFF &h00000001
#define FMOD_LOOP_NORMAL &h00000002
#define FMOD_LOOP_BIDI &h00000004
#define FMOD_2D &h00000008
#define FMOD_3D &h00000010
#define FMOD_HARDWARE &h00000020
#define FMOD_SOFTWARE &h00000040
#define FMOD_CREATESTREAM &h00000080
#define FMOD_CREATESAMPLE &h00000100
#define FMOD_OPENUSER &h00000400
#define FMOD_OPENMEMORY &h00000800
#define FMOD_OPENRAW &h00001000
#define FMOD_OPENONLY &h00002000
#define FMOD_ACCURATETIME &h00004000
#define FMOD_MPEGSEARCH &h00008000
#define FMOD_NONBLOCKING &h00010000
#define FMOD_UNIQUE &h00020000
#define FMOD_3D_HEADRELATIVE &h00040000
#define FMOD_3D_WORLDRELATIVE &h00080000
#define FMOD_3D_LOGROLLOFF &h00100000
#define FMOD_3D_LINEARROLLOFF &h00200000
#define FMOD_CDDA_FORCEASPI &h00400000
#define FMOD_CDDA_JITTERCORRECT &h00800000
#define FMOD_UNICODE &h01000000
#define FMOD_IGNORETAGS &h02000000

enum FMOD_OPENSTATE
	FMOD_OPENSTATE_READY = 0
	FMOD_OPENSTATE_LOADING
	FMOD_OPENSTATE_ERROR
	FMOD_OPENSTATE_CONNECTING
	FMOD_OPENSTATE_BUFFERING
	FMOD_OPENSTATE_MAX
	FMOD_OPENSTATE_FORCEINT = 65536
end enum


enum FMOD_CHANNEL_CALLBACKTYPE
	FMOD_CHANNEL_CALLBACKTYPE_END
	FMOD_CHANNEL_CALLBACKTYPE_VIRTUALVOICE
	FMOD_CHANNEL_CALLBACKTYPE_SYNCPOINT
	FMOD_CHANNEL_CALLBACKTYPE_MODZXX
	FMOD_CHANNEL_CALLBACKTYPE_MODROW
	FMOD_CHANNEL_CALLBACKTYPE_MODORDER
	FMOD_CHANNEL_CALLBACKTYPE_MODINST
	FMOD_CHANNEL_CALLBACKTYPE_MAX
	FMOD_CHANNEL_CALLBACKTYPE_FORCEINT = 65536
end enum


enum FMOD_DSP_FFT_WINDOW
	FMOD_DSP_FFT_WINDOW_RECT
	FMOD_DSP_FFT_WINDOW_TRIANGLE
	FMOD_DSP_FFT_WINDOW_HAMMING
	FMOD_DSP_FFT_WINDOW_HANNING
	FMOD_DSP_FFT_WINDOW_BLACKMAN
	FMOD_DSP_FFT_WINDOW_BLACKMANHARRIS
	FMOD_DSP_FFT_WINDOW_MAX
	FMOD_DSP_FFT_WINDOW_FORCEINT = 65536
end enum


enum FMOD_DSP_RESAMPLER
	FMOD_DSP_RESAMPLER_NOINTERP
	FMOD_DSP_RESAMPLER_LINEAR
	FMOD_DSP_RESAMPLER_CUBIC
	FMOD_DSP_RESAMPLER_SPLINE
	FMOD_DSP_RESAMPLER_MAX
	FMOD_DSP_RESAMPLER_FORCEINT = 65536
end enum


enum FMOD_TAGTYPE
	FMOD_TAGTYPE_UNKNOWN = 0
	FMOD_TAGTYPE_ID3V1
	FMOD_TAGTYPE_ID3V2
	FMOD_TAGTYPE_VORBISCOMMENT
	FMOD_TAGTYPE_SHOUTCAST
	FMOD_TAGTYPE_ICECAST
	FMOD_TAGTYPE_ASF
	FMOD_TAGTYPE_MIDI
	FMOD_TAGTYPE_PLAYLIST
	FMOD_TAGTYPE_FMOD
	FMOD_TAGTYPE_USER
	FMOD_TAGTYPE_MAX
	FMOD_TAGTYPE_FORCEINT = 65536
end enum


enum FMOD_TAGDATATYPE
	FMOD_TAGDATATYPE_BINARY = 0
	FMOD_TAGDATATYPE_INT
	FMOD_TAGDATATYPE_FLOAT
	FMOD_TAGDATATYPE_STRING
	FMOD_TAGDATATYPE_STRING_UTF16
	FMOD_TAGDATATYPE_STRING_UTF16BE
	FMOD_TAGDATATYPE_STRING_UTF8
	FMOD_TAGDATATYPE_CDTOC
	FMOD_TAGDATATYPE_MAX
	FMOD_TAGDATATYPE_FORCEINT = 65536
end enum


type FMOD_TAG
	type as FMOD_TAGTYPE
	datatype as FMOD_TAGDATATYPE
	name as string
	data as any ptr
	datalen as uinteger
	updated as FMOD_BOOL
end type

type FMOD_CDTOC
	numtracks as integer
	min(0 to 100-1) as integer
	sec(0 to 100-1) as integer
	frame(0 to 100-1) as integer
end type

#define FMOD_TIMEUNIT_MS &h00000001
#define FMOD_TIMEUNIT_PCM &h00000002
#define FMOD_TIMEUNIT_PCMBYTES &h00000004
#define FMOD_TIMEUNIT_RAWBYTES &h00000008
#define FMOD_TIMEUNIT_MODORDER &h00000100
#define FMOD_TIMEUNIT_MODROW &h00000200
#define FMOD_TIMEUNIT_MODPATTERN &h00000400
#define FMOD_TIMEUNIT_SENTENCE_MS &h00010000
#define FMOD_TIMEUNIT_SENTENCE_PCM &h00020000
#define FMOD_TIMEUNIT_SENTENCE_PCMBYTES &h00040000
#define FMOD_TIMEUNIT_SENTENCE &h00080000
#define FMOD_TIMEUNIT_SENTENCE_SUBSOUND &h00100000
#define FMOD_TIMEUNIT_BUFFERED &h10000000

type FMOD_SOUND_NONBLOCKCALLBACK as function(byval as FMOD_SOUND ptr, byval as FMOD_RESULT) as FMOD_RESULT
type FMOD_SOUND_PCMREADCALLBACK as function(byval as FMOD_SOUND ptr, byval as any ptr, byval as uinteger) as FMOD_RESULT
type FMOD_SOUND_PCMSETPOSCALLBACK as function(byval as FMOD_SOUND ptr, byval as integer, byval as uinteger, byval as FMOD_TIMEUNIT) as FMOD_RESULT

type FMOD_CREATESOUNDEXINFO
	cbsize as integer
	length as uinteger
	fileoffset as uinteger
	numchannels as integer
	defaultfrequency as integer
	format as FMOD_SOUND_FORMAT
	decodebuffersize as uinteger
	initialsubsound as integer
	numsubsounds as integer
	inclusionlist as integer ptr
	inclusionlistnum as integer
	pcmreadcallback as FMOD_SOUND_PCMREADCALLBACK
	pcmsetposcallback as FMOD_SOUND_PCMSETPOSCALLBACK
	nonblockcallback as FMOD_SOUND_NONBLOCKCALLBACK
	dlsname as string
	encryptionkey as string
	maxpolyphony as integer
	userdata as any ptr
end type

type FMOD_REVERB_PROPERTIES
	Instance as integer
	Environment as uinteger
	EnvSize as single
	EnvDiffusion as single
	Room as integer
	RoomHF as integer
	RoomLF as integer
	DecayTime as single
	DecayHFRatio as single
	DecayLFRatio as single
	Reflections as integer
	ReflectionsDelay as single
	ReflectionsPan(0 to 3-1) as single
	Reverb as integer
	ReverbDelay as single
	ReverbPan(0 to 3-1) as single
	EchoTime as single
	EchoDepth as single
	ModulationTime as single
	ModulationDepth as single
	AirAbsorptionHF as single
	HFReference as single
	LFReference as single
	RoomRolloffFactor as single
	Diffusion as single
	Density as single
	Flags as uinteger
end type

#define FMOD_REVERB_FLAGS_DECAYTIMESCALE &h00000001
#define FMOD_REVERB_FLAGS_REFLECTIONSSCALE &h00000002
#define FMOD_REVERB_FLAGS_REFLECTIONSDELAYSCALE &h00000004
#define FMOD_REVERB_FLAGS_REVERBSCALE &h00000008
#define FMOD_REVERB_FLAGS_REVERBDELAYSCALE &h00000010
#define FMOD_REVERB_FLAGS_DECAYHFLIMIT &h00000020
#define FMOD_REVERB_FLAGS_ECHOTIMESCALE &h00000040
#define FMOD_REVERB_FLAGS_MODULATIONTIMESCALE &h00000080
#define FMOD_REVERB_FLAGS_HIGHQUALITYREVERB &h00000100
#define FMOD_REVERB_FLAGS_DEFAULT (&h00000001 or &h00000002 or &h00000004 or &h00000008 or &h00000010 or &h00000020)

type FMOD_REVERB_CHANNELPROPERTIES
	Direct as integer
	DirectHF as integer
	Room as integer
	RoomHF as integer
	Obstruction as integer
	ObstructionLFRatio as single
	Occlusion as integer
	OcclusionLFRatio as single
	OcclusionRoomRatio as single
	OcclusionDirectRatio as single
	Exclusion as integer
	ExclusionLFRatio as single
	OutsideVolumeHF as integer
	DopplerFactor as single
	RolloffFactor as single
	RoomRolloffFactor as single
	AirAbsorptionFactor as single
	Flags as uinteger
end type

#define FMOD_REVERB_CHANNELFLAGS_DIRECTHFAUTO &h00000001
#define FMOD_REVERB_CHANNELFLAGS_ROOMAUTO &h00000002
#define FMOD_REVERB_CHANNELFLAGS_ROOMHFAUTO &h00000004
#define FMOD_REVERB_CHANNELFLAGS_ENVIRONMENT0 &h00000008
#define FMOD_REVERB_CHANNELFLAGS_ENVIRONMENT1 &h00000010
#define FMOD_REVERB_CHANNELFLAGS_ENVIRONMENT2 &h00000020
#define FMOD_REVERB_CHANNELFLAGS_DEFAULT (&h00000001 or &h00000002 or &h00000004 or &h00000008)

enum FMOD_CHANNELINDEX
	FMOD_CHANNEL_FREE = -1
	FMOD_CHANNEL_REUSE = -2
end enum

type FMOD_CHANNEL_CALLBACK as function(byval as FMOD_CHANNEL ptr, byval as FMOD_CHANNEL_CALLBACKTYPE, byval as integer, byval as uinteger, byval as uinteger) as FMOD_RESULT
type FMOD_FILE_OPENCALLBACK as function(byval as string, byval as integer, byval as uinteger ptr, byval as any ptr ptr, byval as any ptr ptr) as FMOD_RESULT
type FMOD_FILE_CLOSECALLBACK as function(byval as any ptr, byval as any ptr) as FMOD_RESULT
type FMOD_FILE_READCALLBACK as function(byval as any ptr, byval as any ptr, byval as uinteger, byval as uinteger ptr, byval as any ptr) as FMOD_RESULT
type FMOD_FILE_SEEKCALLBACK as function(byval as any ptr, byval as uinteger, byval as any ptr) as FMOD_RESULT
type FMOD_MEMORY_ALLOCCALLBACK as sub(byval as uinteger)
type FMOD_MEMORY_REALLOCCALLBACK as sub(byval as any ptr, byval as uinteger)
type FMOD_MEMORY_FREECALLBACK as sub(byval as any ptr)

declare function FMOD_Memory_Initialize alias "FMOD_Memory_Initialize" (byval poolmem as any ptr, byval poollen as integer, byval useralloc as FMOD_MEMORY_ALLOCCALLBACK, byval userrealloc as FMOD_MEMORY_REALLOCCALLBACK, byval userfree as FMOD_MEMORY_FREECALLBACK) as FMOD_RESULT
declare function FMOD_Memory_GetStats alias "FMOD_Memory_GetStats" (byval currentalloced as integer ptr, byval maxalloced as integer ptr) as FMOD_RESULT
declare function FMOD_System_Create alias "FMOD_System_Create" (byval system as FMOD_SYSTEM ptr ptr) as FMOD_RESULT
declare function FMOD_System_Release alias "FMOD_System_Release" (byval system as FMOD_SYSTEM ptr) as FMOD_RESULT
declare function FMOD_System_SetOutput alias "FMOD_System_SetOutput" (byval system as FMOD_SYSTEM ptr, byval output as FMOD_OUTPUTTYPE) as FMOD_RESULT
declare function FMOD_System_GetOutput alias "FMOD_System_GetOutput" (byval system as FMOD_SYSTEM ptr, byval output as FMOD_OUTPUTTYPE ptr) as FMOD_RESULT
declare function FMOD_System_GetNumDrivers alias "FMOD_System_GetNumDrivers" (byval system as FMOD_SYSTEM ptr, byval numdrivers as integer ptr) as FMOD_RESULT
declare function FMOD_System_GetDriverName alias "FMOD_System_GetDriverName" (byval system as FMOD_SYSTEM ptr, byval id as integer, byval name as string, byval namelen as integer) as FMOD_RESULT
declare function FMOD_System_GetDriverCaps alias "FMOD_System_GetDriverCaps" (byval system as FMOD_SYSTEM ptr, byval id as integer, byval caps as FMOD_CAPS ptr, byval minfrequency as integer ptr, byval maxfrequency as integer ptr, byval controlpanelspeakermode as FMOD_SPEAKERMODE ptr) as FMOD_RESULT
declare function FMOD_System_SetDriver alias "FMOD_System_SetDriver" (byval system as FMOD_SYSTEM ptr, byval driver as integer) as FMOD_RESULT
declare function FMOD_System_GetDriver alias "FMOD_System_GetDriver" (byval system as FMOD_SYSTEM ptr, byval driver as integer ptr) as FMOD_RESULT
declare function FMOD_System_SetHardwareChannels alias "FMOD_System_SetHardwareChannels" (byval system as FMOD_SYSTEM ptr, byval min2d as integer, byval max2d as integer, byval min3d as integer, byval max3d as integer) as FMOD_RESULT
declare function FMOD_System_GetHardwareChannels alias "FMOD_System_GetHardwareChannels" (byval system as FMOD_SYSTEM ptr, byval num2d as integer ptr, byval num3d as integer ptr, byval total as integer ptr) as FMOD_RESULT
declare function FMOD_System_SetSoftwareChannels alias "FMOD_System_SetSoftwareChannels" (byval system as FMOD_SYSTEM ptr, byval numsoftwarechannels as integer) as FMOD_RESULT
declare function FMOD_System_GetSoftwareChannels alias "FMOD_System_GetSoftwareChannels" (byval system as FMOD_SYSTEM ptr, byval numsoftwarechannels as integer ptr) as FMOD_RESULT
declare function FMOD_System_SetSoftwareFormat alias "FMOD_System_SetSoftwareFormat" (byval system as FMOD_SYSTEM ptr, byval samplerate as integer, byval format as FMOD_SOUND_FORMAT, byval numoutputchannels as integer, byval maxinputchannels as integer, byval resamplemethod as FMOD_DSP_RESAMPLER) as FMOD_RESULT
declare function FMOD_System_GetSoftwareFormat alias "FMOD_System_GetSoftwareFormat" (byval system as FMOD_SYSTEM ptr, byval samplerate as integer ptr, byval format as FMOD_SOUND_FORMAT ptr, byval numoutputchannels as integer ptr, byval maxinputchannels as integer ptr, byval resamplemethod as FMOD_DSP_RESAMPLER ptr, byval bits as integer ptr) as FMOD_RESULT
declare function FMOD_System_SetDSPBufferSize alias "FMOD_System_SetDSPBufferSize" (byval system as FMOD_SYSTEM ptr, byval bufferlength as uinteger, byval numbuffers as integer) as FMOD_RESULT
declare function FMOD_System_GetDSPBufferSize alias "FMOD_System_GetDSPBufferSize" (byval system as FMOD_SYSTEM ptr, byval bufferlength as uinteger ptr, byval numbuffers as integer ptr) as FMOD_RESULT
declare function FMOD_System_SetFileSystem alias "FMOD_System_SetFileSystem" (byval system as FMOD_SYSTEM ptr, byval useropen as FMOD_FILE_OPENCALLBACK, byval userclose as FMOD_FILE_CLOSECALLBACK, byval userread as FMOD_FILE_READCALLBACK, byval userseek as FMOD_FILE_SEEKCALLBACK, byval buffersize as integer) as FMOD_RESULT
declare function FMOD_System_AttachFileSystem alias "FMOD_System_AttachFileSystem" (byval system as FMOD_SYSTEM ptr, byval useropen as FMOD_FILE_OPENCALLBACK, byval userclose as FMOD_FILE_CLOSECALLBACK, byval userread as FMOD_FILE_READCALLBACK, byval userseek as FMOD_FILE_SEEKCALLBACK) as FMOD_RESULT
declare function FMOD_System_SetSpeakerMode alias "FMOD_System_SetSpeakerMode" (byval system as FMOD_SYSTEM ptr, byval speakermode as FMOD_SPEAKERMODE) as FMOD_RESULT
declare function FMOD_System_GetSpeakerMode alias "FMOD_System_GetSpeakerMode" (byval system as FMOD_SYSTEM ptr, byval speakermode as FMOD_SPEAKERMODE ptr) as FMOD_RESULT
declare function FMOD_System_SetPluginPath alias "FMOD_System_SetPluginPath" (byval system as FMOD_SYSTEM ptr, byval path as string) as FMOD_RESULT
declare function FMOD_System_LoadPlugin alias "FMOD_System_LoadPlugin" (byval system as FMOD_SYSTEM ptr, byval filename as string, byval plugintype as FMOD_PLUGINTYPE ptr, byval index as integer ptr) as FMOD_RESULT
declare function FMOD_System_GetNumPlugins alias "FMOD_System_GetNumPlugins" (byval system as FMOD_SYSTEM ptr, byval plugintype as FMOD_PLUGINTYPE, byval numplugins as integer ptr) as FMOD_RESULT
declare function FMOD_System_GetPluginInfo alias "FMOD_System_GetPluginInfo" (byval system as FMOD_SYSTEM ptr, byval plugintype as FMOD_PLUGINTYPE, byval index as integer, byval name as string, byval namelen as integer, byval version as uinteger ptr) as FMOD_RESULT
declare function FMOD_System_UnloadPlugin alias "FMOD_System_UnloadPlugin" (byval system as FMOD_SYSTEM ptr, byval plugintype as FMOD_PLUGINTYPE, byval index as integer) as FMOD_RESULT
declare function FMOD_System_SetOutputByPlugin alias "FMOD_System_SetOutputByPlugin" (byval system as FMOD_SYSTEM ptr, byval index as integer) as FMOD_RESULT
declare function FMOD_System_GetOutputByPlugin alias "FMOD_System_GetOutputByPlugin" (byval system as FMOD_SYSTEM ptr, byval index as integer ptr) as FMOD_RESULT
declare function FMOD_System_Init alias "FMOD_System_Init" (byval system as FMOD_SYSTEM ptr, byval maxchannels as integer, byval flags as FMOD_INITFLAGS, byval extradriverdata as any ptr) as FMOD_RESULT
declare function FMOD_System_Close alias "FMOD_System_Close" (byval system as FMOD_SYSTEM ptr) as FMOD_RESULT
declare function FMOD_System_Update alias "FMOD_System_Update" (byval system as FMOD_SYSTEM ptr) as FMOD_RESULT
declare function FMOD_System_Set3DSettings alias "FMOD_System_Set3DSettings" (byval system as FMOD_SYSTEM ptr, byval dopplerscale as single, byval distancefactor as single, byval rolloffscale as single) as FMOD_RESULT
declare function FMOD_System_Get3DSettings alias "FMOD_System_Get3DSettings" (byval system as FMOD_SYSTEM ptr, byval dopplerscale as single ptr, byval distancefactor as single ptr, byval rolloffscale as single ptr) as FMOD_RESULT
declare function FMOD_System_Set3DNumListeners alias "FMOD_System_Set3DNumListeners" (byval system as FMOD_SYSTEM ptr, byval numlisteners as integer) as FMOD_RESULT
declare function FMOD_System_Get3DNumListeners alias "FMOD_System_Get3DNumListeners" (byval system as FMOD_SYSTEM ptr, byval numlisteners as integer ptr) as FMOD_RESULT
declare function FMOD_System_Set3DListenerAttributes alias "FMOD_System_Set3DListenerAttributes" (byval system as FMOD_SYSTEM ptr, byval listener as integer, byval pos as FMOD_VECTOR ptr, byval vel as FMOD_VECTOR ptr, byval forward as FMOD_VECTOR ptr, byval up as FMOD_VECTOR ptr) as FMOD_RESULT
declare function FMOD_System_Get3DListenerAttributes alias "FMOD_System_Get3DListenerAttributes" (byval system as FMOD_SYSTEM ptr, byval listener as integer, byval pos as FMOD_VECTOR ptr, byval vel as FMOD_VECTOR ptr, byval forward as FMOD_VECTOR ptr, byval up as FMOD_VECTOR ptr) as FMOD_RESULT
declare function FMOD_System_SetSpeakerPosition alias "FMOD_System_SetSpeakerPosition" (byval system as FMOD_SYSTEM ptr, byval speaker as FMOD_SPEAKER, byval x as single, byval y as single) as FMOD_RESULT
declare function FMOD_System_GetSpeakerPosition alias "FMOD_System_GetSpeakerPosition" (byval system as FMOD_SYSTEM ptr, byval speaker as FMOD_SPEAKER, byval x as single ptr, byval y as single ptr) as FMOD_RESULT
declare function FMOD_System_SetStreamBufferSize alias "FMOD_System_SetStreamBufferSize" (byval system as FMOD_SYSTEM ptr, byval filebuffersize as uinteger, byval filebuffersizetype as FMOD_TIMEUNIT) as FMOD_RESULT
declare function FMOD_System_GetStreamBufferSize alias "FMOD_System_GetStreamBufferSize" (byval system as FMOD_SYSTEM ptr, byval filebuffersize as uinteger ptr, byval filebuffersizetype as FMOD_TIMEUNIT ptr) as FMOD_RESULT
declare function FMOD_System_GetVersion alias "FMOD_System_GetVersion" (byval system as FMOD_SYSTEM ptr, byval version as uinteger ptr) as FMOD_RESULT
declare function FMOD_System_GetOutputHandle alias "FMOD_System_GetOutputHandle" (byval system as FMOD_SYSTEM ptr, byval handle as any ptr ptr) as FMOD_RESULT
declare function FMOD_System_GetChannelsPlaying alias "FMOD_System_GetChannelsPlaying" (byval system as FMOD_SYSTEM ptr, byval channels as integer ptr) as FMOD_RESULT
declare function FMOD_System_GetCPUUsage alias "FMOD_System_GetCPUUsage" (byval system as FMOD_SYSTEM ptr, byval dsp as single ptr, byval stream as single ptr, byval update as single ptr, byval total as single ptr) as FMOD_RESULT
declare function FMOD_System_GetSoundRAM alias "FMOD_System_GetSoundRAM" (byval system as FMOD_SYSTEM ptr, byval currentalloced as integer ptr, byval maxalloced as integer ptr, byval total as integer ptr) as FMOD_RESULT
declare function FMOD_System_GetNumCDROMDrives alias "FMOD_System_GetNumCDROMDrives" (byval system as FMOD_SYSTEM ptr, byval numdrives as integer ptr) as FMOD_RESULT
declare function FMOD_System_GetCDROMDriveName alias "FMOD_System_GetCDROMDriveName" (byval system as FMOD_SYSTEM ptr, byval drive as integer, byval drivename as string, byval drivenamelen as integer, byval scsiname as string, byval scsinamelen as integer, byval devicename as string, byval devicenamelen as integer) as FMOD_RESULT
declare function FMOD_System_GetSpectrum alias "FMOD_System_GetSpectrum" (byval system as FMOD_SYSTEM ptr, byval spectrumarray as single ptr, byval numvalues as integer, byval channeloffset as integer, byval windowtype as FMOD_DSP_FFT_WINDOW) as FMOD_RESULT
declare function FMOD_System_GetWaveData alias "FMOD_System_GetWaveData" (byval system as FMOD_SYSTEM ptr, byval wavearray as single ptr, byval numvalues as integer, byval channeloffset as integer) as FMOD_RESULT
declare function FMOD_System_CreateSound alias "FMOD_System_CreateSound" (byval system as FMOD_SYSTEM ptr, byval name_or_data as string, byval mode as FMOD_MODE, byval exinfo as FMOD_CREATESOUNDEXINFO ptr, byval sound as FMOD_SOUND ptr ptr) as FMOD_RESULT
declare function FMOD_System_CreateStream alias "FMOD_System_CreateStream" (byval system as FMOD_SYSTEM ptr, byval name_or_data as string, byval mode as FMOD_MODE, byval exinfo as FMOD_CREATESOUNDEXINFO ptr, byval sound as FMOD_SOUND ptr ptr) as FMOD_RESULT
declare function FMOD_System_CreateDSP alias "FMOD_System_CreateDSP" (byval system as FMOD_SYSTEM ptr, byval description as FMOD_DSP_DESCRIPTION ptr, byval dsp as FMOD_DSP ptr ptr) as FMOD_RESULT
declare function FMOD_System_CreateDSPByType alias "FMOD_System_CreateDSPByType" (byval system as FMOD_SYSTEM ptr, byval type as FMOD_DSP_TYPE, byval dsp as FMOD_DSP ptr ptr) as FMOD_RESULT
declare function FMOD_System_CreateDSPByIndex alias "FMOD_System_CreateDSPByIndex" (byval system as FMOD_SYSTEM ptr, byval index as integer, byval dsp as FMOD_DSP ptr ptr) as FMOD_RESULT
declare function FMOD_System_CreateChannelGroup alias "FMOD_System_CreateChannelGroup" (byval system as FMOD_SYSTEM ptr, byval name as string, byval channelgroup as FMOD_CHANNELGROUP ptr ptr) as FMOD_RESULT
declare function FMOD_System_PlaySound alias "FMOD_System_PlaySound" (byval system as FMOD_SYSTEM ptr, byval channelid as FMOD_CHANNELINDEX, byval sound as FMOD_SOUND ptr, byval paused as FMOD_BOOL, byval channel as FMOD_CHANNEL ptr ptr) as FMOD_RESULT
declare function FMOD_System_PlayDSP alias "FMOD_System_PlayDSP" (byval system as FMOD_SYSTEM ptr, byval channelid as FMOD_CHANNELINDEX, byval dsp as FMOD_DSP ptr, byval paused as FMOD_BOOL, byval channel as FMOD_CHANNEL ptr ptr) as FMOD_RESULT
declare function FMOD_System_GetChannel alias "FMOD_System_GetChannel" (byval system as FMOD_SYSTEM ptr, byval channelid as integer, byval channel as FMOD_CHANNEL ptr ptr) as FMOD_RESULT
declare function FMOD_System_GetMasterChannelGroup alias "FMOD_System_GetMasterChannelGroup" (byval system as FMOD_SYSTEM ptr, byval channelgroup as FMOD_CHANNELGROUP ptr ptr) as FMOD_RESULT
declare function FMOD_System_SetReverbProperties alias "FMOD_System_SetReverbProperties" (byval system as FMOD_SYSTEM ptr, byval prop as FMOD_REVERB_PROPERTIES ptr) as FMOD_RESULT
declare function FMOD_System_GetReverbProperties alias "FMOD_System_GetReverbProperties" (byval system as FMOD_SYSTEM ptr, byval prop as FMOD_REVERB_PROPERTIES ptr) as FMOD_RESULT
declare function FMOD_System_GetDSPHead alias "FMOD_System_GetDSPHead" (byval system as FMOD_SYSTEM ptr, byval dsp as FMOD_DSP ptr ptr) as FMOD_RESULT
declare function FMOD_System_AddDSP alias "FMOD_System_AddDSP" (byval system as FMOD_SYSTEM ptr, byval dsp as FMOD_DSP ptr) as FMOD_RESULT
declare function FMOD_System_LockDSP alias "FMOD_System_LockDSP" (byval system as FMOD_SYSTEM ptr) as FMOD_RESULT
declare function FMOD_System_UnlockDSP alias "FMOD_System_UnlockDSP" (byval system as FMOD_SYSTEM ptr) as FMOD_RESULT
declare function FMOD_System_SetRecordDriver alias "FMOD_System_SetRecordDriver" (byval system as FMOD_SYSTEM ptr, byval driver as integer) as FMOD_RESULT
declare function FMOD_System_GetRecordDriver alias "FMOD_System_GetRecordDriver" (byval system as FMOD_SYSTEM ptr, byval driver as integer ptr) as FMOD_RESULT
declare function FMOD_System_GetRecordNumDrivers alias "FMOD_System_GetRecordNumDrivers" (byval system as FMOD_SYSTEM ptr, byval numdrivers as integer ptr) as FMOD_RESULT
declare function FMOD_System_GetRecordDriverName alias "FMOD_System_GetRecordDriverName" (byval system as FMOD_SYSTEM ptr, byval id as integer, byval name as string, byval namelen as integer) as FMOD_RESULT
declare function FMOD_System_GetRecordPosition alias "FMOD_System_GetRecordPosition" (byval system as FMOD_SYSTEM ptr, byval position as uinteger ptr) as FMOD_RESULT
declare function FMOD_System_RecordStart alias "FMOD_System_RecordStart" (byval system as FMOD_SYSTEM ptr, byval sound as FMOD_SOUND ptr, byval loop as FMOD_BOOL) as FMOD_RESULT
declare function FMOD_System_RecordStop alias "FMOD_System_RecordStop" (byval system as FMOD_SYSTEM ptr) as FMOD_RESULT
declare function FMOD_System_IsRecording alias "FMOD_System_IsRecording" (byval system as FMOD_SYSTEM ptr, byval recording as FMOD_BOOL ptr) as FMOD_RESULT
declare function FMOD_System_CreateGeometry alias "FMOD_System_CreateGeometry" (byval system as FMOD_SYSTEM ptr, byval maxpolygons as integer, byval maxvertices as integer, byval geometry as FMOD_GEOMETRY ptr ptr) as FMOD_RESULT
declare function FMOD_System_SetGeometrySettings alias "FMOD_System_SetGeometrySettings" (byval system as FMOD_SYSTEM ptr, byval maxworldsize as single) as FMOD_RESULT
declare function FMOD_System_GetGeometrySettings alias "FMOD_System_GetGeometrySettings" (byval system as FMOD_SYSTEM ptr, byval maxworldsize as single ptr) as FMOD_RESULT
declare function FMOD_System_LoadGeometry alias "FMOD_System_LoadGeometry" (byval system as FMOD_SYSTEM ptr, byval data as any ptr, byval datasize as integer, byval geometry as FMOD_GEOMETRY ptr ptr) as FMOD_RESULT
declare function FMOD_System_SetNetworkProxy alias "FMOD_System_SetNetworkProxy" (byval system as FMOD_SYSTEM ptr, byval proxy as string) as FMOD_RESULT
declare function FMOD_System_GetNetworkProxy alias "FMOD_System_GetNetworkProxy" (byval system as FMOD_SYSTEM ptr, byval proxy as string, byval proxylen as integer) as FMOD_RESULT
declare function FMOD_System_SetNetworkTimeout alias "FMOD_System_SetNetworkTimeout" (byval system as FMOD_SYSTEM ptr, byval timeout as integer) as FMOD_RESULT
declare function FMOD_System_GetNetworkTimeout alias "FMOD_System_GetNetworkTimeout" (byval system as FMOD_SYSTEM ptr, byval timeout as integer ptr) as FMOD_RESULT
declare function FMOD_System_SetUserData alias "FMOD_System_SetUserData" (byval system as FMOD_SYSTEM ptr, byval userdata as any ptr) as FMOD_RESULT
declare function FMOD_System_GetUserData alias "FMOD_System_GetUserData" (byval system as FMOD_SYSTEM ptr, byval userdata as any ptr ptr) as FMOD_RESULT
declare function FMOD_Sound_Release alias "FMOD_Sound_Release" (byval sound as FMOD_SOUND ptr) as FMOD_RESULT
declare function FMOD_Sound_GetSystemObject alias "FMOD_Sound_GetSystemObject" (byval sound as FMOD_SOUND ptr, byval system as FMOD_SYSTEM ptr ptr) as FMOD_RESULT
declare function FMOD_Sound_Lock alias "FMOD_Sound_Lock" (byval sound as FMOD_SOUND ptr, byval offset as uinteger, byval length as uinteger, byval ptr1 as any ptr ptr, byval ptr2 as any ptr ptr, byval len1 as uinteger ptr, byval len2 as uinteger ptr) as FMOD_RESULT
declare function FMOD_Sound_Unlock alias "FMOD_Sound_Unlock" (byval sound as FMOD_SOUND ptr, byval ptr1 as any ptr, byval ptr2 as any ptr, byval len1 as uinteger, byval len2 as uinteger) as FMOD_RESULT
declare function FMOD_Sound_SetDefaults alias "FMOD_Sound_SetDefaults" (byval sound as FMOD_SOUND ptr, byval frequency as single, byval volume as single, byval pan as single, byval priority as integer) as FMOD_RESULT
declare function FMOD_Sound_GetDefaults alias "FMOD_Sound_GetDefaults" (byval sound as FMOD_SOUND ptr, byval frequency as single ptr, byval volume as single ptr, byval pan as single ptr, byval priority as integer ptr) as FMOD_RESULT
declare function FMOD_Sound_SetVariations alias "FMOD_Sound_SetVariations" (byval sound as FMOD_SOUND ptr, byval frequencyvar as single, byval volumevar as single, byval panvar as single) as FMOD_RESULT
declare function FMOD_Sound_GetVariations alias "FMOD_Sound_GetVariations" (byval sound as FMOD_SOUND ptr, byval frequencyvar as single ptr, byval volumevar as single ptr, byval panvar as single ptr) as FMOD_RESULT
declare function FMOD_Sound_Set3DMinMaxDistance alias "FMOD_Sound_Set3DMinMaxDistance" (byval sound as FMOD_SOUND ptr, byval min as single, byval max as single) as FMOD_RESULT
declare function FMOD_Sound_Get3DMinMaxDistance alias "FMOD_Sound_Get3DMinMaxDistance" (byval sound as FMOD_SOUND ptr, byval min as single ptr, byval max as single ptr) as FMOD_RESULT
declare function FMOD_Sound_Set3DConeSettings alias "FMOD_Sound_Set3DConeSettings" (byval sound as FMOD_SOUND ptr, byval insideconeangle as single, byval outsideconeangle as single, byval outsidevolume as single) as FMOD_RESULT
declare function FMOD_Sound_Get3DConeSettings alias "FMOD_Sound_Get3DConeSettings" (byval sound as FMOD_SOUND ptr, byval insideconeangle as single ptr, byval outsideconeangle as single ptr, byval outsidevolume as single ptr) as FMOD_RESULT
declare function FMOD_Sound_SetSubSound alias "FMOD_Sound_SetSubSound" (byval sound as FMOD_SOUND ptr, byval index as integer, byval subsound as FMOD_SOUND ptr) as FMOD_RESULT
declare function FMOD_Sound_GetSubSound alias "FMOD_Sound_GetSubSound" (byval sound as FMOD_SOUND ptr, byval index as integer, byval subsound as FMOD_SOUND ptr ptr) as FMOD_RESULT
declare function FMOD_Sound_SetSubSoundSentence alias "FMOD_Sound_SetSubSoundSentence" (byval sound as FMOD_SOUND ptr, byval subsoundlist as integer ptr, byval numsubsounds as integer) as FMOD_RESULT
declare function FMOD_Sound_GetName alias "FMOD_Sound_GetName" (byval sound as FMOD_SOUND ptr, byval name as string, byval namelen as integer) as FMOD_RESULT
declare function FMOD_Sound_GetLength alias "FMOD_Sound_GetLength" (byval sound as FMOD_SOUND ptr, byval length as uinteger ptr, byval lengthtype as FMOD_TIMEUNIT) as FMOD_RESULT
declare function FMOD_Sound_GetFormat alias "FMOD_Sound_GetFormat" (byval sound as FMOD_SOUND ptr, byval type as FMOD_SOUND_TYPE ptr, byval format as FMOD_SOUND_FORMAT ptr, byval channels as integer ptr, byval bits as integer ptr) as FMOD_RESULT
declare function FMOD_Sound_GetNumSubSounds alias "FMOD_Sound_GetNumSubSounds" (byval sound as FMOD_SOUND ptr, byval numsubsounds as integer ptr) as FMOD_RESULT
declare function FMOD_Sound_GetNumTags alias "FMOD_Sound_GetNumTags" (byval sound as FMOD_SOUND ptr, byval numtags as integer ptr, byval numtagsupdated as integer ptr) as FMOD_RESULT
declare function FMOD_Sound_GetTag alias "FMOD_Sound_GetTag" (byval sound as FMOD_SOUND ptr, byval name as string, byval index as integer, byval tag as FMOD_TAG ptr) as FMOD_RESULT
declare function FMOD_Sound_GetOpenState alias "FMOD_Sound_GetOpenState" (byval sound as FMOD_SOUND ptr, byval openstate as FMOD_OPENSTATE ptr, byval percentbuffered as uinteger ptr, byval starving as FMOD_BOOL ptr) as FMOD_RESULT
declare function FMOD_Sound_ReadData alias "FMOD_Sound_ReadData" (byval sound as FMOD_SOUND ptr, byval buffer as any ptr, byval lenbytes as uinteger, byval read as uinteger ptr) as FMOD_RESULT
declare function FMOD_Sound_SeekData alias "FMOD_Sound_SeekData" (byval sound as FMOD_SOUND ptr, byval pcm as uinteger) as FMOD_RESULT
declare function FMOD_Sound_GetNumSyncPoints alias "FMOD_Sound_GetNumSyncPoints" (byval sound as FMOD_SOUND ptr, byval numsyncpoints as integer ptr) as FMOD_RESULT
declare function FMOD_Sound_GetSyncPoint alias "FMOD_Sound_GetSyncPoint" (byval sound as FMOD_SOUND ptr, byval index as integer, byval point as FMOD_SYNCPOINT ptr ptr) as FMOD_RESULT
declare function FMOD_Sound_GetSyncPointInfo alias "FMOD_Sound_GetSyncPointInfo" (byval sound as FMOD_SOUND ptr, byval point as FMOD_SYNCPOINT ptr, byval name as string, byval namelen as integer, byval offset as uinteger ptr, byval offsettype as FMOD_TIMEUNIT) as FMOD_RESULT
declare function FMOD_Sound_AddSyncPoint alias "FMOD_Sound_AddSyncPoint" (byval sound as FMOD_SOUND ptr, byval offset as uinteger, byval offsettype as FMOD_TIMEUNIT, byval name as string, byval point as FMOD_SYNCPOINT ptr ptr) as FMOD_RESULT
declare function FMOD_Sound_DeleteSyncPoint alias "FMOD_Sound_DeleteSyncPoint" (byval sound as FMOD_SOUND ptr, byval point as FMOD_SYNCPOINT ptr) as FMOD_RESULT
declare function FMOD_Sound_SetMode alias "FMOD_Sound_SetMode" (byval sound as FMOD_SOUND ptr, byval mode as FMOD_MODE) as FMOD_RESULT
declare function FMOD_Sound_GetMode alias "FMOD_Sound_GetMode" (byval sound as FMOD_SOUND ptr, byval mode as FMOD_MODE ptr) as FMOD_RESULT
declare function FMOD_Sound_SetLoopCount alias "FMOD_Sound_SetLoopCount" (byval sound as FMOD_SOUND ptr, byval loopcount as integer) as FMOD_RESULT
declare function FMOD_Sound_GetLoopCount alias "FMOD_Sound_GetLoopCount" (byval sound as FMOD_SOUND ptr, byval loopcount as integer ptr) as FMOD_RESULT
declare function FMOD_Sound_SetLoopPoints alias "FMOD_Sound_SetLoopPoints" (byval sound as FMOD_SOUND ptr, byval loopstart as uinteger, byval loopstarttype as FMOD_TIMEUNIT, byval loopend as uinteger, byval loopendtype as FMOD_TIMEUNIT) as FMOD_RESULT
declare function FMOD_Sound_GetLoopPoints alias "FMOD_Sound_GetLoopPoints" (byval sound as FMOD_SOUND ptr, byval loopstart as uinteger ptr, byval loopstarttype as FMOD_TIMEUNIT, byval loopend as uinteger ptr, byval loopendtype as FMOD_TIMEUNIT) as FMOD_RESULT
declare function FMOD_Sound_SetUserData alias "FMOD_Sound_SetUserData" (byval sound as FMOD_SOUND ptr, byval userdata as any ptr) as FMOD_RESULT
declare function FMOD_Sound_GetUserData alias "FMOD_Sound_GetUserData" (byval sound as FMOD_SOUND ptr, byval userdata as any ptr ptr) as FMOD_RESULT
declare function FMOD_Channel_GetSystemObject alias "FMOD_Channel_GetSystemObject" (byval channel as FMOD_CHANNEL ptr, byval system as FMOD_SYSTEM ptr ptr) as FMOD_RESULT
declare function FMOD_Channel_Stop alias "FMOD_Channel_Stop" (byval channel as FMOD_CHANNEL ptr) as FMOD_RESULT
declare function FMOD_Channel_SetPaused alias "FMOD_Channel_SetPaused" (byval channel as FMOD_CHANNEL ptr, byval paused as FMOD_BOOL) as FMOD_RESULT
declare function FMOD_Channel_GetPaused alias "FMOD_Channel_GetPaused" (byval channel as FMOD_CHANNEL ptr, byval paused as FMOD_BOOL ptr) as FMOD_RESULT
declare function FMOD_Channel_SetVolume alias "FMOD_Channel_SetVolume" (byval channel as FMOD_CHANNEL ptr, byval volume as single) as FMOD_RESULT
declare function FMOD_Channel_GetVolume alias "FMOD_Channel_GetVolume" (byval channel as FMOD_CHANNEL ptr, byval volume as single ptr) as FMOD_RESULT
declare function FMOD_Channel_SetFrequency alias "FMOD_Channel_SetFrequency" (byval channel as FMOD_CHANNEL ptr, byval frequency as single) as FMOD_RESULT
declare function FMOD_Channel_GetFrequency alias "FMOD_Channel_GetFrequency" (byval channel as FMOD_CHANNEL ptr, byval frequency as single ptr) as FMOD_RESULT
declare function FMOD_Channel_SetPan alias "FMOD_Channel_SetPan" (byval channel as FMOD_CHANNEL ptr, byval pan as single) as FMOD_RESULT
declare function FMOD_Channel_GetPan alias "FMOD_Channel_GetPan" (byval channel as FMOD_CHANNEL ptr, byval pan as single ptr) as FMOD_RESULT
declare function FMOD_Channel_SetDelay alias "FMOD_Channel_SetDelay" (byval channel as FMOD_CHANNEL ptr, byval startdelay as uinteger, byval enddelay as uinteger) as FMOD_RESULT
declare function FMOD_Channel_GetDelay alias "FMOD_Channel_GetDelay" (byval channel as FMOD_CHANNEL ptr, byval startdelay as uinteger ptr, byval enddelay as uinteger ptr) as FMOD_RESULT
declare function FMOD_Channel_SetSpeakerMix alias "FMOD_Channel_SetSpeakerMix" (byval channel as FMOD_CHANNEL ptr, byval frontleft as single, byval frontright as single, byval center as single, byval lfe as single, byval backleft as single, byval backright as single, byval sideleft as single, byval sideright as single) as FMOD_RESULT
declare function FMOD_Channel_GetSpeakerMix alias "FMOD_Channel_GetSpeakerMix" (byval channel as FMOD_CHANNEL ptr, byval frontleft as single ptr, byval frontright as single ptr, byval center as single ptr, byval lfe as single ptr, byval backleft as single ptr, byval backright as single ptr, byval sideleft as single ptr, byval sideright as single ptr) as FMOD_RESULT
declare function FMOD_Channel_SetSpeakerLevels alias "FMOD_Channel_SetSpeakerLevels" (byval channel as FMOD_CHANNEL ptr, byval speaker as FMOD_SPEAKER, byval levels as single ptr, byval numlevels as integer) as FMOD_RESULT
declare function FMOD_Channel_GetSpeakerLevels alias "FMOD_Channel_GetSpeakerLevels" (byval channel as FMOD_CHANNEL ptr, byval speaker as FMOD_SPEAKER, byval levels as single ptr, byval numlevels as integer) as FMOD_RESULT
declare function FMOD_Channel_SetMute alias "FMOD_Channel_SetMute" (byval channel as FMOD_CHANNEL ptr, byval mute as FMOD_BOOL) as FMOD_RESULT
declare function FMOD_Channel_GetMute alias "FMOD_Channel_GetMute" (byval channel as FMOD_CHANNEL ptr, byval mute as FMOD_BOOL ptr) as FMOD_RESULT
declare function FMOD_Channel_SetPriority alias "FMOD_Channel_SetPriority" (byval channel as FMOD_CHANNEL ptr, byval priority as integer) as FMOD_RESULT
declare function FMOD_Channel_GetPriority alias "FMOD_Channel_GetPriority" (byval channel as FMOD_CHANNEL ptr, byval priority as integer ptr) as FMOD_RESULT
declare function FMOD_Channel_SetPosition alias "FMOD_Channel_SetPosition" (byval channel as FMOD_CHANNEL ptr, byval position as uinteger, byval postype as FMOD_TIMEUNIT) as FMOD_RESULT
declare function FMOD_Channel_GetPosition alias "FMOD_Channel_GetPosition" (byval channel as FMOD_CHANNEL ptr, byval position as uinteger ptr, byval postype as FMOD_TIMEUNIT) as FMOD_RESULT
declare function FMOD_Channel_SetReverbProperties alias "FMOD_Channel_SetReverbProperties" (byval channel as FMOD_CHANNEL ptr, byval prop as FMOD_REVERB_CHANNELPROPERTIES ptr) as FMOD_RESULT
declare function FMOD_Channel_GetReverbProperties alias "FMOD_Channel_GetReverbProperties" (byval channel as FMOD_CHANNEL ptr, byval prop as FMOD_REVERB_CHANNELPROPERTIES ptr) as FMOD_RESULT
declare function FMOD_Channel_SetChannelGroup alias "FMOD_Channel_SetChannelGroup" (byval channel as FMOD_CHANNEL ptr, byval channelgroup as FMOD_CHANNELGROUP ptr) as FMOD_RESULT
declare function FMOD_Channel_GetChannelGroup alias "FMOD_Channel_GetChannelGroup" (byval channel as FMOD_CHANNEL ptr, byval channelgroup as FMOD_CHANNELGROUP ptr ptr) as FMOD_RESULT
declare function FMOD_Channel_SetCallback alias "FMOD_Channel_SetCallback" (byval channel as FMOD_CHANNEL ptr, byval type as FMOD_CHANNEL_CALLBACKTYPE, byval callback as FMOD_CHANNEL_CALLBACK, byval command as integer) as FMOD_RESULT
declare function FMOD_Channel_Set3DAttributes alias "FMOD_Channel_Set3DAttributes" (byval channel as FMOD_CHANNEL ptr, byval pos as FMOD_VECTOR ptr, byval vel as FMOD_VECTOR ptr) as FMOD_RESULT
declare function FMOD_Channel_Get3DAttributes alias "FMOD_Channel_Get3DAttributes" (byval channel as FMOD_CHANNEL ptr, byval pos as FMOD_VECTOR ptr, byval vel as FMOD_VECTOR ptr) as FMOD_RESULT
declare function FMOD_Channel_Set3DMinMaxDistance alias "FMOD_Channel_Set3DMinMaxDistance" (byval channel as FMOD_CHANNEL ptr, byval mindistance as single, byval maxdistance as single) as FMOD_RESULT
declare function FMOD_Channel_Get3DMinMaxDistance alias "FMOD_Channel_Get3DMinMaxDistance" (byval channel as FMOD_CHANNEL ptr, byval mindistance as single ptr, byval maxdistance as single ptr) as FMOD_RESULT
declare function FMOD_Channel_Set3DConeSettings alias "FMOD_Channel_Set3DConeSettings" (byval channel as FMOD_CHANNEL ptr, byval insideconeangle as single, byval outsideconeangle as single, byval outsidevolume as single) as FMOD_RESULT
declare function FMOD_Channel_Get3DConeSettings alias "FMOD_Channel_Get3DConeSettings" (byval channel as FMOD_CHANNEL ptr, byval insideconeangle as single ptr, byval outsideconeangle as single ptr, byval outsidevolume as single ptr) as FMOD_RESULT
declare function FMOD_Channel_Set3DConeOrientation alias "FMOD_Channel_Set3DConeOrientation" (byval channel as FMOD_CHANNEL ptr, byval orientation as FMOD_VECTOR ptr) as FMOD_RESULT
declare function FMOD_Channel_Get3DConeOrientation alias "FMOD_Channel_Get3DConeOrientation" (byval channel as FMOD_CHANNEL ptr, byval orientation as FMOD_VECTOR ptr) as FMOD_RESULT
declare function FMOD_Channel_Set3DOcclusion alias "FMOD_Channel_Set3DOcclusion" (byval channel as FMOD_CHANNEL ptr, byval directocclusion as single, byval reverbocclusion as single) as FMOD_RESULT
declare function FMOD_Channel_Get3DOcclusion alias "FMOD_Channel_Get3DOcclusion" (byval channel as FMOD_CHANNEL ptr, byval directocclusion as single ptr, byval reverbocclusion as single ptr) as FMOD_RESULT
declare function FMOD_Channel_GetDSPHead alias "FMOD_Channel_GetDSPHead" (byval channel as FMOD_CHANNEL ptr, byval dsp as FMOD_DSP ptr ptr) as FMOD_RESULT
declare function FMOD_Channel_AddDSP alias "FMOD_Channel_AddDSP" (byval channel as FMOD_CHANNEL ptr, byval dsp as FMOD_DSP ptr) as FMOD_RESULT
declare function FMOD_Channel_IsPlaying alias "FMOD_Channel_IsPlaying" (byval channel as FMOD_CHANNEL ptr, byval isplaying as FMOD_BOOL ptr) as FMOD_RESULT
declare function FMOD_Channel_IsVirtual alias "FMOD_Channel_IsVirtual" (byval channel as FMOD_CHANNEL ptr, byval isvirtual as FMOD_BOOL ptr) as FMOD_RESULT
declare function FMOD_Channel_GetAudibility alias "FMOD_Channel_GetAudibility" (byval channel as FMOD_CHANNEL ptr, byval audibility as single ptr) as FMOD_RESULT
declare function FMOD_Channel_GetCurrentSound alias "FMOD_Channel_GetCurrentSound" (byval channel as FMOD_CHANNEL ptr, byval sound as FMOD_SOUND ptr ptr) as FMOD_RESULT
declare function FMOD_Channel_GetSpectrum alias "FMOD_Channel_GetSpectrum" (byval channel as FMOD_CHANNEL ptr, byval spectrumarray as single ptr, byval numvalues as integer, byval channeloffset as integer, byval windowtype as FMOD_DSP_FFT_WINDOW) as FMOD_RESULT
declare function FMOD_Channel_GetWaveData alias "FMOD_Channel_GetWaveData" (byval channel as FMOD_CHANNEL ptr, byval wavearray as single ptr, byval numvalues as integer, byval channeloffset as integer) as FMOD_RESULT
declare function FMOD_Channel_SetMode alias "FMOD_Channel_SetMode" (byval channel as FMOD_CHANNEL ptr, byval mode as FMOD_MODE) as FMOD_RESULT
declare function FMOD_Channel_GetMode alias "FMOD_Channel_GetMode" (byval channel as FMOD_CHANNEL ptr, byval mode as FMOD_MODE ptr) as FMOD_RESULT
declare function FMOD_Channel_SetLoopCount alias "FMOD_Channel_SetLoopCount" (byval channel as FMOD_CHANNEL ptr, byval loopcount as integer) as FMOD_RESULT
declare function FMOD_Channel_GetLoopCount alias "FMOD_Channel_GetLoopCount" (byval channel as FMOD_CHANNEL ptr, byval loopcount as integer ptr) as FMOD_RESULT
declare function FMOD_Channel_SetLoopPoints alias "FMOD_Channel_SetLoopPoints" (byval channel as FMOD_CHANNEL ptr, byval loopstart as uinteger, byval loopstarttype as FMOD_TIMEUNIT, byval loopend as uinteger, byval loopendtype as FMOD_TIMEUNIT) as FMOD_RESULT
declare function FMOD_Channel_GetLoopPoints alias "FMOD_Channel_GetLoopPoints" (byval channel as FMOD_CHANNEL ptr, byval loopstart as uinteger ptr, byval loopstarttype as FMOD_TIMEUNIT, byval loopend as uinteger ptr, byval loopendtype as FMOD_TIMEUNIT) as FMOD_RESULT
declare function FMOD_Channel_SetUserData alias "FMOD_Channel_SetUserData" (byval channel as FMOD_CHANNEL ptr, byval userdata as any ptr) as FMOD_RESULT
declare function FMOD_Channel_GetUserData alias "FMOD_Channel_GetUserData" (byval channel as FMOD_CHANNEL ptr, byval userdata as any ptr ptr) as FMOD_RESULT
declare function FMOD_ChannelGroup_Release alias "FMOD_ChannelGroup_Release" (byval channelgroup as FMOD_CHANNELGROUP ptr) as FMOD_RESULT
declare function FMOD_ChannelGroup_GetSystemObject alias "FMOD_ChannelGroup_GetSystemObject" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval system as FMOD_SYSTEM ptr ptr) as FMOD_RESULT
declare function FMOD_ChannelGroup_SetVolume alias "FMOD_ChannelGroup_SetVolume" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval volume as single) as FMOD_RESULT
declare function FMOD_ChannelGroup_GetVolume alias "FMOD_ChannelGroup_GetVolume" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval volume as single ptr) as FMOD_RESULT
declare function FMOD_ChannelGroup_SetPitch alias "FMOD_ChannelGroup_SetPitch" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval pitch as single) as FMOD_RESULT
declare function FMOD_ChannelGroup_GetPitch alias "FMOD_ChannelGroup_GetPitch" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval pitch as single ptr) as FMOD_RESULT
declare function FMOD_ChannelGroup_Stop alias "FMOD_ChannelGroup_Stop" (byval channelgroup as FMOD_CHANNELGROUP ptr) as FMOD_RESULT
declare function FMOD_ChannelGroup_OverridePaused alias "FMOD_ChannelGroup_OverridePaused" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval paused as FMOD_BOOL) as FMOD_RESULT
declare function FMOD_ChannelGroup_OverrideVolume alias "FMOD_ChannelGroup_OverrideVolume" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval volume as single) as FMOD_RESULT
declare function FMOD_ChannelGroup_OverrideFrequency alias "FMOD_ChannelGroup_OverrideFrequency" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval frequency as single) as FMOD_RESULT
declare function FMOD_ChannelGroup_OverridePan alias "FMOD_ChannelGroup_OverridePan" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval pan as single) as FMOD_RESULT
declare function FMOD_ChannelGroup_OverrideMute alias "FMOD_ChannelGroup_OverrideMute" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval mute as FMOD_BOOL) as FMOD_RESULT
declare function FMOD_ChannelGroup_OverrideReverbProperties alias "FMOD_ChannelGroup_OverrideReverbProperties" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval prop as FMOD_REVERB_CHANNELPROPERTIES ptr) as FMOD_RESULT
declare function FMOD_ChannelGroup_Override3DAttributes alias "FMOD_ChannelGroup_Override3DAttributes" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval pos as FMOD_VECTOR ptr, byval vel as FMOD_VECTOR ptr) as FMOD_RESULT
declare function FMOD_ChannelGroup_AddGroup alias "FMOD_ChannelGroup_AddGroup" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval group as FMOD_CHANNELGROUP ptr) as FMOD_RESULT
declare function FMOD_ChannelGroup_GetNumGroups alias "FMOD_ChannelGroup_GetNumGroups" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval numgroups as integer ptr) as FMOD_RESULT
declare function FMOD_ChannelGroup_GetGroup alias "FMOD_ChannelGroup_GetGroup" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval index as integer, byval group as FMOD_CHANNELGROUP ptr ptr) as FMOD_RESULT
declare function FMOD_ChannelGroup_GetDSPHead alias "FMOD_ChannelGroup_GetDSPHead" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval dsp as FMOD_DSP ptr ptr) as FMOD_RESULT
declare function FMOD_ChannelGroup_AddDSP alias "FMOD_ChannelGroup_AddDSP" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval dsp as FMOD_DSP ptr) as FMOD_RESULT
declare function FMOD_ChannelGroup_GetName alias "FMOD_ChannelGroup_GetName" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval name as string, byval namelen as integer) as FMOD_RESULT
declare function FMOD_ChannelGroup_GetNumChannels alias "FMOD_ChannelGroup_GetNumChannels" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval numchannels as integer ptr) as FMOD_RESULT
declare function FMOD_ChannelGroup_GetChannel alias "FMOD_ChannelGroup_GetChannel" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval index as integer, byval channel as FMOD_CHANNEL ptr ptr) as FMOD_RESULT
declare function FMOD_ChannelGroup_GetSpectrum alias "FMOD_ChannelGroup_GetSpectrum" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval spectrumarray as single ptr, byval numvalues as integer, byval channeloffset as integer, byval windowtype as FMOD_DSP_FFT_WINDOW) as FMOD_RESULT
declare function FMOD_ChannelGroup_GetWaveData alias "FMOD_ChannelGroup_GetWaveData" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval wavearray as single ptr, byval numvalues as integer, byval channeloffset as integer) as FMOD_RESULT
declare function FMOD_ChannelGroup_SetUserData alias "FMOD_ChannelGroup_SetUserData" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval userdata as any ptr) as FMOD_RESULT
declare function FMOD_ChannelGroup_GetUserData alias "FMOD_ChannelGroup_GetUserData" (byval channelgroup as FMOD_CHANNELGROUP ptr, byval userdata as any ptr ptr) as FMOD_RESULT
declare function FMOD_DSP_Release alias "FMOD_DSP_Release" (byval dsp as FMOD_DSP ptr) as FMOD_RESULT
declare function FMOD_DSP_GetSystemObject alias "FMOD_DSP_GetSystemObject" (byval dsp as FMOD_DSP ptr, byval system as FMOD_SYSTEM ptr ptr) as FMOD_RESULT
declare function FMOD_DSP_AddInput alias "FMOD_DSP_AddInput" (byval dsp as FMOD_DSP ptr, byval target as FMOD_DSP ptr) as FMOD_RESULT
declare function FMOD_DSP_DisconnectFrom alias "FMOD_DSP_DisconnectFrom" (byval dsp as FMOD_DSP ptr, byval target as FMOD_DSP ptr) as FMOD_RESULT
declare function FMOD_DSP_Remove alias "FMOD_DSP_Remove" (byval dsp as FMOD_DSP ptr) as FMOD_RESULT
declare function FMOD_DSP_GetNumInputs alias "FMOD_DSP_GetNumInputs" (byval dsp as FMOD_DSP ptr, byval numinputs as integer ptr) as FMOD_RESULT
declare function FMOD_DSP_GetNumOutputs alias "FMOD_DSP_GetNumOutputs" (byval dsp as FMOD_DSP ptr, byval numoutputs as integer ptr) as FMOD_RESULT
declare function FMOD_DSP_GetInput alias "FMOD_DSP_GetInput" (byval dsp as FMOD_DSP ptr, byval index as integer, byval input as FMOD_DSP ptr ptr) as FMOD_RESULT
declare function FMOD_DSP_GetOutput alias "FMOD_DSP_GetOutput" (byval dsp as FMOD_DSP ptr, byval index as integer, byval output as FMOD_DSP ptr ptr) as FMOD_RESULT
declare function FMOD_DSP_SetInputMix alias "FMOD_DSP_SetInputMix" (byval dsp as FMOD_DSP ptr, byval index as integer, byval volume as single) as FMOD_RESULT
declare function FMOD_DSP_GetInputMix alias "FMOD_DSP_GetInputMix" (byval dsp as FMOD_DSP ptr, byval index as integer, byval volume as single ptr) as FMOD_RESULT
declare function FMOD_DSP_SetInputLevels alias "FMOD_DSP_SetInputLevels" (byval dsp as FMOD_DSP ptr, byval index as integer, byval speaker as FMOD_SPEAKER, byval levels as single ptr, byval numlevels as integer) as FMOD_RESULT
declare function FMOD_DSP_GetInputLevels alias "FMOD_DSP_GetInputLevels" (byval dsp as FMOD_DSP ptr, byval index as integer, byval speaker as FMOD_SPEAKER, byval levels as single ptr, byval numlevels as integer) as FMOD_RESULT
declare function FMOD_DSP_SetActive alias "FMOD_DSP_SetActive" (byval dsp as FMOD_DSP ptr, byval active as FMOD_BOOL) as FMOD_RESULT
declare function FMOD_DSP_GetActive alias "FMOD_DSP_GetActive" (byval dsp as FMOD_DSP ptr, byval active as FMOD_BOOL ptr) as FMOD_RESULT
declare function FMOD_DSP_SetBypass alias "FMOD_DSP_SetBypass" (byval dsp as FMOD_DSP ptr, byval bypass as FMOD_BOOL) as FMOD_RESULT
declare function FMOD_DSP_GetBypass alias "FMOD_DSP_GetBypass" (byval dsp as FMOD_DSP ptr, byval bypass as FMOD_BOOL ptr) as FMOD_RESULT
declare function FMOD_DSP_Reset alias "FMOD_DSP_Reset" (byval dsp as FMOD_DSP ptr) as FMOD_RESULT
declare function FMOD_DSP_SetParameter alias "FMOD_DSP_SetParameter" (byval dsp as FMOD_DSP ptr, byval index as integer, byval value as single) as FMOD_RESULT
declare function FMOD_DSP_GetParameter alias "FMOD_DSP_GetParameter" (byval dsp as FMOD_DSP ptr, byval index as integer, byval value as single ptr, byval valuestr as string, byval valuestrlen as integer) as FMOD_RESULT
declare function FMOD_DSP_GetNumParameters alias "FMOD_DSP_GetNumParameters" (byval dsp as FMOD_DSP ptr, byval numparams as integer ptr) as FMOD_RESULT
declare function FMOD_DSP_GetParameterInfo alias "FMOD_DSP_GetParameterInfo" (byval dsp as FMOD_DSP ptr, byval index as integer, byval name as string, byval label as string, byval description as string, byval descriptionlen as integer, byval min as single ptr, byval max as single ptr) as FMOD_RESULT
declare function FMOD_DSP_ShowConfigDialog alias "FMOD_DSP_ShowConfigDialog" (byval dsp as FMOD_DSP ptr, byval hwnd as any ptr, byval show as FMOD_BOOL) as FMOD_RESULT
declare function FMOD_DSP_GetInfo alias "FMOD_DSP_GetInfo" (byval dsp as FMOD_DSP ptr, byval name as string, byval version as uinteger ptr, byval channels as integer ptr, byval configwidth as integer ptr, byval configheight as integer ptr) as FMOD_RESULT
declare function FMOD_DSP_SetDefaults alias "FMOD_DSP_SetDefaults" (byval dsp as FMOD_DSP ptr, byval frequency as single, byval volume as single, byval pan as single, byval priority as integer) as FMOD_RESULT
declare function FMOD_DSP_GetDefaults alias "FMOD_DSP_GetDefaults" (byval dsp as FMOD_DSP ptr, byval frequency as single ptr, byval volume as single ptr, byval pan as single ptr, byval priority as integer ptr) as FMOD_RESULT
declare function FMOD_DSP_SetUserData alias "FMOD_DSP_SetUserData" (byval dsp as FMOD_DSP ptr, byval userdata as any ptr) as FMOD_RESULT
declare function FMOD_DSP_GetUserData alias "FMOD_DSP_GetUserData" (byval dsp as FMOD_DSP ptr, byval userdata as any ptr ptr) as FMOD_RESULT
declare function FMOD_Geometry_Release alias "FMOD_Geometry_Release" (byval geometry as FMOD_GEOMETRY ptr) as FMOD_RESULT
declare function FMOD_Geometry_AddPolygon alias "FMOD_Geometry_AddPolygon" (byval geometry as FMOD_GEOMETRY ptr, byval directocclusion as single, byval reverbocclusion as single, byval doublesided as FMOD_BOOL, byval numvertices as integer, byval vertices as FMOD_VECTOR ptr, byval polygonindex as integer ptr) as FMOD_RESULT
declare function FMOD_Geometry_GetNumPolygons alias "FMOD_Geometry_GetNumPolygons" (byval geometry as FMOD_GEOMETRY ptr, byval numpolygons as integer ptr) as FMOD_RESULT
declare function FMOD_Geometry_GetMaxPolygons alias "FMOD_Geometry_GetMaxPolygons" (byval geometry as FMOD_GEOMETRY ptr, byval maxpolygons as integer ptr, byval maxvertices as integer ptr) as FMOD_RESULT
declare function FMOD_Geometry_GetPolygonNumVertices alias "FMOD_Geometry_GetPolygonNumVertices" (byval geometry as FMOD_GEOMETRY ptr, byval index as integer, byval numvertices as integer ptr) as FMOD_RESULT
declare function FMOD_Geometry_SetPolygonVertex alias "FMOD_Geometry_SetPolygonVertex" (byval geometry as FMOD_GEOMETRY ptr, byval index as integer, byval vertexindex as integer, byval vertex as FMOD_VECTOR ptr) as FMOD_RESULT
declare function FMOD_Geometry_GetPolygonVertex alias "FMOD_Geometry_GetPolygonVertex" (byval geometry as FMOD_GEOMETRY ptr, byval index as integer, byval vertexindex as integer, byval vertex as FMOD_VECTOR ptr) as FMOD_RESULT
declare function FMOD_Geometry_SetPolygonAttributes alias "FMOD_Geometry_SetPolygonAttributes" (byval geometry as FMOD_GEOMETRY ptr, byval index as integer, byval directocclusion as single, byval reverbocclusion as single, byval doublesided as FMOD_BOOL) as FMOD_RESULT
declare function FMOD_Geometry_GetPolygonAttributes alias "FMOD_Geometry_GetPolygonAttributes" (byval geometry as FMOD_GEOMETRY ptr, byval index as integer, byval directocclusion as single ptr, byval reverbocclusion as single ptr, byval doublesided as FMOD_BOOL ptr) as FMOD_RESULT
declare function FMOD_Geometry_SetActive alias "FMOD_Geometry_SetActive" (byval geometry as FMOD_GEOMETRY ptr, byval active as FMOD_BOOL) as FMOD_RESULT
declare function FMOD_Geometry_GetActive alias "FMOD_Geometry_GetActive" (byval geometry as FMOD_GEOMETRY ptr, byval active as FMOD_BOOL ptr) as FMOD_RESULT
declare function FMOD_Geometry_SetRotation alias "FMOD_Geometry_SetRotation" (byval geometry as FMOD_GEOMETRY ptr, byval forward as FMOD_VECTOR ptr, byval up as FMOD_VECTOR ptr) as FMOD_RESULT
declare function FMOD_Geometry_GetRotation alias "FMOD_Geometry_GetRotation" (byval geometry as FMOD_GEOMETRY ptr, byval forward as FMOD_VECTOR ptr, byval up as FMOD_VECTOR ptr) as FMOD_RESULT
declare function FMOD_Geometry_SetPosition alias "FMOD_Geometry_SetPosition" (byval geometry as FMOD_GEOMETRY ptr, byval position as FMOD_VECTOR ptr) as FMOD_RESULT
declare function FMOD_Geometry_GetPosition alias "FMOD_Geometry_GetPosition" (byval geometry as FMOD_GEOMETRY ptr, byval position as FMOD_VECTOR ptr) as FMOD_RESULT
declare function FMOD_Geometry_SetScale alias "FMOD_Geometry_SetScale" (byval geometry as FMOD_GEOMETRY ptr, byval scale as FMOD_VECTOR ptr) as FMOD_RESULT
declare function FMOD_Geometry_GetScale alias "FMOD_Geometry_GetScale" (byval geometry as FMOD_GEOMETRY ptr, byval scale as FMOD_VECTOR ptr) as FMOD_RESULT
declare function FMOD_Geometry_Save alias "FMOD_Geometry_Save" (byval geometry as FMOD_GEOMETRY ptr, byval data as any ptr, byval datasize as integer ptr) as FMOD_RESULT
declare function FMOD_Geometry_SetUserData alias "FMOD_Geometry_SetUserData" (byval geometry as FMOD_GEOMETRY ptr, byval userdata as any ptr) as FMOD_RESULT
declare function FMOD_Geometry_GetUserData alias "FMOD_Geometry_GetUserData" (byval geometry as FMOD_GEOMETRY ptr, byval userdata as any ptr ptr) as FMOD_RESULT

#endif
