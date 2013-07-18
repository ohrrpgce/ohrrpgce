''
''
'' soundcard -- header translated with help of SWIG FB wrapper
''
'' NOTICE: This file is part of the FreeBASIC Compiler package and can't
''         be included in other distributions without authorization.
''
''
'' Copyright by Hannu Savolainen 1993-1997
'' 
'' Redistribution and use in source and binary forms, with or without
'' modification, are permitted provided that the following conditions are
'' met: 1. Redistributions of source code must retain the above copyright
'' notice, this list of conditions and the following disclaimer. 2.
'' Redistributions in binary form must reproduce the above copyright notice,
'' this list of conditions and the following disclaimer in the documentation
'' and/or other materials provided with the distribution.
'' 
'' THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY
'' EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
'' WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
'' DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
'' ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
'' DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
'' SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
'' CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
'' LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
'' OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
'' SUCH DAMAGE.

#ifndef __soundcard_bi__
#define __soundcard_bi__

#define SOUND_VERSION &h030802
#define OPEN_SOUND_SYSTEM

#define SNDCARD_ADLIB 1
#define SNDCARD_SB 2
#define SNDCARD_PAS 3
#define SNDCARD_GUS 4
#define SNDCARD_MPU401 5
#define SNDCARD_SB16 6
#define SNDCARD_SB16MIDI 7
#define SNDCARD_UART6850 8
#define SNDCARD_GUS16 9
#define SNDCARD_MSS 10
#define SNDCARD_PSS 11
#define SNDCARD_SSCAPE 12
#define SNDCARD_PSS_MPU 13
#define SNDCARD_PSS_MSS 14
#define SNDCARD_SSCAPE_MSS 15
#define SNDCARD_TRXPRO 16
#define SNDCARD_TRXPRO_SB 17
#define SNDCARD_TRXPRO_MPU 18
#define SNDCARD_MAD16 19
#define SNDCARD_MAD16_MPU 20
#define SNDCARD_CS4232 21
#define SNDCARD_CS4232_MPU 22
#define SNDCARD_MAUI 23
#define SNDCARD_PSEUDO_MSS 24
#define SNDCARD_GUSPNP 25
#define SNDCARD_UART401 26

#define SIOCPARM_MASK &h1fff
#define SIOC_VOID &h00000000
#define SIOC_OUT &h20000000
#define SIOC_IN &h40000000
#define SIOC_INOUT (&h40000000 or &h20000000)
#define _SIOC_NONE &h00000000
#define _SIOC_READ &h20000000
#define _SIOC_WRITE &h40000000
#define	_SIO(x,y)	(SIOC_VOID OR (x SHL 8) OR y)
#define	_SIOR(x,y,t)	(SIOC_OUT OR ((len(t) AND SIOCPARM_MASK) SHL 16) OR (x SHL 8) OR y)
#define	_SIOW(x,y,t)	(SIOC_IN OR ((len(t) AND SIOCPARM_MASK) SHL 16) OR (x SHL 8) OR y)
'' this should be _SIORW, but stdio got there first
#define	_SIOWR(x,y,t)	(SIOC_INOUT OR ((len(t) AND SIOCPARM_MASK) SHL 16) OR (x SHL 8) OR y)
#define _SIOC_SIZE(x)	((x SHR 16) AND SIOCPARM_MASK)	
#define _SIOC_DIR(x)	(x AND 0xf0000000)

#define SNDCTL_SEQ_RESET		_SIO  (81, 0)
#define SNDCTL_SEQ_SYNC			_SIO  (81, 1)
#define SNDCTL_SYNTH_INFO		_SIOWR(81, 2, synth_info)
#define SNDCTL_SEQ_CTRLRATE		_SIOWR(81, 3, integer)	' Set/get timer resolution (HZ)
#define SNDCTL_SEQ_GETOUTCOUNT	_SIOR (81, 4, integer)
#define SNDCTL_SEQ_GETINCOUNT	_SIOR (81, 5, integer)
#define SNDCTL_SEQ_PERCMODE		_SIOW (81, 6, integer)
#define SNDCTL_FM_LOAD_INSTR	_SIOW (81, 7, sbi_instrument)' Obsolete. Don't use!!!!!!
#define SNDCTL_SEQ_TESTMIDI		_SIOW (81, 8, integer)
#define SNDCTL_SEQ_RESETSAMPLES	_SIOW (81, 9, integer)
#define SNDCTL_SEQ_NRSYNTHS		_SIOR (81,10, integer)
#define SNDCTL_SEQ_NRMIDIS		_SIOR (81,11, integer)
#define SNDCTL_MIDI_INFO		_SIOWR(81,12, midi_info)
#define SNDCTL_SEQ_THRESHOLD	_SIOW (81,13, integer)
#define SNDCTL_SYNTH_MEMAVL		_SIOWR(81,14, integer)	' in=dev#, out=memsize
#define SNDCTL_FM_4OP_ENABLE	_SIOW (81,15, integer)	' in=dev#
#define SNDCTL_SEQ_PANIC		_SIO  (81,17)
#define SNDCTL_SEQ_OUTOFBAND	_SIOW (81,18, seq_event_rec)
#define SNDCTL_SEQ_GETTIME		_SIOR (81,19, integer)
#define SNDCTL_SYNTH_ID			_SIOWR(81,20, synth_info)
#define SNDCTL_SYNTH_CONTROL	_SIOWR(81,21, synth_control)
#define SNDCTL_SYNTH_REMOVESAMPLE	_SIOWR(81,22, remove_sample)

type synth_control
	devno as integer
	data as zstring * 4000
end type



type remove_sample
	devno as integer
	bankno as integer
	instrno as integer
end type



type seq_event_rec
	arr(0 to 8-1) as ubyte
end type



#define SNDCTL_TMR_TIMEBASE		_SIOWR(85, 1, integer)
#define SNDCTL_TMR_START		_SIO  (85, 2)
#define SNDCTL_TMR_STOP			_SIO  (85, 3)
#define SNDCTL_TMR_CONTINUE		_SIO  (85, 4)
#define SNDCTL_TMR_TEMPO		_SIOWR(85, 5, integer)
#define SNDCTL_TMR_SOURCE		_SIOWR(85, 6, integer)
#define TMR_INTERNAL &h00000001
#define TMR_EXTERNAL &h00000002
#define TMR_MODE_MIDI &h00000010
#define TMR_MODE_FSK &h00000020
#define TMR_MODE_CLS &h00000040
#define TMR_MODE_SMPTE &h00000080
#define SNDCTL_TMR_METRONOME	_SIOW (85, 7, integer)
#define SNDCTL_TMR_SELECT		_SIOW (85, 8, integer)

' /*
'  * Some big endian/little endian handling macros
'  */

' #if defined(_AIX) || defined(AIX) || defined(sparc) || defined(__sparc__) || defined(HPPA) || defined(PPC)
' /* Big endian machines */
' #  define _PATCHKEY(id) (0xfd00|id)
' #  define AFMT_S16_NE AFMT_S16_BE
' #else
#  define _PATCHKEY(id) ((id SHL 8) OR 0xfd)
#  define AFMT_S16_NE AFMT_S16_LE
' #endif

type patch_info
	key as ushort
	
	#define WAVE_PATCH	   _PATCHKEY(&H04)
	#define GUS_PATCH	   WAVE_PATCH
	#define WAVEFRONT_PATCH    _PATCHKEY(&H06)
	
	device_no as short
	instr_no as short
	mode as uinteger
	
#define WAVE_16_BITS		&H01	'/* bit 0 = 8 or 16 bit wave data. */
#define WAVE_UNSIGNED		&H02	'/* bit 1 = Signed - Unsigned data. */
#define WAVE_LOOPING		&H04	'/* bit 2 = looping enabled-1. */
#define WAVE_BIDIR_LOOP		&H08	'/* bit 3 = Set is bidirectional looping. */
#define WAVE_LOOP_BACK		&H10	'/* bit 4 = Set is looping backward. */
#define WAVE_SUSTAIN_ON		&H20	'/* bit 5 = Turn sustaining on. (Env. pts. 3)*/
#define WAVE_ENVELOPES		&H40	'/* bit 6 = Enable envelopes - 1 */
#define WAVE_FAST_RELEASE 	&H80	'/* bit 7 = Shut off immediately after note off */
				'/* 	(use the env_rate/env_offs fields). */
'/* Linux specific bits */
#define WAVE_VIBRATO		&H00010000	'/* The vibrato info is valid */
#define WAVE_TREMOLO		&H00020000	'/* The tremolo info is valid */
#define WAVE_SCALE			&H00040000	'/* The scaling info is valid */
#define WAVE_FRACTIONS		&H00080000	'/* Fraction information is valid */
'/* Reserved bits */
#define WAVE_ROM			&H40000000	'/* For future use */
#define WAVE_MULAW			&H20000000	'/* For future use */

	len as integer
	loop_start as integer
	loop_end as integer
	base_freq as uinteger
	base_note as uinteger
	high_note as uinteger
	low_note as uinteger
	panning as integer
	detuning as integer
	env_rate(0 to 6-1) as ubyte
	env_offset(0 to 6-1) as ubyte
	tremolo_sweep as ubyte
	tremolo_rate as ubyte
	tremolo_depth as ubyte
	vibrato_sweep as ubyte
	vibrato_rate as ubyte
	vibrato_depth as ubyte
	scale_frequency as integer
	scale_factor as uinteger
	volume as integer
	fractions as integer
	reserved1 as integer
	spare(0 to 2-1) as integer
	data as zstring * 1
end type

type sysex_info
	key as short
	
	#define SYSEX_PATCH ((&h05 shl 8) or &hfd)
	#define MAUI_PATCH ((&h06 shl 8) or &hfd)
	
	device_no as short
	len as integer
	data(0 to 1-1) as ubyte
end type


#define SEQ_NOTEOFF 0
#define SEQ_FMNOTEOFF 0
#define SEQ_NOTEON 1
#define SEQ_FMNOTEON 1
#define SEQ_PGMCHANGE 3
#define SEQ_FMPGMCHANGE 3
#define SEQ_MIDIPUTC 5
#define SEQ_DRUMON 6
#define SEQ_DRUMOFF 7
#define SEQ_AFTERTOUCH 9
#define SEQ_CONTROLLER 10
#define CTL_BANK_SELECT &h00
#define CTL_MODWHEEL &h01
#define CTL_BREATH &h02
#define CTL_FOOT &h04
#define CTL_PORTAMENTO_TIME &h05
#define CTL_DATA_ENTRY &h06
#define CTL_MAIN_VOLUME &h07
#define CTL_BALANCE &h08
#define CTL_PAN &h0a
#define CTL_EXPRESSION &h&b
#define CTL_GENERAL_PURPOSE1 &h10
#define CTL_GENERAL_PURPOSE2 &h11
#define CTL_GENERAL_PURPOSE3 &h12
#define CTL_GENERAL_PURPOSE4 &h13
#define CTL_DAMPER_PEDAL &h40
#define CTL_SUSTAIN &h40
#define CTL_HOLD &h40
#define CTL_PORTAMENTO &h41
#define CTL_SOSTENUTO &h42
#define CTL_SOFT_PEDAL &h43
#define CTL_HOLD2 &h45
#define CTL_GENERAL_PURPOSE5 &h50
#define CTL_GENERAL_PURPOSE6 &h51
#define CTL_GENERAL_PURPOSE7 &h52
#define CTL_GENERAL_PURPOSE8 &h53
#define CTL_EXT_EFF_DEPTH &h5b
#define CTL_TREMOLO_DEPTH &h5c
#define CTL_CHORUS_DEPTH &h5d
#define CTL_DETUNE_DEPTH &h5e
#define CTL_CELESTE_DEPTH &h5e
#define CTL_PHASER_DEPTH &h5f
#define CTL_DATA_INCREMENT &h60
#define CTL_DATA_DECREMENT &h61
#define CTL_NONREG_PARM_NUM_LSB &h62
#define CTL_NONREG_PARM_NUM_MSB &h63
#define CTL_REGIST_PARM_NUM_LSB &h64
#define CTL_REGIST_PARM_NUM_MSB &h65
#define CTRL_PITCH_BENDER 255
#define CTRL_PITCH_BENDER_RANGE 254
#define CTRL_EXPRESSION 253
#define CTRL_MAIN_VOLUME 252
#define SEQ_BALANCE 11
#define SEQ_VOLMODE 12
#define VOL_METHOD_ADAGIO 1
#define VOL_METHOD_LINEAR 2
#define SEQ_FULLSIZE &hfd
#define SEQ_PRIVATE &hfe
#define SEQ_EXTENDED &hff

type sbi_instr_data as ubyte ptr

type sbi_instrument
	key as ushort
	
#define FM_PATCH ((&h01 shl 8) or &hfd)
#define OPL3_PATCH ((&h03 shl 8) or &hfd)

	device as short
	channel as integer
	operators as sbi_instr_data
end type

type synth_info
	name as zstring * 30
	device as integer
	synth_type as integer

#define SYNTH_TYPE_FM 0
#define SYNTH_TYPE_SAMPLE 1
#define SYNTH_TYPE_MIDI 2
	
	synth_subtype as integer

#define FM_TYPE_ADLIB &h00
#define FM_TYPE_OPL3 &h01
#define MIDI_TYPE_MPU401 &h401
#define SAMPLE_TYPE_BASIC &h10
#define SAMPLE_TYPE_GUS &h10
#define SAMPLE_TYPE_WAVEFRONT &h11
	
	perc_mode as integer
	nr_voices as integer
	nr_drums as integer
	instr_bank_size as integer
	capabilities as uinteger

#define SYNTH_CAP_PERCMODE &h00000001
#define SYNTH_CAP_OPL3 &h00000002
#define SYNTH_CAP_INPUT &h00000004

	dummies(0 to 19-1) as integer
end type


type sound_timer_info
	name as zstring * 32
	caps as integer
end type

#define MIDI_CAP_MPU401 1

type midi_info
	name as zstring * 30
	device as integer
	capabilities as uinteger
	dev_type as integer
	dummies(0 to 18-1) as integer
end type

type mpu_command_rec
	cmd as ubyte
	nr_args as byte
	nr_returns as byte
	data(0 to 30-1) as ubyte
end type

#define SNDCTL_MIDI_PRETIME		_SIOWR(109, 0, integer)
#define SNDCTL_MIDI_MPUMODE		_SIOWR(109, 1, integer)
#define SNDCTL_MIDI_MPUCMD		_SIOWR(109, 2, mpu_command_rec)
#define SNDCTL_DSP_RESET		_SIO  (80, 0)
#define SNDCTL_DSP_SYNC			_SIO  (80, 1)
#define SNDCTL_DSP_SPEED		_SIOWR(80, 2, integer)
#define SNDCTL_DSP_STEREO		_SIOWR(80, 3, integer)
#define SNDCTL_DSP_GETBLKSIZE	_SIOWR(80, 4, integer)
#define SNDCTL_DSP_SAMPLESIZE	SNDCTL_DSP_SETFMT
#define SNDCTL_DSP_CHANNELS		_SIOWR(80, 6, integer)
#define SOUND_PCM_WRITE_CHANNELS SNDCTL_DSP_CHANNELS
#define SOUND_PCM_WRITE_FILTER	_SIOWR(80, 7, integer)
#define SNDCTL_DSP_POST			_SIO  (80, 8)
#define SNDCTL_DSP_SUBDIVIDE	_SIOWR(80, 9, integer)
#define SNDCTL_DSP_SETFRAGMENT	_SIOWR(80,10, integer)
#define SNDCTL_DSP_GETFMTS		_SIOR (80,11, integer) '/* Returns a mask */
#define SNDCTL_DSP_SETFMT		_SIOWR(80,5, integer) '/* Selects ONE fmt*/


#define AFMT_QUERY &h00000000
#define AFMT_MU_LAW &h00000001
#define AFMT_A_LAW &h00000002
#define AFMT_IMA_ADPCM &h00000004
#define AFMT_U8 &h00000008
#define AFMT_S16_LE &h00000010
#define AFMT_S16_BE &h00000020
#define AFMT_S8 &h00000040
#define AFMT_U16_LE &h00000080
#define AFMT_U16_BE &h00000100
#define AFMT_MPEG &h00000200
#define AFMT_AC3 &h00000400

type audio_buf_info
	fragments as integer
	fragstotal as integer
	fragsize as integer
	bytes as integer
end type



#define SNDCTL_DSP_GETOSPACE	_SIOR (80,12, audio_buf_info)
#define SNDCTL_DSP_GETISPACE	_SIOR (80,13, audio_buf_info)
#define SNDCTL_DSP_NONBLOCK		_SIO  (80,14)
#define SNDCTL_DSP_GETCAPS		_SIOR (80,15, integer)

#define DSP_CAP_REVISION &h000000ff
#define DSP_CAP_DUPLEX &h00000100
#define DSP_CAP_REALTIME &h00000200
#define DSP_CAP_BATCH &h00000400
#define DSP_CAP_COPROC &h00000800
#define DSP_CAP_TRIGGER &h00001000
#define DSP_CAP_MMAP &h00002000
#define DSP_CAP_MULTI &h00004000
#define DSP_CAP_BIND &h00008000
#define SNDCTL_DSP_GETTRIGGER		_SIOR (80,16, integer)
#define SNDCTL_DSP_SETTRIGGER		_SIOW (80,16, integer)
#define PCM_ENABLE_INPUT &h00000001
#define PCM_ENABLE_OUTPUT &h00000002

type count_info
	bytes as integer
	blocks as integer
	ptr as integer
end type



#define SNDCTL_DSP_GETIPTR		_SIOR (80,17, count_info)
#define SNDCTL_DSP_GETOPTR		_SIOR (80,18, count_info)

type buffmem_desc
	buffer as uinteger ptr
	size as integer
end type



#define SNDCTL_DSP_MAPINBUF		_SIOR (80, 19, buffmem_desc)
#define SNDCTL_DSP_MAPOUTBUF	_SIOR (80, 20, buffmem_desc)
#define SNDCTL_DSP_SETSYNCRO	_SIO  (80, 21)
#define SNDCTL_DSP_SETDUPLEX	_SIO  (80, 22)
#define SNDCTL_DSP_GETODELAY	_SIOR (80, 23, integer)

#define SNDCTL_DSP_GETCHANNELMASK	_SIOWR(80, 64, integer)
#define SNDCTL_DSP_BIND_CHANNEL		_SIOWR(80, 65, integer)
#define DSP_BIND_QUERY &h00000000
#define DSP_BIND_FRONT &h00000001
#define DSP_BIND_SURR &h00000002
#define DSP_BIND_CENTER_LFE &h00000004
#define DSP_BIND_HANDSET &h00000008
#define DSP_BIND_MIC &h00000010
#define DSP_BIND_MODEM1 &h00000020
#define DSP_BIND_MODEM2 &h00000040
#define DSP_BIND_I2S &h00000080
#define DSP_BIND_SPDIF &h00000100

#define SNDCTL_DSP_SETSPDIF		_SIOW (80, 66, integer)
#define SNDCTL_DSP_GETSPDIF		_SIOR (80, 67, integer)
#define SPDIF_PRO &h0001
#define SPDIF_N_AUD &h0002
#define SPDIF_COPY &h0004
#define SPDIF_PRE &h0008
#define SPDIF_CC &h07f0
#define SPDIF_L &h0800
#define SPDIF_DRS &h4000
#define SPDIF_V &h8000

#define SNDCTL_DSP_PROFILE		_SIOW (80, 23, integer)
#define APF_NORMAL 0
#define APF_NETWORK 1
#define APF_CPUINTENS 2

#define SOUND_PCM_READ_RATE		_SIOR (80, 2, int)
#define SOUND_PCM_READ_CHANNELS	_SIOR (80, 6, int)
#define SOUND_PCM_READ_BITS		_SIOR (80, 5, int)
#define SOUND_PCM_READ_FILTER	_SIOR (80, 7, int)

'/* Some alias names */
#define SOUND_PCM_WRITE_BITS	SNDCTL_DSP_SETFMT
#define SOUND_PCM_WRITE_RATE	SNDCTL_DSP_SPEED
#define SOUND_PCM_POST			SNDCTL_DSP_POST
#define SOUND_PCM_RESET			SNDCTL_DSP_RESET
#define SOUND_PCM_SYNC			SNDCTL_DSP_SYNC
#define SOUND_PCM_SUBDIVIDE		SNDCTL_DSP_SUBDIVIDE
#define SOUND_PCM_SETFRAGMENT	SNDCTL_DSP_SETFRAGMENT
#define SOUND_PCM_GETFMTS		SNDCTL_DSP_GETFMTS
#define SOUND_PCM_SETFMT		SNDCTL_DSP_SETFMT
#define SOUND_PCM_GETOSPACE		SNDCTL_DSP_GETOSPACE
#define SOUND_PCM_GETISPACE		SNDCTL_DSP_GETISPACE
#define SOUND_PCM_NONBLOCK		SNDCTL_DSP_NONBLOCK
#define SOUND_PCM_GETCAPS		SNDCTL_DSP_GETCAPS
#define SOUND_PCM_GETTRIGGER	SNDCTL_DSP_GETTRIGGER
#define SOUND_PCM_SETTRIGGER	SNDCTL_DSP_SETTRIGGER
#define SOUND_PCM_SETSYNCRO		SNDCTL_DSP_SETSYNCRO
#define SOUND_PCM_GETIPTR		SNDCTL_DSP_GETIPTR
#define SOUND_PCM_GETOPTR		SNDCTL_DSP_GETOPTR
#define SOUND_PCM_MAPINBUF		SNDCTL_DSP_MAPINBUF
#define SOUND_PCM_MAPOUTBUF		SNDCTL_DSP_MAPOUTBUF

type copr_buffer
	command as integer
	flags as integer
#define CPF_NONE &h0000
#define CPF_FIRST &h0001
#define CPF_LAST &h0002
	len as integer
	offs as integer
	data(0 to 4000-1) as ubyte
end type



type copr_debug_buf
	command as integer
	parm1 as integer
	parm2 as integer
	flags as integer
	len as integer
end type



type copr_msg
	len as integer
	data(0 to 4000-1) as ubyte
end type



#define SNDCTL_COPR_RESET   _SIO  (67, 0)
#define SNDCTL_COPR_LOAD	_SIOWR(67, 1, copr_buffer)
#define SNDCTL_COPR_RDATA	_SIOWR(67, 2, copr_debug_buf)
#define SNDCTL_COPR_RCODE	_SIOWR(67, 3, copr_debug_buf)
#define SNDCTL_COPR_WDATA	_SIOW (67, 4, copr_debug_buf)
#define SNDCTL_COPR_WCODE	_SIOW (67, 5, copr_debug_buf)
#define SNDCTL_COPR_RUN		_SIOWR(67, 6, copr_debug_buf)
#define SNDCTL_COPR_HALT	_SIOWR(67, 7, copr_debug_buf)
#define SNDCTL_COPR_SENDMSG	_SIOWR(67, 8, copr_msg)
#define SNDCTL_COPR_RCVMSG	_SIOR (67, 9, copr_msg)


#define SOUND_MIXER_NRDEVICES 25
#define SOUND_MIXER_VOLUME 0
#define SOUND_MIXER_BASS 1
#define SOUND_MIXER_TREBLE 2
#define SOUND_MIXER_SYNTH 3
#define SOUND_MIXER_PCM 4
#define SOUND_MIXER_SPEAKER 5
#define SOUND_MIXER_LINE 6
#define SOUND_MIXER_MIC 7
#define SOUND_MIXER_CD 8
#define SOUND_MIXER_IMIX 9
#define SOUND_MIXER_ALTPCM 10
#define SOUND_MIXER_RECLEV 11
#define SOUND_MIXER_IGAIN 12
#define SOUND_MIXER_OGAIN 13
#define SOUND_MIXER_LINE1 14
#define SOUND_MIXER_LINE2 15
#define SOUND_MIXER_LINE3 16
#define SOUND_MIXER_DIGITAL1 17
#define SOUND_MIXER_DIGITAL2 18
#define SOUND_MIXER_DIGITAL3 19
#define SOUND_MIXER_PHONEIN 20
#define SOUND_MIXER_PHONEOUT 21
#define SOUND_MIXER_VIDEO 22
#define SOUND_MIXER_RADIO 23
#define SOUND_MIXER_MONITOR 24
#define SOUND_ONOFF_MIN 28
#define SOUND_ONOFF_MAX 30
#define SOUND_MIXER_NONE 31
#define SOUND_MIXER_ENHANCE 31
#define SOUND_MIXER_MUTE 31
#define SOUND_MIXER_LOUD 31
#define SOUND_MIXER_RECSRC &hff
#define SOUND_MIXER_DEVMASK &hfe
#define SOUND_MIXER_RECMASK &hfd
#define SOUND_MIXER_CAPS &hfc
#define SOUND_CAP_EXCL_INPUT &h00000001
#define SOUND_MIXER_STEREODEVS &hfb
#define SOUND_MIXER_OUTSRC &hfa
#define SOUND_MIXER_OUTMASK &hf9
#define SOUND_MASK_VOLUME (1 shl 0)
#define SOUND_MASK_BASS (1 shl 1)
#define SOUND_MASK_TREBLE (1 shl 2)
#define SOUND_MASK_SYNTH (1 shl 3)
#define SOUND_MASK_PCM (1 shl 4)
#define SOUND_MASK_SPEAKER (1 shl 5)
#define SOUND_MASK_LINE (1 shl 6)
#define SOUND_MASK_MIC (1 shl 7)
#define SOUND_MASK_CD (1 shl 8)
#define SOUND_MASK_IMIX (1 shl 9)
#define SOUND_MASK_ALTPCM (1 shl 10)
#define SOUND_MASK_RECLEV (1 shl 11)
#define SOUND_MASK_IGAIN (1 shl 12)
#define SOUND_MASK_OGAIN (1 shl 13)
#define SOUND_MASK_LINE1 (1 shl 14)
#define SOUND_MASK_LINE2 (1 shl 15)
#define SOUND_MASK_LINE3 (1 shl 16)
#define SOUND_MASK_DIGITAL1 (1 shl 17)
#define SOUND_MASK_DIGITAL2 (1 shl 18)
#define SOUND_MASK_DIGITAL3 (1 shl 19)
#define SOUND_MASK_PHONEIN (1 shl 20)
#define SOUND_MASK_PHONEOUT (1 shl 21)
#define SOUND_MASK_RADIO (1 shl 23)
#define SOUND_MASK_VIDEO (1 shl 22)
#define SOUND_MASK_MONITOR (1 shl 24)
#define SOUND_MASK_MUTE (1 shl 31)
#define SOUND_MASK_ENHANCE (1 shl 31)
#define SOUND_MASK_LOUD (1 shl 31)

#define MIXER_READ(dev)		_SIOR(77, dev, integer)
#define SOUND_MIXER_READ_VOLUME		MIXER_READ(SOUND_MIXER_VOLUME)
#define SOUND_MIXER_READ_BASS		MIXER_READ(SOUND_MIXER_BASS)
#define SOUND_MIXER_READ_TREBLE		MIXER_READ(SOUND_MIXER_TREBLE)
#define SOUND_MIXER_READ_SYNTH		MIXER_READ(SOUND_MIXER_SYNTH)
#define SOUND_MIXER_READ_PCM		MIXER_READ(SOUND_MIXER_PCM)
#define SOUND_MIXER_READ_SPEAKER	MIXER_READ(SOUND_MIXER_SPEAKER)
#define SOUND_MIXER_READ_LINE		MIXER_READ(SOUND_MIXER_LINE)
#define SOUND_MIXER_READ_MIC		MIXER_READ(SOUND_MIXER_MIC)
#define SOUND_MIXER_READ_CD		MIXER_READ(SOUND_MIXER_CD)
#define SOUND_MIXER_READ_IMIX		MIXER_READ(SOUND_MIXER_IMIX)
#define SOUND_MIXER_READ_ALTPCM		MIXER_READ(SOUND_MIXER_ALTPCM)
#define SOUND_MIXER_READ_RECLEV		MIXER_READ(SOUND_MIXER_RECLEV)
#define SOUND_MIXER_READ_IGAIN		MIXER_READ(SOUND_MIXER_IGAIN)
#define SOUND_MIXER_READ_OGAIN		MIXER_READ(SOUND_MIXER_OGAIN)
#define SOUND_MIXER_READ_LINE1		MIXER_READ(SOUND_MIXER_LINE1)
#define SOUND_MIXER_READ_LINE2		MIXER_READ(SOUND_MIXER_LINE2)
#define SOUND_MIXER_READ_LINE3		MIXER_READ(SOUND_MIXER_LINE3)

'/* Obsolete macros */
#define SOUND_MIXER_READ_MUTE		MIXER_READ(SOUND_MIXER_MUTE)
#define SOUND_MIXER_READ_ENHANCE	MIXER_READ(SOUND_MIXER_ENHANCE)
#define SOUND_MIXER_READ_LOUD		MIXER_READ(SOUND_MIXER_LOUD)

#define SOUND_MIXER_READ_RECSRC		MIXER_READ(SOUND_MIXER_RECSRC)
#define SOUND_MIXER_READ_DEVMASK	MIXER_READ(SOUND_MIXER_DEVMASK)
#define SOUND_MIXER_READ_RECMASK	MIXER_READ(SOUND_MIXER_RECMASK)
#define SOUND_MIXER_READ_STEREODEVS	MIXER_READ(SOUND_MIXER_STEREODEVS)
#define SOUND_MIXER_READ_CAPS		MIXER_READ(SOUND_MIXER_CAPS)

#define MIXER_WRITE(dev)		_SIOWR(77, dev, integer)
#define SOUND_MIXER_WRITE_VOLUME	MIXER_WRITE(SOUND_MIXER_VOLUME)
#define SOUND_MIXER_WRITE_BASS		MIXER_WRITE(SOUND_MIXER_BASS)
#define SOUND_MIXER_WRITE_TREBLE	MIXER_WRITE(SOUND_MIXER_TREBLE)
#define SOUND_MIXER_WRITE_SYNTH		MIXER_WRITE(SOUND_MIXER_SYNTH)
#define SOUND_MIXER_WRITE_PCM		MIXER_WRITE(SOUND_MIXER_PCM)
#define SOUND_MIXER_WRITE_SPEAKER	MIXER_WRITE(SOUND_MIXER_SPEAKER)
#define SOUND_MIXER_WRITE_LINE		MIXER_WRITE(SOUND_MIXER_LINE)
#define SOUND_MIXER_WRITE_MIC		MIXER_WRITE(SOUND_MIXER_MIC)
#define SOUND_MIXER_WRITE_CD		MIXER_WRITE(SOUND_MIXER_CD)
#define SOUND_MIXER_WRITE_IMIX		MIXER_WRITE(SOUND_MIXER_IMIX)
#define SOUND_MIXER_WRITE_ALTPCM	MIXER_WRITE(SOUND_MIXER_ALTPCM)
#define SOUND_MIXER_WRITE_RECLEV	MIXER_WRITE(SOUND_MIXER_RECLEV)
#define SOUND_MIXER_WRITE_IGAIN		MIXER_WRITE(SOUND_MIXER_IGAIN)
#define SOUND_MIXER_WRITE_OGAIN		MIXER_WRITE(SOUND_MIXER_OGAIN)
#define SOUND_MIXER_WRITE_LINE1		MIXER_WRITE(SOUND_MIXER_LINE1)
#define SOUND_MIXER_WRITE_LINE2		MIXER_WRITE(SOUND_MIXER_LINE2)
#define SOUND_MIXER_WRITE_LINE3		MIXER_WRITE(SOUND_MIXER_LINE3)

'/* Obsolete macros */
#define SOUND_MIXER_WRITE_MUTE		MIXER_WRITE(SOUND_MIXER_MUTE)
#define SOUND_MIXER_WRITE_ENHANCE	MIXER_WRITE(SOUND_MIXER_ENHANCE)
#define SOUND_MIXER_WRITE_LOUD		MIXER_WRITE(SOUND_MIXER_LOUD)

#define SOUND_MIXER_WRITE_RECSRC	MIXER_WRITE(SOUND_MIXER_RECSRC)


type mixer_info
	id as zstring * 16
	name as zstring * 32
	modify_counter as integer
	fillers(0 to 10-1) as integer
end type



type _old_mixer_info
	id as zstring * 16
	name as zstring * 32
end type



#define SOUND_MIXER_INFO		_SIOR (77, 101, mixer_info)
#define SOUND_OLD_MIXER_INFO	_SIOR (77, 101, _old_mixer_info)

type mixer_record as ubyte ptr

#define SOUND_MIXER_ACCESS		_SIOWR(77, 102, mixer_record)

#define SOUND_MIXER_AGC  _SIOWR(77, 103, integer)
#define SOUND_MIXER_3DSE _SIOWR(77, 104, integer)

#define SOUND_MIXER_PRIVATE1	_SIOWR(77, 111, integer)
#define SOUND_MIXER_PRIVATE2	_SIOWR(77, 112, integer)
#define SOUND_MIXER_PRIVATE3	_SIOWR(77, 113, integer)
#define SOUND_MIXER_PRIVATE4	_SIOWR(77, 114, integer)
#define SOUND_MIXER_PRIVATE5	_SIOWR(77, 115, integer)

type mixer_vol_table
	num as integer
	name as zstring * 32
	levels(0 to 32-1) as integer
end type



#define SOUND_MIXER_GETLEVELS		_SIOWR(77, 116, mixer_vol_table)
#define SOUND_MIXER_SETLEVELS		_SIOWR(77, 117, mixer_vol_table)

#define OSS_GETVERSION			_SIOR (77, 118, integer)

#define EV_SEQ_LOCAL &h80
#define EV_TIMING &h81
#define EV_CHN_COMMON &h92
#define EV_CHN_VOICE &h93
#define EV_SYSEX &h94
#define MIDI_NOTEOFF &h80
#define MIDI_NOTEON &h90
#define MIDI_KEY_PRESSURE &hA0
#define MIDI_CTL_CHANGE &hB0
#define MIDI_PGM_CHANGE &hC0
#define MIDI_CHN_PRESSURE &hD0
#define MIDI_PITCH_BEND &hE0
#define MIDI_SYSTEM_PREFIX &hF0
#define TMR_WAIT_REL 1
#define TMR_WAIT_ABS 2
#define TMR_STOP 3
#define TMR_START 4
#define TMR_CONTINUE 5
#define TMR_TEMPO 6
#define TMR_ECHO 8
#define TMR_CLOCK 9
#define TMR_SPP 10
#define TMR_TIMESIG 11
#define LOCL_STARTAUDIO 1

' #if (!defined(__KERNEL__) && !defined(KERNEL) && !defined(INKERNEL) && !defined(_KERNEL)) || defined(USE_SEQ_MACROS) 
' ' /*
' '  *	Some convenience macros to simplify programming of the
' '  *	/dev/sequencer interface
' '  *
' '  *	These macros define the API which should be used when possible.
' '  */
' #define SEQ_DECLAREBUF()		SEQ_USE_EXTBUF()


' #define SEQ_PM_DEFINES int __foo_bar___
' #ifdef OSSLIB
' #  define SEQ_USE_EXTBUF() extern unsigned char *_seqbuf: extern int _seqbuflen;extern int _seqbufptr
' #  define SEQ_DEFINEBUF(len) SEQ_USE_EXTBUF();static int _requested_seqbuflen=len
' #  define _SEQ_ADVBUF(len) OSS_seq_advbuf(len, seqfd, _seqbuf, _seqbuflen)
' #  define _SEQ_NEEDBUF(len) OSS_seq_needbuf(len, seqfd, _seqbuf, _seqbuflen)
' #  define SEQ_DUMPBUF() OSS_seqbuf_dump(seqfd, _seqbuf, _seqbuflen)

' #  define SEQ_LOAD_GMINSTR(dev, instr) _
' 		OSS_patch_caching(dev, -1, instr, seqfd, _seqbuf, _seqbuflen)
' #  define SEQ_LOAD_GMDRUM(dev, drum) \
' 		OSS_drum_caching(dev, -1, drum, seqfd, _seqbuf, _seqbuflen)
' #else /* !OSSLIB */

' #  define SEQ_LOAD_GMINSTR(dev, instr)
' #  define SEQ_LOAD_GMDRUM(dev, drum)

' #  define SEQ_USE_EXTBUF() \
' 		extern unsigned char _seqbuf[]; \
' 		extern int _seqbuflen;extern int _seqbufptr

' #ifndef USE_SIMPLE_MACROS
' /* Sample seqbuf_dump() implementation:
'  *
'  *	SEQ_DEFINEBUF (2048);	-- Defines a buffer for 2048 bytes
'  *
'  *	int seqfd;		-- The file descriptor for /dev/sequencer.
'  *
'  *	void
'  *	seqbuf_dump ()
'  *	{
'  *	  if (_seqbufptr)
'  *	    if (write (seqfd, _seqbuf, _seqbufptr) == -1)
'  *	      {
'  *		perror ("write /dev/sequencer");
'  *		exit (-1);
'  *	      }
'  *	  _seqbufptr = 0;
'  *	}
'  */

' #define SEQ_DEFINEBUF(len)		unsigned char _seqbuf[len]; int _seqbuflen = len;int _seqbufptr = 0
' #define _SEQ_NEEDBUF(len)		if ((_seqbufptr+(len)) > _seqbuflen) seqbuf_dump()
 #define _SEQ_ADVBUF(len)		_seqbufptr += len
' #define SEQ_DUMPBUF			seqbuf_dump
' #else
' /*
'  * This variation of the sequencer macros is used just to format one event
'  * using fixed buffer.
'  * 
'  * The program using the macro library must define the following macros before
'  * using this library.
'  *
'  * #define _seqbuf 		 name of the buffer (unsigned char[]) 
'  * #define _SEQ_ADVBUF(len)	 If the applic needs to know the exact
'  *				 size of the event, this macro can be used.
'  *				 Otherwise this must be defined as empty.
'  * #define _seqbufptr		 Define the name of index variable or 0 if
'  *				 not required. 
'  */
' #define _SEQ_NEEDBUF(len)	/* empty */
' #endif
' #endif /* !OSSLIB */

' #define SEQ_VOLUME_MODE(dev, mode)	{_SEQ_NEEDBUF(8);\
' 					_seqbuf[_seqbufptr] = SEQ_EXTENDED;\
' 					_seqbuf[_seqbufptr+1] = SEQ_VOLMODE;\
' 					_seqbuf[_seqbufptr+2] = (dev);\
' 					_seqbuf[_seqbufptr+3] = (mode);\
' 					_seqbuf[_seqbufptr+4] = 0;\
' 					_seqbuf[_seqbufptr+5] = 0;\
' 					_seqbuf[_seqbufptr+6] = 0;\
' 					_seqbuf[_seqbufptr+7] = 0;\
' 					_SEQ_ADVBUF(8);}

' /*
'  * Midi voice messages
'  */

dim _seqbuf(2048) as UByte
dim _seqbufptr as integer

#define _CHN_VOICE(dev, event, chn, note, parm) _
					_seqbuf[_seqbufptr+1] = (dev):_
					_seqbuf[_seqbufptr+2] = (event):_
					_seqbuf[_seqbufptr+3] = (chn):_
					_seqbuf[_seqbufptr+4] = (note):_
					_seqbuf[_seqbufptr+5] = (parm):_
					_seqbuf[_seqbufptr+6] = (0):_
					_seqbuf[_seqbufptr+7] = 0:_
					_SEQ_ADVBUF(8):}



#define SEQ_START_NOTE(dev, chn, note, vol) _CHN_VOICE(dev, MIDI_NOTEON, chn, note, vol)

#define SEQ_STOP_NOTE(dev, chn, note, vol) _CHN_VOICE(dev, MIDI_NOTEOFF, chn, note, vol)

' #define SEQ_KEY_PRESSURE(dev, chn, note, pressure) \
' 		_CHN_VOICE(dev, MIDI_KEY_PRESSURE, chn, note, pressure)

' /*
'  * Midi channel messages
'  */

' #define _CHN_COMMON(dev, event, chn, p1, p2, w14) \
' 					{_SEQ_NEEDBUF(8);\
' 					_seqbuf[_seqbufptr] = EV_CHN_COMMON;\
' 					_seqbuf[_seqbufptr+1] = (dev);\
' 					_seqbuf[_seqbufptr+2] = (event);\
' 					_seqbuf[_seqbufptr+3] = (chn);\
' 					_seqbuf[_seqbufptr+4] = (p1);\
' 					_seqbuf[_seqbufptr+5] = (p2);\
' 					*(short *)&_seqbuf[_seqbufptr+6] = (w14);\
' 					_SEQ_ADVBUF(8);}
' /*
'  * SEQ_SYSEX permits sending of sysex messages. (It may look that it permits
'  * sending any MIDI bytes but it's absolutely not possible. Trying to do
'  * so _will_ cause problems with MPU401 intelligent mode).
'  *
'  * Sysex messages are sent in blocks of 1 to 6 bytes. Longer messages must be 
'  * sent by calling SEQ_SYSEX() several times (there must be no other events
'  * between them). First sysex fragment must have 0xf0 in the first byte
'  * and the last byte (buf[len-1] of the last fragment must be 0xf7. No byte
'  * between these sysex start and end markers cannot be larger than 0x7f. Also
'  * lengths of each fragments (except the last one) must be 6.
'  *
'  * Breaking the above rules may work with some MIDI ports but is likely to
'  * cause fatal problems with some other devices (such as MPU401).
'  */
' #define SEQ_SYSEX(dev, buf, len) \
' 					{int ii, ll=(len); \
' 					 unsigned char *bufp=buf;\
' 					 if (ll>6)ll=6;\
' 					_SEQ_NEEDBUF(8);\
' 					_seqbuf[_seqbufptr] = EV_SYSEX;\
' 					_seqbuf[_seqbufptr+1] = (dev);\
' 					for(ii=0;ii<ll;ii++)\
' 					   _seqbuf[_seqbufptr+ii+2] = bufp[ii];\
' 					for(ii=ll;ii<6;ii++)\
' 					   _seqbuf[_seqbufptr+ii+2] = 0xff;\
' 					_SEQ_ADVBUF(8);}

' #define SEQ_CHN_PRESSURE(dev, chn, pressure) \
' 		_CHN_COMMON(dev, MIDI_CHN_PRESSURE, chn, pressure, 0, 0)

' #define SEQ_SET_PATCH SEQ_PGM_CHANGE
' #ifdef OSSLIB
' #   define SEQ_PGM_CHANGE(dev, chn, patch) \
' 		{OSS_patch_caching(dev, chn, patch, seqfd, _seqbuf, _seqbuflen); \
' 		 _CHN_COMMON(dev, MIDI_PGM_CHANGE, chn, patch, 0, 0);}
' #else
' #   define SEQ_PGM_CHANGE(dev, chn, patch) \
' 		_CHN_COMMON(dev, MIDI_PGM_CHANGE, chn, patch, 0, 0)
' #endif

' #define SEQ_CONTROL(dev, chn, controller, value) \
' 		_CHN_COMMON(dev, MIDI_CTL_CHANGE, chn, controller, 0, value)

' #define SEQ_BENDER(dev, chn, value) \
' 		_CHN_COMMON(dev, MIDI_PITCH_BEND, chn, 0, 0, value)


' #define SEQ_V2_X_CONTROL(dev, voice, controller, value)	{_SEQ_NEEDBUF(8);\
' 					_seqbuf[_seqbufptr] = SEQ_EXTENDED;\
' 					_seqbuf[_seqbufptr+1] = SEQ_CONTROLLER;\
' 					_seqbuf[_seqbufptr+2] = (dev);\
' 					_seqbuf[_seqbufptr+3] = (voice);\
' 					_seqbuf[_seqbufptr+4] = (controller);\
' 					_seqbuf[_seqbufptr+5] = ((value)&0xff);\
' 					_seqbuf[_seqbufptr+6] = ((value>>8)&0xff);\
' 					_seqbuf[_seqbufptr+7] = 0;\
' 					_SEQ_ADVBUF(8);}
' /*
'  * The following 5 macros are incorrectly implemented and obsolete.
'  * Use SEQ_BENDER and SEQ_CONTROL (with proper controller) instead.
'  */
' #define SEQ_PITCHBEND(dev, voice, value) SEQ_V2_X_CONTROL(dev, voice, CTRL_PITCH_BENDER, value)
' #define SEQ_BENDER_RANGE(dev, voice, value) SEQ_V2_X_CONTROL(dev, voice, CTRL_PITCH_BENDER_RANGE, value)
' #define SEQ_EXPRESSION(dev, voice, value) SEQ_CONTROL(dev, voice, CTL_EXPRESSION, value*128)
' #define SEQ_MAIN_VOLUME(dev, voice, value) SEQ_CONTROL(dev, voice, CTL_MAIN_VOLUME, (value*16383)/100)
' #define SEQ_PANNING(dev, voice, pos) SEQ_CONTROL(dev, voice, CTL_PAN, (pos+128) / 2)

' /*
'  * Timing and syncronization macros
'  */

' #define _TIMER_EVENT(ev, parm)		{_SEQ_NEEDBUF(8);\
' 				 	_seqbuf[_seqbufptr+0] = EV_TIMING; \
' 				 	_seqbuf[_seqbufptr+1] = (ev); \
' 					_seqbuf[_seqbufptr+2] = 0;\
' 					_seqbuf[_seqbufptr+3] = 0;\
' 				 	*(unsigned int *)&_seqbuf[_seqbufptr+4] = (parm); \
' 					_SEQ_ADVBUF(8);}

' #define SEQ_START_TIMER()		_TIMER_EVENT(TMR_START, 0)
' #define SEQ_STOP_TIMER()		_TIMER_EVENT(TMR_STOP, 0)
' #define SEQ_CONTINUE_TIMER()		_TIMER_EVENT(TMR_CONTINUE, 0)
' #define SEQ_WAIT_TIME(ticks)		_TIMER_EVENT(TMR_WAIT_ABS, ticks)
' #define SEQ_DELTA_TIME(ticks)		_TIMER_EVENT(TMR_WAIT_REL, ticks)
' #define SEQ_ECHO_BACK(key)		_TIMER_EVENT(TMR_ECHO, key)
' #define SEQ_SET_TEMPO(value)		_TIMER_EVENT(TMR_TEMPO, value)
' #define SEQ_SONGPOS(pos)		_TIMER_EVENT(TMR_SPP, pos)
' #define SEQ_TIME_SIGNATURE(sig)		_TIMER_EVENT(TMR_TIMESIG, sig)

' /*
'  * Local control events
'  */

' #define _LOCAL_EVENT(ev, parm)		{_SEQ_NEEDBUF(8);\
' 				 	_seqbuf[_seqbufptr+0] = EV_SEQ_LOCAL; \
' 				 	_seqbuf[_seqbufptr+1] = (ev); \
' 					_seqbuf[_seqbufptr+2] = 0;\
' 					_seqbuf[_seqbufptr+3] = 0;\
' 				 	*(unsigned int *)&_seqbuf[_seqbufptr+4] = (parm); \
' 					_SEQ_ADVBUF(8);}

' #define SEQ_PLAYAUDIO(devmask)		_LOCAL_EVENT(LOCL_STARTAUDIO, devmask)
' /*
'  * Events for the level 1 interface only 
'  */

' #define SEQ_MIDIOUT(device, byte)	{_SEQ_NEEDBUF(4);\
' 					_seqbuf[_seqbufptr] = SEQ_MIDIPUTC;\
' 					_seqbuf[_seqbufptr+1] = (byte);\
' 					_seqbuf[_seqbufptr+2] = (device);\
' 					_seqbuf[_seqbufptr+3] = 0;\
' 					_SEQ_ADVBUF(4);}

' /*
'  * Patch loading.
'  */
' #ifdef OSSLIB
' #   define SEQ_WRPATCH(patchx, len) \
' 		OSS_write_patch(seqfd, (char*)(patchx), len)
' #   define SEQ_WRPATCH2(patchx, len) \
' 		OSS_write_patch2(seqfd, (char*)(patchx), len)
' #else
' #   define SEQ_WRPATCH(patchx, len) \
' 		{if (_seqbufptr) SEQ_DUMPBUF();\
' 		 if (write(seqfd, (char*)(patchx), len)==-1) \
' 		    perror("Write patch: /dev/sequencer");}
' #   define SEQ_WRPATCH2(patchx, len) \
' 		(SEQ_DUMPBUF(), write(seqfd, (char*)(patchx), len))
' #endif

' #endif

#endif
