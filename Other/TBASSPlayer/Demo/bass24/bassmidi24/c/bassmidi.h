/*
	BASSMIDI 2.4 C/C++ header file
	Copyright (c) 2006-2010 Un4seen Developments Ltd.

	See the BASSMIDI.CHM file for more detailed documentation
*/

#ifndef BASSMIDI_H
#define BASSMIDI_H

#include "bass.h"

#if BASSVERSION!=0x204
#error conflicting BASS and BASSMIDI versions
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifndef BASSMIDIDEF
#define BASSMIDIDEF(f) WINAPI f
#endif

typedef DWORD HSOUNDFONT;	// soundfont handle

// Additional BASS_SetConfig options
#define BASS_CONFIG_MIDI_COMPACT	0x10400
#define BASS_CONFIG_MIDI_VOICES		0x10401
#define BASS_CONFIG_MIDI_AUTOFONT	0x10402

// Additional BASS_SetConfigPtr options
#define BASS_CONFIG_MIDI_DEFFONT	0x10403

// Additional sync types
#define BASS_SYNC_MIDI_MARKER	0x10000
#define BASS_SYNC_MIDI_CUE		0x10001
#define BASS_SYNC_MIDI_LYRIC	0x10002
#define BASS_SYNC_MIDI_TEXT		0x10003
#define BASS_SYNC_MIDI_EVENT	0x10004
#define BASS_SYNC_MIDI_TICK		0x10005
#define BASS_SYNC_MIDI_TIMESIG	0x10006
#define BASS_SYNC_MIDI_KEYSIG	0x10007

// Additional BASS_MIDI_StreamCreateFile/etc flags
#define BASS_MIDI_DECAYEND		0x1000
#define BASS_MIDI_NOFX			0x2000
#define BASS_MIDI_DECAYSEEK		0x4000

typedef struct {
	HSOUNDFONT font;	// soundfont
	int preset;			// preset number (-1=all)
	int bank;
} BASS_MIDI_FONT;

typedef struct {
	const char *name;
	const char *copyright;
	const char *comment;
	DWORD presets;		// number of presets/instruments
	DWORD samsize;		// total size (in bytes) of the sample data
	DWORD samload;		// amount of sample data currently loaded
	DWORD samtype;		// sample format (CTYPE) if packed
} BASS_MIDI_FONTINFO;

typedef struct {
	DWORD track;		// track containing marker
	DWORD pos;			// marker position (bytes)
	const char *text;	// marker text
} BASS_MIDI_MARK;

// Marker types
#define BASS_MIDI_MARK_MARKER	0	// marker events
#define BASS_MIDI_MARK_CUE		1	// cue events
#define BASS_MIDI_MARK_LYRIC	2	// lyric events
#define BASS_MIDI_MARK_TEXT		3	// text events
#define BASS_MIDI_MARK_TIMESIG	4	// time signature
#define BASS_MIDI_MARK_KEYSIG	5	// key signature

// MIDI events
#define MIDI_EVENT_NOTE				1
#define MIDI_EVENT_PROGRAM			2
#define MIDI_EVENT_CHANPRES			3
#define MIDI_EVENT_PITCH			4
#define MIDI_EVENT_PITCHRANGE		5
#define MIDI_EVENT_DRUMS			6
#define MIDI_EVENT_FINETUNE			7
#define MIDI_EVENT_COARSETUNE		8
#define MIDI_EVENT_MASTERVOL		9
#define MIDI_EVENT_BANK				10
#define MIDI_EVENT_MODULATION		11
#define MIDI_EVENT_VOLUME			12
#define MIDI_EVENT_PAN				13
#define MIDI_EVENT_EXPRESSION		14
#define MIDI_EVENT_SUSTAIN			15
#define MIDI_EVENT_SOUNDOFF			16
#define MIDI_EVENT_RESET			17
#define MIDI_EVENT_NOTESOFF			18
#define MIDI_EVENT_PORTAMENTO		19
#define MIDI_EVENT_PORTATIME		20
#define MIDI_EVENT_PORTANOTE		21
#define MIDI_EVENT_MODE				22
#define MIDI_EVENT_REVERB			23
#define MIDI_EVENT_CHORUS			24
#define MIDI_EVENT_CUTOFF			25
#define MIDI_EVENT_RESONANCE		26
#define MIDI_EVENT_RELEASE			27
#define MIDI_EVENT_ATTACK			28
#define MIDI_EVENT_REVERB_MACRO		30
#define MIDI_EVENT_CHORUS_MACRO		31
#define MIDI_EVENT_REVERB_TIME		32
#define MIDI_EVENT_REVERB_DELAY		33
#define MIDI_EVENT_REVERB_LOCUTOFF	34
#define MIDI_EVENT_REVERB_HICUTOFF	35
#define MIDI_EVENT_REVERB_LEVEL		36
#define MIDI_EVENT_CHORUS_DELAY		37
#define MIDI_EVENT_CHORUS_DEPTH		38
#define MIDI_EVENT_CHORUS_RATE		39
#define MIDI_EVENT_CHORUS_FEEDBACK	40
#define MIDI_EVENT_CHORUS_LEVEL		41
#define MIDI_EVENT_CHORUS_REVERB	42
#define MIDI_EVENT_DRUM_FINETUNE	50
#define MIDI_EVENT_DRUM_COARSETUNE	51
#define MIDI_EVENT_DRUM_PAN			52
#define MIDI_EVENT_DRUM_REVERB		53
#define MIDI_EVENT_DRUM_CHORUS		54
#define MIDI_EVENT_DRUM_CUTOFF		55
#define MIDI_EVENT_DRUM_RESONANCE	56
#define MIDI_EVENT_DRUM_LEVEL		57
#define MIDI_EVENT_TEMPO			62
#define MIDI_EVENT_MIXLEVEL			0x10000
#define MIDI_EVENT_TRANSPOSE		0x10001

typedef struct {
	DWORD event;		// MIDI_EVENT_xxx
	DWORD param;
	DWORD chan;
	DWORD tick;			// event position (ticks)
	DWORD pos;			// event position (bytes)
} BASS_MIDI_EVENT;

// BASS_CHANNELINFO type
#define BASS_CTYPE_STREAM_MIDI	0x10d00

// Additional attributes
#define BASS_ATTRIB_MIDI_PPQN		0x12000
#define BASS_ATTRIB_MIDI_TRACK_VOL	0x12100 // + track #

// Additional tag type
#define BASS_TAG_MIDI_TRACK	0x11000	// + track #, track text : array of null-terminated ANSI strings

// BASS_ChannelGetLength/GetPosition/SetPosition mode
#define BASS_POS_MIDI_TICK		2		// tick position

HSTREAM BASSMIDIDEF(BASS_MIDI_StreamCreate)(DWORD channels, DWORD flags, DWORD freq);
HSTREAM BASSMIDIDEF(BASS_MIDI_StreamCreateFile)(BOOL mem, const void *file, QWORD offset, QWORD length, DWORD flags, DWORD freq);
HSTREAM BASSMIDIDEF(BASS_MIDI_StreamCreateURL)(const char *url, DWORD offset, DWORD flags, DOWNLOADPROC *proc, void *user, DWORD freq);
HSTREAM BASSMIDIDEF(BASS_MIDI_StreamCreateFileUser)(DWORD system, DWORD flags, const BASS_FILEPROCS *procs, void *user, DWORD freq);
BOOL BASSMIDIDEF(BASS_MIDI_StreamGetMark)(HSTREAM handle, DWORD type, DWORD index, BASS_MIDI_MARK *mark);
BOOL BASSMIDIDEF(BASS_MIDI_StreamSetFonts)(HSTREAM handle, const BASS_MIDI_FONT *fonts, DWORD count);
DWORD BASSMIDIDEF(BASS_MIDI_StreamGetFonts)(HSTREAM handle, BASS_MIDI_FONT *fonts, DWORD count);
BOOL BASSMIDIDEF(BASS_MIDI_StreamLoadSamples)(HSTREAM handle);
BOOL BASSMIDIDEF(BASS_MIDI_StreamEvent)(HSTREAM handle, DWORD chan, DWORD event, DWORD param);
DWORD BASSMIDIDEF(BASS_MIDI_StreamGetEvent)(HSTREAM handle, DWORD chan, DWORD event);
DWORD BASSMIDIDEF(BASS_MIDI_StreamGetEvents)(HSTREAM handle, DWORD track, DWORD filter, BASS_MIDI_EVENT *events);
HSTREAM BASSMIDIDEF(BASS_MIDI_StreamGetChannel)(HSTREAM handle, DWORD chan);

HSOUNDFONT BASSMIDIDEF(BASS_MIDI_FontInit)(const void *file, DWORD flags);
BOOL BASSMIDIDEF(BASS_MIDI_FontFree)(HSOUNDFONT handle);
BOOL BASSMIDIDEF(BASS_MIDI_FontGetInfo)(HSOUNDFONT handle, BASS_MIDI_FONTINFO *info);
const char *BASSMIDIDEF(BASS_MIDI_FontGetPreset)(HSOUNDFONT handle, int preset, int bank);
BOOL BASSMIDIDEF(BASS_MIDI_FontLoad)(HSOUNDFONT handle, int preset, int bank);
BOOL BASSMIDIDEF(BASS_MIDI_FontCompact)(HSOUNDFONT handle);
BOOL BASSMIDIDEF(BASS_MIDI_FontPack)(HSOUNDFONT handle, const void *outfile, const void *encoder, DWORD flags);
BOOL BASSMIDIDEF(BASS_MIDI_FontUnpack)(HSOUNDFONT handle, const void *outfile, DWORD flags);
BOOL BASSMIDIDEF(BASS_MIDI_FontSetVolume)(HSOUNDFONT handle, float volume);
float BASSMIDIDEF(BASS_MIDI_FontGetVolume)(HSOUNDFONT handle);

#ifdef __cplusplus
}
#endif

#endif
