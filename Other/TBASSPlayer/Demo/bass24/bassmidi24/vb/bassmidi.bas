Attribute VB_Name = "BASSMIDI"
' BASSMIDI 2.4 Visual Basic module
' Copyright (c) 2006-2010 Un4seen Developments Ltd.
'
' See the BASSMIDI.CHM file for more detailed documentation

' Additional BASS_SetConfig options
Global Const BASS_CONFIG_MIDI_COMPACT = &H10400
Global Const BASS_CONFIG_MIDI_VOICES = &H10401
Global Const BASS_CONFIG_MIDI_AUTOFONT = &H10402

' Additional BASS_SetConfigPtr options
Global Const BASS_CONFIG_MIDI_DEFFONT = &H10403

' Additional sync types
Global Const BASS_SYNC_MIDI_MARKER = &H10000
Global Const BASS_SYNC_MIDI_CUE = &H10001
Global Const BASS_SYNC_MIDI_LYRIC = &H10002
Global Const BASS_SYNC_MIDI_TEXT = &H10003
Global Const BASS_SYNC_MIDI_EVENT = &H10004
Global Const BASS_SYNC_MIDI_TICK = &H10005
Global Const BASS_SYNC_MIDI_TIMESIG = &H10006
Global Const BASS_SYNC_MIDI_KEYSIG = &H10007

' Additional BASS_MIDI_StreamCreateFile/etc flags
Global Const BASS_MIDI_DECAYEND = &H1000
Global Const BASS_MIDI_NOFX = &H2000
Global Const BASS_MIDI_DECAYSEEK = &H4000

Type BASS_MIDI_FONT
    font As Long            ' soundfont
    preset As Long          ' preset number (-1=all)
    bank As Long
End Type

Type BASS_MIDI_FONTINFO
    name As Long
    copyright As Long
    comment As Long
    presets As Long         ' number of presets/instruments
    samsize As Long         ' total size (in bytes) of the sample data
    samload As Long         ' amount of sample data currently loaded
    samtype As Long         ' sample format (CTYPE) if packed
End Type

Type BASS_MIDI_MARK
    track As Long           ' track containing marker
    pos As Long             ' marker position
    text As Long            ' marker text
End Type

' Marker types
Global Const BASS_MIDI_MARK_MARKER = 0  ' marker events
Global Const BASS_MIDI_MARK_CUE = 1     ' cue events
Global Const BASS_MIDI_MARK_LYRIC = 2   ' lyric events
Global Const BASS_MIDI_MARK_TEXT = 3    ' text events
Global Const BASS_MIDI_MARK_TIMESIG = 4 ' time signature
Global Const BASS_MIDI_MARK_KEYSIG = 5  ' key signature

' MIDI events
Global Const MIDI_EVENT_NOTE = 1
Global Const MIDI_EVENT_PROGRAM = 2
Global Const MIDI_EVENT_CHANPRES = 3
Global Const MIDI_EVENT_PITCH = 4
Global Const MIDI_EVENT_PITCHRANGE = 5
Global Const MIDI_EVENT_DRUMS = 6
Global Const MIDI_EVENT_FINETUNE = 7
Global Const MIDI_EVENT_COARSETUNE = 8
Global Const MIDI_EVENT_MASTERVOL = 9
Global Const MIDI_EVENT_BANK = 10
Global Const MIDI_EVENT_MODULATION = 11
Global Const MIDI_EVENT_VOLUME = 12
Global Const MIDI_EVENT_PAN = 13
Global Const MIDI_EVENT_EXPRESSION = 14
Global Const MIDI_EVENT_SUSTAIN = 15
Global Const MIDI_EVENT_SOUNDOFF = 16
Global Const MIDI_EVENT_RESET = 17
Global Const MIDI_EVENT_NOTESOFF = 18
Global Const MIDI_EVENT_PORTAMENTO = 19
Global Const MIDI_EVENT_PORTATIME = 20
Global Const MIDI_EVENT_PORTANOTE = 21
Global Const MIDI_EVENT_MODE = 22
Global Const MIDI_EVENT_REVERB = 23
Global Const MIDI_EVENT_CHORUS = 24
Global Const MIDI_EVENT_CUTOFF = 25
Global Const MIDI_EVENT_RESONANCE = 26
Global Const MIDI_EVENT_RELEASE = 27
Global Const MIDI_EVENT_ATTACK = 28
Global Const MIDI_EVENT_REVERB_MACRO = 30
Global Const MIDI_EVENT_CHORUS_MACRO = 31
Global Const MIDI_EVENT_REVERB_TIME = 32
Global Const MIDI_EVENT_REVERB_DELAY = 33
Global Const MIDI_EVENT_REVERB_LOCUTOFF = 34
Global Const MIDI_EVENT_REVERB_HICUTOFF = 35
Global Const MIDI_EVENT_REVERB_LEVEL = 36
Global Const MIDI_EVENT_CHORUS_DELAY = 37
Global Const MIDI_EVENT_CHORUS_DEPTH = 38
Global Const MIDI_EVENT_CHORUS_RATE = 39
Global Const MIDI_EVENT_CHORUS_FEEDBACK = 40
Global Const MIDI_EVENT_CHORUS_LEVEL = 41
Global Const MIDI_EVENT_CHORUS_REVERB = 42
Global Const MIDI_EVENT_DRUM_FINETUNE = 50
Global Const MIDI_EVENT_DRUM_COARSETUNE = 51
Global Const MIDI_EVENT_DRUM_PAN = 52
Global Const MIDI_EVENT_DRUM_REVERB = 53
Global Const MIDI_EVENT_DRUM_CHORUS = 54
Global Const MIDI_EVENT_DRUM_CUTOFF = 55
Global Const MIDI_EVENT_DRUM_RESONANCE = 56
Global Const MIDI_EVENT_DRUM_LEVEL = 57
Global Const MIDI_EVENT_TEMPO = 62
Global Const MIDI_EVENT_MIXLEVEL = &H10000
Global Const MIDI_EVENT_TRANSPOSE = &H10001

Type BASS_MIDI_EVENT
	event_ As Long          ' MIDI_EVENT_xxx
	param As Long
	chan As Long
	tick As Long            ' event position (ticks)
	pos As Long             ' event position (bytes)
End Type

' BASS_CHANNELINFO type
Global Const BASS_CTYPE_STREAM_MIDI = &H10D00

' Additional attributes
Global Const BASS_ATTRIB_MIDI_PPQN = &H12000
Global Const BASS_ATTRIB_MIDI_TRACK_VOL = &H12100 ' + track #

' Additional BASS_ChannelGetTags type
Global Const BASS_TAG_MIDI_TRACK = &H11000 ' + track #, track text : array of null-terminated ANSI strings

' BASS_ChannelGetLength/GetPosition/SetPosition mode
Global Const BASS_POS_MIDI_TICK = 2 ' tick position

Declare Function BASS_MIDI_StreamCreate Lib "bassmidi.dll" (ByVal channels As Long, ByVal flags As Long, ByVal freq As Long) As Long
Declare Function BASS_MIDI_StreamCreateFile64 Lib "bassmidi.dll" Alias "BASS_MIDI_StreamCreateFile" (ByVal mem As Long, ByVal file As Any, ByVal offset As Long, ByVal offsethi As Long, ByVal length As Long, ByVal lengthhi As Long, ByVal flags As Long, ByVal freq As Long) As Long
Declare Function BASS_MIDI_StreamCreateURL Lib "bassmidi.dll" (ByVal url As String, ByVal offset As Long, ByVal flags As Long, ByVal proc As Long, ByVal user As Long, ByVal freq As Long) As Long
Declare Function BASS_MIDI_StreamCreateFileUser Lib "bassmidi.dll" (ByVal system As Long, ByVal flags As Long, ByVal procs As Long, ByVal user As Long, ByVal freq As Long) As Long
Declare Function BASS_MIDI_StreamGetMark Lib "bassmidi.dll" (ByVal handle As Long, ByVal type_ As Long, ByVal index As Long, marks As Any) As Long
Declare Function BASS_MIDI_StreamSetFonts Lib "bassmidi.dll" (ByVal handle As Long, ByRef fonts As BASS_MIDI_FONT, ByVal count As Long) As Long
Declare Function BASS_MIDI_StreamGetFonts Lib "bassmidi.dll" (ByVal handle As Long, fonts As Any, ByVal count As Long) As Long
Declare Function BASS_MIDI_StreamLoadSamples Lib "bassmidi.dll" (ByVal handle As Long) As Long
Declare Function BASS_MIDI_StreamEvent Lib "bassmidi.dll" (ByVal handle As Long, ByVal chan As Long, ByVal event_ As Long, ByVal param As Long) As Long
Declare Function BASS_MIDI_StreamGetEvent Lib "bassmidi.dll" (ByVal handle As Long, ByVal chan As Long, ByVal event_ As Long) As Long
Declare Function BASS_MIDI_StreamGetEvents Lib "bassmidi.dll" (ByVal handle As Long, ByVal chan As Long, ByVal filter As Long, events As Any) As Long
Declare Function BASS_MIDI_StreamGetChannel Lib "bassmidi.dll" (ByVal handle As Long, ByVal chan As Long) As Long

Declare Function BASS_MIDI_FontInit Lib "bassmidi.dll" (ByVal file As Any, ByVal flags As Long) As Long
Declare Function BASS_MIDI_FontFree Lib "bassmidi.dll" (ByVal handle As Long) As Long
Declare Function BASS_MIDI_FontGetInfo Lib "bassmidi.dll" (ByVal handle As Long, ByRef info As BASS_MIDI_FONTINFO) As Long
Declare Function BASS_MIDI_FontGetPreset Lib "bassmidi.dll" (ByVal handle As Long, ByVal preset As Long, ByVal bank As Long) As Long
Declare Function BASS_MIDI_FontLoad Lib "bassmidi.dll" (ByVal handle As Long, ByVal preset As Long, ByVal bank As Long) As Long
Declare Function BASS_MIDI_FontCompact Lib "bassmidi.dll" (ByVal handle As Long) As Long
Declare Function BASS_MIDI_FontPack Lib "bassmidi.dll" (ByVal handle As Long, ByVal outfile As String, ByVal encoder As String, ByVal flags As Long) As Long
Declare Function BASS_MIDI_FontUnpack Lib "bassmidi.dll" (ByVal handle As Long, ByVal outfile As String, ByVal flags As Long) As Long
Declare Function BASS_MIDI_FontSetVolume Lib "bassmidi.dll" (ByVal handle As Long, ByVal handle As Single) As Long
Declare Function BASS_MIDI_FontGetVolume Lib "bassmidi.dll" (ByVal handle As Long) As Single

' 32-bit wrappers for 64-bit BASS functions
Function BASS_MIDI_StreamCreateFile(ByVal mem As Long, ByVal file As Long, ByVal offset As Long, ByVal length As Long, ByVal flags As Long, ByVal freq As Long) As Long
BASS_MIDI_StreamCreateFile = BASS_MIDI_StreamCreateFile64(mem, file, offset, 0, length, 0, flags Or BASS_UNICODE, freq)
End Function
