{ Original Source : BASSMIDI.PAS by Ian Luck
  BASSMIDI 2.4 Delphi API, copyright (c) 2002-2006 Ian Luck.
  Requires BASS 2.4 - available from www.un4seen.com

  See the BASSMIDI.CHM file for more complete documentation
}

// Modified for dynamic loading by Silhwan Hyun  (22 Apr 2009)

unit RT_bassmidi;

interface

uses Windows, Dynamic_Bass;

const
  // Additional config options
  BASS_CONFIG_MIDI_COMPACT   = $10400;
  BASS_CONFIG_MIDI_VOICES    = $10401;
  BASS_CONFIG_MIDI_AUTOFONT  = $10402;

  // Additional sync types
  BASS_SYNC_MIDI_MARKER      = $10000;
  BASS_SYNC_MIDI_CUE         = $10001;
  BASS_SYNC_MIDI_LYRIC       = $10002;
  BASS_SYNC_MIDI_TEXT        = $10003;
  BASS_SYNC_MIDI_EVENT       = $10004;
  BASS_SYNC_MIDI_TICK        = $10005;

  // additional BASS_MIDI_StreamCreateFile/etc flags
  BASS_MIDI_DECAYEND         = $1000;
  BASS_MIDI_NOFX             = $2000;

  // Marker types
  BASS_MIDI_MARK_MARKER      = 0; // marker events
  BASS_MIDI_MARK_CUE         = 1; // cue events
  BASS_MIDI_MARK_LYRIC       = 2; // lyric events
  BASS_MIDI_MARK_TEXT        = 3; // text events

  // MIDI events
  MIDI_EVENT_NOTE            = 1;
  MIDI_EVENT_PROGRAM         = 2;
  MIDI_EVENT_CHANPRES        = 3;
  MIDI_EVENT_PITCH           = 4;
  MIDI_EVENT_PITCHRANGE      = 5;
  MIDI_EVENT_DRUMS           = 6;
  MIDI_EVENT_BANK            = 10;
  MIDI_EVENT_MODULATION      = 11;
  MIDI_EVENT_VOLUME          = 12;
  MIDI_EVENT_PAN             = 13;
  MIDI_EVENT_EXPRESSION      = 14;
  MIDI_EVENT_SUSTAIN         = 15;
  MIDI_EVENT_SOUNDOFF        = 16;
  MIDI_EVENT_RESET           = 17;
  MIDI_EVENT_NOTESOFF        = 18;
  MIDI_EVENT_PORTAMENTO      = 19;
  MIDI_EVENT_PORTATIME       = 20;
  MIDI_EVENT_PORTANOTE       = 21;
  MIDI_EVENT_MODE            = 22;
  MIDI_EVENT_REVERB          = 23;
  MIDI_EVENT_CHORUS          = 24;
  MIDI_EVENT_CUTOFF          = 25;
  MIDI_EVENT_RESONANCE       = 26;
  MIDI_EVENT_REVERB_MACRO    = 30;
  MIDI_EVENT_CHORUS_MACRO    = 31;
  MIDI_EVENT_REVERB_TIME     = 32;
  MIDI_EVENT_REVERB_DELAY    = 33;
  MIDI_EVENT_REVERB_LOCUTOFF = 34;
  MIDI_EVENT_REVERB_HICUTOFF = 35;
  MIDI_EVENT_REVERB_LEVEL    = 36;
  MIDI_EVENT_CHORUS_DELAY    = 37;
  MIDI_EVENT_CHORUS_DEPTH    = 38;
  MIDI_EVENT_CHORUS_RATE     = 39;
  MIDI_EVENT_CHORUS_FEEDBACK = 40;
  MIDI_EVENT_CHORUS_LEVEL    = 41;
  MIDI_EVENT_CHORUS_REVERB   = 42;
  MIDI_EVENT_DRUM_FINETUNE   = 50;
  MIDI_EVENT_DRUM_COARSETUNE = 51;
  MIDI_EVENT_DRUM_PAN        = 52;
  MIDI_EVENT_DRUM_REVERB     = 53;
  MIDI_EVENT_DRUM_CHORUS     = 54;
  MIDI_EVENT_DRUM_CUTOFF     = 55;
  MIDI_EVENT_DRUM_RESONANCE  = 56;
  MIDI_EVENT_TEMPO           = 62;
  MIDI_EVENT_MIXLEVEL        = $10000;
  MIDI_EVENT_TRANSPOSE       = $10001;

  // BASS_CHANNELINFO type
  BASS_CTYPE_STREAM_MIDI     = $10d00;

  // Additional tag type
  BASS_TAG_MIDI_TRACK        = $11000; // + track #, track text : array of null-terminated ANSI strings

  // BASS_ChannelGetLength/GetPosition/SetPosition mode
  BASS_POS_MIDI_TICK         = 2; // tick position

type
  HSOUNDFONT = DWORD;   // soundfont handle

  BASS_MIDI_FONT = record
    font: HSOUNDFONT;   // soundfont
    preset: LongInt;    // preset number (-1=all)
	  bank: Longint;
  end;

  BASS_MIDI_FONTINFO = record
	  name: PAnsiChar;
	  copyright: PAnsiChar;
	  comment: PAnsiChar;
	  presets: DWORD;     // number of presets/instruments
	  samsize: DWORD;     // total size (in bytes) of the sample data
	  samload: DWORD;     // amount of sample data currently loaded
    samtype: DWORD;     // sample format (CTYPE) if packed
  end;

  BASS_MIDI_MARK = record
    track: DWORD;       // track containing marker
	  pos: DWORD;         // marker position
	  text: PAnsiChar;        // marker text
  end;

const
  bassmididll = 'bassmidi.dll';

var
  BASS_MIDI_StreamCreate : function(flags,freq:DWORD): HSTREAM; stdcall;
  BASS_MIDI_StreamCreateFile : function(mem:BOOL; fl:pointer; offset,length:QWORD; flags,freq:DWORD): HSTREAM; stdcall;
  BASS_MIDI_StreamCreateURL : function(URL:PAnsiChar; offset:DWORD; flags:DWORD; proc:DOWNLOADPROC; user:Pointer; freq:DWORD): HSTREAM; stdcall;
//  BASS_MIDI_StreamCreateFileUser : function(buffered:BOOL; flags:DWORD; proc:STREAMFILEPROC; user,freq:DWORD): HSTREAM; stdcall;
  BASS_MIDI_StreamCreateFileUser : function(system,flags:DWORD; var procs:BASS_FILEPROCS; user:Pointer; freq:DWORD): HSTREAM; stdcall;
//  BASS_MIDI_StreamGetMarks : function(handle:HSTREAM; mtype:DWORD; var fonts:BASS_MIDI_MARK; count:DWORD): DWORD; stdcall;
  BASS_MIDI_StreamGetMark : function(handle:HSTREAM; type_,index:DWORD; var mark:BASS_MIDI_MARK): BOOL; stdcall;
  BASS_MIDI_StreamSetFonts : function(handle:HSTREAM; var fonts:BASS_MIDI_FONT; count:DWORD): BOOL; stdcall;
  BASS_MIDI_StreamGetFonts : function(handle:HSTREAM; var fonts:BASS_MIDI_FONT; count:DWORD): DWORD; stdcall;
  BASS_MIDI_StreamLoadSamples : function(handle:HSTREAM): BOOL; stdcall;
  BASS_MIDI_StreamEvent : function(handle:HSTREAM; chan,event,param:DWORD): BOOL; stdcall;
  BASS_MIDI_StreamGetEvent : function(handle:HSTREAM; chan,event:DWORD): DWORD; stdcall;

  BASS_MIDI_FontInit : function(fname:PChar; flags:DWORD): HSOUNDFONT; stdcall;
  BASS_MIDI_FontFree : function(handle:HSOUNDFONT): BOOL; stdcall;
  BASS_MIDI_FontGetInfo : function(handle:HSOUNDFONT; var info:BASS_MIDI_FONTINFO): BOOL; stdcall;
  BASS_MIDI_FontGetPreset : function(handle:HSOUNDFONT; preset,bank:LongInt): PAnsiChar; stdcall;
  BASS_MIDI_FontCompact : function(handle:HSOUNDFONT): BOOL; stdcall;
  BASS_MIDI_FontPack : function(handle:HSOUNDFONT; outfile,encoder:PAnsiChar; flags:DWORD): BOOL; stdcall;
  BASS_MIDI_FontUnpack : function(handle:HSOUNDFONT; outfile:PAnsiChar; flags:DWORD): BOOL; stdcall;
  BASS_MIDI_FontSetVolume : function(handle:HSOUNDFONT; volume:Single): BOOL; stdcall;
  BASS_MIDI_FontGetVolume : function(handle:HSOUNDFONT): Single; stdcall;

  BASSMIDI_Handle : Thandle = 0;

function Load_BASSMIDIDLL(const dllfilename : string) : boolean;
procedure Unload_BASSMIDIDLL;

implementation

function Load_BASSMIDIDLL(const dllfilename : string) : boolean;
var
   oldmode : integer;
begin
   if BASSMIDI_Handle <> 0 then // is it already there ?
      result := true
   else begin {go & load the dll}
   oldmode := SetErrorMode($8001);
   BASSMIDI_Handle := LoadLibrary(pchar(dllfilename));  // obtain the handle we want
   SetErrorMode(oldmode);

   if BASSMIDI_Handle <> 0 then
       begin {now we tie the functions to the VARs from above}

   @BASS_MIDI_StreamCreate := GetProcAddress(BASSMIDI_Handle, 'BASS_MIDI_StreamCreate');
   @BASS_MIDI_StreamCreateFile := GetProcAddress(BASSMIDI_Handle, 'BASS_MIDI_StreamCreateFile');
   @BASS_MIDI_StreamCreateURL := GetProcAddress(BASSMIDI_Handle, 'BASS_MIDI_StreamCreateURL');
   @BASS_MIDI_StreamCreateFileUser := GetProcAddress(BASSMIDI_Handle, 'BASS_MIDI_StreamCreateFileUser');
   @BASS_MIDI_StreamGetMark := GetProcAddress(BASSMIDI_Handle, 'BASS_MIDI_StreamGetMark');
   @BASS_MIDI_StreamSetFonts := GetProcAddress(BASSMIDI_Handle, 'BASS_MIDI_StreamSetFonts');
   @BASS_MIDI_StreamGetFonts := GetProcAddress(BASSMIDI_Handle, 'BASS_MIDI_StreamGetFonts');
   @BASS_MIDI_StreamLoadSamples := GetProcAddress(BASSMIDI_Handle, 'BASS_MIDI_StreamLoadSamples');
   @BASS_MIDI_StreamEvent := GetProcAddress(BASSMIDI_Handle, 'BASS_MIDI_StreamEvent');
   @BASS_MIDI_StreamGetEvent := GetProcAddress(BASSMIDI_Handle, 'BASS_MIDI_StreamGetEvent');

   @BASS_MIDI_FontInit := GetProcAddress(BASSMIDI_Handle, 'BASS_MIDI_FontInit');
   @BASS_MIDI_FontFree := GetProcAddress(BASSMIDI_Handle, 'BASS_MIDI_FontFree');
   @BASS_MIDI_FontGetInfo := GetProcAddress(BASSMIDI_Handle, 'BASS_MIDI_FontGetInfo');
   @BASS_MIDI_FontGetPreset := GetProcAddress(BASSMIDI_Handle,  'BASS_MIDI_FontGetPreset');
   @BASS_MIDI_FontCompact := GetProcAddress(BASSMIDI_Handle, 'BASS_MIDI_FontCompact');
   @BASS_MIDI_FontPack := GetProcAddress(BASSMIDI_Handle, 'BASS_MIDI_FontPack');
   @BASS_MIDI_FontUnpack := GetProcAddress(BASSMIDI_Handle, 'BASS_MIDI_FontUnpack');
   @BASS_MIDI_FontSetVolume := GetProcAddress(BASSMIDI_Handle, 'BASS_MIDI_FontSetVolume');
   @BASS_MIDI_FontGetVolume := GetProcAddress(BASSMIDI_Handle, 'BASS_MIDI_FontGetVolume');

 // check if everything is linked in correctly
   if (@BASS_MIDI_StreamCreate = nil) or
      (@BASS_MIDI_StreamCreateFile = nil) or
      (@BASS_MIDI_StreamCreateURL = nil) or
      (@BASS_MIDI_StreamCreateFileUser = nil) or
      (@BASS_MIDI_StreamGetMark = nil) or
      (@BASS_MIDI_StreamSetFonts = nil) or
      (@BASS_MIDI_StreamGetFonts = nil) or
      (@BASS_MIDI_StreamLoadSamples = nil) or
      (@BASS_MIDI_StreamEvent = nil) or
      (@BASS_MIDI_StreamGetEvent = nil) or
      (@BASS_MIDI_FontInit = nil) or
      (@BASS_MIDI_FontFree = nil) or
      (@BASS_MIDI_FontGetInfo = nil) or
      (@BASS_MIDI_FontGetPreset = nil) or
      (@BASS_MIDI_FontCompact = nil) or
      (@BASS_MIDI_FontPack = nil) or
      (@BASS_MIDI_FontUnpack = nil) or
      (@BASS_MIDI_FontSetVolume = nil) or
      (@BASS_MIDI_FontGetVolume = nil) then
       begin
          FreeLibrary(BASSMIDI_Handle);
          BASSMIDI_Handle := 0;
       end;

     end;

     result := (BASSMIDI_Handle <> 0);
   end;
end;

procedure Unload_BASSMIDIDLL;
begin
   if BASSMIDI_Handle <> 0 then
      FreeLibrary(BASSMIDI_Handle);

   BASSMIDI_Handle := 0;
end;

end.
