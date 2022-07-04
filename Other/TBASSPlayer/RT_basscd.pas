{ Original Source : BASSCD.PAS by Ian Luck
  BASSCD 2.4 Delphi API, copyright (c) 2003-2006 Ian Luck.
  Requires BASS 2.4 - available from www.un4seen.com

  See the BASSCD.CHM file for more complete documentation
}

// Modified for dynamic loading by Silhwan Hyun

Unit RT_basscd;

interface

uses windows, Dynamic_bass;

const
  // Additional error codes returned by BASS_ErrorGetCode
  BASS_ERROR_NOCD       = 12; // no CD in drive
  BASS_ERROR_CDTRACK    = 13; // invalid track number
  BASS_ERROR_NOTAUDIO   = 17; // not an audio track

 // Additional config option
  BASS_CONFIG_CD_FREEOLD        = $10200;
  BASS_CONFIG_CD_RETRY          = $10201;
  BASS_CONFIG_CD_AUTOSPEED      = $10202;
  BASS_CONFIG_CD_SKIPERROR      = $10203;

  // BASS_CD_SetInterface options
  BASS_CD_IF_AUTO               = 0;
  BASS_CD_IF_SPTI               = 1;
  BASS_CD_IF_ASPI               = 2;
  BASS_CD_IF_WIO                = 3;

  // "rwflag" read capability flags
  BASS_CD_RWFLAG_READCDR        = 1;
  BASS_CD_RWFLAG_READCDRW       = 2;
  BASS_CD_RWFLAG_READCDRW2      = 4;
  BASS_CD_RWFLAG_READDVD        = 8;
  BASS_CD_RWFLAG_READDVDR       = 16;
  BASS_CD_RWFLAG_READDVDRAM     = 32;
  BASS_CD_RWFLAG_READANALOG     = $10000;
  BASS_CD_RWFLAG_READM2F1       = $100000;
  BASS_CD_RWFLAG_READM2F2       = $200000;
  BASS_CD_RWFLAG_READMULTI      = $400000;
  BASS_CD_RWFLAG_READCDDA       = $1000000;
  BASS_CD_RWFLAG_READCDDASIA    = $2000000;
  BASS_CD_RWFLAG_READSUBCHAN    = $4000000;
  BASS_CD_RWFLAG_READSUBCHANDI  = $8000000;
  BASS_CD_RWFLAG_READISRC       = $20000000;
  BASS_CD_RWFLAG_READUPC        = $40000000;

  // additional BASS_CD_StreamCreate/File flags
  BASS_CD_SUBCHANNEL            = $200;
  BASS_CD_SUBCHANNEL_NOHW       = $400;

  // additional CD sync type
  BASS_SYNC_CD_ERROR            = 1000;
  BASS_SYNC_CD_SPEED            = 1002;

  // BASS_CD_Door actions
  BASS_CD_DOOR_CLOSE            = 0;
  BASS_CD_DOOR_OPEN             = 1;
  BASS_CD_DOOR_LOCK             = 2;
  BASS_CD_DOOR_UNLOCK           = 3;

  // BASS_CD_GetID flags
  BASS_CDID_UPC                 = 1;
  BASS_CDID_CDDB                = 2;
  BASS_CDID_CDDB2               = 3;
  BASS_CDID_TEXT                = 4;
  BASS_CDID_CDPLAYER            = 5;
  BASS_CDID_MUSICBRAINZ         = 6;
  BASS_CDID_ISRC                = $100; // + track #

  // BASS_CHANNELINFO type
  BASS_CTYPE_STREAM_CD          = $10200;


type

  BASS_CD_INFO = record
    vendor: PChar;      // manufacturer
    product: PChar;     // model
    rev: PChar;         // revision
    letter: Integer;    // drive letter
   	rwflags: DWORD;     // read/write capability flags
  	canopen: BOOL;      // BASS_CD_DOOR_OPEN/CLOSE is supported?
  	canlock: BOOL;      // BASS_CD_DOOR_LOCK/UNLOCK is supported?
  	maxspeed: DWORD;    // max read speed (KB/s)
  	cache: DWORD;       // cache size (KB)
  	cdtext: BOOL;       // can read CD-TEXT
  end;

{const
  basscddll = 'basscd.dll'; }

var
  BASS_CD_SetInterface : function(iface:DWORD): DWORD; stdcall;

 // BASS_CD_GetDriveDescription : function(drive:DWORD): PChar; stdcall;
 // BASS_CD_GetDriveLetter : function(drive:DWORD): DWORD; stdcall;
  BASS_CD_GetInfo : function(drive:DWORD; var info:BASS_CD_INFO): BOOL; stdcall;
  BASS_CD_Door : function(drive,action:DWORD): BOOL; stdcall;
  BASS_CD_DoorIsOpen : function(drive:DWORD): BOOL; stdcall;
  BASS_CD_DoorIsLocked : function(drive:DWORD): BOOL; stdcall;
  BASS_CD_IsReady : function(drive:DWORD): BOOL; stdcall;
  BASS_CD_GetTracks : function(drive:DWORD): DWORD; stdcall;
  BASS_CD_GetTrackLength : function(drive,track:DWORD): DWORD; stdcall;
  BASS_CD_GetTrackPregap : function(drive,track:DWORD): DWORD; stdcall;
  BASS_CD_GetID : function(drive,id:DWORD): PChar; stdcall;
  BASS_CD_GetSpeed : function(drive:DWORD): DWORD; stdcall;
  BASS_CD_SetSpeed : function(drive,speed:DWORD): BOOL; stdcall;
  BASS_CD_Release : function(drive:DWORD): BOOL; stdcall;

  BASS_CD_StreamCreate : function(drive,track,flags:DWORD): HSTREAM; stdcall;
  BASS_CD_StreamCreateFile : function(f:pChar; flags:DWORD): HSTREAM; stdcall;
  BASS_CD_StreamGetTrack : function(handle:HSTREAM): DWORD; stdcall;
  BASS_CD_StreamSetTrack : function(handle:HSTREAM; track:DWORD): BOOL; stdcall;

  BASS_CD_Analog_Play : function(drive,track,pos:DWORD): BOOL; stdcall;
  BASS_CD_Analog_PlayFile : function(f:pAnsiChar; pos:DWORD): DWORD; stdcall;
  BASS_CD_Analog_Stop : function(drive:DWORD): BOOL; stdcall;
  BASS_CD_Analog_IsActive : function(drive:DWORD): DWORD; stdcall;
  BASS_CD_Analog_GetPosition : function(drive:DWORD): DWORD; stdcall;

  BASSCD_Handle : Thandle = 0;

function Load_BASSCDDLL(const dllfilename : string) : boolean;
procedure Unload_BASSCDDLL;

implementation

function Load_BASSCDDLL(const dllfilename : string) : boolean;
var
   oldmode : integer;
begin
   if BASSCD_Handle <> 0 then // is it already there ?
      result := true
   else begin {go & load the dll}
   oldmode := SetErrorMode($8001);
   BASSCD_Handle := LoadLibrary(pchar(dllfilename));  // obtain the handle we want
   SetErrorMode(oldmode);
   if BASSCD_Handle <> 0 then
       begin {now we tie the functions to the VARs from above}

   @BASS_CD_SetInterface := GetProcAddress(BASSCD_Handle, 'BASS_CD_SetInterface');
 //  @BASS_CD_GetDriveDescription := GetProcAddress(BASSCD_Handle, 'BASS_CD_GetDriveDescription');
 //  @BASS_CD_GetDriveLetter := GetProcAddress(BASSCD_Handle, 'BASS_CD_GetDriveLetter');
   @BASS_CD_GetInfo := GetProcAddress(BASSCD_Handle, 'BASS_CD_GetInfo');
   @BASS_CD_Door := GetProcAddress(BASSCD_Handle, 'BASS_CD_Door');
   @BASS_CD_DoorIsOpen := GetProcAddress(BASSCD_Handle, 'BASS_CD_DoorIsOpen');
   @BASS_CD_DoorIsLocked := GetProcAddress(BASSCD_Handle, 'BASS_CD_DoorIsLocked');
   @BASS_CD_IsReady := GetProcAddress(BASSCD_Handle, 'BASS_CD_IsReady');
   @BASS_CD_GetTracks := GetProcAddress(BASSCD_Handle, 'BASS_CD_GetTracks');
   @BASS_CD_GetTrackLength := GetProcAddress(BASSCD_Handle, 'BASS_CD_GetTrackLength');
   @BASS_CD_GetTrackPregap := GetProcAddress(BASSCD_Handle, 'BASS_CD_GetTrackPregap');
   @BASS_CD_GetID := GetProcAddress(BASSCD_Handle, 'BASS_CD_GetID');
   @BASS_CD_GetSpeed := GetProcAddress(BASSCD_Handle, 'BASS_CD_GetSpeed');
   @BASS_CD_SetSpeed := GetProcAddress(BASSCD_Handle, 'BASS_CD_SetSpeed');
   @BASS_CD_Release := GetProcAddress(BASSCD_Handle, 'BASS_CD_Release');
   @BASS_CD_StreamCreate := GetProcAddress(BASSCD_Handle, 'BASS_CD_StreamCreate');
   @BASS_CD_StreamCreateFile := GetProcAddress(BASSCD_Handle, 'BASS_CD_StreamCreateFile');
   @BASS_CD_StreamGetTrack := GetProcAddress(BASSCD_Handle, 'BASS_CD_StreamGetTrack');
   @BASS_CD_StreamSetTrack := GetProcAddress(BASSCD_Handle, 'BASS_CD_StreamSetTrack');
   @BASS_CD_Analog_Play := GetProcAddress(BASSCD_Handle, 'BASS_CD_Analog_Play');
   @BASS_CD_Analog_PlayFile := GetProcAddress(BASSCD_Handle, 'BASS_CD_Analog_PlayFile');
   @BASS_CD_Analog_Stop := GetProcAddress(BASSCD_Handle, 'BASS_CD_Analog_Stop');
   @BASS_CD_Analog_IsActive := GetProcAddress(BASSCD_Handle, 'BASS_CD_Analog_IsActive');
   @BASS_CD_Analog_GetPosition := GetProcAddress(BASSCD_Handle, 'BASS_CD_Analog_GetPosition');

 // check if everything is linked in correctly
   if (@BASS_CD_SetInterface = nil) or
    //  (@BASS_CD_GetDriveLetter = nil) or
      (@BASS_CD_GetInfo = nil) or
      (@BASS_CD_Door = nil) or
      (@BASS_CD_DoorIsOpen = nil) or
      (@BASS_CD_DoorIsLocked = nil) or
      (@BASS_CD_IsReady = nil) or
      (@BASS_CD_GetTracks = nil) or
      (@BASS_CD_GetTrackLength = nil) or
      (@BASS_CD_GetTrackPregap = nil) or
      (@BASS_CD_GetID = nil) or
      (@BASS_CD_GetSpeed = nil) or
      (@BASS_CD_SetSpeed = nil) or
      (@BASS_CD_Release = nil) or
      (@BASS_CD_StreamCreate = nil) or
      (@BASS_CD_StreamCreateFile = nil) or
      (@BASS_CD_StreamGetTrack = nil) or
      (@BASS_CD_StreamSetTrack = nil) or
      (@BASS_CD_Analog_Play = nil) or
      (@BASS_CD_Analog_PlayFile = nil) or
      (@BASS_CD_Analog_Stop = nil) or
      (@BASS_CD_Analog_IsActive = nil) or
      (@BASS_CD_Analog_GetPosition = nil) then
       begin
          FreeLibrary(BASSCD_Handle);
          BASSCD_Handle := 0;
       end;
     end;
     result := (BASSCD_Handle <> 0);
   end;
end;

procedure Unload_BASSCDDLL;
begin
   if BASSCD_Handle <> 0 then
      FreeLibrary(BASSCD_Handle);

   BASSCD_Handle := 0;
end;

end.
