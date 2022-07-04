// unit PluginCtrl
//
// This unit takes charge of followings for TBASSPlayer
//  - managing Winamp plug-ins such as loading, unloading and launching plug-ins.
//  - interfacing Winamp input plug-in to BASS sound system.
//  - controlling BASS channels.
// BASS sound sytem acts like an output plug-in when the opened stream is being
//  decoded by Winamp input plug-in in this unit.
// TBASSPlayer can play any types of stream files if you provide appropriate Winamp
//  input plug-ins.
//
//
//       written by Silhwan Hyun  (hyunsh@hanafos.com)
//
// (vis plug-in = Winamp visualization plug-in)
// (vis window = the window used for visualization created by vis plug-in)
// (EMBED window = the window which serves vis plug-in with its client area for visualization)
//

//  Modified IPC message handling function WindowProc to support Winamp GPP.
//
// Ver 1.5                   20 Apr 2009
//   - Modified for Delphi 2009
//
// Ver 1.44                   1 Oct 2008
//  Renamed function (for VisDrawer.dll) names to differentiate them from the function's for
//   native Winamp GPP.
//    StartGPPModule -> RunVisDrawerModule
//    SetGPPInactive -> StopVisDrawerModule
//    GPPActive -> VisDrawerActive
//  Fixed following problems by modifing function omodWrite2
//   - out of sync at playing net radio (~.ASX)
//   - crash at freeing in-use BASS add-on
//  Changed the global procedure WinProcessMessages to local procedure because I am not sure
//   whether the procedure is safe at multi thread environment.
//   -> defined procedure WinProcessMessages per thread (or per unit) basis.
//  Changed function omodGetOutputTime, omodGetWrittenTime and GetPlayedTime to prevent floating
//   point exception.
//  Added an item "Format" in TStreamInfo, to put the information of stream format.
//  New message constants
//    - WM_ChannelUnavailable : to notify that Decode channel is unavailable.
//    - PlayListChange : to support Play List
//    - CheckTitleBar : to adjust the highlighted state of EMBED window
//
// (supplement at Ver 1.44.1)
//  Modified function WindowProc to drive multiple Winamp GPPs simultaneously.
//
// (supplement at Ver 1.44.2)
//  Added a procedure SetLoadingGPP
//
// (supplement at Ver 1.44.3)
//  Removed functions/procedures related driving vis plug-in & vis drawer(-> moved to GPPDrive.pas).
//  Added procedure SetRotateParams
//
//  (supplement at Ver 1.44.4)
//  Added a message constant : WM_SlideEnded
//
// Ver 1.43                      15 Jul 2008
//  Added a function GetBufferAddress which renders the address of data buffer for rendering
//   visualization data
//  Modified function StartGPPModule to set initial vis window's size & position
//  Added a procedure SetChannelInfo2
//  Added a enum type : TPluginRequest
//  Added record types : TVisWindowIs
//  Modified & Renamed a record type : TVisDriveThreadInfo -> TVisPluginInfo
//  Added constants for the driver of vis plug-in : WM_RequestFromVis, UnloadVisPlugin
//
// Ver 1.42                       8 Dec 2006
//  Added functions and a procedure to support Winamp-like visualization window using the
//  customized general purpose plugin, 'Gen_VisDrawer.dll'.
//   function StartGPPModule, GPPActive
//   procedure SetGPPInactive
//  Added message constants : WM_GetLyric, WM_GetHTTPHeaders
//
// Ver 1.41                       25 Oct 2006
//   Some trivial changes such as comments, explanations
//
// Ver 1.40                        27 Feb 2005
//  Added 8 procedures/functions to support "dual channel mode".
//  Added function ActivePlugin to get active input plug-in's name.
//  Modified function SelectInputPlugin to enable to set the state indicating 'none of input
//   plug-ins is in use'.
//  Modified DSP plug-in related functions to prohibit duplicate running DSP plug-in.
//  Added function DSPBufferReady to inform you if DSP plug-in is applicable.
//  Removed function GetMsgHandler.
//  Removed function GetMainHandle.
//  Removed internal message handler. ( Internal messages are handled by main message handler
//   in BASSPlayer.pas to eliminate timing problems. )
//  Removed procedure SetMainHandle.
//  Added procedure SetReachedEnd to set the variable ReachedEnd at external routine.
//
// Ver 1.32                        7 Feb 2005
//  Modified some functions to adjust buffer (to hold decoded sound data) size if previously
//   allocated amount is out of reasonable range.
//  (Required to handle multi channel streams)
//  Added some functions to enhance stability at runnun DSP plug-ins.
//  Removed procedure SetSongDuration.
//
// Ver 1.31                       14 Dec 2004
//  Made some minor modifications for new BASS version (2.0 -> 2.1)
//
// Ver 1.3                         7 Feb 2004
//   Added procedure SetSongDuration
//   Added function IsWinampPluginLoaded
//
// Ver 1.2                        15 Dec 2003
//   Added procedures used by VisoutIntf.pas to correct errors at ending visualization
//   Added functions and a procedure for DSP plug-in
//   Added function GetPluginNumber to get the number of plug-in for specified file type
//
// Ver 1.1                        12 May 2003
//   Modified a procedure LoadVisModule (-> added a parameter "ParentHandle" )
//
// Ver 1.0                        30 Jan 2003
//   - Initial release
//

unit PluginCtrl;

// {$DEFINE DEBUG}

interface

{$INCLUDE Delphi_Ver.inc}

uses
   Windows, SysUtils, Messages, Forms, ioplug, Dynamic_BASS, RT_BASSWMA, wa_ipc, Graphics, UniCodeUtils;

const
   MaxPluginNum = 8;                  // the maximum number of plug-ins, simultaneously loadable

   WM_WA_MPEG_EOF = WM_USER + 2;      // message from Winamp input plug-in, sent when stream reaches the end
   WM_StartPlay   = WM_USER + 101;    // message from output plug-in emulator at starting playback
   WM_BASS_StreamCreate = WM_USER + 105; // message from output plug-in emulator at executing BASS_StreamCreate
//   WM_BASS_StreamFree = WM_USER + 106;   // message from output plug-in emulator at executing BASS_StreamFree
   WM_GetToEnd    = WM_USER + 107;    // message to notify that BASS reaches the end of a stream
   WM_NewFFTData  = WM_USER + 108;    // message to notify that new FFT data are ready
   WM_PluginFirst_Changed = WM_USER + 109; // message from PluginConfigForm(=Input plug-in Configuration Form)

   WM_GetMeta     = WM_USER + 110;    // message to notify that new Shoutcast Metadata are ready
   WM_DownLoaded  = WM_USER + 111;    // message to notify that downloading of an stream file from internet is done.
   WM_GetChannelInfo = WM_USER + 112; // message from Winamp input plug-in at getting stream file's properties (sampling rate, ..)
   WM_GetChannelData = WM_USER + 114; // message for repeated sample data extraction from BASS decoding channel.
   WM_RequestFromVis = WM_USER + 115; // message to notify that a request from Vis plug-in received
 //  WM_PlayListConfig = WM_USER + 116;  // defined in unit PlayListConfig.pas
   WM_ChannelUnavailable = WM_USER + 120;  // message from the PlayThread (Decode channel is not available)
   WM_GetLyric = WM_USER + 121;       // message to notify that a lyric event is encountered
   WM_GetHTTPHeaders = WM_USER + 122; // message to notify that BASS got the HTTP header
   WM_SlideEnded  = WM_USER + 123;    // message to notify that an attribute slide has ended   // * Added at Ver 1.44.4

   MaxVolume = 255;          // Winamp input plug-in volume range : 0 ~ 255
   ChannelLimit = 8;           // Allows maximum 8 channels in a stream

 // error types related loading & unloading of Winamp input plug-ins
   ERROR_OMOD_NOTREADY  = 1;   // output plug-in emulator is not ready
   ERROR_LOADED_BEFORE  = 2;   // already loaded before
   ERROR_LOADED_FULL    = 3;   // no space to load
   ERROR_CANNOT_LOAD    = 4;   // uncertain error at loading
   ERROR_INVALID_PLUGIN = 5;   // not a Winamp input plug-in
   ERROR_CANNOT_UNLOAD  = 6;   // uncertain error at unloading
   ERROR_NOT_LOADED     = 7;   // not loaded yet
   ERROR_IN_USE         = 8;   // specified plug-in is in use

   maxVismodNum = 8;
   maxDSPmodNum = 8;

 // constants for the communication between threads related to driving vis plug-in
   DataReady = 10;
   QuitProg = 20;
 //  PlayerStatus = 30;
   MinimizeWindow = 40;
   ChangeEmbedWindow = 45;
   ChangeEMBEDSwitchMode = 46;
 //  CheckTitleBar = 47;     // * New at Ver 1.44
   RestoreWindow = 50;
   VisModuleLoaded = 55;
   UnloadVisPlugin = 56;
   RequestRestFlag = 57;
   PlayListChange  = 58;    // * New at Ver 1.44
   StartVisOut = 60;
   PauseVisOut = 65;
   UseVisDrawerForm = 66;
   EmbedWindowChanged = 68;
   EndVisOut = 70;

   InformPlayerMode = 81;
   InformStreamInfo = 82;
 //  InformSyncWindows = 83;

   Run_GPP = 91;          // * New at Ver 1.44
   Run_AllGPPs = 92;      // * New at Ver 1.44
   Stop_GPP = 93;         // * New at Ver 1.44
   Stop_AllGPPs = 94;     // * New at Ver 1.44

   MBShowUp = 95;         // * New at Ver 1.44
   MBClosed = 96;         // * New at Ver 1.44
   GenShowClose = 97;     // * New at Ver 1.44.2

   CLASSNAME_WINAMP : pChar = 'Winamp v1.x';
   CLASSNAME_VIS_DRAWER = 'Vis Drawer';

type
   TChannelType = (Channel_NotOpened, Channel_Stream, Channel_CD, Channel_WMA,
                   Channel_Music, Channel_MIDI, Channel_Plugin);
   TPluginRequest = (REQ_VOLUMEUP, REQ_VOLUMEDOWN, REQ_FFWD5S, REQ_REW5S,
                     REQ_PREV, REQ_NEXT, REQ_PLAY, REQ_PAUSE, REQ_STOP);  

   TPlugin = ^TIn_module;
   TPluginInfo = record
      Name : string;
      Version : integer;        // module type (IN_VER)
      Description : string;     // description of module, with version string
      FileExtensions : string;
      Seekable : integer;
      DLLHandle : THandle;
   end;
   TChannelInfo = record
      BitsPerSample : word;     // Bits per sample
      BitRate   : LongInt;
      SampleRate : LongInt;
      Channels : Word;
   end;
   PChannelInfo = ^TChannelInfo;

   TVismod = array[0..maxVismodNum-1] of PWinAmpVisModule;
   TDSPmod = array[0..maxDSPmodNum-1] of PWinAmpDSPModule;
   TVisModuleInfo = record
     latencyMs : Cardinal;
     delayMs : Cardinal;
     spectrumNch : Cardinal;
     waveformNch : Cardinal;
   end;


 // TVisWindowIs defines the type of vis window
   TVisWindowIs = (UnAvailable, CreatedByPlugin, OnCreatedByGPP,
                                OnCreatedByCode, OnAssignedWindow);
     // CreatedByPlugin : vis window and EMBED window(if exist) is created by plug-in in use.
     // OnCreatedByGPP : vis window is on the EMBED window created by VisDrawer.DLL ( EMBED window is
     //                  a Winamp-like shaped, seperate window )
     // OnCreatedByCode : vis window is on the EMBED window created by interal code of this unit. ( EMBED
     //                  window is a normal shaped, seperate window )
     // OnAssignedWindow : vis window is on the EMBED window created by interal code of this unit and
     //                    EMBED window is created on the user specified window (ex. panel of main form).
     //                  I named the display mode by this type of vis window "Panel Mode".

 // TVisEMBEDSwitchMode defines the method of switching EMBED window for running vis plug-in.
   TVisEMBEDSwitchMode = (NewStart, WindowMove);     
     // NewStart : vis plug-in is quitted first, then restarted with newly specified EMBED window.
     // WindowMove : vis window is transfered to newly specified EMBED window.

   TVisPluginInfo = record
     ThreadId : DWORD;       // Thread identifier
     VisHandle : DWORD;      // Window handle to vis window
     VisType : TVisWindowIs; // Type of vis window
     PluginPath : pchar;     // File path to the vis plug-in
     ModNo   : word;         // Number of the selected module in the vis plug-in
     StartType : boolean;    // True : VisType entry has the starting display mode
   end;

   TSupportedBy = (Both, BASSNative, WinampPlugin, None);
   TPlayerMode = (plmStandby, plmReady, plmStopped, plmPlaying, plmPaused);
   TStreamInfo = record
     FileName : string;
     FileSize : DWORD;        // File size in byte
     SampleRate : DWORD;      // Sampling rate in Hz
     BitRate : DWORD;         // Bit Rate in KBPS
     BitsPerSample : Word;    // Bits per sample
     Duration : DWORD;        // playback duration in mili second
     Channels : Word;         // 1- Mono, 2 - Stereo
     Format   : DWORD;        // Stream format   // * Added at Ver 1.44
     Title : string;
     Artist : string;
     Album : string;
     Year : string;
     Genre : string;
     GenreID : byte;
     Track : byte;
     Comment : string;
  end;

  TGenDrawerInfo = record
     FormNo    : integer;
     SW_FLAG   : integer;
     DLLPath   : pchar;  // Path of the DLL requested to create this EMBED window
     WinHandle : HWND;
  end;

  TInitWinPos = record    // Defines initial parameters for vis window
    pos_x : integer;
    pos_y : integer;
    width : integer;
    height : integer;
    close_action : integer;
 // close_action  -> 0 : hide window, 1 : destroy window, 2 : determined in user application program
 //  if close_action = 2, WM_SYSCOMMAND (LParam = SC_CLOSE, WParam = mouse location) message is posted
 //  to the window created by GPP module at receiving mouse click on "CLOSE" button area of title bar.
 //  We should sets a new address for the window procedure to catch the WM_SYSCOMMAND message using
 //  SetWindowLong function.
 //  ex. OrgWindowProc := Pointer(SetWindowLong(WindowHandle, GWL_WNDPROC, LongInt(@NewWindowProc));
  end;

  TSubClass_Proc = function(lng_hWnd: HWND; uMsg: Integer;
                            var Msg: TMessage; var bHandled: Boolean) : boolean;

// Input & output plug-in related functions
  function  OutputReady : boolean;
  procedure SetReachedEnd;
  function  SelectInputPlugin(PluginNum : integer) : integer;
  function  ActivePlugin : string;
  procedure SetPosChanged;
  function  GetOutputTime(omodTime : boolean) : integer;
  function  GetPluginIndex(PluginName : string) : integer;
  function  GetPlugin(PluginNum : integer) : TPlugin;
  function  GetPluginInfo(PluginNum : integer) : TPluginInfo;
  function  IsWinampPluginLoaded(PluginName : string) : boolean;
  function  LoadWinampPlugin(PluginName : string) : integer;
  function  UnloadWinampPlugin(PluginName : string) : integer;
  function  GetPlayableFiles : string;
  function  GetPluginNumber(ExtCode : string) : integer;

// Following 8 procedures/functions are to control BASS channel.
//  procedure SetFlangeParams;
  procedure SetRotateParams; // ** New at Ver Ver 1.44.3
//  procedure Flange(handle: HSYNC; channel: DWORD; buffer: Pointer; length, user: DWORD); stdcall;
  procedure RotateProc(handle: HDSP; Channel: DWORD; buffer: Pointer; length:
                      DWORD; user: DWORD); stdcall;  // ** New at Ver Ver 1.44.3
  function  omodWrite2 : integer;
  function  omodOpen2(S_Channel, D_Channel : DWORD;   // * Changed at Ver 1.44
                      samplerate, numchannels, bitspersamp : integer;
                      IsNetRadio : boolean) : DWORD;
  procedure omodClose2;
  procedure ClearBuffer;
  function  DataInBuffer : DWORD;
  function  GetPlayedTime : DWORD;

// Visualization plug-in related procedure/function (only the loading and unloading functions
//  to show vis modules in a vis plug-in, as to user can select a module to launch.)
  procedure LoadVisModule(PluginPath : string;
                          var Visheader : PWinampVisHeader;
                          var Vismod : TVismod;
                          var NumVismod : integer;
                          ParentHandle : HWND);
  function  UnloadVisModule : integer;

  function  GetBufferAddress : pointer;

// DSP plug-in related functions
  function  DSPBufferReady : boolean;
  function  DSPActive : boolean;
  procedure LoadDSPModule(PluginPath : string;
                          var DSPheader : PWinampDSPHeader;
                          var DSPmod : TDSPmod;
                          var NumDSPmod : integer{;
                          ParentHandle : HWND});
  function  StartDSPModule(ModuleNum : word;
                          ParentHandle : HWND) : integer;
  function  StopDSPModule : integer;
  function  UnloadDSPModule : integer;


  procedure SetStreamInfo2(Stream : TStreamInfo);
  procedure SetPlayerMode2(Mode : TPlayerMode);

  procedure SetChannelInfo2(ChannelType : TChannelType; D_ChannelId, P_ChannelId,
                                        SampleRate, Channels : DWORD);

// General Purpose Plug-in related functions
  procedure SetLoadingGPP(GPP_Path : string);   // * Added at Ver 1.44.2

// Commonly used functions
  function  GetProgDir : string;
  function  GetFakeWinHandle : HWND;    // * Added at Ver 1.44
  function  InitPluginCtrl(MainHandle, ParentHandle, MiniBrowser, DataReadyMsg : HWND) : boolean;
  procedure QuitPluginCtrl;


implementation

uses WasabiAPI, PlayListUtils, GPPDrive;

var
   pBuf : PBYTE;
   dummyBuf : PBYTE;
   bufSize : DWORD = 0;
   DSPBuffer : PBYTE;
   DSPBufferSize : DWORD = 0;
   omod : ^TOut_module;
   imod : ^TIn_module;
   omod1 : TOut_module;
   imodNum   : integer;
   omodReady : boolean = false;
   Plugin : array[0..MaxPluginNum-1] of TPlugin;
   PluginInfo : array[0..MaxPluginNum-1] of TPluginInfo;

   HBASSStream : HSTREAM = 0;
   ChannelInfo : TChannelInfo;
   PosChanged : boolean;

   PlayThreadId : DWORD = 0;
   CloseRequested : boolean;
   PlayThreadStarted : boolean;
   PlayChannel : HSTREAM = 0;
   SourceChannel : DWORD;
   DecodeChannel : DWORD;

 //  NeedData : boolean = false;
   totalGet : int64;

   CurVismod  : TVismod;
   VismodNum : integer = 0;
   VismodIndex : integer;
   VisIsActive : boolean = false;
   VisualizationQuitted : boolean = false;
   hVisOutWindow : HWND = 0; // handle to vis window

   CurDSPmod  : TDSPmod;
   DSPmodNum : integer = 0;
   DSPmodIndex : integer;
   DSPIsActive : boolean = false;

   VisDrawerIsReady : boolean = false;

   hMainMsg      : HWND;     // handle to main message procedure in BASSPlayer.pas
   hParentHandle : HWND;     // handle to parent of instance of TBASSPlayer
                             //  (= Main window of program)
   hFakeWindow : HWND = 0;   // handle to fake Winamp window
   hMBWindow   : HWND = 0;   // handle to Mini Browser window

   PlayerMode : TPlayerMode;
   StreamInfo : TStreamInfo;

   WinampClassRegistered : boolean = false;

 // * Added for function SetChannelInfo2 & PlaybackPosition
   FChannelType : TChannelType;
   FD_ChannelId : DWORD;
   FP_ChannelId : DWORD;
   FSampleRate : DWORD;
   FChannels : DWORD;

   DataBuffer : array[0..2857] of byte;
   PlayThreadHandle : HWND;

   NetRadio : boolean;
   IsStreamClosing : boolean;

   DataMsgId : HWND;
   OrgWndProc : pointer;

   LoadingGPP : string;

 //  Genex_ : TBitmap;

procedure WinProcessMessages;
// Allow Windows to process other system messages
var
    ProcMsg  :  TMsg;
begin
    while PeekMessage(ProcMsg, 0, 0, 0, PM_REMOVE) do begin
      if (ProcMsg.Message = WM_QUIT) then Exit;
      TranslateMessage(ProcMsg);
      DispatchMessage(ProcMsg);
    end;
end;

procedure ShowErrorMsgBox(ErrorStr : string);
begin
   Application.MessageBox(PChar(ErrorStr), 'Error', MB_OK + MB_ICONERROR);
end;

function PlaybackPosition : DWORD;
var
   SongPos : int64;
   FloatPos : FLOAT;
begin
   case FChannelType of
      Channel_NotOpened : SongPos := 0;
      Channel_Stream..Channel_MIDI : begin
                          if (FD_ChannelId = FP_ChannelId) then  // Single channel mode ?
                             SongPos := BASS_ChannelGetPosition(FD_ChannelId, BASS_POS_BYTE)
                          else
                             SongPos := BASS_ChannelGetPosition(FD_ChannelId, BASS_POS_BYTE) - DataInBuffer;
                       end;
      Channel_Plugin : begin
                          if PlayerMode = plmReady then
                             result := 0
                      // Output plug-in's time returns actual elapsed time based on the amount
                      // of sample data output, i.e., not the playback position in a stream
                      // beging played. So we may get different result if we run DSP plug-ins which
                      // returns less or more samples than the amount of source samples.
                         { else if UsesOutputPlugin then
                             result := GetOutputTime(true)  // Output plug-in's time  }
                          else
                             result := GetOutputTime(false); // Input plug-in's time
                          exit;
                       end;
      else
         SongPos := 0;
   end;

   if SongPos <= 0 then
   begin
      result := 0;
      exit;
   end;

   FloatPos := BASS_ChannelBytes2Seconds(FD_ChannelId, SongPos);
   result := round(1000 * FloatPos);     // sec -> milli sec
end;

//---------- procedures & functions for Winamp input plug-in --------------------

procedure SetInfo1(bitrate, srate, stereo, synched : integer); cdecl; // if -1, changes ignored? :)
begin
   if (bitrate = -1) and (srate = -1) and (stereo = -1) then
      exit;

   if bitrate <> -1 then
      ChannelInfo.BitRate := bitrate;
   if srate <> -1 then
      ChannelInfo.SampleRate := srate;
   if stereo <> -1 then
      ChannelInfo.Channels := stereo;

   PostMessage(hMainMsg, WM_GetChannelInfo, 0, longint(@ChannelInfo));
end;

function dsp_isactive1 : integer; cdecl;
begin
   if DSPIsActive then
      result := 1
   else
      result := 0;
end;

function dsp_dosamples1(samples : pointer; numsamples, bps, nch, srate : integer) : integer; cdecl;
var
  rtnsamples : integer;
begin
   result := numsamples;

   if DSPIsActive then
   begin
      rtnsamples := CurDSPmod[DSPmodIndex].ModifySamples(CurDSPmod[DSPmodIndex], samples, numsamples, bps, nch, srate);
      if rtnsamples > 0 then
         result := rtnsamples;
   end;
end;

procedure SAVSAInit1(maxlatency_in_ms : integer; srate : integer); cdecl;  // call in omod.Play()
begin
end;

procedure SAVSADeInit1;cdecl;	// call in omod.Stop()
begin
end;

procedure SAAddPCMData1(PCMData: pointer; nch: integer; bps: integer; timestamp: integer); cdecl;
begin
end;

// gets csa (the current type (4=ws,2=osc,1=spec))
function SAGetMode1: integer; cdecl;
begin
   result := 0;
end;

// sets the spec data, filled in by winamp
procedure SAAdd1(data: pointer; timestamp: integer; csa: integer); cdecl;
begin
end;

// sets the vis data directly from PCM data
procedure VSAAddPCMData1(PCMData: pointer; nch: integer; bps: integer; timestamp: integer); cdecl;
begin
end;

// use to figure out what to give to VSAAdd
function VSAGetMode1(var specNch : integer; var waveNch : integer) : integer; cdecl;
begin
   result := 0;
end;

// filled in by winamp, called by plug-in
procedure VSAAdd1(data : pointer; timestamp : integer); cdecl;
begin
end;

procedure VSASetInfo1(srate : integer; nch : integer); cdecl;
begin
end;


//---------- procedures & functions to emulate Winamp output plug-in -----------

const
   PacketSize = 1152;
   WinampBaseSize = 576;  // Winamp input plug-in's base size of data transfering
   DefaultBufSize = 88200; // buffer size to hold 0.5sec playing data for 44100Hz, 16bit, stereo stream

var
   rOffset : dword;
   wOffset : dword;
   SPS : integer;       // samplerate (samples per second)
   BPS : integer;       // bits per sample
   Channels : integer;  // number of channels (MONO = 1, STEREO = 2)
   BASSDataSize : dword;
   isReading : boolean;
   isWriting : boolean;
   posDelta : integer;
   totalWritten : int64;
   InitialBufferFill : boolean;
   ReachedEnd : boolean;

   ResumeMode : boolean;


// calulate free space of buffer (The buffer pointed by pBuf to hold sound data
// is used as ring buffer)
function FreeSpace(BufferSize, ReadOffset, WriteOffset : dword) : dword;
begin
   if ReadOffset > WriteOffset then
      result := ReadOffset - WriteOffset
   else
      result := BufferSize  - WriteOffset + ReadOffset;
end;

// calculate the amount of data in buffer
function DataRemains(BufferSize, ReadOffset, WriteOffset : dword) : dword;
begin
   if ReadOffset > WriteOffset then
      result := BufferSize  + WriteOffset - ReadOffset
   else
      result := WriteOffset - ReadOffset;
end;

// Message handlers are merged into one message handler in BASSPlayer.pas
//  to prevent timing problems.
{ procedure TMsgHandler.ProcMessage(var Msg: TMessage);
begin
   case Msg.Msg of
      WM_StreamOpen : begin
                         hMainHandle := Msg.wParam;  // main window's message handle
                         imodNum := Msg.lParam;      // index number of Winamp input plug-in to use
                      end;
 //     WM_StreamClose : imodNum := -1;   // -1 : No plug-ins is in use
 //     WM_PosChange  : PosChanged := true;
      WM_StartPlay : begin
                        BASS_ChannelPlay(HBASSStream, false);
                        ResumeMode := false;
                        if hMainHandle <> 0 then
                           PostMessage(hMainHandle, WM_StartPlay, Msg.wParam, Msg.lParam);
                     end;
      WM_WA_MPEG_EOF : begin
                          ReachedEnd := true;
                          if hMainHandle <> 0 then
                             PostMessage(hMainHandle, WM_WA_MPEG_EOF, 0, 0);
                       end;
      WM_GetChannelData : omodWrite2;
      WM_QueryEndSession : Msg.Result := 1;    // Allow system termination
   end;
end; }


// Stream writing callback function for BASS
function StreamWriter(handle : HSTREAM;
                      buffer : pointer;
                      length : DWORD;
                      user : DWORD) : DWORD; stdcall;
// The value of 'length'(data transfering size) is as follows for some cases
//  SPS : 48000, BPS : 16, Channels : 2 -> 38400
//  SPS : 44100, BPS : 16, Channels : 2 -> 35280
//  SPS : 22050, BPS : 8,  Channels : 1 -> 4410
var
   p, p2 : pointer;
   dw : dword;
   wCycle : word;
   OutputTime : DWORD;
   
begin
 // Let BASS wait until sound data is ready.
 // This waiting routine is not necessary in most cases.
 // This routine is added for the Winamp input plug-in "in_midi.dll" (v2.64,
 // written by Mr. Peter Pawlowski) which takes long time to get sound data after
 // changing play position.
   if not ReachedEnd then
      if DataRemains(bufSize, rOffset, wOffset) < length then
      begin
      // Waiting cycle is allowed only when playback position is not at the near end
      // of a stream file.
      // I have found that in_speex.dll v0.7.5a sounds out echo(= repeats playback of
      // internal buffer) at the near end of a stream file. ( I think it is caused by
      // delayed message-out of WM_WA_MPEG_EOF message from in_speex.dll )
         OutputTime := posDelta + round(1000 * totalWritten / (SPS * (BPS SHR 3) * Channels));
         if (StreamInfo.Duration > OutputTime) and   // OutputTime may be greater than StreamInfo.Duration
            ((StreamInfo.Duration - OutputTime) > 2000) then
         begin
            wCycle := 0;
            while DataRemains(bufSize, rOffset, wOffset) < length do
            begin
               WinProcessMessages;
               Sleep(50);
               inc(wCycle);
               if wCycle = 60 then     // 3 sec time out
                  break;
               if ReachedEnd then
                  break;
            end;
            ResumeMode := false;
         end;
      end;

   if isWriting then  // avoid duplicate access on buffer
   begin
      wCycle := 0;
      repeat
         WinProcessMessages;
         sleep(20);
         inc(wCycle);
      until (not isWriting) or (wCycle = 150);
   {$IFDEF DEBUG}
      if isWriting then
         ShowErrorMsgBox('isWriting is not cleared at function StreamWriter');
    {$ENDIF}
   end;

   isReading := true;  // notify that BASS is reading buffer data
   dw := dword(pBuf) + rOffset;
   p := pointer(dw);   // p : starting address to read

   if rOffset > wOffset then
      if (bufSize - rOffset) > length then
      begin
         Move(p^, buffer^, length);
         inc(rOffset, length);
         result := length;
      end
      else if (bufSize - rOffset) = length then
      begin
         Move(p^, buffer^, bufSize - rOffset);
         rOffset := 0;
         result := length;
      end else // (bufSize - rOffset) < length
      begin
         Move(p^, buffer^, bufSize - rOffset);
         dw := dword(buffer) + bufSize - rOffset;
         p2 := pointer(dw);
         if (length - (bufSize - rOffset)) < wOffset then
         begin
            Move(pBuf^, p2^, length - (bufSize - rOffset));
            rOffset := length - (bufSize - rOffset);
            result := length;
         end else
         begin
            Move(pBuf^, p2^, wOffset);
            rOffset := wOffset;
            result := bufSize - rOffset + wOffset {+ BASS_STREAMPROC_END};
         end;
      end
   else if rOffset < wOffset then
      if (wOffset - rOffset) >= length then
      begin
         Move(p^, buffer^, length);
         inc(rOffset, length);
         result := length;
      end else
      begin
         Move(p^, buffer^, wOffset - rOffset);
      // signify that the end of the stream is reached
         result := wOffset - rOffset {+ BASS_STREAMPROC_END};
         rOffset := wOffset;
      end
   else   // rOffset = wOffset : no data in buffer
      result := BASS_STREAMPROC_END{0};

 // Let BASS wait until sound data is ready for long latency devices such CDROM
 // (for the delay time drive's spindle motor gets normal speed after stopping)
   if ResumeMode then
      if DataRemains(bufSize, rOffset, wOffset) < length then
      begin
         wCycle := 0;
         while not isWriting do  // until data is written
         begin
            WinProcessMessages;
            Sleep(200);
            inc(wCycle);
            if wCycle = 50 then  // 10 sec time out
                break;
         end;
         ResumeMode := false;
      end;

   isReading := false;
end;


procedure omodConfig(hwndParent : hwnd); cdecl; // configuration dialog
begin
   Application.MessageBox('No configuration is needed.', 'Confirm', MB_OK);
end;

procedure omodAbout(hwndParent : hwnd); cdecl;  // about dialog
begin
   Application.MessageBox('This is a Winamp output plug-in emulator using BASS',
                          'Confirm', MB_OK);
end;

procedure omodInit; cdecl;     // called when loaded
begin
   if omodReady then
      exit;

   omodReady := true;
end;

procedure omodQuit; cdecl;    // called when unloaded
begin
   if omodReady then
      omodReady := false;

end;

function omodOpen(samplerate, numchannels, bitspersamp, bufferlenms,
                                           prebufferms : integer) : integer; cdecl;
// returns >=0 on success, <0 on failure
// called by input plug-in just before starting playback
var
   flags : DWORD;
begin
   if not omodReady then
   begin
      result := -1;
      exit;
   end;

   if bitspersamp = 8 then
      flags := BASS_SAMPLE_8BITS
   else
      flags := 0;

   HBASSStream := BASS_StreamCreate(samplerate,
                                    numchannels,
                                    flags,
                                    @StreamWriter,
                                    nil {pointer to user data});
   if HBASSStream = 0 then   // 0 : error
      result := -1
   else
   begin
      SPS := samplerate;
      BPS := bitspersamp;
      Channels := numchannels;
  // BASSDataSize : The data size of each transfering to BASS (= amount to play 0.2sec period)
      BASSDataSize := (SPS * (BPS div 8) * CHANNELS * 2) div 10;

  // Readjust buffer size if previously allocated amount is out of reasonable range.
      if ((BASSDataSize * 2) > bufSize) or ((BASSDataSize * 4) < bufSize) then
      begin
         if (pBuf <> nil) then
            FreeMem(pBuf);
         pBuf := nil;
         bufSize := 0;
         GetMem(pBuf, BASSDataSize * 3); // Get memory to hold sound data from Winamp input plug-in
         if (pBuf <> nil) then
            bufSize := BASSDataSize * 3
         else begin
            result := -1;
            exit;
         end;
      end;

      posDelta := 0;
      if hMainMsg <> 0 then
         PostMessage(hMainMsg, WM_BASS_StreamCreate, integer(HBASSStream), 0);
      result := 0;
   end;

   rOffset := 0;
   wOffset := 0;
   totalWritten := 0;
   isReading := false;
   isWriting := false;
   PosChanged := false;
   InitialBufferFill := true;
   ReachedEnd := false;
end;

procedure omodClose; cdecl;   // close the ol' output device.
begin
   if BASS_ChannelIsActive(HBASSStream) = BASS_ACTIVE_PLAYING then
      BASS_ChannelStop(HBASSStream);
   BASS_StreamFree(HBASSStream);
  // if not ReachedEnd then
    {  if hMainHandle <> 0 then
         PostMessage(hMainHandle, WM_BASS_StreamFree, 0, 0); }
   HBASSStream := 0;
end;

function omodWrite(buf : pointer; len : integer) : integer; cdecl;
// 0 on success. Len == bytes to write (<= 8192 always).
// 1 returns not able to write (yet). Non-blocking, always.

var
   dw : dword;
   p, p2 : pointer;
   wCycle : integer;
begin
   if isReading then  // avoid duplicate access on buffer
   begin
      wCycle := 0;
      repeat
         WinProcessMessages;
         sleep(20);
         inc(wCycle);
      until (not isReading) or (wCycle = 150);
   {$IFDEF DEBUG}
      if isReading then
         ShowErrorMsgBox('isReading is not cleared at function omodWrite');
    {$ENDIF}
   end;

   isWriting := true;  // notify that Winamp input plug-in is writing sound data
   dw := dword(pBuf) + wOffset;
   p := pointer(dw);   // p : starting address to write

   if FreeSpace(bufSize, rOffset, wOffset) > DWord(len) then
   begin
      if rOffset > wOffset then
      begin
         Move(buf^, p^, len);
         inc(wOffset, len);
      end
      else
         if (bufSize - wOffset) > DWord(len) then
         begin
            Move(buf^, p^, len);
            inc(wOffset, len);
         end else
         begin
            Move(buf^, p^, bufSize - wOffset);
            if (bufSize - wOffset) < DWord(len) then
            begin
               dw := dword(buf) + (bufSize - wOffset);
               p2 := pointer(dw);
               Move(p2^, pBuf^, DWord(len) - (bufSize - wOffset));
            end;
            wOffset := DWord(len) - (bufSize - wOffset);
         end;

      inc(totalWritten, len);
      result := 0;
   end else
      result := 1;      // This case may happen at using DSP plug-in

   isWriting := false;

 // Let BASS start playing after initial buffer filling
   if InitialBufferFill then
      if (FreeSpace(bufSize, rOffset, wOffset) <= 8192) or
         (DataRemains(bufSize, rOffset, wOffset) >= (BASSDataSize SHL 1)) then
      begin
         InitialBufferFill := false;

         if HBASSStream <> 0 then
         begin
            ResumeMode := false;
            BASS_ChannelPlay(HBASSStream, true);
            ChannelInfo.BitsPerSample := BPS;
            ChannelInfo.SampleRate := SPS;
            ChannelInfo.Channels := Channels;
            if hMainMsg <> 0 then
               PostMessage(hMainMsg, WM_StartPlay, 0, longint(@ChannelInfo));
         end;
      end;
end;

function omodCanWrite: integer; cdecl;
// returns number of bytes possible to write at a given time.
// Never will decrease unless you call Write (or Close, heh)
begin
   if isReading then
   begin
      result := 0;
      exit;
   end;

 // Subtract 1 from real free space to prevent wOffset become equal to rOffset
 // after writing data.  (it means there is no data in buffer if rOffset equals
 //  to wOffset)
   result := FreeSpace(bufSize, rOffset, wOffset) - 1;
end;

function omodIsPlaying : integer; cdecl;
// non0 if output is still going or if data in buffers waiting to be
// written (i.e. closing while IsPlaying() returns 1 would truncate the song
begin
   if BASS_ChannelIsActive(HBASSStream) = BASS_ACTIVE_PLAYING then
      result := 1
   else
      result := 0;
end;

function omodPause(pause : integer) : integer; cdecl;
// returns previous pause state
begin
   if BASS_ChannelIsActive(HBASSStream) = BASS_ACTIVE_PAUSED then
      result := 1
   else
      result := 0;

   if (pause = 0) and (result = 1) then
   begin
      ResumeMode := true;
      BASS_ChannelPlay(HBASSStream, false);
   end

// This case will happen in the sequence, Pause -> position change -> resume
   else if (pause = 0) then
   begin
      ResumeMode := true;
      BASS_ChannelPlay(HBASSStream, true{flush})
   end else if (pause <> 0) and (result = 0) then
      BASS_ChannelPause(HBASSStream);
end;

procedure omodSetVolume(volume : integer); cdecl; // volume is 0-255
var
   SetVolume : integer;
begin
   if Volume < 0 then
      SetVolume := 0
   else if Volume > MaxVolume then
      SetVolume := MaxVolume
   else
      SetVolume := Volume;

   BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, SetVolume * 39);
end;

procedure omodSetPan(pan : integer); cdecl;       // pan is -128 to 128
begin
 //  BASS_ChannelSetAttributes(HBASSStream, -1, -1, round(pan * (100 / 128)));
   BASS_ChannelSetAttribute(HBASSStream, BASS_ATTRIB_PAN, pan * (100 / 128));
end;

procedure omodFlush(t : integer); cdecl;
// flushes buffers and restarts output at time t (in ms) (used for seeking)
// This procedure is called by input plug-in when position is changed
var
   chnStatus : dword;
   wasPlaying : boolean;
   wCycle : integer;
begin
   chnStatus := BASS_ChannelIsActive(HBASSStream);
   if (chnStatus <> BASS_ACTIVE_PAUSED) and
      (chnStatus <> BASS_ACTIVE_STOPPED) then
      if (FreeSpace(bufSize, rOffset, wOffset) > 0) or
         (chnStatus = BASS_ACTIVE_PLAYING) then
      begin
         wasPlaying := true;
      end else
         wasPlaying := false
   else
      wasPlaying := false;

 // Normally this omod.Flush procedure is called by Winamp input plug-in when
 // play position is changed but "in_midi.dll" (v2.64, written by Mr. Peter
 // Pawlowski) calls this procedure when it starts play after pause without
 // position change.
 // The variable PosChanged is used to know if the position has been changed or not.
 // Set PosChanged true in the main program if position is cahnged.
   if chnStatus = BASS_ACTIVE_PAUSED then
      if not PosChanged then  // if position has not been changed then escape
         exit;

   if isReading then  // avoid duplicate access on buffer
   begin
      wCycle := 0;
      repeat
         WinProcessMessages;
         sleep(20);
         inc(wCycle);
      until (not isReading) or (wCycle = 150);
   {$IFDEF DEBUG}
      if isReading then
         ShowErrorMsgBox('isReading is not cleared at function omodFlush');
    {$ENDIF}
   end;

 // Use BASS_ChannelStop regardless the status of BASS_Channel to flush the buffer in BASS
   BASS_ChannelStop(HBASSStream);

   if wasPlaying then
      InitialBufferFill := true;   // Reset to restart after seeking

   posDelta := t;
   rOffset := 0;
   wOffset := 0;
   totalWritten := 0;
   isWriting := false;
   PosChanged := false;
end;

// * Changed to prevent floating point exception at Ver 1.44
function omodGetOutputTime : integer; cdecl;  // returns played time in MS
var
   Devider : DWORD;
begin
   Devider := (SPS * (BPS SHR 3) * Channels);
   if Devider <> 0 then
      result := posDelta +
   //  round(1000 * BASS_ChannelGetPosition(HBASSStream) / (SPS * (BPS SHR 3) * Channels));
       round(1000 * BASS_ChannelGetPosition((HBASSStream), BASS_POS_BYTE) / Devider)
   else
      result := 0;
end;

// * Changed to prevent floating point exception at Ver 1.44
function omodGetWrittenTime : integer; cdecl;
// returns time written in MS (used for synching up vis stuff)
var
   Devider : DWORD;
begin
   Devider := (SPS * (BPS SHR 3) * Channels);
   if Devider <> 0 then
      result := posDelta + round(1000 * totalWritten / Devider)
   else
      result := 0;
end;


procedure SetOutputPlugin;
begin
   omod := @omod1;
   omod.version := $10;
   omod.description := 'BASS Winamp output plug-in emulator';
   omod.id := 65536 + 1;
   omod.hMainWindow := Application.Handle;
   omod.hDllInstance := 0;
   omod.Config := omodConfig;
   omod.About := omodAbout;
   omod.Init := omodInit;
   omod.Quit := omodQuit;
   omod.Open := omodOpen;
   omod.Close := omodClose;
   omod.Write := omodWrite;
   omod.CanWrite := omodCanWrite;
   omod.IsPlaying := omodIsPlaying;
   omod.Pause := omodPause;
   omod.SetVolume := omodSetVolume;
   omod.SetPan := omodSetPan;
   omod.Flush := omodFlush;
   omod.GetOutputTime := omodGetOutputTime;
   omod.GetWrittenTime := omodGetWrittenTime;

   omod.Init;
end;

procedure SetReachedEnd;
begin
   ReachedEnd := true;
end;


//------------------------ BASS dual channel mode support -----------------------

// Following statements for flanger effect is adopted from DTMain.pas of BASS16
// package

{const
   FLABUFLEN = 350;         // buffer length for flanger effect   }

var
   // Variables for DSP (flanger effect) implementation
 {  flabuf : array[0..FLABUFLEN-1, 0..2] of SmallInt;  // buffer
   flapos : Integer;         // cur.pos
   flas, flasinc : FLOAT;    // sweep pos/min/max/inc   }
   RotatePos : single;

{procedure SetFlangeParams;
begin
   FillChar(flabuf, SizeOf(flabuf), 0);
   flapos := 0;
   flas := FLABUFLEN / 2;
   flasinc := 0.002;
end; }

procedure SetRotateParams;
begin
   RotatePos := 0.7853981;
end;

function fmod(a, b: FLOAT): FLOAT;
begin
   Result := a - (b * Trunc(a / b));
end;

{function Clip(a: Integer): Integer;
begin
   if a <= -32768 then
      a := -32768
   else if a >= 32767 then
      a := 32767;

   Result := a;
end;


procedure Flange(handle: HSYNC; channel: DWORD; buffer: Pointer; length, user: DWORD); stdcall;
var
  lc, rc: SmallInt;
  p1, p2, s: Integer;
  d: ^DWORD;
  f: FLOAT;
begin
  d := buffer;
  while (length > 0) do
  begin
    lc := LOWORD(d^); rc := HIWORD(d^);
    p1 := (flapos + Trunc(flas)) mod FLABUFLEN;
    p2 := (p1 + 1) mod FLABUFLEN;
    f := fmod(flas, 1.0);
    s := lc + Trunc(((1.0-f) * flabuf[p1, 0]) + (f * flabuf[p2, 0]));
    flabuf[flapos, 0] := lc;
    lc := Clip(s);
    s := rc + Trunc(((1.0-f) * flabuf[p1, 1]) + (f * flabuf[p2, 1]));
    flabuf[flapos, 1] := rc;
    rc := Clip(s);
    d^ := MakeLong(lc, rc);
    Inc(d);
    Inc(flapos);
    if (flapos = FLABUFLEN) then flapos := 0;
    flas := flas + flasinc;
    if (flas < 0) or (flas > FLABUFLEN) then
      flasinc := -flasinc;
    length := length - 4;
  end;
end;  }

procedure RotateProc(handle: HDSP; Channel: DWORD; buffer: Pointer; length:
  DWORD; user: DWORD); stdcall;
var
  a: DWORD;
  d: PSmallInt;
begin
  d := buffer;
  a := 0;
  while (a < length) do
  begin
    d^ := round(d^ * Abs(Sin(RotatePos)));
    Inc(d);
    d^ := round(d^ * Abs(Cos(RotatePos)));
    RotatePos := fmod(RotatePos + 0.00003, Pi);
    Inc(d);
    a := a + 4;
  end;
end;

function omodWrite2 : integer;   // * Changed at Ver 1.44
// 0 on success.
// 1 on failure

var
   dw : dword;
   ReqSpace, RetSamples : dword;
   ReqSize, GetSize, RetSize : dword;
   p, p2 : pointer;
   ChannelStat : dword;
   wCycle : integer;
begin
   result := 1;

   if PlayChannel = 0 then
      exit;
   if ReachedEnd then
      exit;

   if isReading then  // avoid duplicate access on buffer
   begin
      wCycle := 0;
      repeat
         WinProcessMessages;
         sleep(20);
         inc(wCycle);
      until (not isReading) or (wCycle = 150);
   {$IFDEF DEBUG}
      if isReading then
         ShowErrorMsgBox('isReading is not cleared at function omodWrite2');
    {$ENDIF}
   end;

   isWriting := true;

 // determine transfering size in bytes.
   ReqSize := 576 * Channels;
   if BPS > 8 then
      ReqSize := ReqSize * 2;
   if SPS > 22050 then
      ReqSize := ReqSize * 2;
   if DSPIsActive then
      ReqSpace := ReqSize * 2  // returned samples may be up to twice than original.
   else
      ReqSpace := ReqSize;

   if FreeSpace(bufSize, rOffset, wOffset) < ReqSpace then
   begin
      isWriting := false;
      exit;
   end;

   GetSize := BASS_ChannelGetData(SourceChannel, DSPBuffer, ReqSize);

   if GetSize = DWORD(-1) then  // This case happens if the BASS add-on in-use is freed.
   begin
      ReachedEnd := true;
      isWriting := false;
      if BASS_ErrorGetCode = BASS_ERROR_HANDLE then
         if (not IsStreamClosing) then  // if it's not the time at closing
            result := -1;  // -1 : fatal error
      exit;
   end;

 // if the aqired amount of sound data is less than requested and the opend stream comes from
 // net radio, then fill with silent data. (needed to synchronize Play channel with Decode channel.)
 // ** Added at Ver 1.44
   if NetRadio and (GetSize < ReqSize) then
   begin
      dw := dword(DSPBuffer) + GetSize;
      p := pointer(dw);
      Move(dummyBuf^, p^, ReqSize - GetSize);
      GetSize := ReqSize;
   end;

   if GetSize > 0 then
   begin
      inc(totalGet, GetSize);

      if DSPIsActive then
      begin
         try
           RetSamples := CurDSPmod[DSPmodIndex].ModifySamples(CurDSPmod[DSPmodIndex],
                                      DSPBuffer, GetSize div DWord((Channels * 2)){Samples},
                                      BPS{bits per sample}, DWord(Channels), SPS);
         except
           ShowErrorMsgBox('DSP error');
           RetSamples := 0;
         end;
         RetSize := RetSamples * DWord((BPS div 8)) * DWORD(Channels);
      end else
         RetSize := GetSize;

      dw := dword(pBuf) + wOffset;
      p := pointer(dw);   // p : starting address to write

      if rOffset > wOffset then
      begin
         Move(DSPBuffer^, p^, RetSize);
         inc(wOffset, RetSize);
      end
      else
         if (bufSize - wOffset) > RetSize then
         begin
            Move(DSPBuffer^, p^, RetSize);
            inc(wOffset, RetSize);
         end else
         begin
            Move(DSPBuffer^, p^, bufSize - wOffset);
            if (bufSize - wOffset) < RetSize then
            begin
               dw := dword(DSPBuffer) + (bufSize - wOffset);
               p2 := pointer(dw);
               Move(p2^, pBuf^, RetSize - (bufSize - wOffset));
            end;
            wOffset := RetSize - (bufSize - wOffset);
         end;
   end else
   begin
      ReachedEnd := true;
      isWriting := false;
      if SourceChannel <> DecodeChannel then  // if Mixer is plugged in
      // Check if decode channeld is available
         if BASS_ChannelGetPosition(DecodeChannel, BASS_POS_BYTE) = QWORD(-1) then
            if BASS_ErrorGetCode = BASS_ERROR_HANDLE then
               if (not IsStreamClosing) then  // if it's not the time at closing
                  result := -1;  // -1 : fatal error

      exit;
   end;

   if GetSize < ReqSize then
      ReachedEnd := true
   else begin
      if PlayChannel <> 0 then
      begin
         ChannelStat := BASS_ChannelIsActive(PlayChannel);
         if InitialBufferFill or (ChannelStat = BASS_ACTIVE_PLAYING) or
                                         (ChannelStat = BASS_ACTIVE_STALLED) then
            if FreeSpace(bufSize, rOffset, wOffset) > ReqSpace then
               PostThreadMessage(PlayThreadId, WM_GetChannelData, 0, 0);
            //   NeedData := true;
      end;
      result := 0;
   end;

 // Check initial buffer filling is completed
   if InitialBufferFill then
      if (FreeSpace(bufSize, rOffset, wOffset) <= ReqSpace) or
         (DataRemains(bufSize, rOffset, wOffset) >= (BASSDataSize SHL 1)) then
         InitialBufferFill := false;

   isWriting := false;
end;

function GetResampledData(handle: HSTREAM; buf: Pointer; len, user: DWORD): DWORD; stdcall;
var
   dw : DWORD;
   p, p2 : pointer;
   wCycle : integer;
begin
   if InitialBufferFill then
   begin
      PostThreadMessage(PlayThreadId, WM_GetChannelData, 0, 0);
      repeat
         WinProcessMessages;
         Sleep(20);
         if ReachedEnd then
            break;
      until (InitialBufferFill = false);
   end;

   if isWriting then  // avoid duplicate access on buffer
   begin
      wCycle := 0;
      repeat
         WinProcessMessages;
         sleep(20);
         inc(wCycle);
      until (not isWriting) or (wCycle = 150);
   {$IFDEF DEBUG}
      if isWriting then
         ShowErrorMsgBox('isWriting is not cleared at function GetResampledData');
    {$ENDIF}
   end;

   isReading := true;  // notify that BASS is reading buffer data

   dw := dword(pBuf) + rOffset;
   p := pointer(dw);   // p : starting address to read

   if rOffset > wOffset then
      if (bufSize - rOffset) > len then
      begin
         Move(p^, buf^, len);
         inc(rOffset, len);
         result := len;
      end else   // (bufSize - rOffset) <= len
      begin
         Move(p^, buf^, bufSize - rOffset);
         if (bufSize - rOffset) < len then
         begin
            dw := dword(buf) + bufSize - rOffset;
            p2 := pointer(dw);
            if (len - (bufSize - rOffset)) < wOffset then
            begin
               Move(pBuf^, p2^, len - (bufSize - rOffset));
               rOffset := len - (bufSize - rOffset);
               result := len;
            end else
            begin
               Move(pBuf^, p2^, wOffset);
               rOffset := wOffset;
               result := bufSize - rOffset + wOffset;
            end;
         end else  // (bufSize - rOffset) = len
         begin
            rOffset := 0;
            result := len;
         end;
      end
   else if rOffset < wOffset then
      if (wOffset - rOffset) >= len then
      begin
         Move(p^, buf^, len);
         inc(rOffset, len);
         result := len;
      end else
      begin
         Move(p^, buf^, wOffset - rOffset);
         result := wOffset - rOffset;
         rOffset := wOffset;
      end
   else   // rOffset = wOffset
      result := 0;

   if result < len then
      if ReachedEnd then
         result := result + BASS_STREAMPROC_END
      else
         PostThreadMessage(PlayThreadId, WM_GetChannelData, 0, 0)
   else
      PostThreadMessage(PlayThreadId, WM_GetChannelData, 0, 0);

   isReading := false;
end;

procedure ClearBuffer;
begin
   rOffset := 0;
   wOffset := 0;
   totalGet := 0;
 // FillChar(pBuf^, bufSize, 0);
 //  NeedData := false;
   isWriting := false;
   InitialBufferFill := true;
   ReachedEnd := false;
end;

function PlayNewThread(lpParam : pointer) : DWORD; stdcall;
var
   Msg : TMsg;
   MsgReturn : longbool;
begin
   CloseRequested := false;
   PlayThreadStarted := true;
   SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_ABOVE_NORMAL);

   repeat
     { if NeedData then
      begin
         NeedData := false;
         omodWrite2;
      end else
      begin }
         MsgReturn := GetMessage(Msg, 0, 0, 0);
         if ((Msg.message = WM_QUIT) or (Msg.message = WM_CLOSE)) then
             CloseRequested := true
         else if Msg.message = WM_GetChannelData then
            if PlayChannel <> 0 then
               if not isWriting then
                  if omodWrite2 = -1 then  // -1 : Decode channel is not available.
                     PostMessage(hMainMsg, WM_ChannelUnavailable, 0, 0);

         TranslateMessage(Msg);
         DispatchMessage(Msg);
    //  end;
   until (integer(MsgReturn) <= 0) or (PlayChannel = 0) or CloseRequested;

   Result := 0;
   PlayThreadId := 0;
   ExitThread(0);
end;

function omodOpen2(S_Channel, D_Channel : DWORD;
                   samplerate, numchannels, bitspersamp : integer;
                   IsNetRadio : boolean) : DWORD;
// returns opened channel handle
var
   flags : DWORD;

begin
   result := 0;

   IsStreamClosing := false;
   NetRadio := IsNetRadio;

   if bitspersamp = 8 then
      flags := BASS_SAMPLE_8BITS
   else
      flags := 0;

   PlayChannel := BASS_StreamCreate(samplerate,
                                    numchannels,
                                    flags,
                                    @GetResampledData,
                                    nil {pointer to user data});

   if PlayChannel <> 0 then   // 0 : error
   begin
      SPS := samplerate;
      BPS := bitspersamp;
      Channels := numchannels;
  // BASSDataSize : The data size of each transfering to BASS (= amount to play 0.2sec period)
      BASSDataSize := (SPS * (BPS div 8) * CHANNELS * 2) div 10;

  // Readjust buffer size if previously allocated amount is out of reasonable range.
      if ((BASSDataSize * 2) > bufSize) or ((BASSDataSize * 4) < bufSize) then
      begin
         if (pBuf <> nil) then
            FreeMem(pBuf);
         pBuf := nil;
         bufSize := 0;
         GetMem(pBuf, BASSDataSize * 3); // Get memory to hold sound data
         if (pBuf <> nil) then
            bufSize := BASSDataSize * 3
         else begin
            BASS_StreamFree(PlayChannel);
            exit;
         end;
      end;

   // if Mixer Channel is not plugged in Decode Channel then S_Channel = D_Channel,
   //  else S_channel = Mixer Channel.
      SourceChannel := S_Channel;   // SourceChannel : Data Source of Play Channel
      DecodeChannel := D_Channel;

      ClearBuffer;
      PlayThreadStarted := false;
      PlayThreadHandle := CreateThread(nil, 0, @PlayNewThread, nil, 0, PlayThreadId);

      if PlayThreadHandle <> 0 then
      begin
        repeat
           WinProcessMessages;
           Sleep(20);
        until PlayThreadStarted;

        result := PlayChannel;
      end else begin
         BASS_StreamFree(PlayChannel);
         ShowErrorMsgBox('Unable to create Play Thread.');
      end;
   end;
end;

procedure omodClose2;
begin
   IsStreamClosing := true;

   if PlayChannel <> 0 then
   begin
      BASS_StreamFree(PlayChannel);
      PlayChannel := 0;
   end;

   if (PlayThreadId <> 0) then
   begin
      PostThreadMessage(PlayThreadId, WM_CLOSE, 0, 0);

    // Wait until VisDriveThread terminated
    //  WaitForSingleObject(PlayThreadHandle, INFINITE);

      repeat
         WinProcessMessages;
         sleep(20);
      until PlayThreadId = 0;

      CloseHandle(PlayThreadHandle);
   end;
end;

function DataInBuffer : DWORD;
begin
   result := DataRemains(bufSize, rOffset, wOffset);
end;

// GetPlayedTime will vary if some kind of DSP effect is applied because the amount of
// output sample data may be different from the one of input data.
// * Changed to prevent floating point exception at Ver 1.44
function  GetPlayedTime : DWORD;  // Played time in mili second since last flush
var
   Devider : DWORD;
begin
   Devider := (SPS * (BPS SHR 3) * Channels);
   if Devider <> 0 then
      result := round(1000 * (totalGet - DataInBuffer) / Devider)
   else
      result := 0;
end;


//---------------------- Winamp In/Out plug-in support -----------------------------

// get the reference number of specified plug-in
//  ( -1 :  specified plug-in is not loaded )
function GetPluginIndex(PluginName : string) : integer;
var
   i : integer;
begin
   result := -1;

   for i := 0 to MaxPluginNum - 1 do
      if Plugin[i] <> nil then
         if Uppercase(PluginInfo[i].Name) = Uppercase(PluginName) then
         begin
            result := i;
            break;
         end;
end;

// get the control pointer of specified plug-in
//  ( nil : specified plug-in is not loaded )
function GetPlugin(PluginNum : integer) : TPlugin;
var
   dw : dword;
begin
   if (PluginNum < 0) or (PluginNum > (MaxPluginNum - 1)) then
      result := nil
   else
   begin
      dw := dword(Plugin[PluginNum]);
      result := pointer(dw);
   end;
end;

// get the information on specified plug-in
function GetPluginInfo(PluginNum : integer) : TPluginInfo;
var
  APluginInfo : TPluginInfo;

 procedure ClearPluginInfo;
 begin
    with APluginInfo do
      begin
         Name := '';
         Version := 0;
         Description := '';
         FileExtensions := '';
         Seekable := 0;
         DLLHandle := 0;
      end;
 end;

begin
   if (PluginNum < 0) or (PluginNum > (MaxPluginNum - 1)) then
      ClearPluginInfo
   else if Plugin[PluginNum] = nil then
      ClearPluginInfo
   else
      with APluginInfo do
      begin
          Name := PluginInfo[PluginNum].Name;
          Version := PluginInfo[PluginNum].Version;
          Description := PluginInfo[PluginNum].Description;
          FileExtensions := PluginInfo[PluginNum].FileExtensions;
          Seekable := PluginInfo[PluginNum].Seekable;
          DLLHandle := PluginInfo[PluginNum].DLLHandle;
      end;

   result := APluginInfo;
end;

// get the played time in ms
function GetOutputTime(omodTime : boolean) : integer;
begin
   if omodTime then
      result := omod.GetOutputTime
   else
      result := Plugin[imodNum].GetOutputTime;
end;

procedure SetPosChanged;
begin
   PosChanged := true;
end;

function SelectInputPlugin(PluginNum : integer) : integer;
begin
   result := -1;

   if (PluginNum < -1) or (PluginNum > (MaxPluginNum - 1)) then
      exit;

   if PluginNum >= 0 then
      if Plugin[PluginNum] = nil then   // no valid Plugin[x]
         exit;

   imodNum := PluginNum;  // set imodNum to -1 if no plug-in is in use.
   result := 0;
end;

function ActivePlugin : string;
begin
   if imodNum = - 1 then
      result := ''
   else
      result := PluginInfo[imodNum].Name;
end;

{procedure SetMainHandle(MainHandle : HWND);
begin
   hMainHandle := MainHandle;
end; }

{function GetMainHandle : HWND;
begin
   result := hMainHandle;
end; }

// check if output plug-in emulator is ready  ( true : ready )
function OutputReady : boolean;
begin
   result := omodReady;
end;

// Check whether the specified Winamp input plug-in is loaded.
function IsWinampPluginLoaded(PluginName : string) : boolean;
var
   i : integer;
begin
   result := false;

   for i := 0 to MaxPluginNum - 1 do
      if Plugin[i] <> nil then     // Plugin[i] = nil -> empty element
         if Uppercase(PluginInfo[i].Name) = Uppercase(PluginName) then
         begin
            result := true;  // The specified plug-in is loaded.
            exit;
         end;
end;

// load a Winamp input plug-in  ( 0 : succeed )
function LoadWinampPlugin(PluginName : string) : integer;
var
   indexToLoad : integer;
   FilePath : string;
   DLLHandle : THandle;
   getInModule2 : function : pointer; stdcall;
   dw : dword;
   i : integer;
begin
   if not omodReady then
   begin
      result := ERROR_OMOD_NOTREADY;  // output plug-in emulator is not ready
      exit;
   end;

   if IsWinampPluginLoaded(PluginName) then
   begin
      result := ERROR_LOADED_BEFORE;  // already loaded plug-in
      exit;
   end;

   indexToLoad := -1;
   for i := 0 to MaxPluginNum - 1 do
      if Plugin[i] = nil then
      begin
         indexToLoad := i;       // found out empty element to use
         break;
      end;
   if indexToLoad = -1 then
   begin
      result := ERROR_LOADED_FULL;     // no empty element to use
      exit;
   end;

   FilePath := GetProgDir + 'Plugins\' + PluginName;
   DLLHandle := LoadLibrary(pChar(FilePath));
   if DLLHandle = 0 then
   begin
     result := ERROR_CANNOT_LOAD;
     exit;
   end;

   getInModule2 := GetProcAddress(DLLHandle, 'winampGetInModule2');
   if @getInModule2 = nil then
   begin
     result := ERROR_INVALID_PLUGIN;
     FreeLibrary(DLLHandle);
     exit;
   end;

   imod := getInModule2;
   imod.hMainWindow := hMainMsg;  // <= handle of internal messsage handler
   imod.hDllInstance := DllHandle;
   imod.outMod := omod;
   imod.init;
   imod.SetInfo := SetInfo1;
   imod.dsp_IsActive := dsp_isactive1;
   imod.dsp_dosamples := dsp_dosamples1;
   imod.SAVSAInit := SAVSAInit1;
   imod.SAVSADeInit := SAVSADeinit1;
   imod.SAAddPCMData := SAAddPCMData1;
   imod.SAGetMode := SAGetMode1;
   imod.SAAdd := SAADD1;
   imod.VSASetInfo := VSASetInfo1;
   imod.VSAAddPCMData := VSAAddPCMData1;
   imod.VSAGetMode := VSAGetMode1;
   imod.VSAAdd := VSAAdd1;
//   imod.About(0);

   dw := dword(imod);
   Plugin[indexToLoad] := pointer(dw);
   PluginInfo[indexToLoad].Name := PluginName;
   PluginInfo[indexToLoad].Version := imod.version;
   PluginInfo[indexToLoad].Description := string(imod.description);
   PluginInfo[indexToLoad].FileExtensions := string(imod.FileExtensions);
   PluginInfo[indexToLoad].Seekable := imod.is_seekable;
   PluginInfo[indexToLoad].DLLHandle := DLLHandle;

   result := 0;
end;

// unload a Winamp input plug-in  ( 0 : succeed )
function UnloadWinampPlugin(PluginName : string) : integer;
var
   indexToUnload : integer;
   returnOK : longbool;
begin
   indexToUnload := GetPluginIndex(PluginName);

   if indexToUnload = -1 then
   begin
      result := ERROR_NOT_LOADED;
      exit;
   end;
   if indexToUnload = imodNum then
   begin
      result := ERROR_IN_USE;
      exit;
   end;

   Plugin[indexToUnload].Quit;
   returnOK := FreeLibrary(PluginInfo[indexToUnload].DLLHandle);
   if returnOK then
   begin
      result := 0;
      Plugin[indexToUnload] := nil;
   end else
      result := ERROR_CANNOT_UNLOAD;
end;

// get the file types (file extensions) which can be played by any loaded plug-ins
function GetPlayableFiles : string;
var
   s : string;
   i : integer;

  function GetFileExt(s : string) : string;
  var
     s1, s2 : string;
     i : integer;
  begin
     s1 := s;
     if length(s1) = 0 then
        s2 := ''
     else begin
        s2 := '*.';
        for i := 1 to length(s1) do
        begin
           s2 := s2 + s1[i];
           if s1[i] = ';' then
              if i < length(s1) then
                 s2 := s2 + '*.';
        end;
     end;

     result := LowerCase(s2);
  end;

begin
   s := '';
   if not omodReady then  // if not performed function InitPluginCtrl yet or failed to
      exit;               //  get buffer memory to support Winamp input plug-ins.

   for i := 0 to MaxPluginNum - 1 do
      if Plugin[i] <> nil then
         s := s + GetFileExt(Plugin[i].FileExtensions) + ';';

   result := s;
end;

// Get the number of plug-in which can play specified file type
function GetPluginNumber(ExtCode : string) : integer;
var
   i : integer;
begin
   result := -1;  // Pre-assume there is no plug-in for specified file type

   for i := 0 to MaxPluginNum - 1 do
      if Plugin[i] <> nil then
         if pos(upperCase(ExtCode), upperCase(Plugin[i].FileExtensions)) <> 0 then
         begin
            result := i;
            break;
         end;
end;


//--------------------- Winamp visualization plug-in support --------------------

procedure LoadVisModule(PluginPath : string;
                        var Visheader : PWinampVisHeader;
                        var Vismod : TVismod;
                        var NumVismod : integer;
                        ParentHandle : HWND);
var
   i : integer;
begin
   NumVismod := 0;
   VismodNum := 0;
 //  CloseVisDLL;

   if Uppercase(PluginPath) <> Uppercase(GetLoadedVisDLL) then
      if not initVisDll(PluginPath) then
        exit;

  // try
  //   Visheader := getVisHeader;
  // except
     Visheader := getVisHeader(ParentHandle);
  // end; 

   if VisHeader = nil then
     exit;

   for i := 0 to (maxVismodNum - 1) do
   begin
      Vismod[i] := Visheader.getModule(i);
      if Vismod[i] <> nil then
      begin
         Vismod[i]^.hwndParent := ParentHandle;
         Vismod[i]^.hDllInstance := GetVisDLLHandle;
         inc(VismodNum);
      end else
         break;
   end;

   CurVismod := Vismod;
   NumVismod := VismodNum;
   VismodIndex := -1;
end;


function UnloadVisModule : integer;
begin
   result := -1;
   if VismodNum = 0 then
      exit;

   VisIsActive := false;
   if IsWindow(hVisOutWindow) then
      if not VisualizationQuitted then
         if CurVismod[VismodIndex] <> nil then
            CurVismod[VismodIndex]^.Quit(CurVismod[VismodIndex]);

   CloseVisDLL;
   VismodNum := 0;
   hVisOutWindow := 0;
   result := 0;
end;

function GetBufferAddress : pointer;
begin
   result := @DataBuffer[0];
end;

//--------------------- Winamp DSP & General Purpose plug-in support ---------------------

// Some Winamp DSP plug-in shows errorneous behavior if Winamp IPC messages are not handled.
// I tried to support both DSP plug-in and visaulization plug-in with one Winamp-like window.
// But I could not solve some problems so I decided to use seperate Winamp-like window,
// one for DSP plug-ins and the other for visaulization plug-ins.
// The Winamp-like window for visaulization plug-ins are managed in VisDrive.pas unit.

// This function is called by Winamp general purpose plug-in and creates a window which becomes
// the EMBED window for Winamp GPP.
function MyembedWindow(ews : PembedWindowState) : HWND; cdecl;
var
   WindowEmbed : HWND;
   Loc_X, Loc_Y : integer;
   ClientWidth, ClientHeight : integer;
   pRect : TRect;
   FormNo : integer;

begin
 // Create a EMBED window for Winamp GPP
   WindowEmbed := InitGenDrawer(pchar(LoadingGPP), hParentHandle, hMainMsg, DataMsgId, FormNo);

   if WindowEmbed <> 0 then   // EMBED window is successfully created
   begin
      if (ews = nil) or (ews^.me = 0) then
      begin
        Loc_X := 500;
        Loc_Y := 500;
        ClientWidth := 500;
        ClientHeight := 500;

        if ews <> nil then
        begin
         // if ews^.me = 0 then
              ews^.me := WindowEmbed;

           if (ews^.r.Right > ews^.r.Left) and (ews^.r.Bottom > ews^.r.Top) then
           begin
              ClientWidth := ews^.r.Right - ews^.r.Left {+ 1};
              ClientHeight := ews^.r.Bottom - ews^.r.Top {+ 1};
           end;
        end;

        if IsWindow(hParentHandle) then
         //  if IsWindowVisible(hMainWindow) then
           begin
              GetWindowRect(hParentHandle, pRect);

           // Put the EMBED window at the right side of main window but slightly
           //  different coordinate per each window to avoid complete overlap.
              Loc_X := pRect.Right + (FormNo + 1) * 50;
              Loc_Y := pRect.Top + (FormNo + 1) * 30;
           end;

      // Show the EMBED window
        ShowGenDrawer(FormNo, Loc_X, Loc_Y, ClientWidth, ClientHeight, true);
        result := WindowEmbed;
        exit;
      end;


      SetParent(ews^.me, WindowEmbed);
   end;

   result := WindowEmbed;
end;

function WindowProc(hWnd, Msg, wParam, lParam : Longint) : Longint; stdcall;
var
   TitleP : pAnsiChar;
   FileP : pAnsiChar;
   FileP2 : pChar;
   TitleS : string;
   RetLen : integer;

   ews : PembedWindowState;
   PextendedFileInfoStruct : ^TextendedFileInfoStruct;
   metadataP : pchar;
   pRect : TRect;

begin
   Result := 0;     // default value

   if Msg = WM_WA_IPC then   // Message is from DSP plug-in ?
   begin
     if lParam = IPC_GETVERSION then
        Result := $2041    // acts as if the main program is Winamp ver 2.41
     else if lParam = IPC_ISPLAYING then
     begin
        if PlayerMode = plmPlaying then
           Result := 1
        else if PlayerMode = plmPaused then
           Result := 3;
     end else if lParam = IPC_GETOUTPUTTIME then
     begin
        if wParam = 0 then      // position in miliseconds
        begin
           if PlayerMode <> plmPlaying then
              Result := -1
           else
              Result := PlaybackPosition;
        end else if wParam = 1 then      // song length in seconds
           Result := StreamInfo.Duration div 1000;
     end else if lParam = IPC_GETLISTLENGTH then
        Result := GetNumOfPlayListItem
     else if lParam = IPC_GETLISTPOS then
        Result := GetIndexOfPlayList('')
     else if lParam = IPC_GETINFO then
     begin
        if wParam = 0 then           // Sample rate
           Result := StreamInfo.SampleRate
        else if wParam = 1 then      // Bit rate
           Result := StreamInfo.Bitrate
        else if wParam = 2 then      // Channels
           Result := StreamInfo.Channels;
     end else if lParam = IPC_GETPLAYLISTFILE then
     begin
       {$IFDEF DELPHI_2007_BELOW}
        FileP := pAnsiChar(GetAPlayListFile(WParam));
       {$ELSE}
        FileP := ToPMultiByte(pWideChar(GetAPlayListFile(WParam)));
       {$ENDIF}
        Result := integer(FileP);
     end else if lParam = IPC_GETPLAYLISTTITLE then
     begin
       {$IFDEF DELPHI_2007_BELOW}
        TitleP := pAnsiChar(GetAPlayListTitle(WParam));
       {$ELSE}
        TitleP := ToPMultiByte(pWideChar(GetAPlayListTitle(WParam)));
       {$ENDIF}
        Result := integer(TitleP);
     end else if LParam = IPC_GETINIFILE then
     begin
       {$IFDEF DELPHI_2007_BELOW}
        FileP := pAnsiChar(ExtractFilePath(ParamStr(0)) + 'Plugins\plugin.ini');
       {$ELSE}
        FileP := ToPMultiByte(pWideChar(ExtractFilePath(ParamStr(0)) + 'Plugins\plugin.ini'));
       {$ENDIF}
        Result := integer(FileP);
     end else if LParam = IPC_GETWND then
     begin
        if WParam = IPC_GETWND_PE then
           result := hFakeWindow
        else if WParam = IPC_GETWND_MB then
           result := hMBWindow;

     end else if LParam = IPC_MBOPEN then
     begin
        if hMBWindow <> 0 then
           if WParam = 0 then
           begin
              pRect.Right := 100;   // default X pos
              pRect.Top := 100;     // default Y pos
              if IsWindow(hParentHandle) then
                if IsWindowVisible(hParentHandle) then
                   GetWindowRect(hParentHandle, pRect);

           // Set the size of the mini browser as the size of main form and
           // put the mini browser at the left side of main form.
              ShowMiniBrowser(pRect.Right,  // pos X
                              pRect.Top,                                // pos Y
                              380,   // width
                              pRect.Bottom - pRect.Top, true);  // height
           end else
           begin
              FileP := pAnsiChar(WParam);
              MBNavigate(FileP);
           end;
     end else if LParam = IPC_GET_GENSKINBITMAP then   // * Added at Ver 1.44.1
     begin
       if hMBWindow <> 0 then
        //  result := Genex_.Handle;
          result := GetGenexBitmap;   // Use the bitmap in MBDrawer.dll
     end

     else if LParam = IPC_GET_EXTENDED_FILE_INFO then   // * Added at Ver 1.44.1
     begin
     // tag names : title  artist  albumartist  album  genre  year  disc  publisher
     //             comment  track   composer  conductor
        PextendedFileInfoStruct := pointer(WParam);
        FileP2 := PextendedFileInfoStruct^.filename;
        metadataP := PextendedFileInfoStruct^.metadata;
        if trim(string(FileP2)) = '' then
        begin
           if PextendedFileInfoStruct^.retlen > 0 then
              if string(metadataP) = 'title' then
                 PextendedFileInfoStruct^.ret := 'Changing Partners'
              else if string(metadataP) = 'artist' then
                 PextendedFileInfoStruct^.ret := 'Patti Page';

        end else
        begin
           if string(metadataP) = 'title' then
              PextendedFileInfoStruct^.ret := pchar(GetAPlayListTitle(GetIndexOfPlayList(ansistring(FileP2))))
           else if string(metadataP) = 'artist' then
              PextendedFileInfoStruct^.ret := pchar(GetAPlayListTitle(GetIndexOfPlayList(ansistring(FileP2))));
        end;

        Result := 1;
     end

     else if LParam = IPC_GET_EMBEDIF then
     begin
         if WParam <> 0 then
         begin
            ews := pointer(WParam);
            Result := MyembedWindow(ews)
         end else
            Result := longint(@MyembedWindow);
     end else if lParam = IPC_GET_API_SERVICE then
        Result := WaAPIServiceEntry
   {$IFDEF DEBUG}
     else
        Application.MessageBox(pchar('Unhandled IPC Msg' + intToStr(lParam)), 'Confirm',
                             MB_OK or MB_ICONINFORMATION) {$ENDIF} ;
  // Response to message WM_GETTEXT is to hand over the title of playing stream file to
  // Winamp plug-in.
  // You must add ' - Winamp' to the title string because some Winamp plug-ins
  // show erroneous operation if it is missed (ex. vis_Bass-C.dll)
   end else if Msg = WM_GETTEXT then
   begin
      TitleS := StreamInfo.Title +  ' - Winamp' + chr(0);
      if Length(TitleS) < wParam then
         RetLen := Length(TitleS)
      else
         RetLen := wParam - 1;

      StrPLCopy(PChar(lParam), TitleS, RetLen);
      Result := StrLen(PChar(lParam));
   end else
      Result := DefWindowProc(hWnd, Msg, wParam, lParam);

end;

// Create a fake window which acts like the one of Winamp's main window
function CreateFakeWindow(ParentWindow : HWND) : HWND;
var
   hInst  : HWND;             // handle to program (hinstance)
   WinAtom : TAtom;
   wClass : TWNDCLASSEX;
begin
   result := 0;

   if hFakeWindow <> 0 then   // Avoid duplicate creation of fake Winamp window
   begin
      result := hFakeWindow;
      exit;
   end;

   hInst := GetModuleHandle(nil); // get the application instance

   if not WinampClassRegistered then
   begin
     with wClass do
     begin
       cbSize        := sizeof(wClass);
       Style         := CS_PARENTDC {or CS_VREDRAW};
       lpfnWndProc   := @WindowProc;
       cbClsExtra    := 0;
       cbWndExtra    := 0;
       hInstance     := hInst;
       hIcon         := 0{LoadIcon(hInst, 'MAINICON')};;
       hCursor       := LoadCursor(0, IDC_ARROW);
       hbrBackground := COLOR_BTNFACE + 1;
       lpszMenuName  := nil;
       lpszClassName := CLASSNAME_WINAMP;
       hIconSm       := 0;
     end;

  // Once our class is registered we can start making windows with it
     WinAtom := windows.RegisterClassEx(wClass);

     if WinAtom <> 0 then
        WinampClassRegistered := true;

   end;

   if WinampClassRegistered then
      result := CreateWindowEx(0, CLASSNAME_WINAMP, 'Winamp 2.41',
                                    WS_POPUP,      // no-frame, non-visible window
                                    5, 5, 25, 25,  // x, y, width, height
                                    ParentWindow{0}{hMainWindow}, 0, hInst, nil);

end;


function DestroyFakeWindow : boolean;
begin
   if hFakeWindow <> 0 then
      if IsWindow(hFakeWindow) then
      begin
        try
          result := DestroyWindow(hFakeWindow); 	// handle to window to be destroyed
        except
        //  Application.MessageBox(pchar(SysErrorMessage(GetLastError)), 'Confirm', MB_OK or MB_ICONINFORMATION);
          result := false;
        end;

        if result then
        begin
          hFakeWindow := 0;
       //   windows.UnRegisterClass(CLASSNAME_WINAMP, hInst);
       //   WinampClassRegistered := false;
        end {else
          Application.MessageBox(pchar(SysErrorMessage(GetLastError)), 'Confirm', MB_OK or MB_ICONINFORMATION)};
      end else
         result := false
   else
      result := false
end;

procedure SetStreamInfo2(Stream : TStreamInfo);
begin
   StreamInfo := Stream;
end;

procedure SetPlayerMode2(Mode : TPlayerMode);
begin
   PlayerMode := Mode;
end;

procedure SetChannelInfo2(ChannelType : TChannelType; D_ChannelId, P_ChannelId,
                                        SampleRate, Channels : DWORD);
begin
   FChannelType := ChannelType;
   FD_ChannelId := D_ChannelId;
   FP_ChannelId := P_ChannelId;
   FSampleRate := SampleRate;
   FChannels := Channels;
end;

function DSPBufferReady : boolean;
begin
   result := (DSPBufferSize <> 0);
end;

function DSPActive : boolean;
begin
   result := DSPIsActive;
end;

procedure LoadDSPModule(PluginPath : string;
                        var DSPheader : PWinampDSPHeader;
                        var DSPmod : TDSPmod;
                        var NumDSPmod : integer{;
                        ParentHandle : HWND});
var
   i : integer;
begin
   NumDSPmod := 0;
 //  CloseDSPDLL;

   if Uppercase(PluginPath) <> Uppercase(GetLoadedDSPDLL) then
   begin
      DSPmodIndex := -1;
      DSPmodNum := 0;
      if not initDSPDll(PluginPath) then
         exit;
   end else if DSPmodNum > 0 then  // if already loaded DSP module
   begin
      DSPheader := getDSPHeader;
      DSPmod := CurDSPmod;
      NumDSPmod := DSPmodNum;
      exit;
   end;

   DSPheader := getDSPHeader;
   if DSPHeader = nil then
     exit;

   for i := 0 to (maxDSPmodNum - 1) do
   begin
      DSPmod[i] := DSPheader.getModule(i);
      if DSPmod[i] <> nil then
      begin
      //   DSPmod[i]^.hwndParent := hFakeWindow;
         DSPmod[i]^.hDllInstance := GetDSPDLLHandle;
         inc(DSPmodNum);
      end else
         break;
   end;

   CurDSPmod := DSPmod;
   NumDSPmod := DSPmodNum;
end;


function StartDSPModule(ModuleNum : word;
                        ParentHandle : HWND) : integer;
begin
   result := -1;

   if DSPBufferSize = 0 then
      exit;
   if hFakeWindow = 0 then
      exit;
   if DSPmodNum = 0 then
      exit;
   if ModuleNum > (DSPmodNum - 1) then
      exit;

   if DSPmodIndex = ModuleNum then  // Is alrready running ?
   begin
      result := 0;
      exit;
   end;

   if DSPmodIndex > -1 then
      if CurDSPmod[DSPmodIndex] <> nil then
         CurDSPmod[DSPmodIndex]^.Quit(CurDSPmod[DSPmodIndex]);

   CurDSPmod[ModuleNum]^.hwndParent := hFakeWindow;
   DSPIsActive := (CurDSPmod[ModuleNum]^.init(CurDSPmod[ModuleNum]) = 0);
   if DSPIsActive then
   begin
      DSPmodIndex := ModuleNum;     // DSPmodIndex : running module #
      result := 0;
   end;
end;

function StopDSPModule : integer;
begin
   result := -1;
   if DSPmodNum = 0 then
      exit;

   if DSPIsActive then
   begin
      DSPIsActive := false;
      if CurDSPmod[DSPmodIndex] <> nil then
         CurDSPmod[DSPmodIndex]^.Quit(CurDSPmod[DSPmodIndex]);
   end;

  // DestroyFakeWindow;
   DSPmodIndex := -1;

   result := 0;
end;

function UnloadDSPModule : integer;
begin
   if StopDSPModule = 0 then
   begin
      CloseDSPDLL;
      DSPmodNum := 0;
      result := 0;
   end else
      result := -1;
end;

procedure SetLoadingGPP(GPP_Path : string);   // * Added at Ver 1.44.2
begin
   LoadingGPP := GPP_Path;
end;

// ----------------------------- VisDrawer.dll support ----------------------------

{ var
   MyThreadId : DWORD;
   MyWinHandle : HWND;
   OtherWinHandle : HWND;

function LookAtAllWindows2(Handle: HWnd; Temp: Longint): BOOL; stdcall;
var
   ThreadId : DWORD;
   ProcId : DWORD;
begin
   result := true;

   if IsWindowVisible(Handle) then
   begin
   // get identifier of the thread which created window
      ThreadId := GetWindowThreadProcessId(Handle, @ProcId);
      if ThreadId = MyThreadId then
      begin
         if Handle <> OtherWinHandle then
         begin
            MyWinHandle := Handle;
            result := false;
         end;
      end;
   end;
end;  }


//--------------------------- Commonly used functions -------------------------------

function GetProgDir : string;
begin
   result := ExtractFilePath(ParamStr(0));
end;

function GetFakeWinHandle : HWND;
begin
   result := hFakeWindow;
end;


// initialization
// return value : true on success, false on failure
function InitPluginCtrl(MainHandle, ParentHandle, MiniBrowser, DataReadyMsg : HWND) : boolean;
var
   i : integer;
begin
   if omodReady then
   begin
      result := true;
      exit;
   end;

   result := false;

   if MainHandle = 0 then
      exit
   else
      hMainMsg := MainHandle;
   hParentHandle := ParentHandle;

   for i := 0 to MaxPluginNum - 1 do
     Plugin[i] := nil;
   imodNum := -1;

   try
     GetMem(pBuf, DefaultBufSize); // Get memory to hold sound data from Winamp input plug-in
     GetMem(dummyBuf, 4608);
     SetOutputPlugin;              // omodReady should be set at executing this procedure
     bufSize := DefaultBufSize;
   except
     exit;
   end;

   FillChar(dummyBuf^, SizeOf(dummyBuf), #0);

   if bufSize > 0 then
     try
       GetMem(DSPBuffer, ChannelLimit * PacketSize * 4);
       DSPBufferSize := ChannelLimit * PacketSize * 4
     except
       exit;
     end;

   hFakeWindow := CreateFakeWindow(ParentHandle);
   if hFakeWindow <> 0 then
      OrgWndProc := Pointer(GetWindowLong(hFakeWindow, GWL_WNDPROC));

   hMBWindow := MiniBrowser;
   DataMsgId := DataReadyMsg;

  { try
      Genex_ := TBitmap.Create;
      Genex_.LoadFromFile(GetProgDir + 'Plugins\Skin\Genex.bmp');

   except
      Application.MessageBox(pchar('Error at creating Bitmap.'), 'Information',
                                                           MB_OK or MB_ICONINFORMATION);
   end; }

   result := omodReady;
end;

// finalization
procedure QuitPluginCtrl;
var
   i : integer;
begin
   for i := 0 to MaxPluginNum - 1 do
      if (Plugin[i] <> nil) then
      begin                           // Release all Winamp input plug-ins
         Plugin[i].Quit;
         FreeLibrary(PluginInfo[i].DLLHandle);
      end;

   if (omod <> nil) then
      omod.Quit;

   if (pBuf <> nil) then
   begin
      FreeMem(pBuf);
      bufSize := 0;
   end;
   if dummyBuf <> nil then
      FreeMem(dummyBuf);

   if (DSPBuffer <> nil) then
   begin
      FreeMem(DSPBuffer);
      DSPBufferSize := 0;
   end;

 // Release subclassed window procedure before destroying the Fake Window
   if hFakeWindow <> 0 then
   begin
      if GetWindowLong(hFakeWindow, GWL_WNDPROC) <> LongInt(OrgWndProc) then
         SetWindowLong(hFakeWindow, GWL_WNDPROC, LongInt(OrgWndProc));
      DestroyFakeWindow;
   end;

  { if Assigned(Genex_) then
      Genex_.Free; }
end;

//---------------------------------------------------------------------------------

end.
