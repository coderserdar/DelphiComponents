// Component TBASSPlayer
//
//    written by Silhwan Hyun  (hyunsh@hanafos.com)
//
//
// Don't blame me for the clumsy, ungrammatical expressions, I am not a native speaker.
//
//

// Abbreviations
//
// (BASS add-on = plugin module designed for BASS, which supports the playback of additional
//   file formats with BASS audio library)
// (vis plug-in = Winamp visualization plug-in)
// (vis window = the window used for visualization created by vis plug-in)
// (EMBED window = the window which serves vis plug-in with its client area for visualization)
//

// Revision history
//

// Ver 2.1                     14 May 2009
//  - Modified for Delphi 2009.
//  - Fixed some minor bugs.
//
//  Ver 2.01            31 Jan 2009
//  Modified several procedures to adapt new Vis Drawer Library(VisDrawer2.dll, this provides
//   EMBED window with Winamp-like visualization window frame).
//  Added a procedure LetMBNavigate2, it enables you search lyrics page for given Artist & Title.
//  Added a sound effect member in TSoundEffect : Rotate
//  Added a function IsValidURL
//  Changed property StreamName -> property StreamPath
//  Added a procedure SetMuteState
//  Added properties : FlangerLevel, Mute, BuildDateStr
//  Supports Fade-in and Fade-out the channel's volume over the period of 500ms at pause, stopping
//   or restarting playing. (2.01.01)
//
//
// Ver 2.00              1 Oct 2008
//  Added following functions/procedures/properties to support Play List,
//   - function : NumOfPlayListItem, PlayListTitle, IndexOfPlayList
//   - procedure : ClearPlayList, AddToPlayList, DeleteFromPlayList, PlayListChangeTitle
//   - property : PlayListIndex, OnUpdatePlayList
//  Added following functions and properties to support Winamp General Purpose Plug-ins,
//   - function : RunGPPModule, RunAllGPPModules, QuitGPPModule, NumberLoadedGPP, GetLoadedGPPInfo
//   - property : MiniBrowserReady, OnMBWindowShow
//  Added 2 properties(MixerReady, DownMixToStereo) to support downmixing multi channel sound
//   data to stereo. (=> bassmix.dll is loaded for that by default.)
//  Added a property(BASSAACReady) and modified procedures to support AAC+ stream from net.
//  (=> You should load bass_aac.dll to activate the procedures.)
//  Modified procedure "Open" to use BASS library(basswma.dll) for ASF and WMA streams by
//   default, Winamp plug-in is applied only if basswma.dll is not loaded.
//  Modified procedure ShowErrorMsgBox to support localization of error messages.
//  Added a Timer and Timer event handler to coincide the hilighted state of title bar of
//   EMBED window with the focused state of the winodow, for the EMBED window created by
//   VisDrawer.dll.
//  Placed procedure "WinProcessMessages" per thread because I am not sure whether the procedure
//   is safe at multi thread environment.
//  Modified procedure "Stop" to permit transition of FPlayerMode from plmPaused to plmStopped
//   state.
//  Modified procedure "Play" to fix the sync problem at start playing the stream from Net Radio(~.ASX).
//  Modified procedure "ResumePlay" and "Restart" for proper operation of the stream from Net Radio.
//  ( Fixed the halt problem at resuming/restarting Net Radio after long interval of pause/stop.)
//  Added a BASS_SetConfig option(BASS_CONFIG_NET_PLAYLIST) to process URLs in PLS and M3U playlists.
//  Modified procedure TVisDataThread.DoOnVisTimer to remove range check error.
//  Use of new VisDrawer.dll(Renamed from Gen_VisDrawer.dll, ver 1.0.0.3 -> ver 1.1.0.4)
//   - Added 2 functions : TitleBarHighlighted, SetTitleBar
//  Replaced with new version of Tag managing units for unicode support (ID3v1.pas, ID3v1.pas,
//   MPEGaudio.pas, OggVorbis.pas).
//  Adapted Tag managing units for view and editing the Tag of AAC files (AACfile.pas, APETag.pas).
//
// (supplement at Ver 2.00.1)
//  Modified procedures and added properties to drive multiple Winamp GPPs simultaneously.
//   - added properties : GenWinHandle, OnGenWindowShow
//  Removed property MiniBrowserReady.
//  Added properties : MBWinHandle,
//  Added a parameter(= Artist) for procedure AddToPlayList.
//  Releases subclassed window procedure before deallocating MessageHandle.
//
// (supplement at Ver 2.00.2)
//  Added a procedure LetMBNavigate
//
// (supplement at Ver 2.00.3)
//  Removed property : GenWinHandle
//  Added function : ShowMBForm, GPPDrawerInfo, MagneticWindowAdd, MagneticWindowRemove, GetLyrics
//
//
// Ver 1.92              19 Jul 2008
//   ( Based on bass.dll/basswma.dll/basscd.dll/bassmidi Ver 2.4 )
//  Changed procedure RunVisPlugin of TBASSPlayer, procedure ResumeVis and DoOnVisTimer of
//   TVisDataThread class to increase stability at exchanging vis plug-ins.
//  Fixed bug at opening an WMA stream from net radio station.
//  Fixed erroneous output of BandOut[x] at restarting playback a stream.
//  Added property "VisEMBEDHandle" to enable you to read & to set EMBED window.
//  Added property "VisEMBEDSwitchMode" to enable you to select the method of switching
//   EMBED window for running vis plug-in.
//  Added property "OnPluginRequest" to enable you to handle requests from Winamp plug-in
//   such as volume up/down, seek forward/backward.
//  Modified function "RunVisPlugin" ( excluded a parameter ).
//  Deleteded a function "UseExtVisDriver"
//  Changed to stop TimerFFT while vis plug-in is active.
//   -> OnNewFFTData event does not occurs while vis plug-in is active.
//  Use of new Gen_VisDrawer.dll(ver 1.0.0.1 -> ver 1.1.0.3) : Added a function "init2".
//   a. We can select a response at receiving mouse click on "CLOSE" button area in title
//     bar.
//   b. We can set initial position of vis window
//   c. Removed the need of initialization file(= Gen_VisDrawer.ini)
//  The shared memory component(= fisSharedMemory1) of TVisDataThread is substituted with
//   normal memory buffer.
//

// Ver 1.91            8 Dec 2006
//  Added functions and properties to support "BASSMIDI" (See bassmidi.chm).
//   - function MIDIFontInit    : initializes a SF2 soundfont.
//   - function MIDIGetTrackInfo : retreives track information on a given MIDI file.
//   - property MIDISoundReady  : informs you whether the functions to play a MIDI file is
//                                operational or not.
//   - property BASSMIDIReady   : informs you whether bassmidi.dll is loaded or not.
//   - property OnGetLyric      : event handler on catching lyric while playing a MIDI file.
//  Modified function GetNativeFileExts(=> property NativeFileExts) to support "BASSMIDI".
//  Added properties to support Winamp 5 visulization plug-ins.
//   - property VisDrawerReady : indicates that Winamp-like vis drawer is ready.
//                         (note) The Winamp-like vis drawer is driven by a customized
//                                Winamp general purpose plug-in, 'Gen_VisDrawer.dll'.
//   - property UseVisDrawer   : specifies whether to use Winamp-like Vis drawer or not.
//   - property OnVisWindowShow : event handler at showing up or closing of vis window.
//  Fixed access violation error at excuting "Restart" a opened stream which is not Seekable.
//  Obsoleted external vis plug-in driver("VisOut.exe"). However you can set to use it, but
//   Winamp 5 vis plug-ins may not operate with it.
//  Obsoleted a property SyncVisWindow. (Normally the vis window created by Winamp 5 vis
//     plug-in is shown at showing up main window, is hidden at hiding main windw )
//

// Ver 1.9            26 Oct 2006
//   ( Based on bass.dll/basswma.dll/basscd.dll Ver 2.3.0.1 )
//
//  Modified some functions according to new version of BASS audio library.
//  Added following 3 functions to support BASS add-on.
//    BASSAddonLoad, GetBASSAddonList, BASSAddonFree
//  Added property "BASSAddonExts" to inform you the playable file types with BASS add-ons
//   currently loaded.
//  Added property "DecoderName" to inform you the name of decoder responsible for the currently
//   opened stream.
//  Set StreamInfo.BitRate to zero if the playback duration of the loaded URL stream
//  is zero.
//

// Ver 1.81                      9 Apr 2005
//  Added file information viewer & tag editor for 3 major file types (MPEG, OGG Vorbis, WMA).
//  These types of stream files are suppoorted by internal code, i.e, you do not need to load
//   Winamp input plug-ins to view file information or to edit TAG.
//  Renamed function "PluginInfoBox" to "FileInfoBox".
//

// Ver 1.8                       27 Feb 2005
//  Introduced "dual channel mode" to enable you to activate DSP effects using Winamp DSP plug-ins
//   for the streams being decoded by BASS.
//   You were able to activate DSP effects using Winamp DSP plug-in only if the opened stream is
//   decoded by Winamp input plug-in at previous version.
//   Now you do not need load Winamp input plug-in to activate DSP effects using Winamp DSP
//   plug-in with "dual channel mode" operation.
//
//   note)
//     The previous versions of TBASSPlayer operates in "single channel mode" for the streams
//     being decoded by BASS.
//     "Single channel mode" means one channel is used to process a stream, i.e., decoding
//     & output processes are done with single BASS channel.
//     "Dual channel mode" means two BASS channels are used to process a stream, one for decoding
//     and the other for output.
//     Because it is possible to manipulate decoded data from decoding channel before rendering
//     to output channel at "Dual channel mode" operation, any DSP effects by Winamp DSP plug-in
//     can be applied.
//     But "single channel mode" is more reliable (there is no intermediate operation between channels)
//     and less resource consuming than "Dual channel mode.
//     So if you have low grade or low speed system it is preferable to use "single channel mode".
//
//  Updated PluginCtrl.pas to support "dual channel mode".
//  * About ASF and WMA stream from net radio station : DSP effects which renders less amount of
//     resampled data than the one of original data may cause problems due to a network characteristic.
//  Added property SingleChannelMode to enable you to set TBASSPlayer to "single channel mode"(= same
//   operation mode to previous version).
//  Updated sub unit PluginConfig.pas
//   - The 'Enabled' property of "Unload" button of PluginConfigForm is automatically set.
//   - Displays different colors according to the status of the input plug-ins, i.e., red for the
//     plug-in in use, and black for the plug-ins not in use.
//  Removed property UnloadButEnabled.
//

// Ver 1.7                        6 Feb 2005
//  Made sub unit(=VisDrive.pas) to drive vis plug-ins, which creates a seperate thread to
//   drive vis plug-ins and the created thread co-works with the thread which takes charge of
//   data gathering for vis plug-ins.
//  Replaced sub-component "TVisThread" with new-written "TVisDataThread", which takes charge
//   of data gathering for vis plug-ins.
//  TBASSPlayer is improved as follows with above 2 major updates.
//   - Can drive vis plug-ins without external program(=VisOut.exe).
//   - Operates with multi threads for fast execution.
//   - The process of data gathering for vis plug-in supports multi channel streams.
//   - The amount of trnasfering data is adjusted according to the parameter "spectrumNch"
//      and "waveformNch" of vis plug-in.
//  Added a constant "MaxChannels" to define the maximum channels in a stream.
//  Added a function "UseExtVisDriver" which enables you to select the driving method, driving
//   vis plug-ins by internal code of TBASSPlayer, or by external program(=VisOut.exe).
//  Added a property "OnModeChange" to notify when player's state is changed.
//  Renamed procedure "RunVisoutProgram" to "RunVisPlugin".
//  Renamed procedure "QuitVisoutProgram" to "QuitVisPlugin".
//  Modified some functions to prevent problems at log off or shut down system.
//  Modified function "CurrentPosition" to return correct playback position at running a DSP
//   plug-in.
//  Updated PluginCtrl.pas for
//     - Enhanced stability at running DSP plug-ins.
//     - Supports multi channel streams.
//

// Ver 1.6                        14 Dec 2004
//   ( Based on bass.dll/basswma.dll/basscd.dll Ver 2.1 )
//  Made some modifications for new BASS version (2.0 -> 2.1)
//  Added a function "SetAEQGain" to set the gain of a designated equalizer's band.
//    - function SetAEQGain(BandNum : integer; EQGain : float) : boolean;
//  Modified some functions to support music file(MO3, IT, XM, S3M, MTM, MOD, UMX formats)
//   using BASS native function.
//

// Ver 1.54                       25 Sep 2004
//  Modified function "SetVolume" to control the output volume of Winamp input plug-in
//   which directly sounds out(= not thru output plug-in emulator).
//  Added a property and modified some functions to support playing CD Audio tracks
//   using BASSCD.DLL.
//   -> You do not need Winamp input plug-in such as in_cdda.dll to play CD Audio tracks.
//    - property BASSCDReady : indicates whether BASSCD.DLL is operational.
//  note) BASSCD.DLL cannot access CD drive after accessing it using Winamp input plug-in
//        for CD drive "in_cdda.dll" ("in_CDReader.dll" does not cause this problem).
//        BASS_CD_StreamCreateFile returns BASS_ERROR_NOCD in spite of presence of audio CD.
//        In this case let CD drive re-read media information (open CD drive door then close
//        CD drive door)
//

// Ver 1.53                       25 Jul 2004
//  Added function "PluginInfoBox" to use the function "InfoBox" of Winamp input plug-in.
//    - function : PluginInfoBox(StreamName : string): Boolean;
//  Excluded the parameter "MainWinHwnd" of the functions - RunVisoutProgram, RunDSPPlugin.
//  The parameter "MainWinHwnd" is substituted by the window handle of owner of TBASSPlayer.
//

// Ver 1.52                       7 Feb 2004
//  Attached description to Public declarations and to Published declarations for
//   self-explanation.
//   (I am not a native English speaker.  There may be misspelled, ambiguous, clumsy or
//    grammarless expressions.  I would very much appreciate your sending me the correct/smart
//    expression to replace such expression).
//
//  Added a function "VisThreadReady" in sub-component TVisThread which is used
//   to check the availability of VisThread in TBASSPlayer.
//

// Ver 1.51                       24 Jan 2004
//  Introduced sub-component TVisThread to raise refresh rate of vis window.
//   (The maximum refresh rate is changed from 30 to 100 fps.)
//  VisOut.exe is launched by internal code of BASSPlayer.pas.
//   (Appexec.pas is not needed to launch VisOut.exe.)
//  Inhibited multiple instance of VisOut.exe because they will use same address
//   space obtained by fisSharedMemory1.
//

// Ver 1.5                        15 Dec 2003
//   ( Based on bass.dll/basswma.dll Ver 2.0 )
//
//  Added properties to play stream files from internet, or the stream from net
//   radio station (Shoutcast/Icecast server, WMA broadcaster).
//   Followings are the new properties for them.
//    - IsNetStream : true if the stream file is from internet
//    - IsNetRadio  : true if the stream comes from net radio such as Shoutcast/Icecast
//                    server
//    - ICYInfo     : pointer to ICY tag
//    - HTTPInfo    : pointer to HTTP tag
//    - DownloadProgress : download progress in bytes of the stream file from internet.
//    - OnGetMeta   : event handler for catching Meta data from Shoutcast/Icecast server.
//    - OnGetChannelInfo : event handler for catching information of an URL stream,
//                         obtained from a Winamp input plug-in.
//    - OnDownloaded : event handler for catching re-estimated value of the playback length
//                      & tag data of an URL stream.
//  Added property "DecodingByPlugin" to show which takes charge of decoding stream,
//     (true if it is Winamp input plug-in, false if BASS)
//  Added property "Seekable" to show if playback position can be changed.
//  Added a function and a procedure to support Winamp DSP plug-ins.
//    - function : RunDSPPlugin         // Run a DSP plug-in
//    - procedure : QuitDSPPlugin       // Quit a DSP plug-in
//   note) The DSP effects using Winamp DSP plug-in is active only if the streams being
//         played are decoded by Winamp input plug-in.
//  Added a function to inform player status to the driver of vis window.
//    - procedure : InformPlayerStatus
//  Added some parameters at running VisOut.exe for more proper operation.
//
//  Changed sound volume range : 0 ~ 100 -> 0 ~ 255
//

// Ver 1.4                        12 May 2003
//   ( Based on bass.dll/basswma.dll Ver 1.8 )
//  Added(modified) functions, procedures and properties to set up synchronized action
//   between the windows of main program and the driver of vis window.
//   - New function : GetVisWindowHandle     // get the handle of vis window
//   - Modified function : RunVisoutProgram  // added a parameter to inform vis window driver
//                                           //  the window handle of main program
//   - New procedure : HideVisWindow         // Hide vis window
//                     ShowVisWindow         // Show vis window
//   - New property : VisScale               // set(get) output level of FFT (min : 256 ~ max : 1024)
//                    SyncVisWindow          // set(get) status of synchronized action (true : enabled)
//

// Ver 1.3                        5 Apr 2003
//   ( Based on bass.dll/basswma.dll Ver 1.8 )
//  Added a function and a procedure to run Winamp Visualization plug-ins
//   - function RunVisoutProgram(PluginPath : string; VismodNo : word) : boolean;
//   - procedure QuitVisoutProgram;
//  Modified function GetVolume and procedure SetVolume to reflect/adjust the
//   volume level of Winamp plug-in's output
//  Added property EchoLevel & ReverbLevel to adjust the intensity of sound effect
//

// Ver 1.2                        5 Feb 2003
//   ( Based on bass.dll/basswma.dll Ver 1.7 )
//  Added functions & properties to use Winamp 2 input plug-ins.
//   Now TBASSPlayer can play stream files which BASS audio library cannot decode
//   if provided appropriate Winamp input plug-in.
//  Changed the loading method of bass.dll & basswma.dll from static loading to
//   dynamic loading
//
//   - Modified function : GetStreamInfo
//   - New properties : BASSWMAReady, Version, NativeFileExts, PluginFileExts, CanUsePlugin,
//                      SupportedBy, PluginFirst, OnPluginStartPlay, UnloadButEnabled
//   - Renamed property : DLLVersionStr -> BASSDLLVer
//   - New  procedure : ShowPluginConfigForm (-> used to show PluginConfigForm,
//                        which is defined in sub unit PluginConfig.pas)
//   - New functions in sub unit PluginCtrl which you can call in main program
//     if you want to manage Winamp input plug-ins directly(= not using PluginConfigForm).
//       GetPluginIndex(PluginName : string) : integer;
//       GetPlugin(PluginNum : integer) : TPlugin;
//       GetPluginInfo(PluginNum : integer) : TPluginInfo;
//       LoadWinampPlugin(PluginName : string) : integer;
//       UnloadWinampPlugin(PluginName : string) : integer;
//  * include "PluginCtrl" in uses clause of main program to use above functions
//

// Ver 1.1                         8 Dec 2002
//   ( Based on bass.dll/basswma.dll Ver 1.7 )
//   - New function : GetChannelFFTData (for custom processing of FFT data)
//   - Added field at record TStreamInfo : Year
//   - Added property : EQBands (for custom setting of equalizer bands)
//   - Renamed property : EQGains ( <- EqPreset )
//   - New type identifiers : TBandData, TEQBands
//   - Renamed type identifier : TEQGains ( <- TEqPreset )
//   - Updated procedure : TimerFFTTimer (for better output of spectrum visualization)
//

// Ver 1.0                         4 Aug 2002
//   ( Based on bass.dll/basswma.dll Ver 1.6 )
//   - Initial release

unit BASSPlayer;

interface

{$INCLUDE Delphi_Ver.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, ExtCtrls, syncobjs,
  Dynamic_BASS, RT_BASSWMA, RT_basscd, RT_bassmidi, bass_aac, RT_bassmix, ioplug, PluginCtrl,
  MPEGAudio, OggVorbis, AACfile, WMAFile, WAVFile, PluginConfig,
  MPEGInfoBox, OGGInfoBox, WMAInfoBox, wa_ipc, WasabiAPI, Dialogs, MMSystem, VisDrive2,
  PlayListUtils, TntCollection, UniCodeUtils, GPPDrive
  {$IFNDEF DELPHI_2007_BELOW}, AnsiStrings{$ENDIF}
  {, EmbededBrowser, TestMB};

const
   MaxChannels = ChannelLimit;
   MAXCDDRIVES = 4;
   MaxVolume = 255;      // Volume range : 0 ~ 10000
   MaxDX8Effect = 32;    // DX8 Effect range : 0 ~ 32
   MaxTitleLen = 255;
   NumFFTBands = 25;     // Number of bands for spectrum visualization
   NumEQBands  = 10;     // Number of bands for equalizer
   EQBandWidth  : array[0..NumEQBands-1] of float =
                  (4, 4, 4, 4, 5, 6, 5, 4, 3, 3);
   EQFreq : array[0..NumEQBands-1] of float =
                  (80, 170, 310, 600, 1000, 3000, 6000, 10000, 12000, 14000);
   MaxLoadableAddons = 8;       // maximum number of add-on simultaneously loadable
   MaxLoadableGPPs = MaxGPPNo;  // maximum number of Winamp GPP simultaneously loadable

 //  VisDrawerDLL = 'VisDrawer.dll';
   VisDrawerDLL = 'VisDrawer2.dll';
   MBDrawerDLL =  'MBDrawer.dll';

 //  VisThreadPriority = integer(THREAD_PRIORITY_BELOW_NORMAL); // THREAD_PRIORITY_LOWEST
   VisThreadPriority = integer(THREAD_PRIORITY_NORMAL);

type
  TSoundEffects = set of (Flanger, Equalizer, Echo, Reverb, Rotate);
  TFFTData = array [0..512] of Single;
  TBandOut = array[0..NumFFTBands-1] of word;
  TEQGains = array [0..NumEQBands-1] of float;  // -15.0 ~ +15.0
  TBandData = record
     CenterFreq : float;   // 80 ~ 16,000 (cannot exceed one-third of the sampling frequency)
     BandWidth  : float;   // Bandwidth, in semitones, in the range from 1 to 36
  end;
  TEQBands = record
     Bands : word;     // Number of equalizer bands (0 ~ 10)
     BandPara : array[0..NumEQBands-1] of TBandData;
  end;

  TMetaSyncParam = record
     MsgHandle : HWND;
     ChannelType : TChannelType;  // * Added at Ver 2.00
     TitleP : array[0..255] of ansichar;
  end;
  TLyricSyncParam = record
     MsgHandle : HWND;
     Channel : DWORD;
  end;
  TSlideSyncParam = record
     MsgHandle : HWND;
     NextAction : DWORD;
  end;
  TBASSAddOnInfo = record
     Handle : HPLUGIN;
     Name   : string;
     Version : DWORD;
     NumFormat  : DWORD;
     FormatP : PBASS_PLUGINFORMS;
  end;
  TBASSAddOnList = array[1..MaxLoadableAddons] of TBASSAddOnInfo;

  TMIDI_FONTINFO = record
     FontName : string;
     Copyright : string;
     Comment : string;
     Presets : DWORD;
     SampleSize : DWORD;
     SampleLoaded : DWORD;
  end;

  TMIDITrackInfo = record
     TrackCount : integer;
     TrackText : array[0..255] of string;
  end;

  TWinampGPPInfo = TGPPInfo;
 { TSubClass_Proc = function(lng_hWnd: HWND; uMsg: Integer;
                            var Msg: TMessage; var bHandled: Boolean) : boolean; }

  TNotifyEvent = procedure(Sender: TObject) of object;
  TNotifyEvent2 = procedure(Sender: TObject; GenParam : DWORD) of object;
  TNotifyNetEvent = procedure(Sender: TObject; Content : ansistring) of object;
  TNotifyMIDIEvent = procedure(Sender: TObject; TextP : pAnsiChar) of object;
  TNotifyVisEvent = procedure(Sender: TObject; VisWindowAttr : TVisPluginInfo) of object;
  TNotifyPluginEvent = procedure(Sender: TObject; ChannelInfo : PChannelInfo) of object;
  TNotifyNewFFTDataEvent = procedure(Sender: TObject; BandOut : TBandOut) of object;
  TNotifyModeChangeEvent = procedure(Sender: TObject; OldMode, NewMode : TPlayerMode) of object;

  TVisDataThread = class(TThread)
  private
    { Private declarations }
    DriveThreadId : DWORD;
    VisTimer : HWND;
    ModuledelayMs : DWORD;
 //   PseudoSuspended : boolean;

    FThreadReady : boolean;
    FShareMemPointer : pointer;
    FLatencyMs : DWORD;
    FDelayMS : DWORD;
    FDelayMSChanged : boolean;
    FspectrumNch : DWORD;
    FwaveformNch : DWORD;
    FPlayerMode : TPlayerMode;
    FChannelType : TChannelType;
    FD_ChannelId : DWORD;
    FP_ChannelId : DWORD;
    FSampleRate : DWORD;
    FChannels : DWORD;
    FVisScale : word;
    FDataReadyMsg : hwnd;

    SpectrumData: array[0..2047] of float;
    WaveformData: array[0..1152*MaxChannels - 1] of smallint;

    WaitCounter : integer;
    procedure DoOnVisTimer;
    procedure WinProcessMessages;  // * Added at Ver 2.00
  protected
    LockFlag : TCriticalSection;

    function  Ready : boolean;
    procedure SetDelayMS(delayMS : DWORD);
    procedure SetDriveThreadId(ThreadId : DWORD);
    procedure SetVisModuleInfo(ModuleInfo : TVisModuleInfo);

    procedure SetChannelInfo(ChannelType : TChannelType; D_ChannelId, P_ChannelId,
                             SampleRate, Channels : DWORD);
    function CurrentPosition : DWORD;
    procedure SetPlayerMode1(PlayerMode : TPlayerMode);

    procedure SetVisScale(VisScale : word);
  //  procedure SuspendVis;
    procedure ResumeVis(WaitTime : DWORD);
  //  function  IsSuspended : boolean;
    procedure Change_EMBEDWindow(EMBEDWindow : HWND);
    procedure Change_EMBEDSwitchMode(FEMBEDSwitchMode : TVisEMBEDSwitchMode);
    procedure SetUseOfDrawerForm(UseVisDrawer : boolean);
    procedure InformVisDriver(MsgId, DataLength : DWORD);
    procedure Execute; override;

  public
    constructor Create(DataReadyMsg : hwnd; var MemPointer : pointer);
    destructor  Destroy; override;

  end;

  TBASSPlayer = class(TComponent)
  private
    { Private declarations }
    AppHWND     : THandle;
    ParentHWND  : HWND;
    TimerFFT    : TTimer;
  //  TimerTitleBar : TTimer;  // * Added at Ver 2.00

    ChannelType   : TChannelType;
    DecodeChannel : DWORD;
    PlayChannel   : DWORD;
    NeedFlush     : boolean;

    BassChannelInfo : BASS_CHANNELINFO;
    MessageHandle : hwnd;

    APlugin : TPlugin;
    NowStarting : boolean;
    UsesOutputPlugin : boolean;
 // Some input plug-ins do not need output plug-in. i.e., some input plug-ins
 // sound out decoded data directly.
 // UsesOutputPlugin is used to know if input plug-in needs output plug-in.
 // Use this instead of input plug-in's UsesOutPlug property because some
 // input plug-ins report wrong UsesOutPlug value.

    BASSDLLLoaded : boolean;
    BassInfoParam : BASS_INFO;
    EQParam       : BASS_DX8_PARAMEQ;
    EchoParam     : BASS_DX8_ECHO;
    ReverbParam   : BASS_DX8_REVERB;
    FlangerParam  : BASS_DX8_FLANGER;
    FSoundEffects : TSoundEffects;

    HBASSWMA      : HPLUGIN;   // * Added at Ver 2.00
    HBASSAAC      : HPLUGIN;   // * Added at Ver 2.00
    HMixer        : HSTREAM;   // * Added at Ver 2.00

    HSlideSync    : HSYNC;     // * Added at Ver 2.01
    AttribSliding : boolean;   // * Added at Ver 2.01

 //   DSPHandle     : HDSP;
 //   fladspHandle  : HDSP;
    RotateDSPHandle : HDSP;
    EQHandle      : array [0..NumEQBands-1] of HFX;
    EchoHandle    : HFX;
    ReverbHandle  : HFX;
    FlangerHandle : HFX;

    BandOut: TBandOut;

    NetStream  : boolean;
    NetRadio   : boolean;
    ICYTag     : pAnsiChar;
    HTTPTag    : pAnsiChar;
    tmpHTTPTag : pAnsiChar;
    MetaSyncParam : TMetaSyncParam;
    LyricSyncParam : TLyricSyncParam;
    SlideSyncParam : TSlideSyncParam;   // * Added at Ver 2.01

    MPEG        : TMPEGaudio;
    Vorbis      : TOggVorbis;
    AAC         : TAACfile;
    WMA         : TWMAfile;
    WAV         : TWAVFile;

  //  MusicStartTime : DWORD;
  //  MusicPauseTime : DWORD;
  //  RestartPos : DWORD;

    FMBHandle  : HWND;  // Handle to Mini Browser window
    FMagneticSubProc : TSubClass_Proc;

  // variables for vis plug-in
    VisWindowHandle : HWND;
    FEmbedHandle : HWND;
    FEMBEDSwitchMode : TVisEMBEDSwitchMode;
    ShareMemPointer : pointer;
    GoVisOut : boolean;
    FVisScale : word;
  //  FSyncVisWindow : boolean;
    WaitingVisWindow : boolean;
    WaitingPrevVisStop : boolean;
    WaitingVisStop : boolean;
    VisPlugin_ : string;
    VismodNo_ : word;
    DataReadyMsg : hwnd;

    PluginConfigForm: TPluginConfigForm;
    MPEGFileInfoForm: TMPEGFileInfoForm;
    OggVorbisInfoForm : TOggVorbisInfoForm;
    WMAInfoForm : TWMAInfoForm;

    VisDataThread: TVisDataThread;

    FBASSReady      : boolean;
    FPluginReady    : boolean;
    FBASSWMAReady   : boolean;
    FBASSAACReady   : boolean;   // * Added at Ver 2.00
    FBASSCDReady    : boolean;
    FBASSMIDIReady  : boolean;
    FMIDISoundReady : boolean;
    FGPPluginReady  : boolean;
    FMiniBrowserReady : boolean;
    FUseVisDrawer   : boolean;

    FMixerReady     : boolean;   // * Added at Ver 2.00
    FMixerPlugged   : boolean;   // * Added at Ver 2.00
    FDownMixToStereo: boolean;   // * Added at Ver 2.00

    FSingleChannel  : boolean;
    FSingleChannel2 : boolean;
    FNumCDDrives    : integer;
    CDDriveList     : array[0..MAXCDDRIVES-1] of string;

    FPluginNum      : integer;
    FPluginFirst    : boolean;

    FVersionStr     : string;
    FDLLVersionStr  : string;
    FStreamName     : string;
    FDecodingByPlugin : boolean;
  //  FUsing_BASS_AAC : boolean;
    FDecoderName    : string;
    FDownLoaded     : boolean;
    FGetHTTPHeader  : boolean;
    FStreamInfo     : TStreamInfo;
    FSupportedBy    : TSupportedBy;    // * Added at Ver 2.00
    FVisWindowAttr  : TVisPluginInfo;
    BASSAddOnList   : TBASSAddOnList;

    FOrgWndProc     : pointer;

    defaultFontHandle : HSOUNDFONT;
    defaultMIDI_FONTINFO : TMIDI_FONTINFO;

    FOutputVolume   : DWORD;
    FMute           : boolean;
    FDX8EffectReady : boolean;
    FEchoLevel      : word;
    FReverbLevel    : word;
    FFlangerLevel   : word;
    FEQGains        : TEQGains;
    FEQBands        : TEQBands;
    FPlayerMode     : TPlayerMode;

    FOnPlayEnd         : TNotifyEvent;
    FOnPluginStartPlay : TNotifyPluginEvent;
    FOnGetChannelInfo  : TNotifyPluginEvent;
    FOnGetMeta         : TNotifyNetEvent;
    FOnDownloaded      : TNotifyEvent2;
    FOnNewFFTData      : TNotifyNewFFTDataEvent;
    FOnModeChange      : TNotifyModeChangeEvent;
    FOnGetLyric        : TNotifyMIDIEvent;
    FOnVisWindowShow   : TNotifyVisEvent;
    FOnPluginRequest   : TNotifyEvent2;
    FOnUpdatePlayList  : TNotifyEvent2;
    FOnMBWindowShow    : TNotifyEvent2;
    FOnGenWindowShow   : TNotifyEvent2;

    procedure ClearEffectHandle;
    function  GetStreamInfo2(StreamName : string;
                            var StreamInfo : TStreamInfo;
                            var SupportedBy : TSupportedBy;
                            var PluginNum : integer;
                            var PluginName : string) : boolean;
    procedure SetVersionStr(Value : string);
    procedure SetDLLVersionStr(Value : string);
    procedure SetDX8EffectReady(Value : boolean);
    procedure SetSingleChannel(Value : boolean);
    procedure SetVolume(Value : DWORD);
    procedure SetEchoLevel(Value : word);
    procedure SetReverbLevel(Value : word);
    procedure SetFlangerLevel(Value : word);
    procedure SetSoundEffect(Value : TSoundEffects);
    procedure SetEQGains(Value : TEQGains);
    procedure SetEQBands(Value : TEQBands);
    procedure SetPluginFirst(Value : boolean);
    procedure SetUseVisDrawer(Value : boolean);
    procedure SetVisEMBEDHandle(Value : HWND);
    procedure SetEMBEDSwitchMode(Value : TVisEMBEDSwitchMode);
  //  procedure SetSyncVisWindow(Value : boolean);
    procedure SetVisScale(Value : word);
    procedure SetPlayerMode(Mode : TPlayerMode);
    function  GetDLLVersionStr : string;
    function  GetNativeFileExts : string;
    function  GetPluginFileExts : string;
    function  IsSeekable : boolean;
    function  GetDownloadProgress : DWORD;
    procedure PausePlay;
    procedure ResumePlay;
    procedure Restart;
    procedure TimerFFTTimer(Sender: TObject);
 //   procedure TimerTitleBarTimer(Sender: TObject);  // * Added at Ver 2.00
    procedure ProcMessage(var Msg: TMessage);
    function  CurrentPosition : DWORD;
    procedure SetPosition(Value : DWORD);
    procedure InformPlayerStatus(StatusIndex : integer);
    procedure intQuitVisPlugin;
    procedure Error(msg: string);
    function  GetBASSAddonExts : string;
    function  GetDecoderName(ExtCode : string) : string;
    function  Run_VisPlugin(VisPlugin : string; VismodNo : word) : boolean;
    function  GetPlayListIndex : integer;
    procedure SetPlayListIndex(Value : integer);
    function  OpenURL(URL : string;
                             var StreamInfo : TStreamInfo;
                          //   var tmpChannel : DWORD;
                          //   var tmpChannelType : TChannelType;
                             var SupportedBy : TSupportedBy;
                             Temp_Paused : boolean;
                             CheckOnly : boolean) : boolean;
    procedure SetDownMixToStereo(Value : boolean);  // * Added at Ver 2.00
    function  MutedTemporary : boolean;             // * Added at Ver 2.01
    procedure RestoreFromMutedState;                // * Added at Ver 2.01
  //  function  GetGenWinHandles : TGenHandles;        // * Added at Ver 2.00.1
    function  GetBuildDate : string;
    procedure WinProcessMessages;   // * Added at Ver 2.00
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
      // Allocates memory and constructs an instance of TBASSPlayer.
      // This Create method should be executed first to use TBASSPlayer.
      // Parameter :
      //  - AOwner : an owner component, normally the main form of application

    destructor Destroy; override;
     // Disposes the TBASSPlayer component and its owned components.
     // Do not call Destroy directly. Call Free instead.  Free verifies that the component is not already
     // freed, and only then calls Destroy.

    function GetVisWindowHandle : HWND;
     // Gets the handle of vis window.
     // Gets 0 if vis plug-in is not running.

    function GetStreamInfo(StreamName: string; var StreamInfo: TStreamInfo; var SupportedBy: TSupportedBy)
                           : boolean;
     // Gets the information of a stream file.
     // Parameters :
     //  - StreamName : the file path of a stream file
     //  - StreamInfo : TStreamInfo record where the information of a stream file is given
     //  - SupportedBy : Gets one of following values
     //      Both : The stream file can be decoded by BASS or Winamp input plug-in.
     //             If property "PluginFirst" is true then the stream file will be decoded by Winamp
     //             input plug-in.
     //      BASSNative : The stream file can be decoded only by BASS.
     //      WinampPlugin : The stream file can be decoded only by Winamp input plug-in.
     //      None : The stream file can not be decoded. (may be an invalid stream file or an unsupported
     //             stream file)
     // Return value : True if valid stream file, else False.
     // note) This function is not available for URL streams.
     //       There may be unrecorded features in the stream file, some items of StreamInfo
     //       may be filled with default value.
     //       The default value is 0 for numeric item, null for string item.

    function Open(StreamName : string) : boolean;
      //  Prepares to play a local stream file, a MIDI file, a music file, a CD Audio file or an URL
      //   stream.
      //  Local MPx(MP1, MP2, MP3, MP4), AAC, OGG, WAV, WMA, ASF, MIDI(MID, RMI, KAR), AIFF, MO3, IT,
      //   XM, S3M, MTM, MOD, UMX and CD Audio files are decoded by BASS library if property "PluginFirst"
      //   is not true or appropriate Winamp input plug-in(s) is not loaded.
      //  You should load the appropriate BASS add-on(s) or Winamp input plug-in(s) prior to opening a
      //   local stream file or a local music file other than above file formats.
      //  TBASSPlayer can play MPx, AAC, OGG, WAV, MIDI, WMA and ASF files from internet and the
      //   stream from net radio such as Shoutcast/Icecast server.
      //  Parameter :
      //   - StreamName : file path, URL of a stream file from internet, URL of a net radio.
      //  Return value : True on success, False on failure.
      //  note) TBASSPlayer is programmed to load several BASS extension modules to support various kind of
      //        sound files and net streams metioned above, as follows,
      //         - BASSWMA.DLL : WMA, ASF files and the stream from WMA broadcaster
      //         - BASS_AAC.DLL : MP4, AAC files and AAC+ Shoutcast streams
      //         - BASSCD.DLL : CD Audio files
      //         - BASSMIDI.DLL : MIDI files
      //        So, you must load appropriate Winamp input plug-in if the required BASS extension module is
      //        not loaded.

    function PlayLength : DWORD;
      //  Gets the playback length of opened file in mili seconds.
      //  Gets 0 if there is no opened stream.

    function GetChannelFFTData(PFFTData : pointer; FFTFlag : DWORD) : boolean;
      //  Gets FFT data.
      //  TBASSPlayer provides you the processed FFT data (intensity per frequency band data) to help you
      //  to drive visual display.
      //  Use this function only if you need raw FFT data.
      //  Parameters :
      //   - PFFTData : Buffer pointer where FFT data are given.
      //   - FFTFlag : Number of samples to get FFT data. One of followings,
      //         BASS_DATA_FFT512 : 512 samples (returns 256 floating-point values)
      //         BASS_DATA_FFT1024 : 1024 samples (returns 512 floating-point values)
      //         BASS_DATA_FFT2048 : 2048 samples (returns 1024 floating-point values)
      //       in addition, you can add BASS_DATA_FFT_INDIVIDUAL for multi-channel streams.
      //  Return value : True on success, False on failure.

    procedure Play;
      //  Plays an opened stream.
      //  If TBASSPlayer is in paused state then resumes playing.
      //  If TBASSPlayer is in stopped state then restarts from the beginning.
      //  If there is no opened stream or TBASSPlayer is in playing state then
      //  it takes no effects.

    procedure Stop;
      //  Stops playing.
      //  If TBASSPlayer is in paused state(Mode=plmPaused) then TBASSPlayer switches its
      //  state to Stopped state(Mode=plmStopped).
      //  Else if TBASSPlayer is not playing a stream then it takes no effects.

    procedure Pause(pAction : boolean);
      //  Pauses or resumes playing.
      //  Parameters :
      //   - pAction : Set true if you want to pause palying, false to resume.

    procedure Close;
      //  Closes an opened stream.

    procedure SetMuteState(MuteState : boolean; FadeInOutMS : DWORD);  // ** New at Ver 2.01
      //  Mutes or restores from mute
      //  Parameters :
      //   - MuteState : Set true if you want to mute, false to restore from mute
      //   - FadeInOutMS : Fade-in, Fade-out period in mili seconds (min  : 0, max : 3000)
      //     note) If the value of FadeInOutMS is less than 50 then it is assumed 0.
      //            (= no fade-in, fade-out period).
      //           If the value of FadeInOutMS is greater than 3000 then it is assumed 3000.

    property Mute : boolean read FMute;
      //  Indicates mute state, true if sound output is mute, else false.

    function BASSAddonLoad(FilePath : string) : TBASSAddOnInfo;
      //  Plugs a BASS add-on into the standard stream and sample creation functions.
      //  Support for additional file formats are available via BASS add-ons, which can be downloaded
      //  from the BASS website: http://www.un4seen.com/
      //  Parameters :
      //   - FilePath : The file path of the BASS add-on to be loaded.
      //  Return value : The elements of TBASSAddOnInfo is filled with the information on the BASS add-on
      //                 loaded on success, the Handle element of TBASSAddOnInfo is 0 on failure.

    function BASSAddonFree(AddonHandle : HPLUGIN) : integer;
      //  Unplugs a BASS add-on or all BASS add-ons.
      //  Parameters :
      //   - AddonHandle : The handle to the BASS add-on to be unloaded or 0 for all BASS add-ons.
      //  Return value : The number of BASS add-ons released.

    function GetBASSAddonList : TBASSAddonList;
      //  Gets the BASSAddonList which holds the information on BASS add-ons plugged.
      //  Return value : BASSAddonList.

    function MIDIFontInit(FontFilePath : string;
                            var MIDI_FONTINFO : TMIDI_FONTINFO) : boolean;
      //  Initializes a MIDI soundfont which is used as default soundfont.
      //  If there is a previously initialized soundfont, it is freed, i.e, TBASSPlayer does
      //  not permit multiple soundfonts.
      //  Parameters :
      //   - FontFilePath : the file path of a SF2 soundfont.
      //  Return value : True on success, False on failure.
      //                 MIDI_FONTINFO holds information on the initialized soundfont
      //  note) You should initializes a SF2 sound font to play MIDI files using BASS audio
      //        library "bassmidi.dl".

    function MIDIGetTrackInfo(MIDIFilePath : string;
                              var MIDITrackInfo : TMIDITrackInfo) : boolean;
      //  Retreives track information on a given MIDI file.
      //  Parameters :
      //   - MIDIFilePath : the file path of a MIDI file.
      //  Return value : True on success, False on failure.
      //                 MIDITrackInfo holds track information.

   { function MIDIFontGetInfo : TMIDI_FONTINFO;
      //  Retrieves information on the initialized soundfont.
      //  Return value : MIDI_FONTINFO
      //  note) if there is no initialized soundfont then the value of SampleSize and SampleLoaded
      //         of MIDI_FONTINFO is 0. }

    procedure ShowPluginConfigForm;
      //  Shows "Input plug-in Configuration" form for Winamp input plug-ins.
      //  The "Input plug-in Configuration" form helps you to manage Winamp input plug-ins.
      //  You can load, unload and configure Winamp input plug-ins with it.


    function SetAEQGain(BandNum : integer; EQGain : float) : boolean;
      //  Sets the gain of a equalizer's band.
      //  Parameters :
      //   - BandNum : Band number. ( 0 ~ FEQBands.Bands - 1 )
      //   - EQGain : Gain of the designated equalizer's band. ( -15.0 ~ +15.0 )
      //  Return value : True on success, False on failure.

    property EQGains : TEQGains read FEQGains write SetEQGains;
      //  Specifies equalizer's gain of all bands.
      //  note) The equalizer is not effective if the property "DX8EffectReady" is not true(= the system
      //        does not support DirectX version 8 or higher).

    property EQBands : TEQBands read FEQBands write SetEQBands;
      //  Specifies band parameters of all bands.

    property PlayerReady : boolean read FBASSReady;
      //  Indicates whether BASS.DLL is operational.  It is true if BASS.DLL has been loaded and
      //  initialized successfully.
      //  If FBASSReady is false then none of functions or procedures to play a stream are available.

    property BASSWMAReady : boolean read FBASSWMAReady;
      //  Indicates whether BASSWMA.DLL is operational.  It is true if FBASSReady is true and BASSWMA.DLL
      //  has been loaded.
      //  If FBASSReady is true and FBASSWMAReady is false then you should load a Winamp input plug-in for
      //  WMA file(ex: in_WM.dll) to play WMA files.

    property BASSAACReady : boolean read FBASSAACReady;  // * New at Ver 2.00
      //  Indicates whether bass_aac.dll is operational.  It is true if FBASSReady is true and bass_aac.dll
      //  has been loaded.
      //  If FBASSReady is true and FBASSAACReady is false then you should load a Winamp input plug-in for
      //  AAC, M4A, MP4 files and AAC+ stream from net.

    property BASSCDReady : boolean read FBASSCDReady;
      //  Indicates whether BASSCD.DLL is operational.  It is true if FBASSReady is true and BASSCD.DLL
      //  has been loaded.
      //  If FBASSReady is true and FBASSCDReady is false then you should load a Winamp input plug-in for
      //  CD Audio file(ex: in_cdda.dll) to play CD Audio tracks.

    property BASSMIDIReady : boolean read FBASSMIDIReady;
      //  Indicates whether BASSMIDI.DLL is operational.  It is true if FBASSReady is true and BASSMIDI.DLL
      //  has been loaded.
      //  If FBASSReady is true and FBASSMIDIReady is false then you should load a Winamp input plug-in for
      //  MIDI file(ex: in_midi.dll) to play MIDI files.

    property MIDISoundReady : boolean read FMIDISoundReady;
      //  Indicates whether the functions to play MIDI files is operational or not.
      //  FMIDISoundReady is true when FBASSMIDIReady is true and a MIDI sound font is loaded.

    property VisDrawerReady : boolean read FGPPluginReady;
      //  Indicates that Winamp-like vis drawer (which is driven by a customized Winamp general purpose
      //  plug-in, VisDrawer.dll) is ready to support Winamp 5 visulization plug-ins.
      //  note) TBASSPlayer can use Winamp 5 visulization plug-ins regardless of the state of this
      //        property. But the shape of visualization window looks like a normal window if this
      //        property is false.
    property UseVisDrawer : boolean read FUseVisDrawer write SetUseVisDrawer;
      //  Specifies whether to use Winamp-like Vis drawer or not.
      //  If VisDrawerReady is false then this property is not effective.

  {  property NumCDDrives : integer read FNumCDDrives;
      // Indicates the number of CDROM drives installed.
      // note) NumCDDrives is available only if BASSCDReady is true. (Always will be zero if not)

    function CDDescription(DriveNum : integer) : string;
      // Gets "drive_letter: description" string for specified CDROM drive.
      //  (0 : 1st drive, 1 : 2nd drive ... )  }

    property DecodingByPlugin : boolean read FDecodingByPlugin;
      //  Indicates whether the opened stream is decoded by Winamp input plug-in.

    property DecoderName : string read FDecoderName;
      //  Indicates the name of decoder responsible for the currently opened stream such as
      //   "BASS_Native" : The Currently opened stream is decoded by BASS sound library.
      //   "in_(Any name).dll" : The Currently opened stream is decoded by a Winamp input
      //                           plug-in. (ex. in_WM.dll)
      //   "bass*.dll          : The Currently opened stream is decoded by a BASS audio library
      //   "bass_(Any name).dll" : The Currently opened stream is decoded by a BASS add-on.
      //                           (ex. bass_spx.dll)

    property Seekable : boolean read IsSeekable;
      //  Indicates whether the playback position is changeable.

    property StreamInfo : TStreamInfo read FStreamInfo;
      //  Gets features of an opened stream.
      //  note) Because there may be unrecorded features in an opened stream file, some items of StreamInfo
      //        may be filled with default value.
      //        The default value is 0 for numeric item, null for string item.

    property Mode : TPlayerMode read FPlayerMode;
      //  Indicates the state of TBASSPlayer, It can be one of followings.
      //   plmStandby : TBASSPlayer is in standby state. (= Not opened a stream to play yet)
      //   plmReady   : TBASSPlayer is ready to play a stream. (= Opened a stream to play)
      //   plmStopped : TBASSPlayer is in stopped state.
      //                Subsequent playback is started from the beginning.
      //   plmPlaying : TBASSPlayer is playing a stream.
      //   plmPaused  : TBASSPlayer is in paused state.
      //                Subsequent playback is started from the position paused.

    property Position : DWORD read CurrentPosition write SetPosition;
      //  Specifies the playback position in mili seconds.
      //  Altering Position, which causes to perform SetPosition, takes no effects if the opened stream
      //  is not seekable stream.

    property StreamPath : string read FStreamName;
      //  Indicates the file path of an opened stream

    property ChannelId : DWORD read DecodeChannel;
      //  Indicates the handle which is given at creating a sample stream by BASS.
      //  You can implement most of BASS functions in your own program if you know the handle of channel.

    property Volume : DWORD read FOutputVolume write SetVolume;
      //  Specifies the output volume.
      //  The output volume range is from 0 to 255.

    property SoundEffects : TSoundEffects read FSoundEffects write SetSoundEffect;
      //  Specifies sound effects to be applied.
      //  The 4 different sound effects can be applied according to the value of SoundEffects.
      //  ex) BASSPlayer1.SoundEffects := [Echo] + [Reverb];  {apply echo and reverb}

    property EchoLevel : word read FEchoLevel write SetEchoLevel;
      //  Specifies the echo level.
      //  The range of echo level is from 0 to 32.

    property ReverbLevel : word read FReverbLevel write SetReverbLevel;
      //  Specifies the reverb level.
      //  The range of reverb level is from 0 to 32.

    property FlangerLevel : word read FFlangerLevel write SetFlangerLevel;  // ** New at Ver 2.01
      //  Specifies the Flanger level.
      //  The range of flanger level is from 0 to 32.

    property CanUsePlugin : boolean read FPluginReady;
      //  Indicates whether TBASSPlayer is ready to use Winamp input plug-ins.

    property NativeFileExts : string read GetNativeFileExts;
      //  Reports playable file types which can be decoded by BASS or BASS extension moules,
      //  i.e., without Winamp input plug-ins and BASS add-ons.
      //  This property reflects the currently loaded BASS extension moules such as basscd.dll,
      //  basswma.dll, bassmidi.dll.

    property PluginFileExts : string read GetPluginFileExts;
      //  Reports the file types which can be decoded by Winamp input plug-ins.
      //  This PluginFileExts changes according to the currently loaded Winamp input plug-ins.
      //  If TBASSPlayer failed to get buffer memory to support Winamp input plug-ins then
      //  you will get null value regardless the loaded Winamp input plug-ins.

    property BASSAddonExts : string read GetBASSAddonExts;
      //  Reports the file types which can be decoded by BASS add-on plugins.
      //  This BASSAddonExts changes according to the currently loaded BASS add-on plug-ins.

    function IsValidURL(URL : string) : boolean;   // * New at Ver 2.01
      // Checks if the URL is a playable stream source.
      // Return value : True on success, False on failure.

    property IsNetStream : boolean read NetStream;
      //  Indicates whether the opened stream is a stream file from internet.

    property IsNetRadio : boolean read NetRadio;
      //  Indicates whether the opened stream comes from net radio station such as Shoutcast/
      //   Icecast server.

    property ICYInfo : pAnsiChar read ICYTag;
      //  Reports ICY (Shoutcast) tags. A pointer to a series of null-terminated strings is returned,
      //  the final string ending with a double null.
      //  ICYInfo will be nil if the opened stream has no ICY (Shoutcast) tags.

    property HTTPInfo : pAnsiChar read HTTPTag;
      //  Reports HTTP headers, only available when streaming from a HTTP server.
      //  A pointer to a series of null-terminated strings is returned, the final string ending with a
      //  double null.
      //  HTTPInfo will be nil if the opened stream is not from a HTTP server.

    property DownloadProgress : DWORD read GetDownloadProgress;
      //  Reports download progress of the opened stream file from internet, in bytes.

    function FileInfoBox(StreamName : string): Boolean;
      //  Shows dialog box which presents some information on a specified stream file.
      //  The MPEG, OGG Vorbis and WMA files are supported by internal code.
      //  You can also edit TAG of these files.
      //  For the stream files other than MPEG, OGG Vorbis and WMA files, this function is effective
      //  only if the specified stream file is supported by one of currently loaded Winamp input plug-ins.
      //  Return value : True on success, False on failure.
      //  note) This function is valid only for local stream files.

    property VisEMBEDHandle : HWND read FEMBEDHandle write SetVisEMBEDHandle;
      //  Specifies the handle to EMBED window on which vis window is created.
      //  note) If EMBEDHandle is set to 0 or EMBEDHandle is not a valid window handle,
      //        TBassPlayer will create a EMBED window(a seperated window) if it is needed.

    property VisEMBEDSwitchMode : TVisEMBEDSwitchMode read FEMBEDSwitchMode write SetEMBEDSwitchMode;
      //  Specifies the method of switching EMBED window for running vis plug-in.
      //  note) Use "NewStart" if you want reliable operation at display mode change (full screen
      //          -> windowed mode).
      //        Use "WindowMove" if you want to preserve current status of vis plug-in at switching
      //         EMBED window. This option shows some minor problem at display mode change (full screen
      //          -> windowed mode).

    function RunVisPlugin(VisPlugin : string; VismodNo : word) : boolean;  
      //  Runs a Winamp visualization plug-in.
      //  Parameters :
      //   - VisPlugin : File path to the vis plug-in to be run.
      //   - VismodNo : The number to select a module among the modules in a vis plug-in.
      //                (0 : the 1st module, 1 : 2nd module,.. N-1 : Nth module)
      //        note) A vis plug-in can have upto 8 modules.
      //  Return value : True on success, False on failure.
      //  note) If a vis plug-in is running, the running vis plug-in is stopped first,
      //        then specified vis plug-in is started.

    procedure QuitVisPlugin;
      //  Quits the running Winamp visualization plug-in.

    function RunDSPPlugin(DSPPlugin : string; DSPmodNo : word) : boolean;
      //  Runs a Winamp DSP plug-in.
      //  DSP effects using Winamp DSP plug-in is effective if the stream being played is decoded
      //  by Winamp input plug-in or TBASSPlayer operates in "dual channel mode".
      //  Parameters :
      //   - DSPPlugin : File path to the DSP plug-in to be run.
      //   - DSPmodNo : The number to select a module among the modules of a Winamp DSP plug-in.
      //                (0 : the 1st module, 1 : 2nd module,.. N-1 : Nth module)
      //        note) A Winamp DSP plug-in can have upto 8 modules.
      //   - MainWinHwnd : The handle of main window.
      //  Return value : True on success, False on failure.

    procedure QuitDSPPlugin;
      //  Quits the running Winamp DSP plug-in.

    procedure HideVisWindow;
      //  Hides the vis window.

    procedure ShowVisWindow;
      //  Shows or restores the vis window.

    function GetMessageHandle : HWND;   // debug purpose (to debug VisOut program)
      //  Gets the message handle of TBASSPlayer.


  // Followings are for Play List handling.
    procedure ClearPlayList;                               // * New at Ver 2.00
      // Clears the contents of Play List

    procedure AddToPlayList(FilePath, Title, Artist : string);   // * Changed at Ver 2.00.1
      // Adds an Item to Play List

    procedure DeleteFromPlayList(ListIndex : integer);     // * New at Ver 2.00
      // Deletes an Item from Play List

    procedure PlayListChangeTitle(FilePath : string; Title : string); // * New at Ver 2.00
      // Changes the title for the entry FilePath in Play List

    property PlayListIndex : integer read GetPlayListIndex write SetPlayListIndex;  // * New at Ver 2.00
      //  Specifies the index number of Play List

    function NumOfPlayListItem : integer;                 // * New at Ver 2.00
      // Gets the number of items in Play List

    function PlayListTitle(ListIndex : integer) : string;  // * New at Ver 2.00
      // Gets the Title of specified item in Play List

    function IndexOfPlayList(FilePath : string) : integer; // * New at Ver 2.00
      // Gets the index number of the item in Play List.
      // If (FilePath = '') then the current index number of Play List is returned,
      //  else the the index number of the item which holds matched FilePath is returned.

    property MixerReady : boolean read FMixerReady;        // * New at Ver 2.00
      //  Indicates whether BASSmix is ready to operation.

    property DownMixToStereo : boolean read FDownMixToStereo write SetDownMixToStereo;  // * New at Ver 2.00
      //  Determines whether the multi channel source data are to be downmixed to stereo.
      //  This property is applied from new stream to be opened.
      //  This property is valid only if the opened stream is decoded by BASS library(= not by
      //   Winamp input plug-in), BASSmix is ready to operational(MixerReady = true) and
      //   BASS is operating in Dual channel mode(SingleChannel = false).


  // Followings are for managing Winamp General Purpose Plug-ins
    function RunGPPModule(GPP_Path : string) : integer;  // * New at Ver 2.00
      // Runs a Winamp General Purpose Plug-in.
      //  Parameter :
      //   - GPP_Path : the file path of a Winamp General Purpose Plug-in to be launched.
      //  Return value : the entry number on success, -1 on failure.

   function RunAllGPPModules : integer;  // * New at Ver 2.00
      // Runs all Winamp General Purpose Plug-in.
      //   note) The Winamp General Purpose Plug-ins to be run should be placed in
      //          <program_directory>\Plugins directory.
      //         Use only once at starting program to prevent unexpected problem.
      //  Return value : the number of Winamp General Purpose Plug-ins launched

    procedure QuitGPPModule(GPPRegNo : integer);  // * New at Ver 2.00
      // Quits a Winamp General Purpose Plug-in.
      //  Parameter :
      //   - GPPRegNo : the entry number of Winamp General Purpose Plug-in.

    function NumberLoadedGPP : integer;               // * New at Ver 2.00
      // Gets the number of loaded Winamp General Purpose Plug-in.

    function GetLoadedGPPInfo(index : integer; indexByEntryNo : boolean) : TGPPInfo; // * New at Ver 2.00
      // Gets the information on a specified Winamp General Purpose Plug-in.
      //  Parameter :
      //   - index : the entry number or the order number(1 ~ n, n = the number of loaded plug-ins)
      //             in managing table.
      //   - indexByEntryNo : index means entry number on true, order number on false.
      //  Return value : if index points a invalid entry then TGPPInfo.Handle gets zero.

    property MBWinHandle : HWND read FMBHandle;      // ** New at 2.00.1
      // Indicates handle to the Mini Browser window
      //  note) Some GPP requires Mini Browser.

    function MagneticWindowAdd(WndHandle, HWndParent : HWND; var SubClass_Proc : TSubClass_Proc): Boolean;  // * New at Ver 2.00.3
      // Register the specified window to be endowed with magnectic effect.
      //  Parameters :
      //   - WndHandle : Handle to the window to be endowed with magnectic effect.
      //   - HWndParent : Handle to the Parent window of the window of WndHandle.
      //                  Parent window drags its child windows snapped to it when it is moving.
      //                 HwndParent can be 0 for parent window or for having no parent window.
      //   - SubClass_Proc : function pointer to get address of the function which hands over the
      //                 window messages to be sub classed to actual message handler in MBDrawer.dll.
      //                 MBDrawer.dll puts valid address on success, nil on failure.
      //  Return value : True on success, False on failure.

    function MagneticWindowRemove(Handle: HWND): Boolean;    // * New at Ver 2.00.3
      // Unregister the specified window to be endowed with magnectic effect.
      //  Parameter :
      //   - WndHandle : Handle to the window to be unregistered.
      //  Return value : True on success, False on failure.
      
    procedure LetMBNavigate(URL : pAnsiChar);         // * New at Ver 2.00.2
      // Forces on Mini Browser to navigate specified URL.

    procedure LetMBNavigate2(Artist, Title : string; UseCixURL : boolean); // * New at Ver 2.01
      // Forces on Mini Browser to navigate lytrics for given Artist & Title
      //  Parameter :
      //   - UseCixURL : Searchs "http://www.cix.co.kr/" on true,
      //                 "http://www.lyricsplugin.com/" on false.

    procedure ShowMBForm(Loc_X, Loc_Y, Width, Height : integer);  // * New at Ver 2.00.3
      // Shows Mini Browser at specified position and size.
      //  Parameter :
      //   - Loc_X, Loc_Y : Position of Mini Browser (Loc_X : Left, Loc_Y : top)
      //   - Width, Height : Size of Mini Browser

    function GPPDrawerInfo(FormNo : integer) : TGenDrawerInfo;    // * New at Ver 2.00.3
      // Gets information on a EMBED window created for Winamp GPP
      //  Parameter :
      //   - FormNo : The entry number of the EMBED window for Winamp GPP to be queried
      //  Return value : each element of TGenDrawerInfo is filled with registered value.

    function GetLyrics(Artist, Title : string; CheckTitleMatch : boolean;
                       var pLyrics : pchar; var lLyrics : integer) : boolean;    // * New at Ver 2.00.3
      // Gets Lyrics for a specified title
      //  Parameter :
      //   - Artist, Title : Artist and Title for searching lyrics
      //   - CheckTitleMatch : if true then only the Lyrics it's title is identical to
      //      searching title is accepted.
      //      (not case sensitive, ignores spaces, ignores trailing dot(".") )
      //   - pLyrics : gets buffer address where Lyrics text is placed
      //   - lLyrics : gets the length of Lyrics text
      //  note) The lyrics is pasted at edit box of new started notepad.
      // Return value : True on success, False on failure.

    property BuildDateStr : string read GetBuildDate;          // * New at Ver 2.01
      //  Indicates the Build date of BASS.DLL in string.

  published
    { Published declarations }
    property Version : string read FVersionStr write SetVersionStr;
      //  Indicates the version of TBASSPlayer in string.
      //  note) Altering Version, which causes to perform SetVersionStr, takes no effects.
      //        SetVersionStr is needed only to show FVersionStr at form design.

    property BASSDLLVer : string read FDLLVersionStr write SetDLLVersionStr;
      //  Indicates the version of BASS.DLL in string.
      //  note) Altering BASSDLLVer, which causes to perform SetDLLVersionStr, takes no effects.
      //        SetDLLVersionStr is needed only to show FDLLVersionStr at form design.

    property DX8EffectReady : boolean read FDX8EffectReady write SetDX8EffectReady;
      //  Indicates whether TBASSPlayer is ready to use sound effects which are supported
      //  by Direct-X.
      //  It is set True if the DirectX version 8 or higher is properly installed in your system.
      //  note) Altering DX8EffectReady, which causes to perform SetDX8EffectReady, takes no effects.
      //        SetDX8EffectReady is needed only to show FDX8EffectReady at form design.

    property SingleChannel : boolean read FSingleChannel2 write SetSingleChannel;
      //  At reading - Indicates the operation mode for currently opened stream.
      //  At writing - Specifies the operation mode for opening stream.
      //  This property is valid only for the streams being decoded by BASS.
      //  Set for "single channel mode", reset for "dual channel mode"(= default mode).
      //  If this property is set then DSP effects using Winamp DSP plug-in is not applied.
      //  If the opened stream is to be decoded by Winamp input plug-in then DSP effects using
      //  Winamp DSP plug-in is applied regardless the status of this property.
      //  This property does not take effects on already opened stream, i.e., the operation mode of
      //  TBASSPlayer is determined at the time a stream is opened.

    property PluginFirst : boolean read FPluginFirst write SetPluginFirst;
      //  Specifies the priority of decoder which takes in charge of decoding the stream to be played.
      //  This property is effective only if the opened stream file can be decoded by both BASS and
      //  Winamp input plug-in.
      //  Set True if you want to use Winamp plug-ins for the file types that can be decoded by both
      //  BASS and Winamp input plug-in.
      //  ex) The MP3 files can be played without using Winamp input plug-in.
      //      If you want to play a MP3 file using Winamp input plug-in then load appropriate plug-in
      //      such as in_MP3.dll and set "PluginFirst" True prior to opening the MP3 file.
      //      In this case the decoding process is performed by Winamp input plug-in,
      //      but other processes except decoding process are still performed by BASS.

    property VisScale : word read FVisScale write SetVisScale;
      //  Specifies the scale factor to adjust output level of FFT data. (min. 256 ~ max. 1024)
      //  The original FFT data obtained by BASS is multiplied by this scale factor.
      //  note) The original FFT data are floating-point values ranging from 0 to 1.
      //        Winamp visualization plug-in uses BYTE array to accept FFT data.
      //        So minimum VisScale should be 256.

  //  property SyncVisWindow : boolean read FSyncVisWindow write SetSyncVisWindow;  // Obsoleted at Ver 1.91
      //  Determines whether TBASSPlayer should synchronize the owner's window and
      //  the vis window.
      //  If you set this property True then vis window will be hidden when main window is
      //  hidden, and will be shown(or restored) when main window is shown(or restored).

    property OnNewFFTData : TNotifyNewFFTDataEvent read FOnNewFFTData write FOnNewFFTData;
      //  Occurs when new FFT data is placed.
      //  This event does not occur while vis plug-in is active. 

    property OnPluginStartPlay : TNotifyPluginEvent read FOnPluginStartPlay write FOnPluginStartPlay;
      //  Occurs when playback of a stream, which is decoded by Winamp input plug-in, is started.

    property OnGetChannelInfo : TNotifyPluginEvent read FOnGetChannelInfo write FOnGetChannelInfo;
      //  Occurs when the basic characteristics(the number of channels, bit rate and sample rate)
      //  of a stream to be played are received from Winamp input plug-in.
      //  This event also occurs when one of the basic characteristics of a stream
      //  being played is changed.
      //  note) You cannot get the basic characteristics of a stream at opening the stream
      //        if the stream file type is not one of WAV, MPx, OGG or WMA.
      //        If the opened stream is decoded by Winamp input plug-in then the basic characteristics
      //        of the stream are catched by Winamp input plug-in at just before starting playback.
      //        So you can get the basic characteristics of a stream to be played at this event.

    property OnGetMeta : TNotifyNetEvent read FOnGetMeta write FOnGetMeta;
      //  Occurs when metadata is received in a Shoutcast stream.

    property OnDownloaded : TNotifyEvent2 read FOnDownloaded write FOnDownloaded;
      //  Occurs when downloading of a stream file from internet is done.
      //  note) The downloading process is automatically started when you play an URL stream.
      //        This OnDownloaded event does not occur if the stream file from internet is played
      //        in buffered mode(= download and play in smaller chunks).

    property OnPlayEnd : TNotifyEvent read FOnPlayEnd write FOnPlayEnd;
      //  Occurs when stream reaches the end.

    property OnModeChange : TNotifyModeChangeEvent read FOnModeChange write FOnModeChange;
      //  Occurs when player's state is changed.

    property OnGetLyric : TNotifyMIDIEvent read FOnGetLyric write FOnGetLyric;
      //  Occurs when a lyric event is encountered.

    property OnVisWindowShow : TNotifyVisEvent read FOnVisWindowShow write FOnVisWindowShow;
      //  Occurs at showing up or closing of vis window.

    property OnPluginRequest : TNotifyEvent2 read FOnPluginRequest write FOnPluginRequest;
      //  Occurs at receiving a request from Winamp plug-in such as volume up/down, seek forward/backward.
      // ( e.g. user pressed a specified control key when vis window is the focused window and
      //   the vis plug-in in use transfers such activities to it's parent window. )
      //  The activities defined are as follows.
      //    Volume up(REQ_VOLUMEUP), Volume down(REQ_VOLUMEDOWN)
      //    Seek forward(REQ_FFWD5S), Seek backward(REQ_REW5S)
      //    Previous title(REQ_PREV), Next title(REQ_NEXT)
      //    Play(REQ_PLAY), Pause(REQ_PAUSE), Stop(REQ_STOP)

    property OnUpdatePlayList : TNotifyEvent2 read FOnUpdatePlayList write FOnUpdatePlayList; // * New at Ver 2.00
      // Occurs at receiving request for changing of play list from vis plug-in.

    property OnMBWindowShow : TNotifyEvent2 read FOnMBWindowShow write FOnMBWindowShow;  // * New at Ver 2.00
      //  Occurs at Showing up or Closing Mini Browser window.

    property OnGenWindowShow : TNotifyEvent2 read FOnGenWindowShow write FOnGenWindowShow;  // * New at Ver 2.00.1
      //  Occurs at Showing up or Closing the window for Winamp GPP.

  end;


 // A Window API function "SetWaitableTimer" may not be executed normally if your
 // program is compiled with old Delphi versions (V4 and may be V5).
 // Because one of it's parameters is incorrectly declared in Delphi RTL.
 // ("lpDueTime" is declared as const instead of var)
 // So, I decided to use an alternate wrapper function to avoid this RTL bug.
  function MySetWaitableTimer(hTimer: THandle; var lpDueTime: TLargeInteger;
                              lPeriod: Longint; pfnCompletionRoutine: TFNTimerAPCRoutine;
                              lpArgToCompletionRoutine: Pointer; fResume: BOOL): BOOL;
                              stdcall; external 'kernel32.dll' name 'SetWaitableTimer';

  procedure Register;


implementation


const
   MajorVer = '2';
   MinorVer = '10';
   RevVer = '0';
   BuildDate = '14 May 2009';

   NoDX8Msg : pchar = 'DirectX Ver 8 or higher is not installed.' + #13#10 +
                      'Sound effects except rotate are disabled.';
   StreamFileExt = '*.wav;*.mp1;*.mp2;*.mp3;*.ogg;*.aiff';
   MIDIFileExt = '*.MID;*.MIDI;*.RMI;*.KAR;';
   MusicFileExt = '*.MO3;*.IT;*.XM;*.S3M;*.MTM;*.MOD;*.UMX;';

 // Following 2 constants are for stable operation of visualization 
   InitDelay = 1000;  // Delay time in ms for starting rendering after showing up vis window
   RestartDelay = 600; // Delay time in ms for restarting rendering

var
   msgs_org : array of string;
   msgs_local : array of String;
   msgCount : integer = 0;
   typeStr : pchar;
   Entries_Msg_Str : integer;
   CheckedLocalString : boolean = false;

procedure ShowErrorMsgBox(ErrorStr : string);  // * Modified at Ver 2.00
var
   F: TextFile;
   SearchRec: TSearchRec;
   MyEntry : boolean;
   CommonEntry : boolean;
   S : string;
   tmpStr : string;
   localStr : string;
   i : integer;
   SepPos : integer;
   ErrorStr_ : string;

begin
  if not CheckedLocalString then
  begin
    CheckedLocalString := true;
    if FindFirst(ExtractFilePath(ParamStr(0)) + 'lang_*.txt', faAnyFile, SearchRec) = 0 then
    begin

      SetLength(msgs_org, 16);
      SetLength(msgs_local, 16);
      Entries_Msg_Str := 16;
      MyEntry := false;
      CommonEntry := false;

      AssignFile(F, ExtractFilePath(ParamStr(0)) + SearchRec.Name);
      Reset(F);
      FindClose(SearchRec);

      while not Eof(F) do
      begin
        Readln(F, S);
        S := trim(S);

        if S = '' then
           continue;
        if copy(S, 1, 2) = '//' then
           continue;

        if (S[1] = '[') and (S[length(S)] = ']') then
        begin
           tmpStr := copy(S, 2, length(S) - 2);
           if uppercase(tmpStr) = 'TBASSPLAYER' then
           begin
              MyEntry := true;
              CommonEntry := false;
              continue;
           end else if uppercase(tmpStr) = 'COMMON' then
           begin
              MyEntry := false;
              CommonEntry := true;
              continue;
           end else if MyEntry then
              break
           else begin
              MyEntry := false;
              CommonEntry := false;
              continue;
           end
        end
        else if (not MyEntry) and (not CommonEntry) then
           continue;

        if ((S[1] = '&') or (S[1] = '$') or (S[1] = '*') or (S[1] = '^')) then
           continue;

      // Store message strings (format : Original=Local)
        SepPos := pos('=', S);
        if SepPos = 0 then
           continue;

        localStr := trim(copy(S, SepPos + 1, length(S) - SepPos));
        if localStr = '' then
           continue;
        if localStr[1] = '"' then
           localStr := copy(localStr, 2, length(localStr) - 1);
        if localStr[length(localStr)] = '"' then
           localStr := copy(localStr, 1, length(localStr) - 1);
        if localStr = '' then
           continue;

        msgs_org[msgCount] := trim(copy(S, 1, SepPos - 1));  // original message string
        msgs_local[msgCount] := localStr;
        inc(msgCount);
        if msgCount = Entries_Msg_Str then
        begin
           Entries_Msg_Str := Entries_Msg_Str + 16;
           SetLength(msgs_org, Entries_Msg_Str);
           SetLength(msgs_local, Entries_Msg_Str);
        end;
      end;

      CloseFile(F);
      SetLength(msgs_org, msgCount);
      SetLength(msgs_local, msgCount);

    end else
      FindClose(SearchRec);

  // following sentences are to get the local text for the title of messagebox
    typeStr := 'Error';
    tmpStr := string(typeStr);
    if msgCount > 0 then
    begin
       for i := 1 to msgCount do
       begin
         if msgs_org[i - 1] = tmpStr then
         begin
            typeStr := pChar(msgs_local[i - 1]);
            break;
         end;
       end;
    end;
  end;

   localStr := ErrorStr;

   if msgCount > 0 then
   begin
   // I programmed to apply the localization for only the 1st line of message string.
   // (=> message string may include variable contents in it, ex : file name to open )
      SepPos := pos(chr(10), ErrorStr);
      if SepPos > 0 then
         ErrorStr_ := copy(ErrorStr, 1, SepPos - 1)
      else
         ErrorStr_ := ErrorStr;

      for i := 1 to msgCount do
      begin
         if ErrorStr_ = msgs_org[i - 1] then
         begin
            localStr := msgs_local[i - 1];
            if SepPos > 0 then
               localStr := localStr + chr(10) + copy(ErrorStr, SepPos + 1, length(ErrorStr) - Seppos);
            break;
         end;
      end;
   end;

   Application.MessageBox(pChar(localStr), typeStr, MB_OK + MB_ICONERROR);
end;


// ------------------------------ Class TVisDataThread ------------------------------
// The instance of TVisDataThread takes charge of rendering fourier trandformed sound
// data to the driver of Winamp vis plug-in.
// This instance of TVisDataThread is created at the creation stage of an instance of
// TBassPlayer but operates as a seperate thread.

constructor TVisDataThread.Create(DataReadyMsg : hwnd; var MemPointer : pointer);
begin
   DriveThreadId := 0;

   FShareMemPointer := GetBufferAddress;
   MemPointer := FShareMemPointer;
   VisTimer := CreateWaitableTimer(nil, false, 'VisTimer');
   if VisTimer <> 0 then
   begin
      FDataReadyMsg := DataReadyMsg;
      FDelayMS := 0;
      ModuledelayMs := 0;
      FDelayMSChanged := false;
      FThreadReady := true;
   end;

   LockFlag := TCriticalSection.Create;

   inherited Create(true);
end;

destructor  TVisDataThread.Destroy;
begin
   if VisTimer <> 0 then
   begin
    // Error occurs if TVisDataThread is terminated during suspended state.
      if not Suspended then
         Terminate;

      CancelWaitableTimer(VisTimer);
      CloseHandle(VisTimer);
   end;

   LockFlag.Free;

   inherited Destroy;
end;

function TVisDataThread.Ready : boolean;
begin
   result := FThreadReady;
end;

procedure TVisDataThread.Execute;
const
   _SECOND = 10000000;
var
   qwDueTime : int64;
   liDueTime : _LARGE_INTEGER;
   ErrMsg : string;
begin
   if VisTimer = 0 then
      exit;

  // Create a negative 64-bit integer that will be used to
  // signal the timer 1/4 seconds from now.
    qwDueTime := -1 * (_SECOND div 4);

  // Copy the relative time into a LARGE_INTEGER.
   liDueTime.LowPart  := DWORD(qwDueTime and $FFFFFFFF);
   liDueTime.HighPart := longint(qwDueTime shr 32);
   if MySetWaitableTimer(VisTimer,	// handle to a timer object
                       TLargeInteger(liDueTime),	// when timer will become signaled
                       FDelayMS,	// periodic timer interval
                       nil,	// pointer to the completion routine
                       nil,	// data passed to the completion routine
                       false{flag for resume state}) then
   // Following sentences are repeated every FDelayMS interval all the time from initial
   //  start up to program end.
      repeat
       // We need to re-adjust timer interval according to the parameter "DelayMS" of vis plug-in.
       //  (in case we exchange vis plug-ins)
         if FDelayMSChanged then
         begin
           FDelayMSChanged := false;
           CancelWaitableTimer(VisTimer);
           MySetWaitableTimer(VisTimer,	TLargeInteger(liDueTime), FDelayMS, nil,
                            nil, false);
         end;

         if WaitForSingleObject(VisTimer, 1000{1sec}) = WAIT_OBJECT_0 then
         begin
         //  if not PseudoSuspended then
              DoOnVisTimer
         end else
           Terminate
      until Terminated
   else begin
      ErrMsg := SysErrorMessage(GetLastError);
      ShowErrorMsgBox(ErrMsg);
      Terminate;
   end;
end;

procedure TVisDataThread.DoOnVisTimer;
// * Changed at Ver 2.00
// Modified to prevent to put the value greater than 255 in SpectrumData[x]
var
   i : word;
   p1, p3 : ^DWORD;
   p2 : PBYTE;
   BytesValid : DWORD;
   tmpVal : int64;
begin
   if BASS_ChannelIsActive(FP_ChannelId) <> BASS_ACTIVE_PLAYING then
      exit;

   BytesValid := Bass_ChannelGetdata(FP_ChannelId, nil, BASS_DATA_AVAILABLE);
   if BytesValid < (2048 * FChannels) then
      exit;

   p1 := FShareMemPointer;
   inc(p1, 70); // Flag information is stored at byte offset 280 ~ 283

 // The flag is to be zero when rendering is completed.
 // But there may be some cases that won't be zero. (ex : moving vis window.)
   if p1^ <> 0 then
   begin
      WaitCounter := WaitCounter + 1;
      if WaitCounter < 20 then
         exit;

      if DriveThreadId <> 0 then
         PostThreadMessage(DriveThreadId, FDataReadyMsg, RequestRestFlag, 0);
      WaitCounter := 0;
      exit;
   end;

   p3 := p1;
   inc(p1, 1);
   p1^ := FSampleRate;
   inc(p1, 1);

 // Set number of channels to 2 for multi channel streams.
   if FChannels > 2 then
      p1^ := 2
   else
      p1^ := FChannels;
   inc(p1, 1);
 //  p1^ := FPosition;
   p1^ := CurrentPosition;
   inc(p1, 1);
   p2 := pointer(p1);

   if FspectrumNch > 0 then
   begin
   // Perform combined FFT if the value "spectrumNch" of vis is 1.
     if FspectrumNch = 1 then
     begin
        Bass_ChannelGetdata(FP_ChannelId, @SpectrumData, BASS_DATA_FFT2048);

        for i := 1 to 576 do
        begin
          tmpVal := trunc(SpectrumData[i] * FVisScale);
          if tmpVal > 255 then
             p2^ := 255
          else
             p2^ := tmpVal;
          inc(p2);
        end;
     end else  // FspectrumNch = 2
     begin
   // Perform combined FFT to reduce processing time for multi channel streams.
        if (FChannels = 1) or (FChannels > 2) then
        begin
           Bass_ChannelGetdata(FP_ChannelId, @SpectrumData, BASS_DATA_FFT2048);

           for i := 1 to 576 do
           begin
             tmpVal := trunc(SpectrumData[i] * FVisScale);
             if tmpVal > 255 then
                p2^ := 255
             else
                p2^ := tmpVal;
             inc(p2);
           end;

           for i := 1 to 576 do
           begin
             tmpVal := trunc(SpectrumData[i] * FVisScale);
             if tmpVal > 255 then
                p2^ := 255
             else
                p2^ := tmpVal;
             inc(p2);
           end;
        end else  // if FChannels = 2 then
        begin
     // Add BASS_DATA_FFT_INDIVIDUAL flag for seperate FFT per each channel
           Bass_ChannelGetdata(FP_ChannelId, @SpectrumData, BASS_DATA_FFT2048 + BASS_DATA_FFT_INDIVIDUAL);

           for i := 1 to 576 do
           begin
             tmpVal := trunc(SpectrumData[i*2] * FVisScale);
             if tmpVal > 255 then
                p2^ := 255
             else
                p2^ := tmpVal;
             inc(p2);
           end;

           for i := 1 to 576 do
           begin
             tmpVal := trunc(SpectrumData[i*2+1] * FVisScale);
             if tmpVal > 255 then
                p2^ := 255
             else
                p2^ := tmpVal;
             inc(p2);
           end;
        end;
     end;
   end;

   if FwaveformNch > 0 then
   begin
     if FChannels = 1 then
     begin
       Bass_ChannelGetdata(FP_ChannelId, @WaveformData, 1152);

       for i := 0 to 575 do
       begin
     //  p2^ := waveformdata[i] div 256;
         p2^ := Hi(WaveformData[i]);
         inc(p2);
       end;

       if FwaveformNch > 1 then
          for i := 0 to 575 do
          begin
        //  p2^ := waveformdata[i] div 256;
            p2^ := Hi(WaveformData[i]);
            inc(p2);
          end;
     end else if FChannels <= MaxChannels then
     begin
        Bass_ChannelGetdata(FP_ChannelId, @WaveformData, 1152 * FChannels);

        for i := 0 to 575 do
        begin
      //  p2^ := waveformdata[i*2] div 256;
          p2^ := Hi(WaveformData[i * FChannels]);
          inc(p2);
        end;

        if FwaveformNch > 1 then
           for i := 0 to 575 do
           begin
         //  p2^ := waveformdata[i*2+1] div 256;
             p2^ := Hi(WaveformData[i * FChannels + 1]);
             inc(p2);
           end;
     end;
   end;

 //  LockFlag.Acquire; // lock out other threads
 //  try
     p3^ := 1;    // Set flag to notify that new data is written.
 //  finally
 //    LockFlag.Release;
 //  end;

   if DriveThreadId <> 0 then
     PostThreadMessage(DriveThreadId, FDataReadyMsg, DataReady, 0);
end;

procedure TVisDataThread.WinProcessMessages;   // * Added at Ver 2.00
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

procedure TVisDataThread.SetDelayMS(delayMS : DWORD);
var
   tmpDelayMS : DWORD;
begin
   tmpDelayMS := delayMs;
   if tmpDelayMS < 10 then
      tmpDelayMS := 10;
   if tmpDelayMS <> FDelayMS then
   begin
      if FDelayMS <> 0 then     // if not initial start up
         FDelayMSChanged := true;
      FDelayMS := tmpDelayMS
   end;
end;

procedure TVisDataThread.SetDriveThreadId(ThreadId : DWORD);
begin
   DriveThreadId := ThreadId;
end;

procedure TVisDataThread.SetVisModuleInfo(ModuleInfo : TVisModuleInfo);
begin
   FLatencyMS := ModuleInfo.latencyMs;
   FspectrumNch := ModuleInfo.spectrumNch;
   FwaveformNch := ModuleInfo.waveformNch;
   ModuledelayMs := ModuleInfo.delayMs;
   SetDelayMS(ModuledelayMs);
end;

procedure TVisDataThread.SetChannelInfo(ChannelType : TChannelType; D_ChannelId, P_ChannelId,
                                        SampleRate, Channels : DWORD);
begin
   FChannelType := ChannelType;
   FD_ChannelId := D_ChannelId;
   FP_ChannelId := P_ChannelId;
   FSampleRate := SampleRate;
   FChannels := Channels;
end;

function TVisDataThread.CurrentPosition : DWORD;  // get playback position in mili seconds
var
   SongPos : int64;
   FloatPos : FLOAT;
begin
   case FChannelType of
      Channel_NotOpened : SongPos := 0;
      Channel_Stream..Channel_MIDI : begin
                          if (FD_ChannelId = FP_ChannelId) then  // Single channel mode ?
                             SongPos := BASS_ChannelGetPosition(FD_ChannelId, BASS_POS_BYTE)  // * Changed at 1.9
                          else
                             SongPos := BASS_ChannelGetPosition(FD_ChannelId, BASS_POS_BYTE) - DataInBuffer;   // * Changed at 1.9
                       end;
      Channel_Plugin : begin
                          if FPlayerMode = plmReady then
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

procedure TVisDataThread.SetPlayerMode1(PlayerMode : TPlayerMode);
begin
   FPlayerMode := PlayerMode;
end;

procedure TVisDataThread.SetVisScale(VisScale : word);
begin
   FVisScale := VisScale;
end;

procedure TVisDataThread.InformVisDriver(MsgId, DataLength : DWORD);
begin
   if DriveThreadId <> 0 then
      PostThreadMessage(DriveThreadId, FDataReadyMsg, MsgId, DataLength);
end;

// Put TVisDataThread lazy state(i.e., long timer interval) instead of
//  suspending it, when vis plug-in is not operational.
{procedure TVisDataThread.SuspendVis;
begin
   PseudoSuspended := true;
   SetDelayMS(100);  // Timer interval while vis plug-in is not operational = 100ms
   Suspend;
end; }

procedure TVisDataThread.ResumeVis(WaitTime : DWORD);
var
   p1 : PDWORD;
   WaitCycle, loopCounter : integer;
begin
   WaitCounter := 0;
   p1 := FShareMemPointer;
   inc(p1, 70);

 //  LockFlag.Acquire; // lock out other threads
 //  try
     p1^ := 0;    // Reset flag to enable rendering.
 //  finally
 //    LockFlag.Release;
 //  end;

 // Added wait cycle before starting rendering to provide vis plug-in with extra time margin.
   if WaitTime <> 0 then
      if WaitTime > 20 then
      begin
         WaitCycle := WaitTime div 20;
         loopCounter := 0;
         repeat
            sleep(20);
            WinProcessMessages;
            inc(loopCounter);
         until (loopCounter = WaitCycle);
      end else
        Sleep(WaitTime);

   if Suspended then
      Resume;
end;

procedure TVisDataThread.Change_EMBEDWindow(EMBEDWindow : HWND);
begin
   if DriveThreadId <> 0 then
      PostThreadMessage(DriveThreadId, FDataReadyMsg, ChangeEmbedWindow, EMBEDWindow);
end;

procedure TVisDataThread.Change_EMBEDSwitchMode(FEMBEDSwitchMode : TVisEMBEDSwitchMode);
begin
   if DriveThreadId <> 0 then
      PostThreadMessage(DriveThreadId, FDataReadyMsg, ChangeEMBEDSwitchMode, ord(FEMBEDSwitchMode));
end;

procedure TVisDataThread.SetUseOfDrawerForm(UseVisDrawer : boolean);
begin
   if DriveThreadId <> 0 then
      if UseVisDrawer then
         PostThreadMessage(DriveThreadId, FDataReadyMsg, UseVisDrawerForm, 1)
      else
         PostThreadMessage(DriveThreadId, FDataReadyMsg, UseVisDrawerForm, 0);
end;

{function  TVisDataThread.IsSuspended : boolean;
begin
   result := PseudoSuspended;
end; }

// --------------------------- end of TVisDataThread ----------------------------


// -------------------------------- Event Handlers ------------------------------

// This procedure is called when metadata are received in a Shoutcast stream.
procedure MetaSync(SyncHandle : HSYNC; Channel, data : DWORD; user : pointer); stdcall;
var
   TagP : pAnsiChar;
   tmpStr, StreamTitle : ansistring;
   TitlePos, DelimeterPos : integer;
   PMetaSyncParam : ^TMetaSyncParam;
   tmpPChar : pAnsiChar;
   tmpStr2 : WideString;
begin
   PMetaSyncParam := user;
   StreamTitle := '';
 //  TagP := pAnsiChar(data);
   if PMetaSyncParam^.ChannelType = Channel_WMA then
      TagP := BASS_ChannelGetTags(channel, BASS_TAG_WMA_META) // get metadata
   else
      TagP := BASS_ChannelGetTags(channel, BASS_TAG_META); // get metadata
   if TagP <> nil then
   begin
      tmpStr := ansistring(TagP);
      if PMetaSyncParam^.ChannelType = Channel_WMA then
        {$IFDEF DELPHI_2007_BELOW}
         TitlePos := Pos('Title=', tmpStr)
        {$Else}
         TitlePos := PosEx('Title=', tmpStr, 1)
        {$ENDIF}
      else
        {$IFDEF DELPHI_2007_BELOW}
         TitlePos := Pos('StreamTitle=', tmpStr);
        {$Else}
         TitlePos := PosEx('StreamTitle=', tmpStr, 1);
        {$ENDIF}
   end else
   begin
      tmpStr := '';
      TitlePos := 0;
   end;

   if TitlePos <> 0 then
   begin
     {$IFDEF DELPHI_2007_BELOW}
      DelimeterPos := Pos(';', tmpStr);
     {$Else}
      DelimeterPos := PosEx(';', tmpStr);
     {$ENDIF}
      if PMetaSyncParam^.ChannelType = Channel_WMA then
      begin
         if DelimeterPos = 0 then
            StreamTitle := copy(tmpStr, TitlePos + 7, length(tmpStr) - TitlePos - 7)
         else
            StreamTitle := copy(tmpStr, TitlePos + 7, DelimeterPos - TitlePos - 8);
        {$IFDEF DELPHI_2007_BELOW}
         tmpStr2 := UTF8Decode(StreamTitle);
        {$ELSE}
         tmpStr2 := UTF8ToWideString(StreamTitle);
        {$ENDIF}
         tmpPChar := ToPMultiByte(PWideChar(tmpStr2));
         StreamTitle := ansistring(tmpPChar);
      end else
         if DelimeterPos = 0 then
            StreamTitle := copy(tmpStr, TitlePos + 13, length(tmpStr) - TitlePos - 13)
         else
            StreamTitle := copy(tmpStr, TitlePos + 13, DelimeterPos - TitlePos - 14);
   end else
   begin   // May be a chained OGG stream
      while StrLen(TagP) > 0 do
      begin
        {$IFNDEF DELPHI_2007_BELOW}
         if PosEX('TITLE=', Ansistrings.upperCase(ansistring(TagP)), 1) <> 0 then
        {$ELSE}
         if pos('TITLE=', upperCase(ansistring(TagP))) <> 0 then
        {$ENDIF}
         begin
            inc(TagP, 6);
            StreamTitle := ansistring(TagP);
            break;

            inc(TagP, StrLen(TagP) + 1);
         end;
      end;
   end;

 {  if StreamTitle = '' then
      StreamTitle := tmpStr;  }

   StrPLCopy(PMetaSyncParam^.TitleP, StreamTitle, MaxTitleLen);
   PostMessage(PMetaSyncParam^.MsgHandle, WM_GetMeta, 0, longint(@PMetaSyncParam^.TitleP));
end;


// This procedure is called when a stream reaches the end.
procedure PlayEndSync(SyncHandle : HSYNC; Channel, data : DWORD; user : pointer); stdcall;
var
   Msg_Handle : DWORD;
begin
   Msg_Handle := PDWORD(user)^;
   PostMessage(Msg_Handle, WM_GetToEnd, 0, 0);
end;

// This procedure is called when downloading of an URL stream is done.
procedure DownloadSync(SyncHandle : HSYNC; Channel, data: DWORD; user : pointer); stdcall;
var
   Msg_Handle : DWORD;
begin
   Msg_Handle := PDWORD(user)^;
   PostMessage(Msg_Handle, WM_DownLoaded, 0, 0);
end;

// This procedure is called when a lyric event is encountered.
procedure LyricSync(SyncHandle : HSYNC; Channel, data : DWORD; user : pointer); stdcall;
var
   PLyricSyncParam : ^TLyricSyncParam;
   MarkP : BASS_MIDI_MARK;
begin
   PLyricSyncParam := user;
   if BASS_MIDI_StreamGetMark(PLyricSyncParam^.Channel, BASS_MIDI_MARK_LYRIC, data, MarkP) then
      PostMessage(PLyricSyncParam^.MsgHandle, WM_GetLyric, longint(MarkP.text), 0);
end;

// This procedure is called when an attribute slide has ended
// * Added at Ver 2.01
procedure SlideEndSync(SyncHandle : HSYNC; Channel, data : DWORD; user : pointer); stdcall;
var
   PSlideSyncParam : ^TSlideSyncParam;
begin
   PSlideSyncParam := user;
   PostMessage(PSlideSyncParam^.MsgHandle, WM_SlideEnded, PSlideSyncParam^.NextAction, 0);
end;

// This procedure is called before the BASS_MIDI_StreamCreateURL call returns.
// This procedure is used to get HTTP or ICY tags from the server.
procedure DownProc(buffer : pchar; length : DWORD; user : pointer); stdcall;
var
   Msg_Handle : DWORD;
   bufferStr : string;
   p : pointer;
begin
   Msg_Handle := PDWORD(user)^;

   if length = 0 then
   begin
      bufferStr := string(buffer);
      if copy(bufferStr, 1, 4) = 'HTTP' then
      begin
         p := buffer + 4;
         PostMessage(Msg_Handle, WM_GetHTTPHeaders, DWORD(p), 0);
      end else
      if copy(bufferStr, 1, 3) = 'ICY' then
      begin
         p := buffer + 3;
         PostMessage(Msg_Handle, WM_GetHTTPHeaders, DWORD(p), 0);
      end
   end else
   if buffer = nil then
      PostMessage(Msg_Handle, WM_GetHTTPHeaders, 0, 0);  // no HTTP or ICY tags from the server
                                                         // but finished downloading process.
end;

// ----------------------------- end of Event Handlers ---------------------------

// ------------------------------ Class TBassPlayer ------------------------------

// get the handle of vis window
function TBASSPlayer.GetVisWindowHandle : HWND;
begin
   result := VisWindowHandle;
end;

procedure TBASSPlayer.Error(msg: string);
var
  s: string;
begin
  s := msg + #13#10 + '(error code: ' + IntToStr(BASS_ErrorGetCode) + ')';
  ShowErrorMsgBox(s);
end;

procedure TBASSPlayer.WinProcessMessages;  // * Added at Ver 2.00
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

{ function TBASSPlayer.CDDescription(DriveNum : integer) : string;
begin
   if not FBASSCDReady then
      result := ''
   else if DriveNum >= FNumCDDrives then
      result := ''
   else
      result := CDDriveList[DriveNum];
end; }

procedure TBASSPlayer.SetSingleChannel(Value : boolean);
begin
   if Value = true then
      FSingleChannel := true
   else if FPluginReady then         // buffer for dual channel mode is ready ?
      FSingleChannel := false
   else
      ShowErrorMsgBox('Cannot support dual channel mode. (Failed to get buffer)' );
end;

procedure TBASSPlayer.ClearEffectHandle;
var
   i : integer;
begin
 //  fladspHandle := 0;
   RotateDSPHandle := 0;
   if FDX8EffectReady then
   begin
      for i := 0 to (FEQBands.Bands - 1) do
          EQHandle[i] := 0;
      EchoHandle := 0;
      ReverbHandle := 0;
      FlangerHandle := 0;
   end;
end;

function TBASSPlayer.PlayLength : DWORD;  // get playback length in mili seconds
var
   SongLength : int64;
   FloatLen : FLOAT;
begin
   result := 0;
   case ChannelType of
      Channel_NotOpened : exit;
      Channel_Stream..Channel_MIDI :
                 SongLength := BASS_ChannelGetLength(DecodeChannel, BASS_POS_BYTE);
      Channel_Plugin : begin
                         result := FStreamInfo.Duration;
                         exit;
                       end;
       else
          SongLength := 0;
   end;

   if SongLength <= 0 then  // can be -1 if DecodeChannel is not available (ex. BASS add-on was freed.)
   begin
      result := 0;
      exit;
   end;

   FloatLen := BASS_ChannelBytes2Seconds(DecodeChannel, SongLength);
   result := round(1000 * FloatLen);   // sec -> milli sec
end;


function TBASSPlayer.CurrentPosition : DWORD;  // get playback position in mili seconds
var
   SongPos : int64;
   FloatPos : FLOAT;
begin
   case ChannelType of
      Channel_NotOpened : SongPos := 0;
      Channel_Stream..Channel_MIDI : begin
                          if FSingleChannel2 then
                             SongPos := BASS_ChannelGetPosition(DecodeChannel, BASS_POS_BYTE)  // * Changed at 1.9
                          else
                             SongPos := BASS_ChannelGetPosition(DecodeChannel, BASS_POS_BYTE) - DataInBuffer;   // * Changed at 1.9
                       end;
      Channel_Plugin : begin
                          if FPlayerMode = plmReady then
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

   if SongPos <= 0 then  // can be -1 if DecodeChannel is not available (ex. BASS add-on was freed.)
   begin
      result := 0;
      exit;
   end;

   FloatPos := BASS_ChannelBytes2Seconds(DecodeChannel, SongPos);
   result := round(1000 * FloatPos);     // sec -> milli sec
end;


procedure TBASSPlayer.SetPosition(Value : DWORD); // set playback position in mili seconds
var
   SongPos : int64;
   wCycle : integer;
   PlayerStat : DWORD;
   tmpMuted : boolean;
begin
   if not IsSeekable then
      exit;

   if Value > PlayLength {= FStreamInfo.Duration} then
      exit;

   if NowStarting then
      exit;

   PlayerStat := BASS_ACTIVE_STOPPED;   // Pre-assume
   if not FMute then
   begin
     SetMuteState(true, 500);   // Mute
     repeat
        Sleep(20);
        WinProcessMessages;
     until (not AttribSliding);

     tmpMuted := true;
   end else
     tmpMuted := false;

   if not FSingleChannel2 then
      if ChannelType <> Channel_Plugin then
      begin
         PlayerStat := BASS_ChannelIsActive(PlayChannel);
         if (PlayerStat = BASS_ACTIVE_PAUSED) then
            NeedFlush := true
         else if (PlayerStat = BASS_ACTIVE_PLAYING) then
            BASS_ChannelStop(PlayChannel);

      // Reset buffer pointers to invalidate the data in "pBuf"
         ClearBuffer;
      end;

   case ChannelType of
      Channel_NotOpened : exit;
    {  Channel_Music   : begin   
                           if (BASS_ChannelSetPosition(DecodeChannel, MAKELONG((Value div 1000), $ffff))) then
                              if FSingleChannel2 then
                              begin
                                 MusicStartTime := timeGetTime - Value;
                                 exit;
                              end else
                              begin
                                 RestartPos := Value;
                                 if PlayerStat = BASS_ACTIVE_PLAYING then
                                 begin
                                    BASS_ChannelPlay(PlayChannel, true);
                                    if Assigned(FOnNewFFTData) then
                                      if VisWindowHandle = 0 then  // vis plug-in is not active ?
                                         TimerFFT.Enabled := true;
                                 end;
                              end;
                           exit;
                        end; }
      Channel_Stream..Channel_MIDI  : begin
                           SongPos := BASS_ChannelSeconds2Bytes(DecodeChannel, Value / 1000);
                           BASS_ChannelSetPosition(DecodeChannel, SongPos, BASS_POS_BYTE);
                           if not FSingleChannel2 then
                              if PlayerStat = BASS_ACTIVE_PLAYING then
                              begin
                                 BASS_ChannelPlay(PlayChannel, true);
                                 if Assigned(FOnNewFFTData) then
                                 //  if VisWindowHandle = 0 then  // vis plug-in is not active ?
                                      TimerFFT.Enabled := true;   
                              end;
                        end;
      Channel_Plugin  : begin
                           if (FPlayerMode = plmPlaying) and UsesOutputPlugin then
                              NowStarting := true;
                           SetPosChanged;
                           APlugin.SetOutputTime(Value);
                           wCycle := 0;
                           while FPlayerMode = plmPlaying do
                           begin
                              if NowStarting = false then
                               begin
                                  if Assigned(FOnNewFFTData) then
                                 //   if VisWindowHandle = 0 then  // vis plug-in is not active ?
                                       TimerFFT.Enabled := true;  
                                  break;
                               end;
                               WinProcessMessages;
                               sleep(40);
                               inc(wCycle);
                               if wCycle > 50 then   // 2 sec time out
                                  break;
                           end;
                           NowStarting := false;
                        end;
   end;

   if tmpMuted then
      SetMuteState(false, 500);
end;

function TBASSPlayer.GetDownloadProgress : DWORD;  // get downloaded bytes
begin
   if ChannelType = Channel_NotOpened then
      result := 0
   else if (not NetStream) then
      result := 0
   else if ChannelType = Channel_Plugin then  // not available if current stream is decoded by Winamp input plug-in
      result := 0
   else
      result := BASS_StreamGetFilePosition(DecodeChannel, BASS_FILEPOS_DOWNLOAD);
end;

function TBASSPlayer.IsSeekable : boolean;
begin
   if (ChannelType = Channel_NotOpened) {or (ChannelType = Channel_Music)} then
      result := false
   else if NetRadio then
      result := false
   else if ChannelType = Channel_Plugin then
      result := (APlugin.is_seekable <> 0)
   else if NetStream then
   begin
      if (ChannelType = Channel_WMA) then   // * Added at Ver 2.00
         result := false
      else
         result := FDownLoaded
   end else
      result := true;
end;


procedure TBASSPlayer.SetVersionStr(Value : string);
begin
   // nothing to do : only to show FVersionStr at form design
end;

procedure TBASSPlayer.SetDLLVersionStr(Value : string);
begin
   // nothing to do : only to show FDLLVersionStr at form design
end;

procedure TBASSPlayer.SetDX8EffectReady(Value : boolean);
begin
   // nothing to do : only to show FDX8EffectReady at form design
end;

procedure TBASSPlayer.SetVolume(Value : DWORD);
var
   tmpValue : DWORD;
begin
   if not FBASSReady then
      exit;
      
   if Value > MaxVolume then
      tmpValue := MaxVolume
   else
      tmpValue := Value;

   FOutputVolume := tmpValue;
   if (ChannelType = Channel_Plugin) {and (not UsesOutputPlugin)} then
      APlugin.SetVolume(FOutputVolume)
   else if not FMute then
   begin
      BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, FOutputVolume * 39);
      BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, FOutputVolume * 39);
   end;
end;

procedure TBASSPlayer.SetMuteState(MuteState : boolean; FadeInOutMS : DWORD);  // ** New at Ver 2.01
begin
   if AttribSliding then
      exit;

   if FadeInOutMS > 3000 then
      FadeInOutMS := 3000;

   if MuteState <> FMute then
   begin
      FMute := MuteState;
      if FMute then
      begin
         if (PlayChannel <> 0) and (FadeInOutMS >= 50) then
         begin
            AttribSliding := true;
            BASS_ChannelSlideAttribute(PlayChannel, BASS_ATTRIB_VOL, 0, 500);
            SlideSyncParam.NextAction := 0;  // 0 : Mute
            HSlideSync := BASS_ChannelSetSync(PlayChannel, BASS_SYNC_SLIDE, 0, @SlideEndSync, @SlideSyncParam);
         end else
         begin
            BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, 0);
            BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, 0);
            BASS_ChannelSetAttribute(PlayChannel, BASS_ATTRIB_VOL, 0);
         end;
      end else
      begin
         BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, FOutputVolume * 39);
         BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, FOutputVolume * 39);
         if (PlayChannel <> 0) and (FadeInOutMS >= 50)then
         begin
            AttribSliding := true;
            BASS_ChannelSetAttribute(PlayChannel, BASS_ATTRIB_VOL, 0);
            BASS_ChannelSlideAttribute(PlayChannel, BASS_ATTRIB_VOL, 1, 500);
            SlideSyncParam.NextAction := 1;  // 1 : Restore
            HSlideSync := BASS_ChannelSetSync(PlayChannel, BASS_SYNC_SLIDE, 0, @SlideEndSync, @SlideSyncParam);
         end else
            BASS_ChannelSetAttribute(PlayChannel, BASS_ATTRIB_VOL, 1);
      end;
   end;
end;

function TBASSPlayer.SetAEQGain(BandNum : integer; EQGain : float) : boolean;  
begin
   result := false;

   if not FDX8EffectReady then
      exit;
   if BandNum >= FEQBands.Bands then
      exit;

   if EQGain > 15.0 then
      FEQGains[BandNum] := 15.0
   else if EQGain < -15.0 then
      FEQGains[BandNum] := -15.0
   else
      FEQGains[BandNum] := EQGain;

   if (Equalizer in FSoundEffects) then
   begin
      if EQHandle[BandNum] = 0 then
         exit;

      if BASS_FXGetParameters(EQHandle[BandNum], @EQParam) then
      begin
      // Avoid redundant operation
         if abs(EQParam.fGain - FEQGains[BandNum]) > 0.001 then   // consider round-off error
         begin
            EQParam.fBandWidth := FEQBands.BandPara[BandNum].BandWidth;
            EQParam.fCenter := FEQBands.BandPara[BandNum].CenterFreq;
            EQParam.fGain:= FEQGains[BandNum];
            if BASS_FXSetParameters(EQHandle[BandNum], @EQParam) then
               result := true;
         end else
            result := true;
      end;
   end;
end;

procedure TBASSPlayer.SetEQGains(Value : TEQGains);
var
   i : integer;
begin
   if not FDX8EffectReady then
      exit;

   for i := 0 to (FEQBands.Bands - 1) do
   begin
      if Value[i] > 15.0 then
         FEQGains[i] := 15.0
      else if Value[i] < -15.0 then
         FEQGains[i] := -15.0
      else
         FEQGains[i] := Value[i];

      if not (Equalizer in FSoundEffects) then
         Continue;

      if EQHandle[i] = 0 then
         Continue;

      if BASS_FXGetParameters(EQHandle[i], @EQParam) then
      begin
      // Avoid redundant operation
         if abs(EQParam.fGain - FEQGains[i]) < 0.001 then   // consider round-off error
            Continue;

         EQParam.fBandWidth := FEQBands.BandPara[i].BandWidth;
         EQParam.fCenter := FEQBands.BandPara[i].CenterFreq;
         EQParam.fGain:= FEQGains[i];
         BASS_FXSetParameters(EQHandle[i], @EQParam);
      end;
   end;
end;

procedure TBASSPlayer.SetEchoLevel(Value : word);
begin
   if Value > MaxDX8Effect then
      exit;

   FEchoLevel := Value;
   if Echo in FSoundEffects then
   begin
       if EchoHandle <> 0 then
       begin
         if BASS_FXGetParameters(EchoHandle, @EchoParam) then
         begin
            EchoParam.fWetDryMix := FEchoLevel * 1.2{30.0};
            EchoParam.fFeedBack := 30.0;
            BASS_FXSetParameters(EchoHandle, @EchoParam)
         end;
       end;
   end;
end;

procedure TBASSPlayer.SetReverbLevel(Value : word);
begin
   if Value > MaxDX8Effect then
      exit;

   FReverbLevel := Value;
   if Reverb in FSoundEffects then
   begin
      if ReverbHandle <> 0 then
       begin
         if BASS_FXGetParameters(ReverbHandle, @ReverbParam) then
         begin
            ReverbParam.fInGain := 0.0;
            ReverbParam.fReverbMix := FReverbLevel * 0.5 - 16.0;
            ReverbParam.fReverbTime := 1000.0;
            ReverbParam.fHighFreqRTRatio := 0.1;
            BASS_FXSetParameters(ReverbHandle, @ReverbParam)
         end;
       end;
   end;
end;

procedure TBASSPlayer.SetFlangerLevel(Value : word);
begin
   if Value > MaxDX8Effect then
      exit;

   FFlangerLevel := Value;
   if Flanger in FSoundEffects then
   begin
      if FlangerHandle <> 0 then
       begin
         if BASS_FXGetParameters(FlangerHandle, @FlangerParam) then
         begin
            FlangerParam.fWetDryMix := FFlangerLevel * 1.5;
            FlangerParam.fDepth     := 75;
            FlangerParam.fFeedback  := -60;
            FlangerParam.fFrequency := 5;
            FlangerParam.lWaveform  := 0;
            FlangerParam.fDelay     := 3;
            FlangerParam.lPhase     := BASS_DX8_PHASE_NEG_180;
            BASS_FXSetParameters(FlangerHandle, @FlangerParam)
         end;
       end;
   end;
end;

procedure TBASSPlayer.SetEQBands(Value : TEQBands);
var
   i : integer;
begin
   if Value.Bands > NumEQBands then
      exit;

   FEQBands.Bands := Value.Bands;
   if FEQBands.Bands > NumEQBands then
      FEQBands.Bands := NumEQBands;

   for i := 0 to (FEQBands.Bands-1) do
   begin
      FEQBands.BandPara[i].CenterFreq := Value.BandPara[i].CenterFreq;
      FEQBands.BandPara[i].BandWidth := Value.BandPara[i].BandWidth;
   end;

   if Equalizer in FSoundEffects then
     for i := 0 to (FEQBands.Bands-1) do
     begin
        if EQHandle[i] = 0 then
           EQHandle[i] := BASS_ChannelSetFX(PlayChannel, BASS_FX_DX8_PARAMEQ, i+1);
        if EQHandle[i] <> 0 then
        begin
           EQParam.fGain := FEQGains[i];
           EQParam.fBandWidth := FEQBands.BandPara[i].BandWidth;
           EQParam.fCenter := FEQBands.BandPara[i].CenterFreq;
           BASS_FXSetParameters(EQHandle[i], @EQParam);
        end;
     end;

   for i := FEQBands.Bands to (NumEQBands-1) do
   begin
      if EQHandle[i] <> 0 then
         if BASS_ChannelRemoveFX(PlayChannel, EQHandle[i]) then
            EQHandle[i] := 0;
   end;
end;

procedure TBASSPlayer.SetSoundEffect(Value : TSoundEffects);
var
   i : integer;
begin
   FSoundEffects := Value;

   if ChannelType = Channel_NotOpened then
      exit;

  { if Flanger in Value then
   begin
      if fladspHandle = 0 then
      begin
         SetFlangeParams;
         fladspHandle := BASS_ChannelSetDSP(PlayChannel, @Flange, nil, 0);
      end;
   end else
      if fladspHandle <> 0 then
         if BASS_ChannelRemoveDSP(PlayChannel, fladspHandle) then
            fladspHandle := 0;  }
   if Rotate in Value then
   begin
      if RotateDSPHandle = 0 then
      begin
         SetRotateParams;
       //  RotateDSPHandle := BASS_ChannelSetDSP(PlayChannel, @RotateProc, nil, 0);
         RotateDSPHandle := BASS_ChannelSetDSP(PlayChannel, @RotateProc, nil, 0);
      end;
   end else
      if RotateDSPHandle <> 0 then
         if BASS_ChannelRemoveDSP(PlayChannel, RotateDSPHandle) then
            RotateDSPHandle := 0;

   if not FDX8EffectReady then
      exit;

   if Equalizer in Value then
   begin
      for i := 0 to (FEQBands.Bands-1) do
      begin
         if EQHandle[i] = 0 then
            EQHandle[i] := BASS_ChannelSetFX(PlayChannel, BASS_FX_DX8_PARAMEQ, i+1);

         if EQHandle[i] <> 0 then
         begin
            EQParam.fGain := FEQGains[i];
            EQParam.fBandWidth := FEQBands.BandPara[i].BandWidth;
            EQParam.fCenter := FEQBands.BandPara[i].CenterFreq;
            BASS_FXSetParameters(EQHandle[i], @EQParam);
         end;
      end;
   end else
      for i := 0 to (FEQBands.Bands - 1) do
      begin
         if EQHandle[i] <> 0 then
            if BASS_ChannelRemoveFX(PlayChannel, EQHandle[i]) then
               EQHandle[i] := 0;
      end;

   if Echo in Value then
   begin
       if EchoHandle = 0 then
          EchoHandle := BASS_ChannelSetFX(PlayChannel, BASS_FX_DX8_ECHO, NumEQBands+1);
       if EchoHandle <> 0 then
       begin
         if BASS_FXGetParameters(EchoHandle, @EchoParam) then
         begin
            EchoParam.fWetDryMix := FEchoLevel * 1.2{30.0};
            EchoParam.fFeedBack := 30.0;
            BASS_FXSetParameters(EchoHandle, @EchoParam)
         end;
       end;
   end else
      if EchoHandle <> 0 then
         if BASS_ChannelRemoveFX(PlayChannel, EchoHandle) then
            EchoHandle := 0;

   if Reverb in Value then
   begin
       if ReverbHandle = 0 then
          ReverbHandle := BASS_ChannelSetFX(PlayChannel, BASS_FX_DX8_REVERB, NumEQBands+2);
       if ReverbHandle <> 0 then
       begin
         if BASS_FXGetParameters(ReverbHandle, @ReverbParam) then
         begin
            ReverbParam.fInGain := 0.0;
            ReverbParam.fReverbMix := FReverbLevel * 1.5 - 16.0;
            ReverbParam.fReverbTime := 1000.0;
            ReverbParam.fHighFreqRTRatio := 0.1;
            BASS_FXSetParameters(ReverbHandle, @ReverbParam)
         end;
       end;
   end else
      if ReverbHandle <> 0 then
         if BASS_ChannelRemoveFX(PlayChannel, ReverbHandle) then
            ReverbHandle := 0;

   if Flanger in Value then
   begin
      if FlangerHandle = 0 then
          FlangerHandle := BASS_ChannelSetFX(PlayChannel, BASS_FX_DX8_FLANGER, NumEQBands+3);
       if FlangerHandle <> 0 then
       begin
         if BASS_FXGetParameters(FlangerHandle, @FlangerParam) then
         begin
            FlangerParam.fWetDryMix := FFlangerLevel * 1.5;
            FlangerParam.fDepth     := 75;
            FlangerParam.fFeedback  := -60;
            FlangerParam.fFrequency := 5;
            FlangerParam.lWaveform  := 0;
            FlangerParam.fDelay     := 3;
            FlangerParam.lPhase     := BASS_DX8_PHASE_NEG_180;
            BASS_FXSetParameters(FlangerHandle, @FlangerParam)
         end;
       end;
   end else
      if FlangerHandle <> 0 then
         if BASS_ChannelRemoveFX(PlayChannel, FlangerHandle) then
            FlangerHandle := 0;
end;


procedure TBASSPlayer.SetPlayerMode(Mode : TPlayerMode);
var
   p1 : PDWORD;
   ModeId : DWORD;
   OldMode : TPlayerMode;
begin
   OldMode := FPlayerMode;
   FPlayerMode := Mode;

   if Mode = plmStandby then
   begin
      FStreamName := '';
      FStreamInfo.FileName := '';
      FStreamInfo.Title := '';
      FStreamInfo.FileSize := 0;
      FStreamInfo.Duration := 0;
      FStreamInfo.Channels := 0;
      FStreamInfo.SampleRate := 0;
      FStreamInfo.BitRate := 0;
      SetStreamInfo2(FStreamInfo);
   end;

   VisDataThread.SetPlayerMode1(FPlayerMode);   // inform to vis plug-in driver
   SetPlayerMode2(FPlayerMode);   // inform to DSP plug-in driver

   if Mode <> plmPlaying then
   begin
      if not VisDataThread.Suspended then
         VisDataThread.Suspend;
     //  TimerFFT.Enabled := false;
   end else
   begin
      if Assigned(FOnNewFFTData) then
       //  if VisWindowHandle = 0 then
            TimerFFT.Enabled := true;
   end;

   if GoVisOut then
   begin
      p1 := ShareMemPointer;
      ModeId := ord(FPlayerMode);
      p1^ := ModeId;
      VisDataThread.InformVisDriver(InformPlayerMode, 4);
      if (Mode = plmPlaying) and VisDataThread.Suspended then
         VisDataThread.ResumeVis(RestartDelay);
   end;

   if Assigned(FOnModeChange) then
      FOnModeChange(Self, OldMode, FPlayerMode);
end;

procedure TBASSPlayer.SetVisEMBEDHandle(Value : HWND);   
var
   Changed : boolean;
begin
   if Value = FEmbedHandle then
      exit;

   Changed := false;
   if Value <> 0 then
   begin
      if IsWindow(Value) then
      begin
         FEmbedHandle := Value;
         Changed := true;
      end
   end else
   begin
      FEmbedHandle := 0;
      Changed := true;
   end;

  { if Changed then
      InformPlayerStatus(stEMBEDHandle); }
   if Changed then
      VisDataThread.Change_EMBEDWindow(FEmbedHandle);
end;

procedure TBASSPlayer.SetEMBEDSwitchMode(Value : TVisEMBEDSwitchMode);
begin
   if Value = FEMBEDSwitchMode then
      exit;

   FEMBEDSwitchMode := Value;
   VisDataThread.Change_EMBEDSwitchMode(FEMBEDSwitchMode);
end;

procedure TBASSPlayer.SetUseVisDrawer(Value : boolean);  
begin
   if Value = FUseVisDrawer then
      exit;

   if Value and (not FGPPluginReady) then
      exit;

   FUseVisDrawer := Value;
 //  InformPlayerStatus(stUseVisDrawer);
   VisDataThread.SetUseOfDrawerForm(FUseVisDrawer);
end;

function  TBASSPlayer.Run_VisPlugin(VisPlugin : string; VismodNo : word) : boolean;
var
   ChannelIs : TChannelAttrb;
begin
   if WaitingPrevVisStop then
      WaitingPrevVisStop := false;

   ChannelIs.Title := FStreamInfo.Title;
   ChannelIs.FilePath := FStreamInfo.FileName;  
   ChannelIs.SampleRate := FStreamInfo.SampleRate;
   ChannelIs.BitRate := FStreamInfo.BitRate;
   ChannelIs.Duration := FStreamInfo.Duration;
   if FMixerPlugged then
      ChannelIs.Channels := 2
   else
      ChannelIs.Channels := FStreamInfo.Channels;
   if Start_Vis(VisPlugin, VismodNo, FEmbedHandle, {FSyncVisWindow,}
              FUseVisDrawer, FEMBEDSwitchMode, ord(FPlayerMode), ChannelIs) <> 0 then
      result := true
   else
      result := false;
end;

function  TBASSPlayer.RunVisPlugin(VisPlugin : string; VismodNo : word) : boolean;  
begin
 // The vis plug-in driver VisDrive.pas takes charges of running Winamp visualization
 // plug-ins in a seperate thread.
 // The visualization data are fed to the vis plug-in driver via shared memory.

   result := false;

   if not VisDataThread.Ready then
   begin
      ShowErrorMsgBox('Cannot run Winamp visualization plug-ins');
      exit;
   end;

   if not FileExists(VisPlugin) then
   begin
      ShowErrorMsgBox('Specified visualization plug-in does not exist');
      exit;
   end;

   if WaitingVisWindow or WaitingVisStop then   // prohibit duplicate request to run vis plug-in
      exit;

   WaitingVisWindow := true;
   if VisWindowHandle <> 0 then
      if IsWindow(VisWindowHandle) then
      begin
      // Run_VisPlugin is called indirectly if vis plug-in is running.
      // It is called at message handling routine for "EndVisOut" message
      // to prevent timing problems.
         WaitingPrevVisStop := true;
         VisPlugin_ := VisPlugin;
         VismodNo_ := VismodNo;
         QuitVisPlugin;
         exit;
      end;

   Run_VisPlugin(VisPlugin, VismodNo);
end;

procedure TBASSPlayer.QuitVisPlugin;
begin
   if not VisDataThread.Ready then
      exit;

   if WaitingVisStop then
      exit;

   GoVisOut := false;
   if not VisDataThread.Suspended then
      VisDataThread.Suspend;

   WaitingVisStop := true;
   Stop_Vis;

  { if VisWindowHandle <> 0 then
      repeat
         WinProcessMessages;
         sleep(20);
      until VisWindowHandle = 0; }

end;

// This procedure is called when an instance of TBASSPlayer is destroyed.
procedure TBASSPlayer.intQuitVisPlugin;
begin
   if not VisDataThread.Ready then
      exit;

   GoVisOut := false;
   if not VisDataThread.Suspended then
      VisDataThread.Suspend;

   Stop_Vis2;

end;


function TBassPlayer.RunDSPPlugin(DSPPlugin : string; DSPmodNo : word) : boolean;
var
   DSPheader : PWinampDSPHeader;
   DSPmod : TDSPmod;
   NumDSPmod : integer;
begin
   result := false;
   if not DSPBufferReady then
   begin
      ShowErrorMsgBox('DSP buffer is not ready');
      exit;
   end;

   LoadDSPModule(DSPPlugin, DSPheader, DSPmod, NumDSPmod{, ParentHWND});
   if (DSPmodNo < NumDSPmod) then
   begin
     // SetPosition2(CurrentPosition);
      if StartDSPModule(DSPmodNo, ParentHWND) = 0 then
         result := true   // succeed
      else begin
         UnloadDSPModule;
         ShowErrorMsgBox('Specified DSP plug-in is not operative');
      end;
   end else begin
      if NumDSPmod = 0 then
         ShowErrorMsgBox('Specified DSP plug-in is invalid')
      else
         ShowErrorMsgBox('Invalid module number');
   end;
end;

procedure TBASSPlayer.QuitDSPPlugin;
begin
   if DSPActive then
      StopDSPModule;
 //  if DSPHandle <> 0 then
 //  begin
 //     BASS_ChannelRemoveDSP(Channel, DSPHandle);
 //     DSPHandle := 0;
 //  end;
end;

procedure TBASSPlayer.SetPluginFirst(Value : boolean);
begin
   FPluginFirst := Value;
end;

{procedure TBASSPlayer.SetSyncVisWindow(Value : boolean);
begin
   if Value <> FSyncVisWindow then
   begin
      FSyncVisWindow := Value;
      InformPlayerStatus(stSyncWindows);
   end;
end; }

procedure TBASSPlayer.SetVisScale(Value : word);
begin
   if (Value < 256) or (Value > 1024) then
      exit;

   FVisScale := Value;
   VisDataThread.SetVisScale(FVisScale);
end;

constructor TBASSPlayer.Create(AOwner: TComponent);
var
   i : integer;
 //  BASSVersion : DWORD;
 //  d: PChar;
   t : string;
   n: Integer;
   CD_Info : BASS_CD_INFO;
   fn : string;

begin
   inherited Create(AOwner);

   FVersionStr := MajorVer + '.' + MinorVer + '.' + RevVer;
 {  FBASSReady := false;
   FBASSWMAReady := false;
   FBASSAACReady := false;
   FBASSCDReady := false;
   FBASSMIDIReady := false;
   FMIDISoundReady := false;
   FGPPluginReady := false;
   FMiniBrowserReady := false;
   FUseVisDrawer := false;
   FSingleChannel := false;
   NeedFlush   := false;

   HBASSWMA := 0;
   HBASSAAC := 0;
   FNumCDDrives := 0;
   defaultFontHandle := 0;
   DecodeChannel := 0;
   PlayChannel := 0;  }

   ChannelType := Channel_NotOpened;
   FSoundEffects := [];
   FPlayerMode := plmStandby;

  { FStreamName :='';
   FStreamInfo.FileName := '';
   FStreamInfo.FileSize := 0;
   FStreamInfo.SampleRate := 0;
   FPluginFirst := false;
   FDecodingByPlugin := false;
   FUsing_BASS_AAC := false;
   FDecoderName := '';

   FDownMixToStereo := false;
   FMixerReady := false;
   FMixerPlugged := false;  }

   FVisScale := 512;        // Scale factor for vis window
 //  FSyncVisWindow := false; // Syncronized action (disabled)
   FEMBEDSwitchMode := NewStart;
  { FEMBEDHandle := 0;
   FMBHandle := 0;
   MessageHandle := 0; }
   
   if not (csDesigning in ComponentState) then
   begin
     ParentHWND := (AOwner as TWinControl).Handle;
     AppHWND := Application.Handle;
     MessageHandle := AllocateHWnd(ProcMessage);
   //  MessageHandle := MakeObjectInstance(ProcMessage);

     FOrgWndProc := Pointer(GetWindowLong(MessageHandle, GWL_WNDPROC));
     MetaSyncParam.MsgHandle := MessageHandle;
     LyricSyncParam.MsgHandle := MessageHandle;
     SlideSyncParam.MsgHandle := MessageHandle;
     DataReadyMsg := registerWindowMessage('WM_BASSSendingData');

     TimerFFT := TTimer.Create(nil);
     TimerFFT.Interval := 25;
     TimerFFT.Enabled := false;
     TimerFFT.OnTimer := TimerFFTTimer;
   {  TimerTitleBar := TTimer.Create(nil);
     TimerTitleBar.Interval := 300;
     TimerTitleBar.Enabled := false;
     TimerTitleBar.OnTimer := TimerTitleBarTimer;  }
   end;

   BASSDLLLoaded := Load_BASSDLL(GetProgDir + 'bass.dll');
   if not BASSDLLLoaded then
   begin
      if (csDesigning in ComponentState) then
         FDLLVersionStr := 'N.A.(Copy bass.dll in <directory Delphi installed>\Bin to get version)'
      else
         FDLLVersionStr := '';
      exit;
   end;

 //  BASSVERSION & BASSVERSIONTEXT are defined in Dynamic_Bass.pas
   if (HIWORD(BASS_GetVersion)<> BASSVERSION) or (LOWORD(BASS_GetVersion) < 1) then
   begin
     if not (csDesigning in ComponentState) then
        ShowErrorMsgBox('BASS version is not ' + BASSVERSIONTEXT + ' !');
     exit;
   end;

 //  BASS_SetConfig(BASS_CONFIG_MAXVOL, MaxVolume);  // Set maximum volume range
   BASS_SetConfig(BASS_CONFIG_CD_FREEOLD, 1);   // Automatically free an existing stream
                                                // when creating a new one on the same drive
   BASS_SetConfig(BASS_CONFIG_NET_PLAYLIST, 1);   // * Added at Ver 2.00
   BASS_SetConfig(BASS_CONFIG_WMA_BASSFILE, 1);   // * Added at Ver 2.00
   BASS_SetConfig(BASS_CONFIG_WMA_PREBUF, 1);     // * Added at Ver 2.00

 // setup output - default device, 44100hz, stereo, 16 bits
   if not BASS_Init(1, 44100, 0, AppHWND, nil) then
   begin
      if not (csDesigning in ComponentState) then
         Error('Can''t initialize device');
      exit;
   end else
      FBASSReady := true;

 //  BassInfoParam.size := SizeOf(BassInfoParam);
   BASS_GetInfo(BassInfoParam);

 // BASS_ChannelSetFX requires DirectX version 8 or higher
   if BassInfoParam.dsver >= 8 then
   begin
      FDX8EffectReady := true;
   end else begin
      if not (csDesigning in ComponentState) then
         MessageBox(AppHWND, NoDX8Msg, 'Warning', MB_ICONWARNING or MB_OK);
      FDX8EffectReady := false;
   end;

   FEQBands.Bands := NumEQBands;
   for i := 0 to (NumEQBands-1) do
   begin
      FEQGains[i] := 0;
      FEQBands.BandPara[i].CenterFreq := EQFreq[i];
      FEQBands.BandPara[i].BandWidth := EQBandWidth[i];
   end;
   FEchoLevel := MaxDX8Effect div 2{16};
   FReverbLevel := MaxDX8Effect div 2{16};
   FFlangerLevel := MaxDX8Effect div 2{16};

   FOutputVolume := MaxVolume;
   BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, FOutputVolume * 39);
   BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, FOutputVolume * 39); 

   FDLLVersionStr := GetDLLVersionStr;

   if not (csDesigning in ComponentState) then
   begin
      if Load_MBDrawerDLL(GetProgDir + MBDrawerDLL) then
      begin
         FMBHandle := InitMiniBrowser(Get_MBDrawerDLL_Handle, ParentHWND, MessageHandle, DataReadyMsg);
         if FMBHandle <> 0 then
         begin
            FMiniBrowserReady := true;
            FMagneticSubProc := pointer(GetSubClass_Proc);
         end else
            Unload_MBDrawerDLL;
      end;

      FPluginReady := InitPluginCtrl(MessageHandle, ParentHWND, FMBHandle, DataReadyMsg);
      if not FPluginReady then
      begin
         FSingleChannel := true;
         ShowErrorMsgBox('Failed to get buffer.'#10 + ' - Cannot run Winamp input plug-ins.'
                                              + #10' - Cannot support dual channel mode.' )
      end;

      fn := GetProgDir + 'basswma.dll';
      if FileExists(fn) then  // * Changed at Ver 2.00
      begin
        {$IFDEF DELPHI_2007_BELOW}
         HBASSWMA := BASS_PluginLoad(pChar(fn), 0);
        {$ELSE}
         HBASSWMA := BASS_PluginLoad(pChar(fn), BASS_UNICODE);
        {$ENDIF}
         if HBASSWMA <> 0 then
            FBASSWMAReady := true;
      end;
      if FileExists(GetProgDir + 'bassmix.dll') then
      begin
         if Load_BASSMIXDLL(GetProgDir + 'bassmix.dll') then
            if (HIWORD(BASS_Mixer_GetVersion) = BASSVERSION) then
               FMixerReady := true
            else
               ShowErrorMsgBox('BASS Mixer version is not ' + BASSVERSIONTEXT + ' !');
      end;

      if Load_BASSCDDLL(GetProgDir + 'basscd.dll') then
      begin
         FBASSCDReady := true;
      // Get list of available CDROM drives
         n := 0;
         while (n < MAXCDDRIVES) do
         begin
            if BASS_CD_GetInfo(n, CD_Info) then
            begin
               t := Format('%s: %s', [Char(CD_Info.letter + Ord('A')), CD_Info.product]); // "letter: description"
               CDDriveList[n] := t;
            end else
               break;

            n := n + 1;
         end;
         FNumCDDrives := n;
      end;

      if Load_BASSMIDIDLL(GetProgDir + 'bassmidi.dll') then
         FBASSMIDIReady := true;
      if Load_VisDrawerDLL(GetProgDir + VisDrawerDLL) then
         FGPPluginReady := true;

      MPEG := TMPEGaudio.Create;
      Vorbis := TOggVorbis.Create;
      AAC := TAACfile.Create;
      WMA := TWMAfile.Create;
      WAV := TWAVFile.Create;

   //    MPEGFileInfoForm.SetMPEG(MPEG);
      SetMessageHandle(MessageHandle);
      VisDataThread := TVisDataThread.Create(DataReadyMsg, ShareMemPointer);
      if VisDataThread.Ready then
      begin
         VisDataThread.SetVisScale(FVisScale);
         SetBasicParams(ParentHWND, MessageHandle, DataReadyMsg, ShareMemPointer,
                        VisThreadPriority, VisDataThread.LockFlag, GetVisDrawerDLL2Handle);
         WaitingPrevVisStop := false;
         WaitingVisStop := false;
      end;

   // * Changed at Ver 2.00 ( the owner of the forms : Self -> AOwner )
      PluginConfigForm := TPluginConfigForm.Create(AOwner);
      MPEGFileInfoForm := TMPEGFileInfoForm.Create(AOwner);
      OggVorbisInfoForm := TOggVorbisInfoForm.Create(AOwner);
      WMAInfoForm := TWMAInfoForm.Create(AOwner);

     { for i := 1 to MaxLoadableAddons do
         BASSAddonList[i].Handle := 0; }

   end;
end;


destructor  TBASSPlayer.Destroy;
var
   i : integer;
begin
   if not (csDesigning in ComponentState) then
   begin
     TimerFFT.Free;
   //  TimerTitleBar.Free;
   end;

   if ChannelType = Channel_Plugin then
   begin
      if PlayChannel <> 0 then
         APlugin.Stop;
   end else if ChannelType <> Channel_NotOpened then
   begin
      BASS_ChannelStop(PlayChannel);
      if not FSingleChannel2 then
         omodClose2;
   end;


   if FMiniBrowserReady then
   begin
      QuitMiniBrowser;
      for i := 0 to MAxGPPNo - 1 do
         QuitGenDrawer(i);
   end;

   if GoVisOut then
      intQuitVisPlugin;

  //  StopGPPModules;
   Quit_AllGPPs;

   if DSPActive then
      StopDSPModule;

   if not (csDesigning in ComponentState) then
   begin
     MPEG.Free;
     Vorbis.Free;
     AAC.Free;
     WMA.Free;
     WAV.Free;
   //  QuitPluginCtrl;
     VisDataThread.Free;

     PluginConfigForm.Free;
     MPEGFileInfoForm.Free;
     OggVorbisInfoForm.Free;
     WMAInfoForm.Free;

    { if MessageHandle <> 0 then
        DeallocateHWnd(MessageHandle);   }
   end;

   if BASSDLLLoaded then
   begin
      BASS_PluginFree(0);  // Unplugs all plugins
      BASS_Free;
      Unload_BASSDLL;
   end;
  { if FBASSWMAReady then
      Unload_BASSWMADLL; }
   if HBASSWMA <> 0 then
      BASS_PluginFree(HBASSWMA);
   if HBASSAAC <> 0 then
      BASS_PluginFree(HBASSAAC);
   if FBASSCDReady then
      Unload_BASSCDDLL;
   if FBASSMIDIReady then
   begin
      if defaultFontHandle <> 0 then
         BASS_MIDI_FontFree(defaultFontHandle);
      Unload_BASSMIDIDLL;
   end;
   if FMixerReady then
      Unload_BASSMIXDLL;

   Unload_MBDrawerDLL;
   Unload_VisDrawerDLL;

   if not (csDesigning in ComponentState) then
      QuitPluginCtrl;

   if msgCount > 0 then
   begin
      SetLength(msgs_org, 0);
      SetLength(msgs_local, 0);
   end;

 // Release subclassed window procedure before deallocating MessageHandle.
   if MessageHandle <> 0 then      // * Added at Ver 2.00.1
   begin
      if GetWindowLong(MessageHandle, GWL_WNDPROC) <> LongInt(FOrgWndProc) then
         SetWindowLong(MessageHandle, GWL_WNDPROC, LongInt(FOrgWndProc));
      DeallocateHWnd(MessageHandle);
    //  FreeObjectInstance(MessageHandle);
   end;

   inherited Destroy;
end;

// Get the version information as string
function TBASSPlayer.GetDLLVersionStr : string;
var
   VersionNum : DWORD;
begin
   if not BASSDLLLoaded then
   begin
      result := '';
      exit;
   end;

   VersionNum := BASS_GetVersion;
   result := intToStr(HIBYTE(HiWord(VersionNum))) + '.' +
             intToStr(LOBYTE(HiWord(VersionNum))) + '.' +
             intToStr(HIBYTE(LoWord(VersionNum))) + '.' +
             intToStr(LOBYTE(LoWord(VersionNum)));
   // intToStr(LoWord(VersionNum)) + '.' + intToStr(HiWord(VersionNum));
end;

function TBASSPlayer.GetNativeFileExts : string;
begin
   if not FBASSReady then
   begin
      result := '';
      exit;
   end;

   result := StreamFileExt + Lowercase(MusicFileExt);
   if FBASSCDReady then
      result := result +  '*.cda;';
   if FBASSWMAReady then
      result := result + '*.wma;*.asf;';  // * Changed at Ver 2.00
   if HBASSAAC <> 0 then         // * Added at Ver 2.00
      result := result + '*.aac;*.m4a;*.mp4;';
   if FMIDISoundReady then
      result := result + Lowercase(MIDIFileExt);
end;

function TBASSPlayer.GetPluginFileExts : string;
begin
   result := GetPlayableFiles;
end;

function TBASSPlayer.FileInfoBox(StreamName : string): Boolean;
var
   i : integer;
   tmpChannel : DWORD;
   ByteLen : int64;
   s, ExtCode : string;
   tmpAPlugin : TPlugin;
   APluginInfo : TPluginInfo;
   {$IFDEF DELPHI_2007_BELOW}
   _file : array[0..255] of ansichar;
   {$ENDIF}
   Duration, BitRate : DWORD;
   UseGivenData : boolean;

   dumNum : integer;
   dumStr : string;
begin
   result := false;

   if (StreamName = '') or (not FileExists(StreamName)) then
   begin
    //  ShowMessage('Invalid file name.');
     { if StreamName = '' then
         exit;

      if (StreamName <> FStreamName) or (not NetStream) or
         (FSupportedBy <> WinampPlugin) then   // to continue for stream from net }
         exit;
   end;

   ExtCode := UpperCase(ExtractFileExt(StreamName));
   if (ExtCode = '') or (ExtCode = '.') or
       ((length(ExtCode) <> 4) and (length(ExtCode) <> 3)) then
   begin
    //  ShowMessage('Invalid file name.');
      exit;
   end;

 // Check if it is the file type that can be analyzed by native code.
   if (ExtCode = '.MP1') or (ExtCode = '.MP2') or (ExtCode = '.MP3') then
   begin
      MPEG.ReadFromFile(StreamName);
      if MPEG.Valid then
      begin
         MPEGFileInfoForm.SetAttributes(StreamName, MPEG, AAC, false, 0, 0, false);
         MPEGFileInfoForm.ShowModal;
         result := true;
      end {else
         ShowMessage('Not a valid MPEG file - ' + StreamName)};
   end else if (ExtCode = '.AAC') then
   begin
      AAC.ReadFromFile(StreamName);
      if AAC.Valid then
      begin
         UseGivenData := false;
         Duration := 0;
         BitRate := 0;

         if FBASSAACReady then
         begin
         // AACfile.pas Version 1.2 reports wrong BitRate & Duration for MPEG-4 files.
            if AAC.MPEGVersionID = AAC_MPEG_VERSION_4 then
            begin
             {$IFDEF DELPHI_2007_BELOW}
               tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName), 0, 0, 0);
             {$ELSE}
               tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName), 0, 0, BASS_UNICODE);
             {$ENDIF}
               if tmpChannel <> 0 then
               begin
                  ByteLen := BASS_ChannelGetLength(tmpChannel, BASS_POS_BYTE);
                  Duration := round(BASS_ChannelBytes2Seconds(tmpChannel, ByteLen) * 1000);
                  ByteLen := BASS_StreamGetFilePosition(tmpChannel, BASS_FILEPOS_END);
                  BitRate := round(ByteLen / (0.125 * Duration));   // bitrate (Kbps)
                  BASS_StreamFree(tmpChannel);
                  UseGivenData := true;
               end;
            end;
         end;
         MPEGFileInfoForm.SetAttributes(StreamName, MPEG, AAC, true, Duration, BitRate, UseGivenData);
         MPEGFileInfoForm.ShowModal;
         result := true;
      end
   end else if (ExtCode = '.OGG') then
   begin
      Vorbis.ReadFromFile(StreamName);
      if Vorbis.Valid then
      begin
         OggVorbisInfoForm.SetAttributes(StreamName, Vorbis);
         OggVorbisInfoForm.ShowModal;
         result := true;
      end {else
         ShowMessage('Not a valid OGG Vorbis file - ' + StreamName)};
   end else if (ExtCode = '.WMA') or (ExtCode = '.ASF') then
   begin
      WMA.ReadFromFile(StreamName);
      if WMA.Valid then
      begin
         WMAInfoForm.SetAttributes(StreamName, WMA);
         WMAInfoForm.ShowModal;
         result := true;
      end {else
         ShowMessage('Not a valid WMA file - ' + StreamName)};
   end;

   if StreamName = FStreamName then
   // Reload information of the stream file if it is currently opened stream file.
   // (may be altered some items of them.)
      GetStreamInfo2(StreamName, FStreamInfo, FSupportedBy, dumNum, dumStr);
   if result = true then
      exit; 

  // Check if it is the file type that can be supported by one of currently loaded
  // Winamp input plug-ins.
   for i := 0 to MaxPluginNum - 1 do
   begin
      tmpAPlugin := GetPlugin(i);
      if tmpAPlugin <> nil then
      begin
         APluginInfo := GetPluginInfo(i);
         s := uppercase(APluginInfo.FileExtensions);
         if pos(copy(ExtCode, 2, length(ExtCode) - 1), s) <> 0 then
         begin
            result := true;
           {$IFDEF DELPHI_2007_BELOW}
            StrPLCopy(_file, StreamName, 255);
            tmpAPlugin.InfoBox(_file, ParentHWND);
           {$ELSE}
            tmpAPlugin.InfoBox(ToPMultiByte(pWideChar(StreamName)), ParentHWND);
           {$ENDIF}
            break;
         end;
      end;
   end;

   if StreamName = FStreamName then
   // Reload information of the stream file if it is currently opened stream file.
   // (may be altered some items of them.)
      GetStreamInfo2(StreamName, FStreamInfo, FSupportedBy, dumNum, dumStr);
end;

function TBASSPlayer.GetStreamInfo2(StreamName : string;
                       var StreamInfo : TStreamInfo;
                       var SupportedBy : TSupportedBy;
                       var PluginNum : integer;
                       var PluginName : string) : boolean;
var
   i, n : integer;
   f : file;
   tmpTitle : array [1..28] of Char;
   s, s2, ExtCode : string;
   DriveLetter : string[2];

   tmpChannel : DWORD;
   ChannelInfo : BASS_CHANNELINFO;
   ByteLen : int64;

   tmpAPlugin : TPlugin;
   APluginInfo : TPluginInfo;
   {$IFDEF DELPHI_2007_BELOW}
   _file : array[0..255] of ansichar;
   {$ENDIF}
   _title : array[0..255] of ansichar;
   _length_in_ms : integer;

begin
   result := false;

   with StreamInfo do
   begin
      FileName := '';
      FileSize := 0;
      SampleRate := 0;
      BitRate := 0;
      BitsPerSample := 16;  // default value
      Duration := 0;
      Channels := 0;
      Format   := 0;        // Unknown format
      Title := '';
      Artist := '';
      Album := '';
      Year := '';
      Genre := '';
      GenreID := 0;
      Track := 0;
      Comment := '';
   end;

   tmpChannel := 0;

   StreamInfo.FileName := StreamName;
   SupportedBy := None;
   PluginNum := -1;
   PluginName := '';

   ExtCode := UpperCase(ExtractFileExt(StreamName));
   if (ExtCode = '') or (ExtCode = '.') or (length(ExtCode) > 5) or
      (length(ExtCode) < 3) then
      exit;

   if (copy(StreamName, 1, 5) = 'http:') or (copy(StreamName, 1, 4) = 'mms:')
      or (copy(StreamName, 1, 4) = 'ftp:') then
      exit;

 // Check if native file types
   if ExtCode = '.WAV' then
   begin
      WAV.ReadFromFile(StreamName);
      if WAV.Valid then
      begin
         with StreamInfo do
         begin
            FileSize := WAV.FileSize;
            SampleRate := WAV.SampleRate;
            BitsPerSample := WAV.BitsPerSample;
            BitRate := (SampleRate * BitsPerSample) div 1000;  // Why not 1024 ?
            Duration := round(WAV.Duration * 1000);
            Channels := WAV.ChannelModeID;
           {$IFDEF DELPHI_2007_BELOW}
            tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName), 0, 0, 0);
           {$ELSE}
            tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName), 0, 0, BASS_UNICODE);
           {$ENDIF}
            if tmpChannel <> 0 then
            begin
               BASS_ChannelGetInfo(tmpChannel, ChannelInfo);
               Format := ChannelInfo.ctype;
               BASS_StreamFree(tmpChannel);
            end;
         end;
         if FBASSReady then
            SupportedBy := BASSNative;
         result := true;
      end;
   end else if (ExtCode = '.MP1') or (ExtCode = '.MP2') or (ExtCode = '.MP3') then
   begin
      MPEG.ReadFromFile(StreamName);
      if MPEG.Valid then
      begin
         with StreamInfo do
         begin
            FileSize := MPEG.FileLength;
            SampleRate := MPEG.SampleRate;
            BitRate := MPEG.BitRate;
            Duration := round(MPEG.Duration * 1000);

            if MPEG.ID3v1.Exists then
            begin
               Title := MPEG.ID3v1.Title;
               Artist := MPEG.ID3v1.Artist;
               Album := MPEG.ID3v1.Album;
               Year := MPEG.ID3v1.Year;
               Genre := MPEG.ID3v1.Genre;
               GenreID := MPEG.ID3v1.GenreID;
               Track := MPEG.ID3v1.Track;
               Comment := MPEG.ID3v1.Comment;
            end else if MPEG.ID3v2.Exists then
            begin
               Title := MPEG.ID3v2.Title;
               Artist := MPEG.ID3v2.Artist;
               Album := MPEG.ID3v2.Album;
               Year := MPEG.ID3v2.Year;
               Genre := MPEG.ID3v2.Genre;
               Track := MPEG.ID3v2.Track;
               Comment := MPEG.ID3v2.Comment;
            end;

            if MPEG.ChannelMode = 'Mono' {MPEG_CM_MONO} then
               Channels := 1
            else
               Channels := 2;
            if MPEG.Layer = MPEG_LAYER[1] then
               Format := BASS_CTYPE_STREAM_MP3
            else if MPEG.Layer = MPEG_LAYER[2] then
               Format := BASS_CTYPE_STREAM_MP2
            else if MPEG.Layer = MPEG_LAYER[3] then
               Format := BASS_CTYPE_STREAM_MP1;

         end;

         if FBASSReady then
            SupportedBy := BASSNative;
         result := true;
      end;
   end else if ExtCode = '.OGG' then
   begin
      Vorbis.ReadFromFile(StreamName);
      if Vorbis.Valid then
      begin
         with StreamInfo do
         begin
            FileSize := Vorbis.FileSize;
            SampleRate := Vorbis.SampleRate;
            BitRate := Vorbis.BitRate;
            Duration := round(Vorbis.Duration * 1000);
            Channels := Vorbis.ChannelModeID;
            Title := Vorbis.Title;
            Artist := Vorbis.Artist;
            Album := Vorbis.Album;
            Year := Vorbis.Date;
            Genre := Vorbis.Genre;
            Track := Vorbis.Track;
            Comment := Vorbis.Comment;
            Format := BASS_CTYPE_STREAM_OGG;
         end;
         if FBASSReady then
            SupportedBy := BASSNative;
         result := true;
      end;
   end else if (ExtCode = '.AAC') then
   begin
      AAC.ReadFromFile(StreamName);
      if AAC.Valid then
      begin
         with StreamInfo do
         begin
            FileSize := AAC.FileSize;
            SampleRate := AAC.SampleRate;
            BitRate := AAC.BitRate div 1000;
            Duration := round(AAC.Duration * 1000);
            Channels := AAC.Channels;
            if AAC.ID3v1.Exists then
            begin
               Title := AAC.ID3v1.Title;
               Artist := AAC.ID3v1.Artist;
               Album := AAC.ID3v1.Album;
               Year := AAC.ID3v1.Year;
               Genre := AAC.ID3v1.Genre;
               GenreID := AAC.ID3v1.GenreID;
               Track := AAC.ID3v1.Track;
               Comment := AAC.ID3v1.Comment;
            end else if AAC.ID3v2.Exists then
            begin
               Title := AAC.ID3v2.Title;
               Artist := AAC.ID3v2.Artist;
               Album := AAC.ID3v2.Album;
               Year := AAC.ID3v2.Year;
               Genre := AAC.ID3v2.Genre;
               Track := AAC.ID3v2.Track;
               Comment := AAC.ID3v2.Comment;
            end;
            Format := BASS_CTYPE_STREAM_AAC;
         end;
         if FBASSAACReady then
         begin
        // AACfile.pas Version 1.2 reports wrong BitRate & Duration for MPEG-4 files.
            if AAC.MPEGVersionID = AAC_MPEG_VERSION_4 then
            begin
              {$IFDEF DELPHI_2007_BELOW}
               tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName), 0, 0, 0);
              {$ELSE}
               tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName), 0, 0, BASS_UNICODE);
              {$ENDIF}
               if tmpChannel <> 0 then
               begin
                  ByteLen := BASS_ChannelGetLength(tmpChannel, BASS_POS_BYTE);
                  StreamInfo.Duration := round(BASS_ChannelBytes2Seconds(tmpChannel, ByteLen) * 1000.0);
                  ByteLen := BASS_StreamGetFilePosition(tmpChannel, BASS_FILEPOS_END);
                  StreamInfo.BitRate := round(ByteLen / (0.125 * StreamInfo.Duration));   // bitrate (Kbps)
                  BASS_StreamFree(tmpChannel);
               end;
            end;
            SupportedBy := BASSNative;
         end;
         result := true;
      end;
   end else if (ExtCode = '.WMA') or (ExtCode = '.ASF') then  // * Changed at Ver 2.00
   begin                                                      //   (add for 'ASF' files)
      WMA.ReadFromFile(StreamName);
      if WMA.Valid then
      begin
         with StreamInfo do
         begin
            FileSize := WMA.FileSize;
            SampleRate := WMA.SampleRate;
            BitRate := WMA.BitRate;
            Duration := round(WMA.Duration * 1000);
            Title := WMA.Title;
            Artist := WMA.Artist;
            Album := WMA.Album;
            Year := WMA.Year;
            Genre := WMA.Genre;
            Channels := WMA.ChannelModeID;
            Track := WMA.Track;
            Comment := WMA.Comment;
            Format := BASS_CTYPE_STREAM_WMA;
         end;
         if FBASSWMAReady then
            SupportedBy := BASSNative;
         result := true;
      end;
   end else if ExtCode = '.CDA' then
   begin
      if FBASSCDReady then
      begin
         DriveLetter := copy(UpperCase(StreamName), 1, 1);
         for i := 0 to (FNumCDDrives - 1) do
         begin
          // Check if the specified stream file is in a Audio CD.
            if DriveLetter = copy(CDDriveList[i], 1, 1) then
            begin
               n := strToint(copy(StreamName, length(StreamName) - 5, 2)) - 1;
               with StreamInfo do
               begin
                  FileSize := BASS_CD_GetTrackLength(i, n);
                  SampleRate := 44100;
                  BitRate := 1411;   
                  Duration := (FileSize * 10) div 1764;  // in mili seconds
                  Channels := 2;
                  Track := n;
                  Format := BASS_CTYPE_STREAM_CD;
               end;
               SupportedBy := BASSNative;
               result := true;
            end;
         end;
      end;

   end else if (ExtCode = '.MO3') or (ExtCode = '.IT') or (ExtCode = '.XM') or
      (ExtCode = '.S3M') or (ExtCode = '.MTM') or (ExtCode = '.MOD') or
      (ExtCode = '.UMX') then
   begin
  // I do not have full technical information on MOD music, so only basic information
  // is given to StreamInfo record.
      AssignFile(f, StreamName);
      Reset(f, 1);
      StreamInfo.FileSize := FileSize(f);

      with StreamInfo do
      begin
     // Get the title for some types of music files.
     // I am not sure it always works correctly.
         FillChar(tmpTitle, SizeOf(tmpTitle), ' ');
         if (ExtCode = '.MOD') or (ExtCode = '.MTM') or (ExtCode = '.XM') or
            (ExtCode = '.IT') then
         begin
            if (ExtCode = '.XM') then
                Seek(f, $11)
            else if (ExtCode = '.IT') or (ExtCode = '.MTM') then
                Seek(f, 4);
            BlockRead(f, tmpTitle, 20);
            Title := TrimRight(tmpTitle);
         end else if (ExtCode = '.S3M') then
         begin
            BlockRead(f, tmpTitle, 28);
            Title := TrimRight(tmpTitle);
         end else    // for MO3 and UMX file ( I cannot get any documentation on these files )
            Title := ExtractFileName(StreamName);  // Not the title given to music file

         CloseFile(f);
        {$IFDEF DELPHI_2007_BELOW}
         tmpChannel := BASS_MusicLoad(FALSE, PChar(StreamName), 0, 0, BASS_MUSIC_PRESCAN, 0);
        {$ELSE}
         tmpChannel := BASS_MusicLoad(FALSE, PChar(StreamName), 0, 0, BASS_MUSIC_PRESCAN or BASS_UNICODE, 0);
        {$ENDIF}
         if tmpChannel <> 0 then
         begin
            ByteLen := BASS_ChannelGetLength(tmpChannel, BASS_POS_BYTE);
            Duration := round(BASS_ChannelBytes2Seconds(tmpChannel, ByteLen) * 1000.0);
            BASS_ChannelGetInfo(tmpChannel, ChannelInfo);
            SampleRate := ChannelInfo.freq;
            if (ChannelInfo.flags and BASS_SAMPLE_8BITS) = BASS_SAMPLE_8BITS then
               BitsPerSample := 8
            else if (ChannelInfo.flags and BASS_SAMPLE_FLOAT) = BASS_SAMPLE_FLOAT then
               BitsPerSample := 32;
            Channels := ChannelInfo.chans;
            Format := ChannelInfo.ctype;
            BitRate := 0;  // Meaningless value for music files
            BASS_MusicFree(tmpChannel);
            SupportedBy := BASSNative;
            result := true;
         end;
      end;
   end else if pos(ExtCode, MIDIFileExt) > 0 then
   begin
      if FMIDISoundReady then
      begin
         AssignFile(f, StreamName);
         Reset(f, 1);
         StreamInfo.FileSize := FileSize(f);
         CloseFile(f);

         StreamInfo.FileName := StreamName;

         if FMIDISoundReady then
          {$IFDEF DELPHI_2007_BELOW}
            tmpChannel := BASS_MIDI_StreamCreateFile(FALSE, pChar(StreamName), 0, 0, 0, 44100);
          {$ELSE}
            tmpChannel := BASS_MIDI_StreamCreateFile(FALSE, pChar(StreamName), 0, 0, BASS_UNICODE, 44100);
          {$ENDIF}
         if tmpChannel <> 0 then
         begin
            ByteLen := BASS_ChannelGetLength(tmpChannel, BASS_POS_BYTE);
            StreamInfo.Duration := round(BASS_ChannelBytes2Seconds(tmpChannel, ByteLen) * 1000.0);
            BASS_StreamFree(tmpChannel);
         end;

         StreamInfo.SampleRate := 44100;
         StreamInfo.BitRate := 0;
         StreamInfo.Format := BASS_CTYPE_STREAM_MIDI;
         SupportedBy := BASSNative;
         result := true;
      end;
   end else
   begin   // Check if the opening stream is playable by an add-on.
      AssignFile(f, StreamName);
      Reset(f, 1);
      StreamInfo.FileSize := FileSize(f);
      CloseFile(f);

      S := UpperCase('.AIFF;' + GetBASSAddonExts);
      if FBASSAACReady then
         S := S + '.M4A;.MP4;'; // acceptable by bass_aac.dll (*.AAC files are checked independently)
    //  s2 := copy(ExtCode, 2, length(ExtCode) - 1);
      s2 := s2 + ';';
      if pos(s2, s) > 0 then
      begin
        {$IFDEF DELPHI_2007_BELOW}
         tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName), 0, 0, 0);
        {$ELSE}
         tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName), 0, 0, BASS_UNICODE);
        {$ENDIF}
         if tmpChannel <> 0 then
         begin
            ByteLen := BASS_ChannelGetLength(tmpChannel, BASS_POS_BYTE);
            StreamInfo.Duration := round(BASS_ChannelBytes2Seconds(tmpChannel, ByteLen) * 1000.0);
            BASS_ChannelGetInfo(tmpChannel, ChannelInfo);
            StreamInfo.SampleRate := ChannelInfo.freq;
            if (ChannelInfo.flags and BASS_SAMPLE_8BITS) = BASS_SAMPLE_8BITS then
               StreamInfo.BitsPerSample := 8
            else if (ChannelInfo.flags and BASS_SAMPLE_FLOAT) = BASS_SAMPLE_FLOAT then
               StreamInfo.BitsPerSample := 32;
            StreamInfo.Channels := ChannelInfo.chans;
            StreamInfo.Format := ChannelInfo.ctype;

            ByteLen := BASS_StreamGetFilePosition(tmpChannel, BASS_FILEPOS_END);
            StreamInfo.BitRate := round(ByteLen / (0.125 * StreamInfo.Duration));   // bitrate (Kbps)

            BASS_StreamFree(tmpChannel);
            SupportedBy := BASSNative;
            result := true;
         end;
      end;
   end;

 // Check if it is the file type that can be played by Winamp input plug-in
   for i := 0 to MaxPluginNum - 1 do
   begin
      tmpAPlugin := GetPlugin(i);
      if tmpAPlugin <> nil then
      begin
         APluginInfo := GetPluginInfo(i);
         s := uppercase(APluginInfo.FileExtensions);
         if pos(copy(ExtCode, 2, 3), s) <> 0 then
         begin
            PluginNum := i;
            PluginName := APluginInfo.Name;
           {$IFDEF DELPHI_2007_BELOW}
            StrPLCopy(_file, StreamName, 255);
            tmpAPlugin.GetFileInfo(_file, _title, _length_in_ms);
           {$ELSE}
            tmpAPlugin.GetFileInfo(ToPMultiByte(pWideChar(StreamName)), _title, _length_in_ms);
           {$ENDIF}
            if (_length_in_ms > 0) then
            begin
               if StreamInfo.Title = '' then  // * in_asf.dll returns "Artist - Title"
                  StreamInfo.Title := string( _title);
               StreamInfo.Duration := _length_in_ms;
               if FPluginReady then
                  if SupportedBy = BASSNative then
                     SupportedBy := Both
                  else begin
                     SupportedBy := WinampPlugin;
                 // Set StreamInfo.Format for the stream files that may be decoded
                 //  by Winamp input plug-ins.  (2009-05-14)
                     if pos(ExtCode, MIDIFileExt) > 0 then
                        StreamInfo.Format := BASS_CTYPE_STREAM_MIDI
                     else if pos(ExtCode, '.AC3') > 0 then
                        StreamInfo.Format := $11000  // BASS_CTYPE_STREAM_AC3
                     else if pos(ExtCode, '.SPX') > 0 then
                        StreamInfo.Format := $10c00  // BASS_CTYPE_STREAM_SPX
                     else if pos(ExtCode, '.APE') > 0 then
                        StreamInfo.Format := $10700  // BASS_CTYPE_STREAM_APE;
                  end;
               result := true;
               break;
            end;
         end;
      end;
   end;

   if (result = true) and (StreamInfo.Title = '') then
      StreamInfo.Title := ExtractFileName(StreamName);
end;

function TBASSPlayer.GetStreamInfo(StreamName : string;
                       var StreamInfo : TStreamInfo;
                       var SupportedBy : TSupportedBy) : boolean;
var
   dumNum : integer;
   dumStr : string;
begin
   if StreamName = FStreamName then
   begin
      StreamInfo := FStreamInfo;
      SupportedBy := FSupportedBy;
      result := true;
   end else
      result := GetStreamInfo2(StreamName, StreamInfo, SupportedBy, dumNum, dumStr);
end;


procedure TBASSPlayer.ProcMessage(var Msg: TMessage);
var
   PVisModuleInfo : ^TVisModuleInfo;
   PDriveThreadInfo : ^TVisPluginInfo;
   PChannelInfo : ^TChannelInfo;
   PTitle : PAnsiChar;
   ExtCode : string;
   TagP : pAnsiChar;
   TagVer : word;
   MP3Tag : MP3TagRec;
   PlaybackSec : float;
   tmpChannels : Word;
 //  PBandOut : ^WORD;
 //  i : integer;

   _title : array[0..255] of ansichar;
   _length_in_ms : integer;

   FileP : pChar;

 //  pRect : TRect;

 //  ElapsedByte : int64;
 //  ElapsedTime : double;

begin
   if Msg.Msg = DataReadyMsg then
   begin
      if Msg.wParam = MBShowUp then
      begin
         if Assigned(FOnMBWindowShow) then
            FOnMBWindowShow(Self, SW_SHOW);
      end else if Msg.wParam = MBClosed then
      begin
         if Assigned(FOnMBWindowShow) then
            FOnMBWindowShow(Self, SW_HIDE);
      end else if Msg.wParam = GenShowClose then
      begin
         if Assigned(FOnGenWindowShow) then
            FOnGenWindowShow(Self, Msg.LParam);
      end else if Msg.wParam = VisModuleLoaded then      // vis plug-in loaded ?
      begin
         PVisModuleInfo := pointer(Msg.lParam);
         VisDataThread.SetVisModuleInfo(PVisModuleInfo^);
      end else if Msg.wParam = StartVisOut then // vis plug-in started ?
      begin
         if Msg.lParam <> 0 then
         begin
          //  TimerFFT.Enabled := false;
            PDriveThreadInfo := pointer(Msg.lParam);
            VisDataThread.SetDriveThreadId(PDriveThreadInfo^.ThreadId);
            VisWindowHandle := PDriveThreadInfo^.VisHandle;
            FVisWindowAttr := PDriveThreadInfo^;
            if Assigned(FOnVisWindowShow) then
               FOnVisWindowShow(Self, FVisWindowAttr);

            GoVisOut := true;
            if (FPlayerMode = plmPlaying) then
               VisDataThread.ResumeVis(InitDelay);
          {  if FVisWindowAttr.VisType = OnCreatedByGPP then
               TimerTitleBar.Enabled := true; }
         end else
         begin
            FVisWindowAttr.ThreadId := 0;
            if Assigned(FOnVisWindowShow) then
               FOnVisWindowShow(Self, FVisWindowAttr);
            if Assigned(FOnNewFFTData) then
               if FPlayerMode = plmPlaying then
                  TimerFFT.Enabled := true;
         end;

         WaitingVisWindow := false;
      end else
      if Msg.wParam = EmbedWindowChanged then  
      begin
         GoVisOut := true;
         PDriveThreadInfo := pointer(Msg.lParam);
         VisWindowHandle := PDriveThreadInfo^.VisHandle;
         FVisWindowAttr := PDriveThreadInfo^;
         if Assigned(FOnVisWindowShow) then
            FOnVisWindowShow(Self, FVisWindowAttr);
       {  if FVisWindowAttr.VisType = OnCreatedByGPP then
            TimerTitleBar.Enabled := true; }
      end else
      if Msg.wParam = PauseVisOut then          // vis plug-in paused ?
         GoVisOut := false
      else if Msg.wParam = EndVisOut then       // vis plug-in ended ?
      begin
         GoVisOut := false;
         if not VisDataThread.Suspended then
            VisDataThread.Suspend;

         VisDataThread.SetDriveThreadId(0);
         VisWindowHandle := 0;

         with FVisWindowAttr do
         begin
            ThreadId := 0;
            VisHandle := 0;
            VisType := UnAvailable;
            PluginPath := '';
         end;
         if Assigned(FOnVisWindowShow) then
            FOnVisWindowShow(Self, FVisWindowAttr);
         if Assigned(FOnNewFFTData) then
            if FPlayerMode = plmPlaying then
               if not WaitingVisWindow then
                  TimerFFT.Enabled := true; 

         WaitingVisStop := false;
         if WaitingPrevVisStop then
            Run_VisPlugin(VisPlugin_, VismodNo_);
      end
      else if Msg.wParam = PlayListChange then  // * Added at Ver 2.00
      begin
         if Assigned(FOnUpdatePlayList) then
            FOnUpdatePlayList(Self, Msg.lParam);
      end else
         exit;
   end else if Msg.Msg = WM_RequestFromVis then
   begin
      if Assigned(FOnPluginRequest) then
         FOnPluginRequest(Self, Msg.WParam);
   end;

   case Msg.Msg of
      WM_NewFFTData    : if Assigned(FOnNewFFTData) then
                            FOnNewFFTData(Self, BandOut);
      WM_GetMeta       : begin     // BASS received Metadata in a Shoutcast stream
                            PTitle := pointer(Msg.lParam);
                            FStreamInfo.Title := ansistring(PTitle);
                            InformPlayerStatus(InformStreamInfo);
                            if Assigned(FOnGetMeta) then
                               FOnGetMeta(Self, ansistring(PTitle));
                         end;
      WM_GetLyric      : if Assigned(FOnGetLyric) then  // BASS encountered lyric event
                            FOnGetLyric(Self, pointer(Msg.WParam));

      WM_SlideEnded    : begin
                           if Msg.WParam = 0 then    // Mute
                           begin
                             BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, 0);
                             BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, 0);
                           end;

                           AttribSliding := false;
                           if HSlideSync <> 0 then
                           begin
                             BASS_ChannelRemoveSync(PlayChannel, HSlideSync);
                             HSlideSync := 0;
                           end;
                         end;

      WM_GetHTTPHeaders : begin
                             tmpHTTPTag := pAnsiChar(Msg.WParam);
                             FGetHTTPHeader := true;
                          end;
      WM_DownLoaded    : begin     // BASS finished download of an URL stream
                            FDownloaded := true;
                            ExtCode := UpperCase(ExtractFileExt(FStreamName));
                            if (ExtCode = '.MP3') or (ExtCode = '.MP2') or (ExtCode = '.MP1') or
                               (FBASSAACReady and (ExtCode = '.AAC')) then
                            begin
                               TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_ID3);
                               if TagP = nil then
                               begin
                                  TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_ID3V2);
                                  if TagP = nil then
                                     TagVer := 0
                                  else
                                     TagVer := 2;
                               end else
                                  TagVer := 1;
                            end else
                          {  if (ExtCode = '.WMA') then  // ** Tag info is obtained at opening
                            begin
                               TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_WMA);
                               if TagP = nil then
                                  TagVer := 0
                                else
                                   TagVer := 3;
                            end else }
                            begin
                               TagP := nil;
                               TagVer := 0;
                            end;

                            FStreamInfo.FileSize := BASS_StreamGetFilePosition(DecodeChannel, BASS_FILEPOS_END);

                            if (TagVer = 1) or (TagVer = 2) then
                              if ReadFromTagStream(TagP, FStreamInfo.FileSize, TagVer, MP3Tag) then
                                 with FStreamInfo do
                                 begin
                                    Title := MP3Tag.Title;
                                    Artist := MP3Tag.Artist;
                                    Album := MP3Tag.Album;
                                    Year := MP3Tag.Year;
                                    Genre := MP3Tag.Genre;
                                    Track := MP3Tag.Track;
                                    Comment := MP3Tag.Comment;
                                 end;

                         // Reestimate playback length of an URL stream after finishing download.
                         // ** Following method also shows inaccurate result.
                          {  if (ExtCode = '.AAC') then
                            begin
                            // The returned value of BASS_ChannelGetLength is not accurate for AAC files.
                            // So, I used BASS_ChannelGetPosition instead of BASS_ChannelGetLength.
                              ElapsedByte := BASS_StreamGetFilePosition(DecodeChannel, BASS_FILEPOS_CURRENT);
                              ElapsedTime := BASS_ChannelBytes2Seconds(DecodeChannel,
                                                        BASS_ChannelGetPosition(DecodeChannel, BASS_POS_BYTE));
                            // I assumed that about 60% of buffer is filled with data which are accounted in
                            //  ElapsedByte, and not accounted in ElapsedTime.
                              ElapsedTime := ElapsedTime + (BASS_GetConfig(BASS_CONFIG_BUFFER) / 1000) * 0.60;
                              if (ElapsedByte > 0) and (ElapsedTime > 0) then
                              begin
                                 FStreamInfo.Duration := round(ElapsedTime * FStreamInfo.FileSize * 1000 / ElapsedByte);
                                 FStreamInfo.BitRate := round(ElapsedByte / (125 * ElapsedTime));
                              end;
                            end else
                            begin   }
                              PlaybackSec := BASS_ChannelBytes2Seconds(DecodeChannel,
                                                           BASS_ChannelGetLength(DecodeChannel, BASS_POS_BYTE));
                              FStreamInfo.Duration := round(1000 * PlaybackSec);  // in mili seconds
                           // end;

                            if Assigned(FOnDownloaded) then
                               FOnDownloaded(Self, FStreamInfo.Duration);
                         end;

      WM_BASS_StreamCreate : PlayChannel := DWORD(Msg.WParam);  // Message from output plug-in emulator
      WM_GetChannelInfo : begin    // The basic characteristics of a stream to be played
                                   // are received from Winamp input plug-in.
                            PChannelInfo := pointer(Msg.lParam);
                            FStreamInfo.Channels := PChannelInfo^.Channels;
                            FStreamInfo.SampleRate := PChannelInfo^.SampleRate;
                          // Some plug-ins report Bitrate in KBPS, others in BPS.
                          // So, we should convert the value if it is in BPS.
                          // ** Changed at 2005-05-14
                            if PChannelInfo^.BitRate >= 8000 then
                               PChannelInfo^.BitRate := PChannelInfo^.BitRate div 1000;
                            FStreamInfo.BitRate := PChannelInfo^.BitRate;
                            if Assigned(FOnGetChannelInfo) then
                               FOnGetChannelInfo(Self, pointer(Msg.lParam));
                         end;

   // We do not know the Channels & SampleRate of opened stream file before Winamp
   // input plug-in informs to output plug-in, if the file type is the one which is
   // supported only by Winamp input plug-in.
   // WM_StartPlay message is used to receive the Channels & SampleRate of opened
   // stream file at starting playback from output plug-in emulator.
   // We still cannot know the Channels & SampleRate of opened stream file if its related
   // Winamp input plug-in is the type which directly sound out (= not thru output plug-in emulator).
      WM_StartPlay     : begin             // Message from output plug-in emulator
                            NowStarting := false;
                            UsesOutputPlugin := true;
                            APlugin.SetVolume(FOutputVolume);
                            PChannelInfo := pointer(Msg.lParam);
                            FStreamInfo.Channels := PChannelInfo^.Channels;
                            FStreamInfo.SampleRate := PChannelInfo^.SampleRate;
                            FStreamInfo.BitsPerSample := PChannelInfo^.BitsPerSample;
                            if FStreamInfo.Duration = 0 then  // for URL stream
                            begin
                           //    FStreamInfo.Duration := APlugin.GetLength;
                               APlugin.GetFileInfo(nil, _title, _length_in_ms);
                               if (_length_in_ms > 0) then
                               begin
                                  with FStreamInfo do
                                  begin
                                    Title := string( _title);
                                    Duration := _length_in_ms;
                                  end;
                               end;
                            end;
                            InformPlayerStatus(InformStreamInfo);
                            if Assigned(FOnPluginStartPlay) then
                               FOnPluginStartPlay(Self, pointer(Msg.lParam));
                            if FMixerPlugged then
                               tmpChannels := 2
                            else
                               tmpChannels := FStreamInfo.Channels;
                            VisDataThread.SetChannelInfo(ChannelType,
                                                         DecodeChannel, PlayChannel,
                                                         FStreamInfo.SampleRate,
                                                         tmpChannels);
                         // inform DSP plug-in control unit
                          //  if DSPActive then   // may be needed pre-setting
                               SetChannelInfo2(ChannelType, DecodeChannel, PlayChannel,
                                                         FStreamInfo.SampleRate,
                                                         tmpChannels);
                            SetSoundEffect(FSoundEffects);
                            if Assigned(FOnNewFFTData) then
                             //  if VisWindowHandle = 0 then  // vis plug-in is not active ?
                                  TimerFFT.Enabled := true;
                         end;
      WM_WA_MPEG_EOF   : begin           // Winamp input plug-in reached the end of stream
                            SetReachedEnd;
                            NowStarting := false;
                            if UsesOutputPlugin then
                               while BASS_ChannelIsActive(PlayChannel) = BASS_ACTIVE_PLAYING do
                               begin
                                  WinProcessMessages;
                                  sleep(50);
                               end;

                            APlugin.Stop;
                            PlayChannel := 0;

                            ClearEffectHandle;
                            SetPlayerMode(plmStopped);
                            if Assigned(FOnPlayEnd) then
                               FOnPlayEnd(Self);
                         end;

    //  Use of WM_BASS_StreamFree may cause problem because of reset PlayChannel after set by opening playing channel
    //  WM_BASS_StreamFree   : PlayChannel := 0;        // Message from output plug-in emulator

    //  WM_GetChannelData : omodWrite2;                // message requesting sample data for BASS playing channel
      WM_GetToEnd      : begin                       // BASS reached the end of stream
                          //  WaitCycle := 0;
                            while BASS_ChannelIsActive(PlayChannel) = BASS_ACTIVE_PLAYING do
                            begin
                               WinProcessMessages;
                               sleep(50);
                              { inc(WaitCycle);
                               if WaitCycle = 100 then
                                  break; }
                            end;

                            SetPlayerMode(plmStopped);
                            if Assigned(FOnPlayEnd) then
                               FOnPlayEnd(Self);
                         end;
      WM_ChannelUnavailable : begin   // Message from PlayThread (DecodeChannel is not available)
                            ShowErrorMsgBox('Decoding Channel is unavailable by any reasons.'
                                              + chr(10) + 'Opened stream is beging closed.');
                            Close;
                          end;

      WM_PluginFirst_Changed : if Msg.lParam = 0 then  // Message from PluginConfigForm
                                  FPluginFirst := false
                               else
                                  FPluginFirst := true;

      WM_QueryEndSession : begin
            // TimerFFT should be disabled soon after receiving WM_QueryEndSession
            // message to prevent problems at log off or shut down system.
                              TimerFFT.Enabled := false;
                            //  TimerTitleBar.Enabled := false;
                              Msg.Result := 1;    // Allow system termination
                           end;
      WM_WA_IPC : begin   // for plug-ins for Winamp 5.x ( not finished )
                    Msg.Result := 0;  // Put default value
                    if Msg.LParam = IPC_GETVERSION then
                       Msg.Result := $5010    // acts as if the main program is Winamp ver 5.1
                    else if Msg.LParam = IPC_GETINIFILE then
                    begin
                       FileP := pChar(ExtractFilePath(ParamStr(0)) + 'Plugins\plugin.ini');
                       Msg.Result := integer(FileP);
                    end
                    else if Msg.LParam = IPC_GETLISTPOS then  // * Added at Ver 2.00.1
                       Msg.Result := GetIndexOfPlayList('')
                    else if Msg.LParam = IPC_GETPLAYLISTFILE then  // * Added at Ver 2.00.1
                    begin
                       FileP := pChar(GetAPlayListFile(Msg.WParam));
                       Msg.Result := integer(FileP);
                    end
                    else if Msg.LParam = IPC_GET_API_SERVICE then
                       Msg.Result := WaAPIServiceEntry
                   { else
                       Application.MessageBox(pAnsiChar('Unhandled IPC Msg' + intToStr(Msg.LParam)),
                                             'Confirm', MB_OK or MB_ICONINFORMATION) } ;
                  end;
   end;
end;

function TBASSPlayer.OpenURL(URL : string;
                             var StreamInfo : TStreamInfo;
                             var SupportedBy : TSupportedBy;
                             Temp_Paused : boolean;
                             CheckOnly : boolean) : boolean;
var
  PluginNum : integer;
  APluginInfo : TPluginInfo;
  OpenFlag : DWORD;
  ExtCode, LoExtCode : string;
  TagP : pAnsiChar;
  tmpChannel : DWORD;
  tmpChannelType : TChannelType;
  tmpStr : string;
  tmpStr1 : ansistring;
  tmpStr2 : wideString;
  tmpPChar : pAnsiChar;
  RepeatCounter : integer;
  PlaybackSec : float;
  FileLen : integer;
  TitlePos, DelimeterPos : integer;
  Using_BASS_AAC : boolean;

  function ExtractFileName2(NetStream : string) : string;
  var
    pos, i : integer;
  begin
     pos := 0;
     for i := (length(NetStream) - 6)  downto 6 do
        if NetStream[i] = '/' then
        begin
           pos := i;
           break;
        end;

     if pos > 0 then
        result := copy(NetStream, pos + 1, length(NetStream) - pos - 4)
     else
        result := '';
  end;

  procedure SetupWinampPlugin(PlugNumber : integer);
  begin
     Close;
     ChannelType := Channel_Plugin;
     FPluginNum := PlugNumber;
     APlugin := GetPlugin(FPluginNum);
     SelectInputPlugin(FPluginNum);
     APluginInfo := GetPluginInfo(FPluginNum);
     FDecoderName := APluginInfo.Name;
     PluginConfigForm.SetInUsePlugin(ActivePlugin{APluginInfo.Name});  // inform PluginConfig.pas
     FDecodingByPlugin := true;
  end;

begin
   result := false;
   tmpChannel := 0;
   tmpChannelType := Channel_NotOpened;
   Using_BASS_AAC := false;

   ExtCode := UpperCase(ExtractFileExt(URL));
   LoExtCode := LowerCase(ExtCode);

 //  NameHeader1 := copy(URL, 1, 7);
 //  NameHeader2 := copy(URL, 1, 6);


 //----------------------------- Check validity of URL -------------------------------

  // ASF and WMA files are streamed with Winamp input plug-in for the file types,
  //  only if basswma.dll is not loaded.
   if ((ExtCode = '.ASF') or (ExtCode = '.WMA')) and (not FBASSWMAReady) then
   begin
      if ExtCode = '.ASF' then
         PluginNum := GetPluginNumber('ASF')
      else
         PluginNum := GetPluginNumber('WMA');
      if PluginNum >= 0 then  // = A Winamp input plug-in which can play ASF (or WMA) files is loaded
      begin
         if not FPluginReady then
         begin
            if not CheckOnly then
            begin
              ShowErrorMsgBox('Cannot run Winamp input plug-ins. (Faild to get buffer) !');
              if Temp_Paused then
                 ResumePlay;
            end;
            exit;
         end;

         if not CheckOnly then
            SetupWinampPlugin(PluginNum);

         StreamInfo.SampleRate := 0;
         StreamInfo.Duration := 0;
      end else
      begin
         if not CheckOnly then
         begin
           ShowErrorMsgBox('Load BASSWMA.DLL or Winamp input plug-in for ASF/WMA file.');
           if Temp_Paused then
              ResumePlay;
         end;
         exit;
      end;
   end else
   begin
      if FSingleChannel then
         OpenFlag := 0
      else
         OpenFlag := BASS_STREAM_DECODE;

      if pos(ExtCode, MIDIFileExt) = 0 then  // not a MIDI file ?
      begin
        {$IFDEF DELPHI_2007_BELOW}
         tmpChannel := BASS_StreamCreateURL(pChar(URL), 0, OpenFlag, nil, nil);
        {$ELSE}
         tmpChannel := BASS_StreamCreateURL(ToPMultiByte(pWideChar(URL)), 0, OpenFlag, nil, nil);
        {$ENDIF}
         if tmpChannel <> 0 then
         begin
            BASS_ChannelGetInfo(tmpChannel, BassChannelInfo);
            if (BassChannelInfo.ctype = BASS_CTYPE_STREAM_WMA) or
               (BassChannelInfo.ctype = BASS_CTYPE_STREAM_WMA_MP3) then
                tmpChannelType := Channel_WMA
            else begin
               tmpChannelType := Channel_Stream;
               if (BassChannelInfo.ctype = BASS_CTYPE_STREAM_AAC) or
                  (BassChannelInfo.ctype = BASS_CTYPE_STREAM_MP4) then
                  Using_BASS_AAC := true;
            end;
         end;

      end else if FMIDISoundReady then
      begin
         FGetHTTPHeader := false;
        {$IFDEF DELPHI_2007_BELOW}
         tmpChannel := BASS_MIDI_StreamCreateURL(pChar(URL), 0,
                                                 OpenFlag + BASS_STREAM_STATUS,
                                                 @DownProc, @MessageHandle, 44100);
        {$ELSE}
         tmpChannel := BASS_MIDI_StreamCreateURL(ToPMultiByte(pWideChar(URL)), 0,
                                                 OpenFlag + BASS_STREAM_STATUS,
                                                 @DownProc, @MessageHandle, 44100);
        {$ENDIF}
         if tmpChannel <> 0 then
            tmpChannelType := Channel_MIDI;
      end else if FBASSMIDIReady then
         if not CheckOnly then
            ShowErrorMsgBox('None of sound font is loaded.');

      if tmpChannel <> 0 then
      begin
         if not CheckOnly then
         begin
            Close;
            DecodeChannel := tmpChannel;
            ChannelType := tmpChannelType;

       // BASSMIDI pre-downloads the entire MIDI file, and closes the file/connection
       // before the stream creation function(BASS_MIDI_StreamCreateURL) returns.
           if ChannelType = Channel_MIDI then
              FDownLoaded := true;
         end else
         begin
            case ChannelType of
               Channel_Music : BASS_MusicFree(tmpChannel);
               Channel_CD, Channel_Stream, Channel_MIDI, Channel_WMA :
                         BASS_StreamFree(tmpChannel);
            end;
         end;
      end else
      begin     // for tmpChannel = 0
      // Check if the file type of URL stream is playable file type by Winamp input plug-in.
      // note) Not all the Winamp input plug-ins support URL stream.
      //       So, Following code should be modified. (But I do not know the method to
      //       classify them.)
         if FPluginReady and (pos(LoExtCode, GetPluginFileExts) > 0) then
         begin
            if length(ExtCode) >= 3 then
               PluginNum := GetPluginNumber(copy(ExtCode, 2, length(ExtCode) - 1))
            else
               PluginNum := -1;
            if PluginNum = - 1 then
            begin
              if not CheckOnly then
              begin
                 ShowErrorMsgBox('Invalid or unsupported URL stream.'#10' -> ' + URL);
                 if Temp_Paused then
                    ResumePlay;
               end;
               exit;
            end;
            if not CheckOnly then
               SetupWinampPlugin(PluginNum);
            StreamInfo.SampleRate := 0;
            StreamInfo.Duration := 0;
         end else
         begin
            if not CheckOnly then
            begin
              ShowErrorMsgBox('Invalid or unsupported URL stream.'#10' -> ' + URL);
              if Temp_Paused then
                 ResumePlay;
            end;
            exit;
         end;
      end;
   end;

 //----------------------- End of checking validity of URL ----------------------------

   result := true;
   if CheckOnly then
      exit;

   if (pos(LoExtCode, GetNativeFileExts) > 0) or
      (pos(LoExtCode, GetBASSAddOnExts) > 0) or
      (pos(LoExtCode, GetPluginFileExts) > 0) then
   begin
      NetStream := true;

      if (ChannelType <> Channel_Plugin) and (ExtCode = '.OGG') then
      begin
         TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_OGG);
         if TagP <> nil then
         while StrLen(TagP) > 0 do
         begin
          {$IFDEF DELPHI_2007_BELOW}
            tmpStr := ansistring(TagP);
          {$ELSE}
            tmpStr := ToWideString(ansistring(TagP));
          {$ENDIF}
            if pos('TITLE=', tmpStr) <> 0 then
            begin
               StreamInfo.Title := copy(tmpStr, 7, length(tmpStr) - 6);
               break;
            end else if pos('ARTIST=', tmpStr) <> 0 then
               StreamInfo.Artist := copy(tmpStr, 8, length(tmpStr) - 7)
            else if pos('GENRE=', tmpStr) <> 0 then
               StreamInfo.Genre := copy(tmpStr, 7, length(tmpStr) - 6)
            else if pos('ALBUM=', tmpStr) <> 0 then
               StreamInfo.Album := copy(tmpStr, 7, length(tmpStr) - 6);

            inc(TagP, StrLen(TagP) + 1);
         end;

   // <-
      end else if (ChannelType = Channel_WMA) then
      begin
         TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_WMA);
         ICYTag := TagP;
         if TagP <> nil then
         while StrLen(TagP) > 0 do
         begin
            tmpStr1 := ansistring(TagP);
           {$IFDEF DELPHI_2007_BELOW}
            if pos('FileSize=', tmpStr1) <> 0 then
           {$ELSE}
            if posEX('FileSize=', tmpStr1, 1) <> 0 then
           {$ENDIF}
               StreamInfo.FileSize := strToInt(copy(tmpStr1, 10, length(tmpStr1) - 9));
            if copy(tmpStr1, 1, 8) = 'Bitrate=' then   // to exclude 'CurrentBitrate=', 'OptimalBitrate='
            begin
             //  SepPos := pos('=', tmpStr1);
               StreamInfo.Bitrate := strToInt(copy(tmpStr1, 9, length(tmpStr1) - 8)) div 1000;
            end;
           {$IFDEF DELPHI_2007_BELOW}
            if pos('Title=', tmpStr1) <> 0 then
           {$ELSE}
            if posEX('Title=', tmpStr1, 1) <> 0 then
           {$ENDIF}
            begin
              {$IFDEF DELPHI_2007_BELOW}
               tmpStr2 := UTF8Decode(tmpStr1);
              {$ELSE}
               tmpStr2 := UTF8ToWideString(tmpStr1);
              {$ENDIF}
               tmpPChar := ToPMultiByte(PWideChar(tmpStr2));
               tmpStr1 := ansistring(tmpPChar);
               StreamInfo.Title := copy(tmpStr1, 7, length(tmpStr1) - 6);
            end;
           {$IFDEF DELPHI_2007_BELOW}
            if pos('Author=', tmpStr1) <> 0 then
           {$ELSE}
            if posEX('Author=', tmpStr1, 1) <> 0 then
           {$ENDIF}
            begin
              {$IFDEF DELPHI_2007_BELOW}
               tmpStr2 := UTF8Decode(tmpStr1);
              {$ELSE}
               tmpStr2 := UTF8ToWideString(tmpStr1);
              {$ENDIF}

               tmpPChar := ToPMultiByte(PWideChar(tmpStr2));
               tmpStr1 := ansistring(tmpPChar);
               StreamInfo.Artist := copy(tmpStr1, 8, length(tmpStr1) - 7);
            end;
            inc(TagP, StrLen(TagP) + 1);
         end;  // <-

         if StreamInfo.Title = '' then
            StreamInfo.Title := ExtractFileName2(URL);

   // We can get the TITLE & Playback length of the stream files from internet
   // at just before the starting playback from Winamp input plug-in if decoded
   // by Winamp input plug-in.
      end else if (ChannelType <> Channel_Plugin) then
         StreamInfo.Title := ExtractFileName2(URL);

      if ChannelType <> Channel_Plugin then
      begin
         if ChannelType <> Channel_MIDI then
         begin
            TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_HTTP);
            HTTPTag := TagP;
         end else
         begin
            RepeatCounter := 0;
            repeat
               sleep(30);
               inc(RepeatCounter);
               WinProcessMessages;
            until FGetHTTPHeader or (RepeatCounter = 100);

            if FGetHTTPHeader then
               HTTPTag := tmpHTTPTag;
         end;

      // BASS_StreamGetLength may return a very rough estimation until the whole file
      // has been downloaded.
      // So it is needed to re-estimate after finishing download.
         PlaybackSec := BASS_ChannelBytes2Seconds(DecodeChannel, BASS_ChannelGetLength(DecodeChannel,
                                                     BASS_POS_BYTE));
         StreamInfo.Duration := round(1000 * PlaybackSec);  // in mili seconds
         FileLen := BASS_StreamGetFilePosition(DecodeChannel, BASS_FILEPOS_END); // file length
         if FileLen <> -1 then
         begin
            if ChannelType <> Channel_WMA then  // obtained at getting tag info for WMA stream
            begin
              StreamInfo.FileSize := FileLen;
              if PlaybackSec > 0 then
                 StreamInfo.BitRate := round(FileLen / (125 * PlaybackSec))   // bitrate (Kbps)
              else
                 StreamInfo.BitRate := 0;
            end;

          // BASS add-on "bass_spx.dll" does not issue Sync signal when downloading of the stream
          //  file from internet is done.
          // So, I use following compare clause to check if download process is done.
            if FileLen = BASS_StreamGetFilePosition(DecodeChannel, BASS_FILEPOS_DOWNLOAD) then
               FDownloaded := true;
         end;

         if not FDownloaded then
            BASS_ChannelSetSync(DecodeChannel, BASS_SYNC_DOWNLOAD, 0, @DownloadSync, @MessageHandle);
      end;
   end else   // = Shoutcast/Icecast server
   begin
      NetRadio := true;

      if (ChannelType = Channel_WMA) then
      begin
         TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_WMA);
         if TagP <> nil then
         while StrLen(TagP) > 0 do
         begin
            tmpStr1 := ansistring(TagP);
            if copy(tmpStr1, 1, 8) = 'Bitrate=' then   // to exclude 'CurrentBitrate=', 'OptimalBitrate='
            begin
             //  SepPos := pos('=', tmpStr1);
               StreamInfo.Bitrate := strToInt(copy(tmpStr1, 9, length(tmpStr1) - 8)) div 1000;
               break;
            end;

            inc(TagP, StrLen(TagP) + 1);
         end;

         TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_WMA_META);
         if TagP <> nil then
         begin
            tmpStr1 := ansistring(TagP);
           {$IFDEF DELPHI_2007_BELOW}
            if pos('Title=', tmpStr1) <> 0 then
           {$ELSE}
            if posEX('Title=', tmpStr1, 1) <> 0 then
           {$ENDIF}
               StreamInfo.Title := copy(tmpStr1, 7, length(tmpStr1) - 6);
         end;
      end else
      begin    // for ChannelType <> Channel_WMA
        TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_ICY);  // for Shoutcast
        if TagP = nil then
           TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_HTTP);  // for Icecast
        ICYTag := TagP;
        if TagP <> nil then
           while StrLen(TagP) > 0 do
           begin
            {$IFDEF DELPHI_2007_BELOW}
             if pos('icy-br:', ansistring(TagP)) <> 0 then
            {$ELSE}
             if posEX('icy-br:', ansistring(TagP), 1) <> 0 then
            {$ENDIF}
             begin
               inc(TagP, 7);
               StreamInfo.BitRate := strToInt(ansistring(TagP));
               break;
             end else
            {$IFDEF DELPHI_2007_BELOW}
             if pos('ice-bitrate:', ansistring(TagP)) <> 0 then
            {$ELSE}
             if posEX('ice-bitrate:', ansistring(TagP), 1) <> 0 then
            {$ENDIF}
             begin
               inc(TagP, 12);
               StreamInfo.BitRate := strToInt(ansistring(TagP));
               break;
             end;

             inc(TagP, StrLen(TagP) + 1);
           end;

        TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_META);
        if TagP <> nil then
        begin
          tmpStr1 := ansistring(TagP);
         {$IFDEF DELPHI_2007_BELOW}
          TitlePos := Pos('StreamTitle=', tmpStr1);
         {$ELSE}
          TitlePos := PosEX('StreamTitle=', tmpStr1, 1);
         {$ENDIF}
        end else
          TitlePos := 0;

        if TitlePos <> 0 then
        begin
          {$IFDEF DELPHI_2007_BELOW}
          DelimeterPos := Pos(';', tmpStr1);
          {$ELSE}
          DelimeterPos := PosEx(';', tmpStr1);
          {$ENDIF}
          if DelimeterPos = 0 then
             StreamInfo.Title := copy(tmpStr1, TitlePos + 13, length(tmpStr1) - TitlePos - 13)
          else
             StreamInfo.Title := copy(tmpStr1, TitlePos + 13, DelimeterPos - TitlePos - 14);
        end else
          StreamInfo.Title := '';

      end;

      MetaSyncParam.ChannelType := ChannelType;
      if (ChannelType = Channel_WMA) then
      begin
         BASS_ChannelSetSync(DecodeChannel, BASS_SYNC_WMA_META, 0, @MetaSync, @MetaSyncParam);
         BASS_ChannelSetSync(DecodeChannel, BASS_SYNC_WMA_CHANGE, 0, @MetaSync, @MetaSyncParam);
      end else
         BASS_ChannelSetSync(DecodeChannel, BASS_SYNC_META, 0, @MetaSync, @MetaSyncParam);
   end;

   if (ChannelType <> Channel_Plugin) then
   begin
      BASS_ChannelGetInfo(DecodeChannel, BassChannelInfo);
      StreamInfo.SampleRate := BassChannelInfo.freq;
      StreamInfo.Channels := BassChannelInfo.chans;
      StreamInfo.Format := BassChannelInfo.ctype;

   // Apply mixer if source has multi channels.   (* Added at Ver 2.00)
      if (StreamInfo.Channels > 2) and FDownMixToStereo then
         if FMixerReady and (not FSingleChannel) then
         begin
         // create stereo mixer
            if HMixer = 0 then
               HMixer := BASS_Mixer_StreamCreate(BassChannelInfo.freq, 2, BASS_MIXER_END or BASS_STREAM_DECODE);
            if HMixer <> 0 then
             // plug in the decoder
               FMixerPlugged := BASS_Mixer_StreamAddChannel(HMixer, DecodeChannel, BASS_MIXER_DOWNMIX);
         end;

      if (pos(LoExtCode, GetNativeFileExts) > 0) or NetRadio then
      begin
         case ChannelType of
           Channel_CD : FDecoderName := 'basscd.dll';
           Channel_MIDI : FDecoderName := 'bassmidi.dll';
           Channel_WMA : FDecoderName := 'basswma.dll';
           else
             if Using_BASS_AAC then
                FDecoderName := 'bass_aac.dll'
              else
                FDecoderName := 'bass.dll';
           end;
      end else
         FDecoderName := GetDecoderName(lowercase(ExtCode));

      SupportedBy := BASSNative;
   end else
      SupportedBy := WinampPlugin;
end;

function TBASSPlayer.IsValidURL(URL : string) : boolean;
var
   StreamInfo : TStreamInfo;
   SupportedBy : TSupportedBy;
begin
   result := OpenURL(URL, StreamInfo, SupportedBy, false, true);
end;

function TBASSPlayer.Open(StreamName : string) : boolean;
var
   IsMusicFile : boolean;
   FromNet : boolean;
   StreamInfo : TStreamInfo;
   OpenFlag : DWORD;
   tmpChannel : DWORD;
   tmpChannelType : TChannelType;
   SupportedBy : TSupportedBy;
   APluginInfo : TPluginInfo;
   PluginNum : integer;
   StreamName_ : string;
   PluginName : string;
   NameHeader1, NameHeader2 : string;
   ExtCode : string;
   tmpPaused : boolean;
   Using_BASS_AAC : boolean;

 // get the file name from the name of an URL stream
 // (ex: http://.../.../file_name.mp3 -> file_name)
  function ExtractFileName2(NetStream : string) : string;
  var
    pos, i : integer;
  begin
     pos := 0;
     for i := (length(NetStream) - 6)  downto 6 do
        if NetStream[i] = '/' then
        begin
           pos := i;
           break;
        end;

     if pos > 0 then
        result := copy(NetStream, pos + 1, length(NetStream) - pos - 4)
     else
        result := '';
  end;

  procedure SetupWinampPlugin(PlugNumber : integer);
  begin
     Close;
     ChannelType := Channel_Plugin;
     FPluginNum := PlugNumber;
     APlugin := GetPlugin(FPluginNum);
     SelectInputPlugin(FPluginNum);
     APluginInfo := GetPluginInfo(FPluginNum);
     FDecoderName := APluginInfo.Name;
     PluginConfigForm.SetInUsePlugin(ActivePlugin{APluginInfo.Name});  // inform PluginConfig.pas
     FDecodingByPlugin := true;
  end;

begin
   result := false;

   if not FBASSReady then
   begin
      ShowErrorMsgBox('Player is not ready !');
      exit;
   end;

   FromNet := false;

   StreamName_ := StreamName;
   FillChar(StreamInfo, sizeOf(StreamInfo), 0);
   
 // remove preceding and trailing '"'.
   if StreamName_[1] = '"' then
      StreamName_ := copy(StreamName_, 2, length(StreamName_) - 1);
   if StreamName_[length(StreamName_)] = '"' then
      StreamName_ := copy(StreamName_, 1, length(StreamName_) - 1);

   NameHeader1 := copy(StreamName_, 1, 7);
   NameHeader2 := copy(StreamName_, 1, 6);
   if (NameHeader1 = 'http://') or (NameHeader2 = 'mms://') then
   begin
      FromNet := true;
      StreamInfo.FileName := StreamName_;
      StreamInfo.BitRate := 0;
   end
   else if not GetStreamInfo2(StreamName_, StreamInfo, SupportedBy, PluginNum, PluginName) then
   begin
      ShowErrorMsgBox('Invalid or unsupported stream file.'#10' -> ' + StreamName);
      exit;
   end;
   if StreamInfo.Channels > MaxChannels then  // Will this case happen ?
   begin
      ShowErrorMsgBox('Channel count exceeds. (Max Channels : ' + intToStr(MaxChannels)
                        + ')'#10 + ' -> ' + StreamName);
      exit;
   end;

 //  StreamInfo.Format := 0;  // not determined yet.

   ExtCode := UpperCase(ExtractFileExt(StreamName_));
   if length(ExtCode) = 1 then  // ExtCode = '.' ; the last character of StreamName_ is '.'
      ExtCode := '';            // to prevent mis decision
   if length(ExtCode) >= 3 then
      if pos(ExtCode, MusicFileExt) > 0 then
         IsMusicFile := true
      else
         IsMusicFile := false
   else
      IsMusicFile := false;

  // Pause previously assigned channel only if it is for net radio station.
   tmpPaused := false;
   if ChannelType <> Channel_NotOpened then
   //   if NetRadio then
         if FPlayerMode = plmPlaying then
         begin
            tmpPaused := true;
            PausePlay;
         end;

   tmpChannelType := Channel_NotOpened;
   Using_BASS_AAC := false;
   
   if FromNet then   // The streams from internet.
   begin
      if not OpenURL(StreamName_, StreamInfo, SupportedBy,
                     tmpPaused, false{CheckOnly}) then
         exit;
   end

 // for local stream files
   else if FPluginReady and
       ((SupportedBy = WinampPlugin) or ((SupportedBy = Both) and FPluginFirst)) then
      SetupWinampPlugin(PluginNum)
   else
   begin
      if (ExtCode = '.CDA') then
      begin   // (ExtCode = '.CDA')
         if FBASSCDReady then
            if FSingleChannel then
             {$IFDEF DELPHI_2007_BELOW}
               tmpChannel := BASS_CD_StreamCreateFile(PChar(StreamName_), 0)
             {$ELSE}
               tmpChannel := BASS_CD_StreamCreateFile(PChar(StreamName_), BASS_UNICODE)
             {$ENDIF}
            else
             {$IFDEF DELPHI_2007_BELOW}
               tmpChannel := BASS_CD_StreamCreateFile(PChar(StreamName_), BASS_STREAM_DECODE)
             {$ELSE}
               tmpChannel := BASS_CD_StreamCreateFile(PChar(StreamName_), BASS_STREAM_DECODE or BASS_UNICODE)
             {$ENDIF}
         else
            tmpChannel := 0;
         if (tmpChannel <> 0) then
            tmpChannelType := Channel_CD;
      end else
      begin
         if IsMusicFile then
         begin
            if FSingleChannel then
               OpenFlag := BASS_MUSIC_PRESCAN + BASS_MUSIC_POSRESET + BASS_MUSIC_RAMPS
            else
               OpenFlag := BASS_MUSIC_PRESCAN + BASS_MUSIC_POSRESET + BASS_MUSIC_RAMPS + BASS_STREAM_DECODE;
          {$IFDEF DELPHI_2007_BELOW}
            tmpChannel := BASS_MusicLoad(FALSE, PChar(StreamName_), 0, 0, OpenFlag, 0);
          {$ELSE}
            tmpChannel := BASS_MusicLoad(FALSE, PChar(StreamName_), 0, 0, OpenFlag or BASS_UNICODE, 0);
          {$ENDIF}
            if (tmpChannel <> 0) then
            begin
               tmpChannelType := Channel_Music;
               StreamInfo.Duration := PlayLength;
             //  RestartPos := 0;
            end;
         end else if pos(ExtCode, MIDIFileExt) <> 0 then  // is a MIDI file ?
         begin
            if FMIDISoundReady then
               if FSingleChannel then
                 {$IFDEF DELPHI_2007_BELOW}
                  tmpChannel := BASS_MIDI_StreamCreateFile(FALSE, pChar(StreamName_), 0, 0, 0, 44100)
                 {$ELSE}
                  tmpChannel := BASS_MIDI_StreamCreateFile(FALSE, pChar(StreamName_), 0, 0, BASS_UNICODE, 44100)
                 {$ENDIF}
               else
                 {$IFDEF DELPHI_2007_BELOW}
                  tmpChannel := BASS_MIDI_StreamCreateFile(FALSE, pChar(StreamName_), 0, 0, BASS_STREAM_DECODE, 44100)
                 {$ELSE}
                  tmpChannel := BASS_MIDI_StreamCreateFile(FALSE, pChar(StreamName_), 0, 0, BASS_STREAM_DECODE or BASS_UNICODE, 44100)
                 {$ENDIF}
            else begin
               tmpChannel := 0;
               if FBASSMIDIReady then
                  ShowErrorMsgBox('None of sound font is loaded.')
            end;

            if tmpChannel <> 0 then
            begin
               tmpChannelType := Channel_MIDI;
               StreamInfo.Duration := PlayLength;
            end;
         end else
         begin
            if FSingleChannel then
              {$IFDEF DELPHI_2007_BELOW}
               tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName_), 0, 0, 0)
              {$ELSE}
               tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName_), 0, 0, BASS_UNICODE)
              {$ENDIF}
            else
              {$IFDEF DELPHI_2007_BELOW}
               tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName_), 0, 0, BASS_STREAM_DECODE);
              {$ELSE}
               tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName_), 0, 0, BASS_STREAM_DECODE or BASS_UNICODE);
              {$ENDIF}
            if (tmpChannel <> 0) then
            begin
               BASS_ChannelGetInfo(tmpChannel, BassChannelInfo);
               if (BassChannelInfo.ctype = BASS_CTYPE_STREAM_WMA) or
                  (BassChannelInfo.ctype = BASS_CTYPE_STREAM_WMA_MP3) then
                   tmpChannelType := Channel_WMA
               else begin
                  if (BassChannelInfo.ctype = BASS_CTYPE_STREAM_AAC) or
                     (BassChannelInfo.ctype = BASS_CTYPE_STREAM_MP4) then
                     Using_BASS_AAC := true;
                  tmpChannelType := Channel_Stream;
               end;
            end;
         end;
      end;

      if (tmpChannel = 0) then  // not a playable file
      begin
         ShowErrorMsgBox('Invalid or unsupported stream file.'#10' -> ' + StreamName);
         if tmpPaused then
            ResumePlay;
         exit;
      end else
      begin
         Close;
         DecodeChannel := tmpChannel;
         ChannelType := tmpChannelType;
      end;

      BASS_ChannelGetInfo(DecodeChannel, BassChannelInfo);
      StreamInfo.SampleRate := BassChannelInfo.freq;
      StreamInfo.Channels := BassChannelInfo.chans;
      StreamInfo.Format := BassChannelInfo.ctype;

    // Following codes are not necessary in most cases
      if (StreamInfo.BitsPerSample = 32) or
         ((StreamInfo.BitsPerSample = 16) and
         ((BassChannelInfo.flags and BASS_SAMPLE_8BITS) = BASS_SAMPLE_8BITS)) then
      begin
         BASS_MusicFree(DecodeChannel);
         BASS_StreamFree(DecodeChannel);
         if (StreamInfo.BitsPerSample = 32) then
            ShowErrorMsgBox('TBASSPlayer does not support 32-bit sources.')
         else
            ShowErrorMsgBox('Sound card does not support 16-bit sources.');
         if tmpPaused then
            ResumePlay;
         exit;
      end;

      // Apply mixer if source has multi channels. (* Added at Ver 2.00)
      if (StreamInfo.Channels > 2) and FDownMixToStereo then
         if FMixerReady and (not FSingleChannel) then
         begin
         // create stereo mixer
            if HMixer = 0 then
               HMixer := BASS_Mixer_StreamCreate(BassChannelInfo.freq, 2, BASS_MIXER_END or BASS_STREAM_DECODE);
            if HMixer <> 0 then
             // plug in the decoder
               FMixerPlugged := BASS_Mixer_StreamAddChannel(HMixer, DecodeChannel, BASS_MIXER_DOWNMIX);
         end;

      if pos(LowerCase(ExtCode), GetNativeFileExts) > 0 then
      begin
         case ChannelType of
           Channel_CD : FDecoderName := 'basscd.dll';
           Channel_MIDI : FDecoderName := 'bassmidi.dll';
           Channel_WMA : FDecoderName := 'basswma.dll';
           else
             if Using_BASS_AAC then
                FDecoderName := 'bass_aac.dll'
              else
                FDecoderName := 'bass.dll';
           end;
      end else
         FDecoderName := GetDecoderName(lowercase(ExtCode));

   end;   // end of for 'local stream'

   FSingleChannel2 := FSingleChannel;
   FStreamName := StreamName_;
   FSupportedBy := SupportedBy;
   FStreamInfo := StreamInfo;
   InformPlayerStatus(InformStreamInfo);
   SetPlayerMode(plmReady);
   result := true;
end;


procedure TBASSPlayer.Play;
var
   StartOK : boolean;
   i : integer;
   tmpChannels : Word;

  function WaitBuffering(Channel : DWORD) : boolean;   // * Added at Ver 2.00
  var
    progress, len : DWORD;
    ExitBuffering : boolean;

  begin
    ExitBuffering := false;
    if (BASS_StreamGetFilePosition(Channel, BASS_FILEPOS_WMA_BUFFER) <> -1) then // it's a WMA stream
      repeat
     // progress = percentage of buffer filled
        progress := BASS_StreamGetFilePosition(Channel, BASS_FILEPOS_WMA_BUFFER);
        if (progress = dword(-1)) then
           ExitBuffering := true; // something's gone wrong! (eg. BASS_Free called)
        if (progress = 100) then
           break; // full
        WinProcessMessages;
        Sleep(20);
      until ExitBuffering
    else  // Shoutcast/Icecast stream
      repeat
        len := BASS_StreamGetFilePosition(Channel, BASS_FILEPOS_END);
        if (len = dword(-1)) then
            ExitBuffering := true; // something's gone wrong! (eg. BASS_Free called)
     // progress = percentage of buffer filled
        progress := round((BASS_StreamGetFilePosition(Channel, BASS_FILEPOS_DOWNLOAD) -
                          BASS_StreamGetFilePosition(Channel, BASS_FILEPOS_CURRENT)) * 100 / len);
        if (progress > 75) then
           break; // over 75% full, enough
        WinProcessMessages;
        Sleep(20);
      until ExitBuffering;

    if ExitBuffering then
    begin
       result := false;
       ShowErrorMsgBox('Error at buffering process.');
       exit;
    end else
       result := true;
  end;

begin
   if NowStarting then   // avoid duplicate Play operation
      exit
   else if ChannelType = Channel_NotOpened then
      exit
   else if FPlayerMode = plmPlaying then
      exit
   else if FPlayerMode = plmPaused then
   begin
      ResumePlay;
      exit;
   end else if FPlayerMode = plmStopped then
   begin
      if (ChannelType <> Channel_Plugin) then
      begin
         SetPosition(0);   // Set playback position to starting point
         if CurrentPosition = PlayLength then
         begin
            ShowErrorMsgBox('Playback reposition of opened stream is not allowed.');
            exit;
         end;
      end;
      Restart;
      exit;
   end;

   StartOK := false;
   for i := 0 to (NumFFTBands - 1) do
      BandOut[i] := 0;

 // play an opened channel
   if ChannelType = Channel_Plugin then
   begin
      NowStarting := true;
      UsesOutputPlugin := false;
      PlayChannel := 1;  // for Winamp input plug-in which does not need output plug-in
   //   SetSongDuration(FStreamInfo.Duration);
    {$IFDEF DELPHI_2007_BELOW}
      if APlugin.Play(pAnsiChar(FStreamName)) = 0 then
    {$ELSE}
      if APlugin.Play(ToPMultiByte(pWideChar(FStreamName))) = 0 then
    {$ENDIF}
      begin
        // some Winamp input plug-ins starts with minimum volume (ex: in_cdda.dll)
        // APlugin.SetVolume(FOutputVolume);
       {  wCycle := 0;
         while wCycle < 60 do
         begin
            if GetOutputTime(false) > 200 then
            begin }
               StartOK := true;
              { break;
            end;
            WinProcessMessages;
            inc(wCycle);
            sleep(50);
         end; }
      end;
      if not StartOK then
      begin
         ShowErrorMsgBox('Can''t play the specified stream.'#10' -> ' + FStreamName);
         SelectInputPlugin(-1);
         PluginConfigForm.SetInUsePlugin('');
         FDecoderName := '';
      end;
      NowStarting := false;
   end else
   begin
      if PlayChannel <> 0 then
         BASS_StreamFree(PlayChannel);
      ClearEffectHandle;

      if FMixerPlugged then
         tmpChannels := 2
      else
         tmpChannels := FStreamInfo.Channels;

      if FSingleChannel2 then
         PlayChannel := DecodeChannel
      else
      begin
         if NetRadio then   // * Added at Ver 2.00
         begin
            if not WaitBuffering(DecodeChannel) then
            begin
               FDecoderName := '';
               exit;
            end;
         end
      // WMA files from internet require buffering process.
         else if NetStream and (ChannelType = Channel_WMA) then
            if not WaitBuffering(DecodeChannel) then
            begin
               FDecoderName := '';
               exit;
            end;

         if (BassChannelInfo.flags and BASS_SAMPLE_8BITS) = BASS_SAMPLE_8BITS then
            if FMixerPlugged then
               PlayChannel := omodOpen2(HMixer, DecodeChannel, BassChannelInfo.freq,
                                        tmpChannels, 8, NetRadio)
            else
               PlayChannel := omodOpen2(DecodeChannel, DecodeChannel, BassChannelInfo.freq,
                                        tmpChannels, 8, NetRadio)
         else
            if FMixerPlugged then
               PlayChannel := omodOpen2(HMixer, DecodeChannel, BassChannelInfo.freq,
                                        tmpChannels, 16, NetRadio)
            else
               PlayChannel := omodOpen2(DecodeChannel, DecodeChannel, BassChannelInfo.freq,
                                        tmpChannels, 16, NetRadio);
      end;

      if PlayChannel = 0 then
      begin
         ShowErrorMsgBox('Could not open the channel for play.');
         FDecoderName := '';
         exit;
      end;

      SetSoundEffect(FSoundEffects);  // Apply equalyzation and sound effects
      VisDataThread.SetChannelInfo(ChannelType, DecodeChannel, PlayChannel,
                                    BassChannelInfo.freq, tmpChannels);
    // inform DSP plug-in control unit
    //  if DSPActive then  // may be needed pre-setting
         SetChannelInfo2(ChannelType, DecodeChannel, PlayChannel,
                                      FStreamInfo.SampleRate, tmpChannels);

      StartOK := BASS_ChannelPlay(PlayChannel, true);
   end;

   if StartOK then
   begin
      if ChannelType <> Channel_Plugin then
         BASS_ChannelSetSync(DecodeChannel, BASS_SYNC_END, 0, @PlayEndSync, @MessageHandle);
      if ChannelType = Channel_MIDI then
      begin
         LyricSyncParam.Channel := DecodeChannel;
         BASS_ChannelSetSync(DecodeChannel, BASS_SYNC_MIDI_LYRIC, 0, @LyricSync, @LyricSyncParam);
      end;
    //  if ChannelType = Channel_Music then
    //     MusicStartTime := timeGetTime;

      SetPlayerMode(plmPlaying);
      if (ChannelType <> Channel_Plugin) then
         if Assigned(FOnNewFFTData) then
          //  if VisWindowHandle = 0 then  // vis plug-in is not active ?
               TimerFFT.Enabled := true;
   end;
end;

function TBASSPlayer.MutedTemporary : boolean;
begin
   if not FMute then
   begin
     SetMuteState(true, 500);   // Mute
     repeat
        Sleep(20);
        WinProcessMessages;
     until (not AttribSliding);

     result := true;
   end else
     result := false;
end;

procedure TBASSPlayer.RestoreFromMutedState;
begin
   BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, FOutputVolume * 39);
   BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, FOutputVolume * 39);
   BASS_ChannelSetAttribute(PlayChannel, BASS_ATTRIB_VOL, 1);
   FMute := false;
end;

procedure TBASSPlayer.PausePlay;
var
   tmpMuted : boolean;
begin
   if FPlayerMode <> plmPlaying then
      exit;

   if MutedTemporary then
      tmpMuted := true
   else
      tmpMuted := false;

   if ChannelType = Channel_Plugin then
      APlugin.Pause
   else
      BASS_ChannelPause(PlayChannel);

 //  if ChannelType = Channel_Music then
 //     MusicPauseTime := timeGetTime - MusicStartTime;
   if tmpMuted then
      RestoreFromMutedState;

   SetPlayerMode(plmPaused);
end;

procedure TBASSPlayer.ResumePlay;
var
   tmpMuted : boolean;
   i : integer;
   ResumePos : DWORD;
begin
   if FPlayerMode <> plmPaused then
      exit;

   if not FMute then
   begin
     SetMuteState(true, 0);   // Mute instantly - no fade-out time is needed (not playing state)
     tmpMuted := true;
   end else
     tmpMuted := false;

 // Compensate non-played sound data in buffer, unheard time during fade-in, fade-out
   ResumePos := CurrentPosition;
   if ResumePos > 500 then
      ResumePos := ResumePos - 500    // backward 0.5 Sec
   else
      ResumePos := 0;

   SetPosition(ResumePos);

 // It is not possible to resume the paused stream from NetRadio. (-> required real time processing.)
 // So, we should restart it.
 // note) Normal restart (= resume playing the previously opened stream) causes problem.
   if NetRadio then   // * Added at Ver 2.00
   begin
      if Open(FStreamName) then    // re-open for clean restart
         Play;                     // then play
      if tmpMuted then
         SetMuteState(false, 500);
      exit;
   end;

   for i := 0 to (NumFFTBands - 1) do
      BandOut[i] := 0;

 // * Fadeout, Fadein 기능을 추가한 후 Pause 후 Resume시에 간혹 연주음 대신 잡음이 출력되는
 //    문제가 발생됨.
 //   이 문제는 Resume시 버퍼를 클리어 하면 나타나지 않으므로 NeedFlush 값을 True로 세트함.
 //    2009-01-25
   NeedFlush := true;

   if ChannelType = Channel_Plugin then
      APlugin.UnPause
   else if NeedFlush then
   begin
      if not FSingleChannel2 then
         ClearBuffer;
      BASS_ChannelPlay(PlayChannel, true);
      NeedFlush := false;
   end else
      BASS_ChannelPlay(PlayChannel, false);

 //  if ChannelType = Channel_Music then
 //     MusicStartTime := timeGetTime - MusicPauseTime;
   if tmpMuted then
      SetMuteState(false, 500);
   SetPlayerMode(plmPlaying);

   if Assigned(FOnNewFFTData) then
      if ChannelType <> Channel_Plugin then
      begin
       //  if VisWindowHandle = 0 then  // vis plug-in is not active ?
            TimerFFT.Enabled := true
      end else if UsesOutputPlugin then
       //  if VisWindowHandle = 0 then  // vis plug-in is not active ?
            TimerFFT.Enabled := true;
end;

procedure TBASSPlayer.Pause(pAction : boolean);
begin
   if pAction then
      PausePlay
   else
      ResumePlay;
end;

procedure TBASSPlayer.Stop;
var
   tmpMuted : boolean;
begin
   if (FPlayerMode <> plmPlaying) and (FPlayerMode <> plmPaused) then  // * Changed at 2007-07-25
      exit;

   if MutedTemporary then
     tmpMuted := true
   else
     tmpMuted := false;

   if ChannelType = Channel_Plugin then
   begin
      if PlayChannel <> 0 then
      begin
         APlugin.Stop;
         PlayChannel := 0;
         ClearEffectHandle;
      end
   end else
      BASS_ChannelStop(PlayChannel);

   if tmpMuted then
      RestoreFromMutedState;

   SetPlayerMode(plmStopped);
end;

procedure TBASSPlayer.Restart;
var
   StartOK : boolean;
 //  wCycle : integer;
   i : integer;
   tmpMuted : boolean;
begin
   if (FPlayerMode <> plmStopped) then
       exit;

   if NowStarting then
      exit;

 // If Stopped or Paused stream is the type of NetRadio then we should re-open the stream.
 // note) Normal restart (= resume playing the previously opened stream) causes problem.
   if NetRadio then  // * Added at Ver 2.00
   begin
      if not FMute then
      begin
        SetMuteState(true, 0);   // Mute instantly - no fade-out time is needed (not playing state)
        tmpMuted := true;
      end else
        tmpMuted := false;

      if Open(FStreamName) then  // re-open for clean restart
         Play;                   // then re-start

      if tmpMuted then
         SetMuteState(false, 500);
      exit;
   end;

   StartOK := false;

   for i := 0 to (NumFFTBands - 1) do
      BandOut[i] := 0;

  // play a opened channel
   if ChannelType = Channel_Plugin then
   begin
      NowStarting := true;
      UsesOutputPlugin := false;
      PlayChannel := 1;  // for Winamp input plug-in which does not need output plug-in
     {$IFDEF DELPHI_2007_BELOW}
      if APlugin.Play(pAnsiChar(FStreamName)) = 0 then
     {$ELSE}
      if APlugin.Play(ToPMultiByte(pWideChar(FStreamName))) = 0 then
     {$ENDIF}
      begin
   //  some Winamp input plug-ins starts with minimum volume (ex: in_cdda.dll)
     //    APlugin.SetVolume(FOutputVolume);
        { wCycle := 0;
         while wCycle < 60 do
         begin
            if GetOutputTime(false) > 200 then
            begin }
               StartOK := true;
              { break;
            end;
            WinProcessMessages;
            inc(wCycle);
            sleep(50);
         end; }
      end;
      NowStarting := false;
   end else
   begin
      if not FSingleChannel2 then
         ClearBuffer;
      StartOK := BASS_ChannelPlay(PlayChannel, true);
   end;

   if StartOK then
   begin
      SetPlayerMode(plmPlaying);
      if (ChannelType <> Channel_Plugin) then
         if Assigned(FOnNewFFTData) then
          //  if VisWindowHandle = 0 then  // vis plug-in is not active ?
               TimerFFT.Enabled := true;
   end;
end;

procedure TBASSPlayer.Close;
begin
   if ChannelType = Channel_NotOpened then
      exit;

   if ChannelType <> Channel_Plugin then
      if not FSingleChannel2 then
         omodClose2;   // close playing channel

   if FMixerPlugged then
   begin
      BASS_Mixer_ChannelRemove(DecodeChannel);
      FMixerPlugged := false;
   end;

   case ChannelType of
      Channel_Music : BASS_MusicFree(DecodeChannel);
      Channel_CD, Channel_Stream, Channel_MIDI, Channel_WMA :
                       BASS_StreamFree(DecodeChannel);
      Channel_Plugin : begin
                          if PlayChannel <> 0 then
                             APlugin.Stop;
                          FPluginNum := -1;
                          APlugin := nil;
                          SelectInputPlugin(-1);
                          PluginConfigForm.SetInUsePlugin('');
                       end;
   end;

   PlayChannel := 0;
   ChannelType := Channel_NotOpened;
   FStreamName := '';
   FStreamInfo.FileName := '';
   FStreamInfo.FileSize := 0;
   FStreamInfo.SampleRate := 0;
   ClearEffectHandle;
   FDecodingByPlugin := false;
 //  FUsing_BASS_AAC := false;
   FDecoderName := '';
   FDownLoaded := false;
   NetStream := false;
   NetRadio  := false;
   ICYTag    := nil;
   HTTPTag := nil;

   if FPlayerMode <> plmStandby then
      SetPlayerMode(plmStandby);
end;


function TBASSPlayer.GetChannelFFTData(PFFTData : pointer; FFTFlag : DWORD) : boolean;
begin
   result := false;

   if BASS_ChannelIsActive(PlayChannel) <> BASS_ACTIVE_PLAYING then
      exit;

   if (FFTFlag = BASS_DATA_FFT512) or
      (FFTFlag = BASS_DATA_FFT512 + BASS_DATA_FFT_INDIVIDUAL) or
      (FFTFlag = BASS_DATA_FFT1024) or
      (FFTFlag = BASS_DATA_FFT1024 + BASS_DATA_FFT_INDIVIDUAL) or
      (FFTFlag = BASS_DATA_FFT2048) or
      (FFTFlag = BASS_DATA_FFT2048 + BASS_DATA_FFT_INDIVIDUAL) then
      if BASS_ChannelGetData(PlayChannel, PFFTData, FFTFlag) <> DW_ERROR {-1} then
         result := true;
end;


// If you have any idea on spectrum visualization for better result,
// please let me know.
procedure TBASSPlayer.TimerFFTTimer(Sender: TObject);
const
 // TBASSplayer uses the result of FFT per 512 time-domain samples.
 // If the sampling rate is (standard) 44100Hz then FFTData[x] - the result of FFT, i.e,
 // frequency-domain samples - represents the intensity of frequency 44100 / 512 * x (Hz).
 // We need to classify frequency-domain samples(= FFTData[x]) by the number of required
 // bands for spectrum visualization.
 // FreqCoef defines the last index number of FFTData[x] for each band.
   FreqCoef : array[0..NumFFTBands - 1] of word =
               (  1,   2,   3,   6,       // narrow interval for low freq. range
                 12,  18,  24,  30,  36,  // normal interval for middle freq. range
                 42,  48,  54,  60,  66,
                 72,  78,  84,  90,  96,
                102, 108,
                120, 132, 156, 180 );     // wide interval for high freq. range

  // If the sampling rate is (standard) 44100Hz then the representative(or maximum)
  // frequency of each band is as followings
  //  BandOut[0] : 86.1Hz     BandOut[1] : 172.3Hz    BandOut[2] : 344.5Hz
  //  BandOut[3] : 516.8Hz    BandOut[4] : 1033.6Hz   BandOut[5] : 1550.4Hz
  //  . . . (516.8Hz interval upto BandOut[20]) . .   BandOut[20] : 9302.2Hz
  //  BandOut[21] : 10336Hz   BandOut[22] : 11370Hz   BandOut[23] : 13437Hz
  //  BandOut[24] : 15503Hz   . . very high frequency range is excluded

   Boost = 0.15; // Factor to intensify high frequency bands
   Scale = 80;   // Factor to calibrate the output range of BandOut[x]
                 //   ( approximately from 0 to 24 ).
var
   FFTData : TFFTData;
   NewBandOut : TBandOut;
   StartIndex : integer;
   i, k : integer;
   tmpIntensity : double;
 //  PlaybackPosition : DWORD;

begin
   if (Mode <> plmPlaying) {or (VisWindowHandle <> 0)} then
   begin
      TimerFFT.Enabled := false;
      exit;
   end;

   TimerFFT.Enabled := false;  // Avoid duplicate timer interrupt

   if BASS_ChannelIsActive(PlayChannel) <> BASS_ACTIVE_PLAYING then
      if BASS_ChannelIsActive(PlayChannel) <> BASS_ACTIVE_STALLED then
         exit;   // exit without re-enabling TimerFFT

   if BASS_ChannelGetData(PlayChannel, @FFTData, BASS_DATA_FFT512) = DW_ERROR {-1} then
   begin
      for i := 0 to (NumFFTBands - 1) do
         BandOut[i] := 0;

     // if VisWindowHandle = 0 then
         TimerFFT.Enabled := true;
      exit;
   end;

   for i := 0 to (NumFFTBands - 1) do
   begin
       if i = 0 then
          StartIndex := 1
       else
          StartIndex := FreqCoef[i-1] + 1;

    // tmpIntensity <= the greatest value in a group of classified FFTData[x]'s
       tmpIntensity := 0;
       for k := StartIndex to FreqCoef[i] do
           if FFTData[k] > tmpIntensity then
              tmpIntensity := FFTData[k];

       NewBandOut[i] := round(tmpIntensity * (1 + i * Boost) * Scale);

    // Decrease the value of BandOut[i] smoothly for better looking
       if NewBandOut[i] >= BandOut[i] then
          BandOut[i] := NewBandOut[i]
       else
          if BandOut[i] >= 2 then
             dec(BandOut[i], 2)
          else
             BandOut[i] := 0;

    // for the case that original NewBandOut[i] is smaller than BandOut[i] by 1
       if NewBandOut[i] > BandOut[i] then
          BandOut[i] := NewBandOut[i];
   end;

 //  if not GoVisOut then
      PostMessage(MessageHandle, WM_NewFFTData, 0, 0);

 // * Changed at Ver 1.92 : Each thread has its own function to get PlaybackPosition
 //  PlaybackPosition := CurrentPosition;
 //  if DSPActive then
 //     SetPosition2(PlaybackPosition);
 //  if GoVisOut then
 //     VisDataThread.SetPosition(PlaybackPosition);

 //  if VisWindowHandle = 0 then
      TimerFFT.Enabled := true;
end;

{ procedure TBASSPlayer.TimerTitleBarTimer(Sender: TObject);  // * Added at Ver 2.00
begin
   if FVisWindowAttr.VisHandle = 0 then
   begin
      TimerTitleBar.Enabled := false;
      exit;
   end;
   if FVisWindowAttr.VisType <> OnCreatedByGPP then
   begin
      TimerTitleBar.Enabled := false;
      exit;
   end;

 // Forces the highlighted state of title bar according to the focused state of EMBED window
 //  or vis window. (if EMBED window or vis window is focused then the title bar of EMBED
 //  window is highlighted, else not highlighted.)
   VisDataThread.InformVisDriver(CheckTitleBar, 0);
end; }

procedure TBASSPlayer.ShowPluginConfigForm;
begin
   if not FBASSReady then
   begin
      ShowErrorMsgBox('Player is not ready !');
      exit;
   end else if not FPluginReady then
      ShowErrorMsgBox('Cannot run Winamp input plug-ins. (Faild to get buffer)!');

   if FPluginFirst then
      PluginConfigForm.cbPluginFirst.Checked := true
   else
      PluginConfigForm.cbPluginFirst.Checked := false;

   PluginConfigForm.ShowModal;
end;

procedure TBASSPlayer.HideVisWindow;
begin
   if not GoVisOut then
      exit;

   VisDataThread.InformVisDriver(MinimizeWindow, 0);
end;

procedure TBASSPlayer.ShowVisWindow;
begin
   if not GoVisOut then
      exit;

   VisDataThread.InformVisDriver(RestoreWindow, 0);
end;

function TBASSPlayer.GetMessageHandle : HWND;
begin
   result := MessageHandle;
end;

// Inform current player status to the driver of vis plug-in.
procedure TBASSPlayer.InformPlayerStatus(StatusIndex : integer);
var
   p1 : PDWORD;
   p2 : PBYTE;
//   ModeId : DWORD;
   TitleBuf : array[0..MaxTitleLen] of char;

// The DataBuffer defined in PluginCtrl.pas is used as follows
//    byte offset       usage
//      0 -    3     Player Mode
//      4 -  275     Stream Information
//    276 -  279     SyncWindows flag / EMBED window Handle
//    280 -  283     Flag to notify that new FFT data is given
//    284 -  287     Sample rate
//    288 -  291     number of channels
//    292 -  295     playback position;
//    296 - 2599     FFT Data
//   2600 - 2855     Stream file path

begin
   if StatusIndex = InformStreamInfo then
      SetStreamInfo2(FStreamInfo);

   if not GoVisOut then
      exit;

   p1 := ShareMemPointer;

   case StatusIndex of
     // following sentences for "stPlayerMode" are useless (executed in procedure "SetPlayerMode")
     { stPlayerMode : begin
           ModeId := ord(FPlayerMode);
           p1^ := ModeId;
           VisDataThread.InformVisDriver(InformPlayerMode, 4);
         end; }

      InformStreamInfo : begin
           inc(p1, 1);
           p1^ := FStreamInfo.SampleRate;
           inc(p1, 1);
           p1^ := FStreamInfo.BitRate;
           inc(p1, 1);
           if FMixerPlugged then
              p1^ := 2
           else
              p1^ := FStreamInfo.Channels;
           inc(p1, 1);
           p1^ := FStreamInfo.Duration;
           inc(p1, 1);
           FillChar(TitleBuf, MaxTitleLen+1, 0);
           StrPLCopy(TitleBuf, FStreamInfo.Title, MaxTitleLen);
           p2 := pointer(p1);
           move(TitleBuf, p2^, MaxTitleLen+1);
           p2 := ShareMemPointer;
           inc(p2, 2600);   // skip 2600 byte
           FillChar(TitleBuf, MaxTitleLen+1, 0);
           StrPLCopy(TitleBuf, FStreamInfo.FileName, MaxTitleLen);
           move(TitleBuf, p2^, MaxTitleLen+1);

           VisDataThread.InformVisDriver(InformStreamInfo, 528);
         end;
     { stSyncWindows : begin
           inc(p1, 69);
           if FSyncVisWindow then
              p1^ := 1
           else
              p1^ := 0;
           VisDataThread.InformVisDriver(InformSyncWindows, 4);
         end; }
     { stEMBEDHandle : begin
           inc(p1, 69);
           p1^ := FEMBEDHandle;
           VisDataThread.InformVisDriver(InformEMBEDHandle, 4);
         end; }
     { stUseVisDrawer : begin
           inc(p1, 69);
           p1^ := ord(FUseVisDrawer);
           VisDataThread.InformVisDriver(InformUseVisDrawer, 4);
         end; }
   end;

end;


function TBASSPlayer.BASSAddonLoad(FilePath : string) : TBASSAddOnInfo;
var
   i : integer;
   FoundEmpty, FoundPreloaded : boolean;
   FileName : string;
   AddonHandle : HPLUGIN;
   AddonInfoP : PBASS_PLUGININFO;
begin
   result.Handle := 0;

   if not BASSDLLLoaded then   
      exit;

   if not FileExists(FilePath) then
      exit;

   FoundPreloaded := false;
   FileName := Lowercase(ExtractFileName(FilePath));
   for i := 1 to MaxLoadableAddons do
      if BASSAddonList[i].Handle <> 0 then
         if BASSAddonList[i].Name = FileName then
         begin
            FoundPreloaded := true;
            break;
         end;

   if FoundPreloaded then
      exit;

 // Check if BASSAddonList has a empty element for an add-on to be loaded.
   FoundEmpty := false;
   for i := 1 to MaxLoadableAddons do
      if BASSAddonList[i].Handle = 0 then
      begin
         FoundEmpty := true;
         break;
      end;

   if not FoundEmpty then
      exit;

  {$IFDEF DELPHI_2007_BELOW}
   AddonHandle := BASS_PluginLoad(pChar(FilePath), 0);
  {$ELSE}
   AddonHandle := BASS_PluginLoad(pChar(FilePath), BASS_UNICODE);
  {$ENDIF}
   if AddonHandle <> 0 then
   begin
      BASSAddonList[i].Handle := AddonHandle;
      BASSAddonList[i].Name := FileName;
      AddonInfoP := BASS_PluginGetInfo(AddonHandle);
      if AddonInfoP <> nil then
      begin
         BASSAddonList[i].Version := AddonInfoP^.Version;
         BASSAddonList[i].NumFormat := AddonInfoP^.formatc;
         BASSAddonList[i].FormatP := AddonInfoP^.formats;
      end;

      if FileName = 'bass_aac.dll' then
         FBASSAACReady := true;
      result := BASSAddonList[i];
   end;

end;

function TBASSPlayer.BASSAddonFree(AddonHandle : HPLUGIN) : integer;
var
   i : integer;
   Matched : boolean;
begin
   result := 0;

   if not BASSDLLLoaded then
      exit;

  // AddonHandle : 0 =>  Release all add-ons

   for i := 1 to MaxLoadableAddons do
   begin
      Matched := false;
      if BASSAddonList[i].Handle <> 0 then
         if (AddonHandle = 0) or (BASSAddonList[i].Handle = AddonHandle) then
             Matched := true;

      if not Matched then
         continue;

    {  if BASSAddonList[i].Name = 'bass_aac.dll' then
         if FUsing_BASS_AAC then
            break;   // cannot unload  }

      if BASS_PluginFree(BASSAddonList[i].Handle) then
      begin
         BASSAddonList[i].Handle := 0;
         if BASSAddonList[i].Name = 'bass_aac.dll' then
            FBASSAACReady := false;
         inc(result);
      end;

      if AddonHandle <> 0 then   // if not unloading all add-ons
         break;
   end;

end;

function TBASSPlayer.GetBASSAddonList : TBASSAddonList;
begin
   result := BASSAddonList;
end;

function TBASSPlayer.GetBASSAddonExts : string;
var
   i, j : integer;
begin
   result := '';

   for i := 1 to MaxLoadableAddons do
      if BASSAddonList[i].Handle <> 0 then
         for j := 1 to BASSAddonList[i].NumFormat do
            {$IFDEF DELPHI_2007_BELOW}
             result := result + LowerCase(BASSAddonList[i].FormatP[j-1].exts) + ';';
            {$ELSE}
             result := result + AnsiStrings.LowerCase(BASSAddonList[i].FormatP[j-1].exts) + ';';
            {$ENDIF}
end;

function TBASSPlayer.GetDecoderName(ExtCode : string) : string;
var
   s : string;
   i, j : integer;
begin
   result := '';

   for i := 1 to MaxLoadableAddons do
      if BASSAddonList[i].Handle <> 0 then
      begin
         s := '';
         for j := 1 to BASSAddonList[i].NumFormat do
           {$IFDEF DELPHI_2007_BELOW}
            s := s + LowerCase(BASSAddonList[i].FormatP[j-1].exts);
           {$ELSE}
            s := s + AnsiStrings.LowerCase(BASSAddonList[i].FormatP[j-1].exts);
           {$ENDIF}

         if pos(ExtCode, s) > 0 then
         begin
            result := BASSAddonList[i].Name;
            break;
         end;
      end;
end;

function TBASSPlayer.MIDIFontInit(FontFilePath : string;
                            var MIDI_FONTINFO : TMIDI_FONTINFO) : boolean;
var
   aFontHandle : HSOUNDFONT;
   MIDIFont : BASS_MIDI_FONT;
   aMIDI_FONTINFO : BASS_MIDI_FONTINFO;
begin
   result := false;

   if not FBASSMIDIReady then
      exit;

   if FileExists(FontFilePath) then
    {$IFDEF DELPHI_2007_BELOW}
      aFontHandle := BASS_MIDI_FontInit(pChar(FontFilePath), 0)
    {$ELSE}
      aFontHandle := BASS_MIDI_FontInit(pChar(FontFilePath), BASS_UNICODE)
    {$ENDIF}
   else
      exit;

   if aFontHandle <> 0 then
   begin
      if BASS_MIDI_FontGetInfo(aFontHandle, aMIDI_FONTINFO) then
         with MIDI_FONTINFO do
         begin
            FontName := string(aMIDI_FONTINFO.name);
            Copyright := string(aMIDI_FONTINFO.copyright);
            Comment := string(aMIDI_FONTINFO.comment);
            Presets := aMIDI_FONTINFO.presets;
            SampleSize := aMIDI_FONTINFO.samsize;
            SampleLoaded := aMIDI_FONTINFO.samload;
         end;

      if defaultFontHandle <> 0 then
         BASS_MIDI_FontFree(defaultFontHandle);   // free previously initialized default font

      defaultFontHandle := 0;
      FMIDISoundReady := false;

      MIDIFont.font := aFontHandle;
      MIDIFont.preset := -1; // use all presets
      MIDIFont.bank := 0;    // use default bank(s)

    // set default soundfont configuration
      if BASS_MIDI_StreamSetFonts(0, MIDIFont, 1) then
      begin
         defaultFontHandle := aFontHandle;
         defaultMIDI_FONTINFO := MIDI_FONTINFO;
         FMIDISoundReady := true;
         result := true;
      end;

   end;
end;

{ function TBASSPlayer.MIDIFontGetInfo : TMIDI_FONTINFO;
begin
   if MIDIFontHandle = 0 then
   begin
      result.FontName := '';
      result.SampleSize := 0;
      result.SampleLoaded := 0;
   end else
      result := MIDI_FONTINFO;
end; }

function TBASSPlayer.MIDIGetTrackInfo(MIDIFilePath : string;
                                      var MIDITrackInfo : TMIDITrackInfo) : boolean;
var
   pTrackText : pAnsiChar;
   tmpChannel : DWORD;
   NameHeader1 : string[7];
   NameHeader2 : string[6];
   Tracks : integer;
   i : integer;

begin
   result := false;
   MIDITrackInfo.TrackCount := 0;

   if not FBASSMIDIReady then
      exit;

   Tracks := 0;

   if MIDIFilePath = StreamInfo.FileName then
   begin
      if ChannelType <> Channel_MIDI then
         exit;

      tmpChannel := DecodeChannel;
   end else
   begin
      if pos(UpperCase(ExtractFileExt(FStreamName)), MIDIFileExt) = 0 then
         exit;

      NameHeader1 := copy(FStreamName, 1, 7);
      NameHeader2 := copy(FStreamName, 1, 6);
      if (NameHeader1 = 'http://') or (NameHeader2 = 'mms://') then
        {$IFDEF DELPHI_2007_BELOW}
         tmpChannel := BASS_MIDI_StreamCreateURL(pChar(FStreamName), 0,
                                                    BASS_STREAM_DECODE,
                                                    nil, nil, 44100)
        {$ELSE}
         tmpChannel := BASS_MIDI_StreamCreateURL(ToPMultiByte(pWideChar(FStreamName)), 0,
                                                    BASS_STREAM_DECODE,
                                                    nil, nil, 44100)
        {$ENDIF}
      else
        {$IFDEF DELPHI_2007_BELOW}
         tmpChannel := BASS_MIDI_StreamCreateFile(FALSE, pChar(FStreamName), 0, 0,
                                                  BASS_STREAM_DECODE, 44100);
        {$ELSE}
         tmpChannel := BASS_MIDI_StreamCreateFile(FALSE, ToPMultiByte(pWideChar(FStreamName)), 0, 0,
                                                  BASS_STREAM_DECODE or BASS_UNICODE, 44100);
        {$ENDIF}
   end;

   if tmpChannel <> 0 then
   begin
      for i := 0 to 255 do
      begin
         pTrackText := BASS_ChannelGetTags(tmpChannel, BASS_TAG_MIDI_TRACK + i);
         if pTrackText <> nil then
         begin
            inc(Tracks);
            MIDITrackInfo.TrackText[i] := string(pTrackText);
         end else
            break;
      end;

      if tmpChannel <> DecodeChannel then
         BASS_StreamFree(tmpChannel);
   end;

   if Tracks > 0 then
   begin
      MIDITrackInfo.TrackCount := Tracks;
      result := true;
   end;

end;


// ------------------------ Play list support ------------------------------------

procedure TBASSPlayer.ClearPlayList;  // * New at Ver 2.00
begin
   _ClearPlayList;
end;

procedure TBASSPlayer.AddToPlayList(FilePath, Title, Artist : string);  // * New at Ver 2.00
begin
   _AddToPlayList(FilePath, Title, Artist);
end;

procedure TBASSPlayer.DeleteFromPlayList(ListIndex : integer);   // * New at Ver 2.00
begin
   _DeleteFromPlayList(ListIndex);
end;

function TBASSPlayer.GetPlayListIndex : integer;           // * New at Ver 2.00
begin
   result := GetIndexOfPlayList('');
end;

procedure TBASSPlayer.SetPlayListIndex(Value : integer);  // * New at Ver 2.00
begin
   _SetPlayListIndex(Value);
end;

procedure TBASSPlayer.PlayListChangeTitle(FilePath : string; Title : string); // * New at Ver 2.00
begin
   ChangeTitle(FilePath, Title);
end;

function TBASSPlayer.NumOfPlayListItem : integer;        // * New at Ver 2.00
begin
   result := GetNumOfPlayListItem;
end;

function TBASSPlayer.PlayListTitle(ListIndex : integer) : string;  // * New at Ver 2.00
begin
   result := GetAPlayListTitle(ListIndex);
end;

function TBASSPlayer.IndexOfPlayList(FilePath : string) : integer;  // * New at Ver 2.00
begin
   result := GetIndexOfPlayList(FilePath);
end;


// ------------------------ Winamp General purpose plug-in support -----------------

function TBASSPlayer.RunGPPModule(GPP_Path : string) : integer;  // * New at Ver 2.00
begin
   result := -1;

   SetLoadingGPP(GPP_Path );
   if (FMBHandle <> 0) then   // to check MBDrawer.dll is operative
      if NumOfLoadedGPP < MaxGPPNo then
         result := Start_GPP(GPP_Path, DataReadyMsg, ParentHWND);
end;

function TBASSPlayer.RunAllGPPModules : integer;         // * New at Ver 2.00
begin
   result := 0;

   if (FMBHandle <> 0) then   // to check MBDrawer.dll is operative
      result := Start_AllGPPs(DataReadyMsg, ParentHWND);
end;

procedure TBASSPlayer.QuitGPPModule(GPPRegNo : integer);  // * New at Ver 2.00
begin
   Quit_GPP(GPPRegNo);
end;

function TBASSPlayer.NumberLoadedGPP : integer;               // * New at Ver 2.00
begin
   result := NumOfLoadedGPP;
end;

function TBASSPlayer.GetLoadedGPPInfo(index : integer; indexByEntryNo : boolean) : TGPPInfo; // * New at Ver 2.00
var
   GPPInfo : TGPPInfo;
begin
   GPPInfo.Handle := 0;
   if GetGPPInfo(index, indexByEntryNo, GPPInfo) then
      result := GPPInfo;
end;

procedure TBASSPlayer.ShowMBForm(Loc_X, Loc_Y, Width, Height : integer);   // * New at Ver 2.00.3
begin
   ShowMiniBrowser(Loc_X, Loc_Y, Width, Height, true);
end;

procedure TBASSPlayer.LetMBNavigate(URL : pAnsiChar);         // * New at Ver 2.00.2
begin
   MBNavigate(URL);
end;

procedure TBASSPlayer.LetMBNavigate2(Artist, Title : string; UseCixURL : boolean); // * New at Ver 2.01
var
   buf1 : array[0..255] of ansiChar;
   buf2 : array[0..255] of ansiChar;
  {$IFNDEF DELPHI_2007_BELOW}
   p1 : pAnsiChar;
  {$ENDIF}
begin
 {$IFDEF DELPHI_2007_BELOW}
   if Artist <> '' then
      StrPLCopy(buf1, Artist, 255)
   else
      buf1[0] := AnsiChar(0);
   if Title <> '' then
      StrPLCopy(buf2, Title, 255)
   else
      buf2[0] := AnsiChar(0);
 {$ELSE}
   if Artist <> '' then
   begin
     p1 := ToPMultiByte(pWideChar(Artist));
     StrLCopy(buf1, p1, 256);
   end else
     buf1[0] := AnsiChar(0);
   if Title <> '' then
   begin
     p1 := ToPMultiByte(pWideChar(Title));
     StrLCopy(buf2, p1, 256);
   end else
     buf2[0] := AnsiChar(0);
 {$ENDIF}
   MBNavigate2(buf1, buf2, UseCixURL);
end;

function TBASSPlayer.MagneticWindowAdd(WndHandle, HWndParent : HWND; var SubClass_Proc : TSubClass_Proc): Boolean;  // * New at Ver 2.00.3
var
   p : pointer;
begin
   result := false;

   if FMiniBrowserReady then
      if AddMagneticWindow(WndHandle, HWndParent, p) then
         result := true
      else
         result := false;

   if result then
      SubClass_Proc := p   // valid address if result = true, nil if result = false
   else
      SubClass_Proc := nil;
end;

function TBASSPlayer.MagneticWindowRemove(Handle: HWND): Boolean;  // * New at Ver 2.00.3
begin
   if FMiniBrowserReady then
      result := RemoveMagneticWindow(Handle)
   else
      result := false;
end;

function TBASSPlayer.GPPDrawerInfo(FormNo : integer) : TGenDrawerInfo;    // * New at Ver 2.00.3
begin
   result := GetGenDrawerInfo(FormNo);
end;

function TBASSPlayer.GetLyrics(Artist, Title : string; CheckTitleMatch : boolean;
                var pLyrics : pchar; var lLyrics : integer) : boolean;    // * New at Ver 2.00.3
begin
   result := GetLyricsText(pChar(Artist), pChar(Title), CheckTitleMatch, pLyrics, lLyrics);
end;

// ----------------------------------------------------------------------------------------

procedure TBASSPlayer.SetDownMixToStereo(Value : boolean);   // * New at Ver 2.00
begin
   if not FMixerReady then
      exit;

   FDownMixToStereo := Value;

end;

function TBASSPlayer.GetBuildDate : string;       // * New at Ver 2.00
begin
   Result := BuildDate;
end;

// ------------------------------ end of TBassPlayer ------------------------------


procedure Register;
begin
   RegisterComponents('Samples', [TBASSPlayer]);
end;

end.

