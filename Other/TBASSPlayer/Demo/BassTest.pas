// Unit BASSTest
//   Demo program to test TBASSPlayer commponent
//
//    written by Silhwan Hyun  (hyunsh@hanafos.com)    
//
//
// (vis window = the window used for visualization created by vis plug-in)
//
// Note : BASS audio library (bass.dll, bass_aac.dll, basswma.dll, bassmidi.dll, basscd.dll) should
//        be copied in the directory where the executable(compiled program, BASSTest.exe) resides in,
//        and BASS add-ons, Winamp plug-ins, MIDI soundfont Chorium.SF2 in the sub directory
//        "Plugins" under the directory where the executable resides in.


// Ver 2.1                     14 May 2009
//  - Modified for Delphi 2009.
//  - Fixed some minor bugs.
//
// Ver 2.01                     6 Mar 2009
//  - Changed form appearance with added procedures for skin. The form's apperence can
//     be customized with user supplied skin image.
//  - Added a sound effect control : Rotate
//  - Added menus & related sub forms : "Open URL", "View Status"
//  - Added a menu & a related sub form : "About BASSPlay"  (2.01.03)
//  - Changed procedures to automatic save / load playlist at closing / starting
//      program (2.01.04)
//  - Updated Knob component on form, which shows antialsed boundary. (2.01.05)
//  - Adapted new MBDrawer.dll, and the lyrics display window by new MBDrawer.dll shows
//     same style to Playlist window. (2.01.05)
//
//
// Ver 2.00                      2 Oct 2008
//  - Added and Modified serveral procedures to support Play List.
//  - Added 3 check boxes : "PL"(Show/Hide Play List), "Repeat" to support Play List.
//                          "Downmatrix to Stereo" to support downmixing multi channel sound to stereo.
//  - Added procedure ApplyLocalLanguage to support localizaton of program
//  - Modified procedure FormShow,
//     a. loads bass_aac.dll by default, to support AAC+ stream from net.
//     b. loads a Winamp GPP for lyrics(gen_kmpcix.dll) by default, to support automatic display of
//        lyrics according to the opened stream.
//  - Restricted the length of title to be displayed for currently opened stream.
//  - Disabled the use of check box "Single Channel Mode" which is used for changing operation mode
//     of TBASSPlayer. ( Now single channel mode is used only for debugging purpose. )
//  - Modified procedures to test driving multiple Winamp GPPs simultaneously.
//  - Modified the procedures related to Spectrum Image Display.
//     ( Replaced the TPaintbox component for Spectrum Image Display with TImage )
//  - Modified procedure ForceRefreshLyrics to Update lyrics display while playing(receiving)
//     net radio.
//  - Modified procedure GenWindowShow
//  - Added message handlers for Magnetic effect of MainForm
//
//
// Ver 1.92                     16 Jul 2008
//  - Added support for the Winamp vis plug-ins which uses Wasabi API.
//  - Set the ThumbVisible property of the PosSlider(= The slider to adjust/show playing position)
//     to false if the opend stream comes from net radio station.
//  - Fixed erroneous display of "Peak Mark" in Spectrum display window at restarting playback a
//     stream.
//  - Set to load in_midi.dll at program start to enable you to view information on MIDI file.
//  - Add function to process the request from Winamp plug-in.
//    (e.g. : If you use Milkdrop 2.0, you can control volume, playback position.. etc with a specified
//            control key when the visualization window of Milkdrop 2.0 has focus.)
//  - Added a check box which enables you to select the option for transition of vis window.
//
//
// Ver 1.91                     10 Dec 2006
//  - Modified some functions/procedures to support "BASSMIDI" (See bassmidi.chm).
//  - Modified some functions/procedures/Form to support Winamp 5 Vis plug-in.
//    ( Form change : Added "Vis Window" page in the Form.
//      So, the visual display by Winamp 5 Vis plug-in can be merged into the form of Demo program. )
//
// Ver 1.9                      26 Oct 2006
//  - Modified some functions/procedures to support BASS add-ons.
//
// Ver 1.81                     9 Apr 2005
//  - Changed the function of "File Info" button to enable selecting the stream file
//     you want to view file information or to edit its TAG.
//
// Ver 1.8                      26 Feb 2005
//  - Added/Modified some functions according to updated TBASSPlayer.
//  - Bug fixed the access violation error at closing program.
//
// Ver 1.7                       31 Jan 2005
//  - Modified some functions to display the numer of channels(previously
//    displayed "Stereo") for multi channel stream files.
//  - Added procedure "ModeChanged" which uses the TBASSPlayer's "OnModeChange"
//    event property.
//  - Modified some functions to prevent problems at log off or shut down system.
//
// Ver 1.6                        14 Dec 2004
//  - Modified Equalizer's gain control to affect the adjusted band only.
//
// Ver 1.51                       25 Jul 2004
//  - Added a button and codes to test "InfoBox" function of Winamp input plug-in.
//
// Ver 1.5                         15 Dec 2003
//  - Added components and codes to test the functions/properties for playing a stream
//     file from internet.
//  - Added 'DSP plug-in Control' button to set up Winamp DSP plug-in
//  - Changed output volume range : 0 ~ 100 -> 0 ~ 255
//
// Ver 1.4                         12 May 2003
//  - Added message handling to set up synchronized action between the windows
//    of BassPlay and the one of Winamp 2 visualization plug-in
//
// Ver 1.3                         5 Apr 2003
//  - Added a button to invoke Winamp visualization plug-in control form, which
//    enables you to select, start and stop visualization plug-ins.
//  - Added two sliders to adjust the depth of echo & reverb effects
//
// Ver 1.2                         5 Feb 2003
//  - Added a button to invoke Winamp input plug-in configuration form to play
//    stream files which BASS audio library cannot decode.
//  - Compiled with Delphi 4
//
// Ver 1.1                         8 Dec 2002
//   - Compiled on Delphi 4 for old version Delphi users
//   - Modified procedure DisplayFFTBand for better result
//
// Ver 1.0                         4 Aug 2002
//   - Initial release
//   - Compiled with Delphi 6       InputBox

unit BassTest;

interface

{$INCLUDE Delphi_Ver.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, BassPlayer, PluginCtrl, Knob, slider, ExtCtrls,
  ComCtrls, Buttons, UniCodeUtils, Menus,
  {$IFNDEF DELPHI_2007_BELOW}AnsiStrings,{$ENDIF}M7SegDis;

type

  // Redefine TImage to add OnMouseEnter & OnMouseLeave
  // Adapted from the example code of Sungho Jang, KOREA
  TImage = class(ExtCtrls.TImage)
  private
    FOnMouseEnter : TNotifyEvent;
    FOnMouseLeave : TNotifyEvent;

  private
    procedure SetMouseEnter(evt:TNotifyEvent);
    procedure SetMouseLeave(evt:TNotifyEvent);

  protected
    procedure WndProc(var Message:TMessage); override;

  public
    constructor Create(AOwner: TComponent); override;

  public
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write SetMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write SetMouseLeave;

  end;

  TMainForm = class(TForm)
    OpenDialog1: TOpenDialog;
    Timer_Stat: TTimer;
    picTopL: TImage;
    picTopF: TImage;
    picTopText: TImage;
    picTopM: TImage;
    picTopStretchL: TImage;
    picTopC: TImage;
    picTopStretchR: TImage;
    picBorderL: TImage;
    picBorderR: TImage;
    picBottomL: TImage;
    picBottomStretchL: TImage;
    picBottomR: TImage;
    picClose: TImage;
    picMenu: TImage;
    MainMenu: TPopupMenu;
    menuAddonSetup: TMenuItem;
    menuPluginSetup: TMenuItem;
    menuDSPCtrl: TMenuItem;
    menuVisCtrl: TMenuItem;
    menuClose: TMenuItem;
    N2: TMenuItem;
    menuOpen: TMenuItem;
    menuOpenURL: TMenuItem;
    N1: TMenuItem;
    menuExpandView: TMenuItem;
    picBottomExpand: TImage;
    picBottomStretchR: TImage;
    Panel4: TPanel;
    gbSoundEffects: TGroupBox;
    cbFlanger: TCheckBox;
    cbEcho: TCheckBox;
    cbReverb: TCheckBox;
    EchoSlider: TSlider;
    ReverbSlider: TSlider;
    FlangerSlider: TSlider;
    cbRotate: TCheckBox;
    Panel3: TPanel;
    Label2: TLabel;
    Slider2: TSlider;
    Slider3: TSlider;
    Slider4: TSlider;
    Slider7: TSlider;
    Slider8: TSlider;
    Slider9: TSlider;
    Slider1: TSlider;
    Slider10: TSlider;
    Slider5: TSlider;
    Slider6: TSlider;
    picEQBtn: TImage;
    picResetBtn: TImage;
    menuViewStatus: TMenuItem;
    lbl_Writer: TLabel;
    menuHideWindow: TMenuItem;
    N3: TMenuItem;
    menuAbout: TMenuItem;
    MainPanel: TPanel;
    PanelImage: TImage;
    Panel2: TPanel;
    SpectrumImage: TImage;
    lbl_Track: TLabel;
    lbl_Time: TLabel;
    lbl_Type: TLabel;
    edBitrate: TLabel;
    TrackMin10: M7SegDisp;
    TrackMin1: M7SegDisp;
    TrackSec10: M7SegDisp;
    TrackSec1: M7SegDisp;
    Shape_TU: TShape;
    Shape_TL: TShape;
    TimeMin10: M7SegDisp;
    TimeMin1: M7SegDisp;
    Shape_L: TShape;
    Shape_U: TShape;
    TimeSec10: M7SegDisp;
    TimeSec1: M7SegDisp;
    TitleImage: TImage;
    picMuteBtn: TImage;
    picLyricsBtn: TImage;
    picPlayListBtn: TImage;
    picRepeatBtn: TImage;
    picEjectBtn: TImage;
    picNextBtn: TImage;
    picStopBtn: TImage;
    picPauseBtn: TImage;
    picPlayBtn: TImage;
    picPrevBtn: TImage;
    picRulerR: TImage;
    picRulerL: TImage;
    picThumb: TImage;
    VolumeKnob: TKnob;

    procedure FormShow(Sender: TObject);
    function  OpenStream(StreamName : string; StartPlay : boolean) : boolean;
    procedure btnOpenClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnPlayClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure cbFlangerClick(Sender: TObject);
    procedure cbEchoClick(Sender: TObject);
    procedure cbReverbClick(Sender: TObject);
    procedure VolumeKnobChange(Sender: TObject);
    procedure EQSliderChange(Sender: TObject);
    procedure cbEqualizerClick(Sender: TObject);
    procedure PosSliderStartTracking(Sender: TObject);
  //  procedure PosSliderStopTracking(Sender: TObject);
    procedure Timer_StatTimer(Sender: TObject);
    procedure CreateBasicImage;
    procedure ShowBackground;
    procedure PluginStartPlay(Sender: TObject; ChannelInfo : PChannelInfo);
    procedure PluginChannelInfo(Sender: TObject; ChannelInfo : PChannelInfo);
    procedure DisplayFFTBand(Sender: TObject; Bands : TBandOut);
    procedure GetMETAFromNet(Sender: TObject; Content : ansistring);
    procedure GetMIDILyric(Sender: TObject; TextP : pAnsiChar);
    procedure DownloadEnded(Sender: TObject; Amount : DWORD);
    procedure ModeChanged(Sender: TObject; OldMode, NewMode : TPlayerMode);
    procedure PlayEnded(Sender: TObject);  // * New at Ver 2.00
    procedure ProcessPluginRequest(Sender: TObject; GenParam : DWORD);
    procedure btnPluginSetupClick(Sender: TObject);
    procedure EchoSliderChange(Sender: TObject);
    procedure ReverbSliderChange(Sender: TObject);
    procedure btnShowVisCtrlFormClick(Sender: TObject);
    procedure btnShowDSPCtrlFormClick(Sender: TObject);
    procedure btnNetOpenClick(Sender: TObject);
    procedure btnMuteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAddonSetupClick(Sender: TObject);
    procedure GetVisPluginInfo(Sender: TObject; VisWindowAttr : TVisPluginInfo);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure btnPrevClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnResetEQClick(Sender: TObject);
    procedure cbShowPlayListClick(Sender: TObject);
    procedure cbDownMixClick(Sender: TObject);
    procedure cbShowMBClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure picCloseMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure picTopItemsMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure picMenuClick(Sender: TObject);
    procedure menuExpandViewClick(Sender: TObject);
    procedure menuOpenURLClick(Sender: TObject);
    procedure FlangerSliderChange(Sender: TObject);
    procedure cbRotateClick(Sender: TObject);
    procedure picButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure picButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure menuViewStatusClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure picThumbMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure picThumbMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure picThumbMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure picRulerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure menuHideWindowClick(Sender: TObject);
    procedure picRulerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure menuAboutClick(Sender: TObject);
    procedure PanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure picCloseClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure SetChannelInfo(SampleRate, Channels : DWORD);      // * New at Ver 2.00
    procedure UpdatePlayList(Sender: TObject; GenParam : DWORD); // * New at Ver 2.00
    procedure PlayNewSong(ListIndex : integer);                  // * New at Ver 2.00
    procedure MBWindowShow(Sender: TObject; SW_FLAG : DWORD);    // * New at Ver 2.00
    procedure GenWindowShow(Sender: TObject; GenInfoP : DWORD);  // * Changed at Ver 2.00.3
    procedure SetChannelType(ChannelType : DWORD);
    procedure SetTrackLenDisplay(TimeStr : string);
    procedure SetTimeDisplay(TimeStr : string);
    procedure ForceRefreshLyrics;
  //  procedure CheckCursor;
  public
    { Public declarations }
    procedure ApplyLocalLanguage;

  // Following procedures and a function are for implementing skin effect of form frame.
  //  * New at Ver 2.01
    procedure RelocateElement(Width_, Height_ : integer);
    procedure ResizeElement(Sender: TObject);
    procedure SetImage(Destination: TImage;
                                x, y, W, H: integer;
                                Source: TImage;
                                startX, StartY: integer);
    procedure UpperImageCopy;
    procedure UpperImageLoad;
    procedure ExpandImageLoad(b_Highlighted : boolean);
    procedure BtnImageLoad(Btn_No : integer; b_Pressed, b_Highlighted : boolean);
    procedure ImageCopy;
    function  WriteTitle(title : string; GetWidthOnly : boolean) : integer;
    procedure LoadSkin;

  // Following definitions of message handler are for implementing Magnetic effect.
    procedure WMEnterSizeMove(var Msg: TMessage); message WM_ENTERSIZEMOVE;
    procedure WMSizing(var Msg: TMessage); message WM_SIZING;
    procedure WMMoving(var Msg: TMessage); message WM_MOVING;
    procedure WMExitSizeMove(var Msg: TMessage); message WM_EXITSIZEMOVE;
    procedure WMSysCommand(var Msg: TMessage); message WM_SYSCOMMAND;
    procedure WMCommand(var Msg: TMessage); message WM_COMMAND;

    procedure WndProc(var Msg : TMessage); override;
    procedure ImageMouseEnter(Sender: TObject);
    procedure ImageMouseLeave(Sender: TObject);
    procedure SetImageMouseEvent(img: TImage);

    procedure SetBitrateText(BitRate : Dword);
    procedure SetTitleText(PrefixStr : string);
    procedure TitleTextOut(FlowStep : integer);
  end;

 // Delphi 4 incorrectly defined function "AlphaBlend" in Windows.pas unit
 // So, I use re-defined function to avoid runtime error.
  function MyAlphaBlend(DC: HDC; p2, p3, p4, p5: Integer;
                        DC6: HDC; p7, p8, p9, p10: Integer;
                        p11: TBlendFunction): BOOL; stdcall;
                        external 'msimg32.dll' name 'AlphaBlend';

var
  MainForm: TMainForm;
  BassPlayer1 : TBassPlayer;


implementation

uses PlayListConfig, VisPluginCtrl, DSPPluginCtrl, AddonConfig, StatusView, wa_ipc, WasabiAPI,
     Util_LeoLyrics, InputURL, About;


{$R *.dfm}

const
   MajorVer = '2';
   MinorVer = '10';
   RevVer = '00';
   BuildDateStr = '14 May 2009';

// Constants for frequency spectrum visualizzer
   BlockWidth = 3;
   HBlockGap = 1;
   HBlockCount = NumFFTBands;
   VLimit = 22;    // Intensity range : 0 ~ 22
   BackColor = clBlack;

   TitleLimit = 42;  // max. length of title

   m_nSnapOffset = 10;

 // These message is not used because BASSPlay is changed to set to operate only in
 // "Dual Channel Mode" since Ver 2.00
  { WarningMsg : array[0..1023] of char
            = 'The DSP by Winamp DSP plug-ins can be applied only if the ' + chr(10)
            + 'stream being played is decoded by a Winamp input plug-in' + chr(10)
            + 'while TBASSPlayer is set to operate in "Single Channel Mode".' + chr(10) + chr(10)
            + '(note)' + chr(10)
            + '"Single channel mode" means one channel is used to process' + chr(10)
            + 'a stream, i.e., decoding & output processes are done within' + chr(10)
            + 'one BASS channel.' + chr(10)
            + '"Dual channel mode" means two BASS channels are used to' + chr(10)
            + 'process a stream, one for decoding and the other for output.' + chr(10)
            + '"Dual channel mode" is default operation mode and used to' + chr(10)
            + 'support Winamp DSP plug-ins.' + chr(10)
            + 'So you do not need load Winamp input plug-in to activate' + chr(10)
            + 'Winamp DSP plug-in while TBASSPlayer is set to operate' + chr(10)
            + 'in "Dual channel mode".' + chr(10)
            + 'But "Single channel mode" is more reliable (there is no' + chr(10)
            + 'intermediate operation between channels) and less resource' + chr(10)
            + 'consuming than "Dual channel mode."' + chr(0);  }

   BASS_CTYPE_STREAM_OGG   = $10002;
   BASS_CTYPE_STREAM_MP1   = $10003;
   BASS_CTYPE_STREAM_MP2   = $10004;
   BASS_CTYPE_STREAM_MP3   = $10005;
   BASS_CTYPE_STREAM_AIFF  = $10006;
   BASS_CTYPE_STREAM_WAV   = $40000; // WAVE flag, LOWORD=codec
   BASS_CTYPE_STREAM_WAV_PCM = $50001;
   BASS_CTYPE_STREAM_WAV_FLOAT = $50003;
   BASS_CTYPE_MUSIC_MOD    = $20000;
   BASS_CTYPE_MUSIC_MTM    = $20001;
   BASS_CTYPE_MUSIC_S3M    = $20002;
   BASS_CTYPE_MUSIC_XM     = $20003;
   BASS_CTYPE_MUSIC_IT     = $20004;
   BASS_CTYPE_MUSIC_MO3    = $00100; // MO3 flag

   BASS_CTYPE_STREAM_MPC   = $10a00;    // Musepack
   BASS_CTYPE_STREAM_AAC   = $10b00;    // AAC
   BASS_CTYPE_STREAM_MP4   = $10b01;    // MP4
   BASS_CTYPE_STREAM_SPX   = $10c00;    // Speex
   BASS_CTYPE_STREAM_MIDI  = $10d00;
   BASS_CTYPE_STREAM_ALAC  = $10e00;    // Apple Lossless
   BASS_CTYPE_STREAM_TTA   = $10f00;

   BASS_CTYPE_STREAM_CD      = $10200;
   BASS_CTYPE_STREAM_WMA     = $10300;
   BASS_CTYPE_STREAM_WMA_MP3 = $10301;
   BASS_CTYPE_STREAM_OFR     = $10600;
   BASS_CTYPE_STREAM_APE     = $10700;
   BASS_CTYPE_STREAM_AC3     = $11000;
   BASS_CTYPE_STREAM_ADX     = $1F000;

   msgCount = 41;

   URL_CixHome : pAnsiChar = 'http://www.cix.co.kr/kr/lyrics_plugin_index.html';
   URL_LyricsHome : pAnsiChar = 'http://www.lyricsplugin.com/winamp03/plugin/?title=';

   L_Margin = 11;   // Margin Left side for frame
   R_Margin = 8;    // Margin Right side for frame
   T_Margin = 20;   // Margin Top side for frame
   B_Margin = 14;   // Margin Bottom side for frame
   LR_Margin = L_Margin + R_Margin;
   TB_Margin = T_Margin + B_Margin;

   BasicHeight = 151;
   ExpandedHeight = 370;

   PosSlider_MaxValue = 200;

   TitleForStandby = 'Demo program for TBASSPlayer V2.10';
   
var
   MagneticWndProc : TSubClass_Proc;

   SPS : LongInt;    // sample rate
   NowTracking : boolean;

   BasicBMP : TBitMap;
   DisplayBar: TBitmap;
   GaugeRect : TRect;

   EQGains : TEQGains;
   PeakValue : array[1..NumFFTBands] of single;
   PassedCounter : array[1..NumFFTBands] of integer;
   IsNetRadio : boolean;
   IsNetStream : boolean;
   CurMode : TPlayerMode;
   NowLogOff : boolean = false;
   ProgDir : string;
   Saved8087CW: Word;

   CurVisWindowAttr : TVisPluginInfo;

   MBWinHandle : HWND = 0;
   GenWinHandle : array[0..MaxLoadableGPPs-1] of HWND;
   CycleNo : integer = 0;
   StartedInType : TVisWindowIs = UnAvailable;


   OpenByButton : boolean = false;
   OpenByNetButton : boolean = false;
   ChangingPlayList : boolean = false;
   IsOpeningStream : boolean = false;

   TimerCounter : integer = 0;
   HiIntensityCounter : integer;
   PlaySec : real = 0.0;
   PlayLength : DWORD = 0;

   GPPRegNo : integer = -1;
   LeosHandleList : TLeosHandleList;

   NowClosing : boolean = false;
   ShownMB : boolean = false;
   ShownLeosLyrics : boolean = false;
   UseCixURL : boolean = false;   // true : for Korean and English lyrics,
                                  // false : for English only lyrics

   bCheckStat : boolean = false;
   m_bFocused : boolean = false;
   b_BtnPressed : boolean = false;
   b_EqualizerOn : boolean = false;
   b_Repeat : boolean = false;
   b_PlayList : boolean = false;
   b_ViewLyrics : boolean = false;
   b_Mute : boolean = false;
   b_MuteHighlighted : boolean = false;
   i_CursorOn : integer = 0;
   picW           : integer;
   picMap  : TImage;
   picMap2 : TImage;
   picMap3 : TImage;
   PicMapDC  : hdc;
   PicMapDC2 : hdc;
   PicMapDC3 : hdc;

   TmpBmp : TBitMap;
   TitleBackBmp : TBitMap;
   TitlePosX : integer = 0;
   lbl_Title : string;

   OrgCursor   : HCURSOR;
   CURSOR_HAND : HCURSOR;
   CURSOR_SHIFT : HCURSOR;
   ThumbStartX : integer;

   ExpandHighlighted : boolean = false;
   PassedActivation : boolean = false;

   msgs : array[1..msgCount] of string
          = ('BASS.DLL is not ready to operate',          // 1
             'Following Library files are not loaded',    // 2
             'Following Library file is not loaded',      // 3
             'Not a valid URL format',                    // 4
             'Cannot open file (BASSPlayer is not ready)',  // 5
             'Opened a file from internet',               // 6
             'Connected to net radio',                    // 7
             'Being decoded by Winamp plug-in',           // 8
             'Being decoded by',                          // 9
             'Opened a local file',                       // 10
             'Displaying...',                             // 11
             'MIDI sound ready !',                        // 12
             'MIDI soundfont "Chorium.SF2" loaded',       // 13
             'MIDI soundfont "Chorium.SF2" not loaded',   // 14
             'You can get it from,',                      // 15
             'Stream Format :',                           // 16
             'Standby',                                   // 17
             'Ready to play',                             // 18
             'Stopped',                                   // 19
             'Paused',                                    // 20
             'Playing',                                   // 21
             'BASS native files',                         // 22
             'All playable files',                        // 23
             'Winamp plug-in supported files',            // 24
             'BASS add-on supported files',               // 25
             'Confirm',                                   // 26
             'Warning',                                   // 27
             'Ended',                                     // 28
             'Winamp GPP for lyrics loaded',              // 29
             '"Title" field cannot be empty',             // 30

          // followings are for menu text
             '&Open local file',                          // 31
             'Open &URL',                                 // 32
             '&Bass Addon Setup',                         // 33
             '&Input Plugin Setup',                       // 34
             '&DSP Plugin Setup',                         // 35
             '&Vis Plugin Setup',                         // 36
             'View &Status',                              // 37
             '&Expanded View',                            // 38
             'E&xit',                                     // 39
             '&Hide Window',                              // 40
             '&About BASSPlay');                          // 41
              
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

function GetStreamTypeStr(StreamFormat : DWORD) : string;
begin
   case StreamFormat of
     BASS_CTYPE_STREAM_OGG  : result := 'Ogg Vorbis';
     BASS_CTYPE_STREAM_MP1  : result := 'MPEG Layer 1';
     BASS_CTYPE_STREAM_MP2  : result := 'MPEG Layer 2';
     BASS_CTYPE_STREAM_MP3  : result := 'MP3';
     BASS_CTYPE_STREAM_AIFF : result := 'Audio IFF';
     BASS_CTYPE_STREAM_WAV  : result := 'WAVE';
     BASS_CTYPE_STREAM_WAV_PCM   : result := 'Integer PCM WAVE';
     BASS_CTYPE_STREAM_WAV_FLOAT : result := 'Floating-point PCM WAVE';
     BASS_CTYPE_MUSIC_MOD   : result := 'Generic MOD';
     BASS_CTYPE_MUSIC_MTM   : result := 'MultiTracker';
     BASS_CTYPE_MUSIC_S3M   : result := 'ScreamTracker 3';
     BASS_CTYPE_MUSIC_XM    : result := 'FastTracker 2';
     BASS_CTYPE_MUSIC_IT    : result := 'Impulse Tracker';
     BASS_CTYPE_MUSIC_MO3   : result := 'MO3';

     BASS_CTYPE_STREAM_MPC  : result := 'Musepack';
     BASS_CTYPE_STREAM_AAC  : result := 'AAC';
     BASS_CTYPE_STREAM_MP4  : result := 'MP4';
     BASS_CTYPE_STREAM_SPX  : result := 'Speex';
     BASS_CTYPE_STREAM_MIDI : result := 'MIDI';
     BASS_CTYPE_STREAM_ALAC : result := 'Apple Lossless';
     BASS_CTYPE_STREAM_TTA  : result := 'TTA';

     BASS_CTYPE_STREAM_CD   : result := 'CD Track';
     BASS_CTYPE_STREAM_WMA  : result := 'WMA';
     BASS_CTYPE_STREAM_WMA_MP3 : result := 'WMA_MP3';
     BASS_CTYPE_STREAM_OFR  : result := 'OptimFROG';
     BASS_CTYPE_STREAM_APE  : result := 'Monkey''s Audio';
     BASS_CTYPE_STREAM_AC3  : result := 'Dolby Digital AC-3';
     BASS_CTYPE_STREAM_ADX  : result := 'ADX';

     else
      result := 'Unknown';
   end;
end;

function GetLimitedStr(Source : string; MaxLen : integer) : string;
var
   tmpStr : string;
   pos1 : integer;
begin
   if Length(Source) <= MaxLen then
      tmpStr := Source
   else begin
      pos1 := pos(' - ', Source);
      if pos1 > 0 then
         tmpStr := copy(Source, pos1 + 3, length(Source) - pos1 - 2)
      else
         tmpStr := Source;
      if Length(tmpStr) > MaxLen then
      repeat
         pos1 := pos(' ', tmpStr);
         if pos1 = 0 then
            break;
         tmpStr := copy(tmpStr, pos1 + 1, length(tmpStr) - pos1);
      until (Length(tmpStr) <= MaxLen);
   end;

   if Length(tmpStr) > MaxLen then
      result := copy(tmpStr, Length(tmpStr) - MaxLen + 1, MaxLen)
   else
      result := tmpStr;
end;


//--------------------------------------------------------------------------
{ TMyImage }
// Adapted from the example code of Sungho Jang, KOREA
constructor TImage.Create(AOwner: TComponent); 
begin 
  FOnMouseEnter:=nil;
  FOnMouseLeave:=nil;

  inherited;
end; 

procedure TImage.SetMouseEnter(evt: TNotifyEvent); 
begin 
  FOnMouseEnter:=evt; 
end;

procedure TImage.SetMouseLeave(evt: TNotifyEvent); 
begin
  FOnMouseLeave:=evt;
end; 

procedure TImage.WndProc(var Message: TMessage);
begin 
  if Message.Msg = CM_MOUSEENTER then 
    if Assigned(FOnMouseEnter) then
      FOnMouseEnter(self); 

  if Message.Msg = CM_MOUSELEAVE then
    if Assigned(FOnMouseLeave) then 
      FOnMouseLeave(self); 

  inherited; 

end;

//-------------------------------------------------------------------------

procedure GetAlphablendedImage(Image1, Image2 : TImage; Loc_X, Loc_Y : integer;
                                Transparency : DWORD);
const
   AC_SRC_ALPHA = $1;
var
   bf : _BLENDFUNCTION;
begin
   if Transparency > 255 then
      Transparency := 255;

   try
      bf.BlendOp := AC_SRC_OVER;               //
      bf.BlendFlags := 0;                      //
      bf.SourceConstantAlpha := Transparency;  // 0: full transparent  255: non-transparent
      bf.AlphaFormat := AC_SRC_ALPHA;          // use source alpha
      MyAlphablend(Image1.Canvas.Handle,       // destination
                   0, 0, Image1.Width, Image1.Height,
                   Image2.Canvas.Handle,       // source
                   Loc_X, Loc_Y, Image1.Width, Image1.Height, bf);
      Image1.Refresh;
   finally

   end;
end;

procedure TMainForm.ApplyLocalLanguage;
var
  F: TextFile;
  SearchRec: TSearchRec;
  S: string;
  FormName : string;
  MyFormEntry : boolean;
  CommonEntry : boolean;
  i, j, SepPos : integer;
  ItemType : string[1];
  ItemName, ItemVal : string;
  Temp: TComponent;

  Lang_Str : array of string;
  Entries_Lang_Str : integer;
  Items_Lang_Str : integer;
  S_org, S_local : string;

begin
  if FindFirst(ExtractFilePath(ParamStr(0)) + 'lang_*.txt', faAnyFile, SearchRec) <> 0 then
  begin
     FindClose(SearchRec);
     exit;
  end;   

  MyFormEntry := false;
  CommonEntry := false;
  Items_Lang_Str := 0;
  SetLength(Lang_Str, 16);
  Entries_Lang_Str := 16;

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
        FormName := copy(S, 2, length(S) - 2);
        if FormName = Self.Name then
        begin
           MyFormEntry := true;
           CommonEntry := false;
           continue;
        end else if uppercase(FormName) = 'COMMON' then
        begin
           MyFormEntry := false;
           CommonEntry := true;
           continue;
        end else if MyFormEntry then
           break
        else begin
           MyFormEntry := false;
           CommonEntry := false;
           continue;
        end
     end
     else if (not MyFormEntry) and (not CommonEntry) then
        continue;

     SepPos := pos('=', S);
     if SepPos = 0 then
        Continue;

     ItemVal := trim(copy(S, SepPos + 1, length(S) - SepPos));
     if ItemVal = '' then
        continue;
     if ItemVal[1] = '"' then
        ItemVal := copy(ItemVal, 2, length(ItemVal) - 1);
     if ItemVal[length(ItemVal)] = '"' then
        ItemVal := copy(ItemVal, 1, length(ItemVal) - 1);
     if ItemVal = '' then
        continue;

     if ((S[1] = '&') or (S[1] = '$') or (S[1] = '*')) then
     begin
        ItemType := S[1];
        ItemName := trim(copy(S, 2, SepPos - 2));

        for i := ComponentCount - 1 downto 0 do
        begin
           Temp := Components[i];
           if not (Temp is TControl) then
              continue;
           if Temp.Name = ItemName then
           begin
              if ItemType = '&' then
              begin
                 if (Temp is TLabel) then
                    (Temp as TLabel).Caption := ItemVal
                 else if (Temp is TButton) then
                    (Temp as TButton).Caption := ItemVal
                 else if (Temp is TSpeedButton) then
                    (Temp as TSpeedButton).Caption := ItemVal
                 else if (Temp is TCheckBox) then
                    (Temp as TCheckBox).Caption := ItemVal
                 else if (Temp is TTabSheet) then
                    (Temp as TTabSheet).Caption := ItemVal
                 else if (Temp is TGroupBox) then
                    (Temp as TGroupBox).Caption := ItemVal
              end else if ItemType = '$' then
              begin
                 if (Temp is TEdit) then
                    (Temp as TEdit).Text := ItemVal
                 else if (Temp is TComboBox) then
                    (Temp as TComboBox).Text := ItemVal
              end else if ItemType = '*' then
                 (Temp as TControl).Hint := ItemVal
           end;
        end;
     end else
     begin
     // Store message strings (format : Original=Local)
        Lang_Str[Items_Lang_Str] := trim(copy(S, 1, SepPos - 1)) + '=' + ItemVal;
        inc(Items_Lang_Str);
        if Items_Lang_Str = Entries_Lang_Str then
        begin
           Entries_Lang_Str := Entries_Lang_Str + 16;
           SetLength(Lang_Str, Entries_Lang_Str);
        end;
     end;

  end;

  CloseFile(F);
  SetLength(Lang_Str, Items_Lang_Str);

  // Substitute message string with local text.
  for i := 1 to Items_Lang_Str do
  begin
     SepPos := pos('=', Lang_Str[i - 1]);
     S_org := copy(Lang_Str[i - 1], 1, SepPos - 1);
     if S_org[1] = '%' then
        S_org := copy(S_org, 2, length(S_org) - 1);
     S_local := copy(Lang_Str[i - 1], SepPos + 1, length(Lang_Str[i - 1]) - SepPos);
     for j := 1 to msgCount do
        if msgs[j] = S_org then
        begin
           msgs[j] := S_local;
           break;
        end;
  end;

  SetLength(Lang_Str, 0);
end;


procedure TMainForm.WndProc(var Msg : TMessage);
var
   s : string;
   i : integer;
 //  p : pointer;
   GPPDrawerInfo : TGenDrawerInfo;

begin
   if NowLogOff then
   begin
      inherited WndProc(Msg);
      exit;
   end;
   if BassPlayer1 = nil then   // if before creation of BassPlayer1
   begin
      inherited WndProc(Msg);
      exit;
   end;

  // message WM_ACTIVATEAPP is received when BassPlay gets focus or loses focus
  // from/to other application's window.
 {  if (Msg.Msg = WM_ACTIVATEAPP) then
   begin
    // Msg.WParam = 0 : BassPlay losed focus
      if (Msg.WParam = 0) or (MBWinHandle = 0) or (GetForegroundWindow = MBWinHandle)
         or (not cbShowMB.Checked) then
      begin
         inherited WndProc(Msg);
         exit;
      end;

   // Put the Mini Browser and the window for Winamp GPP just below main window in Z-axis
      if MBWinHandle <> 0 then
         SetWindowPos(MBWinHandle, Self.Handle, 0, 0, 0, 0,
                                       SWP_NOACTIVATE + SWP_NOMOVE + SWP_NOSIZE);
      if GenWinHandle[0] <> 0 then
         SetWindowPos(GenWinHandle[0], Self.Handle, 0, 0, 0, 0,
                                       SWP_NOACTIVATE + SWP_NOMOVE + SWP_NOSIZE);

    //  Msg.result := 0;
      inherited WndProc(Msg);
   end else }

 // Show or hide vis window when BassPlay is showing or hiding
   if (Msg.Msg = WM_SHOWWINDOW) then
   begin
      if not NowClosing then
      begin
        if MBWinHandle <> 0 then
          if Msg.wParam <> 0 then      // Showing
          begin
            if b_ViewLyrics then
               ShowWindow(MBWinHandle, SW_SHOW)
          end else                    // Closing
            ShowWindow(MBWinHandle, SW_HIDE);

        for i := 1 to MaxLoadableGPPs do
          if GenWinHandle[i-1] <> 0 then
            if Msg.wParam <> 0 then    // Showing
            begin
              GPPDrawerInfo := BassPlayer1.GPPDrawerInfo(i-1);
              if GPPDrawerInfo.SW_FLAG <> SW_HIDE then   // if not closed by user
                 ShowWindow(GenWinHandle[i-1], SW_SHOW)
            end else                   // Closing
              ShowWindow(GenWinHandle[i-1], SW_HIDE);
      end;

    //  VisWindowHandle := BassPlayer1.GetVisWindowHandle;
    // VisWindowHandle = 0 : There is no vis window.
      if (CurVisWindowAttr.VisHandle = 0) then
      begin
         inherited WndProc(Msg);
         exit;
      end;

      if not NowClosing then
        if Msg.wParam = 0 then         // = BassPlay is hiding
           BASSPlayer1.HideVisWindow   // hide vis window
        else                           // = BassPlay is showing
           BASSPlayer1.ShowVisWindow;  // show vis window

    // Following is the method using direct system call
    {  if Msg.wParam <> 0 then
         ShowWindow(CurVisWindowAttr.VisHandle, SW_SHOW)
      else
         ShowWindow(CurVisWindowAttr.VisHandle, SW_HIDE); }

    //  Msg.result := 0;
      inherited WndProc(Msg);
   end;

   if (Msg.Msg = WM_QueryEndSession) then
   begin
   // Timer_Stat should be disabled soon after receiving WM_QueryEndSession message
   // to prevent problems at log off or shut down system.
      Timer_Stat.Enabled := false;
      NowLogOff := true;
      Msg.Result := 1;    // Allow system termination
   end else

   if Msg.Msg = WM_WA_IPC then
   begin
     Msg.result := 0;   // default value
     if Msg.LParam = IPC_GETWND then
     begin
        if Msg.WParam = IPC_GETWND_PE then
           Msg.result := PlayListConfigForm.Handle;

     end else if Msg.LParam = IPC_GET_API_SERVICE then
     begin
        Msg.Result := WaAPIServiceEntry;
     end;
   end else

   if Msg.Msg = WM_PlayListConfig then  // Message from PlayListConfigForm
   begin
      if Msg.wParam = PlayList_Close then   // PlayListConfigForm is closing
      begin
         if b_PlayList then
         begin
            b_PlayList := false;
            BtnImageLoad(picPlayListBtn.Tag, false, false);
         end;   
      end else
      if Msg.wParam = PlayList_Delete then
      begin
         s := string(pAnsiChar(Msg.lParam));
         if s = BassPlayer1.StreamPath then
            BassPlayer1.Close;
      end
      else if Msg.wParam = PlayList_DBClick then  // Double click an a song in PlayListForm
         PlayNewSong(Msg.lParam)
      else if Msg.wParam = Title_Changed then
      begin
         case BassPlayer1.Mode of
            plmPlaying : SetTitleText(msgs[21]);
            plmPaused  : SetTitleText(msgs[20]);
            plmStopped : SetTitleText(msgs[19]);
            plmReady   : SetTitleText(msgs[18]);
         end;
         ForceRefreshLyrics;
      end;
   end else
     inherited WndProc(Msg);

end;

procedure TMainForm.SetChannelInfo(SampleRate, Channels : DWORD);
var
   s1, s2 : string;
begin
   StatusViewForm.InfoMemo.Lines.Add(' ');
   if not BassPlayer1.DecodingByPlugin then
   begin
      if BassPlayer1.SingleChannel then
         StatusViewForm.InfoMemo.Lines.Add('(Single channel mode)');
      { else
          StatusViewForm.InfoMemo.Lines.Add(' (Operates in dual channel mode)')};
      StatusViewForm.InfoMemo.Lines.Add(msgs[16] + ' ' + GetStreamTypeStr(BassPlayer1.StreamInfo.Format));
   end;

   s1 := intToStr(SampleRate) + 'Hz';
   if Channels = 1 then
      S2 := 'Mono'
   else if Channels = 2 then
       S2 := 'Stereo'
   else if Channels > 2 then
       S2 := intToStr(Channels) + ' Channel';

   StatusViewForm.InfoMemo.Lines.Add(s1 + ', ' + s2);
end;

procedure TMainForm.FormShow(Sender: TObject);
var
   i : integer;
 //  BassAddonInfo : TBassAddonInfo;
   MIDI_FONTINFO : TMIDI_FONTINFO;
   tmpStr : string;

 procedure SetupBassPlayerHandlers;
 begin
   BassPlayer1.OnNewFFTData := DisplayFFTBand;
   BassPlayer1.OnPluginStartPlay := PluginStartPlay;
   BassPlayer1.OnGetChannelInfo := PluginChannelInfo;
   BassPlayer1.OnGetMeta := GetMETAFromNet;
   BassPlayer1.OnGetLyric := GetMIDILyric;
   BassPlayer1.OnDownloaded := DownloadEnded;
   BassPlayer1.OnModeChange := ModeChanged;
   BassPlayer1.OnVisWindowShow := GetVisPluginInfo;
   BassPlayer1.OnPluginRequest := ProcessPluginRequest;
   BassPlayer1.OnUpdatePlayList := UpdatePlayList;
   BassPlayer1.OnPlayEnd := PlayEnded;
   BassPlayer1.OnMBWindowShow := MBWindowShow;
   BassPlayer1.OnGenWindowShow := GenWindowShow;
 end;

 function UnloadedDLLMsg : string;
 begin
   result := '';

   if not BassPlayer1.BASSWMAReady then
      result := ' BASSWMA.DLL  (used to support WMA stream)'
   else
      StatusViewForm.InfoMemo.Lines.Add(' basswma.dll for WMA stream');

   if not BassPlayer1.BASSCDReady then
   begin
      if result = '' then
         result := ' BASSCD.DLL'
      else
         result := result + chr(10) + ' BASSCD.DLL';
      result := result + '  (used to support CD Audio track)'
   end else
      StatusViewForm.InfoMemo.Lines.Add(' basscd.dll for Audio CD');

   if not BassPlayer1.BASSMIDIReady then
   begin
      if result = '' then
         result := ' BASSMIDI.DLL'
       else
         result := result + chr(10) + ' BASSMIDI.DLL';
       result := result + '  (used to support MIDI file)';
   end else
      StatusViewForm.InfoMemo.Lines.Add(' bassmidi.dll for MIDI sound');

   if pos('*.mid;', BassPlayer1.PluginFileExts) = 0 then
   begin
      if result = '' then
         result := ' IN_MIDI.DLL'
       else
         result := result + chr(10) + ' IN_MIDI.DLL';
       result := result + '  (used to view info. of MIDI file)';
   end else
      StatusViewForm.InfoMemo.Lines.Add(' in_midi.dll for view info. of MIDI file');

   if not BassPlayer1.BASSAACReady then
   begin
      if result = '' then
         result := ' Plugins\BASS_AAC.DLL'
       else
         result := result + chr(10) + ' Plugins\BASS_AAC.DLL';
       result := result + '  (used to support AAC, AAC+ stream)';
    end else
       StatusViewForm.InfoMemo.Lines.Add(' bass_aac.dll for AAC stream');

  { if not BassPlayer1.MixerReady then
   begin
      if result = '' then
         result := ' BASSMIX.DLL'
       else
         result := result + chr(10) + ' BASSMIX.DLL';
       result := result + '  (used for Downmixing multi-channel data to stereo)';
   end else
      StatusViewForm.InfoMemo.Lines.Add(' bassmix.dll for mixing Audio'); }

   if not BassPlayer1.VisDrawerReady then
   begin
      if result = '' then
         result := ' VisDrawer.dll'
       else
         result := result + chr(10) + ' VisDrawer.dll';
       result := result + '  (used for Winamp-like visualization window)';
   end else
      StatusViewForm.InfoMemo.Lines.Add(' VisDrawer2.dll for Winamp-like visualization');

   if (BassPlayer1.MBWinHandle = 0) then
   begin
      if result = '' then
         result := ' MBDrawer.dll'
       else
         result := result + chr(10) + ' MBDrawer.dll';
       result := result + '  (used to support Winamp GPP)';
   end else
      StatusViewForm.InfoMemo.Lines.Add(' MBDrawer.dll for Lyrics display');
 end;

 procedure SetupMIDISoundFont;
 begin
   if FileExists(ProgDir + 'Plugins\Chorium.SF2') then
   begin
   // You can use any SF2 soundfont other than Chorium.SF2 to activate BASSMIDI.dll.
   // However this demo program requires Chorium.SF2.
      if BassPlayer1.MIDIFontInit(ProgDir + 'Plugins\Chorium.SF2', MIDI_FONTINFO) then
      begin
         StatusViewForm.InfoMemo.Lines.Add('');
         if BassPlayer1.MIDISoundReady then
            StatusViewForm.InfoMemo.Lines.Add(msgs[12]);

         StatusViewForm.InfoMemo.Lines.Add(msgs[13]);
         StatusViewForm.InfoMemo.Lines.Add(' Name : ' + MIDI_FONTINFO.FontName);
         StatusViewForm.InfoMemo.Lines.Add(' Presets : ' + formatFloat('#,##0', MIDI_FONTINFO.Presets));
         StatusViewForm.InfoMemo.Lines.Add(' Sample Size : '
                                           + formatFloat('#,##0', MIDI_FONTINFO.SampleSize) + ' bytes');
         StatusViewForm.InfoMemo.Lines.Add(' ');
      end
   end else
   begin
      StatusViewForm.InfoMemo.Lines.Add('');
      StatusViewForm.InfoMemo.Lines.Add('MIDI soundfont "Chorium.SF2" does not exist in '#10 +
                         '<Program_Directory>\Plugins directory.');
      StatusViewForm.InfoMemo.Lines.Add(msgs[14]);
      StatusViewForm.InfoMemo.Lines.Add(msgs[15] + chr(10) +
                         'http://www.un4seen.com/download.php?Chorium.exe');
      StatusViewForm.InfoMemo.Lines.Add(' ');
   end;
 end;

begin
   DoubleBuffered := True;  // Set to reduce the amount of flicker

   BassPlayer1 := TBassPlayer.Create(Self);
   ApplyLocalLanguage;

 // Check if required DLL's are succefully loaded and initialized.
   if (not BassPlayer1.PlayerReady) then
   begin
      Application.MessageBox(pChar(msgs[1]), pChar(msgs[26]), MB_OK or MB_ICONINFORMATION);
    //  btnOpen.Enabled := false;
      picEjectBtn.Enabled := false;
      picPlayListBtn.Enabled := false;
      picMuteBtn.Enabled := false;
      picLyricsBtn.Enabled := false;
      menuOpen.Enabled := false;
      menuOpenURL.Enabled := false;
      menuAddonSetup.Enabled := false;
      menuPluginSetup.Enabled := false;
      menuDSPCtrl.Enabled := false;
      menuVisCtrl.Enabled := false;
    //  exit;
   end else
      SetupBassPlayerHandlers;

   VolumeKnob.Position := BassPlayer1.Volume;
   CreateBasicImage;
 //  ShowBackground;
   for i := 1 to NumFFTBands do
      PeakValue[i] := 0;

   EchoSlider.Value := BassPlayer1.EchoLevel;
   ReverbSlider.Value := BassPlayer1.ReverbLevel;
   FlangerSlider.Value := BassPlayer1.FlangerLevel;

   About.BASSPlayVer := MajorVer + '.' + MinorVer + '.' + RevVer + ' (' + BuildDateStr + ')';
   About.TBASSPlayerVer := BassPlayer1.Version + ' (' + BassPlayer1.BuildDateStr + ')';
   if BassPlayer1.PlayerReady then
      About.BASSLibVer := BassPlayer1.BASSDLLVer
   else
      About.BASSLibVer := 'N.A.';

   if BassPlayer1.PlayerReady and BassPlayer1.VisDrawerReady then
      BassPlayer1.UseVisDrawer := true
   else
   begin
      VisControlForm.cbUseWinampLook.Checked := false;
      VisControlForm.cbUseWinampLook.Enabled := false;
   end;

   if BassPlayer1.PlayerReady then
   begin
   // Load bass_ac3.dll to support Dolby Digital AC-3 stream.
    //  if FileExists(GetProgDir + 'Plugins\bass_ac3.dll') then
    //     BassPlayer1.BASSAddonLoad(GetProgDir + 'Plugins\bass_ac3.dll');

    // Load bass_aac.dll to support AAC, AAC+ stream.
     if FileExists(GetProgDir + 'Plugins\bass_aac.dll') then
        BassPlayer1.BASSAddonLoad(GetProgDir + 'Plugins\bass_aac.dll');

    // Load in_midi.dll to support viewing file info. of MIDI file. (not needed for playing)
     LoadWinampPlugin('in_midi.dll');
   end;

   lbl_Title := TitleForStandby;
   CurVisWindowAttr.VisHandle := 0;
   CurVisWindowAttr.VisType := Unavailable;

  { if not BassPlayer1.MixerReady then
   begin
      cbDownMix.Checked := false;
      cbDownMix.Enabled := false;
   end else
      BassPlayer1.DownMixToStereo := true; }

   if BassPlayer1.PlayerReady then
   begin
      StatusViewForm.InfoMemo.Lines.Add('Loaded DLL''s :');
      StatusViewForm.InfoMemo.Lines.Add(' bass.dll for BASS basic native stream');
      tmpStr := UnloadedDLLMsg;
      if tmpStr <> '' then
        if pos(chr(10), tmpStr) > 0 then
           Application.MessageBox(pChar(msgs[2] + char(10) +
                           char(10) + tmpStr), pChar(msgs[26]), MB_OK or MB_ICONINFORMATION)
        else
           Application.MessageBox(pChar(msgs[3] + char(10) +
                           char(10) + tmpStr), pChar(msgs[26]), MB_OK or MB_ICONINFORMATION);

    // Following sentences are for supporting BASSMDIDI
      if BassPlayer1.BASSMIDIReady then
        SetupMIDISoundFont;
   end else
      StatusViewForm.InfoMemo.Lines.Add('BASS.DLL is not loaded.');

   if not BassPlayer1.DX8EffectReady then
   begin
      picEQBtn.Enabled := false;
      picResetBtn.Enabled := false;
      cbFlanger.Enabled := false;
      cbEcho.Enabled := false;
      cbReverb.Enabled := false;
   end;

   FillChar(LeosHandleList, SizeOf(LeosHandleList), 0);

   Self.Height := BasicHeight;
   m_bFocused := true;
   picTopText.Width := WriteTitle(Self.Caption, true) + 8;
   ResizeElement(Self);
   RelocateElement(Self.Width, Self.Height);
   ImageCopy;         // Copy basic bitmap images
   WriteTitle(Self.Caption, false);

   picRulerL.Left := picRulerR.Left;
   picRulerL.Width := 3;  // virtually hide picRulerL
   
   Saved8087CW := Default8087CW;
   Set8087CW($133f);  // Disable all fpu exceptions
   Timer_Stat.Enabled := true;
end;

procedure TMainForm.SetChannelType(ChannelType : DWORD);
begin
   case ChannelType of
      BASS_CTYPE_STREAM_OGG : lbl_Type.Caption := 'OGG';
      BASS_CTYPE_STREAM_MP1 : lbl_Type.Caption := 'MP1';
      BASS_CTYPE_STREAM_MP2 : lbl_Type.Caption := 'MP2';
      BASS_CTYPE_STREAM_MP3 : lbl_Type.Caption := 'MP3';
      BASS_CTYPE_STREAM_AIFF : lbl_Type.Caption := 'AIFF';
      BASS_CTYPE_STREAM_WAV : lbl_Type.Caption := 'WAV';
      BASS_CTYPE_STREAM_WAV_PCM : lbl_Type.Caption := 'PCM';
      BASS_CTYPE_STREAM_WAV_FLOAT : lbl_Type.Caption := 'WAV_F';
      BASS_CTYPE_MUSIC_MOD : lbl_Type.Caption := 'MOD';
      BASS_CTYPE_MUSIC_MTM : lbl_Type.Caption := 'MTM';
      BASS_CTYPE_MUSIC_S3M : lbl_Type.Caption := 'S3M';
      BASS_CTYPE_MUSIC_XM : lbl_Type.Caption := 'XM';
      BASS_CTYPE_MUSIC_IT : lbl_Type.Caption := 'IT';
      BASS_CTYPE_MUSIC_MO3 : lbl_Type.Caption := 'MO3';
      BASS_CTYPE_STREAM_MPC : lbl_Type.Caption := 'MPC';
      BASS_CTYPE_STREAM_AAC : lbl_Type.Caption := 'AAC';
      BASS_CTYPE_STREAM_MP4 : lbl_Type.Caption := 'MP4';
      BASS_CTYPE_STREAM_SPX : lbl_Type.Caption := 'SPX';
      BASS_CTYPE_STREAM_MIDI : lbl_Type.Caption := 'MIDI';
      BASS_CTYPE_STREAM_ALAC : lbl_Type.Caption := 'ALAC';
      BASS_CTYPE_STREAM_TTA : lbl_Type.Caption := 'TTA';
      BASS_CTYPE_STREAM_CD : lbl_Type.Caption := 'CD';
      BASS_CTYPE_STREAM_WMA : lbl_Type.Caption := ' WMA';   // put a preceeding space to prevent incorrect display of "W"
      BASS_CTYPE_STREAM_WMA_MP3 : lbl_Type.Caption := 'W_MP3';
      BASS_CTYPE_STREAM_OFR : lbl_Type.Caption := 'OFR';
      BASS_CTYPE_STREAM_APE : lbl_Type.Caption := 'APE';
      BASS_CTYPE_STREAM_AC3 : lbl_Type.Caption := 'AC3';
      BASS_CTYPE_STREAM_ADX : lbl_Type.Caption := 'ADX';
     else
       lbl_Type.Caption := '???'
   end;
end;

procedure TMainForm.SetTrackLenDisplay(TimeStr : string);
begin
   if length(TimeStr) = 5 then
   begin
      TrackMin10.Value := StrToInt(TimeStr[1]);
      TrackMin1.Value := StrToInt(TimeStr[2]);
      TrackSec10.Value := StrToInt(TimeStr[4]);
      TrackSec1.Value := StrToInt(TimeStr[5]);
   end else
   if length(TimeStr) = 8 then
   begin
      TrackMin10.Value := StrToInt(TimeStr[4]);
      TrackMin1.Value := StrToInt(TimeStr[5]);
      TrackSec10.Value := StrToInt(TimeStr[7]);
      TrackSec1.Value := StrToInt(TimeStr[8]);
   end;
end;

function TMainForm.OpenStream(StreamName : string; StartPlay : boolean) : boolean;
var
   s1 : string;
   URLInfo : pAnsiChar;
   i : integer;

   tmpStr : String;
   tmpPChar : pAnsiChar;
   tmpStr1 : AnsiString;
   tmpStr2 : WideString;
 //  PlaySec : real;
   MIDITrackInfo : TMIDITrackInfo;
begin
   result := false;

 // Inhibit duplicate call while previous call is being processed.
   if IsOpeningStream then
      exit
   else
      IsOpeningStream := true;

   IsNetRadio := false;
   IsNetStream := false;

   if BassPlayer1.Open(StreamName) then
   begin
    //  SetTitleText(msgs[18]);   // * processed at ModeChange event handler

      PlayLength := BassPlayer1.PlayLength;
      PlaySec := PlayLength / (1000 * 24 * 3600);
      if PlaySec < (1.0 / 24.0) then
         tmpStr := FormatDateTime ('nn:ss', PlaySec)
      else
         tmpStr := FormatDateTime ('hh:nn:ss', PlaySec);

      SetTrackLenDisplay(tmpStr);

     { if PassedActivation and (not BassPlayer1.IsNetRadio)then
         StatusViewForm.InfoMemo.Clear; }  // performed at Mode change event handler
      SPS := 0;
    //  edBitrate.Caption := '  0 KBPS';

      if BassPlayer1.IsNetRadio then
      begin
       // Following codes are performed at Mode change event handler for Net radio
       {  StatusViewForm.InfoMemo.Lines.Add(msgs[7]);
         StatusViewForm.InfoMemo.Lines.Add('URL : ' + BassPlayer1.StreamInfo.FileName);
         URLInfo := BassPlayer1.ICYInfo;
         if URLInfo <> nil then
            while StrLen(URLInfo) > 0 do
            begin
               if pos('icy-name:', ansistring(URLInfo)) <> 0 then
               begin
                  inc(URLInfo, 9);
                  StatusViewForm.InfoMemo.Lines.Add('icy-name : ' + ansistring(URLInfo));
                  break;
               end;
               inc(URLInfo, StrLen(URLInfo) + 1);
            end;

         SetBitrateText(BassPlayer1.StreamInfo.BitRate);  }

         IsNetRadio := true;

      end else
      if BassPlayer1.IsNetStream then
      begin
         SPS := BassPlayer1.StreamInfo.SampleRate;
         if SPS <> 0 then
         begin
            StatusViewForm.InfoMemo.Lines.Add(msgs[6]);
            StatusViewForm.InfoMemo.Lines.Add('URL : ' + BassPlayer1.StreamInfo.FileName);
            SetBitrateText(BassPlayer1.StreamInfo.BitRate);
         end;
         URLInfo := BassPlayer1.HTTPInfo;
         if URLInfo <> nil then
            while StrLen(URLInfo) > 0 do
            begin
              {$IFDEF DELPHI_2007_BELOW}
               if pos('Server:', ansistring(URLInfo)) <> 0 then
              {$ELSE}
               if posEX('Server:', ansistring(URLInfo), 1) <> 0 then
              {$ENDIF}
               begin
                  StatusViewForm.InfoMemo.Lines.Add(string(URLInfo));
                  break;
               end;
               inc(URLInfo, StrLen(URLInfo) + 1);
            end;
         IsNetStream := true;

        // <-
         URLInfo := BassPlayer1.ICYInfo;
         if URLInfo <> nil then
            while StrLen(URLInfo) > 0 do
            begin
               if (BassPlayer1.StreamInfo.Format = BASS_CTYPE_STREAM_WMA_MP3) or
                  (BassPlayer1.StreamInfo.Format = BASS_CTYPE_STREAM_WMA) then
               begin
                  tmpStr1 := ansistring(URLInfo);
                 {$IFDEF DELPHI_2007_BELOW}
                  tmpStr2 := UTF8Decode(tmpStr1);
                 {$ELSE}
                  tmpStr2 := UTF8ToWideString(tmpStr1);
                 {$ENDIF}
                  tmpPChar := ToPMultiByte(PWideChar(tmpStr2));
                  StatusViewForm.InfoMemo.Lines.Add(string(tmpPChar));
               end else
                  StatusViewForm.InfoMemo.Lines.Add(string(URLInfo));
               inc(URLInfo, StrLen(URLInfo) + 1);
            end;   // <-

      end else   // local stream file
      begin
         StatusViewForm.InfoMemo.Lines.Add(msgs[10]);
         StatusViewForm.InfoMemo.Lines.Add(ExtractFileName(BassPlayer1.StreamInfo.FileName));
         SetBitrateText(BassPlayer1.StreamInfo.BitRate);
      end;

      StatusViewForm.InfoMemo.Lines.Add(' ');
     { if BassPlayer1.DecodingByPlugin then
         StatusViewForm.InfoMemo.Lines.Add(' (' + msgs[8] + ')')
      else }
         StatusViewForm.InfoMemo.Lines.Add(msgs[9] + ' : ' + BassPlayer1.DecoderName);
            
      if not IsNetStream then
         SPS := BassPlayer1.StreamInfo.SampleRate;
      if (SPS <> 0) and (not BassPlayer1.IsNetRadio) then
         SetChannelInfo(SPS, BassPlayer1.StreamInfo.Channels);

    // Activate following sentences if you want to know the track information of a opened
    // MIDI file.
      if pos(UpperCase(ExtractFileExt(StreamName)), '*.MID;*.MIDI;*.RMI;*.KAR;') > 0 then
      begin
         if BassPlayer1.MIDIGetTrackInfo(StreamName, MIDITrackInfo) then
         begin
            StatusViewForm.InfoMemo.Lines.Add(' ');
            StatusViewForm.InfoMemo.Lines.Add('MIDI Track info :');
            for i := 1 to MIDITrackInfo.TrackCount do
               StatusViewForm.InfoMemo.Lines.Add('Track[' + intToStr(i-1) + '] : ' + MIDITrackInfo.TrackText[i-1]);
         end;
      end;

   // Scroll up InfoMemo to make its last line visible.
      StatusViewForm.InfoMemo.Perform(EM_SCROLL, SB_PAGEDOWN, 0);

      if IsNetRadio then
      begin
       //  PosSlider.ThumbVisible := false;
         picThumb.Visible := false;
       //  CheckCursor;
         picRulerL.Width := 3;   // virtually hide picRulerL
      end else
      begin
       //  PosSlider.ThumbVisible := true;
         picThumb.Left := picRulerR.Left;
         picThumb.Visible := true;
       //  CheckCursor;
      end;

      for i := 1 to HBlockCount do
          PeakValue[i] := 0;

      if OpenByButton or OpenByNetButton then
      begin
         PlayListConfigForm.ClearPlayList;  // Clear PlayList
         if BassPlayer1.IsNetRadio then
            PlayListConfigForm.AddTo_PlayList(BassPlayer1.StreamPath, '','', true)
         else
            PlayListConfigForm.AddTo_PlayList(BassPlayer1.StreamPath,
                                             BassPlayer1.StreamInfo.Title,
                                             BassPlayer1.StreamInfo.Artist, true);
      end else
      if (not PassedActivation) and IsNetStream then
         PlayListConfigForm.ChangeTitle(BassPlayer1.StreamPath,
                                        BassPlayer1.StreamInfo.Title, 
                                        BassPlayer1.StreamInfo.Artist);

      if StartPlay then
         BassPlayer1.Play;

      s1 := BassPlayer1.DecoderName;
      if copy(s1, 1, 5) = 'bass_' then
         AddonConfigForm.SetInUsePlugin(s1)
      else
         AddonConfigForm.SetInUsePlugin('');

      SetChannelType(BassPlayer1.StreamInfo.Format);
      result := true;
   end;

   OpenByButton := false;
   OpenByNetButton := false;
   IsOpeningStream := false;
end;

procedure TMainForm.btnOpenClick(Sender: TObject);
var
   s1, s2, s3 : string;
begin
  { if not BassPlayer1.PlayerReady then
   begin
      Application.MessageBox('Cannot open file (BassPlayer is not ready)',
                 'Confirm', MB_OK or MB_ICONINFORMATION);
      exit;
   end; }

   if IsOpeningStream then
      exit;

   OpenDialog1.FileName := '';
   s1 := BassPlayer1.NativeFileExts;
   s2 := BassPlayer1.PluginFileExts;
   s3 := BassPlayer1.BASSAddonExts;
   if (s2 = '') and (s3 = '') then
      OpenDialog1.Filter := msgs[22] + ' (' + s1 + ')|' + s1 + '|'
   else
      OpenDialog1.Filter := msgs[23] + ' |' + s1 + s2 + s3 + '|' +
                            msgs[22] + ' (' + s1 + ')|' + s1 + '|';
   if s2 <> '' then
      OpenDialog1.Filter := OpenDialog1.Filter + msgs[24] + ' (' + s2 + ')|' + s2 + '|';
   if s3 <> '' then
      OpenDialog1.Filter := OpenDialog1.Filter + msgs[25] + ' (' + s3 + ')|' + s3 + '|' ;

   if OpenDialog1.Execute then
   begin
      OpenByButton := true;
      OpenStream(OpenDialog1.FileName, true);
   end;
end;


procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   NowClosing := true;
   BassPlayer1.MagneticWindowRemove(Self.Handle);
   MagneticWndProc := nil;  // Disable Magnetic effect
   Timer_Stat.Enabled := false;

 //  BassPlayer1.Free;
   BasicBMP.Free;
   DisplayBar.Free;
   picMap.Free;
   picMap2.Free;
   picMap3.Free;

   TmpBmp.Free;
   TitleBackBmp.Free;

   Set8087CW(Saved8087CW);
end;

procedure TMainForm.btnPlayClick(Sender: TObject);
var
   i : integer;
begin
   if BassPlayer1.Mode = plmPlaying then
      exit;

   if PlayListConfigForm.NumEntry = 0 then
      exit;

   if PlayListConfigForm.SelectedEntry = -1 then
      exit;

   for i := 1 to HBlockCount do  
       PeakValue[i] := 0;

   if BassPlayer1.Mode = plmStopped then
   begin
      if BassPlayer1.StreamPath <> PlayListConfigForm.SelectedFile then
         OpenStream(PlayListConfigForm.SelectedFile, true)
      else
         BassPlayer1.Play;
   end
   else if (BassPlayer1.Mode = plmStandBy) then
      OpenStream(PlayListConfigForm.SelectedFile, true)
   else if BassPlayer1.Mode = plmPaused then
      BassPlayer1.Play
   else if BassPlayer1.Mode = plmReady then
      BassPlayer1.Play

end;

procedure TMainForm.btnPauseClick(Sender: TObject);
begin
   if BassPlayer1.Mode = plmPaused then
      exit;

   BassPlayer1.Pause(true);
end;

procedure TMainForm.btnStopClick(Sender: TObject);
begin
   if BassPlayer1.Mode = plmStopped then
      exit;

   BassPlayer1.Stop;
end;

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
   NowClosing := true;
   if PlayListConfigForm.Visible then
      PlayListConfigForm.btnCloseClick(Self);

   Close;
end;

procedure TMainForm.cbFlangerClick(Sender: TObject);
begin
   if cbFlanger.Checked then
      BassPlayer1.SoundEffects := BassPlayer1.SoundEffects + [Flanger]
   else
      BassPlayer1.SoundEffects := BassPlayer1.SoundEffects - [Flanger];

end;

procedure TMainForm.cbEchoClick(Sender: TObject);
begin
   if cbEcho.Checked then
      BassPlayer1.SoundEffects := BassPlayer1.SoundEffects + [Echo]
   else
      BassPlayer1.SoundEffects := BassPlayer1.SoundEffects - [Echo];
end;

procedure TMainForm.cbReverbClick(Sender: TObject);
begin
   if cbReverb.Checked then
      BassPlayer1.SoundEffects := BassPlayer1.SoundEffects + [Reverb]
   else
      BassPlayer1.SoundEffects := BassPlayer1.SoundEffects - [Reverb];
end;


procedure TMainForm.cbEqualizerClick(Sender: TObject);
begin
   if b_EqualizerOn then
      BassPlayer1.SoundEffects := BassPlayer1.SoundEffects + [Equalizer]
   else
      BassPlayer1.SoundEffects := BassPlayer1.SoundEffects - [Equalizer];
end;

procedure TMainForm.cbRotateClick(Sender: TObject);
begin
   if cbRotate.Checked then
      BassPlayer1.SoundEffects := BassPlayer1.SoundEffects + [Rotate]
   else
      BassPlayer1.SoundEffects := BassPlayer1.SoundEffects - [Rotate];
end;

procedure TMainForm.VolumeKnobChange(Sender: TObject);
begin
   BassPlayer1.Volume := VolumeKnob.Position;
end;

procedure TMainForm.EQSliderChange(Sender: TObject);
var
   BandNum : integer;
begin
   BandNum := (Sender as TSlider).Tag - 1;
   EQGains[BandNum] := (Sender as TSlider).Value - 15.0;

 //  BassPlayer1.EQGains := EQGains;
   BassPlayer1.SetAEQGain(BandNum, EQGains[BandNum]);
end;


procedure TMainForm.PosSliderStartTracking(Sender: TObject);
begin
   NowTracking := true;
end;

{ procedure TMainForm.PosSliderStopTracking(Sender: TObject);
var
   SongPos : DWORD;
begin
   if BassPlayer1.Seekable then
   begin
      SongPos := Trunc(DWORD(PosSlider.Value) * PlayLength / DWORD(PosSlider.MaxValue));
      BassPlayer1.Position := SongPos;
   end;

   NowTracking := false;
end; }

procedure TMainForm.SetBitrateText(BitRate : Dword);
begin
   if BitRate = 0 then
      edBitrate.Caption := '  0' + ' KBPS'
   else if BitRate > 99 then
      edBitrate.Caption := intToStr(BitRate) + ' KBPS'
   else
      edBitrate.Caption := ' ' + intToStr(BitRate) + ' KBPS';
end;

procedure TMainForm.SetTitleText(PrefixStr : string);
begin
   if BassPlayer1.IsNetRadio and (not PassedActivation) then
      lbl_Title := PrefixStr + ' >> ' + BassPlayer1.StreamPath
   else if BassPlayer1.StreamInfo.Title = '' then
      lbl_Title := PrefixStr + ' >> ' + BassPlayer1.StreamInfo.FileName
   else if BassPlayer1.StreamInfo.Artist <> '' then
      lbl_Title := PrefixStr + ' >> ' + BassPlayer1.StreamInfo.Artist + ' - ' + BassPlayer1.StreamInfo.Title
   else
      lbl_Title := PrefixStr + ' >> ' + BassPlayer1.StreamInfo.Title;
end;

procedure TMainForm.ModeChanged(Sender: TObject; OldMode, NewMode : TPlayerMode);
var
   URLInfo : pAnsiChar;

begin
   CurMode := NewMode;
   PlayListConfigForm.SetCurrentMode(CurMode);

   if CurMode = plmStandby then
   begin
      edBitrate.Caption := '  0 KBPS';
      StatusViewForm.InfoMemo.Clear;
    //  lbl_Title := msgs[17];  // 'Standby'
      lbl_Title := TitleForStandby;
      TrackMin10.Value := 0;
      TrackMin1.Value := 0;
      TrackSec10.Value := 0;
      TrackSec1.Value := 0;
      TimeMin10.Value := 0;
      TimeMin1.Value := 0;
      TimeSec10.Value := 0;
      TimeSec1.Value := 0;

   end else if CurMode = plmReady then
   begin
      if BassPlayer1.IsNetRadio then
         lbl_Title := msgs[18] + ' >> ' + BassPlayer1.StreamPath
      else
         SetTitleText(msgs[18]);

   // The StatusViewForm.InfoMemo may have been cleared if it's time to resume net radio.
   // So, it is needed to re-fill InfoMemo.
      if BassPlayer1.IsNetRadio {and (StatusViewForm.InfoMemo.Lines.Count = 0)} then
      begin
         StatusViewForm.InfoMemo.Lines.Add(msgs[7]);
         StatusViewForm.InfoMemo.Lines.Add('URL : ' + BassPlayer1.StreamInfo.FileName);
         URLInfo := BassPlayer1.ICYInfo;
         if URLInfo <> nil then
            while StrLen(URLInfo) > 0 do
            begin
              {$IFDEF DELPHI_2007_BELOW}
               if pos('icy-name:', ansistring(URLInfo)) <> 0 then
              {$ELSE}
               if posEX('icy-name:', ansistring(URLInfo), 1) <> 0 then
              {$ENDIF}
               begin
                  inc(URLInfo, 9);
                  StatusViewForm.InfoMemo.Lines.Add('icy-name : ' + ansistring(URLInfo));
                  break;
               end;
               inc(URLInfo, StrLen(URLInfo) + 1);
            end;

         SetBitrateText(BassPlayer1.StreamInfo.BitRate);

         SPS := BassPlayer1.StreamInfo.SampleRate;
         if SPS <> 0 then
            SetChannelInfo(SPS, BassPlayer1.StreamInfo.Channels);

         IsNetRadio := true;
      end;
   end
   else if CurMode = plmPlaying then
   begin
      SetTitleText(msgs[21]);
      if BassPlayer1.StreamPath <> PlayListConfigForm.SelectedFile then
         PlayListConfigForm.SelectEntryByName(BassPlayer1.StreamPath);

      ForceRefreshLyrics;
    //  Timer_Stat.Enabled := true;
      Timer_Stat.Interval := 50;
      bCheckStat := true;
      if (not IsNetRadio) and (not picThumb.Visible {PosSlider.ThumbVisible}) then
      begin
       //  PosSlider.ThumbVisible := true;
         picThumb.Left := picRulerR.Left;
         picThumb.Visible := true;
       //  CheckCursor;
      end;
   end else if CurMode = plmStopped then
   begin
      if IsNetradio then
         lbl_Title := msgs[19] + ' >> ' + BassPlayer1.StreamInfo.FileName + ' *'
      else
         SetTitleText(msgs[19]);
    //  PosSlider.Value := 0;
    //  PosSlider.ThumbVisible := false;
      picThumb.Visible := false;
    //  CheckCursor;
      picRulerL.Width := 3;   // virtually hide picRulerL
   end
   else if CurMode = plmPaused then
      if IsNetradio then
         lbl_Title := msgs[20] + ' >> ' + BassPlayer1.StreamInfo.FileName + ' *'
      else
         SetTitleText(msgs[20]);
   if (OldMode = plmPlaying) and (CurMode <> plmPlaying) then
   begin
    //  Timer_Stat.Enabled := false;
      Timer_Stat.Interval := 100;
      bCheckStat := false;
      ShowBackground;
   end;
end;

procedure TMainForm.PlayEnded(Sender: TObject);  // * New at Ver 2.00
var
   NextFile : string;
begin
   // Following code is for auto starting of next item of Play List.
   if PlayListConfigForm.SelectedEntry < (PlayListConfigForm.NumEntry - 1) then
   begin
      NextFile := PlayListConfigForm.SelectEntry(PlayListConfigForm.SelectedEntry + 1);
      if NextFile <> '' then
         OpenStream(NextFile, true);
   end else
   if (PlayListConfigForm.SelectedEntry > 1) and b_Repeat then
   begin
      NextFile := PlayListConfigForm.SelectEntry(0);
      if NextFile <> '' then
         OpenStream(NextFile, true);
   end else
   if b_Repeat then
      BassPlayer1.Play
   else
      SetTitleText(msgs[28]);
end;

procedure TMainForm.SetTimeDisplay(TimeStr : string);
begin
   if length(TimeStr) = 5 then
   begin
      TimeMin10.Value := StrToInt(TimeStr[1]);
      TimeMin1.Value := StrToInt(TimeStr[2]);
      TimeSec10.Value := StrToInt(TimeStr[4]);
      TimeSec1.Value := StrToInt(TimeStr[5]);
   end else
   if length(TimeStr) = 8 then
   begin
      TimeMin10.Value := StrToInt(TimeStr[4]);
      TimeMin1.Value := StrToInt(TimeStr[5]);
      TimeSec10.Value := StrToInt(TimeStr[7]);
      TimeSec1.Value := StrToInt(TimeStr[8]);
   end;
end;

procedure TMainForm.Timer_StatTimer(Sender: TObject);
var
   CheckTimeDisplay : boolean;
   tmpStr : string;
   Thumb_Left : integer;
   PosSlider_Value : integer;
begin
   Timer_Stat.Enabled := false;

   inc(TimerCounter);
   if TimerCounter = 140 then   {= the lowest common multiple of 4, 5 and 7 }
      TimerCounter := 0;

   if (TimerCounter mod 5) = 0 then   // Move title text per every 250ms
   begin
      CheckTimeDisplay := true;
      TitleTextOut(3);
   end else
      CheckTimeDisplay := false;

   if (TimerCounter mod 4) = 0 then   // Check title bar status per every 200ms
   begin
     if PlayListConfigForm.Visible then
        PlayListConfigForm.CheckTitleBar;

     if GetForegroundWindow <> Self.Handle then
     begin
       if m_bFocused then
       begin
         m_bFocused := false;
         UpperImageCopy;
         UpperImageLoad;
         WriteTitle(Self.Caption, false);
       end;
     end else   // GetForegroundWindow = Self.Handle
        if not m_bFocused then
        begin
          m_bFocused := true;
          UpperImageCopy;
          UpperImageLoad;
          WriteTitle(Self.Caption, false);
        end;
   end;

   if b_Mute and (not b_BtnPressed) then  // Blink Mute button per every 350ms
     if (TimerCounter mod 7) = 0 then
     begin
       if b_MuteHighlighted then
       begin
         b_MuteHighlighted := false;
         SetImage(picMuteBtn, 0, 0, 47, 15, picMap3, 123, 0);
       end else
       begin
         b_MuteHighlighted := true;
         SetImage(picMuteBtn, 0, 0, 47, 15, picMap3, 123, 38);
       end;

    // Following 2 line is to get Alpha Blended button image
    //   picMuteBtn.Refresh;
    //   GetAlphablendedImage(picMuteBtn, PanelImage, picMuteBtn.Left, picMuteBtn.Top, 50);
     end;

   if not bCheckStat then   // = BASSPlayer is not playing a stream
   begin
      if Shape_U.Brush.Color = $00402C00 then
      begin
         Shape_U.Brush.Color := $00B9C70C;
         Shape_L.Brush.Color := $00B9C70C;
         Shape_U.Pen.Color := $00B9C70C;
         Shape_L.Pen.Color := $00B9C70C;
      end;
      Timer_Stat.Enabled := true;
      exit;
   end;

   if CheckTimeDisplay and (CurMode = plmPlaying) then  // Check for time display per every 250ms
   begin
      if PlaySec < (1.0 / 24.0) then   // less than 1 hour ?
         tmpStr := FormatDateTime ('nn:ss', BassPlayer1.Position / (1000 * 24 * 3600))
      else
         tmpStr := FormatDateTime ('hh:nn:ss', BassPlayer1.Position / (1000 * 24 * 3600));

      if StrToint(copy(tmpStr, length(tmpStr), 1)) <> TimeSec1.Value then
      begin
         SetTimeDisplay(tmpStr);
         HiIntensityCounter := 0;
         if Shape_U.Brush.Color <> $00B9C70C then
         begin    // bright color for ":" if HiIntensityCounter is 0 or 1 (approx. 500ms)
            Shape_U.Brush.Color := $00B9C70C;
            Shape_L.Brush.Color := $00B9C70C;
            Shape_U.Pen.Color := $00B9C70C;
            Shape_L.Pen.Color := $00B9C70C;
         end;

         if (not IsNetRadio) and (not NowTracking) then
            if PlaySec > 0 then
            begin
             //  PosSlider.Value := (BassPlayer1.Position * DWORD(PosSlider.MaxValue)) div PlayLength;
               PosSlider_Value := (BassPlayer1.Position * DWORD(PosSlider_MaxValue)) div PlayLength;
               Thumb_Left := picRulerR.Left +
                       round((PosSlider_Value / PosSlider_MaxValue) * (picRulerR.Width - picThumb.Width));
               if Thumb_Left <= (picRulerR.Left + picRulerR.Width - picThumb.Width) then
                  picThumb.Left := Thumb_Left
               else
                  picThumb.Left := (picRulerR.Left + picRulerR.Width - picThumb.Width);
             //  CheckCursor;
               picRulerL.Width := picThumb.Left - picRulerR.Left;
            end;
      end else
      begin
         inc(HiIntensityCounter);
         if HiIntensityCounter >= 2 then
           if Shape_U.Brush.Color = $00B9C70C then
           begin       // dark color for ":" if HiIntensityCounter >= 2  (approx. 500ms)
             Shape_U.Brush.Color := $00402C00;
             Shape_L.Brush.Color := $00402C00;
             Shape_U.Pen.Color := $00402C00;
             Shape_L.Pen.Color := $00402C00;
           end;
      end;
   end;

   Timer_Stat.Enabled := true;
end;


// Set background image & bar image for spectrum display
procedure TMainForm.CreateBasicImage;
var
   i : integer;
   R, G, B: Integer;
begin
   BasicBMP := TBitMap.Create;
   BasicBMP.Width := SpectrumImage.Width;
   BasicBMP.Height := SpectrumImage.Height;

  // Backup SpectrumImage's original background image to BasicBMP
   BitBlt(BasicBMP.Canvas.Handle, // handle to destination device context
          0,                    // x-coordinate of destination rectangle's upper-left corner
          0,                    // y-coordinate of destination rectangle's upper-left corner
          BasicBMP.Width,       // width of destination rectangle
          BasicBMP.Height,      // height of destination rectangle
          SpectrumImage.Picture.Bitmap.Canvas.Handle, // handle to source device context
          0,                    // x-coordinate of source rectangle's upper-left corner
          0,                    // y-coordinate of source rectangle's upper-left corner
          SRCCOPY);             // Copies the source rectangle directly to the destination rectangle.

   GaugeRect.Left := 0;
   GaugeRect.Top := 0;
   GaugeRect.Right := BasicBMP.Width;
   GaugeRect.Bottom := BasicBMP.Height;

   DisplayBar := TBitmap.Create;
   DisplayBar.PixelFormat := pf32bit;
   DisplayBar.Width := BlockWidth;
   DisplayBar.Height := VLimit;

   R := 255;
   G := 0;
   B := 0;

   for i := 0 to VLimit - 1 do
   begin
     if i > VLimit / 2 then
        Dec(R, Trunc(256 / VLimit))
     else
        Inc(G, Trunc(768 / VLimit));
     if R < 0 then R := 0;
     if G > 255 then G := 255;
     DisplayBar.Canvas.Brush.Color := TColor(RGB(R, G, B));
     DisplayBar.Canvas.FillRect(Rect(0, i, BlockWidth, i + 1));
   end;

end;

// Winamp plug-in notified that it has started playing a stream file
procedure TMainForm.PluginStartPlay(Sender: TObject; ChannelInfo : PChannelInfo);
var
   tmpStr : string;
begin
   if SPS = 0 then     // = have not gotten stream information yet
   begin
      if IsNetStream then
      begin
         StatusViewForm.InfoMemo.Lines.Add(msgs[6]);
         StatusViewForm.InfoMemo.Lines.Add('URL : ' + BassPlayer1.StreamInfo.FileName);
        { if BassPlayer1.DecodingByPlugin then
            StatusViewForm.InfoMemo.Lines.Add(' (' + msgs[8] + ')'); }
      end;

      SPS := ChannelInfo^.SampleRate;
      if SPS <> 0 then
         SetChannelInfo(SPS, BassPlayer1.StreamInfo.Channels);

    // Scroll down InfoMemo to make its 1st line as the top line.     
      // StatusViewForm.InfoMemo.Perform(EM_SCROLL, SB_PAGEUP, 0);

      PlayLength := BassPlayer1.PlayLength;
      PlaySec := PlayLength / (1000 * 24 * 3600);
      if PlaySec < (1.0 / 24.0) then   // less than 1 hour ?
         tmpStr := FormatDateTime ('nn:ss', PlaySec)
      else
         tmpStr := FormatDateTime ('hh:nn:ss', PlaySec);
      SetTrackLenDisplay(tmpStr);

      SetTitleText(msgs[21]);
      if not BassPlayer1.IsNetradio then
         PlayListConfigForm.ChangeTitle(BassPlayer1.StreamPath,
                                        BassPlayer1.StreamInfo.Title,
                                        BassPlayer1.StreamInfo.Artist);
      ForceRefreshLyrics;
   end;
end;

// Winamp plug-in has notified that it had gotten channel information on a playing stream file
// The first call of this procedure precedes PluginStartPlay.
procedure TMainForm.PluginChannelInfo(Sender: TObject; ChannelInfo : PChannelInfo);
begin
  // SPS := ChannelInfo^.SampleRate;

   SetBitrateText(ChannelInfo^.BitRate);
end;

procedure TMainForm.GetMETAFromNet(Sender: TObject; Content : ansistring);
{var
   tmpStr : string; }
begin
 //  tmpStr := trim(GetLimitedStr(Content, TitleLimit));
 //  tmpStr := Content;
  { if tmpStr = '' then
      exit; }

   lbl_Title := msgs[21] + ' >> ' + Content;
  // PlayListConfigForm.ChangeTitle(BassPlayer1.StreamPath, tmpStr,
  //                                BassPlayer1.StreamInfo.Artist);

   ForceRefreshLyrics;
end;

procedure TMainForm.GetMIDILyric(Sender: TObject; TextP : pAnsiChar);
var
   RawLyricStr : ansistring;
begin
   if not StatusViewForm.Visible then
      StatusViewForm.Show;
      
   RawLyricStr := ansistring(TextP);
   if RawLyricStr[1] = '\' then
   begin
    //  StatusViewForm.InfoMemo.Clear;
      StatusViewForm.InfoMemo.Lines.Add(copy(RawLyricStr, 2, length(RawLyricStr) - 1));
   end
   else if RawLyricStr[1] = '/' then
       StatusViewForm.InfoMemo.Lines.Add(copy(RawLyricStr, 2, length(RawLyricStr) - 1))
   else
      StatusViewForm.InfoMemo.Lines[StatusViewForm.InfoMemo.Lines.Count - 1] :=
         StatusViewForm.InfoMemo.Lines[StatusViewForm.InfoMemo.Lines.Count - 1] + RawLyricStr;
end;

procedure TMainForm.DownloadEnded(Sender: TObject; Amount : DWORD);
var
   tmpStr : string;
begin
 // Now we can get the exact value of playback length of the stream file from internet.
 //  Label_Length.Caption := 'Length ' +
 //           FormatDateTime ('nn:ss', StrToInt(Content) / (1000 * 24 * 3600));
   PlayLength := Amount;
   PlaySec := PlayLength / (1000 * 24 * 3600);
   if PlaySec < (1.0 / 24.0) then
      tmpStr := FormatDateTime ('nn:ss', PlaySec)
   else
      tmpStr := FormatDateTime ('hh:nn:ss', PlaySec);
   SetTrackLenDisplay(tmpStr);

 // We may have gotten correct 'BitRate' and 'Title' for opened stream file.
   SetBitrateText(BassPlayer1.StreamInfo.BitRate);

   if (BassPlayer1.StreamInfo.Title <> '') then
   begin
      if BassPlayer1.Mode = plmReady then
         SetTitleText(msgs[18])
      else if BassPlayer1.Mode = plmPlaying then
         SetTitleText(msgs[21]);
      PlayListConfigForm.ChangeTitle(BassPlayer1.StreamPath,
                                     BassPlayer1.StreamInfo.Title,
                                     BassPlayer1.StreamInfo.Artist);
      ForceRefreshLyrics;
   end;
end;

// Redraw spectrum image according to FFT Data
procedure TMainForm.DisplayFFTBand(Sender: TObject; Bands : TBandOut);
var
   tmpRect, BarRect : TRect;
   j : integer;
begin
 // Copy BasicBMP's image to SpectrumImage
   BitBlt(SpectrumImage.Picture.Bitmap.Canvas.Handle, // handle to destination device context
          GaugeRect.Left,	// x-coordinate of destination rectangle's upper-left corner
          GaugeRect.Top,	// y-coordinate of destination rectangle's upper-left corner
          BasicBMP.Width,	// width of destination rectangle
          BasicBMP.Height,	// height of destination rectangle
          BasicBMP.Canvas.Handle, // handle to source device context
          GaugeRect.Left,	// x-coordinate of source rectangle's upper-left corner
          GaugeRect.Top,	// y-coordinate of source rectangle's upper-left corner
          SRCCOPY);             // Copies the source rectangle directly to the destination rectangle.

 // Draw spectrum image
   for j := 1 to HBlockCount do
   begin
      if Bands[j-1] > VLimit then
         Bands[j-1] := VLimit;

      if Bands[j-1] > 0 then
      begin
     // Copy partial image of DisplayBar
        BarRect.Left := 0;
        BarRect.Right := BlockWidth;
        BarRect.Top := VLimit - Bands[j-1];
        if BarRect.Top < 0 then
           BarRect.Top := 0;
        BarRect.Bottom := DisplayBar.Height;

        tmpRect.Left := (BlockWidth + HBlockGap) * (j - 1) + 3;
        tmpRect.Right := tmpRect.Left + BlockWidth;
        tmpRect.Top := BarRect.Top;
        tmpRect.Bottom := BarRect.Bottom;

        BitBlt(SpectrumImage.Picture.Bitmap.Canvas.Handle,
               tmpRect.Left,
               tmpRect.Top + SpectrumImage.Height - VLimit - 2,
               BlockWidth,
               tmpRect.Bottom - tmpRect.Top + 1,
               DisplayBar.Canvas.Handle,
               BarRect.Left,
               BarRect.Top,
               SRCCOPY);
      end;

      if Bands[j-1] >= trunc(PeakValue[j]) then
      begin
         PeakValue[j] := Bands[j-1] + 0.01;  // 0.01 : to compensate round off
         PassedCounter[j] := 0;
      end else if Bands[j-1] < trunc(PeakValue[j]) then
      begin
         if trunc(PeakValue[j]) > 0 then
         begin
            with SpectrumImage.Picture.Bitmap.Canvas do
            begin
            // Draw peak line
               Pen.Color := TColor(RGB(192, 192, 192));   // color for peak line
               MoveTo((BlockWidth + HBlockGap) * (j - 1) + 3, SpectrumImage.Height - trunc(PeakValue[j]) - 2);
               LineTo((BlockWidth + HBlockGap) * (j - 1) + 3 + BlockWidth, SpectrumImage.Height - trunc(PeakValue[j]) - 2);
            end;

      // Followings are to show simillar spectrum image to WINAMP's
      //  - Put delay time before lowering peak line
      //  - Accerate lowering speed according to the time elapsed
            if PassedCounter[j] >= 8 then
                PeakValue[j] := PeakValue[j] - 0.3 * (PassedCounter[j] - 8);

            if PeakValue[j] < 0 then
               PeakValue[j] := 0
            else
               inc(PassedCounter[j]);
         end;
      end;

   end;

   SpectrumImage.Refresh;
end;

// Show background image during non-playing period
procedure TMainForm.ShowBackground;
begin
   BitBlt(SpectrumImage.Picture.Bitmap.Canvas.Handle,
          GaugeRect.Left,
          GaugeRect.Top,
          BasicBMP.Width,
          BasicBMP.Height,
          BasicBMP.Canvas.Handle,
          GaugeRect.Left,
          GaugeRect.Top,
          SRCCOPY);

   SpectrumImage.Refresh;
end;


procedure TMainForm.btnPluginSetupClick(Sender: TObject);
begin
   BassPlayer1.ShowPluginConfigForm;
end;

procedure TMainForm.EchoSliderChange(Sender: TObject);
begin
   BassPlayer1.EchoLevel := EchoSlider.Value;
end;

procedure TMainForm.FlangerSliderChange(Sender: TObject);
begin
   BassPlayer1.FlangerLevel := FlangerSlider.Value;
end;

procedure TMainForm.ReverbSliderChange(Sender: TObject);
begin
   BassPlayer1.ReverbLevel := ReverbSlider.Value;
end;

procedure TMainForm.btnShowVisCtrlFormClick(Sender: TObject);
begin
   VisControlForm.Show;
end;

procedure TMainForm.btnShowDSPCtrlFormClick(Sender: TObject);
begin
   DSPControlForm.Show;
end;

procedure TMainForm.btnNetOpenClick(Sender: TObject);
var
   tmpStr1, tmpStr2, tmpStr3 : string;
begin
  { if not BassPlayer1.PlayerReady then
   begin
      Application.MessageBox('Cannot open file (BassPlayer is not ready)',
                 'Confirm', MB_OK or MB_ICONINFORMATION);
      exit;
   end; }

   tmpStr1 := copy(URLInputForm.ComboBox1.Text, 1, 8);
   tmpStr2 := copy(URLInputForm.ComboBox1.Text, 1, 7);
   tmpStr3 := copy(URLInputForm.ComboBox1.Text, 1, 6);
   if (tmpStr1 <> '"http://') and (tmpStr2 <> 'http://') and
      (tmpStr2 <> '"mms://') and (tmpStr3 <> 'mms://') then
   begin
      Application.MessageBox(pChar(msgs[4]),
                             pChar(msgs[26]), MB_OK or MB_ICONINFORMATION);
      exit;
   end;

   if IsOpeningStream then
      exit;

  // lbl_Title := 'Waiting stream from NET';
  // Label_Length.Caption := '00:00';
   SetTrackLenDisplay('00:00');

   OpenByNetButton := true;
   OpenStream(URLInputForm.ComboBox1.Text, true);
end;


//------------------ Skin related functions/procedures --------------------------

procedure TMainForm.RelocateElement(Width_, Height_ : integer);
begin
 //  The placement of graphic elements in title bar is as follows
 //  ------------------------------------------------------------------
 //  | TopL | TopStretchL | TopF | TopText | TopM | TopStretchR | TopC |
 //  ------------------------------------------------------------------

 //  The placement of graphic elements in bottom area is as follows
 //  ---------------------------------------------------------------------
 //  | BottomL | BottomStretchL | BottomExpand | BottomStretchR | BottomR |
 //  ---------------------------------------------------------------------

  picTopStretchL.Canvas.MoveTo(picTopL.Width, 0);
  picTopText.Canvas.MoveTo((Width_ div 2) - (picTopText.Width div 2), 0);
  picTopF.Canvas.MoveTo(picTopText.Left - picTopF.Width, 0);
  picTopM.Canvas.MoveTo(picTopText.Left + picTopText.Width, 0);
  picTopStretchR.Canvas.MoveTo(picTopM.Left + picTopM.Width, 0);

  picTopL.Canvas.MoveTo(0, 0);
  picMenu.Canvas.MoveTo(4, 4);
  picTopC.Canvas.MoveTo(Width_ - picTopC.Width, 0);
  picClose.Canvas.MoveTo(Width_ - 12, 2);

  picBorderL.Canvas.MoveTo(0, T_Margin);
  picBorderR.Canvas.MoveTo(Width_ - R_Margin, T_Margin);

  picBottomL.Canvas.MoveTo(0, Height_ - B_Margin);
  picBottomStretchL.Canvas.MoveTo(picBottomL.Width, Height_ - B_Margin);
  picBottomExpand.Canvas.MoveTo(Width_ div 2 - 12, Height_ - B_Margin);
  picBottomStretchR.Canvas.MoveTo(picBottomExpand.Left + picBottomExpand.Width, Height_ - B_Margin);
  picBottomR.Canvas.MoveTo(Width_ - 125, Height_ - B_Margin);
end;

procedure TMainForm.ResizeElement(Sender: TObject);
var
  picBorderL_hDC: HDC;
  picBorderR_hDC: HDC;

  WidthTotal : integer;
begin
  picTopL.left := 0;
  picTopL.Top  := 0;

  picTopC.left := Width - picTopC.Width;
  picTopC.top  := 0;

  picMenu.Left := 4;
  picMenu.Top := 4;
  picClose.Left   := Width - 12;
  picClose.Top    := 2;

  picTopText.Left   := (Width div 2) - (picTopText.Width div 2);
  picTopText.Top    := 0;

  picTopF.left   := picTopText.Left - picTopF.Width;
  picTopF.top    := 0;

  picTopStretchL.left   := picTopL.Left + picTopL.Width;
  picTopStretchL.top    := 0;
  picTopStretchL.Width := picTopF.Left - picTopStretchL.left;

  picTopM.left   := picTopText.Left + picTopText.Width;
  picTopM.top    := 0;

  picTopStretchR.left   := picTopM.Left + picTopM.Width;
  picTopStretchR.top    := 0;
  picTopStretchR.Width  := picTopC.left - picTopStretchR.left;

  WidthTotal := picTopL.Width + picTopStretchL.Width + picTopF.Width + picTopText.Width
                + picTopM.Width + picTopStretchR.Width + picTopC.Width;
  if WidthTotal < Width then
  begin
     picTopStretchL.Width := picTopStretchL.Width + ((Width - WidthTotal + 1) div 2);
     picTopStretchR.Width := picTopStretchR.Width + ((Width - WidthTotal + 1) div 2);
  end;

  picBottomL.left := 0;
  picBottomL.Top  := Height - B_Margin;

  picBottomR.left := Width - 125;
  picBottomR.top  := Height - B_Margin;

  picBorderL.Canvas.MoveTo(0, T_Margin);
  picBorderL.Left   := 0;
  picBorderL.Top    := T_Margin;
  picBorderL.Height := Height - TB_Margin;

  picBorderL_hDC := picBorderL.Picture.Bitmap.Canvas.Handle;
  StretchBlt(picBorderL_hDC, 0, 0, L_Margin, Height - TB_Margin, PicMapDC, 127, 42, L_Margin, 29, SRCCOPY);
  picBorderL.Refresh;

  picBorderR.Canvas.MoveTo(Width - R_Margin, T_Margin);
  picBorderR.left   := Width - R_Margin;
  picBorderR.Top    := T_Margin;
  picBorderR.Height := Height - TB_Margin;

  picBorderR_hDC := picBorderR.Picture.Bitmap.Canvas.Handle;
  StretchBlt(picBorderR_hDC, 0, 0, R_Margin, Height - TB_Margin, PicMapDC, 139, 42, R_Margin, 29, SRCCOPY);
  picBorderR.Refresh;

  picBottomExpand.Left := Width div 2 - 12;
  picBottomExpand.Top    := Height - B_Margin;

  picBottomStretchL.left   := picBottomL.Width;
  picBottomStretchL.Top    := Height - B_Margin;
  picBottomStretchL.Width  := picBottomExpand.Left - picBottomStretchL.left;

  picBottomStretchR.left   := picBottomExpand.Left + picBottomExpand.Width;
  picBottomStretchR.Top    := Height - B_Margin;
  picBottomStretchR.Width  := picBottomR.left - picBottomStretchR.left;
end;

procedure TMainForm.SetImage(Destination: TImage;
                                x, y, W, H: integer;
                                Source: TImage;
                                startX, StartY: integer);
var
  DestDC: HDC;
  SrcDC:  HDC;

begin
  DestDC := Destination.Picture.Bitmap.Canvas.Handle;
  SrcDC  := Source.Picture.Bitmap.Canvas.Handle;

  BitBlt(DestDC, x, y, W, H, SrcDC, startX, StartY, SRCCOPY);
  Destination.Refresh;
end;

procedure TMainForm.UpperImageCopy;

begin
  if m_bFocused then
  begin
    SetImage(picTopC, 0, 0, 25, T_Margin, picMap, 130, 0);
    SetImage(picTopL, 0, 0, 25, T_Margin, picMap, 0, 0);
    SetImage(picTopF, 0, 0, 25, T_Margin, picMap, 26, 0);
    SetImage(picTopM, 0, 0, 25, T_Margin, picMap, 78, 0);
    SetImage(picMenu, 0, 0, 11, 10, picTopL, 4, 4);
  end else
  begin
    SetImage(picTopC, 0, 0, 25, T_Margin, picMap, 130, 21);
    SetImage(picTopL, 0, 0, 25, T_Margin, picMap, 0, 21);
    SetImage(picTopF, 0, 0, 25, T_Margin, picMap, 26, 21);
    SetImage(picTopM, 0, 0, 25, T_Margin, picMap, 78, 21);
    SetImage(picMenu, 0, 0, 11, 10, picTopL, 4, 4);
  end;

  SetImage(picClose, 0, 0, 9, 10, picTopC, 13, 2);
end;

procedure TMainForm.UpperImageLoad;
var
  picTop_HDC: HDC;

begin
  picTop_HDC := picTopText.Picture.Bitmap.Canvas.Handle;
  PicMapDC := PicMap.Picture.Bitmap.Canvas.Handle;  // ** should be re-defined for each use
  picW := picTopText.Width;
  if m_bFocused then
     StretchBlt(picTop_HDC, 0, 0, picW, T_Margin, PicMapDC, 52, 0, 25, T_Margin, SRCCOPY)
  else
     StretchBlt(picTop_HDC, 0, 0, picW, T_Margin, PicMapDC, 52, 21, 25, T_Margin, SRCCOPY);
  picTopText.Refresh;

  picTop_HDC := picTopStretchL.Picture.Bitmap.Canvas.Handle;
  PicMapDC := PicMap.Picture.Bitmap.Canvas.Handle;  // ** should be re-defined for each use
  picW := picTopStretchL.Width;
  if m_bFocused then
     StretchBlt(picTop_HDC, 0, 0, picW, T_Margin, PicMapDC, 104, 0, 25, T_Margin, SRCCOPY)
  else
     StretchBlt(picTop_HDC, 0, 0, picW, T_Margin, PicMapDC, 104, 21, 25, T_Margin, SRCCOPY);
  picTopStretchL.Refresh;

  picTop_HDC := picTopStretchR.Picture.Bitmap.Canvas.Handle;
  PicMapDC := PicMap.Picture.Bitmap.Canvas.Handle;  // ** should be re-defined for each use
  picW := picTopStretchR.Width;
  if m_bFocused then
     StretchBlt(picTop_HDC, 0, 0, picW, T_Margin, PicMapDC, 104, 0, 25, T_Margin, SRCCOPY)
  else
     StretchBlt(picTop_HDC, 0, 0, picW, T_Margin, PicMapDC, 104, 21, 25, T_Margin, SRCCOPY);
  picTopStretchR.Refresh;

end;

procedure TMainForm.BtnImageLoad(Btn_No : integer; b_Pressed, b_Highlighted : boolean);
begin
   if NowClosing then
     exit;

   case Btn_No of
      21 : if b_Pressed then
             SetImage(picPrevBtn, 0, 0, 22, 18, picMap2, 0, 19)
          else if b_Highlighted then
             SetImage(picPrevBtn, 0, 0, 22, 18, picMap2, 0, 38)
          else
             SetImage(picPrevBtn, 0, 0, 22, 18, picMap2, 0, 0);
      22 : if b_Pressed then
             SetImage(picPlayBtn, 0, 0, 22, 18, picMap2, 23, 19)
          else if b_Highlighted then
             SetImage(picPlayBtn, 0, 0, 22, 18, picMap2, 23, 38)
          else
             SetImage(picPlayBtn, 0, 0, 22, 18, picMap2, 23, 0);
      23 : if b_Pressed then
             SetImage(picPauseBtn, 0, 0, 22, 18, picMap2, 46, 19)
          else if b_Highlighted then
             SetImage(picPauseBtn, 0, 0, 22, 18, picMap2, 46, 38)
          else
             SetImage(picPauseBtn, 0, 0, 22, 18, picMap2, 46, 0);
      24 : if b_Pressed then
             SetImage(picStopBtn, 0, 0, 22, 18, picMap2, 69, 19)
          else if b_Highlighted then
             SetImage(picStopBtn, 0, 0, 22, 18, picMap2, 69, 38)
          else
             SetImage(picStopBtn, 0, 0, 22, 18, picMap2, 69, 0);
      25 : if b_Pressed then
             SetImage(picNextBtn, 0, 0, 22, 18, picMap2, 92, 19)
          else if b_Highlighted then
             SetImage(picNextBtn, 0, 0, 22, 18, picMap2, 92, 38)
          else
             SetImage(picNextBtn, 0, 0, 22, 18, picMap2, 92, 0);
      26 : if b_Pressed then
             SetImage(picEjectBtn, 0, 0, 22, 18, picMap2, 115, 19)
          else if b_Highlighted then
             SetImage(picEjectBtn, 0, 0, 22, 18, picMap2, 115, 38)
          else
             SetImage(picEjectBtn, 0, 0, 22, 18, picMap2, 115, 0);

      31 : if b_Pressed then
           begin
              if b_Repeat then
                 SetImage(picRepeatBtn, 0, 0, 28, 18, picMap3, 0, 57)
              else
                 SetImage(picRepeatBtn, 0, 0, 28, 18, picMap3, 0, 19)
           end else
              if b_Repeat then
                 SetImage(picRepeatBtn, 0, 0, 28, 18, picMap3, 0, 38)
              else
                 SetImage(picRepeatBtn, 0, 0, 28, 18, picMap3, 0, 0);
      32 : if b_Pressed then
           begin
              if b_PlayList then
                 SetImage(picPlayListBtn, 0, 0, 28, 18, picMap3, 29, 95)
              else
                 SetImage(picPlayListBtn, 0, 0, 28, 18, picMap3, 0, 95)
           end else
              if b_PlayList then
                 SetImage(picPlayListBtn, 0, 0, 28, 18, picMap3, 29, 76)
              else
                 SetImage(picPlayListBtn, 0, 0, 28, 18, picMap3, 0, 76);
      33 : begin
           if b_Pressed then
           begin
              if b_ViewLyrics then
                 SetImage(picLyricsBtn, 0, 0, 47, 18, picMap3, 75, 57)
              else
                 SetImage(picLyricsBtn, 0, 0, 47, 18, picMap3, 75, 19)
           end else
              if b_ViewLyrics then
                 SetImage(picLyricsBtn, 0, 0, 47, 18, picMap3, 75, 38)
              else
                 SetImage(picLyricsBtn, 0, 0, 47, 18, picMap3, 75, 0);

          // Following 2 line is to get Alpha Blended button image
          //    picLyricsBtn.Refresh;
          //    GetAlphablendedImage(picLyricsBtn, PanelImage, picMuteBtn.Left, picMuteBtn.Top, 50);
           end;
      36 : begin
           if b_Pressed then
           begin
              if b_Mute then
                 SetImage(picMuteBtn, 0, 0, 47, 15, picMap3, 123, 57)
              else
                 SetImage(picMuteBtn, 0, 0, 47, 15, picMap3, 123, 19)
           end else
              if b_Mute then
                 SetImage(picMuteBtn, 0, 0, 47, 15, picMap3, 123, 38)
              else
                 SetImage(picMuteBtn, 0, 0, 47, 15, picMap3, 123, 0);

          // Following 2 line is to get Alpha Blended button image
          //    picMuteBtn.Refresh;
          //    GetAlphablendedImage(picMuteBtn, PanelImage, picMuteBtn.Left, picMuteBtn.Top, 50);
           end;
      34 : if b_Pressed then
           begin
              if b_EqualizerOn then
                 SetImage(picEQBtn, 0, 0, 28, 15, picMap3, 87, 95)
              else
                 SetImage(picEQBtn, 0, 0, 28, 15, picMap3, 58, 95)
           end else
              if b_EqualizerOn then
                 SetImage(picEQBtn, 0, 0, 28, 15, picMap3, 87, 76)
              else
                 SetImage(picEQBtn, 0, 0, 28, 15, picMap3, 58, 76);
      35 : if b_Pressed then
              SetImage(picResetBtn, 0, 0, 30, 15, picMap3, 116, 95)
           else
              SetImage(picResetBtn, 0, 0, 30, 15, picMap3, 116, 76);
      99 : if b_Pressed then
              SetImage(picClose, 0, 0, 9, 10, picTopC, 13, 2);
    end;;
end;

procedure TMainForm.ExpandImageLoad(b_Highlighted : boolean);
begin
   if Self.Height <> ExpandedHeight then
      if b_Highlighted then
         SetImage(picBottomExpand, 0, 0, 24, 14, picMap, 50, 72)
      else
         SetImage(picBottomExpand, 0, 0, 24, 14, picMap, 0, 72)
   else
      if b_Highlighted then
         SetImage(picBottomExpand, 0, 0, 24, 14, picMap, 75, 72)
      else
         SetImage(picBottomExpand, 0, 0, 24, 14, picMap, 25, 72);
end;

procedure TMainForm.ImageCopy;
var
  picBottomM_HDC: HDC;

begin
  UpperImageCopy;   // for fixed width items
  UpperImageLoad;   // for variable width items

  picBottomM_HDC := picBottomStretchL.Picture.Bitmap.Canvas.Handle;
  PicMapDC := PicMap.Picture.Bitmap.Canvas.Handle;  // ** should be re-defined for each use
  picW := picBottomStretchL.Width;
  StretchBlt(picBottomM_HDC, 0, 0, picW, B_Margin, PicMapDC, 127, 72, 25, B_Margin, SRCCOPY);
  picBottomStretchL.Refresh;
  picBottomM_HDC := picBottomStretchR.Picture.Bitmap.Canvas.Handle;
  PicMapDC := PicMap.Picture.Bitmap.Canvas.Handle;  // ** should be re-defined for each use
  picW := picBottomStretchR.Width;
  StretchBlt(picBottomM_HDC, 0, 0, picW, B_Margin, PicMapDC, 127, 72, 25, B_Margin, SRCCOPY);
  picBottomStretchR.Refresh;

 { picBottomM_HDC := picBottomExpand.Picture.Bitmap.Canvas.Handle;
  PicMapDC := PicMap.Picture.Bitmap.Canvas.Handle;  // ** should be re-defined for each use
  picW := picBottomExpand.Width;
  StretchBlt(picBottomM_HDC, 0, 0, picW, B_Margin, PicMapDC, 0, 72, 24, B_Margin, SRCCOPY);
  picBottomExpand.Refresh; }

  SetImage(picBottomR, 0, 0, 125, B_Margin, picMap, 0, 57);
  SetImage(picBottomL, 0, 0, 125, B_Margin, picMap, 0, 42);
  SetImage(picBorderL, 0, 0, L_Margin, 29, picMap, 127, 42);
  SetImage(picBorderR, 0, 0, R_Margin, 29, picMap, 139, 42);
  ExpandImageLoad(ExpandHighlighted);
  BtnImageLoad(picPrevBtn.Tag, false, false);
  BtnImageLoad(picPlayBtn.Tag, false, false);
  BtnImageLoad(picPauseBtn.Tag, false, false);
  BtnImageLoad(picStopBtn.Tag, false, false);
  BtnImageLoad(picNextBtn.Tag, false, false);
  BtnImageLoad(picEjectBtn.Tag, false, false);
  BtnImageLoad(picRepeatBtn.Tag, false, false);
  BtnImageLoad(picPlayListBtn.Tag, false, false);
  BtnImageLoad(picLyricsBtn.Tag, false, false);
  BtnImageLoad(picMuteBtn.Tag, false, false);
  BtnImageLoad(picEQBtn.Tag, false, false);
  BtnImageLoad(picResetBtn.Tag, false, false);
end;

function TMainForm.WriteTitle(title : string; GetWidthOnly : boolean) : integer;
var
  tmpBitmap : TBitmap;
begin
   if GetWidthOnly then
   begin
      tmpBitmap := TBitmap.Create;
      with tmpBitmap do
      begin
        Canvas.Font.Charset := ANSI_CHARSET;
        Canvas.Font.Name := 'Arial';
        Canvas.Font.Height := 12;
        result := Canvas.TextWidth(title);
        tmpBitmap.Free;
      end;
   end else
     with picTopText.Picture.Bitmap do
     begin
       Canvas.Brush.Style := bsClear;
       Canvas.Font.Charset := ANSI_CHARSET;
       Canvas.Font.Name := 'Arial';
       Canvas.Font.Height := 12;
       if m_bFocused then
          Canvas.Font.Color := RGB(0, 255, 0)
       else
          Canvas.Font.Color := RGB(64, 160, 0);
       Canvas.TextOut(4, 4, title);
       result := Canvas.TextWidth(title);
     end;
end;

procedure TMainForm.LoadSkin;
begin
  if FileExists(ExtractFilePath(ParamStr(0)) + 'Skin\Gen_.bmp') then
  begin
     try
       picMap.Picture.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Skin\Gen_.bmp');
     except
       picMap.Picture.Bitmap.LoadFromResourceName(HInstance, 'GEN');
     end;
  end else
     picMap.Picture.Bitmap.LoadFromResourceName(HInstance, 'GEN');

  PicMapDC := PicMap.Picture.Bitmap.Canvas.Handle;

  if FileExists(ExtractFilePath(ParamStr(0)) + 'Skin\CButtons_.bmp') then
  begin
     try
       picMap2.Picture.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Skin\CButtons_.bmp');
     except
       picMap2.Picture.Bitmap.LoadFromResourceName(HInstance, 'CBUTTONS');
     end;
  end else
     picMap2.Picture.Bitmap.LoadFromResourceName(HInstance, 'CBUTTONS');

  PicMapDC2 := PicMap2.Picture.Bitmap.Canvas.Handle;

  if FileExists(ExtractFilePath(ParamStr(0)) + 'Skin\Shufrep_.bmp') then
  begin
     try
       picMap3.Picture.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Skin\Shufrep_.bmp');
     except
       picMap3.Picture.Bitmap.LoadFromResourceName(HInstance, 'SHUFREP');
     end;
  end else
     picMap3.Picture.Bitmap.LoadFromResourceName(HInstance, 'SHUFREP');

  PicMapDC3 := PicMap3.Picture.Bitmap.Canvas.Handle;

  if FileExists(ExtractFilePath(ParamStr(0)) + 'Skin\SpectrumBack_.bmp') then
  begin
     try
       SpectrumImage.Picture.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Skin\SpectrumBack_.bmp');
     except
       SpectrumImage.Picture.Bitmap.LoadFromResourceName(HInstance, 'SPECTRUMBACK');
     end;
  end else
     SpectrumImage.Picture.Bitmap.LoadFromResourceName(HInstance, 'SPECTRUMBACK');

  if FileExists(ExtractFilePath(ParamStr(0)) + 'Skin\MainPanel_.bmp') then
  begin
     try
       PanelImage.Picture.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Skin\MainPanel_.bmp');
     except
       PanelImage.Picture.Bitmap.LoadFromResourceName(HInstance, 'MAINPANEL');
     end;
  end else
     PanelImage.Picture.Bitmap.LoadFromResourceName(HInstance, 'MAINPANEL');
end;

procedure TMainForm.picCloseMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   picClose_hDC: HDC;
begin
   if not (ssLeft in Shift) then
      exit;

   b_BtnPressed := true;
   PicMapDC := PicMap.Picture.Bitmap.Canvas.Handle;  // ** should be re-defined for each use
   picClose_hDC := picClose.Picture.Bitmap.Canvas.Handle;
   BitBlt(picClose_hDC, 0, 0, 9, 9, PicMapDC, 148, 42, SRCCOPY);
   picClose.Refresh;
end;

procedure TMainForm.picCloseClick(Sender: TObject);
begin
   NowClosing := true;
   if PlayListConfigForm.Visible then
      PlayListConfigForm.btnCloseClick(Self);

   Close;
end;

procedure TMainForm.picTopItemsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then
   begin
     ReleaseCapture;
     Perform(wm_syscommand, $f012, 0);
   end;
end;

procedure TMainForm.picMenuClick(Sender: TObject);
begin
   MainMenu.Popup(Self.Left + 5, Self.Top + 15);
end;

procedure TMainForm.ImageMouseEnter(Sender: TObject);
begin
  i_CursorOn := (Sender as TComponent).Tag;

  if b_BtnPressed then
     exit;

  case (Sender as TComponent).Tag of
    10..11 : SetCursor(CURSOR_SHIFT);
    12 : SetCursor(CURSOR_HAND);
    20 : ExpandImageLoad(true);
    21..26 : BtnImageLoad((Sender as TComponent).Tag, false, true);
  end;
end;

procedure TMainForm.ImageMouseLeave(Sender: TObject);
begin
  if (Sender as TComponent).Tag = i_CursorOn then
     i_CursorOn := 0;

 { if b_BtnPressed then
     b_BtnPressed := false; }

  case (Sender as TComponent).Tag of
    20 : ExpandImageLoad(false);
    21..26 : BtnImageLoad((Sender as TComponent).Tag, false, false);
    31..36 : BtnImageLoad((Sender as TComponent).Tag, false, false);
    99 : BtnImageLoad((Sender as TComponent).Tag, b_BtnPressed, false);
  end;

  if b_BtnPressed then
     b_BtnPressed := false;
end;

procedure TMainForm.SetImageMouseEvent(img: TImage);
begin
  img.OnMouseEnter := ImageMouseEnter;
  img.OnMouseLeave := ImageMouseLeave;

  img.Picture.Bitmap.Width := img.Width;
  img.Picture.Bitmap.Height := img.Height;
end;

procedure TMainForm.picButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if not (ssLeft in Shift) then
      exit;

   b_BtnPressed := true;
   if ((Sender as TComponent).Name = 'picEQBtn') or
      ((Sender as TComponent).Name = 'picResetBtn') then
      SetCapture(Panel3.Handle)
   else
      SetCapture(MainPanel.Handle);
  // BtnImageLoad(Btn_No : integer; b_Pressed, b_Highlighted : boolean);
   BtnImageLoad((Sender as TComponent).Tag, true, false);
end;

procedure TMainForm.picButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   b_BtnPressed := false;
   ReleaseCapture;
   if (Sender as TComponent).Tag = i_CursorOn then
   begin
      if (Sender as TComponent).Tag = 31 then
         b_Repeat := not b_Repeat
      else if (Sender as TComponent).Tag = 32 then
      begin
         b_PlayList := not b_PlayList;
         cbShowPlayListClick(Self);
      end
      else if (Sender as TComponent).Tag = 33 then
      begin
         b_ViewLyrics := not b_VIewLyrics;
         cbShowMBClick(Self);
      end
      else if (Sender as TComponent).Tag = 36 then
      begin
         b_ViewLyrics := not b_VIewLyrics;
         btnMuteClick(Self);
      end
      else if (Sender as TComponent).Tag = 34 then
      begin
         b_EqualizerOn := not b_EqualizerOn;
         cbEqualizerClick(Self);
      end
      else if (Sender as TComponent).Tag = 35 then
      begin
         btnResetEQClick(Self);
      end;

      BtnImageLoad((Sender as TComponent).Tag, false, true);
      case (Sender as TComponent).Tag of
         21 : btnPrevClick(Self);
         22 : btnPlayClick(Self);
         23 : btnPauseClick(Self);
         24 : btnStopClick(Self);
         25 : btnNextClick(Self);
         26 : btnOpenClick(Self);
      end;
   end else
      BtnImageLoad((Sender as TComponent).Tag, false, false);
end;

//------------------ End of Skin related functions/procedures -----------------------


procedure TMainForm.FormCreate(Sender: TObject);
var
   TitleRect : TRect;
begin
   ProgDir := ExtractFilePath(ParamStr(0));

   picMap := TImage.Create(Self);
   picMap2 := TImage.Create(Self);
   picMap3 := TImage.Create(Self);

   TmpBmp := TBitMap.Create;
   TmpBmp.width := TitleImage.Width;
   TmpBmp.Height := TitleImage.Height;
   with TitleRect do
   begin
      Left := 0;
      Top := 0;
      Right := TitleImage.Width;
      Bottom := TitleImage.Height;
   end;
   TitleBackBmp := TBitMap.Create;
   TitleBackBmp.width := TitleImage.Width;
   TitleBackBmp.Height := TitleImage.Height;
   TitleBackBmp.Canvas.CopyRect(TitleRect, TitleImage.Canvas, TitleRect);

   LoadSkin;
   SetImageMouseEvent(picBottomExpand);
   SetImageMouseEvent(picPrevBtn);
   SetImageMouseEvent(picPlayBtn);
   SetImageMouseEvent(picPauseBtn);
   SetImageMouseEvent(picStopBtn);
   SetImageMouseEvent(picNextBtn);
   SetImageMouseEvent(picEjectBtn);
   SetImageMouseEvent(picRepeatBtn);
   SetImageMouseEvent(picPlayListBtn);
   SetImageMouseEvent(picLyricsBtn);
   SetImageMouseEvent(picMuteBtn);
   SetImageMouseEvent(picEQBtn);
   SetImageMouseEvent(picResetBtn);

   SetImageMouseEvent(picRulerL);
   SetImageMouseEvent(picRulerR);
   SetImageMouseEvent(picThumb);

   SetImageMouseEvent(picClose);

   CURSOR_HAND := LoadCursor(hInstance, 'HAND_CURSOR');
   CURSOR_SHIFT := LoadCursor(hInstance, 'LRSHIFT_CURSOR');
end;

{ procedure TMainForm.btnFileInfoClick(Sender: TObject);
var
   s1, s2 : string;
begin
   if not BassPlayer1.PlayerReady then
   begin
      Application.MessageBox('Cannot open file (BassPlayer is not ready)',
                 'Confirm', MB_OK or MB_ICONINFORMATION);
      exit;
   end;

   OpenDialog1.FileName := '';
   s1 := BassPlayer1.NativeFileExts;
   s2 := BassPlayer1.PluginFileExts;
   OpenDialog1.Filter := 'All supported files |' + s1 + s2 + '|' +
                         'BASS native files (' + s1 + ')|' + s1 + '|' +
                         'Plug-in supported files (' + s2 + ')|' + s2 + '|';

 // Pre-select the opened stream file.
   if BassPlayer1.StreamName <> '' then
      if (not BassPlayer1.IsNetStream) and (not BassPlayer1.IsNetRadio) then
      begin
         OpenDialog1.InitialDir := ExtractFileDir(BassPlayer1.StreamName);
         OpenDialog1.FileName := ExtractFileName(BassPlayer1.StreamName);
      end;

   if OpenDialog1.Execute then
      if not BassPlayer1.FileInfoBox(OpenDialog1.FileName) then
         Application.MessageBox('Unsupported file type.', 'Information', MB_OK + MB_ICONINFORMATION);

end; }

procedure TMainForm.ProcessPluginRequest(Sender: TObject; GenParam : DWORD);
begin
   case TPluginRequest(GenParam) of
      REQ_VOLUMEUP : begin
                           if VolumeKnob.Position < VolumeKnob.Max then
                              if (VolumeKnob.Max - VolumeKnob.Position) > 4 then
                                 VolumeKnob.Position := VolumeKnob.Position + 4
                              else
                                 VolumeKnob.Position := VolumeKnob.Max;
                        end;
      REQ_VOLUMEDOWN : begin
                           if VolumeKnob.Position > 4 then
                              VolumeKnob.Position := VolumeKnob.Position - 4
                           else
                              VolumeKnob.Position := 0;
                        end;
      REQ_FFWD5S :   begin
                         //  if BassPlayer1.Mode = plmPlaying then
                              if (BassPlayer1.Position + 5000) < PlayLength then
                                  BassPlayer1.Position := BassPlayer1.Position + 5000;

                        end;
      REQ_REW5S :    begin
                         //  if BassPlayer1.Mode = plmPlaying then
                              if (BassPlayer1.Position - 5000) > 0 then
                                  BassPlayer1.Position := BassPlayer1.Position - 5000;

                        end;

      REQ_PREV :  btnPrevClick(Self);
      REQ_NEXT :  btnNextClick(Self);
      REQ_PLAY :  btnPlayClick(self);
      REQ_PAUSE : btnPauseClick(Self);
      REQ_STOP :  btnStopClick(self);
   end;
end;


procedure TMainForm.btnAddonSetupClick(Sender: TObject);
begin
   AddonConfigForm.ShowModal;
end;

procedure TMainForm.GetVisPluginInfo(Sender: TObject; VisWindowAttr : TVisPluginInfo);
begin
   if NowClosing then
      exit;

   CurVisWindowAttr := VisWindowAttr;
   VisControlForm.SetVisWindowAttr(CurVisWindowAttr);

   if CurVisWindowAttr.VisHandle <> 0 then
   begin
      if CurVisWindowAttr.StartType then
         StartedInType := CurVisWindowAttr.VisType;
    //  ShowBackground;
   end;
end;



// Following is test code which clears vis window.
// One of vis plug-in, vis_milk.dll shows a wrinkled image at showing up.
// Following code can be used to clean the plug-in's display surface for such plug-ins.
{procedure TMainForm.Button1Click(Sender: TObject);
var
   MyDC : HDC;
   r, r2 : TRect;
//   MyBitmap : HBITMAP;
   MyBrush : HBRUSH;
   OldObj : HGDIOBJ;
begin
   if ChildHandle <> 0 then
   begin
      MyDC := GetDC(ParentHandle);
      windows.GetClientRect(ParentHandle, r);
    //  MyBitmap := CreateCompatibleBitmap(MyDC, r.Right - r.Left, r.Bottom - r.Top);
      MyBrush := CreateSolidBrush(0);
    //  OldObj := SelectObject(MyDC, MyBitmap);
      OldObj := SelectObject(MyDC, MyBrush);

      r2.Left := CurVisWindowAttr.ClientPosX;
      r2.Top := CurVisWindowAttr.ClientPosY;
      r2.Right := r2.Left + r.Right - r.Left - CurVisWindowAttr.L_R_Margin;
      r2.Bottom := r2.Top + r.Bottom - r.Top - CurVisWindowAttr.T_B_Margin;
      windows.FillRect(MyDC, r2, MyBrush);

      SelectObject(MyDC, OldObj);
    //  DeleteObject(MyBitmap);
      DeleteObject(MyBrush);
      ReleaseDC(ParentHandle, MyDC);
   end;
end; }



procedure TMainForm.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
 // The vis_avs.dll plug-in alters form size at display mode change (full screen mode -> windowed mode).
 // This code is to prevent such unintentional change of form size.
 //  Resize:= false;
end;



procedure TMainForm.btnPrevClick(Sender: TObject);   // * New at Ver 2.00
var
   WasPlaying : boolean;
begin
   if IsOpeningStream then
      exit;

   if PlayListConfigForm.NumEntry <= 1 then
      exit;

   if (PlayListConfigForm.SelectedEntry = 0) and (not b_Repeat) then
      exit;

   WasPlaying := false;
   if (CurMode = plmPlaying) or (CurMode = plmPaused) then
   begin
      if (CurMode = plmPlaying) then
         WasPlaying := true;

      btnStopClick(Self);
   end;

   if PlayListConfigForm.SelectedEntry = 0 then
      PlayListConfigForm.SelectEntry(PlayListConfigForm.NumEntry - 1)
   else
      PlayListConfigForm.SelectEntry(PlayListConfigForm.SelectedEntry - 1);
   if WasPlaying then
      OpenStream(PlayListConfigForm.SelectedFile, WasPlaying);

end;

procedure TMainForm.btnNextClick(Sender: TObject);   // * New at Ver 2.00
var
   WasPlaying : boolean;
begin
   if IsOpeningStream then
      exit;

   if PlayListConfigForm.NumEntry <= 1 then
      exit;

   if PlayListConfigForm.SelectedEntry = (PlayListConfigForm.NumEntry - 1) then
      if (PlayListConfigForm.NumEntry = 1) or (not b_Repeat) then
         exit;

   WasPlaying := false;
   if (CurMode = plmPlaying) or (CurMode = plmPaused) then
   begin
      if (CurMode = plmPlaying) then
         WasPlaying := true;

      btnStopClick(Self);
   end;

   if PlayListConfigForm.SelectedEntry < (PlayListConfigForm.NumEntry - 1) then
      PlayListConfigForm.SelectEntry(PlayListConfigForm.SelectedEntry + 1)
   else
      PlayListConfigForm.SelectEntry(0);
   if WasPlaying then
      OpenStream(PlayListConfigForm.SelectedFile, WasPlaying);

end;

procedure TMainForm.PlayNewSong(ListIndex : integer);  // * New at Ver 2.00
begin
   if (CurMode = plmPlaying) or (CurMode = plmPaused) then
       btnStopClick(Self);

   OpenStream(PlayListConfigForm.SelectedFile, true);
end;

procedure TMainForm.UpdatePlayList(Sender: TObject; GenParam : DWORD); // * New at Ver 2.00
var
   AddedFile : string;
begin
   if GenParam = 0 then
   begin
      if (CurMode = plmPlaying) or (CurMode = plmPaused) then
         BassPlayer1.Stop;

      PlayListConfigForm.ClearPlayList;
      ChangingPlayList := true;
   end else
   begin
      AddedFile := string(pAnsiChar(GenParam));
      PlayListConfigForm.AddTo_PlayList(AddedFile, '', '', false);
      if PlayListConfigForm.NumEntry = 1 then
         if ChangingPlayList then
         begin
            OpenStream(AddedFile, true);
            ChangingPlayList := false;
         end;
   end;
end;

procedure TMainForm.cbShowPlayListClick(Sender: TObject);  // * New at Ver 2.00
begin
   if b_PlayList then
    //  PlayListConfigForm.Show
      PlayListConfigForm.ShowAtLoc(Self.Left - Self.Width,
                                   Self.Top - (ExpandedHeight - Self.Height),
                                   Self.Width, ExpandedHeight, true)
   else
      PlayListConfigForm.Close;
end;

procedure TMainForm.cbDownMixClick(Sender: TObject);
begin
  { if cbDownMix.Checked then
      BassPlayer1.DownMixToStereo := true
   else
      BassPlayer1.DownMixToStereo := false; }
end;


procedure TMainForm.MBWindowShow(Sender: TObject; SW_FLAG : DWORD);  // * New at Ver 2.00
begin
   if NowClosing then
      exit;

   if SW_FLAG = SW_SHOW then
      b_ViewLyrics := true
   else
      b_ViewLyrics := false;

   BtnImageLoad(picLyricsBtn.Tag, false, false);
end;

procedure TMainForm.GenWindowShow(Sender: TObject; GenInfoP : DWORD);  // * Changed at Ver 2.00.3
var
   GenDrawerInfoP : ^TGenDrawerInfo;
   DLLName : string;
begin
   if NowClosing then
      exit;

   GenDrawerInfoP := pointer(GenInfoP);
   if GenDrawerInfoP^.SW_FLAG = SW_SHOW then   // Show
   begin
      GenWinHandle[GenDrawerInfoP^.FormNo] := GenDrawerInfoP^.WinHandle;
      DLLName := uppercase(ExtractFileName(GenDrawerInfoP^.DLLPath));
      if DLLName = 'GEN_LEOSLYRICS.DLL' then
         begin
            ShownLeosLyrics := true;
            LeosHandleList.hGenWindow := GenDrawerInfoP^.WinHandle;
            GetLeosWinHandle(LeosHandleList);
         end;
   end else   // Close
   begin
      DLLName := uppercase(ExtractFileName(GenDrawerInfoP^.DLLPath));
      if DLLName = 'GEN_LEOSLYRICS.DLL' then
         ShownLeosLyrics := false;
   end;
end;


// procedure ForceRefreshLyrics lets Gen_LeosLyrics.dll and Mini Browser refresh their lyrics
//  display at application level.
// note) The Gen_LeosLyrics.dll does not refresh their lyrics display via Winamp IPC message.
//        (There should be hidden mechanism for that I do not know.)
//       I couldn't find out the reason. (Please let me know if you have any idea.)

procedure TMainForm.ForceRefreshLyrics;
var
 //  buf : array[0..255] of char;
 //  URL : string;
   Title_, Artist_ : string;

 procedure ParseMetaData(MetaData : string; var Artist, Title : string);
 var
   p1, p2 : integer;
   tmpStr, s1, s2 : string;

 begin
   Artist := '';
   Title := '';
   tmpStr := trim(MetaData);

   if tmpStr = '' then
      exit;

 //  Seperate string : text1 - text2  => s1 = text1, s2 = text2
   p1 := pos(' - ', tmpStr);
   if p1 > 0 then
   begin
      s1 := copy(tmpStr, 1, p1 - 1);
      s2 := copy(tmpStr, p1 + 3, length(tmpStr) - p1 - 2);
   end else
   begin
      s1 := '';
      s2 := tmpStr;
   end;

   if s1 <> '' then
   begin
 // Remove the string between '[' and ']'  (ex. : ...[text1]text2  => text2 )
     p1 := pos('[', s1);
     p2 := pos(']', s1);
     if (p1 > 0) and (p2 > p1) then
        s1 := copy(s1, p2 + 1, length(s1) - p2);
   end;

   Artist := trim(s1);
   Title := trim(s2);
 end;

begin
   if IsNetRadio then
   begin
   // ** process metadata gotton from net radio **
   // BassPlayer1.StreamInfo.Title has Meta data from net radio
      ParseMetaData(BassPlayer1.StreamInfo.Title, Artist_, Title_);
    end else begin
      Title_ := BassPlayer1.StreamInfo.Title;
      Artist_ := BassPlayer1.StreamInfo.Artist;
   end;

   if b_ViewLyrics then
      BassPlayer1.LetMBNavigate2(Artist_, Title_, UseCixURL);

   if ShownLeosLyrics then
   begin
      if LeosHandleList.hDisplay = 0 then
         GetLeosWinHandle(LeosHandleList)
      else
         SendTextToLeos(LeosHandleList, Artist_, Title_);
   end;
   
end;

procedure TMainForm.cbShowMBClick(Sender: TObject);
var
   r : TRect;
begin
   if MBWinHandle = 0 then
      exit;

   if b_ViewLyrics then
   begin
      if not ShownMB then
      begin
         if Self.Height > 370  then
            BassPlayer1.ShowMBForm(Self.Left + Self.Width, Self.Top, Self.Width, Self.Height)
         else
            BassPlayer1.ShowMBForm(Self.Left + Self.Width, Self.Top - (370 - Self.Height), Self.Width, 370);

       //  ForceRefreshLyrics;
         ShownMB := true;
      end else
      begin
         GetWindowRect(MBWinHandle, r);
         SetWindowPos(MBWinHandle, 0,
                      Self.Left + Self.Width, // horizontal position,
                      Self.Top - (r.Bottom - r.Top - Self.Height),
                      Self.Width,      // width
                      Self.Height, // height
                      SWP_NOSIZE + SWP_NOZORDER);
         ShowWindow(MBWinHandle, SW_SHOW);
      end;
   end else
      ShowWindow(MBWinHandle, SW_HIDE);
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
 // This routine should be executed only once at starting program.
   Self.OnActivate := nil;

 // Mini Browser and Drawer Window should be ready before running Winamp GPP.
   if (BassPlayer1.MBWinHandle = 0) then
      exit;

   MBWinHandle := BassPlayer1.MBWinHandle;

  // Enable Magnetic effect
   BassPlayer1.MagneticWindowAdd(Self.Handle, 0, MagneticWndProc);

 //  b_ViewLyrics := true;

 // Show Mini Brower at the right side of Main form, with the same height of Main Form
   if b_ViewLyrics then
   begin
      BassPlayer1.ShowMBForm(Self.Left + Self.Width, Self.Top, 380, Self.Height);
      ShownMB := true;
   end;

  // If you want to start all the Winamp GPPs at start up, use following code.
 {  i := BassPlayer1.RunAllGPPModules;
   if i > 0 then
   begin
      StatusViewForm.InfoMemo.Lines.Add('  ');
      StatusViewForm.InfoMemo.Lines.Add(inttoStr(i) + ' Winamp GPP loaded');
      for i := 1 to BassPlayer1.NumberLoadedGPP do
      begin
         GPPInfo := BassPlayer1.GetLoadedGPPInfo(i, false);
         StatusViewForm.InfoMemo.Lines.Add(string(GPPInfo.Description));
      end;
   end; }

 // We need not to load gen_kmpcix.dll or gen_lyrics.dll to display lyrics.
 // This program lets Mini Browser to search a specified URL directly at getting
 //  new title information.
 // The URLs to get the lyrics are obtained with the functions in unit Util_Kmpcix.

   if GetOEMCP = 949 then   // OEM code page is Korean Language ?
      UseCixURL := true
   else
      if FileExists(GetProgDir + 'Plugins\Gen_LeosLyrics.dll') then
         BassPlayer1.RunGPPModule(GetProgDir + 'Plugins\Gen_LeosLyrics.dll');

   if UseCixURL then
      BassPlayer1.LetMBNavigate(URL_CixHome)
   else
      BassPlayer1.LetMBNavigate(URL_LyricsHome);

 // Replace menu Text with local language
   menuOpen.Caption := msgs[31];
   menuOpenURL.Caption := msgs[32];
   menuAddonSetup.Caption := msgs[33];
   menuPluginSetup.Caption := msgs[34];
   menuDSPCtrl.Caption := msgs[35];
   menuVisCtrl.Caption := msgs[36];
   menuViewStatus.Caption := msgs[37];
   menuExpandView.Caption := msgs[38];
   menuClose.Caption := msgs[39];
   menuHideWindow.Caption := msgs[40];
   menuAbout.Caption := msgs[41];

 // Load default playlist file  
   PlayListConfigForm.AddPlayList(ExtractFilePath(ParamStr(0))+ 'Playlist\Default.m3u');
   if PlayListConfigForm.NumEntry > 0 then
   begin
      OpenStream(PlayListConfigForm.SelectEntry(0), false);
   //   BassPlayer1.Play;
   end;

   PassedActivation := true;
end;


//------------ Message handlers for Magnetic effect of MainForm ----------------
// * Added at Ver 2.00.3

procedure TMainForm.WMEnterSizeMove(var Msg: TMessage);
var
   bHandled: Boolean;
begin
   inherited;

   if Assigned(MagneticWndProc) then
      MagneticWndProc(Self.Handle, WM_ENTERSIZEMOVE, Msg, bHandled);
end;

procedure TMainForm.WMSizing(var Msg: TMessage);
var
   bHandled: Boolean;
begin
   if not Assigned(MagneticWndProc) then
      inherited
   else
      if MagneticWndProc(Self.Handle, WM_SIZING, Msg, bHandled) then
         if not bHandled then
            inherited;
end;

procedure TMainForm.WMMoving(var Msg: TMessage);
var
   bHandled: Boolean;
begin
   if not Assigned(MagneticWndProc) then
      inherited
   else
      if MagneticWndProc(Self.Handle, WM_MOVING, Msg, bHandled) then
         if not bHandled then
            inherited;
end;

procedure TMainForm.WMExitSizeMove(var Msg: TMessage);
var
   bHandled: Boolean;
begin
   inherited;

   if Assigned(MagneticWndProc) then
      MagneticWndProc(Self.Handle, WM_EXITSIZEMOVE, Msg, bHandled);
end;

procedure TMainForm.WMSysCommand(var Msg: TMessage);
var
   bHandled: Boolean;
begin
   inherited;

   if Assigned(MagneticWndProc) then
      MagneticWndProc(Self.Handle, WM_SYSCOMMAND, Msg, bHandled);
end;

procedure TMainForm.WMCommand(var Msg: TMessage);
var
   bHandled: Boolean;
begin
   inherited;

   if Assigned(MagneticWndProc) then
      MagneticWndProc(Self.Handle, WM_COMMAND, Msg, bHandled);
end;

//----------- End of message handlers for Magnetic effect of MainForm ----------


procedure TMainForm.menuExpandViewClick(Sender: TObject);
begin
   ExpandHighlighted := false;
   if menuExpandView.Checked then
   begin
      lbl_Writer.Visible := false;
      menuExpandView.Checked := false;
      Self.Height := BasicHeight;
      ExpandImageLoad(ExpandHighlighted);
      ResizeElement(Self);
   end else
   begin
      lbl_Writer.Visible := true;
      menuExpandView.Checked := true;
      Self.Height := ExpandedHeight;
      ExpandImageLoad(ExpandHighlighted);
      ResizeElement(Self);
   end;
end;

procedure TMainForm.menuOpenURLClick(Sender: TObject);
var
   s : string;
begin
   if IsOpeningStream then
      exit;

   URLInputForm.ShowModal;

   if URLInputForm.RequestToOpen then
   begin
     s := URLInputForm.ComboBox1.Text;
     if s <> '' then
     begin
       OpenByNetButton := true;
       OpenStream(s, true);
     end;
   end;

end;

procedure TMainForm.btnResetEQClick(Sender: TObject);
var
   i : integer;
begin
   Slider1.Value  := 15;
   Slider2.Value  := 15;
   Slider3.Value  := 15;
   Slider4.Value  := 15;
   Slider5.Value  := 15;
   Slider6.Value  := 15;
   Slider7.Value  := 15;
   Slider8.Value  := 15;
   Slider9.Value  := 15;
   Slider10.Value := 15;

   if BassPlayer1.DX8EffectReady then
     for i := 0 to 9 do
     begin
       EQGains[i] := 0;
       BassPlayer1.SetAEQGain(i, EQGains[i]);
     end;
end;


procedure TMainForm.btnMuteClick(Sender: TObject);
begin
   b_Mute := (not b_Mute);
   BassPlayer1.SetMuteState(b_Mute, 500);
   if b_Mute then
      b_MuteHighlighted := true
   else
      b_MuteHighlighted := false;
end;

procedure TMainForm.TitleTextOut(FlowStep : integer);
var
   BmpHalf : integer;
   TitleTextWidth : integer;
   TitleRect : TRect;
begin
   with TitleRect do
   begin
      Left := 0;
      Top := 0;
      Right := TitleImage.Width;
      Bottom := TitleImage.Height;
   end;

 // Copy original background image to TmpBmp
   BitBlt(TmpBmp.Canvas.Handle,
          TitleRect.Left,
          TitleRect.Top,
          TmpBmp.Width,
          TmpBmp.Height,
          TitleBackBmp.Canvas.Handle,
          TitleRect.Left,
          TitleRect.Top,
          SRCCOPY);

  { if lbl_Title = '' then
   begin
      TitleImage.Canvas.CopyRect(TitleRect, TmpBmp.Canvas, TitleRect);
      TitleImage.Refresh;
     // TitleShown := false;
      exit;
   end; }

   BMPHalf := TitleImage.Width div 2;
   with TmpBmp.Canvas do
   begin
    //  TitleShown := true;
      Brush.Style := bsClear;
      Font.Name := 'Areal';
      if MainForm.PixelsPerInch > 96 then
         Font.Size := 8
      else
         Font.Size := 9;
      Font.Color := TColor(RGB(160, 160, 64));   // clYellow;
      TitleTextWidth := TextWidth(lbl_Title { + '<'});
      if TitleTextWidth < BMPHalf then
         TitleTextWidth := BMPHalf;
      TextOut(-TitlePosX{ + TextWidth('<')}, 1, lbl_Title);

    // Keep the distance between of title texts (= a half of the with of TitleImage)
      if (TitleTextWidth - TitlePosX) < BMPHalf then
         TextOut(TitleTextWidth + BMPHalf - TitlePosX{ + TextWidth('<')}, 1,
                 lbl_Title)
   end;

 // Copy the image of TmpBmp to TitleImage(=displaying surface)
   BitBlt(TitleImage.Picture.Bitmap.Canvas.Handle,
          TitleRect.Left,
          TitleRect.Top,
          TmpBmp.Width,
          TmpBmp.Height,
          TmpBmp.Canvas.Handle,
          TitleRect.Left,
          TitleRect.Top,
          SRCCOPY);

   TitleImage.Refresh;

 // Set next put position (shift left side)
   TitlePosX := TitlePosX + FlowStep;
   if TitlePosX >= TitleTextWidth + BMPHalf then
      TitlePosX := TitlePosX - (TitleTextWidth + BMPHalf);
end;


procedure TMainForm.menuViewStatusClick(Sender: TObject);
begin
   StatusViewForm.Show;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssAlt in Shift) then
     if Key <> 18 then
        case Key of
          65 : menuAboutClick(Self);          // 'A'
          66 : btnAddonSetupClick(Self);      // 'B'
          68 : btnShowDSPCtrlFormClick(Self); // 'D'
          69 : menuExpandViewClick(Self);     // 'E'
          72 : menuHideWindowClick(Self);     // 'H'
          73 : btnPluginSetupClick(Self);     // 'I'
          79 : btnOpenClick(Self);            // 'O'
          83 : menuViewStatusClick(Self);     // 'S'
          85 : menuOpenURLClick(Self);        // 'U'
          86 : btnShowVisCtrlFormClick(Self); // 'V'
          88 : btnCloseClick(Self);           // 'X'
        end;
end;

// This procedure is substituted with the event handler of Mouse enter.
// Mouse cursor is setuped in the event handler. so no additional chckeking is needed.
{ procedure TMainForm.CheckCursor;
var
   m_point : TPoint;
   ThumbRect : TRect;
   CurCursor : HCURSOR;
   mouseIn_picThumb : boolean;
begin
   CurCursor := GetCursor;

   if not picThumb.Visible then
   begin
      if CurCursor = CURSOR_HAND then
         SetCursor(OrgCursor);
      exit;
   end;

 // ThumbRect defines the boundary coordinate of picThumb
   ThumbRect.Left := Self.Left + MainPanel.Left + picThumb.Left;
   ThumbRect.Right := ThumbRect.Left + picThumb.Width - 1;
   ThumbRect.Top := Self.Top + MainPanel.Top + picThumb.Top;
   ThumbRect.Bottom := ThumbRect.Top + picThumb.Height - 1;
   GetCursorPos(m_point);
   if (m_point.x < ThumbRect.Left) or (m_point.x > ThumbRect.Right) or
      (m_point.y < ThumbRect.Top) or (m_point.y > ThumbRect.Bottom) then
      mouseIn_picThumb := false
   else
      mouseIn_picThumb := true;

   if mouseIn_picThumb then
   begin
      if CurCursor <> CURSOR_HAND then
      begin
         SetCursor(CURSOR_HAND);
         OrgCursor := CurCursor;
      end;
   end else
      if CurCursor = CURSOR_HAND then
         SetCursor(OrgCursor);
end;  }

procedure TMainForm.picThumbMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   m_point : TPoint;
   CurCursor : HCURSOR;
begin
   if not (ssLeft in Shift) then
      exit;

   CurCursor := GetCursor;
   if CurCursor <> CURSOR_HAND then
   begin
      SetCursor(CURSOR_HAND);
      OrgCursor := CurCursor;
   end;
   NowTracking := true;

   SetCapture(MainPanel.Handle);
   GetCursorPos(m_point);
   ThumbStartX := m_point.X;

end;

procedure TMainForm.picThumbMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   SongPos : DWORD;
   PosSlider_Value : integer;
begin
   if not NowTracking then
      exit;

   if BassPlayer1.Seekable then
   begin
      PosSlider_Value :=
        round((picThumb.Left - picRulerR.Left) / (picRulerR.Width - picThumb.Width) * PosSlider_MaxValue);
      SongPos := Trunc(DWORD(PosSlider_Value) * PlayLength / DWORD(PosSlider_MaxValue));
      BassPlayer1.Position := SongPos;
   end;

   ReleaseCapture;
   NowTracking := false;
end;

procedure TMainForm.picThumbMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
   CurCursor : HCURSOR;
   diffX, NewPosX : integer;
   m_point : TPoint;
begin
   CurCursor := GetCursor;
   if CurCursor <> CURSOR_HAND then
   begin
      SetCursor(CURSOR_HAND);
      OrgCursor := CurCursor;
   end;

   if not NowTracking then
      exit;

   GetCursorPos(m_point);
   diffX := (m_point.X) - ThumbStartX;
   if diffX <= 0 then
   begin
      if picThumb.Left > picRulerR.Left then
      begin
         NewPosX := ThumbStartX + diffX - Self.Left - MainPanel.Left;
         if NewPosX < picRulerR.Left then
            NewPosX := picRulerR.Left;

         picThumb.Left := NewPosX;
         picRulerL.Width := picThumb.Left - picRulerR.Left;
      end;
   end else
      if picThumb.Left < picRulerR.Left + picRulerR.Width - picThumb.Width then
      begin
         NewPosX := ThumbStartX + diffX - Self.Left - MainPanel.Left;

         if NewPosX > picRulerR.Left + picRulerR.Width - picThumb.Width then
            NewPosX := picRulerR.Left + picRulerR.Width - picThumb.Width;

          picThumb.Left := NewPosX;
          picRulerL.Width := picThumb.Left - picRulerR.Left;
      end;
end;

procedure TMainForm.picRulerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   SongPos : DWORD;
   PosSlider_Value : integer;
begin
   if not (ssLeft in Shift) then
      exit;

   SetCursor(CURSOR_SHIFT);
      
   if not picThumb.Visible then
      exit;

   if BassPlayer1.Seekable then
   begin
      PosSlider_Value :=
        round(X / (picRulerR.Width - picThumb.Width) * PosSlider_MaxValue);
      if PosSlider_Value >= PosSlider_MaxValue - 3 then
         PosSlider_Value := PosSlider_MaxValue - 3;
      SongPos := Trunc(DWORD(PosSlider_Value) * PlayLength / DWORD(PosSlider_MaxValue));
      BassPlayer1.Position := SongPos;
   end;
end;

procedure TMainForm.menuHideWindowClick(Sender: TObject);
begin
   PostMessage(Self.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
end;

procedure TMainForm.picRulerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
   SetCursor(CURSOR_SHIFT);
end;

procedure TMainForm.menuAboutClick(Sender: TObject);
begin
   AboutForm.StartABout;
end;

procedure TMainForm.PanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   if not (ssRight in Shift) then
      exit;

   MainMenu.Popup(Self.Left + X, Self.Top + Y);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
   BassPlayer1.Free;
end;

end.
