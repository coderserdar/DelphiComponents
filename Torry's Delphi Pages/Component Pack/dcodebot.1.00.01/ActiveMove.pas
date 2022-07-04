unit ActiveMovie;

{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 

interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  AMovieMajorVersion = 2;
  AMovieMinorVersion = 0;

  LIBID_AMovie: TGUID = '{05589FA0-C356-11CE-BF01-00AA0055595A}';

  DIID_DActiveMovieEvents: TGUID = '{05589FA3-C356-11CE-BF01-00AA0055595A}';
  DIID_DActiveMovieEvents2: TGUID = '{B6CD6553-E9CB-11D0-821F-00A0C91F9CA0}';
  IID_IActiveMovie: TGUID = '{05589FA2-C356-11CE-BF01-00AA0055595A}';
  IID_IActiveMovie2: TGUID = '{B6CD6554-E9CB-11D0-821F-00A0C91F9CA0}';
  IID_IActiveMovie3: TGUID = '{265EC140-AE62-11D1-8500-00A0C91F9CA0}';
  CLASS_ActiveMovie: TGUID = '{05589FA1-C356-11CE-BF01-00AA0055595A}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum ReadyStateConstants
type
  ReadyStateConstants = TOleEnum;
const
  amvUninitialized = $00000000;
  amvLoading = $00000001;
  amvInteractive = $00000003;
  amvComplete = $00000004;

// Constants for enum StateConstants
type
  StateConstants = TOleEnum;
const
  amvNotLoaded = $FFFFFFFF;
  amvStopped = $00000000;
  amvPaused = $00000001;
  amvRunning = $00000002;

// Constants for enum DisplayModeConstants
type
  DisplayModeConstants = TOleEnum;
const
  amvTime = $00000000;
  amvFrames = $00000001;

// Constants for enum WindowSizeConstants
type
  WindowSizeConstants = TOleEnum;
const
  amvOriginalSize = $00000000;
  amvDoubleOriginalSize = $00000001;
  amvOneSixteenthScreen = $00000002;
  amvOneFourthScreen = $00000003;
  amvOneHalfScreen = $00000004;

// Constants for enum AppearanceConstants
type
  AppearanceConstants = TOleEnum;
const
  amvFlat = $00000000;
  amv3D = $00000001;

// Constants for enum BorderStyleConstants
type
  BorderStyleConstants = TOleEnum;
const
  amvNone = $00000000;
  amvFixedSingle = $00000001;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  DActiveMovieEvents = dispinterface;
  DActiveMovieEvents2 = dispinterface;
  IActiveMovie = interface;
  IActiveMovieDisp = dispinterface;
  IActiveMovie2 = interface;
  IActiveMovie2Disp = dispinterface;
  IActiveMovie3 = interface;
  IActiveMovie3Disp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  ActiveMovie = IActiveMovie3;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PSmallint1 = ^Smallint; {*}
  PWordBool1 = ^WordBool; {*}


// *********************************************************************//
// DispIntf:  DActiveMovieEvents
// Flags:     (4112) Hidden Dispatchable
// GUID:      {05589FA3-C356-11CE-BF01-00AA0055595A}
// *********************************************************************//
  DActiveMovieEvents = dispinterface
    ['{05589FA3-C356-11CE-BF01-00AA0055595A}']
    procedure StateChange(oldState: Integer; newState: Integer); dispid 1;
    procedure PositionChange(oldPosition: Double; newPosition: Double); dispid 2;
    procedure Timer; dispid 3;
    procedure OpenComplete; dispid 50;
    procedure Click; dispid -600;
    procedure DblClick; dispid -601;
    procedure KeyDown(var KeyCode: Smallint; Shift: Smallint); dispid -602;
    procedure KeyUp(var KeyCode: Smallint; Shift: Smallint); dispid -604;
    procedure KeyPress(var KeyAscii: Smallint); dispid -603;
    procedure MouseDown(Button: Smallint; Shift: Smallint; x: OLE_XPOS_PIXELS; y: OLE_YPOS_PIXELS); dispid -605;
    procedure MouseMove(Button: Smallint; Shift: Smallint; x: OLE_XPOS_PIXELS; y: OLE_YPOS_PIXELS); dispid -606;
    procedure MouseUp(Button: Smallint; Shift: Smallint; x: OLE_XPOS_PIXELS; y: OLE_YPOS_PIXELS); dispid -607;
    procedure Error(SCode: Smallint; const Description: WideString; const Source: WideString; 
                    var CancelDisplay: WordBool); dispid 999;
  end;

// *********************************************************************//
// DispIntf:  DActiveMovieEvents2
// Flags:     (4112) Hidden Dispatchable
// GUID:      {B6CD6553-E9CB-11D0-821F-00A0C91F9CA0}
// *********************************************************************//
  DActiveMovieEvents2 = dispinterface
    ['{B6CD6553-E9CB-11D0-821F-00A0C91F9CA0}']
    procedure StateChange(oldState: Integer; newState: Integer); dispid 1;
    procedure PositionChange(oldPosition: Double; newPosition: Double); dispid 2;
    procedure Timer; dispid 3;
    procedure OpenComplete; dispid 50;
    procedure Click; dispid -600;
    procedure DblClick; dispid -601;
    procedure KeyDown(var KeyCode: Smallint; Shift: Smallint); dispid -602;
    procedure KeyUp(var KeyCode: Smallint; Shift: Smallint); dispid -604;
    procedure KeyPress(var KeyAscii: Smallint); dispid -603;
    procedure MouseDown(Button: Smallint; Shift: Smallint; x: OLE_XPOS_PIXELS; y: OLE_YPOS_PIXELS); dispid -605;
    procedure MouseMove(Button: Smallint; Shift: Smallint; x: OLE_XPOS_PIXELS; y: OLE_YPOS_PIXELS); dispid -606;
    procedure MouseUp(Button: Smallint; Shift: Smallint; x: OLE_XPOS_PIXELS; y: OLE_YPOS_PIXELS); dispid -607;
    procedure Error(SCode: Smallint; const Description: WideString; const Source: WideString; 
                    var CancelDisplay: WordBool); dispid 999;
    procedure DisplayModeChange; dispid 51;
    procedure ReadyStateChange(ReadyState: ReadyStateConstants); dispid -609;
    procedure ScriptCommand(const bstrType: WideString; const bstrText: WideString); dispid 52;
  end;

// *********************************************************************//
// Interface: IActiveMovie
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {05589FA2-C356-11CE-BF01-00AA0055595A}
// *********************************************************************//
  IActiveMovie = interface(IDispatch)
    ['{05589FA2-C356-11CE-BF01-00AA0055595A}']
    procedure AboutBox; safecall;
    procedure Run; safecall;
    procedure Pause; safecall;
    procedure Stop; safecall;
    function  Get_ImageSourceWidth: Integer; safecall;
    function  Get_ImageSourceHeight: Integer; safecall;
    function  Get_Author: WideString; safecall;
    function  Get_Title: WideString; safecall;
    function  Get_Copyright: WideString; safecall;
    function  Get_Description: WideString; safecall;
    function  Get_Rating: WideString; safecall;
    function  Get_FileName: WideString; safecall;
    procedure Set_FileName(const pbstrFileName: WideString); safecall;
    function  Get_Duration: Double; safecall;
    function  Get_CurrentPosition: Double; safecall;
    procedure Set_CurrentPosition(pValue: Double); safecall;
    function  Get_PlayCount: Integer; safecall;
    procedure Set_PlayCount(pPlayCount: Integer); safecall;
    function  Get_SelectionStart: Double; safecall;
    procedure Set_SelectionStart(pValue: Double); safecall;
    function  Get_SelectionEnd: Double; safecall;
    procedure Set_SelectionEnd(pValue: Double); safecall;
    function  Get_CurrentState: StateConstants; safecall;
    function  Get_Rate: Double; safecall;
    procedure Set_Rate(pValue: Double); safecall;
    function  Get_Volume: Integer; safecall;
    procedure Set_Volume(pValue: Integer); safecall;
    function  Get_Balance: Integer; safecall;
    procedure Set_Balance(pValue: Integer); safecall;
    function  Get_EnableContextMenu: WordBool; safecall;
    procedure Set_EnableContextMenu(pEnable: WordBool); safecall;
    function  Get_ShowDisplay: WordBool; safecall;
    procedure Set_ShowDisplay(Show: WordBool); safecall;
    function  Get_ShowControls: WordBool; safecall;
    procedure Set_ShowControls(Show: WordBool); safecall;
    function  Get_ShowPositionControls: WordBool; safecall;
    procedure Set_ShowPositionControls(Show: WordBool); safecall;
    function  Get_ShowSelectionControls: WordBool; safecall;
    procedure Set_ShowSelectionControls(Show: WordBool); safecall;
    function  Get_ShowTracker: WordBool; safecall;
    procedure Set_ShowTracker(Show: WordBool); safecall;
    function  Get_EnablePositionControls: WordBool; safecall;
    procedure Set_EnablePositionControls(Enable: WordBool); safecall;
    function  Get_EnableSelectionControls: WordBool; safecall;
    procedure Set_EnableSelectionControls(Enable: WordBool); safecall;
    function  Get_EnableTracker: WordBool; safecall;
    procedure Set_EnableTracker(Enable: WordBool); safecall;
    function  Get_AllowHideDisplay: WordBool; safecall;
    procedure Set_AllowHideDisplay(Show: WordBool); safecall;
    function  Get_AllowHideControls: WordBool; safecall;
    procedure Set_AllowHideControls(Show: WordBool); safecall;
    function  Get_DisplayMode: DisplayModeConstants; safecall;
    procedure Set_DisplayMode(pValue: DisplayModeConstants); safecall;
    function  Get_AllowChangeDisplayMode: WordBool; safecall;
    procedure Set_AllowChangeDisplayMode(fAllow: WordBool); safecall;
    function  Get_FilterGraph: IUnknown; safecall;
    procedure Set_FilterGraph(const ppFilterGraph: IUnknown); safecall;
    function  Get_FilterGraphDispatch: IDispatch; safecall;
    function  Get_DisplayForeColor: LongWord; safecall;
    procedure Set_DisplayForeColor(ForeColor: LongWord); safecall;
    function  Get_DisplayBackColor: LongWord; safecall;
    procedure Set_DisplayBackColor(BackColor: LongWord); safecall;
    function  Get_MovieWindowSize: WindowSizeConstants; safecall;
    procedure Set_MovieWindowSize(WindowSize: WindowSizeConstants); safecall;
    function  Get_FullScreenMode: WordBool; safecall;
    procedure Set_FullScreenMode(pEnable: WordBool); safecall;
    function  Get_AutoStart: WordBool; safecall;
    procedure Set_AutoStart(pEnable: WordBool); safecall;
    function  Get_AutoRewind: WordBool; safecall;
    procedure Set_AutoRewind(pEnable: WordBool); safecall;
    function  Get_hWnd: Integer; safecall;
    function  Get_Appearance: AppearanceConstants; safecall;
    procedure Set_Appearance(pAppearance: AppearanceConstants); safecall;
    function  Get_BorderStyle: BorderStyleConstants; safecall;
    procedure Set_BorderStyle(pBorderStyle: BorderStyleConstants); safecall;
    function  Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(pEnabled: WordBool); safecall;
    function  Get_Info: Integer; safecall;
    property ImageSourceWidth: Integer read Get_ImageSourceWidth;
    property ImageSourceHeight: Integer read Get_ImageSourceHeight;
    property Author: WideString read Get_Author;
    property Title: WideString read Get_Title;
    property Copyright: WideString read Get_Copyright;
    property Description: WideString read Get_Description;
    property Rating: WideString read Get_Rating;
    property FileName: WideString read Get_FileName write Set_FileName;
    property Duration: Double read Get_Duration;
    property CurrentPosition: Double read Get_CurrentPosition write Set_CurrentPosition;
    property PlayCount: Integer read Get_PlayCount write Set_PlayCount;
    property SelectionStart: Double read Get_SelectionStart write Set_SelectionStart;
    property SelectionEnd: Double read Get_SelectionEnd write Set_SelectionEnd;
    property CurrentState: StateConstants read Get_CurrentState;
    property Rate: Double read Get_Rate write Set_Rate;
    property Volume: Integer read Get_Volume write Set_Volume;
    property Balance: Integer read Get_Balance write Set_Balance;
    property EnableContextMenu: WordBool read Get_EnableContextMenu write Set_EnableContextMenu;
    property ShowDisplay: WordBool read Get_ShowDisplay write Set_ShowDisplay;
    property ShowControls: WordBool read Get_ShowControls write Set_ShowControls;
    property ShowPositionControls: WordBool read Get_ShowPositionControls write Set_ShowPositionControls;
    property ShowSelectionControls: WordBool read Get_ShowSelectionControls write Set_ShowSelectionControls;
    property ShowTracker: WordBool read Get_ShowTracker write Set_ShowTracker;
    property EnablePositionControls: WordBool read Get_EnablePositionControls write Set_EnablePositionControls;
    property EnableSelectionControls: WordBool read Get_EnableSelectionControls write Set_EnableSelectionControls;
    property EnableTracker: WordBool read Get_EnableTracker write Set_EnableTracker;
    property AllowHideDisplay: WordBool read Get_AllowHideDisplay write Set_AllowHideDisplay;
    property AllowHideControls: WordBool read Get_AllowHideControls write Set_AllowHideControls;
    property DisplayMode: DisplayModeConstants read Get_DisplayMode write Set_DisplayMode;
    property AllowChangeDisplayMode: WordBool read Get_AllowChangeDisplayMode write Set_AllowChangeDisplayMode;
    property FilterGraph: IUnknown read Get_FilterGraph write Set_FilterGraph;
    property FilterGraphDispatch: IDispatch read Get_FilterGraphDispatch;
    property DisplayForeColor: LongWord read Get_DisplayForeColor write Set_DisplayForeColor;
    property DisplayBackColor: LongWord read Get_DisplayBackColor write Set_DisplayBackColor;
    property MovieWindowSize: WindowSizeConstants read Get_MovieWindowSize write Set_MovieWindowSize;
    property FullScreenMode: WordBool read Get_FullScreenMode write Set_FullScreenMode;
    property AutoStart: WordBool read Get_AutoStart write Set_AutoStart;
    property AutoRewind: WordBool read Get_AutoRewind write Set_AutoRewind;
    property hWnd: Integer read Get_hWnd;
    property Appearance: AppearanceConstants read Get_Appearance write Set_Appearance;
    property BorderStyle: BorderStyleConstants read Get_BorderStyle write Set_BorderStyle;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property Info: Integer read Get_Info;
  end;

// *********************************************************************//
// DispIntf:  IActiveMovieDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {05589FA2-C356-11CE-BF01-00AA0055595A}
// *********************************************************************//
  IActiveMovieDisp = dispinterface
    ['{05589FA2-C356-11CE-BF01-00AA0055595A}']
    procedure AboutBox; dispid -552;
    procedure Run; dispid 1610743809;
    procedure Pause; dispid 1610743810;
    procedure Stop; dispid 1610743811;
    property ImageSourceWidth: Integer readonly dispid 4;
    property ImageSourceHeight: Integer readonly dispid 5;
    property Author: WideString readonly dispid 6;
    property Title: WideString readonly dispid 7;
    property Copyright: WideString readonly dispid 8;
    property Description: WideString readonly dispid 9;
    property Rating: WideString readonly dispid 10;
    property FileName: WideString dispid 11;
    property Duration: Double readonly dispid 12;
    property CurrentPosition: Double dispid 13;
    property PlayCount: Integer dispid 14;
    property SelectionStart: Double dispid 15;
    property SelectionEnd: Double dispid 16;
    property CurrentState: StateConstants readonly dispid 17;
    property Rate: Double dispid 18;
    property Volume: Integer dispid 19;
    property Balance: Integer dispid 20;
    property EnableContextMenu: WordBool dispid 21;
    property ShowDisplay: WordBool dispid 22;
    property ShowControls: WordBool dispid 23;
    property ShowPositionControls: WordBool dispid 24;
    property ShowSelectionControls: WordBool dispid 25;
    property ShowTracker: WordBool dispid 26;
    property EnablePositionControls: WordBool dispid 27;
    property EnableSelectionControls: WordBool dispid 28;
    property EnableTracker: WordBool dispid 29;
    property AllowHideDisplay: WordBool dispid 30;
    property AllowHideControls: WordBool dispid 31;
    property DisplayMode: DisplayModeConstants dispid 32;
    property AllowChangeDisplayMode: WordBool dispid 33;
    property FilterGraph: IUnknown dispid 34;
    property FilterGraphDispatch: IDispatch readonly dispid 35;
    property DisplayForeColor: LongWord dispid 36;
    property DisplayBackColor: LongWord dispid 37;
    property MovieWindowSize: WindowSizeConstants dispid 38;
    property FullScreenMode: WordBool dispid 39;
    property AutoStart: WordBool dispid 40;
    property AutoRewind: WordBool dispid 41;
    property hWnd: Integer readonly dispid -515;
    property Appearance: AppearanceConstants dispid -520;
    property BorderStyle: BorderStyleConstants dispid 42;
    property Enabled: WordBool dispid -514;
    property Info: Integer readonly dispid 1610743885;
  end;

// *********************************************************************//
// Interface: IActiveMovie2
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {B6CD6554-E9CB-11D0-821F-00A0C91F9CA0}
// *********************************************************************//
  IActiveMovie2 = interface(IActiveMovie)
    ['{B6CD6554-E9CB-11D0-821F-00A0C91F9CA0}']
    function  IsSoundCardEnabled: WordBool; safecall;
    function  Get_ReadyState: ReadyStateConstants; safecall;
    property ReadyState: ReadyStateConstants read Get_ReadyState;
  end;

// *********************************************************************//
// DispIntf:  IActiveMovie2Disp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {B6CD6554-E9CB-11D0-821F-00A0C91F9CA0}
// *********************************************************************//
  IActiveMovie2Disp = dispinterface
    ['{B6CD6554-E9CB-11D0-821F-00A0C91F9CA0}']
    function  IsSoundCardEnabled: WordBool; dispid 53;
    property ReadyState: ReadyStateConstants readonly dispid -525;
    procedure AboutBox; dispid -552;
    procedure Run; dispid 1610743809;
    procedure Pause; dispid 1610743810;
    procedure Stop; dispid 1610743811;
    property ImageSourceWidth: Integer readonly dispid 4;
    property ImageSourceHeight: Integer readonly dispid 5;
    property Author: WideString readonly dispid 6;
    property Title: WideString readonly dispid 7;
    property Copyright: WideString readonly dispid 8;
    property Description: WideString readonly dispid 9;
    property Rating: WideString readonly dispid 10;
    property FileName: WideString dispid 11;
    property Duration: Double readonly dispid 12;
    property CurrentPosition: Double dispid 13;
    property PlayCount: Integer dispid 14;
    property SelectionStart: Double dispid 15;
    property SelectionEnd: Double dispid 16;
    property CurrentState: StateConstants readonly dispid 17;
    property Rate: Double dispid 18;
    property Volume: Integer dispid 19;
    property Balance: Integer dispid 20;
    property EnableContextMenu: WordBool dispid 21;
    property ShowDisplay: WordBool dispid 22;
    property ShowControls: WordBool dispid 23;
    property ShowPositionControls: WordBool dispid 24;
    property ShowSelectionControls: WordBool dispid 25;
    property ShowTracker: WordBool dispid 26;
    property EnablePositionControls: WordBool dispid 27;
    property EnableSelectionControls: WordBool dispid 28;
    property EnableTracker: WordBool dispid 29;
    property AllowHideDisplay: WordBool dispid 30;
    property AllowHideControls: WordBool dispid 31;
    property DisplayMode: DisplayModeConstants dispid 32;
    property AllowChangeDisplayMode: WordBool dispid 33;
    property FilterGraph: IUnknown dispid 34;
    property FilterGraphDispatch: IDispatch readonly dispid 35;
    property DisplayForeColor: LongWord dispid 36;
    property DisplayBackColor: LongWord dispid 37;
    property MovieWindowSize: WindowSizeConstants dispid 38;
    property FullScreenMode: WordBool dispid 39;
    property AutoStart: WordBool dispid 40;
    property AutoRewind: WordBool dispid 41;
    property hWnd: Integer readonly dispid -515;
    property Appearance: AppearanceConstants dispid -520;
    property BorderStyle: BorderStyleConstants dispid 42;
    property Enabled: WordBool dispid -514;
    property Info: Integer readonly dispid 1610743885;
  end;

// *********************************************************************//
// Interface: IActiveMovie3
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {265EC140-AE62-11D1-8500-00A0C91F9CA0}
// *********************************************************************//
  IActiveMovie3 = interface(IActiveMovie2)
    ['{265EC140-AE62-11D1-8500-00A0C91F9CA0}']
    function  Get_MediaPlayer: IDispatch; safecall;
    property MediaPlayer: IDispatch read Get_MediaPlayer;
  end;

// *********************************************************************//
// DispIntf:  IActiveMovie3Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {265EC140-AE62-11D1-8500-00A0C91F9CA0}
// *********************************************************************//
  IActiveMovie3Disp = dispinterface
    ['{265EC140-AE62-11D1-8500-00A0C91F9CA0}']
    property MediaPlayer: IDispatch readonly dispid 1111;
    function  IsSoundCardEnabled: WordBool; dispid 53;
    property ReadyState: ReadyStateConstants readonly dispid -525;
    procedure AboutBox; dispid -552;
    procedure Run; dispid 1610743809;
    procedure Pause; dispid 1610743810;
    procedure Stop; dispid 1610743811;
    property ImageSourceWidth: Integer readonly dispid 4;
    property ImageSourceHeight: Integer readonly dispid 5;
    property Author: WideString readonly dispid 6;
    property Title: WideString readonly dispid 7;
    property Copyright: WideString readonly dispid 8;
    property Description: WideString readonly dispid 9;
    property Rating: WideString readonly dispid 10;
    property FileName: WideString dispid 11;
    property Duration: Double readonly dispid 12;
    property CurrentPosition: Double dispid 13;
    property PlayCount: Integer dispid 14;
    property SelectionStart: Double dispid 15;
    property SelectionEnd: Double dispid 16;
    property CurrentState: StateConstants readonly dispid 17;
    property Rate: Double dispid 18;
    property Volume: Integer dispid 19;
    property Balance: Integer dispid 20;
    property EnableContextMenu: WordBool dispid 21;
    property ShowDisplay: WordBool dispid 22;
    property ShowControls: WordBool dispid 23;
    property ShowPositionControls: WordBool dispid 24;
    property ShowSelectionControls: WordBool dispid 25;
    property ShowTracker: WordBool dispid 26;
    property EnablePositionControls: WordBool dispid 27;
    property EnableSelectionControls: WordBool dispid 28;
    property EnableTracker: WordBool dispid 29;
    property AllowHideDisplay: WordBool dispid 30;
    property AllowHideControls: WordBool dispid 31;
    property DisplayMode: DisplayModeConstants dispid 32;
    property AllowChangeDisplayMode: WordBool dispid 33;
    property FilterGraph: IUnknown dispid 34;
    property FilterGraphDispatch: IDispatch readonly dispid 35;
    property DisplayForeColor: LongWord dispid 36;
    property DisplayBackColor: LongWord dispid 37;
    property MovieWindowSize: WindowSizeConstants dispid 38;
    property FullScreenMode: WordBool dispid 39;
    property AutoStart: WordBool dispid 40;
    property AutoRewind: WordBool dispid 41;
    property hWnd: Integer readonly dispid -515;
    property Appearance: AppearanceConstants dispid -520;
    property BorderStyle: BorderStyleConstants dispid 42;
    property Enabled: WordBool dispid -514;
    property Info: Integer readonly dispid 1610743885;
  end;

// *********************************************************************//
// The Class CoActiveMovie provides a Create and CreateRemote method to          
// create instances of the default interface IActiveMovie3 exposed by              
// the CoClass ActiveMovie. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoActiveMovie = class
    class function Create: IActiveMovie3;
    class function CreateRemote(const MachineName: string): IActiveMovie3;
  end;

  TActiveMovie = class(TOleControl)
  protected
    procedure InitControlData; override;
  published
    property  TabStop;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  Visible;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
  end;


implementation

uses ComObj;

procedure TActiveMovie.InitControlData;
const
  CControlData: TControlData2 = (
    ClassID: '{05589FA1-C356-11CE-BF01-00AA0055595A}';
    EventIID: '{05589FA1-C356-11CE-BF01-00AA0055595A}';
    EventCount: 0;
    EventDispIDs: nil;
    LicenseKey: nil;
    Flags: $00000000;
    Version: 0);
begin
  ControlData := @CControlData;
end;


class function CoActiveMovie.Create: IActiveMovie3;
begin
  Result := CreateComObject(CLASS_ActiveMovie) as IActiveMovie3;
end;

class function CoActiveMovie.CreateRemote(const MachineName: string): IActiveMovie3;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ActiveMovie) as IActiveMovie3;
end;

end.