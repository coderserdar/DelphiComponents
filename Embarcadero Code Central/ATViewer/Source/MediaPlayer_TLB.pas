{$WRITEABLECONST ON}
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 

unit MediaPlayer_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : $Revision:   1.88.1.0.1.0  $
// File generated on 29.03.06 23:45:14 from Type Library described below.

// ************************************************************************ //
// Type Lib: C:\WINDOWS\SYSTEM\MSDXM.OCX (1)
// IID\LCID: {22D6F304-B0F6-11D0-94AB-0080C74C7E95}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\SYSTEM\stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\WINDOWS\SYSTEM\STDVCL40.DLL)
// Errors:
//   Hint: TypeInfo 'MediaPlayer' changed to 'MediaPlayer_'
// ************************************************************************ //
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
  MediaPlayerMajorVersion = 1;
  MediaPlayerMinorVersion = 0;

  LIBID_MediaPlayer: TGUID = '{22D6F304-B0F6-11D0-94AB-0080C74C7E95}';

  DIID__IDirectControlEvents: TGUID = '{39A2C2A7-4778-11D2-9BDB-204C4F4F5020}';
  IID_IDirectControl: TGUID = '{39A2C2A5-4778-11D2-9BDB-204C4F4F5020}';
  CLASS_DirectControl: TGUID = '{39A2C2A6-4778-11D2-9BDB-204C4F4F5020}';
  DIID__IDirectContainerEvents: TGUID = '{39A2C2AA-4778-11D2-9BDB-204C4F4F5020}';
  IID_IDirectContainer: TGUID = '{39A2C2A8-4778-11D2-9BDB-204C4F4F5020}';
  CLASS_DirectContainer: TGUID = '{39A2C2A9-4778-11D2-9BDB-204C4F4F5020}';
  IID_IServiceProvider: TGUID = '{6D5140C1-7436-11CE-8034-00AA006009FA}';
  DIID__IRadioViewEvents: TGUID = '{847B4DF6-4B61-11D2-9BDB-204C4F4F5020}';
  IID_IRadioView: TGUID = '{847B4DF4-4B61-11D2-9BDB-204C4F4F5020}';
  CLASS_RadioView: TGUID = '{847B4DF5-4B61-11D2-9BDB-204C4F4F5020}';
  DIID__MediaPlayerEvents: TGUID = '{2D3A4C40-E711-11D0-94AB-0080C74C7E95}';
  IID_IMediaPlayer: TGUID = '{22D6F311-B0F6-11D0-94AB-0080C74C7E95}';
  IID_IMediaPlayer2: TGUID = '{20D4F5E0-5475-11D2-9774-0000F80855E6}';
  IID_IMediaBindStream: TGUID = '{920F0DE3-91C5-11D2-828F-00C04FC99D4E}';
  IID_IMediaPlayerDvd: TGUID = '{746EB440-3835-11D2-9774-0000F80855E6}';
  CLASS_MediaPlayer_: TGUID = '{22D6F312-B0F6-11D0-94AB-0080C74C7E95}';
  CLASS_ppDShowNet: TGUID = '{5C85DCB0-F967-11D0-81ED-00C04FC99D4C}';
  CLASS_ppDShowPlay: TGUID = '{C0CD59AE-020D-11D1-81F2-00C04FC99D4C}';
  CLASS_ppDSMeta: TGUID = '{2FEB9591-50CF-11D1-A6DF-006097C4E476}';
  CLASS_ppDSCnnl: TGUID = '{BB314F91-A010-11D1-A75A-006097C4E476}';
  CLASS_ppDSClip: TGUID = '{31C48C31-70B0-11D1-A708-006097C4E476}';
  CLASS_ppDSDetl: TGUID = '{31C48C32-70B0-11D1-A708-006097C4E476}';
  CLASS_ppDSApp: TGUID = '{2AFA62E2-5548-11D1-A6E1-006097C4E476}';
  CLASS_ppDSPropAdv: TGUID = '{8C4EB103-516F-11D1-A6DF-006097C4E476}';
  CLASS_ppDSView: TGUID = '{AE1A5812-5230-11D1-A6E0-006097C4E476}';
  CLASS_ppDSOAdv: TGUID = '{AE1A5813-5230-11D1-A6E0-006097C4E476}';
  DIID__IAsyncPProtEvents: TGUID = '{3DA2AA3C-3D96-11D2-9BD2-204C4F4F5020}';
  IID_IAsyncPProt: TGUID = '{3DA2AA3A-3D96-11D2-9BD2-204C4F4F5020}';
  CLASS_AsyncPProt: TGUID = '{3DA2AA3B-3D96-11D2-9BD2-204C4F4F5020}';
  IID_IAsyncMHandler: TGUID = '{3DA2AA3D-3D96-11D2-9BD2-204C4F4F5020}';
  CLASS_AsyncMHandler: TGUID = '{3DA2AA3E-3D96-11D2-9BD2-204C4F4F5020}';
  DIID__IRadioPlayerEvents: TGUID = '{9C2263B1-3E3C-11D2-9BD3-204C4F4F5020}';
  IID_IRadioPlayer: TGUID = '{9C2263AF-3E3C-11D2-9BD3-204C4F4F5020}';
  IID_IRadioServer: TGUID = '{9C2263A0-3E3C-11D2-9BD3-204C4F4F5020}';
  CLASS_RadioPlayer: TGUID = '{9C2263B0-3E3C-11D2-9BD3-204C4F4F5020}';
  IID_IRadioServerControl: TGUID = '{8E718889-423F-11D2-876E-00A0C9082467}';
  IID_IMediaPlayerListener: TGUID = '{33222211-5E5E-11D2-9E8E-0000F8085981}';
  CLASS_RadioServer: TGUID = '{8E71888A-423F-11D2-876E-00A0C9082467}';
  IID_IRadioBand: TGUID = '{8E718881-423F-11D2-876E-00A0C9082467}';
  CLASS_RadioBand: TGUID = '{8E718888-423F-11D2-876E-00A0C9082467}';
  CLASS_ppDSFile: TGUID = '{1D1237A0-6CD6-11D2-96BA-00104B242E64}';

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

// Constants for enum MPPlayStateConstants
type
  MPPlayStateConstants = TOleEnum;
const
  mpStopped = $00000000;
  mpPaused = $00000001;
  mpPlaying = $00000002;
  mpWaiting = $00000003;
  mpScanForward = $00000004;
  mpScanReverse = $00000005;
  mpClosed = $00000006;

// Constants for enum MPDisplaySizeConstants
type
  MPDisplaySizeConstants = TOleEnum;
const
  mpDefaultSize = $00000000;
  mpHalfSize = $00000001;
  mpDoubleSize = $00000002;
  mpFullScreen = $00000003;
  mpFitToSize = $00000004;
  mpOneSixteenthScreen = $00000005;
  mpOneFourthScreen = $00000006;
  mpOneHalfScreen = $00000007;

// Constants for enum MPReadyStateConstants
type
  MPReadyStateConstants = TOleEnum;
const
  mpReadyStateUninitialized = $00000000;
  mpReadyStateLoading = $00000001;
  mpReadyStateInteractive = $00000003;
  mpReadyStateComplete = $00000004;

// Constants for enum MPDisplayModeConstants
type
  MPDisplayModeConstants = TOleEnum;
const
  mpTime = $00000000;
  mpFrames = $00000001;

// Constants for enum MPMoreInfoType
type
  MPMoreInfoType = TOleEnum;
const
  mpShowURL = $00000000;
  mpClipURL = $00000001;
  mpBannerURL = $00000002;

// Constants for enum MPMediaInfoType
type
  MPMediaInfoType = TOleEnum;
const
  mpShowFilename = $00000000;
  mpShowTitle = $00000001;
  mpShowAuthor = $00000002;
  mpShowCopyright = $00000003;
  mpShowRating = $00000004;
  mpShowDescription = $00000005;
  mpShowLogoIcon = $00000006;
  mpClipFilename = $00000007;
  mpClipTitle = $00000008;
  mpClipAuthor = $00000009;
  mpClipCopyright = $0000000A;
  mpClipRating = $0000000B;
  mpClipDescription = $0000000C;
  mpClipLogoIcon = $0000000D;
  mpBannerImage = $0000000E;
  mpBannerMoreInfo = $0000000F;
  mpWatermark = $00000010;

// Constants for enum DVDMenuIDConstants
type
  DVDMenuIDConstants = TOleEnum;
const
  dvdMenu_Title = $00000002;
  dvdMenu_Root = $00000003;
  dvdMenu_Subpicture = $00000004;
  dvdMenu_Audio = $00000005;
  dvdMenu_Angle = $00000006;
  dvdMenu_Chapter = $00000007;

// Constants for enum MPShowDialogConstants
type
  MPShowDialogConstants = TOleEnum;
const
  mpShowDialogHelp = $00000000;
  mpShowDialogStatistics = $00000001;
  mpShowDialogOptions = $00000002;
  mpShowDialogContextMenu = $00000003;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  _IDirectControlEvents = dispinterface;
  IDirectControl = interface;
  IDirectControlDisp = dispinterface;
  _IDirectContainerEvents = dispinterface;
  IDirectContainer = interface;
  IServiceProvider = interface;
  _IRadioViewEvents = dispinterface;
  IRadioView = interface;
  IRadioViewDisp = dispinterface;
  _MediaPlayerEvents = dispinterface;
  IMediaPlayer = interface;
  IMediaPlayerDisp = dispinterface;
  IMediaPlayer2 = interface;
  IMediaPlayer2Disp = dispinterface;
  IMediaBindStream = interface;
  IMediaBindStreamDisp = dispinterface;
  IMediaPlayerDvd = interface;
  IMediaPlayerDvdDisp = dispinterface;
  _IAsyncPProtEvents = dispinterface;
  IAsyncPProt = interface;
  IAsyncPProtDisp = dispinterface;
  IAsyncMHandler = interface;
  IAsyncMHandlerDisp = dispinterface;
  _IRadioPlayerEvents = dispinterface;
  IRadioPlayer = interface;
  IRadioPlayerDisp = dispinterface;
  IRadioServer = interface;
  IRadioServerDisp = dispinterface;
  IRadioServerControl = interface;
  IRadioServerControlDisp = dispinterface;
  IMediaPlayerListener = interface;
  IRadioBand = interface;
  IRadioBandDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  DirectControl = IDirectControl;
  DirectContainer = IDirectContainer;
  RadioView = IRadioView;
  MediaPlayer_ = IMediaPlayer2;
  ppDShowNet = IUnknown;
  ppDShowPlay = IUnknown;
  ppDSMeta = IUnknown;
  ppDSCnnl = IUnknown;
  ppDSClip = IUnknown;
  ppDSDetl = IUnknown;
  ppDSApp = IUnknown;
  ppDSPropAdv = IUnknown;
  ppDSView = IUnknown;
  ppDSOAdv = IUnknown;
  AsyncPProt = IAsyncPProt;
  AsyncMHandler = IAsyncMHandler;
  RadioPlayer = IRadioPlayer;
  RadioServer = IRadioPlayer;
  RadioBand = IRadioBand;
  ppDSFile = IUnknown;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PIUnknown1 = ^IUnknown; {*}
  PUserType1 = ^TGUID; {*}
  PInteger1 = ^Integer; {*}

  VB_OLE_COLOR = LongWord; 

// *********************************************************************//
// DispIntf:  _IDirectControlEvents
// Flags:     (4096) Dispatchable
// GUID:      {39A2C2A7-4778-11D2-9BDB-204C4F4F5020}
// *********************************************************************//
  _IDirectControlEvents = dispinterface
    ['{39A2C2A7-4778-11D2-9BDB-204C4F4F5020}']
  end;

// *********************************************************************//
// Interface: IDirectControl
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {39A2C2A5-4778-11D2-9BDB-204C4F4F5020}
// *********************************************************************//
  IDirectControl = interface(IDispatch)
    ['{39A2C2A5-4778-11D2-9BDB-204C4F4F5020}']
    procedure CreateView(const bszClsid: WideString); safecall;
    procedure DestroyAllViews; safecall;
  end;

// *********************************************************************//
// DispIntf:  IDirectControlDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {39A2C2A5-4778-11D2-9BDB-204C4F4F5020}
// *********************************************************************//
  IDirectControlDisp = dispinterface
    ['{39A2C2A5-4778-11D2-9BDB-204C4F4F5020}']
    procedure CreateView(const bszClsid: WideString); dispid 1610743808;
    procedure DestroyAllViews; dispid 1610743809;
  end;

// *********************************************************************//
// DispIntf:  _IDirectContainerEvents
// Flags:     (4096) Dispatchable
// GUID:      {39A2C2AA-4778-11D2-9BDB-204C4F4F5020}
// *********************************************************************//
  _IDirectContainerEvents = dispinterface
    ['{39A2C2AA-4778-11D2-9BDB-204C4F4F5020}']
  end;

// *********************************************************************//
// Interface: IDirectContainer
// Flags:     (0)
// GUID:      {39A2C2A8-4778-11D2-9BDB-204C4F4F5020}
// *********************************************************************//
  IDirectContainer = interface(IUnknown)
    ['{39A2C2A8-4778-11D2-9BDB-204C4F4F5020}']
    function  CreateControl(const bszClsid: WideString; dwClsContext: LongWord; 
                            var ppunk: IUnknown; dwWindowStyle: LongWord): HResult; stdcall;
    function  SetServiceProvider(const pspSet: IServiceProvider): HResult; stdcall;
    function  SetIInputObjectSite(const pios: IUnknown): HResult; stdcall;
    function  ShowControl: HResult; stdcall;
    function  HideControl: HResult; stdcall;
    function  IsControlCreated: HResult; stdcall;
    function  DestroyControl: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IServiceProvider
// Flags:     (0)
// GUID:      {6D5140C1-7436-11CE-8034-00AA006009FA}
// *********************************************************************//
  IServiceProvider = interface(IUnknown)
    ['{6D5140C1-7436-11CE-8034-00AA006009FA}']
    function  RemoteQueryService(var guidService: TGUID; var riid: TGUID; out ppvObject: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  _IRadioViewEvents
// Flags:     (4096) Dispatchable
// GUID:      {847B4DF6-4B61-11D2-9BDB-204C4F4F5020}
// *********************************************************************//
  _IRadioViewEvents = dispinterface
    ['{847B4DF6-4B61-11D2-9BDB-204C4F4F5020}']
  end;

// *********************************************************************//
// Interface: IRadioView
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {847B4DF4-4B61-11D2-9BDB-204C4F4F5020}
// *********************************************************************//
  IRadioView = interface(IDispatch)
    ['{847B4DF4-4B61-11D2-9BDB-204C4F4F5020}']
  end;

// *********************************************************************//
// DispIntf:  IRadioViewDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {847B4DF4-4B61-11D2-9BDB-204C4F4F5020}
// *********************************************************************//
  IRadioViewDisp = dispinterface
    ['{847B4DF4-4B61-11D2-9BDB-204C4F4F5020}']
  end;

// *********************************************************************//
// DispIntf:  _MediaPlayerEvents
// Flags:     (4112) Hidden Dispatchable
// GUID:      {2D3A4C40-E711-11D0-94AB-0080C74C7E95}
// *********************************************************************//
  _MediaPlayerEvents = dispinterface
    ['{2D3A4C40-E711-11D0-94AB-0080C74C7E95}']
    procedure DVDNotify(EventCode: Integer; EventParam1: Integer; EventParam2: Integer); dispid 1505;
    procedure EndOfStream(Result: Integer); dispid 3002;
    procedure KeyDown(KeyCode: Smallint; ShiftState: Smallint); dispid -602;
    procedure KeyUp(KeyCode: Smallint; ShiftState: Smallint); dispid -604;
    procedure KeyPress(CharacterCode: Smallint); dispid -603;
    procedure MouseMove(Button: Smallint; ShiftState: Smallint; x: OLE_XPOS_PIXELS; 
                        y: OLE_YPOS_PIXELS); dispid -606;
    procedure MouseDown(Button: Smallint; ShiftState: Smallint; x: OLE_XPOS_PIXELS; 
                        y: OLE_YPOS_PIXELS); dispid -605;
    procedure MouseUp(Button: Smallint; ShiftState: Smallint; x: OLE_XPOS_PIXELS; y: OLE_YPOS_PIXELS); dispid -607;
    procedure Click(Button: Smallint; ShiftState: Smallint; x: OLE_XPOS_PIXELS; y: OLE_YPOS_PIXELS); dispid -600;
    procedure DblClick(Button: Smallint; ShiftState: Smallint; x: OLE_XPOS_PIXELS; 
                       y: OLE_YPOS_PIXELS); dispid -601;
    procedure OpenStateChange(OldState: Integer; NewState: Integer); dispid 3011;
    procedure PlayStateChange(OldState: Integer; NewState: Integer); dispid 3012;
    procedure ScriptCommand(const scType: WideString; const Param: WideString); dispid 3001;
    procedure Buffering(Start: WordBool); dispid 3003;
    procedure Error; dispid 3010;
    procedure MarkerHit(MarkerNum: Integer); dispid 3006;
    procedure Warning(WarningType: Integer; Param: Integer; const Description: WideString); dispid 3009;
    procedure NewStream; dispid 3008;
    procedure Disconnect(Result: Integer); dispid 3004;
    procedure PositionChange(oldPosition: Double; newPosition: Double); dispid 2;
    procedure DisplayModeChange; dispid 51;
    procedure ReadyStateChange(ReadyState: ReadyStateConstants); dispid -609;
  end;

// *********************************************************************//
// Interface: IMediaPlayer
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {22D6F311-B0F6-11D0-94AB-0080C74C7E95}
// *********************************************************************//
  IMediaPlayer = interface(IDispatch)
    ['{22D6F311-B0F6-11D0-94AB-0080C74C7E95}']
    function  Get_CurrentPosition: Double; safecall;
    procedure Set_CurrentPosition(pCurrentPosition: Double); safecall;
    function  Get_Duration: Double; safecall;
    function  Get_ImageSourceWidth: Integer; safecall;
    function  Get_ImageSourceHeight: Integer; safecall;
    function  Get_MarkerCount: Integer; safecall;
    function  Get_CanScan: WordBool; safecall;
    function  Get_CanSeek: WordBool; safecall;
    function  Get_CanSeekToMarkers: WordBool; safecall;
    function  Get_CurrentMarker: Integer; safecall;
    procedure Set_CurrentMarker(pCurrentMarker: Integer); safecall;
    function  Get_FileName: WideString; safecall;
    procedure Set_FileName(const pbstrFileName: WideString); safecall;
    function  Get_SourceLink: WideString; safecall;
    function  Get_CreationDate: TDateTime; safecall;
    function  Get_ErrorCorrection: WideString; safecall;
    function  Get_Bandwidth: Integer; safecall;
    function  Get_SourceProtocol: Integer; safecall;
    function  Get_ReceivedPackets: Integer; safecall;
    function  Get_RecoveredPackets: Integer; safecall;
    function  Get_LostPackets: Integer; safecall;
    function  Get_ReceptionQuality: Integer; safecall;
    function  Get_BufferingCount: Integer; safecall;
    function  Get_IsBroadcast: WordBool; safecall;
    function  Get_BufferingProgress: Integer; safecall;
    function  Get_ChannelName: WideString; safecall;
    function  Get_ChannelDescription: WideString; safecall;
    function  Get_ChannelURL: WideString; safecall;
    function  Get_ContactAddress: WideString; safecall;
    function  Get_ContactPhone: WideString; safecall;
    function  Get_ContactEmail: WideString; safecall;
    function  Get_BufferingTime: Double; safecall;
    procedure Set_BufferingTime(pBufferingTime: Double); safecall;
    function  Get_AutoStart: WordBool; safecall;
    procedure Set_AutoStart(pAutoStart: WordBool); safecall;
    function  Get_AutoRewind: WordBool; safecall;
    procedure Set_AutoRewind(pAutoRewind: WordBool); safecall;
    function  Get_Rate: Double; safecall;
    procedure Set_Rate(pRate: Double); safecall;
    function  Get_SendKeyboardEvents: WordBool; safecall;
    procedure Set_SendKeyboardEvents(pSendKeyboardEvents: WordBool); safecall;
    function  Get_SendMouseClickEvents: WordBool; safecall;
    procedure Set_SendMouseClickEvents(pSendMouseClickEvents: WordBool); safecall;
    function  Get_SendMouseMoveEvents: WordBool; safecall;
    procedure Set_SendMouseMoveEvents(pSendMouseMoveEvents: WordBool); safecall;
    function  Get_PlayCount: Integer; safecall;
    procedure Set_PlayCount(pPlayCount: Integer); safecall;
    function  Get_ClickToPlay: WordBool; safecall;
    procedure Set_ClickToPlay(pClickToPlay: WordBool); safecall;
    function  Get_AllowScan: WordBool; safecall;
    procedure Set_AllowScan(pAllowScan: WordBool); safecall;
    function  Get_EnableContextMenu: WordBool; safecall;
    procedure Set_EnableContextMenu(pEnableContextMenu: WordBool); safecall;
    function  Get_CursorType: Integer; safecall;
    procedure Set_CursorType(pCursorType: Integer); safecall;
    function  Get_CodecCount: Integer; safecall;
    function  Get_AllowChangeDisplaySize: WordBool; safecall;
    procedure Set_AllowChangeDisplaySize(pAllowChangeDisplaySize: WordBool); safecall;
    function  Get_IsDurationValid: WordBool; safecall;
    function  Get_OpenState: Integer; safecall;
    function  Get_SendOpenStateChangeEvents: WordBool; safecall;
    procedure Set_SendOpenStateChangeEvents(pSendOpenStateChangeEvents: WordBool); safecall;
    function  Get_SendWarningEvents: WordBool; safecall;
    procedure Set_SendWarningEvents(pSendWarningEvents: WordBool); safecall;
    function  Get_SendErrorEvents: WordBool; safecall;
    procedure Set_SendErrorEvents(pSendErrorEvents: WordBool); safecall;
    function  Get_PlayState: MPPlayStateConstants; safecall;
    function  Get_SendPlayStateChangeEvents: WordBool; safecall;
    procedure Set_SendPlayStateChangeEvents(pSendPlayStateChangeEvents: WordBool); safecall;
    function  Get_DisplaySize: MPDisplaySizeConstants; safecall;
    procedure Set_DisplaySize(pDisplaySize: MPDisplaySizeConstants); safecall;
    function  Get_InvokeURLs: WordBool; safecall;
    procedure Set_InvokeURLs(pInvokeURLs: WordBool); safecall;
    function  Get_BaseURL: WideString; safecall;
    procedure Set_BaseURL(const pbstrBaseURL: WideString); safecall;
    function  Get_DefaultFrame: WideString; safecall;
    procedure Set_DefaultFrame(const pbstrDefaultFrame: WideString); safecall;
    function  Get_HasError: WordBool; safecall;
    function  Get_ErrorDescription: WideString; safecall;
    function  Get_ErrorCode: Integer; safecall;
    function  Get_AnimationAtStart: WordBool; safecall;
    procedure Set_AnimationAtStart(pAnimationAtStart: WordBool); safecall;
    function  Get_TransparentAtStart: WordBool; safecall;
    procedure Set_TransparentAtStart(pTransparentAtStart: WordBool); safecall;
    function  Get_Volume: Integer; safecall;
    procedure Set_Volume(pVolume: Integer); safecall;
    function  Get_Balance: Integer; safecall;
    procedure Set_Balance(pBalance: Integer); safecall;
    function  Get_ReadyState: MPReadyStateConstants; safecall;
    function  Get_SelectionStart: Double; safecall;
    procedure Set_SelectionStart(pValue: Double); safecall;
    function  Get_SelectionEnd: Double; safecall;
    procedure Set_SelectionEnd(pValue: Double); safecall;
    function  Get_ShowDisplay: WordBool; safecall;
    procedure Set_ShowDisplay(Show: WordBool); safecall;
    function  Get_ShowControls: WordBool; safecall;
    procedure Set_ShowControls(Show: WordBool); safecall;
    function  Get_ShowPositionControls: WordBool; safecall;
    procedure Set_ShowPositionControls(Show: WordBool); safecall;
    function  Get_ShowTracker: WordBool; safecall;
    procedure Set_ShowTracker(Show: WordBool); safecall;
    function  Get_EnablePositionControls: WordBool; safecall;
    procedure Set_EnablePositionControls(Enable: WordBool); safecall;
    function  Get_EnableTracker: WordBool; safecall;
    procedure Set_EnableTracker(Enable: WordBool); safecall;
    function  Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(pEnabled: WordBool); safecall;
    function  Get_DisplayForeColor: VB_OLE_COLOR; safecall;
    procedure Set_DisplayForeColor(ForeColor: VB_OLE_COLOR); safecall;
    function  Get_DisplayBackColor: VB_OLE_COLOR; safecall;
    procedure Set_DisplayBackColor(BackColor: VB_OLE_COLOR); safecall;
    function  Get_DisplayMode: MPDisplayModeConstants; safecall;
    procedure Set_DisplayMode(pValue: MPDisplayModeConstants); safecall;
    function  Get_VideoBorder3D: WordBool; safecall;
    procedure Set_VideoBorder3D(pVideoBorderWidth: WordBool); safecall;
    function  Get_VideoBorderWidth: Integer; safecall;
    procedure Set_VideoBorderWidth(pVideoBorderWidth: Integer); safecall;
    function  Get_VideoBorderColor: VB_OLE_COLOR; safecall;
    procedure Set_VideoBorderColor(pVideoBorderWidth: VB_OLE_COLOR); safecall;
    function  Get_ShowGotoBar: WordBool; safecall;
    procedure Set_ShowGotoBar(pbool: WordBool); safecall;
    function  Get_ShowStatusBar: WordBool; safecall;
    procedure Set_ShowStatusBar(pbool: WordBool); safecall;
    function  Get_ShowCaptioning: WordBool; safecall;
    procedure Set_ShowCaptioning(pbool: WordBool); safecall;
    function  Get_ShowAudioControls: WordBool; safecall;
    procedure Set_ShowAudioControls(pbool: WordBool); safecall;
    function  Get_CaptioningID: WideString; safecall;
    procedure Set_CaptioningID(const pstrText: WideString); safecall;
    function  Get_Mute: WordBool; safecall;
    procedure Set_Mute(vbool: WordBool); safecall;
    function  Get_CanPreview: WordBool; safecall;
    function  Get_PreviewMode: WordBool; safecall;
    procedure Set_PreviewMode(pPreviewMode: WordBool); safecall;
    function  Get_HasMultipleItems: WordBool; safecall;
    function  Get_Language: Integer; safecall;
    procedure Set_Language(pLanguage: Integer); safecall;
    function  Get_AudioStream: Integer; safecall;
    procedure Set_AudioStream(pStream: Integer); safecall;
    function  Get_SAMIStyle: WideString; safecall;
    procedure Set_SAMIStyle(const pbstrStyle: WideString); safecall;
    function  Get_SAMILang: WideString; safecall;
    procedure Set_SAMILang(const pbstrLang: WideString); safecall;
    function  Get_SAMIFileName: WideString; safecall;
    procedure Set_SAMIFileName(const pbstrFileName: WideString); safecall;
    function  Get_StreamCount: Integer; safecall;
    function  Get_ClientId: WideString; safecall;
    function  Get_ConnectionSpeed: Integer; safecall;
    function  Get_AutoSize: WordBool; safecall;
    procedure Set_AutoSize(pbool: WordBool); safecall;
    function  Get_EnableFullScreenControls: WordBool; safecall;
    procedure Set_EnableFullScreenControls(pbVal: WordBool); safecall;
    function  Get_ActiveMovie: IDispatch; safecall;
    function  Get_NSPlay: IDispatch; safecall;
    function  Get_WindowlessVideo: WordBool; safecall;
    procedure Set_WindowlessVideo(pbool: WordBool); safecall;
    procedure Play; safecall;
    procedure Stop; safecall;
    procedure Pause; safecall;
    function  GetMarkerTime(MarkerNum: Integer): Double; safecall;
    function  GetMarkerName(MarkerNum: Integer): WideString; safecall;
    procedure AboutBox; safecall;
    function  GetCodecInstalled(CodecNum: Integer): WordBool; safecall;
    function  GetCodecDescription(CodecNum: Integer): WideString; safecall;
    function  GetCodecURL(CodecNum: Integer): WideString; safecall;
    function  GetMoreInfoURL(MoreInfoType: MPMoreInfoType): WideString; safecall;
    function  GetMediaInfoString(MediaInfoType: MPMediaInfoType): WideString; safecall;
    procedure Cancel; safecall;
    procedure Open(const bstrFileName: WideString); safecall;
    function  IsSoundCardEnabled: WordBool; safecall;
    procedure Next; safecall;
    procedure Previous; safecall;
    procedure StreamSelect(StreamNum: Integer); safecall;
    procedure FastForward; safecall;
    procedure FastReverse; safecall;
    function  GetStreamName(StreamNum: Integer): WideString; safecall;
    function  GetStreamGroup(StreamNum: Integer): Integer; safecall;
    function  GetStreamSelected(StreamNum: Integer): WordBool; safecall;
    property CurrentPosition: Double read Get_CurrentPosition write Set_CurrentPosition;
    property Duration: Double read Get_Duration;
    property ImageSourceWidth: Integer read Get_ImageSourceWidth;
    property ImageSourceHeight: Integer read Get_ImageSourceHeight;
    property MarkerCount: Integer read Get_MarkerCount;
    property CanScan: WordBool read Get_CanScan;
    property CanSeek: WordBool read Get_CanSeek;
    property CanSeekToMarkers: WordBool read Get_CanSeekToMarkers;
    property CurrentMarker: Integer read Get_CurrentMarker write Set_CurrentMarker;
    property FileName: WideString read Get_FileName write Set_FileName;
    property SourceLink: WideString read Get_SourceLink;
    property CreationDate: TDateTime read Get_CreationDate;
    property ErrorCorrection: WideString read Get_ErrorCorrection;
    property Bandwidth: Integer read Get_Bandwidth;
    property SourceProtocol: Integer read Get_SourceProtocol;
    property ReceivedPackets: Integer read Get_ReceivedPackets;
    property RecoveredPackets: Integer read Get_RecoveredPackets;
    property LostPackets: Integer read Get_LostPackets;
    property ReceptionQuality: Integer read Get_ReceptionQuality;
    property BufferingCount: Integer read Get_BufferingCount;
    property IsBroadcast: WordBool read Get_IsBroadcast;
    property BufferingProgress: Integer read Get_BufferingProgress;
    property ChannelName: WideString read Get_ChannelName;
    property ChannelDescription: WideString read Get_ChannelDescription;
    property ChannelURL: WideString read Get_ChannelURL;
    property ContactAddress: WideString read Get_ContactAddress;
    property ContactPhone: WideString read Get_ContactPhone;
    property ContactEmail: WideString read Get_ContactEmail;
    property BufferingTime: Double read Get_BufferingTime write Set_BufferingTime;
    property AutoStart: WordBool read Get_AutoStart write Set_AutoStart;
    property AutoRewind: WordBool read Get_AutoRewind write Set_AutoRewind;
    property Rate: Double read Get_Rate write Set_Rate;
    property SendKeyboardEvents: WordBool read Get_SendKeyboardEvents write Set_SendKeyboardEvents;
    property SendMouseClickEvents: WordBool read Get_SendMouseClickEvents write Set_SendMouseClickEvents;
    property SendMouseMoveEvents: WordBool read Get_SendMouseMoveEvents write Set_SendMouseMoveEvents;
    property PlayCount: Integer read Get_PlayCount write Set_PlayCount;
    property ClickToPlay: WordBool read Get_ClickToPlay write Set_ClickToPlay;
    property AllowScan: WordBool read Get_AllowScan write Set_AllowScan;
    property EnableContextMenu: WordBool read Get_EnableContextMenu write Set_EnableContextMenu;
    property CursorType: Integer read Get_CursorType write Set_CursorType;
    property CodecCount: Integer read Get_CodecCount;
    property AllowChangeDisplaySize: WordBool read Get_AllowChangeDisplaySize write Set_AllowChangeDisplaySize;
    property IsDurationValid: WordBool read Get_IsDurationValid;
    property OpenState: Integer read Get_OpenState;
    property SendOpenStateChangeEvents: WordBool read Get_SendOpenStateChangeEvents write Set_SendOpenStateChangeEvents;
    property SendWarningEvents: WordBool read Get_SendWarningEvents write Set_SendWarningEvents;
    property SendErrorEvents: WordBool read Get_SendErrorEvents write Set_SendErrorEvents;
    property PlayState: MPPlayStateConstants read Get_PlayState;
    property SendPlayStateChangeEvents: WordBool read Get_SendPlayStateChangeEvents write Set_SendPlayStateChangeEvents;
    property DisplaySize: MPDisplaySizeConstants read Get_DisplaySize write Set_DisplaySize;
    property InvokeURLs: WordBool read Get_InvokeURLs write Set_InvokeURLs;
    property BaseURL: WideString read Get_BaseURL write Set_BaseURL;
    property DefaultFrame: WideString read Get_DefaultFrame write Set_DefaultFrame;
    property HasError: WordBool read Get_HasError;
    property ErrorDescription: WideString read Get_ErrorDescription;
    property ErrorCode: Integer read Get_ErrorCode;
    property AnimationAtStart: WordBool read Get_AnimationAtStart write Set_AnimationAtStart;
    property TransparentAtStart: WordBool read Get_TransparentAtStart write Set_TransparentAtStart;
    property Volume: Integer read Get_Volume write Set_Volume;
    property Balance: Integer read Get_Balance write Set_Balance;
    property ReadyState: MPReadyStateConstants read Get_ReadyState;
    property SelectionStart: Double read Get_SelectionStart write Set_SelectionStart;
    property SelectionEnd: Double read Get_SelectionEnd write Set_SelectionEnd;
    property ShowDisplay: WordBool read Get_ShowDisplay write Set_ShowDisplay;
    property ShowControls: WordBool read Get_ShowControls write Set_ShowControls;
    property ShowPositionControls: WordBool read Get_ShowPositionControls write Set_ShowPositionControls;
    property ShowTracker: WordBool read Get_ShowTracker write Set_ShowTracker;
    property EnablePositionControls: WordBool read Get_EnablePositionControls write Set_EnablePositionControls;
    property EnableTracker: WordBool read Get_EnableTracker write Set_EnableTracker;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property DisplayForeColor: VB_OLE_COLOR read Get_DisplayForeColor write Set_DisplayForeColor;
    property DisplayBackColor: VB_OLE_COLOR read Get_DisplayBackColor write Set_DisplayBackColor;
    property DisplayMode: MPDisplayModeConstants read Get_DisplayMode write Set_DisplayMode;
    property VideoBorder3D: WordBool read Get_VideoBorder3D write Set_VideoBorder3D;
    property VideoBorderWidth: Integer read Get_VideoBorderWidth write Set_VideoBorderWidth;
    property VideoBorderColor: VB_OLE_COLOR read Get_VideoBorderColor write Set_VideoBorderColor;
    property ShowGotoBar: WordBool read Get_ShowGotoBar write Set_ShowGotoBar;
    property ShowStatusBar: WordBool read Get_ShowStatusBar write Set_ShowStatusBar;
    property ShowCaptioning: WordBool read Get_ShowCaptioning write Set_ShowCaptioning;
    property ShowAudioControls: WordBool read Get_ShowAudioControls write Set_ShowAudioControls;
    property CaptioningID: WideString read Get_CaptioningID write Set_CaptioningID;
    property Mute: WordBool read Get_Mute write Set_Mute;
    property CanPreview: WordBool read Get_CanPreview;
    property PreviewMode: WordBool read Get_PreviewMode write Set_PreviewMode;
    property HasMultipleItems: WordBool read Get_HasMultipleItems;
    property Language: Integer read Get_Language write Set_Language;
    property AudioStream: Integer read Get_AudioStream write Set_AudioStream;
    property SAMIStyle: WideString read Get_SAMIStyle write Set_SAMIStyle;
    property SAMILang: WideString read Get_SAMILang write Set_SAMILang;
    property SAMIFileName: WideString read Get_SAMIFileName write Set_SAMIFileName;
    property StreamCount: Integer read Get_StreamCount;
    property ClientId: WideString read Get_ClientId;
    property ConnectionSpeed: Integer read Get_ConnectionSpeed;
    property AutoSize: WordBool read Get_AutoSize write Set_AutoSize;
    property EnableFullScreenControls: WordBool read Get_EnableFullScreenControls write Set_EnableFullScreenControls;
    property ActiveMovie: IDispatch read Get_ActiveMovie;
    property NSPlay: IDispatch read Get_NSPlay;
    property WindowlessVideo: WordBool read Get_WindowlessVideo write Set_WindowlessVideo;
  end;

// *********************************************************************//
// DispIntf:  IMediaPlayerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {22D6F311-B0F6-11D0-94AB-0080C74C7E95}
// *********************************************************************//
  IMediaPlayerDisp = dispinterface
    ['{22D6F311-B0F6-11D0-94AB-0080C74C7E95}']
    property CurrentPosition: Double dispid 1027;
    property Duration: Double readonly dispid 1003;
    property ImageSourceWidth: Integer readonly dispid 1001;
    property ImageSourceHeight: Integer readonly dispid 1002;
    property MarkerCount: Integer readonly dispid 1010;
    property CanScan: WordBool readonly dispid 1011;
    property CanSeek: WordBool readonly dispid 1012;
    property CanSeekToMarkers: WordBool readonly dispid 1047;
    property CurrentMarker: Integer dispid 1029;
    property FileName: WideString dispid 1026;
    property SourceLink: WideString readonly dispid 1009;
    property CreationDate: TDateTime readonly dispid 1036;
    property ErrorCorrection: WideString readonly dispid 1038;
    property Bandwidth: Integer readonly dispid 1037;
    property SourceProtocol: Integer readonly dispid 1060;
    property ReceivedPackets: Integer readonly dispid 1039;
    property RecoveredPackets: Integer readonly dispid 1040;
    property LostPackets: Integer readonly dispid 1041;
    property ReceptionQuality: Integer readonly dispid 1042;
    property BufferingCount: Integer readonly dispid 1043;
    property IsBroadcast: WordBool readonly dispid 1058;
    property BufferingProgress: Integer readonly dispid 1080;
    property ChannelName: WideString readonly dispid 1050;
    property ChannelDescription: WideString readonly dispid 1051;
    property ChannelURL: WideString readonly dispid 1052;
    property ContactAddress: WideString readonly dispid 1053;
    property ContactPhone: WideString readonly dispid 1054;
    property ContactEmail: WideString readonly dispid 1055;
    property BufferingTime: Double dispid 1070;
    property AutoStart: WordBool dispid 1017;
    property AutoRewind: WordBool dispid 1018;
    property Rate: Double dispid 1028;
    property SendKeyboardEvents: WordBool dispid 1013;
    property SendMouseClickEvents: WordBool dispid 1014;
    property SendMouseMoveEvents: WordBool dispid 1015;
    property PlayCount: Integer dispid 1030;
    property ClickToPlay: WordBool dispid 1025;
    property AllowScan: WordBool dispid 1035;
    property EnableContextMenu: WordBool dispid 1021;
    property CursorType: Integer dispid 1044;
    property CodecCount: Integer readonly dispid 1057;
    property AllowChangeDisplaySize: WordBool dispid 1056;
    property IsDurationValid: WordBool readonly dispid 1059;
    property OpenState: Integer readonly dispid 1061;
    property SendOpenStateChangeEvents: WordBool dispid 1062;
    property SendWarningEvents: WordBool dispid 1063;
    property SendErrorEvents: WordBool dispid 1064;
    property PlayState: MPPlayStateConstants readonly dispid 1068;
    property SendPlayStateChangeEvents: WordBool dispid 1069;
    property DisplaySize: MPDisplaySizeConstants dispid 1032;
    property InvokeURLs: WordBool dispid 1020;
    property BaseURL: WideString dispid 1082;
    property DefaultFrame: WideString dispid 1083;
    property HasError: WordBool readonly dispid 1065;
    property ErrorDescription: WideString readonly dispid 1066;
    property ErrorCode: Integer readonly dispid 1067;
    property AnimationAtStart: WordBool dispid 1045;
    property TransparentAtStart: WordBool dispid 1022;
    property Volume: Integer dispid 19;
    property Balance: Integer dispid 20;
    property ReadyState: MPReadyStateConstants readonly dispid -525;
    property SelectionStart: Double dispid 15;
    property SelectionEnd: Double dispid 16;
    property ShowDisplay: WordBool dispid 22;
    property ShowControls: WordBool dispid 23;
    property ShowPositionControls: WordBool dispid 24;
    property ShowTracker: WordBool dispid 26;
    property EnablePositionControls: WordBool dispid 27;
    property EnableTracker: WordBool dispid 29;
    property Enabled: WordBool dispid -514;
    property DisplayForeColor: VB_OLE_COLOR dispid 36;
    property DisplayBackColor: VB_OLE_COLOR dispid 37;
    property DisplayMode: MPDisplayModeConstants dispid 32;
    property VideoBorder3D: WordBool dispid 1103;
    property VideoBorderWidth: Integer dispid 1101;
    property VideoBorderColor: VB_OLE_COLOR dispid 1102;
    property ShowGotoBar: WordBool dispid 1088;
    property ShowStatusBar: WordBool dispid 1086;
    property ShowCaptioning: WordBool dispid 1084;
    property ShowAudioControls: WordBool dispid 1107;
    property CaptioningID: WideString dispid 1085;
    property Mute: WordBool dispid 1089;
    property CanPreview: WordBool readonly dispid 1093;
    property PreviewMode: WordBool dispid 1091;
    property HasMultipleItems: WordBool readonly dispid 1094;
    property Language: Integer dispid 1095;
    property AudioStream: Integer dispid 1096;
    property SAMIStyle: WideString dispid 1097;
    property SAMILang: WideString dispid 1098;
    property SAMIFileName: WideString dispid 1099;
    property StreamCount: Integer readonly dispid 1100;
    property ClientId: WideString readonly dispid 1106;
    property ConnectionSpeed: Integer readonly dispid 1113;
    property AutoSize: WordBool dispid -500;
    property EnableFullScreenControls: WordBool dispid 1108;
    property ActiveMovie: IDispatch readonly dispid 1109;
    property NSPlay: IDispatch readonly dispid 1110;
    property WindowlessVideo: WordBool dispid 1112;
    procedure Play; dispid 2001;
    procedure Stop; dispid 2003;
    procedure Pause; dispid 2002;
    function  GetMarkerTime(MarkerNum: Integer): Double; dispid 2004;
    function  GetMarkerName(MarkerNum: Integer): WideString; dispid 2005;
    procedure AboutBox; dispid -552;
    function  GetCodecInstalled(CodecNum: Integer): WordBool; dispid 2007;
    function  GetCodecDescription(CodecNum: Integer): WideString; dispid 2008;
    function  GetCodecURL(CodecNum: Integer): WideString; dispid 2009;
    function  GetMoreInfoURL(MoreInfoType: MPMoreInfoType): WideString; dispid 2011;
    function  GetMediaInfoString(MediaInfoType: MPMediaInfoType): WideString; dispid 2016;
    procedure Cancel; dispid 2006;
    procedure Open(const bstrFileName: WideString); dispid 2010;
    function  IsSoundCardEnabled: WordBool; dispid 53;
    procedure Next; dispid 2023;
    procedure Previous; dispid 2022;
    procedure StreamSelect(StreamNum: Integer); dispid 2015;
    procedure FastForward; dispid 2024;
    procedure FastReverse; dispid 2025;
    function  GetStreamName(StreamNum: Integer): WideString; dispid 2019;
    function  GetStreamGroup(StreamNum: Integer): Integer; dispid 2020;
    function  GetStreamSelected(StreamNum: Integer): WordBool; dispid 2021;
  end;

// *********************************************************************//
// Interface: IMediaPlayer2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {20D4F5E0-5475-11D2-9774-0000F80855E6}
// *********************************************************************//
  IMediaPlayer2 = interface(IMediaPlayer)
    ['{20D4F5E0-5475-11D2-9774-0000F80855E6}']
    function  Get_DVD: IMediaPlayerDvd; safecall;
    function  GetMediaParameter(EntryNum: Integer; const bstrParameterName: WideString): WideString; safecall;
    function  GetMediaParameterName(EntryNum: Integer; Index: Integer): WideString; safecall;
    function  Get_EntryCount: Integer; safecall;
    function  GetCurrentEntry: Integer; safecall;
    procedure SetCurrentEntry(EntryNumber: Integer); safecall;
    procedure ShowDialog(mpDialogIndex: MPShowDialogConstants); safecall;
    property DVD: IMediaPlayerDvd read Get_DVD;
    property EntryCount: Integer read Get_EntryCount;
  end;

// *********************************************************************//
// DispIntf:  IMediaPlayer2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {20D4F5E0-5475-11D2-9774-0000F80855E6}
// *********************************************************************//
  IMediaPlayer2Disp = dispinterface
    ['{20D4F5E0-5475-11D2-9774-0000F80855E6}']
    property DVD: IMediaPlayerDvd readonly dispid 1500;
    function  GetMediaParameter(EntryNum: Integer; const bstrParameterName: WideString): WideString; dispid 2028;
    function  GetMediaParameterName(EntryNum: Integer; Index: Integer): WideString; dispid 2029;
    property EntryCount: Integer readonly dispid 2030;
    function  GetCurrentEntry: Integer; dispid 2031;
    procedure SetCurrentEntry(EntryNumber: Integer); dispid 2032;
    procedure ShowDialog(mpDialogIndex: MPShowDialogConstants); dispid 2033;
    property CurrentPosition: Double dispid 1027;
    property Duration: Double readonly dispid 1003;
    property ImageSourceWidth: Integer readonly dispid 1001;
    property ImageSourceHeight: Integer readonly dispid 1002;
    property MarkerCount: Integer readonly dispid 1010;
    property CanScan: WordBool readonly dispid 1011;
    property CanSeek: WordBool readonly dispid 1012;
    property CanSeekToMarkers: WordBool readonly dispid 1047;
    property CurrentMarker: Integer dispid 1029;
    property FileName: WideString dispid 1026;
    property SourceLink: WideString readonly dispid 1009;
    property CreationDate: TDateTime readonly dispid 1036;
    property ErrorCorrection: WideString readonly dispid 1038;
    property Bandwidth: Integer readonly dispid 1037;
    property SourceProtocol: Integer readonly dispid 1060;
    property ReceivedPackets: Integer readonly dispid 1039;
    property RecoveredPackets: Integer readonly dispid 1040;
    property LostPackets: Integer readonly dispid 1041;
    property ReceptionQuality: Integer readonly dispid 1042;
    property BufferingCount: Integer readonly dispid 1043;
    property IsBroadcast: WordBool readonly dispid 1058;
    property BufferingProgress: Integer readonly dispid 1080;
    property ChannelName: WideString readonly dispid 1050;
    property ChannelDescription: WideString readonly dispid 1051;
    property ChannelURL: WideString readonly dispid 1052;
    property ContactAddress: WideString readonly dispid 1053;
    property ContactPhone: WideString readonly dispid 1054;
    property ContactEmail: WideString readonly dispid 1055;
    property BufferingTime: Double dispid 1070;
    property AutoStart: WordBool dispid 1017;
    property AutoRewind: WordBool dispid 1018;
    property Rate: Double dispid 1028;
    property SendKeyboardEvents: WordBool dispid 1013;
    property SendMouseClickEvents: WordBool dispid 1014;
    property SendMouseMoveEvents: WordBool dispid 1015;
    property PlayCount: Integer dispid 1030;
    property ClickToPlay: WordBool dispid 1025;
    property AllowScan: WordBool dispid 1035;
    property EnableContextMenu: WordBool dispid 1021;
    property CursorType: Integer dispid 1044;
    property CodecCount: Integer readonly dispid 1057;
    property AllowChangeDisplaySize: WordBool dispid 1056;
    property IsDurationValid: WordBool readonly dispid 1059;
    property OpenState: Integer readonly dispid 1061;
    property SendOpenStateChangeEvents: WordBool dispid 1062;
    property SendWarningEvents: WordBool dispid 1063;
    property SendErrorEvents: WordBool dispid 1064;
    property PlayState: MPPlayStateConstants readonly dispid 1068;
    property SendPlayStateChangeEvents: WordBool dispid 1069;
    property DisplaySize: MPDisplaySizeConstants dispid 1032;
    property InvokeURLs: WordBool dispid 1020;
    property BaseURL: WideString dispid 1082;
    property DefaultFrame: WideString dispid 1083;
    property HasError: WordBool readonly dispid 1065;
    property ErrorDescription: WideString readonly dispid 1066;
    property ErrorCode: Integer readonly dispid 1067;
    property AnimationAtStart: WordBool dispid 1045;
    property TransparentAtStart: WordBool dispid 1022;
    property Volume: Integer dispid 19;
    property Balance: Integer dispid 20;
    property ReadyState: MPReadyStateConstants readonly dispid -525;
    property SelectionStart: Double dispid 15;
    property SelectionEnd: Double dispid 16;
    property ShowDisplay: WordBool dispid 22;
    property ShowControls: WordBool dispid 23;
    property ShowPositionControls: WordBool dispid 24;
    property ShowTracker: WordBool dispid 26;
    property EnablePositionControls: WordBool dispid 27;
    property EnableTracker: WordBool dispid 29;
    property Enabled: WordBool dispid -514;
    property DisplayForeColor: VB_OLE_COLOR dispid 36;
    property DisplayBackColor: VB_OLE_COLOR dispid 37;
    property DisplayMode: MPDisplayModeConstants dispid 32;
    property VideoBorder3D: WordBool dispid 1103;
    property VideoBorderWidth: Integer dispid 1101;
    property VideoBorderColor: VB_OLE_COLOR dispid 1102;
    property ShowGotoBar: WordBool dispid 1088;
    property ShowStatusBar: WordBool dispid 1086;
    property ShowCaptioning: WordBool dispid 1084;
    property ShowAudioControls: WordBool dispid 1107;
    property CaptioningID: WideString dispid 1085;
    property Mute: WordBool dispid 1089;
    property CanPreview: WordBool readonly dispid 1093;
    property PreviewMode: WordBool dispid 1091;
    property HasMultipleItems: WordBool readonly dispid 1094;
    property Language: Integer dispid 1095;
    property AudioStream: Integer dispid 1096;
    property SAMIStyle: WideString dispid 1097;
    property SAMILang: WideString dispid 1098;
    property SAMIFileName: WideString dispid 1099;
    property StreamCount: Integer readonly dispid 1100;
    property ClientId: WideString readonly dispid 1106;
    property ConnectionSpeed: Integer readonly dispid 1113;
    property AutoSize: WordBool dispid -500;
    property EnableFullScreenControls: WordBool dispid 1108;
    property ActiveMovie: IDispatch readonly dispid 1109;
    property NSPlay: IDispatch readonly dispid 1110;
    property WindowlessVideo: WordBool dispid 1112;
    procedure Play; dispid 2001;
    procedure Stop; dispid 2003;
    procedure Pause; dispid 2002;
    function  GetMarkerTime(MarkerNum: Integer): Double; dispid 2004;
    function  GetMarkerName(MarkerNum: Integer): WideString; dispid 2005;
    procedure AboutBox; dispid -552;
    function  GetCodecInstalled(CodecNum: Integer): WordBool; dispid 2007;
    function  GetCodecDescription(CodecNum: Integer): WideString; dispid 2008;
    function  GetCodecURL(CodecNum: Integer): WideString; dispid 2009;
    function  GetMoreInfoURL(MoreInfoType: MPMoreInfoType): WideString; dispid 2011;
    function  GetMediaInfoString(MediaInfoType: MPMediaInfoType): WideString; dispid 2016;
    procedure Cancel; dispid 2006;
    procedure Open(const bstrFileName: WideString); dispid 2010;
    function  IsSoundCardEnabled: WordBool; dispid 53;
    procedure Next; dispid 2023;
    procedure Previous; dispid 2022;
    procedure StreamSelect(StreamNum: Integer); dispid 2015;
    procedure FastForward; dispid 2024;
    procedure FastReverse; dispid 2025;
    function  GetStreamName(StreamNum: Integer): WideString; dispid 2019;
    function  GetStreamGroup(StreamNum: Integer): Integer; dispid 2020;
    function  GetStreamSelected(StreamNum: Integer): WordBool; dispid 2021;
  end;

// *********************************************************************//
// Interface: IMediaBindStream
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {920F0DE3-91C5-11D2-828F-00C04FC99D4E}
// *********************************************************************//
  IMediaBindStream = interface(IDispatch)
    ['{920F0DE3-91C5-11D2-828F-00C04FC99D4E}']
    procedure LoadMoniker(const bszTransferContext: WideString; const bszUrl: WideString); safecall;
  end;

// *********************************************************************//
// DispIntf:  IMediaBindStreamDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {920F0DE3-91C5-11D2-828F-00C04FC99D4E}
// *********************************************************************//
  IMediaBindStreamDisp = dispinterface
    ['{920F0DE3-91C5-11D2-828F-00C04FC99D4E}']
    procedure LoadMoniker(const bszTransferContext: WideString; const bszUrl: WideString); dispid 8004;
  end;

// *********************************************************************//
// Interface: IMediaPlayerDvd
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {746EB440-3835-11D2-9774-0000F80855E6}
// *********************************************************************//
  IMediaPlayerDvd = interface(IDispatch)
    ['{746EB440-3835-11D2-9774-0000F80855E6}']
    procedure ButtonSelectAndActivate(uiButton: LongWord); safecall;
    procedure UpperButtonSelect; safecall;
    procedure LowerButtonSelect; safecall;
    procedure LeftButtonSelect; safecall;
    procedure RightButtonSelect; safecall;
    procedure ButtonActivate; safecall;
    procedure ForwardScan(dwSpeed: Double); safecall;
    procedure BackwardScan(dwSpeed: Double); safecall;
    procedure PrevPGSearch; safecall;
    procedure TopPGSearch; safecall;
    procedure NextPGSearch; safecall;
    procedure TitlePlay(uiTitle: LongWord); safecall;
    procedure ChapterPlay(uiTitle: LongWord; uiChapter: LongWord); safecall;
    procedure ChapterSearch(Chapter: LongWord); safecall;
    procedure MenuCall(MenuID: DVDMenuIDConstants); safecall;
    procedure ResumeFromMenu; safecall;
    procedure TimePlay(uiTitle: LongWord; const bstrTime: WideString); safecall;
    procedure TimeSearch(const bstrTime: WideString); safecall;
    procedure ChapterPlayAutoStop(ulTitle: LongWord; ulChapter: LongWord; ulChaptersToPlay: LongWord); safecall;
    procedure StillOff; safecall;
    procedure GoUp; safecall;
    function  Get_TotalTitleTime: WideString; safecall;
    function  GetNumberOfChapters(ulTitle: LongWord): LongWord; safecall;
    function  GetAudioLanguage(ulStream: LongWord): WideString; safecall;
    function  GetSubpictureLanguage(ulStream: LongWord): WideString; safecall;
    function  GetAllGPRMs: OleVariant; safecall;
    function  GetAllSPRMs: OleVariant; safecall;
    function  UOPValid(ulUOP: LongWord): WordBool; safecall;
    function  Get_ButtonsAvailable: LongWord; safecall;
    function  Get_CurrentButton: LongWord; safecall;
    function  Get_AudioStreamsAvailable: LongWord; safecall;
    function  Get_CurrentAudioStream: LongWord; safecall;
    procedure Set_CurrentAudioStream(ulAudioStream: LongWord); safecall;
    function  Get_CurrentSubpictureStream: LongWord; safecall;
    procedure Set_CurrentSubpictureStream(ulSubpictureStream: LongWord); safecall;
    function  Get_SubpictureStreamsAvailable: LongWord; safecall;
    function  Get_SubpictureOn: WordBool; safecall;
    procedure Set_SubpictureOn(bSubpictureON: WordBool); safecall;
    function  Get_AnglesAvailable: LongWord; safecall;
    function  Get_CurrentAngle: LongWord; safecall;
    procedure Set_CurrentAngle(ulAngle: LongWord); safecall;
    function  Get_CurrentTitle: LongWord; safecall;
    function  Get_CurrentChapter: LongWord; safecall;
    function  Get_CurrentTime: WideString; safecall;
    procedure Set_Root(const pbstrPath: WideString); safecall;
    function  Get_Root: WideString; safecall;
    function  Get_FramesPerSecond: LongWord; safecall;
    function  Get_CurrentDomain: LongWord; safecall;
    function  Get_TitlesAvailable: LongWord; safecall;
    function  Get_VolumesAvailable: LongWord; safecall;
    function  Get_CurrentVolume: LongWord; safecall;
    function  Get_CurrentDiscSide: LongWord; safecall;
    function  Get_CCActive: WordBool; safecall;
    procedure Set_CCActive(bCCActive: WordBool); safecall;
    function  Get_CurrentCCService: LongWord; safecall;
    procedure Set_CurrentCCService(pulService: LongWord); safecall;
    function  Get_UniqueID: WideString; safecall;
    function  Get_ColorKey: LongWord; safecall;
    procedure Set_ColorKey(pClr: LongWord); safecall;
    property TotalTitleTime: WideString read Get_TotalTitleTime;
    property ButtonsAvailable: LongWord read Get_ButtonsAvailable;
    property CurrentButton: LongWord read Get_CurrentButton;
    property AudioStreamsAvailable: LongWord read Get_AudioStreamsAvailable;
    property CurrentAudioStream: LongWord read Get_CurrentAudioStream write Set_CurrentAudioStream;
    property CurrentSubpictureStream: LongWord read Get_CurrentSubpictureStream write Set_CurrentSubpictureStream;
    property SubpictureStreamsAvailable: LongWord read Get_SubpictureStreamsAvailable;
    property SubpictureOn: WordBool read Get_SubpictureOn write Set_SubpictureOn;
    property AnglesAvailable: LongWord read Get_AnglesAvailable;
    property CurrentAngle: LongWord read Get_CurrentAngle write Set_CurrentAngle;
    property CurrentTitle: LongWord read Get_CurrentTitle;
    property CurrentChapter: LongWord read Get_CurrentChapter;
    property CurrentTime: WideString read Get_CurrentTime;
    property Root: WideString read Get_Root write Set_Root;
    property FramesPerSecond: LongWord read Get_FramesPerSecond;
    property CurrentDomain: LongWord read Get_CurrentDomain;
    property TitlesAvailable: LongWord read Get_TitlesAvailable;
    property VolumesAvailable: LongWord read Get_VolumesAvailable;
    property CurrentVolume: LongWord read Get_CurrentVolume;
    property CurrentDiscSide: LongWord read Get_CurrentDiscSide;
    property CCActive: WordBool read Get_CCActive write Set_CCActive;
    property CurrentCCService: LongWord read Get_CurrentCCService write Set_CurrentCCService;
    property UniqueID: WideString read Get_UniqueID;
    property ColorKey: LongWord read Get_ColorKey write Set_ColorKey;
  end;

// *********************************************************************//
// DispIntf:  IMediaPlayerDvdDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {746EB440-3835-11D2-9774-0000F80855E6}
// *********************************************************************//
  IMediaPlayerDvdDisp = dispinterface
    ['{746EB440-3835-11D2-9774-0000F80855E6}']
    procedure ButtonSelectAndActivate(uiButton: LongWord); dispid 1526;
    procedure UpperButtonSelect; dispid 1521;
    procedure LowerButtonSelect; dispid 1522;
    procedure LeftButtonSelect; dispid 1523;
    procedure RightButtonSelect; dispid 1524;
    procedure ButtonActivate; dispid 1525;
    procedure ForwardScan(dwSpeed: Double); dispid 1517;
    procedure BackwardScan(dwSpeed: Double); dispid 1518;
    procedure PrevPGSearch; dispid 1514;
    procedure TopPGSearch; dispid 1515;
    procedure NextPGSearch; dispid 1516;
    procedure TitlePlay(uiTitle: LongWord); dispid 1507;
    procedure ChapterPlay(uiTitle: LongWord; uiChapter: LongWord); dispid 1508;
    procedure ChapterSearch(Chapter: LongWord); dispid 1513;
    procedure MenuCall(MenuID: DVDMenuIDConstants); dispid 1519;
    procedure ResumeFromMenu; dispid 1520;
    procedure TimePlay(uiTitle: LongWord; const bstrTime: WideString); dispid 1509;
    procedure TimeSearch(const bstrTime: WideString); dispid 1512;
    procedure ChapterPlayAutoStop(ulTitle: LongWord; ulChapter: LongWord; ulChaptersToPlay: LongWord); dispid 1541;
    procedure StillOff; dispid 1527;
    procedure GoUp; dispid 1511;
    property TotalTitleTime: WideString readonly dispid 1582;
    function  GetNumberOfChapters(ulTitle: LongWord): LongWord; dispid 1550;
    function  GetAudioLanguage(ulStream: LongWord): WideString; dispid 1551;
    function  GetSubpictureLanguage(ulStream: LongWord): WideString; dispid 1555;
    function  GetAllGPRMs: OleVariant; dispid 1560;
    function  GetAllSPRMs: OleVariant; dispid 1559;
    function  UOPValid(ulUOP: LongWord): WordBool; dispid 1579;
    property ButtonsAvailable: LongWord readonly dispid 1571;
    property CurrentButton: LongWord readonly dispid 1570;
    property AudioStreamsAvailable: LongWord readonly dispid 1543;
    property CurrentAudioStream: LongWord dispid 1544;
    property CurrentSubpictureStream: LongWord dispid 1545;
    property SubpictureStreamsAvailable: LongWord readonly dispid 1546;
    property SubpictureOn: WordBool dispid 1547;
    property AnglesAvailable: LongWord readonly dispid 1549;
    property CurrentAngle: LongWord dispid 1548;
    property CurrentTitle: LongWord readonly dispid 1567;
    property CurrentChapter: LongWord readonly dispid 1568;
    property CurrentTime: WideString readonly dispid 1569;
    property Root: WideString dispid 1538;
    property FramesPerSecond: LongWord readonly dispid 1573;
    property CurrentDomain: LongWord readonly dispid 1574;
    property TitlesAvailable: LongWord readonly dispid 1575;
    property VolumesAvailable: LongWord readonly dispid 1576;
    property CurrentVolume: LongWord readonly dispid 1577;
    property CurrentDiscSide: LongWord readonly dispid 1578;
    property CCActive: WordBool dispid 1581;
    property CurrentCCService: LongWord dispid 1580;
    property UniqueID: WideString readonly dispid 1584;
    property ColorKey: LongWord dispid 1585;
  end;

// *********************************************************************//
// DispIntf:  _IAsyncPProtEvents
// Flags:     (4096) Dispatchable
// GUID:      {3DA2AA3C-3D96-11D2-9BD2-204C4F4F5020}
// *********************************************************************//
  _IAsyncPProtEvents = dispinterface
    ['{3DA2AA3C-3D96-11D2-9BD2-204C4F4F5020}']
  end;

// *********************************************************************//
// Interface: IAsyncPProt
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3DA2AA3A-3D96-11D2-9BD2-204C4F4F5020}
// *********************************************************************//
  IAsyncPProt = interface(IDispatch)
    ['{3DA2AA3A-3D96-11D2-9BD2-204C4F4F5020}']
  end;

// *********************************************************************//
// DispIntf:  IAsyncPProtDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3DA2AA3A-3D96-11D2-9BD2-204C4F4F5020}
// *********************************************************************//
  IAsyncPProtDisp = dispinterface
    ['{3DA2AA3A-3D96-11D2-9BD2-204C4F4F5020}']
  end;

// *********************************************************************//
// Interface: IAsyncMHandler
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3DA2AA3D-3D96-11D2-9BD2-204C4F4F5020}
// *********************************************************************//
  IAsyncMHandler = interface(IDispatch)
    ['{3DA2AA3D-3D96-11D2-9BD2-204C4F4F5020}']
  end;

// *********************************************************************//
// DispIntf:  IAsyncMHandlerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3DA2AA3D-3D96-11D2-9BD2-204C4F4F5020}
// *********************************************************************//
  IAsyncMHandlerDisp = dispinterface
    ['{3DA2AA3D-3D96-11D2-9BD2-204C4F4F5020}']
  end;

// *********************************************************************//
// DispIntf:  _IRadioPlayerEvents
// Flags:     (4096) Dispatchable
// GUID:      {9C2263B1-3E3C-11D2-9BD3-204C4F4F5020}
// *********************************************************************//
  _IRadioPlayerEvents = dispinterface
    ['{9C2263B1-3E3C-11D2-9BD3-204C4F4F5020}']
    procedure StateChange(const bszUrl: WideString; fPlay: WordBool; lVolume: Integer; 
                          fMute: WordBool); dispid 12;
  end;

// *********************************************************************//
// Interface: IRadioPlayer
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9C2263AF-3E3C-11D2-9BD3-204C4F4F5020}
// *********************************************************************//
  IRadioPlayer = interface(IDispatch)
    ['{9C2263AF-3E3C-11D2-9BD3-204C4F4F5020}']
    procedure BindRadioMemory; safecall;
    procedure ReleaseRadio; safecall;
    function  RegisterEvent(const bszEvent: WideString): Integer; safecall;
    function  RegisterWindow(lHWND: Integer; dwMessage: LongWord; dwCodeSet: LongWord): Integer; safecall;
    function  GetSection: WideString; safecall;
    procedure Unregister(lRegister: Integer); safecall;
    function  GetInstanceCount: Integer; safecall;
    procedure Play; safecall;
    procedure Stop; safecall;
    procedure Set_Url(const Param1: WideString); safecall;
    procedure Set_Volume(Param1: Integer); safecall;
    procedure Set_Mute(Param1: WordBool); safecall;
    procedure GetStatus(out plVolume: Integer; out pfMute: Integer; out pfPlay: Integer; 
                        out __MIDL_0012: WideString; out __MIDL_0013: WideString; 
                        out __MIDL_0014: WideString; out __MIDL_0015: WideString; 
                        out __MIDL_0016: WideString; out __MIDL_0017: WideString; 
                        out __MIDL_0018: WideString); safecall;
    procedure GetState(out plOpenState: Integer; out pfBuffering: Integer; 
                       out plBufferingPercent: Integer; out plQuality: Integer); safecall;
    property Url: WideString write Set_Url;
    property Volume: Integer write Set_Volume;
    property Mute: WordBool write Set_Mute;
  end;

// *********************************************************************//
// DispIntf:  IRadioPlayerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9C2263AF-3E3C-11D2-9BD3-204C4F4F5020}
// *********************************************************************//
  IRadioPlayerDisp = dispinterface
    ['{9C2263AF-3E3C-11D2-9BD3-204C4F4F5020}']
    procedure BindRadioMemory; dispid 17;
    procedure ReleaseRadio; dispid 18;
    function  RegisterEvent(const bszEvent: WideString): Integer; dispid 5;
    function  RegisterWindow(lHWND: Integer; dwMessage: LongWord; dwCodeSet: LongWord): Integer; dispid 4;
    function  GetSection: WideString; dispid 9;
    procedure Unregister(lRegister: Integer); dispid 6;
    function  GetInstanceCount: Integer; dispid 20;
    procedure Play; dispid 7;
    procedure Stop; dispid 8;
    property Url: WideString writeonly dispid 1;
    property Volume: Integer writeonly dispid 2;
    property Mute: WordBool writeonly dispid 3;
    procedure GetStatus(out plVolume: Integer; out pfMute: Integer; out pfPlay: Integer; 
                        out __MIDL_0012: WideString; out __MIDL_0013: WideString; 
                        out __MIDL_0014: WideString; out __MIDL_0015: WideString; 
                        out __MIDL_0016: WideString; out __MIDL_0017: WideString; 
                        out __MIDL_0018: WideString); dispid 11;
    procedure GetState(out plOpenState: Integer; out pfBuffering: Integer; 
                       out plBufferingPercent: Integer; out plQuality: Integer); dispid 19;
  end;

// *********************************************************************//
// Interface: IRadioServer
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9C2263A0-3E3C-11D2-9BD3-204C4F4F5020}
// *********************************************************************//
  IRadioServer = interface(IDispatch)
    ['{9C2263A0-3E3C-11D2-9BD3-204C4F4F5020}']
    function  BindToRadio(const wszRadio: WideString): IRadioPlayer; safecall;
    procedure IsRadioExists(const wszRadio: WideString); safecall;
    procedure LaunchStandardUrl(const bszUrl: WideString; const pBrowser: IUnknown); safecall;
  end;

// *********************************************************************//
// DispIntf:  IRadioServerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9C2263A0-3E3C-11D2-9BD3-204C4F4F5020}
// *********************************************************************//
  IRadioServerDisp = dispinterface
    ['{9C2263A0-3E3C-11D2-9BD3-204C4F4F5020}']
    function  BindToRadio(const wszRadio: WideString): IRadioPlayer; dispid 10;
    procedure IsRadioExists(const wszRadio: WideString); dispid 21;
    procedure LaunchStandardUrl(const bszUrl: WideString; const pBrowser: IUnknown); dispid 22;
  end;

// *********************************************************************//
// Interface: IRadioServerControl
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8E718889-423F-11D2-876E-00A0C9082467}
// *********************************************************************//
  IRadioServerControl = interface(IDispatch)
    ['{8E718889-423F-11D2-876E-00A0C9082467}']
  end;

// *********************************************************************//
// DispIntf:  IRadioServerControlDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8E718889-423F-11D2-876E-00A0C9082467}
// *********************************************************************//
  IRadioServerControlDisp = dispinterface
    ['{8E718889-423F-11D2-876E-00A0C9082467}']
  end;

// *********************************************************************//
// Interface: IMediaPlayerListener
// Flags:     (0)
// GUID:      {33222211-5E5E-11D2-9E8E-0000F8085981}
// *********************************************************************//
  IMediaPlayerListener = interface(IUnknown)
    ['{33222211-5E5E-11D2-9E8E-0000F8085981}']
    function  PlayStateChanged(lNewState: Integer): HResult; stdcall;
    function  Buffering(fStart: WordBool): HResult; stdcall;
    function  BufferPercent(lBufferPercent: Integer): HResult; stdcall;
    function  OpenStateChanged(lOpenState: Integer): HResult; stdcall;
    function  MediaInfoChanged(const bstrShowTitle: WideString; const bstrClipTitle: WideString; 
                               const bstrClipAuthor: WideString; 
                               const bstrClipCopyright: WideString; const bstrStationURL: WideString): HResult; stdcall;
    function  QualityChanged(lQuality: Integer): HResult; stdcall;
    function  Error(const bstrError: WideString): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRadioBand
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8E718881-423F-11D2-876E-00A0C9082467}
// *********************************************************************//
  IRadioBand = interface(IDispatch)
    ['{8E718881-423F-11D2-876E-00A0C9082467}']
    procedure Create(var phwnd: Integer; hwndParent: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IRadioBandDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8E718881-423F-11D2-876E-00A0C9082467}
// *********************************************************************//
  IRadioBandDisp = dispinterface
    ['{8E718881-423F-11D2-876E-00A0C9082467}']
    procedure Create(var phwnd: Integer; hwndParent: Integer); dispid 1610743808;
  end;

// *********************************************************************//
// The Class CoDirectControl provides a Create and CreateRemote method to          
// create instances of the default interface IDirectControl exposed by              
// the CoClass DirectControl. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoDirectControl = class
    class function Create: IDirectControl;
    class function CreateRemote(const MachineName: AnsiString): IDirectControl;
  end;

// *********************************************************************//
// The Class CoDirectContainer provides a Create and CreateRemote method to          
// create instances of the default interface IDirectContainer exposed by              
// the CoClass DirectContainer. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoDirectContainer = class
    class function Create: IDirectContainer;
    class function CreateRemote(const MachineName: AnsiString): IDirectContainer;
  end;

// *********************************************************************//
// The Class CoRadioView provides a Create and CreateRemote method to          
// create instances of the default interface IRadioView exposed by              
// the CoClass RadioView. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRadioView = class
    class function Create: IRadioView;
    class function CreateRemote(const MachineName: AnsiString): IRadioView;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TWMP
// Help String_     : Windows Media Player
// Default Interface: IMediaPlayer2
// Def. Intf. DISP? : No
// Event   Interface: _MediaPlayerEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TWMPDVDNotify = procedure(Sender: TObject; EventCode: Integer; EventParam1: Integer; 
                                             EventParam2: Integer) of object;
  TWMPEndOfStream = procedure(Sender: TObject; Result: Integer) of object;
  TWMPOpenStateChange = procedure(Sender: TObject; OldState: Integer; NewState: Integer) of object;
  TWMPPlayStateChange = procedure(Sender: TObject; OldState: Integer; NewState: Integer) of object;
  TWMPScriptCommand = procedure(Sender: TObject; const scType: WideString; const Param: WideString) of object;
  TWMPBuffering = procedure(Sender: TObject; Start: WordBool) of object;
  TWMPMarkerHit = procedure(Sender: TObject; MarkerNum: Integer) of object;
  TWMPWarning = procedure(Sender: TObject; WarningType: Integer; Param: Integer; 
                                           const Description: WideString) of object;
  TWMPDisconnect = procedure(Sender: TObject; Result: Integer) of object;
  TWMPPositionChange = procedure(Sender: TObject; oldPosition: Double; newPosition: Double) of object;
  TWMPReadyStateChange = procedure(Sender: TObject; ReadyState: ReadyStateConstants) of object;

  TWMP = class(TOleControl)
  private
    FOnDVDNotify: TWMPDVDNotify;
    FOnEndOfStream: TWMPEndOfStream;
    FOnOpenStateChange: TWMPOpenStateChange;
    FOnPlayStateChange: TWMPPlayStateChange;
    FOnScriptCommand: TWMPScriptCommand;
    FOnBuffering: TWMPBuffering;
    FOnError: TNotifyEvent;
    FOnMarkerHit: TWMPMarkerHit;
    FOnWarning: TWMPWarning;
    FOnNewStream: TNotifyEvent;
    FOnDisconnect: TWMPDisconnect;
    FOnPositionChange: TWMPPositionChange;
    FOnDisplayModeChange: TNotifyEvent;
    FOnReadyStateChange: TWMPReadyStateChange;
    FIntf: IMediaPlayer2;
    function  GetControlInterface: IMediaPlayer2;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function  Get_ActiveMovie: IDispatch;
    function  Get_NSPlay: IDispatch;
    function  Get_DVD: IMediaPlayerDvd;
  public
    procedure Play;
    procedure Stop;
    procedure Pause;
    function  GetMarkerTime(MarkerNum: Integer): Double;
    function  GetMarkerName(MarkerNum: Integer): WideString;
    procedure AboutBox;
    function  GetCodecInstalled(CodecNum: Integer): WordBool;
    function  GetCodecDescription(CodecNum: Integer): WideString;
    function  GetCodecURL(CodecNum: Integer): WideString;
    function  GetMoreInfoURL(MoreInfoType: MPMoreInfoType): WideString;
    function  GetMediaInfoString(MediaInfoType: MPMediaInfoType): WideString;
    procedure Cancel;
    procedure Open(const bstrFileName: WideString);
    function  IsSoundCardEnabled: WordBool;
    procedure Next;
    procedure Previous;
    procedure StreamSelect(StreamNum: Integer);
    procedure FastForward;
    procedure FastReverse;
    function  GetStreamName(StreamNum: Integer): WideString;
    function  GetStreamGroup(StreamNum: Integer): Integer;
    function  GetStreamSelected(StreamNum: Integer): WordBool;
    function  GetMediaParameter(EntryNum: Integer; const bstrParameterName: WideString): WideString;
    function  GetMediaParameterName(EntryNum: Integer; Index: Integer): WideString;
    function  GetCurrentEntry: Integer;
    procedure SetCurrentEntry(EntryNumber: Integer);
    procedure ShowDialog(mpDialogIndex: MPShowDialogConstants);
    property  ControlInterface: IMediaPlayer2 read GetControlInterface;
    property  DefaultInterface: IMediaPlayer2 read GetControlInterface;
    property Duration: Double index 1003 read GetDoubleProp;
    property ImageSourceWidth: Integer index 1001 read GetIntegerProp;
    property ImageSourceHeight: Integer index 1002 read GetIntegerProp;
    property MarkerCount: Integer index 1010 read GetIntegerProp;
    property CanScan: WordBool index 1011 read GetWordBoolProp;
    property CanSeek: WordBool index 1012 read GetWordBoolProp;
    property CanSeekToMarkers: WordBool index 1047 read GetWordBoolProp;
    property SourceLink: WideString index 1009 read GetWideStringProp;
    property CreationDate: TDateTime index 1036 read GetTDateTimeProp;
    property ErrorCorrection: WideString index 1038 read GetWideStringProp;
    property Bandwidth: Integer index 1037 read GetIntegerProp;
    property SourceProtocol: Integer index 1060 read GetIntegerProp;
    property ReceivedPackets: Integer index 1039 read GetIntegerProp;
    property RecoveredPackets: Integer index 1040 read GetIntegerProp;
    property LostPackets: Integer index 1041 read GetIntegerProp;
    property ReceptionQuality: Integer index 1042 read GetIntegerProp;
    property BufferingCount: Integer index 1043 read GetIntegerProp;
    property IsBroadcast: WordBool index 1058 read GetWordBoolProp;
    property BufferingProgress: Integer index 1080 read GetIntegerProp;
    property ChannelName: WideString index 1050 read GetWideStringProp;
    property ChannelDescription: WideString index 1051 read GetWideStringProp;
    property ChannelURL: WideString index 1052 read GetWideStringProp;
    property ContactAddress: WideString index 1053 read GetWideStringProp;
    property ContactPhone: WideString index 1054 read GetWideStringProp;
    property ContactEmail: WideString index 1055 read GetWideStringProp;
    property CodecCount: Integer index 1057 read GetIntegerProp;
    property IsDurationValid: WordBool index 1059 read GetWordBoolProp;
    property OpenState: Integer index 1061 read GetIntegerProp;
    property PlayState: TOleEnum index 1068 read GetTOleEnumProp;
    property HasError: WordBool index 1065 read GetWordBoolProp;
    property ErrorDescription: WideString index 1066 read GetWideStringProp;
    property ErrorCode: Integer index 1067 read GetIntegerProp;
    property ReadyState: TOleEnum index -525 read GetTOleEnumProp;
    property CanPreview: WordBool index 1093 read GetWordBoolProp;
    property HasMultipleItems: WordBool index 1094 read GetWordBoolProp;
    property StreamCount: Integer index 1100 read GetIntegerProp;
    property ClientId: WideString index 1106 read GetWideStringProp;
    property ConnectionSpeed: Integer index 1113 read GetIntegerProp;
    property ActiveMovie: IDispatch index 1109 read GetIDispatchProp;
    property NSPlay: IDispatch index 1110 read GetIDispatchProp;
    property DVD: IMediaPlayerDvd read Get_DVD;
    property EntryCount: Integer index 2030 read GetIntegerProp;
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
    property  OnMouseUp;
    property  OnMouseMove;
    property  OnMouseDown;
    property  OnKeyUp;
    property  OnKeyPress;
    property  OnKeyDown;
    property  OnDblClick;
    property  OnClick;
    property CurrentPosition: Double index 1027 read GetDoubleProp write SetDoubleProp stored False;
    property CurrentMarker: Integer index 1029 read GetIntegerProp write SetIntegerProp stored False;
    property FileName: WideString index 1026 read GetWideStringProp write SetWideStringProp stored False;
    property BufferingTime: Double index 1070 read GetDoubleProp write SetDoubleProp stored False;
    property AutoStart: WordBool index 1017 read GetWordBoolProp write SetWordBoolProp stored False;
    property AutoRewind: WordBool index 1018 read GetWordBoolProp write SetWordBoolProp stored False;
    property Rate: Double index 1028 read GetDoubleProp write SetDoubleProp stored False;
    property SendKeyboardEvents: WordBool index 1013 read GetWordBoolProp write SetWordBoolProp stored False;
    property SendMouseClickEvents: WordBool index 1014 read GetWordBoolProp write SetWordBoolProp stored False;
    property SendMouseMoveEvents: WordBool index 1015 read GetWordBoolProp write SetWordBoolProp stored False;
    property PlayCount: Integer index 1030 read GetIntegerProp write SetIntegerProp stored False;
    property ClickToPlay: WordBool index 1025 read GetWordBoolProp write SetWordBoolProp stored False;
    property AllowScan: WordBool index 1035 read GetWordBoolProp write SetWordBoolProp stored False;
    property EnableContextMenu: WordBool index 1021 read GetWordBoolProp write SetWordBoolProp stored False;
    property CursorType: Integer index 1044 read GetIntegerProp write SetIntegerProp stored False;
    property AllowChangeDisplaySize: WordBool index 1056 read GetWordBoolProp write SetWordBoolProp stored False;
    property SendOpenStateChangeEvents: WordBool index 1062 read GetWordBoolProp write SetWordBoolProp stored False;
    property SendWarningEvents: WordBool index 1063 read GetWordBoolProp write SetWordBoolProp stored False;
    property SendErrorEvents: WordBool index 1064 read GetWordBoolProp write SetWordBoolProp stored False;
    property SendPlayStateChangeEvents: WordBool index 1069 read GetWordBoolProp write SetWordBoolProp stored False;
    property DisplaySize: TOleEnum index 1032 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property InvokeURLs: WordBool index 1020 read GetWordBoolProp write SetWordBoolProp stored False;
    property BaseURL: WideString index 1082 read GetWideStringProp write SetWideStringProp stored False;
    property DefaultFrame: WideString index 1083 read GetWideStringProp write SetWideStringProp stored False;
    property AnimationAtStart: WordBool index 1045 read GetWordBoolProp write SetWordBoolProp stored False;
    property TransparentAtStart: WordBool index 1022 read GetWordBoolProp write SetWordBoolProp stored False;
    property Volume: Integer index 19 read GetIntegerProp write SetIntegerProp stored False;
    property Balance: Integer index 20 read GetIntegerProp write SetIntegerProp stored False;
    property SelectionStart: Double index 15 read GetDoubleProp write SetDoubleProp stored False;
    property SelectionEnd: Double index 16 read GetDoubleProp write SetDoubleProp stored False;
    property ShowDisplay: WordBool index 22 read GetWordBoolProp write SetWordBoolProp stored False;
    property ShowControls: WordBool index 23 read GetWordBoolProp write SetWordBoolProp stored False;
    property ShowPositionControls: WordBool index 24 read GetWordBoolProp write SetWordBoolProp stored False;
    property ShowTracker: WordBool index 26 read GetWordBoolProp write SetWordBoolProp stored False;
    property EnablePositionControls: WordBool index 27 read GetWordBoolProp write SetWordBoolProp stored False;
    property EnableTracker: WordBool index 29 read GetWordBoolProp write SetWordBoolProp stored False;
    property Enabled: WordBool index -514 read GetWordBoolProp write SetWordBoolProp stored False;
    property DisplayForeColor: TColor index 36 read GetTColorProp write SetTColorProp stored False;
    property DisplayBackColor: TColor index 37 read GetTColorProp write SetTColorProp stored False;
    property DisplayMode: TOleEnum index 32 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property VideoBorder3D: WordBool index 1103 read GetWordBoolProp write SetWordBoolProp stored False;
    property VideoBorderWidth: Integer index 1101 read GetIntegerProp write SetIntegerProp stored False;
    property VideoBorderColor: TColor index 1102 read GetTColorProp write SetTColorProp stored False;
    property ShowGotoBar: WordBool index 1088 read GetWordBoolProp write SetWordBoolProp stored False;
    property ShowStatusBar: WordBool index 1086 read GetWordBoolProp write SetWordBoolProp stored False;
    property ShowCaptioning: WordBool index 1084 read GetWordBoolProp write SetWordBoolProp stored False;
    property ShowAudioControls: WordBool index 1107 read GetWordBoolProp write SetWordBoolProp stored False;
    property CaptioningID: WideString index 1085 read GetWideStringProp write SetWideStringProp stored False;
    property Mute: WordBool index 1089 read GetWordBoolProp write SetWordBoolProp stored False;
    property PreviewMode: WordBool index 1091 read GetWordBoolProp write SetWordBoolProp stored False;
    property Language: Integer index 1095 read GetIntegerProp write SetIntegerProp stored False;
    property AudioStream: Integer index 1096 read GetIntegerProp write SetIntegerProp stored False;
    property SAMIStyle: WideString index 1097 read GetWideStringProp write SetWideStringProp stored False;
    property SAMILang: WideString index 1098 read GetWideStringProp write SetWideStringProp stored False;
    property SAMIFileName: WideString index 1099 read GetWideStringProp write SetWideStringProp stored False;
    property AutoSize: WordBool index -500 read GetWordBoolProp write SetWordBoolProp stored False;
    property EnableFullScreenControls: WordBool index 1108 read GetWordBoolProp write SetWordBoolProp stored False;
    property WindowlessVideo: WordBool index 1112 read GetWordBoolProp write SetWordBoolProp stored False;
    property OnDVDNotify: TWMPDVDNotify read FOnDVDNotify write FOnDVDNotify;
    property OnEndOfStream: TWMPEndOfStream read FOnEndOfStream write FOnEndOfStream;
    property OnOpenStateChange: TWMPOpenStateChange read FOnOpenStateChange write FOnOpenStateChange;
    property OnPlayStateChange: TWMPPlayStateChange read FOnPlayStateChange write FOnPlayStateChange;
    property OnScriptCommand: TWMPScriptCommand read FOnScriptCommand write FOnScriptCommand;
    property OnBuffering: TWMPBuffering read FOnBuffering write FOnBuffering;
    property OnError: TNotifyEvent read FOnError write FOnError;
    property OnMarkerHit: TWMPMarkerHit read FOnMarkerHit write FOnMarkerHit;
    property OnWarning: TWMPWarning read FOnWarning write FOnWarning;
    property OnNewStream: TNotifyEvent read FOnNewStream write FOnNewStream;
    property OnDisconnect: TWMPDisconnect read FOnDisconnect write FOnDisconnect;
    property OnPositionChange: TWMPPositionChange read FOnPositionChange write FOnPositionChange;
    property OnDisplayModeChange: TNotifyEvent read FOnDisplayModeChange write FOnDisplayModeChange;
    property OnReadyStateChange: TWMPReadyStateChange read FOnReadyStateChange write FOnReadyStateChange;
  end;

// *********************************************************************//
// The Class CoppDShowNet provides a Create and CreateRemote method to          
// create instances of the default interface IUnknown exposed by              
// the CoClass ppDShowNet. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoppDShowNet = class
    class function Create: IUnknown;
    class function CreateRemote(const MachineName: AnsiString): IUnknown;
  end;

// *********************************************************************//
// The Class CoppDShowPlay provides a Create and CreateRemote method to          
// create instances of the default interface IUnknown exposed by              
// the CoClass ppDShowPlay. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoppDShowPlay = class
    class function Create: IUnknown;
    class function CreateRemote(const MachineName: AnsiString): IUnknown;
  end;

// *********************************************************************//
// The Class CoppDSMeta provides a Create and CreateRemote method to          
// create instances of the default interface IUnknown exposed by              
// the CoClass ppDSMeta. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoppDSMeta = class
    class function Create: IUnknown;
    class function CreateRemote(const MachineName: AnsiString): IUnknown;
  end;

// *********************************************************************//
// The Class CoppDSCnnl provides a Create and CreateRemote method to          
// create instances of the default interface IUnknown exposed by              
// the CoClass ppDSCnnl. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoppDSCnnl = class
    class function Create: IUnknown;
    class function CreateRemote(const MachineName: AnsiString): IUnknown;
  end;

// *********************************************************************//
// The Class CoppDSClip provides a Create and CreateRemote method to          
// create instances of the default interface IUnknown exposed by              
// the CoClass ppDSClip. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoppDSClip = class
    class function Create: IUnknown;
    class function CreateRemote(const MachineName: AnsiString): IUnknown;
  end;

// *********************************************************************//
// The Class CoppDSDetl provides a Create and CreateRemote method to          
// create instances of the default interface IUnknown exposed by              
// the CoClass ppDSDetl. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoppDSDetl = class
    class function Create: IUnknown;
    class function CreateRemote(const MachineName: AnsiString): IUnknown;
  end;

// *********************************************************************//
// The Class CoppDSApp provides a Create and CreateRemote method to          
// create instances of the default interface IUnknown exposed by              
// the CoClass ppDSApp. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoppDSApp = class
    class function Create: IUnknown;
    class function CreateRemote(const MachineName: AnsiString): IUnknown;
  end;

// *********************************************************************//
// The Class CoppDSPropAdv provides a Create and CreateRemote method to          
// create instances of the default interface IUnknown exposed by              
// the CoClass ppDSPropAdv. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoppDSPropAdv = class
    class function Create: IUnknown;
    class function CreateRemote(const MachineName: AnsiString): IUnknown;
  end;

// *********************************************************************//
// The Class CoppDSView provides a Create and CreateRemote method to          
// create instances of the default interface IUnknown exposed by              
// the CoClass ppDSView. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoppDSView = class
    class function Create: IUnknown;
    class function CreateRemote(const MachineName: AnsiString): IUnknown;
  end;

// *********************************************************************//
// The Class CoppDSOAdv provides a Create and CreateRemote method to          
// create instances of the default interface IUnknown exposed by              
// the CoClass ppDSOAdv. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoppDSOAdv = class
    class function Create: IUnknown;
    class function CreateRemote(const MachineName: AnsiString): IUnknown;
  end;

// *********************************************************************//
// The Class CoAsyncPProt provides a Create and CreateRemote method to          
// create instances of the default interface IAsyncPProt exposed by              
// the CoClass AsyncPProt. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoAsyncPProt = class
    class function Create: IAsyncPProt;
    class function CreateRemote(const MachineName: AnsiString): IAsyncPProt;
  end;

// *********************************************************************//
// The Class CoAsyncMHandler provides a Create and CreateRemote method to          
// create instances of the default interface IAsyncMHandler exposed by              
// the CoClass AsyncMHandler. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoAsyncMHandler = class
    class function Create: IAsyncMHandler;
    class function CreateRemote(const MachineName: AnsiString): IAsyncMHandler;
  end;

// *********************************************************************//
// The Class CoRadioPlayer provides a Create and CreateRemote method to          
// create instances of the default interface IRadioPlayer exposed by              
// the CoClass RadioPlayer. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRadioPlayer = class
    class function Create: IRadioPlayer;
    class function CreateRemote(const MachineName: AnsiString): IRadioPlayer;
  end;

// *********************************************************************//
// The Class CoRadioServer provides a Create and CreateRemote method to          
// create instances of the default interface IRadioPlayer exposed by              
// the CoClass RadioServer. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRadioServer = class
    class function Create: IRadioPlayer;
    class function CreateRemote(const MachineName: AnsiString): IRadioPlayer;
  end;

// *********************************************************************//
// The Class CoRadioBand provides a Create and CreateRemote method to          
// create instances of the default interface IRadioBand exposed by              
// the CoClass RadioBand. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRadioBand = class
    class function Create: IRadioBand;
    class function CreateRemote(const MachineName: AnsiString): IRadioBand;
  end;

// *********************************************************************//
// The Class CoppDSFile provides a Create and CreateRemote method to          
// create instances of the default interface IUnknown exposed by              
// the CoClass ppDSFile. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoppDSFile = class
    class function Create: IUnknown;
    class function CreateRemote(const MachineName: AnsiString): IUnknown;
  end;

procedure Register;

implementation

uses ComObj;

class function CoDirectControl.Create: IDirectControl;
begin
  Result := CreateComObject(CLASS_DirectControl) as IDirectControl;
end;

class function CoDirectControl.CreateRemote(const MachineName: AnsiString): IDirectControl;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DirectControl) as IDirectControl;
end;

class function CoDirectContainer.Create: IDirectContainer;
begin
  Result := CreateComObject(CLASS_DirectContainer) as IDirectContainer;
end;

class function CoDirectContainer.CreateRemote(const MachineName: AnsiString): IDirectContainer;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DirectContainer) as IDirectContainer;
end;

class function CoRadioView.Create: IRadioView;
begin
  Result := CreateComObject(CLASS_RadioView) as IRadioView;
end;

class function CoRadioView.CreateRemote(const MachineName: AnsiString): IRadioView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RadioView) as IRadioView;
end;

procedure TWMP.InitControlData;
const
  CEventDispIDs: array [0..13] of DWORD = (
    $000005E1, $00000BBA, $00000BC3, $00000BC4, $00000BB9, $00000BBB,
    $00000BC2, $00000BBE, $00000BC1, $00000BC0, $00000BBC, $00000002,
    $00000033, $FFFFFD9F);
  CControlData: TControlData2 = (
    ClassID: '{22D6F312-B0F6-11D0-94AB-0080C74C7E95}';
    EventIID: '{2D3A4C40-E711-11D0-94AB-0080C74C7E95}';
    EventCount: 14;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$00000000*);
    Flags: $00000008;
    Version: 401);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnDVDNotify) - Cardinal(Self);
end;

procedure TWMP.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IMediaPlayer2;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TWMP.GetControlInterface: IMediaPlayer2;
begin
  CreateControl;
  Result := FIntf;
end;

function  TWMP.Get_ActiveMovie: IDispatch;
begin
  Result := DefaultInterface.Get_ActiveMovie;
end;

function  TWMP.Get_NSPlay: IDispatch;
begin
  Result := DefaultInterface.Get_NSPlay;
end;

function  TWMP.Get_DVD: IMediaPlayerDvd;
begin
  Result := DefaultInterface.Get_DVD;
end;

procedure TWMP.Play;
begin
  DefaultInterface.Play;
end;

procedure TWMP.Stop;
begin
  DefaultInterface.Stop;
end;

procedure TWMP.Pause;
begin
  DefaultInterface.Pause;
end;

function  TWMP.GetMarkerTime(MarkerNum: Integer): Double;
begin
  Result := DefaultInterface.GetMarkerTime(MarkerNum);
end;

function  TWMP.GetMarkerName(MarkerNum: Integer): WideString;
begin
  Result := DefaultInterface.GetMarkerName(MarkerNum);
end;

procedure TWMP.AboutBox;
begin
  DefaultInterface.AboutBox;
end;

function  TWMP.GetCodecInstalled(CodecNum: Integer): WordBool;
begin
  Result := DefaultInterface.GetCodecInstalled(CodecNum);
end;

function  TWMP.GetCodecDescription(CodecNum: Integer): WideString;
begin
  Result := DefaultInterface.GetCodecDescription(CodecNum);
end;

function  TWMP.GetCodecURL(CodecNum: Integer): WideString;
begin
  Result := DefaultInterface.GetCodecURL(CodecNum);
end;

function  TWMP.GetMoreInfoURL(MoreInfoType: MPMoreInfoType): WideString;
begin
  Result := DefaultInterface.GetMoreInfoURL(MoreInfoType);
end;

function  TWMP.GetMediaInfoString(MediaInfoType: MPMediaInfoType): WideString;
begin
  Result := DefaultInterface.GetMediaInfoString(MediaInfoType);
end;

procedure TWMP.Cancel;
begin
  DefaultInterface.Cancel;
end;

procedure TWMP.Open(const bstrFileName: WideString);
begin
  DefaultInterface.Open(bstrFileName);
end;

function  TWMP.IsSoundCardEnabled: WordBool;
begin
  Result := DefaultInterface.IsSoundCardEnabled;
end;

procedure TWMP.Next;
begin
  DefaultInterface.Next;
end;

procedure TWMP.Previous;
begin
  DefaultInterface.Previous;
end;

procedure TWMP.StreamSelect(StreamNum: Integer);
begin
  DefaultInterface.StreamSelect(StreamNum);
end;

procedure TWMP.FastForward;
begin
  DefaultInterface.FastForward;
end;

procedure TWMP.FastReverse;
begin
  DefaultInterface.FastReverse;
end;

function  TWMP.GetStreamName(StreamNum: Integer): WideString;
begin
  Result := DefaultInterface.GetStreamName(StreamNum);
end;

function  TWMP.GetStreamGroup(StreamNum: Integer): Integer;
begin
  Result := DefaultInterface.GetStreamGroup(StreamNum);
end;

function  TWMP.GetStreamSelected(StreamNum: Integer): WordBool;
begin
  Result := DefaultInterface.GetStreamSelected(StreamNum);
end;

function  TWMP.GetMediaParameter(EntryNum: Integer; const bstrParameterName: WideString): WideString;
begin
  Result := DefaultInterface.GetMediaParameter(EntryNum, bstrParameterName);
end;

function  TWMP.GetMediaParameterName(EntryNum: Integer; Index: Integer): WideString;
begin
  Result := DefaultInterface.GetMediaParameterName(EntryNum, Index);
end;

function  TWMP.GetCurrentEntry: Integer;
begin
  Result := DefaultInterface.GetCurrentEntry;
end;

procedure TWMP.SetCurrentEntry(EntryNumber: Integer);
begin
  DefaultInterface.SetCurrentEntry(EntryNumber);
end;

procedure TWMP.ShowDialog(mpDialogIndex: MPShowDialogConstants);
begin
  DefaultInterface.ShowDialog(mpDialogIndex);
end;

class function CoppDShowNet.Create: IUnknown;
begin
  Result := CreateComObject(CLASS_ppDShowNet) as IUnknown;
end;

class function CoppDShowNet.CreateRemote(const MachineName: AnsiString): IUnknown;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ppDShowNet) as IUnknown;
end;

class function CoppDShowPlay.Create: IUnknown;
begin
  Result := CreateComObject(CLASS_ppDShowPlay) as IUnknown;
end;

class function CoppDShowPlay.CreateRemote(const MachineName: AnsiString): IUnknown;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ppDShowPlay) as IUnknown;
end;

class function CoppDSMeta.Create: IUnknown;
begin
  Result := CreateComObject(CLASS_ppDSMeta) as IUnknown;
end;

class function CoppDSMeta.CreateRemote(const MachineName: AnsiString): IUnknown;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ppDSMeta) as IUnknown;
end;

class function CoppDSCnnl.Create: IUnknown;
begin
  Result := CreateComObject(CLASS_ppDSCnnl) as IUnknown;
end;

class function CoppDSCnnl.CreateRemote(const MachineName: AnsiString): IUnknown;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ppDSCnnl) as IUnknown;
end;

class function CoppDSClip.Create: IUnknown;
begin
  Result := CreateComObject(CLASS_ppDSClip) as IUnknown;
end;

class function CoppDSClip.CreateRemote(const MachineName: AnsiString): IUnknown;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ppDSClip) as IUnknown;
end;

class function CoppDSDetl.Create: IUnknown;
begin
  Result := CreateComObject(CLASS_ppDSDetl) as IUnknown;
end;

class function CoppDSDetl.CreateRemote(const MachineName: AnsiString): IUnknown;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ppDSDetl) as IUnknown;
end;

class function CoppDSApp.Create: IUnknown;
begin
  Result := CreateComObject(CLASS_ppDSApp) as IUnknown;
end;

class function CoppDSApp.CreateRemote(const MachineName: AnsiString): IUnknown;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ppDSApp) as IUnknown;
end;

class function CoppDSPropAdv.Create: IUnknown;
begin
  Result := CreateComObject(CLASS_ppDSPropAdv) as IUnknown;
end;

class function CoppDSPropAdv.CreateRemote(const MachineName: AnsiString): IUnknown;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ppDSPropAdv) as IUnknown;
end;

class function CoppDSView.Create: IUnknown;
begin
  Result := CreateComObject(CLASS_ppDSView) as IUnknown;
end;

class function CoppDSView.CreateRemote(const MachineName: AnsiString): IUnknown;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ppDSView) as IUnknown;
end;

class function CoppDSOAdv.Create: IUnknown;
begin
  Result := CreateComObject(CLASS_ppDSOAdv) as IUnknown;
end;

class function CoppDSOAdv.CreateRemote(const MachineName: AnsiString): IUnknown;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ppDSOAdv) as IUnknown;
end;

class function CoAsyncPProt.Create: IAsyncPProt;
begin
  Result := CreateComObject(CLASS_AsyncPProt) as IAsyncPProt;
end;

class function CoAsyncPProt.CreateRemote(const MachineName: AnsiString): IAsyncPProt;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_AsyncPProt) as IAsyncPProt;
end;

class function CoAsyncMHandler.Create: IAsyncMHandler;
begin
  Result := CreateComObject(CLASS_AsyncMHandler) as IAsyncMHandler;
end;

class function CoAsyncMHandler.CreateRemote(const MachineName: AnsiString): IAsyncMHandler;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_AsyncMHandler) as IAsyncMHandler;
end;

class function CoRadioPlayer.Create: IRadioPlayer;
begin
  Result := CreateComObject(CLASS_RadioPlayer) as IRadioPlayer;
end;

class function CoRadioPlayer.CreateRemote(const MachineName: AnsiString): IRadioPlayer;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RadioPlayer) as IRadioPlayer;
end;

class function CoRadioServer.Create: IRadioPlayer;
begin
  Result := CreateComObject(CLASS_RadioServer) as IRadioPlayer;
end;

class function CoRadioServer.CreateRemote(const MachineName: AnsiString): IRadioPlayer;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RadioServer) as IRadioPlayer;
end;

class function CoRadioBand.Create: IRadioBand;
begin
  Result := CreateComObject(CLASS_RadioBand) as IRadioBand;
end;

class function CoRadioBand.CreateRemote(const MachineName: AnsiString): IRadioBand;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RadioBand) as IRadioBand;
end;

class function CoppDSFile.Create: IUnknown;
begin
  Result := CreateComObject(CLASS_ppDSFile) as IUnknown;
end;

class function CoppDSFile.CreateRemote(const MachineName: AnsiString): IUnknown;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ppDSFile) as IUnknown;
end;

procedure Register;
begin
  RegisterComponents('ActiveX',[TWMP]);
end;

end.
