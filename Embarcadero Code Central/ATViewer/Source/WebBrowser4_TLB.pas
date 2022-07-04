{$WRITEABLECONST ON}
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 

unit WebBrowser4_TLB;

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
// File generated on 11.10.2006 13:08:24 from Type Library described below.

// ************************************************************************ //
// Type Lib: C:\WINDOWS\system32\ieframe.dll (1)
// IID\LCID: {EAB22AC0-30C1-11CF-A7EB-0000C05BAE0B}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\WINDOWS\system32\STDVCL40.DLL)
// Errors:
//   Hint: Member 'Type' of 'IWebBrowser' changed to 'Type_'
//   Hint: Parameter 'Type' of IWebBrowser.Type changed to 'Type_'
//   Hint: Parameter 'Property' of DWebBrowserEvents.PropertyChange changed to 'Property_'
//   Hint: Parameter 'Property' of IWebBrowserApp.PutProperty changed to 'Property_'
//   Hint: Parameter 'Property' of IWebBrowserApp.GetProperty changed to 'Property_'
//   Hint: Parameter 'Type' of IShellUIHelper.AddDesktopComponent changed to 'Type_'
//   Hint: Parameter 'var' of IShellNameSpace.Expand changed to 'var_'
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
  SHDocVwMajorVersion = 1;
  SHDocVwMinorVersion = 1;

  LIBID_SHDocVw: TGUID = '{EAB22AC0-30C1-11CF-A7EB-0000C05BAE0B}';

  IID_IWebBrowser: TGUID = '{EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}';
  DIID_DWebBrowserEvents: TGUID = '{EAB22AC2-30C1-11CF-A7EB-0000C05BAE0B}';
  IID_IWebBrowserApp: TGUID = '{0002DF05-0000-0000-C000-000000000046}';
  IID_IWebBrowser2: TGUID = '{D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}';
  DIID_DWebBrowserEvents2: TGUID = '{34A715A0-6587-11D0-924A-0020AFC7AC4D}';
  CLASS_WebBrowser_V1: TGUID = '{EAB22AC3-30C1-11CF-A7EB-0000C05BAE0B}';
  CLASS_WebBrowser: TGUID = '{8856F961-340A-11D0-A96B-00C04FD705A2}';
  CLASS_InternetExplorer: TGUID = '{0002DF01-0000-0000-C000-000000000046}';
  CLASS_ShellBrowserWindow: TGUID = '{C08AFD90-F2A1-11D1-8455-00A0C91F3880}';
  DIID_DShellWindowsEvents: TGUID = '{FE4106E0-399A-11D0-A48C-00A0C90A8F39}';
  IID_IShellWindows: TGUID = '{85CB6900-4D95-11CF-960C-0080C7F4EE85}';
  CLASS_ShellWindows: TGUID = '{9BA05972-F6A8-11CF-A442-00A0C90A8F39}';
  IID_IShellUIHelper: TGUID = '{729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}';
  IID_IShellUIHelper2: TGUID = '{F849B80C-A19C-496D-AEFB-F405971E8962}';
  CLASS_ShellUIHelper: TGUID = '{64AB4BB7-111E-11D1-8F79-00C04FC2FBE1}';
  DIID_DShellNameSpaceEvents: TGUID = '{55136806-B2DE-11D1-B9F2-00A0C98BC547}';
  IID_IShellFavoritesNameSpace: TGUID = '{55136804-B2DE-11D1-B9F2-00A0C98BC547}';
  IID_IShellNameSpace: TGUID = '{E572D3C9-37BE-4AE2-825D-D521763E3108}';
  CLASS_ShellNameSpace: TGUID = '{55136805-B2DE-11D1-B9F2-00A0C98BC547}';
  CLASS_ShellShellNameSpace: TGUID = '{2F2F1F96-2BC1-4B1C-BE28-EA3774F4676A}';
  IID_IScriptErrorList: TGUID = '{F3470F24-15FD-11D2-BB2E-00805FF7EFCA}';
  CLASS_CScriptErrorList: TGUID = '{EFD01300-160F-11D2-BB2E-00805FF7EFCA}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum CommandStateChangeConstants
type
  CommandStateChangeConstants = TOleEnum;
const
  CSC_UPDATECOMMANDS = $FFFFFFFF;
  CSC_NAVIGATEFORWARD = $00000001;
  CSC_NAVIGATEBACK = $00000002;

// Constants for enum OLECMDID
type
  OLECMDID = TOleEnum;
const
  OLECMDID_OPEN = $00000001;
  OLECMDID_NEW = $00000002;
  OLECMDID_SAVE = $00000003;
  OLECMDID_SAVEAS = $00000004;
  OLECMDID_SAVECOPYAS = $00000005;
  OLECMDID_PRINT = $00000006;
  OLECMDID_PRINTPREVIEW = $00000007;
  OLECMDID_PAGESETUP = $00000008;
  OLECMDID_SPELL = $00000009;
  OLECMDID_PROPERTIES = $0000000A;
  OLECMDID_CUT = $0000000B;
  OLECMDID_COPY = $0000000C;
  OLECMDID_PASTE = $0000000D;
  OLECMDID_PASTESPECIAL = $0000000E;
  OLECMDID_UNDO = $0000000F;
  OLECMDID_REDO = $00000010;
  OLECMDID_SELECTALL = $00000011;
  OLECMDID_CLEARSELECTION = $00000012;
  OLECMDID_ZOOM = $00000013;
  OLECMDID_GETZOOMRANGE = $00000014;
  OLECMDID_UPDATECOMMANDS = $00000015;
  OLECMDID_REFRESH = $00000016;
  OLECMDID_STOP = $00000017;
  OLECMDID_HIDETOOLBARS = $00000018;
  OLECMDID_SETPROGRESSMAX = $00000019;
  OLECMDID_SETPROGRESSPOS = $0000001A;
  OLECMDID_SETPROGRESSTEXT = $0000001B;
  OLECMDID_SETTITLE = $0000001C;
  OLECMDID_SETDOWNLOADSTATE = $0000001D;
  OLECMDID_STOPDOWNLOAD = $0000001E;
  OLECMDID_ONTOOLBARACTIVATED = $0000001F;
  OLECMDID_FIND = $00000020;
  OLECMDID_DELETE = $00000021;
  OLECMDID_HTTPEQUIV = $00000022;
  OLECMDID_HTTPEQUIV_DONE = $00000023;
  OLECMDID_ENABLE_INTERACTION = $00000024;
  OLECMDID_ONUNLOAD = $00000025;
  OLECMDID_PROPERTYBAG2 = $00000026;
  OLECMDID_PREREFRESH = $00000027;
  OLECMDID_SHOWSCRIPTERROR = $00000028;
  OLECMDID_SHOWMESSAGE = $00000029;
  OLECMDID_SHOWFIND = $0000002A;
  OLECMDID_SHOWPAGESETUP = $0000002B;
  OLECMDID_SHOWPRINT = $0000002C;
  OLECMDID_CLOSE = $0000002D;
  OLECMDID_ALLOWUILESSSAVEAS = $0000002E;
  OLECMDID_DONTDOWNLOADCSS = $0000002F;
  OLECMDID_UPDATEPAGESTATUS = $00000030;
  OLECMDID_PRINT2 = $00000031;
  OLECMDID_PRINTPREVIEW2 = $00000032;
  OLECMDID_SETPRINTTEMPLATE = $00000033;
  OLECMDID_GETPRINTTEMPLATE = $00000034;
  OLECMDID_PAGEACTIONBLOCKED = $00000037;
  OLECMDID_PAGEACTIONUIQUERY = $00000038;
  OLECMDID_FOCUSVIEWCONTROLS = $00000039;
  OLECMDID_FOCUSVIEWCONTROLSQUERY = $0000003A;
  OLECMDID_SHOWPAGEACTIONMENU = $0000003B;
  OLECMDID_ADDTRAVELENTRY = $0000003C;
  OLECMDID_UPDATETRAVELENTRY = $0000003D;
  OLECMDID_UPDATEBACKFORWARDSTATE = $0000003E;
  OLECMDID_OPTICAL_ZOOM = $0000003F;
  OLECMDID_OPTICAL_GETZOOMRANGE = $00000040;
  OLECMDID_WINDOWSTATECHANGED = $00000041;

// Constants for enum OLECMDF
type
  OLECMDF = TOleEnum;
const
  OLECMDF_SUPPORTED = $00000001;
  OLECMDF_ENABLED = $00000002;
  OLECMDF_LATCHED = $00000004;
  OLECMDF_NINCHED = $00000008;
  OLECMDF_INVISIBLE = $00000010;
  OLECMDF_DEFHIDEONCTXTMENU = $00000020;

// Constants for enum OLECMDEXECOPT
type
  OLECMDEXECOPT = TOleEnum;
const
  OLECMDEXECOPT_DODEFAULT = $00000000;
  OLECMDEXECOPT_PROMPTUSER = $00000001;
  OLECMDEXECOPT_DONTPROMPTUSER = $00000002;
  OLECMDEXECOPT_SHOWHELP = $00000003;

// Constants for enum tagREADYSTATE
type
  tagREADYSTATE = TOleEnum;
const
  READYSTATE_UNINITIALIZED = $00000000;
  READYSTATE_LOADING = $00000001;
  READYSTATE_LOADED = $00000002;
  READYSTATE_INTERACTIVE = $00000003;
  READYSTATE_COMPLETE = $00000004;

// Constants for enum SecureLockIconConstants
type
  SecureLockIconConstants = TOleEnum;
const
  secureLockIconUnsecure = $00000000;
  secureLockIconMixed = $00000001;
  secureLockIconSecureUnknownBits = $00000002;
  secureLockIconSecure40Bit = $00000003;
  secureLockIconSecure56Bit = $00000004;
  secureLockIconSecureFortezza = $00000005;
  secureLockIconSecure128Bit = $00000006;

// Constants for enum ShellWindowTypeConstants
type
  ShellWindowTypeConstants = TOleEnum;
const
  SWC_EXPLORER = $00000000;
  SWC_BROWSER = $00000001;
  SWC_3RDPARTY = $00000002;
  SWC_CALLBACK = $00000004;
  SWC_DESKTOP = $00000008;

// Constants for enum ShellWindowFindWindowOptions
type
  ShellWindowFindWindowOptions = TOleEnum;
const
  SWFO_NEEDDISPATCH = $00000001;
  SWFO_INCLUDEPENDING = $00000002;
  SWFO_COOKIEPASSED = $00000004;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IWebBrowser = interface;
  IWebBrowserDisp = dispinterface;
  DWebBrowserEvents = dispinterface;
  IWebBrowserApp = interface;
  IWebBrowserAppDisp = dispinterface;
  IWebBrowser2 = interface;
  IWebBrowser2Disp = dispinterface;
  DWebBrowserEvents2 = dispinterface;
  DShellWindowsEvents = dispinterface;
  IShellWindows = interface;
  IShellWindowsDisp = dispinterface;
  IShellUIHelper = interface;
  IShellUIHelperDisp = dispinterface;
  IShellUIHelper2 = interface;
  IShellUIHelper2Disp = dispinterface;
  DShellNameSpaceEvents = dispinterface;
  IShellFavoritesNameSpace = interface;
  IShellFavoritesNameSpaceDisp = dispinterface;
  IShellNameSpace = interface;
  IShellNameSpaceDisp = dispinterface;
  IScriptErrorList = interface;
  IScriptErrorListDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  WebBrowser_V1 = IWebBrowser;
  WebBrowser = IWebBrowser2;
  InternetExplorer = IWebBrowser2;
  ShellBrowserWindow = IWebBrowser2;
  ShellWindows = IShellWindows;
  ShellUIHelper = IShellUIHelper2;
  ShellNameSpace = IShellNameSpace;
  ShellShellNameSpace = IShellNameSpace;
  CScriptErrorList = IScriptErrorList;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  POleVariant1 = ^OleVariant; {*}


// *********************************************************************//
// Interface: IWebBrowser
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}
// *********************************************************************//
  IWebBrowser = interface(IDispatch)
    ['{EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}']
    procedure GoBack; safecall;
    procedure GoForward; safecall;
    procedure GoHome; safecall;
    procedure GoSearch; safecall;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; 
                       var TargetFrameName: OleVariant; var PostData: OleVariant; 
                       var Headers: OleVariant); safecall;
    procedure Refresh; safecall;
    procedure Refresh2(var Level: OleVariant); safecall;
    procedure Stop; safecall;
    function  Get_Application: IDispatch; safecall;
    function  Get_Parent: IDispatch; safecall;
    function  Get_Container: IDispatch; safecall;
    function  Get_Document: IDispatch; safecall;
    function  Get_TopLevelContainer: WordBool; safecall;
    function  Get_Type_: WideString; safecall;
    function  Get_Left: Integer; safecall;
    procedure Set_Left(pl: Integer); safecall;
    function  Get_Top: Integer; safecall;
    procedure Set_Top(pl: Integer); safecall;
    function  Get_Width: Integer; safecall;
    procedure Set_Width(pl: Integer); safecall;
    function  Get_Height: Integer; safecall;
    procedure Set_Height(pl: Integer); safecall;
    function  Get_LocationName: WideString; safecall;
    function  Get_LocationURL: WideString; safecall;
    function  Get_Busy: WordBool; safecall;
    property Application: IDispatch read Get_Application;
    property Parent: IDispatch read Get_Parent;
    property Container: IDispatch read Get_Container;
    property Document: IDispatch read Get_Document;
    property TopLevelContainer: WordBool read Get_TopLevelContainer;
    property Type_: WideString read Get_Type_;
    property Left: Integer read Get_Left write Set_Left;
    property Top: Integer read Get_Top write Set_Top;
    property Width: Integer read Get_Width write Set_Width;
    property Height: Integer read Get_Height write Set_Height;
    property LocationName: WideString read Get_LocationName;
    property LocationURL: WideString read Get_LocationURL;
    property Busy: WordBool read Get_Busy;
  end;

// *********************************************************************//
// DispIntf:  IWebBrowserDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}
// *********************************************************************//
  IWebBrowserDisp = dispinterface
    ['{EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}']
    procedure GoBack; dispid 100;
    procedure GoForward; dispid 101;
    procedure GoHome; dispid 102;
    procedure GoSearch; dispid 103;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; 
                       var TargetFrameName: OleVariant; var PostData: OleVariant; 
                       var Headers: OleVariant); dispid 104;
    procedure Refresh; dispid -550;
    procedure Refresh2(var Level: OleVariant); dispid 105;
    procedure Stop; dispid 106;
    property Application: IDispatch readonly dispid 200;
    property Parent: IDispatch readonly dispid 201;
    property Container: IDispatch readonly dispid 202;
    property Document: IDispatch readonly dispid 203;
    property TopLevelContainer: WordBool readonly dispid 204;
    property Type_: WideString readonly dispid 205;
    property Left: Integer dispid 206;
    property Top: Integer dispid 207;
    property Width: Integer dispid 208;
    property Height: Integer dispid 209;
    property LocationName: WideString readonly dispid 210;
    property LocationURL: WideString readonly dispid 211;
    property Busy: WordBool readonly dispid 212;
  end;

// *********************************************************************//
// DispIntf:  DWebBrowserEvents
// Flags:     (4112) Hidden Dispatchable
// GUID:      {EAB22AC2-30C1-11CF-A7EB-0000C05BAE0B}
// *********************************************************************//
  DWebBrowserEvents = dispinterface
    ['{EAB22AC2-30C1-11CF-A7EB-0000C05BAE0B}']
    procedure BeforeNavigate(const URL: WideString; Flags: Integer; 
                             const TargetFrameName: WideString; var PostData: OleVariant; 
                             const Headers: WideString; var Cancel: WordBool); dispid 100;
    procedure NavigateComplete(const URL: WideString); dispid 101;
    procedure StatusTextChange(const Text: WideString); dispid 102;
    procedure ProgressChange(Progress: Integer; ProgressMax: Integer); dispid 108;
    procedure DownloadComplete; dispid 104;
    procedure CommandStateChange(Command: Integer; Enable: WordBool); dispid 105;
    procedure DownloadBegin; dispid 106;
    procedure NewWindow(const URL: WideString; Flags: Integer; const TargetFrameName: WideString; 
                        var PostData: OleVariant; const Headers: WideString; var Processed: WordBool); dispid 107;
    procedure TitleChange(const Text: WideString); dispid 113;
    procedure FrameBeforeNavigate(const URL: WideString; Flags: Integer; 
                                  const TargetFrameName: WideString; var PostData: OleVariant; 
                                  const Headers: WideString; var Cancel: WordBool); dispid 200;
    procedure FrameNavigateComplete(const URL: WideString); dispid 201;
    procedure FrameNewWindow(const URL: WideString; Flags: Integer; 
                             const TargetFrameName: WideString; var PostData: OleVariant; 
                             const Headers: WideString; var Processed: WordBool); dispid 204;
    procedure Quit(var Cancel: WordBool); dispid 103;
    procedure WindowMove; dispid 109;
    procedure WindowResize; dispid 110;
    procedure WindowActivate; dispid 111;
    procedure PropertyChange(const Property_: WideString); dispid 112;
  end;

// *********************************************************************//
// Interface: IWebBrowserApp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {0002DF05-0000-0000-C000-000000000046}
// *********************************************************************//
  IWebBrowserApp = interface(IWebBrowser)
    ['{0002DF05-0000-0000-C000-000000000046}']
    procedure Quit; safecall;
    procedure ClientToWindow(var pcx: SYSINT; var pcy: SYSINT); safecall;
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant); safecall;
    function  GetProperty(const Property_: WideString): OleVariant; safecall;
    function  Get_Name: WideString; safecall;
    function  Get_HWND: Integer; safecall;
    function  Get_FullName: WideString; safecall;
    function  Get_Path: WideString; safecall;
    function  Get_Visible: WordBool; safecall;
    procedure Set_Visible(pBool: WordBool); safecall;
    function  Get_StatusBar: WordBool; safecall;
    procedure Set_StatusBar(pBool: WordBool); safecall;
    function  Get_StatusText: WideString; safecall;
    procedure Set_StatusText(const StatusText: WideString); safecall;
    function  Get_ToolBar: SYSINT; safecall;
    procedure Set_ToolBar(Value: SYSINT); safecall;
    function  Get_MenuBar: WordBool; safecall;
    procedure Set_MenuBar(Value: WordBool); safecall;
    function  Get_FullScreen: WordBool; safecall;
    procedure Set_FullScreen(pbFullScreen: WordBool); safecall;
    property Name: WideString read Get_Name;
    property HWND: Integer read Get_HWND;
    property FullName: WideString read Get_FullName;
    property Path: WideString read Get_Path;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property StatusBar: WordBool read Get_StatusBar write Set_StatusBar;
    property StatusText: WideString read Get_StatusText write Set_StatusText;
    property ToolBar: SYSINT read Get_ToolBar write Set_ToolBar;
    property MenuBar: WordBool read Get_MenuBar write Set_MenuBar;
    property FullScreen: WordBool read Get_FullScreen write Set_FullScreen;
  end;

// *********************************************************************//
// DispIntf:  IWebBrowserAppDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {0002DF05-0000-0000-C000-000000000046}
// *********************************************************************//
  IWebBrowserAppDisp = dispinterface
    ['{0002DF05-0000-0000-C000-000000000046}']
    procedure Quit; dispid 300;
    procedure ClientToWindow(var pcx: SYSINT; var pcy: SYSINT); dispid 301;
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant); dispid 302;
    function  GetProperty(const Property_: WideString): OleVariant; dispid 303;
    property Name: WideString readonly dispid 0;
    property HWND: Integer readonly dispid -515;
    property FullName: WideString readonly dispid 400;
    property Path: WideString readonly dispid 401;
    property Visible: WordBool dispid 402;
    property StatusBar: WordBool dispid 403;
    property StatusText: WideString dispid 404;
    property ToolBar: SYSINT dispid 405;
    property MenuBar: WordBool dispid 406;
    property FullScreen: WordBool dispid 407;
    procedure GoBack; dispid 100;
    procedure GoForward; dispid 101;
    procedure GoHome; dispid 102;
    procedure GoSearch; dispid 103;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; 
                       var TargetFrameName: OleVariant; var PostData: OleVariant; 
                       var Headers: OleVariant); dispid 104;
    procedure Refresh; dispid -550;
    procedure Refresh2(var Level: OleVariant); dispid 105;
    procedure Stop; dispid 106;
    property Application: IDispatch readonly dispid 200;
    property Parent: IDispatch readonly dispid 201;
    property Container: IDispatch readonly dispid 202;
    property Document: IDispatch readonly dispid 203;
    property TopLevelContainer: WordBool readonly dispid 204;
    property Type_: WideString readonly dispid 205;
    property Left: Integer dispid 206;
    property Top: Integer dispid 207;
    property Width: Integer dispid 208;
    property Height: Integer dispid 209;
    property LocationName: WideString readonly dispid 210;
    property LocationURL: WideString readonly dispid 211;
    property Busy: WordBool readonly dispid 212;
  end;

// *********************************************************************//
// Interface: IWebBrowser2
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}
// *********************************************************************//
  IWebBrowser2 = interface(IWebBrowserApp)
    ['{D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}']
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                        var TargetFrameName: OleVariant; var PostData: OleVariant; 
                        var Headers: OleVariant); safecall;
    function  QueryStatusWB(cmdID: OLECMDID): OLECMDF; safecall;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant; 
                     var pvaOut: OleVariant); safecall;
    procedure ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant; 
                             var pvarSize: OleVariant); safecall;
    function  Get_ReadyState: tagREADYSTATE; safecall;
    function  Get_Offline: WordBool; safecall;
    procedure Set_Offline(pbOffline: WordBool); safecall;
    function  Get_Silent: WordBool; safecall;
    procedure Set_Silent(pbSilent: WordBool); safecall;
    function  Get_RegisterAsBrowser: WordBool; safecall;
    procedure Set_RegisterAsBrowser(pbRegister: WordBool); safecall;
    function  Get_RegisterAsDropTarget: WordBool; safecall;
    procedure Set_RegisterAsDropTarget(pbRegister: WordBool); safecall;
    function  Get_TheaterMode: WordBool; safecall;
    procedure Set_TheaterMode(pbRegister: WordBool); safecall;
    function  Get_AddressBar: WordBool; safecall;
    procedure Set_AddressBar(Value: WordBool); safecall;
    function  Get_Resizable: WordBool; safecall;
    procedure Set_Resizable(Value: WordBool); safecall;
    property ReadyState: tagREADYSTATE read Get_ReadyState;
    property Offline: WordBool read Get_Offline write Set_Offline;
    property Silent: WordBool read Get_Silent write Set_Silent;
    property RegisterAsBrowser: WordBool read Get_RegisterAsBrowser write Set_RegisterAsBrowser;
    property RegisterAsDropTarget: WordBool read Get_RegisterAsDropTarget write Set_RegisterAsDropTarget;
    property TheaterMode: WordBool read Get_TheaterMode write Set_TheaterMode;
    property AddressBar: WordBool read Get_AddressBar write Set_AddressBar;
    property Resizable: WordBool read Get_Resizable write Set_Resizable;
  end;

// *********************************************************************//
// DispIntf:  IWebBrowser2Disp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}
// *********************************************************************//
  IWebBrowser2Disp = dispinterface
    ['{D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}']
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                        var TargetFrameName: OleVariant; var PostData: OleVariant; 
                        var Headers: OleVariant); dispid 500;
    function  QueryStatusWB(cmdID: OLECMDID): OLECMDF; dispid 501;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant; 
                     var pvaOut: OleVariant); dispid 502;
    procedure ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant; 
                             var pvarSize: OleVariant); dispid 503;
    property ReadyState: tagREADYSTATE readonly dispid -525;
    property Offline: WordBool dispid 550;
    property Silent: WordBool dispid 551;
    property RegisterAsBrowser: WordBool dispid 552;
    property RegisterAsDropTarget: WordBool dispid 553;
    property TheaterMode: WordBool dispid 554;
    property AddressBar: WordBool dispid 555;
    property Resizable: WordBool dispid 556;
    procedure Quit; dispid 300;
    procedure ClientToWindow(var pcx: SYSINT; var pcy: SYSINT); dispid 301;
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant); dispid 302;
    function  GetProperty(const Property_: WideString): OleVariant; dispid 303;
    property Name: WideString readonly dispid 0;
    property HWND: Integer readonly dispid -515;
    property FullName: WideString readonly dispid 400;
    property Path: WideString readonly dispid 401;
    property Visible: WordBool dispid 402;
    property StatusBar: WordBool dispid 403;
    property StatusText: WideString dispid 404;
    property ToolBar: SYSINT dispid 405;
    property MenuBar: WordBool dispid 406;
    property FullScreen: WordBool dispid 407;
    procedure GoBack; dispid 100;
    procedure GoForward; dispid 101;
    procedure GoHome; dispid 102;
    procedure GoSearch; dispid 103;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; 
                       var TargetFrameName: OleVariant; var PostData: OleVariant; 
                       var Headers: OleVariant); dispid 104;
    procedure Refresh; dispid -550;
    procedure Refresh2(var Level: OleVariant); dispid 105;
    procedure Stop; dispid 106;
    property Application: IDispatch readonly dispid 200;
    property Parent: IDispatch readonly dispid 201;
    property Container: IDispatch readonly dispid 202;
    property Document: IDispatch readonly dispid 203;
    property TopLevelContainer: WordBool readonly dispid 204;
    property Type_: WideString readonly dispid 205;
    property Left: Integer dispid 206;
    property Top: Integer dispid 207;
    property Width: Integer dispid 208;
    property Height: Integer dispid 209;
    property LocationName: WideString readonly dispid 210;
    property LocationURL: WideString readonly dispid 211;
    property Busy: WordBool readonly dispid 212;
  end;

// *********************************************************************//
// DispIntf:  DWebBrowserEvents2
// Flags:     (4112) Hidden Dispatchable
// GUID:      {34A715A0-6587-11D0-924A-0020AFC7AC4D}
// *********************************************************************//
  DWebBrowserEvents2 = dispinterface
    ['{34A715A0-6587-11D0-924A-0020AFC7AC4D}']
    procedure StatusTextChange(const Text: WideString); dispid 102;
    procedure ProgressChange(Progress: Integer; ProgressMax: Integer); dispid 108;
    procedure CommandStateChange(Command: Integer; Enable: WordBool); dispid 105;
    procedure DownloadBegin; dispid 106;
    procedure DownloadComplete; dispid 104;
    procedure TitleChange(const Text: WideString); dispid 113;
    procedure PropertyChange(const szProperty: WideString); dispid 112;
    procedure BeforeNavigate2(const pDisp: IDispatch; var URL: OleVariant; var Flags: OleVariant; 
                              var TargetFrameName: OleVariant; var PostData: OleVariant; 
                              var Headers: OleVariant; var Cancel: WordBool); dispid 250;
    procedure NewWindow2(var ppDisp: IDispatch; var Cancel: WordBool); dispid 251;
    procedure NavigateComplete2(const pDisp: IDispatch; var URL: OleVariant); dispid 252;
    procedure DocumentComplete(const pDisp: IDispatch; var URL: OleVariant); dispid 259;
    procedure OnQuit; dispid 253;
    procedure OnVisible(Visible: WordBool); dispid 254;
    procedure OnToolBar(ToolBar: WordBool); dispid 255;
    procedure OnMenuBar(MenuBar: WordBool); dispid 256;
    procedure OnStatusBar(StatusBar: WordBool); dispid 257;
    procedure OnFullScreen(FullScreen: WordBool); dispid 258;
    procedure OnTheaterMode(TheaterMode: WordBool); dispid 260;
    procedure WindowSetResizable(Resizable: WordBool); dispid 262;
    procedure WindowSetLeft(Left: Integer); dispid 264;
    procedure WindowSetTop(Top: Integer); dispid 265;
    procedure WindowSetWidth(Width: Integer); dispid 266;
    procedure WindowSetHeight(Height: Integer); dispid 267;
    procedure WindowClosing(IsChildWindow: WordBool; var Cancel: WordBool); dispid 263;
    procedure ClientToHostWindow(var CX: Integer; var CY: Integer); dispid 268;
    procedure SetSecureLockIcon(SecureLockIcon: Integer); dispid 269;
    procedure FileDownload(ActiveDocument: WordBool; var Cancel: WordBool); dispid 270;
    procedure NavigateError(const pDisp: IDispatch; var URL: OleVariant; var Frame: OleVariant; 
                            var StatusCode: OleVariant; var Cancel: WordBool); dispid 271;
    procedure PrintTemplateInstantiation(const pDisp: IDispatch); dispid 225;
    procedure PrintTemplateTeardown(const pDisp: IDispatch); dispid 226;
    procedure UpdatePageStatus(const pDisp: IDispatch; var nPage: OleVariant; var fDone: OleVariant); dispid 227;
    procedure PrivacyImpactedStateChange(bImpacted: WordBool); dispid 272;
    procedure NewWindow3(var ppDisp: IDispatch; var Cancel: WordBool; dwFlags: LongWord; 
                         const bstrUrlContext: WideString; const bstrUrl: WideString); dispid 273;
    procedure SetPhishingFilterStatus(PhishingFilterStatus: Integer); dispid 282;
    procedure WindowStateChanged(dwWindowStateFlags: LongWord; dwValidFlagsMask: LongWord); dispid 283;
  end;

// *********************************************************************//
// DispIntf:  DShellWindowsEvents
// Flags:     (4096) Dispatchable
// GUID:      {FE4106E0-399A-11D0-A48C-00A0C90A8F39}
// *********************************************************************//
  DShellWindowsEvents = dispinterface
    ['{FE4106E0-399A-11D0-A48C-00A0C90A8F39}']
    procedure WindowRegistered(lCookie: Integer); dispid 200;
    procedure WindowRevoked(lCookie: Integer); dispid 201;
  end;

// *********************************************************************//
// Interface: IShellWindows
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {85CB6900-4D95-11CF-960C-0080C7F4EE85}
// *********************************************************************//
  IShellWindows = interface(IDispatch)
    ['{85CB6900-4D95-11CF-960C-0080C7F4EE85}']
    function  Get_Count: Integer; safecall;
    function  Item(index: OleVariant): IDispatch; safecall;
    function  _NewEnum: IUnknown; safecall;
    procedure Register(const pid: IDispatch; HWND: Integer; swClass: SYSINT; out plCookie: Integer); safecall;
    procedure RegisterPending(lThreadId: Integer; var pvarloc: OleVariant; 
                              var pvarlocRoot: OleVariant; swClass: SYSINT; out plCookie: Integer); safecall;
    procedure Revoke(lCookie: Integer); safecall;
    procedure OnNavigate(lCookie: Integer; var pvarloc: OleVariant); safecall;
    procedure OnActivated(lCookie: Integer; fActive: WordBool); safecall;
    function  FindWindowSW(var pvarloc: OleVariant; var pvarlocRoot: OleVariant; swClass: SYSINT; 
                           out pHWND: Integer; swfwOptions: SYSINT): IDispatch; safecall;
    procedure OnCreated(lCookie: Integer; const punk: IUnknown); safecall;
    procedure ProcessAttachDetach(fAttach: WordBool); safecall;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IShellWindowsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {85CB6900-4D95-11CF-960C-0080C7F4EE85}
// *********************************************************************//
  IShellWindowsDisp = dispinterface
    ['{85CB6900-4D95-11CF-960C-0080C7F4EE85}']
    property Count: Integer readonly dispid 1610743808;
    function  Item(index: OleVariant): IDispatch; dispid 0;
    function  _NewEnum: IUnknown; dispid -4;
    procedure Register(const pid: IDispatch; HWND: Integer; swClass: SYSINT; out plCookie: Integer); dispid 1610743811;
    procedure RegisterPending(lThreadId: Integer; var pvarloc: OleVariant; 
                              var pvarlocRoot: OleVariant; swClass: SYSINT; out plCookie: Integer); dispid 1610743812;
    procedure Revoke(lCookie: Integer); dispid 1610743813;
    procedure OnNavigate(lCookie: Integer; var pvarloc: OleVariant); dispid 1610743814;
    procedure OnActivated(lCookie: Integer; fActive: WordBool); dispid 1610743815;
    function  FindWindowSW(var pvarloc: OleVariant; var pvarlocRoot: OleVariant; swClass: SYSINT; 
                           out pHWND: Integer; swfwOptions: SYSINT): IDispatch; dispid 1610743816;
    procedure OnCreated(lCookie: Integer; const punk: IUnknown); dispid 1610743817;
    procedure ProcessAttachDetach(fAttach: WordBool); dispid 1610743818;
  end;

// *********************************************************************//
// Interface: IShellUIHelper
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}
// *********************************************************************//
  IShellUIHelper = interface(IDispatch)
    ['{729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}']
    procedure ResetFirstBootMode; safecall;
    procedure ResetSafeMode; safecall;
    procedure RefreshOfflineDesktop; safecall;
    procedure AddFavorite(const URL: WideString; var Title: OleVariant); safecall;
    procedure AddChannel(const URL: WideString); safecall;
    procedure AddDesktopComponent(const URL: WideString; const Type_: WideString; 
                                  var Left: OleVariant; var Top: OleVariant; var Width: OleVariant; 
                                  var Height: OleVariant); safecall;
    function  IsSubscribed(const URL: WideString): WordBool; safecall;
    procedure NavigateAndFind(const URL: WideString; const strQuery: WideString; 
                              var varTargetFrame: OleVariant); safecall;
    procedure ImportExportFavorites(fImport: WordBool; const strImpExpPath: WideString); safecall;
    procedure AutoCompleteSaveForm(var Form: OleVariant); safecall;
    procedure AutoScan(const strSearch: WideString; const strFailureUrl: WideString; 
                       var pvarTargetFrame: OleVariant); safecall;
    procedure AutoCompleteAttach(var Reserved: OleVariant); safecall;
    function  ShowBrowserUI(const bstrName: WideString; var pvarIn: OleVariant): OleVariant; safecall;
  end;

// *********************************************************************//
// DispIntf:  IShellUIHelperDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}
// *********************************************************************//
  IShellUIHelperDisp = dispinterface
    ['{729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}']
    procedure ResetFirstBootMode; dispid 1;
    procedure ResetSafeMode; dispid 2;
    procedure RefreshOfflineDesktop; dispid 3;
    procedure AddFavorite(const URL: WideString; var Title: OleVariant); dispid 4;
    procedure AddChannel(const URL: WideString); dispid 5;
    procedure AddDesktopComponent(const URL: WideString; const Type_: WideString; 
                                  var Left: OleVariant; var Top: OleVariant; var Width: OleVariant; 
                                  var Height: OleVariant); dispid 6;
    function  IsSubscribed(const URL: WideString): WordBool; dispid 7;
    procedure NavigateAndFind(const URL: WideString; const strQuery: WideString; 
                              var varTargetFrame: OleVariant); dispid 8;
    procedure ImportExportFavorites(fImport: WordBool; const strImpExpPath: WideString); dispid 9;
    procedure AutoCompleteSaveForm(var Form: OleVariant); dispid 10;
    procedure AutoScan(const strSearch: WideString; const strFailureUrl: WideString; 
                       var pvarTargetFrame: OleVariant); dispid 11;
    procedure AutoCompleteAttach(var Reserved: OleVariant); dispid 12;
    function  ShowBrowserUI(const bstrName: WideString; var pvarIn: OleVariant): OleVariant; dispid 13;
  end;

// *********************************************************************//
// Interface: IShellUIHelper2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F849B80C-A19C-496D-AEFB-F405971E8962}
// *********************************************************************//
  IShellUIHelper2 = interface(IShellUIHelper)
    ['{F849B80C-A19C-496D-AEFB-F405971E8962}']
    procedure AddSearchProvider(const URL: WideString); safecall;
    procedure RunOnceShown; safecall;
    procedure SkipRunOnce; safecall;
    procedure CustomizeSettings(fSQM: WordBool; fPhishing: WordBool; const bstrLocale: WideString); safecall;
    function  SqmEnabled: WordBool; safecall;
    function  PhishingEnabled: WordBool; safecall;
    function  BrandImageUri: WideString; safecall;
    procedure SkipTabsWelcome; safecall;
    procedure DiagnoseConnection; safecall;
    procedure CustomizeClearType(fSet: WordBool); safecall;
  end;

// *********************************************************************//
// DispIntf:  IShellUIHelper2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F849B80C-A19C-496D-AEFB-F405971E8962}
// *********************************************************************//
  IShellUIHelper2Disp = dispinterface
    ['{F849B80C-A19C-496D-AEFB-F405971E8962}']
    procedure AddSearchProvider(const URL: WideString); dispid 14;
    procedure RunOnceShown; dispid 15;
    procedure SkipRunOnce; dispid 16;
    procedure CustomizeSettings(fSQM: WordBool; fPhishing: WordBool; const bstrLocale: WideString); dispid 17;
    function  SqmEnabled: WordBool; dispid 18;
    function  PhishingEnabled: WordBool; dispid 19;
    function  BrandImageUri: WideString; dispid 20;
    procedure SkipTabsWelcome; dispid 21;
    procedure DiagnoseConnection; dispid 22;
    procedure CustomizeClearType(fSet: WordBool); dispid 23;
    procedure ResetFirstBootMode; dispid 1;
    procedure ResetSafeMode; dispid 2;
    procedure RefreshOfflineDesktop; dispid 3;
    procedure AddFavorite(const URL: WideString; var Title: OleVariant); dispid 4;
    procedure AddChannel(const URL: WideString); dispid 5;
    procedure AddDesktopComponent(const URL: WideString; const Type_: WideString; 
                                  var Left: OleVariant; var Top: OleVariant; var Width: OleVariant; 
                                  var Height: OleVariant); dispid 6;
    function  IsSubscribed(const URL: WideString): WordBool; dispid 7;
    procedure NavigateAndFind(const URL: WideString; const strQuery: WideString; 
                              var varTargetFrame: OleVariant); dispid 8;
    procedure ImportExportFavorites(fImport: WordBool; const strImpExpPath: WideString); dispid 9;
    procedure AutoCompleteSaveForm(var Form: OleVariant); dispid 10;
    procedure AutoScan(const strSearch: WideString; const strFailureUrl: WideString; 
                       var pvarTargetFrame: OleVariant); dispid 11;
    procedure AutoCompleteAttach(var Reserved: OleVariant); dispid 12;
    function  ShowBrowserUI(const bstrName: WideString; var pvarIn: OleVariant): OleVariant; dispid 13;
  end;

// *********************************************************************//
// DispIntf:  DShellNameSpaceEvents
// Flags:     (4096) Dispatchable
// GUID:      {55136806-B2DE-11D1-B9F2-00A0C98BC547}
// *********************************************************************//
  DShellNameSpaceEvents = dispinterface
    ['{55136806-B2DE-11D1-B9F2-00A0C98BC547}']
    procedure FavoritesSelectionChange(cItems: Integer; hItem: Integer; const strName: WideString; 
                                       const strUrl: WideString; cVisits: Integer; 
                                       const strDate: WideString; fAvailableOffline: Integer); dispid 1;
    procedure SelectionChange; dispid 2;
    procedure DoubleClick; dispid 3;
    procedure Initialized; dispid 4;
  end;

// *********************************************************************//
// Interface: IShellFavoritesNameSpace
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {55136804-B2DE-11D1-B9F2-00A0C98BC547}
// *********************************************************************//
  IShellFavoritesNameSpace = interface(IDispatch)
    ['{55136804-B2DE-11D1-B9F2-00A0C98BC547}']
    procedure MoveSelectionUp; safecall;
    procedure MoveSelectionDown; safecall;
    procedure ResetSort; safecall;
    procedure NewFolder; safecall;
    procedure Synchronize; safecall;
    procedure Import; safecall;
    procedure Export; safecall;
    procedure InvokeContextMenuCommand(const strCommand: WideString); safecall;
    procedure MoveSelectionTo; safecall;
    function  Get_SubscriptionsEnabled: WordBool; safecall;
    function  CreateSubscriptionForSelection: WordBool; safecall;
    function  DeleteSubscriptionForSelection: WordBool; safecall;
    procedure SetRoot(const bstrFullPath: WideString); safecall;
    property SubscriptionsEnabled: WordBool read Get_SubscriptionsEnabled;
  end;

// *********************************************************************//
// DispIntf:  IShellFavoritesNameSpaceDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {55136804-B2DE-11D1-B9F2-00A0C98BC547}
// *********************************************************************//
  IShellFavoritesNameSpaceDisp = dispinterface
    ['{55136804-B2DE-11D1-B9F2-00A0C98BC547}']
    procedure MoveSelectionUp; dispid 1;
    procedure MoveSelectionDown; dispid 2;
    procedure ResetSort; dispid 3;
    procedure NewFolder; dispid 4;
    procedure Synchronize; dispid 5;
    procedure Import; dispid 6;
    procedure Export; dispid 7;
    procedure InvokeContextMenuCommand(const strCommand: WideString); dispid 8;
    procedure MoveSelectionTo; dispid 9;
    property SubscriptionsEnabled: WordBool readonly dispid 10;
    function  CreateSubscriptionForSelection: WordBool; dispid 11;
    function  DeleteSubscriptionForSelection: WordBool; dispid 12;
    procedure SetRoot(const bstrFullPath: WideString); dispid 13;
  end;

// *********************************************************************//
// Interface: IShellNameSpace
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {E572D3C9-37BE-4AE2-825D-D521763E3108}
// *********************************************************************//
  IShellNameSpace = interface(IShellFavoritesNameSpace)
    ['{E572D3C9-37BE-4AE2-825D-D521763E3108}']
    function  Get_EnumOptions: Integer; safecall;
    procedure Set_EnumOptions(pgrfEnumFlags: Integer); safecall;
    function  Get_SelectedItem: IDispatch; safecall;
    procedure Set_SelectedItem(const pItem: IDispatch); safecall;
    function  Get_Root: OleVariant; safecall;
    procedure Set_Root(pvar: OleVariant); safecall;
    function  Get_Depth: SYSINT; safecall;
    procedure Set_Depth(piDepth: SYSINT); safecall;
    function  Get_Mode: SYSUINT; safecall;
    procedure Set_Mode(puMode: SYSUINT); safecall;
    function  Get_Flags: LongWord; safecall;
    procedure Set_Flags(pdwFlags: LongWord); safecall;
    procedure Set_TVFlags(dwFlags: LongWord); safecall;
    function  Get_TVFlags: LongWord; safecall;
    function  Get_Columns: WideString; safecall;
    procedure Set_Columns(const bstrColumns: WideString); safecall;
    function  Get_CountViewTypes: SYSINT; safecall;
    procedure SetViewType(iType: SYSINT); safecall;
    function  SelectedItems: IDispatch; safecall;
    procedure Expand(var_: OleVariant; iDepth: SYSINT); safecall;
    procedure UnselectAll; safecall;
    property EnumOptions: Integer read Get_EnumOptions write Set_EnumOptions;
    property SelectedItem: IDispatch read Get_SelectedItem write Set_SelectedItem;
    property Root: OleVariant read Get_Root write Set_Root;
    property Depth: SYSINT read Get_Depth write Set_Depth;
    property Mode: SYSUINT read Get_Mode write Set_Mode;
    property Flags: LongWord read Get_Flags write Set_Flags;
    property TVFlags: LongWord read Get_TVFlags write Set_TVFlags;
    property Columns: WideString read Get_Columns write Set_Columns;
    property CountViewTypes: SYSINT read Get_CountViewTypes;
  end;

// *********************************************************************//
// DispIntf:  IShellNameSpaceDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {E572D3C9-37BE-4AE2-825D-D521763E3108}
// *********************************************************************//
  IShellNameSpaceDisp = dispinterface
    ['{E572D3C9-37BE-4AE2-825D-D521763E3108}']
    property EnumOptions: Integer dispid 14;
    property SelectedItem: IDispatch dispid 15;
    property Root: OleVariant dispid 16;
    property Depth: SYSINT dispid 17;
    property Mode: SYSUINT dispid 18;
    property Flags: LongWord dispid 19;
    property TVFlags: LongWord dispid 20;
    property Columns: WideString dispid 21;
    property CountViewTypes: SYSINT readonly dispid 22;
    procedure SetViewType(iType: SYSINT); dispid 23;
    function  SelectedItems: IDispatch; dispid 24;
    procedure Expand(var_: OleVariant; iDepth: SYSINT); dispid 25;
    procedure UnselectAll; dispid 26;
    procedure MoveSelectionUp; dispid 1;
    procedure MoveSelectionDown; dispid 2;
    procedure ResetSort; dispid 3;
    procedure NewFolder; dispid 4;
    procedure Synchronize; dispid 5;
    procedure Import; dispid 6;
    procedure Export; dispid 7;
    procedure InvokeContextMenuCommand(const strCommand: WideString); dispid 8;
    procedure MoveSelectionTo; dispid 9;
    property SubscriptionsEnabled: WordBool readonly dispid 10;
    function  CreateSubscriptionForSelection: WordBool; dispid 11;
    function  DeleteSubscriptionForSelection: WordBool; dispid 12;
    procedure SetRoot(const bstrFullPath: WideString); dispid 13;
  end;

// *********************************************************************//
// Interface: IScriptErrorList
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {F3470F24-15FD-11D2-BB2E-00805FF7EFCA}
// *********************************************************************//
  IScriptErrorList = interface(IDispatch)
    ['{F3470F24-15FD-11D2-BB2E-00805FF7EFCA}']
    procedure advanceError; safecall;
    procedure retreatError; safecall;
    function  canAdvanceError: Integer; safecall;
    function  canRetreatError: Integer; safecall;
    function  getErrorLine: Integer; safecall;
    function  getErrorChar: Integer; safecall;
    function  getErrorCode: Integer; safecall;
    function  getErrorMsg: WideString; safecall;
    function  getErrorUrl: WideString; safecall;
    function  getAlwaysShowLockState: Integer; safecall;
    function  getDetailsPaneOpen: Integer; safecall;
    procedure setDetailsPaneOpen(fDetailsPaneOpen: Integer); safecall;
    function  getPerErrorDisplay: Integer; safecall;
    procedure setPerErrorDisplay(fPerErrorDisplay: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IScriptErrorListDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {F3470F24-15FD-11D2-BB2E-00805FF7EFCA}
// *********************************************************************//
  IScriptErrorListDisp = dispinterface
    ['{F3470F24-15FD-11D2-BB2E-00805FF7EFCA}']
    procedure advanceError; dispid 10;
    procedure retreatError; dispid 11;
    function  canAdvanceError: Integer; dispid 12;
    function  canRetreatError: Integer; dispid 13;
    function  getErrorLine: Integer; dispid 14;
    function  getErrorChar: Integer; dispid 15;
    function  getErrorCode: Integer; dispid 16;
    function  getErrorMsg: WideString; dispid 17;
    function  getErrorUrl: WideString; dispid 18;
    function  getAlwaysShowLockState: Integer; dispid 23;
    function  getDetailsPaneOpen: Integer; dispid 19;
    procedure setDetailsPaneOpen(fDetailsPaneOpen: Integer); dispid 20;
    function  getPerErrorDisplay: Integer; dispid 21;
    procedure setPerErrorDisplay(fPerErrorDisplay: Integer); dispid 22;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TWebBrowser_V1
// Help String_     : WebBrowser Control
// Default Interface: IWebBrowser
// Def. Intf. DISP? : No
// Event   Interface: DWebBrowserEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TWebBrowser_V1BeforeNavigate = procedure(Sender: TObject; const URL: WideString; Flags: Integer; 
                                                            const TargetFrameName: WideString; 
                                                            var PostData: OleVariant; 
                                                            const Headers: WideString; 
                                                            var Cancel: WordBool) of object;
  TWebBrowser_V1NavigateComplete = procedure(Sender: TObject; const URL: WideString) of object;
  TWebBrowser_V1StatusTextChange = procedure(Sender: TObject; const Text: WideString) of object;
  TWebBrowser_V1ProgressChange = procedure(Sender: TObject; Progress: Integer; ProgressMax: Integer) of object;
  TWebBrowser_V1CommandStateChange = procedure(Sender: TObject; Command: Integer; Enable: WordBool) of object;
  TWebBrowser_V1NewWindow = procedure(Sender: TObject; const URL: WideString; Flags: Integer; 
                                                       const TargetFrameName: WideString; 
                                                       var PostData: OleVariant; 
                                                       const Headers: WideString; 
                                                       var Processed: WordBool) of object;
  TWebBrowser_V1TitleChange = procedure(Sender: TObject; const Text: WideString) of object;
  TWebBrowser_V1FrameBeforeNavigate = procedure(Sender: TObject; const URL: WideString; 
                                                                 Flags: Integer; 
                                                                 const TargetFrameName: WideString; 
                                                                 var PostData: OleVariant; 
                                                                 const Headers: WideString; 
                                                                 var Cancel: WordBool) of object;
  TWebBrowser_V1FrameNavigateComplete = procedure(Sender: TObject; const URL: WideString) of object;
  TWebBrowser_V1FrameNewWindow = procedure(Sender: TObject; const URL: WideString; Flags: Integer; 
                                                            const TargetFrameName: WideString; 
                                                            var PostData: OleVariant; 
                                                            const Headers: WideString; 
                                                            var Processed: WordBool) of object;
  TWebBrowser_V1Quit = procedure(Sender: TObject; var Cancel: WordBool) of object;
  TWebBrowser_V1PropertyChange = procedure(Sender: TObject; const Property_: WideString) of object;

  TWebBrowser_V1 = class(TOleControl)
  private
    FOnBeforeNavigate: TWebBrowser_V1BeforeNavigate;
    FOnNavigateComplete: TWebBrowser_V1NavigateComplete;
    FOnStatusTextChange: TWebBrowser_V1StatusTextChange;
    FOnProgressChange: TWebBrowser_V1ProgressChange;
    FOnDownloadComplete: TNotifyEvent;
    FOnCommandStateChange: TWebBrowser_V1CommandStateChange;
    FOnDownloadBegin: TNotifyEvent;
    FOnNewWindow: TWebBrowser_V1NewWindow;
    FOnTitleChange: TWebBrowser_V1TitleChange;
    FOnFrameBeforeNavigate: TWebBrowser_V1FrameBeforeNavigate;
    FOnFrameNavigateComplete: TWebBrowser_V1FrameNavigateComplete;
    FOnFrameNewWindow: TWebBrowser_V1FrameNewWindow;
    FOnQuit: TWebBrowser_V1Quit;
    FOnWindowMove: TNotifyEvent;
    FOnWindowResize: TNotifyEvent;
    FOnWindowActivate: TNotifyEvent;
    FOnPropertyChange: TWebBrowser_V1PropertyChange;
    FIntf: IWebBrowser;
    function  GetControlInterface: IWebBrowser;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function  Get_Application: IDispatch;
    function  Get_Parent: IDispatch;
    function  Get_Container: IDispatch;
    function  Get_Document: IDispatch;
  public
    procedure GoBack;
    procedure GoForward;
    procedure GoHome;
    procedure GoSearch;
    procedure Navigate(const URL: WideString); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; var TargetFrameName: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; 
                       var TargetFrameName: OleVariant; var PostData: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; 
                       var TargetFrameName: OleVariant; var PostData: OleVariant; 
                       var Headers: OleVariant); overload;
    procedure Refresh;
    procedure Refresh2; overload;
    procedure Refresh2(var Level: OleVariant); overload;
    procedure Stop;
    property  ControlInterface: IWebBrowser read GetControlInterface;
    property  DefaultInterface: IWebBrowser read GetControlInterface;
    property Application: IDispatch index 200 read GetIDispatchProp;
    property Parent: IDispatch index 201 read GetIDispatchProp;
    property Container: IDispatch index 202 read GetIDispatchProp;
    property Document: IDispatch index 203 read GetIDispatchProp;
    property TopLevelContainer: WordBool index 204 read GetWordBoolProp;
    property Type_: WideString index 205 read GetWideStringProp;
    property LocationName: WideString index 210 read GetWideStringProp;
    property LocationURL: WideString index 211 read GetWideStringProp;
    property Busy: WordBool index 212 read GetWordBoolProp;
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
    property OnBeforeNavigate: TWebBrowser_V1BeforeNavigate read FOnBeforeNavigate write FOnBeforeNavigate;
    property OnNavigateComplete: TWebBrowser_V1NavigateComplete read FOnNavigateComplete write FOnNavigateComplete;
    property OnStatusTextChange: TWebBrowser_V1StatusTextChange read FOnStatusTextChange write FOnStatusTextChange;
    property OnProgressChange: TWebBrowser_V1ProgressChange read FOnProgressChange write FOnProgressChange;
    property OnDownloadComplete: TNotifyEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnCommandStateChange: TWebBrowser_V1CommandStateChange read FOnCommandStateChange write FOnCommandStateChange;
    property OnDownloadBegin: TNotifyEvent read FOnDownloadBegin write FOnDownloadBegin;
    property OnNewWindow: TWebBrowser_V1NewWindow read FOnNewWindow write FOnNewWindow;
    property OnTitleChange: TWebBrowser_V1TitleChange read FOnTitleChange write FOnTitleChange;
    property OnFrameBeforeNavigate: TWebBrowser_V1FrameBeforeNavigate read FOnFrameBeforeNavigate write FOnFrameBeforeNavigate;
    property OnFrameNavigateComplete: TWebBrowser_V1FrameNavigateComplete read FOnFrameNavigateComplete write FOnFrameNavigateComplete;
    property OnFrameNewWindow: TWebBrowser_V1FrameNewWindow read FOnFrameNewWindow write FOnFrameNewWindow;
    property OnQuit: TWebBrowser_V1Quit read FOnQuit write FOnQuit;
    property OnWindowMove: TNotifyEvent read FOnWindowMove write FOnWindowMove;
    property OnWindowResize: TNotifyEvent read FOnWindowResize write FOnWindowResize;
    property OnWindowActivate: TNotifyEvent read FOnWindowActivate write FOnWindowActivate;
    property OnPropertyChange: TWebBrowser_V1PropertyChange read FOnPropertyChange write FOnPropertyChange;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TWebBrowser
// Help String_     : WebBrowser Control
// Default Interface: IWebBrowser2
// Def. Intf. DISP? : No
// Event   Interface: DWebBrowserEvents2
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TWebBrowserStatusTextChange = procedure(Sender: TObject; const Text: WideString) of object;
  TWebBrowserProgressChange = procedure(Sender: TObject; Progress: Integer; ProgressMax: Integer) of object;
  TWebBrowserCommandStateChange = procedure(Sender: TObject; Command: Integer; Enable: WordBool) of object;
  TWebBrowserTitleChange = procedure(Sender: TObject; const Text: WideString) of object;
  TWebBrowserPropertyChange = procedure(Sender: TObject; const szProperty: WideString) of object;
  TWebBrowserBeforeNavigate2 = procedure(Sender: TObject; const pDisp: IDispatch; 
                                                           var URL: OleVariant; 
                                                           var Flags: OleVariant; 
                                                           var TargetFrameName: OleVariant; 
                                                           var PostData: OleVariant; 
                                                           var Headers: OleVariant; 
                                                           var Cancel: WordBool) of object;
  TWebBrowserNewWindow2 = procedure(Sender: TObject; var ppDisp: IDispatch; var Cancel: WordBool) of object;
  TWebBrowserNavigateComplete2 = procedure(Sender: TObject; const pDisp: IDispatch; 
                                                             var URL: OleVariant) of object;
  TWebBrowserDocumentComplete = procedure(Sender: TObject; const pDisp: IDispatch; 
                                                            var URL: OleVariant) of object;
  TWebBrowserOnVisible = procedure(Sender: TObject; Visible: WordBool) of object;
  TWebBrowserOnToolBar = procedure(Sender: TObject; ToolBar: WordBool) of object;
  TWebBrowserOnMenuBar = procedure(Sender: TObject; MenuBar: WordBool) of object;
  TWebBrowserOnStatusBar = procedure(Sender: TObject; StatusBar: WordBool) of object;
  TWebBrowserOnFullScreen = procedure(Sender: TObject; FullScreen: WordBool) of object;
  TWebBrowserOnTheaterMode = procedure(Sender: TObject; TheaterMode: WordBool) of object;
  TWebBrowserWindowSetResizable = procedure(Sender: TObject; Resizable: WordBool) of object;
  TWebBrowserWindowSetLeft = procedure(Sender: TObject; Left: Integer) of object;
  TWebBrowserWindowSetTop = procedure(Sender: TObject; Top: Integer) of object;
  TWebBrowserWindowSetWidth = procedure(Sender: TObject; Width: Integer) of object;
  TWebBrowserWindowSetHeight = procedure(Sender: TObject; Height: Integer) of object;
  TWebBrowserWindowClosing = procedure(Sender: TObject; IsChildWindow: WordBool; 
                                                         var Cancel: WordBool) of object;
  TWebBrowserClientToHostWindow = procedure(Sender: TObject; var CX: Integer; var CY: Integer) of object;
  TWebBrowserSetSecureLockIcon = procedure(Sender: TObject; SecureLockIcon: Integer) of object;
  TWebBrowserFileDownload = procedure(Sender: TObject; ActiveDocument: WordBool; 
                                                        var Cancel: WordBool) of object;
  TWebBrowserNavigateError = procedure(Sender: TObject; const pDisp: IDispatch; 
                                                         var URL: OleVariant; 
                                                         var Frame: OleVariant; 
                                                         var StatusCode: OleVariant; 
                                                         var Cancel: WordBool) of object;
  TWebBrowserPrintTemplateInstantiation = procedure(Sender: TObject; const pDisp: IDispatch) of object;
  TWebBrowserPrintTemplateTeardown = procedure(Sender: TObject; const pDisp: IDispatch) of object;
  TWebBrowserUpdatePageStatus = procedure(Sender: TObject; const pDisp: IDispatch; 
                                                            var nPage: OleVariant; 
                                                            var fDone: OleVariant) of object;
  TWebBrowserPrivacyImpactedStateChange = procedure(Sender: TObject; bImpacted: WordBool) of object;
  TWebBrowserNewWindow3 = procedure(Sender: TObject; var ppDisp: IDispatch; var Cancel: WordBool; 
                                                      dwFlags: LongWord; 
                                                      const bstrUrlContext: WideString; 
                                                      const bstrUrl: WideString) of object;
  TWebBrowserSetPhishingFilterStatus = procedure(Sender: TObject; PhishingFilterStatus: Integer) of object;
  TWebBrowserWindowStateChanged = procedure(Sender: TObject; dwWindowStateFlags: LongWord; 
                                                              dwValidFlagsMask: LongWord) of object;

  TWebBrowser = class(TOleControl)
  private
    FOnStatusTextChange: TWebBrowserStatusTextChange;
    FOnProgressChange: TWebBrowserProgressChange;
    FOnCommandStateChange: TWebBrowserCommandStateChange;
    FOnDownloadBegin: TNotifyEvent;
    FOnDownloadComplete: TNotifyEvent;
    FOnTitleChange: TWebBrowserTitleChange;
    FOnPropertyChange: TWebBrowserPropertyChange;
    FOnBeforeNavigate2: TWebBrowserBeforeNavigate2;
    FOnNewWindow2: TWebBrowserNewWindow2;
    FOnNavigateComplete2: TWebBrowserNavigateComplete2;
    FOnDocumentComplete: TWebBrowserDocumentComplete;
    FOnQuit: TNotifyEvent;
    FOnVisible: TWebBrowserOnVisible;
    FOnToolBar: TWebBrowserOnToolBar;
    FOnMenuBar: TWebBrowserOnMenuBar;
    FOnStatusBar: TWebBrowserOnStatusBar;
    FOnFullScreen: TWebBrowserOnFullScreen;
    FOnTheaterMode: TWebBrowserOnTheaterMode;
    FOnWindowSetResizable: TWebBrowserWindowSetResizable;
    FOnWindowSetLeft: TWebBrowserWindowSetLeft;
    FOnWindowSetTop: TWebBrowserWindowSetTop;
    FOnWindowSetWidth: TWebBrowserWindowSetWidth;
    FOnWindowSetHeight: TWebBrowserWindowSetHeight;
    FOnWindowClosing: TWebBrowserWindowClosing;
    FOnClientToHostWindow: TWebBrowserClientToHostWindow;
    FOnSetSecureLockIcon: TWebBrowserSetSecureLockIcon;
    FOnFileDownload: TWebBrowserFileDownload;
    FOnNavigateError: TWebBrowserNavigateError;
    FOnPrintTemplateInstantiation: TWebBrowserPrintTemplateInstantiation;
    FOnPrintTemplateTeardown: TWebBrowserPrintTemplateTeardown;
    FOnUpdatePageStatus: TWebBrowserUpdatePageStatus;
    FOnPrivacyImpactedStateChange: TWebBrowserPrivacyImpactedStateChange;
    FOnNewWindow3: TWebBrowserNewWindow3;
    FOnSetPhishingFilterStatus: TWebBrowserSetPhishingFilterStatus;
    FOnWindowStateChanged: TWebBrowserWindowStateChanged;
    FIntf: IWebBrowser2;
    function  GetControlInterface: IWebBrowser2;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function  Get_Application: IDispatch;
    function  Get_Parent: IDispatch;
    function  Get_Container: IDispatch;
    function  Get_Document: IDispatch;
  public
    procedure GoBack;
    procedure GoForward;
    procedure GoHome;
    procedure GoSearch;
    procedure Navigate(const URL: WideString); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; var TargetFrameName: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; 
                       var TargetFrameName: OleVariant; var PostData: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; 
                       var TargetFrameName: OleVariant; var PostData: OleVariant; 
                       var Headers: OleVariant); overload;
    procedure Refresh;
    procedure Refresh2; overload;
    procedure Refresh2(var Level: OleVariant); overload;
    procedure Stop;
    procedure Quit;
    procedure ClientToWindow(var pcx: SYSINT; var pcy: SYSINT);
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant);
    function  GetProperty(const Property_: WideString): OleVariant;
    procedure Navigate2(var URL: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant; var TargetFrameName: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                        var TargetFrameName: OleVariant; var PostData: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                        var TargetFrameName: OleVariant; var PostData: OleVariant; 
                        var Headers: OleVariant); overload;
    function  QueryStatusWB(cmdID: OLECMDID): OLECMDF;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT); overload;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant); overload;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant; 
                     var pvaOut: OleVariant); overload;
    procedure ShowBrowserBar(var pvaClsid: OleVariant); overload;
    procedure ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant); overload;
    procedure ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant; 
                             var pvarSize: OleVariant); overload;
    property  ControlInterface: IWebBrowser2 read GetControlInterface;
    property  DefaultInterface: IWebBrowser2 read GetControlInterface;
    property Application: IDispatch index 200 read GetIDispatchProp;
    property Parent: IDispatch index 201 read GetIDispatchProp;
    property Container: IDispatch index 202 read GetIDispatchProp;
    property Document: IDispatch index 203 read GetIDispatchProp;
    property TopLevelContainer: WordBool index 204 read GetWordBoolProp;
    property Type_: WideString index 205 read GetWideStringProp;
    property LocationName: WideString index 210 read GetWideStringProp;
    property LocationURL: WideString index 211 read GetWideStringProp;
    property Busy: WordBool index 212 read GetWordBoolProp;
    property Name: WideString index 0 read GetWideStringProp;
    property HWND: Integer index -515 read GetIntegerProp;
    property FullName: WideString index 400 read GetWideStringProp;
    property Path: WideString index 401 read GetWideStringProp;
    property ReadyState: TOleEnum index -525 read GetTOleEnumProp;
  published
    property  TabStop;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property Visible: WordBool index 402 read GetWordBoolProp write SetWordBoolProp stored False;
    property StatusBar: WordBool index 403 read GetWordBoolProp write SetWordBoolProp stored False;
    property StatusText: WideString index 404 read GetWideStringProp write SetWideStringProp stored False;
    property ToolBar: Integer index 405 read GetIntegerProp write SetIntegerProp stored False;
    property MenuBar: WordBool index 406 read GetWordBoolProp write SetWordBoolProp stored False;
    property FullScreen: WordBool index 407 read GetWordBoolProp write SetWordBoolProp stored False;
    property Offline: WordBool index 550 read GetWordBoolProp write SetWordBoolProp stored False;
    property Silent: WordBool index 551 read GetWordBoolProp write SetWordBoolProp stored False;
    property RegisterAsBrowser: WordBool index 552 read GetWordBoolProp write SetWordBoolProp stored False;
    property RegisterAsDropTarget: WordBool index 553 read GetWordBoolProp write SetWordBoolProp stored False;
    property TheaterMode: WordBool index 554 read GetWordBoolProp write SetWordBoolProp stored False;
    property AddressBar: WordBool index 555 read GetWordBoolProp write SetWordBoolProp stored False;
    property Resizable: WordBool index 556 read GetWordBoolProp write SetWordBoolProp stored False;
    property OnStatusTextChange: TWebBrowserStatusTextChange read FOnStatusTextChange write FOnStatusTextChange;
    property OnProgressChange: TWebBrowserProgressChange read FOnProgressChange write FOnProgressChange;
    property OnCommandStateChange: TWebBrowserCommandStateChange read FOnCommandStateChange write FOnCommandStateChange;
    property OnDownloadBegin: TNotifyEvent read FOnDownloadBegin write FOnDownloadBegin;
    property OnDownloadComplete: TNotifyEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnTitleChange: TWebBrowserTitleChange read FOnTitleChange write FOnTitleChange;
    property OnPropertyChange: TWebBrowserPropertyChange read FOnPropertyChange write FOnPropertyChange;
    property OnBeforeNavigate2: TWebBrowserBeforeNavigate2 read FOnBeforeNavigate2 write FOnBeforeNavigate2;
    property OnNewWindow2: TWebBrowserNewWindow2 read FOnNewWindow2 write FOnNewWindow2;
    property OnNavigateComplete2: TWebBrowserNavigateComplete2 read FOnNavigateComplete2 write FOnNavigateComplete2;
    property OnDocumentComplete: TWebBrowserDocumentComplete read FOnDocumentComplete write FOnDocumentComplete;
    property OnQuit: TNotifyEvent read FOnQuit write FOnQuit;
    property OnVisible: TWebBrowserOnVisible read FOnVisible write FOnVisible;
    property OnToolBar: TWebBrowserOnToolBar read FOnToolBar write FOnToolBar;
    property OnMenuBar: TWebBrowserOnMenuBar read FOnMenuBar write FOnMenuBar;
    property OnStatusBar: TWebBrowserOnStatusBar read FOnStatusBar write FOnStatusBar;
    property OnFullScreen: TWebBrowserOnFullScreen read FOnFullScreen write FOnFullScreen;
    property OnTheaterMode: TWebBrowserOnTheaterMode read FOnTheaterMode write FOnTheaterMode;
    property OnWindowSetResizable: TWebBrowserWindowSetResizable read FOnWindowSetResizable write FOnWindowSetResizable;
    property OnWindowSetLeft: TWebBrowserWindowSetLeft read FOnWindowSetLeft write FOnWindowSetLeft;
    property OnWindowSetTop: TWebBrowserWindowSetTop read FOnWindowSetTop write FOnWindowSetTop;
    property OnWindowSetWidth: TWebBrowserWindowSetWidth read FOnWindowSetWidth write FOnWindowSetWidth;
    property OnWindowSetHeight: TWebBrowserWindowSetHeight read FOnWindowSetHeight write FOnWindowSetHeight;
    property OnWindowClosing: TWebBrowserWindowClosing read FOnWindowClosing write FOnWindowClosing;
    property OnClientToHostWindow: TWebBrowserClientToHostWindow read FOnClientToHostWindow write FOnClientToHostWindow;
    property OnSetSecureLockIcon: TWebBrowserSetSecureLockIcon read FOnSetSecureLockIcon write FOnSetSecureLockIcon;
    property OnFileDownload: TWebBrowserFileDownload read FOnFileDownload write FOnFileDownload;
    property OnNavigateError: TWebBrowserNavigateError read FOnNavigateError write FOnNavigateError;
    property OnPrintTemplateInstantiation: TWebBrowserPrintTemplateInstantiation read FOnPrintTemplateInstantiation write FOnPrintTemplateInstantiation;
    property OnPrintTemplateTeardown: TWebBrowserPrintTemplateTeardown read FOnPrintTemplateTeardown write FOnPrintTemplateTeardown;
    property OnUpdatePageStatus: TWebBrowserUpdatePageStatus read FOnUpdatePageStatus write FOnUpdatePageStatus;
    property OnPrivacyImpactedStateChange: TWebBrowserPrivacyImpactedStateChange read FOnPrivacyImpactedStateChange write FOnPrivacyImpactedStateChange;
    property OnNewWindow3: TWebBrowserNewWindow3 read FOnNewWindow3 write FOnNewWindow3;
    property OnSetPhishingFilterStatus: TWebBrowserSetPhishingFilterStatus read FOnSetPhishingFilterStatus write FOnSetPhishingFilterStatus;
    property OnWindowStateChanged: TWebBrowserWindowStateChanged read FOnWindowStateChanged write FOnWindowStateChanged;
  end;

// *********************************************************************//
// The Class CoInternetExplorer provides a Create and CreateRemote method to          
// create instances of the default interface IWebBrowser2 exposed by              
// the CoClass InternetExplorer. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoInternetExplorer = class
    class function Create: IWebBrowser2;
    class function CreateRemote(const MachineName: AnsiString): IWebBrowser2;
  end;

// *********************************************************************//
// The Class CoShellBrowserWindow provides a Create and CreateRemote method to          
// create instances of the default interface IWebBrowser2 exposed by              
// the CoClass ShellBrowserWindow. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoShellBrowserWindow = class
    class function Create: IWebBrowser2;
    class function CreateRemote(const MachineName: AnsiString): IWebBrowser2;
  end;

// *********************************************************************//
// The Class CoShellWindows provides a Create and CreateRemote method to          
// create instances of the default interface IShellWindows exposed by              
// the CoClass ShellWindows. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoShellWindows = class
    class function Create: IShellWindows;
    class function CreateRemote(const MachineName: AnsiString): IShellWindows;
  end;

// *********************************************************************//
// The Class CoShellUIHelper provides a Create and CreateRemote method to          
// create instances of the default interface IShellUIHelper2 exposed by              
// the CoClass ShellUIHelper. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoShellUIHelper = class
    class function Create: IShellUIHelper2;
    class function CreateRemote(const MachineName: AnsiString): IShellUIHelper2;
  end;

// *********************************************************************//
// The Class CoShellNameSpace provides a Create and CreateRemote method to          
// create instances of the default interface IShellNameSpace exposed by              
// the CoClass ShellNameSpace. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoShellNameSpace = class
    class function Create: IShellNameSpace;
    class function CreateRemote(const MachineName: AnsiString): IShellNameSpace;
  end;

// *********************************************************************//
// The Class CoShellShellNameSpace provides a Create and CreateRemote method to          
// create instances of the default interface IShellNameSpace exposed by              
// the CoClass ShellShellNameSpace. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoShellShellNameSpace = class
    class function Create: IShellNameSpace;
    class function CreateRemote(const MachineName: AnsiString): IShellNameSpace;
  end;

// *********************************************************************//
// The Class CoCScriptErrorList provides a Create and CreateRemote method to          
// create instances of the default interface IScriptErrorList exposed by              
// the CoClass CScriptErrorList. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCScriptErrorList = class
    class function Create: IScriptErrorList;
    class function CreateRemote(const MachineName: AnsiString): IScriptErrorList;
  end;

//procedure Register;

implementation

uses ComObj;

procedure TWebBrowser_V1.InitControlData;
const
  CEventDispIDs: array [0..16] of DWORD = (
    $00000064, $00000065, $00000066, $0000006C, $00000068, $00000069,
    $0000006A, $0000006B, $00000071, $000000C8, $000000C9, $000000CC,
    $00000067, $0000006D, $0000006E, $0000006F, $00000070);
  CControlData: TControlData2 = (
    ClassID: '{EAB22AC3-30C1-11CF-A7EB-0000C05BAE0B}';
    EventIID: '{EAB22AC2-30C1-11CF-A7EB-0000C05BAE0B}';
    EventCount: 17;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$80040111*);
    Flags: $00000000;
    Version: 401);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnBeforeNavigate) - Cardinal(Self);
end;

procedure TWebBrowser_V1.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IWebBrowser;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TWebBrowser_V1.GetControlInterface: IWebBrowser;
begin
  CreateControl;
  Result := FIntf;
end;

function  TWebBrowser_V1.Get_Application: IDispatch;
begin
  Result := DefaultInterface.Get_Application;
end;

function  TWebBrowser_V1.Get_Parent: IDispatch;
begin
  Result := DefaultInterface.Get_Parent;
end;

function  TWebBrowser_V1.Get_Container: IDispatch;
begin
  Result := DefaultInterface.Get_Container;
end;

function  TWebBrowser_V1.Get_Document: IDispatch;
begin
  Result := DefaultInterface.Get_Document;
end;

procedure TWebBrowser_V1.GoBack;
begin
  DefaultInterface.GoBack;
end;

procedure TWebBrowser_V1.GoForward;
begin
  DefaultInterface.GoForward;
end;

procedure TWebBrowser_V1.GoHome;
begin
  DefaultInterface.GoHome;
end;

procedure TWebBrowser_V1.GoSearch;
begin
  DefaultInterface.GoSearch;
end;

procedure TWebBrowser_V1.Navigate(const URL: WideString);
var
  Param: OleVariant;
begin
  Param := EmptyParam;
  DefaultInterface.Navigate(URL, Param, Param, Param, Param);
end;

procedure TWebBrowser_V1.Navigate(const URL: WideString; var Flags: OleVariant);
var
  Param: OleVariant;
begin
  Param := EmptyParam;
  DefaultInterface.Navigate(URL, Flags, Param, Param, Param);
end;

procedure TWebBrowser_V1.Navigate(const URL: WideString; var Flags: OleVariant;
                                  var TargetFrameName: OleVariant);
var
  Param: OleVariant;
begin
  Param := EmptyParam;
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, Param, Param);
end;

procedure TWebBrowser_V1.Navigate(const URL: WideString; var Flags: OleVariant;
                                  var TargetFrameName: OleVariant; var PostData: OleVariant);
var
  Param: OleVariant;
begin
  Param := EmptyParam;
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, PostData, Param);
end;

procedure TWebBrowser_V1.Navigate(const URL: WideString; var Flags: OleVariant; 
                                  var TargetFrameName: OleVariant; var PostData: OleVariant; 
                                  var Headers: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, PostData, Headers);
end;

procedure TWebBrowser_V1.Refresh;
begin
  DefaultInterface.Refresh;
end;

procedure TWebBrowser_V1.Refresh2;
var
  Param: OleVariant;
begin
  Param := EmptyParam;
  DefaultInterface.Refresh2(Param);
end;

procedure TWebBrowser_V1.Refresh2(var Level: OleVariant);
begin
  DefaultInterface.Refresh2(Level);
end;

procedure TWebBrowser_V1.Stop;
begin
  DefaultInterface.Stop;
end;

procedure TWebBrowser.InitControlData;
const
  CEventDispIDs: array [0..34] of DWORD = (
    $00000066, $0000006C, $00000069, $0000006A, $00000068, $00000071,
    $00000070, $000000FA, $000000FB, $000000FC, $00000103, $000000FD,
    $000000FE, $000000FF, $00000100, $00000101, $00000102, $00000104,
    $00000106, $00000108, $00000109, $0000010A, $0000010B, $00000107,
    $0000010C, $0000010D, $0000010E, $0000010F, $000000E1, $000000E2,
    $000000E3, $00000110, $00000111, $0000011A, $0000011B);
  CControlData: TControlData2 = (
    ClassID: '{8856F961-340A-11D0-A96B-00C04FD705A2}';
    EventIID: '{34A715A0-6587-11D0-924A-0020AFC7AC4D}';
    EventCount: 35;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$80040111*);
    Flags: $00000000;
    Version: 401);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnStatusTextChange) - Cardinal(Self);
end;

procedure TWebBrowser.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IWebBrowser2;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TWebBrowser.GetControlInterface: IWebBrowser2;
begin
  CreateControl;
  Result := FIntf;
end;

function  TWebBrowser.Get_Application: IDispatch;
begin
  Result := DefaultInterface.Get_Application;
end;

function  TWebBrowser.Get_Parent: IDispatch;
begin
  Result := DefaultInterface.Get_Parent;
end;

function  TWebBrowser.Get_Container: IDispatch;
begin
  Result := DefaultInterface.Get_Container;
end;

function  TWebBrowser.Get_Document: IDispatch;
begin
  Result := DefaultInterface.Get_Document;
end;

procedure TWebBrowser.GoBack;
begin
  DefaultInterface.GoBack;
end;

procedure TWebBrowser.GoForward;
begin
  DefaultInterface.GoForward;
end;

procedure TWebBrowser.GoHome;
begin
  DefaultInterface.GoHome;
end;

procedure TWebBrowser.GoSearch;
begin
  DefaultInterface.GoSearch;
end;

procedure TWebBrowser.Navigate(const URL: WideString);
var
  Param: OleVariant;
begin
  Param := EmptyParam;
  DefaultInterface.Navigate(URL, Param, Param, Param, Param);
end;

procedure TWebBrowser.Navigate(const URL: WideString; var Flags: OleVariant);
var
  Param: OleVariant;
begin
  Param := EmptyParam;
  DefaultInterface.Navigate(URL, Flags, Param, Param, Param);
end;

procedure TWebBrowser.Navigate(const URL: WideString; var Flags: OleVariant;
                                var TargetFrameName: OleVariant);
var
  Param: OleVariant;
begin
  Param := EmptyParam;
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, Param, Param);
end;

procedure TWebBrowser.Navigate(const URL: WideString; var Flags: OleVariant;
                                var TargetFrameName: OleVariant; var PostData: OleVariant);
var
  Param: OleVariant;
begin
  Param := EmptyParam;
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, PostData, Param);
end;

procedure TWebBrowser.Navigate(const URL: WideString; var Flags: OleVariant;
                                var TargetFrameName: OleVariant; var PostData: OleVariant;
                                var Headers: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, PostData, Headers);
end;

procedure TWebBrowser.Refresh;
begin
  DefaultInterface.Refresh;
end;

procedure TWebBrowser.Refresh2;
var
  Param: OleVariant;
begin
  Param := EmptyParam;
  DefaultInterface.Refresh2(Param);
end;

procedure TWebBrowser.Refresh2(var Level: OleVariant);
begin
  DefaultInterface.Refresh2(Level);
end;

procedure TWebBrowser.Stop;
begin
  DefaultInterface.Stop;
end;

procedure TWebBrowser.Quit;
begin
  DefaultInterface.Quit;
end;

procedure TWebBrowser.ClientToWindow(var pcx: SYSINT; var pcy: SYSINT);
begin
  DefaultInterface.ClientToWindow(pcx, pcy);
end;

procedure TWebBrowser.PutProperty(const Property_: WideString; vtValue: OleVariant);
begin
  DefaultInterface.PutProperty(Property_, vtValue);
end;

function  TWebBrowser.GetProperty(const Property_: WideString): OleVariant;
begin
  Result := DefaultInterface.GetProperty(Property_);
end;

procedure TWebBrowser.Navigate2(var URL: OleVariant);
var
  Param: OleVariant;
begin
  Param := EmptyParam;
  DefaultInterface.Navigate2(URL, Param, Param, Param, Param);
end;

procedure TWebBrowser.Navigate2(var URL: OleVariant; var Flags: OleVariant);
var
  Param: OleVariant;
begin
  Param := EmptyParam;
  DefaultInterface.Navigate2(URL, Flags, Param, Param, Param);
end;

procedure TWebBrowser.Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                                 var TargetFrameName: OleVariant);
var
  Param: OleVariant;
begin
  Param := EmptyParam;
  DefaultInterface.Navigate2(URL, Flags, TargetFrameName, Param, Param);
end;

procedure TWebBrowser.Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                                 var TargetFrameName: OleVariant; var PostData: OleVariant);
var
  Param: OleVariant;
begin
  Param := EmptyParam;
  DefaultInterface.Navigate2(URL, Flags, TargetFrameName, PostData, Param);
end;

procedure TWebBrowser.Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                                 var TargetFrameName: OleVariant; var PostData: OleVariant; 
                                 var Headers: OleVariant);
begin
  DefaultInterface.Navigate2(URL, Flags, TargetFrameName, PostData, Headers);
end;

function  TWebBrowser.QueryStatusWB(cmdID: OLECMDID): OLECMDF;
begin
  Result := DefaultInterface.QueryStatusWB(cmdID);
end;

procedure TWebBrowser.ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT);
var
  Param: OleVariant;
begin
  Param := EmptyParam;
  DefaultInterface.ExecWB(cmdID, cmdexecopt, Param, Param);
end;

procedure TWebBrowser.ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant);
var
  Param: OleVariant;
begin
  Param := EmptyParam;
  DefaultInterface.ExecWB(cmdID, cmdexecopt, pvaIn, Param);
end;

procedure TWebBrowser.ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant; 
                              var pvaOut: OleVariant);
begin
  DefaultInterface.ExecWB(cmdID, cmdexecopt, pvaIn, pvaOut);
end;

procedure TWebBrowser.ShowBrowserBar(var pvaClsid: OleVariant);
var
  Param: OleVariant;
begin
  Param := EmptyParam;
  DefaultInterface.ShowBrowserBar(pvaClsid, Param, Param);
end;

procedure TWebBrowser.ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant);
var
  Param: OleVariant;
begin
  Param := EmptyParam;
  DefaultInterface.ShowBrowserBar(pvaClsid, pvarShow, Param);
end;

procedure TWebBrowser.ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant;
                                      var pvarSize: OleVariant);
begin
  DefaultInterface.ShowBrowserBar(pvaClsid, pvarShow, pvarSize);
end;

class function CoInternetExplorer.Create: IWebBrowser2;
begin
  Result := CreateComObject(CLASS_InternetExplorer) as IWebBrowser2;
end;

class function CoInternetExplorer.CreateRemote(const MachineName: AnsiString): IWebBrowser2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_InternetExplorer) as IWebBrowser2;
end;

class function CoShellBrowserWindow.Create: IWebBrowser2;
begin
  Result := CreateComObject(CLASS_ShellBrowserWindow) as IWebBrowser2;
end;

class function CoShellBrowserWindow.CreateRemote(const MachineName: AnsiString): IWebBrowser2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ShellBrowserWindow) as IWebBrowser2;
end;

class function CoShellWindows.Create: IShellWindows;
begin
  Result := CreateComObject(CLASS_ShellWindows) as IShellWindows;
end;

class function CoShellWindows.CreateRemote(const MachineName: AnsiString): IShellWindows;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ShellWindows) as IShellWindows;
end;

class function CoShellUIHelper.Create: IShellUIHelper2;
begin
  Result := CreateComObject(CLASS_ShellUIHelper) as IShellUIHelper2;
end;

class function CoShellUIHelper.CreateRemote(const MachineName: AnsiString): IShellUIHelper2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ShellUIHelper) as IShellUIHelper2;
end;

class function CoShellNameSpace.Create: IShellNameSpace;
begin
  Result := CreateComObject(CLASS_ShellNameSpace) as IShellNameSpace;
end;

class function CoShellNameSpace.CreateRemote(const MachineName: AnsiString): IShellNameSpace;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ShellNameSpace) as IShellNameSpace;
end;

class function CoShellShellNameSpace.Create: IShellNameSpace;
begin
  Result := CreateComObject(CLASS_ShellShellNameSpace) as IShellNameSpace;
end;

class function CoShellShellNameSpace.CreateRemote(const MachineName: AnsiString): IShellNameSpace;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ShellShellNameSpace) as IShellNameSpace;
end;

class function CoCScriptErrorList.Create: IScriptErrorList;
begin
  Result := CreateComObject(CLASS_CScriptErrorList) as IScriptErrorList;
end;

class function CoCScriptErrorList.CreateRemote(const MachineName: AnsiString): IScriptErrorList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CScriptErrorList) as IScriptErrorList;
end;

{
//Registration disabled to avoid conflicts with default TWebBrowser
procedure Register;
begin
  RegisterComponents('ActiveX',[TWebBrowser_V1, TWebBrowser]);
end;
}

end.
