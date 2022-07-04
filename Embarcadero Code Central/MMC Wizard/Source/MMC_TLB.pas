unit MMC_TLB;

// *** Fix bug in TLB
// ************************************************************************ //
// WARNING                                                                  //
// -------                                                                  //
// The types declared in this file were generated from data read from a     //
// Type Library. If this type library is explicitly or indirectly (via      //
// another type library referring to this type library) re-imported, or the //
// 'Refresh' command of the Type Library Editor activated while editing the //
// Type Library, the contents of this file will be regenerated and all      //
// manual modifications will be lost.                                       //
// ************************************************************************ //

// PASTLWTR : $Revision:   1.27.1.0  $
// File generated on 7/27/99 11:49:37 AM from Type Library described below.

// ************************************************************************ //
// Type Lib: mmc.tlb
// IID\LCID: {EAB22AC0-30C1-11CF-A7EB-0000C05BAE0B}\0
// Helpfile: 
// HelpString: Microsoft Internet Controls
// Version:    0.0
// ************************************************************************ //

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:      //
//   Type Libraries     : LIBID_xxxx                                    //
//   CoClasses          : CLASS_xxxx                                    //
//   DISPInterfaces     : DIID_xxxx                                     //
//   Non-DISP interfaces: IID_xxxx                                      //
// *********************************************************************//
const
  LIBID_MMC: TGUID = '{EAB22AC0-30C1-11CF-A7EB-0000C05BAE0B}';
  IID_IConsole: TGUID = '{43136EB1-D36C-11CF-ADBC-00AA00A80033}';
  IID_IHeaderCtrl: TGUID = '{43136EB3-D36C-11CF-ADBC-00AA00A80033}';
  IID_IToolbar: TGUID = '{43136EB9-D36C-11CF-ADBC-00AA00A80033}';
  IID_IImageList: TGUID = '{43136EB8-D36C-11CF-ADBC-00AA00A80033}';
  IID_IConsoleVerb: TGUID = '{E49F7A60-74AF-11D0-A286-00C04FD8FE93}';
  IID_IResultData: TGUID = '{31DA5FA0-E0EB-11CF-9F21-00AA003CA9F6}';
  IID_IConsoleNameSpace: TGUID = '{BEDEB620-F24D-11CF-8AFC-00AA003CA9F6}';
  IID_IPropertySheetProvider: TGUID = '{85DE64DE-EF21-11CF-A285-00C04FD8DBE6}';
  IID_IComponent: TGUID = '{43136EB2-D36C-11CF-ADBC-00AA00A80033}';
  IID_IPropertySheetCallback: TGUID = '{85DE64DD-EF21-11CF-A285-00C04FD8DBE6}';
  IID_IContextMenuCallback: TGUID = '{43136EB7-D36C-11CF-ADBC-00AA00A80033}';
  IID_IContextMenuProvider: TGUID = '{43136EB6-D36C-11CF-ADBC-00AA00A80033}';
  IID_IControlbar: TGUID = '{69FB811E-6C1C-11D0-A2CB-00C04FD909DD}';
  IID_IExtendControlbar: TGUID = '{49506520-6F40-11D0-A98B-00C04FD8D565}';
  IID_IMenuButton: TGUID = '{951ED750-D080-11D0-B197-000000000000}';
  IID_IConsole2: TGUID = '{103D842A-AA63-11D1-A7E1-00C04FD8D565}';
  IID_IHeaderCtrl2: TGUID = '{9757ABB8-1B32-11D1-A7CE-00C04FD8D565}';
  IID_IConsoleNameSpace2: TGUID = '{255F18CC-65DB-11D1-A7DC-00C04FD8D565}';
  IID_IDisplayHelp: TGUID = '{CC593830-B926-11D1-8063-0000F875A9CE}';
  IID_IStringTable: TGUID = '{DE40B7A4-0F65-11D2-8E25-00C04F8ECD78}';
  IID_IViewData: TGUID = '{CA656020-B47D-11D2-A692-00C04F8EF4CB}';
  IID_IComponentData: TGUID = '{955AB28A-5218-11D0-A985-00C04FD8D565}';
  IID_IExtendPropertySheet: TGUID = '{85DE64DC-EF21-11CF-A285-00C04FD8DBE6}';
  IID_IExtendContextMenu: TGUID = '{4F3B7A4F-CFAC-11CF-B8E3-00C04FD8D5B0}';
  IID_IResultDataCompare: TGUID = '{E8315A52-7A1A-11D0-A2D2-00C04FD909DD}';
  IID_ISnapinAbout: TGUID = '{1245208C-A151-11D0-A7D7-00C04FD909DD}';
  IID_IResultOwnerData: TGUID = '{9CB396D8-EA83-11D0-AEF1-00C04FB6DD2C}';
  IID_ISnapinHelp: TGUID = '{A6B15ACE-DF59-11D0-A7DD-00C04FD909DD}';
  IID_IEnumTASK: TGUID = '{338698B1-5A02-11D1-9FEC-00600832DB4A}';
  IID_IExtendPropertySheet2: TGUID = '{B7A87232-4A51-11D1-A7EA-00C04FD909DD}';
  IID_ISnapinHelp2: TGUID = '{4861A010-20F9-11D2-A510-00C04FB6DD2C}';
  IID_IExtendTaskPad: TGUID = '{8DEE6511-554D-11D1-9FEA-00600832DB4A}';
  IID_IRequiredExtensions: TGUID = '{72782D7A-A4A0-11D1-AF0F-00C04FB6DD2C}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                  //
// *********************************************************************//
// _MMC_BUTTON_STATE constants
type
  _MMC_BUTTON_STATE = TOleEnum;
const
  ENABLED = $00000001;
  CHECKED = $00000002;
  HIDDEN = $00000004;
  INDETERMINATE = $00000008;
  BUTTONPRESSED = $00000010;

// _MMC_CONSOLE_VERB constants
type
  _MMC_CONSOLE_VERB = TOleEnum;
const
  MMC_VERB__dummy_ = $00000000;

// _MMC_RESULT_VIEW_STYLE constants
type
  _MMC_RESULT_VIEW_STYLE = TOleEnum;
const
  MMC_SINGLESEL = $00000001;
  MMC_SHOWSELALWAYS = $00000002;
  MMC_NOSORTHEADER = $00000004;

// _MMC_NOTIFY_TYPE constants
type
  _MMC_NOTIFY_TYPE = TOleEnum;
const
  MMCN__dummy_ = $00000000;

// _DATA_OBJECT_TYPES constants
type
  _DATA_OBJECT_TYPES = TOleEnum;
const
  CCT_SCOPE = $00008000;
  CCT_RESULT = $00008001;
  CCT_SNAPIN_MANAGER = $00008002;
  CCT_UNINITIALIZED = $0000FFFF;

// _MMC_CONTROL_TYPE constants
type
  _MMC_CONTROL_TYPE = TOleEnum;
const
  TOOLBAR = $00000000;
  MENUBUTTON = $00000001;
  COMBOBOXBAR = $00000002;

// _MMC_TASK_DISPLAY_TYPE constants
type
  _MMC_TASK_DISPLAY_TYPE = TOleEnum;
const
  MMC_TASK_DISPLAY_UNINITIALIZED = $00000000;
  MMC_TASK_DISPLAY_TYPE_SYMBOL = $00000001;
  MMC_TASK_DISPLAY_TYPE_VANILLA_GIF = $00000002;
  MMC_TASK_DISPLAY_TYPE_CHOCOLATE_GIF = $00000003;
  MMC_TASK_DISPLAY_TYPE_BITMAP = $00000004;

// _MMC_ACTION_TYPE constants
type
  _MMC_ACTION_TYPE = TOleEnum;
const
  MMC_ACTION_UNINITIALIZED = $FFFFFFFF;
  MMC_ACTION_ID = $00000000;
  MMC_ACTION_LINK = $00000001;
  MMC_ACTION_SCRIPT = $00000002;

// _MMC_SCOPE_ITEM_STATE constants
type
  _MMC_SCOPE_ITEM_STATE = TOleEnum;
const
  MMC_SCOPE_ITEM_STATE_NORMAL = $00000001;
  MMC_SCOPE_ITEM_STATE_BOLD = $00000002;
  MMC_SCOPE_ITEM_STATE_EXPANDEDONCE = $00000003;

// _MMC_MENU_COMMAND_IDS constants
type
  _MMC_MENU_COMMAND_IDS = TOleEnum;
const
  MMCC_STANDARD_VIEW_SELECT = $FFFFFFFF;

// _MMC_FILTER_TYPE constants
type
  _MMC_FILTER_TYPE = TOleEnum;
const
  MMC_STRING_FILTER = $00000000;
  MMC_INT_FILTER = $00000001;
  MMC_FILTER_NOVALUE = $00008000;

// _MMC_FILTER_CHANGE_CODE constants
type
  _MMC_FILTER_CHANGE_CODE = TOleEnum;
const
  MFCC_DISABLE = $00000000;
  MFCC_ENABLE = $00000001;
  MFCC_VALUE_CHANGE = $00000002;

// __MIDL___MIDL_itf_mmc_0116_0001 constants
type
  __MIDL___MIDL_itf_mmc_0116_0001 = TOleEnum;
const
  CCM_INSERTIONPOINTID_MASK_SPECIAL = $FFFF0000;
  CCM_INSERTIONPOINTID_MASK_SHARED = $80000000;
  CCM_INSERTIONPOINTID_MASK_CREATE_PRIMARY = $40000000;
  CCM_INSERTIONPOINTID_MASK_ADD_PRIMARY = $20000000;
  CCM_INSERTIONPOINTID_MASK_ADD_3RDPARTY = $10000000;
  CCM_INSERTIONPOINTID_MASK_RESERVED = $0FFF0000;
  CCM_INSERTIONPOINTID_MASK_FLAGINDEX = $0000001F;
  CCM_INSERTIONPOINTID_PRIMARY_TOP = $A0000000;
  CCM_INSERTIONPOINTID_PRIMARY_NEW = $A0000001;
  CCM_INSERTIONPOINTID_PRIMARY_TASK = $A0000002;
  CCM_INSERTIONPOINTID_PRIMARY_VIEW = $A0000003;
  CCM_INSERTIONPOINTID_3RDPARTY_NEW = $90000001;
  CCM_INSERTIONPOINTID_3RDPARTY_TASK = $90000002;
  CCM_INSERTIONPOINTID_ROOT_MENU = $80000000;

// __MIDL___MIDL_itf_mmc_0116_0002 constants
type
  __MIDL___MIDL_itf_mmc_0116_0002 = TOleEnum;
const
  CCM_INSERTIONALLOWED_TOP = $00000001;
  CCM_INSERTIONALLOWED_NEW = $00000002;
  CCM_INSERTIONALLOWED_TASK = $00000004;
  CCM_INSERTIONALLOWED_VIEW = $00000008;

// __MIDL___MIDL_itf_mmc_0116_0003 constants
type
  __MIDL___MIDL_itf_mmc_0116_0003 = TOleEnum;
const
  CCM_COMMANDID_MASK_RESERVED = $FFFF0000;

// __MIDL___MIDL_itf_mmc_0116_0004 constants
type
  __MIDL___MIDL_itf_mmc_0116_0004 = TOleEnum;
const
  CCM_SPECIAL_SEPARATOR = $00000001;
  CCM_SPECIAL_SUBMENU = $00000002;
  CCM_SPECIAL_DEFAULT_ITEM = $00000004;
  CCM_SPECIAL_INSERTION_POINT = $00000008;
  CCM_SPECIAL_TESTONLY = $00000010;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                  //
// *********************************************************************//
  IConsole = interface;
  IHeaderCtrl = interface;
  IToolbar = interface;
  IImageList = interface;
  IConsoleVerb = interface;
  IResultData = interface;
  IConsoleNameSpace = interface;
  IPropertySheetProvider = interface;
  IComponent = interface;
  IPropertySheetCallback = interface;
  IContextMenuCallback = interface;
  IContextMenuProvider = interface;
  IControlbar = interface;
  IExtendControlbar = interface;
  IMenuButton = interface;
  IConsole2 = interface;
  IHeaderCtrl2 = interface;
  IConsoleNameSpace2 = interface;
  IDisplayHelp = interface;
  IStringTable = interface;
  IViewData = interface;
  IComponentData = interface;
  IExtendPropertySheet = interface;
  IExtendContextMenu = interface;
  IResultDataCompare = interface;
  ISnapinAbout = interface;
  IResultOwnerData = interface;
  ISnapinHelp = interface;
  IEnumTASK = interface;
  IExtendPropertySheet2 = interface;
  ISnapinHelp2 = interface;
  IExtendTaskPad = interface;
  IRequiredExtensions = interface;

// *********************************************************************//
// Declaration of structures, unions and aliases.                       //
// *********************************************************************//
  wireHBITMAP = ^_userHBITMAP; 
  wireCLIPFORMAT = ^_userCLIPFORMAT; 
  wireSTGMEDIUM = ^_userSTGMEDIUM;
  wireFLAG_STGMEDIUM = ^_userFLAG_STGMEDIUM;
  wireASYNC_STGMEDIUM = ^_userSTGMEDIUM;
  wireHWND = ^_RemotableHandle;
  wireHICON = ^_RemotableHandle;
  wireHPALETTE = ^_userHPALETTE;


  _userBITMAP = record
    bmType: Integer;
    bmWidth: Integer;
    bmHeight: Integer;
    bmWidthBytes: Integer;
    bmPlanes: Word;
    bmBitsPixel: Word;
    cbSize: UINT;
    pBuffer: ^Shortint;
  end;

  __MIDL_IWinTypes_0007 = record
    case Integer of
      0: (hInproc: Integer);
      1: (hRemote: ^_userBITMAP);
      2: (hGlobal: Integer);
  end;

  _userHBITMAP = record
    fContext: Integer;
    u: __MIDL_IWinTypes_0007;
  end;

  _MMCButton = record
    nBitmap: SYSINT;
    idCommand: SYSINT;
    fsState: Shortint;
    fsType: Shortint;
    lpButtonText: PWideChar;
    lpTooltipText: PWideChar;
  end;


  tagFORMATETC = record
    cfFormat: wireCLIPFORMAT;
    ptd: ^tagDVTARGETDEVICE;
    dwAspect: UINT;
    lindex: Integer;
    tymed: UINT;
  end;

  __MIDL_IWinTypes_0001 = record
    case Integer of
      0: (dwValue: UINT);
      1: (pwszName: PWideChar);
  end;

  _userCLIPFORMAT = packed record
    fContext: Integer;
    u: __MIDL_IWinTypes_0001;
  end;

  tagDVTARGETDEVICE = record
    tdSize: UINT;
    tdDriverNameOffset: Word;
    tdDeviceNameOffset: Word;
    tdPortNameOffset: Word;
    tdExtDevmodeOffset: Word;
    tdData: ^Shortint;
  end;

  _BYTE_BLOB = record
    clSize: UINT;
    abData: ^Shortint;
  end;


  __MIDL_IWinTypes_0004 = record
    case Integer of
      0: (hInproc: Integer);
      1: (hRemote: ^_BYTE_BLOB);
      2: (hGlobal: Integer);
  end;

  _userHMETAFILE = record
    fContext: Integer;
    u: __MIDL_IWinTypes_0004;
  end;

  _remoteMETAFILEPICT = record
    mm: Integer;
    xExt: Integer;
    yExt: Integer;
    hMF: ^_userHMETAFILE;
  end;

  __MIDL_IWinTypes_0005 = record
    case Integer of
      0: (hInproc: Integer);
      1: (hRemote: ^_remoteMETAFILEPICT);
      2: (hGlobal: Integer);
  end;

  _userHMETAFILEPICT = record
    fContext: Integer;
    u: __MIDL_IWinTypes_0005;
  end;

  __MIDL_IWinTypes_0006 = record
    case Integer of
      0: (hInproc: Integer);
      1: (hRemote: ^_BYTE_BLOB);
      2: (hGlobal: Integer);
  end;

  _userHENHMETAFILE = record
    fContext: Integer;
    u: __MIDL_IWinTypes_0006;
  end;

  tagPALETTEENTRY = record
    peRed: Shortint;
    peGreen: Shortint;
    peBlue: Shortint;
    peFlags: Shortint;
  end;

  tagrpcLOGPALETTE = record
    palVersion: Word;
    palNumEntries: Word;
    palPalEntry: ^tagPALETTEENTRY;
  end;

  __MIDL_IWinTypes_0008 = record
    case Integer of
      0: (hInproc: Integer);
      1: (hRemote: ^tagrpcLOGPALETTE);
      2: (hGlobal: Integer);
  end;

  _userHPALETTE = record
    fContext: Integer;
    u: __MIDL_IWinTypes_0008;
  end;

  _FLAGGED_BYTE_BLOB = record
    fFlags: UINT;
    clSize: UINT;
    abData: ^Shortint;
  end;

  __MIDL_IWinTypes_0003 = record
    case Integer of
      0: (hInproc: Integer);
      1: (hRemote: ^_FLAGGED_BYTE_BLOB);
      2: (hGlobal: Integer);
  end;

  _userHGLOBAL = record
    fContext: Integer;
    u: __MIDL_IWinTypes_0003;
  end;

  __MIDL_IAdviseSink_0002 = record
    case Integer of
      0: (hBitmap: ^_userHBITMAP);
      1: (hPalette: ^_userHPALETTE);
      2: (hGeneric: ^_userHGLOBAL);
  end;

  _GDI_OBJECT = record
    ObjectType: UINT;
    u: __MIDL_IAdviseSink_0002;
  end;

  __MIDL_IAdviseSink_0003 = record
    case Integer of
      0: (hMetaFilePict: ^_userHMETAFILEPICT);
      1: (hHEnhMetaFile: ^_userHENHMETAFILE);
      2: (hGdiHandle: ^_GDI_OBJECT);
      3: (hGlobal: ^_userHGLOBAL);
      4: (lpszFileName: PWideChar);
      5: (pstm: ^_BYTE_BLOB);
      6: (pstg: ^_BYTE_BLOB);
  end;

  _STGMEDIUM_UNION = record
    tymed: UINT;
    u: __MIDL_IAdviseSink_0003;
  end;

  _userSTGMEDIUM = record
    __MIDL_0003: _STGMEDIUM_UNION;
    pUnkForRelease: IUnknown;
  end;


  _userFLAG_STGMEDIUM = record
    ContextFlags: Integer;
    fPassOwnership: Integer;
    Stgmed: _userSTGMEDIUM;
  end;


  _LARGE_INTEGER = record
    QuadPart: Comp;
  end;

  _ULARGE_INTEGER = record
    QuadPart: Largeuint;
  end;

  _FILETIME = record
    dwLowDateTime: UINT;
    dwHighDateTime: UINT;
  end;

  tagSTATSTG = record
    pwcsName: PWideChar;
    type_: UINT;
    cbSize: _ULARGE_INTEGER;
    mtime: _FILETIME;
    ctime: _FILETIME;
    atime: _FILETIME;
    grfMode: UINT;
    grfLocksSupported: UINT;
    clsid: TGUID;
    grfStateBits: UINT;
    reserved: UINT;
  end;

  tagBIND_OPTS2 = record
    cbStruct: UINT;
    grfFlags: UINT;
    grfMode: UINT;
    dwTickCountDeadline: UINT;
    dwTrackFlags: UINT;
    dwClassContext: UINT;
    locale: UINT;
    pServerInfo: ^_COSERVERINFO;
  end;

  _COAUTHIDENTITY = record
    User: ^Word;
    UserLength: UINT;
    Domain: ^Word;
    DomainLength: UINT;
    Password: ^Word;
    PasswordLength: UINT;
    Flags: UINT;
  end;

  _COAUTHINFO = record
    dwAuthnSvc: UINT;
    dwAuthzSvc: UINT;
    pwszServerPrincName: PWideChar;
    dwAuthnLevel: UINT;
    dwImpersonationLevel: UINT;
    pAuthIdentityData: ^_COAUTHIDENTITY;
    dwCapabilities: UINT;
  end;

  _COSERVERINFO = record
    dwReserved1: UINT;
    pwszName: PWideChar;
    pAuthInfo: ^_COAUTHINFO;
    dwReserved2: UINT;
  end;

  tagSTATDATA = record
    formatetc: tagFORMATETC;
    advf: UINT;
    pAdvSink: IAdviseSink;
    dwConnection: UINT;
  end;


  __MIDL_IWinTypes_0009 = record
    case Integer of
      0: (hInproc: Integer);
      1: (hRemote: Integer);
  end;

  _RemotableHandle = record
    fContext: Integer;
    u: __MIDL_IWinTypes_0009;
  end;

  _RESULTDATAITEM = record
    mask: UINT;
    bScopeItem: Integer;
    itemID: Integer;
    nIndex: SYSINT;
    nCol: SYSINT;
    str: PWideChar;
    nImage: SYSINT;
    nState: SYSUINT;
    lParam: Integer;
    iIndent: SYSINT;
  end;

  _SCOPEDATAITEM = record
    mask: UINT;
    displayname: PWideChar;
    nImage: SYSINT;
    nOpenImage: SYSINT;
    nState: SYSUINT;
    cChildren: SYSINT;
    lParam: Integer;
    relativeID: Integer;
    ID: Integer;
  end;

  _PSP = record
    dummy: SYSINT;
  end;

  _CONTEXTMENUITEM = record
    strName: PWideChar;
    strStatusBarText: PWideChar;
    lCommandID: Integer;
    lInsertionPointID: Integer;
    fFlags: Integer;
    fSpecialFlags: Integer;
  end;

  _MMC_FILTERDATA = record
    pszText: PWideChar;
    cchTextMax: SYSINT;
    lValue: Integer;
  end;

  _SNodeID2 = record
    dwFlags: UINT;
    cBytes: UINT;
    ID: array[0..0] of Shortint;
  end;

  _MMC_COLUMN_DATA = record
    nColIndex: SYSINT;
    dwFlags: UINT;
    nWidth: SYSINT;
    ulReserved: UINT;
  end;

  _MMC_COLUMN_SET_DATA = record
    cbSize: SYSINT;
    nNumCols: SYSINT;
    pColData: ^_MMC_COLUMN_DATA;
  end;
  PMMC_COLUMN_SET_DATA = ^_MMC_COLUMN_SET_DATA;

  _MMC_SORT_DATA = record
    nColIndex: SYSINT;
    dwSortOptions: UINT;
    ulReserved: UINT;
  end;

  _MMC_SORT_SET_DATA = record
    cbSize: SYSINT;
    nNumItems: SYSINT;
    pSortData: ^_MMC_SORT_DATA;
  end;
  PMMC_SORT_SET_DATA = ^_MMC_SORT_SET_DATA;


  _RESULTFINDINFO = record
    psz: PWideChar;
    nStart: SYSINT;
    dwOptions: UINT;
  end;

  _MMC_TASK_DISPLAY_BITMAP = record
    szMouseOverBitmap: PWideChar;
    szMouseOffBitmap: PWideChar;
  end;

  _MMC_TASK_DISPLAY_SYMBOL = record
    szFontFamilyName: PWideChar;
    szURLtoEOT: PWideChar;
    szSymbolString: PWideChar;
  end;

  __MIDL___MIDL_itf_mmc_0137_0001 = record
    case Integer of
      0: (uBitmap: _MMC_TASK_DISPLAY_BITMAP);
      1: (uSymbol: _MMC_TASK_DISPLAY_SYMBOL);
  end;

  _MMC_TASK_DISPLAY_OBJECT = record
    eDisplayType: _MMC_TASK_DISPLAY_TYPE;
    __MIDL_0011: __MIDL___MIDL_itf_mmc_0137_0001;
  end;

  __MIDL___MIDL_itf_mmc_0137_0002 = record
    case Integer of
      0: (nCommandID: Integer);
      1: (szActionURL: PWideChar);
      2: (szScript: PWideChar);
  end;

  _MMC_TASK = record
    sDisplayObject: _MMC_TASK_DISPLAY_OBJECT;
    szText: PWideChar;
    szHelpString: PWideChar;
    eActionType: _MMC_ACTION_TYPE;
    __MIDL_0012: __MIDL___MIDL_itf_mmc_0137_0002;
  end;


  _MMC_LISTPAD_INFO = record
    szTitle: PWideChar;
    szButtonText: PWideChar;
    nCommandID: Integer;
  end;

  _MENUBUTTONDATA = record
    idCommand: SYSINT;
    x: SYSINT;
    y: SYSINT;
  end;

  _MMC_RESTORE_VIEW = record
    dwSize: UINT;
    cookie: Integer;
    pViewType: PWideChar;
    lViewOptions: Integer;
  end;

  _MMC_EXPANDSYNC_STRUCT = record
    bHandled: Integer;
    bExpanding: Integer;
    hItem: Integer;
  end;

  _MMC_VISIBLE_COLUMNS = record
    nVisibleColumns: SYSINT;
    rgVisibleCols: array[0..0] of SYSINT;
  end;

  _SMMCDataObjects = record
    count: UINT;
    lpDataObject: array[0..0] of IDataObject;
  end;

  _SMMCObjectTypes = record
    count: UINT;
    guid: array[0..0] of TGUID;
  end;

  _SNodeID = record
    cBytes: UINT;
    ID: array[0..0] of Shortint;
  end;

  PSP = _PSP;
  MMC_TASK_DISPLAY_TYPE = _MMC_TASK_DISPLAY_TYPE;

// *********************************************************************//
// Interface: IConsole
// Flags:     (0)
// GUID:      {43136EB1-D36C-11CF-ADBC-00AA00A80033}
// *********************************************************************//
  IConsole = interface(IUnknown)
    ['{43136EB1-D36C-11CF-ADBC-00AA00A80033}']
    function SetHeader(const pHeader: IHeaderCtrl): HResult; stdcall;
    function SetToolbar(const pToolbar: IToolbar): HResult; stdcall;
    function QueryResultView(out pUnknown: IUnknown): HResult; stdcall;
    function QueryScopeImageList(out ppImageList: IImageList): HResult; stdcall;
    function QueryResultImageList(out ppImageList: IImageList): HResult; stdcall;
    function UpdateAllViews(const lpDataObject: IDataObject; data: Integer; hint: Integer): HResult; stdcall;
    function MessageBox(lpszText: PWideChar; lpszTitle: PWideChar; fuStyle: SYSUINT; 
                        out piRetval: SYSINT): HResult; stdcall;
    function QueryConsoleVerb(out ppConsoleVerb: IConsoleVerb): HResult; stdcall;
    function SelectScopeItem(hScopeItem: Integer): HResult; stdcall;
    function GetMainWindow(out phwnd: wireHWND): HResult; stdcall;
    function NewWindow(hScopeItem: Integer; lOptions: UINT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IHeaderCtrl
// Flags:     (0)
// GUID:      {43136EB3-D36C-11CF-ADBC-00AA00A80033}
// *********************************************************************//
  IHeaderCtrl = interface(IUnknown)
    ['{43136EB3-D36C-11CF-ADBC-00AA00A80033}']
    function InsertColumn(nCol: SYSINT; title: PWideChar; nFormat: SYSINT; nWidth: SYSINT): HResult; stdcall;
    function DeleteColumn(nCol: SYSINT): HResult; stdcall;
    function SetColumnText(nCol: SYSINT; title: PWideChar): HResult; stdcall;
    function GetColumnText(nCol: SYSINT; out pText: PWideChar): HResult; stdcall;
    function SetColumnWidth(nCol: SYSINT; nWidth: SYSINT): HResult; stdcall;
    function GetColumnWidth(nCol: SYSINT; out pWidth: SYSINT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IToolbar
// Flags:     (0)
// GUID:      {43136EB9-D36C-11CF-ADBC-00AA00A80033}
// *********************************************************************//
  IToolbar = interface(IUnknown)
    ['{43136EB9-D36C-11CF-ADBC-00AA00A80033}']
    function AddBitmap(nImages: SYSINT; var hbmp: _userHBITMAP; cxSize: SYSINT; cySize: SYSINT; 
                       crMask: UINT): HResult; stdcall;
    function AddButtons(nButtons: SYSINT; var lpButtons: _MMCButton): HResult; stdcall;
    function InsertButton(nIndex: SYSINT; var lpButton: _MMCButton): HResult; stdcall;
    function DeleteButton(nIndex: SYSINT): HResult; stdcall;
    function GetButtonState(idCommand: SYSINT; nState: _MMC_BUTTON_STATE; out pState: Integer): HResult; stdcall;
    function SetButtonState(idCommand: SYSINT; nState: _MMC_BUTTON_STATE; bState: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IImageList
// Flags:     (0)
// GUID:      {43136EB8-D36C-11CF-ADBC-00AA00A80033}
// *********************************************************************//
  IImageList = interface(IUnknown)
    ['{43136EB8-D36C-11CF-ADBC-00AA00A80033}']

// *** Fix bug in TLB
//    function ImageListSetIcon(var pIcon: Integer; nLoc: Integer): HResult; stdcall;
    function ImageListSetIcon(pIcon: Integer; nLoc: Integer): HResult; stdcall;
//    function ImageListSetStrip(var pBMapSm: Integer; var pBMapLg: Integer; nStartLoc: Integer;
//                               cMask: UINT): HResult; stdcall;
    function ImageListSetStrip(pBMapSm: Integer; pBMapLg: Integer; nStartLoc: Integer;
                               cMask: UINT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IConsoleVerb
// Flags:     (0)
// GUID:      {E49F7A60-74AF-11D0-A286-00C04FD8FE93}
// *********************************************************************//
  IConsoleVerb = interface(IUnknown)
    ['{E49F7A60-74AF-11D0-A286-00C04FD8FE93}']
    function GetVerbState(eCmdID: _MMC_CONSOLE_VERB; nState: _MMC_BUTTON_STATE; out pState: Integer): HResult; stdcall;
    function SetVerbState(eCmdID: _MMC_CONSOLE_VERB; nState: _MMC_BUTTON_STATE; bState: Integer): HResult; stdcall;
    function SetDefaultVerb(eCmdID: _MMC_CONSOLE_VERB): HResult; stdcall;
    function GetDefaultVerb(out peCmdID: _MMC_CONSOLE_VERB): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IResultData
// Flags:     (0)
// GUID:      {31DA5FA0-E0EB-11CF-9F21-00AA003CA9F6}
// *********************************************************************//
  IResultData = interface(IUnknown)
    ['{31DA5FA0-E0EB-11CF-9F21-00AA003CA9F6}']
    function InsertItem(var item: _RESULTDATAITEM): HResult; stdcall;
    function DeleteItem(itemID: Integer; nCol: SYSINT): HResult; stdcall;
    function FindItemByLParam(lParam: Integer; out pItemID: Integer): HResult; stdcall;
    function DeleteAllRsltItems: HResult; stdcall;
    function SetItem(var item: _RESULTDATAITEM): HResult; stdcall;
    function GetItem(var item: _RESULTDATAITEM): HResult; stdcall;
    function GetNextItem(var item: _RESULTDATAITEM): HResult; stdcall;
    function ModifyItemState(nIndex: SYSINT; itemID: Integer; uAdd: SYSUINT; uRemove: SYSUINT): HResult; stdcall;
    function ModifyViewStyle(add: _MMC_RESULT_VIEW_STYLE; remove: _MMC_RESULT_VIEW_STYLE): HResult; stdcall;
    function SetViewMode(lViewMode: Integer): HResult; stdcall;
    function GetViewMode(out lViewMode: Integer): HResult; stdcall;
    function UpdateItem(itemID: Integer): HResult; stdcall;
    function Sort(nColumn: SYSINT; dwSortOptions: UINT; lUserParam: Integer): HResult; stdcall;
    function SetDescBarText(DescText: PWideChar): HResult; stdcall;
    function SetItemCount(nItemCount: SYSINT; dwOptions: UINT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IConsoleNameSpace
// Flags:     (0)
// GUID:      {BEDEB620-F24D-11CF-8AFC-00AA003CA9F6}
// *********************************************************************//
  IConsoleNameSpace = interface(IUnknown)
    ['{BEDEB620-F24D-11CF-8AFC-00AA003CA9F6}']
    function InsertItem(var item: _SCOPEDATAITEM): HResult; stdcall;
    function DeleteItem(hItem: Integer; fDeleteThis: Integer): HResult; stdcall;
    function SetItem(var item: _SCOPEDATAITEM): HResult; stdcall;
    function GetItem(var item: _SCOPEDATAITEM): HResult; stdcall;
    function GetChildItem(item: Integer; out pItemChild: Integer; out pCookie: Integer): HResult; stdcall;
    function GetNextItem(item: Integer; out pItemNext: Integer; out pCookie: Integer): HResult; stdcall;
    function GetParentItem(item: Integer; out pItemParent: Integer; out pCookie: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IPropertySheetProvider
// Flags:     (0)
// GUID:      {85DE64DE-EF21-11CF-A285-00C04FD8DBE6}
// *********************************************************************//
  IPropertySheetProvider = interface(IUnknown)
    ['{85DE64DE-EF21-11CF-A285-00C04FD8DBE6}']
    function CreatePropertySheet(title: PWideChar; _type: Shortint; cookie: Integer;
                                 const pIDataObjectm: IDataObject; dwOptions: UINT): HResult; stdcall;
    function FindPropertySheet(cookie: Integer; const lpComponent: IComponent; 
                               const lpDataObject: IDataObject): HResult; stdcall;
    function AddPrimaryPages(const lpUnknown: IUnknown; bCreateHandle: Integer; 
                             var hNotifyWindow: _RemotableHandle; bScopePane: Integer): HResult; stdcall;
    function AddExtensionPages: HResult; stdcall;
    function Show(window: Integer; page: SYSINT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComponent
// Flags:     (0)
// GUID:      {43136EB2-D36C-11CF-ADBC-00AA00A80033}
// *********************************************************************//
  IComponent = interface(IUnknown)
    ['{43136EB2-D36C-11CF-ADBC-00AA00A80033}']
    function Initialize(const lpConsole: IConsole): HResult; stdcall;
    function Notify(const lpDataObject: IDataObject; event: _MMC_NOTIFY_TYPE; arg: Integer; 
                    param: Integer): HResult; stdcall;
    function Destroy(cookie: Integer): HResult; stdcall;
    function QueryDataObject(cookie: Integer; _type: _DATA_OBJECT_TYPES;
                             out ppDataObject: IDataObject): HResult; stdcall;
    function GetResultViewType(cookie: Integer; out ppViewType: PWideChar; out pViewOptions: Integer): HResult; stdcall;
    function GetDisplayInfo(var pResultDataItem: _RESULTDATAITEM): HResult; stdcall;
    function CompareObjects(const lpDataObjectA: IDataObject; const lpDataObjectB: IDataObject): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IPropertySheetCallback
// Flags:     (0)
// GUID:      {85DE64DD-EF21-11CF-A285-00C04FD8DBE6}
// *********************************************************************//
  IPropertySheetCallback = interface(IUnknown)
    ['{85DE64DD-EF21-11CF-A285-00C04FD8DBE6}']
    function AddPage(var hPage: _PSP): HResult; stdcall;
    function RemovePage(var hPage: _PSP): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IContextMenuCallback
// Flags:     (0)
// GUID:      {43136EB7-D36C-11CF-ADBC-00AA00A80033}
// *********************************************************************//
  IContextMenuCallback = interface(IUnknown)
    ['{43136EB7-D36C-11CF-ADBC-00AA00A80033}']
    function AddItem(var pItem: _CONTEXTMENUITEM): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IContextMenuProvider
// Flags:     (0)
// GUID:      {43136EB6-D36C-11CF-ADBC-00AA00A80033}
// *********************************************************************//
  IContextMenuProvider = interface(IContextMenuCallback)
    ['{43136EB6-D36C-11CF-ADBC-00AA00A80033}']
    function EmptyMenuList: HResult; stdcall;
    function AddPrimaryExtensionItems(const piExtension: IUnknown; const piDataObject: IDataObject): HResult; stdcall;
    function AddThirdPartyExtensionItems(const piDataObject: IDataObject): HResult; stdcall;
    function ShowContextMenu(var hwndParent: _RemotableHandle; xPos: Integer; yPos: Integer; 
                             out plSelected: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IControlbar
// Flags:     (0)
// GUID:      {69FB811E-6C1C-11D0-A2CB-00C04FD909DD}
// *********************************************************************//
  IControlbar = interface(IUnknown)
    ['{69FB811E-6C1C-11D0-A2CB-00C04FD909DD}']
    function Create(nType: _MMC_CONTROL_TYPE; const pExtendControlbar: IExtendControlbar; 
                    out ppUnknown: IUnknown): HResult; stdcall;
    function Attach(nType: _MMC_CONTROL_TYPE; const lpUnknown: IUnknown): HResult; stdcall;
    function Detach(const lpUnknown: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IExtendControlbar
// Flags:     (0)
// GUID:      {49506520-6F40-11D0-A98B-00C04FD8D565}
// *********************************************************************//
  IExtendControlbar = interface(IUnknown)
    ['{49506520-6F40-11D0-A98B-00C04FD8D565}']
    function SetControlbar(const pControlbar: IControlbar): HResult; stdcall;
    function ControlbarNotify(event: _MMC_NOTIFY_TYPE; arg: Integer; param: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMenuButton
// Flags:     (0)
// GUID:      {951ED750-D080-11D0-B197-000000000000}
// *********************************************************************//
  IMenuButton = interface(IUnknown)
    ['{951ED750-D080-11D0-B197-000000000000}']
    function AddButton(idCommand: SYSINT; lpButtonText: PWideChar; lpTooltipText: PWideChar): HResult; stdcall;
    function SetButton(idCommand: SYSINT; lpButtonText: PWideChar; lpTooltipText: PWideChar): HResult; stdcall;
    function SetButtonState(idCommand: SYSINT; nState: _MMC_BUTTON_STATE; bState: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IConsole2
// Flags:     (0)
// GUID:      {103D842A-AA63-11D1-A7E1-00C04FD8D565}
// *********************************************************************//
  IConsole2 = interface(IConsole)
    ['{103D842A-AA63-11D1-A7E1-00C04FD8D565}']
    function Expand(hItem: Integer; bExpand: Integer): HResult; stdcall;
    function IsTaskpadViewPreferred: HResult; stdcall;
    function SetStatusText(pszStatusText: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IHeaderCtrl2
// Flags:     (0)
// GUID:      {9757ABB8-1B32-11D1-A7CE-00C04FD8D565}
// *********************************************************************//
  IHeaderCtrl2 = interface(IHeaderCtrl)
    ['{9757ABB8-1B32-11D1-A7CE-00C04FD8D565}']
    function SetChangeTimeOut(uTimeout: UINT): HResult; stdcall;
    function SetColumnFilter(nColumn: SYSUINT; dwType: UINT; var pFilterData: _MMC_FILTERDATA): HResult; stdcall;
    function GetColumnFilter(nColumn: SYSUINT; var pdwType: UINT; var pFilterData: _MMC_FILTERDATA): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IConsoleNameSpace2
// Flags:     (0)
// GUID:      {255F18CC-65DB-11D1-A7DC-00C04FD8D565}
// *********************************************************************//
  IConsoleNameSpace2 = interface(IConsoleNameSpace)
    ['{255F18CC-65DB-11D1-A7DC-00C04FD8D565}']
    function Expand(hItem: Integer): HResult; stdcall;
    function AddExtension(hItem: Integer; var lpClsid: TGUID): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDisplayHelp
// Flags:     (0)
// GUID:      {CC593830-B926-11D1-8063-0000F875A9CE}
// *********************************************************************//
  IDisplayHelp = interface(IUnknown)
    ['{CC593830-B926-11D1-8063-0000F875A9CE}']
    function ShowTopic(pszHelpTopic: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IStringTable
// Flags:     (0)
// GUID:      {DE40B7A4-0F65-11D2-8E25-00C04F8ECD78}
// *********************************************************************//
  IStringTable = interface(IUnknown)
    ['{DE40B7A4-0F65-11D2-8E25-00C04F8ECD78}']
    function AddString(pszAdd: PWideChar; out pStringID: UINT): HResult; stdcall;
    function GetString(StringID: UINT; cchBuffer: UINT; out lpBuffer: PWideChar; out pcchOut: UINT): HResult; stdcall;
    function GetStringLength(StringID: UINT; out pcchString: UINT): HResult; stdcall;
    function DeleteString(StringID: UINT): HResult; stdcall;
    function DeleteAllStrings: HResult; stdcall;
    function FindString(pszFind: PWideChar; out pStringID: UINT): HResult; stdcall;
    function Enumerate(out ppenum: IEnumString): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IViewData
// Flags:     (0)
// GUID:      {CA656020-B47D-11D2-A692-00C04F8EF4CB}
// *********************************************************************//
  IViewData = interface(IUnknown)
    ['{CA656020-B47D-11D2-A692-00C04F8EF4CB}']
    function SetColumnConfigData(var pColID: _SNodeID2; var pColSetData: _MMC_COLUMN_SET_DATA): HResult; stdcall;
    function GetColumnConfigData(var pColID: _SNodeID2; out ppColSetData: PMMC_COLUMN_SET_DATA): HResult; stdcall;
    function SetColumnSortData(var pColID: _SNodeID2; var pColSortData: _MMC_SORT_SET_DATA): HResult; stdcall;
    function GetColumnSortData(var pColID: _SNodeID2; out ppColSortData: PMMC_SORT_SET_DATA): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IComponentData
// Flags:     (0)
// GUID:      {955AB28A-5218-11D0-A985-00C04FD8D565}
// *********************************************************************//
  IComponentData = interface(IUnknown)
    ['{955AB28A-5218-11D0-A985-00C04FD8D565}']
    function Initialize(const pUnknown: IUnknown): HResult; stdcall;
    function CreateComponent(out ppComponent: IComponent): HResult; stdcall;
    function Notify(const lpDataObject: IDataObject; event: _MMC_NOTIFY_TYPE; arg: Integer; 
                    param: Integer): HResult; stdcall;
    function Destroy: HResult; stdcall;
    function QueryDataObject(cookie: Integer; _type: _DATA_OBJECT_TYPES;
                             out ppDataObject: IDataObject): HResult; stdcall;
    function GetDisplayInfo(var pScopeDataItem: _SCOPEDATAITEM): HResult; stdcall;
    function CompareObjects(const lpDataObjectA: IDataObject; const lpDataObjectB: IDataObject): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IExtendPropertySheet
// Flags:     (0)
// GUID:      {85DE64DC-EF21-11CF-A285-00C04FD8DBE6}
// *********************************************************************//
  IExtendPropertySheet = interface(IUnknown)
    ['{85DE64DC-EF21-11CF-A285-00C04FD8DBE6}']
    function CreatePropertyPages(const lpProvider: IPropertySheetCallback; handle: Integer; 
                                 const lpIDataObject: IDataObject): HResult; stdcall;
    function QueryPagesFor(const lpDataObject: IDataObject): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IExtendContextMenu
// Flags:     (0)
// GUID:      {4F3B7A4F-CFAC-11CF-B8E3-00C04FD8D5B0}
// *********************************************************************//
  IExtendContextMenu = interface(IUnknown)
    ['{4F3B7A4F-CFAC-11CF-B8E3-00C04FD8D5B0}']
    function AddMenuItems(const piDataObject: IDataObject; const piCallback: IContextMenuCallback; 
                          var pInsertionAllowed: Integer): HResult; stdcall;
    function Command(lCommandID: Integer; const piDataObject: IDataObject): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IResultDataCompare
// Flags:     (0)
// GUID:      {E8315A52-7A1A-11D0-A2D2-00C04FD909DD}
// *********************************************************************//
  IResultDataCompare = interface(IUnknown)
    ['{E8315A52-7A1A-11D0-A2D2-00C04FD909DD}']
    function Compare(lUserParam: Integer; cookieA: Integer; cookieB: Integer; var pnResult: SYSINT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISnapinAbout
// Flags:     (0)
// GUID:      {1245208C-A151-11D0-A7D7-00C04FD909DD}
// *********************************************************************//
  ISnapinAbout = interface(IUnknown)
    ['{1245208C-A151-11D0-A7D7-00C04FD909DD}']
    function GetSnapinDescription(out lpDescription: PWideChar): HResult; stdcall;
    function GetProvider(out lpName: PWideChar): HResult; stdcall;
    function GetSnapinVersion(out lpVersion: PWideChar): HResult; stdcall;
    function GetSnapinImage(out hAppIcon: wireHICON): HResult; stdcall;
    function GetStaticFolderImage(out hSmallImage: wireHBITMAP; out hSmallImageOpen: wireHBITMAP; 
                                  out hLargeImage: wireHBITMAP; out cMask: UINT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IResultOwnerData
// Flags:     (0)
// GUID:      {9CB396D8-EA83-11D0-AEF1-00C04FB6DD2C}
// *********************************************************************//
  IResultOwnerData = interface(IUnknown)
    ['{9CB396D8-EA83-11D0-AEF1-00C04FB6DD2C}']
    function FindItem(var pFindInfo: _RESULTFINDINFO; out pnFoundIndex: SYSINT): HResult; stdcall;
    function CacheHint(nStartIndex: SYSINT; nEndIndex: SYSINT): HResult; stdcall;
    function SortItems(nColumn: SYSINT; dwSortOptions: UINT; lUserParam: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISnapinHelp
// Flags:     (0)
// GUID:      {A6B15ACE-DF59-11D0-A7DD-00C04FD909DD}
// *********************************************************************//
  ISnapinHelp = interface(IUnknown)
    ['{A6B15ACE-DF59-11D0-A7DD-00C04FD909DD}']
    function GetHelpTopic(out lpCompiledHelpFile: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumTASK
// Flags:     (0)
// GUID:      {338698B1-5A02-11D1-9FEC-00600832DB4A}
// *********************************************************************//
  IEnumTASK = interface(IUnknown)
    ['{338698B1-5A02-11D1-9FEC-00600832DB4A}']
    function Next(celt: UINT; out rgelt: _MMC_TASK; out pceltFetched: UINT): HResult; stdcall;
    function Skip(celt: UINT): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumTASK): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IExtendPropertySheet2
// Flags:     (0)
// GUID:      {B7A87232-4A51-11D1-A7EA-00C04FD909DD}
// *********************************************************************//
  IExtendPropertySheet2 = interface(IExtendPropertySheet)
    ['{B7A87232-4A51-11D1-A7EA-00C04FD909DD}']
    function GetWatermarks(const lpIDataObject: IDataObject; out lphWatermark: wireHBITMAP; 
                           out lphHeader: wireHBITMAP; out lphPalette: wireHPALETTE; 
                           out bStretch: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISnapinHelp2
// Flags:     (0)
// GUID:      {4861A010-20F9-11D2-A510-00C04FB6DD2C}
// *********************************************************************//
  ISnapinHelp2 = interface(ISnapinHelp)
    ['{4861A010-20F9-11D2-A510-00C04FB6DD2C}']
    function GetLinkedTopics(out lpCompiledHelpFiles: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IExtendTaskPad
// Flags:     (0)
// GUID:      {8DEE6511-554D-11D1-9FEA-00600832DB4A}
// *********************************************************************//
  IExtendTaskPad = interface(IUnknown)
    ['{8DEE6511-554D-11D1-9FEA-00600832DB4A}']
    function TaskNotify(const pdo: IDataObject; var arg: OleVariant; var param: OleVariant): HResult; stdcall;
    function EnumTasks(const pdo: IDataObject; szTaskGroup: PWideChar; out ppEnumTASK: IEnumTASK): HResult; stdcall;
    function GetTitle(pszGroup: PWideChar; out pszTitle: PWideChar): HResult; stdcall;
    function GetDescriptiveText(pszGroup: PWideChar; out pszDescriptiveText: PWideChar): HResult; stdcall;
    function GetBackground(pszGroup: PWideChar; out pTDO: _MMC_TASK_DISPLAY_OBJECT): HResult; stdcall;
    function GetListPadInfo(pszGroup: PWideChar; out lpListPadInfo: _MMC_LISTPAD_INFO): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRequiredExtensions
// Flags:     (0)
// GUID:      {72782D7A-A4A0-11D1-AF0F-00C04FB6DD2C}
// *********************************************************************//
  IRequiredExtensions = interface(IUnknown)
    ['{72782D7A-A4A0-11D1-AF0F-00C04FB6DD2C}']
    function EnableAllExtensions: HResult; stdcall;
    function GetFirstExtension(out pExtCLSID: TGUID): HResult; stdcall;
    function GetNextExtension(out pExtCLSID: TGUID): HResult; stdcall;
  end;

implementation

uses ComObj;

end.
