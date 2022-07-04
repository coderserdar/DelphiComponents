unit ShlExt;

interface

uses CommCtrl, ActiveX,Windows, ShlObj, ShlObj2, Messages;

const
  // New Interface IDs
  {$EXTERNALSYM IID_IBrowserFrameOptions}
  IID_IBrowserFrameOptions : TGUID = '{10DF43C8-1DBE-11D3-8B34-006097DF5BD4}';

  IID_IGetNameSpaceExtensionPointer: TGUID = '{287D4A71-439F-43A4-8D5B-0E0AE71E84A9}';

  IID_IPersistFreeThreadedObject: TGUID = '{C7264BF0-EDB6-11D1-8546-006008059368}';

  SID_IUIElement = '{EC6FE84F-DC14-4FBB-889F-EA50FE27FE0F}';
  SID_IUICommand = '{4026DFB9-7691-4142-B71C-DCF08EA4DD9C}';
  SID_IEnumUICommand = '{869447DA-9F84-4E2A-B92D-00642DC8A911}';


  // String constants for Interface IDs
  SID_IBrowserFrameOptions  = '{10DF43C8-1DBE-11D3-8B34-006097DF5BD4}';
  SID_IGetNameSpaceExtensionPointer = '{287D4A71-439F-43a4-8D5B-0E0AE71E84A9}';
  SID_IPersistFreeThreadedObject = '{C7264BF0-EDB6-11D1-8546-006008059368}';

type
  PPidlArray = ^TPidlArray;
  TPidlArray = array [0..255] of PItemIdList;
  PGuidArray = ^TGuidArray;
  TGuidArray = array [0..255] of TGUID;

{$IFDEF VER130}
type
  TIntegerArray = array [0..255] of integer;
  PIntegerArray = ^TIntegerArray;
{$ENDIF}

//==========================================================================
// Used by the IShellBrowser/IShellView objects.
//==========================================================================
const
  // The shell handles internally all the following ID's without calling
  // InvokeCommand()
  FCIDM_SHVIEWSHELL = $7000;

  // File menu options
  FCIDM_MENU_FILE_CREATESHORTCUT = FCIDM_SHVIEWSHELL + $0010;
  FCIDM_MENU_FILE_DELETE         = FCIDM_SHVIEWSHELL + $0011;
  FCIDM_MENU_FILE_RENAME         = FCIDM_SHVIEWSHELL + $0012;
  FCIDM_MENU_FILE_PROPERTIES     = FCIDM_SHVIEWSHELL + $0013;

  // Edit menu options
  FCIDM_MENU_EDIT_CUT            = FCIDM_SHVIEWSHELL + $0018;
  FCIDM_MENU_EDIT_COPY           = FCIDM_SHVIEWSHELL + $0019;
  FCIDM_MENU_EDIT_PASTE          = FCIDM_SHVIEWSHELL + $001A;
  FCIDM_MENU_EDIT_UNDO           = FCIDM_SHVIEWSHELL + $001B;
  FCIDM_MENU_EDIT_PASTESHORTCUT  = FCIDM_SHVIEWSHELL + $001C;

  // View Report styles
  FCIDM_MENU_VIEW_SELECTALL      = FCIDM_SHVIEWSHELL + $0021;
  FCIDM_MENU_VIEW_INVERTSELECTION = FCIDM_SHVIEWSHELL + $0022;

  FCIDM_MENU_VIEW_LARGEICONS     = FCIDM_SHVIEWSHELL + $0029;
  FCIDM_MENU_VIEW_SMALLICONS     = FCIDM_SHVIEWSHELL + $002A;
  FCIDM_MENU_VIEW_LIST           = FCIDM_SHVIEWSHELL + $002B;
  FCIDM_MENU_VIEW_DETAILS        = FCIDM_SHVIEWSHELL + $002C;

  FCIDM_MENU_VIEW_AUTOARRANGE    = FCIDM_SHVIEWSHELL + $0031;
  FCIDM_MENU_VIEW_LINEUPICONS    = FCIDM_SHVIEWSHELL + $0032;

  // Help menu
  FCIDM_MENU_HELP_HELPTOPICS     = FCIDM_SHVIEWSHELL + $0041;

  // The refresh command
  FCIDM_MENU_VIEW_REFRESH        = FCIDM_SHVIEWSHELL + $0103;

const
  SFVID_FIRST = FCIDM_SHVIEWFIRST + $2000;
  SFVID_LAST  = FCIDM_SHVIEWFIRST + $3000;
  SFVID_MENU_ARRANGE = SFVID_FIRST;

type
  TSFVCBColumnInfoStruct = record
    pidl : PITEMIDLIST;  // NULL if column header requested, else
                         // a simple pidl to the item whose details are needed.
    sci : TShellDetails; // return filled in with the details.
  end;
  PSFVCBColumnInfoStruct = ^TSFVCBColumnInfoStruct;
{$IFDEF VER130}
const
  SFVTI_ADDTOEND = 0;
  SFVTI_ADDTOFRONT = 1;
  SFVTI_OVERWRITE  = 2;
type
  SFVTIF = CARDINAL;
{$ELSE}
type
  // SFVTOOLBARINFO flags
  SFVTIF = (SFVTI_ADDTOEND = 0, SFVTI_ADDTOFRONT = 1, SFVTI_OVERWRITE  = 2);
{$ENDIF}
  TSFVCBToolBarInfo = record
    dwNumItems : DWord;
    dwFlags : DWord; // combination of the FCT_MERGE,... flags
  end;
  PSFVCBToolBarInfo = ^TSFVCBToolBarInfo;
  
  TSFVCBSelectInfo = record
    uOldState : DWORD; // 0
    uNewState :  DWORD; //LVIS_SELECTED, LVIS_FOCUSED,...
    pidl : PItemIdList;
  end;

  // Generic structure used by several messages
  TSFVCBInfo = record
    dwReserved : DWORD;
    dwReserved2 : DWORD;
    pidl : PItemIdList;
    dwUser : PDWORD;
  end;
  PSVCBInfo = ^TSFVCBInfo;

  // SFVCB_COPYHOOKCALLBACK structure
  TSFVCopyHookInfo = record
    wnd : HWND;
    wFunc : UINT;
    wFlags : UINT;
    szSrcFile : LPCSTR;
    dwSrcAttribs : DWORD;
    szDestFile : LPCSTR;
    dwDestAttribs : DWORD;
  end;
  PSFVCopyHookInfo = ^TSFVCopyHookInfo;

  TSFVM_WEBVIEW_LAYOUT_DATA = record
	  flags : cardinal;
	  pUnk : IUnknown; //IPreview3?
  end;
  PSFVM_WEBVIEW_LAYOUT_DATA = ^TSFVM_WEBVIEW_LAYOUT_DATA;

  TSFVM_WEBVIEW_CONTENT_DATA = packed record
  	l1 : integer;
	  l2 : integer;
	  pUnk : IUnknown; // IUIElement
	  pUnk2 : IUnknown; // IUIElement
	  pEnum : IEnumIdList;
  end;
  PSFVM_WEBVIEW_CONTENT_DATA = ^TSFVM_WEBVIEW_CONTENT_DATA;

  TSFVM_WEBVIEW_TASKSECTION_DATA = record
  	pEnum : IUnknown; // IEnumUICommand
	  pEnum2 : IUnknown; // IEnumUICommand
  end;
  PSFVM_WEBVIEW_TASKSECTION_DATA = ^TSFVM_WEBVIEW_TASKSECTION_DATA;

  TSFVM_WEBVIEW_THEME_DATA = record
  	pszTheme : PWideChar;
  end;
  PSFVM_WEBVIEW_THEME_DATA = ^TSFVM_WEBVIEW_THEME_DATA;

type
  IEnumShellItems = interface(IUnknown)
//    ['{4670AC35-34A6-4D2B-B7B6-CD665C6189A5}']
    function Next(celt: UINT; out rgelt: IShellItem; out pceltFetched: UINT): HResult; stdcall;
    function Skip(celt: UINT): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumShellItems): HResult; stdcall;
  end;

  IShellItemArray = interface(IUnknown)
//    ['{90CF20DE-73B4-4AA4-BA7A-82FF310AF24A}']
  	// IShellItemArray methods
	  function BindToHandler(pbc : IBindCtx; const rbhid : TGUID; const riid : TIID; out ppvOut): HResult; stdcall;
	  function GetAttrributes(nEnum : integer; dwRequested : dword; out pdwResult : dword): HResult; stdcall;
	  function GetCount(out pCount : UINT): HResult; stdcall;
	  function GetItemAt(nIndex : uint; out ppItem : IShellItem) : HResult; stdcall;
	  function EnumItems(out enumShellItems : IEnumShellItems): HResult; stdcall;
  end;

  IUIElement = interface(IUnknown)
    [SID_IUIElement]
  	// IUIElement methods
  	function get_Name(pItemArray : IShellItemArray; var bstrName : TBStr) : HResult; stdcall;
	  function get_Icon(pItemArray : IShellItemArray; var bstrName : TBStr) : HResult; stdcall;
	  function get_Tooltip(pItemArray : IShellItemArray; var bstrName : TBStr) : HResult; stdcall;
  end;

  UISTATE = TOLEEnum;

  IUICommand = interface(IUIElement)
    [SID_IUICommand]
  	function get_CanonicalName(Guid : TGUID) : HResult; stdcall;
  	function get_State(pItemArray : IShellItemArray; nRequested : integer; var pState : UIState) : HResult; stdcall;
	  function Invoke(pItemArray : IShellItemArray; pCtx : IBindCtx) : HResult; stdcall;
  end;

  IEnumUICommand = interface(IUnknown)
    [SID_IEnumUICommand]
    // *** IEnumIDList methods ***
    function Next(celt: UINT; out rgelt: IUICommand; out pceltFetched: UINT): HResult; stdcall;
    function Skip(celt: UINT): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumUICommand): HResult; stdcall;
  end;

  // The interface IShellFiolderView is implemented by the IUnknown that
  // is passed to IObjectWithSite::SetSite.
  TItemSpacing = record
    cxSmall : integer;
    cySmall : integer;
    cxLarge : integer;
    cyLarge : integer;
  end;

  IShellFolderView = interface(IUnknown)
    ['{37A378C0-F82D-11CE-AE65-08002B2E1262}']
    // *** IShellFolderView methods ***
    function Rearrange(lParamSort : LPARAM) : HResult; stdcall;
    function GetArrangeParam(var plParamSort : LPARAM) : HResult; stdcall;
    function ArrangeGrid : HResult; stdcall;
    function AutoArrange : HResult; stdcall;
    function GetAutoArrange : HResult; stdcall;
    function AddObject(pidl : PItemIdList; var puItem : cardinal) : HResult; stdcall;
    function GetObject(var ppidl : PItemIdList; uItem : cardinal) : HResult; stdcall;
    function RemoveObject(pidl : PItemIdList; var puItem : cardinal) : HResult; stdcall;
    function GetObjectCount(var puCount : cardinal) : HResult; stdcall;
    function SetObjectCount(uCount : cardinal; dwFlags : cardinal) : HResult; stdcall;
    function UpdateObject(pidlOls,pidlNew : PItemIdList; var puItem : cardinal) : HResult; stdcall;
    function RefreshObject(pidl : PItemIdList; var puItem : cardinal) : HResult; stdcall;
    function SetRedraw(bRedraw : bool) : HResult; stdcall;
    function GetSelectedCount(var puSelected : cardinal) : HResult; stdcall;
    function GetSelectedObjects(var pidl : PPidlArray; var puItems : cardinal) : HResult; stdcall;
    function IsDropOnSource(DropTarget : IDropTarget) : HResult; stdcall;
    function GetDragPoint(var ppt : TPOINT) : HResult; stdcall;
    function GetDropPoint(var ppt : TPOINT) : HResult; stdcall;
    function MoveIcons(DataObject : IDataObject) : HResult; stdcall;
    function SetItemPos(pidl : PItemIdList; var ppt : TPOINT) : HResult; stdcall;
    function IsBkDropTarget(DropTarget : IDropTarget) : HResult; stdcall;
    function SetClipboard (bMove : BOOL) : HResult; stdcall;
    function SetPoints(DataObject : IDataObject) : HResult; stdcall;
    function GetItemSpacing(var spacing : TITEMSPACING) : HResult; stdcall;
    function SetCallback(pNewCB : IShellFolderViewCB; var ppOldCB : IShellFolderViewCB) : HResult; stdcall;
    function Select(dwFlags : cardinal) : HResult; stdcall;
    function QuerySupport(var pdwSupport : cardinal) : HResult; stdcall;
    function SetAutomationObject(disp : IDispatch) : HResult; stdcall;
  end;

const
  //                        uMsg       wParam             lParam
  // When the selection state of an item is changed (i.e. an item is selected or deselected)
  SFVM_SELECTIONCHANGED   = 8; //      idCmdFirst,nItem   TSFVCBSelectInfo struct
  SFVM_DRAWMENUITEM       = 9; //      idCmdFirst         pdis
  SFVM_MEASUREMENUITEM    = 10;//      idCmdFist          pmis
  // called when context menu exits, not main menu
  SFVM_EXITMENULOOP       = 11;//        0                 0
  // indicates that the IShellView object is being released.
  SFVM_VIEWRELEASE        = 12;//        -                lSelChangeInfo
  // Sent when beginning label edit.
  SFVM_GETNAMELENGTH      = 13;//        pidlItem         length
  // Called to indicate that the view window is being destroyed.
  SFVM_WINDOWCLOSING      = 16;//        hwnd             PDVSELCHANGEINFO
  SFVM_LISTREFRESHED      = 17;//         0               lSelChangeInfo
  // Sent to inform us that the list view has received the focus.
  SFVM_WINDOWFOCUSED      = 18;//         0               0
  SFVM_KILLFOCUS          = 19; //         0               0
  SFVM_REGISTERCOPYHOOK   = 20;//         0               0
  SFVM_COPYHOOKCALLBACK   = 21;//         -               LPCOPYHOOKINFO
  SFVM_NOTIFY	           	= 22;//         idFrom		      LPNOTIFY
  SFVM_ADDINGOBJECT       = 29;//         pidl            PDVSELCHANGEINFO
  SFVM_REMOVINGOBJECT     = 30;//         pidl            PDVSELCHANGEINFO
  SFVM_UPDATESTATUSBAR    = 31;//         -               lSelChangeInfo
  SFVM_GETCOMMANDDIR      = 33;
  // Get an IStream interface
  SFVM_GETCOLUMNSTREAM    = 34;// READ/WRITE/READWRITE    IStream
  SFVM_CANSELECTALL       = 35;//                         lSelChangeInfo
  SFVM_SUPPORTSIDENTITY   = 37;//         0               0
  SFVM_ISCHILDOBJECT      = 38;
  SFVM_GETEXTVIEWS        = 40;
  SFVM_GETITEM            = 42;//       iItem             LPITMIDLIST*
  SFVM_SETITEM            = 43;//       iItem             LPITEMIDLIST
  SFVM_INDEXOFITEM        = 44;//       *iItem            LPITEMIDLIST
  SFVM_FINDITEM           = 45;//       *iItem            NM_FINDITEM*
  SFVM_WNDMAIN            = 46;//                         hwndMain
  SFVM_COLUMNCLICK2       = 50;//       nil               column index
  SFVM_STANDARDVIEWS      = 51;//                         BOOL *
  SFVM_REUSEEXTVIEW       = 52;//                         BOOL *
  SFVM_GETEMPTYTEXT       = 54;//      cchMax             pszText
  SFVM_GETITEMICONINDEX   = 55;//      iItem              int *piIcon
  SFVM_DONTCUSTOMIZE      = 56;//      -                  BOOL *pbDontCustomize
  SFVM_ISOWNERDATA        = 60;//      ISOWNERDATA        BOOL *
  SFVM_GETRANGEOBJECT     = 61;//      iWhich             ILVRange **
  SFVM_CACHEHINT          = 62;//      -                  NMLVCACHEHINT *
  SFVM_OVERRIDEITEMCOUNT  = 64;//      -                  UINT*
  SFVM_GETHELPTEXTW       = 65;//      idCmd,cchMax       pszText - unicode
  SFVM_GETTOOLTIPTEXTW    = 66;//      idCmd,cchMax       pszText - unicode
  SFVM_GETIPERSISTHISTORY = 67;//                         IPersistHistory **
  SFVM_GETHELPTEXTA       = 69;//      idCmd,cchMax       pszText - ansi
  SFVM_GETTOOLTIPTEXTA    = 70;//      idCmd,cchMax       pszText - ansi
  SFVM_GETICONOVERLAY     = 71;//      iItem              int iOverlayIndex
  SFVM_SETICONOVERLAY     = 72;//      iItem              int * piOverlayIndex
  SFVM_ALTERDROPEFFECT    = 73;//      DWORD*             IDataObject*

  // XP messages - not all id'ed yet
  SFVM_MESSAGE4A          = 74;
  SFVM_MESSAGE4B          = 75;
  SFVM_MESSAGE4C          = 76;
  SFVM_GET_CUSTOMVIEWINFO = 77;
  SFVM_MESSAGE4E          = 78;
  SFVM_ENUMERATEDITEMS    = 79;
  SFVM_GET_VIEW_DATA      = 80;
  SFVM_MESSAGE51          = 81;
  SFVM_GET_WEBVIEW_LAYOUT = 82;
  SFVM_GET_WEBVIEW_CONTENT= 83;
  SFVM_GET_WEBVIEW_TASKS  = 84;
  SFVM_MESSAGE55          = 85;
  SFVM_GET_WEBVIEW_THEME  = 86;
  SFVM_MESSAGE57          = 87;
  SFVM_MESSAGE58          = 88;
  SFVM_MESSAGE59          = 89;
  SFVM_MESSAGE5A          = 90;
  SFVM_MESSAGE5B          = 91;
  SFVM_GETDEFERREDVIEWSETTINGS = 92;

// The interface IDelegateFolder is used for items in Internet Explorer
// like FTP folders.
// The problem:
// The root of these items is not displayed.
// For example, you don't have Internet Explorer\FTP\aserver.com
// The FTP folder is not displayed.
// But of course there has to be a root for the
// FTP folders.
// Internet explorer can not know about ftp items and items of other
// protocols.
// So what they did is they embedded the pidls for items directly
// under the Internet Explorer root in an Internet Explorer pidl.
// The Internet Explorer pidls look like this:
// {total size}{sig}{child pidl}{rest of pidl}
// where {total size) is the size of the pidl, as always.
//       {sig} is a singature of 4 bytes
//       {child} is an ebmedded pidl
//       {rest of pidl} defines the protocol etc. and is used by IE.
// The child is a normal pidl like you are used to. As usual it
// starts with the size.
// Internet Explorer therefore calls SetItemMalloc and gives the child
// an IMalloc it can usxe to allocate pidls.
// These pidls are IE pidls of the above form, but the child pidl
// is not filled in.
// So after SetItemAlloc has been called, when ever you create a pidl
// you should call the IMalloc's Alloc, and put your pidl at offset 6
// of the returned buffer.
type
  IDelegateFolder = interface(IUnknown)
    [SID_IDelegateFolder]
    function SetItemAlloc(const Malloc : IMalloc) : HResult; stdcall;
  end;

/////////////////////////////////////////////////////////////////////////////
// Totally miscellaneous stuff

// ??? about IShellFolder ?
// This is the interface for a browser to "subclass" the main File Cabinet
// window.  Note that only the hwnd, message, wParam, and lParam fields of
// the msg structure are used.  The browser window will get a WM_NOTIFY
// message with NULL ID, FCN_MESSAGE as the code, and a far pointer to
// FCMSG_NOTIFY as the lParam.
type
  TFCMSGNotify = record
    hdr : TNMHDR;
    msg : TMSG;
    Result : LResult;
  end;

const
  FCN_MESSAGE = 100;

//---------------------------------------------------------------------------
// messages that can be send to the cabinet by other apps
//---------------------------------------------------------------------------
// Change the path of an existing folder.
// wParam:
//	0:		LPARAM is a string, handle the message immediately.
//	CSP_HANDLE:	LPARAM is a handle. handle the message immediately
//			and then free the handle.
//	CSP_REPOST:	LPARAM is a string, copy the string and handle the
// 			message later.
// 	CSP_REPOST|CSP_HANDLE:
//			LPARAM is a handle, just handle the message later
//			and free the handle then.
// lParam: LPSTR or HANDLE of path.
//
const
  CSP_REPOST  = $0001;
  CSP_HANDLE  =	$0002;

type
  // lpsv points to the Shell View extension that requested idle processing
  // uID is an app define identifier for the processor
  // returns: TRUE if there is more idle processing necessary, FALSE if all done
  // Note that the idle processor should do one "atomic" operation and return
  // as soon as possible.
  FCIdleProc = function (lpsv : Pointer; uID : UINT) : BOOL; stdcall;


///////////////////////////////////////////////////////////////////////
// typeDefine the undocumented interfaces
//
/////////////////////////////////////////////////////////////////////////////
// IBrowserFrameOptions
type
  TBrowserFrameOption = (bfoBrowserPersistSettings,
                         bfoRenameFolderOptionsToInternet,
	                       bfoBothOptions,
                         bfoPreferInternetShortcut,
	                       bfoBrowseNoInNewProcess,
	                       bfoEnableHyperlinkTracking,
                         bfoUseIEOfflineSupport,
	                       bfoSubstituteInternetStartPage,
	                       bfoUseIELogoBanding,
	                       bfoAddIEToCaptionBar,
	                       bfoUseDialupRef,
	                       bfoUseIEToolbar,
	                       bfoNoParentFolderSupport,
	                       bfoNoReopenNextRestart,
	                       bfoGoHomePage,
	                       bfoPreferIEProcess,
	                       bfoShowNavigationCancelled) ;

  TBrowserFrameOptions = set of TBrowserFrameOption;

const
  bfoNone = [];
	bfoQueryAll	= [bfoBrowserPersistSettings..bfoShowNavigationCancelled];

type
  IBrowserFrameOptions = interface(IUnknown)
    [SID_IBrowserFrameOptions]
    function GetFrameOptions(dwRequested : DWORD; var pdwResult : DWORD) : HResult; stdcall;
  end;

const
  PID_STG_STORAGETYPE = 4; // The object's type VT_BSTR
  PID_STG_NAME = 10;       // The object's display name VT_BSTR
  PID_STG_SIZE = 12;       // The object's size VT_BSTR
  PID_STG_ATTRIBUTES = 13;  // The object's attributes VT_BSTR
  PID_STG_WRITETIME = 14;  // The object's modified attribute VT_BSTR

  FMTID_FileAttributes : TGUID = (
    D1:$8D72ACA1; D2:$0716; D3:$419A; D4:($9a,$c1,$ac,$b0,$7b,$18,$dc,$32));

  // Other PIDs
  PID_STG_DIRECTORY = 2;
  PID_STG_CLASSID = 3;
  PID_STG_VOLUME_ID = 5;
  PID_STG_PARENT_WORKID = 6;
  PID_STG_SECONDARYSTORE = 7;
  PID_STG_FILEINDEX = 8;
  PID_STG_LASTCHANGEUSN = 9;
  PID_STG_PATH = $0b;
  PID_STG_CREATETIME = $0f;
  PID_STG_ACCESSTIME = $10;
  PID_STG_CHANGETIME = $11;
  PID_STG_CONTENTS = $13;
  PID_STG_SHORTNAME = $14;
  PID_STG_MAX = PID_STG_SHORTNAME;
  CSTORAGEPROPERTY = $15;


//===========================================================================
//
// Shell Common Dialogs
//
//===========================================================================
// RunFileDlg flags
const
  RFF_NOBROWSE = $01;
  RFF_NODEFAULT = $02;
  RFF_CALCDIRECTORY = $04;
  RFF_NOLABEL = $08;
  RFF_NOSEPARATEMEM = $20;  // NT only

type
  // RunFileFlg notification structure
  NM_RUNFILEDLG = record
    hdr : NMHDR;
    lpFile : LPCSTR;
    lpDirectory : LPCSTR;
    nShow : integer;
  end;
  PNM_RUNFILEDLG = ^NM_RUNFILEDLG;

const
  // RunFileDlg notification return values
  RF_OK = $00;
  RF_CANCEL = $01;
  RF_RETRY  = $02;

type
  TRunFileDlg = procedure(hWndOwner : HWND; Icon : HICON; Directory,Title,Description : PChar; Flags : UInt); stdcall;
  TExitWindowsDialog = procedure(hWndOwner : HWND); stdcall;
  TSHFindComputer = function(pidlRoot,pidlSavedSearch : PItemIdList) : bool; stdcall;

var
  RunFileDlg : TRunFileDlg;
  ExitWindowsDialog : TExitWindowsDialog;
  SHFindComputer : TSHFindComputer;


type
  TSHHandleDiskFull = procedure(hWndOwner : HWND; uDrive : UINT); stdcall;
  TSHOutOfMemoryMessageBox = function(hWndOwner : HWND; Caption : PChar; uType : UINT) : integer; stdcall;
  TSHNetConnectionDialog = function(hWndOwner : HWND; RemoteName : PChar; dwType : cardinal) : cardinal; stdcall;
  TShellMessageBoxA = function(Instance : THandle; wnd : HWnd; Msg : PChar;
                               Title : PChar; uStyle : cardinal;
                               Format : PChar) : integer; stdcall;
var
  ShHandleDiskFull : TShHandleDiskFull;
  SHOutOfMemoryMessageBox : TSHOutOfMemoryMessageBox;
  SHNetConnectionDialog : TSHNetConnectionDialog;
  ShellMessageBoxA : TShellMessageBoxA;

//===========================================================================
//
// PIDL Manipulation Routines
//
//===========================================================================
type


  // returns the next pidl in a complex idlist
  TILGetNext = function (pidl : PItemIdList) : PItemIdList; stdcall;

  // Used to implement explorer parameters?
  TILGlobalClone = function(pidl : PItemIdList) : PItemIdList; stdcall;
  TILGlobalFree = procedure(pidl : PItemIdList); stdcall;


var
  ILGetNext : TILGetNext;
  ILGlobalClone : TILGlobalClone;
  ILGlobalFree : TILGlobalFree;

//===========================================================================
//
// Shell Notifications
//
//===========================================================================
// SHChangeNotifyRegister flags
const
  SHCNF_ACCEPT_INTERRUPTS     = $0001;
  SHCNF_ACCEPT_NON_INTERRUPTS = $0002;
  SHCNF_NO_PROXY              = $8000;  // NT only

type
  // SHChangeNotifyRegister structure
  TShChangeNotifyEntry = record
    pidlPath : PItemIdList;
    bWatchSubtree : bool;
  end;
  PShChangeNotifyEntry = ^TShChangeNotifyEntry;

type
  // DWORD item id structure
  DWordItemId = record
    cb : WORD;
    dwItem1 : dword;
    dwItem2 : dword;
  end;

//===========================================================================
//
// Cabinet Window Messages
//
//===========================================================================
const
  CWM_SETPATH           = WM_USER + 2;
  // Inform the File Cabinet that you want idle messages.
  // This should ONLY be used by File Cabinet extensions.
  // wParam: app define UINT (passed to FCIDLEPROC).
  // lParam: pointer to an FCIDLEPROC.
  // return: TRUE if successful; FALSE otherwise
  CWM_WANTIDLE          = WM_USER + 3;
  // get or set the FOLDERSETTINGS for a view
  // wParam: BOOL TRUE -> set to view info buffer, FALSE -> get view info buffer
  // lParam: LPFOLDERSETTINGS buffer to get or set view info
  CWM_GETSETCURRENTINFO	= WM_USER + 4;
  // selects the specified item in the current view
  // wParam: BOOL TRUE -> select, FALSE -> deselect
  // lParam: LPCSTR of the item ID (not display name), NULL -> all items
  CWM_SELECTITEM        = WM_USER + 5;
  CWM_SELECTITEMSTR     = WM_USER + 6;
  // tells the window to punt its wait cursor.  used for handoff
  // while thread inits
  CWM_STOPWAITING       = WM_USER + 6;
  // Get the IShellBrowser object associated with an hwndMain
  CWM_GETISHELLBROWSER  = WM_USER + 7;
  CWM_TESTPATH          = WM_USER + 9;
  CWM_STATECHANGE       = WM_USER + 10;
  CWM_GETPATH           = WM_USER + 12;

  // CWM_TESTPATH types
  CWTP_ISEQUAL  = 0;
  CWTP_ISCHILD  = 1;

type
  // CWM_TESTPATH structure
  CWTESTPATHSTRUCT = record
    dwType : dword;
    idl : ITEMIDLIST;
  end;
  LPCWTESTPATHSTRUCT = ^CWTESTPATHSTRUCT;

  procedure FileCabinet_SelectItem(wnd : HWND; bSel : boolean; lpidl : PItemIdList);
  // New way to get IShellBrowser etc interface
  function IUnknown_QueryService(punkSite : IUnknown; sid : TGUID; riid : TGUID; var ppv) : HResult;
  // Old way to get IShellBrowser etc interface
  function FileCabinet_GetIShellBrowser(wnd : HWND) : IShellBrowser;

//===========================================================================
//
// System Imagelist Routines
//
//===========================================================================
type
  TSHMapPIDLToSystemImageListIndex = function(psf : IShellFolder; pidl : PItemIdList; var Index : integer) : integer; stdcall;
  TFileIconInit = function(bFullInit : bool) : bool; stdcall;

var
  FileIconInit : TFileIconInit;

//===========================================================================
//
// File Menu Routines
//
//===========================================================================
const
  // FileMenu_Create nSelHeight constants
  FM_DEFAULT_SELHEIGHT = -1;
  FM_FULL_SELHEIGHT = 0;

  // FileMenu_Create flags
  FMF_SMALL_ICONS     = $00;
  FMF_LARGE_ICONS     = $08;
  FMF_NO_COLUMN_BREAK = $10;

const
  // FileMenu_AppendItem constants
  FM_SEPARATOR  = PChar(1);
  FM_BLANK_ICON = -1;
  FM_DEFAULT_HEIGHT = 0;

const
  // FileMenu_InsertUsingPidl flags
  FMF_NO_EMPTY_ITEM = $01;
  FMF_NO_PROGRAM_GROUPS = $04;

type
  TFNFMCallback = procedure (pidlFolder : PItemIdList; pidlFile : PItemIdList);

//===========================================================================
//
// Drag And Drop Routines
//
//===========================================================================

type
  TSHRegisterDragDrop = function(wnd : HWND; pDropTarget : IDropTarget) : HResult; stdcall;
  TSHRevokeDragDrop = function(wnd : HWND) : HResult; stdcall;
  TSHDoDragDrop = function(wnd : HWND; dtObj : IDataObject; dsrc : IDropSource;
                           OKEffect : DWORD; var Effect : dword) : HResult; stdcall;
  TDAD_DragEnter = function (WndTarget : HWND) : bool; stdcall;

var
  SHRegisterDragDrop : TSHRegisterDragDrop;
  SHRevokeDragDrop : TSHRevokeDragDrop;
  DAD_DragEnter : TDAD_DragEnter;

const
  // DAD_AutoScroll return values
  DAD_SCROLL_UP    = 1;
  DAD_SCROLL_DOWN  = 2;
  DAD_SCROLL_LEFT  = 4;
  DAD_SCROLL_RIGHT = 8;

type
  TDAD_SetDragImageFromListView = function(wnd : HWND; pt : TPoint) : bool; stdcall;
  TDAD_ShowDragImage = function(fShow : bool) : bool; stdcall;
  TCIDLData_CreateFromIDArray = function(pidlFolder : PItemIdList; cpidlFiles : DWORD; var pidlFiles : PItemIdList; DataObject : IDataObject) : HResult; stdcall;

var
  DAD_SetDragImageFromListView : TDAD_SetDragImageFromListView;
  CIDLData_CreateFromIDArray : TCIDLData_CreateFromIDArray;

//===========================================================================
//
// Path Manipulation Routines
//
//===========================================================================
type
  TPathAppend = function(szPath1,szPath2 : LPSTR) : PChar; stdcall;
  TPathCombine = function(destPath : LPSTR; Dir : LPCSTR; szfile : LPCSTR) : LPSTR; stdcall;
  TPathAddBackslash = function(szPath : LPSTR) : LPSTR; stdcall;
  TPathBuildRoot = function(path : LPSTR; Drive : integer) : LPSTR; stdcall;
  TPathFindFileName = function(szPath : LPCSTR) : LPSTR; stdcall;
  TPathFindExtension = function(szPath : LPCSTR) : LPSTR; stdcall;
  TPathGetExtension = function(szPath : LPCSTR) : LPSTR; stdcall;
  TPathGetArgs = function(szPath : LPCSTR) : LPSTR; stdcall;
  TPathGetDriveNumber = function(szPath : LPCSTR) : integer; stdcall;
  TPathRemoveFileSpec = function(szPath : LPSTR) : bool; stdcall;
  TPathRemoveBlanks = procedure(szPath : LPSTR); stdcall;
  TPathQuoteSpaces = procedure(szPath : LPSTR); stdcall;
  TPathUnquoteSpaces = procedure(szPath : LPSTR); stdcall;
  TPathIsUNC = function(szPath : LPCWSTR) : bool; stdcall;
  TPathIsRelative = function(szPath : LPCWSTR) : bool; stdcall;
  TPathIsRoot = function(szPath : LPCSTR) : bool; stdcall;

  TPathIsDirectory = function(szPath : LPCSTR) : bool; stdcall;
  TPathFileExists = function(szPath : LPCSTR) : bool; stdcall;
  TPathMatchSpec = function(szFile : LPCSTR; szSpec : LPCSTR) : bool; stdcall;
  TPathFindOnPath = procedure(szFile : LPSTR; var ppszPaths : LPCSTR); stdcall;
var
  PathAppend : TPathAppend;
  PathCombine : TPathCombine;
  PathAddBackslash : TPathAddBackslash;
  PathBuildRoot : TPathBuildRoot;
  PathFindFileName : TPathFindFileName;
  PathFindExtension : TPathFindExtension;
  PathGetExtension : TPathGetExtension;
  PathGetArgs : TPathGetArgs;
  PathGetDriveNumber : TPathGetDriveNumber;
  PathRemoveFileSpec : TPathRemoveFileSpec;
  PathRemoveBlanks : TPathRemoveBlanks;
  PathQuoteSpaces : TPathQuoteSpaces;
  PathUnquoteSpaces : TPathUnquoteSpaces;
  PathIsUNC : TPathIsUNC;
  PathIsRelative : TPathIsRelative;
  PathIsRoot : TPathIsRoot;
  PathIsDirectory : TPathIsDirectory;
  PathFileExists : TPathFileExists;
  PathMatchSpec : TPathMatchSpec;
  PathFindOnPath : TPathFindOnPath;

type
  TPathSetDlgItemPath = function(hDlg : HWND; nIDDlgItem : integer; szPath : LPCSTR) : bool; stdcall;

var
  PathSetDlgItemPath : TPathSetDlgItemPath;

type
  TPathStripPath = procedure(szPath : LPSTR); stdcall;
  TPathStripToRoot = function(szPath : LPSTR) : bool; stdcall;
  TPathRemoveArgs = procedure(szPath : LPWSTR); stdcall;
  TPathRemoveExtension = procedure(szPath : LPWSTR); stdcall;
  TPathParseIconLocation = function(szPath : LPWSTR) : integer; stdcall;
  TPathIsSameRoot = function(lpszPath1,lpszPath2 : LPCWSTR) : bool; stdcall;

var
  PathStripPath : TPathStripPath;
  PathStripToRoot : TPathStripToRoot;
  PathRemoveArgs : TPathRemoveArgs;
  PathRemoveExtension : TPathRemoveExtension;
  PathParseIconLocation : TPathParseIconLocation;
  PathIsSameRoot : TPathIsSameRoot;

//===========================================================================
//
// Shell Namespace Routines
//
//===========================================================================
// This function can be used to send some special messages to the cabinet window.
// These messages have mostly to do with gathering information about items and
// changing some attributes of some items.
// hwndCabinet: The window handle that was passed in IShellFolder::CreateViewObject
// uMsg: One of the SFVM_ constants that are described above
// lParam: a message-dependant value
// The return value depends on the message.
// You may need to call this function to enable sorting on a specified column.
// You use SFVM_REARRANGE for that.

const
//------------------------------------------------------------------------------
  // This message indicates that the user has clicked a column in the header of
  // the list control. The list needs to be rearranged.
  // lParam The index of the column on which to sort (starting at 0). This value
  // will be passed to IShellFolder::CompareIDs.
  SFVM_GETARRANGECOLUMN   = $0002;

  // Used to retrieve the number of items in the list.
  // Return value The number of items in the list
  SFVM_GETITEMCOUNT       = $0004;

  SFVM_GETITEMPIDL        = $0005;


  SFVM_SETREDRAW          = $0008;

  SFVM_ISDROPONSOURCE     = $000A;
  SFVM_MOVEICONS          = $000B;
  SFVM_GETDRAGPOINT       = $000C;
  SFVM_GETDROPPOINT       = $000D;
  SFVM_ISDROPONBACKGROUND = $000F;
  SFVM_TOGGLEAUTOARRANGE  = $0011;
  SFVM_LINEUPICONS        = $0012;
  SFVM_GETAUTOARRANGE     = $0013;

  // Used to learn how many items are selected in the list.
  // Return value The number of selected items
  SFVM_GETSELECTEDCOUNT   = $0014;
  SFVM_GETITEMSPACING     = $0015;
  SFVM_REFRESHOBJECT      = $0016;

function ShellFolderView_GetArrangeColumn(wnd : hwnd) : cardinal;
function ShellFolderView_GetItemCount(wnd : hwnd) : cardinal;
function ShellFolderView_GetItemPidl(wnd : HWND; index : cardinal) : PItemIdList;
function ShellFolderView_SetRedraw(wnd : HWND; mode : bool) : LPARAM;
function ShellFolderView_IsDropOnSource(wnd : HWND; target : IDropTarget) : bool;
procedure ShellFolderView_MoveIcons(wnd: HWND; target : IDropTarget);
function ShellFolderView_GetDragPoint(wnd : HWND; pt : PPoint) : bool;
function ShellFolderView_GetDropPoint(wnd : HWND; pt : PPoint) : bool;
function ShellFolderView_IsDropOnBackground(wnd : HWND; target : IDropTarget) : bool;
procedure ShellFolderView_ToggleAutoArrange(wnd : HWND);
procedure ShellFolderView_LineUpIcons(wnd : HWND);
function ShellFolderView_GetAutoArrange(wnd : HWND) : BOOL;
function ShellFolderView_GetSelectedCount(wnd : HWND) : cardinal;
function ShellFolderView_GetItemSpacing(wnd : HWND; var spacing : TItemSpacing) : bool;
function ShellFolderView_RefreshObject(wnd : HWND; pidl : PItemIdList): cardinal;
// Useful hack to refresh the shell folder window.
function ShellFolderView_RefreshAll(wnd : HWND) : bool;
//===========================================================================
//
// Misc Stuff
//
//===========================================================================
const
  // SHWaitForFileToOpen flags
  SHWFF_ADD = $01;
  SHWFF_REMOVE = $02;
  SHWFF_WAIT = $04;

const
  // RegisterShellHook types
  RSH_DEREGISTER       = 0;
  RSH_REGISTER         = 1;
  RSH_REGISTER_PROGMAN = 2;
  RSH_REGISTER_TASKMAN = 3;

const
  // SHCreateLinks flags
  SHCLF_PREFIXNAME      = 1;
  SHCLF_CREATEONDESKTOP = 2;

type
  TExtractAssociatedIconExA = function(hInst : THandle; IconPath : PChar;
                                       var IconIndex : word; var IconId : word) : HICON; stdcall;
var
  ExtractAssociatedIconExA : TExtractAssociatedIconExA;

type
  TSHGetNewLinkInfo = function(pszLinkTo : LPCTSTR;pszDir : LPCTSTR; szName : LPTSTR;
                               var pfMustCopy : bool; uFlags : cardinal) : bool; stdcall;

  TSHGetDiskFreeSpace = function(pszVolume : LPCTSTR; var pqwFreeCaller, pqwTot, pqwFree : int64) : bool; stdcall;
  TSHInvokePrinterCommand = function(wnd : HWnd;uAction : UINT; lpBuf1,lpBuf2 : LPCTSTR; fModal : bool) : bool; stdcall;
  TSHIsFileAvailableOffline = function(szPath : LPCWSTR;pdwStatus : PDWORD) : HResult; stdcall;

var
  SHGetNewLinkInfo : TSHGetNewLinkInfo;
  SHGetDiskFreeSpace : TSHGetDiskFreeSpace;
  SHInvokePrinterCommand : TSHInvokePrinterCommand;
  SHIsFileAvailableOffline : TSHIsFileAvailableOffline;

const
  CMF_FINDCMD = $80;
  CMF_BANDCMD = $20000;
  CMF_DVFILE  = $10000; // Indicates explorer is populating the file menu.

const
  // Definitions for CDefFolderMenu_Create2
  // Extra undocumented callback messages
  // (discovered by Maksym Schipka and Henk Devos)
//  DFM_MERGECONTEXTMENU = 1;      // uFlags       LPQCMINFO
//  DFM_INVOKECOMMAND = 2;         // idCmd        pszArgs
  DFM_CREATE = 3;                // AddRef?
  DFM_DESTROY = 4;               // Release
  DFM_GETHELPTEXTA = 5;          // idCmd,cchMax pszText
  DFM_MEASUREITEM =  6;          // same as WM_MEASUREITEM
  DFM_DRAWITEM = 7;              // same as WM_DRAWITEM
  DFM_INITMENUPOPUP = 8;         // same as WM_INITMENUPOPUP
  DFM_VALIDATECMD = 9;           // idCmd        0
  DFM_MERGECONTEXTMENU_TOP = 10; // uFlags       LPQCMINFO
  DFM_GETHELPTEXTW = 11;         // idCmd,cchMax pszText -Unicode
  DFM_INVOKECOMMANDEX = 12;      // idCmd        PDFMICS
  DFM_MAPCOMMANDNAME = 13;       // idCmd *      pszCommandName
//  DFM_GETDEFSTATICID = 14;       // idCmd *      0
  DFM_GETVERBW = 15;             // idCmd,cchMax pszText -Unicode
  DFM_GETVERBA = 16;             // idCmd,cchMax pszText -Ansi

  // Extra command IDs
  // (from Axel Sommerfeldt and Henk Devos)
  DFM_CMD_DELETE = cardinal(-1);
  DFM_CMD_CUT = cardinal(-2);
  DFM_CMD_COPY = cardinal(-3);
  DFM_CMD_CREATESHORTCUT  = cardinal(-4);
//  DFM_CMD_PROPERTIES  = UINT(-5);
  DFM_CMD_NEWFOLDER	= cardinal(-6);
  DFM_CMD_PASTE = cardinal(-7);
  DFM_CMD_VIEWLIST	= cardinal(-8);
  DFM_CMD_VIEWDETAILS	= cardinal(-9);
  DFM_CMD_PASTELINK = cardinal(-10);
  DFM_CMD_PASTESPECIAL = cardinal(-11);
  DFM_CMD_MODALPROP = cardinal(-12);

implementation

uses LogError, SysUtils;

const
  CILGlobalClone = 20;
  CPathIsRoot = 29;
  CPathBuildRootW = 30;
  CPathFindExtension = 31;
  CPathAddBackslash = 32;
  CPathRemoveBlanks = 33;
  CPathFindFileName = 34;
  CPathRemoveFileSpec = 35;
  CPathAppend = 36;
  CPathCombineW = 37;
  CPathStripPath = 38;
  CPathIsUNCW = 39;
  CPathIsRelativeW = 40;
  CPathFileExists = 45;
  CPathMatchSpec = 46;
  CPathSetDlgItemPath = 48;
  CPathStripToRoot = 50;
  CPathGetArgs = 52;
  CPathQuoteSpaces = 55;
  CPathUnquoteSpaces = 56;
  CPathGetDriveNumber = 57;
  CExitWindowsDialog = 60;
  CRunFileDlg = 61;
  CCIDLData_CreateFromIDArray = 83;
  CSHRegisterDragDrop = 86;
  CSHRevokeDragDrop = 87;
  CSHFindComputer = 91;
  CSHOutOfMemoryMessageBox = 126;
  CPathFindOnPath = 145;
  CILGetNext = 153;
  CILGlobalFree = 156;
  CPathGetExtension = 158;
  CPathIsDirectory = 159;
  CSHNetConnectionDialog = 160;
  CDAD_SetDragImageFromListView = 177;
  CSHGetNewLinkInfoA = 179;
  CShellMessageBoxA = 183;
  CSHHandleDiskFull = 185;
  CPathParseIconLocation = 249;
  CPathRemoveExtension = 250;
  CPathRemoveArgs = 251;
  CExtractAssociatedIconExA = 261;
  CSHGetDiskFreeSpaceA = 311;
  CSHInvokePrinterCommandA = 335;
  CSHIsFileAvailableOffline = 337;
  CPathIsSameRoot = 650;
  CFileIconInit = 660;

var
  SaveExit: pointer;
  DLLHandle: THandle;
  ErrorMode: Integer;

//procedure NewExit; far;
//begin
//  ExitProc := SaveExit;
//  FreeLibrary(DLLHandle)
//end;

var
  DLLLoaded : boolean = False;

procedure LoadDLL;
begin
  if DLLLoaded then Exit;
  ErrorMode := SetErrorMode(SEM_NoOpenFileErrorBox);
  DLLHandle := LoadLibrary('SHELL32.DLL');
  if DLLHandle >= 32 then
  begin
    DLLLoaded := True;
//    SaveExit := ExitProc;
//    ExitProc := @NewExit;
    @ILGetNext  := GetProcAddress(DLLHandle,MakeIntResource(CILGetNext));
    @ILGlobalClone  := GetProcAddress(DLLHandle,MakeIntResource(CILGlobalClone));
    @ILGlobalFree  := GetProcAddress(DLLHandle,MakeIntResource(CILGlobalFree));
    @SHRegisterDragDrop  := GetProcAddress(DLLHandle,MakeIntResource(CSHRegisterDragDrop));
    @RunFileDlg := GetProcAddress(DLLHandle,MakeIntResource(CRunFileDlg));
    @ExitWindowsDialog := GetProcAddress(DLLHandle,MakeIntResource(CExitWindowsDialog));
    @SHFindComputer := GetProcAddress(DLLHandle,MakeIntResource(CSHFindComputer));
    @ShHandleDiskFull := GetProcAddress(DLLHandle,MakeIntResource(CShHandleDiskFull));
    @SHOutOfMemoryMessageBox := GetProcAddress(DLLHandle,MakeIntResource(CSHOutOfMemoryMessageBox));
    @SHNetConnectionDialog := GetProcAddress(DLLHandle,MakeIntResource(CSHNetConnectionDialog));
    @ShellMessageBoxA := GetProcAddress(DLLHandle,MakeIntResource(CShellMessageBoxA));
    @FileIconInit := GetProcAddress(DLLHandle,MakeIntResource(CFileIconInit));
    @SHRevokeDragDrop := GetProcAddress(DLLHandle,MakeIntResource(CSHRevokeDragDrop));
    @DAD_SetDragImageFromListView := GetProcAddress(DLLHandle,MakeIntResource(CDAD_SetDragImageFromListView));
    @CIDLData_CreateFromIDArray := GetProcAddress(DLLHandle,MakeIntResource(CCIDLData_CreateFromIDArray));
    @PathAppend := GetProcAddress(DLLHandle,MakeIntResource(CPathAppend));
    @PathCombine := GetProcAddress(DLLHandle,MakeIntResource(CPathCombineW));
    @PathAddBackslash := GetProcAddress(DLLHandle,MakeIntResource(CPathAddBackslash));
    @PathBuildRoot := GetProcAddress(DLLHandle,MakeIntResource(CPathBuildRootW));
    @PathFindFileName := GetProcAddress(DLLHandle,MakeIntResource(CPathFindFileName));
    @PathFindExtension := GetProcAddress(DLLHandle,MakeIntResource(CPathFindExtension));
    @PathGetExtension := GetProcAddress(DLLHandle,MakeIntResource(CPathGetExtension));
    @PathGetArgs := GetProcAddress(DLLHandle,MakeIntResource(CPathGetArgs));
    @PathGetDriveNumber := GetProcAddress(DLLHandle,MakeIntResource(CPathGetDriveNumber));
    @PathRemoveFileSpec := GetProcAddress(DLLHandle,MakeIntResource(CPathRemoveFileSpec));
    @PathRemoveBlanks := GetProcAddress(DLLHandle,MakeIntResource(CPathRemoveBlanks));
    @PathQuoteSpaces := GetProcAddress(DLLHandle,MakeIntResource(CPathQuoteSpaces));
    @PathUnquoteSpaces := GetProcAddress(DLLHandle,MakeIntResource(CPathUnquoteSpaces));
    @PathIsUNC := GetProcAddress(DLLHandle,MakeIntResource(CPathIsUNCW));
    @PathIsRelative := GetProcAddress(DLLHandle,MakeIntResource(CPathIsRelativeW));
    @PathIsRoot := GetProcAddress(DLLHandle,MakeIntResource(CPathIsRoot));
    @PathIsDirectory := GetProcAddress(DLLHandle,MakeIntResource(CPathIsDirectory));
    @PathFileExists := GetProcAddress(DLLHandle,MakeIntResource(CPathFileExists));
    @PathMatchSpec := GetProcAddress(DLLHandle,MakeIntResource(CPathMatchSpec));
    @PathFindOnPath := GetProcAddress(DLLHandle,MakeIntResource(CPathFindOnPath));
    @PathSetDlgItemPath := GetProcAddress(DLLHandle,MakeIntResource(CPathSetDlgItemPath));
    @PathStripPath := GetProcAddress(DLLHandle,MakeIntResource(CPathStripPath));
    @PathStripToRoot := GetProcAddress(DLLHandle,MakeIntResource(CPathStripToRoot));
    @PathRemoveArgs := GetProcAddress(DLLHandle,MakeIntResource(CPathRemoveArgs));
    @PathRemoveExtension := GetProcAddress(DLLHandle,MakeIntResource(CPathRemoveExtension));
    @PathParseIconLocation := GetProcAddress(DLLHandle,MakeIntResource(CPathParseIconLocation));
    @PathIsSameRoot := GetProcAddress(DLLHandle,MakeIntResource(CPathIsSameRoot));
    @ExtractAssociatedIconExA := GetProcAddress(DLLHandle,MakeIntResource(CExtractAssociatedIconExA));
    @SHGetNewLinkInfo := GetProcAddress(DLLHandle,MakeIntResource(CSHGetNewLinkInfoA));
    @SHGetDiskFreeSpace := GetProcAddress(DLLHandle,MakeIntResource(CSHGetDiskFreeSpaceA));
    @SHInvokePrinterCommand := GetProcAddress(DLLHandle,MakeIntResource(CSHInvokePrinterCommandA));
    @SHIsFileAvailableOffline := GetProcAddress(DLLHandle,MakeIntResource(CSHIsFileAvailableOffline));
    @SHGetIconOverlayIndex := GetProcAddress(DLLHandle,'SHGetIconOverlayIndexA');
    @SHGetIconOverlayIndexA := GetProcAddress(DLLHandle,'SHGetIconOverlayIndexA');
    @SHGetIconOverlayIndexW := GetProcAddress(DLLHandle,'SHGetIconOverlayIndexW');
  end else begin
    DLLLoaded := False;
  end;
  SetErrorMode(ErrorMode)
end;

function IUnknown_QueryService(punkSite : IUnknown; sid : TGUID; riid : TGUID; var ppv) : HResult;
var
  psp : IServiceProvider;
begin
  try
    Pointer(ppv) := nil;
    Result := E_FAIL;
    if Assigned(punkSite) then
    begin
      Result := punkSite.QueryInterface(IServiceProvider, psp);
      if SUCCEEDED(Result) then
        Result := psp.QueryService(sid, riid, ppv);
    end;
  except
    on E: Exception do begin
      LogMessage('IUnknown_QueryService : ' + e.Message);
      Raise;
    end;
  end;
end;

function FileCabinet_GetIShellBrowser(wnd : HWND) : IShellBrowser;
begin
  try
    Result := IShellBrowser(SendMessage(wnd, CWM_GETISHELLBROWSER, 0, 0));
  except
    on E: Exception do begin
      LogMessage('FileCabinet_GetIShellBrowser : ' + e.Message);
      Raise;
    end;
  end;
end;

procedure FileCabinet_SelectItem(wnd : HWND; bSel : boolean; lpidl : PItemIdList);
begin
  try
    SendMessage(wnd, CWM_SELECTITEM, Integer(bSel), Integer(lpidl));
  except
    on E: Exception do begin
      LogMessage('FileCabinet_SelectItem : ' + e.Message);
      Raise;
    end;
  end;
end;

function ShellFolderView_GetArrangeColumn(wnd : hwnd) : cardinal;
begin
  Result := SHShellFolderView_Message(wnd, SFVM_GETARRANGECOLUMN, 0);
end;

function ShellFolderView_GetItemCount(wnd : hwnd) : cardinal;
begin
  Result := SHShellFolderView_Message(wnd, SFVM_GETITEMCOUNT, 0);
end;

function ShellFolderView_GetItemPidl(wnd : HWND; index : cardinal) : PItemIdList;
begin
  Result := PItemIdList(SHShellFolderView_Message(wnd, SFVM_GETITEMPIDL, index));
end;

function ShellFolderView_SetRedraw(wnd : HWND; mode : bool) : LPARAM;
begin
  Result := SHShellFolderView_Message(wnd, SFVM_SETREDRAW, LPARAM(mode));
end;

function ShellFolderView_IsDropOnSource(wnd : HWND; target : IDropTarget) : bool;
begin
  Result := BOOL(SHShellFolderView_Message(wnd, SFVM_ISDROPONSOURCE, LPARAM(target)))
end;

procedure ShellFolderView_MoveIcons(wnd: HWND; target : IDropTarget);
begin
  SHShellFolderView_Message(wnd, SFVM_MOVEICONS, LPARAM(target));
end;

function ShellFolderView_GetDragPoint(wnd : HWND; pt : PPoint) : bool;
begin
  Result := BOOL(SHShellFolderView_Message(wnd, SFVM_GETDRAGPOINT, LPARAM(pt)));
end;

function ShellFolderView_GetDropPoint(wnd : HWND; pt : PPoint) : bool;
begin
  Result := BOOL(SHShellFolderView_Message(wnd, SFVM_GETDROPPOINT, LPARAM(pt)));
end;

function ShellFolderView_IsDropOnBackground(wnd : HWND; target : IDropTarget) : bool;
begin
  Result := BOOL(SHShellFolderView_Message(wnd, SFVM_ISDROPONBACKGROUND, LPARAM(target)));
end;

procedure ShellFolderView_ToggleAutoArrange(wnd : HWND);
begin
  SHShellFolderView_Message(wnd, SFVM_TOGGLEAUTOARRANGE, 0);
end;

procedure ShellFolderView_LineUpIcons(wnd : HWND);
begin
  SHShellFolderView_Message(wnd, SFVM_LINEUPICONS, 0);
end;

function ShellFolderView_GetAutoArrange(wnd : HWND) : BOOL;
begin
  Result := BOOL(SHShellFolderView_Message(wnd, SFVM_GETAUTOARRANGE, 0));
end;

function ShellFolderView_GetSelectedCount(wnd : HWND) : cardinal;
begin
  Result := Cardinal(SHShellFolderView_Message(wnd, SFVM_GETSELECTEDCOUNT, 0));
end;

function ShellFolderView_GetItemSpacing(wnd : HWND; var spacing : TItemSpacing) : bool;
begin
  Result := BOOL(SHShellFolderView_Message(wnd, SFVM_GETITEMSPACING, LPARAM(@spacing)));
end;

function ShellFolderView_RefreshObject(wnd : HWND; pidl : PItemIdList): cardinal;
begin
  Result := SHShellFolderView_Message(wnd, SFVM_REFRESHOBJECT, LPARAM(pidl));
end;

function ShellFolderView_RefreshAll(wnd : HWND) : bool;
begin
  Result := PostMessage(wnd, WM_COMMAND, FCIDM_MENU_VIEW_REFRESH, 0);
end;

initialization
  LoadDLL;
finalization
  FreeLibrary(DLLHandle)
end.



ShellApi2.pas

//==========================================================================
// File : ShellApi2.pas
//
// Definitions for shell functions in Delphi, according to the August 2002
// version of the Microsoft Platform SDK
//
// Author : K. Giddings
//
//
// This file is part of the NSELib library for Delphi.
// NSELib can be found on the web on this URL:
// http://www.whirlingdervishes.com/nselib
// The documentation can be found at this URL:
// http://www.whirlingdervishes.com/nselib/help
//
// This source code is the property of its authors and may not be
// redistributed or reproduced in any form without the written
// permission of the authors.
// You can use it in commercial software if you purchased the license
// to do so.
//
// Copyright (C) 1998-2004 Whirling Dervishes Software
//
// This software is provided 'as-is', without any express or implied
// warranty.  In no event will the authors be held liable for any damages
// arising from the use of this software.
//
//==========================================================================

unit ShellAPI2;

{$WEAKPACKAGEUNIT}

interface

uses Windows, ActiveX, Messages;

function ExtractAssociatedIconEx(inst : HINST; IconPath : PChar; var IconIndex : word; var IconId : word) : HIcon; stdcall;
function ExtractAssociatedIconExA(inst : HINST; IconPath : PAnsiChar; var IconIndex : word; var IconId : word) : HIcon; stdcall;
function ExtractAssociatedIconExW(inst : HINST; IconPath : PWideChar;  var IconIndex : word; var IconId : word) : HIcon; stdcall;

const
  { AppBar stuff }
  {$EXTERNALSYM ABM_SETSTATE}
  ABM_SETSTATE         = $0000000a;

{ Shell File Operations }

const
  {$EXTERNALSYM FOF_NOCOPYSECURITYATTRIBS}
  FOF_NOCOPYSECURITYATTRIBS  = $0800;  // dont copy NT file Security Attributes
  {$EXTERNALSYM FOF_NORECURSION}
  FOF_NORECURSION            = $1000;  // don't recurse into directories.
  {$EXTERNALSYM FOF_NO_CONNECTED_ELEMENTS}
  FOF_NO_CONNECTED_ELEMENTS  = $2000;  // don't operate on connected elements.
  {$EXTERNALSYM FOF_WANTNUKEWARNING}
  FOF_WANTNUKEWARNING        = $4000;  // during delete operation, warn if nuking instead of recycling (partially overrides FOF_NOCONFIRMATION)
  {$EXTERNALSYM FOF_NORECURSEREPARSE}
  FOF_NORECURSEREPARSE       = $8000;  // treat reparse points as objects, not containers

{ ShellExecute() and ShellExecuteEx() error codes }
const
  {$EXTERNALSYM SEE_MASK_HMONITOR}
  SEE_MASK_HMONITOR       = $00200000;
  {$EXTERNALSYM SEE_MASK_NOZONECHECKS}
  SEE_MASK_NOZONECHECKS   = $00800000;
  {$EXTERNALSYM SEE_MASK_NOQUERYCLASSSTORE}
  SEE_MASK_NOQUERYCLASSSTORE = $01000000;
  {$EXTERNALSYM SEE_MASK_WAITFORINPUTIDLE}
  SEE_MASK_WAITFORINPUTIDLE  = $02000000;
  {$EXTERNALSYM SEE_MASK_FLAG_LOG_USAGE}
  SEE_MASK_FLAG_LOG_USAGE    = $04000000;

{$EXTERNALSYM WinExecError}
procedure WinExecError(wnd : HWND; error : integer; lpstrFileName, lpstrTitle : LPCSTR); stdcall;
{$EXTERNALSYM WinExecErrorA}
procedure WinExecErrorA(wnd : HWND; error : integer; lpstrFileName, lpstrTitle : LPCSTR); stdcall;
{$EXTERNALSYM WinExecErrorW}
procedure WinExecErrorW(wnd : HWND; error : integer; lpstrFileName, lpstrTitle : LPCWSTR); stdcall;

type
  {$EXTERNALSYM SHCREATEPROCESSINFOW}
  SHCREATEPROCESSINFOW = packed record
    cbSize : DWORD;
    fMask : ULONG;
    wnd : HWND;
    pszFile : LPCWSTR;
    pszParameters : LPCWSTR;
    pszCurrentDirectory : LPCWSTR;
    hUserToken : THandle;
    lpProcessAttributes : PSecurityAttributes;
    lpThreadAttributes : PSecurityAttributes;
    bInheritHandles : bool;
    dwCreationFlags : DWORD;
    lpStartupInfo : PSTARTUPINFO;
    lpProcessInformation : PPROCESSINFORMATION;
  end;
  {$EXTERNALSYM PSHCREATEPROCESSINFOW}
  PSHCREATEPROCESSINFOW = ^SHCREATEPROCESSINFOW;

{$EXTERNALSYM SHCreateProcessAsUserW}
function SHCreateProcessAsUserW(pscpi : PSHCREATEPROCESSINFOW) : bool; stdcall;

// struct for query recycle bin info
type
  {$EXTERNALSYM SHQUERYRBINFO}
  SHQUERYRBINFO = packed record
    cbSize : DWORD;
    i64Size : int64;
    i64NumItems : int64;
  end;
  {$EXTERNALSYM LPSHQUERYRBINFO}
  LPSHQUERYRBINFO = ^SHQUERYRBINFO;


// flags for SHEmptyRecycleBin
//
const
  {$EXTERNALSYM SHERB_NOCONFIRMATION}
  SHERB_NOCONFIRMATION   = $00000001;
  {$EXTERNALSYM SHERB_NOPROGRESSUI}
  SHERB_NOPROGRESSUI     = $00000002;
  {$EXTERNALSYM SHERB_NOSOUND}
  SHERB_NOSOUND          = $00000004;


{$EXTERNALSYM SHQueryRecycleBin}
function SHQueryRecycleBin(pszRootPath : LPCSTR; pSHQueryRBInfo : LPSHQUERYRBINFO) : HResult; stdcall;
{$EXTERNALSYM SHQueryRecycleBinA}
function SHQueryRecycleBinA(pszRootPath : LPCSTR; pSHQueryRBInfo : LPSHQUERYRBINFO) : HResult; stdcall;
{$EXTERNALSYM SHQueryRecycleBinW}
function SHQueryRecycleBinW(pszRootPath : LPCWSTR; pSHQueryRBInfo : LPSHQUERYRBINFO) : HResult; stdcall;
{$EXTERNALSYM SHEmptyRecycleBin}
function SHEmptyRecycleBin(wnd : HWND; pszRootPath : LPCSTR; dwFlags : DWORD) : HResult; stdcall;
{$EXTERNALSYM SHEmptyRecycleBinA}
function SHEmptyRecycleBinA(wnd : HWND; pszRootPath : LPCSTR; dwFlags : DWORD) : HResult; stdcall;
{$EXTERNALSYM SHEmptyRecycleBinW}
function SHEmptyRecycleBinW(wnd : HWND; pszRootPath : LPCWSTR; dwFlags : DWORD) : HResult; stdcall;

type
  PNotifyIconDataA = ^TNotifyIconDataA;
  PNotifyIconDataW = ^TNotifyIconDataW;
  PNotifyIconData = PNotifyIconDataA;
  {$EXTERNALSYM _NOTIFYICONDATAA}
  _NOTIFYICONDATAA = record
    cbSize: DWORD;
    Wnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array [0..127] of AnsiChar;
    dwState : DWORD;
    dwStateMask : DWORD;
    szInfo : array [0..255] of AnsiChar;
    uTimeoutOrVersion : UINT;
    szInfoTitle : array [0..63] of AnsiChar;
    dwInfoFlags : DWORD;
    guidItem : TGUID;
  end;

  {$EXTERNALSYM _NOTIFYICONDATAW}
  _NOTIFYICONDATAW = record
    cbSize: DWORD;
    Wnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array [0..127] of WideChar;
    dwState : DWORD;
    dwStateMask : DWORD;
    szInfo : array [0..255] of WideChar;
    uTimeoutOrVersion : UINT;
    szInfoTitle : array [0..63] of WideChar;
    dwInfoFlags : DWORD;
    guidItem : TGUID;
  end;
  {$EXTERNALSYM _NOTIFYICONDATA}
  _NOTIFYICONDATA = _NOTIFYICONDATAA;
  TNotifyIconDataA = _NOTIFYICONDATAA;
  TNotifyIconDataW = _NOTIFYICONDATAW;
  TNotifyIconData = TNotifyIconDataA;
  {$EXTERNALSYM NOTIFYICONDATAA}
  NOTIFYICONDATAA = _NOTIFYICONDATAA;
  {$EXTERNALSYM NOTIFYICONDATAW}
  NOTIFYICONDATAW = _NOTIFYICONDATAW;
  {$EXTERNALSYM NOTIFYICONDATA}
  NOTIFYICONDATA = NOTIFYICONDATAA;

const
  {$EXTERNALSYM NIN_SELECT}
  NIN_SELECT          = (WM_USER + 0);
  {$EXTERNALSYM NINF_KEY}
  NINF_KEY            = 1;
  {$EXTERNALSYM NIN_KEYSELECT}
  NIN_KEYSELECT       = (NIN_SELECT or NINF_KEY);

  {$EXTERNALSYM NIN_BALLOONSHOW}
  NIN_BALLOONSHOW     = (WM_USER + 2);
  {$EXTERNALSYM NIN_BALLOONHIDE}
  NIN_BALLOONHIDE     = (WM_USER + 3);
  {$EXTERNALSYM NIN_BALLOONTIMEOUT}
  NIN_BALLOONTIMEOUT  = (WM_USER + 4);
  {$EXTERNALSYM NIN_BALLOONUSERCLICK}
  NIN_BALLOONUSERCLICK = (WM_USER + 5);

  {$EXTERNALSYM NIM_SETFOCUS}
  NIM_SETFOCUS    = $00000003;
  {$EXTERNALSYM NIM_SETVERSION}
  NIM_SETVERSION  = $00000004;
  {$EXTERNALSYM NOTIFYICON_VERSION}
  NOTIFYICON_VERSION = 3;

  {$EXTERNALSYM NIF_STATE}
  NIF_STATE       = $00000008;
  {$EXTERNALSYM NIF_INFO}
  NIF_INFO        = $00000010;
  {$EXTERNALSYM NIF_GUID}
  NIF_GUID        = $00000020;

  {$EXTERNALSYM NIS_HIDDEN}
  NIS_HIDDEN      = $00000001;
  {$EXTERNALSYM NIS_SHAREDICON}
  NIS_SHAREDICON  = $00000002;

  {$EXTERNALSYM NIIF_NONE}
  NIIF_NONE       = $00000000;
  // icon flags are mutually exclusive
  // and take only the lowest 2 bits
  {$EXTERNALSYM NIIF_INFO}
  NIIF_INFO       = $00000001;
  {$EXTERNALSYM NIIF_WARNING}
  NIIF_WARNING    = $00000002;
  {$EXTERNALSYM NIIF_ERROR}
  NIIF_ERROR      = $00000003;
  {$EXTERNALSYM NIIF_ICON_MASK}
  NIIF_ICON_MASK  = $0000000F;
  {$EXTERNALSYM NIIF_NOSOUND}
  NIIF_NOSOUND    = $00000010;


const
  {$EXTERNALSYM SHGFI_ATTR_SPECIFIED}
  SHGFI_ATTR_SPECIFIED    = $000020000;     // get only specified attributes
  {$EXTERNALSYM SHGFI_ADDOVERLAYS}
  SHGFI_ADDOVERLAYS       = $000000020;     // apply the appropriate overlays
  {$EXTERNALSYM SHGFI_OVERLAYINDEX}
  SHGFI_OVERLAYINDEX      = $000000040;     // Get the index of the overlay

{$EXTERNALSYM SHGetDiskFreeSpace}
function SHGetDiskFreeSpace(pszDirectoryName : LPCSTR; var pulFreeBytesAvailableToCaller,pulTotalNumberOfBytes,pulTotalNumberOfFreeBytes : int64) : bool; stdcall;
{$EXTERNALSYM SHGetDiskFreeSpaceEx}
function SHGetDiskFreeSpaceEx(pszDirectoryName : LPCSTR; var pulFreeBytesAvailableToCaller,pulTotalNumberOfBytes,pulTotalNumberOfFreeBytes : int64) : bool; stdcall;
{$EXTERNALSYM SHGetDiskFreeSpaceExA}
function SHGetDiskFreeSpaceExA(pszDirectoryName : LPCSTR; var pulFreeBytesAvailableToCaller,pulTotalNumberOfBytes,pulTotalNumberOfFreeBytes : int64) : bool; stdcall;
{$EXTERNALSYM SHGetDiskFreeSpaceExW}
function SHGetDiskFreeSpaceExW(pszDirectoryName : LPCWSTR; var pulFreeBytesAvailableToCaller,pulTotalNumberOfBytes,pulTotalNumberOfFreeBytes : int64) : bool; stdcall;

{$EXTERNALSYM SHGetNewLinkInfo}
function SHGetNewLinkInfo(pszLinkTo, pszDir : LPCSTR; pszName : LPSTR; var pfMustCopy : bool; uFlags : uint) : bool; stdcall;
{$EXTERNALSYM SHGetNewLinkInfoA}
function SHGetNewLinkInfoA(pszLinkTo, pszDir : LPCSTR; pszName : LPSTR; var pfMustCopy : bool; uFlags : uint) : bool; stdcall;
{$EXTERNALSYM SHGetNewLinkInfoW}
function SHGetNewLinkInfoW(pszLinkTo, pszDir : LPCWSTR; pszName : LPWSTR; var pfMustCopy : bool; uFlags : uint) : bool; stdcall;

const
  {$EXTERNALSYM SHGNLI_NOLNK}
  SHGNLI_NOLNK            = $000000008;     // don't add ".lnk" extension

// Printer stuff
const
  {$EXTERNALSYM PRINTACTION_OPEN}
  PRINTACTION_OPEN           = 0;
  {$EXTERNALSYM PRINTACTION_PROPERTIES}
  PRINTACTION_PROPERTIES     = 1;
  {$EXTERNALSYM PRINTACTION_NETINSTALL}
  PRINTACTION_NETINSTALL     = 2;
  {$EXTERNALSYM PRINTACTION_NETINSTALLLINK}
  PRINTACTION_NETINSTALLLINK = 3;
  {$EXTERNALSYM PRINTACTION_TESTPAGE}
  PRINTACTION_TESTPAGE       = 4;
  {$EXTERNALSYM PRINTACTION_OPENNETPRN}
  PRINTACTION_OPENNETPRN     = 5;
  {$EXTERNALSYM PRINTACTION_DOCUMENTDEFAULTS}
  PRINTACTION_DOCUMENTDEFAULTS = 6;
  {$EXTERNALSYM PRINTACTION_SERVERPROPERTIES}
  PRINTACTION_SERVERPROPERTIES = 7;

{$EXTERNALSYM SHInvokePrinterCommand}
function SHInvokePrinterCommand(wnd : HWND; uAction : UINT; lpBuf1, lpBuf2 : LPCSTR; fModal : BOOL) : bool; stdcall;
{$EXTERNALSYM SHInvokePrinterCommandA}
function SHInvokePrinterCommandA(wnd : HWND; uAction : UINT; lpBuf1, lpBuf2 : LPCSTR; fModal : BOOL) : bool; stdcall;
{$EXTERNALSYM SHInvokePrinterCommandW}
function SHInvokePrinterCommandW(wnd : HWND; uAction : UINT; lpBuf1, lpBuf2 : LPCSTR; fModal : BOOL) : bool; stdcall;

//
// The SHLoadNonloadedIconOverlayIdentifiers API causes the shell's
// icon overlay manager to load any registered icon overlay
// identifers that are not currently loaded.  This is useful if an
// overlay identifier did not load at shell startup but is needed
// and can be loaded at a later time.  Identifiers already loaded
// are not affected.  Overlay identifiers implement the
// IShellIconOverlayIdentifier interface.
//
// Returns:
//      S_OK
//
{$EXTERNALSYM SHLoadNonloadedIconOverlayIdentifiers}
function SHLoadNonloadedIconOverlayIdentifiers : hresult; stdcall;

//
// The SHIsFileAvailableOffline API determines whether a file
// or folder is available for offline use.
//
// Parameters:
//     pwszPath             file name to get info about
//     pdwStatus            (optional) OFFLINE_STATUS_* flags returned here
//
// Returns:
//     S_OK                 File/directory is available offline, unless
//                            OFFLINE_STATUS_INCOMPLETE is returned.
//     E_INVALIDARG         Path is invalid, or not a net path
//     E_FAIL               File/directory is not available offline
//
// Notes:
//     OFFLINE_STATUS_INCOMPLETE is never returned for directories.
//     Both OFFLINE_STATUS_LOCAL and OFFLINE_STATUS_REMOTE may be returned,
//     indicating "open in both places." This is common when the server is online.
//
{$EXTERNALSYM SHIsFileAvailableOffline}
function SHIsFileAvailableOffline(pwszPath : LPCWSTR; pdwStatus : LPDWORD) : hresult; stdcall;

const
  {$EXTERNALSYM OFFLINE_STATUS_LOCAL}
  OFFLINE_STATUS_LOCAL        = $0001;  // If open, it's open locally
  {$EXTERNALSYM OFFLINE_STATUS_REMOTE}
  OFFLINE_STATUS_REMOTE       = $0002;  // If open, it's open remotely
  {$EXTERNALSYM OFFLINE_STATUS_INCOMPLETE}
  OFFLINE_STATUS_INCOMPLETE   = $0004;  // The local copy is currently imcomplete.
                                            // The file will not be available offline
                                            // until it has been synchronized.

//  sets the specified path to use the string resource
//  as the UI instead of the file system name
{$EXTERNALSYM SHSetLocalizedName}
function SHSetLocalizedName(pszPath : LPWSTR; pszResModule : LPCWSTR; idsRes : integer) : HResult; stdcall;

//====== ShellMessageBox ================================================
// If lpcTitle is NULL, the title is taken from hWnd
// If lpcText is NULL, this is assumed to be an Out Of Memory message
// If the selector of lpcTitle or lpcText is NULL, the offset should be a
//     string resource ID
// The variable arguments must all be 32-bit values (even if fewer bits
//     are actually used)
// lpcText (or whatever string resource it causes to be loaded) should
//     be a formatting string similar to wsprintf except that only the
//     following formats are available:
//         %%              formats to a single '%'
//         %nn%s           the nn-th arg is a string which is inserted
//         %nn%ld          the nn-th arg is a DWORD, and formatted decimal
//         %nn%lx          the nn-th arg is a DWORD, and formatted hex
//     note that lengths are allowed on the %s, %ld, and %lx, just
//                         like wsprintf
//
{$EXTERNALSYM ShellMessageBox}
function ShellMessageBox(Instance : THandle; wnd : HWnd; Msg : PChar;
                          Title : PChar; uStyle : cardinal;
                          Format : PChar) : integer; cdecl;
{$EXTERNALSYM ShellMessageBoxA}
function ShellMessageBoxA(Instance : THandle; wnd : HWnd; Msg : PChar;
                          Title : PChar; uStyle : cardinal;
                          Format : PChar) : integer; cdecl;
{$EXTERNALSYM ShellMessageBoxW}
function ShellMessageBoxW(Instance : THandle; wnd : HWnd; Msg : PWideChar;
                          Title : PWideChar; uStyle : cardinal;
                          Format : PWideChar) : integer; cdecl;

{$EXTERNALSYM IsLFNDrive}
function IsLFNDrive(pszPath : LPCSTR) : bool; stdcall;
{$EXTERNALSYM IsLFNDriveA}
function IsLFNDriveA(pszPath : LPCSTR) : bool; stdcall;
{$EXTERNALSYM IsLFNDriveW}
function IsLFNDriveW(pszPath : LPCSTR) : bool; stdcall;

{$EXTERNALSYM SHEnumerateUnreadMailAccounts}
function SHEnumerateUnreadMailAccounts(hKeyUser : HKEY; dwIndex : DWORD; pszMailAddress : LPSTR; cchMailAddress : integer) : hresult; stdcall;
{$EXTERNALSYM SHEnumerateUnreadMailAccountsA}
function SHEnumerateUnreadMailAccountsA(hKeyUser : HKEY; dwIndex : DWORD; pszMailAddress : LPSTR; cchMailAddress : integer) : hresult; stdcall;
{$EXTERNALSYM SHEnumerateUnreadMailAccountsW}
function SHEnumerateUnreadMailAccountsW(hKeyUser : HKEY; dwIndex : DWORD; pszMailAddress : LPWSTR; cchMailAddress : integer) : hresult; stdcall;

{$EXTERNALSYM SHGetUnreadMailCount}
function SHGetUnreadMailCount(hKeyUser : HKEY; pszMailAddress : LPCSTR; var pdwCount : DWORD; var pFileTime : TFILETIME; pszShellExecuteCommand : LPSTR; cchShellExecuteCommand : integer) : hresult; stdcall;
{$EXTERNALSYM SHGetUnreadMailCountA}
function SHGetUnreadMailCountA(hKeyUser : HKEY; pszMailAddress : LPCSTR; var pdwCount : DWORD; var pFileTime : TFILETIME; pszShellExecuteCommand : LPSTR; cchShellExecuteCommand : integer) : hresult; stdcall;
{$EXTERNALSYM SHGetUnreadMailCountW}
function SHGetUnreadMailCountW(hKeyUser : HKEY; pszMailAddress : LPCWSTR; var pdwCount : DWORD; var pFileTime : TFILETIME; pszShellExecuteCommand : LPWSTR; cchShellExecuteCommand : integer) : hresult; stdcall;


{$EXTERNALSYM SHSetUnreadMailCount}
function SHSetUnreadMailCount(pszMailAddress : LPCSTR; dwCount : DWORD; pszShellExecuteCommand : LPCSTR) : hresult; stdcall;
{$EXTERNALSYM SHSetUnreadMailCountA}
function SHSetUnreadMailCountA(pszMailAddress : LPCSTR; dwCount : DWORD; pszShellExecuteCommand : LPCSTR) : hresult; stdcall;
{$EXTERNALSYM SHSetUnreadMailCountW}
function SHSetUnreadMailCountW(pszMailAddress : LPCWSTR; dwCount : DWORD; pszShellExecuteCommand : LPCWSTR) : hresult; stdcall;

{$EXTERNALSYM SHTestTokenMembership}
function SHTestTokenMembership(hToken : THandle; ulRID : cardinal) : bool; stdcall;

{$EXTERNALSYM SHGetImageList}
function SHGetImageList(iImageList : integer; riid : TIID; var ppvObj : Pointer) : hresult; stdcall;

const
  {$EXTERNALSYM SHIL_LARGE}
  SHIL_LARGE         = 0;   // normally 32x32
  {$EXTERNALSYM SHIL_SMALL}
  SHIL_SMALL         = 1;   // normally 16x16
  {$EXTERNALSYM SHIL_EXTRALARGE}
  SHIL_EXTRALARGE    = 2;
  {$EXTERNALSYM SHIL_SYSSMALL}
  SHIL_SYSSMALL      = 3;   // like SHIL_SMALL, but tracks system small icon metric correctly

  {$EXTERNALSYM SHIL_LAST}
  SHIL_LAST          = SHIL_SYSSMALL;

type
  {$EXTERNALSYM PFNCANSHAREFOLDERW}
  PFNCANSHAREFOLDERW = function (pszPath : LPCWSTR): hresult; stdcall;
  {$EXTERNALSYM PFNSHOWSHAREFOLDERUIW}
  PFNSHOWSHAREFOLDERUIW = function(hwndParent : hwnd; pszPath : LPCWSTR) : hresult; stdcall;

const
  shell32 = 'shell32.dll';

implementation

function ExtractAssociatedIconEx; external shell32 name 'ExtractAssociatedIconExA';
function ExtractAssociatedIconExA; external shell32 name 'ExtractAssociatedIconExA';
function ExtractAssociatedIconExW; external shell32 name 'ExtractAssociatedIconExW';
procedure WinExecError; external shell32 name 'WinExecErrorA';
procedure WinExecErrorA; external shell32 name 'WinExecErrorA';
procedure WinExecErrorW; external shell32 name 'WinExecErrorW';
function SHCreateProcessAsUserW; external shell32 name 'SHCreateProcessAsUserW';
function SHQueryRecycleBin; external shell32 name 'SHQueryRecycleBinA';
function SHQueryRecycleBinA; external shell32 name 'SHQueryRecycleBinA';
function SHQueryRecycleBinW; external shell32 name 'SHQueryRecycleBinW';
function SHEmptyRecycleBin; external shell32 name 'SHEmptyRecycleBinA';
function SHEmptyRecycleBinA; external shell32 name 'SHEmptyRecycleBinA';
function SHEmptyRecycleBinW; external shell32 name 'SHEmptyRecycleBinW';
function SHGetDiskFreeSpace; external shell32 name 'SHGetDiskFreeSpaceExA';
function SHGetDiskFreeSpaceEx; external shell32 name 'SHGetDiskFreeSpaceExA';
function SHGetDiskFreeSpaceExA; external shell32 name 'SHGetDiskFreeSpaceExA';
function SHGetDiskFreeSpaceExW; external shell32 name 'SHGetDiskFreeSpaceExW';
function SHGetNewLinkInfo; external shell32 name 'SHGetNewLinkInfoA';
function SHGetNewLinkInfoA; external shell32 name 'SHGetNewLinkInfoA';
function SHGetNewLinkInfoW; external shell32 name 'SHGetNewLinkInfoW';
function SHInvokePrinterCommand; external shell32 name 'SHInvokePrinterCommandA';
function SHInvokePrinterCommandA; external shell32 name 'SHInvokePrinterCommandA';
function SHInvokePrinterCommandW; external shell32 name 'SHInvokePrinterCommandW';
function SHLoadNonloadedIconOverlayIdentifiers; external shell32 name 'SHLoadNonloadedIconOverlayIdentifiers';
function SHIsFileAvailableOffline; external shell32 name 'SHIsFileAvailableOffline';
function SHSetLocalizedName; external shell32 name 'SHSetLocalizedName';
function ShellMessageBox; external shell32 name 'ShellMessageBoxA';
function ShellMessageBoxA; external shell32 name 'ShellMessageBoxA';
function ShellMessageBoxW; external shell32 name 'ShellMessageBoxW';
function IsLFNDrive; external shell32 name 'IsLFNDriveA';
function IsLFNDriveA; external shell32 name 'IsLFNDriveA';
function IsLFNDriveW; external shell32 name 'IsLFNDriveW';
function SHEnumerateUnreadMailAccounts; external shell32 name 'SHEnumerateUnreadMailAccountsA';
function SHEnumerateUnreadMailAccountsA; external shell32 name 'SHEnumerateUnreadMailAccountsA';
function SHEnumerateUnreadMailAccountsW; external shell32 name 'SHEnumerateUnreadMailAccountsW';
function SHGetUnreadMailCount; external shell32 name 'SHGetUnreadMailCountA';
function SHGetUnreadMailCountA; external shell32 name 'SHGetUnreadMailCountA';
function SHGetUnreadMailCountW; external shell32 name 'SHGetUnreadMailCountW';
function SHSetUnreadMailCount; external shell32 name 'SHSetUnreadMailCountA';
function SHSetUnreadMailCountA; external shell32 name 'SHSetUnreadMailCountA';
function SHSetUnreadMailCountW; external shell32 name 'SHSetUnreadMailCountW';
function SHTestTokenMembership; external shell32 name 'SHTestTokenMembership';
function SHGetImageList; external shell32 name 'SHGetImageList';
end.

ShlObj2.pas

//==========================================================================
// File : ShellObj2.pas
//
// Definitions for shell functions in Delphi, according to the August 2002
// version of the Microsoft Platform SDK
//
// Author : K. Giddings
//
//
// This file is part of the NSELib library for Delphi.
// NSELib can be found on the web on this URL:
// http://www.whirlingdervishes.com/nselib
// The documentation can be found at this URL:
// http://www.whirlingdervishes.com/nselib/help
//
// This source code is the property of its authors and may not be
// redistributed or reproduced in any form without the written
// permission of the authors.
// You can use it in commercial software if you purchased the license
// to do so.
//
// Copyright (C) 1998-2004 Whirling Dervishes Software
//
// This software is provided 'as-is', without any express or implied
// warranty.  In no event will the authors be held liable for any damages
// arising from the use of this software.
//
//==========================================================================

unit ShlObj2;

interface

uses ShlObj, ShellApi2, Windows, ActiveX, CommCtrl, ShellAPI, RegStr, Messages, WinInet, UrlMon;

const
  CLSID_NetworkPlaces: TGUID = (
    D1:$208D2C60; D2:$3AEA; D3:$1069; D4:($A2,$D7,$08,$00,$2B,$30,$30,$9D));

  CLSID_NetworkDomain: TGUID = (
    D1:$46e06680; D2:$4bf0; D3:$11d1; D4:($83,$ee,$00,$a0,$c9,$0d,$c8,$49));

  CLSID_NetworkServer: TGUID = (
    D1:$c0542a90; D2:$4bf0; D3:$11d1; D4:($83,$ee,$00,$a0,$c9,$0d,$c8,$49));

  CLSID_NetworkShare: TGUID = (
    D1:$54a754c0; D2:$4bf0; D3:$11d1; D4:($83,$ee,$00,$a0,$c9,$0d,$c8,$49));

  CLSID_MyComputer: TGUID = (
    D1:$20D04FE0; D2:$3AEA; D3:$1069; D4:($A2,$D8,$08,$00,$2B,$30,$30,$9D));

  CLSID_Internet: TGUID = (
    D1:$871C5380; D2:$42A0; D3:$1069; D4:($A2,$EA,$08,$00,$2B,$30,$30,$9D));

  CLSID_ShellFSFolder: TGUID = (
    D1:$F3364BA0; D2:$65B9; D3:$11CE; D4:($A9,$BA,$00,$AA,$00,$4A,$E8,$37));

  CLSID_RecycleBin: TGUID = (
    D1:$645FF040; D2:$5081; D3:$101B; D4:($9F,$08,$00,$AA,$00,$2F,$95,$4E));

  CLSID_ControlPanel: TGUID = (
    D1:$21EC2020; D2:$3AEA; D3:$1069; D4:($A2,$DD,$08,$00,$2B,$30,$30,$9D));

  CLSID_Printers: TGUID = (
    D1:$2227A280; D2:$3AEA; D3:$1069; D4:($A2,$DE,$08,$00,$2B,$30,$30,$9D));

  CLSID_MyDocuments: TGUID = (
    D1:$450d8fba; D2:$ad25; D3:$11d0; D4:($98,$a8,$08,$00,$36,$1b,$11,$03));

  CLSID_FolderShortcut: TGUID = (
    D1:$0AFACED1; D2:$E828; D3:$11D1; D4:($91, $87, $B5, $32, $F1, $E9, $57, $5D));

  CLSID_StgFolder: TGUID = (
    D1:$E773F1AF; D2:$3A65; D3:$4866; D4:($85, $7D, $84, $6F, $C9, $C4, $59, $8A));

  CLSID_CFSIconOverlayManager: TGUID = (
    D1:$63B51F81; D2:$C868; D3:$11D0; D4:($99, $9C, $00, $C0, $4F, $D6, $55, $E1));

  CLSID_ShellThumbnailDiskCache : TGUID = (
    D1:$1EBDCF80; D2:$A200; D3:$11D0; D4:($A3, $A4, $00, $C0, $4F, $D7, $06, $EC));

  CLSID_MenuBand : TGUID = (
    D1:$5b4dae26; D2:$b807; D3:$11d0; D4:($98, $15, $00, $c0, $4f, $d9, $19, $72));

  CLSID_InternetShortcut : TGUID = (
    D1:$FBF23B40; D2:$E3F0; D3:$101B; D4:($84, $88, $00, $AA, $00, $3E, $56, $F8));

  CLSID_CUrlHistory: TGUID = (
    D1:$3C374A40; D2:$BAE4; D3:$11CF; D4:($BF, $7D, $00, $AA, $00, $69, $46, $EE));

  CLSID_CURLSearchHook: TGUID = (
    D1:$CFBFAE00; D2:$17A6; D3:$11D0; D4:($99, $CB, $00, $C0, $4F, $D6, $44, $97));

  CLSID_ACLHistory: TGUID = (
    D1:$00BB2764; D2:$6A77; D3:$11D0; D4:($A5, $35, $00, $C0, $4F, $D7, $D0, $62));

  CLSID_ACListISF: TGUID = (
    D1:$03C036F1; D2:$A186; D3:$11D0; D4:($82, $4A, $00, $AA, $00, $5B, $43, $83));

  CLSID_ACLMRU: TGUID = (
    D1:$6756a641; D2:$de71; D3:$11d0; D4:($83, $1b, $0, $aa, $0, $5b, $43, $83));

  CLSID_ACLMulti: TGUID = (
    D1:$00BB2765; D2:$6A77; D3:$11D0; D4:($A5, $35, $00, $C0, $4F, $D7, $D0, $62));

  CLSID_ACLCustomMRU: TGUID = (
    D1:$6935db93; D2:$21e8; D3:$4ccc; D4:($be, $b9, $9f, $e3, $c7, $7a, $29, $7a));

  CLSID_ProgressDialog: TGUID = (
    D1:$f8383852; D2:$fcd3; D3:$11d1; D4:($a6, $b9, $0, $60, $97, $df, $5b, $d4));

  CLSID_QueryAssociations: TGUID = (
    D1:$a07034fd; D2:$6caa; D3:$4954; D4:($ac, $3f, $97, $a2, $72, $16, $f9, $8a));

  CLSID_DocFileColumnProvider: TGUID = (
    D1:$24f14f01; D2:$7b1c; D3:$11d1; D4:($83, $8f, $0, $0, $f8, $4, $61, $cf));

  CLSID_LinkColumnProvider: TGUID = (
    D1:$24f14f02; D2:$7b1c; D3:$11d1; D4:($83, $8f, $0, $0, $f8, $4, $61, $cf));

  CLSID_FileSysColumnProvider: TGUID = (
    D1:$d2e74c4; D2:$3c34; D3:$11d2; D4:($a2, $7e, $0, $c0, $4f, $c3, $8, $71));

  CLSID_FileTypes: TGUID = (
    D1:$b091e540; D2:$83e3; D3:$11cf; D4:($a7, $13, $0, $20, $af, $d7, $97, $62));

  CLSID_InternetButtons: TGUID = (
    D1:$1E796980; D2:$9CC5; D3:$11D1; D4:($A8, $3F, $00, $C0, $4F, $C9, $9D, $61));

  CLSID_MSOButtons: TGUID = (
    D1:$178f34b8; D2:$a282; D3:$11d2; D4:($86, $c5, $0, $c0, $4f, $8e, $ea, $99));

  CLSID_ToolbarExtButtons: TGUID = (
    D1:$2ce4b5d8; D2:$a28f; D3:$11d2; D4:($86, $c5, $0, $c0, $4f, $8e, $ea, $99));

  CLSID_DarwinAppPublisher: TGUID = (
    D1:$CFCCC7A0; D2:$A282; D3:$11D1; D4:($90, $82, $00, $60, $08, $05, $93, $82));

  CLSID_DocHostUIHandler: TGUID = (
    D1:$7057e952; D2:$bd1b; D3:$11d1; D4:($89, $19, $0, $c0, $4f, $c2, $c8, $36));

  CLSID_MountedVolume: TGUID = (
    D1:$12518493; D2:$00B2; D3:$11d2; D4:($9F, $A5, $9E, $34, $20, $52, $41, $53));

  CLSID_HWShellExecute: TGUID = (
    D1:$ffb8655f; D2:$81b9; D3:$4fce; D4:($b8, $9c, $9a, $6b, $a7, $6d, $13, $e7));

  CLSID_DragDropHelper: TGUID = (
    D1:$4657278a; D2:$411b; D3:$11d2; D4:($83, $9a, $0, $c0, $4f, $d9, $18, $d0));

  CLSID_CAnchorBrowsePropertyPage: TGUID = (
    D1:$3050f3BB; D2:$98b5; D3:$11cf; D4:($bb, $82, $00, $aa, $00, $bd, $ce, $0b));

  CLSID_CImageBrowsePropertyPage: TGUID = (
    D1:$3050f3B3; D2:$98b5; D3:$11cf; D4:($bb, $82, $00, $aa, $00, $bd, $ce, $0b));

  CLSID_CDocBrowsePropertyPage: TGUID = (
    D1:$3050f3B4; D2:$98b5; D3:$11cf; D4:($bb, $82, $00, $aa, $00, $bd, $ce, $0b));

  CLSID_FolderItem : TGUID = (
    D1:$fef10fa2; D2:$355e; D3:$4e06; D4:($93, $81, $9b, $24, $d7, $f7, $cc, $88));

  CLSID_FolderItemsFDF : TGUID = (
    D1:$53c74826; D2:$ab99; D3:$4d33; D4:($ac, $a4, $31, $17, $f5, $1d, $37, $88));

  CLSID_NewMenu : TGUID = (
    D1:$d969a300; D2:$e7ff; D3:$11d0; D4:($a9, $3b, $00, $a0, $c9, $0f, $27, $19));

  CLSID_QuickLinks : TGUID = (
    D1:$e5cbf21; D2:$d15f; D3:$11d0; D4:($83, $01, $00, $aa, $00, $5b, $43, $83));

  // The IShellFolder band
  CLSID_ISFBand : TGUID = (
    D1:$D82BE2B0; D2:$5764; D3:$11D0; D4:($A9, $6E, $00, $C0, $4F, $D7, $05, $A2));

  CLSID_ShellFldSetExt : TGUID = (
    D1:$6D5313C0; D2:$8C62; D3:$11D1; D4:($B2,$CD,$00,$60,$97,$DF,$8C,$11));

  CLSID_MenuToolbarBase : TGUID = (
    D1:$40b96610; D2:$b522; D3:$11d1; D4:($b3, $b4, $00, $aa, $00, $6e, $fd, $e7));

  CLSID_MenuBandSite : TGUID = (
    D1:$e13ef4e4; D2:$d2f2; D3:$11d0; D4:($98, $16, $00, $c0, $4f, $d9, $19, $72));

  STR_MYDOCS_CLSID = '{450D8FBA-AD25-11D0-98A8-0800361B1103}';

  //  BHIDs for IShellItem::BindToHandler()
  //  BHID_LocalCopyHelper = CLSID_LocalCopyHelper;
  BHID_SFObject: TGUID = (
    D1:$3981e224; D2:$f559; D3:$11d3; D4:($8e, $3a, $00, $c0, $4f, $68, $37, $d5));

  BHID_SFUIObject: TGUID = (
    D1:$3981e225; D2:$f559; D3:$11d3; D4:($8e, $3a, $00, $c0, $4f, $68, $37, $d5));

  BHID_SFViewObject: TGUID = (
    D1:$3981e226; D2:$f559; D3:$11d3; D4:($8e, $3a, $00, $c0, $4f, $68, $37, $d5));

  BHID_Storage: TGUID = (
    D1:$3981e227; D2:$f559; D3:$11d3; D4:($8e, $3a, $00, $c0, $4f, $68, $37, $d5));

  BHID_Stream: TGUID = (
    D1:$1cebb3ab; D2:$7c10; D3:$499a; D4:($a4, $17, $92, $ca, $16, $c4, $cb, $83));

  BHID_LinkTargetItem: TGUID = (
    D1:$3981e228; D2:$f559; D3:$11d3; D4:($8e, $3a, $00, $c0, $4f, $68, $37, $d5));

  BHID_StorageEnum: TGUID = (
    D1:$4621a4e3; D2:$f0d6; D3:$4773; D4:($8a, $9c, $46, $e7, $7b, $17, $48, $40));

  CATID_BrowsableShellExt: TGUID = (
    D1:$00021490; D2:$; D3:$; D4:($C0,0,0,0,0,0,0,$46));

  CATID_BrowseInPlace: TGUID = (
    D1:$00021491; D2:$; D3:$; D4:($C0,0,0,0,0,0,0,$46));

  CATID_DeskBand: TGUID = (
    D1:$00021492; D2:$; D3:$; D4:($C0,0,0,0,0,0,0,$46));

  CATID_InfoBand: TGUID = (
    D1:$00021493; D2:$; D3:$; D4:($C0,0,0,0,0,0,0,$46));

  CATID_CommBand: TGUID = (
    D1:$00021494; D2:$; D3:$; D4:($C0,0,0,0,0,0,0,$46));

  VID_LargeIcons : TGUID = (
    D1:$0057D0E0; D2:$3573; D3:$11CF; D4:($AE, $69, $08, $00, $2B, $2E, $12, $62));

  VID_SmallIcons : TGUID = (
    D1:$089000C0; D2:$3573; D3:$11CF; D4:($AE, $69, $08, $00, $2B, $2E, $12, $62));

  VID_List : TGUID = (
    D1:$E1FA5E0; D2:$3573; D3:$11CF; D4:($AE, $69, $08, $00, $2B, $2E, $12, $62));

  VID_Details : TGUID = (
    D1:$137E7700; D2:$3573; D3:$11CF; D4:($AE, $69, $08, $00, $2B, $2E, $12, $62));

  VID_Tile : TGUID = (
    D1:$65f125e5; D2:$7be1; D3:$4810; D4:($ba, $9d, $d2, $71, $c8, $43, $2c, $e3));

  VID_Thumbnails : TGUID = (
    D1:$8bebb290; D2:$52d0; D3:$11d0; D4:($b7, $f4, $00, $c0, $4f, $d7, $06, $ec));

  VID_ThumbStrip : TGUID = (
    D1:$8EEFA624; D2:$D1E9; D3:$445B; D4:($94, $B7, $74, $FB, $CE, $2E, $A1, $1A));

  CGID_ShellServiceObject: TGUID = (
    D1:$000214D2; D2:$; D3:$; D4:($C0,0,0,0,0,0,0,$46));

  CGID_ExplorerBarDoc: TGUID = (
    D1:$000214D3; D2:$; D3:$; D4:($C0,0,0,0,0,0,0,$46));

  CGID_DefView : TGUID = (
    D1:$4af07f10; D2:$d231; D3:$11d0; D4:($b9, $42, $00, $a0, $c9, $03, $12, $e1));

  CGID_ShortCut: TGUID = (
    D1:$93A68750; D2:$951A; D3:$11D1; D4:($94, $6F, $0, $0, $0, $0, $0, $0));

  CGID_MENUDESKBAR : TGUID = (
    D1:$5c9f0a12; D2:$959e; D3:$11d0; D4:($a3, $a4, $00, $a0, $c9, $08, $26, $36));

  IID_ICopyHookA: TGUID = (
    D1:$000214EF; D2:$0000; D3:$0000; D4:($C0,00,00,00,00,00,00,$46));

  IID_ICopyHookW: TGUID = (
    D1:$000214FC; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));

  IID_IRemoteComputer: TGUID = (
    D1:$000214FE; D2:$0000; D3:$0000; D4:($C0,0,0,0,0,0,0,$46));

  IID_IQueryInfo: TGUID = (
    D1:$00021500; D2:$0000; D3:$0000; D4:($C0,0,0,0,0,0,0,$46));

  IID_IBriefcaseStg: TGUID = (
    D1:$8BCE1FA1; D2:$0921; D3:$101B; D4:($B1, $FF, $00, $DD, $01, $0C, $CC, $48));

  IID_IShellLinkDataList: TGUID = (
    D1:$45e2b4ae; D2:$b1c3; D3:$11d0; D4:($b9, $2f, $0, $a0, $c9, $3, $12, $e1));

  IID_IResolveShellLink: TGUID = (
    D1:$5cd52983; D2:$9449; D3:$11d2; D4:($96, $3a, $00, $c0, $4f, $79, $ad, $f0));

  IID_IURLSearchHook: TGUID = (
    D1:$AC60F6A0; D2:$0FD9; D3:$11D0; D4:($99, $CB, $00, $C0, $4F, $D6, $44, $97));

  IID_ISearchContext: TGUID = (
    D1:$09F656A2; D2:$41AF; D3:$480C; D4:($88, $F7, $16, $CC, $0D, $16, $46, $15));

  IID_IURLSearchHook2: TGUID = (
    D1:$5ee44da4; D2:$6d32; D3:$46e3; D4:($86, $bc, $07, $54, $0d, $ed, $d0, $e0));

  IID_IDelegateFolder: TGUID = (
    D1:$ADD8BA80; D2:$002B; D3:$11D0; D4:($8F, $0F, $00, $C0, $4F, $D7, $D0, $62));

  IID_IDefViewID: TGUID = (
    D1:$985F64F0; D2:$D410; D3:$4E02; D4:($BE, $22, $DA, $07, $F2, $B5, $C5, $E1));

  IID_IInputObject: TGUID = (
    D1:$68284faa; D2:$6a48; D3:$11d0; D4:($8c, $78, $0, $c0, $4f, $d9, $18, $b4));

  IID_IInputObjectSite: TGUID = (
    D1:$f1db8392; D2:$7331; D3:$11d0; D4:($8c, $99, $0, $a0, $c9, $2d, $bf, $e8));

  IID_IDockingWindowSite: TGUID = (
    D1:$2a342fc2; D2:$7b26; D3:$11d0; D4:($8c, $a9, $0, $a0, $c9, $2d, $bf, $e8));

  IID_IDockingWindowFrame: TGUID = (
    D1:$47d2657a; D2:$7b27; D3:$11d0; D4:($8c, $a9, $0, $a0, $c9, $2d, $bf, $e8));

  IID_IShellIconOverlay: TGUID = (
    D1:$7D688A70; D2:$C613; D3:$11D0; D4:($99, $9B, $00, $C0, $4F, $D6, $55, $E1));

  IID_IShellIconOverlayIdentifier: TGUID = (
    D1:$0C6C4200; D2:$C589; D3:$11D0; D4:($99, $9A, $00, $C0, $4F, $D6, $55, $E1));

  IID_ICommDlgBrowser2: TGUID = (
    D1:$10339516; D2:$2894; D3:$11d2; D4:($90, $39, $0, $c0, $4f, $8e, $eb, $3e));

  IID_IShellFolderViewCB: TGUID = (
    D1:$2047E320; D2:$F2A9; D3:$11CE; D4:($AE, $65, $08, $00, $2B, $2E, $12, $62));

  IID_IPersistFolder2: TGUID = (
    D1:$1AC3D9F0; D2:$175C; D3:$11d1; D4:($95,$BE,$00,$60,$97,$97,$EA,$4F));

  IID_IPersistFolder3: TGUID = (
    D1:$CEF04FDF; D2:$FE72; D3:$11D2; D4:($87, $A5, $0, $C0, $4F, $68, $37, $CF));

  IID_IShellIconOverlayManager: TGUID = (
    D1:$F10B5E34; D2:$DD3B; D3:$42A7; D4:($AA, $7D, $2F, $4E, $C5, $4B, $B0, $9B));

  IID_IRunnableTask: TGUID = (
    D1:$85788D00; D2:$6807; D3:$11D0; D4:($B8, $10, $0, $C0, $4F, $D7, $6, $EC));

  IID_IThumbnailCapture: TGUID = (
    D1:$4EA39266; D2:$7211; D3:$409F; D4:($B6, $22, $F6, $3D, $BD, $16, $C5, $33));

  IID_IShellImageStore : TGUID =(
    D1:$48C8118C; D2:$B924; D3:$11D1; D4:($98, $D5, $00, $C0, $4F, $B6, $87, $DA));

  IID_IEnumShellImageStore : TGUID =(
    D1:$6DFD582B; D2:$92E3; D3:$11D1; D4:($98, $A3, $00, $C0, $4F, $B6, $87, $DA));

  IID_IContextMenu3: TGUID = (
    D1:$BCFCE0A0; D2:$EC17; D3:$11D0; D4:($8D, $10, $0, $A0, $C9, $F, $27, $19));

  IID_DefView : TGUID = (
    D1:$6D12FE80; D2:$7911; D3:$11CF; D4:($95, $34, $00, $00, $C0, $5B, $AE, $0B));

  IID_IShellFolderBand : TGUID = (
    D1:$7FE80CC8; D2:$C247; D3:$11D0; D4:($B9, $3A, $00, $A0, $C9, $03, $12, $E1));

  IID_IDefViewFrame : TGUID = (
    D1:$710EB7A0; D2:$45ED; D3:$11D0; D4:($92, $4A, $00, $20, $AF, $C7, $AC, $4D));

  IID_IDiscardableBrowserProperty: TGUID = (
    D1:$49C3DE7C; D2:$D329; D3:$11D0; D4:($AB, $73, $00, $C0, $4F, $C3, $3E, $80));

  IID_IShellChangeNotify: TGUID = (
    D1:$D82BE2B1; D2:$5764; D3:$11D0; D4:($A9, $6E, $00, $C0, $4F, $D7, $05, $A2));

  IID_IUniformResourceLocatorA : TGUID = (
    D1:$FBF23B80; D2:$E3F0; D3:$101B; D4:($84, $88, $00, $AA, $00, $3E, $56, $F8));

  IID_IUniformResourceLocator : TGUID = (
    D1:$FBF23B80; D2:$E3F0; D3:$101B; D4:($84, $88, $00, $AA, $00, $3E, $56, $F8));

  IID_IUniformResourceLocatorW : TGUID = (
    D1:$CABB0DA0; D2:$DA57; D3:$11CF; D4:($99, $74, $00, $20, $AF, $D7, $97, $62));

  IID_IAutoCompList: TGUID = (
    D1:$00BB2760; D2:$6A77; D3:$11D0; D4:($A5, $35, $00, $C0, $4F, $D7, $D0, $62));

  IID_IObjMgr: TGUID = (
    D1:$00BB2761; D2:$6A77; D3:$11D0; D4:($A5, $35, $00, $C0, $4F, $D7, $D0, $62));

  IID_IACList: TGUID = (
    D1:$77A130B0; D2:$94FD; D3:$11D0; D4:($A5, $44, $00, $C0, $4F, $D7, $d0, $62));

  IID_IACList2: TGUID = (
    D1:$470141A0; D2:$5186; D3:$11D2; D4:($BB, $B6, $00, $60, $97, $7B, $46, $4C));

  IID_ICurrentWorkingDirectory: TGUID = (
    D1:$91956D21; D2:$9276; D3:$11D1; D4:($92, $1A, $00, $60, $97, $DF, $5B, $D4));

  IID_IProgressDialog: TGUID = (
    D1:$EBBC7C04; D2:$315E; D3:$11D2; D4:($B6, $2F, $0, $60, $97, $DF, $5B, $D4));

  IID_IActiveDesktop: TGUID = (
    D1:$F490EB00; D2:$1240; D3:$11D1; D4:($98, $88, $00, $60, $97, $DE, $AC, $F9));

  IID_IActiveDesktopP : TGUID = (
    D1:$52502EE0; D2:$EC80; D3:$11D0; D4:($89, $AB, $00, $C0, $4F, $C2, $97, $2D));

  IID_IADesktopP2 : TGUID = (
    D1:$B22754E2; D2:$4574; D3:$11D1; D4:($98, $88, $00, $60, $97, $DE, $AC, $F9));

  IID_ISynchronizedCallBack: TGUID = (
    D1:$74C26041; D2:$70D1; D3:$11D1; D4:($B7, $5A, $0, $A0, $C9, $5, $64, $FE));

  IID_IShellDetails3: TGUID = (
    D1:$D2A105C0; D2:$87D5; D3:$11D1; D4:($83, $91, $0, $0, $F8, $4, $61, $CF));

  IID_IQueryAssociations: TGUID = (
    D1:$C46CA590; D2:$3C3F; D3:$11D2; D4:($BE, $E6, $00, $00, $F8, $05, $CA, $57));

  IID_IColumnProvider: TGUID = (
    D1:$E8025004; D2:$1C42; D3:$11D2; D4:($BE, $2C, $0, $A0, $C9, $A8, $3D, $A1));

  IID_INamedPropertyBag: TGUID = (
    D1:$FB700430; D2:$952C; D3:$11D1; D4:($94, $6F, $0, $0, $0, $0, $0, $0));

  IID_IMountedVolume: TGUID = (
    D1:$12518492; D2:$00B2; D3:$11D2; D4:($9F, $A5, $9E, $34, $20, $52, $41, $53));

  IID_IDropTargetHelper: TGUID = (
    D1:$4657278B; D2:$411B; D3:$11D2; D4:($83, $9A, $0, $C0, $4F, $D9, $18, $D0));

  IID_IDragSourceHelper: TGUID = (
    D1:$DE5BF786; D2:$477A; D3:$11D2; D4:($83, $9D, $0, $C0, $4F, $D9, $18, $D0));

  IID_IFileSystemBindData: TGUID = (
    D1:$1e18d10; D2:$4d8b; D3:$11d2; D4:($85, $5d, $0, $60, $8, $5, $93, $67));

  IID_IDocViewSite : TGUID = (
    D1:$87D605E0; D2:$C511; D3:$11CF; D4:($89, $A9, $00, $A0, $C9, $05, $41, $29));

  IID_CDefView : TGUID = (
    D1:$4434FF80; D2:$EF4C; D3:$11CE; D4:($AE, $65, $08, $00, $2B, $2E, $12, $62));

  IID_IBanneredBar : TGUID = (
    D1:$596a9a94; D2:$013e; D3:$11d1; D4:($8d, $34, $00, $a0, $c9, $0f, $27, $19));

  IID_IPersistIDList: TGUID = (
    D1:$1079acfc; D2:$29bd; D3:$11d3; D4:($8e,$0d,$00,$c0,$4f,$68,$37,$d5));

{$IFDEF VER130}
  IID_IShellFolder2: TGUID = (
    D1:$93F2F68C; D2:$1D1B; D3:$11D3; D4:($A3,$0E,$00,$C0,$4F,$79,$AB,$D1));

  IID_IEnumExtraSearch: TGUID = (
    D1:$E700BE1; D2: $9DB6; D3:$11D1; D4:($A1,$CE,$00,$C0,$4F,$D7,$5D,$13));

  IID_IShellDetails: TGUID = (
    D1:$000214EC; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
{$ENDIF}

  SID_IDelayedRelease    = '{000214ED-0000-0000-C000-000000000046}';
  SID_LinkSite           = '{000214F9-0000-0000-C000-000000000046}';
  SID_IRemoteComputer    = '{000214FE-0000-0000-C000-000000000046}';
  SID_IBriefcaseStg      = '{8BCE1FA1-0921-101B-B1FF-00DD010CCC48}';
  SID_IShellLinkDataList = '{45E2B4AE-B1C3-11D0-B92F-00A0C90312E1}';
  SID_IResolveShellLink  = '{5CD52983-9449-11D2-963A-00C04F79ADF0}';
  SID_ISearchContext     = '{09F656A2-41AF-480C-88F7-16CC0D164615}';
  SID_IURLSearchHook2    = '{5EE44DA4-6D32-46E3-86BC-07540DEDD0E0}';
  SID_IDelegateFolder    = '{ADD8BA80-002B-11D0-8F0F-00C04FD7D062}';
  SID_IDefViewID         = '{985F64F0-D410-4E02-BE22-DA07F2B5C5E1}';
  SID_ICommDlgBrowser2   = '{10339516-2894-11d2-9039-00C04F8EEB3E}';
  SID_IShellFolderViewCB = '{2047E320-F2A9-11CE-AE65-08002B2E1262}';
  SID_IPersistFolder3    = '{CEF04FDF-FE72-11D2-87A5-00C04F6837CF}';
  SID_IShellIconOverlayManager = '{F10B5E34-DD3B-42A7-AA7D-2F4EC54BB09B}';
  SID_IRunnableTask      = '{85788D00-6807-11D0-B810-00C04FD706EC}';
  SID_IThumbnailCapture  = '{4EA39266-7211-409F-B622-F63DBD16C533}';
  SID_IShellImageStore   = '{48C8118C-B924-11D1-98D5-00C04FB687DA}';
  SID_IEnumShellImageStore = '{6DFD582B-92E3-11D1-98A3-00C04FB687DA}';
  SID_DefView            = '{6D12FE80-7911-11CF-9534-0000C05BAE0B}';
  SID_IShellFolderBand   = '{7FE80CC8-C247-11D0-B93A-00A0C90312E1}';
  SID_IDefViewFrame      = '{710EB7A0-45ED-11D0-924A-0020AFC7AC4D}';
  SID_SShellBrowser      = '{000214E2-0000-0000-C000-000000000046}';
  SID_SShellDesktop      = '{00021400-0000-0000-C000-000000000046}';
  SID_IDiscardableBrowserProperty = '{49C3DE7C-D329-11D0-AB73-00C04FC33E80}';
  SID_IUniformResourceLocatorA = '{FBF23B80-E3F0-101B-8488-00AA003E56F8}';
  SID_IUniformResourceLocator = SID_IUniformResourceLocatorA;
  SID_IUniformResourceLocatorW = '{CABB0DA0-DA57-11CF-9974-0020AFD79762}';
  SID_CUrlHistory        = '{3C374A40-BAE4-11CF-BF7D-00AA006946EE}';
  SID_SInternetExplorer  = '{0002DF05-0000-0000-C000-000000000046}';
  SID_SWebBrowserApp     = '{0002DF05-0000-0000-C000-000000000046}';
  SID_IAutoCompList      = '{00BB2760-6A77-11D0-A535-00C04FD7D062}';
  SID_IObjMgr            = '{00BB2761-6A77-11D0-A535-00C04FD7D062}';
  SID_IACList            = '{77A130B0-94FD-11D0-A544-00C04FD7D062}';
  SID_IACList2           = '{470141A0-5186-11D2-BBB6-0060977B464C}';
  SID_ICurrentWorkingDirectory = '{91956D21-9276-11D1-921A-006097DF5BD4}';
  SID_IProgressDialog    = '{EBBC7C04-315E-11D2-B62F-006097DF5BD4}';
  SID_SProgressUI        = '{F8383852-FCD3-11D1-A6B9-006097DF5BD4}';
  SID_STopLevelBrowser   = '{4C96BE40-915C-11CF-99D3-00AA004AE837}';
  SID_IActiveDesktopP    = '{52502EE0-EC80-11D0-89AB-00C04FC2972D}';
  SID_IADesktopP2        = '{B22754E2-4574-11D1-9888-006097DEACF9}';
  SID_ISynchronizedCallBack = '{74C26041-70D1-11D1-B75A-00A0C90564FE}';
  SID_IShellDetails3     = '{D2A105C0-87D5-11D1-8391-0000F80461CF}';
  SID_IQueryAssociations = '{C46CA590-3C3F-11D2-BEE6-0000F805CA57}';
  SID_IColumnProvider    = '{E8025004-1C42-11D2-BE2C-00A0C9A83DA1}';
  SID_INamedPropertyBag  = '{FB700430-952C-11D1-946F-000000000000}';
  SID_IMountedVolume     = '{12518492-00B2-11D2-9FA5-9E3420524153}';
  SID_IDropTargetHelper  = '{4657278B-411B-11D2-839A-00C04FD918D0}';
  SID_IDragSourceHelper  = '{DE5BF786-477A-11D2-839D-00C04FD918D0}';
  SID_IFileSystemBindData ='{01E18D10-4D8B-11D2-855D-006008059367}';
  SID_STopWindow         = '{49E1B500-4636-11D3-97F7-00C04F45D0B3}';
  SID_SGetViewFromViewDual = '{889A935D-971E-4B12-B90C-24DFC9E1E5E8}';
  SID_CtxQueryAssociations = '{FAADFC40-B777-4B69-AA81-77035EF0E6E8}';
  SID_IDocViewSite       = '{87D605E0-C511-11CF-89A9-00A0C9054129}';
  SID_CDefView           = '{4434FF80-EF4C-11CE-AE65-08002B2E1262}';
  SID_SMenuBandChild     = '{ED9CC020-08B9-11D1-9823-00C04FD91972}';
  SID_SMenuBandParent    = '{8C278EEC-3EAB-11D1-8CB0-00C04FD918D0}';
  SID_SMenuPopup         = '{D1E7AFEB-6A2E-11d0-8C78-00C04FD918B4}';
  SID_SMenuBandBottomSelected = '{165EBAF4-6D51-11D2-83AD-00C04FD918D0}';
  SID_SMenuBandBottom    = '{743CA664-0DEB-11D1-9825-00C04FD91972}';
  SID_MenuShellFolder    = '{A6C17EB4-2D65-11D2-838F-00C04FD918D0}';
  SID_SMenuBandTop       = '{9493A810-EC38-11D0-BC46-00AA006CE2F5}';
  SID_IBanneredBar       = '{596A9A94-013E-11D1-8D34-00A0C90F2719}';
  SID_SCommDlgBrowser    = '{80F30233-B7DF-11D2-A33B-006097DF5BD4}';
  SID_IPersistIDList     = '{1079acfc-29bd-11d3-8e0d-00c04f6837d5}';
{$IFDEF VER130}
  SID_IShellFolder2      = '{93F2F68C-1D1B-11D3-A30E-00C04F79ABD1}';
  SID_IEnumExtraSearch   = '{0E700BE1-9DB6-11D1-A1CE-00C04FD75D13}';
  SID_IShellDetails      = '{000214EC-0000-0000-C000-000000000046}';
{$ENDIF}

  IDefViewID = IUnknown;


  DISPID_BEFORENAVIGATE    = 100;   // this is sent before navigation to give a chance to abort
  DISPID_NAVIGATECOMPLETE  = 101;   // in async, this is sent when we have enough to show
  DISPID_STATUSTEXTCHANGE  = 102;
  DISPID_QUIT              = 103;
  DISPID_DOWNLOADCOMPLETE  = 104;
  DISPID_COMMANDSTATECHANGE= 105;
  DISPID_DOWNLOADBEGIN     = 106;
  DISPID_NEWWINDOW         = 107;   // sent when a new window should be created
  DISPID_PROGRESSCHANGE    = 108;   // sent when download progress is updated
  DISPID_WINDOWMOVE        = 109;   // sent when main window has been moved
  DISPID_WINDOWRESIZE      = 110;   // sent when main window has been sized
  DISPID_WINDOWACTIVATE    = 111;   // sent when main window has been activated
  DISPID_PROPERTYCHANGE    = 112;   // sent when the PutProperty method is called
  DISPID_TITLECHANGE       = 113;   // sent when the document title changes
  DISPID_TITLEICONCHANGE   = 114;   // sent when the top level window icon may have changed.

  DISPID_FRAMEBEFORENAVIGATE   = 200;
  DISPID_FRAMENAVIGATECOMPLETE = 201;
  DISPID_FRAMENEWWINDOW        = 204;

  DISPID_BEFORENAVIGATE2     = 250;   // hyperlink clicked on
  DISPID_NEWWINDOW2          = 251;
  DISPID_NAVIGATECOMPLETE2   = 252;   // UIActivate new document
  DISPID_ONQUIT              = 253;
  DISPID_ONVISIBLE           = 254;   // sent when the window goes visible/hidden
  DISPID_ONTOOLBAR           = 255;   // sent when the toolbar should be shown/hidden
  DISPID_ONMENUBAR           = 256;   // sent when the menubar should be shown/hidden
  DISPID_ONSTATUSBAR         = 257;   // sent when the statusbar should be shown/hidden
  DISPID_ONFULLSCREEN        = 258;   // sent when kiosk mode should be on/off
  DISPID_DOCUMENTCOMPLETE    = 259;   // new document goes ReadyState_Complete
  DISPID_ONTHEATERMODE       = 260;   // sent when theater mode should be on/off
  DISPID_ONADDRESSBAR        = 261;   // sent when the address bar should be shown/hidden
  DISPID_WINDOWSETRESIZABLE  = 262;   // sent to set the style of the host window frame
  DISPID_WINDOWCLOSING       = 263;   // sent before script window.close closes the window
  DISPID_WINDOWSETLEFT       = 264;   // sent when the put_left method is called on the WebOC
  DISPID_WINDOWSETTOP        = 265;   // sent when the put_top method is called on the WebOC
  DISPID_WINDOWSETWIDTH      = 266;   // sent when the put_width method is called on the WebOC
  DISPID_WINDOWSETHEIGHT     = 267;   // sent when the put_height method is called on the WebOC
  DISPID_CLIENTTOHOSTWINDOW  = 268;   // sent during window.open to request conversion of dimensions
  DISPID_SETSECURELOCKICON   = 269;   // sent to suggest the appropriate security icon to show
  DISPID_FILEDOWNLOAD        = 270;   // Fired to indicate the File Download dialog is opening
  DISPID_NAVIGATEERROR       = 271;   // Fired to indicate the a binding error has occured
  DISPID_PRIVACYIMPACTEDSTATECHANGE =  272;  // Fired when the user's browsing experience is impacted

  // Printing events
  DISPID_PRINTTEMPLATEINSTANTIATION =  225;   // Fired to indicate that a print template is instantiated
  DISPID_PRINTTEMPLATETEARDOWN      =  226;   // Fired to indicate that a print templete is completely gone
  DISPID_UPDATEPAGESTATUS           =  227;   // Fired to indicate that the spooling status has changed

  // define the events for the shell wiwndow list
  DISPID_WINDOWREGISTERED    = 200;     // Window registered
  DISPID_WINDOWREVOKED       = 201;     // Window Revoked

  DISPID_RESETFIRSTBOOTMODE     =  1;
  DISPID_RESETSAFEMODE          =  2;
  DISPID_REFRESHOFFLINEDESKTOP  =  3;
  DISPID_ADDFAVORITE            =  4;
  DISPID_ADDCHANNEL             =  5;
  DISPID_ADDDESKTOPCOMPONENT    =  6;
  DISPID_ISSUBSCRIBED           =  7;
  DISPID_NAVIGATEANDFIND        =  8;
  DISPID_IMPORTEXPORTFAVORITES  =  9;
  DISPID_AUTOCOMPLETESAVEFORM   =  10;
  DISPID_AUTOSCAN               =  11;
  DISPID_AUTOCOMPLETEATTACH     =  12;
  DISPID_SHOWBROWSERUI          =  13;
  DISPID_SHELLUIHELPERLAST      =  13;

  DISPID_ADVANCEERROR           = 10;
  DISPID_RETREATERROR           = 11;
  DISPID_CANADVANCEERROR        = 12;
  DISPID_CANRETREATERROR        = 13;
  DISPID_GETERRORLINE           = 14;
  DISPID_GETERRORCHAR           = 15;
  DISPID_GETERRORCODE           = 16;
  DISPID_GETERRORMSG            = 17;
  DISPID_GETERRORURL            = 18;
  DISPID_GETDETAILSSTATE        = 19;
  DISPID_SETDETAILSSTATE        = 20;
  DISPID_GETPERERRSTATE         = 21;
  DISPID_SETPERERRSTATE         = 22;
  DISPID_GETALWAYSSHOWLOCKSTATE = 23;

  // Dispatch IDS for ShellFavoritesNameSpace Dispatch Events.
  //
  DISPID_FAVSELECTIONCHANGE     =  1;
  DISPID_SELECTIONCHANGE        =  2;
  DISPID_DOUBLECLICK            =  3;
  DISPID_INITIALIZED            =  4;

  DISPID_MOVESELECTIONUP        =  1;
  DISPID_MOVESELECTIONDOWN      =  2;
  DISPID_RESETSORT              =  3;
  DISPID_NEWFOLDER              =  4;
  DISPID_SYNCHRONIZE            =  5;
  DISPID_IMPORT                 =  6;
  DISPID_EXPORT                 =  7;
  DISPID_INVOKECONTEXTMENU      =  8;
  DISPID_MOVESELECTIONTO        =  9;
  DISPID_SUBSCRIPTIONSENABLED   =  10;
  DISPID_CREATESUBSCRIPTION     =  11;
  DISPID_DELETESUBSCRIPTION     =  12;
  DISPID_SETROOT                =  13;
  DISPID_ENUMOPTIONS            =  14;
  DISPID_SELECTEDITEM           =  15;
  DISPID_ROOT                   =  16;
  DISPID_DEPTH                  =  17;
  DISPID_MODE                   =  18;
  DISPID_FLAGS                  =  19;
  DISPID_TVFLAGS                =  20;
  DISPID_NSCOLUMNS              =  21;
  DISPID_COUNTVIEWTYPES         =  22;
  DISPID_SETVIEWTYPE            =  23;
  DISPID_SELECTEDITEMS          =  24;
  DISPID_EXPAND                 =  25;
  DISPID_UNSELECTALL            =  26;

  // Rest of this block is in Shell32_tlb.pas equivalent to shldisp.h

  PSGUID_INTERNETSHORTCUT: TGUID = (
    D1:$000214A0; D2:$; D3:$; D4:($C0,0,0,0,0,0,0,$46));

  PSGUID_INTERNETSITE: TGUID = (
    D1:$000214A1; D2:$; D3:$; D4:($C0,0,0,0,0,0,0,$46));

  FMTID_INTERNETSITE: TGUID = (
    D1:$000214A1; D2:$; D3:$; D4:($C0,0,0,0,0,0,0,$46));

  PSGUID_SHELLDETAILS: TGUID = (
    D1:$28636aa6; D2:$953d; D3:$11d2; D4:($b5, $d6, $0, $c0, $4f, $d9, $18, $d0));

  FMTID_ShellDetails: TGUID = (
    D1:$28636aa6; D2:$953d; D3:$11d2; D4:($b5, $d6, $0, $c0, $4f, $d9, $18, $d0));

  PID_FINDDATA        = 0;
  PID_NETRESOURCE     = 1;
  PID_DESCRIPTIONID   = 2;
  PID_WHICHFOLDER     = 3;
  PID_NETWORKLOCATION = 4;
  PID_COMPUTERNAME    = 5;

  PSGUID_STORAGE : TGUID = (
    D1:$b725f130; D2:$47ef; D3:$101a; D4:($a5, $f1, $02, $60, $8c, $9e, $eb, $ac));

  FMTID_Storage: TGUID = (
    D1:$b725f130; D2:$47ef; D3:$101a; D4:($a5, $f1, $02, $60, $8c, $9e, $eb, $ac));

  // Image properties
  PSGUID_IMAGEPROPERTIES: TGUID = (
    D1:$14b81da1; D2:$0135; D3:$4d31; D4:($96, $d9, $6c, $bf, $c9, $67, $1a, $99));

  FMTID_ImageProperties: TGUID = (
    D1:$14b81da1; D2:$0135; D3:$4d31; D4:($96, $d9, $6c, $bf, $c9, $67, $1a, $99));

  // The GUIDs used to identify shell item attributes (columns). See IShellFolder2::GetDetailsEx implementations...
  PSGUID_DISPLACED: TGUID = (
    D1:$9b174b33; D2:$40ff; D3:$11d2; D4:($a2, $7e, $0, $c0, $4f, $c3, $8, $71));

  FMTID_Displaced: TGUID = (
    D1:$9b174b33; D2:$40ff; D3:$11d2; D4:($a2, $7e, $0, $c0, $4f, $c3, $8, $71));

  PID_DISPLACED_FROM = 2;
  PID_DISPLACED_DATE = 3;

  PSGUID_BRIEFCASE: TGUID = (
    D1:$328d8b21; D2:$7729; D3:$4bfc; D4:($95, $4c, $90, $2b, $32, $9d, $56, $b0));

  FMTID_Briefcase: TGUID = (
    D1:$328d8b21; D2:$7729; D3:$4bfc; D4:($95, $4c, $90, $2b, $32, $9d, $56, $b0));

  PID_SYNC_COPY_IN = 2;

  PSGUID_MISC: TGUID = (
    D1:$9b174b34; D2:$40ff; D3:$11d2; D4:($a2, $7e, $0, $c0, $4f, $c3, $8, $71));

  FMTID_Misc: TGUID = (
    D1:$9b174b34; D2:$40ff; D3:$11d2; D4:($a2, $7e, $0, $c0, $4f, $c3, $8, $71));

  PID_MISC_STATUS      = 2;
  PID_MISC_ACCESSCOUNT = 3;
  PID_MISC_OWNER       = 4;
  PID_HTMLINFOTIPFILE  = 5;
  PID_MISC_PICS        = 6;

  PSGUID_WEBVIEW: TGUID = (
    D1:$f2275480; D2:$f782; D3:$4291; D4:($bd, $94, $f1, $36, $93, $51, $3a, $ec));

  FMTID_WebView: TGUID = (
    D1:$f2275480; D2:$f782; D3:$4291; D4:($bd, $94, $f1, $36, $93, $51, $3a, $ec));

  PID_DISPLAY_PROPERTIES = 0;
  PID_INTROTEXT          = 1;

  PSGUID_MUSIC: TGUID = (
    D1:$56a3372e; D2:$ce9c; D3:$11d2; D4:($9f, $e, $0, $60, $97, $c6, $86, $f6));

  FMTID_MUSIC: TGUID = (
    D1:$56a3372e; D2:$ce9c; D3:$11d2; D4:($9f, $e, $0, $60, $97, $c6, $86, $f6));

  PIDSI_ARTIST    = 2;
  PIDSI_SONGTITLE = 3;
  PIDSI_ALBUM     = 4;
  PIDSI_YEAR      = 5;
  PIDSI_COMMENT   = 6;
  PIDSI_TRACK     = 7;
  PIDSI_GENRE     = 11;
  PIDSI_LYRICS    = 12;

  PSGUID_DRM: TGUID = (
    D1:$aeac19e4; D2:$89ae; D3:$4508; D4:($b9, $b7, $bb, $86, $7a, $be, $e2, $ed));

  FMTID_DRM: TGUID = (
    D1:$aeac19e4; D2:$89ae; D3:$4508; D4:($b9, $b7, $bb, $86, $7a, $be, $e2, $ed));

  PIDDRSI_PROTECTED   = 2;
  PIDDRSI_DESCRIPTION = 3;
  PIDDRSI_PLAYCOUNT   = 4;
  PIDDRSI_PLAYSTARTS  = 5;
  PIDDRSI_PLAYEXPIRES = 6;

  PSGUID_VIDEO : TGUID = (
    D1:$64440491; D2:$4c8b; D3:$11d1; D4:($8b, $70, $08, $00, $36, $b1, $1a, $03));

  //FMTID_VideoSummaryInformation property identifiers
  PIDVSI_STREAM_NAME   = $00000002; // "StreamName", VT_LPWSTR
  PIDVSI_FRAME_WIDTH   = $00000003; // "FrameWidth", VT_UI4
  PIDVSI_FRAME_HEIGHT  = $00000004; // "FrameHeight", VT_UI4
  PIDVSI_TIMELENGTH    = $00000007; // "TimeLength", VT_UI4, milliseconds
  PIDVSI_FRAME_COUNT   = $00000005; // "FrameCount". VT_UI4
  PIDVSI_FRAME_RATE    = $00000006; // "FrameRate", VT_UI4, frames/millisecond
  PIDVSI_DATA_RATE     = $00000008; // "DataRate", VT_UI4, bytes/second
  PIDVSI_SAMPLE_SIZE   = $00000009; // "SampleSize", VT_UI4
  PIDVSI_COMPRESSION   = $0000000A; // "Compression", VT_LPWSTR
  PIDVSI_STREAM_NUMBER = $0000000B; // "StreamNumber", VT_UI2

  PSGUID_AUDIO : TGUID = (
    D1:$64440490; D2:$4c8b; D3:$11d1; D4:($8b, $70, $08, $00, $36, $b1, $1a, $03));

  // FMTID_AudioSummaryInformation property identifiers
  PIDASI_FORMAT        = $00000002; // VT_BSTR
  PIDASI_TIMELENGTH    = $00000003; // VT_UI4, milliseconds
  PIDASI_AVG_DATA_RATE = $00000004; // VT_UI4,  Hz
  PIDASI_SAMPLE_RATE   = $00000005; // VT_UI4,  bits
  PIDASI_SAMPLE_SIZE   = $00000006; // VT_UI4,  bits
  PIDASI_CHANNEL_COUNT = $00000007; // VT_UI4
  PIDASI_STREAM_NUMBER = $00000008; // VT_UI2
  PIDASI_STREAM_NAME   = $00000009; // VT_LPWSTR
  PIDASI_COMPRESSION   = $0000000A; // VT_LPWSTR

  PSGUID_CONTROLPANEL : TGUID = (
    D1:$305ca226; D2:$d286; D3:$468e; D4:($b8, $48, $2b, $2e, $8e, $69, $7b, $74));

  PID_CONTROLPANEL_CATEGORY = 2;

  PSGUID_VOLUME: TGUID = (
    D1:$9b174b35; D2:$40ff; D3:$11d2; D4:($a2, $7e, $0, $c0, $4f, $c3, $8, $71));

  FMTID_Volume: TGUID = (
    D1:$9b174b35; D2:$40ff; D3:$11d2; D4:($a2, $7e, $0, $c0, $4f, $c3, $8, $71));

  PID_VOLUME_FREE       = 2;
  PID_VOLUME_CAPACITY   = 3;
  PID_VOLUME_FILESYSTEM = 4;

  PSGUID_SHARE : TGUID = (
    D1:$d8c3986f; D2:$813b; D3:$449c; D4:($84, $5d, $87, $b9, $5d, $67, $4a, $de));

  PID_SHARE_CSC_STATUS = 2;

  PSGUID_LINK : TGUID = (
    D1:$b9b4b3fc; D2:$2b51; D3:$4a42; D4:($b5, $d8, $32, $41, $46, $af, $cf, $25));

  PID_LINK_TARGET = 2;

  PSGUID_QUERY_D: TGUID = (
    D1:$49691c90; D2:$7e17; D3:$101a; D4:($a9,$1c,$08,$00,$2b,$2e,$cd,$a9));

  FMTID_Query: TGUID = (
    D1:$49691c90; D2:$7e17; D3:$101a; D4:($a9,$1c,$08,$00,$2b,$2e,$cd,$a9));

  PID_QUERY_RANK = 2;

  //FMTID_SummaryInformation, see OLE docs for PID_ values for these
  FMTID_SUMMARYINFORMATION : TGUID = (
    D1:$f29f85e0; D2:$4ff9; D3:$1068; D4:($ab, $91, $08, $00, $2b, $27, $b3, $d9));

  PSGUID_SUMMARYINFORMATION : TGUID = (
    D1:$f29f85e0; D2:$4ff9; D3:$1068; D4:($ab, $91, $08, $00, $2b, $27, $b3, $d9));

  // FMTID_DocumentSummaryInformation, see OLE docs on the PID_ values for this
  PSGUID_DOCUMENTSUMMARYINFORMATION : TGUID = (
    D1:$d5cdd502; D2:$2e9c; D3:$101b; D4:($93, $97, $08, $00, $2b, $2c, $f9, $ae));

  // FMTID_MediaFileSummaryInformation, see propidl.h PID_ values for this
  PSGUID_MEDIAFILESUMMARYINFORMATION : TGUID = (
    D1:$64440492; D2:$4c8b; D3:$11d1; D4:($8b, $70, $08, $00, $36, $b1, $1a, $03));

  // FMTID_ImageSummaryInformation,, see propidl.h PID_ values for this
  PSGUID_IMAGESUMMARYINFORMATION : TGUID = (
    D1:$6444048f; D2:$4c8b; D3:$11d1; D4:($8b, $70, $08, $00, $36, $b1, $1a, $03));

type
  IPersistIDList = interface(IPersist)
    [SID_IPersistIDList]
    // sets or gets a fully qualifed idlist for an object
    function SetIDList(pidl : PItemIdList) : HResult; stdcall;
    function GetIDList(var pidl : PItemIdList) : HResult; stdcall;
  end;

type
  SHGDNF = DWORD;

const
  SHCONTF_INIT_ON_FIRST_NEXT  = $0100;   // allow EnumObject() to return before validating enum
  SHCONTF_NETPRINTERSRCH      = $0200;   // hint that client is looking for printers
  SHCONTF_SHAREABLE           = $0400;   // hint that client is looking sharable resources (remote shares)
  SHCONTF_STORAGE             = $0800;   // include all items with accessible storage and their ancestors

type
  SHCONTF = DWORD;

const
  SHCIDS_ALLFIELDS = $80000000;
  SHCIDS_CANONICALONLY = $10000000;
  SHCIDS_BITMASK = $FFFF0000;
  SHCIDS_COLUMNMASK = $0000FFFF;

  SFGAO_STORAGE           = $00000008;       { supports BindToObject(IID_IStorage)}
  SFGAO_ENCRYPTED         = $00002000;       { object is encrypted (use alt color)}
  SFGAO_ISSLOW            = $00004000;       { 'slow' object}
  SFGAO_GHOSTED           = $00008000;       { ghosted icon} // - nb different to ShlObj
  SFGAO_DISPLAYATTRMASK   = $000FC000;        // - nb different to ShlObj
  SFGAO_CANMONIKER        = $00400000;       // defunct
  SFGAO_HASSTORAGE        = $00400000;       // defunct
  SFGAO_STREAM            = $00400000;       // supports BindToObject(IID_IStream)
  SFGAO_STORAGEANCESTOR   = $00800000;       // may contain children with SFGAO_STORAGE or SFGAO_STREAM
  SFGAO_STORAGECAPMASK    = $70C50008;       // for determining storage capabilities, ie for open/save semantics

  STR_SKIP_BINDING_CLSID  = 'Skip Binding CLSID';
  STR_PARSE_PREFER_FOLDER_BROWSING = 'Parse Prefer Folder Browsing';
  STR_DONT_PARSE_RELATIVE = 'Don''t Parse Relative';
  STR_PARSE_TRANSLATE_ALIASES = 'Parse Translate Aliases';

type
  SFGAOF = ULONG;

type
  // Different from Shlobj
  {$EXTERNALSYM PExtraSearch}
  PExtraSearch = ^TExtraSearch;
  {$EXTERNALSYM tagExtraSearch}
  tagExtraSearch = record
    guidSearch: TGUID;
    wszFriendlyName : array [0..79] of WideChar;
    wszUrl : array[0..2047] of WideChar;
  end;
  {$EXTERNALSYM TExtraSearch}
  TExtraSearch = tagExtraSearch;

{$IFDEF VER130}
  IEnumExtraSearch = interface(IUnknown)
    [SID_IEnumExtraSearch]
    function Next(celt: ULONG; out rgelt: PExtraSearch;
      out pceltFetched: ULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppEnum: IEnumExtraSearch): HResult; stdcall;
  end;
{$ENDIF}

const
  {IShellFolder2.GetDefaultColumnState Values}
  SHCOLSTATE_PREFER_VARCMP = $00000200;  // VarCmp() (with folderness test) produces same result as CompareIDs()

type
  SHCOLSTATEF = DWORD;

type
{$IFDEF VER130}
  PShColumnID = ^TShColumnID;
  SHCOLUMNID = record
    fmtid: TGUID;
    pid: DWORD;
  end;
  TShColumnID = SHCOLUMNID;

  PShellDetails = ^TShellDetails;
  {$EXTERNALSYM _SHELLDETAILS}
  _SHELLDETAILS = record
    fmt,
    cxChar: Integer;
    str: STRRET;
  end;
  TShellDetails = _SHELLDETAILS;
  {$EXTERNALSYM SHELLDETAILS}
  SHELLDETAILS = _SHELLDETAILS;

  IShellDetails = interface(IUnknown)
    [SID_IShellDetails]
    function GetDetailsOf(pidl: PItemIDList; iColumn: UINT;
      var pDetails: TShellDetails): HResult; stdcall;
    function ColumnClick(iColumn: UINT): HResult; stdcall;
  end;
{$ENDIF}

  // different to ShlObj!
  IShellFolder2 = interface(IShellFolder)
    [SID_IShellFolder2]
    // Returns the guid of the search that is to be invoked when user clicks
    // on the search toolbar button
    function GetDefaultSearchGUID(out pguid: TGUID): HResult; stdcall;
    // gives an enumerator of the searches to be added to the search menu
    function EnumSearches(out ppEnum: IEnumExtraSearch): HResult; stdcall;
    function GetDefaultColumn(dwRes: DWORD; var pSort: ULONG;
      var pDisplay: ULONG): HResult; stdcall;
    // return SHCOLSTATE_ values
    function GetDefaultColumnState(iColumn: UINT; var pcsFlags: DWORD): HResult; stdcall;
    function GetDetailsEx(pidl: PItemIDList; const pscid: SHCOLUMNID;
      pv: POleVariant): HResult; stdcall;
    function GetDetailsOf(pidl: PItemIDList; iColumn: UINT;
      var psd: TShellDetails): HResult; stdcall;
    function MapColumnToSCID(iColumn: uint; var pscid: TShColumnID): HResult; stdcall;
  end;


const
  FVM_FIRST = 1;
  FVM_THUMBNAIL = 5;
  FVM_TILE = 6;
  FVM_THUMBSTRIP = 7;
  FVM_LAST = 7;

{$IFDEF VER130}
type
  TFolderViewMode = (fvmIcon,fvmSmallIcon,fvmList,fvmDetails,fvmThumbNail,fvmTile,fvmThumbStrip);
const
  fvmFirst = fvmIcon;
  fvmLast = fvmThumbStrip;
{$ELSE}
type
  TFolderViewMode = (fvmFirst = 1,
                     fvmIcon = 1,
                     fvmSmallIcon = 2,
                     fvmList = 3,
	                   fvmDetails = 4,
	                   fvmThumbNail	= 5,
                     fvmTile = 6,
                     fvmThumbStrip = 7,
                     fvmLast = 7);
{$ENDIF}
const
  // shellview select item flags
  SVSI_SELECTIONMARK      = $0040;
  SVSI_POSITIONITEM       = $0080;
  SVSI_CHECK              = $0100;
  SVSI_NOSTATECHANGE      = $80000000;

type
  SVSIF = UINT;

const
  { shellview get item object flags }
  SVGIO_CHECKED           = $00000003;
  SVGIO_TYPE_MASK         = $0000000F;
  SVGIO_FLAG_VIEWORDER    = $80000000;

const
  SBSP_HELPMODE        = $0040; // IEUNIX : Help window uses this.
  SBSP_NOTRANSFERHIST  = $0080;

const
  IID_IFolderView: TGUID = (
    D1:$cde725b0; D2:$ccc9; D3:$4519; D4:($91,$7e,$32,$5d,$72,$fa,$b4,$ce));

  SID_IFolderView        = '{cde725b0-ccc9-4519-917e-325d72fab4ce}';

type
  IFolderView = interface(IUnknown)
    [SID_IFolderView]
    function GetCurrentViewMode(var pViewMode : uint) : hresult; stdcall;
    function SetCurrentViewMode(ViewMode : uint) : hresult; stdcall;
    function GetFolder(const riid : TIID; out ppv) : hresult; stdcall;
    function Item(ItemIndex : integer; var pidl : PItemIdList) : hresult; stdcall;
    function ItemCount(uFlags : UINT; var pcItems : integer) : hresult; stdcall;
    function Items(uFlags : UINT; const riid : TIID; out ppv) : hresult; stdcall;
    function GetSelectionMarkedItem(var piItem : integer) : hresult; stdcall;
    function GetFocusedItem(var piItem : integer) : hresult; stdcall;
    function GetItemPosition(pidl : PITEMIDLIST; var pt : PPoint) : hresult; stdcall;
    function GetSpacing(var pt : PPOINT) : hresult; stdcall;
    function GetDefaultSpacing(var pt : PPOINT) : hresult; stdcall;
    function GetAutoArrange : hresult; stdcall;
    // like IShellView::SelectItem() by index
    function SelectItem(iItem : integer; dwFlags : dword) : hresult; stdcall;
    function SelectAndPositionItems(cidl : uint; apidl : PITEMIDLIST; apt : PPOINT;
                                    dwFlags : dword) : hresult; stdcall;
  end;

const
  IID_IFolderFilterSite: TGUID = (
    D1:$C0A651F5; D2:$B48B; D3:$11d2; D4:($b5,$ed,$00,$60,$97,$c6,$86,$f6));

  SID_IFolderFilterSite        = '{C0A651F5-B48B-11d2-B5ED-006097C686F6}';

type
  IFolderFilterSite = interface(IUnknown)
    [SID_IfolderFilterSite]
    function SetFilter(punk : IUnknown) : HResult; stdcall;
  end;

const
  IID_IFolderFilter: TGUID = (
    D1:$9CC22886; D2:$DC8e; D3:$11d2; D4:($b1,$d0,$00,$c0,$4f,$8e,$eb,$3e));

  SID_IFolderFilter        = '{9CC22886-DC8E-11d2-B1D0-00C04F8EEB3E}';

type
  IFolderFilter = interface(IUnknown)
    [SID_IFolderFilter]
    function ShouldShow(psf : IShellFolder; pidlFolder,pidlItem : PITEMIDLIST) : HResult; stdcall;
    function GetEnumFlags(psf : IShellFolder; pidlFolder : PITEMIDLIST; const phWnd : HWND; var pgrfFlags : DWORD) : HResult; stdcall;
  end;

const
  IID_IProfferService: TGUID = (
    D1:$cb728b20; D2:$f786; D3:$11ce; D4:($92,$ad,$00,$aa,$00,$a7,$4c,$d0));

  SID_IProfferService = '{cb728b20-f786-11ce-92ad-00aa00a74cd0}';

type
  IProfferService = interface(IUnknown)
    [SID_IProfferService]
    function ProfferService(rguidService : TGUID; psp : IServiceProvider; var pdwCookie : dword) : hresult; stdcall;
    function RevokeService(dwCookie : dword) : hresult; stdcall;
  end;

const
  // PROPERTYUI_NAME_FLAGS
  PUIFNF_DEFAULT  = 0;
  PUIFNF_MNEMONIC = 1;   // include mnemonic in display name

  // PROPERTYUI_FLAGS;
  PUIF_DEFAULT = 0;
  PUIF_RIGHTALIGN = 1;  // this property should be right alligned
  PUIF_NOLABELININFOTIP = 2;   // this property should not display a label in the infotip

  // PROPERTYUI_FORMAT_FLAGS
  PUIFFDF_DEFAULT = 0;
  PUIFFDF_RIGHTTOLEFT = 1;   // BIDI support, right to left caller
  PUIFFDF_SHORTFORMAT = 2;   // short format version of string
  PUIFFDF_NOTIME      = 4;   // truncate time to days, not hours/mins/sec
  PUIFFDF_FRIENDLYDATE = 8;   // "Today", "Yesterday", etc
  PUIFFDF_NOUNITS      = $10; // don't do "KB", "MB", "KHz"

const
  IID_IPropertyUI: TGUID = (
    D1:$757a7d9f; D2:$919a; D3:$4118; D4:($99,$d7,$db,$b2,$08,$c8,$cc,$66));

  SID_IPropertyUI = '{757a7d9f-919a-4118-99d7-dbb208c8cc66}';

type
  IPropertyUI = interface(IUnknown)
    [SID_IPropertyUI]
    function ParsePropertyName(pszName : LPCWSTR; var pfmtid : FMTID; var ppid : PROPID; var chEaten : ulong) : hresult; stdcall;
    function GetCannonicalName(fmtid : TGUID; pid : PROPID; pwszText : LPWSTR; cchText : DWORD) : hresult; stdcall;
    function GetDisplayName(fmtid : TGUID; pid : PROPID; flags : integer; pwszText : LPWSTR; cchText : dword) : hresult; stdcall;
    function GetPropertyDescription(fmtid : TGUID; pid : PROPID; pwszText : LPWSTR; cchText : DWORD) : hresult; stdcall;
    function GetDefaultWidth(fmtid : TGUID; pid : PROPID; var pcxChars : ULONG) : hresult; stdcall;
    function GetFlags(fmtid : TGUID; pid : PROPID; var pFlags : integer) : hresult; stdcall;
    function FormatForDisplay(fmtid : TGUID; pid : PROPID; const pvar : PROPVARIANT; flags : integer; pwszText : LPWSTR; cchText : dword) : hresult; stdcall;
    function GetHelpInfo(fmtid : TGUID; pid : PROPID; pwszHelpFile : LPWSTR; cch : DWORD; var puHelpID : uint) : hresult; stdcall;
  end;

const
  SID_ICategoryProvider = '{9af64809-5864-4c26-a720-c1f78c086ee3}';

type
  ICategoryProvider = interface(IUnknown)
    [SID_ICategoryProvider]
    // Returns S_OK if the view should display this column in category selection UI, or S_FALSE to remove it.
    function CanCategorizeOnSCID(pscid  : PSHCOLUMNID) : HResult; stdcall;
    // Returns either a GUID to create in CreateCategory, or a SHCOLUNNID that
    // is used by the default categorizer. Return S_FALSE if you do not support
    // a default group. GUID_NULL returned in pguid indicates to the client to
    // use pscid as the default category.
    function GetDefaultCategory(var pguid : TGUID; pscid : PSHCOLUMNID) : HResult; stdcall;
    // Returns either a GUID that represents the categorizer to use for the
    // specified SHCOLUMNID.
    function GetCategoryForSCID(pscid : PSHColumnID; var guid : TGUID) : HResult; stdcall;
    // Returns an IEnumGUID that has a list of GUIDs that represent categories.
    function EnumCategories(var pEnum : IEnumGUID) : HResult; stdcall;
    // Returns the name of the given category.
    function GetCategoryName(const pguid : TGUID; pszName : LPWSTR; cch : LongWord) : HResult; stdcall;
    // Creates the category.
    function CreateCategory(const pguid : TGUID; const riid : TIID; out ppv) : HResult; stdcall;
  end;

const
  CATINFO_NORMAL      = $00000000;   // Apply default properties to this category
  CATINFO_COLLAPSED   = $00000001;   // This category should appear collapsed. useful for the "None" category.
  CATINFO_HIDDEN      = $00000002;   // This category should follow the "Hidden" files setting for being displayed

type
  CATEGORYINFO_FLAGS = DWORD;

const
  CATSORT_DEFAULT     = $00000000;   // Default Sort order
  CATSORT_NAME        = $00000001;   // Sort by name

type
  CATSORT_FLAGS = dword;

  CATEGORY_INFO = packed record
    cif : CATEGORYINFO_FLAGS;
    wszName : array [0..259] of WideChar;
  end;
  TCategoryInfo = CATEGORY_INFO;
  PCategoryInfo = ^TCategoryInfo;

  ICategorizer = interface(IUnknown)
    ['{A3B14589-9174-49A8-89A3-06A1AE2B9BA7}']
    // Returns the description of this category that will be displayed in the UI
    function GetDescription(pszDesc: PWideChar; cch: LongWord): HResult; stdcall;
    // Returns a list of categories associated with a list of ID Lists. NOTE:
    // -1 is an invalid Category ID, and they cannot be persisted
    function GetCategory(cidl: LongWord; var apidl: PItemIdList; rgCategoryIds: PLongWord): HResult; stdcall;
    // Returns information about the category, such as default display and the
    // text to display in the UI
    function GetCategoryInfo(dwCategoryId: LongWord; pci: PCategoryInfo): HResult; stdcall;
    // Returns HRESULTFromShort. -1, 0, 1 indicate the comparison of the IDs.
    // Used for sorting categories in the UI
    function CompareCategory(csfFlags: CATSORT_FLAGS; dwCategoryId1: LongWord;
                             dwCategoryId2: LongWord): HResult; stdcall;
  end;

const
  SLR_NOSEARCH            = $0010;   // don't execute the search heuristics
  SLR_NOTRACK             = $0020;   // don't use NT5 object ID to track the link
  SLR_NOLINKINFO          = $0040;   // don't use the net and volume relative info
  SLR_INVOKE_MSI          = $0080;   // if we have a darwin link, then call msi to fault in the applicaion
  SLR_NO_UI_WITH_MSG_PUMP = $0101;   // SLR_NO_UI + requires an enable modeless site or HWND

type
  SLR_FLAGS = DWORD;

type
  SLGP_FLAGS = dword;

const
  SPINITF_NORMAL         = $00000000;      // default normal progress behavior
  SPINITF_MODAL          = $00000001;      // call punkSite->EnableModeless() or EnableWindow()
  SPINITF_NOMINIMIZE     = $00000008;      // Do not have a minimize button in the caption bar.

type
  SPINITF = DWORD;

  IActionProgressDialog = interface(IUnknown)
    ['{49ff1172-eadc-446d-9285-156453a6431c}']
    function Initialize(flags : SPINITF; pszTitle : LPCWSTR; pszCancel : LPCWSTR) : HResult; stdcall;
    function Stop : HResult; stdcall;
  end;

  IHWEventHandler = interface(IUnknown)
    ['{C1FB73D0-EC3A-4ba2-B512-8CDB9187B6D1}']
    function Initialize(pszParams : LPCWSTR) : HResult; stdcall;
    function HandleEvent(pszDeviceID, pszAltDeviceID, pszEventType : LPCWSTR) : HResult; stdcall;
    function HandleEventWithContent(pszDeviceID,pszAltDeviceID,pszEventType,pszContentTypeHandler : LPCWSTR;
                                    pDataObject : IDataObject) : HResult; stdcall;
  end;

const
  ARCONTENT_AUTORUNINF     = $00000002;
  ARCONTENT_AUDIOCD        = $00000004;
  ARCONTENT_DVDMOVIE       = $00000008;
  ARCONTENT_BLANKCD        = $00000010;
  ARCONTENT_BLANKDVD       = $00000020;
  ARCONTENT_UNKNOWNCONTENT = $00000040;
  ARCONTENT_AUTOPLAYPIX    = $00000080;
  ARCONTENT_AUTOPLAYMUSIC  = $00000100;
  ARCONTENT_AUTOPLAYVIDEO  = $00000200;

type
  IQueryCancelAutoPlay = interface (IUnknown)
    ['{DDEFE873-6997-4e68-BE26-39B633ADBE12}']
    function AllowAutoPlay(pszPath : LPCWSTR; dwContentType : DWORD;
      pszLabel : LPCWSTR; dwSerialNumber : DWORD) : HResult; stdcall;
  end;

const
  SPBEGINF_NORMAL  = 0;
  SPBEGINF_AUTOTIME  = 2;
  SPBEGINF_NOPROGRESSBAR = $10;
  SPBEGINF_MARQUEEPROGRESS = $20;

type
  SPBEGINF = DWord;

const
  SPACTION_NONE	= 0;
  SPACTION_MOVING = 1;
  SPACTION_COPYING = 2;
  SPACTION_RECYCLING = 3;
  SPACTION_APPLYINGATTRIBS = 4;
  SPACTION_DOWNLOADING	= 5;
  SPACTION_SEARCHING_INTERNET = 6;
  SPACTION_CALCULATING	= 7;
  SPACTION_UPLOADING = 8;
  SPACTION_SEARCHING_FILES = 9;

type
  SPACTION = dword;

const
  SPTEXT_ACTIONDESCRIPTION    = 1;
  SPTEXT_ACTIONDETAIL	      = 2;
type
  SPTEXT = DWord;

  IActionProgress = interface(IUnknown)
    ['{49ff1173-eadc-446d-9285-156453a6431c}']
    function bbegin(action : SPACTION; flags : SPBEGINF) : HResult; stdcall;
    function UpdateProgress(ulCompleted : int64; ulTotal : int64) : HResult; stdcall;
    function UpdateText(asptext : SPTEXT;pszText : LPCWSTR; fMayCompact : bool) : HResult; stdcall;
    function QueryCancel(var pfCancelled : bool) : HResult; stdcall;
    function ResetCancel : HResult; stdcall;
    function bEnd : HResult; stdcall;
  end;

const
  EXPPS_FILETYPES     = $00000001;

type
  EXPPS = UINT;

type
  IRemoteComputer = interface
    [SID_IRemoteComputer]
    function Initialize(pszMachine : PWideChar; bEnumerating : bool) : HResult; stdcall;
  end;

  IQueryContinue = interface
    ['{7307055c-b24a-486b-9f25-163e597a28a9}']
    function QueryContinue : HResult; stdcall;    // S_OK -> Continue, other
  end;

  IUserNotification = interface
    ['{ba9711ba-5893-4787-a7e1-41277151550b}']
    function SetBalloonInfo(pszTitle : LPCWSTR; pszText : LPCWSTR; dwInfoFlags : dword) : hresult; stdcall;
    function SetBalloonRetry(dwShowTime : dword; dwInterval : dword; cRetryCount : uint) : hresult; stdcall;
    function SetIconInfo(icon : HICON; pszToolTip : LPCWSTR) : hresult; stdcall;
    function Show(pqc : IQueryContinue; dwContinuePollInterval : dword) : hresult; stdcall;
    function PlaySound(pszSoundName : LPCWSTR) : hresult; stdcall;
  end;

  IItemNameLimits = interface
    ['{1df0d7f1-b267-4d28-8b10-12e23202a5c4}']
    function GetValidCharacters(ppwszValidChars : LPWSTR; ppwszInvalidChars : LPWSTR) : hresult; stdcall;
    function GetMaxLength(pszName : LPCWSTR; var piMaxNameLen : integer) : hresult; stdcall;
  end;

const
  SNCF_REFRESHLIST = $00000001;  // refresh the list (eg. from F5 or opening a folder)

type
  INetCrawler = interface
    ['{49c929ee-a1b7-4c58-b539-e63be392b6f3}']
    function Update(dwFlags : DWORD) : hresult; stdcall;
  end;

const
  ITSAT_MAX_PRIORITY = $7fffffff;
  ITSAT_MIN_PRIORITY = $00000000;
  ITSAT_DEFAULT_PRIORITY = $10000000;

  IEI_PRIORITY_MAX = ITSAT_MAX_PRIORITY;
  IEI_PRIORITY_MIN = ITSAT_MIN_PRIORITY;
  IEI_PRIORITY_NORMAL = ITSAT_DEFAULT_PRIORITY;
  IEIFLAG_ASYNC      = $0001;      // ask the extractor if it supports ASYNC extract (free threaded)
  IEIFLAG_CACHE      = $0002;      // returned from the extractor if it does NOT cache the thumbnail
  IEIFLAG_ASPECT     = $0004;      // passed to the extractor to beg it to render to the aspect ratio of the supplied rect
  IEIFLAG_OFFLINE    = $0008;      // if the extractor shouldn't hit the net to get any content neede for the rendering
  IEIFLAG_GLEAM      = $0010;      // does the image have a gleam ? this will be returned if it does
  IEIFLAG_SCREEN     = $0020;      // render as if for the screen  (this is exlusive with IEIFLAG_ASPECT )
  IEIFLAG_ORIGSIZE   = $0040;      // render to the approx size passed, but crop if neccessary
  IEIFLAG_NOSTAMP    = $0080;      // returned from the extractor if it does NOT want an icon stamp on the thumbnail
  IEIFLAG_NOBORDER   = $0100;      // returned from the extractor if it does NOT want an a border around the thumbnail
  IEIFLAG_QUALITY    = $0200;      // passed to the Extract method to indicate that a slower, higher quality image is desired, re-compute the thumbnail
  IEIFLAG_REFRESH    = $0400;      // returned from the extractor if it would like to have Refresh Thumbnail available

type
  IExtractImage = interface
    ['{BB2E617C-0920-11d1-9A0B-00C04FC2D6C1}']
    function GetLocation(Buffer : PWideChar;
                         BufferSize : cardinal;
                         var Priority : cardinal;
                         const Size : TSize;
                         ColorDepth : cardinal;
                         var Flags : cardinal) : hresult; stdcall;
    function Extract(var BmpThumbnail : HBITMAP) : hresult; stdcall;
  end;

  IExtractImage2 = interface(IExtractImage)
    ['{953BB1EE-93B4-11d1-98A3-00C04FB687DA}']
    function GetDateStamp(var DateStamp : TFILETIME) : hresult; stdcall;
  end;

  IUserEventTimerCallback = interface
    ['{e9ead8e6-2a25-410e-9b58-a9fbef1dd1a2}']
    function UserEventTimerProc(uUserEventTimerID : ULONG; uTimerElapse : UINT) : hresult; stdcall;
  end;

  IUserEventTimer = interface
    ['{0F504B94-6E42-42E6-99E0-E20FAFE52AB4}']
    function SetUserEventTimer(Wnd : HWND; uCallbackMessage : UINT;
                               uTimerElapse : UINT; pUserEventTimerCallback : IUserEventTimerCallback;
                               var puUserEventTimerID : ULONG) : hresult; stdcall;
    function KillUserEventTimer(Wnd : HWND; uUserEventTimerID : ULONG) : hresult; stdcall;
    function GetUserEventTimerElapsed(Wnd : HWND;uUserEventTimerID : ULONG;
                                      var puTimerElapsed : UINT) : hresult; stdcall;
    function InitTimerTickInterval(uTimerTickIntervalMs : uint) : hresult; stdcall;
  end;

const
  DBIMF_FIXED                 = $0001;
  DBIMF_FIXEDBMP              = $0004;   // a fixed background bitmap (if supported)
  DBIMF_UNDELETEABLE          = $0010;
  DBIMF_USECHEVRON            = $0080;
  DBIMF_BREAK                 = $0100;
  DBIMF_ADDTOFRONT            = $0200;
  DBIMF_TOPALIGN              = $0400;

	DBID_PUSHCHEVRON	= 3;
	DBID_DELAYINIT	= 4;
	DBID_FINISHINIT	= 5;
	DBID_SETWINDOWTHEME	= 6;
	DBID_PERMITAUTOHIDE	= 7;

  DBPC_SELECTFIRST    = DWORD(-1);
  DBPC_SELECTLAST     = DWORD(-2);

type
  ITaskbarList = interface(IUnknown)
    ['{56FDF342-FD6D-11d0-958A-006097C9A090}']
    function HrInit : HResult; stdcall;
    function AddTab(wnd : HWND) : HResult; stdcall;
    function DeleteTab(wnd : HWND) : HResult; stdcall;
    function ActivateTab(wnd : HWND) : HResult; stdcall;
    function SetActiveAlt(wnd : HWND) : HResult; stdcall;
  end;

  ITaskbarList2 = interface(ITaskbarList)
    ['{602D4995-B13A-429b-A66E-1935E44F4317}']
    function MarkFullscreenWindow(wnd : HWND; fFullscreen : bool) : HResult; stdcall;
  end;

  ICDBurn = interface
    ['{3d73a659-e5d0-4d42-afc0-5121ba425c8d}']
    function GetRecorderDriveLetter(pszDrive : LPWSTR; cch : UINT) : HResult; stdcall;
    function Burn(wnd : HWND) : HResult; stdcall;
    function HasRecordableDrive(var pfHasRecorder : bool) : HResult; stdcall;
  end;

  IAddressBarParser = interface
    ['{C9D81948-443A-40C7-945C-5E171B8C66B4}']
    function ParseFromOutsideSource(pcszUrlIn : LPCWSTR;dwParseFlags : DWORD;
                                    var pfWasCorrected : bool) : HResult; stdcall;
    function GetUrl(pszUrlOut : LPWSTR; cchUrlOutSize : DWORD) : HResult; stdcall;
    function SetUrl(pcszUrlIn : LPCWSTR; dwGenType : dword) : HResult; stdcall;
    function GetDisplayName(pszUrlOut : LPWSTR; cchUrlOutSize : dword) : HResult; stdcall;
    function GetPidl(var ppidl : PItemIdList) : HResult; stdcall;
    function SetPidl(pidl : PItemIdList) : HResult; stdcall;
    function GetArgs(pszArgsOut : LPWSTR; cchArgsOutSize : dword) : HResult; stdcall;
    function AddPath(pidl : PItemIdList) : HResult; stdcall;
  end;

const
  IDD_WIZEXTN_FIRST  = $5000;
  IDD_WIZEXTN_LAST   = $5100;

type
  IWizardSite = interface
    ['{88960f5b-422f-4e7b-8013-73415381c3c3}']
    function GetPreviousPage(var phpage : HPROPSHEETPAGE) : HResult; stdcall;
    function GetNextPage(var phPage : HPROPSHEETPAGE) : HResult; stdcall;
    function GetCancelledPage(var phPage : HPROPSHEETPAGE) : HResult; stdcall;
  end;

  IWizardExtension = interface
    ['{c02ea696-86cc-491e-9b23-74394a0444a8}']
    function AddPages(var apages : HPROPSHEETPAGE; cPages : uint; var pnPagesAdded : uint) : HResult; stdcall;
    function GetFirstPage(var phPage : HPROPSHEETPAGE) : HResult; stdcall;
    function GetLastPage(var phPage : HPROPSHEETPAGE) : HResult; stdcall;
  end;

  IWebWizardExtension = interface
    ['{0e6b3f66-98d1-48c0-a222-fbde74e2fbc5}']
    function SetInitialURL(pszURL : LPCWSTR) : HResult; stdcall;
    function SetErrorURL(pszErrorURL : LPCWSTR) : HResult; stdcall;
  end;

const
  SHPWHF_NORECOMPRESS          = $00000001;  // don't allow/prompt for recompress of streams
  SHPWHF_NONETPLACECREATE      = $00000002;  // don't create a network place when transfer is complete
  SHPWHF_NOFILESELECTOR        = $00000004;  // don't show the file selector
  SHPWHF_VALIDATEVIAWEBFOLDERS = $00010000;  // enable web folders to validate network places (ANP support)

type
  IPublishingWizard = interface(IWizardExtension)
    ['{aa9198bb-ccec-472d-beed-19a4f6733f7a}']
    function Initialize(pdo : IDataObject; dwOptions : dword; pszServiceProvider : LPCWSTR) : HResult; stdcall;
  end;

  IFolderViewHost = interface
    ['{1ea58f02-d55a-411d-b09e-9e65ac21605b}']
    function Initialize(hwndParent : HWND; pdo : IDataObject; const prc : TRect) : HResult; stdcall;
  end;

const
  ACDD_VISIBLE = $0001;

type
  IAutoCompleteDropDown = interface
    ['{3CD141F4-3C6A-11d2-BCAA-00C04FD929DB}']
    function GetDropDownStatus(var pdwFlags : DWORD; ppwszString : LPWSTR) : HResult; stdcall;
    function ResetEnumerator : HResult; stdcall;
  end;

const
  PPW_LAUNCHEDBYUSER = $00000001; // The wizard was launch explicitly by the user, not on demand by the key manager

type
  IModalWindow = interface
    ['{b4db1657-70d7-485e-8e3e-6fcb5a5c1802}']
    function Show(hwndParent : HWnd) : HResult; stdcall;
  end;

  IPassportWizard = interface(IModalWindow)
    ['{a09db586-9180-41ac-9114-460a7f362b76}']
    function SetOptions(dwOptions : DWORD) : HResult; stdcall;
  end;

// IContextMenuSite - handles QueryContextMenu, TrackPopupMenu, Invoke (with all
// the proper point unicode/ansi information applied), and all message forwarding
// for whatever wants to do a TrackPopupMenu on an IContextMenu.
// The site usually updates the status bar as well.
//
// fFlags is passed to the QueryContexteMneu call, pt is in screen coordinates.
//
  IContextMenuSite = interface
    ['{0811AEBE-0B87-4C54-9E72-548CF649016B}']
    function DoContextMenuPopup(unkContextMenu : IUnknown; fFlags : UINT; pt : TPoint) : HResult; stdcall;
  end;

  PFDVENUMREADYBALLBACK = procedure(pvData : Pointer); stdcall;

  IDVGetEnum = interface
    ['{70F55181-5FEA-4900-B6B8-7343CB0A348C}']
    function SetEnumReadyCallback(pfn : PFDVENUMREADYBALLBACK; pvData : Pointer) : HResult; stdcall;
    function CreateEnumIDListFromContents(pidlFolder : PItemIdList;
              dwEnumFlags : DWORD; EnumIDList : IEnumIDList) : HResult; stdcall;
  end;

  IInsertItem = interface
    ['{D2B57227-3D23-4b95-93C0-492BD454C356}']
    function InsertItem(pidl : PItemIdList) : HResult; stdcall;
  end;

  IDeskBar = interface
    ['{EB0FE173-1A3A-11D0-89B3-00A0C90A90AC}']
    function SetClient(unkClient : IUnknown) : HResult; stdcall;
    function GetClient(var unkClient : IUnknown) : HResult; stdcall;
    function OnPosRectChangeDB(const prc : TRect) : HResult; stdcall;
  end;

//-------------------------------------------------------------------------
//
// IMenuBand interface
//
//   This interface provides methods the menuband (CLSID_MenuBand)
//  to receive pertinent messages.
//
// [Member functions]
//
// IMenuBand::IsMenuMessage(pmsg)
//   A message pump calls this function to see if any messages need
//   to be redirected to this object.  If this returns S_OK, the
//   message loop should not call TranslateMessage or DispatchMessage.
//   If this returns E_FAIL, the menu has exited menu mode and is ready
//   to be destroyed.
//
// IMenuBand::TranslateMenuMessage(pmsg, plRet)
//   Offers the object an opportunity to translate messages.  The
//   parent window proc must call this method for every message (not
//   the message pump).  The message, wParam, and lParam should be
//   delivered to this method in *pmsg.  This method may change the
//   values of pmsg->wParam or pmsg->lParam, in which case these changes
//   should be forwarded on.
//
//   This method is required because some modal message pumps (like the one
//   in TrackPopupMenu) do not give an opportunity to call a custom
//   TranslateAccelerator method like IInputObject::TranslateAcceleratorIO.
//
//   TranslateMenuMessage returns S_OK if the message was handled and
//   should be eaten.  *plRet is not touched if this returns S_FALSE.
//
//
//-------------------------------------------------------------------------
const
  MBHANDCID_PIDLSELECT	= 0;

type
  IMenuBand = interface
    ['{568804CD-CBD7-11d0-9816-00C04FD91972}']
    function IsMenuMessage(const pmsg : MSG) : HResult; stdcall;
    function TranslateMenuMessage(var pmsg : MSG; plRet : LRESULT) : HResult; stdcall;
  end;

  IFolderBandPriv = interface
    ['{47c01f95-e185-412c-b5c5-4f27df965aea}']
    function SetCascade(f : BOOL) : HResult; stdcall;
    function SetAccelerators(f : BOOL) : HResult; stdcall;
    function SetNoIcons(f : BOOL) : HResult; stdcall;
    function SetNoText(f : BOOL) : HResult; stdcall;
  end;

//-------------------------------------------------------------------------
//
// IBandSite interface
//
//   This interface provides methods to get or set bandsite information.
//
// [Member functions]
//
// IBandSite::AddBand(punk)
//   Add a band to the bandsite.  Returns the band ID in ShortFromResult(hres).
//
// IBandSite::EnumBands(uBand, *pdwBandID)
//   Enumerate the bands. If uBand is -1, pdwBandID is ignored and this
//   method returns the count of bands in the bandsite.  Call this method
//   with uBand starting at 0 to begin enumerating.  Returns S_OK and the
//   band ID in *pdwBandID of the next band.
//
// IBandSite::QueryBand(dwBandID, ppstb, pdwState, pszName, cchName)
//   Get info about a band.
//
// IBandSite::SetBandState(dwBandID, dwState)
//   Set the band's state.
//
// IBandSite::RemoveBand(dwBandID)
//   Remove the band.
//
// IBandSite::GetBandObject(dwBandID, riid, ppv)
//   Get an object that support riid for the band.
//
// IBandSite::GetBandSiteInfo(pbsinfo)
//   Get info about the bandsite.
//
// IBandSite::SetBandSiteInfo(pbsinfo)
//   Set info about the bandsite.
//
//-------------------------------------------------------------------------
type
  BANDSITEINFO = record
    dwMask : DWORD;
    dwState : DWORD;
    dwStyle : DWORD;
  end;
  TBandSiteInfo = BANDSITEINFO;
  PBandSiteInfo = ^TBandSiteInfo;

const
  BSID_BANDADDED	  = 0;
	BSID_BANDREMOVED	= 1;

  BSIM_STATE             = $00000001;
  BSIM_STYLE             = $00000002;
  BSSF_VISIBLE           = $00000001;
  BSSF_NOTITLE           = $00000002;
  BSSF_UNDELETEABLE      = $00001000;
  BSIS_AUTOGRIPPER       = $00000000;
  BSIS_NOGRIPPER         = $00000001;
  BSIS_ALWAYSGRIPPER     = $00000002;
  BSIS_LEFTALIGN         = $00000004;
  BSIS_SINGLECLICK       = $00000008;
  BSIS_NOCONTEXTMENU     = $00000010;
  BSIS_NODROPTARGET      = $00000020;
  BSIS_NOCAPTION         = $00000040;
  BSIS_PREFERNOLINEBREAK = $00000080;
  BSIS_LOCKED            = $00000100;

type
  IBandSite = interface
    ['{4CF504B0-DE96-11D0-8B3F-00A0C911E8E5}']
    function AddBand(pUnk : IUnknown) : HResult; stdcall;
    function EnumBands(Band : UINT; var pdwBandID : DWord) : HResult; stdcall;
    function QueryBand(BandID : dword; var ppstb : IDeskBand;
                       var pdwState : DWORD; pszName : LPWSTR;
                       cchName : integer) : HResult; stdcall;
    function SetBandState(dwBandID : DWORD; dwMask : DWORD; dwState : DWORD) : HResult; stdcall;
    function RemoveBand(dwBandID : DWORD) : HResult; stdcall;
    function GetBandObject(dwBandID : DWORD; const riid : TIID; out ppv : Pointer) : HResult; stdcall;
    function SetBandSiteInfo(const pbsinfo : BANDSITEINFO) : HResult; stdcall;
    function GetBandSiteInfo(var pbsInfo : BANDSITEINFO) : HResult; stdcall;
  end;

  INamespaceWalkCB = interface
    ['{d92995f8-cf5e-4a76-bf59-ead39ea2b97e}']
    function FoundItem(psf : IShellFolder; pidl : PItemIdList) : HResult; stdcall;
    function EnterFolder(psf : IShellFolder; pidl : PItemIdList) : HResult; stdcall;
    function LeaveFolder(psf : IShellFolder; pidl : PItemIdList) : HResult; stdcall;
    function InitializeProgressDialog(ppszTitle : LPWSTR; ppszCancel : LPWSTR) : HResult; stdcall;
  end;

const
  NSWF_NONE_IMPLIES_ALL	= $1;
	NSWF_ONE_IMPLIES_ALL	= $2;
	NSWF_DONT_TRAVERSE_LINKS	= $4;
	NSWF_DONT_ACCUMULATE_RESULT	= $8;
	NSWF_TRAVERSE_STREAM_JUNCTIONS	= $10;
	NSWF_FILESYSTEM_ONLY	= $20;
	NSWF_SHOW_PROGRESS	= $40;
	NSWF_FLAG_VIEWORDER	= $80;
	NSWF_IGNORE_AUTOPLAY_HIDA	= $100;

type
  INamespaceWalk = interface
    ['{57ced8a7-3f4a-432c-9350-30f24483f74f}']
    function Walk(punkToWalk : IUnknown; dwFlags : dword; cDepth : integer; pnswcb : INamespaceWalkCB) : HResult; stdcall;
    function GetIDArrayResult(var pcItems : UINT; var pppidl : PItemIdList) : HResult; stdcall;
  end;

  IRegTreeItem = interface
    ['{A9521922-0812-4d44-9EC3-7FD38C726F3D}']
    function GetCheckState(var pbCheck : BOOL) : HResult; stdcall;
    function SetCheckState(bCheck : BOOL) : HResult; stdcall;
  end;

//-------------------------------------------------------------------------
//
// IMenuPopup interface
//
//   This interface provides methods to navigate thru a menu.
//
// [Member functions]
//
// IMenuPopup::Popup(ppt, prcExclude, dwFlags)
//   Invoke the menu, located at the point *ppt (in screen coordinates).
//   The optional prcExclude points to the rectangle to exclude when
//   positioning the menu, otherwise it should be NULL.  dwFlags may be:
//
//      MDBPU_SETFOCUS: the menu can take the focus.
//
//   Returns S_OK if the object implements the popup menu as a modeless
//   menu.  Otherwise it returns S_FALSE, and the menu is finished.
//
// IMenuPopup::OnSelect(dwSelectType)
//   This method handles selection notifications.
//
// IMenuPopup::SetSubMenu(pmp, fSet)
//   Sets the given menu bar interface to be the submenu of this
//   object's interface.  Set fSet == FALSE to remove the submenu.
//
//-------------------------------------------------------------------------

const
  // Type values for IMenuPopup::OnSelect
  MPOS_EXECUTE = 0;               // Execute the selected menu item
  MPOS_FULLCANCEL = 1;            // Cancel the entire menu
  MPOS_CANCELLEVEL = 2;           // Cancel the current cascaded menu
  MPOS_SELECTLEFT = 3;            // select one to the left of the cur selection
  MPOS_SELECTRIGHT = 4;           // select one to the right of the cur selection
  MPOS_CHILDTRACKING = 5;         // the child got a tracking select (mouse moved over)

  // Flags for IMenuPopup::Popup
  MPPF_SETFOCUS	= $1;
	MPPF_INITIALSELECT	= $2;
	MPPF_NOANIMATE	= $4;
	MPPF_KEYBOARD	= $10;
	MPPF_REPOSITION	= $20;
	MPPF_FORCEZORDER	= $40;
	MPPF_FINALSELECT	= $80;
	MPPF_TOP	= $20000000;
	MPPF_LEFT	= $40000000;
	MPPF_RIGHT	= $60000000;
	MPPF_BOTTOM	= $80000000;
	MPPF_POS_MASK	= $e0000000;

type
  IMenuPopup = interface(IDeskBar)
    ['{D1E7AFEB-6A2E-11d0-8C78-00C04FD918B4}']
    function Popup(const ppt : TPointL; const prcExclude : TRect; dwFlags : dword) : HResult; stdcall;
    function OnSelect(dwSelectType : DWord) : HResult; stdcall;
    function SetSubMenu(pmp : IMenuPopup; fSet : BOOL) : HResult; stdcall;
  end;

const
  SIGDN_NORMALDISPLAY               = $00000000;
  SIGDN_PARENTRELATIVEPARSING       = $80018001;
  SIGDN_PARENTRELATIVEFORADDRESSBAR = $8001c001;
  SIGDN_DESKTOPABSOLUTEPARSING      = $80028000;
  SIGDN_PARENTRELATIVEEDITING       = $80031001;
  SIGDN_DESKTOPABSOLUTEEDITING      = $8004c000;
  SIGDN_FILESYSPATH                 = $80058000;
  SIGDN_URL                         = $80068000;

  SICHINT_DISPLAY   = $00000000; //iOrder based on display in a folder view
  SICHINT_ALLFIELDS = $80000000; //exact instance compare
  SICHINT_CANONICAL = $10000000; //iOrder based on canonical name (better performance)

type
  SIGDN = DWORD;
  SICHINTF = DWORD;

  IShellItem = interface
    ['{43826d1e-e718-42ee-bc55-a1e261c37bfe}']
    function BindToHandler(pbc : IBindCtx; rbhid : TGUID; const riid : TIID; out ppvOut : Pointer) : HResult; stdcall;
    function GetParent(var ppsi : IShellItem) : HResult; stdcall;
    function GetDisplayName(sigdnName : SIGDN; var ppszName : POLESTR) : HResult; stdcall;
    function GetAttributes(sfgaoMask : SFGAOF; var psfgaoAttribs : SFGAOF) : HResult; stdcall;
    function Compare(psi : IShellItem; hint : SICHINTF; var piOrder : integer) : HResult; stdcall;
  end;

  IImageRecompress = interface
    ['{505f1513-6b3e-4892-a272-59f8889a4d3e}']
    function RecompressImage(psi : IShellItem; cx,cy,iQuality : integer;
                             pstg : IStorage; var ppstrmOut : IStream) : HResult; stdcall;
  end;

  IDefViewSafety = interface
    ['{9A93B3FB-4E75-4c74-871A-2CDA667F39A5}']
    function IsSafePage : HResult; stdcall;
  end;

const
  PROPSTR_EXTENSIONCOMPLETIONSTATE = 'ExtensionCompletionState';
 	CDBE_RET_DEFAULT	= 0;
	CDBE_RET_DONTRUNOTHEREXTS	= 1;
	CDBE_RET_STOPWIZARD	= 2;

type
  ICDBurnExt = interface
    ['{2271dcca-74fc-4414-8fb7-c56b05ace2d7}']
    function GetSupportedActionTypes(pdwActions : dword) : HResult; stdcall;
  end;

type
  QueryCancelAutoPlay = IQueryCancelAutoPlay;
  DriveSizeCategorizer = ICategorizer;
  DriveTypeCategorizer = ICategorizer;
  FreeSpaceCategorizer = ICategorizer;
  TimeCategorizer = ICategorizer;
  SizeCategorizer = ICategorizer;
  AlphabeticalCategorizer = ICategorizer;
  MergedCategorizer = ICategorizer;
  ImageProperties = IPersistFile;
  PropertiesUI = IPropertyUI;
  UserNotification = IUserNotification;
  UserEventTimerCallback = IUserEventTimerCallback;
  UserEventTimer = IUserEventTimer;
  NetCrawler = INetCrawler;
  CDBurn = ICDBurn;
  AddressBarParser = IAddressBarParser;
  TaskbarList = ITaskbarList;
  WebWizardHost = IWebWizardExtension;
  PublishDropTarget = IDropTarget;
  PublishingWizard = IPublishingWizard;
  InternetPrintOrdering = IDropTarget;
  FolderViewHost = IFolderViewHost;
  NamespaceWalker = INamespaceWalk;
  ImageRecompress = IImageRecompress;
  TrayBandSiteService = IBandSite;
  PassportWizard = IPassportWizard;

function SHAlloc(cb : cardinal) : pointer; stdcall;
procedure SHFree(pv : pointer); stdcall;

//===========================================================================
//
// IContextMenu interface
//
// [OverView]
//
//  The shell uses the IContextMenu interface in following three cases.
//
// case-1: The shell is loading context menu extensions.
//
//   When the user clicks the right mouse button on an item within the shell's
//  name space (i.g., file, directory, server, work-group, etc.), it creates
//  the default context menu for its type, then loads context menu extensions
//  that are registered for that type (and its base type) so that they can
//  add extra menu items. Those context menu extensions are registered at
//  HKCR\{ProgID}\shellex\ContextMenuHandlers.
//
// case-2: The shell is retrieving a context menu of sub-folders in extended
//   name-space.
//
//   When the explorer's name space is extended by name space extensions,
//  the shell calls their IShellFolder::GetUIObjectOf to get the IContextMenu
//  objects when it creates context menus for folders under those extended
//  name spaces.
//
// case-3: The shell is loading non-default drag and drop handler for directories.
//
//   When the user performed a non-default drag and drop onto one of file
//  system folders (i.e., directories), it loads shell extensions that are
//  registered at HKCR\{ProgID}\DragDropHandlers.
//
//
// [Member functions]
//
//
// IContextMenu::QueryContextMenu
//
//   This member function may insert one or more menuitems to the specified
//  menu (hmenu) at the specified location (indexMenu which is never be -1).
//  The IDs of those menuitem must be in the specified range (idCmdFirst and
//  idCmdLast). It returns the maximum menuitem ID offset (ushort) in the
//  'code' field (low word) of the scode.
//
//   The uFlags specify the context. It may have one or more of following
//  flags.
//
//  CMF_DEFAULTONLY: This flag is passed if the user is invoking the default
//   action (typically by double-clicking, case 1 and 2 only). Context menu
//   extensions (case 1) should not add any menu items, and returns NOERROR.
//
//  CMF_VERBSONLY: The explorer passes this flag if it is constructing
//   a context menu for a short-cut object (case 1 and case 2 only). If this
//   flag is passed, it should not add any menu-items that is not appropriate
//   from a short-cut.
//    A good example is the "Delete" menuitem, which confuses the user
//   because it is not clear whether it deletes the link source item or the
//   link itself.
//
//  CMF_EXPLORER: The explorer passes this flag if it has the left-side pane
//   (case 1 and 2 only). Context menu extensions should ignore this flag.
//
//   High word (16-bit) are reserved for context specific communications
//  and the rest of flags (13-bit) are reserved by the system.
//
//
// IContextMenu::InvokeCommand
//
//   This member is called when the user has selected one of menuitems that
//  are inserted by previous QueryContextMenu member. In this case, the
//  LOWORD(lpici->lpVerb) contains the menuitem ID offset (menuitem ID -
//  idCmdFirst).
//
//   This member function may also be called programmatically. In such a case,
//  lpici->lpVerb specifies the canonical name of the command to be invoked,
//  which is typically retrieved by GetCommandString member previously.
//
//  Parameters in lpci:
//    cbSize -- Specifies the size of this structure (sizeof(*lpci))
//    hwnd   -- Specifies the owner window for any message/dialog box.
//    fMask  -- Specifies whether or not dwHotkey/hIcon paramter is valid.
//    lpVerb -- Specifies the command to be invoked.
//    lpParameters -- Parameters (optional)
//    lpDirectory  -- Working directory (optional)
//    nShow -- Specifies the flag to be passed to ShowWindow (SW_*).
//    dwHotKey -- Hot key to be assigned to the app after invoked (optional).
//    hIcon -- Specifies the icon (optional).
//    hMonitor -- Specifies the default monitor (optional).
//
//
// IContextMenu::GetCommandString
//
//   This member function is called by the explorer either to get the
//  canonical (language independent) command name (uFlags == GCS_VERB) or
//  the help text ((uFlags & GCS_HELPTEXT) != 0) for the specified command.
//  The retrieved canonical string may be passed to its InvokeCommand
//  member function to invoke a command programmatically. The explorer
//  displays the help texts in its status bar; therefore, the length of
//  the help text should be reasonably short (<40 characters).
//
//  Parameters:
//   idCmd -- Specifies menuitem ID offset (from idCmdFirst)
//   uFlags -- Either GCS_VERB or GCS_HELPTEXT
//   pwReserved -- Reserved (must pass NULL when calling, must ignore when called)
//   pszName -- Specifies the string buffer.
//   cchMax -- Specifies the size of the string buffer.
//
//===========================================================================
const
  // QueryContextMenu uFlags
  CMF_EXTENDEDVERBS      = $00000100;      // rarely used verbs


//  CMIC_MASK_HASLINKNAME       = SEE_MASK_HASLINKNAME; //?
//  CMIC_MASK_FLAG_SEP_VDM      = SEE_MASK_FLAG_SEPVDM; //?
//  CMIC_MASK_HASTITLE          = SEE_MASK_HASTITLE;    //?
  CMIC_MASK_NOZONECHECKS      = SEE_MASK_NOZONECHECKS;
  CMIC_MASK_SHIFT_DOWN        = $10000000;
  CMIC_MASK_CONTROL_DOWN      = $40000000;
  CMIC_MASK_FLAG_LOG_USAGE    = SEE_MASK_FLAG_LOG_USAGE;

const
  CSIDL_FLAG_PFTI_TRACKTARGET = $4000; // CSIDL_FLAG_DONT_VERIFY;

// DESCRIPTION: PERSIST_FOLDER_TARGET_INFO
//    This stucture is used for Folder Shortcuts which allow the shell to
// have a file system folder act like another area in the name space.
// One of pidlTargetFolder, szTargetParsingName, or csidl needs to
// specify the destination name space.
//
// pidlTargetFolder: This is a full pidl to the target folder.  Can be NULL in the IPersistFolder3::InitializeEx()
//                   call but not in the GetFolderTargetInfo() return structure.
// szTargetParsingName: Empty string if not specified. Ortherwise, it is the parsible name
//                       to the target.  This name can be parsed by IShellFolder::
//                       ParsedName() from the desktop.
// szNetworkProvider: Can be an empty string.  If not empty, it specifies the type of network
//                    provider that will be used when binding to the target.  This is used
//                    for performance optimizations for the WNet APIs.
// dwAttributes: -1 if not known.  These are the SFGAO_ flags for IShellFolder::GetAttributesOf()
// csidl: This is -1 if it's not used.  This can be used instead of pidlTargetFolder or
//        szTargetParsingName to indicate the TargetFolder.  See the list of CSIDL_ folders
//        below.  CSIDL_FLAG_PFTI_TRACKTARGET means that the IShellFolder's target folder
//        should change if the user changes the target of the underlying CSIDL value.
//        You can also pass CSIDL_FLAG_CREATE to indicate that the target folder
//        should be created if it does not exist.  No other CSIDL_FLAG_* values are supported.

type
  PERSIST_FOLDER_TARGET_INFO = record
    pidlTargetFolder : PItemIdList;                    // pidl for the folder we want to intiailize
    szTargetParsingName : array [0..MAX_PATH-1] of WideChar;  // optional parsing name for the target
    szNetworkProvider : array [0..MAX_PATH-1] of WideChar;    // optional network provider
    dwAttributes : DWORD;                              // optional FILE_ATTRIBUTES_ flags (-1 if not used)
    csidl : integer;                          // optional folder index (SHGetFolderPath()) -1 if not used
  end;
  TPersistFolderTargetInfo = PERSIST_FOLDER_TARGET_INFO;
  PPersistFolderTargetInfo = ^PERSIST_FOLDER_TARGET_INFO;

  IPersistFolder3 = interface(IPersistFolder2)
    [SID_IPersistFolder3]
    function InitializeEx(pbc : IBindCtx; pidlRoot : PItemIdList; const ppfti : TPersistFolderTargetInfo) : HResult; stdcall;
    function GetFolderTargetInfo(var ppfti : TPersistFolderTargetInfo) : HResult; stdcall;
  end;

//===========================================================================
//
// IExtractIcon interface
//
//  This interface is used in two different places in the shell.
//
// Case-1: Icons of sub-folders for the scope-pane of the explorer.
//
//  It is used by the explorer to get the "icon location" of
// sub-folders from each shell folders. When the user expands a folder
// in the scope pane of the explorer, the explorer does following:
//  (1) binds to the folder (gets IShellFolder),
//  (2) enumerates its sub-folders by calling its EnumObjects member,
//  (3) calls its GetUIObjectOf member to get IExtractIcon interface
//     for each sub-folders.
//  In this case, the explorer uses only IExtractIcon::GetIconLocation
// member to get the location of the appropriate icon. An icon location
// always consists of a file name (typically DLL or EXE) and either an icon
// resource or an icon index.
//
//
// Case-2: Extracting an icon image from a file
//
//  It is used by the shell when it extracts an icon image
// from a file. When the shell is extracting an icon from a file,
// it does following:
//  (1) creates the icon extraction handler object (by getting its CLSID
//     under the {ProgID}\shell\ExtractIconHanler key and calling
//     CoCreateInstance requesting for IExtractIcon interface).
//  (2) Calls IExtractIcon::GetIconLocation.
//  (3) Then, calls IExtractIcon::ExtractIcon with the location/index pair.
//  (4) If (3) returns NOERROR, it uses the returned icon.
//  (5) Otherwise, it recursively calls this logic with new location
//     assuming that the location string contains a fully qualified path name.
//
//  From extension programmer's point of view, there are only two cases
// where they provide implementations of IExtractIcon:
//  Case-1) providing explorer extensions (i.e., IShellFolder).
//  Case-2) providing per-instance icons for some types of files.
//
// Because Case-1 is described above, we'll explain only Case-2 here.
//
// When the shell is about display an icon for a file, it does following:
//  (1) Finds its ProgID and ClassID.
//  (2) If the file has a ClassID, it gets the icon location string from the
//    "DefaultIcon" key under it. The string indicates either per-class
//    icon (e.g., "FOOBAR.DLL,2") or per-instance icon (e.g., "%1,1").
//  (3) If a per-instance icon is specified, the shell creates an icon
//    extraction handler object for it, and extracts the icon from it
//    (which is described above).
//
//  It is important to note that the shell calls IExtractIcon::GetIconLocation
// first, then calls IExtractIcon::Extract. Most application programs
// that support per-instance icons will probably store an icon location
// (DLL/EXE name and index/id) rather than an icon image in each file.
// In those cases, a programmer needs to implement only the GetIconLocation
// member and it Extract member simply returns S_FALSE. They need to
// implement Extract member only if they decided to store the icon images
// within files themselved or some other database (which is very rare).
//
//
//
// [Member functions]
//
//
// IExtractIcon::GetIconLocation
//
//  This function returns an icon location.
//
//  Parameters:
//   uFlags     [in]  -- Specifies if it is opened or not (GIL_OPENICON or 0)
//   szIconFile [out] -- Specifies the string buffer buffer for a location name.
//   cchMax     [in]  -- Specifies the size of szIconFile (almost always MAX_PATH)
//   piIndex    [out] -- Sepcifies the address of UINT for the index.
//   pwFlags    [out] -- Returns GIL_* flags
//  Returns:
//   NOERROR, if it returns a valid location; S_FALSE, if the shell use a
//   default icon.
//
//  Notes: The location may or may not be a path to a file. The caller can
//   not assume anything unless the subsequent Extract member call returns
//   S_FALSE.
//
//   if the returned location is not a path to a file, GIL_NOTFILENAME should
//   be set in the returned flags.
//
// IExtractIcon::Extract
//
//  This function extracts an icon image from a specified file.
//
//  Parameters:
//   pszFile [in] -- Specifies the icon location (typically a path to a file).
//   nIconIndex [in] -- Specifies the icon index.
//   phiconLarge [out] -- Specifies the HICON variable for large icon.
//   phiconSmall [out] -- Specifies the HICON variable for small icon.
//   nIconSize [in] -- Specifies the size icon required (size of large icon)
//                     LOWORD is the requested large icon size
//                     HIWORD is the requested small icon size
//  Returns:
//   NOERROR, if it extracted the from the file.
//   S_FALSE, if the caller should extract from the file specified in the
//           location.
//
//===========================================================================

// GetIconLocation() input flags
const
  GIL_DEFAULTICON      = $0040;      // get the default icon location if the final one takes too long to get
  GIL_FORSHORTCUT      = $0080;      // the icon is for a shortcut to the object


//===========================================================================
//
// IShellIconOverlayManager
//
// Used to return the icon overlay information including OverlayIndex, Image Index or Priority for an IShellFolder object
//
// IShellIconOverlayManager:GetFileOverlayInfo(LPCWSTR pwszPath, DWORD dwAttrib, int * pIndex, DWORD dwflags)
//      pwszPath        full path of the file
//      dwAttrib        attribute of this file
//      pIndex          pointer to the Icon Index in the system image list
//      pOverlayIndex   pointer to the OverlayIndex in the system image list
//      pPriority       pointer to the Priority of this overlay
// IShellIconOverlayManager:GetReservedOverlayInfo(LPCWSTR pwszPath, DWORD dwAttrib, int * pIndex, DWORD dwflags, int iReservedID)
//      iReservedID     reserved icon overlay id
//  returns:
//      S_OK,  if the index of an Overlay is found
//      S_FALSE, if no Overlay exists for this file
//      E_FAIL, if lpfd is bad
// IShellIconOverlayManager:RefreshOverlayImages(DWORD dwFlags)
//      This will refresh the overlay cache, depends on the dwFlags passed in
//      It will reload the icons into the imagelist, when passed SIOM_ICONINDEX
// IShellIconOverlayManager::LoadNonloadedOverlayIdentifiers()
//      This method loads any registered overlay identifiers (handlers) that
//      are not currently loaded.
// IShellIconOverlayManager::OverlayIndexFromImageIndex(int iImage, int *piIndex, BOOL fAdd)
//      iImage          existing shell image list index to look for
//      piIndex         returned overlay index
//      fAdd            Add image if not already present?
//===========================================================================
type
  IShellIconOverlayManager = interface
    [SID_IShellIconOverlayManager]
    function GetFileOverlayInfo(pwszPath : LPCWSTR; dwAttrib : DWORD; var pIndex : integer; dwflags : DWORD): HResult; stdcall;
    function GetReservedOverlayInfo(pwszPath : LPCWSTR; dwAttrib : DWORD; var pIndex : integer; dwflags : DWORD; iReservedID : integer): HResult; stdcall;
    function RefreshOverlayImages(dwFlags : DWORD): HResult; stdcall;
    function LoadNonloadedOverlayIdentifiers: HResult; stdcall;
    function OverlayIndexFromImageIndex(iImage : integer; var piIndex : integer; fAdd : bool): HResult; stdcall;
  end;

const
  SIOM_OVERLAYINDEX      = 1;
  SIOM_ICONINDEX         = 2;
//  SIOM_PRIORITY          = 3;
  SIOM_RESERVED_SHARED   = 0;
  SIOM_RESERVED_LINK     = 1;
  SIOM_RESERVED_SLOWFILE = 2;

const
  OI_ASYNC = $FFFFEEEE;

//-------------------------------------------------------------------------
//
// SHGetIconOverlayIndex
//
// This function takes the path and icon/res id to the icon and convert it into
// an overlay index in the system image list.
// Note: there are totally only 15 slots for system image overlays, some of which
// was reserved by the system, or taken by the overlayidentifiers, so it's possible
// that this function would fail and return -1;
//
// To get the default overlays in the system, such as the share hand, link shortcut
// and slow files, pass NULL as the file name, then the IDO_SHGIOI_* flags as the icon index
//-------------------------------------------------------------------------
const
  IDO_SHGIOI_SHARE = $0FFFFFFF;
  IDO_SHGIOI_LINK  = $0FFFFFFE;
  IDO_SHGIOI_SLOWFILE = $0FFFFFFFD;

var
  SHGetIconOverlayIndex : function(pszIconPath : LPCSTR; iIconIndex : integer): integer; stdcall;
  SHGetIconOverlayIndexA : function(pszIconPath : LPCSTR; iIconIndex : integer): integer; stdcall;
  SHGetIconOverlayIndexW : function(pszIconPath : LPCWSTR; iIconIndex : integer) : integer; stdcall;

// IShellLinkDataList::GetFlags()/SetFlags()
const
  SLDF_HAS_ID_LIST         = $00000001;   // Shell link saved with ID list
  SLDF_HAS_LINK_INFO       = $00000002;   // Shell link saved with LinkInfo
  SLDF_HAS_NAME            = $00000004;
  SLDF_HAS_RELPATH         = $00000008;
  SLDF_HAS_WORKINGDIR      = $00000010;
  SLDF_HAS_ARGS            = $00000020;
  SLDF_HAS_ICONLOCATION    = $00000040;
  SLDF_UNICODE             = $00000080;   // the strings are unicode
  SLDF_FORCE_NO_LINKINFO   = $00000100;   // don't create a LINKINFO (make a dumb link)
  SLDF_HAS_EXP_SZ          = $00000200;   // the link contains expandable env strings
  SLDF_RUN_IN_SEPARATE     = $00000400;   // Run the 16-bit target exe in a separate VDM/WOW
  SLDF_HAS_LOGO3ID         = $00000800;   // this link is a special Logo3/MSICD link
  SLDF_HAS_DARWINID        = $00001000;   // this link is a special Darwin link
  SLDF_RUNAS_USER          = $00002000;   // Run this link as a different user
  SLDF_HAS_EXP_ICON_SZ     = $00004000;   // contains expandable env string for icon path
  SLDF_NO_PIDL_ALIAS       = $00008000;   // don't ever resolve to a logical location
  SLDF_FORCE_UNCNAME       = $00010000;   // make GetPath() prefer the UNC name to the local name
  SLDF_RUN_WITH_SHIMLAYER  = $00020000;   // Launch the target of this link w/ shim layer active
  SLDF_RESERVED            = $80000000;   // Reserved-- so we can use the low word as an index value in the future

type
  SHELL_LINK_DATA_FLAGS = DWORD;

  DATABLOCK_HEADER = record
    cbSize : DWORD;             // Size of this extra data block
    dwSignature : DWORD;        // signature of this extra data block
  end;
  LPDATABLOCK_HEADER = ^DATABLOCK_HEADER;
  LPDBLIST = ^DATABLOCK_HEADER;
  TDataBlockHeader = DATABLOCK_HEADER;
  PDataBlockHeader = LPDATABLOCK_HEADER;
  PDBList = LPDBLIST;

  NT_CONSOLE_PROPS = record
    dbh : DATABLOCK_HEADER;
    wFillAttribute : word;         // fill attribute for console
    wPopupFillAttribute : word;    // fill attribute for console popups
    dwScreenBufferSize : COORD;     // screen buffer size for console
    dwWindowSize : COORD;           // window size for console
    dwWindowOrigin : COORD;         // window origin for console
    nFont : DWORD;
    nInputBufferSize : DWORD;
    dwFontSize : COORD;
    uFontFamily : UINT;
    uFontWeight : UINT;
    FaceName : array [0..LF_FACESIZE-1] of WideChar;
    uCursorSize : UINT;
    bFullScreen : BOOL;
    bQuickEdit : BOOL;
    bInsertMode : BOOL;
    bAutoPosition : BOOL;
    uHistoryBufferSize : UINT;
    uNumberOfHistoryBuffers : UINT;
    bHistoryNoDup : BOOL;
    ColorTable : array [0..15] of COLORREF;
  end;
  LPNT_CONSOLE_PROPS = ^NT_CONSOLE_PROPS;
  TNTConsoleProps = NT_CONSOLE_PROPS;
  PNTConsoleProps = LPNT_CONSOLE_PROPS;

const
  NT_CONSOLE_PROPS_SIG = $A0000002;

type
  // This is a FE Console property
  NT_FE_CONSOLE_PROPS = record
    dbh : DATABLOCK_HEADER;
    uCodePage : UINT;
  end;
  LPNT_FE_CONSOLE_PROPS = ^NT_FE_CONSOLE_PROPS;
  TNTFEConsoleProps = NT_FE_CONSOLE_PROPS;
  PNTFEConsoleProps = LPNT_FE_CONSOLE_PROPS;

const
  NT_FE_CONSOLE_PROPS_SIG = $A0000004;

type
  EXP_DARWIN_LINK = record
    dbh : DATABLOCK_HEADER;
    szDarwinID : array [0..MAX_PATH-1] of Char;  // ANSI darwin ID associated with link
    szwDarwinID : array [0..MAX_PATH-1] of WideChar; // UNICODE darwin ID associated with link
  end;
  LPEXP_DARWIN_LINK = ^EXP_DARWIN_LINK;
  TExpDarwinLink = EXP_DARWIN_LINK;
  PExpDarwinLink = LPEXP_DARWIN_LINK;

const
  EXP_DARWIN_ID_SIG = $A0000006;
  EXP_LOGO3_ID_SIG  = $A0000007;

  EXP_SPECIAL_FOLDER_SIG = $A0000005;   // LPEXP_SPECIAL_FOLDER

type
  EXP_SPECIAL_FOLDER = record
    cbSize : DWORD;             // Size of this extra data block
    dwSignature : DWORD;        // signature of this extra data block
    idSpecialFolder : DWORD;    // special folder id this link points into
    cbOffset : DWORD;           // ofset into pidl from SLDF_HAS_ID_LIST for child
  end;
  LPEXP_SPECIAL_FOLDER = ^EXP_SPECIAL_FOLDER;
  TExpSpecialFolder = EXP_SPECIAL_FOLDER;
  PExpSpecialFolder = LPEXP_SPECIAL_FOLDER;

  EXP_SZ_LINK = record
    cbSize : DWORD;             // Size of this extra data block
    dwSignature : DWORD;        // signature of this extra data block
    szTarget : array [0..MAX_PATH - 1] of char;   // ANSI target name w/EXP_SZ in it
    swzTarget : array [0..MAX_PATH - 1] of WideChar;  // UNICODE target name w/EXP_SZ in it
  end;
  LPEXP_SZ_LINK = ^EXP_SZ_LINK;
  TExpSzLink = EXP_SZ_LINK;
  PExpSzLink = LPEXP_SZ_LINK;

const
  EXP_SZ_LINK_SIG = $A0000001;   // LPEXP_SZ_LINK (target)
  EXP_SZ_ICON_SIG = $A0000007;   // LPEXP_SZ_LINK (icon)

type
  IShellLinkDataList = interface
    [SID_IShellLinkDataList]
    function AddDataBlock(pDataBlock : pointer) : HResult; stdcall;
    function CopyDataBlock(dwSig : DWORD; var ppDataBlock : Pointer) : HResult; stdcall;
    function RemoveDataBlock(dwSig : DWORD) : HResult; stdcall;
    function GetFlags(var pdwFlags : DWORD) : HResult; stdcall;
    function SetFlags(dwFlags : DWORD) : HResult; stdcall;
  end;

  IResolveShellLink = interface
    [SID_IResolveShellLink]
    function ResolveShellLink(punk : IUnknown; wnd : HWND; fFlags : DWORD) : HResult; stdcall;
  end;

  ISearchContext = interface
    [SID_ISearchContext]
    function GetSearchUrl(var pbstrSearchUrl : LPWSTR) : HResult; stdcall;
    function GetSearchText(var pbstrSearchText : LPWSTR) : HResult; stdcall;
    function GetSearchStyle(var pdwSearchStyle : DWORD) : HResult; stdcall;
  end;

  IURLSearchHook2 = interface(IURLSearchHook)
    [SID_IURLSearchHook2]
    function TranslateWithSearchContext(lpwszSearchURL : LPWSTR; cchBufferSize : DWORD; pSearchContext : ISearchContext) : HResult; stdcall;
  end;

//==========================================================================
//
// IShellBrowser/IShellView/IShellFolder interface
//
//  These three interfaces are used when the shell communicates with
// name space extensions. The shell (explorer) provides IShellBrowser
// interface, and extensions implements IShellFolder and IShellView
// interfaces.
//
//==========================================================================


//--------------------------------------------------------------------------
//
// Command/menuitem IDs
//
//  The explorer dispatches WM_COMMAND messages based on the range of
// command/menuitem IDs. All the IDs of menuitems that the view (right
// pane) inserts must be in FCIDM_SHVIEWFIRST/LAST (otherwise, the explorer
// won't dispatch them). The view should not deal with any menuitems
// in FCIDM_BROWSERFIRST/LAST (otherwise, it won't work with the future
// version of the shell).
//
//  FCIDM_SHVIEWFIRST/LAST      for the right pane (IShellView)
//  FCIDM_BROWSERFIRST/LAST     for the explorer frame (IShellBrowser)
//  FCIDM_GLOBAL/LAST           for the explorer's submenu IDs
//
//--------------------------------------------------------------------------
const
  PANE_NONE       = -1;
  PANE_ZONE       = 1;
  PANE_OFFLINE    = 2;
  PANE_PRINTER    = 3;
  PANE_SSL        = 4;
  PANE_NAVIGATION = 5;
  PANE_PROGRESS   = 6;
  PANE_PRIVACY    = 7;


//-------------------------------------------------------------------------
// ICommDlgBrowser2 interface
//
// Member functions:
//
//  ICommDlgBrowser2::Notify(IShellView *pshv, DWORD dwNotfyType)
//   Called when the view is wants to notify common dialog when an event
//  occurrs.
//
//  CDB2N_CONTEXTMENU_START indicates the context menu has started.
//  CDB2N_CONTEXTMENU_DONE  indicates the context menu has completed.
//
//  ICommDlgBrowser2::GetDefaultMenuText(IShellView *pshv,
//                                      WCHAR *pszText, INT cchMax)
//   Called when the view wants to get the default context menu text.
//  pszText points to buffer and cchMax specifies the size of the
//  buffer in characters.  The browser on return has filled the buffer
//  with the default context menu text.  The Shell will call this method
//  with at least a buffer size of MAX_PATH.  The browser should return
//  S_OK if it returned a new default menu text, S_FALSE to let the view
//  to use the normal default menu text.
//
//  ICommDlgBrowser2::GetViewFlags(DWORD *pdwFlags)
//     Called when the view wants to determine  if special customization needs to
//    be done for the common dialog browser. For example View calls this function to
//    determin if all files(hidden and system)needs to be shown. If the GetViewFlags returns a DWORD with
//    CDB2GVF_SHOWALLFILES  flag set then it will show all the files.
//-------------------------------------------------------------------------
const
  CDB2N_CONTEXTMENU_DONE  = $00000001;
  CDB2N_CONTEXTMENU_START = $00000002;

  //GetViewFlags
  CDB2GVF_SHOWALLFILES    = $00000001;
type
  ICommDlgBrowser2 = interface(IUnknown)
    [SID_ICommDlgBrowser2]
    function Notify(ppshv : IShellView; dwNotifyType : DWORD): HResult; stdcall;
    function GetDefaultMenuText(ppshv : IShellView; pszText : PWideChar; cchMax : integer): HResult; stdcall;
    function GetViewFlags(var pdwFlags : DWORD): HResult; stdcall;
  end;

{ This function assumes the size of the buffer (MAX_PATH). The pidl
  should point to a file system object. }

function SHCreateDirectory(wnd : HWND; pszPath : LPCWSTR) : integer; stdcall;
function SHCreateDirectoryEx(wnd : HWND; pszPath : LPCSTR; psa : PSECURITYATTRIBUTES) : integer; stdcall;
function SHCreateDirectoryExA(wnd : HWND; pszPath : LPCSTR; psa : PSECURITYATTRIBUTES) : integer; stdcall;
function SHCreateDirectoryExW(wnd : HWND; pszPath : LPCWSTR; psa : PSECURITYATTRIBUTES) : integer; stdcall;

function SHOpenFolderAndSelectItems(pidlFolder : PItemIdList; cidl : uint; var apidl : PItemIdList; dwFlags : DWORD) : HResult; stdcall;
function SHCreateShellItem(pidlParent : PItemIdList; psfParent : IShellFolder; pidl : PItemIdList; var ppsi : IShellItem) : HResult; stdcall;

//-------------------------------------------------------------------------
//
// SHGetSpecialFolderLocation
//
//  Caller should use SHGetMalloc to obtain an allocator that can free the pidl
//
//
//-------------------------------------------------------------------------
//
const
{ registry entries for special paths are kept in : }
  CSIDL_MYDOCUMENTS                   = $000c; // logical "My Documents" desktop icon
  CSIDL_MYMUSIC                       = $000d; // "My Music" folder
  CSIDL_MYVIDEO                       = $000e; // "My Videos" folder
  CSIDL_LOCAL_APPDATA                 = $001c; // \Local Settings\Applicaiton Data (non roaming)

  CSIDL_COMMON_APPDATA            = $0023;   // All Users\Application Data
  CSIDL_WINDOWS                   = $0024;   // GetWindowsDirectory()
  CSIDL_SYSTEM                    = $0025;   // GetSystemDirectory()
  CSIDL_PROGRAM_FILES             = $0026;   // C:\Program Files
  CSIDL_MYPICTURES                = $0027;   // C:\Program Files\My Pictures
  CSIDL_PROFILE                   = $0028;        // USERPROFILE
  CSIDL_SYSTEMX86                 = $0029;        // x86 system directory on RISC
  CSIDL_PROGRAM_FILESX86          = $002a;        // x86 C:\Program Files on RISC

  CSIDL_PROGRAM_FILES_COMMON      = $002b;        // C:\Program Files\Common
  CSIDL_PROGRAM_FILES_COMMONX86   = $002c;        // x86 Program Files\Common on RISC
  CSIDL_COMMON_TEMPLATES          = $002d;        // All Users\Templates

  CSIDL_COMMON_DOCUMENTS          = $002e;        // All Users\Documents
  CSIDL_COMMON_ADMINTOOLS         = $002f;        // All Users\Start Menu\Programs\Administrative Tools
  CSIDL_ADMINTOOLS                = $0030;        // \Start Menu\Programs\Administrative Tools
  CSIDL_CONNECTIONS               = $0031;        // Network and Dial-up Connections
  CSIDL_COMMON_MUSIC              = $0035;        // All Users\My Music
  CSIDL_COMMON_PICTURES           = $0036;        // All Users\My Pictures
  CSIDL_COMMON_VIDEO              = $0037;        // All Users\My Video
  CSIDL_RESOURCES                 = $0038;        // Resource Direcotry

  CSIDL_RESOURCES_LOCALIZED       = $0039;        // Localized Resource Direcotry

  CSIDL_COMMON_OEM_LINKS          = $003a;        // Links to All Users OEM specific apps
  CSIDL_CDBURN_AREA               = $003b;        // USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning

  CSIDL_COMPUTERSNEARME           = $003d;        // Computers Near Me (computered from Workgroup membership)

  CSIDL_FLAG_CREATE               = $8000;        // combine with CSIDL_ value to force folder creation in SHGetFolderPath()

  CSIDL_FLAG_DONT_VERIFY          = $4000;        // combine with CSIDL_ value to return an unverified folder path
  CSIDL_FLAG_NO_ALIAS             = $1000;        // combine with CSIDL_ value to insure non-alias versions of the pidl
  CSIDL_FLAG_PER_USER_INIT        = $0800;        // combine with CSIDL_ value to indicate per-user init (eg. upgrade)
  CSIDL_FLAG_MASK                 = $FF00;        // mask for all possible flag values

procedure SHFlushSFCache; stdcall;
function SHCloneSpecialIDList(wnd : HWND; csidl : integer; fCreate : bool) : PItemIdList; stdcall;

const
  SHGFP_TYPE_CURRENT  = 0;   // current value for user, verify it exists
  SHGFP_TYPE_DEFAULT  = 1;   // default value, may not exist

function SHGetFolderPath(wnd : HWND; csidl : integer; hToken : THANDLE; dwFlags : dword; pszPath : LPSTR) : HResult; stdcall;
function SHGetFolderPathA(wnd : HWND; csidl : integer; hToken : THANDLE; dwFlags : dword; pszPath : LPSTR) : HResult; stdcall;
function SHGetFolderPathW(wnd : HWND; csidl : integer; hToken : THANDLE; dwFlags : dword; pszPath : LPWSTR) : HResult; stdcall;

function SHGetFolderLocation(wnd : HWND; csidl : integer; hToken : THandle; dwFlags : DWord; var ppidl : PItemIdList) : HResult; stdcall;

function SHGetFolderPathAndSubDir(wnd : HWND; csidl : integer; hToken : THandle; dwFlags : DWord; pszSubDir : LPCSTR; pszPath : LPSTR) : HResult; stdcall;
function SHGetFolderPathAndSubDirA(wnd : HWND; csidl : integer; hToken : THandle; dwFlags : DWord; pszSubDir : LPCSTR; pszPath : LPSTR) : HResult; stdcall;
function SHGetFolderPathAndSubDirW(wnd : HWND; csidl : integer; hToken : THandle; dwFlags : DWord; pszSubDir : LPCWSTR; pszPath : LPWSTR) : HResult; stdcall;

const
  FCS_READ        = $00000001;
  FCS_FORCEWRITE  = $00000002;
  FCS_WRITE       = (FCS_READ or FCS_FORCEWRITE);

  FCS_FLAG_DRAGDROP = 2;

  FCSM_VIEWID          = $00000001;
  FCSM_WEBVIEWTEMPLATE = $00000002;
  FCSM_INFOTIP         = $00000004;
  FCSM_CLSID           = $00000008;
  FCSM_ICONFILE        = $00000010;
  FCSM_LOGO            = $00000020;
  FCSM_FLAGS           = $00000040;

type
  SHFOLDERCUSTOMSETTINGSA = record
    dwSize : dword;
    dwMask : dword;             // IN/OUT   Which Attributes to Get/Set
    pvid : PSHELLVIEWID;               // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    // The folder's WebView template path
    pszWebViewTemplate : LPSTR;  // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    cchWebViewTemplate : DWORD;  // IN - Specifies the size of the buffer pointed to by pszWebViewTemplate
                                // Ignored if dwReadWrite is FCS_READ
    pszWebViewTemplateVersion : LPSTR;  // currently IN only
    // Infotip for the folder
    pszInfoTip : LPSTR;         // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    cchInfoTip : DWORD;         // IN - Specifies the size of the buffer pointed to by pszInfoTip
                                // Ignored if dwReadWrite is FCS_READ
    // CLSID that points to more info in the registry
    pclsid : PCLSID;             // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    // Other flags for the folder. Takes FCS_FLAG_* values
    dwFlags : DWORD;            // OUT - if dwReadWrite is FCS_READ, IN - otherwise

    pszIconFile : LPSTR;        // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    cchIconFile : DWORD;        // IN - Specifies the size of the buffer pointed to by pszIconFile
                                // Ignored if dwReadWrite is FCS_READ

    iIconIndex : integer;         // OUT - if dwReadWrite is FCS_READ, IN - otherwise

    pszLogo : LPSTR;        // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    cchLogo : DWORD;        // IN - Specifies the size of the buffer pointed to by pszIconFile
                            // Ignored if dwReadWrite is FCS_READ
  end;
  LPSHFOLDERCUSTOMSETTINGSA = ^SHFOLDERCUSTOMSETTINGSA;
  TShFolderCustomSettingsA = SHFOLDERCUSTOMSETTINGSA;
  PShFolderCustomSettingsA = LPSHFOLDERCUSTOMSETTINGSA;

  TShFolderCustomSettings = TShFolderCustomSettingsA;
  PShFolderCustomSettings = PShFolderCustomSettingsA;

  SHFOLDERCUSTOMSETTINGSW = record
    dwSize : dword;
    dwMask : dword;             // IN/OUT   Which Attributes to Get/Set
    pvid : PSHELLVIEWID;               // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    // The folder's WebView template path
    pszWebViewTemplate : LPWSTR;  // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    cchWebViewTemplate : DWORD;  // IN - Specifies the size of the buffer pointed to by pszWebViewTemplate
                                // Ignored if dwReadWrite is FCS_READ
    pszWebViewTemplateVersion : LPWSTR;  // currently IN only
    // Infotip for the folder
    pszInfoTip : LPWSTR;         // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    cchInfoTip : DWORD;         // IN - Specifies the size of the buffer pointed to by pszInfoTip
                                // Ignored if dwReadWrite is FCS_READ
    // CLSID that points to more info in the registry
    pclsid : PCLSID;             // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    // Other flags for the folder. Takes FCS_FLAG_* values
    dwFlags : DWORD;            // OUT - if dwReadWrite is FCS_READ, IN - otherwise

    pszIconFile : LPWSTR;        // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    cchIconFile : DWORD;        // IN - Specifies the size of the buffer pointed to by pszIconFile
                                // Ignored if dwReadWrite is FCS_READ

    iIconIndex : integer;         // OUT - if dwReadWrite is FCS_READ, IN - otherwise

    pszLogo : LPWSTR;        // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    cchLogo : DWORD;        // IN - Specifies the size of the buffer pointed to by pszIconFile
                            // Ignored if dwReadWrite is FCS_READ
  end;
  LPSHFOLDERCUSTOMSETTINGSW = ^SHFOLDERCUSTOMSETTINGSW;
  TShFolderCustomSettingsW = SHFOLDERCUSTOMSETTINGSW;
  PShFolderCustomSettingsW = LPSHFOLDERCUSTOMSETTINGSW;

function SHGetSetFolderCustomSettings(pfcs : PSHFOLDERCUSTOMSETTINGS; pszPath : LPCSTR; dwReadWrite : DWORD) : HResult; stdcall;
function SHGetSetFolderCustomSettingsA(pfcs : PSHFOLDERCUSTOMSETTINGSA; pszPath : LPCSTR; dwReadWrite : DWORD) : HResult; stdcall;
function SHGetSetFolderCustomSettingsW(pfcs : PSHFOLDERCUSTOMSETTINGSW; pszPath : LPCWSTR; dwReadWrite : DWORD) : HResult; stdcall;

{ SHBrowseForFolder API }

const
  { Browsing for directory. }
  BIF_UAHINT             = $0100;   // Add a UA hint to the dialog, in place of the edit box. May not be combined with BIF_EDITBOX
  BIF_NONEWFOLDERBUTTON  = $0200;   // Do not add the "New Folder" button to the dialog.  Only applicable with BIF_NEWDIALOGSTYLE.
  BIF_NOTRANSLATETARGETS = $0400;   // don't traverse target as shortcut

  { message from browser }
  BFFM_IUNKNOWN          = 5;   // provides IUnknown to client. lParam: IUnknown*

  { messages to browser }
  BFFM_SETOKTEXT              = WM_USER + 105; // Unicode only
  BFFM_SETEXPANDED            = WM_USER + 106; // Unicode only


//-------------------------------------------------------------------------
//
// SHEnableServiceObject
//
//   Like SHLoadInProc, but gives control over the object's lifetime
//  via fEnable parameter.  TRUE tells the shell to create the object
//  and hold onto it, FALSE tells the shell to look for the previously
//  created instance of the object and release it.
//
//-------------------------------------------------------------------------
function SHEnableServiceObject(rclsid : TClsid; fEnable : BOOL): HRESULT; stdcall;

//-------------------------------------------------------------------------
//
// Internet Shortcut Object
//
//-------------------------------------------------------------------------
// Cmds for CGID_ShortCut
const
  ISHCUTCMDID_DOWNLOADICON  = 0;
  ISHCUTCMDID_INTSHORTCUTCREATE = 1;

  CMDID_INTSHORTCUTCREATE = ISHCUTCMDID_INTSHORTCUTCREATE;

// IShellFolder IBindCtx* parameters. the IUnknown for these are
// accessed through IBindCtx::RegisterObjectParam/GetObjectParam
// use this to provide the data needed create IDLists through
// IShellFolder::ParseDisplayName(). this data applies to the last element
// of the name that is parsed (c:\foo\bar.txt, data applies to bar.txt)
// this makes creating these IDLists much faster that suppling the name only
const
  STR_FILE_SYS_BIND_DATA      = 'File System Bind Data';

type
  IFileSystemBindData = interface
    [SID_IFileSystemBindData]
    function SetFindData(const pfd : WIN32_FIND_DATAW) : HResult; stdcall;
    function GetFindData(var pfd : WIN32_FIND_DATAW) : HResult; stdcall;
  end;

//-------------------------------------------------------------------------
//
// IObjMgr interface
//
//
// [Member functions]
//
// IObjMgr::Append(punk)
//   This function adds an object to the end of a list of objects.
//
// IObjMgr::Remove(punk)
//   This function removes an object from a list of objects.
//
// This is implemented by CLSID_ACLMulti so each AutoComplete List
// (CLSID_ACLHistory, CLSID_ACListISF, CLSID_ACLMRU) can be added.
// CLSID_ACLMulti's IEnumString will then be the union of the results
// from the COM Objects added.
//-------------------------------------------------------------------------
type
  IObjMgr = interface
    [SID_IObjMgr]
    function Append(punk : IUnknown) : HResult; stdcall;
    function Remove(punk : IUnknown) : HResult; stdcall;
  end;

//-------------------------------------------------------------------------
//
// ICurrentWorkingDirectory interface
//
//
// [Member functions]
//
// ICurrentWorkingDirectory::GetDirectory(LPWSTR pwzPath, DWORD cchSize)
//   This function gets the Current Working Directory from a COM object that
//   stores such state.
//
// ICurrentWorkingDirectory::SetDirectory(LPCWSTR pwzPath)
//   This function sets the Current Working Directory of a COM object that
//   stores such state.
//
// This function can be used generically.  One COM object that implements it
// is CLSID_ACListISF so that the AutoComplete engine can complete relative
// paths.  SetDirectory() will set the "Current Working Directory" and
// AutoComplete with then complete both absolute and relative paths.
// For Example, if ::SetDirectory(L"C:\Program Files") is called, then
// the user can AutoComplete "..\winnt".  In order to set the current
// working directory for non-file system paths, "ftp://ftp.microsoft.com/" or
// "Control Panel" for example, use IPersistFolder.
//-------------------------------------------------------------------------
type
  ICurrentWorkingDirectory = interface
    [SID_ICurrentWorkingDirectory]
    function GetDirectory(pwzPath : LPWSTR; cchSize : DWORD) : HResult; stdcall;
    function SetDirectory(pwzPath : LPCWSTR) : HResult; stdcall;
  end;
//-------------------------------------------------------------------------
//
// IACList interface
//
//
// [Member functions]
//
// IObjMgr::Expand(LPCOLESTR)
//   This function tells an autocomplete list to expand a specific string.
//
// If the user enters a multi-level path, AutoComplete (CLSID_AutoComplete)
// will use this interface to tell the "AutoComplete Lists" where to expand
// the results.
//
// For Example, if the user enters "C:\Program Files\Micros", AutoComplete
// first completely enumerate the "AutoComplete Lists" via IEnumString.  Then it
// will call the "AutoComplete Lists" with IACList::Expand(L"C:\Program Files").
// It will then enumerate the IEnumString interface again to get results in
// that directory.
//-------------------------------------------------------------------------
type
  IACList = interface
    [SID_IACList]
    function Expand(pszExpand : POLESTR) : HResult; stdcall;
  end;

//-------------------------------------------------------------------------
//
// IACList2 interface
//
// [Description]
//              This interface exists to allow the caller to set filter criteria
// for an AutoComplete List.  AutoComplete Lists generates the list of
// possible AutoComplete completions.  CLSID_ACListISF is one AutoComplete
// List COM object that implements this interface.
//-------------------------------------------------------------------------
const
  ACLO_NONE            = 0;    // don't enumerate anything
  ACLO_CURRENTDIR      = 1;    // enumerate current directory
  ACLO_MYCOMPUTER      = 2;    // enumerate MyComputer
  ACLO_DESKTOP         = 4;    // enumerate Desktop Folder
  ACLO_FAVORITES       = 8;    // enumerate Favorites Folder
  ACLO_FILESYSONLY     = 16;   // enumerate only the file system
  ACLO_FILESYSDIRS     = 32;   // enumerate only the file system dirs, UNC shares, and UNC servers.

type
  AUTOCOMPLETELISTOPTIONS = DWORD;

  IACList2 = interface
    [SID_IACList2]
    // *** IACList2 specific methods ***
    function SetOptions(dwFlag : DWORD) : HResult; stdcall;
    function GetOptions(var pdwFlag : DWORD) : HResult; stdcall;
  end;

(*-------------------------------------------------------------------------
    INTERFACE: IProgressDialog

    DESCRIPTION:
        CLSID_ProgressDialog/IProgressDialog exist to allow a caller to create
    a progress dialog, set it's title, animation, text lines, progress, and
    it will do all the work of updating on a background thread, being modless,
    handling the user cancelling the operation, and estimating the time remaining
    until the operation completes.

    USAGE:
        This is how the dialog is used during operations that require progress
    and the ability to cancel:
    {
        DWORD dwComplete, dwTotal;
        IProgressDialog * ppd;
        CoCreateInstance(CLSID_ProgressDialog, NULL, CLSCTX_INPROC_SERVER, IID_IProgressDialog, (void ** )&ppd);
        ppd->SetTitle(L"My Slow Operation");                                // Set the title of the dialog.
        ppd->SetAnimation(hInstApp, IDA_OPERATION_ANIMATION);               // Set the animation to play.
        ppd->StartProgressDialog(hwndParent, punk, PROGDLG_AUTOTIME, NULL); // Display and enable automatic estimated time remaining.
        ppd->SetCancelMsg(L"Please wait while the current operation is cleaned up", NULL);   // Will only be displayed if Cancel button is pressed.

        dwComplete = 0;
        dwTotal = CalcTotalUnitsToDo();

        // Reset because CalcTotalUnitsToDo() took a long time and the estimated time
        // is based on the time between ::StartProgressDialog() and the first
        // ::SetProgress() call.
        ppd->Timer(PDTIMER_RESET, NULL);

        for (nIndex = 0; nIndex < nTotal; nIndex++)
        {
            if (TRUE == ppd->HasUserCancelled())
                break;

            ppd->SetLine(2, L"I'm processing item n", FALSE, NULL);
            dwComplete += DoSlowOperation();

            ppd->SetProgress(dwCompleted, dwTotal);
        }

        ppd->StopProgressDialog();
        ppd->Release();
    }
-------------------------------------------------------------------------*)
// Flags for IProgressDialog::StartProgressDialog() (dwFlags)
const
  PROGDLG_NORMAL        =  $00000000;      // default normal progress dlg behavior
  PROGDLG_MODAL         =  $00000001;      // the dialog is modal to its hwndParent (default is modeless)
  PROGDLG_AUTOTIME      =  $00000002;      // automatically updates the "Line3" text with the "time remaining" (you cant call SetLine3 if you passs this!)
  PROGDLG_NOTIME        =  $00000004;      // we dont show the "time remaining" if this is set. We need this if dwTotal < dwCompleted for sparse files
  PROGDLG_NOMINIMIZE    =  $00000008;      // Do not have a minimize button in the caption bar.
  PROGDLG_NOPROGRESSBAR =  $00000010;      // Don't display the progress bar

  // Time Actions (dwTimerAction)
  PDTIMER_RESET     = $00000001;       // Reset the timer so the progress will be calculated from now until the first ::SetProgress() is called so

type                                       // those this time will correspond to the values passed to ::SetProgress().  Only do this before ::SetProgress() is called.
  IProgressDialog = interface
    [SID_IProgressDialog]
    function StartProgressDialog(hwndParent : HWND; punkEnableModless : IUnknown; dwFlags : DWORD; pvResevered : Pointer) : HResult; stdcall;
    function StopProgressDialog : HResult; stdcall;
    function SetTitle(pwzTitle : LPCWSTR) : HResult; stdcall;
    function SetAnimation(hInstAnimation : THandle; idAnimation : UINT) : HResult; stdcall;
    function HasUserCancelled : BOOL; stdcall;
    function SetProgress(dwCompleted : DWORD; dwTotal : DWORD) : HResult; stdcall;
    function SetProgress64(ullCompleted : int64; ullTotal : int64) : HResult; stdcall;
    function SetLine(dwLineNum : DWORD; pwzString : LPCWSTR; fCompactPath : BOOL; pvResevered : Pointer) : HResult; stdcall;
    function SetCancelMsg(pwzCancelMsg : LPCWSTR; pvResevered : Pointer) : HResult; stdcall;
    function Timer(dwTimerAction : DWORD; pvResevered : Pointer) : HResult; stdcall;
  end;

//-------------------------------------------------------------------------
//
// IRunnableTask interface
//
//   This is a free threaded interface used for putting items on a background
// scheduler for execution within the view.  It allows a scheduler to start and
// stop tasks on as many worker threads as it deems necessary.
//
// Run(), Kill() and Suspend() may be called from different threads.
//
// [Member functions]
//
// IRunnableTask::Run(void)
//   Initiate the task to run.  This should return E_PENDING if the task
//   has been suspended.
//
// IRunnableTask::Kill(void)
//
// IRunnableTask::Suspend(void)
//
// IRunnableTask::Resume(void)
//
// IRunnableTask::IsRunning(void)
//
//-------------------------------------------------------------------------
const
  // Convenient state values
  IRTIR_TASK_NOT_RUNNING = 0;
  IRTIR_TASK_RUNNING     = 1;
  IRTIR_TASK_SUSPENDED   = 2;
  IRTIR_TASK_PENDING     = 3;
  IRTIR_TASK_FINISHED    = 4;

type
  IRunnableTask = interface
    [SID_IRunnableTask]
    function Run: HResult; stdcall;
    function Kill(fWait : BOOL): HResult; stdcall;
    function Suspend: HResult; stdcall;
    function Resume: HResult; stdcall;
    function IsRunning : ULONG; stdcall;
  end;

// ---IShellTaskScheduler
// An interface for interacting with and controlling a task scheduler. This
// interface does not need to be free-threaded unless the items in the queue
// interact with the scheduler as well as the main execution thread on which the
// task scheduler was created.

// IShellTaskScheduler::AddTask()
//      Adds Tasks to the scheduler's background queue. The TASKOWNERID allow particular types
//      of tasks to be grouped so that they can be counted or removed. The lParam allows the task
//      to be associated with a particular item (for example an item in a listview).
// IShellTaskScheduler::RemoveTasks()
//      Removes tasks from the scheduler's queue. These can be sepcified in terms of their TASKOWNERID
//      or their LPARAM, or both, or neither (TOID_NULL && ITSAT_DEFAULT_LPARAM results in all tasks being
//      removed). If a task that matches is currently running and ITaskScheduler::Status() has been
//      passeed ITSSFLAG_KILL_ON_DESTROY then the scheduler will attempt to kill the current task. The
//      fWaitIfRunning parameter is then passed to IRunnableTask::Kill().
// IShellTaskScheduler::CountTasks()
//      Counts the tasks in the queue depending upon the TASKOWNERID and the LPARAM passed. (TOID_NULL and
//      ITSAT_DEFAULT_LPARAM will count all tasks in the queue)
// IShellTaskScheduler::Status()
//      This sets the ReleaseStatus for the current task and the background thread timeout. When
//      ITaskScheduler::RemoveTasks() is called and there is a task currently running that matches
//      ITSSFLAG_COMPLETE_ON_DESTROY will cause TRUE to be passed to the task's IRunnableTask::Kill().
//      The dwThreadTimeout parameter if not set to the default will cause the background thread to
//      die if no new tasks have been added to the queue in the timeout period. The Thread will be
//      recreated when the next new task is added.

////////////////////////
const
  // Status() flags,
  // wait for the current task to complete before deleting the scheduler
  ITSSFLAG_COMPLETE_ON_DESTROY = $0000;

  // kill the current task (if there is one) when the task scheduler is deleted
  ITSSFLAG_KILL_ON_DESTROY = $0001;

  ITSSFLAG_SUPPORTS_TERMINATE = $0002;

  ITSSFLAG_FLAGS_MASK = $0003;

  // set the timeout for killing the thread when the object is terminated.
  // this timeout can be used to stop the object from blocking the system
  // indefinitely.
  ITSSFLAG_THREAD_TERMINATE_TIMEOUT = $0010;

  // set the timeout for threads that are idle in the thread pool
  ITSSFLAG_THREAD_POOL_TIMEOUT = $0020;

  // The default timeout passed to release Status to determine how long the thread
  // can be asleep before the thread is expired
  ITSS_THREAD_DESTROY_DEFAULT_TIMEOUT = 60*1000;

  // default, we won't kill it...
  ITSS_THREAD_TERMINATE_TIMEOUT = INFINITE;

  // there is no change to the thread timeout
  ITSS_THREAD_TIMEOUT_NO_CHANGE = INFINITE - 1;

  // the LPARAM allows task to be associated with items thus all tasks owned by a
  // particular item can be accessed by passing a non default value for this parameter
  ITSAT_DEFAULT_LPARAM  = $ffffffff;

type
  IShellTaskScheduler = interface
    ['{148BD527-A2AB-11CE-B11F-00AA00530503}']
    function AddTask(pTask : IRunnableTask; rtoid : TIID; var lParam : DWORD; dwPriority : DWORD) : HResult; stdcall;
    function RemoveTasks(rtoid : TIID; var lParam : DWORD; fWaitIfRunning : bool) : HResult; stdcall;
    function CountTasks(rtoid : TIID) : UINT; stdcall;
    function Status(dwReleaseStatus, dwThreadTimeout : DWORD) : HResult; stdcall;
  end;

const
  ITSSFLAG_TASK_PLACEINFRONT = $00000001;
  ITSSFLAG_TASK_PLACEINBACK  = $00000002;

type
  IShellTaskScheduler2 = interface(IShellTaskScheduler)
//    [SID_IShellTaskScheduler2] ??????
    function AddTask2(pTask : IRunnableTask; rtoid : TIID; var lParam : DWORD; dwPriority : DWORD; grfFlags : DWORD) : HResult; stdcall;
    function MoveTask(rtoid : TIID; var lParam : DWORD; dwPriority : DWORD; grfFlags : DWORD) : HResult; stdcall;
  end;

//* ***************** IThumbnailCapture
//* CaptureThumbnail : takes an IHTMLDocument2 and returns a thumbnail of specified
//*                    size as an hbitmap
type
  IThumbnailCapture = interface
    [SID_IThumbnailCapture]
    function CaptureThumbnail(const pMaxSize : SIZE; pHTMLDoc2 : IUnknown; var phbmThumbnail : HBitmap) : HResult; stdcall;
  end;

type
  ENUMSHELLIMAGESTOREDATA = record
    szPath : array [0..MAX_PATH - 1] of WideChar;
    ftTimeStamp : TFileTime;
  end;
  LPENUMSHELLIMAGESTOREDATA = ^ENUMSHELLIMAGESTOREDATA;
  TEnumShellImageStoreData = ENUMSHELLIMAGESTOREDATA;
  PEnumShellImageStoreData = LPENUMSHELLIMAGESTOREDATA;

  IEnumShellImageStore = interface
    [SID_IEnumShellImageStore]
    function Reset: HResult; stdcall;
    function Next(celt: ULONG; out rgelt: PEnumShellImageStoreData; var pceltFetched: ULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Clone(out ppenum: IEnumShellImageStore): HResult; stdcall;
  end;

const
  SHIMSTCAPFLAG_LOCKABLE  = $0001;   // does the store require/support locking
  SHIMSTCAPFLAG_PURGEABLE = $0002;   // does the store require dead items purging externally ?
type
  IShellImageStore = interface
    [SID_IShellImageStore]
    function Open(dwMode : DWORD; var pdwLock : DWORD): HResult; stdcall;
    function Create(dwMode : DWORD; pdwLock : DWORD): HResult; stdcall;
    // if the lock is passed to either of these two methods, it releases the lock
    // once the operation is complete.
    function ReleaseLock(const pdwLock : DWORD): HResult; stdcall;
    function Close(const pdwLock : DWORD): HResult; stdcall;
    function Commit(const pdwLock : DWORD): HResult; stdcall;
    function IsLocked: HResult; stdcall;
    function GetMode(var pdwMode : DWORD): HResult; stdcall;
    function GetCapabilities(var pdwCapMask : DWORD): HResult; stdcall;
    function AddEntry(pszName : LPCWSTR; const pftTimeStamp : TFileTime; dwMode : DWORD; hImage : HBITMAP): HResult; stdcall;
    function GetEntry(pszName : LPCWSTR; dwMode : DWORD; var phImage : HBITMAP): HResult; stdcall;
    function DeleteEntry(pszName : LPCWSTR): HResult; stdcall;
    function IsEntryInStore(pszName : LPCWSTR; var pftTimeStamp : TFiletime): HResult; stdcall;
    function Enum(ppEnum : IEnumShellImageStore): HResult; stdcall;
  end;

//  IShellFolderBand
const
  // Field mask
  ISFB_MASK_STATE          = $00000001; // TRUE if dwStateMask and dwState is valid
  ISFB_MASK_BKCOLOR        = $00000002; // TRUE if crBkgnd field is valid
  ISFB_MASK_VIEWMODE       = $00000004; // TRUE if wViewMode field is valid
  ISFB_MASK_SHELLFOLDER    = $00000008;
  ISFB_MASK_IDLIST         = $00000010;
  ISFB_MASK_COLORS         = $00000020; // TRUE if crXXXX fields are valid (except bkgnd)

  ISFB_STATE_DEFAULT       = $00000000;
  ISFB_STATE_DEBOSSED      = $00000001;
  ISFB_STATE_ALLOWRENAME   = $00000002;
  ISFB_STATE_NOSHOWTEXT    = $00000004; // TRUE if _fNoShowText
  ISFB_STATE_CHANNELBAR    = $00000010; // TRUE if we want NavigateTarget support
  ISFB_STATE_QLINKSMODE    = $00000020; // TRUE if we want to turn off drag & drop onto content items
  ISFB_STATE_FULLOPEN      = $00000040; // TRUE if band should maximize when opened
  ISFB_STATE_NONAMESORT    = $00000080; // TRUE if band should _not_ sort icons by name
  ISFB_STATE_BTNMINSIZE    = $00000100; // TRUE if band should report min thickness of button

  ISFBVIEWMODE_SMALLICONS   = $0001;
  ISFBVIEWMODE_LARGEICONS   = $0002;
  ISFBVIEWMODE_LOGOS        = $0003;

type
  BANDINFOSFB = record
    dwMask : DWORD;       // [in] ISFB_MASK mask of valid fields from crBkgnd on
    dwStateMask : DWORD;  // [in] ISFB_STATE mask of dwState bits being set/queried
    dwState : DWORD;      // [in/out] ISFB_STATE bits
    crBkgnd : COLORREF;      // [in/out]
    crBtnLt: COLORREF;      // [in/out]
    crBtnDk : COLORREF;      // [in/out]
    wViewMode : WORD;    // [in/out]
    wAlign : WORD;       // not used (yet)
    psf : IShellFolder;       // [out]
    pidl : PItemIdList;      // [out]
  end;
  PBANDINFOSFB = ^BANDINFOSFB;
  TBandInfoSB = BANDINFOSFB;

  IShellFolderBand = interface
    [SID_IShellFolderBand]
    function InitializeSFB(psf : IShellFolder; pidl : PItemIdList): HResult; stdcall;
    function SetBandInfoSFB(pbi : PBANDINFOSFB): HResult; stdcall;
    function GetBandInfoSFB(pbi : PBANDINFOSFB): HResult; stdcall;
  end;

const
  SFBID_PIDLCHANGED = 0;

type
  IDeskBarClient = interface(IOleWindow)
//    [SID_IDeskBarClient]
    function SetDeskBarSite(pUnkSite : IUnknown): HResult; stdcall;
    function SetModeDBC(dwMode : DWORD): HResult; stdcall;
    function UIActivateDBC(dwState : DWORD): HResult; stdcall;
    function GetSize(dwWhich : DWORD; var prc : TRect): HResult; stdcall;
  end;

const
  DBC_GS_IDEAL = 0;  // get the ideal size
  DBC_GS_SIZEDOWN = 1;  // clip the height of a rect to a multiple of the rebar's integral size

  DBC_HIDE = 0; // Band is hidden (being destroyed)
  DBC_SHOW        = 1; // Band is visible
  DBC_SHOWOBSCURE = 2; // Band is completely obscured

  DBCID_EMPTY = 0;        // bandsite is empty
  DBCID_ONDRAG = 1;       // (down)DragMoveEnter/Leave vaIn:I4:eDrag
  DBCID_CLSIDOFBAR = 2;   // clsid of bar inside
  DBCID_RESIZE = 3;       // resize from keyboard
  DBCID_GETBAR = 4;       // returns vaOut:VT_UNKNOWN of hosting dockbar (IDeskBar)

type
  COMPSTATEINFO = packed record
    dwSize : dword;             // Size of this structure.
    iLeft : integer;              // Left of the top-left corner in screen co-ordinates.
    iTop : integer;               // Top of top-left corner in screen co-ordinates.
    dwWidth : DWORD;            // Width in pixels.
    dwHeight : DWORD;           // Height in pixels.
    dwItemState : DWORD;        // State of the component (full-screen mode or split-screen or normal state.
  end;
  TCompStateInfo = COMPSTATEINFO;
  PCompStateInfo = ^COMPSTATEINFO;

const
  IS_NORMAL              = $00000001;
  IS_FULLSCREEN          = $00000002;
  IS_SPLIT               = $00000004;
  IS_VALIDSIZESTATEBITS  = (IS_NORMAL or IS_SPLIT or IS_FULLSCREEN);  // The set of IS_* state bits which define the "size" of the component - these bits are mutually exclusive.
  IS_VALIDSTATEBITS      = (IS_NORMAL or IS_SPLIT or IS_FULLSCREEN or $80000000 or $40000000);  // All of the currently defined IS_* bits.

  // Flags for IActiveDesktop::ApplyChanges()
  AD_APPLY_DYNAMICREFRESH = $00000020;

  // Flags for IActiveDesktop::ModifyComponent()
  COMP_ELEM_ORIGINAL_CSI  = $00001000;
  COMP_ELEM_RESTORED_CSI  = $00002000;
  COMP_ELEM_CURITEMSTATE  = $00004000;

  COMP_ELEM_ALL           = COMP_ELEM_TYPE or COMP_ELEM_CHECKED or
                            COMP_ELEM_DIRTY or COMP_ELEM_NOSCROLL or
                            COMP_ELEM_POS_LEFT or COMP_ELEM_SIZE_WIDTH  or
                            COMP_ELEM_SIZE_HEIGHT or COMP_ELEM_POS_ZINDEX or
                            COMP_ELEM_SOURCE or COMP_ELEM_FRIENDLYNAME or
                            COMP_ELEM_POS_TOP or COMP_ELEM_SUBSCRIBEDURL or
                            COMP_ELEM_ORIGINAL_CSI or COMP_ELEM_RESTORED_CSI or
                            COMP_ELEM_CURITEMSTATE;


type
  // Flags for IActiveDesktop::AddDesktopItemWithUI()
  DTI_ADTIWUI = (DTI_ADDUI_DEFAULT,DTI_ADDUI_DISPSUBWIZARD,DTI_ADDUI_POSITIONITEM);

const
  // Flags for IActiveDesktop::AddUrl()
  COMPONENT_DEFAULT_LEFT = $FFFF;
  COMPONENT_DEFAULT_TOP  = $FFFF;

const
  // Flags for SetSafeMode
  SSM_CLEAR   = $0000;
  SSM_SET     = $0001;
  SSM_REFRESH = $0002;
  SSM_UPDATE  = $0004;

  SCHEME_DISPLAY  = $0001;
  SCHEME_EDIT     =  $0002;
  SCHEME_LOCAL    =  $0004;
  SCHEME_GLOBAL   =  $0008;
  SCHEME_REFRESH  =  $0010;
  SCHEME_UPDATE   =  $0020;
  SCHEME_DONOTUSE = $0040; // used to be SCHEME_ENUMERATE; no longer supported
  SCHEME_CREATE   =  $0080;

type
  IActiveDesktopP = interface
    [SID_IActiveDesktopP]
    function SetSafeMode(dwFlags : DWORD): HResult; stdcall;
    function EnsureUpdateHTML: HResult; stdcall;
    function SetScheme(pwszSchemeName : LPCWSTR; dwFlags : DWORD): HResult; stdcall;
    function GetScheme(pwszSchemeName : LPWSTR; var lpdwcchBuffer : DWORD; dwFlags : DWORD): HResult; stdcall;
  end;

const
  GADOF_DIRTY  = 00000001;

type
  IADesktopP2 = interface
    [SID_IADesktopP2]
    function ReReadWallpaper : HResult; stdcall;
    function GetADObjectFlags(var lpdwFlags : DWORD; dwMask : DWORD): HResult; stdcall;
    function UpdateAllDesktopSubscriptions : HResult; stdcall;
    function MakeDynamicChanges(pOleObj : IOleObject): HResult; stdcall;
  end;

const
  MAX_COLUMN_NAME_LEN = 80;
  MAX_COLUMN_DESC_LEN = 128;

type
  SHCOLUMNINFO = packed record
    scid : SHCOLUMNID;                          // OUT the unique identifier of this column
    vt : integer;                               // OUT the native type of the data returned
    fmt : DWORD;                                // OUT this listview format (LVCFMT_LEFT, usually)
    cChars : UINT;                         // OUT the default width of the column, in characters
    csFlags : dword;                        // OUT SHCOLSTATE flags
    wszTitle : array [0..MAX_COLUMN_NAME_LEN - 1] of WideChar;        // OUT the title of the column
    wszDescription : array [0..MAX_COLUMN_DESC_LEN - 1] of WideChar;  // OUT full description of this column
  end;
  LPSHCOLUMNINFO = ^SHCOLUMNINFO;
  TShColumnInfo = SHCOLUMNINFO;
  PShColumnInfo = LPSHCOLUMNINFO;

  SHCOLUMNINIT = packed record
    dwFlags : ulong;              // initialization flags
    dwReserved : ulong;           // reserved for future use.
    wszFolder : array [0..MAX_PATH - 1] of WideChar;  // fully qualified folder path (or empty if multiple folders)
  end;
  LPSHCOLUMNINIT = ^SHCOLUMNINIT;
  TShColumnInit = SHCOLUMNINIT;
  PShColumnInit = LPSHCOLUMNINIT;

const
  SHCDF_UPDATEITEM  = $00000001;      // this flag is a hint that the file has changed since the last call to GetItemData

type
  SHCOLUMNDATA = packed record
    dwFlags : ULONG;              // combination of SHCDF_ flags.
    dwFileAttributes : DWord;    // file attributes.
    dwReserved : ULONG;          // reserved for future use.
    pwszExt : PWideChar;             // address of file name extension
    wszFile : array [0..MAX_PATH - 1] of WideChar;   // Absolute path of file.
  end;
  LPSHCOLUMNDATA = ^SHCOLUMNDATA;
  TShColumnData = SHCOLUMNDATA;
  PShColumnData = LPSHCOLUMNDATA;

  // Note: these objects must be threadsafe!  GetItemData _will_ be called
  // simultaneously from multiple threads.
  IColumnProvider = interface
    [SID_IColumnProvider]
    function Initialize(psci : PSHCOLUMNINIT): HResult; stdcall;
    function GetColumnInfo(dwIndex : DWORD; psci : PSHCOLUMNINFO): HResult; stdcall;
    function GetItemData(pscid : PSHCOLUMNID; pscd : PSHCOLUMNDATA; pvarData : Variant): HResult; stdcall;
  end;

///////////////////////////////////////////////////////
//
// Drag and Drop helper
//
// Purpose: To expose the Shell drag images
//
// This interface is implemented in the shell by CLSID_DragDropHelper.
//
// To use:
//   If you are the source of a drag (i.e. in response to LV_DRAGBEGIN or
//    equivelent begin drag message) call
//    IDragSourceHelper::InitializeFromWindow
//              (,
//               ,
//               )
//
//      NOTE: The Data object must support IDataObject::SetData with multiple
//            data types and GetData must implement data type cloning
//            (Including HGLOBAL), not just aliasing.
//
//   If you wish to have an image while over your application add the
//    IDragImages::Dr* calls to your IDropTarget implementation. For Example:
//
//    STDMETHODIMP CUserDropTarget::DragEnter(IDataObject* pDataObject,
//                                            DWORD grfKeyState,
//                                            POINTL pt, DWORD* pdwEffect)
//    {
//          // Process your DragEnter
//          // Call IDragImages::DragEnter last.
//          _pDropTargetHelper->DragEnter(_hwndDragOver, pDataObject,
//                                        (POINT*)&pt, *pdwEffect);
//          return hres;
//    }
//
//
//   If you wish to be able to source a drag image from a custom control,
//     implement a handler for the RegisterWindowMessage(DI_GETDRAGIMAGE).
//     The LPARAM is a pointer to an SHDRAGIMAGE structure.
//
//      sizeDragImage  -   Calculate the length and width required to render
//                          the images.
//      ptOffset       -   Calculate the offset from the upper left corner to
//                          the mouse cursor within the image
//      hbmpDragImage  -   CreateBitmap( sizeDragImage.cx, sizeDragImage.cy,
//                           GetDeviceCaps(hdcScreen, PLANES),
//                           GetDeviceCaps(hdcScreen, BITSPIXEL),
//                           NULL);
//
//   Drag Images will only be displayed on Windows NT 5.0 or later.
//
//
//   Note about IDropTargetHelper::Show - This method is provided for
//     showing/hiding the Drag image in low color depth video modes. When
//     painting to a window that is currently being dragged over (i.e. For
//     indicating a selection) you need to hide the drag image by calling this
//     method passing FALSE. After the window is done painting, Show the image
//     again by passing TRUE.
type
  SHDRAGIMAGE = record
    sizeDragImage : SIZE;      // OUT - The length and Width of the
                               //        rendered image
    ptOffset : TPoint;         // OUT - The Offset from the mouse cursor to
                               //        the upper left corner of the image
    hbmpDragImage : HBITMAP;   // OUT - The Bitmap containing the rendered
                               //        drag images
    crColorKey : TColorRef;    // OUT - The COLORREF that has been blitted
                               //        to the background of the images
  end;
  LPSHDRAGIMAGE = ^SHDRAGIMAGE;
  TShDragImage = SHDRAGIMAGE;
  PShDragImage = LPSHDRAGIMAGE;

const
  DI_GETDRAGIMAGE = 'ShellGetDragImage';

type
  IDropTargetHelper = interface
    [SID_IDropTargetHelper]
    function DragEnter(hwndTarget : hwnd; pDataObject : IDataObject;
                        const ppt : TPoint; dwEffect : DWORD): HResult; stdcall;
    function DragLeave : HResult; stdcall;
    function DragOver(const ppt : TPoint; dwEffect : DWORD): HResult; stdcall;
    function Drop(pDataObject : IDataObject; const ppt : TPoint; dwEffect : DWORD): HResult; stdcall;
    function Show(fShow : bool): HResult; stdcall;
  end;

  IDragSourceHelper = interface
    [SID_IDragSourceHelper]
    function InitializeFromBitmap(pshdi : PShDragImage; pDataObject : IDataObject): HResult; stdcall;
    function InitializeFromWindow(wnd : HWND; var ppt : TPoint; pDataObject : IDataObject): HResult; stdcall;
  end;

{ Clipboard format which may be supported by IDataObject from system
  defined shell folders (such as directories, network, ...). }
const
  CFSTR_INETURLA              =  CFSTR_SHELLURL;
  CFSTR_INETURLW              = 'UniformResourceLocatorW';
  CFSTR_DRAGCONTEXT           = 'DragContext';
  CFSTR_MOUNTEDVOLUME         = 'MountedVolume';
  CFSTR_PERSISTEDDATAOBJECT   = 'PersistedDataObject';
  CFSTR_TARGETCLSID           = 'TargetCLSID';                         // HGLOBAL with a CLSID of the drop target
  CFSTR_LOGICALPERFORMEDDROPEFFECT = 'Logical Performed DropEffect';
  CFSTR_AUTOPLAY_SHELLIDLISTS = 'Autoplay Enumerated IDList Array';    //  (HGLOBAL with LPIDA)

  CFSTR_INETURL               = CFSTR_INETURLA;

  DVASPECT_COPY          = 3; // use to indicate format is a "Copy" of the data (FILECONTENTS, FILEDESCRIPTOR, etc)
  DVASPECT_LINK          = 4; // use to indicate format is a "Shortcut" to the data (FILECONTENTS, FILEDESCRIPTOR, etc)


const
  { FILEDESCRIPTOR.dwFlags field indicate which fields are to be used }
  FD_PROGRESSUI       = $4000;       // Show Progress UI w/Drag and Drop

type
  SHChangeNotifyEntry = record
    pidl : PItemIdList;
    fRecursive : bool;
  end;

const
  SHCNEE_MSI_CHANGE         = 4;  // pidl2 is a SHChangeProductKeyAsIDList
  SHCNEE_MSI_UNINSTALL      = 5;  // pidl2 is a SHChangeProductKeyAsIDList

const
  QITIPF_DEFAULT       = $00000000;
  QITIPF_USENAME       = $00000001;
  QITIPF_LINKNOTARGET  = $00000002;
  QITIPF_LINKUSETARGET = $00000004;
  QITIPF_USESLOWTIP    = $00000008;  // Flag says it's OK to take a long time generating tip

type
  SHChangeDWORDAsIDList = record
    cb : word;
    dwItem1 : DWORD;
    dwItem2 : DWORD;
    cbZero : word;
  end;
  PSHChangeDWORDAsIDList = ^SHChangeDWORDAsIDList;
  TSHChangeDWORDAsIDList = SHChangeDWORDAsIDList;

  SHChangeUpdateImageIDList = record
    cb : word;
    iIconIndex : integer;
    iCurIndex : integer;
    uFlags : UINT;
    dwProcessID : DWORD;
    szName : array [0..MAX_PATH - 1] of WideChar;
    cbZero : word;
  end;
  PSHChangeUpdateImageIDList = ^SHChangeUpdateImageIDList;
  TSHChangeUpdateImageIDList = SHChangeUpdateImageIDList;

function SHHandleUpdateImage(pidlExtra :PItemIdList) : integer; stdcall;

type
  SHChangeProductKeyAsIDList = record
    cb : word;
    wszProductKey : array [0..38] of WideChar;
    cbZero : word;
  end;
  PSHChangeProductKeyAsIDList = ^SHChangeProductKeyAsIDList;
  TSHChangeProductKeyAsIDList = SHChangeProductKeyAsIDList;

procedure SHUpdateImage(pszHashItem : LPCSTR; iIndex : integer; uFlags : UINT; iImageIndex : integer);stdcall;
procedure SHUpdateImageA(pszHashItem : LPCSTR; iIndex : integer; uFlags : UINT; iImageIndex : integer);stdcall;
procedure SHUpdateImageW(pszHashItem : LPCWSTR; iIndex : integer; uFlags : UINT; iImageIndex : integer);stdcall;

const
  SHCNRF_InterruptLevel     = $0001;
  SHCNRF_ShellLevel         = $0002;
  SHCNRF_RecursiveInterrupt = $1000;
  SHCNRF_NewDelivery        = $8000;

function SHChangeNotifyRegister(wnd : HWND; fSources : integer; fEvents : LongInt; wMsg : UINT; cEntries : Integer; var pshcne : SHChangeNotifyEntry) : ulong; stdcall;
function SHChangeNotifyDeregister(uIID : LongWord): bool; stdcall;
function SHChangeNotification_Lock(hChangeNotification : THandle; dwProcessId : DWORD; var ppidl : PItemIdList; var plEvent : LongInt) : THandle; stdcall;
function SHChangeNotification_Unlock(hLock : THandle): bool; stdcall;
function SHGetRealIDL(psf : IShellFolder; pidlSimple : PItemIdList; ppidlReal : PItemIdlist) : HResult; stdcall;

//
// SHGetDataFromIDListA/W
//
// SHGetDataFromIDList nFormat values TCHAR
const
  SHDID_COMPUTER_IMAGING          = 18;
  SHDID_COMPUTER_AUDIO            = 19;
  SHDID_COMPUTER_SHAREDDOCS       = 20;

//===========================================================================
// PathResolve flags
const
  PRF_VERIFYEXISTS  = 0001;
  PRF_TRYPROGRAMEXTENSIONS = $0002 or PRF_VERIFYEXISTS;
  PRF_FIRSTDIRDEF  = $0004;
  PRF_DONTFINDLNK  = $0008;      // if PRF_TRYPROGRAMEXTENSIONS is specified
function RestartDialog(wnd : HWND; lpPrompt : LPCWSTR; dwReturn : DWORD) : integer; stdcall;
function RestartDialogEx(wnd : HWND; lpPrompt : LPCWSTR; dwReturn : DWORD; dwReasonCode : DWORD) : integer; stdcall;

function SHCoCreateInstance(pszCLSID : LPCWSTR; const pclsid : TCLSID; pUnkOuter : IUnknown; const riid : TIID; out ppv) : HResult; stdcall;

function CallCPLEntry16(inst : THandle; lpfnEntry : THandle; hwndCPL : HWnd; msg : UINT; lParam1, lParam2 : LParam) : LResult; stdcall;
function SHCreateStdEnumFmtEtc(cfmt : UINT; afmt : PFormatEtc; var ppenumFormatEtc : IEnumFORMATETC) : HResult; stdcall;
function SHDoDragDrop(wnd : HWnd; pData : IDataObject; pdsrc : IDropSource; dwEffect : DWORD; var pdwEffect : DWORD) : HResult; stdcall;

const
  NUM_POINTS = 3;

type
  AUTO_SCROLL_DATA = record
    iNextSample : integer;
    dwLastScroll : DWORD;
    bFull : BOOL;
    pts : array [0..NUM_POINTS-1] of TPoint;
    dwTimes : array [0..NUM_POINTS-1] of DWORD;
  end;
  PAutoScrollData = ^AUTO_SCROLL_DATA;
  TAutoScrollData = AUTO_SCROLL_DATA;

function DAD_SetDragImage(him : HImageList; var ptOffset : TPoint) : bool; stdcall;
function DAD_DragEnterEx(WndTarget : HWND; ptStart : TPoint) : bool; stdcall;
function DAD_DragEnterEx2(WndTarget : hwnd; ptStart : TPoint; dtObject : IDataObject) : bool; stdcall;
function DAD_ShowDragImage(fShow : bool) : bool; stdcall;
function DAD_DragMove(pt : TPoint) : bool; stdcall;
function DAD_DragLeave : bool; stdcall;
function DAD_AutoScroll(wnd : HWND; pad : PAutoScrollData; const ptNow : TPoint) : DWORD; stdcall;

const
  fFullPathTitle = 0;
  fSaveLocalView = 1;
  fNotShell  = 2;
  fSimpleDefault = 4;
  fDontShowDescBar = 8;
  fNewWindowMode = $10;
  fShowCompColor = $20;  // NT: Show compressed volumes in a different colour
  fDontPrettyNames = $40;  // NT: Do 8.3 name conversion, or not!
  fAdminsCreateCommonGroups = $80;  // NT: Administrators create comon groups

type
  CABINETSTATE = record
    cLength : WORD;
    nVersion : WORD;
    State : DWORD;
    fMenuEnumFilter : UINT;
  end;
  PCabinetState = ^CABINETSTATE;
  TCabinetState = CABINETSTATE;

function ReadCabinetState(var State : TCabinetState; iSize : integer) : bool; stdcall;
function WriteCabinetState(const state : TCabinetState) : bool; stdcall;
function PathMakeUniqueName(szUniqueName : LPWSTR; cchMax : cardinal;
                            szTemplate,szLongName,szDir : LPCWSTR) : bool; stdcall;
procedure PathQualify(szPath : LPWSTR) ; stdcall;
function PathIsExe(pszPath : LPCWSTR) : bool; stdcall;
function PathIsSlow(pszFile : LPCSTR; dwAttr : DWORD) : bool; stdcall;
function PathIsSlowA(pszFile : LPCSTR; dwAttr : DWORD) : bool; stdcall;
function PathIsSlowW(pszFile : LPCWSTR; dwAttr : DWORD) : bool; stdcall;

//  Return codes from PathCleanupSpec.  Negative return values are
//  unrecoverable errors
const
  PCS_FATAL        = $80000000;
  PCS_REPLACEDCHAR = $00000001;
  PCS_REMOVEDCHAR  = $00000002;
  PCS_TRUNCATED    = $00000004;
  PCS_PATHTOOLONG  = $00000008;

function PathCleanupSpec(pszDir : LPCWSTR; pszSpec : LPWSTR) : integer; stdcall;
function PathResolve(szPath : LPWSTR; var dirs : LPCWSTR; fFlags : uint) : bool; stdcall;

function GetFileNameFromBrowse(wnd : HWND; FilePath : LPWSTR;
                               cbFilePath : cardinal; pszWorkingDir : LPCWSTR;
                               DefExt : LPCWSTR; Filters : LPCWSTR;
                               Title : LPCWSTR) : bool; stdcall;
function DriveType(iDrive : integer) : integer; stdcall;
function RealDriveType(iDrive : integer; fOKToHitNet : bool) : integer; stdcall;
function IsNetDrive(Drive : integer) : integer; stdcall;

const
  // Flags for Shell_MergeMenus
  MM_ADDSEPARATOR    = $00000001;
  MM_SUBMENUSHAVEIDS = $00000002;
  MM_DONTREMOVESEPS  = $00000004;

function Shell_MergeMenus(mDst : HMenu; mSrc : HMenu; Insert : cardinal;
                               IdAdjust : cardinal; IdAdjustMax : cardinal;
                               flags : cardinal) : cardinal; stdcall;

(*
 * The SHObjectProperties API provides an easy way to invoke
 *   the Properties context menu command on shell objects.
 *
 *   PARAMETERS
 *
 *     hwnd    The window handle of the window which will own the dialog
 *     dwType       A SHOP_ value as defined below
 *     lpObject     Name of the object, see SHOP_ values below
 *     lpPage       The name of the property sheet page to open to or NULL.
 *
 *   RETURN
 *
 *     TRUE if the Properties command was invoked
 *)
function SHObjectProperties (wnd : HWND; dwType : cardinal; szObject, Page : PWideChar) : bool; stdcall;

const
  // SHObjectProperties flags
  SHOP_PRINTERNAME = $00000001;  // lpObject points to a printer friendly name
  SHOP_FILEPATH    = $00000002;  // lpObject points to a fully qualified path+file name
  SHOP_VOLUMEGUID  = $00000004;  // lpObject points to a Volume GUID

(*
 * The SHFormatDrive API provides access to the Shell
 *   format dialog. This allows apps which want to format disks
 *   to bring up the same dialog that the Shell does to do it.
 *
 *   This dialog is not sub-classable. You cannot put custom
 *   controls in it. If you want this ability, you will have
 *   to write your own front end for the DMaint_FormatDrive
 *   engine.
 *
 *   NOTE that the user can format as many diskettes in the specified
 *   drive, or as many times, as he/she wishes to. There is no way to
 *   force any specififc number of disks to format. If you want this
 *   ability, you will have to write your own front end for the
 *   DMaint_FormatDrive engine.
 *
 *   NOTE also that the format will not start till the user pushes the
 *   start button in the dialog. There is no way to do auto start. If
 *   you want this ability, you will have to write your own front end
 *   for the DMaint_FormatDrive engine.
 *
 *   PARAMETERS
 *
 *     hwnd    = The window handle of the window which will own the dialog
 *               NOTE that unlike SHCheckDrive, hwnd == NULL does not cause
 *               this dialog to come up as a "top level application" window.
 *               This parameter should always be non-null, this dialog is
 *               only designed to be the child of another window, not a
 *               stand-alone application.
 *     drive   = The 0 based (A: == 0) drive number of the drive to format
 *     fmtID   = The ID of the physical format to format the disk with
 *               NOTE: The special value SHFMT_ID_DEFAULT means "use the
 *                     default format specified by the DMaint_FormatDrive
 *                     engine". If you want to FORCE a particular format
 *                     ID "up front" you will have to call
 *                     DMaint_GetFormatOptions yourself before calling
 *                     this to obtain the valid list of phys format IDs
 *                     (contents of the PhysFmtIDList array in the
 *                     FMTINFOSTRUCT).
 *     options = There is currently only two option bits defined
 *
 *                SHFMT_OPT_FULL
 *                SHFMT_OPT_SYSONLY
 *
 *               The normal defualt in the Shell format dialog is
 *               "Quick Format", setting this option bit indicates that
 *               the caller wants to start with FULL format selected
 *               (this is useful for folks detecting "unformatted" disks
 *               and wanting to bring up the format dialog).
 *
 *               The SHFMT_OPT_SYSONLY initializes the dialog to
 *               default to just sys the disk.
 *
 *               All other bits are reserved for future expansion and
 *               must be 0.
 *
 *               Please note that this is a bit field and not a value
 *               and treat it accordingly.
 *
 *   RETURN
 *      The return is either one of the SHFMT_* values, or if the
 *      returned DWORD value is not == to one of these values, then
 *      the return is the physical format ID of the last succesful
 *      format. The LOWORD of this value can be passed on subsequent
 *      calls as the fmtID parameter to "format the same type you did
 *      last time".
 *
 *)
function SHFormatDrive(wnd: hwnd; drive,fmtID,options : cardinal) : DWORD; stdcall;

const
  // Special value of fmtID which means "use the default format"
  SHFMT_ID_DEFAULT  = $FFFF;

  // Option bits for options parameter
  SHFMT_OPT_FULL    = $0001;
  SHFMT_OPT_SYSONLY = $0002;

  // Special return values. PLEASE NOTE that these are DWORD values.
  SHFMT_ERROR    = $FFFFFFFF;     // Error on last format, drive may be formatable
  SHFMT_CANCEL   = $FFFFFFFE;     // Last format was canceled
  SHFMT_NOFORMAT = $FFFFFFFD;     // Drive is not formatable

function SHCreatePropSheetExtArray(Key : HKEY; pszSubKey : LPCWSTR; max_iface : UINT) : THandle; stdcall;
procedure SHDestroyPropSheetExtArray(hpsxa : THandle); stdcall;
function SHAddFromPropSheetExtArray(sxa : THandle; lpfnAddPage : LPFNADDPROPSHEETPAGE; lparm : LPARAM) : cardinal; stdcall;
function SHReplaceFromPropSheetExtArray(sxa : THandle; PageID : cardinal; lpfnReplaceWith : LPFNADDPROPSHEETPAGE;
                                    lparm : LPARAM) : cardinal; stdcall;
function ILClone(pidl : PItemIdList) : PItemIdList; stdcall;
function ILGetSize(pidl : PItemIdList): cardinal; stdcall;
function ILFindLastID(pidl : PItemIdList) : PItemIdList; stdcall;
function ILRemoveLastID(pidl : PItemIdList) : BOOL; stdcall;
function ILAppendId(pidl : PItemIdList; item : PShItemId; Append : Bool) : PItemIdList; stdcall;
procedure ILFree(pidl : PItemIdList); stdcall;
function ILCloneFirst(pidl : PItemIdList): PItemIdList; stdcall;
function ILIsEqual(pidl1,pidl2 : PItemIdList) : BOOL; stdcall;
function ILIsParent(pidlParent,pidlBelow : PItemIdList; Immediate : bool) : bool; stdcall;
function ILFindChild(pidlParent, pidlChild : PItemIdList) : PItemIdList; stdcall;
function ILCombine(pidl1,pidl2 : PItemIdList): PItemIdList; stdcall;
function ILLoadFromStream(pStm : IStream; var Pidl : PItemIdList) : HResult; stdcall;
function ILSaveToStream(pStm : IStream; pidl : PItemIdList) : HResult; stdcall;
function ILCreateFromPath(pszPath : LPCSTR) : PItemIdList; stdcall;
function ILCreateFromPathA(pszPath : LPCSTR) : PItemIdList; stdcall;
function ILCreateFromPathW(pszPath : LPCWSTR) : PItemIdList; stdcall;
function SHILCreateFromPath(path : PWideChar; var pidl : PItemIdList;
                                 var fInOut : cardinal) : HResult; stdcall;

type
  IDefViewFrame = interface
    [SID_IDefViewFrame]
    function GetWindowLV(var phWnd : HWND) : HResult; stdcall;
    function ReleaseWindowLV : HResult; stdcall;
    function GetShellFolder(var ppsf : IShellFolder) : HResult; stdcall;
  end;

//===========================================================================
// Shell restrictions. (Parameter for SHRestricted)
const
  REST_NONE                       = $00000000;
  REST_NORUN                      = $00000001;
  REST_NOCLOSE                    = $00000002;
  REST_NOSAVESET                  = $00000004;
  REST_NOFILEMENU                 = $00000008;
  REST_NOSETFOLDERS               = $00000010;
  REST_NOSETTASKBAR               = $00000020;
  REST_NODESKTOP                  = $00000040;
  REST_NOFIND                     = $00000080;
  REST_NODRIVES                   = $00000100;
  REST_NODRIVEAUTORUN             = $00000200;
  REST_NODRIVETYPEAUTORUN         = $00000400;
  REST_NONETHOOD                  = $00000800;
  REST_STARTBANNER                = $00001000;
  REST_RESTRICTRUN                = $00002000;
  REST_NOPRINTERTABS              = $00004000;
  REST_NOPRINTERDELETE            = $00008000;
  REST_NOPRINTERADD               = $00010000;
  REST_NOSTARTMENUSUBFOLDERS      = $00020000;
  REST_MYDOCSONNET                = $00040000;
  REST_NOEXITTODOS                = $00080000;
  REST_ENFORCESHELLEXTSECURITY    = $00100000;
  REST_LINKRESOLVEIGNORELINKINFO  = $00200000;
  REST_NOCOMMONGROUPS             = $00400000;
  REST_SEPARATEDESKTOPPROCESS     = $00800000;
  REST_NOWEB                      = $01000000;
  REST_NOTRAYCONTEXTMENU          = $02000000;
  REST_NOVIEWCONTEXTMENU          = $04000000;
  REST_NONETCONNECTDISCONNECT     = $08000000;
  REST_STARTMENULOGOFF            = $10000000;
  REST_NOSETTINGSASSIST           = $20000000;
  REST_NOINTERNETICON             = $40000001;
  REST_NORECENTDOCSHISTORY        = $40000002;
  REST_NORECENTDOCSMENU           = $40000003;
  REST_NOACTIVEDESKTOP            = $40000004;
  REST_NOACTIVEDESKTOPCHANGES     = $40000005;
  REST_NOFAVORITESMENU            = $40000006;
  REST_CLEARRECENTDOCSONEXIT      = $40000007;
  REST_CLASSICSHELL               = $40000008;
  REST_NOCUSTOMIZEWEBVIEW         = $40000009;
  REST_NOHTMLWALLPAPER            = $40000010;
  REST_NOCHANGINGWALLPAPER        = $40000011;
  REST_NODESKCOMP                 = $40000012;
  REST_NOADDDESKCOMP              = $40000013;
  REST_NODELDESKCOMP              = $40000014;
  REST_NOCLOSEDESKCOMP            = $40000015;
  REST_NOCLOSE_DRAGDROPBAND       = $40000016;  // Disable Close and Drag & Drop on ALL Bands
  REST_NOMOVINGBAND               = $40000017;  // Disable Moving ALL Bands
  REST_NOEDITDESKCOMP             = $40000018;
  REST_NORESOLVESEARCH            = $40000019;
  REST_NORESOLVETRACK             = $4000001A;
  REST_FORCECOPYACLWITHFILE       = $4000001B;
  REST_NOLOGO3CHANNELNOTIFY       = $4000001C;
  REST_NOFORGETSOFTWAREUPDATE     = $4000001D;
  REST_NOSETACTIVEDESKTOP         = $4000001E;  // No Active desktop on Settings Menu
  REST_NOUPDATEWINDOWS            = $4000001F;  // No Windows Update on Settings Menu
  REST_NOCHANGESTARMENU           = $40000020;  // No Context menu or Drag and Drop on Start menu
  REST_NOFOLDEROPTIONS            = $40000021;  // No Folder Options on Settings Menu
  REST_HASFINDCOMPUTERS           = $40000022;  // Show Start/Search/Computers
  REST_INTELLIMENUS               = $40000023;
  REST_RUNDLGMEMCHECKBOX          = $40000024;
  REST_ARP_ShowPostSetup          = $40000025;  // ARP: Show Post-Setup page
  REST_NOCSC                      = $40000026;  // Disable the ClientSide caching on SM
  REST_NOCONTROLPANEL             = $40000027;  // Remove the Control Panel only from SM|Settings
  REST_ENUMWORKGROUP              = $40000028;  // Enumerate workgroup in root of nethood
  REST_ARP_NOARP                  = $40000029;  // ARP: Don't Allow ARP to come up at all
  REST_ARP_NOREMOVEPAGE           = $4000002A;  // ARP: Don't allow Remove page
  REST_ARP_NOADDPAGE              = $4000002B;  // ARP: Don't allow Add page
  REST_ARP_NOWINSETUPPAGE         = $4000002C;  // ARP: Don't allow opt components page
  REST_GREYMSIADS                 = $4000002D;   // SM: Allow the greying of Darwin Ads in SM
  REST_NOCHANGEMAPPEDDRIVELABEL   = $4000002E;  // Don't enable the UI which allows users to rename mapped drive labels
  REST_NOCHANGEMAPPEDDRIVECOMMENT = $4000002F;  // Don't enable the UI which allows users to change mapped drive comments
  REST_MaxRecentDocs              = $40000030;
  REST_NONETWORKCONNECTIONS       = $40000031;  // No Start Menu | Settings |Network Connections
  REST_FORCESTARTMENULOGOFF       = $40000032;  // Force logoff on the Start Menu
  REST_NOWEBVIEW                  = $40000033;  // Disable Web View
  REST_NOCUSTOMIZETHISFOLDER      = $40000034;  // Disable Customize This Folder
  REST_NOENCRYPTION               = $40000035;  // Don't allow file encryption

  REST_DONTSHOWSUPERHIDDEN        = $40000037;  // don't show super hidden files
  REST_NOSHELLSEARCHBUTTON        = $40000038;
  REST_NOHARDWARETAB              = $40000039;  // No Hardware tab on Drives or in control panel
  REST_NORUNASINSTALLPROMPT       = $4000003A;  // Don't bring up "Run As" prompt for install programs
  REST_PROMPTRUNASINSTALLNETPATH  = $4000003B;  // Force the  "Run As" prompt for install programs on unc/network shares
  REST_NOMANAGEMYCOMPUTERVERB     = $4000003C;  // No Manage verb on My Computer
  REST_NORECENTDOCSNETHOOD        = $4000003D;  // dont add the recent docs shares to nethood
  REST_DISALLOWRUN                = $4000003E;  // don't allow certain apps to be run
  REST_NOWELCOMESCREEN            = $4000003F;  // don't allow the welcome screen to be displayed.
  REST_RESTRICTCPL                = $40000040;  // only allow certain cpls to be run
  REST_DISALLOWCPL                = $40000041;  // don't allow certain cpls to be run
  REST_NOSMBALLOONTIP             = $40000042;  // No Start Menu Balloon Tip
  REST_NOSMHELP                   = $40000043;  // No Help on the Start Menu
  REST_NOWINKEYS                  = $40000044;  // No Windows-X Hot keys
  REST_NOENCRYPTONMOVE            = $40000045;  // Don't automatically try to encrypt files that are moved to encryped directories
  REST_NOLOCALMACHINERUN          = $40000046;  // ignore HKLM\sw\ms\win\cv\Run and all of it's sub keys
  REST_NOCURRENTUSERRUN           = $40000047;  // ignore HKCU\sw\ms\win\cv\Run and all of it's sub keys
  REST_NOLOCALMACHINERUNONCE      = $40000048;  // ignore HKLM\sw\ms\win\cv\RunOnce and all of it's sub keys
  REST_NOCURRENTUSERRUNONCE       = $40000049;  // ignore HKCU\sw\ms\win\cv\RunOnce and all of it's sub keys
  REST_FORCEACTIVEDESKTOPON       = $4000004A;  // Force ActiveDesktop to be turned ON all the time.
  REST_NOCOMPUTERSNEARME          = $4000004B;  // removes the "Computers near me" link
  REST_NOVIEWONDRIVE              = $4000004C;  // disallows CreateViewObject() on specified drives (CFSFolder only)
  REST_NONETCRAWL                 = $4000004D;  // disables the crawling of the WNet namespace.
  REST_NOSHAREDDOCUMENTS          = $4000004E;  // don't auto share the Shared Documents/create link
  REST_NOSMMYDOCS                 = $4000004F;  // Don't show the My Documents item on the Start Menu.
  REST_NOSMMYPICS                 = $40000050;  // Don't show the My Pictures item on the Start Menu
  REST_ALLOWBITBUCKDRIVES         = $40000051;  // Bit mask indicating which which drives have bit bucket support
  REST_NONLEGACYSHELLMODE         = $40000052;  // new consumer shell modes
  REST_NOCONTROLPANELBARRICADE    = $40000053;  // The webview barricade in Control Panel
  REST_NOSTARTPAGE                = $40000054;  // Whistler Start Page on desktop.
  REST_NOAUTOTRAYNOTIFY           = $40000055;  // Whistler auto-tray notify feature
  REST_NOTASKGROUPING             = $40000056;  // Whistler taskbar button grouping feature
  REST_NOCDBURNING                = $40000057;  // whistler cd burning feature
  REST_MYCOMPNOPROP               = $40000058;  // disables Properties on My Computer's context menu
  REST_MYDOCSNOPROP               = $40000059;  // disables Properties on My Documents' context menu
  REST_NOSTARTPANEL               = $4000005A;  // Windows start panel (New start menu) for Whistler.
  REST_NODISPLAYAPPEARANCEPAGE    = $4000005B;  // disable Themes and Appearance tabs in the Display Control Panel.
  REST_NOTHEMESTAB                = $4000005C;  // disable the Themes tab in the Display Control Panel.
  REST_NOVISUALSTYLECHOICE        = $4000005D;  // disable the visual style drop down in the Appearance tab of the Display Control Panel.
  REST_NOSIZECHOICE               = $4000005E;  // disable the size drop down in the Appearance tab of the Display Control Panel.
  REST_NOCOLORCHOICE              = $4000005F;  // disable the color drop down in the Appearance tab of the Display Control Panel.
  REST_SETVISUALSTYLE             = $40000060;  // Load the specified file as the visual style.
  REST_STARTRUNNOHOMEPATH         = $40000061;  // dont use the %HOMEPATH% env var for the Start-Run dialog
  REST_NOUSERNAMEINSTARTPANEL     = $40000062;  // don't show the username is the startpanel.
  REST_NOMYCOMPUTERICON           = $40000063;  // don't show my computer anywhere;hide its contents
  REST_NOSMNETWORKPLACES          = $40000064;  // don't show network places in startpanel.
  REST_NOSMPINNEDLIST             = $40000065;  // don't show the pinned list in startpanel.
  REST_NOSMMYMUSIC                = $40000066;  // don't show MyMusic folder in startpanel
  REST_NOSMEJECTPC                = $40000067;  // don't show "Undoc PC" command in startmenu
  REST_NOSMMOREPROGRAMS           = $40000068;  // don't show "More Programs" button in StartPanel.
  REST_NOSMMFUPROGRAMS            = $40000069;  // don't show the MFU programs list in StartPanel.
  REST_NOTRAYITEMSDISPLAY         = $4000006A;  // disables the display of the system tray
  REST_NOTOOLBARSONTASKBAR        = $4000006B;  // disables toolbar display on the taskbar
  REST_NOSMCONFIGUREPROGRAMS      = $4000006F;  // No Configure Programs on Settings Menu
  REST_HIDECLOCK                  = $40000070;  // don't show the clock
  REST_NOLOWDISKSPACECHECKS       = $40000071;  // disable the low disk space checking
  REST_NOENTIRENETWORK            = $40000072;  // removes the "Entire Network" link (i.e. from "My Network Places")
  REST_NODESKTOPCLEANUP           = $40000073;  // disable the desktop cleanup wizard
  REST_BITBUCKNUKEONDELETE        = $40000074;  // disables recycling of files
  REST_BITBUCKCONFIRMDELETE       = $40000075;  // always show the delete confirmation dialog when deleting files
  REST_BITBUCKNOPROP              = $40000076;  // disables Properties on Recycle Bin's context menu
  REST_NODISPBACKGROUND           = $40000077;  // disables the Desktop tab in the Display CPL
  REST_NODISPSCREENSAVEPG         = $40000078;  // disables the Screen Saver tab in the Display CPL
  REST_NODISPSETTINGSPG           = $40000079;  // disables the Settings tab in the Display CPL
  REST_NODISPSCREENSAVEPREVIEW    = $4000007A;  // disables the screen saver on the Screen Saver tab in the Display CPL
  REST_NODISPLAYCPL               = $4000007B;  // disables the Display CPL
  REST_HIDERUNASVERB              = $4000007C;  // hides the "Run As..." context menu item
  REST_NOTHUMBNAILCACHE           = $4000007D;  // disables use of the thumbnail cache
  REST_NOSTRCMPLOGICAL            = $4000007E;  // dont use StrCmpLogical() instead use default CompareString()

  REST_ALLOWUNHASHEDWEBVIEW       = $40000082;  // allow the user to be promted to accept web view templates that don't already have an md5 hash in the registry
  REST_ALLOWLEGACYWEBVIEW         = $40000083;  // allow legacy webview template to be shown.
  REST_REVERTWEBVIEWSECURITY      = $40000084;  // disable added webview security measures (revert to w2k functionality).

  REST_INHERITCONSOLEHANDLES      = $40000086;  // ShellExec() will check for the current process and target process being console processes to inherit handles

  REST_NODISCONNECT               = $41000001;  // No Disconnect option in Start menu
  REST_NOSECURITY                 = $41000002;  // No Security option in start menu
  REST_NOFILEASSOCIATE            = $41000003;  // Do not allow user to change file association
type
  TRestrictions = DWORD;

function OpenRegStream(Key : HKEY; SubKey : PWideChar; Value : PWideChar;
                            mode : cardinal) : IStream; stdcall;
function SHFindFiles(pidlFolder,pidlSaveFile : PItemIdList) : bool; stdcall;
procedure PathGetShortPath(szLongPath : LPWSTR); stdcall;
function PathYetAnotherMakeUniqueName(szUniqueName : LPWSTR; szPathName,szShortName,szFileSpec : LPCWSTR) : bool; stdcall;
function Win32DeleteFile(pszPath : LPCWSTR) : bool; stdcall;

const
  // PathProcessCommand flags
  PPCF_ADDQUOTES               = $00000001;        // return a quoted name if required
  PPCF_ADDARGUMENTS            = $00000003;        // appends arguments (and wraps in quotes if required)
  PPCF_NODIRECTORIES           = $00000010;        // don't match to directories
  PPCF_NORELATIVEOBJECTQUALIFY = $00000020;        // don't return fully qualified relative objects
  PPCF_FORCEQUALIFY            = $00000040;        // qualify even non-relative names
  PPCF_LONGESTPOSSIBLE         = $00000080;        // always find the longest possible name

function PathProcessCommand(src : LPCWSTR; Dest : LPWSTR; destMax : integer; Flags : cardinal) : integer; stdcall;
function SHRestricted(rest : TRestrictions) : cardinal; stdcall;
function SignalFileOpen(pidl : PItemIdList) : bool; stdcall;
function SHSimpleIDListFromPath(path : PWideChar) : PItemIdList; stdcall;
function SHLoadOLE(lparm : LPARAM) : HResult; stdcall;  // deprecated
function SHStartNetConnectionDialog(wnd : HWND; szRemoteName : PChar; dwType : DWORD) : HRESULT; stdcall;
function SHStartNetConnectionDialogA(wnd : HWND; szRemoteName : PChar; dwType : DWORD) : HRESULT; stdcall;
function SHStartNetConnectionDialogW(wnd : HWND; szRemoteName : PWideChar; dwType : DWORD) : HRESULT; stdcall;

function SHDefExtractIconA(pszIconFile : LPCSTR; iIndex : integer; uFlags :  UINT;
                           var phiconLarge,phiconSmall : HICON; nIconSize : UINT) : HResult; stdcall;
function SHDefExtractIcon(pszIconFile : LPCSTR; iIndex : integer; uFlags :  UINT;
                           var phiconLarge,phiconSmall : HICON; nIconSize : UINT) : HResult; stdcall;
function SHDefExtractIconW(pszIconFile : LPCWSTR; iIndex : integer; uFlags :  UINT;
                           var phiconLarge,phiconSmall : HICON; nIconSize : UINT) : HResult; stdcall;

function Shell_GetImageLists(var himlLarge,himlSmall : HIMAGELIST) : bool; stdcall;
function Shell_GetCachedImageIndex(IconPath : PWideChar; IconIndex : integer; IconFlags : cardinal) : integer; stdcall;

//
// IDocViewSite
//
type
  IDocViewSite = interface
    [SID_IDocViewSite]
    function OnSetTitle(pvTitle : PVARIANT) : HResult; stdcall;
  end;

const
  VALIDATEUNC_NOUI = $0002;          // don't bring up UI
  VALIDATEUNC_CONNECT = $0001;       // connect a drive letter
  VALIDATEUNC_PRINT = $0004;         // validate as print share instead of disk share
  VALIDATEUNC_VALID = $0007;         // valid flags

function SHValidateUNC(wndOwner : HWnd; szFile : LPWSTR; fConnect : cardinal) : bool; stdcall;

const
  OPENPROPS_NONE          = $0000;
  OPENPROPS_INHIBITPIF    = $8000;
  GETPROPS_NONE           = $0000;
  SETPROPS_NONE           = $0000;
  CLOSEPROPS_NONE         = $0000;
  CLOSEPROPS_DISCARD      = $0001;

  PIFNAMESIZE     = 30;
  PIFSTARTLOCSIZE = 63;
  PIFDEFPATHSIZE  = 64;
  PIFPARAMSSIZE   = 64;
  PIFSHPROGSIZE   = 64;
  PIFSHDATASIZE   = 64;
  PIFDEFFILESIZE  = 80;
  PIFMAXFILEPATH  = 260;

type
  PROPPRG = packed record
    flPrg : word;                          // see PRG_ flags
    flPrgInit : word;                      // see PRGINIT_ flags
    achTitle : array [0..PIFNAMESIZE-1] of char;          // name[30]
    achCmdLine : array [0..PIFSTARTLOCSIZE+PIFPARAMSSIZE] of char;// startfile[63] + params[64]
    achWorkDir : array [0..PIFDEFPATHSIZE-1] of char;     // defpath[64]
    wHotKey : word;                        // PfHotKeyScan thru PfHotKeyVal
    achIconFile : array [0..PIFDEFFILESIZE-1] of char;    // name of file containing icon
    wIconIndex : word;                     // index of icon within file
    dwEnhModeFlags : dword;                 // reserved enh-mode flags
    dwRealModeFlags : dword;                // real-mode flags (see RMOPT_*)
    achOtherFile : array [0..PIFDEFFILESIZE - 1] of char;   // name of "other" file in directory
    achPIFFile : array [0..PIFMAXFILEPATH-1] of char;     // name of PIF file
  end;
  PPROPPRG = ^PPROPPRG;
  TPropPrg = PROPPRG;

function PifMgr_OpenProperties(App,pif : PWideChar; hInf,flOpt : cardinal) : THandle; stdcall;
function PifMgr_GetProperties(hProps : THandle; group : PChar; Props : Pointer;
                              cbProps : integer; lOpt : cardinal): integer; stdcall;
function PifMgr_SetProperties(hProps : THandle; Group : PChar; lpProps : Pointer;
                              cbProps, flOpt : cardinal): integer; stdcall;
function PifMgr_CloseProperties(hProps : THandle; lOpt : cardinal) : THandle; stdcall;

procedure SHSetInstanceExplorer(unk : IUnknown); stdcall;
function IsUserAnAdmin : bool; stdcall;

type
  IInitializeObject = interface
//    [SID_IInitializeObject]
    function Initialize : HResult; stdcall;
  end;

const
  BMICON_LARGE = 0;
  BMICON_SMALL = 1;

type
  IBanneredBar = interface
    [SID_IBanneredBar]
    function SetIconSize(iIcon : DWORD) : HResult; stdcall;
    function GetIconSize(var piIcon : DWORD) : HResult; stdcall;
    function SetBitmap(Bitmap : HBITMAP) : HResult; stdcall;
    function GetBitmap(var phBitmap : HBITMAP) : HResult; stdcall;
  end;

function SHShellFolderView_Message(hWndMain : HWND; uMsg : UINT; lParm : LParam) : LResult; stdcall;

// Callback interface for the IShellFolderView
type
  IShellFolderViewCB = interface
    [SID_IShellFolderViewCB]
    function MessageSFVCB(uMsg : UINT; wParm : WPARAM; lParm : LPARAM) : HResult; stdcall;
  end;

const
  QCMINFO_PLACE_BEFORE   = 0;
  QCMINFO_PLACE_AFTER    = 1;

type
  QCMINFO_IDMAP_PLACEMENT = record
    id : UINT;
    fFlags : UINT;
  end;
  PQCMINFO_IDMAP_PLACEMENT = ^QCMINFO_IDMAP_PLACEMENT;
  TQCMInfoIDMapPlacement = QCMINFO_IDMAP_PLACEMENT;
  PQCMInfoIDMapPlacement = PQCMINFO_IDMAP_PLACEMENT;

  QCMINFO_IDMAP = record
    nMaxIDs : UINT;
    pIdList : array [0..0] of QCMINFO_IDMAP_PLACEMENT;
  end;
  PQCMINFO_IDMAP = ^QCMINFO_IDMAP;

  QCMINFO = record
    menu : HMENU;           // in
    indexMenu : UINT;       // in
    idCmdFirst : UINT;      // in/out
    idCmdLast : UINT;       // in
    pIDMap : PQCMINFO_IDMAP; // in / unused
  end;
  PQCMINFO = ^QCMINFO;
  TQCMInfo = QCMINFO;

// TBINFO flags
const
  TBIF_APPEND    = 0;
  TBIF_PREPEND   = 1;
  TBIF_REPLACE   = 2;
  TBIF_DEFAULT   = 0;
  TBIF_INTERNETBAR = $00010000;
  TBIF_STANDARDTOOLBAR = $00020000;
  TBIF_NOTOOLBAR  = $00030000;

type
  TBINFO = record
    cbuttons : UINT;       // out
    uFlags : UINT;         // out (one of TBIF_ flags)
  end;
  TTbInfo = TBINFO;
  PTbInfo = ^TBINFO;

  DETAILSINFO = record
    pidl : PItemIdList;
    fmt : integer;
    cxChar : integer;
    str : TStrRet;
    iImage : integer;
  end;
  TDetailsInfo = DETAILSINFO;
  PDetailsInfo = ^DETAILSINFO;

  SFVM_PROPPAGE_DATA = record
    dwReserved : DWORD;
    pfn : LPFNADDPROPSHEETPAGE;
    lParm : LPARAM;
  end;
  TSFVMPropPageData = SFVM_PROPPAGE_DATA;
  PSFVMPropPageData = ^SFVM_PROPPAGE_DATA;

  SFVM_HELPTOPIC_DATA = record
    wszHelpFile : array [0..MAX_PATH-1] of WideChar;
    wszHelpTopic : array [0..MAX_PATH-1] of WideChar;
  end;
  TSFVMHelpTopicData = SFVM_HELPTOPIC_DATA;
  PSFVMHelpTopicData = ^SFVM_HELPTOPIC_DATA;

const
  //                        uMsg       wParam             lParam
  SFVM_MERGEMENU           =  1;    // uFlags             LPQCMINFO
  SFVM_INVOKECOMMAND       =  2;    // idCmd              -
  SFVM_GETHELPTEXT         =  3;    // idCmd,cchMax       pszText
  SFVM_GETTOOLTIPTEXT      =  4;    // idCmd,cchMax       pszText
  SFVM_GETBUTTONINFO       =  5;    // -                  LPTBINFO
  SFVM_GETBUTTONS          =  6;    // idCmdFirst,cbtnMax LPTBBUTTON
  SFVM_INITMENUPOPUP       =  7;    // idCmdFirst,nIndex  hmenu
  SFVM_GETSELECTEDOBJECTS  =  9;    // DWORD              LPTBBUTTON
  SFVM_FSNOTIFY            = 14;    // LPITEMIDLIST       DWrd - SHCNE_ value
  SFVM_WINDOWCREATED       = 15;    // hwnd               PDVSELCHANGEINFO
  SFVM_GETDETAILSOF        = 23;    // iColumn            DETAILSINFO*
  SFVM_COLUMNCLICK         = 24;    // iColumn            -
  SFVM_QUERYFSNOTIFY       = 25;    // -                  SHChangeNotifyEntry *
  SFVM_DEFITEMCOUNT        = 26;    // -                  UINT* number of items in the folder view
  SFVM_DEFVIEWMODE         = 27;    // -                  FOLDERVIEWMODE*
  SFVM_UNMERGEMENU         = 28;    // -                  hmenu
  SFVM_UPDATESTATUSBAR     = 31;    // fInitialize        -
  SFVM_BACKGROUNDENUM      = 32;    // -                  -
  SFVM_DIDDRAGDROP         = 36;    // dwEffect           IDataObject *
  SFVM_SETISFV             = 39;    // -                  IShellFolderView*
  SFVM_THISIDLIST          = 41;    // -                  LPITMIDLIST*
  SFVM_ADDPROPERTYPAGES    = 47;    // -                  SFVM_PROPPAGE_DATA *
  SFVM_BACKGROUNDENUMDONE  = 48;    // -                  -
  SFVM_GETNOTIFY           = 49;    // LPITEMIDLIST*      LONG*
  SFVM_GETSORTDEFAULTS     = 53;    // iDirection         iParamSort
  SFVM_SIZE                = 57;    // -                  -
  SFVM_GETZONE             = 58;    // -                  DWORD*
  SFVM_GETPANE             = 59;    // Pane ID            DWORD*
  SFVM_GETHELPTOPIC        = 63;    // -                  SFVM_HELPTOPIC_DATA *
  SFVM_GETANIMATION        = 68;    // HInstance          widechar

// SHCreateShellFolderView struct
type
  SFV_CREATE = record
    Size : DWORD;
    ShellFolder : IShellFolder;
    psvOuter : IShellView;
    psfvcb : IShellFolderViewCB; // No callback if NULL
  end;
  TSFVCreate = SFV_CREATE;
  PSFVCreate = ^SFV_CREATE;

function SHCreateShellFolderView(const pcsfv : TSFVCreate; var ppsv : IShellView) : HResult; stdcall;
type
  TFNDFMCallback = function (psf : Ishellfolder; wnd : HWND;
                             pdtObj : IDataObject; uMsg : UINT;
                             WParm : WParam; lParm : LParam) : HResult; stdcall;

  TDefFolderMenu_Create2 = function (pidlFolder : PItemIdList; wnd : HWnd;
                                cidl : uint; var apidl : PItemIdList;
                                psf : IShellFolder; lpfn : TFNDFMCallback;
                                nKeys : UINT; ahkeyClsKeys : PHKEY;
                                var ppcm : IContextMenu) : integer; stdcall;
var
  CDefFolderMenu_Create2 : TDefFolderMenu_Create2;

function SHOpenPropSheet(pszCaption : LPCSTR; const ahkeys : HKEY;
                         ckeys : cardinal; const clsidDef : TClsId;
                         dtObj : IDataObject; sb : IShellBrowser;
                         startPage : LPCSTR) : bool; stdcall;
function SHOpenPropSheetA(pszCaption : LPCSTR; const ahkeys : HKEY;
                          ckeys : cardinal; const clsidDef : TClsId;
                          dtObj : IDataObject; sb : IShellBrowser;
                          startPage : LPCSTR) : bool; stdcall;
function SHOpenPropSheetW(pszCaption : LPCWSTR; ahkeys : PHKEY;
                          ckeys : cardinal; clsidDef : PClsId;
                          dtObj : IDataObject; sb : IShellBrowser;
                          startPage : LPCWSTR) : bool; stdcall;
const
  //                          uMsg       wParam       lParam
  DFM_MERGECONTEXTMENU      =  1;      // uFlags       LPQCMINFO
  DFM_INVOKECOMMAND         =  2;      // idCmd        pszArgs
  DFM_GETDEFSTATICID        =  14;     // idCmd *      0
  // Commands from DFM_INVOKECOMMAND when strings are passed in
  DFM_CMD_PROPERTIES  = UINT(-5);

type
  TFnViewCallback = function (psvUser : integer;
                              ShellFolder : IShellFolder;
                              wnd : HWND; Msg : UInt; WParm : WParam;
                              lParm : LParam) : HResult; stdcall;

// SHCreateShellFolderViewEx struct
  CSFV = record
    Size : DWORD;
    ShellFolder : IShellFolder;
    psvUser : integer; // psvOuter : IShellView;
    pidl : PITEMIDLIST;
    dwEventId : DWORD;
    CallBack : TFnViewCallback;
    fvm : TFolderViewMode;
  end;
  TCSFV = CSFV;
  PCSFV = ^CSFV;

const
  // Tell the FolderView to rearrange.  The lParam will be passed to
  // IShellFolder::CompareIDs
  SFVM_REARRANGE           =  1;    //                    LParam
  // Add an OBJECT into the view
  SFVM_ADDOBJECT           =  3;    // -                  PItemIdList object to add
  // Remove an OBJECT into the view
  SFVM_REMOVEOBJECT        =  6;    //                    pidl
  // updates an object by passing in pointer to two PIDLS, the first
  // is the old pidl, the second one is the one with update information.
  //
  // _ppidl[1] must be a *copy* of a pidl, as control over the lifetime
  // of the pidl belongs to the view after successful completion of
  // this call.  (Unsuccessful completion (a -1 return) implies failure
  // and the caller must free the memory.)  Win95 waits a while before
  // freeing the pidl, IE4 frees the pidl immediately.
  // IShellFolderView::UpdateObject does not suffer from this problem.
  //
  SFVM_UPDATEOBJECT = $00000007;
  // Returns an array of the selected IDS to the caller.
  //     lparam is a pointer to receive the idlists into
  //     return value is the count of items in the array.
  // Sets the position of an item in the viewer
  //     lparam is a pointer to a SVF_SETITEMPOS
  //     return value is unused
  SFVM_SETITEMPOS         = $0000000e;
  //  Notifies a ShellView when one of its objects get put on the clipboard
  //  as a result of a menu command.
  //
  //
  //     lparam is the dwEffect (DROPEFFECT_MOVE, DROPEFFECT_COPY)
  //     return value is void.
  SFVM_SETCLIPBOARD        = 16;    //                    dwEffect
  SFVM_SETPOINTS         = $00000017;

function ShellFolderView_ReArrange(wnd : hwnd; lParm : LPARAM) : BOOL;
function ShellFolderView_AddObject(wnd : HWND; pidl : PItemIdList) : LPARAM;
function ShellFolderView_RemoveObject(wnd : hwnd; pidl : PItemIdList) : LPARAM;
function ShellFolderView_UpdateObject(wnd : hwnd; var ppidl : PItemIdList) : LPARAM;
function ShellFolderView_GetSelectedObjects(wnd : hwnd; var ppidl : PItemIdList) : LParam;

type
  SFV_SETITEMPOS = record
    pidl : PItemIdList;
    pt : TPoint;
  end;
  PSFVSetItemPos = ^SFV_SETITEMPOS;
  TSFVSetItemPos = SFV_SETITEMPOS;

procedure ShellFolderView_SetItemPos(wnd : hwnd; pidl : PItemIdList; x,y : integer);
procedure ShellFolderView_SetClipboard(wnd : hwnd; dwEffect : DWORD);
procedure ShellFolderView_SetPoints(wnd : hwnd; pdtobj : IDataObject);

function SHFind_InitMenuPopup(menu : HMENU; wnd : HWND; CmdFirst,CmdLast : cardinal) : IContextMenu; stdcall;
function SHCreateShellFolderViewEx(var csfv : TCSFV; var ppsv : IShellView) : HResult; stdcall;

const
// PROPIDs for Internet Sites (FMTID_InternetSite) to be used with
// IPropertySetStorage/IPropertyStorage
  PID_INTSITE_ICONINDEX     = 20;
  PID_INTSITE_ICONFILE      = 21;

////////////////////////////////////////////////////////////////////
//
// The shell keeps track of some per-user state to handle display
// options that is of major interest to ISVs.
// The key one requested right now is "DoubleClickInWebView".
type
  SHELLSTATEA = record
    Flags : DWORD;
{
    BOOL fShowAllObjects : 1;
    BOOL fShowExtensions : 1;
    BOOL fNoConfirmRecycle : 1;

    BOOL fShowSysFiles : 1;
    BOOL fShowCompColor : 1;
    BOOL fDoubleClickInWebView : 1;
    BOOL fDesktopHTML : 1;
    BOOL fWin95Classic : 1;
    BOOL fDontPrettyPath : 1;
    BOOL fShowAttribCol : 1; // No longer used, dead bit
    BOOL fMapNetDrvBtn : 1;
    BOOL fShowInfoTip : 1;
    BOOL fHideIcons : 1;
    BOOL fWebView : 1;
    BOOL fFilter : 1;
    BOOL fShowSuperHidden : 1;
    BOOL fNoNetCrawling : 1;
}
    dwWin95Unused : DWORD; // Win95 only - no longer supported pszHiddenFileExts
    uWin95Unused : UINT; // Win95 only - no longer supported cbHiddenFileExts

    // Note: Not a typo!  This is a persisted structure so we cannot use LPARAM
    lParamSort : LongInt;
    iSortDirection : integer;

    version : UINT;

    // new for win2k. need notUsed var to calc the right size of ie4 struct
    // FIELD_OFFSET does not work on bit fields
    uNotUsed : UINT; // feel free to rename and use
    Flags2 : WORD;
{
    BOOL fSepProcess: 1;
    // new for Whistler.
    BOOL fStartPanelOn: 1;       //Indicates if the Whistler StartPanel mode is ON or OFF.
    BOOL fShowStartPage: 1;      //Indicates if the Whistler StartPage on desktop is ON or OFF.
    UINT fSpareFlags : 13;
}
  end;
  PSHELLSTATEA = ^SHELLSTATEA;
  TShellStateA = SHELLSTATEA;
  TShellState = SHELLSTATEA;
  ShellState = SHELLSTATEA;

const
  SHELLSTATEVERSION_IE4  = 9;
  SHELLSTATEVERSION_WIN2K = 10;

procedure SHGetSetSettings(var lpss : ShellState; dwMask : DWORD; bSet : bool); stdcall;

// SHBindToParent(LPCITEMIDLIST pidl, REFIID riid, void **ppv, LPCITEMIDLIST *ppidlLast)
//
// Given a pidl, you can get an interface pointer (as specified by riid) of the pidl's parent folder (in ppv)
// If ppidlLast is non-NULL, you can also get the pidl of the last item.
//
function SHBindToParent(pidl : PItemIdList; const riid : TIID; out ppv; const ppidlLast : PItemIdList) : Hresult; stdcall;

// SHSTDAPI SHParseDisplayName(PCWSTR pszName, IBindCtx *pbc, LPITEMIDLIST *ppidl, SFGAOF sfgaoIn, SFGAOF *psfgaoOut)
//
//  given a string it will call psfDesktop->ParseDisplayName() to try and create a pidl
//  if no pbc specified, it uses the preferred options for parsing.
//  this includes mapping file system paths to their appropriate aliased location (RegisterObjectParam(STR_PARSE_TRANSLATE_ALIASES))
//  psfgaoOut is optional for SFGAO attributes
//
function SHParseDisplayName(pszName : LPWSTR; pbc : IBindCtx; var ppidl : PItemIdList; sfgaoIn : DWORD; var psfgaoOut : DWORD) : HResult; stdcall;

// SHPathPrepareForWrite(HWND hwnd, IUnknown *punkEnableModless, LPCTSTR pszPath, DWORD dwFlags)
//
// DESCRIPTION:
//     This API will prepare the path for the caller.  This includes:
// 1. Prompting for the ejectable media to be re-inserted. (Floppy, CD-ROM, ZIP drive, etc.)
// 2. Prompting for the media to be formatted. (Floppy, hard drive, etc.)
// 3. Remount mapped drives if the connection was lost. (\\unc\share mapped to N: becomes disconnected)
// 4. If the path doesn't exist, create it.  (SHPPFW_DIRCREATE and SHPPFW_ASKDIRCREATE)
// 5. Display an error if the media is read only. (SHPPFW_NOWRITECHECK not set)
//
// PARAMETERS:
//      hwnd: Parernt window for UI.  NULL means don't display UI. OPTIONAL
//      punkEnableModless: Parent that will be set to modal during UI using IOleInPlaceActiveObject::EnableModeless(). OPTIONAL
//      pszPath: Path to verify is valid for writting.  This can be a UNC or file drive path.  The path
//               should only contain directories.  Pass SHPPFW_IGNOREFILENAME if the last path segment
//               is always filename to ignore.
//      dwFlags: SHPPFW_* Flags to modify behavior
//
//-------------------------------------------------------------------------
const
  SHPPFW_NONE           = $00000000;
  SHPPFW_DIRCREATE      = $00000001;              // Create the directory if it doesn't exist without asking the user.
  SHPPFW_DEFAULT        = SHPPFW_DIRCREATE;       // May change
  SHPPFW_ASKDIRCREATE   = $00000002;              // Create the directory if it doesn't exist after asking the user.
  SHPPFW_IGNOREFILENAME = $00000004;              // Ignore the last item in pszPath because it's a file.  Example: pszPath="C:\DirA\DirB", only use "C:\DirA".
  SHPPFW_NOWRITECHECK   = $00000008;              // Caller only needs to read from the drive, so don't check if it's READ ONLY.
  SHPPFW_MEDIACHECKONLY = $00000010;              // do the retrys on the media (or net path), return errors if the file can't be found

function SHPathPrepareForWrite(wnd : HWND; punkEnableModless : IUnknown;
                                pszPath : LPCSTR; dwFlags : dword) : hresult; stdcall;
function SHPathPrepareForWriteA(wnd : HWND; punkEnableModless : IUnknown;
                                pszPath : LPCSTR; dwFlags : dword) : hresult; stdcall;
function SHPathPrepareForWriteW(wnd : HWND; punkEnableModless : IUnknown;
                                pszPath : LPCWSTR; dwFlags : dword) : hresult; stdcall;

//--------------------------------------------------------------------------
//
// Interface used for exposing the INI file methods on a shortcut file
//
//
//--------------------------------------------------------------------------
type
  INamedPropertyBag = interface
    [SID_INamedPropertyBag]
    function ReadPropertyNPB(pszBagname : POLESTR; pszPropName : POLESTR; var pVar : PROPVARIANT) : hresult; stdcall;
    function WritePropertyNPB(pszBagname : POLESTR; pszPropName : POLESTR; var pVar : PROPVARIANT) : hresult; stdcall;
    function RemovePropertyNPB(pszBagname : POLESTR; pszPropName : POLESTR) : hresult; stdcall;
  end;

//  SHPropStgCreate()
//  Wrap of IPropertySetStorage::Open/Create
//
//  This function ensures proper handling of code page retrieval/assignment
//  for the requested property set operation.
//
//  psstg,          //  Address of IPropertySetStorage vtable
//  fmtid,          //  property set ID
//  pclsid,         //  class ID associated with the set. This can be NULL
//  grfFlags,       //  PROPSETFLAG_xxx.  All sets containing ansi bytes should be created with
                    //  PROPSETFLAG_ANSI, otherwise PROPSETFLAG_DEFAULT.
//  grfMode,        //  STGM_ flags.  Must contain STGM_DIRECT|STGM_EXCLUSIVE.
//  dwDisposition,  //  OPEN_EXISTING. OPEN_ALWAYS, CREATE_NEW, or CREATE_ALWAYS
//  IPropertyStorage** ppstg,  // Address to receive requested vtable
//  puCodePage      //  Optional address to receive the code page ID for the set.
//
function SHPropStgCreate(psstg : IPropertySetStorage; fmtid : TIID;
                         var pclsid : TClsId;grfFlags : DWORD;
                         grfMode : DWORD; dwDisposition : DWORD;
                         out ppstg : IPropertyStorage;
                         out puCodePage : uint) : HResult; stdcall;

//  SHPropStgReadMultiple()
//  IPropertyStorage::ReadMultiple wrap
//
//  The wrap ensures ANSI/UNICODE translations are handled properly for
//  legacy property sets.
//
//  pps,       // address of IPropertyStorage vtable.
//  uCodePage, //Code page value retrieved from SHCreatePropertySet
//  cpspec,    //Count of properties being read
//  rgpspec,   //Array of the properties to be read
//  rgvar      //Array of PROPVARIANTs containing the property values on return
//
function SHPropStgReadMultiple(pps : IPropertyStorage; uCodePage : UINT;
                               cpspec : ULONG; rgpspec : PPROPSPEC;
                               rgvar : PPROPVARIANT) : hresult; stdcall;

//  SHPropStgWriteMultiple()
//  IPropertyStorage::WriteMultiple wrap
//
//  The wrap ensures ANSI/UNICODE translations are handled properly for
//  legacy property sets.
//
//  pps,       // address of IPropertyStorage vtable.
//  uCodePage, // code page retrieved from SHCreatePropertySet.
//  cpspec,    // The number of properties being set
//  rgpspec,   // Property specifiers
//  rgvar,     // Array of PROPVARIANT values
//  propidNameFirst // Minimum value for property identifiers. This value should be >= PID_FIRST_USABLE
//
function SHPropStgWriteMultiple(pps : IPropertyStorage; uCodePage : UINT;
                                cpspec : ULONG; rgpspec : PPROPSPEC;
                                rgvar : PPROPVARIANT; propidNameFirst : PROPID) : HResult; stdcall;

function SHCreateFileExtractIcon(pszFile : LPCSTR; dwFileAttributes : DWORD; const riid : TIID; out ppv) : hresult; stdcall;
function SHCreateFileExtractIconA(pszFile : LPCSTR; dwFileAttributes : DWORD; const riid : TIID; out ppv) : hresult; stdcall;
function SHCreateFileExtractIconW(pszFile : LPCWSTR; dwFileAttributes : DWORD; const riid : TIID; out ppv : pointer) : hresult; stdcall;

function SHLimitInputEdit(hwndEdit : hwnd; psf : IShellFolder) : hresult; stdcall;

//
// The SHMultiFileProperties API displays a property sheet for a
// set of files specified in an IDList Array.
//
// Parameters:
//      pdtobj  - Data object containing list of files.  The data
//                object must provide the "Shell IDList Array"
//                clipboard format.  The parent folder's implementation of
//                IShellFolder::GetDisplayNameOf must return a fully-qualified
//                filesystem path for each item in response to the
//                SHGDN_FORPARSING flag.
//
//      dwFlags - Reserved for future use.  Should be set to 0.
//
// Returns:
//      S_OK
//
function SHMultiFileProperties(pdtObj : IDataObject; dwFlags : DWORD) : hresult; stdcall;

type
  PFNASYNCICONTASKBALLBACK = procedure(pidl : PItemIdList; pvData,pvHint : Pointer; iIconIndex, iOpenIconIndex : integer); stdcall;

// HRESULT SHMapIDListToImageListIndexAsync(IShellTaskScheduler* pts, IShellFolder *psf, LPCITEMIDLIST pidl, UINT flags,
//                                            PFNASYNCICONTASKBALLBACK pfn, LPVOID pvData, LPVOID pvHint, int *piIndex, int *piIndexSel);
// A usefull function for asynchronously mapping idlist into index into system
// image list.  Optionally it can also look up the index of the selected icon.
// pts          Task scheduler interface to use to create the background task
// psf          Shell folder relating to the pidl
// pidl         Item whose icon is requested
// flags        GIL_ flags
// pfn          Function called back when the background task is done
// pvData       User data passed back in the (*pfn) callback
// pvHint       User data passed back in the (*pfn) callback
// piIndex      Icon index returned. This is the temporary index if the function returns E_PENDING. The final index will be provided thru the callback
// piIndexSel   Optional icon index for the open icon case (GIL_OPENICON).
//
// Returns S_OK if all the requested info was available. E_PENDING means that you get temporary icons, and will be called back
//              asynchronously with the final icons. Other failure code means the function failed.
function SHMapIDListToImageListIndexAsync(pts : IShellTaskScheduler; psf : IShellFolder;
                                          pidl : PItemIdList; flags : UINT;
                                          pfn : PFNASYNCICONTASKBALLBACK;
                                          pvData, pvHint : pointer;
                                          var piIndex, piIndexSel : integer) : HResult; stdcall;

// A usefull function in Defview for mapping idlist into index into system
// image list.  Optionally it can also look up the index of the selected
// icon.
function SHMapPIDLToSystemImageListIndex(pshf : IShellFolder; pidl : PItemIdList; var piIndexSel : integer) : integer; stdcall;

function SHCLSIDFromString(lpsz : PWideChar; var clsid : TCLSID) : HResult; stdcall;

function _SHAllocShared(pvData : pointer; dwSize : dword; dwDestinationProcessId : dword) : THandle; stdcall;
function _SHFreeShared(hData : THandle; dwSourceProcessId : DWORD) : bool; stdcall;
function _SHLockShared(hData : THandle; dwSourceProcessId : DWORD) : Pointer; stdcall;
function _SHUnlockShared(pvData : Pointer) : bool; stdcall;
function SHFlushClipboard : HResult; stdcall;
function SHCreateQueryCancelAutoPlayMoniker(var ppMoniker : IMoniker) : HResult; stdcall;
function SHGetShellStyleHInstance : THandle; stdcall;
procedure PerUserInit; stdcall;
function SHRunControlPanel(lpcszCmdLine : LPCWSTR; hwndMsgParent : HWND) : bool; stdcall;

function PickIconDlg(hWndOwner : HWND; szIconPath : PWideChar; MaxPath : cardinal; var IconIndex : cardinal) : integer; stdcall;

type
  AASHELLMENUFILENAME = record
    cbTotal : shortint;
    rgbReserved : array[0..11] of byte;
    szFileName : array[0..0] of WideChar; 	// variable length string
  end;
  PAAShellMenuFilename = ^AASHELLMENUFILENAME;
  TAAShellMenuFilename = AASHELLMENUFILENAME;

  AASHELLMENUITEM = record
    lpReserved1 : Pointer;
    iReserved : integer;
    uiReserved : UINT;
    lpName : PAAShellMenuFilename; // name of file
    psz : LPWSTR; 			// text to use if no file
  end;
  PAAShellMenuItem = ^AASHELLMENUITEM;
  TAAShellMenuItem = AASHELLMENUITEM;

function SHGetAttributesFromDataObject(pdo : IDataObject; dwAttributeMask : DWORD; var pdwAttributes : DWORD; var pcItems : UINT) : HResult; stdcall;

function ImportPrivacySettings(szFilename :  LPCWSTR;
                               var pfParsePrivacyPreferences : bool;
                               var pfParsePerSiteRules : bool): bool; stdcall;

type
  IEnumPrivacyRecords = interface
    ['{3050f844-98b5-11cf-bb82-00aa00bdce0b}']
    function Reset : hresult; stdcall;
    function GetSize(var pSize : cardinal) : hresult; stdcall;
    function GetPrivacyImpacted(var pState : bool) : hresult; stdcall;
    function Next(var pbstrPolicyRef, pbstrUrl : POleStr; var pdwReserved : integer; var pdwPrivacyFlags : DWORD) : hresult; stdcall;
  end;

function DoPrivacyDlg(hwndParent : hwnd; pszUrl : POLESTR; pPrivacyEnum : IEnumPrivacyRecords; fReportAllSites : bool) : DWORD; stdcall;

var
  ShellMajorVersion : cardinal;
  ShellMinorVersion : cardinal;

//------------------------------------------------------------------------

implementation

function ShellFolderView_ReArrange(wnd : hwnd; lParm : LPARAM) : BOOL;
begin
  Result := BOOL(SHShellFolderView_Message(wnd, SFVM_REARRANGE, lparm));
end;

function ShellFolderView_AddObject(wnd : HWND; pidl : PItemIdList) : LPARAM;
begin
  Result := LPARAM(SHShellFolderView_Message(wnd, SFVM_ADDOBJECT, LPARAM(pidl)));
end;

function ShellFolderView_RemoveObject(wnd : hwnd; pidl : PItemIdList) : LPARAM;
begin
  Result := LPARAM(SHShellFolderView_Message(wnd, SFVM_REMOVEOBJECT, LPARAM(pidl)));
end;

function ShellFolderView_UpdateObject(wnd : hwnd; var ppidl : PItemIdList) : LPARAM;
begin
  Result := LPARAM(SHShellFolderView_Message(wnd, SFVM_UPDATEOBJECT, LPARAM(ppidl)));
end;

function ShellFolderView_GetSelectedObjects(wnd : hwnd; var ppidl : PItemIdList) : LParam;
begin
  Result := LPARAM(SHShellFolderView_Message(wnd, SFVM_GETSELECTEDOBJECTS, LPARAM(ppidl)));
end;

procedure ShellFolderView_SetItemPos(wnd : hwnd; pidl : PItemIdList; x,y : integer);
var
  sip : TSFVSetItemPos;
begin
  sip.pidl := pidl;
  sip.pt.x := x;
  sip.pt.y := y;
  SHShellFolderView_Message(wnd, SFVM_SETITEMPOS, LPARAM(PSFVSetItemPos(@sip)));
end;

procedure ShellFolderView_SetClipboard(wnd : hwnd; dwEffect : DWORD);
begin
  SHShellFolderView_Message(wnd, SFVM_SETCLIPBOARD, LPARAM(dwEffect));
end;

procedure ShellFolderView_SetPoints(wnd : hwnd; pdtobj : IDataObject);
begin
  SHShellFolderView_Message(wnd, SFVM_SETPOINTS, LPARAM(pdtobj));
end;

const
  shell32 = 'shell32.dll';

function SHChangeNotifyRegister; external shell32 index 2;
function SHDefExtractIcon; external shell32 index 3;
function SHDefExtractIconA; external shell32 index 3;
function SHChangeNotifyDeregister; external shell32 index 4;
function SHDefExtractIconW; external shell32 index 6;
procedure PerUserInit; external shell32 index 7;
function PifMgr_OpenProperties; external shell32 index 9;
function PifMgr_GetProperties; external shell32 index 10;
function PifMgr_SetProperties; external shell32 index 11;
function SHStartNetConnectionDialog; external shell32 index 12;
function SHStartNetConnectionDialogA; external shell32 index 12;
function PifMgr_CloseProperties; external shell32 index 13;
function SHStartNetConnectionDialogW; external shell32 index 14;
function ILFindLastID; external shell32 index 16;
function ILRemoveLastID; external shell32 index 17;
function ILClone; external shell32 index 18;
function ILCloneFirst; external shell32 index 19;
function ILIsEqual; external shell32 index 21;
function DAD_DragEnterEx2; external shell32 index 22;
function ILIsParent; external shell32 index 23;
function ILFindChild; external shell32 index 24;
function ILCombine; external shell32 index 25;
function ILLoadFromStream; external shell32 index 26;
function ILSaveToStream; external shell32 index 27;
function SHILCreateFromPath; external shell32 index 28;
function PathIsExe; external shell32 index 43;
function PathMakeUniqueName; external shell32 index 47;
procedure PathQualify; external shell32 index 49;
function PathResolve; external shell32 index 51;
function RestartDialog; external shell32 index 59;
function PickIconDlg; external shell32 index 62;
function GetFileNameFromBrowse; external shell32 index 63;
function DriveType; external shell32 index 64;
function IsNetDrive; external shell32 index 66;
function Shell_MergeMenus; external shell32 index 67;
procedure SHGetSetSettings; external shell32 index 68;
function Shell_GetImageLists; external shell32 index 71;
function Shell_GetCachedImageIndex; external shell32 index 72;
function SHShellFolderView_Message; external shell32 index 73;
function SHCreateStdEnumFmtEtc; external shell32 index 74;
function PathYetAnotherMakeUniqueName; external shell32 index 75;
function SHMapPIDLToSystemImageListIndex; external shell32 index 77;
function SHOpenPropSheetW; external shell32 index 80;
function OpenRegStream; external shell32 index 85;
function SHDoDragDrop; external shell32 index 88;
function SHCloneSpecialIDList; external shell32 index 89;
function SHFindFiles; external shell32 index 90;
procedure PathGetShortPath; external shell32 index 92;
function SHGetRealIDL; external shell32 index 98;
function SHRestricted; external shell32 index 100;
function SHCoCreateInstance; external shell32 index 102;
function SignalFileOpen; external shell32 index 103;
function SHFlushClipboard; external shell32 index 121;
function DAD_AutoScroll; external shell32 index 129;
function DAD_DragEnterEx ; external shell32 index 131;
function DAD_DragLeave; external shell32 index 132;
function DAD_DragMove; external shell32 index 134;
function DAD_SetDragImage ; external shell32 index 136;
function DAD_ShowDragImage; external shell32 index 137;
function SHCLSIDFromString; external shell32 index 147;
function SHMapIDListToImageListIndexAsync; external shell32 index 148;
function SHFind_InitMenuPopup; external shell32 index 149;
function SHLoadOLE; external shell32 index 151;
function ILGetSize; external shell32 index 152;
function ILAppendId; external shell32 index 154;
procedure ILFree; external shell32 index 155;
function ILCreateFromPathW; external shell32 index 157;
function SHRunControlPanel; external shell32 index 161;
function SHSimpleIDListFromPath; external shell32 index 162;
function Win32DeleteFile; external shell32 index 164;
function SHCreateDirectory; external shell32 index 165;
function CallCPLEntry16; external shell32 index 166;
function SHAddFromPropSheetExtArray; external shell32 index 167;
function SHCreatePropSheetExtArray; external shell32 index 168;
procedure SHDestroyPropSheetExtArray; external shell32 index 169;
function SHReplaceFromPropSheetExtArray; external shell32 index 170;
function PathCleanupSpec; external shell32 index 171;
function SHValidateUNC; external shell32 index 173;
function SHCreateShellFolderViewEx; external shell32 index 174;
procedure SHSetInstanceExplorer; external shell32 index 176;
function SHObjectProperties; external shell32 index 178;
function ILCreateFromPath; external shell32 index 189;
function ILCreateFromPathA; external shell32 index 189;
procedure SHUpdateImage; external shell32 index 191;
procedure SHUpdateImageA; external shell32 index 191;
procedure SHUpdateImageW; external shell32 index 192;
function SHHandleUpdateImage; external shell32 index 193;
function SHAlloc; external shell32 index 196;
procedure SHFree; external shell32 index 195;
function PathIsSlowW; external shell32 index 239;
function PathIsSlow; external shell32 index 240;
function PathIsSlowA; external shell32 index 240;
function SHCreateShellFolderView; external shell32 index 256;
function SHCreateQueryCancelAutoPlayMoniker; external shell32 index 281;
function SHCreateShellItem; external shell32 index 282;
function SHEnableServiceObject; external shell32 index 285;
function SHGetFolderPathAndSubDir; external shell32 index 304;
function SHGetFolderPathAndSubDirA; external shell32 index 304;
function SHGetFolderPathAndSubDirW; external shell32 index 305;
function SHOpenFolderAndSelectItems; external shell32 index 328;
function SHParseDisplayName; external shell32 index 329;
function _SHAllocShared; external shell32 index 520;
function _SHLockShared; external shell32 index 521;
function _SHUnlockShared; external shell32 index 522;
function _SHFreeShared; external shell32 index 523;
function RealDriveType; external shell32 index 524;
procedure SHFlushSFCache; external shell32 index 526;
function SHChangeNotification_Lock; external shell32 index 644;
function SHChangeNotification_Unlock; external shell32 index 645;
function WriteCabinetState; external shell32 index 652;
function PathProcessCommand; external shell32 index 653;
function ReadCabinetState; external shell32 index 654;
function IsUserAnAdmin; external shell32 index 680;
function SHPropStgCreate; external shell32 index 685;
function SHPropStgReadMultiple; external shell32 index 688;
function SHPropStgWriteMultiple; external shell32 index 689;
// function CDefFolderMenu_Create2; external shell32 index 701;
function SHOpenPropSheet; external shell32 index 707;
function SHOpenPropSheetA; external shell32 index 707;
function SHGetSetFolderCustomSettings; external  shell32 index 708;
function SHGetSetFolderCustomSettingsA; external  shell32 index 708;
function SHGetSetFolderCustomSettingsW; external  shell32 index 709;
function SHMultiFileProperties; external shell32 index 716;
function RestartDialogEx; external shell32 index 728;
function SHCreateFileExtractIconW; external shell32 index 743;
function SHLimitInputEdit; external shell32 index 747;
function SHGetShellStyleHInstance; external shell32 index 749;
function SHGetAttributesFromDataObject; external shell32 index 750;

// all in previous version of shell32 so import by name
function SHPathPrepareForWrite; external shell32 name 'SHPathPrepareForWriteA';
function SHPathPrepareForWriteA; external shell32 name 'SHPathPrepareForWriteA';
function SHPathPrepareForWriteW; external shell32 name 'SHPathPrepareForWriteW';
//function SHGetIconOverlayIndex; external shell32 name 'SHGetIconOverlayIndexA';
//function SHGetIconOverlayIndexA; external shell32 name 'SHGetIconOverlayIndexA';
//function SHGetIconOverlayIndexW; external shell32 name 'SHGetIconOverlayIndexW';
function SHGetFolderPath; external shell32 name 'SHGetFolderPathA';
function SHGetFolderPathA; external shell32 name 'SHGetFolderPathA';
function SHGetFolderPathW; external shell32 name 'SHGetFolderPathW';
function SHGetFolderLocation; external shell32 name 'SHGetFolderLocation';
function SHFormatDrive; external shell32 name 'SHFormatDrive';
function SHCreateDirectoryEx; external shell32 name 'SHCreateDirectoryExA';
function SHCreateDirectoryExA; external shell32 name 'SHCreateDirectoryExA';
function SHCreateDirectoryExW; external shell32 name 'SHCreateDirectoryExW';
function SHBindToParent; external shell32 name 'ShBindToParent';

function SHCreateFileExtractIcon; external shell32 name 'SHCreateFileExtractIconA';
function SHCreateFileExtractIconA; external shell32 name 'SHCreateFileExtractIconA';
function ImportPrivacySettings; external 'shdocvw.dll' name 'ImportPrivacySettings';
function DoPrivacyDlg; external 'shdocvw.dll' name 'DoPrivacyDlg';

type
  TDllVersionInfo = record
    cbSize : dword;
    dwMajorVersion : dword;
    dwMinorVersion : dword;
    dwBuildNumber : dword;
    dwPlatformID : dword;
  end;
  TDllVersionInfo2 = record
    info1 : TDLLVersionInfo;
    dwFlags : dword;
    ullVersion : Int64;
  end;

  TDllGetVersion = function(var pdvi : TDLLVersionInfo) : hresult; stdcall;

procedure GetDllVersion;
var
  hinstDll : THandle;
  dllgetVersion : TDllGetVersion;
  dvi : TDllVersionInfo;
  hr : integer;
begin
  hinstDll := LoadLibrary('shell32.dll');

  if hinstDll > 0 then begin
    DllGetVersion := GetProcAddress(hinstDll,'DllGetVersion');
		CDefFolderMenu_Create2 := GetProcAddress(hinstDll, MakeIntResource(701));

    if Assigned(DllGetVersion) then begin
      FillChar(dvi, sizeof(dvi),#0);
      dvi.cbSize := sizeof(dvi);

      hr := DllGetVersion(dvi);

      if SUCCEEDED(hr) then begin
        ShellMajorVersion := dvi.dwMajorVersion;
        ShellMinorVersion := dvi.dwMinorVersion;
      end;
    end;

    FreeLibrary(hinstDll);
  end;
end;

initialization
  GetDllVersion;
end.