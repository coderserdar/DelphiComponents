
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit ShlIntf;

interface

{$I STD.INC}

uses
  Windows, Messages, ActiveX, ShlObj;

const
  CLSID_AutoComplete: TGUID = (
    D1:$00BB2763; D2:$6A77; D3:$11D0; D4:($A5,$35,$00,$C0,$4F,$D7,$D0,$62));
  CLSID_ACLHistory: TGUID = (
    D1:$00BB2764; D2:$6A77; D3:$11D0; D4:($A5,$35,$00,$C0,$4F,$D7,$D0,$62));
  CLSID_ACListISF: TGUID = (
    D1:$03C036F1; D2:$A186; D3:$11D0; D4:($82,$4A,$00,$AA,$00,$5B,$43,$83));
  CLSID_ACLMRU: TGUID = (
    D1:$6756A641; D2:$DE71; D3:$11D0; D4:($83,$1B,$00,$AA,$00,$5B,$43,$83));
  CLSID_ACLMulti: TGUID = (
    D1:$00BB2765; D2:$6A77; D3:$11D0; D4:($A5,$35,$00,$C0,$4F,$D7,$D0,$62));

  IID_IAutoComplete: TGUID = (
    D1:$00BB2762; D2:$6A77; D3:$11D0; D4:($A5,$35,$00,$C0,$4F,$D7,$D0,$62));
  IID_IAutoComplete2: TGUID = (
    D1:$EAC04BC0; D2:$3791; D3:$11D2; D4:($BB,$95,$00,$60,$97,$7B,$46,$4C));
  IID_IAutoCompList: TGUID = (
    D1:$00BB2760; D2:$6A77; D3:$11D0; D4:($A5,$35,$00,$C0,$4F,$D7,$D0,$62));
  IID_IACList: TGUID = (
    D1:$77A130B0; D2:$94FD; D3:$11D0; D4:($A5,$44,$00,$C0,$4F,$D7,$d0,$62));
  IID_IACList2: TGUID = (
    D1:$470141A0; D2:$5186; D3:$11D2; D4:($BB,$B6,$00,$60,$97,$7B,$46,$4C));
  IID_ICurrentWorkingDirectory: TGUID = (
    D1:$91956D21; D2:$9276; D3:$11D1; D4:($92,$1A,$00,$60,$97,$DF,$5B,$D4));
  IID_IObjMgr: TGUID = (
    D1:$00BB2761; D2:$6A77; D3:$11D0; D4:($A5,$35,$00,$C0,$4F,$D7,$D0,$62));

  SID_IFolderView              = '{CDE725B0-CCC9-4519-917E-325D72FAB4CE}';
  SID_ICommDlgBrowser2         = '{10339516-2894-11D2-9039-00C04F8EEB3E}';
  SID_IAutoComplete            = '{00BB2762-6A77-11D0-A535-00C04FD7D062}';
  SID_IAutoComplete2           = '{EAC04BC0-3791-11D2-BB95-0060977B464C}';
  SID_IACList                  = '{77A130B0-94FD-11D0-A544-00C04FD7d062}';
  SID_IACList2                 = '{470141A0-5186-11D2-BBB6-0060977B464C}';
  SID_ICurrentWorkingDirectory = '{91956D21-9276-11D1-921A-006097DF5BD4}';
  SID_IObjMgr                  = '{00BB2761-6A77-11D0-A535-00C04FD7D062}';

  WM_GETISHELLBROWSER = WM_USER + 7;

	FWF_AUTOARRANGE	= $1;
	FWF_ABBREVIATEDNAMES = $2;
	FWF_SNAPTOGRID = $4;
	FWF_OWNERDATA = $8;
	FWF_BESTFITWINDOW = $10;
	FWF_DESKTOP = $20;
	FWF_SINGLESEL = $40;
	FWF_NOSUBFOLDERS = $80;
	FWF_TRANSPARENT = $100;
	FWF_NOCLIENTEDGE = $200;
	FWF_NOSCROLL = $400;
	FWF_ALIGNLEFT = $800;
	FWF_NOICONS = $1000;
	FWF_SHOWSELALWAYS = $2000;
	FWF_NOVISIBLE = $4000;
	FWF_SINGLECLICKACTIVATE = $8000;
	FWF_NOWEBVIEW = $10000;
	FWF_HIDEFILENAMES = $20000;
	FWF_CHECKSELECT = $40000;

	FVM_ICON = 1;
	FVM_SMALLICON = 2;
	FVM_LIST = 3;
	FVM_DETAILS = 4;
	FVM_THUMBNAIL = 5;
	FVM_TILE = 6;
	FVM_THUMBSTRIP = 7;

  SVSI_DESELECT = $00000000;
  SVSI_SELECT = $00000001;
  SVSI_EDIT = $00000003;
  SVSI_DESELECTOTHERS = $00000004;
  SVSI_ENSUREVISIBLE = $00000008;
  SVSI_FOCUSED = $00000010;
  SVSI_TRANSLATEPT = $00000020;
  SVSI_SELECTIONMARK = $00000040;
  SVSI_POSITIONITEM = $00000080;
  SVSI_CHECK = $00000100;
  SVSI_NOSTATECHANGE = $80000000;

	CDB2N_CONTEXTMENU_DONE = $00000001;
	CDB2N_CONTEXTMENU_START = $00000002;

  CDB2GVF_SHOWALLFILES = $00000001;

type
	IFolderView = interface(IUnknown)
  	[SID_IFolderView]
		function GetCurrentViewMode(out ViewMode: UINT): HResult; stdcall;
		function SetCurrentViewMode(ViewMode: UINT): HResult; stdcall;
		function GetFolder(riid: TIID; out ppv): HResult; stdcall;
		function Item(iItemIndex: Integer; out ppidl: PItemIDList): HResult; stdcall;
		function ItemCount(uFlags: UINT; out pcItems: Integer): HResult; stdcall;
		function Items(uFlags: UINT; riid: TIID; out ppv): HResult; stdcall;
		function GetSelectionMarkedItem(out ppidl: PItemIDList): HResult; stdcall;
		function GetFocusedItem(out piItem: Integer): HResult; stdcall;
		function GetItemPosition(pidl: PItemIDList; out ppt: TPoint): HResult; stdcall;
		function GetSpacing(out ppt: TPoint): HResult; stdcall;
		function GetDefaultSpacing(out ppt: TPoint): HResult; stdcall;
		function GetAutoArrange: HResult; stdcall;
		function SelectItem(iItem: Integer; dwFlags: DWORD): HResult; stdcall;
		function SelectAndPositionItems(cidl: UINT; pidl: PItemIDList;
    	const apt: TPoint; dwFlags: DWORD): HResult; stdcall;
  end;

  ICommDlgBrowser2 = interface(ICommDlgBrowser)
		[SID_ICommDlgBrowser2]
    function Notify(ppshv: IShellView; dwNotifyType: DWORD): HResult; stdcall;
    function GetDefaultMenuText(ppshv: IShellView; pszText: PWideChar;
    	cchMax: Integer): HResult; stdcall;
    function GetViewFlags(out pdwFlags: DWORD): HResult; stdcall;
	end;

  IAutoComplete = interface(IUnknown)
    [SID_IAutoComplete]
    function Init(hwndEdit: HWND; punkACL: IUnknown; pwszRegKeyPath: PWideChar;
      pwszQuickComplete: PWideChar): HResult; stdcall;
    function Enable(fEnable: Boolean): HResult; stdcall;
   end;

const
  ACO_NONE               = $0000;
  ACO_AUTOSUGGEST        = $0001;
  ACO_AUTOAPPEND         = $0002;
  ACO_SEARCH             = $0004;
  ACO_FILTERPREFIXES     = $0008;
  ACO_USETAB             = $0010;
  ACO_UPDOWNKEYDROPSLIST = $0020;
  ACO_RTLREADING         = $0040;

type
  IAutoComplete2 = interface(IAutoComplete)
    [SID_IAutoComplete2]
    function SetOptions(dwFlag: DWORD): HResult; stdcall;
    function GetOptions(out dwFlag: DWORD): HResult; stdcall;
  end;

  IACList = interface(IUnknown)
    [SID_IACList]
    function Expand(pszExpand: PWideChar): HResult; stdcall;
  end;

const
  ACLO_NONE            = 0;    // don't enumerate anything
  ACLO_CURRENTDIR      = 1;    // enumerate current directory
  ACLO_MYCOMPUTER      = 2;    // enumerate MyComputer
  ACLO_DESKTOP         = 4;    // enumerate Desktop Folder
  ACLO_FAVORITES       = 8;    // enumerate Favorites Folder
  ACLO_FILESYSONLY     = 16;   // enumerate only the file system

type
  IACList2 = interface(IACList)
    [SID_IACList2]
    function SetOptions(dwFlag: DWORD): HResult; stdcall;
    function GetOptions(out pdwFlag: DWORD): HResult; stdcall;
  end;

  ICurrentWorkingDirectory = interface(IUnknown)
    [SID_ICurrentWorkingDirectory]
    function GetDirectory(pwzPath: PWideChar; cchSize: DWORD): HResult; stdcall;
    function SetDirectory(pwzPath: PWideChar): HResult; stdcall;
  end;

  IObjMgr = interface(IUnknown)
    [SID_IObjMgr]
    function Append(punk: IUnknown): HResult; stdcall;
    function Remove(punk: IUnknown): HResult; stdcall;
  end;

implementation

end.
