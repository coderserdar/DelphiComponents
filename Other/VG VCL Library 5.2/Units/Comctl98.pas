{ Begin VG Update }
{$I VG.INC }
{$D-,L-}
{ End VG Update }

////////////////////////////////////////////////////////////////////////////////
// COMCTL98                                                                   //
////////////////////////////////////////////////////////////////////////////////
// Enhanced COMMCTRL for D3                                                   //
// * some constants and functions forgoten by Borland,...                     //
////////////////////////////////////////////////////////////////////////////////
// Version 1.34                                                               //
// Date de création           : 10/03/1997                                    //
// Date dernière modification : 11/03/1999                                    //
////////////////////////////////////////////////////////////////////////////////
// Jean-Luc Mattei                                                            //
// jlucm@club-internet.fr  / jlucm@mygale.org                                 //
////////////////////////////////////////////////////////////////////////////////
//  REVISIONS :                                                               //
//                                                                            //
//  1.10 :                                                                    //
//         * Added enhanced TreeView style                                    //
//  1.20 :                                                                    //
//         * Added (some) IE4 TabControl constants                            //
//         * Added IE4 ListView Constants and Functions                       //
//           (Background images)                                              //
//         * Added IE4 DateTime Extended functions                            //
//  1.30 : * PageScroller const, type & func                                  //
//         * NativeFontControl const, type & func                             //
//         * IPAdress const, Type & Func                                      //
//         * FlatScrollBar API's                                              //
//         * New New IE4 ListView & TreeView Message                          //
//         * Begining of commctl98 (D3) & commctrlEx (D2) unification         //
//         * More TreeView const                                              //
//  1.31 : * PToolTipText Defined for D2 / BC++Buider (thanks to Alper Yazgan)//
//  1.32 : * New Messages and structures for ListView control                 //
//           TNMItemActivate instead of TNMListView                           //
//           TNMLvStateChange  and LVN_ODSTATECHANGED notification            //
//           iSubItem added to TNMLVCustomDraw structure                      //
//  1.33 : * Some errors corrected (D2)                                       //
//  1.34 : * Added IE4 Toolbar extensions                                     //
//         * IE4 WM_Notify Generic structures                                 //
////////////////////////////////////////////////////////////////////////////////

unit ComCtl98;

interface

uses Messages, Windows, CommCtrl;

{$ifndef VER100} // Many const and type are missing in D2 CommCtrl

Type
	TInitCommonControlsEx = record
          dwSize : LONGINT;
          dwICC  : LONGINT;
        end;

Const

	ICC_LISTVIEW_CLASSES = $00000001; // listview, header
	ICC_TREEVIEW_CLASSES = $00000002; // treeview, tooltips
	ICC_BAR_CLASSES      = $00000004; // toolbar, statusbar, trackbar, tooltips
	ICC_TAB_CLASSES      = $00000008; // tab, tooltips
	ICC_UPDOWN_CLASS     = $00000010; // updown
	ICC_PROGRESS_CLASS   = $00000020; // progress
	ICC_HOTKEY_CLASS     = $00000040; // hotkey
	ICC_ANIMATE_CLASS    = $00000080; // animate
	ICC_WIN95_CLASSES    = $000000FF;
	ICC_DATE_CLASSES     = $00000100; // month picker, date picker, time picker, updown
	ICC_USEREX_CLASSES   = $00000200; // comboex
        ICC_COOL_CLASSES     = $00000400; // rebar (coolbar) control

function InitCommonControlsEx(Var CC : TInitCommonControlsEx): boolean; stdcall;
procedure InitCommonControl(CClass: Longint);

{$endif}

const

//==================== CUSTOM CONTROL EX ====================================

  // IE4 ONLY
  ICC_INTERNET_CLASSES   = $00000800;
  ICC_PAGESCROLLER_CLASS = $00001000;   // page scroller
  ICC_NATIVEFNTCTL_CLASS = $00002000;   // native font control

//====== Generic IE4 WM_NOTIFY notification structures ============================

type

  PNMMouse = ^TNMMouse;
  TNMMouse = packed record
    hdr: TNMHdr;
    dwItemSpec: Longint;
    dwItemData: Longint;
    pt: TPoint;
    dwHitInfo: Longint; // any specifics about where on the item or control the mouse is
  end;

  PNMClick = PNMMouse;
  TNMClick = TNMMouse;

// Generic structure to request an object of a specific type.

  PNMObjectNotify = ^TNMObjectNotify;
  TNMObjectNotify = packed record
    hdr: TNMHdr;
    iItem: Integer;
    piid: Pointer;
    pObject: Pointer;
    hResult: HRESULT;
    dwFlags: Longint;    // control specific flags (hints as to where in iItem it hit)
  end;

// Generic structure for a key

  PNMKey = ^TNMKey;
  TNMKey = packed record
    hdr: TNMHdr;
    nVKey: Cardinal;
    uFlags: Cardinal;
  end;

// Generic structure for a character

  PNMChar = ^TNMChar;
  TNMChar = packed record
    hdr: TNMHdr;
    ch: Cardinal;
    dwItemPrev: Longint;     // Item previously selected
    dwItemNext: Longint;     // Item to be selected
  end;

{$ifndef VER100}

Type

  PToolTipText = PToolTipTextA;

Const

  MCN_FIRST              = 0-750;       // monthcal
  MCN_LAST               = 0-759;

  DTN_FIRST              = 0-760;       // datetimepick
  DTN_LAST               = 0-799;

  CBEN_FIRST             = 0-800;       // combo box ex
  CBEN_LAST              = 0-830;

  RBN_FIRST              = 0-831;       // rebar
  RBN_LAST               = 0-859;

  CCS_VERT               = $00000080;
  CCS_LEFT               = (CCS_VERT OR CCS_TOP);
  CCS_RIGHT              = (CCS_VERT OR CCS_BOTTOM);
  CCS_NOMOVEX            = (CCS_VERT OR CCS_NOMOVEY);

{$endif}

Const

  NM_CUSTOMDRAW        = (NM_FIRST-12);
  NM_HOVER             = (NM_FIRST-13);

  // IE4 ONLY
  NM_NCHITTEST         = (NM_FIRST-14);
  NM_KEYDOWN           = (NM_FIRST-15);   // uses NMKEY struct
  NM_RELEASEDCAPTURE   = (NM_FIRST-16);
  NM_SETCURSOR         = (NM_FIRST-17);   // uses NMMOUSE struct
  NM_CHAR              = (NM_FIRST-18);   // uses NMCHAR struct

  IPN_FIRST             = (0-860); // internet address
  IPN_LAST              = (0-879); // internet address

  PGM_FIRST             = $1400;  // Pager control messages

  PGN_FIRST             = (0-900); // Pager Control
  PGN_LAST              = (0-950);

  CCM_FIRST             = $2000;           // Common control shared messages
  CCM_SETBKCOLOR        = (CCM_FIRST + 1); // lParam is bkColor

  (*
typedef struct tagCOLORSCHEME {
   DWORD            dwSize;
   COLORREF         clrBtnHighlight;       // highlight color
   COLORREF         clrBtnShadow;          // shadow color
} COLORSCHEME, *LPCOLORSCHEME;
  *)

  CCM_SETCOLORSCHEME    =  (CCM_FIRST + 2); // lParam is color scheme
  CCM_GETCOLORSCHEME    =  (CCM_FIRST + 3); // fills in COLORSCHEME pointed to by lParam
  CCM_GETDROPTARGET     =  (CCM_FIRST + 4);
  CCM_SETUNICODEFORMAT  =  (CCM_FIRST + 5);
  CCM_GETUNICODEFORMAT  =  (CCM_FIRST + 6);

  //==================== CUSTOM DRAW ==========================================

// custom draw return flags
// values under 0x00010000 are reserved for global custom draw values.
// above that are for specific controls

const
	CDRF_DODEFAULT        = $00000000;
        CDRF_NEWFONT          = $00000002;
	CDRF_SKIPDEFAULT      = $00000004;

	CDRF_NOTIFYPOSTPAINT  = $00000010;
	CDRF_NOTIFYITEMDRAW   = $00000020;
        CDRF_NOTIFYPOSTERASE  = $00000040;
        CDRF_NOTIFYITEMERASE  = $00000080;
        // IE4 Only
        CDRF_NOTIFYSUBITEMDRAW = $00000020;  // flags are the same, we can distinguish by context

// drawstage flags
// values under 0x00010000 are reserved for global custom draw values.
// above that are for specific controls

	CDDS_PREPAINT         = $000000001;
	CDDS_POSTPAINT        = $000000002;
        CDDS_PREERASE         = $000000003;
        CDDS_POSTERASE        = $000000004;

// the 0x000010000 bit means it's individual item specific

	CDDS_ITEM             = $000010000;
	CDDS_ITEMPREPAINT     = (CDDS_ITEM OR CDDS_PREPAINT);
	CDDS_ITEMPOSTPAINT    = (CDDS_ITEM OR CDDS_POSTPAINT);
        CDDS_ITEMPREERASE     = (CDDS_ITEM OR CDDS_PREERASE);
        CDDS_ITEMPOSTERASE    = (CDDS_ITEM OR CDDS_POSTERASE);
        // IE4 Only
        CDDS_SUBITEM          = $00020000;
        CDDS_SUBITEMPREPAINT  = (CDDS_SUBITEM OR CDDS_PREPAINT);
	CDDS_SUBITEMPOSTPAINT = (CDDS_SUBITEM OR CDDS_POSTPAINT);
        CDDS_SUBITEMPREERASE  = (CDDS_SUBITEM OR CDDS_PREERASE);
        CDDS_SUBITEMPOSTERASE = (CDDS_SUBITEM OR CDDS_POSTERASE);

// itemState flags

	CDIS_SELECTED  =  $0001;
	CDIS_GRAYED    =  $0002;
	CDIS_DISABLED  =  $0004;
	CDIS_CHECKED   =  $0008;
	CDIS_FOCUS     =  $0010;
	CDIS_DEFAULT   =  $0020;
        CDIS_HOT       =  $0040;

Type
         PNMCustomDrawInfo = ^TNMCustomDrawInfo;
	 TNMCustomDrawInfo = packed record
		hdr : TNMHDR;
		dwDrawStage : LONGINT;
		hdc : HDC;
		rc : TRect;
		dwItemSpec : LONGINT;  // this is control specific, but it's how to specify an item.  valid only with CDDS_ITEM bit set
		uItemState : UINT;
                lItemlParam : Longint;
	 end;

         PNMLVCustomDraw = ^TNMLVCustomDraw;
         TNMLVCustomDraw = packed record
            nmcd : TNMCustomDrawInfo;
            clrText : COLORREF;
            clrTextBk : COLORREF;
            iSubItem : Integer; // IE 4 ONLY
         end;

         PNMTVCustomDraw = ^TNMTVCustomDraw;
         TNMTVCustomDraw = packed record
            nmcd : TNMCustomDrawInfo;
            clrText : COLORREF;
            clrTextBk : COLORREF;
            iLevel : Integer; // IE 4 ONLY
         end;

//================ TOOLBAR CONTROL =====================================
//                  IE4 EXTENSIONS

const

  TBSTATE_MARKED = $80;

  TBSTYLE_AUTOSIZE = $0010; // automatically calculate the cx of the button
  TBSTYLE_NOPREFIX = $0020; // if this button should not have accel prefix

  TBSTYLE_REGISTERDROP    = $4000;
  TBSTYLE_TRANSPARENT     = $8000;
  TBSTYLE_EX_DRAWDDARROWS = $00000001;

// Custom Draw Structure

type

  PNMTBCustomDraw = ^TNMTBCustomDraw;
  TNMTBCustomDraw = packed record
    nmcd: TNMCustomDrawInfo;
    hbrMonoDither: HBRUSH;
    hbrLines: HBRUSH;                // For drawing lines on buttons
    hpenLines: HPEN;                 // For drawing lines on buttons

    clrText: COLORREF;               // Color of text
    clrMark: COLORREF;               // Color of text bk when marked. (only if TBSTATE_MARKED)
    clrTextHighlight: COLORREF;      // Color of text when highlighted
    clrBtnFace: COLORREF;            // Background of the button
    clrBtnHighlight: COLORREF;       // 3D highlight
    clrHighlightHotTrack: COLORREF;  // In conjunction with fHighlightHotTrack
                                     // will cause button to highlight like a menu
    rcText: TRect;                   // Rect for text

    nStringBkMode: Integer;
    nHLStringBkMode: Integer;
  end;

const

// Toolbar custom draw return flags

  TBCDRF_NOEDGES         = $00010000;  // Don't draw button edges
  TBCDRF_HILITEHOTTRACK  = $00020000;  // Use color of the button bk when hottracked
  TBCDRF_NOOFFSET        = $00040000;  // Don't offset button if pressed
  TBCDRF_NOMARK          = $00080000;  // Don't draw default highlight of image/text for TBSTATE_MARKED
  TBCDRF_NOETCHEDEFFECT  = $00100000;  // Don't draw etched effect for disabled items

  TB_MARKBUTTON          = (WM_USER + 6);
  TB_ISBUTTONHIGHLIGHTED = (WM_USER + 14);

// icon indexes for standard view bitmap
  VIEW_VIEWMENU          = 12;

  TBIF_IMAGE    = $00000001;
  TBIF_TEXT     = $00000002;
  TBIF_STATE    = $00000004;
  TBIF_STYLE    = $00000008;
  TBIF_LPARAM   = $00000010;
  TBIF_COMMAND  = $00000020;
  TBIF_SIZE     = $00000040;

type

  PTBButtonInfoA = ^TTBButtonInfoA;
  TTBButtonInfoA = packed record
    cbSize: Cardinal;
    dwMask: Longint;
    idCommand: Integer;
    iImage: Integer;
    fsState: Byte;
    fsStyle: Byte;
    cx: Word;
    lParam: Longint;
    pszText: PAnsiChar;
    cchText: Integer;
  end;

  PTBButtonInfoW = ^TTBButtonInfoW;
  TTBButtonInfoW = packed record
    cbSize: Cardinal;
    dwMask: Longint;
    idCommand: Integer;
    iImage: Integer;
    fsState: Byte;
    fsStyle: Byte;
    cx: Word;
    lParam: Longint;
    pszText: PWideChar;
    cchText: Integer;
  end;

  TTBButtonInfo = TTBButtonInfoA;
  PTBButtonInfo = PTBButtonInfoA;


const
// BUTTONINFO APIs do NOT support the string pool.
  TB_GETBUTTONINFOW  =  (WM_USER + 63);
  TB_SETBUTTONINFOW  =  (WM_USER + 64);
  TB_GETBUTTONINFOA  =  (WM_USER + 65);
  TB_SETBUTTONINFOA  =  (WM_USER + 66);

  TB_GETBUTTONINFO   =  TB_GETBUTTONINFOA;
  TB_SETBUTTONINFO   =  TB_SETBUTTONINFOA;

  TB_GETOBJECT           = (WM_USER + 62);  // wParam == IID, lParam void **ppv
  TB_GETHOTITEM          = (WM_USER + 71);
  TB_SETHOTITEM          = (WM_USER + 72);  // wParam == iHotItem
  TB_SETANCHORHIGHLIGHT  = (WM_USER + 73);  // wParam == TRUE/FALSE
  TB_GETANCHORHIGHLIGHT  = (WM_USER + 74);
  TB_MAPACCELERATORA     = (WM_USER + 78);  // wParam == ch, lParam int * pidBtn

type

  PTBInsertMark = ^TTBInsertMark;
  TTBInsertMark = packed record
    iButton: Integer;
    dwFlags: Longint;
  end;

const

  TBIMHT_AFTER      = $00000001; // TRUE = insert After iButton, otherwise before
  TBIMHT_BACKGROUND = $00000002; // TRUE iff missed buttons completely

  TB_GETINSERTMARK      =  (WM_USER + 79);  // lParam == LPTBINSERTMARK
  TB_SETINSERTMARK      =  (WM_USER + 80);  // lParam == LPTBINSERTMARK
  TB_INSERTMARKHITTEST  =  (WM_USER + 81);  // wParam == LPPOINT lParam == LPTBINSERTMARK
  TB_MOVEBUTTON         =  (WM_USER + 82);
  TB_GETMAXSIZE         =  (WM_USER + 83);  // lParam == LPSIZE
  TB_SETEXTENDEDSTYLE   =  (WM_USER + 84);  // For TBSTYLE_EX_*
  TB_GETEXTENDEDSTYLE   =  (WM_USER + 85);  // For TBSTYLE_EX_*
  TB_GETPADDING         =  (WM_USER + 86);
  TB_SETPADDING         =  (WM_USER + 87);
  TB_SETINSERTMARKCOLOR =  (WM_USER + 88);
  TB_GETINSERTMARKCOLOR =  (WM_USER + 89);

  TB_SETCOLORSCHEME     =  CCM_SETCOLORSCHEME;  // lParam is color scheme
  TB_GETCOLORSCHEME     =  CCM_GETCOLORSCHEME;	// fills in COLORSCHEME pointed to by lParam

  TB_SETUNICODEFORMAT   =  CCM_SETUNICODEFORMAT;
  TB_GETUNICODEFORMAT   =  CCM_GETUNICODEFORMAT;

  TB_MAPACCELERATORW    =  (WM_USER + 90);  // wParam == ch, lParam int * pidBtn
  TB_MAPACCELERATOR     = TB_MAPACCELERATORA;

  TB_INSERTBUTTONW   =  (WM_USER + 67);
  TB_ADDBUTTONSW     =  (WM_USER + 68);
  TB_ADDBUTTONSA     =  (WM_USER + 20);
  TB_INSERTBUTTONA   =  (WM_USER + 21);

  TB_HITTEST         =  (WM_USER + 69);

// New post Win95/NT4 for InsertButton and AddButton.  if iString member
// is a pointer to a string, it will be handled as a string like listview
// (although LPSTR_TEXTCALLBACK is not supported).

  TB_INSERTBUTTON    =  TB_INSERTBUTTONA;
  TB_ADDBUTTONS      =  TB_ADDBUTTONSA;

  TB_SETDRAWTEXTFLAGS  = (WM_USER + 70);  // wParam == mask lParam == bit values

  TBN_GETOBJECT      = (TBN_FIRST - 12);

type
// Structure for TBN_HOTITEMCHANGE notification
//
  PNMTBHotItem = ^TNMTBHotItem;
  TNMTBHotItem = packed record
    hdr: TNMHdr;
    idOld: Integer;
    idNew: Integer;
    dwFlags: Longint;           // HICF_*
  end;

const
// Hot item change flags
  HICF_OTHER        =  $00000000;
  HICF_MOUSE        =  $00000001;          // Triggered by mouse
  HICF_ARROWKEYS    =  $00000002;          // Triggered by arrow keys
  HICF_ACCELERATOR  =  $00000004;          // Triggered by accelerator
  HICF_DUPACCEL     =  $00000008;          // This accelerator is not unique
  HICF_ENTERING     =  $00000010;          // idOld is invalid
  HICF_LEAVING      =  $00000020;          // idNew is invalid
  HICF_RESELECT     =  $00000040;          // hot item reselected

  TBN_HOTITEMCHANGE   = (TBN_FIRST - 13);
  TBN_DRAGOUT         = (TBN_FIRST - 14); // this is sent when the user clicks down on a button then drags off the button
  TBN_DELETINGBUTTON  = (TBN_FIRST - 15); // uses TBNOTIFY
  TBN_GETDISPINFOA    = (TBN_FIRST - 16); // This is sent when the  toolbar needs  some display information
  TBN_GETDISPINFOW    = (TBN_FIRST - 17); // This is sent when the  toolbar needs  some display information
  TBN_GETINFOTIPA     = (TBN_FIRST - 18);
  TBN_GETINFOTIPW     = (TBN_FIRST - 19);

type
  PNMTBGetInfoTipA = ^TNMTBGetInfoTipA;
  TNMTBGetInfoTipA = packed record
    hdr: TNMHdr;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iItem: Integer;
    lParam: Longint;
  end;

  PNMTBGetInfoTipW = ^TNMTBGetInfoTipW;
  TNMTBGetInfoTipW = packed record
    hdr: TNMHdr;
    pszText: PWideChar;
    cchTextMax: Integer;
    iItem: Integer;
    lParam: Longint;
  end;

  PNMTBGetInfoTip = PNMTBGetInfoTipA;
  TNMTBGetInfoTip = TNMTBGetInfoTipA;

const
  TBN_GETINFOTIP = TBN_GETINFOTIPA;

  TBNF_IMAGE       = $00000001;
  TBNF_TEXT        = $00000002;
  TBNF_DI_SETITEM  = $10000000;

type

  PNMTBDispInfoA = ^TNMTBDispInfoA;
  TNMTBDispInfoA = packed record
    hdr: TNMHdr;
    dwMask: Longint;     // [in] Specifies the values requested .[out] Client ask the data to be set for future use
    idCommand: Integer;    // [in] id of button we're requesting info for
    lParam: Longint;     // [in] lParam of button
    iImage: Integer;       // [out] image index
    pszText: PAnsiChar;    // [out] new text for item
    cchText: Integer;      // [in] size of buffer pointed to by pszText
  end;

  PNMTBDispInfoW = ^TNMTBDispInfoW;
  TNMTBDispInfoW = packed record
    hdr: TNMHdr;
    dwMask: Longint;     // [in] Specifies the values requested .[out] Client ask the data to be set for future use
    idCommand: Integer;    // [in] id of button we're requesting info for
    lParam: Longint;     // [in] lParam of button
    iImage: Integer;       // [out] image index
    pszText: PWideChar;    // [out] new text for item
    cchText: Integer;      // [in] size of buffer pointed to by pszText
  end;

  PNMTBDispInfo = PNMTBDispInfoA;
  TNMTBDispInfo = TNMTBDispInfoA;

const
  TBN_GETDISPINFO = TBN_GETDISPINFOA;

// Return codes for TBN_DROPDOWN
  TBDDRET_DEFAULT       =  0;
  TBDDRET_NODEFAULT     =  1;
  TBDDRET_TREATPRESSED  =  2;       // Treat as a standard press button

//================ DATETIMEPICK CONTROL =====================================
//                   IE4 EXTENSIONS

const

{ Begin VG Update }
{$IFNDEF _D3_}
  DTM_FIRST        = $1000;
{$ENDIF}
{ End VG Update }

  DTM_SETMCFONT    = (DTM_FIRST + 9);
  DTM_GETMCFONT    = (DTM_FIRST + 10);

   procedure DateTime_SetMonthCalFont(hdp: HWND; hfont: HFONT; fRedraw: Bool);
   //SNDMSG(hdp, DTM_SETMCFONT, (WPARAM)hfont, (LPARAM)fRedraw)
   procedure DateTime_GetMonthCalFont(hdp: HWND);
   //SNDMSG(hdp, DTM_GETMCFONT, 0, 0)

//==================== TAB CONTROL ==========================================

{$ifndef VER100}

const

	TCS_SCROLLOPPOSITE    = $0001;   // assumes multiline tab
	TCS_BOTTOM            = $0002;
	TCS_RIGHT             = $0002;
	TCS_VERTICAL          = $0080;   // only valid with multiline mode
        TCS_HOTTRACK          = $0040;

	TCIF_RTLREADING       = $0004;
	TCIF_STATE            = $0010;

	TCIS_BUTTONPRESSED    = $0001;

{$endif}

const

  TCS_MULTISELECT       = $0004;
  // IE4 ONLY
  TCS_FLATBUTTONS       = $0008;
  // IE4 ONLY
  TCS_EX_FLATSEPARATORS = $00000001;
  // IE4 ONLY
  TCS_EX_REGISTERDROP   = $00000002;

  // IE4 ONLY
  TCIS_HIGHLIGHTED      = $0002;

  // IE4 ONLY
  TCM_HIGHLIGHTITEM     = (TCM_FIRST + 51);
  TCM_SETEXTENDEDSTYLE  = (TCM_FIRST + 52);  // optional wParam == mask
  TCM_GETEXTENDEDSTYLE  = (TCM_FIRST + 53);

  function TabCtrl_HighlightItem(hwnd: HWND; i : Cardinal; fHighlight : Bool): Bool;
  // (BOOL)SNDMSG((hwnd), TCM_HIGHLIGHTITEM, (WPARAM)i, (LPARAM)MAKELONG (fHighlight, 0))

  function TabCtrl_SetExtendedStyle(hwnd: HWND; dw: Longint): Longint;
  // (DWORD)SNDMSG((hwnd), TCM_SETEXTENDEDSTYLE, 0, dw)

  function TabCtrl_GetExtendedStyle(hwnd: HWND): Longint;
  // (DWORD)SNDMSG((hwnd), TCM_GETEXTENDEDSTYLE, 0, 0)

  function TabCtrl_GetItemRect(hwnd: HWND; i: Integer; Var prc: TRect): Boolean;
  // (BOOL)SNDMSG((hwnd), TCM_GETITEMRECT, (WPARAM)(int)(i), (LPARAM)(RECT FAR*)(prc))

Const

  // IE4 ONLY
  TCN_GETOBJECT         = (TCN_FIRST - 3);

  //==================== TREE VIEW ==========================================

  TVS_NOTOOLTIPS        = $0080;
  TVS_CHECKBOXES        = $0100;
  TVS_HOTTRACK          = $0200;

  TVS_SHAREDIMAGELISTS  = $0000;
  TVS_PRIVATEIMAGELISTS = $0400;

  TVS_SINGLEEXPAND      = $0400;
  TVS_INFOTIP           = $0800;
  TVS_FULLROWSELECT     = $1000;
  TVS_NOSCROLL          = $2000;
  TVS_NONEVENHEIGHT     = $4000;

  TVIF_INTEGRAL         = $0080;

  TVGN_LASTVISIBLE      = $000A;

  TVN_GETINFOTIPA       = (TVN_FIRST-13);
  TVN_GETINFOTIPW       = (TVN_FIRST-14);
  TVN_SINGLEEXPAND      = (TVN_FIRST-15);

//  #define TreeView_GetLastVisible(hwnd)
//    TreeView_GetNextItem(hwnd, NULL,  TVGN_LASTVISIBLE)

(*
#define TVM_SETINSERTMARK       (TV_FIRST + 26)
#define TreeView_SetInsertMark(hwnd, hItem, fAfter) \
        (BOOL)SNDMSG((hwnd), TVM_SETINSERTMARK, (WPARAM) (fAfter), (LPARAM) (hItem))

#define TVM_SETUNICODEFORMAT     CCM_SETUNICODEFORMAT
#define TreeView_SetUnicodeFormat(hwnd, fUnicode)  \
    (BOOL)SNDMSG((hwnd), TVM_SETUNICODEFORMAT, (WPARAM)(fUnicode), 0)

#define TVM_GETUNICODEFORMAT     CCM_GETUNICODEFORMAT
#define TreeView_GetUnicodeFormat(hwnd)  \
    (BOOL)SNDMSG((hwnd), TVM_GETUNICODEFORMAT, 0, 0)

#define TVM_SETITEMHEIGHT         (TV_FIRST + 27)
#define TreeView_SetItemHeight(hwnd,  iHeight) \
    (int)SNDMSG((hwnd), TVM_SETITEMHEIGHT, (WPARAM)iHeight, 0)
#define TVM_GETITEMHEIGHT         (TV_FIRST + 28)
#define TreeView_GetItemHeight(hwnd) \
    (int)SNDMSG((hwnd), TVM_GETITEMHEIGHT, 0, 0)

#define TVM_SETBKCOLOR              (TV_FIRST + 29)
#define TreeView_SetBkColor(hwnd, clr) \
    (COLORREF)SNDMSG((hwnd), TVM_SETBKCOLOR, 0, (LPARAM)clr)

#define TVM_SETTEXTCOLOR              (TV_FIRST + 30)
#define TreeView_SetTextColor(hwnd, clr) \
    (COLORREF)SNDMSG((hwnd), TVM_SETTEXTCOLOR, 0, (LPARAM)clr)

#define TVM_GETBKCOLOR              (TV_FIRST + 31)
#define TreeView_GetBkColor(hwnd) \
    (COLORREF)SNDMSG((hwnd), TVM_GETBKCOLOR, 0, 0)

#define TVM_GETTEXTCOLOR              (TV_FIRST + 32)
#define TreeView_GetTextColor(hwnd) \
    (COLORREF)SNDMSG((hwnd), TVM_GETTEXTCOLOR, 0, 0)

#define TVM_SETSCROLLTIME              (TV_FIRST + 33)
#define TreeView_SetScrollTime(hwnd, uTime) \
    (UINT)SNDMSG((hwnd), TVM_SETSCROLLTIME, uTime, 0)

#define TVM_GETSCROLLTIME              (TV_FIRST + 34)
#define TreeView_GetScrollTime(hwnd) \
    (UINT)SNDMSG((hwnd), TVM_GETSCROLLTIME, 0, 0)


#if (_WIN32_IE >= 0x0400)
#define TVM_SETINSERTMARKCOLOR              (TV_FIRST + 37)
#define TreeView_SetInsertMarkColor(hwnd, clr) \
    (COLORREF)SNDMSG((hwnd), TVM_SETINSERTMARKCOLOR, 0, (LPARAM)clr)
#define TVM_GETINSERTMARKCOLOR              (TV_FIRST + 38)
#define TreeView_GetInsertMarkColor(hwnd) \
    (COLORREF)SNDMSG((hwnd), TVM_GETINSERTMARKCOLOR, 0, 0)
#endif  /* (_WIN32_IE >= 0x0400) */

// for tooltips

typedef struct tagNMTVGETINFOTIPA
{
    NMHDR hdr;
    LPSTR pszText;
    int cchTextMax;
    HTREEITEM hItem;
    LPARAM lParam;
} NMTVGETINFOTIPA, *LPNMTVGETINFOTIPA;

typedef struct tagNMTVGETINFOTIPW
{
    NMHDR hdr;
    LPWSTR pszText;
    int cchTextMax;
    HTREEITEM hItem;
    LPARAM lParam;
} NMTVGETINFOTIPW, *LPNMTVGETINFOTIPW;


#ifdef UNICODE
#define TVN_GETINFOTIP          TVN_GETINFOTIPW
#define NMTVGETINFOTIP          NMTVGETINFOTIPW
#define LPNMTVGETINFOTIP        LPNMTVGETINFOTIPW
#else
#define TVN_GETINFOTIP          TVN_GETINFOTIPA
#define NMTVGETINFOTIP          NMTVGETINFOTIPA
#define LPNMTVGETINFOTIP        LPNMTVGETINFOTIPA
#endif

// treeview's customdraw return meaning don't draw images.  valid on CDRF_NOTIFYITEMPREPAINT
#define TVCDRF_NOIMAGES         0x00010000
*)

  //==================== HEADER CONTROL ==========================================

type
  PHDItemExA = ^THDItemExA;
  PHDItemExW = ^THDItemExW;
  THDItemExA = packed record
    Mask: Cardinal;
    cxy: Integer;
    pszText: PAnsiChar;
    hbm: HBITMAP;
    cchTextMax: Integer;
    fmt: Integer;
    lParam: LPARAM;
    iImage: Integer;
    iOrder: Integer;
  end;
  THDItemExW = packed record
    Mask: Cardinal;
    cxy: Integer;
    pszText: PWideChar;
    hbm: HBITMAP;
    cchTextMax: Integer;
    fmt: Integer;
    lParam: LPARAM;
    iImage: Integer;
    iOrder: Integer;
  end;
  THDItemEx = THDItemExA;
  PHDItemEx = PHDItemExA;

const
  HDI_IMAGE      = $0020;
  HDI_DI_SETITEM = $0040;
  HDI_ORDER      = $0080;

  HDF_BITMAP_ON_RIGHT = $01000;
  HDF_IMAGE  = $0800;

  function Header_InsertItemEx(Header: HWnd; Index: Integer;
  const Item: THDItemEx): Integer;
  function Header_GetItemEx(Header: HWnd; Index: Integer;
  var Item: THDItemEx): Bool;
  function Header_SetItemEx(Header: HWnd; Index: Integer; const Item: THDItemEx): Bool;

const
  HDM_GETITEMRECT = (HDM_FIRST + 7);
  HDM_SETIMAGELIST = (HDM_FIRST + 8);
  HDM_GETIMAGELIST = (HDM_FIRST + 9);
  HDM_ORDERTOINDEX = (HDM_FIRST + 15);
  HDM_CREATEDRAGIMAGE = (HDM_FIRST + 16);  // wparam = which item (by index)
  HDM_GETORDERARRAY = (HDM_FIRST + 17);
  HDM_SETORDERARRAY = (HDM_FIRST + 18);
  HDM_SETHOTDIVIDER = (HDM_FIRST + 19);

  function Header_GetItemRect(Header: HWnd; Index: Integer; var Rect: TRect): BOOL;
       // (BOOL)SNDMSG((hwnd), HDM_GETITEMRECT, (WPARAM)iItem, (LPARAM)lprc)

  function Header_SetImageList(Header: HWnd; iImageList: Integer): HImageList;
       // (HIMAGELIST)SNDMSG((hwnd), HDM_SETIMAGELIST, 0, (LPARAM)himl)

  function Header_GetImageList(Header: HWnd): HImageList;
       // (HIMAGELIST)SNDMSG((hwnd), HDM_GETIMAGELIST, 0, 0)

  function Header_OrderToIndex(Header: HWnd; Order: Integer): Integer;
       // (int)SNDMSG((hwnd), HDM_ORDERTOINDEX, (WPARAM)i, 0)

  function Header_CreateDragImage(Header: HWnd; Index: Integer): HImageList;
       // (HIMAGELIST)SNDMSG((hwnd), HDM_CREATEDRAGIMAGE, (WPARAM)i, 0)

  function Header_GetOrderArray(Header: HWnd; iCount: Integer; pi: PInteger): BOOL;
       // (BOOL)SNDMSG((hwnd), HDM_GETORDERARRAY, (WPARAM)iCount, (LPARAM)lpi)

  function Header_SetOrderArray(Header: HWnd; iCount: Integer; pi: PInteger): BOOL;
       // (BOOL)SNDMSG((hwnd), HDM_SETORDERARRAY, (WPARAM)iCount, (LPARAM)lpi)
// lparam = int array of size HDM_GETITEMCOUNT
// the array specifies the order that all items should be displayed.
// e.g.  { 2, 0, 1}
// says the index 2 item should be shown in the 0ths position
//      index 0 should be shown in the 1st position
//      index 1 should be shown in the 2nd position

   function Header_SetHotDivider(Header: HWnd; fPos: BOOL; dw: Longint): Integer;
       // (int)SNDMSG((hwnd), HDM_SETHOTDIVIDER, (WPARAM)fPos, (LPARAM)dw)
// convenience message for external dragdrop
// wParam = BOOL  specifying whether the lParam is a dwPos of the cursor
//              position or the index of which divider to hotlight
// lParam = depends on wParam  (-1 and wParm = FALSE turns off hotlight)

  //==================== LIST VIEW ============================================
  //                  IE 4 EXTENSIONS
const

{$IFNDEF _D3_ }
{ TListView Ex-style }
  LVS_EX_GRIDLINES        = $00000001;
  LVS_EX_SUBITEMIMAGES    = $00000002;
  LVS_EX_CHECKBOXES       = $00000004;
  LVS_EX_TRACKSELECT      = $00000008;
  LVS_EX_HEADERDRAGDROP   = $00000010;
  LVS_EX_FULLROWSELECT    = $00000020; // applies to report mode only
  LVS_EX_ONECLICKACTIVATE = $00000040;
  LVS_EX_TWOCLICKACTIVATE = $00000080;
{$ENDIF }

  LVS_EX_FLATSB          = $00000100; // cannot be cleared
  LVS_EX_REGIONAL        = $00000200;
  LVS_EX_INFOTIP         = $00000400; // listview does InfoTips for you

  LVS_EX_UNDERLINEHOT    = $00000800;
  LVS_EX_UNDERLINECOLD   = $00001000;
  LVS_EX_MULTIWORKAREAS  = $00002000;

  // these flags only apply to LVS_OWNERDATA listviews in report or list mode
  LVSICF_NOINVALIDATEALL = $00000001;
  LVSICF_NOSCROLL        = $00000002;
  LVIF_INDENT            = $0010;
  LVIF_NORECOMPUTE       = $0800;

  LVM_GETSELECTIONMARK   = (LVM_FIRST + 66);
  LVM_SETSELECTIONMARK   = (LVM_FIRST + 67);
  LVM_GETWORKAREA        = (LVM_FIRST + 70);
  LVM_SETHOVERTIME       = (LVM_FIRST + 71);
  LVM_GETHOVERTIME       = (LVM_FIRST + 72);

  function ListView_GetSelectionMark(hwnd: HWND): Integer;
  //  (int)SNDMSG((hwnd), LVM_GETSELECTIONMARK, 0, 0)

  function ListView_SetSelectionMark(hwnd: HWND; i: Cardinal): Integer;
  //  (int)SNDMSG((hwnd), LVM_GETSELECTIONMARK, 0, (LPARAM)i)

  function ListView_GetWorkArea(hwnd: HWND; Var prc: TRect): Bool;
  //  (BOOL)SNDMSG((hwnd), LVM_GETWORKAREA, 0, (LPARAM)(RECT FAR*)(prc))

  function ListView_SetHoverTime(hwndLV: HWND; dwHoverTimeMs: Longint): Longint;
  //  (DWORD)SendMessage((hwndLV), LVM_SETHOVERTIME, 0, dwHoverTimeMs)

  function ListView_GetHoverTime(hwndLV: HWND): Longint;
  //  (DWORD)SendMessage((hwndLV), LVM_GETHOVERTIME, 0, 0)

const

  LV_MAX_WORKAREAS         = 16;
  LVM_SETWORKAREAS         = (LVM_FIRST + 65);
  LVM_GETWORKAREAS         = (LVM_FIRST + 70);
  LVM_GETNUMBEROFWORKAREAS = (LVM_FIRST + 73);
  //LVM_GETSELECTIONMARK     = (LVM_FIRST + 66);
  //LVM_SETSELECTIONMARK     = (LVM_FIRST + 67);
  LVM_SETTOOLTIPS          = (LVM_FIRST + 74);
  LVM_GETTOOLTIPS          = (LVM_FIRST + 78);

  (*
  #define ListView_SetWorkAreas(hwnd, nWorkAreas, prc) \
    (BOOL)SNDMSG((hwnd), LVM_SETWORKAREAS, (WPARAM)(int)nWorkAreas, (LPARAM)(RECT FAR* )(prc))

  #define ListView_GetWorkAreas(hwnd, nWorkAreas, prc) \
    (BOOL)SNDMSG((hwnd), LVM_GETWORKAREAS, (WPARAM)(int)nWorkAreas, (LPARAM)(RECT FAR* )(prc))

  #define ListView_GetNumberOfWorkAreas(hwnd, pnWorkAreas) \
    (BOOL)SNDMSG((hwnd), LVM_GETNUMBEROFWORKAREAS, 0, (LPARAM)(UINT * )(pnWorkAreas))

  #define ListView_GetSelectionMark(hwnd) \
    (int)SNDMSG((hwnd), LVM_GETSELECTIONMARK, 0, 0)

  #define ListView_SetSelectionMark(hwnd, i) \
    (int)SNDMSG((hwnd), LVM_SETSELECTIONMARK, 0, (LPARAM)i)

  #define ListView_SetToolTips(hwndLV, hwndNewHwnd)\
        (HWND)SendMessage((hwndLV), LVM_SETTOOLTIPS, hwndNewHwnd, 0)

  #define ListView_GetToolTips(hwndLV)\
        (HWND)SendMessage((hwndLV), LVM_GETTOOLTIPS, 0, 0)
  *)

type

  PLVBkImageA = ^TLVBkImageA;
  TLVBkImageA = packed record
    ulFlags: Longint;              // LVBKIF_*
    hbm: HBITMAP;
    pszImage: PAnsiChar;
    cchImageMax: Cardinal;
    xOffsetPercent: Integer;
    yOffsetPercent: Integer;
  end;

  PLVBkImageW = ^TLVBkImageW;
  TLVBkImageW = packed record
    ulFlags: Longint;              // LVBKIF_*
    hbm: HBITMAP;
    pszImage: PWideChar;
    cchImageMax: Cardinal;
    xOffsetPercent: Integer;
    yOffsetPercent: Integer;
  end;
  TLvBkImage = TLvBkImageA;
  PLvBkImage = PLvBkImageA;

const

  LVBKIF_SOURCE_NONE     = $00000000;
  LVBKIF_SOURCE_HBITMAP  = $00000001; // Not Implemented in CommCtl32.dd V 4.71
  LVBKIF_SOURCE_URL      = $00000002;
  LVBKIF_SOURCE_MASK     = $00000003;
  LVBKIF_STYLE_NORMAL    = $00000000;
  LVBKIF_STYLE_TILE      = $00000010;
  LVBKIF_STYLE_MASK      = $00000010;

  LVM_SETBKIMAGEA        = (LVM_FIRST + 68);
  LVM_SETBKIMAGEW        = (LVM_FIRST + 138);
  LVM_GETBKIMAGEA        = (LVM_FIRST + 69);
  LVM_GETBKIMAGEW        = (LVM_FIRST + 139);

  {$ifdef UNICODE}
  LVM_SETBKIMAGE         = LVM_SETBKIMAGEW;
  LVM_GETBKIMAGE         = LVM_GETBKIMAGEW;
  {$else}
  LVM_SETBKIMAGE         = LVM_SETBKIMAGEA;
  LVM_GETBKIMAGE         = LVM_GETBKIMAGEA;
  {$endif}

  // CoInitialize MUST BE CALLED BEFORE USING THIS FUNCTION

  function ListView_SetBkImage(hwnd: HWND; Var BkImage: TLVBkImage): Bool;
  //  (BOOL)SNDMSG((hwnd), LVM_SETBKIMAGE, 0, (LPARAM)plvbki)

  function ListView_GetBkImage(hwnd: HWND; Var BkImage: TLVBkImage): Bool;
  //  (BOOL)SNDMSG((hwnd), LVM_GETBKIMAGE, 0, (LPARAM)plvbki)

type

  PNMLvGetInfoTipA = ^TNMLvGetInfoTipA;
  TNMLvGetInfoTipA = packed record
    hdr: TNMHDR;
    dwFlags: Longint;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iItem: Integer;
    iSubItem: Integer;
    lParam: Longint;
  end;

  PNMLvGetInfoTipW = ^TNMLvGetInfoTipW;
  TNMLvGetInfoTipW = packed record
    hdr: TNMHDR;
    dwFlags: Longint;
    pszText: PWidechar;
    cchTextMax: Integer;
    iItem: Integer;
    iSubItem: Integer;
    lParam: Longint;
  end;

  TNMLvGetInfoTip       = TNMLvGetInfoTipA;
  PNMLvGetInfoTip       = PNMLvGetInfoTipA;

  PNMLvStateChange = ^TNMLvStateChange;
  TNMLvStateChange = packed record
    hdr: TNMHDR;
    iFrom: Integer;
    iTo: Integer;
    uNewState: UINT;
    uOldState: UINT;
  end;

// NMITEMACTIVATE is used instead of NMLISTVIEW in IE >= 0x400
// therefore all the fields are the same except for extra uKeyFlags
// they are used to store key flags at the time of the single click with
// delayed activation - because by the time the timer goes off a user may
// not hold the keys (shift, ctrl) any more

  PNMItemActivate = ^TNMItemActivate;
  TNMItemActivate = packed record
    hdr: TNMHDR;
    iItem: Integer;
    iSubItem: Integer;
    uNewState: UINT;
    uOldState: UINT;
    uChanged: UINT;
    ptAction: TPoint;
    lParam: LPARAM;
    uKeyFlags: UINT;
  end;

const

  // key flags stored in uKeyFlags
  LVKF_ALT      = $0001;
  LVKF_CONTROL  = $0002;
  LVKF_SHIFT    = $0004;

// TNMLVGETINFOTIPA.dwFlag values

  LVGIT_UNFOLDED = $0001;

  LVN_GETINFOTIPA = (LVN_FIRST-57);
  LVN_GETINFOTIPW = (LVN_FIRST-58);

  {$ifdef UNICODE}
  LVN_GETINFOTIP        = LVN_GETINFOTIPW;
  {$else}
  LVN_GETINFOTIP        = LVN_GETINFOTIPA;
  {$endif}
  LVN_ITEMACTIVATE      = (LVN_FIRST-14);
  LVN_ODSTATECHANGED    = (LVN_FIRST-15);

(*
#define LVM_SETUNICODEFORMAT     CCM_SETUNICODEFORMAT
#define ListView_SetUnicodeFormat(hwnd, fUnicode)  \
    (BOOL)SNDMSG((hwnd), LVM_SETUNICODEFORMAT, (WPARAM)(fUnicode), 0)

#define LVM_GETUNICODEFORMAT     CCM_GETUNICODEFORMAT
#define ListView_GetUnicodeFormat(hwnd)  \
    (BOOL)SNDMSG((hwnd), LVM_GETUNICODEFORMAT, 0, 0)

#define ListView_SetExtendedListViewStyleEx(hwndLV, dwMask, dw)\
        (DWORD)SNDMSG((hwndLV), LVM_SETEXTENDEDLISTVIEWSTYLE, dwMask, dw)
*)


  //==================== IP ADRESS EDIT CONTROL============================================
  //                         IE 4 EXTENSIONS

// Messages sent to IPAddress controls

const

  IPM_CLEARADDRESS = (WM_USER+100); // no parameters
  IPM_SETADDRESS   = (WM_USER+101); // lparam = TCP/IP address
  IPM_GETADDRESS   = (WM_USER+102); // lresult = # of non black fields.  lparam = LPDWORD for TCP/IP address
  IPM_SETRANGE     = (WM_USER+103); // wparam = field, lparam = range
  IPM_SETFOCUS     = (WM_USER+104); // wparam = field
  IPM_ISBLANK      = (WM_USER+105); // no parameters

  WC_IPADDRESS     = 'SysIPAddress32';

  IPN_FIELDCHANGED = (IPN_FIRST - 0);

type

  PNMIPAdress = ^TNMIPAdress;
  TNMIPAdress = packed record
    hdr: TNMHDR;
    iField: Integer;
    iValue: Integer;
  end;

// The following is a useful macro for passing the range values in the
// IPM_SETRANGE message.

function MakeIPRange(Low, High : Byte): Longint;
  //#define MAKEIPRANGE(low, high)    ((LPARAM)(WORD)(((BYTE)(high) << 8) + (BYTE)(low)))

// And this is a useful macro for making the IP Address to be passed
// as a LPARAM.

function MakeIPAdress(b1, b2, b3, b4 : Byte): Longint;
  //#define MAKEIPADDRESS(b1,b2,b3,b4)  ((LPARAM)(((DWORD)(b1)<<24)+((DWORD)(b2)<<16)+((DWORD)(b3)<<8)+((DWORD)(b4))))

// Get individual number
function First_IPAdress(x : Longint): Byte;
  //#define FIRST_IPADDRESS(x)  ((x>>24) & 0xff)
function Second_IPAdress(x : Longint): Byte;
  //#define SECOND_IPADDRESS(x) ((x>>16) & 0xff)
function Third_IPAdress(x : Longint): Byte;
  //#define THIRD_IPADDRESS(x)  ((x>>8) & 0xff)
function Fourth_IPAdress(x : Longint): Byte;
  //#define FOURTH_IPADDRESS(x) (x & 0xff)


  //==================== PAGER CONTROL============================================
  //                    IE 4 EXTENSIONS


const

  //Pager Class Name
  WC_PAGESCROLLER = 'SysPager';

//---------------------------------------------------------------------------------------
// Pager Control Styles
//---------------------------------------------------------------------------------------
  PGS_VERT           = $00000000;
  PGS_HORZ           = $00000001;
  PGS_AUTOSCROLL     = $00000002;
  PGS_DRAGNDROP      = $00000004;

//---------------------------------------------------------------------------------------
// Pager Button State
//---------------------------------------------------------------------------------------
//The scroll can be in one of the following control State

  PGF_INVISIBLE =  0;      // Scroll button is not visible
  PGF_NORMAL    =  1;      // Scroll button is in normal state
  PGF_GRAYED    =  2;      // Scroll button is in grayed state
  PGF_DEPRESSED =  4;      // Scroll button is in depressed state
  PGF_HOT       =  8;      // Scroll button is in hot state


// The following identifiers specifies the button control
  PGB_TOPORLEFT      = 0;
  PGB_BOTTOMORRIGHT  = 1;

//---------------------------------------------------------------------------------------
// Pager Control  Messages
//---------------------------------------------------------------------------------------

  PGM_SETCHILD           = (PGM_FIRST + 1);  // lParam == hwnd
  PGM_RECALCSIZE         = (PGM_FIRST + 2);
  PGM_FORWARDMOUSE       = (PGM_FIRST + 3);
  PGM_SETBKCOLOR         = (PGM_FIRST + 4);
  PGM_GETBKCOLOR         = (PGM_FIRST + 5);
  PGM_SETBORDER          = (PGM_FIRST + 6);
  PGM_GETBORDER          = (PGM_FIRST + 7);
  PGM_SETPOS             = (PGM_FIRST + 8);
  PGM_GETPOS             = (PGM_FIRST + 9);
  PGM_SETBUTTONSIZE      = (PGM_FIRST + 10);
  PGM_GETBUTTONSIZE      = (PGM_FIRST + 11);
  PGM_GETBUTTONSTATE     = (PGM_FIRST + 12);
  PGM_GETDROPTARGET      = CCM_GETDROPTARGET;


  procedure Pager_SetChild(hwnd: HWND; hwndChild: HWND);
     // (void)SNDMSG((hwnd), PGM_SETCHILD, 0, (LPARAM)(hwndChild))

  procedure Pager_RecalcSize(hwnd: HWND);
     // (void)SNDMSG((hwnd), PGM_RECALCSIZE, 0, 0)

  procedure Pager_ForwardMouse(hwnd: HWND; bForward: Bool);
     // (void)SNDMSG((hwnd), PGM_FORWARDMOUSE, (WPARAM)(bForward), 0)

  function Pager_SetBkColor(hwnd: HWND; clr: TColorRef): TColorRef;
     // (COLORREF)SNDMSG((hwnd), PGM_SETBKCOLOR, 0, (LPARAM)clr)

  function Pager_GetBkColor(hwnd: HWND): TColorRef;
     // (COLORREF)SNDMSG((hwnd), PGM_GETBKCOLOR, 0, 0)

  function Pager_SetBorder(hwnd: HWND; iBorder: Integer): Integer;
     // (int)SNDMSG((hwnd), PGM_SETBORDER, 0, (LPARAM)iBorder)

  function Pager_GetBorder(hwnd: HWND): Integer;
     // (int)SNDMSG((hwnd), PGM_GETBORDER, 0, 0)

  function Pager_SetPos(hwnd: HWND; iPos: Integer): Integer;
     // (int)SNDMSG((hwnd), PGM_SETPOS, 0, (LPARAM)iPos)

  function Pager_GetPos(hwnd: HWND): Integer;
     // (int)SNDMSG((hwnd), PGM_GETPOS, 0, 0)

  function Pager_SetButtonSize(hwnd: HWND; iSize: Integer): Integer;
     // (int)SNDMSG((hwnd), PGM_SETBUTTONSIZE, 0, (LPARAM)iSize)

  function Pager_GetButtonSize(hwnd: HWND): Integer;
     // (int)SNDMSG((hwnd), PGM_GETBUTTONSIZE, 0,0)

  function Pager_GetButtonState(hwnd: HWND; iButton: Integer): Longint;
     // (DWORD)SNDMSG((hwnd), PGM_GETBUTTONSTATE, 0, (LPARAM)iButton)

(*  procedure Pager_GetDropTarget(hwnd: HWND; ppdt: ) \
        (void)SNDMSG((hwnd), PGM_GETDROPTARGET, 0, (LPARAM)ppdt)*)

//---------------------------------------------------------------------------------------
//Pager Control Notification Messages
//---------------------------------------------------------------------------------------

const

  // PGN_SCROLL Notification Message

  PGN_SCROLL         = (PGN_FIRST-1);

  PGF_SCROLLUP      =  1;
  PGF_SCROLLDOWN    =  2;
  PGF_SCROLLLEFT    =  4;
  PGF_SCROLLRIGHT   =  8;

  //Keys down
  PGK_SHIFT         =  1;
  PGK_CONTROL       =  2;
  PGK_MENU          =  4;

// This structure is sent along with PGN_SCROLL notifications

type

  PNMPgScroll = ^TNMPgScroll;
  TNMPgScroll = packed record
    hdr: TNMHDR;
    fwKeys: Word;           // Specifies which keys are down when this notification is send
    rcParent: TRect;        // Contains Parent Window Rect
    iDir: Integer;          // Scrolling Direction
    iXpos: Integer;         // Horizontal scroll position
    iYpos: Integer;         // Vertical scroll position
    iScroll: Integer;       // [in/out] Amount to scroll
  end;

const

// PGN_CALCSIZE Notification Message

  PGN_CALCSIZE      =  (PGN_FIRST-2);

  PGF_CALCWIDTH     =  1;
  PGF_CALCHEIGHT    =  2;

type

  PNMPgCalcSize = ^TNMPgCalcSize;
  TNMPgCalcSize = packed record
    hdr: TNMHDR;
    dwFlag: Longint;
    iWidth: Integer;
    iHeight: Integer;
  end;

  //==================== NATIVE FONT CONTROL============================================
  //                      IE 4 EXTENSIONS

const

  WC_NATIVEFONTCTLA = 'NativeFontCtl';

  // style definition
  NFS_EDIT              =  $0001;
  NFS_STATIC            =  $0002;
  NFS_LISTCOMBO         =  $0004;
  NFS_BUTTON            =  $0008;
  NFS_ALL               =  $0010;

  //==================== FLAT SCROLLBAR API============================================
  //                      IE 4 EXTENSIONS

const

  WSB_PROP_CYVSCROLL =  $00000001;
  WSB_PROP_CXHSCROLL =  $00000002;
  WSB_PROP_CYHSCROLL =  $00000004;
  WSB_PROP_CXVSCROLL =  $00000008;
  WSB_PROP_CXHTHUMB  =  $00000010;
  WSB_PROP_CYVTHUMB  =  $00000020;
  WSB_PROP_VBKGCOLOR =  $00000040;
  WSB_PROP_HBKGCOLOR =  $00000080;
  WSB_PROP_VSTYLE    =  $00000100;
  WSB_PROP_HSTYLE    =  $00000200;
  WSB_PROP_WINSTYLE  =  $00000400;
  WSB_PROP_PALETTE   =  $00000800;
  WSB_PROP_MASK      =  $00000FFF;

  FSB_FLAT_MODE         =  2;
  FSB_ENCARTA_MODE      =  1;
  FSB_REGULAR_MODE      =  0;


  function FlatSB_EnableScrollBar(Hwnd: HWND; Int: Integer; Card: Cardinal): Bool;
  function FlatSB_ShowScrollBar(Hwnd: HWND; Int: Integer; B: Bool): Bool;
  function FlatSB_GetScrollRange(Hwnd: HWND; Code: Integer; Var Min, Max: Integer): Bool;
  function FlatSB_GetScrollInfo(Hwnd: HWND; Code: Integer; Var ScrollInfo: TScrollInfo): Bool;
  function FlatSB_GetScrollPos(Hwnd: HWND; Code: Integer): Integer;
  Function FlatSB_GetScrollProp(Hwnd: HWND; propIndex: Integer; Var Prop: Integer): Bool;
  function FlatSB_SetScrollPos(Hwnd: HWND; Code: Integer; Pos: Integer; fRedraw: Bool): Integer;
  function FlatSB_SetScrollInfo(Hwnd: HWND; Code: Integer; Var ScrollInfo: TScrollInfo; fRedraw: Bool): Integer;
  function FlatSB_SetScrollRange(Hwnd: HWND; Code: Integer; Min: Integer; Max: Integer; fRedraw: Bool): Integer;
  function FlatSB_SetScrollProp(Hwnd: HWND; Index: Cardinal; newValue: Integer; B: Bool): Bool;
  function InitializeFlatSB(Hwnd: HWND): Bool;
  function UninitializeFlatSB(Hwnd: HWND): HRESULT;

implementation

const
  cctrl = 'comctl32.dll';

{$ifndef VER100}
function InitCommonControlsEx(Var CC : TInitCommonControlsEx): boolean; external cctrl name 'InitCommonControlsEx';
//function _TrackMouseEvent(Var lpEventTrack: PTrackMouseEvent): Boolean; external cctrl name '_TrackMouseEvent';

procedure InitCommonControl(CClass: Longint);
Var CC : TInitCommonControlsEx;
begin
  CC.dwSize:= sizeOf(TInitCommonControlsEx);
  CC.dwICC:= CClass;
  InitCommonControlsEx(CC);
end;
{$endif}

function FlatSB_EnableScrollBar(Hwnd: HWND; Int: Integer; Card: Cardinal): Bool; external cctrl name 'FlatSB_EnableScrollBar';
function FlatSB_ShowScrollBar(Hwnd: HWND; Int: Integer; B: Bool): Bool; external cctrl name 'FlatSB_ShowScrollBar';
function FlatSB_GetScrollRange(Hwnd: HWND; Code: Integer; Var Min, Max: Integer): Bool; external cctrl name 'FlatSB_GetScrollRange';
function FlatSB_GetScrollInfo(Hwnd: HWND; Code: Integer; Var ScrollInfo: TScrollInfo): Bool; external cctrl name 'FlatSB_GetScrollInfo';
function FlatSB_GetScrollPos(Hwnd: HWND; Code: Integer): Integer; external cctrl name 'FlatSB_GetScrollPos';
Function FlatSB_GetScrollProp(Hwnd: HWND; propIndex: Integer; Var Prop: Integer): Bool; external cctrl name 'FlatSB_GetScrollProp';
function FlatSB_SetScrollPos(Hwnd: HWND; Code: Integer; Pos: Integer; fRedraw: Bool): Integer; external cctrl name 'FlatSB_SetScrollPos';
function FlatSB_SetScrollInfo(Hwnd: HWND; Code: Integer; Var ScrollInfo: TScrollInfo; fRedraw: Bool): Integer; external cctrl name 'FlatSB_SetScrollInfo';
function FlatSB_SetScrollRange(Hwnd: HWND; Code: Integer; Min: Integer; Max: Integer; fRedraw: Bool): Integer; external cctrl name 'FlatSB_SetScrollRange';
function FlatSB_SetScrollProp(Hwnd: HWND; Index: Cardinal; newValue: Integer; B: Bool): Bool; external cctrl name 'FlatSB_SetScrollProp';
function InitializeFlatSB(Hwnd: HWND): Bool; external cctrl name 'InitializeFlatSB';
function UninitializeFlatSB(Hwnd: HWND): HRESULT; external cctrl name 'UninitializeFlatSB';

function TabCtrl_HighlightItem(hwnd: HWND; i : Cardinal; fHighlight : Bool): Bool;
begin
  Result:= Bool(SendMessage(hwnd, TCM_HIGHLIGHTITEM, i, Longint(fHighlight)));
end;

function TabCtrl_SetExtendedStyle(hwnd: HWND; dw: Longint): Longint;
begin
  Result:= SendMessage(hwnd, TCM_SETEXTENDEDSTYLE, 0, dw);
end;

function TabCtrl_GetExtendedStyle(hwnd: HWND): Longint;
begin
  Result:= SendMessage(hwnd, TCM_GETEXTENDEDSTYLE, 0, 0);
end;

function TabCtrl_GetItemRect(hwnd: HWND; i: Integer; Var prc: TRect): Boolean;
begin
  Result:= Boolean(SendMessage(hwnd, TCM_GETITEMRECT, i, Longint(@prc)));
end;

function Header_InsertItemEx(Header: HWnd; Index: Integer;
  const Item: THDItemEx): Integer;
begin
  Result := SendMessage(Header, HDM_INSERTITEM, Index, Longint(@Item));
end;

function Header_GetItemEx(Header: HWnd; Index: Integer; var Item: THDItemEx): Bool;
begin
  Result := Bool( SendMessage(Header, HDM_GETITEM, Index, Longint(@Item)) );
end;

function Header_SetItemEx(Header: HWnd; Index: Integer; const Item: THDItemEx): Bool;
begin
  Result := Bool( SendMessage(Header, HDM_SETITEM, Index, Longint(@Item)) );
end;

function Header_GetItemRect(Header: HWnd; Index: Integer; var Rect: TRect): Bool;
begin
  Result:= Bool(SendMessage(Header, HDM_GETITEMRECT, Index, Longint(@Rect)));
end;

function Header_GetImageList(Header: HWnd): HImageList;
begin
  Result:= HImageList(SendMessage(Header, HDM_GETIMAGELIST, 0, 0));
end;

function Header_SetImageList(Header: HWnd; iImageList: Integer): HImageList;
begin
  Result:= HImageList(SendMessage(Header, HDM_SETIMAGELIST, 0, iImageList));
end;

function Header_OrderToIndex(Header: HWnd; Order: Integer): Integer;
begin
  Result:= Integer(SendMessage(Header, HDM_ORDERTOINDEX, Order, 0));
end;

function Header_CreateDragImage(Header: HWnd; Index: Integer): HImageList;
begin
  Result:= HImageList(SendMessage(Header, HDM_CREATEDRAGIMAGE, Index, 0));
end;

function Header_GetOrderArray(Header: HWnd; iCount: Integer; pi: PInteger): BOOL;
begin
  Result:= Bool(SendMessage(Header, HDM_GETORDERARRAY, iCount, Longint(pi)));
end;

function Header_SetOrderArray(Header: HWnd; iCount: Integer; pi: PInteger): BOOL;
begin
  Result:= Bool(SendMessage(Header, HDM_SETORDERARRAY, iCount, Longint(pi)));
end;
function Header_SetHotDivider(Header: HWnd; fPos: BOOL; dw: Longint): Integer;
begin
  Result:= Integer(SendMessage(Header, HDM_SETHOTDIVIDER, Integer(fPos), dw));
end;

function ListView_GetSelectionMark(hwnd: HWND): Integer;
begin
  Result:= Integer(SendMessage(hwnd, LVM_GETSELECTIONMARK, 0, 0));
end;

function ListView_SetSelectionMark(hwnd: HWND; i: Cardinal): Integer;
begin
  Result:= Integer(SendMessage(hwnd, LVM_GETSELECTIONMARK, 0, Longint(i)));
end;

function ListView_GetWorkArea(hwnd: HWND; Var prc: TRect): Bool;
begin
  Result:= Bool(SendMessage(hwnd, LVM_GETWORKAREA, 0, Longint(@prc)));
end;

function ListView_SetHoverTime(hwndLV: HWND; dwHoverTimeMs: Longint): Longint;
begin
  Result:= SendMessage(hwndLV, LVM_SETHOVERTIME, 0, dwHoverTimeMs);
end;

function ListView_GetHoverTime(hwndLV: HWND): Longint;
begin
  Result:= SendMessage(hwndLV, LVM_GETHOVERTIME, 0, 0);
end;

function ListView_SetBkImage(hwnd: HWND; Var BkImage: TLVBkImage): Bool;
begin
  Result:= Bool(SendMessage(hwnd, LVM_SETBKIMAGE, 0, Longint(@BkImage)));
end;

function ListView_GetBkImage(hwnd: HWND; Var BkImage: TLVBkImage): Bool;
begin
  Result:= Bool(SendMessage(hwnd, LVM_GETBKIMAGE, 0, Longint(@BkImage)));
end;

procedure DateTime_SetMonthCalFont(hdp: HWND; hfont: HFONT; fRedraw: Bool);
begin
  SendMessage(hdp, DTM_SETMCFONT, wParam(hfont), Longint(fRedraw));
end;

procedure DateTime_GetMonthCalFont(hdp: HWND);
begin
  SendMessage(hdp, DTM_GETMCFONT, 0, 0);
end;

function MakeIPRange(Low, High : Byte): Longint;
begin
  Result:= Longint((Longint(high) SHL 8) + Longint(low));
end;

function MakeIPAdress(b1, b2, b3, b4 : Byte): Longint;
begin
  Result:= Longint((Longint(b1) Shl 24)+(Longint(b2) Shl 16)+(Longint(b3) Shl 8)+(Longint(b4)));
end;

function First_IPAdress(x : Longint): Byte;
begin
  Result:= Byte((x Shr 24) and $0FF);
end;

function Second_IPAdress(x : Longint): Byte;
begin
  Result:= Byte((x Shr 16) and $0FF);
end;

function Third_IPAdress(x : Longint): Byte;
begin
  Result:= Byte((x Shr 8) and $0FF);
end;

function Fourth_IPAdress(x : Longint): Byte;
begin
  Result:= Byte(x and $0FF);
end;

procedure Pager_SetChild(hwnd: HWND; hwndChild: HWND);
begin
  SendMessage(hwnd, PGM_SETCHILD, 0, Longint(hwndChild));
end;

procedure Pager_RecalcSize(hwnd: HWND);
begin
  SendMessage(hwnd, PGM_RECALCSIZE, 0, 0);
end;

procedure Pager_ForwardMouse(hwnd: HWND; bForward: Bool);
begin
  SendMessage(hwnd, PGM_FORWARDMOUSE, Integer(bForward), 0);
end;

function Pager_SetBkColor(hwnd: HWND; clr: TColorRef): TColorRef;
begin
  Result:= TColorRef(SendMessage(hwnd, PGM_SETBKCOLOR, 0, Longint(clr)));
end;

function Pager_GetBkColor(hwnd: HWND): TColorRef;
begin
  Result:=  TColorRef(SendMessage(hwnd, PGM_GETBKCOLOR, 0, 0));
end;

function Pager_SetBorder(hwnd: HWND; iBorder: Integer): Integer;
begin
  Result:= Integer(SendMessage(hwnd, PGM_SETBORDER, 0, Longint(iBorder)));
end;

function Pager_GetBorder(hwnd: HWND): Integer;
begin
  Result:= Integer(SendMessage(hwnd, PGM_GETBORDER, 0, 0));
end;

function Pager_SetPos(hwnd: HWND; iPos: Integer): Integer;
begin
  Result:= Integer(SendMessage(hwnd, PGM_SETPOS, 0, Longint(iPos)));
end;

function Pager_GetPos(hwnd: HWND): Integer;
begin
  Result:= Integer(SendMessage(hwnd, PGM_GETPOS, 0, 0));
end;

function Pager_SetButtonSize(hwnd: HWND; iSize: Integer): Integer;
begin
  Result:= Integer(SendMessage(hwnd, PGM_SETBUTTONSIZE, 0, Longint(iSize)));
end;

function Pager_GetButtonSize(hwnd: HWND): Integer;
begin
  Result:= Integer(SendMessage(hwnd, PGM_GETBUTTONSIZE, 0,0));
end;

function Pager_GetButtonState(hwnd: HWND; iButton: Integer): Longint;
begin
  Result:= Longint(SendMessage(hwnd, PGM_GETBUTTONSTATE, 0, Longint(iButton)));
end;

end.
