{$SMARTLINK ON}

{*******************************************************}
{                                                       }
{       Borland Delphi Runtime Library                  }
{       Windows Messages and Types                      }
{                                                       }
{       Copyright (C) 1991,99 Inprise Corporation       }
{                                                       }
{*******************************************************}

unit BareMsg;

{$A-}
{$WEAKPACKAGEUNIT}

interface

uses Windows;

{ Window Messages }

const
  {$EXTERNALSYM WM_NULL}
  WM_NULL             = $0000;
  {$EXTERNALSYM WM_CREATE}
  WM_CREATE           = $0001;
  {$EXTERNALSYM WM_DESTROY}
  WM_DESTROY          = $0002;
  {$EXTERNALSYM WM_MOVE}
  WM_MOVE             = $0003;
  {$EXTERNALSYM WM_SIZE}
  WM_SIZE             = $0005;
  {$EXTERNALSYM WM_ACTIVATE}
  WM_ACTIVATE         = $0006;
  {$EXTERNALSYM WM_SETFOCUS}
  WM_SETFOCUS         = $0007;
  {$EXTERNALSYM WM_KILLFOCUS}
  WM_KILLFOCUS        = $0008;
  {$EXTERNALSYM WM_ENABLE}
  WM_ENABLE           = $000A;
  {$EXTERNALSYM WM_SETREDRAW}
  WM_SETREDRAW        = $000B;
  {$EXTERNALSYM WM_SETTEXT}
  WM_SETTEXT          = $000C;
  {$EXTERNALSYM WM_GETTEXT}
  WM_GETTEXT          = $000D;
  {$EXTERNALSYM WM_GETTEXTLENGTH}
  WM_GETTEXTLENGTH    = $000E;
  {$EXTERNALSYM WM_PAINT}
  WM_PAINT            = $000F;
  {$EXTERNALSYM WM_CLOSE}
  WM_CLOSE            = $0010;
  {$EXTERNALSYM WM_QUERYENDSESSION}
  WM_QUERYENDSESSION  = $0011;
  {$EXTERNALSYM WM_QUIT}
  WM_QUIT             = $0012;
  {$EXTERNALSYM WM_QUERYOPEN}
  WM_QUERYOPEN        = $0013;
  {$EXTERNALSYM WM_ERASEBKGND}
  WM_ERASEBKGND       = $0014;
  {$EXTERNALSYM WM_SYSCOLORCHANGE}
  WM_SYSCOLORCHANGE   = $0015;
  {$EXTERNALSYM WM_ENDSESSION}
  WM_ENDSESSION       = $0016;
  {$EXTERNALSYM WM_SYSTEMERROR}
  WM_SYSTEMERROR      = $0017;
  {$EXTERNALSYM WM_SHOWWINDOW}
  WM_SHOWWINDOW       = $0018;
  {$EXTERNALSYM WM_CTLCOLOR}
  WM_CTLCOLOR         = $0019;
  {$EXTERNALSYM WM_WININICHANGE}
  WM_WININICHANGE     = $001A;
  {$EXTERNALSYM WM_SETTINGCHANGE}
  WM_SETTINGCHANGE = WM_WININICHANGE;
  {$EXTERNALSYM WM_DEVMODECHANGE}
  WM_DEVMODECHANGE    = $001B;
  {$EXTERNALSYM WM_ACTIVATEAPP}
  WM_ACTIVATEAPP      = $001C;
  {$EXTERNALSYM WM_FONTCHANGE}
  WM_FONTCHANGE       = $001D;
  {$EXTERNALSYM WM_TIMECHANGE}
  WM_TIMECHANGE       = $001E;
  {$EXTERNALSYM WM_CANCELMODE}
  WM_CANCELMODE       = $001F;
  {$EXTERNALSYM WM_SETCURSOR}
  WM_SETCURSOR        = $0020;
  {$EXTERNALSYM WM_MOUSEACTIVATE}
  WM_MOUSEACTIVATE    = $0021;
  {$EXTERNALSYM WM_CHILDACTIVATE}
  WM_CHILDACTIVATE    = $0022;
  {$EXTERNALSYM WM_QUEUESYNC}
  WM_QUEUESYNC        = $0023;
  {$EXTERNALSYM WM_GETMINMAXINFO}
  WM_GETMINMAXINFO    = $0024;
  {$EXTERNALSYM WM_PAINTICON}
  WM_PAINTICON        = $0026;
  {$EXTERNALSYM WM_ICONERASEBKGND}
  WM_ICONERASEBKGND   = $0027;
  {$EXTERNALSYM WM_NEXTDLGCTL}
  WM_NEXTDLGCTL       = $0028;
  {$EXTERNALSYM WM_SPOOLERSTATUS}
  WM_SPOOLERSTATUS    = $002A;
  {$EXTERNALSYM WM_DRAWITEM}
  WM_DRAWITEM         = $002B;
  {$EXTERNALSYM WM_MEASUREITEM}
  WM_MEASUREITEM      = $002C;
  {$EXTERNALSYM WM_DELETEITEM}
  WM_DELETEITEM       = $002D;
  {$EXTERNALSYM WM_VKEYTOITEM}
  WM_VKEYTOITEM       = $002E;
  {$EXTERNALSYM WM_CHARTOITEM}
  WM_CHARTOITEM       = $002F;
  {$EXTERNALSYM WM_SETFONT}
  WM_SETFONT          = $0030;
  {$EXTERNALSYM WM_GETFONT}
  WM_GETFONT          = $0031;
  {$EXTERNALSYM WM_SETHOTKEY}
  WM_SETHOTKEY        = $0032;
  {$EXTERNALSYM WM_GETHOTKEY}
  WM_GETHOTKEY        = $0033;
  {$EXTERNALSYM WM_QUERYDRAGICON}
  WM_QUERYDRAGICON    = $0037;
  {$EXTERNALSYM WM_COMPAREITEM}
  WM_COMPAREITEM      = $0039;
  {$EXTERNALSYM WM_GETOBJECT}
  WM_GETOBJECT        = $003D;
  {$EXTERNALSYM WM_COMPACTING}
  WM_COMPACTING       = $0041;

  {$EXTERNALSYM WM_COMMNOTIFY}
  WM_COMMNOTIFY       = $0044;    { obsolete in Win32}

  {$EXTERNALSYM WM_WINDOWPOSCHANGING}
  WM_WINDOWPOSCHANGING = $0046;
  {$EXTERNALSYM WM_WINDOWPOSCHANGED}
  WM_WINDOWPOSCHANGED = $0047;
  {$EXTERNALSYM WM_POWER}
  WM_POWER            = $0048;

  {$EXTERNALSYM WM_COPYDATA}
  WM_COPYDATA         = $004A;
  {$EXTERNALSYM WM_CANCELJOURNAL}
  WM_CANCELJOURNAL    = $004B;
  {$EXTERNALSYM WM_NOTIFY}
  WM_NOTIFY           = $004E;
  {$EXTERNALSYM WM_INPUTLANGCHANGEREQUEST}
  WM_INPUTLANGCHANGEREQUEST = $0050;
  {$EXTERNALSYM WM_INPUTLANGCHANGE}
  WM_INPUTLANGCHANGE  = $0051;
  {$EXTERNALSYM WM_TCARD}
  WM_TCARD            = $0052;
  {$EXTERNALSYM WM_HELP}
  WM_HELP             = $0053;
  {$EXTERNALSYM WM_USERCHANGED}
  WM_USERCHANGED      = $0054;
  {$EXTERNALSYM WM_NOTIFYFORMAT}
  WM_NOTIFYFORMAT     = $0055;

  {$EXTERNALSYM WM_CONTEXTMENU}
  WM_CONTEXTMENU      = $007B;
  {$EXTERNALSYM WM_STYLECHANGING}
  WM_STYLECHANGING    = $007C;
  {$EXTERNALSYM WM_STYLECHANGED}
  WM_STYLECHANGED     = $007D;
  {$EXTERNALSYM WM_DISPLAYCHANGE}
  WM_DISPLAYCHANGE    = $007E;
  {$EXTERNALSYM WM_GETICON}
  WM_GETICON          = $007F;
  {$EXTERNALSYM WM_SETICON}
  WM_SETICON          = $0080;

  {$EXTERNALSYM WM_NCCREATE}
  WM_NCCREATE         = $0081;
  {$EXTERNALSYM WM_NCDESTROY}
  WM_NCDESTROY        = $0082;
  {$EXTERNALSYM WM_NCCALCSIZE}
  WM_NCCALCSIZE       = $0083;
  {$EXTERNALSYM WM_NCHITTEST}
  WM_NCHITTEST        = $0084;
  {$EXTERNALSYM WM_NCPAINT}
  WM_NCPAINT          = $0085;
  {$EXTERNALSYM WM_NCACTIVATE}
  WM_NCACTIVATE       = $0086;
  {$EXTERNALSYM WM_GETDLGCODE}
  WM_GETDLGCODE       = $0087;
  {$EXTERNALSYM WM_NCMOUSEMOVE}
  WM_NCMOUSEMOVE      = $00A0;
  {$EXTERNALSYM WM_NCLBUTTONDOWN}
  WM_NCLBUTTONDOWN    = $00A1;
  {$EXTERNALSYM WM_NCLBUTTONUP}
  WM_NCLBUTTONUP      = $00A2;
  {$EXTERNALSYM WM_NCLBUTTONDBLCLK}
  WM_NCLBUTTONDBLCLK  = $00A3;
  {$EXTERNALSYM WM_NCRBUTTONDOWN}
  WM_NCRBUTTONDOWN    = $00A4;
  {$EXTERNALSYM WM_NCRBUTTONUP}
  WM_NCRBUTTONUP      = $00A5;
  {$EXTERNALSYM WM_NCRBUTTONDBLCLK}
  WM_NCRBUTTONDBLCLK  = $00A6;
  {$EXTERNALSYM WM_NCMBUTTONDOWN}
  WM_NCMBUTTONDOWN    = $00A7;
  {$EXTERNALSYM WM_NCMBUTTONUP}
  WM_NCMBUTTONUP      = $00A8;
  {$EXTERNALSYM WM_NCMBUTTONDBLCLK}
  WM_NCMBUTTONDBLCLK  = $00A9;

  {$EXTERNALSYM WM_KEYFIRST}
  WM_KEYFIRST         = $0100;
  {$EXTERNALSYM WM_KEYDOWN}
  WM_KEYDOWN          = $0100;
  {$EXTERNALSYM WM_KEYUP}
  WM_KEYUP            = $0101;
  {$EXTERNALSYM WM_CHAR}
  WM_CHAR             = $0102;
  {$EXTERNALSYM WM_DEADCHAR}
  WM_DEADCHAR         = $0103;
  {$EXTERNALSYM WM_SYSKEYDOWN}
  WM_SYSKEYDOWN       = $0104;
  {$EXTERNALSYM WM_SYSKEYUP}
  WM_SYSKEYUP         = $0105;
  {$EXTERNALSYM WM_SYSCHAR}
  WM_SYSCHAR          = $0106;
  {$EXTERNALSYM WM_SYSDEADCHAR}
  WM_SYSDEADCHAR      = $0107;
  {$EXTERNALSYM WM_KEYLAST}
  WM_KEYLAST          = $0108;

  {$EXTERNALSYM WM_INITDIALOG}
  WM_INITDIALOG       = $0110;
  {$EXTERNALSYM WM_COMMAND}
  WM_COMMAND          = $0111;
  {$EXTERNALSYM WM_SYSCOMMAND}
  WM_SYSCOMMAND       = $0112;
  {$EXTERNALSYM WM_TIMER}
  WM_TIMER            = $0113;
  {$EXTERNALSYM WM_HSCROLL}
  WM_HSCROLL          = $0114;
  {$EXTERNALSYM WM_VSCROLL}
  WM_VSCROLL          = $0115;
  {$EXTERNALSYM WM_INITMENU}
  WM_INITMENU         = $0116;
  {$EXTERNALSYM WM_INITMENUPOPUP}
  WM_INITMENUPOPUP    = $0117;
  {$EXTERNALSYM WM_MENUSELECT}
  WM_MENUSELECT       = $011F;
  {$EXTERNALSYM WM_MENUCHAR}
  WM_MENUCHAR         = $0120;
  {$EXTERNALSYM WM_ENTERIDLE}
  WM_ENTERIDLE        = $0121;

  {$EXTERNALSYM WM_MENURBUTTONUP}
  WM_MENURBUTTONUP    = $0122;
  {$EXTERNALSYM WM_MENUDRAG}
  WM_MENUDRAG         = $0123;
  {$EXTERNALSYM WM_MENUGETOBJECT}
  WM_MENUGETOBJECT    = $0124;
  {$EXTERNALSYM WM_UNINITMENUPOPUP}
  WM_UNINITMENUPOPUP  = $0125;
  {$EXTERNALSYM WM_MENUCOMMAND}
  WM_MENUCOMMAND      = $0126;

  {$EXTERNALSYM WM_CHANGEUISTATE}
  WM_CHANGEUISTATE    = $0127;
  {$EXTERNALSYM WM_UPDATEUISTATE}
  WM_UPDATEUISTATE    = $0128;
  {$EXTERNALSYM WM_QUERYUISTATE}
  WM_QUERYUISTATE     = $0129;

  {$EXTERNALSYM WM_CTLCOLORMSGBOX}
  WM_CTLCOLORMSGBOX   = $0132;
  {$EXTERNALSYM WM_CTLCOLOREDIT}
  WM_CTLCOLOREDIT     = $0133;
  {$EXTERNALSYM WM_CTLCOLORLISTBOX}
  WM_CTLCOLORLISTBOX  = $0134;
  {$EXTERNALSYM WM_CTLCOLORBTN}
  WM_CTLCOLORBTN      = $0135;
  {$EXTERNALSYM WM_CTLCOLORDLG}
  WM_CTLCOLORDLG      = $0136;
  {$EXTERNALSYM WM_CTLCOLORSCROLLBAR}
  WM_CTLCOLORSCROLLBAR= $0137;
  {$EXTERNALSYM WM_CTLCOLORSTATIC}
  WM_CTLCOLORSTATIC   = $0138;

  {$EXTERNALSYM WM_MOUSEFIRST}
  WM_MOUSEFIRST       = $0200;
  {$EXTERNALSYM WM_MOUSEMOVE}
  WM_MOUSEMOVE        = $0200;
  {$EXTERNALSYM WM_LBUTTONDOWN}
  WM_LBUTTONDOWN      = $0201;
  {$EXTERNALSYM WM_LBUTTONUP}
  WM_LBUTTONUP        = $0202;
  {$EXTERNALSYM WM_LBUTTONDBLCLK}
  WM_LBUTTONDBLCLK    = $0203;
  {$EXTERNALSYM WM_RBUTTONDOWN}
  WM_RBUTTONDOWN      = $0204;
  {$EXTERNALSYM WM_RBUTTONUP}
  WM_RBUTTONUP        = $0205;
  {$EXTERNALSYM WM_RBUTTONDBLCLK}
  WM_RBUTTONDBLCLK    = $0206;
  {$EXTERNALSYM WM_MBUTTONDOWN}
  WM_MBUTTONDOWN      = $0207;
  {$EXTERNALSYM WM_MBUTTONUP}
  WM_MBUTTONUP        = $0208;
  {$EXTERNALSYM WM_MBUTTONDBLCLK}
  WM_MBUTTONDBLCLK    = $0209;
  {$EXTERNALSYM WM_MOUSEWHEEL}
  WM_MOUSEWHEEL       = $020A;
  {$EXTERNALSYM WM_MOUSELAST}
  WM_MOUSELAST        = $020A;

  {$EXTERNALSYM WM_PARENTNOTIFY}
  WM_PARENTNOTIFY     = $0210;
  {$EXTERNALSYM WM_ENTERMENULOOP}
  WM_ENTERMENULOOP    = $0211;
  {$EXTERNALSYM WM_EXITMENULOOP}
  WM_EXITMENULOOP     = $0212;
  {$EXTERNALSYM WM_NEXTMENU}
  WM_NEXTMENU         = $0213;

  {$EXTERNALSYM WM_SIZING}
  WM_SIZING           = 532;
  {$EXTERNALSYM WM_CAPTURECHANGED}
  WM_CAPTURECHANGED   = 533;
  {$EXTERNALSYM WM_MOVING}
  WM_MOVING           = 534;
  {$EXTERNALSYM WM_POWERBROADCAST}
  WM_POWERBROADCAST   = 536;
  {$EXTERNALSYM WM_DEVICECHANGE}
  WM_DEVICECHANGE     = 537;

  {$EXTERNALSYM WM_IME_STARTCOMPOSITION}
  WM_IME_STARTCOMPOSITION        = $010D;
  {$EXTERNALSYM WM_IME_ENDCOMPOSITION}
  WM_IME_ENDCOMPOSITION          = $010E;
  {$EXTERNALSYM WM_IME_COMPOSITION}
  WM_IME_COMPOSITION             = $010F;
  {$EXTERNALSYM WM_IME_KEYLAST}
  WM_IME_KEYLAST                 = $010F;

  {$EXTERNALSYM WM_IME_SETCONTEXT}
  WM_IME_SETCONTEXT              = $0281;
  {$EXTERNALSYM WM_IME_NOTIFY}
  WM_IME_NOTIFY                  = $0282;
  {$EXTERNALSYM WM_IME_CONTROL}
  WM_IME_CONTROL                 = $0283;
  {$EXTERNALSYM WM_IME_COMPOSITIONFULL}
  WM_IME_COMPOSITIONFULL         = $0284;
  {$EXTERNALSYM WM_IME_SELECT}
  WM_IME_SELECT                  = $0285;
  {$EXTERNALSYM WM_IME_CHAR}
  WM_IME_CHAR                    = $0286;
  {$EXTERNALSYM WM_IME_REQUEST}
  WM_IME_REQUEST                 = $0288;

  {$EXTERNALSYM WM_IME_KEYDOWN}
  WM_IME_KEYDOWN                 = $0290;
  {$EXTERNALSYM WM_IME_KEYUP}
  WM_IME_KEYUP                   = $0291;

  {$EXTERNALSYM WM_MDICREATE}
  WM_MDICREATE        = $0220;
  {$EXTERNALSYM WM_MDIDESTROY}
  WM_MDIDESTROY       = $0221;
  {$EXTERNALSYM WM_MDIACTIVATE}
  WM_MDIACTIVATE      = $0222;
  {$EXTERNALSYM WM_MDIRESTORE}
  WM_MDIRESTORE       = $0223;
  {$EXTERNALSYM WM_MDINEXT}
  WM_MDINEXT          = $0224;
  {$EXTERNALSYM WM_MDIMAXIMIZE}
  WM_MDIMAXIMIZE      = $0225;
  {$EXTERNALSYM WM_MDITILE}
  WM_MDITILE          = $0226;
  {$EXTERNALSYM WM_MDICASCADE}
  WM_MDICASCADE       = $0227;
  {$EXTERNALSYM WM_MDIICONARRANGE}
  WM_MDIICONARRANGE   = $0228;
  {$EXTERNALSYM WM_MDIGETACTIVE}
  WM_MDIGETACTIVE     = $0229;
  {$EXTERNALSYM WM_MDISETMENU}
  WM_MDISETMENU       = $0230;

  {$EXTERNALSYM WM_ENTERSIZEMOVE}
  WM_ENTERSIZEMOVE    = $0231;
  {$EXTERNALSYM WM_EXITSIZEMOVE}
  WM_EXITSIZEMOVE     = $0232;
  {$EXTERNALSYM WM_DROPFILES}
  WM_DROPFILES        = $0233;
  {$EXTERNALSYM WM_MDIREFRESHMENU}
  WM_MDIREFRESHMENU   = $0234;

  {$EXTERNALSYM WM_MOUSEHOVER}
  WM_MOUSEHOVER       = $02A1;
  {$EXTERNALSYM WM_MOUSELEAVE}
  WM_MOUSELEAVE       = $02A3;

  {$EXTERNALSYM WM_CUT}
  WM_CUT              = $0300;
  {$EXTERNALSYM WM_COPY}
  WM_COPY             = $0301;
  {$EXTERNALSYM WM_PASTE}
  WM_PASTE            = $0302;
  {$EXTERNALSYM WM_CLEAR}
  WM_CLEAR            = $0303;
  {$EXTERNALSYM WM_UNDO}
  WM_UNDO             = $0304;
  {$EXTERNALSYM WM_RENDERFORMAT}
  WM_RENDERFORMAT     = $0305;
  {$EXTERNALSYM WM_RENDERALLFORMATS}
  WM_RENDERALLFORMATS = $0306;
  {$EXTERNALSYM WM_DESTROYCLIPBOARD}
  WM_DESTROYCLIPBOARD = $0307;
  {$EXTERNALSYM WM_DRAWCLIPBOARD}
  WM_DRAWCLIPBOARD    = $0308;
  {$EXTERNALSYM WM_PAINTCLIPBOARD}
  WM_PAINTCLIPBOARD   = $0309;
  {$EXTERNALSYM WM_VSCROLLCLIPBOARD}
  WM_VSCROLLCLIPBOARD = $030A;
  {$EXTERNALSYM WM_SIZECLIPBOARD}
  WM_SIZECLIPBOARD    = $030B;
  {$EXTERNALSYM WM_ASKCBFORMATNAME}
  WM_ASKCBFORMATNAME  = $030C;
  {$EXTERNALSYM WM_CHANGECBCHAIN}
  WM_CHANGECBCHAIN    = $030D;
  {$EXTERNALSYM WM_HSCROLLCLIPBOARD}
  WM_HSCROLLCLIPBOARD = $030E;
  {$EXTERNALSYM WM_QUERYNEWPALETTE}
  WM_QUERYNEWPALETTE  = $030F;
  {$EXTERNALSYM WM_PALETTEISCHANGING}
  WM_PALETTEISCHANGING= $0310;
  {$EXTERNALSYM WM_PALETTECHANGED}
  WM_PALETTECHANGED   = $0311;
  {$EXTERNALSYM WM_HOTKEY}
  WM_HOTKEY           = $0312;

  {$EXTERNALSYM WM_PRINT}
  WM_PRINT            = 791;
  {$EXTERNALSYM WM_PRINTCLIENT}
  WM_PRINTCLIENT      = 792;

  {$EXTERNALSYM WM_HANDHELDFIRST}
  WM_HANDHELDFIRST    = 856;
  {$EXTERNALSYM WM_HANDHELDLAST}
  WM_HANDHELDLAST     = 863;

  {$EXTERNALSYM WM_PENWINFIRST}
  WM_PENWINFIRST      = $0380;
  {$EXTERNALSYM WM_PENWINLAST}
  WM_PENWINLAST       = $038F;

  {$EXTERNALSYM WM_COALESCE_FIRST}
  WM_COALESCE_FIRST   = $0390;
  {$EXTERNALSYM WM_COALESCE_LAST}
  WM_COALESCE_LAST    = $039F;

  {$EXTERNALSYM WM_DDE_FIRST}
  WM_DDE_FIRST        = $03E0;
  {$EXTERNALSYM WM_DDE_INITIATE}
  WM_DDE_INITIATE     = WM_DDE_FIRST + 0;
  {$EXTERNALSYM WM_DDE_TERMINATE}
  WM_DDE_TERMINATE    = WM_DDE_FIRST + 1;
  {$EXTERNALSYM WM_DDE_ADVISE}
  WM_DDE_ADVISE       = WM_DDE_FIRST + 2;
  {$EXTERNALSYM WM_DDE_UNADVISE}
  WM_DDE_UNADVISE     = WM_DDE_FIRST + 3;
  {$EXTERNALSYM WM_DDE_ACK}
  WM_DDE_ACK          = WM_DDE_FIRST + 4;
  {$EXTERNALSYM WM_DDE_DATA}
  WM_DDE_DATA         = WM_DDE_FIRST + 5;
  {$EXTERNALSYM WM_DDE_REQUEST}
  WM_DDE_REQUEST      = WM_DDE_FIRST + 6;
  {$EXTERNALSYM WM_DDE_POKE}
  WM_DDE_POKE         = WM_DDE_FIRST + 7;
  {$EXTERNALSYM WM_DDE_EXECUTE}
  WM_DDE_EXECUTE      = WM_DDE_FIRST + 8;
  {$EXTERNALSYM WM_DDE_LAST}
  WM_DDE_LAST         = WM_DDE_FIRST + 8;

  {$EXTERNALSYM WM_APP}
  WM_APP = $8000;

{ NOTE: All Message Numbers below 0x0400 are RESERVED }

{ Private Window Messages Start Here }

  {$EXTERNALSYM WM_USER}
  WM_USER             = $0400;

{ Button Notification Codes }

const
  {$EXTERNALSYM BN_CLICKED}
  BN_CLICKED       = 0;
  {$EXTERNALSYM BN_PAINT}
  BN_PAINT         = 1;
  {$EXTERNALSYM BN_HILITE}
  BN_HILITE        = 2;
  {$EXTERNALSYM BN_UNHILITE}
  BN_UNHILITE      = 3;
  {$EXTERNALSYM BN_DISABLE}
  BN_DISABLE       = 4;
  {$EXTERNALSYM BN_DOUBLECLICKED}
  BN_DOUBLECLICKED = 5;
  {$EXTERNALSYM BN_PUSHED}
  BN_PUSHED = BN_HILITE;
  {$EXTERNALSYM BN_UNPUSHED}
  BN_UNPUSHED = BN_UNHILITE;
  {$EXTERNALSYM BN_DBLCLK}
  BN_DBLCLK = BN_DOUBLECLICKED;
  {$EXTERNALSYM BN_SETFOCUS}
  BN_SETFOCUS = 6;
  {$EXTERNALSYM BN_KILLFOCUS}
  BN_KILLFOCUS = 7;

{ Button Control Messages }
const
  {$EXTERNALSYM BM_GETCHECK}
  BM_GETCHECK = $00F0;
  {$EXTERNALSYM BM_SETCHECK}
  BM_SETCHECK = $00F1;
  {$EXTERNALSYM BM_GETSTATE}
  BM_GETSTATE = $00F2;
  {$EXTERNALSYM BM_SETSTATE}
  BM_SETSTATE = $00F3;
  {$EXTERNALSYM BM_SETSTYLE}
  BM_SETSTYLE = $00F4;
  {$EXTERNALSYM BM_CLICK}
  BM_CLICK    = $00F5;
  {$EXTERNALSYM BM_GETIMAGE}
  BM_GETIMAGE = $00F6;
  {$EXTERNALSYM BM_SETIMAGE}
  BM_SETIMAGE = $00F7;

{ Listbox Notification Codes }

const
  {$EXTERNALSYM LBN_ERRSPACE}
  LBN_ERRSPACE  = (-2);
  {$EXTERNALSYM LBN_SELCHANGE}
  LBN_SELCHANGE = 1;
  {$EXTERNALSYM LBN_DBLCLK}
  LBN_DBLCLK    = 2;
  {$EXTERNALSYM LBN_SELCANCEL}
  LBN_SELCANCEL = 3;
  {$EXTERNALSYM LBN_SETFOCUS}
  LBN_SETFOCUS  = 4;
  {$EXTERNALSYM LBN_KILLFOCUS}
  LBN_KILLFOCUS = 5;

{ Listbox messages }

const
  {$EXTERNALSYM LB_ADDSTRING}
  LB_ADDSTRING            = $0180;
  {$EXTERNALSYM LB_INSERTSTRING}
  LB_INSERTSTRING         = $0181;
  {$EXTERNALSYM LB_DELETESTRING}
  LB_DELETESTRING         = $0182;
  {$EXTERNALSYM LB_SELITEMRANGEEX}
  LB_SELITEMRANGEEX       = $0183;
  {$EXTERNALSYM LB_RESETCONTENT}
  LB_RESETCONTENT         = $0184;
  {$EXTERNALSYM LB_SETSEL}
  LB_SETSEL               = $0185;
  {$EXTERNALSYM LB_SETCURSEL}
  LB_SETCURSEL            = $0186;
  {$EXTERNALSYM LB_GETSEL}
  LB_GETSEL               = $0187;
  {$EXTERNALSYM LB_GETCURSEL}
  LB_GETCURSEL            = $0188;
  {$EXTERNALSYM LB_GETTEXT}
  LB_GETTEXT              = $0189;
  {$EXTERNALSYM LB_GETTEXTLEN}
  LB_GETTEXTLEN           = $018A;
  {$EXTERNALSYM LB_GETCOUNT}
  LB_GETCOUNT             = $018B;
  {$EXTERNALSYM LB_SELECTSTRING}
  LB_SELECTSTRING         = $018C;
  {$EXTERNALSYM LB_DIR}
  LB_DIR                  = $018D;
  {$EXTERNALSYM LB_GETTOPINDEX}
  LB_GETTOPINDEX          = $018E;
  {$EXTERNALSYM LB_FINDSTRING}
  LB_FINDSTRING           = $018F;
  {$EXTERNALSYM LB_GETSELCOUNT}
  LB_GETSELCOUNT          = $0190;
  {$EXTERNALSYM LB_GETSELITEMS}
  LB_GETSELITEMS          = $0191;
  {$EXTERNALSYM LB_SETTABSTOPS}
  LB_SETTABSTOPS          = $0192;
  {$EXTERNALSYM LB_GETHORIZONTALEXTENT}
  LB_GETHORIZONTALEXTENT  = $0193;
  {$EXTERNALSYM LB_SETHORIZONTALEXTENT}
  LB_SETHORIZONTALEXTENT  = $0194;
  {$EXTERNALSYM LB_SETCOLUMNWIDTH}
  LB_SETCOLUMNWIDTH       = $0195;
  {$EXTERNALSYM LB_ADDFILE}
  LB_ADDFILE              = $0196;
  {$EXTERNALSYM LB_SETTOPINDEX}
  LB_SETTOPINDEX          = $0197;
  {$EXTERNALSYM LB_GETITEMRECT}
  LB_GETITEMRECT          = $0198;
  {$EXTERNALSYM LB_GETITEMDATA}
  LB_GETITEMDATA          = $0199;
  {$EXTERNALSYM LB_SETITEMDATA}
  LB_SETITEMDATA          = $019A;
  {$EXTERNALSYM LB_SELITEMRANGE}
  LB_SELITEMRANGE         = $019B;
  {$EXTERNALSYM LB_SETANCHORINDEX}
  LB_SETANCHORINDEX       = $019C;
  {$EXTERNALSYM LB_GETANCHORINDEX}
  LB_GETANCHORINDEX       = $019D;
  {$EXTERNALSYM LB_SETCARETINDEX}
  LB_SETCARETINDEX        = $019E;
  {$EXTERNALSYM LB_GETCARETINDEX}
  LB_GETCARETINDEX        = $019F;
  {$EXTERNALSYM LB_SETITEMHEIGHT}
  LB_SETITEMHEIGHT        = $01A0;
  {$EXTERNALSYM LB_GETITEMHEIGHT}
  LB_GETITEMHEIGHT        = $01A1;
  {$EXTERNALSYM LB_FINDSTRINGEXACT}
  LB_FINDSTRINGEXACT      = $01A2;
  {$EXTERNALSYM LB_SETLOCALE}
  LB_SETLOCALE            = $01A5;
  {$EXTERNALSYM LB_GETLOCALE}
  LB_GETLOCALE            = $01A6;
  {$EXTERNALSYM LB_SETCOUNT}
  LB_SETCOUNT             = $01A7;
  {$EXTERNALSYM LB_INITSTORAGE}
  LB_INITSTORAGE          = $01A8;
  {$EXTERNALSYM LB_ITEMFROMPOINT}
  LB_ITEMFROMPOINT        = $01A9;
  {$EXTERNALSYM LB_MSGMAX}
  LB_MSGMAX               = 432;

{ Combo Box Notification Codes }

const
  {$EXTERNALSYM CBN_ERRSPACE}
  CBN_ERRSPACE   = (-1);
  {$EXTERNALSYM CBN_SELCHANGE}
  CBN_SELCHANGE  = 1;
  {$EXTERNALSYM CBN_DBLCLK}
  CBN_DBLCLK     = 2;
  {$EXTERNALSYM CBN_SETFOCUS}
  CBN_SETFOCUS   = 3;
  {$EXTERNALSYM CBN_KILLFOCUS}
  CBN_KILLFOCUS  = 4;
  {$EXTERNALSYM CBN_EDITCHANGE}
  CBN_EDITCHANGE = 5;
  {$EXTERNALSYM CBN_EDITUPDATE}
  CBN_EDITUPDATE = 6;
  {$EXTERNALSYM CBN_DROPDOWN}
  CBN_DROPDOWN   = 7;
  {$EXTERNALSYM CBN_CLOSEUP}
  CBN_CLOSEUP    = 8;
  {$EXTERNALSYM CBN_SELENDOK}
  CBN_SELENDOK   = 9;
  {$EXTERNALSYM CBN_SELENDCANCEL}
  CBN_SELENDCANCEL = 10;

{ Combo Box messages }

  {$EXTERNALSYM CB_GETEDITSEL}
  CB_GETEDITSEL            = $0140;
  {$EXTERNALSYM CB_LIMITTEXT}
  CB_LIMITTEXT             = $0141;
  {$EXTERNALSYM CB_SETEDITSEL}
  CB_SETEDITSEL            = $0142;
  {$EXTERNALSYM CB_ADDSTRING}
  CB_ADDSTRING             = $0143;
  {$EXTERNALSYM CB_DELETESTRING}
  CB_DELETESTRING          = $0144;
  {$EXTERNALSYM CB_DIR}
  CB_DIR                   = $0145;
  {$EXTERNALSYM CB_GETCOUNT}
  CB_GETCOUNT              = $0146;
  {$EXTERNALSYM CB_GETCURSEL}
  CB_GETCURSEL             = $0147;
  {$EXTERNALSYM CB_GETLBTEXT}
  CB_GETLBTEXT             = $0148;
  {$EXTERNALSYM CB_GETLBTEXTLEN}
  CB_GETLBTEXTLEN          = $0149;
  {$EXTERNALSYM CB_INSERTSTRING}
  CB_INSERTSTRING          = $014A;
  {$EXTERNALSYM CB_RESETCONTENT}
  CB_RESETCONTENT          = $014B;
  {$EXTERNALSYM CB_FINDSTRING}
  CB_FINDSTRING            = $014C;
  {$EXTERNALSYM CB_SELECTSTRING}
  CB_SELECTSTRING          = $014D;
  {$EXTERNALSYM CB_SETCURSEL}
  CB_SETCURSEL             = $014E;
  {$EXTERNALSYM CB_SHOWDROPDOWN}
  CB_SHOWDROPDOWN          = $014F;
  {$EXTERNALSYM CB_GETITEMDATA}
  CB_GETITEMDATA           = $0150;
  {$EXTERNALSYM CB_SETITEMDATA}
  CB_SETITEMDATA           = $0151;
  {$EXTERNALSYM CB_GETDROPPEDCONTROLRECT}
  CB_GETDROPPEDCONTROLRECT = $0152;
  {$EXTERNALSYM CB_SETITEMHEIGHT}
  CB_SETITEMHEIGHT         = $0153;
  {$EXTERNALSYM CB_GETITEMHEIGHT}
  CB_GETITEMHEIGHT         = $0154;
  {$EXTERNALSYM CB_SETEXTENDEDUI}
  CB_SETEXTENDEDUI         = $0155;
  {$EXTERNALSYM CB_GETEXTENDEDUI}
  CB_GETEXTENDEDUI         = $0156;
  {$EXTERNALSYM CB_GETDROPPEDSTATE}
  CB_GETDROPPEDSTATE       = $0157;
  {$EXTERNALSYM CB_FINDSTRINGEXACT}
  CB_FINDSTRINGEXACT       = $0158;
  {$EXTERNALSYM CB_SETLOCALE}
  CB_SETLOCALE             = 345;
  {$EXTERNALSYM CB_GETLOCALE}
  CB_GETLOCALE             = 346;
  {$EXTERNALSYM CB_GETTOPINDEX}
  CB_GETTOPINDEX           = 347;
  {$EXTERNALSYM CB_SETTOPINDEX}
  CB_SETTOPINDEX           = 348;
  {$EXTERNALSYM CB_GETHORIZONTALEXTENT}
  CB_GETHORIZONTALEXTENT   = 349;
  {$EXTERNALSYM CB_SETHORIZONTALEXTENT}
  CB_SETHORIZONTALEXTENT   = 350;
  {$EXTERNALSYM CB_GETDROPPEDWIDTH}
  CB_GETDROPPEDWIDTH       = 351;
  {$EXTERNALSYM CB_SETDROPPEDWIDTH}
  CB_SETDROPPEDWIDTH       = 352;
  {$EXTERNALSYM CB_INITSTORAGE}
  CB_INITSTORAGE           = 353;
  {$EXTERNALSYM CB_MSGMAX}
  CB_MSGMAX                = 354;

{ Edit Control Notification Codes }

const
  {$EXTERNALSYM EN_SETFOCUS}
  EN_SETFOCUS  = $0100;
  {$EXTERNALSYM EN_KILLFOCUS}
  EN_KILLFOCUS = $0200;
  {$EXTERNALSYM EN_CHANGE}
  EN_CHANGE    = $0300;
  {$EXTERNALSYM EN_UPDATE}
  EN_UPDATE    = $0400;
  {$EXTERNALSYM EN_ERRSPACE}
  EN_ERRSPACE  = $0500;
  {$EXTERNALSYM EN_MAXTEXT}
  EN_MAXTEXT   = $0501;
  {$EXTERNALSYM EN_HSCROLL}
  EN_HSCROLL   = $0601;
  {$EXTERNALSYM EN_VSCROLL}
  EN_VSCROLL   = $0602;

{ Edit Control Messages }

const
  {$EXTERNALSYM EM_GETSEL}
  EM_GETSEL              = $00B0;
  {$EXTERNALSYM EM_SETSEL}
  EM_SETSEL              = $00B1;
  {$EXTERNALSYM EM_GETRECT}
  EM_GETRECT             = $00B2;
  {$EXTERNALSYM EM_SETRECT}
  EM_SETRECT             = $00B3;
  {$EXTERNALSYM EM_SETRECTNP}
  EM_SETRECTNP           = $00B4;
  {$EXTERNALSYM EM_SCROLL}
  EM_SCROLL              = $00B5;
  {$EXTERNALSYM EM_LINESCROLL}
  EM_LINESCROLL          = $00B6;
  {$EXTERNALSYM EM_SCROLLCARET}
  EM_SCROLLCARET         = $00B7;
  {$EXTERNALSYM EM_GETMODIFY}
  EM_GETMODIFY           = $00B8;
  {$EXTERNALSYM EM_SETMODIFY}
  EM_SETMODIFY           = $00B9;
  {$EXTERNALSYM EM_GETLINECOUNT}
  EM_GETLINECOUNT        = $00BA;
  {$EXTERNALSYM EM_LINEINDEX}
  EM_LINEINDEX           = $00BB;
  {$EXTERNALSYM EM_SETHANDLE}
  EM_SETHANDLE           = $00BC;
  {$EXTERNALSYM EM_GETHANDLE}
  EM_GETHANDLE           = $00BD;
  {$EXTERNALSYM EM_GETTHUMB}
  EM_GETTHUMB            = $00BE;
  {$EXTERNALSYM EM_LINELENGTH}
  EM_LINELENGTH          = $00C1;
  {$EXTERNALSYM EM_REPLACESEL}
  EM_REPLACESEL          = $00C2;
  {$EXTERNALSYM EM_GETLINE}
  EM_GETLINE             = $00C4;
  {$EXTERNALSYM EM_LIMITTEXT}
  EM_LIMITTEXT           = $00C5;
  {$EXTERNALSYM EM_CANUNDO}
  EM_CANUNDO             = $00C6;
  {$EXTERNALSYM EM_UNDO}
  EM_UNDO                = $00C7;
  {$EXTERNALSYM EM_FMTLINES}
  EM_FMTLINES            = $00C8;
  {$EXTERNALSYM EM_LINEFROMCHAR}
  EM_LINEFROMCHAR        = $00C9;
  {$EXTERNALSYM EM_SETTABSTOPS}
  EM_SETTABSTOPS         = $00CB;
  {$EXTERNALSYM EM_SETPASSWORDCHAR}
  EM_SETPASSWORDCHAR     = $00CC;
  {$EXTERNALSYM EM_EMPTYUNDOBUFFER}
  EM_EMPTYUNDOBUFFER     = $00CD;
  {$EXTERNALSYM EM_GETFIRSTVISIBLELINE}
  EM_GETFIRSTVISIBLELINE = $00CE;
  {$EXTERNALSYM EM_SETREADONLY}
  EM_SETREADONLY         = $00CF;
  {$EXTERNALSYM EM_SETWORDBREAKPROC}
  EM_SETWORDBREAKPROC    = $00D0;
  {$EXTERNALSYM EM_GETWORDBREAKPROC}
  EM_GETWORDBREAKPROC    = $00D1;
  {$EXTERNALSYM EM_GETPASSWORDCHAR}
  EM_GETPASSWORDCHAR     = $00D2;
  {$EXTERNALSYM EM_SETMARGINS}
  EM_SETMARGINS          = 211;
  {$EXTERNALSYM EM_GETMARGINS}
  EM_GETMARGINS          = 212;
  {$EXTERNALSYM EM_SETLIMITTEXT}
  EM_SETLIMITTEXT        = EM_LIMITTEXT;    //win40 Name change
  {$EXTERNALSYM EM_GETLIMITTEXT}
  EM_GETLIMITTEXT        = 213;
  {$EXTERNALSYM EM_POSFROMCHAR}
  EM_POSFROMCHAR         = 214;
  {$EXTERNALSYM EM_CHARFROMPOS}
  EM_CHARFROMPOS         = 215;
  {$EXTERNALSYM EM_SETIMESTATUS}
  EM_SETIMESTATUS        = 216;
  {$EXTERNALSYM EM_GETIMESTATUS}
  EM_GETIMESTATUS        = 217;

const
  { Scroll bar messages }
  {$EXTERNALSYM SBM_SETPOS}
  SBM_SETPOS = 224;             { not in win3.1  }
  {$EXTERNALSYM SBM_GETPOS}
  SBM_GETPOS = 225;             { not in win3.1  }
  {$EXTERNALSYM SBM_SETRANGE}
  SBM_SETRANGE = 226;           { not in win3.1  }
  {$EXTERNALSYM SBM_SETRANGEREDRAW}
  SBM_SETRANGEREDRAW = 230;     { not in win3.1  }
  {$EXTERNALSYM SBM_GETRANGE}
  SBM_GETRANGE = 227;           { not in win3.1  }
  {$EXTERNALSYM SBM_ENABLE_ARROWS}
  SBM_ENABLE_ARROWS = 228;      { not in win3.1  }
  {$EXTERNALSYM SBM_SETSCROLLINFO}
  SBM_SETSCROLLINFO = 233;
  {$EXTERNALSYM SBM_GETSCROLLINFO}
  SBM_GETSCROLLINFO = 234;

{ Dialog messages }

  {$EXTERNALSYM DM_GETDEFID}
  DM_GETDEFID = (WM_USER+0);
  {$EXTERNALSYM DM_SETDEFID}
  DM_SETDEFID = (WM_USER+1);
  {$EXTERNALSYM DM_REPOSITION}
  DM_REPOSITION = (WM_USER+2);

  {$EXTERNALSYM PSM_PAGEINFO}
  PSM_PAGEINFO = (WM_USER+100);
  {$EXTERNALSYM PSM_SHEETINFO}
  PSM_SHEETINFO = (WM_USER+101);

type

{ Generic window message record }

  PMessage = ^TMessage;
  TMessage = packed record
    Msg: Cardinal;
    case Integer of
      0: (
        WParam: Longint;
        LParam: Longint;
        Result: Longint);
      1: (
        WParamLo: Word;
        WParamHi: Word;
        LParamLo: Word;
        LParamHi: Word;
        ResultLo: Word;
        ResultHi: Word);
  end;

{ Common message format records }

  TWMNoParams = packed record
    Msg: Cardinal;
    Unused: array[0..3] of Word;
    Result: Longint;
  end;

  TWMKey = packed record
    Msg: Cardinal;
    CharCode: Word;
    Unused: Word;
    KeyData: Longint;
    Result: Longint;
  end;

  TWMMouse = packed record
    Msg: Cardinal;
    Keys: Longint;
    case Integer of
      0: (
        XPos: Smallint;
        YPos: Smallint);
      1: (
        Pos: TSmallPoint;
        Result: Longint);
  end;

  TWMMouseWheel = packed record
    Msg: Cardinal;
    Keys: SmallInt;
    WheelDelta: SmallInt;
    case Integer of
      0: (
        XPos: Smallint;
        YPos: Smallint);
      1: (
        Pos: TSmallPoint;
        Result: Longint);
  end;

  TMSHMouseWheel = packed record
    Msg: Cardinal;
    WheelDelta: Integer;
    case Integer of
      0: (
        XPos: Smallint;
        YPos: Smallint);
      1: (
        Pos: TSmallPoint;
        Result: Longint);
  end;

  TWMWindowPosMsg = packed record
    Msg: Cardinal;
    Unused: Integer;
    WindowPos: PWindowPos;
    Result: Longint;
  end;

  TWMScroll = packed record
    Msg: Cardinal;
    ScrollCode: Smallint; { SB_xxxx }
    Pos: Smallint;
    ScrollBar: HWND;
    Result: Longint;
  end;

{ Message records }

  TWMActivate = packed record
    Msg: Cardinal;
    Active: Word; { WA_INACTIVE, WA_ACTIVE, WA_CLICKACTIVE }
    Minimized: WordBool;
    ActiveWindow: HWND;
    Result: Longint;
  end;

  TWMActivateApp = packed record
    Msg: Cardinal;
    Active: BOOL;
    ThreadId: Longint;
    Result: Longint;
  end;

  TWMAskCBFormatName = packed record
    Msg: Cardinal;
    NameLen: Word;
    Unused: Word;
    FormatName: PChar;
    Result: Longint;
  end;

  TWMCancelMode = TWMNoParams;

  TWMChangeCBChain = packed record
    Msg: Cardinal;
    Remove: HWND;
    Next: HWND;
    Result: Longint;
  end;

  TWMChar = TWMKey;

  TWMCharToItem = packed record
    Msg: Cardinal;
    Key: Word;
    CaretPos: Word;
    ListBox: HWND;
    Result: Longint;
  end;

  TWMChildActivate = TWMNoParams;

  TWMChooseFont_GetLogFont = packed record
    Msg: Cardinal;
    Unused: Longint;
    LogFont: PLogFont;
    Result: Longint;
  end;

  TWMClear = TWMNoParams;
  TWMClose = TWMNoParams;

  TWMCommand = packed record
    Msg: Cardinal;
    ItemID: Word;
    NotifyCode: Word;
    Ctl: HWND;
    Result: Longint;
  end;

  TWMCompacting = packed record
    Msg: Cardinal;
    CompactRatio: Longint;
    Unused: Longint;
    Result: Longint;
  end;

  TWMCompareItem = packed record
    Msg: Cardinal;
    Ctl: HWnd;
    CompareItemStruct: PCompareItemStruct;
    Result: Longint;
  end;

  TWMCopy = TWMNoParams;

  TWMCopyData = packed record
    Msg: Cardinal;
    From: HWND;
    CopyDataStruct: PCopyDataStruct;
    Result: Longint;
  end;

  { ?? WM_CLP_LAUNCH, WM_CPL_LAUNCHED }

  TWMCreate = packed record
    Msg: Cardinal;
    Unused: Integer;
    CreateStruct: PCreateStruct;
    Result: Longint;
  end;

  TWMCtlColor = packed record
    Msg: Cardinal;
    ChildDC: HDC;
    ChildWnd: HWND;
    Result: Longint;
  end;

  TWMCtlColorBtn = TWMCtlColor;
  TWMCtlColorDlg = TWMCtlColor;
  TWMCtlColorEdit = TWMCtlColor;
  TWMCtlColorListbox = TWMCtlColor;
  TWMCtlColorMsgbox = TWMCtlColor;
  TWMCtlColorScrollbar = TWMCtlColor;
  TWMCtlColorStatic = TWMCtlColor;

  TWMCut = TWMNoParams;

  TWMDDE_Ack = packed record
    Msg: Cardinal;
    PostingApp: HWND;
    case Word of
      WM_DDE_INITIATE: (
        App: Word;
        Topic: Word;
        Result: Longint);
      WM_DDE_EXECUTE {and all others}: (
        PackedVal: Longint);
  end;

  TWMDDE_Advise = packed record
    Msg: Cardinal;
    PostingApp: HWND;
    PackedVal: Longint;
    Result: Longint;
  end;

  TWMDDE_Data = packed record
    Msg: Cardinal;
    PostingApp: HWND;
    PackedVal: Longint;
    Result: Longint;
  end;

  TWMDDE_Execute = packed record
    Msg: Cardinal;
    PostingApp: HWND;
    Commands: THandle;
    Result: Longint;
  end;

  TWMDDE_Initiate = packed record
    Msg: Cardinal;
    PostingApp: HWND;
    App: Word;
    Topic: Word;
    Result: Longint;
  end;

  TWMDDE_Poke = packed record
    Msg: Cardinal;
    PostingApp: HWND;
    PackedVal: Longint;
    Result: Longint;
  end;

  TWMDDE_Request = packed record
    Msg: Cardinal;
    PostingApp: HWND;
    Format: Word;
    Item: Word;
    Result: Longint;
  end;

  TWMDDE_Terminate = packed record
    Msg: Cardinal;
    PostingApp: HWND;
    Unused: Longint;
    Result: Longint;
  end;

  TWMDDE_Unadvise = packed record
    Msg: Cardinal;
    PostingApp: HWND;
    Format: Word;
    Item: Word;
    Result: Longint;
  end;

  TWMDeadChar = TWMChar;

  TWMDeleteItem = packed record
    Msg: Cardinal;
    Ctl: HWND;
    DeleteItemStruct: PDeleteItemStruct;
    Result: Longint;
  end;

  TWMDestroy = TWMNoParams;
  TWMDestroyClipboard = TWMNoParams;

  TWMDevModeChange = packed record
    Msg: Cardinal;
    Unused: Integer;
    Device: PChar;
    Result: Longint;
  end;

  TWMDrawClipboard = TWMNoParams;

  TWMDrawItem = packed record
    Msg: Cardinal;
    Ctl: HWND;
    DrawItemStruct: PDrawItemStruct;
    Result: Longint;
  end;

  TWMDropFiles = packed record
    Msg: Cardinal;
    Drop: THANDLE;
    Unused: Longint;
    Result: Longint;
  end;

  TWMEnable = packed record
    Msg: Cardinal;
    Enabled: LongBool;
    Unused: Longint;
    Result: Longint;
  end;

  TWMEndSession = packed record
    Msg: Cardinal;
    EndSession: LongBool;
    Unused: Longint;
    Result: Longint;
  end;

  TWMEnterIdle = packed record
    Msg: Cardinal;
    Source: Longint; { MSGF_DIALOGBOX, MSGF_MENU }
    IdleWnd: HWND;
    Result: Longint;
  end;

  TWMEnterMenuLoop = packed record
    Msg: Cardinal;
    IsTrackPopupMenu: LongBool;
    Unused: Longint;
    Result: Longint;
  end;

  TWMExitMenuLoop = TWMEnterMenuLoop;

  TWMEraseBkgnd = packed record
    Msg: Cardinal;
    DC: HDC;
    Unused: Longint;
    Result: Longint;
  end;

  TWMFontChange = TWMNoParams;
  TWMGetDlgCode = TWMNoParams;
  TWMGetFont = TWMNoParams;

  TWMGetIcon = packed record
    Msg: Cardinal;
    BigIcon: Longbool;
    Unused: Longint;
    Result: Longint;
  end;

  TWMGetHotKey = TWMNoParams;

  TWMGetMinMaxInfo = packed record
    Msg: Cardinal;
    Unused: Integer;
    MinMaxInfo: PMinMaxInfo;
    Result: Longint;
  end;

  TWMGetText = packed record
    Msg: Cardinal;
    TextMax: Integer;
    Text: PChar;
    Result: Longint;
  end;

  TWMGetTextLength = TWMNoParams;

  TWMHotKey = packed record
    Msg: Cardinal;
    HotKey: Longint;
    Unused: Longint;
    Result: Longint;
  end;

  TWMHScroll = TWMScroll;

  TWMHScrollClipboard = packed record
    Msg: Cardinal;
    Viewer: HWND;
    ScrollCode: Word; {SB_BOTTOM, SB_ENDSCROLL, SB_LINEDOWN, SB_LINEUP,
                       SB_PAGEDOWN, SB_PAGEUP, SB_THUMBPOSITION,
                       SB_THUMBTRACK, SB_TOP }
    Pos: Word;
    Result: Longint;
  end;

  TWMIconEraseBkgnd = TWMEraseBkgnd;

  TWMInitDialog = packed record
    Msg: Cardinal;
    Focus: HWND;
    InitParam: Longint;
    Result: Longint;
  end;

  TWMInitMenu = packed record
    Msg: Cardinal;
    Menu: HMENU;
    Unused: Longint;
    Result: Longint;
  end;

  TWMInitMenuPopup = packed record
    Msg: Cardinal;
    MenuPopup: HMENU;
    Pos: Smallint;
    SystemMenu: WordBool;
    Result: Longint;
  end;

  TWMKeyDown = TWMKey;
  TWMKeyUp = TWMKey;

  TWMKillFocus = packed record
    Msg: Cardinal;
    FocusedWnd: HWND;
    Unused: Longint;
    Result: Longint;
  end;

  TWMLButtonDblClk = TWMMouse;
  TWMLButtonDown   = TWMMouse;
  TWMLButtonUp     = TWMMouse;
  TWMMButtonDblClk = TWMMouse;
  TWMMButtonDown   = TWMMouse;
  TWMMButtonUp     = TWMMouse;

  TWMMDIActivate = packed record
    Msg: Cardinal;
    case Integer of
      0: (
        ChildWnd: HWND);
      1: (
        DeactiveWnd: HWND;
        ActiveWnd: HWND;
        Result: Longint);
  end;

  TWMMDICascade = packed record
    Msg: Cardinal;
    Cascade: Longint; { 0, MDITILE_SKIPDISABLED }
    Unused: Longint;
    Result: Longint;
  end;

  TWMMDICreate = packed record
    Msg: Cardinal;
    Unused: Integer;
    MDICreateStruct: PMDICreateStruct;
    Result: Longint;
  end;

  TWMMDIDestroy = packed record
    Msg: Cardinal;
    Child: HWND;
    Unused: Longint;
    Result: Longint;
  end;

  TWMMDIGetActive = TWMNoParams;
  TWMMDIIconArrange = TWMNoParams;

  TWMMDIMaximize = packed record
    Msg: Cardinal;
    Maximize: HWND;
    Unused: Longint;
    Result: Longint;
  end;

  TWMMDINext = packed record
    Msg: Cardinal;
    Child: HWND;
    Next: Longint;
    Result: Longint;
  end;

  TWMMDIRefreshMenu = TWMNoParams;

  TWMMDIRestore = packed record
    Msg: Cardinal;
    IDChild: HWND;
    Unused: Longint;
    Result: Longint;
  end;

  TWMMDISetMenu = packed record
    Msg: Cardinal;
    MenuFrame: HMENU;
    MenuWindow: HMENU;
    Result: Longint;
  end;

  TWMMDITile = packed record
    Msg: Cardinal;
    Tile: Longint; { MDITILE_HORIZONTAL, MDITILE_SKIPDISABLE,
                     MDITILE_VERTICAL }
    Unused: Longint;
    Result: Longint;
  end;

  TWMMeasureItem = packed record
    Msg: Cardinal;
    IDCtl: HWnd;
    MeasureItemStruct: PMeasureItemStruct;
    Result: Longint;
  end;

  TWMMenuChar = packed record
    Msg: Cardinal;
    User: Char;
    Unused: Byte;
    MenuFlag: Word; { MF_POPUP, MF_SYSMENU }
    Menu: HMENU;
    Result: Longint;
  end;

  TWMMenuSelect = packed record
    Msg: Cardinal;
    IDItem: Word;
    MenuFlag: Word; { MF_BITMAP, MF_CHECKED, MF_DISABLED, MF_GRAYED,
                      MF_MOUSESELECT, MF_OWNERDRAW, MF_POPUP, MF_SEPARATOR,
                      MF_SYSMENU }
    Menu: HMENU;
    Result: Longint;
  end;

  TWMMouseActivate = packed record
    Msg: Cardinal;
    TopLevel: HWND;
    HitTestCode: Word;
    MouseMsg: Word;
    Result: Longint;
  end;

  TWMMouseMove = TWMMouse;

  TWMMove = packed record
    Msg: Cardinal;
    Unused: Integer;
    case Integer of
      0: (
        XPos: Smallint;
        YPos: Smallint);
      1: (
        Pos: TSmallPoint;
        Result: Longint);
  end;

  TWMNCActivate = packed record
    Msg: Cardinal;
    Active: BOOL;
    Unused: Longint;
    Result: Longint;
  end;

  TWMNCCalcSize = packed record
    Msg: Cardinal;
    CalcValidRects: BOOL;
    CalcSize_Params: PNCCalcSizeParams;
    Result: Longint;
  end;

  TWMNCCreate = packed record
    Msg: Cardinal;
    Unused: Integer;
    CreateStruct: PCreateStruct;
    Result: Longint;
  end;

  TWMNCDestroy = TWMNoParams;

  TWMNCHitTest = packed record
    Msg: Cardinal;
    Unused: Longint;
    case Integer of
      0: (
        XPos: Smallint;
        YPos: Smallint);
      1: (
        Pos: TSmallPoint;
        Result: Longint);
  end;

  TWMNCHitMessage = packed record
    Msg: Cardinal;
    HitTest: Longint;
    XCursor: Smallint;
    YCursor: Smallint;
    Result: Longint;
  end;

  TWMNCLButtonDblClk = TWMNCHitMessage;
  TWMNCLButtonDown   = TWMNCHitMessage;
  TWMNCLButtonUp     = TWMNCHitMessage;
  TWMNCMButtonDblClk = TWMNCHitMessage;
  TWMNCMButtonDown   = TWMNCHitMessage;
  TWMNCMButtonUp     = TWMNCHitMessage;
  TWMNCMouseMove     = TWMNCHitMessage;

  TWMNCPaint = TWMNoParams;

  TWMNCRButtonDblClk = TWMNCHitMessage;
  TWMNCRButtonDown   = TWMNCHitMessage;
  TWMNCRButtonUp     = TWMNCHitMessage;

  TWMNextDlgCtl = packed record
    Msg: Cardinal;
    CtlFocus: Longint;
    Handle: WordBool;
    Unused: Word;
    Result: Longint;
  end;

  TWMNotify = packed record
    Msg: Cardinal;
    IDCtrl: Longint;
    NMHdr: PNMHdr;
    Result: Longint;
  end;

  TWMNotifyFormat = packed record
    Msg: Cardinal;
    From: HWND;
    Command: Longint;
    Result: Longint;
  end;

  TWMPaint = packed record
    Msg: Cardinal;
    DC: HDC;
    Unused: Longint;
    Result: Longint;
  end;

  TWMPaintClipboard = packed record
    Msg: Cardinal;
    Viewer: HWND;
    PaintStruct: THandle;
    Result: Longint;
  end;

  TWMPaintIcon = TWMNoParams;

  TWMPaletteChanged = packed record
    Msg: Cardinal;
    PalChg: HWND;
    Unused: Longint;
    Result: Longint;
  end;

  TWMPaletteIsChanging = packed record
    Msg: Cardinal;
    Realize: HWND;
    Unused: Longint;
    Result: Longint;
  end;

  TWMParentNotify = packed record
    Msg: Cardinal;
    case Event: Word of
      WM_CREATE, WM_DESTROY: (
        ChildID: Word;
        ChildWnd: HWnd);
      WM_LBUTTONDOWN, WM_MBUTTONDOWN, WM_RBUTTONDOWN: (
        Value: Word;
        XPos: Smallint;
        YPos: Smallint);
      0: (
        Value1: Word;
        Value2: Longint;
        Result: Longint);
  end;

  TWMPaste = TWMNoParams;

  TWMPower = packed record
    Msg: Cardinal;
    PowerEvt: Longint; { PWR_SUSPENDREQUEST, PWR_SUSPENDRESUME,
                         PWR_CRITICALRESUME }
    Unused: Longint;
    Result: Longint;
  end;

  TWMQueryDragIcon = TWMNoParams;

  TWMQueryEndSession = packed record
    Msg: Cardinal;
    Source: Longint;
    Unused: Longint;
    Result: Longint;
  end;

  TWMQueryNewPalette = TWMNoParams;
  TWMQueryOpen = TWMNoParams;
  TWMQueueSync = TWMNoParams;

  TWMQuit = packed record
    Msg: Cardinal;
    ExitCode: Longint;
    Unused: Longint;
    Result: Longint;
  end;

  TWMRButtonDblClk = TWMMouse;
  TWMRButtonDown = TWMMouse;
  TWMRButtonUp = TWMMouse;

  TWMRenderAllFormats = TWMNoParams;

  TWMRenderFormat = packed record
    Msg: Cardinal;
    Format: Longint;
    Unused: Longint;
    Result: Longint;
  end;

  TWMSetCursor = packed record
    Msg: Cardinal;
    CursorWnd: HWND;
    HitTest: Word;
    MouseMsg: Word;
    Result: Longint;
  end;

  TWMSetFocus = packed record
    Msg: Cardinal;
    FocusedWnd: HWND;
    Unused: Longint;
    Result: Longint;
  end;

  TWMSetFont = packed record
    Msg: Cardinal;
    Font: HFONT;
    Redraw: WordBool;
    Unused: Word;
    Result: Longint;
  end;

  TWMSetHotKey = packed record
    Msg: Cardinal;
    Key: Longint;
    Unused: Longint;
    Result: Longint;
  end;

  TWMSetIcon = packed record
    Msg: Cardinal;
    BigIcon: Longbool;
    Icon: HICON;
    Result: Longint;
  end;

  TWMSetRedraw = packed record
    Msg: Cardinal;
    Redraw: Longint;
    Unused: Longint;
    Result: Longint;
  end;

  TWMSetText = packed record
    Msg: Cardinal;
    Unused: Longint;
    Text: PChar;
    Result: Longint;
  end;

  TWMShowWindow = packed record
    Msg: Cardinal;
    Show: BOOL;
    Status: Longint;
    Result: Longint;
  end;

  TWMSize = packed record
    Msg: Cardinal;
    SizeType: Longint; { SIZE_MAXIMIZED, SIZE_MINIMIZED, SIZE_RESTORED,
                         SIZE_MAXHIDE, SIZE_MAXSHOW }
    Width: Word;
    Height: Word;
    Result: Longint;
  end;

  TWMSizeClipboard = packed record
    Msg: Cardinal;
    Viewer: HWND;
    RC: THandle;
    Result: Longint;
  end;

  TWMSpoolerStatus = packed record
    Msg: Cardinal;
    JobStatus: Longint;
    JobsLeft: Word;
    Unused: Word;
    Result: Longint;
  end;

  TWMStyleChange = packed record
    Msg: Cardinal;
    StyleType: Longint;
    StyleStruct: PStyleStruct;
    Result: Longint;
  end;

  TWMStyleChanged = TWMStyleChange;
  TWMStyleChanging = TWMStyleChange;

  TWMSysChar = TWMKey;
  TWMSysColorChange = TWMNoParams;

  TWMSysCommand = packed record
    Msg: Cardinal;
    case CmdType: Longint of
      SC_HOTKEY: (
        ActivateWnd: HWND);
      SC_KEYMENU: (
        Key: Word);
      SC_CLOSE, SC_HSCROLL, SC_MAXIMIZE, SC_MINIMIZE, SC_MOUSEMENU, SC_MOVE,
      SC_NEXTWINDOW, SC_PREVWINDOW, SC_RESTORE, SC_SCREENSAVE, SC_SIZE,
      SC_TASKLIST, SC_VSCROLL: (
        XPos: Smallint;
        YPos: Smallint;
        Result: Longint);
  end;

  TWMSysDeadChar = packed record
    Msg: Cardinal;
    CharCode: Word;
    Unused: Word;
    KeyData: Longint;
    Result: Longint;
  end;

  TWMSysKeyDown = TWMKey;
  TWMSysKeyUp = TWMKey;

  TWMSystemError = packed record
    Msg: Cardinal;
    ErrSpec: Word;
    Unused: Longint;
    Result: Longint;
  end;

  TWMTimeChange = TWMNoParams;

  TWMTimer = packed record
    Msg: Cardinal;
    TimerID: Longint;
    TimerProc: TFarProc;
    Result: Longint;
  end;

  TWMUIState = packed record
    Msg: Cardinal;
    Action: Word;
    Flags: Word;
    Unused: Longint;
  end;

  TWMChangeUIState = TWMUIState;
  TWMUpdateUIState = TWMUIState;
  TWMQueryUIState = TWMNoParams;

  TWMUndo = TWMNoParams;

  TWMVKeyToItem = TWMCharToItem;

  TWMVScroll = TWMScroll;

  TWMVScrollClipboard = packed record
    Msg: Cardinal;
    Viewer: HWND;
    ScollCode: Word;
    ThumbPos: Word;
    Result: Longint;
  end;

  TWMWindowPosChanged = TWMWindowPosMsg;
  TWMWindowPosChanging = TWMWindowPosMsg;

  TWMWinIniChange = packed record
    Msg: Cardinal;
    Unused: Integer;
    Section: PChar;
    Result: Longint;
  end;

  TWMSettingChange = packed record
    Msg: Cardinal;
    Flag: Integer;
    Section: PChar;
    Result: Longint;
  end;

  TWMHelp = packed record
    Msg: Cardinal;
    Unused: Integer;
    HelpInfo: PHelpInfo;
    Result: Longint;
  end;

  TWMDisplayChange = packed record
    Msg: Cardinal;
    BitsPerPixel: Integer;
    Width: Word;
    Height: Word;
    Result: Longint;
  end;

  TWMContextMenu = packed record
    Msg: Cardinal;
    hWnd: HWND;
    case Integer of
      0: (
        XPos: Smallint;
        YPos: Smallint);
      1: (
        Pos: TSmallPoint;
        Result: Longint);
  end;

implementation

end.
