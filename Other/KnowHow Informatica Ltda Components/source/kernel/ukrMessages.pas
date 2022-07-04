{
=======================================================================

		KLIB v100
		Serious Software Made in Brazil


		home-page: www.knowhow-online.com.br (sorry, just portuguese)
		authors: Demian Lessa (demian@knowhow-online.com.br) and Leonardo Freitas

		Released under the Netscape Public License Version 1.0
	 (see license.txt)

		Unless otherwise noted, all materials provided in this release
		are copyright © 2001 by KnowHow Informatica Ltda.

=======================================================================
}

unit ukrMessages;

{$I s:\v100\include\iKLIB100.inc}

interface

{
--------------------------------------------------------------------------------
-------------------------- Message Mapping Support -----------------------------
--------------------------------------------------------------------------------
}

type

	TIdentMsgMapEntryEnum = type Byte;
	TIdentMsgMapEntryEnums = set of TIdentMsgMapEntryEnum;

const

	INV_MESSAGE = -1;

	MaxEnum_WM  = 208; { Window Messages }
	MaxEnum_BM  =   8; { Button Messages }
	MaxEnum_LBN =   6; { ListBox Notifications }
	MaxEnum_LB  =  41; { LibBox Messages }
	MaxEnum_CBN =  11; { ComboBox Notifications }
	MaxEnum_CB  =  35; { ComboBox Messages }
	MaxEnum_EN  =   8; { Edit Notifications }
	MaxEnum_EM  =  37; { Edit Messages }
	MaxEnum_SBM =   8; { Scroll Bar Messages }
	MaxEnum_DM  =   5; { Dialog Messages }

	MaxEnum_SBC =  15; { StatusBar Commands - from Windows.pas }
	MaxEnum_SM2 =  19; { StatusBar Messages - from CommCtrl.pas }

	MaxEnum_CM  =  56; { Component Messages }
	MaxEnum_CN  =  23; { Control Notifications }

	imeWM  = 0;
	imeBM  = 1;
	imeLBN = 2;
	imeLBM = 3;
	imeCBN = 4;
	imeCBM = 5;
	imeEN  = 6;
	imeEM  = 7;
	imeSBM = 8;
	imeDM  = 9;
	imeSBC = 10;
	imeSM2 = 11;
	imeCM  = 12;
	imeCN  = 13;

	IME_ALL_TYPES = [Low( TIdentMsgMapEntryEnum )..High( TIdentMsgMapEntryEnum )];

function MessageToIdent( Message: Integer; var Ident: string; Map: TIdentMsgMapEntryEnum ): Boolean;
function IdentToMessage( const Ident: string; var Message: Integer; Map: TIdentMsgMapEntryEnum ): Boolean;
function MessageToString( Message: Integer; Map: TIdentMsgMapEntryEnum ): string;
function StringToMessage( const Source: string; Map: TIdentMsgMapEntryEnum ): Integer;

function MessageToStringEnums( Message: Integer;
	EnumTypes: TIdentMsgMapEntryEnums; var Translated: Boolean ): string;
function StringToMessageEnums( const Message: string;
	EnumTypes: TIdentMsgMapEntryEnums; var Translated: Boolean ): Integer;

implementation

uses
	Windows, Messages, SysUtils, Classes, Controls, CommCtrl, uksyConsts, ukrUtils;

{
--------------------------------------------------------------------------------
-------------------------- Message Mapping Support -----------------------------
--------------------------------------------------------------------------------
}

const

	WMMessageMap: array[0..MaxEnum_WM-1] of TIdentMapEntry =
	(
		( Value: WM_NULL;                   Name: 'WM_NULL' ),
		( Value: WM_CREATE;                 Name: 'WM_CREATE' ),
		( Value: WM_DESTROY;                Name: 'WM_DESTROY' ),
		( Value: WM_MOVE;                   Name: 'WM_MOVE' ),
		( Value: WM_SIZE;                   Name: 'WM_SIZE' ),
		( Value: WM_ACTIVATE;               Name: 'WM_ACTIVATE' ),
		( Value: WM_SETFOCUS;               Name: 'WM_SETFOCUS' ),
    ( Value: WM_KILLFOCUS;              Name: 'WM_KILLFOCUS' ),
    ( Value: WM_ENABLE;                 Name: 'WM_ENABLE' ),
    ( Value: WM_SETREDRAW;              Name: 'WM_SETREDRAW' ),
		( Value: WM_SETTEXT;                Name: 'WM_SETTEXT' ),
		( Value: WM_GETTEXT;                Name: 'WM_GETTEXT' ),
		( Value: WM_GETTEXTLENGTH;          Name: 'WM_GETTEXTLENGTH' ),
    ( Value: WM_PAINT;                  Name: 'WM_PAINT' ),
    ( Value: WM_CLOSE;                  Name: 'WM_CLOSE' ),
		( Value: WM_QUERYENDSESSION;        Name: 'WM_QUERYENDSESSION' ),
		( Value: WM_QUIT;                   Name: 'WM_QUIT' ),
		( Value: WM_QUERYOPEN;              Name: 'WM_QUERYOPEN' ),
    ( Value: WM_ERASEBKGND;             Name: 'WM_ERASEBKGND' ),
    ( Value: WM_QUIT;                   Name: 'WM_QUIT' ),
    ( Value: WM_SYSCOLORCHANGE;         Name: 'WM_SYSCOLORCHANGE' ),
		( Value: WM_ENDSESSION;             Name: 'WM_ENDSESSION' ),
		( Value: WM_SYSTEMERROR;            Name: 'WM_SYSTEMERROR' ),
    ( Value: WM_SHOWWINDOW;             Name: 'WM_SHOWWINDOW' ),
    ( Value: WM_CTLCOLOR;               Name: 'WM_CTLCOLOR' ),
    ( Value: WM_WININICHANGE;           Name: 'WM_WININICHANGE' ),
		( Value: WM_SETTINGCHANGE;          Name: 'WM_SETTINGCHANGE' ),
		( Value: WM_DEVMODECHANGE;          Name: 'WM_DEVMODECHANGE' ),
    ( Value: WM_ACTIVATEAPP;            Name: 'WM_ACTIVATEAPP' ),
    ( Value: WM_FONTCHANGE;             Name: 'WM_FONTCHANGE' ),
    ( Value: WM_TIMECHANGE;             Name: 'WM_TIMECHANGE' ),
		( Value: WM_CANCELMODE;             Name: 'WM_CANCELMODE' ),
		( Value: WM_SETCURSOR;              Name: 'WM_SETCURSOR' ),
		( Value: WM_MOUSEACTIVATE;          Name: 'WM_MOUSEACTIVATE' ),
    ( Value: WM_CHILDACTIVATE;          Name: 'WM_CHILDACTIVATE' ),
    ( Value: WM_QUEUESYNC;              Name: 'WM_QUEUESYNC' ),
		( Value: WM_GETMINMAXINFO;          Name: 'WM_GETMINMAXINFO' ),
		( Value: WM_PAINTICON;              Name: 'WM_PAINTICON' ),
		( Value: WM_ICONERASEBKGND;         Name: 'WM_ICONERASEBKGND' ),
    ( Value: WM_NEXTDLGCTL;             Name: 'WM_NEXTDLGCTL' ),
    ( Value: WM_SPOOLERSTATUS;          Name: 'WM_SPOOLERSTATUS' ),
		( Value: WM_DRAWITEM;               Name: 'WM_DRAWITEM' ),
		( Value: WM_MEASUREITEM;            Name: 'WM_MEASUREITEM' ),
		( Value: WM_DELETEITEM;             Name: 'WM_DELETEITEM' ),
		( Value: WM_VKEYTOITEM;             Name: 'WM_VKEYTOITEM' ),
    ( Value: WM_CHARTOITEM;             Name: 'WM_CHARTOITEM' ),
		( Value: WM_SETFONT;                Name: 'WM_SETFONT' ),
    ( Value: WM_GETFONT;                Name: 'WM_GETFONT' ),
    ( Value: WM_SETHOTKEY;              Name: 'WM_SETHOTKEY' ),
		( Value: WM_GETHOTKEY;              Name: 'WM_GETHOTKEY' ),
		( Value: WM_QUERYDRAGICON;          Name: 'WM_QUERYDRAGICON' ),
		( Value: WM_COMPAREITEM;            Name: 'WM_COMPAREITEM' ),
    ( Value: WM_COMPACTING;             Name: 'WM_COMPACTING' ),
    ( Value: WM_COMMNOTIFY;             Name: 'WM_COMMNOTIFY' ),
    ( Value: WM_WINDOWPOSCHANGING;      Name: 'WM_WINDOWPOSCHANGING' ),
		( Value: WM_WINDOWPOSCHANGED;       Name: 'WM_WINDOWPOSCHANGED' ),
		( Value: WM_POWER;                  Name: 'WM_POWER' ),
    ( Value: WM_COPYDATA;               Name: 'WM_COPYDATA' ),
    ( Value: WM_CANCELJOURNAL;          Name: 'WM_CANCELJOURNAL' ),
    ( Value: WM_NOTIFY;                 Name: 'WM_NOTIFY' ),
    ( Value: WM_INPUTLANGCHANGEREQUEST; Name: 'WM_INPUTLANGCHANGEREQUEST' ),
		( Value: WM_INPUTLANGCHANGE;        Name: 'WM_INPUTLANGCHANGE' ),
    ( Value: WM_TCARD;                  Name: 'WM_TCARD' ),
    ( Value: WM_HELP;                   Name: 'WM_HELP' ),
		( Value: WM_USERCHANGED;            Name: 'WM_USERCHANGED' ),
    ( Value: WM_NOTIFYFORMAT;           Name: 'WM_NOTIFYFORMAT' ),
		( Value: WM_CONTEXTMENU;            Name: 'WM_CONTEXTMENU' ),
		( Value: WM_STYLECHANGING;          Name: 'WM_STYLECHANGING' ),
    ( Value: WM_STYLECHANGED;           Name: 'WM_STYLECHANGED' ),
    ( Value: WM_DISPLAYCHANGE;          Name: 'WM_DISPLAYCHANGE' ),
    ( Value: WM_GETICON;                Name: 'WM_GETICON' ),
		( Value: WM_SETICON;                Name: 'WM_SETICON' ),
		( Value: WM_NCCREATE;               Name: 'WM_NCCREATE' ),
    ( Value: WM_NCDESTROY;              Name: 'WM_NCDESTROY' ),
    ( Value: WM_NCCALCSIZE;             Name: 'WM_NCCALCSIZE' ),
    ( Value: WM_NCHITTEST;              Name: 'WM_NCHITTEST' ),
		( Value: WM_NCPAINT;                Name: 'WM_NCPAINT' ),
    ( Value: WM_NCACTIVATE;             Name: 'WM_NCACTIVATE' ),
		( Value: WM_GETDLGCODE;             Name: 'WM_GETDLGCODE' ),
    ( Value: WM_NCMOUSEMOVE;            Name: 'WM_NCMOUSEMOVE' ),
    ( Value: WM_NCLBUTTONDOWN;          Name: 'WM_NCLBUTTONDOWN' ),
		( Value: WM_NCLBUTTONUP;            Name: 'WM_NCLBUTTONUP' ),
    ( Value: WM_NCLBUTTONDBLCLK;        Name: 'WM_NCLBUTTONDBLCLK' ),
		( Value: WM_NCRBUTTONDOWN;          Name: 'WM_NCRBUTTONDOWN' ),
		( Value: WM_NCRBUTTONUP;            Name: 'WM_NCRBUTTONUP' ),
    ( Value: WM_NCRBUTTONDBLCLK;        Name: 'WM_NCRBUTTONDBLCLK' ),
		( Value: WM_NCMBUTTONDOWN;          Name: 'WM_NCMBUTTONDOWN' ),
    ( Value: WM_NCMBUTTONUP;            Name: 'WM_NCMBUTTONUP' ),
    ( Value: WM_NCMBUTTONDBLCLK;        Name: 'WM_NCMBUTTONDBLCLK' ),
		( Value: WM_KEYFIRST;               Name: 'WM_KEYFIRST' ),
    ( Value: WM_KEYDOWN;                Name: 'WM_KEYDOWN' ),
		( Value: WM_KEYUP;                  Name: 'WM_KEYUP' ),
    ( Value: WM_CHAR;                   Name: 'WM_CHAR' ),
		( Value: WM_DEADCHAR;               Name: 'WM_DEADCHAR' ),
    ( Value: WM_SYSKEYDOWN;             Name: 'WM_SYSKEYDOWN' ),
		( Value: WM_SYSKEYUP;               Name: 'WM_SYSKEYUP' ),
		( Value: WM_SYSCHAR;                Name: 'WM_SYSCHAR' ),
    ( Value: WM_SYSDEADCHAR;            Name: 'WM_SYSDEADCHAR' ),
    ( Value: WM_KEYLAST;                Name: 'WM_KEYLAST' ),
    ( Value: WM_INITDIALOG;             Name: 'WM_INITDIALOG' ),
		( Value: WM_COMMAND;                Name: 'WM_COMMAND' ),
		( Value: WM_SYSCOMMAND;             Name: 'WM_SYSCOMMAND' ),
    ( Value: WM_TIMER;                  Name: 'WM_TIMER' ),
    ( Value: WM_HSCROLL;                Name: 'WM_HSCROLL' ),
    ( Value: WM_VSCROLL;                Name: 'WM_VSCROLL' ),
    ( Value: WM_INITMENU;               Name: 'WM_INITMENU' ),
		( Value: WM_INITMENUPOPUP;          Name: 'WM_INITMENUPOPUP' ),
    ( Value: WM_MENUSELECT;             Name: 'WM_MENUSELECT' ),
		( Value: WM_MENUCHAR;               Name: 'WM_MENUCHAR' ),
    ( Value: WM_ENTERIDLE;              Name: 'WM_ENTERIDLE' ),
    ( Value: WM_CTLCOLORMSGBOX;         Name: 'WM_CTLCOLORMSGBOX' ),
		( Value: WM_CTLCOLOREDIT;           Name: 'WM_CTLCOLOREDIT' ),
		( Value: WM_CTLCOLORLISTBOX;        Name: 'WM_CTLCOLORLISTBOX' ),
    ( Value: WM_CTLCOLORBTN;            Name: 'WM_CTLCOLORBTN' ),
    ( Value: WM_CTLCOLORDLG;            Name: 'WM_CTLCOLORDLG' ),
		( Value: WM_CTLCOLORSCROLLBAR;      Name: 'WM_CTLCOLORSCROLLBAR' ),
		( Value: WM_CTLCOLORSTATIC;         Name: 'WM_CTLCOLORSTATIC' ),
		( Value: WM_MOUSEFIRST;             Name: 'WM_MOUSEFIRST' ),
		( Value: WM_MOUSEMOVE;              Name: 'WM_MOUSEMOVE' ),
    ( Value: WM_LBUTTONDOWN;            Name: 'WM_LBUTTONDOWN' ),
    ( Value: WM_LBUTTONUP;              Name: 'WM_LBUTTONUP' ),
		( Value: WM_LBUTTONDBLCLK;          Name: 'WM_LBUTTONDBLCLK' ),
    ( Value: WM_RBUTTONDOWN;            Name: 'WM_RBUTTONDOWN' ),
		( Value: WM_RBUTTONUP;              Name: 'WM_RBUTTONUP' ),
    ( Value: WM_RBUTTONDBLCLK;          Name: 'WM_RBUTTONDBLCLK' ),
    ( Value: WM_MBUTTONDOWN;            Name: 'WM_MBUTTONDOWN' ),
		( Value: WM_MBUTTONUP;              Name: 'WM_MBUTTONUP' ),
    ( Value: WM_MBUTTONDBLCLK;          Name: 'WM_MBUTTONDBLCLK' ),
    ( Value: WM_MOUSEWHEEL;             Name: 'WM_MOUSEWHEEL' ),
		( Value: WM_MOUSELAST;              Name: 'WM_MOUSELAST' ),
		( Value: WM_PARENTNOTIFY;           Name: 'WM_PARENTNOTIFY' ),
		( Value: WM_ENTERMENULOOP;          Name: 'WM_ENTERMENULOOP' ),
    ( Value: WM_EXITMENULOOP;           Name: 'WM_EXITMENULOOP' ),
    ( Value: WM_NEXTMENU;               Name: 'WM_NEXTMENU' ),
		( Value: WM_SIZING;                 Name: 'WM_SIZING' ),
		( Value: WM_CAPTURECHANGED;         Name: 'WM_CAPTURECHANGED' ),
		( Value: WM_MOVING;                 Name: 'WM_MOVING' ),
		( Value: WM_POWERBROADCAST;         Name: 'WM_POWERBROADCAST' ),
    ( Value: WM_DEVICECHANGE;           Name: 'WM_DEVICECHANGE' ),
    ( Value: WM_IME_STARTCOMPOSITION;   Name: 'WM_IME_STARTCOMPOSITION' ),
		( Value: WM_IME_ENDCOMPOSITION;     Name: 'WM_IME_ENDCOMPOSITION' ),
		( Value: WM_IME_COMPOSITION;        Name: 'WM_IME_COMPOSITION' ),
    ( Value: WM_IME_KEYLAST;            Name: 'WM_IME_KEYLAST' ),
    ( Value: WM_IME_SETCONTEXT;         Name: 'WM_IME_SETCONTEXT' ),
    ( Value: WM_IME_NOTIFY;             Name: 'WM_IME_NOTIFY' ),
    ( Value: WM_IME_CONTROL;            Name: 'WM_IME_CONTROL' ),
		( Value: WM_IME_COMPOSITIONFULL;    Name: 'WM_IME_COMPOSITIONFULL' ),
    ( Value: WM_IME_SELECT;             Name: 'WM_IME_SELECT' ),
    ( Value: WM_IME_CHAR;               Name: 'WM_IME_CHAR' ),
    ( Value: WM_IME_KEYDOWN;            Name: 'WM_IME_KEYDOWN' ),
    ( Value: WM_IME_KEYUP;              Name: 'WM_IME_KEYUP' ),
		( Value: WM_MDICREATE;              Name: 'WM_MDICREATE' ),
		( Value: WM_MDIDESTROY;             Name: 'WM_MDIDESTROY' ),
    ( Value: WM_MDIACTIVATE;            Name: 'WM_MDIACTIVATE' ),
    ( Value: WM_MDIRESTORE;             Name: 'WM_MDIRESTORE' ),
    ( Value: WM_MDINEXT;                Name: 'WM_MDINEXT' ),
		( Value: WM_MDIMAXIMIZE;            Name: 'WM_MDIMAXIMIZE' ),
		( Value: WM_MDITILE;                Name: 'WM_MDITILE' ),
    ( Value: WM_MDICASCADE;             Name: 'WM_MDICASCADE' ),
		( Value: WM_MDIICONARRANGE;         Name: 'WM_MDIICONARRANGE' ),
    ( Value: WM_MDIGETACTIVE;           Name: 'WM_MDIGETACTIVE' ),
		( Value: WM_MDISETMENU;             Name: 'WM_MDISETMENU' ),
    ( Value: WM_ENTERSIZEMOVE;          Name: 'WM_ENTERSIZEMOVE' ),
		( Value: WM_EXITSIZEMOVE;           Name: 'WM_EXITSIZEMOVE' ),
    ( Value: WM_DROPFILES;              Name: 'WM_DROPFILES' ),
    ( Value: WM_MDIREFRESHMENU;         Name: 'WM_MDIREFRESHMENU' ),
		( Value: WM_MOUSEHOVER;             Name: 'WM_MOUSEHOVER' ),
    ( Value: WM_MOUSELEAVE;             Name: 'WM_MOUSELEAVE' ),
		( Value: WM_CUT;                    Name: 'WM_CUT' ),
		( Value: WM_COPY;                   Name: 'WM_COPY' ),
    ( Value: WM_PASTE;                  Name: 'WM_PASTE' ),
		( Value: WM_CLEAR;                  Name: 'WM_CLEAR' ),
    ( Value: WM_UNDO;                   Name: 'WM_UNDO' ),
    ( Value: WM_RENDERFORMAT;           Name: 'WM_RENDERFORMAT' ),
		( Value: WM_RENDERALLFORMATS;       Name: 'WM_RENDERALLFORMATS' ),
    ( Value: WM_DESTROYCLIPBOARD;       Name: 'WM_DESTROYCLIPBOARD' ),
		( Value: WM_DRAWCLIPBOARD;          Name: 'WM_DRAWCLIPBOARD' ),
    ( Value: WM_PAINTCLIPBOARD;         Name: 'WM_PAINTCLIPBOARD' ),
    ( Value: WM_VSCROLLCLIPBOARD;       Name: 'WM_VSCROLLCLIPBOARD' ),
    ( Value: WM_SIZECLIPBOARD;          Name: 'WM_SIZECLIPBOARD' ),
		( Value: WM_ASKCBFORMATNAME;        Name: 'WM_ASKCBFORMATNAME' ),
		( Value: WM_CHANGECBCHAIN;          Name: 'WM_CHANGECBCHAIN' ),
    ( Value: WM_HSCROLLCLIPBOARD;       Name: 'WM_HSCROLLCLIPBOARD' ),
    ( Value: WM_QUERYNEWPALETTE;        Name: 'WM_QUERYNEWPALETTE' ),
    ( Value: WM_PALETTEISCHANGING;      Name: 'WM_PALETTEISCHANGING' ),
		( Value: WM_PALETTECHANGED;         Name: 'WM_PALETTECHANGED' ),
		( Value: WM_HOTKEY;                 Name: 'WM_HOTKEY' ),
    ( Value: WM_PRINT;                  Name: 'WM_PRINT' ),
    ( Value: WM_PRINTCLIENT;            Name: 'WM_PRINTCLIENT' ),
		( Value: WM_HANDHELDFIRST;          Name: 'WM_HANDHELDFIRST' ),
		( Value: WM_HANDHELDLAST;           Name: 'WM_HANDHELDLAST' ),
		( Value: WM_PENWINFIRST;            Name: 'WM_PENWINFIRST' ),
		( Value: WM_PENWINLAST;             Name: 'WM_PENWINLAST' ),
    ( Value: WM_COALESCE_FIRST;         Name: 'WM_COALESCE_FIRST' ),
		( Value: WM_COALESCE_LAST;          Name: 'WM_COALESCE_LAST' ),
		( Value: WM_DDE_FIRST;              Name: 'WM_DDE_FIRST' ),
		( Value: WM_DDE_INITIATE;           Name: 'WM_DDE_INITIATE' ),
		( Value: WM_DDE_TERMINATE;          Name: 'WM_DDE_TERMINATE' ),
		( Value: WM_DDE_ADVISE;             Name: 'WM_DDE_ADVISE' ),
		( Value: WM_DDE_UNADVISE;           Name: 'WM_DDE_UNADVISE' ),
		( Value: WM_DDE_ACK;                Name: 'WM_DDE_ACK' ),
		( Value: WM_DDE_DATA;               Name: 'WM_DDE_DATA' ),
		( Value: WM_DDE_REQUEST;            Name: 'WM_DDE_REQUEST' ),
		( Value: WM_DDE_POKE;               Name: 'WM_DDE_POKE' ),
		( Value: WM_DDE_EXECUTE;            Name: 'WM_DDE_EXECUTE' ),
		( Value: WM_DDE_LAST;               Name: 'WM_DDE_LAST' ),
		( Value: WM_APP;                    Name: 'WM_APP' ),
		( Value: WM_USER;                   Name: 'WM_USER' )
	);

	ButtonMessageMap : array[0..MaxEnum_BM-1] of TIdentMapEntry =
	(
		( Value: BM_GETCHECK; Name: 'BM_GETCHECK' ),
		( Value: BM_SETCHECK; Name: 'BM_SETCHECK' ),
		( Value: BM_GETSTATE; Name: 'BM_GETSTATE' ),
		( Value: BM_SETSTATE; Name: 'BM_SETSTATE' ),
		( Value: BM_SETSTYLE; Name: 'BM_SETSTYLE' ),
		( Value: BM_CLICK;    Name: 'BM_CLICK' ),
		( Value: BM_GETIMAGE; Name: 'BM_GETIMAGE' ),
		( Value: BM_SETIMAGE; Name: 'BM_SETIMAGE' )
	);

	ListBoxNotificationMap : array[0..MaxEnum_LBN-1] of TIdentMapEntry =
	(
		( Value: LBN_ERRSPACE;  Name: 'LBN_ERRSPACE' ),
		( Value: LBN_SELCHANGE; Name: 'LBN_SELCHANGE' ),
		( Value: LBN_DBLCLK;    Name: 'LBN_DBLCLK' ),
		( Value: LBN_SELCANCEL; Name: 'LBN_SELCANCEL' ),
		( Value: LBN_SETFOCUS;  Name: 'LBN_SETFOCUS' ),
		( Value: LBN_KILLFOCUS; Name: 'LBN_KILLFOCUS' )
	);

	ListBoxMessageMap : array[0..MaxEnum_LB-1] of TIdentMapEntry =
	(
		( Value: LB_ADDSTRING;           Name: 'LB_ADDSTRING' ),
		( Value: LB_INSERTSTRING;        Name: 'LB_INSERTSTRING' ),
		( Value: LB_DELETESTRING;        Name: 'LB_DELETESTRING' ),
		( Value: LB_SELITEMRANGEEX;      Name: 'LB_SELITEMRANGEEX' ),
		( Value: LB_RESETCONTENT;        Name: 'LB_RESETCONTENT' ),
		( Value: LB_SETSEL;              Name: 'LB_SETSEL' ),
		( Value: LB_SETCURSEL;           Name: 'LB_SETCURSEL' ),
		( Value: LB_GETSEL;              Name: 'LB_GETSEL' ),
		( Value: LB_GETCURSEL;           Name: 'LB_GETCURSEL' ),
		( Value: LB_GETTEXT;             Name: 'LB_GETTEXT' ),
		( Value: LB_GETTEXTLEN;          Name: 'LB_GETTEXTLEN' ),
		( Value: LB_GETCOUNT;            Name: 'LB_GETCOUNT' ),
		( Value: LB_SELECTSTRING;        Name: 'LB_SELECTSTRING' ),
		( Value: LB_DIR;                 Name: 'LB_DIR' ),
		( Value: LB_GETTOPINDEX;         Name: 'LB_GETTOPINDEX' ),
		( Value: LB_FINDSTRING;          Name: 'LB_FINDSTRING' ),
		( Value: LB_GETSELCOUNT;         Name: 'LB_GETSELCOUNT' ),
		( Value: LB_GETSELITEMS;         Name: 'LB_GETSELITEMS' ),
		( Value: LB_SETTABSTOPS;         Name: 'LB_SETTABSTOPS' ),
		( Value: LB_GETHORIZONTALEXTENT; Name: 'LB_GETHORIZONTALEXTENT' ),
		( Value: LB_SETHORIZONTALEXTENT; Name: 'LB_SETHORIZONTALEXTENT' ),
		( Value: LB_SETCOLUMNWIDTH;      Name: 'LB_SETCOLUMNWIDTH' ),
		( Value: LB_ADDFILE;             Name: 'LB_ADDFILE' ),
		( Value: LB_SETTOPINDEX;         Name: 'LB_SETTOPINDEX' ),
		( Value: LB_GETITEMRECT;         Name: 'LB_GETITEMRECT' ),
		( Value: LB_GETITEMDATA;         Name: 'LB_GETITEMDATA' ),
		( Value: LB_SETITEMDATA;         Name: 'LB_SETITEMDATA' ),
		( Value: LB_SELITEMRANGE;        Name: 'LB_SELITEMRANGE' ),
		( Value: LB_SETANCHORINDEX;      Name: 'LB_SETANCHORINDEX' ),
		( Value: LB_GETANCHORINDEX;      Name: 'LB_GETANCHORINDEX' ),
		( Value: LB_SETCARETINDEX;       Name: 'LB_SETCARETINDEX' ),
		( Value: LB_GETCARETINDEX;       Name: 'LB_GETCARETINDEX' ),
		( Value: LB_SETITEMHEIGHT;       Name: 'LB_SETITEMHEIGHT' ),
		( Value: LB_GETITEMHEIGHT;       Name: 'LB_GETITEMHEIGHT' ),
		( Value: LB_FINDSTRINGEXACT;     Name: 'LB_FINDSTRINGEXACT' ),
		( Value: LB_SETLOCALE;           Name: 'LB_SETLOCALE' ),
		( Value: LB_GETLOCALE;           Name: 'LB_GETLOCALE' ),
		( Value: LB_SETCOUNT;            Name: 'LB_SETCOUNT' ),
		( Value: LB_INITSTORAGE;         Name: 'LB_INITSTORAGE' ),
		( Value: LB_ITEMFROMPOINT;       Name: 'LB_ITEMFROMPOINT' ),
		( Value: LB_MSGMAX;              Name: 'LB_MSGMAX' )
	);

	ComboBoxNotificationMap : array[0..MaxEnum_CBN-1] of TIdentMapEntry =
	(
		( Value: CBN_ERRSPACE;     Name: 'CBN_ERRSPACE' ),
		( Value: CBN_SELCHANGE;    Name: 'CBN_SELCHANGE' ),
		( Value: CBN_DBLCLK;       Name: 'CBN_DBLCLK' ),
		( Value: CBN_SETFOCUS;     Name: 'CBN_SETFOCUS' ),
		( Value: CBN_KILLFOCUS;    Name: 'CBN_KILLFOCUS' ),
		( Value: CBN_EDITCHANGE;   Name: 'CBN_EDITCHANGE' ),
		( Value: CBN_EDITUPDATE;   Name: 'CBN_EDITUPDATE' ),
		( Value: CBN_DROPDOWN;     Name: 'CBN_DROPDOWN' ),
		( Value: CBN_CLOSEUP;      Name: 'CBN_CLOSEUP' ),
		( Value: CBN_SELENDOK;     Name: 'CBN_SELENDOK' ),
		( Value: CBN_SELENDCANCEL; Name: 'CBN_SELENDCANCEL' )
	);

	ComboBoxMessageMap : array[0..MaxEnum_CB-1] of TIdentMapEntry =
	(
		( Value: CB_GETEDITSEL;            Name: 'CB_GETEDITSEL' ),
		( Value: CB_LIMITTEXT;             Name: 'CB_LIMITTEXT' ),
		( Value: CB_SETEDITSEL;            Name: 'CB_SETEDITSEL' ),
		( Value: CB_ADDSTRING;             Name: 'CB_ADDSTRING' ),
		( Value: CB_DELETESTRING;          Name: 'CB_DELETESTRING' ),
		( Value: CB_DIR;                   Name: 'CB_DIR' ),
		( Value: CB_GETCOUNT;              Name: 'CB_GETCOUNT' ),
		( Value: CB_GETCURSEL;             Name: 'CB_GETCURSEL' ),
		( Value: CB_GETLBTEXT;             Name: 'CB_GETLBTEXT' ),
		( Value: CB_GETLBTEXTLEN;          Name: 'CB_GETLBTEXTLEN' ),
		( Value: CB_INSERTSTRING;          Name: 'CB_INSERTSTRING' ),
		( Value: CB_RESETCONTENT;          Name: 'CB_RESETCONTENT' ),
		( Value: CB_FINDSTRING;            Name: 'CB_FINDSTRING' ),
		( Value: CB_SELECTSTRING;          Name: 'CB_SELECTSTRING' ),
		( Value: CB_SETCURSEL;             Name: 'CB_SETCURSEL' ),
		( Value: CB_SHOWDROPDOWN;          Name: 'CB_SHOWDROPDOWN' ),
		( Value: CB_GETITEMDATA;           Name: 'CB_GETITEMDATA' ),
		( Value: CB_SETITEMDATA;           Name: 'CB_SETITEMDATA' ),
		( Value: CB_GETDROPPEDCONTROLRECT; Name: 'CB_GETDROPPEDCONTROLRECT' ),
		( Value: CB_SETITEMHEIGHT;         Name: 'CB_SETITEMHEIGHT' ),
		( Value: CB_GETITEMHEIGHT;         Name: 'CB_GETITEMHEIGHT' ),
		( Value: CB_SETEXTENDEDUI;         Name: 'CB_SETEXTENDEDUI' ),
		( Value: CB_GETEXTENDEDUI;         Name: 'CB_GETEXTENDEDUI' ),
		( Value: CB_GETDROPPEDSTATE;       Name: 'CB_GETDROPPEDSTATE' ),
		( Value: CB_FINDSTRINGEXACT;       Name: 'CB_FINDSTRINGEXACT' ),
		( Value: CB_SETLOCALE;             Name: 'CB_SETLOCALE' ),
		( Value: CB_GETLOCALE;             Name: 'CB_GETLOCALE' ),
		( Value: CB_GETTOPINDEX;           Name: 'CB_GETTOPINDEX' ),
		( Value: CB_SETTOPINDEX;           Name: 'CB_SETTOPINDEX' ),
		( Value: CB_GETHORIZONTALEXTENT;   Name: 'CB_GETHORIZONTALEXTENT' ),
		( Value: CB_SETHORIZONTALEXTENT;   Name: 'CB_SETHORIZONTALEXTENT' ),
		( Value: CB_GETDROPPEDWIDTH;       Name: 'CB_GETDROPPEDWIDTH' ),
		( Value: CB_SETDROPPEDWIDTH;       Name: 'CB_SETDROPPEDWIDTH' ),
		( Value: CB_INITSTORAGE;           Name: 'CB_INITSTORAGE' ),
		( Value: CB_MSGMAX;                Name: 'CB_MSGMAX' )
	);

	EditNotificationMap : array[0..MaxEnum_EN-1] of TIdentMapEntry =
	(
		( Value: EN_SETFOCUS;  Name: 'EN_SETFOCUS' ),
		( Value: EN_KILLFOCUS; Name: 'EN_KILLFOCUS' ),
		( Value: EN_CHANGE;    Name: 'EN_CHANGE' ),
		( Value: EN_UPDATE;    Name: 'EN_UPDATE' ),
		( Value: EN_ERRSPACE;  Name: 'EN_ERRSPACE' ),
		( Value: EN_MAXTEXT;   Name: 'EN_MAXTEXT' ),
		( Value: EN_HSCROLL;   Name: 'EN_HSCROLL' ),
		( Value: EN_VSCROLL;   Name: 'EN_VSCROLL' )
	);

	EditMessageMap : array[0..MaxEnum_EM-1] of TIdentMapEntry =
	(
		( Value: EM_GETSEL;              Name: 'EM_GETSEL' ),
		( Value: EM_SETSEL;              Name: 'EM_SETSEL' ),
		( Value: EM_GETRECT;             Name: 'EM_GETRECT' ),
		( Value: EM_SETRECT;             Name: 'EM_SETRECT' ),
		( Value: EM_SETRECTNP;           Name: 'EM_SETRECTNP' ),
		( Value: EM_SCROLL;              Name: 'EM_SCROLL' ),
		( Value: EM_LINESCROLL;          Name: 'EM_LINESCROLL' ),
		( Value: EM_SCROLLCARET;         Name: 'EM_SCROLLCARET' ),
		( Value: EM_GETMODIFY;           Name: 'EM_GETMODIFY' ),
		( Value: EM_SETMODIFY;           Name: 'EM_SETMODIFY' ),
		( Value: EM_GETLINECOUNT;        Name: 'EM_GETLINECOUNT' ),
		( Value: EM_LINEINDEX;           Name: 'EM_LINEINDEX' ),
		( Value: EM_SETHANDLE;           Name: 'EM_SETHANDLE' ),
		( Value: EM_GETHANDLE;           Name: 'EM_GETHANDLE' ),
		( Value: EM_GETTHUMB;            Name: 'EM_GETTHUMB' ),
		( Value: EM_LINELENGTH;          Name: 'EM_LINELENGTH' ),
		( Value: EM_REPLACESEL;          Name: 'EM_REPLACESEL' ),
		( Value: EM_GETLINE;             Name: 'EM_GETLINE' ),
		( Value: EM_LIMITTEXT;           Name: 'EM_LIMITTEXT' ),
		( Value: EM_CANUNDO;             Name: 'EM_CANUNDO' ),
		( Value: EM_UNDO;                Name: 'EM_UNDO' ),
		( Value: EM_FMTLINES;            Name: 'EM_FMTLINES' ),
		( Value: EM_LINEFROMCHAR;        Name: 'EM_LINEFROMCHAR' ),
		( Value: EM_SETTABSTOPS;         Name: 'EM_SETTABSTOPS' ),
		( Value: EM_SETPASSWORDCHAR;     Name: 'EM_SETPASSWORDCHAR' ),
		( Value: EM_EMPTYUNDOBUFFER;     Name: 'EM_EMPTYUNDOBUFFER' ),
		( Value: EM_GETFIRSTVISIBLELINE; Name: 'EM_GETFIRSTVISIBLELINE' ),
		( Value: EM_SETREADONLY;         Name: 'EM_SETREADONLY' ),
		( Value: EM_SETWORDBREAKPROC;    Name: 'EM_SETWORDBREAKPROC' ),
		( Value: EM_GETWORDBREAKPROC;    Name: 'EM_GETWORDBREAKPROC' ),
		( Value: EM_GETPASSWORDCHAR;     Name: 'EM_GETPASSWORDCHAR' ),
		( Value: EM_SETMARGINS;          Name: 'EM_SETMARGINS' ),
		( Value: EM_GETMARGINS;          Name: 'EM_GETMARGINS' ),
		( Value: EM_SETLIMITTEXT;        Name: 'EM_SETLIMITTEXT' ),
		( Value: EM_GETLIMITTEXT;        Name: 'EM_GETLIMITTEXT' ),
		( Value: EM_POSFROMCHAR;         Name: 'EM_POSFROMCHAR' ),
		( Value: EM_CHARFROMPOS;         Name: 'EM_CHARFROMPOS' )
	);

	ScrollBarMessageMap : array[0..MaxEnum_SBM-1] of TIdentMapEntry =
	(
		( Value: SBM_SETPOS;         Name: 'SBM_SETPOS' ),
		( Value: SBM_GETPOS;         Name: 'SBM_GETPOS' ),
		( Value: SBM_SETRANGE;       Name: 'SBM_SETRANGE' ),
		( Value: SBM_SETRANGEREDRAW; Name: 'SBM_SETRANGEREDRAW' ),
		( Value: SBM_GETRANGE;       Name: 'SBM_GETRANGE' ),
		( Value: SBM_ENABLE_ARROWS;  Name: 'SBM_ENABLE_ARROWS' ),
		( Value: SBM_SETSCROLLINFO;  Name: 'SBM_SETSCROLLINFO' ),
		( Value: SBM_GETSCROLLINFO;  Name: 'SBM_GETSCROLLINFO' )
	);

	DialogMessageMap : array[0..MaxEnum_DM-1] of TIdentMapEntry =
	(
		( Value: DM_GETDEFID;   Name: 'DM_GETDEFID' ),
		( Value: DM_SETDEFID;   Name: 'DM_SETDEFID' ),
		( Value: DM_REPOSITION; Name: 'DM_REPOSITION' ),
		( Value: PSM_PAGEINFO;  Name: 'PSM_PAGEINFO' ),
		( Value: PSM_SHEETINFO; Name: 'PSM_SHEETINFO' )
	);

	StatusBarCommandsMap: array[0..MaxEnum_SBC-1] of TIdentMapEntry =
	(
		( Value: SB_LINEUP;        Name: 'SB_LINEUP' ),
		( Value: SB_LINELEFT;      Name: 'SB_LINELEFT' ),
		( Value: SB_LINEDOWN;      Name: 'SB_LINEDOWN' ),
		( Value: SB_LINERIGHT;     Name: 'SB_LINERIGHT' ),
		( Value: SB_PAGEUP;        Name: 'SB_PAGEUP' ),
		( Value: SB_PAGELEFT;      Name: 'SB_PAGELEFT' ),
		( Value: SB_PAGEDOWN;      Name: 'SB_PAGEDOWN' ),
		( Value: SB_PAGERIGHT;     Name: 'SB_PAGERIGHT' ),
		( Value: SB_THUMBPOSITION; Name: 'SB_THUMBPOSITION' ),
		( Value: SB_THUMBTRACK;    Name: 'SB_THUMBTRACK' ),
		( Value: SB_TOP;           Name: 'SB_TOP' ),
		( Value: SB_LEFT;          Name: 'SB_LEFT' ),
		( Value: SB_BOTTOM;        Name: 'SB_BOTTOM' ),
		( Value: SB_RIGHT;         Name: 'SB_RIGHT' ),
		( Value: SB_ENDSCROLL;     Name: 'SB_ENDSCROLL' )
	);

	StatusBarMessagesMap: array[0..MaxEnum_SM2-1] of TIdentMapEntry =
	(
		( Value: SB_SETTEXTA;       Name: 'SB_SETTEXTA' ),
		( Value: SB_GETTEXTA;       Name: 'SB_GETTEXTA' ),
		( Value: SB_GETTEXTLENGTHA; Name: 'SB_GETTEXTLENGTHA' ),
		( Value: SB_SETTEXTW;       Name: 'SB_SETTEXTW' ),
		( Value: SB_GETTEXTW;       Name: 'SB_GETTEXTW' ),
		( Value: SB_GETTEXTLENGTHW; Name: 'SB_GETTEXTLENGTHW' ),
		( Value: SB_SETTEXT;        Name: 'SB_SETTEXT' ),
		( Value: SB_GETTEXT;        Name: 'SB_GETTEXT' ),
		( Value: SB_GETTEXTLENGTH;  Name: 'SB_GETTEXTLENGTH' ),
		( Value: SB_SETPARTS;       Name: 'SB_SETPARTS' ),
		( Value: SB_GETPARTS;       Name: 'SB_GETPARTS' ),
		( Value: SB_GETBORDERS;     Name: 'SB_GETBORDERS' ),
		( Value: SB_SETMINHEIGHT;   Name: 'SB_SETMINHEIGHT' ),
		( Value: SB_SIMPLE;         Name: 'SB_SIMPLE' ),
		( Value: SB_GETRECT;        Name: 'SB_GETRECT' ),
		( Value: SBT_OWNERDRAW;     Name: 'SBT_OWNERDRAW' ),
		( Value: SBT_NOBORDERS;     Name: 'SBT_NOBORDERS' ),
		( Value: SBT_POPOUT;        Name: 'SBT_POPOUT' ),
		( Value: SBT_RTLREADING;    Name: 'SBT_RTLREADING' )
	);

	ComponentMessageMap : array[0..MaxEnum_CM-1] of TIdentMapEntry =
	(
		( Value: CM_ACTIVATE;              Name: 'CM_ACTIVATE' ),
		( Value: CM_DEACTIVATE;            Name: 'CM_DEACTIVATE' ),
		( Value: CM_GOTFOCUS;              Name: 'CM_GOTFOCUS' ),
		( Value: CM_LOSTFOCUS;             Name: 'CM_LOSTFOCUS' ),
		( Value: CM_CANCELMODE;            Name: 'CM_CANCELMODE' ),
		( Value: CM_DIALOGKEY;             Name: 'CM_DIALOGKEY' ),
		( Value: CM_DIALOGCHAR;            Name: 'CM_DIALOGCHAR' ),
		( Value: CM_FOCUSCHANGED;          Name: 'CM_FOCUSCHANGED' ),
		( Value: CM_PARENTFONTCHANGED;     Name: 'CM_PARENTFONTCHANGED' ),
		( Value: CM_PARENTCOLORCHANGED;    Name: 'CM_PARENTCOLORCHANGED' ),
		( Value: CM_HITTEST;               Name: 'CM_HITTEST' ),
		( Value: CM_VISIBLECHANGED;        Name: 'CM_VISIBLECHANGED' ),
		( Value: CM_ENABLEDCHANGED;        Name: 'CM_ENABLEDCHANGED' ),
		( Value: CM_COLORCHANGED;          Name: 'CM_COLORCHANGED' ),
		( Value: CM_FONTCHANGED;           Name: 'CM_FONTCHANGED' ),
		( Value: CM_CURSORCHANGED;         Name: 'CM_CURSORCHANGED' ),
		( Value: CM_CTL3DCHANGED;          Name: 'CM_CTL3DCHANGED' ),
		( Value: CM_PARENTCTL3DCHANGED;    Name: 'CM_PARENTCTL3DCHANGED' ),
		( Value: CM_TEXTCHANGED;           Name: 'CM_TEXTCHANGED' ),
		( Value: CM_MOUSEENTER;            Name: 'CM_MOUSEENTER' ),
		( Value: CM_MOUSELEAVE;            Name: 'CM_MOUSELEAVE' ),
		( Value: CM_MENUCHANGED;           Name: 'CM_MENUCHANGED' ),
		( Value: CM_APPKEYDOWN;            Name: 'CM_APPKEYDOWN' ),
		( Value: CM_APPSYSCOMMAND;         Name: 'CM_APPSYSCOMMAND' ),
		( Value: CM_BUTTONPRESSED;         Name: 'CM_BUTTONPRESSED' ),
		( Value: CM_SHOWINGCHANGED;        Name: 'CM_SHOWINGCHANGED' ),
		( Value: CM_ENTER;                 Name: 'CM_ENTER' ),
		( Value: CM_EXIT;                  Name: 'CM_EXIT' ),
		( Value: CM_DESIGNHITTEST;         Name: 'CM_DESIGNHITTEST' ),
		( Value: CM_ICONCHANGED;           Name: 'CM_ICONCHANGED' ),
		( Value: CM_WANTSPECIALKEY;        Name: 'CM_WANTSPECIALKEY' ),
		( Value: CM_INVOKEHELP;            Name: 'CM_INVOKEHELP' ),
		( Value: CM_WINDOWHOOK;            Name: 'CM_WINDOWHOOK' ),
		( Value: CM_RELEASE;               Name: 'CM_RELEASE' ),
		( Value: CM_SHOWHINTCHANGED;       Name: 'CM_SHOWHINTCHANGED' ),
		( Value: CM_PARENTSHOWHINTCHANGED; Name: 'CM_PARENTSHOWHINTCHANGED' ),
		( Value: CM_SYSCOLORCHANGE;        Name: 'CM_SYSCOLORCHANGE' ),
		( Value: CM_WININICHANGE;          Name: 'CM_WININICHANGE' ),
		( Value: CM_FONTCHANGE;            Name: 'CM_FONTCHANGE' ),
		( Value: CM_TIMECHANGE;            Name: 'CM_TIMECHANGE' ),
		( Value: CM_TABSTOPCHANGED;        Name: 'CM_TABSTOPCHANGED' ),
		( Value: CM_UIACTIVATE;            Name: 'CM_UIACTIVATE' ),
		( Value: CM_UIDEACTIVATE;          Name: 'CM_UIDEACTIVATE' ),
		( Value: CM_DOCWINDOWACTIVATE;     Name: 'CM_DOCWINDOWACTIVATE' ),
		( Value: CM_CONTROLLISTCHANGE;     Name: 'CM_CONTROLLISTCHANGE' ),
		( Value: CM_GETDATALINK;           Name: 'CM_GETDATALINK' ),
		( Value: CM_CHILDKEY;              Name: 'CM_CHILDKEY' ),
		( Value: CM_DRAG;                  Name: 'CM_DRAG' ),
		( Value: CM_HINTSHOW;              Name: 'CM_HINTSHOW' ),
		( Value: CM_DIALOGHANDLE;          Name: 'CM_DIALOGHANDLE' ),
		( Value: CM_ISTOOLCONTROL;         Name: 'CM_ISTOOLCONTROL' ),
		( Value: CM_RECREATEWND;           Name: 'CM_RECREATEWND' ),
		( Value: CM_INVALIDATE;            Name: 'CM_INVALIDATE' ),
		( Value: CM_SYSFONTCHANGED;        Name: 'CM_SYSFONTCHANGED' ),
		( Value: CM_CONTROLCHANGE;         Name: 'CM_CONTROLCHANGE' ),
		( Value: CM_CHANGED;               Name: 'CM_CHANGED' )
	);

	ControlNotificationMap : array[0..MaxEnum_CN-1] of TIdentMapEntry =
	(
		( Value: CN_CHARTOITEM;        Name: 'CN_CHARTOITEM' ),
		( Value: CN_COMMAND;           Name: 'CN_COMMAND' ),
		( Value: CN_COMPAREITEM;       Name: 'CN_COMPAREITEM' ),
		( Value: CN_CTLCOLORBTN;       Name: 'CN_CTLCOLORBTN' ),
		( Value: CN_CTLCOLORDLG;       Name: 'CN_CTLCOLORDLG' ),
		( Value: CN_CTLCOLOREDIT;      Name: 'CN_CTLCOLOREDIT' ),
		( Value: CN_CTLCOLORLISTBOX;   Name: 'CN_CTLCOLORLISTBOX' ),
		( Value: CN_CTLCOLORMSGBOX;    Name: 'CN_CTLCOLORMSGBOX' ),
		( Value: CN_CTLCOLORSCROLLBAR; Name: 'CN_CTLCOLORSCROLLBAR' ),
		( Value: CN_CTLCOLORSTATIC;    Name: 'CN_CTLCOLORSTATIC' ),
		( Value: CN_DELETEITEM;        Name: 'CN_DELETEITEM' ),
		( Value: CN_DRAWITEM;          Name: 'CN_DRAWITEM' ),
		( Value: CN_HSCROLL;           Name: 'CN_HSCROLL' ),
		( Value: CN_MEASUREITEM;       Name: 'CN_MEASUREITEM' ),
		( Value: CN_PARENTNOTIFY;      Name: 'CN_PARENTNOTIFY' ),
		( Value: CN_VKEYTOITEM;        Name: 'CN_VKEYTOITEM' ),
		( Value: CN_VSCROLL;           Name: 'CN_VSCROLL' ),
		( Value: CN_KEYDOWN;           Name: 'CN_KEYDOWN' ),
		( Value: CN_KEYUP;             Name: 'CN_KEYUP' ),
		( Value: CN_CHAR;              Name: 'CN_CHAR' ),
		( Value: CN_SYSKEYDOWN;        Name: 'CN_SYSKEYDOWN' ),
		( Value: CN_SYSCHAR;           Name: 'CN_SYSCHAR' ),
		( Value: CN_NOTIFY;            Name: 'CN_NOTIFY' )
	);

function MessageToIdent( Message: Integer; var Ident: string;
	Map: TIdentMsgMapEntryEnum ): Boolean;
begin
	case Map of
		imeWM  : Result := IntToIdent( Message, Ident, WMMessageMap );
		imeBM  : Result := IntToIdent( Message, Ident, ButtonMessageMap );
		imeLBN : Result := IntToIdent( Message, Ident, ListBoxNotificationMap );
		imeLBM : Result := IntToIdent( Message, Ident, ListBoxMessageMap );
		imeCBN : Result := IntToIdent( Message, Ident, ComboBoxNotificationMap );
		imeCBM : Result := IntToIdent( Message, Ident, ComboBoxMessageMap );
		imeEN  : Result := IntToIdent( Message, Ident, EditNotificationMap );
		imeEM  : Result := IntToIdent( Message, Ident, EditMessageMap );
		imeSBM : Result := IntToIdent( Message, Ident, ScrollBarMessageMap );
		imeDM  : Result := IntToIdent( Message, Ident, DialogMessageMap );
		imeSBC : Result := IntToIdent( Message, Ident, StatusBarCommandsMap );
		imeSM2 : Result := IntToIdent( Message, Ident, StatusBarMessagesMap );
		imeCM  : Result := IntToIdent( Message, Ident, ComponentMessageMap );
		imeCN  : Result := IntToIdent( Message, Ident, ControlNotificationMap );
	else
		Result := False;
	end;
end;

function IdentToMessage( const Ident: string; var Message: Integer;
	Map: TIdentMsgMapEntryEnum ): Boolean;
begin
  case Map of
  	imeWM  : Result := IdentToInt( Ident, Message, WMMessageMap );
  	imeBM  : Result := IdentToInt( Ident, Message, ButtonMessageMap );
		imeLBN : Result := IdentToInt( Ident, Message, ListBoxNotificationMap );
		imeLBM : Result := IdentToInt( Ident, Message, ListBoxMessageMap );
		imeCBN : Result := IdentToInt( Ident, Message, ComboBoxNotificationMap );
		imeCBM : Result := IdentToInt( Ident, Message, ComboBoxMessageMap );
		imeEN  : Result := IdentToInt( Ident, Message, EditNotificationMap );
		imeEM  : Result := IdentToInt( Ident, Message, EditMessageMap );
		imeSBM : Result := IdentToInt( Ident, Message, ScrollBarMessageMap );
		imeDM  : Result := IdentToInt( Ident, Message, DialogMessageMap );
		imeSBC : Result := IdentToInt( Ident, Message, StatusBarCommandsMap );
		imeSM2 : Result := IdentToInt( Ident, Message, StatusBarMessagesMap );
		imeCM  : Result := IdentToInt( Ident, Message, ComponentMessageMap );
		imeCN  : Result := IdentToInt( Ident, Message, ControlNotificationMap );
  else
  	Result := False;
  end;
end;

function MessageToString( Message: Integer; Map: TIdentMsgMapEntryEnum ): string;
begin
	if not MessageToIdent( Message, Result, Map ) then
		FmtStr( Result, DEFAULT_HEX_FORMAT, [Message] );
end;

function StringToMessage( const Source: string; Map: TIdentMsgMapEntryEnum ): Integer;
begin
	if ( not IdentToMessage( Source, Result, Map ) ) then
		Result := StrToIntDef( Source, INV_MESSAGE );
end;

function MessageToStringEnums( Message: Integer; EnumTypes: TIdentMsgMapEntryEnums;
  var Translated: Boolean ): string;
var
	i: TIdentMsgMapEntryEnum;
begin
	Result := '';
	Translated := False;
	for i := Low( TIdentMsgMapEntryEnum ) to High( TIdentMsgMapEntryEnum ) do
		if ( i in EnumTypes ) then
		begin
			Translated := MessageToIdent( Message, Result, i );
			if Translated then
				Exit;
		end;
	if ( not Translated ) then
		FmtStr( Result, DEFAULT_HEX_FORMAT, [Message] );
end;

function StringToMessageEnums( const Message: string; EnumTypes: TIdentMsgMapEntryEnums;
	var Translated: Boolean ): Integer;
var
	i: TIdentMsgMapEntryEnum;
begin
	Translated := False;
	Result := INV_MESSAGE;
	for i := Low( TIdentMsgMapEntryEnum ) to High( TIdentMsgMapEntryEnum ) do
		if (  i in EnumTypes  ) then
		begin
			Translated := IdentToMessage( Message, Result, i );
			if Translated then
				Exit;
		end;
	if ( not Translated ) then
		Result := StrToIntDef( Message, INV_MESSAGE );
end;

end.
