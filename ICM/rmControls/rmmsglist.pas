{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmMsgList
Purpose  : To provide a simple method of identifying window messages or even
           control messages.
Date     : 11-13-1999
Author   : Ryan J. Mills
Version  : 1.90
Notes    : This code was originally given to me by Daniel Parnell, and I've
           continued to add to it.
================================================================================}

unit rmMSGList;

interface

{$I CompilerDefines.INC}

uses Messages;

type
   TrmMsgEvent = procedure(Message:TMessage) of object;

function GetMessageName(Msg: Integer): string;

implementation

uses
  SysUtils, Classes;

var
  MsgList: TStringList;

function GetMessageName (Msg: Integer): string;
var
  N: Integer;
begin
  N := MsgList.IndexOfObject (TObject(Msg));
  if N >= 0 then
    Result := MsgList.Strings [N]
  else if (Msg >= wm_User) and
      (Msg <= $7FFF) then
    Result := Format (
      'wm_User message (%d)', [Msg])
  else
    Result := Format (
      'Undocumented (%d)', [Msg]);
end;

initialization
  MsgList := TStringList.Create;
  MsgList.AddObject ('wm_Null',              TObject($0000));
  MsgList.AddObject ('wm_Create',            TObject($0001));
  MsgList.AddObject ('wm_Destroy',           TObject($0002));
  MsgList.AddObject ('wm_Move',              TObject($0003));
  MsgList.AddObject ('wm_Size',              TObject($0005));
  MsgList.AddObject ('wm_Activate',          TObject($0006));
  MsgList.AddObject ('wm_SetFocus',          TObject($0007));
  MsgList.AddObject ('wm_KillFocus',         TObject($0008));
  MsgList.AddObject ('wm_Enable',            TObject($000A));
  MsgList.AddObject ('wm_SetRedraw',         TObject($000B));
  MsgList.AddObject ('wm_SetText',           TObject($000C));
  MsgList.AddObject ('wm_GetText',           TObject($000D));
  MsgList.AddObject ('wm_GetTextLength',     TObject($000E));
  MsgList.AddObject ('wm_Paint',             TObject($000F));
  MsgList.AddObject ('wm_Close',             TObject($0010));
  MsgList.AddObject ('wm_QueryEndSession',   TObject($0011));
  MsgList.AddObject ('wm_Quit',              TObject($0012));
  MsgList.AddObject ('wm_QueryOpen',         TObject($0013));
  MsgList.AddObject ('wm_EraseBkGnd',        TObject($0014));
  MsgList.AddObject ('wm_SysColorChange',    TObject($0015));
  MsgList.AddObject ('wm_EndSession',        TObject($0016));
  MsgList.AddObject ('wm_SystemError',       TObject($0017));
  MsgList.AddObject ('wm_ShowWindow',        TObject($0018));
  MsgList.AddObject ('wm_CtlColor',          TObject($0019));
  MsgList.AddObject ('wm_WinIniChange',      TObject($001A));
  MsgList.AddObject ('wm_DevModeChange',     TObject($001B));
  MsgList.AddObject ('wm_ActivateApp',       TObject($001C));
  MsgList.AddObject ('wm_FontChange',        TObject($001D));
  MsgList.AddObject ('wm_TimeChange',        TObject($001E));
  MsgList.AddObject ('wm_CancelMode',        TObject($001F));
  MsgList.AddObject ('wm_SetCursor',         TObject($0020));
  MsgList.AddObject ('wm_MouseActivate',     TObject($0021));
  MsgList.AddObject ('wm_ChildActivate',     TObject($0022));
  MsgList.AddObject ('wm_QueueSync',         TObject($0023));
  MsgList.AddObject ('wm_GetMinMaxInfo',     TObject($0024));
  MsgList.AddObject ('wm_PaintIcon',         TObject($0026));
  MsgList.AddObject ('wm_IconEraseBkGnd',    TObject($0027));
  MsgList.AddObject ('wm_NextDlgCtl',        TObject($0028));
  MsgList.AddObject ('wm_SpoolerStatus',     TObject($002A));
  MsgList.AddObject ('wm_DrawItem',          TObject($002B));
  MsgList.AddObject ('wm_MeasureItem',       TObject($002C));
  MsgList.AddObject ('wm_DeleteItem',        TObject($002D));
  MsgList.AddObject ('wm_VKeyToItem',        TObject($002E));
  MsgList.AddObject ('wm_CharToItem',        TObject($002F));
  MsgList.AddObject ('wm_SetFont',           TObject($0030));
  MsgList.AddObject ('wm_GetFont',           TObject($0031));
  MsgList.AddObject ('wm_SetHotKey',         TObject($0032));
  MsgList.AddObject ('wm_GetHotKey',         TObject($0033));
  MsgList.AddObject ('wm_QueryDragIcon',     TObject($0037));
  MsgList.AddObject ('wm_CompareItem',       TObject($0039));
  MsgList.AddObject ('wm_Compacting',        TObject($0041));
  MsgList.AddObject ('wm_CommNotify',        TObject($0044));
  MsgList.AddObject ('wm_WindowPosChanging', TObject($0046));
  MsgList.AddObject ('wm_WindowPosChanged',  TObject($0047));
  MsgList.AddObject ('wm_Power',             TObject($0048));
  MsgList.AddObject ('wm_CopyData',          TObject($004A));
  MsgList.AddObject ('wm_CancelJournal',     TObject($004B));
  MsgList.AddObject ('wm_Notify',            TObject($004E));
  MsgList.AddObject ('wm_InputLangChangeRequest', TObject($0050));
  MsgList.AddObject ('wm_InputLangChange',        TObject($0051));
  MsgList.AddObject ('wm_tCard',                  TObject($0052));
  MsgList.AddObject ('wm_Help',                   TObject($0053));
  MsgList.AddObject ('wm_UserChanged',            TObject($0054));
  MsgList.AddObject ('wm_NotifyFormat',           TObject($0055));
  MsgList.AddObject ('wm_ContextMenu',            TObject($007B));
  MsgList.AddObject ('wm_StyleChanging',          TObject($007C));
  MsgList.AddObject ('wm_StyleChanged',           TObject($007D));
  MsgList.AddObject ('wm_DisplayChange',          TObject($007E));
  MsgList.AddObject ('wm_GetIcon',                TObject($007F));
  MsgList.AddObject ('wm_SetIcon',                TObject($0080));
  MsgList.AddObject ('wm_NCCreate',               TObject($0081));
  MsgList.AddObject ('wm_NCDestroy',              TObject($0082));
  MsgList.AddObject ('wm_NCCalcSize',             TObject($0083));
  MsgList.AddObject ('wm_NCHitTest',              TObject($0084));
  MsgList.AddObject ('wm_NCPaint',                TObject($0085));
  MsgList.AddObject ('wm_NCActivate',             TObject($0086));
  MsgList.AddObject ('wm_GetDlgCode',             TObject($0087));
  MsgList.AddObject ('wm_NCMouseMove',       TObject($00A0));
  MsgList.AddObject ('wm_NCLButtonDown',     TObject($00A1));
  MsgList.AddObject ('wm_NCLButtonUp',       TObject($00A2));
  MsgList.AddObject ('wm_NCLButtonDblClk',   TObject($00A3));
  MsgList.AddObject ('wm_NCRButtonDown',     TObject($00A4));
  MsgList.AddObject ('wm_NCRButtonUp',       TObject($00A5));
  MsgList.AddObject ('wm_NCRButtonDblClk',   TObject($00A6));
  MsgList.AddObject ('wm_NCMButtonDown',     TObject($00A7));
  MsgList.AddObject ('wm_NCMButtonUp',       TObject($00A8));
  MsgList.AddObject ('wm_NCMButtonDblClk',   TObject($00A9));
  MsgList.AddObject ('wm_KeyDown',           TObject($0100));
  MsgList.AddObject ('wm_KeyUp',             TObject($0101));
  MsgList.AddObject ('wm_Char',              TObject($0102));
  MsgList.AddObject ('wm_DeadChar',          TObject($0103));
  MsgList.AddObject ('wm_SysKeyDown',        TObject($0104));
  MsgList.AddObject ('wm_SysKeyUp',          TObject($0105));
  MsgList.AddObject ('wm_SysChar',           TObject($0106));
  MsgList.AddObject ('wm_SysDeadChar',       TObject($0107));
  MsgList.AddObject ('wm_KeyLast',           TObject($0108));
  MsgList.AddObject ('wm_IME_StartComposition', TObject($010D));
  MsgList.AddObject ('wm_IME_EndComposition',   TObject($010E));
  MsgList.AddObject ('wm_IME_Composition/KeyLast', TObject($010F));
  MsgList.AddObject ('wm_InitDialog',        TObject($0110));
  MsgList.AddObject ('wm_Command',           TObject($0111));
  MsgList.AddObject ('wm_SysCommand',        TObject($0112));
  MsgList.AddObject ('wm_Timer',             TObject($0113));
  MsgList.AddObject ('wm_HScroll',           TObject($0114));
  MsgList.AddObject ('wm_VScroll',           TObject($0115));
  MsgList.AddObject ('wm_InitMenu',          TObject($0116));
  MsgList.AddObject ('wm_InitMenuPopup',     TObject($0117));
  MsgList.AddObject ('wm_MenuSelect',        TObject($011F));
  MsgList.AddObject ('wm_MenuChar',          TObject($0120));
  MsgList.AddObject ('wm_EnterIdle',         TObject($0121));
  MsgList.AddObject ('wm_CtlColorMsgbox',    TObject($0132));
  MsgList.AddObject ('wm_CtlColorEdit',      TObject($0133));
  MsgList.AddObject ('wm_CtlColorListbox',   TObject($0134));
  MsgList.AddObject ('wm_CtlColorBtn',       TObject($0135));
  MsgList.AddObject ('wm_CtlColorDlg',       TObject($0136));
  MsgList.AddObject ('wm_CtlColorScrollbar', TObject($0137));
  MsgList.AddObject ('wm_CtlColorStatic',    TObject($0138));
  MsgList.AddObject ('wm_MouseMove',         TObject($0200));
  MsgList.AddObject ('wm_LButtonDown',       TObject($0201));
  MsgList.AddObject ('wm_LButtonUp',         TObject($0202));
  MsgList.AddObject ('wm_LButtonDblClk',     TObject($0203));
  MsgList.AddObject ('wm_RButtonDown',       TObject($0204));
  MsgList.AddObject ('wm_RButtonUp',         TObject($0205));
  MsgList.AddObject ('wm_RButtonDblClk',     TObject($0206));
  MsgList.AddObject ('wm_MButtonDown',       TObject($0207));
  MsgList.AddObject ('wm_MButtonUp',         TObject($0208));
  MsgList.AddObject ('wm_MButtonDblClk',     TObject($0209));
  MsgList.AddObject ('wm_MouseLast/Wheel',   TObject($020A));
  MsgList.AddObject ('wm_ParentNotify',      TObject($0210));
  MsgList.AddObject ('wm_EnterMenuLoop',     TObject($0211));
  MsgList.AddObject ('wm_ExitMenuLoop',      TObject($0212));
  MsgList.AddObject ('wm_NextMenu',          TObject($0213));
  MsgList.AddObject ('wm_Sizing',            TObject($0214));
  MsgList.AddObject ('wm_CaptureChanged',    TObject($0215));
  MsgList.AddObject ('wm_Moving',            TObject($0216));
  MsgList.AddObject ('wm_PowerBroadcast',    TObject($0218));
  MsgList.AddObject ('wm_DeviceChange',      TObject($0219));
  MsgList.AddObject ('wm_MDICreate',         TObject($0220));
  MsgList.AddObject ('wm_MDIDestroy',        TObject($0221));
  MsgList.AddObject ('wm_MDIActivate',       TObject($0222));
  MsgList.AddObject ('wm_MDIRestore',        TObject($0223));
  MsgList.AddObject ('wm_MDINext',           TObject($0224));
  MsgList.AddObject ('wm_MDIMaximize',       TObject($0225));
  MsgList.AddObject ('wm_MDITile',           TObject($0226));
  MsgList.AddObject ('wm_MDICascade',        TObject($0227));
  MsgList.AddObject ('wm_MDIIconArrange',    TObject($0228));
  MsgList.AddObject ('wm_MDIGetActive',      TObject($0229));
  MsgList.AddObject ('wm_MDISetMenu',        TObject($0230));
  MsgList.AddObject ('wm_EnterSizeMove',    TObject($0231));
  MsgList.AddObject ('wm_ExitSizeMove',     TObject($0232));
  MsgList.AddObject ('wm_DropFiles',         TObject($0233));
  MsgList.AddObject ('wm_MDIRefreshMenu',    TObject($0234));
  MsgList.AddObject ('wm_IME_Setcontext',              TObject($0281));
  MsgList.AddObject ('wm_IME_Notify',                  TObject($0282));
  MsgList.AddObject ('wm_IME_Control',                 TObject($0283));
  MsgList.AddObject ('wm_IME_Compositionfull',         TObject($0284));
  MsgList.AddObject ('wm_IME_Select',                  TObject($0285));
  MsgList.AddObject ('wm_IME_Char',                    TObject($0286));
  MsgList.AddObject ('wm_IME_Keydown',                 TObject($0290));
  MsgList.AddObject ('wm_IME_Keyup',                   TObject($0291));
  MsgList.AddObject ('wm_MouseHover',       TObject($02a1));
  MsgList.AddObject ('wm_MouseLeave',       TObject($02a3));
  MsgList.AddObject ('wm_Cut',               TObject($0300));
  MsgList.AddObject ('wm_Copy',              TObject($0301));
  MsgList.AddObject ('wm_Paste',             TObject($0302));
  MsgList.AddObject ('wm_Clear',             TObject($0303));
  MsgList.AddObject ('wm_Undo',              TObject($0304));
  MsgList.AddObject ('wm_RenderFormat',      TObject($0305));
  MsgList.AddObject ('wm_RenderAllFormats',  TObject($0306));
  MsgList.AddObject ('wm_DestroyClipboard',  TObject($0307));
  MsgList.AddObject ('wm_DrawClipboard',     TObject($0308));
  MsgList.AddObject ('wm_PaintClipboard',    TObject($0309));
  MsgList.AddObject ('wm_VScrollClipboard',  TObject($030A));
  MsgList.AddObject ('wm_SizeClipboard',     TObject($030B));
  MsgList.AddObject ('wm_AskCBFormatName',   TObject($030C));
  MsgList.AddObject ('wm_ChangeCBChain',     TObject($030D));
  MsgList.AddObject ('wm_HScrollClipboard',  TObject($030E));
  MsgList.AddObject ('wm_QueryNewPalette',   TObject($030F));
  MsgList.AddObject ('wm_PaletteIsChanging', TObject($0310));
  MsgList.AddObject ('wm_PaletteChanged',    TObject($0311));
  MsgList.AddObject ('wm_HotKey',           TObject($0312));
  MsgList.AddObject ('wm_Print',            TObject($0317));
  MsgList.AddObject ('wm_PrintClient',      TObject($0318));

  MsgList.AddObject ('CM_ACTIVATE',                TObject($B000 + 0));
  MsgList.AddObject ('CM_DEACTIVATE',              TObject($B000 + 1));
  MsgList.AddObject ('CM_GOTFOCUS',                TObject($B000 + 2));
  MsgList.AddObject ('CM_LOSTFOCUS',               TObject($B000 + 3));
  MsgList.AddObject ('CM_CANCELMODE',              TObject($B000 + 4));
  MsgList.AddObject ('CM_DIALOGKEY',               TObject($B000 + 5));
  MsgList.AddObject ('CM_DIALOGCHAR',              TObject($B000 + 6));
  MsgList.AddObject ('CM_FOCUSCHANGED',            TObject($B000 + 7));
  MsgList.AddObject ('CM_PARENTFONTCHANGED',       TObject($B000 + 8));
  MsgList.AddObject ('CM_PARENTCOLORCHANGED',      TObject($B000 + 9));
  MsgList.AddObject ('CM_HITTEST',                 TObject($B000 + 10));
  MsgList.AddObject ('CM_VISIBLECHANGED',          TObject($B000 + 11));
  MsgList.AddObject ('CM_ENABLEDCHANGED',          TObject($B000 + 12));
  MsgList.AddObject ('CM_COLORCHANGED',            TObject($B000 + 13));
  MsgList.AddObject ('CM_FONTCHANGED',             TObject($B000 + 14));
  MsgList.AddObject ('CM_CURSORCHANGED',           TObject($B000 + 15));
  MsgList.AddObject ('CM_CTL3DCHANGED',            TObject($B000 + 16));
  MsgList.AddObject ('CM_PARENTCTL3DCHANGED',      TObject($B000 + 17));
  MsgList.AddObject ('CM_TEXTCHANGED',             TObject($B000 + 18));
  MsgList.AddObject ('CM_MOUSEENTER',              TObject($B000 + 19));
  MsgList.AddObject ('CM_MOUSELEAVE',              TObject($B000 + 20));
  MsgList.AddObject ('CM_MENUCHANGED',             TObject($B000 + 21));
  MsgList.AddObject ('CM_APPKEYDOWN',              TObject($B000 + 22));
  MsgList.AddObject ('CM_APPSYSCOMMAND',           TObject($B000 + 23));
  MsgList.AddObject ('CM_BUTTONPRESSED',           TObject($B000 + 24));
  MsgList.AddObject ('CM_SHOWINGCHANGED',          TObject($B000 + 25));
  MsgList.AddObject ('CM_ENTER',                   TObject($B000 + 26));
  MsgList.AddObject ('CM_EXIT',                    TObject($B000 + 27));
  MsgList.AddObject ('CM_DESIGNHITTEST',           TObject($B000 + 28));
  MsgList.AddObject ('CM_ICONCHANGED',             TObject($B000 + 29));
  MsgList.AddObject ('CM_WANTSPECIALKEY',          TObject($B000 + 30));
  MsgList.AddObject ('CM_INVOKEHELP',              TObject($B000 + 31));
  MsgList.AddObject ('CM_WINDOWHOOK',              TObject($B000 + 32));
  MsgList.AddObject ('CM_RELEASE',                 TObject($B000 + 33));
  MsgList.AddObject ('CM_SHOWHINTCHANGED',         TObject($B000 + 34));
  MsgList.AddObject ('CM_PARENTSHOWHINTCHANGED',   TObject($B000 + 35));
  MsgList.AddObject ('CM_SYSCOLORCHANGE',          TObject($B000 + 36));
  MsgList.AddObject ('CM_WININICHANGE',            TObject($B000 + 37));
  MsgList.AddObject ('CM_FONTCHANGE',              TObject($B000 + 38));
  MsgList.AddObject ('CM_TIMECHANGE',              TObject($B000 + 39));
  MsgList.AddObject ('CM_TABSTOPCHANGED',          TObject($B000 + 40));
  MsgList.AddObject ('CM_UIACTIVATE',              TObject($B000 + 41));
  MsgList.AddObject ('CM_UIDEACTIVATE',            TObject($B000 + 42));
  MsgList.AddObject ('CM_DOCWINDOWACTIVATE',       TObject($B000 + 43));
  MsgList.AddObject ('CM_CONTROLLISTCHANGE',       TObject($B000 + 44));
  MsgList.AddObject ('CM_GETDATALINK',             TObject($B000 + 45));
  MsgList.AddObject ('CM_CHILDKEY',                TObject($B000 + 46));
  MsgList.AddObject ('CM_DRAG',                    TObject($B000 + 47));
  MsgList.AddObject ('CM_HINTSHOW',                TObject($B000 + 48));
  MsgList.AddObject ('CM_DIALOGHANDLE',            TObject($B000 + 49));
  MsgList.AddObject ('CM_ISTOOLCONTROL',           TObject($B000 + 50));
  MsgList.AddObject ('CM_RECREATEWND',             TObject($B000 + 51));
  MsgList.AddObject ('CM_INVALIDATE',              TObject($B000 + 52));
  MsgList.AddObject ('CM_SYSFONTCHANGED',          TObject($B000 + 53));
  MsgList.AddObject ('CM_CONTROLCHANGE',           TObject($B000 + 54));
  MsgList.AddObject ('CM_CHANGED',                 TObject($B000 + 55));
  MsgList.AddObject('CM_DOCKCLIENT', TObject($B000 + 56));
  MsgList.AddObject('CM_UNDOCKCLIENT', TObject($B000 + 57));
  MsgList.AddObject('CM_FLOAT', TObject($B000 + 58));
  MsgList.AddObject('CM_BORDERCHANGED', TObject($B000 + 59));
  MsgList.AddObject('CM_BIDIMODECHANGED', TObject($B000 + 60));
  MsgList.AddObject('CM_PARENTBIDIMODECHANGED', TObject($B000 + 61));
  MsgList.AddObject('CM_ALLCHILDRENFLIPPED', TObject($B000 + 62));
  MsgList.AddObject('CM_ACTIONUPDATE', TObject($B000 + 63));
  MsgList.AddObject('CM_ACTIONEXECUTE', TObject($B000 + 64));
  MsgList.AddObject('CM_HINTSHOWPAUSE', TObject($B000 + 65));
  MsgList.AddObject('CM_DOCKNOTIFICATION', TObject($B000 + 66));
  MsgList.AddObject('CM_MOUSEWHEEL', TObject($B000 + 67));


  MsgList.AddObject('CN_CHARTOITEM', TObject($BC00 + WM_CHARTOITEM));
  MsgList.AddObject('CN_COMMAND', TObject($BC00 + WM_COMMAND));
  MsgList.AddObject('CN_COMPAREITEM', TObject($BC00 + WM_COMPAREITEM));
  MsgList.AddObject('CN_CTLCOLORBTN', TObject($BC00 + WM_CTLCOLORBTN));
  MsgList.AddObject('CN_CTLCOLORDLG', TObject($BC00 + WM_CTLCOLORDLG));
  MsgList.AddObject('CN_CTLCOLOREDIT', TObject($BC00 + WM_CTLCOLOREDIT));
  MsgList.AddObject('CN_CTLCOLORLISTBOX', TObject($BC00 + WM_CTLCOLORLISTBOX));
  MsgList.AddObject('CN_CTLCOLORMSGBOX', TObject($BC00 + WM_CTLCOLORMSGBOX));
  MsgList.AddObject('CN_CTLCOLORSCROLLBAR', TObject($BC00 + WM_CTLCOLORSCROLLBAR));
  MsgList.AddObject('CN_CTLCOLORSTATIC', TObject($BC00 + WM_CTLCOLORSTATIC));
  MsgList.AddObject('CN_DELETEITEM', TObject($BC00 + WM_DELETEITEM));
  MsgList.AddObject('CN_DRAWITEM', TObject($BC00 + WM_DRAWITEM));
  MsgList.AddObject('CN_HSCROLL', TObject($BC00 + WM_HSCROLL));
  MsgList.AddObject('CN_MEASUREITEM', TObject($BC00 + WM_MEASUREITEM));
  MsgList.AddObject('CN_PARENTNOTIFY', TObject($BC00 + WM_PARENTNOTIFY));
  MsgList.AddObject('CN_VKEYTOITEM', TObject($BC00 + WM_VKEYTOITEM));
  MsgList.AddObject('CN_VSCROLL', TObject($BC00 + WM_VSCROLL));
  MsgList.AddObject('CN_KEYDOWN', TObject($BC00 + WM_KEYDOWN));
  MsgList.AddObject('CN_KEYUP', TObject($BC00 + WM_KEYUP));
  MsgList.AddObject('CN_CHAR', TObject($BC00 + WM_CHAR));
  MsgList.AddObject('CN_SYSKEYDOWN', TObject($BC00 + WM_SYSKEYDOWN));
  MsgList.AddObject ('CN_SYSKEYUP', TObject($BC00 + WM_SYSKEYUP));
  MsgList.AddObject('CN_SYSCHAR', TObject($BC00 + WM_SYSCHAR));
  MsgList.AddObject('CN_NOTIFY', TObject($BC00 + WM_NOTIFY));

  MsgList.AddObject('TVM_INSERTITEMA', TObject($1100 + 0));
  MsgList.AddObject('TVM_INSERTITEMW', TObject($1100 + 50));
  MsgList.AddObject('TVM_DELETEITEM', TObject($1100 + 1));
  MsgList.AddObject('TVM_EXPAND', TObject($1100 + 2));
  MsgList.AddObject('TVM_GETITEMRECT', TObject($1100 + 4));
  MsgList.AddObject('TVM_GETCOUNT', TObject($1100 + 5));
  MsgList.AddObject('TVM_GETINDENT', TObject($1100 + 6));
  MsgList.AddObject('TVM_SETINDENT', TObject($1100 + 7));
  MsgList.AddObject('TVM_GETIMAGELIST', TObject($1100 + 8));
  MsgList.AddObject('TVM_SETIMAGELIST', TObject($1100 + 9));
  MsgList.AddObject('TVM_GETNEXTITEM', TObject($1100 + 10));
  MsgList.AddObject('TVM_SELECTITEM', TObject($1100 + 11));
  MsgList.AddObject('TVM_GETITEMA', TObject($1100 + 12));
  MsgList.AddObject('TVM_GETITEMW', TObject($1100 + 62));
  MsgList.AddObject('TVM_SETITEMA', TObject($1100 + 13));
  MsgList.AddObject('TVM_SETITEMW', TObject($1100 + 63));
  MsgList.AddObject('TVM_EDITLABELA', TObject($1100 + 14));
  MsgList.AddObject('TVM_EDITLABELW', TObject($1100 + 65));
  MsgList.AddObject('TVM_GETEDITCONTROL', TObject($1100 + 15));
  MsgList.AddObject('TVM_GETVISIBLECOUNT', TObject($1100 + 16));
  MsgList.AddObject('TVM_HITTEST', TObject($1100 + 17));
  MsgList.AddObject('TVM_CREATEDRAGIMAGE', TObject($1100 + 18));
  MsgList.AddObject('TVM_SORTCHILDREN', TObject($1100 + 19));
  MsgList.AddObject('TVM_ENSUREVISIBLE', TObject($1100 + 20));
  MsgList.AddObject('TVM_SORTCHILDRENCB', TObject($1100 + 21));
  MsgList.AddObject('TVM_ENDEDITLABELNOW', TObject($1100 + 22));
  MsgList.AddObject('TVM_GETISEARCHSTRINGA', TObject($1100 + 23));
  MsgList.AddObject('TVM_GETISEARCHSTRINGW', TObject($1100 + 64));
  MsgList.AddObject('TVM_SETTOOLTIPS', TObject($1100 + 24));
  MsgList.AddObject('TVM_GETTOOLTIPS', TObject($1100 + 25));
  MsgList.AddObject('TVM_SETINSERTMARK', TObject($1100 + 26));
  MsgList.AddObject('TVM_SETITEMHEIGHT', TObject($1100 + 27));
  MsgList.AddObject('TVM_GETITEMHEIGHT', TObject($1100 + 28));
  MsgList.AddObject('TVM_SETBKCOLOR', TObject($1100 + 29));
  MsgList.AddObject('TVM_SETTEXTCOLOR', TObject($1100 + 30));
  MsgList.AddObject('TVM_GETBKCOLOR', TObject($1100 + 31));
  MsgList.AddObject('TVM_GETTEXTCOLOR', TObject($1100 + 32));
  MsgList.AddObject('TVM_SETSCROLLTIME', TObject($1100 + 33));
  MsgList.AddObject('TVM_GETSCROLLTIME', TObject($1100 + 34));
  MsgList.AddObject('Unknown (TVM-$1123)', TObject($1100 + 35));
  MsgList.AddObject('Unknown (TVM-$1124)', TObject($1100 + 36));
  MsgList.AddObject('TVM_SETINSERTMARKCOLOR', TObject($1100 + 37));
  MsgList.AddObject('TVM_GETINSERTMARKCOLOR', TObject($1100 + 38));

end.
