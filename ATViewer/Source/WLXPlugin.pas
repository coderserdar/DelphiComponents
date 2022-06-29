// Lister API definitions.
// This unit is written by Christian Ghisler, it's from Total Commander
// Lister API Guide, which can be found at http://ghisler.com.
// Version: 1.8.

unit WLXPlugin;

interface

uses
  Windows;

const
  lc_copy=1;
  lc_newparams=2;
  lc_selectall=3;
  lc_setpercent=4;
  lcp_wraptext=1;
  lcp_fittowindow=2;
  lcp_ansi=4;
  lcp_ascii=8;
  lcp_variable=12;
  lcp_forceshow=16;
  lcp_fitlargeronly=32;
  lcp_center=64;
  lcs_findfirst=1;
  lcs_matchcase=2;
  lcs_wholewords=4;
  lcs_backwards=8;
  itm_percent=$FFFE;
  itm_fontstyle=$FFFD;
  itm_wrap=$FFFC;
  itm_fit=$FFFB;
  itm_next=$FFFA;
  itm_center=$FFF9;
  LISTPLUGIN_OK=0;
  LISTPLUGIN_ERROR=1;

type
  tListDefaultParamStruct=record
    size,
    PluginInterfaceVersionLow,
    PluginInterfaceVersionHi:longint;
    DefaultIniName:array[0..MAX_PATH-1] of AnsiChar;
  end;

  pListDefaultParamStruct=^tListDefaultParamStruct;

{ Function prototypes: Functions need to be defined exactly like this!}
{
function ListLoad(ParentWin:thandle;FileToLoad:PAnsiChar;ShowFlags:integer):thandle; stdcall;
function ListLoadNext(ParentWin,PluginWin:thandle;FileToLoad:PAnsiChar;ShowFlags:integer):integer; stdcall;
procedure ListCloseWindow(ListWin:thandle); stdcall;
procedure ListGetDetectString(DetectString:PAnsiChar;maxlen:integer); stdcall;
function ListSearchText(ListWin:thandle;SearchString:PAnsiChar;
                        SearchParameter:integer):integer; stdcall;
function ListSearchDialog(ListWin:thandle;FindNext:integer):integer; stdcall;
function ListSendCommand(ListWin:thandle;Command,Parameter:integer):integer; stdcall;
function ListPrint(ListWin:thandle;FileToPrint,DefPrinter:PAnsiChar;
                   PrintFlags:integer;var Margins:trect):integer; stdcall;
function ListNotificationReceived(ListWin:thandle;Message,wParam,lParam:integer):integer; stdcall;
procedure ListSetDefaultParams(dps:pListDefaultParamStruct); stdcall;
function ListGetPreviewBitmap(FileToLoad:PAnsiChar;width,height:integer;
    contentbuf:PAnsiChar;contentbuflen:integer):hbitmap; stdcall;
}

implementation

end.
