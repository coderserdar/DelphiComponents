//---------------------------------------------------------------- 
// Most of these functions are taken from "Mini Webbrowser Demo",
// which is available on http://torry.net.
// Some functions are added later by AT as stated in the comments.
// The original WBFuncs.pas unit caption is below:
//---------------------------------------------------------------- 

(**************************************************************)
(*                                                            *)
(*  TWebbrowser functions by toms                             *)
(*  Version 1.9                                               *)
(*  E-Mail: tom@swissdelphicenter.ch                          *)
(*                                                            *)
(*  Contributors: www.swissdelphicenter.ch                    *)
(*                                                            *)
(*                                                            *)
(**************************************************************)

{$I ATViewerOptions.inc} //ATViewer options
{$I Compilers.inc}       //Compiler defines

unit ATxWBProc;

interface

uses
  {$ifdef IE4X} WebBrowser4_TLB {$else} SHDocVw {$endif};

//From WBFuncs.pas:

procedure WB_Wait(WB: TWebbrowser);
procedure WB_SetFocus(WB: TWebbrowser);
procedure WB_Set3DBorderStyle(WB: TWebBrowser; bValue: Boolean);
procedure WB_Copy(WB: TWebbrowser);
procedure WB_SelectAll(WB: TWebbrowser);
procedure WB_ShowPrintDialog(WB: TWebbrowser);
procedure WB_ShowPrintPreview(WB: TWebbrowser);
procedure WB_ShowPageSetup(WB: TWebbrowser);
procedure WB_ShowFindDialog(WB: TWebbrowser);

//Added by AT:

procedure WB_NavigateBlank(WB: TWebbrowser);
procedure WB_NavigateFilename(WB: TWebbrowser; const FileName: WideString; DoWait: Boolean);

procedure WB_SelectNone(WB: TWebbrowser);
function WB_GetScrollTop(WB: TWebbrowser): Integer;
procedure WB_SetScrollTop(WB: TWebbrowser; Value: Integer);
function WB_GetScrollHeight(WB: TWebbrowser): Integer;
procedure WB_IncreaseFont(WB: TWebbrowser; Increment: Boolean);

{$ifdef OFFLINE}
procedure WB_SetGlobalOffline(AValue: Boolean);
function WB_GetGlobalOffline: Boolean;
{$endif}

var
  WB_MessagesEnabled: Boolean = False; //Can be set to True for debugging purposes


implementation

uses
  Windows, SysUtils, {$ifdef COMPILER_6_UP} Variants, {$endif}
  ActiveX, MSHTML, Forms, OleCtrls;

type
  TWBFontSize = 0..4;

//----------------------------------------------------------------------------
procedure MsgError(const Msg: WideString);
begin
  if WB_MessagesEnabled then
    MessageBoxW(0, PWideChar(Msg), 'Webbrowser Error', MB_OK or MB_ICONERROR or MB_TASKMODAL);
end;

//----------------------------------------------------------------------------
function InvokeCMD(WB: TWebbrowser; nCmdID: DWORD): Boolean; overload; forward;
function InvokeCMD(WB: TWebbrowser; InvokeIE: Boolean; Value1, Value2: Integer; var vaIn, vaOut: OleVariant): Boolean; overload; forward;

const
  CGID_WebBrowser: TGUID = '{ED016940-BD5B-11cf-BA4E-00C04FD70816}';
  HTMLID_FIND = 1;

function InvokeCMD(WB: TWebbrowser; nCmdID: DWORD): Boolean;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCMD(WB, True, nCmdID, 0, vaIn, vaOut);
end;

function InvokeCMD(WB: TWebbrowser; InvokeIE: Boolean; Value1, Value2: Integer; var vaIn, vaOut: OleVariant): Boolean;
var
  CmdTarget: IOleCommandTarget;
  PtrGUID: PGUID;
begin
  Result:= False;
  New(PtrGUID);
  if InvokeIE then
    PtrGUID^ := CGID_WebBrowser
  else
    PtrGuid := PGUID(nil);
  if WB.ControlInterface.Document <> nil then
  try
    WB.ControlInterface.Document.QueryInterface(IOleCommandTarget, CmdTarget);
    if CmdTarget <> nil then
    try
      CmdTarget.Exec(PtrGuid, Value1, Value2, vaIn, vaOut);
      Result:= True;
    finally
      CmdTarget._Release;
    end;
  except
  end;
  Dispose(PtrGUID);
end;

//----------------------------------------------------------------------------
procedure WB_Wait(WB: TWebbrowser);
begin
  while (WB.ReadyState <> READYSTATE_COMPLETE)
    and not (Application.Terminated) do
  begin
    Application.ProcessMessages;
    Sleep(0);
  end;
end;

//----------------------------------------------------------------------------
function WB_DocumentLoaded(WB: TWebbrowser): Boolean;
var
  Doc: IHTMLDocument2;
begin
  Result := False;
  if Assigned(WB) then
    if WB.ControlInterface.Document <> nil then
    begin
      WB.ControlInterface.Document.QueryInterface(IHTMLDocument2, Doc);
      Result := Assigned(Doc);
    end;
end;

//----------------------------------------------------------------------------
procedure WB_SetFocus(WB: TWebbrowser);
begin
  try
    if WB_DocumentLoaded(WB) then
      (WB.ControlInterface.Document as IHTMLDocument2).ParentWindow.Focus;
  except
    MsgError('Cannot focus the WebBrowser control.');
  end;
end;

//----------------------------------------------------------------------------
procedure WB_Set3DBorderStyle(WB: TWebBrowser; bValue: Boolean);
{
  bValue: True: Show a 3D border style
          False: Show no border
}
var
  Document: IHTMLDocument2;
  Element: IHTMLElement;
  StrBorderStyle: AnsiString;
begin
  if Assigned(WB) then
    if WB_DocumentLoaded(WB) then
      try
        Document := WB.ControlInterface.Document as IHTMLDocument2;
        if Assigned(Document) then
        begin
          Element := Document.Body;
          if Element <> nil then
          begin
            case bValue of
              False: StrBorderStyle := 'none';
              True: StrBorderStyle := '';
            end;
            Element.Style.BorderStyle := StrBorderStyle;
          end;
        end;
      except
        MsgError('Cannot change border style for WebBrowser control.');
      end;
end;

//----------------------------------------------------------------------------
procedure WB_Copy(WB: TWebbrowser);
var
  vaIn, vaOut: Olevariant;
begin
  InvokeCmd(WB, FALSE, OLECMDID_COPY, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut);
end;

//----------------------------------------------------------------------------
procedure WB_SelectAll(WB: TWebbrowser);
var
  vaIn, vaOut: Olevariant;
begin
  InvokeCmd(WB, FALSE, OLECMDID_SELECTALL, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut);
end;

//----------------------------------------------------------------------------
procedure WB_SelectNone(WB: TWebbrowser);
var
  vaIn, vaOut: Olevariant;
begin
  InvokeCmd(WB, FALSE, OLECMDID_CLEARSELECTION, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut);
end;

//----------------------------------------------------------------------------
procedure WB_ShowPrintDialog(WB: TWebbrowser);
var
  OleCommandTarget: IOleCommandTarget;
  Command: TOleCmd;
  Success: HResult;
  Param: OleVariant;
begin
  if WB_DocumentLoaded(WB) then
  begin
    WB.ControlInterface.Document.QueryInterface(IOleCommandTarget, OleCommandTarget);
    Command.cmdID := OLECMDID_PRINT;
    if OleCommandTarget.QueryStatus(nil, 1, @Command, nil) <> S_OK then
    begin
      //ShowMessage('Nothing to print');
      Exit;
    end;
    if (Command.cmdf and OLECMDF_ENABLED) <> 0 then
    begin
      Success := OleCommandTarget.Exec(nil, OLECMDID_PRINT, OLECMDEXECOPT_PROMPTUSER, Param, Param);
      if Success = S_OK then begin end;
      { //AT
      case Success of
        S_OK: ;
        OLECMDERR_E_CANCELED: ShowMessage('Canceled by user');
      else
        ShowMessage('Error while printing');
      end;
      }
    end
  end;
end;

//----------------------------------------------------------------------------
procedure WB_ShowPrintPreview(WB: TWebbrowser);
var
  vaIn, vaOut: OleVariant;
begin
  if WB_DocumentLoaded(WB) then
  try
    // Execute the print preview command.
    WB.ControlInterface.ExecWB(OLECMDID_PRINTPREVIEW,
      OLECMDEXECOPT_DONTPROMPTUSER, vaIn, vaOut);
  except
    MsgError('Cannot show Print Preview for WebBrowser control.');
  end;
end;

//----------------------------------------------------------------------------
procedure WB_ShowPageSetup(WB: TWebbrowser);
var
  vaIn, vaOut: OleVariant;
begin
  if WB_DocumentLoaded(WB) then
  try
    // Execute the page setup command.
    WB.ControlInterface.ExecWB(OLECMDID_PAGESETUP, OLECMDEXECOPT_PROMPTUSER,
      vaIn, vaOut);
  except
    MsgError('Cannot show Print Setup for WebBrowser control.');
  end;
end;

//----------------------------------------------------------------------------
procedure WB_ShowFindDialog(WB: TWebbrowser);
begin
  InvokeCMD(WB, HTMLID_FIND);
end;

//----------------------------------------------------------------------------
function WB_GetZoom(WB: TWebBrowser): TWBFontSize;
var
  vaIn, vaOut: OleVariant;
begin
  result := 0;
  if WB_DocumentLoaded(WB) then
  begin
    vaIn := EmptyParam; //was null
    InvokeCmd(WB, FALSE, OLECMDID_ZOOM, OLECMDEXECOPT_DONTPROMPTUSER, vaIn, vaOut);
    result := vaOut;
  end;
end;

//----------------------------------------------------------------------------
procedure WB_SetZoom(WB: TWebBrowser; Size: TWBFontSize);
var
  V: OleVariant;
begin
  if WB_DocumentLoaded(WB) then
  begin
    V := Size;
    WB.ExecWB(OLECMDID_ZOOM, OLECMDEXECOPT_DODEFAULT, V);
  end;
end;

//------------------------------------------------------
// Added by AT:

procedure WB_NavigateBlank(WB: TWebbrowser);
begin
  try
    WB.Navigate('about:blank');
    WB_Wait(WB);
  except
    MsgError('Cannot navigate to blank page in WebBrowser control.');
  end;
end;

//----------------------------------------------------------------------------
procedure WB_NavigateFilename(WB: TWebbrowser; const FileName: WideString; DoWait: Boolean);
begin
  try
    WB.Navigate('file:///' + FileName); //Prefix mandatory
    if DoWait then
      WB_Wait(WB);
  except
    MsgError('Cannot navigate in WebBrowser control.');
  end;
end;

//----------------------------------------------------------------------------
function GetBodyElement(WB: TWebbrowser): IHTMLElement2;
var
  Dispatch: IDispatch;
begin
  Result := nil;
  if WB_DocumentLoaded(WB) then
    try
      Dispatch := (WB.ControlInterface.Document as IHTMLDocument2).Body;
      if Assigned(Dispatch) then
        Dispatch.QueryInterface(IHTMLElement2, Result);
    except
      MsgError('Cannot get body element in WebBrowser control.');
    end;
end;

//----------------------------------------------------------------------------
function WB_GetScrollTop(WB: TWebbrowser): Integer;
var
  Element: IHTMLElement2;
begin
  Result := 0;
  try
    Element := GetBodyElement(WB);
    if Assigned(Element) then
      Result := Element.ScrollTop;
  except
    MsgError('Cannot get scroll state in WebBrowser control.');
  end;
end;

//----------------------------------------------------------------------------
procedure WB_SetScrollTop(WB: TWebbrowser; Value: Integer);
var
  Element: IHTMLElement2;
begin
  try
    Element := GetBodyElement(WB);
    if Assigned(Element) then
      Element.ScrollTop := Value;
  except
    MsgError('Cannot scroll in WebBrowser control.');
  end;
end;

//----------------------------------------------------------------------------
function WB_GetScrollHeight(WB: TWebbrowser): Integer;
var
  Element: IHTMLElement2;
begin
  Result := 0;
  try
    Element := GetBodyElement(WB);
    if Assigned(Element) then
      Result := Element.ScrollHeight;
  except
    MsgError('Cannot get scroll state in WebBrowser control.');
  end;
end;

//----------------------------------------------------------------------------
procedure WB_IncreaseFont(WB: TWebbrowser; Increment: Boolean);
var
  N: TWBFontSize;
begin
  N := WB_GetZoom(WB);
  if Increment then
  begin
    if N < High(TWBFontSize) then
      WB_SetZoom(WB, Succ(N));
  end
  else
  begin
    if N > Low(TWBFontSize) then
      WB_SetZoom(WB, Pred(N));
  end;
end;

//----------------------------------------------------------------------------

{$ifdef OFFLINE}

{ Declarations from WinInet.pas }

type
  TInternetConnectedInfo = record
    dwConnectedState: DWORD;
    dwFlags: DWORD;
  end;

const
  INTERNET_STATE_CONNECTED                    = $00000001;  { connected state (mutually exclusive with disconnected) }
  INTERNET_STATE_DISCONNECTED                 = $00000002;  { disconnected from network }
  INTERNET_STATE_DISCONNECTED_BY_USER         = $00000010;  { disconnected by user request }
  INTERNET_STATE_IDLE                         = $00000100;  { no network requests being made (by Wininet) }
  INTERNET_STATE_BUSY                         = $00000200;  { network requests being made (by Wininet) }

  INTERNET_OPTION_CONNECTED_STATE             = 50;
  ISO_FORCE_DISCONNECTED  = $00000001;

type
  HINTERNET = Pointer;

  TInternetSetOption = function (hInet: HINTERNET; dwOption: DWORD;
    lpBuffer: Pointer; dwBufferLength: DWORD): BOOL; stdcall;

  TInternetQueryOption = function (hInet: HINTERNET; dwOption: DWORD;
    lpBuffer: Pointer; var lpdwBufferLength: DWORD): BOOL; stdcall;

var
  HLib: THandle = 0;
  InternetSetOption: TInternetSetOption = nil;
  InternetQueryOption: TInternetQueryOption = nil;


{ Custom definitions }

type
  TWebOfflineMode = (woUnknown, woOfflineOn, woOfflineOff);

var
  WebInitialOffline: TWebOfflineMode = woUnknown;

function InitWinInetDLL: Boolean;
begin
  Result := False;
  try
    if HLib <> 0 then
      Exit;
    HLib := LoadLibrary('wininet.dll');
    if HLib <> 0 then
    begin
      InternetSetOption := GetProcAddress(HLib, 'InternetSetOptionA');
      InternetQueryOption:= GetProcAddress(HLib, 'InternetQueryOptionA');
    end;
  finally
    Result := (HLib <> 0) and
      Assigned(InternetSetOption) and
      Assigned(InternetQueryOption);
  end;
end;

procedure FreeWinInetDLL;
begin
  if HLib <> 0 then
  begin
    //Restore initial offline status
    if WebInitialOffline <> woUnknown then
      WB_SetGlobalOffline(WebInitialOffline = woOfflineOn);

    //Unload DLL
    FreeLibrary(HLib);
    HLib := 0;
    InternetSetOption := nil;
    InternetQueryOption := nil;
  end;
end;

//----------------------------------------------------------------------------
procedure WB_SetGlobalOffline(AValue: Boolean);
var
  ci: TInternetConnectedInfo;
  dwSize: DWORD;
begin
  //Load DLL
  if not InitWinInetDLL then
    Exit;

  //Remember initial offline status
  if WebInitialOffline = woUnknown then
  begin
    if WB_GetGlobalOffline then
      WebInitialOffline := woOfflineOn
    else
      WebInitialOffline := woOfflineOff;
  end;

  //Set the option
  dwSize := SizeOf(ci);
  if AValue then
  begin
    ci.dwConnectedState := INTERNET_STATE_DISCONNECTED_BY_USER;
    ci.dwFlags := ISO_FORCE_DISCONNECTED;
    InternetSetOption(nil, INTERNET_OPTION_CONNECTED_STATE, @ci, dwSize);
  end
  else
  begin
    ci.dwConnectedState := INTERNET_STATE_CONNECTED;
    ci.dwFlags := 0;
    InternetSetOption(nil, INTERNET_OPTION_CONNECTED_STATE, @ci, dwSize);
  end;
end;

//----------------------------------------------------------------------------
function WB_GetGlobalOffline: Boolean;
var
  dwState: DWORD;
  dwSize: DWORD;
begin
  //Load DLL
  if not InitWinInetDLL then
  begin
    Result := False;
    Exit
  end;

  //Get the option
  dwState := 0;
  dwSize := SizeOf(dwState);
  Result := False;
  if (InternetQueryOption(nil, INTERNET_OPTION_CONNECTED_STATE, @dwState, dwSize)) then
    if ((dwState and INTERNET_STATE_DISCONNECTED_BY_USER) <> 0) then
      Result := True;
end;

{$endif}


initialization

  OleInitialize(nil);

finalization

  {$ifdef OFFLINE}
  FreeWinInetDLL;
  {$endif}

  OleUninitialize;

end.
