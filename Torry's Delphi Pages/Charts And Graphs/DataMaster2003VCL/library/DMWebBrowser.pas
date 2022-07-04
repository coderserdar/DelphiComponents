///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

unit DMWebBrowser;

interface

uses
  Windows, SysUtils, Classes, Controls, OleCtrls, SHDocVw, ActiveX, MSHTML,
  StdCtrls, Forms, URLMON;

const
  DLCTL_DLIMAGES = $00000010;
  DLCTL_VIDEOS = $00000020;
  DLCTL_BGSOUNDS = $00000040;
  DLCTL_NO_SCRIPTS = $00000080;
  DLCTL_NO_JAVA = $00000100;
  DLCTL_NO_RUNACTIVEXCTLS = $00000200;
  DLCTL_NO_DLACTIVEXCTLS = $00000400;
  DLCTL_DOWNLOADONLY = $00000800;
  DLCTL_NO_FRAMEDOWNLOAD = $00001000;
  DLCTL_RESYNCHRONIZE = $00002000;
  DLCTL_PRAGMA_NO_CACHE = $00004000;
  DLCTL_NO_BEHAVIORS = $00008000;
  DLCTL_NO_METACHARSET = $00010000;
  DLCTL_URL_ENCODING_DISABLE_UTF8 = $00020000;
  DLCTL_URL_ENCODING_ENABLE_UTF8 = $00040000;
  DLCTL_FORCEOFFLINE = $10000000;
  DLCTL_NO_CLIENTPULL = $20000000;
  DLCTL_SILENT = $40000000;
  DLCTL_OFFLINEIFNOTCONNECTED = $80000000;
  DLCTL_OFFLINE = DLCTL_OFFLINEIFNOTCONNECTED;

type
  PDOCHOSTUIINFO = ^TDOCHOSTUIINFO;
  TDOCHOSTUIINFO = record
    cbSize: ULONG;
    dwFlags: DWORD;
    dwDoubleClick: DWORD;
    chHostCss: POLESTR;
    chHostNS: POLESTR;
  end;

  IDocHostUIHandler=interface(IUnknown)
    ['{bd3f23c0-d43e-11cf-893b-00aa00bdce1a}']
    function ShowContextMenu(const dwID: DWORD; const ppt: PPOINT; const pcmdtReserved: IUnknown;
      const pdispReserved: IDispatch): HRESULT; stdcall;
    function GetHostInfo(var pInfo: TDOCHOSTUIINFO): HRESULT; stdcall;
    function ShowUI(const dwID: DWORD; const pActiveObject: IOleInPlaceActiveObject;
      const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;
      const pDoc: IOleInPlaceUIWindow): HRESULT; stdcall;
    function HideUI: HRESULT; stdcall;
    function UpdateUI: HRESULT; stdcall;
    function EnableModeless(const fEnable: BOOL): HRESULT; stdcall;
    function OnDocWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function OnFrameWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function ResizeBorder(const prcBorder: PRECT; const pUIWindow: IOleInPlaceUIWindow;
      const fRameWindow: BOOL): HRESULT; stdcall;
    function TranslateAccelerator(const lpMsg: PMSG; const pguidCmdGroup: PGUID;
      const nCmdID: DWORD): HRESULT; stdcall;
    function GetOptionKeyPath(var pchKey: POLESTR; const dw: DWORD): HRESULT; stdcall;
    function GetDropTarget(const pDropTarget: IDropTarget;
      out ppDropTarget: IDropTarget): HRESULT; stdcall;
    function GetExternal(out ppDispatch: IDispatch): HRESULT; stdcall;
    function TranslateUrl(const dwTranslate: DWORD; const pchURLIn: POLESTR;
      var ppchURLOut: POLESTR): HRESULT; stdcall;
    function FilterDataObject(const pDO: IDataObject; out ppDORet: IDataObject): HRESULT; stdcall;
  end;

 IDocHostShowUI = interface(IUnknown)
    ['{c4d244b0-d43e-11cf-893b-00aa00bdce1a}']
    function ShowMessage(hwnd: THandle; lpstrText: POLESTR; lpstrCaption: POLESTR;
      dwType: longint; lpstrHelpFile: POLESTR; dwHelpContext: longint;
      plResult: pointer): HRESULT; stdcall;
    function ShowHelp(hwnd: THandle; pszHelpFile: POLESTR; uCommand: integer;
      dwData: longint; ptMouse: TPoint;
      var pDispatchObjectHit: IDispatch): HRESULT; stdcall;
  end; 

  TGetExternalEvent = function(out ppDispatch: IDispatch): HRESULT of object;
  TShowContextMenuEvent=function(const dwID: DWORD; const ppt: PPOINT;
    const pcmdtReserved: IUnknown; const pdispReserved: IDispatch): HRESULT of object;

  TDMWebBrowser=class(TWebBrowser, IDocHostUIHandler, IDispatch, IDocHostShowUI, IServiceProvider)
  private
    { Private declarations }
    ActualHandle: HWND;   // required for correct docking
    FOnGetExternal: TGetExternalEvent;
    FOnShowContextMenu: TShowContextMenuEvent;
    FScrollBars: TScrollStyle;
    FCtl3D: boolean;
    FDisableSelection: boolean;
    FSecurityManager: IInternetSecurityManager;
    procedure SetScrollBars(St: TScrollStyle);
    procedure SetCtl3D(B: boolean); 
  protected
    { Protected declarations }  
    // IDocHostUIHandler
    function ShowContextMenu(const dwID: DWORD; const ppt: PPOINT; 
      const pcmdtReserved: IUnknown; const pdispReserved: IDispatch): HRESULT; stdcall;
    function GetHostInfo(var pInfo: TDOCHOSTUIINFO): HRESULT; stdcall;
    function ShowUI(const dwID: DWORD; const pActiveObject: IOleInPlaceActiveObject;
      const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;
      const pDoc: IOleInPlaceUIWindow): HRESULT; stdcall;
    function HideUI: HRESULT; stdcall;
    function UpdateUI: HRESULT; stdcall;
    function EnableModeless(const fEnable: BOOL): HRESULT; stdcall;
    function OnDocWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function OnFrameWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function ResizeBorder(const prcBorder: PRECT; const pUIWindow: IOleInPlaceUIWindow;
      const fRameWindow: BOOL): HRESULT; stdcall;
    function TranslateAccelerator(const lpMsg: PMSG; const pguidCmdGroup: PGUID;
      const nCmdID: DWORD): HRESULT; stdcall;
    function GetOptionKeyPath(var pchKey: POLESTR; const dw: DWORD): HRESULT; stdcall;
    function GetDropTarget(const pDropTarget: IDropTarget;
      out ppDropTarget: IDropTarget): HRESULT; stdcall;
    function GetExternal(out ppDispatch: IDispatch): HRESULT; stdcall;
    function TranslateUrl(const dwTranslate: DWORD; const pchURLIn: POLESTR;
      var ppchURLOut: POLESTR): HRESULT; stdcall;
    function FilterDataObject(const pDO: IDataObject; out ppDORet: IDataObject): HRESULT; stdcall;
    // IDispatch override
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    // IDocHostShowUI  
    function ShowMessage(hwnd: THandle;
      lpstrText: POLESTR; lpstrCaption: POLESTR; dwType: longint; lpstrHelpFile: POLESTR;
      dwHelpContext: longint; plResult: pointer): HRESULT; stdcall;
    function ShowHelp(hwnd: THandle; pszHelpFile: POLESTR; uCommand: integer;
      dwData: longint; ptMouse: TPoint; var pDispatchObjectHit: IDispatch): HRESULT; stdcall;
    // IServiceProvider
    function QueryService(const rsid, iid: TGuid; out Obj): HResult; stdcall;
    // these methods required for correct docking
    procedure DestroyWnd; override; 
    procedure CreateWnd; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    function FireEventHandler(HandlerName: string; Arguments: Variant): Variant;
  published
    { Published declarations }
    property OnGetExternal: TGetExternalEvent read FOnGetExternal write FOnGetExternal;
    property OnShowContextMenu: TShowContextMenuEvent read FOnShowContextmenu write FOnShowContextmenu;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars;
    property Ctl3D: boolean read FCtl3D write SetCtl3D;
    property DisableSelection: boolean read FDisableSelection write FDisableSelection;
  end;

  TDMSecurityManager=class(TContainedObject, IInternetSecurityManager)
  protected
    { Protected declarations }
    // IInternetSecurityManager
    function SetSecuritySite(Site: IInternetSecurityMgrSite): HResult; stdcall;
    function GetSecuritySite(out Site: IInternetSecurityMgrSite): HResult; stdcall;
    function MapUrlToZone(pwszUrl: LPCWSTR; out dwZone: DWORD;
      dwFlags: DWORD): HResult; stdcall;
    function GetSecurityId(pwszUrl: LPCWSTR; pbSecurityId: Pointer;
      var cbSecurityId: DWORD; dwReserved: DWORD): HResult; stdcall;
    function ProcessUrlAction(pwszUrl: LPCWSTR; dwAction: DWORD;
      pPolicy: Pointer; cbPolicy: DWORD; pContext: Pointer; cbContext: DWORD;
      dwFlags, dwReserved: DWORD): HResult; stdcall;
    function QueryCustomPolicy(pwszUrl: LPCWSTR; const guidKey: TGUID;
      out pPolicy: Pointer; out cbPolicy: DWORD; pContext: Pointer; cbContext: DWORD;
      dwReserved: DWORD): HResult; stdcall;
    function SetZoneMapping(dwZone: DWORD; lpszPattern: LPCWSTR;
      dwFlags: DWORD): HResult; stdcall;
    function GetZoneMappings(dwZone: DWORD; out enumString: IEnumString;
      dwFlags: DWORD): HResult; stdcall;
  end;

procedure Register;

var
  DMBrowserDownloadOptions: cardinal=DLCTL_DLIMAGES or DLCTL_VIDEOS or DLCTL_BGSOUNDS;
  DisableDMBrowserSecurity: boolean=true; // control security management

implementation

uses Variants, Messages;

procedure Register;
begin
  RegisterComponents('DM2003', [TDMWebBrowser]);
end;

{ TDMWebBrowser }

constructor TDMWebBrowser.Create(AOwner: TComponent);
begin
  inherited;
  FSecurityManager:=TDMSecurityManager.Create(Self);
end;

function TDMWebBrowser.FireEventHandler(HandlerName: string; Arguments: Variant): Variant;
var
  Doc: IHTMLDocument;
  Disp: IDispatch;
  DID: TDISPID;
  Params: TDISPPARAMS;
  Name: array[0..1000] of widechar;
  PName: PWideChar;
  Args: PVariantArgList;
  MemSize: integer;
  ACount, I: integer;
begin
  Result:=VarNull;
  // some safechecks
  if (Document=nil) or (ReadyState<{READYSTATE_COMPLETE}READYSTATE_INTERACTIVE)
  then Exit;
  // obtain "script" dispinterface
  Document.QueryInterface(IID_IHTMLDocument, Doc);
  if Doc=nil then Exit;
  Disp:=Doc.Script;
  // prepare event handler name
  StringToWideChar(HandlerName, Name, SizeOf(Name)-1);
  PName:=@Name; // <- double pointer!!!
  // retrieve DispID
  if Disp.GetIDsOfNames(GUID_NULL, @PName, 1, GetSystemDefaultLCID, @DID)<>S_OK
  then Exit;  // no such event handler
  // prepare handler parameters
  Params.rgdispidNamedArgs:=nil;
  Params.cNamedArgs:=0;
  if VarIsArray(Arguments) // notice: must be an array!
  then ACount:=VarArrayHighBound(Arguments,1)+1
  else ACount:=0;
  Params.cArgs:=ACount;
  if ACount>0 then
  begin
    MemSize:=ACount*SizeOf(OleVariant);
    GetMem(Args, MemSize);
    FillChar(Args^, MemSize, 0);
    Params.rgvarg:=Args;
    for I:=0 to ACount-1 do
    OleVariant(Args[I]):=Arguments[I];
  end else
  begin
    Args:=nil;
    MemSize:=0;
    Params.rgvarg:=nil;
  end;
  // safely call event handler
  try
    if Disp.Invoke(DID, GUID_NULL, GetSystemDefaultLCID, DISPATCH_METHOD,
      Params, @Result, nil, nil)<>S_OK
    then Windows.Beep(1000,100);
  finally
    if Args<>nil then
    begin
      Finalize(OleVariant(Args[0]), ACount);
      FreeMem(Args, MemSize);
    end;
  end;
end;

procedure TDMWebBrowser.SetScrollBars(St: TScrollStyle);
begin
  if St<>FScrollBars then
  begin
    FScrollBars:=St;
    // (Application as IOleControl).OnAmbientPropertyChange(DISPID_AMBIENT_DLCONTROL);  
    {No!!! ^this force to reload Invoke, but ScrollBars and Ctl3D defined in GetHostInfo!!!
    although this code is like that in tembeddedwb...}
    if Assigned(Document)
    then Refresh;
  end;
end;

procedure TDMWebBrowser.SetCtl3D(B: boolean);
begin
  if FCtl3D<>B then
  begin
    FCtl3D:=B;
    if Assigned(Document)
    then Refresh;
  end;
end;

{TWebBrowser docking problem workaround code by henry34223
http://groups.yahoo.com/group/delphi-webbrowser/message/6666}

procedure TDMWebBrowser.DestroyWnd;
begin
  if (csDestroying in ComponentState)
  then inherited {TOleControl::DestroyWnd();} else
  begin
    //Parent to the Application window which is 0x0 in size
    Windows.SetParent(WindowHandle, Forms.Application.Handle);
    //save the WindowHandle
    ActualHandle:=WindowHandle;
    //set it to 0 so Createwnd will be called again...
    WindowHandle:=0;
  end;
end;

procedure TDMWebBrowser.CreateWnd;
begin
  if (ActualHandle<>0) then
  begin
    if (IsWindow(ActualHandle)) then
    begin
      WindowHandle:=ActualHandle;
      ActualHandle:=0;
      Windows.SetParent(WindowHandle, TWinControl(Self).Parent.Handle);
      //Force a resize on the client window
      MoveWindow(WindowHandle, 0, 0, TWinControl(Self).Parent.Width,
         TWinControl(Self).Parent.Height, true);
      //quick exit because there is NO need to create the window
      Exit;
    end;
  end;
  inherited;
end;

{IDocHostUIHandler members
Partially based on TEmbeddedWB component by Per Lindsoe Larsen
See http://www.euromind.com/iedelphi for more details}

function TDMWebBrowser.EnableModeless(const fEnable: BOOL): HRESULT;
begin
  Result:=E_NOTIMPL;
end;

function TDMWebBrowser.FilterDataObject(const pDO: IDataObject; out ppDORet: IDataObject): HRESULT;
begin 
  Result:=E_NOTIMPL;
end;

function TDMWebBrowser.GetDropTarget(const pDropTarget: IDropTarget;
  out ppDropTarget: IDropTarget): HRESULT;
begin
  Result:=E_NOTIMPL;
end;

function TDMWebBrowser.GetExternal(out ppDispatch: IDispatch): HRESULT;
begin
  if Assigned(FOnGetExternal) 
  then Result:=FOnGetExternal(ppDispatch)
  else Result:=S_FALSE;
end;

function TDMWebBrowser.GetHostInfo(var pInfo: TDOCHOSTUIINFO): HRESULT;
begin
  if FDisableSelection
  then pInfo.dwFlags:=pInfo.dwFlags or 1; //DOCHOSTUIFLAG_DIALOG=1
  if not FCtl3D
  then pInfo.dwFlags:=pInfo.dwFlags or 4; // DOCHOSTUIFLAG_NO3DBORDER=4
  if FScrollBars=ssNone
  then pInfo.dwFlags:=pInfo.dwFlags or 8; // DOCHOSTUIFLAG_SCROLL_NO=8
  // pInfo.dwFlags:=pInfo.dwFlags or $40000; // DOCHOSTUIFLAG_THEME=$40000
  // pInfo.dwFlags:=pInfo.dwFlags or $80000; // DOCHOSTUIFLAG_NOTHEME=$80000 
  // Themes better controlled in the META tags (see PSDK)
  Result:=S_OK;
end;

function TDMWebBrowser.GetOptionKeyPath(var pchKey: POLESTR; const dw: DWORD): HRESULT;
begin 
  Result:=E_NOTIMPL; 
end;

function TDMWebBrowser.HideUI: HRESULT;
begin 
  Result:=E_NOTIMPL; 
end;

function TDMWebBrowser.OnDocWindowActivate(const fActivate: BOOL): HRESULT;
begin 
  Result:=E_NOTIMPL; 
end;

function TDMWebBrowser.OnFrameWindowActivate(const fActivate: BOOL): HRESULT;
begin 
  Result:=E_NOTIMPL; 
end;

function TDMWebBrowser.ResizeBorder(const prcBorder: PRECT;
  const pUIWindow: IOleInPlaceUIWindow; const fRameWindow: BOOL): HRESULT;
begin 
  Result:=E_NOTIMPL;
end;

function TDMWebBrowser.ShowContextMenu(const dwID: DWORD; const ppt: PPOINT; 
  const pcmdtReserved: IUnknown; const pdispReserved: IDispatch): HRESULT;
begin
  if Assigned(FOnShowContextmenu) 
  then Result:=FOnSHowContextmenu(dwID, ppt, pcmdtreserved, pdispreserved)
  else Result:=E_NOTIMPL;
end;

function TDMWebBrowser.ShowUI(const dwID: DWORD; const pActiveObject: IOleInPlaceActiveObject;
  const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;
  const pDoc: IOleInPlaceUIWindow): HRESULT;
begin
  Result:=E_NOTIMPL;
end;

function TDMWebBrowser.TranslateAccelerator(const lpMsg: PMSG;
  const pguidCmdGroup: PGUID; const nCmdID: DWORD): HRESULT;
(*const
  BannedCtrlKeys: set of byte=[{VK_P}$50, {N}$4E, {O}$4F, {F}$46];
var
  H: THandle;
  CN: array[0..127] of char;
  B: boolean;*)
begin
  Result:=E_NOTIMPL;
(* FillChar(CN, SizeOf(CN), 0);
  CN:='';
  H:=GetFocus;
  if H<>0
  then GetClassname(H, CN, SizeOf(CN)-3);
  // check whether ActiveX control focused
  B:=(StrPos(CN, 'Edit')<>nil) or (StrPos(CN, 'List')<>nil);
  // redirect keystroke to its HWND AND return S_OK
  if (H<>0) and (H<>lpMsg.hwnd) and (lpMsg.message=WM_KEYDOWN) and B and
    ((lpMsg.wParam=VK_LEFT) or (lpMsg.wParam=VK_RIGHT) or
    (lpMsg.wParam=VK_UP) or (lpMsg.wParam=VK_DOWN) or
    (lpMsg.wParam=VK_BACK)) then
  begin
    SendMessage(H, WM_KEYDOWN, lpMsg.wParam, lpMsg.lParam);
    Result:=S_OK;
  end;
  // disable unnecessary shortcuts
  if (lpMsg.message=WM_KEYDOWN) and ( ((GetKeyState(VK_CONTROL)<0)
    and (lpMsg.wParam in BannedCtrlKeys)) or (lpMsg.wParam=VK_F5) ) then
  begin
    Windows.Beep(2000, 200);
    Result:=S_OK;
  end; *)
end;

function TDMWebBrowser.TranslateUrl(const dwTranslate: DWORD;
  const pchURLIn: POLESTR; var ppchURLOut: POLESTR): HRESULT;
begin 
  Result:=E_NOTIMPL; 
end;

function TDMWebBrowser.UpdateUI: HRESULT;
begin
  Result:=E_NOTIMPL;
end;

// IDispatch members (for download control)

function TDMWebBrowser.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params;
  VarResult, ExcepInfo, ArgErr: Pointer): HResult;
var
  V: variant;
begin
  Result:=S_OK;
  V:=DMBrowserDownloadOptions;
  TVarData(V).VType:=varInteger;  // REQUIRED to force VType!!!
  if (DispId=DISPID_AMBIENT_DLCONTROL) and (VarResult<>nil)
     and ((Flags and DISPATCH_PROPERTYGET)<>0)
  then PVariant(VarResult)^:=V
  else Result:=inherited Invoke(DispID, IID, LocaleID, Flags, Params,
         VarResult, ExcepInfo, ArgErr);
end;

// IDocHostShowUI members

{Just change messagebox title (default is "Internet Explorer").
Unfortunately, script error messages are NOT handled here.}
function TDMWebBrowser.ShowMessage(hwnd: THandle; lpstrText: POLESTR;
  lpstrCaption: POLESTR; dwType: longint; lpstrHelpFile: POLESTR;
  dwHelpContext: longint; plResult: pointer): HRESULT;
var
  Buf: array[0..4095] of widechar;
  L: longint;
begin
  Result:=S_OK;
  StringToWideChar(Forms.Application.Title, Buf, SizeOf(Buf)-1);
  L:=MessageBoxW(Forms.Application.Handle{hwnd}, lpstrText, Buf, dwType);
  if Assigned(plResult)
  then longint(plResult^):=L;
end;

function TDMWebBrowser.ShowHelp(hwnd: THandle; pszHelpFile: POLESTR;
  uCommand: integer; dwData: longint; ptMouse: TPoint;
  var pDispatchObjectHit: IDispatch): HRESULT;
begin
  Result:=S_FALSE;
end;

// IServiceProvider members

function TDMWebBrowser.QueryService(const rsid, iid: TGuid; out Obj): HResult;
begin
  if IsEqualGUID(rsid, SID_IInternetSecurityManager)
    and IsEqualGUID(iid, IInternetSecurityManager) then
  begin
    Result:=S_OK;
    pointer(Obj):=pointer(FSecurityManager);
  end else Result:=E_NOTIMPL;
end;

{ TDMSecurityManager }

function TDMSecurityManager.GetSecurityId(pwszUrl: LPCWSTR;
  pbSecurityId: Pointer; var cbSecurityId: DWORD;
  dwReserved: DWORD): HResult;
begin
  Result:=INET_E_DEFAULT_ACTION;
end;

function TDMSecurityManager.GetSecuritySite(
  out Site: IInternetSecurityMgrSite): HResult;
begin
  Result:=INET_E_DEFAULT_ACTION;
end;

function TDMSecurityManager.GetZoneMappings(dwZone: DWORD;
  out enumString: IEnumString; dwFlags: DWORD): HResult;
begin
  Result:=INET_E_DEFAULT_ACTION;
end;

function TDMSecurityManager.MapUrlToZone(pwszUrl: LPCWSTR;
  out dwZone: DWORD; dwFlags: DWORD): HResult;
begin
  Result:=INET_E_DEFAULT_ACTION;
end;

function TDMSecurityManager.ProcessUrlAction(pwszUrl: LPCWSTR;
  dwAction: DWORD; pPolicy: Pointer; cbPolicy: DWORD; pContext: Pointer;
  cbContext, dwFlags, dwReserved: DWORD): HResult;
begin
  Result:=INET_E_DEFAULT_ACTION;
  if not DisableDMBrowserSecurity then Exit; // with default security
  if cbPolicy=SizeOf(DWORD) then // check size!
  begin
    DWORD(pPolicy^):=URLPOLICY_ALLOW;
    Result:=S_OK;
  end;
end;

function TDMSecurityManager.QueryCustomPolicy(pwszUrl: LPCWSTR;
  const guidKey: TGUID; out pPolicy: Pointer; out cbPolicy: DWORD;
  pContext: Pointer; cbContext, dwReserved: DWORD): HResult;
begin
  Result:=INET_E_DEFAULT_ACTION;
end;

function TDMSecurityManager.SetSecuritySite(
  Site: IInternetSecurityMgrSite): HResult;
begin
  Result:=INET_E_DEFAULT_ACTION;
end;

function TDMSecurityManager.SetZoneMapping(dwZone: DWORD;
  lpszPattern: LPCWSTR; dwFlags: DWORD): HResult;
begin
  Result:=INET_E_DEFAULT_ACTION;
end;

initialization
  OleInitialize(nil);
finalization
  OleUninitialize;
end.
