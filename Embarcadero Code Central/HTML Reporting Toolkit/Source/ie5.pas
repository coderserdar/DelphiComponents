{*******************************************************}
{                                                       }
{       Delphi Visual Component Library  (WebPack)      }
{       Pluggable Namespace Handler Component           }
{                                                       }
{       Copyright (c) 2000, Semyon A. Chertkov          }
{                                                       }
{     Written by:                                       }
{       Semyon A. Chertkov                              }
{       e-mail:  chertkov@chat.ru                       }
{       WWW: www.chat.ru/~chertkov                      }
{                                                       }
{*******************************************************}

unit ie5;
interface

uses Windows, ActiveX, urlmon, Classes, Graphics, OleCtrls, StdVCL, Forms,
     UrlMon2, Menus, SHDocVw, mshtml;


type
  PDocHostUIInfo = ^TDocHostUIInfo;
  TDocHostUIInfo = record
    cbSize: LongInt;
    dwFlags: DWord;
    dwDoubleClick: DWord;
    pchHostCss: POleStr;
    pchHostNS: POleStr;
  end;

const
  DOCHOSTUIFLAG_DIALOG                      = 1;
  DOCHOSTUIFLAG_DISABLE_HELP_MENU           = 2;
  DOCHOSTUIFLAG_NO3DBORDER                  = 4;
  DOCHOSTUIFLAG_SCROLL_NO                   = 8;
  DOCHOSTUIFLAG_DISABLE_SCRIPT_INACTIVE     = 16;
  DOCHOSTUIFLAG_OPENNEWWIN                  = 32;
  DOCHOSTUIFLAG_DISABLE_OFFSCREEN           = 64;
  DOCHOSTUIFLAG_FLAT_SCROLLBAR              = 128;
  DOCHOSTUIFLAG_DIV_BLOCKDEFAULT            = 256;
  DOCHOSTUIFLAG_ACTIVATE_CLIENTHIT_ONLY     = 512;

const
  DOCHOSTUIDBLCLK_DEFAULT                   = 0;
  DOCHOSTUIDBLCLK_SHOWPROPERTIES            = 1;
  DOCHOSTUIDBLCLK_SHOWCODE                  = 2;

const // Context menu dwID
  CONTEXT_MENU_DEFAULT                      = 0;
  CONTEXT_MENU_IMAGE                        = 1;
  CONTEXT_MENU_CONTROL                      = 2;
  CONTEXT_MENU_TABLE                        = 3;
  CONTEXT_MENU_DEBUG                        = 4;
  CONTEXT_MENU_1DSELECT                     = 5;
  CONTEXT_MENU_ANCHOR                       = 6;
  CONTEXT_MENU_IMGDYNSRC                    = 7;

const
  DISPID_AMBIENT_DLCONTROL = -5512;
  DISPID_AMBIENT_USERAGENT = -5513;

const
  DLCTL_DLIMAGES                          = $00000010;
  DLCTL_VIDEOS                            = $00000020;
  DLCTL_BGSOUNDS                          = $00000040;
  DLCTL_NO_SCRIPTS                        = $00000080;
  DLCTL_NO_JAVA                           = $00000100;
  DLCTL_NO_RUNACTIVEXCTLS                 = $00000200;
  DLCTL_NO_DLACTIVEXCTLS                  = $00000400;
  DLCTL_DOWNLOADONLY                      = $00000800;
  DLCTL_NO_FRAMEDOWNLOAD                  = $00001000;
  DLCTL_RESYNCHRONIZE                     = $00002000;
  DLCTL_PRAGMA_NO_CACHE                   = $00004000;
  DLCTL_NO_BEHAVIORS                   	  = $00008000;
  DLCTL_NO_METACHARSET                    = $00010000;
  DLCTL_URL_ENCODING_DISABLE_UTF8         = $00020000;
  DLCTL_URL_ENCODING_ENABLE_UTF8          = $00040000;
  DLCTL_FORCEOFFLINE                      = $10000000;
  DLCTL_NO_CLIENTPULL                     = $20000000;
  DLCTL_SILENT                            = $40000000;
  DLCTL_OFFLINEIFNOTCONNECTED             = $80000000;
  DLCTL_OFFLINE                           = DLCTL_OFFLINEIFNOTCONNECTED;

type
  IDocHostUIHandler = interface(IUnknown)
    ['{BD3F23C0-D43E-11CF-893B-00AA00BDCE1A}']
    function ShowContextMenu(dwID: DWord; const pt: TPoint;
             pcmdtReserved: IUnknown; pdispReserved: IDispatch): HResult; stdcall;
    function GetHostInfo(var Info: TDocHostUIInfo) : HResult; stdcall;
    function ShowUI(dwID: DWord; ActiveObject: IOleInPlaceActiveObject;
             CommandTarget: IOleCommandTarget; Frame: IOleInPlaceFrame;
             Doc: IOleInPlaceUIWindow) : HResult; stdcall;
    function HideUI: HResult; stdcall;
    function UpdateUI: HResult; stdcall;
    function EnableModeless(fEnable: BOOL) : HResult; stdcall;
    function OnDocWindowActivate(fActivate: BOOL) : HResult; stdcall;
    function OnFrameWindowActivate(fActivate: BOOL) : HResult; stdcall;
    function ResizeBorder(const rcBorder: TRect; pUIWindow: IOleInPlaceUIWindow;
             fRameWindow: BOOL) : HResult; stdcall;
    function TranslateAccelerator(var msg: TMsg; const pguidCmdGroup: TGUID;
             nCmdID: DWord) : HResult; stdcall;
    function GetOptionKeyPath(var pchKey: POleStr; dw: DWord) : HResult; stdcall;
    function GetDropTarget(pDropTarget: IDropTarget;
             out ppDropTarget: IDropTarget) : HResult; stdcall;
    function GetExternal(out ppDispatch: IDispatch): HResult; stdcall;
    function TranslateUrl(dwTranslate: DWord; pchURLIn: POleStr;
             var ppchURLOut: POleStr) : HResult; stdcall;
    function FilterDataObject(pDO: IDataObject; out ppDORet: IDataObject) : HResult; stdcall;
  end;

  ICustomDoc = interface(IUnknown)
    ['{3050F3F0-98B5-11CF-BB82-00AA00BDCE0B}']
    function SetUIHandler(pUIHandler: IDocHostUIHandler): HResult; stdcall;
  end;

type
  TWebBrowserControlOnGetExternal = procedure(Sender: TObject; out ppDisp: IDispatch) of object;
  TWebBrowserControlOnPopupMenu = procedure (Sender: TObject; PopupMenu: TPopupMenu; dwID: DWord;
    cmdTarget: IOleCommandTarget; pdispObject: IDispatch; var Handled: Boolean) of object;
  TWebBrowserControlOnTranslateUrl = procedure (Sender: TObject; const URLIn: String;
    var URLOut: String; var Handled: Boolean) of Object;
  TWebBrowserControlOnFindBehavior = procedure (Sender: TObject; const Behavior: String;
    const BehaviorUrl: WideString; const pBehaviorSite: IElementBehaviorSite;
    out ppBehavior: IElementBehavior) of Object;

  TWebBrowserHostUIFlag = (uiDialog, uiDisableHelpMenu, uiNo3DBorder,
    uiScrollNo, uiDisableScriptInactive, uiOpenNewWin, uiDisableOffscreen,
    uiFlatScrollbar, uiDivBlockdefault, uiActivateClienthitOnly,
    uiOverrideBehaviorFactory, uiCodepageLinkedFonts, uiUrlEncodingDisableUtf8,
    uiUrlEncodingEnableUtf8, uiEnableFormsAutocomplete, uiEnableInplaceNavigation);

  TWebBrowserDLCTLFlag = (dcBGSounds, dcImages, dcDownloadOnly,
    dcForceOffline, dcNoBehaviours, dcNoClientPull,
    dcNoActiveXCtls, dcNoFrameDownload, dcNoJava, dcNoMetaCharset,
    dcNoRunActiveXCtls, dcNoScripts, dcOffline, dcOfflineIfNotConnected,
    dcPragmaNoCache, dcResynch, dcSilent, dcDisableUTF8, dcEnableUTF8,
    dcVideos);

  TWebBrowserHostUIFlags = set of TWebBrowserHostUIFlag;
  TWebBrowserDLCTLFlags = set of TWebBrowserDLCTLFlag;

  TWebBrowserControl = class(TWebBrowser, IDispatch, IDocHostUIHandler,
    IServiceProvider, IElementBehaviorFactory)
  private
    FCSSStyles: TStrings;
    FUIFlags: TWebBrowserHostUIFlags;
    FDCTLFlags: TWebBrowserDLCTLFlags;
    FUserAgent: String;
    FOnGetExternal: TWebBrowserControlOnGetExternal;
    FOnPopupMenu: TWebBrowserControlOnPopupMenu;
    FOnTranslateUrl: TWebBrowserControlOnTranslateUrl;
    FOnFindBehavior: TWebBrowserControlOnFindBehavior;
    procedure SetCSSStyles(Value: TStrings);
  protected
    // IDocHostUIHandler
    function ShowContextMenu(dwID: DWord; const pt: TPoint;
             pcmdtReserved: IUnknown; pdispReserved: IDispatch): HResult; stdcall;
    function GetHostInfo(var Info: TDocHostUIInfo) : HResult; stdcall;
    function ShowUI(dwID: DWord; ActiveObject: IOleInPlaceActiveObject;
             CommandTarget: IOleCommandTarget; Frame: IOleInPlaceFrame;
             Doc: IOleInPlaceUIWindow) : HResult; stdcall;
    function HideUI: HResult; stdcall;
    function UpdateUI: HResult; stdcall;
    function EnableModeless(fEnable: BOOL) : HResult; stdcall;
    function OnDocWindowActivate(fActivate: BOOL) : HResult; stdcall;
    function OnFrameWindowActivate(fActivate: BOOL) : HResult; stdcall;
    function ResizeBorder(const rcBorder: TRect; pUIWindow: IOleInPlaceUIWindow;
             fRameWindow: BOOL) : HResult; stdcall;
    function TranslateAccelerator(var msg: TMsg; const pguidCmdGroup: TGUID;
             nCmdID: DWord) : HResult; stdcall;
    function GetOptionKeyPath(var pchKey: POleStr; dw: DWord) : HResult; stdcall;
    function GetDropTarget(pDropTarget: IDropTarget;
             out ppDropTarget: IDropTarget) : HResult; stdcall;
    function GetExternal(out ppDispatch: IDispatch): HResult; stdcall;
    function TranslateUrl(dwTranslate: DWord; pchURLIn: POleStr;
             var ppchURLOut: POleStr) : HResult; stdcall;
    function FilterDataObject(pDO: IDataObject; out ppDORet: IDataObject) : HResult; stdcall;
    // IDispatch
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    // IServiceProvider
    function QueryService(const rsid, iid: TGuid; out Obj): HResult; stdcall;
    // IElementBehaviorFactory
    function  FindBehavior(const bstrBehavior: WideString; const bstrBehaviorUrl: WideString;
       const pSite: IElementBehaviorSite; out ppBehavior: IElementBehavior): HResult; stdcall;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property UIFlags: TWebBrowserHostUIFlags read FUIFlags write FUIFlags;
    property DCTLFlags: TWebBrowserDLCTLFlags read FDCTLFlags write FDCTLFlags;
    property CSSStyles: TStrings read FCSSStyles write SetCSSStyles;
    property UserAgent: String read FUserAgent write FUserAgent;
    property OnGetExternal: TWebBrowserControlOnGetExternal read FOnGetExternal write FOnGetExternal;
    property OnPopupMenu: TWebBrowserControlOnPopupMenu read FOnPopupMenu write FOnPopupMenu;
    property OnTranslateUrl: TWebBrowserControlOnTranslateUrl read FOnTranslateUrl write FOnTranslateUrl;
    property OnFindBehavior: TWebBrowserControlOnFindBehavior read FOnFindBehavior write FOnFindBehavior;
  end;

implementation

uses ComObj;

constructor TWebBrowserControl.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FCSSStyles := TStringList.Create;
end;

destructor TWebBrowserControl.Destroy;
begin
  FCSSStyles.Free;
  inherited;
end;

procedure TWebBrowserControl.SetCSSStyles(Value: TStrings);
begin
  FCSSStyles.Assign(Value);
end;

// IDocHostUIHandler
function TWebBrowserControl.ShowContextMenu(dwID: DWord; const pt: TPoint;
         pcmdtReserved: IUnknown; pdispReserved: IDispatch): HResult;
var
  Handled: Boolean;
begin
  if PopupMenu <> nil then
    begin
      Handled := True;
      if Assigned(FOnPopupMenu) then
        FOnPopupMenu(self, PopupMenu, dwID,
          pcmdtReserved as IOleCommandTarget, pdispReserved, Handled);
      if Handled then
        begin
          PopupMenu.Popup(pt.X, pt.Y);
          Result := S_OK;
        end
      else
        Result := S_FALSE;
    end
  else
    Result := S_FALSE;
end;

function TWebBrowserControl.GetHostInfo(var Info: TDocHostUIInfo) : HResult;
var
  UIFlag: TWebBrowserHostUIFlag;
  S: String;
  Size: Integer;
begin
  Info.dwFlags := 0;
  for UIFlag := Low(UIFlag) to High(UIFlag) do
    if UIFlag in FUIFlags then
      Info.dwFlags := Info.dwFlags or (1 shl Ord(UIFlag));
  if Info.cbSize = sizeof(TDocHostUIInfo) then
    begin // Version 5.0 and later
      S := FCSSStyles.Text;
      if S <> '' then
        begin // IMPORTANT ! It is necessary to use CoTaskMemAlloc, not SysAllocString
          Size := (Length(S) + 1) shl 1;
          Info.pchHostCss := CoTaskMemAlloc(Size);
          StringToWideChar(S, Info.pchHostCss, Size);
        end
      else
        Info.pchHostCss := nil;
      Info.pchHostNS := nil;
    end;
  Result := S_OK;
end;

function TWebBrowserControl.ShowUI(dwID: DWord; ActiveObject: IOleInPlaceActiveObject;
         CommandTarget: IOleCommandTarget; Frame: IOleInPlaceFrame;
         Doc: IOleInPlaceUIWindow) : HResult;
begin
   Result := E_NOTIMPL; // S_FALSE;
end;

function TWebBrowserControl.HideUI: HResult;
begin
  Result := S_OK;
end;

function TWebBrowserControl.UpdateUI: HResult;
begin
  Result := S_OK;
end;

function TWebBrowserControl.EnableModeless(fEnable: BOOL) : HResult;
begin
  Result := S_OK;
end;

function TWebBrowserControl.OnDocWindowActivate(fActivate: BOOL) : HResult;
begin
  Result := S_OK;
end;

function TWebBrowserControl.OnFrameWindowActivate(fActivate: BOOL) : HResult;
begin
  Result := S_OK;
end;

function TWebBrowserControl.ResizeBorder(const rcBorder: TRect; pUIWindow: IOleInPlaceUIWindow;
         FrameWindow: BOOL) : HResult;
begin
  Result := S_OK;
end;

function TWebBrowserControl.TranslateAccelerator(var msg: TMsg; const pguidCmdGroup: TGUID;
         nCmdID: DWord) : HResult;
begin
  Result := S_FALSE;
end;

function TWebBrowserControl.GetOptionKeyPath(var pchKey: POleStr; dw: DWord) : HResult;
begin
  Result := S_FALSE;
end;

function TWebBrowserControl.GetDropTarget(pDropTarget: IDropTarget;
         out ppDropTarget: IDropTarget) : HResult;
begin
  Result := S_FALSE;
end;

function TWebBrowserControl.GetExternal(out ppDispatch: IDispatch): HResult;
begin
  ppDispatch := nil;
  if Assigned(FOnGetExternal) then
    begin
      FOnGetExternal(Self, ppDispatch);
      Result := S_OK;
    end
  else
    Result := E_NOTIMPL;
end;

function TWebBrowserControl.TranslateUrl(dwTranslate: DWord; pchURLIn: POleStr;
         var ppchURLOut: POleStr) : HResult;
var
  Handled: Boolean;
  URLOut: String;
  Size: Integer;
begin
  Result := S_FALSE;
  if Assigned(FOnTranslateUrl) then
    begin
      Handled := True;
      URLOut := pchURLIn;
      FOnTranslateUrl(self, pchURLIn, URLOut, Handled);
      if Handled then
        begin
          Size := (Length(URLOut) + 1) shl 1;
          ppchURLOut := CoTaskMemAlloc(Size);
          StringToWideChar(URLOut, ppchURLOut, Size);
          Result := S_OK;
        end;
    end;
end;

function TWebBrowserControl.FilterDataObject(pDO: IDataObject; out ppDORet: IDataObject) : HResult;
begin
  Result := S_FALSE;
end;

function TWebBrowserControl.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
  Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;

const
  DCTLFlags: array [TWebBrowserDLCTLFlag] of DWord =
    (DLCTL_BGSOUNDS, DLCTL_DLIMAGES, DLCTL_DOWNLOADONLY,
     DLCTL_FORCEOFFLINE, DLCTL_NO_BEHAVIORS, DLCTL_NO_CLIENTPULL,
     DLCTL_NO_RUNACTIVEXCTLS, DLCTL_NO_FRAMEDOWNLOAD, DLCTL_NO_JAVA, DLCTL_NO_METACHARSET,
     DLCTL_NO_RUNACTIVEXCTLS, DLCTL_NO_SCRIPTS, DLCTL_OFFLINE, DLCTL_OFFLINEIFNOTCONNECTED,
     DLCTL_PRAGMA_NO_CACHE, DLCTL_RESYNCHRONIZE, DLCTL_SILENT,
     DLCTL_URL_ENCODING_DISABLE_UTF8, DLCTL_URL_ENCODING_ENABLE_UTF8, DLCTL_VIDEOS);
var
  dwFlags: DWord;
  DCTLFlag: TWebBrowserDLCTLFlag;
begin
  if (Flags and DISPATCH_PROPERTYGET <> 0) and (VarResult <> nil) then
    begin
      Result := S_OK;
      case DispID of
        DISPID_AMBIENT_USERAGENT:
          if FUserAgent <> '' then
            begin
              POleVariant(VarResult)^ := FUserAgent;
              Exit;
            end;
        DISPID_AMBIENT_DLCONTROL:
            begin
              dwFlags := 0;
              for DCTLFlag := Low(DCTLFlag) to High(DCTLFlag) do
                if DCTLFlag in FDCTLFlags then
                  dwFlags := dwFlags or DCTLFlags[DCTLFlag];
              if dwFlags <> 0 then
                begin
                  POleVariant(VarResult)^ := Integer(dwFlags);
                  Exit;
                end;
            end;
      end;
      Result := inherited Invoke(DispID, IID, LocaleID, Flags, Params,
        VarResult, ExcepInfo, ArgErr);
    end
  else
    Result := inherited Invoke(DispID, IID, LocaleID, Flags, Params,
      VarResult, ExcepInfo, ArgErr);
end;

function TWebBrowserControl.QueryService(const rsid, iid: TGuid; out Obj): HResult;
begin
  if IsEqualIID(rsid, IElementBehaviorFactory) and IsEqualIID(iid, IElementBehaviorFactory) then
    Result := QueryInterface(IElementBehaviorFactory, Obj)
  else
    Result := E_NOINTERFACE;
end;

function TWebBrowserControl.FindBehavior(const bstrBehavior: WideString; const bstrBehaviorUrl: WideString;
  const pSite: IElementBehaviorSite; out ppBehavior: IElementBehavior): HResult;
begin
  Result := E_FAIL;
  if Assigned(FOnFindBehavior) then
    try
      FOnFindBehavior(self, bstrBehavior, bstrBehaviorUrl, pSite, ppBehavior);
      Result := S_OK;
    except
      Forms.Application.HandleException(self);
    end;
end;


end.
