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

unit iebrowser;
interface

uses Windows, ActiveX, urlmon, Classes, Graphics, OleCtrls, StdVCL,
     UrlMon2, Menus, SHDocVw;


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

const
  CONTEXT_MENU_DEFAULT                      = 0;
  CONTEXT_MENU_IMAGE                        = 1;
  CONTEXT_MENU_CONTROL                      = 2;
  CONTEXT_MENU_TABLE                        = 3;
  CONTEXT_MENU_DEBUG                        = 4;
  CONTEXT_MENU_1DSELECT                     = 5;
  CONTEXT_MENU_ANCHOR                       = 6;
  CONTEXT_MENU_IMGDYNSRC                    = 7;


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

  TWebBrowserHostUIFlag = (uiDialog, uiDisableHelpMenu, uiNo3DBorder,
    uiScrollNo, uiDisableScriptInactive, uiOpenNewWin, uiDisableOffscreen,
    uiFlatScrollbar, uiDivBlockdefault, uiActivateClienthitOnly,
    uiOverrideBehaviorFactory, uiCodepageLinkedFonts, uiUrlEncodingDisableUtf8,
    uiUrlEncodingEnableUtf8, uiEnableFormsAutocomplete, uiEnableInplaceNavigation);

  TWebBrowserHostUIFlags = set of TWebBrowserHostUIFlag;

  TWebBrowserControl = class(TWebBrowser, IDocHostUIHandler)
  private
    FCSSStyles: TStrings;
    FStyles: WideString;
    FUIFlags: TWebBrowserHostUIFlags;
    FOnGetExternal: TWebBrowserControlOnGetExternal;
    FOnPopupMenu: TWebBrowserControlOnPopupMenu;
    procedure OnStylesChange(sender: TObject);
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
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property UIFlags: TWebBrowserHostUIFlags read FUIFlags write FUIFlags;
    property CSSStyles: TStrings read FCSSStyles write SetCSSStyles;
    property OnGetExternal: TWebBrowserControlOnGetExternal read FOnGetExternal write FOnGetExternal;
    property OnPopupMenu: TWebBrowserControlOnPopupMenu read FOnPopupMenu write FOnPopupMenu;
  end;

procedure Register;

implementation

uses ComObj;

constructor TWebBrowserControl.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FCSSStyles := TStringList.Create;
  TStringList(FCSSStyles).OnChange := OnStylesChange;
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

procedure TWebBrowserControl.OnStylesChange(sender: TObject);
begin
  FStyles := FCSSStyles.Text;
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
begin
  Info.dwFlags := 0;
  for UIFlag := Low(UIFlag) to High(UIFlag) do
    if UIFlag in FUIFlags then
      Info.dwFlags := Info.dwFlags or (1 shl Ord(UIFlag));
  if Info.cbSize = sizeof(TDocHostUIInfo) then  // Version 5.0 and later
    begin
      if FStyles <> '' then
        Info.pchHostCss := SysAllocString(PWideChar(WideString(FCSSStyles.Text))) else
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
  Result := E_NOTIMPL;
end;

function TWebBrowserControl.TranslateUrl(dwTranslate: DWord; pchURLIn: POleStr;
         var ppchURLOut: POleStr) : HResult;
begin
  Result := S_FALSE;
end;

function TWebBrowserControl.FilterDataObject(pDO: IDataObject; out ppDORet: IDataObject) : HResult;
begin
  Result := S_FALSE;
end;

procedure Register;
begin
  RegisterComponents('WebPack', [TWebBrowserControl]);
end;

end.
