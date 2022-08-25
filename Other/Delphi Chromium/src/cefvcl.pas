(*
 *                       Delphi Chromium Embedded
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *)

unit cefvcl;

{$I cef.inc}

interface
uses
  Windows, Messages, Classes,
  cefgui, ceflib,
{$ifdef DELPHI16_UP}
  Vcl.Controls, Vcl.Graphics
{$else}
  Controls, Graphics
{$endif};

type
  TCustomChromium = class(TWinControl, IChromiumEvents)
  private
    FHandler: ICefBase;
    FBrowser: ICefBrowser;
    FBrowserHandle: HWND;
    FDefaultUrl: ustring;

    FOnBeforePopup: TOnBeforePopup;
    FOnAfterCreated: TOnAfterCreated;
    FOnBeforeClose: TOnBeforeClose;
    FOnClose: TOnClose;
    FOnRunModal: TOnRunModal;

    FOnLoadStart: TOnLoadStart;
    FOnLoadEnd: TOnLoadEnd;
    FOnLoadError: TOnLoadError;

    FOnAuthCredentials: TOnAuthCredentials;
    FOnGetDownloadHandler: TOnGetDownloadHandler;
    FOnBeforeBrowse: TOnBeforeBrowse;
    FOnBeforeResourceLoad: TOnBeforeResourceLoad;
    FOnProtocolExecution: TOnProtocolExecution;
    FOnResourceResponse: TOnResourceResponse;

    FOnAddressChange: TOnAddressChange;
    FOnConsoleMessage: TOnConsoleMessage;
    FOnNavStateChange: TOnNavStateChange;
    FOnStatusMessage: TOnStatusMessage;
    FOnTitleChange: TOnTitleChange;
    FOnTooltip: TOnTooltip;

    FOnTakeFocus: TOnTakeFocus;
    FOnSetFocus: TOnSetFocus;

    FOnKeyEvent: TOnKeyEvent;

    FOnBeforeMenu: TOnBeforeMenu;
    FOnGetMenuLabel: TOnGetMenuLabel;
    FOnMenuAction: TOnMenuAction;

    FOnPrintHeaderFooter: TOnPrintHeaderFooter;
    FOnPrintOptions: TOnPrintOptions;

    FOnFindResult: TOnFindResult;

    FOnJsAlert: TOnJsAlert;
    FOnJsConfirm: TOnJsConfirm;
    FOnJsPrompt: TOnJsPrompt;
    FOnJsBinding: TOnJsBinding;

    FOnDragStart: TOnDragEvent;
    FOnDragEnter: TOnDragEvent;

    FOptions: TChromiumOptions;
    FUserStyleSheetLocation: ustring;
    FDefaultEncoding: ustring;
    FFontOptions: TChromiumFontOptions;

    procedure GetSettings(var settings: TCefBrowserSettings);
    procedure CreateBrowser;
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure Loaded; override;
    procedure Resize; override;

    function doOnBeforePopup(const parentBrowser: ICefBrowser;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var url: ustring; var client: ICefBase): Boolean; virtual;
    procedure doOnAfterCreated(const browser: ICefBrowser); virtual;
    function doOnBeforeClose(const browser: ICefBrowser): Boolean; virtual;
    function doOnClose(const browser: ICefBrowser): Boolean; virtual;
    function doOnRunModal(const browser: ICefBrowser): Boolean; virtual;

    procedure doOnLoadStart(const browser: ICefBrowser; const frame: ICefFrame); virtual;
    function doOnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer): Boolean; virtual;
    function doOnLoadError(const browser: ICefBrowser;
      const frame: ICefFrame; errorCode: TCefHandlerErrorcode;
      const failedUrl: ustring; var errorText: ustring): Boolean; virtual;

    function doOnAuthCredentials(const browser: ICefBrowser; isProxy: Boolean; Port: Integer;
      const host, realm, scheme: ustring; var username, password: ustring): Boolean; virtual;
    function doOnGetDownloadHandler(const browser: ICefBrowser; const mimeType, fileName: ustring;
      contentLength: int64; var handler: ICefDownloadHandler): Boolean; virtual;
    function doOnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; navType: TCefHandlerNavtype;
      isRedirect: boolean): Boolean; virtual;
    function doOnBeforeResourceLoad(const browser: ICefBrowser;
      const request: ICefRequest; var redirectUrl: ustring;
      var resourceStream: ICefStreamReader; const response: ICefResponse;
      loadFlags: Integer): Boolean; virtual;
    function doOnProtocolExecution(const browser: ICefBrowser;
      const url: ustring; var AllowOsExecution: Boolean): Boolean; virtual;
    procedure doOnResourceResponse(const browser: ICefBrowser;
      const url: ustring; const response: ICefResponse; var filter: ICefBase); virtual;

    function doOnAddressChange(const browser: ICefBrowser;
      const frame: ICefFrame; const url: ustring): Boolean; virtual;
    function doOnConsoleMessage(const browser: ICefBrowser; const message,
      source: ustring; line: Integer): Boolean; virtual;
    function doOnNavStateChange(const browser: ICefBrowser; canGoBack,
      canGoForward: Boolean): Boolean; virtual;
    function doOnStatusMessage(const browser: ICefBrowser; const value: ustring;
      StatusType: TCefHandlerStatusType): Boolean; virtual;
    function doOnTitleChange(const browser: ICefBrowser;
      const title: ustring): Boolean; virtual;
    function doOnTooltip(const browser: ICefBrowser; var text: ustring): Boolean; virtual;

    procedure doOnTakeFocus(const browser: ICefBrowser; next: Boolean); virtual;
    function doOnSetFocus(const browser: ICefBrowser; source: TCefHandlerFocusSource): Boolean; virtual;

    function doOnKeyEvent(const browser: ICefBrowser; event: TCefHandlerKeyEventType;
      code, modifiers: Integer; isSystemKey: Boolean): Boolean; virtual;

    function doOnBeforeMenu(const browser: ICefBrowser;
      const menuInfo: PCefHandlerMenuInfo): Boolean; virtual;
    function doOnGetMenuLabel(const browser: ICefBrowser;
      menuId: TCefHandlerMenuId; var caption: ustring): Boolean; virtual;
    function doOnMenuAction(const browser: ICefBrowser;
      menuId: TCefHandlerMenuId): Boolean; virtual;

    function doOnPrintHeaderFooter(const browser: ICefBrowser;
      const frame: ICefFrame; printInfo: PCefPrintInfo;
      const url, title: ustring; currentPage, maxPages: Integer;
      var topLeft, topCenter, topRight, bottomLeft, bottomCenter,
      bottomRight: ustring): Boolean; virtual;
    function doOnPrintOptions(const browser: ICefBrowser;
        printOptions: PCefPrintOptions): Boolean; virtual;

    function doOnJsAlert(const browser: ICefBrowser; const frame: ICefFrame;
      const message: ustring): Boolean; virtual;
    function doOnJsConfirm(const browser: ICefBrowser; const frame: ICefFrame;
      const message: ustring; var retval: Boolean): Boolean; virtual;
    function doOnJsPrompt(const browser: ICefBrowser; const frame: ICefFrame;
      const message, defaultValue: ustring; var retval: Boolean;
      var return: ustring): Boolean; virtual;
    function doOnJsBinding(const browser: ICefBrowser;
      const frame: ICefFrame; const obj: ICefv8Value): Boolean; virtual;
    function doOnFindResult(const browser: ICefBrowser; count: Integer;
      selectionRect: PCefRect; identifier, activeMatchOrdinal,
      finalUpdate: Boolean): Boolean; virtual;

    function doOnGetViewRect(const browser: ICefBrowser; rect: PCefRect): Boolean;
    function doOnGetScreenRect(const browser: ICefBrowser; rect: PCefRect): Boolean;
    function doOnGetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer;
      screenX, screenY: PInteger): Boolean;
    procedure doOnPopupShow(const browser: ICefBrowser; show: Boolean);
    procedure doOnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
    procedure doOnPaint(const browser: ICefBrowser; kind: TCefPaintElementType;
        const dirtyRect: PCefRect; const buffer: Pointer);
    procedure doOnCursorChange(const browser: ICefBrowser; cursor: TCefCursorHandle);

    function doOnDragStart(const browser: ICefBrowser;
      const dragData: ICefDragData; mask: Integer): Boolean;
    function doOnDragEnter(const browser: ICefBrowser;
      const dragData: ICefDragData; mask: Integer): Boolean;

    property DefaultUrl: ustring read FDefaultUrl write FDefaultUrl;

    property OnBeforePopup: TOnBeforePopup read FOnBeforePopup write FOnBeforePopup;
    property OnAfterCreated: TOnAfterCreated read FOnAfterCreated write FOnAfterCreated;
    property OnBeforeClose: TOnBeforeClose read FOnBeforeClose write FOnBeforeClose;
    property OnClose: TOnClose read FOnClose write FOnClose;
    property OnRunModal: TOnRunModal read FOnRunModal write FOnRunModal;

    property OnLoadStart: TOnLoadStart read FOnLoadStart write FOnLoadStart;
    property OnLoadEnd: TOnLoadEnd read FOnLoadEnd write FOnLoadEnd;
    property OnLoadError: TOnLoadError read FOnLoadError write FOnLoadError;

    property OnAuthCredentials: TOnAuthCredentials read FOnAuthCredentials write FOnAuthCredentials;
    property OnGetDownloadHandler: TOnGetDownloadHandler read FOnGetDownloadHandler write FOnGetDownloadHandler;
    property OnBeforeBrowse: TOnBeforeBrowse read FOnBeforeBrowse write FOnBeforeBrowse;
    property OnBeforeResourceLoad: TOnBeforeResourceLoad read FOnBeforeResourceLoad write FOnBeforeResourceLoad;
    property OnProtocolExecution: TOnProtocolExecution read FOnProtocolExecution write FOnProtocolExecution;
    property OnResourceResponse: TOnResourceResponse read FOnResourceResponse write FOnResourceResponse;

    property OnAddressChange: TOnAddressChange read FOnAddressChange write FOnAddressChange;
    property OnConsoleMessage: TOnConsoleMessage read FOnConsoleMessage write FOnConsoleMessage;
    property OnNavStateChange: TOnNavStateChange read FOnNavStateChange write FOnNavStateChange;
    property OnStatusMessage: TOnStatusMessage read FOnStatusMessage write FOnStatusMessage;
    property OnTitleChange: TOnTitleChange read FOnTitleChange write FOnTitleChange;
    property OnTooltip: TOnTooltip read FOnTooltip write FOnTooltip;

    property OnTakeFocus: TOnTakeFocus read FOnTakeFocus write FOnTakeFocus;
    property OnSetFocus: TOnSetFocus read FOnSetFocus write FOnSetFocus;

    property OnKeyEvent: TOnKeyEvent read FOnKeyEvent write FOnKeyEvent;

    property OnBeforeMenu: TOnBeforeMenu read FOnBeforeMenu write FOnBeforeMenu;
    property OnGetMenuLabel: TOnGetMenuLabel read FOnGetMenuLabel write FOnGetMenuLabel;
    property OnMenuAction: TOnMenuAction read FOnMenuAction write FOnMenuAction;

    property OnPrintHeaderFooter: TOnPrintHeaderFooter read FOnPrintHeaderFooter write FOnPrintHeaderFooter;
    property OnPrintOptions: TOnPrintOptions read FOnPrintOptions write FOnPrintOptions;

    property OnJsAlert: TOnJsAlert read FOnJsAlert write FOnJsAlert;
    property OnJsConfirm: TOnJsConfirm read FOnJsConfirm write FOnJsConfirm;
    property OnJsPrompt: TOnJsPrompt read FOnJsPrompt write FOnJsPrompt;
    property OnJsBinding: TOnJsBinding read FOnJsBinding write FOnJsBinding;
    property OnFindResult: TOnFindResult read FOnFindResult write FOnFindResult;

    property OnDragStart: TOnDragEvent read FOnDragStart write FOnDragStart;
    property OnDragEnter: TOnDragEvent read FOnDragEnter write FOnDragEnter;

    property Options: TChromiumOptions read FOptions write FOptions default [];
    property FontOptions: TChromiumFontOptions read FFontOptions;
    property DefaultEncoding: ustring read FDefaultEncoding write FDefaultEncoding;
    property UserStyleSheetLocation: ustring read FUserStyleSheetLocation write FUserStyleSheetLocation;
    property BrowserHandle: HWND read FBrowserHandle;
    property Browser: ICefBrowser read FBrowser;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load(const url: ustring);
    procedure ReCreateBrowser(const url: string);
  end;

  TChromium = class(TCustomChromium)
  public
    property BrowserHandle;
    property Browser;
  published
    property Color;
    property Constraints;
    property TabStop;
    property Align;
    property Anchors;
    property DefaultUrl;
    property TabOrder;
    property Visible;

    property OnBeforePopup;
    property OnAfterCreated;
    property OnBeforeClose;
    property OnClose;
    property OnRunModal;

    property OnLoadStart;
    property OnLoadEnd;
    property OnLoadError;

    property OnAuthCredentials;
    property OnGetDownloadHandler;
    property OnBeforeBrowse;
    property OnBeforeResourceLoad;
    property OnProtocolExecution;
    property OnResourceResponse;

    property OnAddressChange;
    property OnConsoleMessage;
    property OnNavStateChange;
    property OnStatusMessage;
    property OnTitleChange;
    property OnTooltip;

    property OnTakeFocus;
    property OnSetFocus;

    property OnKeyEvent;

    property OnBeforeMenu;
    property OnGetMenuLabel;
    property OnMenuAction;

    property OnPrintHeaderFooter;
    property OnPrintOptions;

    property OnJsAlert;
    property OnJsConfirm;
    property OnJsPrompt;
    property OnJsBinding;
    property OnFindResult;

    property OnDragStart;
    property OnDragEnter;

    property Options;
    property FontOptions;
    property DefaultEncoding;
    property UserStyleSheetLocation;
  end;

  TChromiumOSR = class(TCustomChromiumOSR)
  protected
    function GetHandlerClass: TCustomClientHandlerClass; override;
  public
    property Browser;
  published
    property DefaultUrl;

    property OnBeforePopup;
    property OnAfterCreated;
    property OnBeforeClose;
    property OnClose;
    property OnRunModal;

    property OnLoadStart;
    property OnLoadEnd;
    property OnLoadError;

    property OnAuthCredentials;
    property OnGetDownloadHandler;
    property OnBeforeBrowse;
    property OnBeforeResourceLoad;
    property OnProtocolExecution;
    property OnResourceResponse;

    property OnAddressChange;
    property OnConsoleMessage;
    property OnNavStateChange;
    property OnStatusMessage;
    property OnTitleChange;
    property OnTooltip;

    property OnTakeFocus;
    property OnSetFocus;

    property OnKeyEvent;

    property OnBeforeMenu;
    property OnGetMenuLabel;
    property OnMenuAction;

    property OnPrintHeaderFooter;
    property OnPrintOptions;

    property OnJsAlert;
    property OnJsConfirm;
    property OnJsPrompt;
    property OnJsBinding;
    property OnFindResult;

    property OnGetViewRect;
    property OnGetScreenRect;
    property OnGetScreenPoint;
    property OnPopupShow;
    property OnPopupSize;
    property OnPaint;
    property OnCursorChange;

    property OnDragStart;
    property OnDragEnter;

    property Options;
    property FontOptions;
    property DefaultEncoding;
    property UserStyleSheetLocation;
  end;

function CefGetBitmap(const browser: ICefBrowser; typ: TCefPaintElementType; Bitmap: TBitmap): Boolean;

implementation
{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  uses
{$IFDEF DELPHI16_UP}
  Vcl.AppEvnts;
{$ELSE}
  AppEvnts;
{$ENDIF}

var
  CefInstances: Integer = 0;
  CefTimer: UINT = 0;
{$ENDIF}

type
  TVCLClientHandler = class(TCustomClientHandler)
  public
    constructor Create(const crm: IChromiumEvents); override;
    destructor Destroy; override;
  end;

function CefGetBitmap(const browser: ICefBrowser; typ: TCefPaintElementType; Bitmap: TBitmap): Boolean;
var
  w, h, i: Integer;
  p, s: Pointer;
begin
  browser.GetSize(typ, w, h);
  Bitmap.PixelFormat := pf32bit;
{$IFDEF DELPHI12_UP}
  Bitmap.SetSize(w, h);
{$ELSE}
  Bitmap.Width := w;
  Bitmap.Height := h;
{$ENDIF}
  GetMem(p, h * w * 4);
  try
    Result := browser.GetImage(typ, w, h, p);
    s := p;
    for i := 0 to h - 1 do
    begin
      Move(s^, Bitmap.ScanLine[i]^, w*4);
      Inc(Integer(s), w*4);
    end;
  finally
    FreeMem(p);
  end;
end;

{ TVCLClientHandler }

constructor TVCLClientHandler.Create(const crm: IChromiumEvents);
begin
  inherited Create(crm);
{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  if CefInstances = 0 then
    CefTimer := SetTimer(0, 0, 10, nil);
  InterlockedIncrement(CefInstances);
{$ENDIF}
end;

destructor TVCLClientHandler.Destroy;
begin
{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  InterlockedDecrement(CefInstances);
  if CefInstances = 0 then
    KillTimer(0, CefTimer);
{$ENDIF}
  inherited;
end;

{ TCustomChromium }

constructor TCustomChromium.Create(AOwner: TComponent);
begin
  inherited;

  if not (csDesigning in ComponentState) then
    FHandler := TVCLClientHandler.Create(Self) as ICefBase;

  FOptions := [];
  FFontOptions := TChromiumFontOptions.Create;

  FUserStyleSheetLocation := '';
  FDefaultEncoding := '';
  FBrowserHandle := INVALID_HANDLE_VALUE;
  FBrowser := nil;
end;

procedure TCustomChromium.CreateBrowser;
var
  info: TCefWindowInfo;
  settings: TCefBrowserSettings;
  rect: TRect;
begin
  if not (csDesigning in ComponentState) then
  begin
    FillChar(info, SizeOf(info), 0);
    rect := GetClientRect;
    info.Style := WS_CHILD or WS_VISIBLE or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or WS_TABSTOP;
    info.WndParent := Handle;
    info.x := rect.left;
    info.y := rect.top;
    info.Width := rect.right - rect.left;
    info.Height := rect.bottom - rect.top;
    info.ExStyle := 0;
    FillChar(settings, SizeOf(TCefBrowserSettings), 0);
    settings.size := SizeOf(TCefBrowserSettings);
    GetSettings(settings);
    FBrowser := CefBrowserCreateSync(@info, FHandler.Wrap, '', @settings);
    FBrowserHandle := FBrowser.GetWindowHandle;
  end;
end;

procedure TCustomChromium.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited;
  CreateBrowser;
end;

destructor TCustomChromium.Destroy;
begin
  if FBrowser <> nil then
    FBrowser.ParentWindowWillClose;
  if FHandler <> nil then
    (FHandler as ICefClientHandler).Disconnect;
  FHandler := nil;
  FBrowser := nil;
  FFontOptions.Free;
  inherited;
end;

function TCustomChromium.doOnAddressChange(const browser: ICefBrowser;
  const frame: ICefFrame; const url: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnAddressChange) then
    FOnAddressChange(Self, browser, frame, url, Result);
end;

procedure TCustomChromium.doOnAfterCreated(const browser: ICefBrowser);
begin
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  if (browser <> nil) and not browser.IsPopup then
  begin
    FBrowser := browser;
    FBrowserHandle := browser.GetWindowHandle;
  end;
{$ENDIF}
  if Assigned(FOnAfterCreated) then
    FOnAfterCreated(Self, browser);
end;

function TCustomChromium.doOnAuthCredentials(const browser: ICefBrowser;
  isProxy: Boolean; Port: Integer; const host, realm, scheme: ustring; var username,
  password: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnAuthCredentials) then
    FOnAuthCredentials(Self, browser, isProxy, port, host, realm, scheme, username, password, Result);
end;

function TCustomChromium.doOnBeforeBrowse(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  navType: TCefHandlerNavtype; isRedirect: boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforeBrowse) then
    FOnBeforeBrowse(Self, browser, frame, request, navType, isRedirect, Result);
end;

function TCustomChromium.doOnBeforeMenu(const browser: ICefBrowser;
  const menuInfo: PCefHandlerMenuInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforeMenu) then
    FOnBeforeMenu(Self, browser, menuInfo, Result);
end;

function TCustomChromium.doOnBeforePopup(const parentBrowser: ICefBrowser;
  var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var url: ustring; var client: ICefBase): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforePopup) then
    FOnBeforePopup(Self, parentBrowser, popupFeatures, windowInfo, url, client, Result);
end;

function TCustomChromium.doOnBeforeResourceLoad(const browser: ICefBrowser;
  const request: ICefRequest; var redirectUrl: ustring;
  var resourceStream: ICefStreamReader; const response: ICefResponse;
  loadFlags: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforeResourceLoad) then
    FOnBeforeResourceLoad(Self, browser, request, redirectUrl, resourceStream,
      response, loadFlags, Result);
end;

function TCustomChromium.doOnBeforeClose(
  const browser: ICefBrowser): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforeClose) then
    FOnBeforeClose(Self, browser, Result);
end;

function TCustomChromium.doOnConsoleMessage(const browser: ICefBrowser; const message,
  source: ustring; line: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnConsoleMessage) then
    FOnConsoleMessage(Self, browser, message, source, line, Result);
end;

function TCustomChromium.doOnGetDownloadHandler(const browser: ICefBrowser;
  const mimeType, fileName: ustring; contentLength: int64;
  var handler: ICefDownloadHandler): Boolean;
begin
  Result := False;
  if Assigned(FOnGetDownloadHandler) then
    FOnGetDownloadHandler(Self, browser, mimeType, fileName, contentLength, handler, Result);
end;

function TCustomChromium.doOnFindResult(const browser: ICefBrowser;
  count: Integer; selectionRect: PCefRect; identifier, activeMatchOrdinal,
  finalUpdate: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnFindResult) then
    FOnFindResult(Self, browser, count, selectionRect, identifier,
      activeMatchOrdinal, finalUpdate, Result);
end;

function TCustomChromium.doOnGetMenuLabel(const browser: ICefBrowser;
  menuId: TCefHandlerMenuId; var caption: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnGetMenuLabel) then
    FOnGetMenuLabel(Self, browser, menuId, caption, Result);
end;

function TCustomChromium.doOnGetScreenPoint(const browser: ICefBrowser; viewX,
  viewY: Integer; screenX, screenY: PInteger): Boolean;
begin
  Result := False;
end;

function TCustomChromium.doOnGetScreenRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := False;
end;

function TCustomChromium.doOnGetViewRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := False;
end;

function TCustomChromium.doOnJsAlert(const browser: ICefBrowser;
  const frame: ICefFrame; const message: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnJsAlert) then
    FOnJsAlert(Self, browser, frame, message, Result);
end;

function TCustomChromium.doOnJsBinding(const browser: ICefBrowser;
  const frame: ICefFrame; const obj: ICefv8Value): Boolean;
begin
  Result := False;
  if Assigned(FOnJsBinding) then
    FOnJsBinding(Self, browser, frame, obj, Result);
end;

function TCustomChromium.doOnJsConfirm(const browser: ICefBrowser;
  const frame: ICefFrame; const message: ustring;
  var retval: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnJsConfirm) then
    FOnJsConfirm(Self, browser, frame, message, retval, Result);
end;

function TCustomChromium.doOnJsPrompt(const browser: ICefBrowser;
  const frame: ICefFrame; const message, defaultValue: ustring;
  var retval: Boolean; var return: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnJsPrompt) then
    FOnJsPrompt(Self, browser, frame, message, defaultValue, retval, return, Result);
end;

function TCustomChromium.doOnKeyEvent(const browser: ICefBrowser;
  event: TCefHandlerKeyEventType; code, modifiers: Integer;
  isSystemKey: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnKeyEvent) then
    FOnKeyEvent(Self, browser, event, code, modifiers, isSystemKey, Result);
end;

function TCustomChromium.doOnLoadEnd(const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnLoadEnd) then
    FOnLoadEnd(Self, browser, frame, httpStatusCode, Result);
end;

function TCustomChromium.doOnLoadError(const browser: ICefBrowser;
  const frame: ICefFrame; errorCode: TCefHandlerErrorcode;
  const failedUrl: ustring; var errorText: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnLoadError) then
    FOnLoadError(Self, browser, frame, errorCode, failedUrl, errorText, Result);
end;

procedure TCustomChromium.doOnLoadStart(const browser: ICefBrowser;
  const frame: ICefFrame);
begin
  if Assigned(FOnLoadStart) then
    FOnLoadStart(Self, browser, frame);
end;

function TCustomChromium.doOnMenuAction(const browser: ICefBrowser;
  menuId: TCefHandlerMenuId): Boolean;
begin
  Result := False;
  if Assigned(FOnMenuAction) then
    FOnMenuAction(Self, browser, menuId, Result);
end;

function TCustomChromium.doOnNavStateChange(const browser: ICefBrowser;
  canGoBack, canGoForward: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnNavStateChange) then
    FOnNavStateChange(Self, browser, canGoBack, canGoForward, Result);
end;

procedure TCustomChromium.doOnCursorChange(const browser: ICefBrowser;
  cursor: TCefCursorHandle);
begin

end;

function TCustomChromium.doOnDragEnter(const browser: ICefBrowser;
  const dragData: ICefDragData; mask: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnDragEnter) then
    FOnDragEnter(Self, browser, dragData, mask, Result);
end;

function TCustomChromium.doOnDragStart(const browser: ICefBrowser;
  const dragData: ICefDragData; mask: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnDragStart) then
    FOnDragStart(Self, browser, dragData, mask, Result);
end;

procedure TCustomChromium.doOnPaint(const browser: ICefBrowser;
  kind: TCefPaintElementType; const dirtyRect: PCefRect; const buffer: Pointer);
begin

end;

procedure TCustomChromium.doOnPopupShow(const browser: ICefBrowser;
  show: Boolean);
begin

end;

procedure TCustomChromium.doOnPopupSize(const browser: ICefBrowser;
  const rect: PCefRect);
begin

end;

function TCustomChromium.doOnPrintHeaderFooter(const browser: ICefBrowser;
  const frame: ICefFrame; printInfo: PCefPrintInfo; const url, title: ustring;
  currentPage, maxPages: Integer; var topLeft, topCenter, topRight, bottomLeft,
  bottomCenter, bottomRight: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnPrintHeaderFooter) then
    FOnPrintHeaderFooter(Self, browser, frame, printInfo, url, title,
      currentPage, maxPages, topLeft, topCenter, topRight, bottomLeft,
      bottomCenter, bottomRight, Result);
end;

function TCustomChromium.doOnPrintOptions(const browser: ICefBrowser;
  printOptions: PCefPrintOptions): Boolean;
begin
  Result := False;
  if Assigned(FOnPrintOptions) then
    FOnPrintOptions(Self, browser, printOptions, Result);
end;

function TCustomChromium.doOnProtocolExecution(const browser: ICefBrowser;
  const url: ustring; var AllowOsExecution: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnProtocolExecution) then
    FOnProtocolExecution(Self, browser, url, AllowOsExecution, Result);
end;

function TCustomChromium.doOnClose(const browser: ICefBrowser): Boolean;
begin
  Result := False;
  if Assigned(FOnClose) then
    FOnClose(Self, browser, Result);
end;

procedure TCustomChromium.doOnResourceResponse(const browser: ICefBrowser;
  const url: ustring; const response: ICefResponse; var filter: ICefBase);
begin
  if Assigned(FOnResourceResponse) then
    FOnResourceResponse(Self, browser, url, response, filter);
end;

function TCustomChromium.doOnRunModal(const browser: ICefBrowser): Boolean;
begin
  Result := False;
  if Assigned(FOnRunModal) then
    FOnRunModal(Self, browser, Result);
end;

function TCustomChromium.doOnSetFocus(const browser: ICefBrowser;
  source: TCefHandlerFocusSource): Boolean;
begin
  Result := False;
  if Assigned(FOnSetFocus) then
    FOnSetFocus(Self, browser, source, Result);
end;

function TCustomChromium.doOnStatusMessage(const browser: ICefBrowser;
  const value: ustring; StatusType: TCefHandlerStatusType): Boolean;
begin
  Result := False;
  if Assigned(FOnStatusMessage) then
    FOnStatusMessage(Self, browser, value, StatusType, Result);
end;

procedure TCustomChromium.doOnTakeFocus(const browser: ICefBrowser;
  next: Boolean);
begin
  if Assigned(FOnTakeFocus) then
    FOnTakeFocus(Self, browser, next);
end;

function TCustomChromium.doOnTitleChange(const browser: ICefBrowser;
  const title: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnTitleChange) then
    FOnTitleChange(Self, browser, title, Result);
end;

function TCustomChromium.doOnTooltip(const browser: ICefBrowser;
  var text: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnTooltip) then
    FOnTooltip(Self, browser, text, Result);
end;

procedure TCustomChromium.GetSettings(var settings: TCefBrowserSettings);
begin
  Assert(settings.size = SizeOf(settings));
  settings.standard_font_family := CefString(FFontOptions.StandardFontFamily);
  settings.fixed_font_family := CefString(FFontOptions.FixedFontFamily);
  settings.serif_font_family := CefString(FFontOptions.SerifFontFamily);
  settings.sans_serif_font_family := CefString(FFontOptions.SansSerifFontFamily);
  settings.cursive_font_family := CefString(FFontOptions.CursiveFontFamily);
  settings.fantasy_font_family := CefString(FFontOptions.FantasyFontFamily);
  settings.default_font_size := FFontOptions.DefaultFontSize;
  settings.default_fixed_font_size := FFontOptions.DefaultFixedFontSize;
  settings.minimum_font_size := FFontOptions.MinimumFontSize;
  settings.minimum_logical_font_size := FFontOptions.MinimumLogicalFontSize;
  settings.remote_fonts_disabled := FFontOptions.RemoteFontsDisabled;
  settings.default_encoding := CefString(DefaultEncoding);
  settings.user_style_sheet_location := CefString(UserStyleSheetLocation);

  settings.drag_drop_disabled := coDragDropDisabled in FOptions;
  settings.encoding_detector_enabled := coEncodingDetectorEnabled in FOptions;
  settings.javascript_disabled := coJavascriptDisabled in FOptions;
  settings.javascript_open_windows_disallowed := coJavascriptOpenWindowsDisallowed in FOptions;
  settings.javascript_close_windows_disallowed := coJavascriptCloseWindowsDisallowed in FOptions;
  settings.javascript_access_clipboard_disallowed := coJavascriptAccessClipboardDisallowed in FOptions;
  settings.dom_paste_disabled := coDomPasteDisabled in FOptions;
  settings.caret_browsing_enabled := coCaretBrowsingEnabled in FOptions;
  settings.java_disabled := coJavaDisabled in FOptions;
  settings.plugins_disabled := coPluginsDisabled in FOptions;
  settings.universal_access_from_file_urls_allowed := coUniversalAccessFromFileUrlsAllowed in FOptions;
  settings.file_access_from_file_urls_allowed := coFileAccessFromFileUrlsAllowed in FOptions;
  settings.web_security_disabled := coWebSecurityDisabled in FOptions;
  settings.xss_auditor_enabled := coXssAuditorEnabled in FOptions;
  settings.image_load_disabled := coImageLoadDisabled in FOptions;
  settings.shrink_standalone_images_to_fit := coShrinkStandaloneImagesToFit in FOptions;
  settings.site_specific_quirks_disabled := coSiteSpecificQuirksDisabled in FOptions;
  settings.text_area_resize_disabled := coTextAreaResizeDisabled in FOptions;
  settings.page_cache_disabled := coPageCacheDisabled in FOptions;
  settings.tab_to_links_disabled := coTabToLinksDisabled in FOptions;
  settings.hyperlink_auditing_disabled := coHyperlinkAuditingDisabled in FOptions;
  settings.user_style_sheet_enabled := coUserStyleSheetEnabled in FOptions;
  settings.author_and_user_styles_disabled := coAuthorAndUserStylesDisabled in FOptions;
  settings.local_storage_disabled := coLocalStorageDisabled in FOptions;
  settings.databases_disabled := coDatabasesDisabled in FOptions;
  settings.application_cache_disabled := coApplicationCacheDisabled in FOptions;
  settings.webgl_disabled := coWebglDisabled in FOptions;
  settings.accelerated_compositing_enabled := coAcceleratedCompositingEnabled in FOptions;
  settings.accelerated_layers_disabled := coAcceleratedLayersDisabled in FOptions;
  settings.accelerated_2d_canvas_disabled := coAccelerated2dCanvasDisabled in FOptions;
  settings.developer_tools_disabled := coDeveloperToolsDisabled in FOptions;
end;

procedure TCustomChromium.Load(const url: ustring);
var
  frm: ICefFrame;
begin
  HandleNeeded;
  if FBrowser <> nil then
  begin
    frm := FBrowser.MainFrame;
    if frm <> nil then
      frm.LoadUrl(url);
  end;
end;

procedure TCustomChromium.Loaded;
begin
  inherited;
  Load(FDefaultUrl);
end;

procedure TCustomChromium.ReCreateBrowser(const url: string);
begin
  if (FBrowser <> nil) {$IFNDEF FMX}and (FBrowserHandle <> 0){$ENDIF} then
  begin
    FBrowser.ParentWindowWillClose;
    SendMessage(FBrowserHandle, WM_CLOSE, 0, 0);
    SendMessage(FBrowserHandle, WM_DESTROY, 0, 0);
    FBrowserHandle := 0;
    FBrowser := nil;

    CreateBrowser;
    Load(url);
  end;
end;

procedure TCustomChromium.Resize;
var
  brws: ICefBrowser;
  rect: TRect;
  hdwp: THandle;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    brws := FBrowser;
    if (brws <> nil) and (brws.GetWindowHandle <> INVALID_HANDLE_VALUE) then
    begin
      rect := GetClientRect;
      hdwp := BeginDeferWindowPos(1);
      try
        hdwp := DeferWindowPos(hdwp, brws.GetWindowHandle, 0,
          rect.left, rect.top, rect.right - rect.left, rect.bottom - rect.top,
          SWP_NOZORDER);
      finally
        EndDeferWindowPos(hdwp);
      end;
    end;
  end;
end;

procedure TCustomChromium.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_SETFOCUS:
      begin
        if (FBrowser <> nil) and (FBrowser.GetWindowHandle <> 0) then
          PostMessage(FBrowser.GetWindowHandle, WM_SETFOCUS, Message.WParam, 0);
        inherited WndProc(Message);
      end;
    WM_ERASEBKGND:
      if (csDesigning in ComponentState) or (FBrowser = nil) then
        inherited WndProc(Message);
    CM_WANTSPECIALKEY:
      if not (TWMKey(Message).CharCode in [VK_LEFT .. VK_DOWN]) then
        Message.Result := 1 else
        inherited WndProc(Message);
    WM_GETDLGCODE:
      Message.Result := DLGC_WANTARROWS;
  else
    inherited WndProc(Message);
  end;
end;

{ TChromiumOSR }

function TChromiumOSR.GetHandlerClass: TCustomClientHandlerClass;
begin
  Result := TVCLClientHandler;
end;

{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}{$IFNDEF FMX}
type
  TCefApplicationEvents = class(TApplicationEvents)
  public
    procedure doIdle(Sender: TObject; var Done: Boolean);
    procedure doMessage(var Msg: TMsg; var Handled: Boolean);
    constructor Create(AOwner: TComponent); override;
  end;

constructor TCefApplicationEvents.Create(AOwner: TComponent);
begin
  inherited;
  OnIdle := doIdle;
  OnMessage := doMessage;
end;

procedure TCefApplicationEvents.doIdle(Sender: TObject; var Done: Boolean);
begin
  if CefInstances > 0 then
    CefDoMessageLoopWork;
end;

procedure TCefApplicationEvents.doMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if CefInstances > 0 then
    CefDoMessageLoopWork;
end;

var
  AppEvent: TCefApplicationEvents;

initialization
  AppEvent := TCefApplicationEvents.Create(nil);

finalization
  AppEvent.Free;
{$ENDIF}{$ENDIF}

end.
