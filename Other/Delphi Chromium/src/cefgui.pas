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

unit cefgui;

{$I cef.inc}

interface

uses
  Classes, ceflib;

type
  TOnBeforePopup = procedure(Sender: TObject; const parentBrowser: ICefBrowser;
    var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
    var url: ustring; var client: ICefBase; out Result: Boolean) of object;
  TOnAfterCreated = procedure(Sender: TObject; const browser: ICefBrowser) of object;
  TOnClose = procedure(Sender: TObject; const browser: ICefBrowser; out Result: Boolean) of object;
  TOnRunModal = procedure(Sender: TObject; const browser: ICefBrowser; out Result: Boolean) of object;

  TOnLoadStart = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame) of object;
  TOnLoadEnd = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer; out Result: Boolean) of object;
  TOnLoadError = procedure(Sender: TObject; const browser: ICefBrowser;
    const frame: ICefFrame; errorCode: TCefHandlerErrorcode;
    const failedUrl: ustring; var errorText: ustring; out Result: Boolean) of object;

  TOnAuthCredentials = procedure(Sender: TObject; const browser: ICefBrowser; isProxy: Boolean; Port: Integer;
    const host, realm, scheme: ustring; var username, password: ustring; out Result: Boolean) of object;
  TOnGetDownloadHandler = procedure(Sender: TObject; const browser: ICefBrowser; const mimeType, fileName: ustring;
    contentLength: int64; var handler: ICefDownloadHandler; out Result: Boolean) of object;
  TOnBeforeBrowse = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
    const request: ICefRequest; navType: TCefHandlerNavtype;
    isRedirect: boolean; out Result: Boolean) of object;
  TOnBeforeResourceLoad = procedure(Sender: TObject; const browser: ICefBrowser;
    const request: ICefRequest; var redirectUrl: ustring;
    var resourceStream: ICefStreamReader; const response: ICefResponse;
    loadFlags: Integer; out Result: Boolean) of object;
  TOnProtocolExecution = procedure(Sender: TObject; const browser: ICefBrowser;
    const url: ustring; var AllowOsExecution: Boolean; out Result: Boolean) of object;
  TOnResourceResponse = procedure(Sender: TObject; const browser: ICefBrowser;
    const url: ustring; const response: ICefResponse; var filter: ICefBase) of object;

  TOnAddressChange = procedure(Sender: TObject; const browser: ICefBrowser;
    const frame: ICefFrame; const url: ustring; out Result: Boolean) of object;
  TOnConsoleMessage = procedure(Sender: TObject; const browser: ICefBrowser; message, source: ustring;
    line: Integer; out Result: Boolean) of object;
  TOnNavStateChange = procedure(Sender: TObject; const browser: ICefBrowser;
    canGoBack, canGoForward: Boolean; out Result: Boolean) of object;
  TOnStatusMessage = procedure(Sender: TObject; const browser: ICefBrowser; const value: ustring; StatusType: TCefHandlerStatusType; out Result: Boolean) of object;
  TOnTitleChange = procedure(Sender: TObject; const browser: ICefBrowser;
    const title: ustring; out Result: Boolean) of object;
  TOnTooltip = procedure(Sender: TObject; const browser: ICefBrowser; var text: ustring; out Result: Boolean) of object;

  TOnTakeFocus = procedure(Sender: TObject; const browser: ICefBrowser; next: Boolean) of object;
  TOnSetFocus = procedure(Sender: TObject; const browser: ICefBrowser; source: TCefHandlerFocusSource; out Result: Boolean) of object;

  TOnKeyEvent = procedure(Sender: TObject; const browser: ICefBrowser; event: TCefHandlerKeyEventType;
    code, modifiers: Integer; isSystemKey: Boolean; out Result: Boolean) of object;

  TOnBeforeMenu = procedure(Sender: TObject; const browser: ICefBrowser;
    const menuInfo: PCefHandlerMenuInfo; out Result: Boolean) of object;
  TOnGetMenuLabel = procedure(Sender: TObject; const browser: ICefBrowser;
    menuId: TCefHandlerMenuId; var caption: ustring; out Result: Boolean) of object;
  TOnMenuAction = procedure(Sender: TObject; const browser: ICefBrowser;
    menuId: TCefHandlerMenuId; out Result: Boolean) of object;

  TOnPrintHeaderFooter = procedure(Sender: TObject; const browser: ICefBrowser;
    const frame: ICefFrame; printInfo: PCefPrintInfo;
    const url, title: ustring; currentPage, maxPages: Integer;
    var topLeft, topCenter, topRight, bottomLeft, bottomCenter,
    bottomRight: ustring; out Result: Boolean) of object;
  TOnPrintOptions = procedure(Sender: TObject; const browser: ICefBrowser; printOptions: PCefPrintOptions; out Result: Boolean) of object;

  TOnJsAlert = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
    const message: ustring; out Result: Boolean) of object;
  TOnJsConfirm = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
    const message: ustring; var retval: Boolean; out Result: Boolean) of object;
  TOnJsPrompt = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
    const message, defaultValue: ustring; var retval: Boolean;
    var return: ustring; out Result: Boolean) of object;
  TOnBeforeClose = procedure(Sender: TObject; const browser: ICefBrowser; out Result: Boolean) of object;
  TOnJsBinding = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const obj: ICefv8Value; out Result: Boolean) of object;
  TOnFindResult = procedure(Sender: TObject; const browser: ICefBrowser; count: Integer;
    selectionRect: PCefRect; identifier, activeMatchOrdinal,
    finalUpdate: Boolean; out Result: Boolean) of object;

  TOnGetViewRect = procedure(Sender: TObject; const browser: ICefBrowser; rect: PCefRect; out Result: Boolean) of object;
  TOnGetScreenRect = procedure(Sender: TObject; const browser: ICefBrowser; rect: PCefRect; out Result: Boolean) of object;
  TOnGetScreenPoint = procedure(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer;
    screenX, screenY: PInteger; out Result: Boolean) of object;
  TOnPopupShow = procedure(Sender: TObject; const browser: ICefBrowser; show: Boolean) of object;
  TOnPopupSize = procedure(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect) of object;
  TOnPaint = procedure(Sender: TObject; const browser: ICefBrowser; kind: TCefPaintElementType;
        const dirtyRect: PCefRect; const buffer: Pointer) of object;
  TOnCursorChange = procedure(Sender: TObject; const browser: ICefBrowser; cursor: TCefCursorHandle) of object;

  TOnDragEvent = procedure(Sender: TObject; const browser: ICefBrowser; const dragData: ICefDragData; mask: Integer; out Result: Boolean) of object;

  TChromiumOption = (coDragDropDisabled, coEncodingDetectorEnabled, coJavascriptDisabled, coJavascriptOpenWindowsDisallowed,
    coJavascriptCloseWindowsDisallowed, coJavascriptAccessClipboardDisallowed, coDomPasteDisabled,
    coCaretBrowsingEnabled, coJavaDisabled, coPluginsDisabled, coUniversalAccessFromFileUrlsAllowed,
    coFileAccessFromFileUrlsAllowed, coWebSecurityDisabled, coXssAuditorEnabled, coImageLoadDisabled,
    coShrinkStandaloneImagesToFit, coSiteSpecificQuirksDisabled, coTextAreaResizeDisabled,
    coPageCacheDisabled, coTabToLinksDisabled, coHyperlinkAuditingDisabled, coUserStyleSheetEnabled,
    coAuthorAndUserStylesDisabled, coLocalStorageDisabled, coDatabasesDisabled,
    coApplicationCacheDisabled, coWebglDisabled, coAcceleratedCompositingEnabled,
    coAcceleratedLayersDisabled, coAccelerated2dCanvasDisabled, coDeveloperToolsDisabled);

  TChromiumOptions = set of TChromiumOption;

  TChromiumFontOptions = class(TPersistent)
  private
    FStandardFontFamily: ustring;
    FCursiveFontFamily: ustring;
    FSansSerifFontFamily: ustring;
    FMinimumLogicalFontSize: Integer;
    FFantasyFontFamily: ustring;
    FSerifFontFamily: ustring;
    FDefaultFixedFontSize: Integer;
    FDefaultFontSize: Integer;
    FRemoteFontsDisabled: Boolean;
    FFixedFontFamily: ustring;
    FMinimumFontSize: Integer;
  public
    constructor Create; virtual;
  published
    property StandardFontFamily: ustring read FStandardFontFamily;
    property FixedFontFamily: ustring read FFixedFontFamily write FFixedFontFamily;
    property SerifFontFamily: ustring read FSerifFontFamily write FSerifFontFamily;
    property SansSerifFontFamily: ustring read FSansSerifFontFamily write FSansSerifFontFamily;
    property CursiveFontFamily: ustring read FCursiveFontFamily write FCursiveFontFamily;
    property FantasyFontFamily: ustring read FFantasyFontFamily write FFantasyFontFamily;
    property DefaultFontSize: Integer read FDefaultFontSize write FDefaultFontSize default 0;
    property DefaultFixedFontSize: Integer read FDefaultFixedFontSize write FDefaultFixedFontSize default 0;
    property MinimumFontSize: Integer read FMinimumFontSize write FMinimumFontSize default 0;
    property MinimumLogicalFontSize: Integer read FMinimumLogicalFontSize write FMinimumLogicalFontSize default 0;
    property RemoteFontsDisabled: Boolean read FRemoteFontsDisabled write FRemoteFontsDisabled default False;
  end;


  IChromiumEvents = interface
  ['{0C139DB1-0349-4D7F-8155-76FEA6A0126D}']
    procedure GetSettings(var settings: TCefBrowserSettings);

    function doOnBeforePopup(const parentBrowser: ICefBrowser;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var url: ustring; var client: ICefBase): Boolean;
    procedure doOnAfterCreated(const browser: ICefBrowser);
    function doOnBeforeClose(const browser: ICefBrowser): Boolean;
    function doOnClose(const browser: ICefBrowser): Boolean;
    function doOnRunModal(const browser: ICefBrowser): Boolean;

    procedure doOnLoadStart(const browser: ICefBrowser; const frame: ICefFrame);
    function doOnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer): Boolean;
    function doOnLoadError(const browser: ICefBrowser;
      const frame: ICefFrame; errorCode: TCefHandlerErrorcode;
      const failedUrl: ustring; var errorText: ustring): Boolean;

    function doOnAuthCredentials(const browser: ICefBrowser; isProxy: Boolean; Port: Integer;
      const host, realm, scheme: ustring; var username, password: ustring): Boolean;
    function doOnGetDownloadHandler(const browser: ICefBrowser; const mimeType, fileName: ustring;
      contentLength: int64; var handler: ICefDownloadHandler): Boolean;
    function doOnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; navType: TCefHandlerNavtype;
      isRedirect: boolean): Boolean;
    function doOnBeforeResourceLoad(const browser: ICefBrowser;
      const request: ICefRequest; var redirectUrl: ustring;
      var resourceStream: ICefStreamReader; const response: ICefResponse;
      loadFlags: Integer): Boolean;
    function doOnProtocolExecution(const browser: ICefBrowser;
      const url: ustring; var AllowOsExecution: Boolean): Boolean;
    procedure doOnResourceResponse(const browser: ICefBrowser;
      const url: ustring; const response: ICefResponse; var filter: ICefBase);

    function doOnAddressChange(const browser: ICefBrowser;
      const frame: ICefFrame; const url: ustring): Boolean;
    function doOnConsoleMessage(const browser: ICefBrowser; const message,
      source: ustring; line: Integer): Boolean;
    function doOnNavStateChange(const browser: ICefBrowser; canGoBack,
      canGoForward: Boolean): Boolean;
    function doOnStatusMessage(const browser: ICefBrowser; const value: ustring;
      StatusType: TCefHandlerStatusType): Boolean;
    function doOnTitleChange(const browser: ICefBrowser;
      const title: ustring): Boolean;
    function doOnTooltip(const browser: ICefBrowser; var text: ustring): Boolean;

    procedure doOnTakeFocus(const browser: ICefBrowser; next: Boolean);
    function doOnSetFocus(const browser: ICefBrowser; source: TCefHandlerFocusSource): Boolean;

    function doOnKeyEvent(const browser: ICefBrowser; event: TCefHandlerKeyEventType;
      code, modifiers: Integer; isSystemKey: Boolean): Boolean;

    function doOnBeforeMenu(const browser: ICefBrowser;
      const menuInfo: PCefHandlerMenuInfo): Boolean;
    function doOnGetMenuLabel(const browser: ICefBrowser;
      menuId: TCefHandlerMenuId; var caption: ustring): Boolean;
    function doOnMenuAction(const browser: ICefBrowser;
      menuId: TCefHandlerMenuId): Boolean;

    function doOnPrintHeaderFooter(const browser: ICefBrowser;
      const frame: ICefFrame; printInfo: PCefPrintInfo;
      const url, title: ustring; currentPage, maxPages: Integer;
      var topLeft, topCenter, topRight, bottomLeft, bottomCenter,
      bottomRight: ustring): Boolean;
    function doOnPrintOptions(const browser: ICefBrowser;
        printOptions: PCefPrintOptions): Boolean;

    function doOnJsAlert(const browser: ICefBrowser; const frame: ICefFrame;
      const message: ustring): Boolean;
    function doOnJsConfirm(const browser: ICefBrowser; const frame: ICefFrame;
      const message: ustring; var retval: Boolean): Boolean;
    function doOnJsPrompt(const browser: ICefBrowser; const frame: ICefFrame;
      const message, defaultValue: ustring; var retval: Boolean;
      var return: ustring): Boolean;
    function doOnJsBinding(const browser: ICefBrowser;
      const frame: ICefFrame; const obj: ICefv8Value): Boolean;
    function doOnFindResult(const browser: ICefBrowser; count: Integer;
      selectionRect: PCefRect; identifier, activeMatchOrdinal,
      finalUpdate: Boolean): Boolean;

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
  end;

  ICefClientHandler = interface
    ['{E76F6888-D9C3-4FCE-9C23-E89659820A36}']
    procedure Disconnect;
  end;

  TCustomClientHandler = class(TCefClientOwn, ICefClientHandler)
  private
    FLifeSpanHandler: ICefBase;
    FLoadHandler: ICefBase;
    FRequestHandler: ICefBase;
    FDisplayHandler: ICefBase;
    FFocusHandler: ICefBase;
    FKeyboardHandler: ICefBase;
    FMenuHandler: ICefBase;
    FPrintHandler: ICefBase;
    FFindHandler: ICefBase;
    FJsdialogHandler: ICefBase;
    FJsbindingHandler: ICefBase;
    FRenderHandler: ICefBase;
    FDragHandler: ICefBase;
  protected
    function GetLifeSpanHandler: ICefBase; override;
    function GetLoadHandler: ICefBase; override;
    function GetRequestHandler: ICefBase; override;
    function GetDisplayHandler: ICefBase; override;
    function GetFocusHandler: ICefBase; override;
    function GetKeyboardHandler: ICefBase; override;
    function GetMenuHandler: ICefBase; override;
    function GetPrintHandler: ICefBase; override;
    function GetFindHandler: ICefBase; override;
    function GetJsdialogHandler: ICefBase; override;
    function GetJsbindingHandler: ICefBase; override;
    function GetRenderHandler: ICefBase; override;
    function GetDragHandler: ICefBase; override;
    procedure Disconnect;
  public
    constructor Create(const crm: IChromiumEvents); reintroduce; virtual;
    destructor Destroy; override;
  end;

  TCustomClientHandlerClass = class of TCustomClientHandler;

  TCustomLifeSpanHandler = class(TCefLifeSpanHandlerOwn)
  private
    FCrm: IChromiumEvents;
  protected
    function OnBeforePopup(const parentBrowser: ICefBrowser;
       var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
       var url: ustring; var client: ICefBase;
       var settings: TCefBrowserSettings): Boolean; override;
    procedure OnAfterCreated(const browser: ICefBrowser); override;
    procedure OnBeforeClose(const browser: ICefBrowser); override;
    function RunModal(const browser: ICefBrowser): Boolean; override;
    function DoClose(const browser: ICefBrowser): Boolean; override;
  public
    constructor Create(const crm: IChromiumEvents); reintroduce;
  end;

  TCustomLoadHandler = class(TCefLoadHandlerOwn)
  private
    FCrm: IChromiumEvents;
  protected
    procedure OnLoadStart(const browser: ICefBrowser; const frame: ICefFrame); override;
    procedure OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer); override;
    function OnLoadError(const browser: ICefBrowser; const frame: ICefFrame;
      errorCode: TCefHandlerErrorcode; const failedUrl: ustring; var errorText: ustring): Boolean; override;
  public
    constructor Create(const crm: IChromiumEvents); reintroduce;
  end;

  TCustomRequestHandler = class(TCefRequestHandlerOwn)
  private
    FCrm: IChromiumEvents;
  protected
    function OnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; navType: TCefHandlerNavtype;
      isRedirect: Boolean): Boolean; override;
    function OnBeforeResourceLoad(const browser: ICefBrowser; const request: ICefRequest;
      var redirectUrl: ustring;  var resourceStream: ICefStreamReader;
      const response: ICefResponse; loadFlags: Integer): Boolean; override;
    procedure OnResourceResponse(const browser: ICefBrowser; const url: ustring;
        const response: ICefResponse; var filter: ICefBase); override;
    function OnProtocolExecution(const browser: ICefBrowser; const url: ustring;
        var allowOSExecution: Boolean): Boolean; override;
    function GetDownloadHandler(const browser: ICefBrowser;
      const mimeType, fileName: ustring; contentLength: int64;
        var handler: ICefDownloadHandler): Boolean; override;
    function GetAuthCredentials(const browser: ICefBrowser;
      isProxy: Boolean; Port: Integer; const host, realm, scheme: ustring;
      var username, password: ustring): Boolean; override;
  public
    constructor Create(const crm: IChromiumEvents); reintroduce;
  end;

  TCustomDisplayHandler = class(TCefDisplayHandlerOwn)
  private
    FCrm: IChromiumEvents;
  protected
    procedure OnNavStateChange(const browser: ICefBrowser;
      canGoBack, canGoForward: Boolean); override;
    procedure OnAddressChange(const browser: ICefBrowser;
      const frame: ICefFrame; const url: ustring); override;
    procedure OnTitleChange(const browser: ICefBrowser;
      const title: ustring); override;
    function OnTooltip(const browser: ICefBrowser;
      var text: ustring): Boolean; override;
    procedure OnStatusMessage(const browser: ICefBrowser; const value: ustring;
        kind: TCefHandlerStatusType); override;
    function OnConsoleMessage(const browser: ICefBrowser; const message,
      source: ustring; line: Integer): Boolean; override;
  public
    constructor Create(const crm: IChromiumEvents); reintroduce;
  end;

  TCustomFocusHandler = class(TCefFocusHandlerOwn)
  private
    FCrm: IChromiumEvents;
  protected
    procedure OnTakeFocus(const browser: ICefBrowser; next: Boolean); override;
    function OnSetFocus(const browser: ICefBrowser; source: TCefHandlerFocusSource): Boolean; override;
  public
    constructor Create(const crm: IChromiumEvents); reintroduce;
  end;

  TCustomKeyboardHandler = class(TCefKeyboardHandlerOwn)
  private
    FCrm: IChromiumEvents;
  protected
    function OnKeyEvent(const browser: ICefBrowser; event: TCefHandlerKeyEventType;
      code, modifiers: Integer; isSystemKey: Boolean): Boolean; override;
  public
    constructor Create(const crm: IChromiumEvents); reintroduce;
  end;

  TCustomMenuHandler = class(TCefMenuHandlerOwn)
  private
    FCrm: IChromiumEvents;
  protected
    function OnBeforeMenu(const browser: ICefBrowser;
      const menuInfo: PCefHandlerMenuInfo): Boolean; override;
    procedure GetMenuLabel(const browser: ICefBrowser;
      menuId: TCefHandlerMenuId; var caption: ustring); override;
    function OnMenuAction(const browser: ICefBrowser;
      menuId: TCefHandlerMenuId): Boolean; override;
  public
    constructor Create(const crm: IChromiumEvents); reintroduce;
  end;

  TCustomPrintHandler = class(TCefPrintHandlerOwn)
  private
    FCrm: IChromiumEvents;
  protected
    function GetPrintOptions(const browser: ICefBrowser;
      printOptions: PCefPrintOptions): Boolean; override;
    function GetPrintHeaderFooter(const browser: ICefBrowser; const frame: ICefFrame;
      const printInfo: PCefPrintInfo; const url, title: ustring; currentPage, maxPages: Integer;
      var topLeft, topCenter, topRight, bottomLeft, bottomCenter, bottomRight: ustring): Boolean; override;
  public
    constructor Create(const crm: IChromiumEvents); reintroduce;
  end;

  TCustomFindHandler = class(TCefFindHandlerOwn)
  private
    FCrm: IChromiumEvents;
  protected
    procedure OnFindResult(const browser: ICefBrowser; count: Integer;
      const selectionRect: PCefRect; identifier, activeMatchOrdinal,
      finalUpdate: Boolean); override;
  public
    constructor Create(const crm: IChromiumEvents); reintroduce;
  end;

  TCustomJsDialogHandler = class(TCefJsDialogHandlerOwn)
  private
    FCrm: IChromiumEvents;
  protected
    function OnJsAlert(const browser: ICefBrowser; const frame: ICefFrame;
      const message: ustring): Boolean; override;
    function OnJsConfirm(const browser: ICefBrowser; const frame: ICefFrame;
      const message: ustring; var retval: Boolean): Boolean; override;
    function OnJsPrompt(const browser: ICefBrowser; const frame: ICefFrame;
      const message, defaultValue: ustring; var retval: Boolean;
      var return: ustring): Boolean; override;
  public
    constructor Create(const crm: IChromiumEvents); reintroduce;
  end;

  TCustomJsBindingHandler = class(TCefJsBindingHandlerOwn)
  private
    FCrm: IChromiumEvents;
  protected
    procedure OnJsBinding(const browser: ICefBrowser;
      const frame: ICefFrame; const obj: ICefv8Value); override;
  public
    constructor Create(const crm: IChromiumEvents); reintroduce;
  end;

  TCustomRenderHandler = class(TCefRenderHandlerOwn)
  private
    FCrm: IChromiumEvents;
  protected
    function GetViewRect(const browser: ICefBrowser; rect: PCefRect): Boolean; override;
    function GetScreenRect(const browser: ICefBrowser; rect: PCefRect): Boolean; override;
    function GetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer;
      screenX, screenY: PInteger): Boolean; override;
    procedure OnPopupShow(const browser: ICefBrowser; show: Boolean); override;
    procedure OnPopupSize(const browser: ICefBrowser; const rect: PCefRect); override;
    procedure OnPaint(const browser: ICefBrowser; kind: TCefPaintElementType;
        const dirtyRect: PCefRect; const buffer: Pointer); override;
    procedure OnCursorChange(const browser: ICefBrowser; cursor: TCefCursorHandle); override;
  public
    constructor Create(const crm: IChromiumEvents); reintroduce;
  end;

  TCustomDragHandler = class(TCefDragHandlerOwn)
  private
    FCrm: IChromiumEvents;
  protected
    function OnDragStart(const browser: ICefBrowser;
      const dragData: ICefDragData; mask: Integer): Boolean; override;
    function OnDragEnter(const browser: ICefBrowser;
      const dragData: ICefDragData; mask: Integer): Boolean; override;
  public
    constructor Create(const crm: IChromiumEvents); reintroduce;
  end;

  TCustomChromiumOSR = class(TComponent, IChromiumEvents)
  private
    FHandler: ICefBase;
    FBrowser: ICefBrowser;
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

    FOnGetViewRect: TOnGetViewRect;
    FOnGetScreenRect: TOnGetScreenRect;
    FOnGetScreenPoint: TOnGetScreenPoint;
    FOnPopupShow: TOnPopupShow;
    FOnPopupSize: TOnPopupSize;
    FOnPaint: TOnPaint;
    FOnCursorChange: TOnCursorChange;

    FOnDragStart: TOnDragEvent;
    FOnDragEnter: TOnDragEvent;

    FOptions: TChromiumOptions;
    FUserStyleSheetLocation: ustring;
    FDefaultEncoding: ustring;
    FFontOptions: TChromiumFontOptions;

    procedure GetSettings(var settings: TCefBrowserSettings);
    procedure CreateBrowser;
  protected
    procedure Loaded; override;
    function GetHandlerClass: TCustomClientHandlerClass; virtual;
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

    function doOnGetViewRect(const browser: ICefBrowser; rect: PCefRect): Boolean; virtual;
    function doOnGetScreenRect(const browser: ICefBrowser; rect: PCefRect): Boolean; virtual;
    function doOnGetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer;
      screenX, screenY: PInteger): Boolean; virtual;
    procedure doOnPopupShow(const browser: ICefBrowser; show: Boolean); virtual;
    procedure doOnPopupSize(const browser: ICefBrowser; const rect: PCefRect); virtual;
    procedure doOnPaint(const browser: ICefBrowser; kind: TCefPaintElementType;
        const dirtyRect: PCefRect; const buffer: Pointer); virtual;
    procedure doOnCursorChange(const browser: ICefBrowser; cursor: TCefCursorHandle); virtual;

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

    property OnGetViewRect: TOnGetViewRect read FOnGetViewRect write FOnGetViewRect;
    property OnGetScreenRect: TOnGetScreenRect read FOnGetScreenRect write FOnGetScreenRect;
    property OnGetScreenPoint: TOnGetScreenPoint read FOnGetScreenPoint write FOnGetScreenPoint;
    property OnPopupShow: TOnPopupShow read FOnPopupShow write FOnPopupShow;
    property OnPopupSize: TOnPopupSize read FOnPopupSize write FOnPopupSize;
    property OnPaint: TOnPaint read FOnPaint write FOnPaint;
    property OnCursorChange: TOnCursorChange read FOnCursorChange write FOnCursorChange;

    property OnDragStart: TOnDragEvent read FOnDragStart write FOnDragStart;
    property OnDragEnter: TOnDragEvent read FOnDragEnter write FOnDragEnter;

    property Options: TChromiumOptions read FOptions write FOptions default [];
    property FontOptions: TChromiumFontOptions read FFontOptions;
    property DefaultEncoding: ustring read FDefaultEncoding write FDefaultEncoding;
    property UserStyleSheetLocation: ustring read FUserStyleSheetLocation write FUserStyleSheetLocation;
    property Browser: ICefBrowser read FBrowser;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load(const url: ustring);
    procedure ReCreateBrowser(const url: string);
  end;


implementation

{ TChromiumFontOptions }

constructor TChromiumFontOptions.Create;
begin
  FStandardFontFamily := '';
  FCursiveFontFamily := '';
  FSansSerifFontFamily := '';
  FMinimumLogicalFontSize := 0;
  FFantasyFontFamily := '';
  FSerifFontFamily := '';
  FDefaultFixedFontSize := 0;
  FDefaultFontSize := 0;
  FRemoteFontsDisabled := False;
  FFixedFontFamily := '';
  FMinimumFontSize := 0;
end;

{ TCefCustomHandler }

constructor TCustomClientHandler.Create(const crm: IChromiumEvents);
begin
  inherited Create;
  FLifeSpanHandler := TCustomLifeSpanHandler.Create(crm);
  FLoadHandler := TCustomLoadHandler.Create(crm);
  FRequestHandler := TCustomRequestHandler.Create(crm);
  FDisplayHandler := TCustomDisplayHandler.Create(crm);
  FFocusHandler := TCustomFocusHandler.Create(crm);
  FKeyboardHandler := TCustomKeyboardHandler.Create(crm);
  FMenuHandler := TCustomMenuHandler.Create(crm);
  FPrintHandler := TCustomPrintHandler.Create(crm);
  FFindHandler := TCustomFindHandler.Create(crm);
  FJsdialogHandler := TCustomJsDialogHandler.Create(crm);
  FJsbindingHandler := TCustomJsBindingHandler.Create(crm);
  FRenderHandler := TCustomRenderHandler.Create(crm);
  FDragHandler := TCustomDragHandler.Create(crm);
//{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
//  if CefInstances = 0 then
//    CefTimer := SetTimer(0, 0, 10, nil);
//  InterlockedIncrement(CefInstances);
//{$ENDIF}
end;

destructor TCustomClientHandler.Destroy;
begin
//{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
//  InterlockedDecrement(CefInstances);
//  if CefInstances = 0 then
//    KillTimer(0, CefTimer);
//{$ENDIF}
  inherited;
end;

procedure TCustomClientHandler.Disconnect;
begin
  FLifeSpanHandler := nil;
  FLoadHandler := nil;
  FRequestHandler := nil;
  FDisplayHandler := nil;
  FFocusHandler := nil;
  FKeyboardHandler := nil;
  FMenuHandler := nil;
  FPrintHandler := nil;
  FFindHandler := nil;
  FJsdialogHandler := nil;
  FJsbindingHandler := nil;
  FRenderHandler := nil;
  FDragHandler := nil;
end;

function TCustomClientHandler.GetDisplayHandler: ICefBase;
begin
  Result := FDisplayHandler;
end;

function TCustomClientHandler.GetDragHandler: ICefBase;
begin
  Result := FDragHandler;
end;

function TCustomClientHandler.GetFindHandler: ICefBase;
begin
  Result := FFindHandler;
end;

function TCustomClientHandler.GetFocusHandler: ICefBase;
begin
  Result := FFocusHandler;
end;

function TCustomClientHandler.GetJsbindingHandler: ICefBase;
begin
  Result := FJsbindingHandler;
end;

function TCustomClientHandler.GetJsdialogHandler: ICefBase;
begin
  Result := FJsdialogHandler;
end;

function TCustomClientHandler.GetKeyboardHandler: ICefBase;
begin
  Result := FKeyboardHandler;
end;

function TCustomClientHandler.GetLifeSpanHandler: ICefBase;
begin
  Result := FLifeSpanHandler;
end;

function TCustomClientHandler.GetLoadHandler: ICefBase;
begin
  Result := FLoadHandler;
end;

function TCustomClientHandler.GetMenuHandler: ICefBase;
begin
  Result := FMenuHandler;
end;

function TCustomClientHandler.GetPrintHandler: ICefBase;
begin
  Result := FPrintHandler;
end;

function TCustomClientHandler.GetRenderHandler: ICefBase;
begin
  Result := FRenderHandler;
end;

function TCustomClientHandler.GetRequestHandler: ICefBase;
begin
  Result := FRequestHandler;
end;

{ TCustomLifeSpanHandler }

constructor TCustomLifeSpanHandler.Create(const crm: IChromiumEvents);
begin
  inherited Create;
  FCrm := crm;
end;

procedure TCustomLifeSpanHandler.OnAfterCreated(const browser: ICefBrowser);
begin
  FCrm.doOnAfterCreated(browser);
end;

procedure TCustomLifeSpanHandler.OnBeforeClose(const browser: ICefBrowser);
begin
  FCrm.doOnBeforeClose(browser);
end;

function TCustomLifeSpanHandler.OnBeforePopup(const parentBrowser: ICefBrowser;
  var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var url: ustring; var client: ICefBase;
  var settings: TCefBrowserSettings): Boolean;
begin
  FCrm.GetSettings(settings);
  Result := FCrm.doOnBeforePopup(parentBrowser, popupFeatures, windowInfo, url, client);
end;

function TCustomLifeSpanHandler.DoClose(const browser: ICefBrowser): Boolean;
begin
  Result := FCrm.doOnClose(browser);
end;

function TCustomLifeSpanHandler.RunModal(const browser: ICefBrowser): Boolean;
begin
  Result := FCrm.doOnRunModal(browser);
end;

{ TCustomLoadHandler }

constructor TCustomLoadHandler.Create(const crm: IChromiumEvents);
begin
  inherited Create;
  FCrm := crm;
end;

procedure TCustomLoadHandler.OnLoadEnd(const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer);
begin
  FCrm.doOnLoadEnd(browser, frame, httpStatusCode);
end;

function TCustomLoadHandler.OnLoadError(const browser: ICefBrowser;
  const frame: ICefFrame; errorCode: TCefHandlerErrorcode; const failedUrl: ustring;
  var errorText: ustring): Boolean;
begin
  Result := FCrm.doOnLoadError(browser, frame, errorCode, failedUrl, errorText);
end;

procedure TCustomLoadHandler.OnLoadStart(const browser: ICefBrowser;
  const frame: ICefFrame);
begin
  FCrm.doOnLoadStart(browser, frame)
end;

{ TCustomRequestHandler }

constructor TCustomRequestHandler.Create(const crm: IChromiumEvents);
begin
  inherited Create;
  FCrm := crm;
end;

function TCustomRequestHandler.GetAuthCredentials(const browser: ICefBrowser;
  isProxy: Boolean; Port: Integer; const host, realm, scheme: ustring; var username,
  password: ustring): Boolean;
begin
  Result := FCrm.doOnAuthCredentials(browser, isProxy, Port, host, realm, scheme, username, password);
end;

function TCustomRequestHandler.GetDownloadHandler(const browser: ICefBrowser;
  const mimeType, fileName: ustring; contentLength: int64;
  var handler: ICefDownloadHandler): Boolean;
begin
  Result := FCrm.doOnGetDownloadHandler(browser, mimeType, fileName, contentLength, handler);
end;

function TCustomRequestHandler.OnBeforeBrowse(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  navType: TCefHandlerNavtype; isRedirect: Boolean): Boolean;
begin
  Result := FCrm.doOnBeforeBrowse(browser, frame, request, navType, isRedirect);
end;

function TCustomRequestHandler.OnBeforeResourceLoad(const browser: ICefBrowser;
  const request: ICefRequest; var redirectUrl: ustring;
  var resourceStream: ICefStreamReader; const response: ICefResponse;
  loadFlags: Integer): Boolean;
begin
  Result := FCrm.doOnBeforeResourceLoad(browser, request, redirectUrl,
    resourceStream, response, loadFlags);
end;

function TCustomRequestHandler.OnProtocolExecution(const browser: ICefBrowser;
  const url: ustring; var allowOSExecution: Boolean): Boolean;
begin
  Result := FCrm.doOnProtocolExecution(browser, url, allowOSExecution);
end;

procedure TCustomRequestHandler.OnResourceResponse(const browser: ICefBrowser;
  const url: ustring; const response: ICefResponse; var filter: ICefBase);
begin
  FCrm.doOnResourceResponse(browser, url, response, filter);
end;

{ TCustomDisplayHandler }

constructor TCustomDisplayHandler.Create(const crm: IChromiumEvents);
begin
  inherited Create;
  FCrm := crm;
end;

procedure TCustomDisplayHandler.OnAddressChange(const browser: ICefBrowser;
  const frame: ICefFrame; const url: ustring);
begin
  FCrm.doOnAddressChange(browser, frame, url);
end;

function TCustomDisplayHandler.OnConsoleMessage(const browser: ICefBrowser;
  const message, source: ustring; line: Integer): Boolean;
begin
  Result := FCrm.doOnConsoleMessage(browser, message, source, line);
end;

procedure TCustomDisplayHandler.OnNavStateChange(const browser: ICefBrowser;
  canGoBack, canGoForward: Boolean);
begin
  FCrm.doOnNavStateChange(browser, canGoBack, canGoForward);
end;

procedure TCustomDisplayHandler.OnStatusMessage(const browser: ICefBrowser;
  const value: ustring; kind: TCefHandlerStatusType);
begin
  FCrm.doOnStatusMessage(browser, value, kind);
end;

procedure TCustomDisplayHandler.OnTitleChange(const browser: ICefBrowser;
  const title: ustring);
begin
  FCrm.doOnTitleChange(browser, title);
end;

function TCustomDisplayHandler.OnTooltip(const browser: ICefBrowser;
  var text: ustring): Boolean;
begin
  Result := FCrm.doOnTooltip(browser, text);
end;

{ TCustomFocusHandler }

constructor TCustomFocusHandler.Create(const crm: IChromiumEvents);
begin
  inherited Create;
  FCrm := crm;
end;

function TCustomFocusHandler.OnSetFocus(const browser: ICefBrowser;
  source: TCefHandlerFocusSource): Boolean;
begin
  Result := FCrm.doOnSetFocus(browser, source)
end;

procedure TCustomFocusHandler.OnTakeFocus(const browser: ICefBrowser;
  next: Boolean);
begin
  FCrm.doOnTakeFocus(browser, next)
end;

{ TCustomKeyboardHandler }

constructor TCustomKeyboardHandler.Create(const crm: IChromiumEvents);
begin
  inherited Create;
  FCrm := crm;
end;

function TCustomKeyboardHandler.OnKeyEvent(const browser: ICefBrowser;
  event: TCefHandlerKeyEventType; code, modifiers: Integer;
  isSystemKey: Boolean): Boolean;
begin
  Result := FCrm.doOnKeyEvent(browser, event, code, modifiers, isSystemKey);
end;

{ TCustomMenuHandler }

constructor TCustomMenuHandler.Create(const crm: IChromiumEvents);
begin
  inherited Create;
  FCrm := crm;
end;

procedure TCustomMenuHandler.GetMenuLabel(const browser: ICefBrowser;
  menuId: TCefHandlerMenuId; var caption: ustring);
begin
  FCrm.doOnGetMenuLabel(browser, menuId, caption);
end;

function TCustomMenuHandler.OnBeforeMenu(const browser: ICefBrowser;
  const menuInfo: PCefHandlerMenuInfo): Boolean;
begin
  Result := FCrm.doOnBeforeMenu(browser, menuInfo);
end;

function TCustomMenuHandler.OnMenuAction(const browser: ICefBrowser;
  menuId: TCefHandlerMenuId): Boolean;
begin
  Result := FCrm.doOnMenuAction(browser, menuId);
end;

{ TCustomPrintHandler }

constructor TCustomPrintHandler.Create(const crm: IChromiumEvents);
begin
  inherited Create;
  FCrm := crm;
end;

function TCustomPrintHandler.GetPrintHeaderFooter(const browser: ICefBrowser;
  const frame: ICefFrame; const printInfo: PCefPrintInfo; const url,
  title: ustring; currentPage, maxPages: Integer; var topLeft, topCenter,
  topRight, bottomLeft, bottomCenter, bottomRight: ustring): Boolean;
begin
  Result := FCrm.doOnPrintHeaderFooter(browser, frame, printInfo, url, title, currentPage,
    maxPages, topLeft, topCenter, topRight, bottomLeft, bottomCenter, bottomRight);
end;

function TCustomPrintHandler.GetPrintOptions(const browser: ICefBrowser;
  printOptions: PCefPrintOptions): Boolean;
begin
  Result := FCrm.doOnPrintOptions(browser, printOptions);
end;

{ TCustomFindHandler }

constructor TCustomFindHandler.Create(const crm: IChromiumEvents);
begin
  inherited Create;
  FCrm := crm;
end;

procedure TCustomFindHandler.OnFindResult(const browser: ICefBrowser;
  count: Integer; const selectionRect: PCefRect; identifier, activeMatchOrdinal,
  finalUpdate: Boolean);
begin
  FCrm.doOnFindResult(browser, count, selectionRect, identifier, activeMatchOrdinal, finalUpdate);
end;

{ TCustomJsDialogHandler }

constructor TCustomJsDialogHandler.Create(const crm: IChromiumEvents);
begin
  inherited Create;
  FCrm := crm;
end;

function TCustomJsDialogHandler.OnJsAlert(const browser: ICefBrowser;
  const frame: ICefFrame; const message: ustring): Boolean;
begin
  Result := FCrm.doOnJsAlert(browser, frame, message);
end;

function TCustomJsDialogHandler.OnJsConfirm(const browser: ICefBrowser;
  const frame: ICefFrame; const message: ustring; var retval: Boolean): Boolean;
begin
  Result := FCrm.doOnJsConfirm(browser, frame, message, retval);
end;

function TCustomJsDialogHandler.OnJsPrompt(const browser: ICefBrowser;
  const frame: ICefFrame; const message, defaultValue: ustring;
  var retval: Boolean; var return: ustring): Boolean;
begin
  Result := FCrm.doOnJsPrompt(browser, frame, message, defaultValue, retval, return);
end;

{ TCustomJsBindingHandler }

constructor TCustomJsBindingHandler.Create(const crm: IChromiumEvents);
begin
  inherited Create;
  FCrm := crm;
end;

procedure TCustomJsBindingHandler.OnJsBinding(const browser: ICefBrowser;
  const frame: ICefFrame; const obj: ICefv8Value);
begin
  FCrm.doOnJsBinding(browser, frame, obj)
end;

{ TCustomRenderHandler }

constructor TCustomRenderHandler.Create(const crm: IChromiumEvents);
begin
  inherited Create;
  FCrm := crm;
end;

function TCustomRenderHandler.GetScreenPoint(const browser: ICefBrowser; viewX,
  viewY: Integer; screenX, screenY: PInteger): Boolean;
begin
  Result := FCrm.doOnGetScreenPoint(browser, viewX, viewY, screenX, screenY);
end;

function TCustomRenderHandler.GetScreenRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := FCrm.doOnGetScreenRect(browser, rect);
end;

function TCustomRenderHandler.GetViewRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := FCrm.doOnGetViewRect(browser, rect);
end;

procedure TCustomRenderHandler.OnCursorChange(const browser: ICefBrowser;
  cursor: TCefCursorHandle);
begin
  FCrm.doOnCursorChange(browser, cursor);
end;

procedure TCustomRenderHandler.OnPaint(const browser: ICefBrowser;
  kind: TCefPaintElementType; const dirtyRect: PCefRect; const buffer: Pointer);
begin
  FCrm.doOnPaint(browser, kind, dirtyRect, buffer);
end;

procedure TCustomRenderHandler.OnPopupShow(const browser: ICefBrowser;
  show: Boolean);
begin
  FCrm.doOnPopupShow(browser, show);
end;

procedure TCustomRenderHandler.OnPopupSize(const browser: ICefBrowser;
  const rect: PCefRect);
begin
  FCrm.doOnPopupSize(browser, rect);
end;

{ TCustomDragHandler }

constructor TCustomDragHandler.Create(const crm: IChromiumEvents);
begin
  inherited Create;
  FCrm := crm;
end;

function TCustomDragHandler.OnDragEnter(const browser: ICefBrowser;
  const dragData: ICefDragData; mask: Integer): Boolean;
begin
  Result := FCrm.doOnDragEnter(browser, dragData, mask);
end;

function TCustomDragHandler.OnDragStart(const browser: ICefBrowser;
  const dragData: ICefDragData; mask: Integer): Boolean;
begin
  Result := FCrm.doOnDragStart(browser, dragData, mask);
end;

{ TCustomChromiumOSR }

constructor TCustomChromiumOSR.Create(AOwner: TComponent);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    FHandler := GetHandlerClass.Create(Self) as ICefBase;

  FOptions := [];
  FFontOptions := TChromiumFontOptions.Create;

  FUserStyleSheetLocation := '';
  FDefaultEncoding := '';

  FBrowser := nil;
end;

procedure TCustomChromiumOSR.CreateBrowser;
var
  info: TCefWindowInfo;
  settings: TCefBrowserSettings;
begin
  if not (csDesigning in ComponentState) then
  begin
    FillChar(info, SizeOf(info), 0);
    info.m_bWindowRenderingDisabled := True;
    FillChar(settings, SizeOf(TCefBrowserSettings), 0);
    settings.size := SizeOf(TCefBrowserSettings);
    GetSettings(settings);
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    CefBrowserCreate(@info, FHandler.Wrap, FDefaultUrl, @settings);
{$ELSE}
    FBrowser := CefBrowserCreateSync(@info, FHandler.Wrap, '', @settings);
{$ENDIF}
  end;
end;

destructor TCustomChromiumOSR.Destroy;
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

function TCustomChromiumOSR.doOnAddressChange(const browser: ICefBrowser;
  const frame: ICefFrame; const url: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnAddressChange) then
    FOnAddressChange(Self, browser, frame, url, Result);
end;

procedure TCustomChromiumOSR.doOnAfterCreated(const browser: ICefBrowser);
begin
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  if (browser <> nil) and not browser.IsPopup then
    FBrowser := browser;
{$ENDIF}
  if Assigned(FOnAfterCreated) then
    FOnAfterCreated(Self, browser);
end;

function TCustomChromiumOSR.doOnAuthCredentials(const browser: ICefBrowser;
  isProxy: Boolean; Port: Integer; const host, realm, scheme: ustring; var username,
  password: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnAuthCredentials) then
    FOnAuthCredentials(Self, browser, isProxy, Port, host, realm, scheme, username, password, Result);
end;

function TCustomChromiumOSR.doOnBeforeBrowse(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  navType: TCefHandlerNavtype; isRedirect: boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforeBrowse) then
    FOnBeforeBrowse(Self, browser, frame, request, navType, isRedirect, Result);
end;

function TCustomChromiumOSR.doOnBeforeMenu(const browser: ICefBrowser;
  const menuInfo: PCefHandlerMenuInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforeMenu) then
    FOnBeforeMenu(Self, browser, menuInfo, Result);
end;

function TCustomChromiumOSR.doOnBeforePopup(const parentBrowser: ICefBrowser;
  var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var url: ustring; var client: ICefBase): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforePopup) then
    FOnBeforePopup(Self, parentBrowser, popupFeatures, windowInfo, url, client, Result);
end;

function TCustomChromiumOSR.doOnBeforeResourceLoad(const browser: ICefBrowser;
  const request: ICefRequest; var redirectUrl: ustring;
  var resourceStream: ICefStreamReader; const response: ICefResponse;
  loadFlags: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforeResourceLoad) then
    FOnBeforeResourceLoad(Self, browser, request, redirectUrl, resourceStream,
      response, loadFlags, Result);
end;

function TCustomChromiumOSR.doOnBeforeClose(
  const browser: ICefBrowser): Boolean;
begin
  Result := False;
  if Assigned(FOnBeforeClose) then
    FOnBeforeClose(Self, browser, Result);
end;

function TCustomChromiumOSR.doOnConsoleMessage(const browser: ICefBrowser; const message,
  source: ustring; line: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnConsoleMessage) then
    FOnConsoleMessage(Self, browser, message, source, line, Result);
end;

function TCustomChromiumOSR.doOnGetDownloadHandler(const browser: ICefBrowser;
  const mimeType, fileName: ustring; contentLength: int64;
  var handler: ICefDownloadHandler): Boolean;
begin
  Result := False;
  if Assigned(FOnGetDownloadHandler) then
    FOnGetDownloadHandler(Self, browser, mimeType, fileName, contentLength, handler, Result);
end;

function TCustomChromiumOSR.doOnFindResult(const browser: ICefBrowser;
  count: Integer; selectionRect: PCefRect; identifier, activeMatchOrdinal,
  finalUpdate: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnFindResult) then
    FOnFindResult(Self, browser, count, selectionRect, identifier,
      activeMatchOrdinal, finalUpdate, Result);
end;

function TCustomChromiumOSR.doOnGetMenuLabel(const browser: ICefBrowser;
  menuId: TCefHandlerMenuId; var caption: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnGetMenuLabel) then
    FOnGetMenuLabel(Self, browser, menuId, caption, Result);
end;

function TCustomChromiumOSR.doOnGetScreenPoint(const browser: ICefBrowser; viewX,
  viewY: Integer; screenX, screenY: PInteger): Boolean;
begin
  Result := False;
  if Assigned(FOnGetScreenPoint) then
    FOnGetScreenPoint(Self, browser, viewX, viewY, screenX, screenY, Result);
end;

function TCustomChromiumOSR.doOnGetScreenRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := False;
  if Assigned(FOnGetScreenRect) then
    FOnGetScreenRect(Self, browser, rect, Result);
end;

function TCustomChromiumOSR.doOnGetViewRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := False;
  if Assigned(FOnGetViewRect) then
    FOnGetViewRect(Self, browser, rect, Result);
end;

function TCustomChromiumOSR.doOnJsAlert(const browser: ICefBrowser;
  const frame: ICefFrame; const message: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnJsAlert) then
    FOnJsAlert(Self, browser, frame, message, Result);
end;

function TCustomChromiumOSR.doOnJsBinding(const browser: ICefBrowser;
  const frame: ICefFrame; const obj: ICefv8Value): Boolean;
begin
  Result := False;
  if Assigned(FOnJsBinding) then
    FOnJsBinding(Self, browser, frame, obj, Result);
end;

function TCustomChromiumOSR.doOnJsConfirm(const browser: ICefBrowser;
  const frame: ICefFrame; const message: ustring;
  var retval: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnJsConfirm) then
    FOnJsConfirm(Self, browser, frame, message, retval, Result);
end;

function TCustomChromiumOSR.doOnJsPrompt(const browser: ICefBrowser;
  const frame: ICefFrame; const message, defaultValue: ustring;
  var retval: Boolean; var return: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnJsPrompt) then
    FOnJsPrompt(Self, browser, frame, message, defaultValue, retval, return, Result);
end;

function TCustomChromiumOSR.doOnKeyEvent(const browser: ICefBrowser;
  event: TCefHandlerKeyEventType; code, modifiers: Integer;
  isSystemKey: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnKeyEvent) then
    FOnKeyEvent(Self, browser, event, code, modifiers, isSystemKey, Result);
end;

function TCustomChromiumOSR.doOnLoadEnd(const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnLoadEnd) then
    FOnLoadEnd(Self, browser, frame, httpStatusCode, Result);
end;

function TCustomChromiumOSR.doOnLoadError(const browser: ICefBrowser;
  const frame: ICefFrame; errorCode: TCefHandlerErrorcode;
  const failedUrl: ustring; var errorText: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnLoadError) then
    FOnLoadError(Self, browser, frame, errorCode, failedUrl, errorText, Result);
end;

procedure TCustomChromiumOSR.doOnLoadStart(const browser: ICefBrowser;
  const frame: ICefFrame);
begin
  if Assigned(FOnLoadStart) then
    FOnLoadStart(Self, browser, frame);
end;

function TCustomChromiumOSR.doOnMenuAction(const browser: ICefBrowser;
  menuId: TCefHandlerMenuId): Boolean;
begin
  Result := False;
  if Assigned(FOnMenuAction) then
    FOnMenuAction(Self, browser, menuId, Result);
end;

function TCustomChromiumOSR.doOnNavStateChange(const browser: ICefBrowser;
  canGoBack, canGoForward: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnNavStateChange) then
    FOnNavStateChange(Self, browser, canGoBack, canGoForward, Result);
end;

procedure TCustomChromiumOSR.doOnCursorChange(const browser: ICefBrowser;
  cursor: TCefCursorHandle);
begin
  if Assigned(FOnCursorChange) then
    FOnCursorChange(Self, browser, cursor);
end;

function TCustomChromiumOSR.doOnDragEnter(const browser: ICefBrowser;
  const dragData: ICefDragData; mask: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnDragEnter) then
    FOnDragEnter(Self, browser, dragData, mask, Result);
end;

function TCustomChromiumOSR.doOnDragStart(const browser: ICefBrowser;
  const dragData: ICefDragData; mask: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnDragStart) then
    FOnDragStart(Self, browser, dragData, mask, Result);
end;

procedure TCustomChromiumOSR.doOnPaint(const browser: ICefBrowser;
  kind: TCefPaintElementType; const dirtyRect: PCefRect; const buffer: Pointer);
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self, browser, kind, dirtyRect, buffer);
end;

procedure TCustomChromiumOSR.doOnPopupShow(const browser: ICefBrowser;
  show: Boolean);
begin
  if Assigned(FOnPopupShow) then
    FOnPopupShow(Self, browser, show);
end;

procedure TCustomChromiumOSR.doOnPopupSize(const browser: ICefBrowser;
  const rect: PCefRect);
begin
  if Assigned(FOnPopupSize) then
    FOnPopupSize(Self, browser, rect);
end;

function TCustomChromiumOSR.doOnPrintHeaderFooter(const browser: ICefBrowser;
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

function TCustomChromiumOSR.doOnPrintOptions(const browser: ICefBrowser;
  printOptions: PCefPrintOptions): Boolean;
begin
  Result := False;
  if Assigned(FOnPrintOptions) then
    FOnPrintOptions(Self, browser, printOptions, Result);
end;

function TCustomChromiumOSR.doOnProtocolExecution(const browser: ICefBrowser;
  const url: ustring; var AllowOsExecution: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnProtocolExecution) then
    FOnProtocolExecution(Self, browser, url, AllowOsExecution, Result);
end;

function TCustomChromiumOSR.doOnClose(const browser: ICefBrowser): Boolean;
begin
  Result := False;
  if Assigned(FOnClose) then
    FOnClose(Self, browser, Result);
end;

procedure TCustomChromiumOSR.doOnResourceResponse(const browser: ICefBrowser;
  const url: ustring; const response: ICefResponse; var filter: ICefBase);
begin
  if Assigned(FOnResourceResponse) then
    FOnResourceResponse(Self, browser, url, response, filter);
end;

function TCustomChromiumOSR.doOnRunModal(const browser: ICefBrowser): Boolean;
begin
  Result := False;
  if Assigned(FOnRunModal) then
    FOnRunModal(Self, browser, Result);
end;

function TCustomChromiumOSR.doOnSetFocus(const browser: ICefBrowser;
  source: TCefHandlerFocusSource): Boolean;
begin
  Result := False;
  if Assigned(FOnSetFocus) then
    FOnSetFocus(Self, browser, source, Result);
end;

function TCustomChromiumOSR.doOnStatusMessage(const browser: ICefBrowser;
  const value: ustring; StatusType: TCefHandlerStatusType): Boolean;
begin
  Result := False;
  if Assigned(FOnStatusMessage) then
    FOnStatusMessage(Self, browser, value, StatusType, Result);
end;

procedure TCustomChromiumOSR.doOnTakeFocus(const browser: ICefBrowser;
  next: Boolean);
begin
  if Assigned(FOnTakeFocus) then
    FOnTakeFocus(Self, browser, next);
end;

function TCustomChromiumOSR.doOnTitleChange(const browser: ICefBrowser;
  const title: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnTitleChange) then
    FOnTitleChange(Self, browser, title, Result);
end;

function TCustomChromiumOSR.doOnTooltip(const browser: ICefBrowser;
  var text: ustring): Boolean;
begin
  Result := False;
  if Assigned(FOnTooltip) then
    FOnTooltip(Self, browser, text, Result);
end;

function TCustomChromiumOSR.GetHandlerClass: TCustomClientHandlerClass;
begin
  Result := TCustomClientHandler;
end;

procedure TCustomChromiumOSR.GetSettings(var settings: TCefBrowserSettings);
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

procedure TCustomChromiumOSR.Load(const url: ustring);
var
  frm: ICefFrame;
begin
  if FBrowser <> nil then
  begin
    frm := FBrowser.MainFrame;
    if frm <> nil then
      frm.LoadUrl(url);
  end;
end;

procedure TCustomChromiumOSR.Loaded;
begin
  inherited;
  CreateBrowser;
  Load(FDefaultUrl);
end;

procedure TCustomChromiumOSR.ReCreateBrowser(const url: string);
begin
  if (FBrowser <> nil) then
  begin
    FBrowser.ParentWindowWillClose;
    FBrowser := nil;

    CreateBrowser;
    Load(url);
  end;
end;


end.
