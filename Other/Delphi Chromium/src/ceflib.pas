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

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}
unit ceflib;
{$ALIGN ON}
{$MINENUMSIZE 4}
{$I cef.inc}

interface
uses
{$IFDEF DELPHI14_UP}
  Rtti, TypInfo, Variants, Generics.Collections,
{$ENDIF}
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  Messages,
{$ENDIF}
  SysUtils, Classes, Windows
{$IFNDEF FPC}
{$ENDIF}
  ;

type
{$IFDEF UNICODE}
  ustring = type string;
  rbstring = type RawByteString;
{$ELSE}
  {$IFDEF FPC}
    {$if defined(unicodestring)}
      ustring = type unicodestring;
    {$else}
      ustring = type WideString;
    {$ifend}
  {$ELSE}
    ustring = type WideString;
  {$ENDIF}
  rbstring = type AnsiString;
{$ENDIF}

{$if not defined(uint64)}
  uint64 = int64;
{$ifend}

  TCefWindowHandle = HWND;
  TCefCursorHandle = HCURSOR;

  // CEF provides functions for converting between UTF-8, -16 and -32 strings.
  // CEF string types are safe for reading from multiple threads but not for
  // modification. It is the user's responsibility to provide synchronization if
  // modifying CEF strings from multiple threads.

  // CEF character type definitions. wchat_t is 2 bytes on Windows and 4 bytes on
  // most other platforms.

  Char16 = WideChar;
  PChar16 = PWideChar;

  // CEF string type definitions. Whomever allocates |str| is responsible for
  // providing an appropriate |dtor| implementation that will free the string in
  // the same memory space. When reusing an existing string structure make sure
  // to call |dtor| for the old value before assigning new |str| and |dtor|
  // values. Static strings will have a NULL |dtor| value. Using the below
  // functions if you want this managed for you.

  PCefStringWide = ^TCefStringWide;
  TCefStringWide = record
    str: PWideChar;
    length: Cardinal;
    dtor: procedure(str: PWideChar); stdcall;
  end;

  PCefStringUtf8 = ^TCefStringUtf8;
  TCefStringUtf8 = record
    str: PAnsiChar;
    length: Cardinal;
    dtor: procedure(str: PAnsiChar); stdcall;
  end;

  PCefStringUtf16 = ^TCefStringUtf16;
  TCefStringUtf16 = record
    str: PChar16;
    length: Cardinal;
    dtor: procedure(str: PChar16); stdcall;
  end;


  // It is sometimes necessary for the system to allocate string structures with
  // the expectation that the user will free them. The userfree types act as a
  // hint that the user is responsible for freeing the structure.

  PCefStringUserFreeWide = ^TCefStringUserFreeWide;
  TCefStringUserFreeWide = type TCefStringWide;

  PCefStringUserFreeUtf8 = ^TCefStringUserFreeUtf8;
  TCefStringUserFreeUtf8 = type TCefStringUtf8;

  PCefStringUserFreeUtf16 = ^TCefStringUserFreeUtf16;
  TCefStringUserFreeUtf16 = type TCefStringUtf16;

{$IFDEF CEF_STRING_TYPE_UTF8}
  TCefChar = AnsiChar;
  PCefChar = PAnsiChar;
  TCefStringUserFree = TCefStringUserFreeUtf8;
  PCefStringUserFree = PCefStringUserFreeUtf8;
  TCefString = TCefStringUtf8;
  PCefString = PCefStringUtf8;
{$ENDIF}

{$IFDEF CEF_STRING_TYPE_UTF16}
  TCefChar = Char16;
  PCefChar = PChar16;
  TCefStringUserFree = TCefStringUserFreeUtf16;
  PCefStringUserFree = PCefStringUserFreeUtf16;
  TCefString = TCefStringUtf16;
  PCefString = PCefStringUtf16;
{$ENDIF}

{$IFDEF CEF_STRING_TYPE_WIDE}
  TCefChar = WideChar;
  PCefChar = PWideChar;
  TCefStringUserFree = TCefStringUserFreeWide;
  PCefStringUserFree = PCefStringUserFreeWide;
  TCefString = TCefStringWide;
  PCefString = PCefStringWide;
{$ENDIF}

  // CEF strings are NUL-terminated wide character strings prefixed with a size
  // value, similar to the Microsoft BSTR type.  Use the below API functions for
  // allocating, managing and freeing CEF strings.

  // CEF string maps are a set of key/value string pairs.
  TCefStringMap = Pointer;

  // CEF string maps are a set of key/value string pairs.
  TCefStringList = Pointer;

  // Supported graphics implementations.
  TCefGraphicsImplementation = (
    ANGLE_IN_PROCESS = 0,
    ANGLE_IN_PROCESS_COMMAND_BUFFER,
    DESKTOP_IN_PROCESS,
    DESKTOP_IN_PROCESS_COMMAND_BUFFER
  );

  // Class representing window information.
  PCefWindowInfo = ^TCefWindowInfo;
  TCefWindowInfo = record
    // Standard parameters required by CreateWindowEx()
    ExStyle: DWORD;
    windowName: TCefString;
    Style: DWORD;
    x: Integer;
    y: Integer;
    Width: Integer;
    Height: Integer;
    WndParent: HWND;
    Menu: HMENU;

    // If window rendering is disabled no browser window will be created. Set
    // |m_hWndParent| to the window that will act as the parent for popup menus,
    // dialog boxes, etc.
    m_bWindowRenderingDisabled: BOOL;

    // Handle for the new browser window.
    Wnd: HWND ;
  end;

  // Class representing print context information.
  TCefPrintInfo = record
    DC: HDC;
    Rect: TRect;
    Scale: double;
  end;

  // Log severity levels.
  TCefLogSeverity = (
    LOGSEVERITY_VERBOSE = -1,
    LOGSEVERITY_INFO,
    LOGSEVERITY_WARNING,
    LOGSEVERITY_ERROR,
    LOGSEVERITY_ERROR_REPORT,
    // Disables logging completely.
    LOGSEVERITY_DISABLE = 99
  );

  // Initialization settings. Specify NULL or 0 to get the recommended default
  // values.
  PCefSettings = ^TCefSettings;
  TCefSettings = record
    // Size of this structure.
    size: Cardinal;

    // Set to true (1) to have the message loop run in a separate thread. If
    // false (0) than the CefDoMessageLoopWork() function must be called from
    // your application message loop.
    multi_threaded_message_loop: Boolean;

    // The location where cache data will be stored on disk. If empty an
    // in-memory cache will be used. HTML5 databases such as localStorage will
    // only persist across sessions if a cache path is specified.
    cache_path: TCefString;

    // Value that will be returned as the User-Agent HTTP header. If empty the
    // default User-Agent string will be used.
    user_agent: TCefString;

    // Value that will be inserted as the product portion of the default
    // User-Agent string. If empty the Chromium product version will be used. If
    // |userAgent| is specified this value will be ignored.
    product_version: TCefString;

    // The locale string that will be passed to WebKit. If empty the default
    // locale of "en-US" will be used.
    locale: TCefString;

    // List of fully qualified paths to plugins (including plugin name) that will
    // be loaded in addition to any plugins found in the default search paths.
    extra_plugin_paths: TCefStringList;

    // The directory and file name to use for the debug log. If empty, the
    // default name of "debug.log" will be used and the file will be written
    // to the application directory.
    log_file: TCefString;

    // The log severity. Only messages of this severity level or higher will be
    // logged.
    log_severity: TCefLogSeverity;

    // The graphics implementation that CEF will use for rendering GPU accelerated
    // content like WebGL, accelerated layers and 3D CSS.
    graphics_implementation: TCefGraphicsImplementation;

    // Quota limit for localStorage data across all origins. Default size is 5MB.
    local_storage_quota: Cardinal;

    // Quota limit for sessionStorage data per namespace. Default size is 5MB.
    session_storage_quota: Cardinal;
  end;

  // Browser initialization settings. Specify NULL or 0 to get the recommended
  // default values. The consequences of using custom values may not be well
  // tested.
  PCefBrowserSettings = ^TCefBrowserSettings;
  TCefBrowserSettings = record
    // Size of this structure.
    size: Cardinal;

    // Disable drag & drop of URLs from other windows.
    drag_drop_disabled: Boolean;

    // The below values map to WebPreferences settings.

    // Font settings.
    standard_font_family: TCefString;
    fixed_font_family: TCefString;
    serif_font_family: TCefString;
    sans_serif_font_family: TCefString;
    cursive_font_family: TCefString;
    fantasy_font_family: TCefString;
    default_font_size: Integer;
    default_fixed_font_size: Integer;
    minimum_font_size: Integer;
    minimum_logical_font_size: Integer;

    // Set to true (1) to disable loading of fonts from remote sources.
    remote_fonts_disabled: Boolean;

    // Default encoding for Web content. If empty "ISO-8859-1" will be used.
    default_encoding: TCefString;

    // Set to true (1) to attempt automatic detection of content encoding.
    encoding_detector_enabled: Boolean;

    // Set to true (1) to disable JavaScript.
    javascript_disabled: Boolean;

    // Set to true (1) to disallow JavaScript from opening windows.
    javascript_open_windows_disallowed: Boolean;

    // Set to true (1) to disallow JavaScript from closing windows.
    javascript_close_windows_disallowed: Boolean;

    // Set to true (1) to disallow JavaScript from accessing the clipboard.
    javascript_access_clipboard_disallowed: Boolean;

    // Set to true (1) to disable DOM pasting in the editor. DOM pasting also
    // depends on |javascript_cannot_access_clipboard| being false (0).
    dom_paste_disabled: Boolean;

    // Set to true (1) to enable drawing of the caret position.
    caret_browsing_enabled: Boolean;

    // Set to true (1) to disable Java.
    java_disabled: Boolean;

    // Set to true (1) to disable plugins.
    plugins_disabled: Boolean;

    // Set to true (1) to allow access to all URLs from file URLs.
    universal_access_from_file_urls_allowed: Boolean;

    // Set to true (1) to allow access to file URLs from other file URLs.
    file_access_from_file_urls_allowed: Boolean;

    // Set to true (1) to allow risky security behavior such as cross-site
    // scripting (XSS). Use with extreme care.
    web_security_disabled: Boolean;

    // Set to true (1) to enable console warnings about XSS attempts.
    xss_auditor_enabled: Boolean;

    // Set to true (1) to suppress the network load of image URLs.  A cached
    // image will still be rendered if requested.
    image_load_disabled: Boolean;

    // Set to true (1) to shrink standalone images to fit the page.
    shrink_standalone_images_to_fit: Boolean;

    // Set to true (1) to disable browser backwards compatibility features.
    site_specific_quirks_disabled: Boolean;

    // Set to true (1) to disable resize of text areas.
    text_area_resize_disabled: Boolean;

    // Set to true (1) to disable use of the page cache.
    page_cache_disabled: Boolean;

    // Set to true (1) to not have the tab key advance focus to links.
    tab_to_links_disabled: Boolean;

    // Set to true (1) to disable hyperlink pings (<a ping> and window.sendPing).
    hyperlink_auditing_disabled: Boolean;

    // Set to true (1) to enable the user style sheet for all pages.
    // |user_style_sheet_location| must be set to the style sheet URL.
    user_style_sheet_enabled: Boolean;
    user_style_sheet_location: TCefString;

    // Set to true (1) to disable style sheets.
    author_and_user_styles_disabled: Boolean;

    // Set to true (1) to disable local storage.
    local_storage_disabled: Boolean;

    // Set to true (1) to disable databases.
    databases_disabled: Boolean;

    // Set to true (1) to disable application cache.
    application_cache_disabled: Boolean;

    // Set to true (1) to disable WebGL.
    webgl_disabled: Boolean;

    // Set to true (1) to enable accelerated compositing. This is turned off by
    // default because the current in-process GPU implementation does not
    // support it correctly.
    accelerated_compositing_enabled: Boolean;

    // Set to true (1) to disable accelerated layers. This affects features like
    // 3D CSS transforms.
    accelerated_layers_disabled: Boolean;

    // Set to true (1) to disable accelerated 2d canvas.
    accelerated_2d_canvas_disabled: Boolean;

    // Set to true (1) to disable developer tools (WebKit inspector).
    developer_tools_disabled: Boolean;
  end;

  // URL component parts.
  PCefUrlParts = ^TCefUrlParts;
  TCefUrlParts = record
    // The complete URL specification.
    spec: TCefString;

    // Scheme component not including the colon (e.g., "http").
    scheme: TCefString;

    // User name component.
    username: TCefString;

    // Password component.
    password: TCefString;

    // Host component. This may be a hostname, an IPv4 address or an IPv6 literal
    // surrounded by square brackets (e.g., "[2001:db8::1]").
    host: TCefString;

    // Port number component.
    port: TCefString;

    // Path component including the first slash following the host.
    path: TCefString;

    // Query string component (i.e., everything following the '?').
    query: TCefString;
  end;

  // Time information. Values should always be in UTC.
  PCefTime = ^TCefTime;
  TCefTime = record
    year: Integer;          // Four digit year "2007"
    month: Integer;         // 1-based month (values 1 = January, etc.)
    day_of_week: Integer;   // 0-based day of week (0 = Sunday, etc.)
    day_of_month: Integer;  // 1-based day of month (1-31)
    hour: Integer;          // Hour within the current day (0-23)
    minute: Integer;        // Minute within the current hour (0-59)
    second: Integer;        // Second within the current minute (0-59 plus leap
                            //   seconds which may take it up to 60).
    millisecond: Integer;   // Milliseconds within the current second (0-999)
  end;

  // Cookie information.
  TCefCookie = record
    // The cookie name.
    name: TCefString;

    // The cookie value.
    value: TCefString;

    // If |domain| is empty a host cookie will be created instead of a domain
    // cookie. Domain cookies are stored with a leading "." and are visible to
    // sub-domains whereas host cookies are not.
    domain: TCefString;

    // If |path| is non-empty only URLs at or below the path will get the cookie
    // value.
    path: TCefString;

    // If |secure| is true the cookie will only be sent for HTTPS requests.
    secure: Boolean;

    // If |httponly| is true the cookie will only be sent for HTTP requests.
    httponly: Boolean;

    // The cookie creation date. This is automatically populated by the system on
    // cookie creation.
    creation: TCefTime;

    // The cookie last access date. This is automatically populated by the system
    // on access.
    last_access: TCefTime;

    // The cookie expiration date is only valid if |has_expires| is true.
    has_expires: Boolean;
    expires: TCefTime;
  end;

  // Storage types.
  TCefStorageType = (
    ST_LOCALSTORAGE = 0,
    ST_SESSIONSTORAGE
  );

  // Mouse button types.

  TCefMouseButtonType = (
    MBT_LEFT   = 0,
    MBT_MIDDLE,
    MBT_RIGHT
  );

  // Key types.
  TCefKeyType = (
    KT_KEYUP    = 0,
    KT_KEYDOWN,
    KT_CHAR
  );

  // Various browser navigation types supported by chrome.
  TCefHandlerNavtype = (
    NAVTYPE_LINKCLICKED = 0,
    NAVTYPE_FORMSUBMITTED,
    NAVTYPE_BACKFORWARD,
    NAVTYPE_RELOAD,
    NAVTYPE_FORMRESUBMITTED,
    NAVTYPE_OTHER
  );

  // Supported error code values. See net\base\net_error_list.h for complete
  // descriptions of the error codes.
  TCefHandlerErrorcode = Integer;

const
  ERR_FAILED = -2;
  ERR_ABORTED = -3;
  ERR_INVALID_ARGUMENT = -4;
  ERR_INVALID_HANDLE = -5;
  ERR_FILE_NOT_FOUND = -6;
  ERR_TIMED_OUT = -7;
  ERR_FILE_TOO_BIG = -8;
  ERR_UNEXPECTED = -9;
  ERR_ACCESS_DENIED = -10;
  ERR_NOT_IMPLEMENTED = -11;
  ERR_CONNECTION_CLOSED = -100;
  ERR_CONNECTION_RESET = -101;
  ERR_CONNECTION_REFUSED = -102;
  ERR_CONNECTION_ABORTED = -103;
  ERR_CONNECTION_FAILED = -104;
  ERR_NAME_NOT_RESOLVED = -105;
  ERR_INTERNET_DISCONNECTED = -106;
  ERR_SSL_PROTOCOL_ERROR = -107;
  ERR_ADDRESS_INVALID = -108;
  ERR_ADDRESS_UNREACHABLE = -109;
  ERR_SSL_CLIENT_AUTH_CERT_NEEDED = -110;
  ERR_TUNNEL_CONNECTION_FAILED = -111;
  ERR_NO_SSL_VERSIONS_ENABLED = -112;
  ERR_SSL_VERSION_OR_CIPHER_MISMATCH = -113;
  ERR_SSL_RENEGOTIATION_REQUESTED = -114;
  ERR_CERT_COMMON_NAME_INVALID = -200;
  ERR_CERT_DATE_INVALID = -201;
  ERR_CERT_AUTHORITY_INVALID = -202;
  ERR_CERT_CONTAINS_ERRORS = -203;
  ERR_CERT_NO_REVOCATION_MECHANISM = -204;
  ERR_CERT_UNABLE_TO_CHECK_REVOCATION = -205;
  ERR_CERT_REVOKED = -206;
  ERR_CERT_INVALID = -207;
  ERR_CERT_END = -208;
  ERR_INVALID_URL = -300;
  ERR_DISALLOWED_URL_SCHEME = -301;
  ERR_UNKNOWN_URL_SCHEME = -302;
  ERR_TOO_MANY_REDIRECTS = -310;
  ERR_UNSAFE_REDIRECT = -311;
  ERR_UNSAFE_PORT = -312;
  ERR_INVALID_RESPONSE = -320;
  ERR_INVALID_CHUNKED_ENCODING = -321;
  ERR_METHOD_NOT_SUPPORTED = -322;
  ERR_UNEXPECTED_PROXY_AUTH = -323;
  ERR_EMPTY_RESPONSE = -324;
  ERR_RESPONSE_HEADERS_TOO_BIG = -325;
  ERR_CACHE_MISS = -400;
  ERR_INSECURE_RESPONSE = -501;

type
  // "Verb" of a drag-and-drop operation as negotiated between the source and
  // destination. These constants match their equivalents in WebCore's
  // DragActions.h and should not be renumbered.
  TCefDragOperations = Integer;
const
    DRAG_OPERATION_NONE    = 0;
    DRAG_OPERATION_COPY    = 1;
    DRAG_OPERATION_LINK    = 2;
    DRAG_OPERATION_GENERIC = 4;
    DRAG_OPERATION_PRIVATE = 8;
    DRAG_OPERATION_MOVE    = 16;
    DRAG_OPERATION_DELETE  = 32;
    DRAG_OPERATION_EVERY   = $FFFFFFFF;

type
  // V8 access control values.
  TCefV8AccessControls = Integer;
const
  V8_ACCESS_CONTROL_DEFAULT               = 0;
  V8_ACCESS_CONTROL_ALL_CAN_READ          = 1;
  V8_ACCESS_CONTROL_ALL_CAN_WRITE         = 1 shl 1;
  V8_ACCESS_CONTROL_PROHIBITS_OVERWRITING = 1 shl 2;

type
  // V8 property attribute values.
  TCefV8PropertyAttributes = Integer;
const
  V8_PROPERTY_ATTRIBUTE_NONE       = 0;       // Writeable, Enumerable, Configurable
  V8_PROPERTY_ATTRIBUTE_READONLY   = 1 shl 0;  // Not writeable
  V8_PROPERTY_ATTRIBUTE_DONTENUM   = 1 shl 1;  // Not enumerable
  V8_PROPERTY_ATTRIBUTE_DONTDELETE = 1 shl 2;  // Not configurable

type
  // Structure representing menu information.
  TCefHandlerMenuInfo = record
    // Values from the cef_handler_menutypebits_t enumeration.
    typeFlags: Integer;
    // If window rendering is enabled |x| and |y| will be in screen coordinates.
    // Otherwise, |x| and |y| will be in view coordinates.
    x: Integer;
    y: Integer;

    linkUrl: TCefString;
    imageUrl: TCefString;
    pageUrl: TCefString;
    frameUrl: TCefString;
    selectionText: TCefString;
    misspelledWord: TCefString;

    // Values from the cef_handler_menucapabilitybits_t enumeration
    editFlags: Integer;
    securityInfo: TCefString;
  end;

  // The TCefHandlerMenuInfo typeFlags value will be a combination of the
  // following values.
  TCefHandlerMenuTypeBits =  Integer;
const
  // No node is selected
  MENUTYPE_NONE = $0;
  // The top page is selected
  MENUTYPE_PAGE = $1;
  // A subframe page is selected
  MENUTYPE_FRAME = $2;
  // A link is selected
  MENUTYPE_LINK = $4;
  // An image is selected
  MENUTYPE_IMAGE = $8;
  // There is a textual or mixed selection that is selected
  MENUTYPE_SELECTION = $10;
  // An editable element is selected
  MENUTYPE_EDITABLE = $20;
  // A misspelled word is selected
  MENUTYPE_MISSPELLED_WORD = $40;
  // A video node is selected
  MENUTYPE_VIDEO = $80;
  // A video node is selected
  MENUTYPE_AUDIO = $100;

type
  // The TCefHandlerMenuInfo editFlags value will be a combination of the
  // following values.
  TCefHandlerMenuCapabilityBits = Integer;
const
    // Values from WebContextMenuData::EditFlags in WebContextMenuData.h
    MENU_CAN_DO_NONE = $0;
    MENU_CAN_UNDO = $1;
    MENU_CAN_REDO = $2;
    MENU_CAN_CUT = $4;
    MENU_CAN_COPY = $8;
    MENU_CAN_PASTE = $10;
    MENU_CAN_DELETE = $20;
    MENU_CAN_SELECT_ALL = $40;
    MENU_CAN_TRANSLATE = $80;
    // Values unique to CEF
    MENU_CAN_GO_FORWARD = $10000000;
    MENU_CAN_GO_BACK = $20000000;

type
  // Supported menu ID values.
  TCefHandlerMenuId = (
    MENU_ID_NAV_BACK = 10,
    MENU_ID_NAV_FORWARD = 11,
    MENU_ID_NAV_RELOAD = 12,
    MENU_ID_NAV_RELOAD_NOCACHE = 13,
    MENU_ID_NAV_STOP = 14,
    MENU_ID_UNDO = 20,
    MENU_ID_REDO = 21,
    MENU_ID_CUT = 22,
    MENU_ID_COPY = 23,
    MENU_ID_PASTE = 24,
    MENU_ID_DELETE = 25,
    MENU_ID_SELECTALL = 26,
    MENU_ID_PRINT = 30,
    MENU_ID_VIEWSOURCE = 31
  );

  TCefPaintElementType = (
    PET_VIEW  = 0,
    PET_POPUP
  );

  // Post data elements may represent either bytes or files.
  TCefPostDataElementType = (
    PDE_TYPE_EMPTY  = 0,
    PDE_TYPE_BYTES,
    PDE_TYPE_FILE
  );


type
  TCefWebUrlRequestFlags = Integer;
const
    WUR_FLAG_NONE = 0;
    WUR_FLAG_SKIP_CACHE = $1;
    WUR_FLAG_ALLOW_CACHED_CREDENTIALS = $2;
    WUR_FLAG_ALLOW_COOKIES = $4;
    WUR_FLAG_REPORT_UPLOAD_PROGRESS = $8;
    WUR_FLAG_REPORT_LOAD_TIMING = $10;
    WUR_FLAG_REPORT_RAW_HEADERS = $20;

type
  TCefWebUrlRequestState = (
    WUR_STATE_UNSENT = 0,
    WUR_STATE_STARTED = 1,
    WUR_STATE_HEADERS_RECEIVED = 2,
    WUR_STATE_LOADING = 3,
    WUR_STATE_DONE = 4,
    WUR_STATE_ERROR = 5,
    WUR_STATE_ABORT = 6
  );

  // Focus sources.
  TCefHandlerFocusSource = (
    // The source is explicit navigation via the API (LoadURL(), etc).
    FOCUS_SOURCE_NAVIGATION = 0,
    // The source is a system-generated focus event.
    FOCUS_SOURCE_SYSTEM,
    // The source is a child widget of the browser window requesting focus.
    FOCUS_SOURCE_WIDGET
  );


  // Key event types.
  TCefHandlerKeyEventType = (
    KEYEVENT_RAWKEYDOWN = 0,
    KEYEVENT_KEYDOWN,
    KEYEVENT_KEYUP,
    KEYEVENT_CHAR
  );

  // Key event modifiers.
  TCefHandlerKeyEventModifiers = Integer;
const
  KEY_SHIFT = 1 shl 0;
  KEY_CTRL  = 1 shl 1;
  KEY_ALT   = 1 shl 2;
  KEY_META  = 1 shl 3;

type
  // Structure representing a rectangle.
  PCefRect = ^TCefRect;
  TCefRect = record
    x: Integer;
    y: Integer;
    width: Integer;
    height: Integer;
  end;

  // Existing thread IDs.
  TCefThreadId = (
    TID_UI      = 0,
    TID_IO      = 1,
    TID_FILE    = 2
  );

  // Paper type for printing.
  TCefPaperType = (
    PT_LETTER = 0,
    PT_LEGAL,
    PT_EXECUTIVE,
    PT_A3,
    PT_A4,
    PT_CUSTOM
  );

  // Paper metric information for printing.
  TCefPaperMetrics = record
    paper_type: TCefPaperType;
    //Length and width needed if paper_type is custom_size
    //Units are in inches.
    length: Double;
    width: Double;
  end;

  // Paper print margins.
  TCefPrintMargins = record
    //Margin size in inches for left/right/top/bottom (this is content margins).
    left: Double;
    right: Double;
    top: Double;
    bottom: Double;
    //Margin size (top/bottom) in inches for header/footer.
    header: Double;
    footer: Double;
  end;

  // Page orientation for printing
  TCefPageOrientation = (
    PORTRAIT = 0,
    LANDSCAPE
  );

  // Printing options.
  PCefPrintOptions = ^TCefPrintOptions;
  TCefPrintOptions = record
    page_orientation: TCefPageOrientation;
    paper_metrics: TCefPaperMetrics;
    paper_margins: TCefPrintMargins;
  end;

  // Supported XML encoding types. The parser supports ASCII, ISO-8859-1, and
  // UTF16 (LE and BE) by default. All other types must be translated to UTF8
  // before being passed to the parser. If a BOM is detected and the correct
  // decoder is available then that decoder will be used automatically.
  TCefXmlEncodingType = (
    XML_ENCODING_NONE = 0,
    XML_ENCODING_UTF8,
    XML_ENCODING_UTF16LE,
    XML_ENCODING_UTF16BE,
    XML_ENCODING_ASCII
  );

  // XML node types.
  TCefXmlNodeType = (
    XML_NODE_UNSUPPORTED = 0,
    XML_NODE_PROCESSING_INSTRUCTION,
    XML_NODE_DOCUMENT_TYPE,
    XML_NODE_ELEMENT_START,
    XML_NODE_ELEMENT_END,
    XML_NODE_ATTRIBUTE,
    XML_NODE_TEXT,
    XML_NODE_CDATA,
    XML_NODE_ENTITY_REFERENCE,
    XML_NODE_WHITESPACE,
    XML_NODE_COMMENT
  );

  // Status message types.
  TCefHandlerStatusType = (
    STATUSTYPE_TEXT = 0,
    STATUSTYPE_MOUSEOVER_URL,
    STATUSTYPE_KEYBOARD_FOCUS_URL
  );

  // Popup window features.
  PCefPopupFeatures = ^TCefPopupFeatures;
  TCefPopupFeatures = record
    x: Integer;
    xSet: Boolean;
    y: Integer;
    ySet: Boolean;
    width: Integer;
    widthSet: Boolean;
    height: Integer;
    heightSet: Boolean;

    menuBarVisible: Boolean;
    statusBarVisible: Boolean;
    toolBarVisible: Boolean;
    locationBarVisible: Boolean;
    scrollbarsVisible: Boolean;
    resizable: Boolean;

    fullscreen: Boolean;
    dialog: Boolean;
    additionalFeatures: TCefStringList;
  end;

  // DOM document types.
  TCefDomDocumentType = (
    DOM_DOCUMENT_TYPE_UNKNOWN = 0,
    DOM_DOCUMENT_TYPE_HTML,
    DOM_DOCUMENT_TYPE_XHTML,
    DOM_DOCUMENT_TYPE_PLUGIN
  );

  // DOM event category flags.
  TCefDomEventCategory = Integer;
const
  DOM_EVENT_CATEGORY_UNKNOWN = $0;
  DOM_EVENT_CATEGORY_UI = $1;
  DOM_EVENT_CATEGORY_MOUSE = $2;
  DOM_EVENT_CATEGORY_MUTATION = $4;
  DOM_EVENT_CATEGORY_KEYBOARD = $8;
  DOM_EVENT_CATEGORY_TEXT = $10;
  DOM_EVENT_CATEGORY_COMPOSITION = $20;
  DOM_EVENT_CATEGORY_DRAG = $40;
  DOM_EVENT_CATEGORY_CLIPBOARD = $80;
  DOM_EVENT_CATEGORY_MESSAGE = $100;
  DOM_EVENT_CATEGORY_WHEEL = $200;
  DOM_EVENT_CATEGORY_BEFORE_TEXT_INSERTED = $400;
  DOM_EVENT_CATEGORY_OVERFLOW = $800;
  DOM_EVENT_CATEGORY_PAGE_TRANSITION = $1000;
  DOM_EVENT_CATEGORY_POPSTATE = $2000;
  DOM_EVENT_CATEGORY_PROGRESS = $4000;
  DOM_EVENT_CATEGORY_XMLHTTPREQUEST_PROGRESS = $8000;
  DOM_EVENT_CATEGORY_WEBKIT_ANIMATION = $10000;
  DOM_EVENT_CATEGORY_WEBKIT_TRANSITION = $20000;
  DOM_EVENT_CATEGORY_BEFORE_LOAD = $40000;

type
  // DOM event processing phases.
  TCefDomEventPhase = (
    DOM_EVENT_PHASE_UNKNOWN = 0,
    DOM_EVENT_PHASE_CAPTURING,
    DOM_EVENT_PHASE_AT_TARGET,
    DOM_EVENT_PHASE_BUBBLING
  );

  // DOM node types.
  TCefDomNodeType = (
    DOM_NODE_TYPE_UNSUPPORTED = 0,
    DOM_NODE_TYPE_ELEMENT,
    DOM_NODE_TYPE_ATTRIBUTE,
    DOM_NODE_TYPE_TEXT,
    DOM_NODE_TYPE_CDATA_SECTION,
    DOM_NODE_TYPE_ENTITY_REFERENCE,
    DOM_NODE_TYPE_ENTITY,
    DOM_NODE_TYPE_PROCESSING_INSTRUCTIONS,
    DOM_NODE_TYPE_COMMENT,
    DOM_NODE_TYPE_DOCUMENT,
    DOM_NODE_TYPE_DOCUMENT_TYPE,
    DOM_NODE_TYPE_DOCUMENT_FRAGMENT,
    DOM_NODE_TYPE_NOTATION,
    DOM_NODE_TYPE_XPATH_NAMESPACE
  );

(*******************************************************************************
   capi
 *******************************************************************************)
type
  PCefv8Handler = ^TCefv8Handler;
  PCefV8Accessor = ^TCefV8Accessor;
  PCefv8Value = ^TCefv8Value;
  PCefV8ValueArray = array[0..(High(Integer) div SizeOf(Integer)) - 1] of PCefV8Value;
  PPCefV8Value = ^PCefV8ValueArray;
  PCefSchemeHandlerFactory = ^TCefSchemeHandlerFactory;
  PCefSchemeHandlerCallback = ^TCefSchemeHandlerCallback;
//  PCefHandler = ^TCefHandler;
  PCefFrame = ^TCefFrame;
  PCefRequest = ^TCefRequest;
  PCefStreamReader = ^TCefStreamReader;
  PCefHandlerMenuInfo = ^TCefHandlerMenuInfo;
  PCefPrintInfo = ^TCefPrintInfo;
  PCefPostData = ^TCefPostData;
  PCefPostDataElement = ^TCefPostDataElement;
  PCefReadHandler = ^TCefReadHandler;
  PCefWriteHandler = ^TCefWriteHandler;
  PCefStreamWriter = ^TCefStreamWriter;
  PCefSchemeHandler = ^TCefSchemeHandler;
  PCefBase = ^TCefBase;
  PCefBrowser = ^TCefBrowser;
  PCefTask = ^TCefTask;
  PCefDownloadHandler = ^TCefDownloadHandler;
  PCefXmlReader = ^TCefXmlReader;
  PCefZipReader = ^TCefZipReader;
  PCefDomVisitor = ^TCefDomVisitor;
  PCefDomDocument = ^TCefDomDocument;
  PCefDomNode = ^TCefDomNode;
  PCefDomEventListener = ^TCefDomEventListener;
  PCefDomEvent = ^TCefDomEvent;
  PCefResponse = ^TCefResponse;
  PCefv8Context = ^TCefv8Context;
  PCefWebUrlRequest = ^TCefWebUrlRequest;
  PCefWebUrlRequestClient = ^TCefWebUrlRequestClient;
  PCefCookieVisitor = ^TCefCookieVisitor;
  PCefCookie = ^TCefCookie;
  PCefClient = ^TCefClient;
  PCefLifeSpanHandler = ^TCefLifeSpanHandler;
  PCefLoadHandler = ^TCefLoadHandler;
  PCefRequestHandler = ^TCefRequestHandler;
  PCefContentFilter = ^TCefContentFilter;
  PCefDisplayHandler = ^TCefDisplayHandler;
  PCefFocusHandler = ^TCefFocusHandler;
  PCefKeyboardHandler = ^TCefKeyboardHandler;
  PCefMenuHandler = ^TCefMenuHandler;
  PCefPrintHandler = ^TCefPrintHandler;
  PCefFindHandler = ^TCefFindHandler;
  PCefJsDialogHandler = ^TCefJsDialogHandler;
  PCefJsBindingHandler = ^TCefJsBindingHandler;
  PCefRenderHandler = ^TCefRenderHandler;
  PCefDragHandler = ^TCefDragHandler;
  PCefDragData = ^TCefDragData;
  PCefStorageVisitor = ^TCefStorageVisitor;

  TCefBase = record
    // Size of the data structure.
    size: Cardinal;

    // Increment the reference count.
    add_ref: function(self: PCefBase): Integer; stdcall;
    // Decrement the reference count.  Delete this object when no references
    // remain.
    release: function(self: PCefBase): Integer; stdcall;
    // Returns the current number of references.
    get_refct: function(self: PCefBase): Integer; stdcall;
  end;

  // Implement this structure for task execution. The functions of this structure
  // may be called on any thread.
  TCefTask = record
    // Base structure.
    base: TCefBase;
    // Method that will be executed. |threadId| is the thread executing the call.
    execute: procedure(self: PCefTask; threadId: TCefThreadId); stdcall;
  end;

  // Structure used to represent a browser window. The functions of this structure
  // may be called on any thread unless otherwise indicated in the comments.
  TCefBrowser = record
    // Base structure.
    base: TCefBase;

    // Call this function before destroying a contained browser window. This
    // function performs any internal cleanup that may be needed before the
    // browser window is destroyed.
    parent_window_will_close: procedure(self: PCefBrowser); stdcall;

    // Closes this browser window.
    close_browser: procedure(self: PCefBrowser); stdcall;

    // Returns true (1) if the browser can navigate backwards.
    can_go_back: function(self: PCefBrowser): Integer; stdcall;

    // Navigate backwards.
    go_back: procedure(self: PCefBrowser); stdcall;

    // Returns true (1) if the browser can navigate forwards.
    can_go_forward: function(self: PCefBrowser): Integer; stdcall;

    // Navigate forwards.
    go_forward: procedure(self: PCefBrowser); stdcall;

    // Reload the current page.
    reload: procedure(self: PCefBrowser); stdcall;

    // Reload the current page ignoring any cached data.
    reload_ignore_cache: procedure(self: PCefBrowser); stdcall;

    // Stop loading the page.
    stop_load: procedure(self: PCefBrowser); stdcall;

    // Set focus for the browser window. If |enable| is true (1) focus will be set
    // to the window. Otherwise, focus will be removed.
    set_focus: procedure(self: PCefBrowser; enable: Integer); stdcall;

    // Retrieve the window handle for this browser.
    get_window_handle: function(self: PCefBrowser): TCefWindowHandle; stdcall;

    // Retrieve the window handle of the browser that opened this browser. Will
    // return NULL for non-popup windows. This function can be used in combination
    // with custom handling of modal windows.
    get_opener_window_handle: function(self: PCefBrowser): TCefWindowHandle; stdcall;

    // Returns true (1) if the window is a popup window.
    is_popup: function(self: PCefBrowser): Integer; stdcall;

    // Returns true (1) if a document has been loaded in the browser.
    has_document: function(self: PCefBrowser): Integer; stdcall;

    // Returns the client for this browser.
    get_client: function(self: PCefBrowser): PCefClient; stdcall;

    // Returns the main (top-level) frame for the browser window.
    get_main_frame: function(self: PCefBrowser): PCefFrame; stdcall;

    // Returns the focused frame for the browser window. This function should only
    // be called on the UI thread.
    get_focused_frame: function (self: PCefBrowser): PCefFrame; stdcall;

    // Returns the frame with the specified name, or NULL if not found. This
    // function should only be called on the UI thread.
    get_frame: function(self: PCefBrowser; const name: PCefString): PCefFrame; stdcall;

    // Returns the names of all existing frames. This function should only be
    // called on the UI thread.
    get_frame_names: procedure(self: PCefBrowser; names: TCefStringList); stdcall;

    // Search for |searchText|. |identifier| can be used to have multiple searches
    // running simultaniously. |forward| indicates whether to search forward or
    // backward within the page. |matchCase| indicates whether the search should
    // be case-sensitive. |findNext| indicates whether this is the first request
    // or a follow-up.
    find: procedure(self: PCefBrowser; identifier: Integer; const searchText: PCefString;
      forward, matchCase, findNext: Integer); stdcall;

    // Cancel all searches that are currently going on.
    stop_finding: procedure(self: PCefBrowser; clearSelection: Integer); stdcall;

    // Get the zoom level.
    get_zoom_level: function(self: PCefBrowser): Double; stdcall;

    // Change the zoom level to the specified value.
    set_zoom_level: procedure(self: PCefBrowser; zoomLevel: Double); stdcall;

    // Clear the back/forward browsing history.
    clear_history: procedure(self: PCefBrowser); stdcall;

    // Open developer tools in its own window.
    show_dev_tools: procedure(self: PCefBrowser); stdcall;

    // Explicitly close the developer tools window if one exists for this browser
    // instance.
    close_dev_tools: procedure(self: PCefBrowser); stdcall;

    // Returns true (1) if window rendering is disabled.
    is_window_rendering_disabled: function(self: PCefBrowser): Integer; stdcall;

    // Get the size of the specified element. This function should only be called
    // on the UI thread.
    get_size: function(self: PCefBrowser; kind: TCefPaintElementType; width, height: PInteger): Integer; stdcall;

    // Set the size of the specified element. This function is only used when
    // window rendering is disabled.
    set_size: procedure(self: PCefBrowser; kind: TCefPaintElementType; width, height: Integer); stdcall;

    // Returns true (1) if a popup is currently visible. This function should only
    // be called on the UI thread.
    is_popup_visible: function(self: PCefBrowser): Integer; stdcall;

    // Hide the currently visible popup, if any.
    hide_popup: procedure(self: PCefBrowser); stdcall;

    // Invalidate the |dirtyRect| region of the view. This function is only used
    // when window rendering is disabled and will result in a call to
    // HandlePaint().
    invalidate: procedure(self: PCefBrowser; const dirtyRect: PCefRect); stdcall;

    // Get the raw image data contained in the specified element without
    // performing validation. The specified |width| and |height| dimensions must
    // match the current element size. On Windows |buffer| must be width*height*4
    // bytes in size and represents a BGRA image with an upper-left origin. This
    // function should only be called on the UI thread.
    get_image: function(self: PCefBrowser; kind: TCefPaintElementType;
      width, height: Integer; buffer: Pointer): Integer; stdcall;

    // Send a key event to the browser.
    send_key_event: procedure(self: PCefBrowser; kind: TCefKeyType; key, modifiers,
      sysChar, imeChar: Integer); stdcall;

    // Send a mouse click event to the browser. The |x| and |y| coordinates are
    // relative to the upper-left corner of the view.
    send_mouse_click_event: procedure(self: PCefBrowser;
      x, y: Integer; kind: TCefMouseButtonType; mouseUp, clickCount: Integer); stdcall;

    // Send a mouse move event to the browser. The |x| and |y| coordinates are
    // relative to the upper-left corner of the view.
    send_mouse_move_event: procedure(self: PCefBrowser; x, y, mouseLeave: Integer); stdcall;

    // Send a mouse wheel event to the browser. The |x| and |y| coordinates are
    // relative to the upper-left corner of the view.
    send_mouse_wheel_event: procedure(self: PCefBrowser; x, y, delta: Integer); stdcall;

    // Send a focus event to the browser.
    send_focus_event: procedure(self: PCefBrowser; setFocus: Integer); stdcall;

    // Send a capture lost event to the browser.
    send_capture_lost_event: procedure(self: PCefBrowser); stdcall;
  end;

  // Structure used to represent a frame in the browser window. The functions of
  // this structure may be called on any thread unless otherwise indicated in the
  // comments.
  TCefFrame = record
    // Base structure.
    base: TCefBase;

    // Execute undo in this frame.
    undo: procedure(self: PCefFrame); stdcall;

    // Execute redo in this frame.
    redo: procedure(self: PCefFrame); stdcall;

    // Execute cut in this frame.
    cut: procedure(self: PCefFrame); stdcall;

    // Execute copy in this frame.
    copy: procedure(self: PCefFrame); stdcall;

    // Execute paste in this frame.
    paste: procedure(self: PCefFrame); stdcall;

    // Execute delete in this frame.
    del: procedure(self: PCefFrame); stdcall;

    // Execute select all in this frame.
    select_all: procedure(self: PCefFrame); stdcall;

    // Execute printing in the this frame.  The user will be prompted with the
    // print dialog appropriate to the operating system.
    print: procedure(self: PCefFrame); stdcall;

    // Save this frame's HTML source to a temporary file and open it in the
    // default text viewing application.
    view_source: procedure(self: PCefFrame); stdcall;

    // Returns this frame's HTML source as a string. This function should only be
    // called on the UI thread.
    get_source: function(self: PCefFrame): PCefStringUserFree; stdcall;

    // Returns this frame's display text as a string. This function should only be
    // called on the UI thread.
    get_text: function(self: PCefFrame): PCefStringUserFree; stdcall;

    // Load the request represented by the |request| object.
    load_request: procedure(self: PCefFrame; request: PCefRequest); stdcall;

    // Load the specified |url|.
    load_url: procedure(self: PCefFrame; const url: PCefString); stdcall;

    // Load the contents of |string| with the optional dummy target |url|.
    load_string: procedure(self: PCefFrame; const string_, url: PCefString); stdcall;

    // Load the contents of |stream| with the optional dummy target |url|.
    load_stream: procedure(self: PCefFrame; stream: PCefStreamReader; const url: PCefString); stdcall;

    // Execute a string of JavaScript code in this frame. The |script_url|
    // parameter is the URL where the script in question can be found, if any. The
    // renderer may request this URL to show the developer the source of the
    // error.  The |start_line| parameter is the base line number to use for error
    // reporting.
    execute_java_script: procedure(self: PCefFrame; const jsCode, scriptUrl: PCefString; startLine: Integer); stdcall;

    // Returns true (1) if this is the main frame.
    is_main: function(self: PCefFrame): Integer; stdcall;

    // Returns true (1) if this is the focused frame. This function should only be
    // called on the UI thread.
    is_focused: function(self: PCefFrame): Integer; stdcall;

    // Returns this frame's name.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_name: function(self: PCefFrame): PCefStringUserFree; stdcall;

    // Returns the URL currently loaded in this frame. This function should only
    // be called on the UI thread.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_url: function(self: PCefFrame): PCefStringUserFree; stdcall;

    // Returns the browser that this frame belongs to.
    get_browser: function(self: PCefFrame): PCefBrowser; stdcall;

    // Visit the DOM document.
    visit_dom: procedure(self: PCefFrame; visitor: PCefDomVisitor); stdcall;
  end;


  // Implement this structure to handle events related to browser life span. The
  // functions of this structure will be called on the UI thread.
  TCefLifeSpanHandler = record
    // Base structure.
    base: TCefBase;

    // Called before a new popup window is created. The |parentBrowser| parameter
    // will point to the parent browser window. The |popupFeatures| parameter will
    // contain information about the style of popup window requested. Return false
    // (0) to have the framework create the new popup window based on the
    // parameters in |windowInfo|. Return true (1) to cancel creation of the popup
    // window. By default, a newly created popup window will have the same client
    // and settings as the parent window. To change the client for the new window
    // modify the object that |client| points to. To change the settings for the
    // new window modify the |settings| structure.
    on_before_popup: function(self: PCefLifeSpanHandler; parentBrowser: PCefBrowser;
       const popupFeatures: PCefPopupFeatures; windowInfo: PCefWindowInfo;
       const url: PCefString; var client: PCefClient;
       settings: PCefBrowserSettings): Integer; stdcall;

    // Called after a new window is created.
    on_after_created: procedure(self: PCefLifeSpanHandler; browser: PCefBrowser); stdcall;

    // Called when a modal window is about to display and the modal loop should
    // begin running. Return false (0) to use the default modal loop
    // implementation or true (1) to use a custom implementation.
    run_modal: function(self: PCefLifeSpanHandler; browser: PCefBrowser): Integer; stdcall;

    // Called when a window has recieved a request to close. Return false (0) to
    // proceed with the window close or true (1) to cancel the window close. If
    // this is a modal window and a custom modal loop implementation was provided
    // in run_modal() this callback should be used to restore the opener window to
    // a usable state.
    do_close: function(self: PCefLifeSpanHandler; browser: PCefBrowser): Integer; stdcall;

    // Called just before a window is closed. If this is a modal window and a
    // custom modal loop implementation was provided in run_modal() this callback
    // should be used to exit the custom modal loop.
    on_before_close: procedure(self: PCefLifeSpanHandler; browser: PCefBrowser); stdcall;
  end;

  // Implement this structure to handle events related to browser load status. The
  // functions of this structure will be called on the UI thread.
  TCefLoadHandler = record
    // Base structure.
    base: TCefBase;

    // Called when the browser begins loading a frame. The |frame| value will
    // never be NULL -- call the is_main() function to check if this frame is the
    // main frame. Multiple frames may be loading at the same time. Sub-frames may
    // start or continue loading after the main frame load has ended. This
    // function may not be called for a particular frame if the load request for
    // that frame fails.
    on_load_start: procedure(self: PCefLoadHandler;
      browser: PCefBrowser; frame: PCefFrame); stdcall;

    // Called when the browser is done loading a frame. The |frame| value will
    // never be NULL -- call the is_main() function to check if this frame is the
    // main frame. Multiple frames may be loading at the same time. Sub-frames may
    // start or continue loading after the main frame load has ended. This
    // function will always be called for all frames irrespective of whether the
    // request completes successfully.
    on_load_end: procedure(self: PCefLoadHandler; browser: PCefBrowser;
      frame: PCefFrame; httpStatusCode: Integer); stdcall;

    // Called when the browser fails to load a resource. |errorCode| is the error
    // code number and |failedUrl| is the URL that failed to load. To provide
    // custom error text assign the text to |errorText| and return true (1).
    // Otherwise, return false (0) for the default error text. See
    // net\base\net_error_list.h for complete descriptions of the error codes.
    on_load_error: function(self: PCefLoadHandler;
        browser: PCefBrowser; frame: PCefFrame;
        errorCode: TCefHandlerErrorcode; const failedUrl: PCefString;
        var errorText: TCefString): Integer; stdcall;
  end;

  // Implement this structure to handle events related to browser requests. The
  // functions of this structure will be called on the thread indicated.
  TCefRequestHandler = record
    // Base structure.
    base: TCefBase;

    // Called on the UI thread before browser navigation. Return true (1) to
    // cancel the navigation or false (0) to allow the navigation to proceed.
    on_before_browse: function(self: PCefRequestHandler;
        browser: PCefBrowser; frame: PCefFrame;
        request: PCefRequest; navType: TCefHandlerNavtype;
        isRedirect: Integer): Integer; stdcall;

    // Called on the IO thread before a resource is loaded.  To allow the resource
    // to load normally return false (0). To redirect the resource to a new url
    // populate the |redirectUrl| value and return false (0).  To specify data for
    // the resource return a CefStream object in |resourceStream|, use the
    // |response| object to set mime type, HTTP status code and optional header
    // values, and return false (0). To cancel loading of the resource return true
    // (1). Any modifications to |request| will be observed.  If the URL in
    // |request| is changed and |redirectUrl| is also set, the URL in |request|
    // will be used.
    on_before_resource_load: function(
        self: PCefRequestHandler; browser: PCefBrowser;
        request: PCefRequest; redirectUrl: PCefString;
        var resourceStream: PCefStreamReader;
        response: PCefResponse; loadFlags: Integer): Integer; stdcall;

    // Called on the UI thread after a response to the resource request is
    // received. Set |filter| if response content needs to be monitored and/or
    // modified as it arrives.
    on_resource_response: procedure(self: PCefRequestHandler;
        browser: PCefBrowser; const url: PCefString;
        response: PCefResponse; var filter: PCefContentFilter); stdcall;

    // Called on the IO thread to handle requests for URLs with an unknown
    // protocol component. Return true (1) to indicate that the request should
    // succeed because it was handled externally. Set |allowOSExecution| to true
    // (1) and return false (0) to attempt execution via the registered OS
    // protocol handler, if any. If false (0) is returned and either
    // |allow_os_execution| is false (0) or OS protocol handler execution fails
    // then the request will fail with an error condition. SECURITY WARNING: YOU
    // SHOULD USE THIS METHOD TO ENFORCE RESTRICTIONS BASED ON SCHEME, HOST OR
    // OTHER URL ANALYSIS BEFORE ALLOWING OS EXECUTION.
    on_protocol_execution: function(self: PCefRequestHandler;
        browser: PCefBrowser; const url: PCefString;
        var allowOSExecution: Integer): Integer; stdcall;

    // Called on the UI thread when a server indicates via the 'Content-
    // Disposition' header that a response represents a file to download.
    // |mimeType| is the mime type for the download, |fileName| is the suggested
    // target file name and |contentLength| is either the value of the 'Content-
    // Size' header or -1 if no size was provided. Set |handler| to the
    // cef_download_handler_t instance that will recieve the file contents. Return
    // true (1) to download the file or false (0) to cancel the file download.
    get_download_handler: function(self: PCefRequestHandler;
        browser: PCefBrowser; const mimeType: PCefString;
        const fileName: PCefString; contentLength: int64;
        var handler: PCefDownloadHandler): Integer; stdcall;

    // Called on the IO thread when the browser needs credentials from the user.
    // |isProxy| indicates whether the host is a proxy server. |host| contains the
    // hostname and port number. Set |username| and |password| and return true (1)
    // to handle the request. Return false (0) to cancel the request.
    get_auth_credentials: function(self: PCefRequestHandler;
        browser: PCefBrowser; isProxy: Integer; const host: PCefString;
        port: Integer; const realm: PCefString; const scheme: PCefString;
        username, password: PCefString): Integer; stdcall;
  end;

  // Implement this structure to handle events related to browser display state.
  // The functions of this structure will be called on the UI thread.
  TCefDisplayHandler = record
    // Base structure.
    base: TCefBase;

    // Called when the navigation state has changed.
    on_nav_state_change: procedure(self: PCefDisplayHandler;
      browser: PCefBrowser; canGoBack, canGoForward: Integer); stdcall;

    // Called when a frame's address has changed.
    on_address_change: procedure(self: PCefDisplayHandler;
        browser: PCefBrowser; frame: PCefFrame;
        const url: PCefString); stdcall;

    // Called when the page title changes.
    on_title_change: procedure(self: PCefDisplayHandler;
        browser: PCefBrowser; const title: PCefString); stdcall;

    // Called when the browser is about to display a tooltip. |text| contains the
    // text that will be displayed in the tooltip. To handle the display of the
    // tooltip yourself return true (1). Otherwise, you can optionally modify
    // |text| and then return false (0) to allow the browser to display the
    // tooltip.
    on_tooltip: function(self: PCefDisplayHandler;
        browser: PCefBrowser; text: PCefString): Integer; stdcall;

    // Called when the browser receives a status message. |text| contains the text
    // that will be displayed in the status message and |type| indicates the
    // status message type.
    on_status_message: procedure(self: PCefDisplayHandler;
        browser: PCefBrowser; const value: PCefString;
        kind: TCefHandlerStatusType); stdcall;

    // Called to display a console message. Return true (1) to stop the message
    // from being output to the console.
    on_console_message: function(self: PCefDisplayHandler;
        browser: PCefBrowser; const message: PCefString;
        const source: PCefString; line: Integer): Integer; stdcall;
  end;

  // Implement this structure to handle events related to focus. The functions of
  // this structure will be called on the UI thread.
  TCefFocusHandler = record
    // Base structure.
    base: TCefBase;

    // Called when the browser component is about to loose focus. For instance, if
    // focus was on the last HTML element and the user pressed the TAB key. |next|
    // will be true (1) if the browser is giving focus to the next component and
    // false (0) if the browser is giving focus to the previous component.
    on_take_focus: procedure(self: PCefFocusHandler;
        browser: PCefBrowser; next: Integer); stdcall;

    // Called when the browser component is requesting focus. |source| indicates
    // where the focus request is originating from. Return false (0) to allow the
    // focus to be set or true (1) to cancel setting the focus.
    on_set_focus: function(self: PCefFocusHandler;
        browser: PCefBrowser; source: TCefHandlerFocusSource): Integer; stdcall;
  end;

  // Implement this structure to handle events related to keyboard input. The
  // functions of this structure will be called on the UI thread.
  TCefKeyboardHandler = record
    // Base structure.
    base: TCefBase;

    // Called when the browser component receives a keyboard event. |type| is the
    // type of keyboard event, |code| is the windows scan-code for the event,
    // |modifiers| is a set of bit-flags describing any pressed modifier keys and
    // |isSystemKey| is true (1) if Windows considers this a 'system key' message
    // (see http://msdn.microsoft.com/en-us/library/ms646286(VS.85).aspx). Return
    // true (1) if the keyboard event was handled or false (0) to allow the
    // browser component to handle the event.
    on_key_event: function(self: PCefKeyboardHandler;
        browser: PCefBrowser; kind: TCefHandlerKeyEventType;
        code, modifiers, isSystemKey: Integer): Integer; stdcall;
  end;

  // Implement this structure to handle events related to browser context menus.
  // The functions of this structure will be called on the UI thread.
  TCefMenuHandler = record
    // Base structure.
    base: TCefBase;

    // Called before a context menu is displayed. Return false (0) to display the
    // default context menu or true (1) to cancel the display.
    on_before_menu: function(self: PCefMenuHandler; browser: PCefBrowser;
        const menuInfo: PCefHandlerMenuInfo): Integer; stdcall;

    // Called to optionally override the default text for a context menu item.
    // |label| contains the default text and may be modified to substitute
    // alternate text.
    get_menu_label: procedure(self: PCefMenuHandler;
        browser: PCefBrowser; menuId: TCefHandlerMenuId;
        var label_: TCefString); stdcall;

    // Called when an option is selected from the default context menu. Return
    // false (0) to execute the default action or true (1) to cancel the action.
    on_menu_action: function(self: PCefMenuHandler;
        browser: PCefBrowser; menuId: TCefHandlerMenuId): Integer; stdcall;
  end;

  // Implement this structure to handle events related to printing. The functions
  // of this structure will be called on the UI thread.
  TCefPrintHandler = record
    // Base structure.
    base: TCefBase;

    // Called to allow customization of standard print options before the print
    // dialog is displayed. |printOptions| allows specification of paper size,
    // orientation and margins. Note that the specified margins may be adjusted if
    // they are outside the range supported by the printer. All units are in
    // inches. Return false (0) to display the default print options or true (1)
    // to display the modified |printOptions|.
    get_print_options: function(self: PCefPrintHandler;
        browser: PCefBrowser; printOptions: PCefPrintOptions): Integer; stdcall;

    // Called to format print headers and footers. |printInfo| contains platform-
    // specific information about the printer context. |url| is the URL if the
    // currently printing page, |title| is the title of the currently printing
    // page, |currentPage| is the current page number and |maxPages| is the total
    // number of pages. Six default header locations are provided by the
    // implementation: top left, top center, top right, bottom left, bottom center
    // and bottom right. To use one of these default locations just assign a
    // string to the appropriate variable. To draw the header and footer yourself
    // return true (1). Otherwise, populate the approprate variables and return
    // false (0).
    get_print_header_footer: function(self: PCefPrintHandler;
        browser: PCefBrowser; frame: PCefFrame;
        const printInfo: PCefPrintInfo; const url: PCefString;
        const title: PCefString; currentPage, maxPages: Integer;
        var topLeft, topCenter, topRight, bottomLeft, bottomCenter,
        bottomRight: TCefString): Integer; stdcall;
  end;

  // Implement this structure to handle events related to find results. The
  // functions of this structure will be called on the UI thread.
  TCefFindHandler = record
    // Base structure.
    base: TCefBase;

    ///
    // Called to report find results returned by cef_browser_t::find().
    // |identifer| is the identifier passed to cef_browser_t::find(), |count| is
    // the number of matches currently identified, |selectionRect| is the location
    // of where the match was found (in window coordinates), |activeMatchOrdinal|
    // is the current position in the search results, and |finalUpdate| is true
    // (1) if this is the last find notification.
    ///
    on_find_result: procedure(self: PCefFindHandler;
        browser: PCefBrowser; identifier, count: Integer;
        const selectionRect: PCefRect; activeMatchOrdinal,
        finalUpdate: Integer); stdcall;
  end;


  // Implement this structure to handle events related to JavaScript dialogs. The
  // functions of this structure will be called on the UI thread.
  TCefJsDialogHandler = record
    // Base structure.
    base: TCefBase;

    // Called  to run a JavaScript alert message. Return false (0) to display the
    // default alert or true (1) if you displayed a custom alert.
    on_jsalert: function(self: PCefJsDialogHandler;
        browser: PCefBrowser; frame: PCefFrame;
        const message: PCefString): Integer; stdcall;

    // Called to run a JavaScript confirm request. Return false (0) to display the
    // default alert or true (1) if you displayed a custom alert. If you handled
    // the alert set |retval| to true (1) if the user accepted the confirmation.
    on_jsconfirm: function(self: PCefJsDialogHandler;
        browser: PCefBrowser; frame: PCefFrame;
        const message: PCefString; var retval: Integer): Integer; stdcall;

    // Called to run a JavaScript prompt request. Return false (0) to display the
    // default prompt or true (1) if you displayed a custom prompt. If you handled
    // the prompt set |retval| to true (1) if the user accepted the prompt and
    // request and |result| to the resulting value.
    on_jsprompt: function(self: PCefJsDialogHandler;
        browser: PCefBrowser; frame: PCefFrame;
        const message, defaultValue: PCefString;
        var retval: Integer; var result: TCefString): Integer; stdcall;
  end;

  // Implement this structure to handle JavaScript binding. The functions of this
  // structure will be called on the UI thread.
  TCefJsBindingHandler = record
    // Base structure.
    base: TCefBase;

    // Called for adding values to a frame's JavaScript 'window' object.
    on_jsbinding: procedure(self: PCefJsBindingHandler;
        browser: PCefBrowser; frame: PCefFrame;
        obj: PCefv8Value); stdcall;
  end;

  // Implement this structure to handle events when window rendering is disabled.
  // The functions of this structure will be called on the UI thread.
  TCefRenderHandler = record
    // Base structure.
    base: TCefBase;

    // Called to retrieve the view rectangle which is relative to screen
    // coordinates. Return true (1) if the rectangle was provided.
    get_view_rect: function(self: PCefRenderHandler;
        browser: PCefBrowser; rect: PCefRect): Integer; stdcall;

    // Called to retrieve the simulated screen rectangle. Return true (1) if the
    // rectangle was provided.
    get_screen_rect: function(self: PCefRenderHandler;
        browser: PCefBrowser; rect: PCefRect): Integer; stdcall;

    // Called to retrieve the translation from view coordinates to actual screen
    // coordinates. Return true (1) if the screen coordinates were provided.
    get_screen_point: function(self: PCefRenderHandler;
        browser: PCefBrowser; viewX, viewY: Integer;
        screenX, screenY: PInteger): Integer; stdcall;

    // Called when the browser wants to show or hide the popup widget. The popup
    // should be shown if |show| is true (1) and hidden if |show| is false (0).
    on_popup_show: procedure(self: PCefRenderHandler;
        browser: PCefBrowser; show: Integer); stdcall;

    // Called when the browser wants to move or resize the popup widget. |rect|
    // contains the new location and size.
    on_popup_size: procedure(self: PCefRenderHandler;
        browser: PCefBrowser; const rect: PCefRect); stdcall;

    // Called when an element should be painted. |type| indicates whether the
    // element is the view or the popup widget. |buffer| contains the pixel data
    // for the whole image. |dirtyRect| indicates the portion of the image that
    // has been repainted. On Windows |buffer| will be width*height*4 bytes in
    // size and represents a BGRA image with an upper-left origin.
    on_paint: procedure(self: PCefRenderHandler;
        browser: PCefBrowser; kind: TCefPaintElementType;
        const dirtyRect: PCefRect; const buffer: Pointer); stdcall;

    // Called when the browser window's cursor has changed.
    on_cursor_change: procedure(self: PCefRenderHandler;
        browser: PCefBrowser; cursor: TCefCursorHandle); stdcall;
  end;

  // Implement this structure to handle events related to dragging. The functions
  // of this structure will be called on the UI thread.
  TCefDragHandler = record
    // Base structure.
    base: TCefBase;

    // Called when the browser window initiates a drag event. |dragData| contains
    // the drag event data and |mask| represents the type of drag operation.
    // Return false (0) for default drag handling behavior or true (1) to cancel
    // the drag event.
    on_drag_start: function(self: PCefDragHandler; browser: PCefBrowser; dragData: PCefDragData;
        mask: TCefDragOperations): Integer; stdcall;

    // Called when an external drag event enters the browser window. |dragData|
    // contains the drag event data and |mask| represents the type of drag
    // operation. Return false (0) for default drag handling behavior or true (1)
    // to cancel the drag event.
    on_drag_enter: function(self: PCefDragHandler; browser: PCefBrowser;
      dragData: PCefDragData; mask: TCefDragOperations): Integer; stdcall;
  end;

  // Implement this structure to provide handler implementations.
  TCefClient = record
    // Base structure.
    base: TCefBase;

    // Return the handler for browser life span events.
    get_life_span_handler: function(self: PCefClient): PCefLifeSpanHandler; stdcall;

    // Return the handler for browser load status events.
    get_load_handler: function(self: PCefClient): PCefLoadHandler; stdcall;

    // Return the handler for browser request events.
    get_request_handler: function(self: PCefClient): PCefRequestHandler; stdcall;

    // Return the handler for browser display state events.
    get_display_handler: function(self: PCefClient): PCefDisplayHandler; stdcall;

    // Return the handler for focus events.
    get_focus_handler: function(self: PCefClient): PCefFocusHandler; stdcall;

    // Return the handler for keyboard events.
    get_keyboard_handler: function(self: PCefClient): PCefKeyboardHandler; stdcall;

    // Return the handler for context menu events.
    get_menu_handler: function(self: PCefClient): PCefMenuHandler; stdcall;

    // Return the handler for printing events.
    get_print_handler: function(self: PCefClient): PCefPrintHandler; stdcall;

    // Return the handler for find result events.
    get_find_handler: function(self: PCefClient): PCefFindHandler; stdcall;

    // Return the handler for JavaScript dialog events.
    get_jsdialog_handler: function(self: PCefClient): PCefJsDialogHandler; stdcall;

    // Return the handler for JavaScript binding events.
    get_jsbinding_handler: function(self: PCefClient): PCefJsBindingHandler; stdcall;

    // Return the handler for off-screen rendering events.
    get_render_handler: function(self: PCefClient): PCefRenderHandler; stdcall;

    // Return the handler for drag events.
    get_drag_handler: function(self: PCefClient): PCefDragHandler; stdcall;
  end;

  // Structure used to represent a web request. The functions of this structure
  // may be called on any thread.
  TCefRequest = record
    // Base structure.
    base: TCefBase;

    // Get the fully qualified URL.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_url: function(self: PCefRequest): PCefStringUserFree; stdcall;
    // Set the fully qualified URL.
    set_url: procedure(self: PCefRequest; const url: PCefString); stdcall;

    // Get the request function type. The value will default to POST if post data
    // is provided and GET otherwise.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_method: function(self: PCefRequest): PCefStringUserFree; stdcall;
    // Set the request function type.
    set_method: procedure(self: PCefRequest; const method: PCefString); stdcall;

    // Get the post data.
    get_post_data: function(self: PCefRequest): PCefPostData; stdcall;
    // Set the post data.
    set_post_data: procedure(self: PCefRequest; postData: PCefPostData); stdcall;

    // Get the header values.
    get_header_map: procedure(self: PCefRequest; headerMap: TCefStringMap); stdcall;
    // Set the header values.
    set_header_map: procedure(self: PCefRequest; headerMap: TCefStringMap); stdcall;

    // Set all values at one time.
    set_: procedure(self: PCefRequest; const url, method: PCefString;
      postData: PCefPostData; headerMap: TCefStringMap); stdcall;

    // Get the flags used in combination with cef_web_urlrequest_t.
    get_flags: function(self: PCefRequest): TCefWebUrlRequestFlags; stdcall;
    // Set the flags used in combination with cef_web_urlrequest_t.
    set_flags: procedure(self: PCefRequest; flags: TCefWebUrlRequestFlags); stdcall;

    // Get the URL to the first party for cookies used in combination with
    // cef_web_urlrequest_t.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_first_party_for_cookies: function(self: PCefRequest): PCefStringUserFree; stdcall;
    // Set the URL to the first party for cookies used in combination with
    // cef_web_urlrequest_t.
    set_first_party_for_cookies: procedure(self: PCefRequest; const url: PCefString); stdcall;
  end;

  // Structure used to represent post data for a web request. The functions of
  // this structure may be called on any thread.
  TCefPostData = record
    // Base structure.
    base: TCefBase;

    // Returns the number of existing post data elements.
    get_element_count: function(self: PCefPostData): Cardinal; stdcall;

    // Retrieve the post data elements.
    get_elements: function(self: PCefPostData;
      elementIndex: Integer): PCefPostDataElement; stdcall;

    // Remove the specified post data element.  Returns true (1) if the removal
    // succeeds.
    remove_element: function(self: PCefPostData;
      element: PCefPostDataElement): Integer; stdcall;

    // Add the specified post data element.  Returns true (1) if the add succeeds.
    add_element: function(self: PCefPostData;
        element: PCefPostDataElement): Integer; stdcall;

    // Remove all existing post data elements.
    remove_elements: procedure(self: PCefPostData); stdcall;

  end;

  // Structure used to represent a single element in the request post data. The
  // functions of this structure may be called on any thread.
  TCefPostDataElement = record
    // Base structure.
    base: TCefBase;

    // Remove all contents from the post data element.
    set_to_empty: procedure(self: PCefPostDataElement); stdcall;

    // The post data element will represent a file.
    set_to_file: procedure(self: PCefPostDataElement;
        const fileName: PCefString); stdcall;

    // The post data element will represent bytes.  The bytes passed in will be
    // copied.
    set_to_bytes: procedure(self: PCefPostDataElement;
        size: Cardinal; const bytes: Pointer); stdcall;

    // Return the type of this post data element.
    get_type: function(self: PCefPostDataElement): TCefPostDataElementType; stdcall;

    // Return the file name.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_file: function(self: PCefPostDataElement): PCefStringUserFree; stdcall;

    // Return the number of bytes.
    get_bytes_count: function(self: PCefPostDataElement): Cardinal; stdcall;

    // Read up to |size| bytes into |bytes| and return the number of bytes
    // actually read.
    get_bytes: function(self: PCefPostDataElement;
        size: Cardinal; bytes: Pointer): Cardinal; stdcall;
  end;

  // Structure used to represent a web response. The functions of this structure
  // may be called on any thread.
  TCefResponse = record
    // Base structure.
    base: TCefBase;

    // Get the response status code.
    get_status: function(self: PCefResponse): Integer; stdcall;
    // Set the response status code.
    set_status: procedure(self: PCefResponse; status: Integer); stdcall;

    // Get the response status text.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_status_text: function(self: PCefResponse): PCefStringUserFree; stdcall;
    // Set the response status text.
    set_status_text: procedure(self: PCefResponse; const statusText: PCefString); stdcall;

    // Get the response mime type.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_mime_type: function(self: PCefResponse): PCefStringUserFree; stdcall;
    // Set the response mime type.
    set_mime_type: procedure(self: PCefResponse; const mimeType: PCefString); stdcall;

    // Get the value for the specified response header field.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_header: function(self: PCefResponse; const name: PCefString): PCefStringUserFree; stdcall;

    // Get all response header fields.
    get_header_map: procedure(self: PCefResponse; headerMap: TCefStringMap); stdcall;
    // Set all response header fields.
    set_header_map: procedure(self: PCefResponse; headerMap: TCefStringMap); stdcall;
  end;

  // Structure the client can implement to provide a custom stream reader. The
  // functions of this structure may be called on any thread.
  TCefReadHandler = record
    // Base structure.
    base: TCefBase;

    // Read raw binary data.
    read: function(self: PCefReadHandler; ptr: Pointer;
      size, n: Cardinal): Cardinal; stdcall;

    // Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
    // SEEK_END or SEEK_SET.
    seek: function(self: PCefReadHandler; offset: LongInt;
      whence: Integer): Integer; stdcall;

    // Return the current offset position.
    tell: function(self: PCefReadHandler): LongInt; stdcall;

    // Return non-zero if at end of file.
    eof: function(self: PCefReadHandler): Integer; stdcall;
  end;

  // Structure used to read data from a stream. The functions of this structure
  // may be called on any thread.
  TCefStreamReader = record
    // Base structure.
    base: TCefBase;

    // Read raw binary data.
    read: function(self: PCefStreamReader; ptr: Pointer;
        size, n: Cardinal): Cardinal; stdcall;

    // Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
    // SEEK_END or SEEK_SET. Returns zero on success and non-zero on failure.
    seek: function(self: PCefStreamReader; offset: LongInt;
        whence: Integer): Integer; stdcall;

    // Return the current offset position.
    tell: function(self: PCefStreamReader): LongInt; stdcall;

    // Return non-zero if at end of file.
    eof: function(self: PCefStreamReader): Integer; stdcall;
  end;

  // Structure the client can implement to provide a custom stream writer. The
  // functions of this structure may be called on any thread.
  TCefWriteHandler = record
    // Base structure.
    base: TCefBase;

    // Write raw binary data.
    write: function(self: PCefWriteHandler;
        const ptr: Pointer; size, n: Cardinal): Cardinal; stdcall;

    // Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
    // SEEK_END or SEEK_SET.
    seek: function(self: PCefWriteHandler; offset: LongInt;
        whence: Integer): Integer; stdcall;

    // Return the current offset position.
    tell: function(self: PCefWriteHandler): LongInt; stdcall;

    // Flush the stream.
    flush: function(self: PCefWriteHandler): Integer; stdcall;
  end;

  // Structure used to write data to a stream. The functions of this structure may
  // be called on any thread.
  TCefStreamWriter = record
    // Base structure.
    base: TCefBase;

    // Write raw binary data.
    write: function(self: PCefStreamWriter;
        const ptr: Pointer; size, n: Cardinal): Cardinal; stdcall;

    // Seek to the specified offset position. |whence| may be any one of SEEK_CUR,
    // SEEK_END or SEEK_SET.
    seek: function(self: PCefStreamWriter; offset: LongInt;
        whence: Integer): Integer; stdcall;

    // Return the current offset position.
    tell: function(self: PCefStreamWriter): LongInt; stdcall;

    // Flush the stream.
    flush: function(self: PCefStreamWriter): Integer; stdcall;
  end;

  // Structure that encapsulates a V8 context handle.
  TCefV8Context = record
    // Base structure.
    base: TCefBase;

    // Returns the browser for this context.
    get_browser: function(self: PCefv8Context): PCefBrowser; stdcall;

    // Returns the frame for this context.
    get_frame: function(self: PCefv8Context): PCefFrame; stdcall;

    // Returns the global object for this context.
    get_global: function(self: PCefv8Context): PCefv8Value; stdcall;

    // Enter this context. A context must be explicitly entered before creating a
    // V8 Object, Array or Function asynchronously. exit() must be called the same
    // number of times as enter() before releasing this context. V8 objects belong
    // to the context in which they are created. Returns true (1) if the scope was
    // entered successfully.
    enter: function(self: PCefv8Context): Integer; stdcall;

    // Exit this context. Call this function only after calling enter(). Returns
    // true (1) if the scope was exited successfully.
    exit: function(self: PCefv8Context): Integer; stdcall;
  end;

  // Structure that should be implemented to handle V8 function calls. The
  // functions of this structure will always be called on the UI thread.
  TCefv8Handler = record
    // Base structure.
    base: TCefBase;

    // Execute with the specified argument list and return value. Return true (1)
    // if the function was handled. To invoke V8 callback functions outside the
    // scope of this function you need to keep references to the current V8
    // context (cef_v8context_t) along with any necessary callback objects.
    execute: function(self: PCefv8Handler;
        const name: PCefString; obj: PCefv8Value; argumentCount: Cardinal;
        const arguments: PPCefV8Value; var retval: PCefV8Value;
        var exception: TCefString): Integer; stdcall;

    // Execute the function using the specified V8 context.
    execute_function_with_context: function(self: PCefv8Handler;
        context: PCefv8Context; obj: PCefv8Value; argumentCount: Cardinal;
        const arguments: PPCefV8Value; var retval: PCefV8Value;
        var exception: TCefString): Integer; stdcall;
  end;

  // Structure that should be implemented to handle V8 accessor calls. Accessor
  // identifiers are registered by calling cef_v8value_t::set_value(). The
  // functions of this structure will always be called on the UI thread.
  TCefV8Accessor = record
    // Base structure.
    base: TCefBase;

    // Called to get an accessor value. |name| is the name of the property being
    // accessed. |object| is the This() object from V8's AccessorInfo structure.
    // |retval| is the value to return for this property. Return true (1) if
    // handled.
    get: function(self: PCefV8Accessor; const name: PCefString;
      obj: PCefv8Value; out retval: PCefv8Value; exception: PCefString): Integer; stdcall;

    // Called to set an accessor value. |name| is the name of the property being
    // accessed. |value| is the new value being assigned to this property.
    // |object| is the This() object from V8's AccessorInfo structure. Return true
    // (1) if handled.

    put: function(self: PCefV8Accessor; const name: PCefString;
      obj: PCefv8Value; value: PCefv8Value; exception: PCefString): Integer; stdcall;
  end;


  // Structure representing a V8 value. The functions of this structure should
  // only be called on the UI thread.
  TCefv8Value = record
    // Base structure.
    base: TCefBase;

    // True if the value type is undefined.
    is_undefined: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is null.
    is_null: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is bool.
    is_bool: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is int.
    is_int: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is double.
    is_double: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is Date.
    is_date: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is string.
    is_string: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is object.
    is_object: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is array.
    is_array: function(self: PCefv8Value): Integer; stdcall;
    // True if the value type is function.
    is_function: function(self: PCefv8Value): Integer; stdcall;

    // Returns true (1) if this object is pointing to the same handle as |that|
    // object.
    is_same: function(self, that: PCefv8Value): Integer; stdcall;

    // Return a bool value.  The underlying data will be converted to if
    // necessary.
    get_bool_value: function(self: PCefv8Value): Integer; stdcall;
    // Return an int value.  The underlying data will be converted to if
    // necessary.
    get_int_value: function(self: PCefv8Value): Integer; stdcall;
    // Return a double value.  The underlying data will be converted to if
    // necessary.
    get_double_value: function(self: PCefv8Value): double; stdcall;
    // Return a Date value.  The underlying data will be converted to if
    // necessary.
    get_date_value: function(self: PCefv8Value): TCefTime; stdcall;
    // Return a string value.  The underlying data will be converted to if
    // necessary.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_string_value: function(self: PCefv8Value): PCefStringUserFree; stdcall;


    // OBJECT METHODS - These functions are only available on objects. Arrays and
    // functions are also objects. String- and integer-based keys can be used
    // interchangably with the framework converting between them as necessary.
    // Keys beginning with "Cef::" and "v8::" are reserved by the system.

    // Returns true (1) if the object has a value with the specified identifier.
    has_value_bykey: function(self: PCefv8Value; const key: PCefString): Integer; stdcall;
    // Returns true (1) if the object has a value with the specified identifier.
    has_value_byindex: function(self: PCefv8Value; index: Integer): Integer; stdcall;

    // Delete the value with the specified identifier.
    delete_value_bykey: function(self: PCefv8Value; const key: PCefString): Integer; stdcall;
    // Delete the value with the specified identifier.
    delete_value_byindex: function(self: PCefv8Value; index: Integer): Integer; stdcall;

    // Returns the value with the specified identifier.
    get_value_bykey: function(self: PCefv8Value; const key: PCefString): PCefv8Value; stdcall;
    // Returns the value with the specified identifier.
    get_value_byindex: function(self: PCefv8Value; index: Integer): PCefv8Value; stdcall;

    // Associate a value with the specified identifier.
    set_value_bykey: function(self: PCefv8Value;
       const key: PCefString; value: PCefv8Value): Integer; stdcall;
    // Associate a value with the specified identifier.
    set_value_byindex: function(self: PCefv8Value; index: Integer;
       value: PCefv8Value): Integer; stdcall;

    // Register an identifier whose access will be forwarded to the
    // cef_v8accessor_t instance passed to
    // cef_v8value_t::cef_v8value_create_object_with_accessor().
    set_value_byaccessor: function(self: PCefv8Value; const key: PCefString;
      settings: TCefV8AccessControls; attribute: TCefV8PropertyAttributes): Integer; stdcall;

    // Read the keys for the object's values into the specified vector. Integer-
    // based keys will also be returned as strings.
    get_keys: function(self: PCefv8Value;
        keys: TCefStringList): Integer; stdcall;

    // Returns the user data, if any, specified when the object was created.
    get_user_data: function(
        self: PCefv8Value): PCefBase; stdcall;


    // ARRAY METHODS - These functions are only available on arrays.

    // Returns the number of elements in the array.
    get_array_length: function(self: PCefv8Value): Integer; stdcall;


    // FUNCTION METHODS - These functions are only available on functions.

    // Returns the function name.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_function_name: function(self: PCefv8Value): PCefStringUserFree; stdcall;

    // Returns the function handler or NULL if not a CEF-created function.
    get_function_handler: function(
        self: PCefv8Value): PCefv8Handler; stdcall;

    // Execute the function.
    execute_function: function(self: PCefv8Value;
        obj: PCefv8Value; argumentCount: Cardinal;
        const arguments: PPCefV8Value; var retval: PCefV8Value;
        var exception: TCefString): Integer; stdcall;

    // Execute the function using the specified V8 context.
    execute_function_with_context: function(self: PCefV8value;
        context: PCefv8Context; obj: PCefv8Value;
        argumentCount: Cardinal; const arguments: PPCefV8Value;
        var retval: PCefv8Value; var exception: TCefString): Integer; stdcall;
  end;

  // Structure that creates cef_scheme_handler_t instances. The functions of this
  // structure will always be called on the IO thread.
  TCefSchemeHandlerFactory = record
    // Base structure.
    base: TCefBase;

    // Return a new scheme handler instance to handle the request. |browser| will
    // be the browser window that initiated the request. If the request was
    // initiated using the cef_web_urlrequest_t API |browser| will be NULL.
    create: function(self: PCefSchemeHandlerFactory;
      browser: PCefBrowser; const scheme_name: PCefString;
      request: PCefRequest): PCefSchemeHandler; stdcall;
  end;

  // Structure used to facilitate asynchronous responses to custom scheme handler
  // requests. The functions of this structure may be called on any thread.
  TCefSchemeHandlerCallback = record
    // Base structure.
    base: TCefBase;

    // Notify that header information is now available for retrieval.
    headers_available: procedure(self: PCefSchemeHandlerCallback); stdcall;

    // Notify that response data is now available for reading.
    bytes_available: procedure(self: PCefSchemeHandlerCallback); stdcall;

    // Cancel processing of the request.
    cancel: procedure(self: PCefSchemeHandlerCallback); stdcall;
  end;

  // Structure used to represent a custom scheme handler structure. The functions
  // of this structure will always be called on the IO thread.
  TCefSchemeHandler = record
    // Base structure.
    base: TCefBase;

    // Begin processing the request. To handle the request return true (1) and
    // call headers_available() once the response header information is available
    // (headers_available() can also be called from inside this function if header
    // information is available immediately). To redirect the request to a new URL
    // set |redirectUrl| to the new URL and return true (1). To cancel the request
    // return false (0).
    process_request: function(self: PCefSchemeHandler; request: PCefRequest;
      redirectUrl: PCefString; callback: PCefSchemeHandlerCallback): Integer; stdcall;

    // Retrieve response header information. If the response length is not known
    // set |response_length| to -1 and read_response() will be called until it
    // returns false (0). If the response length is known set |response_length| to
    // a positive value and read_response() will be called until it returns false
    // (0) or the specified number of bytes have been read. Use the |response|
    // object to set the mime type, http status code and other optional header
    // values.
    get_response_headers: procedure(self: PCefSchemeHandler;
      response: PCefResponse; response_length: PInt64); stdcall;

    // Read response data. If data is available immediately copy up to
    // |bytes_to_read| bytes into |data_out|, set |bytes_read| to the number of
    // bytes copied, and return true (1). To read the data at a later time set
    // |bytes_read| to 0, return true (1) and call bytes_available() when the data
    // is available. To indicate response completion return false (0).
    read_response: function(self: PCefSchemeHandler;
      data_out: Pointer; bytes_to_read: Integer; var bytes_read: Integer;
      callback: PCefSchemeHandlerCallback): Integer; stdcall;

    // Cancel processing of the request.
    cancel: procedure(self: PCefSchemeHandler); stdcall;
  end;

  // Structure used to handle file downloads. The functions of this structure will
  // always be called on the UI thread.
  TCefDownloadHandler = record
    // Base structure.
    base: TCefBase;

    // A portion of the file contents have been received. This function will be
    // called multiple times until the download is complete. Return |true (1)| to
    // continue receiving data and |false (0)| to cancel.
    received_data: function(self: PCefDownloadHandler; data: Pointer; data_size: Integer): Integer; stdcall;

    // The download is complete.
    complete: procedure(self: PCefDownloadHandler); stdcall;
  end;

  // Structure used to make a Web URL request. Web URL requests are not associated
  // with a browser instance so no cef_client_t callbacks will be executed. The
  // functions of this structure may be called on any thread.
  TCefWebUrlRequest = record
    // Base structure.
    base: TCefBase;

    // Cancels the request.
    cancel: procedure(self: PCefWebUrlRequest); stdcall;

    // Returns the current ready state of the request.
    get_state: function(self: PCefWebUrlRequest): TCefWebUrlRequestState; stdcall;
  end;

  TCefWebUrlRequestClient = record
    // Base structure.
    base: TCefBase;

    // Notifies the client that the request state has changed. State change
    // notifications will always be sent before the below notification functions
    // are called.
    on_state_change: procedure(self: PCefWebUrlRequestClient;
      requester: PCefWebUrlRequest; state: TCefWebUrlRequestState); stdcall;

    // Notifies the client that the request has been redirected and  provides a
    // chance to change the request parameters.
    on_redirect: procedure(self: PCefWebUrlRequestClient;
        requester: PCefWebUrlRequest; request: PCefRequest;
        response: PCefResponse); stdcall;

    // Notifies the client of the response data.
    on_headers_received: procedure(self: PCefWebUrlRequestClient;
        requester: PCefWebUrlRequest;
        response: PCefResponse); stdcall;

    // Notifies the client of the upload progress.
    on_progress: procedure(self: PCefWebUrlRequestClient;
        requester: PCefWebUrlRequest; bytesSent,
        totalBytesToBeSent: uint64); stdcall;

    // Notifies the client that content has been received.
    on_data: procedure(self: PCefWebUrlRequestClient;
        requester: PCefWebUrlRequest; const data: Pointer;
        dataLength: Integer); stdcall;

    // Notifies the client that the request ended with an error.
    on_error: procedure(self: PCefWebUrlRequestClient;
        requester: PCefWebUrlRequest; errorCode: TCefHandlerErrorcode); stdcall;
  end;

  // Structure that supports the reading of XML data via the libxml streaming API.
  // The functions of this structure should only be called on the thread that
  // creates the object.
  TCefXmlReader = record
    // Base structure.
    base: TcefBase;

    // Moves the cursor to the next node in the document. This function must be
    // called at least once to set the current cursor position. Returns true (1)
    // if the cursor position was set successfully.
    move_to_next_node: function(self: PCefXmlReader): Integer; stdcall;

    // Close the document. This should be called directly to ensure that cleanup
    // occurs on the correct thread.
    close: function(self: PCefXmlReader): Integer; stdcall;

    // Returns true (1) if an error has been reported by the XML parser.
    has_error: function(self: PCefXmlReader): Integer; stdcall;

    // Returns the error string.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_error: function(self: PCefXmlReader): PCefStringUserFree; stdcall;


    // The below functions retrieve data for the node at the current cursor
    // position.

    // Returns the node type.
    get_type: function(self: PCefXmlReader): TCefXmlNodeType; stdcall;

    // Returns the node depth. Depth starts at 0 for the root node.
    get_depth: function(self: PCefXmlReader): Integer; stdcall;

    // Returns the local name. See http://www.w3.org/TR/REC-xml-names/#NT-
    // LocalPart for additional details.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_local_name: function(self: PCefXmlReader): PCefStringUserFree; stdcall;

    // Returns the namespace prefix. See http://www.w3.org/TR/REC-xml-names/ for
    // additional details.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_prefix: function(self: PCefXmlReader): PCefStringUserFree; stdcall;

    // Returns the qualified name, equal to (Prefix:)LocalName. See
    // http://www.w3.org/TR/REC-xml-names/#ns-qualnames for additional details.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_qualified_name: function(self: PCefXmlReader): PCefStringUserFree; stdcall;

    // Returns the URI defining the namespace associated with the node. See
    // http://www.w3.org/TR/REC-xml-names/ for additional details.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_namespace_uri: function(self: PCefXmlReader): PCefStringUserFree; stdcall;

    // Returns the base URI of the node. See http://www.w3.org/TR/xmlbase/ for
    // additional details.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_base_uri: function(self: PCefXmlReader): PCefStringUserFree; stdcall;

    // Returns the xml:lang scope within which the node resides. See
    // http://www.w3.org/TR/REC-xml/#sec-lang-tag for additional details.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_xml_lang: function(self: PCefXmlReader): PCefStringUserFree; stdcall;

    // Returns true (1) if the node represents an NULL element. <a/> is considered
    // NULL but <a></a> is not.
    is_empty_element: function(self: PCefXmlReader): Integer; stdcall;

    // Returns true (1) if the node has a text value.
    has_value: function(self: PCefXmlReader): Integer; stdcall;

    // Returns the text value.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_value: function(self: PCefXmlReader): PCefStringUserFree; stdcall;

    // Returns true (1) if the node has attributes.
    has_attributes: function(self: PCefXmlReader): Integer; stdcall;

    // Returns the number of attributes.
    get_attribute_count: function(self: PCefXmlReader): Cardinal; stdcall;

    // Returns the value of the attribute at the specified 0-based index.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_attribute_byindex: function(self: PCefXmlReader; index: Integer): PCefStringUserFree; stdcall;

    // Returns the value of the attribute with the specified qualified name.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_attribute_byqname: function(self: PCefXmlReader; const qualifiedName: PCefString): PCefStringUserFree; stdcall;

    // Returns the value of the attribute with the specified local name and
    // namespace URI.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_attribute_bylname: function(self: PCefXmlReader; const localName, namespaceURI: PCefString): PCefStringUserFree; stdcall;

    // Returns an XML representation of the current node's children.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_inner_xml: function(self: PCefXmlReader): PCefStringUserFree; stdcall;

    // Returns an XML representation of the current node including its children.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_outer_xml: function(self: PCefXmlReader): PCefStringUserFree; stdcall;

    // Returns the line number for the current node.
    get_line_number: function(self: PCefXmlReader): Integer; stdcall;


    // Attribute nodes are not traversed by default. The below functions can be
    // used to move the cursor to an attribute node. move_to_carrying_element()
    // can be called afterwards to return the cursor to the carrying element. The
    // depth of an attribute node will be 1 + the depth of the carrying element.

    // Moves the cursor to the attribute at the specified 0-based index. Returns
    // true (1) if the cursor position was set successfully.
    move_to_attribute_byindex: function(self: PCefXmlReader; index: Integer): Integer; stdcall;

    // Moves the cursor to the attribute with the specified qualified name.
    // Returns true (1) if the cursor position was set successfully.
    move_to_attribute_byqname: function(self: PCefXmlReader; const qualifiedName: PCefString): Integer; stdcall;

    // Moves the cursor to the attribute with the specified local name and
    // namespace URI. Returns true (1) if the cursor position was set
    // successfully.
    move_to_attribute_bylname: function(self: PCefXmlReader; const localName, namespaceURI: PCefString): Integer; stdcall;

    // Moves the cursor to the first attribute in the current element. Returns
    // true (1) if the cursor position was set successfully.
    move_to_first_attribute: function(self: PCefXmlReader): Integer; stdcall;

    // Moves the cursor to the next attribute in the current element. Returns true
    // (1) if the cursor position was set successfully.
    move_to_next_attribute: function(self: PCefXmlReader): Integer; stdcall;

    // Moves the cursor back to the carrying element. Returns true (1) if the
    // cursor position was set successfully.
    move_to_carrying_element: function(self: PCefXmlReader): Integer; stdcall;
  end;

  // Structure that supports the reading of zip archives via the zlib unzip API.
  // The functions of this structure should only be called on the thread that
  // creates the object.
  TCefZipReader = record
    // Base structure.
    base: TCefBase;

    // Moves the cursor to the first file in the archive. Returns true (1) if the
    // cursor position was set successfully.
    move_to_first_file: function(self: PCefZipReader): Integer; stdcall;

    // Moves the cursor to the next file in the archive. Returns true (1) if the
    // cursor position was set successfully.
    move_to_next_file: function(self: PCefZipReader): Integer; stdcall;

    // Moves the cursor to the specified file in the archive. If |caseSensitive|
    // is true (1) then the search will be case sensitive. Returns true (1) if the
    // cursor position was set successfully.
    move_to_file: function(self: PCefZipReader; const fileName: PCefString; caseSensitive: Integer): Integer; stdcall;

    // Closes the archive. This should be called directly to ensure that cleanup
    // occurs on the correct thread.
    close: function(Self: PCefZipReader): Integer; stdcall;


    // The below functions act on the file at the current cursor position.

    // Returns the name of the file.
  // The resulting string must be freed by calling cef_string_userfree_free().
    get_file_name: function(Self: PCefZipReader): PCefStringUserFree; stdcall;

    // Returns the uncompressed size of the file.
    get_file_size: function(Self: PCefZipReader): LongInt; stdcall;

    // Returns the last modified timestamp for the file.
    get_file_last_modified: function(Self: PCefZipReader): LongInt; stdcall;

    // Opens the file for reading of uncompressed data. A read password may
    // optionally be specified.
    open_file: function(Self: PCefZipReader; const password: PCefString): Integer; stdcall;

    // Closes the file.
    close_file: function(Self: PCefZipReader): Integer; stdcall;

    // Read uncompressed file contents into the specified buffer. Returns < 0 if
    // an error occurred, 0 if at the end of file, or the number of bytes read.
    read_file: function(Self: PCefZipReader; buffer: Pointer; bufferSize: Cardinal): Integer; stdcall;

    // Returns the current offset in the uncompressed file contents.
    tell: function(Self: PCefZipReader): LongInt; stdcall;

    // Returns true (1) if at end of the file contents.
    eof: function(Self: PCefZipReader): Integer; stdcall;
  end;

  // Structure to implement for visiting the DOM. The functions of this structure
  // will be called on the UI thread.
  TCefDomVisitor = record
    // Base structure.
    base: TCefBase;

    // Method executed for visiting the DOM. The document object passed to this
    // function represents a snapshot of the DOM at the time this function is
    // executed. DOM objects are only valid for the scope of this function. Do not
    // keep references to or attempt to access any DOM objects outside the scope
    // of this function.
    visit: procedure(self: PCefDomVisitor; document: PCefDomDocument); stdcall;
  end;


  // Structure used to represent a DOM document. The functions of this structure
  // should only be called on the UI thread.
  TCefDomDocument = record
    // Base structure.
    base: TCefBase;

    // Returns the document type.
    get_type: function(self: PCefDomDocument): TCefDomDocumentType; stdcall;

    // Returns the root document node.
    get_document: function(self: PCefDomDocument): PCefDomNode; stdcall;

    // Returns the BODY node of an HTML document.
    get_body: function(self: PCefDomDocument): PCefDomNode; stdcall;

    // Returns the HEAD node of an HTML document.
    get_head: function(self: PCefDomDocument): PCefDomNode; stdcall;

    // Returns the title of an HTML document.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_title: function(self: PCefDomDocument): PCefStringUserFree; stdcall;

    // Returns the document element with the specified ID value.
    get_element_by_id: function(self: PCefDomDocument; const id: PCefString): PCefDomNode; stdcall;

    // Returns the node that currently has keyboard focus.
    get_focused_node: function(self: PCefDomDocument): PCefDomNode; stdcall;

    // Returns true (1) if a portion of the document is selected.
    has_selection: function(self: PCefDomDocument): Integer; stdcall;

    // Returns the selection start node.
    get_selection_start_node: function(self: PCefDomDocument): PCefDomNode; stdcall;

    // Returns the selection offset within the start node.
    get_selection_start_offset: function(self: PCefDomDocument): Integer; stdcall;

    // Returns the selection end node.
    get_selection_end_node: function(self: PCefDomDocument): PCefDomNode; stdcall;

    // Returns the selection offset within the end node.
    get_selection_end_offset: function(self: PCefDomDocument): Integer; stdcall;

    // Returns the contents of this selection as markup.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_selection_as_markup: function(self: PCefDomDocument): PCefStringUserFree; stdcall;

    // Returns the contents of this selection as text.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_selection_as_text: function(self: PCefDomDocument): PCefStringUserFree; stdcall;

    // Returns the base URL for the document.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_base_url: function(self: PCefDomDocument): PCefStringUserFree; stdcall;

    // Returns a complete URL based on the document base URL and the specified
    // partial URL.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_complete_url: function(self: PCefDomDocument; const partialURL: PCefString): PCefStringUserFree; stdcall;
  end;


  // Structure used to represent a DOM node. The functions of this structure
  // should only be called on the UI thread.
  TCefDomNode = record
    // Base structure.
    base: TCefBase;

    // Returns the type for this node.
    get_type: function(self: PCefDomNode): TCefDomNodeType; stdcall;

    // Returns true (1) if this is a text node.
    is_text: function(self: PCefDomNode): Integer; stdcall;

    // Returns true (1) if this is an element node.
    is_element: function(self: PCefDomNode): Integer; stdcall;

    // Returns true (1) if this object is pointing to the same handle as |that|
    // object.
    is_same: function(self, that: PCefDomNode): Integer; stdcall;

    // Returns the name of this node.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_name: function(self: PCefDomNode): PCefStringUserFree; stdcall;

    // Returns the value of this node.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_value: function(self: PCefDomNode): PCefStringUserFree; stdcall;

    // Set the value of this node. Returns true (1) on success.
    set_value: function(self: PCefDomNode; const value: PCefString): Integer; stdcall;

    // Returns the contents of this node as markup.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_as_markup: function(self: PCefDomNode): PCefStringUserFree; stdcall;

    // Returns the document associated with this node.
    get_document: function(self: PCefDomNode): PCefDomDocument; stdcall;

    // Returns the parent node.
    get_parent: function(self: PCefDomNode): PCefDomNode; stdcall;

    // Returns the previous sibling node.
    get_previous_sibling: function(self: PCefDomNode): PCefDomNode; stdcall;

    // Returns the next sibling node.
    get_next_sibling: function(self: PCefDomNode): PCefDomNode; stdcall;

    // Returns true (1) if this node has child nodes.
    has_children: function(self: PCefDomNode): Integer; stdcall;

    // Return the first child node.
    get_first_child: function(self: PCefDomNode): PCefDomNode; stdcall;

    // Returns the last child node.
    get_last_child: function(self: PCefDomNode): PCefDomNode; stdcall;

    // Add an event listener to this node for the specified event type. If
    // |useCapture| is true (1) then this listener will be considered a capturing
    // listener. Capturing listeners will recieve all events of the specified type
    // before the events are dispatched to any other event targets beneath the
    // current node in the tree. Events which are bubbling upwards through the
    // tree will not trigger a capturing listener. Separate calls to this function
    // can be used to register the same listener with and without capture. See
    // WebCore/dom/EventNames.h for the list of supported event types.
    add_event_listener: procedure(self: PCefDomNode; const eventType: PCefString;
      listener: PCefDomEventListener; useCapture: Integer); stdcall;

    // The following functions are valid only for element nodes.

    // Returns the tag name of this element.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_element_tag_name: function(self: PCefDomNode): PCefStringUserFree; stdcall;

    // Returns true (1) if this element has attributes.
    has_element_attributes: function(self: PCefDomNode): Integer; stdcall;

    // Returns true (1) if this element has an attribute named |attrName|.
    has_element_attribute: function(self: PCefDomNode; const attrName: PCefString): Integer; stdcall;

    // Returns the element attribute named |attrName|.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_element_attribute: function(self: PCefDomNode; const attrName: PCefString): PCefStringUserFree; stdcall;

    // Returns a map of all element attributes.
    get_element_attributes: procedure(self: PCefDomNode; attrMap: TCefStringMap); stdcall;

    // Set the value for the element attribute named |attrName|. Returns true (1)
    // on success.
    set_element_attribute: function(self: PCefDomNode; const attrName, value: PCefString): Integer; stdcall;

    // Returns the inner text of the element.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_element_inner_text: function(self: PCefDomNode): PCefStringUserFree; stdcall;
  end;


  // Structure used to represent a DOM event. The functions of this structure
  // should only be called on the UI thread.
  TCefDomEvent = record
    // Base structure.
    base: TCefBase;

    // Returns the event type.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_type: function(self: PCefDomEvent): PCefStringUserFree; stdcall;

    // Returns the event category.
    get_category: function(self: PCefDomEvent): TCefDomEventCategory; stdcall;

    // Returns the event processing phase.
    get_phase: function(self: PCefDomEvent): TCefDomEventPhase; stdcall;

    // Returns true (1) if the event can bubble up the tree.
    can_bubble: function(self: PCefDomEvent): Integer; stdcall;

    // Returns true (1) if the event can be canceled.
    can_cancel: function(self: PCefDomEvent): Integer; stdcall;

    // Returns the document associated with this event.
    get_document: function(self: PCefDomEvent): PCefDomDocument; stdcall;

    // Returns the target of the event.
    get_target: function(self: PCefDomEvent): PCefDomNode; stdcall;

    // Returns the current target of the event.
    get_current_target: function(self: PCefDomEvent): PCefDomNode; stdcall;
  end;

  // Structure to implement for handling DOM events. The functions of this
  // structure will be called on the UI thread.
  TCefDomEventListener = record
    // Base structure.
    base: TCefBase;

    // Called when an event is received. The event object passed to this function
    // contains a snapshot of the DOM at the time this function is executed. DOM
    // objects are only valid for the scope of this function. Do not keep
    // references to or attempt to access any DOM objects outside the scope of
    // this function.
    handle_event: procedure(self: PCefDomEventListener; event: PCefDomEvent); stdcall;
  end;

  // Structure to implement for visiting cookie values. The functions of this
  // structure will always be called on the IO thread.
  TCefCookieVisitor = record
    // Base structure.
    base: TCefBase;

    // Method that will be called once for each cookie. |count| is the 0-based
    // index for the current cookie. |total| is the total number of cookies. Set
    // |deleteCookie| to true (1) to delete the cookie currently being visited.
    // Return false (0) to stop visiting cookies. This function may never be
    // called if no cookies are found.

    visit: function(self: PCefCookieVisitor; const cookie: PCefCookie;
      count, total: Integer; deleteCookie: PInteger): Integer; stdcall;
  end;

  // Structure to implement for visiting storage. The functions of this structure
  // will always be called on the UI thread.
  TCefStorageVisitor = record
    // Base structure.
    base: TCefBase;

    // Method that will be called once for each key/value data pair in storage.
    // |count| is the 0-based index for the current pair. |total| is the total
    // number of pairs. Set |deleteData| to true (1) to delete the pair currently
    // being visited. Return false (0) to stop visiting pairs. This function may
    // never be called if no data is found.
    visit: function(self: PCefStorageVisitor; type_: TCefStorageType;
      const origin, key, value: PCefString; count, total: Integer;
      deleteData: PInteger): Integer; stdcall;
  end;

  // Structure to implement for filtering response content. The functions of this
  // structure will always be called on the UI thread.
  TCefContentFilter = record
    // Base structure.
    base: TCefBase;

    // Set |substitute_data| to the replacement for the data in |data| if data
    // should be modified.
    process_data: procedure(self: PCefContentFilter;
        const data: Pointer; data_size: Integer;
        var substitute_data: PCefStreamReader); stdcall;

    // Called when there is no more data to be processed. It is expected that
    // whatever data was retained in the last process_data() call, it should be
    // returned now by setting |remainder| if appropriate.
    drain: procedure(self: PCefContentFilter;
      var remainder: PCefStreamReader); stdcall;
  end;

  // Structure used to represent drag data. The functions of this structure may be
  // called on any thread.
  TCefDragData = record
    // Base structure.
    base: TCefBase;

    // Returns true (1) if the drag data is a link.
    is_link: function(self: PCefDragData): Integer; stdcall;

    // Returns true (1) if the drag data is a text or html fragment.
    is_fragment: function(self: PCefDragData): Integer; stdcall;

    // Returns true (1) if the drag data is a file.
    is_file: function(self: PCefDragData): Integer; stdcall;

    // Return the link URL that is being dragged.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_link_url: function(self: PCefDragData): PCefStringUserFree; stdcall;

    // Return the title associated with the link being dragged.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_link_title: function(self: PCefDragData): PCefStringUserFree; stdcall;

    // Return the metadata, if any, associated with the link being dragged.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_link_metadata: function(self: PCefDragData): PCefStringUserFree; stdcall;

    // Return the plain text fragment that is being dragged.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_fragment_text: function(self: PCefDragData): PCefStringUserFree; stdcall;

    // Return the text/html fragment that is being dragged.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_fragment_html: function(self: PCefDragData): PCefStringUserFree; stdcall;

    // Return the base URL that the fragment came from. This value is used for
    // resolving relative URLs and may be NULL.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_fragment_base_url: function(self: PCefDragData): PCefStringUserFree; stdcall;

    // Return the extension of the file being dragged out of the browser window.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_file_extension: function(self: PCefDragData): PCefStringUserFree; stdcall;

    // Return the name of the file being dragged out of the browser window.
    // The resulting string must be freed by calling cef_string_userfree_free().
    get_file_name: function(self: PCefDragData): PCefStringUserFree; stdcall;

    // Retrieve the list of file names that are being dragged into the browser
    // window.
    get_file_names: function(self: PCefDragData; names: TCefStringList): Integer; stdcall;
  end;

  ICefBrowser = interface;
  ICefFrame = interface;
  ICefRequest = interface;
  ICefv8Value = interface;
  ICefDomVisitor = interface;
  ICefDomDocument = interface;
  ICefDomNode = interface;

  ICefBase = interface
    ['{1F9A7B44-DCDC-4477-9180-3ADD44BDEB7B}']
    function Wrap: Pointer;
  end;

  ICefBrowser = interface(ICefBase)
    ['{BA003C2E-CF15-458F-9D4A-FE3CEFCF3EEF}']
    procedure ParentWindowWillClose;
    procedure CloseBrowser;
    function CanGoBack: Boolean;
    procedure GoBack;
    function CanGoForward: Boolean;
    procedure GoForward;
    procedure Reload;
    procedure ReloadIgnoreCache;
    procedure StopLoad;
    procedure SetFocus(enable: Boolean);
    function GetWindowHandle: TCefWindowHandle;
    function GetOpenerWindowHandle: TCefWindowHandle;
    function IsPopup: Boolean;
    function HasDocument: Boolean;
    function GetClient: ICefBase;
    function GetMainFrame: ICefFrame;
    function  GetFocusedFrame: ICefFrame;
    function GetFrame(const name: ustring): ICefFrame;
    procedure GetFrameNames(const names: TStrings);
    procedure Find(const searchText: ustring;
      identifier, forward, matchCase, findNext: Boolean);
    procedure StopFinding(ClearSelection: Boolean);
    function GetZoomLevel: Double;
    procedure SetZoomLevel(zoomLevel: Double);
    procedure ClearHistory;
    procedure ShowDevTools;
    procedure CloseDevTools;
    function IsWindowRenderingDisabled: Boolean;
    function GetSize(typ: TCefPaintElementType; var width, height: Integer): Boolean;
    procedure SetSize(typ: TCefPaintElementType; width, height: Integer);
    function IsPopupVisible: Boolean;
    procedure HidePopup;
    procedure Invalidate(dirtyRect: PCefRect);
    function GetImage(typ: TCefPaintElementType; width, height: Integer; buffer: Pointer): Boolean;
    procedure SendKeyEvent(typ: TCefKeyType; key, modifiers: Integer; sysChar, imeChar: Boolean);
    procedure SendMouseClickEvent(x, y: Integer; typ: TCefMouseButtonType;
      mouseUp: Boolean; clickCount: Integer);
    procedure SendMouseMoveEvent(x, y: Integer; mouseLeave: Boolean);
    procedure SendMouseWheelEvent(x, y, delta: Integer);
    procedure SendFocusEvent(setFocus: Boolean);
    procedure SendCaptureLostEvent;
    property MainFrame: ICefFrame read GetMainFrame;
    property Frame[const name: ustring]: ICefFrame read GetFrame;
    property ZoomLevel: Double read GetZoomLevel write SetZoomLevel;
  end;

  ICefPostDataElement = interface(ICefBase)
    ['{3353D1B8-0300-4ADC-8D74-4FF31C77D13C}']
    procedure SetToEmpty;
    procedure SetToFile(const fileName: ustring);
    procedure SetToBytes(size: Cardinal; bytes: Pointer);
    function GetType: TCefPostDataElementType;
    function GetFile: ustring;
    function GetBytesCount: Cardinal;
    function GetBytes(size: Cardinal; bytes: Pointer): Cardinal;
  end;

  ICefPostData = interface(ICefBase)
    ['{1E677630-9339-4732-BB99-D6FE4DE4AEC0}']
    function GetCount: Cardinal;
    function GetElement(Index: Integer): ICefPostDataElement;
    function RemoveElement(const element: ICefPostDataElement): Integer;
    function AddElement(const element: ICefPostDataElement): Integer;
    procedure RemoveElements;
  end;

  ICefStringMap = interface
  ['{A33EBC01-B23A-4918-86A4-E24A243B342F}']
    function GetHandle: TCefStringMap;
    function GetSize: Integer;
    function Find(const key: ustring): ustring;
    function GetKey(index: Integer): ustring;
    function GetValue(index: Integer): ustring;
    procedure Append(const key, value: ustring);
    procedure Clear;

    property Handle: TCefStringMap read GetHandle;
    property Size: Integer read GetSize;
    property Key[index: Integer]: ustring read GetKey;
    property Value[index: Integer]: ustring read GetValue;
  end;

  ICefRequest = interface(ICefBase)
    ['{FB4718D3-7D13-4979-9F4C-D7F6C0EC592A}']
    function GetUrl: ustring;
    function GetMethod: ustring;
    function GetPostData: ICefPostData;
    procedure GetHeaderMap(const HeaderMap: ICefStringMap);
    procedure SetUrl(const value: ustring);
    procedure SetMethod(const value: ustring);
    procedure SetPostData(const value: ICefPostData);
    procedure SetHeaderMap(const HeaderMap: ICefStringMap);
    function GetFlags: TCefWebUrlRequestFlags;
    procedure SetFlags(flags: TCefWebUrlRequestFlags);
    function GetFirstPartyForCookies: ustring;
    procedure SetFirstPartyForCookies(const url: ustring);
    property Url: ustring read GetUrl write SetUrl;
    property Method: ustring read GetMethod write SetMethod;
    property PostData: ICefPostData read GetPostData write SetPostData;
    property Flags: TCefWebUrlRequestFlags read GetFlags write SetFlags;
    property FirstPartyForCookies: ustring read GetFirstPartyForCookies write SetFirstPartyForCookies;
  end;

  TCefFastDomVisitorProc = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const document: ICefDomDocument);

  ICefFrame = interface(ICefBase)
    ['{8FD3D3A6-EA3A-4A72-8501-0276BD5C3D1D}']
    procedure Undo;
    procedure Redo;
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Del;
    procedure SelectAll;
    procedure Print;
    procedure ViewSource;
    function GetSource: ustring;
    function getText: ustring;
    procedure LoadRequest(const request: ICefRequest);
    procedure LoadUrl(const url: ustring);
    procedure LoadString(const str, url: ustring);
    procedure LoadStream(const stream: TStream; Owned: Boolean; const url: ustring);
    procedure LoadFile(const filename, url: ustring);
    procedure ExecuteJavaScript(const jsCode, scriptUrl: ustring; startLine: Integer);
    function IsMain: Boolean;
    function IsFocused: Boolean;
    function GetName: ustring;
    function GetUrl: ustring;
    function GetBrowser: ICefBrowser;
    procedure VisitDom(const visitor: ICefDomVisitor);
    procedure VisitDomProc(const proc: TCefFastDomVisitorProc);
    property Name: ustring read GetName;
    property Url: ustring read GetUrl;
    property Source: ustring read GetSource;
    property Text: ustring read GetText;
    property Browser: ICefBrowser read GetBrowser;
  end;

  ICefCustomStreamReader = interface(ICefBase)
    ['{BBCFF23A-6FE7-4C28-B13E-6D2ACA5C83B7}']
    function Read(ptr: Pointer; size, n: Cardinal): Cardinal;
    function Seek(offset: LongInt; whence: Integer): Integer;
    function Tell: LongInt;
    function Eof: Boolean;
  end;

  ICefStreamReader = interface(ICefBase)
    ['{DD5361CB-E558-49C5-A4BD-D1CE84ADB277}']
    function Read(ptr: Pointer; size, n: Cardinal): Cardinal;
    function Seek(offset: LongInt; whence: Integer): Integer;
    function Tell: LongInt;
    function Eof: Boolean;
  end;

  ICefResponse = interface(ICefBase)
  ['{E9C896E4-59A8-4B96-AB5E-6EA3A498B7F1}']
    function GetStatus: Integer;
    procedure SetStatus(status: Integer);
    function GetStatusText: ustring;
    procedure SetStatusText(const StatusText: ustring);
    function GetMimeType: ustring;
    procedure SetMimeType(const mimetype: ustring);
    function GetHeader(const name: ustring): ustring;
    procedure GetHeaderMap(const headerMap: ICefStringMap);
    procedure SetHeaderMap(const headerMap: ICefStringMap);
    property Status: Integer read GetStatus write SetStatus;
    property StatusText: ustring read GetStatusText write SetStatusText;
    property MimeType: ustring read GetMimeType write SetMimeType;
  end;

  ICefSchemeHandlerCallback = interface(ICefBase)
    ['{487F1C0D-3A77-4F84-84B8-C3DC8162E1F9}']
    procedure HeadersAvailable;
    procedure BytesAvailable;
    procedure Cancel;
  end;

  ICefSchemeHandler = interface(ICefBase)
  ['{A965F2A8-1675-44AE-AA54-F4C64B85A263}']
    function ProcessRequest(const Request: ICefRequest; var redirectUrl: ustring;
      const callback: ICefSchemeHandlerCallback): Boolean;
    procedure GetResponseHeaders(const response: ICefResponse; var responseLength: Int64);
    function ReadResponse(DataOut: Pointer; BytesToRead: Integer;
      var BytesRead: Integer; const callback: ICefSchemeHandlerCallback): Boolean;
    procedure Cancel;
  end;

  ICefSchemeHandlerFactory = interface(ICefBase)
    ['{4D9B7960-B73B-4EBD-9ABE-6C1C43C245EB}']
    function New(const scheme: ustring; const browser: ICefBrowser;
      const request: ICefRequest): ICefSchemeHandler;
  end;

  ICefDownloadHandler = interface(ICefBase)
  ['{3137F90A-5DC5-43C1-858D-A269F28EF4F1}']
    function ReceivedData(data: Pointer; DataSize: Integer): Integer;
    procedure Complete;
  end;

  ICefv8Context = interface(ICefBase)
    ['{2295A11A-8773-41F2-AD42-308C215062D9}']
    function GetBrowser: ICefBrowser;
    function GetFrame: ICefFrame;
    function GetGlobal: ICefv8Value;
    function Enter: Boolean;
    function Exit: Boolean;
    property Browser: ICefBrowser read GetBrowser;
    property Frame: ICefFrame read GetFrame;
    property Global: ICefv8Value read GetGlobal;
  end;

  TCefv8ValueArray = array of ICefv8Value;

  ICefv8Handler = interface(ICefBase)
    ['{F94CDC60-FDCB-422D-96D5-D2A775BD5D73}']
    function Execute(const name: ustring; const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean;
    function ExecuteFunctionWithContext(const context: ICefv8Context;
      const obj: ICefv8Value; const arguments: TCefv8ValueArray;
      var retval: ICefV8Value; var exception: ustring): Boolean;
  end;

  ICefV8Accessor = interface(ICefBase)
    ['{DCA6D4A2-726A-4E24-AA64-5E8C731D868A}']
    function Get(const name: ustring; const obj: ICefv8Value;
      out value: ICefv8Value; const exception: string): Boolean;
    function Put(const name: ustring; const obj: ICefv8Value;
      const value: ICefv8Value; const exception: string): Boolean;
  end;

  ICefTask = interface(ICefBase)
    ['{0D965470-4A86-47CE-BD39-A8770021AD7E}']
    procedure Execute(threadId: TCefThreadId);
  end;

  ICefv8Value = interface(ICefBase)
  ['{52319B8D-75A8-422C-BD4B-16FA08CC7F42}']
    function IsUndefined: Boolean;
    function IsNull: Boolean;
    function IsBool: Boolean;
    function IsInt: Boolean;
    function IsDouble: Boolean;
    function IsDate: Boolean;
    function IsString: Boolean;
    function IsObject: Boolean;
    function IsArray: Boolean;
    function IsFunction: Boolean;
    function IsSame(const that: ICefv8Value): Boolean;
    function GetBoolValue: Boolean;
    function GetIntValue: Integer;
    function GetDoubleValue: Double;
    function GetDateValue: TDateTime;
    function GetStringValue: ustring;
    function HasValueByKey(const key: ustring): Boolean;
    function HasValueByIndex(index: Integer): Boolean;
    function DeleteValueByKey(const key: ustring): Boolean;
    function DeleteValueByIndex(index: Integer): Boolean;
    function GetValueByKey(const key: ustring): ICefv8Value;
    function GetValueByIndex(index: Integer): ICefv8Value;
    function SetValueByKey(const key: ustring; const value: ICefv8Value): Boolean;
    function SetValueByIndex(index: Integer; const value: ICefv8Value): Boolean;
    function SetValueByAccessor(const key: ustring; settings: TCefV8AccessControls;
      attribute: TCefV8PropertyAttributes): Boolean;
    function GetKeys(const keys: TStrings): Integer;
    function GetUserData: ICefv8Value;
    function GetArrayLength: Integer;
    function GetFunctionName: ustring;
    function GetFunctionHandler: ICefv8Handler;
    function ExecuteFunction(const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean;
    function ExecuteFunctionWithContext(const context: ICefv8Context;
      const obj: ICefv8Value; const arguments: TCefv8ValueArray;
      var retval: ICefv8Value; var exception: ustring): Boolean;
  end;

  ICefXmlReader = interface(ICefBase)
  ['{0DE686C3-A8D7-45D2-82FD-92F7F4E62A90}']
    function MoveToNextNode: Boolean;
    function Close: Boolean;
    function HasError: Boolean;
    function GetError: ustring;
    function GetType: TCefXmlNodeType;
    function GetDepth: Integer;
    function GetLocalName: ustring;
    function GetPrefix: ustring;
    function GetQualifiedName: ustring;
    function GetNamespaceUri: ustring;
    function GetBaseUri: ustring;
    function GetXmlLang: ustring;
    function IsEmptyElement: Boolean;
    function HasValue: Boolean;
    function GetValue: ustring;
    function HasAttributes: Boolean;
    function GetAttributeCount: Cardinal;
    function GetAttributeByIndex(index: Integer): ustring;
    function GetAttributeByQName(const qualifiedName: ustring): ustring;
    function GetAttributeByLName(const localName, namespaceURI: ustring): ustring;
    function GetInnerXml: ustring;
    function GetOuterXml: ustring;
    function GetLineNumber: Integer;
    function MoveToAttributeByIndex(index: Integer): Boolean;
    function MoveToAttributeByQName(const qualifiedName: ustring): Boolean;
    function MoveToAttributeByLName(const localName, namespaceURI: ustring): Boolean;
    function MoveToFirstAttribute: Boolean;
    function MoveToNextAttribute: Boolean;
    function MoveToCarryingElement: Boolean;
  end;

  ICefZipReader = interface(ICefBase)
  ['{3B6C591F-9877-42B3-8892-AA7B27DA34A8}']
    function MoveToFirstFile: Boolean;
    function MoveToNextFile: Boolean;
    function MoveToFile(const fileName: ustring; caseSensitive: Boolean): Boolean;
    function Close: Boolean;
    function GetFileName: ustring;
    function GetFileSize: LongInt;
    function GetFileLastModified: LongInt;
    function OpenFile(const password: ustring): Boolean;
    function CloseFile: Boolean;
    function ReadFile(buffer: Pointer; bufferSize: Cardinal): Integer;
    function Tell: LongInt;
    function Eof: Boolean;
  end;

  ICefDomEvent = interface(ICefBase)
  ['{2CBD2259-ADC6-4187-9008-A666B57695CE}']
    function GetType: ustring;
    function GetCategory: TCefDomEventCategory;
    function GetPhase: TCefDomEventPhase;
    function CanBubble: Boolean;
    function CanCancel: Boolean;
    function GetDocument: ICefDomDocument;
    function GetTarget: ICefDomNode;
    function GetCurrentTarget: ICefDomNode;

    property EventType: ustring read GetType;
    property Category: TCefDomEventCategory read GetCategory;
    property Phase: TCefDomEventPhase read GetPhase;
    property Bubble: Boolean read CanBubble;
    property Cancel: Boolean read CanCancel;
    property Document: ICefDomDocument read GetDocument;
    property Target: ICefDomNode read GetTarget;
    property CurrentTarget: ICefDomNode read GetCurrentTarget;
  end;

  ICefDomEventListener = interface(ICefBase)
  ['{68BABB49-1824-42D0-ACCC-FDE9F8D39B88}']
    procedure HandleEvent(const event: ICefDomEvent);
  end;

  TCefFastDomEventListenerProc = {$IFDEF DELPHI12_UP}reference to {$ENDIF}procedure(const event: ICefDomEvent);

  ICefDomNode = interface(ICefBase)
  ['{96C03C9E-9C98-491A-8DAD-1947332232D6}']
    function GetType: TCefDomNodeType;
    function IsText: Boolean;
    function IsElement: Boolean;
    function IsSame(const that: ICefDomNode): Boolean;
    function GetName: ustring;
    function GetValue: ustring;
    function SetValue(const value: ustring): Boolean;
    function GetAsMarkup: ustring;
    function GetDocument: ICefDomDocument;
    function GetParent: ICefDomNode;
    function GetPreviousSibling: ICefDomNode;
    function GetNextSibling: ICefDomNode;
    function HasChildren: Boolean;
    function GetFirstChild: ICefDomNode;
    function GetLastChild: ICefDomNode;
    procedure AddEventListener(const eventType: ustring; useCapture: Boolean;
      const listener: ICefDomEventListener);
    procedure AddEventListenerProc(const eventType: ustring; useCapture: Boolean;
      const proc: TCefFastDomEventListenerProc);
    function GetElementTagName: ustring;
    function HasElementAttributes: Boolean;
    function HasElementAttribute(const attrName: ustring): Boolean;
    function GetElementAttribute(const attrName: ustring): ustring;
    procedure GetElementAttributes(const attrMap: ICefStringMap);
    function SetElementAttribute(const attrName, value: ustring): Boolean;
    function GetElementInnerText: ustring;

    property NodeType: TCefDomNodeType read GetType;
    property Name: ustring read GetName;
    property AsMarkup: ustring read GetAsMarkup;
    property Document: ICefDomDocument read GetDocument;
    property Parent: ICefDomNode read GetParent;
    property PreviousSibling: ICefDomNode read GetPreviousSibling;
    property NextSibling: ICefDomNode read GetNextSibling;
    property FirstChild: ICefDomNode read GetFirstChild;
    property LastChild: ICefDomNode read GetLastChild;
    property ElementTagName: ustring read GetElementTagName;
    property ElementInnerText: ustring read GetElementInnerText;
  end;

  ICefDomDocument = interface(ICefBase)
  ['{08E74052-45AF-4F69-A578-98A5C3959426}']
    function GetType: TCefDomDocumentType;
    function GetDocument: ICefDomNode;
    function GetBody: ICefDomNode;
    function GetHead: ICefDomNode;
    function GetTitle: ustring;
    function GetElementById(const id: ustring): ICefDomNode;
    function GetFocusedNode: ICefDomNode;
    function HasSelection: Boolean;
    function GetSelectionStartNode: ICefDomNode;
    function GetSelectionStartOffset: Integer;
    function GetSelectionEndNode: ICefDomNode;
    function GetSelectionEndOffset: Integer;
    function GetSelectionAsMarkup: ustring;
    function GetSelectionAsText: ustring;
    function GetBaseUrl: ustring;
    function GetCompleteUrl(const partialURL: ustring): ustring;
    property DocType: TCefDomDocumentType read GetType;
    property Document: ICefDomNode read GetDocument;
    property Body: ICefDomNode read GetBody;
    property Head: ICefDomNode read GetHead;
    property Title: ustring read GetTitle;
    property FocusedNode: ICefDomNode read GetFocusedNode;
    property SelectionStartNode: ICefDomNode read GetSelectionStartNode;
    property SelectionStartOffset: Integer read GetSelectionStartOffset;
    property SelectionEndNode: ICefDomNode read GetSelectionEndNode;
    property SelectionEndOffset: Integer read GetSelectionEndOffset;
    property SelectionAsMarkup: ustring read GetSelectionAsMarkup;
    property SelectionAsText: ustring read GetSelectionAsText;
    property BaseUrl: ustring read GetBaseUrl;
  end;

  ICefDomVisitor = interface(ICefBase)
  ['{30398428-3196-4531-B968-2DDBED36F6B0}']
    procedure visit(const document: ICefDomDocument);
  end;

  ICefWebUrlRequest = interface(ICefBase)
  ['{6AF727C3-54E7-4F4B-9147-7308CD405C71}']
    procedure Cancel;
    function GetState: TCefWebUrlRequestState;
  end;

  ICefWebUrlRequestClient = interface(ICefBase)
  ['{7A89C098-9C7D-43AE-9A82-CF1D10D91D20}']
    procedure OnStateChange(const requester: ICefWebUrlRequest; state: TCefWebUrlRequestState);
    procedure OnRedirect(const requester: ICefWebUrlRequest; const request: ICefRequest; const response: ICefResponse);
    procedure OnHeadersReceived(const requester: ICefWebUrlRequest; const response: ICefResponse);
    procedure OnProgress(const requester: ICefWebUrlRequest; bytesSent, totalBytesToBeSent: uint64);
    procedure OnData(const requester: ICefWebUrlRequest; const data: Pointer; dataLength: Integer);
    procedure OnError(const requester: ICefWebUrlRequest; errorCode: TCefHandlerErrorcode);
  end;

  ICefCookieVisitor = interface(ICefBase)
  ['{8378CF1B-84AB-4FDB-9B86-34DDABCCC402}']
    function visit(const name, value, domain, path: ustring; secure, httponly,
      hasExpires: Boolean; const creation, lastAccess, expires: TDateTime;
      count, total: Integer; out deleteCookie: Boolean): Boolean;
  end;

  ICefStorageVisitor = interface(ICefBase)
  ['{F6FDF9E2-FEC4-427D-8618-80D96B1E056B}']
    function visit(StorageType: TCefStorageType;
      const origin, key, value: ustring; count, total: Integer;
      out deleteData: Boolean): Boolean;
  end;

  ICefDragData = interface(ICefBase)
  ['{70088159-3D67-496E-89B1-56ACAF627CBF}']
    function IsLink: Boolean;
    function IsFragment: Boolean;
    function IsFile: Boolean;
    function GetLinkUrl: string;
    function GetLinkTitle: string;
    function GetLinkMetadata: string;
    function GetFragmentText: string;
    function GetFragmentHtml: string;
    function GetFragmentBaseUrl: string;
    function GetFileExtension: string;
    function GetFileName: string;
    function GetFileNames(names: TStrings): Boolean;
  end;

  TCefBaseOwn = class(TInterfacedObject, ICefBase)
  private
    FData: Pointer;
    FCriticaSection: TRTLCriticalSection;
  protected
    procedure Lock;
    procedure Unlock;
  public
    function Wrap: Pointer;
    constructor CreateData(size: Cardinal); virtual;
    destructor Destroy; override;
  end;

  TCefBaseRef = class(TInterfacedObject, ICefBase)
  private
    FData: Pointer;
  public
    constructor Create(data: Pointer); virtual;
    destructor Destroy; override;
    function Wrap: Pointer;
    class function UnWrap(data: Pointer): ICefBase;
  end;

  TCefBrowserRef = class(TCefBaseRef, ICefBrowser)
  protected
    procedure ParentWindowWillClose;
    procedure CloseBrowser;
    function CanGoBack: Boolean;
    procedure GoBack;
    function CanGoForward: Boolean;
    procedure GoForward;
    procedure Reload;
    procedure ReloadIgnoreCache;
    procedure StopLoad;
    procedure SetFocus(enable: Boolean);
    function GetWindowHandle: TCefWindowHandle;
    function GetOpenerWindowHandle: TCefWindowHandle;
    function IsPopup: Boolean;
    function HasDocument: Boolean;
    function GetClient: ICefBase;
    function GetMainFrame: ICefFrame;
    function  GetFocusedFrame: ICefFrame;
    function GetFrame(const name: ustring): ICefFrame;
    procedure GetFrameNames(const names: TStrings);
    procedure Find(const searchText: ustring;
      identifier, forward, matchCase, findNext: Boolean);
    procedure StopFinding(ClearSelection: Boolean);
    function GetZoomLevel: Double;
    procedure SetZoomLevel(zoomLevel: Double);
    procedure ClearHistory;
    procedure ShowDevTools;
    procedure CloseDevTools;
    function IsWindowRenderingDisabled: Boolean;
    function GetSize(typ: TCefPaintElementType; var width, height: Integer): Boolean;
    procedure SetSize(typ: TCefPaintElementType; width, height: Integer);
    function IsPopupVisible: Boolean;
    procedure HidePopup;
    procedure Invalidate(dirtyRect: PCefRect);
    function GetImage(typ: TCefPaintElementType; width, height: Integer; buffer: Pointer): Boolean;
    procedure SendKeyEvent(typ: TCefKeyType; key, modifiers: Integer; sysChar, imeChar: Boolean);
    procedure SendMouseClickEvent(x, y: Integer; typ: TCefMouseButtonType;
      mouseUp: Boolean; clickCount: Integer);
    procedure SendMouseMoveEvent(x, y: Integer; mouseLeave: Boolean);
    procedure SendMouseWheelEvent(x, y, delta: Integer);
    procedure SendFocusEvent(setFocus: Boolean);
    procedure SendCaptureLostEvent;
  public
    class function UnWrap(data: Pointer): ICefBrowser;
  end;

  TCefFrameRef = class(TCefBaseRef, ICefFrame)
  protected
    procedure Undo;
    procedure Redo;
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Del;
    procedure SelectAll;
    procedure Print;
    procedure ViewSource;
    function GetSource: ustring;
    function getText: ustring;
    procedure LoadRequest(const request: ICefRequest);
    procedure LoadUrl(const url: ustring);
    procedure LoadString(const str, url: ustring);
    procedure LoadStream(const stream: TStream; Owned: Boolean; const url: ustring);
    procedure LoadFile(const filename, url: ustring);
    procedure ExecuteJavaScript(const jsCode, scriptUrl: ustring; startLine: Integer);
    function IsMain: Boolean;
    function IsFocused: Boolean;
    function GetName: ustring;
    function GetUrl: ustring;
    function GetBrowser: ICefBrowser;
    procedure VisitDom(const visitor: ICefDomVisitor);
    procedure VisitDomProc(const proc: TCefFastDomVisitorProc);
  public
    class function UnWrap(data: Pointer): ICefFrame;
  end;

  TCefPostDataRef = class(TCefBaseRef, ICefPostData)
  protected
    function GetCount: Cardinal;
    function GetElement(Index: Integer): ICefPostDataElement;
    function RemoveElement(const element: ICefPostDataElement): Integer;
    function AddElement(const element: ICefPostDataElement): Integer;
    procedure RemoveElements;
  public
    class function UnWrap(data: Pointer): ICefPostData;
    class function New: ICefPostData;
  end;

  TCefPostDataElementRef = class(TCefBaseRef, ICefPostDataElement)
  protected
    procedure SetToEmpty;
    procedure SetToFile(const fileName: ustring);
    procedure SetToBytes(size: Cardinal; bytes: Pointer);
    function GetType: TCefPostDataElementType;
    function GetFile: ustring;
    function GetBytesCount: Cardinal;
    function GetBytes(size: Cardinal; bytes: Pointer): Cardinal;
  public
    class function UnWrap(data: Pointer): ICefPostDataElement;
    class function New: ICefPostDataElement;
  end;

  TCefRequestRef = class(TCefBaseRef, ICefRequest)
  protected
    function GetUrl: ustring;
    function GetMethod: ustring;
    function GetPostData: ICefPostData;
    procedure GetHeaderMap(const HeaderMap: ICefStringMap);
    procedure SetUrl(const value: ustring);
    procedure SetMethod(const value: ustring);
    procedure SetPostData(const value: ICefPostData);
    procedure SetHeaderMap(const HeaderMap: ICefStringMap);
    function GetFlags: TCefWebUrlRequestFlags;
    procedure SetFlags(flags: TCefWebUrlRequestFlags);
    function GetFirstPartyForCookies: ustring;
    procedure SetFirstPartyForCookies(const url: ustring);
  public
    class function UnWrap(data: Pointer): ICefRequest;
    class function New: ICefRequest;
  end;

  TCefStreamReaderRef = class(TCefBaseRef, ICefStreamReader)
  protected
    function Read(ptr: Pointer; size, n: Cardinal): Cardinal;
    function Seek(offset: LongInt; whence: Integer): Integer;
    function Tell: LongInt;
    function Eof: Boolean;
  public
    class function UnWrap(data: Pointer): ICefStreamReader;
    class function CreateForFile(const filename: ustring): ICefStreamReader;
    class function CreateForCustomStream(const stream: ICefCustomStreamReader): ICefStreamReader;
    class function CreateForStream(const stream: TSTream; owned: Boolean): ICefStreamReader;
    class function CreateForData(data: Pointer; size: Cardinal): ICefStreamReader;
  end;


  TCefFastV8AccessorGetterProc = {$IFDEF DELPHI12_UP} reference to{$ENDIF} function(
    const name: ustring; const obj: ICefv8Value; out value: ICefv8Value; const exception: string): Boolean;

  TCefFastV8AccessorSetterProc = {$IFDEF DELPHI12_UP}reference to {$ENDIF} function(
    const name: ustring; const obj, value: ICefv8Value; const exception: string): Boolean;

  TCefv8ValueRef = class(TCefBaseRef, ICefv8Value)
  protected
    function IsUndefined: Boolean;
    function IsNull: Boolean;
    function IsBool: Boolean;
    function IsInt: Boolean;
    function IsDouble: Boolean;
    function IsDate: Boolean;
    function IsString: Boolean;
    function IsObject: Boolean;
    function IsArray: Boolean;
    function IsFunction: Boolean;
    function IsSame(const that: ICefv8Value): Boolean;
    function GetBoolValue: Boolean;
    function GetIntValue: Integer;
    function GetDateValue: TDateTime;
    function GetDoubleValue: Double;
    function GetStringValue: ustring;
    function HasValueByKey(const key: ustring): Boolean;
    function HasValueByIndex(index: Integer): Boolean;
    function DeleteValueByKey(const key: ustring): Boolean;
    function DeleteValueByIndex(index: Integer): Boolean;
    function GetValueByKey(const key: ustring): ICefv8Value;
    function GetValueByIndex(index: Integer): ICefv8Value;
    function SetValueByKey(const key: ustring; const value: ICefv8Value): Boolean;
    function SetValueByIndex(index: Integer; const value: ICefv8Value): Boolean;
    function SetValueByAccessor(const key: ustring; settings: TCefV8AccessControls;
      attribute: TCefV8PropertyAttributes): Boolean;
    function GetKeys(const keys: TStrings): Integer;
    function GetUserData: ICefv8Value;
    function GetArrayLength: Integer;
    function GetFunctionName: ustring;
    function GetFunctionHandler: ICefv8Handler;
    function ExecuteFunction(const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean;
    function ExecuteFunctionWithContext(const context: ICefv8Context;
      const obj: ICefv8Value; const arguments: TCefv8ValueArray;
      var retval: ICefv8Value; var exception: ustring): Boolean;
  public
    class function UnWrap(data: Pointer): ICefv8Value;
    class function CreateUndefined: ICefv8Value;
    class function CreateNull: ICefv8Value;
    class function CreateBool(value: Boolean): ICefv8Value;
    class function CreateInt(value: Integer): ICefv8Value;
    class function CreateDouble(value: Double): ICefv8Value;
    class function CreateDate(value: TDateTime): ICefv8Value;
    class function CreateString(const str: ustring): ICefv8Value;
    class function CreateObject(const UserData: ICefv8Value): ICefv8Value;
    class function CreateObjectWithAccessor(const UserData: ICefv8Value; const Accessor: ICefV8Accessor): ICefv8Value;
    class function CreateObjectWithAccessorProc(const UserData: ICefv8Value;
      const getter: TCefFastV8AccessorGetterProc;
      const setter: TCefFastV8AccessorSetterProc): ICefv8Value;
    class function CreateArray: ICefv8Value;
    class function CreateFunction(const name: ustring; const handler: ICefv8Handler): ICefv8Value;
  end;

  TCefv8ContextRef = class(TCefBaseRef, ICefv8Context)
  protected
    function GetBrowser: ICefBrowser;
    function GetFrame: ICefFrame;
    function GetGlobal: ICefv8Value;
    function Enter: Boolean;
    function Exit: Boolean;
  public
    class function UnWrap(data: Pointer): ICefv8Context;
    class function Current: ICefv8Context;
    class function Entered: ICefv8Context;
  end;

  TCefv8HandlerRef = class(TCefBaseRef, ICefv8Handler)
  protected
    function Execute(const name: ustring; const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean;
    function ExecuteFunctionWithContext(const context: ICefv8Context;
      const obj: ICefv8Value; const arguments: TCefv8ValueArray;
      var retval: ICefV8Value; var exception: ustring): Boolean;
  public
    class function UnWrap(data: Pointer): ICefv8Handler;
  end;

  TCefClientOwn = class(TCefBaseOwn)
  protected
    function GetLifeSpanHandler: ICefBase; virtual;
    function GetLoadHandler: ICefBase; virtual;
    function GetRequestHandler: ICefBase; virtual;
    function GetDisplayHandler: ICefBase; virtual;
    function GetFocusHandler: ICefBase; virtual;
    function GetKeyboardHandler: ICefBase; virtual;
    function GetMenuHandler: ICefBase; virtual;
    function GetPrintHandler: ICefBase; virtual;
    function GetFindHandler: ICefBase; virtual;
    function GetJsdialogHandler: ICefBase; virtual;
    function GetJsbindingHandler: ICefBase; virtual;
    function GetRenderHandler: ICefBase; virtual;
    function GetDragHandler: ICefBase; virtual;
  public
    constructor Create; virtual;
  end;

  TCefLifeSpanHandlerOwn = class(TCefBaseOwn)
  protected
    function OnBeforePopup(const parentBrowser: ICefBrowser;
       var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
       var url: ustring; var client: ICefBase;
       var settings: TCefBrowserSettings): Boolean; virtual;
    procedure OnAfterCreated(const browser: ICefBrowser); virtual;
    procedure OnBeforeClose(const browser: ICefBrowser); virtual;
    function RunModal(const browser: ICefBrowser): Boolean; virtual;
    function DoClose(const browser: ICefBrowser): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefLoadHandlerOwn = class(TCefBaseOwn)
  protected
    procedure OnLoadStart(const browser: ICefBrowser; const frame: ICefFrame); virtual;
    procedure OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer); virtual;
    function OnLoadError(const browser: ICefBrowser; const frame: ICefFrame;
      errorCode: TCefHandlerErrorcode; const failedUrl: ustring; var errorText: ustring): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefContentFilterOwn = class(TCefBaseOwn)
  protected
    procedure ProcessData(const Data: Pointer; Size: Integer;
        var SubstituteData: ICefStreamReader); virtual;
    procedure Drain(var remainder: ICefStreamReader); virtual;
  public
    constructor Create; virtual;
  end;

  TCefRequestHandlerOwn = class(TCefBaseOwn)
  protected
    function OnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; navType: TCefHandlerNavtype;
      isRedirect: Boolean): Boolean; virtual;
    function OnBeforeResourceLoad(const browser: ICefBrowser; const request: ICefRequest;
      var redirectUrl: ustring;  var resourceStream: ICefStreamReader;
      const response: ICefResponse; loadFlags: Integer): Boolean; virtual;
    procedure OnResourceResponse(const browser: ICefBrowser; const url: ustring;
        const response: ICefResponse; var filter: ICefBase); virtual;
    function OnProtocolExecution(const browser: ICefBrowser; const url: ustring;
        var allowOSExecution: Boolean): Boolean; virtual;
    function GetDownloadHandler(const browser: ICefBrowser;
      const mimeType, fileName: ustring; contentLength: int64;
        var handler: ICefDownloadHandler): Boolean; virtual;
    function GetAuthCredentials(const browser: ICefBrowser;
      isProxy: Boolean; port: Integer; const host, realm, scheme: ustring;
      var username, password: ustring): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefDisplayHandlerOwn = class(TCefBaseOwn)
  protected
    procedure OnNavStateChange(const browser: ICefBrowser;
      canGoBack, canGoForward: Boolean); virtual;
    procedure OnAddressChange(const browser: ICefBrowser;
      const frame: ICefFrame; const url: ustring); virtual;
    procedure OnTitleChange(const browser: ICefBrowser;
      const title: ustring); virtual;
    function OnTooltip(const browser: ICefBrowser;
      var text: ustring): Boolean; virtual;
    procedure OnStatusMessage(const browser: ICefBrowser; const value: ustring;
        kind: TCefHandlerStatusType); virtual;
    function OnConsoleMessage(const browser: ICefBrowser; const message,
      source: ustring; line: Integer): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefFocusHandlerOwn = class(TCefBaseOwn)
  protected
    procedure OnTakeFocus(const browser: ICefBrowser; next: Boolean); virtual;
    function OnSetFocus(const browser: ICefBrowser; source: TCefHandlerFocusSource): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefKeyboardHandlerOwn = class(TCefBaseOwn)
  protected
    function OnKeyEvent(const browser: ICefBrowser; event: TCefHandlerKeyEventType;
      code, modifiers: Integer; isSystemKey: Boolean): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefMenuHandlerOwn = class(TCefBaseOwn)
  protected
    function OnBeforeMenu(const browser: ICefBrowser;
      const menuInfo: PCefHandlerMenuInfo): Boolean; virtual;
    procedure GetMenuLabel(const browser: ICefBrowser;
      menuId: TCefHandlerMenuId; var caption: ustring); virtual;
    function OnMenuAction(const browser: ICefBrowser;
      menuId: TCefHandlerMenuId): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefPrintHandlerOwn = class(TCefBaseOwn)
  protected
    function GetPrintOptions(const browser: ICefBrowser;
      printOptions: PCefPrintOptions): Boolean; virtual;
    function GetPrintHeaderFooter(const browser: ICefBrowser; const frame: ICefFrame;
      const printInfo: PCefPrintInfo; const url, title: ustring; currentPage, maxPages: Integer;
      var topLeft, topCenter, topRight, bottomLeft, bottomCenter, bottomRight: ustring): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefFindHandlerOwn = class(TCefBaseOwn)
  protected
    procedure OnFindResult(const browser: ICefBrowser; count: Integer;
      const selectionRect: PCefRect; identifier, activeMatchOrdinal,
      finalUpdate: Boolean); virtual;
  public
    constructor Create; virtual;
  end;

  TCefJsDialogHandlerOwn = class(TCefBaseOwn)
  protected
    function OnJsAlert(const browser: ICefBrowser; const frame: ICefFrame;
      const message: ustring): Boolean; virtual;
    function OnJsConfirm(const browser: ICefBrowser; const frame: ICefFrame;
      const message: ustring; var retval: Boolean): Boolean; virtual;
    function OnJsPrompt(const browser: ICefBrowser; const frame: ICefFrame;
      const message, defaultValue: ustring; var retval: Boolean;
      var return: ustring): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefJsBindingHandlerOwn = class(TCefBaseOwn)
  protected
    procedure OnJsBinding(const browser: ICefBrowser;
      const frame: ICefFrame; const obj: ICefv8Value); virtual;
  public
    constructor Create; virtual;
  end;

  TCefRenderHandlerOwn = class(TCefBaseOwn)
  protected
    function GetViewRect(const browser: ICefBrowser; rect: PCefRect): Boolean; virtual;
    function GetScreenRect(const browser: ICefBrowser; rect: PCefRect): Boolean; virtual;
    function GetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer;
      screenX, screenY: PInteger): Boolean; virtual;
    procedure OnPopupShow(const browser: ICefBrowser; show: Boolean); virtual;
    procedure OnPopupSize(const browser: ICefBrowser; const rect: PCefRect); virtual;
    procedure OnPaint(const browser: ICefBrowser; kind: TCefPaintElementType;
        const dirtyRect: PCefRect; const buffer: Pointer); virtual;
    procedure OnCursorChange(const browser: ICefBrowser; cursor: TCefCursorHandle); virtual;
  public
    constructor Create; virtual;
  end;

  TCefDragHandlerOwn = class(TCefBaseOwn)
  protected
    function OnDragStart(const browser: ICefBrowser; const dragData: ICefDragData;
      mask: TCefDragOperations): Boolean; virtual;
    function OnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData;
      mask: TCefDragOperations): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefCustomStreamReader = class(TCefBaseOwn, ICefCustomStreamReader)
  private
    FStream: TStream;
    FOwned: Boolean;
  protected
    function Read(ptr: Pointer; size, n: Cardinal): Cardinal; virtual;
    function Seek(offset: LongInt; whence: Integer): Integer; virtual;
    function Tell: LongInt; virtual;
    function Eof: Boolean; virtual;
  public
    constructor Create(Stream: TStream; Owned: Boolean); overload; virtual;
    constructor Create(const filename: string); overload; virtual;
    destructor Destroy; override;
  end;

  TCefPostDataElementOwn = class(TCefBaseOwn, ICefPostDataElement)
  private
    FDataType: TCefPostDataElementType;
    FValueByte: Pointer;
    FValueStr: TCefString;
    FSize: Cardinal;
    procedure Clear;
  protected
    procedure SetToEmpty; virtual;
    procedure SetToFile(const fileName: ustring); virtual;
    procedure SetToBytes(size: Cardinal; bytes: Pointer); virtual;
    function GetType: TCefPostDataElementType; virtual;
    function GetFile: ustring; virtual;
    function GetBytesCount: Cardinal; virtual;
    function GetBytes(size: Cardinal; bytes: Pointer): Cardinal; virtual;
  public
    constructor Create; virtual;
  end;

  TCefSchemeHandlerOwn = class(TCefBaseOwn, ICefSchemeHandler)
  private
    FCancelled: Boolean;
    FScheme: ustring;
    FBrowser: ICefBrowser;
    FRequest: ICefRequest;
  protected
    function ProcessRequest(const Request: ICefRequest; var redirectUrl: ustring;
      const callback: ICefSchemeHandlerCallback): Boolean; virtual;
    procedure GetResponseHeaders(const response: ICefResponse; var responseLength: Int64); virtual;
    function ReadResponse(DataOut: Pointer; BytesToRead: Integer;
      var BytesRead: Integer; const callback: ICefSchemeHandlerCallback): Boolean; virtual;
    procedure Cancel; virtual;
  public
    constructor Create(SyncMainThread: Boolean; const scheme: ustring;
     const browser: ICefBrowser; const request: ICefRequest); virtual;
    property Cancelled: Boolean read FCancelled;
    property Scheme: ustring read FScheme;
    property Browser: ICefBrowser read FBrowser;
    property Request: ICefRequest read FRequest;
  end;
  TCefSchemeHandlerClass = class of TCefSchemeHandlerOwn;

  TCefSchemeHandlerCallbackRef = class(TCefBaseRef, ICefSchemeHandlerCallback)
  protected
    procedure HeadersAvailable;
    procedure BytesAvailable;
    procedure Cancel;
  public
    class function UnWrap(data: Pointer): ICefSchemeHandlerCallback;
  end;

  TCefSchemeHandlerFactoryOwn = class(TCefBaseOwn, ICefSchemeHandlerFactory)
  private
    FClass: TCefSchemeHandlerClass;
    FSyncMainThread: Boolean;
  protected
    function New(const scheme: ustring; const browser: ICefBrowser; const request: ICefRequest): ICefSchemeHandler; virtual;
  public
    constructor Create(const AClass: TCefSchemeHandlerClass; SyncMainThread: Boolean); virtual;
  end;

  TCefDownloadHandlerOwn = class(TCefBaseOwn, ICefDownloadHandler)
  protected
    function ReceivedData(data: Pointer; DataSize: Integer): Integer; virtual; abstract;
    procedure Complete; virtual; abstract;
  public
    constructor Create; virtual;
  end;

  TCefv8HandlerOwn = class(TCefBaseOwn, ICefv8Handler)
  protected
    function Execute(const name: ustring; const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean; virtual;
    function ExecuteFunctionWithContext(const context: ICefv8Context;
      const obj: ICefv8Value; const arguments: TCefv8ValueArray;
      var retval: ICefV8Value; var exception: ustring): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefTaskOwn = class(TCefBaseOwn, ICefTask)
  protected
    procedure Execute(threadId: TCefThreadId); virtual;
  public
    constructor Create; virtual;
  end;

  TCefStringMapOwn = class(TInterfacedObject, ICefStringMap)
  private
    FStringMap: TCefStringMap;
  protected
    function GetHandle: TCefStringMap; virtual;
    function GetSize: Integer; virtual;
    function Find(const key: ustring): ustring; virtual;
    function GetKey(index: Integer): ustring; virtual;
    function GetValue(index: Integer): ustring; virtual;
    procedure Append(const key, value: ustring); virtual;
    procedure Clear; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TCefXmlReaderRef = class(TCefBaseRef, ICefXmlReader)
  protected
    function MoveToNextNode: Boolean;
    function Close: Boolean;
    function HasError: Boolean;
    function GetError: ustring;
    function GetType: TCefXmlNodeType;
    function GetDepth: Integer;
    function GetLocalName: ustring;
    function GetPrefix: ustring;
    function GetQualifiedName: ustring;
    function GetNamespaceUri: ustring;
    function GetBaseUri: ustring;
    function GetXmlLang: ustring;
    function IsEmptyElement: Boolean;
    function HasValue: Boolean;
    function GetValue: ustring;
    function HasAttributes: Boolean;
    function GetAttributeCount: Cardinal;
    function GetAttributeByIndex(index: Integer): ustring;
    function GetAttributeByQName(const qualifiedName: ustring): ustring;
    function GetAttributeByLName(const localName, namespaceURI: ustring): ustring;
    function GetInnerXml: ustring;
    function GetOuterXml: ustring;
    function GetLineNumber: Integer;
    function MoveToAttributeByIndex(index: Integer): Boolean;
    function MoveToAttributeByQName(const qualifiedName: ustring): Boolean;
    function MoveToAttributeByLName(const localName, namespaceURI: ustring): Boolean;
    function MoveToFirstAttribute: Boolean;
    function MoveToNextAttribute: Boolean;
    function MoveToCarryingElement: Boolean;
  public
    class function UnWrap(data: Pointer): ICefXmlReader;
    class function CreateForStream(const stream: ICefStreamReader;
      encodingType: TCefXmlEncodingType; const URI: ustring): ICefXmlReader;
  end;

  TCefZipReaderRef = class(TCefBaseRef, ICefZipReader)
  protected
    function MoveToFirstFile: Boolean;
    function MoveToNextFile: Boolean;
    function MoveToFile(const fileName: ustring; caseSensitive: Boolean): Boolean;
    function Close: Boolean;
    function GetFileName: ustring;
    function GetFileSize: LongInt;
    function GetFileLastModified: LongInt;
    function OpenFile(const password: ustring): Boolean;
    function CloseFile: Boolean;
    function ReadFile(buffer: Pointer; bufferSize: Cardinal): Integer;
    function Tell: LongInt;
    function Eof: Boolean;
  public
    class function UnWrap(data: Pointer): ICefZipReader;
    class function CreateForStream(const stream: ICefStreamReader): ICefZipReader;
  end;

  TCefDomVisitorOwn = class(TCefBaseOwn, ICefDomVisitor)
  protected
    procedure visit(const document: ICefDomDocument); virtual;
  public
    constructor Create; virtual;
  end;

  TCefFastDomVisitor = class(TCefDomVisitorOwn)
  private
    FProc: TCefFastDomVisitorProc;
  protected
    procedure visit(const document: ICefDomDocument); override;
  public
    constructor Create(const proc: TCefFastDomVisitorProc); reintroduce; virtual;
  end;

  TCefDomDocumentRef = class(TCefBaseRef, ICefDomDocument)
  protected
    function GetType: TCefDomDocumentType;
    function GetDocument: ICefDomNode;
    function GetBody: ICefDomNode;
    function GetHead: ICefDomNode;
    function GetTitle: ustring;
    function GetElementById(const id: ustring): ICefDomNode;
    function GetFocusedNode: ICefDomNode;
    function HasSelection: Boolean;
    function GetSelectionStartNode: ICefDomNode;
    function GetSelectionStartOffset: Integer;
    function GetSelectionEndNode: ICefDomNode;
    function GetSelectionEndOffset: Integer;
    function GetSelectionAsMarkup: ustring;
    function GetSelectionAsText: ustring;
    function GetBaseUrl: ustring;
    function GetCompleteUrl(const partialURL: ustring): ustring;
  public
    class function UnWrap(data: Pointer): ICefDomDocument;
  end;

  TCefDomNodeRef = class(TCefBaseRef, ICefDomNode)
  protected
    function GetType: TCefDomNodeType;
    function IsText: Boolean;
    function IsElement: Boolean;
    function IsSame(const that: ICefDomNode): Boolean;
    function GetName: ustring;
    function GetValue: ustring;
    function SetValue(const value: ustring): Boolean;
    function GetAsMarkup: ustring;
    function GetDocument: ICefDomDocument;
    function GetParent: ICefDomNode;
    function GetPreviousSibling: ICefDomNode;
    function GetNextSibling: ICefDomNode;
    function HasChildren: Boolean;
    function GetFirstChild: ICefDomNode;
    function GetLastChild: ICefDomNode;
    procedure AddEventListener(const eventType: ustring;
      useCapture: Boolean; const listener: ICefDomEventListener);
    procedure AddEventListenerProc(const eventType: ustring; useCapture: Boolean;
      const proc: TCefFastDomEventListenerProc);
    function GetElementTagName: ustring;
    function HasElementAttributes: Boolean;
    function HasElementAttribute(const attrName: ustring): Boolean;
    function GetElementAttribute(const attrName: ustring): ustring;
    procedure GetElementAttributes(const attrMap: ICefStringMap);
    function SetElementAttribute(const attrName, value: ustring): Boolean;
    function GetElementInnerText: ustring;
  public
    class function UnWrap(data: Pointer): ICefDomNode;
  end;

  TCefDomEventRef = class(TCefBaseRef, ICefDomEvent)
  protected
    function GetType: ustring;
    function GetCategory: TCefDomEventCategory;
    function GetPhase: TCefDomEventPhase;
    function CanBubble: Boolean;
    function CanCancel: Boolean;
    function GetDocument: ICefDomDocument;
    function GetTarget: ICefDomNode;
    function GetCurrentTarget: ICefDomNode;
  public
    class function UnWrap(data: Pointer): ICefDomEvent;
  end;

  TCefDomEventListenerOwn = class(TCefBaseOwn, ICefDomEventListener)
  protected
    procedure HandleEvent(const event: ICefDomEvent); virtual;
  public
    constructor Create; virtual;
  end;

  TCefResponseRef = class(TCefBaseRef, ICefResponse)
  protected
    function GetStatus: Integer;
    procedure SetStatus(status: Integer);
    function GetStatusText: ustring;
    procedure SetStatusText(const StatusText: ustring);
    function GetMimeType: ustring;
    procedure SetMimeType(const mimetype: ustring);
    function GetHeader(const name: ustring): ustring;
    procedure GetHeaderMap(const headerMap: ICefStringMap);
    procedure SetHeaderMap(const headerMap: ICefStringMap);
  public
    class function UnWrap(data: Pointer): ICefResponse;
  end;

  TCefWebUrlRequestClientOwn = class(TCefBaseOwn, ICefWebUrlRequestClient)
  protected
    procedure OnStateChange(const requester: ICefWebUrlRequest; state: TCefWebUrlRequestState); virtual;
    procedure OnRedirect(const requester: ICefWebUrlRequest; const request: ICefRequest; const response: ICefResponse); virtual;
    procedure OnHeadersReceived(const requester: ICefWebUrlRequest; const response: ICefResponse); virtual;
    procedure OnProgress(const requester: ICefWebUrlRequest; bytesSent, totalBytesToBeSent: uint64); virtual;
    procedure OnData(const requester: ICefWebUrlRequest; const data: Pointer; dataLength: Integer); virtual;
    procedure OnError(const requester: ICefWebUrlRequest; errorCode: TCefHandlerErrorcode); virtual;
  public
    constructor Create; virtual;
  end;

  TCefWebUrlRequestRef = class(TCefBaseRef, ICefWebUrlRequest)
  protected
    procedure Cancel;
    function GetState: TCefWebUrlRequestState;
  public
    class function UnWrap(data: Pointer): ICefWebUrlRequest;
    class function New(const request: ICefRequest;
      const client: ICefWebUrlRequestClient): ICefWebUrlRequest;
  end;

  TCefFastDomEventListener = class(TCefDomEventListenerOwn)
  private
    FProc: TCefFastDomEventListenerProc;
  protected
    procedure HandleEvent(const event: ICefDomEvent); override;
  public
    constructor Create(const proc: TCefFastDomEventListenerProc); reintroduce; virtual;
  end;

{$IFDEF DELPHI12_UP}
  TTaskMethod = TProc;
{$ELSE}
  TTaskMethod = procedure(const Browser: ICefBrowser);
{$ENDIF}

  TCefFastTask = class(TCefTaskOwn)
  private
    FMethod: TTaskMethod;
{$IFNDEF DELPHI12_UP}
    FBrowser: ICefBrowser;
{$ENDIF}
  protected
    procedure Execute(threadId: TCefThreadId); override;
  public
    class procedure Post(threadId: TCefThreadId; const method: TTaskMethod{$IFNDEF DELPHI12_UP}; const Browser: ICefBrowser{$ENDIF});
    class procedure PostDelayed(threadId: TCefThreadId; Delay: Integer; const method: TTaskMethod{$IFNDEF DELPHI12_UP}; const Browser: ICefBrowser{$ENDIF});
    constructor Create(const method: TTaskMethod{$IFNDEF DELPHI12_UP}; const Browser: ICefBrowser{$ENDIF}); reintroduce;
  end;

{$IFDEF DELPHI14_UP}
  TCefRTTIExtension = class(TCefv8HandlerOwn)
  private
    FValue: TValue;
    FCtx: TRttiContext;
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    FSyncMainThread: Boolean;
{$ENDIF}
    function GetValue(pi: PTypeInfo; const v: ICefv8Value; var ret: TValue): Boolean;
    function SetValue(const v: TValue; var ret: ICefv8Value): Boolean;
  protected
    function Execute(const name: ustring; const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean; override;
  public
    constructor Create(const value: TValue
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    ; SyncMainThread: Boolean
{$ENDIF}
); reintroduce;
    destructor Destroy; override;
    class procedure Register(const name: string; const value: TValue
      {$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}; SyncMainThread: Boolean{$ENDIF});
  end;
{$ENDIF}

  TCefV8AccessorOwn = class(TCefBaseOwn, ICefV8Accessor)
  protected
    function Get(const name: ustring; const obj: ICefv8Value;
      out value: ICefv8Value; const exception: string): Boolean; virtual;
    function Put(const name: ustring; const obj, value: ICefv8Value;
      const exception: string): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefFastV8Accessor = class(TCefV8AccessorOwn)
  private
    FGetter: TCefFastV8AccessorGetterProc;
    FSetter: TCefFastV8AccessorSetterProc;
  protected
    function Get(const name: ustring; const obj: ICefv8Value;
      out value: ICefv8Value; const exception: string): Boolean; override;
    function Put(const name: ustring; const obj, value: ICefv8Value;
      const exception: string): Boolean; override;
  public
    constructor Create(const getter: TCefFastV8AccessorGetterProc;
      const setter: TCefFastV8AccessorSetterProc); reintroduce;
  end;

  TCefCookieVisitorOwn = class(TCefBaseOwn, ICefCookieVisitor)
  protected
    function visit(const name, value, domain, path: ustring; secure, httponly,
      hasExpires: Boolean; const creation, lastAccess, expires: TDateTime;
      count, total: Integer; out deleteCookie: Boolean): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefStorageVisitorOwn = class(TCefBaseOwn, ICefStorageVisitor)
  protected
    function visit(StorageType: TCefStorageType;
      const origin, key, value: ustring; count, total: Integer;
      out deleteData: Boolean): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

  TCefCookieVisitorProc = {$IFDEF DELPHI12_UP} reference to {$ENDIF} function(
    const name, value, domain, path: ustring; secure, httponly,
    hasExpires: Boolean; const creation, lastAccess, expires: TDateTime;
    count, total: Integer; out deleteCookie: Boolean): Boolean;

  TCefFastCookieVisitor = class(TCefCookieVisitorOwn)
  private
    FVisitor: TCefCookieVisitorProc;
  protected
    function visit(const name, value, domain, path: ustring; secure, httponly,
      hasExpires: Boolean; const creation, lastAccess, expires: TDateTime;
      count, total: Integer; out deleteCookie: Boolean): Boolean; override;
  public
    constructor Create(const visitor: TCefCookieVisitorProc); reintroduce;
  end;

  TCefStorageVisitorProc = {$IFDEF DELPHI12_UP} reference to {$ENDIF} function(
    StorageType: TCefStorageType; const origin: ustring;
    const key: ustring; const value: ustring; count: Integer; total: Integer;
    out deleteData: Boolean): Boolean;

  TCefFastStorageVisitor = class(TCefStorageVisitorOwn)
  private
    FVisitor: TCefStorageVisitorProc;
  protected
    function visit(StorageType: TCefStorageType; const origin: ustring;
      const key: ustring; const value: ustring; count: Integer; total: Integer;
      out deleteData: Boolean): Boolean; override;
  public
    constructor Create(const visitor: TCefStorageVisitorProc); reintroduce;
  end;


  TCefDragDataRef = class(TCefBaseRef, ICefDragData)
  protected
    function IsLink: Boolean;
    function IsFragment: Boolean;
    function IsFile: Boolean;
    function GetLinkUrl: string;
    function GetLinkTitle: string;
    function GetLinkMetadata: string;
    function GetFragmentText: string;
    function GetFragmentHtml: string;
    function GetFragmentBaseUrl: string;
    function GetFileExtension: string;
    function GetFileName: string;
    function GetFileNames(names: TStrings): Boolean;
  public
    class function UnWrap(data: Pointer): ICefDragData;
  end;

  ECefException = class(Exception)
  end;

procedure CefLoadLibDefault;
procedure CefLoadLib(const Cache: ustring = ''; const UserAgent: ustring = '';
  const ProductVersion: ustring = ''; const Locale: ustring = '';
  const LogFile: ustring = ''; const ExtraPluginPaths: ustring = '';
  LogSeverity: TCefLogSeverity = LOGSEVERITY_DISABLE;
  GraphicsImplementation: TCefGraphicsImplementation = ANGLE_IN_PROCESS;
  LocalStorageQuota: Cardinal = 0; SessionStorageQuota: Cardinal = 0
  );
function CefGetObject(ptr: Pointer): TObject;
function CefStringAlloc(const str: ustring): TCefString;

function CefString(const str: ustring): TCefString; overload;
function CefString(const str: PCefString): ustring; overload;
function CefUserFreeString(const str: ustring): PCefStringUserFree;

function CefStringClearAndGet(var str: TCefString): ustring;
procedure CefStringFree(const str: PCefString);
function CefStringFreeAndGet(const str: PCefStringUserFree): ustring;
procedure CefStringSet(const str: PCefString; const value: ustring);
function CefBrowserCreate(windowInfo: PCefWindowInfo; client: PCefClient;
  const url: ustring; const settings: PCefBrowserSettings): Boolean;
function CefBrowserCreateSync(windowInfo: PCefWindowInfo; client: PCefClient;
  const url: ustring; const settings: PCefBrowserSettings): ICefBrowser;
{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
procedure CefDoMessageLoopWork;
procedure CefRunMessageLoop;
{$ENDIF}

function CefRegisterSchemeHandlerFactory(const SchemeName, HostName: ustring;
  SyncMainThread: Boolean; const handler: TCefSchemeHandlerClass): Boolean;
function CefClearSchemeHandlerFactories: Boolean;
function CefRegisterCustomScheme(const SchemeName: ustring; IsStandard,
  IsLocal, IsDisplayIsolated: Boolean): Boolean;

function CefAddCrossOriginWhitelistEntry(const SourceOrigin, TargetProtocol,
  TargetDomain: ustring; AllowTargetSubdomains: Boolean): Boolean;
function CefRemoveCrossOriginWhitelistEntry(
  const SourceOrigin, TargetProtocol, TargetDomain: ustring;
  AllowTargetSubdomains: Boolean): Boolean;
function CefClearCrossOriginWhitelist: Boolean;

function CefRegisterExtension(const name, code: ustring;
  const Handler: ICefv8Handler): Boolean;
function CefCurrentlyOn(ThreadId: TCefThreadId): Boolean;
procedure CefPostTask(ThreadId: TCefThreadId; const task: ICefTask);
procedure CefPostDelayedTask(ThreadId: TCefThreadId; const task: ICefTask; delayMs: Integer);
function CefGetData(const i: ICefBase): Pointer;
function CefParseUrl(const url: ustring; var parts: TCefUrlParts): Boolean;

function CefVisitAllCookies(const visitor: ICefCookieVisitor): Boolean; overload;
function CefVisitAllCookies(const visitor: TCefCookieVisitorProc): Boolean; overload;
function CefVisitUrlCookies(const url: ustring; includeHttpOnly: Boolean; const visitor: ICefCookieVisitor): Boolean; overload;
function CefVisitUrlCookies(const url: ustring; includeHttpOnly: Boolean; const visitor: TCefCookieVisitorProc): Boolean; overload;

// must be run on the io thread
function CefSetCookie(const url: ustring; const name, value, domain, path: ustring; secure, httponly,
  hasExpires: Boolean; const creation, lastAccess, expires: TDateTime): Boolean;
function CefDeleteCookies(const url, cookieName: ustring): Boolean;

function CefVisitStorage(StorageType: TCefStorageType;
  const origin, key: ustring; const visitor: ICefStorageVisitor): Boolean; overload;
function CefVisitStorage(StorageType: TCefStorageType;
  const origin, key: ustring; const visitor: TCefStorageVisitorProc): Boolean; overload;
function CefSetStorage(StorageType: TCefStorageType;
  const origin, key, value: ustring): Boolean;
function CefDeleteStorage(StorageType: TCefStorageType;
  const origin, key: ustring): Boolean;

var
  CefLibrary: string = 'libcef.dll';
  CefCache: ustring = '';
  CefUserAgent: ustring = '';
  CefProductVersion: ustring = '';
  CefLocale: ustring = '';
  CefLogFile: ustring = '';
  CefLogSeverity: TCefLogSeverity = LOGSEVERITY_DISABLE;
  CefGraphicsImplementation: TCefGraphicsImplementation = ANGLE_IN_PROCESS;
  CefExtraPluginPaths: ustring = '';
  CefLocalStorageQuota: Cardinal;
  CefSessionStorageQuota: Cardinal;

implementation

function TzSpecificLocalTimeToSystemTime(
  lpTimeZoneInformation: PTimeZoneInformation;
  lpLocalTime, lpUniversalTime: PSystemTime): BOOL; stdcall; external 'kernel32.dll';

function SystemTimeToTzSpecificLocalTime(
  lpTimeZoneInformation: PTimeZoneInformation;
  lpUniversalTime, lpLocalTime: PSystemTime): BOOL; stdcall; external 'kernel32.dll';

type
  TSHSyncProcessRequest = class
  private
    FHandler: TCefSchemeHandlerOwn;
    FRequest: ICefRequest;
    FResult: Boolean;
    FRedirectUrl: ustring;
    FCallback: ICefSchemeHandlerCallback;
  public
    procedure Execute;
    constructor Create(Handler: TCefSchemeHandlerOwn;
      const Request: ICefRequest; const Callback: ICefSchemeHandlerCallback); virtual;
  end;

  procedure TSHSyncProcessRequest.Execute;
  begin
    FResult := FHandler.ProcessRequest(FRequest, FRedirectUrl, FCallback);
  end;

  constructor TSHSyncProcessRequest.Create(Handler: TCefSchemeHandlerOwn;
    const Request: ICefRequest; const Callback: ICefSchemeHandlerCallback);
  begin
    FHandler := Handler;
    FRequest := Request;
    FResult := False;
    FRedirectUrl := '';
    FCallback := Callback;
  end;

type
  TSHSyncReadResponse = class
  private
    FHandler: TCefSchemeHandlerOwn;
    FDataOut: Pointer;
    FBytesToRead: Integer;
    FBytesRead: Integer;
    FCallback: ICefSchemeHandlerCallback;
    FResult: Boolean;
  public
    procedure Execute;
    constructor Create(Handler: TCefSchemeHandlerOwn; DataOut: Pointer;
      BytesToRead: Integer; BytesRead: Integer; const Callback: ICefSchemeHandlerCallback);
  end;

  procedure TSHSyncReadResponse.Execute;
  begin
    FResult := FHandler.ReadResponse(FDataOut, FBytesToRead, FBytesRead, FCallback);
  end;

  constructor TSHSyncReadResponse.Create(Handler: TCefSchemeHandlerOwn; DataOut: Pointer;
    BytesToRead: Integer; BytesRead: Integer; const Callback: ICefSchemeHandlerCallback);
  begin
    FHandler := Handler;
    FDataOut := DataOut;
    FBytesToRead := BytesToRead;
    FBytesRead := BytesRead;
    FCallback := Callback;
    FResult := False;
  end;

type
  TSHSyncGetResponseHeaders = class
  private
    FHandler: TCefSchemeHandlerOwn;
    FResponse: ICefResponse;
    FResponseLength: Int64;
  public
    procedure Execute;
    constructor Create(Handler: TCefSchemeHandlerOwn; const Response: ICefResponse);
  end;

  procedure TSHSyncGetResponseHeaders.Execute;
  begin
    FHandler.GetResponseHeaders(FResponse, FResponseLength);
  end;

  constructor TSHSyncGetResponseHeaders.Create(Handler: TCefSchemeHandlerOwn;
    const Response: ICefResponse);
  begin
    FHandler := Handler;
    FResponse := Response;
    FResponseLength := 0;
  end;

var
// These functions set string values. If |copy| is true (1) the value will be
// copied instead of referenced. It is up to the user to properly manage
// the lifespan of references.

  cef_string_wide_set: function(const src: PWideChar; src_len: Cardinal;  output: PCefStringWide; copy: Integer): Integer; cdecl;
  cef_string_utf8_set: function(const src: PAnsiChar; src_len: Cardinal; output: PCefStringUtf8; copy: Integer): Integer; cdecl;
  cef_string_utf16_set: function(const src: PChar16; src_len: Cardinal; output: PCefStringUtf16; copy: Integer): Integer; cdecl;
  cef_string_set: function(const src: PCefChar; src_len: Cardinal; output: PCefString; copy: Integer): Integer; cdecl;

  // These functions clear string values. The structure itself is not freed.

  cef_string_wide_clear: procedure(str: PCefStringWide); cdecl;
  cef_string_utf8_clear: procedure(str: PCefStringUtf8); cdecl;
  cef_string_utf16_clear: procedure(str: PCefStringUtf16); cdecl;
  cef_string_clear: procedure(str: PCefString); cdecl;

  // These functions compare two string values with the same results as strcmp().

  cef_string_wide_cmp: function(const str1, str2: PCefStringWide): Integer; cdecl;
  cef_string_utf8_cmp: function(const str1, str2: PCefStringUtf8): Integer; cdecl;
  cef_string_utf16_cmp: function(const str1, str2: PCefStringUtf16): Integer; cdecl;

  // These functions convert between UTF-8, -16, and -32 strings. They are
  // potentially slow so unnecessary conversions should be avoided. The best
  // possible result will always be written to |output| with the boolean return
  // value indicating whether the conversion is 100% valid.

  cef_string_wide_to_utf8: function(const src: PWideChar; src_len: Cardinal; output: PCefStringUtf8): Integer; cdecl;
  cef_string_utf8_to_wide: function(const src: PAnsiChar; src_len: Cardinal; output: PCefStringWide): Integer; cdecl;

  cef_string_wide_to_utf16: function (const src: PWideChar; src_len: Cardinal; output: PCefStringUtf16): Integer; cdecl;
  cef_string_utf16_to_wide: function(const src: PChar16; src_len: Cardinal; output: PCefStringWide): Integer; cdecl;

  cef_string_utf8_to_utf16: function(const src: PAnsiChar; src_len: Cardinal; output: PCefStringUtf16): Integer; cdecl;
  cef_string_utf16_to_utf8: function(const src: PChar16; src_len: Cardinal; output: PCefStringUtf8): Integer; cdecl;

  cef_string_to_utf8: function(const src: PCefChar; src_len: Cardinal; output: PCefStringUtf8): Integer; cdecl;
  cef_string_from_utf8: function(const src: PAnsiChar; src_len: Cardinal; output: PCefString): Integer; cdecl;
  cef_string_to_utf16: function(const src: PCefChar; src_len: Cardinal; output: PCefStringUtf16): Integer; cdecl;
  cef_string_from_utf16: function(const src: PChar16; src_len: Cardinal; output: PCefString): Integer; cdecl;
  cef_string_to_wide: function(const src: PCefChar; src_len: Cardinal; output: PCefStringWide): Integer; cdecl;
  cef_string_from_wide: function(const src: PWideChar; src_len: Cardinal; output: PCefString): Integer; cdecl;

  // These functions convert an ASCII string, typically a hardcoded constant, to a
  // Wide/UTF16 string. Use instead of the UTF8 conversion routines if you know
  // the string is ASCII.

  cef_string_ascii_to_wide: function(const src: PAnsiChar; src_len: Cardinal; output: PCefStringWide): Integer; cdecl;
  cef_string_ascii_to_utf16: function(const src: PAnsiChar; src_len: Cardinal; output: PCefStringUtf16): Integer; cdecl;
  cef_string_from_ascii: function(const src: PAnsiChar; src_len: Cardinal; output: PCefString): Integer; cdecl;

  // These functions allocate a new string structure. They must be freed by
  // calling the associated free function.

  cef_string_userfree_wide_alloc: function(): PCefStringUserFreeWide; cdecl;
  cef_string_userfree_utf8_alloc: function(): PCefStringUserFreeUtf8; cdecl;
  cef_string_userfree_utf16_alloc: function(): PCefStringUserFreeUtf16; cdecl;
  cef_string_userfree_alloc: function(): PCefStringUserFree; cdecl;

  // These functions free the string structure allocated by the associated
  // alloc function. Any string contents will first be cleared.

  cef_string_userfree_wide_free: procedure(str: PCefStringUserFreeWide); cdecl;
  cef_string_userfree_utf8_free: procedure(str: PCefStringUserFreeUtf8); cdecl;
  cef_string_userfree_utf16_free: procedure(str: PCefStringUserFreeUtf16); cdecl;
  cef_string_userfree_free: procedure(str: PCefStringUserFree); cdecl;

// Convenience macros for copying values.
function cef_string_wide_copy(const src: PWideChar; src_len: Cardinal;  output: PCefStringWide): Integer;
begin
  Result := cef_string_wide_set(src, src_len, output, ord(True))
end;

function cef_string_utf8_copy(const src: PAnsiChar; src_len: Cardinal; output: PCefStringUtf8): Integer;
begin
  Result := cef_string_utf8_set(src, src_len, output, ord(True))
end;

function cef_string_utf16_copy(const src: PChar16; src_len: Cardinal; output: PCefStringUtf16): Integer; cdecl;
begin
  Result := cef_string_utf16_set(src, src_len, output, ord(True))
end;

function cef_string_copy(const src: PCefChar; src_len: Cardinal; output: PCefString): Integer; cdecl;
begin
  Result := cef_string_set(src, src_len, output, ord(True));
end;

var
  // Create a new browser window using the window parameters specified by
  // |windowInfo|. All values will be copied internally and the actual window will
  // be created on the UI thread. This function call will not block.
  cef_browser_create: function(windowInfo: PCefWindowInfo;
      client: PCefClient; const url: PCefString;
      const settings: PCefBrowserSettings): Integer; cdecl;

  // Create a new browser window using the window parameters specified by
  // |windowInfo|. This function should only be called on the UI thread.
  cef_browser_create_sync: function(windowInfo: PCefWindowInfo;
    client: PCefClient; const url: PCefString;
    const settings: PCefBrowserSettings): PCefBrowser; cdecl;

  // Perform a single iteration of CEF message loop processing. This function is
  // used to integrate the CEF message loop into an existing application message
  // loop. Care must be taken to balance performance against excessive CPU usage.
  // This function should only be called on the main application thread and only
  // if cef_initialize() is called with a CefSettings.multi_threaded_message_loop
  // value of false (0). This function will not block.
  cef_do_message_loop_work: procedure(); cdecl;

  // Run the CEF message loop. Use this function instead of an application-
  // provided message loop to get the best balance between performance and CPU
  // usage. This function should only be called on the main application thread and
  // only if cef_initialize() is called with a
  // CefSettings.multi_threaded_message_loop value of false (0). This function
  // will block until a quit message is received by the system.
  cef_run_message_loop: procedure; cdecl;

  // This function should be called on the main application thread to initialize
  // CEF when the application is started.  A return value of true (1) indicates
  // that it succeeded and false (0) indicates that it failed.
  cef_initialize: function(const settings: PCefSettings): Integer; cdecl;

  // This function should be called on the main application thread to shut down
  // CEF before the application exits.
  cef_shutdown: procedure(); cdecl;

  // Allocate a new string map.
  cef_string_map_alloc: function(): TCefStringMap; cdecl;
  //function cef_string_map_size(map: TCefStringMap): Integer; cdecl;
  cef_string_map_size: function(map: TCefStringMap): Integer; cdecl;
  // Return the value assigned to the specified key.
  cef_string_map_find: function(map: TCefStringMap; const key: PCefString; var value: TCefString): Integer; cdecl;
  // Return the key at the specified zero-based string map index.
  cef_string_map_key: function(map: TCefStringMap; index: Integer; var key: TCefString): Integer; cdecl;
  // Return the value at the specified zero-based string map index.
  cef_string_map_value: function(map: TCefStringMap; index: Integer; var value: TCefString): Integer; cdecl;
  // Append a new key/value pair at the end of the string map.
  cef_string_map_append: function(map: TCefStringMap; const key, value: PCefString): Integer; cdecl;
  // Clear the string map.
  cef_string_map_clear: procedure(map: TCefStringMap); cdecl;
  // Free the string map.
  cef_string_map_free: procedure(map: TCefStringMap); cdecl;

  // Allocate a new string map.
  cef_string_list_alloc: function(): TCefStringList; cdecl;
  // Return the number of elements in the string list.
  cef_string_list_size: function(list: TCefStringList): Integer; cdecl;
  // Retrieve the value at the specified zero-based string list index. Returns
  // true (1) if the value was successfully retrieved.
  cef_string_list_value: function(list: TCefStringList; index: Integer; value: PCefString): Integer; cdecl;
  // Append a new value at the end of the string list.
  cef_string_list_append: procedure(list: TCefStringList; const value: PCefString); cdecl;
  // Clear the string list.
  cef_string_list_clear: procedure(list: TCefStringList); cdecl;
  // Free the string list.
  cef_string_list_free: procedure(list: TCefStringList); cdecl;
  // Creates a copy of an existing string list.
  cef_string_list_copy: function(list: TCefStringList): TCefStringList;


  // Register a new V8 extension with the specified JavaScript extension code and
  // handler. Functions implemented by the handler are prototyped using the
  // keyword 'native'. The calling of a native function is restricted to the scope
  // in which the prototype of the native function is defined. This function may
  // be called on any thread.
  //
  // Example JavaScript extension code:
  //
  //   // create the 'example' global object if it doesn't already exist.
  //   if (!example)
  //     example = {};
  //   // create the 'example.test' global object if it doesn't already exist.
  //   if (!example.test)
  //     example.test = {};
  //   (function() {
  //     // Define the function 'example.test.myfunction'.
  //     example.test.myfunction = function() {
  //       // Call CefV8Handler::Execute() with the function name 'MyFunction'
  //       // and no arguments.
  //       native function MyFunction();
  //       return MyFunction();
  //     };
  //     // Define the getter function for parameter 'example.test.myparam'.
  //     example.test.__defineGetter__('myparam', function() {
  //       // Call CefV8Handler::Execute() with the function name 'GetMyParam'
  //       // and no arguments.
  //       native function GetMyParam();
  //       return GetMyParam();
  //     });
  //     // Define the setter function for parameter 'example.test.myparam'.
  //     example.test.__defineSetter__('myparam', function(b) {
  //       // Call CefV8Handler::Execute() with the function name 'SetMyParam'
  //       // and a single argument.
  //       native function SetMyParam();
  //       if(b) SetMyParam(b);
  //     });
  //
  //     // Extension definitions can also contain normal JavaScript variables
  //     // and functions.
  //     var myint = 0;
  //     example.test.increment = function() {
  //       myint += 1;
  //       return myint;
  //     };
  //   })();
  //
  // Example usage in the page:
  //
  //   // Call the function.
  //   example.test.myfunction();
  //   // Set the parameter.
  //   example.test.myparam = value;
  //   // Get the parameter.
  //   value = example.test.myparam;
  //   // Call another function.
  //   example.test.increment();
  //
  cef_register_extension: function(const extension_name,
    javascript_code: PCefString; handler: PCefv8Handler): Integer; cdecl;

  // Register a custom scheme. This function should not be called for the built-in
  // HTTP, HTTPS, FILE, FTP, ABOUT and DATA schemes.
  //
  // If |is_standard| is true (1) the scheme will be treated as a standard scheme.
  // Standard schemes are subject to URL canonicalization and parsing rules as
  // defined in the Common Internet Scheme Syntax RFC 1738 Section 3.1 available
  // at http://www.ietf.org/rfc/rfc1738.txt
  //
  // In particular, the syntax for standard scheme URLs must be of the form:
  //  [scheme]://[username]:[password]@[host]:[port]/[url-path]
  // </pre> Standard scheme URLs must have a host component that is a fully
  // qualified domain name as defined in Section 3.5 of RFC 1034 [13] and Section
  // 2.1 of RFC 1123. These URLs will be canonicalized to "scheme://host/path" in
  // the simplest case and "scheme://username:password@host:port/path" in the most
  // explicit case. For example, "scheme:host/path" and "scheme:///host/path" will
  // both be canonicalized to "scheme://host/path".
  //
  // For non-standard scheme URLs only the "scheme:" component is parsed and
  // canonicalized. The remainder of the URL will be passed to the handler as-is.
  // For example, "scheme:///some%20text" will remain the same. Non-standard
  // scheme URLs cannot be used as a target for form submission.

  // If |is_local| is true (1) the scheme will be treated as local (i.e., with the
  // same security rules as those applied to "file" URLs). This means that normal
  // pages cannot link to or access URLs of this scheme.
  //
  // If |is_display_isolated| is true (1) the scheme will be treated as display-
  // isolated. This means that pages cannot display these URLs unless they are
  // from the same scheme. For example, pages in another origin cannot create
  // iframes or hyperlinks to URLs with this scheme.
  //
  // This function may be called on any thread. It should only be called once per
  // unique |scheme_name| value. If |scheme_name| is already registered or if an
  // error occurs this function will return false (0).

  cef_register_custom_scheme: function(const scheme_name: PCefString;
      is_standard, is_local, is_display_isolated: Integer): Integer; cdecl;

  // Register a scheme handler factory for the specified |scheme_name| and
  // optional |domain_name|. An NULL |domain_name| value for a standard scheme
  // will cause the factory to match all domain names. The |domain_name| value
  // will be ignored for non-standard schemes. If |scheme_name| is a built-in
  // scheme and no handler is returned by |factory| then the built-in scheme
  // handler factory will be called. If |scheme_name| is a custom scheme the
  // cef_register_custom_scheme() function should be called for that scheme. This
  // function may be called multiple times to change or remove the factory that
  // matches the specified |scheme_name| and optional |domain_name|. Returns false
  // (0) if an error occurs. This function may be called on any thread.
  cef_register_scheme_handler_factory: function(
      const scheme_name, domain_name: PCefString;
      factory: PCefSchemeHandlerFactory): Integer; cdecl;

  // Clear all registered scheme handler factories. Returns false (0) on error.
  // This function may be called on any thread.
  cef_clear_scheme_handler_factories: function: Integer; cdecl;

  // Add an entry to the cross-origin access whitelist.
  //
  // The same-origin policy restricts how scripts hosted from different origins
  // (scheme + domain) can communicate. By default, scripts can only access
  // resources with the same origin. Scripts hosted on the HTTP and HTTPS schemes
  // (but no other schemes) can use the "Access-Control-Allow-Origin" header to
  // allow cross-origin requests. For example, https://source.example.com can make
  // XMLHttpRequest requests on http://target.example.com if the
  // http://target.example.com request returns an "Access-Control-Allow-Origin:
  // https://source.example.com" response header.
  //
  // Scripts in separate frames or iframes and hosted from the same protocol and
  // domain suffix can execute cross-origin JavaScript if both pages set the
  // document.domain value to the same domain suffix. For example,
  // scheme://foo.example.com and scheme://bar.example.com can communicate using
  // JavaScript if both domains set document.domain="example.com".
  //
  // This function is used to allow access to origins that would otherwise violate
  // the same-origin policy. Scripts hosted underneath the fully qualified
  // |source_origin| URL (like http://www.example.com) will be allowed access to
  // all resources hosted on the specified |target_protocol| and |target_domain|.
  // If |allow_target_subdomains| is true (1) access will also be allowed to all
  // subdomains of the target domain. This function may be called on any thread.
  // Returns false (0) if |source_origin| is invalid or the whitelist cannot be
  // accessed.
  cef_add_cross_origin_whitelist_entry: function(const source_origin, target_protocol,
    target_domain: PCefString; allow_target_subdomains: Integer): Integer; cdecl;

  // Remove an entry from the cross-origin access whitelist. Returns false (0) if
  // |source_origin| is invalid or the whitelist cannot be accessed.
  cef_remove_cross_origin_whitelist_entry: function(
      const source_origin, target_protocol, target_domain: PCefString;
      allow_target_subdomains: Integer): Integer; cdecl;

  // Remove all entries from the cross-origin access whitelist. Returns false (0)
  // if the whitelist cannot be accessed.
  cef_clear_cross_origin_whitelist: function: Integer; cdecl;

  // CEF maintains multiple internal threads that are used for handling different
  // types of tasks. The UI thread creates the browser window and is used for all
  // interaction with the WebKit rendering engine and V8 JavaScript engine (The UI
  // thread will be the same as the main application thread if cef_initialize() is
  // called with a CefSettings.multi_threaded_message_loop value of false (0).)
  // The IO thread is used for handling schema and network requests. The FILE
  // thread is used for the application cache and other miscellaneous activities.
  // This function will return true (1) if called on the specified thread.
  cef_currently_on: function(threadId: TCefThreadId): Integer; cdecl;

  // Post a task for execution on the specified thread. This function may be
  // called on any thread.
  cef_post_task: function(threadId: TCefThreadId; task: PCefTask): Integer; cdecl;

  // Post a task for delayed execution on the specified thread. This function may
  // be called on any thread.
  cef_post_delayed_task: function(threadId: TCefThreadId;
      task: PCefTask; delay_ms: LongInt): Integer; cdecl;

  // Parse the specified |url| into its component parts. Returns false (0) if the
  // URL is NULL or invalid.
  cef_parse_url: function(const url: PCefString; var parts: TCefUrlParts): Integer; cdecl;

  // Creates a URL from the specified |parts|, which must contain a non-NULL spec
  // or a non-NULL host and path (at a minimum), but not both. Returns false (0)
  // if |parts| isn't initialized as described.
  cef_create_url: function(parts: PCefUrlParts; url: PCefString): Integer; cdecl;

  // Visit all cookies. The returned cookies are ordered by longest path, then by
  // earliest creation date. Returns false (0) if cookies cannot be accessed.
  cef_visit_all_cookies: function(visitor: PCefCookieVisitor): Integer; cdecl;

  // Visit a subset of cookies. The results are filtered by the given url scheme,
  // host, domain and path. If |includeHttpOnly| is true (1) HTTP-only cookies
  // will also be included in the results. The returned cookies are ordered by
  // longest path, then by earliest creation date. Returns false (0) if cookies
  // cannot be accessed.
  cef_visit_url_cookies: function(const url: PCefString; includeHttpOnly: Integer;
    visitor: PCefCookieVisitor): Integer; cdecl;

  // Sets a cookie given a valid URL and explicit user-provided cookie attributes.
  // This function expects each attribute to be well-formed. It will check for
  // disallowed characters (e.g. the ';' character is disallowed within the cookie
  // value attribute) and will return false (0) without setting the cookie if such
  // characters are found. This function must be called on the IO thread.
  cef_set_cookie: function(const url: PCefString; cookie: PCefCookie): Integer; cdecl;

  // Delete all cookies that match the specified parameters. If both |url| and
  // |cookie_name| are specified all host and domain cookies matching both values
  // will be deleted. If only |url| is specified all host cookies (but not domain
  // cookies) irrespective of path will be deleted. If |url| is NULL all cookies
  // for all hosts and domains will be deleted. Returns false (0) if a non-NULL
  // invalid URL is specified or if cookies cannot be accessed. This function must
  // be called on the IO thread.
  cef_delete_cookies: function(const url, cookie_name: PCefString): Integer; cdecl;

  // Visit storage of the specified type. If |origin| is non-NULL only data
  // matching that origin will be visited. If |key| is non-NULL only data matching
  // that key will be visited. Otherwise, all data for the storage type will be
  // visited. Returns false (0) if the storage cannot be accessed. Origin should
  // be of the form scheme://domain.
  cef_visit_storage: function(type_: TCefStorageType;
    const origin, key: PCefString; visitor: PCefStorageVisitor): Integer; cdecl;

  // Sets storage of the specified type, origin, key and value. Returns false (0)
  // if storage cannot be accessed. This function must be called on the UI thread.
  cef_set_storage: function(type_: TCefStorageType;
    const origin, key, value: PCefString): Integer; cdecl;

  // Deletes all storage of the specified type. If |origin| is non-NULL only data
  // matching that origin will be cleared. If |key| is non-NULL only data matching
  // that key will be cleared. Otherwise, all data for the storage type will be
  // cleared. Returns false (0) if storage cannot be accessed. This function must
  // be called on the UI thread.
  cef_delete_storage: function(type_: TCefStorageType;
    const origin, key: PCefString): Integer; cdecl;

  // Create a new TCefRequest object.
  cef_request_create: function(): PCefRequest; cdecl;

  // Create a new TCefPostData object.
  cef_post_data_create: function(): PCefPostData; cdecl;

  // Create a new cef_post_data_Element object.
  cef_post_data_element_create: function(): PCefPostDataElement; cdecl;

  // Create a new cef_stream_reader_t object from a file.
  cef_stream_reader_create_for_file: function(const fileName: PCefString): PCefStreamReader; cdecl;
  // Create a new cef_stream_reader_t object from data.
  cef_stream_reader_create_for_data: function(data: Pointer; size: Cardinal): PCefStreamReader; cdecl;
  // Create a new cef_stream_reader_t object from a custom handler.
  cef_stream_reader_create_for_handler: function(handler: PCefReadHandler): PCefStreamReader; cdecl;

  // Create a new cef_stream_writer_t object for a file.
  cef_stream_writer_create_for_file: function(const fileName: PCefString): PCefStreamWriter; cdecl;
  // Create a new cef_stream_writer_t object for a custom handler.
  cef_stream_writer_create_for_handler: function(handler: PCefWriteHandler): PCefStreamWriter; cdecl;

  // Returns the current (top) context object in the V8 context stack.
  cef_v8context_get_current_context: function(): PCefv8Context; cdecl;

  // Returns the entered (bottom) context object in the V8 context stack.
  cef_v8context_get_entered_context: function(): PCefv8Context; cdecl;

  // Create a new cef_v8value_t object of type undefined.
  cef_v8value_create_undefined: function(): PCefv8Value; cdecl;
  // Create a new cef_v8value_t object of type null.
  cef_v8value_create_null: function(): PCefv8Value; cdecl;
  // Create a new cef_v8value_t object of type bool.
  cef_v8value_create_bool: function(value: Integer): PCefv8Value; cdecl;
  // Create a new cef_v8value_t object of type int.
  cef_v8value_create_int: function(value: Integer): PCefv8Value; cdecl;
  // Create a new cef_v8value_t object of type double.
  cef_v8value_create_double: function(value: Double): PCefv8Value; cdecl;
  // Create a new cef_v8value_t object of type Date.
  cef_v8value_create_date: function(const value: PCefTime): PCefv8Value; cdecl;
  // Create a new cef_v8value_t object of type string.
  cef_v8value_create_string: function(const value: PCefString): PCefv8Value; cdecl;
  // Create a new cef_v8value_t object of type object.
  cef_v8value_create_object: function(user_data: PCefBase): PCefv8Value; cdecl;
  // Create a new cef_v8value_t object of type object with accessors.
  cef_v8value_create_object_with_accessor: function(user_data: PCefBase;
    accessor: PCefV8Accessor): PCefv8Value; cdecl;
  // Create a new cef_v8value_t object of type array.
  cef_v8value_create_array: function(): PCefv8Value; cdecl;
  // Create a new cef_v8value_t object of type function.
  cef_v8value_create_function: function(const name: PCefString; handler: PCefv8Handler): PCefv8Value; cdecl;

  // Create a new CefWebUrlRequest object.
  cef_web_urlrequest_create: function(request: PCefRequest; client: PCefWebUrlRequestClient): PCefWebUrlRequest; cdecl;

  // Create a new cef_xml_reader_t object. The returned object's functions can
  // only be called from the thread that created the object.
  cef_xml_reader_create: function(stream: PCefStreamReader;
    encodingType: TCefXmlEncodingType; const URI: PCefString): PCefXmlReader; cdecl;

  // Create a new cef_zip_reader_t object. The returned object's functions can
  // only be called from the thread that created the object.
  cef_zip_reader_create: function(stream: PCefStreamReader): PCefZipReader; cdecl;

function CefGetData(const i: ICefBase): Pointer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  if i <> nil then
    Result := i.Wrap else
    Result := nil;
end;

function CefGetObject(ptr: Pointer): TObject; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Dec(PByte(ptr), SizeOf(Pointer));
  Result := TObject(PPointer(ptr)^);
end;

function CefParseUrl(const url: ustring; var parts: TCefUrlParts): Boolean;
var
  u: TCefString;
begin
  FillChar(parts, sizeof(parts), 0);
  u := CefString(url);
  Result := cef_parse_url(@u, parts) <> 0;
end;

function CefTimeToSystemTime(const dt: TCefTime): TSystemTime;
begin
  Result.wYear := dt.year;
  Result.wMonth := dt.month;
  Result.wDayOfWeek := dt.day_of_week;
  Result.wDay := dt.day_of_month;
  Result.wHour := dt.hour;
  Result.wMinute := dt.minute;
  Result.wSecond := dt.second;
  Result.wMilliseconds := dt.millisecond;
end;

function SystemTimeToCefTime(const dt: TSystemTime): TCefTime;
begin
  Result.year := dt.wYear;
  Result.month := dt.wMonth;
  Result.day_of_week := dt.wDayOfWeek;
  Result.day_of_month := dt.wDay;
  Result.hour := dt.wHour;
  Result.minute := dt.wMinute;
  Result.second := dt.wSecond;
  Result.millisecond := dt.wMilliseconds;
end;

function CefTimeToDateTime(const dt: TCefTime): TDateTime;
var
  st: TSystemTime;
begin
  st := CefTimeToSystemTime(dt);
  SystemTimeToTzSpecificLocalTime(nil, @st, @st);
  Result := SystemTimeToDateTime(st);
end;

function DateTimeToCefTime(dt: TDateTime): TCefTime;
var
  st: TSystemTime;
begin
  DateTimeToSystemTime(dt, st);
  TzSpecificLocalTimeToSystemTime(nil, @st, @st);
  Result := SystemTimeToCefTime(st);
end;

function CefVisitAllCookies(const visitor: ICefCookieVisitor): Boolean;
begin
  Result := cef_visit_all_cookies(CefGetData(visitor)) <> 0;
end;

function CefVisitAllCookies(const visitor: TCefCookieVisitorProc): Boolean;
begin
  Result := CefVisitAllCookies(TCefFastCookieVisitor.Create(visitor) as ICefCookieVisitor)
end;

function CefVisitUrlCookies(const url: ustring; includeHttpOnly: Boolean; const visitor: ICefCookieVisitor): Boolean;
var
  str: TCefString;
begin
  str := CefString(url);
  Result := cef_visit_url_cookies(@str, Ord(includeHttpOnly), CefGetData(visitor)) <> 0;
end;

function CefVisitUrlCookies(const url: ustring; includeHttpOnly: Boolean; const visitor: TCefCookieVisitorProc): Boolean;
begin
  Result := CefVisitUrlCookies(url, includeHttpOnly, TCefFastCookieVisitor.Create(visitor) as ICefCookieVisitor);
end;

function CefSetCookie(const url: ustring; const name, value, domain, path: ustring; secure, httponly,
  hasExpires: Boolean; const creation, lastAccess, expires: TDateTime): Boolean;
var
  str: TCefString;
  cook: TCefCookie;
begin
  str := CefString(url);
  cook.name := CefString(name);
  cook.value := CefString(value);
  cook.domain := CefString(domain);
  cook.path := CefString(path);
  cook.secure := secure;
  cook.httponly := httponly;
  cook.creation := DateTimeToCefTime(creation);
  cook.last_access := DateTimeToCefTime(lastAccess);
  cook.has_expires := hasExpires;
  if hasExpires then
    cook.expires := DateTimeToCefTime(expires) else
    FillChar(cook.expires, SizeOf(TCefTime), 0);
  Result := cef_set_cookie(@str, @cook) <> 0;
end;

function CefDeleteCookies(const url, cookieName: ustring): Boolean;
var
  u, c: TCefString;
begin
  u := CefString(url);
  c := CefString(cookieName);
  Result := cef_delete_cookies(@u, @c) <> 0;
end;

function CefVisitStorage(StorageType: TCefStorageType;
  const origin, key: ustring; const visitor: ICefStorageVisitor): Boolean;
var
  o, k: TCefString;
begin
  o := CefString(origin);
  k := CefString(key);
  Result := cef_visit_storage(StorageType, @o, @k, CefGetData(visitor)) <> 0;
end;

function CefVisitStorage(StorageType: TCefStorageType;
  const origin, key: ustring; const visitor: TCefStorageVisitorProc): Boolean;
begin
  Result := CefVisitStorage(StorageType, origin, key,
    TCefFastStorageVisitor.Create(visitor) as ICefStorageVisitor);
end;

function CefSetStorage(StorageType: TCefStorageType;
  const origin, key, value: ustring): Boolean;
var
  o, k, v: TCefString;
begin
  o := CefString(origin);
  k := CefString(key);
  v := CefString(value);
  Result := cef_set_storage(StorageType, @o, @k, @v) <> 0;
end;

function CefDeleteStorage(StorageType: TCefStorageType;
  const origin, key: ustring): Boolean;
var
  o, k: TCefString;
begin
  o := CefString(origin);
  k := CefString(key);
  Result := cef_delete_storage(StorageType, @o, @k) <> 0;
end;

{ cef_base }

function cef_base_add_ref(self: PCefBase): Integer; stdcall;
begin
  Result := TCefBaseOwn(CefGetObject(self))._AddRef;
end;

function cef_base_release(self: PCefBase): Integer; stdcall;
begin
  Result := TCefBaseOwn(CefGetObject(self))._Release;
end;

function cef_base_get_refct(self: PCefBase): Integer; stdcall;
begin
  Result := TCefBaseOwn(CefGetObject(self)).FRefCount;
end;

{ cef_client }

function cef_client_get_life_span_handler(self: PCefClient): PCefLifeSpanHandler; stdcall;
begin
  with TCefClientOwn(CefGetObject(self)) do
    Result := CefGetData(GetLifeSpanHandler);
end;

function cef_client_get_load_handler(self: PCefClient): PCefLoadHandler; stdcall;
begin
  with TCefClientOwn(CefGetObject(self)) do
    Result := CefGetData(GetLoadHandler);
end;

function cef_client_get_request_handler(self: PCefClient): PCefRequestHandler; stdcall;
begin
  with TCefClientOwn(CefGetObject(self)) do
    Result := CefGetData(GetRequestHandler);
end;

function cef_client_get_display_handler(self: PCefClient): PCefDisplayHandler; stdcall;
begin
  with TCefClientOwn(CefGetObject(self)) do
    Result := CefGetData(GetDisplayHandler);
end;

function cef_client_get_focus_handler(self: PCefClient): PCefFocusHandler; stdcall;
begin
  with TCefClientOwn(CefGetObject(self)) do
    Result := CefGetData(GetFocusHandler);
end;

function cef_client_get_keyboard_handler(self: PCefClient): PCefKeyboardHandler; stdcall;
begin
  with TCefClientOwn(CefGetObject(self)) do
    Result := CefGetData(GetKeyboardHandler);
end;

function cef_client_get_menu_handler(self: PCefClient): PCefMenuHandler; stdcall;
begin
  with TCefClientOwn(CefGetObject(self)) do
    Result := CefGetData(GetMenuHandler);
end;

function cef_client_get_print_handler(self: PCefClient): PCefPrintHandler; stdcall;
begin
  with TCefClientOwn(CefGetObject(self)) do
    Result := CefGetData(GetPrintHandler);
end;

function cef_client_get_find_handler(self: PCefClient): PCefFindHandler; stdcall;
begin
  with TCefClientOwn(CefGetObject(self)) do
    Result := CefGetData(GetFindHandler);
end;

function cef_client_get_jsdialog_handler(self: PCefClient): PCefJsDialogHandler; stdcall;
begin
  with TCefClientOwn(CefGetObject(self)) do
    Result := CefGetData(GetJsdialogHandler);
end;

function cef_client_get_jsbinding_handler(self: PCefClient): PCefJsBindingHandler; stdcall;
begin
  with TCefClientOwn(CefGetObject(self)) do
    Result := CefGetData(GetJsbindingHandler);
end;

function cef_client_get_render_handler(self: PCefClient): PCefRenderHandler; stdcall;
begin
  with TCefClientOwn(CefGetObject(self)) do
    Result := CefGetData(GetRenderHandler);
end;

function cef_client_get_drag_handler(self: PCefClient): PCefDragHandler; stdcall;
begin
  with TCefClientOwn(CefGetObject(self)) do
    Result := CefGetData(GetDragHandler);
end;

{ cef_life_span_handler }

function cef_life_span_handler_on_before_popup(self: PCefLifeSpanHandler; parentBrowser: PCefBrowser;
   const popupFeatures: PCefPopupFeatures; windowInfo: PCefWindowInfo; const url: PCefString;
   var client: PCefClient; settings: PCefBrowserSettings): Integer; stdcall;
var
  _url: ustring;
  _client: ICefBase;
begin
  _url := CefString(url);
  _client := TCefBaseRef.UnWrap(client);
  with TCefLifeSpanHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnBeforePopup(
      TCefBrowserRef.UnWrap(parentBrowser),
      popupFeatures^,
      windowInfo^,
      _url,
      _client,
      settings^
    ));
  CefStringSet(url, _url);
  client := CefGetData(_client);
  _client := nil;
end;

procedure cef_life_span_handler_on_after_created(self: PCefLifeSpanHandler; browser: PCefBrowser); stdcall;
begin
  with TCefLifeSpanHandlerOwn(CefGetObject(self)) do
    OnAfterCreated(TCefBrowserRef.UnWrap(browser));
end;

procedure cef_life_span_handler_on_before_close(self: PCefLifeSpanHandler; browser: PCefBrowser); stdcall;
begin
  with TCefLifeSpanHandlerOwn(CefGetObject(self)) do
    OnBeforeClose(TCefBrowserRef.UnWrap(browser));
end;

function cef_life_span_handler_run_modal(self: PCefLifeSpanHandler; browser: PCefBrowser): Integer; stdcall;
begin
  with TCefLifeSpanHandlerOwn(CefGetObject(self)) do
    Result := Ord(RunModal(TCefBrowserRef.UnWrap(browser)));
end;

function cef_life_span_handler_do_close(self: PCefLifeSpanHandler; browser: PCefBrowser): Integer; stdcall;
begin
  with TCefLifeSpanHandlerOwn(CefGetObject(self)) do
    Result := Ord(DoClose(TCefBrowserRef.UnWrap(browser)));
end;

{ cef_load_handler }

procedure cef_load_handler_on_load_start(self: PCefLoadHandler;
  browser: PCefBrowser; frame: PCefFrame); stdcall;
begin
  with TCefLoadHandlerOwn(CefGetObject(self)) do
    OnLoadStart(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame));
end;

procedure cef_load_handler_on_load_end(self: PCefLoadHandler;
  browser: PCefBrowser; frame: PCefFrame; httpStatusCode: Integer); stdcall;
begin
  with TCefLoadHandlerOwn(CefGetObject(self)) do
    OnLoadEnd(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame), httpStatusCode);
end;

function cef_load_handler_on_load_error(self: PCefLoadHandler; browser: PCefBrowser;
  frame: PCefFrame; errorCode: TCefHandlerErrorcode; const failedUrl: PCefString;
  var errorText: TCefString): Integer; stdcall;
var
  err: ustring;
begin
  err := CefString(@errorText);
  with TCefLoadHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnLoadError(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      errorCode, CefString(failedUrl), err));
  if Result <> 0 then
    CefStringSet(@errorText, err);
end;

{ cef_content_filter }

procedure cef_content_filter_process_data(self: PCefContentFilter;
  const data: Pointer; data_size: Integer; var substitute_data: PCefStreamReader); stdcall;
var
  stream: ICefStreamReader;
begin
  stream := TCefStreamReaderRef.UnWrap(substitute_data);
  with TCefContentFilterOwn(CefGetObject(self)) do
    ProcessData(data, data_size, stream);
  substitute_data := CefGetData(stream);
end;

procedure cef_content_filter_drain(self: PCefContentFilter; var remainder: PCefStreamReader); stdcall;
var
  stream: ICefStreamReader;
begin
  with TCefContentFilterOwn(CefGetObject(self)) do
    Drain(stream);
  remainder := CefGetData(stream);
end;

{ cef_request_handler }

function cef_request_handler_on_before_browse(self: PCefRequestHandler;
  browser: PCefBrowser; frame: PCefFrame; request: PCefRequest;
  navType: TCefHandlerNavtype; isRedirect: Integer): Integer; stdcall;
begin
  with TCefRequestHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnBeforeBrowse(
      TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame),
      TCefRequestRef.UnWrap(request),
      navType,
      isRedirect <> 0));
end;

function cef_request_handler_on_before_resource_load(
  self: PCefRequestHandler; browser: PCefBrowser; request: PCefRequest;
  redirectUrl: PCefString; var resourceStream: PCefStreamReader;
  response: PCefResponse; loadFlags: Integer): Integer; stdcall;
var
  _redirectUrl: ustring;
  _resourceStream: ICefStreamReader;
begin
  with TCefRequestHandlerOwn(CefGetObject(self)) do
  begin
    _redirectUrl := CefString(redirectUrl);
    _resourceStream := TCefStreamReaderRef.UnWrap(resourceStream);

    Result := Ord(OnBeforeResourceLoad(
      TCefBrowserRef.UnWrap(browser),
      TCefRequestRef.UnWrap(request),
      _redirectUrl,
      _resourceStream,
      TCefResponseRef.UnWrap(response),
      loadFlags));

    if Result = 0 then
    begin
      if _redirectUrl <> '' then
         CefStringSet(redirectUrl, _redirectUrl);
      resourceStream := CefGetData(_resourceStream);
    end;
  end;
end;

procedure cef_request_handler_on_resource_response(self: PCefRequestHandler;
  browser: PCefBrowser; const url: PCefString; response: PCefResponse;
  var filter: PCefContentFilter); stdcall;
var
  fltr: ICefBase;
begin
   fltr := TCefBaseRef.UnWrap(filter);
   with TCefRequestHandlerOwn(CefGetObject(self)) do
     OnResourceResponse(
       TCefBrowserRef.UnWrap(browser),
       CefString(url),
       TCefResponseRef.UnWrap(response),
       fltr);
   filter := CefGetData(fltr);
end;

function cef_request_handler_on_protocol_execution(self: PCefRequestHandler;
  browser: PCefBrowser; const url: PCefString; var allowOSExecution: Integer): Integer; stdcall;
var
  allow: Boolean;
begin
  allow := allowOSExecution <> 0;
  with TCefRequestHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnProtocolExecution(
      TCefBrowserRef.UnWrap(browser),
      CefString(url), allow));
  if allow then
    allowOSExecution := 1 else
    allowOSExecution := 0;
end;

function cef_request_handler_get_download_handler(self: PCefRequestHandler;
  browser: PCefBrowser; const mimeType: PCefString; const fileName: PCefString;
  contentLength: int64; var handler: PCefDownloadHandler): Integer; stdcall;
var
  _handler: ICefDownloadHandler;
begin
  with TCefRequestHandlerOwn(CefGetObject(self)) do
    Result := Ord(GetDownloadHandler(
      TCefBrowserRef.UnWrap(browser),
      CefString(mimeType), CefString(fileName), contentLength, _handler));
  handler := CefGetData(_handler);
end;

function cef_request_handler_get_auth_credentials(self: PCefRequestHandler;
  browser: PCefBrowser; isProxy: Integer; const host: PCefString;
  port: Integer; const realm: PCefString; const scheme: PCefString;
  username, password: PCefString): Integer; stdcall;
var
  _username, _password: ustring;
begin
  _username := CefString(username);
  _password := CefString(password);
  with TCefRequestHandlerOwn(CefGetObject(self)) do
    Result := Ord(GetAuthCredentials(
      TCefBrowserRef.UnWrap(browser), isProxy <> 0,
      port, CefString(host), CefString(realm), CefString(scheme),
      _username, _password));
  if Result <> 0 then
  begin
    CefStringSet(username, _username);
    CefStringSet(password, _password);
  end;
end;

{ cef_display_handler }

procedure cef_display_handler_on_nav_state_change(self: PCefDisplayHandler;
  browser: PCefBrowser; canGoBack, canGoForward: Integer); stdcall;
begin
  with TCefDisplayHandlerOwn(CefGetObject(self)) do
    OnNavStateChange(TCefBrowserRef.UnWrap(browser), canGoBack <> 0, canGoForward <> 0);
end;

procedure cef_display_handler_on_address_change(self: PCefDisplayHandler;
  browser: PCefBrowser; frame: PCefFrame; const url: PCefString); stdcall;
begin
  with TCefDisplayHandlerOwn(CefGetObject(self)) do
   OnAddressChange(
     TCefBrowserRef.UnWrap(browser),
     TCefFrameRef.UnWrap(frame),
     cefstring(url))
end;

procedure cef_display_handler_on_title_change(self: PCefDisplayHandler;
  browser: PCefBrowser; const title: PCefString); stdcall;
begin
  with TCefDisplayHandlerOwn(CefGetObject(self)) do
    OnTitleChange(TCefBrowserRef.UnWrap(browser), CefString(title));
end;

function cef_display_handler_on_tooltip(self: PCefDisplayHandler;
  browser: PCefBrowser; text: PCefString): Integer; stdcall;
var
  t: ustring;
begin
  t := CefStringClearAndGet(text^);
  with TCefDisplayHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnTooltip(
      TCefBrowserRef.UnWrap(browser), t));
  text^ := CefStringAlloc(t);
end;

procedure cef_display_handler_on_status_message(self: PCefDisplayHandler;
  browser: PCefBrowser; const value: PCefString; kind: TCefHandlerStatusType); stdcall;
begin
  with TCefDisplayHandlerOwn(CefGetObject(self)) do
    OnStatusMessage(TCefBrowserRef.UnWrap(browser), CefString(value), kind);
end;

function cef_display_handler_on_console_message(self: PCefDisplayHandler;
    browser: PCefBrowser; const message: PCefString;
    const source: PCefString; line: Integer): Integer; stdcall;
begin
  with TCefDisplayHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnConsoleMessage(TCefBrowserRef.UnWrap(browser),
    CefString(message), CefString(source), line));
end;

{ cef_focus_handler }

procedure cef_focus_handler_on_take_focus(self: PCefFocusHandler;
  browser: PCefBrowser; next: Integer); stdcall;
begin
  with TCefFocusHandlerOwn(CefGetObject(self)) do
    OnTakeFocus(TCefBrowserRef.UnWrap(browser), next <> 0);
end;

function cef_focus_handler_on_set_focus(self: PCefFocusHandler;
  browser: PCefBrowser; source: TCefHandlerFocusSource): Integer; stdcall;
begin
  with TCefFocusHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnSetFocus(TCefBrowserRef.UnWrap(browser), source))
end;

{ cef_keyboard_handler }

function cef_keyboard_handler_on_key_event(self: PCefKeyboardHandler;
  browser: PCefBrowser; kind: TCefHandlerKeyEventType;
  code, modifiers, isSystemKey: Integer): Integer; stdcall;
begin
  with TCefKeyboardHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnKeyEvent(TCefBrowserRef.UnWrap(browser), kind, code,
      modifiers, isSystemKey <> 0));
end;

{ cef_menu_handler }

function cef_menu_handler_on_before_menu(self: PCefMenuHandler;
  browser: PCefBrowser; const menuInfo: PCefHandlerMenuInfo): Integer; stdcall;
begin
  with TCefMenuHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnBeforeMenu(TCefBrowserRef.UnWrap(browser), menuInfo));
end;

procedure cef_menu_handler_get_menu_label(self: PCefMenuHandler;
  browser: PCefBrowser; menuId: TCefHandlerMenuId; var label_: TCefString); stdcall;
var
  str: ustring;
begin
  str := CefString(@label_);
  with TCefMenuHandlerOwn(CefGetObject(self)) do
  begin
    GetMenuLabel(
      TCefBrowserRef.UnWrap(browser),
      menuId,
      str);
    CefStringSet(@label_, str);
  end;
end;

function cef_menu_handler_on_menu_action(self: PCefMenuHandler;
  browser: PCefBrowser; menuId: TCefHandlerMenuId): Integer; stdcall;
begin
  with TCefMenuHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnMenuAction(TCefBrowserRef.UnWrap(browser), menuId));
end;

{ cef_print_handler }

function cef_print_handler_get_print_options(self: PCefPrintHandler;
    browser: PCefBrowser; printOptions: PCefPrintOptions): Integer; stdcall;
begin
  with TCefPrintHandlerOwn(CefGetObject(self)) do
    Result := Ord(GetPrintOptions(
      TCefBrowserRef.UnWrap(browser), printOptions));
end;

function cef_print_handler_get_print_header_footer(self: PCefPrintHandler;
  browser: PCefBrowser; frame: PCefFrame; const printInfo: PCefPrintInfo;
  const url: PCefString; const title: PCefString; currentPage,
  maxPages: Integer; var topLeft, topCenter, topRight, bottomLeft,
  bottomCenter, bottomRight: TCefString): Integer; stdcall;
var
  _topLeft, _topCenter, _topRight, _bottomLeft, _bottomCenter, _bottomRight: ustring;
begin
  with TCefPrintHandlerOwn(CefGetObject(self)) do
  begin
    Result := Ord(GetPrintHeaderFooter(
      TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame),
      printInfo, CefString(url), CefString(title), currentPage, maxPages,
      _topLeft, _topCenter, _topRight, _bottomLeft, _bottomCenter, _bottomRight
    ));
    if Result = 0 then
    begin
      topLeft := CefStringAlloc(_topLeft);
      topCenter := CefStringAlloc(_topCenter);
      topRight := CefStringAlloc(_topRight);
      bottomLeft := CefStringAlloc(_bottomLeft);
      bottomCenter := CefStringAlloc(_bottomCenter);
      bottomRight := CefStringAlloc(_bottomRight);
    end;
  end;
end;

{ cef_find_handler }

procedure cef_find_handler_on_find_result(self: PCefFindHandler; browser: PCefBrowser;
  identifier, count: Integer; const selectionRect: PCefRect; activeMatchOrdinal,
  finalUpdate: Integer); stdcall;
begin
  with TCefFindHandlerOwn(CefGetObject(self)) do
    OnFindResult(TCefBrowserRef.UnWrap(browser),
      count, selectionRect, identifier <> 0, activeMatchOrdinal <> 0, finalUpdate <> 0);
end;


{ cef_jsdialog_handler }

function cef_jsdialog_handler_on_jsalert(self: PCefJsDialogHandler;
  browser: PCefBrowser; frame: PCefFrame; const message: PCefString): Integer; stdcall;
begin
  with TCefJsDialogHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnJsAlert(TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame), CefString(message)));
end;

function cef_jsdialog_handler_on_jsconfirm(self: PCefJsDialogHandler;
  browser: PCefBrowser; frame: PCefFrame; const message: PCefString;
  var retval: Integer): Integer; stdcall;
var
  ret: Boolean;
begin
  ret := retval <> 0;
  with TCefJsDialogHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnJsConfirm(TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame), CefString(message), ret));
  if Result <> 0 then
    retval := Ord(ret);
end;

function cef_jsdialog_handler_on_jsprompt(self: PCefJsDialogHandler;
  browser: PCefBrowser; frame: PCefFrame; const message, defaultValue: PCefString;
  var retval: Integer; var return: TCefString): Integer; stdcall;
var
  ret: Boolean;
  str: ustring;
begin
  ret := retval <> 0;
  with TCefJsDialogHandlerOwn(CefGetObject(self)) do
  begin
    Result := Ord(OnJsPrompt(
      TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame),
      CefString(message), CefString(defaultValue), ret, str));
    if Result <> 0 then
    begin
      retval := Ord(ret);
      return := CefStringAlloc(str)
    end;
  end;
end;

{ cef_jsbinding_handler }

procedure cef_jsbinding_handler_on_jsbinding(self: PCefJsBindingHandler;
  browser: PCefBrowser; frame: PCefFrame; obj: PCefv8Value); stdcall;
begin
  with TCefJsBindingHandlerOwn(CefGetObject(self)) do
    OnJsBinding(
      TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame),
      TCefv8ValueRef.UnWrap(obj));
end;

{ cef_render_handler }

function cef_render_handler_get_view_rect(self: PCefRenderHandler;
  browser: PCefBrowser; rect: PCefRect): Integer; stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    Result := Ord(GetViewRect(TCefBrowserRef.UnWrap(browser), rect));
end;

function cef_render_handler_get_screen_rect(self: PCefRenderHandler;
  browser: PCefBrowser; rect: PCefRect): Integer; stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    Result := Ord(GetScreenRect(TCefBrowserRef.UnWrap(browser), rect));
end;

function cef_render_handler_get_screen_point(self: PCefRenderHandler;
  browser: PCefBrowser; viewX, viewY: Integer; screenX, screenY: PInteger): Integer; stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    Result := Ord(GetScreenPoint(TCefBrowserRef.UnWrap(browser), viewX, viewY, screenX, screenY));
end;

procedure cef_render_handler_on_popup_show(self: PCefRenderHandler;
  browser: PCefBrowser; show: Integer); stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    OnPopupShow(TCefBrowserRef.UnWrap(browser), show <> 0);
end;

procedure cef_render_handler_on_popup_size(self: PCefRenderHandler;
  browser: PCefBrowser; const rect: PCefRect); stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    OnPopupSize(TCefBrowserRef.UnWrap(browser), rect);
end;

procedure cef_render_handler_on_paint(self: PCefRenderHandler;
  browser: PCefBrowser; kind: TCefPaintElementType;
  const dirtyRect: PCefRect; const buffer: Pointer); stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    OnPaint(TCefBrowserRef.UnWrap(browser), kind, dirtyRect, buffer);
end;

procedure cef_render_handler_on_cursor_change(self: PCefRenderHandler;
  browser: PCefBrowser; cursor: TCefCursorHandle); stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    OnCursorChange(TCefBrowserRef.UnWrap(browser), cursor);
end;

{ cef_drag_handler }

function cef_drag_handler_on_drag_start(self: PCefDragHandler; browser: PCefBrowser;
  dragData: PCefDragData; mask: TCefDragOperations): Integer; stdcall;
begin
  with TCefDragHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnDragStart(TCefBrowserRef.UnWrap(browser),
      TCefDragDataRef.UnWrap(dragData), mask));
end;

function cef_drag_handler_on_drag_enter(self: PCefDragHandler; browser: PCefBrowser;
  dragData: PCefDragData; mask: TCefDragOperations): Integer; stdcall;
begin
  with TCefDragHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnDragEnter(TCefBrowserRef.UnWrap(browser),
      TCefDragDataRef.UnWrap(dragData), mask));
end;

{  cef_stream_reader }

function cef_read_handler_read(self: PCefReadHandler; ptr: Pointer; size, n: Cardinal): Cardinal; stdcall;
begin
  with TCefCustomStreamReader(CefGetObject(self)) do
    Result := Read(ptr, size, n);
end;

function cef_read_handler_seek(self: PCefReadHandler; offset: LongInt; whence: Integer): Integer; stdcall;
begin
  with TCefCustomStreamReader(CefGetObject(self)) do
    Result := Seek(offset, whence);
end;

function cef_read_handler_tell(self: PCefReadHandler): LongInt; stdcall;
begin
  with TCefCustomStreamReader(CefGetObject(self)) do
    Result := Tell;
end;

function cef_read_handler_eof(self: PCefReadHandler): Integer; stdcall;
begin
  with TCefCustomStreamReader(CefGetObject(self)) do
    Result := Ord(eof);
end;

{ cef_post_data_element }

procedure cef_post_data_element_set_to_empty(self: PCefPostDataElement); stdcall;
begin
  with TCefPostDataElementOwn(CefGetObject(self)) do
    SetToEmpty;
end;

procedure cef_post_data_element_set_to_file(self: PCefPostDataElement; const fileName: PCefString); stdcall;
begin
  with TCefPostDataElementOwn(CefGetObject(self)) do
    SetToFile(CefString(fileName));
end;

procedure cef_post_data_element_set_to_bytes(self: PCefPostDataElement; size: Cardinal; const bytes: Pointer); stdcall;
begin
  with TCefPostDataElementOwn(CefGetObject(self)) do
    SetToBytes(size, bytes);
end;

function cef_post_data_element_get_type(self: PCefPostDataElement): TCefPostDataElementType; stdcall;
begin
  with TCefPostDataElementOwn(CefGetObject(self)) do
    Result := GetType;
end;

function cef_post_data_element_get_file(self: PCefPostDataElement): TCefString; stdcall;
begin
  with TCefPostDataElementOwn(CefGetObject(self)) do
    Result := CefStringAlloc(GetFile);
end;

function cef_post_data_element_get_bytes_count(self: PCefPostDataElement): Cardinal; stdcall;
begin
  with TCefPostDataElementOwn(CefGetObject(self)) do
    Result := GetBytesCount;
end;

function cef_post_data_element_get_bytes(self: PCefPostDataElement; size: Cardinal; bytes: Pointer): Cardinal; stdcall;
begin
  with TCefPostDataElementOwn(CefGetObject(self)) do
    Result := GetBytes(size, bytes)
end;

{ cef_scheme_handler_factory}

function cef_scheme_handler_factory_create(self: PCefSchemeHandlerFactory;
  browser: PCefBrowser; const scheme_name: PCefString;
  request: PCefRequest): PCefSchemeHandler; stdcall;
begin
  with TCefSchemeHandlerFactoryOwn(CefGetObject(self)) do
    Result := CefGetData(New(CefString(scheme_name), TCefBrowserRef.UnWrap(browser),
      TCefRequestRef.UnWrap(request)));
end;

{ cef_scheme_handler }

function cef_scheme_handler_process_request(self: PCefSchemeHandler; request_: PCefRequest;
  redirectUrl: PCefString; callback: PCefSchemeHandlerCallback): Integer; stdcall;
var
  _redirectUrl: ustring;
begin
  with TCefSchemeHandlerOwn(CefGetObject(self)) do
    Result := Ord(ProcessRequest(TCefRequestRef.UnWrap(request_),
      _redirectUrl, TCefSchemeHandlerCallbackRef.UnWrap(callback)));
  if _redirectUrl <> '' then
    CefStringSet(redirectUrl, _redirectUrl);
end;

function cef_scheme_handler_process_request_sync(self: PCefSchemeHandler;
  request: PCefRequest; redirectUrl: PCefString;
  callback: PCefSchemeHandlerCallback): Integer; stdcall;
var
  sync: TSHSyncProcessRequest;
begin
  sync := TSHSyncProcessRequest.Create(TCefSchemeHandlerOwn(CefGetObject(self)),
    TCefRequestRef.UnWrap(request), TCefSchemeHandlerCallbackRef.UnWrap(callback));
  try
    TThread.Synchronize(nil, sync.Execute);
    Result := Ord(sync.FResult);
    if sync.FRedirectUrl <> '' then
      CefStringSet(redirectUrl, sync.FRedirectUrl);
  finally
    sync.Free;
  end;
end;

procedure cef_scheme_handler_cancel(self: PCefSchemeHandler); stdcall;
begin
  with TCefSchemeHandlerOwn(CefGetObject(self)) do
    Cancel;
end;

function cef_scheme_handler_read_response(self: PCefSchemeHandler;
  data_out: Pointer; bytes_to_read: Integer; var bytes_read: Integer;
  callback: PCefSchemeHandlerCallback): Integer; stdcall;
begin
  with TCefSchemeHandlerOwn(CefGetObject(self)) do
    Result := Ord(ReadResponse(data_out, bytes_to_read, bytes_read,
    TCefSchemeHandlerCallbackRef.UnWrap(callback)));
end;

function cef_scheme_handler_read_response_sync(self: PCefSchemeHandler;
  data_out: Pointer; bytes_to_read: Integer; var bytes_read: Integer;
  callback: PCefSchemeHandlerCallback): Integer; stdcall;
var
  sync: TSHSyncReadResponse;
begin
  sync := TSHSyncReadResponse.Create(TCefSchemeHandlerOwn(CefGetObject(self)),
    data_out, bytes_to_read, bytes_read, TCefSchemeHandlerCallbackRef.UnWrap(callback));
  try
    TThread.Synchronize(nil, sync.Execute);
    Result := ord(sync.FResult);
    bytes_read := sync.FBytesRead;
  finally
    sync.Free
  end;
end;

procedure cef_scheme_handler_get_response_headers_sync(self: PCefSchemeHandler;
  response: PCefResponse; response_length: PInt64); stdcall;
var
  sync: TSHSyncGetResponseHeaders;
begin
  sync := TSHSyncGetResponseHeaders.Create(TCefSchemeHandlerOwn(CefGetObject(self)),
    TCefResponseRef.UnWrap(response));
  try
    TThread.Synchronize(nil, sync.Execute);
    response_length^ := sync.FResponseLength;
  finally
    sync.Free
  end;
end;

procedure cef_scheme_handler_get_response_headers(self: PCefSchemeHandler; response: PCefResponse;
  response_length: PInt64); stdcall;
begin
  with TCefSchemeHandlerOwn(CefGetObject(self)) do
    GetResponseHeaders(TCefResponseRef.UnWrap(response), response_length^);
end;

{ cef_v8_handler }

function cef_v8_handler_execute(self: PCefv8Handler;
  const name: PCefString; obj: PCefv8Value; argumentCount: Cardinal;
  const arguments: PPCefV8Value; var retval: PCefV8Value;
  var exception: TCefString): Integer; stdcall;
var
  args: TCefv8ValueArray;
  i: Integer;
  ret: ICefv8Value;
  exc: ustring;
begin
  SetLength(args, argumentCount);
  for i := 0 to argumentCount - 1 do
    args[i] := TCefv8ValueRef.UnWrap(arguments[i]);

  Result := -Ord(TCefv8HandlerOwn(CefGetObject(self)).Execute(
    CefString(name), TCefv8ValueRef.UnWrap(obj), args, ret, exc));
  retval := CefGetData(ret);
  ret := nil;
  exception := CefString(exc);
end;

function cef_v8_handler_execute_function_with_context(self: PCefv8Handler;
    context: PCefv8Context; obj: PCefv8Value; argumentCount: Cardinal;
    const arguments: PPCefV8Value; var retval: PCefV8Value;
    var exception: TCefString): Integer; stdcall;
var
  args: TCefv8ValueArray;
  i: Integer;
  ret: ICefv8Value;
  exc: ustring;
begin
  SetLength(args, argumentCount);
  for i := 0 to argumentCount - 1 do
    args[i] := TCefv8ValueRef.UnWrap(arguments[i]);

  Result := -Ord(TCefv8HandlerOwn(CefGetObject(self)).ExecuteFunctionWithContext(
    TCefv8ContextRef.UnWrap(context), TCefv8ValueRef.UnWrap(obj), args, ret, exc));
  retval := CefGetData(ret);
  ret := nil;
  exception := CefString(exc);
end;

{ cef_task }

procedure cef_task_execute(self: PCefTask; threadId: TCefThreadId); stdcall;
begin
  TCefTaskOwn(CefGetObject(self)).Execute(threadId);
end;

{ cef_download_handler }

function cef_download_handler_received_data(self: PCefDownloadHandler; data: Pointer; data_size: Integer): Integer; stdcall;
begin
  Result := TCefDownloadHandlerOwn(CefGetObject(self)).ReceivedData(data, data_size);
end;

procedure cef_download_handler_complete(self: PCefDownloadHandler); stdcall;
begin
  TCefDownloadHandlerOwn(CefGetObject(self)).Complete;
end;

{ cef_dom_visitor }

procedure cef_dom_visitor_visite(self: PCefDomVisitor; document: PCefDomDocument); stdcall;
begin
  TCefDomVisitorOwn(CefGetObject(self)).visit(TCefDomDocumentRef.UnWrap(document));
end;

{ cef_dom_event_listener }

procedure cef_dom_event_listener_handle_event(self: PCefDomEventListener; event: PCefDomEvent); stdcall;
begin
  TCefDomEventListenerOwn(CefGetObject(self)).HandleEvent(TCefDomEventRef.UnWrap(event));
end;

{ cef_web_url_request_client }

procedure cef_web_url_request_client_on_state_change(self: PCefWebUrlRequestClient;
  requester: PCefWebUrlRequest; state: TCefWebUrlRequestState); stdcall;
begin
  TCefWebUrlRequestClientOwn(CefGetObject(self)).OnStateChange(
    TCefWebUrlRequestRef.UnWrap(requester),
    state);
end;

procedure cef_web_url_request_client_on_redirect(self: PCefWebUrlRequestClient;
    requester: PCefWebUrlRequest; request: PCefRequest; response: PCefResponse); stdcall;
begin
  TCefWebUrlRequestClientOwn(CefGetObject(self)).OnRedirect(
    TCefWebUrlRequestRef.UnWrap(requester),
    TCefRequestRef.UnWrap(request),
    TCefResponseRef.UnWrap(response));
end;

procedure cef_web_url_request_client_on_headers_received(self: PCefWebUrlRequestClient;
    requester: PCefWebUrlRequest; response: PCefResponse); stdcall;
begin
  TCefWebUrlRequestClientOwn(CefGetObject(self)).OnHeadersReceived(
    TCefWebUrlRequestRef.UnWrap(requester),
    TCefResponseRef.UnWrap(response));
end;

procedure cef_web_url_request_client_on_progress(self: PCefWebUrlRequestClient;
    requester: PCefWebUrlRequest; bytesSent,
    totalBytesToBeSent: uint64); stdcall;
begin
  TCefWebUrlRequestClientOwn(CefGetObject(self)).OnProgress(
    TCefWebUrlRequestRef.UnWrap(requester),
    bytesSent,
    totalBytesToBeSent);
end;

procedure cef_web_url_request_client_on_data(self: PCefWebUrlRequestClient;
    requester: PCefWebUrlRequest; const data: Pointer; dataLength: Integer); stdcall;
begin
  TCefWebUrlRequestClientOwn(CefGetObject(self)).OnData(
    TCefWebUrlRequestRef.UnWrap(requester),
    data,
    dataLength);
end;

procedure cef_web_url_request_client_on_error(self: PCefWebUrlRequestClient;
    requester: PCefWebUrlRequest; errorCode: TCefHandlerErrorcode); stdcall;
begin
  TCefWebUrlRequestClientOwn(CefGetObject(self)).OnError(
    TCefWebUrlRequestRef.UnWrap(requester),
    errorCode);
end;

{ cef_v8_accessor }

function cef_v8_accessor_get(self: PCefV8Accessor; const name: PCefString;
      obj: PCefv8Value; out retval: PCefv8Value; exception: PCefString): Integer; stdcall;
var
  ret: ICefv8Value;
begin
  Result := Ord(TCefV8AccessorOwn(CefGetObject(self)).Get(CefString(name),
    TCefv8ValueRef.UnWrap(obj), ret, CefString(exception)));
  retval := CefGetData(ret);
end;


function cef_v8_accessor_put(self: PCefV8Accessor; const name: PCefString;
      obj: PCefv8Value; value: PCefv8Value; exception: PCefString): Integer; stdcall;
begin
  Result := Ord(TCefV8AccessorOwn(CefGetObject(self)).Put(CefString(name),
    TCefv8ValueRef.UnWrap(obj), TCefv8ValueRef.UnWrap(value), CefString(exception)));
end;

{ cef_cookie_visitor }

function cef_cookie_visitor_visit(self: PCefCookieVisitor; const cookie: PCefCookie;
  count, total: Integer; deleteCookie: PInteger): Integer; stdcall;
var
  delete: Boolean;
  exp: TDateTime;
begin
  delete := False;
  if cookie.has_expires then
    exp := CefTimeToDateTime(cookie.expires) else
    exp := 0;
  Result := Ord(TCefCookieVisitorOwn(CefGetObject(self)).visit(CefString(@cookie.name),
    CefString(@cookie.value), CefString(@cookie.domain), CefString(@cookie.path),
    cookie.secure, cookie.httponly, cookie.has_expires, CefTimeToDateTime(cookie.creation),
    CefTimeToDateTime(cookie.last_access), exp, count, total, delete));
  deleteCookie^ := Ord(delete);
end;

{ cef_storage_visitor }

function cef_storage_visitor_visit(self: PCefStorageVisitor; type_: TCefStorageType;
  const origin, key, value: PCefString; count, total: Integer; deleteData: PInteger): Integer; stdcall;
var
  delete: Boolean;
begin
  delete := False;
  Result := Ord(TCefStorageVisitorOwn(CefGetObject(self)).visit(type_,
    CefString(origin), CefString(key), CefString(value), count, total, delete));
  deleteData^ := Ord(delete);
end;

{ TCefBaseOwn }

constructor TCefBaseOwn.CreateData(size: Cardinal);
begin
  InitializeCriticalSection(FCriticaSection);
  GetMem(FData, size + SizeOf(Pointer));
  PPointer(FData)^ := Self;
  inc(PByte(FData), SizeOf(Pointer));
  FillChar(FData^, size, 0);
  PCefBase(FData)^.size := size;
  PCefBase(FData)^.add_ref := @cef_base_add_ref;
  PCefBase(FData)^.release := @cef_base_release;
  PCefBase(FData)^.get_refct := @cef_base_get_refct;
end;

destructor TCefBaseOwn.Destroy;
begin
  Dec(PByte(FData), SizeOf(Pointer));
  FreeMem(FData);
  DeleteCriticalSection(FCriticaSection);
  inherited;
end;

function TCefBaseOwn.Wrap: Pointer;
begin
  Result := FData;
  if Assigned(PCefBase(FData)^.add_ref) then
    PCefBase(FData)^.add_ref(PCefBase(FData));
end;

procedure TCefBaseOwn.Lock;
begin
  EnterCriticalSection(FCriticaSection);
end;

procedure TCefBaseOwn.Unlock;
begin
  LeaveCriticalSection(FCriticaSection);
end;

{ TCefBaseRef }

constructor TCefBaseRef.Create(data: Pointer);
begin
  Assert(data <> nil);
  FData := data;
end;

destructor TCefBaseRef.Destroy;
begin
  if Assigned(PCefBase(FData)^.release) then
    PCefBase(FData)^.release(PCefBase(FData));
  inherited;
end;

class function TCefBaseRef.UnWrap(data: Pointer): ICefBase;
begin
  if data <> nil then
    Result := Create(data) as ICefBase else
    Result := nil;
end;

function TCefBaseRef.Wrap: Pointer;
begin
  Result := FData;
  if Assigned(PCefBase(FData)^.add_ref) then
    PCefBase(FData)^.add_ref(PCefBase(FData));
end;

{ TCefBrowserRef }

function TCefBrowserRef.CanGoBack: Boolean;
begin
  Result := PCefBrowser(FData)^.can_go_back(PCefBrowser(FData)) <> 0;
end;

function TCefBrowserRef.CanGoForward: Boolean;
begin
  Result := PCefBrowser(FData)^.can_go_forward(PCefBrowser(FData)) <> 0;
end;

procedure TCefBrowserRef.ClearHistory;
begin
  PCefBrowser(FData)^.clear_history(PCefBrowser(FData));
end;

procedure TCefBrowserRef.CloseBrowser;
begin
  PCefBrowser(FData)^.close_browser(PCefBrowser(FData));
end;

procedure TCefBrowserRef.CloseDevTools;
begin
  PCefBrowser(FData)^.close_dev_tools(PCefBrowser(FData));
end;

procedure TCefBrowserRef.Find(const searchText: ustring; identifier,
  forward, matchCase, findNext: Boolean);
var
  s: TCefString;
begin
  s := CefString(searchText);
  PCefBrowser(FData)^.find(PCefBrowser(FData), Ord(identifier), @s,
    Ord(forward), Ord(matchCase), Ord(findNext));
end;

function TCefBrowserRef.GetFocusedFrame: ICefFrame;
begin
  Result := TCefFrameRef.UnWrap(PCefBrowser(FData)^.get_focused_frame(PCefBrowser(FData)))
end;

function TCefBrowserRef.GetFrame(const name: ustring): ICefFrame;
var
  n: TCefString;
begin
  n := CefString(name);
  Result := TCefFrameRef.UnWrap(PCefBrowser(FData)^.get_frame(PCefBrowser(FData), @n));
end;

procedure TCefBrowserRef.GetFrameNames(const names: TStrings);
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    PCefBrowser(FData)^.get_frame_names(PCefBrowser(FData), list);
    FillChar(str, SizeOf(str), 0);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      cef_string_list_value(list, i, @str);
      names.Add(CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefBrowserRef.GetClient: ICefBase;
begin
  Result := TCefBaseRef.UnWrap(PCefBrowser(FData)^.get_client(PCefBrowser(FData)));
end;

function TCefBrowserRef.GetImage(typ: TCefPaintElementType; width,
  height: Integer; buffer: Pointer): Boolean;
begin
  Result := PCefBrowser(FData)^.get_image(PCefBrowser(FData), typ, width, height, buffer) <> 0;
end;

function TCefBrowserRef.GetMainFrame: ICefFrame;
begin
  Result := TCefFrameRef.UnWrap(PCefBrowser(FData)^.get_main_frame(PCefBrowser(FData)))
end;

function TCefBrowserRef.GetOpenerWindowHandle: TCefWindowHandle;
begin
  Result := PCefBrowser(FData)^.get_opener_window_handle(PCefBrowser(FData));
end;

function TCefBrowserRef.GetSize(typ: TCefPaintElementType; var width,
  height: Integer): Boolean;
begin
  Result := PCefBrowser(FData)^.get_size(PCefBrowser(FData), typ, @width, @height) <> 0;
end;

function TCefBrowserRef.GetWindowHandle: TCefWindowHandle;
begin
  Result := PCefBrowser(FData)^.get_window_handle(PCefBrowser(FData));
end;

function TCefBrowserRef.GetZoomLevel: Double;
begin
  Result := PCefBrowser(FData)^.get_zoom_level(PCefBrowser(FData))
end;

procedure TCefBrowserRef.GoBack;
begin
  PCefBrowser(FData)^.go_back(PCefBrowser(FData));
end;

procedure TCefBrowserRef.GoForward;
begin
  PCefBrowser(FData)^.go_forward(PCefBrowser(FData));
end;

function TCefBrowserRef.HasDocument: Boolean;
begin
  Result := PCefBrowser(FData)^.has_document(PCefBrowser(FData)) <> 0;
end;

procedure TCefBrowserRef.HidePopup;
begin
  PCefBrowser(FData)^.hide_popup(PCefBrowser(FData));
end;

procedure TCefBrowserRef.Invalidate(dirtyRect: PCefRect);
begin
  PCefBrowser(FData)^.invalidate(PCefBrowser(FData), dirtyRect);
end;

function TCefBrowserRef.IsPopup: Boolean;
begin
  Result := PCefBrowser(FData)^.is_popup(PCefBrowser(FData)) <> 0;
end;

function TCefBrowserRef.IsPopupVisible: Boolean;
begin
  Result := PCefBrowser(FData)^.is_popup_visible(PCefBrowser(FData)) <> 0;
end;

function TCefBrowserRef.IsWindowRenderingDisabled: Boolean;
begin
  Result := PCefBrowser(FData)^.is_window_rendering_disabled(PCefBrowser(FData)) <> 0;
end;

procedure TCefBrowserRef.ParentWindowWillClose;
begin
  PCefBrowser(FData)^.parent_window_will_close(PCefBrowser(FData));
end;

procedure TCefBrowserRef.Reload;
begin
  PCefBrowser(FData)^.reload(PCefBrowser(FData));
end;

procedure TCefBrowserRef.ReloadIgnoreCache;
begin
  PCefBrowser(FData)^.reload_ignore_cache(PCefBrowser(FData));
end;

procedure TCefBrowserRef.SendCaptureLostEvent;
begin
  PCefBrowser(FData)^.send_capture_lost_event(PCefBrowser(FData));
end;

procedure TCefBrowserRef.SendFocusEvent(setFocus: Boolean);
begin
  PCefBrowser(FData)^.send_focus_event(PCefBrowser(FData), Ord(setFocus));
end;

procedure TCefBrowserRef.SendKeyEvent(typ: TCefKeyType; key, modifiers: Integer; sysChar, imeChar: Boolean);
begin
  PCefBrowser(FData)^.send_key_event(PCefBrowser(FData), typ, key, modifiers, Ord(sysChar), Ord(imeChar));
end;

procedure TCefBrowserRef.SendMouseClickEvent(x, y: Integer;
  typ: TCefMouseButtonType; mouseUp: Boolean; clickCount: Integer);
begin
  PCefBrowser(FData)^.send_mouse_click_event(PCefBrowser(FData), x, y, typ, Ord(mouseUp), clickCount);
end;

procedure TCefBrowserRef.SendMouseMoveEvent(x, y: Integer; mouseLeave: Boolean);
begin
  PCefBrowser(FData)^.send_mouse_move_event(PCefBrowser(FData), x, y, ord(mouseLeave));
end;

procedure TCefBrowserRef.SendMouseWheelEvent(x, y, delta: Integer);
begin
  PCefBrowser(FData)^.send_mouse_wheel_event(PCefBrowser(FData), x, y, delta);
end;

procedure TCefBrowserRef.SetFocus(enable: Boolean);
begin
  PCefBrowser(FData)^.set_focus(PCefBrowser(FData), ord(enable));
end;

procedure TCefBrowserRef.SetSize(typ: TCefPaintElementType; width,
  height: Integer);
begin
  PCefBrowser(FData)^.set_size(PCefBrowser(FData), typ, width, height);
end;

procedure TCefBrowserRef.SetZoomLevel(zoomLevel: Double);
begin
  PCefBrowser(FData)^.set_zoom_level(PCefBrowser(FData), zoomlevel);
end;

procedure TCefBrowserRef.ShowDevTools;
begin
  PCefBrowser(FData)^.show_dev_tools(PCefBrowser(FData));
end;

procedure TCefBrowserRef.StopFinding(ClearSelection: Boolean);
begin
  PCefBrowser(FData)^.stop_finding(PCefBrowser(FData), Ord(ClearSelection));
end;

procedure TCefBrowserRef.StopLoad;
begin
  PCefBrowser(FData)^.stop_load(PCefBrowser(FData));
end;

class function TCefBrowserRef.UnWrap(data: Pointer): ICefBrowser;
begin
  if data <> nil then
    Result := Create(data) as ICefBrowser else
    Result := nil;
end;

{ TCefFrameRef }

procedure TCefFrameRef.Copy;
begin
  PCefFrame(FData)^.copy(PCefFrame(FData));
end;

procedure TCefFrameRef.Cut;
begin
  PCefFrame(FData)^.cut(PCefFrame(FData));
end;

procedure TCefFrameRef.Del;
begin
  PCefFrame(FData)^.del(PCefFrame(FData));
end;

procedure TCefFrameRef.ExecuteJavaScript(const jsCode, scriptUrl: ustring;
  startLine: Integer);
var
  j, s: TCefString;
begin
  j := CefString(jsCode);
  s := CefString(scriptUrl);
  PCefFrame(FData)^.execute_java_script(PCefFrame(FData), @j, @s, startline);
end;

function TCefFrameRef.GetBrowser: ICefBrowser;
begin
  Result := TCefBrowserRef.UnWrap(PCefFrame(FData)^.get_browser(PCefFrame(FData)));
end;

function TCefFrameRef.GetName: ustring;
begin
  Result := CefStringFreeAndGet(PCefFrame(FData)^.get_name(PCefFrame(FData)));
end;

function TCefFrameRef.GetSource: ustring;
begin
  Result := CefStringFreeAndGet(PCefFrame(FData)^.get_source(PCefFrame(FData)));
end;

function TCefFrameRef.getText: ustring;
begin
  Result := CefStringFreeAndGet(PCefFrame(FData)^.get_text(PCefFrame(FData)));
end;

function TCefFrameRef.GetUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefFrame(FData)^.get_url(PCefFrame(FData)));
end;

function TCefFrameRef.IsFocused: Boolean;
begin
  Result := PCefFrame(FData)^.is_focused(PCefFrame(FData)) <> 0;
end;

function TCefFrameRef.IsMain: Boolean;
begin
  Result := PCefFrame(FData)^.is_main(PCefFrame(FData)) <> 0;
end;

procedure TCefFrameRef.LoadFile(const filename, url: ustring);
var
  strm: ICefStreamReader;
  u, f: TCefString;
begin
  f := CefString(filename);
  strm := TCefStreamReaderRef.UnWrap(cef_stream_reader_create_for_file(@f));
  if strm <> nil then
  begin
    u := CefString(url);
    PCefFrame(FData)^.load_stream(PCefFrame(FData), CefGetData(strm), @u);
  end;
end;

procedure TCefFrameRef.LoadRequest(const request: ICefRequest);
begin
  PCefFrame(FData)^.load_request(PCefFrame(FData), CefGetData(request));
end;

procedure TCefFrameRef.LoadStream(const stream: TStream; Owned: Boolean; const url: ustring);
var
  strm: ICefStreamReader;
  u: TCefString;
begin
  strm := TCefStreamReaderRef.CreateForStream(stream, Owned) as ICefStreamReader;
  if strm  <> nil then
  begin
    u := CefString(url);
    PCefFrame(FData)^.load_stream(PCefFrame(FData), CefGetData(strm), @u);
  end;
end;

procedure TCefFrameRef.LoadString(const str, url: ustring);
var
  s, u: TCefString;
begin
  s := CefString(str);
  u := CefString(url);
  PCefFrame(FData)^.load_string(PCefFrame(FData), @s, @u);
end;

procedure TCefFrameRef.LoadUrl(const url: ustring);
var
  u: TCefString;
begin
  u := CefString(url);
  PCefFrame(FData)^.load_url(PCefFrame(FData), @u);
end;

procedure TCefFrameRef.Paste;
begin
  PCefFrame(FData)^.paste(PCefFrame(FData));
end;

procedure TCefFrameRef.Print;
begin
  PCefFrame(FData)^.print(PCefFrame(FData));
end;

procedure TCefFrameRef.Redo;
begin
  PCefFrame(FData)^.redo(PCefFrame(FData));
end;

procedure TCefFrameRef.SelectAll;
begin
  PCefFrame(FData)^.select_all(PCefFrame(FData));
end;

procedure TCefFrameRef.Undo;
begin
  PCefFrame(FData)^.undo(PCefFrame(FData));
end;

procedure TCefFrameRef.ViewSource;
begin
  PCefFrame(FData)^.view_source(PCefFrame(FData));
end;

procedure TCefFrameRef.VisitDom(const visitor: ICefDomVisitor);
begin
  PCefFrame(FData)^.visit_dom(PCefFrame(FData), CefGetData(visitor));
end;

procedure TCefFrameRef.VisitDomProc(const proc: TCefFastDomVisitorProc);
begin
  VisitDom(TCefFastDomVisitor.Create(proc) as ICefDomVisitor);
end;

class function TCefFrameRef.UnWrap(data: Pointer): ICefFrame;
begin
  if data <> nil then
    Result := Create(data) as ICefFrame else
    Result := nil;
end;

{ TCefStreamReaderOwn }

constructor TCefCustomStreamReader.Create(Stream: TStream; Owned: Boolean);
begin
  inherited CreateData(SizeOf(TCefReadHandler));
  FStream := stream;
  FOwned := Owned;
  with PCefReadHandler(FData)^ do
  begin
    read := cef_read_handler_read;
    seek := cef_read_handler_seek;
    tell := cef_read_handler_tell;
    eof := cef_read_handler_eof;
  end;
end;

constructor TCefCustomStreamReader.Create(const filename: string);
begin
  Create(TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite), True);
end;

destructor TCefCustomStreamReader.Destroy;
begin
  if FOwned then
    FStream.Free;
  inherited;
end;

function TCefCustomStreamReader.Eof: Boolean;
begin
  Lock;
  try
    Result := FStream.Position = FStream.size;
  finally
    Unlock;
  end;
end;

function TCefCustomStreamReader.Read(ptr: Pointer; size, n: Cardinal): Cardinal;
begin
  Lock;
  try
    result := Cardinal(FStream.Read(ptr^, n * size)) div size;
  finally
    Unlock;
  end;
end;

function TCefCustomStreamReader.Seek(offset, whence: Integer): Integer;
begin
  Lock;
  try
    Result := FStream.Seek(offset, whence);
  finally
    Unlock;
  end;
end;

function TCefCustomStreamReader.Tell: LongInt;
begin
  Lock;
  try
    Result := FStream.Position;
  finally
    Unlock;
  end;
end;

{ TCefPostDataRef }

function TCefPostDataRef.AddElement(
  const element: ICefPostDataElement): Integer;
begin
  Result := PCefPostData(FData)^.add_element(PCefPostData(FData), CefGetData(element));
end;

function TCefPostDataRef.GetCount: Cardinal;
begin
  Result := PCefPostData(FData)^.get_element_count(PCefPostData(FData))
end;

function TCefPostDataRef.GetElement(Index: Integer): ICefPostDataElement;
begin
  Result := TCefPostDataElementRef.UnWrap(PCefPostData(FData)^.get_elements(PCefPostData(FData), Index))
end;

class function TCefPostDataRef.New: ICefPostData;
begin
  Result := UnWrap(cef_post_data_create);
end;

function TCefPostDataRef.RemoveElement(
  const element: ICefPostDataElement): Integer;
begin
  Result := PCefPostData(FData)^.remove_element(PCefPostData(FData), CefGetData(element));
end;

procedure TCefPostDataRef.RemoveElements;
begin
  PCefPostData(FData)^.remove_elements(PCefPostData(FData));
end;

class function TCefPostDataRef.UnWrap(data: Pointer): ICefPostData;
begin
  if data <> nil then
    Result := Create(data) as ICefPostData else
    Result := nil;
end;

{ TCefPostDataElementRef }

function TCefPostDataElementRef.GetBytes(size: Cardinal;
  bytes: Pointer): Cardinal;
begin
  Result := PCefPostDataElement(FData)^.get_bytes(PCefPostDataElement(FData), size, bytes);
end;

function TCefPostDataElementRef.GetBytesCount: Cardinal;
begin
  Result := PCefPostDataElement(FData)^.get_bytes_count(PCefPostDataElement(FData));
end;

function TCefPostDataElementRef.GetFile: ustring;
begin
  Result := CefStringFreeAndGet(PCefPostDataElement(FData)^.get_file(PCefPostDataElement(FData)));
end;

function TCefPostDataElementRef.GetType: TCefPostDataElementType;
begin
  Result := PCefPostDataElement(FData)^.get_type(PCefPostDataElement(FData));
end;

class function TCefPostDataElementRef.New: ICefPostDataElement;
begin
  Result := UnWrap(cef_post_data_element_create);
end;

procedure TCefPostDataElementRef.SetToBytes(size: Cardinal; bytes: Pointer);
begin
  PCefPostDataElement(FData)^.set_to_bytes(PCefPostDataElement(FData), size, bytes);
end;

procedure TCefPostDataElementRef.SetToEmpty;
begin
  PCefPostDataElement(FData)^.set_to_empty(PCefPostDataElement(FData));
end;

procedure TCefPostDataElementRef.SetToFile(const fileName: ustring);
var
  f: TCefString;
begin
  f := CefString(fileName);
  PCefPostDataElement(FData)^.set_to_file(PCefPostDataElement(FData), @f);
end;

class function TCefPostDataElementRef.UnWrap(data: Pointer): ICefPostDataElement;
begin
  if data <> nil then
    Result := Create(data) as ICefPostDataElement else
    Result := nil;
end;

{ TCefPostDataElementOwn }

procedure TCefPostDataElementOwn.Clear;
begin
  case FDataType of
    PDE_TYPE_BYTES:
      if (FValueByte <> nil) then
      begin
        FreeMem(FValueByte);
        FValueByte := nil;
      end;
    PDE_TYPE_FILE:
      CefStringFree(@FValueStr)
  end;
  FDataType := PDE_TYPE_EMPTY;
  FSize := 0;
end;

constructor TCefPostDataElementOwn.Create;
begin
  inherited CreateData(SizeOf(TCefPostDataElement));
  FDataType := PDE_TYPE_EMPTY;
  FValueByte := nil;
  FillChar(FValueStr, SizeOf(FValueStr), 0);
  FSize := 0;
  with PCefPostDataElement(FData)^ do
  begin
    set_to_empty := @cef_post_data_element_set_to_empty;
    set_to_file := @cef_post_data_element_set_to_file;
    set_to_bytes := @cef_post_data_element_set_to_bytes;
    get_type := @cef_post_data_element_get_type;
    get_file := @cef_post_data_element_get_file;
    get_bytes_count := @cef_post_data_element_get_bytes_count;
    get_bytes := @cef_post_data_element_get_bytes;
  end;
end;

function TCefPostDataElementOwn.GetBytes(size: Cardinal;
  bytes: Pointer): Cardinal;
begin
  Lock;
  try
    if (FDataType = PDE_TYPE_BYTES) and (FValueByte <> nil) then
    begin
      if size > FSize then
        Result := FSize else
        Result := size;
      Move(FValueByte^, bytes^, Result);
    end else
      Result := 0;
  finally
    Unlock;
  end;
end;

function TCefPostDataElementOwn.GetBytesCount: Cardinal;
begin
  if (FDataType = PDE_TYPE_BYTES) then
    Result := FSize else
    Result := 0;
end;

function TCefPostDataElementOwn.GetFile: ustring;
begin
  Lock;
  try
    if (FDataType = PDE_TYPE_FILE) then
      Result := CefString(@FValueStr) else
      Result := '';
  finally
    Unlock;
  end;
end;

function TCefPostDataElementOwn.GetType: TCefPostDataElementType;
begin
  Result := FDataType;
end;

procedure TCefPostDataElementOwn.SetToBytes(size: Cardinal; bytes: Pointer);
begin
  Lock;
  try
    Clear;
    if (size > 0) and (bytes <> nil) then
    begin
      GetMem(FValueByte, size);
      Move(bytes^, FValueByte, size);
      FSize := size;
    end else
    begin
      FValueByte := nil;
      FSize := 0;
    end;
    FDataType := PDE_TYPE_BYTES;
  finally
    Unlock;
  end;
end;

procedure TCefPostDataElementOwn.SetToEmpty;
begin
  Lock;
  try
    Clear;
  finally
    Unlock;
  end;
end;

procedure TCefPostDataElementOwn.SetToFile(const fileName: ustring);
begin
  Lock;
  try
    Clear;
    FSize := 0;
    FValueStr := CefStringAlloc(fileName);
    FDataType := PDE_TYPE_FILE;
  finally
    Unlock;
  end;
end;

{ TCefRequestRef }

function TCefRequestRef.GetFirstPartyForCookies: ustring;
begin
  Result := CefStringFreeAndGet(PCefRequest(FData).get_first_party_for_cookies(PCefRequest(FData)));
end;

function TCefRequestRef.GetFlags: TCefWebUrlRequestFlags;
begin
  Result := PCefRequest(FData)^.get_flags(PCefRequest(FData));
end;

procedure TCefRequestRef.GetHeaderMap(const HeaderMap: ICefStringMap);
begin
  PCefRequest(FData)^.get_header_map(PCefRequest(FData), HeaderMap.Handle);
end;

function TCefRequestRef.GetMethod: ustring;
begin
  Result := CefStringFreeAndGet(PCefRequest(FData)^.get_method(PCefRequest(FData)))
end;

function TCefRequestRef.GetPostData: ICefPostData;
begin
  Result := TCefPostDataRef.UnWrap(PCefRequest(FData)^.get_post_data(PCefRequest(FData)));
end;

function TCefRequestRef.GetUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefRequest(FData)^.get_url(PCefRequest(FData)))
end;

class function TCefRequestRef.New: ICefRequest;
begin
  Result := UnWrap(cef_request_create);
end;

procedure TCefRequestRef.SetFirstPartyForCookies(const url: ustring);
var
  str: TCefString;
begin
  str := CefString(url);
  PCefRequest(FData).set_first_party_for_cookies(PCefRequest(FData), @str);
end;

procedure TCefRequestRef.SetFlags(flags: TCefWebUrlRequestFlags);
begin
  PCefRequest(FData)^.set_flags(PCefRequest(FData), flags);
end;

procedure TCefRequestRef.SetHeaderMap(const HeaderMap: ICefStringMap);
begin
  PCefRequest(FData)^.set_header_map(PCefRequest(FData), HeaderMap.Handle);
end;

procedure TCefRequestRef.SetMethod(const value: ustring);
var
  v: TCefString;
begin
  v := CefString(value);
  PCefRequest(FData)^.set_method(PCefRequest(FData), @v);
end;

procedure TCefRequestRef.SetPostData(const value: ICefPostData);
begin
  if value <> nil then
    PCefRequest(FData)^.set_post_data(PCefRequest(FData), CefGetData(value));
end;

procedure TCefRequestRef.SetUrl(const value: ustring);
var
  v: TCefString;
begin
  v := CefString(value);
  PCefRequest(FData)^.set_url(PCefRequest(FData), @v);
end;

class function TCefRequestRef.UnWrap(data: Pointer): ICefRequest;
begin
  if data <> nil then
    Result := Create(data) as ICefRequest else
    Result := nil;
end;

{ TCefStreamReaderRef }

class function TCefStreamReaderRef.CreateForCustomStream(
  const stream: ICefCustomStreamReader): ICefStreamReader;
begin
  Result := UnWrap(cef_stream_reader_create_for_handler(CefGetData(stream)))
end;

class function TCefStreamReaderRef.CreateForData(data: Pointer; size: Cardinal): ICefStreamReader;
begin
  Result := UnWrap(cef_stream_reader_create_for_data(data, size))
end;

class function TCefStreamReaderRef.CreateForFile(const filename: ustring): ICefStreamReader;
var
  f: TCefString;
begin
  f := CefString(filename);
  Result := UnWrap(cef_stream_reader_create_for_file(@f))
end;

class function TCefStreamReaderRef.CreateForStream(const stream: TSTream;
  owned: Boolean): ICefStreamReader;
begin
  Result := CreateForCustomStream(TCefCustomStreamReader.Create(stream, owned) as ICefCustomStreamReader);
end;

function TCefStreamReaderRef.Eof: Boolean;
begin
  Result := PCefStreamReader(FData)^.eof(PCefStreamReader(FData)) <> 0;
end;

function TCefStreamReaderRef.Read(ptr: Pointer; size, n: Cardinal): Cardinal;
begin
  Result := PCefStreamReader(FData)^.read(PCefStreamReader(FData), ptr, size, n);
end;

function TCefStreamReaderRef.Seek(offset, whence: Integer): Integer;
begin
  Result := PCefStreamReader(FData)^.seek(PCefStreamReader(FData), offset, whence);
end;

function TCefStreamReaderRef.Tell: LongInt;
begin
  Result := PCefStreamReader(FData)^.tell(PCefStreamReader(FData));
end;

class function TCefStreamReaderRef.UnWrap(data: Pointer): ICefStreamReader;
begin
  if data <> nil then
    Result := Create(data) as ICefStreamReader else
    Result := nil;
end;

{ TCefLib }

var
  LibHandle: THandle = 0;

procedure CefLoadLibDefault;
begin
  if LibHandle = 0 then
    CefLoadLib(CefCache, CefUserAgent, CefProductVersion, CefLocale, CefLogFile,
      CefExtraPluginPaths, CefLogSeverity, CefGraphicsImplementation,
      CefLocalStorageQuota, CefSessionStorageQuota);
end;

procedure CefLoadLib(const Cache, UserAgent, ProductVersion, Locale, LogFile, ExtraPluginPaths: ustring;
  LogSeverity: TCefLogSeverity; GraphicsImplementation: TCefGraphicsImplementation; LocalStorageQuota: Cardinal;
  SessionStorageQuota: Cardinal);
var
  settings: TCefSettings;
  i: Integer;
  paths: TStringList;
  p: TCefString;
begin
  if LibHandle = 0 then
  begin
{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    Set8087CW(Get8087CW or $3F); // deactivate FPU exception
{$ENDIF}
    LibHandle := LoadLibrary(PChar(CefLibrary));
    if LibHandle = 0 then
      RaiseLastOSError;

    cef_string_wide_set := GetProcAddress(LibHandle, 'cef_string_wide_set');
    cef_string_utf8_set := GetProcAddress(LibHandle, 'cef_string_utf8_set');
    cef_string_utf16_set := GetProcAddress(LibHandle, 'cef_string_utf16_set');
    cef_string_wide_clear := GetProcAddress(LibHandle, 'cef_string_wide_clear');
    cef_string_utf8_clear := GetProcAddress(LibHandle, 'cef_string_utf8_clear');
    cef_string_utf16_clear := GetProcAddress(LibHandle, 'cef_string_utf16_clear');
    cef_string_wide_cmp := GetProcAddress(LibHandle, 'cef_string_wide_cmp');
    cef_string_utf8_cmp := GetProcAddress(LibHandle, 'cef_string_utf8_cmp');
    cef_string_utf16_cmp := GetProcAddress(LibHandle, 'cef_string_utf16_cmp');
    cef_string_wide_to_utf8 := GetProcAddress(LibHandle, 'cef_string_wide_to_utf8');
    cef_string_utf8_to_wide := GetProcAddress(LibHandle, 'cef_string_utf8_to_wide');
    cef_string_wide_to_utf16 := GetProcAddress(LibHandle, 'cef_string_wide_to_utf16');
    cef_string_utf16_to_wide := GetProcAddress(LibHandle, 'cef_string_utf16_to_wide');
    cef_string_utf8_to_utf16 := GetProcAddress(LibHandle, 'cef_string_utf8_to_utf16');
    cef_string_utf16_to_utf8 := GetProcAddress(LibHandle, 'cef_string_utf16_to_utf8');
    cef_string_ascii_to_wide := GetProcAddress(LibHandle, 'cef_string_ascii_to_wide');
    cef_string_ascii_to_utf16 := GetProcAddress(LibHandle, 'cef_string_ascii_to_utf16');
    cef_string_userfree_wide_alloc := GetProcAddress(LibHandle, 'cef_string_userfree_wide_alloc');
    cef_string_userfree_utf8_alloc := GetProcAddress(LibHandle, 'cef_string_userfree_utf8_alloc');
    cef_string_userfree_utf16_alloc := GetProcAddress(LibHandle, 'cef_string_userfree_utf16_alloc');
    cef_string_userfree_wide_free := GetProcAddress(LibHandle, 'cef_string_userfree_wide_free');
    cef_string_userfree_utf8_free := GetProcAddress(LibHandle, 'cef_string_userfree_utf8_free');
    cef_string_userfree_utf16_free := GetProcAddress(LibHandle, 'cef_string_userfree_utf16_free');

{$IFDEF CEF_STRING_TYPE_UTF8}
  cef_string_set := cef_string_utf8_set;
  cef_string_clear := cef_string_utf8_clear;
  cef_string_userfree_alloc := cef_string_userfree_utf8_alloc;
  cef_string_userfree_free := cef_string_userfree_utf8_free;
  cef_string_from_ascii := cef_string_utf8_copy;
  cef_string_to_utf8 := cef_string_utf8_copy;
  cef_string_from_utf8 := cef_string_utf8_copy;
  cef_string_to_utf16 := cef_string_utf8_to_utf16;
  cef_string_from_utf16 := cef_string_utf16_to_utf8;
  cef_string_to_wide := cef_string_utf8_to_wide;
  cef_string_from_wide := cef_string_wide_to_utf8;
{$ENDIF}

{$IFDEF CEF_STRING_TYPE_UTF16}
    cef_string_set := cef_string_utf16_set;
    cef_string_clear := cef_string_utf16_clear;
    cef_string_userfree_alloc := cef_string_userfree_utf16_alloc;
    cef_string_userfree_free := cef_string_userfree_utf16_free;
    cef_string_from_ascii := cef_string_ascii_to_utf16;
    cef_string_to_utf8 := cef_string_utf16_to_utf8;
    cef_string_from_utf8 := cef_string_utf8_to_utf16;
    cef_string_to_utf16 := cef_string_utf16_copy;
    cef_string_from_utf16 := cef_string_utf16_copy;
    cef_string_to_wide := cef_string_utf16_to_wide;
    cef_string_from_wide := cef_string_wide_to_utf16;
{$ENDIF}

{$IFDEF CEF_STRING_TYPE_WIDE}
    cef_string_set := cef_string_wide_set;
    cef_string_clear := cef_string_wide_clear;
    cef_string_userfree_alloc := cef_string_userfree_wide_alloc;
    cef_string_userfree_free := cef_string_userfree_wide_free;
    cef_string_from_ascii := cef_string_ascii_to_wide;
    cef_string_to_utf8 := cef_string_wide_to_utf8;
    cef_string_from_utf8 := cef_string_utf8_to_wide;
    cef_string_to_utf16 := cef_string_wide_to_utf16;
    cef_string_from_utf16 := cef_string_utf16_to_wide;
    cef_string_to_wide := cef_string_wide_copy;
    cef_string_from_wide := cef_string_wide_copy;
{$ENDIF}

    cef_string_map_alloc := GetProcAddress(LibHandle, 'cef_string_map_alloc');
    cef_string_map_size := GetProcAddress(LibHandle, 'cef_string_map_size');
    cef_string_map_find := GetProcAddress(LibHandle, 'cef_string_map_find');
    cef_string_map_key := GetProcAddress(LibHandle, 'cef_string_map_key');
    cef_string_map_value := GetProcAddress(LibHandle, 'cef_string_map_value');
    cef_string_map_append := GetProcAddress(LibHandle, 'cef_string_map_append');
    cef_string_map_clear := GetProcAddress(LibHandle, 'cef_string_map_clear');
    cef_string_map_free := GetProcAddress(LibHandle, 'cef_string_map_free');
    cef_string_list_alloc := GetProcAddress(LibHandle, 'cef_string_list_alloc');
    cef_string_list_size := GetProcAddress(LibHandle, 'cef_string_list_size');
    cef_string_list_value := GetProcAddress(LibHandle, 'cef_string_list_value');
    cef_string_list_append := GetProcAddress(LibHandle, 'cef_string_list_append');
    cef_string_list_clear := GetProcAddress(LibHandle, 'cef_string_list_clear');
    cef_string_list_free := GetProcAddress(LibHandle, 'cef_string_list_free');
    cef_string_list_copy := GetProcAddress(LibHandle, 'cef_string_list_copy');
    cef_initialize := GetProcAddress(LibHandle, 'cef_initialize');
    cef_shutdown := GetProcAddress(LibHandle, 'cef_shutdown');
    cef_do_message_loop_work := GetProcAddress(LibHandle, 'cef_do_message_loop_work');
    cef_run_message_loop := GetProcAddress(LibHandle, 'cef_run_message_loop');
    cef_register_extension := GetProcAddress(LibHandle, 'cef_register_extension');
    cef_register_custom_scheme := GetProcAddress(LibHandle, 'cef_register_custom_scheme');
    cef_register_scheme_handler_factory := GetProcAddress(LibHandle, 'cef_register_scheme_handler_factory');
    cef_clear_scheme_handler_factories := GetProcAddress(LibHandle, 'cef_clear_scheme_handler_factories');
    cef_add_cross_origin_whitelist_entry := GetProcAddress(LibHandle, 'cef_add_cross_origin_whitelist_entry');
    cef_remove_cross_origin_whitelist_entry := GetProcAddress(LibHandle, 'cef_remove_cross_origin_whitelist_entry');
    cef_clear_cross_origin_whitelist := GetProcAddress(LibHandle, 'cef_clear_cross_origin_whitelist');
    cef_currently_on := GetProcAddress(LibHandle, 'cef_currently_on');
    cef_post_task := GetProcAddress(LibHandle, 'cef_post_task');
    cef_post_delayed_task := GetProcAddress(LibHandle, 'cef_post_delayed_task');
    cef_parse_url := GetProcAddress(LibHandle, 'cef_parse_url');
    cef_create_url := GetProcAddress(LibHandle, 'cef_create_url');
    cef_visit_all_cookies := GetProcAddress(LibHandle, 'cef_visit_all_cookies');
    cef_visit_url_cookies := GetProcAddress(LibHandle, 'cef_visit_url_cookies');
    cef_set_cookie := GetProcAddress(LibHandle, 'cef_set_cookie');
    cef_delete_cookies := GetProcAddress(LibHandle, 'cef_delete_cookies');
    cef_visit_storage := GetProcAddress(LibHandle, 'cef_visit_storage');
    cef_set_storage := GetProcAddress(LibHandle, 'cef_set_storage');
    cef_delete_storage := GetProcAddress(LibHandle, 'cef_delete_storage');
    cef_browser_create := GetProcAddress(LibHandle, 'cef_browser_create');
    cef_browser_create_sync := GetProcAddress(LibHandle, 'cef_browser_create_sync');
    cef_request_create := GetProcAddress(LibHandle, 'cef_request_create');
    cef_post_data_create := GetProcAddress(LibHandle, 'cef_post_data_create');
    cef_post_data_element_create := GetProcAddress(LibHandle, 'cef_post_data_element_create');
    cef_stream_reader_create_for_file := GetProcAddress(LibHandle, 'cef_stream_reader_create_for_file');
    cef_stream_reader_create_for_data := GetProcAddress(LibHandle, 'cef_stream_reader_create_for_data');
    cef_stream_reader_create_for_handler := GetProcAddress(LibHandle, 'cef_stream_reader_create_for_handler');
    cef_stream_writer_create_for_file := GetProcAddress(LibHandle, 'cef_stream_writer_create_for_file');
    cef_stream_writer_create_for_handler := GetProcAddress(LibHandle, 'cef_stream_writer_create_for_handler');
    cef_v8context_get_current_context := GetProcAddress(LibHandle, 'cef_v8context_get_current_context');
    cef_v8context_get_entered_context := GetProcAddress(LibHandle, 'cef_v8context_get_entered_context');
    cef_v8value_create_undefined := GetProcAddress(LibHandle, 'cef_v8value_create_undefined');
    cef_v8value_create_null := GetProcAddress(LibHandle, 'cef_v8value_create_null');
    cef_v8value_create_bool := GetProcAddress(LibHandle, 'cef_v8value_create_bool');
    cef_v8value_create_int := GetProcAddress(LibHandle, 'cef_v8value_create_int');
    cef_v8value_create_double := GetProcAddress(LibHandle, 'cef_v8value_create_double');
    cef_v8value_create_date := GetProcAddress(LibHandle, 'cef_v8value_create_date');
    cef_v8value_create_string := GetProcAddress(LibHandle, 'cef_v8value_create_string');
    cef_v8value_create_object := GetProcAddress(LibHandle, 'cef_v8value_create_object');
    cef_v8value_create_object_with_accessor := GetProcAddress(LibHandle, 'cef_v8value_create_object_with_accessor');
    cef_v8value_create_array := GetProcAddress(LibHandle, 'cef_v8value_create_array');
    cef_v8value_create_function := GetProcAddress(LibHandle, 'cef_v8value_create_function');
    cef_web_urlrequest_create := GetProcAddress(LibHandle, 'cef_web_urlrequest_create');
    cef_xml_reader_create := GetProcAddress(LibHandle, 'cef_xml_reader_create');
    cef_zip_reader_create := GetProcAddress(LibHandle, 'cef_zip_reader_create');

    if not (
      Assigned(cef_string_wide_set) and
      Assigned(cef_string_utf8_set) and
      Assigned(cef_string_utf16_set) and
      Assigned(cef_string_wide_clear) and
      Assigned(cef_string_utf8_clear) and
      Assigned(cef_string_utf16_clear) and
      Assigned(cef_string_wide_cmp) and
      Assigned(cef_string_utf8_cmp) and
      Assigned(cef_string_utf16_cmp) and
      Assigned(cef_string_wide_to_utf8) and
      Assigned(cef_string_utf8_to_wide) and
      Assigned(cef_string_wide_to_utf16) and
      Assigned(cef_string_utf16_to_wide) and
      Assigned(cef_string_utf8_to_utf16) and
      Assigned(cef_string_utf16_to_utf8) and
      Assigned(cef_string_ascii_to_wide) and
      Assigned(cef_string_ascii_to_utf16) and
      Assigned(cef_string_userfree_wide_alloc) and
      Assigned(cef_string_userfree_utf8_alloc) and
      Assigned(cef_string_userfree_utf16_alloc) and
      Assigned(cef_string_userfree_wide_free) and
      Assigned(cef_string_userfree_utf8_free) and
      Assigned(cef_string_userfree_utf16_free) and

      Assigned(cef_string_map_alloc) and
      Assigned(cef_string_map_size) and
      Assigned(cef_string_map_find) and
      Assigned(cef_string_map_key) and
      Assigned(cef_string_map_value) and
      Assigned(cef_string_map_append) and
      Assigned(cef_string_map_clear) and
      Assigned(cef_string_map_free) and
      Assigned(cef_string_list_alloc) and
      Assigned(cef_string_list_size) and
      Assigned(cef_string_list_value) and
      Assigned(cef_string_list_append) and
      Assigned(cef_string_list_clear) and
      Assigned(cef_string_list_free) and
      Assigned(cef_string_list_copy) and
      Assigned(cef_initialize) and
      Assigned(cef_shutdown) and
      Assigned(cef_do_message_loop_work) and
      Assigned(cef_run_message_loop) and
      Assigned(cef_register_extension) and
      Assigned(cef_register_custom_scheme) and
      Assigned(cef_register_scheme_handler_factory) and
      Assigned(cef_clear_scheme_handler_factories) and
      Assigned(cef_add_cross_origin_whitelist_entry) and
      Assigned(cef_remove_cross_origin_whitelist_entry) and
      Assigned(cef_clear_cross_origin_whitelist) and
      Assigned(cef_currently_on) and
      Assigned(cef_post_task) and
      Assigned(cef_post_delayed_task) and
      Assigned(cef_parse_url) and
      Assigned(cef_create_url) and
      Assigned(cef_visit_all_cookies) and
      Assigned(cef_visit_url_cookies) and
      Assigned(cef_set_cookie) and
      Assigned(cef_delete_cookies) and
      Assigned(cef_visit_storage) and
      Assigned(cef_set_storage) and
      Assigned(cef_delete_storage) and
      Assigned(cef_browser_create) and
      Assigned(cef_browser_create_sync) and
      Assigned(cef_request_create) and
      Assigned(cef_post_data_create) and
      Assigned(cef_post_data_element_create) and
      Assigned(cef_stream_reader_create_for_file) and
      Assigned(cef_stream_reader_create_for_data) and
      Assigned(cef_stream_reader_create_for_handler) and
      Assigned(cef_stream_writer_create_for_file) and
      Assigned(cef_stream_writer_create_for_handler) and
      Assigned(cef_v8context_get_current_context) and
      Assigned(cef_v8context_get_entered_context) and
      Assigned(cef_v8value_create_undefined) and
      Assigned(cef_v8value_create_null) and
      Assigned(cef_v8value_create_bool) and
      Assigned(cef_v8value_create_int) and
      Assigned(cef_v8value_create_double) and
      Assigned(cef_v8value_create_date) and
      Assigned(cef_v8value_create_string) and
      Assigned(cef_v8value_create_object) and
      Assigned(cef_v8value_create_object_with_accessor) and
      Assigned(cef_v8value_create_array) and
      Assigned(cef_v8value_create_function) and
      Assigned(cef_web_urlrequest_create) and
      Assigned(cef_xml_reader_create) and
      Assigned(cef_zip_reader_create)
    ) then raise ECefException.Create('Invalid CEF Library version');

    FillChar(settings, SizeOf(settings), 0);
    settings.size := SizeOf(settings);
{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    settings.multi_threaded_message_loop := False;
{$ELSE}
    settings.multi_threaded_message_loop := True;
{$ENDIF}
    settings.cache_path := CefString(Cache);
    settings.user_agent := cefstring(UserAgent);
    settings.product_version := CefString(ProductVersion);
    settings.locale := CefString(Locale);
    if (ExtraPluginPaths <> '') then
    begin
      settings.extra_plugin_paths := cef_string_list_alloc;
      paths := TStringList.Create;
      try
        paths.Delimiter := ';';
        paths.DelimitedText := ExtraPluginPaths;
        for i := 0 to paths.Count - 1 do
        begin
          p := cefString(paths[i]);
          cef_string_list_append(settings.extra_plugin_paths, @p);
        end;
      finally
        paths.free;
      end;
    end;
    settings.log_file := CefString(LogFile);
    settings.log_severity := LogSeverity;
    settings.graphics_implementation := GraphicsImplementation;
    settings.local_storage_quota := LocalStorageQuota;
    settings.session_storage_quota := SessionStorageQuota;
    cef_initialize(@settings);
    if settings.extra_plugin_paths <> nil then
      cef_string_list_free(settings.extra_plugin_paths);
  end;
end;

function CefBrowserCreate(windowInfo: PCefWindowInfo; client: PCefClient;
  const url: ustring; const settings: PCefBrowserSettings): Boolean;
var
  u: TCefString;
begin
  CefLoadLibDefault;
  u := CefString(url);
  Result :=
    cef_browser_create(
      windowInfo,
      client,
      @u,
      settings) <> 0;
end;

function CefBrowserCreateSync(windowInfo: PCefWindowInfo; client: PCefClient;
  const url: ustring; const settings: PCefBrowserSettings): ICefBrowser;
var
  u: TCefString;
begin
  CefLoadLibDefault;
  u := CefString(url);
  Result := TCefBrowserRef.UnWrap(
    cef_browser_create_sync(
      windowInfo,
      client,
      @u,
      settings));
end;

{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
procedure CefDoMessageLoopWork;
begin
  if LibHandle > 0 then
    cef_do_message_loop_work;
end;

procedure CefRunMessageLoop;
begin
  if LibHandle > 0 then
    cef_run_message_loop;
end;

{$ENDIF}

function CefString(const str: ustring): TCefString;
begin
  Result.str := PChar16(PWideChar(str));
  Result.length := Length(str);
  Result.dtor := nil;
end;

function CefString(const str: PCefString): ustring;
begin
  if str <> nil then
    SetString(Result, str.str, str.length) else
    Result := '';
end;

procedure _free_string(str: PChar16); stdcall;
begin
  if str <> nil then
    FreeMem(str);
end;

function CefUserFreeString(const str: ustring): PCefStringUserFree;
begin
  Result := cef_string_userfree_alloc;
  Result.length := Length(str);
  GetMem(Result.str, Result.length * SizeOf(TCefChar));
  Move(PCefChar(str)^, Result.str^, Result.length * SizeOf(TCefChar));
  Result.dtor := @_free_string;
end;

function CefStringAlloc(const str: ustring): TCefString;
begin
  FillChar(Result, SizeOf(Result), 0);
  if str <> '' then
    cef_string_from_wide(PWideChar(str), Length(str), @Result);
end;

procedure CefStringSet(const str: PCefString; const value: ustring);
begin
  if str <> nil then
    cef_string_set(PWideChar(value), Length(value), str, 1);
end;

function CefStringClearAndGet(var str: TCefString): ustring;
begin
  Result := CefString(@str);
  cef_string_clear(@str);
end;

function CefStringFreeAndGet(const str: PCefStringUserFree): ustring;
begin
  if str <> nil then
  begin
    Result := CefString(PCefString(str));
    cef_string_userfree_free(str);
  end else
    Result := '';
end;

procedure CefStringFree(const str: PCefString);
begin
  if str <> nil then
    cef_string_clear(str);
end;

function CefRegisterSchemeHandlerFactory(const SchemeName, HostName: ustring;
  SyncMainThread: Boolean; const handler: TCefSchemeHandlerClass): Boolean;
var
  s, h: TCefString;
begin
  CefLoadLibDefault;
  s := CefString(SchemeName);
  h := CefString(HostName);
  Result := cef_register_scheme_handler_factory(
    @s,
    @h,
    CefGetData(TCefSchemeHandlerFactoryOwn.Create(handler, SyncMainThread) as ICefBase)) <> 0;
end;

function CefClearSchemeHandlerFactories: Boolean;
begin
  CefLoadLibDefault;
  Result := cef_clear_scheme_handler_factories <> 0;
end;

function CefRegisterCustomScheme(const SchemeName: ustring; IsStandard,
  IsLocal, IsDisplayIsolated: Boolean): Boolean;
var
  s: TCefString;
begin
  CefLoadLibDefault;
  s := CefString(SchemeName);
  Result := cef_register_custom_scheme(@s, Ord(IsStandard),
    Ord(IsLocal), Ord(IsDisplayIsolated)) <> 0;
end;

function CefAddCrossOriginWhitelistEntry(const SourceOrigin, TargetProtocol,
  TargetDomain: ustring; AllowTargetSubdomains: Boolean): Boolean;
var
  so, tp, td: TCefString;
begin
  CefLoadLibDefault;
  so := CefString(SourceOrigin);
  tp := CefString(TargetProtocol);
  td := CefString(TargetDomain);
  Result := cef_add_cross_origin_whitelist_entry(@so, @tp, @td, Ord(AllowTargetSubdomains)) <> 0;
end;

function CefRemoveCrossOriginWhitelistEntry(
  const SourceOrigin, TargetProtocol, TargetDomain: ustring;
  AllowTargetSubdomains: Boolean): Boolean;
var
  so, tp, td: TCefString;
begin
  CefLoadLibDefault;
  so := CefString(SourceOrigin);
  tp := CefString(TargetProtocol);
  td := CefString(TargetDomain);
  Result := cef_remove_cross_origin_whitelist_entry(@so, @tp, @td, Ord(AllowTargetSubdomains)) <> 0;
end;

function CefClearCrossOriginWhitelist: Boolean;
begin
  CefLoadLibDefault;
  Result := cef_clear_cross_origin_whitelist <> 0;
end;

function CefRegisterExtension(const name, code: ustring;
  const Handler: ICefv8Handler): Boolean;
var
  n, c: TCefString;
begin
  CefLoadLibDefault;
  n := CefString(name);
  c := CefString(code);
  Result := cef_register_extension(@n, @c, CefGetData(handler)) <> 0;
end;

function CefCurrentlyOn(ThreadId: TCefThreadId): Boolean;
begin
  Result := cef_currently_on(ThreadId) <> 0;
end;

procedure CefPostTask(ThreadId: TCefThreadId; const task: ICefTask);
begin
  cef_post_task(ThreadId, CefGetData(task));
end;

procedure CefPostDelayedTask(ThreadId: TCefThreadId; const task: ICefTask; delayMs: Integer);
begin
  cef_post_delayed_task(ThreadId, CefGetData(task), delayMs);
end;

{ TCefSchemeHandlerFactoryOwn }

constructor TCefSchemeHandlerFactoryOwn.Create(const AClass: TCefSchemeHandlerClass; SyncMainThread: Boolean);
begin
  inherited CreateData(SizeOf(TCefSchemeHandlerFactory));
  with PCefSchemeHandlerFactory(FData)^ do
    create := @cef_scheme_handler_factory_create;
  FClass := AClass;
  FSyncMainThread := SyncMainThread;
end;

function TCefSchemeHandlerFactoryOwn.New(const scheme: ustring;
  const browser: ICefBrowser; const request: ICefRequest): ICefSchemeHandler;
begin
  Result := FClass.Create(FSyncMainThread, scheme, browser, request) as ICefSchemeHandler;
end;

{ TCefSchemeHandlerOwn }

procedure TCefSchemeHandlerOwn.Cancel;
begin
  // do not lock
  FCancelled := True;
end;

constructor TCefSchemeHandlerOwn.Create(SyncMainThread: Boolean;
  const scheme: ustring; const browser: ICefBrowser; const request: ICefRequest);
begin
  inherited CreateData(SizeOf(TCefSchemeHandler));
  FCancelled := False;
  FScheme := scheme;
  FBrowser := browser;
  FRequest := request;
  with PCefSchemeHandler(FData)^ do
  begin
    if SyncMainThread then
    begin
      process_request := @cef_scheme_handler_process_request_sync;
      get_response_headers := @cef_scheme_handler_get_response_headers_sync;
      read_response := @cef_scheme_handler_read_response_sync;
    end else
    begin
      process_request := @cef_scheme_handler_process_request;
      get_response_headers := @cef_scheme_handler_get_response_headers;
      read_response := @cef_scheme_handler_read_response;
    end;
    cancel := @cef_scheme_handler_cancel;
  end;
end;

procedure TCefSchemeHandlerOwn.GetResponseHeaders(const response: ICefResponse;
  var responseLength: Int64);
begin

end;

function TCefSchemeHandlerOwn.ProcessRequest(const Request: ICefRequest;
  var redirectUrl: ustring; const callback: ICefSchemeHandlerCallback): Boolean;
begin
  Result := False;
end;

function TCefSchemeHandlerOwn.ReadResponse(DataOut: Pointer; BytesToRead: Integer;
  var BytesRead: Integer; const callback: ICefSchemeHandlerCallback): Boolean;
begin
  Result := False;
end;

{ TCefv8ValueRef }

class function TCefv8ValueRef.CreateArray: ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_array);
end;

class function TCefv8ValueRef.CreateBool(value: Boolean): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_bool(Ord(value)));
end;

class function TCefv8ValueRef.CreateDate(value: TDateTime): ICefv8Value;
var
  dt: TCefTime;
begin
  dt := DateTimeToCefTime(value);
  Result := UnWrap(cef_v8value_create_date(@dt));
end;

class function TCefv8ValueRef.CreateDouble(value: Double): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_double(value));
end;

class function TCefv8ValueRef.CreateFunction(const name: ustring;
  const handler: ICefv8Handler): ICefv8Value;
var
  n: TCefString;
begin
  n := CefString(name);
  Result := UnWrap(cef_v8value_create_function(@n, CefGetData(handler)));
end;

class function TCefv8ValueRef.CreateInt(value: Integer): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_int(value));
end;

class function TCefv8ValueRef.CreateNull: ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_null);
end;

class function TCefv8ValueRef.CreateObject(const UserData: ICefv8Value): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_object(CefGetData(UserData)));
end;

class function TCefv8ValueRef.CreateObjectWithAccessor(
  const UserData: ICefv8Value; const Accessor: ICefV8Accessor): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_object_with_accessor(CefGetData(UserData), CefGetData(Accessor)));
end;

class function TCefv8ValueRef.CreateObjectWithAccessorProc(
  const UserData: ICefv8Value; const getter: TCefFastV8AccessorGetterProc;
  const setter: TCefFastV8AccessorSetterProc): ICefv8Value;
begin
  Result := CreateObjectWithAccessor(UserData,
    TCefFastV8Accessor.Create(getter, setter) as ICefV8Accessor);
end;

class function TCefv8ValueRef.CreateString(const str: ustring): ICefv8Value;
var
  s: TCefString;
begin
  s := CefString(str);
  Result := UnWrap(cef_v8value_create_string(@s));
end;

class function TCefv8ValueRef.CreateUndefined: ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_undefined);
end;

function TCefv8ValueRef.DeleteValueByIndex(index: Integer): Boolean;
begin
  Result := PCefV8Value(FData)^.delete_value_byindex(PCefV8Value(FData), index) <> 0;
end;

function TCefv8ValueRef.DeleteValueByKey(const key: ustring): Boolean;
var
  k: TCefString;
begin
  k := CefString(key);
  Result := PCefV8Value(FData)^.delete_value_bykey(PCefV8Value(FData), @k) <> 0;
end;

function TCefv8ValueRef.ExecuteFunction(const obj: ICefv8Value;
  const arguments: TCefv8ValueArray; var retval: ICefv8Value;
  var exception: ustring): Boolean;
var
  args: PPCefV8Value;
  i: Integer;
  ret: PCefV8Value;
  exc: TCefString;
begin
  GetMem(args, SizeOf(PCefV8Value) * Length(arguments));
  try
    for i := 0 to Length(arguments) - 1 do
      args[i] := CefGetData(arguments[i]);
    ret := nil;
    FillChar(exc, SizeOf(exc), 0);
    Result := PCefV8Value(FData)^.execute_function(PCefV8Value(FData),
      CefGetData(obj), Length(arguments), args, ret, exc) <> 0;
    retval := TCefv8ValueRef.UnWrap(ret);
    exception := CefStringClearAndGet(exc);
  finally
    FreeMem(args);
  end;
end;

function TCefv8ValueRef.ExecuteFunctionWithContext(const context: ICefv8Context;
  const obj: ICefv8Value; const arguments: TCefv8ValueArray;
  var retval: ICefv8Value; var exception: ustring): Boolean;
var
  args: PPCefV8Value;
  i: Integer;
  ret: PCefV8Value;
  exc: TCefString;
begin
  GetMem(args, SizeOf(PCefV8Value) * Length(arguments));
  try
    for i := 0 to Length(arguments) - 1 do
      args[i] := CefGetData(arguments[i]);
    ret := nil;
    FillChar(exc, SizeOf(exc), 0);
    Result := PCefV8Value(FData)^.execute_function_with_context(PCefV8Value(FData),
      CefGetData(context), CefGetData(obj), Length(arguments), args, ret, exc) <> 0;
    retval := TCefv8ValueRef.UnWrap(ret);
    exception := CefStringClearAndGet(exc);
  finally
    FreeMem(args);
  end;
end;

function TCefv8ValueRef.GetArrayLength: Integer;
begin
  Result := PCefV8Value(FData)^.get_array_length(PCefV8Value(FData));
end;

function TCefv8ValueRef.GetBoolValue: Boolean;
begin
  Result := PCefV8Value(FData)^.get_bool_value(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.GetDateValue: TDateTime;
begin
  Result := CefTimeToDateTime(PCefV8Value(FData)^.get_date_value(PCefV8Value(FData)));
end;

function TCefv8ValueRef.GetDoubleValue: Double;
begin
  Result := PCefV8Value(FData)^.get_double_value(PCefV8Value(FData));
end;

function TCefv8ValueRef.GetFunctionHandler: ICefv8Handler;
begin
  Result := TCefv8HandlerRef.UnWrap(PCefV8Value(FData)^.get_function_handler(PCefV8Value(FData)));
end;

function TCefv8ValueRef.GetFunctionName: ustring;
begin
  Result := CefStringFreeAndGet(PCefV8Value(FData)^.get_function_name(PCefV8Value(FData)))
end;

function TCefv8ValueRef.GetIntValue: Integer;
begin
  Result := PCefV8Value(FData)^.get_int_value(PCefV8Value(FData))
end;

function TCefv8ValueRef.GetKeys(const keys: TStrings): Integer;
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    Result := PCefV8Value(FData)^.get_keys(PCefV8Value(FData), list);
    FillChar(str, SizeOf(str), 0);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      cef_string_list_value(list, i, @str);
      keys.Add(CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefv8ValueRef.GetStringValue: ustring;
begin
  Result := CefStringFreeAndGet(PCefV8Value(FData)^.get_string_value(PCefV8Value(FData)));
end;

function TCefv8ValueRef.GetUserData: ICefv8Value;
begin
  Result := TCefv8ValueRef.UnWrap(PCefV8Value(FData)^.get_user_data(PCefV8Value(FData)));
end;

function TCefv8ValueRef.GetValueByIndex(index: Integer): ICefv8Value;
begin
  Result := TCefv8ValueRef.UnWrap(PCefV8Value(FData)^.get_value_byindex(PCefV8Value(FData), index))
end;

function TCefv8ValueRef.GetValueByKey(const key: ustring): ICefv8Value;
var
  k: TCefString;
begin
  k := CefString(key);
  Result := TCefv8ValueRef.UnWrap(PCefV8Value(FData)^.get_value_bykey(PCefV8Value(FData), @k))
end;

function TCefv8ValueRef.HasValueByIndex(index: Integer): Boolean;
begin
  Result := PCefV8Value(FData)^.has_value_byindex(PCefV8Value(FData), index) <> 0;
end;

function TCefv8ValueRef.HasValueByKey(const key: ustring): Boolean;
var
  k: TCefString;
begin
  k := CefString(key);
  Result := PCefV8Value(FData)^.has_value_bykey(PCefV8Value(FData), @k) <> 0;
end;

function TCefv8ValueRef.IsArray: Boolean;
begin
  Result := PCefV8Value(FData)^.is_array(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsBool: Boolean;
begin
  Result := PCefV8Value(FData)^.is_bool(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsDate: Boolean;
begin
  Result := PCefV8Value(FData)^.is_date(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsDouble: Boolean;
begin
  Result := PCefV8Value(FData)^.is_double(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsFunction: Boolean;
begin
  Result := PCefV8Value(FData)^.is_function(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsInt: Boolean;
begin
  Result := PCefV8Value(FData)^.is_int(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsNull: Boolean;
begin
  Result := PCefV8Value(FData)^.is_null(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsObject: Boolean;
begin
  Result := PCefV8Value(FData)^.is_object(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsSame(const that: ICefv8Value): Boolean;
begin
  Result := PCefV8Value(FData)^.is_same(PCefV8Value(FData), CefGetData(that)) <> 0;
end;

function TCefv8ValueRef.IsString: Boolean;
begin
  Result := PCefV8Value(FData)^.is_string(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsUndefined: Boolean;
begin
  Result := PCefV8Value(FData)^.is_undefined(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.SetValueByAccessor(const key: ustring;
  settings: TCefV8AccessControls; attribute: TCefV8PropertyAttributes): Boolean;
var
  k: TCefString;
begin
  k := CefString(key);
  Result:= PCefV8Value(FData)^.set_value_byaccessor(PCefV8Value(FData), @k, settings, attribute) <> 0;
end;

function TCefv8ValueRef.SetValueByIndex(index: Integer;
  const value: ICefv8Value): Boolean;
begin
  Result:= PCefV8Value(FData)^.set_value_byindex(PCefV8Value(FData), index, CefGetData(value)) <> 0;
end;

function TCefv8ValueRef.SetValueByKey(const key: ustring;
  const value: ICefv8Value): Boolean;
var
  k: TCefString;
begin
  k := CefString(key);
  Result:= PCefV8Value(FData)^.set_value_bykey(PCefV8Value(FData), @k, CefGetData(value)) <> 0;
end;

class function TCefv8ValueRef.UnWrap(data: Pointer): ICefv8Value;
begin
  if data <> nil then
    Result := Create(data) as ICefv8Value else
    Result := nil;
end;

{ TCefv8HandlerRef }

function TCefv8HandlerRef.Execute(const name: ustring; const obj: ICefv8Value;
  const arguments: TCefv8ValueArray; var retval: ICefv8Value;
  var exception: ustring): Boolean;
var
  args: array of PCefV8Value;
  i: Integer;
  ret: PCefV8Value;
  exc: TCefString;
  n: TCefString;
begin
  SetLength(args, Length(arguments));
  for i := 0 to Length(arguments) - 1 do
    args[i] := CefGetData(arguments[i]);
  ret := nil;
  FillChar(exc, SizeOf(exc), 0);
  n := CefString(name);
  Result := PCefv8Handler(FData)^.execute(PCefv8Handler(FData), @n,
    CefGetData(obj), Length(arguments), @args, ret, exc) <> 0;
  retval := TCefv8ValueRef.UnWrap(ret);
  exception := CefStringClearAndGet(exc);
end;

function TCefv8HandlerRef.ExecuteFunctionWithContext(
  const context: ICefv8Context; const obj: ICefv8Value;
  const arguments: TCefv8ValueArray; var retval: ICefV8Value;
  var exception: ustring): Boolean;
var
  args: array of PCefV8Value;
  i: Integer;
  ret: PCefV8Value;
  exc: TCefString;
begin
  SetLength(args, Length(arguments));
  for i := 0 to Length(arguments) - 1 do
    args[i] := CefGetData(arguments[i]);
  ret := nil;
  FillChar(exc, SizeOf(exc), 0);
  Result := PCefv8Handler(FData)^.execute_function_with_context(PCefv8Handler(FData),
    CefGetData(context), CefGetData(obj), Length(arguments), @args, ret, exc) <> 0;
  retval := TCefv8ValueRef.UnWrap(ret);
  exception := CefStringClearAndGet(exc);
end;

class function TCefv8HandlerRef.UnWrap(data: Pointer): ICefv8Handler;
begin
  if data <> nil then
    Result := Create(data) as ICefv8Handler else
    Result := nil;
end;

{ TCefv8HandlerOwn }

constructor TCefv8HandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefv8Handler));
  with PCefv8Handler(FData)^ do
  begin
    execute := @cef_v8_handler_execute;
    execute_function_with_context := @cef_v8_handler_execute_function_with_context;
  end;
end;

function TCefv8HandlerOwn.Execute(const name: ustring; const obj: ICefv8Value;
  const arguments: TCefv8ValueArray; var retval: ICefv8Value;
  var exception: ustring): Boolean;
begin
  Result := False;
end;

function TCefv8HandlerOwn.ExecuteFunctionWithContext(
  const context: ICefv8Context; const obj: ICefv8Value;
  const arguments: TCefv8ValueArray; var retval: ICefV8Value;
  var exception: ustring): Boolean;
begin
  Result := False;
end;

{ TCefTaskOwn }

constructor TCefTaskOwn.Create;
begin
  inherited CreateData(SizeOf(TCefTask));
  with PCefTask(FData)^ do
    execute := @cef_task_execute;
end;

procedure TCefTaskOwn.Execute(threadId: TCefThreadId);
begin

end;

{ TCefStringMapOwn }

procedure TCefStringMapOwn.Append(const key, value: ustring);
var
  k, v: TCefString;
begin
  k := CefString(key);
  v := CefString(value);
  cef_string_map_append(FStringMap, @k, @v);
end;

procedure TCefStringMapOwn.Clear;
begin
  cef_string_map_clear(FStringMap);
end;

constructor TCefStringMapOwn.Create;
begin
  FStringMap := cef_string_map_alloc;
end;

destructor TCefStringMapOwn.Destroy;
begin
  cef_string_map_free(FStringMap);
end;

function TCefStringMapOwn.Find(const key: ustring): ustring;
var
  str, k: TCefString;
begin
  FillChar(str, SizeOf(str), 0);
  k := CefString(key);
  cef_string_map_find(FStringMap, @k, str);
  Result := CefString(@str);
end;

function TCefStringMapOwn.GetHandle: TCefStringMap;
begin
  Result := FStringMap;
end;

function TCefStringMapOwn.GetKey(index: Integer): ustring;
var
  str: TCefString;
begin
  FillChar(str, SizeOf(str), 0);
  cef_string_map_key(FStringMap, index, str);
  Result := CefString(@str);
end;

function TCefStringMapOwn.GetSize: Integer;
begin
  Result := cef_string_map_size(FStringMap);
end;

function TCefStringMapOwn.GetValue(index: Integer): ustring;
var
  str: TCefString;
begin
  FillChar(str, SizeOf(str), 0);
  cef_string_map_value(FStringMap, index, str);
  Result := CefString(@str);
end;

{ TCefDownloadHandlerOwn }

constructor TCefDownloadHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDownloadHandler));
  with PCefDownloadHandler(FData)^ do
  begin
    received_data := @cef_download_handler_received_data;
    complete := @cef_download_handler_complete;
  end;
end;

{ TCefXmlReaderRef }

function TCefXmlReaderRef.Close: Boolean;
begin
  Result := PCefXmlReader(FData).close(FData) <> 0;
end;

class function TCefXmlReaderRef.CreateForStream(const stream: ICefStreamReader;
  encodingType: TCefXmlEncodingType; const URI: ustring): ICefXmlReader;
var
  u: TCefString;
begin
  u := CefString(URI);
  Result := UnWrap(cef_xml_reader_create(CefGetData(stream), encodingType, @u));
end;

function TCefXmlReaderRef.GetAttributeByIndex(index: Integer): ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData).get_attribute_byindex(FData, index));
end;

function TCefXmlReaderRef.GetAttributeByLName(const localName,
  namespaceURI: ustring): ustring;
var
  l, n: TCefString;
begin
  l := CefString(localName);
  n := CefString(namespaceURI);
  Result := CefStringFreeAndGet(PCefXmlReader(FData).get_attribute_bylname(FData, @l, @n));
end;

function TCefXmlReaderRef.GetAttributeByQName(
  const qualifiedName: ustring): ustring;
var
  q: TCefString;
begin
  q := CefString(qualifiedName);
  Result := CefStringFreeAndGet(PCefXmlReader(FData).get_attribute_byqname(FData, @q));
end;

function TCefXmlReaderRef.GetAttributeCount: Cardinal;
begin
  Result := PCefXmlReader(FData).get_attribute_count(FData);
end;

function TCefXmlReaderRef.GetBaseUri: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData).get_base_uri(FData));
end;

function TCefXmlReaderRef.GetDepth: Integer;
begin
  Result := PCefXmlReader(FData).get_depth(FData);
end;

function TCefXmlReaderRef.GetError: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData).get_error(FData));
end;

function TCefXmlReaderRef.GetInnerXml: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData).get_inner_xml(FData));
end;

function TCefXmlReaderRef.GetLineNumber: Integer;
begin
  Result := PCefXmlReader(FData).get_line_number(FData);
end;

function TCefXmlReaderRef.GetLocalName: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData).get_local_name(FData));
end;

function TCefXmlReaderRef.GetNamespaceUri: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData).get_namespace_uri(FData));
end;

function TCefXmlReaderRef.GetOuterXml: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData).get_outer_xml(FData));
end;

function TCefXmlReaderRef.GetPrefix: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData).get_prefix(FData));
end;

function TCefXmlReaderRef.GetQualifiedName: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData).get_qualified_name(FData));
end;

function TCefXmlReaderRef.GetType: TCefXmlNodeType;
begin
  Result := PCefXmlReader(FData).get_type(FData);
end;

function TCefXmlReaderRef.GetValue: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData).get_value(FData));
end;

function TCefXmlReaderRef.GetXmlLang: ustring;
begin
  Result := CefStringFreeAndGet(PCefXmlReader(FData).get_xml_lang(FData));
end;

function TCefXmlReaderRef.HasAttributes: Boolean;
begin
  Result := PCefXmlReader(FData).has_attributes(FData) <> 0;
end;

function TCefXmlReaderRef.HasError: Boolean;
begin
  Result := PCefXmlReader(FData).has_error(FData) <> 0;
end;

function TCefXmlReaderRef.HasValue: Boolean;
begin
  Result := PCefXmlReader(FData).has_value(FData) <> 0;
end;

function TCefXmlReaderRef.IsEmptyElement: Boolean;
begin
  Result := PCefXmlReader(FData).is_empty_element(FData) <> 0;
end;

function TCefXmlReaderRef.MoveToAttributeByIndex(index: Integer): Boolean;
begin
  Result := PCefXmlReader(FData).move_to_attribute_byindex(FData, index) <> 0;
end;

function TCefXmlReaderRef.MoveToAttributeByLName(const localName,
  namespaceURI: ustring): Boolean;
var
  l, n: TCefString;
begin
  l := CefString(localName);
  n := CefString(namespaceURI);
  Result := PCefXmlReader(FData).move_to_attribute_bylname(FData, @l, @n) <> 0;
end;

function TCefXmlReaderRef.MoveToAttributeByQName(
  const qualifiedName: ustring): Boolean;
var
  q: TCefString;
begin
  q := CefString(qualifiedName);
  Result := PCefXmlReader(FData).move_to_attribute_byqname(FData, @q) <> 0;
end;

function TCefXmlReaderRef.MoveToCarryingElement: Boolean;
begin
  Result := PCefXmlReader(FData).move_to_carrying_element(FData) <> 0;
end;

function TCefXmlReaderRef.MoveToFirstAttribute: Boolean;
begin
  Result := PCefXmlReader(FData).move_to_first_attribute(FData) <> 0;
end;

function TCefXmlReaderRef.MoveToNextAttribute: Boolean;
begin
  Result := PCefXmlReader(FData).move_to_next_attribute(FData) <> 0;
end;

function TCefXmlReaderRef.MoveToNextNode: Boolean;
begin
  Result := PCefXmlReader(FData).move_to_next_node(FData) <> 0;
end;

class function TCefXmlReaderRef.UnWrap(data: Pointer): ICefXmlReader;
begin
  if data <> nil then
    Result := Create(data) as ICefXmlReader else
    Result := nil;
end;

{ TCefZipReaderRef }

function TCefZipReaderRef.Close: Boolean;
begin
  Result := PCefZipReader(FData).close(FData) <> 0;
end;

function TCefZipReaderRef.CloseFile: Boolean;
begin
  Result := PCefZipReader(FData).close_file(FData) <> 0;
end;

class function TCefZipReaderRef.CreateForStream(const stream: ICefStreamReader): ICefZipReader;
begin
  Result := UnWrap(cef_zip_reader_create(CefGetData(stream)));
end;

function TCefZipReaderRef.Eof: Boolean;
begin
  Result := PCefZipReader(FData).eof(FData) <> 0;
end;

function TCefZipReaderRef.GetFileLastModified: LongInt;
begin
  Result := PCefZipReader(FData).get_file_last_modified(FData);
end;

function TCefZipReaderRef.GetFileName: ustring;
begin
  Result := CefStringFreeAndGet(PCefZipReader(FData).get_file_name(FData));
end;

function TCefZipReaderRef.GetFileSize: LongInt;
begin
  Result := PCefZipReader(FData).get_file_size(FData);
end;

function TCefZipReaderRef.MoveToFile(const fileName: ustring;
  caseSensitive: Boolean): Boolean;
var
  f: TCefString;
begin
  f := CefString(fileName);
  Result := PCefZipReader(FData).move_to_file(FData, @f, Ord(caseSensitive)) <> 0;
end;

function TCefZipReaderRef.MoveToFirstFile: Boolean;
begin
  Result := PCefZipReader(FData).move_to_first_file(FData) <> 0;
end;

function TCefZipReaderRef.MoveToNextFile: Boolean;
begin
  Result := PCefZipReader(FData).move_to_next_file(FData) <> 0;
end;

function TCefZipReaderRef.OpenFile(const password: ustring): Boolean;
var
  p: TCefString;
begin
  p := CefString(password);
  Result := PCefZipReader(FData).open_file(FData, @p) <> 0;
end;

function TCefZipReaderRef.ReadFile(buffer: Pointer;
  bufferSize: Cardinal): Integer;
begin
    Result := PCefZipReader(FData).read_file(FData, buffer, buffersize);
end;

function TCefZipReaderRef.Tell: LongInt;
begin
  Result := PCefZipReader(FData).tell(FData);
end;

class function TCefZipReaderRef.UnWrap(data: Pointer): ICefZipReader;
begin
  if data <> nil then
    Result := Create(data) as ICefZipReader else
    Result := nil;
end;

{ TCefFastTask }

constructor TCefFastTask.Create(const method: TTaskMethod
{$IFNDEF DELPHI12_UP}
    ; const Browser: ICefBrowser
{$ENDIF}
);
begin
  inherited Create;
{$IFNDEF DELPHI12_UP}
  FBrowser := Browser;
{$ENDIF}
  FMethod := method;
end;

procedure TCefFastTask.Execute(threadId: TCefThreadId);
begin
{$IFDEF DELPHI12_UP}
  FMethod();
{$ELSE}
  FMethod(FBrowser);
{$ENDIF}
end;

class procedure TCefFastTask.Post(threadId: TCefThreadId; const method: TTaskMethod
{$IFNDEF DELPHI12_UP}
    ; const Browser: ICefBrowser
{$ENDIF}
);
begin
  CefPostTask(threadId, Create(method
{$IFNDEF DELPHI12_UP}
    , Browser
{$ENDIF}
  ));
end;

class procedure TCefFastTask.PostDelayed(threadId: TCefThreadId;
  Delay: Integer; const method: TTaskMethod
{$IFNDEF DELPHI12_UP}
    ; const Browser: ICefBrowser
{$ENDIF}
  );
begin
  CefPostDelayedTask(threadId, Create(method
{$IFNDEF DELPHI12_UP}
    , Browser
{$ENDIF}
  ), Delay);
end;

{ TCefv8ContextRef }

class function TCefv8ContextRef.Current: ICefv8Context;
begin
  Result := UnWrap(cef_v8context_get_current_context)
end;

function TCefv8ContextRef.Enter: Boolean;
begin
  Result := PCefv8Context(FData)^.enter(PCefv8Context(FData)) <> 0;
end;

class function TCefv8ContextRef.Entered: ICefv8Context;
begin
  Result := UnWrap(cef_v8context_get_entered_context)
end;

function TCefv8ContextRef.Exit: Boolean;
begin
  Result := PCefv8Context(FData)^.exit(PCefv8Context(FData)) <> 0;
end;

function TCefv8ContextRef.GetBrowser: ICefBrowser;
begin
  Result := TCefBrowserRef.UnWrap(PCefv8Context(FData)^.get_browser(PCefv8Context(FData)));
end;

function TCefv8ContextRef.GetFrame: ICefFrame;
begin
  Result := TCefFrameRef.UnWrap(PCefv8Context(FData)^.get_frame(PCefv8Context(FData)))
end;

function TCefv8ContextRef.GetGlobal: ICefv8Value;
begin
  Result := TCefv8ValueRef.UnWrap(PCefv8Context(FData)^.get_global(PCefv8Context(FData)));
end;

class function TCefv8ContextRef.UnWrap(data: Pointer): ICefv8Context;
begin
  if data <> nil then
    Result := Create(data) as ICefv8Context else
    Result := nil;
end;

{ TCefDomVisitorOwn }

constructor TCefDomVisitorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDomVisitor));
  with PCefDomVisitor(FData)^ do
    visit := @cef_dom_visitor_visite;
end;

procedure TCefDomVisitorOwn.visit(const document: ICefDomDocument);
begin

end;

{ TCefFastDomVisitor }

constructor TCefFastDomVisitor.Create(const proc: TCefFastDomVisitorProc);
begin
  inherited Create;
  FProc := proc;
end;

procedure TCefFastDomVisitor.visit(const document: ICefDomDocument);
begin
  FProc(document);
end;

{ TCefDomDocumentRef }

function TCefDomDocumentRef.GetBaseUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomDocument(FData)^.get_base_url(PCefDomDocument(FData)))
end;

function TCefDomDocumentRef.GetBody: ICefDomNode;
begin
  Result :=  TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_body(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetCompleteUrl(const partialURL: ustring): ustring;
var
  p: TCefString;
begin
  p := CefString(partialURL);
  Result := CefStringFreeAndGet(PCefDomDocument(FData)^.get_complete_url(PCefDomDocument(FData), @p));
end;

function TCefDomDocumentRef.GetDocument: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_document(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetElementById(const id: ustring): ICefDomNode;
var
  i: TCefString;
begin
  i := CefString(id);
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_element_by_id(PCefDomDocument(FData), @i));
end;

function TCefDomDocumentRef.GetFocusedNode: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_focused_node(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetHead: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_head(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetSelectionAsMarkup: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomDocument(FData)^.get_selection_as_markup(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetSelectionAsText: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomDocument(FData)^.get_selection_as_text(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetSelectionEndNode: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_selection_end_node(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetSelectionEndOffset: Integer;
begin
  Result := PCefDomDocument(FData)^.get_selection_end_offset(PCefDomDocument(FData));
end;

function TCefDomDocumentRef.GetSelectionStartNode: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomDocument(FData)^.get_selection_start_node(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetSelectionStartOffset: Integer;
begin
  Result := PCefDomDocument(FData)^.get_selection_start_offset(PCefDomDocument(FData));
end;

function TCefDomDocumentRef.GetTitle: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomDocument(FData)^.get_title(PCefDomDocument(FData)));
end;

function TCefDomDocumentRef.GetType: TCefDomDocumentType;
begin
  Result := PCefDomDocument(FData)^.get_type(PCefDomDocument(FData));
end;

function TCefDomDocumentRef.HasSelection: Boolean;
begin
  Result := PCefDomDocument(FData)^.has_selection(PCefDomDocument(FData)) <> 0;
end;

class function TCefDomDocumentRef.UnWrap(data: Pointer): ICefDomDocument;
begin
  if data <> nil then
    Result := Create(data) as ICefDomDocument else
    Result := nil;
end;

{ TCefDomNodeRef }

procedure TCefDomNodeRef.AddEventListener(const eventType: ustring;
  useCapture: Boolean; const listener: ICefDomEventListener);
var
  et: TCefString;
begin
  et := CefString(eventType);
  PCefDomNode(FData)^.add_event_listener(PCefDomNode(FData), @et, CefGetData(listener), Ord(useCapture));
end;

procedure TCefDomNodeRef.AddEventListenerProc(const eventType: ustring; useCapture: Boolean;
  const proc: TCefFastDomEventListenerProc);
begin
  AddEventListener(eventType, useCapture, TCefFastDomEventListener.Create(proc) as ICefDomEventListener);
end;

function TCefDomNodeRef.GetAsMarkup: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomNode(FData)^.get_as_markup(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetDocument: ICefDomDocument;
begin
  Result := TCefDomDocumentRef.UnWrap(PCefDomNode(FData)^.get_document(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetElementAttribute(const attrName: ustring): ustring;
var
  p: TCefString;
begin
  p := CefString(attrName);
  Result := CefStringFreeAndGet(PCefDomNode(FData)^.get_element_attribute(PCefDomNode(FData), @p));
end;

procedure TCefDomNodeRef.GetElementAttributes(const attrMap: ICefStringMap);
begin
  PCefDomNode(FData)^.get_element_attributes(PCefDomNode(FData), attrMap.Handle);
end;

function TCefDomNodeRef.GetElementInnerText: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomNode(FData)^.get_element_inner_text(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetElementTagName: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomNode(FData)^.get_element_tag_name(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetFirstChild: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)^.get_first_child(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetLastChild: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)^.get_last_child(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetName: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomNode(FData)^.get_name(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetNextSibling: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)^.get_next_sibling(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetParent: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)^.get_parent(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetPreviousSibling: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomNode(FData)^.get_previous_sibling(PCefDomNode(FData)));
end;

function TCefDomNodeRef.GetType: TCefDomNodeType;
begin
  Result := PCefDomNode(FData)^.get_type(PCefDomNode(FData));
end;

function TCefDomNodeRef.GetValue: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomNode(FData)^.get_value(PCefDomNode(FData)));
end;

function TCefDomNodeRef.HasChildren: Boolean;
begin
  Result := PCefDomNode(FData)^.has_children(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.HasElementAttribute(const attrName: ustring): Boolean;
var
  p: TCefString;
begin
  p := CefString(attrName);
  Result := PCefDomNode(FData)^.has_element_attribute(PCefDomNode(FData), @p) <> 0;
end;

function TCefDomNodeRef.HasElementAttributes: Boolean;
begin
  Result := PCefDomNode(FData)^.has_element_attributes(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.IsElement: Boolean;
begin
  Result := PCefDomNode(FData)^.is_element(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.IsSame(const that: ICefDomNode): Boolean;
begin
  Result := PCefDomNode(FData)^.is_same(PCefDomNode(FData), CefGetData(that)) <> 0;
end;

function TCefDomNodeRef.IsText: Boolean;
begin
  Result := PCefDomNode(FData)^.is_text(PCefDomNode(FData)) <> 0;
end;

function TCefDomNodeRef.SetElementAttribute(const attrName,
  value: ustring): Boolean;
var
  p1, p2: TCefString;
begin
  p1 := CefString(attrName);
  p2 := CefString(value);
  Result := PCefDomNode(FData)^.set_element_attribute(PCefDomNode(FData), @p1, @p2) <> 0;
end;

function TCefDomNodeRef.SetValue(const value: ustring): Boolean;
var
  p: TCefString;
begin
  p := CefString(value);
  Result := PCefDomNode(FData)^.set_value(PCefDomNode(FData), @p) <> 0;
end;

class function TCefDomNodeRef.UnWrap(data: Pointer): ICefDomNode;
begin
  if data <> nil then
    Result := Create(data) as ICefDomNode else
    Result := nil;
end;

{ TCefDomEventListenerOwn }

constructor TCefDomEventListenerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDomEventListener));
  with PCefDomEventListener(FData)^ do
    handle_event := @cef_dom_event_listener_handle_event;
end;

procedure TCefDomEventListenerOwn.HandleEvent(const event: ICefDomEvent);
begin

end;

{ TCefDomEventRef }

function TCefDomEventRef.CanBubble: Boolean;
begin
  Result := PCefDomEvent(FData)^.can_bubble(PCefDomEvent(FData)) <> 0;
end;

function TCefDomEventRef.CanCancel: Boolean;
begin
  Result := PCefDomEvent(FData)^.can_cancel(PCefDomEvent(FData)) <> 0;
end;

function TCefDomEventRef.GetCategory: TCefDomEventCategory;
begin
  Result := PCefDomEvent(FData)^.get_category(PCefDomEvent(FData));
end;

function TCefDomEventRef.GetCurrentTarget: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomEvent(FData)^.get_current_target(PCefDomEvent(FData)));
end;

function TCefDomEventRef.GetDocument: ICefDomDocument;
begin
  Result := TCefDomDocumentRef.UnWrap(PCefDomEvent(FData)^.get_document(PCefDomEvent(FData)));
end;

function TCefDomEventRef.GetPhase: TCefDomEventPhase;
begin
  Result := PCefDomEvent(FData)^.get_phase(PCefDomEvent(FData));
end;

function TCefDomEventRef.GetTarget: ICefDomNode;
begin
  Result := TCefDomNodeRef.UnWrap(PCefDomEvent(FData)^.get_target(PCefDomEvent(FData)));
end;

function TCefDomEventRef.GetType: ustring;
begin
  Result := CefStringFreeAndGet(PCefDomEvent(FData)^.get_type(PCefDomEvent(FData)));
end;

class function TCefDomEventRef.UnWrap(data: Pointer): ICefDomEvent;
begin
  if data <> nil then
    Result := Create(data) as ICefDomEvent else
    Result := nil;
end;

{ TCefFastDomEventListener }

constructor TCefFastDomEventListener.Create(
  const proc: TCefFastDomEventListenerProc);
begin
  inherited Create;
  FProc := proc;
end;

procedure TCefFastDomEventListener.HandleEvent(const event: ICefDomEvent);
begin
  inherited;
  FProc(event);
end;

{ TCefResponseRef }

function TCefResponseRef.GetHeader(const name: ustring): ustring;
var
  n: TCefString;
begin
  n := CefString(name);
  Result := CefStringFreeAndGet(PCefResponse(FData)^.get_header(PCefResponse(FData), @n));
end;

procedure TCefResponseRef.GetHeaderMap(const headerMap: ICefStringMap);
begin
  PCefResponse(FData)^.get_header_map(PCefResponse(FData), headermap.Handle);
end;

function TCefResponseRef.GetMimeType: ustring;
begin
  Result := CefStringFreeAndGet(PCefResponse(FData)^.get_mime_type(PCefResponse(FData)));
end;

function TCefResponseRef.GetStatus: Integer;
begin
  Result := PCefResponse(FData)^.get_status(PCefResponse(FData));
end;

function TCefResponseRef.GetStatusText: ustring;
begin
  Result := CefStringFreeAndGet(PCefResponse(FData)^.get_status_text(PCefResponse(FData)));
end;

procedure TCefResponseRef.SetHeaderMap(const headerMap: ICefStringMap);
begin
  PCefResponse(FData)^.set_header_map(PCefResponse(FData), headerMap.Handle);
end;

procedure TCefResponseRef.SetMimeType(const mimetype: ustring);
var
  txt: TCefString;
begin
  txt := CefString(mimetype);
  PCefResponse(FData)^.set_mime_type(PCefResponse(FData), @txt);
end;

procedure TCefResponseRef.SetStatus(status: Integer);
begin
  PCefResponse(FData)^.set_status(PCefResponse(FData), status);
end;

procedure TCefResponseRef.SetStatusText(const StatusText: ustring);
var
  txt: TCefString;
begin
  txt := CefString(StatusText);
  PCefResponse(FData)^.set_status_text(PCefResponse(FData), @txt);
end;

class function TCefResponseRef.UnWrap(data: Pointer): ICefResponse;
begin
  if data <> nil then
    Result := Create(data) as ICefResponse else
    Result := nil;
end;

{ TCefWebUrlRequestClientOwn }

constructor TCefWebUrlRequestClientOwn.Create;
begin
  inherited CreateData(SizeOf(TCefWebUrlRequestClient));
  PCefWebUrlRequestClient(FData)^.on_state_change := @cef_web_url_request_client_on_state_change;
  PCefWebUrlRequestClient(FData)^.on_redirect := @cef_web_url_request_client_on_redirect;
  PCefWebUrlRequestClient(FData)^.on_headers_received := @cef_web_url_request_client_on_headers_received;
  PCefWebUrlRequestClient(FData)^.on_progress := @cef_web_url_request_client_on_progress;
  PCefWebUrlRequestClient(FData)^.on_data := @cef_web_url_request_client_on_data;
  PCefWebUrlRequestClient(FData)^.on_error := @cef_web_url_request_client_on_error;
end;

procedure TCefWebUrlRequestClientOwn.OnData(const requester: ICefWebUrlRequest;
  const data: Pointer; dataLength: Integer);
begin

end;

procedure TCefWebUrlRequestClientOwn.OnError(const requester: ICefWebUrlRequest;
  errorCode: TCefHandlerErrorcode);
begin

end;

procedure TCefWebUrlRequestClientOwn.OnHeadersReceived(
  const requester: ICefWebUrlRequest; const response: ICefResponse);
begin

end;

procedure TCefWebUrlRequestClientOwn.OnProgress(
  const requester: ICefWebUrlRequest; bytesSent, totalBytesToBeSent: uint64);
begin

end;

procedure TCefWebUrlRequestClientOwn.OnRedirect(
  const requester: ICefWebUrlRequest; const request: ICefRequest;
  const response: ICefResponse);
begin

end;

procedure TCefWebUrlRequestClientOwn.OnStateChange(
  const requester: ICefWebUrlRequest; state: TCefWebUrlRequestState);
begin

end;

{ TCefWebUrlRequestRef }

procedure TCefWebUrlRequestRef.Cancel;
begin
  PCefWebUrlRequest(FData)^.cancel(PCefWebUrlRequest(FData));
end;

class function TCefWebUrlRequestRef.New(const request: ICefRequest;
  const client: ICefWebUrlRequestClient): ICefWebUrlRequest;
begin
  Result := UnWrap(cef_web_urlrequest_create(CefGetData(request), CefGetData(client)));
end;

function TCefWebUrlRequestRef.GetState: TCefWebUrlRequestState;
begin
  Result := PCefWebUrlRequest(FData)^.get_state(PCefWebUrlRequest(FData));
end;

class function TCefWebUrlRequestRef.UnWrap(data: Pointer): ICefWebUrlRequest;
begin
  if data <> nil then
    Result := Create(data) as ICefWebUrlRequest else
    Result := nil;
end;

{ TCefRTTIExtension }

{$IFDEF DELPHI14_UP}

constructor TCefRTTIExtension.Create(const value: TValue
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
; SyncMainThread: Boolean
{$ENDIF}
);
begin
  inherited Create;
  FCtx := TRttiContext.Create;
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  FSyncMainThread := SyncMainThread;
{$ENDIF}
  FValue := value;
end;

destructor TCefRTTIExtension.Destroy;
begin
  FCtx.Free;
  inherited;
end;

function TCefRTTIExtension.GetValue(pi: PTypeInfo; const v: ICefv8Value; var ret: TValue): Boolean;

  function ProcessInt: Boolean;
  var
    sv: record
      case byte of
      0:  (ub: Byte);
      1:  (sb: Short);
      2:  (uw: Word);
      3:  (sw: SmallInt);
      4:  (si: Integer);
      5:  (ui: Cardinal);
    end;
    pd: PTypeData;
  begin
    pd := GetTypeData(pi);
    if v.IsInt and (v.GetIntValue >= pd.MinValue) and (v.GetIntValue <= pd.MaxValue) then
    begin
      case pd.OrdType of
        otSByte: sv.sb := v.GetIntValue;
        otUByte: sv.ub := v.GetIntValue;
        otSWord: sv.sw := v.GetIntValue;
        otUWord: sv.uw := v.GetIntValue;
        otSLong: sv.si := v.GetIntValue;
        otULong: sv.ui := v.GetIntValue;
      end;
      TValue.Make(@sv, pi, ret);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessInt64: Boolean;
  var
    i: Int64;
  begin
    i := StrToInt64(v.GetStringValue); // hack
    TValue.Make(@i, pi, ret);
    Result := True;
  end;

  function ProcessUString: Boolean;
  var
    vus: string;
  begin
    if v.IsString then
    begin
      vus := v.GetStringValue;
      TValue.Make(@vus, pi, ret);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessLString: Boolean;
  var
    vas: AnsiString;
  begin
    if v.IsString then
    begin
      vas := AnsiString(v.GetStringValue);
      TValue.Make(@vas, pi, ret);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessWString: Boolean;
  var
    vws: WideString;
  begin
    if v.IsString then
    begin
      vws := v.GetStringValue;
      TValue.Make(@vws, pi, ret);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessFloat: Boolean;
  var
    sv: record
      case byte of
      0: (fs: Single);
      1: (fd: Double);
      2: (fe: Extended);
      3: (fc: Comp);
      4: (fcu: Currency);
    end;
  begin
    if v.IsDouble or v.IsInt then
    begin
      case GetTypeData(pi).FloatType of
        ftSingle: sv.fs := v.GetDoubleValue;
        ftDouble: sv.fd := v.GetDoubleValue;
        ftExtended: sv.fe := v.GetDoubleValue;
        ftComp: sv.fc := v.GetDoubleValue;
        ftCurr: sv.fcu := v.GetDoubleValue;
      end;
      TValue.Make(@sv, pi, ret);
    end else
    if v.IsDate then
    begin
      sv.fd := v.GetDateValue;
      TValue.Make(@sv, pi, ret);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessSet: Boolean;
  var
    sv: record
      case byte of
      0:  (ub: Byte);
      1:  (sb: Short);
      2:  (uw: Word);
      3:  (sw: SmallInt);
      4:  (si: Integer);
      5:  (ui: Cardinal);
    end;
  begin
    if v.IsInt then
    begin
      case GetTypeData(pi).OrdType of
        otSByte: sv.sb := v.GetIntValue;
        otUByte: sv.ub := v.GetIntValue;
        otSWord: sv.sw := v.GetIntValue;
        otUWord: sv.uw := v.GetIntValue;
        otSLong: sv.si := v.GetIntValue;
        otULong: sv.ui := v.GetIntValue;
      end;
      TValue.Make(@sv, pi, ret);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessVariant: Boolean;
  var
    vr: Variant;
    i: Integer;
    vl: TValue;
  begin
    VarClear(vr);
    if v.IsString then vr := v.GetStringValue else
    if v.IsBool then vr := v.GetBoolValue else
    if v.IsInt then vr := v.GetIntValue else
    if v.IsDouble then vr := v.GetDoubleValue else
    if v.IsUndefined then TVarData(vr).VType := varEmpty else
    if v.IsNull then TVarData(vr).VType := varNull else
    if v.IsArray then
      begin
        vr := VarArrayCreate([0, v.GetArrayLength], varVariant);
        for i := 0 to v.GetArrayLength - 1 do
        begin
          if not GetValue(pi, v.GetValueByIndex(i), vl) then Exit(False);
          VarArrayPut(vr, vl.AsVariant, i);
        end;
      end else
      Exit(False);
    TValue.Make(@vr, pi, ret);
    Result := True;
  end;

  function ProcessObject: Boolean;
  var
    ud: ICefv8Value;
    i: Integer;// Pointer
    td: PTypeData;
    rt: TRttiType;
  begin
    if v.IsObject then
    begin
      ud := v.GetUserData;
      if (ud = nil) then Exit(False);
      rt := TRttiType(ud.GetValueByIndex(0).GetIntValue);
      td := GetTypeData(rt.Handle);

      if (rt.TypeKind = tkClass) and td.ClassType.InheritsFrom(GetTypeData(pi).ClassType) then
      begin
        i := ud.GetValueByIndex(1).GetIntValue;
        TValue.Make(@i, pi, ret);
      end else
        Exit(False);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessClass: Boolean;
  var
    ud: ICefv8Value;
    i: Integer;// Pointer
    rt: TRttiType;
  begin
    if v.IsObject then
    begin
      ud := v.GetUserData;
      if (ud = nil) then Exit(False);
      rt := TRttiType(ud.GetValueByIndex(0).GetIntValue);
      if (rt.TypeKind = tkClassRef) then
      begin
        i := ud.GetValueByIndex(1).GetIntValue;
        TValue.Make(@i, pi, ret);
      end else
        Exit(False);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessRecord: Boolean;
  var
    r: TRttiField;
    f: TValue;
    rec: Pointer;
  begin
    if v.IsObject then
    begin
      TValue.Make(nil, pi, ret);
{$IFDEF DELPHI15_UP}
      rec := TValueData(ret).FValueData.GetReferenceToRawData;
{$ELSE}
      rec := IValueData(TValueData(ret).FHeapData).GetReferenceToRawData;
{$ENDIF}
      for r in FCtx.GetType(pi).GetFields do
      begin
        if not GetValue(r.FieldType.Handle, v.GetValueByKey(r.Name), f) then
          Exit(False);
        r.SetValue(rec, f);
      end;
      Result := True;
    end else
      Result := False;
  end;

  function ProcessInterface: Boolean;
  begin
    if pi = TypeInfo(ICefV8Value) then
    begin
      TValue.Make(@v, pi, ret);
      Result := True;
    end else
      Result := False; // todo
  end;

begin
  case pi.Kind of
    tkInteger, tkEnumeration: Result := ProcessInt;
    tkInt64: Result := ProcessInt64;
    tkUString: Result := ProcessUString;
    tkLString: Result := ProcessLString;
    tkWString: Result := ProcessWString;
    tkFloat: Result := ProcessFloat;
    tkSet: Result := ProcessSet;
    tkVariant: Result := ProcessVariant;
    tkClass: Result := ProcessObject;
    tkClassRef: Result := ProcessClass;
    tkRecord: Result := ProcessRecord;
    tkInterface: Result := ProcessInterface;
  else
    Result := False;
  end;
end;

function TCefRTTIExtension.SetValue(const v: TValue; var ret: ICefv8Value): Boolean;

  function ProcessRecord: Boolean;
  var
    rf: TRttiField;
    vl: TValue;
    ud, v8: ICefv8Value;
    rec: Pointer;
    rt: TRttiType;
  begin
    ud := TCefv8ValueRef.CreateArray;
    rt := FCtx.GetType(v.TypeInfo);
    ud.SetValueByIndex(0, TCefv8ValueRef.CreateInt(Integer(rt)));
    ret := TCefv8ValueRef.CreateObject(ud);

{$IFDEF DELPHI15_UP}
    rec := TValueData(v).FValueData.GetReferenceToRawData;
{$ELSE}
    rec := IValueData(TValueData(v).FHeapData).GetReferenceToRawData;
{$ENDIF}
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    if FSyncMainThread then
    begin
      v8 := ret;
      TThread.Synchronize(nil, procedure
      var
        rf: TRttiField;
        o: ICefv8Value;
      begin
        for rf in rt.GetFields do
        begin
          vl := rf.GetValue(rec);
          SetValue(vl, o);
          v8.SetValueByKey(rf.Name, o);
        end;
      end)
    end else
{$ENDIF}
      for rf in FCtx.GetType(v.TypeInfo).GetFields do
      begin
        vl := rf.GetValue(rec);
        if not SetValue(vl, v8) then
          Exit(False);
        ret.SetValueByKey(rf.Name, v8);
      end;
    Result := True;
  end;

  function ProcessObject: Boolean;
  var
    m: TRttiMethod;
    p: TRttiProperty;
    fl: TRttiField;
    f: ICefv8Value;
    _r, _g, _s, ud: ICefv8Value;
    _e: ustring;
    _a: TCefv8ValueArray;
    proto: ICefv8Value;
    rt: TRttiType;
  begin
    rt := FCtx.GetType(v.TypeInfo);

    ud := TCefv8ValueRef.CreateArray;
    ud.SetValueByIndex(0, TCefv8ValueRef.CreateInt(Integer(rt)));
    ud.SetValueByIndex(1, TCefv8ValueRef.CreateInt(Integer(v.AsObject)));
    ret := TCefv8ValueRef.CreateObject(ud);
    proto := ret.GetValueByKey('__proto__');

    for m in rt.GetMethods do
      if m.Visibility > mvProtected then
      begin
        f := TCefv8ValueRef.CreateFunction(m.Name, Self);
        proto.SetValueByKey(m.Name, f);
      end;

    for p in rt.GetProperties do
      if (p.Visibility > mvProtected) then
      begin
        if _g = nil then _g := proto.GetValueByKey('__defineGetter__');
        if _s = nil then _s := proto.GetValueByKey('__defineSetter__');
        SetLength(_a, 2);
        _a[0] := TCefv8ValueRef.CreateString(p.Name);
        if p.IsReadable then
        begin
          _a[1] := TCefv8ValueRef.CreateFunction('$pg' + p.Name, Self);
          _g.ExecuteFunction(ret, _a, _r, _e);
        end;
        if p.IsWritable then
        begin
          _a[1] := TCefv8ValueRef.CreateFunction('$ps' + p.Name, Self);
          _s.ExecuteFunction(ret, _a, _r, _e);
        end;
      end;

    for fl in rt.GetFields do
      if (fl.Visibility > mvProtected) then
      begin
        if _g = nil then _g := proto.GetValueByKey('__defineGetter__');
        if _s = nil then _s := proto.GetValueByKey('__defineSetter__');

        SetLength(_a, 2);
        _a[0] := TCefv8ValueRef.CreateString(fl.Name);
        _a[1] := TCefv8ValueRef.CreateFunction('$vg' + fl.Name, Self);
        _g.ExecuteFunction(ret, _a, _r, _e);
        _a[1] := TCefv8ValueRef.CreateFunction('$vs' + fl.Name, Self);
        _s.ExecuteFunction(ret, _a, _r, _e);
      end;

    Result := True;
  end;

  function ProcessClass: Boolean;
  var
    m: TRttiMethod;
    f, ud: ICefv8Value;
    c: TClass;
    proto: ICefv8Value;
    rt: TRttiType;
  begin
    c := v.AsClass;
    rt := FCtx.GetType(c);

    ud := TCefv8ValueRef.CreateArray;
    ud.SetValueByIndex(0, TCefv8ValueRef.CreateInt(Integer(rt)));
    ud.SetValueByIndex(1, TCefv8ValueRef.CreateInt(Integer(c)));
    ret := TCefv8ValueRef.CreateObject(ud);
    if c <> nil then
    begin
      proto := ret.GetValueByKey('__proto__');
      for m in rt.GetMethods do
        if (m.Visibility > mvProtected) and (m.MethodKind in [mkClassProcedure, mkClassFunction]) then
        begin
          f := TCefv8ValueRef.CreateFunction(m.Name, Self);
          proto.SetValueByKey(m.Name, f);
        end;
    end;

    Result := True;
  end;

  function ProcessVariant: Boolean;
  var
    vr: Variant;
  begin
    vr := v.AsVariant;
    case TVarData(vr).VType of
      varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64, varUInt64:
        ret := TCefv8ValueRef.CreateInt(vr);
      varUString, varOleStr, varString:
        ret := TCefv8ValueRef.CreateString(vr);
      varSingle, varDouble, varCurrency:
        ret := TCefv8ValueRef.CreateDouble(vr);
      varBoolean:
        ret := TCefv8ValueRef.CreateBool(vr);
      varNull:
        ret := TCefv8ValueRef.CreateNull;
      varEmpty:
        ret := TCefv8ValueRef.CreateUndefined;
    else
      ret := nil;
      Exit(False)
    end;
    Result := True;
  end;

  function ProcessInterface: Boolean;
  var
    m: TRttiMethod;
    f: ICefv8Value;
    ud: ICefv8Value;
    proto: ICefv8Value;
    rt: TRttiType;
  begin
    rt := FCtx.GetType(v.TypeInfo);

    ud := TCefv8ValueRef.CreateArray;
    ud.SetValueByIndex(0, TCefv8ValueRef.CreateInt(Integer(rt)));
    ud.SetValueByIndex(1, TCefv8ValueRef.CreateInt(Integer(v.AsInterface)));
    ret := TCefv8ValueRef.CreateObject(ud);
    proto := ret.GetValueByKey('__proto__');


    for m in rt.GetMethods do
      if m.Visibility > mvProtected then
      begin
        f := TCefv8ValueRef.CreateFunction(m.Name, Self);
        proto.SetValueByKey(m.Name, f);
      end;

    Result := True;
  end;

  function ProcessFloat: Boolean;
  begin
    if v.TypeInfo = TypeInfo(TDateTime) then
      ret := TCefv8ValueRef.CreateDate(TValueData(v).FAsDouble) else
      ret := TCefv8ValueRef.CreateDouble(v.AsExtended);
    Result := True;
  end;

begin
  case v.TypeInfo.Kind of
    tkUString, tkLString, tkWString, tkChar, tkWChar:
      ret := TCefv8ValueRef.CreateString(v.AsString);
    tkInteger: ret := TCefv8ValueRef.CreateInt(v.AsInteger);
    tkEnumeration:
      if v.TypeInfo = TypeInfo(Boolean) then
        ret := TCefv8ValueRef.CreateBool(v.AsBoolean) else
        ret := TCefv8ValueRef.CreateInt(TValueData(v).FAsSLong);
    tkFloat: if not ProcessFloat then Exit(False);
    tkInt64: ret := TCefv8ValueRef.CreateInt(v.AsInt64);
    tkClass: if not ProcessObject then Exit(False);
    tkClassRef: if not ProcessClass then Exit(False);
    tkRecord: if not ProcessRecord then Exit(False);
    tkVariant: if not ProcessVariant then Exit(False);
    tkInterface: if not ProcessInterface then Exit(False);
  else
    Exit(False)
  end;
  Result := True;
end;

class procedure TCefRTTIExtension.Register(const name: string;
  const value: TValue{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}; SyncMainThread: Boolean{$ENDIF});
begin
  CefRegisterExtension(name,
    format('__defineSetter__(''%s'', function(v){native function $s();$s(v)});__defineGetter__(''%0:s'', function(){native function $g();return $g()});', [name]),
    TCefRTTIExtension.Create(value
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    , SyncMainThread
{$ENDIF}
    ) as ICefv8Handler);
end;

function TCefRTTIExtension.Execute(const name: ustring; const obj: ICefv8Value;
  const arguments: TCefv8ValueArray; var retval: ICefv8Value;
  var exception: ustring): Boolean;
var
  p: PChar;
  ud: ICefv8Value;
  rt: TRttiType;
  val: TObject;
  cls: TClass;
  m: TRttiMethod;
  pr: TRttiProperty;
  vl: TRttiField;
  args: array of TValue;
  prm: TArray<TRttiParameter>;
  i: Integer;
  ret: TValue;
begin
  Result := True;
  p := PChar(name);
  m := nil;
  if obj <> nil then
  begin
    ud := obj.GetUserData;
    if ud <> nil then
    begin
      rt := TRttiType(ud.GetValueByIndex(0).GetIntValue);
      case rt.TypeKind of
        tkClass:
          begin
            val := TObject(ud.GetValueByIndex(1).GetIntValue);
            cls := GetTypeData(rt.Handle).ClassType;

            if p^ = '$' then
            begin
              inc(p);
              case p^ of
                'p':
                  begin
                    inc(p);
                    case p^ of
                    'g':
                      begin
                        inc(p);
                        pr := rt.GetProperty(p);
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
                        if FSyncMainThread then
                        begin
                          TThread.Synchronize(nil, procedure begin
                            ret := pr.GetValue(val);
                          end);
                          Exit(SetValue(ret, retval));
                        end else
{$ENDIF}
                          Exit(SetValue(pr.GetValue(val), retval));
                      end;
                    's':
                      begin
                        inc(p);
                        pr := rt.GetProperty(p);
                        if GetValue(pr.PropertyType.Handle, arguments[0], ret) then
                        begin
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
                          if FSyncMainThread then
                            TThread.Synchronize(nil, procedure begin
                              pr.SetValue(val, ret) end) else
{$ENDIF}
                            pr.SetValue(val, ret);
                          Exit(True);
                        end else
                          Exit(False);
                      end;
                    end;
                  end;
                'v':
                  begin
                    inc(p);
                    case p^ of
                    'g':
                      begin
                        inc(p);
                        vl := rt.GetField(p);
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
                        if FSyncMainThread then
                        begin
                          TThread.Synchronize(nil, procedure begin
                            ret := vl.GetValue(val);
                          end);
                          Exit(SetValue(ret, retval));
                        end else
{$ENDIF}
                          Exit(SetValue(vl.GetValue(val), retval));
                      end;
                    's':
                      begin
                        inc(p);
                        vl := rt.GetField(p);
                        if GetValue(vl.FieldType.Handle, arguments[0], ret) then
                        begin
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
                          if FSyncMainThread then
                            TThread.Synchronize(nil, procedure begin
                              vl.SetValue(val, ret) end) else
{$ENDIF}
                            vl.SetValue(val, ret);
                          Exit(True);
                        end else
                          Exit(False);
                      end;
                    end;
                  end;
              end;
            end else
              m := rt.GetMethod(name);
          end;
        tkClassRef:
          begin
            val := nil;
            cls := TClass(ud.GetValueByIndex(1).GetIntValue);
            m := FCtx.GetType(cls).GetMethod(name);
          end;
      else
        m := nil;
        cls := nil;
        val := nil;
      end;

      prm := m.GetParameters;
      i := Length(prm);
      if i = Length(arguments) then
      begin
        SetLength(args, i);
        for i := 0 to i - 1 do
          if not GetValue(prm[i].ParamType.Handle, arguments[i], args[i]) then
            Exit(False);

        case m.MethodKind of
          mkClassProcedure, mkClassFunction:
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
            if FSyncMainThread then
              TThread.Synchronize(nil, procedure begin
                ret := m.Invoke(cls, args) end) else
{$ENDIF}
              ret := m.Invoke(cls, args);
          mkProcedure, mkFunction:
            if (val <> nil) then
            begin
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
              if FSyncMainThread then
                TThread.Synchronize(nil, procedure begin
                  ret := m.Invoke(val, args) end) else
{$ENDIF}
                ret := m.Invoke(val, args);
            end else
              Exit(False)
        else
          Exit(False);
        end;

        if m.MethodKind in [mkClassFunction, mkFunction] then
          if not SetValue(ret, retval) then
            Exit(False);
      end else
        Exit(False);
    end else
    if p^ = '$' then
    begin
      inc(p);
      case p^ of
        'g': SetValue(FValue, retval);
        's': GetValue(FValue.TypeInfo, arguments[0], FValue);
      else
        Exit(False);
      end;
    end else
      Exit(False);
  end else
    Exit(False);
end;
{$ENDIF}

{ TCefV8AccessorOwn }

constructor TCefV8AccessorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefV8Accessor));
  PCefV8Accessor(FData)^.get  := @cef_v8_accessor_get;
  PCefV8Accessor(FData)^.put := @cef_v8_accessor_put;
end;

function TCefV8AccessorOwn.Get(const name: ustring; const obj: ICefv8Value;
  out value: ICefv8Value; const exception: string): Boolean;
begin
  Result := False;
end;

function TCefV8AccessorOwn.Put(const name: ustring; const obj,
  value: ICefv8Value; const exception: string): Boolean;
begin
  Result := False;
end;

{ TCefFastV8Accessor }

constructor TCefFastV8Accessor.Create(
  const getter: TCefFastV8AccessorGetterProc;
  const setter: TCefFastV8AccessorSetterProc);
begin
  FGetter := getter;
  FSetter := setter;
end;

function TCefFastV8Accessor.Get(const name: ustring; const obj: ICefv8Value;
  out value: ICefv8Value; const exception: string): Boolean;
begin
  if Assigned(FGetter)  then
    Result := FGetter(name, obj, value, exception) else
    Result := False;
end;

function TCefFastV8Accessor.Put(const name: ustring; const obj,
  value: ICefv8Value; const exception: string): Boolean;
begin
  if Assigned(FSetter)  then
    Result := FSetter(name, obj, value, exception) else
    Result := False;
end;

{ TCefCookieVisitorOwn }

constructor TCefCookieVisitorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefCookieVisitor));
  PCefCookieVisitor(FData)^.visit := @cef_cookie_visitor_visit;
end;

function TCefCookieVisitorOwn.visit(const name, value, domain, path: ustring;
  secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime;
  count, total: Integer; out deleteCookie: Boolean): Boolean;
begin
  Result := True;
end;

{ TCefStorageVisitorOwn }

constructor TCefStorageVisitorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefStorageVisitor));
  PCefStorageVisitor(FData)^.visit := @cef_storage_visitor_visit;
end;

function TCefStorageVisitorOwn.visit(StorageType: TCefStorageType; const origin,
  key, value: ustring; count, total: Integer; out deleteData: Boolean): Boolean;
begin
  Result := True;
end;

{ TCefFastCookieVisitor }

constructor TCefFastCookieVisitor.Create(const visitor: TCefCookieVisitorProc);
begin
  inherited Create;
  FVisitor := visitor;
end;

function TCefFastCookieVisitor.visit(const name, value, domain, path: ustring;
  secure, httponly, hasExpires: Boolean; const creation, lastAccess,
  expires: TDateTime; count, total: Integer; out deleteCookie: Boolean): Boolean;
begin
  Result := FVisitor(name, value, domain, path, secure, httponly, hasExpires,
    creation, lastAccess, expires, count, total, deleteCookie);
end;

{ TCefFastStorageVisitor }

constructor TCefFastStorageVisitor.Create(
  const visitor: TCefStorageVisitorProc);
begin
  inherited Create;
  FVisitor := visitor;
end;

function TCefFastStorageVisitor.visit(StorageType: TCefStorageType;
  const origin, key, value: ustring; count, total: Integer;
  out deleteData: Boolean): Boolean;
begin
  Result := FVisitor(StorageType, origin, key, value, count, total, deleteData);
end;

{ TCefClientOwn }

constructor TCefClientOwn.Create;
begin
  inherited CreateData(SizeOf(TCefClient));
  with PCefClient(FData)^ do
  begin
    get_life_span_handler := @cef_client_get_life_span_handler;
    get_load_handler := @cef_client_get_load_handler;
    get_request_handler := @cef_client_get_request_handler;
    get_display_handler := @cef_client_get_display_handler;
    get_focus_handler := @cef_client_get_focus_handler;
    get_keyboard_handler := @cef_client_get_keyboard_handler;
    get_menu_handler := @cef_client_get_menu_handler;
    get_print_handler := @cef_client_get_print_handler;
    get_find_handler := @cef_client_get_find_handler;
    get_jsdialog_handler := @cef_client_get_jsdialog_handler;
    get_jsbinding_handler := @cef_client_get_jsbinding_handler;
    get_render_handler := @cef_client_get_render_handler;
    get_drag_handler := @cef_client_get_drag_handler;
  end;
end;

function TCefClientOwn.GetDisplayHandler: ICefBase;
begin
  Result := nil;
end;

function TCefClientOwn.GetDragHandler: ICefBase;
begin
  Result := nil;
end;

function TCefClientOwn.GetFindHandler: ICefBase;
begin
  Result := nil;
end;

function TCefClientOwn.GetFocusHandler: ICefBase;
begin
  Result := nil;
end;

function TCefClientOwn.GetJsbindingHandler: ICefBase;
begin
  Result := nil;
end;

function TCefClientOwn.GetJsdialogHandler: ICefBase;
begin
  Result := nil;
end;

function TCefClientOwn.GetKeyboardHandler: ICefBase;
begin
  Result := nil;
end;

function TCefClientOwn.GetLifeSpanHandler: ICefBase;
begin
  Result := nil;
end;

function TCefClientOwn.GetLoadHandler: ICefBase;
begin
  Result := nil;
end;

function TCefClientOwn.GetMenuHandler: ICefBase;
begin
  Result := nil;
end;

function TCefClientOwn.GetPrintHandler: ICefBase;
begin
  Result := nil;
end;

function TCefClientOwn.GetRenderHandler: ICefBase;
begin
  Result := nil;
end;

function TCefClientOwn.GetRequestHandler: ICefBase;
begin
  Result := nil;
end;

{ TCefLifeSpanHandlerOwn }

constructor TCefLifeSpanHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefLifeSpanHandler));
  with PCefLifeSpanHandler(FData)^ do
  begin
    on_before_popup := @cef_life_span_handler_on_before_popup;
    on_after_created := @cef_life_span_handler_on_after_created;
    on_before_close := @cef_life_span_handler_on_before_close;
    run_modal := @cef_life_span_handler_run_modal;
    do_close := @cef_life_span_handler_do_close;
  end;
end;

procedure TCefLifeSpanHandlerOwn.OnAfterCreated(const browser: ICefBrowser);
begin

end;

procedure TCefLifeSpanHandlerOwn.OnBeforeClose(const browser: ICefBrowser);
begin

end;

function TCefLifeSpanHandlerOwn.OnBeforePopup(const parentBrowser: ICefBrowser;
  var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var url: ustring; var client: ICefBase;
  var settings: TCefBrowserSettings): Boolean;
begin
  Result := False;
end;

function TCefLifeSpanHandlerOwn.DoClose(const browser: ICefBrowser): Boolean;
begin
  Result := False;
end;

function TCefLifeSpanHandlerOwn.RunModal(const browser: ICefBrowser): Boolean;
begin
  Result := False;
end;


{ TCefLoadHandlerOwn }

constructor TCefLoadHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefLoadHandler));
  with PCefLoadHandler(FData)^ do
  begin
    on_load_start := @cef_load_handler_on_load_start;
    on_load_end := @cef_load_handler_on_load_end;
    on_load_error := @cef_load_handler_on_load_error;
  end;
end;

procedure TCefLoadHandlerOwn.OnLoadEnd(const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer);
begin

end;

function TCefLoadHandlerOwn.OnLoadError(const browser: ICefBrowser;
  const frame: ICefFrame; errorCode: TCefHandlerErrorcode; const failedUrl: ustring;
  var errorText: ustring): Boolean;
begin
  Result := False;
end;

procedure TCefLoadHandlerOwn.OnLoadStart(const browser: ICefBrowser;
  const frame: ICefFrame);
begin

end;

{ TCefContentFilterOwn }

constructor TCefContentFilterOwn.Create;
begin
  inherited CreateData(SizeOf(TCefContentFilter));
  with PCefContentFilter(FData)^ do
  begin
    process_data := @cef_content_filter_process_data;
    drain := @cef_content_filter_drain;
  end;
end;

procedure TCefContentFilterOwn.Drain(var remainder: ICefStreamReader);
begin

end;

procedure TCefContentFilterOwn.ProcessData(const Data: Pointer; Size: Integer;
  var SubstituteData: ICefStreamReader);
begin

end;

{ TCefRequestHandlerOwn }

constructor TCefRequestHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefRequestHandler));
  with PCefRequestHandler(FData)^ do
  begin
    on_before_browse := @cef_request_handler_on_before_browse;
    on_before_resource_load := @cef_request_handler_on_before_resource_load;
    on_resource_response := @cef_request_handler_on_resource_response;
    on_protocol_execution := @cef_request_handler_on_protocol_execution;
    get_download_handler := @cef_request_handler_get_download_handler;
    get_auth_credentials := @cef_request_handler_get_auth_credentials;
  end;

end;

function TCefRequestHandlerOwn.GetAuthCredentials(const browser: ICefBrowser;
  isProxy: Boolean; Port: Integer; const host, realm, scheme: ustring; var username,
  password: ustring): Boolean;
begin
  Result := False;
end;

function TCefRequestHandlerOwn.GetDownloadHandler(const browser: ICefBrowser;
  const mimeType, fileName: ustring; contentLength: int64;
  var handler: ICefDownloadHandler): Boolean;
begin
  Result := False;
end;

function TCefRequestHandlerOwn.OnBeforeBrowse(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  navType: TCefHandlerNavtype; isRedirect: Boolean): Boolean;
begin
  Result := False;
end;

function TCefRequestHandlerOwn.OnBeforeResourceLoad(const browser: ICefBrowser;
  const request: ICefRequest; var redirectUrl: ustring;
  var resourceStream: ICefStreamReader; const response: ICefResponse;
  loadFlags: Integer): Boolean;
begin
  Result := False;
end;

function TCefRequestHandlerOwn.OnProtocolExecution(const browser: ICefBrowser;
  const url: ustring; var allowOSExecution: Boolean): Boolean;
begin
  Result := False;
end;

procedure TCefRequestHandlerOwn.OnResourceResponse(const browser: ICefBrowser;
  const url: ustring; const response: ICefResponse;
  var filter: ICefBase);
begin

end;

{ TCefDisplayHandlerOwn }

constructor TCefDisplayHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDisplayHandler));
  with PCefDisplayHandler(FData)^ do
  begin
    on_nav_state_change := @cef_display_handler_on_nav_state_change;
    on_address_change := @cef_display_handler_on_address_change;
    on_title_change := @cef_display_handler_on_title_change;
    on_tooltip := @cef_display_handler_on_tooltip;
    on_status_message := @cef_display_handler_on_status_message;
    on_console_message := @cef_display_handler_on_console_message;
  end;
end;

procedure TCefDisplayHandlerOwn.OnAddressChange(const browser: ICefBrowser;
  const frame: ICefFrame; const url: ustring);
begin

end;

function TCefDisplayHandlerOwn.OnConsoleMessage(const browser: ICefBrowser;
  const message, source: ustring; line: Integer): Boolean;
begin
  Result := False;
end;

procedure TCefDisplayHandlerOwn.OnNavStateChange(const browser: ICefBrowser;
  canGoBack, canGoForward: Boolean);
begin

end;

procedure TCefDisplayHandlerOwn.OnStatusMessage(const browser: ICefBrowser;
  const value: ustring; kind: TCefHandlerStatusType);
begin

end;

procedure TCefDisplayHandlerOwn.OnTitleChange(const browser: ICefBrowser;
  const title: ustring);
begin

end;

function TCefDisplayHandlerOwn.OnTooltip(const browser: ICefBrowser;
  var text: ustring): Boolean;
begin
  Result := False;
end;

{ TCefFocusHandlerOwn }

constructor TCefFocusHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefFocusHandler));
  with PCefFocusHandler(FData)^ do
  begin
    on_take_focus := @cef_focus_handler_on_take_focus;
    on_set_focus := @cef_focus_handler_on_set_focus;
  end;
end;

function TCefFocusHandlerOwn.OnSetFocus(const browser: ICefBrowser;
  source: TCefHandlerFocusSource): Boolean;
begin
  Result := False;
end;

procedure TCefFocusHandlerOwn.OnTakeFocus(const browser: ICefBrowser;
  next: Boolean);
begin

end;

{ TCefKeyboardHandlerOwn }

constructor TCefKeyboardHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefKeyboardHandler));
  with PCefKeyboardHandler(FData)^ do
    on_key_event := @cef_keyboard_handler_on_key_event;
end;

function TCefKeyboardHandlerOwn.OnKeyEvent(const browser: ICefBrowser;
  event: TCefHandlerKeyEventType; code, modifiers: Integer;
  isSystemKey: Boolean): Boolean;
begin
  Result := False;
end;

{ TCefMenuHandlerOwn }

constructor TCefMenuHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefMenuHandler));
  with PCefMenuHandler(FData)^ do
  begin
    on_before_menu := @cef_menu_handler_on_before_menu;
    get_menu_label := @cef_menu_handler_get_menu_label;
    on_menu_action := @cef_menu_handler_on_menu_action;
  end;
end;

procedure TCefMenuHandlerOwn.GetMenuLabel(const browser: ICefBrowser;
  menuId: TCefHandlerMenuId; var caption: ustring);
begin

end;

function TCefMenuHandlerOwn.OnBeforeMenu(const browser: ICefBrowser;
  const menuInfo: PCefHandlerMenuInfo): Boolean;
begin
  Result := False;
end;

function TCefMenuHandlerOwn.OnMenuAction(const browser: ICefBrowser;
  menuId: TCefHandlerMenuId): Boolean;
begin
  Result := False;
end;

{ TCefPrintHandlerOwn }

constructor TCefPrintHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefPrintHandler));
  with PCefPrintHandler(FData)^ do
  begin
    get_print_options := @cef_print_handler_get_print_options;
    get_print_header_footer := @cef_print_handler_get_print_header_footer;
  end;
end;

function TCefPrintHandlerOwn.GetPrintHeaderFooter(const browser: ICefBrowser;
  const frame: ICefFrame; const printInfo: PCefPrintInfo; const url,
  title: ustring; currentPage, maxPages: Integer; var topLeft, topCenter,
  topRight, bottomLeft, bottomCenter, bottomRight: ustring): Boolean;
begin
  Result := False;
end;

function TCefPrintHandlerOwn.GetPrintOptions(const browser: ICefBrowser;
  printOptions: PCefPrintOptions): Boolean;
begin
  Result := False;
end;

{ TCefFindHandlerOwn }

constructor TCefFindHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefFindHandler));
  with PCefFindHandler(FData)^ do
    on_find_result := @cef_find_handler_on_find_result;
end;

procedure TCefFindHandlerOwn.OnFindResult(const browser: ICefBrowser;
  count: Integer; const selectionRect: PCefRect; identifier,
  activeMatchOrdinal, finalUpdate: Boolean);
begin

end;

{ TCefJsDialogHandlerOwn }

constructor TCefJsDialogHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefJsDialogHandler));
  with PCefJsDialogHandler(FData)^ do
  begin
    on_jsalert := @cef_jsdialog_handler_on_jsalert;
    on_jsconfirm := @cef_jsdialog_handler_on_jsconfirm;
    on_jsprompt := @cef_jsdialog_handler_on_jsprompt;
  end;
end;

function TCefJsDialogHandlerOwn.OnJsAlert(const browser: ICefBrowser;
  const frame: ICefFrame; const message: ustring): Boolean;
begin
  Result := False;
end;

function TCefJsDialogHandlerOwn.OnJsConfirm(const browser: ICefBrowser;
  const frame: ICefFrame; const message: ustring; var retval: Boolean): Boolean;
begin
  Result := False;
end;

function TCefJsDialogHandlerOwn.OnJsPrompt(const browser: ICefBrowser;
  const frame: ICefFrame; const message, defaultValue: ustring;
  var retval: Boolean; var return: ustring): Boolean;
begin
  Result := False;
end;

{ TCefJsBindingHandlerOwn }

constructor TCefJsBindingHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefJsBindingHandler));
  with PCefJsBindingHandler(FData)^ do
    on_jsbinding := @cef_jsbinding_handler_on_jsbinding;
end;

procedure TCefJsBindingHandlerOwn.OnJsBinding(const browser: ICefBrowser;
  const frame: ICefFrame; const obj: ICefv8Value);
begin

end;

{ TCefRenderHandlerOwn }

constructor TCefRenderHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefRenderHandler));
  with PCefRenderHandler(FData)^ do
  begin
    get_view_rect := @cef_render_handler_get_view_rect;
    get_screen_rect := @cef_render_handler_get_screen_rect;
    get_screen_point := @cef_render_handler_get_screen_point;
    on_popup_show := @cef_render_handler_on_popup_show;
    on_popup_size := @cef_render_handler_on_popup_size;
    on_paint := @cef_render_handler_on_paint;
    on_cursor_change := @cef_render_handler_on_cursor_change;
  end;
end;

function TCefRenderHandlerOwn.GetScreenPoint(const browser: ICefBrowser; viewX,
  viewY: Integer; screenX, screenY: PInteger): Boolean;
begin
  Result := False;
end;

function TCefRenderHandlerOwn.GetScreenRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := False;
end;

function TCefRenderHandlerOwn.GetViewRect(const browser: ICefBrowser;
  rect: PCefRect): Boolean;
begin
  Result := False;
end;

procedure TCefRenderHandlerOwn.OnCursorChange(const browser: ICefBrowser;
  cursor: TCefCursorHandle);
begin

end;

procedure TCefRenderHandlerOwn.OnPaint(const browser: ICefBrowser;
  kind: TCefPaintElementType; const dirtyRect: PCefRect; const buffer: Pointer);
begin

end;

procedure TCefRenderHandlerOwn.OnPopupShow(const browser: ICefBrowser;
  show: Boolean);
begin

end;

procedure TCefRenderHandlerOwn.OnPopupSize(const browser: ICefBrowser;
  const rect: PCefRect);
begin

end;

{ TCefDragHandlerOwn }

constructor TCefDragHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDragHandler));
  with PCefDragHandler(FData)^ do
  begin
    on_drag_start := @cef_drag_handler_on_drag_start;
    on_drag_enter := @cef_drag_handler_on_drag_enter;
  end;

end;

function TCefDragHandlerOwn.OnDragEnter(const browser: ICefBrowser;
  const dragData: ICefDragData; mask: TCefDragOperations): Boolean;
begin
  Result := False;
end;

function TCefDragHandlerOwn.OnDragStart(const browser: ICefBrowser;
  const dragData: ICefDragData; mask: TCefDragOperations): Boolean;
begin
  Result := False;
end;

{ TCefSchemeHandlerCallbackRef }

procedure TCefSchemeHandlerCallbackRef.BytesAvailable;
begin
  PCefSchemeHandlerCallback(FData).bytes_available(FData);
end;

procedure TCefSchemeHandlerCallbackRef.Cancel;
begin
  PCefSchemeHandlerCallback(FData).cancel(FData);
end;

procedure TCefSchemeHandlerCallbackRef.HeadersAvailable;
begin
  PCefSchemeHandlerCallback(FData).headers_available(FData);
end;

class function TCefSchemeHandlerCallbackRef.UnWrap(
  data: Pointer): ICefSchemeHandlerCallback;
begin
  if data <> nil then
    Result := Create(data) as ICefSchemeHandlerCallback else
    Result := nil;
end;

{ TCefDragDataRef }

function TCefDragDataRef.GetFileExtension: string;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData)^.get_file_extension(FData));
end;

function TCefDragDataRef.GetFileName: string;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData)^.get_file_name(FData));
end;

function TCefDragDataRef.GetFileNames(names: TStrings): Boolean;
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    Result := PCefDragData(FData)^.get_file_names(FData, list) <> 0;
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      cef_string_list_value(list, i, @str);
      names.Add(CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefDragDataRef.GetFragmentBaseUrl: string;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData)^.get_fragment_base_url(FData));
end;

function TCefDragDataRef.GetFragmentHtml: string;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData)^.get_fragment_html(FData));
end;

function TCefDragDataRef.GetFragmentText: string;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData)^.get_fragment_text(FData));
end;

function TCefDragDataRef.GetLinkMetadata: string;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData)^.get_link_metadata(FData));
end;

function TCefDragDataRef.GetLinkTitle: string;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData)^.get_link_title(FData));
end;

function TCefDragDataRef.GetLinkUrl: string;
begin
  Result := CefStringFreeAndGet(PCefDragData(FData)^.get_link_url(FData));
end;

function TCefDragDataRef.IsFile: Boolean;
begin
  Result := PCefDragData(FData)^.is_file(FData) <> 0;
end;

function TCefDragDataRef.IsFragment: Boolean;
begin
  Result := PCefDragData(FData)^.is_fragment(FData) <> 0;
end;

function TCefDragDataRef.IsLink: Boolean;
begin
  Result := PCefDragData(FData)^.is_link(FData) <> 0;
end;

class function TCefDragDataRef.UnWrap(data: Pointer): ICefDragData;
begin
  if data <> nil then
    Result := Create(data) as ICefDragData else
    Result := nil;
end;

initialization
  IsMultiThread := True;

finalization
  if LibHandle <> 0 then
  begin
    cef_shutdown;
    FreeLibrary(LibHandle);
  end;

end.

