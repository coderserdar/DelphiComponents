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

unit UrlMon2;

interface
uses Windows, ActiveX, URLMon;

const
  PI_PARSE_URL	        = $1;
  PI_FILTER_MODE	= $2;
  PI_FORCE_ASYNC	= $4;
  PI_USE_WORKERTHREAD	= $8;
  PI_MIMEVERIFICATION	= $10;
  PI_CLSIDLOOKUP	= $20;
  PI_DATAPROGRESS	= $40;
  PI_SYNCHRONOUS	= $80;
  PI_APARTMENTTHREADED	= $100;
  PI_CLASSINSTALL	= $200;
  PD_FORCE_SWITCH	= $10000;

const
   BINDSTATUS_BEGINSYNCOPERATION = 15;
   BINDSTATUS_ENDSYNCOPERATION   = 16;
   BINDSTATUS_BEGINUPLOADDATA = 17;
   BINDSTATUS_UPLOADINGDATA = 18;
   BINDSTATUS_ENDUPLOADINGDATA = 19;
   BINDSTATUS_PROTOCOLCLASSID = 20;
   BINDSTATUS_ENCODING = 21;
   BINDSTATUS_VERFIEDMIMETYPEAVAILABLE = 22;
   BINDSTATUS_CLASSINSTALLLOCATION = 23;
   BINDSTATUS_DECODING = 24;
   BINDSTATUS_LOADINGMIMEHANDLER = 25;
   BINDSTATUS_CONTENTDISPOSITIONATTACH = 26;
   BINDSTATUS_FILTERREPORTMIMETYPE = 27;
   BINDSTATUS_CLSIDCANINSTANTIATE = 28;
   BINDSTATUS_IUNKNOWNAVAILABLE = 29;
   BINDSTATUS_DIRECTBIND = 30;
   BINDSTATUS_RAWMIMETYPE = 31;
   BINDSTATUS_PROXYDETECTING = 32;
   BINDSTATUS_ACCEPTRANGES = 33;

const
  INET_E_USE_DEFAULT_PROTOCOLHANDLER = HRESULT($800C0011);
  INET_E_USE_DEFAULT_SETTING         = HRESULT($800C0012);
  INET_E_DEFAULT_ACTION              = INET_E_USE_DEFAULT_PROTOCOLHANDLER;
  INET_E_QUERYOPTION_UNKNOWN         = HRESULT($800C0013);
  INET_E_REDIRECTING                 = HRESULT($800C0014);

const
  BINDSTRING_HEADERS	         = 1;
  BINDSTRING_ACCEPT_MIMES	 = BINDSTRING_HEADERS + 1;
  BINDSTRING_EXTRA_URL	         = BINDSTRING_ACCEPT_MIMES + 1;
  BINDSTRING_LANGUAGE	         = BINDSTRING_EXTRA_URL + 1;
  BINDSTRING_USERNAME	         = BINDSTRING_LANGUAGE + 1;
  BINDSTRING_PASSWORD	         = BINDSTRING_USERNAME + 1;
  BINDSTRING_UA_PIXELS	         = BINDSTRING_PASSWORD + 1;
  BINDSTRING_UA_COLOR	         = BINDSTRING_UA_PIXELS + 1;
  BINDSTRING_OS	                 = BINDSTRING_UA_COLOR + 1;
  BINDSTRING_USER_AGENT	         = BINDSTRING_OS + 1;
  BINDSTRING_ACCEPT_ENCODINGS	 = BINDSTRING_USER_AGENT + 1;
  BINDSTRING_POST_COOKIE	 = BINDSTRING_ACCEPT_ENCODINGS + 1;
  BINDSTRING_POST_DATA_MIME	 = BINDSTRING_POST_COOKIE + 1;
  BINDSTRING_URL	         = BINDSTRING_POST_DATA_MIME + 1;
  BINDSTRING_IID                 = BINDSTRING_URL + 1;
  BINDSTRING_FLAG_BIND_TO_OBJECT = BINDSTRING_IID + 1;
  BINDSTRING_PTR_BIND_CONTEXT    = BINDSTRING_FLAG_BIND_TO_OBJECT + 1;


const
    BINDF_ASYNCHRONOUS             = $00000001;
    BINDF_ASYNCSTORAGE             = $00000002;
    BINDF_NOPROGRESSIVERENDERING   = $00000004;
    BINDF_OFFLINEOPERATION         = $00000008;
    BINDF_GETNEWESTVERSION         = $00000010;
    BINDF_NOWRITECACHE             = $00000020;
    BINDF_NEEDFILE                 = $00000040;
    BINDF_PULLDATA                 = $00000080;
    BINDF_IGNORESECURITYPROBLEM    = $00000100;
    BINDF_RESYNCHRONIZE            = $00000200;
    BINDF_HYPERLINK                = $00000400;
    BINDF_NO_UI                    = $00000800;
    BINDF_SILENTOPERATION          = $00001000;
    BINDF_PRAGMA_NO_CACHE          = $00002000;
    BINDF_FREE_THREADED            = $00010000;
    BINDF_DIRECT_READ              = $00020000;
    BINDF_FORMS_SUBMIT             = $00040000;
    BINDF_GETFROMCACHE_IF_NET_FAIL = $00080000;

const
    BSCF_INTERMEDIATEDATANOTIFICATION	= $2;
    BSCF_LASTDATANOTIFICATION	        = $4;
    BSCF_DATAFULLYAVAILABLE	        = $8;
    BSCF_AVAILABLEDATASIZEUNKNOWN	= $10;

const
   PARSE_CANONICALIZE	  = 1;
   PARSE_FRIENDLY	  = PARSE_CANONICALIZE + 1;
   PARSE_SECURITY_URL	  = PARSE_FRIENDLY + 1;
   PARSE_ROOTDOCUMENT	  = PARSE_SECURITY_URL + 1;
   PARSE_DOCUMENT	  = PARSE_ROOTDOCUMENT + 1;
   PARSE_ANCHOR	          = PARSE_DOCUMENT + 1;
   PARSE_ENCODE	          = PARSE_ANCHOR + 1;
   PARSE_DECODE	          = PARSE_ENCODE + 1;
   PARSE_PATH_FROM_URL	  = PARSE_DECODE + 1;
   PARSE_URL_FROM_PATH	  = PARSE_PATH_FROM_URL + 1;
   PARSE_MIME	          = PARSE_URL_FROM_PATH + 1;
   PARSE_SERVER	          = PARSE_MIME + 1;
   PARSE_SCHEMA	          = PARSE_SERVER + 1;
   PARSE_SITE	          = PARSE_SCHEMA + 1;
   PARSE_DOMAIN	          = PARSE_SITE + 1;
   PARSE_LOCATION	  = PARSE_DOMAIN + 1;
   PARSE_SECURITY_DOMAIN  = PARSE_LOCATION + 1;

const
   PSU_DEFAULT	          = 1;
   PSU_SECURITY_URL_ONLY  = PSU_DEFAULT + 1;

const
   QUERY_EXPIRATION_DATE     = 1;
   QUERY_TIME_OF_LAST_CHANGE = QUERY_EXPIRATION_DATE + 1;
   QUERY_CONTENT_ENCODING    = QUERY_TIME_OF_LAST_CHANGE + 1;
   QUERY_CONTENT_TYPE	     = QUERY_CONTENT_ENCODING + 1;
   QUERY_REFRESH	     = QUERY_CONTENT_TYPE + 1;
   QUERY_RECOMBINE	     = QUERY_REFRESH + 1;
   QUERY_CAN_NAVIGATE	     = QUERY_RECOMBINE + 1;
   QUERY_USES_NETWORK	     = QUERY_CAN_NAVIGATE + 1;
   QUERY_IS_CACHED	     = QUERY_USES_NETWORK + 1;
   QUERY_IS_INSTALLEDENTRY   = QUERY_IS_CACHED + 1;
   QUERY_IS_CACHED_OR_MAPPED = QUERY_IS_INSTALLEDENTRY + 1;
   QUERY_USES_CACHE	     = QUERY_IS_CACHED_OR_MAPPED + 1;

type
  IInternetBindInfo = interface;
  IInternetProtocolSink = interface;
  IInternetProtocolRoot = interface;
  IInternetProtocol = interface;
  IInternetSession = interface;
  IInternetProtocolInfo = interface;

  PProtocolData = ^TProtocolData;
  TProtocolData = packed record
    grfFlags: DWord;
    dwState: DWord;
    pData: Pointer;
    cbData: LongInt;
  end;

  PProtocolFilterData = ^TProtocolFilterData;
  TProtocolFilterData = packed record
    cbSize: DWord;
    pProtocolSink: IInternetProtocolSink;
    pProtocol: IInternetProtocol;
    pUnk: IUnknown;
    dwFilterFlags: DWord;
  end;

  IInternetBindInfo = interface(IUnknown)
    ['{79EAC9E1-BAF9-11CE-8C82-00AA004BA90B}']
    function GetBindInfo(var grfBINDF: DWord;
       var pbindinfo: TBindInfo): HResult; stdcall;
    function GetBindString(ulStringType: LongInt; ppwzStr: Pointer;
       cEl: LongInt; pcElFetched: PLongInt): HResult; stdcall;
  end;

  IInternetProtocolSink = interface(IUnknown)
    ['{79EAC9E5-BAF9-11CE-8C82-00AA004BA90B}']
    function Switch(pProtocolData: PProtocolData): HResult; stdcall;
    function ReportProgress(ulStatusCode: LongInt;
       szStatusText: POleStr): HResult; stdcall;
    function ReportData(grfBSCF: DWord; ulProgress: LongInt;
       ulProgressMax: LongInt): HResult; stdcall;
    function ReportResult(hrResult: HResult; dwError: DWord;
       szResult: POleStr): HResult; stdcall;
  end;

  IInternetProtocolRoot = interface(IUnknown)
    ['{79EAC9E3-BAF9-11CE-8C82-00AA004BA90B}']
    function Start(pwszUrl: POleStr; pOIProtSink: IInternetProtocolSink;
       pOIBindInfo: IInternetBindInfo; grfPI: DWord; dwReserved: DWord): HResult; stdcall;
    function Continue(pProtocolData: PProtocolData): HResult; stdcall;
    function Abort(hrReason: HResult; dwOptions: DWord): HResult; stdcall;
    function Terminate(dwOptions: DWord): HResult; stdcall;
    function Suspend: HResult; stdcall;
    function Resume: HResult; stdcall;
  end;

  IInternetProtocol = interface(IInternetProtocolRoot)
    ['{79EAC9E4-BAF9-11CE-8C82-00AA004BA90B}']
    function Read(pv: Pointer; cb: LongInt;
        pcbRead: PLongInt): HResult; stdcall;
    function Seek(dlibMove: Largeint; dwOrigin: DWord;
        out plibNewPosition: LargeInt): HResult; stdcall;
    function LockRequest(dwOptions: DWord): HResult; stdcall;
    function UnlockRequest: HResult; stdcall;
  end;

  IInternetProtocolInfo = interface(IUnknown)
    ['{79EAC9EC-BAF9-11CE-8C82-00AA004BA90B}']
    function ParseUrl(pwszUrl: POleStr; ParseAction: Smallint;
             dwParseFlags: DWord; pwszResult: POleStr;  cchResult: DWord;
             pcchResult: LPDWord; Reserved: DWord): HResult; stdcall;
    function CombineUrl(pwzBaseUrl, pwzRelativeUrl: POleStr;
             dwCombineFlags: DWord; pwzResult: POleStr; cchResult: DWord;
             pcchResult: LPDWord;  Reserved: DWord): HResult; stdcall;
    function CompareUrl(pwzUrl1, pwzUrl2: POleStr;
             dwCompareFlags: DWord): HResult; stdcall;
    function QueryInfo(pwzUrl: POleStr; OueryOption: Smallint;
             dwQueryFlags: DWord; pBuffer: Pointer; cbBuffer: DWord;
             pcbBuffer: LPDWord; Reserved: DWord): HResult; stdcall;
  end;

  IInternetSession = interface(IUnknown)
    ['{79EAC9E7-BAF9-11CE-8C82-00AA004BA90B}']
    function RegisterNameSpace(pCF: IClassFactory; const rclsid: TGUID;
        pwzProtocol: POleStr; cPatterns: LongInt; ppwzPatterns: Pointer;
        dwReserved: DWord): HResult; stdcall;
    function UnregisterNameSpace(pCF: IClassFactory;
        pwzProtocol: POleStr): HResult; stdcall;
    function RegisterMimeFilter(pCF: IClassFactory; const rclsid: TGUID;
        pwzType: POleStr): HResult;  stdcall;
    function UnregisterMimeFilter(pCF: IClassFactory;
        pwzType: POleStr): HResult; stdcall;
    function CreateBinding(BindCtx: IBindCtx; pszUrl: POleStr;
        pUnkOuter: IUnknown; out ppUnk: IUnknown; out ppOInetProt: IInternetProtocol;
        dwOption: DWord): HResult; stdcall;
    function SetSessionOption(dwOption: DWord; pBuffer: Pointer;
        dwBufferLength: DWord; dwReserved: DWord): HResult; stdcall;
    function GetSessionOption(dwOption: DWord; pBuffer: Pointer;
        out dwBufferLength: DWord; dwReserved: DWord): HResult; stdcall;
  end;

function CoInternetParseUrl(pwzUrl: POleStr; ParseAction: Smallint;
    dwFlags: DWord; pszResult: Pointer; cchResult: DWord;
    var pcchResult: DWord; dwReserved: DWord): HResult; stdcall;

function CoInternetCombineUrl(pwzBaseUrl: POleStr;
    pwzRelativeUrl: POleStr; dwCombineFlags: DWord;
    pszResult: Pointer; cchResult: DWord; var pcchResult: DWord;
    dwReserved: DWord): HResult; stdcall;

function CoInternetCompareUrl(pwzUrl1: POleStr; pwzUrl2: POleStr;
    dwFlags: DWord): HResult; stdcall;

function CoInternetGetProtocolFlags(pwzUrl: POleStr;
    var pdwFlags: DWord; dwReserved: DWord): HResult; stdcall;

function CoInternetQueryInfo(pwzUrl: POleStr; QueryOptions: Smallint;
    dwQueryFlags: DWord; pvBuffer: Pointer; cbBuffer: DWord;
    var pcbBuffer: DWord; dwReserved: DWord): HResult; stdcall;

function CoInternetGetSession(dwSessionMode: DWord;
    out ppIInternetSession: IInternetSession; dwReserved: DWord): HResult; stdcall;


implementation

const
  UrlMonLib = 'URLMON.DLL';

function CoInternetParseUrl;                  external UrlMonLib;
function CoInternetCombineUrl;                external UrlMonLib;
function CoInternetCompareUrl;                external UrlMonLib;
function CoInternetGetProtocolFlags;          external UrlMonLib;
function CoInternetQueryInfo;                 external UrlMonLib;
function CoInternetGetSession;                external UrlMonLib;

end.
