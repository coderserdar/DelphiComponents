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

unit WebProtocol;

interface

uses
  Windows, ActiveX, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  URLMon, URLMon2, ComObj, HTTPApp, WinInet, AxCtrls, MSXML2, Masks;

type
  TRequestObject = class;
  TResponseObject = class;
  TWebProvider = class;
  TWebDispatcher = class;
  ETransform = class(Exception);
  TMethodType = (mtAny, mtGet, mtPut, mtPost, mtHead);
  TDispatchEvent = procedure (Sender: TObject; Request: TRequestObject;
     Response: TResponseObject; var Handled: Boolean) of object;
  TXMLParserLevel = (MSXML20, MSXML30);

  TRequestObject = class
  private
    FURL: String;
    FMethodType: TMethodType;
    FLocation: String;
    FProtocolInfo: String;
    FPathInfo: String;
    FParams: TStrings;
    FParamText: String;
    FBindInfo: IInternetBindInfo;
  public
    constructor Create(const URL: String; BindInfo: IInternetBindInfo);
    destructor Destroy; override;
    property URL: String read FURL;
    property MethodType: TMethodType read FMethodType;
    property Location: String read FLocation;
    property PathInfo: String read FPathInfo;
    property Params: TStrings read FParams;
    property ParamText: String read FParamText;
  end;

  TResponseObject = class
    FStream: IStream;
    FProtSink: IInternetProtocolSink;
    FProcessor: IXSLProcessor;
    FDocument: IXMLDOMDocument2;
    FMimeType: String;
    FStatus: HResult;
    procedure SetContent(Value: String);
    function attr(const attr: String; const value: String): String;
    function createTag(const name: String;
              const attr1: String = ''; const value1: String = '';
              const attr2: String = ''; const value2: String = '';
              const attr3: String = ''; const value3: String = '';
              const attr4: String = ''; const value4: String = '';
              const attr5: String = ''; const value5: String = '';
              const attr6: String = ''; const value6: String = '';
              const attr7: String = ''; const value7: String = '';
              const attr8: String = ''; const value8: String = '';
              const attr9: String = ''; const value9: String = ''): String;
    function VarToStrStd(const V: Variant): string;
    function GetDocument: IXMLDOMDocument2;
  public
    constructor Create(ProtSink: IInternetProtocolSink; Stream: IStream);
    procedure Reset;
    function GetStream: TStream;
    function EncodeText(const Text: String): String;
    procedure WriteStream(Source: TStream);
    procedure WriteFile(const FileName: String);
    procedure WriteText(AStr: String);
    procedure WriteTextFmt(AFmtStr: String; const Args: array of const);
    procedure WriteTextLn(AStr: String);
    procedure WriteTextFmtLn(AFmtStr: String; const Args: array of const);
    procedure head(const encoding: String);
    procedure doctype(const name: String; const DTD: String);
    procedure CDATA;
    procedure endCDATA;
    procedure createRemark;
    procedure endRemark;
    procedure Remark(const text: String);
    procedure createElement(const name: String;
              const attr1: String = ''; const value1: String = '';
              const attr2: String = ''; const value2: String = '';
              const attr3: String = ''; const value3: String = '';
              const attr4: String = ''; const value4: String = '';
              const attr5: String = ''; const value5: String = '';
              const attr6: String = ''; const value6: String = '';
              const attr7: String = ''; const value7: String = '';
              const attr8: String = ''; const value8: String = '';
              const attr9: String = ''; const value9: String = '');
    procedure endElement(const name: String);
    procedure EmptyElement(const name: String;
              const attr1: String = ''; const value1: String = '';
              const attr2: String = ''; const value2: String = '';
              const attr3: String = ''; const value3: String = '';
              const attr4: String = ''; const value4: String = '';
              const attr5: String = ''; const value5: String = '';
              const attr6: String = ''; const value6: String = '';
              const attr7: String = ''; const value7: String = '';
              const attr8: String = ''; const value8: String = '';
              const attr9: String = ''; const value9: String = '');
    procedure Element(const name: String; Value: Variant;
              const attr1: String = ''; const value1: String = '';
              const attr2: String = ''; const value2: String = '';
              const attr3: String = ''; const value3: String = '';
              const attr4: String = ''; const value4: String = '';
              const attr5: String = ''; const value5: String = '';
              const attr6: String = ''; const value6: String = '';
              const attr7: String = ''; const value7: String = '';
              const attr8: String = ''; const value8: String = '';
              const attr9: String = ''; const value9: String = '');
    procedure SetParam(const ParamName: String; ParamValue: OleVariant;
      namespaceURI: String = '');
    procedure Redirect(Url: WideString);
    procedure UseDefaultHandler;
    property Document: IXMLDOMDocument2 read GetDocument;
    property Content: String write SetContent;
    property Stream: IStream read FStream;
    property MimeType: String read FMimeType write FMimeType;
  end;

  TWebDispatcher = class(TCollectionItem)
  private
    FXSLT: Boolean;
    FName: String;
    FEnabled: Boolean;
    FDefault: Boolean;
    FCacheTemplates: Boolean;
    FTemplate: String;
    FLocation: String;
    FOnAction: TDispatchEvent;
    FMask: TMask;
    FMethodType: TMethodType;
    FProcessor: IXSLProcessor;
    FMimeType: String;
    procedure SetDefault(Value: Boolean);
    procedure SetXSLT(Value: Boolean);
    procedure SetLocation(Value: String);
    procedure SetEnabled(Value: Boolean);
    procedure SetAction(Value: TDispatchEvent);
    procedure SetMethodType(Value: TMethodType);
    function CreateProcessor(const TemplateName: WideString): IXSLProcessor;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
    function DispatchItem(Request: TRequestObject; Response: TResponseObject;
      DoDefault: Boolean): Boolean; virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function GetNamePath: String; override;
    procedure AssignTo(Dest: TPersistent); override;
    function GetProvider: TWebProvider;
  published
    property XSLT: Boolean read FXSLT write SetXSLT;
    property CacheTemplates: Boolean read FCacheTemplates write FCacheTemplates;
    property MimeType: String read FMimeType write FMimeType;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Default: Boolean read FDefault write SetDefault;
    property Template: String read FTemplate write FTemplate;
    property Location: String read FLocation write SetLocation;
    property Name: string read GetDisplayName write SetDisplayName;
    property MethodType: TMethodType read FMethodType write SetMethodType default mtAny;
    property OnAction: TDispatchEvent read FOnAction write SetAction;
  end;

  TWebDispatchers = class(TCollection)
  private
    FWebProvider: TWebProvider;
    function GetActionItem(Index: Integer): TWebDispatcher;
    procedure SetActionItem(Index: Integer; Value: TWebDispatcher);
  protected
    function GetAttrCount: Integer; override;
    function GetAttr(Index: Integer): string; override;
    function GetItemAttr(Index, ItemIndex: Integer): string; override;
    function GetOwner: TPersistent; override;
    procedure SetItemName(Item: TCollectionItem); override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(WebProvider: TWebProvider;
      ItemClass: TCollectionItemClass);
    function  Add: TWebDispatcher;
    property WebProvider: TWebProvider read FWebProvider;
    property Items[Index: Integer]: TWebDispatcher read GetActionItem
      write SetActionItem; default;
  end;

  TWebProvider = class(TComponent)
  private
    FParserLevel: TXMLParserLevel;
    FActive: Boolean;
    FWindowHandle: HWnd;
    FFactory: IClassFactory;
    FRefreshDelay: Integer;
    FBasePath: String;
    FProtocol: String;
    FCached: Boolean;
    FBeforeDispatch: TDispatchEvent;
    FAfterDispatch: TDispatchEvent;
    FItems: TWebDispatchers;
    FRequest: TRequestObject;
    FResponse: TResponseObject;
    FDispObject: IDispatch;
    procedure SetBasePath(Value: String);
    procedure SetItems(Value: TWebDispatchers);
    procedure SetActive(Value: Boolean);
    procedure WndProc(var Msg: TMessage);
    procedure InvokeURL(const URL: String; BindInfo: IInternetBindInfo;
      ProtSink: IInternetProtocolSink; Stream: IStream;
      var Status: HResult; var MimeType: String);
  protected
    function CreateProtocolFactory: IClassFactory; virtual;
    function DoAfterDispatch(Request: TRequestObject;
      Response: TResponseObject): Boolean; dynamic;
    function DoBeforeDispatch(Request: TRequestObject;
      Response: TResponseObject): Boolean; dynamic;
    function DispatchAction(Request: TRequestObject;
      Response: TResponseObject): Boolean; virtual;
    function GetErrorPage(const Msg: String): String; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterNameSpace;
    procedure UnRegisterNameSpace;
    property DispObject: IDispatch read FDispObject write FDispObject;
  published
    property XMLParserLevel: TXMLParserLevel read FParserLevel write FParserLevel;
    property Active: Boolean read FActive write SetActive;
    property Items: TWebDispatchers read FItems write SetItems;
    property RefreshDelay: Integer read FRefreshDelay write FRefreshDelay;
    property BasePath: String read FBasePath write SetBasePath;
    property Protocol: String read FProtocol write FProtocol;
    property Cached: Boolean read FCached write FCached default True;
    property BeforeDispatch: TDispatchEvent read FBeforeDispatch write FBeforeDispatch;
    property AfterDispatch: TDispatchEvent read FAfterDispatch write FAfterDispatch;
  end;

procedure ParseURL(const URL: String; var Proto, Handler, Location, Params: String);

const
  CLASS_OldDOMDocument: TGUID = '{2933BF90-7B36-11D2-B20E-00C04F983E60}';

implementation
uses Dialogs, pkgmsg;

const
  DummyGUID: TGUID = '{71FCABF1-57C0-11D4-B2D5-0050DA40F7CD}';

type
  PURLInfo = ^TURLInfo;
  TURLInfo = record
    Size: Cardinal;
    TargetURL: array [0..512] of WideChar;
    Stream: IStream;
    BindInfo: IInternetBindInfo;
    ProtSink: IInternetProtocolSink;
    Status: HResult;
    MimeType: array [0..32] of Char;
  end;

  TWebProtocol = class(TObject, IUnknown, IInternetProtocol, IInternetProtocolInfo)
  private
    FRefCount: Integer;
    FController: Pointer;
    FLockCount: Integer;
    FStream: IStream;
    FProtSink: IInternetProtocolSink;
    FWebProvider: TWebProvider;
    {$IFDEF DEBUG_OUTPUT}
    FURL: String;
    {$ENDIF}
    procedure WriteToCache(const URL: String; const MimeType: String);
    function ReadFromCache(const URL: String;
       LocalFileName: String): HResult;
    function ObjAddRef: Integer; stdcall;
    function ObjQueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function ObjRelease: Integer; stdcall;
  public
    constructor Create(WebProvider: TWebProvider; Controller: Pointer);
    destructor Destroy; override;
    // IUnknown
    function IUnknown.QueryInterface = ObjQueryInterface;
    function IUnknown._AddRef = ObjAddRef;
    function IUnknown._Release = ObjRelease;
    // IInternetProtocol
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Start(pwszUrl: POleStr; pOIProtSink: IInternetProtocolSink;
       pOIBindInfo: IInternetBindInfo; grfPI: DWord; dwReserved: DWord): HResult; stdcall;
    function Continue(pProtocolData: PProtocolData): HResult; stdcall;
    function Abort(hrReason: HResult; dwOptions: DWord): HResult; stdcall;
    function Terminate(dwOptions: DWord): HResult; stdcall;
    function Suspend: HResult; stdcall;
    function Resume: HResult; stdcall;
    function Read(pv: Pointer; cb: LongInt;
        pcbRead: PLongInt): HResult; stdcall;
    function Seek(dlibMove: Largeint; dwOrigin: DWord;
        out plibNewPosition: LargeInt): HResult; stdcall;
    function LockRequest(dwOptions: DWord): HResult; stdcall;
    function UnlockRequest: HResult; stdcall;
    // IInternetProtocolInfo
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

  TWebProtocolFactory = class(TObject, IUnknown, IClassFactory)
  private
    FRefCount: Integer;
    FWebProvider: TWebProvider;
  protected
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IClassFactory
    function CreateInstance(const UnkOuter: IUnknown; const IID: TGUID;
      out Obj): HResult; stdcall;
    function LockServer(fLock: BOOL): HResult; stdcall;
  public
    constructor Create(WebProvider: TWebProvider);
    destructor Destroy; override;
    property WebProvider: TWebProvider read FWebProvider;
  end;

// TWebProtocol
constructor TWebProtocol.Create(WebProvider: TWebProvider; Controller: Pointer);
begin
  {$IFDEF DEBUG_OUTPUT}
    OutputDebugString('TWebProtocol.Create');
  {$ENDIF}
  inherited Create;
  FWebProvider := WebProvider;
  FController := Controller;
end;

destructor TWebProtocol.Destroy;
begin
  {$IFDEF DEBUG_OUTPUT}
    OutputDebugString('TWebProtocol.Destroy');
  {$ENDIF}
  inherited;
end;

function TWebProtocol.ObjQueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := S_OK else Result := E_NOINTERFACE;
end;

function TWebProtocol.ObjAddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TWebProtocol.ObjRelease: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TWebProtocol.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if FController <> nil then
    Result := IUnknown(FController).QueryInterface(IID, Obj) else
    Result := ObjQueryInterface(IID, Obj);
end;

function TWebProtocol._AddRef: Integer;
begin
  if FController <> nil then
    Result := IUnknown(FController)._AddRef else
    Result := ObjAddRef;
end;

function TWebProtocol._Release: Integer;
begin
  if FController <> nil then
    Result := IUnknown(FController)._Release else
    Result := ObjRelease;
end;

function TWebProtocol.Start(pwszUrl: POleStr; pOIProtSink: IInternetProtocolSink;
    pOIBindInfo: IInternetBindInfo; grfPI: DWord; dwReserved: DWord): HResult;
var
  bindinfo: TBindInfo;
  grfBINDF: DWord;
  libSize, libNewPosition: Largeint;
  Status: HResult;
  Size: Cardinal;
  CacheEntryBuffer: array [0..SZ_BUFFER-1] of Char;
  CacheEntryInfo: TInternetCacheEntryInfo absolute CacheEntryBuffer;
  ReadCache: Boolean;
  LastModifiedTime: _SYSTEMTIME;
  LastModified: TDateTime;
  URLInfo: TURLInfo;
  MimeType: String;
begin
  FProtSink := pOIProtSink;
  bindinfo.cbSize := sizeof(TBindInfo);
  {$IFDEF DEBUG_OUTPUT}
    FURL := pwszURL;
    OutputDebugString(PChar(FURL + ' IInternetProtocol :: Start ' + IntToStr(grfBINDF)));
  {$ENDIF}
  try
    OleCheck(pOIBindInfo.GetBindInfo(grfBINDF, bindinfo));
    if bindinfo.dwBindVerb <> BINDVERB_GET then
      SysUtils.Abort;
    ReadCache := False;
    Status := S_OK;
    OleCheck(CreateStreamOnHGlobal(0, True, FStream));
    Size := sizeof(CacheEntryBuffer) - sizeof(CacheEntryInfo);
    CacheEntryInfo.dwStructSize := sizeof(CacheEntryInfo);
    if FWebProvider.FCached then // Cache enabled
      if not (Bool(grfBINDF and BINDF_RESYNCHRONIZE) or
              Bool(grfBINDF and BINDF_GETNEWESTVERSION)) and
              GetUrlCacheEntryInfo(PChar(String(pwszUrl)), CacheEntryInfo, Size) then
        begin // Trying to read contents from cache
          FileTimeToSystemTime(CacheEntryInfo.LastModifiedTime, LastModifiedTime);
          LastModified := Now - SystemTimeToDateTime(LastModifiedTime);
          if (LastModified < FWebProvider.FRefreshDelay / SecsPerDay) and (CacheEntryInfo.dwHeaderInfoSize > 0) then
            begin
              ReadCache := True;
              SetLength(MimeType, CacheEntryInfo.dwHeaderInfoSize);
              StrLCopy(PChar(MimeType), PChar(CacheEntryInfo.lpHeaderInfo),
                CacheEntryInfo.dwHeaderInfoSize);
            end;
        end;
    if not ReadCache then
      begin
        URLInfo.Stream := FStream;
        URLInfo.BindInfo := pOIBindInfo;
        URLInfo.ProtSink := pOIProtSink;
        StringToWideChar(pwszUrl, URLInfo.TargetURL, sizeof(URLInfo.TargetURL));
        // Main thread synchronization
        if SendMessage(FWebProvider.FWindowHandle, CM_INVOKE_URL, 0, LongInt(@URLInfo)) <> 1 then
          SysUtils.Abort;
        Status := URLInfo.Status;
        MimeType := URLInfo.MimeType;
      end;
    if Status = S_OK then
      begin
        pOIProtSink.ReportProgress(BINDSTATUS_MIMETYPEAVAILABLE, PWideChar(WideString(MimeType)));
        if ReadCache then
          ReadFromCache(pwszUrl, CacheEntryInfo.lpszLocalFileName)
        else
          if not Bool(grfBINDF and BINDF_NOWRITECACHE) then
             WriteToCache(pwszUrl, MimeType);
        FStream.Seek(0, STREAM_SEEK_END, libSize);
        FStream.Seek(0, STREAM_SEEK_SET, libNewPosition);
        FProtSink.ReportData(BSCF_FIRSTDATANOTIFICATION, 0, 0);
        FProtSink.ReportData(BSCF_LASTDATANOTIFICATION + BSCF_DATAFULLYAVAILABLE,
          libSize, libSize);
      end;
    if (Status = S_OK) or (Status = INET_E_INVALID_URL) then
      begin
        FProtSink.ReportResult(Status, 0, nil);
        Result := S_OK;
      end
    else
      Result := Status;
  except
    Result := E_FAIL;
  end;
end;

function CopyFrom(Source: IStream; Target: TStream): Longint;
const
  MaxBufSize = $F000;
var
  BufSize, N: Integer;
  Buffer: PChar;
  Count, libNewPosition: Largeint;
  Status: HResult;
begin
  Source.Seek(0, STREAM_SEEK_END, Count);
  Source.Seek(0, STREAM_SEEK_SET, libNewPosition);
  Result := Count;
  if Count > MaxBufSize then BufSize := MaxBufSize else BufSize := Count;
  if Count = 0 then
    Exit;
  GetMem(Buffer, BufSize);
  try
    while Count <> 0 do
    begin
      if Count > BufSize then N := BufSize else N := Count;
      Status := Source.Read(Buffer, N, nil);
      if Status <> S_FALSE then
        OleCheck(Status);
      Target.WriteBuffer(Buffer^, N);
      Dec(Count, N);
    end;
  finally
    FreeMem(Buffer, BufSize);
  end;
end;

procedure TWebProtocol.WriteToCache(const URL: String; const MimeType: String);
var
  FileName: array [0..2048] of Char;
  FileStream: TFileStream;
  ExpireTime, LastModifiedTime: TFileTime;
  SystemTime: _SYSTEMTIME;
  Stream: IStream;

begin
  if not CreateUrlCacheEntry(PChar(Url), 0, 'htm', FileName, 0) then
    RaiseLastWin32Error;
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    OleCheck(FStream.Clone(Stream));
    CopyFrom(Stream, FileStream);
  finally
    FileStream.Free;
  end;
  GetLocalTime(SystemTime);
  SystemTimeToFileTime(SystemTime, LastModifiedTime);
  SystemTimeToFileTime(SystemTime, ExpireTime);
  if not CommitUrlCacheEntry(PChar(Url), FileName, ExpireTime, LastModifiedTime,
    NORMAL_CACHE_ENTRY, PByte(MimeType), Length(MimeType), nil, 0) then
    RaiseLastWin32Error;
  // Very important !
  // Used by Microsoft Internet Controls, Win32 Internet Functions, and so on
  FProtSink.ReportProgress(BINDSTATUS_CACHEFILENAMEAVAILABLE,
    PWideChar(WideString(FileName)));
end;

function TWebProtocol.ReadFromCache(const URL: String; LocalFileName: String): HResult;
var
  FileStream: TFileStream;
  OleStream: TOleStream;
begin
  {$IFDEF DEBUG_OUTPUT}
  OutputDebugString(PChar(FURL + ' ReadFromCache [' + LocalFileName + ']'));
  {$ENDIF}
  FileStream := TFileStream.Create(LocalFileName, fmOpenRead);
  OleStream := TOleStream.Create(FStream);
  try
    OleStream.CopyFrom(FileStream, FileStream.Size);
  finally
    FileStream.Free;
    OleStream.Free;
  end;
  FProtSink.ReportProgress(BINDSTATUS_CACHEFILENAMEAVAILABLE,
    PWideChar(WideString(LocalFileName)));
  Result := S_OK;
end;

function TWebProtocol.Continue(pProtocolData: PProtocolData): HResult;
begin
  Result := E_NOTIMPL;
end;

function TWebProtocol.Abort(hrReason: HResult; dwOptions: DWord): HResult;
begin
  {$IFDEF DEBUG_OUTPUT}
    OutputDebugString(PChar(FURL + ' IInternetProtocol :: Abort'));
  {$ENDIF}
  FLockCount := 0;
  FStream := nil;
  FProtSink := nil;
  Result := S_OK;
end;

function TWebProtocol.Terminate(dwOptions: DWord): HResult;
begin
  {$IFDEF DEBUG_OUTPUT}
    OutputDebugString(PChar(FURL + ' IInternetProtocol :: Terminate'));
  {$ENDIF}
  if FLockCount = 0 then
    begin
      FStream := nil;
      FProtSink := nil;
    end;
  Result := S_OK;
end;

function TWebProtocol.Suspend: HResult;
begin
  Result := E_NOTIMPL;
end;

function TWebProtocol.Resume: HResult;
begin
  Result := E_NOTIMPL;
end;

function TWebProtocol.Read(pv: Pointer; cb: LongInt;
  pcbRead: PLongInt): HResult;
begin
  {$IFDEF DEBUG_OUTPUT}
    OutputDebugString(PChar(FURL + ' IInternetProtocol :: Read'));
  {$ENDIF}
  if (FStream <> nil) and (FStream.Read(pv, cb, pcbRead) = S_OK) then
    if ( pcbRead^ < cb ) or  ( pcbRead^ = 0 ) then
      Result := S_FALSE
    else Result := S_OK
  else Result := INET_E_DOWNLOAD_FAILURE;
end;

function TWebProtocol.Seek(dlibMove: Largeint; dwOrigin: DWord;
    out plibNewPosition: LargeInt): HResult;
begin
  Result := E_FAIL;
end;

function TWebProtocol.LockRequest(dwOptions: DWord): HResult;
begin
  {$IFDEF DEBUG_OUTPUT}
    OutputDebugString(PChar(FURL + ' IInternetProtocol :: LockRequest'));
  {$ENDIF}
  Inc(FLockCount);
  Result := S_OK;
end;

function TWebProtocol.UnlockRequest: HResult;
begin
  {$IFDEF DEBUG_OUTPUT}
    OutputDebugString(PChar(FURL + ' IInternetProtocol :: UnlockRequest'));
  {$ENDIF}
  Dec(FLockCount);
  if FLockCount = 0 then
    begin
      FStream := nil;
      FProtSink := nil;
    end;
  Result := S_OK;
end;

// We must implement support PARSE_SECURITY_URL and PARSE_SECURITY_DOMAIN
function TWebProtocol.ParseUrl(pwszUrl: POleStr; ParseAction: Smallint;
         dwParseFlags: DWord; pwszResult: POleStr;  cchResult: DWord;
         pcchResult: LPDWord; Reserved: DWord): HResult;
var
  Proto, Handler, Location, Params: String;
begin
  Result := S_OK;
  if ParseAction = PARSE_SECURITY_URL then
    begin
      WebProtocol.ParseURL(pwszUrl, Proto, Handler, Location, Params);
      StringToWideChar(Proto + '://' + Handler, pwszResult, cchResult);
      pcchResult^ := Length(pwszResult) + 1;
    end
  else if ParseAction = PARSE_SECURITY_DOMAIN then
    begin
      StringToWideChar(FWebProvider.FProtocol + '://', pwszResult, cchResult);
      pcchResult^ := Length(pwszResult) + 1;
    end
  else
    Result := E_NOTIMPL;
end;

// The implementation of the following methods are not meaningful,
// because URLMON will not use them for Pluggable Namespace Handler
function TWebProtocol.CombineUrl(pwzBaseUrl, pwzRelativeUrl: POleStr;
         dwCombineFlags: DWord; pwzResult: POleStr; cchResult: DWord;
         pcchResult: LPDWord;  Reserved: DWord): HResult;
begin
  Result := E_NOTIMPL;
end;

function TWebProtocol.CompareUrl(pwzUrl1, pwzUrl2: POleStr;
         dwCompareFlags: DWord): HResult;
begin
  Result := E_NOTIMPL;
end;

function TWebProtocol.QueryInfo(pwzUrl: POleStr; OueryOption: Smallint;
         dwQueryFlags: DWord; pBuffer: Pointer; cbBuffer: DWord;
         pcbBuffer: LPDWord; Reserved: DWord): HResult;
begin
  Result := E_NOTIMPL;
end;

// TWebProtocolFactory
constructor TWebProtocolFactory.Create(WebProvider: TWebProvider);
begin
  {$IFDEF DEBUG_OUTPUT}
    OutputDebugString('TWebProtocolFactory.Create');
  {$ENDIF}
  inherited Create;
  FWebProvider := WebProvider;
end;

destructor TWebProtocolFactory.Destroy;
begin
  {$IFDEF DEBUG_OUTPUT}
    OutputDebugString('TWebProtocolFactory.Destroy');
  {$ENDIF}
  inherited;
end;

function TWebProtocolFactory.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := S_OK else Result := E_NOINTERFACE;
end;

function TWebProtocolFactory._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TWebProtocolFactory._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TWebProtocolFactory.CreateInstance(const UnkOuter: IUnknown;
  const IID: TGUID; out Obj): HResult;
var
  WebProtocol: TWebProtocol;
begin
  if @Obj = nil then
  begin
    Result := E_POINTER;
    Exit;
  end;
  Pointer(Obj) := nil;
  if (unkOuter <> nil) and not (IsEqualIID(iid, IUnknown)) then
  begin
    Result := CLASS_E_NOAGGREGATION;
    Exit;
  end;
  WebProtocol := TWebProtocol.Create(FWebProvider, Pointer(UnkOuter));
  Result := WebProtocol.ObjQueryInterface(IID, Obj);
  if Failed(Result) then
    WebProtocol.Free;
end;

function TWebProtocolFactory.LockServer(fLock: BOOL): HResult;
begin
  Result := E_NOTIMPL;
end;

// TRequestObject
constructor TRequestObject.Create(const URL: String;
  BindInfo: IInternetBindInfo);
var
  bndinfo: TBindInfo;
  grfBINDF: DWord;
  Buffer: PChar;
  Proto, Handler, Location, Params: String;

begin
  ParseURL(URL, Proto, Handler, Location, Params);
  TStringList(FParams) := TStringList.Create;
  FBindInfo := BindInfo;
  FURL := URL;
  FProtocolInfo := Proto;
  FPathInfo := Handler + Location;
  FParamText := Params;
  Buffer := StrNew(PChar(Params));
  ExtractHeaderFields(['&'], [], Buffer, FParams, True);
  StrDispose(Buffer);
  bndinfo.cbSize := sizeof(TBindInfo);
  OleCheck(BindInfo.GetBindInfo(grfBINDF, bndinfo));
  if bndinfo.dwBindVerb = BINDVERB_GET then
    FMethodType := mtGet
  else if bndinfo.dwBindVerb = BINDVERB_POST then
    FMethodType := mtPost;
end;

destructor TRequestObject.Destroy;
begin
  FParams.Free;
  inherited;
end;

// TResponseObject
constructor TResponseObject.Create(ProtSink: IInternetProtocolSink; Stream: IStream);
begin
  FStream := Stream;
  FProtSink := ProtSink;
  FStatus := S_OK;
end;

procedure TResponseObject.Reset;
var
  NewPos: Int64;
begin
  FStream.Seek(0, 0, NewPos);
  FStream.SetSize(0);
end;

function TResponseObject.GetStream: TStream;
begin
  Result := TOleStream.Create(FStream);
end;

procedure TResponseObject.WriteText(AStr: String);
begin
  FStream.Write(PChar(AStr), Length(AStr), nil);
end;

procedure TResponseObject.WriteTextLn(AStr: String);
begin
  WriteText(AStr + #13#10);
end;

procedure TResponseObject.WriteTextFmt(AFmtStr: String; const Args: array of const);
begin
  WriteText(Format(AFmtStr, Args));
end;

procedure TResponseObject.WriteTextFmtLn(AFmtStr: String; const Args: array of const);
begin
  WriteTextFmt(AFmtStr + #13#10, Args);
end;

procedure TResponseObject.WriteStream(Source: TStream);
const
  MaxBufSize = $F000;
var
  BufSize, N: Integer;
  Buffer: PChar;
  Count: LongInt;
  NewPosition: LargeInt;

begin
  Source.Position := 0;
  Count := Source.Size;
  OleCheck(FStream.SetSize(Count));
  OleCheck(FStream.Seek(0, STREAM_SEEK_SET, NewPosition));
  if Count > MaxBufSize then BufSize := MaxBufSize else BufSize := Count;
  GetMem(Buffer, BufSize);
  try
    while Count <> 0 do
    begin
      if Count > BufSize then N := BufSize else N := Count;
      Source.ReadBuffer(Buffer^, N);
      FStream.Write(Buffer, N, nil);
      Dec(Count, N);
    end;
  finally
    FreeMem(Buffer, BufSize);
  end;
end;

procedure TResponseObject.WriteFile(const FileName: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    WriteStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TResponseObject.SetContent(Value: String);
var
  NewPosition: LargeInt;

begin
  OleCheck(FStream.SetSize(Length(Value)));
  OleCheck(FStream.Seek(0, STREAM_SEEK_SET, NewPosition));
  WriteText(Value);
end;

procedure TResponseObject.head(const encoding: String);
begin
  WriteTextFmtLn('<?xml version="1.0" encoding ="%s"?>', [encoding]);
end;

procedure TResponseObject.doctype(const name: String; const DTD: String);
begin
  WriteTextFmtLn('<!DOCTYPE %s %s >', [name, DTD]);
end;

procedure TResponseObject.CDATA;
begin
  WriteTextLn('<![CDATA[');
end;

procedure TResponseObject.endCDATA;
begin
  WriteTextLn(']]>');
end;

procedure TResponseObject.createRemark;
begin
  WriteTextLn('<!--');
end;

procedure TResponseObject.endRemark;
begin
  WriteTextLn('-->');
end;

procedure TResponseObject.Remark(const text: String);
begin
  WriteTextFmtLn('<!-- %s -->', [text]);
end;

function TResponseObject.VarToStrStd(const V: Variant): string;
begin
  if VarType(V) = varDouble then
    Str(Double(V), Result)  // DecimalSeparator always should be  '.' for XML Data Types
  else Result := System.VarToStr(V);
end;

function TResponseObject.attr(const attr: String; const value: String): String;
begin
  if (attr <> '') and (value <> '') then
    Result := ' ' + attr + '="' + EncodeText(value) + '"';
end;

function TResponseObject.EncodeText(const Text: String): String;
var
  Buffer, CurPos: PChar;
  BufSz: Integer;

procedure Replace(Ch: Char; const Str: String);
begin
  if CurPos - Buffer + Length(Str) >= BufSz then
    begin
      ReallocMem(Buffer, BufSz + 1024);
      FillChar(Buffer[BufSz], 1024, 0);
      Inc(BufSz, 1024);
    end;
  StrMove(CurPos + Length(Str), CurPos + 1, StrLen(CurPos) -1);
  Move(PChar(Str)^, CurPos^, Length(Str));
end;

begin
  BufSz := 512 + Length(Text);
  Buffer := AllocMem(BufSz);
  try
    StrCopy(Buffer, PChar(Text));
    CurPos := Buffer;
    while CurPos^ <> #0 do begin
      case CurPos^ of
        '>': Replace('>', '&lt;');
        '<': Replace('<', '&gt;');
        '"': Replace('"', '&quot;');
        '&': Replace('&', '&amp;');
      end;
      Inc(CurPos);
    end;
    Result := Buffer;
  finally
    FreeMem(Buffer);
  end;
end;

function TResponseObject.createTag(const name: String;
          const attr1: String = ''; const value1: String = '';
          const attr2: String = ''; const value2: String = '';
          const attr3: String = ''; const value3: String = '';
          const attr4: String = ''; const value4: String = '';
          const attr5: String = ''; const value5: String = '';
          const attr6: String = ''; const value6: String = '';
          const attr7: String = ''; const value7: String = '';
          const attr8: String = ''; const value8: String = '';
          const attr9: String = ''; const value9: String = ''): String;
begin
  Result :=  '<' + name + attr(attr1, value1) +
    attr(attr2, value2) + attr(attr3, value3) + attr(attr4, value4) +
    attr(attr5, value5) + attr(attr6, value6) + attr(attr7, value7) +
    attr(attr8, value8) + attr(attr9, value9);
end;

procedure TResponseObject.createElement(const name: String;
          const attr1: String = ''; const value1: String = '';
          const attr2: String = ''; const value2: String = '';
          const attr3: String = ''; const value3: String = '';
          const attr4: String = ''; const value4: String = '';
          const attr5: String = ''; const value5: String = '';
          const attr6: String = ''; const value6: String = '';
          const attr7: String = ''; const value7: String = '';
          const attr8: String = ''; const value8: String = '';
          const attr9: String = ''; const value9: String = '');
begin
  WriteTextLn(createTag(name, attr1, value1, attr2, value2,
    attr3, value3, attr4, value4, attr5, value5, attr6, value6,
    attr7, value7, attr8, value8, attr9, value9) + '>');
end;

procedure TResponseObject.endElement(const name: String);
begin
  WriteTextLn('</' + name + '>');
end;

procedure TResponseObject.EmptyElement(const name: String;
          const attr1: String = ''; const value1: String = '';
          const attr2: String = ''; const value2: String = '';
          const attr3: String = ''; const value3: String = '';
          const attr4: String = ''; const value4: String = '';
          const attr5: String = ''; const value5: String = '';
          const attr6: String = ''; const value6: String = '';
          const attr7: String = ''; const value7: String = '';
          const attr8: String = ''; const value8: String = '';
          const attr9: String = ''; const value9: String = '');
begin
  WriteTextLn(createTag(name, attr1, value1, attr2, value2,
    attr3, value3, attr4, value4, attr5, value5, attr6, value6,
    attr7, value7, attr8, value8, attr9, value9) + '/>');
end;

procedure TResponseObject.Element(const name: String; Value: Variant;
          const attr1: String = ''; const value1: String = '';
          const attr2: String = ''; const value2: String = '';
          const attr3: String = ''; const value3: String = '';
          const attr4: String = ''; const value4: String = '';
          const attr5: String = ''; const value5: String = '';
          const attr6: String = ''; const value6: String = '';
          const attr7: String = ''; const value7: String = '';
          const attr8: String = ''; const value8: String = '';
          const attr9: String = ''; const value9: String = '');
begin
  WriteTextLn(createTag(name, attr1, value1, attr2, value2,
    attr3, value3, attr4, value4, attr5, value5, attr6, value6,
    attr7, value7, attr8, value8, attr9, value9) + '>'
       + EncodeText(VarToStrStd(Value)) + '</' + name + '>');
end;

procedure TResponseObject.SetParam(const ParamName: String;
  ParamValue: OleVariant; namespaceURI: String = '');
begin
  if FProcessor <> nil then
    FProcessor.addParameter(ParamName, ParamValue, namespaceURI);
end;

procedure TResponseObject.Redirect(Url: WideString);
begin
  FProtSink.ReportProgress(BINDSTATUS_REDIRECTING, PWideChar(Url));
  FStatus := INET_E_REDIRECTING;
end;

procedure TResponseObject.UseDefaultHandler;
begin
  FStatus := INET_E_USE_DEFAULT_PROTOCOLHANDLER;
end;

function TResponseObject.GetDocument: IXMLDOMDocument2;
begin
  if FDocument = nil then
    begin
      OleCheck(CoCreateInstance(CLASS_FreeThreadedDOMDocument30, nil, CLSCTX_INPROC_SERVER,
        IID_IXMLDOMDocument2, FDocument));
      FDocument.async := False;
    end;
  Result := FDocument;
end;

// TWebDispatcher
constructor TWebDispatcher.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEnabled := True;
  FMask := TMask.Create('');
end;

destructor TWebDispatcher.Destroy;
begin
  FMask.Free;
  inherited Destroy;
end;

procedure TWebDispatcher.AssignTo(Dest: TPersistent);
begin
  if Dest is TWebDispatcher then
  begin
    if Assigned(Collection) then Collection.BeginUpdate;
    try
      with TWebDispatcher(Dest) do
      begin
        Default := Self.Default;
        Location := Self.Location;
        Enabled := Self.Enabled;
        XSLT := Self.XSLT;
        Template := Self.Template;
        MethodType := Self.MethodType;
      end;
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end;
  end else inherited AssignTo(Dest);
end;

procedure TWebDispatcher.SetDefault(Value: Boolean);
var
  I: Integer;
  Action: TWebDispatcher;
begin
  if Value <> FDefault then
  begin
    if Value and (Collection <> nil) then
      for I := 0 to Collection.Count - 1 do
      begin
        Action := TWebDispatchers(Collection).Items[I];
        if (Action <> Self) and (Action is TWebDispatcher) then
          Action.Default := False;
      end;
    FDefault := Value;
    Changed(False);
  end;
end;

procedure TWebDispatcher.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

procedure TWebDispatcher.SetAction(Value: TDispatchEvent);
begin
  FOnAction := Value;
  Changed(False);
end;

procedure TWebDispatcher.SetMethodType(Value: TMethodType);
begin
  if Value <> FMethodType then
  begin
    FMethodType := Value;
    Changed(False);
  end;
end;

procedure TWebDispatcher.SetXSLT(Value: Boolean);
begin
  if Value <> FXSLT then
    begin
      FXSLT := Value;
      Changed(False);
    end;
end;

procedure TWebDispatcher.SetLocation(Value: String);
var
  NewValue: string;
  Mask: TMask;
begin
  if Value <> '' then NewValue := DosPathToUnixPath(Value);
  begin
    Mask := TMask.Create(NewValue);
    try
      FLocation := NewValue;
      FMask.Free;
      FMask := nil;
    except
      Mask.Free;
      raise;
    end;
    FMask := Mask;
    Changed(False);
  end;
end;

function TWebDispatcher.GetDisplayName: string;
begin
  Result := FName;
end;

procedure TWebDispatcher.SetDisplayName(const Value: string);
var
  I: Integer;
  Action: TWebDispatcher;
begin
  if AnsiCompareText(Value, FName) <> 0 then
  begin
    if Collection <> nil then
      for I := 0 to Collection.Count - 1 do
      begin
        Action := TWebDispatchers(Collection).Items[I];
        if (Action <> Self) and (Action is TWebDispatcher) and
          (AnsiCompareText(Value, Action.Name) = 0) then
          raise Exception.Create(SDuplicateDispatchName);
      end;
    FName := Value;
    Changed(False);
  end;
end;

function TWebDispatcher.GetNamePath: String;
begin
  if Collection <> nil then
    Result := Format('%s[%d]',[Collection.GetNamePath, ID]) else
    Result := ClassName;
end;

function TWebDispatcher.GetProvider: TWebProvider;
begin
  Result := (Collection as TWebDispatchers).WebProvider;
end;

function TWebDispatcher.CreateProcessor(const TemplateName: WideString): IXSLProcessor;
var
  Stylesheet: IXMLDOMDocument2;
  ParseError: IXMLDOMParseError;
  XSLTemplate: IXSLTemplate;
begin
  OleCheck(CoCreateInstance(CLASS_XSLTemplate30, nil, CLSCTX_INPROC_SERVER,
    IID_IXSLTemplate, XSLTemplate));
  OleCheck(CoCreateInstance(CLASS_FreeThreadedDOMDocument30, nil, CLSCTX_INPROC_SERVER,
    IID_IXMLDOMDocument2, StyleSheet));
  StyleSheet.async := False;
  if not StyleSheet.load(TemplateName) then
    begin
      ParseError := StyleSheet.parseError;
      raise ETransform.CreateResFmt(@SErrorMsg,
        [ParseError.URL, ParseError.URL, ParseError.reason, ParseError.line, ParseError.linepos,
           ParseError.SrcText]);
    end;
  XSLTemplate.stylesheet := StyleSheet;
  Result := XSLTemplate.createProcessor;
  if GetProvider.DispObject <> nil then
    Result.addObject(GetProvider.DispObject, '');
end;

function TWebDispatcher.DispatchItem(Request: TRequestObject; Response: TResponseObject;
  DoDefault: Boolean): Boolean;
var
  cb: Int64;
  pwzMimeOut: PWideChar;
  Stylesheet, Document: IXMLDOMDocument;
  ParseError: IXMLDOMParseError;
  cchResult: DWord;
  wszResult: array [0..512] of WideChar;
begin
  Result := False;
  if (FDefault and DoDefault) or (FEnabled and ((FMethodType = mtAny) or
    (FMethodType = Request.MethodType)) and
    FMask.Matches(Request.PathInfo)) then
    begin
      if FXSLT then
        begin
          OleCheck(CoInternetCombineUrl(PWideChar(WideString(GetProvider.BasePath)),
            PWideChar(WideString(FTemplate)), 0, @wszResult, sizeof(wszResult), cchResult, 0));
          if GetProvider.FParserLevel = MSXML30 then
            begin
              if FProcessor = nil then  // Templates not cached
                FProcessor := CreateProcessor(wszResult);
              Response.FProcessor := FProcessor;
            end;
        end;
      try
        if not Result and Assigned(FOnAction) then
          begin
            Result := True;
            FOnAction(Self, Request, Response, Result);
          end;
        if Result then
          begin
            if FMimeType = '' then
              begin
                if Failed(FindMimeFromData(nil, PWideChar(WideString(FLocation)), nil, 0, nil, 0, pwzMimeOut, 0)) then
                  raise ETransform.CreateRes(@SMimeTypeNotDetected);
                Response.MimeType := pwzMimeOut;
              end
            else
              Response.MimeType := FMimeType;
            if FXSLT then
              if GetProvider.FParserLevel = MSXML30 then
                begin // New MS XML Parser SDK Preview Beta with support XSLT and XPath
                  if Response.FDocument <> nil then
                    FProcessor.input := Response.FDocument
                  else
                    begin
                      OleCheck(Response.Stream.Seek(0, STREAM_SEEK_SET, cb));
                      OleCheck(CoCreateInstance(CLASS_FreeThreadedDOMDocument30, nil, CLSCTX_INPROC_SERVER,
                        IID_IXMLDOMDocument, Document));
                      if not Document.load(Response.Stream) then
                        begin
                          ParseError := Document.parseError;
                          raise ETransform.CreateResFmt(@SErrorMsg,
                            [ParseError.URL, ParseError.URL, ParseError.reason, ParseError.line, ParseError.linepos,
                               ParseError.SrcText]);
                        end;
                      FProcessor.input := Document;
                    end;
                  OleCheck(Response.Stream.Seek(0, STREAM_SEEK_SET, cb));
                  OleCheck(Response.Stream.SetSize(0));
                  FProcessor.output := Response.Stream;
                  FProcessor.transform;
                end
              else // Standard IE 5 XML Parser
                begin
                  OleCheck(CoCreateInstance(CLASS_OldDOMDocument, nil, CLSCTX_INPROC_SERVER,
                    IID_IXMLDOMDocument, Stylesheet));
                  Stylesheet.async := False;
                  if not Stylesheet.load(WideString(wszResult)) then
                    begin
                      ParseError := StyleSheet.parseError;
                      raise ETransform.CreateResFmt(@SErrorMsg,
                        [ParseError.URL, ParseError.URL, ParseError.reason, ParseError.line, ParseError.linepos,
                           ParseError.SrcText]);
                    end;
                  if Response.FDocument = nil then
                    begin
                      OleCheck(Response.Stream.Seek(0, STREAM_SEEK_SET, cb));
                      OleCheck(CoCreateInstance(CLASS_OldDOMDocument, nil, CLSCTX_INPROC_SERVER,
                        IID_IXMLDOMDocument, Document));
                      if not Document.load(Response.Stream) then
                        begin
                          ParseError := Document.parseError;
                          raise ETransform.CreateResFmt(@SErrorMsg,
                            [ParseError.URL, ParseError.URL, ParseError.reason, ParseError.line, ParseError.linepos,
                               ParseError.SrcText]);
                        end;
                    end
                  else
                    Document := Response.FDocument;
                  OleCheck(Response.Stream.Seek(0, STREAM_SEEK_SET, cb));
                  OleCheck(Response.Stream.SetSize(0));
                  Document.transformNodeToObject(Stylesheet, Response.Stream);
                end;
          end;
      finally
        Response.FDocument := nil;
        Response.FProcessor := nil;
        if not FCacheTemplates then
          FProcessor := nil;
      end;
    end;
end;

// TWebDispatchers
constructor TWebDispatchers.Create(WebProvider: TWebProvider;
  ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FWebProvider := WebProvider;
end;

function  TWebDispatchers.Add: TWebDispatcher;
begin
  Result := TWebDispatcher(inherited Add);
end;

function TWebDispatchers.GetActionItem(Index: Integer): TWebDispatcher;
begin
  Result := TWebDispatcher(inherited Items[Index]);
end;

procedure TWebDispatchers.SetActionItem(Index: Integer; Value: TWebDispatcher);
begin
  Items[Index].Assign(Value);
end;

function TWebDispatchers.GetAttrCount: Integer;
begin
  Result := 6;
end;

function TWebDispatchers.GetAttr(Index: Integer): string;
begin
  case Index of
    0: Result := SDispatchItemName;
    1: Result := SDispatchItemURI;
    2: Result := SDispatchItemEnabled;
    3: Result := SDispatchItemDefault;
    4: Result := SDispatchItemXSLT;
    5: Result := SDispatchItemTemplate;
  else
    Result := '';
  end;
end;

function TWebDispatchers.GetItemAttr(Index, ItemIndex: Integer): string;
begin
  case Index of
    0: Result := Items[ItemIndex].Name;
    1: Result := Items[ItemIndex].FLocation;
    2: if Items[ItemIndex].Enabled then
         Result := 'True' else Result := 'False';
    3: if Items[ItemIndex].Default then
         Result := '*' else Result := '';
    4: if Items[ItemIndex].FXSLT then
         Result := 'True' else Result := 'False';
    5: Result := Items[ItemIndex].FTemplate;
  else
    Result := '';
  end;
end;

function TWebDispatchers.GetOwner: TPersistent;
begin
  Result := FWebProvider;
end;

procedure TWebDispatchers.SetItemName(Item: TCollectionItem);
var
  I, J: Integer;
  ItemName: string;
  CurItem: TWebDispatcher;
begin
  J := 1;
  while True do
  begin
    ItemName := Format('WebDisp%d', [J]);
    I := 0;
    while I < Count do
    begin
      CurItem := Items[I] as TWebDispatcher;
      if (CurItem <> Item) and (CompareText(CurItem.Name, ItemName) = 0) then
      begin
        Inc(J);
        Break;
      end;
      Inc(I);
    end;
    if I >= Count then
    begin
      (Item as TWebDispatcher).Name := ItemName;
      Break;
    end;
  end;
end;

procedure TWebDispatchers.Update(Item: TCollectionItem);
begin
end;

// TWebProvider
constructor TWebProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TWebDispatchers.Create(self, TWebDispatcher);
  FFactory := CreateProtocolFactory;
  FWindowHandle := AllocateHWnd(WndProc);
  FRefreshDelay := 300;
  FProtocol := 'local';
  FCached := True;
end;

destructor TWebProvider.Destroy;
begin
  UnRegisterNamespace;
  FItems.Free;
  FFactory := nil;
  DeallocateHWnd(FWindowHandle);
  inherited Destroy;
end;

function TWebProvider.CreateProtocolFactory: IClassFactory;
begin
  Result := TWebProtocolFactory.Create(self);
end;

function TWebProvider.DoBeforeDispatch(Request: TRequestObject;
  Response: TResponseObject): Boolean;
begin
  Result := False;
  if Assigned(FBeforeDispatch) then
    FBeforeDispatch(self, Request, Response, Result);
end;

function TWebProvider.DoAfterDispatch(Request: TRequestObject;
  Response: TResponseObject): Boolean;
begin
  if Assigned(FAfterDispatch) then
    FAfterDispatch(self, Request, Response, Result);
end;

function TWebProvider.DispatchAction(Request: TRequestObject;
  Response: TResponseObject): Boolean;
var
  I: Integer;
  Item, Default: TWebDispatcher;
begin
  FRequest := Request;
  FResponse := Response;
  I := 0;
  Default := nil;
  try
    Result := DoBeforeDispatch(Request, Response);
    while not Result and (I < FItems.Count) do
    begin
      Item := FItems[I];
      Result := Item.DispatchItem(Request, Response, False);
      if Item.Default then Default := Item;
      Inc(I);
    end;
    if not Result and Assigned(Default) then
      Result := Default.DispatchItem(Request, Response, True);
    if Result then
      Result := DoAfterDispatch(Request, Response);
  except
    on E: Exception do
      begin
        Result := True;
        FResponse.Reset;
        FResponse.FMimeType := 'text/html';
        FResponse.WriteText(GetErrorPage(Format('class <B>%s</B> with message ''%s''',
          [E.ClassName, E.Message])));
      end;
  end;
  FRequest := nil;
  FResponse := nil;
end;

procedure TWebProvider.SetItems(Value: TWebDispatchers);
begin
  FItems.Assign(Value);
end;

procedure TWebProvider.SetBasePath(Value: String);
begin
  if Value <> FBasePath then
    begin
      if (Value <> '') and (Copy(Value, Length(Value), 1) <> '/')
        then Value := Value + '/';
      FBasePath := Value;
    end;
end;

// This text is missed, as in RegisterNameSpace IE5 ignores patterns

(*
procedure TWebProvider.ExtractPatterns(Patterns: String; var cPatterns: Cardinal;
  var ppwPatterns: Pointer);

type
  POleStrings = ^TOleStrings;
  TOleStrings = array [0..0] of POleStr;

  function AllocPatternStr(const S: String; var Count: Cardinal): String;
  var
    P: PChar;
  begin
    Result := '';
    if S <> '' then
    begin
      Count := 1;
      Result := S;
      P := AnsiStrScan(PChar(Result), '|');
      while P <> nil do
      begin
        P^ := #0;
        Inc(P);
        P := AnsiStrScan(P, '|');
        Inc(Count);
      end;
    end;
  end;

var
  Str: String;
  pStrArray: POleStrings;
  Buffer: POleStr;
  cchBuffer, cStrArray: Cardinal;
  Index: Integer;
begin
  cPatterns := 0;
  ppwPatterns := nil;
  Str := AllocPatternStr(Patterns, cPatterns);
  if Str <> '' then
    begin
      cchBuffer := (Length(Str) + 1) shl 1;
      cStrArray := sizeof(POleStr) * cPatterns;
      ppwPatterns := CoTaskMemAlloc(cStrArray + cchBuffer);
      pStrArray := ppwPatterns;
      Buffer := POleStr(PChar(ppwPatterns) + cStrArray);
      MultiByteToWideChar(CP_ACP, 0, PChar(Str), Length(Str) + 1, Buffer, cchBuffer);
      for Index := 0 to cPatterns -1 do
        begin
          pStrArray[Index] := Buffer;
          Buffer := Buffer + Length(WideString(Buffer)) + 1;
        end;
    end;
end;
*)

procedure TWebProvider.SetActive(Value: Boolean);
begin
  if Value <> FActive then
    if Value then
      RegisterNameSpace else
      UnRegisterNameSpace;
end;

procedure TWebProvider.RegisterNameSpace;
var
  InternetSession: IInternetSession;
begin
  if not FActive and not (csDesigning in ComponentState) then
    begin
      OleCheck(CoInternetGetSession(0, InternetSession, 0));
      OleCheck(InternetSession.RegisterNameSpace(FFactory,
         DummyGUID, PWideChar(WideString(FProtocol)), 0, nil, 0));
      FActive := True;
    end;
end;

procedure TWebProvider.UnRegisterNameSpace;
var
  InternetSession: IInternetSession;
begin
  if FActive then
    begin
      OleCheck(CoInternetGetSession(0, InternetSession, 0));
      OleCheck(InternetSession.UnregisterNameSpace(FFactory,
         PWideChar(WideString(FProtocol))));
      FActive := False;
    end;
end;

function TWebProvider.GetErrorPage(const Msg: String): String;
begin
  Result := Format(LoadResString(@SErrorPage), [ExtractFileName(ParamStr(0)), Msg]);
end;

procedure TWebProvider.WndProc(var Msg: TMessage);
var
  URLInfo: PURLInfo;
  MimeType: String;
begin
  with Msg do
    if Msg = CM_INVOKE_URL then
      try
        URLInfo := Pointer(LParam);
        InvokeURL(URLInfo.TargetURL, URLInfo.BindInfo,
          URLInfo.ProtSink, URLInfo.Stream, URLInfo.Status, MimeType);
        StrLCopy(URLInfo.MimeType, PChar(MimeType), sizeof(URLInfo.MimeType));
        Result := 1;
      except
        Application.HandleException(Self);
      end
    else
      Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
end;

procedure TWebProvider.InvokeURL(const URL: String; BindInfo: IInternetBindInfo;
  ProtSink: IInternetProtocolSink; Stream: IStream;
  var Status: HResult; var MimeType: String);
var
  Request: TRequestObject;
  Response: TResponseObject;
begin
  Request := TRequestObject.Create(URL, BindInfo);
  Response := TResponseObject.Create(ProtSink, Stream);
  try
    if DispatchAction(Request, Response) then
      if Response.FStatus = S_OK then
        begin
          MimeType := Response.MimeType;
          Status := S_OK;
        end
      else
        Status := Response.FStatus
    else
      Status := INET_E_INVALID_URL;
  finally
    Request.Free;
    Response.Free;
  end;
end;

procedure ParseURL(const URL: String; var Proto, Handler, Location, Params: String);
var
  K: Integer;

begin
  K := Pos(':', URL);
  if K <> 0 then
    Proto := Copy(URL, 1, K -1)
  else Proto := '';
  Inc(K);
  while URL[K] = '/' do Inc(K);
  Handler := '';
  while not (URL[K] in ['?', '/', #0]) do
    begin
      Handler := Handler + URL[K];
      Inc(K);
    end;
  Location := '';
  while not (URL[K] in ['?', #0]) do
    begin
      if (Location = '') or (URL[K] <> '/') or (Location[Length(Location)] <> '/') then
        Location := Location + URL[K];
      Inc(K);
    end;
  Params := '';
  if URL[K] = '?' then
    begin
      Inc(K);
      while URL[K] <> #0 do
        begin
          Params := Params + URL[K];
          Inc(K);
        end;
    end;
end;


end.
