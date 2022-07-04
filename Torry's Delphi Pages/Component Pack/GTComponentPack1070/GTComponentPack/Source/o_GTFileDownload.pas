{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtFileDownload                                 }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_GTFileDownload;

interface
{$I ../../GTDIRECTIVES.inc}
uses
   Classes
  ,UrlMon
  ,Windows
  ,ActiveX
  ;
type
{------------------------------------------------------------------------------}
  TgtFileDownload = class;
{------------------------------------------------------------------------------}
  TgtFileDownloadProgressEvent = procedure (Sender : TObject ; const Max : Integer ; const CurrentPos :Integer; var Cancel : Boolean) of Object;
{------------------------------------------------------------------------------}
  TgtFileDownloadThread = class(TInterfacedPersistent,IBindStatusCallback)
  private
    { Private declarations }
    FFileDownload : TgtFileDownload;
    FRemoteFile   : AnsiString;
    FLocalFile    : AnsiString;
    FMax          : Integer;
    FCurPos       : Integer;
  protected
    { Protected declarations }
    procedure UpdateUI;
    procedure DownloadBegin;
    procedure DownloadComplete;
  public
    { Public declarations }
    constructor Create(AFileDownload : TgtFileDownload);
    procedure   Execute;
  public
    property RemoteFile : AnsiString read FRemoteFile write FRemoteFile;
    property LocalFile  : AnsiString read FLocalFile  write FLocalFile;
  public
    { IBindStatusCallback }
    function OnStartBinding(dwReserved: DWORD; pib: IBinding): HResult; stdcall;
    function GetPriority(out nPriority): HResult; stdcall;
    function OnLowResource(reserved: DWORD): HResult; stdcall;
    function OnProgress(ulProgress, ulProgressMax, ulStatusCode: ULONG;szStatusText: LPCWSTR): HResult; stdcall;
    function OnStopBinding(hresult: HResult; szError: LPCWSTR): HResult; stdcall;
    function GetBindInfo(out grfBINDF: DWORD; var bindinfo: TBindInfo): HResult; stdcall;
    function OnDataAvailable(grfBSCF: DWORD; dwSize: DWORD; formatetc: PFormatEtc;
      stgmed: PStgMedium): HResult; stdcall;
    function OnObjectAvailable(const iid: TGUID; punk: IUnknown): HResult; stdcall;
  published
    { Published declarations}
  end;
{------------------------------------------------------------------------------}
  TgtFileDownload = class(TComponent)
  private
    FOnProgressEvent    : TgtFileDownloadProgressEvent;
    FOnDownloadBegin    : TNotifyEvent;
    FOnDownloadComplete : TNotifyEvent;
    FDownloadActive     : Boolean;
    FCancelled          : Boolean;
    FOnDownloadError    : TNotifyEvent;
    FOnDownloadCancelled: TNotifyEvent;
    { Private declarations }
  protected
    { Protected declarations }
    FDownLoadThread : TgtFileDownloadThread;
    procedure DoDownloadBegin;virtual;
    procedure DoDownloadComplete;virtual;
    procedure DoDownloadError;virtual;
    procedure DoDownloadCancelled;virtual;
    procedure DestroyThread;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    function DownloadFile(const FileURL : string ; LocalFileName : string):Boolean;virtual;
    procedure CancelDownload;
  published
    { Published declarations}
    property OnDownloadProgress : TgtFileDownloadProgressEvent read FOnProgressEvent     write FOnProgressEvent;
    property OnDownloadBegin    : TNotifyEvent                 read FOnDownloadBegin     write FOnDownloadBegin;
    property OnDownloadComplete : TNotifyEvent                 read FOnDownloadComplete  write FOnDownloadComplete;
    property OnDownloadError    : TNotifyEvent                 read FOnDownloadError     write FOnDownloadError;
    property OnDownloadCancelled: TNotifyEvent                 read FOnDownloadCancelled write FOnDownloadCancelled;
    property DownloadActive     : Boolean                      read FDownloadActive;
  end;
{------------------------------------------------------------------------------}


implementation

uses
   SysUtils
  ,Consts
  ,Forms
  ;

{ Original Idea and code from TDownLoadURL ExtActns.pas }
type
  TUrlMonDownloadToFile = function(Caller: IUnknown; URL: PAnsiChar; FileName: PAnsiChar;
    Reserved: DWORD; StatusCB: IBindStatusCallback): HResult; stdcall;

var
  UrlMonHandle         : HMODULE;
  UrlMonDownloadToFile : TUrlMonDownloadToFile;
const
  UrlMonLib              = 'URLMON.DLL';
  sURLMonDownloadToFileA = 'URLDownloadToFileA';

{$IFDEF DELPHI6}
  resourcestring
    SUrlMonDllMissing = 'Unable to load %s';
{$ENDIF}

{ TgtFileDownloadThread }
{------------------------------------------------------------------------------}
constructor TgtFileDownloadThread.Create(AFileDownload: TgtFileDownload);
begin
  FFileDownload   := AFileDownload;
end;
{------------------------------------------------------------------------------}
procedure TgtFileDownloadThread.DownloadBegin;
begin
  FFileDownload.DoDownloadBegin;
end;
{------------------------------------------------------------------------------}
procedure TgtFileDownloadThread.DownloadComplete;
begin
  FFileDownload.DoDownloadComplete;
end;
{------------------------------------------------------------------------------}
procedure TgtFileDownloadThread.Execute;
var
  DownloadResult : Cardinal;
begin
  try
    UrlMonHandle := Windows.LoadLibrary(UrlMonLib);
    if  UrlMonHandle <> 0 then
      UrlMonDownloadToFile := GetProcAddress(UrlMonHandle, PChar(sURLMonDownloadToFileA));
    if Assigned(UrlMonDownloadToFile) then
    begin
      DownloadResult := URLMonDownloadToFile(nil, PAnsiChar(PAnsiString(RemoteFile)), PAnsiChar(PAnsiString(LocalFile)), 0, Self as IBindStatusCallback);
      case DownloadResult of
        S_OK    :FFileDownLoad.DoDownloadComplete;
        S_FALSE :FFileDownLoad.DoDownLoadCancelled;
        else
          FFileDownLoad.DoDownLoadError;
      end;
      //if URLMonDownloadToFile(nil, PChar(RemoteFile), PChar(LocalFile), 0, Self as IBindStatusCallback) <> S_OK then
      //  raise Exception.CreateResFmt(@SErrorDownloadingURL, [RemoteFile]);
    end
    else
      raise Exception.CreateResFmt(@SUrlMonDllMissing, [UrlMonLib]);

  finally
    if  UrlMonHandle <> 0 then
      Windows.FreeLibrary(UrlMonHandle);
  end;
end;
{------------------------------------------------------------------------------}
function TgtFileDownloadThread.GetBindInfo(out grfBINDF: DWORD;var bindinfo: TBindInfo): HResult;
begin
  Result := E_NOTIMPL;
end;
{------------------------------------------------------------------------------}
function TgtFileDownloadThread.GetPriority(out nPriority): HResult;
begin
  Result := E_NOTIMPL;
end;
{------------------------------------------------------------------------------}
function TgtFileDownloadThread.OnDataAvailable(grfBSCF, dwSize: DWORD;formatetc: PFormatEtc; stgmed: PStgMedium): HResult;
begin
  Result := E_NOTIMPL;
end;
{------------------------------------------------------------------------------}
function TgtFileDownloadThread.OnLowResource(reserved: DWORD): HResult;
begin
  Result := E_NOTIMPL;
end;
{------------------------------------------------------------------------------}
function TgtFileDownloadThread.OnObjectAvailable(const iid: TGUID;punk: IInterface): HResult;
begin
  Result := E_NOTIMPL;
end;
{------------------------------------------------------------------------------}
function TgtFileDownloadThread.OnProgress(ulProgress, ulProgressMax,ulStatusCode: ULONG; szStatusText: LPCWSTR): HResult;
const
  ContinueDownload: array[Boolean] of Integer = (S_OK, E_ABORT);
begin
  FMax    := ulProgressMax;
  FCurPos := ulProgress;
  UpdateUI;
  Result :=  ContinueDownload[FFileDownload.FCancelled];
end;
{------------------------------------------------------------------------------}
function TgtFileDownloadThread.OnStartBinding(dwReserved: DWORD;pib: IBinding): HResult;
begin
  Result := E_NOTIMPL;
end;
{------------------------------------------------------------------------------}
function TgtFileDownloadThread.OnStopBinding(hresult: HResult;szError: LPCWSTR): HResult;
begin
  Result := E_NOTIMPL;
end;
{------------------------------------------------------------------------------}
procedure TgtFileDownloadThread.UpdateUI;
begin
  if Assigned(FFileDownload.OnDownloadProgress) then
    FFileDownload.OnDownloadProgress(FFileDownload,FMax,FCurPos,FFileDownload.FCancelled);
  Application.ProcessMessages;
end;
{------------------------------------------------------------------------------}



{ TgtFileDownload }
{------------------------------------------------------------------------------}
constructor TgtFileDownload.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCancelled := False;
end;
{------------------------------------------------------------------------------}
destructor TgtFileDownload.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtFileDownload.DestroyThread;
begin
  if Assigned(FDownLoadThread) then
  begin
    FDownLoadThread.Free;
    FDownLoadThread := nil;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtFileDownload.DoDownloadBegin;
begin
  FDownloadActive := True;
  FCancelled      := False;
  if Assigned(FOnDownloadBegin) then
    FOnDownloadBegin(Self);
end;
{------------------------------------------------------------------------------}
procedure TgtFileDownload.DoDownloadComplete;
begin
  FDownloadActive := False;
  if Assigned(FOnDownloadComplete) then
    FOnDownloadComplete(Self);
end;
{------------------------------------------------------------------------------}
function TgtFileDownload.DownloadFile(const FileURL: string;LocalFileName: string): Boolean;
begin
  Result := True;
  FDownLoadThread := TgtFileDownloadThread.Create(Self);
  try
    FDownLoadThread.RemoteFile := AnsiString(FileURL);
    FDownLoadThread.LocalFile  := AnsiString(LocalFileName);
    FDownLoadThread.Execute;
  finally
    DestroyThread;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtFileDownload.CancelDownload;
begin
  FCancelled := True;
  DoDownloadCancelled;
  FDownloadActive := False;
end;
{------------------------------------------------------------------------------}
procedure TgtFileDownload.DoDownloadCancelled;
begin
  if (FCancelled) and (FDownloadActive) then
    if Assigned(FOnDownloadCancelled) then
      FOnDownloadCancelled(Self);
end;
{------------------------------------------------------------------------------}
procedure TgtFileDownload.DoDownloadError;
begin
  if not FCancelled then
    if Assigned(FOnDownloadError) then
      FOnDownloadError(Self);
end;
{------------------------------------------------------------------------------}





end.
