unit o_GTFTPClient;

interface
uses
   Classes
  ,Windows
  ,WinINet
  ;
type
{------------------------------------------------------------------------------}
  TgtFtpTransferType = (
                      fttUnKnown
                     ,fttAscii
                     ,fttBinary
                     );
{------------------------------------------------------------------------------}
  TgtFtpDirectoryCommand =
                          (
                            fdcGetCurrentDir
                           ,fdcSetCurrentDir
                           ,fdcCreateDir
                           ,fdcDeleteDir
                           )
                           ;
{------------------------------------------------------------------------------}
  TgtFTPClient = class(TComponent)
  private
    FPort: Integer;
    FServer: string;
    FUserName: string;
    FPassword: string;
    FOnDisConnect: TNotifyEvent;
    FOnConnect: TNotifyEvent;
    FTransferType: TgtFtpTransferType;
    function GetConnected: Boolean;
    { Private declarations }
  protected
    { Protected declarations }
    FInternetOpenHandle    : HInternet;
    FInternetConnectHandle : HInternet;
    FAppName               : string;
    FCurrentDir            : string;
  protected
    procedure DoConnect;virtual;
    procedure DoDisconnect;virtual;
    procedure DoExecuteCommand  (Command: string;ExpectResponse: Boolean);virtual;
    function  DoDirectoryCommand(Directory : string; ADirCommand : TgtFtpDirectoryCommand):Boolean;virtual;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    procedure Connect;
    procedure DisConnect;
    procedure ExecuteCommand     (Command   : string ;ExpectResponse : Boolean);
    function  CreateDirectory    (Directory : string):Boolean;
    function  DeleteDirectory    (Directory : string):Boolean;
    function  GetCurrentDirectory(var Dir   : string):Boolean;
    function  SetCurrentDirectory(Directory : string):Boolean;
  published
    { Published declarations}
    property Server       : string               read FServer        write FServer;
    property Port         : Integer              read FPort          write FPort;
    property UserName     : string               read FUserName      write FUserName;
    property PassWord     : string               read FPassword      write FPassWord;
    property TransferType : TgtFtpTransferType   read FTransferType  write FTransferType default fttBinary;
    property Connected    : Boolean              read GetConnected;
  published
    property OnConnect    : TNotifyEvent read FOnConnect    write FOnConnect;
    property OnDisConnect : TNotifyEvent read FOnDisConnect write FOnDisConnect;
  end;
{------------------------------------------------------------------------------}


implementation

uses
   SysUtils
  ,Dialogs
  ;

const
  TransferTypeArray : array [0..2] of Cardinal =(FTP_TRANSFER_TYPE_UNKNOWN,FTP_TRANSFER_TYPE_ASCII,FTP_TRANSFER_TYPE_BINARY);

{ TgtFTPClient }
{------------------------------------------------------------------------------}
constructor TgtFTPClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAppName := ExtractFileName(ParamStr(0));
  FPort    := 21;
  FTransferType := fttBinary;
end;
{------------------------------------------------------------------------------}
destructor TgtFTPClient.Destroy;
begin
  DisConnect;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtFTPClient.DoConnect;
begin
 FInternetOpenHandle := WinINet.InternetOpen(PChar(FAppName)
                                             ,INTERNET_OPEN_TYPE_PRECONFIG
                                             ,nil
                                             ,nil
                                             ,INTERNET_FLAG_ASYNC);
 if Assigned(FInternetOpenHandle) then
  FInternetConnectHandle := WinINet.InternetConnect(Pointer(FInternetOpenHandle)
                                                   ,PChar(FServer)
                                                   ,FPort
                                                   ,PChar(FUserName)
                                                   ,PChar(FPassword)
                                                   ,INTERNET_SERVICE_FTP
                                                   ,INTERNET_FLAG_PASSIVE
                                                   ,0);
  if Assigned(FInternetConnectHandle) then
    if Assigned(FOnConnect) then
      FOnConnect(Self);
end;
{------------------------------------------------------------------------------}
procedure TgtFTPClient.DoDisconnect;
begin
  if Assigned(FInternetConnectHandle) then
    InternetCloseHandle(FInternetConnectHandle);
  if Assigned(FInternetOpenHandle) then
    InternetCloseHandle(FInternetOpenHandle);
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self);
end;
{------------------------------------------------------------------------------}
procedure TgtFTPClient.DoExecuteCommand(Command: string;ExpectResponse: Boolean);
begin
  if Connected then
    FtpCommand(FInternetConnectHandle,ExpectResponse,Ord(FTransferType),PChar(Command),0);
end;
{------------------------------------------------------------------------------}
function TgtFTPClient.DoDirectoryCommand(Directory : string; ADirCommand : TgtFtpDirectoryCommand): Boolean;
var
  Buffer : array [0..MAX_PATH] of Char;
  BufferSize : Cardinal;
begin
  Result := False;
  if Connected then
  begin
    case ADirCommand of
      fdcCreateDir     : Result := FtpCreateDirectory(FInternetConnectHandle,PChar(Directory));
      fdcDeleteDir     : Result := FtpRemoveDirectory(FInternetConnectHandle,PChar(Directory));
      fdcGetCurrentDir :
        begin
           BufferSize  := SizeOf(Buffer);
           FillChar(Buffer,BufferSize,#0);
           Result      := FtpGetCurrentDirectory(FInternetConnectHandle,Buffer,BufferSize);
           FCurrentDir := Buffer;
        end;
      fdcSetCurrentDir : Result := FtpSetCurrentDirectory(FInternetConnectHandle,PChar(Directory));
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtFTPClient.Connect;
begin
 DoConnect;
end;
{------------------------------------------------------------------------------}
procedure TgtFTPClient.DisConnect;
begin
 DoDisconnect;
end;
{------------------------------------------------------------------------------}
procedure TgtFTPClient.ExecuteCommand(Command: string;ExpectResponse: Boolean);
begin
  DoExecuteCommand(Command,ExpectResponse);
end;
{------------------------------------------------------------------------------}
function TgtFTPClient.CreateDirectory(Directory: string): Boolean;
begin
  Result := DoDirectoryCommand(Directory,fdcCreateDir);
end;
{------------------------------------------------------------------------------}
function TgtFTPClient.DeleteDirectory(Directory: string): Boolean;
begin
  Result := DoDirectoryCommand(Directory,fdcDeleteDir);
end;
{------------------------------------------------------------------------------}
function TgtFTPClient.GetCurrentDirectory(var Dir: string): Boolean;
begin
  Result := DoDirectoryCommand('',fdcGetCurrentDir);
  Dir := FCurrentDir;
end;
{------------------------------------------------------------------------------}
function TgtFTPClient.SetCurrentDirectory(Directory: string): Boolean;
begin
  Result := DoDirectoryCommand(Directory,fdcSetCurrentDir);
end;
{------------------------------------------------------------------------------}




{------------------------------------------------------------------------------}
function TgtFTPClient.GetConnected: Boolean;
begin
  Result := Assigned(FInternetConnectHandle);
end;
{------------------------------------------------------------------------------}



end.
