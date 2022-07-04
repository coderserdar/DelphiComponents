unit Pipes;

// Komponenty do do komunikacji przez nazwane potoki
// w architekturze klient-serwer
// Tomasz Bojara

interface

uses
  Windows, Classes, SysUtils;

type

  EPipeError = Class(Exception);

  TAbstractPipe = class;

  TReadDataEvent = procedure(APipe: TAbstractPipe; Buffer: Pointer; BuffSize: DWord) of object;
  TPipeEvent = procedure(APipe: TAbstractPipe) of object;
  TPipeExceptionEvent = procedure(APipe: TAbstractPipe; AE: Exception) of object;

  TServerNamedPipe = class;
  TClientNamedPipe = class;

  TAbstractPipe = class(TThread)
  private
    OL : TOVERLAPPED;
    FEvent: THandle;
    FEventWrite: THandle;
    OLWrite : TOVERLAPPED;
    hPipe: THandle;
    FBuffer: Pointer;
    FConnected: Boolean;
    FPipeName: AnsiString;
    FSecurityAttributes: TSecurityAttributes;
    FIsDisconnectPipe: Boolean;
    FReadBufSize: Integer;
    FOnReadData: TReadDataEvent;
    FOnDisconnect: TPipeEvent;
    FOnConnect: TPipeEvent;
    FOnException: TPipeExceptionEvent;
    FOutBytes: Cardinal;
    FInBytes: Cardinal;
    FTimeout: Cardinal;
  protected
    procedure CreatePipe; virtual; abstract;
    procedure DoReadData(Buffer: Pointer; BuffSize : DWord); virtual;
    procedure ReadData;
    procedure Execute; override;
    procedure Stop; virtual;
    function Stoped: Boolean; virtual;
    procedure Connect; virtual;
    procedure Disconnect; virtual;
  public
    Data: TObject;
    constructor Create(aPipeName : AnsiString; aReadBufSize: Integer;
      aSecurityAttributes: TSecurityAttributes); virtual;
    destructor Destroy; override;
    procedure CloseHandlePipe;
    procedure DisconnectPipe; virtual;
    procedure Write(var Buffer; BuffSize: Cardinal; var BytesWritten: Cardinal; WrTimeout: Cardinal=500);
    property Connected: Boolean read FConnected;
    property PipeName: AnsiString read FPipeName;
    property Buffer: Pointer read FBuffer;
    property IsDisconnectPipe: Boolean read FIsDisconnectPipe;
    property Handle: THandle read hPipe;
    property OutBytes: Cardinal read FOutBytes;
    property InBytes: Cardinal read FInBytes;
    property OnReadData: TReadDataEvent read FOnReadData write FOnReadData;
    property OnConnect: TPipeEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TPipeEvent read FOnDisconnect write FOnDisconnect;
    property OnException: TPipeExceptionEvent read FOnException write FOnException;
  end;

  TMultiReadDataEvent = procedure(Sender : TObject; ServerPipe : TAbstractPipe;
    Buffer : Pointer; BuffSize : DWord) of object;

  TPipeMode = (pmByte, pmMessage);
  TPipeAccess = (paDuplex, paOutBound, paInBound);

  TServerPipe = class(TAbstractPipe)
  private
    FMultiServer: TServerNamedPipe;
  protected
    procedure CreatePipe; override;
    procedure Connect; override;
    procedure Disconnect; override;
    procedure Execute; override;
    procedure DoReadData(Buffer : Pointer; BuffSize : DWord); override;
    function Stoped: Boolean; override;
  public
    property MultiServer: TServerNamedPipe read FMultiServer;
  end;

  TClientPipe = class(TAbstractPipe)
  private
    FClientNamedPipe: TClientNamedPipe;
  protected
    procedure Execute; override;
    procedure CreatePipe; override;
    procedure Connect; override;
    procedure Stop; override;
    function Stoped: Boolean; override;
  public
    property ClientNamedPipe: TClientNamedPipe read FClientNamedPipe;
  end;

  TNamedPipe = class(TComponent)
  private
    FPipeAccess: TPipeAccess;
    FTimeout: Cardinal;
    FInBufSize: Integer;
    FOutBufSize: Integer;
  protected
    function GetConnected: Boolean; virtual; abstract;
    function GetActive: Boolean; virtual; abstract;
    function GetIn: Cardinal; virtual; abstract;
    function GetOut: Cardinal; virtual; abstract;
  public
    property Active: Boolean read GetActive;
    property Connected: Boolean read GetConnected;
    property InByt: Cardinal read GetIn;
    property OutByt: Cardinal read GetOut;
  published
    property PipeAccess: TPipeAccess read FPipeAccess write FPipeAccess;
    property Timeout: Cardinal read FTimeout write FTimeout;
    property OutBufSize: Integer read FOutBufSize write FOutBufSize;
    property InBufSize: Integer read FInBufSize write FInBufSize;
  end;

  TServerNamedPipe = class(TNamedPipe)
  private
    FCriticalSection: TRTLCriticalSection;
    FPipes: TList;
    FPipeName: AnsiString;
    FActive: Boolean;
    FOnReadData: TReadDataEvent;
    FOnDisconnect: TPipeEvent;
    FOnConnect: TPipeEvent;
    FOnException: TPipeExceptionEvent;
    FSecurityAttributes: TSecurityAttributes;
    FPSD: TSecurityDescriptor;
    WaitPipe: TServerPipe;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FMaxPipes: Integer;
    FPipeMode: TPipeMode;
    FReadMode: TPipeMode;
    function GetPipe(Index : Integer) : TServerPipe;
    procedure SetActive(const Value: Boolean);
    function GetCount: Integer;
  protected
    function Add : TServerPipe;
    procedure AddWaitPipe;
    function GetConnected: Boolean; override;
    function GetActive: Boolean; override;
    function GetIn: Cardinal; override;
    function GetOut: Cardinal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DisconectItem(aItem : TServerPipe);
    procedure AllDisconect;
    procedure RemoveItem(aItem : TServerPipe);
    procedure InsertItem(aItem : TServerPipe);
    procedure Write(var Buffer; BuffSize: DWord; var BytesWritten: DWord);
    property Count : Integer read GetCount;
    property Pipes[Index : Integer] : TServerPipe read GetPipe;
  published
    property Active: Boolean read GetActive write SetActive;
    property PipeName: string read FPipeName write FPipeName;
    property MaxPipes: Integer read FMaxPipes write FMaxPipes;
    property PipeMode: TPipeMode read FPipeMode write FPipeMode;
    property ReadMode: TPipeMode read FReadMode write FReadMode;
    property OnReadData: TReadDataEvent read FOnReadData write FOnReadData;
    property OnConnect: TPipeEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TPipeEvent read FOnDisconnect write FOnDisconnect;
    property OnException: TPipeExceptionEvent read FOnException write FOnException;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
  end;

  TClientNamedPipe = class(TNamedPipe)
  private
    FPipe: TClientPipe;
    FPipeName: string;
    FOnDisconnect: TPipeEvent;
    FOnConnect: TPipeEvent;
    FOnReadData: TReadDataEvent;
    FOnException: TPipeExceptionEvent;
    FAutoConnect: Boolean;
    FConnectInterv: Integer;
    procedure SetAutoConnect(const Value: Boolean);
    procedure SetPipeName(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Connect: Boolean;
    function Disconnect: Boolean;
    procedure Write(var Buffer; BuffSize: DWord; var BytesWritten: DWord);
    procedure WriteString(S: string);
    property Connected: Boolean read GetConnected;
    property Pipe: TClientPipe read FPipe;
    property Active: Boolean read GetActive;
  protected
    function GetConnected: Boolean; override;
    function GetActive: Boolean; override;
    function GetIn: Cardinal; override;
    function GetOut: Cardinal; override;
  published
    property PipeName: string read FPipeName write SetPipeName;
    property AutoConnect: Boolean read FAutoConnect write SetAutoConnect;
    property ConnectInterv: Integer read FConnectInterv write FConnectInterv;
    property OnReadData: TReadDataEvent read FOnReadData write FOnReadData;
    property OnConnect: TPipeEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TPipeEvent read FOnDisconnect write FOnDisconnect;
    property OnException: TPipeExceptionEvent read FOnException write FOnException;
  end;

implementation

function LastError: string;
var
  OutputMessage: PChar;
begin
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ALLOCATE_BUFFER,
    nil,
    GetLastError,
    0,
    @OutputMessage,
    0,
    nil);

  Result := string(OutputMessage);
end;

{ TAbstractPipe }

procedure TAbstractPipe.Connect;
begin
  try
    FInBytes:= 0;
    FOutBytes:= 0;
    FConnected:= True;
    if Assigned(FOnConnect) then FOnConnect(Self);
  except
    raise
  end;
end;

constructor TAbstractPipe.Create(aPipeName: AnsiString; aReadBufSize: Integer;
  aSecurityAttributes: TSecurityAttributes);
begin
  inherited Create(True);
  FReadBufSize:= aReadBufSize;
  FreeOnTerminate:= True;
  FEvent := CreateEvent(nil, True, True, nil);
  OL.hEvent := FEvent;
  FEventWrite := CreateEvent(nil, True, True, nil);
  OLWrite.hEvent := FEventWrite;
  FConnected := False;
  FPipeName := aPipeName;
  UniqueString(FPipeName);
  FSecurityAttributes := aSecurityAttributes;
  GetMem(FBuffer, FReadBufSize);
end;

destructor TAbstractPipe.Destroy;
begin
  CloseHandle(FEvent);
  CloseHandle(FEventWrite);
  FreeMem(FBuffer);
  SysUtils.FreeAndNil(Data);
  inherited;
end;

procedure TAbstractPipe.Disconnect;
begin
  try
    CloseHandlePipe;
    if FConnected then
    begin
      FConnected:= False;
      if Assigned(FOnDisconnect) then FOnDisconnect(Self);
    end;
  except
    raise
  end;
  FIsDisconnectPipe:= False;
end;

procedure TAbstractPipe.DoReadData(Buffer: Pointer; BuffSize: DWord);
begin
  if Assigned(FOnReadData) then FOnReadData(Self, FBuffer, BuffSize);
end;

procedure TAbstractPipe.Execute;
begin
  try
    try
      while not Stoped do ReadData;
    finally
      if hPipe <> 0 then Disconnect;
    end;
  except
    raise
  end;
end;

procedure TAbstractPipe.DisconnectPipe;
begin
  FIsDisconnectPipe:= True;
end;

procedure TAbstractPipe.ReadData;
var
  BytesRead: Cardinal;

  function _is_disconect: Boolean;
  var
    C: Cardinal;
  begin
    C:= GetLastError;
    Result:= FIsDisconnectPipe or (FConnected and (C > 0) and (C <> ERROR_IO_PENDING));
//      ((C = ERROR_BROKEN_PIPE) or (C = ERROR_PIPE_NOT_CONNECTED) or (C = ERROR_NETNAME_DELETED));
  end;

begin
  if not Stoped then
  try
    with OL do begin
      Internal := 0;                                     
      InternalHigh := 0;
      Offset := 0;
      OffsetHigh := 0;
    end;
    if not ReadFile(hPipe, FBuffer^, FReadBufSize, BytesRead, @OL) then
    begin
      if _is_disconect then
      begin
        Stop;
        Exit;
      end;
    end;
{
        if BytesRead > 0 then
        begin
          Inc(FInBytes, BytesRead);
          DoReadData(Buffer, BytesRead);
        end;
}
    if WaitForSingleObject(FEvent, INFINITE) = WAIT_OBJECT_0 then
    begin
      ResetEvent(FEvent);
      if _is_disconect then
        Stop
      else begin
        BytesRead:= 0;
        if GetOverlappedResult(hPipe, OL, BytesRead, True) then
        begin
          if (BytesRead > 0) and not _is_disconect then
          begin
            Inc(FInBytes, BytesRead);
            DoReadData(Buffer, BytesRead);
          end;
        end else
        begin
          if _is_disconect then Stop;
        end;
      end;
    end else begin
      if _is_disconect then Stop;
    end;
  except
    Stop;
    raise
  end;
end;

procedure TAbstractPipe.Write(var Buffer; BuffSize: Cardinal; var BytesWritten: Cardinal; WrTimeout: Cardinal=500);
var
  error: Boolean;
  E: EPipeError;
begin
  with OLWrite do begin
    Internal := 0;
    InternalHigh := 0;
    Offset := 0;
    OffsetHigh := 0;
  end;
  error:= WriteFile(hPipe, Buffer, BuffSize, BytesWritten, @OLWrite);
  if error then Exit;
  WaitForSingleObject(FEventWrite, WrTimeout);
  error:= not GetOverlappedResult(hPipe, OLWrite, BytesWritten, True);
  ResetEvent(FEventWrite);
  if error then
  begin
    E:= EPipeError.Create('Write named pipe error = ' +
      IntToStr(GetLastError) + '. ' + LastError);
    if Assigned(FOnException) then
      FOnException(Self, E)
    else
      raise E;
  end else
    Inc(FOutBytes, BytesWritten);
end;

procedure TServerPipe.Connect;
var
  Res: DWord;

begin
  try
    if not FConnected and not FIsDisconnectPipe then
    begin
      CreatePipe;
      ConnectNamedPipe(hPipe, @OL);
      Res := WaitForSingleObject(FEvent, INFINITE);
      ResetEvent(FEvent);
      FConnected := (Res <> WAIT_TIMEOUT){ and not FIsDisconnectPipe};
      if FConnected then
      begin
        MultiServer.InsertItem(Self);
        inherited;
      end;
    end;
  except
    raise
  end;
end;

procedure TServerPipe.CreatePipe;
const
  c_pipe_mode: array[TPipeMode] of DWord =
    (PIPE_TYPE_BYTE, PIPE_TYPE_MESSAGE);
    
  c_read_mode: array[TPipeMode] of DWord =
    (PIPE_READMODE_BYTE, PIPE_READMODE_MESSAGE);

  c_pipe_access: array[TPipeAccess] of DWord =
    (PIPE_ACCESS_DUPLEX, PIPE_ACCESS_OUTBOUND, PIPE_ACCESS_INBOUND);

begin
  try
    with FMultiServer do
    hPipe := CreateNamedPipe(
      PChar('\\.\Pipe\' + FPipeName),
      c_pipe_access[FPipeAccess] or FILE_FLAG_OVERLAPPED,
      c_pipe_mode[FPipeMode] or c_read_mode[FReadMode],
      PIPE_UNLIMITED_INSTANCES,
      OutBufSize, InBufSize, Timeout, @FSecurityAttributes);

    if hPipe = INVALID_HANDLE_VALUE then
      raise EPipeError.Create('Create named pipe error = ' +
        IntToStr(GetLastError) + '. ' + LastError);
  except
    raise
  end;
end;

procedure TServerPipe.Execute;
begin
  try
    Connect;
    if FConnected then
    begin
      MultiServer.AddWaitPipe;
      inherited;
    end;
  except
    on E: Exception do
    begin
      MultiServer.WaitPipe:= nil;
      if Assigned(FOnException) then FOnException(Self, E);
    end;
  end;
end;

function TServerPipe.Stoped: Boolean;
begin
  Result:= inherited Stoped or FIsDisconnectPipe;
end;

procedure TAbstractPipe.CloseHandlePipe;
begin
  if hPipe <> 0 then
  begin
    CancelIO(hPipe);
    CloseHandle(hPipe);
    hPipe:= 0;
  end;
end;

procedure TAbstractPipe.Stop;
begin
  Terminate;
end;

function TAbstractPipe.Stoped: Boolean;
begin
  Result:= Terminated;
end;

{ TClientPipe }

procedure TClientPipe.Connect;
begin
  if not FConnected and not FIsDisconnectPipe then
  try
    CreatePipe;
    if hPipe <> 0 then inherited;
  except
    if (hPipe = INVALID_HANDLE_VALUE) and not ClientNamedPipe.AutoConnect then raise
  end;
end;

procedure TClientPipe.CreatePipe;
const
  c_access: array[TPipeAccess] of Cardinal =
    (GENERIC_READ or GENERIC_WRITE, GENERIC_WRITE, GENERIC_READ);


begin
  try
    hPipe := CreateFile(PChar(FPipeName), c_access[ClientNamedPipe.PipeAccess],
                            FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING,
                            FILE_ATTRIBUTE_NORMAL or FILE_FlAG_OVERLAPPED, 0);
    if hPipe = INVALID_HANDLE_VALUE then
      raise EPipeError.Create('Create pipe file error = ' +
        IntToStr(GetLastError) + '. ' + LastError);
  except
    raise
  end;
end;

procedure TClientPipe.Execute;
begin
  try
    while not Terminated do
    begin
      if WaitNamedPipe(PAnsiChar(FPipeName), ClientNamedPipe.TimeOut) then
      begin
        Connect;
        try
          if FConnected then inherited;
        finally
          if FConnected then Disconnect;  // na wszelki wypadek je¿eli nie roz³¹czy siê wczeœniej
                                          // przez Disconect w inherited
          if not ClientNamedPipe.AutoConnect then
            Terminate
          else
            Sleep(ClientNamedPipe.ConnectInterv);
        end;
      end else
        Sleep(1);
    end;
  except
    on E: Exception do
    begin
      Disconnect;
      if Assigned(FOnException) then FOnException(Self, E);
    end;
  end;
end;

procedure TClientPipe.Stop;
begin
  FInBytes:= 0;
  FOutBytes:= 0;
  FIsDisconnectPipe:= True;
  if not ClientNamedPipe.AutoConnect then inherited;
end;

function TClientPipe.Stoped: Boolean;
begin
  Result:= inherited Stoped or FIsDisconnectPipe;
end;

{ TServerNamedPipe }

function TServerNamedPipe.Add: TServerPipe;
begin
  Result := TServerPipe.Create(FPipeName, FInBufSize, FSecurityAttributes);
  Result.FreeOnTerminate:= True;
  Result.FTimeout:= Timeout;
  Result.FMultiServer:= Self;
  Result.OnException:= FOnException;
  Result.OnConnect := FOnConnect;
  Result.OnDisconnect := FOnDisconnect;
  Result.OnReadData := FOnReadData;
  Result.Resume;
end;

constructor TServerNamedPipe.Create;
begin
  inherited;
  FMaxPipes:= 16;
  FOutBufSize:= 2048;
  FInBufSize:= 2048;
  FTimeout:= 500;
  FPipeMode:= pmMessage;
  FReadMode:= pmMessage;
  InitializeSecurityDescriptor(@FPSD, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@FPSD, True, nil, False);
  FSecurityAttributes.nLength := Sizeof(FSecurityAttributes);
  FSecurityAttributes.lpSecurityDescriptor := @FPSD;
  FSecurityAttributes.bInheritHandle := True;
  FPipes := TList.Create;
  InitializeCriticalSection(FCriticalSection);
end;

destructor TServerNamedPipe.Destroy;
begin
  if WaitPipe <> nil then RemoveItem(WaitPipe);
  Active:= False;
  FPipes.Free;
  DeleteCriticalSection(FCriticalSection);
  inherited;
end;

function TServerNamedPipe.GetPipe(Index: Integer): TServerPipe;
begin
  EnterCriticalSection(FCriticalSection);
  try
    Result := TServerPipe(FPipes[Index]);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TServerNamedPipe.InsertItem(aItem: TServerPipe);
begin
  if FPipes.IndexOf(aItem) < 0 then begin
    EnterCriticalSection(FCriticalSection);
    try
      FPipes.Add(aItem);
      aItem.FMultiServer := Self;
    finally
      LeaveCriticalSection(FCriticalSection);
    end;
  end;
end;

procedure TServerNamedPipe.RemoveItem(aItem: TServerPipe);
begin
  if FPipes.IndexOf(aItem) > -1 then begin
    EnterCriticalSection(FCriticalSection);
    try
      aItem.FMultiServer := nil;
      FPipes.Remove(aItem);
      if WaitPipe = nil then AddWaitPipe;
    finally
      LeaveCriticalSection(FCriticalSection);
    end;
  end;
end;

procedure TServerNamedPipe.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if not (csDesigning in ComponentState) then
    begin
      if Value then
      begin
        AddWaitPipe;
        if Assigned(FOnActivate) then FOnActivate(Self);
      end else begin
        if WaitPipe <> nil then WaitPipe.DisconnectPipe;
        AllDisconect;
        if Assigned(FOnDeactivate) then FOnDeactivate(Self);
      end;
    end;
    FActive := Value;
  end;
end;

procedure TServerNamedPipe.DisconectItem(aItem: TServerPipe);
begin
  aItem.DisconnectPipe;
  RemoveItem(aItem);
end;

procedure TServerNamedPipe.AllDisconect;
begin
  EnterCriticalSection(FCriticalSection);
  try
    while FPipes.Count > 0 do
    begin
      DisconectItem(FPipes[0]);
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TServerNamedPipe.AddWaitPipe;
begin
  if FPipes.Count < FMaxPipes then
    WaitPipe:= Add
  else
    WaitPipe:= nil;
end;

function TServerNamedPipe.GetCount: Integer;
begin
  Result := FPipes.Count;
end;

procedure TServerNamedPipe.Write(var Buffer; BuffSize: DWord;
  var BytesWritten: DWord);
var i: Integer;
begin
  EnterCriticalSection(FCriticalSection);
  try
    for i:= 0 to FPipes.Count - 1 do
      TServerPipe(FPipes[i]).Write(Buffer, BuffSize, BytesWritten, Timeout);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TServerNamedPipe.GetActive: Boolean;
begin
  Result:= FActive;
end;

function TServerNamedPipe.GetConnected: Boolean;
var i: Integer;
begin
  Result:= False;
  for i:= 0 to FPipes.Count - 1 do if not Result then Result:= True;
end;

function TServerNamedPipe.GetIn: Cardinal;
var i: Integer;
begin
  Result:= 0;
  for i:= 0 to FPipes.Count - 1 do Result:= Result + Pipes[i].InBytes;
end;

function TServerNamedPipe.GetOut: CArdinal;
var i: Integer;
begin
  Result:= 0;
  for i:= 0 to FPipes.Count - 1 do Result:= Result + Pipes[i].OutBytes;
end;

{ TClientNamedPipe }

function TClientNamedPipe.Connect: Boolean;
var
  SA: TSecurityAttributes;
  PSD: TSecurityDescriptor;

begin
  Result:= True;
  if (FPipeName <> '') and (FAutoConnect or WaitNamedPipe(PAnsiChar(FPipeName), FTimeOut)) then
  begin
    InitializeSecurityDescriptor(@PSD, SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(@PSD, True, nil, False);
    SA.nLength := Sizeof(SA);
    SA.lpSecurityDescriptor := @PSD;
    SA.bInheritHandle := True;
    FPipe:= TClientPipe.Create(FPipeName, FInBufSize, SA);
    FPipe.FreeOnTerminate:= True;
    FPipe.FTimeout:= Timeout;
    FPipe.FClientNamedPipe:= Self;
    FPipe.OnException:= FOnException;
    FPipe.OnConnect:= FOnConnect;
    FPipe.OnDisconnect:= FOnDisconnect;
    FPipe.OnReadData:= FOnReadData;
    FPipe.Resume;
    Result:= True;
  end;
end;

constructor TClientNamedPipe.Create(AOwner: TComponent);
begin
  inherited;
  FInBufSize:= 2048;
  FOutBufSize:= 2048;
  FTimeOut:= 500;
  FConnectInterv:= 1;
end;

destructor TClientNamedPipe.Destroy;
begin
//  AutoConnect:= False;
  Disconnect;
  inherited;
end;

function TClientNamedPipe.Disconnect: Boolean;
begin
  if Connected then
  begin
    FPipe.DisconnectPipe;
    if not AutoConnect then FPipe:= nil;
    Result:= True;
  end else
    Result:= False;
end;

function TClientNamedPipe.GetActive: Boolean;
begin
  Result:= AutoConnect;
end;

function TClientNamedPipe.GetConnected: Boolean;
begin
  if FPipe <> nil then
    Result:= FPipe.Connected
  else
    Result:= False;
end;

function TClientNamedPipe.GetIn: Cardinal;
begin
  if FPipe <> nil then
    Result:= FPipe.InBytes
  else
    Result:= 0;
end;

function TClientNamedPipe.GetOut: Cardinal;
begin
  if FPipe <> nil then
    Result:= FPipe.OutBytes
  else
    Result:= 0;
end;

procedure TClientNamedPipe.SetAutoConnect(const Value: Boolean);
begin
  if Value <> FAutoConnect then
  begin
    if not (csDesigning in ComponentState) then
    if Value then
    begin
      FAutoConnect:= True;
      Connect;
    end else begin
      Disconnect;
      FPipe.Terminate;
      WaitForSingleObject(FPipe.ThreadID, Timeout);
      FAutoConnect:= False;
    end;
  end;
end;

procedure TClientNamedPipe.SetPipeName(const Value: string);
begin
  if FPipeName <> Value then
  begin
    FPipeName := Value;
    if FAutoConnect then
    begin
      AutoConnect:= False;
      AutoConnect:= True;
    end;
  end;
end;

procedure TClientNamedPipe.Write(var Buffer; BuffSize: DWord;
  var BytesWritten: DWord);
begin
  if FPipe <> nil then
  begin
    FPipe.Write(Buffer, BuffSize, BytesWritten, Timeout);
  end;
end;

procedure TClientNamedPipe.WriteString(S: string);
var L: Cardinal;
begin
  if FPipe <> nil then
  begin
    FPipe.Write(PChar(S)^, Length(S) + 1, L, Timeout);
  end;
end;

procedure TServerPipe.Disconnect;
begin
  try
    if (hPipe <> 0) then DisconnectNamedPipe(hPipe);
    inherited;
    if FMultiServer <> nil then FMultiServer.RemoveItem(Self);
  except
    raise
  end;
end;

procedure TServerPipe.DoReadData(Buffer: Pointer; BuffSize: DWord);
begin
  if Assigned(FOnReadData) then
  begin
//    EnterCriticalSection(FMultiServer.FCriticalSection);
    try
      FOnReadData(Self, Buffer, BuffSize);
    finally
//      LeaveCriticalSection(FMultiServer.FCriticalSection);
    end;
  end;
end;

end.


