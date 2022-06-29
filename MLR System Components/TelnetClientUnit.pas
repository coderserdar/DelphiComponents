unit TelnetClientUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ScktComp, StdCtrls, SyncObjs;

{
  Telnet codes:
      NAME               CODE              MEANING
      SE                  240    End of subnegotiation parameters.
      NOP                 241    No operation.
      Data Mark           242    The data stream portion of a Synch.
                                 This should always be accompanied
                                 by a TCP Urgent notification.
      Break               243    NVT character BRK.
      Interrupt Process   244    The function IP.
      Abort output        245    The function AO.
      Are You There       246    The function AYT.
      Erase character     247    The function EC.
      Erase Line          248    The function EL.
      Go ahead            249    The GA signal.
      SB                  250    Indicates that what follows is
                                 subnegotiation of the indicated
                                 option.
      WILL (option code)  251    Indicates the desire to begin
                                 performing, or confirmation that
                                 you are now performing, the
                                 indicated option.
      WON'T (option code) 252    Indicates the refusal to perform,
                                 or continue performing, the
                                 indicated option.
      DO (option code)    253    Indicates the request that the
                                 other party perform, or
                                 confirmation that you are expecting
                                 the other party to perform, the
                                 indicated option.
      DON'T (option code) 254    Indicates the demand that the
                                 other party stop performing,
                                 or confirmation that you are no
                                 longer expecting the other party
                                 to perform, the indicated option.
      IAC                 255    Data Byte 255.
}
const
  TCSE                = #240;
  TCNOP               = #241;
  TCDataMark          = #242;
  TCBreak             = #243;
  TCInterruptProcess  = #244;
  TCAbortOutput       = #245;
  TCAreYouThere       = #246;
  TCEraseCharacter    = #247;
  TCGoAhead           = #249;
  TCSB                = #250;
  TCWill              = #251;
  TCWont              = #252;
  TCDo                = #253;
  TCDont              = #254;
  TCIAC               = #255;

  TelnetPort          = 23;

type
  TLineEvent = procedure (Sender :TObject; Line :string) of object;
  TTelnetClient = class(TComponent)
  private
    FActive: Boolean;
    FHost: string;
    FAddress: string;
    FNVTPrinter: TCustomMemo;
    FPort: Integer;
    FOnReceivedLine: TLineEvent;
    FOnCommand: TLineEvent;
    FAsynchronous: Boolean;
    FOnDisconnect: TNotifyEvent;
    FOnConnect: TNotifyEvent;
    FBuildingList :TStrings;
    FIsBuildingList :Boolean;
    FLineWaitMs: Cardinal;
    FOkWaitMs: Cardinal;
    procedure SetActive(const Value: Boolean);
    procedure SetAddress(const Value: string);
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: Integer);
    { Private declarations }
  protected
    { Protected declarations }
    FSocket :TClientSocket;
    procedure DoReceivedText(Line: string);
    procedure Notification(AComponent :TComponent; Operation :TOperation); override;
    procedure EnsureActive(const ProcedureName :string);
    procedure SocketOnRead(Sender :TObject; Socket :TCustomWinSocket);
    procedure SocketConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure SocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
  public
    { Public declarations }
    constructor Create(AOwner :TComponent); override;
    destructor Destroy; override;
    procedure SendCommand(const Command :string);
    procedure Quit;
    procedure RetrieveList(Strings :TStrings);
    procedure CommandAnswer(const Command: string; Answer: TStrings);
    procedure DiscardResponses;
    function  GetReply: string;
  published
    { Published declarations }
    property Active :Boolean read FActive write SetActive;
    property Address :string read FAddress write SetAddress;
    property Asynchronous :Boolean read FAsynchronous write FAsynchronous default True;
    property Host :string read FHost write SetHost;
    property Port :Integer read FPort write SetPort default TelnetPort;
    property NVTPrinter :TCustomMemo read FNVTPrinter write FNVTPrinter;
    property OkWaitMs: Cardinal read FOkWaitMs write FOkWaitMs default 1000 * 15;
    property LineWaitMs: Cardinal read FLineWaitMs write FLineWaitMs default 1000 * 5;
    property OnReceivedLine :TLineEvent read FOnReceivedLine write FOnReceivedLine;
    property OnCommand :TLineEvent read FOnCommand write FOnCommand;
    property OnConnect :TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect :TNotifyEvent read FOnDisconnect write FOnDisconnect;
  end;

function SocketErrorDescription(const ErrorCode: Longint): string;

procedure Register;

implementation

uses
  WinSock;

procedure Register;
begin
  RegisterComponents('MLR Sysco', [TTelnetClient]);
end;

function SocketErrorDescription(const ErrorCode: Longint): string;
begin
  case ErrorCode of
    WSAEINTR:
      Result := 'The (blocking) call was canceled through WSACancelBlockingCall.';
    WSAEBADF:
      Result := 'Bad format';
    WSAEACCES:
      Result := 'Attempt to connect datagram socket to broadcast address failed because setsockopt SO_BROADCAST is not enabled.';
    WSAEFAULT:
      Result := 'The parameters are invalid';
    WSAEINVAL:
      Result := 'Invalid state';
    WSAEMFILE:
      Result := 'The queue is nonempty upon entry to accept and there are no descriptors available.';
    WSAEWOULDBLOCK:
      Result := 'The socket is marked as nonblocking and no connections are present to be accepted.';
    WSAEINPROGRESS:
      Result := 'A blocking Windows Sockets 1.1 call is in progress, or the service provider is still processing a callback function.';
    WSAEALREADY:
      Result := 'A nonblocking connect call is in progress on the specified socket.';
    WSAENOTSOCK:
      Result := 'The descriptor is not a socket.';
    WSAEDESTADDRREQ:
      Result := 'Coso';
    WSAEMSGSIZE:
      Result := 'Coso';
    WSAEPROTOTYPE:
      Result := 'Coso';
    WSAENOPROTOOPT:
      Result := 'Coso';
    WSAEPROTONOSUPPORT:
      Result := 'Coso';
    WSAESOCKTNOSUPPORT:
      Result := 'Coso';
    WSAEOPNOTSUPP:
      Result := 'The referenced socket is not a type that supports connection-oriented service.';
    WSAEPFNOSUPPORT:
      Result := 'Coso';
    WSAEAFNOSUPPORT:
      Result := 'Coso';
    WSAEADDRINUSE:
      Result := 'The specified address is already in use.';
    WSAEADDRNOTAVAIL:
      Result := 'The specified address is not available from the local machine.';
    WSAENETDOWN:
      Result := 'The network subsystem has failed.';
    WSAENETUNREACH:
      Result := 'The network cannot be reached from this host at this time.';
    WSAENETRESET:
      Result := 'Coso';
    WSAECONNABORTED:
      Result := 'The connection was aborted.';
    WSAECONNRESET:
      Result := 'The connection was reset.';
    WSAENOBUFS:
      Result := 'No buffer space is available.';
    WSAEISCONN:
      Result := 'The socket is already connected (connection-oriented sockets only).';
    WSAENOTCONN:
      Result := 'Coso';
    WSAESHUTDOWN:
      Result := 'Coso';
    WSAETOOMANYREFS:
      Result := 'Coso';
    WSAETIMEDOUT:
      Result := 'Attempt to connect timed out without establishing a connection.';
    WSAECONNREFUSED:
      Result := 'The attempt to connect was forcefully rejected.';
    WSAELOOP:
      Result := 'Coso';
    WSAENAMETOOLONG:
      Result := 'Coso';
    WSAEHOSTDOWN:
      Result := 'The host is down.';
    WSAEHOSTUNREACH:
      Result := 'The host is unreachable.';
    WSAENOTEMPTY:
      Result := 'Coso';
    WSAEPROCLIM:
      Result := 'Coso';
    WSAEUSERS:
      Result := 'Coso';
    WSAEDQUOT:
      Result := 'Coso';
    WSAESTALE:
      Result := 'Coso';
    WSAEREMOTE:
      Result := 'Coso';
    WSASYSNOTREADY:
      Result := 'Coso';
    WSAVERNOTSUPPORTED:
      Result := 'The requested version is not supported';
    WSANOTINITIALISED:
      Result := 'A successful WSAStartup should have occured before.';
    WSAEDISCON:
      Result := 'Coso';
    else
      Result := 'Unknown socket error';
  end;
end;

{ TTelnetClient }

constructor TTelnetClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPort := TelnetPort;
  FAsynchronous := True;
  FOkWaitMs := 1000 * 15;
  FLineWaitMs := 1000 * 5;
end;

destructor TTelnetClient.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

procedure TTelnetClient.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FNVTPrinter) and (Operation = opRemove) then
    FNVTPrinter := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TTelnetClient.SetActive(const Value: Boolean);
begin
  if Value = FActive then exit;
  if Value then begin
    FSocket := TClientSocket.Create(nil);
    if FAsynchronous then
      FSocket.ClientType := ctNonBlocking
    else
      FSocket.ClientType := ctBlocking;
    FSocket.Port := FPort;
    FSocket.Host := FHost;
    FSocket.Address := FAddress;
    FSocket.OnRead := SocketOnRead;
    FSocket.OnDisconnect := SocketDisconnect;
    FSocket.OnConnect := SocketConnect;
    FSocket.Active := True;
  end else begin
    if Assigned(FSocket) then begin
      FSocket.Active := False;
      FSocket.Free;
      FSocket := nil;
    end;
  end;
  FActive := Value;
end;

procedure TTelnetClient.SocketOnRead(Sender: TObject;
  Socket: TCustomWinSocket);
var Line :string;
begin
  Line := Socket.ReceiveText;
  DoReceivedText(Line);
end;

procedure TTelnetClient.SetAddress(const Value: string);
begin
  if Assigned(FSocket) then
    FSocket.Address := Value;
  FAddress := Value;
end;

procedure TTelnetClient.SetHost(const Value: string);
begin
  if Assigned(FSocket) then
    FSocket.Host := Value;
  FHost := Value;
end;

procedure TTelnetClient.SetPort(const Value: Integer);
begin
  FPort := Value;
end;

procedure TTelnetClient.SendCommand(const Command: string);
var
  Stream: TWinSocketStream;
  Comm: string;
  Retries: Integer;
  BytesWritten: Integer;
begin
  Comm := Command + #10;
  EnsureActive('SendCommand');
  if FAsynchronous then
    FSocket.Socket.SendText(Comm)
  else begin
    Stream := TWinSocketStream.Create(FSocket.Socket, LineWaitMs);
    try
      Stream.WaitForData(LineWaitMs);
      Retries := 5;
      repeat
        BytesWritten := Stream.Write(PChar(Comm)^, Length(Comm));
        Dec(Retries);
      until (BytesWritten > 0) or (Retries = 0);
      if Retries = 0 then
        raise Exception.Create('Socket timed out when sending command:'#13 + Command);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TTelnetClient.EnsureActive(const ProcedureName: string);
begin
  if FActive then exit;
  raise Exception.Create('Cannot execute ' + ProcedureName + ' on telnet ' +
    'client ' + Name + ' when the connection is not active.');
end;

procedure TTelnetClient.Quit;
begin
  EnsureActive('Quit');
  SendCommand('Quit');
  Active := False;
end;

procedure TTelnetClient.SocketConnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  if Assigned(FOnConnect) then
    FOnConnect(Self);
end;

procedure TTelnetClient.SocketDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self);
end;

procedure TTelnetClient.RetrieveList(Strings: TStrings);
const
  BufferSize = 1024;
var
  Ms: Cardinal;
  SocketStream: TWinSocketStream;
  Buffer: string;
  BytesRead: Integer;
begin
  FBuildingList := Strings;
  FIsBuildingList := True;
  if FAsynchronous then begin
    Ms := GetTickCount;
    while (Ms + OkWaitMs > GetTickCount) and FIsBuildingList do
      Application.ProcessMessages;
  end else begin
    SocketStream := TWinSocketStream.Create(FSocket.Socket, OkWaitMs);
    try
      {
      if not SocketStream.WaitForData(OkWaitMs) then
        raise Exception.Create('Socket timed out when retriving list');
      }
      repeat
        SetLength(Buffer, BufferSize);
        BytesRead := SocketStream.Read(PChar(Buffer)^, BufferSize);
        if BytesRead > 0 then begin
          SetLength(Buffer, BytesRead);
          DoReceivedText(Buffer);
        end;
      until (BytesRead = 0) or (not FIsBuildingList);
    finally
      SocketStream.Free;
    end;
  end;
  FIsBuildingList := False;
end;

procedure TTelnetClient.CommandAnswer(const Command: string;
  Answer: TStrings);
begin
  SendCommand(Command);
  if Assigned(Answer) then begin
    Answer.Clear;
    RetrieveList(Answer);
  end;
end;

procedure TTelnetClient.DiscardResponses;
const
  BufferSize = 1024;
var
  Ms: Cardinal;
  SocketStream: TWinSocketStream;
  Buffer: string;
  BytesRead: Integer;
begin
  if FAsynchronous then begin
    Ms := GetTickCount;
    while Ms + LineWaitMs > GetTickCount do
      Application.ProcessMessages;
  end else begin
    SocketStream := TWinSocketStream.Create(FSocket.Socket, LineWaitMs);
    try
      SocketStream.WaitForData(LineWaitMs);
      repeat
        SetLength(Buffer, BufferSize);
        BytesRead := SocketStream.Read(PChar(Buffer)^, BufferSize);
        if BytesRead > 0 then begin
          SetLength(Buffer, BytesRead);
          DoReceivedText(Buffer);
        end;
      until (BytesRead = 0) or (Buffer = 'OK'#$D#$A);
    finally
      SocketStream.Free;
    end;
  end;
end;

function TTelnetClient.GetReply: string;
var
  Answer: TStringList;
begin
  Answer := TStringList.Create;
  try
    RetrieveList(Answer);
    Result := Answer.Text + 'OK'#$0A#$0D;
  finally
    Answer.Free;
  end;
end;

procedure TTelnetClient.DoReceivedText(Line: string);
var
  Command :string;
  LineLength :Integer;
  P :PChar;
  procedure AddLines(const s: string; Lines: TStrings);
  var P:PChar; Line: string; i: Integer;
  begin
    P := PChar(s);
    i := Length(s);
    Line := '';
    while i > 0 do begin
      if P^ = #13 then begin
        Lines.Add(Line);
        Line := '';
      end else if P^ <> #10 then
        Line := Line + P^;
      Inc(P);
      Dec(i);
    end;
    if Line <> '' then
      Lines.Add(Line);
  end;
begin
  LineLength := Length(Line);
  if FIsBuildingList then begin
    AddLines(Line, FBuildingList);
    if (LineLength >= 4) and (Copy(Line, LineLength - 3, 4) = 'OK'#$D#$A) then begin
      FIsBuildingList := False;
      FBuildingList.Delete(FBuildingList.Count - 1);
    end;
  end else begin
    if LineLength = 0 then exit;
    P := PChar(Line);
    Command := '';
    while P^ <> #0 do begin
      if P^ = TCIAC then begin
        if (P + 1)^ <> TCIAC then
          Command := Command + P^
        else
          Inc(P);
      end;
      Inc(P);
    end;
    if Command <> '' then begin
      if Assigned(FOnCommand) then
        FOnCommand(Self, Command);
    end else begin
      if Assigned(FNVTPrinter) then
        FNVTPrinter.Lines.Add(Line);
      if Assigned(FOnReceivedLine) then
        FOnReceivedLine(Self, Line);
    end;
  end;
end;


end.

