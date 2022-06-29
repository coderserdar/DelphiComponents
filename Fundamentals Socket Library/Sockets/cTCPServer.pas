{$INCLUDE ..\cDefines.inc}
unit cTCPServer;

{                                                                              }
{                         TCP Server Component v3.08                           }
{                                                                              }
{        This unit is copyright © 2000-2003 by David Butler (david@e.co.za)    }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                  Its original file name is cTCPServer.pas                    }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{                                                                              }
{ Revision history:                                                            }
{   27/11/2000  0.01  cInternetServer spawned from cInternetStandards.         }
{                     Refactored into AInternetServer, ASynchronousServer and  }
{                     added AAsynchronousServer.                               }
{   25/01/2001  0.02  Refactored unit. Renamed to cSocketServer.               }
{   25/03/2001  0.03  Removed support for Delphi's sockets.                    }
{                     Francois Piette's TWSocket class is required.            }
{   15/12/2001  1.04  Removed support for TWSocket, now uses cSockets unit.    }
{   11/02/2002  2.05  Refactored for Fundamentals 2.                           }
{   16/02/2002  2.06  Added ATCPServer.GetReadRate/GetWriteRate and            }
{                     ATCPServer.ThrottleClientRead/ThrottleClientWrite.       }
{   04/09/2002  3.07  Refactored for Fundamentals 3.                           }
{                     Renamed unit cTCPServer.                                 }
{   22/06/2003  3.08  Fixed bug in TCPServer.OnDataAvailable.                  }
{                                                                              }

interface

uses
  { Delphi }
  WinSock,
  SysUtils,
  Classes,

  { Fundamentals }
  cStrings,
  cStreams,
  cLog,
  cThreads,
  cSocketsTCP,
  cTCPStream,
  cSocketsTCPServer;



{                                                                              }
{ ATCPServer                                                                   }
{   Base class for TCP servers.                                                }
{                                                                              }
type
  { ATCPServer                                                                 }
  TTCPServerClient = class;
  CTCPServerClient = class of TTCPServerClient;
  ATCPServer = class;
  ATCPServerEvent = procedure (Sender: ATCPServer) of object;
  ATCPServerLogEvent = procedure (Sender: ATCPServer; Msg : String) of object;
  TTCPServerClientEvent = procedure (Sender: TTCPServerClient) of object;
  TTCPServerCreateClientEvent = function (Sender: ATCPServer) : TTCPServerClient of object;
  TCPServerClientArray = Array of TTCPServerClient;
  TTCPServerMode = (smManualAccept,         // User calls Accept in OnConnectionAvailable
                    smAcceptClient,         // Automatically Accept a client
                    smAcceptClientThread);  // Automatically Accept a client in a seperate thread
  ATCPServer = class(TComponent)
  protected
    FOnLog                   : ATCPServerLogEvent;
    FLogTo                   : TLog;
    FServerMode              : TTCPServerMode;
    FListenPort              : String;
    FTimeOut                 : Integer;
    FOnActive                : ATCPServerEvent;
    FOnInactive              : ATCPServerEvent;
    FOnConnectionAvailable   : ATCPServerEvent;
    FOnCreateClient          : TTCPServerCreateClientEvent;
    FOnClientActive          : TTCPServerClientEvent;
    FOnClientInactive        : TTCPServerClientEvent;
    FClientClass             : CTCPServerClient;
    FOnDataAvailable         : TTCPServerClientEvent;
    FOnThreadRun             : TTCPServerClientEvent;
    FMaxClients              : Integer;
    FOnClientRemoved         : TTCPServerClientEvent;
    FThrottleClientRead      : Boolean;
    FThrottleClientReadRate  : Integer;
    FThrottleClientWrite     : Boolean;
    FThrottleClientWriteRate : Integer;

    FActive        : Boolean;
    FSocket        : TTCPServerSocket;
    FClients       : TCPServerClientArray;
    FAcceptPending : Boolean;
    FStartTime     : TDateTime;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure Init; virtual;
    procedure RaiseError(const Msg: String); virtual;
    procedure RaiseAndLogError(const Msg: String);
    procedure Log(const LogClass: TLogClass; const Msg: String);

    procedure SetListenPort(const ListenPort: String);
    function  GetLocalHost: String;
    procedure SetLocalHost(const LocalHost: String);
    function  GetMaxBacklog: Integer;
    procedure SetMaxBacklog(const MaxBacklog: Integer);
    procedure SetServerMode(const ServerMode: TTCPServerMode);

    procedure SetClientSocketProperties;
    procedure SetThrottleClientRead(const ThrottleClientRead: Boolean);
    procedure SetThrottleClientWrite(const ThrottleClientWrite: Boolean);
    procedure SetThrottleClientReadRate(const ThrottleClientReadRate: Integer);
    procedure SetThrottleClientWriteRate(const ThrottleClientWriteRate: Integer);

    procedure TriggerActive;
    procedure TriggerInactive;

    procedure TriggerClientRemoved(const Client: TTCPServerClient);

    procedure Start; virtual;
    procedure Stop; virtual;
    procedure SetActive(const Active: Boolean);
    function  GetUpTime: TDateTime;
    function  GetUpTimeStr: String;

    procedure RemoveClientByIndex(const Idx: Integer);
    procedure RemoveClient(const Client: TTCPServerClient);
    procedure TriggerClientActive(const Client: TTCPServerClient);
    procedure TriggerClientInactive(const Client: TTCPServerClient);

    procedure TerminateClients;

    procedure TriggerConnectionAvailable; virtual;
    procedure OnServerConnectionAvailable(Sender: TTCPServerSocket);
    procedure AcceptClient;
    procedure AcceptClientThread;
    function  CreateClient: TTCPServerClient; virtual;

    procedure TriggerThreadRun(const Client: TTCPServerClient); virtual;

    function  GetClientCount: Integer;
    function  GetClientByIndex(const Idx: Integer): TTCPServerClient;

    procedure OnClientSocketDataAvailable(Sender: ATCPClientSocket);
    procedure OnClientSocketClose(Sender: ATCPClientSocket);

    property  OnCreateClient: TTCPServerCreateClientEvent read FOnCreateClient write FOnCreateClient;
    property  OnClientActive: TTCPServerClientEvent read FOnClientActive write FOnClientActive;
    property  OnClientInactive: TTCPServerClientEvent read FOnClientInactive write FOnClientInactive;
    property  OnClientRemoved: TTCPServerClientEvent read FOnClientRemoved write FOnClientRemoved;
    property  OnDataAvailable: TTCPServerClientEvent read FOnDataAvailable write FOnDataAvailable;
    property  OnThreadRun: TTCPServerClientEvent read FOnThreadRun write FOnThreadRun;

  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateEx(const ListenPort: String; const TimeOut: Integer;
                const ServerMode: TTCPServerMode; const ClientClass: CTCPServerClient);
    destructor Destroy; override;

    property  LogTo: TLog read FLogTo write FLogTo;
    property  OnLog: ATCPServerLogEvent read FOnLog write FOnLog;

    property  ServerMode: TTCPServerMode read FServerMode write SetServerMode default smAcceptClient;

    function  LocalHostName: String;
    property  LocalHost: String read GetLocalHost write SetLocalHost;
    property  ListenPort: String read FListenPort write SetListenPort;
    property  MaxBacklog: Integer read GetMaxBacklog write SetMaxBacklog default DefaultBacklog;
    property  TimeOut: Integer read FTimeOut write FTimeOut default DefaultSocketStreamTimeOut;
    property  MaxClients: Integer read FMaxClients write FMaxClients default -1;

    property  Active: Boolean read FActive write SetActive default False;
    property  OnActive: ATCPServerEvent read FOnActive write FOnActive;
    property  OnInactive: ATCPServerEvent read FOnInactive write FOnInactive;
    property  StartTime: TDateTime read FStartTime;
    property  UpTime: TDateTime read GetUpTime;
    property  UpTimeStr: String read GetUpTimeStr;

    property  ThrottleClientRead: Boolean read FThrottleClientRead write SetThrottleClientRead default False;
    property  ThrottleClientReadRate: Integer read FThrottleClientReadRate write SetThrottleClientReadRate default 0;
    property  ThrottleClientWrite: Boolean read FThrottleClientWrite write SetThrottleClientWrite default False;
    property  ThrottleClientWriteRate: Integer read FThrottleClientWriteRate write SetThrottleClientWriteRate default 0;
    function  GetReadRate: Integer;
    function  GetWriteRate: Integer;

    function  Accept: TTCPServerClient;

    property  OnConnectionAvailable: ATCPServerEvent read FOnConnectionAvailable write FOnConnectionAvailable;

    property  Socket: TTCPServerSocket read FSocket;
    property  Client[const Idx: Integer]: TTCPServerClient read GetClientByIndex;
    property  ClientCount: Integer read GetClientCount;

    function  GetClientIndex(const Client: TTCPServerClient): Integer;
    function  GetClientIndexByStream(const Stream: AStream): Integer;
    function  GetClientIndexBySocket(const Socket: ATCPClientSocket): Integer;
    function  HasClient(const Client: TTCPServerClient): Boolean;
  end;
  ETCPServer = class(Exception);

  { TTCPServerClient                                                           }
  TTCPServerClientSocket = class(ATCPClientSocket);
  TTCPServerClient = class
  protected
    FWinSocket  : TSocket;
    FAddress    : TSockAddr;
    FServer     : ATCPServer;
    FStream     : TSocketStream;
    FSocket     : TTCPServerClientSocket;
    FTerminated : Boolean;
    FThread     : TThreadEx;
    FErrorMsg   : String;
    FData       : Pointer;

    procedure Init; virtual;
    procedure Log(const LogClass: TLogClass; const Msg: String); virtual;

    procedure SetSocketProperties;
    function  GetStream: TSocketStream;

    procedure ThreadRun; virtual;

    procedure ClientError(const Msg: String); virtual;
    procedure ClientActive; virtual;
    procedure ClientInactive; virtual;

  public
    constructor Create(const Server: ATCPServer); virtual;
    destructor Destroy; override;

    property  Server: ATCPServer read FServer;
    property  Socket: TTCPServerClientSocket read FSocket;
    function  GetSocket: TTCPServerClientSocket;
    property  Stream: TSocketStream read GetStream;
    property  Thread: TThreadEx read FThread;
    property  ErrorMsg: String read FErrorMsg;

    property  Data: Pointer read FData write FData; // user-defined data

    procedure Terminate; virtual;
    property  Terminated: Boolean read FTerminated;

    function  ReadAvailable: String;
    function  PeekStr(const Count: Integer): String;
    procedure WriteStr(const S: String);
    function  ReadEx(const MinCount, MaxCount: Integer; const Delimiter: String;
              var Buf: String; const Peek: Boolean = False): Boolean;
    function  StreamReadLine(var S: String): Boolean;
    function  SocketReadLine(var Buf: String; const Delimiter: String = CRLF): Boolean;

    procedure Synchronize(const Method: TThreadMethod);
  end;



{                                                                              }
{ TfndTCPServer                                                                }
{                                                                              }
type
  TfndTCPServer = class(ATCPServer)
  published
    property  LogTo;
    property  OnLog;

    property  ServerMode;

    property  LocalHost;
    property  ListenPort;
    property  MaxBacklog;
    property  TimeOut;
    property  MaxClients;

    property  ThrottleClientRead;
    property  ThrottleClientReadRate;
    property  ThrottleClientWrite;
    property  ThrottleClientWriteRate;

    property  OnConnectionAvailable;
    property  OnDataAvailable;

    property  OnCreateClient;
    property  OnClientRemoved;
    property  OnClientActive;
    property  OnClientInactive;

    property  OnThreadRun;

    property  Active;
    property  OnActive;
    property  OnInactive;
  end;



implementation

uses
  { Delphi }
  Messages,
  Windows,

  { Fundamentals }
  cUtils,
  cDateTime;



{                                                                              }
{ TTCPServerClientThread                                                       }
{                                                                              }
type
  TTCPServerClientThread = class(TThreadEx)
  protected
    FServer    : ATCPServer;
    FClient    : TTCPServerClient;
    FLogClass  : TLogClass;
    FLogMsg    : String;

    procedure NotifyLog;
    procedure NotifyClientActive;
    procedure NotifyClientInactive;

    procedure Log(const LogClass: TLogClass; const Msg: String);

    procedure InitRun(const Server: ATCPServer; const Client: TTCPServerClient);
    procedure Execute; override;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Terminate; override;
  end;

constructor TTCPServerClientThread.Create;
begin
  inherited Create(True);
end;

destructor TTCPServerClientThread.Destroy;
begin
  if Assigned(FClient) and (FClient.FThread = self) then
    FClient.FThread := nil;
  inherited Destroy;
end;

procedure TTCPServerClientThread.InitRun(const Server: ATCPServer;
    const Client: TTCPServerClient);
begin
  Assert(Assigned(Server), 'Assigned(Server)');
  Assert(Assigned(Client), 'Assigned(Client)');
  FServer := Server;
  FClient := Client;
  FClient.FSocket := nil;
end;

procedure TTCPServerClientThread.NotifyClientActive;
begin
  FServer.TriggerClientActive(FClient);
end;

procedure TTCPServerClientThread.NotifyClientInactive;
begin
  if not Assigned(FClient) then
    exit;
  if not Terminated and not FClient.Terminated then
    FServer.TriggerClientInactive(FClient);
  FServer := nil;
  FreeAndNil(FClient);
end;

procedure TTCPServerClientThread.NotifyLog;
begin
  Assert(Assigned(FServer), 'Assigned(FServer)');
  FServer.Log(FLogClass, FLogMsg);
end;

procedure TTCPServerClientThread.Log(const LogClass: TLogClass; const Msg: String);
begin
  FLogClass := LogClass;
  FLogMsg := Msg;
  Synchronize(NotifyLog);
end;

procedure TTCPServerClientThread.Execute;
begin
  try
    if Terminated or not Assigned(FClient) or FClient.Terminated or not Assigned(FServer) then
      exit;
    // Socket window must be created in thread to have messages posted to this thread
    try
      FClient.FStream := TSocketStream.Create(FClient.GetSocket, False,
          smBlockWaitMessage, FServer.FTimeOut);
    except
      on E: Exception do
        begin
          Log(lcError, 'Socket creation error: ' + E.Message);
          exit;
        end;
    end;
    if Terminated or not Assigned(FClient) or FClient.Terminated or FClient.FSocket.Terminated then
      exit;
    // Notify Active
    try
      Synchronize(NotifyClientActive);
    except
      on E: Exception do
        begin
          Log(lcError, 'Client activate error: ' + E.Message);
          exit;
        end;
    end;
    if Terminated or not Assigned(FClient) or FClient.Terminated or FClient.FSocket.Terminated then
      exit;
    // Run
    try
      FClient.ThreadRun;
    except
      on E: Exception do
        begin
          Log(lcError, 'Client execute error: ' + E.Message);
          exit;
        end;
    end;
  finally
    // Notify Inactive
    try
      Synchronize(NotifyClientInactive);
    except
      on E: Exception do
        Log(lcError, 'Client free error: ' + E.Message);
    end;
  end;
end;

procedure TTCPServerClientThread.Terminate;
var S : ATCPClientSocket;
begin
  if not Terminated then
    if Assigned(FClient) then
      begin
        S := FClient.FSocket;
        if Assigned(S) then
          begin
            S.Terminate;
            // Send terminate message to socket
            if S.WindowHandle <> 0 then
              PostMessage(S.WindowHandle, WM_QUIT, 0, 0);
          end;
      end;
  inherited Terminate;
end;

function GetTCPServerClientThread: TTCPServerClientThread;
begin
  Result := TTCPServerClientThread.Create;
end;



{                                                                              }
{ ATCPServer                                                                   }
{                                                                              }
constructor ATCPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Init;
end;

constructor ATCPServer.CreateEx(const ListenPort: String; const TimeOut: Integer;
    const ServerMode: TTCPServerMode; const ClientClass: CTCPServerClient);
begin
  inherited Create(nil);
  Init;
  FTimeOut := TimeOut;
  FServerMode := ServerMode;
  FClientClass := ClientClass;
  FListenPort := ListenPort;
end;

destructor ATCPServer.Destroy;
begin
  FActive := False;
  if Assigned(FSocket) then
    FSocket.Close;
  TerminateClients;
  FreeAndNilObjectArray(ObjectArray(FClients));
  FreeAndNil(FSocket);
  inherited Destroy;
end;

procedure ATCPServer.Init;
begin
  FTimeOut := DefaultSocketStreamTimeOut;
  FServerMode := smAcceptClient;
  FMaxClients := -1;
  FSocket := TTCPServerSocket.Create;
  FSocket.OnConnectionAvailable := OnServerConnectionAvailable;
end;

procedure ATCPServer.RaiseError(const Msg: String);
begin
  raise ETCPServer.Create(Msg);
end;

procedure ATCPServer.RaiseAndLogError(const Msg: String);
begin
  Log(lcError, Msg);
  RaiseError(Msg);
end;

procedure ATCPServer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FLogTo then
      FLogTo := nil;
end;

procedure ATCPServer.Log(const LogClass: TLogClass; const Msg: String);
begin
  if csDesigning in ComponentState then
    exit;
  if Assigned(FOnLog) then
    FOnLog(self, Msg);
  if Assigned(FLogTo) then
    FLogTo.Log(self, LogClass, Msg);
end;

procedure ATCPServer.SetListenPort(const ListenPort: String);
begin
  if ListenPort = FListenPort then
    exit;
  if ListenPort = '' then
    RaiseError('Invalid ListenPort');
  if Active then
    RaiseError('Cannot change port while server is active');
  FListenPort := ListenPort;
end;

function ATCPServer.GetLocalHost: String;
begin
  Assert(Assigned(FSocket), 'Assigned(FSocket)');
  Result := FSocket.LocalHost;
end;

function ATCPServer.LocalHostName: String;
begin
  Result := FSocket.LocalHostName;
end;

procedure ATCPServer.SetLocalHost(const LocalHost: String);
begin
  if Active then
    RaiseError('Cannot change local host while server is active');
  Assert(Assigned(FSocket), 'Assigned(FSocket)');
  FSocket.LocalHost := LocalHost;
end;

function ATCPServer.GetMaxBacklog: Integer;
begin
  Assert(Assigned(FSocket), 'Assigned(FSocket)');
  Result := FSocket.MaxBacklog;
end;

procedure ATCPServer.SetMaxBacklog(const MaxBacklog: Integer);
begin
  Assert(Assigned(FSocket), 'Assigned(FSocket)');
  FSocket.MaxBacklog := MaxBacklog;
end;

procedure ATCPServer.SetServerMode(const ServerMode: TTCPServerMode);
begin
  if FServerMode = ServerMode then
    exit;
  if Active then
    RaiseError('Cannot change server mode while server is active');
  FServerMode := ServerMode;
end;

procedure ATCPServer.SetClientSocketProperties;
var I : Integer;
begin
  FThrottleClientReadRate := ThrottleClientReadRate;
  For I := 0 to Length(FClients) - 1 do
    FClients[I].SetSocketProperties;
end;

procedure ATCPServer.SetThrottleClientReadRate(const ThrottleClientReadRate: Integer);
begin
  if ThrottleClientReadRate = FThrottleClientReadRate then
    exit;
  FThrottleClientReadRate := ThrottleClientReadRate;
  SetClientSocketProperties;
end;

procedure ATCPServer.SetThrottleClientWriteRate(const ThrottleClientWriteRate: Integer);
begin
  if ThrottleClientWriteRate = FThrottleClientWriteRate then
    exit;
  FThrottleClientWriteRate := ThrottleClientWriteRate;
  SetClientSocketProperties;
end;

procedure ATCPServer.SetThrottleClientRead(const ThrottleClientRead: Boolean);
begin
  if ThrottleClientRead = FThrottleClientRead then
    exit;
  FThrottleClientRead := ThrottleClientRead;
  SetClientSocketProperties;
end;

procedure ATCPServer.SetThrottleClientWrite(const ThrottleClientWrite: Boolean);
begin
  if ThrottleClientWrite = FThrottleClientWrite then
    exit;
  FThrottleClientWrite := ThrottleClientWrite;
  SetClientSocketProperties;
end;

procedure ATCPServer.SetActive(const Active: Boolean);
begin
  if FActive = Active then
    exit;

  if csDesigning in ComponentState then
    begin
      FActive := Active;
      exit;
    end;

  if Active then
    begin
      if ListenPort = '' then
        RaiseError('ListenPort not set');
      try
        Start;
      except
        on E: Exception do
          RaiseAndLogError('Server activation failed: ' + E.Message);
      end;
      FStartTime := Now;
      FActive := True;
      Log(lcInfo, 'Server active. Listening on port ' + FListenPort + '.');
      TriggerActive;
    end else
    begin
      FActive := False;
      try
        Stop;
      except
        on E: Exception do
          Log(lcError, 'Server deactivation failed: ' + E.Message);
      end;
      Log(lcInfo, 'Server shutdown.');
      TriggerInactive;
    end;
end;

function ATCPServer.GetUpTime: TDateTime;
begin
  if FActive then
    Result := Now - FStartTime else
    Result := 0.0;
end;

function ATCPServer.GetUpTimeStr: String;
begin
  Result := DateTimeAsElapsedTime(GetUpTime);
end;

procedure ATCPServer.TriggerActive;
begin
  if Assigned(FOnActive) then
    FOnActive(self);
end;

procedure ATCPServer.TriggerInactive;
begin
  if Assigned(FOnInactive) then
    FOnInactive(self);
end;

procedure ATCPServer.Start;
begin
  FSocket.ListenPort := FListenPort;
  FSocket.Listen;
end;

procedure ATCPServer.Stop;
begin
  Assert(Assigned(FSocket), 'Assigned(FSocket)');
  FSocket.Close;
  TerminateClients;
end;

procedure ATCPServer.OnServerConnectionAvailable(Sender: TTCPServerSocket);
begin
  if not FActive then
    exit;
  if (FMaxClients >= 0) and (ClientCount >= FMaxClients) then
    begin
      FAcceptPending := True;
      exit;
    end;
  TriggerConnectionAvailable;
  if FServerMode = smManualAccept then
    exit;
  Case FServerMode of
    smAcceptClient       : AcceptClient;
    smAcceptClientThread : AcceptClientThread;
  end;
end;

procedure ATCPServer.TriggerConnectionAvailable;
begin
  if Assigned(FOnConnectionAvailable) then
    FOnConnectionAvailable(self);
end;

function ATCPServer.Accept: TTCPServerClient;
var Socket  : TSocket;
    Address : TSockAddr;
begin
  try
    Socket := FSocket.SocketAccept(Address);
  except
    Result := nil;
    exit;
  end;
  if Socket = INVALID_SOCKET then
    begin
      Result := nil;
      exit;
    end;
  try
    Result := CreateClient;
  except
    on E: Exception do
      begin
        WinSock.CloseSocket(Socket);
        Log(lcError, 'Error creating client: ' + E.Message);
        Result := nil;
        exit;
      end;
  end;
  Result.FWinSocket := Socket;
  Result.FAddress := Address;
  Append(ObjectArray(FClients), Result);
end;

procedure ATCPServer.AcceptClient;
var Client  : TTCPServerClient;
begin
  Repeat
    Client := Accept;
    if not Assigned(Client) then
      exit;
    Client.FStream := TSocketStream.Create(Client.GetSocket, False,
        smAsynchronous, FTimeOut);
    With Client.FSocket do
      begin
        OnDataAvailable := OnClientSocketDataAvailable;
        OnClose := OnClientSocketClose;
      end;
    TriggerClientActive(Client);
    if not FActive or ((FMaxClients >= 0) and (ClientCount >= FMaxClients)) then
      exit;
  Until False;
end;

procedure ATCPServer.AcceptClientThread;
var Client : TTCPServerClient;
    Thread : TTCPServerClientThread;
begin
  Repeat
    Client := Accept;
    if not Assigned(Client) then
      exit;
    Thread := GetTCPServerClientThread;
    if not Assigned(Thread) then
      begin
        Client.Free;
        exit;
      end;
    Client.FThread := Thread;
    Thread.InitRun(self, Client);
    Thread.Resume;
    if not FActive or ((FMaxClients >= 0) and (ClientCount >= FMaxClients)) then
      exit;
  Until False;
end;

function ATCPServer.CreateClient: TTCPServerClient;
begin
  if Assigned(FOnCreateClient) then
    begin
      Result := FOnCreateClient(self);
      if Assigned(Result) then
        exit;
    end;
  if Assigned(FClientClass) then
    begin
      Result := FClientClass.Create(self);
      exit;
    end;
  Result := TTCPServerClient.Create(self);
end;

procedure ATCPServer.TriggerClientActive(const Client: TTCPServerClient);
begin
  Assert(Assigned(Client), 'Assigned(Client)');
  if Assigned(FOnClientActive) then
    FOnClientActive(Client);
  Client.ClientActive;
end;

procedure ATCPServer.TriggerClientInactive(const Client: TTCPServerClient);
begin
  Assert(Assigned(Client), 'Assigned(Client)');
  RemoveClient(Client);
  if Assigned(FOnClientInactive) then
    FOnClientInactive(Client);
  Client.ClientInactive;
end;

procedure ATCPServer.TriggerThreadRun(const Client: TTCPServerClient);
begin
  if Assigned(FOnThreadRun) then
    FOnThreadRun(Client);
end;

procedure ATCPServer.TriggerClientRemoved(const Client: TTCPServerClient);
begin
  if Assigned(FOnClientRemoved) then
    FOnClientRemoved(Client);
end;

procedure ATCPServer.RemoveClientByIndex(const Idx: Integer);
var Client : TTCPServerClient;
begin
  Assert(Idx >= 0, 'Idx >= 0');
  Client := FClients[Idx];
  Remove(ObjectArray(FClients), Idx, 1, False);
  TriggerClientRemoved(Client);
  if FAcceptPending then
    OnServerConnectionAvailable(FSocket);
end;

procedure ATCPServer.RemoveClient(const Client: TTCPServerClient);
var I : Integer;
begin
  For I := 0 to Length(FClients) - 1 do
    if FClients[I] = Client then
      begin
        RemoveClientByIndex(I);
        exit;
      end;
end;

function ATCPServer.GetReadRate: Integer;
var I : Integer;
    S : ATCPClientSocket;
begin
  Result := 0;
  For I := 0 to Length(FClients) - 1 do
    begin
      S := FClients[I].FSocket;
      if Assigned(S) then
        Inc(Result, S.GetReadRate);
    end;
end;

function ATCPServer.GetWriteRate: Integer;
var I : Integer;
    S : ATCPClientSocket;
begin
  Result := 0;
  For I := 0 to Length(FClients) - 1 do
    begin
      S := FClients[I].FSocket;
      if Assigned(S) then
        Inc(Result, S.GetWriteRate);
    end;
end;

procedure ATCPServer.OnClientSocketDataAvailable(Sender: ATCPClientSocket);
var I : Integer;
    C : TTCPServerClient;
begin
  if not FActive or not Assigned(FOnDataAvailable) then
    exit;
  I := GetClientIndexBySocket(Sender);
  if I = -1 then
    exit;
  C := FClients[I];
  if not C.Terminated then
    FOnDataAvailable(C);
end;

procedure ATCPServer.OnClientSocketClose(Sender: ATCPClientSocket);
var I : Integer;
    C : TTCPServerClient;
begin
  I := GetClientIndexBySocket(Sender);
  if I = -1 then
    exit;
  C := FClients[I];
  if FActive and not C.Terminated then
    TriggerClientInactive(C);
  C.Free;
end;

function ATCPServer.GetClientCount: Integer;
begin
  Result := Length(FClients);
end;

function ATCPServer.GetClientByIndex(const Idx: Integer): TTCPServerClient;
begin
  Result := FClients[Idx];
end;

function ATCPServer.GetClientIndex(const Client: TTCPServerClient): Integer;
var I : Integer;
begin
  For I := 0 to Length(FClients) - 1 do
    if FClients[I] = Client then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function ATCPServer.GetClientIndexByStream(const Stream: AStream): Integer;
var I : Integer;
begin
  For I := 0 to Length(FClients) - 1 do
    if Assigned(FClients[I]) and (FClients[I].FStream = Stream) then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function ATCPServer.GetClientIndexBySocket(const Socket: ATCPClientSocket): Integer;
var I : Integer;
begin
  For I := 0 to Length(FClients) - 1 do
    if FClients[I].FSocket = Socket then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function ATCPServer.HasClient(const Client: TTCPServerClient): Boolean;
begin
  Result := GetClientIndex(Client) >= 0;
end;

procedure ATCPServer.TerminateClients;
var I : Integer;
    R : Boolean;
    C : TTCPServerClient;
begin
  Repeat
    R := False;
    For I := Length(FClients) - 1 downto 0 do
      begin
        C := FClients[I];
        if not C.Terminated then
          begin
            R := True;
            C.Terminate;
            break;
          end;
      end;
  Until not R;
end;



{                                                                              }
{ TTCPServerClient                                                             }
{                                                                              }
constructor TTCPServerClient.Create(const Server: ATCPServer);
begin
  inherited Create;
  Assert(Assigned(Server), 'Assigned(Server)');
  FServer := Server;
  Init;
end;

procedure TTCPServerClient.Init;
begin
end;

destructor TTCPServerClient.Destroy;
begin
  if Assigned(FServer) then
    begin
      FServer.RemoveClient(self);
      FServer := nil;
    end;
  if Assigned(FThread) then
    begin
      FThread.Terminate;
      TTCPServerClientThread(FThread).FClient := nil;
      FThread := nil;
    end;
  FreeAndNil(FStream);
  FreeAndNil(FSocket);
  inherited Destroy;
end;

procedure TTCPServerClient.Terminate;
begin
  FTerminated := True;
  if Assigned(FSocket) then
    begin
      FSocket.Terminate;
      if Assigned(FThread) then
        SendMessage(FSocket.SocketHandle, WM_QUIT, 0, 0);
      try
        FSocket.Close(True);
      except end;
    end;
  if Assigned(FThread) then
    FThread.Terminate;
end;

procedure TTCPServerClient.Log(const LogClass: TLogClass; const Msg: String);
begin
  if Assigned(FThread) then
    TTCPServerClientThread(FThread).Log(LogClass, Msg) else
    if Assigned(FServer) then
      FServer.Log(LogClass, Msg);
end;

procedure TTCPServerClient.ThreadRun;
begin
  if not FTerminated and Assigned(FServer) then
    FServer.TriggerThreadRun(self);
end;

procedure TTCPServerClient.SetSocketProperties;
begin
  With FSocket do
    begin
      ReadThrottleRate := FServer.FThrottleClientReadRate;
      ThrottleRead := FServer.FThrottleClientRead;
      WriteThrottleRate := FServer.FThrottleClientWriteRate;
      ThrottleWrite := FServer.FThrottleClientWrite;
    end;
end;

function TTCPServerClient.GetSocket: TTCPServerClientSocket;
begin
  if not Assigned(FSocket) then
    begin
      FSocket := TTCPServerClientSocket.Create(FWinSocket);
      SetSocketProperties;
    end;
  Result := FSocket;
end;

function TTCPServerClient.GetStream: TSocketStream;
begin
  if not Assigned(FStream) then
    FStream := TSocketStream.Create(GetSocket, False,
        smAsynchronous, FServer.FTimeOut);
  Result := FStream;
end;

procedure TTCPServerClient.ClientActive;
begin
end;

procedure TTCPServerClient.ClientInactive;
begin
end;

procedure TTCPServerClient.ClientError(const Msg: String);
begin
  FErrorMsg := Msg;
end;

procedure TTCPServerClient.WriteStr(const S: String);
begin
  Stream.WriteStr(S);
end;

function TTCPServerClient.ReadAvailable: String;
begin
  Result := GetStream.Reader.ReadAvailable;
end;

function TTCPServerClient.PeekStr(const Count: Integer): String;
begin
  Result := GetStream.Reader.PeekStr(Count);
end;

{ Returns True if at least MinCount bytes if available and either i) Delimiter }
{ occurs (Delimiter <> '') or ii) if MaxCount is available (MaxCount <> -1) or }
{ iii) (Delimiter = '') and (MaxCount = -1)                                    }
function TTCPServerClient.ReadEx(const MinCount, MaxCount: Integer;
    const Delimiter: String; var Buf: String; const Peek: Boolean): Boolean;
var I, L  : Integer;
    InBuf : String;
begin
  L := FSocket.AvailableToRead;
  if (L = 0) or (L < MinCount) then
    begin
      Buf := '';
      Result := False;
      exit;
    end;
  InBuf := FSocket.PeekStr(L);

  if Delimiter <> '' then
    begin
      I := Pos(Delimiter, InBuf);
      if (I > 0) and ((MaxCount = -1) or (I <= MaxCount)) then
        if I - 1 >= MinCount then
          begin
            Buf := CopyLeft(InBuf, I - 1);
            if not Peek then
              FSocket.Skip(I + Length(Delimiter) - 1);
            Result := True;
            exit;
          end else
          begin
            Result := False;
            Buf := '';
            exit;
          end;
    end else
    if MaxCount = -1 then
      begin
        Buf := InBuf;
        if not Peek then
          FSocket.Skip(Length(InBuf));
        Result := True;
        exit;
      end;

  if (MaxCount >= 0) and (L >= MaxCount) then
    begin
      Buf := CopyLeft(InBuf, MaxCount);
      if not Peek then
        FSocket.Skip(MaxCount);
      Result := True;
    end else
    begin
      Buf := '';
      Result := False;
    end;
end;

function TTCPServerClient.SocketReadLine(var Buf: String; const Delimiter: String): Boolean;
begin
  try
    Result := ReadEx(0, -1, Delimiter, Buf);
  except
    on E: Exception do
      begin
        Log(lcError, 'Read error: ' + E.Message);
        Result := False;
      end;
  end;
end;

function TTCPServerClient.StreamReadLine(var S: String): Boolean;
begin
  S := '';
  Result := False;
  if FTerminated then
    exit;
  try
    S := Stream.Reader.ExtractLine;
    Result := True;
  except
    on E: Exception do
      Log(lcError, 'Read error: ' + E.Message);
  end;
end;

procedure TTCPServerClient.Synchronize(const Method: TThreadMethod);
begin
  if Assigned(FThread) then
    FThread.Synchronize(Method);
end;



end.

