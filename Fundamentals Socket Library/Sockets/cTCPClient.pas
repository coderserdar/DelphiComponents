{$INCLUDE ..\cDefines.inc}
unit cTCPClient;

{                                                                              }
{                          TCP Client Component v3.05                          }
{                                                                              }
{        This unit is copyright © 2001-2003 by David Butler (david@e.co.za)    }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                  Its original file name is cTCPClient.pas                    }
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
{ Revision history:                                                            }
{  [ cSockets ]                                                                }
{  10/02/2001  0.01  Initial version.                                          }
{  10/02/2002  2.02  Refactored for Fundamentals 2.                            }
{  19/02/2002  2.03  Small changes and bug fixes.                              }
{  [ cTCPClient ]                                                              }
{  01/07/2002  3.04  Refactored for Fundamentals 3.                            }
{  01/12/2002  3.05  Add ClientPrototype property to TTCPClientCollection.     }
{                                                                              }

interface

uses
  { Delphi }
  Windows,
  WinSock,
  SysUtils,
  Classes,

  { Fundamentals }
  cLog,
  cThreads,
  cSockets,
  cSocketsTCP,
  cSocketsTCPClient,
  cTCPStream;



const
  UnitName    = 'cTCPClient';
  UnitVersion = '3.05';



{                                                                              }
{ ATCPClient                                                                   }
{   Base class for TCP Client components.                                      }
{                                                                              }
{   If RunInThread then ThreadRun is executed, which connects and calls        }
{   OnThreadRun, from the Thread. Events with an OnSync- prefix are always     }
{   called synchronized, while their On- counterparts are called from the      }
{   thread.                                                                    }
{                                                                              }
{   If not RunInThread then Start and Stop is called, which Connects and       }
{   Disconnects respectively.                                                  }
{                                                                              }
type
  {$TYPEINFO ON}
  TTCPClientState = (csClosed, csConnecting, csConnected);
  ATCPClient = class;
  ATCPClientEvent = procedure (Sender: ATCPClient) of object;
  ATCPClientSocketStateChangeEvent = procedure (Sender: ATCPClient;
      OldState, State: TSocketState) of object;
  ATCPClientStateChangeEvent = procedure (Sender: ATCPClient;
      OldState, State: TTCPClientState) of object;
  ATCPClientLogEvent = procedure (Sender: ATCPClient; Msg: String) of object;
  TTCPClientCollection = class;
  ATCPClientConnectionProxy = class;
  ATCPClient = class(TComponent)
  protected
    FTagObject               : TObject;
    FHost                    : String;
    FPort                    : String;
    FLocalHost               : String;
    FLocalPort               : String;
    FProxy                   : ATCPClientConnectionProxy;
    FStreamMode              : TSocketStreamMode;
    FTimeOut                 : Integer;
    FRunInThread             : Boolean;
    FCollection              : TTCPClientCollection;
    FLogTo                   : TLog;

    FOnLog                   : ATCPClientLogEvent;
    FOnInitSocket            : ATCPClientEvent;
    FOnStateChange           : ATCPClientStateChangeEvent;
    FOnSocketStateChange     : ATCPClientSocketStateChangeEvent;
    FOnNegotiating           : ATCPClientEvent;
    FOnConnected             : ATCPClientEvent;
    FOnConnectFailed         : ATCPClientEvent;
    FOnDataAvailable         : ATCPClientEvent;
    FOnLineAvailable         : ATCPClientEvent;
    FOnClose                 : ATCPClientEvent;
    FOnStreamBlockNotify     : ATCPClientEvent;

    FOnSyncSocketStateChange : ATCPClientSocketStateChangeEvent;
    FOnSyncNegotiating       : ATCPClientEvent;
    FOnSyncConnected         : ATCPClientEvent;
    FOnSyncConnectFailed     : ATCPClientEvent;
    FOnSyncDataAvailable     : ATCPClientEvent;
    FOnSyncLineAvailable     : ATCPClientEvent;
    FOnSyncClose             : ATCPClientEvent;

    FOnThreadRun             : ATCPClientEvent;
    FOnThreadRunComplete     : ATCPClientEvent;

    FActive                  : Boolean;
    FLoadActive              : Boolean;
    FState                   : TTCPClientState;
    FTerminated              : Boolean;
    FSocket                  : TTCPClientSocket;
    FStream                  : TSocketStream;
    FThread                  : TThreadEx;
    FErrorCode               : Integer;
    FErrorMsg                : String;
    FSyncOldSocketState      : TSocketState;
    FSyncSocketState         : TSocketState;

    FThrottleWrite           : Boolean;
    FWriteThrottleRate       : Integer;
    FThrottleRead            : Boolean;
    FReadThrottleRate        : Integer;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;

    procedure Init; virtual;
    procedure RaiseError(const Msg: String); virtual;
    procedure Log(const Msg: String); virtual;
    procedure ThreadLog(const Msg: String);
    procedure SetError(const Code: Integer; const Msg: String);
    procedure CheckInactive;

    procedure ClearSocket;
    procedure CloseSocket;
    procedure ClearThread;
    procedure InitSocket;
    procedure InitFromSocket(const Socket: TTCPClientSocket);
    procedure InitNewSocket;
    procedure SetSocket(const Socket: TTCPClientSocket);
    procedure SetStream(const Stream: TSocketStream); virtual;

    procedure SetCollection(const Collection: TTCPClientCollection); virtual;
    procedure SetHost(const Host: String); virtual;
    procedure SetPort(const Port: String); virtual;
    procedure SetProxy(const Proxy: ATCPClientConnectionProxy); virtual;
    procedure SetStreamMode(const StreamMode: TSocketStreamMode); virtual;

    function  GetLocalHost: String;
    procedure SetLocalHost(const LocalHost: String);
    function  GetLocalPort: String;
    procedure SetLocalPort(const LocalPort: String);

    function  GetThrottleWrite: Boolean;
    procedure SetThrottleWrite(const ThrottleWrite: Boolean);
    function  GetWriteThrottleRate: Integer;
    procedure SetWriteThrottleRate(const ThrottleRate: Integer);
    function  GetThrottleRead: Boolean;
    procedure SetThrottleRead(const ThrottleRead: Boolean);
    function  GetReadThrottleRate: Integer;
    procedure SetReadThrottleRate(const ThrottleRate: Integer);

    procedure Connect; virtual;
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure ThreadRun; virtual;
    procedure SetActive(const Active: Boolean);
    procedure SetState(const State: TTCPClientState);
    function  GetTerminated: Boolean;

    procedure BeforeActivation; virtual;
    procedure BeforeDeactivation; virtual;
    procedure ClientInactive; virtual;

    procedure SyncTriggerSocketStateChange;
    procedure SyncTriggerNegotiating;
    procedure SyncTriggerConnected;
    procedure SyncTriggerConnectFailed;
    procedure SyncTriggerDataAvailable;
    procedure SyncTriggerLineAvailable;
    procedure SyncTriggerClose;

    procedure TriggerSocketStateChange(const OldState, State: TSocketState); virtual;
    procedure TriggerStateChange(const OldState, State: TTCPClientState); virtual;
    procedure TriggerNegotiating;
    procedure TriggerConnected; virtual;
    procedure TriggerConnectFailed; virtual;
    procedure TriggerDataAvailable; virtual;
    procedure TriggerClose; virtual;
    procedure TriggerThreadRunComplete; virtual;

    procedure DoOnSocketStateChange(Sender: ASocket; OldState, State: TSocketState);
    procedure DoOnSocketNegotiating(Sender: TTCPClientSocket);
    procedure DoOnSocketConnected(Sender: TTCPClientSocket);
    procedure DoOnSocketConnectFailed(Sender: TTCPClientSocket);
    procedure DoOnSocketDataAvailable(Sender: ATCPClientSocket);
    procedure DoOnSocketClose(Sender: ATCPClientSocket);
    procedure DoOnStreamBlockNotify(const Sender: TSocketStream);

    function  GetSocket: TTCPClientSocket;
    function  GetSocketProxy: ATCPClientSocketProxy;
    function  GetStream: TSocketStream;

  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateEx(const Host, Port: String;
                const TimeOut: Integer = DefaultSocketStreamTimeOut); overload;
    constructor CreateEx(const Socket: TTCPClientSocket); overload;
    constructor CreateEx(const Stream: TSocketStream); overload;
    destructor Destroy; override;

    procedure AssignPrototype(const Prototype: ATCPClient); virtual;

    property  TagObject: TObject read FTagObject write FTagObject;
    property  Collection: TTCPClientCollection read FCollection write SetCollection;
    property  LogTo: TLog read FLogTo write FLogTo;

    property  Host: String read FHost write SetHost;
    property  Port: String read FPort write SetPort;

    property  Proxy: ATCPClientConnectionProxy read FProxy write FProxy;

    property  StreamMode: TSocketStreamMode read FStreamMode write SetStreamMode;
    property  OnStreamBlockNotify: ATCPClientEvent read FOnStreamBlockNotify write FOnStreamBlockNotify;
    property  TimeOut: Integer read FTimeOut write FTimeOut;

    property  LocalHost: String read GetLocalHost write SetLocalHost;
    property  LocalPort: String read GetLocalPort write SetLocalPort;

    property  Active: Boolean read FActive write SetActive;
    property  State: TTCPClientState read FState;
    property  Socket: TTCPClientSocket read FSocket;
    property  Stream: TSocketStream read FStream;
    procedure Release;
    procedure Synchronize(const Method: TThreadMethod);
    procedure Terminate; virtual;
    property  Terminated: Boolean read GetTerminated;
    property  ErrorCode: Integer read FErrorCode;
    property  ErrorMsg: String read FErrorMsg;
    property  OnInitSocket: ATCPClientEvent read FOnInitSocket write FOnInitSocket;

    property  ThrottleWrite: Boolean read GetThrottleWrite write SetThrottleWrite;
    property  WriteThrottleRate: Integer read GetWriteThrottleRate write SetWriteThrottleRate;
    property  ThrottleRead: Boolean read GetThrottleRead write SetThrottleRead;
    property  ReadThrottleRate: Integer read GetReadThrottleRate write SetReadThrottleRate;

    property  RunInThread: Boolean read FRunInThread write FRunInThread;
    property  OnThreadRun: ATCPClientEvent read FOnThreadRun write FOnThreadRun;
    property  OnThreadRunComplete: ATCPClientEvent read FOnThreadRunComplete write FOnThreadRunComplete;

    property  OnLog: ATCPClientLogEvent read FOnLog write FOnLog;
    property  OnStateChange: ATCPClientStateChangeEvent read FOnStateChange write FOnStateChange;
    property  OnSocketStateChange: ATCPClientSocketStateChangeEvent read FOnSocketStateChange write FOnSocketStateChange;
    property  OnNegotiating: ATCPClientEvent read FOnNegotiating write FOnNegotiating;
    property  OnConnected: ATCPClientEvent read FOnConnected write FOnConnected;
    property  OnConnectFailed: ATCPClientEvent read FOnConnectFailed write FOnConnectFailed;
    property  OnDataAvailable: ATCPClientEvent read FOnDataAvailable write FOnDataAvailable;
    property  OnLineAvailable: ATCPClientEvent read FOnLineAvailable write FOnLineAvailable;
    property  OnClose: ATCPClientEvent read FOnClose write FOnClose;

    property  OnSyncSocketStateChange: ATCPClientSocketStateChangeEvent read FOnSyncSocketStateChange write FOnSyncSocketStateChange;
    property  OnSyncNegotiating: ATCPClientEvent read FOnSyncNegotiating write FOnSyncNegotiating;
    property  OnSyncConnected: ATCPClientEvent read FOnSyncConnected write FOnSyncConnected;
    property  OnSyncConnectFailed: ATCPClientEvent read FOnSyncConnectFailed write FOnSyncConnectFailed;
    property  OnSyncDataAvailable: ATCPClientEvent read FOnSyncDataAvailable write FOnSyncDataAvailable;
    property  OnSyncLineAvailable: ATCPClientEvent read FOnSyncLineAvailable write FOnSyncLineAvailable;
    property  OnSyncClose: ATCPClientEvent read FOnSyncClose write FOnSyncClose;
  end;
  ATCPClientClass = class of ATCPClient;
  ETCPClient = class(Exception);
  ATCPClientArray = Array of ATCPClient;

  { ATCPClientConnectionProxy                                                  }
  ATCPClientConnectionProxy = class(TComponent)
  protected
    FHost                : String;
    FPort                : String;
    FUseAuthentication   : Boolean;
    FUsername            : String;
    FPassword            : String;
    FResolveThroughProxy : Boolean;
    FNextProxy           : ATCPClientConnectionProxy;

    procedure Init; virtual;
    procedure SetNextProxy(const NextProxy: ATCPClientConnectionProxy); virtual;

    function  GetSocketProxy(const Client: ATCPClient): ATCPClientSocketProxy; virtual; abstract;
    function  GetNextSocketProxy(const Client: ATCPClient): ATCPClientSocketProxy;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;

    property  Host: String read FHost write FHost;
    property  Port: String read FPort write FPort;
    property  UseAuthentication: Boolean read FUseAuthentication write FUseAuthentication;
    property  Username: String read FUsername write FUsername;
    property  Password: String read FPassword write FPassword;
    property  ResolveThroughProxy: Boolean read FResolveThroughProxy write FResolveThroughProxy;
    property  NextProxy: ATCPClientConnectionProxy read FNextProxy write SetNextProxy;
  end;
  ETCPClientConnectionProxy = class(Exception);

  { TTCPClientCollection                                                       }
  TTCPClientCollectionClientEvent = procedure (Sender: TTCPClientCollection;
      Client: ATCPClient) of object;
  TTCPClientCollectionCreateClientEvent = procedure (Sender: TTCPClientCollection;
      var Client: ATCPClient) of object;
  TTCPClientCollection = class(TComponent)
  protected
    FClientPrototype  : ATCPClient;
    FOnCreateClient   : TTCPClientCollectionCreateClientEvent;
    FOnClientCreate   : TTCPClientCollectionClientEvent;
    FOnClientAdd      : TTCPClientCollectionClientEvent;
    FOnClientRemove   : TTCPClientCollectionClientEvent;
    FLock             : TRTLCriticalSection;
    FClients          : ATCPClientArray;

    procedure TriggerCreateClient(var Client: ATCPClient); virtual;

    procedure Add(const Client: ATCPClient);
    procedure Remove(const Client: ATCPClient);

    procedure ClientAdded(const Client: ATCPClient); virtual;
    procedure ClientRemoved(const Client: ATCPClient); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure Lock;
    procedure Unlock;

    procedure Clear; virtual;
    function  Count: Integer;

    function  ActiveCount: Integer;
    function  Start(const Client: ATCPClient): Boolean; virtual;
    function  CreateClient: ATCPClient;
    function  CreateAndAdd: ATCPClient;
    function  CreateAndStart: ATCPClient;
    function  RemoveFirst: ATCPClient;

    property  ClientPrototype: ATCPClient read FClientPrototype write FClientPrototype;

    property  OnCreateClient: TTCPClientCollectionCreateClientEvent read FOnCreateClient write FOnCreateClient;
    property  OnClientCreate: TTCPClientCollectionClientEvent read FOnClientCreate write FOnClientCreate;
    property  OnClientAdd: TTCPClientCollectionClientEvent read FOnClientAdd write FOnClientAdd;
    property  OnClientRemove: TTCPClientCollectionClientEvent read FOnClientRemove write FOnClientRemove;
  end;
  ETCPClientCollection = class(Exception);
  {$TYPEINFO OFF}



{                                                                              }
{ TfndTCPClient                                                                }
{   ATCPClient implementation for a TCP client socket.                         }
{                                                                              }
type
  TfndTCPClient = class(ATCPClient)
  published
    property  Host;
    property  Port;

    property  LocalHost;
    property  LocalPort;

    property  Proxy;

    property  StreamMode;
    property  TimeOut;
    property  RunInThread;

    property  Collection;
    property  OnLog;
    property  LogTo;

    property  Active;

    property  ThrottleWrite;
    property  WriteThrottleRate;
    property  ThrottleRead;
    property  ReadThrottleRate;

    property  OnInitSocket;
    property  OnStateChange;
    property  OnSocketStateChange;
    property  OnNegotiating;
    property  OnConnected;
    property  OnConnectFailed;
    property  OnDataAvailable;
    property  OnClose;
    property  OnThreadRun;
    property  OnThreadRunComplete;

    property  OnSyncSocketStateChange;
    property  OnSyncNegotiating;
    property  OnSyncConnected;
    property  OnSyncConnectFailed;
    property  OnSyncDataAvailable;
    property  OnSyncClose;
  end;



{                                                                              }
{ TfndTCPClientCollection                                                      }
{                                                                              }
type
  TfndTCPClientCollection = class(TTCPClientCollection)
  published
    property  ClientPrototype;

    property  OnCreateClient;
    property  OnClientCreate;
    property  OnClientAdd;
    property  OnClientRemove;
  end;



{                                                                              }
{ TfndSocks5Proxy                                                              }
{                                                                              }
type
  TfndSocks5Proxy = class(ATCPClientConnectionProxy)
  protected
    procedure Init; override;
    function  GetSocketProxy(const Client: ATCPClient): ATCPClientSocketProxy; override;

  published
    property  Host;
    property  Port;
    property  UseAuthentication;
    property  Username;
    property  Password;
    property  ResolveThroughProxy;
    property  NextProxy;
  end;



{                                                                              }
{ TfndHTTPTunnelProxy                                                          }
{                                                                              }
type
  TfndHTTPTunnelProxy = class(ATCPClientConnectionProxy)
  protected
    procedure Init; override;
    function  GetSocketProxy(const Client: ATCPClient): ATCPClientSocketProxy; override;

  published
    property  Host;
    property  Port;
    property  UseAuthentication;
    property  Username;
    property  Password;
    property  ResolveThroughProxy;
    property  NextProxy;
  end;



implementation

uses
  { Delphi }
  Messages,

  { Fundamentals }
  cUtils;



{                                                                              }
{ TTCPClientThread                                                             }
{                                                                              }
type
  TTCPClientThread = class(TThreadEx)
  protected
    FClient   : ATCPClient;
    FLogMsg   : String;
    FErrorMsg : String;

    procedure InitRun(const Client: ATCPClient);
    procedure Execute; override;
    procedure SyncThreadRunComplete;
    procedure Log(const Msg: String);
    procedure NotifyLog;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Terminate; override;
  end;

constructor TTCPClientThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

destructor TTCPClientThread.Destroy;
begin
  if Assigned(FClient) and (FClient.FThread = self) then // abnormal destruction
    FClient.FThread := nil;
  FClient := nil;
  inherited Destroy;
end;

procedure TTCPClientThread.NotifyLog;
begin
  if Terminated or not Assigned(FClient) then
    exit;
  FClient.Log(FLogMsg);
end;

procedure TTCPClientThread.Log(const Msg: String);
begin
  if Terminated or not Assigned(FClient) then
    exit;
  FLogMsg := Msg;
  Synchronize(NotifyLog);
end;

procedure TTCPClientThread.InitRun(const Client: ATCPClient);
begin
  Assert(Assigned(Client), 'Assigned(Client)');
  FClient := Client;
end;

procedure TTCPClientThread.Execute;
begin
  FErrorMsg := '';
  if not Assigned(FClient) then
    exit;
  try try
    FClient.ThreadRun;
  except
    on E : Exception do
      FErrorMsg := E.Message;
  end;
  finally
    if Assigned(FClient) then
      Synchronize(SyncThreadRunComplete);
  end;
end;

procedure TTCPClientThread.SyncThreadRunComplete;
var C : ATCPClient;
begin
  C := FClient;
  FClient := nil;
  if Assigned(C) and (C.FThread = self) then
    begin
      C.FThread := nil;
      if C.FErrorCode = 0 then
        C.SetError(SocketGeneralApplicationError, FErrorMsg);
      C.TriggerThreadRunComplete;
    end;
end;

procedure TTCPClientThread.Terminate;
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



{                                                                              }
{ ATCPClient                                                                   }
{                                                                              }
constructor ATCPClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Init;
end;

constructor ATCPClient.CreateEx(const Host, Port: String; const TimeOut: Integer);
begin
  inherited Create(nil);
  Init;
  SetHost(Host);
  SetPort(Port);
  FTimeOut := TimeOut;
end;

constructor ATCPClient.CreateEx(const Socket: TTCPClientSocket);
begin
  inherited Create(nil);
  Init;
  InitFromSocket(Socket);
end;

constructor ATCPClient.CreateEx(const Stream: TSocketStream);
begin
  inherited Create(nil);
  Init;
  SetStream(Stream);
end;

destructor ATCPClient.Destroy;
begin
  FActive := False;
  SetCollection(nil);
  Terminate;
  FreeAndNil(FTagObject);
  inherited Destroy;
end;

procedure ATCPClient.Init;
begin
  FState := csClosed;
  FRunInThread := False;
  FTimeOut := DefaultSocketStreamTimeOut;
  FStreamMode := smAsynchronous;
  ThrottleRead := False;
  FReadThrottleRate := 0;
  FThrottleWrite := False;
  FWriteThrottleRate := 0;
end;

procedure ATCPClient.InitSocket;
begin
  Assert(Assigned(FSocket), 'Assigned(FSocket)');
  FSocket.OnNegotiating := DoOnSocketNegotiating;
  FSocket.OnConnected := DoOnSocketConnected;
  FSocket.OnConnectFailed := DoOnSocketConnectFailed;
  FSocket.OnDataAvailable := DoOnSocketDataAvailable;
  FSocket.OnClose := DoOnSocketClose;
  FSocket.OnStateChange := DoOnSocketStateChange;
  if Assigned(FOnInitSocket) then
    FOnInitSocket(self);
end;

procedure ATCPClient.InitFromSocket(const Socket: TTCPClientSocket);
begin
  Assert(Assigned(Socket), 'Assigned(Socket)');
  FSocket := Socket;
  FHost := Socket.Host;
  FPort := Socket.Port;
  if Socket.Connected then
    SetState(csConnected) else
  if Socket.Connecting then
    SetState(csConnecting);
  InitSocket;
end;

procedure ATCPClient.InitNewSocket;
begin
  Assert(Assigned(FSocket), 'Assigned(Socket)');
  FSocket.LocalHost := FLocalHost;
  FSocket.LocalPort := FLocalPort;
  FSocket.ThrottleRead := FThrottleRead;
  FSocket.ReadThrottleRate := FReadThrottleRate;
  FSocket.ThrottleWrite := FThrottleWrite;
  FSocket.WriteThrottleRate := FWriteThrottleRate;
  InitSocket;
end;

procedure ATCPClient.AssignPrototype(const Prototype: ATCPClient);
begin
  CheckInactive;
  if not Assigned(Prototype) then
    raise ETCPClient.Create('No prototype');

  SetHost(Prototype.FHost);
  SetPort(Prototype.FPort);
  SetLocalHost(Prototype.FLocalHost);
  SetProxy(Prototype.FProxy);
  SetStreamMode(Prototype.FStreamMode);
  FTimeOut := Prototype.FTimeOut;
  FRunInThread := Prototype.FRunInThread;
  FLogTo := Prototype.FLogTo;

  FOnLog := Prototype.FOnLog;
  FOnStateChange := Prototype.FOnStateChange;
  FOnSocketStateChange := Prototype.FOnSocketStateChange;
  FOnNegotiating := Prototype.FOnNegotiating;
  FOnConnected := Prototype.FOnConnected;
  FOnConnectFailed := Prototype.FOnConnectFailed;
  FOnDataAvailable := Prototype.FOnDataAvailable;
  FOnLineAvailable := Prototype.FOnLineAvailable;
  FOnClose := Prototype.FOnClose;

  FOnSyncSocketStateChange := Prototype.FOnSyncSocketStateChange;
  FOnSyncNegotiating := Prototype.FOnSyncNegotiating;
  FOnSyncConnected := Prototype.FOnSyncConnected;
  FOnSyncConnectFailed := Prototype.FOnSyncConnectFailed;
  FOnSyncDataAvailable := Prototype.FOnSyncDataAvailable;
  FOnSyncLineAvailable := Prototype.FOnSyncLineAvailable;
  FOnSyncClose := Prototype.FOnSyncClose;

  FOnThreadRun := Prototype.FOnThreadRun;
  FOnThreadRunComplete := Prototype.FOnThreadRunComplete;

  FThrottleWrite := Prototype.FThrottleWrite;
  FWriteThrottleRate := Prototype.FWriteThrottleRate;
  FThrottleRead := Prototype.FThrottleRead;
  FReadThrottleRate := Prototype.FReadThrottleRate;
end;

procedure ATCPClient.SetError(const Code: Integer; const Msg: String);
begin
  FErrorCode := Code;
  FErrorMsg := Msg;
end;

procedure ATCPClient.Log(const Msg: String);
begin
  if Assigned(FOnLog) then
    FOnLog(self, Msg);
  if Assigned(FLogTo) then
    FLogTo.Log(self, lcInfo, Msg);
end;

procedure ATCPClient.ThreadLog(const Msg: String);
begin
  if Assigned(FThread) and not FThread.Suspended then
    TTCPClientThread(FThread).Log(Msg);
end;

procedure ATCPClient.Terminate;
begin
  if not FTerminated and FActive and (FErrorCode = 0) then
    SetError(SocketGeneralAbortError, 'Client terminated');
  FTerminated := True;
  ClearThread;
  ClearSocket;
end;

procedure ATCPClient.ClearThread;
var T : TThreadEx;
begin
  T := FThread;
  if Assigned(T) then
    begin
      FThread := nil;
      FActive := False;
      try
        T.Terminate;
        T.WaitFor;
      except
      end;
    end;
end;

procedure ATCPClient.ClearSocket;
var R : Boolean;
begin
  R := Assigned(FStream) and FStream.SocketOwner;
  FreeAndNil(FStream);
  if not R then
    FreeAndNil(FSocket) else
    FSocket := nil;
  SetState(csClosed);
end;

procedure ATCPClient.CloseSocket;
begin
  if Assigned(FSocket) then
    FSocket.Close(True);
end;

procedure ATCPClient.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and Assigned(AComponent) then
    if AComponent = FProxy then
      FProxy := nil else
    if AComponent = FCollection then
      FCollection := nil else
    if AComponent = FLogTo then
      FLogTo := nil;
  inherited Notification(AComponent, Operation);
end;

procedure ATCPClient.Loaded;
begin
  inherited Loaded;
  if FLoadActive then  // Component was loaded with Active property True
    SetActive(True);   // Set Active after all properties have been loaded
end;

procedure ATCPClient.SetCollection(const Collection: TTCPClientCollection);
var C : TTCPClientCollection;
begin
  C := FCollection;
  if C = Collection then
    exit;
  if Assigned(C) then
    begin
      FCollection := nil;
      C.Remove(self);
    end;
  if Assigned(Collection) then
    begin
      FCollection := Collection;
      Collection.Add(self);
    end;
end;

procedure ATCPClient.Release;
begin
  if Assigned(FSocket) then
    With FSocket do
      begin
        OnNegotiating := nil;
        OnConnected := nil;
        OnConnectFailed := nil;
        OnDataAvailable := nil;
        OnClose := nil;
        OnStateChange := nil;
      end;
  if Assigned(FStream) then
    FStream.OnBlockNotify := nil;
  FStream := nil;
  FSocket := nil;
  FThread := nil;
  FActive := False;
  FState := csClosed;
end;

procedure ATCPClient.RaiseError(const Msg: String);
begin
  raise ETCPClient.Create(Msg);
end;

procedure ATCPClient.CheckInactive;
begin
  if not (csDesigning in ComponentState) and FActive then
    RaiseError('Client active');
end;

procedure ATCPClient.SetSocket(const Socket: TTCPClientSocket);
begin
  ClearSocket;
  InitFromSocket(Socket);
end;

procedure ATCPClient.SetStream(const Stream: TSocketStream);
begin
  Assert(Assigned(Stream), 'Assigned(Stream)');
  if not (Stream.Socket is TTCPClientSocket) then
    RaiseError('Socket must be derived from TTCPClientSocket');

  SetSocket(TTCPClientSocket(Stream.Socket));

  FStreamMode := Stream.StreamMode;
  FStream := Stream;
end;

procedure ATCPClient.SetHost(const Host: String);
begin
  if FHost = Host then
    exit;
  CheckInactive;
  FHost := Host;
end;

procedure ATCPClient.SetPort(const Port: String);
begin
  if FPort = Port then
    exit;
  CheckInactive;
  FPort := Port;
end;

procedure ATCPClient.SetProxy(const Proxy: ATCPClientConnectionProxy);
begin
  if FProxy = Proxy then
    exit;
  CheckInactive;
  FProxy := Proxy;
end;

procedure ATCPClient.SetStreamMode(const StreamMode: TSocketStreamMode);
begin
  if FStreamMode = StreamMode then
    exit;
  CheckInactive;
  FStreamMode := StreamMode;
end;

function ATCPClient.GetLocalHost: String;
begin
  if Assigned(FSocket) then
    Result := FSocket.LocalHost else
    Result := FLocalHost;
end;

procedure ATCPClient.SetLocalHost(const LocalHost: String);
begin
  CheckInactive;
  if Assigned(FSocket) then
    FSocket.LocalHost := LocalHost else
    FLocalHost := LocalHost;
end;

function ATCPClient.GetLocalPort: String;
begin
  if Assigned(FSocket) then
    Result := FSocket.LocalPort else
    Result := FLocalPort;
end;

procedure ATCPClient.SetLocalPort(const LocalPort: String);
begin
  if Assigned(FSocket) then
    FSocket.LocalPort := LocalPort else
    FLocalPort := LocalPort;
end;

function ATCPClient.GetThrottleWrite: Boolean;
begin
  if Assigned(FSocket) then
    Result := FSocket.ThrottleWrite else
    Result := FThrottleWrite;
end;

procedure ATCPClient.SetThrottleWrite(const ThrottleWrite: Boolean);
begin
  if Assigned(FSocket) then
    FSocket.ThrottleWrite := ThrottleWrite;
  FThrottleWrite := ThrottleWrite;
end;

function ATCPClient.GetWriteThrottleRate: Integer;
begin
  if Assigned(FSocket) then
    Result := FSocket.WriteThrottleRate else
    Result := FWriteThrottleRate;
end;

procedure ATCPClient.SetWriteThrottleRate(const ThrottleRate: Integer);
begin
  if Assigned(FSocket) then
    FSocket.WriteThrottleRate := ThrottleRate;
  FWriteThrottleRate := ThrottleRate;
end;

function ATCPClient.GetThrottleRead: Boolean;
begin
  if Assigned(FSocket) then
    Result := FSocket.ThrottleRead else
    Result := FThrottleRead;
end;

procedure ATCPClient.SetThrottleRead(const ThrottleRead: Boolean);
begin
  if Assigned(FSocket) then
    FSocket.ThrottleRead := ThrottleRead;
  FThrottleRead := ThrottleRead;
end;

function ATCPClient.GetReadThrottleRate: Integer;
begin
  if Assigned(FSocket) then
    Result := FSocket.ReadThrottleRate else
    Result := FReadThrottleRate;
end;

procedure ATCPClient.SetReadThrottleRate(const ThrottleRate: Integer);
begin
  if Assigned(FSocket) then
    FSocket.ReadThrottleRate := ThrottleRate;
  FReadThrottleRate := ThrottleRate;
end;

procedure ATCPClient.BeforeActivation;
begin
end;

procedure ATCPClient.BeforeDeactivation;
begin
end;

procedure ATCPClient.ClientInactive;
begin
end;

procedure ATCPClient.SetActive(const Active: Boolean);
begin
  if FActive = Active then
    exit;
  if Active and (csLoading in ComponentState) then // component being loaded with Active property True
    begin
      FLoadActive := True;
      exit;
    end;
  if csDesigning in ComponentState then // component does not activate while designing
    begin
      FActive := Active;
      exit;
    end;
  if Active then
    begin
      BeforeActivation;
      FErrorCode := 0;
      FErrorMsg := '';
      FActive := True;
      SetState(csConnecting);
      if FRunInThread then
        begin
          FThread := TTCPClientThread.Create;
          TTCPClientThread(FThread).InitRun(self);
          FThread.Resume;
        end else
        Start;
    end
  else
    begin
      BeforeDeactivation;
      if FRunInThread then
        begin
          ClearThread;
          ClearSocket;
        end else
        Stop;
      FActive := False;
    end;
end;

procedure ATCPClient.SetState(const State: TTCPClientState);
var S : TTCPClientState;
begin
  S := FState;
  if S = State then
    exit;
  FState := State;
  TriggerStateChange(S, State);
end;

function ATCPClient.GetTerminated: Boolean;
begin
  Result := FTerminated or
            (Assigned(FThread) and FThread.Terminated) or
            (Assigned(FSocket) and FSocket.Terminated);
end;

procedure ATCPClient.Synchronize(const Method: TThreadMethod);
begin
  if Assigned(FThread) and not FThread.Suspended then
    FThread.Synchronize(Method);
end;

procedure ATCPClient.Start;
begin
  Connect;
end;

procedure ATCPClient.Stop;
begin
  CloseSocket;
end;

procedure ATCPClient.ThreadRun;
begin
  Connect;
  if Assigned(FOnThreadRun) then
    FOnThreadRun(self);
end;

procedure ATCPClient.TriggerThreadRunComplete;
begin
  FThread := nil;
  FActive := False;
  if Assigned(FOnThreadRunComplete) then
    FOnThreadRunComplete(self);
  ClearSocket; // Handles are only valid while Thread running
end;

procedure ATCPClient.SyncTriggerSocketStateChange;
begin
  if Assigned(FOnSyncSocketStateChange) then
    FOnSyncSocketStateChange(self, FSyncOldSocketState, FSyncSocketState);
end;

procedure ATCPClient.SyncTriggerNegotiating;
begin
  if Assigned(FOnSyncNegotiating) then
    FOnSyncNegotiating(self);
end;

procedure ATCPClient.SyncTriggerConnected;
begin
  if Assigned(FOnSyncConnected) then
    FOnSyncConnected(self);
end;

procedure ATCPClient.SyncTriggerConnectFailed;
begin
  if Assigned(FOnSyncConnectFailed) then
    FOnSyncConnectFailed(self);
end;

procedure ATCPClient.SyncTriggerDataAvailable;
begin
  if Assigned(FOnSyncDataAvailable) then
    FOnSyncDataAvailable(self);
end;

procedure ATCPClient.SyncTriggerLineAvailable;
begin
  if Assigned(FOnSyncLineAvailable) then
    FOnSyncLineAvailable(self);
end;

procedure ATCPClient.SyncTriggerClose;
begin
  if Assigned(FOnSyncClose) then
    FOnSyncClose(self);
end;

procedure ATCPClient.DoOnSocketStateChange(Sender: ASocket;
    OldState, State: TSocketState);
begin
  TriggerSocketStateChange(OldState, State);
end;

procedure ATCPClient.DoOnSocketNegotiating(Sender: TTCPClientSocket);
begin
  TriggerNegotiating;
end;

procedure ATCPClient.DoOnSocketConnected(Sender: TTCPClientSocket);
begin
  TriggerConnected;
end;

procedure ATCPClient.DoOnSocketConnectFailed(Sender: TTCPClientSocket);
begin
  SetError(Sender.ErrorCode, Sender.ErrorMessage);
  TriggerConnectFailed;
end;

procedure ATCPClient.DoOnSocketDataAvailable(Sender: ATCPClientSocket);
begin
  TriggerDataAvailable;
end;

procedure ATCPClient.DoOnSocketClose(Sender: ATCPClientSocket);
begin
  TriggerClose;
end;

procedure ATCPClient.DoOnStreamBlockNotify(const Sender: TSocketStream);
begin
  if Assigned(FOnStreamBlockNotify) then
    FOnStreamBlockNotify(self);
end;

procedure ATCPClient.TriggerSocketStateChange(const OldState, State: TSocketState);
begin
  if Assigned(FOnSocketStateChange) then
    FOnSocketStateChange(self, OldState, State);
  if Assigned(FOnSyncSocketStateChange) then
    begin
      FSyncOldSocketState := OldState;
      FSyncSocketState := State;
      Synchronize(SyncTriggerSocketStateChange);
    end;
end;

procedure ATCPClient.TriggerStateChange(const OldState, State: TTCPClientState);
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(self, OldState, State);
end;

procedure ATCPClient.TriggerNegotiating;
begin
  if Assigned(FOnNegotiating) then
    FOnNegotiating(self);
  if Assigned(FOnSyncNegotiating) then
    Synchronize(SyncTriggerNegotiating);
end;

procedure ATCPClient.TriggerConnected;
begin
  SetState(csConnected);
  if Assigned(FOnConnected) then
    FOnConnected(self);
  if Assigned(FOnSyncConnected) then
    Synchronize(SyncTriggerConnected);
end;

procedure ATCPClient.TriggerConnectFailed;
begin
  SetState(csClosed);
  if Assigned(FOnConnectFailed) then
    FOnConnectFailed(self);
  if Assigned(FOnSyncConnectFailed) then
    Synchronize(SyncTriggerConnectFailed);
end;

procedure ATCPClient.TriggerDataAvailable;
begin
  if Assigned(FOnDataAvailable) then
    FOnDataAvailable(self);
  if Assigned(FOnSyncDataAvailable) then
    Synchronize(SyncTriggerDataAvailable);
  if Assigned(FOnLineAvailable) or Assigned(FOnSyncLineAvailable) then
    if FSocket.IsLineAvailable > 0 then
      begin
        if Assigned(FOnLineAvailable) then
          FOnLineAvailable(self);
        if Assigned(FOnSyncLineAvailable) then
          Synchronize(SyncTriggerLineAvailable);
      end;
end;

procedure ATCPClient.TriggerClose;
begin
  SetState(csClosed);
  if Assigned(FOnClose) then
    FOnClose(self);
  if Assigned(FOnSyncClose) then
    Synchronize(SyncTriggerClose);
end;

function ATCPClient.GetSocketProxy: ATCPClientSocketProxy;
begin
  if not Assigned(FProxy) then
    Result := nil else
    Result := FProxy.GetSocketProxy(self);
end;

function ATCPClient.GetSocket: TTCPClientSocket;
begin
  if not Assigned(FSocket) then
    begin
      if FHost = '' then
        RaiseError('Host not set');
      if FPort = '' then
        RaiseError('Port not set');
      FSocket := TTCPClientSocket.Create(FHost, FPort, GetSocketProxy);
      InitNewSocket;
    end;
  Result := FSocket;
end;

function ATCPClient.GetStream: TSocketStream;
begin
  if not Assigned(FStream) then
    begin
      FStream := TSocketStream.Create(GetSocket, False, FStreamMode, FTimeOut);
      FStream.OnBlockNotify := DoOnStreamBlockNotify;
      FStream.CloseOnDestroy := False;
    end;
  Result := FStream;
end;

procedure ATCPClient.Connect;
begin
  GetSocket;
  Assert(Assigned(FSocket), 'Assigned(FSocket)');
  try
    GetStream.ConnectSocket;
  except
    if (FSocket.ErrorCode <> 0) and (FErrorCode = 0) then
      SetError(FSocket.ErrorCode, FSocket.ErrorMessage);
    raise;
  end;
end;



{                                                                              }
{ ATCPClientConnectionProxy                                                    }
{                                                                              }
constructor ATCPClientConnectionProxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Init;
end;

procedure ATCPClientConnectionProxy.Init;
begin
  FUseAuthentication := False;
  FResolveThroughProxy := True;
end;

procedure ATCPClientConnectionProxy.SetNextProxy(const NextProxy: ATCPClientConnectionProxy);
var P : ATCPClientConnectionProxy;
begin
  if Assigned(NextProxy) then
    begin
      P := NextProxy;
      While Assigned(P) do
        if P = self then
          raise ETCPClientConnectionProxy.Create('Circular proxy reference') else
          P := P.NextProxy;
    end;
  FNextProxy := NextProxy;
end;

procedure ATCPClientConnectionProxy.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
    if AComponent = FNextProxy then
      FNextProxy := nil;
  inherited Notification(AComponent, Operation);
end;

function ATCPClientConnectionProxy.GetNextSocketProxy(const Client: ATCPClient): ATCPClientSocketProxy;
begin
  if Assigned(FNextProxy) then
    Result := FNextProxy.GetSocketProxy(Client) else
    Result := nil;
end;



{                                                                              }
{ TfndSocks5Proxy                                                              }
{                                                                              }
procedure TfndSocks5Proxy.Init;
begin
  inherited Init;
  Port := '1080';
end;

function TfndSocks5Proxy.GetSocketProxy(const Client: ATCPClient): ATCPClientSocketProxy;
begin
  Result := TSocks5SocketProxy.Create(FHost, FPort, FResolveThroughProxy,
      FUseAuthentication, FUsername, FPassword, GetNextSocketProxy(Client));
end;



{                                                                              }
{ TfndHTTPTunnelProxy                                                          }
{                                                                              }
procedure TfndHTTPTunnelProxy.Init;
begin
  inherited Init;
  Port := '80';
end;

function TfndHTTPTunnelProxy.GetSocketProxy(const Client: ATCPClient): ATCPClientSocketProxy;
begin
  Result := THTTPTunnelSocketProxy.Create(FHost, FPort, FResolveThroughProxy,
      FUseAuthentication, FUsername, FPassword, GetNextSocketProxy(Client));
end;



{                                                                              }
{ TTCPClientCollection                                                         }
{                                                                              }
constructor TTCPClientCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitializeCriticalSection(FLock);
end;

destructor TTCPClientCollection.Destroy;
begin
  Clear;
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

procedure TTCPClientCollection.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and Assigned(AComponent) then
    if AComponent = FClientPrototype then
      FClientPrototype := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TTCPClientCollection.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TTCPClientCollection.Unlock;
begin
  LeaveCriticalSection(FLock);
end;

function TTCPClientCollection.Count: Integer;
begin
  Result := Length(FClients);
end;

procedure TTCPClientCollection.Clear;
var L : Integer;
begin
  Repeat
    L := Length(FClients);
    While (L > 0) and FClients[L - 1].Terminated do
      Dec(L);
    if L > 0 then
      FClients[L - 1].Terminate;
  Until L = 0;
  FreeAndNilObjectArray(ObjectArray(FClients));
end;

procedure TTCPClientCollection.Add(const Client: ATCPClient);
begin
  if not Assigned(Client) then
    exit;
  Lock;
  try
    Append(ObjectArray(FClients), Client);
  finally
    Unlock;
  end;
  ClientAdded(Client);
  if Assigned(FOnClientAdd) then
    FOnClientAdd(self, Client);
end;

procedure TTCPClientCollection.ClientAdded(const Client: ATCPClient);
begin
end;

procedure TTCPClientCollection.Remove(const Client: ATCPClient);
var I : Integer;
begin
  Lock;
  try
    I := PosNext(Client, ObjectArray(FClients));
    if I < 0 then
      exit;
    cUtils.Remove(ObjectArray(FClients), I, 1, False);
  finally
    Unlock;
  end;
  ClientRemoved(Client);
  if Assigned(FOnClientRemove) then
    FOnClientRemove(self, Client);
end;

function TTCPClientCollection.RemoveFirst: ATCPClient;
var L: Integer;
begin
  Lock;
  try
    L := Length(FClients);
    if L = 0 then
      begin
        Result := nil;
        exit;
      end else
      begin
        Result := FClients[0];
        cUtils.Remove(ObjectArray(FClients), 0, 1, False);
      end;
  finally
    Unlock;
  end;
  ClientRemoved(Result);
  if Assigned(FOnClientRemove) then
    FOnClientRemove(self, Result);
end;

procedure TTCPClientCollection.ClientRemoved(const Client: ATCPClient);
begin
end;

function TTCPClientCollection.Start(const Client: ATCPClient): Boolean;
begin
  if not Assigned(Client) then
    begin
      Result := False;
      exit;
    end;
  Client.Collection := self;
  Client.Active := True;
  Result := True;
end;

procedure TTCPClientCollection.TriggerCreateClient(var Client: ATCPClient);
begin
  if Assigned(FOnCreateClient) then
    FOnCreateClient(self, Client);
end;

function TTCPClientCollection.CreateClient: ATCPClient;
begin
  // Create client
  Result := nil;
  TriggerCreateClient(Result);
  if not Assigned(Result) then
    begin
      if not Assigned(FClientPrototype) then
        raise ETCPClientCollection.Create('No client prototype');
      // Create client from prototype
      Result := ATCPClientClass(FClientPrototype.ClassType).Create(FClientPrototype.Owner);
    end;
  // Initialize new client from prototype
  if Assigned(FClientPrototype) then
    Result.AssignPrototype(FClientPrototype);
  // Notify
  if Assigned(FOnClientCreate) then
    FOnClientCreate(self, Result);
end;

function TTCPClientCollection.CreateAndAdd: ATCPClient;
begin
  Result := CreateClient;
  Result.Collection := self;
end;

function TTCPClientCollection.CreateAndStart: ATCPClient;
begin
  Result := CreateClient;
  Start(Result);
end;

function TTCPClientCollection.ActiveCount: Integer;
var I: Integer;
begin
  Result := 0;
  Lock;
  try
    For I := 0 to Length(FClients) - 1 do
      if FClients[I].Active then
        Inc(Result);
  finally
    Unlock;
  end;
end;



end.

