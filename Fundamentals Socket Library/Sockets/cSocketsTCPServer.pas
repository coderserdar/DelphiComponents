{$INCLUDE ..\cDefines.inc}
unit cSocketsTCPServer;

{                                                                              }
{                        TCP server socket class v3.03                         }
{                                                                              }
{       This unit is copyright © 2001-2003 by David Butler (david@e.co.za)     }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{               Its original file name is cSocketsTCPServer.pas                }
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
{   11/12/2001  0.01  Initial version.                                         }
{   04/09/2002  3.02  Refactored for Fundamentals 3.                           }
{   03/05/2003  3.03  Added PendingConnections property.                       }
{                                                                              }

interface

uses
  { Delphi }
  WinSock,

  { Fundamentals }
  cWinSock,
  cSockets;



{                                                                              }
{ TTCPServerSocket                                                             }
{   A TCP server socket.                                                       }
{                                                                              }
const
  DefaultBacklog = 4;

type
  TTCPServerSocket = class;
  TTCPServerSocketEvent = procedure (Sender: TTCPServerSocket) of object;
  TTCPServerSocket = class(ASocket)
  protected
    FListenPort            : String;
    FMaxBacklog            : Integer;
    FOnListen              : TTCPServerSocketEvent;
    FOnConnectionAvailable : TTCPServerSocketEvent;
    FPendingConnections    : Integer;

    procedure Init; override;

    procedure WMSocket(const Events, lWordHi: Word); override;
    function  GetAsynchronousEvents: LongInt; override;

    procedure HandleAcceptEvent;
    procedure TriggerConnectionAvailable; virtual;

    procedure SetListenPort(const ListenPort: String);
    procedure SetMaxBacklog(const MaxBacklog: Integer);

  public
    constructor Create(const ListenPort: String;
                const MaxBacklog: Integer = DefaultBacklog); reintroduce; overload;

    property  ListenPort: String read FListenPort write SetListenPort;
    property  MaxBacklog: Integer read FMaxBacklog write SetMaxBacklog default DefaultBacklog;

    procedure Listen;
    procedure Close;
    function  SocketAccept(var Address: TSockAddr): TSocket;
    property  PendingConnections: Integer read FPendingConnections;

    property  OnListen: TTCPServerSocketEvent read FOnListen write FOnListen;
    property  OnConnectionAvailable: TTCPServerSocketEvent read FOnConnectionAvailable write FOnConnectionAvailable;
  end;



implementation



{                                                                              }
{ TTCPServerSocket                                                             }
{                                                                              }
constructor TTCPServerSocket.Create(const ListenPort: String;
    const MaxBacklog: Integer);
begin
  inherited Create(spTCP);
  SetListenPort(ListenPort);
  SetMaxBacklog(MaxBacklog);
end;

procedure TTCPServerSocket.Init;
begin
  inherited Init;
  FProtocol := spTCP;
  FMaxBacklog := DefaultBacklog;
end;

procedure TTCPServerSocket.SetMaxBacklog(const MaxBacklog: Integer);
begin
  if MaxBacklog = FMaxBacklog then
    exit;
  CheckStateClosed('SetMaxBacklog');
  FMaxBacklog := MaxBacklog;
end;

procedure TTCPServerSocket.SetListenPort(const ListenPort: String);
begin
  if ListenPort = FListenPort then
    exit;
  CheckStateClosed('SetListenPort');
  FListenPort := ListenPort;
  SetLocalPort(ListenPort);
end;

function TTCPServerSocket.GetAsynchronousEvents: LongInt;
begin
  Result := FD_ACCEPT;
end;

procedure TTCPServerSocket.WMSocket(const Events, lWordHi: Word);
begin
  if Events and FD_ACCEPT <> 0 then
    HandleAcceptEvent;
end;

procedure TTCPServerSocket.TriggerConnectionAvailable;
begin
  if Assigned(FOnConnectionAvailable) then
    FOnConnectionAvailable(self);
end;

procedure TTCPServerSocket.HandleAcceptEvent;
begin
  Inc(FPendingConnections);
  TriggerConnectionAvailable;
end;

procedure TTCPServerSocket.Listen;
begin
  CheckStateClosed('Listen');
  BindLocalAddress;
  Assert(FSocketHandle <> INVALID_SOCKET, 'FSocketHandle <> INVALID_SOCKET');
  SetSocketAsynchronous;
  if Winsock.listen(GetSocketHandle, FMaxBacklog) <> 0 then
    RaiseLastWinSockError('Listen failed');
  SetState(ssListening);
  if Assigned(FOnListen) then
    FOnListen(self);
end;

procedure TTCPServerSocket.Close;
begin
  if FSocketHandle = INVALID_SOCKET then
    exit;
  WinSock.CloseSocket(FSocketHandle);
  FSocketHandle := INVALID_SOCKET;
  FPendingConnections := 0;
  SetState(ssClosed);
end;

function TTCPServerSocket.SocketAccept(var Address: TSockAddr): TSocket;
var Len : Integer;
begin
  if FState <> ssListening then
    RaiseError('Socket closed', -1);
  Assert(FSocketHandle <> INVALID_SOCKET, 'FSocketHandle <> INVALID_SOCKET');
  Len := Sizeof(TSockAddr);
  Result := WinSock.accept(FSocketHandle, @Address, @Len);
  if Result = INVALID_SOCKET then
    begin
      FillChar(Address, Sizeof(Address), #0);
      FPendingConnections := 0;
    end
  else
    if FPendingConnections > 0 then
      Dec(FPendingConnections);
end;



end.

