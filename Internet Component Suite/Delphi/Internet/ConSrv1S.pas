{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Server handling
Creation:     Feb 17, 1999
Version:      1.00

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit ConSrv1S;

interface

uses
  Windows, SysUtils, Messages, Classes, WSocket, WinSock, ConSrv1C;

type
    // TServerObject handle all clients sessions
    TServerObject = class (TObject)
    protected
        FSrvWSocket : TWSocket;
        FCliList    : TList;
        FCtrlWindow : HWND;
        procedure SessionAvailableHandler(Sender : TObject; Error : Word);
    public
        constructor Create; virtual;
        destructor  Destroy; override;
        procedure StartServer;
        procedure DisconnectedClient(Client : TClientObject);
        procedure DisplayClientList;
        property CtrlWindow : HWND     read  FCtrlWindow
                                       write FCtrlWindow;
        property SrvWSocket : TWSocket read  FSrvWSocket;
    end;


implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TServerObject.Create;
begin
    inherited Create;
    FCliList    := TList.Create;
    FSrvWSocket := TWSocket.Create(nil);
    StartServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TServerObject.Destroy;
begin
    if Assigned(FSrvWSocket) then begin
        FSrvWSocket.Destroy;
        FSrvWSocket := nil;
    end;
    if Assigned(FCliList) then begin
        FCliList.Destroy;
        FCliList := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerObject.StartServer;
begin
    FSrvWSocket.Proto := 'tcp';
    FSrvWSocket.Port  := 'telnet';
    FSrvWSocket.Addr  := '0.0.0.0';
    FSrvWSocket.OnSessionAvailable := SessionAvailableHandler;
    FSrvWSocket.Listen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerObject.SessionAvailableHandler(Sender : TObject; Error : Word);
var
    Client : TClientObject;
    ASocket : TSocket;
    SAddr    : TSockAddrIn;
    SAddrLen : integer;
begin
    Write('Client connected: ');

    // Create a new object to handle client session
    Client  := TClientObject.Create;

    // Add to our client list
    FCliList.Add(Client);

    // Accept the connection
    ASocket := FSrvWSocket.Accept;

    // Determine who has connected before really starting the session
    SAddrLen := SizeOf(SAddr);
    WSocket_getpeername(ASocket, SAddr, SAddrLen);
    WriteLn(WSocket_inet_ntoa(SAddr.sin_addr));
    WriteLn('There are: ', FCliList.Count, ' connected clients.');

    // Startup the client connection (send banner)
    Client.CtrlWindow := FCtrlWindow;
    Client.StartClient(ASocket);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is called to disconnect a client and remove it from our    }
{ client list.                                                              }
procedure TServerObject.DisconnectedClient(Client : TClientObject);
var
    Index : Integer;
begin

    // Search client in our list
    Index := FCliList.IndexOf(Client);
    if Index < 0 then
        Exit;    // Not found already disconnected

    FCliList.Delete(Index);
    WriteLn('Client ', Client.PeerName, ' disconnected');
    WriteLn('Remains: ', FCliList.Count, ' connected clients.');

    // Then destroy client (this will abort the session if still active)
    Client.Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerObject.DisplayClientList;
var
    I      : Integer;
    Client : TClientObject;
begin
    WriteLn('There are ', FCliList.Count, ' connected client.');
    for I := 1 to FCliList.Count do begin
        Client := FCliList.Items[I - 1];
        WriteLn(I:3, ': ', Client.PeerName);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

