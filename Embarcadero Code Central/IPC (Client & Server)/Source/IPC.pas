{

IPC Version 1.0 - A Simple VCL Encapsulation of the SendMessage API

VCL Classes in this Unit:
  TIPCClient - A IPC Client (derived from TComponent)
  TICPServer - A IPC Server (derived from TComponent)

Other classes in this Unit:
  None:

Legal issues:

Copyright (C) 2000 by Diversified Eng. Service (DES) & Bill Nemmers <billnemmers@csi.com>

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the author be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented, you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.

  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.

  3. This notice may not be removed or altered from any source distribution.

  4. These components may not be included in any component package that is
      distributed for profit.

Credits go to:

 Robert T. Palmqvist. I copy his header for these comments

History:
    Ver 1.00 Released 05/10/2000
        No bugs found yet

Server Properties
  Active:		True if a valid handle was created.
  SessionName:		This is the common name that the Server and Client will use to
			Make a connection. Set the same the same in the Server and Client.

Server Methods:
  Create:		Just like the others.
  Open:	                Open the server and make ready for client connection.
  Close:		Broadcast message to all clients that server is closing and close hanlde.
  SendMsg:		Send out this message to a client.

Server Events:
  OnClientData:		When a data packed is received.
  OnConnect:		When a client is connected.
  OnDisconnected 	When a client is disconnected.
  OnAfteOpen            Fired after the session is opened.
  OnAfterClose          Fired after the session is closed.

Client Properties
  Active:		True if a valid handle was received from the server.
  SessionName:		This is the common name that the Server and Client will use to
			Make a connection. Set the same the same in the Server and Client.

Client Methods:
  Create:		Just like the others.
  Open:	                To open a connection to a server with the same Session Name.
  Close:		Close the connection with the server.
  SendMsg:		Send out this message to the server.

Client Events:
  OnData:		When a data packed is received
  OnConnect:		When a client is connected.
  OnDisconnected 	When a client is disconnected.

}

unit IPC;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

const
  IPCServerDisconneting = 'IPCServerDisconneting';
  IPCClientDisconneting = 'IPCClientDisconneting';
  IPCConnectRequest = 'IPCConnectRequest';
  IPCConnectRespose = 'IPCConnectRespose';

type
  TOnData = procedure(MsgPointer: Pointer) of object;
  TOnClientData = procedure(MsgPointer: Pointer; AHwnd: HWND) of object;
  TOnConnect = procedure(AHwnd: HWND) of object;
  TOnDisconnect = procedure(AHwnd: HWND) of object;

  TIPCServer = class(TComponent)
  private
    FWinHwnd: HWND;
    FOnClientData: TOnClientData;
    FOnConnect: TOnConnect;
    FOnDisconnect: TOnDisconnect;
    FActive: Boolean;
    FSessionName: string;
    FSessionHandle: Longint;
    FServerDisconnetHwnd: Longword;
    FConnectRequestHwnd: Longword;
    FConnectResposeHwnd: Longword;
    FOnAfterClose: TNotifyEvent;
    FOnAfterOpen: TNotifyEvent;
  protected
    procedure WndProc(var AMsg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendMsg(MsgPointer: Pointer; AWinHwnd: HWND; ASize: DWORD);
    procedure Open;
    procedure Close;
  published
    property Active: Boolean read FActive default False;
    property OnClientData: TOnClientData read FOnClientData write FOnClientData;
    property OnConnect: TOnConnect read FOnConnect write FOnConnect;
    property OnDisconnect: TOnDisconnect read FOnDisconnect write FOnDisconnect;
    property OnAfterClose: TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnAfterOpen: TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property SessionName: string read FSessionName write FSessionName;
  end;

  TIPCClient = class(TComponent)
  private
    FWinHwnd: HWND;
    FServerWinHwnd: HWND;
    FOnData: TOnData;
    FOnConnect: TOnConnect;
    FOnDisconnect: TOnDisconnect;
    FActive: Boolean;
    FSessionName: string;
    FSessionHandle: Longint;
    FServerDisconnetHwnd: Longword;
    FConnectRequestHwnd: Longword;
    FConnectResposeHwnd: Longword;
    FClientDisconnetHwnd: Longword;
  protected
    procedure WndProc(var AMsg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendMsg(MsgPointer: Pointer; ASize: DWORD);
    procedure Open;
    procedure Close;
  published
    property Active: Boolean read FActive default False;
    property OnData: TOnData read FOnData write FOnData;
    property OnConnect: TOnConnect read FOnConnect write FOnConnect;
    property OnDisconnect: TOnDisconnect read FOnDisconnect write FOnDisconnect;
    property SessionName: string read FSessionName write FSessionName;
  end;

procedure Register;

implementation

//TIPCServer
constructor TIPCServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWinHwnd := 0;
  FSessionHandle := 0;
  FServerDisconnetHwnd := RegisterWindowMessage(IPCServerDisconneting);
  FConnectRequestHwnd := RegisterWindowMessage(IPCConnectRequest);
  FConnectResposeHwnd := RegisterWindowMessage(IPCConnectRespose);
end;

destructor TIPCServer.Destroy;
begin
  if FWinHwnd <> 0 then
  begin
    SendMessage(HWND_BROADCAST, FServerDisconnetHwnd, FWinHwnd, 0);
    DeallocateHWND(FWinHwnd);
  end;
  inherited Destroy;
end;

procedure TIPCServer.Open;
begin
  if not FActive then
  begin
    FSessionHandle := RegisterWindowMessage(PChar(FSessionName));
    FWinHwnd := AllocateHWND(WndProc);
    if FWinHwnd <> 0 then
    begin
      FActive := True;
      if Assigned(FOnAfterOpen) then
        FOnAfterOpen(Self);
    end
    else
      raise Exception.Create('Cannot Allocate Window Handle');
  end;
end;

procedure TIPCServer.Close;
begin
  if FActive then
  begin
    if FWinHwnd <> 0 then
    begin
      SendMessage(HWND_BROADCAST, FServerDisconnetHwnd, FWinHwnd, FSessionHandle);
      DeallocateHWND(FWinHwnd);
      FWinHwnd := 0;
      FActive := False;
      if Assigned(FOnAfterClose) then
        FOnAfterClose(Self);
    end;
  end;
end;

procedure TIPCServer.WndProc(var AMsg: TMessage);
var
  MsgPointer: Pointer;
  ClientHwnd: HWND;
begin
  if ((AMsg.Msg = WM_COPYDATA) and (AMsg.wParam = FSessionHandle)) then
  begin
    MsgPointer := (TCopyDataStruct(Pointer(AMsg.lParam)^)).lpData;
    ClientHwnd := (TCopyDataStruct(Pointer(AMsg.lParam)^)).dwData;
    if Assigned(FOnClientData) then
      FOnClientData(MsgPointer, ClientHwnd);
  end
  else if ((AMsg.Msg = FConnectRequestHwnd) and (AMsg.lParam = FSessionHandle)) then
  begin
    if FActive then
    begin
      ClientHwnd := AMsg.wParam;
      if ClientHwnd <> 0 then
      begin
        SendMessage(ClientHwnd, FConnectResposeHwnd, FWinHwnd, 0);
        if Assigned(FOnConnect) then
          FOnConnect(ClientHwnd);
      end;
    end;
  end ;
end;

procedure TIPCServer.SendMsg(MsgPointer: Pointer; AWinHwnd: HWND; ASize: DWORD);
var
  CopyDataStruct: TCopyDataStruct;
begin
  CopyDataStruct.dwData := 0;
  CopyDataStruct.cbData := ASize;
  CopyDataStruct.lpData := MsgPointer;
  SendMessage(AWinHwnd, WM_COPYDATA, FSessionHandle, lParam(@CopyDataStruct));
end;

//TIPCClient
constructor TIPCClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWinHwnd := 0;
  FServerWinHwnd := 0;
  FSessionHandle := 0;
  FServerDisconnetHwnd := RegisterWindowMessage(IPCServerDisconneting);
  FConnectRequestHwnd := RegisterWindowMessage(IPCConnectRequest);
  FConnectResposeHwnd := RegisterWindowMessage(IPCConnectRespose);
  FClientDisconnetHwnd := RegisterWindowMessage(IPCClientDisconneting);
end;

destructor TIPCClient.Destroy;
begin
  if FWinHwnd <> 0 then
    DeallocateHWND(FWinHwnd);
  inherited Destroy;
end;

procedure TIPCClient.Open;
begin
  if not FActive then
  begin
    FSessionHandle := RegisterWindowMessage(PChar(FSessionName));
    FWinHwnd := AllocateHWND(WndProc);
    if FWinHwnd <> 0 then
      SendMessage(HWND_BROADCAST, FConnectRequestHwnd, FWinHwnd, FSessionHandle);
  end;
end;

procedure TIPCClient.Close;
begin
  if FActive then
  begin
    if FWinHwnd <> 0 then
    begin
      SendMessage(HWND_BROADCAST, FClientDisconnetHwnd, FWinHwnd, FSessionHandle);
      DeallocateHWND(FWinHwnd);
      FWinHwnd := 0;
      FActive := False;
    end;
  end;
end;

procedure TIPCClient.WndProc(var AMsg: TMessage);
var
  MsgPointer: Pointer;
begin
  if ((AMsg.Msg = WM_COPYDATA) and (AMsg.wParam = FSessionHandle)) then
  begin
    MsgPointer := (TCopyDataStruct(Pointer(AMsg.lParam)^)).lpData;
    if Assigned(FOnData) then
      FOnData(MsgPointer);
  end
  else if AMsg.Msg = FConnectResposeHwnd then
  begin
    if not FActive then
    begin
      FServerWinHwnd := AMsg.wParam;
      if Assigned(FOnConnect) then
        FOnConnect(FServerWinHwnd);
      FActive := True;
    end;
  end
  else if AMsg.Msg = FServerDisconnetHwnd then
  begin
    if FActive then
    begin
      if AMsg.wParam = Longint(FServerWinHwnd) then
      begin
        if Assigned(FOnDisconnect) then
          FOnDisconnect(FServerWinHwnd);
        FServerWinHwnd := 0;
        FActive := False;
      end;
    end;
  end;
end;

procedure TIPCClient.SendMsg(MsgPointer: Pointer; ASize: DWORD);
var
  CopyDataStruct: TCopyDataStruct;
begin
  if FServerWinHwnd <> 0 then
  begin
    CopyDataStruct.dwData := FWinHwnd;
    CopyDataStruct.cbData := ASize;
    CopyDataStruct.lpData := MsgPointer;
    SendMessage(FServerWinHwnd, WM_COPYDATA, FSessionHandle, lParam(@CopyDataStruct));
  end;
end;

procedure Register;
begin
  RegisterComponents('DES', [TIPCServer, TIPCClient]);
end;

end.
