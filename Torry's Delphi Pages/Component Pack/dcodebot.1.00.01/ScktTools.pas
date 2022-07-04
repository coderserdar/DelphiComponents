
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit ScktTools;

interface

{$I STD.INC}

uses
  Classes, ScktComp, SysUtils, Windows, Messages, WinTools, FileTools, WinSock;

const
  WM_TERMINATE = WM_USER + 1;

type
  TRedirectSocket = class(TServerSocket)
  private
    FRedirector: TRedirector;
    FUtilityWindow: TUtilityWindow;
    procedure ClientConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure ClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure RedirectData(Sender: TObject; Buffer: Pointer; Size: Integer);
  public
    destructor Destroy; override;
    procedure DefaultHandler(var Message); override;
    procedure Run;
    procedure Terminate;
  end;

implementation

destructor TRedirectSocket.Destroy;
begin
  Active := False;
  FRedirector.Free;
  FUtilityWindow.Free;
  inherited Destroy;
end;

procedure TRedirectSocket.DefaultHandler(var Message);
begin
  inherited DefaultHandler(Message);
  case TMessage(Message).Msg of
    WM_CLOSE, WM_TERMINATE: Terminate;
  end;
end;

procedure TRedirectSocket.Run;
var
  Mutex: THandle;
  Msg: TMsg;
begin
  Mutex := CreateMutex(nil, False, 'sparrow');
  try
    if (Mutex <> 0) and (GetLastError <> ERROR_ALREADY_EXISTS) then
    begin
      FUtilityWindow := TUtilityWindow.Create(Self);
      OnClientConnect := ClientConnect;
      OnClientRead := ClientRead;
      FRedirector := TRedirector.Create;
      FRedirector.KillOnDestroy := True;
      FRedirector.ShowState := ssNormal;
      FRedirector.Executable := GetConsolePath;
      FRedirector.OnData := RedirectData;
      FRedirector.OnErrorData := RedirectData;
      if FRedirector.Executable <> '' then
      begin
        Port := 81;
        Active := True;
        while GetMessage(Msg, 0, 0, 0) do
          DispatchMessage(Msg);
      end;
    end;
  finally
    if Mutex <> 0 then
      CloseHandle(Mutex);
  end;
end;

procedure TRedirectSocket.Terminate;
begin
  Active := False;
  PostQuitMessage(0);
end;

procedure TRedirectSocket.ClientConnect(Sender: TObject; Socket: TCustomWinSocket);
var
  Connection: TCustomWinSocket;
begin
  if (Self.Socket.ActiveConnections = 0) or (Socket = Self.Socket.Connections[0]) then
  begin
    if not FRedirector.Running then FRedirector.Execute;
  end
  else
  begin
    Connection := Self.Socket.Connections[0];
    Connection.SendText('GOODBYE');
    Connection.Close;
    Connection.Free;
  end;
end;

procedure TRedirectSocket.ClientRead(Sender: TObject; Socket: TCustomWinSocket);
var
  Command: string;
  S: string;
begin
  S := Socket.ReceiveText;
  Command := UpperCase(S);
  if Command = 'QUIT'#13#10 then
    Terminate
  else
    FRedirector.SendText(S);
end;

procedure TRedirectSocket.RedirectData(Sender: TObject; Buffer: Pointer; Size: Integer);
var
  S: string;
begin
  SetString(S, PChar(Buffer), Size);
  if Socket.ActiveConnections > 0 then
    Socket.Connections[0].SendText(S);
end;

end.
