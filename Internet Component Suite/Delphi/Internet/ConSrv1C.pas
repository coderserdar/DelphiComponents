{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Client handling
Creation:     Feb 17, 1999
Version:      1.00

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit ConSrv1C;

interface

uses
    Windows, SysUtils, Messages, Classes, WSocket, WinSock;

const
    WM_CLIENT_DISCONNECTED = WM_USER + 1;
    CmdPrompt = #13#10 + '--> ';

type
    // TClientObject handle a single client communication
    TClientObject = class (TObject)
    protected
        FCliWSocket    : TWSocket;
        FCtrlWindow    : HWND;
        FCommand       : array [0..2047] of char;
        FCmdLen        : Integer;
        FPeerName      : String;
        procedure DataAvailableHandler(Sender : TObject; Error : Word);
        procedure SessionClosedHandler(Sender : TObject; Error : Word);
        procedure CommandInterpreter;
        procedure TELNET_Interpreter(CommandVerb : String;
                                     CommandTail : String);
    public
        constructor Create; virtual;
        destructor  Destroy; override;
        procedure   StartClient(ASocket : TSocket);
        property CtrlWindow : HWND     read  FCtrlWindow
                                       write FCtrlWindow;
        property PeerName : String     read  FPeerName;
    end;

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TClientObject.Create;
begin
    inherited Create;
    FCliWSocket := TWSocket.Create(nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TClientObject.Destroy;
begin
    if Assigned(FCliWSocket) then begin
        FCliWSocket.Destroy;
        FCliWSocket := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientObject.StartClient(ASocket : TSocket);
begin
    FCliWSocket.LineMode        := TRUE;
    FCliWSocket.LineEnd         := #13#10;
    FCliWSocket.LineEcho        := TRUE;
    FCliWSocket.LineEdit        := TRUE;
    FCliWSocket.OnDataAvailable := DataAvailableHandler;
    FCliWSocket.OnSessionClosed := SessionClosedHandler;
    FCliWSocket.HSocket         := ASocket;
    FPeerName                   := FCliWSocket.GetPeerAddr;
    FCliWSocket.SendStr('Welcome to ConSrv' + CmdPrompt);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientObject.SessionClosedHandler(Sender : TObject; Error : Word);
begin
    PostMessage(FCtrlWindow, WM_CLIENT_DISCONNECTED, 0, LParam(Self));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// This handler is called each time we received a complete line from
// connected client (remember we use line mode)
procedure TClientObject.DataAvailableHandler(Sender : TObject; Error : Word);
begin
    // Get data from socket component. We should receive a complete line.
    FCmdLen := FCliWSocket.Receive(@FCommand, SizeOf(FCommand) - 1);
    if FCmdLen <= 0 then
        Exit;    // No data available
    if FCliWSocket.State <> wsConnected then
        Exit;    // Ignore any data received while closing

    // Remove trailling CR/LF, if any (could be missing if our buffer
    // was too small)
    while (FCmdLen > 0) and (FCommand[FCmdLen - 1] in [#13, #10]) do
        Dec(FCmdLen);
    FCommand[FCmdLen] := #0;

    // Interpret received command
    CommandInterpreter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This is the command line interpreter. Should extend the code to support   }
{ every command needed...                                                   }
procedure TClientObject.CommandInterpreter;
var
    CommandVerb : String;
    CommandTail : String;
    I, J        : Integer;
begin
    { Skip leading spaces }
    I := 0;
    while (I < FCmdLen) and (FCommand[I] in [' ', #9]) do
        Inc(I);

    { Find separator and separe CommandVerb and CommandTail }
    J := I;
    while TRUE do begin
        if (J >= FCmdLen) then begin
            SetLength(CommandVerb, FCmdLen - I);
            Move(FCommand[I], CommandVerb[1], Length(CommandVerb));
            CommandTail := '';
            break;
        end;

        if FCommand[J] in [' ', #9, '/'] then begin
            SetLength(CommandVerb, J - I);
            Move(FCommand[I], CommandVerb[1], Length(CommandVerb));
            SetLength(CommandTail, FCmdLen - J);
            Move(FCommand[J], CommandTail[1], Length(CommandTail));
            break;
        end;
        Inc(J);
    end;
    CommandVerb := UpperCase(CommandVerb);

    // We could chack which port we are servicing and call appropriate
    // interpeter (telnet, smtp, pop3, nntp or any line oriented protocol)
    TELNET_Interpreter(CommandVerb, CommandTail);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Not a real TELNET command interpreter, just enough to see how it could    }
{ be implemented.                                                           }
procedure TClientObject.TELNET_Interpreter(
    CommandVerb : String;
    CommandTail : String);
begin
    if Length(CommandVerb) > 0 then begin
        FCliWSocket.SendStr(#13#10 + 'Executing command ''' +
                            CommandVerb + '''...' + #13#10);

        if CommandVerb = 'EXIT' then begin
            PostMessage(FCtrlWindow, WM_CLIENT_DISCONNECTED, 0, LParam(Self));
//            FCliWSocket.Close;
            Exit;
        end
        else if CommandVerb = 'HELP' then
            FCliWSocket.SendStr('List of commands:' + #13#10 +
                                '    Exit      logoff from server' + #13#10 +
                                '    Help      show this help screen' + #13#10)
        else
            FCliWSocket.SendStr('Unknown command, ignoring');
    end;

    FCliWSocket.SendStr(CmdPrompt);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
