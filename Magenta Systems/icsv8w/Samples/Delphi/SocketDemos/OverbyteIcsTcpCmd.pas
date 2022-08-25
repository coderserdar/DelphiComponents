{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This code is part of SvcTcp and SrvTcp sample programs. In those
              samples, all TWSocket code has been encapsulated in TTcpDaemon
              object. This is done so that you can see how the same code can
              be used inside a service or inside a normal exe program.
Creation:     July 15, 2000
Version:      1.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2000-2010 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsTcpCmd;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$IFNDEF VER80} { Not for Delphi 1                    }
    {$H+}       { Use long strings                    }
    {$J+}       { Allow typed constant to be modified }
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, OverbyteIcsWSocket, 
  OverbyteIcsWSocketS;

const
  TcpCmdVersion             = 100;
  CopyRight    : String     = ' TcpCmd (c) 2000 F. Piette V1.00 ';

type
  { This class is used as a client class for TWSocketServer. Each time a    }
  { client connect to the server, TWSocketServer will instanciate a new     }
  { TTcpSrvClient to handle the client.                                     }
  TTcpSrvClient = class(TWSocketClient)
  public
    RcvdLine    : String;
    Param       : array [0..10] of String;
    ParamCount  : Integer;
    ConnectTime : TDateTime;
  end;

  TDisplayProc = procedure (Msg : String) of object;

  { This class encapsulate all the work for a basic TCP server. It include }
  { a basic command interpreter.                                           }
  TTcpDaemon = class(TObject)
  private
    WSocketServer1 : TWSocketServer;
    FPort          : String;
    FAddr          : String;
    FOnDisplay     : TDisplayProc;
    procedure Display(Msg : String);
    procedure WSocketServer1BgException(Sender: TObject; E: Exception;
                                        var CanClose: Boolean);
    procedure WSocketServer1ClientConnect(Sender: TObject;
                                          Client: TWSocketClient;
                                          Error: Word);
    procedure WSocketServer1ClientDisconnect(Sender: TObject;
                                             Client: TWSocketClient;
                                             Error: Word);
    procedure ClientDataAvailable(Sender: TObject; Error: Word);
    procedure ProcessData(Client: TTcpSrvClient);
    procedure ClientBgException(Sender: TObject; E: Exception;
                                var CanClose: Boolean);
    function  GetBanner: String;
    procedure SetBanner(const Value: String);
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   Start;
    procedure   Stop;
    property OnDisplay : TDisplayProc read FOnDisplay write FOnDisplay;
    { Make Banner property available to the outside. We could make other    }
    { TWSocket properties available.                                        }
    property Banner : String          read GetBanner  write SetBanner;
    property Port   : String          read FPort      write FPort;
    property Addr   : String          read FAddr      write FAddr;
  end;

implementation

uses
    OverbyteIcsUtils;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TTcpDaemon.Create;
begin
    inherited Create;
    WSocketServer1        := TWSocketServer.Create(nil);
    WSocketServer1.Banner := 'ICS Tcp Service Ready';
    FPort                 := '2120';
    FAddr                 := '0.0.0.0';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor  TTcpDaemon.Destroy;
begin
    if Assigned(WSocketServer1) then begin
        WSocketServer1.Destroy;
        WSocketServer1 := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpDaemon.WSocketServer1BgException(Sender: TObject;
  E: Exception; var CanClose: Boolean);
begin
    Display('Server exception occured: ' + E.ClassName + ': ' + E.Message);
    CanClose := FALSE;  { Hoping that server will still work ! }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event is called each time a new client connect. We can setup our     }
{ client class to fit our needs (We use line mode and two events)           }
procedure TTcpDaemon.WSocketServer1ClientConnect(
    Sender : TObject;
    Client : TWSocketClient;
    Error  : Word);
begin
    with Client as TTcpSrvClient do begin
        Display('Client connecting: ' + PeerAddr);
        LineMode        := TRUE;
        LineEdit        := TRUE;
        OnDataAvailable := ClientDataAvailable;
        OnBgException   := ClientBgException;
        ConnectTime     := Now;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event is called each time a client disconnect. No many things to do  }
{ here. Just display a message.                                             }
procedure TTcpDaemon.WSocketServer1ClientDisconnect(
    Sender : TObject;
    Client : TWSocketClient;
    Error  : Word);
begin
    with Client as TTcpSrvClient do begin
        Display('Client disconnecting: ' + PeerAddr + '   ' +
                'Duration: ' + FormatDateTime('hh:nn:ss',
                Now - ConnectTime));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpDaemon.ClientDataAvailable(
    Sender : TObject;
    Error  : Word);
begin
    with Sender as TTcpSrvClient do begin
        { We use line mode. We will receive complete lines }
        RcvdLine := ReceiveStr;
        { Remove trailing CR/LF }
        while (Length(RcvdLine) > 0) and
              IsCharInSysCharSet(RcvdLine[Length(RcvdLine)], [#13, #10]) do
            RcvdLine := Copy(RcvdLine, 1, Length(RcvdLine) - 1);
        Display('Received from ' + GetPeerAddr + ': ''' + RcvdLine + '''');
        ProcessData(Sender as TTcpSrvClient);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Split a command line into an array of words. Use spaces or tabs as        }
{ command delimiter. Commands words have to be delimited by doubles quotes  }
{ if they include spaces or tabs.                                           }
function ParseCommandLine(
    const CmdLine     : String;
    var   ParamsArray : array of string) : Integer;
var
    Index   : Integer;
    I, J    : Integer;
begin
    I      := 1;
    Index  := Low(ParamsArray);
    while (Index <= High(ParamsArray)) and
          (I <= Length(CmdLine)) do begin
        { Skip spaces and tabs }
        while (I <= Length(CmdLine)) and IsCharInSysCharSet(CmdLine[I], [' ', #9]) do
            Inc(I);
        if I > Length(CmdLine) then
            break;
        { Check if quoted parameters (can have embeded spaces) }
        if CmdLine[I] = '"' then begin
            Inc(I);
            ParamsArray[Index] := '';
            while I <= Length(CmdLine) do begin
                if CmdLine[I] = '"' then begin
                    if (I >= Length(CmdLine)) or (CmdLine[I + 1] <> '"') then begin
                        Inc(I);
                        break;
                    end;
                    ParamsArray[Index] := ParamsArray[Index] + '"';
                    Inc(I, 2);
                end
                else begin
                    ParamsArray[Index] := ParamsArray[Index] + CmdLine[I];
                    Inc(I);
                end;
            end;
        end
        else begin
            J := I;
            while (I <= Length(CmdLine)) and (not IsCharInSysCharSet(CmdLine[I], [' ', #9])) do
                Inc(I);
            if J = I then
                break;
            ParamsArray[Index] := Copy(CmdLine, J, I - J);
        end;
        Inc(Index);
    end;
    Result := Index - Low(ParamsArray);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Process a command line received from any client. If process takes time,   }
{ you should use a thread to do the work and return immediately.            }
procedure TTcpDaemon.ProcessData(Client : TTcpSrvClient);
var
    I       : Integer;
    AClient : TTcpSrvClient;
begin
    { Parse command line. }
    Client.ParamCount := ParseCommandLine(Client.RcvdLine, Client.Param);
    if Client.ParamCount <= 0 then
        Exit;

    { We could replace all those CompareText with a table lookup }
    if CompareText(Client.Param[0], 'exit') = 0 then
        { We can't call Client.Close here because we will immediately }
        { reenter DataAvailable event handler with same line because  }
        { a line is removed from buffer AFTER it has been processed.  }
        { Using CloseDelayed will delay Close until we are out of     }
        { current event handler.                                      }
        Client.CloseDelayed
    else if CompareText(Client.Param[0], 'time') = 0 then
        { Send server date and time to client }
        Client.SendStr(DateTimeToStr(Now) + #13#10)
    else if CompareText(Client.Param[0], 'who') = 0 then begin
        { Send client list to client }
        Client.SendStr('There are ' + IntToStr(WSocketServer1.ClientCount) +
                       ' connected users:' + #13#10);
        for I := WSocketServer1.ClientCount - 1 downto 0 do begin
            AClient := TTcpSrvClient(WSocketServer1.Client[I]);
            Client.SendStr(AClient.PeerAddr + ':' + AClient.GetPeerPort + ' ' +
                           DateTimeToStr(AClient.ConnectTime) + #13#10);
        end;
    end
    else if CompareText(Client.Param[0], 'help') = 0 then begin
        Client.SendStr('Commands are:' + #13#10 +
                       '   exit        Close current session' + #13#10 +
                       '   time        Display server time and date' + #13#10 +
                       '   who         Display connected clients' + #13#10 +
                       '   help        Show this help text' + #13#10);
    end
    else begin
        if Client.State = wsConnected then
            Client.SendStr('Unknown command: ''' +
                           Client.Param[0] + '''' + #13#10 +
                           'Type help if you need help...' +#13#10);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when a client socket experience a background }
{ exception. It is likely to occurs when client aborted connection and data }
{ has not been sent yet.                                                    }
procedure TTcpDaemon.ClientBgException(
    Sender       : TObject;
    E            : Exception;
    var CanClose : Boolean);
begin
    Display('Client exception occured: ' + E.ClassName + ': ' + E.Message);
    CanClose := TRUE;   { Goodbye client ! }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpDaemon.Display(Msg: String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpDaemon.Start;
begin
    WSocketServer1.OnBgException      := WSocketServer1BgException;
    WSocketServer1.OnClientConnect    := WSocketServer1ClientConnect;
    WSocketServer1.OnClientDisconnect := WSocketServer1ClientDisconnect;
    WSocketServer1.Proto              := 'tcp';
    WSocketServer1.Port               := FPort;
    WSocketServer1.Addr               := FAddr;
    WSocketServer1.ClientClass        := TTcpSrvClient;
    WSocketServer1.Listen;
    Display('Waiting for clients...');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpDaemon.Stop;
begin
    WSocketServer1.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTcpDaemon.GetBanner: String;
begin
    Result := WSocketServer1.Banner;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpDaemon.SetBanner(const Value: String);
begin
    WSocketServer1.Banner := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
