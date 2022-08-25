{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Copyright:    You can use this software freely, at your own risks
Creation:     April 14, 2005
Version:      1.00
Object:       Demo program to show how to use TWSocket object inside a console
              mode application to listen UDP messages from the network.
              Use UDPSend or any other program to send UDP messages.
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2005-2017 by François PIETTE
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

Updates:

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
program OverbyteIcsConUdpLstn;

{$I OVERBYTEICSDEFS.INC}
{$IFDEF DELPHI25_UP}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFDEF VER80}
    Bomb('Sorry, Delphi 1 does not support console mode programs');
{$ENDIF}
{$IFNDEF NOFORMS}
Bomb('Please add NOFORMS to your project defines to reduce exe size');
{$ENDIF}
{$APPTYPE CONSOLE}
{$DEFINE DEBUG}         // debug output.

uses
  Windows,
  SysUtils,
  Classes,
  OverbyteIcsConApp in '..\..\..\Source\Extras\OverbyteIcsConApp.pas',
  OverbyteIcsWSocket,
  OverbyteIcsWinSock;

// WinSock functions

const
  ConUdpLstnVersion     = 100;
  CopyRight : String = ' ConUdpLstn (c) 2005-2010 F. Piette V1.00 ';

type
  TConUdpRcv = class(TConApplication)
  private
    WSocket        : TWSocket;
    FPort          : String;
    FSender        : String;
    FSenderAddr    : TInAddr;
    FCount         : Integer;
  public
    constructor Create(Parent:TComponent); override;
    destructor  Destroy; override;
    procedure   Execute; override;
    procedure   Startlistening;
    procedure   WSocketDataAvailable(Sender: TObject;    ErrCode: Word);
    procedure   WSocketSessionConnected(Sender: TObject; ErrCode: Word);
    procedure   WSocketSessionClosed(Sender: TObject;    ErrCode: Word);
    procedure   DoCharReceived(Ch : Char); override;
  end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TConUDPRcv.Create;
begin
    WSocket:= TWSocket.Create(Self);
    FSender:= '0.0.0.0';
    FPort  := '600';
    FCount := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TConUDPRcv.Destroy;
begin
    WSocket.Close;
    WSocket.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConUdpRcv.StartListening;
begin
    FSenderAddr               := WSocketResolveHost(AnsiString(FSender));
    if FSenderAddr.S_addr = htonl(INADDR_LOOPBACK) then begin
        { Replace loopback address by real localhost IP addr }
        FSenderAddr           := WSocketResolveHost(LocalHostName);
    end;
    WSocket.Ondataavailable   := WSocketDataAvailable;
    WSocket.OnSessionClosed   := WSocketSessionClosed;
    WSocket.OnSessionConnected:= WSocketSessionConnected;
    WSocket.Proto             := 'udp';
    WSocket.Addr              := '0.0.0.0';
    WSocket.Port              := FPort;
    WSocket.Listen;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConUdpRcv.Execute;
begin
    // Very simple here: just call the message pump until terminated
    while not Terminated do
        MessageLoop;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConUdpRcv.WSocketDataAvailable(Sender: TObject; ErrCode: Word);
var
    Buffer : array [0..1023] of AnsiChar;
    Len    : Integer;
    Src    : TSockAddrIn;
    SrcLen : Integer;
begin
    SrcLen := SizeOf(Src);
    Len    := WSocket.ReceiveFrom(@Buffer, SizeOf(Buffer), Src, SrcLen);
    WriteLn('receiving something from:', FSenderAddr.s_addr);
    if Len >= 0 then begin
        if (FSenderAddr.S_addr = INADDR_ANY) or
           (FSenderAddr.S_addr = Src.Sin_addr.S_addr) then begin
            Buffer[Len] := #0;
            Inc(FCount);
            Writeln(IntToStr(fcount) ,
                    '  ' + StrPas(inet_ntoa(Src.sin_addr)) ,
                    ':'  + IntToStr(ntohs(Src.sin_port)) ,
                    '--> ' + String(StrPas(Buffer)));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConUdpRcv.WSocketSessionConnected(
    Sender  : TObject;
    ErrCode : Word);
begin
    Writeln('Listening');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConUdpRcv.WSocketSessionClosed(Sender: TObject; ErrCode: Word);
begin
    Writeln('Disconnected');
    FCount := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConUdpRcv.DoCharReceived(Ch: Char);
begin
    if Ch = #27 then           // Test for ESC key
        Terminate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
var ConUdp : TConUdpRcv;
begin
    TConApplication.CreateInstance(TConUdpRcv);
    ConUdp := TConUdpRcv(ConApplication);
    if ParamCount > 0 then
        ConUdp.FPort := ParamStr(1);

    ConUdp.StartListening;
    Writeln('* Server listening on port ' +
            ConUdp.FPort + '. Press ESC or CTRL-C to abort');
    try
        ConUdp.Run;
    finally
        ConUdp.Free;
    end;
end.

