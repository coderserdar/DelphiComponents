{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Creation:     December 2009
Description:  Test of Server Name Indication (SNI) in server mode.
Version:      8.01
EMail:        francois.piette@overbyte.be    http://www.overbyte.be
Support:      Unsupported code.
Legal issues: Copyright (C) 2009-2016 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
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
                 to François PIETTE. Use a nice stamp and mention your name,
                 street address, EMail address and any comment you like to say.

History:
Mar 23, 2015 V8.00 SslServerName is now a published property
Mar 23 2016  V8.01 Angus set ErrCode in onSslServerName event to stop Java clients
                    rejecting SSL connections, and illustrate it's proper use

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSslSniSrv1;

{$I OverbyteIcsSslDefs.inc}

{$IFNDEF USE_SSL}
  {$MESSAGE FATAL 'Define conditional define "USE_SSL" in the project options'};
{$ENDIF}
{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}
{$IFDEF OPENSSL_NO_TLSEXT}
    {$MESSAGE FATAL 'Undefine symbol "OPENSSL_NO_TLSEXT" in OverbyteIcsSslDefs.inc'};
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OverbyteIcsWSocket, OverbyteIcsWndControl,
  OverbyteIcsWSocketS, OverbyteIcsSslX509Utils;

type
  TMainForm = class(TForm)
    SslWSocketServer1: TSslWSocketServer;
    SslContext1: TSslContext;
    SslContext2: TSslContext;
    DisplayMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ClientConnect(Sender: TObject;
      Client: TWSocketClient; Error: Word);
    procedure ClientDisconnect(Sender: TObject;
      Client: TWSocketClient; Error: Word);
    procedure ClientSslServerName(Sender: TObject;
      var Ctx: TSslContext; var ErrCode: TTlsExtError);
  private
    FIsInit: Boolean;
    FComputerName: String;
    procedure ClientDataAvailable(Sender: TObject; ErrCode: Word);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

const
  RFC1123_StrWeekDay : String = 'MonTueWedThuFriSatSun';
  RFC1123_StrMonth   : String = 'JanFebMarAprMayJunJulAugSepOctNovDec';
  OkResponse : String =
  'HTTP/1.0 200 OK'#13#10 +
  'Last-Modified: %s '#13#10+
  'Content-Type: text/html'#13#10#13#10 +
  '<HTML><HEAD><TITLE>ICS SNI Server Demo</TITLE></HEAD>' +
  '<BODY><H2>ICS Server Name Indication (SNI) demo</H2></BODY></HTML>';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Copied from OverbyteHttpProt.pas }
function RFC1123_Date(aDate : TDateTime) : String;
var
   Year, Month, Day       : Word;
   Hour, Min,   Sec, MSec : Word;
   DayOfWeek              : Word;
begin
   DecodeDate(aDate, Year, Month, Day);
   DecodeTime(aDate, Hour, Min,   Sec, MSec);
   DayOfWeek := ((Trunc(aDate) - 2) mod 7);
   Result := Copy(RFC1123_StrWeekDay, 1 + DayOfWeek * 3, 3) + ', ' +
             Format('%2.2d %s %4.4d %2.2d:%2.2d:%2.2d',
                    [Day, Copy(RFC1123_StrMonth, 1 + 3 * (Month - 1), 3),
                     Year, Hour, Min, Sec]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.FormCreate(Sender: TObject);
var
    ProgDir: String;
begin
    FComputerName := AnsiLowerCase(String(LocalHostName));
    ProgDir := ExtractFilePath(ParamStr(0));
    if not FileExists(ProgDir + 'SelfSignedServer1.pem') then
        CreateSelfSignedCert(ProgDir + 'SelfSignedServer1.pem',
                             'DE', 'Berlin', 'Berlin', 'ICS Demos', 'ICS Demos',
                             'localhost', 'certs@domain.de', 1024, TRUE,
                             365 * 10);
    if not FileExists(ProgDir + 'SelfSignedServer2.pem') then
        CreateSelfSignedCert(ProgDir + 'SelfSignedServer2.pem',
                             'DE', 'Berlin', 'Berlin', 'ICS Demos', 'ICS Demos',
                             FComputerName, 'certs@domain.de', 1024, TRUE,
                             365 * 10);

    SslContext1.SslCertFile := ProgDir + 'SelfSignedServer1.pem';
    SslContext1.SslPrivKeyFile := SslContext1.SslCertFile;
    SslContext1.SslPassPhrase := '';
    SslContext2.SslCertFile := ProgDir + 'SelfSignedServer2.pem';
    SslContext2.SslPrivKeyFile := SslContext2.SslCertFile;
    SslContext2.SslPassPhrase := '';

    DisplayMemo.Text :=
    'This example shows SSL Server Name Indication (SNI)'#13#10 +
    'Internet Explorer >= v7 or Firefox >= v2 will do.';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.FormShow(Sender: TObject);
begin
    if not FIsInit then begin
        FIsInit := TRUE;
        SslWSocketServer1.Banner := '';
        SslWSocketServer1.Listen;
        DisplayMemo.Lines.Add('Listening on port ' + SslWSocketServer1.Port);
        DisplayMemo.Lines.Add('Navigate to URL: "https://' + FComputerName +
                              '/" - "localhost" or IP would NOT work!');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ClientConnect(Sender: TObject;
  Client: TWSocketClient; Error: Word);
begin
    Client.LineEnd         := #13#10;
    Client.LineMode        := TRUE;
    Client.OnDataAvailable := ClientDataAvailable;
 // Client.OnSslServerName := ClientSslServerName;
    DisplayMemo.Lines.Add(#13#10'! Client connected');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ClientDisconnect(Sender: TObject;
  Client: TWSocketClient; Error: Word);
begin
    DisplayMemo.Lines.Add('! Client disconnected');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ClientSslServerName(
  Sender      : TObject;
  var Ctx     : TSslContext;
  var ErrCode : TTlsExtError); // Optional error code
var
    Cli : TSslWSocketClient;
begin
   { V8.06 tell SSL whether server can handle SslServerName }
    ErrCode := teeOk;              { accept SSL connection }
  //  ErrCode := teeAlertWarning;  { old default, stopped Java clients connecting }
  //  ErrCode := teeAlertFatal;    { reject SSL connection }

    Cli := TSslWSocketClient(Sender);
    { Provide a SslContext that corresponds to the server name received }
    if FComputerName = Cli.SslServerName then begin
        if not SslContext2.IsCtxInitialized then
            SslContext2.InitContext;
        Ctx := SslContext2;
        DisplayMemo.Lines.Add('! Server name "' + Cli.SslServerName +'" received');
        DisplayMemo.Lines.Add('! Switching context to SslContext2');
    end
    else begin
        DisplayMemo.Lines.Add('! Unknown server name "' + Cli.SslServerName +
                              '" received. Context switch denied');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ClientDataAvailable(Sender: TObject;
  ErrCode: Word);
var
    FRcvBuf : AnsiString;
    Cli : TSslWSocketClient;
begin
    if ErrCode <> 0 then Exit;
    Cli := TSslWSocketClient(Sender);
    FRcvBuf := Cli.ReceiveStrA;
    if FRcvBuf = #13#10 then begin
        { Fake Last-Modified header to prevent client caching }
        Cli.SendStr(Format(OkResponse, [RFC1123_Date(Now)]));
        Cli.CloseDelayed;
    end
    else begin
        while (Length(FRcvBuf) > 0) and (FRcvBuf[Length(FRcvBuf)] in [#10, #13]) do
            SetLength(FRcvBuf, Length(FRcvBuf) -1);
        DisplayMemo.Lines.Add('> ' + String(FRcvBuf));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
