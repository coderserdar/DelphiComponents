{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Jan 24, 2003
Description:  A basic SSL server using TSslWSocket.
              Make use of OpenSSL (http://www.openssl.org)
Version:      8.62
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list ics-ssl@elists.org
              Follow "SSL" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2003-2019 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>
              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany, contact: <arno.garrels@gmx.de>

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
Jan 29, 2009 V1.00.2 Arno removed a D2009 warning.
Dec 10, 2014 V8.0 Angus added handshake response message
Jun 18, 2019 V8.62 Removed AcceptableHosts, never used with servers.





 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSimpleSslServer1;

{$IFNDEF USE_SSL}
  {$MESSAGE FATAL 'Define conditional define "USE_SSL" in the project options'};
{$ENDIF}
{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }
{ If you use Delphi 7, you may wants to disable warnings for unsage type,   }
{ unsafe code and unsafe typecast in the project options. Those warning are }
{ intended for .NET programs. You may also want to turn off deprecated      }
{ symbol and platform symbol warnings.                                      }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, OverbyteIcsWSocket, OverbyteIcsWSocketS,
  OverbyteIcsSSLEAY, OverbyteIcsLIBEAY, OverbyteIcsWndControl, OverbyteIcsUtils;

const
  SimpleSslServer1Version            = 800;
  SimpleSslServer1Date               = 'Dec 10, 2014';
  SimpleSslServer1CopyRight : String = ' SimpleSslServer1 (c) 2003-2014 Francois Piette V8.00 ';

  WM_SSL_NOT_TRUSTED = WM_USER + 1;

type
  { TTcpSrvClient is the class which will be instanciated by server component }
  { for each new client. N simultaneous clients means N TTcpSrvClient will be }
  { instanciated. Each being used to handle only a single client.             }
  { We can add any data that has to be private for each client, such as       }
  { receive buffer or any other data needed for processing.                   }
  TTcpSrvClient = class(TSslWSocketClient)
  public
    RcvdLine    : String;
    ConnectTime : TDateTime;
  end;

  TSimpleSslServerForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    SslWSocketServer1: TSslWSocketServer;
    StartButton: TButton;
    Label1: TLabel;
    PortEdit: TEdit;
    Label3: TLabel;
    CertFileEdit: TEdit;
    Label7: TLabel;
    CAFileEdit: TEdit;
    Label6: TLabel;
    PrivKeyFileEdit: TEdit;
    Label4: TLabel;
    PassPhraseEdit: TEdit;
    Label2: TLabel;
    CAPathEdit: TEdit;
    VerifyPeerCheckBox: TCheckBox;
    StopButton: TButton;
    Label9: TLabel;
    Label8: TLabel;
    AcceptableHostsEdit: TEdit;
    SslContext1: TSslContext;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure SslWSocketServer1ClientConnect(Sender: TObject;
      Client: TWSocketClient; Error: Word);
    procedure StopButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ClientVerifyPeer(Sender        : TObject;
                               var Ok        : Integer;
                               Cert          : TX509Base);
    procedure SslWSocketServer1SslHandshakeDone(Sender: TObject; ErrCode: Word; PeerCert: TX509Base;
      var Disconnect: Boolean);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    FTrustedList : TStringList;
    FNotTrusted  : String;
    procedure ClientDataAvailable(Sender: TObject; Error: Word);
    procedure ProcessData(Client : TTcpSrvClient);
    procedure ClientBgException(Sender       : TObject;
                                E            : Exception;
                                var CanClose : Boolean);
    procedure ClientLineLimitExceeded(Sender        : TObject;
                                      Cnt           : LongInt;
                                      var ClearData : Boolean);
    procedure WMSslNotTrusted(var Msg: TMessage); message WM_SSL_NOT_TRUSTED;
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  SimpleSslServerForm: TSimpleSslServerForm;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';
    KeyPort            = 'Port';
    KeyCertFile        = 'CertFile';
    KeyPassPhrase      = 'PassPhrase';
    KeyPrivKeyFile     = 'PrivKeyFile';
    KeyCAFile          = 'CAFile';
    KeyCAPath          = 'CAPath';
    KeyVerifyPeer      = 'VerifyPeer';
//    KeyAcceptableHosts = 'AcceptableHosts';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure BigConsole(nCols, nLines : Integer);
var
    sc : TCoord;
    N  : DWord;
begin
    if not IsConsole then
        Exit;
    sc.x := nCols;
    sc.y := nLines;
    SetConsoleScreenBufferSize(GetStdHandle(STD_OUTPUT_HANDLE), sc);
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
                            BACKGROUND_BLUE or BACKGROUND_GREEN or
                            BACKGROUND_RED or BACKGROUND_INTENSITY);
    sc.x := 0;
    sc.y := 0;
    FillConsoleOutputAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
                               BACKGROUND_BLUE or BACKGROUND_GREEN or
                               BACKGROUND_RED or BACKGROUND_INTENSITY,
                               nCols * nLines, sc, N);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSimpleSslServerForm.FormCreate(Sender: TObject);
begin
    BigConsole(80, 100);
    FIniFileName := GetIcsIniFileName;
    FTrustedList := TStringList.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSimpleSslServerForm.FormDestroy(Sender: TObject);
begin
    if Assigned(FTrustedList) then
        FreeAndNil(FTrustedList);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSimpleSslServerForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIcsIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        PortEdit.Text        := IniFile.ReadString(SectionData, KeyPort,
                                                   '443');
        CertFileEdit.Text    := IniFile.ReadString(SectionData, KeyCertFile,
                                                   '01cert.pem');
        PrivKeyFileEdit.Text := IniFile.ReadString(SectionData, KeyPrivKeyFile,
                                                   '01key.pem');
        PassPhraseEdit.Text  := IniFile.ReadString(SectionData, KeyPassPhrase,
                                                   'password');
        CAFileEdit.Text      := IniFile.ReadString(SectionData, KeyCAFile,
                                                   'cacert.pem');
        CAPathEdit.Text      := IniFile.ReadString(SectionData, KeyCAPath,
                                                   '');
        VerifyPeerCheckBox.Checked := Boolean(IniFile.ReadInteger(SectionData,
                                                                  KeyVerifyPeer,
                                                                  0));
        IniFile.Free;
        DisplayMemo.Clear;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSimpleSslServerForm.FormClose(
    Sender     : TObject;
    var Action : TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.WriteString(SectionData,    KeyPort,        PortEdit.Text);
    IniFile.WriteString(SectionData,    KeyCertFile,    CertFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyPrivKeyFile, PrivKeyFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyPassPhrase,  PassPhraseEdit.Text);
    IniFile.WriteString(SectionData,    KeyCAFile,      CAFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyCAPath,      CAPathEdit.Text);
    IniFile.WriteInteger(SectionData,   KeyVerifyPeer,  Ord(VerifyPeerCheckBox.Checked));
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSimpleSslServerForm.Display(Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSimpleSslServerForm.StartButtonClick(Sender: TObject);
begin
    SslWSocketServer1.Proto          := 'tcp';
    SslWSocketServer1.Addr           := '0.0.0.0'; // Use any interface
    SslWSocketServer1.Port           := PortEdit.Text;
    SslWSocketServer1.SslEnable      := TRUE;
    SslContext1.SslCertFile          := CertFileEdit.Text;
    SslContext1.SslPassPhrase        := PassPhraseEdit.Text;
    SslContext1.SslPrivKeyFile       := PrivKeyFileEdit.Text;
    SslContext1.SslCAFile            := CAFileEdit.Text;
    SslContext1.SslCAPath            := CAPathEdit.Text;
    SslContext1.SslVerifyPeer        := VerifyPeerCheckBox.Checked;
    SslWSocketServer1.SetAcceptableHostsList(AcceptableHostsEdit.Text);
    SslWSocketServer1.Listen;
    SslWSocketServer1.ClientClass    := TTcpSrvClient; // Use our component
    Display('Listenning...');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSimpleSslServerForm.SslWSocketServer1ClientConnect(
    Sender : TObject;
    Client : TWSocketClient;
    Error  : Word);
begin
    with Client as TTcpSrvClient do begin
        Display('Client connected.' +
                ' Remote: ' + PeerAddr + '/' + PeerPort +
                ' Local: '  + GetXAddr + '/' + GetXPort);
        Display('There is now ' +
                IntToStr(TWSocketServer(Sender).ClientCount) +
                ' clients connected.');
        LineMode            := TRUE;
        LineEdit            := TRUE;
        LineLimit           := 80; { Do not accept long lines }
        OnDataAvailable     := ClientDataAvailable;
        OnLineLimitExceeded := ClientLineLimitExceeded;
        OnBgException       := ClientBgException;
        OnSslVerifyPeer     := ClientVerifyPeer;
        ConnectTime         := Now;
    end;
end;


procedure TSimpleSslServerForm.SslWSocketServer1SslHandshakeDone(Sender: TObject; ErrCode: Word; PeerCert: TX509Base;
  var Disconnect: Boolean);
begin
     Display((Sender as TTcpSrvClient).SslHandshakeRespMsg);  { V8.00 }
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSimpleSslServerForm.ClientBgException(
    Sender       : TObject;
    E            : Exception;
    var CanClose : Boolean);
begin
    Display('Client exception occured: ' + E.ClassName + ': ' + E.Message);
    CanClose := TRUE;   // Goodbye client !
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSimpleSslServerForm.ClientDataAvailable(
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
procedure TSimpleSslServerForm.ClientLineLimitExceeded(
    Sender        : TObject;
    Cnt           : Integer;
    var ClearData : Boolean);
begin
    with Sender as TTcpSrvClient do begin
        Display('Line limit exceeded from ' + GetPeerAddr + '. Closing.');
        ClearData := TRUE;
        Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSimpleSslServerForm.ProcessData(Client: TTcpSrvClient);
begin
    if Client.State = wsConnected then
        Client.SendStr('Unknown command: ''' + Client.RcvdLine + '''' + #13#10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSimpleSslServerForm.StopButtonClick(Sender: TObject);
begin
    SslWSocketServer1.Close;
    Display('Server stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSimpleSslServerForm.WMSslNotTrusted(var Msg: TMessage);
begin
    if Application.MessageBox(
           PChar('Do you want to trust this certificate ?' + #10 +
                 FNotTrusted),
           PChar(Caption), MB_YESNO + MB_DEFBUTTON2) = ID_YES then begin
        FTrustedList.Add(FNotTrusted);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSimpleSslServerForm.ClientVerifyPeer(Sender: TObject;
  var Ok: Integer; Cert: TX509Base);
var
    Issuer : String;
begin
    Issuer := Cert.IssuerOneLine;
    if Ok <> 0 then
        Display('Received certificate. Issuer = "' + Issuer + '"')
    else begin
        if Cert.VerifyResult = X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN then begin
            if FTrustedList.IndexOf(Issuer) >= 0 then begin
                Display('Received certificate. Issuer = "' + Issuer + '"');
                Display('We trust this one');
                Ok := 1;
                Exit;
            end;
            FNotTrusted := Issuer;
            (Sender as TSslWSocket).CloseDelayed;
            PostMessage(Handle, WM_SSL_NOT_TRUSTED, 0, 0);
            Exit;
        end;
        Display('Can''t verify certificate:');
        Display('  Issuer = "' + Issuer + '"');
        Display('  Error  = ' + IntToStr(Cert.VerifyResult) + ' (' + Cert.VerifyErrMsg + ')');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
