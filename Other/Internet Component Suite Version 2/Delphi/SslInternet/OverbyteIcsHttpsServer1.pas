{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Jan 12, 2003
Description:  A basic HTTPS server using TSslWSocket.
              Make use of OpenSSL (http://www.openssl.org)
Version:      1.00.7
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list ics-ssl@elists.org
              Follow "SSL" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2003-2006 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
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


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ DEFINE DEBUG_OUTPUT}
unit OverbyteIcsHttpsServer1;

{ You must define USE_SSL in the project options so that SSL code is        }
{ included in the socket component.                                         }
{ To be able to compile the component, you must have the SSL related files  }
{ which are _NOT_ freeware. See http://www.overbyte.be for details.         }
{$IFNDEF USE_SSL}
    Bomb('Add USE_SSL in the define section in project options');
{$ENDIF}

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, OverbyteIcsIniFiles, OverbyteIcsWinsock, OverbyteIcsWSocket,
  OverbyteIcsSSLEAY, OverbyteIcsLIBEAY, OverbyteIcsWndControl;

const
  HttpsSrvVersion         = 100;
  HttpsSrvDate            = 'Apr 06, 2003';
  CopyRight   : String    = ' IcsHttpsServer (c) 2003 Francois Piette V1.00.6 ';
  WM_APPSTARTUP           = WM_USER + 1;
  WM_SSL_NOT_TRUSTED      = WM_USER + 2;

type
  THttpConnection = class(TSslWSocket)
  public
    IpAddr         : String;
    RcvBuf         : PChar;
    RcvCount       : Integer;
    RcvSize        : Integer;
    FRcvdLineCount : Integer;
    FRcvdLine      : String;
    FMethod        : String;
    FPath          : String;
    FVersion       : String;
    FParams        : String;
    FDocDir        : String;
    FDocument      : String;
    FPeerIP        : String;
    FDocStream     : TFileStream;
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   ProcessLine;
    procedure   ParseRequest;
    function    BuildSslPage(var Response: String): Integer;
    function    Build404Page(var Response: String): Integer;
    procedure   SendDoc;
    procedure   DataSent(Sender : TObject; Error : WORD);
    procedure   DataSentShutdown(Sender: TObject; Error: WORD);
  end;

  THttpsSrvForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    Label3: TLabel;
    Label6: TLabel;
    Label5: TLabel;
    PortEdit: TEdit;
    PrivKeyFileEdit: TEdit;
    CertFileEdit: TEdit;
    Label7: TLabel;
    Label4: TLabel;
    Label1: TLabel;
    DocDirEdit: TEdit;
    PassPhraseEdit: TEdit;
    CAFileEdit: TEdit;
    ClearButton: TButton;
    CloseButton: TButton;
    ListenButton: TButton;
    SslWSocket1: TSslWSocket;
    CAPathEdit: TEdit;
    Label2: TLabel;
    VerifyPeerCheckBox: TCheckBox;
    Label8: TLabel;
    Label9: TLabel;
    AcceptableHostsEdit: TEdit;
    ClientCountLabel: TLabel;
    SslContext1: TSslContext;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SslWSocket1SessionAvailable(Sender: TObject; Error: Word);
    procedure FormDestroy(Sender: TObject);
    procedure ListenButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject); 
  protected
    FIniFileName : String;
    FInitialized : Boolean;
    FClients     : TList;
    FTrustedList : TStringList;
    FNotTrusted  : String;
    procedure ClientDataAvailable(Sender: TObject; Error: Word);
    procedure ClientSessionClosed(Sender: TObject; Error: Word);
    procedure ClientSessionConnected(Sender: TObject; Error: Word);
    procedure Notification(AComponent : TComponent;
                           Operation  : TOperation); override;
    procedure WMSslNotTrusted(var Msg: TMessage); message WM_SSL_NOT_TRUSTED;
    procedure ClientVerifyPeer(Sender        : TObject;
                               var Ok        : Integer;
                               Cert          : TX509Base);
    procedure WMAppStartup(var Msg: TMessage); message WM_APPSTARTUP;
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  HttpsSrvForm: THttpsSrvForm;

implementation

{$R *.DFM}

// Version informations. Synchronize this with VersionInfo in project options !
const
    ProgName           = 'IcsHttpsServer';
    ProgVersion        = '1.0.1';
    ProgDate           = 'Jan 12, 2003';
    ProgCopyright      = '(c) 2003 by François PIETTE';

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';
    KeyPort            = 'Port';
    KeyDocDir          = 'DocDir';
    KeyCertFile        = 'CertFile';
    KeyPassPhrase      = 'PassPhrase';
    KeyPrivKeyFile     = 'PrivKeyFile';
    KeyVerifyPeer      = 'VerifyPeer';
    KeyCAFile          = 'CAFile';
    KeyCAPath          = 'CAPath';
    KeyAcceptableHosts = 'AcceptableHosts';

const
    ReqCount    : Integer  = 0;
    LogFileName : String   = 'IcsHttpsServer.log';
    LogFileOpen : Boolean  = FALSE;
var
    LogFile     : TextFile;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure OutputDebugString(const Msg : String);
begin
{$IFDEF DEBUG_OUTPUT}
    Windows.OutputDebugString(PChar(Msg));
//    if IsConsole then
//        WriteLn(Msg);
{$ENDIF}
end;


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
procedure LogText(Prefix : Char; const Msg : String);
begin
    // ToDo: Check exceptions
    if not LogFileOpen then begin
        AssignFile(LogFile, LogFileName);
        if not FileExists(LogFileName) then begin
            Rewrite(LogFile);
            CloseFile(LogFile);
        end;
        Append(LogFile);
        LogFileOpen := TRUE;
    end;
    WriteLn(LogFile, Prefix, ' [', FormatDateTime('YYYYMMDD HHNNSS', Now),
            '] ', Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure LogClose;
begin
    if LogFileOpen then begin
        CloseFile(LogFile);
        LogFileOpen := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function xdigit(Ch : char) : Integer;
begin
    if ch in ['0'..'9'] then
        Result := ord(Ch) - ord('0')
    else
        Result := (ord(Ch) and 15) + 9;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function isxdigit(Ch : char) : Boolean;
begin
    Result := (Ch in ['0'..'9']) or (Ch in ['a'..'f']) or (ch in ['A'..'F']);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function htoin(Value : PChar; Len : Integer) : Integer;
var
    I : Integer;
begin
    Result := 0;
    I      := 0;
    while (I < len) and (Value[I] = ' ') do
        I := I + 1;
    while (I < Len) and (isxDigit(Value[I])) do begin
        Result := Result * 16 + xdigit(Value[I]);
        I := I + 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function htoi2(Value : PChar) : Integer;
begin
    Result := htoin(Value, 2);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function URLDecode(S : String) : String;
var
    I  : Integer;
    Ch : Char;
begin
    Result := '';
    I := 1;
    while (I <= Length(S)) and (S[I] <> '&') do begin
        Ch := S[I];
        if Ch = '%' then begin
            Ch := chr(htoi2(@S[I + 1]));
            Inc(I, 2);
        end
        else if Ch = '+' then
            Ch := ' ';
        Result := Result + Ch;
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsSrvForm.FormCreate(Sender: TObject);
begin
    BigConsole(80, 100);
    FIniFileName := GetIcsIniFileName;
    LogFileName  := ChangeFileExt(FIniFileName, '.log');
    LogText('!', 'Start ' + ProgName +
            ' V' + ProgVersion + ' ' + ProgDate + ', ' + ProgCopyright);
    LogClose;
    FClients     := TList.Create;
    FTrustedList := TStringList.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsSrvForm.FormDestroy(Sender: TObject);
begin
    if Assigned(FCLients) then begin
        // Remove all client sockets
        while FClients.Count > 0 do
            THttpConnection(FClients.Items[0]).Destroy;
        ClientCountLabel.Caption := IntToStr(FClients.Count);
        // Remove clients list
        FreeAndNil(FClients);
    end;
    if Assigned(FTrustedList) then
        FreeAndNil(FTrustedList);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsSrvForm.Notification(
    AComponent : TComponent;
    Operation  : TOperation);
var
    Item : Integer;
begin
    inherited Notification(AComponent, Operation);
    if (Operation = opRemove) and Assigned(FClients) then begin
        Item := FClients.Remove(AComponent);
        if Item >= 0 then begin
            // We found the component in our list and removed it
            // Display('Removed ' + IntToStr(Item));
        end;
        ClientCountLabel.Caption := IntToStr(FClients.Count);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsSrvForm.FormShow(Sender: TObject);
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
        DocDirEdit.Text      := IniFile.ReadString(SectionData, KeyDocDir,
                                                   'c:\inetpub\wwwroot');
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
        AcceptableHostsEdit.Text := IniFile.ReadString(SectionData, KeyAcceptableHosts,
                                                       'www.overbyte.be;www.borland.com');
        VerifyPeerCheckBox.Checked := Boolean(IniFile.ReadInteger(SectionData,
                                                                  KeyVerifyPeer,
                                                                  0));
        IniFile.Free;
        DisplayMemo.Clear;
        Display(Trim(CopyRight));
        Display('Using ' + Trim(OverbyteIcsWSocket.CopyRight));
        Display('      ' + Trim(OverbyteIcsWSocket.SslWSocketCopyRight));
        PostMessage(Handle, WM_APPSTARTUP, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsSrvForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.WriteString(SectionData,    KeyPort,        PortEdit.Text);
    IniFile.WriteString(SectionData,    KeyDocDir,      DocDirEdit.Text);
    IniFile.WriteString(SectionData,    KeyCertFile,    CertFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyPrivKeyFile, PrivKeyFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyPassPhrase,  PassPhraseEdit.Text);
    IniFile.WriteString(SectionData,    KeyCAFile,      CAFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyCAPath,      CAPathEdit.Text);
    IniFile.WriteString(SectionData,    KeyAcceptableHosts, AcceptableHostsEdit.Text);
    IniFile.WriteInteger(SectionData,   KeyVerifyPeer,  Ord(VerifyPeerCheckBox.Checked));
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsSrvForm.Display(Msg : String);
var
    I, J : Integer;
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
        end;
        // Display Msg, breaking it into separate lines
        // Line end is either LF alone or CR/LF pair
        I := 1;
        while I <= Length(Msg) do begin
            J := I;
            while (I <= Length(Msg)) and (Msg[I] <> #10) do
                Inc(I);
            if (I > 1) and (I <= Length(Msg)) and (Msg[I] = #10) and (Msg[I-1] = #13) then
                DisplayMemo.Lines.Add(Copy(Msg, J, I -J - 1))
            else
                DisplayMemo.Lines.Add(Copy(Msg, J, I - J));
            Inc(I);
        end;
    finally
        DisplayMemo.Lines.EndUpdate;
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsSrvForm.WMAppStartup(var Msg: TMessage);
begin
    ListenButtonClick(nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsSrvForm.ListenButtonClick(Sender: TObject);
begin
    if SslWSocket1.State = wsListening then
        Exit;
    SslWSocket1.Proto          := 'tcp';
    SslWSocket1.Port           := PortEdit.Text;
    SslWSocket1.Addr           := '0.0.0.0';
    SslWSocket1.SslEnable      := FAlSE;
    SslWSocket1.SslMode        := sslModeServer;
    SslContext1.SslCertFile    := CertFileEdit.Text;
    SslContext1.SslPassPhrase  := PassPhraseEdit.Text;
    SslContext1.SslPrivKeyFile := PrivKeyFileEdit.Text;
    SslContext1.SslCAFile      := CAFileEdit.Text;
    SslContext1.SslCAPath      := CAPathEdit.Text;
    SslContext1.SslVerifyPeer  := VerifyPeerCheckBox.Checked;
    SslWSocket1.SetAcceptableHostsList(AcceptableHostsEdit.Text);
    SslWSocket1.Listen;
    OutputDebugString('Listen called.');
    Display('Listening');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsSrvForm.CloseButtonClick(Sender: TObject);
var
    I : Integer;
begin
    SslWSocket1.Close;
    for I := FClients.Count - 1 downto 0 do begin
        Display('Disconnectiong client at ' +
                THttpConnection(FClients.Items[I]).IpAddr);
        THttpConnection(FClients.Items[I]).Abort;
        FClients.Delete(I);
        ClientCountLabel.Caption := IntToStr(FClients.Count);
    end;
    Display('Stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsSrvForm.SslWSocket1SessionAvailable(
    Sender : TObject;
    Error  : Word);
var
    NewHSocket   : TSocket;
    PeerName     : TSockAddrIn;
    ClientSocket : THttpConnection;
    Buffer       : String;
begin
    OutputDebugString('OnSessionAvailable');
    //Display('OnSessionAvailable');
    NewHSocket := (Sender as TSslWSocket).Accept;
{$IFDEF DEBUG_OUTPUT}
    OutputDebugString('NewHSocket = ' + IntToStr(NewHSocket));
{$ENDIF}
    ClientSocket                    := THttpConnection.Create(Self);
    FClients.Add(ClientSocket);
    ClientCountLabel.Caption        := IntToStr(FClients.Count);
    ClientSocket.FDocDir            := DocDirEdit.Text;
    ClientSocket.SslContext         := SslWSocket1.SslContext;
    ClientSocket.SslMode            := sslModeServer;
    ClientSocket.SslEnable          := TRUE;
    ClientSocket.SslAcceptableHosts := SslWSocket1.SslAcceptableHosts;
    ClientSocket.OnSessionConnected := ClientSessionConnected;
    ClientSocket.OnSessionClosed    := ClientSessionClosed;
    ClientSocket.OnDataAvailable    := ClientDataAvailable;
    ClientSocket.OnSslVerifyPeer    := ClientVerifyPeer;
    ClientSocket.Dup(NewHSocket);
    ClientSocket.GetPeerName(PeerName, sizeof(PeerName));
    ClientSocket.IpAddr             := WSocket_inet_ntoa(PeerName.sin_addr);
    Buffer := 'Remote ' + ClientSocket.IpAddr + ' connected';
    Display(Buffer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsSrvForm.ClientSessionConnected(Sender : TObject; Error : Word);
begin
    OutputDebugString('ClientSessionConnected');
    Display('ClientSessionConnected');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsSrvForm.ClientSessionClosed(Sender : TObject; Error : Word);
var
    Client : THttpConnection;
begin
    Client := Sender as THttpConnection;
    OutputDebugString('Remote ' + Client.IpAddr + ' SessionClosed');
    Display('Remote ' + Client.IpAddr + ' SessionClosed');
    // Release the component used for TCP session.
    // This will remove it from client list (done from destructor)
    Client.Release;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsSrvForm.ClientDataAvailable(Sender : TObject; Error : Word);
var
    Client   : THttpConnection;
    Len      : Integer;
    p        : PChar;
    PeerName : TSockAddrIn;
begin
    Client := Sender as THttpConnection;

    if Client.RcvBuf = nil then begin
        Client.RcvCount := 0;
        Client.RcvSize  := 4096;
        GetMem(Client.RcvBuf, Client.RcvSize);
    end
    else if (Client.RcvSize - Client.RcvCount) < 1024 then begin
        Client.RcvSize := Client.RcvSize + 1024;
        ReallocMem(Client.RcvBuf, Client.RcvSize);
    end;

    Len := Client.Receive(Client.RcvBuf + Client.RcvCount,
                          Client.RcvSize - Client.RcvCount - 1);
    if Len > 0 then begin
       Client.RcvCount                := Client.RcvCount + Len;
       Client.RcvBuf[Client.RcvCount] := #0;
       while Client.RcvCount > 0 do begin
           p := StrPos(Client.RcvBuf, #10);
           if p = nil then
               break;
           if p[-1] = #13 then
               p[-1] := #0;
           Client.FRcvdLine := Client.RcvBuf;
           Client.ProcessLine;
           if Client.FRcvdLineCount = 1 then begin
               Client.GetPeerName(PeerName, sizeof(PeerName));
               Client.FPeerIP := WSocket_inet_ntoa(PeerName.sin_addr);
               Display(IntToStr(ReqCount + 1) + ') [' +
                       FormatDateTime('YYYYMMDD HHNNSS', Now) + '] ' +
                       Client.FPeerIP + ' ' +
                       Client.FRcvdLine);
           end;
           Move(p[1], Client.RcvBuf[0], Client.RcvCount - (p - Client.RcvBuf));
           Client.RcvCount := Client.RcvCount - (p - Client.RcvBuf) - 1;
       end;
       LogClose;
    end
    else if Len = 0 then
        OutputDebugString('ClientDataAvailable. No Data !')
    else
        OutputDebugString('ClientDataAvailable. Error.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsSrvForm.WMSslNotTrusted(var Msg: TMessage);
begin
    if Application.MessageBox(
           PChar('Do you want to trust this certificate ?' + #10 +
                 FNotTrusted),
           PChar(Caption), MB_YESNO + MB_DEFBUTTON2) = ID_YES then begin
        FTrustedList.Add(FNotTrusted);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsSrvForm.ClientVerifyPeer(
    Sender: TObject;
    var Ok: Integer;
    Cert  : TX509Base);
var
    Client : THttpConnection;
    Issuer : String;
begin
    Client := Sender as THttpConnection;
    Issuer := Cert.IssuerOneLine;
    if Ok <> 0 then
        Display('Remote ' + Client.IpAddr +
                ' Received certificate. Issuer = "' + Issuer + '"')
    else begin
        if Cert.VerifyResult = X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN then begin
            if FTrustedList.IndexOf(Issuer) >= 0 then begin
                Display('Remote ' + Client.IpAddr +
                        ' Received certificate. Issuer = "' + Issuer + '"');
                Display('    We trust this one');
                Ok := 1;
                Exit;
            end;
            FNotTrusted := Issuer;
            SslWSocket1.CloseDelayed;
            PostMessage(Handle, WM_SSL_NOT_TRUSTED, 0, 0);
            Exit;
        end;
        Display('Remote ' + Client.IpAddr + ' Can''t verify certificate:');
        Display('  Issuer = "' + Issuer + '"');
        Display('  Error  = ' + IntToStr(Cert.VerifyResult) + ' (' + Cert.VerifyErrMsg + ')');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpConnection.Build404Page(var Response: String): Integer;
var
    Body    : String;
begin
    Body := '<HTML>' + #13#10 +
              '<BODY>' + #13#10 +
                'Error 404 Page "' + URLDecode(FPath) + '" not found !' +
              '</BODY>' + #13#10 +
            '</HTML>' + #13#10;
    Response := FVersion + ' 404 OK' + #13#10 +
                'Content-type: text/html' + #13#10 +
                'Content-length: ' + IntToStr(Length(Body)) + #13#10 +
                'Pragma: no-cache' + #13#10 +
                'Expires: -1' + #13#10 +
                #13#10 +
                Body;
    Result := Length(Response);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpConnection.BuildSslPage(var Response: String): Integer;
var
    Body    : String;
begin
    Body := '<HTML>' + #13#10 +
              '<HEAD>' + #13#10 +
                '<TITLE>' + #13#10 +
                  ProgName + ' - Version ' + ProgVersion + #13#10 +
                '</TITLE>' + #13#10 +
              '</HEAD>' + #13#10 +
              '<BODY>' + #13#10 +
                ProgName + ' - Version ' + ProgVersion + ' ' +
                ProgDate + '<BR>' + #13#10 +
                ProgCopyright + '<BR>' + #13#10 +
                IntToStr(ReqCount) + ') Hello SSL !' + #13#10 +
              '</BODY>' + #13#10 +
            '</HTML>' + #13#10;
    Response := FVersion + ' 200 OK' + #13#10 +
                'Content-type: text/html' + #13#10 +
                'Content-length: ' + IntToStr(Length(Body)) + #13#10 +
                'Pragma: no-cache' + #13#10 +
                'Expires: -1' + #13#10 +
                #13#10 +
                Body;
    Result := Length(Response);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor THttpConnection.Create(AOwner: TComponent);
begin
    OutputDebugString('THttpConnection.Create $' + IntToHex(Integer(Self), 8));
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor THttpConnection.Destroy;
begin
    OutputDebugString('THttpConnection.Destroy $' + IntToHex(Integer(Self), 8));
    if RcvBuf <> nil then begin
        FreeMem(RcvBuf);
        RcvBuf := nil;
    end;
    if Assigned(FDocStream) then
        FreeAndNil(FDocStream);
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.ParseRequest;
var
    I, J : Integer;
begin
    I := 1;
    while (I <= Length(FRcvdLine)) and (FRcvdLine[I] <> ' ') do
        Inc(I);
    FMethod := UpperCase(Copy(FRcvdLine, 1, I - 1));
    Inc(I);
    while (I <= Length(FRcvdLine)) and (FRcvdLine[I] = ' ') do
        Inc(I);
    J := I;
    while (I <= Length(FRcvdLine)) and (FRcvdLine[I] <> ' ') do
        Inc(I);
    FPath := Copy(FRcvdLine, J, I - J);
    // Find parameters
    J := Pos('?', FPath);
    if J <= 0 then
        FParams := ''
    else begin
        FParams := Copy(FPath, J + 1, Length(FPath));
        FPath   := Copy(FPath, 1, J - 1);
    end;
    Inc(I);
    while (I <= Length(FRcvdLine)) and (FRcvdLine[I] = ' ') do
        Inc(I);
    J := I;
    while (I <= Length(FRcvdLine)) and (FRcvdLine[I] <> ' ') do
        Inc(I);
    FVersion := Trim(UpperCase(Copy(FRcvdLine, J, I - J)));
    if Length(FVersion) = 0 then
        FVersion := 'HTTP/1.0';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.ProcessLine;
var
    Response : String;
    RespLen  : Integer;
    I        : Integer;
begin
    Inc(FRcvdLineCount);
    if FRcvdLineCount = 1 then begin
        // First line contains request
        LogText('!', FPeerIP + ' HDR: ' + FRcvdLine);
        ParseRequest;
        Exit;
    end;

    if Length(FRcvdLine) <> 0 then begin
        LogText('!', FPeerIP + ' HDR: ' + FRcvdLine);
        Exit;
    end;

    Inc(ReqCount);
    LogText('!', FPeerIP + ' Request #' + IntToStr(ReqCount));

    // End of header
    FRcvdLineCount := 0;   // Reset line counter

    if FPath[1] = '/' then
        FDocument := FDocDir + FPath
    else
        FDocument := FDocDir + '\' + FPath;
    // Check for default document
    if FDocument[Length(FDocument)] = '/' then
        FDocument := FDocument + 'index.html';
    // Change slashes to backslashes
    for I := 1 to Length(FDocument) do begin
        if FDocument[I] = '/' then
            FDocument[I] := '\';
    end;

    FDocument := URLDecode(FDocument);

    // Send reply
    if FPath = '/sslpage' then begin
        RespLen    := BuildSslPage(Response);
        OnDataSent := DataSentShutdown;
        Send(PChar(Response), RespLen);
    end
    else if FileExists(FDocument) then
        SendDoc
    else begin
        LogText('!', FPeerIP + ' Error 404');
        RespLen    := Build404Page(Response);
        OnDataSent := DataSentShutdown;
        Send(PChar(Response), RespLen);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.SendDoc;
var
    Header    : String;
begin
    if Assigned(FDocStream) then
        FreeAndNil(FDocStream);

    FDocStream := TFileStream.Create(FDocument, fmOpenRead + fmShareDenyNone);
    Header     := FVersion + ' 200 OK' + #13#10 +
                  'Content-type: text/html' + #13#10 +
                  'Content-length: ' + IntToStr(FDocStream.Size) + #13#10 +
                  'Pragma: no-cache' + #13#10 +
                  'Expires: -1' + #13#10 +
                  #13#10;
    OnDataSent := DataSent;
    Send(PChar(Header), Length(Header));
    OutputDebugString('HTTP header sent');
    DataSent(nil, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.DataSentShutdown(Sender: TObject; Error: WORD);
begin
    if FVersion = 'HTTP/1.0' then
        ShutDown(1);  // This will send a FIN (close request) to remote
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.DataSent(Sender: TObject; Error: WORD);
var
    Buf : array [0..2047] of char;
    Len : Integer;
begin
    if not Assigned(FDocStream) then
        Exit;
    OutputDebugString('DataSent');
    if State <> wsConnected then begin
        // Session closed prematurely
        FreeAndNil(FDocStream);
        OutputDebugString('DataSent, session closed');
        Exit;
    end;
    Len := FDocStream.Read(Buf, SizeOf(Buf));
    if Len > 0 then
        Send(@Buf, Len)
    else begin
        // End of file
        FreeAndNil(FDocStream);
        OutputDebugString('DataSent, EOF');
        DataSentShutdown(Self, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.

