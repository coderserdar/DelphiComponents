{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  Verify and show an OpenSSL certificate or certificate chain using
              class TMsCertChainEngine which uses MS crypto API.
              The advantage is that no trusted PEM CA store is required and
              CRL and OCSP checks (Vista+) are performed by Windows as well.
              Also unknown root certificates are downloaded from the MS root
              certificate server if required. It's not very fast so SSL session
              caching is used to speed things up.
Creation:     May 2011
Version:      8.35
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2016 by François PIETTE
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
June 2015 - V8.00 - updated SslServerName to support Server Name Indication (SNI)
Oct 2016  - V8.35 - removed old SSL2 ciphers that broke latest version

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsMsVerify1;

interface

{$IFNDEF USE_SSL}
  {$MESSAGE FATAL 'Define conditional define "USE_SSL" in the project options'};
{$ENDIF}
{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
{$IF CompilerVersion > 23}
  System.UITypes,
{$IFEND}
  OverbyteIcsWinCrypt,
  OverbyteIcsIniFiles,
  OverbyteIcsWSocket,
  OverbyteIcsWndControl,
  OverbyteIcsMsSslUtils,
  OverbyteIcsSslSessionCache;

const
  WM_RECONNECT = WM_USER + 1;

type
  TMsVerifyForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    SslWSocket1: TSslWSocket;
    SslContext1: TSslContext;
    HostEdit: TEdit;
    PortEdit: TEdit;
    TimeoutEdit: TEdit;
    RevocationCheckBox: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ConnectButton: TButton;
    ShowCertButton: TButton;
    DisconnectButton: TButton;
    SslAvlSessionCache1: TSslAvlSessionCache;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure SslWSocket1SslVerifyPeer(Sender: TObject; var Ok: Integer;
      Cert: TX509Base);
    procedure SslWSocket1SslHandshakeDone(Sender: TObject; ErrCode: Word;
      PeerCert: TX509Base; var Disconnect: Boolean);
    procedure ConnectButtonClick(Sender: TObject);
    procedure SslWSocket1SessionClosed(Sender: TObject; ErrCode: Word);
    procedure SslWSocket1SessionConnected(Sender: TObject; ErrCode: Word);
    procedure SslWSocket1DataAvailable(Sender: TObject; ErrCode: Word);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure ShowCertButtonClick(Sender: TObject);
    procedure SslWSocket1SslCliGetSession(Sender: TObject;
      var SslSession: Pointer; var FreeSession: Boolean);
    procedure SslWSocket1SslCliNewSession(Sender: TObject;
      SslSession: Pointer; WasReused: Boolean; var IncRefCount: Boolean);
    procedure HostEditChange(Sender: TObject);
  private
    FInitialized : Boolean;
    FIniFile     : TIcsIniFile;
    FMsCertChainEngine: TMsCertChainEngine;
    procedure Display(const Msg: String);
    procedure ShowHideCertButton(AShow, ASafe: Boolean; const ACaption: String);
  protected
    procedure WmReconnect(var Msg: TMessage); message WM_RECONNECT;
  public
    property IniFile: TIcsIniFile read FIniFile;
  end;

var
  MsVerifyForm: TMsVerifyForm;

implementation

{$R *.dfm}

uses
    OverbyteIcsSSLEAY, OverbyteIcsLIBEAY, OverbyteIcsMD5;

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionSettings    = 'Settings';
    KeyHost            = 'Host';
    KeyPort            = 'Port';
    KeyRevocation      = 'CheckRevocation';
    KeyUrlTimeout      = 'UrlRetrievalTime';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsVerifyForm.FormCreate(Sender: TObject);
begin
{$IF RTLVersion >= 18}
    { Built-in memory leak detection and display since BDS2006 }
    { This is useful for debugging, however a bit slower.      }
    ReportMemoryLeaksOnShutdown := DebugHook <> 0;
{$IFEND}
    DisplayMemo.Clear;
    FIniFile := TIcsIniFile.Create(OverbyteIcsIniFiles.GetIcsIniFileName);
    ToolsPanel.DoubleBuffered   := True;
    ToolsPanel.ParentBackground := False;
    HostEdit.DoubleBuffered     := True;
    DisplayMemo.DoubleBuffered  := True;
    DisconnectButton.Enabled    := False;
    ShowCertButton.Width        := 0;
    HostEdit.OnChange           := nil;
    SslContext1.SslVerifyPeer   := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsVerifyForm.FormDestroy(Sender: TObject);
begin
    FIniFile.Free;
    FMsCertChainEngine.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsVerifyForm.FormShow(Sender: TObject);
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                               (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                               (Screen.Width  - Width)  div 2);
        HostEdit.Text := IniFile.ReadString(SectionSettings, KeyHost,
                                            'www.embarcadero.com');
        PortEdit.Text := IniFile.ReadString(SectionSettings, KeyPort, '443');
        RevocationCheckBox.Checked := IniFile.ReadBool(SectionSettings,
                                                       KeyRevocation, TRUE);
        TimeoutEdit.Text := IniFile.ReadString(SectionSettings, KeyUrlTimeout,
                                               '15000');
        HostEdit.OnChange := HostEditChange;
 //       GSSLEAY_DLL_IgnoreNew := true;  { V8.35 ignore OpenSSL 1.1.0 and later }
        try
            SslContext1.InitContext;
        except
            on E:Exception do begin
                Display('Failed to initialize SSL Context: ' + E.Message);
                Exit;
            end;
         end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsVerifyForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    try
        IniFile.WriteInteger(SectionWindow,   KeyTop,         Top);
        IniFile.WriteInteger(SectionWindow,   KeyLeft,        Left);
        IniFile.WriteInteger(SectionWindow,   KeyWidth,       Width);
        IniFile.WriteInteger(SectionWindow,   KeyHeight,      Height);
        IniFile.WriteString(SectionSettings,  KeyHost,        HostEdit.Text);
        IniFile.WriteString(SectionSettings,  KeyPort,        PortEdit.Text);
        IniFile.WriteBool(SectionSettings,    KeyRevocation,  RevocationCheckBox.Checked);
        IniFile.WriteString(SectionSettings,  KeyUrlTimeout,  TimeoutEdit.Text);
        IniFile.UpdateFile;
    except
        on E: Exception do
            MessageDlg(E.ClassName + ' ' + E.Message, mtError, [mbOK], 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsVerifyForm.Display(const Msg: String);
var
    I : Integer;
begin
    if not Assigned(DisplayMemo) then
        Exit;
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            { This is much faster than deleting line by line of the memo }
            { however still slow enough to throttle ICS speed!           }
            with TStringList.Create do
            try
                BeginUpdate;
                Assign(DisplayMemo.Lines);
                for I := 1 to 50 do
                    Delete(0);
                DisplayMemo.Lines.Text := Text;
            finally
                Free;
            end;
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        { Makes last line visible }
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsVerifyForm.HostEditChange(Sender: TObject);
begin
    if ShowCertButton.Width > 0 then begin
        ShowHideCertButton(False, False, '');
        DisconnectButtonClick(nil);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsVerifyForm.ShowHideCertButton(AShow, ASafe: Boolean;
  const ACaption: String);
begin
    if AShow then begin
        ShowCertButton.Caption := ACaption;
        ShowCertButton.Width   := Canvas.TextWidth(ACaption) + 10;
        HostEdit.Left     := ShowCertButton.Left + ShowCertButton.Width;
        HostEdit.Width    := HostEdit.Width - ShowCertButton.Width;
        if ShowCertButton.CanFocus then
            ShowCertButton.SetFocus;
        if ASafe then
        begin
            HostEdit.Color := Rgb($66, $99, $FF);
            ShowCertButton.Hint := 'Secure: ' + ACaption;
        end
        else begin
            HostEdit.Color := Rgb($FF, $00, $66);
            ShowCertButton.Hint := 'User trusted: ' + ACaption;
        end;
    end
    else begin
        HostEdit.Color        := clWindow;
        HostEdit.Width        := HostEdit.Width + ShowCertButton.Width;
        HostEdit.Left         := ShowCertButton.Left;
        ShowCertButton.Width  := 0;
    end;
    ShowCertButton.Update;
    HostEdit.Update;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsVerifyForm.SslWSocket1SessionClosed(Sender: TObject;
  ErrCode: Word);
begin
    ConnectButton.Enabled    := TRUE;
    DisconnectButton.Enabled := FALSE;
    Display('Session closed');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsVerifyForm.SslWSocket1SessionConnected(Sender: TObject;
  ErrCode: Word);
begin
    if ErrCode <> 0 then
        Display('Session connected error #' + IntToStr(ErrCode))
    else
        DisconnectButton.Enabled := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsVerifyForm.SslWSocket1DataAvailable(Sender: TObject;
  ErrCode: Word);
var
    Buf: array[0..4095] of AnsiChar;
    Rcvd: Integer;
begin
    if ErrCode = 0 then begin
        Rcvd := SslWSocket1.Receive(@Buf[0], SizeOf(Buf));
        if Rcvd > 0 then
            Display(Format('Received %d bytes', [Rcvd]));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsVerifyForm.SslWSocket1SslCliGetSession(Sender: TObject;
  var SslSession: Pointer; var FreeSession: Boolean);
begin
    SslSession := SslAvlSessionCache1.GetCliSession(SslWSocket1.Addr+
                                                    SslWSocket1.Port,
                                                    FreeSession);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsVerifyForm.SslWSocket1SslCliNewSession(Sender: TObject;
  SslSession: Pointer; WasReused: Boolean; var IncRefCount: Boolean);
begin
    if (not WasReused) then begin
        SslAvlSessionCache1.CacheCliSession(SslSession,
                                            SslWSocket1.Addr +
                                            SslWSocket1.Port,
                                            IncRefCount);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsVerifyForm.SslWSocket1SslVerifyPeer(Sender: TObject;
  var Ok: Integer; Cert: TX509Base);
begin
    Ok := 1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsVerifyForm.WmReconnect(var Msg: TMessage);
begin
    ConnectButtonClick(nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsVerifyForm.SslWSocket1SslHandshakeDone(Sender: TObject;
  ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
var
    ChainVerifyResult : LongWord;
    UserTrusted       : Boolean;
    CertChain         : TX509List;
    Safe              : Boolean;
    I                 : Integer;
begin
    if (ErrCode <> 0) or (PeerCert.X509 = nil) then
        Exit; // Nothing to check

    { SslAcceptableHosts is used to tell us whether a session was explicitly  }
    { excepted by the user, don't clear this list at runtime otherwise a user }
    { accepted, reused session would be shown OK.                             }
    UserTrusted := SslWSocket1.SslAcceptableHosts.IndexOf(
            SslWSocket1.Addr + SslWSocket1.Port) > -1;

    if not SslWSocket1.SslSessionReused then
    begin
        { Here certificate verification starts, create our engine if not yet }
        { done.                                                              }
        if not Assigned(FMsCertChainEngine) then
            FMsCertChainEngine := TMsCertChainEngine.Create;

        if RevocationCheckbox.Checked then
            { This option includes CRL checks and OCSP checks in Vista+ }
            FMsCertChainEngine.VerifyOptions :=
                [mvoRevocationCheckChainExcludeRoot]
        else
            FMsCertChainEngine.VerifyOptions := [];

        { This option doesn't seem to work, at least when a DNS lookup fails }
        FMsCertChainEngine.UrlRetrievalTimeoutMsec := StrToInt(TimeoutEdit.Text);

        { The chain contains all certificates the peer sent to us. Note that }
        { SslCaPath and SslCaFile should not be assigned.                    }
        CertChain := SslWSocket1.SslCertChain;

        { Pass the certificate and the chain certificates to the engine      }
        FMsCertChainEngine.VerifyCert(PeerCert, CertChain,
                                      ChainVerifyResult, True);

        Safe := (ChainVerifyResult = 0) or
                { We ignore the case if a revocation status is unknown.      }
                (ChainVerifyResult = CERT_TRUST_REVOCATION_STATUS_UNKNOWN) or
                (ChainVerifyResult = CERT_TRUST_IS_OFFLINE_REVOCATION) or
                (ChainVerifyResult = CERT_TRUST_REVOCATION_STATUS_UNKNOWN or
                                     CERT_TRUST_IS_OFFLINE_REVOCATION);

        { The MsChainVerifyErrorToStr function works on chain error codes     }
        Display(Format('Chain verify result $%.8x %s'#13#10,
            [ChainVerifyResult, MsChainVerifyErrorToStr(ChainVerifyResult)]));

        { This is very important since the certificate must be issued to the  }
        { server DNS name we connect to, if not, one should not trust it.     }
        if not PeerCert.PostConnectionCheck(HostEdit.Text) then begin
          Display('Post connection check failed:'#13#10 +
                  'The name specified in the peer certificate ' +
                  'doesn''t match the DNS name!'#13#10);
          Safe := False;
        end;

        { Display each certificate in chain with verify result                }
        Display(Format('%d Certificates in the verify chain:',
            [CertChain.Count]));
        { The following works only if FMsCertChainEngine.VerifyCert was called}
        { with a non-nil param ACertChain and param AUpdateChain = TRUE.      }
        for I := 0 to CertChain.Count -1 do
        begin
          { TMsCertChainEngine writes the Microsoft error codes to the        }
          { TX509Base.CustomVerifyResult property. Ignore any other result.   }
          { MsCertVerifyErrorToStr() works on certificate error codes.        }
          Display(IntToStr(I + 1) + ')');
          Display(' ' + CertChain[I].SubjectOneLine + #13#10 + ' VerifyResult: ' +
             MsCertVerifyErrorToStr(CertChain[I].CustomVerifyResult) + #13#10);
        end;
    end
    else
        Safe := not UserTrusted;

    if not Safe then begin
        ShowHideCertButton(True, Safe, HostEdit.Text);

        if not UserTrusted then begin
          SslWSocket1.Abort;
          if MessageDlg('The certificate is not OK!'#13#10 + 'Continue the ' +
                        'connection nonetheless and accept it for this session?',
                         mtWarning, [mbYes, mbNo], 0) = mrYes then
          begin
              SslWSocket1.SslAcceptableHosts.Add(SslWSocket1.Addr + SslWSocket1.Port);
              PostMessage(Handle, WM_RECONNECT, 0, 0);
          end;
        end
        else
            Display('User trusted connection:');
    end
    else begin
        ShowHideCertButton(True, Safe, HostEdit.Text);
        Display('Secure connection:');
    end;

    if SslWSocket1.State = wsConnected then
    Display(Format('IP %s:%s, %s, cipher %s, %d secret bits ' +
                    '(%d total), session reused %d', [SslWSocket1.GetPeerAddr,
                    SslWSocket1.GetPeerPort, SslWSocket1.SslVersion,
                    SslWSocket1.SslCipher, SslWSocket1.SslSecretBits,
                    SslWSocket1.SslTotalBits, Ord(SslWSocket1.SslSessionReused)]));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsVerifyForm.ConnectButtonClick(Sender: TObject);
begin
    ShowHideCertButton(False, False, '');
    Display('');
    Display('Connecting to ' + HostEdit.Text + ' port #' + PortEdit.Text);
    SslWSocket1.Addr               := HostEdit.Text;
    SslWSocket1.SslServerName      := HostEdit.Text;   // angus support SNI
    SslWSocket1.Port               := PortEdit.Text;
    SslWSocket1.Proto              := 'tcp';
    SslWSocket1.SslEnable          := TRUE;
    SslWSocket1.OnSslVerifyPeer    := SslWSocket1SslVerifyPeer;
    SslWSocket1.OnSslHandshakeDone := SslWSocket1SslHandshakeDone;
    SslWSocket1.Connect;
    ConnectButton.Enabled := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsVerifyForm.DisconnectButtonClick(Sender: TObject);
begin
    if SslWSocket1.State <> wsClosed then
        SslWSocket1.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsVerifyForm.ShowCertButtonClick(Sender: TObject);
begin
    if Assigned(FMsCertChainEngine) then begin
        if SslWSocket1.SslSessionReused then
            FMsCertChainEngine.ViewCertVerified(SslWSocket1.SslPeerCert, Handle)
        else
            FMsCertChainEngine.ViewCertLastVerified(Handle);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
