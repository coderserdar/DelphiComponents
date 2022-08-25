{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  IP Log Streaming Component - Test Application
Creation:     Aug 2007
Updated:      May 2022
Version:      8.69
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2022 by Angus Robertson, Magenta Systems Ltd,
              Croydon, England. delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/

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


A client and server test application for the TIcsIpStrmLog IP Log component.
Note the components are created dynamically to avoid needing to install
them in the component pallete.


22nd Nov 2006 - baseline

18th August 2007 - 1.1 - using OverbyteIcsFtpSrvT instead of OverbyteIcsLibrary
UDP receive packets may be from multiple hosts, always keep IP

5th August 2008 - 1.2 - made compatible with ICS V7 and Delphi 2009
Note - only sends and receives ANSI text

20th August 2009 - 1.3 - fixed problem with MaxSockets being reported as closed
in the event when only one was open, tested with Delphi 2010

9th August 2010 - 1.4 - removed cast warnings with Delphi 2009 and later

7th July 2014 - 2.0 - ICS 8 and later, using new ICS ping
                      added IPv6 and SSL support, including server certificate checking
                      added host name support for UDP and TCP client with DNS lookup
                      save all settings in local INI file
                      added send a file stream

13th July 2015 - 2.2 - added better SSL handshake error reporting
                       added lineendCRLF, only support FF as lineend if using CR
                       added Debug Info button for ICS info level logging
                       added SSL Server DH Params, set ECDHCurves, both for ECDH ciphers
                       Note OpenSSL no longer support dhparam512, minimum is 768 bits.

23rd Oct 2015  - 2.3 - better SSL client and server certificate reporting

8th July 2016  - 2.4 - removed TBufferedFileStream
                       added SrvTimeoutSecs to close idle server sessions
                       Report session length and data xmit/recv before closing

23rd Nov 2016  - 2.5 - fixed bug reporting data sent after close
                       use sslRootCACertsBundle if no file
                       check not overloading send buffer
                       increased default MaxSendBuffer size to 64K
                       only works with latest digitally signed OpenSSL DLLs

7th March 2017 - 2.6 - check server private key and certificate match
                       better validation of list of remote hosts
                       SSL contenxt logging enabled
                       new way to load SSL certificate bundles supports
                         PFX files as well as PEM, validate chain before
                         initialising context rather than after, report
                         chain here

5th May 2017   - 2.7 - Added localhost to local IP address list
                       Ignore hosts suppressed with *

22nd June 2018 - 2.7 - support TLSv1.3
                       Added SslCliSecurity to set client security, pending
                          server version (needs IcsHosts)

20 Mar 2019 - V8.60 - Adapted for ICS, separate tab for settings, allow to
                        order X509 SSL certificates.

17 Jun 2019 - V8.62 - SSL server now works properly again.
                      Allow SSL certificates to be ordered and installed automatically.
                      Added Log Directory for log file.

13 Nov 2019 - V8.63 - Restart server once first certificate issued.
                      Corrected log file name.

27 Apr 2020 - V8.64 - Ensure old server port cleared when changing SSL.
                      Updated X509Certs slightly.
                      Support for X509Cert DNS challenge using WMI on Windowe Server.
                      Still start servers with certificate warnings.
                      Now starts with SSL self signed certificate if no certificate
                        found, then orders a certificate after five seconds.

05 Oct 2020 - V8.65 - Don't set TCP buffer size, leave it to Windows TCP Autotune.
                      If SSL handshake fails due to bad certificate or chain, remove
                        SSL session from cache so an immediate retry does not succeed by
                        skipping the certificate checks.

01 Jun 2021 - V8.67 - UDP server only now works correctly, Local always worked.
05 Nov 2021 - V8.68 - Log OpenSSL version on startup.
05 May 2022 - V8.69 - Support OCSP to check certificate revocation when verifying
                        handshake using certificate bundle.  Note OCSP settings
                        made in code, not from the GUI.  Server OCSP stapling
                        is enabled from Revoke Check tickbox.  
                      With Local mode, only start client if server starts.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsIpStmLogTst1;

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

{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}



interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, OverbyteIcsIniFiles, Buttons, Dialogs, TypInfo, ComCtrls,
{$IF CompilerVersion > 23}
  System.UITypes,
{$IFEND}
  OverbyteIcsWSocket, OverbyteIcsWinsock, OverbyteIcsLIBEAY,
  OverbyteIcsSSLEAY, OverbyteIcsSslX509Utils, OverbyteIcsSslSessionCache,
  OverbyteIcsUtils, OverbyteIcsLogger, OverbyteIcsStreams,
  OverbyteIcsIpStreamLog, OverbyteIcsSslX509Certs, OverbyteIcsWndControl,
  OverbyteIcsBlacklist,
  OverbyteIcsTicks64,  { V8.64 }
  OverbyteIcsWmi;      { V8.64 }


type
  TIpLogForm = class(TForm)
// saved components
    DataClient: TCheckBox;
    DataGap: TEdit;
    DataServer: TCheckBox;
    HeavyTraffic: TCheckBox;
    LocalAddr: TComboBox;
    LocalPort: TEdit;
    LogErrors: TCheckBox;
    LogInfo: TCheckBox;
    MaxSockets: TEdit;
    PingRemote: TCheckBox;
    Protocol: TRadioGroup;
    ProxyURL: TEdit;
    RawData: TCheckBox;
    RemoteHosts: TMemo;
    RemotePort: TComboBox;
    ReportChain: TCheckBox;
    RevokeCheck: TCheckBox;
    SendFileName: TEdit;
    ServerPort: TComboBox;
    SocketFamily: TRadioGroup;
    SrvTimeout: TEdit;
    SslCACerts: TEdit;
    SslCertAuth: TEdit;
    SslCertKey: TEdit;
    SslDomainName: TEdit;
    SslCertPassword: TEdit;
    SslServCert: TEdit;
    UseSSL: TCheckBox;
    VerifyCertMode: TRadioGroup;
    SslCertAutoOrder: TCheckBox;
    SslCertExpireDays: TEdit;
    SslCertSupplierProto: TComboBox;
    SslCertChallenge: TComboBox;
    SslCertPKeyType: TComboBox;
    SslCertSignDigest: TComboBox;
    SslCertProduct: TEdit;
    SslCertDirWork: TEdit;
    SslCliSec: TComboBox;
    SslSrvSec: TComboBox;
    DirLogs: TEdit;
    DnsChlgType: TComboBox;

// not saved
    DataTimer: TTimer;
    OpenDialog: TOpenDialog;
    PageControl1: TPageControl;
    SheetSettings: TTabSheet;
    BoxClient: TGroupBox;
    Label5: TLabel;
    Label2: TLabel;
    BoxServer: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    Label11: TLabel;
    Label10: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    BoxLocalAddr: TGroupBox;
    Label9: TLabel;
    SheetOperation: TTabSheet;
    PanelBottom: TPanel;
    doStop: TButton;
    doLocal: TButton;
    doClient: TButton;
    doExit: TButton;
    doServer: TButton;
    doClear: TButton;
    doCliSendFile: TButton;
    SelectFile: TBitBtn;
    doSrvSendFile: TButton;
    Label1: TLabel;
    PanelSplitter: TPanel;
    LabelSendClient: TLabel;
    LabelSendServer: TLabel;
    LogWin: TMemo;
    DataWin: TMemo;
    Label19: TLabel;
    Label16: TLabel;
    SslX509Certs: TSslX509Certs;
    SslAvlSessionCache: TSslAvlSessionCache;
    Label4: TLabel;
    BoxSampleData: TGroupBox;
    Label3: TLabel;
    Label8: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label31: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    SelDirLogs: TBitBtn;
    TimerLog: TTimer;
    Label26: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure doExitClick(Sender: TObject);
    procedure DataTimerTimer(Sender: TObject);
    procedure doLocalClick(Sender: TObject);
    procedure doClientClick(Sender: TObject);
    procedure doServerClick(Sender: TObject);
    procedure doStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LocalAddrChange(Sender: TObject);
    procedure SocketFamilyChange(Sender: TObject);
    procedure doClearClick(Sender: TObject);
    procedure doCliSendFileClick(Sender: TObject);
    procedure SelectFileClick(Sender: TObject);
    procedure doSrvSendFileClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SslX509CertsCertProg(Sender: TObject; LogOption: TLogOption;
      const Msg: string);
    procedure SslX509CertsNewCert(Sender: TObject);
    procedure SslX509CertsChallengeDNS(Sender: TObject;
      ChallengeItem: TChallengeItem; var ChlgOK: Boolean);
    procedure SslX509CertsOAuthAuthUrl(Sender: TObject; const URL: string);
    procedure SelDirLogsClick(Sender: TObject);
    procedure TimerLogTimer(Sender: TObject);
  private
    { Private declarations }
    procedure AddLog (const S: string) ;
    procedure OpenLogFile;
    procedure SetButtons (Started: boolean) ;
    procedure LogRecvEvent (Sender: TObject; Socnr: integer; const Line: string) ;
    procedure LogProgEvent (Sender: TObject; Socnr: integer;
                              LogOption: TLogOption; const Msg: string);
    procedure LogChangeEvent (Sender: TObject; Socnr: integer;
                                                 LogState: TStrmLogState);
  public
    { Public declarations }
  end;

const
    ProtoUdp = 0 ; ProtoTcp = 1 ;
    MyLogOptions: TLogOptions = [loDestEvent, loWsockErr, loSslErr] ;
    MyLogOptions2: TLogOptions = [loDestEvent, loWsockErr, loWsockInfo, loSslErr , loSslInfo] ;
    DnsChlgWindows = 0;  DnsChlgCloudfare = 1;

var
    IpLogForm: TIpLogForm;
    IpLogClient: TIcsIpStrmLog ;
    IpLogServer: TIcsIpStrmLog ;
    CSerialNr: integer = 1 ;
    SSerialNr: integer = 1 ;
    FIniFileName: string;
    FCertificateDir: string ;
    FLocalFileStream: TFileStream ;
    FIcsBuffLogStream: TIcsBuffLogStream;
    BuffLogLines: String;
    CertCheckTrigger: int64 ;  { V8.64 }

implementation

{$R *.dfm}

procedure TIpLogForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
    section, temp: string ;
begin
    FreeAndNil(FIcsBuffLogStream); // write log file }
    IniFile := TIcsIniFile.Create(FIniFileName);
    with IniFile do  begin
        section := 'Main' ;
        if DataClient.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'DataClient_Checked', temp) ;
        WriteString (section, 'DataGap_Text', DataGap.Text) ;
        if DataServer.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'DataServer_Checked', temp) ;
        WriteInteger (section, 'DataServer_State', Ord (DataServer.State)) ;
        if HeavyTraffic.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'HeavyTraffic_Checked', temp) ;
        WriteInteger (section, 'HeavyTraffic_State', Ord (HeavyTraffic.State)) ;
        WriteString (section, 'LocalAddr_Text', LocalAddr.Text) ;
        WriteString (section, 'LocalPort_Text', LocalPort.Text) ;
        if LogErrors.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'LogErrors_Checked', temp) ;
        WriteInteger (section, 'LogErrors_State', Ord (LogErrors.State)) ;
        if LogInfo.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'LogInfo_Checked', temp) ;
        WriteInteger (section, 'LogInfo_State', Ord (LogInfo.State)) ;
        WriteString (section, 'MaxSockets_Text', MaxSockets.Text) ;
        if PingRemote.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'PingRemote_Checked', temp) ;
        WriteInteger (section, 'Protocol_ItemIndex', Protocol.ItemIndex) ;
        WriteString (section, 'ProxyURL_Text', ProxyURL.Text) ;
        if RawData.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'RawData_Checked', temp) ;
        WriteString (section, 'RemoteHosts_Lines', RemoteHosts.Lines.CommaText) ;
        WriteString (section, 'RemotePort_Text', RemotePort.Text) ;
        if ReportChain.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'ReportChain_Checked', temp) ;
        if RevokeCheck.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'RevokeCheck_Checked', temp) ;
        WriteString (section, 'SendFileName_Text', SendFileName.Text) ;
        WriteString (section, 'ServerPort_Text', ServerPort.Text) ;
        WriteInteger (section, 'SocketFamily_ItemIndex', SocketFamily.ItemIndex) ;
        WriteString (section, 'SrvTimeout_Text', SrvTimeout.Text) ;
        WriteString (section, 'SslCACerts_Text', SslCACerts.Text) ;
        WriteString (section, 'SslCertAuth_Text', SslCertAuth.Text) ;
        WriteString (section, 'SslCertKey_Text', SslCertKey.Text) ;
        WriteInteger (section, 'SslCliSec_ItemIndex', SslCliSec.ItemIndex) ;
        WriteString (section, 'SslDomainName_Text', SslDomainName.Text) ;
        WriteString (section, 'SslCertPassword_Text', SslCertPassword.Text) ;
        WriteString (section, 'SslServCert_Text', SslServCert.Text) ;
        WriteInteger (section, 'SslSrvSec_ItemIndex', SslSrvSec.ItemIndex) ;
        if UseSSL.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'UseSSL_Checked', temp) ;
        WriteInteger (section, 'VerifyCertMode_ItemIndex', VerifyCertMode.ItemIndex) ;
       if SslCertAutoOrder.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'SslCertAutoOrder_Checked', temp) ;
       WriteString (section, 'SslCertExpireDays_Text', SslCertExpireDays.Text) ;
       WriteInteger (section, 'SslCertSupplierProto_ItemIndex', SslCertSupplierProto.ItemIndex) ;
       WriteInteger (section, 'SslCertChallenge_ItemIndex', SslCertChallenge.ItemIndex) ;
       WriteInteger (section, 'SslCertPKeyType_ItemIndex', SslCertPKeyType.ItemIndex) ;
       WriteInteger (section, 'SslCertSignDigest_ItemIndex', SslCertSignDigest.ItemIndex) ;
       WriteString (section, 'SslCertProduct_Text', SslCertProduct.Text) ;
       WriteString (section, 'SslCertDirWork_Text', SslCertDirWork.Text) ;
       WriteString (section, 'DirLogs_Text', DirLogs.Text) ;
       WriteInteger (section, 'DnsChlgType_ItemIndex', DnsChlgType.ItemIndex) ;

        WriteInteger ('Window', 'Top', Top);
        WriteInteger ('Window', 'Left', Left);
        WriteInteger ('Window', 'Width', Width);
        WriteInteger ('Window', 'Height', Height);
    end ;
    IniFile.UpdateFile;
    IniFile.Free;
end;

procedure TIpLogForm.FormCreate(Sender: TObject);
var
    SF: TSocketFamily;
    SL: TSslSrvSecurity;
    CL: TSslCliSecurity;
    I: Integer;
    CT: TChallengeType;
    SP: TSupplierProto;
    IniFile : TIcsIniFile;
    section: string ;
begin
{$IF CompilerVersion > 17}
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);     { V8.69 }
{$IFEND}

// see socket families and security levels
    SocketFamily.Items.Clear;
    for SF := Low (TSocketFamily) to High (TSocketFamily) do
        SocketFamily.Items.Add (SocketFamilyNames [SF]) ;
    SslSrvSec.Items.Clear;
    for SL := Low (TSslSrvSecurity) to High (TSslSrvSecurity) do
        SslSrvSec.Items.Add (SslSrvSecurityNames[SL]);
    SslCliSec.Items.Clear;
    for CL := Low(TSslCliSecurity) to High(TSslCliSecurity) do
         SslCliSec.Items.Add (SslCliSecurityNames[CL]);
    SslCertSupplierProto.Items.Clear;
    for SP := Low(TSupplierProto) to High(TSupplierProto) do
        SslCertSupplierProto.Items.Add(SupplierProtoLits[SP]);
    SslCertSignDigest.Items.Clear;
    for I := 0 to DigestListLitsLast do
      SslCertSignDigest.Items.Add(DigestListLits[I]);    { V8.62 }
    SslCertPKeyType.Items.Clear;
    for I := 0 to SslPrivKeyTypeLitsLast1 do
        SslCertPKeyType.Items.Add(SslPrivKeyTypeLits[I]);     { V8.62 }
    SslCertChallenge.Items.Clear;
    for CT := Low(TChallengeType) to High(TChallengeType) do
        SslCertChallenge.Items.Add(ChallengeTypeLits[CT]);    { V8.62 }

// set local IPs
    try
        LocalAddr.Items.Assign (LocalIPList (sfIPv4, IPPROTO_TCP)) ;  // Wsocket function
    except
    end ;
    if IsIPv6ApiAvailable then
    begin
        try
            LocalAddr.Items.Assign (LocalIPList (sfIPv4, IPPROTO_TCP)) ;  // Wsocket function
            LocalAddr.Items.AddStrings (LocalIPList (sfIPv6, IPPROTO_TCP)) ;    // Wsocket function
        except
        end ;
        LocalAddr.Items.Insert (0, ICS_ANY_HOST_V6);
    end;
    LocalAddr.Items.Insert (0, ICS_ANY_HOST_V4);
    LocalAddr.Items.Insert (1, ICS_LOCAL_HOST_V4);  // May 2017

// get old settings
    FIniFileName := GetIcsIniFileName;
    LogWin.Lines.Add ('INI File: ' + FIniFileName) ;
    FCertificateDir := ExtractFileDir (FIniFileName) + '\';
    IniFile := TIcsIniFile.Create(FIniFileName);
    with IniFile do
    begin
        section := 'Main' ;
        if ReadString (section, 'DataClient_Checked', 'False') = 'True' then DataClient.Checked := true else DataClient.Checked := false ;
        DataGap.Text := ReadString (section, 'DataGap_Text', '1000') ;
        if ReadString (section, 'DataServer_Checked', 'False') = 'True' then DataServer.Checked := true else DataServer.Checked := false ;
        DataServer.State := TCheckBoxState (ReadInteger (section, 'DataServer_State', Ord (cbUnchecked))) ;
        if ReadString (section, 'HeavyTraffic_Checked', 'False') = 'True' then HeavyTraffic.Checked := true else HeavyTraffic.Checked := false ;
        HeavyTraffic.State := TCheckBoxState (ReadInteger (section, 'HeavyTraffic_State', Ord (cbUnchecked))) ;
        LocalAddr.Text := ReadString (section, 'LocalAddr_Text', '0.0.0.0') ;
        LocalPort.Text := ReadString (section, 'LocalPort_Text', '25678') ;
        if ReadString (section, 'LogErrors_Checked', 'False') = 'True' then LogErrors.Checked := true else LogErrors.Checked := false ;
        LogErrors.State := TCheckBoxState (ReadInteger (section, 'LogErrors_State', Ord (cbUnchecked))) ;
        if ReadString (section, 'LogInfo_Checked', 'False') = 'True' then LogInfo.Checked := true else LogInfo.Checked := false ;
        LogInfo.State := TCheckBoxState (ReadInteger (section, 'LogInfo_State', Ord (cbUnchecked))) ;
        MaxSockets.Text := ReadString (section, 'MaxSockets_Text', '4') ;
        if ReadString (section, 'PingRemote_Checked', 'False') = 'True' then PingRemote.Checked := true else PingRemote.Checked := false ;
        Protocol.ItemIndex := ReadInteger (section, 'Protocol_ItemIndex', 0) ;
        ProxyURL.Text := ReadString (section, 'ProxyURL_Text', '') ;
        if ReadString (section, 'RawData_Checked', 'False') = 'True' then RawData.Checked := true else RawData.Checked := false ;
        RemoteHosts.Lines.CommaText := ReadString (section, 'RemoteHosts_Lines', '192.168.1.120') ;
        RemotePort.Text := ReadString (section, 'RemotePort_Text', '514') ;
        if ReadString (section, 'ReportChain_Checked', 'False') = 'True' then ReportChain.Checked := true else ReportChain.Checked := false ;
        if ReadString (section, 'RevokeCheck_Checked', 'False') = 'True' then RevokeCheck.Checked := true else RevokeCheck.Checked := false ;
        SendFileName.Text := ReadString (section, 'SendFileName_Text', '') ;
        ServerPort.Text := ReadString (section, 'ServerPort_Text', '514') ;
        SocketFamily.ItemIndex := ReadInteger (section, 'SocketFamily_ItemIndex', 0) ;
        SrvTimeout.Text := ReadString (section, 'SrvTimeout_Text', '300') ;
        SslCACerts.Text := ReadString (section, 'SslCACerts_Text', '') ;
        SslCertAuth.Text := ReadString (section, 'SslCertAuth_Text', '') ;
        SslCertKey.Text := ReadString (section, 'SslCertKey_Text', '') ;
        SslCliSec.ItemIndex := ReadInteger (section, 'SslCliSec_ItemIndex', 0) ;
        SslDomainName.Text := ReadString (section, 'SslDomainName_Text', '') ;
        SslCertPassword.Text := ReadString (section, 'SslCertPassword_Text', '') ;
        SslServCert.Text := ReadString (section, 'SslServCert_Text', '') ;
        SslSrvSec.ItemIndex := ReadInteger (section, 'SslSrvSec_ItemIndex', 0) ;
        if ReadString (section, 'UseSSL_Checked', 'False') = 'True' then UseSSL.Checked := true else UseSSL.Checked := false ;
        VerifyCertMode.ItemIndex := ReadInteger (section, 'VerifyCertMode_ItemIndex', 0) ;
       if ReadString (section, 'SslCertAutoOrder_Checked', 'False') = 'True' then SslCertAutoOrder.Checked := true else SslCertAutoOrder.Checked := false ;
       SslCertExpireDays.Text := ReadString (section, 'SslCertExpireDays_Text', '30') ;
       SslCertSupplierProto.ItemIndex := ReadInteger (section, 'SslCertSupplierProto_ItemIndex', 2) ;
       SslCertChallenge.ItemIndex := ReadInteger (section, 'SslCertChallenge_ItemIndex', 3) ;
       SslCertPKeyType.ItemIndex := ReadInteger (section, 'SslCertPKeyType_ItemIndex', 1) ;
       SslCertSignDigest.ItemIndex := ReadInteger (section, 'SslCertSignDigest_ItemIndex', 2) ;
       SslCertDirWork.Text := ReadString (section, 'SslCertDirWork_Text', '') ;
       SslCertProduct.Text := ReadString (section, 'SslCertProduct_Text', SslCertProduct.Text) ;
       DirLogs.Text := ReadString (section, 'DirLogs_Text', '') ;
       DnsChlgType.ItemIndex := ReadInteger (section, 'DnsChlgType_ItemIndex', 0) ;

        Top := ReadInteger ('Window', 'Top', (Screen.Height - Height) div 2);
        Left := ReadInteger ('Window', 'Left', (Screen.Width - Width) div 2);
        Width := ReadInteger ('Window', 'Width', Width);
        Height := ReadInteger ('Window', 'Height', Height);
    end;
    IniFile.Free;

    SetButtons (false) ;
    LabelSendClient.Caption := 'Total Lines Sent to Server 0' ;
    LabelSendServer.Caption := 'Total Lines Sent by Server 0' ;

    IpLogServer := TIcsIpStrmLog.Create (Self) ;
    IpLogServer.Tag := 1 ;
    IpLogServer.onLogRecvEvent := LogRecvEvent ;
    IpLogServer.onLogProgEvent := LogProgEvent ;
    IpLogServer.onLogChangeEvent := LogChangeEvent ;
    IpLogServer.MaxSockets := 1 ;

    IpLogClient := TIcsIpStrmLog.Create (Self) ;
    IpLogClient.Tag := 2 ;
    IpLogClient.onLogRecvEvent := LogRecvEvent ;
    IpLogClient.onLogProgEvent := LogProgEvent ;
    IpLogClient.onLogChangeEvent := LogChangeEvent ;
    IpLogClient.RetryWaitSecs := 10 ;
    IpLogServer.MaxSockets := 1 ;
    CertCheckTrigger := Trigger64Disabled;  { V8.64 }

{ load OpenSSL, then display OpenSSL DLL name and version  }
//  GSSLEAY_DLL_IgnoreNew := True;     { ignore OpenSSL 3.0 and later }
//  GSSLEAY_DLL_IgnoreOld := True;     { ignore OpenSSL 1.1 }
// note both not allowed true
    GSSL_DLL_DIR := ExtractFilePath(ParamStr(0));  { V8.68 only from our directory }
    GSSL_SignTest_Check := True;       { V8.68 check digitally signed }
    GSSL_SignTest_Certificate := True; { V8.68 check digital certificate }
    GSSLEAY_LOAD_LEGACY := False;      { V8.68 OpenSSL 3.0 legacy provider for old algorithms }
    LoadSsl;                           { V8.66 need version number }
    if NOT GSSLStaticLinked  then begin
        if NOT FileExists (GLIBEAY_DLL_FileName) then
            AddLog('SSL/TLS DLL not found: ' + GLIBEAY_DLL_FileName)
        else
            AddLog('SSL/TLS DLL: ' + GLIBEAY_DLL_FileName + ', Version: ' + OpenSslVersion);
    end
    else
        AddLog('SSL/TLS Static Linked, Version: ' + OpenSslVersion);    { V8.66 }

   if SslSrvSec.ItemIndex <= 0 then SslSrvSec.ItemIndex := Ord(sslSrvSecDefault);
   if SslCliSec.ItemIndex <= 0 then SslCliSec.ItemIndex := Ord(sslCliSecDefault);
end;

procedure TIpLogForm.FormDestroy(Sender: TObject);
begin
    CertCheckTrigger := Trigger64Disabled;  { V8.64 }
    DataTimer.Enabled := false ;
    FreeAndNil (IpLogClient) ;
    FreeAndNil (IpLogServer) ;
end;

procedure TIpLogForm.FormResize(Sender: TObject);
begin
    DataWin.Width := (PanelSplitter.Width div 3);
    LogWin.Width := PanelSplitter.Width - DataWin.Width;
    DataWin.Left := LogWin.Width;
end;

procedure TIpLogForm.AddLog (const S: string) ;
begin
    BuffLogLines := BuffLogLines + S + IcsCRLF;
    try
        if (DirLogs.Text = '') then Exit ;
        if NOT Assigned(FIcsBuffLogStream) then Exit; // sanity check
        FIcsBuffLogStream.WriteLine(S);
    except
    end;
end;

procedure TIpLogForm.TimerLogTimer(Sender: TObject);
var
    displen: integer ;
    S1: String ;
    NewFlag: Boolean;
begin
  // check SSL certificates every two hours, may order expired certs
    if IcsTestTrgTick64 (CertCheckTrigger) then  { V8.57 }
    begin
        CertCheckTrigger := IcsGetTrgMins64 (120) ;
        try
          // don't stop on first error, no exceptions
            NewFlag := IpLogServer.SrvRecheckSslCerts(S1, False, True);
            if NewFlag or (S1 <> '') then  begin
                if NewFlag then AddLog('Server Recheck Loaded New SSL Certificate(s)');
                AddLog('Recheck SSL Certificate Errors:' + IcsCRLF + S1);
            end;
        except
            on E:Exception do begin
               AddLog('Recheck SSL Certificate Failed - ' + E.Message);
            end;
        end;
    end;

    displen := Length(BuffLogLines);
    if displen > 0 then begin
        try
            SetLength(BuffLogLines, displen - 2) ;  // remove CRLF
            LogWin.Lines.Add(BuffLogLines);
            SendMessage(LogWin.Handle, EM_LINESCROLL, 0, 999999);
        except
        end ;
        BuffLogLines := '';
    end;
end;

{ V8.60 this event is used to open the log file, or change it's name
  if already opened, change only needed for GUI applications where the user
  can change the log path. Note ls written as UTF8 codepage }
procedure TIpLogForm.OpenLogFile;
var
    FName: String;
begin
    if DirLogs.Text = '' then Exit; // no log
    FName := '"' + IncludeTrailingPathDelimiter(DirLogs.Text) +
                                              'ics-ipstmlog-"yyyy-mm-dd".log"';
    if NOT Assigned(FIcsBuffLogStream) then
        FIcsBuffLogStream := TIcsBuffLogStream.Create(self, FName,
                                IpLogForm.Caption + IcsCRLF, FileCPUtf8)
    else begin
        if FName = FIcsBuffLogStream.NameMask then Exit; // skip no change
        if FIcsBuffLogStream.LogSize > 0 then
            FIcsBuffLogStream.FlushFile(True);  // changing log path, write old log first
        FIcsBuffLogStream.NameMask := FName;
    end;
    AddLog(IcsCRLF + 'Opened log file: ' + FIcsBuffLogStream.FullName);
end;

procedure TIpLogForm.SetButtons (Started: boolean) ;
begin
    doStop.Enabled := Started ;
    doLocal.Enabled := NOT Started ;
    doClient.Enabled := NOT Started ;
    doServer.Enabled := NOT Started ;
    doSrvSendFile.Enabled := Started ;
    doCliSendFile.Enabled := Started ;
end ;

procedure TIpLogForm.doExitClick(Sender: TObject);
begin
    doStopClick (Self) ;
    Close ;
end;

procedure TIpLogForm.DataTimerTimer(Sender: TObject);
var
    Line: AnsiString ;    // 8 Aug 2010
    I, tot, old: integer ;
begin
    DataTimer.Enabled := false ;
    try

    // sending sample data
        DataTimer.Interval := atoi (DataGap.Text) ;
        if DataClient.Checked then
        begin
            tot := 1 ;
            if HeavyTraffic.Checked then tot := 50 ;
            if IpLogClient.AnyStateOK then
            begin
                old := IpLogClient.GetSendWaiting (0) ;
                if old > IpLogClient.MaxSendBuffer - 1000 then
                    LogWin.Lines.Add ('Client already waiting to send (bytes) ' + IcsIntToCStr (old)) ;
                for I := 1 to tot do
                begin
                    Line := AnsiString (TimeToStr (Time) +
                       ' Test Line of Data to Server, Serial ' + IntToStr (CSerialNr)) ;
                    if IpLogClient.SendLogLine (Line) then
                    begin
                        inc (CSerialNr) ;
                    end ;
                end ;
                LabelSendClient.Caption := 'Total Lines Sent to Server ' + IcsIntToCStr (CSerialNr) ;
            end ;
        end ;
        if DataServer.Checked then
        begin
            if IpLogServer.AnyStateOK then
            begin
                Line := AnsiString (TimeToStr (Time) +
                  ' Test Line of Data from Server, Serial ' + IcsIntToCStr (SSerialNr)) ;
                if IpLogServer.SendLogLine (Line) then
                begin
                   LabelSendServer.Caption := 'Total Lines Sent by Server ' + IcsIntToCStr (SSerialNr) ;
                   inc (SSerialNr) ;
                end ;
            end ;
        end ;
    finally
        DataTimer.Enabled := true ;
    end;
end;

// local needed both client and server to send stuff to each other

procedure TIpLogForm.doLocalClick(Sender: TObject);
var
    ErrStr: string;
begin
    OpenLogFile;

/// server stuff
    IpLogServer.MaxSockets := 1 ;
    IpLogServer.RawData := RawData.Checked ;
    if (Protocol.ItemIndex = ProtoUdp) then
    begin
        IpLogServer.ForceSsl := False;
        IpLogClient.LogProtocol := logprotUdpClient ;
        IpLogServer.LogProtocol := logprotUdpServer ;
        IpLogServer.SocFamily := TSocketFamily (SocketFamily.ItemIndex) ;
        IpLogServer.LocalIpAddr := LocalAddr.Text ;
        IpLogServer.LocalIpPort := LocalPort.Text ;
    end
    else if (Protocol.ItemIndex = ProtoTcp) then
    begin
        IpLogClient.LogProtocol := logprotTcpClient ;
        IpLogClient.ForceSsl := UseSsl.Checked;

        IpLogServer.LogProtocol := logprotTcpServer ;
        IpLogServer.ForceSsl := UseSsl.Checked;
        IpLogServer.LogSslVerMethod := logSslVerNone ;
        IpLogServer.ExternalSslSessCache := SslAvlSessionCache ;
        IpLogServer.SrvTimeoutSecs := atoi(SrvTimeout.Text) ; // 5 July 2016
        IpLogServer.SrvIcsHosts.Clear;
        IpLogServer.SrvIcsHosts.Add;  // only need one host
        with IpLogServer.SrvIcsHosts [0] do  // only one host supported at moment
        begin
            HostEnabled := True;
            BindIpAddr := LocalAddr.Text ;
        //    BindIpAddr2 :=
            if NOT IpLogServer.ForceSsl then begin
                BindNonPort := atoi(LocalPort.Text);
                BindSslPort := 0;  { V8.64 }
            end
            else begin
                BindSslPort := atoi(LocalPort.Text) ;
                BindNonPort := 0;  { V8.64 }
            end;
            HostTag := 'LocalServer' ;
            Descr := HostTag;
            CertSupplierProto := SuppProtoNone;

            if IpLogServer.ForceSsl then begin
                if (SslDomainName.Text = '') or (SslServCert.Text = '') then
                begin
                    AddLog('SSL reqires certificate file name and domain name');
                    exit;
                end;
                IpLogServer.LogSslReportChain := ReportChain.Checked ;  // Nov 2016
                IpLogServer.LogSslRevocation := RevokeCheck.Checked ;   { V8.69 }
                IpLogServer.LogSslRootFile := SslCertAuth.Text;  // before SrvValidateHosts
                SslSrvSecurity := TSslSrvSecurity(SslSrvSec.ItemIndex);
                HostNames.Text := IcsTrim(SslDomainName.Text);
                SslCert := IcsTrim(SslServCert.Text);
                SslKey := IcsTrim(SslCertKey.Text);
                SslPassword := IcsTrim(SslCertPassword.text);
                SslInter := IcsTrim(SslCACerts.Text);
              { following are for automatic ordering and installation of SSL certificates }
                IpLogServer.CertExpireDays := atoi(SslCertExpireDays.Text);
                IpLogServer.SrvCertAutoOrder := SslCertAutoOrder.Checked;
                if IpLogServer.SrvCertAutoOrder then begin            { V8.62 }
                    CertDirWork := IcsTrim(SslCertDirWork.Text);
                    if (CertDirWork <> '') and DirectoryExists(CertDirWork) then begin
                        SslX509Certs.ProxyURL := ProxyURL.Text;
                        IpLogServer.SrvX509Certs := SslX509Certs;
                        CertSupplierProto := TSupplierProto(SslCertSupplierProto.ItemIndex);
                        CertChallenge := TChallengeType(SslCertChallenge.ItemIndex);
                        CertPKeyType := TSslPrivKeyType(SslCertPKeyType.ItemIndex);
                        CertProduct := IcsTrim(SslCertProduct.Text);
                        CertSignDigest := TEvpDigest(SslCertSignDigest.ItemIndex);
                    end
                    else
                        AddLog('Automatic Certificate Ordering Disabled, Work Directory Not Found');
                end;
            end;
        end;

    // set-up binding and SSL contexts, check certificates
    // validate hosts and keep site certificiate information
        if IpLogServer.ForceSsl then begin
            try
                ErrStr := IpLogServer.SrvValidateHosts(False, True); // don't stop on first error, no exceptions
                if IpLogServer.SrvIcsHosts [0].CertValRes = chainFail then begin  // V8.64  don't stop on warnings
                    AddLog('Server Not Started, Host Validation Errors:' + icsCRLF + ErrStr);
                    Exit;
                end;
            except
                on E:Exception do begin
                    AddLog('Server Not Started, Host Validation Failed - ' + E.Message);
                    Exit;
                end;
            end;

          { validation may have changed SSL bundle name }
            if IpLogServer.SrvIcsHosts [0].SslCert <> SslServCert.Text then begin
                SslServCert.Text := IpLogServer.SrvIcsHosts [0].SslCert;
                AddLog('Server changed SSL certificate file name to default: ' + SslServCert.Text);
            end;
       end;
    end
    else
        exit ;

 // client stuff common to UDP and TCP
    IpLogClient.MaxSockets := 1 ;
    IpLogClient.SocFamily := TSocketFamily (SocketFamily.ItemIndex) ;
    if IpLogClient.SocFamily = sfAnyIPv6 then
        IpLogClient.RemoteHost := ICS_LOCAL_HOST_V6
    else if IpLogClient.SocFamily = sfAnyIPv4 then
        IpLogClient.RemoteHost := ICS_LOCAL_HOST_V4
    else
        IpLogClient.RemoteHost := LocalAddr.Text ;
    IpLogClient.RemoteIpPort := LocalPort.Text ;
    IpLogClient.CheckPing := PingRemote.Checked ;
    IpLogClient.RawData := RawData.Checked ;

 // client SSL
    if (Protocol.ItemIndex = ProtoTcp) and IpLogClient.ForceSsl then
    begin
        IpLogClient.LogSslRootFile := SslCertAuth.Text;
        IpLogClient.LogSslVerMethod := TStrmVerifyMethod (VerifyCertMode.ItemIndex) ;
        IpLogClient.LogSslRevocation := RevokeCheck.Checked ;
        IpLogClient.LogSslReportChain := ReportChain.Checked ;
        IpLogClient.ExternalSslSessCache := SslAvlSessionCache ;
        IpLogClient.LogSslCliSecurity := TSslCliSecurity(SslCliSec.ItemIndex);
        IpLogClient.LogSslRootFile := IcsTrim(SslCertAuth.Text);
        IpLogClient.RemoteHost := IcsTrim(SslDomainName.Text) ;  // SSL uses host name not IP address
    end;

// diagnostic stuff
    if LogInfo.Checked then
    begin
        IpLogClient.IpIcsLogger.LogOptions := MyLogOptions2 ;
        IpLogServer.IpIcsLogger.LogOptions := MyLogOptions2 ;
    end
    else if LogErrors.Checked then
    begin
        IpLogClient.IpIcsLogger.LogOptions := MyLogOptions ;
        IpLogServer.IpIcsLogger.LogOptions := MyLogOptions ;
    end
    else
    begin
        IpLogClient.IpIcsLogger.LogOptions := [] ;
        IpLogServer.IpIcsLogger.LogOptions := [] ;
    end;

// start logging
    if IpLogServer.StartLogging then begin
        SetButtons (true) ;
        CertCheckTrigger := IcsGetTrgSecs64 (5) ;  { V8.64 first check is early to order new certificates }
        DataTimerTimer (Self) ;
        IpLogClient.StartLogging ;  { V8.69 only start client if server started }
    end;
end;

procedure TIpLogForm.doClearClick(Sender: TObject);
begin
    DataWin.Lines.Clear ;
    LogWin.Lines.Clear ;
end;

procedure TIpLogForm.doClientClick(Sender: TObject);
var
    I, tot: integer ;
    Host: String ;
begin
    OpenLogFile;

    if (Protocol.ItemIndex = ProtoUdp) then
    begin
         IpLogClient.LogProtocol := logprotUdpClient;
         IpLogClient.ForceSsl := False;
    end
    else if (Protocol.ItemIndex = ProtoTcp) then
    begin
         IpLogClient.LogProtocol := logprotTcpClient;
         IpLogClient.ForceSsl := UseSsl.Checked;
    end
    else
        exit ;

    if RemoteHosts.Lines.Count = 0 then exit ;
    IpLogClient.SocFamily := TSocketFamily (SocketFamily.ItemIndex) ;
    IpLogClient.RemoteIpPort := RemotePort.Text ;
    if RemoteHosts.Lines.Count > 1 then
    begin
        IpLogClient.MaxSockets := RemoteHosts.Lines.Count ;
        tot := 0 ;
        for I := 0 to Pred (RemoteHosts.Lines.Count) do begin
            Host := Trim (RemoteHosts.Lines [I]) ;  // Dec 2016 better check for hosts
            if (Host <> '') and (Pos ('*', Host) <> 1) then begin  // May 2017 ignore lines with *
                IpLogClient.SetRemotes (tot, Host, RemotePort.Text) ;
                inc (tot) ;
            end;
        end;
        if IpLogClient.MaxSockets <> tot then IpLogClient.MaxSockets := tot;
    end
    else
    begin
        IpLogClient.RemoteHost := RemoteHosts.Lines [0] ;
    end ;
    if LogInfo.Checked then
        IpLogClient.IpIcsLogger.LogOptions := MyLogOptions2
    else if LogErrors.Checked then
        IpLogClient.IpIcsLogger.LogOptions := MyLogOptions
    else
        IpLogClient.IpIcsLogger.LogOptions := [] ;
    IpLogClient.LocalIpAddr := LocalAddr.Text ;
    IpLogClient.CheckPing := PingRemote.Checked ;
    IpLogClient.RawData := RawData.Checked ;
    if (Protocol.ItemIndex = ProtoTcp) and IpLogClient.ForceSsl then
    begin
        IpLogClient.LogSslRootFile := SslCertAuth.Text;
        IpLogClient.LogSslVerMethod := TStrmVerifyMethod (VerifyCertMode.ItemIndex) ;
        IpLogClient.LogSslRevocation := RevokeCheck.Checked ;
        IpLogClient.LogSslReportChain := ReportChain.Checked ;
        IpLogClient.ExternalSslSessCache := SslAvlSessionCache ;
        IpLogClient.LogSslCliSecurity := TSslCliSecurity(SslCliSec.ItemIndex);
        IpLogClient.LogSslRootFile := IcsTrim(SslCertAuth.Text);
    end;
    SetButtons (true) ;
    IpLogClient.StartLogging ;
    DataTimerTimer (Self) ;
end;

procedure TIpLogForm.doServerClick(Sender: TObject);
var
    ErrStr: String;
begin
    OpenLogFile;

    IpLogServer.RawData := RawData.Checked ;
    IpLogServer.MaxSockets := atoi (MaxSockets.Text) ;

    if (Protocol.ItemIndex = ProtoUdp) then
    begin
        IpLogServer.LogProtocol := logprotUdpServer;
        IpLogServer.SocFamily := TSocketFamily (SocketFamily.ItemIndex) ;  { V8.67 must set bindings }
        IpLogServer.LocalIpAddr := LocalAddr.Text ;                        { V8.67 must set bindings }
        IpLogServer.LocalIpPort := ServerPort.Text ;                       { V8.67 must set bindings }
        IpLogServer.ForceSsl := False;
    end
    else if (Protocol.ItemIndex = ProtoTcp) then
    begin
        IpLogServer.LogProtocol := logprotTcpServer;
        IpLogServer.ForceSsl := UseSsl.Checked;
    end
    else
        exit ;

    if LogInfo.Checked then
        IpLogServer.IpIcsLogger.LogOptions := MyLogOptions2
     else if LogErrors.Checked then
        IpLogServer.IpIcsLogger.LogOptions := MyLogOptions
    else
        IpLogServer.IpIcsLogger.LogOptions := [] ;

    if (Protocol.ItemIndex = ProtoTcp) then
    begin
        IpLogServer.SocFamily := TSocketFamily (SocketFamily.ItemIndex) ;
        IpLogServer.LocalIpAddr := LocalAddr.Text ;
        IpLogServer.LocalIpPort := ServerPort.Text ;
        IpLogServer.LogSslVerMethod := logSslVerNone ;
        IpLogServer.ExternalSslSessCache := SslAvlSessionCache ;
        IpLogServer.SrvTimeoutSecs := atoi(SrvTimeout.Text) ; // 5 July 2016
        IpLogServer.SrvIcsHosts.Clear;
        IpLogServer.SrvIcsHosts.Add;  // only need one host
        with IpLogServer.SrvIcsHosts [0] do
        begin
            HostEnabled := True;
            BindIpAddr := LocalAddr.Text ;
        //    BindIpAddr2 :=
            if NOT IpLogServer.ForceSsl then begin
                BindNonPort := atoi(ServerPort.Text);
                BindSslPort := 0;  { V8.64 }
            end
            else begin
                BindSslPort := atoi(ServerPort.Text) ;
                BindNonPort := 0;  { V8.64 }
            end;
            HostTag := 'TCPServer' ;
            Descr := HostTag;
            CertSupplierProto := SuppProtoNone;

            if IpLogServer.ForceSsl then begin
                if (SslDomainName.Text = '') or (SslServCert.Text = '') then
                begin
                    AddLog('SSL reqires certificate file name and domain name');
                    exit;
                end;
                IpLogServer.LogSslReportChain := ReportChain.Checked ;  // Nov 2016
                IpLogServer.LogSslRevocation := RevokeCheck.Checked ;   { V8.69 }
                IpLogServer.LogSslRootFile := SslCertAuth.Text;  // before SrvValidateHosts
                SslSrvSecurity := TSslSrvSecurity(SslSrvSec.ItemIndex);
                HostNames.Text := IcsTrim(SslDomainName.Text);
                SslCert := IcsTrim(SslServCert.Text);
                SslKey := IcsTrim(SslCertKey.Text);
                SslPassword := IcsTrim(SslCertPassword.text);
                SslInter := IcsTrim(SslCACerts.Text);
              { following are for automatic ordering and installation of SSL certificates }
                CertSupplierProto := SuppProtoNone;                          { V8.59 sanity test }
                IpLogServer.CertExpireDays := atoi(SslCertExpireDays.Text);
                IpLogServer.SrvCertAutoOrder := SslCertAutoOrder.Checked;
                if IpLogServer.SrvCertAutoOrder then begin            { V8.62 }
                    CertDirWork := IcsTrim(SslCertDirWork.Text);
                    if (CertDirWork <> '') and DirectoryExists(CertDirWork) then begin
                        SslX509Certs.ProxyURL := ProxyURL.Text;
                        IpLogServer.SrvX509Certs := SslX509Certs;
                        CertSupplierProto := TSupplierProto(SslCertSupplierProto.ItemIndex);
                        CertChallenge := TChallengeType(SslCertChallenge.ItemIndex);
                        CertPKeyType := TSslPrivKeyType(SslCertPKeyType.ItemIndex);
                        CertProduct := IcsTrim(SslCertProduct.Text);
                        CertSignDigest := TEvpDigest(SslCertSignDigest.ItemIndex);
                    end
                    else
                        AddLog('Automatic Certificate Ordering Disabled, Work Directory Not Found');
                end;
            end;
        end;

    // set-up binding and SSL contexts, check certificates
    // validate hosts and keep site certificiate information
        if IpLogServer.ForceSsl then begin
            try
                ErrStr := IpLogServer.SrvValidateHosts(False, True); // don't stop on first error, no exceptions
                if IpLogServer.SrvIcsHosts [0].CertValRes = chainFail then begin  // V8.64  don't stop on warnings
                    AddLog('Server Not Started, Host Validation Errors:' + icsCRLF + ErrStr);
                    Exit;
                end;
            except
                on E:Exception do begin
                    AddLog('Server Not Started, Host Validation Failed - ' + E.Message);
                    Exit;
                end;
            end;

          { validation may have changed SSL bundle name }
            if IpLogServer.SrvIcsHosts [0].SslCert <> SslServCert.Text then begin
                SslServCert.Text := IpLogServer.SrvIcsHosts [0].SslCert;
                AddLog('Server changed SSL certificate file name to default: ' + SslServCert.Text);
            end;
        end;
    end;
    if IpLogServer.StartLogging then begin
        SetButtons (true) ;
        DataTimerTimer (Self) ;
        CertCheckTrigger := IcsGetTrgSecs64 (5) ;  { V8.64 first check is early to order new certificates }
    end;
end;

procedure TIpLogForm.doStopClick(Sender: TObject);
var
    endtick: longword ;
    stopflag: boolean ;
    MySocket: TWSocket ;
begin
    CertCheckTrigger := Trigger64Disabled;  { V8.64 }
    DataTimer.Enabled := false ;

 // 7 July 2016 report traffic
    if IpLogServer.LogActive then
    begin
        MySocket := IpLogServer.Socket [0] ;
        if Assigned (MySocket) and Assigned (MySocket.Counter) and
            (MySocket.Counter.ConnectTick > 0) then   { V8.63 }
               AddLog ('Server session lasted ' + IntToStr (IcsCalcTickDiff
                 (MySocket.Counter.ConnectTick, IcsGetTickCount) div 1000) + ' secs, Xmit ' +
                    IntToStr (MySocket.WriteCount) + ', Recv ' + IntToStr (MySocket.ReadCount)) ;
    end;
    if IpLogClient.LogActive then
    begin
        MySocket := IpLogClient.Socket [0] ;
        if Assigned (MySocket) and Assigned (MySocket.Counter) and
            (MySocket.Counter.ConnectTick > 0) then   { V8.63 }
              AddLog ('Client session lasted ' + IntToStr (IcsCalcTickDiff
                 (MySocket.Counter.ConnectTick, IcsGetTickCount) div 1000) + ' secs, Xmit ' +
                    IntToStr (MySocket.WriteCount) + ', Recv ' + IntToStr (MySocket.ReadCount)) ;
    end;

    IpLogServer.StopLogging ;
    IpLogClient.StopLogging ;

 // TCP does not close instantly, we should really wait until it's all done
    endtick := GetTickCount + 5000 ;  // wait five seconds
    while endtick > GetTickCount do
    begin
        stopflag := true ;
        if NOT IpLogServer.CheckStopped then stopflag := false ;
        if NOT IpLogClient.CheckStopped then stopflag := false ;
        if stopflag then break ;
        AddLog ('Waiting for Streams to Stop') ; // TEMP !!!
        Application.ProcessMessages ;
        Sleep (250) ;
        Application.ProcessMessages ;
    end ;
    SetButtons (false) ;
    FreeAndNil (FLocalFileStream) ;
end;

procedure TIpLogForm.SelDirLogsClick(Sender: TObject);
begin
    OpenDialog.InitialDir := DirLogs.Text ;
    if OpenDialog.Execute then
        DirLogs.Text := ExtractFilePath(OpenDialog.FileName);
end;

procedure TIpLogForm.SelectFileClick(Sender: TObject);
begin
    OpenDialog.FileName := SendFileName.Text ;
    OpenDialog.InitialDir := ExtractFileDir(SendFileName.Text) ;
    if OpenDialog.Execute then
        SendFileName.Text := OpenDialog.FileName;
end;

procedure TIpLogForm.doCliSendFileClick(Sender: TObject);
begin
    if (SendFileName.Text = '') or (NOT FileExists (SendFileName.Text)) then
    begin
        AddLog ('Must Specify a File Name to Send') ;
        exit ;
    end;
    try
        if Assigned (FLocalFileStream) then
        begin
            AddLog ('Last File Still Being Sent') ;
            exit ;
        end;
        FLocalFileStream := TFileStream.Create (SendFileName.Text,
                                                fmOpenRead + fmShareDenyWrite, MAX_BUFSIZE);
        if NOT IpLogClient.SendStream (FLocalFileStream) then
        begin
            FreeAndNil (FLocalFileStream) ;
            AddLog ('Failed to Send File') ;
        end
        else
        begin
            AddLog ('Client Sending File: ' + SendFileName.Text +
                                                ', Size ' + IcsIntToCStr(FLocalFileStream.Size)) ;
        end;
    except
        AddLog ('Failed to Open Send File - ' + IcsGetExceptMess (ExceptObject)) ;
    end;
end;

procedure TIpLogForm.doSrvSendFileClick(Sender: TObject);
begin
    if (SendFileName.Text = '') or (NOT FileExists (SendFileName.Text)) then
    begin
        AddLog ('Must Specify a File Name to Send') ;
        exit ;
    end;
    try
        if Assigned (FLocalFileStream) then
        begin
            AddLog ('Last File Still Being Sent') ;
            exit ;
        end;
        FLocalFileStream := TFileStream.Create (SendFileName.Text,
                                                fmOpenRead + fmShareDenyWrite, MAX_BUFSIZE);
        if NOT IpLogServer.SendStream (FLocalFileStream) then
        begin
            FreeAndNil (FLocalFileStream) ;
            AddLog ('Failed to Send File') ;
        end
        else
        begin
            AddLog ('Server Sending File: ' + SendFileName.Text +
                                                ', Size ' + IcsIntToCStr(FLocalFileStream.Size)) ;
        end;
    except
        AddLog ('Failed to Open Send File - ' + IcsGetExceptMess (ExceptObject)) ;
    end;
end;

procedure TIpLogForm.SocketFamilyChange(Sender: TObject);
begin
    if SocketFamily.ItemIndex = Ord (sfAnyIPv4) then
        LocalAddr.Text := ICS_ANY_HOST_V4
    else if SocketFamily.ItemIndex = Ord (sfAnyIPv6) then
        LocalAddr.Text := ICS_ANY_HOST_V6 ;
end;

procedure TIpLogForm.SslX509CertsCertProg(Sender: TObject;
  LogOption: TLogOption; const Msg: string);
begin
    AddLog (TimeToStr (Time) + ' ' + Msg);  { V8.63 }
end;

procedure TIpLogForm.SslX509CertsChallengeDNS(Sender: TObject;
  ChallengeItem: TChallengeItem; var ChlgOK: Boolean);            { V8.64 }
var
    I: integer;
    Zone: String;
    WmiDnsRec: TWmiDnsRec;
    Errinfo: string;
begin
    if ChallengeItem.CType = ChallDnsMan then
    begin
// !!! Add DNS TXT Record for: _acme-challenge.ftptest.org, with: 4haqLIK79RaOO7X8YYCo3f5gCzq1j6CqNy-4i34Q3sA
        if ChallengeItem.CIssueState = IssStateCancel then   // delete DNS
            AddLog('Manually Remove DNS TXT Record for: ' + ChallengeItem.CPage + ', with: ' + ChallengeItem.CDNSValue)
        else
            AddLog('Manually Add DNS TXT Record for: ' + ChallengeItem.CPage + ', with: ' + ChallengeItem.CDNSValue);
        ChlgOK := True;
    end
    else if DnsChlgType.ItemIndex = DnsChlgWindows then
    begin
        if ChallengeItem.CIssueState = IssStateCancel then   // delete DNS
            AddLog('Removing Windows DNS Server TXT Record')
        else
            AddLog('Updating Windows DNS Server TXT Record');
        Zone := ChallengeItem.CDomain;
      // strip off wild card prefix
        if Pos ('*.', Zone) = 1 then Zone := Copy(Zone, 3, 99);
        WmiDnsRec.HostName := ChallengeItem.CPage;
        WmiDnsRec.RecType := 'TXT';
        WmiDnsRec.RecData := ChallengeItem.CDNSValue;
        AddLog('Zone: ' + Zone + ': ' + WmiDnsRec.HostName + ' IN ' +
                                    WmiDnsRec.RecType + ' ' + WmiDnsRec.RecData);
        if ChallengeItem.CIssueState = IssStateCancel then   // delete DNS
        begin
            IcsWmiUpdDnsRec ('', '', '', Zone, WmiDnsRec, EdtFuncDel, Errinfo);
            ChlgOK := True;  // don't care if it worked
        end
        else begin
            I := IcsWmiUpdDnsRec ('', '', '', Zone, WmiDnsRec, EdtFuncAdd, Errinfo);
            if I = 0 then begin
                AddLog('Updated Windows DNS Server TXT Record OK');
                ChlgOK := True;
            end
            else
                AddLog('Failed to Update Windows DNS Server: ' + ErrInfo);
        end;
    end
    else if DnsChlgType.ItemIndex = DnsChlgCloudfare then
    begin
         AddLog('Cloudfare DNS Server Not Supported Yet');
    end;
end;

procedure TIpLogForm.SslX509CertsNewCert(Sender: TObject);
var
    S1: String ;
    NewFlag: Boolean;
begin
    AddLog('Server ordered new SSL cerrtificate.' + IcsCRLF +
                                               SslX509Certs.GetOrderResult);

  { if server not started yet due to no certificate, tell user to do it }
    if NOT IpLogServer.LogActive then begin
        AddLog('Restarting Server');
        if IpLogServer.StartLogging then begin      { V8.63 }
            SetButtons (true) ;
            DataTimerTimer (Self) ;
        end;
        Exit;
     end;

  { server running, so we can reload automatically }
    NewFlag := IpLogServer.SrvRecheckSslCerts(S1, False, True);
    if NewFlag then
        AddLog('Server Recheck Loaded New SSL Certificate(s)');
    if S1 <> '' then
        AddLog('Server Recheck SSL Certificate Errors:' + icsCRLF + S1);
end;

procedure TIpLogForm.SslX509CertsOAuthAuthUrl(Sender: TObject;
  const URL: string);
begin
   AddLog('Demo needs OAuth authenfication for new certificate, ' +
                'Browse to this URL: ' + URL +  ', From PC: ' + IcsGetCompName) ;
end;


procedure TIpLogForm.LocalAddrChange(Sender: TObject);
var
    SF: TSocketFamily;
begin
    if NOT WSocketIsIPEx (LocalAddr.Text, SF) then exit;
    SocketFamily.ItemIndex := Ord (SF);
end;

procedure TIpLogForm.LogChangeEvent (Sender: TObject; Socnr: integer;
                                                 LogState: TStrmLogState);
var
    S, S2: string ;
begin
    case LogState of
        logstateNone: S2 := 'None' ;
        logstateStart: S2 := 'Starting' ;
        logstateHandshake: S2 := 'SSL Handshake' ;
        logstateOK: S2 := 'OK' ;
        logstateOKStream: S2 := 'OK Sending Stream' ;
        logstateStopping: S2 := 'Stopping' ;
    end ;
    // close file stream when state changes from logstateOKStream
    if (LogState <> logstateOKStream) and (Assigned (FLocalFileStream)) then
                    FreeAndNil (FLocalFileStream) ;
    S := TimeToStr (Time) ;
    if (Sender as TIcsIpStrmLog).Tag = 1 then
        S := S + ' S['
    else
        S := S + ' C[' ;
    S := S + IntToStr (Socnr) + '] State: ' + S2 ;
    AddLog (S) ;
end ;

procedure TIpLogForm.LogProgEvent (Sender: TObject; Socnr: integer;
                                LogOption: TLogOption; const Msg: string);
var
    S: string ;
begin
    S := TimeToStr (Time) ;
    if (Sender as TIcsIpStrmLog).Tag = 1 then
        S := S + ' S['
    else
        S := S + ' C[' ;
    S := S + IntToStr (Socnr) + '] ' ;
    case LogOption of
        loWsockErr: S := S + 'WsockErr ' ;
        loWsockInfo: S := S + 'WsockInfo ' ;
        loSslErr: S := S + 'SslErr ' ;
        loSslInfo: S := S + 'SslInfo ' ;
    end;
    S := S  + Msg ;
    AddLog (S) ;
end ;

procedure TIpLogForm.LogRecvEvent (Sender: TObject; Socnr: integer;
                                                        const Line: string) ;
var
    S: string ;
begin
    if (Sender as TIcsIpStrmLog).Tag = 1 then
        S := 'S['
    else
        S := 'C[' ;
     S := S + IntToStr (Socnr) + '] ' + Line ;
    DataWin.Lines.Add (S) ;
end ;

end.
