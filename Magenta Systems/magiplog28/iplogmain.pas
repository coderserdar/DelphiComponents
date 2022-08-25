unit iplogmain;

{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

{ Magenta Systems IP Log Streaming Component - Test Application

A client and server test application for the TMagIpLog IP Log component.
Note the components are created dynamically to avoid needing to install
then in the component pallete.

Updated by Angus Robertson, Magenta Systems Ltd, England, 14th December 2018 - 2.8
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

Requires Internet Component Suite (ICS) V8.55 dated June 2018 or later and
OpenSSL 1.1.0 or later, both of which may be downloaded from:
http://wiki.overbyte.eu/wiki/index.php/ICS_Download
or https://www.magsys.co.uk/delphi/magics.asp
The latest ICS version in the nightly zip includes the latest OpenSSL.

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
14th Dec 2018 - 2.8 - removed madexcept, included final TLSv1.3 version 

Last version - new work will be 3.0 in iplogmain3.pas

}


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, IniFiles, Buttons, Dialogs, TypInfo,
  MagentaIpLog, Magsubs1,
  OverbyteIcsWSocket, OverbyteIcsWinsock, OverbyteIcsLIBEAY,
  OverbyteIcsSSLEAY, OverbyteIcsSslX509Utils, OverbyteIcsSslSessionCache,
  OverbyteIcsUtils, OverbyteIcsLogger, OverbyteIcsStreams;

type
  TIpLogForm = class(TForm)
    LocalPort: TEdit;
    MaxSockets: TEdit;
    RemoteHosts: TMemo;
    RemotePort: TComboBox;
    ServerPort: TComboBox;
    SslAllowNames: TEdit;
    SslCertAuth: TEdit;
    SslCertKey: TEdit;
    SslServCert: TEdit;
    SslCACerts: TEdit;
    SendFileName: TEdit;
    SslDHParams: TEdit;
    Protocol: TRadioGroup;
    DataClient: TCheckBox;
    DataServer: TCheckBox;
    PingRemote: TCheckBox;
    DataGap: TEdit;
    HeavyTraffic: TCheckBox;
    LocalAddr: TComboBox;
    SocketFamily: TComboBox;
    UseSSL: TCheckBox;
    LogErrors: TCheckBox;
    VerifyCertMode: TRadioGroup;
    RevokeCheck: TCheckBox;
    ReportChain: TCheckBox;
    RawData: TCheckBox;
    LogInfo: TCheckBox;
    SrvTimeout: TEdit;
    SslSecLevel: TComboBox;
    SslCliSecurity: TComboBox;
// components above saved in INI file

    doStop: TButton;
    doLocal: TButton;
    doClient: TButton;
    doExit: TButton;
    DataTimer: TTimer;
    BoxLocalMode: TGroupBox;
    Label4: TLabel;
    BoxClient: TGroupBox;
    Label5: TLabel;
    Label2: TLabel;
    BoxServer: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    PanelBottom: TPanel;
    doServer: TButton;
    Panel2: TPanel;
    LabelSendClient: TLabel;
    LabelSendServer: TLabel;
    doClear: TButton;
    SslContextSrv: TSslContext;
    SslContextCli: TSslContext;
    Label11: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    SslAvlSessionCache: TSslAvlSessionCache;
    Label13: TLabel;
    PanelLogs: TPanel;
    DataWin: TMemo;
    LogWin: TMemo;
    Label1: TLabel;
    Label14: TLabel;
    doCliSendFile: TButton;
    SelectFile: TBitBtn;
    OpenDialog: TOpenDialog;
    doSrvSendFile: TButton;
    Label15: TLabel;
    BoxLocalAddr: TGroupBox;
    Label3: TLabel;
    Label9: TLabel;
    Label8: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
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
    procedure FormResize(Sender: TObject);
    procedure doCliSendFileClick(Sender: TObject);
    procedure SelectFileClick(Sender: TObject);
    procedure doSrvSendFileClick(Sender: TObject);
  private
    { Private declarations }
    procedure SetButtons (Started: boolean) ;
    procedure LogRecvEvent (Sender: TObject; Socnr: integer; const Line: string) ;
    procedure LogProgEvent (Sender: TObject; Socnr: integer;
                              LogOption: TLogOption; const Msg: string);
    procedure LogChangeEvent (Sender: TObject; Socnr: integer;
                                                 LogState: TLogState);
    function SetSsl (client, server: boolean): boolean ;
  public
    { Public declarations }
  end;

const
    ProtoUdp = 0 ; ProtoTcp = 1 ;
    MyLogOptions: TLogOptions = [loDestEvent, loWsockErr, loSslErr] ;
    MyLogOptions2: TLogOptions = [loDestEvent, loWsockErr, loWsockInfo, loSslErr , loSslInfo] ;

var
    IpLogForm: TIpLogForm;
    IpLogClient: TSslMagIpLog ;
    IpLogServer: TSslMagIpLog ;
    CSerialNr: integer = 1 ;
    SSerialNr: integer = 1 ;
    FIniFileName: string;
    FCertificateDir: string ;
    FLocalFileStream: TFileStream ;

implementation

{$R *.dfm}

procedure TIpLogForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TMemIniFile;
    section, temp: string ;
begin
    IniFile := TMemIniFile.Create(FIniFileName);
    with IniFile do
    begin
        section := 'Main' ;
        if DataClient.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'DataClient_Checked', temp) ;
        WriteString (section, 'DataGap_Text', DataGap.Text) ;
        if DataServer.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'DataServer_Checked', temp) ;
        WriteString (section, 'FileName_Text', SendFileName.Text) ;
        if HeavyTraffic.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'HeavyTraffic_Checked', temp) ;
        WriteString (section, 'LocalAddr_Text', LocalAddr.Text) ;
        WriteString (section, 'LocalPort_Text', LocalPort.Text) ;
        WriteString (section, 'MaxSockets_Text', MaxSockets.Text) ;
        if PingRemote.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'PingRemote_Checked', temp) ;
        WriteInteger (section, 'Protocol_ItemIndex', Protocol.ItemIndex) ;
        WriteString (section, 'RemoteHosts_Lines', RemoteHosts.Lines.CommaText) ;
        WriteString (section, 'RemotePort_Text', RemotePort.Text) ;
        WriteString (section, 'ServerPort_Text', ServerPort.Text) ;
        WriteInteger (section, 'SocketFamily_ItemIndex', SocketFamily.ItemIndex) ;
        WriteString (section, 'SslAllowNames_Text', SslAllowNames.Text) ;
        WriteString (section, 'SslServCert_Text', SslServCert.Text) ;
        WriteString (section, 'SslCertAuth_Text', SslCertAuth.Text) ;
        WriteString (section, 'SslCertKey_Text', SslCertKey.Text) ;
        WriteString (section, 'SslCACerts_Text', SslCACerts.Text) ;
        if UseSSL.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'UseSSL_Checked', temp) ;
        if RawData.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'RawData_Checked', temp) ;
        WriteInteger (section, 'VerifyCertMode_ItemIndex', VerifyCertMode.ItemIndex) ;
        if RevokeCheck.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'RevokeCheck_Checked', temp) ;
        if ReportChain.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'ReportChain_Checked', temp) ;
        if LogErrors.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'LogErrors_Checked', temp) ;
        if LogInfo.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'LogInfo_Checked', temp) ;
        WriteString (section, 'SslDHParams_Text', SslDHParams.Text) ;
        WriteString (section, 'SrvTimeout_Text', SrvTimeout.Text) ;
        WriteInteger (section, 'SslSecLevel_ItemIndex', SslSecLevel.ItemIndex) ;  // Dec 2016
        WriteInteger (section, 'SslCliSecurity_ItemIndex', SslCliSecurity.ItemIndex) ;  // June 2018

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
    SL: TSslSecLevel;
    IniFile : TMemIniFile;
    section: string ;
    Level: TSslCliSecurity;  // June 2018 TSslCliSecurity instead of TSslSecLevel
begin
// see socket families and security level
    for SF := Low (TSocketFamily) to High (TSocketFamily) do
        SocketFamily.Items.Add (SocketFamilyNames [SF]) ;
    for SL := Low (TSslSecLevel) to High (TSslSecLevel) do
        SslSecLevel.Items.Add (GetEnumName(TypeInfo(TSslSecLevel), Ord(SL)));
    SslCliSecurity.Items.Clear;  // June 2018 update SSL client security levels
    for Level := Low(TSslCliSecurity) to High(TSslCliSecurity) do
         SslCliSecurity.Items.Add (SslCliSecurityNames[Level]);

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
    FIniFileName := ChangeFileExt (ParamStr (0), '.ini') ;
    FCertificateDir := ExtractFileDir (FIniFileName) + '\';
    IniFile := TMemIniFile.Create(FIniFileName);
    with IniFile do
    begin
        section := 'Main' ;
        if ReadString (section, 'DataClient_Checked', 'True') = 'True' then DataClient.Checked := true else DataClient.Checked := false ;
        DataGap.Text := ReadString (section, 'DataGap_Text', '1000') ;
        if ReadString (section, 'DataServer_Checked', 'False') = 'True' then DataServer.Checked := true else DataServer.Checked := false ;
        SendFileName.Text := ReadString (section, 'FileName_Text', '') ;
        if ReadString (section, 'HeavyTraffic_Checked', 'False') = 'True' then HeavyTraffic.Checked := true else HeavyTraffic.Checked := false ;
        LocalAddr.Text := ReadString (section, 'LocalAddr_Text', '0.0.0.0') ;
        LocalPort.Text := ReadString (section, 'LocalPort_Text', '25678') ;
        MaxSockets.Text := ReadString (section, 'MaxSockets_Text', '4') ;
        if ReadString (section, 'PingRemote_Checked', 'False') = 'True' then PingRemote.Checked := true else PingRemote.Checked := false ;
        Protocol.ItemIndex := ReadInteger (section, 'Protocol_ItemIndex', 0) ;
        RemoteHosts.Lines.CommaText := ReadString (section, 'RemoteHosts_Lines', '192.168.1.119') ;
        RemotePort.Text := ReadString (section, 'RemotePort_Text', '514') ;
        ServerPort.Text := ReadString (section, 'ServerPort_Text', '514') ;
        SocketFamily.ItemIndex := ReadInteger (section, 'SocketFamily_ItemIndex', 0) ;
        SslAllowNames.Text := ReadString (section, 'SslAllowNames_Text', '') ;
        SslServCert.Text := ReadString (section, 'SslServCert_Text', 'iplog-cert.pem') ;
        SslCertAuth.Text := ReadString (section, 'SslCertAuth_Text', 'RootCaCertsBundle.pem') ;
        SslCertKey.Text := ReadString (section, 'SslCertKey_Text', 'iplog-prvkey.pem') ;
        SslCACerts.Text := ReadString (section, 'SslCACerts_Text', '') ;
        if ReadString (section, 'UseSSL_Checked', 'False') = 'True' then UseSSL.Checked := true else UseSSL.Checked := false ;
        if ReadString (section, 'RawData_Checked', 'False') = 'True' then RawData.Checked := true else RawData.Checked := false ;
        VerifyCertMode.ItemIndex := ReadInteger (section, 'VerifyCertMode_ItemIndex', 0) ;
        if ReadString (section, 'RevokeCheck_Checked', 'False') = 'True' then RevokeCheck.Checked := true else RevokeCheck.Checked := false ;
        if ReadString (section, 'ReportChain_Checked', 'False') = 'True' then ReportChain.Checked := true else ReportChain.Checked := false ;
        if ReadString (section, 'LogErrors_Checked', 'False') = 'True' then LogErrors.Checked := true else LogErrors.Checked := false ;
        if ReadString (section, 'LogInfo_Checked', 'False') = 'True' then LogInfo.Checked := true else LogInfo.Checked := false ;
        SslDHParams.Text := ReadString (section, 'SslDHParams_Text', 'dhparam2048.pem') ;
        SrvTimeout.Text := ReadString (section, 'SrvTimeout_Text', '300') ;
        SslSecLevel.ItemIndex := ReadInteger (section, 'SslSecLevel_ItemIndex', 0) ; // Dec 2016
        SslCliSecurity.ItemIndex := ReadInteger (section, 'SslCliSecurity_ItemIndex', 0) ;  // June 2018

        Top := ReadInteger ('Window', 'Top', (Screen.Height - Height) div 2);
        Left := ReadInteger ('Window', 'Left', (Screen.Width - Width) div 2);
        Width := ReadInteger ('Window', 'Width', Width);
        Height := ReadInteger ('Window', 'Height', Height);
    end;
    IniFile.Free;

    SetButtons (false) ;
    LabelSendClient.Caption := 'Total Lines Sent to Server 0' ;
    LabelSendServer.Caption := 'Total Lines Sent by Server 0' ;

    IpLogServer := TSslMagIpLog.Create (Self) ;
    IpLogServer.Tag := 1 ;
    IpLogServer.onLogRecvEvent := LogRecvEvent ;
    IpLogServer.onLogProgEvent := LogProgEvent ;
    IpLogServer.onLogChangeEvent := LogChangeEvent ;
    IpLogServer.MaxSockets := 1 ;

    IpLogClient := TSslMagIpLog.Create (Self) ;
    IpLogClient.Tag := 2 ;
    IpLogClient.onLogRecvEvent := LogRecvEvent ;
    IpLogClient.onLogProgEvent := LogProgEvent ;
    IpLogClient.onLogChangeEvent := LogChangeEvent ;
    IpLogClient.RetryWaitSecs := 10 ;
    IpLogServer.MaxSockets := 1 ;

  // ignore OpenSSL 1.1.0 and later, or earlier
//    GSSLEAY_DLL_IgnoreNew := True;
//    GSSLEAY_DLL_IgnoreOld := True;    // Nov 2016 use latest OpenSSL
    GSSL_DLL_DIR := ExtractFilePath (ParamStr (0)) ;   // Nov 2016 from our directory
    GSSL_SignTest_Check := True;         // Nov 2016 check OpenSSL digitall signed
 //   GSSL_SignTest_Certificate := True;
   if SslSecLevel.ItemIndex <= 0 then SslSecLevel.ItemIndex := 2;   // June 2018
   if SslCliSecurity.ItemIndex <= 0 then
                SslCliSecurity.ItemIndex := Ord(sslCliSecDefault);   // June 2018
end;

procedure TIpLogForm.FormDestroy(Sender: TObject);
begin
    DataTimer.Enabled := false ;
    FreeAndNil (IpLogClient) ;
    FreeAndNil (IpLogServer) ;
end;

procedure TIpLogForm.FormResize(Sender: TObject);
begin
    LogWin.Height := PanelBottom.Top - LogWin.Top - 5 ;
    LogWin.Width := IpLogForm.Width - LogWin.Left - 15 ;
    DataWin.Width := IpLogForm.Width - DataWin.Left - 15 ;
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

procedure TIpLogForm.SocketFamilyChange(Sender: TObject);
begin
    if SocketFamily.ItemIndex = Ord (sfAnyIPv4) then
        LocalAddr.Text := ICS_ANY_HOST_V4
    else if SocketFamily.ItemIndex = Ord (sfAnyIPv6) then
        LocalAddr.Text := ICS_ANY_HOST_V6 ;
end;

procedure TIpLogForm.LocalAddrChange(Sender: TObject);
var
    SF: TSocketFamily;
begin
    if NOT WSocketIsIPEx (LocalAddr.Text, SF) then exit;
    SocketFamily.ItemIndex := Ord (SF);
end;

procedure TIpLogForm.LogChangeEvent (Sender: TObject; Socnr: integer;
                                                 LogState: TLogState);
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
    if (Sender as TMagIpLog).Tag = 1 then
        S := S + ' S['
    else
        S := S + ' C[' ;
    S := S + IntToStr (Socnr) + '] State: ' + S2 ;
    LogWin.Lines.Add (S) ;
end ;

procedure TIpLogForm.LogProgEvent (Sender: TObject; Socnr: integer;
                                LogOption: TLogOption; const Msg: string);
var
    S: string ;
begin
    S := TimeToStr (Time) ;
    if (Sender as TMagIpLog).Tag = 1 then
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
    LogWin.Lines.Add (S) ;
end ;

procedure TIpLogForm.LogRecvEvent (Sender: TObject; Socnr: integer;
                                                        const Line: string) ;
var
    S: string ;
begin
    if (Sender as TMagIpLog).Tag = 1 then
        S := 'S['
    else
        S := 'C[' ;
     S := S + IntToStr (Socnr) + '] ' + Line ;
    DataWin.Lines.Add (S) ;
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
        DataTimer.Interval := atoi (DataGap.Text) ;
        if DataClient.Checked then
        begin
            tot := 1 ;
            if HeavyTraffic.Checked then tot := 50 ;
            if IpLogClient.AnyStateOK then
            begin
                old := IpLogClient.GetSendWaiting (0) ;
                if old > IpLogClient.MaxSendBuffer - 1000 then
                    LogWin.Lines.Add ('Client already waiting to send (bytes) ' + IntToCStr (old)) ;
                for I := 1 to tot do
                begin
                    Line := AnsiString (TimeToStr (Time) +
                       ' Test Line of Data to Server, Serial ' + IntToStr (CSerialNr)) ;
                    if IpLogClient.SendLogLine (Line) then
                    begin
                        inc (CSerialNr) ;
                    end ;
                end ;
                LabelSendClient.Caption := 'Total Lines Sent to Server ' + IntToStr (CSerialNr) ;
            end ;
        end ;
        if DataServer.Checked then
        begin
            if IpLogServer.AnyStateOK then
            begin
                Line := AnsiString (TimeToStr (Time) +
                  ' Test Line of Data from Server, Serial ' + IntToStr (SSerialNr)) ;
                if IpLogServer.SendLogLine (Line) then
                begin
                   LabelSendServer.Caption := 'Total Lines Sent by Server ' + IntToStr (SSerialNr) ;
                   inc (SSerialNr) ;
                end ;
            end ;
        end ;
    finally
        DataTimer.Enabled := true ;
    end;
end;

function TIpLogForm.SetSsl (client, server: boolean): boolean ;
var
    fname: string ;
    CertStr, ErrStr: string;
    valres: TChainResult;
begin
    Result := false ;
    IpLogClient.ForceSsl := false ;
    IpLogServer.ForceSsl := false ;
    IpLogClient.LogSslContext := Nil ;
    IpLogServer.LogSslContext := Nil ;
    if (Protocol.ItemIndex <> ProtoTcp) or (NOT UseSsl.Checked) then
    begin
        Result := true ;
        exit ;
    end ;
    try
        // Feb 2016, before cert functions
        if NOT SslContextSrv.IsSslInitialized then begin
            SslContextSrv.InitializeSsl;
            LogWin.Lines.Add ('SSL Version: ' + OpenSslVersion + ', Dir: ' + GLIBEAY_DLL_FileName) ;
        end;

        // SSL server must have a certificate and private key to work
        // but generally we don't verify the client talking to the server
        if server then
        begin
            fname := SslServCert.Text;
            if (Pos (':', fname) = 0) then fname := FCertificateDir + fname ;
            if NOT FileExists (fname) then
            begin
                LogWin.Lines.Add ('Can Not Find SSL Server Certificate File - ' + fname) ;
                exit ;
            end;

         // Feb 2017 try and load bundle, certificate, private key and intermediates
            with SslContextSrv.SslCertX509 do
            begin
                LogWin.Lines.Add ('Loading SSL Server Certificate File - ' + fname) ;
                LoadFromFile (fname, croTry, croTry, 'password');
                if NOT IsPkeyLoaded then  // no private password, try from separate file
                begin
                    fname := SslCertKey.Text;
                    if (Pos (':', fname) = 0) then fname := FCertificateDir + fname ;
                    if NOT FileExists (fname) then
                    begin
                        LogWin.Lines.Add ('Can Not Find SSL Server Private Key File - ' + fname) ;
                        exit ;
                    end;
                    PrivateKeyLoadFromPemFile (fname, 'password');
                end;
                if NOT IsInterLoaded then  // no intermediate certificates, try from separate file
                begin
                    fname := SslCACerts.Text;
                    if fname <> '' then
                    begin
                        if (Pos (':', fname) = 0) then fname := FCertificateDir + fname ;
                        if NOT FileExists (fname) then
                        begin
                            LogWin.Lines.Add ('Can Not Find SSL Server CA Bundle File - ' + fname) ;
                            exit ;
                        end;
                        LoadIntersFromPemFile (fname);
                    end;
                end;

             // Feb 2017 validate server certificate chain
                fname := SslCertAuth.Text;
                if fname <> '' then
                begin
                    if (Pos (':', fname) = 0) then fname := FCertificateDir + fname ;
                    if FileExists (fname) then
                        LoadCATrustFromPemFile(fname);
                end
                else
                    LoadCATrustFromString(sslRootCACertsBundle);  { trusted root }
                valres := ValidateCertChain('', CertStr, ErrStr);   // really need host name
                if ReportChain.Checked and (CertStr <> '') then begin
                    LogWin.Lines.Add (CertStr);
                    IpLogServer.LogSslReportChain := false;  // not a second time
                end
                else
                    IpLogServer.LogSslReportChain := ReportChain.Checked ;  // Nov 2016
                if valres = chainOK then
                    ErrStr := 'Chain Validated OK'
                else if valres = chainWarn then
                    ErrStr := 'Chain Warning - ' + ErrStr
                else
                    ErrStr := 'Chain Failed - ' + ErrStr;
                LogWin.Lines.Add (ErrStr + #13#10);
            end;

            fname := SslDHParams.Text;  // May 2015
            if (Pos (':', fname) = 0) then fname := FCertificateDir + fname ;
            if NOT FileExists (fname) then
            begin
                LogWin.Lines.Add ('Can Not Find SSL Server DH Params File - ' + fname) ;
                exit ;
            end;
            SslContextSrv.SslDHParamFile := fname;
            SslContextSrv.SslOptions := { SslContextSrv.SslOptions + } [sslOpt_NO_SSLv2, sslOpt_NO_SSLv3,
                sslOpt_CIPHER_SERVER_PREFERENCE, sslOpt_NO_SESSION_RESUMPTION_ON_RENEGOTIATION,
                sslOpt_SINGLE_DH_USE, SslOpt_SINGLE_ECDH_USE] ; //SSLv2/3 are unsecure
            SslContextSrv.SslCipherList := sslCiphersNormal ;
         //   SslContextSrv.SslCipherList := sslCiphersMozillaSrvInter ;  // Oct 2014, no SSLv3
//            SslContextSrv.SslVersionMethod := sslBestVer_SERVER ;
            SslContextSrv.SslMinVersion := sslVerTLS1;               // Nov 2017
            SslContextSrv.SslMaxVersion  := sslVerMax;               // Nov 2017
            SslContextSrv.SslSecLevel := TSslSecLevel (SslSecLevel.ItemIndex);   // Dec 2016
        // June 2018, pending use IcsHosts so we can use     
            SslContextSrv.InitContext;
            if NOT SslContextSrv.CheckPrivateKey then   // Dec 2016
            begin
                LogWin.Lines.Add ('Mismatch for Certificate and Private Key') ;
                exit ;
            end;
            IpLogServer.ForceSsl := true ;
            IpLogServer.LogSslContext := SslContextSrv ;
            IpLogServer.LogSslVerMethod := logSslVerNone ;
            IpLogServer.LogSslRevocation := false ;
            if IpLogServer.LogSslVerMethod <> logSslVerBundle then SslContextCli.SslCAFile := '' ;
            IpLogServer.LogSslSessCache := SslAvlSessionCache ;
            IpLogServer.SrvTimeoutSecs := AscToInt(SrvTimeout.Text) ; // 5 July 2016
        end;

        // SSL client needs certificate authority root certificates if the server
        // certificate is to be fully verified, but we also have a list of allowed
        // certificates that fail verfication such as our own self signed certificate
        // listed in SslAllowNames.Text
        // not needed if Microsoft Certificate Store is used instead of PEM bundle
        if client then
        begin
            fname := SslCertAuth.Text;
            if fname <> '' then
            begin
                if (Pos (':', fname) = 0) then fname := FCertificateDir+ fname ;
                if NOT FileExists (fname) then
                begin
                    LogWin.Lines.Add ('Can Not Find SSL Client CA Bundle File - ' + fname) ;
                    exit ;
                end ;
                SslContextCli.SslCAFile := fname;
            end
            else
                SslContextCli.SslCALines.Text := sslRootCACertsBundle ;
            SslContextCli.SslOptions := [] ;              // Nov 2017 kill old stuff
            SslContextCli.SslCipherList := sslCiphersNormal;
 //         SslContextCli.SslVersionMethod := sslBestVer_CLIENT ;
        //    SslContextCli.SslMinVersion := sslVerTLS1;               // Nov 2017 modern stuff
        //    SslContextCli.SslMaxVersion  := sslVerMax;               // Nov 2017
        //    SslContextCli.SslSecLevel := TSslSecLevel (SslSecLevel.ItemIndex);   // Dec 2016
            SslContextCli.SslCliSecurity := TSslCliSecurity(SslCliSecurity.ItemIndex);  // June 2018 replaces prior stuff
            SslContextCli.InitContext;
            IpLogClient.ForceSsl := true ;
            IpLogClient.LogSslContext := SslContextCli ;
            IpLogClient.LogTrustedList := SslAllowNames.Text ;
            IpLogClient.LogSslVerMethod := TVerifyMethod (VerifyCertMode.ItemIndex) ;
            IpLogClient.LogSslRevocation := RevokeCheck.Checked ;
            IpLogClient.LogSslReportChain := ReportChain.Checked ;
            IpLogClient.LogSslSessCache := SslAvlSessionCache ;
        end;
        Result := true ;
    except
        LogWin.Lines.Add ('Failed to Initialise SSL - ' + GetExceptMess (ExceptObject)) ;
    end ;
end;

procedure TIpLogForm.doLocalClick(Sender: TObject);
var
    Family: TSocketFamily ;
begin
    IpLogServer.MaxSockets := 1 ;
    Family := TSocketFamily (SocketFamily.ItemIndex) ;
    if (Protocol.ItemIndex = ProtoUdp) and (Family in [sfIPv6, sfAnyIPv6]) then
    begin
         IpLogServer.LogProtocol := logprotUdpServer6 ;
         IpLogClient.LogProtocol := logprotUdpClient6 ;
    end
    else if (Protocol.ItemIndex = ProtoUdp) and (Family in [sfIPv4, sfAnyIPv4]) then
    begin
         IpLogServer.LogProtocol := logprotUdpServer ;
         IpLogClient.LogProtocol := logprotUdpClient ;
    end
    else if (Protocol.ItemIndex = ProtoTcp) and (Family in [sfIPv6, sfAnyIPv6]) then
    begin
         IpLogServer.LogProtocol := logprotTcpServer6 ;
         IpLogClient.LogProtocol := logprotTcpClient6 ;
    end
    else if (Protocol.ItemIndex = ProtoTcp) and (Family in [sfIPv4, sfAnyIPv4]) then
    begin
         IpLogServer.LogProtocol := logprotTcpServer ;
         IpLogClient.LogProtocol := logprotTcpClient ;
    end
    else
        exit ;

    SetButtons (true) ;
    IpLogClient.MaxSockets := 1 ;
    IpLogServer.MaxSockets := 1 ;
    IpLogServer.LocalIpAddr := LocalAddr.Text ;
    IpLogServer.LocalIpPort := LocalPort.Text ;
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

    if Family = sfAnyIPv6 then
        IpLogClient.RemoteHost := ICS_LOCAL_HOST_V6
    else if Family = sfAnyIPv4 then
        IpLogClient.RemoteHost := ICS_LOCAL_HOST_V4
    else
        IpLogClient.RemoteHost := LocalAddr.Text ;
    IpLogClient.RemoteIpPort := LocalPort.Text ;
    IpLogClient.CheckPing := PingRemote.Checked ;
    IpLogClient.RawData := RawData.Checked ;
    IpLogServer.RawData := RawData.Checked ;
    if (Protocol.ItemIndex = ProtoTcp) then
    begin
        if NOT SetSsl (true, true) then
        begin
            SetButtons (false) ;
            exit ;
        end ;
    end;
    IpLogServer.StartLogging ;
    IpLogClient.StartLogging ;
    DataTimerTimer (Self) ;
end;

procedure TIpLogForm.doClearClick(Sender: TObject);
begin
    DataWin.Lines.Clear ;
    LogWin.Lines.Clear ;
end;

procedure TIpLogForm.doClientClick(Sender: TObject);
var
    I, tot: integer ;
    Family: TSocketFamily ;
    Host: String ;
begin
    Family := TSocketFamily (SocketFamily.ItemIndex) ;
    if (Protocol.ItemIndex = ProtoUdp) and (Family in [sfIPv6, sfAnyIPv6]) then
         IpLogClient.LogProtocol := logprotUdpClient6
    else if (Protocol.ItemIndex = ProtoUdp) and (Family in [sfIPv4, sfAnyIPv4]) then
         IpLogClient.LogProtocol := logprotUdpClient
    else if (Protocol.ItemIndex = ProtoTcp) and (Family in [sfIPv6, sfAnyIPv6]) then
         IpLogClient.LogProtocol := logprotTcpClient6
    else if (Protocol.ItemIndex = ProtoTcp) and (Family in [sfIPv4, sfAnyIPv4]) then
         IpLogClient.LogProtocol := logprotTcpClient
    else
        exit ;

    SetButtons (true) ;
    if RemoteHosts.Lines.Count = 0 then exit ;
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
    if (Protocol.ItemIndex = ProtoTcp) then
    begin
        if NOT SetSsl (true, false) then
        begin
            SetButtons (false) ;
            exit ;
        end ;
    end;
    IpLogClient.StartLogging ;
    DataTimerTimer (Self) ;
end;

procedure TIpLogForm.doServerClick(Sender: TObject);
var
    Family: TSocketFamily ;
begin
    SetButtons (true) ;
    Family := TSocketFamily (SocketFamily.ItemIndex) ;
    IpLogServer.MaxSockets := atoi (MaxSockets.Text) ;
    if (Protocol.ItemIndex = ProtoUdp) and (Family in [sfIPv6, sfAnyIPv6]) then
         IpLogServer.LogProtocol := logprotUdpServer6
    else if (Protocol.ItemIndex = ProtoUdp) and (Family in [sfIPv4, sfAnyIPv4]) then
         IpLogServer.LogProtocol := logprotUdpServer
    else if (Protocol.ItemIndex = ProtoTcp) and (Family in [sfIPv6, sfAnyIPv6]) then
         IpLogServer.LogProtocol := logprotTcpServer6
    else if (Protocol.ItemIndex = ProtoTcp) and (Family in [sfIPv4, sfAnyIPv4]) then
         IpLogServer.LogProtocol := logprotTcpServer
    else
        exit ;

    IpLogServer.LocalIpAddr := LocalAddr.Text ;
    IpLogServer.LocalIpPort := ServerPort.Text ;
    IpLogServer.RawData := RawData.Checked ;
    if (Protocol.ItemIndex = ProtoTcp) then
    begin
        if NOT SetSsl (false, true) then
        begin
            SetButtons (false) ;
            exit ;
        end ;
    end;
    if LogInfo.Checked then
        IpLogServer.IpIcsLogger.LogOptions := MyLogOptions2
     else if LogErrors.Checked then
        IpLogServer.IpIcsLogger.LogOptions := MyLogOptions
    else
        IpLogServer.IpIcsLogger.LogOptions := [] ;
    IpLogServer.StartLogging ;
    DataTimerTimer (Self) ;
end;

procedure TIpLogForm.doStopClick(Sender: TObject);
var
    endtick: longword ;
    stopflag: boolean ;
    MySocket: TWSocket ;
begin
    DataTimer.Enabled := false ;

 // 7 July 2016 report traffic
    if IpLogServer.LogActive then
    begin
        MySocket := IpLogServer.Socket [0] ;
        if Assigned (MySocket) and Assigned (MySocket.Counter) then
            LogWin.Lines.Add ('Server session lasted ' + IntToStr (IcsCalcTickDiff
                 (MySocket.Counter.ConnectTick, IcsGetTickCount) div 1000) + ' secs, Xmit ' +
                    IntToStr (MySocket.WriteCount) + ', Recv ' + IntToStr (MySocket.ReadCount)) ;
    end;
    if IpLogClient.LogActive then
    begin
        MySocket := IpLogClient.Socket [0] ;
        if Assigned (MySocket) and Assigned (MySocket.Counter) then
            LogWin.Lines.Add ('Client session lasted ' + IntToStr (IcsCalcTickDiff
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
        LogWin.Lines.Add ('Waiting for Streams to Stop') ; // TEMP !!!
        Application.ProcessMessages ;
        Sleep (250) ;
        Application.ProcessMessages ;
    end ;
    SetButtons (false) ;
    FreeAndNil (FLocalFileStream) ;
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
        LogWin.Lines.Add ('Must Specify a File Name to Send') ;
        exit ;
    end;
    try
        if Assigned (FLocalFileStream) then
        begin
            LogWin.Lines.Add ('Last File Still Being Sent') ;
            exit ;
        end;
        FLocalFileStream := TFileStream.Create (SendFileName.Text,
                                                fmOpenRead + fmShareDenyWrite, MAX_BUFSIZE);
        if NOT IpLogClient.SendStream (FLocalFileStream) then
        begin
            FreeAndNil (FLocalFileStream) ;
            LogWin.Lines.Add ('Failed to Send File') ;
        end
        else
        begin
            LogWin.Lines.Add ('Client Sending File: ' + SendFileName.Text +
                                                ', Size ' + IntToCStr(FLocalFileStream.Size)) ;
        end;
    except
        LogWin.Lines.Add ('Failed to Open Send File - ' + GetExceptMess (ExceptObject)) ;
    end;
end;

procedure TIpLogForm.doSrvSendFileClick(Sender: TObject);
begin
    if (SendFileName.Text = '') or (NOT FileExists (SendFileName.Text)) then
    begin
        LogWin.Lines.Add ('Must Specify a File Name to Send') ;
        exit ;
    end;
    try
        if Assigned (FLocalFileStream) then
        begin
            LogWin.Lines.Add ('Last File Still Being Sent') ;
            exit ;
        end;
        FLocalFileStream := TFileStream.Create (SendFileName.Text,
                                                fmOpenRead + fmShareDenyWrite, MAX_BUFSIZE);
        if NOT IpLogServer.SendStream (FLocalFileStream) then
        begin
            FreeAndNil (FLocalFileStream) ;
            LogWin.Lines.Add ('Failed to Send File') ;
        end
        else
        begin
            LogWin.Lines.Add ('Server Sending File: ' + SendFileName.Text +
                                                ', Size ' + IntToCStr(FLocalFileStream.Size)) ;
        end;
    except
        LogWin.Lines.Add ('Failed to Open Send File - ' + GetExceptMess (ExceptObject)) ;
    end;
end;

end.
