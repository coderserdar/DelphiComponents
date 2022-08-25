{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Oct 10, 1999
Description:  WebSrv1 show how to use THttpServer component to implement
              a web server. WARNING: The code below is for demonstration
              only. You need to add code to fit your needs about security.
              The code below allows to get all files on the computer running
              the demo. Add code in OnGetDocument, OnHeadDocument and
              OnPostDocument to check for authorized access to files.
Version:      8.69
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1999-2022 by François PIETTE
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
May 21, 2000 V1.01 Worked around a bug with Delphi 3 and lpVendorInfo
Oct 07, 2001 V1.02 Added Logfile feature
                   Added display if time and IP Addr for GET command.
Feb 15, 2003 V1.03 Call PostedDataReceived so that things are handled
                   correctly with HTTP 1.1 version.
Mar 11, 2003 V1.04 Changer LingerOnOff to LingerNotSet (Wilfried)
Jul 19, 2003 V1.05 Support SSL (that is HTTPS protocol)
Nov 19, 2005 V1.06 Make this sample support both HTTPS (SSL) and HTTP on two
                   different ports (2 component used)
Dec 14, 2005 V1.07 A. Garrels fixed the call to get a session ID string,
                   added a simple SSL renegotiation request (doesn't work
                   with IE 6 so far!?).
Aug 04, 2005 V1.08 A. Garrels made a few changes to prepare code for Unicode.
Jul 9, 2014  V8.00 Angus using better SSL cipher list for more secure comms
Dec 9, 2014  V8.01 Angus added SslHandshakeRespMsg for better error handling
                   Disable SSL3
Mar 16 2015  V8.02 Angus added DHParam File needed to supporting DH key exchange
                   Added EllCurve to support ECDH key exchange
                   Display SSL handshake info on demo menu
                   Added Server Name Indication (SNI) display, used to support
                     multiple host and certificates on the same IP address
Mar 23 2015  V8.03 SslServerName is now a published property
Feb 22 2016  V8.04 Angus fixed exception posting data
Mar 17 2016  V8.05 Angus added local IP address, SSL Version and SSL Cipher selection
                    and new suite edits to make testing easier
                   Reset SSL when changing parameters to try and force new negotiation
                   Report server SSL certificates and warn if expired
                   Display more SSL diags on demo.html menu
Mar 23 2016  V8.06 Angus set ErrCode in onSslServerName event to stop Java clients
                    rejecting SSL connections, and illustrate it's proper use
May 24 2016  V8.27 Angus testing OpenSSL 1.1.0, added SslThrdLock
                   Specify minimum and maximum SSL version supported
                   List SSL ciphers available and supported by protocols
                   Old SSL check box to ignore OpenSSL 1.1.0 and use older versions
Nov 04 2016  V8.37 Set friendly errors
Nov 29 2016  V8.39 Better way of listing certificates actually loaded into context
                     rather than opening files a second time
Feb 26 2017  V8.41 Added SslSecLevel to set minimum effective bits for
                     certificate key length, 128 bits and higher won't usually work!
                   Load certificates into SslCertX509 which supports PEM, DER,
                     PKCS12, PKCS8 formats and check chain for errors before
                     initialising SSL context, also reports chain
                   Removed old ciphers, adding new new cipher
Jun 26 2017 V8.49 Added .well-known directory support.  If WellKnownPath is
                     specified as a path, any access to /.well-known/xx is
                     handled locally either in the OnWellKnownDir Event or
                     by returning a file from WellKnownPath instead of DocDir.
                     This is primarily for Let's Encrypt challenges.
Feb 14, 2018 V8.52 Added IPv6 support by listing IPv6 local listen addresses,
                     and sorting them.
                   Add TLSv3 ciphers for OpenSSL 1.1.1 and later only
Jul 6, 2018  V8.56 Added SslAlpnSelect callback for SSL application layer protocol
                      negotiation, used for HTTP/2 (not supported yet)
Apr 24, 2020 V8.64 Stop web servers before closing.
                   Display SSL client hello information.
                   Certificate chain validation changed to use TX509List.  
Feb 17, 2021 V8.66 OverbyteIcsSslThrdLock gone, only used for 1.0.2.
                   ECDH setting gone, always automatic now.
                   DHParams file gone, not needed for modern ciphers.
Apr 12, 2022 V8.69 Added OCSP (Online Certificate Status Protocol) support using the TOcspHttp
                     component to confirm server SSL/TLS certificates are legitimate and not
                     revoked for security reasons (which (Let's Encrypt did with two days
                     notice on 28 Jan 2022).  The certificate OSCP response is also stapled
                     to the initial SSL/TLS HELO handshake and sent to the client to avoid
                     it needing to lookup OCSP using HTTP itself. OCSP responses are cached
                     and saved to a file for reloading later.

Note OverbyteIcsSslMultiWebServ is a better demo than this, it handles multiple listeners
with automatic SSL/TLS ordering and has more demo functions from the non-SSL sample.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSslWebServ1;

{$IFNDEF USE_SSL}
  {$MESSAGE FATAL 'Define conditional define "USE_SSL" in the project options'};
{$ENDIF}
{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

{$B-}                 { Enable partial boolean evaluation   }
{$T-}                 { Untyped pointers                    }
{$X+}                 { Enable extended syntax              }
{$I+}                 { Turn IO exceptions to on            }
{$H+}                 { Use long strings                    }
{$J+}                 { Allow typed constant to be modified }
{$IF CompilerVersion > 19}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  OFF}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$IFEND}
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}


interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, OverbyteIcsWinsock,
  OverbyteIcsWSocket, OverbyteIcsWSocketS, OverbyteIcsHttpSrv,
  OverbyteIcsLIBEAY, OverbyteIcsSSLEAY, OverbyteIcsSslSessionCache,
  OverbyteIcsSslX509Utils, OverbyteIcsLogger, OverbyteIcsWndControl,
  TypInfo, OverbyteIcsUtils, OverbyteIcsSocketUtils,
  OverbyteIcsSslHttpRest;    { V8.69 }

const
  CopyRight : String         = 'WebServ (c) 1999-2022 F. Piette V8.69 ';
  Ssl_Session_ID_Context     = 'WebServ_Test';

type
  { This component is used for client connection instead of default one.    }
  { This enables to add any data we need to handle our application.         }
  { As this data is located in client component, each connected client has  }
  { his own private data.                                                   }
  TMyHttpConnection = class(THttpConnection)
  protected
    FPostedRawData    : PAnsiChar; { Will hold dynamically allocated buffer }
    FPostedDataBuffer : PChar;     { Contains either Unicode or Ansi data   }
    FPostedDataSize   : Integer;   { Databuffer size                        }
    FDataLen          : Integer;   { Keep track of received byte count.     }
    LastHandshake     : Longword;
  public
    destructor  Destroy; override;
    constructor Create(AOwner: TComponent); override;
  end;

  { This is the main form for our application. Any data here is global for  }
  { all clients. Put provate data in TMyHttpConnection class (see above).   }
  TSslWebServForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    Label1: TLabel;
    DocDirEdit: TEdit;
    Label2: TLabel;
    DefaultDocEdit: TEdit;
    StartHttpsButton: TButton;
    StopButton: TButton;
    Label3: TLabel;
    PortHttpsEdit: TEdit;
    ClientHttpsCountLabel: TLabel;
    Label5: TLabel;
    ClearButton: TButton;
    DisplayHeaderCheckBox: TCheckBox;
    WriteLogFileCheckBox: TCheckBox;
    Label4: TLabel;
    CertFileEdit: TEdit;
    Label6: TLabel;
    PrivKeyFileEdit: TEdit;
    Label7: TLabel;
    PassPhraseEdit: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    AcceptableHostsEdit: TEdit;
    Label10: TLabel;
    CAPathEdit: TEdit;
    Label11: TLabel;
    CAFileEdit: TEdit;
    VerifyPeerCheckBox: TCheckBox;
    SslHttpServer1: TSslHttpServer;
    SslContext1: TSslContext;
    HttpServer2: THttpServer;
    StartHttpButton: TButton;
    PortHttpEdit: TEdit;
    Label12: TLabel;
    Label15: TLabel;
    ClientHttpCountLabel: TLabel;
    RenegotiationIntervalEdit: TEdit;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    ButtonOSSLVersion: TButton;
    DisplaySslInfoCheckBox: TCheckBox;
    IcsLogger1: TIcsLogger;
    SslAvlSessionCache1: TSslAvlSessionCache;
    Label21: TLabel;
    SslCipherEdit: TEdit;
    Label14: TLabel;
    SslCipherList: TComboBox;
    Label22: TLabel;
    ListenAddr: TComboBox;
    Label20: TLabel;
    SslMinVersion: TComboBox;
    Label23: TLabel;
    SslMaxVersion: TComboBox;
    OldSslCheckBox: TCheckBox;
    Label24: TLabel;
    DebugEventCheckBox: TCheckBox;
    SslSecLevel: TComboBox;
    Label25: TLabel;
    WellKnownPathEdit: TEdit;
    Label26: TLabel;
    OcspSrvHttp: TOcspHttp;
    OcspCheckBox: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SslHttpServer1GetDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure StartHttpsButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure SslHttpServer1ClientConnect(Sender: TObject;
      Client: TObject; Error: Word);
    procedure SslHttpServer1ClientDisconnect(Sender: TObject;
      Client: TObject; Error: Word);
    procedure SslHttpServer1ServerStarted(Sender: TObject);
    procedure SslHttpServer1ServerStopped(Sender: TObject);
    procedure SslHttpServer1HeadDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure SslHttpServer1PostedData(Sender: TObject;
      Client: TObject; Error: Word);
    procedure SslHttpServer1PostDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure ClearButtonClick(Sender: TObject);
    procedure WriteLogFileCheckBoxClick(Sender: TObject);
    procedure SslHttpServer1SslVerifyPeer(Sender: TObject; var Ok: Integer;
      Cert: TX509Base);
    procedure FormDestroy(Sender: TObject);
    procedure SslHttpServer1SslSvrGetSession(Sender: TObject;
      var SslSession: Pointer; SessId: Pointer; Idlen: Integer;
      var IncRefCount: Boolean);
    procedure SslHttpServer1SslSvrNewSession(Sender: TObject; SslSession,
      SessId: Pointer; Idlen: Integer; var AddToInternalCache: Boolean);
    procedure StartHttpButtonClick(Sender: TObject);
    procedure HttpServer2ServerStopped(Sender: TObject);
    procedure HttpServer2ServerStarted(Sender: TObject);
    procedure HttpServer2ClientConnect(Sender, Client: TObject;
      Error: Word);
    procedure HttpServer2ClientDisconnect(Sender, Client: TObject;
      Error: Word);
    procedure RenegotiationIntervalEditChange(Sender: TObject);
    procedure SslHttpServer1BeforeProcessRequest(Sender, Client: TObject);
    procedure SslHttpServer1SslHandshakeDone(Sender: TObject;
      ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
    procedure ButtonOSSLVersionClick(Sender: TObject);
    procedure SslHttpServer1SslSetSessionIDContext(Sender: TObject;
      var SessionIDContext: String);
    procedure SslHttpServer1SslServerName(Sender      : TObject;
                                  var Ctx     : TSslContext;
                                  var ErrCode : TTlsExtError);
    procedure IcsLogger1IcsLogEvent(Sender: TObject; LogOption: TLogOption;
      const Msg: string);
    procedure SslHttpServer1WellKnownDir(Sender, Client: TObject;
      const Path: string; var BodyStr: string);
    procedure HttpServer2WellKnownDir(Sender, Client: TObject;
      const Path: string; var BodyStr: string);
    procedure SslHttpServer1SslAlpnSelect(Sender: TObject; ProtoList: TStrings;
      var SelProto: string; var ErrCode: TTlsExtError);
    procedure OcspSrvHttpOcspProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
  private
    FIniFileName            : String;
    FInitialized            : Boolean;
    FCountRequests          : Integer;
    FLogFile                : TextFile;
    FLogFileName            : String;
    FLogFileOpened          : Boolean;
    FRenegotiationInterval  : Longword;
    FSrvSslCert             : string;
    FSrvCipherList          : string;
    FCliCipherList          : string;
    FOcspStapleResp         : string;                             { V8.69 }
    procedure CreateVirtualDocument_Demo(Sender    : TObject;
                                         Client    : TObject;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_time_htm(Sender    : TObject;
                                    Client    : TObject;
                                    var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_redir_htm(Sender    : TObject;
                                    Client    : TObject;
                                    var Flags : THttpGetFlag);
    procedure BackgroundException(Sender : TObject;
                                  E            : Exception;
                                  var CanClose : Boolean);

    procedure DisplayHeader(Client : TMyHttpConnection);
    procedure ProcessPostedData_CgiFrm1(Client : TMyHttpConnection);
    procedure CloseLogFile;
    procedure OpenLogFile;
  public
    procedure Display(Msg : String);
    property  IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  SslWebServForm: TSslWebServForm;

implementation

{$R *.DFM}

const
    { IniFile layout for persistent data }
    SectionWindow      = 'WindowMain';
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';
    KeyDocDir          = 'DocDir';
    KeyDefaultDoc      = 'DefaultDoc';
    KeyPortHttps       = 'PortHttps';
    KeyPortHttp        = 'PortHttp';
    KeyDisplayHeader   = 'DisplayHeader';
    KeyDisplaySslInfo  = 'DisplaySslInfo';
    KeyLogToFile       = 'LogToFile';
    KeyCertFile        = 'CertFile';
    KeyPassPhrase      = 'PassPhrase';
    KeyPrivKeyFile     = 'PrivKeyFile';
    KeyVerifyPeer      = 'VerifyPeer';
    KeyCAFile          = 'CAFile';
    KeyCAPath          = 'CAPath';
    KeyAcceptableHosts = 'AcceptableHosts';
    KeyRenegInterval   = 'RenegotiationInterval';
//  KeyDHFile          = 'DHFile';
//  KeyECDHList        = 'ECDHList';
    KeySslCipherEdit   = 'SslCipherEdit';
    KeySslCipherList   = 'SslCipherList';
    KeyListenAddr      = 'ListenAddr';
    KeySslMinVersion   = 'SslMinVersion';
    KeySslMaxVersion   = 'SslMaxVersion';
    KeyOldSsl          = 'OldSsl';
    KeyDebugEvent      = 'DebugEvent';
    KeySslSecLevel     = 'SslSecLevel';
    KeyWellKnownPath   = 'WellKnownPath';
    KeyOcspCheckBox    = 'OcspCheckBox';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.FormCreate(Sender: TObject);
begin
{$IFDEF DELPHI10_UP}
    // BDS2006 has built-in memory leak detection and display
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
{$ENDIF}
    //IsConsole := AllocConsole;
    { Create IniFileName based on EXE file name; }
    FIniFileName := GetIcsIniFileName;
    FLogFileName := ChangeFileExt(FIniFileName, '.log');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.FormDestroy(Sender: TObject);
begin
    //
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
    wsi     : TWSADATA;
    OldIp   : string;
    I       : integer;
    SL      : TSslSecLevel;
    InterfaceList : TStringList;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

    { V8.41 set SSL security level items from TSslSecLevel }
        for SL := Low (TSslSecLevel) to High (TSslSecLevel) do
            SslSecLevel.Items.Add (GetEnumName(TypeInfo(TSslSecLevel), Ord(SL)));

    { Restore persistent data from INI file }
        IniFile      := TIcsIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        DocDirEdit.Text     := IniFile.ReadString(SectionData, KeyDocDir,
                                                  '..\Internet\WebServData\WwwRoot');
        DefaultDocEdit.Text := IniFile.ReadString(SectionData, KeyDefaultDoc,
                                                  'index.html');
        PortHttpsEdit.Text  := IniFile.ReadString(SectionData, KeyPortHttps,
                                                  '443');
        PortHttpEdit.Text   := IniFile.ReadString(SectionData, KeyPortHttp,
                                                  '80');
        DisplayHeaderCheckBox.Checked :=
                Boolean(IniFile.ReadInteger(SectionData, KeyDisplayHeader, 0));
        WriteLogFileCheckBox.Checked :=
                Boolean(IniFile.ReadInteger(SectionData, KeyLogToFile, 0));
        DisplaySslInfoCheckBox.Checked :=
                Boolean(IniFile.ReadInteger(SectionData, KeyDisplaySslInfo, 0));
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
 //       DHParamFileEdit.Text := IniFile.ReadString(SectionData, KeyDHFile, ''); { V8.02 }
        AcceptableHostsEdit.Text := IniFile.ReadString(SectionData, KeyAcceptableHosts,
                                                       'www.overbyte.be;www.borland.com');
        VerifyPeerCheckBox.Checked := Boolean(IniFile.ReadInteger(SectionData,
                                                                  KeyVerifyPeer,
                                                                  0));
        FRenegotiationInterval := IniFile.ReadInteger(SectionData,
                                                      KeyRenegInterval, 0);
//        ECDHList.ItemIndex := IniFile.ReadInteger(SectionData, KeyECDHList, 1);   { V8.02 }
        SslMinVersion.ItemIndex := IniFile.ReadInteger(SectionData,               { V8.07 }
                                                       KeySslMinVersion,
                                                       0);
        SslMaxVersion.ItemIndex := IniFile.ReadInteger(SectionData,               { V8.07 }
                                                       KeySslMaxVersion,
                                                       5);
        SslCipherList.ItemIndex := IniFile.ReadInteger(SectionData, KeySslCipherList, 0);    { V8.05 }
        SslCipherEdit.Text := IniFile.ReadString(SectionData, KeySslCipherEdit, '');         { V8.05 }
        OldIp := IniFile.ReadString(SectionData, KeyListenAddr, '');                         { V8.05 }
        OldSslCheckBox.Checked  := IniFile.ReadBool(SectionData, KeyOldSsl, False);          { V8.07 }
        DebugEventCheckBox.Checked := IniFile.ReadBool(SectionData, KeyDebugEvent, False);   { V8.07 }
        SslSecLevel.ItemIndex := IniFile.ReadInteger(SectionData, KeySslSecLevel, 1);        { V8.41 }
        WellKnownPathEdit.Text := IniFile.ReadString(SectionData, KeyWellKnownPath, '');     { V8.49 }
        OcspCheckBox.Checked := IniFile.ReadBool(SectionData, KeyOcspCheckBox, True);        { V8.69 }
        IniFile.Free;

        RenegotiationIntervalEdit.Text := IntToStr(FRenegotiationInterval);

      { V8.05 allow user to choose which IP address to listen }
        InterfaceList := TStringList.Create;
        InterfaceList.Sorted := True;  { V8.52 sort the list }
        try
       { V8.52 get local IP list from newer cross platform function }
            IcsGetInterfaceList(InterfaceList);
         //   InterfaceList.AddStrings(LocalIPList(sfAny));   { V8.52 show IPv6 as well }
            ListenAddr.Items.Text := 'localhost' + #13#10 +
                                '0.0.0.0' + #13#10 + InterfaceList.Text;
        finally
            InterfaceList.Free;
        end;
        I := ListenAddr.Items.IndexOf(OldIp);
        if I >= 0 then
            ListenAddr.ItemIndex := I
        else
            ListenAddr.ItemIndex := 0;

        { Start log file }
        if WriteLogFileCheckBox.Checked then begin
            OpenLogFile;
            WriteLogFileCheckBox.Checked := FLogFileOpened;
        end;
        { Initialize client count caption }
        ClientHttpsCountLabel.Caption := '0';
        ClientHttpCountLabel.Caption  := '0';
        { Display version info for program and used components }
        wsi := WinsockInfo;
        DisplayMemo.Clear;
        Display(CopyRight);
        Display('Using:');
        Display('   ' + OverbyteIcsWSocket.CopyRight);
        Display('   ' + OverbyteIcsWSocketS.CopyRight);
        Display('   ' + OverbyteIcsHttpSrv.CopyRight);
        Display('    Winsock:');
        Display('        Version ' +
                Format('%d.%d', [WinsockInfo.wHighVersion shr 8,
                                 WinsockInfo.wHighVersion and 15]));
        Display('        ' + StrPas(wsi.szDescription));
        Display('        ' + StrPas(wsi.szSystemStatus));
{$IFNDEF VER100}
        { A bug in Delphi 3 makes lpVendorInfo invalid }
        if wsi.lpVendorInfo <> nil then
            Display('        ' + StrPas(wsi.lpVendorInfo));
{$ENDIF}

    { V8.07 load OpenSSL, then display OpenSSL DLL name and version  }
        GSSLEAY_DLL_IgnoreNew := OldSslCheckBox.Checked;  { V8.66 ignore OpenSSL 3.0 and later }
     //   SslStaticLock1.Enabled := true ;  { V8.66 gone with 1.0.2, note also loaded OpenSSL }
        LoadSsl;                            { V8.66 need version number }
        if NOT FileExists (GLIBEAY_DLL_FileName) then
            DisplayMemo.Lines.Add('SSL/TLS DLL not found: ' + GLIBEAY_DLL_FileName)
        else
            DisplayMemo.Lines.Add('SSL/TLS DLL: ' + GLIBEAY_DLL_FileName +
                                                ', Version: ' + OpenSslVersion);

        { Automatically start server }
        StartHttpsButtonClick(Self);
        StartHttpButtonClick(Self);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
     StopButtonClick(Self);   { V8.64 }
    { Save persistent data to INI file }
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.WriteString(SectionData,    KeyDocDir,      SslHttpServer1.DocDir);
    IniFile.WriteString(SectionData,    KeyDefaultDoc,  SslHttpServer1.DefaultDoc);
    IniFile.WriteString(SectionData,    KeyPortHttps,   SslHttpServer1.Port);
    IniFile.WriteString(SectionData,    KeyPortHttp,    HttpServer2.Port);
    IniFile.WriteInteger(SectionData,   KeyDisplayHeader,
                                        ord(DisplayHeaderCheckBox.Checked));
    IniFile.WriteInteger(SectionData,   KeyLogToFile,
                                        ord(WriteLogFileCheckBox.Checked));
    IniFile.WriteInteger(SectionData,   KeyDisplaySslInfo,
                                        ord(DisplaySslInfoCheckBox.Checked));
    IniFile.WriteString(SectionData,    KeyCertFile,    CertFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyPrivKeyFile, PrivKeyFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyPassPhrase,  PassPhraseEdit.Text);
    IniFile.WriteString(SectionData,    KeyCAFile,      CAFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyCAPath,      CAPathEdit.Text);
//    IniFile.WriteString(SectionData,    KeyDHFile,      DhParamFileEdit.Text);        { V8.01 }
    IniFile.WriteString(SectionData,    KeyAcceptableHosts, AcceptableHostsEdit.Text);
    IniFile.WriteInteger(SectionData,   KeyVerifyPeer,  Ord(VerifyPeerCheckBox.Checked));
    IniFile.WriteInteger(SectionData,   KeyRenegInterval, FRenegotiationInterval);
//    IniFile.WriteInteger(SectionData,   KeyECDHList,  ECDHList.ItemIndex);        { V8.01 }
    IniFile.WriteInteger(SectionData,   KeySslMinVersion,   SslMinVersion.ItemIndex);  { V8.07 }
    IniFile.WriteInteger(SectionData,   KeySslMaxVersion,   SslMaxVersion.ItemIndex);  { V8.07 }
    IniFile.WriteInteger(SectionData,   KeySslCipherList, SslCipherList.ItemIndex);    { V8.05 }
    IniFile.WriteString(SectionData,    KeySslCipherEdit, SslCipherEdit.Text);         { V8.05 }
    IniFile.WriteString(SectionData,    KeyListenAddr, ListenAddr.Items [ListenAddr.ItemIndex]); { V8.05 }
    IniFile.WriteBool(SectionData,      KeyOldSsl,      OldSslCheckBox.Checked);       { V8.07 }
    IniFile.WriteBool(SectionData,      KeyDebugEvent,  DebugEventCheckBox.Checked);   { V8.07 }
    IniFile.WriteInteger(SectionData,   KeySslSecLevel, SslSecLevel.ItemIndex);        { V8.41 }
    IniFile.WriteString(SectionData,    KeyWellKnownPath, WellKnownPathEdit.Text);     { V8.49 }
    IniFile.WriteBool(SectionData,      KeyOcspCheckBox,  OcspCheckBox.Checked);       { V8.69 }
    IniFile.UpdateFile;
    IniFile.Free;
    CloseLogFile;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Display a message in display memo box, making sure we don't overflow it.  }
procedure TSslWebServForm.Display(Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        { We preserve only 5000 lines }
        while DisplayMemo.Lines.Count > 5000 do
            DisplayMemo.Lines.Delete(0);
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        { Makes last line visible }
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
    if FLogFileOpened then begin
        try
            WriteLn(FLogFile, Msg);
        except
            on E:Exception do begin
                DisplayMemo.Lines.Add('*** Exception' +
                                      E.CLassName + ': ' + E.Message +
                                      ' writing to log file ***');
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when user clicks on start button. It is also }
{ called from FormShow event handler, at program startup. It starts server. }
{ We need to pass default document, document directory and client class     }
{ to HTTP server component. Client class is very usefull because it         }
{ instruct server component to instanciate our own client class instead of  }
{ defualt client class. Using our own client class will enables you to add  }
{ any data we need to handle our application. This data is private for each }
{ client.                                                                   }
{ When server is started, we will get OnServerStarted event triggered.      }
procedure TSslWebServForm.StartHttpsButtonClick(Sender: TObject);
var
    CAList: TX509List;       { V8.64 }
    ErrStr: string;
    valres: TChainResult;
//    FDir, FName: String;
    Inters: TX509List;

    function AddTls13(const Ciphers: String): String; { V8.52 }
    begin
        if ICS_OPENSSL_VERSION_NUMBER < OSSL_VER_1101 then
            result := Ciphers
        else
            result := sslCipherTLS13 + Ciphers;
    end;

begin
    IcsLogger1.LogOptions := [];
    if DebugEventCheckBox.Checked then
        IcsLogger1.LogOptions := IcsLogger1.LogOptions + [loDestEvent];
    if IcsLogger1.LogOptions <> [] then
        IcsLogger1.LogOptions := IcsLogger1.LogOptions +
                                 LogAllOptInfo + [loAddStamp];
//  IcsLogger1.LogOptions := IcsLogger1.LogOptions + LogAllOptDump ; { SSL devel dump }
    SslHttpServer1.DocDir           := Trim(DocDirEdit.Text);
    SslHttpServer1.WellKnownPath    := Trim(WellKnownPathEdit.Text);      { V8.49 }
    SslHttpServer1.DefaultDoc       := Trim(DefaultDocEdit.Text);
    SslHttpServer1.Port             := Trim(PortHttpsEdit.Text);
    SslHttpServer1.Addr             := ListenAddr.Items [ListenAddr.ItemIndex];  { V8.05 }
    SslHttpServer1.ClientClass      := TMyHttpConnection;
    SslHttpServer1.SetAcceptableHostsList(AcceptableHostsEdit.Text);
    FSrvSslCert := '';

  { V8.41 new way, supports PEM, DER, PKCS12, PKCS8 formats and check them earlier }
    SslContext1.SslCertX509.LoadFromFile(CertFileEdit.Text, croTry, croTry,
                                                          PassPhraseEdit.Text);  { try and load pkey and inter certs }
    if NOT SslContext1.SslCertX509.IsPkeyLoaded then
            SslContext1.SslCertX509.PrivateKeyLoadFromPemFile(PrivKeyFileEdit.Text);
    if NOT SslContext1.SslCertX509.IsInterLoaded then
            SslContext1.SslCertX509.LoadIntersFromPemFile(CAFileEdit.Text);

  { V8.41 check certificate chain for errors }
    CAList := TX509List.Create(Self);      { V8.64 }
    CAList.LoadAllFromString(sslRootCACertsBundle);  { V8.64 trusted root so we check chain }
    valres := SslContext1.SslCertX509.ValidateCertChain('', CAList, FSrvSslCert, ErrStr);  { really need host name  }
    if ErrStr <> '' then
       ErrStr := ', ' + ErrStr;

  { V8.69 check OCSP server if revoked and keep OCSP response for status stapling }
    if (valres <> chainFail) and OcspCheckBox.Checked then begin
        if (SslContext1.SslCertX509.UrlOcsp <> '') then begin
            Display('OCSP Check Starting');
            Inters := TX509List.Create(self);
            try
                CAList.SetX509Store;
                Inters.X509Class := TX509Base;
                Inters.LoadAllStack(SslContext1.SslCertX509.X509Inters);
                OcspSrvHttp.ClearOcsp;
                OcspSrvHttp.OcspCert := SslContext1.SslCertX509;
                OcspSrvHttp.OcspInters := Inters;
             // must block and wait for OSCP response
                if OcspSrvHttp.CheckOcspRevoked(CAList, 3) then begin   // three seconds wait
                    valres := chainFail;
                end;
                if OcspSrvHttp.OcspRespStatus <> OCSP_RESPONSE_STATUS_SUCCESSFUL then begin
                    ErrStr := ErrStr + ', OCSP Check Failed: ' + OcspSrvHttp.OcspInters.OcspRespStatusStr;
                end
                else if OcspSrvHttp.OcspCertStatus = V_OCSP_CERTSTATUS_GOOD then
                    ErrStr := ErrStr + ', OCSP Check OK Not Revoked'
                else begin
                    ErrStr := ErrStr + ', OCSP Check Failed: ' + OcspSrvHttp.OcspInters.OcspCertStatusStr;
                end;
                Display(OcspSrvHttp.OcspLastResp);

           // keep response in Hosts, sent when checking SNI to each connection
                FOcspStapleResp := OcspSrvHttp.OcspRespRaw;
            finally
                OcspSrvHttp.OcspInters := Nil;
                Inters.Free;
            end;
       end
       else
            Display('OCSP Check Skipped, No OCSP URL in Certificate');
    end;

    if valres = chainOK then
        ErrStr := 'Chain Validated OK' + ErrStr
    else if valres = chainWarn then
        ErrStr := 'Chain Warning' + ErrStr
    else
        ErrStr := 'Chain Failed' + ErrStr;
    Display(FSrvSslCert + #13#10 + ErrStr + #13#10);
    CAList.Destroy;
    if valres = chainFail then Exit;

//    SslContext1.SslDHParamFile      := DhParamFileEdit.Text;      { V8.02, gone V8.66 }
    SslContext1.SslVerifyPeer       := VerifyPeerCheckBox.Checked;
//    SslContext1.SslECDHMethod       := TSslECDHMethod(ECDHList.ItemIndex); { V8.02, gone V8.66 }
    SslContext1.SslMinVersion       := TSslVerMethod (SslMinVersion.ItemIndex);  { V8.07}
    SslContext1.SslMaxVersion       := TSslVerMethod (SslMaxVersion.ItemIndex);  { V8.07}
    case SslCipherList.ItemIndex of   { V8.05 choice of ciphers }
        0: SslContext1.SslCipherList := sslCiphersServer;
        1: SslContext1.SslCipherList := AddTls13(sslCiphersMozillaSrvBack);
        2: SslContext1.SslCipherList := AddTls13(sslCiphersMozillaSrvInter);
        3: SslContext1.SslCipherList := AddTls13(sslCiphersMozillaSrvInterFS); { V8.41 }
        4: SslContext1.SslCipherList := AddTls13(sslCiphersMozillaSrvHigh);
    end;
   if SslCipherEdit.Text <> '' then  SslContext1.SslCipherList := SslCipherEdit.Text; { V8.05 }
   SslContext1.SslSecLevel := TSslSecLevel (SslSecLevel.ItemIndex);   { V8.41 }

 { V8.02 single DH needed for perfect forward secrecy }
    SslContext1.SslOptions2 := SslContext1.SslOptions2 +
          [sslOpt2_CIPHER_SERVER_PREFERENCE {, sslOpt_SINGLE_DH_USE, SslOpt_SINGLE_ECDH_USE}] ;
    try
        if SslContext1.IsCtxInitialized then   { V8.05 }
        begin
          SslContext1.DeInitContext;
          SslHttpServer1.WSocketServer.ResetSSL;
        end;
        SslContext1.InitContext;  { V8.02 get any error now before starting server }

        FSrvSslCert := StringReplace (FSrvSslCert, #13#10, '<BR>'+#13#10, [rfReplaceAll]) ;

      { V8.07  list SSL ciphers }
        FSrvCipherList := SslContext1.SslGetAllCiphers;
        FSrvCipherList := StringReplace(FSrvCipherList, #13#10, ', ', [rfReplaceAll]);
        Display('SSL Ciphers Accepted: ' + #13#10 + FSrvCipherList + #13#10);
     except
        on E:Exception do begin
            Display('Failed to initialize SSL Context: ' + E.Message);
            Exit;
        end;
    end;
    SslHttpServer1.Start;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.StartHttpButtonClick(Sender: TObject);
begin
    // Just a little quick test to support also HTTP without SSL
    HttpServer2.DocDir         := Trim(DocDirEdit.Text);
    HttpServer2.WellKnownPath  := Trim(WellKnownPathEdit.Text);  { V8.49 }
    HttpServer2.DefaultDoc     := Trim(DefaultDocEdit.Text);
    HttpServer2.Port           := Trim(PortHttpEdit.Text);
    HttpServer2.Addr           := ListenAddr.Items [ListenAddr.ItemIndex];  { V8.05 }
    HttpServer2.ClientClass    := TMyHttpConnection;
    HttpServer2.Start;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when user clicks on stop button. We just  }
{ stop the server. We will get OnServerStopped event triggered.             }
procedure TSslWebServForm.StopButtonClick(Sender: TObject);
begin
    SslHttpServer1.Stop;
    HttpServer2.Stop;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when user clicks on clear buttoN; We just }
{ clear the memo used for displaying activity.                              }
procedure TSslWebServForm.ClearButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when HTTP server is started, that is when }
{ server socket has started listening.                                      }
procedure TSslWebServForm.SslHttpServer1ServerStarted(Sender: TObject);
var
  S: string ;
begin
    DocDirEdit.Enabled       := FALSE;
    DefaultDocEdit.Enabled   := FALSE;
    PortHttpsEdit.Enabled    := FALSE;
    PortHttpEdit.Enabled     := FALSE;
    StartHttpsButton.Enabled := FALSE;
    StopButton.Enabled       := TRUE;
    Display('HTTPS Server is waiting for connections');
    S := IcsFmtIpv6Addr(SslHttpServer1.Addr);  { V8.52 browser friendly IPv6 }
    if S = '0.0.0.0' then S:= 'localhost';
    Display('https://' + S + '/demo.html');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.HttpServer2ServerStarted(Sender: TObject);
var
  S: string ;
begin
    StartHttpButton.Enabled    := FALSE;
    Display('HTTP Server is waiting for connections');
    S := IcsFmtIpv6Addr(SslHttpServer1.Addr); { V8.52 browser friendly IPv6 }
    if S = '0.0.0.0' then S:= 'localhost';
    Display('http://' + S + '/demo.html');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when server has been stopped, that is     }
{ when server socket stop listening.                                        }
procedure TSslWebServForm.SslHttpServer1ServerStopped(Sender: TObject);
begin
    DocDirEdit.Enabled       := TRUE;
    DefaultDocEdit.Enabled   := TRUE;
    PortHttpsEdit.Enabled    := TRUE;
    StartHttpsButton.Enabled := TRUE;
    //StopButton.Enabled     := FALSE;
    Display('HTTPS Server stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.HttpServer2ServerStopped(Sender: TObject);
begin
    PortHttpEdit.Enabled    := TRUE;
    StartHttpButton.Enabled := TRUE;
    Display('HTTP Server stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when a client specifies the /.well-known/ path }
procedure TSslWebServForm.HttpServer2WellKnownDir(Sender, Client: TObject;     { V8.49 }
  const Path: string; var BodyStr: string);
begin
   Display('HTTP Server: Well-Known File Requested: ' + Path);
   if Pos('/acme-challenge/', Path) > 1 then begin
     // check challenge token received Let's Encrypt and return key authorization
     // sample only !!!
        if Pos('/LoqXcYV8q5ONbJQxbmR7SCTNo3tiAXDfowyjxAjEuX0', Path) > 1 then  begin
            BodyStr := 'LoqXcYV8q5ONbJQxbmR7SCTNo3tiAXDfowyjxAjEuX0' +
                                            '.9jg46WB3rR_AHD-EBXdN7cBkH1WOu0tA3M9fm21mqTI';
           Display('HTTP Server: acme-challenge response: ' + BodyStr);
        end;
   end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when a client specifies the /.well-known/ path }
procedure TSslWebServForm.SslHttpServer1WellKnownDir(Sender, Client: TObject;   { V8.49 }
  const Path: string; var BodyStr: string);
begin
   Display('HTTPS Server: Well-Known File Requested: ' + Path);

 { !!! note, acme challenges use HTTP only since they precede the ussuing
       of an SSL certificate, but other services might use SSL }
   if Pos('/acme-challenge/', Path) > 1 then begin
     // check challenge token received Let's Encrypt and return key authorization
     // sample only !!!
        if Pos('/LoqXcYV8q5ONbJQxbmR7SCTNo3tiAXDfowyjxAjEuX0', Path) > 1 then  begin
            BodyStr := 'LoqXcYV8q5ONbJQxbmR7SCTNo3tiAXDfowyjxAjEuX0' +
                                            '.9jg46WB3rR_AHD-EBXdN7cBkH1WOu0tA3M9fm21mqTI';
           Display('HTTPS Server: acme-challenge response: ' + BodyStr);
        end;
   end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.IcsLogger1IcsLogEvent(Sender: TObject;
  LogOption: TLogOption; const Msg: string);
begin
    Display(Msg);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when a new client has connected.          }
procedure TSslWebServForm.SslHttpServer1ClientConnect(
    Sender : TObject;               { HTTP server component                 }
    Client : TObject;               { Client connecting                     }
    Error  : Word);                 { Error in connection                   }
begin
    ClientHttpsCountLabel.Caption :=
        IntToStr((Sender as THttpServer).ClientCount);
    TMyHttpConnection(Client).OnBgException := BackgroundException;
//    TMyHttpConnection(Client).OnSslServerName := ClientSslServerName;  { V8.02 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.SslHttpServer1SslServerName(      { V8.02 }
  Sender      : TObject;
  var Ctx     : TSslContext;
  var ErrCode : TTlsExtError); // Optional error code
var
    Cli : TSslWSocketClient;
//    CipherList: String;
begin
   { V8.06 tell SSL whether server can handle SslServerName }
    ErrCode := teeOk;              { accept SSL connection }
  //  ErrCode := teeAlertWarning;  { old default, stopped Java clients connecting }
  //  ErrCode := teeAlertFatal;    { reject SSL connection }

    Cli := TSslWSocketClient(Sender);
    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
            Cli.GetPeerAddr + '] SNI "' + Cli.SslServerName +'" received');

 // list SMI, ALPN, versioon, extensions and stuff from Cli.CliHelloData
    Display('Client Hello: ' + WSocketGetCliHelloStr(Cli.CliHelloData));

  { does not work yet - API always returns empty stacks }
//    CipherList := Cli.SslBytesToCiphers(Cli.CliHelloData.CipherSuites);
//    Display('Client Ciphers: ' + StringReplace(CipherList, #13#10, ', ', [rfReplaceAll]));

    { Provide a SslContext that corresponds to the server name received }
    { this allows different hosts and certificates on the same IP address }
    { note this is better done using IcsHosts in SslSocketServer }

 {   if FComputerName = Cli.SslServerName then begin
        if not SslContext2.IsCtxInitialized then
            SslContext2.InitContext;
        Ctx := SslContext2;
        DisplayMemo.Lines.Add('! Switching context to SslContext2');
    end
    else
        DisplayMemo.Lines.Add('! Unknown server name "' + Cli.SslServerName +
                              '" received. Context switch denied');   }

{ V8.69 set OCSP staple response }
     Cli.OcspStapleRaw := FOcspStapleResp;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.SslHttpServer1SslAlpnSelect(Sender: TObject;
  ProtoList: TStrings; var SelProto: string; var ErrCode: TTlsExtError);  { V8.56 }
var
    I: Integer;
begin
    if ProtoList.Count = 0 then Exit;
  // optionally select a protocol we want to use
    for I := 0 to ProtoList.Count - 1 do begin
        if ProtoList[I] = ALPN_ID_HTTP11 then begin
            SelProto := ALPN_ID_HTTP11;
        //    SelProto := ALPN_ID_HTTP2; // TEMP confuse them
            ErrCode := teeOk;
            Exit;
        end;
   //     if ProtoList[I] = ALPN_ID_HTTP2 then begin  don't support HTTP/2 yet

    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when a client is disconnecting, just      }
{ before client component is closed.                                        }
procedure TSslWebServForm.SslHttpServer1ClientDisconnect(
    Sender : TObject;               { HTTP server component                 }
    Client : TObject;               { Client connecting                     }
    Error  : Word);                 { Error in disconnection                }
begin
    ClientHttpsCountLabel.Caption :=
        IntToStr((Sender as THttpServer).ClientCount - 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.HttpServer2ClientConnect(
    Sender : TObject;               { HTTP server component                 }
    Client : TObject;               { Client connecting                     }
    Error  : Word);                 { Error in connection                   }
begin
    ClientHttpCountLabel.Caption :=
        IntToStr((Sender as THttpServer).ClientCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.HttpServer2ClientDisconnect(
    Sender : TObject;               { HTTP server component                 }
    Client : TObject;               { Client connecting                     }
    Error  : Word);                 { Error in disconnection                }
begin
    ClientHttpCountLabel.Caption :=
        IntToStr((Sender as THttpServer).ClientCount - 1);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when HTTP server component receive a HEAD }
{ command from any client.                                                  }
{ We just count the request, display a message and let HTTP server          }
{ component handle everything.                                              }
{ We should trap every URI we handle internally...                          }
procedure TSslWebServForm.SslHttpServer1HeadDocument(
    Sender    : TObject;            { HTTP server component                 }
    Client    : TObject;            { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
begin
    Inc(FCountRequests);
    Display(IntToStr(FCountRequests) +
            ': HEAD ' + TMyHttpConnection(Client).Path);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when HTTP server component receive a GET  }
{ command from any client.                                                  }
{ We count the request, display a message and trap '/time.htm' path for     }
{ special handling.                                                         }
{ There is no document time.htm on disk, we will create it on the fly. With }
{ a classic webserver we would have used a CGI or ISAPI/NSAPI to achieve    }
{ the same goal. It is much easier here since we can use Delphi code        }
{ directly to generate whatever we wants. Here for the demo we generate a   }
{ page with server data and time displayed.                                 }
procedure TSslWebServForm.SslHttpServer1GetDocument(
    Sender    : TObject;            { HTTP server component                 }
    Client    : TObject;            { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
begin
    { Count request and display a message }
    Inc(FCountRequests);
    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
            TWSocket(Client).GetPeerAddr + '] ' + IntToStr(FCountRequests) +
            ': GET ' + TMyHttpConnection(Client).Path);
    DisplayHeader(TMyHttpConnection(Client));

    { Trap '/time.htm' path to dynamically generate an answer. }
    if (CompareText(THttpConnection(Client).Path, '/demo.html') = 0) then
        CreateVirtualDocument_Demo(Sender, Client, Flags)
    else if CompareText(THttpConnection(Client).Path, '/time.html') = 0 then
        CreateVirtualDocument_time_htm(Sender, Client, Flags)
    { Trap '/redir.htm' to dynamically generate a redirection answer }
    else if CompareText(THttpConnection(Client).Path, '/redir.html') = 0 then
        CreateVirtualDocument_redir_htm(Sender, Client, Flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /redir.htm document                     }
procedure TSslWebServForm.CreateVirtualDocument_redir_htm(    // redir.html
    Sender    : TObject;            { HTTP server component                 }
    Client    : TObject;            { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
var
    Body     : String;
    Header   : String;
    Stream   : TMemoryStream;
    Location : String;
begin
    Location := (Client as THttpConnection).Params;
    if Location = '' then
        Location := '/time.html';

    { Let HTTP server component know we will send data to client }
    Flags  := hgWillSendMySelf;
    { Create a stream to hold data sent to client that is the answer }
    { made of a HTTP header and a body made of HTML code.            }
    Stream := TMemoryStream.Create;
    Body   := '<HTML>' +
                '<HEAD>' +
                  '<TITLE>ICS WebServer Demo - Redir</TITLE>' +
                '</HEAD>' + #13#10 +
                '<BODY>' +
                  'You should be redirected automatically !<BR>' + #13#10 +
                  '<A HREF="' + Location + '">Click Here</A><BR>' + #13#10 +
                '</BODY>' +
              '</HTML>' + #13#10;
    Header := TMyHttpConnection(Client).Version + ' 302 OK' + #13#10 +
              'Content-Type: text/html' + #13#10 +
              'Location: ' + Location + #13#10 +
              'Content-Length: ' +
              IntToStr(Length(Body)) + #13#10 +
              #13#10;
    //Stream.Write(Header[1], Length(Header));
    StreamWriteStrA(Stream, Header);
    //Stream.Write(Body[1],   Length(Body));
    StreamWriteStrA(Stream, Body);
    { We need to seek to start of stream ! }
    Stream.Seek(0, 0);
    { We ask server component to send the stream for us. }
    TMyHttpConnection(Client).DocStream := Stream;
    TMyHttpConnection(Client).SendStream;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.CreateVirtualDocument_Demo(     // demo.html
    Sender    : TObject;
    Client    : TObject;
    var Flags : THttpGetFlag);
var
    Body   : String;
begin
    Body := '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS-SSL WebServer Demo - Menu</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS-SSL WebServer Demo Menu</H2>' + #13#10 +
            '<H3>' + TMyHttpConnection(Client).SslHandshakeRespMsg  + '</H3>' + #13#10 + { V8.02 }
            '<A HREF="/time.html">Server time</A><BR>'  + #13#10 +
            '<A HREF="/redir.html">Redirection</A><BR>' + #13#10 +
            '<A HREF="/">Default document</A><BR>' + #13#10 +
            '<A HREF="http://www.overbyte.be">ICS Home page</A><P>' + #13#10 +
            '<P>OpenSSL Version: ' + OpenSslVersion + '<BR>' + #13#10#13#10 +
             'Server SSL Certificates: <BR>' + #13#10 +
             FSrvSslCert + '<BR><BR>' + #13#10 +
             'OpenSSL Server Ciphers: ' + FSrvCipherList + '<P>' + #13#10 +
             'SSL Ciphers from Client: ' + FCliCipherList + '<P>' + #13#10 +
             'Note: You can find a better demo in the non-SSL ICS.<P>' + #13#10 +
          '</BODY>' + #13#10 +
        '</HTML>' + #13#10;
    TMyHttpConnection(Client).AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        Body);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /time.htm document                      }
procedure TSslWebServForm.CreateVirtualDocument_time_htm(
    Sender    : TObject;            { HTTP server component                 }
    Client    : TObject;            { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
var
    Body   : String;
    Header : String;
    Stream : TMemoryStream;
begin
    { Let HTTP server component know we will send data to client }
    Flags  := hgWillSendMySelf;
    { Create a stream to hold data sent to client that is the answer }
    { made of a HTTP header and a body made of HTML code.            }
    Stream := TMemoryStream.Create;
    Body   := '<HTML>' +
                '<HEAD>' +
                  '<TITLE>ICS WebServer Demo</TITLE>' +
                '</HEAD>' + #13#10 +
                '<BODY>' +
                  '<H2>Time at server side:</H2>' + #13#10 +
                  '<P>' + DateTimeToStr(Now) +'</P>' + #13#10 +
                '</BODY>' +
              '</HTML>' + #13#10;
    Header := TMyHttpConnection(Client).Version + ' 200 OK' + #13#10 +
              'Content-Type: text/html' + #13#10 +
              'Content-Length: ' +
              IntToStr(Length(Body)) + #13#10 +
              #13#10;
    //Stream.Write(Header[1], Length(Header));
    StreamWriteStrA(Stream, Header);
    //Stream.Write(Body[1],   Length(Body));
    StreamWriteStrA(Stream, Body);
    { We need to seek to start of stream ! }
    Stream.Seek(0, 0);
    { We ask server component to send the stream for us. }
    TMyHttpConnection(Client).DocStream := Stream;
    TMyHttpConnection(Client).SendStream;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when HTTP server component receive a POST }
{ command from any client.                                                  }
{ We count the request, display a message and trap posted data.             }
{ To check for posted data, you may construct the following HTML document:  }
{ <HTML>                                                                    }
{   <HEAD>                                                                  }
{     <TITLE>Test Form 1</TITLE>                                            }
{   </HEAD>                                                                 }
{   <BODY>                                                                  }
{     <H2>Enter your first and last name</H2>                               }
{     <FORM METHOD="POST" ACTION="/cgi-bin/cgifrm1.exe">                    }
{       <TABLE BORDER="0" ALIGN="DEFAULT" WIDTH="100%">                     }
{         <TR>                                                              }
{           <TD>First name</TD>                                             }
{           <TD><INPUT TYPE="TEXT" NAME="FirstName"                         }
{                      MAXLENGTH="25" VALUE="YourFirstName"></TD>           }
{         </TR>                                                             }
{         <TR>                                                              }
{           <TD>Last name</TD>                                              }
{           <TD><INPUT TYPE="TEXT" NAME="LastName"                          }
{                      MAXLENGTH="25" VALUE="YourLastName"></TD>            }
{         </TR>                                                             }
{       </TABLE>                                                            }
{       <P><INPUT TYPE="SUBMIT" NAME="Submit" VALUE="Button"></P>           }
{     </FORM>                                                               }
{   </BODY>                                                                 }
{ </HTML>                                                                   }
procedure TSslWebServForm.SslHttpServer1PostDocument(
    Sender    : TObject;            { HTTP server component                 }
    Client    : TObject;            { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
var
    Remote  : TMyHttpConnection;
begin
    { It's easyer to do the cast one time. Could use with clause... }
    Remote := TMyHttpConnection(Client);

    { Count request and display a message }
    Inc(FCountRequests);
    Display(IntToStr(FCountRequests) + ': POST ' + Remote.Path);
    DisplayHeader(Remote);

    { Check for request past. We only accept data for '/cgi-bin/cgifrm1.exe' }
    if CompareText(Remote.Path, '/cgi-bin/cgifrm1.exe') = 0 then begin
        { Tell HTTP server that we will accept posted data        }
        { OnPostedData event will be triggered when data comes in }
        Flags := hgAcceptData;
        { We wants to receive any data type. So we turn line mode off on   }
        { client connection.                                               }
        Remote.LineMode := FALSE;
        { We need a buffer to hold posted data. We allocate as much as the }
        { size of posted data plus one byte for terminating nul char.      }
        { We should check for ContentLength = 0 and handle that case...    }
        ReallocMem(Remote.FPostedRawData, Remote.RequestContentLength + 1);
        { Clear received length }
        Remote.FDataLen := 0;
    end
    else
        Flags := hg404;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered for each data packet posted by client     }
{ when we told HTTP server component that we will accept posted data.       }
{ We have to receive ALL data which is sent by remote client, even if there }
{ is more than what ContentLength tells us !                                }
{ If ContentLength = 0, then we should receive data until connection is     }
{ closed...                                                                 }
procedure TSslWebServForm.SslHttpServer1PostedData(
    Sender : TObject;               { HTTP server component                 }
    Client : TObject;               { Client posting data                   }
    Error  : Word);                 { Error in data receiving               }
var
    Len     : Integer;
    Remains : Integer;
    Junk    : array [0..255] of AnsiChar;
    Remote  : TMyHttpConnection;
begin
    { It's easyer to do the cast one time. Could use with clause... }
    Remote := TMyHttpConnection(Client);

    { How much data do we have to receive ? }
    Remains := Remote.RequestContentLength - Remote.FDataLen;
    if Remains <= 0 then begin
        { We got all our data. Junk anything else ! }
        Len := Remote.Receive(@Junk, SizeOf(Junk) - 1);
        if Len >= 0 then
            Junk[Len] := #0;
        Exit;
    end;
    { Receive as much data as we need to receive. But warning: we may       }
    { receive much less data. Data will be split into several packets we    }
    { have to assemble in our buffer.                                       }
    Len := Remote.Receive(Remote.FPostedRawData + Remote.FDataLen, Remains);
    { Sometimes, winsock doesn't wants to givve any data... }
    if Len <= 0 then
        Exit;

    { Add received length to our count }
    Inc(Remote.FDataLen, Len);
    { Add a nul terminating byte (handy to handle data as a string) }
    Remote.FPostedRawData[Remote.FDataLen] := #0;     { V8.04 was wrong variable
    { Display receive data so far }
    Display('Data: ''' + StrPas(Remote.FPostedRawData) + '''');

    { When we received the whole thing, we can process it }
    if Remote.FDataLen = Remote.RequestContentLength then begin
{$IF CompilerVersion > 19}
        Remote.FPostedDataBuffer := Pointer(UnicodeString(Remote.FPostedRawData)); // Cast to Unicode
{$ELSE}
        Remote.FPostedDataBuffer := Remote.FPostedRawData;
{$IFEND}
        { First we must tell the component that we've got all the data }
        Remote.PostedDataReceived;
        { Then we check if the request is one we handle }
        if CompareText(Remote.Path, '/cgi-bin/cgifrm1.exe') = 0 then
            { We are happy to handle this one }
            ProcessPostedData_CgiFrm1(Remote)
        else
            { We don't accept any other request }
            Remote.Answer404;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will process posted data for CgiFrm1.exe                             }
procedure TSslWebServForm.ProcessPostedData_CgiFrm1(Client : TMyHttpConnection);
var
    Stream    : TStream;
    FileName  : String;
    Body      : String;
    Header    : String;
    FirstName : String;
    LastName  : String;
    HostName  : String;
    Buf       : String;
begin
    { Extract fields from posted data. }
    ExtractURLEncodedValue(Client.FPostedDataBuffer, 'FirstName', FirstName);
    ExtractURLEncodedValue(Client.FPostedDataBuffer, 'LastName',  LastName);
    { Get client IP address. We could to ReverseDnsLookup to get hostname }
    HostName := Client.PeerAddr;
    { Build the record to write to data file }
    Buf      := FormatDateTime('YYYYMMDD HHNNSS ', Now) +
                FirstName + '.' + LastName + '@' + HostName + #13#10;

    { Save data to a text file }
    FileName := ExtractFilePath(Application.ExeName) + 'CgiFrm1.txt';
    if FileExists(FileName) then
        Stream := TFileStream.Create(FileName, fmOpenWrite)
    else
        Stream := TFileStream.Create(FileName, fmCreate);
    Stream.Seek(0, soFromEnd);
    StreamWriteStrA(Stream, Buf);
    Stream.Destroy;

    { Now create output stream to send back to remote client }
    Stream := TMemoryStream.Create;
    Body   := '<HTML>' +
                '<HEAD>' +
                  '<TITLE>ICS WebServer Demo</TITLE>' +
                '</HEAD>' + #13#10 +
                '<BODY>' +
                  '<H2>Your data has been recorded:</H2>' + #13#10 +
                  '<P>' + FirstName + '.' + LastName + '@' + HostName +'</P>' +
                '</BODY>' +
              '</HTML>' + #13#10;
    Header := Client.Version + ' 200 OK' + #13#10 +
              'Content-Type: text/html' + #13#10 +
              'Content-Length: ' +
              IntToStr(Length(Body)) + #13#10 +
              #13#10;
    //Stream.Write(Header[1], Length(Header));
    StreamWriteStrA(Stream, Header);
    //Stream.Write(Body[1],   Length(Body));
    StreamWriteStrA(Stream, Body);
    Stream.Seek(0, 0);
    { Ask HTTP server component to send data stream for us }
    Client.DocStream := Stream;
    Client.SendStream;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.DisplayHeader(Client : TMyHttpConnection);
var
    I : Integer;
begin
    if not DisplayHeaderCheckBox.Checked then
        Exit;
    for I := 0 to Client.RequestHeader.Count - 1 do
        Display('HDR' + IntToStr(I + 1) + ') ' +
                Client.RequestHeader.Strings[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMyHttpConnection.Create(AOwner: TComponent);
begin
    inherited;
    { Little speed test }
    //SndBlkSize := 10 * 1024;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We need to override parent class destructor because we have allocated     }
{ memory for our data buffer.                                               }
destructor TMyHttpConnection.Destroy;
begin
    if Assigned(FPostedDataBuffer) then begin
        FreeMem(FPostedDataBuffer, FPostedDataSize);
        FPostedDataBuffer := nil;
        FPostedDataSize   := 0;
    end;
    if Assigned(FPostedRawData) then begin
        FreeMem(FPostedRawData);
        FPostedRawData := nil;
        FPostedDataSize   := 0;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.OcspSrvHttpOcspProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.OpenLogFile;
begin
    if FLogFileOpened then
        Exit;
    try
        AssignFile(FLogFile, FLogFileName);
        if FileExists(FLogFileName) then
            Append(FLogFile)
        else
            Rewrite(FLogFile);
        WriteLn(FLogFile, '[' + FormatDateTime('HH:NN:SS YYYY/MM/DD', Now) +
                          ' Log file opened.]');
        FLogFileOpened := TRUE;
    except
        FLogFileOpened := FALSE;
        Display('*** Unable to open log file ***');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.CloseLogFile;
begin
    if not FLogFileOpened then
        Exit;
    FLogFileOpened := FALSE;
    WriteLn(FLogFile, '[' + FormatDateTime('HH:NN:SS YYYY/MM/DD', Now) +
                      ' Log file Closed.]');
    CloseFile(FLogFile);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.WriteLogFileCheckBoxClick(Sender: TObject);
begin
    if WriteLogFileCheckBox.Checked then
        OpenLogFile
    else
        CloseLogFile;
    WriteLogFileCheckBox.Checked := FLogFileOpened;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.SslHttpServer1SslVerifyPeer(
    Sender  : TObject;
    var Ok  : Integer;
    Cert    : TX509Base);
begin
    if DisplaySslInfoCheckBox.Checked then
        Display('Received certificate'#13#10 +
                'Subject: ' + Cert.SubjectOneLine + #13#10 +
                'Issuer: '  + Cert.IssuerOneLine);
    if OK <> 1 then begin
        if DisplaySslInfoCheckBox.Checked then
            Display('Error msg: ' + Cert.VerifyErrMsg + #13#10 +
                    'In this example we accept any cert');
        OK := 1; //In this example we accept any client.
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.SslHttpServer1SslSetSessionIDContext(
    Sender               : TObject;
    var SessionIDContext : String);
begin
    { Tell Openssl a Session_ID_Context.                                    }
    { Openssl uses this data to tag a session before it's cached.           }
    SessionIDContext := Ssl_Session_ID_Context;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.SslHttpServer1SslSvrGetSession(
    Sender          : TObject;
    var SslSession  : Pointer;
    SessId          : Pointer;
    Idlen           : Integer;
    var IncRefCount : Boolean);
var
    LookupKey : string;
begin
{$IFDEF UNICODE}
    { We need to get binary data into a UnicodeString, allocate enough space. }
    { Not nice, however works in this case.                                   }
    SetLength(LookupKey, (IDLen div 2) + (IdLen mod 2));
{$ELSE}
    SetLength(LookupKey, IDLen);
{$ENDIF}
    Move(SessId^, Pointer(LookupKey)^, IDLen);
    SslSession  := SslAvlSessionCache1.GetSvrSession(LookupKey +
                                                     Ssl_Session_ID_Context,
                                                     IncRefCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.SslHttpServer1SslSvrNewSession(
    Sender                 : TObject;
    SslSession,
    SessId                 : Pointer;
    Idlen                  : Integer;
    var AddToInternalCache : Boolean);
var
    LookupKey : string;
begin
{$IFDEF UNICODE}
    { We need to get binary data into a UnicodeString, allocate enough space. }
    { Not nice, however works in this case.                                   }
    SetLength(LookupKey, (IDLen div 2) + (IdLen mod 2));
{$ELSE}
    SetLength(LookupKey, IDLen);
{$ENDIF}
    Move(SessId^, Pointer(LookupKey)^, IDLen);
    SslAvlSessionCache1.CacheSvrSession(SslSession,
                                        LookupKey + Ssl_Session_ID_Context,
                                        AddToInternalCache);
    if DisplaySslInfoCheckBox.Checked then
        Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
                TWSocket(Sender).GetPeerAddr + '] New SSL session created and ' +
                'cached in external cache class.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.SslHttpServer1BeforeProcessRequest(
    Sender,
    Client : TObject);
var
    Ticks  : Longword;
    Remote : TMyHttpConnection;
begin
    Remote := Client as TMyHttpConnection;
    { Request SSL3 renegotiation - doesn't work with IE so far!? }
    if Remote.SslEnable and (FRenegotiationInterval > 0) then begin
        Ticks := GetTickCount;
        if Remote.LastHandshake + FRenegotiationInterval < Ticks then begin
            if not Remote.SslStartRenegotiation then begin
                if DisplaySslInfoCheckBox.Checked then
                    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
                       Remote.GetPeerAddr + '] SslStartRenegotiation failed ');
            end
            else
                if DisplaySslInfoCheckBox.Checked then
                    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
                       Remote.GetPeerAddr + '] SSL renegotiation flag set.');
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.SslHttpServer1SslHandshakeDone(
    Sender         : TObject;
    ErrCode        : Word;
    PeerCert       : TX509Base;
    var Disconnect : Boolean);
var
    Remote : TMyHttpConnection;
begin
    Remote := Sender as TMyHttpConnection;
    FCliCipherList := Remote.SslGetSupportedCiphers (True, True);
    FCliCipherList := StringReplace(FCliCipherList, #13#10, ', ', [rfReplaceAll]);

 // normally list Client Hello in Server Name event, do it here if no name
 // list SMI, ALPN, versioon, extensions and stuff from Cli.CliHelloData
    if Remote.SslServerName = '' then
        Display('Client Hello: ' + WSocketGetCliHelloStr(Remote.CliHelloData));

    if ErrCode = 0 then begin
        Remote.LastHandshake := GetTickCount;
        if DisplaySslInfoCheckBox.Checked then begin
            Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
                    Remote.GetPeerAddr + '] ' + Remote.SslHandshakeRespMsg +
                    ', SessionReused ' + IntToStr (Ord(Remote.SslSessionReused)));    { V8.01 }
            Display('SSL Ciphers from Client: ' + #13#10 + FCliCipherList + #13#10);
        end;
    end
    else
        if DisplaySslInfoCheckBox.Checked then begin
            Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
                      Remote.GetPeerAddr + '] SslHandshake failed, error #' +
                        IntToStr (ErrCode) + ' - ' + Remote.SslHandshakeRespMsg);  { V8.01 }
            Display('SSL Ciphers from Client: ' + #13#10 + FCliCipherList + #13#10);
        end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.ButtonOSSLVersionClick(Sender: TObject);
begin
    SslContext1.InitContext; //Pre-loads OpenSSL DLL's
    Display(OpenSslVersion);
    Display(OpenSslCompilerFlags);
    Display(OpenSslBuiltOn);
    Display(OpenSslPlatForm);
    Display(OpenSslDir);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.RenegotiationIntervalEditChange(Sender: TObject);
begin
    try
        FRenegotiationInterval := StrToInt((Sender as TEdit).Text);
    except
        FRenegotiationInterval := 0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.BackgroundException(
    Sender       : TObject;
    E            : Exception;
    var CanClose : Boolean);
begin
    raise Exception.Create('BgException: ' + E.ClassName + ': ' + E.Message); //Test
    CanClose := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

