{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  Automatically download SSL X509 certificates from various
              issuers, including free certificates from Let's Encrypt, and
              commercial certificates from CertCentre AG.
              Supports ACME V2 protocol, and REST protocols for specific
              vendors.  Domain and DNS validated certificates should generally
              be issued without internvention, other commercial certificates
              may take days to be approved.
Creation:     May 2018
Updated:      Mar 2022
Version:      8.69
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

Trade Marks:  Let’s Encrypt and ISRG are trademarks of the Internet Security
              Research Group. All rights reserved.

History:
May 22, 2018 - V8.54 baseline
Oct 5, 2018  - V8.57 Added Database tab and settings
Oct 19, 2018 - V8.58 Bug fixes
Feb 21, 2019 - V8.60 Using new TIcsBuffLogStream for UTF8 or UTF16 file logging,
                 one log per day rather than per session.
               Added Socket Family to allow use with IPv6 hosts.
Jul 25, 2019 - V8.62 Supplier tab displays paths for cert and well-known dirs.
                     Load several type lists from literals for future proofing.
                     Removed Acme V1 protocol support (withdrawn from Nov 2019)
                     OpenSSL 1.0.2 only tick box gone, not needed any longer.
                     Added Proxy URL support, might be needed for servers behind
                       NAT firewalls for public access.
                     CertCenter AlwaysOn is discontinued and removed.
                     Comodo is now called Sectigo, sometimes old name still used.
                     Clear SAN grid properly.
                     BEWARE tls-alpn-01 challenge not working yet, wrong certificate
                      is sent to client.
Nov 12, 2019 - V8.63 Better selection for supplier database order ListView.
                     10 minute timeout to close idle account.
                     Added List Challenges button to log list of pending challenges
                       from the database.
May 18, 2020 - V8.64 Added support for International Domain Names for Applications (IDNA),
                       i.e. using accents and unicode characters in domain names.
                     X509 certificates always have A-Lavels (Punycode ASCII) domain names,
                      never UTF8 or Unicode.   IDNs are converted back to Unicode
                      for display, but X509 subject remains as A-Labels.
                     Fixed bug that stopped new orders after a successful one
                        saying no more today, due to date not being cleared.
                     Retain TRadiGroup defaults when reloading literals on first run.
                     No longer logs errors while waiting for DNS challenge to be
                       manually updated. There will be one 'wait' for each domain
                       being checked.
                     Support Domain Name Server Challenge automatically, updates
                       Windows DNS Server with TXT records for DNS Challenges using
                       WMI and local web server. Pending Cloudfare DNS using REST.
                     Support TLS-ALPN Cert Challenge for tls-alpn-01 SSL Certificate,
                        using local web server.
                     Certificate chain validation changed to use TX509List.
                     Split Acme ordering into get, local test and then start
                       challenges, added step numbers (1) to buttons to make process
                       more obvious. This allows manual updating of servers for
                       DNS or file challenge.
                     Self Signed button now works and will create CA Cert.
                     Acme Cancel Order and Revoke Order now work, former useful
                       to remove old challenges so fresh challenges can be tested.
Dec 09, 2020 - V8.65 Fixed warning.
                     Made Windows Server DNS updating more robust.
                     Display Supplier Database info even for order state none.
                     Supplier Database state and buttons reset for new order.
Sep 27, 2021 - V8.67 OpenSSL 3.0 makes several old ciphers and digests legacy so
                       default for encrypting passwords is now PrivKeyEncAES256.
                      Log OpenSSL version.
                     Tries to load OpenSSL 3.0 legacy provider for old algorithms,
                       logs if it's loaded, without which 3DES password encryption
                       is not available.
                     Added new Output Certificate tick boxes to password PEM and
                       PKCS12/PFX keys, and to install a certificate into the
                       Windows Personal/My Store so IIS can use it (note the private
                       key can not be exported).
Dec 20, 2021 - V8.68 Added property KeepOldCA set true to keep LA intermediate for
                       expired DST Root CA X3 root in bundles (for old Androids).
                       Keeping it may prevent some clients verifying the chain and
                       SslLabs testing gives a chain warning.
May 18, 2022 - V8.69 Added OverbyteIcsSslHttpOAuth split from OverbyteIcsSslHttpRest
                       with OAuth and related components.


For docunentation on how to use this sample, please see a lengthy Overview in
the OverbyteIcsSslX509Certs.pas unit.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsX509CertsTst1;

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids, Buttons,
  OverbyteIcsTypes,
  OverbyteIcsWSocket,
  OverbyteIcsWndControl,
  OverbyteIcsHttpProt,
  OverbyteIcsIniFiles,
  OverbyteIcsUtils,
  OverbyteIcsMimeUtils,
  OverbyteIcsURL,
  OverbyteIcsLogger,     { for TLogOption }
  OverbyteIcsSSLEAY,
  OverbyteIcsLibeay,
  OverbyteIcsSslHttpRest,
  OverbyteIcsSslHttpOAuth,   { V8.69 }
  OverbyteIcsSuperObject,
  OverbyteIcsSslJose,
  OverbyteIcsSslX509Utils,
  OverbyteIcsSslX509Certs,
  OverbyteIcsWmi,      { V8.64 }
  OverbyteIcsBlacklist;


const
    DnsChlgWindows = 0;  DnsChlgCloudfare = 1;


type
  TX509CertsForm = class(TForm)
    AccAcmeKeyV2: TComboBox;
    AcmeServerV2: TComboBox;
    AutoOrderComplete: TCheckBox;
    CACertFile: TEdit;
    CAPkeyFile: TEdit;
    CAPkeyPw: TEdit;
    CertAddress: TEdit;
    CertCentreApprovEmail: TComboBox;
    CertCentreOrderId: TEdit;
    CertCentreOrderRef: TEdit;
    CertCentreProducts: TListBox;
    CertCentreServer: TComboBox;
    CertContactEmail: TEdit;
    CertContactFirst: TEdit;
    CertContactLast: TEdit;
    CertContactTitle: TEdit;
    CertCountry: TEdit;
    CertCsrOrigin: TRadioGroup;
    CertLocality: TEdit;
    CertOldCsrFile: TEdit;
    CertOldPrvKey: TEdit;
    CertOrganization: TEdit;
    CertOrganizationalUnit: TEdit;
    CertOutFmtBudl: TCheckBox;
    CertOutFmtP12: TCheckBox;
    CertOutFmtP7: TCheckBox;
    CertOutFmtReq: TCheckBox;
    CertOutFmtSep: TCheckBox;
    CertPhone: TEdit;
    CertPostCode: TEdit;
    CertSignDigestType: TRadioGroup;
    CertState: TEdit;
    CertValidity: TEdit;
    DebugLogging: TComboBox;
    DirAcmeConfV2: TEdit;
    DirCertCenConf: TEdit;
    DirDatabase: TComboBox;
    DirLogs: TEdit;
    DirPubWebCert: TEdit;
    DirWellKnown: TEdit;
    DomWebSrvIP: TComboBox;
    LogJson: TCheckBox;
    LogPkeys: TCheckBox;
    OAuthAccToken: TEdit;
    OAuthAppUrl: TEdit;
    OAuthAutoRefresh: TCheckBox;
    OAuthClientId: TEdit;
    OAuthClientSecret: TEdit;
    OAuthExpire: TEdit;
    OAuthRedirectUrl: TEdit;
    OAuthRefToken: TEdit;
    OAuthRefrMins: TEdit;
    OAuthScope: TEdit;
    OAuthTokenUrl: TEdit;
    OAuthWebIP: TComboBox;
    OAuthWebPort: TEdit;
    OwnCACertDir: TEdit;
    PrivKeyCipher: TRadioGroup;
    PrivKeyPassword: TEdit;
    PrivKeyType: TRadioGroup;
    SuppCertChallenge: TRadioGroup;
    SupplierEmail: TEdit;
    IpSocFamily: TComboBox;   { V8.60 }
    ProxyURL: TEdit;          { V8.62 }
    DnsChlgType: TComboBox;   { V8.64 }
    SelfSignedCA: TCheckBox;  { V8.64}
    CertPwPemKeys: TCheckBox; { V8.67}
    CertPwPkcs12: TCheckBox;  { V8.67}
    CertWindowsStore: TCheckBox; { V8.67}

 // properties not saved
    X509Certs1: TSslX509Certs;
    LogWin: TMemo;
    CertSANGrid: TStringGrid;
    CertBuy: TEdit;
    OAuthAuthCode: TEdit;
    PageControl1: TPageControl;
    TabCommon: TTabSheet;
    Label3: TLabel;
    Label15: TLabel;
    TabDomain: TTabSheet;
    Label9: TLabel;
    Label2: TLabel;
    Label13: TLabel;
    Label23: TLabel;
    doTestChallenge: TButton;
    TabInfo: TTabSheet;
    lbCountry: TLabel;
    lbState: TLabel;
    lbLocality: TLabel;
    lbOrganization: TLabel;
    lbOrganizationalUnit: TLabel;
    lbEMail: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label24: TLabel;
    TabAcme2: TTabSheet;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    doAcmeAcc: TButton;
    doAcmeCheckOrder: TButton;
    doAcmeStartChallng: TButton;
    doAcmeGetCert: TButton;
    TabCCOrder: TTabSheet;
    Label6: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    Label16: TLabel;
    LabelCertInfo: TLabel;
    Label4: TLabel;
    doCCProfile: TButton;
    doCertCentreCollect: TButton;
    doCertCentreOther: TButton;
    doCertCentreOrders: TButton;
    doCertCentreCheck: TButton;
    TabCCOAuth: TTabSheet;
    TabServtas: TTabSheet;
    BoxOAuthApp: TGroupBox;
    Label7: TLabel;
    Label11: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label34: TLabel;
    BoxOAuthTokens: TGroupBox;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    BoxOAuthWeb: TGroupBox;
    Label39: TLabel;
    Label40: TLabel;
    Label14: TLabel;
    Label22: TLabel;
    Label41: TLabel;
    doOARefreshNow: TButton;
    doOACodeToken: TButton;
    Label42: TLabel;
    CertCommonName: TEdit;
    LabelAcme2Info: TLabel;
    TabDatabase: TTabSheet;
    Label43: TLabel;
    SelDirDatabase: TBitBtn;
    OpenDirDiag: TOpenDialog;
    doOpenDatabase: TButton;
    LabelDB: TLabel;
    Label45: TLabel;
    doDBCheck: TButton;
    doDBOrder: TButton;
    LabelInfoDomain: TLabel;
    doClearDomain: TButton;
    doCloseDatabase: TButton;
    TabOwnCA: TTabSheet;
    CABox: TGroupBox;
    Label44: TLabel;
    Label46: TLabel;
    SelCACertFile: TBitBtn;
    SelCAPkeyFile: TBitBtn;
    doSelfSigned: TButton;
    doCASignCert: TButton;
    LabelOwnCA: TLabel;
    Label48: TLabel;
    doLoadCA: TButton;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    OpenFileDlg: TOpenDialog;
    BoxCertFmts: TGroupBox;
    SelDirLogs: TBitBtn;
    SelCertOldCsrFile: TBitBtn;
    SelCertOldPrvKey: TBitBtn;
    SelDirWellKnown: TBitBtn;
    SelDirPubWebCert: TBitBtn;
    SelDirAcmeConfV2: TBitBtn;
    SelDirCertCenConf: TBitBtn;
    doCheckCSR: TButton;
    LabelCertOwnCA: TLabel;
    SelOwnCACertDir: TBitBtn;
    CertSerNumType: TRadioGroup;
    DatabaseDomains: TListView;
    TabChallenges: TTabSheet;
    DatabaseChallg: TListView;
    Label47: TLabel;
    doDBRevoke: TButton;
    doDBCollect: TButton;
    doDBCancel: TButton;
    doDBRemove: TButton;
    doAcmeSaveOrder: TButton;
    doCertCentreSaveOrder: TButton;
    doDBRedist: TButton;
    Label54: TLabel;
    Label1: TLabel;
    doDBListChallg: TButton;
    Label5: TLabel;
    doAcmeGetChallng: TButton;
    doAcmeTestChallng: TButton;
    doDBGetChallg: TButton;
    Label28: TLabel;


    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure doCCProfileClick(Sender: TObject);
    procedure doCertCentreCheckClick(Sender: TObject);
    procedure doCertCentreOtherClick(Sender: TObject);
    procedure doCertCentreOrdersClick(Sender: TObject);
    procedure doCertCentreCollectClick(Sender: TObject);
    procedure CertCentreProductsClick(Sender: TObject);
    procedure doTestChallengeClick(Sender: TObject);
    procedure doAcmeAccClick(Sender: TObject);
    procedure doAcmeCheckOrderClick(Sender: TObject);
    procedure doAcmeStartChallngClick(Sender: TObject);
    procedure doAcmeGetCertClick(Sender: TObject);
    procedure X509Certs1CertProg(Sender: TObject; LogOption: TLogOption;
      const Msg: string);
    procedure X509Certs1NewCert(Sender: TObject);
    procedure X509Certs1NewToken(Sender: TObject);
    procedure doOARefreshNowClick(Sender: TObject);
    procedure doOACodeTokenClick(Sender: TObject);
    procedure X509Certs1ChallengeDNS(Sender: TObject;
      ChallengeItem: TChallengeItem; var ChlgOK: Boolean);
    procedure X509Certs1ChallengeEmail(Sender: TObject;
      ChallengeItem: TChallengeItem; var ChlgOK: Boolean);
    procedure X509Certs1ChallengeFTP(Sender: TObject;
      ChallengeItem: TChallengeItem; var ChlgOK: Boolean);
    procedure SelDirDatabaseClick(Sender: TObject);
    procedure doOpenDatabaseClick(Sender: TObject);
    procedure DatabaseDomainsClick(Sender: TObject);
    procedure doDBCheckClick(Sender: TObject);
    procedure doDBOrderClick(Sender: TObject);
    procedure doClearDomainClick(Sender: TObject);
    procedure doCloseDatabaseClick(Sender: TObject);
    procedure SelCACertFileClick(Sender: TObject);
    procedure doLoadCAClick(Sender: TObject);
    procedure SelCAPkeyFileClick(Sender: TObject);
    procedure SelDirLogsClick(Sender: TObject);
    procedure SelCertOldCsrFileClick(Sender: TObject);
    procedure SelCertOldPrvKeyClick(Sender: TObject);
    procedure SelDirWellKnownClick(Sender: TObject);
    procedure SelDirPubWebCertClick(Sender: TObject);
    procedure SelDirAcmeConfV2Click(Sender: TObject);
    procedure SelDirCertCenConfClick(Sender: TObject);
    procedure doCheckCSRClick(Sender: TObject);
    procedure doCASignCertClick(Sender: TObject);
    procedure doSelfSignedClick(Sender: TObject);
    procedure SelOwnCACertDirClick(Sender: TObject);
    procedure doDBCollectClick(Sender: TObject);
    procedure doDBCancelClick(Sender: TObject);
    procedure doDBRevokeClick(Sender: TObject);
    procedure doDBRemoveClick(Sender: TObject);
    procedure X509Certs1ChallgRefresh(Sender: TObject);
    procedure X509Certs1DomainsRefresh(Sender: TObject);
    procedure X509Certs1SuppDBRefresh(Sender: TObject);
    procedure doAcmeSaveOrderClick(Sender: TObject);
    procedure doCertCentreSaveOrderClick(Sender: TObject);
    procedure X509Certs1OAuthAuthUrl(Sender: TObject; const URL: string);
    procedure doDBRedistClick(Sender: TObject);
    procedure DirLogsExit(Sender: TObject);
    procedure DatabaseDomainsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure doDBListChallgClick(Sender: TObject);
    procedure doAcmeGetChallngClick(Sender: TObject);
    procedure doAcmeTestChallngClick(Sender: TObject);
    procedure doDBGetChallgClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FProgDir: String;
    FIniFileName: String;
    FCookieFileName: String;
    FInitialized: Boolean;
    FIcsBuffLogStream: TIcsBuffLogStream;  { V8.60 }
    FPendCCProfile: Boolean;
    procedure AddLog (const S: string) ;
    procedure SetOAParams;
    procedure SetCommParams;
    procedure SetCertParams;
    procedure ResetButtons;
    procedure RefreshDomains;
    procedure ResetDomButtons;
    procedure SetDomButtons;
    procedure OpenLogFile;
  end;

const
    SectionMainWindow    = 'MainWindow';
    KeyTop               = 'Top';
    KeyLeft              = 'Left';
    KeyWidth             = 'Width';
    KeyHeight            = 'Height';
    SectionData          = 'Data';
    KeyCertSANGrid       = 'CertSANGrid';

var
  X509CertsForm: TX509CertsForm;
  DnsNetComputer, DnsNetUserName, DnsNetPassword: String;  { V8.65 WMI login }


implementation

{$R *.dfm}


procedure TX509CertsForm.FormCreate(Sender: TObject);
var
    I, J: Integer;
    CT: TChallengeType;
    KC: TSslPrivKeyCipher;
begin
{$IF CompilerVersion > 17}
{$WARNINGS OFF}
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);     { V8.69 }
{$WARNINGS ON}
{$IFEND}
    FProgDir     := ExtractFilePath(ParamStr(0));
    FIniFileName := GetIcsIniFileName;
    FCookieFileName := ChangeFileExt(FIniFileName, '.cookie');

 // Avoid dynamical loading and unloading the SSL DLLs plenty of times
//  GSSLEAY_DLL_IgnoreNew := True;     { ignore OpenSSL 3.0 and later }
//  GSSLEAY_DLL_IgnoreOld := True;     { ignore OpenSSL 1.1 }
// note both not allowed true
    GSSL_DLL_DIR := FProgDir;          { only from our directory }
    GSSL_SignTest_Check := True;       { check digitally signed }
    GSSL_SignTest_Certificate := True; { check digital certificate }
    GSSLEAY_LOAD_LEGACY := True;       { V8.67 OpenSSL 3.0 legacy provider for old algorithms }
    CertSANGrid.Cells[0,0] := 'Domain Name';
    CertSANGrid.Cells[1,0] := 'Web Server UNC HTTP .Well-Known';
    CertSANGrid.Cells[2,0] := 'Web Server UNC Public Certs Dir';
    CertSANGrid.Cells[3,0] := 'Approval Email';
    OpenFileDlg.Filter := SslCertFileOpenExts;    { V8.62 }
      {  'Certs *.pem;*.cer;*.crt;*.der;*.p12;*.pfx;*.p7*;*.spc|' +
                          '*.pem;*.cer;*.crt;*.der;*.p12;*.pfx;*.p7*;*.spc|' +
                          'All Files *.*|*.*'; }
    J := CertSignDigestType.ItemIndex;                    { V8.84 keep initial default }
    CertSignDigestType.Items.Clear;
    for I := 0 to DigestListLitsLast do
      CertSignDigestType.Items.Add(DigestListLits[I]);    { V8.62 }
    CertSignDigestType.ItemIndex := j;                    { V8.84 reset default }
    J := PrivKeyType.ItemIndex;                           { V8.84 keep initial default }
    PrivKeyType.Items.Clear;
    for I := 0 to SslPrivKeyTypeLitsLast1 do
        PrivKeyType.Items.Add(SslPrivKeyTypeLits[I]);     { V8.62 }
    PrivKeyType.ItemIndex := j;                           { V8.84 reset default }
    J := SuppCertChallenge.ItemIndex;                     { V8.84 keep initial default }
    SuppCertChallenge.Items.Clear;
    for CT := Low(TChallengeType) to High(TChallengeType) do
        SuppCertChallenge.Items.Add(ChallengeTypeLits[CT]);  { V8.62 }
    SuppCertChallenge.ItemIndex := j;                        { V8.84 reset default }
    J := PrivKeyCipher.ItemIndex;                            { V8.84 keep initial default }
    PrivKeyCipher.Items.Clear;
    for KC := Low(TSslPrivKeyCipher) to High(TSslPrivKeyCipher) do
        PrivKeyCipher.Items.Add(SslPrivKeyCipherLits[KC]);  { V8.62 }
    PrivKeyCipher.ItemIndex := j;                           { V8.84 reset default }
    DnsNetComputer := '';   { V8.65 WMI login }
    DnsNetUserName := '';
    DnsNetPassword := '';

end;

procedure TX509CertsForm.FormDestroy(Sender: TObject);
begin
//
end;

procedure TX509CertsForm.FormShow(Sender: TObject);
var
    IniFile: TIcsIniFile;
    SL, SR: TStringList;
    I, tot, row: Integer;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile := TIcsIniFile.Create(FIniFileName);
        Width := IniFile.ReadInteger(SectionMainWindow, KeyWidth,  Width);
        Height := IniFile.ReadInteger(SectionMainWindow, KeyHeight, Height);
        Top := IniFile.ReadInteger(SectionMainWindow, KeyTop, (Screen.Height - Height) div 2);
        Left := IniFile.ReadInteger(SectionMainWindow, KeyLeft, (Screen.Width  - Width)  div 2);
        SL := TStringList.Create;
        SR := TStringList.Create;
        try
             SL.Delimiter := ',';
             SR.Delimiter := '|';
             SL.DelimitedText := IniFile.ReadString(SectionData, KeyCertSANGrid, '');
             tot := SL.Count;
             CertSANGrid.RowCount := tot + 10;
             row := 1;
             for I := 0 to tot - 1 do begin
                SR.DelimitedText := SL[I];
                if SR.Count >= 4 then begin
                    if Length(Trim(SR[0])) > 2 then begin
                        CertSANGrid.Cells[0, row] := SR[0];
                        CertSANGrid.Cells[1, row] := SR[1];
                        CertSANGrid.Cells[2, row] := SR[2];
                        CertSANGrid.Cells[3, row] := SR[3];
                        row := row + 1;
                    end;
                end;
             end;
        finally
             SL.Free;
             SR.Free;
        end;

       with IniFile do begin
  AccAcmeKeyV2.ItemIndex := ReadInteger (SectionData, 'AccAcmeKeyV2_ItemIndex', AccAcmeKeyV2.ItemIndex) ;
  AcmeServerV2.Text := ReadString (SectionData, 'AcmeServerV2_Text', AcmeServerV2.Text) ;
  if ReadString (SectionData, 'AutoOrderComplete_Checked', 'False') = 'True' then AutoOrderComplete.Checked := true else AutoOrderComplete.Checked := false ;
  CACertFile.Text := ReadString (SectionData, 'CACertFile_Text', CACertFile.Text) ;
  CAPkeyFile.Text := ReadString (SectionData, 'CAPkeyFile_Text', CAPkeyFile.Text) ;
  CAPkeyPw.Text := ReadString (SectionData, 'CAPkeyPw_Text', CAPkeyPw.Text) ;
  CertAddress.Text := ReadString (SectionData, 'CertAddress_Text', CertAddress.Text) ;
  CertCentreApprovEmail.Text := ReadString (SectionData, 'CertCentreApprovEmail_Text', CertCentreApprovEmail.Text) ;
  CertCentreOrderId.Text := ReadString (SectionData, 'CertCentreOrderId_Text', CertCentreOrderId.Text) ;
  CertCentreOrderRef.Text := ReadString (SectionData, 'CertCentreOrderRef_Text', CertCentreOrderRef.Text) ;
  CertCentreProducts.ItemIndex := ReadInteger (SectionData, 'CertCentreProducts_ItemIndex', CertCentreProducts.ItemIndex) ;
  CertCentreProducts.Items.CommaText := ReadString (SectionData, 'CertCentreProducts_Items', '') ;
  CertCentreServer.Text := ReadString (SectionData, 'CertCentreServer_Text', CertCentreServer.Text) ;
  CertCommonName.Text := ReadString (SectionData, 'CertCommonName_Text', CertCommonName.Text) ;
  CertContactEmail.Text := ReadString (SectionData, 'CertContactEmail_Text', CertContactEmail.Text) ;
  CertContactFirst.Text := ReadString (SectionData, 'CertContactFirst_Text', CertContactFirst.Text) ;
  CertContactLast.Text := ReadString (SectionData, 'CertContactLast_Text', CertContactLast.Text) ;
  CertContactTitle.Text := ReadString (SectionData, 'CertContactTitle_Text', CertContactTitle.Text) ;
  CertCountry.Text := ReadString (SectionData, 'CertCountry_Text', CertCountry.Text) ;
  CertCsrOrigin.ItemIndex := ReadInteger (SectionData, 'CertCsrOrigin_ItemIndex', CertCsrOrigin.ItemIndex) ;
  CertLocality.Text := ReadString (SectionData, 'CertLocality_Text', CertLocality.Text) ;
  CertOldCsrFile.Text := ReadString (SectionData, 'CertOldCsrFile_Text', CertOldCsrFile.Text) ;
  CertOldPrvKey.Text := ReadString (SectionData, 'CertOldPrvKey_Text', CertOldPrvKey.Text) ;
  CertOrganization.Text := ReadString (SectionData, 'CertOrganization_Text', CertOrganization.Text) ;
  CertOrganizationalUnit.Text := ReadString (SectionData, 'CertOrganizationalUnit_Text', CertOrganizationalUnit.Text) ;
  if ReadString (SectionData, 'CertOutFmtBudl_Checked', 'False') = 'True' then CertOutFmtBudl.Checked := true else CertOutFmtBudl.Checked := false ;
  if ReadString (SectionData, 'CertOutFmtP12_Checked', 'False') = 'True' then CertOutFmtP12.Checked := true else CertOutFmtP12.Checked := false ;
  if ReadString (SectionData, 'CertOutFmtP7_Checked', 'False') = 'True' then CertOutFmtP7.Checked := true else CertOutFmtP7.Checked := false ;
  if ReadString (SectionData, 'CertOutFmtReq_Checked', 'False') = 'True' then CertOutFmtReq.Checked := true else CertOutFmtReq.Checked := false ;
  if ReadString (SectionData, 'CertOutFmtSep_Checked', 'False') = 'True' then CertOutFmtSep.Checked := true else CertOutFmtSep.Checked := false ;
  CertPhone.Text := ReadString (SectionData, 'CertPhone_Text', CertPhone.Text) ;
  CertPostCode.Text := ReadString (SectionData, 'CertPostCode_Text', CertPostCode.Text) ;
  CertSerNumType.ItemIndex := ReadInteger (SectionData, 'CertSerNumType_ItemIndex', CertSerNumType.ItemIndex) ;
  CertSignDigestType.ItemIndex := ReadInteger (SectionData, 'CertSignDigestType_ItemIndex', CertSignDigestType.ItemIndex) ;
  CertState.Text := ReadString (SectionData, 'CertState_Text', CertState.Text) ;
  CertValidity.Text := ReadString (SectionData, 'CertValidity_Text', CertValidity.Text) ;
  DebugLogging.ItemIndex := ReadInteger (SectionData, 'DebugLogging_ItemIndex', DebugLogging.ItemIndex) ;
  DirAcmeConfV2.Text := ReadString (SectionData, 'DirAcmeConfV2_Text', DirAcmeConfV2.Text) ;
  DirCertCenConf.Text := ReadString (SectionData, 'DirCertCenConf_Text', DirCertCenConf.Text) ;
  DirDatabase.Items.CommaText := ReadString (SectionData, 'DirDatabase_Items', '') ;
  DirDatabase.Text := ReadString (SectionData, 'DirDatabase_Text', DirDatabase.Text) ;
  DirLogs.Text := ReadString (SectionData, 'DirLogs_Text', DirLogs.Text) ;
  DirPubWebCert.Text := ReadString (SectionData, 'DirPubWebCert_Text', DirPubWebCert.Text) ;
  DirWellKnown.Text := ReadString (SectionData, 'DirWellKnown_Text', DirWellKnown.Text) ;
  DomWebSrvIP.Text := ReadString (SectionData, 'DomWebSrvIP_Text', DomWebSrvIP.Text) ;
  if ReadString (SectionData, 'LogJson_Checked', 'False') = 'True' then LogJson.Checked := true else LogJson.Checked := false ;
  if ReadString (SectionData, 'LogPkeys_Checked', 'False') = 'True' then LogPkeys.Checked := true else LogPkeys.Checked := false ;
  OAuthAccToken.Text := ReadString (SectionData, 'OAuthAccToken_Text', OAuthAccToken.Text) ;
  OAuthAppUrl.Text := ReadString (SectionData, 'OAuthAppUrl_Text', OAuthAppUrl.Text) ;
  if ReadString (SectionData, 'OAuthAutoRefresh_Checked', 'False') = 'True' then OAuthAutoRefresh.Checked := true else OAuthAutoRefresh.Checked := false ;
  OAuthClientId.Text := ReadString (SectionData, 'OAuthClientId_Text', OAuthClientId.Text) ;
  OAuthClientSecret.Text := ReadString (SectionData, 'OAuthClientSecret_Text', OAuthClientSecret.Text) ;
  OAuthExpire.Text := ReadString (SectionData, 'OAuthExpire_Text', OAuthExpire.Text) ;
  OAuthRedirectUrl.Text := ReadString (SectionData, 'OAuthRedirectUrl_Text', OAuthRedirectUrl.Text) ;
  OAuthRefToken.Text := ReadString (SectionData, 'OAuthRefToken_Text', OAuthRefToken.Text) ;
  OAuthRefrMins.Text := ReadString (SectionData, 'OAuthRefrMins_Text', OAuthRefrMins.Text) ;
  OAuthScope.Text := ReadString (SectionData, 'OAuthScope_Text', OAuthScope.Text) ;
  OAuthTokenUrl.Text := ReadString (SectionData, 'OAuthTokenUrl_Text', OAuthTokenUrl.Text) ;
  OAuthWebIP.Text := ReadString (SectionData, 'OAuthWebIP_Text', OAuthWebIP.Text) ;
  OAuthWebPort.Text := ReadString (SectionData, 'OAuthWebPort_Text', OAuthWebPort.Text) ;
  OwnCACertDir.Text := ReadString (SectionData, 'OwnCACertDir_Text', OwnCACertDir.Text) ;
  PrivKeyCipher.ItemIndex := ReadInteger (SectionData, 'PrivKeyCipher_ItemIndex', PrivKeyCipher.ItemIndex) ;
  PrivKeyPassword.Text := ReadString (SectionData, 'PrivKeyPassword_Text', PrivKeyPassword.Text) ;
  PrivKeyType.ItemIndex := ReadInteger (SectionData, 'PrivKeyType_ItemIndex', PrivKeyType.ItemIndex) ;
  SuppCertChallenge.ItemIndex := ReadInteger (SectionData, 'SuppCertChallenge_ItemIndex', SuppCertChallenge.ItemIndex) ;
  SupplierEmail.Text := ReadString (SectionData, 'SupplierEmail_Text', SupplierEmail.Text) ;
  IpSocFamily.ItemIndex := ReadInteger (SectionData, 'IpSocFamily_ItemIndex', IpSocFamily.ItemIndex) ;
  ProxyURL.Text := ReadString (SectionData, 'ProxyURL_Text', ProxyURL.Text) ;
  DnsChlgType.ItemIndex := ReadInteger (SectionData, 'DnsChlgType_ItemIndex', DnsChlgType.ItemIndex) ;
  if ReadString (SectionData, 'SelfSignedCA_Checked', 'False') = 'True' then SelfSignedCA.Checked := true else SelfSignedCA.Checked := false ;
  if ReadString (SectionData, 'CertPwPemKeys_Checked', 'False') = 'True' then CertPwPemKeys.Checked := true else CertPwPemKeys.Checked := false ;
  if ReadString (SectionData, 'CertPwPkcs12_Checked', 'True') = 'True' then CertPwPkcs12.Checked := true else CertPwPkcs12.Checked := false ;   // V8.67 default true
  if ReadString (SectionData, 'CertWindowsStore_Checked', 'False') = 'True' then CertWindowsStore.Checked := true else CertWindowsStore.Checked := false ;
      end;
        IniFile.Free;
    end;

    OAuthWebIP.Items.Assign(LocalIPList);
    DomWebSrvIP.Items.Assign(OAuthWebIP.Items);
    OAuthWebIP.Items.Insert(0, ICS_LOCAL_HOST_V4);
    ResetButtons;
    OverbyteIcsWSocket.LoadSsl;

// get API server URLs
    AcmeServerV2.Items.Clear;
    AcmeServerV2.Items.Add(X509Certs1.GetServerAPIUrl(SuppProtoAcmeV2, False));
    AcmeServerV2.Items.Add(X509Certs1.GetServerAPIUrl(SuppProtoAcmeV2, True));
    CertCentreServer.Items.Clear;
    CertCentreServer.Items.Add(X509Certs1.GetServerAPIUrl(SuppProtoCertCentre, False));
    if AcmeServerV2.Text = '' then AcmeServerV2.ItemIndex := 0;
    if CertCentreServer.Text = '' then CertCentreServer.ItemIndex := 0;

// V8.60 prepare log file
    OpenLogFile;
    if NOT GSSLStaticLinked then    { V8.67 }
        AddLog('SSL/TLS Version: ' + OpenSslVersion + ' - ' + OpenSslPlatForm + ': ' + GLIBEAY_DLL_FileName)
    else
        AddLog('SSL/TLS Static Linked Version: ' + OpenSslVersion + ' - ' + OpenSslPlatForm);
    if (ICS_OPENSSL_VERSION_MAJOR >= 3) then begin      { V8.67 }
        if ICS_OSSL3_LOADED_LEGACY then
            AddLog('Legacy Provider Loaded OK')
        else
             AddLog('Legacy Provider Not Loaded');
    end;
end;

{ V8.60 this event is used to open the log file, or change it's name
  if already opened, change only needed for GUI applications where the user
  can change the log path. Note ls written as UTF8 codepage }
procedure TX509CertsForm.OpenLogFile;
var
    FName: String;
begin
    if DirLogs.Text = '' then Exit; // no log
    FName := '"' + IncludeTrailingPathDelimiter(DirLogs.Text) +
                                              'ics-certlog-"yyyy-mm-dd".log"';
    if NOT Assigned(FIcsBuffLogStream) then
        FIcsBuffLogStream := TIcsBuffLogStream.Create(self, FName,
                                X509CertsForm.Caption + IcsCRLF, FileCPUtf8)
    else begin
        if FName = FIcsBuffLogStream.NameMask then Exit; // skip no change
        if FIcsBuffLogStream.LogSize > 0 then
            FIcsBuffLogStream.FlushFile(True);  // changing log path, write old log first
        FIcsBuffLogStream.NameMask := FName;
    end;
    AddLog(IcsCRLF + 'Opened log file: ' + FIcsBuffLogStream.FullName);
end;

procedure TX509CertsForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
    temp: String;
    SL: TStringList;
    I: Integer;
begin
    X509Certs1.StopDomSrv;  // V8.64
    FreeAndNil(FIcsBuffLogStream); // V8.60 write log file }
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionMainWindow, KeyTop, Top);
    IniFile.WriteInteger(SectionMainWindow, KeyLeft, Left);
    IniFile.WriteInteger(SectionMainWindow, KeyWidth, Width);
    IniFile.WriteInteger(SectionMainWindow, KeyHeight, Height);
    SL := TStringList.Create;
    try
        SL.Delimiter := ',';
        for I := 1 to CertSANGrid.RowCount - 1 do begin
            if Length(Trim(CertSANGrid.Cells[0,I])) > 2 then
                SL.Add(CertSANGrid.Cells[0,I] + '|' + CertSANGrid.Cells[1,I] +
                     '|' + CertSANGrid.Cells[2,I] + '|' + CertSANGrid.Cells[3,I] );
        end;
        IniFile.WriteString(SectionData, KeyCertSANGrid, SL.DelimitedText);
    finally
        SL.Free;
    end;

    with IniFile do begin
  WriteInteger (SectionData, 'AccAcmeKeyV2_ItemIndex', AccAcmeKeyV2.ItemIndex) ;
  WriteString (SectionData, 'AcmeServerV2_Text', AcmeServerV2.Text) ;
  if AutoOrderComplete.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'AutoOrderComplete_Checked', temp) ;
  WriteString (SectionData, 'CACertFile_Text', CACertFile.Text) ;
  WriteString (SectionData, 'CAPkeyFile_Text', CAPkeyFile.Text) ;
  WriteString (SectionData, 'CAPkeyPw_Text', CAPkeyPw.Text) ;
  WriteString (SectionData, 'CertAddress_Text', CertAddress.Text) ;
  WriteString (SectionData, 'CertCentreApprovEmail_Text', CertCentreApprovEmail.Text) ;
  WriteString (SectionData, 'CertCentreOrderId_Text', CertCentreOrderId.Text) ;
  WriteString (SectionData, 'CertCentreOrderRef_Text', CertCentreOrderRef.Text) ;
  WriteInteger (SectionData, 'CertCentreProducts_ItemIndex', CertCentreProducts.ItemIndex) ;
  WriteString (SectionData, 'CertCentreProducts_Items', CertCentreProducts.Items.CommaText) ;
  WriteString (SectionData, 'CertCentreServer_Text', CertCentreServer.Text) ;
  WriteString (SectionData, 'CertCommonName_Text', CertCommonName.Text) ;
  WriteString (SectionData, 'CertContactEmail_Text', CertContactEmail.Text) ;
  WriteString (SectionData, 'CertContactFirst_Text', CertContactFirst.Text) ;
  WriteString (SectionData, 'CertContactLast_Text', CertContactLast.Text) ;
  WriteString (SectionData, 'CertContactTitle_Text', CertContactTitle.Text) ;
  WriteString (SectionData, 'CertCountry_Text', CertCountry.Text) ;
  WriteInteger (SectionData, 'CertCsrOrigin_ItemIndex', CertCsrOrigin.ItemIndex) ;
  WriteString (SectionData, 'CertLocality_Text', CertLocality.Text) ;
  WriteString (SectionData, 'CertOldCsrFile_Text', CertOldCsrFile.Text) ;
  WriteString (SectionData, 'CertOldPrvKey_Text', CertOldPrvKey.Text) ;
  WriteString (SectionData, 'CertOrganization_Text', CertOrganization.Text) ;
  WriteString (SectionData, 'CertOrganizationalUnit_Text', CertOrganizationalUnit.Text) ;
  if CertOutFmtBudl.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertOutFmtBudl_Checked', temp) ;
  if CertOutFmtP12.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertOutFmtP12_Checked', temp) ;
  if CertOutFmtP7.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertOutFmtP7_Checked', temp) ;
  if CertOutFmtReq.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertOutFmtReq_Checked', temp) ;
  if CertOutFmtSep.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertOutFmtSep_Checked', temp) ;
  WriteString (SectionData, 'CertPhone_Text', CertPhone.Text) ;
  WriteString (SectionData, 'CertPostCode_Text', CertPostCode.Text) ;
  WriteInteger (SectionData, 'CertSerNumType_ItemIndex', CertSerNumType.ItemIndex) ;
  WriteInteger (SectionData, 'CertSignDigestType_ItemIndex', CertSignDigestType.ItemIndex) ;
  WriteString (SectionData, 'CertState_Text', CertState.Text) ;
  WriteString (SectionData, 'CertValidity_Text', CertValidity.Text) ;
  WriteInteger (SectionData, 'DebugLogging_ItemIndex', DebugLogging.ItemIndex) ;
  WriteString (SectionData, 'DirAcmeConfV2_Text', DirAcmeConfV2.Text) ;
  WriteString (SectionData, 'DirCertCenConf_Text', DirCertCenConf.Text) ;
  WriteString (SectionData, 'DirDatabase_Items', DirDatabase.Items.CommaText) ;
  WriteString (SectionData, 'DirDatabase_Text', DirDatabase.Text) ;
  WriteString (SectionData, 'DirLogs_Text', DirLogs.Text) ;
  WriteString (SectionData, 'DirPubWebCert_Text', DirPubWebCert.Text) ;
  WriteString (SectionData, 'DirWellKnown_Text', DirWellKnown.Text) ;
  WriteString (SectionData, 'DomWebSrvIP_Text', DomWebSrvIP.Text) ;
  if LogJson.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'LogJson_Checked', temp) ;
  if LogPkeys.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'LogPkeys_Checked', temp) ;
  WriteString (SectionData, 'OAuthAccToken_Text', OAuthAccToken.Text) ;
  WriteString (SectionData, 'OAuthAppUrl_Text', OAuthAppUrl.Text) ;
  if OAuthAutoRefresh.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'OAuthAutoRefresh_Checked', temp) ;
  WriteString (SectionData, 'OAuthClientId_Text', OAuthClientId.Text) ;
  WriteString (SectionData, 'OAuthClientSecret_Text', OAuthClientSecret.Text) ;
  WriteString (SectionData, 'OAuthExpire_Text', OAuthExpire.Text) ;
  WriteString (SectionData, 'OAuthRedirectUrl_Text', OAuthRedirectUrl.Text) ;
  WriteString (SectionData, 'OAuthRefToken_Text', OAuthRefToken.Text) ;
  WriteString (SectionData, 'OAuthRefrMins_Text', OAuthRefrMins.Text) ;
  WriteString (SectionData, 'OAuthScope_Text', OAuthScope.Text) ;
  WriteString (SectionData, 'OAuthTokenUrl_Text', OAuthTokenUrl.Text) ;
  WriteString (SectionData, 'OAuthWebIP_Text', OAuthWebIP.Text) ;
  WriteString (SectionData, 'OAuthWebPort_Text', OAuthWebPort.Text) ;
  WriteString (SectionData, 'OwnCACertDir_Text', OwnCACertDir.Text) ;
  WriteInteger (SectionData, 'PrivKeyCipher_ItemIndex', PrivKeyCipher.ItemIndex) ;
  WriteString (SectionData, 'PrivKeyPassword_Text', PrivKeyPassword.Text) ;
  WriteInteger (SectionData, 'PrivKeyType_ItemIndex', PrivKeyType.ItemIndex) ;
  WriteInteger (SectionData, 'SuppCertChallenge_ItemIndex', SuppCertChallenge.ItemIndex) ;
  WriteString (SectionData, 'SupplierEmail_Text', SupplierEmail.Text) ;
  WriteInteger (SectionData, 'IpSocFamily_ItemIndex', IpSocFamily.ItemIndex) ;
  WriteString (SectionData, 'ProxyURL_Text', ProxyURL.Text) ;
  WriteInteger (SectionData, 'DnsChlgType_ItemIndex', DnsChlgType.ItemIndex) ;
  if SelfSignedCA.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'SelfSignedCA_Checked', temp) ;
  if CertPwPemKeys.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertPwPemKeys_Checked', temp) ;
  if CertPwPkcs12.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertPwPkcs12_Checked', temp) ;
  if CertWindowsStore.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertWindowsStore_Checked', temp) ;
    end;
    IniFile.UpdateFile;
    IniFile.Free;
end;

procedure TX509CertsForm.AddLog(const S: string);
begin
    try
        if Pos (IcsLF, S) > 0 then
            LogWin.Lines.Text := LogWin.Lines.Text + IcsCRLF + S
        else
           LogWin.Lines.Add (S) ;
        SendMessage(LogWin.Handle, EM_LINESCROLL, 0, 999999);

  { V8.60 write log file }
        if (DirLogs.Text = '') then Exit ;
        if NOT Assigned(FIcsBuffLogStream) then Exit; // sanity check
        FIcsBuffLogStream.WriteLine(S);
    except
    end;
end;

procedure TX509CertsForm.X509Certs1CertProg(Sender: TObject;
  LogOption: TLogOption; const Msg: string);
begin
    AddLog(Msg);
end;


procedure TX509CertsForm.X509Certs1ChallengeDNS(Sender: TObject;
  ChallengeItem: TChallengeItem; var ChlgOK: Boolean);
var
    I, Tot: integer;
    Zone: String;
    WmiDnsRec: TWmiDnsRec;
    WmiDnsRecs: TWmiDnsRecs;
    IpAddrs, Errinfo, SrvName: string;
begin
    ChlgOK := False; 
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

     // check we can talk to DNS server
        Zone := ChallengeItem.CDomain;
        SrvName := IcsWmiChkDnsSrv (DnsNetComputer, DnsNetUserName, DnsNetPassword, IpAddrs, Errinfo) ;
        if (SrvName = '') or (ErrInfo <> '') then begin
            AddLog('Can Not Find Windows DNS Server: ' + Errinfo);
            Exit;  // nothing more we can do
        end;
        AddLog('Windows DNS Server Name: ' + SrvName);
        Tot := IcsWmiListDnsRecs (DnsNetComputer, DnsNetUserName, DnsNetPassword, Zone, '', WmiDnsRecs, Errinfo) ;
        if (Tot = 0) or (Errinfo <> '') then begin
            AddLog('Can Not Find DNS Zone: ' + Errinfo);
            Exit;  // nothing more we can do
        end;

     // now start real work
        if ChallengeItem.CIssueState = IssStateCancel then   // delete DNS
            AddLog('Removing Windows DNS Server TXT Record')
        else
            AddLog('Updating Windows DNS Server TXT Record');
      // strip off wild card prefix
        if Pos ('*.', Zone) = 1 then Zone := Copy(Zone, 3, 99);
        WmiDnsRec.HostName := ChallengeItem.CPage;
        WmiDnsRec.RecType := 'TXT';
        WmiDnsRec.RecData := Trim(ChallengeItem.CDNSValue);
        AddLog('Zone: ' + Zone + ': ' + WmiDnsRec.HostName + ' IN ' +
                                    WmiDnsRec.RecType + ' ' + WmiDnsRec.RecData);
        if ChallengeItem.CIssueState = IssStateCancel then   // delete DNS
        begin
            I := IcsWmiUpdDnsRec (DnsNetComputer, DnsNetUserName, DnsNetPassword,
                                                    Zone, WmiDnsRec, EdtFuncDel, Errinfo);
            if I = 0 then begin
              { V8.65 see if it has gone }
                if IcsWmiChkDnsRec ('', '', '', Zone, WmiDnsRec, Errinfo) then begin
                    AddLog('Failed to Delete DNS Server TXT Record: ' + ErrInfo);
                end
                else begin
                    ChlgOK := True;
                    AddLog('Deleted Windows DNS Server TXT Record OK');
                end;
            end
            else
                AddLog('Failed to Delete DNS Server TXT Record: ' + ErrInfo);
        end
        else begin
            I := IcsWmiUpdDnsRec ('', '', '', Zone, WmiDnsRec, EdtFuncAdd, Errinfo); // leaves old records
            if I = 0 then begin
              { V8.65 see if we can read new record }
                if IcsWmiChkDnsRec (DnsNetComputer, DnsNetUserName,
                                DnsNetPassword, Zone, WmiDnsRec, Errinfo) then begin
                    AddLog('Updated Windows DNS Server TXT Record OK');
                    ChlgOK := True;
                end
                else
                    AddLog('Check Update Windows DNS Server Failed: ' + ErrInfo);
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

procedure TX509CertsForm.X509Certs1ChallengeEmail(Sender: TObject;
  ChallengeItem: TChallengeItem; var ChlgOK: Boolean);
begin
    LabelCertInfo.Caption := 'Email Challenge: ' + icsCRLF +
        'Please collect order once email challenge has been completed for ' +
                                                         ChallengeItem.CDomain;
    ChlgOK := True;
end;

procedure TX509CertsForm.X509Certs1ChallengeFTP(Sender: TObject;
  ChallengeItem: TChallengeItem; var ChlgOK: Boolean);
var
    S: String;
begin
    S := '.Well-Known Challenge: ' + icsCRLF +
        'Use FTP to Copy file: ' + ChallengeItem.CWKFullName +
            ' to server .Well-Known directory for domain: ' + ChallengeItem.CDomain;
    if ChallengeItem.CSupplierProto = SuppProtoAcmeV2 then
        LabelAcme2Info.Caption := S;
    if ChallengeItem.CSupplierProto = SuppProtoCertCentre then
        LabelCertInfo.Caption := S;
    // need some FTP code here
    // !!! must now wait until server is updated, or challenge will fail
    ChlgOK := False;
end;

procedure TX509CertsForm.X509Certs1ChallgRefresh(Sender: TObject);
begin
//
end;

procedure TX509CertsForm.ResetDomButtons;
begin
    doDBCheck.Enabled := False;
    doDBGetChallg.Enabled := False;
    doDBOrder.Enabled := False;
    doDBCollect.Enabled := False;
    doDBRevoke.Enabled := False;
    doDBCancel.Enabled := False;
    doDBRemove.Enabled := False;
    doDBRedist.Enabled := False;
    doDBGetChallg.Enabled := False;
    doDBListChallg.Enabled := False;
    LabelInfoDomain.Caption := 'Order Information:';
end;

function MyDateToStr(DT: TDateTime): String;
begin
    if DT < 10 then
        Result := ''
    else
        Result := DateToStr(DT);
end;

function MyDateTimeToStr(DT: TDateTime): String;
begin
    if DT < 10 then
        Result := ''
    else
        Result := DateTimeToStr(DT);
end;

procedure TX509CertsForm.SetDomButtons;
var
    SelNr: Integer;
    CInfo: String;
begin
    SelNr := DatabaseDomains.ItemIndex;
    ResetDomButtons;
    if SelNr < 0 then Exit;
    if SelNr > Length(X509Certs1.DomainItems) then Exit;
    with X509Certs1.DomainItems[SelNr] do begin
        doDBCheck.Enabled := True;
        doDBRemove.Enabled := True;
        if DIssueState >= IssStateChecked then doDBGetChallg.Enabled := True;
        if DIssueState >= IssStateChallgReq then doDBOrder.Enabled := True;
        if DIssueState >= IssStateChallgPend then begin
            doDBCollect.Enabled := True;
            doDBCancel.Enabled := True;
        end;
     { V8.65 always show what we know }
        CInfo := ChallengeTypeLits[DSuppCertChallg];
        if (DSuppCertChallg in [ChallFileSrv, ChallAlpnSrv]) then
            CInfo := CInfo + ' on ' + DChallgSrvIP;
        LabelInfoDomain.Caption := 'Certificate Common Name Domain: ' + DCommonName + IcsCRLF +
              'Alternate Names: ' + DCertSANs + IcsCRLF +
              'Challenge Type: ' + CInfo + IcsCRLF +
              'Product: ' + DProduct + IcsCRLF +
              'Issue State: ' + IssueStateLits[DIssueState] + IcsCRLF +
              'Issued: ' + MyDateTimeToStr(DStartDT) + IcsCRLF +
              'Expires: ' + MyDateTimeToStr(DEndDT) + IcsCRLF +
              'Web Cert Dir: ' + DDirPubWebCert + IcsCRLF +     { V8.62 }
              'Well-Known Dir: ' + DDirWellKnown + IcsCRLF;    { V8.62 }
        if DIssueState >= IssStateCollect then begin
            doDBRevoke.Enabled := True;
            doDBRedist.Enabled := True;
        end;
    end;
end;

procedure TX509CertsForm.RefreshDomains;
var
    I, Tot: integer;
    OldDom: String;
begin
    Tot := Length(X509Certs1.DomainItems);
    if (Tot = 0) or (DatabaseDomains.Items.Count > Tot) then begin
        DatabaseDomains.Items.Clear;
        ResetDomButtons;
    end
    else begin
        if PageControl1.ActivePage = TabDatabase then
            DatabaseDomains.SetFocus;
        if DatabaseDomains.ItemIndex >= 0 then
            OldDom := DatabaseDomains.Items[DatabaseDomains.ItemIndex].Caption
        else
            OldDom := '';
        while (DatabaseDomains.Items.Count < Tot) do begin
            DatabaseDomains.Items.Add;
            ResetDomButtons;
        end;
        for I := 0 to Tot - 1 do begin
            with X509Certs1.DomainItems[I] do begin
                DatabaseDomains.Items[I].Caption := DCommonName;
                while (DatabaseDomains.Items[I].SubItems.Count) < 7 do
                    DatabaseDomains.Items[I].SubItems.Add('');
                DatabaseDomains.Items[I].SubItems[0] := IssueStateLits[DIssueState];
                DatabaseDomains.Items[I].SubItems[1] := DSuppOrderId;
                DatabaseDomains.Items[I].SubItems[2] := MyDateToStr(DStartDT);
                DatabaseDomains.Items[I].SubItems[3] := MyDateToStr(DEndDT);
                DatabaseDomains.Items[I].SubItems[4] := ChallengeTypeLits[DSuppCertChallg];
                DatabaseDomains.Items[I].SubItems[5] := DProduct;
                DatabaseDomains.Items[I].SubItems[6] := DCertSANs;
                if OldDom = DCommonName then begin
                    DatabaseDomains.Items[I].Selected := True;
                    DatabaseDomains.Items[I].Focused := True;
                end;
            end;
        end;
        SetDomButtons;
    end;
    doCloseDatabase.Enabled := True;
    doDBListChallg.Enabled := (X509Certs1.ChallengesTot <> 0);
end;

procedure TX509CertsForm.X509Certs1DomainsRefresh(Sender: TObject);
begin
    RefreshDomains;
end;

// called when database opened, closed or changes
procedure TX509CertsForm.X509Certs1SuppDBRefresh(Sender: TObject);
var
    S: String;
begin
    if X509Certs1.SupplierProto > SuppProtoNone then begin
        if DirDatabase.Items.IndexOf(DirDatabase.Text) < 0 then
                           DirDatabase.Items.Add(DirDatabase.Text);
        S := 'Account Database Supplier: ' + X509Certs1.SupplierTitle + IcsCRLF +
          'Supplier Protocol: ' + SupplierProtoLits [X509Certs1.SupplierProto] + IcsCRLF;
        LabelDB.Caption := S;
        doCloseDatabase.Enabled := True;
        doDBListChallg.Enabled := (X509Certs1.ChallengesTot <> 0);
    end
    else begin
        LabelDB.Caption := 'No Supplier Account Database Open';
        DatabaseDomains.Items.Clear;
        doCloseDatabase.Enabled := False;
        doDBListChallg.Enabled := False;
    end;
end;

procedure TX509CertsForm.X509Certs1OAuthAuthUrl(Sender: TObject;
  const URL: string);
begin
    { comes here for OAAuthType=OAuthTypeMan generally for windows services,
     we need to ask user to launch the URL, either by sending an email or by
     sending a message to an GUI to launch the browser }
end;

procedure TX509CertsForm.X509Certs1NewCert(Sender: TObject);
begin
    with Sender as TSslX509Certs do begin
        if SupplierProto = SuppProtoAcmeV2 then
            LabelAcme2Info.Caption :=  X509Certs1.GetOrderResult;
        if SupplierProto = SuppProtoCertCentre then
            LabelCertInfo.Caption :=  X509Certs1.GetOrderResult;
    end;
end;

procedure TX509CertsForm.X509Certs1NewToken(Sender: TObject);
begin
    OAuthAccToken.Text := X509Certs1.OAAccToken;
    OAuthRefToken.Text := X509Certs1.OARefreshToken;
    if X509Certs1.OAExpireDT > 10 then
        OAuthExpire.Text := RFC3339_DateToStr(X509Certs1.OAExpireDT)
    else
        OAuthExpire.Text := '';

  // see if repeating CC Profile command, only once
    if FPendCCProfile and (X509Certs1.OAAccToken <> '') then begin
        FPendCCProfile := False;
        doCCProfileClick(Self);
        FPendCCProfile := False;
    end;
end;

procedure TX509CertsForm.ResetButtons;
begin
    doAcmeCheckOrder.Enabled := False;
    doAcmeGetCert.Enabled := False;
    doAcmeGetChallng.Enabled := False;
    doAcmeTestChallng.Enabled := False;
    doAcmeStartChallng.Enabled := False;
    doCertCentreCollect.Enabled := False;
    doCertCentreOrders.Enabled := False;
    doCertCentreCheck.Enabled := False;
end;

procedure TX509CertsForm.SetCommParams;
begin
    OpenLogFile;
    X509Certs1.DebugLevel := THttpDebugLevel(DebugLogging.ItemIndex);
    X509Certs1.LogJson := LogJson.Checked;
    X509Certs1.LogPkeys := LogPkeys.Checked;
    X509Certs1.DomWebSrvIP := DomWebSrvIP.Text;
    X509Certs1.AutoOrderComplete := AutoOrderComplete.Checked;
    X509Certs1.SocketFamily := TSocketFamily(IpSocFamily.ItemIndex); { V8.60 }
    X509Certs1.ProxyURL := ProxyURL.Text;                            { V8.62 }
end;

procedure TX509CertsForm.SetOAParams;
begin
    X509Certs1.OAAccToken := OAuthAccToken.Text;
    X509Certs1.OAAppUrl := OAuthAppUrl.Text;
  // authenication handled internally by launching browser, only works for
  // GUIs, use OAuthTypeMan for services with OAuth1OAuthAuthUrl event
    X509Certs1.OAAuthType := OAuthTypeWeb;
    X509Certs1.OAClientId := OAuthClientId.Text;
    X509Certs1.OAClientSecret := OAuthClientSecret.Text;
    X509Certs1.OAExpireDT := RFC3339_StrToDate(OAuthExpire.Text);
    X509Certs1.OARedirectUrl := OAuthRedirectUrl.Text;
    X509Certs1.OARefrMinsPrior := atoi(OAuthRefrMins.Text);
    X509Certs1.OARefreshAuto := OAuthAutoRefresh.Checked;
    X509Certs1.OARefreshToken := OAuthRefToken.Text; ;
    X509Certs1.OAScope := OAuthScope.Text;
    X509Certs1.OATokenUrl := OAuthTokenUrl.Text;
    X509Certs1.OAWebSrvIP := OAuthWebIP.Text;
    X509Certs1.OAWebSrvPort := OAuthWebPort.Text;
end;


procedure TX509CertsForm.SelCertOldCsrFileClick(Sender: TObject);
var
    FName: String;
begin
    OpenFileDlg.InitialDir := CertOldCsrFile.Text;
    if OpenFileDlg.Execute then begin
        FName := OpenFileDlg.FileName;
        if (FName = '') or (not FileExists(FName)) then Exit;
        CertOldCsrFile.Text := FName;
    end;
end;

procedure TX509CertsForm.SelCertOldPrvKeyClick(Sender: TObject);
var
    FName: String;
begin
    OpenFileDlg.InitialDir := CertOldPrvKey.Text;
    if OpenFileDlg.Execute then begin
        FName := OpenFileDlg.FileName;
        if (FName = '') or (not FileExists(FName)) then Exit;
        CertOldPrvKey.Text := FName;
    end;
end;

procedure TX509CertsForm.SelDirDatabaseClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := DirDatabase.Text ;
    if OpenDirDiag.Execute then
        DirDatabase.Text := ExtractFilePath(OpenDirDiag.FileName);
end;
       
procedure TX509CertsForm.SelDirAcmeConfV2Click(Sender: TObject);
begin
    OpenDirDiag.InitialDir := DirAcmeConfV2.Text ;
    if OpenDirDiag.Execute then
        DirAcmeConfV2.Text := ExtractFilePath(OpenDirDiag.FileName);
end;

procedure TX509CertsForm.SelDirCertCenConfClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := DirCertCenConf.Text ;
    if OpenDirDiag.Execute then
        DirCertCenConf.Text := ExtractFilePath(OpenDirDiag.FileName);
end;

procedure TX509CertsForm.SelDirLogsClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := DirLogs.Text ;
    if OpenDirDiag.Execute then
        DirLogs.Text := ExtractFilePath(OpenDirDiag.FileName);
end;

procedure TX509CertsForm.SelDirPubWebCertClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := DirPubWebCert.Text ;
    if OpenDirDiag.Execute then
        DirPubWebCert.Text := ExtractFilePath(OpenDirDiag.FileName);
end;

procedure TX509CertsForm.SelDirWellKnownClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := DirWellKnown.Text ;
    if OpenDirDiag.Execute then
        DirWellKnown.Text := ExtractFilePath(OpenDirDiag.FileName);
end;

procedure TX509CertsForm.SelOwnCACertDirClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := OwnCACertDir.Text ;
    if OpenDirDiag.Execute then
        OwnCACertDir.Text := ExtractFilePath(OpenDirDiag.FileName);
end;

procedure TX509CertsForm.SelCACertFileClick(Sender: TObject);
var
    FName: String;
begin
    OpenFileDlg.InitialDir := CACertFile.Text;
    if OpenFileDlg.Execute then begin
        FName := OpenFileDlg.FileName;
        if (FName = '') or (not FileExists(FName)) then Exit;
        CACertFile.Text := FName;
    end;
end;

procedure TX509CertsForm.SelCAPkeyFileClick(Sender: TObject);
var
    FName: String;
begin
    OpenFileDlg.InitialDir := CAPkeyFile.Text;
    if OpenFileDlg.Execute then begin
        FName := OpenFileDlg.FileName;
        if (FName = '') or (not FileExists(FName)) then Exit;
        CAPkeyFile.Text := FName;
    end;
end;

procedure TX509CertsForm.SetCertParams;
var
    I, CommRow, NewRow: Integer;
begin
    SetCommParams;
    X509Certs1.CertCsrOrigin := TCertCsrOrigin(CertCsrOrigin.ItemIndex);
    if NOT SelfSignedCA.Checked then
        CertCommonName.Text := IcsLowerCase(Trim(CertCommonName.Text));
    X509Certs1.CertCommonName := '';  // set later, may come from CSR
    X509Certs1.DirWellKnown := DirWellKnown.Text;
    X509Certs1.DirPubWebCert.Text := DirPubWebCert.Text;
    X509Certs1.PrivKeyPassword := PrivKeyPassword.Text;
    X509Certs1.PrivKeyType := TSslPrivKeyType (PrivKeyType.ItemIndex);
    X509Certs1.PrivKeyCipher :=  TSslPrivKeyCipher(PrivKeyCipher.ItemIndex); { V8.62 }
    X509Certs1.CertSignDigestType := DigestDispList[CertSignDigestType.ItemIndex];
    X509Certs1.SuppCertChallenge := TChallengeType(SuppCertChallenge.ItemIndex);
    X509Certs1.CertOldCsrFile := CertOldCsrFile.Text;
    X509Certs1.CertOldPrvKey := CertOldPrvKey.Text;
    X509Certs1.CertApprovEmail := CertCentreApprovEmail.Text;
    X509Certs1.CertSerNumType := TSerNumType(CertSerNumType.ItemIndex);
    X509Certs1.SuppOrderRef := '';
    X509Certs1.SuppOrderId := '';

    X509Certs1.CertOutFmts := [];
  // must have one sensible certificate output format
    if (NOT CertOutFmtBudl.Checked) and (NOT CertOutFmtSep.Checked) and
                     (NOT CertOutFmtP12.Checked) then CertOutFmtSep.Checked := True;
    if CertOutFmtBudl.Checked then X509Certs1.CertOutFmts := X509Certs1.CertOutFmts + [OutFmtBudl];
    if CertOutFmtP12.Checked then X509Certs1.CertOutFmts := X509Certs1.CertOutFmts + [OutFmtP12];
    if CertOutFmtP7.Checked then X509Certs1.CertOutFmts := X509Certs1.CertOutFmts + [OutFmtP7];
    if CertOutFmtReq.Checked then X509Certs1.CertOutFmts := X509Certs1.CertOutFmts + [OutFmtReq];
    if CertOutFmtSep.Checked then X509Certs1.CertOutFmts := X509Certs1.CertOutFmts + [OutFmtSep];
    if CertPwPemKeys.Checked then X509Certs1.CertOutFmts := X509Certs1.CertOutFmts + [OutFmtPwPem];       { V8.67}
    if CertPwPkcs12.Checked then X509Certs1.CertOutFmts := X509Certs1.CertOutFmts + [OutFmtPwP12];         { V8.67}
    if CertWindowsStore.Checked then X509Certs1.CertOutFmts := X509Certs1.CertOutFmts + [OutFmtWinStore]; { V8.67}

 // see if creating own CSR from properties
    if X509Certs1.CertCsrOrigin = CsrOriginProps then begin
        X509Certs1.CertCommonName := CertCommonName.Text;
        X509Certs1.CertOrganization := CertOrganization.Text;
        X509Certs1.CertOrgUnit := CertOrganizationalUnit.Text;
        X509Certs1.SuppOrderId := '';
        X509Certs1.SuppOrderRef := '';
        X509Certs1.CertAddress := CertAddress.Text;
        X509Certs1.CertLocality := CertLocality.Text;
        X509Certs1.CertState := CertState.Text;
        X509Certs1.CertPostCode := CertPostCode.Text;
        X509Certs1.CertCountry := CertCountry.Text;
    //    X509Certs1.CertDescr := CertDescr.Text;
        X509Certs1.CertContactEmail := CertContactEmail.Text;
        X509Certs1.CertContactTitle := CertContactTitle.Text;
        X509Certs1.CertContactFirst := CertContactFirst.Text;
        X509Certs1.CertContactLast := CertContactLast.Text;
        X509Certs1.CertPhone := CertPhone.Text;
        X509Certs1.CertValidity := atoi(CertValidity.Text);

    // build list of SANs from grid
        X509Certs1.CertSubAltNames.Clear;
        CommRow := -1;
        NewRow := -1;
        for I := 1 to CertSANGrid.RowCount - 1 do begin
            CertSANGrid.Cells[0,I] := IcsLowercase(Trim(CertSANGrid.Cells[0,I]));
            if (CertSANGrid.Cells[0,I] <> '') then begin
                if Trim(CertSANGrid.Cells[1,I]) = '' then
                    CertSANGrid.Cells[1,I] := X509Certs1.DirWellKnown;
            end;
            if (CertSANGrid.Cells[0,I] = CertCommonName.Text) then CommRow := I;
            if (CertSANGrid.Cells[0,I] = '') and (NewRow < 0) then NewRow := I;  // first blank row
        end;
    // add common name to SANs if missing, ensure correct workdir
        if (CommRow = -1) and (NewRow >= 1) then begin
            CertSANGrid.Cells[0,NewRow] := CertCommonName.Text;
            CertSANGrid.Cells[1,NewRow] := X509Certs1.DirWellKnown;
            CertSANGrid.Cells[2,NewRow] := X509Certs1.DirPubWebCert[0];
            CertSANGrid.Cells[3,NewRow] := X509Certs1.CertApprovEmail;
        end
        else begin
            if CertSANGrid.Cells[1,CommRow] <> X509Certs1.DirWellKnown then
                        CertSANGrid.Cells[1,CommRow] := X509Certs1.DirWellKnown;
        end;
        for I := 1 to CertSANGrid.RowCount - 1 do begin
            if (CertSANGrid.Cells[0,I] <> '') then begin
                X509Certs1.CertSubAltNames.AddItem(CertSANGrid.Cells[0,I],
                   CertSANGrid.Cells[1,I], CertSANGrid.Cells[2,I], CertSANGrid.Cells[3,I]);
            end;
        end;
    end
    else begin
        if (CertOldCsrFile.Text = '') OR (NOT FileExists(CertOldCsrFile.Text)) then
            AddLog('Must Specify CSR File Name');
    end;
end;

// step 1 - register or open Acme account

procedure TX509CertsForm.doAcmeAccClick(Sender: TObject);
begin
    AddLog('Register or Open ACME Account (1)');
    ResetButtons;
    X509Certs1.CloseAccount;
    SetCommParams;
    X509Certs1.SupplierTitle := 'ACME V2 by Let''s Encrypt';
    X509Certs1.SupplierProto := SuppProtoAcmeV2;
    X509Certs1.SupplierServer := trim(AcmeServerV2.Text);
    DirAcmeConfV2.Text := IncludeTrailingPathDelimiter (DirAcmeConfV2.Text);
    X509Certs1.DirCertWork := DirAcmeConfV2.Text;
    X509Certs1.AcmeAccKeyType := TSslPrivKeyType (AccAcmeKeyV2.ItemIndex);
    X509Certs1.SupplierEmail := SupplierEmail.Text;
    if X509Certs1.SetAcmeAccount(True) then begin
        doAcmeCheckOrder.Enabled := True;
    end;
    X509Certs1SuppDBRefresh(Self);
end;

// step 2 - local checks for challenges, copying and accessing files by URL, setting up DNS, etc

procedure TX509CertsForm.doAcmeCheckOrderClick(Sender: TObject);
begin
    if (X509Certs1.SupplierProto <> SuppProtoAcmeV2) or
                 (X509Certs1.DirCertWork <> DirAcmeConfV2.Text) then begin
        AddLog('Must Register ACME Account First');
        Exit;
    end;
    AddLog('Check ACME Order Locally (2)');
    ResetButtons;
    doAcmeAcc.Enabled := true;
    doAcmeCheckOrder.Enabled := true;
    SetCertParams;
    if X509Certs1.SuppCertChallenge in [ChallFileApp, ChallAlpnApp] then begin
        AddLog('Can Not Place Orders using Application Challenges Here');
        Exit;
    end;
    if NOT X509Certs1.AcmeCheckOrder(True, True) then exit;
    LabelAcme2Info.Caption := 'Let''s Encrypt three month SSL certificate' +
                       icsCRLF + 'Domain(s): ' + X509Certs1.CertSANs.CommaText;
    doAcmeGetChallng.Enabled := true;
    if X509Certs1.IssueState >= IssStateChallgPend then doAcmeGetCert.Enabled := true;
end;

// step 3 - get Acme challenge data, copy files to servers, set-up DNS
// note challenges and results remain valid for one week

procedure TX509CertsForm.doAcmeGetChallngClick(Sender: TObject);
begin
    if (X509Certs1.SupplierProto <> SuppProtoAcmeV2) or
                 (X509Certs1.DirCertWork <> DirAcmeConfV2.Text) then begin
        AddLog('Must Register ACME Account First');
        Exit;
    end;
    if X509Certs1.IssueState < IssStateChecked then begin
        AddLog('Must Check New Certificate First');
        Exit;
    end;
    AddLog('Get ACME Challenges (3)');
    if NOT X509Certs1.AcmeV2GetChallgs then Exit;
    doAcmeTestChallng.Enabled := true;
    if X509Certs1.IssueState >= IssStateChallgWaitTest then doAcmeGetCert.Enabled := true; { V8.65 }
    if X509Certs1.IssueState = IssStateChallgOK then X509Certs1.AcmeV2GetCert;
end;

// step 4 - locally test the Acme challenges work by URL
// we might have manually set-up files or DNS

procedure TX509CertsForm.doAcmeTestChallngClick(Sender: TObject);
begin
    if (X509Certs1.SupplierProto <> SuppProtoAcmeV2) or
                 (X509Certs1.DirCertWork <> DirAcmeConfV2.Text) then begin
        AddLog('Must Register ACME Account First');
        Exit;
    end;
    if X509Certs1.IssueState < IssStateChallgReq then begin
        AddLog('Must Request Acme Challenges First');
        Exit;
    end;
    AddLog('Test ACME Challenges Locally (4)');
    if NOT X509Certs1.AcmeV2TestChallgs then Exit;
    doAcmeStartChallng.Enabled := true;
    if X509Certs1.IssueState >= IssStateChallgPend then doAcmeGetCert.Enabled := true;
    if X509Certs1.IssueState = IssStateChallgOK then X509Certs1.AcmeV2GetCert;
end;

// step 5 - once challenges checked locally, start certificate order process
// so challenges checked by Let's Encrypt and order approved.
// normally runs step 6 automatically unless an error.

procedure TX509CertsForm.doAcmeStartChallngClick(Sender: TObject);
begin
    if (X509Certs1.SupplierProto <> SuppProtoAcmeV2) or
                 (X509Certs1.DirCertWork <> DirAcmeConfV2.Text) then begin
        AddLog('Must Register ACME Account First');
        Exit;
    end;
    if X509Certs1.IssueState < IssStateChallgWaitTest then begin   { V8.65 }
        AddLog('Must Test Acme Challenges First');
        Exit;
    end;
    AddLog('Start ACME Challenges (5)');
    if NOT X509Certs1.AcmeV2StartChallgs then Exit;
    if X509Certs1.IssueState >= IssStateChallgPend then doAcmeGetCert.Enabled := true;
    if X509Certs1.IssueState = IssStateChallgOK then X509Certs1.AcmeV2GetCert;
end;

// step 6 - once order approved, collect the new certificate and save all
// the certificate, private key, intermediate and bundle files.
// Can be repeated if files not saved correctly.

procedure TX509CertsForm.doAcmeGetCertClick(Sender: TObject);
begin
    if (X509Certs1.SupplierProto <> SuppProtoAcmeV2) or
                 (X509Certs1.DirCertWork <> DirAcmeConfV2.Text) then begin
        AddLog('Must Register ACME Account First');
        Exit;
    end;
    if X509Certs1.IssueState < IssStateChallgWaitTest then begin   { V8.65 }
         AddLog('Must Get and Test Acme Challenges First');
        Exit;
    end;
    if X509Certs1.IssueState < IssStateChallgPend then begin
         AddLog('Must Start Acme Order First');
        Exit;
    end;
    AddLog('Collect ACME Certificate (6)');
    X509Certs1.AcmeV2GetCert;
end;

// once an order checked, saves it in the database, normally done automatically.

procedure TX509CertsForm.doAcmeSaveOrderClick(Sender: TObject);
begin
    if (X509Certs1.SupplierProto <> SuppProtoAcmeV2) or
                 (X509Certs1.DirCertWork <> DirAcmeConfV2.Text) then begin
        AddLog('Must Register ACME Account First');
        Exit;
    end;
    if X509Certs1.SupplierProto <> SuppProtoAcmeV2 then begin
        AddLog('Must Register ACME Account First');
        Exit;
    end;
    AddLog('Saving ACME Order');
    SetCertParams;
    if NOT X509Certs1.AcmeCheckOrder(False, True) then exit;
    AddLog('Saved Certificate Order');
end;

procedure TX509CertsForm.doCASignCertClick(Sender: TObject);
begin
    SetCommParams;
    SetCertParams;
    X509Certs1.SupplierProto := SuppProtoOwnCA;
    OwnCACertDir.Text := IncludeTrailingPathDelimiter(OwnCACertDir.Text);
    X509Certs1.DirCertWork := OwnCACertDir.Text;
    X509Certs1.OwnCASign;
end;

procedure TX509CertsForm.doCCProfileClick(Sender: TObject);
begin
    ResetButtons;
    X509Certs1.CloseAccount;
    SetCommParams;
    SetOAParams;
    X509Certs1.SupplierTitle := 'CertCenter AG';
    X509Certs1.SupplierProto := SuppProtoCertCentre;
    X509Certs1.SupplierServer := trim(CertCentreServer.Text);
    DirCertCenConf.Text := IncludeTrailingPathDelimiter (DirCertCenConf.Text);
    X509Certs1.DirCertWork := DirCertCenConf.Text;

    X509Certs1.SetCertCentre(True);
    if X509Certs1.CCGetProfile then begin
        doCertCentreCheck.Enabled := True;
        doCertCentreOrders.Enabled := True;
        doCertCentreCollect.Enabled := True;
        if (CertCentreProducts.Items.Count <> X509Certs1.ProductList.Count) then
            CertCentreProducts.Items.Assign(X509Certs1.ProductList);
        X509Certs1SuppDBRefresh(Self);
    end
    else
        FPendCCProfile := True;  // waiting for oAuth2 response
end;


procedure TX509CertsForm.CertCentreProductsClick(Sender: TObject);
begin
    if (X509Certs1.SupplierProto <> SuppProtoCertCentre) then Exit;
    if CertCentreProducts.ItemIndex < 0 then Exit;
    if X509Certs1.CCGetOneProduct(CertCentreProducts.Items[CertCentreProducts.ItemIndex]) then begin
        LabelCertInfo.Caption := X509Certs1.ProductInfo;
        AddLog(X509Certs1.ProductInfo);
    end;
end;


procedure TX509CertsForm.doCertCentreCheckClick(Sender: TObject);
begin
    if (X509Certs1.SupplierProto <> SuppProtoCertCentre) or
                    (X509Certs1.DirCertWork <> DirCertCenConf.Text) then begin
        AddLog('Must Get CertCentre Profile First');
        Exit;
    end;
    if CertCentreProducts.ItemIndex < 0 then begin
        AddLog('Must Select a Certificate Product First');
        Exit;
    end;
    SetCertParams;
    CertCentreOrderId.Text := '';
    X509Certs1.SuppOrderId := '';
    X509Certs1.SuppOrderRef := CertCentreOrderRef.Text;
    CertCentreProductsClick(Self) ;
    X509Certs1.SuppCertProduct := CertCentreProducts.Items[CertCentreProducts.ItemIndex];
    if X509Certs1.CCCheckOrder(True, True) then begin
        LabelCertInfo.Caption := LabelCertInfo.Caption + IcsCRLF +
            'Domain: ' + X509Certs1.CertCommonName + ', quote price ' + X509Certs1.ProductQuote;
        CertBuy.Enabled := true;
        doCertCentreOther.Enabled := true;
        if X509Certs1.CCGetApproverEmail then begin
            if (CertCentreApprovEmail.Items.Text <> X509Certs1.ApproverEmails.Text) then
                CertCentreApprovEmail.Items.Assign(X509Certs1.ApproverEmails);
            CertCentreApprovEmail.ItemIndex := 0;
        end;
        if X509Certs1.SuppCertChallenge = ChallEmail then begin
            CertCentreApprovEmail.ItemIndex := -1;
            CertCentreApprovEmail.Text := '';
            AddLog('Must Select a Approver Email Address');
        end;
    end;
end;

procedure TX509CertsForm.doCertCentreOtherClick(Sender: TObject);
begin
    if (X509Certs1.SupplierProto <> SuppProtoCertCentre) or
                    (X509Certs1.DirCertWork <> DirCertCenConf.Text) then begin
        AddLog('Must Get CertCentre Profile First');
        Exit;
    end;
    if X509Certs1.IssueState < IssStateChecked then begin
        AddLog('Must Check New Certificate First');
        Exit;
    end;
    if CertBuy.Text <> 'BUY' then begin
        AddLog('!! Must type BUY before ordering commercial certificates that cost money') ;
        exit ;
    end;
    SetCertParams;
    if (CertCentreApprovEmail.Text = '') then begin
         AddLog('Must Select a Approver Email Address First');
         Exit;
    end;
    X509Certs1.SuppOrderId := '';
    CertCentreOrderId.Text := '';
    X509Certs1.SuppOrderRef := CertCentreOrderRef.Text;
    CertBuy.Text := '';
    if X509Certs1.CCOrderCert then begin
        CertCentreOrderId.Text := X509Certs1.SuppOrderId;
        doCertCentreCollect.Enabled := True;
    end;
end;

procedure TX509CertsForm.doCertCentreSaveOrderClick(Sender: TObject);
begin
    if (X509Certs1.SupplierProto <> SuppProtoCertCentre) or
                    (X509Certs1.DirCertWork <> DirCertCenConf.Text) then begin
        AddLog('Must Get CertCentre Profile First');
        Exit;
    end;
    if CertCentreProducts.ItemIndex < 0 then begin
        AddLog('Must Select a Certificate Product First');
        Exit;
    end;
    SetCertParams;
    CertCentreOrderId.Text := '';
    X509Certs1.SuppOrderId := '';
    X509Certs1.SuppOrderRef := CertCentreOrderRef.Text;
    CertCentreProductsClick(Self) ;
    X509Certs1.SuppCertProduct := CertCentreProducts.Items[CertCentreProducts.ItemIndex];
    if NOT X509Certs1.CCCheckOrder(False, True) then Exit;
    AddLog('Saved Certificate Order');
end;

procedure TX509CertsForm.doCertCentreCollectClick(Sender: TObject);
begin
    if X509Certs1.SupplierProto <> SuppProtoCertCentre then begin
        AddLog('Must Get CertCentre Profile First');
        Exit;
    end;
    if CertCentreOrderId.Text = '' then begin
        AddLog('CertCentre Order ID is Required');
        Exit;
    end;
    SetCertParams;
    X509Certs1.SuppOrderId := CertCentreOrderId.Text;
    if X509Certs1.CCGetCert then begin
      //
    end;
end;

procedure TX509CertsForm.doCertCentreOrdersClick(Sender: TObject);
begin
    if X509Certs1.SupplierProto <> SuppProtoCertCentre then begin
        AddLog('Must Get CertCentre Profile First');
        Exit;
    end;
    if X509Certs1.CCListAllOrders then begin
        // should we display a grid ??
    end;
end;


procedure TX509CertsForm.doCheckCSRClick(Sender: TObject);
begin
    if (CertOldCsrFile.Text = '') OR (NOT FileExists(CertOldCsrFile.Text)) then begin
        AddLog('Can Not Find Old Certificate Signing Request File');
        Exit;
    end;
    if (CertOldPrvKey.Text <> '') and (NOT FileExists(CertOldPrvKey.Text)) then begin
        AddLog('Can Not Find Old Private Key File');
        Exit;
    end;
    X509Certs1.CertOldCsrFile := CertOldCsrFile.Text;
    X509Certs1.CertOldPrvKey := CertOldPrvKey.Text;
    X509Certs1.CheckCSR(False);
end;

procedure TX509CertsForm.doClearDomainClick(Sender: TObject);
var
    I, J: Integer;
begin
    ResetButtons;
    CertCommonName.Text := '';
    DirPubWebCert.Text := '';
    DirWellKnown.Text := '';
    for I := 1 to CertSANGrid.RowCount - 1 do begin
        for J := 0 to 3 do
            CertSANGrid.Cells[J,I] := '';
    end;
end;


procedure TX509CertsForm.doOACodeTokenClick(Sender: TObject);
begin
    SetOAParams;
    if OAuthAuthCode.Text = '' then begin
        AddLog('Must Specify OAuth2 Authorization Code');
        Exit;
    end;
    X509Certs1.OAGrantAuthToken(OAuthAuthCode.Text);
end;

procedure TX509CertsForm.doOARefreshNowClick(Sender: TObject);
begin
    SetOAParams;
    if OAuthRefToken.Text = '' then begin
        AddLog('Must Specify OAuth2 Refresh Token');
        Exit;
    end;
    X509Certs1.OAGrantRefresh;
end;

procedure TX509CertsForm.doOpenDatabaseClick(Sender: TObject);
begin
    DatabaseDomains.Items.Clear;
    ResetDomButtons;
    LabelDB.Caption := '';
    DirDatabase.Text := Trim(DirDatabase.Text);
    if DirDatabase.Text = '' then Exit;
    X509Certs1.OpenAccount(DirDatabase.Text, False);
    // result checked in onSuppDBRefresh event
end;

procedure TX509CertsForm.doCloseDatabaseClick(Sender: TObject);
begin
    X509Certs1.CloseAccount;
    ResetDomButtons;
    DatabaseDomains.Items.Clear;
    LabelDB.Caption := '';
    doCloseDatabase.Enabled := False;
end;

procedure TX509CertsForm.DatabaseDomainsClick(Sender: TObject);
begin
    ResetDomButtons;
    if DatabaseDomains.Items.Count = 0 then Exit;
    if DatabaseDomains.ItemIndex < 0 then Exit;
    SetDomButtons;
end;

procedure TX509CertsForm.DatabaseDomainsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
    ResetDomButtons;
    if DatabaseDomains.Items.Count = 0 then Exit;
    if DatabaseDomains.ItemIndex < 0 then Exit;
    SetDomButtons;
end;

procedure TX509CertsForm.DirLogsExit(Sender: TObject);
begin
    OpenLogFile;
end;

procedure TX509CertsForm.doDBCheckClick(Sender: TObject);
var
    SelNr: Integer;
begin
    LabelInfoDomain.Caption := '';
    SelNr := DatabaseDomains.ItemIndex;
    if SelNr < 0 then Exit;
    if SelNr > Length(X509Certs1.DomainItems) then Exit;
    SetCommParams;
    with X509Certs1.DomainItems[SelNr] do begin
        if NOT X509Certs1.CertCheckDomain(DCommonName) then Exit;
        DIssueState := X509Certs1.IssueState;  { V8.65 update so buttons correct }
    end;
    SetDomButtons;
    if X509Certs1.IssueState >= IssStateChecked then doDBGetChallg.Enabled := True;  { V8.64 event not called for check }
end;

procedure TX509CertsForm.doDBGetChallgClick(Sender: TObject);
var
    SelNr: Integer;
begin
    SelNr := DatabaseDomains.ItemIndex;
    if SelNr < 0 then Exit;
    if SelNr > Length(X509Certs1.DomainItems) then Exit;
    SetCommParams;
    with X509Certs1.DomainItems[SelNr] do begin
        if NOT X509Certs1.CertGetChallgDomain(DCommonName) then Exit;
        DIssueState := X509Certs1.IssueState;
    end;
    SetDomButtons;
end;

procedure TX509CertsForm.doDBOrderClick(Sender: TObject);
var
    SelNr: Integer;
begin
    SelNr := DatabaseDomains.ItemIndex;
    if SelNr < 0 then Exit;
    if SelNr > Length(X509Certs1.DomainItems) then Exit;
    SetCommParams;
    with X509Certs1.DomainItems[SelNr] do begin
        if NOT X509Certs1.CertOrderDomain(DCommonName) then Exit;
        DIssueState := X509Certs1.IssueState;
    end;
    SetDomButtons;
end;

procedure TX509CertsForm.doDBCollectClick(Sender: TObject);
var
    SelNr: Integer;
begin
    SelNr := DatabaseDomains.ItemIndex;
    if SelNr < 0 then Exit;
    if SelNr > Length(X509Certs1.DomainItems) then Exit;
    SetCommParams;
    with X509Certs1.DomainItems[SelNr] do begin
        if NOT X509Certs1.CertCollectDomain(DCommonName) then Exit;
        DIssueState := X509Certs1.IssueState;
    end;
    SetDomButtons;
end;

procedure TX509CertsForm.doDBCancelClick(Sender: TObject);
var
    SelNr: Integer;
begin
    SelNr := DatabaseDomains.ItemIndex;
    if SelNr < 0 then Exit;
    if SelNr > Length(X509Certs1.DomainItems) then Exit;
    SetCommParams;
    with X509Certs1.DomainItems[SelNr] do begin
        if NOT X509Certs1.CertCancelDomain(DCommonName) then Exit;
        DIssueState := X509Certs1.IssueState;
    end;
    SetDomButtons;
end;

procedure TX509CertsForm.doDBRedistClick(Sender: TObject);
var
    SelNr: Integer;
begin
    SelNr := DatabaseDomains.ItemIndex;
    if SelNr < 0 then Exit;
    if SelNr > Length(X509Certs1.DomainItems) then Exit;
    SetCommParams;
    with X509Certs1.DomainItems[SelNr] do begin
        if NOT X509Certs1.CertRedistDomain(DCommonName) then Exit;
        DIssueState := X509Certs1.IssueState;
    end;
    SetDomButtons;
end;

procedure TX509CertsForm.doDBRemoveClick(Sender: TObject);
var
    SelNr: Integer;
begin
    SelNr := DatabaseDomains.ItemIndex;
    if SelNr < 0 then Exit;
    if SelNr > Length(X509Certs1.DomainItems) then Exit;
    if Application.MessageBox (PChar('Confirm Delete Order for: ' +
             X509Certs1.DomainItems[SelNr].DCommonName + ' from database'),
                            'Supplier Database', MB_OKCANCEL) <> IDOK then exit  ;
    SetCommParams;
    with X509Certs1.DomainItems[SelNr] do begin
        if NOT X509Certs1.CertRemoveDomain(DCommonName) then Exit;
        DIssueState := X509Certs1.IssueState;
    end;
    SetDomButtons;
end;

procedure TX509CertsForm.doDBRevokeClick(Sender: TObject);
var
    SelNr: Integer;
begin
    SelNr := DatabaseDomains.ItemIndex;
    if SelNr < 0 then Exit;
    if SelNr > Length(X509Certs1.DomainItems) then Exit;
    if Application.MessageBox (PChar ('Confirm Revoke Order for: ' +
            X509Certs1.DomainItems[SelNr].DCommonName + ' from database'),
                            'Supplier Database', MB_OKCANCEL) <> IDOK then exit  ;
    SetCommParams;
    with X509Certs1.DomainItems[SelNr] do begin
        if NOT X509Certs1.CertRevokeDomain(DCommonName) then Exit;
        DIssueState := X509Certs1.IssueState;
    end;
    SetDomButtons;
end;

procedure TX509CertsForm.doDBListChallgClick(Sender: TObject);
var
    J: Integer;
begin
    if (X509Certs1.ChallengesTot = 0) then begin
        AddLog('No Pending Challenges in Database');
        Exit;
    end;
    AddLog('Pending Challenges in Database:');
    with X509Certs1 do begin
        for J:= 0 to Length(ChallengeItems) - 1 do begin
        with ChallengeItems [J] do begin
            if (CDomain = '') then continue;
            AddLog('Domain: ' + CDomain + ', Issue State: ' + IssueStateLits[CIssueState] +
                  ', Challenge Type: ' + ChallengeTypeLits[CType] + ', Proto: ' +
                  SupplierProtoLits[CSupplierProto] +
                  ', Started: ' + MyDateTimeToStr(CStartDT) );
            end;
       end;
   end;
end;

procedure TX509CertsForm.doLoadCAClick(Sender: TObject);
begin
    X509Certs1.CACertFile := CACertFile.Text;
    X509Certs1.CAPkeyFile := CAPkeyFile.Text;
    X509Certs1.CAPkeyPw := CAPkeyPw.Text;
    if NOT X509Certs1.LoadOwnCA then begin
        LabelOwnCA.Caption := 'Failed to load Own CA';
    end
    else begin
        LabelOwnCA.Caption := X509Certs1.NewSslCert.CertInfo(true);
    end;
end;

procedure TX509CertsForm.doSelfSignedClick(Sender: TObject);
begin
    SetCommParams;
    OwnCACertDir.Text := IncludeTrailingPathDelimiter(OwnCACertDir.Text);
    X509Certs1.DirCertWork := OwnCACertDir.Text;
    SetCertParams;
    X509Certs1.SelfSign(SelfSignedCA.Checked);
end;

procedure TX509CertsForm.doTestChallengeClick(Sender: TObject);
begin
    doTestChallenge.Enabled := False;
    SetCommParams;
    X509Certs1.DirCertWork := IncludeTrailingPathDelimiter (DirAcmeConfV2.Text) ;
    SetCertParams;
    X509Certs1.TestAltNameChallgs;
    doTestChallenge.Enabled := True;
end;

end.
