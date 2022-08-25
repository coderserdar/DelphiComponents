{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Creation:     Aug 26, 2003
Description:  A small utility to export SSL certificate from IE certificate
              store to a directory using OpenSSL PEM file format.
              Make use of the ICS Delphi encapsulation for SSLEAY32.DLL &
              LIBEAY32.DLL (OpenSSL) by Francois Piette <francois.piette@overbyte.be>
              Makes use of OpenSSL (http://www.openssl.org)
              Makes use of the Jedi JwaWincrypt.pas (MPL).
Version:      8.69
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2003-2022 by François PIETTE
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
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Aug 26, 2003 V1.01 F. Piette <francois.piette@overbyte.be> added persistance
             to export directory and windows position & size. Also added
             compiler switches and version constant.
Aug 31, 2003 V1.02 func ParseNameProp rewritten.
Sep 04, 2003 V1.03 Added LVCert sort on column header click, and a simple
             application exception handler. Fixed 'CopyCert' bug,
             and beautyfied source.
Sep 11, 2003 V1.04 Test version for new IcsOpenSsl.DLL.
Aug 07, 2007 V1.05 ICS-SSL V6 compatibility
Jun 30, 2008 V1.06 A.Garrels made some changes to prepare SSL code for Unicode.
Jun 30, 2008 V1.07 Some RSA and Blowfish crypto functions.
Jul 14, 2008 V1.08 Paul <paul.blommaerts@telenet.be> added an option to import
             Windows certificates to a single file (CA bundle).
Jul 15, 2008 V1.09 Made one change to prepare SSL code for Unicode.
Jan 29, 2009 V1.10 Removed some string cast warnings.
Dec 20, 2009 V1.11 Memory leak fixed.
Feb 13, 2014 V1.14 Angus using TX509Ex instead of TMyX509 to read PEM entries
             PEM display window now shows all major entries separately as well
                as the raw certificate content.
             ListView now always shows subject common name, and ignore errors.
             Added directory selection buttons (but using Open Dialog for ease).
             Optionally add clear text comments to PEM files to easily identify
             certifcates.
June 23, 2014 V1.15 Angus show issuer Common Name and Organisation Unit in
                    certificate comments
Mar 16, 2015 V8.00 Angus default key length now 2048
June 2015,   V8.01 Angus using new units
Oct 23, 2015 V8.02 Angus get certificate signing and encryption algorithms
Oct 18, 2016 V8.35 Angus, no longer need OverbyteIcsLibeayEx
Nov 15, 2016 V8.38 Angus, only load digitally signed OpenSSL DLLs
                   Added Check Signed button that allows a single file to be
                     selected and it's digital certificate tested
Nov 23, 2016 V8.39 Angus replaced TX509Ex with TX509Base
                   View multiple PEM certificates in a bundle file
Jan 27, 2017 V8.40 Angus display multiple certificate file formats
                   Using new TSslCertTools component to read, create and save
                     certificates, private keys, certificate requests, DHParams,
                     and to sign requests as a certificate authority.
                   This tool can now be used to convert different format certificate
                     files between formats, by reading one format and saving as
                     a different format.  Also combining keys and certificates in a file.
Feb 24, 2017 V8.41 Finished changes for TSslCertTools
                   Simplified creating bundles from Windows with new functions
Apr 20, 2017 V8.46 Force random serial for each new certificate to avoid duplicates
Jun 20, 2017 V8.49 Fixed some missing spaces after : in certificate info listing
                   Clear more certificate fields, specifically DNS names
Sep 22, 2017 V8.50 Corrected X25519 private keys to ED25519, requires OpenSSL 1.1.1
                   Alternate DNS names are now correctly added to requests and certs
                   Specify and save CA Bundle separately to certificate to avoid
                      confusion and needing to repeatedly reload pkey to sign certs
Nov 3, 2017  V8.51 Tested ED25519 keys, can now sign requests and certs
             Added RSA-PSS keys and SHA3 digest hashes, requires OpenSSL 1.1.1
Feb 14, 2018 V8.52 TX509 PublicKey now X509PublicKey
Mar 12, 2018 V8.53 Display Wsocket version in About
Jun 11, 2018 V8.55 don't load OpenSSL in Create
Oct 19, 2018 V8.58 version only
Apr 16, 2019 V8.61 Show certificate expiry and issue time as well as date.
Jul  9, 2019 V8.62 Load several type lists from literals for future proofing.
                   Report ACME Identifier in certificate, if it exists.
Oct 24, 2019 V8.63 Report certificate sha256 fingerprint as well as sha1
May 18, 2020 V8.64  Added support for International Domain Names for Applications (IDNA),
                     i.e. using accents and unicode characters in domain names.
                    X509 certificates always have A-Lavels (Punycode ASCII) domain names,
                      never UTF8 or Unicode.   IDNs are converted back to Unicode
                      for display, but X509 subject remains as A-Labels.
                   Certificate chain validation changed to use TX509List.
                   Alternate Subject IP Addresses are now added correctly to
                      certificates and requests.
Dec 09, 2020 V8.65 Added CA Root Bundle File edit, and List Root Bundle button
                      to list it.
                   Loading PEM bundles ands files now displays better errors for
                       bad certificates.
                   Fixed open diaglog file extensions not set correctly.
Mar 16, 2021 V8.66 Renamed all OpenSSL functions to original names removing ICS
                     f_ prefix.
                   Added support for YuOpenSSL which provides OpenSSL in a pre-built
                     DCU linked into applications, rather than using external DLLs.
Sep 27, 2021 V8.67 OpenSSL 3.0 makes several old ciphers and digests legacy so default
                     for encrypting private key files is now PrivKeyEncAES256 unless
                     support for Windows Server 2012 and early version of Windows 10
                     are needed when 3DES must be used.
                   Display encryption method used for private key when saving them.
                   Added function ResavePrivateKey and Resave Private Key menu option
                     which prompt for a PFX or PEM file containing an encrypted
                     private key -BEGIN ENCRYPTED PRIVATE KEY- with a new cipher,
                     renaming old file to .oldpem/pfx.  Specifically for files saved
                     with old ciphers than OpenSSL 3.0 does not support as standard,
                     should be used with OpenSSL 1.1.1 before upgrading.
                   Tries to load OpenSSL 3.0 legacy provider for old algorithms,
                     About windows shows if it's loaded, without which 3DES password
                     encryption is not available.
                   Displaying certificates and bundles is no longer a new modal window,
                     but updates the existing log window.
                   Improved import certificates from Windows certificate store to use
                     TMsX509List instead of Windows API calls, and to access all Windows
                     store locations instead of just user, specifically the Local Machine
                     store where server certificates are located.
                   New button and list to display the content of any Windows stores using
                     TMsX509List including the private key got My/Personal store, and
                     allowing selected certificates to be loaded for further processing
                     or saving. Displays the Windows friendly name for certificates.
                     Certificates may be deleted from Windows stores.
                   New button and list to display private key stores which are independent
                    of certificates, and to delete private keys from the stores.
                   Added saving a PEM or PFX bundle file to the Windows certificate
                     My/Personal store with private key and intermediates. Certificates
                     can be stored to the users or local machine stores, the latter is
                     required for IIS web server certificates and the application needs
                     administrator rights to do so, labels if admin rights OK.
                   The Create Certificates tab has two new buttons to specify whether
                     PEM and/or PKCS12 private keys should be password protected,
                     previously both or neither were passworded.
Nov 3, 2021  V8.68 Select Microsoft key store, to access smartcard and TPM keys.
May 25, 2022 V8.69 Added new Intermediate CA Certificate bundle file InterCaCertsBundle.pem
                     supplied in the application directory but can be specified as being
                     elsewhere.  At the moment, this is primarily used for OCSP testing
                     of standalone certificates (without a bundle) but can potentially
                     be used to verify certificate chains from servers that don't send
                     the intermediate.  There are new buttons to list the bundle and
                     to add certificates to the bundle.
                   Display Authority Info OCSP and Issuer URLs separately.
                   Added Check OCSP Revoke tick box when displaying certificates
                     that checks the certificate OCSP server using HTTP to ensure
                     it is not revoked or invalid.  Note OCSP will download missing
                     intermediate certificates from the issuer's server using HTTP.
                   Added Test Host Certificates tab that tests SSL/TLS handshake and
                     certificates (using TIcsIpStrmLog) for a list of host names and
                     ports, building a list of host and intermediate certificates that
                     may be further checked and saved as files or added to the
                     intermediate bundle.
                   When searching directory for certificates, ignore Unicode file
                     names on non-unicode compilers.
                   FormDestroy was never called.



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsPemtool1;

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

interface

{$I Include\OverbyteIcsDefs.inc}       { V8.66 }

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, Buttons,
  StdCtrls, OverbyteIcsIniFiles, ComCtrls, Menus, ImgList, ExtCtrls, CommCtrl,
  CheckLst,
{$IF CompilerVersion > 23}
  System.UITypes,
{$IFEND}
  OverByteIcsMimeUtils, OverbyteIcsWSocket,
  OverbyteIcsSsleay, OverbyteIcsLibeay,
  OverbyteIcsWinCrypt, OverbyteIcsMsSslUtils,
  OverbyteIcsUtils, OverbyteIcsSslX509Utils,
  OverbyteIcsLogger,     { for TLogOption }
  OverbyteIcsSslHttpRest,   { V8.69 }
  OverbyteIcsWndControl,
  OverbyteIcsIpStreamLog,
  OverbyteIcsTypes
 {$IFDEF YuOpenSSL}, YuOpenSSL{$ENDIF YuOpenSSL};

const
     PemToolVersion     = 869;
     PemToolDate        = 'May 25, 2022';
     PemToolName        = 'PEM Certificate Tool';
     CopyRight : String = '(c) 2003-2022 by François PIETTE V8.69 ';
     CaptionMain        = 'ICS PEM Certificate Tool - ' + PemToolDate + ' - ';
     WM_APPSTARTUP      = WM_USER + 1;
     WM_IPLOGCONNECT    = WM_USER + 2;
     WM_IPLOGDISCONNECT = WM_USER + 3;

type
  TfrmPemTool1 = class(TForm)
    CAFilesDir: TComboBox;
    CertAddComment: TCheckBox;
    CertAltDomains: TMemo;
    CertAltIPs: TMemo;
    CertCommonName: TEdit;
    CertCountry: TEdit;
    CertDays: TEdit;
    CertDescr: TEdit;
    CertEMail: TEdit;
    CertExtClient: TCheckBox;
    CertExtCodeSign: TCheckBox;
    CertExtEmail: TCheckBox;
    CertExtServer: TCheckBox;
    CertLocality: TEdit;
    CertOrganization: TEdit;
    CertOrganizationalUnit: TEdit;
    CertPassword: TEdit;
    CertSignHash: TRadioGroup;
    CertState: TEdit;
    CertUsageCRLSign: TCheckBox;
    CertUsageCertSign: TCheckBox;
    CertUsageDataEn: TCheckBox;
    CertUsageDigSign: TCheckBox;
    CertUsageKeyAgree: TCheckBox;
    CertUsageKeyEn: TCheckBox;
    CertUsageNonRepud: TCheckBox;
    CheckBoxComment: TCheckBox;
    CheckBoxEmptyDestDir: TCheckBox;
    CheckBoxOverwriteExisting: TCheckBox;
    CheckBoxWarnDestNotEmpty: TCheckBox;
    CheckBoxWriteToBundle: TCheckBox;
    CurrentCertDirEdit: TEdit;
    DHParamFile: TEdit;
    DHParamSize: TRadioGroup;
    DestDirEdit: TEdit;
    KeyEncrypt: TRadioGroup;
    KeyType: TRadioGroup;
    LoadCertFile: TEdit;
    LoadCertInters: TCheckBox;
    LoadCertPrivKey: TCheckBox;
    LoadCertPW: TEdit;
    LoadDirectory: TEdit;
    LoadInterCerts: TEdit;
    LoadPrivatetKey: TEdit;
    LoadRequestFile: TEdit;
    NewCertCopyExt: TCheckBox;
    SaveAutoReplace: TCheckBox;
    SaveCertDer: TEdit;
    SaveCertPW: TEdit;
    SaveCertPem: TEdit;
    SaveDirectory: TEdit;
    SaveInterCerts: TCheckBox;
    SavePkcs12File: TEdit;
    SavePkcs7File: TEdit;
    SavePrivateKey: TCheckBox;
    SavePrvFileFile: TEdit;
    SavePubKeyFile: TEdit;
    SaveReqCertFile: TEdit;
    LoadCaBundleFile: TEdit;
    MsStoreType: TRadioGroup;       { V8.67 }
    MsLocationType: TRadioGroup;    { V8.67 }
    MSBriefList: TCheckBox;         { V8.67 }
    MsInstallFile: TEdit;           { V8.67 }
    CertPwPemKeys: TCheckBox;       { V8.67 }
    CertPwPkcs12: TCheckBox;        { V8.67 }
    CertCheckOCSP: TCheckBox;       { V8.69 }
    IntersCAFile: TEdit;            { V8.69 }
    TestHostList: TMemo;            { V8.69 }
    TestHostOne: TEdit;             { V8.69 }
    DownCertsPath: TEdit;
    DownIntersPath: TEdit;           { V8.69 }

// following not saved
    pmLv: TPopupMenu;
    pmShowDetails: TMenuItem;
    pmDelete: TMenuItem;
    ImageList1: TImageList;
    OpenDlg: TOpenDialog;
    PageControl1: TPageControl;
    TabCertsList: TTabSheet;
    TabWindowsStore: TTabSheet;
    LvCerts: TListView;
    btnRefresh: TButton;
    Label4: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    btnImport: TButton;
    btnDeleteCert: TButton;
    btnCopyCert: TButton;
    pmCopy: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    MainMenu1: TMainMenu;
    MMFile: TMenuItem;
    MMFileExit: TMenuItem;
    MMExtras: TMenuItem;
    MMExtrasCreateSelfSignedCert: TMenuItem;
    MMExtrasCreateCertRequest: TMenuItem;
    MMExtrasEncryptStringRSA: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    MMExtrasEncryptStringBlowfish: TMenuItem;
    MMExtrasEncryptStreamBlowfish: TMenuItem;
    ProgressBar1: TProgressBar;
    MMExtrasEncryptFileBlowfish: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    MMExtrasDecryptFileBlowfish: TMenuItem;
    OpenDirDiag: TOpenDialog;
    SelCurrDir: TBitBtn;
    SelImpDir: TBitBtn;
    About1: TMenuItem;
    TabCreateCerts: TTabSheet;
    btnShowBundleFile: TButton;
    btnShowOneFile: TButton;
    Panel1: TPanel;
    Label8: TLabel;
    BoxLoadCert: TGroupBox;
    TabNewCertProps: TTabSheet;
    GroupBoxCertCreate: TGroupBox;
    lbCountry: TLabel;
    lbState: TLabel;
    lbLocality: TLabel;
    lbOrganization: TLabel;
    lbOrganizationalUnit: TLabel;
    lbCommonName: TLabel;
    lbEMail: TLabel;
    lbDays: TLabel;
    SelLoadDir: TBitBtn;
    CertLinesOld: TMemo;
    doLoadCert: TButton;
    BoxCertProc: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    SelCertFile: TBitBtn;
    SelPrvKeyFile: TBitBtn;
    SelReqFile: TBitBtn;
    doLoadPrvKey: TButton;
    doLoadReq: TButton;
    doLoadBase64: TButton;
    Label13: TLabel;
    LabelStateCert: TLabel;
    LabelStateReq: TLabel;
    doClearCerts: TButton;
    GroupKeys: TGroupBox;
    Label14: TLabel;
    LabelStatePrivKey: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    KeyPairLines: TMemo;
    doGenKey: TButton;
    DHParamsLines: TMemo;
    doDHParams: TButton;
    doCreateReqProps: TButton;
    doCreateReqCert: TButton;
    doCreateSelfCert: TButton;
    doCreateCACert: TButton;
    BoxCertSave: TGroupBox;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label22: TLabel;
    SelSaveDir: TBitBtn;
    doSaveCertPem: TButton;
    doSaveCertDer: TButton;
    doSaveReqCert: TButton;
    LabelStateCACert: TLabel;
    Label21: TLabel;
    Label23: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    doLoadInters: TButton;
    SelIntersFile: TBitBtn;
    Label27: TLabel;
    LabelInters: TLabel;
    doSavePkcs12: TButton;
    doSavePkcs7Cert: TButton;
    doSavePrivKey: TButton;
    doSavePubKey: TButton;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    GroupBox1: TGroupBox;
    CertIsCA: TCheckBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupDHParam: TGroupBox;
    CertLinesNew: TMemo;
    Panel2: TPanel;
    Status: TLabel;
    doCreateBundle: TButton;
    doCheckBundleWin: TButton;
    doCheckBundleSelf: TButton;
    Label33: TLabel;
    SelCAFile: TBitBtn;
    doLoadCABundle: TButton;
    doListRoots: TButton;
    MMResaveKey: TMenuItem;
    BoxSigning: TGroupBox;
    btnCheckSigned: TButton;
    BoxMisc: TGroupBox;
    btnImportPemFile: TButton;
    BoxStoreTools: TGroupBox;
    doMsDisplay: TButton;
    CertStoreList: TListBox;
    BoxStoreImport: TGroupBox;
    BoxStoreInstall: TGroupBox;
    Label3: TLabel;
    SelInstallFile: TBitBtn;
    doMsInstallFile: TButton;
    doLoadMOne: TButton;
    doLoadtoCreate: TButton;
    LabelAdminRights: TLabel;
    doListPkeys: TButton;
    PkeyList: TListBox;
    doKeyStoreDel: TButton;
    doCertStoreDel: TButton;
    MSKeyStoreType: TRadioGroup;
    TabTestHosts: TTabSheet;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label32: TLabel;
    doTestOneCert: TButton;
    doTestListCerts: TButton;
    IcsIpClient: TIcsIpStrmLog;
    Label34: TLabel;
    SelCARoots: TBitBtn;
    Label24: TLabel;
    SelIntersCA: TBitBtn;
    DownloadCerts: TCheckListBox;
    DownloadInters: TCheckListBox;
    Label35: TLabel;
    SelDownHostPath: TBitBtn;
    Label36: TLabel;
    SelDownInters: TBitBtn;
    doHostSave: TButton;
    doSaveInters: TButton;
    doAddInterBundle: TButton;
    Label37: TLabel;
    doTestAbort: TButton;
    doListInters: TButton;


    procedure btnImportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnRefreshClick(Sender: TObject);
    procedure LvCertsDblClick(Sender: TObject);
    procedure btnShowBundleFileClick(Sender: TObject);
    procedure CurrentCertDirEditChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure btnDeleteCertClick(Sender: TObject);
    procedure btnCopyCertClick(Sender: TObject);
    procedure DestDirEditChange(Sender: TObject);
    procedure btnImportPemFileClick(Sender: TObject);
    procedure LvCertsColumnClick(Sender: TObject; Column: TListColumn);
    procedure LvCertsCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure LvCertsCustomDraw(Sender: TCustomListView;
      const ARect: TRect; var DefaultDraw: Boolean);
    procedure AppOnException(Sender: TObject; E: Exception);
    procedure MMFileExitClick(Sender: TObject);
    procedure MMExtrasCreateSelfSignedCertClick(Sender: TObject);
    procedure MMExtrasCreateCertRequestClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MMExtrasEncryptStringRSAClick(Sender: TObject);
    procedure MMExtrasEncryptStringBlowfishClick(Sender: TObject);
    procedure MMExtrasEncryptStreamBlowfishClick(Sender: TObject);
    procedure MMExtrasEncryptFileBlowfishClick(Sender: TObject);
    procedure MMExtrasDecryptFileBlowfishClick(Sender: TObject);
    procedure SelCurrDirClick(Sender: TObject);
    procedure SelImpDirClick(Sender: TObject);
    procedure btnCheckSignedClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure btnShowOneFileClick(Sender: TObject);
    procedure SelLoadDirClick(Sender: TObject);
    procedure SelCertFileClick(Sender: TObject);
    procedure SelPrvKeyFileClick(Sender: TObject);
    procedure SelReqFileClick(Sender: TObject);
    procedure doLoadCertClick(Sender: TObject);
    procedure doLoadPrvKeyClick(Sender: TObject);
    procedure doLoadReqClick(Sender: TObject);
    procedure SelIntersFileClick(Sender: TObject);
    procedure doLoadBase64Click(Sender: TObject);
    procedure doClearCertsClick(Sender: TObject);
    procedure doCreateReqPropsClick(Sender: TObject);
    procedure doCreateReqCertClick(Sender: TObject);
    procedure doCreateSelfCertClick(Sender: TObject);
    procedure doCreateCACertClick(Sender: TObject);
    procedure SelSaveDirClick(Sender: TObject);
    procedure doSaveCertPemClick(Sender: TObject);
    procedure doSaveCertDerClick(Sender: TObject);
    procedure doSaveReqCertClick(Sender: TObject);
    procedure doSavePkcs12Click(Sender: TObject);
    procedure doSavePkcs7CertClick(Sender: TObject);
    procedure doSavePrivKeyClick(Sender: TObject);
    procedure doSavePubKeyClick(Sender: TObject);
    procedure CAFilesDirClick(Sender: TObject);
    procedure SelCertsDBClick(Sender: TObject);
    procedure doGenKeyClick(Sender: TObject);
    procedure doDHParamsClick(Sender: TObject);
    procedure doLoadIntersClick(Sender: TObject);
    procedure doCreateBundleClick(Sender: TObject);
    procedure doCheckBundleWinClick(Sender: TObject);
    procedure doCheckBundleSelfClick(Sender: TObject);
    procedure SelCAFileClick(Sender: TObject);
    procedure doLoadCABundleClick(Sender: TObject);
    procedure doListRootsClick(Sender: TObject);
    procedure MMResaveKeyClick(Sender: TObject);
    procedure doMsDisplayClick(Sender: TObject);
    procedure SelInstallFileClick(Sender: TObject);
    procedure doMsInstallFileClick(Sender: TObject);
    procedure doLoadMOneClick(Sender: TObject);
    procedure doLoadtoCreateClick(Sender: TObject);
    procedure doListPkeysClick(Sender: TObject);
    procedure doCertStoreDelClick(Sender: TObject);
    procedure doKeyStoreDelClick(Sender: TObject);
    procedure doTestOneCertClick(Sender: TObject);
    procedure IcsIpClientLogHandshakeDone(Sender: TObject; ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
    procedure IcsIpClientLogProgEvent(Sender: TObject; Socnr: Integer; LogOption: TLogOption; const Msg: string);
    procedure IcsIpClientLogRecvEvent(Sender: TObject; Socnr: Integer; const Line: string);
    procedure IcsIpClientLogChangeEvent(Sender: TObject; Socnr: Integer; LogState: TStrmLogState);
    procedure doTestListCertsClick(Sender: TObject);
    procedure SelIntersCAClick(Sender: TObject);
    procedure DownloadCertsDblClick(Sender: TObject);
    procedure DownloadIntersDblClick(Sender: TObject);
    procedure doTestAbortClick(Sender: TObject);
    procedure SelDownHostPathClick(Sender: TObject);
    procedure SelDownIntersClick(Sender: TObject);
    procedure doHostSaveClick(Sender: TObject);
    procedure doSaveIntersClick(Sender: TObject);
    procedure doAddInterBundleClick(Sender: TObject);
    procedure doListIntersClick(Sender: TObject);
  protected
    procedure WMAppStartup(var Msg: TMessage); message WM_APPSTARTUP;
    procedure WMIpLogConnect(var Msg: TMessage); message WM_IPLOGCONNECT;
    procedure WMIpLogDisconnect(var Msg: TMessage); message WM_IPLOGDISCONNECT;
  private
    FProgDir         : String;
    FInitialized     : Boolean;
    FCurrentCertDir  : String;
    FLVSortFlag      : Boolean;
    FSslCertTools    : TSslCertTools;
    FSslCAX509       : TX509Base;    // V8.50
    FMsX509List      : TMsX509List;      // V8.67
    FStorePkeyTot    : Integer;          // V8.67
    FStorePkeyInfos  : TStorePkeyInfos;  // V8.67
    procedure AddListView(X: TX509Base; const Filename: String);
    procedure FillListView;
    procedure ShowBundleCerts(const FileName: String);
    procedure ShowOneCert(const FileName: String);
    function  BuildLoadName(const fname: string): string;
    function  BuildSaveName(const fname: string): string;
    procedure SetCertProps;
    procedure ToolsOKeyProgress(Sender: TObject);
    procedure DispError(const Err: String);
    procedure DispCert;
    procedure DispPKey;
    procedure DispReq;
    procedure DispInter;
    procedure DispCACert;
    procedure ShowLogWindow;
    procedure LoadRootCA;                       { V8.65 }
    function  ListOcspStatus(Cert: TX509Base): string;             { V8.69 }
    procedure AddLog (const S: string) ;                                              { V8.69 }
    procedure LogEvent(Sender: TObject; LogOption: TLogOption; const Msg: string);
    procedure LoadInterCA;                             { V8.69 }
    procedure TestHostCert(const HostPort: String);    { V8.69 }
  public
    FIniFileName    : String;
    FTestHostTot    : Integer;
    FTestHostCur    : Integer;
//    LogWinOpen      : Boolean;                { V8.67 always created }
  end;

  function  FindPemFileName(const FileName: String): String;
  function  DirectoryExists(const Name: string): Boolean;
  function  IsDirEmpty(const Path: String): Boolean;
  function  PathAddBackSlash(const Path: String): String;
  procedure EmptyDirectory(Path: String);
  function  ResavePrivateKey(const FName, PW: String; KeyCipher: TSslPrivKeyCipher;
                                                var ErrMsg: String): Boolean;   { V8.67 }

var
  frmPemTool1 : TfrmPemTool1;
  ColumnToSort: Integer;
  VerifyDir: String;
  StartTickCount: integer;
  MsCertChainEngine: TMsCertChainEngine;   { V8.41 }
  RootCAList: TX509List;       { V8.65 }
  InterCAList: TX509List;      { V8.69 }
  OcspHttp: TOcspHttp;         { V8.69 }
  DownCertsList: TX509List;    { V8.69 }
  DownIntersList: TX509List;   { V8.69 }
  ClosingFlag: Boolean;        { V8.69 }

implementation

{$R *.DFM}

uses
    OverByteIcsPemTool2, OverByteIcsPemTool3;

const
    SectionMainWindow    = 'MainWindow';
    SectionDisplayWindow = 'DisplayWindow';
    KeyTop               = 'Top';
    KeyLeft              = 'Left';
    KeyWidth             = 'Width';
    KeyHeight            = 'Height';
    SectionData          = 'Data';
    KeyVerifyDir         = 'VerifyDir';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.AppOnException(Sender: TObject; E: Exception);
begin
    if MessageDlg(E.ClassName + ': ' + E.Message + #13#10
                + 'Exit PemTool now?',
                   mtError, [mbYes, mbNo], 0) = mrYes then
        Application.Terminate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.DispError(const Err: String);  { V8.40 }
begin
    Status.Caption := StringReplace (Err, #13#10, ' ', [rfReplaceAll]);
    beep;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.ToolsOKeyProgress(Sender: TObject);
begin
    Application.ProcessMessages;
    if StartTickCount = 0 then Exit;
    Status.Caption := 'Generating prime numbers: ' + IntToStr(IcsCalcTickDiff
                            (StartTickCount, GetTickCount) div 1000) + ' secs';
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.AddLog (const S: string) ;                                              { V8.69 }
begin
    if ClosingFlag then Exit;
    frmPemTool2.Memo1.Lines.Add (S);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.LogEvent(Sender: TObject; LogOption: TLogOption; const Msg: string);    { V8.69 }
begin
    AddLog(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.FormCreate(Sender: TObject);
var
    I: Integer;
    KC: TSslPrivKeyCipher;
    CS: TMsCertStoreType;
    CL: TMsCertLocation;
    KP: TMsKeyProvider;
begin
{$IF CompilerVersion > 17}
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
{$IFEND}
    Application.OnException := AppOnException;
    FProgDir     := ExtractFilePath(ParamStr(0));
    FIniFileName := GetIcsIniFileName;
    MsStoreType.ItemIndex := 0;
 // Avoid dynamical loading and unloading the SSL DLLs plenty of times
//  GSSLEAY_DLL_IgnoreNew := True;     { ignore OpenSSL 3.0 and later }
//  GSSLEAY_DLL_IgnoreOld := True;     { ignore OpenSSL 1.1 }
// note both not allowed true
    GSSL_DLL_DIR := FProgDir;        { V8.38 only from our directory }
    GSSL_SignTest_Check := True;     { V8.38 check digitally signed }
    GSSL_SignTest_Certificate := True; { V8.38 check digital certificate }
    GSSLEAY_LOAD_LEGACY := True;     { V8.67 OpenSSL 3.0 legacy provider for old algorithms }
    OpenDlg.Filter := SslCertFileOpenExts;    { V8.62 }
   {  'Certs *.pem;*.cer;*.crt;*.der;*.p12;*.pfx;*.p7*;*.spc|' +
                            '*.pem;*.cer;*.crt;*.der;*.p12;*.pfx;*.p7*;*.spc|' +
                            'All Files *.*|*.*'; }
    CertSignHash.Items.Clear;
    for I := 0 to DigestListLitsLast do
      CertSignHash.Items.Add(DigestListLits[I]);    { V8.62 }
    CertSignHash.ItemIndex := 0;
    KeyType.Items.Clear;
    for I := 0 to SslPrivKeyTypeLitsLast2 do
        KeyType.Items.Add(SslPrivKeyTypeLits[I]);     { V8.62 }
    KeyType.ItemIndex := 0;
    KeyEncrypt.Items.Clear;
    for KC := Low(TSslPrivKeyCipher) to High(TSslPrivKeyCipher) do
        KeyEncrypt.Items.Add(SslPrivKeyCipherLits[KC]);     { V8.62 }
    KeyEncrypt.ItemIndex := 0;
    MsStoreType.Items.Clear;
    for CS := Low(TMsCertStoreType) to High(TMsCertStoreType) do
        MsStoreType.Items.Add(MsCertStoreTitles[CS]);        { V8.67 }
    MsStoreType.ItemIndex := 0;
    MsLocationType.Items.Clear;
    for CL := Low(TMsCertLocation) to High(TMsCertLocation) do
        MsLocationType.Items.Add(MsCertLocTitles[CL]);        { V8.67 }
    MsLocationType.ItemIndex := 0;
    MSKeyStoreType.Items.Clear;
    for KP := Low(TMsKeyProvider) to High(TMsKeyProvider) do
        MSKeyStoreType.Items.Add(MsKeyProviderTitles[KP]);        { V8.68 }
    MSKeyStoreType.ItemIndex := 0;
    if IcsIsProgAdmin then      { V8.67 }
        LabelAdminRights.Caption := 'Local Machine store has administrator rights'
    else
        LabelAdminRights.Caption := 'Local Machine store needs administrator rights';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.FormDestroy(Sender: TObject);
begin
    FreeAndNil (MsCertChainEngine) ;
    OverbyteIcsWSocket.UnLoadSsl;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile      := TIcsIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionMainWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionMainWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionMainWindow, KeyTop, (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionMainWindow, KeyLeft, (Screen.Width  - Width)  div 2);
        VerifyDir := IniFile.ReadString(SectionData, KeyVerifyDir, FProgDir); { V8.38 }
        with IniFile do begin   { V8.40 }
          CAFilesDir.Text := ReadString (SectionData, 'CAFilesDir_Text', CAFilesDir.Text) ;
          if ReadString (SectionData, 'CertAddComment_Checked', 'False') = 'True' then CertAddComment.Checked := true else CertAddComment.Checked := false ;
          CertAltDomains.Lines.CommaText := ReadString (SectionData, 'CertAltDomains_CommaText', '') ;
          CertAltIPs.Lines.CommaText := ReadString (SectionData, 'CertAltIPs_CommaText', '') ;
          CertCommonName.Text := ReadString (SectionData, 'CertCommonName_Text', CertCommonName.Text) ;
          CertCountry.Text := ReadString (SectionData, 'CertCountry_Text', CertCountry.Text) ;
          CertDays.Text := ReadString (SectionData, 'CertDays_Text', CertDays.Text) ;
          CertDescr.Text := ReadString (SectionData, 'CertDescr_Text', CertDescr.Text) ;
          CertEMail.Text := ReadString (SectionData, 'CertEMail_Text', CertEMail.Text) ;
          if ReadString (SectionData, 'CertExtClient_Checked', 'False') = 'True' then CertExtClient.Checked := true else CertExtClient.Checked := false ;
          if ReadString (SectionData, 'CertExtCodeSign_Checked', 'False') = 'True' then CertExtCodeSign.Checked := true else CertExtCodeSign.Checked := false ;
          if ReadString (SectionData, 'CertExtEmail_Checked', 'False') = 'True' then CertExtEmail.Checked := true else CertExtEmail.Checked := false ;
          if ReadString (SectionData, 'CertExtServer_Checked', 'False') = 'True' then CertExtServer.Checked := true else CertExtServer.Checked := false ;
          CertLocality.Text := ReadString (SectionData, 'CertLocality_Text', CertLocality.Text) ;
          CertOrganization.Text := ReadString (SectionData, 'CertOrganization_Text', CertOrganization.Text) ;
          CertOrganizationalUnit.Text := ReadString (SectionData, 'CertOrganizationalUnit_Text', CertOrganizationalUnit.Text) ;
          CertPassword.Text := ReadString (SectionData, 'CertPassword_Text', CertPassword.Text) ;
          CertSignHash.ItemIndex := ReadInteger (SectionData, 'CertSignHash_ItemIndex', CertSignHash.ItemIndex) ;
          CertState.Text := ReadString (SectionData, 'CertState_Text', CertState.Text) ;
          if ReadString (SectionData, 'CertUsageCRLSign_Checked', 'False') = 'True' then CertUsageCRLSign.Checked := true else CertUsageCRLSign.Checked := false ;
          if ReadString (SectionData, 'CertUsageCertSign_Checked', 'False') = 'True' then CertUsageCertSign.Checked := true else CertUsageCertSign.Checked := false ;
          if ReadString (SectionData, 'CertUsageDataEn_Checked', 'False') = 'True' then CertUsageDataEn.Checked := true else CertUsageDataEn.Checked := false ;
          if ReadString (SectionData, 'CertUsageDigSign_Checked', 'False') = 'True' then CertUsageDigSign.Checked := true else CertUsageDigSign.Checked := false ;
          if ReadString (SectionData, 'CertUsageKeyAgree_Checked', 'False') = 'True' then CertUsageKeyAgree.Checked := true else CertUsageKeyAgree.Checked := false ;
          if ReadString (SectionData, 'CertUsageKeyEn_Checked', 'False') = 'True' then CertUsageKeyEn.Checked := true else CertUsageKeyEn.Checked := false ;
          if ReadString (SectionData, 'CertUsageNonRepud_Checked', 'False') = 'True' then CertUsageNonRepud.Checked := true else CertUsageNonRepud.Checked := false ;
          if ReadString (SectionData, 'CheckBoxComment_Checked', 'False') = 'True' then CheckBoxComment.Checked := true else CheckBoxComment.Checked := false ;
          if ReadString (SectionData, 'CheckBoxEmptyDestDir_Checked', 'False') = 'True' then CheckBoxEmptyDestDir.Checked := true else CheckBoxEmptyDestDir.Checked := false ;
          if ReadString (SectionData, 'CheckBoxOverwriteExisting_Checked', 'False') = 'True' then CheckBoxOverwriteExisting.Checked := true else CheckBoxOverwriteExisting.Checked := false ;
          if ReadString (SectionData, 'CheckBoxWarnDestNotEmpty_Checked', 'False') = 'True' then CheckBoxWarnDestNotEmpty.Checked := true else CheckBoxWarnDestNotEmpty.Checked := false ;
          if ReadString (SectionData, 'CheckBoxWriteToBundle_Checked', 'False') = 'True' then CheckBoxWriteToBundle.Checked := true else CheckBoxWriteToBundle.Checked := false ;
          CurrentCertDirEdit.Text := ReadString (SectionData, 'CurrentCertDirEdit_Text', CurrentCertDirEdit.Text) ;
          DHParamFile.Text := ReadString (SectionData, 'DHParamFile_Text', DHParamFile.Text) ;
          DHParamSize.ItemIndex := ReadInteger (SectionData, 'DHParamSize_ItemIndex', DHParamSize.ItemIndex) ;
          DestDirEdit.Text := ReadString (SectionData, 'DestDirEdit_Text', DestDirEdit.Text) ;
          KeyEncrypt.ItemIndex := ReadInteger (SectionData, 'KeyEncrypt_ItemIndex', KeyEncrypt.ItemIndex) ;
          KeyType.ItemIndex := ReadInteger (SectionData, 'KeyType_ItemIndex', KeyType.ItemIndex) ;
          LoadCertFile.Text := ReadString (SectionData, 'LoadCertFile_Text', LoadCertFile.Text) ;
          if ReadString (SectionData, 'LoadCertInters_Checked', 'False') = 'True' then LoadCertInters.Checked := true else LoadCertInters.Checked := false ;
          if ReadString (SectionData, 'LoadCertPrivKey_Checked', 'False') = 'True' then LoadCertPrivKey.Checked := true else LoadCertPrivKey.Checked := false ;
          LoadCertPW.Text := ReadString (SectionData, 'LoadCertPW_Text', LoadCertPW.Text) ;
          LoadDirectory.Text := ReadString (SectionData, 'LoadDirectory_Text', LoadDirectory.Text) ;
          LoadInterCerts.Text := ReadString (SectionData, 'LoadInterCerts_Text', LoadInterCerts.Text) ;
          LoadPrivatetKey.Text := ReadString (SectionData, 'LoadPrivatetKey_Text', LoadPrivatetKey.Text) ;
          LoadRequestFile.Text := ReadString (SectionData, 'LoadRequestFile_Text', LoadRequestFile.Text) ;
          if ReadString (SectionData, 'NewCertCopyExt_Checked', 'False') = 'True' then NewCertCopyExt.Checked := true else NewCertCopyExt.Checked := false ;
          if ReadString (SectionData, 'SaveAutoReplace_Checked', 'False') = 'True' then SaveAutoReplace.Checked := true else SaveAutoReplace.Checked := false ;
          SaveCertDer.Text := ReadString (SectionData, 'SaveCertDer_Text', SaveCertDer.Text) ;
          SaveCertPem.Text := ReadString (SectionData, 'SaveCertPem_Text', SaveCertPem.Text) ;
          SaveCertPW.Text := ReadString (SectionData, 'SaveCertPW_Text', SaveCertPW.Text) ;
          SaveDirectory.Text := ReadString (SectionData, 'SaveDirectory_Text', SaveDirectory.Text) ;
          if ReadString (SectionData, 'SaveInterCerts_Checked', 'False') = 'True' then SaveInterCerts.Checked := true else SaveInterCerts.Checked := false ;
          SavePkcs12File.Text := ReadString (SectionData, 'SavePkcs12File_Text', SavePkcs12File.Text) ;
          SavePkcs7File.Text := ReadString (SectionData, 'SavePkcs7File_Text', SavePkcs7File.Text) ;
          if ReadString (SectionData, 'SavePrivateKey_Checked', 'False') = 'True' then SavePrivateKey.Checked := true else SavePrivateKey.Checked := false ;
          SavePrvFileFile.Text := ReadString (SectionData, 'SavePrvFileFile_Text', SavePrvFileFile.Text) ;
          SavePubKeyFile.Text := ReadString (SectionData, 'SavePubKeyFile_Text', SavePubKeyFile.Text) ;
          SaveReqCertFile.Text := ReadString (SectionData, 'SaveReqCertFile_Text', SaveReqCertFile.Text) ;
          LoadCaBundleFile.Text := ReadString (SectionData, 'LoadCaBundleFile_Text', LoadCaBundleFile.Text) ;
          MsStoreType.ItemIndex := ReadInteger (SectionData, 'MsStoreType_ItemIndex', MsStoreType.ItemIndex) ;            { V8.67 }
          MsLocationType.ItemIndex := ReadInteger (SectionData, 'MsLocationType_ItemIndex', MsLocationType.ItemIndex) ;   { V8.67 }
          if ReadString (SectionData, 'MSBriefList_Checked', 'False') = 'True' then MSBriefList.Checked := true else MSBriefList.Checked := false ;   { V8.67 }
          MsInstallFile.Text := ReadString (SectionData, 'MsInstallFile_Text', MsInstallFile.Text) ;                      { V8.67 }
          if ReadString (SectionData, 'CertPwPemKeys_Checked', 'False') = 'True' then CertPwPemKeys.Checked := true else CertPwPemKeys.Checked := false ; { V8.67 }
          if ReadString (SectionData, 'CertPwPkcs12_Checked', 'True') = 'True' then CertPwPkcs12.Checked := true else CertPwPkcs12.Checked := false ;    { V8.67 }
          if ReadString (SectionData, 'CertCheckOCSP_Checked', 'False') = 'True' then CertCheckOCSP.Checked := true else CertCheckOCSP.Checked := false ; { V8.69 }
          IntersCAFile.Text := ReadString (SectionData, 'IntersCAFile_Text', IntersCAFile.Text) ;                           { V8.69 }
          TestHostList.Lines.CommaText := ReadString (SectionData, 'TestHostList_Lines', TestHostList.Lines.CommaText) ;    { V8.69 }
          TestHostOne.Text := ReadString (SectionData, 'TestHostOne_Text', TestHostOne.Text) ;                              { V8.69 }
          DownCertsPath.Text := ReadString (SectionData, 'DownCertsPath_Text', DownCertsPath.Text) ;                        { V8.69 }
          DownIntersPath.Text := ReadString (SectionData, 'DownIntersPath_Text', DownIntersPath.Text) ;                        { V8.69 }
       end;
        IniFile.Free;
        PostMessage(Handle, WM_APPSTARTUP, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.WMAppStartup(var Msg: TMessage);
begin
 // V8.55 don't load OpenSSL in create
    OverbyteIcsWSocket.LoadSsl;
    FSslCertTools := TSslCertTools.Create(self);
    FSslCertTools.OnKeyProgress := ToolsOKeyProgress;
    FSslCAX509 := TX509Base.Create(self);    // V8.50
    frmPemTool1.Caption := CaptionMain + Trim(CurrentCertDirEdit.Text);
    PageControl1.ActivePageIndex := 0;
    LvCerts.Perform(CM_RECREATEWND, 0, 0); // fix column buttons not displayed
    OcspHttp := TOcspHttp.Create(Self);        { V8.69 }
    OcspHttp.OnOcspProg := LogEvent;           { V8.69 }
    OcspHttp.OcspHttpProxy := '';
    RootCAList := TX509List.Create(Self);      { V8.69 }
    RootCAList.X509Class := TX509Base;
    InterCAList := TX509List.Create(self);     { V8.69 }
    InterCAList.X509Class := TX509Base;
    DownCertsList := TX509List.Create(self);   { V8.69 }
    DownCertsList.X509Class := TX509Base;
    DownIntersList := TX509List.Create(self);  { V8.69 }
    DownIntersList.X509Class := TX509Base;
    FillListView;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
    temp: String;
begin
    ClosingFlag := True;   { V8.69 stop logging now }
    OcspHttp.Free;    { V8.69 }
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionMainWindow, KeyTop,               Top);
    IniFile.WriteInteger(SectionMainWindow, KeyLeft,              Left);
    IniFile.WriteInteger(SectionMainWindow, KeyWidth,             Width);
    IniFile.WriteInteger(SectionMainWindow, KeyHeight,            Height);
    IniFile.WriteString(SectionData,        KeyVerifyDir,         VerifyDir); { V8.38 }
    with IniFile do begin   { V8.40 }
          WriteString (SectionData, 'CAFilesDir_Text', CAFilesDir.Text) ;
          if CertAddComment.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertAddComment_Checked', temp) ;
          WriteString (SectionData, 'CertAltDomains_CommaText', CertAltDomains.Lines.CommaText) ;
          WriteString (SectionData, 'CertAltIPs_CommaText', CertAltIPs.Lines.CommaText) ;
          WriteString (SectionData, 'CertCommonName_Text', CertCommonName.Text) ;
          WriteString (SectionData, 'CertCountry_Text', CertCountry.Text) ;
          WriteString (SectionData, 'CertDays_Text', CertDays.Text) ;
          WriteString (SectionData, 'CertDescr_Text', CertDescr.Text) ;
          WriteString (SectionData, 'CertEMail_Text', CertEMail.Text) ;
          if CertExtClient.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertExtClient_Checked', temp) ;
          if CertExtCodeSign.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertExtCodeSign_Checked', temp) ;
          if CertExtEmail.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertExtEmail_Checked', temp) ;
          if CertExtServer.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertExtServer_Checked', temp) ;
          WriteString (SectionData, 'CertLocality_Text', CertLocality.Text) ;
          WriteString (SectionData, 'CertOrganization_Text', CertOrganization.Text) ;
          WriteString (SectionData, 'CertOrganizationalUnit_Text', CertOrganizationalUnit.Text) ;
          WriteString (SectionData, 'CertPassword_Text', CertPassword.Text) ;
          WriteInteger (SectionData, 'CertSignHash_ItemIndex', CertSignHash.ItemIndex) ;
          WriteString (SectionData, 'CertState_Text', CertState.Text) ;
          if CertUsageCRLSign.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertUsageCRLSign_Checked', temp) ;
          if CertUsageCertSign.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertUsageCertSign_Checked', temp) ;
          if CertUsageDataEn.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertUsageDataEn_Checked', temp) ;
          if CertUsageDigSign.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertUsageDigSign_Checked', temp) ;
          if CertUsageKeyAgree.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertUsageKeyAgree_Checked', temp) ;
          if CertUsageKeyEn.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertUsageKeyEn_Checked', temp) ;
          if CertUsageNonRepud.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertUsageNonRepud_Checked', temp) ;
          if CheckBoxComment.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CheckBoxComment_Checked', temp) ;
          if CheckBoxEmptyDestDir.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CheckBoxEmptyDestDir_Checked', temp) ;
          if CheckBoxOverwriteExisting.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CheckBoxOverwriteExisting_Checked', temp) ;
          if CheckBoxWarnDestNotEmpty.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CheckBoxWarnDestNotEmpty_Checked', temp) ;
          if CheckBoxWriteToBundle.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CheckBoxWriteToBundle_Checked', temp) ;
          WriteString (SectionData, 'CurrentCertDirEdit_Text', CurrentCertDirEdit.Text) ;
          WriteString (SectionData, 'DHParamFile_Text', DHParamFile.Text) ;
          WriteInteger (SectionData, 'DHParamSize_ItemIndex', DHParamSize.ItemIndex) ;
          WriteString (SectionData, 'DestDirEdit_Text', DestDirEdit.Text) ;
          WriteInteger (SectionData, 'KeyEncrypt_ItemIndex', KeyEncrypt.ItemIndex) ;
          WriteInteger (SectionData, 'KeyType_ItemIndex', KeyType.ItemIndex) ;
          WriteString (SectionData, 'LoadCertFile_Text', LoadCertFile.Text) ;
          if LoadCertInters.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'LoadCertInters_Checked', temp) ;
          if LoadCertPrivKey.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'LoadCertPrivKey_Checked', temp) ;
          WriteString (SectionData, 'LoadCertPW_Text', LoadCertPW.Text) ;
          WriteString (SectionData, 'LoadDirectory_Text', LoadDirectory.Text) ;
          WriteString (SectionData, 'LoadInterCerts_Text', LoadInterCerts.Text) ;
          WriteString (SectionData, 'LoadPrivatetKey_Text', LoadPrivatetKey.Text) ;
          WriteString (SectionData, 'LoadRequestFile_Text', LoadRequestFile.Text) ;
          if NewCertCopyExt.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'NewCertCopyExt_Checked', temp) ;
          if SaveAutoReplace.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'SaveAutoReplace_Checked', temp) ;
          WriteString (SectionData, 'SaveCertDer_Text', SaveCertDer.Text) ;
          WriteString (SectionData, 'SaveCertPem_Text', SaveCertPem.Text) ;
          WriteString (SectionData, 'SaveCertPW_Text', SaveCertPW.Text) ;
          WriteString (SectionData, 'SaveDirectory_Text', SaveDirectory.Text) ;
          if SaveInterCerts.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'SaveInterCerts_Checked', temp) ;
          WriteString (SectionData, 'SavePkcs12File_Text', SavePkcs12File.Text) ;
          WriteString (SectionData, 'SavePkcs7File_Text', SavePkcs7File.Text) ;
          if SavePrivateKey.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'SavePrivateKey_Checked', temp) ;
          WriteString (SectionData, 'SavePrvFileFile_Text', SavePrvFileFile.Text) ;
          WriteString (SectionData, 'SavePubKeyFile_Text', SavePubKeyFile.Text) ;
          WriteString (SectionData, 'SaveReqCertFile_Text', SaveReqCertFile.Text) ;
          WriteString (SectionData, 'LoadCaBundleFile_Text', LoadCaBundleFile.Text) ;
          WriteInteger (SectionData, 'MsStoreType_ItemIndex', MsStoreType.ItemIndex) ;              { V8.67 }
          WriteInteger (SectionData, 'MsLocationType_ItemIndex', MsLocationType.ItemIndex) ;        { V8.67 }
          if MSBriefList.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'MSBriefList_Checked', temp) ;  { V8.67 }
          WriteString (SectionData, 'MsInstallFile_Text', MsInstallFile.Text) ;                     { V8.67 }
          if CertPwPemKeys.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertPwPemKeys_Checked', temp) ; { V8.67 }
          if CertPwPkcs12.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertPwPkcs12_Checked', temp) ;   { V8.67 }
          if CertCheckOCSP.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertCheckOCSP_Checked', temp) ; { V8.69 }
          WriteString (SectionData, 'IntersCAFile_Text', IntersCAFile.Text) ;
          WriteString (SectionData, 'TestHostList_Lines', TestHostList.Lines.CommaText) ;
          WriteString (SectionData, 'TestHostOne_Text', TestHostOne.Text) ;
          WriteString (SectionData, 'DownCertsPath_Text', DownCertsPath.Text) ;
          WriteString (SectionData, 'DownIntersPath_Text', DownIntersPath.Text) ;
    end;
    IniFile.UpdateFile;
    IniFile.Free;
    if Assigned(FMsX509List) then FMsX509List.Free;
    FreeAndNil(FSslCertTools);
    FreeAndNil(RootCAList);
    FreeAndNil(InterCAList);
    FreeAndNil(DownCertsList);
    FreeAndNil(DownIntersList);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ListCertDetail(Cert: TX509Base): string;
var
    Ext: TExtension;
begin
    if NOT Assigned (Cert) then begin
        Result := 'No certificate loaded';
        Exit;
    end;
    with Cert do begin
    { Angus added major PEM entries separately, also serves to document how
    to access all the different properties of the T509 component.  Note multiple
    items may be returned, normally separated by CRLF, but unwrapped here for display.
    Rarely does a certificate have all these entries, particuarly the personal name
    stuff which is really for email certificates  }
    { V8.49 fixed some missing spaces after : }
        Result := 'ISSUED TO (Subject)' + #13#10 +
            'Common Name (CN): ' + IcsUnwrapNames(SubjectCName) + #13#10 +
            'Alt Name (DNS): ' + IcsUnwrapNames(SubAltNameDNS) + #13#10 +
            'Alt Name (IP): ' + IcsUnwrapNames(SubAltNameIP) + #13#10 +
            'Organisation (O): ' + IcsUnwrapNames(SubjectOName) + #13#10 +
            'Organisational Unit (OU): ' + IcsUnwrapNames(SubjectOUName) + #13#10 +
            'Country (C): ' + SubjectCOName + #13#10 +
            'State/Province(ST): ' + SubjectSTName + #13#10 +
            'Locality (L): ' + SubjectLName + #13#10 +
            'Serial Number: ' + SubjectSerialName + #13#10 +
            'Title (T): ' + GetNameEntryByNid(TRUE, NID_title) + #13#10 +
            'Initials (I): ' + GetNameEntryByNid(TRUE, NID_initials) + #13#10 +
            'Given Name (G): ' + GetNameEntryByNid(TRUE, NID_givenName) + #13#10 +
            'Surname (S): ' + GetNameEntryByNid(TRUE, NID_surname) + #13#10 +
            'Description (D): ' + GetNameEntryByNid(TRUE, NID_description) + #13#10 +
            'Email (Email): ' + SubjectEmailName + #13#10 +
            '' + #13#10;
        if SelfSigned then
            Result := Result + 'SELF SIGNED' + #13#10
        else begin
            Result := Result + 'ISSUED BY' + #13#10 +
                'Common Name (CN): ' + IcsUnwrapNames(IssuerCName) + #13#10 +
                'Organisation (O): ' + IcsUnwrapNames(IssuerOName) + #13#10 +
                'Organisational Unit (OU): ' + IcsUnwrapNames(IssuerOUName) + #13#10 +
                'Country (C): ' + IssuerCOName + #13#10 +
                'State/Province(ST): ' + IssuerSTName + #13#10 +
                'Locality (L): ' + IssuerLName + #13#10 +
                'Email (Email): ' + IssuerEmailName + #13#10;
        end;
       Result := Result + '' + #13#10 +
            'GENERAL' + #13#10 +
            'Serial Number: ' + SerialNumHex + #13#10 + // Oct 2015 not always very numeric IntToStr (SerialNum));
            'Issued on (UTC): ' + DateTimeToStr(ValidNotBefore) + #13#10 +  { V8.61 }
            'Expires on (UTC): ' + DateTimeToStr(ValidNotAfter) + #13#10 +  { V8.61 }
            'Basic Constraints: ' + IcsUnwrapNames(BasicConstraints) + #13#10 +
            'Key Usage: ' + IcsUnwrapNames(KeyUsage) + #13#10 +
            'Extended Key Usage: ' + IcsUnwrapNames(ExKeyUsage) + #13#10 +
            'Authority Info, OCSP: ' + UrlOcsp + #13#10 +                     { V8.69 }
            'Authority Info, Issuer Cert: ' + UrlIssuer + #13#10 +                 { V8.69 }
            'Certificate Policies: ' + IcsUnwrapNames(CertPolicies) + #13#10 +
            'CRL Distribution Points: ' + IcsUnwrapNames(CRLDistribution) + #13#10 +
            'Authority Key Identifier: ' + IcsUnwrapNames(AuthorityKeyId) + #13#10 +
            'Subject Key Identifier: ' + IcsUnwrapNames(SubjectKeyId) + #13#10 +
            'Signature Algorithm: ' + SignatureAlgorithm + #13#10 +  // Oct 2015
            'Fingerprint (sha1): ' + IcsLowerCase(Sha1Hex) + #13#10 +
            'Fingerprint (sha56): ' + IcsLowerCase(Sha256Hex) + #13#10;  { V8.63 }
        Ext := GetExtensionByName('acmeIdentifier');   { V8.62 }
        if Ext.Value <> '' then
            Result := Result + 'ACME Identifier: ' + Ext.Value + #13#10;
        if ExtendedValidation then
            Result := Result + 'Extended Validation (EV) SSL Server Certificate' + #13#10;
        Result := Result + 'Key Info: ' + KeyInfo + #13#10;                         // Oct 2015
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.ShowLogWindow;
begin
    frmPemTool2.Show;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetCertReadOpt(value: Boolean): TCertReadOpt;
begin
    if Value then
        Result := croTry
    else
        Result := croNo;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TfrmPemTool1.ListOcspStatus(Cert: TX509Base): string;             { V8.69 }
var
    Inters: TX509List;
    I: Integer;
begin
    if NOT Assigned (Cert) then begin
        Result := 'No certificate loaded';
        Exit;
    end;
    if (Cert.UrlOcsp = '') then begin
        Result := 'OCSP not available';
        AddLog(Result);
        Exit;
    end;
    AddLog('Checking OCSP status for: ' + Cert.SubjectCName);
    Inters := TX509List.Create(self);
    Inters.X509Class := TX509Base;
    try

     // need intermediate bundle to build OCSP requests with issuing certificate
        if InterCAList.Count = 0 then LoadInterCA;
        if RootCAList.Count = 0 then LoadRootCA;

    // look for issser in intermediate or root bundle
        if Pos('CA=TRUE', Cert.BasicConstraints) > 0 then begin
            I := RootCAList.IndexOfSubj(Cert.IssuerOneLine);
            if I >= 0 then
                Inters.Add(RootCAList[I].X509);
        end
        else if (NOT Cert.SelfSigned) then begin
            I := InterCAList.IndexOfSubj(Cert.IssuerOneLine);
            if I >= 0 then
                Inters.Add(InterCAList[I].X509);
        end;

    // try and download issuer
        if (Inters.Count = 0) and (Cert.UrlIssuer <> '') then begin
            OcspHttp.OcspInters := Inters;
            OcspHttp.DebugLevel := DebugConn;
            if OcspHttp.IssuerHttpRequest(Cert.UrlIssuer, 10) then begin  // wait for response
                AddLog('Downloaded Issuer certificate OK: ' + OcspHttp.OcspInters[0].SubjectOneLine);
            end;
        end;
        if Inters.Count = 0 then begin
            AddLog('Could not find Issuer certificate: ' + Cert.IssuerOneLine);
            exit;
        end;

    // finally get OCSP status
        OcspHttp.ClearOcsp;
        OcspHttp.DebugLevel := DebugSsl;
        OcspHttp.OcspCert := Cert;
        OcspHttp.OcspInters := Inters;
        OcspHttp.CheckOcspRevoked(RootCAList.X509Store, 10);   // wait for response
    finally
        Inters.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.ShowOneCert(const FileName: String);
var
    Cert: TX509Base;
    Errs: String;
begin
    if (FileName = '') or not FileExists(FileName) then
        raise Exception.Create('FileName empty or file doesn''t exist');
    ShowLogWindow;
    Cert := TX509Base.Create(nil);
    try
        try
            Cert.LoadFromFileEx(Filename, croTry, croTry, CertPassword.Text, Errs);  { V8.65 }
            if Errs <> '' then
                AddLog (Errs);   { V8.65 }
            if NOT Cert.IsCertLoaded then begin
                AddLog ('No certificate found in file ' + FileName);
            end
            else begin
                AddLog ('Certificate file ' + FileName);
                AddLog (ListCertDetail(Cert) + #13#10);
                if CertCheckOCSP.Checked then
                    AddLog(ListOcspStatus(Cert));                                  { V8.69 }
                frmPemTool2.Memo1.Lines.Text := frmPemTool2.Memo1.Lines.Text +
                                'Raw Cert' + #13#10 + Cert.GetRawText + #13#10;
                if Cert.IsPKeyLoaded then begin
                    AddLog ('!! Private key available for certificate: ' + Cert.KeyInfo + #13#10);
                end;
            end;
            if Cert.IsInterLoaded then begin
                 AddLog ('!! Intermediate certificates: ' + Cert.ListInters + #13#10);
            end;
        except
            on E:Exception do
            begin
                AddLog (E.Message) ;
            end;
        end;
    finally
        Cert.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.ShowBundleCerts(const FileName: String);
var
    Certs: TX509List;      { V8.39 read multiple certificates from PEM file }
    Total, I: Integer;
    Info, Errs: String;
begin
    if (FileName = '') or not FileExists(FileName) then
        raise Exception.Create('FileName empty or file doesn''t exist');
    ShowLogWindow;
    Certs := TX509List.Create(nil, True);
    Certs.X509Class := TX509Base;
    try
        Total := Certs.LoadAllFromFileEx(Filename, Errs);
        if Errs <> '' then
             AddLog ('Errors reading bundle file: ' + Errs);  { V8.65 }
        if Total <= 0 then begin
            AddLog ('No PEM certificates found in file');
        end
        else begin
            AddLog ('Certificate file ' + FileName);
            AddLog ('Number of PEM certificates found in file: ' + IntToStr (Total));

            for I := 1 to Total do begin
                Info := #13#10 + 'Certificate #' + IntToStr(I) + #13#10 + ListCertDetail(Certs [I-1]);
                AddLog (Info);
                frmPemTool2.Memo1.Lines.Text := frmPemTool2.Memo1.Lines.Text +
                                                 'Raw #' + IntToStr(I) + #13#10 + Certs [I-1].GetRawText;
            end;
        end;
    finally
        Certs.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnShowBundleFileClick(Sender: TObject);
var
    FileName : String;
begin
    if OpenDlg.InitialDir = '' then OpenDlg.InitialDir := DestDirEdit.Text;
    OpenDlg.Filter     := 'PEM Certs *.pem|*.pem|All Files *.*|*.*';
    if OpenDlg.Execute then
        FileName := OpenDlg.FileName;
    if (FileName = '') or not FileExists(FileName) then
        Exit;
    OpenDlg.InitialDir := ExtractFileDir(FileName);
    ShowBundleCerts(FileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnShowOneFileClick(Sender: TObject);
var
    FileName : String;
begin
    if OpenDlg.InitialDir = '' then OpenDlg.InitialDir := DestDirEdit.Text;
    OpenDlg.Filter     := SslCertFileOpenExts;  { V8.65 reset }
    if OpenDlg.Execute then
        FileName := OpenDlg.FileName;
    if (FileName = '') or not FileExists(FileName) then
        Exit;
    OpenDlg.InitialDir := ExtractFileDir(FileName);
    ShowOneCert(FileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.AddListView(X: TX509Base; const Filename: String);
var
    ListItem : TListItem;
    S        : String;
begin
    with LVCerts do begin
        ListItem          := Items.Add;
        ListItem.Caption  := X.SubjectCName;
        if ListItem.Caption = '' then
            ListItem.Caption := X.SubjectOName;
        S := X.SubjectOUName;
        if S = '' then
            S := X.SubjectOName;
        ListItem.SubItems.Add(S);
        S := X.IssuerCName;
        if S = '' then
            S := X.IssuerOUName;
        if S = '' then
            S := X.IssuerOName;
        ListItem.SubItems.Add(S);
        ListItem.SubItems.Add(DateToStr(X.ValidNotAfter));
        ListItem.SubItems.Add(ExtractFileName(FileName));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.FillListView;
var
    SRec    : TSearchRec;
    CertDir, Errs : String;
    X       : TX509Base;
begin
    X := TX509Base.Create(nil);
    try
        LvCerts.Items.BeginUpdate;
        try
            LVCerts.Items.Clear;
            FCurrentCertDir := Trim(CurrentCertDirEdit.Text);
            if not DirectoryExists(FCurrentCertDir) then
                Exit;
            CertDir := PathAddBackSlash(FCurrentCertDir);
            if FindFirst(CertDir + '*.*', faAnyFile - faDirectory, SRec) = 0 then
            try
                try
                    if Pos ('?', SRec.Name) = 0 then begin   { V8.69 ignore unicode file names on old compilers }
                        X.LoadFromFileEx(CertDir + SRec.Name, croNo, croNo, CertPassword.Text, Errs);  { V8.40 was PemFile, V8.65 fewer exceptions }
                        if Errs = '' then  // V8.65 ignore errors
                            AddListView(X, CertDir + SRec.Name);
                    end;
                except  // angus - ignore files that are not really certificates
                end;
                while FindNext(SRec) = 0 do begin
                    try
                        if Pos ('?', SRec.Name) = 0 then begin   { V8.69 ignore unicode file names on old compilers }
                            X.LoadFromFileEx(CertDir + SRec.Name, croNo, croNo, CertPassword.Text, Errs);  // V8.65 fewer exceptions
                            if Errs = '' then  // V8.65 ignore errors
                                AddListView(X, CertDir + SRec.Name);
                        end;         
                    except
                    end;
                end;
            finally
                FindClose(SRec);
            end;
        finally
            LvCerts.Items.EndUpdate;
        end;
    finally
        X.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.LvCertsDblClick(Sender: TObject);
var
    ListItem : TListItem;
    FileName : String;
begin
    with LvCerts do begin
        if Assigned(Selected) then begin
            ListItem := Items[Selected.Index];
            FileName := ListItem.SubItems[3];
            if FileName <> '' then
                FileName := PathAddBackSlash(FCurrentCertDir) + FileName;
            if FileExists(FileName) then
                ShowOneCert(FileName);
        end
        else
            MessageDlg('No item selected', mtError, [mbOK], 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.LvCertsColumnClick(Sender: TObject;
    Column: TListColumn);
var
    I : Integer;
begin
    Screen.Cursor := crHourGlass;
    LVCerts.Items.BeginUpdate;
    try
        if ColumnToSort = Column.Index then
            FLVSortFlag := not FLVSortFlag;
        for I := 0 to LVCerts.Columns.Count -1 do begin
             if I <> Column.Index then
                 LVCerts.Columns[I].ImageIndex := -1
             else begin
                 if FLVSortFlag then
                     LVCerts.Columns[Column.Index].ImageIndex := 1
                 else
                     LVCerts.Columns[Column.Index].ImageIndex := 2;
             end;
        end;
        ColumnToSort := Column.Index;
        with (Sender as TCustomListView) do begin
            AlphaSort;
            if Assigned(Selected) then
                Selected.MakeVisible(TRUE);
        end;
        LVCerts.Items.EndUpdate;
        Screen.Cursor := crDefault;
    except
        LVCerts.Items.EndUpdate;
        Screen.Cursor := crDefault;
        raise;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.LvCertsCompare(Sender: TObject; Item1,
    Item2: TListItem; Data: Integer; var Compare: Integer);
var
    Idx: Integer;
begin
    if not FLVSortFlag then begin
        if ColumnToSort = 0 then
            Compare := CompareText(Item1.Caption, Item2.Caption)
        else begin
            Idx := ColumnToSort - 1;
            if Idx = 1 then begin
                Compare := 0;
                if StrToDate(Item1.SubItems[Idx]) > StrToDate(Item2.SubItems[Idx]) then
                    Compare := 1
                else
                if StrToDate(Item1.SubItems[Idx]) < StrToDate(Item2.SubItems[Idx]) then
                    Compare := -1;
                Exit;
            end;
            Compare := CompareText(Item1.SubItems[Idx], Item2.SubItems[Idx]);
        end;
    end
    else begin
        if ColumnToSort = 0 then
            Compare := CompareText(Item2.Caption, Item1.Caption)
        else begin
            Idx := ColumnToSort - 1;
            if Idx = 1 then begin
                Compare := 0;
                if StrToDate(Item1.SubItems[Idx]) < StrToDate(Item2.SubItems[Idx]) then
                    Compare := 1
                else
                if StrToDate(Item1.SubItems[Idx]) > StrToDate(Item2.SubItems[Idx]) then
                    Compare := -1;
                Exit;
            end;
            Compare := CompareText(Item2.SubItems[Idx], Item1.SubItems[Idx]);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.LvCertsCustomDraw(Sender: TCustomListView;
    const ARect    : TRect;
    var   DefaultDraw: Boolean);
var
    I            : Integer;
    LvColumn     : TLVColumn;
    HeaderHandle : THandle;
begin
    { Display sort BMP on the right of column caption}
    { requires comctl32.dll version 4.70+            }
    HeaderHandle := GetDlgItem(LVCerts.Handle, 0);
    for I := 0 to LVCerts.Columns.Count - 1 do begin
        if (LVCerts.Columns[I].ImageIndex <> -1) then begin
            FillChar(LvColumn, SizeOf(LvColumn), #0);
            ListView_GetColumn(HeaderHandle, I, LvColumn);
            with LvColumn do begin
                iImage := LVCerts.Columns[I].ImageIndex;
                mask   := mask or LVCF_IMAGE or LVCF_FMT;
                fmt    := fmt or LVCFMT_IMAGE or LVCFMT_BITMAP_ON_RIGHT;
                case LVCerts.Columns[I].Alignment of
                    taLeftJustify  : fmt := fmt or LVCFMT_LEFT;
                    taCenter       : fmt := fmt or LVCFMT_CENTER;
                    taRightJustify : fmt := fmt or LVCFMT_RIGHT;
                end;
            end;
            ListView_SetColumn(LVCerts.Handle, I, LvColumn);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TfrmPemTool1.BuildLoadName(const fname: string): string;
begin
    Result := '';
    if fname = '' then begin
        DispError('Must specify load file name');
        exit;
    end;
    if LoadDirectory.Text = '' then begin
        DispError('Must specify load directory');
        exit;
    end;
    Result := PathAddBackSlash(LoadDirectory.Text) + fname;
    if FileExists(Result) then Exit;
    DispError('Load file not found - ' + Result);
    Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TfrmPemTool1.BuildSaveName(const fname: string): string;
begin
    Result := '';
    if fname = '' then begin
        DispError('Must specify save file name');
        exit;
    end;
    if SaveDirectory.Text = '' then begin
        DispError('Must specify save directory');
        exit;
    end;
    if NOT ForceDirectories(SaveDirectory.Text) then begin
        DispError('Failed to create save directory - ' + SaveDirectory.Text);
        exit;
    end;
    Result := PathAddBackSlash(SaveDirectory.Text) + fname;
    if NOT FileExists(Result) then exit;
    if SaveAutoReplace.Checked then begin
        if DeleteFile(Result) then exit;
        DispError('Failed to delete old save file - ' + Result);
    end
    else
        DispError('File already exists and replace not specified - ' + Result);
    Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SetCertProps;
var
    I: Integer;
begin
    CertCommonName.Text := Trim(CertCommonName.Text);
    with FSslCertTools do begin
        Country           := CertCountry.Text;
        State             := CertState.Text;
        Locality          := CertLocality.Text;
        Organization      := CertOrganization.Text;
        OrgUnit           := CertOrganizationalUnit.Text;
        Descr             := CertDescr.Text;
        Email             := CertEMail.Text;
        CommonName        := CertCommonName.Text;

     // V8.64 make sure alt domain contains common name
        if (CertAltDomains.Lines.Count = 0) or
            ((CertAltDomains.Lines.Count > 0) and
             (CertAltDomains.Lines.IndexOf(CertCommonName.Text) < 0)) then
                                CertAltDomains.Lines.Add(CertCommonName.Text);
        for I := 0 to CertAltDomains.Lines.Count - 1 do
              CertAltDomains.Lines[I] := Trim(CertAltDomains.Lines[I]);
        AltDNSList.Assign(CertAltDomains.Lines);
        AltIpList.Assign(CertAltIPs.Lines);
  //      AltEmailList
  //      AltIssuer
  //      CRLDistPoint
  //      AuthInfoAcc
        BasicIsCA         := CertIsCA.Checked;
        BasicPathLen      := 0;
        KeyCertSign       := CertUsageCertSign.Checked;
        KeyCRLSign        := CertUsageCRLSign.Checked;
        KeyDigiSign       := CertUsageDigSign.Checked;
        KeyDataEnc        := CertUsageDataEn.Checked;
        KeyKeyEnc         := CertUsageKeyEn.Checked;
        KeyKeyAgree       := CertUsageKeyAgree.Checked;
        KeyNonRepud       := CertUsageNonRepud.Checked;
        KeyExtClient      := CertExtClient.Checked;
        KeyExtServer      := CertExtServer.Checked;
        KeyExtEmail       := CertExtEmail.Checked;
        KeyExtCode        := CertExtCodeSign.Checked;
        ExpireDays        := atoi(CertDays.Text);
//        SerialNum
        AddComments       := CertAddComment.Checked;
        CertDigest        := DigestDispList[CertSignHash.ItemIndex];  { V8.62 }
        SerialNum         := 0;   { V8.46 force random serial }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelCAFileClick(Sender: TObject);
begin
    OpenDlg.InitialDir := LoadDirectory.Text;
    OpenDlg.FileName := PathAddBackSlash(LoadDirectory.Text) + LoadCaBundleFile.Text;
    if OpenDlg.FileName = '' then Exit;
    if OpenDlg.Execute then begin
        LoadCaBundleFile.Text := ExtractFileName(OpenDlg.FileName);
        LoadDirectory.Text := ExtractFilePath(OpenDlg.FileName);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelCertFileClick(Sender: TObject);
begin
    OpenDlg.InitialDir := LoadDirectory.Text;
    OpenDlg.FileName := PathAddBackSlash(LoadDirectory.Text) + LoadCertFile.Text;
    if OpenDlg.FileName = '' then Exit;
    if OpenDlg.Execute then begin
        LoadCertFile.Text := ExtractFileName(OpenDlg.FileName);
        LoadDirectory.Text := ExtractFilePath(OpenDlg.FileName);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelCertsDBClick(Sender: TObject);
begin

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelCurrDirClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := CurrentCertDirEdit.Text ;
    if OpenDirDiag.Execute then
        CurrentCertDirEdit.Text := ExtractFilePath(OpenDirDiag.FileName);
    FillListView;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelLoadDirClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := LoadDirectory.Text ;
    if OpenDirDiag.Execute then
        LoadDirectory.Text := ExtractFilePath(OpenDirDiag.FileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelPrvKeyFileClick(Sender: TObject);
begin
    OpenDlg.InitialDir := LoadDirectory.Text;
    OpenDlg.FileName := PathAddBackSlash(LoadDirectory.Text) + LoadPrivatetKey.Text;
    if OpenDlg.FileName = '' then Exit;
    if OpenDlg.Execute then begin
        LoadPrivatetKey.Text := ExtractFileName(OpenDlg.FileName);
        LoadDirectory.Text := ExtractFilePath(OpenDlg.FileName);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelReqFileClick(Sender: TObject);
begin
    OpenDlg.InitialDir := LoadDirectory.Text;
    OpenDlg.FileName := PathAddBackSlash(LoadDirectory.Text) + LoadRequestFile.Text;
    if OpenDlg.FileName = '' then Exit;
    if OpenDlg.Execute then begin
        LoadRequestFile.Text := ExtractFileName(OpenDlg.FileName);
        LoadDirectory.Text := ExtractFilePath(OpenDlg.FileName);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelSaveDirClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := SaveDirectory.Text ;
    if OpenDirDiag.Execute then
        SaveDirectory.Text := ExtractFilePath(OpenDirDiag.FileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelImpDirClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := DestDirEdit.Text ;
    if OpenDirDiag.Execute then
        DestDirEdit.Text := ExtractFilePath(OpenDirDiag.FileName);
    DestDirEditChange(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelIntersCAClick(Sender: TObject);   { V8.69 }
begin
    OpenDlg.InitialDir := LoadDirectory.Text;
    OpenDlg.FileName := IntersCAFile.Text;
    if OpenDlg.FileName = '' then Exit;
    if OpenDlg.Execute then begin
        IntersCAFile.Text := OpenDlg.FileName;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelIntersFileClick(Sender: TObject);
begin
    OpenDlg.InitialDir := LoadDirectory.Text;
    OpenDlg.FileName := PathAddBackSlash(LoadDirectory.Text) + LoadInterCerts.Text;
    if OpenDlg.FileName = '' then Exit;
    if OpenDlg.Execute then begin
        LoadInterCerts.Text := ExtractFileName(OpenDlg.FileName);
        LoadDirectory.Text := ExtractFilePath(OpenDlg.FileName);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.CAFilesDirClick(Sender: TObject);
begin
    OpenDlg.InitialDir := LoadDirectory.Text;
    OpenDlg.FileName := CAFilesDir.Text ;
    if OpenDlg.Execute then
        CAFilesDir.Text := OpenDlg.FileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.CurrentCertDirEditChange(Sender: TObject);
begin
    FCurrentCertDir := Trim(CurrentCertDirEdit.Text);
    frmPemTool1.Caption := CaptionMain + FCurrentCertDir;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.PageControl1Change(Sender: TObject);
begin
    case PageControl1.ActivePageIndex of
        0: frmPemTool1.Caption := CaptionMain + FCurrentCertDir;
        1: frmPemTool1.Caption := CaptionMain + Trim(DestDirEdit.Text);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.DestDirEditChange(Sender: TObject);
begin
    frmPemTool1.Caption := CaptionMain + Trim(DestDirEdit.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doClearCertsClick(Sender: TObject);
begin
    FSslCertTools.ClearAll;
    FSslCAX509.ClearAll;
    DispCert;
    DispPKey;
    DispReq;
    DispInter;
    DispCACert;
    KeyPairLines.Lines.Clear;
    DHParamsLines.Lines.Clear;
    CertLinesNew.Lines.Clear;
    CertAltDomains.Lines.Clear;  // V8.49
    CertAltIPs.Lines.Clear;      // V8.49
    CertCommonName.Text := '';   // V8.49
    frmPemTool2.Memo1.Lines.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doCreateBundleClick(Sender: TObject);
var
    certfname, pkeyfname, interfname, savefname: string;
    PrivKeyType: TSslPrivKeyCipher;   { V8.67 }
begin
    doClearCertsClick(self);
    certfname := BuildLoadName(LoadCertFile.Text);
    if certfname = '' then Exit;
    pkeyfname := BuildLoadName(LoadPrivatetKey.Text);
    if pkeyfname = '' then Exit;
    interfname := BuildLoadName(LoadInterCerts.Text);   // optional
    savefname := BuildSaveName(SaveCertPem.Text);
    if savefname = '' then Exit;
    PrivKeyType := PrivKeyEncNone;
    if CertPwPemKeys.Checked then begin   { V8.67 }
        PrivKeyType := TSslPrivKeyCipher(KeyEncrypt.ItemIndex);
        if PrivKeyType = PrivKeyEncNone then
            PrivKeyType := PrivKeyEncAES256;
    end;
    try
        FSslCertTools.CreateCertBundle(certfname, pkeyfname, interfname,
                  LoadCertPW.Text, savefname, SaveCertPW.Text, PrivKeyType); { V8.67 }
        DispError('Saved certificate bundle OK  with ' +
            SslPrivKeyCipherLits[PrivKeyType] + ' key cipher: ' + savefname);  { V8.67 }
        DispCert;
        DispPKey;
        DispInter;
    except
        on E:Exception do
            DispError(E.Message);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.LoadRootCA;                       { V8.65 }
var
    Internal: Boolean;
    Errs: String;
begin
    RootCAList.Clear;
    Internal := False;
    if (CAFilesDir.Text = '') or (CAFilesDir.Text = 'Internal') then begin
        RootCAList.LoadAllFromStringEx(sslRootCACertsBundle, Errs);  { V8.64 trusted root so we check chain }
        Internal := False;
        IcsIpClient.LogSslRootFile := '';                  { V8.69 }
    end
    else begin
        if NOT FileExists(CAFilesDir.Text)then begin      { V8.65 }
            DispError('CA root bundle file not found: ' + CAFilesDir.Text);
            exit;
        end;
        RootCAList.LoadAllFromFileEx(CAFilesDir.Text, Errs);
        IcsIpClient.LogSslRootFile := CAFilesDir.Text;     { V8.69 }
    end;
    if Errs <> '' then DispError(Errs);
    if RootCAList.Count > 0 then begin
        if Internal then
            DispError('Loaded ' + IntToStr(RootCAList.Count) + ' CA root certificates from internal bundle')
        else
            DispError('Loaded ' + IntToStr(RootCAList.Count) + ' CA root certificates from: ' + CAFilesDir.Text);
        if NOT RootCAList.SetX509Store then
            DispError('Failed to Build X509 Store');     { V8.69 }
    end
    else
        DispError('No CA root bundle certificates found in: ' + CAFilesDir.Text);

end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.LoadInterCA;                       { V8.69 }
var
    Errs: String;
begin
    InterCAList.Clear;
    if NOT FileExists(IntersCAFile.Text)then begin
        AddLog('Intermediate CA bundle file not found: ' + IntersCAFile.Text);
        exit;
    end;
    InterCAList.LoadAllFromFileEx(IntersCAFile.Text, Errs);
    if Errs <> '' then DispError(Errs);
    if InterCAList.Count = 0 then
        AddLog('Failed to Load Intermediate CA bundle file: ' + IntersCAFile.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doCheckBundleSelfClick(Sender: TObject);
var
    CertStr, ErrStr: string;
    ValRes: TChainResult;
begin
    if NOT FSslCertTools.IsCertLoaded then begin
        DispError('Must load or create a certificate first');
        exit;
    end;
    if NOT FSslCertTools.IsPKeyLoaded then begin
        DispError('Must load or create a private key first');
        exit;
    end;
    if RootCAList.Count = 0 then LoadRootCA;
    ValRes := FSslCertTools.ValidateCertChain('', RootCAList, CertStr, ErrStr);
    if ValRes = chainOK then
        ErrStr := 'Chain Validated OK'
    else if ValRes = chainWarn then
        ErrStr := 'Chain Warning - ' + ErrStr
    else
        ErrStr := 'Chain Failed - ' + ErrStr;
    DispError(ErrStr);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doListRootsClick(Sender: TObject);    { V8.65 }
var
    I: Integer;
    Info, Num: string;
begin
    ShowLogWindow;
    if RootCAList.Count = 0 then LoadRootCA;
    AddLog ('CA root bundle certificate file ' + CAFilesDir.Text);
    AddLog ('Number of root certificates found: ' + IntToStr (RootCAList.Count));
    if RootCAList.Count > 0 then begin
    //    RootCAList.SortChain(xsrtIssuerFirst);
        for I := 1 to RootCAList.Count do begin
            Num := IntToStr (I);
            while Length(Num) < 3 do Num := '0' + Num;
            Info := Info + '#' + Num + ' ';
            if RootCAList [I-1].SubAltNameDNS <> '' then
                Info := Info + IcsUnwrapNames(RootCAList [I-1].SubAltNameDNS)
            else if RootCAList [I-1].SubjectCName <> '' then
                Info := Info + IcsUnwrapNames(RootCAList [I-1].SubjectCName)
            else
                Info := Info + IcsUnwrapNames(RootCAList [I-1].SubjectOName);
            Info := Info + ' (' + IcsUnwrapNames(RootCAList [I-1].SubjectOName) + ')';
            if RootCAList [I-1].SubjectOUName <> '' then
                Info := Info + ' OU: ' + IcsUnwrapNames(RootCAList [I-1].SubjectOUName);
            Info := Info + ', Expires: ' + FormatDateTime(ISODateMask, RootCAList [I-1].ValidNotAfter);
            Info := Info + ', Fingerprint: ' + IcsLowerCase(RootCAList [I-1].Sha256Hex);
            Info := Info + IcsCRLF;
        end;
        AddLog (Info);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doListIntersClick(Sender: TObject);         { V8.69 }
var
    I: Integer;
    Info, Num: string;
begin
    ShowLogWindow;
    if InterCAList.Count = 0 then LoadInterCA;
    AddLog ('Intermediate CA bundle certificate file ' + IntersCAFile.Text);
    AddLog ('Number of inter certificates found: ' + IntToStr (InterCAList.Count));
    if InterCAList.Count > 0 then begin
    //    InterCAList.SortChain(xsrtIssuerFirst);
        for I := 1 to InterCAList.Count do begin
            Num := IntToStr (I);
            while Length(Num) < 3 do Num := '0' + Num;
            Info := Info + '#' + Num + ' ';
            if InterCAList [I-1].SubAltNameDNS <> '' then
                Info := Info + IcsUnwrapNames(InterCAList [I-1].SubAltNameDNS)
            else if InterCAList [I-1].SubjectCName <> '' then
                Info := Info + IcsUnwrapNames(InterCAList [I-1].SubjectCName)
            else
                Info := Info + IcsUnwrapNames(InterCAList [I-1].SubjectOName);
            Info := Info + ' (' + IcsUnwrapNames(InterCAList [I-1].SubjectOName) + ')';
            if InterCAList [I-1].SubjectOUName <> '' then
                Info := Info + ' OU: ' + IcsUnwrapNames(InterCAList [I-1].SubjectOUName);
            Info := Info + ', Expires: ' + FormatDateTime(ISODateMask, InterCAList [I-1].ValidNotAfter);
            Info := Info + ', Fingerprint: ' + IcsLowerCase(InterCAList [I-1].Sha256Hex);
            Info := Info + IcsCRLF;
        end;
        AddLog (Info);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doCheckBundleWinClick(Sender: TObject);
var
    CertChain: TX509List;
    ChainVerifyResult: LongWord;
    VerifyInfo: String;
begin
    if NOT FSslCertTools.IsCertLoaded then begin
        DispError('Must load or create a certificate first');
        exit;
    end;
    if SaveInterCerts.Checked then begin
        if NOT FSslCertTools.IsInterLoaded then begin
            DispError('Must load intermediate certificates first');
            exit;
        end;
    end;
    if FSslCertTools.IsPKeyLoaded then begin
        if NOT FSslCertTools.CheckCertAndPKey then begin
            DispError('Mismatch certificate and private key');
            exit;
        end;
    end;

  { get intermediate chain }
    CertChain := TX509List.Create(nil);
    try
        FSslCertTools.GetIntersList (CertChain);

      { pending use ValidateCertChain when it's written }

      { start Windows certificate engine }
        if not Assigned (MsCertChainEngine) then
            MsCertChainEngine := TMsCertChainEngine.Create;

      { see if checking revoocation, very slow!!!! }
        MsCertChainEngine.VerifyOptions := []; // [mvoRevocationCheckChainExcludeRoot];

      { Pass the certificate and the chain certificates to the engine      }
        MsCertChainEngine.VerifyCert (FSslCertTools, CertChain, ChainVerifyResult, True);

       { The MsChainVerifyErrorToStr function works on chain error codes     }
        VerifyInfo := MsChainVerifyErrorToStr (ChainVerifyResult);
        DispError('Bundle check result - ' + VerifyInfo);
    finally
        CertChain.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doCreateCACertClick(Sender: TObject);
begin
    SetCertProps;
    doCreateCACert.Enabled := false;
    try
        try
            if NOT FSslCAX509.IsCertLoaded then begin    { V8.50 }
                DispError('Must specify CA ertificate first');
                exit;
            end;
            FSslCertTools.X509CA := FSslCAX509.X509;     { V8.50 }
            FSslCertTools.PrivKeyCA := FSslCAX509.PrivateKey;   { V8.50 }
            DispCACert;
            FSslCertTools.DoSignCertReq(NewCertCopyExt.Checked);
            DispError('Created certificate from request signed by CA certificate OK');
            DispCert;
            CertLinesNew.Lines.Text := FSslCertTools.SaveCertToText(true);
        except
            on E:Exception do
                    DispError(E.Message);
        end;
    finally
        doCreateCACert.Enabled := true;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doCreateReqCertClick(Sender: TObject);
begin
    SetCertProps;
    doCreateReqCert.Enabled := false;
    try
        try
            FSslCertTools.X509Req := Nil;
            DispCert;
            CertLinesNew.Lines.Clear;
            FSslCertTools.DoCertReqOld;
            DispError('Created certificate request from existing certificate OK');
            DispReq;
            CertLinesNew.Lines.Text := FSslCertTools.SaveReqToText(true);
        except
            on E:Exception do
                    DispError(E.Message);
        end;
    finally
        doCreateReqCert.Enabled := true;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doCreateReqPropsClick(Sender: TObject);
begin
    SetCertProps;
    doCreateReqProps.Enabled := false;
    try
        try
            FSslCertTools.X509Req := Nil;
            DispCert;
            CertLinesNew.Lines.Clear;
            FSslCertTools.DoCertReqProps;
            DispError('Created certificate request from properties OK');
            DispReq;
            CertLinesNew.Lines.Text := FSslCertTools.SaveReqToText(true);
        except
            on E:Exception do
                    DispError(E.Message);
        end;
    finally
        doCreateReqProps.Enabled := true;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doCreateSelfCertClick(Sender: TObject);
begin
    SetCertProps;
    doCreateSelfCert.Enabled := false;
    try
        try
            FSslCertTools.X509 := Nil;
            DispCert;
            CertLinesNew.Lines.Clear;
            FSslCertTools.DoSelfSignCert;
            DispError('Created self signed certificate OK');
            DispCert;
            CertLinesNew.Lines.Text := FSslCertTools.SaveCertToText(true);
        except
            on E:Exception do
                    DispError(E.Message);
        end;
    finally
        doCreateSelfCert.Enabled := true;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doDHParamsClick(Sender: TObject);
var
    fname: string;
const
    bittable: array [0..4] of integer = (768, 1024, 2048, 4096, 8192);
begin
    fname := BuildSaveName(DHParamFile.Text);
    if fname = '' then Exit;
    doDHParams.Enabled := false;
    try
        try
            DHParamsLines.Lines.Text := 'Generating DHParams, this may take many minutes';
            DispError(DHParamsLines.Lines.Text);
            StartTickCount := GetTickCount;
            DHParamsLines.Lines.Text := FSslCertTools.DoDHParams(FName, bittable[DHParamSize.ItemIndex]);
            DispError('Generated DHParams OK - ' + fname + ', took ' + IntToStr(IcsCalcTickDiff
                                            (StartTickCount, GetTickCount) div 1000) + ' secs');
        except
            on E:Exception do
                    DispError(E.Message);
        end;
    finally
        doDHParams.Enabled := true;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doGenKeyClick(Sender: TObject);
var
    PrivKeyType: TSslPrivKeyCipher;   { V8.67 }
begin
    FSslCertTools.PrivKeyType := TSslPrivKeyType(KeyType.ItemIndex);
    doGenKey.Enabled := false;
    PrivKeyType := PrivKeyEncNone;
    if CertPwPemKeys.Checked then begin   { V8.67 }
        PrivKeyType := TSslPrivKeyCipher(KeyEncrypt.ItemIndex);
        if PrivKeyType = PrivKeyEncNone then
            PrivKeyType := PrivKeyEncAES256;
    end;
    try
        try
            FSslCertTools.PrivateKey := Nil;
            KeyPairLines.Lines.Clear;
            DispError('Generating private and public key pair, please wait');
            StartTickCount := GetTickCount;
            FSslCertTools.DoKeyPair;
            KeyPairLines.Lines.Text := FSslCertTools.SavePKeyToText(SaveCertPW.Text, PrivKeyType);  { V8.67 }
            DispError('Generated private and public key pair OK with ' +
                                    SslPrivKeyCipherLits[PrivKeyType] + ' key cipher');
            DispPKey;
        except
            on E:Exception do
                DispError(E.Message);
        end;
    finally
        doGenKey.Enabled := true;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.DispCert;
var
    info: String;
begin
    info := 'Certificate: ';
    if NOT FSslCertTools.IsCertLoaded then
        LabelStateCert.Caption := info + 'None'
    else begin
        ShowLogWindow;
        with FSslCertTools do begin
            LabelStateCert.Caption := info + CertInfo(False);
            AddLog (ListCertDetail(FSslCertTools));
            frmPemTool2.Memo1.Lines.Text := frmPemTool2.Memo1.Lines.Text +
                                       'Raw Certificate' + #13#10 + GetRawText;
            AddLog ('');
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.DispPKey;
begin
    LabelStatePrivKey.Caption := 'Private Key: ';
    if NOT FSslCertTools.IsPKeyLoaded then
        LabelStatePrivKey.Caption := LabelStatePrivKey.Caption + 'None'
    else begin
        ShowLogWindow;
        with FSslCertTools do begin
            LabelStatePrivKey.Caption := LabelStatePrivKey.Caption +
                                                    #13#10 + PrivateKeyInfo;
            frmPemTool2.Memo1.Lines.Text := frmPemTool2.Memo1.Lines.Text +
                'Raw Private Key: ' + PrivateKeyInfo + #13#10 + GetPKeyRawText;
            AddLog ('');
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.DispReq;
begin
    LabelStateReq.Caption := 'Certificate Request: ';
    if NOT FSslCertTools.IsReqLoaded then
        LabelStateReq.Caption := LabelStateReq.Caption + 'None'
    else begin
        ShowLogWindow;
        with FSslCertTools do begin
            LabelStateReq.Caption := LabelStateReq.Caption + ReqCertInfo;
            frmPemTool2.Memo1.Lines.Text := frmPemTool2.Memo1.Lines.Text +
                                                       LabelStateReq.Caption;
            frmPemTool2.Memo1.Lines.Text := frmPemTool2.Memo1.Lines.Text +
                          'Raw Certificate Request' + #13#10 + GetRequestRawText;
            AddLog ('');
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.DispInter;
var
    info: String;
begin
    info := 'Intermediate Certificates: ';
    if NOT FSslCertTools.IsInterLoaded then
        LabelInters.Caption := info + 'None'
    else begin
        ShowLogWindow;
        LabelInters.Caption := info + FSslCertTools.ListInters;
        frmPemTool2.Memo1.Lines.Text := frmPemTool2.Memo1.Lines.Text +
                                                       LabelInters.Caption;
        AddLog ('');
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doLoadBase64Click(Sender: TObject);
begin
    if (Pos ('-BEGIN CERTIFICATE-', CertLinesOld.Text) = 0) then begin
        DispError('Must paste base64 certificate into control');
        exit;
    end;
    try
        FSslCertTools.LoadFromText(CertLinesOld.Text, true, LoadCertPW.Text);
        DispCert;
        DispPKey;
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.DispCACert;
begin
    LabelStateCACert.Caption := 'CA Certificate: ';
    if NOT FSslCAX509.IsCertLoaded then                   { V8.50 }
         LabelStateCACert.Caption := 'None'
    else
      { assume cert still loaded before being copied to CA }
        LabelStateCACert.Caption := LabelStateCACert.Caption +
                                                FSslCAX509.CertInfo(True);     { V8.50 }
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doLoadCABundleClick(Sender: TObject);     { V8.50 }
var
    fname: string;
begin
    fname := BuildLoadName(LoadCaBundleFile.Text);
    if fname = '' then Exit;
    try
        FSslCAX509.LoadFromFile(fname, croTry, croNo, LoadCertPW.Text);
        if (Pos ('CA=TRUE', FSslCAX509.BasicConstraints) = 0) then begin
            DispError('Certificate does not allow CA signing');
            FSslCAX509.ClearAll;
            exit;
        end;
        if NOT FSslCAX509.CheckCertAndPKey then begin
            DispError('Need matching CA certificate amd private key');
            FSslCAX509.ClearAll;
            exit;
        end;
        DispError('Loaded CA cerfificate OK - ' + fname);
        DispCACert;
    except
        on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doLoadCertClick(Sender: TObject);
var
    fname, Errs: string;
begin
    fname := BuildLoadName(LoadCertFile.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.LoadFromFileEx(fname, GetCertReadOpt(LoadCertPrivKey.Checked),
                           GetCertReadOpt(LoadCertInters.Checked), LoadCertPW.Text, Errs);  { V8.65 }
        if Errs <> '' then
            DispError('Failed to Loaded cerfificate: ' + Errs + ' - ' + fname)   { V8.65 }
        else
            DispError('Loaded cerfificate OK - ' + fname);
        DispCert;
        if LoadCertPrivKey.Checked then begin
            DispError('Loaded cerfificate and key OK - ' + fname);
            DispPKey;
        end;
        if LoadCertInters.Checked then
            DispInter;
    except
        on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doLoadIntersClick(Sender: TObject);
var
    fname: string;
begin
    fname := BuildLoadName(LoadInterCerts.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.LoadIntersFromPemFile(fname);
        DispError('Loaded intermediates OK - ' + fname);
        DispInter;
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doLoadPrvKeyClick(Sender: TObject);
var
    fname: string;
begin
    fname := BuildLoadName(LoadPrivatetKey.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.PrivateKeyLoadFromPemFile(fname, LoadCertPW.Text);
        DispError('Loaded private key OK - ' + fname);
        DispPKey;
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doLoadReqClick(Sender: TObject);
var
    fname: string;
begin
    fname := BuildLoadName(LoadRequestFile.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.LoadReqFromFile(fname);
        DispError('Loaded request OK - ' + fname);
        DispReq;
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doSaveCertDerClick(Sender: TObject);
var
    fname: string;
begin
    if NOT FSslCertTools.IsCertLoaded then begin
        DispError('Must load or create a certificate first');
        exit;
    end;
    fname := BuildSaveName(SaveCertDer.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.SaveToDERFile(fname);
        DispError('Saved certificate OK - ' + fname);
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doSaveCertPemClick(Sender: TObject);
var
    fname: string;
    PrivKeyType: TSslPrivKeyCipher;   { V8.67 }
begin
    if NOT FSslCertTools.IsCertLoaded then begin
        DispError('Must load or create a certificate first');
        exit;
    end;
    PrivKeyType := PrivKeyEncNone;
    if SavePrivateKey.Checked then begin
        if NOT FSslCertTools.IsPKeyLoaded then begin
            DispError('Must load or create a private key first');
            exit;
            if CertPwPemKeys.Checked then begin   { V8.67 }
                PrivKeyType := TSslPrivKeyCipher(KeyEncrypt.ItemIndex);
                if PrivKeyType = PrivKeyEncNone then
                    PrivKeyType := PrivKeyEncAES256;
             end;
        end;
    end;
    if SaveInterCerts.Checked then begin
        if NOT FSslCertTools.IsInterLoaded then begin
            DispError('Must load intermediate certificates first');
            exit;
        end;
    end;
    fname := BuildSaveName(SaveCertPem.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.SaveToPemFile(fname, SavePrivateKey.Checked,
            CertAddComment.Checked, SaveInterCerts.Checked, SaveCertPW.Text, PrivKeyType); { V8.67 }
        if NOT SavePrivateKey.Checked then
            DispError('Saved certificate OK - ' + fname)
        else
            DispError('Saved certificate OK with ' + SslPrivKeyCipherLits[PrivKeyType] +
                                                              ' key cipher: ' + fname);   { V8.67 }
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doSavePkcs12Click(Sender: TObject);
var
    fname: string;
    PrivKeyType: TSslPrivKeyCipher;   { V8.67 }
begin
    if NOT FSslCertTools.IsCertLoaded then begin
        DispError('Must load or create a certificate first');
        exit;
    end;
    PrivKeyType := PrivKeyEncNone;
    if NOT FSslCertTools.IsPKeyLoaded then begin
        DispError('Must load or create a private key first');
        exit;
    end;
    if CertPwPkcs12.Checked then begin   { V8.67 }
        PrivKeyType := TSslPrivKeyCipher(KeyEncrypt.ItemIndex);
        if PrivKeyType = PrivKeyEncNone then
            PrivKeyType := PrivKeyEncAES256;
     end;
    if SaveInterCerts.Checked then begin
        if NOT FSslCertTools.IsInterLoaded then begin
            DispError('Must load intermediate certificates first');
            exit;
        end;
    end;
    fname := BuildSaveName(SavePkcs12File.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.SaveToP12File(fname, SaveCertPW.Text, SaveInterCerts.Checked, PrivKeyType); { V8.67 }
        DispError('Saved certificate OK with ' +
                SslPrivKeyCipherLits[PrivKeyType] + ' key cipher: ' + fname);   { V8.67 }
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doSavePkcs7CertClick(Sender: TObject);
var
    fname: string;
begin
    if NOT FSslCertTools.IsCertLoaded then begin
        DispError('Must load or create a certificate first');
        exit;
    end;
    if SaveInterCerts.Checked then begin
        if NOT FSslCertTools.IsInterLoaded then begin
            DispError('Must load intermediate certificates first');
            exit;
        end;
    end;
    fname := BuildSaveName(SavePkcs7File.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.SaveToP7BFile(fname, SaveInterCerts.Checked);
        DispError('Saved certificate OK - ' + fname);
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doSavePrivKeyClick(Sender: TObject);
var
    fname: string;
    PrivKeyType: TSslPrivKeyCipher;   { V8.67 }
begin
    if SavePrivateKey.Checked then begin
        if NOT FSslCertTools.IsPKeyLoaded then begin
            DispError('Must load or create a private key first');
            exit;
        end;
    end;
    fname := BuildSaveName(SavePrvFileFile.Text);
    if fname = '' then Exit;
    PrivKeyType := PrivKeyEncNone;
    if CertPwPemKeys.Checked then begin   { V8.67 }
        PrivKeyType := TSslPrivKeyCipher(KeyEncrypt.ItemIndex);
        if PrivKeyType = PrivKeyEncNone then
            PrivKeyType := PrivKeyEncAES256;
    end;
    try
        FSslCertTools.PrivateKeySaveToPemFile(fname, SaveCertPW.Text, PrivKeyType); { V8.67 }
        DispError('Saved perivate key OK with ' +
                SslPrivKeyCipherLits[PrivKeyType] + ' key cipher: ' + fname);   { V8.67 }
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doSavePubKeyClick(Sender: TObject);
var
    fname: string;
begin
    if SavePrivateKey.Checked then begin
        if NOT FSslCertTools.IsPKeyLoaded then begin
            DispError('Must load or create a private key first');
            exit;
        end;
    end;
    fname := BuildSaveName(SavePubKeyFile.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.PublicKeySaveToPemFile(fname);
        DispError('Saved public key OK - ' + fname);
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doSaveReqCertClick(Sender: TObject);
var
    fname: string;
begin
    if NOT FSslCertTools.IsReqLoaded then begin
        DispError('Must load or create a certificate request first');
        exit;
    end;
    fname := BuildSaveName(SaveReqCertFile.Text);
    if fname = '' then Exit;
    try
        FSslCertTools.SaveReqToFile(fname, CertAddComment.Checked);
        DispError('Saved certificate request OK - ' + fname);
    except
         on E:Exception do
            DispError(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnRefreshClick(Sender: TObject);
begin
    FillListView;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnDeleteCertClick(Sender: TObject);
var
    ListItem : TListItem;
    FileName : String;
begin
    with LvCerts do begin
        if Assigned(Selected) then begin
            ListItem := Items[Selected.Index];
            FileName := ListItem.SubItems[3];
            if FileName <> '' then
                FileName := PathAddBackSlash(FCurrentCertDir) + FileName;
            if FileExists(FileName) then
                if MessageDlg('Delete Certificate ''' + ListItem.Caption + ''','
                            + #13#10
                            + 'file ''' + FileName + ''' now?',
                             mtWarning, [mbYes, mbNo], 0) <> mrYes then
                    Exit;
            if DeleteFile(FileName) then
                FillListView;
        end
        else
            MessageDlg('No item selected', mtError, [mbOK], 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.38 check digital signature on EXE and DLL files }
procedure TfrmPemTool1.btnCheckSignedClick(Sender: TObject);
var
    FileName, Info: String;
    RetCode: Integer;
begin
    OpenDlg.InitialDir := VerifyDir;
    OpenDlg.Filter     := 'Executable Files|*.exe;*.dll;*.ocx|All Files *.*|*.*';
    OpenDlg.Title      := 'Select executable file to check Digital Signature';
    if OpenDlg.Execute then
    begin
        FileName := OpenDlg.FileName;
        VerifyDir := ExtractFileDir(FileName);
        if (FileName = '') or not FileExists(FileName) then
            Exit;
        RetCode := IcsVerifyTrust (FileName, False, True, Info);
        MessageDlg('Checked Digital Signature for ' + FileName + #13#10#13#10 +
                   'Result: ' + Info + #13#10 +
                   'RetCode=' + IntToHex (RetCode, 8),
                   mtInformation, [mbOK], 0);
    end;
 end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnCopyCertClick(Sender: TObject);
var
    ListItem    : TListItem;
    FileName    : String;
    NewFileName : String;
    ClickedOK   : Boolean;
begin
    ClickedOk := FALSE;
    with LvCerts do begin
        if Assigned(Selected) then begin
            ListItem := Items[Selected.Index];
            FileName := ListItem.SubItems[3];
            if FileName <> '' then
                FileName := PathAddBackSlash(FCurrentCertDir) + FileName;
            if FileExists(FileName) then begin
                NewFileName := FileName;
                ClickedOK := InputQuery(ListItem.Caption,
                                        'Copy to: ', NewFileName);
            end;
            if ClickedOK and (CompareText(NewFileName, FileName) <> 0) then
                if CopyFile(PChar(FileName), PChar(NewFileName), TRUE) then
                    FillListView
                else
                    MessageDlg('''' + FileName + ''' ' + 'copy failed!',
                    mtError, [mbOK], 0);
        end
        else
            MessageDlg('No item selected', mtError, [mbOK], 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.About1Click(Sender: TObject);
var
    S: String;
begin
    S := PemToolName + #13#10 +
         CopyRight + ' ' + PemToolDate + #13#10 +
         Trim(OverbyteIcsWSocket.CopyRight)  + #13#10 +   { V8.53 }
         'SSL Version: ' + OpenSslVersion + #13#10;
    if NOT GSSLStaticLinked then                          { V8.66 }
        S := S + 'Dir: ' + GLIBEAY_DLL_FileName + #13#10
     else
        S := S + 'Statically Linked' + #13#10;
    if (ICS_OPENSSL_VERSION_MAJOR >= 3) then begin      { V8.67 }
        if ICS_OSSL3_LOADED_LEGACY then
            S := S + 'Legacy Provider Loaded OK' + #13#10
        else
            S := S + 'Legacy Provider Not Loaded' + #13#10;
    end;
    ShowMessage(S);
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnImportPemFileClick(Sender: TObject);
var
    X               : TX509Base;
    Subject_Hash    : Cardinal;
    FileName        : String;
begin
    OpenDlg.InitialDir := DestDirEdit.Text;
    OpenDlg.Filter     := SslCertFileOpenExts;  { V8.65 reset }
    if OpenDlg.Execute then
        FileName := OpenDlg.FileName;
    if (FileName = '') or not FileExists(FileName) then
        raise Exception.Create('FileName empty or file doesn''t exist');
    X := TX509Base.Create(nil);
    try
        X.LoadFromFile(FileName, croNo, croNo, CertPassword.Text);   { V8.40 was pemfile }
        Subject_Hash := X509_subject_name_hash(X.X509);
        FileName     := PathAddBackSlash(Trim(DestDirEdit.Text))
                        + IntToHex(Subject_Hash, 8) + '.0';

        if FileExists(FileName) then
            if MessageDlg('A certificate with the same subject already '
                           + 'exists in folder ''' + Trim(DestDirEdit.Text)
                           + ''' . Change file extension?' + #13#10
                           +'Click ''Yes'' to change file extension.' + #13#10
                           +'Click ''No'' to overwrite existing file.' + #13#10
                           +'Click ''Cancel'' to abort.',
                           mtWarning, [mbYes, mbNo, mbCancel], 0) = mrYES then

                FileName := FindPemFileName(FileName)
            else
                Exit;

            X.SaveToPemFile(FileName);
            ShowMessage('Certificate has been stored to ''' + FileName + '''');
    finally
        X.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FindPemFileName(const FileName: String): String;
var
    I         : Integer;
    FFileName : String;
    FExt      : String;
begin
    { If more than one CA certificate with the same name hash value exist, the }
    { extension must be different (e.g. 9d66eef0.0, 9d66eef0.1 etc). }
    FExt      := ExtractFileExt(FileName);
    FFileName := Copy(FileName, 1, length(FileName) -Length(FExt));
    I         := StrToInt(Copy(FExt, 2, MaxInt));
    Result    := FFileName + '.' + IntToStr(I);
    while FileExists(Result) do
    begin
       Inc(I);
       Result := FFileName + '.' + IntToStr(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DirectoryExists(const Name: string): Boolean; {from D5 FileCtrl.pas}
var
    Code: Integer;
begin
 {$R-}
    Code := GetFileAttributes(PChar(Name));
    Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
 {$R+}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsDirEmpty(const Path: String): Boolean;
var
    SRec : TSearchRec;
begin
    Result := FindFirst(PathAddBackSlash(Path) + '*.*',
                        FaAnyFile - faDirectory,
                        SRec) <> 0;
    if not Result then
        FindClose(SRec);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure EmptyDirectory(Path: String);
var
    SRec : TSearchRec;
begin
    Path := PathAddBackSlash(Path);
    if FindFirst(Path + '*.*', faAnyFile - faDirectory, SRec) = 0 then begin
        try
            DeleteFile(Path + SRec.Name);
            while FindNext(SRec) = 0 do
                DeleteFile(Path + SRec.Name);
        finally
            FindClose(SRec);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function PathAddBackSlash(const Path: String): String;
begin
    Result := Path;
    if Path[Length(Path)] <> '\' then
         Result := Path + '\';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMFileExitClick(Sender: TObject);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMExtrasCreateSelfSignedCertClick(Sender: TObject);
begin
    frmPemTool3.ToDo := tdCreateSelfSignedCert;
    frmPemTool3.Showmodal;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMExtrasCreateCertRequestClick(Sender: TObject);
begin
    frmPemTool3.ToDo := tdCreateCertRequest;
    frmPemTool3.Showmodal;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMExtrasEncryptStringRSAClick(Sender: TObject);
var
    Cert        : TX509Base;
    PemFileName : String;
    OldTitle    : String;
    Password    : String;
    S           : String;
begin
    S := 'This the plain text This the plain text This the plain text';
    if not InputQuery(Application.Title, 'Enter a string to encrypt:', S) then
        Exit; //***
    OldTitle := OpenDlg.Title;
    OpenDlg.Title := 'Select a PEM file containing both private and public key!';
    try
        OpenDlg.InitialDir := ExtractFileDir(Application.ExeName);
        if FileExists(OpenDlg.InitialDir + '\client.pem') then
            OpenDlg.FileName := 'client.pem';
        if OpenDlg.Execute then begin
            Password := 'password';
            if not InputQuery(Application.Title, 'Private key password:', Password) then
                Exit;
            PemFileName := OpenDlg.FileName;
        end
        else
            Exit; //***
    finally
        OpenDlg.Title := OldTitle;
    end;
    { We encrypt using the public key. }
    { Could also load a PEM file containing both private and public key. }
    Cert := TX509Base.Create(nil);
    try
        { Load a certificate (public key) from PEM file, private key must not exist }
        Cert.LoadFromPemFile(PemFileName);
        { Encrypted string is Base64 encoded }
        S := String(StrEncRsa(Cert.X509PublicKey, AnsiString(S), TRUE));   { V8.52 was PublicKey }
        ShowMessage('RSA encryted and Base64 encoded:'#13#10 + S);
    finally
        Cert.Free;
    end;
    { Decrypt using the private key. }
    Cert := TX509Base.Create(nil);
    try
        { Load a private key from PEM file }
        Cert.PrivateKeyLoadFromPemFile(PemFileName, Password);
        { Decrypt the Base64 encoded string }
        S := String(StrDecRsa(Cert.PrivateKey, AnsiString(S), TRUE));
        ShowMessage('Back to plain text again:'#13#10 + S);
    finally
        Cert.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMExtrasEncryptStringBlowfishClick(Sender: TObject);
var
    IV : TIVector;
    S  : AnsiString;
begin
  { if not LibeayExLoaded then
        LoadLibeayEx;  }
    S := 'This the plain text This the plain text This the plain text';
    RAND_bytes(@IV, SizeOf(IV));
    S := StrEncBF(S, 'password', @IV, cklDefault, TRUE);
    ShowMessage(String(S));
    S := StrDecBF(S, 'password', @IV, cklDefault, TRUE);
    ShowMessage(String(S));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMExtrasEncryptStreamBlowfishClick(Sender: TObject);
var
    S : AnsiString;
    Src, Dest : TStream;
    EncCtx, DecCtx : TCiphContext;
begin
    //SetLength(S, 2099);
    //FillChar(S[1], 2099, 'x');
    { Write NULLs, required!! }
    FillChar(EncCtx, SizeOf(EncCtx), #0);
    FillChar(DecCtx, SizeOf(DecCtx), #0);
    { We use one context for encryption and another one for decryption,    }
    { IV will be calculated from the password, key size default = 128-bits }
    CiphInitialize(EncCtx, 'password', nil, nil, {ctBfEcb ctBfOfb} ctBfCbc, cklDefault, True);
    CiphInitialize(DecCtx, 'password', nil, nil, {ctBfEcb ctBfOfb} ctBfCbc, cklDefault, False);
    try
        S := 'This the plain text This the plain text This the plain text';
        Src  := TMemoryStream.Create;
        Dest := TMemoryStream.Create;
        try
            { Populate the source stream }
            Src.WriteBuffer(S[1], Length(S));
            { Encrytion takes place here }
            StreamEncrypt(Src, Dest, 1024, EncCtx, False);
            { Just to display cipher text }
            SetLength(S, Dest.Size);
            Dest.Position := 0;
            Dest.Read(S[1], Length(S));
            ShowMessage(String(Base64Encode(S)));
            { Decrytion takes place here }
            StreamDecrypt(Dest, Src, 1024, DecCtx, False);
            { Just to display decrypted plain result }
            SetLength(S, Src.Size);
            Src.Position := 0;
            Src.Read(S[1], Length(S));
            ShowMessage(String(S));
        finally
            Src.Free;
            Dest.Free;
        end;
    finally
        CiphFinalize(EncCtx);
        CiphFinalize(DecCtx);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Progress(Obj: TObject; Count: Int64; var Cancel: Boolean);
begin
    TProgressBar(Obj).Position := Count;
    Application.ProcessMessages;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMExtrasEncryptFileBlowfishClick(Sender: TObject);
var
    Src, Dest   : TStream;
    EncCtx      : TCiphContext;
    OldTitle    : String;
    SrcFileName : String;
    DestFileName: String;
    Password    : String;
    OldFilter   : String;
begin
    OldFilter := OpenDlg.Filter;
    OldTitle := OpenDlg.Title;
    OpenDlg.Filter := 'All Files *.*|*.*';
    OpenDlg.Title := 'Select a file to encrypt!';
    try
        OpenDlg.InitialDir := ExtractFileDir(Application.ExeName);
        if OpenDlg.Execute then begin
            SrcFileName := OpenDlg.FileName;
            DestFileName := ChangeFileExt(SrcFileName, '_ENC' + ExtractFileExt(SrcFileName));
            if MessageDlg('Save encrypted file as "' + DestFileName + '" ?',
                          mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
                Exit; //***
        end
        else
            Exit; //***
        Password := 'password';
        if not InputQuery(Application.Title, 'Password:', Password) then
            Exit; //***
    finally
        OpenDlg.Filter := OldFilter;
        OpenDlg.Title := OldTitle;
    end;
    FillChar(EncCtx, SizeOf(EncCtx), #0);

    CiphInitialize(EncCtx, AnsiString(Password), nil, nil, ctBfCbc, cklDefault, True);
    try
        Src  := TFileStream.Create(SrcFileName, fmOpenRead or fmShareDenyWrite);
        Dest := TFileStream.Create(DestFileName, fmCreate);
        try
            ProgressBar1.Max := Src.Size;
            ProgressBar1.Position := 0;
            ProgressBar1.Visible := TRUE;
            StreamEncrypt(Src, Dest, 1024 * 4, EncCtx, False, ProgressBar1, Progress);
            ShowMessage('File encrypted');
        finally
            Src.Free;
            Dest.Free;
        end;
    finally
        CiphFinalize(EncCtx);
        ProgressBar1.Visible := False;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMExtrasDecryptFileBlowfishClick(Sender: TObject);
var
    Src, Dest : TStream;
    DecCtx : TCiphContext;
    OldTitle    : String;
    SrcFileName : String;
    DestFileName: String;
    Password    : String;
    OldFilter   : String;
begin
    OldFilter := OpenDlg.Filter;
    OldTitle := OpenDlg.Title;
    OpenDlg.Filter := 'All Files *.*|*.*';
    OpenDlg.Title := 'Select a file to decrypt!';
    try
        OpenDlg.InitialDir := ExtractFileDir(Application.ExeName);
        if OpenDlg.Execute then begin
            SrcFileName := OpenDlg.FileName;
            DestFileName := ChangeFileExt(SrcFileName, '_DEC' + ExtractFileExt(SrcFileName));
            if MessageDlg('Save decrypted file as "' + DestFileName + '" ?',
                          mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
                Exit; //***
        end
        else
            Exit; //***
        Password := 'password';
        if not InputQuery(Application.Title, 'Password:', Password) then
            Exit; //***
    finally
        OpenDlg.Filter := OldFilter;
        OpenDlg.Title := OldTitle;
    end;
    FillChar(DecCtx, SizeOf(DecCtx), #0);
    CiphInitialize(DecCtx, AnsiString(Password), nil, nil, ctBfCbc, cklDefault, False);
    try
        Src  := TFileStream.Create(SrcFileName, fmOpenRead or fmShareDenyWrite);
        Dest := TFileStream.Create(DestFileName, fmCreate);
        try
            ProgressBar1.Max := Src.Size;
            ProgressBar1.Position := 0;
            ProgressBar1.Visible := TRUE;
            StreamDecrypt(Src, Dest, 1024 * 4, DecCtx, False, ProgressBar1, Progress);
            ShowMessage('File decrypted');
        finally
            Src.Free;
            Dest.Free;
        end;
    finally
        CiphFinalize(DecCtx);
        ProgressBar1.Visible := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMResaveKeyClick(Sender: TObject);
var
    FileName, ErrMsg : String;
begin
    if OpenDlg.InitialDir = '' then OpenDlg.InitialDir := DestDirEdit.Text;
    OpenDlg.Filter     := SslCertFileOpenExts;  { V8.65 reset }
    if OpenDlg.Execute then
        FileName := OpenDlg.FileName;
    if (FileName = '') or not FileExists(FileName) then
        Exit;
    OpenDlg.InitialDir := ExtractFileDir(FileName);
    ShowLogWindow;
    AddLog('Resaving file with ' + SslPrivKeyCipherLits[
                TSslPrivKeyCipher(KeyEncrypt.ItemIndex)] + ' key cipher: ' + FileName);
    if ResavePrivateKey(FileName, SaveCertPW.Text,
                    TSslPrivKeyCipher(KeyEncrypt.ItemIndex), ErrMsg) then
    begin
        AddLog(ErrMsg);
    end
    else
        AddLog(ErrMsg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ resave a PEM or PFX file containing an encrypted private key
  -BEGIN ENCRYPTED PRIVATE KEY- with a new cipher, renaming old file to .oldpem/pfx.
  Specifically for files saved with old ciphers than OpenSSL 3.0 does not support
  as standard, should be used with OpenSSL 1.1.1 before upgrading.  }
function ResavePrivateKey(const FName, PW: String; KeyCipher: TSslPrivKeyCipher;
                                                        var ErrMsg: String): Boolean;   { V8.67 }
var
    Cert: TX509Base;
    NewFName: String;
begin
    ErrMsg := '';
    Result := False;
    NewFName := '';
    if (FName = '') or not FileExists(FName) then begin
        ErrMsg := 'File name empty or file doesn''t exist: ' + FName;
        Exit;
    end;
    Cert := TX509Base.Create(nil);
    try
        try
            Cert.LoadFromFileEx(FName, croTry, croTry, PW, ErrMsg);
            if ErrMsg <> '' then Exit;
            if NOT Cert.IsPKeyLoaded then begin
                Cert.ClearAll;
                Cert.PrivateKeyLoadFromPemFile(FName, PW);
                if NOT Cert.IsPKeyLoaded then begin
                    ErrMsg := 'No private key found in file: ' + FName;
                    Exit;
                end;
            end;
            NewFName := ChangeFileExt(FName, '.old' + Copy(ExtractFileExt(FName), 2, 99));
            IcsRenameFile(FName, NewFName, True, True);

        // private key only
            if NOT Cert.IsCertLoaded then begin
                Cert.PrivateKeySaveToPemFile(FName, PW, KeyCipher);
            end
            else begin
                Cert.SaveToFile(FName, True, True, Cert.IsInterLoaded, PW, KeyCipher);
            end;
            NewFName := '';
            Result := True;
            ErrMsg := 'Resaved OK, old file now: ' + NewFName + IcsCRLF + Cert.CertInfo;
            exit;
        except
            on E:Exception do
            begin
                ErrMsg := Trim(E.Message);
            end;
        end;
    // failure, try and restore old file we renamed
        if (NewFName <> '') and (NOT FileExists(FName)) then
                    IcsRenameFile(NewFName, FName, True, True);
    finally
        Cert.Free;
    end;
 end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnImportClick(Sender: TObject);
var
    MsX509List: TMsX509List;
    Subject_Hash    : Cardinal;
    BundleBio       : PBIO;
    FileName        : String;
    Path            : String;
    BundleFilename  : String;
    BundlePath      : String;
    Count, Total, I : Integer;
begin
    Count := 0;
    Path  := Trim(DestDirEdit.Text);

    if (Path = '') or (not DirectoryExists(Path)) then begin
        ForceDirectories(Path); // Angus
    end;
    if CheckBoxWarnDestNotEmpty.Checked then
        if not isDirEmpty(Path) then
            if MessageDlg('Directory ''' + Path + ''' is not empty. Continue?',
                          mtWarning,
                          [mbYes, mbNo],
                          0) <> mrYes then
                Exit;

    if CheckBoxEmptyDestDir.Checked then begin
        if MessageDlg('Any file in destination ''' + Path
                     + ''' will be deleted. Continue?',
                      mtWarning,
                      [mbYes, mbNo],
                      0) <> mrYes then
            Exit;
        EmptyDirectory(Path);
    end;

 { V8.67 rewritten to use TMsX509List instead of Windows API calls }
    MsX509List := TMsX509List.Create(nil, True);
    BundleBio := Nil;
    try
        Total := MsX509List.LoadFromStore(TMsCertStoreType(MsStoreType.ItemIndex), TMsCertLocation(MsLocationType.ItemIndex), True);
        if Total <> 0 then
            ShowMessage('Opened the '''
                       + MsStoreType.Items[MsStoreType.ItemIndex]
                       + ''' system store.')
        else begin
            ShowMessage('Could not open the '''
                       + MsStoreType.Items[MsStoreType.ItemIndex]
                       + ''' system store.');
            Exit;
        end;

        if CheckBoxWriteToBundle.Checked then begin
            BundlePath:= IncludeTrailingPathDelimiter(Path) + 'Bundled certs';
            ForceDirectories(BundlePath);
            BundlePath:= IncludeTrailingPathDelimiter(BundlePath);
            BundleFilename := BundlePath + MsCertStoreNames[TMsCertStoreType(MsStoreType.ItemIndex)] + 'CertsBundle.pem';
            { opens text file, adds CR to LF }
            BundleBio := BIO_new_file(Pointer(AnsiString(BundleFilename)), PAnsiChar('w+'));
        end;

    { Enum all the certs in the store and store them in PEM format }
        for I := 1 to Total do begin
            Subject_Hash := X509_subject_name_hash(MsX509List[I-1].X509);
            FileName := PathAddBackSlash(Path) + IntToHex(Subject_Hash, 8) + '.0';
            if not CheckBoxOverwriteExisting.Checked then
                if FileExists(FileName) then
                    FileName := FindPemFileName(FileName);
            MsX509List[I-1].SaveToPemFile(FileName, False, CheckBoxComment.Checked);  { V8.41 does it all now }
            Inc(Count);
              // save to bundle also
            if (Assigned(BundleBio)) and CheckBoxWriteToBundle.Checked then begin
                MsX509List[I-1].WriteCertToBio(BundleBio, CheckBoxComment.Checked, BundleFilename);   { V8.41 does it all now }
                MsX509List[I-1].WriteStrBio(BundleBio, #10#10);  { blank lines between certs }
            end;
        end;
        ShowMessage(IntToStr(Count) + ' Certificates exported.');
    finally
        if Assigned(BundleBio) then
            BIO_free(BundleBio);
        MsX509List.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.67 display certificates and private keys from a Windows certificate store and location }
procedure TfrmPemTool1.doMsDisplayClick(Sender: TObject);
var
    Total, I: Integer;
    Info, Item: String;
begin
    if NOT Assigned(FMsX509List) then
        FMsX509List := TMsX509List.Create(nil, True);
    FMsX509List.Clear;
    Total := FMsX509List.LoadFromStore(TMsCertStoreType(MsStoreType.ItemIndex), TMsCertLocation(MsLocationType.ItemIndex), True);
    ShowLogWindow;
    AddLog ('Microsoft Certificate Store: ' + MsStoreType.Items[MsStoreType.ItemIndex] +
                                                                ' - ' + MsLocationType.Items[MsLocationType.ItemIndex]);
    CertStoreList.Items.Clear;
    if FMsX509List.MsLastError <> '' then
         AddLog ('Errors reading store: ' + FMsX509List.MsLastError);
    if Total <= 0 then begin
        AddLog ('No certificates found in store');
    end
    else begin
        AddLog ('Number of certificates found in store: ' + IntToStr (Total));
        for I := 0 to Total - 1 do begin
            Item := FMsX509List [I].CertMainInfo;
            CertStoreList.Items.Add (Item);
            Info := 'Certificate #' + IntToStr(I+1) + IcsSpace;
            if MSBriefList.Checked then
                Info := Info + Item
            else
                Info := Info + IcsCRLF + ListCertDetail(FMsX509List [I]);
            Info := Info + IcsCRLF + 'Friendly Name: ' + FMsX509List [I].CertName;
            if FMsX509List [I].Comments <> '' then
                Info := Info + ', Key Name: ' + FMsX509List [I].KeyName + ' - ' + FMsX509List [I].Comments;
            AddLog (Info);
           if FMsX509List [I].IsPKeyLoaded then begin
                if (FMsX509List [I].CheckCertAndPKey) then
                    AddLog ('!! Private key available for certificate: ' + FMsX509List [I].KeyInfo)
                else
                    AddLog ('!! Private key available but does not match certificate');
            end;
        end;
    end;
    AddLog ('');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ load one store certificate into FSslCertTools so we can save as as other formats }
procedure TfrmPemTool1.doLoadtoCreateClick(Sender: TObject);
begin
    if CertStoreList.ItemIndex < 0 then begin
        DispError('No certificate store item selected');
        Exit;
    end;
    try
        FSslCertTools.ClearAll;
        FSslCertTools.X509 := FMsX509List [CertStoreList.ItemIndex].X509;
        FSslCertTools.PrivateKey := FMsX509List [CertStoreList.ItemIndex].PrivateKey;
        FSslCertTools.Comments := FMsX509List [CertStoreList.ItemIndex].Comments;
        if FSslCertTools.IsCertLoaded then begin
            DispError('Loaded cerfificate OK - ' + FSslCertTools.Comments);
            DispCert;
            if LoadCertPrivKey.Checked then begin
                DispError('Loaded cerfificate and key OK - ' + FSslCertTools.Comments);
                DispPKey;
            end;
            PageControl1.ActivePage := TabCreateCerts;
        end
        else
            DispError('Failed to loaded cerfificate from store');
    except
        on E:Exception do
            DispError(E.Message);
    end;

end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ delete selected store certificate and private key  }
procedure TfrmPemTool1.doCertStoreDelClick(Sender: TObject);
var
    CName: String;
begin
    ShowLogWindow;
    if CertStoreList.ItemIndex < 0 then begin
        AddLog('No certificate store item selected');
        Exit;
    end;
    try
        CName := FMsX509List [CertStoreList.ItemIndex].CertName;
        if FMsX509List.DeleteFromStore(FMsX509List [CertStoreList.ItemIndex].Sha1Digest,
                          TMsCertStoreType(MsStoreType.ItemIndex),
                                    TMsCertLocation(MsLocationType.ItemIndex), true) then begin
            AddLog('Deleted cerfificate from store OK - ' + CName);
            AddLog ('');
            doMsDisplayClick(Self);  // refresh store window
        end ;
        if FMsX509List.MsLastError <> '' then
            AddLog('Failed to delete cerfificate from store - ' + FMsX509List.MsLastError);
    except
        on E:Exception do
            AddLog(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ load My/Personal store into a TMsCertTools, one main cert, privaste key and lots of intermediates }
procedure TfrmPemTool1.doLoadMOneClick(Sender: TObject);
var
    MsCertTools: TMsCertTools;
    Info, Item: String;
    Total, I: Integer;
    InterCerts: TX509List;
begin
    ShowLogWindow;
    if (NOT IcsIsProgAdmin) and (TMsCertLocation(MsLocationType.ItemIndex) = MsLocMachine) then begin
        AddLog ('Program needs administrator rights to access local machine store');
        Exit;
    end;
    MsCertTools := TMsCertTools.Create(self);
    InterCerts := TX509List.Create(self);
    InterCerts.X509Class := TX509Base;
    try
        ShowLogWindow;
        CertStoreList.Items.Clear;
        try
            Total := MsCertTools.LoadFromMyStore(TMsCertLocation(MsLocationType.ItemIndex));
        except
            on E:Exception do begin
                AddLog (E.Message);
                exit;
            end;
        end;
        AddLog ('Microsoft Certificate Personal/My Store - via PFX/P12');
        if NOT MsCertTools.IsCertLoaded then begin
            AddLog ('No certificates found in store');
        end
        else begin
            AddLog ('Number of certificates found in store: ' + IntToStr (Total));
            Item := MsCertTools.CertMainInfo;
            CertStoreList.Items.Add (Item);
            Info := Info + IcsCRLF + ListCertDetail(MsCertTools);
            AddLog (Info);
            if MsCertTools.IsPKeyLoaded then begin
                if (MsCertTools.CheckCertAndPKey) then
                    AddLog ('!! Private key available for certificate: ' + MsCertTools.KeyInfo)
                else
                    AddLog ('!! Private key available but does not match certificate');
            end;
            Total := MsCertTools.InterCount;
            if Total > 0 then begin
                InterCerts.LoadAllStack(MsCertTools.X509Inters);
                for I := 1 to Total do begin
                    Item := InterCerts [I-1].CertMainInfo;
                    CertStoreList.Items.Add (Item);
                    Info := 'Inter Certificate #' + IntToStr(I) + IcsSpace;
                    if MSBriefList.Checked then
                        Info := Info + Item
                    else
                        Info := Info + IcsCRLF + ListCertDetail(InterCerts [I-1]);
                    AddLog (Info);
                end;
            end;
        end;
        AddLog ('');
        MsCertTools.Free;
        MsCertTools := Nil;
    finally
        MsCertTools.Free;
        InterCerts.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelInstallFileClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := ExtractFilePath(MsInstallFile.Text) ;
    if OpenDirDiag.Execute then
        MsInstallFile.Text := OpenDirDiag.FileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ install a certificate bundle into a selected Windows store }
procedure TfrmPemTool1.doMsInstallFileClick(Sender: TObject);
var
    MsCertTools: TMsCertTools;
    Errs: String;
begin
    ShowLogWindow;
    if (NOT IcsIsProgAdmin) and (TMsCertLocation(MsLocationType.ItemIndex) = MsLocMachine) then begin
        AddLog ('Program needs administrator rights to access local machine store');
        Exit;
    end;
    if NOT FileExists(MsInstallFile.Text) then
        AddLog ('Certificates file not found: ' + MsInstallFile.Text)
    else begin
        MsCertTools := TMsCertTools.Create(self);
        try
            try
                MsCertTools.LoadFromFileEx(MsInstallFile.Text, croTry, croTry,
                                                            CertPassword.Text, Errs);
                MsCertTools.SaveToStorePfx(TMsCertLocation(MsLocationType.ItemIndex), True, True);
                AddLog ('Saved certificate to Windows Store OK');
                AddLog ('');
                doMsDisplayClick(Self);  // refresh store window
            except
                on E:Exception do AddLog (E.Message);
            end;
        finally
            MsCertTools.Free;
         end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ list private key names in a selected Windows store }
procedure TfrmPemTool1.doListPkeysClick(Sender: TObject);
var
    I: Integer;
    Info: String;
begin
    ShowLogWindow;
    PkeyList.Items.Clear;
    FStorePkeyTot := 0;
    if (NOT IcsIsProgAdmin) and (TMsCertLocation(MsLocationType.ItemIndex) = MsLocMachine) then begin
        AddLog ('Program needs administrator rights to access local machine keys');
        Exit;
    end;
    if NOT Assigned(FMsX509List) then
        FMsX509List := TMsX509List.Create(nil, True);
    SetLength(FStorePkeyInfos, 1);
    FMsX509List.MsKeyProvider := MsKeyProviderNames[TMsKeyProvider(MSKeyStoreType.ItemIndex)];
    FStorePkeyTot := FMsX509List.ListPKeys(TMsCertLocation(MsLocationType.ItemIndex), FStorePkeyInfos);
    ShowLogWindow;
    AddLog ('Microsoft ' +  MSKeyStoreType.Items[MSKeyStoreType.ItemIndex] +
                                                ': ' + MsLocationType.Items[MsLocationType.ItemIndex]);
    if FMsX509List.MsLastError <> '' then
         AddLog ('Errors reading key store: ' + FMsX509List.MsLastError);
    if FStorePkeyTot <= 0 then begin
        AddLog ('No private keys found in store');
    end
    else begin
        AddLog ('Number of private keys found in store: ' + IntToStr (FStorePkeyTot));
        for I := 0 to FStorePkeyTot - 1 do begin
            Info := FStorePkeyInfos[I].KeyName + ' - ' + FStorePkeyInfos [I].KeyAlg;
            PkeyList.Items.Add(Info);
            AddLog (Info);
        end;
    end;
    AddLog ('');
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ delete private key names from a selected Windows store }
procedure TfrmPemTool1.doKeyStoreDelClick(Sender: TObject);
var
    CName: String;
begin
    ShowLogWindow;
    if PkeyList.ItemIndex < 0 then begin
        AddLog('No private key store item selected');
        Exit;
    end;
    try
        CName := FStorePkeyInfos [PkeyList.ItemIndex].KeyName;
        if FMsX509List.DeletePKey(CName, TMsCertLocation(MsLocationType.ItemIndex)) then begin
            AddLog('Deleted private key from store OK - ' + CName);
            AddLog ('');
            doListPkeysClick(Self);  // refresh store window
        end ;
        if FMsX509List.MsLastError <> '' then
            AddLog('Failed to delete private key from store - ' + FMsX509List.MsLastError);
    except
        on E:Exception do
            AddLog(E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.IcsIpClientLogChangeEvent(Sender: TObject; Socnr: Integer; LogState: TStrmLogState);
begin
    case LogState of
        logstateNone: PostMessage(Handle, WM_IPLOGCONNECT, 0, 0);   // disconnect done, try another host
        logstateStart: ;
        logstateHandshake: ;
        logstateOK: PostMessage(Handle, WM_IPLOGDISCONNECT, 0, 0);  // immediate disconnect
        logstateOKStream: ;
        logstateStopping: PostMessage(Handle, WM_IPLOGDISCONNECT, 0, 0);  // immediate disconnect
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.WMIpLogConnect(var Msg: TMessage);
begin
    FTestHostCur := FTestHostCur + 1;
    if (FTestHostCur >= FTestHostTot) or (FTestHostCur >= TestHostList.Lines.Count) then begin
        AddLog('Finished Testing Hosts');
         AddLog('');
        doTestOneCert.Enabled := True;
        doTestListCerts.Enabled := True;
        if DownloadCerts.Items.Count > 1 then
            DownloadCerts.ItemIndex := 0;
        if DownloadInters.Items.Count > 1 then
            DownloadInters.ItemIndex := 0;
        Exit;
    end;
    TestHostCert(TestHostList.Lines[FTestHostCur]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.WMIpLogDisconnect(var Msg: TMessage);
begin
    IcsIpClient.StopLogging;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.IcsIpClientLogHandshakeDone(Sender: TObject; ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
var
    I: Integer;
begin
    if ErrCode <> 0 then Exit;

 // build lists of server and intermediate certificates so we can look at them and save them
    try
        with Sender as TSslWSocket do begin
            if NOT Assigned (SslCertChain) then Exit;
            I := SslCertChain.Count - 1;
            if I < 0 then Exit;
            if DownCertsList.IndexOfSubj(SslCertChain[I].SubjectOneLine) >= 0 then Exit;
            DownCertsList.Add(SslCertChain[I].X509);
            DownloadCerts.Items.Add(SslCertChain[I].CertMainInfo);
            while I >= 1 do begin
                I := I - 1;
                if SslCertChain[I].SelfSigned then continue;    // skip trusted roots
//                if DownIntersList.IndexOf(SslCertChain[I]) < 0 then begin
                if DownIntersList.IndexOfSubj(SslCertChain[I].SubjectOneLine) < 0 then begin
                    DownIntersList.Add(SslCertChain[I].X509);
                    DownloadInters.Items.Add(SslCertChain[I].CertMainInfo);
                end;
            end;
        end;
    except
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.IcsIpClientLogProgEvent(Sender: TObject; Socnr: Integer; LogOption: TLogOption; const Msg: string);
begin
    AddLog(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.IcsIpClientLogRecvEvent(Sender: TObject; Socnr: Integer; const Line: string);
begin
    // ignore received data, should be none
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.TestHostCert(const HostPort: String);
var
    I: Integer;
    Host: String;
begin
    if (Pos('.', HostPort) = 0) or (Pos('*', HostPort) = 1) or (Pos(';', HostPort) = 1) then begin
        PostMessage(Handle, WM_IPLOGCONNECT, 0, 0);
        Exit;
    end;
    doTestOneCert.Enabled := False;
    doTestListCerts.Enabled := False;
    IcsIpClient.SocFamily := sfAny;
    IcsIpClient.ForceSsl := True;
    IcsIpClient.RetryAttempts := -1;  // no retries
    IcsIpClient.LogSslVerMethod := logSslVerBundle;
    IcsIpClient.LogSslRevocation := True;
    IcsIpClient.LogSslReportChain := True;
    IcsIpClient.LogSslCliSecurity := sslCliSecTls12;
    IcsIpClient.OcspHttp.CacheFName := 'OcspPemCache.recs';
    IcsIpClient.OcspHttp.CacheStapled := True;
    IcsIpClient.OcspHttp.CacheFlushMins := 2;
    IcsIpClient.OcspHttp.CacheRefrDays := 3;
    IcsIpClient.OcspHttp.OcspStapleOnly := False;
    IcsIpClient.OcspHttp.OcspHttpProxy := '';

    Host := Lowercase(Trim(HostPort));
    if Pos('https://', Host) = 1 then
        Host := Copy(Host, 9, 999);
    I := Pos(':', Host);
    if I > 1 then begin
        IcsIpClient.RemoteHost := Copy(Host, 1, I - 1);
        IcsIpClient.RemoteIpPort := Copy(Host, I + 1, 9);
    end
    else begin
        IcsIpClient.RemoteHost := Host;
        IcsIpClient.RemoteIpPort := '443';
    end;
    IcsIpClient.LogTitle := IcsIpClient.RemoteHost + ':' + IcsIpClient.RemoteIpPort;
    AddLog('Start Testing Host: ' + IcsIpClient.LogTitle);
    IcsIpClient.StartLogging;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doTestAbortClick(Sender: TObject);
begin
    if doTestListCerts.Enabled then Exit;
    FTestHostCur := FTestHostTot + 1;
    IcsIpClient.StopLogging;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doTestListCertsClick(Sender: TObject);
begin
    ShowLogWindow;
    FTestHostTot := TestHostList.Lines.Count;
    if FTestHostTot = 0 then Exit;
    FTestHostCur := 0;
    AddLog('Start Testing Host List');
    TestHostCert(TestHostList.Lines[0]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doTestOneCertClick(Sender: TObject);
begin
    ShowLogWindow;
    FTestHostTot := 1;
    FTestHostCur := 0;
    AddLog('Start Testing One Host');
    TestHostCert(TestHostOne.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.DownloadCertsDblClick(Sender: TObject);
var
    I: Integer;
begin
    I := DownloadCerts.ItemIndex;
    if I < 0 then Exit;
    if I >= DownCertsList.Count then Exit;
    AddLog (ListCertDetail(DownCertsList[I]) + #13#10);
    AddLog(ListOcspStatus(DownCertsList[I]));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.DownloadIntersDblClick(Sender: TObject);
var
    I: Integer;
begin
    I := DownloadInters.ItemIndex;
    if I < 0 then Exit;
    if I >= DownIntersList.Count then Exit;
    AddLog (ListCertDetail(DownIntersList[I]) + #13#10);
    AddLog(ListOcspStatus(DownIntersList[I]));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelDownHostPathClick(Sender: TObject);
begin
    OpenDlg.InitialDir := DownCertsPath.Text;
    if OpenDlg.Execute then begin
        DownCertsPath.Text := ExtractFilePath(OpenDlg.FileName);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.SelDownIntersClick(Sender: TObject);
begin
    OpenDlg.InitialDir := DownIntersPath.Text;
    if OpenDlg.Execute then begin
        DownIntersPath.Text := ExtractFilePath(OpenDlg.FileName);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doAddInterBundleClick(Sender: TObject);
var
    Tot, I, Mode: Integer;
    FName, Info: String;
    BundleFile: TFileStream;

    procedure WriteLine(const S: String);
    var
        utftext: RawByteString;
    begin
        utftext := StringToUtf8(S) + IcsCRLF;
        BundleFile.Write(Pointer(utftext)^, Length(utftext));
    end;

begin
    if DownIntersList.Count = 0 then Exit;
    FName := IntersCAFile.Text;
    if FName = '' then Exit;
    if InterCAList.Count = 0 then LoadInterCA;
    Mode := fmOpenReadWrite;
    if NOT FileExists(FName)then Mode := fmCreate;
    try
        BundleFile := TFileStream.Create(FName, Mode);
    except
        on E:Exception do begin
            AddLog('Failed to Open Bundle Files: ' + Fname + ' - ' + E.Message);
            Exit;
        end;
    end;
    Tot := 0;
    BundleFile.Seek(0, soEnd);
    for I := 0 to DownIntersList.Count - 1 do begin
        if DownloadInters.Checked[I] then begin
            if InterCAList.FindDigest(DownIntersList[I].Sha1Digest) = Nil then begin
                InterCAList.Add(DownIntersList[I].X509);
                Info := DownIntersList[I].SaveCertToText(True) + IcsCRLF;
                WriteLine(Info);
                AddLog('Added Certificate: ' + DownIntersList[I].SubjectCName + ' to Bundle');
                DownloadInters.Checked[I] := False;
                Tot := Tot + 1;
            end;
        end;
    end;
    BundleFile.Free;  // close file
    if Tot > 0 then
        AddLog('Finshed Saving ' + IntToStr (Tot) + ' Certificates to File: ' +  FName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doSaveIntersClick(Sender: TObject);
var
    I, Tot: Integer;
    FName: String;
begin
    if DownIntersList.Count = 0 then Exit;
    if NOT IcsDirExists(DownIntersPath.Text) then begin
        AddLog('Download Path Not Found: ' + DownIntersPath.Text);
        Exit;
    end;
    Tot := 0;
    for I := 0 to DownIntersList.Count - 1 do begin
        if DownloadInters.Checked[I] then begin
            FName := PathAddBackSlash(DownIntersPath.Text) +
                        IcsLowerCase(DownIntersList[I].Sha256Hex) + '.pem';
            try
                if FileExists(FName) then
                    DeleteFile(FName);
                DownIntersList[I].SaveToPemFile(FName, False, True);
                AddLog('Saved Certificate: ' + DownIntersList[I].SubjectCName + ' as ' + Fname);
                DownloadInters.Checked[I] := False;
                Tot := Tot + 1;
            except
                AddLog('Failed to Save Certificate: ' + DownIntersList[I].SubjectCName + ' as ' + Fname);
            end;
        end;
    end;
    AddLog('Finshed Saving ' + IntToStr (Tot) + ' Certificate Files');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.doHostSaveClick(Sender: TObject);
var
    Tot, I: Integer;
    FName: String;
begin
    if DownCertsList.Count = 0 then Exit;
    if NOT IcsDirExists(DownCertsPath.Text) then begin
        AddLog('Download Path Not Found: ' + DownCertsPath.Text);
        Exit;
    end;
    Tot := 0;
    for I := 0 to DownCertsList.Count - 1 do begin
        if DownloadCerts.Checked[I] then begin
            FName := PathAddBackSlash(DownCertsPath.Text) +
                        IcsLowerCase(DownCertsList[I].Sha256Hex) + '.pem';
            try
                if FileExists(FName) then
                    DeleteFile(FName);
                DownCertsList[I].SaveToPemFile(FName, False, True);
                AddLog('Saved Certificate: ' + DownCertsList[I].SubjectCName + ' as ' + Fname);
                DownloadCerts.Checked[I] := False;
                Tot := Tot + 1;
            except
                AddLog('Failed to Save Certificate: ' + DownCertsList[I].SubjectCName + ' as ' + Fname);
            end;
        end;
    end;
    AddLog('Finshed Saving ' + IntToStr (Tot) + ' Certificate Files');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
