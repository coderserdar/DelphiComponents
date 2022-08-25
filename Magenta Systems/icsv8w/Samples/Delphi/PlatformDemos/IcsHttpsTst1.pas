{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Feb 15, 2003
Description:  A simple HTTPS client.
              Make use of OpenSSL (http://www.openssl.org).
              Make use of freeware TSslHttpCli and TSslWSocket components
              from ICS (Internet Component Suite).
Version:      8.68
EMail:        francois.piette@overbyte.be         http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1997-2021 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany

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
Oct 22, 2005  V1.03 Arno Garrels implemented client-side SSL session caching,
              alternate verify methode, and support for dynamicaly provided
              client certificates.
Nov 08, 2005  V1.04 Arno Garrels adjusted a few type casts in some event
              handlers. Put in OpenSSL version info.
Dec 20, 2005  V1.05 Angus Robertson added new LogOptions and GZIP decompression
                and display more log lines
Jul 18, 2008  V1.06 A. Garrels fixed an AV in SslHttpCli1SslCliCertRequest
May 2012 - V8.00 - Arno converted demo for FireMonkey cross platform Mac
                   OS X support, now XE2 and later only uising FMX components
Dec 09, 2020 V8.65 Added DEFINE FMX.
                   Renamed Ics.Posix.Messages.pas to Ics.Posix.PXMessages.pas.
Dec 21, 2021 V8.68 Builds again, SSL issue.
                   Added OpenSSL initialisations.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit IcsHttpsTst1;
{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{$DEFINE USE_MODEZ}
{$I Include\OverbyteIcsDefs.inc}
{$DEFINE FMX}

interface

{$IF CompilerVersion < 23}
  {$MESSAGE FATAL 'This project requires Delphi or RAD Studio XE2 or better'};
{$IFEND}
{$IFNDEF USE_SSL}
  {$MESSAGE FATAL 'Please add "USE_SSL" to project option''s defines'};
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
  WinApi.Messages,
{$ENDIF}
{$IFDEF POSIX}
  Ics.Posix.WinTypes,
  Ics.Posix.PXMessages,
{$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.IOUtils, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.Layouts, FMX.Memo, FMX.Edit, FMX.ListBox, FMX.TabControl,
  FMX.StdCtrls, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox,
  OverbyteIcsWndControl,
  OverbyteIcsWSocket,
  OverbyteIcsLIBEAY,
  OverbyteIcsSsLeay,
  OverbyteIcsSslSessionCache,
  OverbyteIcsLogger,
  OverbyteIcsIniFiles,
  OverbyteIcsUtils,
{$IFDEF USE_MODEZ}              { V2.102 }
  OverbyteIcsHttpCCodZLib,
{$ENDIF}
  OverbyteIcsHttpProt,
  OverbyteIcsCookies;


const
     HttpsTstVersion     = 868;
     HttpsTstDate        = 'Oct 14, 2021';
     HttpsTstName        = 'HttpsTst';
     CopyRight : String  = ' HttpsTst (c) 2005-2021 Francois Piette V8.68 ';

     WM_SSL_NOT_TRUSTED  = WM_USER + 1;

type
  THttpsTstForm = class(TForm)
    DisplayMemo: TMemo;
    SslHttpCli1: TSslHttpCli;
    DocumentMemo: TMemo;
    Splitter1: TSplitter;
    ToolsPanel: TPanel;
    Label1: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    Label10: TLabel;
    Label6: TLabel;
    Label4: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label5: TLabel;
    Label11: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    UrlEdit: TEdit;
    SocksServerEdit: TEdit;
    SocksPortEdit: TEdit;
    DocEdit: TEdit;
    CertFileEdit: TEdit;
    CAFileEdit: TEdit;
    VerifyPeerCheckBox: TCheckBox;
    CAPathEdit: TEdit;
    PrivKeyFileEdit: TEdit;
    PassPhraseEdit: TEdit;
    AcceptableHostsEdit: TEdit;
    SocksLevelComboBox: TComboBox;
    GetButton: TButton;
    ClearButton: TButton;
    CloseButton: TButton;
    ProxyHostEdit: TEdit;
    ProxyPortEdit: TEdit;
    SslContext1: TSslContext;
    SessCacheCheckBox: TCheckBox;
    Label17: TLabel;
    ButtonOSSLVersion: TButton;
    IcsLogger1: TIcsLogger;
    Label18: TLabel;
    DateTimeEdit: TEdit;
    HeadButton: TButton;
    AbortButton: TButton;
    SslAvlSessionCache1: TSslAvlSessionCache;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    DebugOutputCheckBox: TCheckBox;
    DebugEventCheckBox: TCheckBox;
    DebugFileCheckBox: TCheckBox;
    HttpVersionComboBox: TComboBox;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    IcsCookies: TIcsCookies;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure GetButtonClick(Sender: TObject);
    procedure SslHttpCli1SslVerifyPeer(Sender: TObject; var Ok: Integer;
      Cert : TX509Base);
    procedure FormDestroy(Sender: TObject);
    procedure SslHttpCli1RequestDone(Sender: TObject; RqType: THttpRequest;
      ErrCode: Word);
    procedure ClearButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure SslHttpCli1Command(Sender: TObject; var S: String);
    procedure SslHttpCli1Cookie(Sender: TObject; const Data: String;
      var Accept: Boolean);
    procedure SslHttpCli1DocBegin(Sender: TObject);
    procedure SslHttpCli1DocEnd(Sender: TObject);
    procedure SslHttpCli1LocationChange(Sender: TObject);
    procedure SslHttpCli1HeaderData(Sender: TObject);
    procedure SslHttpCli1SslCliNewSession(Sender: TObject;
      SslSession: Pointer; WasReused: Boolean; var IncRefCount: Boolean);
    procedure SslHttpCli1SslCliGetSession(Sender: TObject;
      var SslSession: Pointer; var FreeSession: Boolean);
    procedure SslHttpCli1SslHandshakeDone(Sender: TObject; ErrCode: Word;
      PeerCert: TX509Base; var Disconnect: Boolean);
    procedure SslHttpCli1SslCliCertRequest(Sender: TObject;
      var Cert: TX509Base);
    procedure ButtonOSSLVersionClick(Sender: TObject);
    procedure IcsLogger1IcsLogEvent(Sender: TObject;
      DebugOption: TLogOption; const Msg: String);
    procedure HeadButtonClick(Sender: TObject);
    procedure AbortButtonClick(Sender: TObject);
    procedure SslHttpCli1DocData(Sender: TObject; Buffer: Pointer;
      Len: Integer);
    procedure IcsCookiesNewCookie(Sender: TObject; ACookie: TCookie;
      var Save: Boolean);

  private
    FIniFileName               : String;
    FInitialized               : Boolean;
    FTrustedList               : TStringList;
    FClientCerts               : TX509List;
    FDocFileName               : String;
    FByteCount                 : Integer;
    FStartTime                 : Integer;
    FCurrentRequest            : THttpRequest;
{$IFDEF POSIX}
    { This is only required if we want to post/send custom messages as in Windows }
    FMessagePump               : TIcsMessagePump;
{$ENDIF}
    FNotifyWindow              : HWND;
    procedure WndProc(var Msg: TMessage);
    procedure BackgroundException(Sender: TObject; E: Exception; var CanClose: Boolean);
    procedure PrepareConnection;
    procedure SetButtonState(State: Boolean);
    procedure TransfertStats;
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  HttpsTstForm: THttpsTstForm;

implementation

{$R *.FMX}

uses
    DemoUtils,
    IcsCliCertDlg;

const
    SectionWindow      = 'HttpTstMainWindow';
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';
    KeyUrl             = 'Url';
    KeySocksServer     = 'SocksServer';
    KeySocksPort       = 'SocksPort';
    KeySocksLevel      = 'SocksLevelIndex';
    KeyProxyHost       = 'ProxyHost';
    KeyProxyPort       = 'ProxyPort';
    KeyDateTime        = 'DateTime';
    KeyDoc             = 'Doc';
    KeyCertFile        = 'CertFile';
    KeyPassPhrase      = 'PassPhrase';
    KeyPrivKeyFile     = 'PrivKeyFile';
    KeyCAFile          = 'CAFile';
    KeyCAPath          = 'CAPath';
    KeyLineMode        = 'LineMode';
    KeyVerifyPeer      = 'VerifyPeer';
    KeyAcceptableHosts = 'AcceptableHosts';
    KeyHttpVer         = 'HttpVer';
    KeySessCache       = 'SessCache';
    KeyDebugEvent      = 'DebugEvent';
    KeyDebugOutput     = 'DebugOutput';
    KeyDebugFile       = 'DebugFile';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF DEBUG_OUTPUT}
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
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.FormCreate(Sender: TObject);
begin
{$IFDEF DEBUG_OUTPUT}
    BigConsole(80, 100);
{$ENDIF}
{$IF CompilerVersion > 17}
    // BDS2006 has built-in memory leak detection and display
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
{$IFEND}
{$IFDEF POSIX}
    FMessagePump  := TIcsMessagePump.Create;
{$ENDIF}
    FNotifyWindow := AllocateHWND(WndProc);
    IcsNameThreadForDebugging('Main');
    FIniFileName := GetIcsIniFileName;
    FTrustedList := TStringList.Create;
    SslHttpCli1.CtrlSocket.OnBgException := BackgroundException;
    FormShow(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.FormDestroy(Sender: TObject);
begin
    FTrustedList.Free;
    FClientCerts.Free;
    if FNotifyWindow <> 0 then
        DeallocateHWND(FNotifyWindow);
{$IFDEF POSIX}
    FMessagePump.Free;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIcsIniFile.Create(FIniFileName);
        try
            Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
            Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
            Top          := IniFile.ReadInteger(SectionWindow, KeyTop, (ScreenHeight - Height) div 2);
            Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,(ScreenWidth - Width) div 2);
            UrlEdit.Text := IniFile.ReadString(SectionData, KeyUrl,
                                              'https://localhost');
            SocksServerEdit.Text := IniFile.ReadString(SectionData, KeySocksServer,
                                                   '');
            SocksPortEdit.Text   := IniFile.ReadString(SectionData, KeySocksPort,
                                                       '1080');
            ProxyHostEdit.Text   := IniFile.ReadString(SectionData, KeyProxyHost,
                                                       '');
            ProxyPortEdit.Text   := IniFile.ReadString(SectionData, KeyProxyPort,
                                                      '8080');
            DateTimeEdit.Text    := IniFile.ReadString(SectionData, KeyDateTime,
                                                       '');
            DocEdit.Text         := IniFile.ReadString(SectionData, KeyDoc,
                                                      '/index.html');
            CertFileEdit.Text    := IniFile.ReadString(SectionData, KeyCertFile,
                                                      '');
            PrivKeyFileEdit.Text := IniFile.ReadString(SectionData, KeyPrivKeyFile,
                                                      '');
            PassPhraseEdit.Text  := IniFile.ReadString(SectionData, KeyPassPhrase,
                                                      '');
            CAFileEdit.Text      := IniFile.ReadString(SectionData, KeyCAFile,
                                                      '');
            CAPathEdit.Text      := IniFile.ReadString(SectionData, KeyCAPath,
                                                      '');

            SocksLevelComboBox.ItemIndex := IniFile.ReadInteger(SectionData,
                                                                KeySocksLevel,
                                                                0);
            AcceptableHostsEdit.Text := IniFile.ReadString(SectionData,
                                                           KeyAcceptableHosts,
                                                           'www.overbyte.be;' +
                                                           'www.embarcadero.com');
            VerifyPeerCheckBox.IsChecked := Boolean(IniFile.ReadInteger(SectionData,
                                                                  KeyVerifyPeer,
                                                                  0));
            HttpVersionComboBox.ItemIndex := IniFile.ReadInteger(SectionData,
                                                                 KeyHttpVer, 0);
            SessCacheCheckBox.IsChecked     := IniFile.ReadBool(SectionData,
                                                              KeySessCache,
                                                              False);
            DebugEventCheckBox.IsChecked    := IniFile.ReadBool(SectionData,
                                                              KeyDebugEvent,
                                                              False);
            DebugOutputCheckBox.IsChecked   := IniFile.ReadBool(SectionData,
                                                              KeyDebugOutput,
                                                              False);
            DebugFileCheckBox.IsChecked     := IniFile.ReadBool(SectionData,
                                                              KeyDebugFile,
                                                              False);
        finally
            IniFile.Free;
        end;
        DisplayMemo.Lines.Clear;
    end;

  { V8.68 load OpenSSL, then display OpenSSL DLL name and version  }
    GSSLEAY_DLL_IgnoreNew := False;  { don't ignore OpenSSL 3.0 and later }
//      GSSLEAY_DLL_IgnoreNew := True;   { don't ignore OpenSSL 3.0 and later }
    GSSLEAY_DLL_IgnoreOld := False;  { ignore OpenSSL 1.1.1 and earlier }
    GSSL_DLL_DIR := ExtractFilePath(ParamStr(0));  { only from our directory }
    GSSL_SignTest_Check := True;     { check digitally signed }
    GSSL_SignTest_Certificate := True; { check digital certificate }
    GSSLEAY_LOAD_LEGACY := True;     { OpenSSL 3.0 legacy provider for old algorithms }
    LoadSsl;                         { need version number }
    if NOT GSSLStaticLinked  then begin
        if NOT FileExists (GLIBEAY_DLL_FileName) then
            DisplayMemo.Lines.Add('SSL/TLS DLL not found: ' + GLIBEAY_DLL_FileName)
        else
            DisplayMemo.Lines.Add('SSL/TLS DLL: ' + GLIBEAY_DLL_FileName +
                                                ', Version: ' + OpenSslVersion);
    end
    else
        DisplayMemo.Lines.Add('SSL/TLS Static Linked, Version: ' + OpenSslVersion);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.WriteString(SectionData,    KeyUrl,         UrlEdit.Text);
    IniFile.WriteString(SectionData,    KeySocksServer, SocksServerEdit.Text);
    IniFile.WriteString(SectionData,    KeySocksPort,   SocksPortEdit.Text);
    IniFile.WriteString(SectionData,    KeyProxyHost,   ProxyHostEdit.Text);
    IniFile.WriteString(SectionData,    KeyProxyPort,   ProxyPortEdit.Text);
    IniFile.WriteString(SectionData,    KeyDateTime,    DateTimeEdit.Text);
    IniFile.WriteInteger(SectionData,   KeyVerifyPeer,  Ord(VerifyPeerCheckBox.IsChecked));
    IniFile.WriteString(SectionData,    KeyDoc,         DocEdit.Text);
    IniFile.WriteString(SectionData,    KeyCertFile,    CertFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyPrivKeyFile, PrivKeyFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyPassPhrase,  PassPhraseEdit.Text);
    IniFile.WriteString(SectionData,    KeyCAFile,      CAFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyCAPath,      CAPathEdit.Text);
    IniFile.WriteString(SectionData,    KeyAcceptableHosts, AcceptableHostsEdit.Text);
    IniFile.WriteInteger(SectionData,   KeySocksLevel,  SocksLevelComboBox.ItemIndex);
    IniFile.WriteInteger(SectionData,   KeyHttpVer,     HttpVersionComboBox.ItemIndex);
    IniFile.WriteBool(SectionData,      KeySessCache,   SessCacheCheckBox.IsChecked);
    IniFile.WriteBool(SectionData,      KeyDebugEvent,  DebugEventCheckBox.IsChecked);
    IniFile.WriteBool(SectionData,      KeyDebugOutput, DebugOutputCheckBox.IsChecked);
    IniFile.WriteBool(SectionData,      KeyDebugFile,   DebugFileCheckBox.IsChecked);
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.Display(Msg : String);
var
    I, J : Integer;
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 1000 then begin
            while DisplayMemo.Lines.Count > 1000 do
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
        DisplayMemo.GoToTextEnd;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.PrepareConnection;
const
    SocksLevelValues : array [0..2] of String = ('5', '4A', '4');
begin
    if SocksServerEdit.Text > '' then begin
        SslHttpCli1.SocksServer := SocksServerEdit.Text;
        SslHttpCli1.SocksPort   := SocksPortEdit.Text;
        SslHttpCli1.SocksLevel  := SocksLevelValues[SocksLevelComboBox.ItemIndex];
    end
    else begin
        SslHttpCli1.SocksServer := '';
        SslHttpCli1.SocksPort   := '';
        SslHttpCli1.SocksLevel  := '5';
    end;

    if ProxyHostEdit.Text > '' then begin
        SslHttpCli1.Proxy         := ProxyHostEdit.Text;
        SslHttpCli1.ProxyPort     := ProxyPortEdit.Text;
    end
    else begin
        SslHttpCli1.Proxy         := ProxyHostEdit.Text;
        SslHttpCli1.ProxyPort     := ProxyPortEdit.Text;
    end;
    SslHttpCli1.URL            := UrlEdit.Text;
    SslHttpCli1.AcceptLanguage := 'en, fr';
    SslHttpCli1.Connection     := 'Keep-Alive';
    SslHttpCli1.RequestVer     := '1.' + IntToStr(HttpVersionComboBox.ItemIndex);

    if DateTimeEdit.Text <> '' then
        SslHttpCli1.ModifiedSince := StrToDateTime(DateTimeEdit.Text)
    else
        SslHttpCli1.ModifiedSince := 0;
    SslHttpCli1.Cookie := IcsCookies.GetCookies(SslHttpCli1.URL);
    //SslHttpCli1.SetAcceptableHostsList(AcceptableHostsEdit.Text);

    SslContext1.SslCertFile         := CertFileEdit.Text;
    SslContext1.SslPassPhrase       := PassPhraseEdit.Text;
    SslContext1.SslPrivKeyFile      := PrivKeyFileEdit.Text;
    SslContext1.SslCAFile           := CAFileEdit.Text;
    SslContext1.SslCAPath           := CAPathEdit.Text;
    SslContext1.SslVerifyPeer       := VerifyPeerCheckBox.IsChecked;
    SslContext1.SslVersionMethod    := sslV23_CLIENT;

    IcsLogger1.LogOptions := [];
    if DebugEventCheckBox.IsChecked then
        IcsLogger1.LogOptions := IcsLogger1.LogOptions + [loDestEvent];
    if DebugOutputCheckBox.IsChecked then
        IcsLogger1.LogOptions := IcsLogger1.LogOptions + [loDestOutDebug];
    if DebugFileCheckBox.IsChecked then begin
        IcsLogger1.LogFileName   := 'Debug_Out_HttpsTst.txt';
        IcsLogger1.LogFileOption := lfoOverwrite;
        IcsLogger1.LogOptions    := IcsLogger1.LogOptions + [loDestFile];
    end;
    if IcsLogger1.LogOptions <> [] then
        IcsLogger1.LogOptions := IcsLogger1.LogOptions +
                                 LogAllOptInfo + [loAddStamp];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.GetButtonClick(Sender: TObject);
begin
    try
        PrepareConnection;
        Display('Connecting...');
        GetButton.Enabled := FALSE;
        DocumentMemo.Lines.Clear;
        FCurrentRequest := httpGet;
        SslHttpCli1.GetAsync;
    except
        on E: Exception do begin
            Display('Connect error. ' + E.Classname + ': ' + E.Message);
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.WndProc(var Msg: TMessage);
begin
    try
        { We have to handle all exceptions }
        if Msg.Msg = WM_SSL_NOT_TRUSTED then
        begin
            if FCurrentRequest = httpGet then
                SslHttpCli1.GetAsync
            else
                HeadButtonClick(HeadButton);
        end
        else
            Msg.Result := DefWindowProc(FNotifyWindow, Msg.Msg, Msg.WParam, Msg.LParam);
    except
        Application.HandleException(Self);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.ClearButtonClick(Sender: TObject);
begin
    DocumentMemo.Lines.Clear;
    DisplayMemo.Lines.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.CloseButtonClick(Sender: TObject);
begin
    SslHttpCli1.CloseAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1Command(Sender: TObject; var S: String);
begin
    Display('cmd> ' + s);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1Cookie(
    Sender     : TObject;
    const Data : String;
    var Accept : Boolean);
begin
    IcsCookies.SetCookie (Data, SslHttpCli1.Url);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1DocBegin(Sender: TObject);
var
    HttpCli : TSslHttpCli;
begin
    FByteCount := 0;
    FStartTime := IcsGetTickCount;
    HttpCli := Sender as TSslHttpCli;
    Display(HttpCli.ContentType + ' => ' + HttpCli.DocName);
    Display('Document = ' + HttpCli.DocName);

    FDocFileName := HttpCli.DocName;

    if HttpCli.ContentType = 'image/gif' then
        ReplaceExt(FDocFileName, 'gif')
    else if HttpCli.ContentType = 'image/jpeg' then
        ReplaceExt(FDocFileName, 'jpg')
    else if HttpCli.ContentType = 'image/bmp' then
        ReplaceExt(FDocFileName, 'bmp');

    if FDocFileName = '' then
        FDocFileName := 'HttpTst.htm';
    try
        HttpCli.RcvdStream := TFileStream.Create(FDocFileName, fmCreate);
    except
        on E:Exception do begin
            Display('Error opening file: ' + E.Message);
            FDocFileName := 'HttpTst.htm';
            Display('Using default file name: ' + FDocFileName);
            HttpCli.RcvdStream := TFileStream.Create(FDocFileName, fmCreate);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1DocData(Sender: TObject; Buffer: Pointer;
  Len: Integer);
begin
    Inc(FByteCount, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.TransfertStats;
var
    Buffer   : String;
    BytesSec : Int64 ;
    Duration : Int64 ;  { V2.100 allow wrap at 49 days, don't show 0 secs or silly bps }
    FStopTime : Integer;
begin
    FStopTime := LongInt(IcsGetTickCount);
    Buffer    := IntToSTr(FByteCount) + ' bytes document received/sent in ';
    if DWORD (FStopTime) >= DWORD (FStartTime) then   { V2.102 fix zero duration downloads }
        Duration := DWORD (FStopTime) - DWORD (FStartTime)
    else
        Duration := ($FFFFFFFF - DWORD (FStartTime)) + DWORD (FStopTime);
    if Duration < 5000 then
        Buffer := Buffer + IntToStr(Duration) + ' milliseconds'
    else begin
        Buffer := Buffer + IntToStr(Duration div 1000) + ' seconds';
        if FStopTime <> FStartTime then begin
            if FByteCount > 32767 then
                BytesSec := 1000 * (FByteCount div Duration)
            else
                BytesSec := (1000 * FByteCount) div Duration;
            Buffer := Buffer + ' (' + IntToStr(BytesSec) + ' Bytes/sec)';
        end;
    end;
    Display('! ' + Buffer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1DocEnd(Sender: TObject);
var
    HttpCli : TSslHttpCli;
begin
    TransfertStats;
    HttpCli := Sender as TSslHttpCli;
    if Assigned(HttpCli.RcvdStream) then begin
        HttpCli.RcvdStream.Free;
        HttpCli.RcvdStream := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1LocationChange(Sender: TObject);
var
    SslHttpCli : TSslHttpCli;
    I          : Integer;
begin
    SslHttpCli := Sender as TSslHttpCli;
    for I := 0 to SslHttpCli.RcvdHeader.Count - 1 do
        Display('hdr>' + SslHttpCli.RcvdHeader.Strings[I]);
    Display('Location changed to "' + SslHttpCli.Location + '"');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1RequestDone(
    Sender  : TObject;
    RqType  : THttpRequest;
    ErrCode : Word);
var
    DataIn  : TStream;
    I       : Integer;
begin
    if RqType <> httpGet then // Head is sync
        Exit;

    GetButton.Enabled := TRUE;
    if (ErrCode <> 0) then begin
        Display('Request done, error #' + IntToStr(ErrCode));
        Exit;
    end;

    Display('Request done, StatusCode #' + IntToStr(SslHttpCli1.StatusCode));

    for I := 0 to SslHttpCli1.RcvdHeader.Count - 1 do
        Display('hdr>' + SslHttpCli1.RcvdHeader.Strings[I]);
    SslHttpCli1.RcvdHeader.Clear;

    if SslHttpCli1.DocName = '' then
        DocumentMemo.Lines.Add('*** NO DOCUMENT FILE NAME ***')
    else begin
        if not FileExists(SslHttpCli1.DocName) then
            DocumentMemo.Lines.Add('*** NO DOCUMENT FILE ***')
        else begin
            DataIn := TFileStream.Create(SslHttpCli1.DocName, fmOpenRead);
            try
                if Copy(SslHttpCli1.ContentType, 1, 5) = 'text/' then
                    DocumentMemo.Lines.LoadFromStream(DataIn)
                else begin
                    DocumentMemo.Lines.Add('Content type is ' +
                                           SslHttpCli1.ContentType);
                    DocumentMemo.Lines.Add('Document stored in ''' +
                                           SslHttpCli1.DocName +
                                           ''' Size=' + IntToStr(DataIn.Size));
                end;
            finally
                DataIn.Free;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1HeaderData(Sender: TObject);
begin
    Display('hdr>' + SslHttpCli1.LastResponse);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1SslCliNewSession(Sender: TObject;
    SslSession      : Pointer;
    WasReused       : Boolean;
    var IncRefCount : Boolean);
var
    HttpCli : TSslHttpCli;
begin
    { SslCliNewSession/SslCliGetSession allow external, client-side session }
    { caching.                                                              }
    if not SessCacheCheckBox.IsChecked then
        Exit;
    HttpCli := (Sender as TSslHttpCli);
    if (not WasReused) then begin
        SslAvlSessionCache1.CacheCliSession(SslSession,
                                            HttpCli.CtrlSocket.PeerAddr +
                                            HttpCli.CtrlSocket.PeerPort,
                                            IncRefCount);
        Display('! New SSL session');
    end
    else
        Display('! SSL Session reused');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1SslCliGetSession(
    Sender          : TObject;
    var SslSession  : Pointer;
    var FreeSession : Boolean);
var
    HttpCli : TSslHttpCli;
begin
    { SslCliNewSession/SslCliGetSession allow external, client-side session }
    { caching.                                                              }
    if not SessCacheCheckBox.IsChecked then Exit;
    HttpCli := (Sender as TSslHttpCli);
    SslSession  := SslAvlSessionCache1.GetCliSession(
                                      HttpCli.CtrlSocket.PeerAddr +
                                      HttpCli.CtrlSocket.PeerPort,
                                      FreeSession);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1SslVerifyPeer(
    Sender  : TObject;
    var Ok  : Integer;
    Cert    : TX509Base);
begin
    { Alternate verification takes place in event HandshakeDone, we    }
    { accept anything temporarily here. Note that the same certificate }
    { may appear multiple times in this event when we set OK to 1      }
    { overwriting the real verify result.                              }
    OK := 1;
    Display('Received certificate'#13#10 +
            'Subject: "' + Cert.SubjectOneLine + '"'#13#10 +
            'Issuer:  "' + Cert.IssuerOneLine + '"'#13#10  +
            'Verify result: ' + Cert.VerifyErrMsg +
            ' Verify depth: ' + IntToStr(Cert.VerifyDepth));

    (* // original source
    begin
        Display('Received certificate'#13#10 +
                'Subject: "' + Cert.SubjectOneLine + '"'#13#10 +
                'Issuer:  "' + Cert.IssuerOneLine + '"');
    end
    else begin
        if (ErrCode = X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN) or
           (ErrCode = X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY) then begin
            FNotTrusted := Cert.IssuerOneLine + '/SN=' + IntToStr(Cert.SerialNum);
            if FTrustedList.IndexOf(FNotTrusted) >= 0 then begin
                Display('Received certificate. Issuer = "' + Cert.IssuerOneLine + '"');
                Display('Serial number = ' + IntToStr(Cert.SerialNum));
                Display('Verify result: ' + Cert.VerifyErrMsg);
                Display('We trust this one');
                Ok := 1;
                Exit;
            end;
            SslHttpCli1.Abort;
            PostMessage(Handle, WM_SSL_NOT_TRUSTED, 0, 0);
            Exit;
        end;
        Display('Can''t verify certificate:');
        Display('  Issuer  = "' + Cert.IssuerOneLine + '"');
        Display('  Subject = "' + Cert.SubjectOneLine + '"');
        Display('  Error   = ' + IntToStr(Cert.VerifyResult) + ' (' + Cert.VerifyErrMsg + ')');
    end; *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1SslHandshakeDone(
    Sender          : TObject;
    ErrCode         : Word;
    PeerCert        : TX509Base;
    var Disconnect  : Boolean);  // If TRUE will close the connection delayed!
var
    CertChain   : TX509List;
    DlgMsg      : String;
    I           : Integer;
    Hash        : String;
    HttpCli     : TSslHttpCli;
    ChainInfo   : String;
begin
    Display('Handshake done, error #' + IntToStr(ErrCode));
    HttpCli   := Sender as TSslHttpCli;

    { A simple custom verification that may be unsecure!...               }
    { See also SslVerifyPeer above.                                       }

    { Ssl handshake error or session is reused or no verification wanted  }
    if (ErrCode <> 0) or (HttpCli.CtrlSocket.SslSessionReused) or
       not HttpCli.SslContext.SslVerifyPeer then
        Exit; // nothing to do, go ahead

    Hash := PeerCert.Sha1Hex;
    { Is current host already in the list of temporarily accepted hosts ? }
    if HttpCli.SslAcceptableHosts.IndexOf(HttpCli.Hostname + Hash) > -1 then
        Exit; // previously accepted, go ahead

    { Property SslCertChain contains all certificates in current verify chain }
    CertChain := HttpCli.CtrlSocket.SslCertChain;
    Display('! ' + IntToStr(CertChain.Count) +
            ' Certificate(s) in the verify chain.');

    { Now to the PostConnectionCheck, a very important security check!
      Our application will be vulnerable if we do not check the peer
      certificate beyond verification of the chain. Nothing prevents an
      attacker from getting his own certificate signed by one of our trusted
      CAs and then hijacking all our sessions. We thward this kind of
      masquerade by tying the certificate to some information unique to the
      machine. In SSL this information is one or multiple full qualified
      domain names (FQDN) also called DNS names stored in certificate's
      commonName field(s) of the subjectName field. Since X.509v3 the
      subjectAltName extension allows to hold the FQDN as well as other
      identifying information such as the IP address.
      We use function PostConnectionCheck to perform these checks for us. }

    if PeerCert.PostConnectionCheck(HttpCli.Hostname) then begin
        { Now check whether chain verify result was OK as well }
        if (PeerCert.VerifyResult = X509_V_OK) then begin
            Display('! Chain verification and PostConnectionCheck succeeded');
            Exit; // Everything OK, go ahead.
        end
        else
           { Prepare our dialog text }
           DlgMsg := 'Peer certificate was issued to the site.'#13#10#13#10 +
                     'Do you want to trust the connection anyway?';
    end
    else { Prepare our other dialog text }
        DlgMsg := 'Post connection check:'#13#10 +
                  'The name specified in the peer certificate is '#13#10 +
                  'invalid or does not match the site!'#13#10#13#10 +
                  'Do you want to trust the connection anyway?';


    { OpenSsl's chain verification and/or our PostConnectionCheck failed,   }
    { abort current connection and ask the user what to do next.            }

    HttpCli.Abort; //***

    { Note that we may not process messages in an event, calling ShowMessage }
    { and the like would call the message pump, that were fatal if we were   }
    { still connected. If you do not need user intervention it's OK to set   }
    { Disconnect to TRUE which will close the connection delayed after this  }
    { handler returned.                                                      }

    { Collect further information to display in the dialog.                  }
    if CertChain.Count > 0 then begin
        ChainInfo := 'Certificates in the verify chain:'+ #13#10;
        for I := 0 to CertChain.Count -1 do begin
            if Length(ChainInfo) > 0 then
                ChainInfo := ChainInfo + #13#10;
            ChainInfo := ChainInfo +  IntToStr(I + 1) + ')' +
                  ' SubjectCommonName: ' + CertChain[I].SubjectCName + #13#10 +
                  ' VerifyResult: ' + CertChain[I].FirstVerifyErrMsg + #13#10;
        end;
    end;
    { ChainInfo := ChainInfo + #13#10 +
                   'PeerCert:'#13#10 +
                   ' SubjectCommonName: ' + PeerCert.SubjectCName + #13#10 +
                   ' VerifyResult: ' + PeerCert.FirstVerifyErrMsg + #13#10; }

    if MessageDlg(ChainInfo + #13#10 + DlgMsg,
         TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then begin
        { If FirstVerifyResult of top most cert in CertChain equals error     }
        { X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN we hit a self-signed root cert }
        { that is not in our trusted store, we should ask the user whether he }
        { wants to trust this root CA by import into the trusted store,       }
        { this was a persistant trust.                                        }
        if (CertChain.Count > 0) and
           (PeerCert.FirstVerifyResult = X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN) and
           (CaPathEdit.Text <> '') then begin
            { This looks ugly, real applications should provide a nicer dialog }
            if (MessageDlg(PeerCert.CertInfo + #13#10 +
                'Do you also want to add this root certificate to your ' +
                'trusted CA certificate store?',
                TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes) then begin
                { Certificates stored this way are being looked up by openssl   }
                { the next time a certificate is verified, w/o initializing the }
                { SslContext first.                                             }
                PeerCert.SaveToPemFile(IncludeTrailingBackSlash(CaPathEdit.Text) +
                   { IntToHex(X509_subject_name_hash(PeerCert.X509), 8) + '.0'); }
                    PeerCert.Sha256Hex + '.0');                                  { V8.68 don't use internal OpenSSL function }
                { If the same file name already exists we need to increment the }
                { extension by one, skipped in this demo.                       }
                { We could append those files to our trusted CA file later on.  }
            end;
        end;
        { Add a unique host ID to our temporarily trusted host list and repeat }
        { last request.                                                        }
        HttpCli.SslAcceptableHosts.Add(HttpCli.Hostname + Hash);
        { Repeat last request asynchronously }
        PostMessage(FNotifyWindow, WM_SSL_NOT_TRUSTED, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SslHttpCli1SslCliCertRequest(Sender: TObject;
    var Cert: TX509Base);
var
    X : TX509Base;
begin
    { A very simple test of the SslCliCertRequest event.               }
    { This event is triggered only if CertFileEdit.Text is empty,      }
    { the server requested a certificate from the client,              }
    { and of course only in case of the SSL session wasn't reused.     }
    if not Assigned(FClientCerts) then begin
        if not Assigned(ClientCertDlg) then
            Application.CreateForm(TClientCertDlg, ClientCertDlg);
        { Create a pool of client certs }
        ClientCertDlg.CertListBox.Clear;
        FClientCerts := TX509List.Create(Self);
        try
            X := FClientCerts.Add;
            X.LoadFromPemFile('01cert.pem');
            X.PrivateKeyLoadFromPemFile('01key.pem', 'password');
            ClientCertDlg.CertListBox.Items.Add(X.SubjectOneLine);
            X := FClientCerts.Add;
            X.LoadFromPemFile('client.pem', True, 'password');
            ClientCertDlg.CertListBox.Items.Add(X.SubjectOneLine);
        except
            FreeAndNil(FClientCerts);
            raise
        end;
    end;
    ClientCertDlg.CertListBox.ItemIndex := 0;
    if ClientCertDlg.ShowModal = mrOK then
        Cert := FClientCerts[ClientCertDlg.CertListBox.ItemIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.ButtonOSSLVersionClick(Sender: TObject);
begin
    SslContext1.InitContext; //Pre-loads OpenSSL DLL's
    Display(OpenSslVersion);
    Display(OpenSslCompilerFlags);
    Display(OpenSslBuiltOn);
    Display(OpenSslPlatForm);
    Display(OpenSslDir);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.BackgroundException(
    Sender       : TObject;
    E            : Exception;
    var CanClose : Boolean);
begin
    Display('!' + E.ClassName + ': ' + E.Message);
    CanClose := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.IcsCookiesNewCookie(Sender: TObject; ACookie: TCookie;
  var Save: Boolean);
var
    S: string;
begin
    with ACookie do begin
        S := 'NewCookie: ' + CName + '=' + CValue + ', Domain=' + CDomain + ', Path=' + CPath ;
        if CPersist then
            S := S + ', Expires=' + DateTimeToStr (CExpireDT)
        else
            S := S + ', Not Persisent';
        if CSecureOnly then S := S + ', SecureOnly';
        if CHttpOnly then S := S + ', HttpOnly';
        Display (S);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.IcsLogger1IcsLogEvent(Sender: TObject;
  DebugOption: TLogOption; const Msg: String);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.SetButtonState(State : Boolean);
begin
    GetButton.Enabled   := State;
    HeadButton.Enabled  := State;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.HeadButtonClick(Sender: TObject);
var
    I : Integer;
begin
    DisplayMemo.Lines.Clear;
    DocumentMemo.Lines.Clear;
    SetButtonState(FALSE);

    try
        PrepareConnection;
        SslHttpCli1.RcvdStream := nil;
        try
            FCurrentRequest := httpHead;
            SslHttpCli1.Head;
        except
            Display('HEAD Failed !');
            Display('StatusCode   = ' + IntToStr(SslHttpCli1.StatusCode));
            Display('ReasonPhrase = ' + SslHttpCli1.ReasonPhrase);
            Exit;
        end;

        Display('StatusCode = ' + IntToStr(SslHttpCli1.StatusCode));

        for I := 0 to SslHttpCli1.RcvdHeader.Count - 1 do
            Display('hdr>' + SslHttpCli1.RcvdHeader.Strings[I]);
        SslHttpCli1.RcvdHeader.Clear;
    finally
        SetButtonState(TRUE);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpsTstForm.AbortButtonClick(Sender: TObject);
begin
    SslHttpCli1.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
