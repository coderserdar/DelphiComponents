{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  ICS SSL Json Object Signing (Jose) Demos
Creation:     Apr 2018
Updated:      Oct 2021
Version:      8.68
Support:      Use the mailing list ics-ssl@elists.org
Legal issues: Copyright (C) 2003-2021 by François PIETTE
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
25 Apr 2018 - 8.54 baseline
04 May 2020 - 8.64 Corrected jose-ras files to jose-rsa and rsapsss to rsapss.
                   Added new Json/XML tab that allows blocks of Json or XML to
                     be copy/pasted and parsed to objects in a grid, repeatedly.
                     Also pretty prints Json, all to help Json debugging.
18 Nov 2020 - 8.65 Split Jose and transcoding tests to separate tabs.
                   Using INI file to keep settings.
                   Added testing of JWK to public and private key for servers,
                      reads and writes JWKs.
                   Added testing of JWS/JWT verification for servers.
                   Allow clicking on Json array line to display only that record.
Feb 23 2021 V8.66 Renamed all OpenSSL functions to original names removing ICS
                     f_ prefix.
                  Added support for YuOpenSSL which provides OpenSSL in a pre-built
                     DCU linked into applications, rather than using external DLLs.
Sep 27 2021 V8.67 Log OpenSSL version, support OpenSSL 3.0.
                  Replaced EVP_PKEY_cmp with TX509Base method.
                  Added PKey Parameters button to show private key parameters
                    using new OpenSSL 3.0 APIs.
Oct 07 2021 V8.68 Support OpenSSL 3.0 for YuOpenSSL.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsJoseTst1;

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

{$I Include\OverbyteIcsDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TypInfo, ComCtrls, ExtCtrls, Buttons,
  OverbyteIcsWSocket,
  OverbyteIcsUtils,
  OverbyteIcsMimeUtils,
  OverbyteIcsURL,
  OverbyteIcsSha1,
  OverbyteIcsSSLEAY,
  OverbyteIcsLibeay,
  OverbyteIcsSslX509Utils,     { V8.65 }
  OverbyteIcsTypes,            { V8.64 }
  OverbyteIcsSuperObject,
  OverbyteIcsSuperXMLParser,   { V8.64 }
  OverbyteIcsIniFiles,
  OverbyteIcsSslJose;
// {$IFDEF YuOpenSSL}, YuOpenSSL{$ENDIF YuOpenSSL};

type
  TJsonDemoForm = class(TForm)
// saved items
    Base64Text: TEdit;
    CodeTextLines: TMemo;
    CompactXML: TCheckBox;
    HexText: TEdit;
    JoseTextLines: TMemo;
    JsonInput: TMemo;
    JwkPrivate: TCheckBox;
    LogJson: TCheckBox;
    NewPrivKey: TComboBox;
    RawJWK: TMemo;
    ShowRawKey: TCheckBox;
    TestHmacKey: TEdit;
    TestJWSAlg: TComboBox;
    TestPrivKeyFile: TComboBox;
    URLText: TEdit;

// not saved
    LogWin: TMemo;
    OpenDlg: TOpenDialog;
    PageControl: TPageControl;
    TabSheetJson: TTabSheet;
    JsonGrid: TListView;
    PanelButtons: TPanel;
    Label33: TLabel;
    doParseJson: TButton;
    doLoadFile: TButton;
    doParseXML: TButton;
    TabSheetJose: TTabSheet;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label1: TLabel;
    doTestSign: TButton;
    doJWSCreate: TButton;
    doSignHmac: TButton;
    TabSheet1: TTabSheet;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    doBase64Enc: TButton;
    doB64URLEn: TButton;
    doTestUrlEnc: TButton;
    doHexEncode: TButton;
    doEncodeURL: TButton;
    doBase64Dec: TButton;
    doB64URLDec: TButton;
    doHashDigest: TButton;
    doHexDec: TButton;
    doDecodeURL: TButton;
    Label4: TLabel;
    doJWKCreate: TButton;
    Label2: TLabel;
    doJKWRead: TButton;
    Label3: TLabel;
    doNewPrivKey: TButton;
    doClear: TButton;
    SelFile: TBitBtn;
    LabelPKey: TLabel;
    doJWSRead: TButton;
    doLoadKeyFile: TButton;
    doKeyParams: TButton;
    procedure doBase64DecClick(Sender: TObject);
    procedure doBase64EncClick(Sender: TObject);
    procedure doB64URLEnClick(Sender: TObject);
    procedure doB64URLDecClick(Sender: TObject);
    procedure doTestUrlEncClick(Sender: TObject);
    procedure doHexDecClick(Sender: TObject);
    procedure doHexEncodeClick(Sender: TObject);
    procedure doEncodeURLClick(Sender: TObject);
    procedure doDecodeURLClick(Sender: TObject);
    procedure doSignHmacClick(Sender: TObject);
    procedure doTestSignClick(Sender: TObject);
    procedure doJWSCreateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure doHashDigestClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JoseTextLinesDblClick(Sender: TObject);
    procedure doParseClick(Sender: TObject);
    procedure doLoadFileClick(Sender: TObject);
    procedure JsonGridDblClick(Sender: TObject);
    procedure CodeTextLinesDblClick(Sender: TObject);
    procedure doJWKCreateClick(Sender: TObject);
    procedure doNewPrivKeyClick(Sender: TObject);
    procedure SelFileClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TestPrivKeyFileChange(Sender: TObject);
    procedure doJKWReadClick(Sender: TObject);
    procedure doClearClick(Sender: TObject);
    procedure doJWSReadClick(Sender: TObject);
    procedure doLoadKeyFileClick(Sender: TObject);
    procedure doKeyParamsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FPrivateKey: TSslCertTools;
    FVerifyPKey: TX509Base;
    procedure AddLog (const S: string) ;
    function LoadPKeyFile(fname: string): boolean;
  end;

var
    JsonDemoForm: TJsonDemoForm;
//    FProgDir: String;
    FIniFileName: String;
    FInitialized: Boolean;
    JArrayTot: Integer;              { V8.65 }
    JArrayItems: Array of String;    { V8.65 }

const
    SectionMainWindow    = 'MainWindow';
    KeyTop               = 'Top';
    KeyLeft              = 'Left';
    KeyWidth             = 'Width';
    KeyHeight            = 'Height';
    SectionData          = 'Data';


implementation

{$R *.dfm}

Uses OverbyteIcsJoseTst2;

procedure TJsonDemoForm.FormCreate(Sender: TObject);
var
    I: Integer;
    FProgDir: String;
begin
    FProgDir := ExtractFilePath(ParamStr(0));
    FIniFileName := GetIcsIniFileName;
    GSSLEAY_DLL_IgnoreNew := False;    { V8.67 don't ignore OpenSSL 3.0 and later }
//  GSSLEAY_DLL_IgnoreNew := True;     { V8.67 don't ignore OpenSSL 3.0 and later }
    GSSLEAY_DLL_IgnoreOld := False;    { V8.67 ignore OpenSSL 1.1.1 and earlier }
    GSSL_DLL_DIR := FProgDir;          { only from our directory }
    GSSL_SignTest_Check := True;       { check digitally signed }
    GSSL_SignTest_Certificate := True; { check digital certificate }
//    GSSLEAY_LOAD_LEGACY := True;       { V8.67 OpenSSL 3.0 legacy provider for old algorithms }
    LoadSsl;
    FPrivateKey := TSslCertTools.Create(self);
    FVerifyPKey := TX509Base.Create(self);
    NewPrivKey.Items.Clear;
    for I := 0 to SslPrivKeyTypeLitsLast2 do
        NewPrivKey.Items.Add(SslPrivKeyTypeLits[I]);
end;

procedure TJsonDemoForm.FormDestroy(Sender: TObject);
var
    IniFile : TIcsIniFile;
    temp, section: String;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionMainWindow, KeyTop, Top);
    IniFile.WriteInteger(SectionMainWindow, KeyLeft, Left);
    IniFile.WriteInteger(SectionMainWindow, KeyWidth, Width);
    IniFile.WriteInteger(SectionMainWindow, KeyHeight, Height);
    with IniFile do begin
      section := SectionData;
      WriteString (section, 'Base64Text_Text', Base64Text.Text) ;
      WriteString (section, 'CodeTextLines_Lines', CodeTextLines.Lines.CommaText) ;
      if CompactXML.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'CompactXML_Checked', temp) ;
      WriteString (section, 'HexText_Text', HexText.Text) ;
      WriteString (section, 'JoseTextLines_Lines', JoseTextLines.Lines.CommaText) ;
      WriteString (section, 'JsonInput_Lines', JsonInput.Lines.CommaText) ;
      if JwkPrivate.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'JwkPrivate_Checked', temp) ;
      if LogJson.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'LogJson_Checked', temp) ;
      WriteInteger (section, 'NewPrivKey_ItemIndex', NewPrivKey.ItemIndex) ;
      WriteString (section, 'NewPrivKey_Text', NewPrivKey.Text) ;
      WriteString (section, 'RawJWK_Lines', RawJWK.Lines.CommaText) ;
      if ShowRawKey.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'ShowRawKey_Checked', temp) ;
      WriteString (section, 'TestHmacKey_Text', TestHmacKey.Text) ;
      WriteInteger (section, 'TestJWSAlg_ItemIndex', TestJWSAlg.ItemIndex) ;
      WriteString (section, 'TestPrivKeyFile_Text', TestPrivKeyFile.Text) ;
      WriteString (section, 'URLText_Text', URLText.Text) ;
    end;
    IniFile.UpdateFile;
    IniFile.Free;
    FPrivateKey.Free;
    FVerifyPKey.Free;
end;

procedure TJsonDemoForm.FormShow(Sender: TObject);
var
    IniFile: TIcsIniFile;
    section: String;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile := TIcsIniFile.Create(FIniFileName);
        Width := IniFile.ReadInteger(SectionMainWindow, KeyWidth,  Width);
        Height := IniFile.ReadInteger(SectionMainWindow, KeyHeight, Height);
        Top := IniFile.ReadInteger(SectionMainWindow, KeyTop, (Screen.Height - Height) div 2);
        Left := IniFile.ReadInteger(SectionMainWindow, KeyLeft, (Screen.Width  - Width)  div 2);

       with IniFile do begin
          section := SectionData;
          Base64Text.Text := ReadString (section, 'Base64Text_Text', Base64Text.Text) ;
          CodeTextLines.Lines.CommaText := ReadString (section, 'CodeTextLines_Lines', CodeTextLines.Lines.CommaText) ;
          if ReadString (section, 'CompactXML_Checked', 'False') = 'True' then CompactXML.Checked := true else CompactXML.Checked := false ;
          HexText.Text := ReadString (section, 'HexText_Text', HexText.Text) ;
          JoseTextLines.Lines.CommaText := ReadString (section, 'JoseTextLines_Lines', JoseTextLines.Lines.CommaText) ;
          JsonInput.Lines.CommaText := ReadString (section, 'JsonInput_Lines', JsonInput.Lines.CommaText) ;
          if ReadString (section, 'LogJson_Checked', 'False') = 'True' then LogJson.Checked := true else LogJson.Checked := false ;
          if ReadString (section, 'JwkPrivate_Checked', 'False') = 'True' then JwkPrivate.Checked := true else JwkPrivate.Checked := false ;
          NewPrivKey.ItemIndex := ReadInteger (section, 'NewPrivKey_ItemIndex', 0) ;
          NewPrivKey.Text := ReadString (section, 'NewPrivKey_Text', NewPrivKey.Text) ;
          RawJWK.Lines.CommaText := ReadString (section, 'RawJWK_Lines', '') ;
          if ReadString (section, 'ShowRawKey_Checked', 'False') = 'True' then ShowRawKey.Checked := true else ShowRawKey.Checked := false ;
          TestHmacKey.Text := ReadString (section, 'TestHmacKey_Text', TestHmacKey.Text) ;
          TestJWSAlg.ItemIndex := ReadInteger (section, 'TestJWSAlg_ItemIndex', 0) ;
          TestPrivKeyFile.Text := ReadString (section, 'TestPrivKeyFile_Text', TestPrivKeyFile.Text) ;
          URLText.Text := ReadString (section, 'URLText_Text', URLText.Text) ;
       end;
       IniFile.Free;
    end;
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

procedure TJsonDemoForm.AddLog (const S: string) ;
begin
    if Pos (IcsLF, S) > 0 then
        LogWin.Lines.Text := LogWin.Lines.Text + S + IcsCRLF
    else
       LogWin.Lines.Add (S) ;
end;

procedure TJsonDemoForm.doClearClick(Sender: TObject);
begin
    LogWin.Lines.Clear;
end;

function TJsonDemoForm.LoadPKeyFile(fname: string): boolean;
begin
    Result := False;
    if NOT FileExists (fname) then
    begin
        AddLog ('Can not find private key: ' +  fname);
        Exit ;
    end;
    try
        FPrivateKey.ClearAll;
        LabelPKey.Caption := 'Private Key: ';
        FPrivateKey.PrivateKeyLoadFromPemFile (fname, '') ;
    except
        on E:Exception do
        begin
            AddLog ('Failed to load private key file: ' + fname + ' - ' + E.Message);
            Exit ;
        end;
    end;
    if NOT FPrivateKey.IsPKeyLoaded then
    begin
        AddLog ('Failed to load private key: ' +  fname);
        Exit ;
    end;
     AddLog('Loaded private key file OK: ' + fname + ' - ' + FPrivateKey.PrivateKeyInfo);
    LabelPKey.Caption := 'Private Key: ' + FPrivateKey.PrivateKeyInfo;
    Result := True;
end;


procedure TJsonDemoForm.doNewPrivKeyClick(Sender: TObject);
begin
    FPrivateKey.PrivKeyType := TSslPrivKeyType(NewPrivKey.ItemIndex);
    doNewPrivKey.Enabled := false;
    try
        try
            FPrivateKey.ClearAll;
            LabelPKey.Caption := 'Private Key: ';
            AddLog('Generating private and public key pair, please wait');
            FPrivateKey.DoKeyPair;
            AddLog('Generated private and public key pair OK: ' + FPrivateKey.PrivateKeyInfo);
            LabelPKey.Caption := 'Private Key: ' + FPrivateKey.PrivateKeyInfo;
            doJWKCreate.Enabled := True;
        except
            on E:Exception do
                AddLog(E.Message);
        end;
    finally
        doNewPrivKey.Enabled := true;
    end;
end;

procedure TJsonDemoForm.doLoadKeyFileClick(Sender: TObject);
begin
    if LoadPKeyFile (TestPrivKeyFile.Text) then
       doJWKCreate.Enabled := True;
end;


procedure TJsonDemoForm.SelFileClick(Sender: TObject);
begin
    OpenDlg.InitialDir := ExtractFilePath(TestPrivKeyFile.Text);
    OpenDlg.FileName := TestPrivKeyFile.Text;
    if OpenDlg.FileName = '' then Exit;
    if OpenDlg.Execute then begin
        TestPrivKeyFile.Text := OpenDlg.FileName;
        FPrivateKey.ClearAll ;
    end;
end;

procedure TJsonDemoForm.TestPrivKeyFileChange(Sender: TObject);
begin
     FPrivateKey.ClearAll ;
end;

procedure TJsonDemoForm.JoseTextLinesDblClick(Sender: TObject);
begin
    JoseTextLines.Lines.Clear;
end;

procedure TJsonDemoForm.doEncodeURLClick(Sender: TObject);
begin
    AddLog (URLEncode(trim(CodeTextLines.Lines.Text)));
end;

procedure TJsonDemoForm.doHashDigestClick(Sender: TObject);
var
    AnsiData: AnsiString;
    Digest: AnsiString;
    Sha1Digest: SHA1DigestString;
begin
    try
        AnsiData := AnsiString(Trim(CodeTextLines.Lines.Text));
        Sha1Digest := SHA1ofStr(AnsiData);
        AddLog ('SHA1ofStr (len ' + IntToStr(Length(Digest)) +
                ') Hex: ' + IcsBufferToHex(Sha1Digest));

        Digest := IcsHashDigest(AnsiData, Digest_sha1);
        AddLog ('IcsHashDigest Sha1 (len ' + IntToStr(Length(Digest)) +
                ') Hex: ' + IcsBufferToHex(Digest));

        Digest := IcsHashDigest(AnsiData, Digest_sha256);
        AddLog ('IcsHashDigest Sha256 (len ' + IntToStr(Length(Digest)) +
                ') Hex: ' + IcsBufferToHex(Digest));
        AddLog ('IcsHashDigest Sha256 Base4Url: ' + IcsBase64UrlEncode (String(Digest)));

        Digest := IcsHashDigest(AnsiData, Digest_sha512);
        AddLog ('IcsHashDigest Sha512 (len ' + IntToStr(Length(Digest)) +
                ') Hex: ' + IcsBufferToHex(Digest));
        AddLog ('IcsHashDigest Sha512 Base4Url: ' + IcsBase64UrlEncode (String(Digest)));

        if ICS_OPENSSL_VERSION_NUMBER >= OSSL_VER_1101 then begin  // OpenSSL 1.1.1 and later
            Digest := IcsHashDigest(AnsiData, Digest_sha3_256);
            AddLog ('IcsHashDigest Sha3-256 (len ' + IntToStr(Length(Digest)) +
                    ') Hex: ' + IcsBufferToHex(Digest));
            AddLog ('IcsHashDigest Sha3-256 Base4Url: ' + IcsBase64UrlEncode (String(Digest)));

            Digest := IcsHashDigest(AnsiData, Digest_sha3_512);
            AddLog ('IcsHashDigest Sha3-512 (len ' + IntToStr(Length(Digest)) +
                    ') Hex: ' + IcsBufferToHex(Digest));
            AddLog ('IcsHashDigest Sha3-512 Base4Url: ' + IcsBase64UrlEncode (String(Digest)));

            Digest := IcsHashDigest(AnsiData, Digest_shake128);
            AddLog ('IcsHashDigest Shake128 (len ' + IntToStr(Length(Digest)) +
                    ') Hex: ' + IcsBufferToHex(Digest));
            AddLog ('IcsHashDigest Shake128 Base4Url: ' + IcsBase64UrlEncode (String(Digest)));

            Digest := IcsHashDigest(AnsiData, Digest_shake256);
            AddLog ('IcsHashDigest Shake256 (len ' + IntToStr(Length(Digest)) +
                    ') Hex: ' + IcsBufferToHex(Digest));
            AddLog ('IcsHashDigest Shake256 Base4Url: ' + IcsBase64UrlEncode (String(Digest)));
        end;

     //   Digest := IcsHashDigest(AnsiData, Digest_None);  // should raise exception
    except
        on E:Exception do
            AddLog ('Hash digest exception - ' + E.Message);
    end;
    AddLog ('');
end;

procedure TJsonDemoForm.doHexDecClick(Sender: TObject);
begin
    AddLog (String(IcsHexToBin(AnsiString(Trim(HexText.Text)))));
    AddLog ('');
end;

procedure TJsonDemoForm.doHexEncodeClick(Sender: TObject);
begin
    AddLog (IcsLowerCase(IcsBufferToHex(AnsiString(Trim(CodeTextLines.Lines.Text)))));
    AddLog ('');
end;

procedure TJsonDemoForm.doTestUrlEncClick(Sender: TObject);
var
    Data, S: String;
begin
    Data := Trim(CodeTextLines.Lines.Text);
    AddLog ('Data: ' + Data);
    S := String(Base64Encode(AnsiString(Data)));
    AddLog ('Base64Encode: ' + S) ;
    S := IcsBase64UrlEncode(Data);
    AddLog ('IcsBase64UrlEncode: ' + S) ;
    S := IcsBase64UrlDecode(S);
    AddLog ('IcsBase64UrlDecode: ' + S) ;
    AddLog ('');
end;

procedure TJsonDemoForm.doB64URLEnClick(Sender: TObject);
begin
    AddLog (IcsBase64UrlEncode(Trim(CodeTextLines.Lines.Text))) ;
    AddLog ('');
end;

procedure TJsonDemoForm.CodeTextLinesDblClick(Sender: TObject);
begin
    CodeTextLines.Lines.Clear;
end;

procedure TJsonDemoForm.doB64URLDecClick(Sender: TObject);
begin
    AddLog (IcsBase64UrlDecode(Trim(Base64Text.Text))) ;
    AddLog ('');
end;

procedure TJsonDemoForm.doDecodeURLClick(Sender: TObject);
begin
    AddLog (URLDecode(trim(CodeTextLines.Lines.Text)));
    AddLog ('');
end;

procedure TJsonDemoForm.doBase64DecClick(Sender: TObject);
begin
    AddLog (Base64Decode(Trim(Base64Text.Text))) ;
    AddLog ('');
end;

procedure TJsonDemoForm.doBase64EncClick(Sender: TObject);
begin
    AddLog (Base64Encode(Trim(CodeTextLines.Lines.Text))) ;
    AddLog ('');
end;

procedure TJsonDemoForm.doSignHmacClick(Sender: TObject);
var
    AnsiData, AnsiSecret, Digest: AnsiString;
    S: String;
    Flag: Boolean;
begin
    AddLog ('Testing Various HMAC Digests with Different SHA keys');

    AnsiData := AnsiString(Trim(JoseTextLines.Lines.Text));
    AnsiSecret := AnsiString(TestHmacKey.Text);
    AddLog ('Test Data: ' + String(AnsiData) + ', HMAC Key: ' + String(AnsiSecret));
    try
        Digest := HMAC_SHA1_EX(AnsiData, AnsiSecret);
        AddLog ('HMAC_SHA1_EX (len ' + IntToStr(Length(Digest)) +
                ') Base64: ' + String(Base64Encode (PAnsiChar(Digest), Length(Digest))));

        Digest := IcsHMACDigest(AnsiData, AnsiSecret, Digest_sha1);
        AddLog ('IcsHMACDigest Sha1 (len ' + IntToStr(Length(Digest)) +
                ') Base64: ' + String(Base64Encode (PAnsiChar(Digest), Length(Digest))));

        Digest := IcsHMACDigest(AnsiData, AnsiSecret, Digest_sha256);
        AddLog ('IcsHMACDigest Sha256 (len ' + IntToStr(Length(Digest)) +
                ') Base64: ' + String(Base64Encode (PAnsiChar(Digest), Length(Digest))));

        Digest := IcsHMACDigest(AnsiData, AnsiSecret, Digest_sha512);
        AddLog ('IcsHMACDigest Sha512 (len ' + IntToStr(Length(Digest)) +
                ') Base64: ' + String(Base64Encode (PAnsiChar(Digest), Length(Digest))));

        Digest := IcsHMACDigestEx(AnsiData, AnsiSecret, Digest_sha1);
        AddLog ('IcsHMACDigestEx Sha1 (len ' + IntToStr(Length(Digest)) +
                ') Base64: ' + String(Base64Encode (PAnsiChar(Digest), Length(Digest))));

        Digest := IcsHMACDigestEx(AnsiData, AnsiSecret, Digest_sha256);
        AddLog ('IcsHMACDigestEx Sha256 (len ' + IntToStr(Length(Digest)) +
                ') Base64: ' + String(Base64Encode (PAnsiChar(Digest), Length(Digest))));

        Digest := IcsHMACDigestEx(AnsiData, AnsiSecret, Digest_sha512);
        AddLog ('IcsHMACDigestEx Sha512 (len ' + IntToStr(Length(Digest)) +
                ') Base64: ' + String(Base64Encode (PAnsiChar(Digest), Length(Digest))));

        Flag := IcsHMACDigestVerify(AnsiData, AnsiSecret, Digest, Digest_sha512);
        S := 'Failed Verify';
        if Flag then S := 'Passed Verify';
        AddLog ('IcsHMACDigestVerify (len ' + IntToStr(Length(Digest)) + ') Sha512: ' + S);
    except
        on E:Exception do
            AddLog ('Hash digest exception - ' + E.Message);
    end;
    AddLog ('');
end;

procedure TJsonDemoForm.doTestSignClick(Sender: TObject);
var
    AnsiData, Signature: AnsiString;
    PublicKeyStr, S: String;
    Flag: Boolean;
begin
    AnsiData := AnsiString(Trim(JoseTextLines.Lines.Text));

    try
        if NOT Assigned(FPrivateKey.PrivateKey) then begin
            AddLog ('Need to create new private key or load old key file, first');
            Exit;
        end;
        AddLog ('Data: ' + String(AnsiData) + IcsCRLF + 'Private Key: ' + fPrivateKey.PrivateKeyInfo);

        Signature := IcsAsymSignDigest(AnsiData, fPrivateKey.PrivateKey, Digest_sha256);
        AddLog ('IcsAsymSignDigest Sha256 (len ' + IntToStr(Length(Signature)) +
                ') IcsBase64UrlEncode: ' + IcsBase64UrlEncode (String(Signature)));
        AddLog ('IcsAsymSignDigest Sha256 (len ' + IntToStr(Length(Signature)) +
                    ') Hex: ' + IcsLowerCase(IcsBufferToHex (Signature)));
  //      TestSignDigest(AnsiData, TestPrivKeyFile.Text, 'sha256');

        PublicKeyStr := fPrivateKey.PublicKeySaveToText;
        FVerifyPKey.PublicKeyLoadFromText(PublicKeyStr);

        Flag := IcsAsymVerifyDigest(AnsiData, Signature, FVerifyPKey.PrivateKey, Digest_sha256);
     //   Flag := IcsAsymVerifyDigest(AnsiData, Signature, PublicKeyStr, Digest_sha256);
        S := 'Failed Verify';
        if Flag then S := 'Passed Verify';
        AddLog ('IcsAsymVerifyDigest Sha256: ' + S);
    except
        on E:Exception do
            AddLog ('Signing exception - ' + E.Message);
    end;
    AddLog ('');
end;

function B64Url2Hex(const S: String): String;
begin
    Result := IcsLowerCase(IcsBufferToHex(AnsiString(IcsBase64UrlDecode(S))));
end;

// build Json Web Key from private key, get public key from JWK and check it

procedure TJsonDemoForm.doJWKCreateClick(Sender: TObject);
var
    JwkPub, Mykid, Myalg, NewSecret, NewKeyid: String;
    JoseAlg: TJoseAlg;
    JwkJson: ISuperObject;
begin
    AddLog ('Creating Jason Web Key using Algorithm:: ' + TestJWSAlg.Items[TestJWSAlg.ItemIndex]);
    RawJWK.Lines.Clear;
    try
        Mykid := TimeToStr(Now);
        if TestJWSAlg.ItemIndex < 1 then Exit;
        JoseAlg := TJoseAlg(TestJWSAlg.ItemIndex);
        if JoseAlg >= jsigRsa256 then begin
            if NOT Assigned(FPrivateKey.PrivateKey) then begin
                AddLog ('Need to create new private key or load old key file, first');
                Exit;
            end;
        end;

     // create Json Web Key
        MyAlg := IcsJoseFindAlg(JoseAlg, FPrivateKey.PrivateKey);
        if JoseAlg >= jsigRsa256 then  begin
            AddLog ('Private Key: ' + FPrivateKey.PrivateKeyInfo);
            if ShowRawKey.Checked then
                AddLog (FPrivateKey.GetPKeyRawText);  { V8.64 }
            JwkPub := IcsJoseJWKPubKey(FPrivateKey.PrivateKey, Myalg, Mykid,
                                                        JoseLituse_sig, JwkPrivate.Checked);
            RawJWK.Lines.Text := JwkPub;
            AddLog ('Ics Jose JWK Pkey Raw: ' + JwkPub);     { V8.64 }
            JwkJson := SO(JwkPub);
            if JwkJson.S[JoseParamk_kty] = JoseLitkty_RSA then begin
                AddLog ('Hex N: ' + B64Url2Hex(JwkJson.S[JoseParamkr_n]));
                AddLog ('Hex E: ' + B64Url2Hex(JwkJson.S[JoseParamkr_e]));
                if JwkPrivate.Checked then
                    AddLog ('Hex D: ' + B64Url2Hex(JwkJson.S[JoseParamkr_d]));
            end;
            if JwkJson.S[JoseParamk_kty] = JoseLitkty_EC then begin
                AddLog ('Hex X: ' + B64Url2Hex(JwkJson.S[JoseParamke_x]));
                AddLog ('Hex Y: ' + B64Url2Hex(JwkJson.S[JoseParamke_y]));
                if JwkPrivate.Checked then
                    AddLog ('Hex D: ' + B64Url2Hex(JwkJson.S[JoseParamke_d]));
            end;
            if JwkJson.S[JoseParamk_kty] = JoseLitkty_OKP then begin
                AddLog ('Hex X: ' + B64Url2Hex(JwkJson.S[JoseParamko_x]));
                 if JwkPrivate.Checked then
                    AddLog ('Hex D: ' + B64Url2Hex(JwkJson.S[JoseParamko_d]));
            end;
       end
        else  begin
            AddLog ('HMAC secret: ' + TestHmacKey.Text );
            JwkPub := IcsJoseJWKHmac(TestHmacKey.Text, Myalg, Mykid, JoseLituse_sig);
            RawJWK.Lines.Text := JwkPub;
            AddLog ('Ics Jose JWK HMAC Raw: ' + JwkPub);     { V8.64 }
        end;
        AddLog ('');

    // build key from Json Web Signature
        if JoseAlg >= jsigRsa256 then  begin
            AddLog ('Getting key from JWK');
            FVerifyPKey.PrivateKey := IcsJoseJWKGetPKey(JwkPub, NewKeyid);
            if Assigned(FVerifyPKey.PrivateKey) then begin
                if ShowRawKey.Checked then begin
                    AddLog (FVerifyPKey.GetPKeyRawText(NOT JwkPrivate.Checked));
                end;

             { V8.67 now got a function to compare key }
                try
                    if FPrivateKey.ComparePkey(FVerifyPKey) then
                         AddLog ('JWK key matches original private key')
                     else
                         AddLog ('Error, JWK key does not match private key');
                except
                    on E:Exception do
                        AddLog ('Failed to compare keys - ' + E.Message);
                end;
            end
            else
                  AddLog ('No JWK public key found');
        end
        else  begin
            AddLog ('Getting HMAC secret from JWK');
            NewSecret := IcsJoseJWKGetHmac(JwkPub);
            if NewSecret = TestHmacKey.Text then
                 AddLog ('JWK HMAC secret matches original HMAC secret, result ' + NewSecret)
             else
                 AddLog ('Error, JWK HMAC secret does not match HMAC secret, result ' + NewSecret);
        end;
    except
        on E:Exception do
            AddLog ('Json Web Key exception - ' + E.Message);
    end;
    AddLog ('');
end;

// get public key or secret from existing Json Web Key (as Json)

procedure TJsonDemoForm.doJKWReadClick(Sender: TObject);
var
    NewSecret, KeyType, NewKeyid: String;
    JoseAlg: TJoseAlg;
    JwkJson: ISuperObject;
begin
     AddLog ('Ics Jose JWK Pkey Raw: ' + RawJWK.Lines.Text);     { V8.64 }
    // get public key from Json Web Signature
    try
        JwkJson := SO(RawJWK.Lines.Text);
        JoseAlg := IcsJoseFindAlgType(JwkJson.S[JoseParamk_alg]);
        KeyType := JwkJson.S[JoseParamk_kty];

        if (JoseAlg >= jsigRsa256) or (KeyType <> JoseLitkty_oct) then  begin
            AddLog ('Getting public key from JWK');
            FVerifyPKey.PrivateKey := IcsJoseJWKGetPKey(RawJWK.Lines.Text, NewKeyid);
            if Assigned(FVerifyPKey.PrivateKey) then begin
                if ShowRawKey.Checked then begin
                    AddLog (FVerifyPKey.GetPKeyRawText(True));
                end;
                AddLog ('JWK public key found: ' + FVerifyPKey.PrivateKeyInfo);
            end
            else
                  AddLog ('No JWK public key found');
        end
        else  begin
            AddLog ('Getting HMAC secret from JWK');
            NewSecret := IcsJoseJWKGetHmac(RawJWK.Lines.Text);
            if NewSecret <> '' then
                 AddLog ('JWK HMAC secret found: ' + NewSecret)
             else
                 AddLog ('Error, JWK HMAC secret not found');
        end;
    except
        on E:Exception do
            AddLog ('Json Web Key exception - ' + E.Message);
    end;
    AddLog ('');
end;

// create Json Web Key, sign data with JWK, check signature

procedure TJsonDemoForm.doJWSCreateClick(Sender: TObject);
var
    Data, S, KwkPub, Mykid, Myalg: String;
    CompactJWSKey, JsonJWSKey, CompactJWSKid, JsonJWSKid: String;
    JoseAlg: TJoseAlg;
    RespJson: ISuperObject;

    procedure VerifyJWS(const MyJWS: String);
    var
        NewKeyid, Nonce, Payload: string;
        VerifyRes: TJoseVerify;
        NewPubKey: PEVP_PKEY;
    begin
        NewKeyid := MyKid;
        NewPubKey := FVerifyPKey.PrivateKey;
        Nonce := '';  // not using this yet
        AddLog ('CheckJWS: ' + MyJWS);
        VerifyRes := IcsJoseCheckJWS(MyJWS, Nonce, TestHmacKey.Text, NewPubKey, NewKeyid, Payload);
        AddLog ('Verify Result: ' + JoseVerifyLits[VerifyRes]);
     // need to save new public key
        if VerifyRes = JVerifyOkNewPubKey then begin
            FVerifyPKey.PrivateKey := NewPubKey;
            AddLog('Saved JWK: ' + Trim(FVerifyPKey.PublicKeySaveToText));
        end;
        if VerifyRes = JVerifyNeedPubKey then begin
            if NewKeyid = MyKid then begin
                AddLog ('Repeating with saved public key for KID: ' + NewKeyid);
                NewPubKey := FVerifyPKey.PrivateKey;
                VerifyRes := IcsJoseCheckJWS(MyJWS, Nonce, '', NewPubKey, NewKeyid, Payload);
                AddLog ('Verify Result: ' + JoseVerifyLits[VerifyRes]);
            end;
        end;
        if Payload <> '' then AddLog (Payload);
        AddLog ('');
    end;


begin
    Data := Trim(JoseTextLines.Lines.Text);
    AddLog ('Building Jason Web Key using Algorithm:: ' + TestJWSAlg.Items[TestJWSAlg.ItemIndex]);
    AddLog ('Data: ' + Data);
    try
        Mykid := IntToStr(GetTickCount);
        if TestJWSAlg.ItemIndex < 1 then Exit;
        JoseAlg := TJoseAlg(TestJWSAlg.ItemIndex);
        if JoseAlg >= jsigRsa256 then begin
            if NOT Assigned(FPrivateKey.PrivateKey) then begin
                AddLog ('Need to create new private key or load old key file, first');
                Exit;
            end;

        // V8.65 keep public key for verify without JWK
            FVerifyPKey.PublicKeyLoadFromText(FPrivateKey.PublicKeySaveToText);
            AddLog ('Public Key: ' + FPrivateKey.PublicKeySaveToText);
            AddLog ('');
        end;

   // build Json Web Key
        MyAlg := IcsJoseFindAlg(JoseAlg, FPrivateKey.PrivateKey);
        if JoseAlg >= jsigRsa256 then
        begin
            if ShowRawKey.Checked then
                AddLog (FPrivateKey.GetPKeyRawText);  { V8.64 }
            KwkPub := IcsJoseJWKPubKey(FPrivateKey.PrivateKey, Myalg, Mykid, JoseLituse_sig);
            AddLog ('IcsJoseJWKPkey Raw: ' + KwkPub);     { V8.64 }
            AddLog ('');
         //   RespJson := SO(KwkPub);
         //   AddLog ('IcsJoseJWK Pkey: ' + RespJson.AsJson(true, false));
        end
        else
        begin
            AddLog ('HMAC secret: ' + TestHmacKey.Text );
            KwkPub := IcsJoseJWKHmac(TestHmacKey.Text, Myalg, Mykid, JoseLituse_sig);
            RespJson := SO(KwkPub);
            AddLog ('IcsJoseJWK HMac: ' + RespJson.AsJson(true, false));
        end;
        AddLog ('');

    // build Json header, unsigfed
        S := String(IcsJoseHeader(Myalg, 'jws', KwkPub, '', 'Nonce'));
        AddLog ('IcsJoseHeader Raw: ' + S);
        AddLog ('');
        RespJson := SO(S);
        AddLog ('IcsJoseHeader: ' + RespJson.AsJson(true, false));
        AddLog ('');

    // build JWS in compacyt (x.x.x) and Json formats
        AddLog ('Building Jason Web Signatures, sending JWK or KID');
        CompactJWSKey := IcsJoseJWSComp(JoseAlg, Data, TestHmacKey.Text, FPrivateKey.PrivateKey,
                                                    'jws', KwkPub, '', '101');
        AddLog ('IcsJoseJWS Compact JWK: ' + CompactJWSKey) ;

        CompactJWSKid := IcsJoseJWSComp(JoseAlg, Data, TestHmacKey.Text, FPrivateKey.PrivateKey,
                                                    'jws', '', MyKid, '102');
        AddLog ('IcsJoseJWS Compact KID: ' + CompactJWSKid) ;
        AddLog ('');

        JsonJWSKey := IcsJoseJWSJson(JoseAlg, Data, TestHmacKey.Text, FPrivateKey.PrivateKey,
                                                    'jws', KwkPub, '', '103');
        RawJWK.Text := JsonJWSKey;
        RespJson := SO(JsonJWSKey);
        AddLog ('IcsJoseJWS Json JWK: ' + RespJson.AsJson(true, false));

        JsonJWSKid := IcsJoseJWSJson(JoseAlg, Data, TestHmacKey.Text, FPrivateKey.PrivateKey,
                                                    'jws', '', MyKid, '104');
        RespJson := SO(JsonJWSKid);
        AddLog ('IcsJoseJWS Json KID: ' + RespJson.AsJson(true, false));
        AddLog ('');


    // verify JWS then all
        AddLog ('Verifying Json Web Signature - Compact JWS with new key');
        VerifyJWS(CompactJWSKey);
        AddLog ('Verifying Json Web Signature - Compact JWS with KID');
        VerifyJWS(CompactJWSKid);
        AddLog ('Verifying Json Web Signature - Json JWS with new key');
        VerifyJWS(JsonJWSKey);
        AddLog ('Verifying Json Web Signature - Json JWS with KID');
        VerifyJWS(JsonJWSKid);

    except
        on E:Exception do
            AddLog ('Json Web Signature exception - ' + E.Message);
    end;
    AddLog ('');
end;

procedure TJsonDemoForm.doJWSReadClick(Sender: TObject);
var
    NewKeyid, Payload: string;
    VerifyRes: TJoseVerify;
    NewPubKey: PEVP_PKEY;
begin
    NewPubKey := FVerifyPKey.PrivateKey;
    AddLog ('CheckJWS: ' + RawJWK.Text);
    VerifyRes := IcsJoseCheckJWS(RawJWK.Text, '', TestHmacKey.Text, NewPubKey, NewKeyid, Payload);
    AddLog ('Verify Result: ' + JoseVerifyLits[VerifyRes]);
 // need to save new public key
    if VerifyRes = JVerifyOkNewPubKey then begin
        FVerifyPKey.PrivateKey := NewPubKey;
        AddLog('Saved JWK: ' + Trim(FVerifyPKey.PublicKeySaveToText));
    end;
    if Payload <> '' then AddLog (Payload);
    AddLog ('');
end;



procedure TJsonDemoForm.doLoadFileClick(Sender: TObject);
begin
    if OpenDlg.Execute then begin
        JsonInput.Lines.LoadFromFile(OpenDlg.FileName);
    end;
end;
procedure TJsonDemoForm.doParseClick(Sender: TObject);
var
    JsonItem: TSuperAvlEntry;
    JsonObj, RespObj: ISuperObject;
    JsonEnum: TSuperAvlIterator;
    I, CWid: integer;
    FirstCol, FirstRow: Boolean;
    SourceTxt: UnicodeString;
    CVal, ErrStr: String;
begin
    RespObj := Nil;
    JsonGrid.Items.Clear;
    SourceTxt := JsonInput.Lines.Text;

  // look for Json response }
    if ((Pos('{', SourceTxt) > 0)  or (Pos('[', SourceTxt) > 0) or (Sender = doParseJson)) then begin
        try
            RespObj := TSuperObject.ParseStringEx(PWideChar(SourceTxt), True, ErrStr);
            if ErrStr <> '' then
                AddLog('Error parsing Json: ' + ErrStr);
        except
            on E:Exception do
                AddLog('Error parsing Json: ' + E.Message);
        end;
        if Assigned(RespObj) then
            AddLog ('Json main content type: ' +
               GetEnumName(TypeInfo(TSuperType), Ord(RespObj.DataType)));
    end;

  // V8.64 look for XML response }
    if ((Pos('<?xml version=', SourceTxt) > 0) or (Sender = doParseXML)) then begin
        try
            RespObj := XMLParseString(SourceTxt, CompactXML.Checked);
        except
            on E:Exception do
                AddLog('Error parsing XML: ' + E.Message);
        end;
    end;
    if NOT Assigned(RespObj) then Exit;  // nothing to show

  // parse Json or XML response to grid
    if LogJson.Checked then AddLog (RespObj.AsJson (True, False));  // formatted
    try
     // note that values containing objects are displayed as raw Json
        JArrayTot := 0;
        if RespObj.DataType = stObject then begin
            JsonGrid.Columns.Clear;
            with JsonGrid.Columns.Add do begin
                Caption := 'Name';
                Width := 100;
            end;
            with JsonGrid.Columns.Add do begin
                Caption := 'Type';
                Width := 70;
            end;
            with JsonGrid.Columns.Add do begin
                Caption := 'Value';
                Width := 1000;
            end;
            with JsonGrid.Columns.Add do begin
                Caption := '';
                Width := 100;
            end;
            JsonEnum := RespObj.AsObject.GetEnumerator;
            try
                while JsonEnum.MoveNext do begin
                    JsonItem := JsonEnum.GetIter;
                    with JsonGrid.Items.Add do begin
                        Caption := JsonItem.Name;
                        SubItems.Add(GetEnumName(TypeInfo(TSuperType),
                                                Ord(JsonItem.Value.DataType)));
                        CVal := JsonItem.Value.AsString;
                        SubItems.Add(CVal);
                    end;
                end;
            finally
                JsonEnum.Free;
            end;
        end;

     // one column per Value, with Name as title
        if RespObj.DataType = stArray then begin
            JsonGrid.Items.BeginUpdate;
            JsonGrid.Columns.Clear;
            JArrayTot := RespObj.AsArray.Length;
            if JArrayTot = 0 then Exit;
            SetLength(JArrayItems, JArrayTot);
            FirstRow := True;
            for I := 0 to JArrayTot - 1 do begin
                JsonObj := RespObj.AsArray[I];
                JArrayItems[I] := RespObj.AsArray[I].AsString;   { V8.65 keep lines so we display them later }
                FirstCol := True;
                with JsonGrid.Items.Add do begin
                    JsonEnum := JsonObj.AsObject.GetEnumerator;
                    while JsonEnum.MoveNext do begin
                        JsonItem := JsonEnum.GetIter;
                        CVal := JsonItem.Value.AsString;
                        if FirstRow then begin
                            CWid := (Length(CVal) * 5) + 30;
                            if CWid > 400 then CWid := 400;
                            with JsonGrid.Columns.Add do begin
                                Caption := JsonItem.Name;
                                Width := CWid;
                            end;
                        end;
                        if FirstCol then
                            Caption := CVal
                        else
                            SubItems.Add(CVal);
                        FirstCol := False;
                    end;
                end;
                FirstRow := False;
            end;
            JsonGrid.Items.EndUpdate;
        end;

    except
        on E:Exception do
            AddLog('Error parsing Json: ' + E.Message);
    end;
end;

procedure TJsonDemoForm.JsonGridDblClick(Sender: TObject);
var
    I: Integer;
begin
    if NOT Assigned(FormObject) then Exit;
    if JsonGrid.ItemIndex < 0 then Exit;
    FormObject.SubJsonGrid.Items.Clear;
    if (JArrayTot > 0) and (Length(JArrayItems) = JArrayTot) then begin { V8.65 one array element }
        FormObject.DispJson(JArrayItems[JsonGrid.ItemIndex]);
    end
    else begin
        with JsonGrid.Items[JsonGrid.ItemIndex] do begin
            if (SubItems.Count >= 2) and ((SubItems[0] = 'stArray') or
                                                (SubItems[0] = 'stObject')) then
                FormObject.DispJson(SubItems[1])

         // V8.64 array may have Json object in any column, search for first, sorry ignore others...
            else if ((Pos ('{', Caption) = 1) or (Pos ('[', Caption) = 1)) then
                FormObject.DispJson(Caption)
            else if (SubItems.Count > 0) then begin
                for I := 0 to SubItems.Count - 1 do begin
                    if ((Pos ('{', SubItems[I]) = 1) or (Pos ('[', SubItems[I]) = 1)) then begin
                        FormObject.DispJson(SubItems[I]);
                        break;
                    end;
                end;
            end;
            FormObject.BringToFront;
        end;
    end;
end;

// report lots of private key parameters

procedure TJsonDemoForm.doKeyParamsClick(Sender: TObject);
begin
    if NOT Assigned(FPrivateKey.PrivateKey) then begin
        AddLog ('Need to create new private key or load old key file, first');
        Exit;
    end;

// report key parameters, OpenSSL 3.0 and later
    if ICS_OPENSSL_VERSION_MAJOR < 3 then begin
        AddLog ('Requires OpenSSL 3.0 or later');
        Exit;
    end;
    AddLog ('IcsPkeyParamGettable: ' + IcsCRLF + IcsPkeyParamGettable(FPrivateKey.PrivateKey));
//    AddLog ('IcsPkeyParamSettable: ' + IcsCRLF + IcsPkeyParamSettable(FPrivateKey.PrivateKey)); only for new key 

    try
        AddLog (IcsPkeyParamsGetAll(FPrivateKey.PrivateKey));
    except
        on E:Exception do
            AddLog('Error printing parameters: ' + E.Message);
    end;
end;


end.
