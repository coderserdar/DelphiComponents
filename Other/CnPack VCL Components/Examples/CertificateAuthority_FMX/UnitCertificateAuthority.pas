unit UnitCertificateAuthority;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Controls, FMX.Dialogs, FMX.Edit, FMX.Forms, FMX.Graphics, FMX.ListBox, FMX.Memo, FMX.StdCtrls, FMX.TabControl, FMX.Types,
  FMX.ScrollBox, FMX.Controls.Presentation, CnRSA, CnCertificateAuthority;

type
  TFormCA = class(TForm)
    pgc1: TTabControl;
    tsRequest: TTabItem;
    grpGenRequest: TGroupBox;
    lblKey: TLabel;
    lblContryName: TLabel;
    lblStateOrProvinceName: TLabel;
    lblLocalityName: TLabel;
    lblOrgName: TLabel;
    lblOrgUnitName: TLabel;
    lblCommonName: TLabel;
    lblEmail: TLabel;
    lblHash: TLabel;
    edtRSAKey: TEdit;
    btnBrowseKey: TButton;
    edtContryName: TEdit;
    edtStateOrProvinceName: TEdit;
    edtLocalityName: TEdit;
    edtOrgName: TEdit;
    edtOrgUnitName: TEdit;
    edtCommonName: TEdit;
    edtEmail: TEdit;
    cbbHash: TComboBox;
    btnGenerateCSR: TButton;
    btnSelfSign: TButton;
    grpParse: TGroupBox;
    lblCSR: TLabel;
    edtCSR: TEdit;
    btnBrowseCSR: TButton;
    mmoCSRParse: TMemo;
    btnParseCSR: TButton;
    btnVerifyCSR: TButton;
    tsSign: TTabItem;
    grpSign: TGroupBox;
    lblSignCSR: TLabel;
    lblRoot: TLabel;
    lblRootCrt: TLabel;
    edtSignCSR: TEdit;
    btnSignCSRBrowse: TButton;
    edtSignKey: TEdit;
    btnSignKeyBrowse: TButton;
    btnSign: TButton;
    edtRootCRT: TEdit;
    btnRootCRTBrowse: TButton;
    grpParseCER: TGroupBox;
    lblCRT: TLabel;
    edtCRT: TEdit;
    btnBrowseCRT: TButton;
    mmoCRT: TMemo;
    btnParseCRT: TButton;
    btnVerifyCRT: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBrowseKeyClick(Sender: TObject);
    procedure btnGenerateCSRClick(Sender: TObject);
    procedure btnSelfSignClick(Sender: TObject);
    procedure btnBrowseCSRClick(Sender: TObject);
    procedure btnParseCSRClick(Sender: TObject);
    procedure btnVerifyCSRClick(Sender: TObject);
    procedure btnSignCSRBrowseClick(Sender: TObject);
    procedure btnSignKeyBrowseClick(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure btnRootCRTBrowseClick(Sender: TObject);
    procedure btnBrowseCRTClick(Sender: TObject);
    procedure btnParseCRTClick(Sender: TObject);
    procedure btnVerifyCRTClick(Sender: TObject);
  private
    FCPriv: TCnRSAPrivateKey;
    FCPub: TCnRSAPublicKey;
    FSPriv: TCnRSAPrivateKey;
    FSPub: TCnRSAPublicKey;
  public

  end;

var
  FormCA: TFormCA;

implementation

{$R *.fmx}

function PrintHex(const Buf: Pointer; Len: Integer): string;
var
  I: Integer;
  P: PByteArray;
const
  Digits: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7',
                                  '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
begin
  Result := '';
  P := PByteArray(Buf);
  for I := 0 to Len - 1 do
  begin
    Result := Result + {$IFDEF UNICODE}string{$ENDIF}(Digits[(P[I] shr 4) and $0F] +
              Digits[P[I] and $0F]);
  end;
end;

procedure TFormCA.FormCreate(Sender: TObject);
begin
  cbbHash.ItemIndex := 1;
  FCPriv := TCnRSAPrivateKey.Create;
  FCPub := TCnRSAPublicKey.Create;
  FSPriv := TCnRSAPrivateKey.Create;
  FSPub := TCnRSAPublicKey.Create;
end;

procedure TFormCA.btnBrowseCSRClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtCSR.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnBrowseKeyClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtRSAKey.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnParseCSRClick(Sender: TObject);
var
  CSR: TCnRSACertificateRequest;
  OutBuf: array of Byte;
  OutLen: Integer;
begin
  CSR := TCnRSACertificateRequest.Create;
  if CnCALoadCertificateSignRequestFromFile(edtCSR.Text, CSR) then
  begin
    mmoCSRParse.Lines.Clear;
    mmoCSRParse.Lines.Add(CSR.ToString);

    if (CSR.SignValue <> nil) and (CSR.SignLength > 0) and (CSR.PublicKey.BitsCount > 128) then
    begin
      SetLength(OutBuf, CSR.PublicKey.BitsCount div 8);
      if CnRSADecryptRawData(CSR.SignValue, CSR.SignLength, @OutBuf[0], OutLen, CSR.PublicKey) then
      begin
        mmoCSRParse.Lines.Add('');
        mmoCSRParse.Lines.Add('--------');
        mmoCSRParse.Lines.Add('Digest after RSA Decryption:');
        mmoCSRParse.Lines.Add(PrintHex(@OutBuf[0], OutLen));
      end;
    end;
  end
  else
    ShowMessage('Parse CSR Failed.');
  CSR.Free;
end;

procedure TFormCA.btnGenerateCSRClick(Sender: TObject);
begin
  if FileExists(edtRSAKey.Text) and CnRSALoadKeysFromPem(edtRSAKey.Text, FCPriv, FCPub) then
  begin
    if dlgSave.Execute then
    begin
      if CnCANewCertificateSignRequest(FCPriv, FCPub, dlgSave.FileName, edtContryName.Text,
        edtStateOrProvinceName.Text, edtLocalityName.Text, edtOrgName.Text,
        edtOrgUnitName.Text, edtCommonName.Text, edtEmail.Text, TCnCASignType(cbbHash.ItemIndex)) then
        ShowMessage('Generate CSR File Success.')
      else
        ShowMessage('Generate CSR File Fail.');
    end;
  end
  else
    ShowMessage('Invalid RSA Keys');
end;

procedure TFormCA.FormDestroy(Sender: TObject);
begin
  FSPub.Free;
  FSPriv.Free;
  FCPub.Free;
  FCPriv.Free;
end;

procedure TFormCA.btnBrowseCRTClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtCRT.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnParseCRTClick(Sender: TObject);
var
  CRT: TCnRSACertificate;
begin
  CRT := TCnRSACertificate.Create;
  if not CnCALoadCertificateFromFile(edtCRT.Text, CRT) then
    ShowMessage('Parse CRT File Failed.')
  else
  begin
    mmoCRT.Lines.Clear;
    mmoCRT.Lines.Add(CRT.ToString);
  end;
  CRT.Free;
end;

procedure TFormCA.btnVerifyCSRClick(Sender: TObject);
begin
  if CnCAVerifyCertificateSignRequestFile(edtCSR.Text) then
    ShowMessage('CSR Verify OK.')
  else
    ShowMessage('CSR Verify Fail.');
end;

procedure TFormCA.btnSelfSignClick(Sender: TObject);
begin
  if FileExists(edtRSAKey.Text) and CnRSALoadKeysFromPem(edtRSAKey.Text, FCPriv, FCPub) then
  begin
    if dlgSave.Execute then
    begin
      if CnCANewSelfSignedCertificate(FCPriv, FCPub, dlgSave.FileName, edtContryName.Text,
        edtStateOrProvinceName.Text, edtLocalityName.Text, edtOrgName.Text,
        edtOrgUnitName.Text, edtCommonName.Text, edtEmail.Text, '1234567890987654321',
        Now - 1, Now + 365, TCnCASignType(cbbHash.ItemIndex)) then
        ShowMessage('Self-Signed CRT File OK.')
      else
        ShowMessage('Self-Signed CRT File Fail.');
    end;
  end;
end;

procedure TFormCA.btnVerifyCRTClick(Sender: TObject);
begin
  if CnCAVerifySelfSignedCertificateFile(edtCRT.Text) then
    ShowMessage('Self-Signed CRT Verify OK.')
  else
    ShowMessage('Self-Signed CRT Verify Fail.');
end;

procedure TFormCA.btnSignClick(Sender: TObject);
begin
  if FileExists(edtSignCSR.Text) and FileExists(edtRootCRT.Text) and FileExists(edtSignKey.Text) then
  begin
    if CnRSALoadKeysFromPem(edtSignKey.Text, FSPriv, FSPub) then
    begin
      if dlgSave.Execute then
      begin
        if CnCASignCertificate(FSPriv, edtRootCRT.Text, edtSignCSR.Text, dlgSave.FileName,
          '1234567890987654321', Now - 1, Now + 365, TCnCASignType(cbbHash.ItemIndex)) then
          ShowMessage('Sign CRT File OK.')
        else
          ShowMessage('Sign CRT File Fail.');
      end;
    end;
  end;
end;

procedure TFormCA.btnSignCSRBrowseClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtSignCSR.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnSignKeyBrowseClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtSignKey.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnRootCRTBrowseClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtRootCRT.Text := dlgOpen.FileName;
end;

end.
