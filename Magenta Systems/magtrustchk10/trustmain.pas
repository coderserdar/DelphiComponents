unit trustmain;

{
Updated by Angus Robertson, Magenta Systems Ltd, England, 26th November 2018
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

SignProg functions are designed for Code Signing, aka Microsoft Authenticode

Test application for two functions in MagSignProg.pas

26 Nov 2018 - renamed signprog to MagSignProg for consistency 


}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComObj, Mask, StdCtrls, ActiveX, StrUtils, MagSignProg, CAPICOM_TLB  ;

type
  TForm4 = class(TForm)
    doClose: TButton;
    Log: TMemo;
    doVerifyTrust: TButton;
    doChkCert: TButton;
    OpenDialog: TOpenDialog;
    FileNames: TComboBox;
    doSelect: TButton;
    procedure doCloseClick(Sender: TObject);
    procedure doVerifyTrustClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure doChkCertClick(Sender: TObject);
    procedure doSelectClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.doCloseClick(Sender: TObject);
begin
    Close ;
end;

procedure TForm4.doSelectClick(Sender: TObject);
begin
    OpenDialog.FileName := Filenames.Text ;
    if OpenDialog.Execute (Handle) then
    begin
        Filenames.Text := OpenDialog.FileName ;
        if Filenames.Items.IndexOf (Filenames.Text) < 0 then
                          Filenames.Items.Add (Filenames.Text) ;
    end;
end;

procedure TForm4.doVerifyTrustClick(Sender: TObject);
var
    S: string ;
begin
    ProgVerifyTrust (Filenames.Text, false, true, S) ;
    Log.Lines.Add ('Verify Trust for ' + Filenames.Text + ' - ' + S + #13#10) ;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
      CoInitialize(nil);
      Filenames.Text := ExtractFilePath (ParamStr(0)) + 'signed-samples\trustok.exe' ;
      Filenames.Items.Add (Filenames.Text) ;
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
    CoUninitialize;
end;

procedure TForm4.doChkCertClick(Sender: TObject);
var
    CertInfo: TCertInfo ;
    S: string ;
begin
    ProgVerifyCert (Filenames.Text, CertInfo) ;
    if CertInfo.Res = 0 then
        S := 'Check Cert OK for '
    else
        S := 'Check Cert Failed for ' ;
    S := S + Filenames.Text + #13#10 + CertInfo.Response + #13#10 ;
    if CertInfo.SignerName <> '' then
    begin
        S := S + 'Signed by: ' +  CertInfo.SignerName + #13#10 +
        'Issued by: ' +  CertInfo.SignerIssuer + #13#10 +
        'Cert Expires: ' + DateToStr (CertInfo.SignerExpireDT) + #13#10 +
        'UTC Timestamp: ' +  DateTimeToStr (CertInfo.SigningDT) + #13#10 +
        'Timestamped by: ' +  CertInfo.StamperName + #13#10 +
        'Description: ' +  CertInfo.DescName + #13#10 +
        'URL: ' +  CertInfo.DescURL + #13#10 ;
    end ;
    Log.Lines.Add (S) ;
end;

end.

