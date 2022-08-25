unit OverbyteIcsPemTool3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
{$IF CompilerVersion > 23}
  System.UITypes,
{$IFEND}
  Dialogs, StdCtrls, ExtCtrls;

type
  TToDo = (tdCreateSelfSignedCert, tdCreateCertRequest);
  TfrmPemTool3 = class(TForm)
    GroupBoxCertCreate: TGroupBox;
    EditCountry: TEdit;
    EditState: TEdit;
    EditLocality: TEdit;
    EditOrganization: TEdit;
    EditOrganizationalUnit: TEdit;
    EditCommonName: TEdit;
    EditEMail: TEdit;
    EditBits: TEdit;
    lbCountry: TLabel;
    lbState: TLabel;
    lbLocality: TLabel;
    lbOrganization: TLabel;
    lbOrganizationalUnit: TLabel;
    lbCommonName: TLabel;
    lbEMail: TLabel;
    lbBits: TLabel;
    CheckBoxCA: TCheckBox;
    lbInfo: TLabel;
    btnCreate: TButton;
    btnClose: TButton;
    Editdays: TEdit;
    lbDays: TLabel;
    CheckBoxComment: TCheckBox;
    Label1: TLabel;
    procedure btnCreateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FToDo : TToDo;
  public
    property ToDo : TToDo read FToDo write FToDo;
  end;

var
  frmPemTool3: TfrmPemTool3;

implementation

{$R *.dfm}

uses
    OverbyteIcsPemtool1, OverbyteIcsSslX509Utils;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool3.FormShow(Sender: TObject);
begin
    CheckBoxCA.Visible := FToDo = tdCreateSelfSignedCert;
    EditDays.Visible   := FToDo = tdCreateSelfSignedCert;
    lbDays.Visible     := FToDo = tdCreateSelfSignedCert;

    if FToDo = tdCreateSelfSignedCert then
    begin
       Caption := 'Create self-signed certificate and private key';
       lbInfo.Caption  := 'Certificate and private key are written to the same file.'#13#10 +
                          'Private key will not be password protected.';
    end
    else begin
       Caption := 'Create certificate request and private key';
       lbInfo.Caption  := 'Request and private key are written to different files.'#13#10 +
                          'Private key will not be password protected.';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool3.btnCreateClick(Sender: TObject);
var
    Dlg : TSaveDialog;
    KeyFileName : String;
begin
    Dlg := TSaveDialog.Create(nil);
    try
        Dlg.DefaultExt := '.pem';
        Dlg.Filter     := 'PEM files|*.pem|All files|*.*';

        if FToDo = tdCreateCertRequest then
        begin
            Dlg.Title := 'Where do you want the private key file to be stored';
            if Dlg.Execute then
                KeyFileName := Dlg.FileName;
            Dlg.FileName := '';
            Dlg.Title := 'Where do you want the request file to be stored';
        end
        else
            Dlg.Title := 'Where do you want the certificate file to be stored';
        if FToDo = tdCreateSelfSignedCert then
        begin
            if Dlg.Execute then
                CreateSelfSignedCert(Dlg.FileName, EditCountry.Text,
                                     EditState.Text,
                                     EditLocality.Text, EditOrganization.Text,
                                     EditOrganizationalUnit.Text,
                                     EditCommonName.Text, EditEMail.Text,
                                     StrToIntDef(EditBits.Text, 2048), TRUE,
                                     StrToIntDef(EditDays.Text, 365), '',
                                     CheckBoxComment.Checked);
        end
        else begin
            if KeyFileName = '' then Exit;
            Dlg.FileName := '';
            while Dlg.Execute and (KeyFileName = Dlg.FileName) do
            begin
                if MessageDlg('Key and request cannot be stored in the same file.'#13#10 +
                              'Do you want to select a different request file name now?',
                              mtError, [mbYes, mbNo], 0) <> mrYes then
                    Exit;
            end;
            CreateCertRequest(Dlg.FileName, KeyFileName, EditCountry.Text,
                              EditState.Text, EditLocality.Text,
                              EditOrganization.Text, EditOrganizationalUnit.Text,
                              EditCommonName.Text, EditEMail.Text,
                              StrToIntDef(EditBits.Text, 2048),
                              CheckBoxComment.Checked);
        end;
    finally
        Dlg.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}


end.
