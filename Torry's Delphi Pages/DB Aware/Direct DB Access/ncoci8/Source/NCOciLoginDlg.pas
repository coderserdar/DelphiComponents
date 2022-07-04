{*******************************************************}
{File:      NCOciLoginDlg.pas                           }
{Revision:  0.02.01 / 01.04.2000                        }
{Comment:   NC OCI8 VCL: Oracle8 login dialog           }
{Copyright: (c) 1999-2000, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, darefiev@da-soft.com        }
{*******************************************************}
{$I NCOciDef.inc}

unit NCOciLoginDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, NCOci, NCOciWrapper, NCOciDB, ExtCtrls;

type
  TNCOciLoginFrm = class(TForm)
    tbshtLogin: TPanel;
    tbshtNewPassword: TPanel;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    pnlStd: TPanel;
    Label1: TLabel;
    edtUserName: TEdit;
    Label2: TLabel;
    edtPassword: TEdit;
    pnlAuthMode: TPanel;
    Label4: TLabel;
    cmbbxAuthMode: TComboBox;
    pnlProfile: TPanel;
    Label5: TLabel;
    cmbbxProfile: TComboBox;
    spdbttnDelProfile: TSpeedButton;
    Bevel1: TBevel;
    Label6: TLabel;
    edtNewPassword: TEdit;
    Memo1: TMemo;
    Label7: TLabel;
    Label8: TLabel;
    edtVerify: TEdit;
    pnlService: TPanel;
    Label3: TLabel;
    cmbbxService: TComboBox;
    procedure cmbbxProfileClick(Sender: TObject);
    procedure spdbttnDelProfileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edtNewPasswordChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FPrevStdLogin: Boolean;
    FTabButtonHeight: Integer;
    FDatabase: TOCIDatabase;
    FCancelY, FCancelX, FOkY, FOkX: Integer;
  public
    { Public declarations }
    class function Execute(ADatabase: TOCICustomDatabase; AStdLogin: Boolean): Boolean;
  end;

var
  NCOciLoginFrm: TNCOciLoginFrm;

implementation

{$R *.DFM}

Uses Registry;

procedure TNCOciLoginFrm.FormCreate(Sender: TObject);
begin
    FPrevStdLogin := False;
    FTabButtonHeight := tbshtLogin.Top;
    FCancelX := ClientWidth - (btnCancel.Left + btnCancel.Width);
    FCancelY := ClientHeight - (btnCancel.Top + btnCancel.Height);
    FOkX := ClientWidth - (btnOk.Left + btnOk.Width);
    FOkY := ClientHeight - (btnOk.Top + btnOk.Height);
end;

class function TNCOciLoginFrm.Execute(ADatabase: TOCICustomDatabase; AStdLogin: Boolean): Boolean;
begin
    if NCOciLoginFrm = nil then
        NCOciLoginFrm := TNCOciLoginFrm.Create(nil);
    with NCOciLoginFrm do begin
        FDatabase := TOCIDatabase(ADatabase);
        pnlAuthMode.Visible := lfMode in FDatabase.LoginFields;
        pnlProfile.Visible := lfProfile in FDatabase.LoginFields;
        pnlService.Visible := lfService in FDatabase.LoginFields;
        edtUserName.Text := FDatabase.UserName;
        edtPassword.Text := FDatabase.Password;
        if pnlService.Visible then begin
            TOCICustomDatabase.GetObjectsList(FDatabase.DatabaseName, cmbbxService.Items, '', okService, False);
            cmbbxService.Text := FDatabase.ServerName;
        end;
        if pnlAuthMode.Visible then
            cmbbxAuthMode.ItemIndex := Integer(FDatabase.AuthentMode);
        if pnlProfile.Visible then
            with TRegistry.Create do
            try
                RootKey := HKEY_CURRENT_USER;
{$IFDEF OCI_D4}
                if OpenKeyReadOnly(SNCVCLRoot + SOCIProfiles) then begin
{$ELSE}
                if OpenKey(SNCVCLRoot + SOCIProfiles, False) then begin
{$ENDIF}                
                    GetKeyNames(cmbbxProfile.Items);
                    cmbbxProfile.ItemIndex := cmbbxProfile.Items.IndexOf(
                        AnsiUpperCase(FDatabase.ServerName + ' - ' + FDatabase.UserName)
                    );
                end
                else
                    cmbbxProfile.Items.Clear;
            finally
                Free;
            end;
        Label1.Enabled := AStdLogin;
        Label2.Enabled := AStdLogin;
        Label3.Enabled := AStdLogin;
        if pnlAuthMode.Visible then
            Label4.Enabled := AStdLogin;
        if pnlProfile.Visible then
            Label5.Enabled := AStdLogin;
        edtUserName.Enabled := AStdLogin;
        edtPassword.Enabled := AStdLogin;
        if pnlService.Visible then
            cmbbxService.Enabled := AStdLogin;
        if pnlAuthMode.Visible then
            cmbbxAuthMode.Enabled := AStdLogin;
        if pnlProfile.Visible then begin
            cmbbxProfile.Enabled := AStdLogin;
            spdbttnDelProfile.Enabled := AStdLogin;
        end;
        Label6.Enabled := not AStdLogin;
        edtNewPassword.Enabled := not AStdLogin;
        edtNewPassword.Text := '';
        Label8.Enabled := not AStdLogin;
        edtVerify.Enabled := not AStdLogin;
        edtVerify.Text := '';
        if FPrevStdLogin <> AStdLogin then begin
            if not AStdLogin then
                Height := Height + FTabButtonHeight
            else
                Height := Height - FTabButtonHeight;
            FPrevStdLogin := AStdLogin;
        end;
        tbshtLogin.Visible := AStdLogin;
        tbshtNewPassword.Visible := not AStdLogin;
        btnOk.Enabled := AStdLogin;
        Result := ShowModal = mrOK;
        if Result then begin
            if not AStdLogin then
                FDatabase.ChangePassword(edtNewPassword.Text)
            else begin
                FDatabase.UserName := edtUserName.Text;
                FDatabase.Password := edtPassword.Text;
                if pnlService.Visible then
                    FDatabase.ServerName := cmbbxService.Text;
                if pnlAuthMode.Visible then
                    FDatabase.AuthentMode := TOCIAuthentMode(cmbbxAuthMode.ItemIndex);
                if pnlProfile.Visible then
                    with TRegistry.Create do
                    try
                        RootKey := HKEY_CURRENT_USER;
                        if OpenKey(SNCVCLRoot + SOCIProfiles + '\' +
                                   AnsiUpperCase(cmbbxService.Text + ' - ' + edtUserName.Text), True) then begin
                            WriteString('UserName', edtUserName.Text);
                            if suStorePassword in FDatabase.ConnectStringUsages then
                                WriteString('Password', edtPassword.Text)
                            else
                                DeleteValue('Password');
                            WriteString('ServerName', cmbbxService.Text);
                            if pnlAuthMode.Visible then
                                WriteInteger('AuthentMode', cmbbxAuthMode.ItemIndex);
                        end;
                    finally
                        Free;
                    end;
            end;
        end;
    end;
    Application.ProcessMessages;
end;

procedure TNCOciLoginFrm.cmbbxProfileClick(Sender: TObject);
begin
    if pnlProfile.Visible then
        with TRegistry.Create do
        try
            RootKey := HKEY_CURRENT_USER;
{$IFDEF OCI_D4}
            if OpenKeyReadOnly(SNCVCLRoot + SOCIProfiles + '\' + cmbbxProfile.Text) then begin
{$ELSE}
            if OpenKey(SNCVCLRoot + SOCIProfiles + '\' + cmbbxProfile.Text, False) then begin
{$ENDIF}
                edtUserName.Text := ReadString('UserName');
                if suStorePassword in FDatabase.ConnectStringUsages then
                    edtPassword.Text := ReadString('Password')
                else
                    edtPassword.Text := '';
                cmbbxService.Text := ReadString('ServerName');
                if pnlAuthMode.Visible then
                    cmbbxAuthMode.ItemIndex := ReadInteger('AuthentMode');
            end;
        finally
            Free;
        end;
end;

procedure TNCOciLoginFrm.spdbttnDelProfileClick(Sender: TObject);
begin
    if pnlProfile.Visible then
        with TRegistry.Create do
        try
            RootKey := HKEY_CURRENT_USER;
            DeleteKey(SNCVCLRoot + SOCIProfiles + '\' + cmbbxProfile.Text);
            cmbbxProfile.Items.Delete(cmbbxProfile.ItemIndex);
            cmbbxProfile.ItemIndex := -1;
            cmbbxProfile.Text := '';
        finally
            Free;
        end;
end;

procedure TNCOciLoginFrm.edtNewPasswordChange(Sender: TObject);
begin
    btnOk.Enabled := (edtNewPassword.Text <> '') and
        (edtNewPassword.Text = edtVerify.Text);
end;

procedure TNCOciLoginFrm.FormResize(Sender: TObject);
begin
    btnCancel.Left := ClientWidth - FCancelX - btnCancel.Width;
    btnCancel.Top := ClientHeight - FCancelY - btnCancel.Height;
    btnOk.Left := ClientWidth - FOkX - btnOk.Width;
    btnOk.Top := ClientHeight - FOkY - btnOk.Height;
end;

initialization
    NCOciLoginFrm := nil;

finalization
    NCOciLoginFrm.Free;
    NCOciLoginFrm := nil;

end.
