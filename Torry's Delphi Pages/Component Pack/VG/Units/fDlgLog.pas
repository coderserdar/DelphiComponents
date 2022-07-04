{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Login dialog                                  }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit fDlgLog;

interface

uses
  Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls, fDlgStd;

type
  TLoginDialogForm = class(TDialogForm)
    lbLogin: TLabel;
    lbPassword: TLabel;
    edLogin: TEdit;
    edPassword: TEdit;
    procedure edLoginChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FPasswordRequired: Boolean;
  public
    { Public declarations }
  end;

  TLoginOption = (loNameReadOnly, loNameUpperCase, loPasswordRequired);
  TLoginOptions = set of TLoginOption;

function LoginDialog(var AUserName, APassword: string; Options: TLoginOptions): Boolean;

implementation
uses vgVCLRes, SysUtils;

{$R *.DFM}

function LoginDialog(var AUserName, APassword: string; Options: TLoginOptions): Boolean;
begin
  with TLoginDialogForm.Create(Application) do
  try
    edLogin.Text := AUserName;
    if (loNameUpperCase in Options) then
      edLogin.CharCase := ecUpperCase;
    edPassword.Text := APassword;
    edLogin.Enabled := not (loNameReadOnly in Options);
    if edLogin.Enabled and (AUserName <> '') then
      ActiveControl := edPassword;
    FPasswordRequired := loPasswordRequired in Options;
    Result := ShowModal = mrOK;
    if Result then
    begin
      AUserName := edLogin.Text;
      APassword := edPassword.Text;
    end;
  finally
    Free;
  end;
end;

procedure TLoginDialogForm.FormShow(Sender: TObject);
begin
  inherited;
  Caption := LoadStr(SLoginDialogCaption);
  lbLogin.Caption := LoadStr(SLoginCaption);
  lbPassword.Caption := LoadStr(SLoginPassword);
  edLoginChange(nil);
end;

procedure TLoginDialogForm.edLoginChange(Sender: TObject);
begin
  cmOK.Enabled := not FPasswordRequired or (edPassword.Text <> '');
end;

end.
