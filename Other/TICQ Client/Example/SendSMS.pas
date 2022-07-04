(*

eICQ: the free ICQ for Microsoft(tm) Windows(tm)

Copyright 2003-2004 eICQ ICQ project,
all portions of this codebase are copyrighted to the people
listed in contributors.txt.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*)

unit SendSMS;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TSendSMSForm = class(TForm)
    btnSend: TButton;
    btnClose: TButton;
    memoSMSText: TMemo;
    Label1: TLabel;
    editPhone: TMemo;
    lblCouter: TLabel;
    procedure memoSMSTextChange(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnSendClick(Sender: TObject);
    procedure editPhoneKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure editPhoneKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
  protected
    procedure CreateParams(var Params : TCreateParams); override;
  public
    { Public declarations }
  end;

var
  SendSMSForm: TSendSMSForm;

implementation

uses
  Main;

{$R *.dfm}

procedure TSendSMSForm.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);       //Don't ever forget to do this!!!
  Params.WindowClass.hIcon := Icon.Handle;
  Params.WndParent := GetDesktopWindow;
end;

procedure TSendSMSForm.memoSMSTextChange(Sender: TObject);
begin
  lblCouter.Caption := Format('%u character(s)', [memoSMSText.GetTextLen]);
  if memoSMSText.GetTextLen > 0 then btnSend.Enabled := True
    else btnSend.Enabled := False;  
end;

procedure TSendSMSForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TSendSMSForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TSendSMSForm.btnSendClick(Sender: TObject);
begin
  if not MainForm.ICQClient1.LoggedIn then begin
    MessageBox(Self.Handle,'You cannot perform this action when you are offline.','Error',MB_OK);
    Exit;
  end;

  MainForm.ICQClient1.SendSMS(editPhone.Text, memoSMSText.Text);
  Close;
end;

procedure TSendSMSForm.editPhoneKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = [ssCtrl]) then
    editPhone.ReadOnly := True;
end;

procedure TSendSMSForm.editPhoneKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  editPhone.ReadOnly := False;
end;

end.
