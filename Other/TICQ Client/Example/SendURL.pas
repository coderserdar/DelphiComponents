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

unit SendURL;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TSendURLForm = class(TForm)
    memoURLDscrb: TMemo;
    lblDescription: TLabel;
    Label1: TLabel;
    btnSend: TButton;
    btnCancel: TButton;
    Image1: TImage;
    lblUIN: TLabel;
    EditURL: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
    procedure EditURLChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnSendClick(Sender: TObject);
    procedure EditURLKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditURLKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
  protected
    procedure CreateParams(var Params : TCreateParams); override;
  public
    FDest, FName: String;
  end;

var
  SendURLForm: TSendURLForm;

implementation

uses
  Main;

{$R *.dfm}

procedure TSendURLForm.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);       //Don't ever forget to do this!!!
  Params.WindowClass.hIcon := Icon.Handle;
  Params.WndParent := GetDesktopWindow;
end;

procedure TSendURLForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TSendURLForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TSendURLForm.EditURLChange(Sender: TObject);
begin
  if EditURL.GetTextLen > 0 then btnSend.Enabled := True
    else btnSend.Enabled := False;
end;

procedure TSendURLForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;

  if (Key = VK_RETURN) and (Shift = [ssCtrl]) then
    if btnSend.Enabled then btnSend.Click;
end;

procedure TSendURLForm.btnSendClick(Sender: TObject);
begin
  if not MainForm.ICQClient1.LoggedIn then begin
    MessageBox(Self.Handle,'You cannot perform this action when you are offline.','Error',MB_OK);
    Exit;
  end;

  MainForm.ICQClient1.SendURL(StrToInt64(FDest), EditURL.Text, memoURLDscrb.Text);
  Close;
end;

procedure TSendURLForm.EditURLKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = [ssCtrl]) then
    EditURL.ReadOnly := True;
end;

procedure TSendURLForm.EditURLKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  EditURL.ReadOnly := False;
end;

end.
