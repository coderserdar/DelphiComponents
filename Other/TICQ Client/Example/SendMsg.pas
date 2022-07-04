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

unit SendMsg;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  StdCtrls, SysUtils, ICQWorks, ComCtrls, Menus, ExtCtrls;

type
  TSendMsgForm = class(TForm)
    btnSend: TButton;
    RichEdit1: TRichEdit;
    LabelNoChar: TLabel;
    btnClose: TButton;
    Image1: TImage;
    lblUIN: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnSendClick(Sender: TObject);
    procedure RichEdit1Change(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
  protected
    procedure CreateParams(var Params : TCreateParams); override;
  public
    FDest, FName: String;
  end;

var
  SendMsgForm: TSendMsgForm;

implementation

uses
  Main;

{$R *.dfm}

procedure TSendMsgForm.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);       //Don't ever forget to do this!!!
  Params.WindowClass.hIcon := Icon.Handle;
  Params.WndParent := GetDesktopWindow;
end;

procedure TSendMsgForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TSendMsgForm.btnSendClick(Sender: TObject);
begin
  if not MainForm.ICQClient1.LoggedIn then
  begin
    MessageBox(Self.Handle, 'You cannot send messages when you are offline!', 'Error!', MB_OK);
    Exit;
  end;
  MainForm.ICQClient1.SendMessage(StrToInt64(FDest), RichEdit1.Text);
  MainForm.LogToFile(FDest, RichEdit1.Text, FName, True, MainForm.ConvertDateTime(Now));
  Close;
end;

procedure TSendMsgForm.RichEdit1Change(Sender: TObject);
begin
  if RichEdit1.GetTextLen > 0 then btnSend.Enabled := True
    else btnSend.Enabled := False;

  LabelNoChar.Caption := Format('Character(s): %u', [RichEdit1.GetTextLen]);
end;

procedure TSendMsgForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TSendMsgForm.FormResize(Sender: TObject);
begin
  btnSend.Left := Self.Width div 2 - btnSend.Width - 8;
  btnClose.Left := Self.Width div 2 + 8;
end;

procedure TSendMsgForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;

  if (Key = VK_RETURN) and (Shift = [ssCtrl]) then
    if btnSend.Enabled then btnSend.Click;
end;

end.
