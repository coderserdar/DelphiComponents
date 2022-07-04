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

unit RecvMsg;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  StdCtrls, ComCtrls, SysUtils, SendMsg, ExtCtrls, Richedit;

type
  TRecvMsgForm = class(TForm)
    btnReply: TButton;
    btnClose: TButton;
    RichEdit1: TRichEdit;
    lblDateTime: TLabel;
    lblUIN: TLabel;
    Image1: TImage;
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnReplyClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  protected
    procedure CreateParams(var Params : TCreateParams); override;
    procedure WndProc(var Message: TMessage); override;
  public
    FSource, FNick, FDateTime: String;
  end;

var
  RecvMsgForm: TRecvMsgForm;

implementation

uses
  Main;

{$R *.dfm}

procedure TRecvMsgForm.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params); //Don't ever forget to do this!!!
  Params.WindowClass.hIcon := Icon.Handle;  
  Params.WndParent := GetDesktopWindow;
end;

procedure TRecvMsgForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TRecvMsgForm.btnReplyClick(Sender: TObject);
begin
  with TSendMsgForm.Create(nil) do
  begin
    FDest := FSource;
    FName := FNick;
    Caption := Format('Send message to %s (%s)', [FName, FDest]);
    MainForm.AdvIconList.GetIcon(7, Image1.Picture.Icon);
    lblUIN.Caption := FDest;
    Show;
  end;
  Close;
end;

procedure TRecvMsgForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TRecvMsgForm.FormResize(Sender: TObject);
begin
  btnReply.Left := Self.Width div 2 - btnReply.Width - 8;
  btnClose.Left := Self.Width div 2 + 8;
end;

procedure TRecvMsgForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

procedure TRecvMsgForm.FormShow(Sender: TObject);
begin
  MainForm.LogToFile(FSource, RichEdit1.Text, FNick, False, FDateTime);
end;

procedure TRecvMsgForm.FormCreate(Sender: TObject);
var
  mask: Word;
begin
  mask := SendMessage(Handle, EM_GETEVENTMASK, 0, 0);
  SendMessage(RichEdit1.Handle, EM_SETEVENTMASK, 0, mask or ENM_LINK);
  SendMessage(RichEdit1.Handle, EM_AUTOURLDETECT, Integer(True), 0);
end;

procedure TRecvMsgForm.WndProc(var Message: TMessage);
var
  p: TENLink;
  strURL: string;
begin
  if (Message.Msg = WM_NOTIFY) then
  begin
    if (PNMHDR(Message.lParam).code = EN_LINK) then
    begin
      p := TENLink(Pointer(TWMNotify(Message).NMHdr)^);
      if (p.Msg = WM_LBUTTONDOWN) then
      begin
        SendMessage(RichEdit1.Handle, EM_EXSETSEL, 0, Longint(@(p.chrg)));
        strURL := RichEdit1.SelText;
        MainForm.OpenURL(PChar(strURL), False);
      end
    end
  end;
  inherited;
end;

end.
