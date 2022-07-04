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

unit RecvURL;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SendURL;

type
  TRecvURLForm = class(TForm)
    Image1: TImage;
    lblUIN: TLabel;
    Label1: TLabel;
    EditURL: TMemo;
    lblDescription: TLabel;
    memoURLDscrb: TMemo;
    btnReply: TButton;
    btnCancel: TButton;
    lblDateTime: TLabel;
    btnOpenURL: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnReplyClick(Sender: TObject);
    procedure btnOpenURLClick(Sender: TObject);
  private
  protected
    procedure CreateParams(var Params : TCreateParams); override;
  public
    FSource, FNick, FDateTime: String;
  end;

var
  RecvURLForm: TRecvURLForm;

implementation

uses Main;

{$R *.dfm}

procedure TRecvURLForm.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);       //Don't ever forget to do this!!!
  Params.WindowClass.hIcon := Icon.Handle;
  Params.WndParent := GetDesktopWindow;
end;

procedure TRecvURLForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TRecvURLForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TRecvURLForm.btnReplyClick(Sender: TObject);
begin
  with TSendURLForm.Create(nil) do
  begin
    FDest := FSource;
    FName := FNick;
    Caption := Format('Send URL to %s (%s)', [FName, FDest]);
    MainForm.AdvIconList.GetIcon(7, Image1.Picture.Icon);
    lblUIN.Caption := FDest;
    Show;
  end;
  Close;
end;

procedure TRecvURLForm.btnOpenURLClick(Sender: TObject);
begin
  MainForm.OpenURL(EditURL.Text, False); 
end;

end.
