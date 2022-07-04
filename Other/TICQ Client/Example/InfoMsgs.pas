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

unit InfoMsgs;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ReasonFrm;

type
  TInfoMsgsForm = class(TForm)
    sbUserInfo: TSpeedButton;
    sbAddUser: TSpeedButton;
    btnClose: TButton;
    labMsg: TLabel;
    btnDeny: TButton;
    btnAccept: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCloseClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure btnDenyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbAddUserClick(Sender: TObject);
    procedure sbUserInfoClick(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure CreateParams(var Params : TCreateParams); override;
  public
    { Public declarations }
    FSource: DWORD;
    FReason: String;
    FHandle: HWND;
  end;

var
  InfoMsgsForm: TInfoMsgsForm;

implementation

uses
  Main, AddUser;

{$R *.dfm}

procedure TInfoMsgsForm.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params); //Don't ever forget to do this!!!
  Params.WndParent := GetDesktopWindow;
end;

procedure TInfoMsgsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TInfoMsgsForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TInfoMsgsForm.btnAcceptClick(Sender: TObject);
begin
  if MainForm.ICQClient1.LoggedIn then
    MainForm.ICQClient1.SendAuthResponse(FSource, True, '');
  Close;
end;

procedure TInfoMsgsForm.btnDenyClick(Sender: TObject);
begin
  with TReasonForm.Create(Self) do
  begin
    RSource := FSource;
    RHandle := FHandle;
    Left := Self.Left + Self.Width div 2 - Width div 2;
    Top := Self.Top + Self.Height div 2 - Height div 2;
    EnableWindow(Self.Handle, False);
    ShowWindow(Self.Handle, SW_SHOWNORMAL);
    Show;
  end;
end;

procedure TInfoMsgsForm.FormCreate(Sender: TObject);
begin
  FHandle := Handle;
end;

procedure TInfoMsgsForm.sbAddUserClick(Sender: TObject);
begin
  with TAddUserForm.Create(Self) do
  begin
    FUIN := FSource;
    Caption := Format('Add Contact (%u)', [FSource]);
    dlgHandle := Self.Handle;
    EnableWindow(Self.Handle, False);
    ShowWindow(Self.Handle, SW_SHOWNORMAL);
    Show;
  end;
end;

procedure TInfoMsgsForm.sbUserInfoClick(Sender: TObject);
begin
  MainForm.DoCreateInfoQuery(IntToStr(FSource));
end;

end.

