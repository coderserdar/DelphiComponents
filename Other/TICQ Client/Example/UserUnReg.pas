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

unit UserUnReg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Passwd;

type
  TUserUnRegForm = class(TForm)
    PasswordEdit: TPasswordEdit;
    Label1: TLabel;
    btnContinue: TButton;
    procedure btnContinueClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UserUnRegForm: TUserUnRegForm;

implementation

{$R *.dfm}

uses
  Main;
  
procedure TUserUnRegForm.btnContinueClick(Sender: TObject);
begin
  if not MainForm.ICQClient1.LoggedIn then begin
    MessageBox(Self.Handle,'You cannot perform this action when you are offline.','Error',MB_OK);
    Exit;
  end;

  MainForm.ICQClient1.UnregisterUIN(PasswordEdit.Text);
  Close;
end;

procedure TUserUnRegForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  UserUnRegForm := nil;
end;

end.
