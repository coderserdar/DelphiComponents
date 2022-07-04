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

unit UserRegNew;

interface

uses
  Windows, Messages, Classes, Controls, Forms,
  StdCtrls, Passwd;

type
  TUserRegNewForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TPasswordEdit;
    Edit2: TPasswordEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UserRegNewForm: TUserRegNewForm;

implementation

{$R *.dfm}

uses
  Main;

procedure TUserRegNewForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  UserRegNewForm := nil;
end;

procedure TUserRegNewForm.Button1Click(Sender: TObject);
begin
  if Edit1.Text <> Edit2.Text then
  begin
    MessageBox(0, 'Please check your passwords!', 'Error!', MB_OK);
    Exit;
  end;
  
  MainForm.ICQClient1.RegisterNewUIN(Edit1.Text);
  MainForm.StatusBar1.Panels.Items[0].Text := 'Registering a new UIN';

  Close;
end;

end.
