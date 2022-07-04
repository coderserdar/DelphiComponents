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

unit ReasonFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TReasonForm = class(TForm)
    btnReasonOK: TButton;
    memoReason: TMemo;
    procedure btnReasonOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    RSource: DWORD;
    RHandle: HWND;
  end;

var
  ReasonForm: TReasonForm;

implementation

uses
  Main, InfoMsgs;

{$R *.dfm}

procedure TReasonForm.btnReasonOKClick(Sender: TObject);
begin
  if not MainForm.ICQClient1.LoggedIn then begin
    MessageBox(Self.Handle,'You cannot perform this action when you are offline.','Error',MB_OK);
    Exit;
  end;

  MainForm.ICQClient1.SendAuthResponse(RSource, False, memoReason.Text);
  Close;
end;

procedure TReasonForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  EnableWindow(RHandle, True);          //API ruleZZZ!!!
  SendMessage(RHandle, WM_CLOSE, 0, 0);
  DestroyWindow(RHandle);
  Action := caFree;
end;

end.
