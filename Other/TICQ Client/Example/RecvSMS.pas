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

unit RecvSMS;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TRecvSMSForm = class(TForm)
    Label1: TLabel;
    editPhone: TMemo;
    memoSMSText: TMemo;
    btnClose: TButton;
    lblDateTime: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  protected
    procedure CreateParams(var Params : TCreateParams); override;
  public
    { Public declarations }
  end;

var
  RecvSMSForm: TRecvSMSForm;

implementation

{$R *.dfm}

procedure TRecvSMSForm.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);       //Don't ever forget to do this!!!
  Params.WindowClass.hIcon := Icon.Handle;
  Params.WndParent := GetDesktopWindow;
end;

procedure TRecvSMSForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TRecvSMSForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
