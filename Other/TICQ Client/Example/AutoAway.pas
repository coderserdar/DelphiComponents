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

unit AutoAway;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, StdCtrls;

type
  TAutoAwayForm = class(TForm)
    AwayMemo: TMemo;
    Button1: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
  private
  protected
    procedure CreateParams(var Params : TCreateParams); override;
  public
    { Public declarations }
  end;

var
  AutoAwayForm: TAutoAwayForm;

implementation

{$R *.dfm}

procedure TAutoAwayForm.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params); //Don't ever forget to do this!!!
  Params.WindowClass.hIcon := Icon.Handle;  
  Params.WndParent := GetDesktopWindow;
end;

procedure TAutoAwayForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TAutoAwayForm.Button1Click(Sender: TObject);
begin
  Close;
end;

end.
