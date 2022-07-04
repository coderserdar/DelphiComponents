{**************************************************************************************************}
{                                                                                                  }
{ Perforce for Delphi plugin (P4Delphi)                                                            }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Chris Fairall. Portions created by                 }
{ Chris Fairall are Copyright (C) Chris Fairall (cfairall at bigpond dot net dot au)               }
{ All rights reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit FormLogin;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls;

type
  TFrmLogin = class(TForm)
    EdtPassword: TEdit;
    LblPassword: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    ChkRemember: TCheckBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  UnitP4Engine;

{$R *.dfm}

procedure TFrmLogin.FormCreate(Sender: TObject);
var
  S: string;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := P4Engine.Command('set');

    S := SL.Values['p4user'];
    if Pos(#32, S) > 0 then
      Delete(S, Pos(#32, S), Length(S));

    LblPassword.Caption := 'Enter password for user ' + S;
  finally
    SL.Free;
  end;
end;

end.
