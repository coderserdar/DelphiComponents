
(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Essentials Vol I
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit ExCCbxU;

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, EsClrCbx;

type
  TForm1 = class(TForm)
    EsColorComboBox1: TEsColorComboBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure EsColorComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.EsColorComboBox1Change(Sender: TObject);
begin
  Label2.Color := EsColorComboBox1.SelectedColor;
end;

end.
