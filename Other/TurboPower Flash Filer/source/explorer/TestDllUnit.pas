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
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * Eivind Bakkestuen
 * Used with permission.
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit TestDllUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ffllbase, ffdbbase, ffdb;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  uReportEngineInterface,
  ffllprot;

procedure TForm1.Button1Click(Sender: TObject);
var
  i : Integer;
  rs,
  re : TRangeFieldValues;
begin
  if ReportEngineDLLLoaded then begin
    for i := 0 to 15 do
      rs[i] := NULL;
    rs[0] := 'F';
    for i := 0 to 15 do
      re[i] := NULL;
    re[0] := 'M'+#255;
    SingleTableReport(ptTCPIP, '192.168.0.28', '', '', 'aflforwin', 'kunde', '', 'kundnavnIdx', rs, re);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if ReportEngineDLLLoaded then begin
    SingleQueryReport(ptTCPIP, '192.168.0.28', '', '', 'aflforwin', 'select * from sjafor', '');
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if ReportEngineDLLLoaded then begin
    DesignReport(ptTCPIP, '192.168.0.28', '', '', 'aflforwin');
  end;
end;

end.
