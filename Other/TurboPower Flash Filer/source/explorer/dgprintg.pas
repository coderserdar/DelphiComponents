{*********************************************************}
{* Print Status Dialog                                   *}
{*********************************************************}

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
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit dgprintg;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls;

type
  TdlgPrinting = class(TForm)
    Bevel1: TBevel;
    lblPrintingCaption: TLabel;
  private
    FCursor: TCursor;
  public
  end;

var
  dlgPrinting: TdlgPrinting;

procedure HidePrintingDlg;

procedure ShowPrintingDlg(const aCaption: string);

implementation

{$R *.DFM}

procedure HidePrintingDlg;
begin
  with dlgPrinting do begin
    Screen.Cursor := FCursor;
    Visible := False;
    dlgPrinting.Free;
    dlgPrinting := nil;
  end;

end;

procedure ShowPrintingDlg(const aCaption: string);
begin
  if not Assigned(dlgPrinting) then
    dlgPrinting := TdlgPrinting.Create(nil);
  with dlgPrinting do begin
    FCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
    lblPrintingCaption.Caption := aCaption;
    Visible := True;
  end;
end;


end.
