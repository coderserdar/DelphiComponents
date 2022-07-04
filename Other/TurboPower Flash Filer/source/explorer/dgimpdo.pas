{*********************************************************}
{* Progress meter for import operations                  *}
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

unit dgimpdo;

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
  ComCtrls,
  StdCtrls,
  Buttons,
  ExtCtrls,
  ffclimex,
  ffllbase,
  uentity;

type
  TdlgImportProgress = class(TForm)
    lblProgress: TLabel;
    btnCancel: TBitBtn;
    lblFrom: TLabel;
    lblTo: TLabel;
    edtImportFilename: TEdit;
    edtTablename: TEdit;
    mtrProgress: TProgressBar;
    procedure btnCancelClick(Sender: TObject);
  private
  public
    FEngine: TffImportEngine;
    procedure ShowProgress(aImportFilename, aTableName: string);
    procedure UpdateProgress(aProgress: TffieProgressPacket);
  end;

function DoImport(aIE: TffImportEngine;
                  aImportFilename: TFilename;
                  aTableName: TffTableName;
                  aTable: TffexpTable;
                  aBlockInserts: SmallInt): Boolean;

var
  dlgImportProgress: TdlgImportProgress;

implementation

{$R *.DFM}

function DoImport(aIE: TffImportEngine;
                  aImportFilename: TFilename;
                  aTableName: TffTableName;
                  aTable: TffexpTable;
                  aBlockInserts: SmallInt): Boolean;
begin
  with TdlgImportProgress.Create(nil) do
    try                                                            {start !!.01}
      FEngine := aIE;
      ShowProgress(aImportFilename, aTableName);
      try
        FEngine.OnYield := UpdateProgress;
        FEngine.Import(aTable, aBlockInserts);
      finally
        Hide;
      end;
      Application.ProcessMessages;
      Result := not FEngine.Terminated;
    finally
      Free;
    end;                                                             {end !!.01}
end;

procedure TdlgImportProgress.ShowProgress(aImportFilename, aTableName: string);
begin
  edtImportFilename.Text := aImportFilename;
  edtTablename.Text := aTableName;
  lblProgress.Hide;
  mtrProgress.Position := 0;
  inherited Show;
  Application.ProcessMessages;
end;

procedure TdlgImportProgress.UpdateProgress(aProgress: TffieProgressPacket);
var
  Dividend: LongInt;
  Divisor: LongInt;
begin
  with aProgress do begin
    with lblProgress do begin
      Caption := Format('Processing record %d of %d', [ppNumRecs, ppTotalRecs]);
      Show;
    end;

    { Calculate % completed }
    if (ppNumRecs >= $1000000) then begin
      Dividend := (ppNumRecs shr 7) * 100;
      Divisor := ppTotalRecs shr 7;
    end
    else begin
      Dividend := ppNumRecs * 100;
      Divisor := ppTotalRecs;
    end;

    if Divisor <> 0 then
      mtrProgress.Position := Dividend div Divisor;

    if IsIconic(Application.Handle) then
      Application.Title := Format('Importing %d%% complete', [mtrProgress.Position]);
  end;
end;

procedure TdlgImportProgress.btnCancelClick(Sender: TObject);
begin
  if MessageDlg('Abort importing data?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    FEngine.Terminate;
end;

end.
