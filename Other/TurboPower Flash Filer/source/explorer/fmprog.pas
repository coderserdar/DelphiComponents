{*********************************************************}
{* Progress meter for rebuild operations                 *}
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

unit fmprog;

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
  ExtCtrls,
  ffllbase;

type
  TfrmRebuildStatus = class(TForm)
    lblProgress: TLabel;
    mtrPercentComplete: TProgressBar;
  private
    FCursor: TCursor;
  public
    procedure Hide;
    procedure ShowProgress(aAction, aTableName: string);
    procedure UpdateProgress(aCompleted: Boolean; aStatus: TffRebuildStatus);
  end;

var
  frmRebuildStatus: TfrmRebuildStatus;

implementation

{$R *.DFM}

procedure TfrmRebuildStatus.Hide;
begin
  Screen.Cursor := FCursor;
  inherited Hide;
end;

procedure TfrmRebuildStatus.ShowProgress(aAction, aTableName: string);
begin
  Caption := Format('%s Table %s', [aAction, aTableName]);
  lblProgress.Hide;
  FCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  mtrPercentComplete.Position := 0;
  inherited Show;
end;

procedure TfrmRebuildStatus.UpdateProgress(aCompleted: Boolean; aStatus: TffRebuildStatus);
begin
  with aStatus do begin
  if rsErrorCode <> 0 then
    ShowMessage(Format('%s', [rsErrorCode]));
    with lblProgress do begin
      Caption := Format('Processing record %d of %d', [rsRecsRead, rsTotalRecs]);
      Show;
      Application.ProcessMessages;
    end;
    mtrPercentComplete.Position := aStatus.rsPercentDone;
  end;
end;

end.
