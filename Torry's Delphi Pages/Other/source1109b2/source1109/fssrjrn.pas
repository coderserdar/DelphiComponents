{*********************************************************}
{* Journal recovery for server                           *}
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

Unit fssrjrn;

{$I FsDEFINE.INC}

Interface

Uses
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Stdctrls,
  Buttons,
  ExtCtrls;

Type
  TJournalState = (jsComplete, jsIncomplete, jsTrash, jsSkipping);
  TfsJournalForm = Class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    OKBtn: TBitBtn;
    RollbackBtn: TBitBtn;
    PrintBtn: TBitBtn;
    CompletenessLabel: TLabel;
    ActionLabel: TLabel;
    ExceptionLabel: TLabel;
    FilenameLabel: TLabel;
    PathLabel: TLabel;
    AliasLabel: TLabel;
    Procedure PrintBtnClick(Sender: TObject);
  Private
    { Private declarations }
  Public
    { Public declarations }
    Procedure Setup(JournalState: TJournalState;
      Alias: String;
      Path: String;
      Filename: String;
      ExceptionString: String);
  End;

  {Begin !!.13}
Const
  ffc_JournalCompletenessMsgs: Array[TJournalState] Of String = (
    'Complete journal file found!',
    'Incomplete journal file found!',
    'Error processing journal file!',
    'Error processing file! Skipped the file.');

  ffc_JournalActionMsgs: Array[TJournalState] Of String = (
    'Recovery is possible!',
    'Recovery is impossible!',
    'Cannot recover!',
    'Cannot recover!');
  {End !!.13}

Var
  fsJournalForm: TfsJournalForm;

Function ShowJournalForm(JournalState: TJournalState;
  Alias: String;
  Path: String;
  Filename: String;
  ExceptionString: String): Integer;

Implementation

{$R *.DFM}

Procedure TfsJournalForm.Setup(JournalState: TJournalState;
  Alias: String;
  Path: String;
  Filename: String;
  ExceptionString: String);
Begin
  ExceptionLabel.Visible := False;
  {Begin !!.13}
  CompletenessLabel.Caption := ffc_JournalCompletenessMsgs[JournalState];
  ActionLabel.Caption := ffc_JournalActionMsgs[JournalState];
  {End !!.13}
  Case JournalState Of
    jsComplete:
      Begin
        OKBtn.Caption := '&Commit';
        RollbackBtn.Visible := True;
        CompletenessLabel.Font.Color := clGreen;
        ActionLabel.Font.Color := clGreen;
      End;
    jsIncomplete:
      Begin
        OKBtn.Caption := '&OK';
        RollbackBtn.Visible := False;
        CompletenessLabel.Font.Color := clRed;
        ActionLabel.Font.Color := clRed;
      End;
    jsTrash:
      Begin
        OKBtn.Caption := '&OK';
        RollbackBtn.Visible := False;
        CompletenessLabel.Font.Color := clRed;
        ActionLabel.Font.Color := clRed;
        ExceptionLabel.Visible := True;
        ExceptionLabel.Caption := ExceptionString;
      End;
    jsSkipping:
      Begin
        OKBtn.Caption := '&OK';
        RollbackBtn.Visible := False;
        CompletenessLabel.Font.Color := clRed;
        ActionLabel.Font.Color := clRed;
        ExceptionLabel.Visible := True;
        ExceptionLabel.Caption := ExceptionString;
      End;
  End;
  ActionLabel.Left := 0;
  ActionLabel.Width := Panel1.Width;
  CompletenessLabel.Left := 0;
  CompletenessLabel.Width := Panel1.Width;
  AliasLabel.Caption := Alias;
  PathLabel.Caption := Path;
  FilenameLabel.Caption := Filename;
End;

Function ShowJournalForm(JournalState: TJournalState;
  Alias: String;
  Path: String;
  Filename: String;
  ExceptionString: String): Integer;
Begin
  fsJournalForm := TfsJournalForm.Create(Application);
  Try
    fsJournalForm.Setup(JournalState, Alias, Path, Filename, ExceptionString);
    fsJournalForm.ShowModal;
    Result := fsJournalForm.ModalResult;
  Finally
    fsJournalForm.Free;
  End;
End;

Procedure TfsJournalForm.PrintBtnClick(Sender: TObject);
Begin
  Print;
End;

End.
