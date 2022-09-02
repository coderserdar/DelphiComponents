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


unit uFFSRJrn;

{$I FFDEFINE.INC}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TJournalState = ( jsComplete, jsIncomplete, jsTrash, jsSkipping );
  TJournalForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Bevel1: TBevel;
    OKBtn: TBitBtn;
    RollbackBtn: TBitBtn;
    PrintBtn: TBitBtn;
    CompletenessLabel: TLabel;
    ActionLabel: TLabel;
    ExceptionLabel: TLabel;
    FilenameLabel: TLabel;
    PathLabel: TLabel;
    AliasLabel: TLabel;
    procedure PrintBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Setup( JournalState : TJournalState;
                     Alias : String;
                     Path : String;
                     Filename : String;
                     ExceptionString : String );
  end;

{Begin !!.13}
const
  ffc_JournalCompletenessMsgs : array[TJournalState] of string = (
    'Complete journal file found!',
    'Incomplete journal file found!',
    'Error processing journal file!',
    'Error processing file! Skipped the file.' );

  ffc_JournalActionMsgs : array[TJournalState] of string = (
    'Recovery is possible!',
    'Recovery is impossible!',
    'Cannot recover!',
    'Cannot recover!' );
{End !!.13}


var
  JournalForm: TJournalForm;

  function ShowJournalForm( JournalState : TJournalState;
                            Alias : String;
                            Path : String;
                            Filename : String;
                            ExceptionString : String ) : Integer;

implementation

{$R *.DFM}

procedure TJournalForm.Setup( JournalState : TJournalState;
                              Alias : String;
                              Path : String;
                              Filename : String;
                              ExceptionString : String );
begin
  ExceptionLabel.Visible := False;
{Begin !!.13}
  CompletenessLabel.Caption := ffc_JournalCompletenessMsgs[JournalState];
  ActionLabel.Caption := ffc_JournalActionMsgs[JournalState];
{End !!.13}
  case JournalState of
    jsComplete :
    begin
      OKBtn.Caption := '&Commit';
      RollbackBtn.Visible := True;
      CompletenessLabel.Font.Color := clGreen;
      ActionLabel.Font.Color := clGreen;
    end;
    jsIncomplete :
    begin
      OKBtn.Caption := '&OK';
      RollbackBtn.Visible := False;
      CompletenessLabel.Font.Color := clRed;
      ActionLabel.Font.Color := clRed;
    end;
    jsTrash :
    begin
      OKBtn.Caption := '&OK';
      RollbackBtn.Visible := False;
      CompletenessLabel.Font.Color := clRed;
      ActionLabel.Font.Color := clRed;
      ExceptionLabel.Visible := True;
      ExceptionLabel.Caption := ExceptionString;
    end;
    jsSkipping :
    begin
      OKBtn.Caption := '&OK';
      RollbackBtn.Visible := False;
      CompletenessLabel.Font.Color := clRed;
      ActionLabel.Font.Color := clRed;
      ExceptionLabel.Visible := True;
      ExceptionLabel.Caption := ExceptionString;
    end;
  end;
  ActionLabel.Left := 0;
  ActionLabel.Width := Bevel1.Width;
  CompletenessLabel.Left := 0;
  CompletenessLabel.Width := Bevel1.Width;
  AliasLabel.Caption := Alias;
  PathLabel.Caption := Path;
  FilenameLabel.Caption := Filename;
end;

function ShowJournalForm( JournalState : TJournalState;
                          Alias : String;
                          Path : String;
                          Filename : String;
                          ExceptionString : String ) : Integer;
begin
  JournalForm := TJournalForm.Create( Application );
  try
    JournalForm.Setup( JournalState, Alias, Path, Filename, ExceptionString );
    JournalForm.ShowModal;
    Result := JournalForm.ModalResult;
  finally
    JournalForm.Free;
  end;
end;

procedure TJournalForm.PrintBtnClick(Sender: TObject);
begin
  Print;
end;

end.
