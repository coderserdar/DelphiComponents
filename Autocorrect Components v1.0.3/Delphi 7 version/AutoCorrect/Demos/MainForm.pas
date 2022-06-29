{**************************************************************************************}
{                                                                                      }
{ AutoCorrect components for Delphi 7                                                  }
{ Version 1.0 (2009-07-24)                                                             }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is MainForm.pas.                                                   }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009 Chris Rolliston. All Rights Reserved.         }
{                                                                                      }
{**************************************************************************************}

unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, StdActns, ExtActns, ActnList, ToolWin, ImgList, Menus,
  CCR.AutoCorrect, CCR.AutoCorrect.Controls;

type
  TRichEdit = class(TRichEditWithAutoCorrect); //get the bug fixes with an interposer class

  TfrmMain = class(TForm)
    ActionList: TActionList;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    actSelectAll: TAction;
    actUndo: TAction;
    RichEditBold1: TRichEditBold;
    RichEditItalic1: TRichEditItalic;
    RichEditUnderline1: TRichEditUnderline;
    RichEditStrikeOut1: TRichEditStrikeOut;
    RichEditBullets1: TRichEditBullets;
    RichEditAlignLeft1: TRichEditAlignLeft;
    RichEditAlignRight1: TRichEditAlignRight;
    RichEditAlignCenter1: TRichEditAlignCenter;
    ImageList: TImageList;
    tbrActions: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    DisabledImageList: TImageList;
    ToolButton14: TToolButton;
    actDecreaseIndent: TAction;
    actIncreaseIndent: TAction;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    Panel1: TPanel;
    grpCombo: TGroupBox;
    ComboBox: TComboBox;
    Bevel1: TBevel;
    grpEdit: TGroupBox;
    Edit: TEdit;
    Bevel2: TBevel;
    grpMemo: TGroupBox;
    Memo: TMemo;
    Splitter1: TSplitter;
    grpRichEdit: TGroupBox;
    RichEdit: TRichEdit;
    Splitter2: TSplitter;
    GroupBox1: TGroupBox;
    chkAutoIndent: TCheckBox;
    chkConvertHyphens: TCheckBox;
    chkTwoInitialCaps: TCheckBox;
    chkCurlQuotes: TCheckBox;
    chkPreferFractionChars: TCheckBox;
    Bevel4: TBevel;
    chkAutoAddTwoCapsExceptions: TCheckBox;
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    redLog: TRichEdit;
    TabSheet2: TTabSheet;
    redLoadedEntries: TRichEdit;
    mnuLog: TPopupMenu;
    Copy1: TMenuItem;
    SelectAll1: TMenuItem;
    N1: TMenuItem;
    itmClearLog: TMenuItem;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    mnuRichEdit: TPopupMenu;
    Undo1: TMenuItem;
    N2: TMenuItem;
    Copy2: TMenuItem;
    Copy3: TMenuItem;
    Paste1: TMenuItem;
    N3: TMenuItem;
    SelectAll2: TMenuItem;
    itmLoadFromFile: TMenuItem;
    itmSaveToFile: TMenuItem;
    N4: TMenuItem;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure actDecreaseIndentExecute(Sender: TObject);
    procedure actIncreaseIndentExecute(Sender: TObject);
    procedure actIndentUpdate(Sender: TObject);
    procedure CorrectionMade(Sender: TAutoCorrectEngine;
      CorrectionKind: TAutoCorrectionKind; Control: TWinControl;
      Entry: TAutoCorrectEntry);
    procedure UndoTwoInitialCapsCorrection(Sender: TAutoCorrectEngine;
      const Word: string; var AddToIgnoredList: Boolean);
    procedure redLogChange(Sender: TObject);
    procedure chkOptionClick(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actCutUpdate(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCopyUpdate(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actPasteUpdate(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSelectAllUpdate(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actUndoUpdate(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure itmClearLogClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure itmLoadFromFileClick(Sender: TObject);
    procedure itmSaveToFileClick(Sender: TObject);
  private
    AutoCorrectEngine: TAutoCorrectEngine;
  end;

var
  frmMain: TfrmMain;

implementation

uses ClipBrd, IniFiles, StrUtils;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
const
  ReadmePath = '..\Readme.rtf';
begin
  Application.Title := Caption;
  chkAutoIndent.Tag := Ord(acAutoIndent);
  chkConvertHyphens.Tag := Ord(acConvertHyphensToDashes);
  chkTwoInitialCaps.Tag := Ord(acCorrectTwoInitialCaps);
  chkCurlQuotes.Tag := Ord(acCurlQuoteChars);
  chkPreferFractionChars.Tag := Ord(acPreferFractionChars);
  //create and initialise the AutoCorrect engine component
  AutoCorrectEngine := TAutoCorrectEngine.Create(Self);
  AutoCorrectEngine.OnCorrectionMade := CorrectionMade;
  AutoCorrectEngine.OnUndoTwoInitialCapsCorrection := UndoTwoInitialCapsCorrection;
  //add some default custom entries - normally you'd do this at design time
  with AutoCorrectEngine.CustomEntries do
  begin
    Add('teh', 'the');
    Add('Borland', 'Embarcadero');
    Add('On Liberty').StylesToAdd := [fsItalic];
    Add('red').FontColorToSet := clRed;
  end;
  //add some default TWo INitial CApitals exceptions - again, normally this would be done at design time
  with AutoCorrectEngine.IgnoredTwoInitialCaps do
  begin
    Add('TEdit');
    Add('TMemo');
  end;
  //add lots more entries from an INI file, assuming it hasn't been deleted
  AutoCorrectEngine.LoadFromIniFile(ExtractFilePath(Application.ExeName) +
    'AutoCorrect.ini', lbKeepExisting);
  //load the readme into the rich edit
  try
    RichEdit.Lines.LoadFromFile(ReadmePath);
  except
    on EFOpenError do {probably been deleted or is currently in use} else raise;
  end;
end;

{ Code to globally hook up any supported control - note KeyPreview has been set to True
  for this to work. Aside from these two handlers, the final needed piece is the second
  line of actUndoExecute below. }

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  AutoCorrectEngine.KeyDownOccurred(ActiveControl, Key, Shift);
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  AutoCorrectEngine.KeyPressOccurred(ActiveControl, Key);
end;

{ general event handlers }

procedure TfrmMain.PageControlChanging(Sender: TObject; var AllowChange: Boolean);
var
  Entry: TAutoCorrectEntry;
  I: Integer;
  SavedPos: Integer;
  NewPara: string;
begin
  if redLoadedEntries.Lines.Count <> 0 then Exit;
  { Rich edit v1 has CRLF as the paragraph marker where v2+ has just CR; D2009's
    TCustomRichEdit use v3, earlier Delphis' TCustomRichEdit only v1. }
  {$IFDEF UNICODE}
  NewPara := #13;
  {$ELSE}
  NewPara := #13#10;
  {$ENDIF}
  redLoadedEntries.Lines.BeginUpdate;
  try
    Screen.Cursor := crHourGlass;
    redLoadedEntries.SelText := Format('%d custom AutoCorrect entries defined in all%s',
      [AutoCorrectEngine.CustomEntries.Count, NewPara]);
    redLoadedEntries.Paragraph.Numbering := nsBullet;
    for I := 0 to AutoCorrectEngine.CustomEntries.Count - 1 do
    begin
      Entry := AutoCorrectEngine.CustomEntries[I];
      redLoadedEntries.SelText := Format('%s to %s%s', [Entry.FindText,
        Entry.ReplaceText, NewPara]);
      if Entry.HasFormatting then
      begin
        SavedPos := redLoadedEntries.SelStart;
        redLoadedEntries.SelStart := SavedPos - Length(NewPara) - Length(Entry.ReplaceText);
        redLoadedEntries.SelLength := Length(Entry.ReplaceText);
        Entry.ApplyFormatting(redLoadedEntries);
        redLoadedEntries.SelStart := SavedPos;
      end;
    end;
    redLoadedEntries.Paragraph.Numbering := nsNone;
    redLoadedEntries.SelStart := 0;
    redLoadedEntries.Perform(EM_SCROLLCARET, 0, 0)
  finally
    redLoadedEntries.Lines.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.chkOptionClick(Sender: TObject);
var
  Option: TAutoCorrectOption;
begin
  Option := TAutoCorrectOption((Sender as TCheckBox).Tag);
  if TCheckBox(Sender).Checked then
    AutoCorrectEngine.Options := AutoCorrectEngine.Options + [Option]
  else
    AutoCorrectEngine.Options := AutoCorrectEngine.Options - [Option]
end;

procedure TfrmMain.itmLoadFromFileClick(Sender: TObject);
begin
  if dlgOpen.Execute then RichEdit.Lines.LoadFromFile(dlgOpen.FileName);
end;

procedure TfrmMain.itmSaveToFileClick(Sender: TObject);
begin
  if dlgSave.Execute then RichEdit.Lines.SaveToFile(dlgSave.FileName);
end;

{ correction logging }

procedure TfrmMain.CorrectionMade(Sender: TAutoCorrectEngine;
  CorrectionKind: TAutoCorrectionKind; Control: TWinControl; Entry: TAutoCorrectEntry);
const
  SCorrectionKinds: array[TAutoCorrectionKind] of string = ('indentation',
    'hyphen(s) to dash', 'TWo INitial CApitals', 'curl quote key',
    'fraction character',
    'custom entry');
  SWithOrWithoutFormatting: array[Boolean] of string = ('', ' with formatting');
var
  S: string;
begin
  FmtStr(S, '%s: %s', [Control.Name, SCorrectionKinds[CorrectionKind]]);
  if CorrectionKind = ckIndentation then
    with (Control as TCustomRichEdit).Paragraph do
      S := Format('%s (FirstIndent now %d, LeftIndent now %d)', [S, FirstIndent, LeftIndent])
  else
    S := Format('%s (%s to %s%s)', [S, Entry.FindText, Entry.ReplaceText,
      SWithOrWithoutFormatting[Entry.HasFormatting]]);
  redLog.Lines.Add(S);
end;

procedure TfrmMain.UndoTwoInitialCapsCorrection(Sender: TAutoCorrectEngine;
  const Word: string; var AddToIgnoredList: Boolean);
var
  S: string;
begin
  S := 'TWo INitial CApitals correction undone';
  if chkAutoAddTwoCapsExceptions.Checked then
    S := Format('%s: %s added to exception list', [S, Word])
  else
    AddToIgnoredList := False;
  redLog.Lines.Add(S);
end;

procedure TfrmMain.redLogChange(Sender: TObject);
begin
  redLog.Perform(EM_SCROLLCARET, 0, 0)
end;

procedure TfrmMain.itmClearLogClick(Sender: TObject);
begin
  redLog.Clear;
end;

{ Action code, much of which is boiler plate stuff working round the lack of
  polymorphism between TCustomEdit and TCustomCombo at the VCL level - you may have
  noticed that the standard TEditAction-derived classes don't work with combo boxes... }

type
  TCustomComboAccess = class(TCustomCombo);
  TCustomEditAccess = class(TCustomEdit);

function GetActiveEditWnd: HWND;
begin
  if Screen.ActiveControl is TCustomEdit then
    Result := TCustomEdit(Screen.ActiveControl).Handle
  else if Screen.ActiveControl is TCustomCombo then
    Result := TCustomComboAccess(Screen.ActiveControl).EditHandle
  else
    Result := 0;
end;

procedure TfrmMain.actCopyExecute(Sender: TObject);
begin
  ActiveControl.Perform(WM_COPY, 0, 0)
end;

procedure TfrmMain.actCopyUpdate(Sender: TObject);
begin
  if ActiveControl is TCustomEdit then
    actCopy.Enabled := (TCustomEdit(ActiveControl).SelLength > 0)
  else if ActiveControl is TComboBox then
    actCopy.Enabled := (TCustomCombo(ActiveControl).SelLength > 0)
  else
    actCopy.Enabled := False;
end;

procedure TfrmMain.actCutExecute(Sender: TObject);
begin
  ActiveControl.Perform(WM_CUT, 0, 0)
end;

procedure TfrmMain.actCutUpdate(Sender: TObject);
begin
  if ActiveControl is TCustomEdit then
    actCut.Enabled := (TCustomEdit(ActiveControl).SelLength > 0) and
      not TCustomEditAccess(ActiveControl).ReadOnly
  else if ActiveControl is TComboBox then
    actCut.Enabled := (TCustomCombo(ActiveControl).SelLength > 0)
  else
    actCut.Enabled := False;
end;

procedure TfrmMain.actDecreaseIndentExecute(Sender: TObject);
begin
  OffsetLeftIndent((ActiveControl as TCustomRichEdit), -AutoCorrectEngine.AutoIndentSize);
end;

procedure TfrmMain.actIncreaseIndentExecute(Sender: TObject);
begin
  OffsetLeftIndent((ActiveControl as TCustomRichEdit), AutoCorrectEngine.AutoIndentSize);
end;

procedure TfrmMain.actIndentUpdate(Sender: TObject);
begin
  (Sender as TCustomAction).Enabled := (ActiveControl is TCustomRichEdit) and
    not TCustomEditAccess(ActiveControl).ReadOnly;
end;

procedure TfrmMain.actPasteExecute(Sender: TObject);
begin
  ActiveControl.Perform(WM_PASTE, 0, 0)
end;

procedure TfrmMain.actPasteUpdate(Sender: TObject);
begin
  actPaste.Enabled := (ActiveControl is TComboBox) or
    ((ActiveControl is TCustomEdit) and not TCustomEditAccess(ActiveControl).ReadOnly);
end;

procedure TfrmMain.actSelectAllExecute(Sender: TObject);
begin
  SendMessage(GetActiveEditWnd, EM_SETSEL, 0, -1);
end;

procedure TfrmMain.actSelectAllUpdate(Sender: TObject);
begin
  actSelectAll.Enabled := (GetActiveEditWnd <> 0);
end;

procedure TfrmMain.actUndoExecute(Sender: TObject);
begin
  SendMessage(GetActiveEditWnd, WM_UNDO, 0, 0);
  AutoCorrectEngine.UndoOccurred(ActiveControl); //this should be called immediately *after* the undo operation
end;

procedure TfrmMain.actUndoUpdate(Sender: TObject);
var
  EditWnd: HWND;
begin
  EditWnd := GetActiveEditWnd;
  actUndo.Enabled := (EditWnd <> 0) and (SendMessage(EditWnd, EM_CANUNDO, 0, 0) <> 0) and
    (GetWindowLong(EditWnd, GWL_STYLE) and ES_READONLY = 0);
end;

end.
