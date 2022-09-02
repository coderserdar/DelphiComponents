{*********************************************************}
{* Design-time SQL Editor                                *}
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

{$I fsdefine.inc}

Unit fsclsqle;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ComCtrls,
  ToolWin,
  ExtCtrls,
  Stdctrls,
  {$IFDEF DCC4OrLater}
  ImgList,
  {$ENDIF}
  Menus;

Type
  TfsSqlEditor = Class(TForm)
    pnlBottom: TPanel;
    ToolBar1: TToolBar;
    imgToolbar: TImageList;
    tbLoad: TToolButton;
    tbSave: TToolButton;
    memSQL: TRichEdit;
    lblStatus: TLabel;
    pbCancel: TButton;
    pbOK: TButton;
    pmMain: TPopupMenu;
    pmMainLoad: TMenuItem;
    pmMainSave: TMenuItem;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    mnuMain: TMainMenu;
    mnuMainFile: TMenuItem;
    mnuMainSave: TMenuItem;
    mnuMainLoad: TMenuItem;
    Procedure memSQLChange(Sender: TObject);
    Procedure tbLoadClick(Sender: TObject);
    Procedure tbSaveClick(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure memSQLMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure FormKeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
  Private
    { Private declarations }
    Function GetLines: Longint;
    Procedure SetLines(anOrdValue: Longint);

  Public
    { Public declarations }
    Property SQLLines: Longint Read GetLines Write SetLines;
  End;

Var
  fsSqlEditor: TfsSqlEditor;

Implementation

Uses
  fsllbase;

{$R *.DFM}

Const
  ffcLine: String = '%d line';
  ffcLines: String = '%d lines';

  {===TffSqlEditor=====================================================}

Procedure TfsSqlEditor.memSQLChange(Sender: TObject);
Var
  aCount: Integer;
Begin
  aCount := memSQL.Lines.Count;
  If aCount = 1 Then
    lblStatus.Caption := format(ffcLine, [aCount])
  Else
    lblStatus.Caption := format(ffcLines, [aCount]);
End;
{--------}

Procedure TfsSqlEditor.tbLoadClick(Sender: TObject);
Begin
  If dlgOpen.Execute Then
    Begin
      dlgOpen.InitialDir := ExtractFilePath(dlgOpen.FileName);
      memSQL.Lines.LoadFromFile(dlgOpen.FileName);
    End;
End;
{--------}

Procedure TfsSqlEditor.tbSaveClick(Sender: TObject);
Begin
  { Do we have a filename from the last save? }
  If dlgSave.FileName = '' Then
    { No.  Use the one from the open dialog. }
    dlgSave.FileName := dlgOpen.FileName;

  If dlgSave.InitialDir = '' Then
    dlgSave.InitialDir := dlgOpen.InitialDir;

  If dlgSave.Execute Then
    Begin
      dlgSave.InitialDir := ExtractFilePath(dlgSave.FileName);
      memSQL.Lines.SaveToFile(dlgSave.FileName);
    End;
End;
{--------}

Procedure TfsSqlEditor.FormShow(Sender: TObject);
Begin
  { Set default file extensions. }
  dlgOpen.DefaultExt := fsc_ExtForSQL;
  dlgSave.DefaultExt := dlgOpen.DefaultExt;
End;
{--------}

Function TfsSqlEditor.GetLines: Longint;
Begin
  Result := Longint(memSQL.Lines);
End;
{--------}

Procedure TfsSqlEditor.SetLines(anOrdValue: Longint);
Begin
  memSQL.Lines := TStrings(anOrdValue);
End;
{--------}

Procedure TfsSqlEditor.memSQLMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  MousePos: TPoint;
Begin
  If Button = mbRight Then
    Begin
      MousePos := memSQL.ClientToScreen(Point(X, Y));
      pmMain.Popup(MousePos.X, MousePos.Y);
    End;
End;
{====================================================================}

Procedure TfsSqlEditor.FormKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If Key = VK_ESCAPE Then
    Close;
End;

End.

