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

{$I ffdefine.inc}

unit ffclsqle;

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
  ToolWin,
  ExtCtrls,
  StdCtrls,
  {$IFDEF DCC4OrLater}
  ImgList,
  {$ENDIF}
  Menus;


type
  TffSqlEditor = class(TForm)
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
    procedure memSQLChange(Sender: TObject);
    procedure tbLoadClick(Sender: TObject);
    procedure tbSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure memSQLMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    function GetLines : longInt;
    procedure SetLines(anOrdValue : longInt);
    
  public
    { Public declarations }
    property SQLLines : longint read GetLines write SetLines;
  end;

var
  ffSqlEditor: TffSqlEditor;

implementation

uses
  ffllbase;

{$R *.DFM}

const
  ffcLine : string = '%d line';
  ffcLines : string = '%d lines';

{===TffSqlEditor=====================================================}
procedure TffSqlEditor.memSQLChange(Sender: TObject);
var
  aCount : integer;
begin
  aCount := memSQL.Lines.Count;
  if aCount = 1 then
    lblStatus.Caption := format(ffcLine, [aCount])
  else
    lblStatus.Caption := format(ffcLines, [aCount]);
end;
{--------}
procedure TffSqlEditor.tbLoadClick(Sender: TObject);
begin
  if dlgOpen.Execute then begin
    dlgOpen.InitialDir := ExtractFilePath(dlgOpen.FileName);
    memSQL.Lines.LoadFromFile(dlgOpen.FileName);
  end;
end;
{--------}
procedure TffSqlEditor.tbSaveClick(Sender: TObject);
begin
  { Do we have a filename from the last save? }
  if dlgSave.FileName = '' then
    { No.  Use the one from the open dialog. }
    dlgSave.FileName := dlgOpen.FileName;

  if dlgSave.InitialDir = '' then
    dlgSave.InitialDir := dlgOpen.InitialDir;

  if dlgSave.Execute then begin
    dlgSave.InitialDir := ExtractFilePath(dlgSave.FileName);
    memSQL.Lines.SaveToFile(dlgSave.FileName);
  end;
end;
{--------}
procedure TffSqlEditor.FormShow(Sender: TObject);
begin
  { Set default file extensions. }
  dlgOpen.DefaultExt := ffc_ExtForSQL;
  dlgSave.DefaultExt := dlgOpen.DefaultExt;
end;
{--------}
function TffSqlEditor.GetLines : longInt;
begin
  Result := longInt(memSQL.Lines);
end;
{--------}
procedure TffSqlEditor.SetLines(anOrdValue : longInt);
begin
  memSQL.Lines := TStrings(anOrdValue);
end;
{--------}
procedure TffSqlEditor.memSQLMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MousePos : TPoint;
begin
  if Button = mbRight then begin
    MousePos := memSQL.ClientToScreen(Point(X, Y));
    pmMain.Popup(MousePos.X, MousePos.Y);
  end;
end;
{====================================================================}
procedure TffSqlEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

end.
