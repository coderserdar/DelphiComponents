{*********************************************************}
{* Dialog to select a table index (for reindexing)       *}
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

unit dgselidx;

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
  Grids,
  Buttons,
  ExtCtrls,
  ffllbase,
  ubase,
  uelement,
  uentity;

type
  TdlgSelectIndex = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Label1: TLabel;
    grdIndexes: TStringGrid;
    edtTableName: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FTable : TffeTableItem;
    FIndexNum: Integer;
    FIndexes: TffeIndexList;
    FCoverage: TffShStr;
  public
  end;                       

function SelectIndexDlg(aTable : TffeTableItem;
                        var aIndexNum: Integer): TModalResult;

var
  dlgSelectIndex: TdlgSelectIndex;

implementation

{$R *.DFM}

function SelectIndexDlg(aTable : TffeTableItem;
                        var aIndexNum: Integer): TModalResult;
begin
  with TdlgSelectIndex.Create(nil) do
  try
    FTable := aTable;
    FIndexes.Empty;
    Result := ShowModal;
    aIndexNum := FIndexNum;
  finally
    Free;
  end;
end;

procedure TdlgSelectIndex.FormShow(Sender: TObject);
var
  I, J: Integer;
  OldCursor: TCursor;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    with FTable do begin
      edtTableName.Text := TableName;
      FIndexes.LoadFromDict(Dictionary);
      with grdIndexes do begin
        RowCount := FIndexes.Count + 1;
        for I := 0 to FIndexes.Count - 1 do begin
          Cells[0, I + 1] := FIndexes.Items[I].Name;
          if I = 0 then
            FCoverage := 'physical record position'
          else with FIndexes.Items[I] do begin
            case iiKeyTypeIndex of
              0: begin
                   FCoverage := 'Comp: ';
                   for J := 0 to FieldCount - 1 do begin
                     FCoverage := FCoverage + FieldName[J];
                     if J < FieldCount - 1 then
                       FCoverage := FCoverage + ', ';
                   end;
                 end;
              1: begin
                   FCoverage := 'User: ';
                 end;
            end;
          end;
          Cells[1, I + 1] := FCoverage;
        end;
      end;
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TdlgSelectIndex.FormCreate(Sender: TObject);
begin
  FIndexes := TffeIndexList.Create;
  grdIndexes.Cells[0, 0] := 'Index Name';
  grdIndexes.Cells[1, 0] := 'Coverage';
end;

procedure TdlgSelectIndex.FormDestroy(Sender: TObject);
begin
  FIndexes.Free;
end;

procedure TdlgSelectIndex.btnOKClick(Sender: TObject);
begin
  FIndexNum := grdIndexes.Selection.Top - 1;
end;

end.
