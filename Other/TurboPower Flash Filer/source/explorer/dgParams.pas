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

unit dgParams;

interface

{$I FFDEFINE.INC}

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Grids, db
  {$IFDEF Delphi3}
  , dbTables
  {$ENDIF}, ffllgrid;

type
  TdlgParams = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    cbParamType: TComboBox;
    gdParams: TffStringGrid;
    procedure gdParamsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure gdParamsKeyPress(Sender: TObject; var Key: Char);
    procedure gdParamsGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: String);
    procedure cbParamTypeChange(Sender: TObject);
    procedure cbParamTypeExit(Sender: TObject);
    procedure gdParamsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    { Private declarations }
    function GetCellBackgroundColour(aColour: TColor; ACol, ARow: Integer) : TColor;
    procedure GetStringProc(const S: String);
    procedure CMDialogKey(var msg: TCMDialogKey); message CM_DIALOGKEY;
    procedure ShowCellCombo(ComboBox: TCustomComboBox; Grid: TCustomGrid;
      Rect: TRect; aColour : TColor);
  public
    { Public declarations }
    function GetParamValues(aParams: TParams) : Boolean;
      { reads values from the stringgrid }
    function EditParamValues(aParams: TParams): Boolean;
      { opens dialog to edit and return values from the stringgrid }
  end;


implementation

{$R *.dfm}

uses
{$IFDEF DCC6OrLater}
  Variants,
{$ENDIF}
  Messages,
  TypInfo;


const
  colParamName = 0;
  colParamValue = 1;
  colParamType = 2;


{ create "hack" classes we can use to
  use the normally protected properties }
type
  THackGrid = class(TStringGrid)
  public
    property InplaceEditor;
  end;

  THackEdit = class(TInplaceEdit)
  public
    property Color;
  end;

  
const
  sBlankNotSupported = 'Blank parameters not supported for non-string types';


{ TdlgParams }

function TdlgParams.GetParamValues(aParams: TParams): Boolean;
var
  RowIdx : Integer;
begin
  Result := True;
  { copy values to Params }
  for RowIdx := 1 to Pred(gdParams.RowCount) do begin
    if (gdParams.Cells[colParamValue, RowIdx]<>'') or
       (TFieldType(GetEnumValue(TypeInfo(TFieldType), gdParams.Cells[colParamType, RowIdx]))=ftString) then begin
      aParams[RowIdx-1].DataType := TFieldType(GetEnumValue(TypeInfo(TFieldType), gdParams.Cells[colParamType, RowIdx]));
      aParams[RowIdx-1].Value := gdParams.Cells[colParamValue, RowIdx];
    end
    else
      raise Exception.Create(sBlankNotSupported);
  end;
end;


function TdlgParams.EditParamValues(aParams: TParams): Boolean;
var
  RowIdx,
  ParIdx : Integer;
begin
  { extract values previously entered }
  { for each row in grid }
  for RowIdx := 1 to Pred(gdParams.RowCount) do
    { check if param exists in new params list }
    for ParIdx := 0 to Pred(aParams.Count) do
      if (aParams[ParIdx].Name=gdParams.Cells[colParamName, RowIdx]) and
         (gdParams.Cells[colParamValue, RowIdx]<>'') then begin
        { and copy value and type if so }
        aParams[ParIdx].DataType := TFieldType(GetEnumValue(TypeInfo(TFieldType), gdParams.Cells[colParamType, RowIdx]));
        aParams[ParIdx].Value := gdParams.Cells[colParamValue, RowIdx];
        Break;
      end;

  { fill grid with new contents }
  gdParams.RowCount := aParams.Count+1;
  for RowIdx := 1 to Pred(gdParams.RowCount) do begin
    gdParams.Cells[colParamName, RowIdx] := aParams[RowIdx-1].Name;
    gdParams.Cells[colParamValue, RowIdx] := aParams[RowIdx-1].Value;
    gdParams.Cells[colParamType, RowIdx] := GetEnumName(TypeInfo(TFieldType), Integer(aParams[RowIdx-1].DataType));
  end;
  Result := ShowModal=mrOK;
  { copy new values to Params? }
  if Result then
    GetParamValues(aParams); 
end;


function TdlgParams.GetCellBackgroundColour(aColour: TColor; ACol, ARow: Integer) : TColor;
Const
  BlueIdx = 0;
var
  ColourBytes : Array[0..3] of byte absolute Result;
begin
  Result := aColour;
  if ((ARow Mod 2) = 1) and (ACol>0) then begin
    Result := ColorToRGB(aColour);
    if ColourBytes[BlueIdx]>127 then
      ColourBytes[BlueIdx] := ColourBytes[BlueIdx]-16
    else
      ColourBytes[BlueIdx] := ColourBytes[BlueIdx]+16;
  end;
end;


procedure TdlgParams.gdParamsDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  with Sender as TStringGrid do
  begin
    { change backgroundcolour slightly on every other row }
    Canvas.Brush.Color := GetCellBackgroundColour(Canvas.Brush.Color, ACol, ARow);
    case ARow of
      1..MaxInt : case ACol of
                    colParamValue,
                    colParamType  : Begin
                                      Canvas.Font.Color := Font.Color;
                                      Canvas.FillRect(Rect);
                                      Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, Cells[ACol, ARow]);
                                    End;
                  end;
    end;
    if gdFocused in State then
      Canvas.DrawFocusRect(Rect);
  end;
end;


procedure TdlgParams.GetStringProc(Const S : String);
begin
  cbParamType.Items.Add(S);
end;


procedure TdlgParams.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  gdParams.DefaultRowHeight := cbParamType.Height;
  gdParams.Cells[colParamName, 0] := 'Parameter:';
  gdParams.Cells[colParamValue, 0] := 'Value:';
  gdParams.Cells[colParamType, 0] := 'Type:';

  cbParamType.Clear;
  with GetTypeData(TypeInfo(TFieldType))^ do
  begin
    for I := MinValue to MaxValue do
      GetStringProc(GetEnumName(TypeInfo(TFieldType), I));
  end;
end;


procedure TdlgParams.gdParamsKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then begin
    if (Succ(gdParams.Col)=gdParams.ColCount) and
       (Succ(gdParams.Row)=gdParams.RowCount) then
      ModalResult := mrOK
    else
    if (Succ(gdParams.Col)=gdParams.ColCount) then begin
      gdParams.Col := colParamValue;
      gdParams.Row := gdParams.Row + 1;
    end
    else
      gdParams.Col := gdParams.Col + 1;
  end
  else
  if Key=#27 then
    ModalResult := mrCancel;
end;


procedure TdlgParams.gdParamsGetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: String);
begin
  Assert(Sender is TStringGrid);
  with THackGrid(Sender) do
    THackEdit(InplaceEditor).Color := GetCellBackgroundColour(Color, ACol, ARow);
end;


procedure TdlgParams.cbParamTypeChange(Sender: TObject);
begin
  with gdParams do begin
    Cells[Col, Row] := cbParamType.Items[cbParamType.ItemIndex];
  end;
  gdParams.Invalidate;
end;


procedure TdlgParams.cbParamTypeExit(Sender: TObject);
begin
  cbParamType.Visible := False;
  if Assigned(ActiveControl) and not(ActiveControl = gdParams) then
    ActiveControl.SetFocus
  else begin
    gdParams.SetFocus;
    gdParams.Perform(WM_KEYDOWN, VK_TAB, 0);
  end;
end;


procedure TdlgParams.gdParamsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  R : TRect;
begin
  case ACol of
    colParamType :
      begin
        R := gdParams.CellRect(ACol, ARow);
        ShowCellCombo(cbParamType, gdParams, R, GetCellBackgroundColour(gdParams.Canvas.Brush.Color, ACol, ARow));
        cbParamType.ItemIndex :=
          cbParamType.Items.IndexOf(gdParams.Cells[ACol, ARow]);
      end;
  end;
end;


procedure TdlgParams.CMDialogKey(var msg: TCMDialogKey);
begin
  if (ActiveControl = cbParamType) then
  begin
    if (msg.CharCode = VK_TAB) then
    begin
      ActiveControl.Visible := False;
      msg.result := 1;
      Exit;
    end;
  end;
  inherited;
end;


procedure TdlgParams.ShowCellCombo(ComboBox: TCustomComboBox;
  Grid: TCustomGrid; Rect: TRect; aColour : TColor);
begin
  Rect.Left := Rect.Left + Grid.Left;
  Rect.Right := Rect.Right + Grid.Left;
  Rect.Top := Rect.Top + Grid.Top;
  Rect.Bottom := Rect.Bottom + Grid.Top;
  ComboBox.Left := Rect.Left + 1;
  ComboBox.Top := Rect.Top + 1;
  ComboBox.Width := (Rect.Right + 1) - Rect.Left;
  ComboBox.Height := (Rect.Bottom + 1) - Rect.Top;

 {Display the combobox}
  ComboBox.Visible := True;
  TComboBox(ComboBox).Color := aColour;
  ComboBox.SetFocus;
end;


end.
