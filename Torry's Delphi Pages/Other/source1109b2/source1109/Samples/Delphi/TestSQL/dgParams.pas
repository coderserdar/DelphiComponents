Unit dgParams;

Interface

{$I FsDEFINE.INC}

Uses Windows,
  SysUtils,
  Classes,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Buttons,
  ExtCtrls,
  Grids,
  db,
  fsllgrid,
  fsexfield;

Type
  TdlgParams = Class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    cbParamType: TComboBox;
    gdParams: TfsGrid;
    Procedure gdParamsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    Procedure FormCreate(Sender: TObject);
    Procedure gdParamsKeyPress(Sender: TObject; Var Key: Char);
    Procedure gdParamsGetEditText(Sender: TObject; ACol, ARow: Integer;
      Var Value: String);
    Procedure cbParamTypeChange(Sender: TObject);
    Procedure cbParamTypeExit(Sender: TObject);
    Procedure gdParamsSelectCell(Sender: TObject; ACol, ARow: Integer;
      Var CanSelect: Boolean);
  Private
    { Private declarations }
    Function GetCellBackgroundColour(aColour: TColor; ACol, ARow: Integer): TColor;
    Procedure GetStringProc(Const S: String);
    Procedure CMDialogKey(Var msg: TCMDialogKey); Message CM_DIALOGKEY;
    Procedure ShowCellCombo(ComboBox: TCustomComboBox; Grid: TCustomGrid;
      Rect: TRect; aColour: TColor);
  Public
    { Public declarations }
    Function GetParamValues(aParams: TfsParams): Boolean;
    { reads values from the stringgrid }
    Function EditParamValues(aParams: TfsParams): Boolean;
    { opens dialog to edit and return values from the stringgrid }
  End;

Implementation

{$R *.dfm}

Uses
  {$IFDEF DCC6OrLater}
  Variants,
  {$ENDIF}
  Messages,
  TypInfo;

Const
  colParamName = 0;
  colParamValue = 1;
  colParamType = 2;

  { create "hack" classes we can use to
    use the normally protected properties }
Type
  THackGrid = Class(TStringGrid)
  Public
    Property InplaceEditor;
  End;

  THackEdit = Class(TInplaceEdit)
  Public
    Property Color;
  End;

Const
  sBlankNotSupported = 'Blank parameters not supported for non-string types';

  { TdlgParams }

Function TdlgParams.GetParamValues(aParams: TfsParams): Boolean;
Var
  RowIdx: Integer;
Begin
  Result := True;
  { copy values to Params }
  For RowIdx := 1 To Pred(gdParams.RowCount) Do
    Begin
      If (gdParams.Cells[colParamValue, RowIdx] <> '') Or
        (TFieldType(GetEnumValue(TypeInfo(TFieldType), gdParams.Cells[colParamType, RowIdx])) = ftString) Then
        Begin
          aParams[RowIdx - 1].DataType := TFieldType(GetEnumValue(TypeInfo(TFieldType), gdParams.Cells[colParamType, RowIdx]));
          aParams[RowIdx - 1].Value := gdParams.Cells[colParamValue, RowIdx];
        End
      Else
        Raise Exception.Create(sBlankNotSupported);
    End;
End;

Function TdlgParams.EditParamValues(aParams: TfsParams): Boolean;
Var
  RowIdx,
    ParIdx: Integer;
Begin
  { extract values previously entered }
  { for each row in grid }
  For RowIdx := 1 To Pred(gdParams.RowCount) Do
    { check if param exists in new params list }
    For ParIdx := 0 To Pred(aParams.Count) Do
      If (aParams[ParIdx].Name = gdParams.Cells[colParamName, RowIdx]) And
        (gdParams.Cells[colParamValue, RowIdx] <> '') Then
        Begin
          { and copy value and type if so }
          aParams[ParIdx].DataType := TFieldType(GetEnumValue(TypeInfo(TFieldType), gdParams.Cells[colParamType, RowIdx]));
          aParams[ParIdx].Value := gdParams.Cells[colParamValue, RowIdx];
          Break;
        End;

  { fill grid with new contents }
  gdParams.RowCount := aParams.Count + 1;
  For RowIdx := 1 To Pred(gdParams.RowCount) Do
    Begin
      gdParams.Cells[colParamName, RowIdx] := aParams[RowIdx - 1].Name;
      gdParams.Cells[colParamValue, RowIdx] := aParams[RowIdx - 1].Value;
      gdParams.Cells[colParamType, RowIdx] := GetEnumName(TypeInfo(TFieldType), Integer(aParams[RowIdx - 1].DataType));
    End;
  Result := ShowModal = mrOK;
  { copy new values to Params? }
  If Result Then
    GetParamValues(aParams);
End;

Function TdlgParams.GetCellBackgroundColour(aColour: TColor; ACol, ARow: Integer): TColor;
Const
  BlueIdx = 0;
Var
  ColourBytes: Array[0..3] Of byte Absolute Result;
Begin
  Result := aColour;
  If ((ARow Mod 2) = 1) And (ACol > 0) Then
    Begin
      Result := ColorToRGB(aColour);
      If ColourBytes[BlueIdx] > 127 Then
        ColourBytes[BlueIdx] := ColourBytes[BlueIdx] - 16
      Else
        ColourBytes[BlueIdx] := ColourBytes[BlueIdx] + 16;
    End;
End;

Procedure TdlgParams.gdParamsDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
Begin
  With Sender As TStringGrid Do
    Begin
      { change backgroundcolour slightly on every other row }
      Canvas.Brush.Color := GetCellBackgroundColour(Canvas.Brush.Color, ACol, ARow);
      Case ARow Of
        1..MaxInt: Case ACol Of
            colParamValue,
              colParamType:
              Begin
                Canvas.Font.Color := Font.Color;
                Canvas.FillRect(Rect);
                Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, Cells[ACol, ARow]);
              End;
          End;
      End;
      If gdFocused In State Then
        Canvas.DrawFocusRect(Rect);
    End;
End;

Procedure TdlgParams.GetStringProc(Const S: String);
Begin
  cbParamType.Items.Add(S);
End;

Procedure TdlgParams.FormCreate(Sender: TObject);
Var
  I: Integer;
Begin
  gdParams.DefaultRowHeight := cbParamType.Height;
  gdParams.Cells[colParamName, 0] := 'Parameter:';
  gdParams.Cells[colParamValue, 0] := 'Value:';
  gdParams.Cells[colParamType, 0] := 'Type:';

  cbParamType.Clear;
  With GetTypeData(TypeInfo(TFieldType))^ Do
    Begin
      For I := MinValue To MaxValue Do
        GetStringProc(GetEnumName(TypeInfo(TFieldType), I));
    End;
End;

Procedure TdlgParams.gdParamsKeyPress(Sender: TObject; Var Key: Char);
Begin
  If Key = #13 Then
    Begin
      If (Succ(gdParams.Col) = gdParams.ColCount) And
        (Succ(gdParams.Row) = gdParams.RowCount) Then
        ModalResult := mrOK
      Else If (Succ(gdParams.Col) = gdParams.ColCount) Then
        Begin
          gdParams.Col := colParamValue;
          gdParams.Row := gdParams.Row + 1;
        End
      Else
        gdParams.Col := gdParams.Col + 1;
    End
  Else If Key = #27 Then
    ModalResult := mrCancel;
End;

Procedure TdlgParams.gdParamsGetEditText(Sender: TObject; ACol,
  ARow: Integer; Var Value: String);
Begin
  Assert(Sender Is TStringGrid);
  With THackGrid(Sender) Do
    THackEdit(InplaceEditor).Color := GetCellBackgroundColour(Color, ACol, ARow);
End;

Procedure TdlgParams.cbParamTypeChange(Sender: TObject);
Begin
  With gdParams Do
    Begin
      Cells[Col, Row] := cbParamType.Items[cbParamType.ItemIndex];
    End;
  gdParams.Invalidate;
End;

Procedure TdlgParams.cbParamTypeExit(Sender: TObject);
Begin
  cbParamType.Visible := False;
  If Assigned(ActiveControl) And Not (ActiveControl = gdParams) Then
    ActiveControl.SetFocus
  Else
    Begin
      gdParams.SetFocus;
      gdParams.Perform(WM_KEYDOWN, VK_TAB, 0);
    End;
End;

Procedure TdlgParams.gdParamsSelectCell(Sender: TObject; ACol,
  ARow: Integer; Var CanSelect: Boolean);
Var
  R: TRect;
Begin
  Case ACol Of
    colParamType:
      Begin
        R := gdParams.CellRect(ACol, ARow);
        ShowCellCombo(cbParamType, gdParams, R, GetCellBackgroundColour(gdParams.Canvas.Brush.Color, ACol, ARow));
        cbParamType.ItemIndex :=
          cbParamType.Items.IndexOf(gdParams.Cells[ACol, ARow]);
      End;
  End;
End;

Procedure TdlgParams.CMDialogKey(Var msg: TCMDialogKey);
Begin
  If (ActiveControl = cbParamType) Then
    Begin
      If (msg.CharCode = VK_TAB) Then
        Begin
          ActiveControl.Visible := False;
          msg.result := 1;
          Exit;
        End;
    End;
  Inherited;
End;

Procedure TdlgParams.ShowCellCombo(ComboBox: TCustomComboBox;
  Grid: TCustomGrid; Rect: TRect; aColour: TColor);
Begin
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
End;

End.

