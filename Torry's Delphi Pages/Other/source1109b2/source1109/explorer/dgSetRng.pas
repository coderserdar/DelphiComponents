{$I fsdefine.inc}

Unit dgSetRng;

Interface

Uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Forms,
  Controls,
  Stdctrls,
  Buttons,
  ExtCtrls,
  fsllbase,
  fsdb,
  ubase,
  uelement,
  uentity;

Type
  TdlgSetRange = Class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    paField1: TPanel;
    laField1: TLabel;
    edStart1: TEdit;
    edEnd1: TEdit;
    cbStartNull1: TCheckBox;
    cbEndNull1: TCheckBox;
    paField16: TPanel;
    laField16: TLabel;
    edStart16: TEdit;
    edEnd16: TEdit;
    cbStartNull16: TCheckBox;
    cbEndNull16: TCheckBox;
    paField15: TPanel;
    laField15: TLabel;
    edStart15: TEdit;
    edEnd15: TEdit;
    cbStartNull15: TCheckBox;
    cbEndNull15: TCheckBox;
    paField14: TPanel;
    laField14: TLabel;
    edStart14: TEdit;
    edEnd14: TEdit;
    cbStartNull14: TCheckBox;
    cbEndNull14: TCheckBox;
    paField13: TPanel;
    laField13: TLabel;
    edStart13: TEdit;
    edEnd13: TEdit;
    cbStartNull13: TCheckBox;
    cbEndNull13: TCheckBox;
    paField12: TPanel;
    laField12: TLabel;
    edStart12: TEdit;
    edEnd12: TEdit;
    cbStartNull12: TCheckBox;
    cbEndNull12: TCheckBox;
    paField11: TPanel;
    laField11: TLabel;
    edStart11: TEdit;
    edEnd11: TEdit;
    cbStartNull11: TCheckBox;
    cbEndNull11: TCheckBox;
    paField10: TPanel;
    laField10: TLabel;
    edStart10: TEdit;
    edEnd10: TEdit;
    cbStartNull10: TCheckBox;
    cbEndNull10: TCheckBox;
    paField9: TPanel;
    laField9: TLabel;
    edStart9: TEdit;
    edEnd9: TEdit;
    cbStartNull9: TCheckBox;
    cbEndNull9: TCheckBox;
    paField8: TPanel;
    laField8: TLabel;
    edStart8: TEdit;
    edEnd8: TEdit;
    cbStartNull8: TCheckBox;
    cbEndNull8: TCheckBox;
    paField7: TPanel;
    laField7: TLabel;
    edStart7: TEdit;
    edEnd7: TEdit;
    cbStartNull7: TCheckBox;
    cbEndNull7: TCheckBox;
    paField6: TPanel;
    laField6: TLabel;
    edStart6: TEdit;
    edEnd6: TEdit;
    cbStartNull6: TCheckBox;
    cbEndNull6: TCheckBox;
    paField5: TPanel;
    laField5: TLabel;
    edStart5: TEdit;
    edEnd5: TEdit;
    cbStartNull5: TCheckBox;
    cbEndNull5: TCheckBox;
    paField4: TPanel;
    laField4: TLabel;
    edStart4: TEdit;
    edEnd4: TEdit;
    cbStartNull4: TCheckBox;
    cbEndNull4: TCheckBox;
    paField3: TPanel;
    laField3: TLabel;
    edStart3: TEdit;
    edEnd3: TEdit;
    cbStartNull3: TCheckBox;
    cbEndNull3: TCheckBox;
    paField2: TPanel;
    laField2: TLabel;
    edStart2: TEdit;
    edEnd2: TEdit;
    cbStartNull2: TCheckBox;
    cbEndNull2: TCheckBox;
    paBottom: TPanel;
    CancelBtn: TButton;
    OKBtn: TButton;
    cbRangeStartKeyExclusive: TCheckBox;
    cbRangeEndKeyExclusive: TCheckBox;
    Bevel1: TBevel;
    Procedure cbStartNull1Click(Sender: TObject);
    Procedure cbEndNull1Click(Sender: TObject);
  Private
    { Private declarations }
    Procedure SetNumberOfFields(NumFields: Integer);
    Function GetStartNull(FieldIdx: Integer): Boolean;
    Function GetEndNull(FieldIdx: Integer): Boolean;
    Function GetStartValue(FieldIdx: Integer): String;
    Function GetEndValue(FieldIdx: Integer): String;
    Procedure SetStartNull(FieldIdx: Integer; IsNull: Boolean);
    Procedure SetEndNull(FieldIdx: Integer; IsNull: Boolean);
    Procedure SetStartValue(FieldIdx: Integer; Value: String);
    Procedure SetEndValue(FieldIdx: Integer; Value: String);
    Procedure SetFieldName(FieldIdx: Integer; Value: String);
  Public
    { Public declarations }
  End;

  TffRangeValuesForField = Record
    StartNull,
      EndNull: Boolean;
    StartValue,
      EndValue: String[255];
  End;

  TffRangeValues = Record  //fscl_MaxIndexFlds
    Field: Array[1..16] Of TffRangeValuesForField;
    RangeStartKeyExclusive,
      RangeEndKeyExclusive: Boolean;
  End;

Function SetRangeDlg(aTable: TFSTable; Var RangeValues: TffRangeValues): TModalResult;

Implementation

{$R *.dfm}

Function SetRangeDlg(aTable: TFSTable; Var RangeValues: TffRangeValues): TModalResult;
Var
  FieldIdx: Integer;
Begin
  With TdlgSetRange.Create(Nil) Do
    Try
      For FieldIdx := Low(RangeValues.Field) To High(RangeValues.Field) Do
        Begin
          SetStartNull(FieldIdx, RangeValues.Field[FieldIdx].StartNull);
          SetEndNull(FieldIdx, RangeValues.Field[FieldIdx].EndNull);
          SetStartValue(FieldIdx, RangeValues.Field[FieldIdx].StartValue);
          SetEndValue(FieldIdx, RangeValues.Field[FieldIdx].EndValue);
        End;
      For FieldIdx := 1 To aTable.IndexFieldCount Do
        SetFieldName(FieldIdx, aTable.IndexFields[FieldIdx - 1].DisplayName);
      SetNumberOfFields(aTable.IndexFieldCount);
      cbRangeStartKeyExclusive.Checked := RangeValues.RangeStartKeyExclusive;
      cbRangeEndKeyExclusive.Checked := RangeValues.RangeEndKeyExclusive;
      Result := ShowModal;
      If Result = mrOK Then
        Begin
          For FieldIdx := Low(RangeValues.Field) To High(RangeValues.Field) Do
            Begin
              RangeValues.Field[FieldIdx].StartNull := GetStartNull(FieldIdx);
              RangeValues.Field[FieldIdx].EndNull := GetEndNull(FieldIdx);
              RangeValues.Field[FieldIdx].StartValue := GetStartValue(FieldIdx);
              RangeValues.Field[FieldIdx].EndValue := GetEndValue(FieldIdx);
            End;
          RangeValues.RangeStartKeyExclusive := cbRangeStartKeyExclusive.Checked;
          RangeValues.RangeEndKeyExclusive := cbRangeEndKeyExclusive.Checked;
        End;
    Finally
      Free;
    End;
End;

{ TdlgSetRange }

Procedure TdlgSetRange.SetNumberOfFields(NumFields: Integer);
Var
  FieldIdx: Integer;
Begin
  For FieldIdx := 16 Downto NumFields + 1 Do
    TPanel(FindComponent('paField' + IntToStr(FieldIdx))).Visible := False;
  ClientHeight := paBottom.Top + paBottom.Height;
End;

Function TdlgSetRange.GetEndNull(FieldIdx: Integer): Boolean;
Begin
  Result := TCheckBox(FindComponent('cbEndNull' + IntToStr(FieldIdx))).Checked;
End;

Function TdlgSetRange.GetEndValue(FieldIdx: Integer): String;
Begin
  Result := TEdit(FindComponent('edEnd' + IntToStr(FieldIdx))).Text;
End;

Function TdlgSetRange.GetStartNull(FieldIdx: Integer): Boolean;
Begin
  Result := TCheckBox(FindComponent('cbStartNull' + IntToStr(FieldIdx))).Checked;
End;

Function TdlgSetRange.GetStartValue(FieldIdx: Integer): String;
Begin
  Result := TEdit(FindComponent('edStart' + IntToStr(FieldIdx))).Text;
End;

Procedure TdlgSetRange.SetEndNull(FieldIdx: Integer; IsNull: Boolean);
Var
  t: tcomponent;
Begin
  t := FindComponent('cbEndNull' + IntToStr(FieldIdx));
  If t <> Nil Then
    TCheckBox(t).Checked := IsNull;
End;

Procedure TdlgSetRange.SetEndValue(FieldIdx: Integer; Value: String);
Var
  t: tcomponent;
Begin
  t := FindComponent('edEnd' + IntToStr(FieldIdx));
  If t <> Nil Then
    TEdit(t).Text := Value;
End;

Procedure TdlgSetRange.SetStartNull(FieldIdx: Integer; IsNull: Boolean);
Var
  t: tcomponent;
Begin
  t := FindComponent('cbStartNull' + IntToStr(FieldIdx));
  If t <> Nil Then
    TCheckBox(t).Checked := IsNull;
End;

Procedure TdlgSetRange.SetStartValue(FieldIdx: Integer; Value: String);
Var
  t: tcomponent;
Begin
  t := FindComponent('edStart' + IntToStr(FieldIdx));
  If t <> Nil Then
    TEdit(FindComponent('edStart' + IntToStr(FieldIdx))).Text := Value;
End;

Procedure TdlgSetRange.SetFieldName(FieldIdx: Integer; Value: String);
Begin
  TLabel(FindComponent('laField' + IntToStr(FieldIdx))).Caption := Value;
End;

Procedure TdlgSetRange.cbStartNull1Click(Sender: TObject);
Var
  FieldIdx: String;
Begin
  FieldIdx := Copy(TCheckBox(Sender).Name, 12, 2);
  TEdit(FindComponent('edStart' + FieldIdx)).Enabled := Not TCheckBox(Sender).Checked;
End;

Procedure TdlgSetRange.cbEndNull1Click(Sender: TObject);
Var
  FieldIdx: String;
Begin
  FieldIdx := Copy(TCheckBox(Sender).Name, 10, 2);
  TEdit(FindComponent('edEnd' + FieldIdx)).Enabled := Not TCheckBox(Sender).Checked;
End;

End.

