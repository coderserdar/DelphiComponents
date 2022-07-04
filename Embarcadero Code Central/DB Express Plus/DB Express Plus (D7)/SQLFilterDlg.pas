unit SQLFilterDlg;

{*************************************************************}
{*                 Freeware dbExpress Plus                   *}
{* Copyright (c) Business Software Systems, Inc. 1995 - 2002 *}
{*                   All rights reserved.                    *}
{*************************************************************}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, DBXpress, DBClient,
  DB, SqlExpr, SQLMetaData, Mask;

type
  Tf_SQLFilterDlg = class(TForm)
    lb_Fields: TListBox;
    l_AvailableFields: TLabel;
    l_Operator: TLabel;
    l_Value: TLabel;
    b_Add: TBitBtn;
    b_Clear: TBitBtn;
    b_Apply: TBitBtn;
    m_SQLWhereStatement: TRichEdit;
    lb_Operator: TListBox;
    sle_Value: TMaskEdit;
    pnl_Options: TPanel;
    rg_Boolean: TRadioGroup;
    gb_Parenthesis: TGroupBox;
    l_LeftParenthesis: TLabel;
    l_RightParenthesis: TLabel;
    cb_LeftParenthesis: TComboBox;
    cb_RightParenthesis: TComboBox;
    procedure lb_FieldsClick(Sender: TObject);
    procedure b_AddClick(Sender: TObject);
    procedure b_ClearClick(Sender: TObject);
    procedure b_ApplyClick(Sender: TObject);
    procedure lb_OperatorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  protected
    { Protected declarations }
    i_OldDataType: Word;
    i_OldFieldType: TFieldType;  
  public
    { Public declarations }
  end;

var
  f_SQLFilterDlg: Tf_SQLFilterDlg;

  function SQLFilterDlgCreate(aSQLMetaData: TSQLMetaData; aTableName, aDbType, aCaption: ShortString;
     aBackGroundColor: TColor; aGlAcctSizes, aSQLWhereClause: Variant): String; overload;
  function SQLFilterDlgCreate(aClientDataSet: TClientDataSet; aTableName, aDbType, aCaption: ShortString;
     aBackGroundColor: TColor; aGlAcctSizes, aSQLWhereClause: Variant): String; overload;
  function FillString(StrLength: Integer; Value: Char): String;
  function FrontFillZero(StrLength: Integer; StrValue: String): String;



implementation

{$R *.dfm}

var
  i_SQLWhereStatement, i_TableName, i_DbType: String;
  i_SQLMetaData: TSQLMetaData;
  i_ClientDataSet: TClientDataSet;
  i_GlAcctSizes: Variant;

function SQLFilterDlgCreate(aSQLMetaData: TSQLMetaData; aTableName, aDbType, aCaption: ShortString;
   aBackGroundColor: TColor; aGlAcctSizes, aSQLWhereClause: Variant): String;
begin
  f_SQLFilterDlg := Tf_SQLFilterDlg.Create(Application.MainForm);

  i_ClientDataSet := nil;
  i_SQLMetaData := aSQLMetaData;
  i_TableName := aTableName;
  i_DbType := UpperCase(aDbType);
  i_SQLWhereStatement := '';
  i_GlAcctSizes := aGlAcctSizes;
  Result := '[ABORT]';

  f_SQLFilterDlg.SetBounds ((Screen.Width div 2) - (f_SQLFilterDlg.Width div 2),
     ((100 + Screen.Height) div 2) - (f_SQLFilterDlg.Height div 2),
     f_SQLFilterDlg.Width, f_SQLFilterDlg.Height);
  f_SQLFilterDlg.Caption := aCaption;
  f_SQLFilterDlg.Color := aBackGroundColor;
  f_SQLFilterDlg.pnl_Options.Color := aBackGroundColor;
  f_SQLFilterDlg.lb_Operator.ItemIndex := 0;
  f_SQLFilterDlg.sle_Value.Text := '';
  f_SQLFilterDlg.cb_LeftParenthesis.ItemIndex := 0;
  f_SQLFilterDlg.cb_RightParenthesis.ItemIndex := 0;
  f_SQLFilterDlg.rg_Boolean.ItemIndex := 0;
  f_SQLFilterDlg.m_SQLWhereStatement.Lines.Clear;
  f_SQLFilterDlg.m_SQLWhereStatement.Text := aSQLWhereClause;
  if aSQLWhereClause <> '[ABORT]' then
     f_SQLFilterDlg.m_SQLWhereStatement.Text := aSQLWhereClause;

  i_SQLMetaData.GetFieldNames(i_TableName, f_SQLFilterDlg.lb_Fields.Items);
  if f_SQLFilterDlg.lb_Fields.Items.Count > 0 then
     f_SQLFilterDlg.lb_Fields.ItemIndex := 0;

  f_SQLFilterDlg.ShowModal;
  if f_SQLFilterDlg.ModalResult = mrOK then Result := i_SQLWhereStatement;

  f_SQLFilterDlg.Free;
end;

function SQLFilterDlgCreate(aClientDataSet: TClientDataSet; aTableName, aDbType, aCaption: ShortString;
     aBackGroundColor: TColor; aGlAcctSizes, aSQLWhereClause: Variant): String;
var
  aFieldNum: Integer;
begin
  f_SQLFilterDlg := Tf_SQLFilterDlg.Create(Application.MainForm);

  i_ClientDataSet := aClientDataSet;
  i_SQLMetaData := nil;
  i_TableName := aTableName;
  i_DbType := UpperCase(aDbType);
  i_SQLWhereStatement := '';
  i_GlAcctSizes := aGlAcctSizes;
  Result := '[ABORT]';

  f_SQLFilterDlg.SetBounds ((Screen.Width div 2) - (f_SQLFilterDlg.Width div 2),
     ((100 + Screen.Height) div 2) - (f_SQLFilterDlg.Height div 2),
     f_SQLFilterDlg.Width, f_SQLFilterDlg.Height);
  f_SQLFilterDlg.Caption := aCaption;
  f_SQLFilterDlg.Color := aBackGroundColor;
  f_SQLFilterDlg.pnl_Options.Color := aBackGroundColor;
  f_SQLFilterDlg.lb_Operator.ItemIndex := 0;
  f_SQLFilterDlg.sle_Value.Text := '';
  f_SQLFilterDlg.cb_LeftParenthesis.ItemIndex := 0;
  f_SQLFilterDlg.cb_RightParenthesis.ItemIndex := 0;
  f_SQLFilterDlg.rg_Boolean.ItemIndex := 0;
  f_SQLFilterDlg.m_SQLWhereStatement.Lines.Clear;
  if aSQLWhereClause <> '[ABORT]' then
     f_SQLFilterDlg.m_SQLWhereStatement.Text := aSQLWhereClause;

  for aFieldNum := 0 to i_ClientDataSet.FieldDefs.Count -1 do begin
    if i_ClientDataSet.FieldDefs.Items[aFieldNum].DataType <> ftDataSet	then begin
      f_SQLFilterDlg.lb_Fields.Items.Add(
         i_ClientDataSet.FieldDefs.Items[aFieldNum].Name)
    end;
  end;
  if f_SQLFilterDlg.lb_Fields.Items.Count > 0 then
     f_SQLFilterDlg.lb_Fields.ItemIndex := 0;

  f_SQLFilterDlg.ShowModal;
  if f_SQLFilterDlg.ModalResult = mrOK then Result := i_SQLWhereStatement;

  f_SQLFilterDlg.Free;
end;

procedure CenterForm(aForm: TForm);
begin
  aForm.SetBounds
     ((Screen.Width div 2) - (aForm.Width div 2),
     ((100 + Screen.Height) div 2) - (aForm.Height div 2),
     aForm.Width, aForm.Height);
end;

function FillString(StrLength: Integer; Value: Char): String;
var
  TempString: String;
begin
   Result := '';
   if StrLength <= 0 then Exit;
   TempString := String(Value);
   while Length(TempString) < StrLength do TempString := TempString + String(Value);
   Result := TempString;
end;

function FrontFillZero(StrLength: Integer; StrValue: String): String;
var
  TempString: String;
begin
   Result := StrValue;
   if StrLength <= 0 then Exit;
   TempString := StrValue;
   while Length(TempString) < StrLength do TempString := '0' + TempString;
   Result := TempString;
end;

procedure Tf_SQLFilterDlg.FormCreate(Sender: TObject);
begin
  i_OldDataType := 0;
  i_OldFieldType := ftUnknown;
end;

procedure Tf_SQLFilterDlg.lb_FieldsClick(Sender: TObject);
var
  aColumnDataType: Word;
  aFieldType: TFieldType;
begin
  if i_SQLMetaData <> nil then begin
    aColumnDataType := i_SQLMetaData.GetFieldMetaData(i_TableName,
       lb_Fields.Items[lb_Fields.ItemIndex]).ColumnDataType;

    if i_OldDataType <> aColumnDataType then sle_Value.Text := '';

    case aColumnDataType of
      2,12,24: sle_Value.EditMask := '!99/99/0000;1;';
    else
      sle_Value.EditMask := '';
    end;
    i_OldDataType := aColumnDataType;
    end
  else begin
    aFieldType := i_ClientDataSet.FieldDefs.Items[lb_Fields.ItemIndex].DataType;

    if i_OldFieldType <> aFieldType then sle_Value.Text := '';

    case aFieldType of
      ftDate, ftTime, ftDateTime, ftTimeStamp: sle_Value.EditMask := '!99/99/0000;1;';
    else
      sle_Value.EditMask := '';
    end;
    i_OldFieldType := aFieldType;
  end;
end;

procedure Tf_SQLFilterDlg.b_AddClick(Sender: TObject);
var
  aLeftParenthesis, aFieldName, aOperator, aValue,
     aRightParenthesis, aBoolean: String;
  aColumnDataType: Integer;
begin
  if cb_LeftParenthesis.ItemIndex > 0 then begin
    aLeftParenthesis := FillString(cb_LeftParenthesis.ItemIndex, '(');
    end
  else begin
    aLeftParenthesis := '';
  end;

  if lb_Fields.Items.Text > #32 then begin
    aFieldName := lb_Fields.Items[lb_Fields.ItemIndex];
    end
  else begin
    Application.MessageBox('A Field is required. Unable to continue.',
       'Error', MB_OK + MB_ICONERROR);
    lb_Fields.SetFocus;
    Exit;
  end;

  case lb_Operator.ItemIndex of
    0: aOperator := ' = ';
    1: aOperator := ' <> ';
    2: aOperator := ' > ';
    3: aOperator := ' < ';
    4: aOperator := ' >= ';
    5: aOperator := ' <= ';
    6: aOperator := ' LIKE ';
    7: aOperator := ' LIKE ';
    8: aOperator := ' IS NULL ';
    9: aOperator := ' IS NOT NULL ';
  else
    Application.MessageBox('An Operator is required. Unable to continue.',
       'Error', MB_OK + MB_ICONERROR);
    lb_Operator.SetFocus;
    Exit;
  end;

  if i_SQLMetaData <> nil then begin
    aColumnDataType := i_SQLMetaData.GetFieldMetaData(i_TableName, aFieldName).ColumnDataType
    end
  else begin
    if lb_Fields.ItemIndex < 0 then Exit;
    case i_ClientDataSet.FieldDefs.Items[lb_Fields.ItemIndex].DataType of
      ftUnknown: aColumnDataType := 0;
      ftString: aColumnDataType := 1;
      ftFixedChar: aColumnDataType := 1;
      ftWideString: aColumnDataType := 1;
      ftDate: aColumnDataType := 2;
      ftBlob: aColumnDataType := 3;
      ftOraBlob: aColumnDataType := 3;
      ftOraClob: aColumnDataType := 3;
      ftBoolean: aColumnDataType := 4;
      ftWord: aColumnDataType := 5;
      ftSmallint: aColumnDataType := 5;
      ftInteger: aColumnDataType := 6;
      ftFloat: aColumnDataType := 7;
      ftBCD: aColumnDataType := 8;
      ftBytes: aColumnDataType := 9;
      ftTime: aColumnDataType := 10;
      ftTimeStamp: aColumnDataType := 11;
      //  12, 13, 14 Have no exact match.
      ftVarBytes: aColumnDataType := 15;
      ftCursor: aColumnDataType := 17;
      ftLargeint: aColumnDataType := 18;
      ftADT: aColumnDataType := 20;
      ftArray: aColumnDataType := 21;
      ftReference: aColumnDataType := 22;
      ftDateTime: aColumnDataType := 24;
      ftFmtMemo: aColumnDataType := 25;
      ftTypedBinary: aColumnDataType := 26;
    else
      aColumnDataType := 0;
    end;
  end;

  if sle_Value.Text > #32 then begin
    case aColumnDataType of
      0: // UnKnown
        begin
          aValue := 'NULL';
        end;
      1: // String
        begin
          aValue := QuotedStr(sle_Value.Text);
          if lb_Operator.ItemIndex = 6 then aValue := QuotedStr(sle_Value.Text + '%');
          if lb_Operator.ItemIndex = 7 then aValue := QuotedStr('%' + sle_Value.Text + '%');
        end;
      5,6,7,8,12,13,14,18,19,25: // Number
        begin
          aValue := sle_Value.Text;
        end;
      2: // Date
        begin
          aValue := QuotedStr(DateToStr(StrToDate(sle_Value.Text)));
        end;
      10: // Time
        begin
          aValue := QuotedStr(TimeToStr(StrToTime(sle_Value.Text)));
        end;
      11,24: // DateTime
        begin
          aValue := QuotedStr(DateTimeToStr(StrToDateTime(sle_Value.Text)));
          if i_DbType = 'ORACLE' then begin
            aValue := 'TO_DATE(''' + FormatDateTime('dd-mmm-yyyy hh:mm:ss AM/PM', StrToDateTime(sle_Value.Text)) +
               ''', ''DD-MON-YYYY HH:MI:SS AM'') ';
          end;
        end;
    else
      if lb_Operator.ItemIndex <= 7 then aValue := 'NULL';
    end; //  End Case

    end
  else begin
    if lb_Operator.ItemIndex <= 7 then begin
      Application.MessageBox('A Value is required. Unable to continue.',
         'Error', MB_OK + MB_ICONERROR);
      sle_Value.SetFocus;
      Exit;
    end;
  end;

  if cb_RightParenthesis.ItemIndex > 0 then begin
    aRightParenthesis := FillString(cb_RightParenthesis.ItemIndex, ')');
    end
  else begin
    aRightParenthesis := '';
  end;

  if rg_Boolean.ItemIndex > 0 then begin
    aBoolean := ' ' + rg_Boolean.Items[rg_Boolean.ItemIndex] + ' ';
    end
  else begin
    aBoolean := '';
  end;

  m_SQLWhereStatement.Lines.Add(aLeftParenthesis + aFieldName + aOperator +
     aValue + aRightParenthesis + aBoolean);

  // Reset Controls
  sle_Value.Text := '';
  lb_Operator.ItemIndex := 0;
  cb_LeftParenthesis.ItemIndex := 0;
  cb_RightParenthesis.ItemIndex := 0;
  rg_Boolean.ItemIndex := 0;   
end;

procedure Tf_SQLFilterDlg.b_ClearClick(Sender: TObject);
begin
  m_SQLWhereStatement.Lines.Clear;
end;

procedure Tf_SQLFilterDlg.b_ApplyClick(Sender: TObject);
begin
  i_SQLWhereStatement := Trim(m_SQLWhereStatement.Text);
end;

procedure Tf_SQLFilterDlg.lb_OperatorClick(Sender: TObject);
begin
  if  lb_Operator.ItemIndex >= 8 then begin
    sle_Value.Text := '';
    sle_Value.Enabled := False;
    sle_Value.Color := clInactiveCaption;
    end
  else begin
    sle_Value.Enabled := True;
    sle_Value.Color := clWindow;
  end;
end;

end.
