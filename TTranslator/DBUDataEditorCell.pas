{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit DBUDataEditorCell;

interface

{$i common.inc}

uses
  Classes,
  DataElements, DataType, DataTypes,
  DBUCell, DBUFormatter, DBUCellTypes, DBUTypes;

type
  TOnSettingValueEvent = procedure (Sender: TObject; var Value : TValue;
    var Reject, RunInherited : Boolean; var RejectParams : TRejectParams) of object;
//  TOnSettingValueEvent = procedure (Sender: TObject; var Value : TValue;
//    var Reject : Boolean; var RejectReason : String; var RunInherited : Boolean) of object;
  TOnSetValueEvent = procedure (Sender: TObject; SetResult : TSetResult) of object;
  TOnOverWritingRowEvent = procedure (Sender: TObject; ADataRow : TAbstractRow; var OverWrite : Boolean) of object;

  IDataEditorCellInterface = interface
    ['{ED988C72-2F32-11D5-A794-00104B479B55}']

    function GetDBUFormatter : TDBUFormatter;
    function GetCol: Integer;
    function GetRow: Integer;

    function GetDataRow : TAbstractRow;
    function GetDataField : TDataField;
    function GetReadOnly: Boolean;
    function GetCellType : TDBUCellType;
    function GetValue : TValue;
    function ActiveParams : TDrawParams;
    function GetForEditor: Boolean;
    function GetKeyFormatting : TDisplayValues;

    procedure SetDataRow( const Value : TAbstractRow );
    procedure SetDataField( const Value : TDataField );
    procedure SetReadOnly( const Value: Boolean );
    procedure SetCellType( const Value: TDBUCellType );
    procedure SetKeyFormatting( const Value : TDisplayValues );

    property Col : Integer read GetCol;
    property Row : Integer read GetRow;
    property DataField : TDataField read GetDataField write SetDataField;
    property DataRow : TAbstractRow read GetDataRow write SetDataRow;
    property ReadOnly : Boolean read GetReadOnly write SetReadOnly;
    property CellType : TDBUCellType read GetCellType write SetCellType;
    property Value : TValue read GetValue;
    property ForEditor : Boolean read GetForEditor;
    property KeyFormatting : TDisplayValues read GetKeyFormatting write SetKeyFormatting;
  end;

  TOnDefineCellEvent = procedure ( Sender : IDataEditorCellInterface ) of object;

  TDataEditorCell = class(TDBUCell, IDataEditorCellInterface)
  private
    FDataRow: TAbstractRow;
    FDataField: TDataField;
    FKeyFormatting: TDisplayValues;
    FOnDefineCell : TOnDefineCellEvent;
    FActiveParams : TDrawParams;

    function ActiveParams : TDrawParams;
    function GetDBUFormatter : TDBUFormatter;
    function GetDataRow : TAbstractRow;
    function GetDataField : TDataField;
    function GetCellType : TDBUCellType;
    function GetForEditor: Boolean;
    function GetKeyFormatting : TDisplayValues;
    procedure SetDataRow( const Value : TAbstractRow );
    procedure SetDataField( const Value : TDataField );
    procedure SetOnDefineCell(const Value: TOnDefineCellEvent);
    procedure SetCellType( const Value: TDBUCellType );
    procedure SetKeyFormatting(const Value: TDisplayValues);
  protected
    function GetValue : TValue;
    function GetDataType : TDataType; override;
    function DoGetValue : TValue; override;
    procedure DrawCell( var Params : TDrawParams ); override;
    procedure GetCell(ACol, ARow: Integer); override;
    procedure DefineCellType; override;
    procedure InitIterator( Iterator : TDBUCell ); override;

    function GetCustomizerIdentifier : Pointer; override;
    function GetDrawValue : TValue; override;
  public
    constructor Create; override;

    property DataField : TDataField read GetDataField write SetDataField;
    property DataRow : TAbstractRow read GetDataRow write SetDataRow;
    property CellType : TDBUCellType read GetCellType write SetCellType;
    property KeyFormatting : TDisplayValues read GetKeyFormatting write SetKeyFormatting;

    property OnDefineCell : TOnDefineCellEvent read FOnDefineCell write SetOnDefineCell;
  end;

  TDataInplaceEditorCell = class(TDBUEditorCell, IDataEditorCellInterface)
  private
    FDataRow: TAbstractRow;
    FDataField: TDataField;
    FKeyFormatting: TDisplayValues;
    FActiveParams : TDrawParams;

    FOnSettingValue: TOnSettingValueEvent;
    FOnSetValue: TOnSetValueEvent;
    FOnDefineCell: TOnDefineCellEvent;
    FOnOverWritingRow : TOnOverWritingRowEvent;
    FOnOverWritedRow : TNotifyEvent;

    function ActiveParams : TDrawParams;
    function GetDBUFormatter : TDBUFormatter;
    function GetDataRow : TAbstractRow;
    function GetDataField : TDataField;
    function GetCellType : TDBUCellType;
    function GetForEditor: Boolean;
    procedure SetDataRow( const Value : TAbstractRow );
    procedure SetDataField( const Value : TDataField );
    procedure SetOnDefineCell(const Value: TOnDefineCellEvent);
    procedure SetCellType( const Value: TDBUCellType );
    function SetValueToDataRow(const AValue: TValue;
      OverWrite: Boolean): TSetResult;
    procedure SetOnOverWritingRow(
      const Value: TOnOverWritingRowEvent);

  protected
    function GetValue : TValue;
    function GetDataType : TDataType; override;
    function DoGetValue : TValue; override;
    function DoSetValue(const AValue : TValue; var RejectParams : TRejectParams) : Boolean; override;
    function ValueChanged( aValue : TValue ) : Boolean; override;
    function KeyValueChanged( aValue : TValue): Boolean;
    function CalcFormula(var Value, ErrorString: String): Boolean;
    function SetCell : Boolean; override;
    function GetKeyFormatting : TDisplayValues;

    procedure DefineCellType; override;
    procedure DrawEditor( var Params : TDrawParams ); override;
    procedure InitIterator( Iterator : TDBUEditorCell ); override;
    procedure SetEditorValue( const AValue : TValue ); override;
    procedure SetKeyFormatting( const Value : TDisplayValues );
    function GetEditorKeyValue : TValue; override;
    function DoGetEditText : String; override;

    function GetCustomizerIdentifier : Pointer; override;
    function GetDrawValue : TValue; override;
  public
    constructor Create; override;

    property DataField : TDataField read GetDataField write SetDataField;
    property DataRow : TAbstractRow read GetDataRow write SetDataRow;
    property CellType : TDBUCellType read GetCellType write SetCellType;
    property KeyFormatting : TDisplayValues read GetKeyFormatting write SetKeyFormatting;

    property OnSetValue : TOnSetValueEvent read fOnSetValue write fOnSetValue;
    property OnSettingValue : TOnSettingValueEvent read FOnSettingValue write FOnSettingValue;
    property OnDefineCell : TOnDefineCellEvent read FOnDefineCell write SetOnDefineCell;
    property OnOverWritingRow : TOnOverWritingRowEvent read FOnOverWritingRow write SetOnOverWritingRow;
    property OnOverWritedRow : TNotifyEvent read FOnOverWritedRow write FOnOverWritedRow;
  end;

  TDataEditorCellFormatter = class(TDBUFormatter)
  public
    function FormatValue(SrcValue: TValue; ADataType: TDataType): TValue; override;
    function ParseValue(SrcValue: TValue; var ParsedValue: TValue;
      ADataType: TDataType): Boolean; override;
  end;

implementation

uses
{$ifndef LINUX}
  Dialogs, Controls,
{$else LINUX}
  QDialogs, QControls,
{$endif LINUX}

  SysUtils, DataTranslations,
  Criteria, DataEditorConstants, DataEditorLib, DBUGrid;
  
{ TDataEditorCellFormatter }

function TDataEditorCellFormatter.FormatValue(SrcValue: TValue;
  ADataType: TDataType): TValue;
begin
  Result := inherited FormatValue( SrcValue, ADataType ) ;
end;

function TDataEditorCellFormatter.ParseValue(SrcValue: TValue;
  var ParsedValue: TValue; ADataType: TDataType): Boolean;
begin
  Result := inherited ParseValue( SrcValue, ParsedValue, ADataType );
end;

{ TDataEditorCell }

constructor TDataEditorCell.Create;
begin
  inherited;
  KeyFormatting := dvDefault;
end;

function TDataEditorCell.ActiveParams : TDrawParams;
begin
  Result := FActiveParams;
end;

procedure TDataEditorCell.DefineCellType;
begin
  // nothing
end;

procedure TDataEditorCell.GetCell(ACol, ARow: Integer);
begin
  inherited;

  if (Col < 0) or (Row < 0) then
    CellType := nil
  else
  begin
    if Assigned( OnDefineCell ) then
      OnDefineCell( Self )
    else
      inherited DefineCellType;
  end;
end;

function TDataEditorCell.DoGetValue: TValue;
begin
  if Assigned( DataField ) then
    Result := DataRow[DataField]
    //GetFieldValue( DataField, DataRow, Result, KeyFormatting )
  else
    Result := inherited DoGetValue;
end;

procedure TDataEditorCell.DrawCell(var Params: TDrawParams);
begin
  FActiveParams := Params;
  inherited;
end;

function TDataEditorCell.GetCellType: TDBUCellType;
begin
  Result := FCellType;  
end;

function TDataEditorCell.GetDataField: TDataField;
begin
  Result := FDataField;
end;

function TDataEditorCell.GetDataRow: TAbstractRow;
begin
  Result := FDataRow;
end;

function TDataEditorCell.GetDBUFormatter: TDBUFormatter;
begin
  Result := Formatter;
end;

function TDataEditorCell.GetForEditor: Boolean;
begin
  Result := False;
end;

function TDataEditorCell.GetKeyFormatting: TDisplayValues;
begin
  Result := FKeyFormatting;
end;

procedure TDataEditorCell.SetKeyFormatting( const Value: TDisplayValues );
begin
  FKeyFormatting := Value;
  ResetValues;
end;

procedure TDataEditorCell.InitIterator(Iterator: TDBUCell);
var
  c : TDataEditorCell;
begin
  inherited;
  c := Iterator as TDataEditorCell;
  c.FOnDefineCell := FOnDefineCell;
end;

procedure TDataEditorCell.SetCellType(const Value: TDBUCellType);
begin
  FCellType := Value;
end;

procedure TDataEditorCell.SetDataField(const Value: TDataField);
begin
  FDataField := Value;
end;

procedure TDataEditorCell.SetDataRow(const Value: TAbstractRow);
begin
  FDataRow := Value;
end;

procedure TDataEditorCell.SetOnDefineCell(
  const Value: TOnDefineCellEvent);
begin
  FOnDefineCell := Value;
end;

function TDataEditorCell.GetDataType: TDataType;
begin
  // The FDataType field is initialized in GetValue
  Value;
  if not Assigned(FDataType) then
    FDataType := DataField.DataType;

  Result := inherited GetDataType;
end;

function TDataEditorCell.GetValue: TValue;
begin
  Result := Value;
end;

function TDataEditorCell.GetDrawValue: TValue;
begin
  Result := inherited GetDrawValue;
// Non-numeric fields sometimes require DisplayString
(* if  ( not DataField.DataType.IsNumeric or
       (Assigned(DataRow) and DataRow.DataTable.TableHasKey(DataField)) ) and
       *)
  if ( not (DataField.DataType is TPointerType) ) and
     ( (KeyFormatting in [dvKeyAndText, dvTextOnly]) or
       (DataField.DisplayValues <> dvKeyOnly) ) then
    Result := ValueFromString(DataRow.DisplayString[DataField, KeyFormatting]);
end;

function TDataEditorCell.GetCustomizerIdentifier: Pointer;
begin
  Result := Pointer(Integer(inherited GetCustomizerIdentifier) - Integer(DataType));
end;

{ TDataInplaceEditorCell }

constructor TDataInplaceEditorCell.Create;
begin
  inherited;
  KeyFormatting := dvDefault;
end;

function TDataInplaceEditorCell.ActiveParams : TDrawParams;
begin
  Result := FActiveParams;
end;

procedure TDataInplaceEditorCell.DefineCellType;
begin
  if Assigned( OnDefineCell ) then
    OnDefineCell( Self )
  else
    inherited;
end;

function TDataInplaceEditorCell.DoGetValue: TValue;
begin
  if Assigned( DataField ) then
    Result := DataRow[DataField]
//     GetFieldValue( DataField, DataRow, Result, KeyFormatting )
  else
    Result := EmptyString;
end;

function TDataInplaceEditorCell.SetValueToDataRow( const AValue : TValue;
  OverWrite : Boolean ) : TSetResult;
begin
  if not OverWrite then
    Result := DataRow.SetFieldValue( DataField, AValue, saDontOverwriteKeys )
  else
    Result := DataRow.SetFieldValue( DataField, AValue, saOverWriteOnKeyChange );
end;

function TDataInplaceEditorCell.SetCell : Boolean;
var
  NewValue : TValue;
  AStr, Error : String;
  RejectParams : TRejectParams;
begin
  Result := True;
  if Modified and
     not ReadOnly and
     ( DataType <> nil ) and
     DataType.IsNumeric then
  begin
    NewValue := EditorValue;
    AStr := AsString( EditorValue );
    if CalcFormula( AStr, Error ) then
    begin
      EditorValue := ValueFromString( AStr );
      Modified := True;
    end
    else
    begin
      RejectParams.ShowDialog := True;
      RejectParams.AllowRestore := True;
      RejectParams.Msg := Error;
      RejectParams.YesNoChoise := not Killing;
      if Killing then
        RejectParams.Msg := RejectParams.Msg + #13#10 + 'Restoring old value!'
      else
        RejectParams.Msg := RejectParams.Msg + #13#10 + 'Restore old value!';

      HandleSetCellError(RejectParams);
//      ShowSetCellErrorAndRestore( error, Killing );
      Result := False;
    end;
  end;

  if Result then
    Result := inherited SetCell
  else
    EditorValue := NewValue;
end;

function TDataInplaceEditorCell.DoSetValue(const AValue : TValue; var RejectParams : TRejectParams) : Boolean;
var
  SetResult : TSetResult;
  ACrit : TCriteria;
  Value : TValue;
  Reject, RunInherited, CanOverWriteRow : Boolean;
  Msg : String;
  ARow : TAbstractRow;
begin
  if Assigned( DataField ) and Assigned( DataRow ) then
  begin
    Value := AValue;
    Reject := False;
    RunInherited := True;

    if Assigned( OnSettingValue ) then
      OnSettingValue(Self, Value, Reject, RunInherited, RejectParams);
//      OnSettingValue(Self, Value, Reject, RejectReason, RunInherited );

    if Reject then
    begin
      SetResult := srInvalidValue;
(*      if RejectReason = '' then
        RejectReason := 'You can not change this value!';
      rejectreason := rejectreason + ' Restore old value?';

      if MessageDlg( rejectreason, mtError, [mbYes, mbNo], 0 ) = mrYes then
        UndoChanges;
        *)
    end
    else if not RunInherited then
      SetResult := srOk
    else
    begin
      SetResult := SetValueToDataRow( Value, False );
      if SetResult = srKeyConflict then
      begin
        ACrit := TCriteria.CreateFromRowKeys(DataRow);
        ACrit[DataField].Reset;
        ACrit[DataField].AddValue(Value);

        ARow := DataRow.Storage.LocateRowByCriteria(ACrit);
        ACrit.Free;

        CanOverWriteRow := False;
        if Assigned(OnOverWritingRow) then
          OnOverWritingRow(Self, ARow, CanOverWriteRow);

        Msg := TranslateMessage(I_KeyCombinationExists) + ' ' + TranslateMessage(Q_OverWriteOld);
        if CanOverWriteRow and
           ( TranslateMessageDlg(Msg, mtConfirmation, mbOKCancel, -1) = mrOK ) then
        begin
          SetResult := SetValueToDataRow( Value, True );
          if not (SetResult in IllegalSetResults) and
             Assigned(OnOverWritedRow) then
            OnOverWritedRow(Self);
        end
        else
        begin
          RejectParams.Msg := TranslateMessage(I_KeyCombinationExists) + ' ' + TranslateMessage(I_OldRowCanNotBeOverwritten) + #13#10 +
                              'Restore old value?';
//          ShowMessage(TranslateMessage(I_KeyCombinationExists) + ' ' + TranslateMessage(I_OldRowCanNotBeOverwritten);
        end;
      end;
    end;

    if Assigned( OnSetValue ) then
      OnSetValue( Self, SetResult );
    Result := not (SetResult in IllegalSetResults);
  end
  else
    Result := inherited DoSetValue(Value, RejectParams);
end;

procedure TDataInplaceEditorCell.DrawEditor(var Params: TDrawParams);
begin
  FActiveParams := Params;
  inherited;
end;

function TDataInplaceEditorCell.GetCellType: TDBUCellType;
begin
  Result := FCellType;
end;

function TDataInplaceEditorCell.GetDataField: TDataField;
begin
  Result := FDataField;
end;

function TDataInplaceEditorCell.GetDataRow: TAbstractRow;
begin
  Result := FDataRow;
end;

function TDataInplaceEditorCell.GetDBUFormatter: TDBUFormatter;
begin
  Result := Formatter;
end;

procedure TDataInplaceEditorCell.SetCellType(const Value: TDBUCellType);
begin
  FCellType :=  Value;
end;

procedure TDataInplaceEditorCell.SetDataField(const Value: TDataField);
begin
  FDataField := Value;
end;

procedure TDataInplaceEditorCell.SetDataRow(const Value: TAbstractRow);
begin
  FDataRow := Value;
end;

procedure TDataInplaceEditorCell.SetOnDefineCell(
  const Value: TOnDefineCellEvent);
begin
  FOnDefineCell := Value;
end;

procedure TDataInplaceEditorCell.SetOnOverWritingRow(
  const Value: TOnOverWritingRowEvent);
begin
  FOnOverWritingRow := Value;
end;

function TDataInplaceEditorCell.GetForEditor: Boolean;
begin
  Result := True;
end;

function TDataInplaceEditorCell.GetKeyFormatting: TDisplayValues;
begin
  Result := FKeyFormatting;
end;

procedure TDataInplaceEditorCell.SetKeyFormatting(
  const Value: TDisplayValues);
begin
  FKeyFormatting := Value;
end;

procedure TDataInplaceEditorCell.InitIterator( Iterator : TDBUEditorCell );
var
  c : TDataInplaceEditorCell;
begin
  inherited;
  c := Iterator as TDataInplaceEditorCell;
  c.FOnSettingValue   := FOnSettingValue;
  c.FOnSetValue       := fOnSetValue;
  c.FOnDefineCell     := FOnDefineCell;
  c.FOnOverWritingRow := FOnOverWritingRow;
  c.FOnOverWritedRow  := FOnOverWritedRow;
end;

procedure TDataInplaceEditorCell.SetEditorValue( const AValue : TValue );
begin
  inherited;
end;

function TDataInplaceEditorCell.ValueChanged( aValue: TValue ): Boolean;
begin
  Result := inherited ValueChanged( aValue );

  if Result and
     Assigned(DataRow) and
     RowHasKey(DataRow, DataField) then
    Result := KeyValueChanged(aValue);
end;

function TDataInplaceEditorCell.KeyValueChanged(aValue: TValue): Boolean;
begin
  Result := not CompareFieldValue( DataRow, DataField, MemoTypeCS, aValue, dvKeyOnly );
end;

function TDataInplaceEditorCell.CalcFormula(var Value, ErrorString : String) : Boolean;

begin
  Result := True;

end;

function TDataInplaceEditorCell.GetDataType: TDataType;
begin
  // The FDataType field is initialized in GetValue
  Value;
  if not Assigned(FDataType) then
    FDataType := DataField.DataType;

  Result := inherited GetDataType;
end;

function TDataInplaceEditorCell.GetValue: TValue;
begin
  Result := Value;
end;

function TDataInplaceEditorCell.GetEditorKeyValue: TValue;
begin
  if Modified then
    Result := inherited GetEditorKeyValue
  else
    Result := DataRow[DataField];
end;

function TDataInplaceEditorCell.GetDrawValue: TValue;
begin
  Result := inherited GetDrawValue;
// Non-numeric fields sometimes require DisplayString
(* if  ( not DataField.DataType.IsNumeric or
       (Assigned(DataRow) and DataRow.DataTable.TableHasKey(DataField)) ) and
       *)
  if ( not (DataField.DataType is TPointerType) ) and
     ( (KeyFormatting in [dvKeyAndText, dvTextOnly]) or
       (DataField.DisplayValues <> dvKeyOnly) ) then
    Result := ValueFromString(DataRow.DisplayString[DataField, KeyFormatting]);
(*  if (KeyFormatting in [dvKeyAndText, dvTextOnly]) or
     (DataField.DisplayValues <> dvKeyOnly) then*)
end;

function TDataInplaceEditorCell.DoGetEditText : String;
begin
  Result := inherited DoGetEditText;
// Non-numeric fields sometimes require DisplayString
(* if  ( not DataField.DataType.IsNumeric or
       (Assigned(DataRow) and DataRow.DataTable.TableHasKey(DataField)) ) and
       *)
  if not Modified and
     not (DataField.DataType is TPointerType) and
     ( (KeyFormatting in [dvKeyAndText, dvTextOnly]) or
       (DataField.DisplayValues <> dvKeyOnly) ) then
   Result := DataRow.DisplayString[DataField, KeyFormatting];

(*  if (KeyFormatting in [dvKeyAndText, dvTextOnly]) or
     (DataField.DisplayValues <> dvKeyOnly) then       *)
end;

function TDataInplaceEditorCell.GetCustomizerIdentifier: Pointer;
begin
  Result := Pointer(Integer(inherited GetCustomizerIdentifier) - Integer(DataType));
end;

end.

