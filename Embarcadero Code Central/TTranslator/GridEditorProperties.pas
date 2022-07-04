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

{ $Id: GridEditorProperties.pas,v 1.57 2003/03/04 07:28:13 lge Exp $ }

{  What:              Properties for defineing properites
                     of Fields and DataRows in a GridEditor
  Version
  Company:           Polycon Ab
  Authors:           MVJ, LGE
}

unit GridEditorProperties;

interface

{$i common.inc}

uses
  RowList, DataElements, DataTypes, DataType, Classes, CommonLib

  ;

type
  TGridEditorDefaultValue = class;
  TGridEditorReadOnly = class;
  TGridEditorComboValues = class;

  TGridEditorProperties = class
  private
    FDataField : TDataField;
    FDependFields : TFieldList;

    function GetDataField : TDataField;

  public
    constructor Create(aDataField : TDataField);
    destructor Destroy; override;

    property DataField : TDataField read GetDataField;
    property DependFields : TFieldList read FDependFields;

    class function GetPropertyObject(AList : TValueList; aDataField : TDataField) : TGridEditorProperties;
    class function GetReadOnlyObject(AList : TValueList; aDataField : TDataField) : TGridEditorReadOnly;
    class function GetDefaultValueObject(AList : TValueList; aDataField : TDataField) : TGridEditorDefaultValue;
    class function GetComboValuesObject(AList : TValueList; aDataField : TDataField) : TGridEditorComboValues;
    class procedure AddToList(AList : TValueList; AProperty : TGridEditorProperties);
  end;

  TGridEditorDefaultValue = class(TGridEditorProperties)
  protected
    FChangeValueOnKeyChange : Boolean;
    function GetChangeValueOnKeyChange : Boolean; virtual;
    procedure SetChangeValueOnKeyChange(Value : Boolean); virtual;
  public
    constructor Create(aDataField : TDataField);
    function GetDefaultValue(Row : TAbstractRow) : TValue; virtual; abstract;
    function ValueLegal( Row : TAbstractRow ) : Boolean; virtual;

    property ChangeValueOnKeyChange : Boolean read GetChangeValueOnKeyChange write SetChangeValueOnKeyChange;
  end;

  TGridEditorReadOnly = class(TGridEditorProperties)
  private
    FIsAbsolute : Boolean;
  public
    constructor Create(aDataField : TDataField; IsAbsolute : Boolean);
    function GetIsReadOnly(Row : TAbstractRow) : Boolean; virtual; abstract;
    property IsAbsolute : Boolean read FIsAbsolute;
  end;

  TGridEditorComboValues = class(TGridEditorProperties)
  private
    FSingleton : Boolean;
  protected
    FIndependentStrings : TDataRowList;

    function DoCreateCopy : TGridEditorComboValues; virtual; abstract;
    procedure FilterComboValues(var Strings : TDataRowList; Row : TAbstractRow); virtual; abstract;
    procedure GetComboStrings(var Strings : TDataRowList; Criteria : TCondition; Row : TAbstractRow); virtual;
    procedure DefaultComboStrings(var Strings: TDataRowList;
      Criteria: TCondition; Row: TAbstractRow);
  public
    constructor Create(aKeyField : TDataField);
    destructor Destroy; override;

    function FilterResultDependsOnRow : Boolean; virtual; abstract;
    function CreateCopy : TGridEditorComboValues;
    procedure GetStrings(Strings : TStrings; Criteria : TCondition; Row : TAbstractRow); virtual;
  end;

  // icke-abstracta klasser

  TAlwaysReadOnly = class(TGridEditorReadOnly)
  public
    function GetIsReadOnly(Row : TAbstractRow) : Boolean; override;
  end;

  TReadOnlyWhenAccepted = class (TGridEditorReadOnly)
  public
    function GetIsReadOnly(Row : TAbstractRow) : Boolean; override;
  end;

  { Defines a constant value for a DataField. Used for SOURCE in BUCOST }
  TConstantDefaultValue = class(TGridEditorDefaultValue)
  private
    FConst : TValue;
  public
    constructor Create(aDataField : TDataField; AConst : TValue);
    function GetDefaultValue(Row : TAbstractRow) : TValue; override;
  end;

  TConditionalReadOnly = class(TGridEditorReadOnly)
  private
    FConditionField : TDataField;
    FValueLegal : Boolean;
  public
    constructor Create(aDataField, ConditionField : TDataField; ConditionValueLegal, IsAbsolute : Boolean);
    function GetIsReadOnly(Row : TAbstractRow) : Boolean;  override;
  end;

  TConditionalDefaultValue = class(TGridEditorDefaultValue)
  private
    FConst : TValue;
    FConditionField : TDataField;
    FValueLegal : Boolean;
  protected
    procedure GetLegalValues( Row : TAbstractRow; Strings : TDataRowList ); virtual;
  public
    constructor Create(aDataField, ConditionField : TDataField; AConst : TValue; ConditionValueLegal : Boolean);
    function GetDefaultValue(Row : TAbstractRow) : TValue; override;
    function ValueLegal( Row : TAbstractRow ) : Boolean; override;
  end;

  TConditionalComboValues = class(TGridEditorComboValues)
  private
    FConditionField : TDataField;
    FDependentResult : Boolean;
    FValueLegal : Boolean;
  protected
    function DoCreateCopy : TGridEditorComboValues; override;
    procedure FilterComboValues(var Strings : TDataRowList; Row : TAbstractRow); override;
  public
    constructor Create(aKeyField : TDataField; ConditionField : TDataField;
                ConditionValueLegal, FilterResultDependsOnRow : Boolean);
    destructor Destroy; override;

    function FilterResultDependsOnRow : Boolean; override;
  end;

  TConditionValuesEqualComboValues = class(TGridEditorComboValues)
  private
    FActiveRowConditionField : TDataField;
    FComboRowConditionField : TDataField;
    FDependentResult : Boolean;
  protected
    function DoCreateCopy : TGridEditorComboValues; override;
    procedure FilterComboValues(var Strings : TDataRowList; Row : TAbstractRow); override;
  public
    constructor Create(aKeyField : TDataField; ActiveRowConditionField, ComboRowConditionField : TDataField;
                FilterResultDependsOnRow : Boolean);

    function FilterResultDependsOnRow : Boolean; override;
  end;

  TTableDependentComboValues = class(TGridEditorComboValues)
  private
    FTable : TAuxTable;
    FConditionField : TDataField;
    function RemoveConditionFieldSelections(Field: TDataField;
      var ConflictAction: TConflictAction;
      var KeepSrc: Boolean): TDataField;
  protected
    FDependentResult : Boolean;
    function DoCreateCopy : TGridEditorComboValues; override;
    function CreateLoadCondition(Row: TAbstractRow): TCondition; virtual;
    procedure FillKeyAndDescription( Strings : TDataRowList ); virtual;
    procedure FilterComboValues(var Strings : TDataRowList; Row : TAbstractRow); override;
    procedure GetComboStrings(var Strings : TDataRowList; Criteria : TCondition; Row : TAbstractRow); override;

    property Table : TAuxTable read FTable;
    property ConditionField : TDataField read FConditionField;
    property DependentResult : Boolean read FDependentResult;
  public
    constructor Create(aKeyField : TDataField; CompField : TDataField; ATable : TAuxTable;
                FilterResultDependsOnRow : Boolean);
    destructor Destroy; override;

    function FilterResultDependsOnRow : Boolean; override;
  end;

  TTableDependentConditionalDefaultValue = class(TConditionalDefaultValue)
  private
    FTable : TAuxTable;
    FComparisonField : TDataField;
  protected
    procedure GetLegalValues( Row : TAbstractRow;  Strings : TDataRowList ); override;
  public
    constructor Create(aDataField, CompField, ConditionField : TDataField; ATable : TAuxTable;
        AConst : TValue; ConditionValueLegal : Boolean);

    function ValueLegal( Row : TAbstractRow ) : Boolean; override;
  end;

  TAddValuesComboValues = class(TGridEditorComboValues)
  private
    FValueRows : TDataRowList;
  protected
    function DoCreateCopy : TGridEditorComboValues; override;
    procedure FilterComboValues(var Strings : TDataRowList; Row : TAbstractRow); override;
    procedure GetComboStrings(var Strings : TDataRowList; Criteria : TCondition; Row : TAbstractRow); override;
    procedure PutRowsToList(AList : TDataRowList; Criteria : TCondition; ARow : TAbstractRow);

    property ValueRows : TDataRowList read FValueRows;
  public
    constructor Create(aKeyField : TDataField; ValueRowsArray : array of TAbstractRow);
    destructor Destroy; override;

    function FilterResultDependsOnRow : Boolean; override;
  end;

implementation

uses
{$ifndef LINUX}
  Dialogs,
{$endif LINUX}
  SysUtils,
  Storages, Criteria;

const
  MSGE_NoDefaultForThis = 'You can not get any default value for this';
  MSGE_NoDefaultValueForAbstractRowStorage = MSGE_NoDefaultForThis + ' AbstactRowStorage!';

{ TGridEditorProperties }

constructor TGridEditorProperties.Create(aDataField : TDataField);
begin
  inherited Create;
  FDataField := aDataField;
  FDependFields := TFieldList.Create;
  FDependFields.Duplicates := dupIgnore;
end;

destructor TGridEditorProperties.Destroy;
begin
  inherited Destroy;

  FDependFields.Free;
end;

function TGridEditorProperties.GetDataField : TDataField;
begin
  Result := FDataField;
end;



class function TGridEditorProperties.GetPropertyObject(AList : TValueList;
      aDataField : TDataField) : TGridEditorProperties;
var
  idx : Integer;
begin
  idx := AList.IndexOfValue(ValueFromObject(aDataField));

  if idx >= 0 then
    Result := TGridEditorProperties(AList.Objects[idx])
  else
    Result := nil;
end;

class function TGridEditorProperties.GetReadOnlyObject(AList : TValueList;
      aDataField : TDataField) : TGridEditorReadOnly;
var
  aProperty : TGridEditorProperties;
begin
  Result := nil;
  aProperty := TGridEditorProperties.GetPropertyObject(AList, aDataField);
  if aProperty is TGridEditorReadOnly then
    Result := TGridEditorReadOnly(aProperty);
end;

class function TGridEditorProperties.GetDefaultValueObject(AList : TValueList;
      aDataField : TDataField) : TGridEditorDefaultValue;
var
  aProperty : TGridEditorProperties;
begin
  Result := nil;
  aProperty := TGridEditorProperties.GetPropertyObject(AList, aDataField);
  if aProperty is TGridEditorDefaultValue then
    Result := TGridEditorDefaultValue(aProperty);
end;

class function TGridEditorProperties.GetComboValuesObject(AList : TValueList;
      aDataField : TDataField) : TGridEditorComboValues;
var
  aProperty : TGridEditorProperties;
begin
  Result := nil;
  aProperty := TGridEditorProperties.GetPropertyObject(AList, aDataField);
  if aProperty is TGridEditorComboValues then
    Result := TGridEditorComboValues(aProperty);
end;

class procedure TGridEditorProperties.AddToList(AList : TValueList; AProperty : TGridEditorProperties);
begin
  AList.Duplicates := dupError;
  if not AList.Sorted then
    AList.Sorted := True;

  AList.AddValue('', ValueFromObject(AProperty.FDataField), AProperty);
end;

{ TGridEditorDefaultValue }

constructor TGridEditorDefaultValue.Create(aDataField : TDataField);
begin
  Inherited Create(aDataField);
  FChangeValueOnKeyChange := False;
end;

function TGridEditorDefaultValue.GetChangeValueOnKeyChange : Boolean;
begin
  Result := FChangeValueOnKeyChange;
end;

procedure TGridEditorDefaultValue.SetChangeValueOnKeyChange(Value : Boolean);
begin
  FChangeValueOnKeyChange := Value;
end;

function TGridEditorDefaultValue.ValueLegal(Row: TAbstractRow): Boolean;
begin
  Result := False;
end;

{ TGridEditorReadOnly }

constructor TGridEditorReadOnly.Create(aDataField : TDataField; IsAbsolute : Boolean);
begin
  Inherited Create(aDataField);
  FIsAbsolute := IsAbsolute;
end;

{ TGridEditorComboValues }

constructor TGridEditorComboValues.Create(aKeyField : TDataField);
begin
  inherited Create(aKeyField);
  FIndependentStrings := TDataRowList.Create;
  FSingleton := True;
end;

destructor TGridEditorComboValues.Destroy;
begin
  FIndependentStrings.Free;
  inherited Destroy;
end;

function TGridEditorComboValues.CreateCopy: TGridEditorComboValues;
begin
  Result := DoCreateCopy;
  Result.FSingleton := False;
end;

procedure TGridEditorComboValues.GetStrings(Strings : TStrings; Criteria : TCondition; Row : TAbstractRow);
begin
  if FSingleton then
    raise Exception.Create( Self.ClassName + '.GetStrings: Don''t run this procedure for a singleton!' )
  else if Strings = nil then
    Exit
  else if FilterResultDependsOnRow or (FIndependentStrings.Count = 0) then
  begin
    GetComboStrings(FIndependentStrings, Criteria, Row);
    FilterComboValues(FIndependentStrings, Row);
  end;
//  Result := FIndependentStrings;
  Strings.Assign( FIndependentStrings );
end;

procedure TGridEditorComboValues.DefaultComboStrings(var Strings : TDataRowList; Criteria : TCondition; Row : TAbstractRow);
begin
  Strings.Clear;
  FDataField.GetValues( Strings, Row.DataTable, Row, Criteria );
  Strings.FillStringsWithKeyAndDescription(FDataField);
end;

procedure TGridEditorComboValues.GetComboStrings(var Strings : TDataRowList; Criteria : TCondition; Row : TAbstractRow);
begin
  DefaultComboStrings(Strings, Criteria, Row);
end;

{ non-abstract classes }


{ TConstantDefaultValue }

constructor TConstantDefaultValue.Create(aDataField : TDataField; AConst : TValue);
begin
  inherited Create(aDataField);
  FConst := AConst;
end;

function TConstantDefaultValue.GetDefaultValue(Row : TAbstractRow) : TValue;
begin
  Result := FConst;
end;

{ TAlwaysReadOnly }

function TAlwaysReadOnly.GetIsReadOnly(Row : TAbstractRow) : Boolean;
begin
  Result := True;
end;

{ TReadOnlyWhenAccepted }

function TReadOnlyWhenAccepted.GetIsReadOnly(Row : TAbstractRow) : Boolean;
begin
  Result := not Row.Storage.RowIsUnaccepted(Row);
end;

{ TConditionalReadOnly }

constructor TConditionalReadOnly.Create(aDataField, ConditionField : TDataField; ConditionValueLegal, IsAbsolute : Boolean);
begin
  inherited Create(aDataField, IsAbsolute);
  FConditionField := ConditionField;
  FValueLegal := ConditionValueLegal;
end;

function TConditionalReadOnly.GetIsReadOnly(Row : TAbstractRow) : Boolean;
begin
  if (FConditionField.LookupTable <> nil) and
     (FConditionField.LookupTable.Cache.LocateByRowValues(Row, [nil]) = nil) then
    Result := False
  else
    Result := (FValueLegal = Row.BooleanValue[FConditionField]);
end;

{ TConditionalDefaultValue }

constructor TConditionalDefaultValue.Create(aDataField, ConditionField : TDataField;
            AConst : TValue; ConditionValueLegal : Boolean);
begin
  inherited Create(aDataField);
  FConst := AConst;
  FConditionField := ConditionField;
  FValueLegal := ConditionValueLegal;
end;

function TConditionalDefaultValue.GetDefaultValue(Row : TAbstractRow) : TValue;
var
  Strings : TDataRowList;
begin
  if FValueLegal = Row.BooleanValue[FConditionField] then
    Result := FConst
  else if (FDataField is TDataField) then
  begin
    if (Row.Storage is TCustomRowStorage) then
    begin
      Strings := TDataRowList.Create;
      GetLegalValues( Row, Strings );

      if Strings.Count > 0 then
        Result := Strings[0][DataField]
      else
        Result := Row[DataField];
    end
    else
      raise Exception.Create('TConditionalDefaultValue.GetDefaultValue: ' + MSGE_NoDefaultValueForAbstractRowStorage);
  end;
end;

function TConditionalDefaultValue.ValueLegal( Row : TAbstractRow ) : Boolean;
begin
  Result := DataField.DataType.Equals( Row[DataField], FConst ) =
            (FValueLegal = Row.BooleanValue[FConditionField] );

end;

procedure TConditionalDefaultValue.GetLegalValues( Row : TAbstractRow; Strings : TDataRowList );
var
  ARowCond : TCondition;
begin
  ARowCond := TCriteria.CreateFromRowKeys( Row );
  DataField.GetRows( Strings, ARowCond );
  ARowCond.Free;
end;

{ TConditionalComboValues }

constructor TConditionalComboValues.Create(aKeyField : TDataField; ConditionField : TDataField;
            ConditionValueLegal, FilterResultDependsOnRow : Boolean);
begin
  inherited Create(aKeyField);
  FDependentResult := FilterResultDependsOnRow;
  FConditionField := ConditionField;
  FValueLegal := ConditionValueLegal;
end;

destructor TConditionalComboValues.Destroy;
begin
//  FIndependentTValues.Free;
  inherited Destroy;
end;

function TConditionalComboValues.DoCreateCopy : TGridEditorComboValues;
begin
  Result := TConditionalComboValues.Create( DataField, FConditionField, FValueLegal, FDependentResult );
end;

function TConditionalComboValues.FilterResultDependsOnRow : Boolean;
begin
  if (FIndependentStrings = nil) or (FIndependentStrings.Count = 0) then
    Result := True
  else
    Result := FDependentResult;
end;

procedure TConditionalComboValues.FilterComboValues(var Strings : TDataRowList; Row : TAbstractRow);
var
  iRow : Integer;
  aRow : TAbstractRow;
  tempList : TDataRowList;
begin
  tempList := TDataRowList.Create;
  for iRow := 0 to Strings.Count -1 do
  begin
    aRow := Strings.DataRows[iRow];
    if (FValueLegal = aRow.BooleanValue[FConditionField]) then
      tempList.AddObject(Strings.Strings[iRow], aRow);
  end;
  Strings.Assign(tempList);
  tempList.Free;
end;

{ TConditionValuesEqualComboValues }

constructor TConditionValuesEqualComboValues.Create(aKeyField : TDataField; ActiveRowConditionField, ComboRowConditionField : TDataField;
            FilterResultDependsOnRow : Boolean);
begin
  Inherited Create(aKeyField);

  FActiveRowConditionField := ActiveRowConditionField;
  FComboRowConditionField := ComboRowConditionField;
  FDependentResult := FilterResultDependsOnRow;
end;

function TConditionValuesEqualComboValues.DoCreateCopy : TGridEditorComboValues;
begin
  Result := TConditionValuesEqualComboValues.Create( DataField,  FActiveRowConditionField,
        FComboRowConditionField, FDependentResult );
end;

function TConditionValuesEqualComboValues.FilterResultDependsOnRow : Boolean;
begin
  if (FIndependentStrings = nil) or (FIndependentStrings.Count = 0) then
    Result := True
  else
    Result := FDependentResult;
end;

procedure TConditionValuesEqualComboValues.FilterComboValues(var Strings : TDataRowList; Row : TAbstractRow);
var
  iRow : Integer;
  ARow : TAbstractRow;
  TempList : TDataRowList;
begin
  TempList := TDataRowList.Create;
  for iRow := 0 to Strings.Count -1 do
  begin
    ARow := Strings.DataRows[iRow];
    if FActiveRowConditionField.DataType.Equals(ARow[FComboRowConditionField],
        Row[FActiveRowConditionField]) then
      TempList.AddObject(Strings.Strings[iRow], aRow);
  end;
  Strings.Assign(TempList);
  TempList.Free;
end;

{ TTableDependentComboValues }

constructor TTableDependentComboValues.Create(aKeyField : TDataField; CompField : TDataField;
            ATable : TAuxTable; FilterResultDependsOnRow : Boolean);
begin
  inherited Create(aKeyField);
  FTable := ATable;
  FDependentResult := FilterResultDependsOnRow;
  FConditionField := CompField;
end;

destructor TTableDependentComboValues.Destroy;
begin
  inherited Destroy;
end;

function TTableDependentComboValues.DoCreateCopy : TGridEditorComboValues;
begin
  Result := TTableDependentComboValues.Create( DataField, FConditionField, FTable,
      FDependentResult );
end;

function TTableDependentComboValues.FilterResultDependsOnRow : Boolean;
begin
  if (FIndependentStrings = nil) or (FIndependentStrings.Count = 0) then
    Result := True
  else
    Result := FDependentResult;
end;

function TTableDependentComboValues.RemoveConditionFieldSelections(Field : TDataField;
  var ConflictAction : TConflictAction; var KeepSrc : Boolean) : TDataField;
begin
  if Field = FConditionField then
    Result := nil
  else
    Result := Field;

  KeepSrc := False;
  ConflictAction := caCurrent;
end;

procedure TTableDependentComboValues.FilterComboValues(var Strings : TDataRowList; Row : TAbstractRow);

  procedure RemoveDuplicates( Rows : TStrings );
  var
    i : Integer;
    OldValue : TValue;
    DataType : TDataType;
  begin
    if Rows.Count > 1 then
    begin
      DataType := DataField.DataType;
      OldValue := TAbstractRow(Rows.Objects[Rows.Count-1])[DataField];
      for i := Rows.Count -2 downto 0 do
        if DataType.Equals( TAbstractRow(Rows.Objects[i])[DataField], OldValue ) then
          Rows.Delete(i)
        else
          OldValue := TAbstractRow(Rows.Objects[i])[DataField];
    end;
  end;

var
  ACondition, TmpCondition : TCondition;
begin
  ACondition := CreateLoadCondition( Row );

//    DisplayCondition( ACondition );
  if (Row.StringValue[FConditionField] = '') then
  begin
    TmpCondition := ACondition;
    ACondition := TmpCondition.CreateFieldTranslatedCopy(RemoveConditionFieldSelections);
    TmpCondition.Free;
  end;
(*
    if (Row.StringValue[FConditionField] = '') then
    ACondition := ACondition.CopyValues( nil, FConditionField, False, False );
    *)
//    DisplayCondition( ACondition );

  Strings.Clear;

  FTable.Cache.GetRows(Strings, ACondition, gaReference);
  RemoveDuplicates( Strings );
  FillKeyAndDescription( Strings );
  ACondition.Free;
end;

function TTableDependentComboValues.CreateLoadCondition( Row : TAbstractRow ) : TCondition;
var
  AFieldCrit : TCriteriaField;
begin
  AFieldCrit := TCriteriaField.Create(FConditionField);
  AFieldCrit.AddValue(Row[FConditionField]);

    Result := AFieldCrit;
end;

procedure TTableDependentComboValues.FillKeyAndDescription( Strings : TDataRowList );
begin
  Strings.FillStringsWithKeyAndDescription(FDataField);
end;

procedure TTableDependentComboValues.GetComboStrings(
  var Strings: TDataRowList; Criteria: TCondition; Row: TAbstractRow);
begin
  // nothing
end;

{ TAddValuesComboValues }

constructor TAddValuesComboValues.Create(aKeyField : TDataField;
    ValueRowsArray : array of TAbstractRow);
var
  i : Integer;
begin
  inherited Create(aKeyField);

  FValueRows := TDataRowList.Create;
  for i := Low(ValueRowsArray) to High(ValueRowsArray) do
    ValueRows.AddObject('', ValueRowsArray[i]);
end;

function TAddValuesComboValues.DoCreateCopy : TGridEditorComboValues;
var
  i : Integer;
begin
  Result := TAddValuesComboValues.Create( DataField, [nil] );
  TAddValuesComboValues(Result).ValueRows.Clear;
  for i := 0 to ValueRows.Count -1 do
    TAddValuesComboValues(Result).ValueRows.AddObject( '', ValueRows.AbstractRows[i] );
end;

destructor TAddValuesComboValues.Destroy;
begin
  inherited Destroy;

  FValueRows.Free;
end;

procedure TAddValuesComboValues.FilterComboValues(
  var Strings: TDataRowList; Row: TAbstractRow);
begin
  // Nothing;
end;

function TAddValuesComboValues.FilterResultDependsOnRow: Boolean;
begin
  Result := False;
end;

procedure TAddValuesComboValues.GetComboStrings(var Strings : TDataRowList; Criteria : TCondition; Row : TAbstractRow);
begin
  Strings.Clear;
  FDataField.GetRows(Strings, Criteria);
  PutRowsToList(Strings, Criteria, Row);
  Strings.FillStringsWithKeyAndDescription(FDataField);
end;

procedure TAddValuesComboValues.PutRowsToList(AList: TDataRowList;
  Criteria : TCondition; ARow: TAbstractRow);
var
  i : Integer;
begin
  for i := ValueRows.Count -1 downto 0 do
    if Criteria.AcceptsValue(FDataField, ValueRows.DataRows[i][FDataField]) then
      AList.InsertObject(0, '', ValueRows.DataRows[i]);
end;

{ TTableDependentConditionalDefaultValue }

constructor TTableDependentConditionalDefaultValue.Create(aDataField,
  CompField, ConditionField: TDataField; ATable: TAuxTable; AConst: TValue;
  ConditionValueLegal: Boolean);
begin
  inherited Create( aDataField, ConditionField, AConst, ConditionValueLegal );

  FTable := ATable;
  FComparisonField := CompField;
end;

procedure TTableDependentConditionalDefaultValue.GetLegalValues( Row : TAbstractRow; Strings : TDataRowList );
var
  AFieldCrit : TCriteriaField;

  AStorage : TAbstractRowStorage;
begin
  AStorage := Row.Storage;
  if AStorage is TCustomRowStorage then
  begin
    if (Row.StringValue[FComparisonField] = '') then
      Exit;

    AFieldCrit := TCriteriaField.Create(FComparisonField);
    AFieldCrit.AddValue(Row[FComparisonField]);

  end;
end;

function TTableDependentConditionalDefaultValue.ValueLegal(
  Row: TAbstractRow): Boolean;
begin
  Result := inherited ValueLegal( Row ) and ( FTable.Cache.LocateByRowValues( Row , [nil] ) <> nil );
end;

end.

