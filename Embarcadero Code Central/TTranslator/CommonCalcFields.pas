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

{ $Id: CommonCalcFields.pas,v 1.99 2003/04/11 09:06:58 lge Exp $}

{----------------------------------------------------------------------------
  CommonCalcFields   Commonly used calc fields

  What:              TConstantField
                     TSumField
                     TDifferenceField
                     TDifferencePctField
                     TRatioField
                     TSubTotalPercentageField

  Company:           Polycon Ab
  Authors:           LAA, MVJ
----------------------------------------------------------------------------}

unit CommonCalcFields;

interface

{$i common.inc}

uses
  Classes,
{$ifndef LINUX}
  Graphics,
{$else LINUX}
  QGraphics,
{$endif LINUX}
  DataType, DataTypes, DataElements, CommonLib, CalcField;

type
  {/** Always returns a constant TValue  */}
  TConstantField = class(TCalcField)
  private
    FConstant : TValue;
  public
    {/** Constructor */}
    constructor Create(AOwner: TComponent); override;
    constructor CreateOld(const FieldName : String; Constant : TValue);
    constructor CreateNoName(Constant : TValue);

    {/** Destructor */}
    destructor Destroy; override;

    function CalcValue(ARow : TAbstractRow) : TValue; override;
    function DistributeValue(ARow : TAbstractRow; Value : TValue) : TSetResult; override;

    property Constant : TValue read FConstant write FConstant;
  end;



  {/** FieldsUserd as Headers in a HeaderRowView */}
  THeaderField = class(TConstantField)
  private
    FDataField : TDataField;
    FUseShortDescription : Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateOld(const FieldName : String; ADataField : TDataField; Constant : TValue);

    {/** Pointer to the DataField that decides this fields description */}
    property DataField : TDataField read FDataField;
    property UseShortDescription : Boolean read FUseShortDescription write FUseShortDescription;
  end;

  TDerivedHeaderField = class(THeaderField)
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateOld(FieldName : String; ADataField : TDataField);

    {/** See @TCalcField */}
    function CalcValue(ARow : TAbstractRow) : TValue; override;
  end;

























  {/** Decides wether a DataRow is modified according to DataRow.ContainsChanges */}
  TRowModifiedField = class(TCalcField)
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateOld(FieldName : String);
    function CalcValue(ARow : TAbstractRow) : TValue; override;
  end;





{ifndef WEBPROFFA}

{endif WEBPROFFA}



  {/** Contains an erroemessage for the given DataField */}
  TErrorMessageField = class(TCalcField)
  private
    FDataField : TDataField;
  protected
    constructor CreateOld(const FieldName : String; ADataField : TDataField);
    function GetErrorString(ARow : TAbstractRow) : String; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    property DataField : TDataField read FDataField;
    property ErrorString[ARow : TAbstractRow] : String read GetErrorString;

    procedure GetAdditionalLegalValues(var Strings : TStrings); virtual; abstract;
    function IsRowValid( const FieldMsgObjectList : TStringList; ARow : TAbstractRow;
                         DisabledFieldList : TAbstractFieldList ) : Boolean; virtual;
  end;

  {/** Check wether a DataRow has legal values or not. Possile to get messages definig problem*/}
  TRowIsValidField = class(TClosedField)
  private
    FObjects : TList;
    function GetSourceField(idx : Integer) : TDataField;
    function GetConditionFieldForSource(AField : TDataField) : TErrorMessageField;

    property SourceField[idx : Integer] : TDataField read GetSourceField;
  protected
    function GetFieldCount : Integer; override;
    function GetField(idx : Integer) : TDataField; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateOld(const FieldName : String; ConditionObjects : array of TErrorMessageField);
    destructor Destroy; override;

    function IsRowValid(const FieldMsgObjectList : TStringList; ARow : TAbstractRow;
        DisabledFieldList : TAbstractFieldList; BreakOnFalse : Boolean) : Boolean; virtual;
    function CalcValue(ARow : TAbstractRow) : TValue; override;
    function CreateCopy(const FieldName : String; GetNewField : TQueryFieldFunction; OwnedList:TList) : TClosedField;  override;
    procedure GetAdditionalLegalValues(AField : TDataField; var Strings : TStrings); virtual;

    property ConditionFieldForSource[AField : TDataField]  : TErrorMessageField read GetConditionFieldForSource;
  end;















  TPictureField = class(TCalcField)
  private
    FPicture : TPicture;
  protected
    property Picture : TPicture read FPicture;
  public
    {/** Constructor */}
    constructor CreateOld(FieldName : String; APicture : TPicture);
    {/** Destructor */}
    destructor Destroy; override;
    {/** See @TCalcField */}
    function CalcValue(ARow : TAbstractRow) : TValue; override;
  end;



  TBooleanTextField = class(TCalcField)
  private
    FOwner : TDataField;
  public
    constructor CreateOld(const FieldName : String; Owner : TDataField);
    destructor Destroy; override;
    function CalcValue(ARow : TAbstractRow) : TValue; override;
  end;

  {/** TWrapperField wraps a TDataField and returns its value. This is needed
       where you want to have the same field twice in the same datatable. 
  */}
  TWrapperField = class(TKnownClosedField)
  public
    constructor CreateOld(BaseField : TDataField);
    function CalcValue(ARow : TAbstractRow) : TValue; override;
  end;

implementation

uses
  SysUtils, Math,
  Criteria, DerivedDataType, DataLib, RowList;

const
  MSGE_ConstValueCantBeConvertedToType = 'The constant value can''t be converted to type';
  MSGE_IndexOutOfBounds = 'Index out of bounds!';
  MSGI_ValueFor = 'The value for';
  MSGI_NotDefinedInTable = 'is not defined in table!';

{ TBooleanTextField }

constructor TBooleanTextField.CreateOld(const FieldName : String; Owner : TDataField);
begin
  inherited CreateOld(FieldName, StringType(5, False), True, True);
  FOwner := Owner;
  FOwner.SetAllTextFields([Self]);
  FOwner.DisplayValues := dvTextOnly;
end;

destructor TBooleanTextField.Destroy;
begin
  inherited Destroy;
end;

function TBooleanTextField.CalcValue(ARow : TAbstractRow) : TValue;
begin
  if ARow.BooleanValue[FOwner] then
    Result := ValueFromString('True')
  else
    Result := ValueFromString('False');
end;

{ TConstantField }

constructor TConstantField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConstant := ZeroVal;
  IsAggregable := True;
end;

constructor TConstantField.CreateOld(const FieldName : String; Constant : TValue);
begin
  inherited CreateOld(FieldName, Constant.DataType, true, false);
  FConstant := Constant;
  IsAggregable := True;
end;

constructor TConstantField.CreateNoName(Constant : TValue);
begin
  CreateOld('', Constant);
end;

destructor TConstantField.Destroy;
begin
  inherited Destroy;
end;

function TConstantField.CalcValue(ARow : TAbstractRow) : TValue;
begin
  Result := FConstant;
end;

function TConstantField.DistributeValue(ARow : TabstractRow; Value : TValue) : TSetResult;
begin
  result := srReadOnly;
end;



// ---------------------------------- THeaderField -------------------------------

constructor THeaderField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDataField := nil;
  FUseShortDescription := False;
end;

constructor THeaderField.CreateOld(const FieldName : String; ADataField : TDataField; Constant : TValue);
begin
  inherited CreateOld(FieldName, Constant);

  FDataField := ADataField;
  FUseShortDescription := False;
end;

// ---------------------------------- TDerivedHeaderField -------------------------------

constructor TDerivedHeaderField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

constructor TDerivedHeaderField.CreateOld(FieldName : String; ADataField : TDataField);
begin
  Assert(ADataField <> nil);
  Inherited CreateOld(FieldName, ADataField, ValueFromString(''));
end;

function TDerivedHeaderField.CalcValue(ARow : TAbstractRow) : TValue;
begin
  if FUseShortDescription then
    result := ValueFromString(FDataField.ShortDescription)
  else
    result := ValueFromString(FDataField.LongDescription);
end;


























{---------------------- TRowModifiedField -----------------------------------}

constructor TRowModifiedField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

constructor TRowModifiedField.CreateOld(FieldName : String);
begin
  Inherited CreateOld(FieldName, BooleanType, True, False);
end;

function TRowModifiedField.CalcValue(ARow : TAbstractRow) : TValue;
begin
  Result := ValueFromBoolean(ARow.ContainsChanges);
end;





//---------------------TConstantSQLField----------------------------------------

{ifndef WEBPROFFA}

{endif WEBPROFFA}



{ TErrorMessageField }

constructor TErrorMessageField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDataField := nil;
end;

constructor TErrorMessageField.CreateOld(const FieldName : String; ADataField : TDataField);
begin
  Inherited CreateOld(FieldName, BooleanType, True, False);

  FDataField := ADataField;
end;

function TErrorMessageField.IsRowValid(const FieldMsgObjectList : TStringList; ARow: TAbstractRow;
  DisabledFieldList: TAbstractFieldList): Boolean;
begin
  Result := ARow.BooleanValue[Self];

  if not Result and (FieldMsgObjectList <> nil) then
    FieldMsgObjectList.Add(ErrorString[ARow]);
end;

{ TRowIsValidField }

constructor TRowIsValidField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FObjects := TList.Create;
end;

constructor TRowIsValidField.CreateOld(const FieldName : String; ConditionObjects : array of TErrorMessageField);
var
  idx : Integer;
begin
  FObjects := nil;
  inherited CreateOld(FieldName, BooleanType, True, True);
  if FObjects=nil then
   FObjects := TList.Create;

  if (TObject(ConditionObjects[Low(ConditionObjects)]) <> nil) then
    for idx := Low(ConditionObjects) to High(ConditionObjects) do
      FObjects.Add(ConditionObjects[idx]);
end;

function TRowIsValidField.CreateCopy(const FieldName : String; GetNewField : TQueryFieldFunction; OwnedList:TList) : TClosedField;
begin
  raise Exception.Create('TRowIsValidField.CreateCopy: Method not implemented!');
end;

destructor TRowIsValidField.Destroy;
begin
  FObjects.Free;
  inherited Destroy;
end;

function TRowIsValidField.GetFieldCount : Integer;
begin
  Result := FObjects.Count;
end;

function TRowIsValidField.GetField(idx : Integer) : TDataField;
begin
  Assert(idx < FieldCount, Self.ClassName + '.GetField: ' + MSGE_IndexOutOfBounds
          + ' ' + IntToStr(idx) + ' >= ' + IntToStr(FieldCount));
  Result := TErrorMessageField(FObjects[idx]);
end;

function TRowIsValidField.GetSourceField(idx : Integer) : TDataField;
begin
  Assert(idx < FieldCount, Self.ClassName + '.GetSourceField: ' + MSGE_IndexOutOfBounds
          + ' ' + IntToStr(idx) + ' >= ' + IntToStr(FieldCount));
  Result := TErrorMessageField(FObjects[idx]).DataField;
end;

function TRowIsValidField.GetConditionFieldForSource(AField : TDataField) : TErrorMessageField;
var
  idx : Integer;
begin
  Result := nil;
  for idx := 0 to FieldCount -1 do
    if SourceField[idx] = AField then
      Result := TErrorMessageField( GetField(idx) );
end;

function TRowIsValidField.IsRowValid(const FieldMsgObjectList : TStringList;
  ARow : TAbstractRow; DisabledFieldList : TAbstractFieldList; BreakOnFalse : Boolean) : Boolean;

  function CheckFieldHasLegalValue(AField : TErrorMessageField) : Boolean;
  begin
    Result := AField.IsRowValid( FieldMsgObjectList, ARow, DisabledFieldList );
  end;

var
  idx : Integer;
  FieldIsValid : Boolean;
  aField : TDataField;
  aConditionField : TErrorMessageField;
  ATable : TDataTable;
  AKeyList : TList;
begin
  Result := True;
  if FieldMsgObjectList <> nil then
    FieldMsgObjectList.Clear;
  ATable := ARow.DataTable;
  AKeyList := TList.Create;
  ATable.KeysToList( AKeyList );

  for idx := 0 to FieldCount -1 do
  begin
    aConditionField := TErrorMessageField( GetField(idx) );
    aField := aConditionField.DataField;
    AKeyList.Remove( AField );
    if (DisabledFieldList <> nil) and
       DisabledFieldList.ContainsField(aField) then
      Continue;

    FieldIsValid := CheckFieldHasLegalValue(aConditionField);
    Result := Result and FieldIsValid;
    if not Result and BreakOnFalse then
      Break;
  end;

  if Result or not BreakOnFalse then
    for idx := 0 to AKeyList.Count -1 do
    begin
      AField := TDataField( AKeyList[idx] );
      if (DisabledFieldList <> nil) and
         DisabledFieldList.ContainsField(aField) then
        Continue;
      if aField.HasAuxTable then
        FieldIsValid := ( AField.AuxTable.Cache.LocateByRowValues(ARow, [aField]) <> nil )
      else
        FieldIsValid := True;

      if not FieldIsValid and (FieldMsgObjectList <> nil) then
        FieldMsgObjectList.Add('Illegal value for field ' + aField.FieldName + '!');

      Result := Result and FieldIsValid;
      if not Result and BreakOnFalse then
        Break;
    end;
    
  AKeyList.Free;
end;

function TRowIsValidField.CalcValue(ARow : TAbstractRow) : TValue;
var
  dummy : TStringList;
begin
  dummy := nil;
  Result := ValueFromBoolean(IsRowValid(dummy, ARow, nil, True));
end;

procedure TRowIsValidField.GetAdditionalLegalValues(AField : TDataField; var Strings : TStrings);
var
  ACondField : TErrorMessageField;
begin
  ACondField := ConditionFieldForSource[AField];
  if ACondField <> nil then
    ACondField.GetAdditionalLegalValues(Strings);
end;



// -------------------------------- TValueLegalField ---------------------------------------













{ TPictureField }

constructor TPictureField.CreateOld(FieldName : String; APicture : TPicture);
begin
  Inherited CreateOld(FieldName, PictureType, True, False);
  Self.IsAggregable := True;
  FPicture := APicture;
end;

destructor TPictureField.Destroy;
begin
  inherited Destroy;
end;

function TPictureField.CalcValue(ARow : TAbstractRow) : TValue;
begin
  Result := ValueFromPicture(FPicture);
end;



{ TWrapperField }

constructor TWrapperField.CreateOld(BaseField: TDataField);
begin
  inherited CreateOld(BaseField.FieldName+'_'+FormatDateTime('zzz',now), BaseField.DataType, True, True, [BaseField]);
end;

function TWrapperField.CalcValue(ARow: TAbstractRow): TValue;
begin
  result := ARow[ Field[0] ];
end;


end.

