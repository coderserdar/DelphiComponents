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

{ $Id: CalcField.pas,v 1.11 2003/01/27 15:18:58 lge Exp $}

unit CalcField;

interface

uses
  DataElements, DataType, DataTypes, Classes;

type
  TCalcField = class;

  TOnCalcValueEvent = function(Sender : TCalcField; ARow : TAbstractRow) : TValue of object;
  TOnDistributeValueEvent = function(Sender : TCalcField; ARow : TAbstractRow; Value : TValue) : TSetResult of object;
  TOnGetValuesEvent = procedure(Sender : TCalcField; Results : TStrings; Table : TDataTable; Row : TAbstractRow; Condition : TCondition) of object;

  TCalcField = class(TDataField)
  private
    FCalcBeforeAggregating : Boolean;
    FLegalForSubTotals : Boolean;
    FCalcInTable : Boolean;

    FOnCalcValue : TOnCalcValueEvent;
    FOnDistributeValue : TOnDistributeValueEvent;
    FOnGetValues : TOnGetValuesEvent;
  protected
    function GetCanBeInDB : Boolean; override;
    function GetExternValue(ARow : TAbstractRow) : TValue; override;
    function SetExternValue(ARow : TAbstractRow; Value : TValue; Action : TSetAction) : TSetResult; override;
    function FetchValue(ARow : TAbstractRow; idx : Integer) : TValue; override;
    function GetValuesOverridden : Boolean; override;
  public
    {/** Constructor; All properties are calculated from these parameters */}
    constructor Create(AOwner: TComponent); override;
    constructor CreateOld(const FieldName : String; DataType : TDataType;
                          ReadOnly : Boolean; CalcBeforeAggregating : Boolean);
    {/** Destructor */}
    destructor Destroy; override;
    {/** Calc value of this field for a given row */}
    function CalcValue(ARow : TAbstractRow) : TValue; virtual;
    {/** Distribute a value for this field and a given row */}
    function DistributeValue(ARow : TAbstractRow; Value : TValue) : TSetResult; virtual;
    {/** Add to list */}
    procedure AddToList(AList : TList; AddCalcFields : Boolean); override;
    procedure GetValues(Results : TStrings; Table : TDataTable; Row : TAbstractRow; Condition : TCondition); override;
  published
    property CanGenerateValueSQL;
    property CanGenerateCondSQL;
    property LegalForSubTotals : Boolean read FLegalForSubTotals write FLegalForSubTotals;
    {/* If the calcfield is in a datatable, shall the value be calculated anyway */}
    property CalcInTable : Boolean read FCalcInTable write FCalcInTable;
    {/** Should we calculate before we sum the subtotals */}
    property CalcbeforeAggregating : Boolean read FCalcBeforeAggregating write FCalcBeforeAggregating;

    property OnCalcValue : TOnCalcValueEvent read FOnCalcValue write FOnCalcValue;
    property OnDistributeValue : TOnDistributeValueEvent read FOnDistributeValue write FOnDistributeValue;
    property OnGetValues : TOnGetValuesEvent read FOnGetValues write FOnGetValues;
  end;

  TValidateWriteEvent = procedure(var AValue : TValue; var AcceptValue : Boolean);
  TValidateReadEvent = procedure(var AValue : TValue);

  TValidatorField = class(TCalcField)
  private
    FRealField : TDataField;
    FOnReadValue : TValidateReadEvent;
    FOnWriteValue : TValidateWriteEvent;
    property RealField : TDataField read FRealField;
  protected
    function ReadValue(AValue : TValue) : TValue; virtual;
    function WriteValue(AValue : TValue; var AcceptValue : Boolean) : TValue; virtual;
  public
    property OnReadValue : TValidateReadEvent read FOnReadValue write FOnReadValue;
    property OnWriteValue : TValidateWriteEvent read FOnWriteValue write FOnWriteValue;
    constructor Create(AOwner: TComponent); override;
    constructor CreateOld(const FieldName : String; RealField : TDataField; ReadOnly : Boolean);
    destructor Destroy; override;

    {/** See TCalcField */}
    function CalcValue(ARow : TAbstractRow) : TValue; override;
    function DistributeValue(ARow : TAbstractRow; Value : TValue) : TSetResult; override;
  end;

  {/** TClosed field is a abstact base class for all calc fields that want to tell
       which fields they consist of. */}
  TClosedField = class(TCalcField)
  protected
    function GetFieldCount : Integer; virtual; abstract;
    function GetField(idx : Integer) : TDataField; virtual; abstract;
  public
    {/** Creates a copy of the field, that points to a new set of subfields, this
         way we generate temporary acrossed fields.
         @param FieldName   the new field name
         @param GetNewField pointer to function that returns the corresponding
                            new sub field for each old sub field
         @param Garbagecan  pointer to procedure that takes care of disposing all
                            objects when they're not needed anymore
         @author LGE, LAA
    */}
    function CreateCopy(const FieldName : String; GetNewField : TQueryFieldFunction; OwnedObjects : TList) : TClosedField; virtual; abstract;
    property FieldCount : Integer read GetFieldCount;
    property Field[idx : Integer] : TDataField read GetField;
  end;

  {/** TKnownClosedField is an instance of TClosedField that implements
       GetField and GetFieldCound that are required by TClosedField. */}
  TKnownClosedField = class(TClosedField)
  private
    fFieldComponents : array of TDataField;
  protected
    function GetFieldCount : Integer; override;
    function GetField(idx : Integer) : TDataField; override;
  public
    // Fixa LGE/LAA -- kan vi ta veck denna metod ; implementationen suger bigtime på den!!!
    function CreateCopy(const FieldName : String; GetNewField : TQueryFieldFunction; OwnedObjects : TList) : TClosedField; override;
    constructor CreateOld(const FieldName : String; DataType : TDataType;
                          ReadOnly : Boolean; CalcBeforeAggregating : Boolean;
                          const FieldComponents : array of TDataField);
  end;

  TKnownClosedFieldClass = class of TKnownClosedField;

  TFictiveField = class(TKeyField)
  private
    FCalcField : TCalcField;
  protected
    function GetCanBeInDB : Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateDependent(FieldName : String; AuxTableField : TDataField; Criteria : TCommonQuilt; CalcField : TCalcField; LookupKey : TDataField);
    destructor Destroy; override;
  published
    property CalcField : TCalcField read FCalcField write FCalcField;
  end;

implementation

uses
  SysUtils;
   
{ TValidatorField }

function TValidatorField.ReadValue(AValue : TValue) : TValue;
begin
  Result := AValue;
  if Assigned(FOnReadValue) then
    FOnReadValue(Result);
end;

function TValidatorField.WriteValue(AValue : TValue; var AcceptValue : Boolean) : TValue;
begin
  Result := AValue;
  if Assigned(FOnWriteValue) then
  begin
    AcceptValue := True;
    FOnWriteValue(Result, AcceptValue);
  end;
end;

constructor TValidatorField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRealField := nil;
end;

constructor TValidatorField.CreateOld(const FieldName : String; RealField : TDataField; ReadOnly : Boolean);
begin
  inherited CreateOld(FieldName, RealField.DataType, ReadOnly, True);

  FRealField := RealField;
end;

destructor TValidatorField.Destroy;
begin
  inherited Destroy;
end;

function TValidatorField.CalcValue(ARow : TAbstractRow) : TValue;
begin
  Result := ReadValue(ARow.Value[RealField]);
end;

function TValidatorField.DistributeValue(ARow : TAbstractRow; Value : TValue) : TSetResult;
var
  TempRes : TValue;
  Accept : Boolean;
begin
  TempRes := WriteValue(Value, Accept);
  if Accept then
    Result := ARow.SetFieldValue(RealField, TempRes, saDontOverwriteKeys)
  else
  begin
    Result := srReadOnly;
    Abort;
  end;
end;

{ TCalcField }

constructor TCalcField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCalcInTable := False;
  DefaultReadOnly := False;
  FCalcBeforeAggregating := True;
  FLegalForSubTotals := True;

  Self.CanGenerateCondSQL := False;
  Self.CanGenerateValueSQL := False;
end;

constructor TCalcField.CreateOld(const FieldName : String; DataType : TDataType; ReadOnly : Boolean; CalcBeforeAggregating : Boolean);
begin
  inherited CreateOld(FieldName, DataType);
  FCalcInTable := False;
  DefaultReadOnly := ReadOnly;
  FCalcBeforeAggregating := CalcBeforeAggregating;
  FLegalForSubTotals := True;

  Self.CanGenerateCondSQL := False;
  Self.CanGenerateValueSQL := False;
end;

destructor TCalcField.Destroy;
begin
  inherited Destroy;
end;

function TCalcField.GetCanBeInDB : Boolean;
begin
  Result := False;
end;

function TCalcField.CalcValue(ARow : TAbstractRow) : TValue;
begin
  if Assigned(OnCalcValue) then
    Result := OnCalcValue(Self, ARow)
  else
    raise Exception.Create(Self.ClassName + '.CalcValue: Method not implemented!');
end;

function TCalcField.DistributeValue(ARow : TAbstractRow; Value : TValue) : TSetResult;
begin
  if Assigned(OnDistributeValue) then
    Result := OnDistributeValue(Self, ARow, Value)
  else
    raise Exception.Create(Self.ClassName + '.DistributeValue: Method not implemented!');
end;

procedure TCalcField.GetValues(Results : TStrings; Table : TDataTable; Row : TAbstractRow; Condition : TCondition);
begin
  if Assigned(OnGetValues) then
    OnGetValues(Self, Results, Table, Row, Condition)
  else
    inherited GetValues(Results, Table, Row, Condition);
end;

procedure TCalcField.AddToList(AList : TList; AddCalcFields : Boolean);
begin
  if AddCalcFields then
    inherited AddToList(AList, AddcalcFields);
end;

function TCalcField.FetchValue(ARow : TAbstractRow; idx : Integer) : TValue;
begin
  if Self.CalcInTable then
    Result := Self.GetExternValue(ARow)
  else
    Result := inherited FetchValue(ARow, idx);
end;

type
  TSubTotalRowLink = class(TSubTotalRow);

function TCalcField.GetExternValue(ARow : TAbstractRow) : TValue;
var
  fieldIdx : Integer;
begin
  if ARow is TSubTotalRow then
  begin
    fieldIdx := ARow.DataTable.IndexOfField(Self);
    if (fieldIdx >= 0) and (FieldIdx < ARow.DataTable.KeyCount) then
      Result := Self.CalcValue(ARow)
    else if not Self.IsAggregable then
    begin
      if Self.LegalForSubTotals then
        Result := Self.CalcValue(ARow)
      else
        Result := DataType.DefaultValue;
    end
    else if Self.CalcBeforeAggregating then
      Result := TSubTotalRowLink(ARow).SumValues(Self)
    else
      Result := Self.CalcValue(ARow);
  end
  else
    Result := Self.CalcValue(ARow);
end;

function TCalcField.SetExternValue(ARow : TAbstractRow; Value : TValue; Action : TSetAction) : TSetResult;
begin
  if Self.ReadOnly[ARow] then
    Result := srReadOnly
  else
    Result := Self.DistributeValue(ARow, Value);
end;

var
  FDummyCalcField : TCalcField = nil;
function DummyCalcField : TCalcField;
begin
  if not Assigned(FDummyCalcField) then
    FDummyCalcField := TCalcField.CreateOld('', BooleanType, True, True);
  Result := FDummyCalcField;
end;

function TCalcField.GetValuesOverridden: Boolean;
begin
  Result := Assigned(OnGetValues) or
            (GetValuesToMethod(Self.GetValues).Code <> GetValuesToMethod(DummyCalcField.GetValues).Code);
end;

{ TFictiveField }

constructor TFictiveField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DefaultReadOnly := True;
  FCalcField := nil;
end;

constructor TFictiveField.CreateDependent(FieldName : String; AuxTableField : TDataField; Criteria : TCommonQuilt; CalcField : TCalcField; LookupKey : TDataField);
begin
  Assert(AuxTableField <> nil, 'TFictiveField.CreateDependent: AuxTableField <> nil');

  inherited CreateDependent(FieldName, AuxTableField, Criteria, LookupKey);
  DefaultReadOnly := True;
  FCalcField := CalcField;
end;

destructor TFictiveField.Destroy;
begin
  inherited Destroy;
end;

function TFictiveField.GetCanBeInDB : Boolean;
begin
  Result := False;
end;

{ TKnownClosedField }

constructor TKnownClosedField.CreateOld(const FieldName: String;
  DataType: TDataType; ReadOnly, CalcBeforeAggregating: Boolean;
  const FieldComponents : array of TDataField);
var
  iField : integer;
begin
  inherited CreateOld(FieldName, DataType, ReadOnly, CalcBeforeAggregating);

  SetLength( fFieldComponents, Length(FieldComponents) );
  for iField := Low(FieldComponents) to High(FieldComponents) do
    fFieldComponents[ iField ] := FieldComponents[ iField ];
end;

function TKnownClosedField.CreateCopy(const FieldName : String; GetNewField : TQueryFieldFunction; OwnedObjects : TList) : TClosedField;
begin
  Result := TKnownClosedFieldClass(ClassType).CreateOld(FieldName, Self.DataType, Self.DefaultReadOnly, Self.CalcBeforeAggregating, fFieldComponents);
end;

function TKnownClosedField.GetField(idx: Integer): TDataField;
begin
  result := fFieldComponents[idx];
end;

function TKnownClosedField.GetFieldCount: Integer;
begin
  Result := Length( fFieldComponents );
end;

initialization
  RegisterClasses([TCalcField, TFictiveField, TClosedField]);

finalization
  FreeAndNil(FDummyCalcField);

end.

