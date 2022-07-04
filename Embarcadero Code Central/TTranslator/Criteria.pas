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

{ $Id: Criteria.pas,v 1.29 2003/04/11 13:26:13 laa Exp $}

{-------------------------------------------------------------------------
  Criteria         Criterias

  What             TCriteria (with descendents)

  Company          Polycon
  Authors          LGE
-------------------------------------------------------------------------}

unit Criteria;

interface

{$i common.inc}

uses

  StdCtrls,

  Quilt, IndexContainer, Classes, DataType, DataElements;

type
  TCriteria = class;
  TCriteriaField = class;



  PCritValueItem = ^TCritValueItem;
  TCritValueItem = record
    FValue1: TValue;
    FValue2: TValue;
  end;

  PCritValueItemList = ^TCritValueItemList;
  TCritValueItemList = array[0..(MaxListSize div 4)] of TCritValueItem;

  TCriteriaValueList = class
  private
    FList: PCritValueItemList;
    FCount: Integer;
    FCapacity: Integer;
    FDuplicates: TDuplicates;
    Field : TDataField;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer);
    procedure InsertItem(Index: Integer; const S: TValue);
    procedure Error(const Msg: string);
  protected
    function Get1(Index: Integer): TValue;
    function Get2(Index: Integer): TValue;
    function GetCapacity: Integer;
    function GetObject(Index: Integer): TObject;
//    procedure Put1(Index: Integer; const S: TValue);
    procedure Put2(Index: Integer; const S: TValue);
    procedure SetCapacity(NewCapacity: Integer);
    function AcceptsValue(Value : TValue) : Boolean;
    function AcceptsInterval(Low, High : TValue) : Boolean;
  public
    constructor Create(Field : TDataField);
    destructor Destroy; override;
    function Add(const S: TValue): Integer;
    function AddInterval(const S1, S2 : TValue) : Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function Find(const S: TValue; var Index: Integer): Boolean;
    function IndexOf(const S: TValue): Integer;
//    procedure Insert(Index: Integer; const S: TValue);
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Values1[Idx : Integer] : TValue read Get1; // write Put1;
    property Values2[Idx : Integer] : TValue read Get2 write Put2;
    property Count : Integer read FCount;
  end;

  TMultiOperandCondition = class(TCondition)
  private
    FOperands : TList;

    function GetCount : Integer;
    function GetOperand(idx : Integer) : TCondition;
  protected
//    procedure Replace( CurrentOperand, NewOperand : TCondition ); override;
    // function DoCopyValues( FromField, ToField : TDataField; CreateUnion, KeepOld : Boolean ) : TCondition; override;
    function ConcernsField(DataField : TDataField) : Boolean; override;
    function ConcernsTable(DataTable : TDataTable) : Boolean; override;
  public
    constructor DoCreateFromStream(AStream : TStream); override;
    procedure DoWriteToStream(AStream : TStream; Indent : Integer); override;
    constructor Create; virtual; abstract;
    constructor CreateFromArray(ConditionArray : array of TCondition); virtual; abstract;
    constructor CreateOwned(Owner : TCondition);
    destructor Destroy; override;

    procedure AddOperand(ACondition : TCondition); virtual;
    procedure RemoveOperand(ACondition : TCondition); virtual;
    procedure Remove(idx : Integer); virtual;
    function Equals(Condition : TCondition) : Boolean; override;

    property Count : Integer read GetCount;
    property Operand[idx : Integer] : TCondition read GetOperand;
    procedure ProcessFields(Proc : TProcFieldCond); override;
  end;

  TAndCondition = class(TMultiOperandCondition)
  protected

  public
    function GetLargerCriteria(var FreeCriteria : Boolean) : TCondition {TCriteria}; override;
    function GetSmallerCriteria(var FreeCriteria : Boolean) : TCondition {TCriteria}; override;

    constructor Create; override;
    constructor CreateFromArray(ConditionArray : array of TCondition); override;
    destructor Destroy; override;
    function IsPureAndCondition : Boolean; override;
    function AcceptsRow(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean; override;
    function AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean; override;
    function AcceptsValue(AField : TDataField; AValue : TValue) : Boolean; override;
    function CreateCopy : TCondition; override;
    function CreateFieldTranslatedCopy(TranslateField : TQueryDataFieldFunction) : TCondition; override;
    function AcceptsAllInTable(DataTable : TDataTAble) : Boolean; override;
    function AcceptsNoValuesForTable(DataTable : TDataTable) : Boolean; override;
    function AcceptsAllForField(DataField : TDataField) : Boolean; override;
    function AcceptsNoValuesForField(DataField : TDataField) : Boolean; override;
    function AcceptsExactlyOneValue(DataField : TDataField; var Value : TValue) : Boolean; override;
    function GetCommonQuilt(out FreeQuilt : Boolean) : TCommonQuilt; override;
  end;






  TNiceCondition = class(TCondition)
  private
    procedure DoMap(Field : TDataField; var Dest : TCriteriaField); virtual; abstract;
    procedure ApplyFromSource(Criteria : TCriteria; DataTable : TDataTable; AndConditions : Boolean); virtual; abstract;
  public
    function IsPureAndCondition : Boolean; override;
  end;

  TCriteriaField = class(TNiceCondition)
  private
    //FOwner : TCriteria;
    FValues : TCriteriaValueList;
    FAcceptAll, FAcceptNone : Boolean;
//    function GetSelfIndex : Integer;
    function GetDataField : TDataField;
    function GetCaption : String;
    procedure SetCaption(ACaption : String);
    function GetConditionCount : Integer;
    function GetIsInterval(idx : Integer) : Boolean;
    function GetSingleValue(idx : Integer) : TValue;
    function GetStartValue(idx : Integer) : TValue;
    function GetEndValue(idx : Integer) : TValue;
    function GetOnlyValue : TValue;
    function GetFieldOwner : TCriteria;
    procedure FreeOwnedMemory;
    procedure Apply(AndConditions : Boolean; SuperKeys : TCriteria; CriteriaField : TCriteriaField);
    procedure ApplyFromSource(Criteria : TCriteria; DataTable : TDataTable; AndConditions : Boolean); override;
    procedure DoMap(Field : TDataField; var Dest : TCriteriaField); override;
    class procedure Map(Field : TDataField; Src : TNiceCondition; var Dest : TCriteriaField);
    function CreateCopyWithField(NewField: TDataField): TCriteriaField;
  protected
    constructor CreateOwned(Owner : TCondition; Field : TDataField);
    constructor CreateOwnedIntersection(CopyOwner : TCondition; Field : TDataField; Src1, Src2 : TNiceCondition);
    constructor CreateOwnedUnion(CopyOwner : TCondition; Field : TDataField; Src1, Src2 : TNiceCondition);
    constructor CreateOwnedCopy(CopyOwner : TCondition; CriteriaField : TCriteriaField);
    procedure Print(List : TStringList);
    procedure InternalGetRows(Handler : TGetRowsHandler; SubTotal : TSubTotalRow); override; // (SubTotal : TSubTotalRow; Results : TStrings; Action : TGetAction; DefaultSort, ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean); override;
    function InternalHasRowsThatMatch(Handler : TGetRowsHandler; SubTotal : TSubTotalRow) : Boolean; override; // (SubTotal : TSubTotalRow; ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean) : Boolean; override;
//    procedure Replace( CurrentOperand, NewOperand : TCondition ); override;
    // function DoCopyValues( FromField, ToField : TDataField; CreateUnion, KeepOld : Boolean ) : TCondition; override;
    property FieldOwner : TCriteria read GetFieldOwner;
    function ConcernsField(DataField : TDataField) : Boolean; override;
    function ConcernsTable(DataTable : TDataTable) : Boolean; override;

  public
    procedure DoWriteToStream(AStream : TStream; Indent : Integer); override;
    constructor DoCreateFromStream(AStream : TStream); override;
    function GetLargerCriteria(var FreeCriteria : Boolean) : TCondition {TCriteria}; override;
    function GetSmallerCriteria(var FreeCriteria : Boolean) : TCondition {TCriteria}; override;

    constructor Create(Field : TDataField);
    constructor CreateIntersection(Field : TDataField; Src1, Src2 : TNiceCondition);
    constructor CreateUnion(Field : TDataField; Src1, Src2 : TNiceCondition);
    constructor CreateCopyOfField(CriteriaField : TCriteriaField);
    constructor CreateAppliedFromSingleField(DataField : TDataField; Source : TCriteriaField; SuperKeys : TCriteria);
    constructor CreateApplied(DataField : TDataField; Source : TNiceCondition; AndConditions : Boolean);
    procedure GenerateIntervals(AllLegalValues : TStrings);
    destructor Destroy; override;
//    property FieldIndex : Integer read GetSelfIndex;
    property DataField : TDataField read GetDataField;
    property Caption : String read GetCaption write SetCaption;
    property ConditionCount : Integer read GetConditionCount;
    property IsInterval[idx : Integer] : Boolean read GetIsInterval;
    property Value[idx : Integer] : TValue read GetSingleValue;
    property StartValue[idx : Integer] : TValue read GetStartValue;
    property EndValue[idx : Integer] : TValue read GetEndValue;
    property OnlyValue : TValue read GetOnlyValue;
    procedure Reset;
    function FieldSQL : String;

    function FieldSQLWithFieldName(Name : String) : String;
    function Equals(Condition : TCondition) : Boolean; override;
    procedure IntersectWith(Cond : TNiceCondition);
    procedure UnionAddFrom(Cond : TNiceCondition);
    procedure CopyFrom(Source : TCriteriaField);
    function ParamCriteriaIsSubCriteria(Cond : TNiceCondition) : Boolean;
    function HasNoLegalValue : Boolean;
    function AcceptsAnyValue : Boolean;
    function HasExactlyOneValue : Boolean;
    // Not to be mixed with AcceptsAnyValue. HasNoConditions returns true exactly when
    // no AddXXX-function has been called. if AcceptAll is called (the criteriafield will have
    // some conditions (it accepts all) and therefore) HasNoConditions will return false!
    function HasNoConditions : Boolean;

{$ifdef D4_OR_HIGHER}
    function AcceptsRow(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean; override;
    function AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean; override;
{$else}
    function AcceptsRow(ARow : TAbstractRow; AffectedFields : TDataFieldSet) : Boolean; override;
    function AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet) : Boolean; override;
{$endif D4_OR_HIGHER}

    function AcceptsValue(AField : TDataField; AValue : TValue) : Boolean; override;
{$ifdef D4_OR_HIGHER}
    function Accepts(Value : TValue) : Boolean; overload;
{$else}
    function AcceptsOL(Value : TValue) : Boolean;
{$endif D4_OR_HIGHER}
    function AcceptsInterval(Low, High : TValue) : Boolean;
    procedure AcceptAll;
    procedure AcceptNone;
    procedure AddValue(Value : TValue);
    procedure AddString(Value : String);
    procedure AddInterval(Low, High : TValue);
    procedure AddStringInterval(Low, High : String);
    procedure AddSingleValues(Values : TStrings);
    {/** Remove a single value from this CriteriaField */}
// This have to wait
//    procedure RemoveValue(Value : String);

    {/** Change criteria for one field to match .Selected rows in a TListBox object.
         @param AListBox A list box containing TDataRows */}
    procedure SyncFromListBox(AListBox : TListBox; ClearIfAllSelected : Boolean);
    {/** Change .Selected rows in a TListBox object to match criteria for one field.
         @param AListBox A list box containing TDataRows */}
    procedure SyncToListBox(AListBox : TListBox; ClearIfAllSelected : Boolean);
{$ifndef LINUX}
{$endif LINUX}

    {/** The number of intervals in this CriteriaField (can be in any order) */}
    function NrOfIntervals : Integer;
    {/** Save this criteriaField to db */}
    procedure SaveToDB(RowStorage : TAbstractRowStorage; DefaultValues : TDataRow;
                       FieldField, FromField : TDataField; ToField : TDataField; EmptyFirst : Boolean);
    {/** Load criteria from db */}
    procedure LoadFromDB(RowStorage : TAbstractRowStorage; DefaultValues : TDataRow; FieldField, FromField : TDataField; ToField : TDataField);
    {/** Set values from a datarow */}
    procedure AddValuesFromRow(DataRow : TDataRow; FromField : TDataField; ToField : TDataField);

    function CreateCopy : TCondition; override;
    function CreateFieldTranslatedCopy(TranslateField : TQueryDataFieldFunction) : TCondition; override;
    {/** Checks if the Criteria has any restricitions on the fields in ATable*/}
    function AcceptsAllInTable(DataTable : TDataTAble) : Boolean; override;
    function AcceptsNoValuesForTable(DataTable : TDataTable) : Boolean; override;
    function AcceptsAllForField(DataField : TDataField) : Boolean; override;
    function AcceptsNoValuesForField(DataField : TDataField) : Boolean; override;
    function AcceptsExactlyOneValue(DataField : TDataField; var Value : TValue) : Boolean; override;

    procedure ProcessFields(Proc : TProcFieldCond); override;

    function GetCommonQuilt(out FreeQuilt : Boolean) : TCommonQuilt; override;
    function CreateQuiltField : TQuiltField;
  end;

  {/**
    * Use TCriteria for listing conditions for budget fields.
    * Both single value- and interval conditions can be set
    * for each field. Multiple values (or intervals) for the
    * same field are of course allowed. Note that the lexical
    * (String) value is used to compare if a value is within a
    * interval for String fields, while the numeric value is used
    * for numeric fields.
    *
    * By default a DataField has no conditions. Once one of the
    * AddValue, AddInterval or UnionAdd methods are called the a
    * condition is created. Calling the Add methods multiple times
    * will cause a field to have multiple legal values.
    *
    * author LGE
    */}

  TProcCriteriaFields = procedure(DataField : TDataField; CriteriaField : TCriteriaField) of object;

  TCriteria = class(TNiceCondition)
  private
    FFields : TIndexContainer;

    // methods for old compatiblity
    function GetFieldCaption(AField : TDataField) : String;
    procedure SetFieldCaption(AField : TDataField; NewCaption : String);

    // ----------- New interfejs starts here --------------------------
    function GetCriteriaField(Field : TDataField) : TCriteriaField;
    procedure SetCriteriaField(Field : TDataField; NewCriteriaField : TCriteriaField);
//    function GetFieldByIndex(idx : Integer) : TCriteriaField;
    // HasCriteriaOnField requires that there is a corresponding TCriteriaField
//    function HasCriteriaOnField(Field : TDataField) : Boolean;
    // HasConditionOnField requires that the corresponding TCriteriaField has HasConditions=True
    // HasRestrictionsOnField requires that the corresponding TCriteriaField has AcceptsAnyValue=False
    function HasRestrictionsOnField(Field : TDataField) : Boolean;
    procedure Apply(DataTable : TDataTable; AndConditions : Boolean; SuperKeys : TCriteria; CriteriaField : TCriteriaField);
    procedure ApplyFromSource(Criteria : TCriteria; DataTable : TDataTable; AndConditions : Boolean); override;
    procedure DoMap(Field : TDataField; var Dest : TCriteriaField); override;
//    procedure WriteCriteria(Writer : TWriter);
//    procedure ReadCriteria(Reader : TReader);
  protected

    function FindCriteriaField(Field : TDataField; DoCreate : Boolean) : TCriteriaField;
    procedure RemoveChild(CritField : TCriteriaField);
    function EqualsOnAllButOneField(ACrit : TCriteria; var NonEqualField : TDataField) : Boolean;
    procedure InternalGetRows(Handler : TGetRowsHandler; SubTotal : TSubTotalRow); override; // (SubTotal : TSubTotalRow; Results : TStrings; Action : TGetAction; DefaultSort, ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean); override;
    function InternalHasRowsThatMatch(Handler : TGetRowsHandler; SubTotal : TSubTotalRow) : Boolean; override; // (SubTotal : TSubTotalRow; ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean) : Boolean; override;
//    procedure Replace( CurrentOperand, NewOperand : TCondition ); override;
    // function DoCopyValues( FromField, ToField : TDataField; CreateUnion, KeepOld : Boolean ) : TCondition; override;
    function ConcernsField(DataField : TDataField) : Boolean; override;
    function ConcernsTable(DataTable : TDataTable) : Boolean; override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    function HasConditionOnField(Field : TDataField) : Boolean;
    procedure DoWriteToStream(AStream : TStream; Indent : Integer); override;
    constructor DoCreateFromStream(AStream : TStream); override;


    function GetLargerCriteria(var FreeCriteria : Boolean) : TCondition {TCriteria}; override;
    function GetSmallerCriteria(var FreeCriteria : Boolean) : TCondition {TCriteria}; override;
    {/** GetCriteriaField by DataField */}
    property CriteriaField[Field : TDataField] : TCriteriaField read GetCriteriaField write SetCriteriaField; default;
    {/** Nr of CriteriaFields of this Criteria */}
    function FieldCount : Integer;
    {/** CriteriaField by index */}
//    property Fields[idx : Integer] : TCriteriaField read GetFieldByIndex;
    {/** Constructor */}
    constructor Create;
    {/** Create an intersection (Leikkaus/Snitt) between two existing criterias */}
    constructor CreateIntersection(Crit1, Crit2 : TCriteria);
    {/** Create a union between two Criterias */}
    constructor CreateUnion(Crit1, Crit2 : TCriteria);
    {/** Create a union between the fields in two Criterias */}
    constructor CreateFieldUnion(Crit1, Crit2 : TCriteria);
    {/** Create a union between the fields in two Criterias, so that if upper levels
         are discarded of the lower levels has no restrictions. This is sort of
         half-way, but solves many common cases when joining read and write access. */}
    constructor CreateHierarchicalFieldUnion(Crit1, Crit2 : TCriteria);
    {/** Create a criteria from a datarow's keys */}
    constructor CreateFromRowKeys(ARow : TAbstractRow);
    procedure AddValuesFromRowKeys(ARow : TAbstractRow);
    {/** Create a criteria from a datarow's fields */}
    constructor CreateFromRowFields(ARow : TAbstractRow);
    {/**
      * Create a Criteria from another Criteria/CriteriaField where conditions
      * are applied to the given DataTable (e.g. ORG1='T' -> CC='123'-'234' for BUSALE).
      * LookupKeyValueSource is used as superkey source (Company/Period in the Busy case).
      * AndConditions determinates weather AND/INTERSECTION or OR/UNION conditions should
      * be used when there's a N -> 1 conversion (N > 1).
      */}
    constructor CreateApplied(DataTable : TDataTable; Source : TNiceCondition; AndConditions : Boolean);
    {/** Destructor */}
    destructor Destroy; override;
    {/** Make a copy of this Criteria */}
    function CreateCopy : TCondition; override;
    function CreateFieldTranslatedCopy(TranslateField : TQueryDataFieldFunction) : TCondition; override;
    {/** Make a copy of this Criteria */}
    function CreateCopyOfCriteria : TCriteria;
    {/** The SQL-string of this Criteria */}
//    function SQLString : String;
    {/** Checks if Crit equals this Criteria */}
    function Equals(Condition : TCondition) : Boolean; override;
    {/** Checks if the Criteria has any restricitions on the fields in ATable*/}
    function AcceptsAllInTable(DataTable : TDataTable) : Boolean; override;
    function AcceptsNoValuesForTable(DataTable : TDataTable) : Boolean; override;
    function AcceptsAllForField(DataField : TDataField) : Boolean; override;
    function AcceptsNoValuesForField(DataField : TDataField) : Boolean; override;
    {/** Delete all CriteriaFields that HasNoConditions */}
    procedure Cleanup;
    {/** Remove all conditions from this criteria */}
    procedure ResetAll;
    {/** Is a DataRow legal to this Criteria? */}
{$ifdef D4_OR_HIGHER}
    function AcceptsRow(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean; override;
    function AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean; override;
{$else}
    function AcceptsRow(ARow : TAbstractRow; AffectedFields : TDataFieldSet) : Boolean; override;
    function AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet) : Boolean; override;
{$endif D4_OR_HIGHER}
    function AcceptsValue(AField : TDataField; AValue : TValue) : Boolean; override;
    {/** Save a criteria to file (for debugging) */}
    procedure SaveToFile(FileName : String);
    {/** Load criteria from textfile */}
    procedure LoadFromFile(FileName : String);
    {/** Save criteria to db */}
    procedure SaveToDB(RowStorage : TAbstractRowStorage; DefaultValues : TDataRow; FieldField, FromField : TDataField; ToField : TDataField; EmptyFirst : Boolean);
    {/** Load criteria from db */}
    procedure LoadFromDB(RowStorage : TAbstractRowStorage; DefaultValues : TDataRow; FieldField, FromField : TDataField; ToField : TDataField);
    {/** Copy all values from a Criteria excluding the previous values */}
    procedure CopyFromCriteria(Crit : TCriteria);
    {/** Print pretty */}
    procedure PrettyPrint(IgnoreFields : TDataFieldSet; Output : TStrings);
    procedure PrettyPrintSkipFieldDescription(IgnoreFields, NoDescrFields : TDataFieldSet; Output : TStrings);
    function PrettyPrintApplied(IgnoreFields : TDataFieldSet) : string;
//    function TableSQLString(DataTable : TDataTable; SQL : TStrings; Params : TParamList; StartingCond : String; CondsOnNonKeys : Boolean) : Integer;
    function CreateTableKeyCriteria(DataTable : TDataTable) : TCriteria;

    // Depricated interface starts here

    {/** Add (accept) a new value for a field */}
    procedure AddValue(Field : TDataField; V : TValue);
    procedure AddString(Field : TDataField; S : String);
    {/** Add (accept) a new interval for a field */}
    procedure AddInterval(Field : TDataField; F, T : TValue);
    {/** Make a field accept any value */}
    procedure AddAllValues(Field : TDataField);
    {/** Make a field have no legal values */}
    procedure AcceptNoValues(Field : TDataField);
    {/** Is the value legal for this Criteria */}
    function ValueLegal(Field : TDataField; Value : TValue) : Boolean;
    {/** Is any value legal for a field */}
    function AllValuesLegal(Field : TDataField) : Boolean;
    {/** Are all values illegal for a field */}
    function NoValuesLegal(Field : TDataField) : Boolean;
    {/** SQL for a single field only */}
//    function FieldSQLString(Field : TDataField; Params : TParamList) : String;
    {/** Is CmpCriteria a subcriteria to Self on a given field */}
    function IsFieldSubCriteria(CmpCriteria : TCriteria; Field : TDataField) : Boolean;
    {/** Is CmpCriteria a subcriteria to Self */}
    function ParamCriteriaIsSubCriteria(CmpCriteria : TCriteria) : Boolean;
    {/** Add values (by taking union) */}
    procedure UnionAdd(Field : TDataField; Cond : TNiceCondition);
    {/** Is the interval legal for this Field */}
    function IntervalLegal(Field : TDataField; F, T : TValue) : Boolean;
    {/** Is nothing known for this field? Note that this is not the same as AllValuesAdded */}
//    function FieldUnknown(Field : TDataField) : Boolean;
    {/** The displayvalue of a given field */}
    property Caption[Field : TDataField] : String read GetFieldCaption write SetFieldCaption;

    {/** Change criteria for one field to match .Selected rows in a TListBox object.
         @param AListBox A list box containing TDataRows */}
    procedure SyncFromListBox(Field : TDataField; AListBox : TListBox; ClearIfAllSelected : Boolean);
    {/** Change .Selected rows in a TListBox object to match criteria for one field.
         @param AListBox A list box containing TDataRows */}
    procedure SyncToListBox(Field : TDataField; AListBox : TListBox; ClearIfAllSelected : Boolean);
{$ifndef LINUX}
{$endif LINUX}

    function AcceptsExactlyOneValue(DataField : TDataField; var Value : TValue) : Boolean; override;
    procedure ProcessFields(Proc : TProcFieldCond); override;

    procedure ProcFields(Proc : TProcCriteriaFields);

    function GetCommonQuilt(out FreeQuilt : Boolean) : TCommonQuilt; override;
    function CreateQuiltPatch: TQuiltPatch;
  end;

  TCriteriaFieldIterator = class
  private
    FIterator : TIndexContainerIterator;
    function GetCritField : TCriteriaField;
    function GetDataField : TDataField;
  public
    constructor Create(Criteria : TCriteria);
    destructor Destroy; override;
    procedure First;
    procedure Next;
    function EOF : Boolean;
    property CriteriaField : TCriteriaField read GetCritField;
    property DataField : TDataField read GetDataField;
  end;



implementation

uses
{$ifndef LINUX}
{$ifdef D4_OR_HIGHER}
  Contnrs,
{$endif D4_OR_HIGHER}
   Consts, Windows,
{$endif LINUX}
  SysUtils,
  DataTypes, RowList, CommonLib;


const
  HASHSIZE = 101{53}; // number isn't important as long as it's of reasonable size and a prime

type
  TConditionLink = class(TCondition);
  TSubTotalRowLink = class(TSubTotalRow);

// ------------------------ Criteria helper functions -----------------------

function QuoteIfContains(QString : String; QConds : array of String) : String;
var
  i : Integer;
begin
  for i := Low(QConds) to High(QConds) do
  begin
    if Length(QConds[i]) > 1 then
    begin
      if QString = QConds[i] then
      begin
        Result := '''' + QString + '''';
        Exit;
      end;
    end
    else if Pos(QConds[i], QString) > 0 then
    begin
      Result := '''' + QString + '''';
      Exit;
    end
    else if (QConds[i] = ' ') and
            (QString = '') then
    begin
      Result := '''' + QString + '''';
      Exit;
    end;
  end;

  Result := QString;
end;

{ TMultiOperandCondition }

constructor TMultiOperandCondition.DoCreateFromStream(AStream : TStream);
var
  ConditionClass : TConditionClass;
  ACondition : TCondition;
begin
  Create;
  while GetNextStreamItem(AStream, ConditionClass) do
  begin
    ACondition := ConditionClass.DoCreateFromStream(AStream);
    Self.AddOperand(ACondition);
  end;

  if not SkipToObjectEnd(AStream) then
    raise Exception.Create('Invalid stream format!');
end;

procedure TMultiOperandCondition.DoWriteToStream(AStream : TStream; Indent : Integer);
var
  i : Integer;
begin
  WriteName(AStream);
  for i := 0 to Count - 1 do
  begin
    Operand[i].DoWriteToStream(AStream, Indent + Length(ClassName) + 1);
    if i < Count - 1 then
      WriteNewLine(AStream, Indent + Length(ClassName) + 1);
  end;
  WriteCloseParenteses(AStream);
end;

function TMultiOperandCondition.ConcernsField(DataField : TDataField) : Boolean;
var
  i : Integer;
begin
  Result := False;

  for i := 0 to Count - 1 do
    if TConditionLink(Operand[i]).ConcernsField(DataField) then
    begin
      Result := True;
      Exit;
    end;
end;

function TMultiOperandCondition.ConcernsTable(DataTable : TDataTable) : Boolean;
var
  i : Integer;
begin
  Result := False;

  for i := 0 to Count - 1 do
    if TConditionLink(Operand[i]).ConcernsTable(DataTable) then
    begin
      Result := True;
      Exit;
    end;
end;

function TMultiOperandCondition.GetCount : Integer;
begin
  Result := FOperands.Count;
end;

function TMultiOperandCondition.GetOperand(idx : Integer) : TCondition;
begin
  Result := TCondition(FOperands[idx]);
end;

constructor TMultiOperandCondition.CreateOwned(Owner : TCondition);
begin
  inherited Create(Owner);
  FOperands := TList.Create;
end;

destructor TMultiOperandCondition.Destroy;
var
  i : Integer;
begin
  inherited Destroy;

  for i := 0 to Count - 1 do
    Operand[i].Free;
  FOperands.Free;
end;

procedure TMultiOperandCondition.AddOperand(ACondition : TCondition);
begin
  if ACondition.Owner <> nil then
    raise Exception.Create('TMultiOperandCondition.AddOperand: Cannot add a condition that has an owner!');

  FOperands.Add(ACondition);
  TConditionLink(ACondition).FOwner := Self;
end;

procedure TMultiOperandCondition.RemoveOperand(ACondition : TCondition);
begin
  FOperands.Remove(ACondition);
  ACondition.Free;
end;

procedure TMultiOperandCondition.Remove(idx : Integer);
begin
  TObject(FOperands.Items[idx]).Free;
  FOperands.Delete(idx);
end;

function TMultiOperandCondition.Equals(Condition : TCondition) : Boolean;
var // Fixa LGE
  iSelf, iCond : Integer;
begin
  Result := (Self.ClassType = Condition.ClassType) and
            (Self.Count = TMultiOperandCondition(Condition).Count);

  if Result then
  begin
    for iSelf := 0 to Self.Count - 1 do
    begin
      Result := False;

      for iCond := 0 to TMultiOperandCondition(Condition).Count - 1 do
        if Self.Operand[iSelf].Equals(TMultiOperandCondition(Condition).Operand[iCond]) then
        begin
          Result := True;
          Break;
        end;

      if Result = False then
        Break;
    end;
  end;
end;

procedure TMultiOperandCondition.ProcessFields(Proc : TProcFieldCond);
var
  i : Integer;
begin
  for i := 0 to Count - 1 do
    Operand[i].ProcessFields(Proc);
end;

{ TAndCondition }

{ifndef TRANSLATOR}
constructor TAndCondition.Create;
begin
  inherited CreateOwned(nil);
end;

constructor TAndCondition.CreateFromArray(ConditionArray : array of TCondition);
var
  i : Integer;
begin
  Create;

  for i := Low(ConditionArray) to High(ConditionArray) do
    AddOperand(ConditionArray[i]);
end;

destructor TAndCondition.Destroy;
begin
  inherited Destroy;
end;

function TAndCondition.AcceptsRow(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean;
var
  i : Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
  begin
    if TConditionLink(Operand[i]).ConcernsTable(ARow.DataTable) and
       not Operand[i].AcceptsRow(ARow) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function TAndCondition.AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean;
var
  i : Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
  begin
    if not Operand[i].AcceptsRowDependent(ARow, AffectedFields) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function TAndCondition.IsPureAndCondition : Boolean;
var
  i : Integer;
begin
  Result := True;

  for i := 0 to Count - 1 do
    if not Operand[i].IsPureAndCondition then
    begin
      Result := False;
      Break;
    end;
end;

function TAndCondition.CreateCopy : TCondition;
var
  AndCondition : TAndCondition;
  i : Integer;
begin
  AndCondition := TAndCondition.Create;
  for i := 0 to Count - 1 do
    AndCondition.AddOperand(Self.Operand[i].CreateCopy);
  Result := AndCondition;
end;

function TAndCondition.CreateFieldTranslatedCopy(TranslateField : TQueryDataFieldFunction) : TCondition;
var
  AndCondition : TAndCondition;
  i : Integer;
  Condition : TCondition;
begin
  AndCondition := TAndCondition.Create;
  for i := 0 to Count - 1 do
  begin
    Condition := Self.Operand[i].CreateFieldTranslatedCopy(TranslateField);
    if Condition <> nil then
      AndCondition.AddOperand(Condition);
  end;
  Result := AndCondition;
end;

function TAndCondition.AcceptsAllInTable(DataTable : TDataTAble) : Boolean;
var
  i : Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if TConditionLink(Operand[i]).ConcernsTable(DataTable) and
       (not Operand[i].AcceptsAllInTable(DataTable)) then
    begin
      Result := False;
      Break;
    end;
end;

function TAndCondition.AcceptsNoValuesForTable(DataTable : TDataTable) : Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if TConditionLink(Operand[i]).ConcernsTable(DataTable) and
       Operand[i].AcceptsNoValuesForTable(DataTable) then
    begin
      Result := True;
      Break;
    end;
end;

function TAndCondition.AcceptsAllForField(DataField : TDataField) : Boolean;
var
  i : Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if not Operand[i].AcceptsAllForField(DataField) then
    begin
      Result := False;
      Break;
    end;
end;

function TAndCondition.AcceptsNoValuesForField(DataField : TDataField) : Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if Operand[i].AcceptsNoValuesForField(DataField) then
    begin
      Result := True;
      Break;
    end;
end;

function TAndCondition.AcceptsValue(AField : TDataField; AValue : TValue) : Boolean;
var
  i : Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if TConditionLink(Operand[i]).ConcernsField(AField) and
       not Operand[i].AcceptsValue(AField, AValue) then
    begin
      Result := False;
      Break;
    end;
end;

function TAndCondition.GetLargerCriteria(
  var FreeCriteria: Boolean): TCondition {TCriteria};
var
  i : Integer;
  OperandCriteria, OldResult : TCriteria;
  FreeOperandCriteria : Boolean;
begin
  if Count = 0 then
  begin
    Result := TCriteria.Create;
    FreeCriteria := True;
  end
  else
  begin
    Result := Operand[0].GetLargerCriteria(FreeCriteria);

    for i := 1 to Count - 1 do
    begin
      OperandCriteria := TCriteria(Operand[i].GetLargerCriteria(FreeOperandCriteria));
      OldResult := TCriteria(Result);

      Result := TCriteria.CreateIntersection(OldResult, OperandCriteria);
      if (i > 1) or FreeCriteria then
        OldResult.Free;
      if FreeOperandCriteria then
        OperandCriteria.Free;
    end;

    if Count > 1 then
      FreeCriteria := True;
  end;
end;

function TAndCondition.GetSmallerCriteria(
  var FreeCriteria: Boolean): TCondition {TCriteria};
var
  iC : Integer;
  OperandCriteria : TCriteria;
  FreeOperandCriteria : Boolean;
  Iterator : TCriteriaFieldIterator;
begin
  if Count = 0 then
  begin
    Result := TCriteria.Create;
    FreeCriteria := True;
  end
  else if Count = 1 then
  begin
    Result := Operand[0].GetSmallerCriteria(FreeCriteria);
  end
  else
  begin
    Result := TCriteria.Create;
    FreeCriteria := True;

    for iC := 0 to Count - 1 do
    begin
      OperandCriteria := TCriteria(Operand[iC].GetSmallerCriteria(FreeOperandCriteria));

      Iterator := TCriteriaFieldIterator.Create(OperandCriteria);
      while not Iterator.EOF do
      begin
        if (not Iterator.CriteriaField.HasNoConditions) and
           (not Iterator.CriteriaField.AcceptsAnyValue) then
          TCriteria(Result)[Iterator.DataField].AcceptNone;
        Iterator.Next;
      end;
      Iterator.Free;

      if FreeOperandCriteria then
        OperandCriteria.Free;
    end;
  end;
end;



(*
function TMultiOperandCondition.DoCopyValues( FromField, ToField : TDataField; CreateUnion, KeepOld : Boolean ) : TCondition;
var
  i : Integer;
  TmpCon : TCondition;
begin
  Result := Self;
  for i := Count - 1 downto 0 do
  begin
    TmpCon := TConditionLink(Operand[i]).DoCopyValues(FromField, ToField, CreateUnion, KeepOld);
    if TmpCon <> nil then
    begin
      FOperands[i] := TmpCon;
      TConditionLink(TmpCon).FOwner := Self;
    end
    else
      FOperands.Delete( i );
  end;

  if Count = 0 then
  begin
    Result := nil;
    Self.Free;
  end
  else if Count = 1 then
  begin
    Result := Operand[0];
    TConditionLink(Result).FOwner := nil;
    FOperands.Delete( 0 );
    Self.Free;
  end
end;
*)
function TAndCondition.AcceptsExactlyOneValue(DataField : TDataField; var Value : TValue) : Boolean;
var
  i, j : Integer;
begin
  Result := False;

  for i := 0 to Count - 1 do
    if Operand[i].AcceptsExactlyOneValue(DataField, Value) then
    begin
      Result := True;

      for j := 0 to Count - 1 do
        if (i <> j) and
           (not Operand[j].AcceptsValue(DataField, Value)) then
        begin
          Result := False;
          Exit;
        end;

      Exit;
    end;
end;

function TAndCondition.GetCommonQuilt(
  out FreeQuilt: Boolean): TCommonQuilt;
var
  Quilts : TList;
  i : Integer;
  AQuilt : TQuilt;
begin
{$ifndef LINUX}
  Quilts := TObjectList.Create(True);
  try
    for i := 0 to Count -1 do
      Quilts.Add( Operand[i].CreateCommonQuilt );

    if Quilts.Count > 0 then
    begin
      AQuilt := TQuilt.Create;
      AQuilt.CopyFromCommon( Quilts[0] );

      for i := 1 to Quilts.Count -1 do
        AQuilt.Intersection( Quilts[i] );

      Result := AQuilt;
    end
    else
      Result := nil;
  finally
    Quilts.Free;
  end;
{$endif LINUX}
end;
{endif TRANSLATOR}

{ TOrCondition }



{ TCriteria }

procedure TCriteria.DoWriteToStream(AStream : TStream; Indent : Integer);
var
  Iterator : TCriteriaFieldIterator;
  First : Boolean;
begin
  WriteName(AStream);
  Iterator := TCriteriaFieldIterator.Create(Self);
  First := True;
  while not Iterator.EOF do
  begin
    if First then
      First := False
    else
      WriteNewLine(AStream, Indent + Length(ClassName) + 1);
    Iterator.CriteriaField.DoWriteToStream(AStream, Indent + Length(ClassName) + 1);
    Iterator.Next;
  end;
  Iterator.Free;
  WriteCloseParenteses(AStream);
end;

constructor TCriteria.DoCreateFromStream(AStream : TStream);
var
  ConditionClass : TConditionClass;
  AField : TCriteriaField;
begin
  Create;
  while GetNextStreamItem(AStream, ConditionClass) do
  begin
    if ConditionClass <> TCriteriaField then
      raise Exception.Create('Invalid stream format!');

    AField := TCriteriaField.DoCreateFromStream(AStream);
    Self[AField.DataField] := AField;
    AField.Free;
  end;

  if not SkipToObjectEnd(AStream) then
    raise Exception.Create('Invalid stream format!');
end;

function TCriteria.ConcernsField(DataField : TDataField) : Boolean;
var
  CritField : TObject;
begin
  Result := FFields.Contains(DataField, CritField) and
            TConditionLink(CritField).ConcernsField(DataField);
end;

function TCriteria.ConcernsTable(DataTable : TDataTable) : Boolean;
var
  Iterator : TCriteriaFieldIterator;
begin
  Result := False;

  Iterator := TCriteriaFieldIterator.Create(Self);
  while not Iterator.EOF do
  begin
    if TConditionLink(Iterator.CriteriaField).ConcernsTable(DataTable) then
    begin
      Result := True;
      Break;
    end;
    Iterator.Next;
  end;
  Iterator.Free;
end;

procedure TCriteria.DefineProperties(Filer: TFiler);
begin
//  Filer.DefineProperty('CritValues', ReadCriteria, WriteCriteria, True);

  inherited DefineProperties(Filer);
//  if Filer is TReader then DoGlobalFixup(Self);
end;

{procedure TCriteria.WriteCriteria(Writer: TWriter);
var
  i : Integer;
  FField : TCriteriaField;
begin
  Writer.WriteListBegin;
  for i := 0 to Self.FieldCount - 1 do
  begin
    FField := Self.Fields[i];
    Writer.WriteString(FField.Caption);
    Writer.WriteIdent(FField.DataField.Name);
  end;
  Writer.WriteListEnd;
end;

procedure TCriteria.ReadCriteria(Reader: TReader);
var
  FCount : Integer;
  FComponent : TComponent;
  FCaption, FComponentName : String;
begin
  FCount := 0;
  Reader.ReadListBegin;
  try
    while not Reader.EndOfList do
    begin
      FCaption := Reader.ReadString;
      FComponentName := Reader.ReadIdent;
      FComponent := LocateComponentByName(Self, piCriteriaValue, FComponentName, FCount, FCaption);
      if FComponent is TDataField then Self.AddString(TDataField(FComponent), FCaption);
      Inc(FCount);
    end;
  finally
    Reader.ReadListEnd;
  end;
end;
}



function TCriteria.InternalHasRowsThatMatch(Handler : TGetRowsHandler; SubTotal : TSubTotalRow) : Boolean;
var
  KeyField : TDataField;
  i : Integer;
  ACritField : TCriteriaField;
begin
  if Handler.AllowExcludeOnSubTotalLevel then
  begin
    KeyField := SubTotal.SubTotalKey.TreeKey;
    if not (Handler.LastLevelNeedCheckAllFields and SubTotal.IsLastTreeNode) then
    begin
      ACritField := FindCriteriaField(KeyField, False);
      if (ACritField <> nil) and ACritField.HasExactlyOneValue then
      begin
        if SubTotal.IsLastTreeNode then
          Result := TSubTotalRowLink(SubTotal).FindValue(ACritField.OnlyValue, i)
        else
          Result := TSubTotalRowLink(SubTotal).FindValue(ACritField.OnlyValue, i) and
                    TSubTotalRowLink(SubTotal.SubRows[i]).HasRowsThatMatch(Handler); // (Self, ExcludeOnSubTotalLevel, LastLevelCheckAllFields);
      end
      else
        Result := inherited InternalHasRowsThatMatch(Handler, SubTotal); // (SubTotal, ExcludeOnSubTotalLevel, LastLevelCheckAllFields);
    end
    else
      Result := inherited InternalHasRowsThatMatch(Handler, SubTotal); // (SubTotal, ExcludeOnSubTotalLevel, LastLevelCheckAllFields);
  end
  else
    Result := inherited InternalHasRowsThatMatch(Handler, SubTotal); // (SubTotal, ExcludeOnSubTotalLevel, LastLevelCheckAllFields);
end;
(*
function TCriteria.DoCopyValues( FromField, ToField : TDataField; CreateUnion, KeepOld : Boolean ) : TCondition;
var
  FromCritField : TCriteriaField;
  Iterator : TIndexContainerIterator;
  Cnt : Integer;
begin
  Result := Self;
  if FromField = nil then
  begin
    if ToField <> nil then
      CriteriaField[ToField].Reset
  end
  else
  begin
    FromCritField := CriteriaField[FromField];
    if ToField <> nil then
    begin
      if CreateUnion and
         not FromCritField.HasNoConditions then
        CriteriaField[ToField].UnionAddFrom(FromCritField)
      else
        CriteriaField[ToField] := FromCritField;
    end;
    if not KeepOld then
      FromCritField.Reset;
  end;
  Cleanup;

  Iterator := TIndexContainerIterator.Create(FFields);
  Cnt := 0;
  while (Cnt < 2) and (not Iterator.EOF) do
  begin
    Inc(Cnt);
    Iterator.Next;
  end;

  if (Cnt = 1) then
  begin
    Iterator.First;
    Result := TCriteriaField(Iterator.Data);
    TConditionLink(Result).FOwner := nil;
{$ifdef D4_OR_HIGHER}
    FFields.Remove(TCriteriaField(Result).DataField);
{$else}
    FFields.RemoveOL(TCriteriaField(Result).DataField);
{$endif D4_OR_HIGHER}
    Self.Free;
  end
  else if (FieldCount = 0) then
  begin
    Result := nil;
    Self.Free;
  end;
  Iterator.Free;
end;
*)
constructor TCriteria.Create;
begin
  inherited Create(nil);

  FFields := TIndexContainer.Create(HASHSIZE, True);
end;

procedure TCriteriaField.ApplyFromSource(Criteria : TCriteria; DataTable : TDataTable; AndConditions : Boolean);
begin
  Criteria.Apply(DataTable, AndConditions, nil, Self);
end;

procedure TCriteria.ApplyFromSource(Criteria : TCriteria; DataTable : TDataTable; AndConditions : Boolean);
var
  i : Integer;
  CritField : TCriteriaField;
  Iterator : TCriteriaFieldIterator;
begin
  if AndConditions then
  begin
    for i := 0 to DataTable.FieldCount - 1 do
    begin
      CritField := TCriteriaField.CreateApplied(DataTable.Field[i], Self, AndConditions);
      if not CritField.HasNoConditions then
        Criteria[DataTable.Field[i]] := CritField;
      CritField.Free;
    end;
  end
  else
  begin
    Iterator := TCriteriaFieldIterator.Create(Self);
    while not Iterator.EOF do
    begin
      Criteria.Apply(DataTable, AndConditions, Self, Iterator.CriteriaField);
      Iterator.Next;
    end;
    Iterator.Free;
  end;
end;

constructor TCriteria.CreateApplied(DataTable : TDataTable; Source : TNiceCondition; AndConditions : Boolean);
begin
  Create;
  Source.ApplyFromSource(Self, DataTable, AndConditions);
end;

procedure TCriteria.Apply(DataTable : TDataTable; AndConditions : Boolean; SuperKeys : TCriteria; CriteriaField : TCriteriaField);
var
  DataField : TDataField;
  CalcedCriteriaField : TCriteriaField;
begin
  DataField := DataTable.FindDependentSource(CriteriaField.DataField);
  if DataField = nil then
    Exit;

  CalcedCriteriaField := TCriteriaField.CreateAppliedFromSingleField(DataField, CriteriaField, SuperKeys);

  if not Self.HasRestrictionsOnField(DataField) then
  begin
    if (SuperKeys = nil) or SuperKeys.HasRestrictionsOnField(CriteriaField.DataField) then
      Self.FFields.Add(CalcedCriteriaField.DataField, TCriteriaField.CreateOwnedCopy(Self, CalcedCriteriaField))
    else
      Self.FFields.Add(CriteriaField.DataField, TCriteriaField.CreateOwnedCopy(Self, CriteriaField));
  end
  else if AndConditions then
    Self.CriteriaField[DataField].IntersectWith(CalcedCriteriaField)
  else
    Self.CriteriaField[DataField].UnionAddFrom(CalcedCriteriaField);

  CalcedCriteriaField.Free;
end;

constructor TCriteria.CreateIntersection(Crit1, Crit2 : TCriteria);
var
  Iterator : TCriteriaFieldIterator;
  ACritField : TCriteriaField;
begin
  Assert(Crit1 <> nil, 'TCriteria.CreateIntersection: Crit1 <> nil');
  Assert(Crit2 <> nil, 'TCriteria.CreateIntersection: Crit2 <> nil');

  Create;

  Iterator := TCriteriaFieldIterator.Create(Crit1);
  while not Iterator.EOF do
  begin
    ACritField := Crit2.FindCriteriaField(Iterator.DataField, False);
    if ACritField <> nil then
      FFields.Add(Iterator.DataField, TCriteriaField.CreateOwnedIntersection(Self, Iterator.DataField, Iterator.CriteriaField, ACritField))
    else
      FFields.Add(Iterator.DataField, TCriteriaField.CreateOwnedCopy(Self, Iterator.CriteriaField));
    Iterator.Next;
  end;
  Iterator.Free;

  Iterator := TCriteriaFieldIterator.Create(Crit2);
  while not Iterator.EOF do
  begin
    ACritField := Crit1.FindCriteriaField(Iterator.DataField, False);

    if ACritField = nil then
      FFields.Add(Iterator.DataField, TCriteriaField.CreateOwnedCopy(Self, Iterator.CriteriaField));

    Iterator.Next;
  end;
  Iterator.Free;
end;

constructor TCriteria.CreateUnion(Crit1, Crit2 : TCriteria);
var
  Iterator : TCriteriaFieldIterator;
  ACritField : TCriteriaField;
begin
  Assert(Crit1 <> nil, 'TCriteria.CreateUnion: Crit1 <> nil');
  Assert(Crit2 <> nil, 'TCriteria.CreateUnion: Crit2 <> nil');

  Create;

  Iterator := TCriteriaFieldIterator.Create(Crit1);
  while not Iterator.EOF do
  begin
    ACritField := Crit2.FindCriteriaField(Iterator.DataField, False);
    if ACritField <> nil then
      FFields.Add(Iterator.DataField, TCriteriaField.CreateOwnedUnion(Self, Iterator.DataField, Iterator.CriteriaField, ACritField));
    Iterator.Next;
  end;
  Iterator.Free;
end;

constructor TCriteria.CreateFieldUnion(Crit1, Crit2 : TCriteria);
var
  Iterator : TCriteriaFieldIterator;
begin
  Assert(Crit1 <> nil, 'TCriteria.CreateFieldUnion: Crit1 <> nil');
  Assert(Crit2 <> nil, 'TCriteria.CreateFieldUnion: Crit2 <> nil');

  Create;

  Iterator := TCriteriaFieldIterator.Create(Crit1);
  while not Iterator.EOF do
  begin
    if not Iterator.CriteriaField.HasNoConditions then
    begin
      if Crit2.HasConditionOnField(Iterator.DataField) then
        FFields.Add(Iterator.DataField, TCriteriaField.CreateOwnedUnion(Self, Iterator.DataField, Iterator.CriteriaField, Crit2))
      else
        FFields.Add(Iterator.DataField, TCriteriaField.CreateOwnedCopy(Self, Iterator.CriteriaField));
    end;
    Iterator.Next;
  end;
  Iterator.Free;

  Iterator := TCriteriaFieldIterator.Create(Crit2);
  while not Iterator.EOF do
  begin
    if not Crit1.HasConditionOnField(Iterator.DataField) then
      FFields.Add(Iterator.DataField, TCriteriaField.CreateOwnedCopy(Self, Iterator.CriteriaField));
    Iterator.Next;
  end;
  Iterator.Free;
end;

constructor TCriteria.CreateHierarchicalFieldUnion(Crit1, Crit2 : TCriteria);
  function HierarchicCheck(Crit:TCriteria; AField : TDataField) : Boolean;
  var
    j : Integer;
    Iterator : TCriteriaFieldIterator;
  begin
    Result := False;

    Iterator := TCriteriaFieldIterator.Create(Crit);
    while not Iterator.EOF do
    begin
      if (Iterator.DataField = AField) and not (Crit.HasRestrictionsOnField( Iterator.DataField )) then
      begin
        Result := True;
        Break;
      end;

      if Iterator.DataField is TKeyField then // Fixa LAA
        for j := 0 to Iterator.DataField.AncestorCount-1 do
//          if HierarchicCheck(Crit, Crit.Fields[i].DataField.Ancestor[j]) then
         if (Iterator.DataField.Ancestor[j] = AField) and not (Crit.HasRestrictionsOnField( Iterator.DataField )) then

          begin
            Result := True;
            Break;
          end;
      Iterator.Next;
    end;
    Iterator.Free;
  end;

var
  Iterator : TCriteriaFieldIterator;
begin
  Assert(Crit1 <> nil, 'TCriteria.CreateHierarchicalFieldUnion: Crit1 = nil');
  Assert(Crit2 <> nil, 'TCriteria.CreateHierarchicalFieldUnion: Crit2 = nil');

  Create;

  Iterator := TCriteriaFieldIterator.Create(Crit1);
  while not Iterator.EOF do
  begin
    if not (Iterator.CriteriaField.HasNoConditions or HierarchicCheck(Crit2, Iterator.DataField)) then
    begin
      if Crit2.HasRestrictionsOnField( Iterator.DataField ) then
        FFields.Add(Iterator.DataField, TCriteriaField.CreateOwnedUnion(Self, Iterator.DataField, Iterator.CriteriaField, Crit2))
      else
        FFields.Add(Iterator.DataField, TCriteriaField.CreateOwnedCopy(Self, Iterator.CriteriaField));
    end;
    Iterator.Next;
  end;
  Iterator.Free;

  Iterator := TCriteriaFieldIterator.Create(Crit2);
  while not Iterator.EOF do
  begin
    if not (Crit1.HasConditionOnField(Iterator.DataField) or HierarchicCheck(Crit2, Iterator.DataField)) then
      FFields.Add(Iterator.DataField, TCriteriaField.CreateOwnedCopy(Self, Iterator.CriteriaField));
    Iterator.Next;
  end;
  Iterator.Free;
end;

constructor TCriteria.CreateFromRowKeys(ARow : TAbstractRow);
begin
  Create;
  AddValuesFromRowKeys(ARow);
end;

procedure TCriteria.AddValuesFromRowKeys(ARow : TAbstractRow);
var
  i : Integer;
  ATreeKey : TDataField;
begin
  Assert(ARow <> nil, 'TCriteria.CreateFromRowKeys: ARow <> nil');

  if ARow is TDataRow then
    for i := 0 to ARow.DataTable.KeyCount - 1 do
      CriteriaField[ARow.DataTable.Field[i]].AddValue(ARow[ARow.DataTable.Field[i]])
  else
    for i := 0 to TSubTotalRow(ARow).SubTotalKey.TreeKeyIndex - 1 do
    begin
      ATreeKey := ARow.Storage.TreeKey[i].TreeKey;
      CriteriaField[ ATreeKey ].AddValue(ARow[ ATreeKey ]);
    end;
end;

constructor TCriteria.CreateFromRowFields(ARow : TAbstractRow);
var
  i : Integer;
begin
  Assert(ARow <> nil, 'TCriteria.CreateFromRowKeys: ARow <> nil');

  Create;

  if ARow is TDataRow then
    for i := 0 to ARow.DataTable.FieldCount - 1 do
      CriteriaField[ARow.DataTable.Field[i]].AddValue(ARow[ARow.DataTable.Field[i]])
  else
  begin
    for i := 0 to TSubTotalRow(ARow).SubTotalKey.TreeKeyIndex - 1 do
      CriteriaField[ARow.DataTable.Field[i]].AddValue(ARow[ARow.DataTable.Field[i]]);
    for i := ARow.DataTable.KeyCount to ARow.DataTable.FieldCount - 1 do
      CriteriaField[ARow.DataTable.Field[i]].AddValue(ARow[ARow.DataTable.Field[i]])
  end;
end;

function TCriteria.CreateCopy : TCondition;
begin
  Result := CreateCopyOfCriteria;
end;

function TCriteria.CreateCopyOfCriteria : TCriteria;
var
  Iterator : TCriteriaFieldIterator;
begin
  Result := TCriteria.Create;

  if Self <> nil then
  begin
    Iterator := TCriteriaFieldIterator.Create(Self);
    while not Iterator.EOF do
    begin
      Result.FFields.Add(Iterator.DataField, TCriteriaField.CreateOwnedCopy(Result, Iterator.CriteriaField));
      Iterator.Next;
    end;
    Iterator.Free;
  end;
end;

function TCriteria.CreateFieldTranslatedCopy(TranslateField : TQueryDataFieldFunction) : TCondition;
var
  Copy : TCriteria;
  TransCritField, SrcCritField : TCriteriaField;
  SrcField, TransField : TDataField;
  ConflictAction : TConflictAction;
  KeepSrc, DestExist : Boolean;
begin
  Copy := TCriteria.Create;

  with TCriteriaFieldIterator.Create(Self) do
  begin
    while not EOF do
    begin
      ConflictAction := caOverwrite;
      KeepSrc := False;

      SrcField := DataField;
      SrcCritField := CriteriaField;

      TransField := TranslateField(SrcField, ConflictAction, KeepSrc);
      TransCritField := SrcCritField.CreateCopyWithField(TransField);

      if (TransCritField <> nil) and
         (TransField <> nil) and
         (not TransCritField.HasNoConditions) then  // Hmm... fundera LGE vad är riktigt vettigt...
      begin
        DestExist := Copy.FindCriteriaField(TransField, False) <> nil;

        if DestExist then
          case ConflictAction of
            caCurrent :;
            caOverwrite :
              Copy[TransField] := TransCritField;
            caUnion :
              Copy[TransField].UnionAddFrom(TransCritField);
            caIntersection :
              Copy[TransField].IntersectWith(TransCritField);
            else
              raise Exception.Create( Self.ClassName + '.CreateFieldTranslatedCopy: Unknown ConflictAction!' );
          end
        else
          Copy[TransField] := TransCritField;

        if KeepSrc then
          Copy[SrcField].UnionAddFrom(SrcCritField);
      end;

      TransCritField.Free;
      Next;
    end;
    Free;
  end;

  Result := Copy;
end;

destructor TCriteria.Destroy;
var
  Iterator : TIndexContainerIterator;
begin
  Iterator := TIndexContainerIterator.Create(FFields);
  while not Iterator.EOF do
  begin
    TConditionLink(Iterator.Data).FOwner := nil;
    TCriteriaField(Iterator.Data).Free;
    Iterator.Next;
  end;
  FFields.Free;
  Iterator.Free;

  inherited Destroy;
end;

procedure TCriteria.InternalGetRows(Handler : TGetRowsHandler; SubTotal : TSubTotalRow);
var
  KeyField : TDataField;
  ACritField : TCriteriaField;
begin
  if Handler.AllowExcludeOnSubTotalLevel then
  begin
    KeyField := SubTotal.SubTotalKey.TreeKey;

    if (not (Handler.LastLevelNeedCheckAllFields and SubTotal.IsLastTreeNode)) then
    begin
      ACritField := FindCriteriaField(KeyField, False);
      if (ACritField <> nil) and ACritField.HasExactlyOneValue then
        TSubTotalRowLink(SubTotal).__AddValue(Handler, Self[KeyField].OnlyValue)
      else
        inherited InternalGetRows(Handler, SubTotal);
    end
    else
      inherited InternalGetRows(Handler, SubTotal);
  end
  else
    inherited InternalGetRows(Handler, SubTotal);
end;

procedure TCriteria.RemoveChild(CritField : TCriteriaField);
begin
{$ifdef D4_OR_HIGHER}
  FFields.Remove(CritField.DataField);
{$else}
  FFields.RemoveOL(CritField.DataField);
{$endif D4_OR_HIGHER}
(*  i := FFields.IndexOf(CritField);
  if i >= 0 then
    FFields.Delete(i); *)
end;

(*
function TCriteria.SQLString;
var
  SQL : String;
  i : Integer;
begin
  Result := '';
  for i := 0 to FieldCount - 1 do
  begin
    SQL := Fields[i].FieldSQL;
    if SQL <> '' then
    begin
      if Result = '' then
        Result := SQL
      else
        Result := Result + ' and ' + SQL;
    end;
  end;
end;
*)

(*
function TCriteria.TableSQLString(DataTable : TDataTable; SQL : TStrings; Params : TParamList; StartingCond : String; CondsOnNonKeys : Boolean) : Integer;
var
  c : Integer;

  function WhereCond : String;
  begin
    if c = 0 then
      Result := StartingCond
    else
      Result := 'and';

    Inc(c);
  end;

var
  i, m : Integer;
  Cond : String;
begin
  c := 0;
  if CondsOnNonKeys then
    m := DataTable.FieldCount
  else
    m := DataTable.KeyCount;

  if Self <> nil then
  for i := 0 to m - 1 do
  begin
    Cond := Self.FieldSQLString(DataTable.Field[i], Params);
    if Cond <> '' then
      SQL.Add(WhereCond + ' ' + Cond);
  end;
  Result := c;
end;
*)



function TCriteria.EqualsOnAllButOneField(ACrit : TCriteria; var NonEqualField : TDataField) : Boolean;
var
  NonEqualCount : Integer;

  CompCFExists : Boolean;
  CompCF : TCriteriaField;
  Iterator : TCriteriaFieldIterator;
begin
  NonEqualCount := 0;

  Iterator := TCriteriaFieldIterator.Create(Self);
  while not Iterator.EOF do
  begin
    CompCF := ACrit.FindCriteriaField(Iterator.DataField, False);
    CompCFExists := (CompCF <> nil) and (not CompCF.AcceptsAnyValue);

    if Iterator.CriteriaField.AcceptsAnyValue and (not CompCFExists) then
    begin
      //
    end
    else if not CompCFExists then
    begin
      Inc(NonEqualCount);
      if NonEqualCount > 1 then
        Break;
      NonEqualField := Iterator.DataField;
    end
    else if not Iterator.CriteriaField.Equals(CompCF) then
    begin
      Inc(NonEqualCount);
      if NonEqualCount > 1 then
        Break;
      NonEqualField := Iterator.DataField;
    end;
    Iterator.Next;
  end;
  Iterator.Free;

  if NonEqualCount <= 1 then
  begin
    Iterator := TCriteriaFieldIterator.Create(ACrit);
    while not Iterator.EOF do
    begin
      CompCFExists := (Self.FindCriteriaField(Iterator.DataField, False) <> nil);
      if not CompCFExists then // else already checked
      begin
        if not Iterator.CriteriaField.AcceptsAnyValue then
        begin
          Inc(NonEqualCount);
          if NonEqualCount > 1 then
            Break;
          NonEqualField := Iterator.DataField;
        end;
      end;

      Iterator.Next;
    end;
    Iterator.Free;
  end;

  Result := (NonEqualCount = 1);
  if not Result then
    NonEqualField := nil;
end;

function TCriteria.Equals(Condition : TCondition) : Boolean;
var
  Iterator : TCriteriaFieldIterator;
begin
  if not (Condition is TCriteria) then
    Result := False
  else
  begin
    Result := True;

    Iterator := TCriteriaFieldIterator.Create(Self);
    while not Iterator.EOF do
    begin
      if not Iterator.CriteriaField.AcceptsAnyValue then
      begin
        if (not TCriteria(Condition).HasConditionOnField(Iterator.DataField)) or
           (not Iterator.CriteriaField.Equals(TCriteria(Condition)[Iterator.DataField])) then
        begin
          Result := False;
          Break;
        end;
      end;
      Iterator.Next;
    end;
    Iterator.Free;

    if not Result then
      Exit;

    Iterator := TCriteriaFieldIterator.Create(TCriteria(Condition));
    while not Iterator.EOF do
    begin
      if not Iterator.CriteriaField.AcceptsAnyValue then
      begin
        if (not Self.HasConditionOnField(Iterator.DataField)) or
           (not Iterator.CriteriaField.Equals(Self[Iterator.DataField])) then
        begin
          Result := False;
          Break;
        end;
      end;
      Iterator.Next;
    end;
    Iterator.Free;
  end;
end;

function TCriteria.AcceptsAllInTable(DataTable : TDataTable) : Boolean;
var
  iField : Integer;
begin
  Result := True;
  for iField := 0 to DataTable.FieldCount - 1 do
    if HasRestrictionsOnField(DataTable.Field[iField]) then
    begin
      Result := False;
      Break;
    end;
end;

function TCriteria.AcceptsNoValuesForTable(DataTable : TDataTable) : Boolean;
var
  Iterator : TCriteriaFieldIterator;
begin
  Result := False;

  if Self = nil then
    Exit;

  Iterator := TCriteriaFieldIterator.Create(Self);
  while not Iterator.EOF do
  begin
    if Iterator.CriteriaField.HasNoLegalValue and
       DataTable.TableHasField(Iterator.DataField) then
    begin
      Result := True;
      Break;
    end;
    Iterator.Next;
  end;
  Iterator.Free;
end;

function TCriteria.AcceptsAllForField(DataField : TDataField) : Boolean;
begin
  Result := not HasRestrictionsOnField(DataField);
end;

function TCriteria.AcceptsNoValuesForField(DataField : TDataField) : Boolean;
var
  ACritField : TCriteriaField;
begin
  ACritField := FindCriteriaField(DataField, False);
  Result := (ACritField <> nil) and ACritField.HasNoLegalValue;
end;

function TCriteria.AcceptsValue(AField : TDataField; AValue : TValue) : Boolean;
var
  ACritField : TCriteriaField;
begin
  ACritField := FindCriteriaField(AField, False);
{$ifdef D4_OR_HIGHER}
  Result := (ACritField = nil) or ACritField.Accepts(AValue);
{$else}
  Result := (ACritField = nil) or ACritField.AcceptsOL(AValue);
{$endif D4_OR_HIGHER}
end;

function TCriteria.GetLargerCriteria(var FreeCriteria: Boolean): TCondition {TCriteria};
begin
  Result := Self;
  FreeCriteria := False;
end;

function TCriteria.GetSmallerCriteria(var FreeCriteria: Boolean): TCondition {TCriteria};
begin
  Result := Self;
  FreeCriteria := False;
end;

function TCriteria.AcceptsExactlyOneValue(DataField : TDataField; var Value : TValue) : Boolean;
var
  ACritField : TCriteriaField;
begin
  ACritField := FindCriteriaField(DataField, False);
  Result := (ACritField <> nil) and ACritField.HasExactlyOneValue;
  if Result then
    Value := ACritField.OnlyValue;
end;

procedure TCriteria.Cleanup;
var
  Iterator : TIndexContainerIterator;
  Field : TCriteriaField;
begin
  Iterator := TIndexContainerIterator.Create(FFields);
  while not Iterator.EOF do
  begin
    Field := TCriteriaField(Iterator.Data);
    if Field.HasNoConditions then
    begin
      TConditionLink(Field).FOwner := nil;
      Field.Free;
{$ifdef D4_OR_HIGHER}
      FFields.Remove(Iterator.Index);
{$else}
      FFields.RemoveOL(Iterator.Index);
{$endif D4_OR_HIGHER}
    end;
    Iterator.Next;
  end;

  Iterator.Free;

(*  for i := FieldCount - 1 downto 0 do
    if Fields[i].HasNoConditions then
    begin
      Fields[i].FOwner := nil;
      Fields[i].Free;
      FFields.Delete(i);
    end; *)
end;

procedure TCriteria.ResetAll;
var
  Iterator : TIndexContainerIterator;
  Field : TCriteriaField;
begin
  Iterator := TIndexContainerIterator.Create(FFields);
  while not Iterator.EOF do
  begin
    Field := TCriteriaField(Iterator.Data);
    TConditionLink(Field).FOwner := nil;
    Field.Free;
{$ifdef D4_OR_HIGHER}
    FFields.Remove(Iterator.Index);
{$else}
    FFields.RemoveOL(Iterator.Index);
{$endif D4_OR_HIGHER}
    Iterator.Next;
  end;

  Iterator.Free;
end;

procedure TCriteria.PrettyPrint(IgnoreFields : TDataFieldSet; Output : TStrings);
begin
  PrettyPrintSkipFieldDescription(IgnoreFields, nil, Output);
end;

procedure TCriteria.PrettyPrintSkipFieldDescription(IgnoreFields, NoDescrFields : TDataFieldSet; Output : TStrings);

  function IsIgnoreField(AField : TCriteriaField) : boolean;
  begin
    if IgnoreFields<>nil then
      result := IgnoreFields.ContainsField(AField.DataField)
    else
      result := False;
  end;

  function IsSkipDescrField(DataField : TDataField) : boolean;
  begin
    if NoDescrFields<>nil then
      result := NoDescrFields.ContainsField(DataField)
    else
      result := False;
  end;

  function GetDescription(DataField : TDataField; AllowSkip : Boolean) : String;
  begin
    if AllowSkip and IsSkipDescrField(DataField) then
      Result := ''
    else
      Result := DataField.ShortDescription + ': ';
  end;

var
  FieldText : String;
  List : TDataRowList;
  Iterator : TCriteriaFieldIterator;
begin
  Iterator := TCriteriaFieldIterator.Create(Self);
  while not Iterator.EOF do
  begin
    if not (Iterator.CriteriaField.HasNoConditions or Iterator.CriteriaField.AcceptsAnyValue or
            Iterator.CriteriaField.HasNoLegalValue or IsIgnoreField(Iterator.CriteriaField)) then
    begin
      if Iterator.CriteriaField.HasExactlyOneValue then
      begin
        List := TDataRowList.Create;
        Iterator.DataField.GetValues(List, nil, nil, Self);
        if List.Count = 1 then
          FieldText := GetDescription(Iterator.DataField, True) + List.DataRows[0].DisplayText[Iterator.DataField]
                                   // List.DataRows[0].KeyAndDescription[Iterator.DataField]
        else
          FieldText := GetDescription(Iterator.DataField, False) + Iterator.CriteriaField.Caption;

        List.Free;
      end
      else
        FieldText := GetDescription(Iterator.DataField, False) + Iterator.CriteriaField.Caption;

      Output.Add(FieldText);
    end;
    Iterator.Next;
  end;
  Iterator.Free;
end;

function TCriteria.PrettyPrintApplied(IgnoreFields : TDataFieldSet) : string;
  function IsIgnoreField(AField : TCriteriaField) : boolean;
  begin
    if IgnoreFields<>nil then
      result := IgnoreFields.ContainsField(AField.DataField)
    else
      result := False;
  end;

var
  Iterator : TCriteriaFieldIterator;
  FieldText : string;
  List : TDataRowList;
begin
  result := '';
  Iterator := TCriteriaFieldIterator.Create(Self);
  while not Iterator.EOF do
  begin
    if not (Iterator.CriteriaField.HasNoConditions or Iterator.CriteriaField.AcceptsAnyValue or
            Iterator.CriteriaField.HasNoLegalValue or IsIgnoreField(Iterator.CriteriaField)) then
    begin
      FieldText := Iterator.DataField.ShortDescription + ': ';

      if (Iterator.DataField is TKeyField) and Iterator.CriteriaField.HasExactlyOneValue then
      begin
        List := TDataRowList.Create;
        Iterator.DataField.GetRows(List, Self);
        if List.Count = 1 then
          FieldText := FieldText + List.DataRows[0].DisplayText[Iterator.DataField]
        else
          FieldText := FieldText + Iterator.CriteriaField.Caption;

        List.Free;
      end
      else
        FieldText := FieldText + Iterator.CriteriaField.Caption;

      result := result + FieldText + '; ';
    end;
    Iterator.Next;
  end;
  Iterator.Free;
end;

{$ifdef D4_OR_HIGHER}
function TCriteria.AcceptsRow(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean;
{$else}
function TCriteria.AcceptsRow(ARow : TAbstractRow; AffectedFields : TDataFieldSet) : Boolean;
{$endif D4_OR_HIGHER}
var
  i : Integer;
  ACritField : TCriteriaField;
begin
  Assert(ARow <> nil, 'TCriteria.AcceptsRow: ARow <> nil');

  Result := False;

  for i := 0 to ARow.DataTable.FieldCount - 1 do
  begin
    if (ARow is TSubTotalRow) and (not TSubTotalRow(ARow).FieldHasValue(ARow.DataTable.Field[i])) then
    begin
      Continue;
    end
    else
    begin
      ACritField := FindCriteriaField(ARow.DataTable.Field[i], False);
{$ifdef D4_OR_HIGHER}
      if (ACritField <> nil) and
         (not ACritField.Accepts(ARow.ValueByIndex[i])) then
{$else}
      if (ACritField <> nil) and
         (not ACritField.AcceptsOL( ARow.ValueByIndex[i] )) then
{$endif D4_OR_HIGHER}
        Exit;
    end;
  end;

  Result := True;
end;

{$ifdef D4_OR_HIGHER}
function TCriteria.AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean;
{$else}
function TCriteria.AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet) : Boolean;
{$endif D4_OR_HIGHER}
begin
  Assert(ARow <> nil, 'TCriteria.AcceptsRow: ARow <> nil');
  Result := True;

  with TCriteriaFieldIterator.Create(Self) do
  try
    while not EOF do
    begin
      if not CriteriaField.AcceptsRowDependent(ARow, AffectedFields) then
      begin
        Result := False;
        Break;
      end;
      Next;
    end;
  finally;
    Free;
  end;
end;

procedure TCriteria.SetCriteriaField(Field : TDataField; NewCriteriaField : TCriteriaField);
begin
  Self[Field].CopyFrom(NewCriteriaField);
end;

function TCriteria.FindCriteriaField(Field : TDataField; DoCreate : Boolean) : TCriteriaField;
var
  CritField : TObject;
begin
  Assert(Field <> nil, 'TCriteria.FindCriteriaField: Field <> nil');


(*  for i := 0 to FieldCount - 1 do
    if Fields[i].DataField = Field then
    begin
      Result := Fields[i];
      Exit;
    end;*)
  if FFields.Contains(Field, CritField) then
  begin
    Result := TCriteriaField(CritField);
    Exit;
  end;

  if DoCreate then
  begin
    Result := TCriteriaField.CreateOwned(Self, Field);
    FFields.Add(Field, Result);
  end
  else
    Result := nil;
end;

function TCriteria.GetCriteriaField(Field : TDataField) : TCriteriaField;
begin
  Result := FindCriteriaField(Field, True);
end;

function TCriteria.FieldCount : Integer;
begin
  Result := FFields.ItemCount;
end;

(*
function TCriteria.GetFieldByIndex(idx : Integer) : TCriteriaField;
begin
  Assert((idx >= 0) and (idx < FFields.Count),
         'TCriteria.GetFieldByIndex: (idx >= 0) and (idx < FFields.Count), idx: ' + IntToStr(idx));

  Result := TCriteriaField(FFields[idx]);
end;
*)

(*
function TCriteria.HasCriteriaOnField(Field : TDataField) : Boolean;
var
  i : Integer;
begin
  Result := False;

  for i := 0 to FieldCount - 1 do
    if Fields[i].DataField = Field then
    begin
      Result := True;
      Exit;
    end;
end;
*)

function TCriteria.HasConditionOnField(Field : TDataField) : Boolean;
var
  CritField : TObject;
begin
  Result := FFields.Contains(Field, CritField) and
            (not TCriteriaField(CritField).HasNoConditions);

(*  Result := false;

    for i := 0 to FieldCount - 1 do
    if Fields[i].DataField = Field then
    begin
      if not Fields[i].HasNoConditions then
        Result := True;
      Exit;
    end; *)
end;

function TCriteria.HasRestrictionsOnField(Field : TDataField) : Boolean;
var
  CritField : TObject;
begin
  Result := FFields.Contains(Field, CritField) and
            (not TCriteriaField(CritField).AcceptsAnyValue);

(*  Result := false;

    for i := 0 to FieldCount - 1 do
    if Fields[i].DataField = Field then
    begin
      if not Fields[i].AcceptsAnyValue  then
        Result := True;
      Exit;
    end; *)
end;

procedure TCriteria.SaveToFile(FileName : String);
var
  TextFile : TStringList;
  Iterator : TCriteriaFieldIterator;
begin
  TextFile := TStringList.Create;

  Iterator := TCriteriaFieldIterator.Create(Self);
  while not Iterator.EOF do
  begin
    Iterator.CriteriaField.Print(TextFile);
    Iterator.Next;
  end;
  Iterator.Free;

  TextFile.SaveToFile(FileName);
  TextFile.Free;
end;

procedure TCriteria.LoadFromFile(FileName : String);
var
  i, cPos : Integer;
  TextFile : TStringList;
  Field : TDataField;
  FieldName, RestText : String;
begin
  Self.ResetAll;

  TextFile := TStringList.Create;
  TextFile.LoadFromFile(FileName);

  for i := 0 to TextFile.Count - 1 do
  begin
    cPos := Pos(':', TextFile[i]);
    if cPos <= 0 then
      Log(ltError, 'LoadFromFile', 'TCriteria.LoadFromFile: File format error.');

    FieldName := Copy(TextFile[i], 1, cPos - 1);
    Field := TDataField.FieldByName(FieldName);
    if Field = nil then
      Log(ltError, 'LoadFromFile', 'TCriteria.LoadFromFile: Unknown Field ' + FieldName);

    RestText := Copy(TextFile[i], cPos + 1, Length(TextFile[i]));
    if RestText = '' then
    begin
      {Ok}
    end
    else if RestText[1] = '-' then
      CriteriaField[Field].AcceptNone
    else if RestText[1] = '+' then
      CriteriaField[Field].AcceptAll
    else
      CriteriaField[Field].Caption := RestText;
  end;

  TextFile.Free;
end;

procedure TCriteria.SaveToDB(RowStorage : TAbstractRowStorage; DefaultValues : TDataRow; FieldField, FromField : TDataField; ToField : TDataField; EmptyFirst : Boolean);
var
  List : TDataRowList;
  i : Integer;
  GetCriteria : TCriteria;
  Iterator : TCriteriaFieldIterator;

begin
  Assert(RowStorage <> nil, 'TCriteria.SaveToDB: RowStorage <> nil');
  Assert(DefaultValues <> nil, 'TCriteria.SaveToDB: DefaultValues <> nil');
  Assert(FieldField <> nil, 'TCriteria.SaveToDB: FieldField <> nil');
  Assert(FromField <> nil, 'TCriteria.SaveToDB: FromField <> nil');
  Assert(ToField <> nil, 'TCriteria.SaveToDB: ToField <> nil');

  if EmptyFirst then
  begin
    List := TDataRowList.Create;
    GetCriteria := TCriteria.CreateFromRowKeys(DefaultValues);
    GetCriteria[FieldField].Reset;
    GetCriteria[FromField].Reset;
    GetCriteria[ToField].Reset;
    RowStorage.GetRows(List, GetCriteria, gaReference);
    for i := 0 to List.Count - 1 do
      List.DataRows[i].Delete;
    List.Free;
    GetCriteria.Free;
  end;

  Iterator := TCriteriaFieldIterator.Create(Self);
  while not Iterator.EOF do
  begin
    Iterator.CriteriaField.SaveToDB(RowStorage, DefaultValues,FieldField, FromField,ToField, false);
    Iterator.Next;
  end;
  Iterator.Free;
end;

procedure TCriteria.LoadFromDB(RowStorage : TAbstractRowStorage; DefaultValues : TDataRow; FieldField, FromField : TDataField; ToField : TDataField);
var
  List : TDataRowList;
  i : Integer;
  GetCriteria : TCriteria;
  FieldName : String;
  Field : TDataField;
begin
  Assert(RowStorage <> nil, 'TCriteria.LoadFromDB: RowStorage <> nil');
  Assert(DefaultValues <> nil, 'TCriteria.LoadFromDB: DefaultValues <> nil');
  Assert(FieldField <> nil, 'TCriteria.LoadFromDB: FieldField <> nil');
  Assert(FromField <> nil, 'TCriteria.LoadFromDB: FromField <> nil');
  Assert(ToField <> nil, 'TCriteria.LoadFromDB: ToField <> nil');

  Self.ResetAll;

  List := TDataRowList.Create;
  GetCriteria := TCriteria.CreateFromRowKeys(DefaultValues);
  GetCriteria[FieldField].Reset;
  GetCriteria[FromField].Reset;
  RowStorage.GetRows(List, GetCriteria, gaReference);
  GetCriteria.Free;

  for i := 0 to List.Count - 1 do
  begin
    FieldName := List.DataRows[i].StringValue[FieldField];
    Field := TDataField.FieldByName(FieldName);
    if Field = nil then
      Log(ltWarning, 'LoadFromFile', 'TCriteria.LoadFromDB: Unknown Field ' + FieldName)
    else
      CriteriaField[Field].AddValuesFromRow(List.DataRows[i], FromField, ToField);
  end;
  List.Free;
end;

procedure TCriteria.CopyFromCriteria(Crit : TCriteria);
var
  Iterator : TCriteriaFieldIterator;
begin
  if Crit = Self then
    Exit;

  Self.ResetAll;

  if Crit = nil then
    Exit;

  Iterator := TCriteriaFieldIterator.Create(Crit);
  while not Iterator.EOF do
  begin
    FFields.Add(Iterator.DataField, TCriteriaField.CreateOwnedCopy(Self, Iterator.CriteriaField));
    Iterator.Next;
  end;
  Iterator.Free;
end;

function TCriteria.CreateTableKeyCriteria(DataTable : TDataTable) : TCriteria;
var
  i : Integer;
begin
  Result := TCriteria.Create;

(*  for i := 0 to Self.FieldCount - 1 do
    if DataTable.TableHasKey(Self.Fields[i].DataField) then
      Result[Self.Fields[i].DataField] := Self.Fields[i]; *)
  for i := 0 to DataTable.KeyCount - 1 do
    Result[DataTable.Field[i]] := Self[DataTable.Field[i]];
end;

// ---------------------- Old TCriteria methods -----------------------------------------------------

function TCriteria.GetFieldCaption(AField : TDataField) : String;
begin
  Assert(AField <> nil, 'TCriteria.GetFieldCaption: AField <> nil');

  Result := CriteriaField[AField].Caption;
end;

procedure TCriteria.SetFieldCaption(AField : TDataField; NewCaption : String);
begin
  Assert(AField <> nil, 'TCriteria.SetFieldCaption: AField <> nil');

  CriteriaField[AField].Caption := NewCaption;
end;

{
function TCriteria.LocateField(Field : TDataField) : TCriteriaField;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to FFields.Count - 1 do
    if TCriteriaField(FFields[i]).SingleValues.Field = Field then
    begin
      Result := TCriteriaField(FFields[i]);
      Exit;
    end;
end;

function TCriteria.IndexOfField(Field : TDataField) : Integer;
var
  i : Integer;
begin
  for i := 0 to FFields.Count - 1 do
    if TCriteriaField(FFields[i]).SingleValues.Field = Field then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;
 }

{
function TCriteria.GetCaption(Field : TDataField) : String;
begin
  Result := CriteriaField[Field].Caption;
end;

procedure TCriteria.SetCaption(Field : TDataField; NewCaption : String);
begin
  CriteriaField[Field].Caption := NewCaption;
end;
 }


procedure TCriteria.SyncFromListBox(Field : TDataField; AListBox : TListBox; ClearIfAllSelected : Boolean);
begin
  Assert(Field <> nil, 'TCriteria.SyncFromListBox: Field <> nil');
  Assert(AListBox <> nil, 'TCriteria.SyncFromListBox: AListBox <> nil');

  CriteriaField[Field].SyncFromListBox(AListBox, ClearIfAllSelected);
end;

procedure TCriteria.SyncToListBox(Field : TDataField; AListBox : TListBox; ClearIfAllSelected : Boolean);
begin
  Assert(Field <> nil, 'TCriteria.SyncToListBox: Field <> nil');
  Assert(AListBox <> nil, 'TCriteria.SyncToListBox: AListBox <> nil');

  CriteriaField[Field].SyncToListBox(AListBox, ClearIfAllSelected);
end;
{$ifndef LINUX}
{$endif LINUX}


procedure TCriteria.AddValue(Field : TDataField; V : TValue);
begin
  Assert(Field <> nil, 'TCriteria.AddValue: Field <> nil');

  CriteriaField[Field].AddValue(V);
end;

procedure TCriteria.AddString(Field : TDataField; S : String);
begin
  Assert(Field <> nil, 'TCriteria.AddString: Field <> nil');

  CriteriaField[Field].AddString(S);
end;

procedure TCriteria.AcceptNoValues(Field : TDataField);
begin
  Assert(Field <> nil, 'TCriteria.AcceptNoValues: Field <> nil');

  CriteriaField[Field].AcceptNone;
end;

procedure TCriteria.AddAllValues(Field : TDataField);
begin
  Assert(Field <> nil, 'TCriteria.AddAllValues: Field <> nil');

  CriteriaField[Field].AcceptAll;
end;

procedure TCriteria.AddInterval(Field : TDataField; F, T : TValue);
begin
  Assert(Field <> nil, 'TCriteria.AddInterval: Field <> nil');

  CriteriaField[Field].AddInterval(F, T);
end;

(*
function TCriteria.FieldUnknown(Field : TDataField) : Boolean;
begin
  Assert(Field <> nil, 'TCriteria.FieldUnknown: Field <> nil');

  Result := not HasCriteriaOnField(Field);
end;
*)

function TCriteria.ValueLegal(Field : TDataField; Value : TValue) : Boolean;
begin
  Assert(Field <> nil, 'TCriteria.ValueLegal: Field <> nil');

{$ifdef D4_OR_HIGHER}
  Result := CriteriaField[Field].Accepts(Value);
{$else}
  Result := CriteriaField[Field].AcceptsOL(Value);
{$endif D4_OR_HIGHER}
end;

function TCriteria.AllValuesLegal(Field : TDataField) : Boolean;
begin
  Assert(Field <> nil, 'TCriteria.AllValuesLegal: Field <> nil');

  Result := CriteriaField[Field].AcceptsAnyValue;
end;

function TCriteria.NoValuesLegal(Field : TDataField) : Boolean;
begin
  Assert(Field <> nil, 'TCriteria.NoValuesLegal: Field <> nil');

  Result := CriteriaField[Field].HasNoLegalValue;
end;

function TCriteria.IntervalLegal(Field : TDataField; F, T : TValue) : Boolean;
begin
  Assert(Field <> nil, 'TCriteria.IntervalLegal: Field <> nil');

  Result := CriteriaField[Field].AcceptsInterval(F, T);
end;

(*
function TCriteria.FieldSQLString(Field : TDataField; Params : TParamList) : String;
begin
  Assert(Field <> nil, 'TCriteria.FieldSQLString: Field <> nil');

  Result := CriteriaField[Field].FieldParamSQL(Field.FieldName, Params);
end;
*)

function TCriteria.IsFieldSubCriteria(CmpCriteria : TCriteria; Field : TDataField) : Boolean;
begin
  Assert(Field <> nil, 'TCriteria.IsFieldSubCriteria: Field <> nil');

  Result := CriteriaField[Field].ParamCriteriaIsSubCriteria(CmpCriteria);
end;

function TCriteria.ParamCriteriaIsSubCriteria(CmpCriteria : TCriteria) : Boolean;
var
  Iterator : TCriteriaFieldIterator;
begin
  Result := True;

  Iterator := TCriteriaFieldIterator.Create(Self);
  while not Iterator.EOF do
  begin
    Result := Iterator.CriteriaField.ParamCriteriaIsSubCriteria(CmpCriteria);

    if Result = False then
      Break;
    Iterator.Next;
  end;
  Iterator.Free;
end;

procedure TCriteria.UnionAdd(Field : TDataField; Cond : TNiceCondition);
begin
  Assert((Field <> nil) and (Cond <> nil), 'TCriteria.UnionAdd: (Field <> nil) and (Cond <> nil)');

  CriteriaField[Field].UnionAddFrom(Cond);
end;

procedure TCriteria.ProcessFields(Proc : TProcFieldCond);
var
  Iterator : TCriteriaFieldIterator;
begin
  Iterator := TCriteriaFieldIterator.Create(Self);
  while not Iterator.EOF do
  begin
    Iterator.CriteriaField.ProcessFields(Proc);
    Iterator.Next;
  end;
  Iterator.Free;
end;

procedure TCriteria.ProcFields(Proc : TProcCriteriaFields);
begin
  FFields.ProcDataContents(TProcData(Proc));
end;

function TCriteria.GetCommonQuilt(out FreeQuilt: Boolean): TCommonQuilt;
begin
  FreeQuilt := True;
  Result := CreateQuiltpatch;
end;

function TCriteria.CreateQuiltPatch: TQuiltPatch;
var
  AQuiltField : TQuiltField;
  Iterator : TCriteriaFieldIterator;
begin
  Result := TQuiltPatch.Create;
  Iterator := TCriteriaFieldIterator.Create(Self);
  while not Iterator.EOF do
  begin
    AQuiltField := Iterator.CriteriaField.CreateQuiltField;
    Result[Iterator.DataField] := AQuiltField;
    AQuiltField.Free;
    Iterator.Next;
  end;
  Iterator.Free;
end;

{ TCriteriaField }

function TCriteriaField.InternalHasRowsThatMatch(Handler : TGetRowsHandler; SubTotal : TSubTotalRow) : Boolean; // (SubTotal : TSubTotalRow; ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean) : Boolean;
var
  KeyField : TDataField;
  i : Integer;
begin
  if Handler.AllowExcludeOnSubTotalLevel then
  begin
    KeyField := SubTotal.SubTotalKey.TreeKey;

    if (not (Handler.LastLevelNeedCheckAllFields and SubTotal.IsLastTreeNode)) and
       (Self.DataField = KeyField) and
       Self.HasExactlyOneValue then
    begin
      if SubTotal.IsLastTreeNode then
        Result := TSubTotalRowLink(SubTotal).FindValue(Self.OnlyValue, i)
      else
        Result := TSubTotalRowLink(SubTotal).FindValue(Self.OnlyValue, i) and
                  TSubTotalRowLink(SubTotal.SubRows[i]).HasRowsThatMatch(Handler); // (Self, ExcludeOnSubTotalLevel, LastLevelCheckAllFields);
    end
    else
      Result := inherited InternalHasRowsThatMatch(Handler, SubTotal); // (SubTotal, ExcludeOnSubTotalLevel, LastLevelCheckAllFields);
  end
  else
    Result := inherited InternalHasRowsThatMatch(Handler, SubTotal); // (SubTotal, ExcludeOnSubTotalLevel, LastLevelCheckAllFields);
end;

constructor TCriteriaField.DoCreateFromStream(AStream : TStream);
var
  AFieldName, ACaption : String;
begin
  if GetStreamTextTo(AStream, ':', #0, AFieldName) and
     GetStreamTextTo(AStream, ')', #0, ACaption) then
  begin
    Create(TDataField.FieldByName(AFieldName));
    Self.Caption := Trim(ACaption);
  end
  else
  begin
    raise Exception.Create('Invalid stream format!');
  end;
end;

procedure TCriteriaField.DoWriteToStream(AStream : TStream; Indent : Integer);
var
  Data : String;
begin
  WriteName(AStream);
  Data := DataField.FieldName + ': ' + Caption;
  AStream.Write(Data[1], Length(Data));
  WriteCloseParenteses(AStream);
end;

function TCriteriaField.ConcernsField(DataField : TDataField) : Boolean;
begin
  Result := (DataField = Self.DataField) and
            (not Self.HasNoConditions);
end;

function TCriteriaField.ConcernsTable(DataTable : TDataTable) : Boolean;
begin
  Result := DataTable.TableHasField(Self.DataField) and
            (not Self.HasNoConditions);
end;

function TCriteriaField.AcceptsValue(AField : TDataField; AValue : TValue) : Boolean;
begin
{$ifdef D4_OR_HIGHER}
  Result := (AField <> Self.DataField) or Self.Accepts(AValue);
{$else}
  Result := (AField <> Self.DataField) or Self.AcceptsOL(AValue);
{$endif D4_OR_HIGHER}
end;

function TCriteriaField.GetLargerCriteria(
  var FreeCriteria: Boolean): TCondition {TCriteria};
begin
  Result := TCriteria.Create;
  TCriteria(Result)[DataField] := Self;
  FreeCriteria := True;
end;

function TCriteriaField.GetSmallerCriteria(
  var FreeCriteria: Boolean): TCondition {TCriteria};
begin
  Result := TCriteria.Create;
  TCriteria(Result)[DataField] := Self;
  FreeCriteria := True;
end;

(*
function TCriteriaField.GetSelfIndex : Integer;
begin
  if FieldOwner = nil then
    Result := -1
  else
    Result := FieldOwner.FFields.IndexOf(Self);
end;
*)

function TCriteriaField.GetDataField : TDataField;
begin
  Result := FValues.Field;
end;

function TCriteriaField.GetCaption : String;
var
  i : Integer;
begin
  if Self.AcceptsAnyValue then
  begin
    if Self.HasNoConditions then
      Result := ''
    else
      Result := 'All';

    Exit;
  end
  else if Self.HasNoLegalValue then
  begin
    Result := '';
    Exit;
  end;

  Result := '';
  for i := 0 to ConditionCount - 1 do
  begin
    if not DataField.Intervals then
      Result := Result + ', ' + QuoteIfContains(AsString(FValues.Values1[i]), [':', ',', ' ', 'All'])
    else if Self.IsInterval[i] then
      Result := Result + ', ' + QuoteIfContains(AsString(FValues.Values1[i]), [':', ',', '-', ' ', 'All']) +
                         '-' + QuoteIfContains(AsString(FValues.Values2[i]), [':', ',', '-', ' ', 'All'])
    else
      Result := Result + ', ' + QuoteIfContains(AsString(FValues.Values1[i]), [':', ',', '-', ' ', 'All']);
  end;

  Result := System.Copy(Result, 3, length(Result));
end;

procedure TCriteriaField.SetCaption(ACaption : String);

begin
  BeginUpdate;


end;

function TCriteriaField.GetConditionCount : Integer;
begin
  Result := FValues.Count;
end;

function TCriteriaField.GetIsInterval(idx : Integer) : Boolean;
begin
  Assert((idx >= 0) and (idx < FValues.Count),
         'TCriteriaField.GetIsInterval: (idx >= 0) and (idx < FValues.Count), idx: ' + IntToStr(idx));

  Result := (DataField.DataType.Compare(FValues.Values1[idx], FValues.Values2[idx]) <> 0);
end;

function TCriteriaField.GetSingleValue(idx : Integer) : TValue;
begin
  Assert((idx >= 0) and (idx < FValues.Count),
         'TCriteriaField.GetSingleValue: (idx >= 0) and (idx < FValues.Count), idx: ' + IntToStr(idx));

  if IsInterval[idx] then
    Log(ltError, 'Criteria', 'TCriteriaField.GetSingleValue: Tried to get single value from interval');

  Result := FValues.Values1[idx];
end;

function TCriteriaField.GetOnlyValue : TValue;
begin
  if Self.HasExactlyOneValue then
    Result := Value[0]
  else
  begin
    Log(ltError, 'OnlyValue', 'TCriteriaField.OnlyValue called when HasExactlyOneValue is False!');
//    Result := '';
  end;
end;

function TCriteriaField.GetStartValue(idx : Integer) : TValue;
begin
  Assert((idx >= 0) and (idx < FValues.Count),
         'TCriteriaField.GetStartValue: (idx >= 0) and (idx < FValues.Count), idx: ' + IntToStr(idx));

  Result := FValues.Values1[idx];
end;

function TCriteriaField.GetEndValue(idx : Integer) : TValue;
begin
  Assert((idx >= 0) and (idx < FValues.Count),
         'TCriteriaField.GetEndValue: (idx >= 0) and (idx < FValues.Count), idx: ' + IntToStr(idx));

  Result := FValues.Values2[idx];
end;

procedure TCriteria.DoMap(Field : TDataField; var Dest : TCriteriaField);
begin
  Dest := FindCriteriaField(Field, False);
end;

procedure TCriteriaField.DoMap(Field : TDataField; var Dest : TCriteriaField);
begin
  if Self.DataField = Field then
    Dest := Self
  else
    Log(ltError, 'Criteriaoperation', 'Tried to perform a Criteriaoperation on CriteteriaFields of different DataFields');
end;

class procedure TCriteriaField.Map(Field : TDataField; Src : TNiceCondition; var Dest : TCriteriaField);
begin
  Src.DoMap(Field, Dest);
end;

constructor TCriteriaField.Create(Field : TDataField);
begin
  Assert(Field <> nil, 'TCriteriaField.Create: Field <> nil');

  CreateOwned(nil, Field);
end;

constructor TCriteriaField.CreateOwned(Owner : TCondition; Field : TDataField);
begin
  inherited Create(Owner);
  FValues := TCriteriaValueList.Create(Field);
  Reset;
end;

constructor TCriteriaField.CreateIntersection(Field : TDataField; Src1, Src2 : TNiceCondition);
begin
  Assert(Field <> nil,
         'TCriteriaField.CreateIntersection: Field <> nil');
  Assert(Src1 <> nil,
         'TCriteriaField.CreateIntersection: Src1 <> nil');
  Assert(Src2 <> nil,
         'TCriteriaField.CreateIntersection: Src2 <> nil');

  CreateOwnedIntersection(nil, Field, Src1, Src2);
end;

constructor TCriteriaField.CreateOwnedIntersection(CopyOwner : TCondition; Field : TDataField; Src1, Src2 : TNiceCondition);
var
  i1, i2 : Integer;
  CritField1, CritField2 : TCriteriaField;
  CompRes : Integer;

  function Intersects(var ResLow, ResHigh : TValue) : Boolean;
  begin
    if CompRes <= 0 then
    begin
      Result := Field.DataType.Compare(CritField1.EndValue[i1], CritField2.StartValue[i2]) >= 0;
      if Result then
      begin
        ResLow := CritField2.StartValue[i2];
        ResHigh := Field.DataType.Min(CritField1.EndValue[i1], CritField2.EndValue[i2]);
      end;
    end
    else
    begin
      Result := Field.DataType.Compare(CritField2.EndValue[i2], CritField1.StartValue[i1]) >= 0;
      if Result then
      begin
        ResLow := CritField1.StartValue[i1];
        ResHigh := Field.DataType.Min(CritField1.EndValue[i1], CritField2.EndValue[i2]);
      end;
    end;
  end;

var
  Low, High : TValue;
begin
  Map(Field, Src1, CritField1);
  Map(Field, Src2, CritField2);

  if (CritField1 <> nil) and (not CritField1.AcceptsAnyValue) then
  begin
    if (CritField2 <> nil) and (not CritField2.AcceptsAnyValue) then
    begin
      CreateOwned(CopyOwner, Field);

      if CritField1.HasNoLegalValue or CritField2.HasNoLegalValue then
      begin
        Self.FAcceptNone := True;
      end
      else
      begin
        i1 := 0;
        i2 := 0;

        while (i1 < CritField1.ConditionCount) and (i2 < CritField2.ConditionCount) do
        begin
          CompRes := Field.DataType.Compare(CritField1.StartValue[i1], CritField2.StartValue[i2]);
          if CompRes = 0 then
            CompRes := Field.DataType.Compare(CritField1.EndValue[i1], CritField2.EndValue[i2]);

          if Intersects(Low, High) then
            FValues.AddInterval(Low, High);

          CompRes := Field.DataType.Compare(CritField1.EndValue[i1], CritField2.EndValue[i2]);
          if CompRes <= 0 then
            Inc(i1);
          if CompRes >= 0 then
            Inc(i2);
        end;

        if Self.ConditionCount = 0 then
          FAcceptNone := True;
      end;
    end
    else
    begin
      CreateOwnedCopy(CopyOwner, CritField1);
    end;
  end
  else if (CritField2 <> nil) and (not CritField2.AcceptsAnyValue) then
  begin
    CreateOwnedCopy(CopyOwner, CritField2);
  end
  else
  begin
    CreateOwned(CopyOwner, Field);
    if (CritField1 <> nil) and CritField1.AcceptsAnyValue and
       (CritField2 <> nil) and CritField2.AcceptsAnyValue then
      FAcceptAll := True;
  end;
end;

constructor TCriteriaField.CreateUnion(Field : TDataField; Src1, Src2 : TNiceCondition);
begin
  Assert(Field <> nil, 'TCriteriaField.CreateIntersection: Field <> nil');
  Assert(Src1 <> nil, 'TCriteriaField.CreateIntersection: Src1 <> nil');
  Assert(Src2 <> nil, 'TCriteriaField.CreateIntersection: Src2 <> nil');


  CreateOwnedUnion(nil, Field, Src1, Src2);
end;

constructor TCriteriaField.CreateOwnedUnion(CopyOwner : TCondition; Field : TDataField; Src1, Src2 : TNiceCondition);
var
  i : Integer;
  CritField1, CritField2 : TCriteriaField;
begin
  Map(Field, Src1, CritField1);
  Map(Field, Src2, CritField2);

  if (CritField1 = nil) or (CritField2 = nil) or
     CritField1.AcceptsAnyValue or CritField2.AcceptsAnyValue then
  begin
    CreateOwned(CopyOwner, Field);
    Self.FAcceptAll := True;
  end
  else if CritField1.HasNoLegalValue then
  begin
    CreateOwnedCopy(CopyOwner, CritField2);
  end
  else if CritField2.HasNoLegalValue then
  begin
    CreateOwnedCopy(CopyOwner, CritField1);
  end
  else
  begin
    CreateOwned(CopyOwner, Field);

    for i := 0 to CritField1.ConditionCount - 1 do
      Self.AddInterval(CritField1.StartValue[i], CritField1.EndValue[i]);

    for i := 0 to CritField2.ConditionCount - 1 do
      Self.AddInterval(CritField2.StartValue[i], CritField2.EndValue[i]);
  end;
end;

constructor TCriteriaField.CreateCopyOfField(CriteriaField : TCriteriaField);
begin
  Assert(CriteriaField <> nil, 'TCriteriaField.CreateCopy: CriteriaField <> nil');

  CreateOwnedCopy(nil, CriteriaField);
end;

constructor TCriteriaField.CreateOwnedCopy(CopyOwner : TCondition; CriteriaField : TCriteriaField);
var
  i : Integer;
begin
  CreateOwned(CopyOwner, CriteriaField.DataField);

  if CriteriaField.FAcceptAll then
    Self.FAcceptAll := True
  else if CriteriaField.FAcceptNone then
    Self.FAcceptNone := True
  else
    for i := 0 to CriteriaField.ConditionCount - 1 do
      FValues.AddInterval(CriteriaField.StartValue[i], CriteriaField.EndValue[i]);
end;

constructor TCriteriaField.CreateAppliedFromSingleField(DataField : TDataField; Source : TCriteriaField; SuperKeys : TCriteria);
var
  KeyField : TDataField;
  KillList : TList;
  RowList : TDataRowList;
  GetCriteria : TCriteria;
  i : Integer;
  CritField : TCriteriaField;
begin
  if DataField = Source.DataField then
    CreateCopyOfField(Source)
  else
  begin
    CreateOwned(nil, DataField);

    if DataField is TKeyField then // Fixa LGE
    begin
      KeyField := DataField;

      KillList := TList.Create;
      while Source.DataField.LookupField <> KeyField do
      begin
        if Source.DataField.LookupField = nil then
          Break;

        Source := TCriteriaField.CreateAppliedFromSingleField(Source.DataField.LookupField, Source, SuperKeys);
        KillList.Add(Source);
      end;

      if (Source <> nil) and (Source.DataField.LookupTable <> nil) then
      begin
        RowList := TDataRowList.Create;

        GetCriteria := TCriteria.Create;
        GetCriteria[Source.DataField].CopyFrom(Source);
        for i := 0 to Source.DataField.LookupTable.KeyCount - 1 do
          if (Source.DataField.LookupTable.Field[i] <> Source.DataField) and
             (SuperKeys <> nil) then
          begin
            CritField := SuperKeys.FindCriteriaField(Source.DataField.LookupTable.Field[i], False);
            if CritField <> nil then
              GetCriteria[Source.DataField.LookupTable.Field[i]].UnionAddFrom(CritField);
          end;

        Source.DataField.LookupTable.Cache.GetRows(RowList, GetCriteria, gaReference);
        GetCriteria.Free;

        Self.AcceptNone;
        for i := 0 to RowList.Count - 1 do
          Self.AddValue(RowList.DataRows[i].Value[KeyField]);
        RowList.Free;
      end;

      for i := 0 to KillList.Count - 1 do
        TObject(KillList.Items[i]).Free;
      KillList.Free;
    end;
  end;
end;

constructor TCriteriaField.CreateApplied(DataField : TDataField; Source : TNiceCondition; AndConditions : Boolean);
var
  i : Integer;
  List : TDataRowList;
  NeedGetRows : Boolean;
  ACritField : TCriteriaField;
  Iterator : TCriteriaFieldIterator;
begin
  CreateOwned(nil, DataField);

  if Source is TCriteriaField then
    Apply(AndConditions, nil, TCriteriaField(Source))
  else if not (DataField is TKeyField) then // Fixa LGE
  begin
    ACritField := TCriteria(Source).FindCriteriaField(DataField, False);
    if ACritField <> nil then
      Self.CopyFrom(ACritField);
  end
  else if AndConditions then
  begin
    NeedGetRows := False;
    for i := 0 to DataField.DependentFieldCount - 1 do
      if TCriteria(Source).HasRestrictionsOnField(DataField.DependentField[i]) then
      begin
        NeedGetRows := True;
        Break;
      end;

    if NeedGetRows then
    begin
      Self.AcceptNone;
      List := TDataRowList.Create;
      DataField.GetRows(List, TCriteria(Source));
      for i := 0 to List.Count - 1 do
        AddValue(List.DataRows[i].Value[DataField]);
      List.Free;
    end
    else if TCriteria(Source).HasConditionOnField(DataField) then
      Self.CopyFrom(TCriteria(Source).CriteriaField[DataField]);
  end
  else
  begin
    Iterator := TCriteriaFieldIterator.Create(TCriteria(Source));
    while not Iterator.EOF do
    begin
      Apply(AndConditions, TCriteria(Source), Iterator.CriteriaField);
      Iterator.Next;
    end;
    Iterator.Free;
  end;
end;

function TCriteriaField.GetFieldOwner : TCriteria;
begin
  if FOwner is TCriteria then
    Result := TCriteria(FOwner)
  else
    Result := nil;
end;

procedure TCriteriaField.InternalGetRows(Handler : TGetRowsHandler; SubTotal : TSubTotalRow); // (SubTotal : TSubTotalRow; Results : TStrings; Action : TGetAction; DefaultSort, ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean);
var
  KeyField : TDataField;
begin
  if Handler.AllowExcludeOnSubTotalLevel then
  begin
    KeyField := SubTotal.SubTotalKey.TreeKey;

    if (not (Handler.LastLevelNeedCheckAllFields and SubTotal.IsLastTreeNode)) and
       (Self.DataField = KeyField) and
       Self.HasExactlyOneValue then
      TSubTotalRowLink(SubTotal).__AddValue(Handler, Self.OnlyValue) // ( Self.OnlyValue, Results, Self, Action, ExcludeOnSubTotalLevel, LastLevelCheckAllFields, DefaultSort)
    else
      inherited InternalGetRows(Handler, SubTotal); // (SubTotal, Results, Action, DefaultSort, ExcludeOnSubTotalLevel, LastLevelCheckAllFields);
  end
  else
    inherited InternalGetRows(Handler, SubTotal); // (SubTotal, Results, Action, DefaultSort, ExcludeOnSubTotalLevel, LastLevelCheckAllFields);
end;

function TCriteriaField.Equals(Condition : TCondition) : Boolean;
var
  i : Integer;
begin
  if not (Condition is TCriteriaField) then
    Result := False
  else if (Self.DataField <> TCriteriaField(Condition).DataField) or
     (Self.ConditionCount <> TCriteriaField(Condition).ConditionCount) or
     (Self.FAcceptAll <> TCriteriaField(Condition).FAcceptAll) or
     (Self.FAcceptNone <> TCriteriaField(Condition).FAcceptNone) then
    Result := False
  else
  begin
    Result := True;
    for i := 0 to ConditionCount - 1 do
      if (DataField.DataType.Compare(Self.StartValue[i], TCriteriaField(Condition).StartValue[i]) <> 0) or
         (DataField.DataType.Compare(Self.EndValue[i], TCriteriaField(Condition).EndValue[i]) <> 0) then
      begin
        Result := False;
        Exit;
      end;
  end;
end;

procedure TCriteriaField.Apply(AndConditions : Boolean; SuperKeys : TCriteria; CriteriaField : TCriteriaField);
var
  DerivedDataField : TDataField;
  CalcedCriteriaField : TCriteriaField;
begin
  BeginUpdate;

  CalcedCriteriaField := nil;
  try
    DerivedDataField := DataField.FindDerivedSource(CriteriaField.DataField);
    if DataField <> DerivedDataField then
      Exit;

    CalcedCriteriaField := TCriteriaField.CreateAppliedFromSingleField(DataField, CriteriaField, SuperKeys);
    if Self.HasNoConditions then
      Self.CopyFrom(CalcedCriteriaField)
    else if AndConditions then
      Self.IntersectWith(CalcedCriteriaField)
    else
      Self.UnionAddFrom(CalcedCriteriaField);

  finally
    CalcedCriteriaField.Free;
    EndUpdate;
    Changed;
  end;
end;

destructor TCriteriaField.Destroy;
begin
  if FieldOwner <> nil then
    FieldOwner.RemoveChild(Self);

  FreeOwnedMemory;
  inherited Destroy;
end;

procedure TCriteriaField.FreeOwnedMemory;
begin
  FValues.Free;
end;

procedure TCriteriaField.Reset;
begin
  BeginUpdate;

  FValues.Clear;
  FAcceptAll := False;
  FAcceptNone := False;

  EndUpdate;
  Changed;
end;

function TCriteriaField.NrOfIntervals : Integer;
var
  i : Integer;
begin
  Result := 0;

  if DataField.Intervals then
    for i := 0 to ConditionCount -1 do
      if IsInterval[i] then
        Inc(Result);
end;

function TCriteriaField.FieldSQL : String;
begin
  Result := FieldSQLWithFieldName(DataField.FieldName);
end;

function TCriteriaField.FieldSQLWithFieldName(Name : String) : String;
var
  c : Integer;

  function Cond : String;
  begin
    if c = 0 then
      Result := ''
    else
      Result := ' or ';
    Inc(c);
  end;

var
  i, intervals : Integer;
  sCounter : Integer;
begin
  if Self.HasNoLegalValue then
  begin
    Result := '(1 = 0)';
    Exit;
  end
  else if Self.AcceptsAnyValue then
  begin
    Result := '';
    Exit;
  end;

  Result := '(';
  intervals := Self.NrOfIntervals;
  c := 0;

  if ConditionCount - intervals = 1 then
  begin
    for i := 0 to ConditionCount - 1 do
      if not IsInterval[i] then
      begin
        Result := Result + Cond + Name + ' = ' + DataField.DataType.AsSQL(Value[i]);
        break;
      end;
  end
  else if ConditionCount - intervals > 1 then
  begin
    Result := Result + Cond + Name + ' in (';
    sCounter := 0;
    for i := 0 to ConditionCount - 1 do
      if not IsInterval[i] then
      begin
        if sCounter > 0 then
          Result := Result + ', ';
        Result := Result + DataField.DataType.AsSQL(Value[i]);
        Inc(sCounter);
      end;
    Result := Result + ')';
  end;

  if intervals > 0 then
    for i := 0 to ConditionCount - 1 do
      if IsInterval[i] then
      begin
        Result := Result + Cond + Name + ' between ' +
                  DataField.DataType.AsSQL(StartValue[i]) +
                  ' and ' + DataField.DataType.AsSQL(EndValue[i]);
      end;

  Result := Result + ')';
end;







procedure TCriteriaField.IntersectWith(Cond : TNiceCondition);
var
  ACriteriaField : TCriteriaField;
begin
  Assert(Cond <> nil, 'TCriteriaField.IntersectWith: Cond <> nil');

  BeginUpdate;
  try
    ACriteriaField := TCriteriaField.CreateIntersection(DataField, Self, Cond);
    Self.CopyFrom(ACriteriaField);
    ACriteriaField.Free;
  finally
    EndUpdate;
    Changed;
  end;
end;

procedure TCriteriaField.CopyFrom(Source : TCriteriaField);
var
  MyDataField : TDataField;
  MyChange : TNotifyEvent;
begin
  Assert(Source <> nil, 'TCriteriaField.CopyFrom: Source <> nil');

  if (Source.ConditionCount > 0) and (Self.DataField.DataType <> Source.DataField.DataType) then
  begin
    Log(ltError, 'CopyFrom', 'TCriteriaField.CopyFrom: Different DataField types!');
    Exit;
  end;

  try
    MyDataField := Self.DataField;
    MyChange := Self.OnChange;
    Self.OnChange := nil;
    Self.FreeOwnedMemory;
    CreateOwnedCopy(FOwner, Source);
    Self.FValues.Field := MyDataField;
    Self.OnChange := MyChange;
  finally
    Changed;
  end;
end;

procedure TCriteriaField.UnionAddFrom(Cond : TNiceCondition);
var
  CritField : TCriteriaField;
  i : Integer;
begin
  Assert(Cond <> nil, 'TCriteriaField.UnionAddFrom: Cond <> nil');

  BeginUpdate;
  try
    CritField := nil;

    if (Cond is TCriteriaField) and
       (TCriteriaField(Cond).DataField <> Self.DataField) and
       (TCriteriaField(Cond).DataField.DataType = Self.DataField.DataType) then
      CritField := TCriteriaField(Cond)
    else
      Map(DataField, Cond, CritField);

    if (CritField = nil) or
       CritField.AcceptsAnyValue then
    begin
      Self.AcceptAll;
    end
    else if not CritField.HasNoLegalValue then
    begin
      for i := 0 to CritField.ConditionCount - 1 do
        Self.AddInterval(CritField.StartValue[i], CritField.EndValue[i]);
    end;
  finally
    EndUpdate;
    Changed;
  end;
end;

function TCriteriaField.AcceptsExactlyOneValue(DataField : TDataField; var Value : TValue) : Boolean;
begin
  Result := (DataField = Self.DataField) and HasExactlyOneValue;
  if Result then
    Value := OnlyValue;
end;

function TCriteriaField.ParamCriteriaIsSubCriteria(Cond : TNiceCondition) : Boolean;
var
  i : Integer;
  CritField : TCriteriaField;
begin
  //Assert(Cond <> nil, 'TCriteriaField.ParamCriteriaIsSubCriteria: Cond <> nil');

  if Self.AcceptsAnyValue then
  begin
    Result := True;
    Exit;
  end;

  Map(DataField, Cond, CritField);

  if (CritField <> nil) and
     (CritField.HasNoLegalValue) then
  begin
    Result := True;
    Exit;
  end;

  if Self.HasNoLegalValue or (CritField = nil) or
     CritField.AcceptsAnyValue then
  begin
    Result := False;
    Exit;
  end;

  for i := 0 to CritField.ConditionCount - 1 do
    if not AcceptsInterval(CritField.StartValue[i], CritField.EndValue[i]) then
    begin
      Result := false;
      Exit;
    end;

  Result := True;
end;

function TCriteriaField.HasNoLegalValue : Boolean;
begin
  Result := FAcceptNone;
end;

function TCriteriaField.HasExactlyOneValue : Boolean;
begin
  Result := (not FAcceptNone) and
            (not FAcceptAll) and
            (ConditionCount = 1) and
            (not IsInterval[0]);
end;

function TCriteriaField.HasNoConditions : Boolean;
begin
  // Note: *not* FAcceptAll! FAcceptAll _is_ a condition in this context!
  Result := (FValues.Count = 0) and (not FAcceptNone) and (not FAcceptAll);
end;

function TCriteriaField.AcceptsAnyValue : Boolean;
begin
  Result := FAcceptAll or
            ((FValues.Count = 0) and (not FAcceptNone));
end;

{$ifdef D4_OR_HIGHER}
function TCriteriaField.Accepts(Value : TValue) : Boolean;
{$else}
function TCriteriaField.AcceptsOL(Value : TValue) : Boolean;
{$endif D4_OR_HIGHER}
begin
  Result := Self.AcceptsAnyValue or
            ((not FAcceptNone) and FValues.AcceptsValue(Value));
end;

{$ifdef D4_OR_HIGHER}
function TCriteriaField.AcceptsRow(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean;
{$else}
function TCriteriaField.AcceptsRow(ARow : TAbstractRow; AffectedFields : TDataFieldSet) : Boolean;
{$endif D4_OR_HIGHER}
begin
  if (ARow is TSubTotalRow) and (not TSubTotalRow(ARow).FieldHasValue(DataField)) then
    Result := True
  else
    Result := Self.AcceptsAnyValue or
              ((not FAcceptNone) and
               FValues.AcceptsValue(ARow[DataField]));
end;

{$ifdef D4_OR_HIGHER}
function TCriteriaField.AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean;
{$else}
function TCriteriaField.AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet) : Boolean;
{$endif D4_OR_HIGHER}
begin
  Result := True;
  if (AffectedFields <> nil) or AffectedFields.ContainsField(DataField) then
    try
      Result := AcceptsValue(DataField, ARow[DataField]);
    except
      Result := True;
    end;
end;

function TCriteriaField.AcceptsInterval(Low, High : TValue) : Boolean;
begin
  Result := Self.AcceptsAnyValue or
            ((not FAcceptNone) and FValues.AcceptsInterval(Low, High));
end;

procedure TCriteriaField.AcceptAll;
begin
  BeginUpdate;

  Reset;
  FAcceptAll := True;

  EndUpdate;
  Changed;
end;

procedure TCriteriaField.AcceptNone;
begin
  BeginUpdate;

  Reset;
  FAcceptNone := True;

  EndUpdate;
  Changed;
end;

function TCriteriaField.CreateCopy : TCondition;
begin
  Result := TCriteriaField.CreateOwnedCopy(nil, Self);
end;

function TCriteriaField.CreateCopyWithField(NewField : TDataField) : TCriteriaField;
var
  i : Integer;
begin
  // Fixa LGE jårsa ifall dest inte tillåter intervall men source gör det...

  if NewField = nil then
    Result := nil
  else
  begin
    Result := TCriteriaField.CreateOwned(nil, NewField);

    if Self.FAcceptAll then
      Result.FAcceptAll := True
    else if Self.FAcceptNone then
      Result.FAcceptNone := True
    else
      for i := 0 to Self.ConditionCount - 1 do
        Result.FValues.AddInterval(Self.StartValue[i], Self.EndValue[i]);
  end;
end;

function TCriteriaField.CreateFieldTranslatedCopy(TranslateField : TQueryDataFieldFunction) : TCondition;
var
  NewField : TDataField;
  ConflictAction : TConflictAction;
  KeepOld : Boolean;
begin
  NewField := TranslateField(Self.DataField, ConflictAction, KeepOld);
  Result := CreateCopyWithField(NewField);
end;

function TCriteriaField.AcceptsAllInTable(DataTable : TDataTAble) : Boolean;
begin
  Result := (not DataTable.TableHasField(DataField)) or Self.AcceptsAnyValue;
end;

function TCriteriaField.AcceptsNoValuesForTable(DataTable : TDataTable) : Boolean;
begin
  Result := DataTable.TableHasField(DataField) and Self.HasNoLegalValue;
end;

function TCriteriaField.AcceptsAllForField(DataField : TDataField) : Boolean;
begin
  Result := (Self.DataField <> DataField) or Self.AcceptsAnyValue;
end;

function TCriteriaField.AcceptsNoValuesForField(DataField : TDataField) : Boolean;
begin
  Result := (Self.DataField = DataField) and Self.HasNoLegalValue;
end;

procedure TCriteriaField.AddString(Value : String);
begin
  AddValue(ValueFromString(Value));
end;

procedure TCriteriaField.AddValue(Value : TValue);
var
  matchpos : Integer;
  match : boolean;
begin
  if FAcceptAll then
    Exit;

  Value := DataField.DataType.Optimize(Value);

  BeginUpdate;

  FAcceptNone := False;

  match := FValues.Find(Value, matchpos);
  if (not match) and
     ((matchpos = 0) or
      (DataField.DataType.Compare(Value, EndValue[matchpos-1]) > 0)) then
    FValues.Add(Value);

  EndUpdate;
  Changed;
end;

procedure TCriteriaField.AddStringInterval(Low, High : String);
begin
  AddInterval(ValueFromString(Low), ValueFromString(High));
end;

procedure TCriteriaField.AddInterval(Low, High : TValue);

  function JoinIntervals(StartIndex : Integer) : Boolean;
  var
    idx : Integer;
    CurrentEnd : TValue;
  begin
    Result := False;

    if (StartIndex < 0) or (StartIndex >= FValues.Count) then
      Exit;

    CurrentEnd := EndValue[StartIndex];
    idx := StartIndex + 1;

    while (idx < FValues.Count) and
          (DataField.DataType.Compare(CurrentEnd, StartValue[idx]) >= 0) do
    begin
      CurrentEnd := DataField.DataType.Max(CurrentEnd, EndValue[idx]);
      Inc(idx);
    end;

    if idx - StartIndex > 1 then
    begin
      Dec(idx);
      while idx > StartIndex do
      begin
        FValues.Delete(idx);
        Dec(idx);
      end;
      FValues.Values2[StartIndex] := CurrentEnd;
      Result := True;
    end;
  end;

var
  matchpos : Integer;
  match : boolean;
begin
  if FAcceptAll then
    Exit;

  Low := DataField.DataType.Optimize(Low);
  High := DataField.DataType.Optimize(High);

  if (not DataField.Intervals) and
     (not DataField.DataType.Equals(Low, High)) then
    Log(ltError, 'Intervals', 'Intervals not supported!');

    // Fixa LGE; vi sku också kunna göra något åt low > hign (skippa)

  BeginUpdate;
  try
    FAcceptNone := False;
    match := FValues.Find(Low, matchpos);
    if Match then
    begin
      if DataField.DataType.Compare(High, EndValue[matchpos]) > 0 then
      begin
        FValues.Values2[matchpos] := High;
        JoinIntervals(matchpos);
      end;
    end
    else
    begin
      matchpos := FValues.AddInterval(Low, High);
      if not JoinIntervals(matchpos - 1) then
        JoinIntervals(matchpos);
    end;
  finally
    EndUpdate;
    Changed;
  end;
end;

procedure TCriteriaField.AddSingleValues(Values : TStrings);
var
  iValues : Integer;
begin
  BeginUpdate;

  for iValues := 0 to Values.Count - 1 do
    AddValue(ValueFromString(Values[iValues]));

  EndUpdate;
  Changed;
end;

procedure TCriteriaField.GenerateIntervals(AllLegalValues : TStrings);
var
  i : Integer;
  FirstRow : TDataRow;
  LastRow : TDataRow;
  ThisRow : TDataRow;
  ThisRowWasSelected : Boolean;
begin
  if Self.AcceptsAnyValue or
     Self.HasNoLegalValue or
     (not Self.DataField.Intervals) then
    Exit;

  BeginUpdate;

  FirstRow := nil;
  LastRow := nil;

  for i := 0 to AllLegalValues.Count -1 do
  begin
    ThisRow := TDataRow(AllLegalValues.Objects[i]);
{$ifdef D4_OR_HIGHER}
    ThisRowWasSelected := Self.Accepts(ThisRow[DataField]);
{$else}
    ThisRowWasSelected := Self.AcceptsOL( ThisRow[DataField] );
{$endif D4_OR_HIGHER}

    if ThisRowWasSelected and (FirstRow = nil) then
      FirstRow := ThisRow
    else if (not ThisRowWasSelected) and (FirstRow <> nil) then
    begin
      if LastRow <> FirstRow then
        AddInterval(FirstRow[DataField], LastRow[DataField]);

      FirstRow := nil;
    end;

    LastRow := ThisRow;
  end;

  if (FirstRow <> nil) and
     (LastRow <> FirstRow) then
    AddInterval(FirstRow[DataField], LastRow[DataField]);

  EndUpdate;
  Changed;
end;



{/** Change criteria for one field to match .Selected rows in a TListBox object.
  * @param AListBox A list box containing TDataRows */}
procedure TCriteriaField.SyncFromListBox(AListBox : TListBox; ClearIfAllSelected : Boolean);
var
  i : Integer;
  FirstRow, ThisRow, LastRow : TDataRow;
  ThisRowWasSelected : Boolean;
begin
  Assert(AListBox <> nil, 'TCriteriaField.SyncFromListBox: AListBox <> nil');

  BeginUpdate;
  AListBox.Items.BeginUpdate;
  // Start by accepting no values at all
  if ClearIfAllSelected then
  begin
    Reset;

    if (AListBox.SelCount = AListBox.Items.Count) or
       (AListBox.SelCount = 0) then
    begin
      EndUpdate;
      AListBox.Items.EndUpdate;
      Changed;
      Exit; // if all or no values are selected, we need to do nothing, because that means any value legal
    end;
  end
  else
  begin
    AcceptNone;

    if (AListBox.SelCount = AListBox.Items.Count) then
    begin
      AcceptAll;
      AListBox.Items.EndUpdate;
      EndUpdate;
      Changed;
      Exit;
    end
    else if (AListBox.SelCount = 0) then
    begin
      AListBox.Items.EndUpdate;
      EndUpdate;
      Changed;
      Exit; // no values are selected, we need to do nothing, because we already defaulted to AcceptNone
    end;
  end;

  FirstRow := nil;
  LastRow := nil;

  for i := 0 to AListBox.Items.Count -1 do
  begin
    ThisRow := TDataRow(AListBox.Items.Objects[i]);
    ThisRowWasSelected := AListBox.Selected[i];

    if ThisRowWasSelected and (not DataField.Intervals) then
      AddValue(ThisRow[DataField])
    else if ThisRowWasSelected and (FirstRow = nil) then
      FirstRow := ThisRow
    else if (not ThisRowWasSelected) and (FirstRow <> nil) then
    begin
      if LastRow = FirstRow then
        AddValue(LastRow[DataField])
      else
        AddInterval(FirstRow[DataField], LastRow[DataField]);

      FirstRow := nil;
    end;

    LastRow := ThisRow;
  end;

  if FirstRow <> nil then
  begin
    if LastRow = FirstRow then
      AddValue(LastRow[DataField])
    else
      AddInterval(FirstRow[DataField], LastRow[DataField]);
  end;

  AListBox.Items.EndUpdate;
  EndUpdate;
  Changed;
end;

{/** Change .Selected rows in a TListBox object to match criteria for one field.
  * @param AListBox A list box containing TDataRows */}
procedure TCriteriaField.SyncToListBox(AListBox : TListBox; ClearIfAllSelected : Boolean);
var
  IsVisible : boolean;
  iRow : integer;
  ThisValue : TValue;
begin
  Assert(AlistBox <> nil, 'TCriteriaField.SyncToListBox: AlistBox <> nil');

  AListBox.Items.BeginUpdate;
  // Empty
  with AListBox do
  begin
    IsVisible := Visible;
    iRow := ItemIndex;
    Visible := False;
    MultiSelect := False;
    ItemIndex := -1;
    MultiSelect := True;
    Visible := IsVisible;
    ItemIndex := iRow;
  end;

  // If we have no restrictions we don't need to paint any rows
  if Self.AcceptsAnyValue then
  begin
    if (not ClearIfAllSelected) and
       (not HasNoConditions) then
      for iRow := 0 to AListBox.Items.Count - 1 do
        AListBox.Selected[iRow] := True;

    AListBox.Items.EndUpdate;
    Exit;
  end;

  for iRow := AListBox.Items.Count-1 downto 0 do
  begin
    ThisValue := TDataRow( AListBox.Items.Objects[iRow] ).Value[DataField];
{$ifdef D4_OR_HIGHER}
    if Self.Accepts(ThisValue) then
{$else}
    if Self.AcceptsOL( ThisValue ) then
{$endif D4_OR_HIGHER}
      AListBox.Selected[iRow] := True
  end;
  AListBox.Items.EndUpdate;
end;
{$ifndef LINUX}
{$endif LINUX}


procedure TCriteriaField.Print(List : TStringList);
var
  Line : String;
begin
  if Self.HasNoConditions then
    Exit;

  Line := DataField.FieldName + ':';
  if Self.AcceptsAnyValue then
    Line := Line + '+'
  else if Self.HasNoLegalValue then
    Line := Line + '-'
  else
    Line := Line + ' ' + Caption;

  List.Add(Line);
end;

procedure TCriteriaField.SaveToDB(RowStorage : TAbstractRowStorage; DefaultValues : TDataRow;
             FieldField, FromField : TDataField; ToField : TDataField; EmptyFirst : Boolean);

  procedure AddRow(StartVal, EndVal : TValue);
  var
    NewRow : TDataRow;
  begin
    NewRow := RowStorage.CreateNewRow(DefaultValues.GetFieldValue);
    NewRow.SetDefaultsFrom(DefaultValues);
    NewRow.StringValue[FieldField] := DataField.FieldName;
    NewRow.Value[FromField] := StartVal;
    NewRow.Value[ToField] := EndVal;
    RowStorage.PutRow(NewRow, paOverwriteOnKeyChange);
  end;

var
  i : Integer;
  List : TDataRowList;
  GetCriteria : TCriteria;
begin
  Assert(RowStorage <> nil, 'TCriteriaField.SaveToDB: RowStorage <> nil');
  Assert(DefaultValues <> nil, 'TCriteriaField.SaveToDB: DefaultValues <> nil');
  Assert(FieldField <> nil, 'TCriteriaField.SaveToDB: FieldField <> nil');
  Assert(FromField <> nil, 'TCriteriaField.SaveToDB: FromField <> nil');
  Assert(ToField <> nil, 'TCriteriaField.SaveToDB: ToField <> nil');

  if EmptyFirst then
  begin
    List := TDataRowList.Create;
    GetCriteria := TCriteria.CreateFromRowKeys(DefaultValues);
    GetCriteria[FromField].Reset;
    GetCriteria[ToField].Reset;
    RowStorage.GetRows(List, GetCriteria, gaReference);
    for i := 0 to List.Count - 1 do
      List.DataRows[i].Delete;
    List.Free;
    GetCriteria.Free;
  end;


  if Self.HasNoConditions then
    Exit
  else if Self.AcceptsAnyValue then
    AddRow(ValueFromInteger(1), ValueFromString(''))
  else if Self.HasNoLegalValue then
    AddRow(ValueFromInteger(0), ValueFromString(''))
  else
  for i := 0 to ConditionCount - 1 do
  begin
    AddRow(StartValue[i], EndValue[i])
  end;
end;

procedure TCriteriaField.LoadFromDB(RowStorage : TAbstractRowStorage; DefaultValues : TDataRow; FieldField, FromField : TDataField; ToField : TDataField);
var
  List : TDataRowList;
  i : Integer;
  GetCriteria : TCriteria;
  FieldName : String;
  Field : TDataField;
begin
  Assert(RowStorage <> nil, 'TCriteriaField.LoadFromDB: RowStorage <> nil');
  Assert(DefaultValues <> nil, 'TCriteriaField.LoadFromDB: DefaultValues <> nil');
  Assert(FieldField <> nil, 'TCriteriaField.LoadFromDB: FieldField <> nil');
  Assert(FromField <> nil, 'TCriteriaField.LoadFromDB: FromField <> nil');
  Assert(ToField <> nil, 'TCriteriaField.LoadFromDB: ToField <> nil');

  BeginUpdate;

  Self.Reset;

  List := TDataRowList.Create;
  GetCriteria := TCriteria.CreateFromRowKeys(DefaultValues);
  GetCriteria[FieldField].Reset;
  GetCriteria[FieldField].AddString(Self.DataField.FieldName);
  GetCriteria[FromField].Reset;
  GetCriteria[ToField].Reset;
  RowStorage.GetRows(List, GetCriteria, gaReference);
  //GetCriteria.Free;

  for i := 0 to List.Count - 1 do
  begin
    FieldName := List.DataRows[i].StringValue[FieldField];
    Field := TDataField.FieldByName(FieldName);
    if Field = nil then
      Log(ltError, 'LoadFromFile', 'TCriteriaField.LoadFromDB: Unknown Field ' + FieldName);

    Self.AddValuesFromRow(List.DataRows[i], FromField, ToField);
  end;
  List.Free;
  GetCriteria.Free;

  EndUpdate;
  Changed;
end;

procedure TCriteriaField.AddValuesFromRow(DataRow : TDataRow; FromField : TDataField; ToField : TDataField);
var
  FromVal, ToVal : TValue;
begin
  Assert(DataRow <> nil, 'TCriteriaField.AddValuesFromRow: DataRow <> nil');
  Assert(FromField <> nil, 'TCriteriaField.AddValuesFromRow: FromField <> nil');
  Assert(ToField <> nil, 'TCriteriaField.AddValuesFromRow: ToField <> nil');

  BeginUpdate;

  FromVal := DataRow[FromField];
  ToVal := DataRow[ToField];

  if (FromField.DataType.Equals(FromVal, ValueFromInteger(0))) and
     (ToField.DataType.Equals(ToVal, ValueFromString(''))) then
    Self.AcceptNone
  else if (FromField.DataType.Equals(FromVal, ValueFromInteger(1))) and
     (ToField.DataType.Equals(ToVal, ValueFromString(''))) then
    Self.AcceptAll
  else
    Self.AddInterval(FromVal, ToVal);

  EndUpdate;
  Changed;
end;

procedure TCriteriaField.ProcessFields(Proc : TProcFieldCond);
begin
  Proc(DataField, Self.AcceptsAnyValue);
end;
(*
function TCriteriaField.DoCopyValues( FromField, ToField : TDataField; CreateUnion, KeepOld : Boolean ) : TCondition;
var
  res : TCriteria;
begin
  Result := Self;
  if FromField = Self.DataField then
  begin
    if ToField <> nil then
    begin
      if KeepOld then
      begin
        res := TCriteria.Create;
        Res[FromField] := Self;
        Res[ToField] := Self;
        Result := res;
        Self.Free;
      end
      else
        Self.FValues.Field := ToField;
    end
    else if not KeepOld then
    begin
      Result := nil;
      Self.Free;
    end;
  end
  else if (FromField = nil) and
          (ToField = Self.DataField) then
  begin
    Result := nil;
    Self.Free;
  end
  else if Self.HasNoConditions then
  begin
    Result := nil;
    Self.Free;
  end;
end;
 *)
function TCriteriaField.GetCommonQuilt(
  out FreeQuilt: Boolean): TCommonQuilt;
begin
  Result := CreateQuiltField;
  FreeQuilt := True;
end;

function TCriteriaField.CreateQuiltField: TQuiltField;
var
  i : Integer;
begin
  Result := TQuiltField.Create( DataField );
  if not HasNoConditions then
  begin
    if AcceptsAnyValue then
      Result.AcceptAll
    else if HasNoLegalValue then
      Result.AcceptNone
    else
      for i := 0 to ConditionCount -1 do
{$ifdef D4_OR_HIGHER}
        Result.AddInterval( QuiltPoint(StartValue[i]), QuiltPoint(EndValue[i]) );
{$else}
        Result.AddInterval( QuiltPoint(StartValue[i], True), QuiltPoint(EndValue[i], True) );
{$endif D4_OR_HIGHER}
  end;
end;

{ TNiceCondition }

function TNiceCondition.IsPureAndCondition : Boolean;
begin
  Result := True;
end;

{ TCriteriaValueList }

constructor TCriteriaValueList.Create(Field : TDataField);
begin
  Assert(Field <> nil, 'TCriteriaValueList.Create: Field <> nil');

  inherited Create;
  Self.Field := Field;
end;

destructor TCriteriaValueList.Destroy;
begin
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

procedure TCriteriaValueList.Error(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

function TCriteriaValueList.Add(const S: TValue) : Integer;
begin
  if Find(S, Result) then
    case Duplicates of
      dupIgnore: Exit;
      dupError: Error({$ifdef USE_RESOURCESTRINGS}SDuplicateString{$else}'SDuplicateString'{$endif USE_RESOURCESTRINGS});
    end;

  InsertItem(Result, S);
end;

function TCriteriaValueList.AcceptsValue(Value : TValue) : Boolean;
var
  pos : Integer;
begin
  if Find(Value, pos) then
    Result := True
  else if pos > 0 then
    Result := Field.DataType.Compare(Value, Values2[pos - 1]) <= 0
  else
    Result := False;
end;

function TCriteriaValueList.AcceptsInterval(Low, High : TValue) : Boolean;
var
  pos : Integer;
begin
  if Find(Low, pos) then
    Result := Field.DataType.Compare(High, Values2[pos]) <= 0
  else if pos > 0 then
    Result := Field.DataType.Compare(High, Values2[pos - 1]) <= 0
  else
    Result := False;
end;

function TCriteriaValueList.AddInterval(const S1, S2 : TValue) : Integer;
begin
  Result := Add(S1);
  Put2(Result, S2);
end;

procedure TCriteriaValueList.Clear;
begin
  if FCount <> 0 then
  begin
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
  end;
end;

procedure TCriteriaValueList.Delete(Index: Integer);
begin
  Assert((Index >= 0) and (Index < FCount),
         'TCriteriaValueList.Delete: (Index >= 0) and (Index < FCount), Index: ' + IntToStr(Index));

  if (Index < 0) or (Index >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}SListIndexError{$else}'SListIndexError'{$endif LINUX});
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TCritValueItem));
end;

procedure TCriteriaValueList.Exchange(Index1, Index2: Integer);
begin
  Assert((Index1 >= 0) and (Index1 < FCount),
         'TCriteriaValueList.Exchange: (Index1 >= 0) and (Index1 < FCount), Index1: ' + IntToStr(Index1));
  Assert((Index2 >= 0) and (Index2 < FCount),
         'TCriteriaValueList.Exchange: (Index2 >= 0) and (Index2 < FCount), Index2: ' + IntToStr(Index2));

  if (Index1 < 0) or (Index1 >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS});
  if (Index2 < 0) or (Index2 >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS});
  ExchangeItems(Index1, Index2);
end;

procedure TCriteriaValueList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: TValue;
  Item1, Item2: PCritValueItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];

  Temp := Item1^.FValue1;
  Item1^.FValue1 := Item2^.FValue1;
  Item2^.FValue1 := Temp;

  Temp := Item1^.FValue2;
  Item1^.FValue2 := Item2^.FValue2;
  Item2^.FValue2 := Temp;
end;

function TCriteriaValueList.Find(const S: TValue; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Field.DataType.Compare(FList^[I].FValue1, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TCriteriaValueList.Get1(Index: Integer): TValue;
begin
  if (Index < 0) or (Index >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS});
  Result := FList^[Index].FValue1;
end;

function TCriteriaValueList.Get2(Index: Integer): TValue;
begin
  Assert((Index >= 0) and (Index < FCount),
         'TCriteriaValueList.Get2: (Index >= 0) and (Index < FCount), Index: ' + IntToStr(Index));

  if (Index < 0) or (Index >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS});
  Result := FList^[Index].FValue2;
end;

function TCriteriaValueList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TCriteriaValueList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS});
  Result := nil;
end;

procedure TCriteriaValueList.Grow;
var
  NewSize: Integer;
begin
  if FCapacity < 16 then
    NewSize := 16
  else
    NewSize := FCapacity * 2;

  SetCapacity(NewSize);
end;

function TCriteriaValueList.IndexOf(const S: TValue): Integer;
begin
  if not Find(S, Result) then
    Result := -1;
end;

procedure TCriteriaValueList.InsertItem(Index: Integer; const S: TValue);
begin
{$ifndef LINUX}
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TCritValueItem));

  FillMemory(@FList^[Index], SizeOf(TCritValueItem), Byte(0));

  with FList^[Index] do
  begin
(*    Pointer(FValue1) := nil;
    Pointer(FValue2) := nil; *)
    FValue1 := S;
    FValue2 := S;
  end;
  Inc(FCount);
{$endif LINUX}
end;

procedure TCriteriaValueList.Put2(Index: Integer; const S: TValue);
begin
  Assert((Index >= 0) and (Index < FCount),
         'TCriteriaValueList.Put2: (Index >= 0) and (Index < FCount), Index: ' + IntToStr(Index));

  if (Index < 0) or (Index >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS});
  FList^[Index].FValue2 := S;
end;

procedure TCriteriaValueList.QuickSort(L, R: Integer);
var
  I, J: Integer;
  P: TValue;
begin
  repeat
    I := L;
    J := R;
    P := FList^[(L + R) shr 1].FValue1;
    repeat
      while Field.DataType.Compare(FList^[I].FValue1, P) < 0 do Inc(I);
      while Field.DataType.Compare(FList^[J].FValue1, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TCriteriaValueList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TCritValueItem));
  FCapacity := NewCapacity;
end;

{ TCriteriaFieldIterator }

function TCriteriaFieldIterator.GetCritField : TCriteriaField;
begin
  Result := TCriteriaField(FIterator.Data);
end;

function TCriteriaFieldIterator.GetDataField : TDataField;
begin
  Result := FIterator.Index;
end;

constructor TCriteriaFieldIterator.Create(Criteria : TCriteria);
begin
  inherited Create;
  if Criteria <> nil then
    FIterator := TIndexContainerIterator.Create(Criteria.FFields)
  else
    FIterator := nil;
end;

destructor TCriteriaFieldIterator.Destroy;
begin
  inherited Destroy;
  FIterator.Free;
end;

procedure TCriteriaFieldIterator.First;
begin
  if FIterator <> nil then
    FIterator.First;
end;

procedure TCriteriaFieldIterator.Next;
begin
  if FIterator <> nil then
    FIterator.Next;
end;

function TCriteriaFieldIterator.EOF : Boolean;
begin
  if FIterator <> nil then
    Result := FIterator.EOF
  else
    Result := True;
end;

initialization

  RegisterClasses([

                   TCriteria, TCriteriaField]);

end.


