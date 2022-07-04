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

{ $Id: DataElements.pas,v 1.670 2003/04/16 14:53:28 laa Exp $}

{-------------------------------------------------------------------------
  DataElements     Polycons DataElements

  What             TDataField (with descendents)
                   TDataTable (with descendents)
                   TDataRow (with descendents)
                   TAbstractRowStorage (with descendents)

  Company          Polycon
  Authors          LGE
-------------------------------------------------------------------------}

unit DataElements;

interface
{$i common.inc}

uses
  Classes, SysUtils,  DataTypes, DataType, IndexContainer, Interruptable, LangArray



{$ifndef LINUX}



{$else LINUX}
  {,SqlExpr}


{$endif LINUX}
  ;

{/**
  * Polycon's DataElements
  *
  * Importent: InitUnit must be called before any of the unit's classes are used!!
  *
  * author LGE Leif Esselström
  */}

const
  LanguageCount = 3;

type
  TypeException = class(Exception)
  end;





  TLogFunction = procedure(const LogType : TLogType; const LogKey, LogMessage : String) of object;

  // Forward declarations
  TLangFieldArray = class;
  TDataTableType = class;
  TDataTable = class;
  TAuxTable = class;
  TDataField = class;
  TKeyField = class;
  TRowSource = class;
  TAbstractRowStorage = class;
  TSubTotalRow = class;
  TDataRow = class;
  TAbstractRow = class;
  TRowSortOrder = class;
  TCommonQuilt = class;
  TDataFieldSet = class;

  TCacheLoadPolicy = class;
  TGetRowsHandler = class;
  TCondition = class;
  TDataBridge = class;

  TSingleton = class;

  TGetFieldEvent = procedure(Sender : TLangFieldArray; var Field : TDataField) of object;
  TQueryValueFunction = function(Field : TDataField) : TValue of object;
  TQueryStringFunction = function(Field : TDataField) : String of object;
  TQueryDataFieldFunction = function(Field : TDataField; var ConflictAction : TConflictAction; var KeepSrc : Boolean) : TDataField of object;
  TProcFieldCond = procedure(Field : TDataField; AcceptsAll : Boolean) of object;
  TProcDataField = procedure(DataField : TDataField) of object;
  TStoreValueMethod = procedure(idx : Integer; Value : TValue) of object;
  TProcRow = procedure(ARow : TAbstractRow) of object;
  TStorageNotifyEvent = procedure(Sender : TAbstractRowStorage; ChangedKeys : TCommonQuilt) of object;
  TTableSavedEvent = procedure(Sender : TAbstractRowStorage) of object;
  TAutoUpdateProc = function(Sender : TDataTable; ARow : TDataRow; AField : TDataField) : TValue of object;

  TWriteAction = (wtRowStorageSave, wtDiffSave, wtUpdateSave, wtInsertOnly);

















  TSingleton = class(TComponent)
  private
    FChanged : TDateTime;
    FHelpTexts : TLangArray;

    procedure AssignHelpTexts(Value : TLangArray);
    function GetHelpText : String;
  protected
    {/** Override this procedure to save any unpublished properties */}
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetName(const NewName: TComponentName); override;
    procedure SetParentComponent(Value: TComponent); override;

    procedure GetCurrentLanguageIndex(Sender : TCommonLangArray; var Language : Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;

    procedure Modified;
    procedure SetHelpTexts(HelpTexts : array of String);
    property HelpTexts : TLangArray read FHelpTexts write AssignHelpTexts;
    property HelpText : String read GetHelpText;
    property Changed : TDateTime read FChanged;


    {/** Fixes the missing object pointer property */}

  end;



  TOnBridgeDataEvent = procedure(DataType : TDataType; var Value : TValue) of object;

  AffectedRowsException = class(Exception);

  TDataBridge = class(TSingleton)
  private
    FCanRead, FCanWrite : Boolean;


    FOnReadDbData : TOnBridgeDataEvent;
    FOnWriteDbData : TOnBridgeDataEvent;
  protected
    fReuseParameters : Boolean;

    function GetSupportsSQL : Boolean; virtual;


    constructor CreateOld(CanRead, CanWrite : Boolean);
  public
    property SupportsSQL : Boolean read GetSupportsSQL;






//    function InsertData(Sender : TDataTable; NewValues : TAbstractRow; Interruptable : TInterruptable) : integer; virtual;
    function UpdateData(Sender : TDataTable; Condition : TCondition; NewValues : TAbstractRow; AffectedFields : TDataFieldSet; Interruptable : TInterruptable) : integer; virtual;
    function DeleteData(Sender : TDataTable; Condition : TCondition; Interruptable : TInterruptable) : integer; virtual;




//    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property OnReadDbData : TOnBridgeDataEvent read FOnReadDbData write FOnReadDbData;
    property OnWriteDbData : TOnBridgeDataEvent read FOnWriteDbData write FOnWriteDbData;

    property ReuseParameters : Boolean read fReuseParameters {write fReuseParameters};
    property CanRead : Boolean read FCanRead write FCanRead;
    property CanWrite : Boolean read FCanWrite write FCanWrite;
  end;

  TGetValues = procedure(Results : TStrings; Table : TDataTable; Row : TAbstractRow; Condition : TCondition) of object;

  {/** A TDataField describes a field in a database table */}
  TDataField = class(TSingleton)
  private
    {/** Properties given at create */}
    FFieldName : String;
    FDataType : TDataType;
    FDefaultValue : TValue;
    FDisplayWidth : integer;
    FLookupTable : TAuxTable;
    FTablesUsingMe : TIndexContainer;
    FDisableIntervals : Boolean;

    {/** Is it legal to sum this fields value of multiple rows */}
    FIsAggregable : Boolean;
    FHasCombo : Boolean;
    FAllowSelections : Boolean;
    FIsReportable : Boolean;
    // FIsLanguageDependent : Boolean;
    {/** Properties read from database at initialisation */}

    { FShortDescriptions : TStringList;
      FLongDescriptions : TStringList; }

    FShortDescriptions : TLangArray;
    FLongDescriptions : TLangArray;
    FRefillType: TRefillType;

    procedure SetLookupTable(AuxTable : TAuxTable);
    function GetLookupField : TDataField;
    function GetLookupKey(idxKey : Integer) : TDataField;
    function GetIsRunningNumber : Boolean;
    procedure SortInterval(Values : TStrings; Order : TSortOrder; Low, High : Integer);
    function GetShortDescription : String;
    function GetLongDescription : String;

    procedure SetRefillType(const Value: TRefillType);
    procedure GetDefaultTextField(Sender : TLangFieldArray; var Field : TDataField);
  private
    // former keyfield methods

    {FTextFields : TList;
    FShortTextFields : TList;}

    FTextFields : TLangFieldArray;
    FShortTextFields : TLangFieldArray;

    FSortField : TDataField;
    FSortOrder : TSortOrder;
    FDirectAuxTableField : TDataField;
    {/** Original field in AuxTable, may differ (e.g. CC for RC and CCTakes) */}
    FAuxTableField : TDataField;
    FCriteria : TCommonQuilt;
    {/** Essential parametres filled in later, when creating AuxTables */}
    FAuxTable : TAuxTable;

    FCanGenerateCondSQL : Boolean;
    FCanGenerateValueSQL : Boolean;

    FDependsOn : TDataField;
    FDependentChildren : TList;
    FDerivedFields : TList;
    FDependentLookupField : TDataField;

    function GetHasAuxTable : Boolean;
    function GetTextField : TDataField;
    function GetShortTextField : TDataField;
    // procedure SetIsLanguageDependent(AValue : Boolean);
    // function GetIsLanguageDependent : Boolean;
    function DependentHasOneValue(ADependentField : TDataField) : Boolean;
    procedure SetFieldName(NewName : String);
    procedure SetDataType(NewDataType : TDataType);
    procedure SetDisplayWidth(AWidth : Integer);
    procedure SetDependParent(AParent : TDataField);
    procedure SetAuxTableField(AField : TDataField);
    function GetIntervals : Boolean;

    procedure SetShortDescriptions(ADesr : TLangArray);
    procedure SetLongDescriptions(ADesr : TLangArray);
    procedure SetTextFields(AFields : TLangFieldArray);
    procedure SetShortTextFields(AFields : TLangFieldArray);
  protected
    FDefaultReadOnly : Boolean;
    FDisplayValues : TDisplayValues;

    function FetchValue(ARow : TAbstractRow; idx : Integer) : TValue; virtual;
    function GetReadOnly(ARow : TAbstractRow) : Boolean; virtual;
    function GetDefaultDisplayValues : TDisplayValues;
    function GetCanBeInDB : Boolean; virtual;
    function GetSQLString(FullFieldName : TQueryStringFunction; DoAggregate : Boolean) : String; virtual;
    function GetExternValue(ARow : TAbstractRow) : TValue; virtual;
    function SetExternValue(ARow : TAbstractRow; Value : TValue; Action : TSetAction) : TSetResult; virtual;
    procedure DefineProperties(Filer: TFiler); override;

    procedure SetName(const NewName: TComponentName); override;
  protected
    // former keyfield methods
    function GetRowsFieldTranslator(Field : TDataField; var ConflictAction : TConflictAction; var KeepOld : Boolean) : TDataField;
    procedure SetAuxTable(Table : TAuxTable); virtual;
    function GetDependentFieldCount : Integer; virtual;
    function GetDependentField(idx : Integer) : TDataField; virtual;
    procedure AddDependentChild(Child : TDataField); virtual;
    procedure RemoveDependentChild(Child : TDataField); virtual;
    function GetDependentChildCount : Integer; virtual;
    function GetDependentChild(idx : Integer) : TDataField; virtual;
    function GetDerivedFieldCount : Integer; virtual;
    function GetDerivedField(idx : Integer) : TDataField; virtual;
    function GetAncestorCount : Integer; virtual;
    function GetAncestor(idx : Integer) : TDataField; virtual;

    {/** Function for defining if we should refill a combo because of changes to dependent fields */}
    function DoDependentRefill(CurrentCondition, FillCondition: TCondition): Boolean; virtual;
    {/** Function for defining if we should refill a combo because of changes to the auxtable */}
    function DoTimeStampRefill(FillTimeStamp: TDateTime): Boolean;
    {/** Extract the Code and Data parts of the given GetValues-function */}
    function GetValuesToMethod(GV: TGetValues): TMethod;
    {/** Check if the virtual method GetValues has been overridden to force combo refill */}
    function GetValuesOverridden: Boolean; virtual;
  published
    {Basic properties}
    property FieldName : String read FFieldName write SetFieldName; // DB FieldName
    property DataType : TDataType read FDataType write SetDataType; // Field Datatype
    property DefaultValue : TValue read FDefaultValue write FDefaultValue;

    {Visual properties}
    property DisplayWidth : Integer read FDisplayWidth write SetDisplayWidth; // Default displaywidth, including explanation


    {Key properties}
    {/** Shall this field be displayed in e.g. a selectionpanel */}
    property AllowSelections : Boolean read FAllowSelections write FAllowSelections;
    property IsReportable : Boolean read FIsReportAble write FIsReportAble; // Allow as group by/sum by
    property IsRunningNumber : Boolean read GetIsRunningNumber;
    property SortField : TDataField read FSortField write FSortField;
    property SortOrder : TSortOrder read FSortOrder write FSortOrder;
    {/** Additional conditions that apply to base AuxTable (e.g. CcIsARc ='T' for RC) */}
    property Criteria : TCommonQuilt read FCriteria write FCriteria; // Fixa LGE Setmetod: freea gamla; assigna nya
    property DisableIntervals : Boolean read FDisableIntervals write FDisableIntervals;
    property Intervals : Boolean read GetIntervals;


    {Combo properties}
    {/** Does the field have few enough values to be shown in a combo */}
    property HasCombo : Boolean read FHasCombo write FHasCombo;
    {/** In what manner are combos for this field refilled */}
    property RefillType : TRefillType read FRefillType write SetRefillType;
    {/** DisplayValues for this field */}
    property DisplayValues : TDisplayValues read FDisplayValues write FDisplayValues;


    {Aggregable properties}
    {/** Can we sum this field on an aggregated level? */}
    property IsAggregable : Boolean read FIsAggregable write FIsAggregable;

    {Database properties}
    property CanGenerateCondSQL : Boolean read FCanGenerateCondSQL write FCanGenerateCondSQL;
    property CanGenerateValueSQL : Boolean read FCanGenerateValueSQL write FCanGenerateValueSQL;
  public
    function GetDependentFieldByDerivedField(AField : TDataField) : TDataField; virtual; // Fixa LGE bättre namn sku va på plats...
    function DisplayTextDataType(DisplayValues : TDisplayValues) : TDataType;


    property CanBeInDB : Boolean read GetCanBeInDB;
    property SQLString[FullFieldName : TQueryStringFunction; DoAggregate : Boolean] : String read GetSQLString;

    property ShortDescriptions : TLangArray read FShortDescriptions write SetShortDescriptions;
    property LongDescriptions : TLangArray read FLongDescriptions write SetLongDescriptions;

    property TextFields : TLangFieldArray read FTextFields write SetTextFields;
    property ShortTextFields : TLangFieldArray read FShortTextFields write SetShortTextFields;

    {Lookup properties and dependensies}
    {/** When doing autolookup, which table should be used for joining */}
    property LookupTable : TAuxTable read FLookupTable write SetLookupTable;
    property DependParent : TDataField read FDependsOn write SetDependParent;
    {/** Original field in AuxTable, may differ (e.g. CC for RC and CCTakes) */}
    property AuxTableField : TDataField read FAuxTableField write SetAuxTableField;
    {/** The auxiliary table the key field belongs to */}
    property AuxTable : TAuxTable read FAuxTable write SetAuxTable;
    {/** MarketRCs LookupField is RC and ORG2s LookupField is CC */}
    property LookupField : TDataField read GetLookupField;
    property LookupKey[idxKey : Integer] : TDataField read GetLookupKey;
    property DependentLookupField : TDataField read FDependentLookupField write FDependentLookupField; // Fixa LGE här borde vi inte tillåta vad som helst på write...

    {/** The field this field is directly dependent on */}
    property DependentChildCount : Integer read GetDependentChildCount;
    property DependentChild[idx : Integer] : TDataField read GetDependentChild;

    property AncestorCount : Integer read GetAncestorCount;
    property Ancestor[idx : Integer] : TDataField read GetAncestor;

    property DependentFieldCount : Integer read GetDependentFieldCount;
    property DependentField[idx : Integer] : TDataField read GetDependentField;
    function IsDependentField(AKeyField : TDataField) : Boolean;

    property DerivedFieldCount : Integer read GetDerivedFieldCount;
    property DerivedField[idx : Integer] : TDataField read GetDerivedField;

    property HasAuxTable : boolean read GetHasAuxTable;
    property DirectAuxTableField : TDataField read FDirectAuxTableField;


    {Readonly properties}
  published
    property DefaultReadOnly : Boolean read FDefaultReadOnly write FDefaultReadOnly;
  public
    property ReadOnly[ARow : TAbstractRow] : Boolean read GetReadOnly;
//    property AlwaysReadOnly : Boolean read FDefaultReadOnly write FDefaultReadOnly; LGE: Use property DefaultReadOnly


    {Descriptions and textfields}
    property ShortDescription : String read GetShortDescription;
    property LongDescription : String read GetLongDescription;

    // property IsLanguageDependent : Boolean read GetIsLanguageDependent write SetIsLanguageDependent; // Multiple Textfields?
    property TextField : TDataField read GetTextField;
    property ShortTextField : TDataField read GetShortTextField;

    {Methods}
    constructor DataFieldCommonCreate(AOwner: TComponent);
    constructor Create(AOwner: TComponent); override;
    constructor CreateOld(const FieldName : String; DataType : TDataType);
    destructor Destroy; override;

    procedure AddToList(AList : TList; AddCalcFields : Boolean); virtual;

//    procedure SetShortDescription(Language : Integer; const Descr : String); virtual;
//    procedure SetLongDescription(Language : Integer; const Descr : String); virtual;
//    procedure SetTextField(Language : Integer; AField : TDataField);
//    procedure SetShortTextField(Language : Integer; AField : TDataField);


    procedure SetAllDescriptions(const Descriptions : array of String);
    procedure SetDescriptions(const ShortDescriptions, LongDescriptions : array of String);
    procedure SetAllTextFields(const TextFields : array of TDataField);
//    procedure SetTextFields(TextFields, ShortTextFields : TList);

//    function GetShortDescByIndex(Language : Integer) : String;
//    function GetLongDescByIndex(Language : Integer) : String;
//    function GetShortTextFieldByIndex(Language : Integer) : TDataField;
//    function GetTextFieldByIndex(Language : Integer) : TDataField;

    procedure CopyDescriptionsFrom(AField : TDataField);



    {/** Locate DataField by FieldName */}
    class function FieldByName(const Name : String) : TDataField;
    {/** if ADataField is derivied from Self, result = Self, else result = nil */}
    function FindDerivedSource(ADataField : TDataField) : TDataField;
    {/** Sort the Strings (not the objects!) in the list with this Fields datatype */}
    procedure SortList(Values : TStrings; Order : TSortOrder);
    class function InstanceCount : Integer;
    class function Instance(idx : Integer) : TDataField;

    function DoRefill( CurrentCondition, FillCondition : TCondition; FillTimeStamp : TDateTime ) : Boolean; virtual;

    procedure GetValues(Results : TStrings; Table : TDataTable; Row : TAbstractRow; Condition : TCondition); virtual;
    procedure GetRows(Results : TStrings; Condition : TCondition); virtual;
    procedure GetRowsByDependeeFields(Results : TStrings; Condition : TCondition); virtual;

    {/** Is this a derived field that has a criteria (e.g. RC or ORG1RC) */}
    function IsDerivedWithCriteria : Boolean;

  end;

  TKeyField = class(TDataField)
  public
    constructor CreateNonAuxTabled(const FieldName : String; DataType : TDataType);
    {/** Normal constructor for KeyFields */}
    constructor Create(AOwner: TComponent); override;
    constructor CreateOld(const FieldName : String; DataType : TDataType;
                          TextFields : array of TDataField; DisplayValues : TDisplayValues; HasCombo : Boolean;
                          DisplayWidth : Integer; DependsOn : TDataField);
    {/** Constructor for keyfields with sortfield */}
    constructor CreateWithSortField(const FieldName : String; DataType : TDataType;
                                    TextFields : array of TDataField; DisplayValues : TDisplayValues; HasCombo : Boolean;
                                    DisplayWidth : Integer; DependsOn : TDataField;
                                    SortField : TDataField; SortOrder : TSortOrder);
    {/** Special constructor for derived fields (CCTakes, RC, etc) */}
    constructor CreateDependent(const FieldName : String; AuxTableField : TDataField; Criteria : TCondition; LookupKey : TDataField);
    {/** Special constructor for very derived fields (Org1RC, MarketRC, etc) */}
//    constructor CreateDependentAndParented(FieldName : String; AuxTableField : TDataField; Patch : TQuiltPatch; DependsOn : TDataField);
    {/** Destructor */}
    destructor Destroy; override;
    {/** Add special short text fields, normally fields ony have one set of text fields. */}
//    procedure SetShortTexts(ShortTextFields : array of TDataField);
    function ValueDefined(ARow : TDataRow) : Boolean;
  end;

  TDataFieldList = class(TDataField)
  protected
    function GetFirstIndex : Integer; virtual; abstract;
    function GetLastIndex : Integer; virtual; abstract;
    function GetFieldCount : Integer; virtual; abstract;
    function GetField(idx : Integer) : TDataField; virtual; abstract;
  public
    function ContainsField(AField : TDataField) : Boolean; virtual; abstract;
    property FirstIndex : Integer read GetFirstIndex;
    property LastIndex : Integer read GetLastIndex;
    property FieldCount : Integer read GetFieldCount;
    property Field[idx : Integer] : TDataField read GetField; default;
  end;

  TRunningNumberGenerator = class
  public

    function IsTempRunningNumber(Row : TDataRow) : Boolean; virtual; abstract;
    function GetNextTempRunningNumber(Table : TDataTable; const Values : TValueList) : TValue; virtual; abstract;
    procedure SaveFailed(Sender : TDataBridge; Row : TDataRow); virtual;
  end;

  TDefaultRunningNumberGenerator = class(TRunningNumberGenerator)
  public

    function IsTempRunningNumber(Row : TDataRow) : Boolean; override;
    function GetNextTempRunningNumber(Table : TDataTable; const Values : TValueList) : TValue; override;
  end;

  TRunningNumberField = class(TKeyField)
  private
    FRunningNumberGenerator : TRunningNumberGenerator;
    procedure SetRunningNumberGenerator(ARNG : TRunningNumberGenerator);
  public
    property RunningNumberGenerator : TRunningNumberGenerator read FRunningNumberGenerator write SetRunningNumberGenerator;
    {/** Normal constructor for KeyFields */}
    constructor Create(AOwner: TComponent); override;
    constructor CreateOld(const FieldName : String; DataType : TDataType;
                          TextFields : array of TDataField; DisplayValues : TDisplayValues; HasCombo : Boolean;
                          DisplayWidth : Integer; DependsOn : TDataField);
    {/** Constructor for keyfields with sortfield */}
    constructor CreateWithSortField(FieldName : String; DataType : TDataType;
                                    TextFields : array of TDataField; DisplayValues : TDisplayValues; HasCombo : Boolean;
                                    DisplayWidth : Integer; DependsOn : TDataField;
                                    SortField : TDataField; SortOrder : TSortOrder);
    {/** Destructor */}
    destructor Destroy; override;
  end;

  TRowStorageTreeKey = class
  private
    FStorage : TAbstractRowStorage;
    FTreeKey : TDataField;
    FSortField : TDataField;
    FSortOrder : TSortOrder;
    FTreeKeyIndex : Integer;
    FRowFieldIndex : Integer;
    FVisible : Boolean;

    procedure SetSortField(SortField : TDataField);
    procedure SetSortOrder(SortOrder : TSortOrder);
    procedure SetVisible(V : Boolean);
    function GetVisible : Boolean;
  protected
    procedure SetTable(Table : TAbstractRowStorage);
    function GetValue(ARow : TAbstractRow) : TValue;
    function IsDetailLevel : Boolean;

    function CreateCopy(Storage : TAbstractRowStorage) : TRowStorageTreeKey;
    constructor CreateDetailLevel(Storage : TAbstractRowStorage);
    constructor CreateSorted(Storage : TAbstractRowStorage; TreeKey : TDataField; SortField : TDataField; SortOrder : TSortOrder);
    constructor Create(Storage : TAbstractRowStorage; TreeKey : TDataField);
  public
    property TreeKey : TDataField read FTreeKey;
    property TreeKeyIndex : Integer read FTreeKeyIndex;
    property RowFieldIndex : Integer read FRowFieldIndex;
    property SortField : TDataField read FSortField write SetSortField;
    property SortOrder : TSortOrder read FSortOrder write SetSortOrder;
    property Visible : Boolean read GetVisible write SetVisible;

    procedure ResetSortField;
    destructor Destroy; override;
  end;

  TDataTable = class(TSingleton)
  private
    FKeyCount : Integer;
    FFieldCount : Integer;
    FFields : TList;
    FFieldsFastSearchList : TValueList;
    FTableName : String;
    FDataBridge : TDataBridge;

//    FDescriptions : TStringList;
    FDescriptions : TLangArray;

    FTableType : TDataTableType;
    FFieldLists : TList;
    FLookupFields : TList;
    FLookupSources : TList;

    FFieldData : TList;
    FRunningNumberField : TRunningNumberField;

    FOnSaved : TTableSavedEvent;
    FAutoUpdateFieldAndProcs : TList;

    procedure CheckDoubleFields;
    procedure SetTableName(NewName : String);
    procedure SetDataTableType(NewType : TDataTableType);

    function GetPresentDescription : String;
    function GetReadPos(idx : Integer) : Integer;
    function GetDataLength : Integer;

    function GetField(idx : Integer) : TDataField;
    function GetFieldCount : Integer;
    function GetIgnoreFieldCount : Integer;
    function GetFieldListCount : Integer;
    function GetFieldList(idx : Integer) : TDataFieldList;

    function GetFieldsAsText : String;
    function IndexOfFieldOrDependent(Field : TDataField) : Integer;
    procedure SetRunningNumberField(AField : TRunningNumberField);


    procedure SetField(AList : TList; ADataField : TDataField; AIndex : Integer);
    procedure SetLookupField(AIndex : Integer; AField : TDataField);
    procedure SetLookupSource(AIndex : Integer; AField : TDataField);
    function GetLookupField(AIndex : Integer) : TDataField;
    function GetLookupSource(AIndex : Integer) : TDataField;
    function GetLookupRuleField(index: integer): TDataField;
    function GetLookupRuleValue(index: integer): TDataField;
    function GetAutoUpdateProc(AField: TDataField): TAutoUpdateProc;
    procedure SetAutoUpdateProc(AField: TDataField;
      const Value: TAutoUpdateProc);
    function IndexOfFieldAndProc(AField: TDataField): Integer;
  protected
//    procedure CalculateStoragePositions;
    {/** Internal FieldNamecalcfunction */}
    function FullFieldName(Field : TDataField; FieldNames : TStringList) : String;
    procedure DefineProperties(Filer: TFiler); override;

    procedure AutoUpdateRow(ARow : TDataRow);
  public

    {/** Row that has all values set to default for their type.
         Used as default when new rows are Created */}
    IDefaultRow : TDataRow;

    procedure CalculateStoragePositions;



    procedure CleanupList;

    property OnSaved : TTableSavedEvent read FOnSaved write FOnSaved;
    function DefaultFieldName(Field : TDataField) : String;
    property ReadPos[idx : Integer] : Integer read GetReadPos;
    property DataLength : Integer read GetDataLength;

    property Descriptions : TLangArray read FDescriptions;

    {/** The number of keys of this table */}
    property KeyCount : Integer read FKeyCount;
    {/** The number of fields of this table including key fields */}
    property FieldCount : Integer read GetFieldCount;
    property IgnoreFieldCount : Integer read GetIgnoreFieldCount;
    {/** The fields of this table */}
    property Field[idx : Integer] : TDataField read GetField; default;
    {/** Number of FieldLists (MonthlyFieldLists) in this table */}
    property FieldListCount : Integer read GetFieldListCount;
    {/** FieldList n */}
    property FieldList[idx : Integer] : TDataFieldList read GetFieldList;
    {/** The description of this DataTable in the current language */}
    property Description : String read GetPresentDescription;

    {/** By assigning a function for a field you will be able to change
         the value of the field if the row has been changed when it is saved */}
    property AutoUpdateProc[AField : TDataField] : TAutoUpdateProc read GetAutoUpdateProc write SetAutoUpdateProc;

    property LookupField[AIndex : Integer] : TDataField read GetLookupField write SetLookupField;
    property LookupSource[AIndex : Integer] : TDataField read GetLookupSource write SetLookupSource;

    {/** Constructor */}
    constructor Create(AOwner: TComponent); override;
    constructor CreateOld(TableName : String; TableType : TDataTableType;
                          Keys : array of TDataField; Fields : array of TDataField;
                          DataBridge : TDataBridge);
    {/** Destructor */}
    destructor Destroy; override;
//    procedure SetDataTableType2(NewType : TDataTableType);
    procedure AddIgnoreFields(Fields : array of TDataField);
//    function GetDescription(Language : Integer) : String;
//    procedure SetDescription(Language : Integer; Descr : String);
    procedure SetDescriptions(Descriptions : array of String);
    procedure AddLookupRule(LookupField : TDataField; ValueField : TDataField);
    procedure InsertLookupRule(AIndex : Integer; LookupField : TDataField; ValueField : TDataField);
    procedure RemoveLookupRule(LookupField : TDataField);
    procedure RemoveLookupRuleByIndex(AIndex : Integer);
    function LookupRulesCount : Integer;
    function IndexOfLookupRule(LookupField : TDataField) : Integer;


    {/** The index of this field */}
    function IndexOfField(Field : TDataField) : Integer;
    {/** Is this field a key? */}
    function FieldIsKey(DataField : TDataField) : Boolean;
    {/** Does the table contain this field? */}
    function TableHasField(Field : TDataField) : Boolean;
    {/** Does the table contain a field that has this field as it's derived field? */}
    function TableHasParentField(ADependentField : TDataField) : Boolean;
    {/** Does the table contain a field, whose auxtablefield is this field? */}
//    function TableHasDependent(Field: TDataField; var OnWhichKey : TDataField): Boolean;
    {/** Does the table have this field as a key? */}
    function TableHasKey(Field : TDataField) : Boolean;
    {/** Does the table have this field as a nonkey? */}
    function TableHasNonKey(Field : TDataField) : Boolean;
    {/** Does the table contain this datafieldlist */}
    function TableHasFieldList(Field : TDataField) : Boolean; virtual;
    {/** Used for operations: are the keys and fields the same (order doesn't matter) */}
    function CompatibleWith(DataTable : TDataTable) : Boolean;
    {/** Find the DataField in this table that ADataField is dependent of * /}
    function FindDependentSource(ADataField : TDataField) : TDataField;
    {/** Update SQL for this table */}


    {/** Get the running number field of this table (if it has one */}
    property RunningNumberField : TRunningNumberField read FRunningNumberField write SetRunningNumberField;
    {/** Copy this table's fields to a TList */}
    procedure FieldsToList(AList : TList);
    {/** Copy this table's keys to a TList */}
    procedure KeysToList(AList : TList; AddFictiveFields:Boolean=True);
    {/** The DataTable where this field can be found (Self or lookupable table) */}
    function PrimaryTable(Field : TDataField) : TDataTable;
    function ClearDb(ClearCriteria : TCondition) : Integer; virtual;
    property FieldsAsText : String read GetFieldsAsText;
    procedure AddFieldsToFieldSet(FieldSet : TDataFieldSet);
    procedure AddNonAggregablesToFieldSet(FieldSet : TDataFieldSet);


    {/** Locate DataTable by TableName */}
    class function TableByName(Name : String) : TDataTable;
    class function InstanceCount : Integer;
    class function Instance(idx : Integer) : TDataTable;
  private
    FLocked : Boolean;
    procedure CheckLocked;
  public
    {/** Constructor, simular to TDataTable's constructor */}
    constructor CreateEmpty(TableName : String; TableType : TDataTableType; DataBridge : TDataBridge);
    {/** Create a copy of another DataTable */}
    constructor CreateCopy(Table : TDataTable; TableType : TDataTableType; DataBridge : TDataBridge); virtual;
    {/** Add a KeyField */}
    procedure AddKey(AKeyField : TDataField);
    {/** Add a DataField */}
    procedure AddField(ADataField : TDataField);
    {/** Add an IgnoreField */}
    procedure AddIgnore(AIgnoreField : TDataField);
    {/** Remove a field from the table */}
    procedure RemoveDataField(AField : TDataField);
    procedure RemoveDataFieldByIndex(AIndex : Integer);
    {/** Move a key within the table */}
    procedure MoveKey(AKeyField : TDataField; NewPos : Integer);
    {/** Move a field within the table */}
    procedure MoveField(ADataField : TDataField; NewPos : Integer);
    {/** Remove all fields from a dynatable */}
    procedure Clear;
    {/** Remove current contents and copy from ADataTable */}
    procedure CopyFrom(ADataTable : TDataTable);
    {/** Lock this DynaTable's structure */}
    procedure Lock;
    {/** Is this dynatable locked? */}
    property Locked : Boolean read FLocked;
    property LookupRuleField[index:integer] : TDataField read GetLookupRuleField;
    property LookupRuleValue[index:integer] : TDataField read GetLookupRuleValue;
  published
    property DataBridge : TDataBridge read FDataBridge write FDataBridge;
    {/** The TableName of this table */}
    property TableName : String read FTableName write SetTableName;
    {/** The TableType of this table */}
    property TableType : TDataTableType read FTableType write SeTDataTableType;
  end;

  TDataTableType = class(TSingleton)
  private
    Tables : TStringList;
    FDescriptions : TLangArray;
    function GetTableCount : Integer;
    function GetTable(idx : Integer) : TDataTable;
    procedure SetTable(AIndex : Integer; ADataTable : TDataTable);
    function GetTableByName(Name : string) : TDataTable;
    function GetTypeIndex : Integer;
    function GetSortTables : Boolean;
    procedure SetSortTables(AValue : Boolean);
    procedure AddTable(Table : TDataTable);
    procedure UpdateTableName(Table : TDataTable);
    procedure RemoveTable(Table : TDataTable);
    procedure SetDescriptions(ADescriptions : TLangArray);

  public
    procedure CleanupList;

    property TableCount : Integer read GetTableCount;
    property Table[idx : Integer] : TDataTable read GetTable write SetTable;
    property TableByName[name : string] : TDataTable read GetTableByName;
    property TypeIndex : Integer read GetTypeIndex;
    constructor Create(AOwner: TComponent); override;
    constructor CreateOld(Descriptions : array of String);
    destructor Destroy; override;

    class function InstanceCount : Integer;
    class function Instance(idx : Integer) : TDataTableType;
  published
    property SortTables : Boolean read GetSortTables write SetSortTables;
    property Descriptions : TLangArray read FDescriptions write SetDescriptions;
  end;



  {/** record for defining sortorder*/}
(*  TOrderField = record
    Field : TDataField;
    Order : TSortOrder;
  end;

  TOrderInt = record
    Index : Integer;
    Order : TSortOrder;
  end; *)

  {/** DataTable internal class */}
  TFieldDataObject = class
  public
    ReadPos : Integer;
    constructor Create(ReadPos : Integer);
    // no destructor needed
  end;

  TAbstractRow = class
  private
    FDataTable : TDataTable;
    FVisible : Boolean;
    FSpecialValueList : TStringList;

    function GetFieldStringValue(Field : TDataField) : String;
    function GetFieldIntValue(Field : TDataField) : Integer;
    function GetFieldDoubleValue(Field : TDataField) : Double;
    function GetFieldDateTimeValue(Field : TDataField) : TDateTime;
    function GetFieldCurrencyValue(Field : TDataField) : Currency;
    function GetFieldBooleanValue(Field : TDataField) : Boolean;
    function GetFieldPointerValue(Field : TDataField) : Pointer;

    procedure SetFStringValue(Field : TDataField; Value : String);
    procedure SetFDoubleValue(Field : TDataField; Value : Double);
    procedure SetFIntValue(Field : TDataField; Value : Integer);
    procedure SetFCurrencyValue(Field : TDataField; Value : Currency);
    procedure SetFDateTimeValue(Field : TDataField; Value : TDateTime);
    procedure SetFBooleanValue(Field : TDataField; Value : Boolean);
    procedure SetFPointerValue(Field : TDataField; Value : Pointer);

    function GetDisplayText(AField : TDataField) : String;
    function GetDisplayString(AField : TDataField; DisplayValues : TDisplayValues) : String;
    function GetKeyAndDescription(AField : TDataField) : String;
    function GetShortDisplayText(AField : TDataField) : String;
    function GetShortDisplayString(AField : TDataField; DisplayValues : TDisplayValues) : String;
    procedure SetVisible(Vis : Boolean);
    function GetFieldSQLValue(Field : TDataField) : String;
    function GetSQLIndexValue(idx : Integer) : String;

    function GetAuxTableRow(AField, ATextField : TDataField; out CanHavetext : Boolean) : TAbstractRow;
    function GetDisplayStringWithTextField(AField, ATextField : TDataField; DisplayValues : TDisplayValues) : String;
    function GetKeyAndDescriptionWithTextField(AField, ATextField : TDataField) : String;

    function SpecialValueList(MinIndex : Integer) : TStrings;
    procedure ReadSpecialValue(Index : Integer; out Value : String);
    procedure StoreSpecialValue(Index : Integer; Value : String);
  protected
    FStorage : TAbstractRowStorage;
    FSubTotalRow : TSubTotalRow;

    procedure RawCopyContents(ASrcRow : TAbstractRow);

    procedure SetFValue(Field : TDataField; Value : TValue); virtual;
    procedure SetIValue(idx : Integer; Value : TValue); virtual;
{$ifdef COMMONLASKU}
    //MAB 29.08.02, Virt. metod som kan overridas, kallas från SetFieldValue om Self <> nil
    function SetFValueVirt(Field : TDataField; Value : TValue; Action : TSetAction) : TSetResult; virtual;
    //MAB 29.08.02, Kallas av SetFieldValue om Self = nil och som default SetFValueVirt
    function DoSetFieldValue(Field : TDataField; Value : TValue; Action : TSetAction) : TSetResult;
{$endif COMMONLASKU}

    function ContainsSubRows : Boolean; virtual; abstract;
    function IsUpdating : Boolean; virtual; abstract;
    {/** Constructor; this abstract class shouldn't be instantied */}
    constructor Create(DataTable : TDataTable);

    function SetIndexValue(idx : Integer; Value : TValue; Action : TSetAction) : TSetResult; virtual; abstract;

    {/** Store (primitive) a value into the row */}
    procedure StoreValue(idx : Integer; Value : TValue);
    {/** Get (primitive) the value for this row/field */}
    function FetchValue(idx : Integer) : TValue;

    {/** Requires field to exist; raises Exception if field doesn't exist */}
    function LegalIndexOfField(Field : TDataField) : Integer;

    {/** Used to create a list of rows for the user of TRowStorage */}
    procedure FillRowList(ARowList : TStrings; ExtraCondition : TCondition; DefaultSort : Boolean); virtual; abstract;
    {/** A very powerful function used to lookup (join) values from auxiliary tables */}
    function Lookup(var AField : TDataField) : TAbstractRow;
    {/** Nr of rows with this status */}
    function GetStatusCount(Status : TRowStatus) : Integer; virtual; abstract;
    {/** Valid nr of Keys */}
    function GetValidKeyCount : Integer; virtual; abstract;
    function GetValidKeyValue(idx : Integer) : TValue;
    function GetValidKey(idx : Integer) : TDataField;
//    function SaveInterrupt(UpdateQuery, InsertQuery, MaxNoQuery : TDataQuery; RunningNumberIndex : Integer;
//                            Interruptable : TInterruptable; AffectedFields : TDataFieldSet; ProcModifiedRow : TProcRow) : Integer; virtual; abstract;

    procedure BeforeFetchValue(Field : TDataField); virtual;
    function GetEditable(Field : TDataField) : Boolean;
    procedure AddRowsToList(AList : TStrings; RowStatus : TRowStatus); virtual; abstract;
  public
    FData : PChar;

    procedure ReplaceWithDetails(RowList : TStrings; var Index : Integer); virtual; abstract;
    function IsClosable : Boolean;

    function GetFieldValue(Field : TDataField) : TValue;
{$ifdef COMMONLASKU}
    function GetFValue(Field : TDataField) : TValue; //MAB 23.8.02
    function GetFValueVirt(Field : TDataField) : TValue; virtual; //MAB 23.8.02
{$endif COMMONLASKU}
    {/** Default displaytext for the given field */}
    property DisplayText[AField : TDataField] : String read GetDisplayText;
    {/** Displaytext for the given field with the given DisplayValues */}
    property DisplayString[AField : TDataField; DisplayValues : TDisplayValues] : String read GetDisplayString;
    {/** Key and description */}
    property KeyAndDescription[AField : TDataField] : String read GetKeyAndDescription;
    {/** The short displaytext for the given field */}
    property ShortDisplayText[AField : TDataField] : String read GetShortDisplayText;
    property ShortDisplayString[AField : TDataField; DisplayValues : TDisplayValues] : String read GetShortDisplayString;

    {/** The owning RowStorage to which the row belongs */}
    property Storage : TAbstractRowStorage read FStorage;
    {/** The Table type of which the row is (regardless if subclassed at creation) */}
    property DataTable : TDataTable read FDataTable;
    {/** Position of field in the internal memory structure; false fields give -1 */}
    function IndexOfField(Field : TDataField) : Integer; virtual;

    property Editable[Field : TDataField] : Boolean read GetEditable;
    function CanEditValue(Field : TDataField; var ReadOnlyReason : String) : Boolean; virtual; abstract;
    {/** Get the value of a field */}
    property Value[Field : TDataField] : TValue read GetFieldValue write SetFValue; default;
    {/** Get the String value of a field */}
    property StringValue[Field : TDataField] : String read GetFieldStringValue write SetFStringValue;
    {/** Get the integer value of a field */}
    property IntValue[Field : TDataField] : Integer read GetFieldIntValue write SetFIntValue;
    {/** Get the float value of a field */}
    property DoubleValue[Field : TDataField] : Double read GetFieldDoubleValue write SetFDoubleValue;
    {/** Get the DateTime value of a field */}
    property DateTimeValue[Field : TDataField] : TDateTime read GetFieldDateTimeValue write SetFDateTimeValue;
    {/** Get the Currency value of a field */}
    property CurrencyValue[Field : TDataField] : Currency read GetFieldCurrencyValue write SetFCurrencyValue;
    {/** Get the Boolean value of a field */}
    property BooleanValue[Field : TDataField] : Boolean read GetFieldBooleanValue write SetFBooleanValue;
    {/** Get the Pointer value of a field */}
    property PointerValue[Field : TDataField] : Pointer read GetFieldPointerValue write SetFPointerValue;

    property SQLValue[Field : TDataField] : String read GetFieldSQLValue;
    property SQLByIndex[idx : Integer] : String read GetSQLIndexValue;

    {/** Get the String value of a field index */}
    property ValueByIndex[idx : Integer] : TValue read FetchValue write SetIValue;

    {/** Set value of a field */}
    function SetFieldValue(Field : TDataField; Value : TValue; Action : TSetAction) : TSetResult;
    {/** Set string value of a field */}
    function SetFieldStringValue(Field : TDataField; Value : String; Action : TSetAction) : TSetResult;
    {/** Set int value of a field */}
    function SetFieldIntValue(Field : TDataField; Value : Integer; Action : TSetAction) : TSetResult;
    {/** Set double value of a field */}
    function SetFieldDoubleValue(Field : TDataField; Value : Double; Action : TSetAction) : TSetResult;
    {/** Set datetime value of a field */}
    function SetFieldDateTimeValue(Field : TDataField; Value : TDateTime; Action : TSetAction) : TSetResult;
    {/** Set currency value of a field */}
    function SetFieldCurrencyValue(Field : TDataField; Value : Currency; Action : TSetAction) : TSetResult;
    {/** Set boolean value of a field */}
    function SetFieldBooleanValue(Field : TDataField; Value : Boolean; Action : TSetAction) : TSetResult;
    {/** Set pointer value of a field */}
    function SetFieldPointerValue(Field : TDataField; Value : Pointer; Action : TSetAction) : TSetResult;

    function IndexEqual(idx : Integer; CmpRow : TAbstractRow) : Boolean;
    function Compare(ARow : TAbstractRow; Order : TRowSortOrder) : Integer;
    function CompareToValues(Values : TValueList; Order : TRowSortOrder) : Integer;
    {/** Is this row (or any of it's subrows changed */}
    function ContainsChanges : Boolean; virtual; abstract;
    property ValidKeyValue[idx : Integer] : TValue read GetValidKeyValue;
    property ValidKey[idx : Integer] : TDataField read GetValidKey;
    {/** KeyCount for DataRows and TreeKeyIndex for SubTotalRows */}
    property ValidKeyCount : Integer read GetValidKeyCount;
    function KeyFieldValues(ATable:TDataTable=nil) : String;
    function AllFieldValues(ATable:TDataTable=nil) : String;
//    function KeyFieldValuesOnly: String;
    property Visible : Boolean read FVisible write SetVisible;
    function BuildingBlocksVisible : Boolean; virtual; abstract;
    {/** This rows parent, also containing subtotal */}
    property SubTotalRow : TSubTotalRow read FSubTotalRow;
    function FieldHasValue(Field : TDataField) : Boolean; virtual; abstract;
    {/** Destructor */}
    destructor Destroy; override;
  end;

  TSubTotalRow = class(TAbstractRow)
  private
    FUnacceptedRowExternValues : TValueList;
    __UnacceptedRowExternSkipField : TDataField;
    FSubTotalKey : TRowStorageTreeKey;
    FSubTotalsUptodate : Boolean;
//    function GetNextNegativeRunningNumber : Integer;

    {/** GetRows helping functions */}
    function __CheckSortingNeeded(DefaultSort : Boolean) : Boolean;
    procedure __AddToSortList(Matches : TStrings; ARow : TAbstractRow);
    procedure __DirectAdd(Handler : TGetRowsHandler; ARow : TAbstractRow); // (ARow : TAbstractRow; Results : TStrings; Condition : TCondition; Action : TGetAction; ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean; DefaultSort : Boolean);
    procedure __AddRow(Handler : TGetRowsHandler; Matches : TStrings; ARow : TAbstractRow; SortingNeeded : Boolean);
    procedure __AddRowCheckDetails(Handler : TGetRowsHandler; Matches : TStrings; ARow : TAbstractRow; SortingNeeded : Boolean);
  protected
    procedure __AddValue(Handler : TGetRowsHandler; AValue : TValue);
  private
    procedure __SortAndAdd(Handler : TGetRowsHandler; Matches : TStrings);
    procedure __AddIfLegal(Handler : TGetRowsHandler; Matches : TStrings; ARow : TAbstractRow; SortingNeeded : Boolean);
    procedure __RemoveEmptyChilds;
    function GetSubRow(idx : Integer) : TAbstractRow;
    function AddSubTotalRow(ARow : TSubtotalRow) : TPutResult;
    procedure UpdateUnacceptedRowExternValues;
    function IndexOfSubRow(ARow : TAbstractRow) : Integer;
    function PutRowConflict(Row : TDataRow; idx : Integer; Action : TPutAction; Force : Boolean) : TPutResult;
  protected
    FSubRows : TValueList;

    function AddChildRow(KeyValue : TValue; Row : TAbstractRow) : Integer;
    function InternalPutRow(Row : TDataRow; Action : TPutAction) : TPutResult;
    function FindValue(Value : TValue; var idx : Integer) : Boolean;
    function ContainsSubRows : Boolean; override;

    function IsUpdating : Boolean; override;
    {/** Internal constructor; subtotalrows can only be created by RowStorages */}
    constructor Create(Storage : TAbstractRowStorage; DirectOwner : TSubTotalRow; OwnKeyValue : TValue; Visible : Boolean);
    {/** Get rows into a Strings */}
    procedure GetRows(Handler : TGetRowsHandler); // (Results : TStrings; Condition : TCondition; Action : TGetAction; ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean; DefaultSort : Boolean);
    {/** Get rows to a TStrings; This replaces GetByConstBegin */}
    function ForcePutRow(Row : TDataRow; PutAction : TPutAction) : TPutResult;
//    procedure GetRowsByKeyBegin(Results : TStrings; Criteria : TCondition; Action : TGetAction; FieldIndex : Integer; BeginWith : String; ExcludeOnSubTotalLevel, LastLevelCheckAllFields, DefaultSort : Boolean);
    {/** Make up internal list of tree */}
    procedure FillRowList(ARowList : TStrings; ExtraCondition : TCondition; DefaultSort : Boolean); override;
    {/** Delete a row and take actions needed */}
    procedure DeleteRow(ARow : TAbstractRow; RecursiveDelete : Boolean);
    {/** Let a subtotalrow know that it's subtotals aren't uptodate */}
    procedure SubTotalsNotUptodate; //(AddedValues, RemovedValues : TDataRow);
    {/** Update (calculate) subtotals */}
    procedure UpdateSubTotals;
    {/** Does the storage contain these keys */}
    function KeysExist(Keys : array of String; Index : Integer) : Boolean;
    function SetIndexValue(idx : Integer; Value : TValue; Action : TSetAction) : TSetResult; override;
    {/** Does the storage contain these keys */}
    function HasRowsThatMatch(Handler : TGetRowsHandler) : Boolean; // (Condition : TCondition; ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean) : Boolean;
    {/** Find a row; idx is the next index to look at */}
    function LocateRow(Keys : array of String; Index : Integer) : TDataRow;
    {/** Find a row */}
    function LocateByRowValues(ValueRow : TAbstractRow; DerivedKeys : array of TDataField) : TDataRow;
    function SumValues(Field : TDataField {TCalcField}) : TValue;
    function IsTreeKeyOnly(AField : TDataField; var KeyValue : TValue) : Boolean;
    {/** Nr of rows with this status */}
    function GetStatusCount(Status : TRowStatus) : Integer; override;
    function GetFirstRow : TDataRow;
    function GetValidKeyCount : Integer; override;
    procedure ClearAll;
    {/* Remove Row from Storage; no extra actions */}
    function ForceRemoveRow(ARow : TDataRow) : Boolean;
    procedure UpdateRowVisibility(Level : Integer; Show : Boolean);
    procedure SetChildVisibility(Vis, Recursive : Boolean);
//    function SaveInterrupt(UpdateQuery, InsertQuery, MaxNoQuery : TDataQuery; RunningNumberIndex : Integer;
//                            Interruptable : TInterruptable; AffectedFields : TDataFieldSet; ProcModifiedRow : TProcRow) : Integer; override;


    // MVJ 10.1.2000: I need these LGE...
//    property SubRows[idx : Integer] : TAbstractRow read GetSubRow;
    function GetSubRowCount : Integer;
    procedure BeforeFetchValue(Field : TDataField); override;
    procedure AddRowsToList(AList : TStrings; RowStatus : TRowStatus); override;

    property SubTotalsUptodate : Boolean read FSubTotalsUptodate;
  public
    procedure ReplaceWithDetails(RowList : TStrings; var Index : Integer); override;
    property SubRows[idx : Integer] : TAbstractRow read GetSubRow;  // Hmm.. these should not be public...
    property SubRowCount : Integer read GetSubRowCount;             // Hmm.. these should not be public...

    {/** TreeKey for this subtotallevel  */}
    property SubTotalKey : TRowStorageTreeKey read FSubTotalKey;
    {/** Index of field in this subtotalrow. Note that keys lower than KeyIndex "do not exist"! */}
    function IndexOfField(Field : TDataField) : Integer; override;
    {/** Is this the last treenode level? */}
    function IsLastTreeNode : Boolean;
    {/** Is any of this nodes' rows changed */}
    function ContainsChanges : Boolean; override;
    function BuildingBlocksVisible : Boolean; override;
    function FieldHasValue(Field : TDataField) : Boolean; override;
    function CanEditValue(Field : TDataField; var ReadOnlyReason : String) : Boolean; override;
    {/** Destructor should only be used by SubTotalRows and RowStorages */}
    destructor Destroy; override;
  end;

  TDataRow = class(TAbstractRow)
  private
    FUpdateDepth : Integer;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure KeysChanged;
    function ConditionAcceptsUpdatedValue(Condition : TCondition; fieldidx : Integer; Value : TValue) : Boolean;
  protected
    FStatus : TRowStatus;

    function ContainsSubRows : Boolean; override;

    function IsUpdating : Boolean; override;
    {/** Make a default datarow for this DataTable */}
    constructor CreateDefault(DataTable : TDataTable);

    {/** SQLString of the keys of this row */}
    function KeySQLString : String;
    {/** Save this row (expects two prepared queries */}
//    function Save(UpdateQuery, InsertQuery, MaxNoQuery : TDataQuery; RunningNumberIndex : Integer; Fields : TDataFieldSet; var Saved : Boolean) : Integer; virtual;
    {/** Add this row to a RowList */}
    procedure FillRowList(ARowList : TStrings; ExtraCondition : TCondition; DefaultSort : Boolean); override;
    {/** Nr of rows with this status */}
    function GetStatusCount(Status : TRowStatus) : Integer; override;
    {/** LoadGrouped */}
//    procedure LoadJoinedGrouped(Query : TDataQuery; GroupedFieldPrefix : String; FieldNames : TStringList; DbDataTable : TDataTable);
    {/** Valid nr of Keys */}
    function GetValidKeyCount : Integer; override;
    {/** Does this Row contain new (value <= 0) running numbers? */}
    function ContainsNewRunningNumbers : Boolean;
    function SetIndexValue(idx : Integer; Value : TValue; Action : TSetAction) : TSetResult; override;
    {/** Internal deletefunction */}
    procedure DeleteSelf(AllowTreeDelete : Boolean); virtual;
//    function SaveInterrupt(UpdateQuery, InsertQuery, MaxNoQuery : TDataQuery; RunningNumberIndex : Integer;
//                            Interruptable : TInterruptable; AffectedFields : TDataFieldSet; ProcModifiedRow : TProcRow) : Integer; override;

    procedure AddRowsToList(AList : TStrings; RowStatus : TRowStatus); override;
  public

    procedure ReplaceWithDetails(RowList : TStrings; var Index : Integer); override;
    {/** Constructor; need to know it's DataTable */}
    constructor Create(DataTable : TDataTable);
    constructor CreateFromByteData(DataTable : TDataTable; Data : Pointer; var Position : Integer);
    {/** Copy the contents of SrcRow (DataTable must be the same) */}
    procedure CopyContents(SrcRow : TDataRow); virtual;
    {/** Copy same and dependent keyfields (DataTables may differ); Row's status *must* be rsExternControlled! */}
    procedure SetDefaultsFrom(DefaultRow : TAbstractRow);
    {/** Set values to a row directly from a row in a (opened) Query */}
    {/** Destructor */}
    destructor Destroy; override;
    {/** Delete (not free) the row from the Storage */}
    procedure Delete;
    {/** Set param values to a query and Execute */}


    {/** Set value of a field index */}
    procedure WriteDataToStream(AStream : TStream);
    {/** Add (aggregable) values from another row, exact behaviour decided by AddAction */}
    procedure AddValues(SourceRow : TDataRow; AddAction : TAddAction);
    {/** Clear the values for all non key fields */}
    procedure ClearValues;
    {/** Copy value of field idx to this row */}
//    procedure CopyIndexValue(idx : Integer; SourceRow : TAbstractRow);
    {/** Add value of field idx to this row. Requires field to be aggregable */}
//    procedure AddIndexValue(idx : Integer; SourceRow : TAbstractRow);
    {/** Copy a fieldvalue from on other row (may have other datatable) */}
//    procedure SetValueFrom(DataField : TDataField; SrcRow : TAbstractRow);
    {/** Copy a fieldvalue from on other row and field (may have other datatable) */}
//    procedure SetValueFromDifferentField(DataField : TDataField; SrcRow : TAbstractRow; SrcField : TDataField);

    function CanEditValue(Field : TDataField; var ReadOnlyReason : String) : Boolean; override;
    function CreateCopy : TDataRow; virtual;
    procedure CopyFromCompatible(ACompatibleRow : TDataRow);
    {/** Is this row changed since the last save */}
    function ContainsChanges : Boolean; override;

    procedure NegateValue(Field : TDataField);
    procedure NegateByIndex(idx : Integer);

    function KeysEqual(ARow : TDataRow) : Boolean;
    function BuildingBlocksVisible : Boolean; override;

    {/** Update status of this datarow */}
    property Status : TRowStatus read FStatus;
    {/** Get Next free running number for this row */}
//    function NextRunningNumber(Query : TDataQuery; FieldSet : TDataFieldSet) : Integer;
    {/** Do not use! */}
    procedure UpdateRunningNumber(Field : TRunningNumberField; NewValue : TValue);
    {/** Get we get this fields value for this row? */}
    function FieldHasValue(Field : TDataField) : Boolean; override;
  end;

  TCallbackCommand = class
  private
    procedure Execute; virtual; abstract;
  end;

  TCallbackCommandRemoveRow = class(TCallbackCommand)
  private
    FRow : TAbstractRow;

    procedure Execute; override;
    constructor Create(Row : TAbstractRow);
  public
    destructor Destroy; override;
  end;

  TCallbackCommandRestoreValue = class(TCallbackCommand)
  private
    FRow : TAbstractRow;
    FDataField : TDataField;
    FOldValue : TValue;

    procedure Execute; override;
    constructor Create(Row : TAbstractRow; DataField : TDataField; OldValue : TValue);
  public
    destructor Destroy; override;
  end;

  TSubTotalHandlerMark = class
  private
    FStorage : TAbstractRowStorage;
    FDataField : TDataField;
    FCallbackCommands : TList;
    FSubTotalRows : TList;
    function GetSubTotalRow : TSubTotalRow;

    property Storage : TAbstractRowStorage read FStorage;
    property DataField : TDataField read FDataField;
    property SubTotalRow : TSubTotalRow read GetSubTotalRow;
    constructor Create(Storage : TAbstractRowStorage; DataField : TDataField);
    procedure Push(SubTotalRow : TSubTotalRow);
    function Pop : TSubTotalRow;
    procedure AddCallbackCommand(Cmd : TCallbackCommand);
  public
    destructor Destroy; override;
    procedure Callback;
  end;

  TSubTotalHandler = class
  private
    FMarks : TList;
    FCallbackOnException : Boolean;
    function InternalDistributeValue(Sender : TAbstractRowStorage; SubTotalRow : TSubTotalRow;
                     Field : TDataField; Value : TValue; Action : TSetAction) : TSetResult;
    procedure InternalUpdateSubTotals(Sender : TAbstractRowStorage; SubTotalRow : TSubTotalRow;
                     StoreValue : TStoreValueMethod);
    function InternalCanEditValue(Sender : TAbstractRowStorage; SubTotalRow : TSubTotalRow;
                            DataField : TDataField; var ReadOnlyReason : String) : Boolean;
    function CanEditValue(Sender : TAbstractRowStorage; SubTotalRow : TSubTotalRow;
                            DataField : TDataField; var ReadOnlyReason : String) : Boolean;
    procedure SetMark(Storage : TAbstractRowStorage; SubTotalRow : TSubTotalRow;
                      DataField : TDataField);
    procedure UnSetMark;
    function ActiveMark : TSubTotalHandlerMark;

    function GetChildRowCount : Integer;
    function GetChildRow(idx : Integer) : TAbstractRow;
    function GetChildRowByKey(KeyValue : TValue) : TAbstractRow;

    function GetStorage : TAbstractRowStorage;
    function GetDataField : TDataField;
    function GetSubTotalRow : TSubTotalRow;
    function GetNextKeyLevel : TDataField;

    property Storage : TAbstractRowStorage read GetStorage;
    property DataField : TDataField read GetDataField;
    property SubTotalRow : TSubTotalRow read GetSubTotalRow;
  protected
    property CallbackOnException : Boolean read FCallbackOnException;
    property NextKeyLevel : TDataField read GetNextKeyLevel;

    function LevelEditable(Sender : TAbstractRowStorage; SubTotalLevel : Integer;
                     var ReadOnlyReason : String) : Boolean; virtual;
    function SubTotalEditable(Sender : TAbstractRowStorage; SubTotalRow : TSubTotalRow;
                     Field : TDataField; var ReadOnlyReason : String) : Boolean; virtual;

    procedure BeforeDistributeValue(Sender : TAbstractRowStorage; SubTotalRow : TSubTotalRow;
                     DataField : TDataField; var Value : TValue); virtual;
    procedure DistributeValue(Sender : TAbstractRowStorage; SubTotalRow : TSubTotalRow;
                     DataField : TDataField; Value : TValue); virtual;

    function SetValue(KeyValue : TValue; NewValue : TValue) : TAbstractRow;
//    procedure SetValue(KeyValue : TValue; NewValue : TValue);
    function IncreaseValue(KeyValue : TValue; IncreaseBy : TValue) : TAbstractRow;
//    procedure IncreaseValue(KeyValue : TValue; IncreaseBy : TValue);

    property ChildRowCount : Integer read GetChildRowCount;
    property ChildRow[idx : Integer] : TAbstractRow read GetChildRow;
    property ChildRowByKey[KeyValue : TValue] : TAbstractRow read GetChildRowByKey;

    procedure UpdateSubTotals(Sender : TAbstractRowStorage; SubTotalRow : TSubTotalRow;
                              StoreValue : TStoreValueMethod); virtual;

    constructor Create(CallbackOnException : Boolean);
  public
    destructor Destroy; override;
  end;

  TDefaultSubTotalHandler = class(TSubTotalHandler)
  private
  protected
    function LevelEditable(Sender : TAbstractRowStorage; SubTotalLevel : Integer;
                     var ReadOnlyReason : String) : Boolean; override;
  public
    constructor Create;
  end;

  TOperationStartedEvent = procedure(Sender : TRowSource) of object;
  TRowIntervalEvent = procedure(Sender : TRowSource; RowsPassed : Integer) of object;
  TLoadIntervalEvent = procedure(Sender : TRowSource; RowsLoaded : Integer; Finished : Boolean) of object;
  TOperationFinishedEvent = procedure(Sender : TRowSource; TotalRows : Integer) of object;

  TRowSource = class
  private
    FGotRows : Integer;
    FInterruptable : TInterruptable;
    FOperationType : TOperationType;

    FDestinations : TList;

    procedure AddDestination(ADest : TObject);
    procedure RemoveDestination(ADest : TObject);
  protected
    FDataTable : TDataTable;

    function GetDataTable : TDataTable;


    procedure DoStartOperations(ARowSource : TRowSource);
    function DoGetNextRow(ARowSource : TRowSource) : TDataRow;
    procedure DoFinishOperations(ARowSource : TRowSource);

    procedure StartOperations; virtual; abstract;
    function GetNextRow : TDataRow; virtual; abstract;
    procedure FinishOperations;
    procedure FreeRowMemory; virtual;

    constructor Create(DataTable : TDataTable; OperationType : TOperationType);
    function GetSortOrder : TRowSortOrder; virtual;
    function GetIsAbsoluteSource : Boolean; virtual; abstract;

    procedure GotRow(Row : TDataRow; RowCount : Integer);
    property ProcessedRowCount : Integer read FGotRows;
    function GetProperties : String; virtual;
  public
    {/** The datatable of this storage/operation */}
    property DataTable : TDataTable read GetDataTable;
    property RowOrder : TRowSortOrder read GetSortOrder;

    property Interruptable : TInterruptable read FInterruptable write FInterruptable;
    property OperationType : TOperationType read FOperationType;

    property IsAbsoluteSource : Boolean read GetIsAbsoluteSource;

    function AddToLog(Components, Log : TStrings; Indentation : Integer) : Boolean; virtual;
    property Properties : String read GetProperties;
    destructor Destroy; override;
  end;

  {/** ChangedConflictRows: Rows that has been deleted from the db
       ChangedKeepList:     Rows that we want to keep though they were deleted
       NewConflictRows:  Rows that already exists in the db
       NewAggregateList: Rows whose values we want to aggregate
       NewReplaceList:   Rows that we want to replace */}
  TConflictRowCallback = procedure(Sender : TAbstractRowStorage; ChangedConflictRows, ChangedKeepList,
     NewConflictRows, NewAggregateList, NewReplaceList : TStrings) of object;

  {/** ConflictRows: Rows that has been deleted from the db
       KeepList:     Rows that we want to keep though they were deleted */}
//  TChangedRowCallback = procedure(Sender : TAbstractRowStorage; ConflictRows, KeepList : TDataRowList) of object;

  {/** ConflictRows:  Rows that already exists in the db
       AggregateList: Rows whose values we want to aggregate
       ReplaceList:   Rows that we want to replace */}
//  TNewRowCallback = procedure(Sender : TAbstractRowStorage; ConflictRows, AggregateList, ReplaceList : TDataRowList) of object;

  TAbstractRowStorage = class(TRowSource)
  private
    FMonitors : TList;
    {/** Storage for RowSource Rows */}
    FRowSourceRows : TStrings;
    {/** Current row when being RowSource */}
    FCurrentRow : Integer;

    // FExistsInDb : Boolean;

    // FSortOrder : TRowSortOrder;
    FCustomSortOrder : TRowSortOrder;
    FUsesCustomSortOrder : Boolean;
    FOnSaved : TStorageNotifyEvent;

    FDeletedRows : TList;
    FTree : TList;
    FUnacceptedRows : TList;
    FTreeKeys : TRowSortOrder;

    FSubTotalHandler : TSubTotalHandler;

    FAutoUpdateEnabled : Boolean;

    function GetTreeKey(idx : Integer) : TRowStorageTreeKey;
    function GetTreeKeyCount : Integer;
    function GetTreeKeyByField(Field : TDataField) : TRowStorageTreeKey;
    function GetDetailTreeKey : TRowStorageTreeKey;
    procedure CreatePrepare(DataTable : TDataTable);
    // procedure CreateFinish;
    function GetUnacceptedRow(idx : Integer) : TAbstractRow;
    function GetUnacceptedRowCount : Integer;
    function GetInternalSubTotalHandler : TSubTotalHandler;
    function SkakaUnacceptedIndex(Field : TDataField) : Integer;

    procedure SetTreeKeys(TreeKeys : TRowSortOrder);
  protected
    FLastChanged : TDateTime;
    FTotal : TSubTotalRow;
    FRowList : TStrings;
    FRowListUpToDate : Boolean;

    ISubTotalsUnder : Boolean;

    function CanDefaultSort : Boolean;
    procedure CopyFromSource(ASource : TAbstractRowStorage); virtual;
    function GetTotal : TSubTotalRow; virtual;
    function GetStatusCount(Status : TRowStatus) : Integer; virtual;
    function AllowUpdateTreeKeys : Boolean; virtual;

    {/** Table/Storage total: Sumrow of all rows */}
    property Total : TSubTotalRow read GetTotal;

    function GetAutoCalcSubTotals : Boolean; virtual; abstract;
    procedure SetAutoCalcSubTotals(AutoCalc : Boolean); virtual; abstract;
    function GetSortOrder : TRowSortOrder; override;
    function ForcePutRow(ARow : TDataRow; PutAction : TPutAction) : TPutResult; virtual;
    procedure MapLargestValues(ResultRow : TDataRow; SrcRow : TAbstractRow);
    procedure CalcTreeFieldPositions;


    {/** RowSource methods */}
    procedure StartOperations; override;
    function GetNextRow : TDataRow; override;
    {/** Load from file */}
    {/** Remove a row from Storage "nicely" (no not put it in del-list etc) */}
    procedure ForceRemoveRow(ARow : TDataRow); virtual;
    {/** Changed */}
    procedure Changed(ChangeType : TChangeType; Row : TAbstractRow; Index : Integer); virtual;


    function GetCanHaveTotals : Boolean; virtual; abstract;

    procedure SetSubTotalsUnder(Under : Boolean); virtual;
    procedure SetShowSubTotals(Show : Boolean); virtual;

//    function GetShowSubTotal(idx : Integer) : Boolean; virtual;
//    procedure SetShowSubTotal(idx : Integer; Show : Boolean); virtual;
//    function GetShowFieldSubTotal(Field : TDataField) : Boolean; virtual;
//    procedure SetShowFieldSubTotal(Field : TDataField; Show : Boolean); virtual;
    procedure ClearAllRowLists;
    function GetIsAbsoluteSource : Boolean; override;
    function CopyOfTreeOrder(Storage : TAbstractRowStorage) : TList;

    procedure BeforeGetDataByCrit(Condition : TCondition); virtual;
    procedure BeforeGetDataByArray(GetValues : array of String); virtual;
    procedure BeforeGetDataByRow(Row : TAbstractRow; DerivedKeys : array of TDataField); virtual;
//    procedure DoBeforeGetData(GetCriteria : TCriteria);



    procedure SetCustomSortOrder(ARowSortOrder : TRowSortOrder);
    procedure SetUsesCustomSortOrder(Value : Boolean);
//    procedure AfterSortOrderCreate(SortOrder : TRowSortOrder; Tree : TList); virtual;
    function CreateSubTotal(DirectOwner : TSubTotalRow; OwnKeyValue : TValue; Visible : Boolean) : TSubTotalRow; virtual;

    property InternalSubTotalHandler : TSubTotalHandler read GetInternalSubTotalHandler;
    property AutoUpdateEnabled : Boolean read FAutoUpdateEnabled write FAutoUpdateEnabled;

    {/** Constructor */}
    constructor Create(DataTable : TDataTable);

    // Fixa LGE byt namn på den övre...
//    constructor CreateKeySortOrder(DataTable : TDataTable; KeysEditable : Boolean; TreeKeysAndOrder : TList);
//    constructor CreateWithKeySortOrder(DataTable : TDataTable; KeysEditable : Boolean; KeySortOrder : TRowSortOrder);
  public

    procedure AddRowsToList(AList : TStrings; RowStatus : TRowStatus); virtual;
    {/** The internal building structure of the tree */}
    property TreeKey[idx : Integer] : TRowStorageTreeKey read GetTreeKey;
    {/** Number of keys in the building tree */}
    property TreeKeyCount : Integer read GetTreeKeyCount;
    {/** Number of keys in the building tree */}
    property TreeKeyByField[Field : TDataField] : TRowStorageTreeKey read GetTreeKeyByField;
    property DetailTreeKey : TRowStorageTreeKey read GetDetailTreeKey;

    {/** Destructor */}
    destructor Destroy; override;

    {/** Get rows to a TStrings; Action should be gaReference in every other case than from the Explorer */}
    procedure GetRows(Results : TStrings; Condition : TCondition; Action : TGetAction); virtual;
    {/** Get rows to a TStrings; This replaces GetByConstBegin */}
    procedure GetRowsByKeyBegin(Results : TStrings; Criteria : TCondition; Action : TGetAction; BeginField : TDataField; BeginWith : String); virtual;
    function GetMatchCount(Cond : TCondition) : Integer;
    function SumValues(Cond : TCondition; AField : TDataField) : TValue;

    property AutoCalcSubTotals : Boolean read GetAutoCalcSubTotals write SetAutoCalcSubTotals;
    {/** Are subtotals viable amongst the rows */}
    property ShowSubTotals : Boolean {read IShowSubTotals} write SetShowSubTotals;
    {/** If visible, shall the subtotals be under the actual rows */}
    property SubTotalsUnder : Boolean read ISubTotalsUnder write SetSubTotalsUnder;

    property TreeKeys : TRowSortOrder read FTreeKeys write SetTreeKeys; // Note: Assign new to change. Storage must be empty!

//    property ShowSubTotal[idx : Integer] : Boolean write SetShowSubTotal;
//    property DefaultVisibility[idx : Integer] : Boolean read GetShowSubTotal;
//    property ShowFieldSubTotal[Field : TDataField] : Boolean write SetShowFieldSubTotal;
//    property DefaultFieldVisibility[Field : TDataField] : Boolean read GetShowFieldSubTotal;

    property OnSaved : TStorageNotifyEvent read FOnSaved write FOnSaved;

    {/** Add a row new row to the storage */}
    function PutRow(Row : TDataRow; Action : TPutAction) : TPutResult; virtual; abstract;
    {/** Deletes rows that match the Criteria */}
    procedure DeleteRows(Condition : TCondition);
    procedure DeleteAll;

    {/** Get a reference to a row by its keys. Note that the length of the array must be the same or larger than the number of keys in DataTable */}
    function LocateRow(Keys : array of String) : TDataRow; virtual;
    {/** Is there any rows with this "criteria"? */}
    function KeysExist(Keys : array of String) : Boolean; virtual;
    {/** Is there any rows with this criteria? */}
    function HasRowsThatMatch(Condition : TCondition) : Boolean; virtual;
    {/** Locate a Row from this Storage by the arg.row's values */}
    function LocateByRowValues(ValueRow : TAbstractRow; DerivedKeys : array of TDataField) : TDataRow; virtual;

{$ifdef D4_OR_HIGHER}
    function LocateRowByCriteria(Crit : TCondition; UseFirstRowOnMultipleHits : Boolean = False) : TDataRow;
{$else}
    function LocateRowByCriteria(Crit : TCondition; UseFirstRowOnMultipleHits : Boolean) : TDataRow;
{$endif D4_OR_HIGHER}
    {/** Save a storage in Polycon RowStorage format (.prs) */}

    {/** Does this RowStorage exist in the database (cann it be loaded/saved/etc) */}
    // property ExistsInDb : Boolean read FExistsInDb;
    {/** Is there any changes in the Storage's contents */}
    function ContainsChanges : Boolean; virtual;
    {/** The number of Rows in this Storage with the given status */}
    property StatusCount[Status : TRowStatus] : Integer read GetStatusCount;
    {/** Is the row in this RowStorage */}
    function ContainsRow(ARow : TDataRow) : Boolean; virtual;
    {/** Is the row in the the UnacceptedList */}
    function RowIsUnaccepted(ARow : TAbstractRow) : Boolean;
    {/** Number of unaccepted rows */}
    property UnacceptedRowCount : Integer read GetUnacceptedRowCount;
    {/** Unaccepted row by index */}
    property UnAcceptedRows[idx : Integer] : TAbstractRow read GetUnacceptedRow;
    {/** Add a new row with illegal key values */}
    procedure NewUnacceptedRow(Index : Integer; Row : TDataRow);
    function NewUnacceptedSubTotal(Index : Integer; SubTotalKey : TRowStorageTreeKey) : TSubTotalRow;
    {/** Accept a row that has been in the unaccepted list */}
    function AcceptRow(Row : TAbstractRow) : TPutResult;
    {/** Get the first row from this storage */}
    function FirstRow : TDataRow; virtual; abstract;
    {/** Can this storage type have subtotals */}
    property CanHaveTotals : Boolean read GetCanHaveTotals;
    {/** This function is used when the RowStorages creates a new row (eg. load from db). Kan be overrided */}
    function CreateNewRow(QueryValueFunction : TQueryValueFunction) : TDataRow; virtual;
    property CustomSortOrder : TRowSortOrder read FCustomSortOrder write SetCustomSortOrder;
    property UsesCustomSortOrder : Boolean read FUsesCustomSortOrder write SetUsesCustomSortOrder;
    property LastChanged : TDateTime read FLastChanged;
    property SubTotalHandler : TSubTotalHandler read FSubTotalHandler write FSubTotalHandler;
  end;

  TCachingStorage = class(TAbstractRowStorage)
  private
    fLoadedRows : TCommonQuilt;
    fBeforeGetData : TStorageNotifyEvent;
    FLoadPolicy : TCacheLoadPolicy;
    FAutoCalcSubTotals : Boolean;
    FCachingNonKeys : TDataFieldSet;

    procedure CachingLoadRows(Quilt : TCommonQuilt);
    function GetReadOnly : Boolean;
  protected
    function GetCanHaveTotals : Boolean; override;
    {/** Load rows */}
    function GetAutoCalcSubTotals : Boolean; override;
    procedure SetAutoCalcSubTotals(AutoCalc : Boolean); override;

    procedure BeforeGetDataByCrit(Condition : TCondition); override;
    procedure BeforeGetDataByArray(GetValues : array of String); override;
    procedure BeforeGetDataByRow(Row : TAbstractRow; DerivedKeys : array of TDataField); override;

    property LoadedRows : TCommonQuilt read fLoadedRows;
  public
    {/** Constructor */}
{$ifdef D4_OR_HIGHER}
    constructor Create(DataTable : TDataTable; LoadPolicy : TCacheLoadPolicy; CachingNonKeys : TDataFieldSet = nil);
{$else}
    constructor Create(DataTable : TDataTable; LoadPolicy : TCacheLoadPolicy; CachingNonKeys : TDataFieldSet);
{$endif D4_OR_HIGHER}
//    constructor CreateWithKeyOrder(DataTable : TDataTable; KeyOrder : TRowSortOrder; LoadPolicy : TCacheLoadPolicy; CachingNonKeys : TDataFieldSet = nil);

    {/** Constructor for in-memory AuxTableCaches */}
    constructor CreateVirtual(DataTable : TDataTable);
    {/** Adds a row to a virtual AuxTableCache */}
    function AddVirtualRow(Row : TDataRow) : Boolean;
    {/** Destructor */}
    destructor Destroy; override;
    {/** Put new row to cache */}
    function PutRow(Row : TDataRow; Action : TPutAction) : TPutResult; override;


    {/** Cancel all edits to table */}
    procedure DiscardChanges;
    {/** Is This a normal AuxTableCache or is it just a virtual one */}
    property ReadOnly : Boolean read GetReadOnly;
    property CachingNonKeys : TDataFieldSet read FCachingNonKeys;

    {/** Get the first row from this storage */}
    function FirstRow : TDataRow; override;

    procedure GetLargestValues(ResultRow : TDataRow; Condition : TCondition);

    property LoadPolicy : TCacheLoadPolicy read FLoadPolicy write FLoadPolicy;
    property BeforeGetData : TStorageNotifyEvent read FBeforeGetData write FBeforeGetData;
  end;

  TAuxTableCache = class(TCachingStorage)
  end;

  TGetRowsHandler = class
  private
    FOwningCondition, FDefaultSort, FAllowExcludeOnSubTotalLevel, FLastLevelNeedCheckAllFields : Boolean;

    FDataTable : TDataTable;
    FCondition : TCondition;

    FResultList : TStrings;
    FGetAction : TGetAction;

{$ifdef D4_OR_HIGHER}
    constructor Create(Storage : TAbstractRowStorage; Condition : TCondition;
                       ResultList : TStrings = nil; GetAction : TGetAction = gaDelete);
{$else}
    constructor Create(Storage : TAbstractRowStorage; Condition : TCondition;
                       ResultList : TStrings; GetAction : TGetAction);
{$endif D4_OR_HIGHER}
    function DoLastLevelNeedCheckAllFields : Boolean;
  public
    property DefaultSort : Boolean read FDefaultSort;
    property AllowExcludeOnSubTotalLevel : Boolean read FAllowExcludeOnSubTotalLevel;
    property LastLevelNeedCheckAllFields : Boolean read FLastLevelNeedCheckAllFields;

    property DataTable : TDataTable read FDataTable;
    property Condition : TCondition read FCondition;

    property ResultList : TStrings read FResultList;
    property GetAction : TGetAction read FGetAction;

    destructor Destroy; override;
  end;

  TConditionClass = class of TCondition;

  TCondition = class(TPersistent)
  private
    FBeginUpdateCount : Integer;
    FOnChange : TNotifyEvent;
    FChanged : Boolean;
  protected
    FOwner : TCondition;

    procedure Changed;
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure InternalGetRows(Handler : TGetRowsHandler; SubTotal : TSubTotalRow); virtual; // (SubTotal : TSubTotalRow; Results : TStrings; Action : TGetAction; DefaultSort, ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean); virtual;
    function InternalHasRowsThatMatch(Handler : TGetRowsHandler; SubTotal : TSubTotalRow) : Boolean; virtual; // (SubTotal : TSubTotalRow; ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean) : Boolean; virtual;

    function GetOnlyValue(AField : TDataField) : TValue;
//    function DoCopyValues( FromField, ToField : TDataField; CreateUnion, KeepOld : Boolean ) : TCondition; virtual; abstract;
    procedure WriteName(AStream : TStream);
    procedure WriteNewLine(AStream : TStream; Indent : Integer);
    procedure WriteCloseParenteses(AStream : TStream);
    class function GetNextStreamItem(AStream : TStream; var ConditionClass : TConditionClass) : Boolean;
    class function GetStreamTextTo(AStream : TStream; Separator, ExitFalseAt : Char; var ReadText : String) : Boolean;
    class function SkipToObjectEnd(AStream : TStream) : Boolean;
    function ConcernsField(DataField : TDataField) : Boolean; virtual; abstract;
    function ConcernsTable(DataTable : TDataTable) : Boolean; virtual; abstract;



    procedure SetOwner(AOwner : TCondition);
  public
    procedure DoWriteToStream(AStream : TStream; Indent : Integer); virtual; abstract;
    constructor DoCreateFromStream(AStream : TStream); virtual; abstract;


    function GetLargerCriteria(var FreeCriteria : Boolean) : TCondition {TCriteria}; virtual; abstract; // Fixa LGE dessa försvinner i någå skede
    function GetSmallerCriteria(var FreeCriteria : Boolean) : TCondition {TCriteria}; virtual; abstract; // Fixa LGE dessa försvinner i någå skede
    property Owner : TCondition read FOwner;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property OnlyValue[AField : TDataField] : TValue read GetOnlyValue;
    function IsPureAndCondition : Boolean; virtual;
    constructor Create(Owner : TCondition);
    destructor Destroy; override;

{$ifdef D4_OR_HIGHER}
    function AcceptsRow(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean; virtual; abstract;
    function AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean; virtual; abstract;
{$else}
    function AcceptsRow(ARow : TAbstractRow; AffectedFields : TDataFieldSet) : Boolean; virtual; abstract;
    function AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet) : Boolean; virtual; abstract;
{$endif D4_OR_HIGHER}
    function AcceptsValue(AField : TDataField; AValue : TValue) : Boolean; virtual; abstract;

//    function AcceptsRow(ARow : TAbstractRow) : Boolean; virtual; abstract;
    // function AcceptsRowValue(Field : TDataField; Value : TValue; ARow : TAbstractRow) : Boolean; virtual; abstract;
    function CreateCopy : TCondition; virtual; abstract;
    function CreateFieldTranslatedCopy(TranslateField : TQueryDataFieldFunction) : TCondition; virtual; abstract;
    function Equals(Condition : TCondition) : Boolean; virtual; abstract;
    {/** Checks if the Criteria has any restricitions on the fields in ATable */}
    function AcceptsAllInTable(DataTable : TDataTAble) : Boolean; virtual; abstract;
    function AcceptsNoValuesForTable(DataTable : TDataTable) : Boolean; virtual; abstract;
    function AcceptsAllForField(DataField : TDataField) : Boolean; virtual; abstract;
    function AcceptsNoValuesForField(DataField : TDataField) : Boolean; virtual; abstract;

    function AcceptsExactlyOneValue(DataField : TDataField; var Value : TValue) : Boolean; virtual; abstract;
    procedure ProcessFields(Proc : TProcFieldCond); virtual; abstract;
    procedure WriteToStream(AStream : TStream);
    class function CreateFromStream(AStream : TStream) : TCondition;
//    function CopyValues( FromField, ToField : TDataField; CreateUnion, KeepOld : Boolean ) : TCondition;

    function GetCommonQuilt(out FreeQuilt : Boolean) : TCommonQuilt; virtual; abstract;
    function CreateCommonQuilt : TCommonQuilt;
  end;

  TCommonQuilt = class( TCondition )
  protected
    // Common interface for objects in a Quilt
    constructor Create;

    // Get the set of Fields this CommonQuilt has conditions on
    function GetAffectedFieldSet : TDataFieldSet; virtual; abstract;

{$ifdef D4_OR_HIGHER}
    // Create the union of this object and AQuilt. AQuilt will be destroyed if FreeParam is true
    function DoCreateUnion( AQuilt : TCommonQuilt; FreeParam : Boolean = True ) : TCommonQuilt; virtual; abstract;
    // Create the difference of this object and AQuilt. AQuilt will be destroyed if FreeParam is true
    function DoCreateDifference( AQuilt : TCommonQuilt; FreeParam : Boolean = True ) : TCommonQuilt; virtual; abstract;
    // Create the intersection of this object and AQuilt. AQuilt will be destroyed if FreeParam is true
    function DoCreateIntersection( AQuilt : TCommonQuilt; FreeParam : Boolean = True ) : TCommonQuilt; virtual; abstract;
{$else}
    function DoCreateUnion( AQuilt : TCommonQuilt; FreeParam : Boolean ) : TCommonQuilt; virtual; abstract;
    function DoCreateDifference( AQuilt : TCommonQuilt; FreeParam : Boolean ) : TCommonQuilt; virtual; abstract;
    function DoCreateIntersection( AQuilt : TCommonQuilt; FreeParam : Boolean ) : TCommonQuilt; virtual; abstract;
{$endif D4_OR_HIGHER}

    // Routines inherited from TCondition
    // function DoCopyValues( FromField, ToField : TDataField; CreateUnion, KeepOld : Boolean ) : TCondition; override;
    function ConcernsField(DataField : TDataField) : Boolean; override;
    function ConcernsTable(DataTable : TDataTable) : Boolean; override;
  public
    // Common interface for objects in a Quilt
    destructor Destroy; override;

    function CreateCommonCopy : TCommonQuilt; virtual; abstract;
    // Returns the lowest level of Quilts with equal selections
    function ExtractOptimalQuilt  : TCommonQuilt; virtual; abstract;

{$ifdef D4_OR_HIGHER}
    function Compare(AQuilt : TCommonQuilt; AffectedFields : TDataFieldSet=nil) : TCompareResult; virtual; abstract;
{$else}
    function Compare(AQuilt : TCommonQuilt; AffectedFields : TDataFieldSet) : TCompareResult; virtual; abstract;
{$endif D4_OR_HIGHER}
    function ContainsCommon(AQuilt : TCommonQuilt) : Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure KeepDimensions(AffectedFields : TDataFieldSet); virtual; abstract;

    // Creates the union of all the parameters. The parameter-conditions are not killed
    class function CreateUnion(Conditions : array of TCondition) : TCommonQuilt;
    // Creates the difference of the two parameters. The parameter-conditions are not killed
    class function CreateDifference(Condition1, Condition2 : TCondition) : TCommonQuilt;
    // Creates the intersection of all the parameters. The parameter-conditions are not killed
    class function CreateIntersection(Conditions : array of TCondition) : TCommonQuilt;

    // The set of fields this Quilt has conditions on.
    property AffectedFieldSet : TDataFieldSet read GetAffectedFieldSet;

{$ifdef D4_OR_HIGHER}
    function AcceptsAll( AffectedFields : TDataFieldSet = nil) : Boolean; virtual; abstract;
    function AcceptsNone( AffectedFields : TDataFieldSet = nil) : Boolean; virtual; abstract;
{$else}
    function AcceptsAll( AffectedFields : TDataFieldSet ) : Boolean; virtual; abstract;
    function AcceptsNone( AffectedFields : TDataFieldSet ) : Boolean; virtual; abstract;
{$endif D4_OR_HIGHER}

    // Routines inherited from TCondition
    procedure DoWriteToStream(AStream : TStream; Indent : Integer); override;
    constructor DoCreateFromStream(AStream : TStream); override;

    function GetLargerCriteria(var FreeCriteria : Boolean) : TCondition; override;
    function GetSmallerCriteria(var FreeCriteria : Boolean) : TCondition; override;
  end;

  TAuxTable = class(TDataTable)
  private
    FAuxTableCache : TCachingStorage;
    function GetAuxTableKey : TDataField;
//    procedure ReadIsVirtual(Reader : TReader);
//    procedure WriteIsVirtual(Writer : TWriter);
    procedure SetCacheLoadPolicy(ALoadPolicy : TCacheLoadPolicy);
    function GetCacheLoadPolicy : TCacheLoadPolicy;
    function GetAuxTableCache : TCachingStorage;
  protected
//    procedure BeforeLoad(Criteria : TCriteria); virtual;
    function GetIsVirtual : Boolean; virtual;
    function CreateCache(LoadPolicy : TCacheLoadPolicy; CachingNonKeys : TDataFieldSet = nil) : TCachingStorage; virtual;
//    procedure DefineProperties(Filer : TFiler); override;
  published
    property CacheLoadPolicy : TCacheLoadPolicy read GetCacheLoadPolicy write SetCacheLoadPolicy;
  public
    {/** The auxtablecache of this DataTable */}
    property Cache : TCachingStorage read GetAuxTableCache;
    {/** Constructor, simular to TDataTable's constructor */}
    constructor Create(AOwner: TComponent); override;
    constructor CreateOld(TableName : String; TableType : TDataTableType;
                          Keys : array of TDataField; Fields : array of TDataField;
                          AuxTableField : TDataField;
                          LoadPolicy : TCacheLoadPolicy; DataBridge : TDataBridge; CachingNonKeys : TDataFieldSet = nil);
    constructor CreateCopy(Table : TDataTable; TableType : TDataTableType; DataBridge : TDataBridge); override;

    {/** Destructor */}
    destructor Destroy; override;
    property AuxTableKey : TDataField read GetAuxTableKey;
    property IsVirtual : Boolean read GetIsVirtual;
  end;



  TQueryFieldFunction = function(Field : TDataField; OwnedObjects : TList) : TDataField of object;

  TRowSortOrderRule = class
  private
    FOwner : TRowSortOrder;
    FDataField : TDataField;
    FSortOrder : TSortOrder;
    FFieldIndex : Integer;
    procedure SetDataField(ADataField : TDataField);
  protected
    constructor Create(ADataField : TDataField; ASortOrder : TSortOrder; Owner : TRowSortOrder);
    procedure ApplyToTable(ADataTable : TDataTable);
    function GetValue(ARow : TAbstractRow) : TValue;
  public
    property DataField : TDataField read FDataField write SetDataField;
    property SortOrder : TSortOrder read FSortOrder write FSortOrder;
    function Equals(ARule : TRowSortOrderRule) : Boolean;
    destructor Destroy; override;
  end;

  TRowSortOrder = class
  private
    FOrder : TList;
    FDataTable : TDataTable;
    function GetRule(idx : Integer) : TRowSortOrderRule;
    procedure SetDataTable(ADataTable : TDataTable);
    procedure OrderRowsInternal(Rows : TStrings; Low, High : Integer);
    function GetIndexOfField(ADataField : TDataField) : Integer;
  protected
    property DataTable : TDataTable read FDataTable write SetDataTable;
  public
    constructor Create;
    constructor CreateCopy(ARowSortOrder : TRowSortOrder);
    constructor CreateFromKeys(ADataTable : TDataTable; SortOrder : TSortOrder);
    constructor CreateCopyKeepKeysOnly(ASortOrder : TRowSortOrder; ADataTable : TDataTable);
    destructor Destroy; override;

    procedure Clear;
    procedure CopyFrom(ARowSortOrder : TRowSortOrder);
    procedure AddDefaultRule(ADataField : TDataField);
    procedure AddRule(ADataField : TDataField; ASortOrder : TSortOrder);
    procedure Remove(Index : Integer);
    procedure RemoveRule(ADataField : TDataField);
    procedure Move(CurrIndex, NewIndex : Integer);
    procedure MoveRule(ADataField : TDataField; NewIndex : Integer);
    property Rule[idx : Integer] : TRowSortOrderRule read GetRule; default;
    function RuleCount : Integer;
    property IndexOfField[ADataField : TDataField] : Integer read GetIndexOfField write MoveRule;
    procedure OrderRows(ADataTable : TDataTable; Rows : TStrings);
    procedure OrderRowsBetween(ADataTable : TDataTable; Rows : TStrings; Low, High : Integer);
    function IsSubSetOf(MainOrder : TRowSortOrder) : Boolean;
  end;

  TDataFieldSet = class
  private
    FFields : TIndexContainer;
    procedure AddConditionField(Field : TDataField; Dummy : Boolean);
  public
    function RowsEqual(Row1, Row2 : TAbstractRow) : Boolean;
    function FieldCount : Integer;
//    property Field[idx : Integer] : TDataField read GetField;

    procedure AddField(Field : TDataField);
    procedure AddFromFieldArray(Fields : array of TDataField);
{$ifdef D4_OR_HIGHER}
    procedure AddFromTable(Table : TDataTable; Keys : Boolean = True; Fields : Boolean = True; IgnoreFields : Boolean = False);
{$else}
    procedure AddFromTable(Table : TDataTable; Keys, Fields, IgnoreFields : Boolean);
{$endif D4_OR_HIGHER}
    procedure AddFieldsFromCondition(ACond : TCondition);
    procedure RemoveField(Field : TDataField);
    function ContainsField(Field : TDataField) : Boolean;
    function ContainsAllFields(DataTable : TDataTable) : Boolean;
    procedure Clear;

    constructor Create;
    constructor CreateFromFieldArray(Fields : array of TDataField);
    constructor CreateFromTable(Table : TDataTable);
    constructor CreateDiff(PositiveSet, NegativeSet : TDataFieldSet);
    destructor Destroy; override;
    procedure CopyFrom(Fields : TDataFieldSet);
    procedure AddFrom(Fields : TDataFieldSet);

    function IncludeField(DataTable : TDataTable; Index : Integer) : Boolean;
    function IncludeFieldAcceptKeys(DataTable : TDataTable; Index : Integer) : Boolean;
    procedure ProcFields(Proc : TProcDataField);
  end;

  TDataFieldSetIterator = class
  private
    FIterator : TIndexContainerIterator;
    function GetField : TDataField;
  public
    constructor Create(ASet : TDataFieldSet);
    destructor Destroy; override;
    procedure First;
    procedure Next;
    function EOF : Boolean;
    property Field : TDataField read GetField;
  end;



  TLangFieldArray = class(TCommonLangArray)
  private
    FOnGetDefaultField : TGetFieldEvent;

    procedure SetField(Index : Integer; Field : TDataField);
    function GetField(Index : Integer) : TDataField;
    function GetCurrentField : TDataField;
  protected
    function GetDefaultObjValue : TObject; override;
  public
    property Fields[idx : Integer] : TDataField read GetField write SetField;
    property Field : TDataField read GetCurrentField;

    property OnGetDefaultField : TGetFieldEvent read FOnGetDefaultField write FOnGetDefaultField;

    constructor Create;
    constructor CreateWithFields(Fields : array of TDataField);

    procedure SetAllFields(Fields : array of TDataField);
    procedure AddField(Field : TDataField);
  end;


  TCacheLoadPolicy = class(TSingleton)
  protected
    function CreateCritCriteria(Table : TDataTable; Condition : TCondition; CachingNonKeys : TDataFieldSet) : TCommonQuilt {TCriteria}; virtual; abstract;
    function CreateArrayCriteria(Table : TDataTable; GetValues : array of String) : TCommonQuilt; virtual; abstract;
    function CreateRowCriteria(Table : TDataTable; Row : TAbstractRow; DerivedKeys : array of TDataField; CachingNonKeys : TDataFieldSet) : TCommonQuilt; virtual; abstract;
  end;

  TDefaultCacheLoadPolicy = class(TCacheLoadPolicy)
  private
    FNoCriteriaKeyCount : Integer;
    function MaxKeys(Table : TDataTable) : Integer;
  protected
    function CreateCritCriteria(Table : TDataTable; Condition : TCondition; CachingNonKeys : TDataFieldSet) : TCommonQuilt; override;
    function CreateArrayCriteria(Table : TDataTable; GetValues : array of String) : TCommonQuilt; override;
    function CreateRowCriteria(Table : TDataTable; Row : TAbstractRow; DerivedKeys : array of TDataField; CachingNonKeys : TDataFieldSet) : TCommonQuilt; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateOld(NoCriteriaKeyCount : Integer);
    destructor Destroy; override;
  published
    property NoCriteriaKeyCount : Integer read FNoCriteriaKeyCount write FNoCriteriaKeyCount;
  end;

  function WhereCond(var i : Integer) : String;

    // LGE 120700: No longer needed!!
//  procedure InitUnit(NewQueryFunction : TNewQueryFunction);
  {/**
    * Set logging function
    */}
  procedure SetLogFunction(LogFunction : TLogFunction);
  {/**
    * DataElements' logging function
    */}
  procedure Log(LogType : TLogType; const LogKey, LogMessage : String);
  {/**
    * Registers the TSingletons component
    */}


var
  LanguageIndex : Integer;

  FieldValue : TDataField;
  //NewQuery : TNewQueryFunction;

implementation

uses
{$ifndef LINUX}
  {Consts,} Windows,
{$endif LINUX}


  CalcField,




  Math, Quilt, Storages;

const
  HASHSIZE = 101{53}; // number isn't important as long as it's of reasonable size and a prime

var
  LogFunc : TLogFunction;

  UTableList : TValueList;
  UFieldList : TValueList;
  UTableTypeList : TValueList;

  FDefaultSubTotalHandler : TDefaultSubTotalHandler;
  FMinimumLoadPolicy : TDefaultCacheLoadPolicy;
  FDefaultRunningNumberGenerator : TDefaultRunningNumberGenerator;

  FieldCallingObject : TKeyField;
  FieldMissingProperty : TKeyField;
  FieldPropInfo : TKeyField;
  FieldIndex : TKeyField;
  FixupTable : TDataTable;
  FixupStorage : TRowStorage;



function FastSearchIndex(AField : TDataField) : Integer;
begin
  Result := Integer(AField) mod HASHSIZE;
end;

procedure CreateUList(var List : TValueList; Sorted : Boolean = True);
begin
  if List = nil then
  begin
    List := TValueList.Create(StringType(255, True));
    List.Sorted := Sorted;
    if Sorted then
      List.Duplicates := dupError;
  end;
end;

procedure FreeUList(var List : TValueList);
begin
  List.Free;
  List := nil;
end;

(*
procedure InitUnit(NewQueryFunction : TNewQueryFunction);
begin
  NewQuery := NewQueryFunction;
end;
*)

procedure SetLogFunction(LogFunction : TLogFunction);
begin
  LogFunc := LogFunction;
end;

procedure Log(LogType : TLogType; const LogKey, LogMessage : String);
begin
  if Assigned(LogFunc) then
    LogFunc(LogType, LogKey, LogMessage);

  if LogType = ltError then
    raise Exception.Create(LogMessage);
end;

















// ------------------------- TSingletons ---------------------------------------

{$ifdef DTIDEBUG}
procedure TSingletons.WriteLog;
begin
  FDebugLog.SaveToFile('E:\temp\' +Self.Name +'Log.txt');
end;
{$endif DTIDEBUG}




























//------------------------- TSingleton ---------------------------------//

constructor TSingleton.Create(AOwner: TComponent);
var
  ADesignInfo : LongInt;
begin
  inherited Create(AOwner);


  LongRec(ADesignInfo).Lo := 32767; // Left
  LongRec(ADesignInfo).Hi := 32767; // Top
  Self.DesignInfo := ADesignInfo;
  FHelpTexts := TLangArray.Create('');
  FHelpTexts.OnGetCurrentLanguage := GetCurrentLanguageIndex;
end;

destructor TSingleton.Destroy;
begin
  FHelpTexts.Free;

  inherited Destroy;
end;

function TSingleton.HasParent: Boolean;
begin

  Result := False;

end;

function TSingleton.GetParentComponent: TComponent;
begin

  Result := nil;

end;

procedure TSingleton.SetParentComponent(Value: TComponent);
begin

end;

procedure TSingleton.GetCurrentLanguageIndex(Sender : TCommonLangArray; var Language : Integer);
begin

  Language := DataElements.LanguageIndex

end;

procedure TSingleton.DefineProperties(Filer: TFiler);
begin

end;

procedure TSingleton.SetName(const NewName: TComponentName);

begin

end;

procedure TSingleton.Modified;

begin

end;

function TSingleton.GetHelpText : String;
begin
  Result := FHelpTexts.Text;
end;

procedure TSingleton.AssignHelpTexts(Value : TLangArray);
begin
  FHelpTexts.Assign(Value);
end;

procedure TSingleton.SetHelpTexts(HelpTexts : array of String);
begin
  FHelpTexts.SetAllTexts(HelpTexts);
end;





// ------------------------------- DataBridge ----------------------------------

(*
constructor TDataBridge.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FCanRead := True;
  FCanWrite := True;
  fReuseParameters := True;

end;
*)

constructor TDataBridge.CreateOld(CanRead, CanWrite : Boolean); // Fixa LGE ta med en DBProperties-param i CreateOld
begin
  inherited Create(nil);

  FCanRead := CanRead;
  FCanWrite := CanWrite;
  fReuseParameters := True;

end;

destructor TDataBridge.Destroy;
begin
  inherited Destroy;
end;





function TDataBridge.DeleteData(Sender : TDataTable; Condition : TCondition; Interruptable : TInterruptable) : integer;
begin
  if not Self.CanWrite then
    raise Exception.Create(Self.ClassName + '.DeleteData: Write is not allowed to this type of DataBridge')
  else
    raise Exception.Create(Self.ClassName + '.DeleteData: Method not implemented');
end;

function TDataBridge.UpdateData(Sender : TDataTable; Condition : TCondition; NewValues : TAbstractRow; AffectedFields : TDataFieldSet; Interruptable : TInterruptable) : integer;
begin
  if not Self.CanWrite then
    raise Exception.Create(Self.ClassName + '.UpdateData: Write is not allowed to this type of DataBridge')
  else
    raise Exception.Create(Self.ClassName + '.UpdateData: Method not implemented');
end;









function TDataBridge.GetSupportsSQL : Boolean;
begin
  Result := False;
end;









// ---------------------- TDefaultCacheLoadPolicy ------------------------------

function TDefaultCacheLoadPolicy.MaxKeys(Table : TDataTable) : Integer;
begin
  Result := MaxIntValue([0, Table.KeyCount - FNoCriteriaKeyCount]);
end;


function TDefaultCacheLoadPolicy.CreateCritCriteria(Table : TDataTable; Condition : TCondition; CachingNonKeys : TDataFieldSet) : TCommonQuilt {TCriteria};
var
  i, m : Integer;
  KeepSet : TDataFieldSet;
begin
  if Condition = nil then
    Result := TQuiltPatch.Create
  else
  begin
    Result := Condition.CreateCommonQuilt;
    KeepSet := TDataFieldSet.Create;

    m := MaxKeys(Table);
    for i := 0 to Table.FieldCount - 1 do
      if (i < m) or CachingNonKeys.IncludeField(Table, i) then
        KeepSet.AddField(Table.Field[i]);

    Result.KeepDimensions(KeepSet);
    KeepSet.Free;
  end;
end;

function TDefaultCacheLoadPolicy.CreateArrayCriteria(Table : TDataTable; GetValues : array of String) : TCommonQuilt;
var
  i, l : Integer;
  ResultPatch : TQuiltPatch;
begin
  l := Low(GetValues);

  ResultPatch := TQuiltPatch.Create;
  Result := ResultPatch;

  for i := l to Min(High(GetValues), l + MaxKeys(Table) - 1) do
    ResultPatch[Table.Field[i-l]].Add(ValueFromString(GetValues[i]));
end;

function TDefaultCacheLoadPolicy.CreateRowCriteria(Table : TDataTable; Row : TAbstractRow; DerivedKeys : array of TDataField; CachingNonKeys : TDataFieldSet) : TCommonQuilt;
var
  i, m, idx : Integer;
  ThisField : TDataField;
  ThisFieldSrc : TDataField;
  ResultPatch : TQuiltPatch;
begin
  ResultPatch := TQuiltPatch.Create;
  Result := ResultPatch;
  m := MaxKeys(Table);

  if Row.DataTable = Table then // special behaviour for own datatable to avoid key conflicts when adding new rows...
  begin
    for i := 0 to Table.KeyCount - 1 do
      if (i < m) then
        ResultPatch[Table.Field[i]].Add(Row.ValueByIndex[i]);
  end
  else
    for i := 0 to Table.FieldCount - 1 do
      if (i < m) or CachingNonKeys.IncludeField(Table, i) then
      begin
        ThisField := Table.Field[i];
        ThisFieldSrc := ThisField;

        for idx := Low(DerivedKeys) to High(DerivedKeys) do
          if (DerivedKeys[idx] <> nil) and
             (DerivedKeys[idx].AuxTableField = ThisField) then
          begin
            ThisFieldSrc := DerivedKeys[idx];
            Break;
          end;

        if Row.FieldHasValue(ThisFieldSrc) then
          ResultPatch[Table.Field[i]].Add(Row[ThisFieldSrc]);
      end;
end;

constructor TDefaultCacheLoadPolicy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNoCriteriaKeyCount := 0;
end;

constructor TDefaultCacheLoadPolicy.CreateOld(NoCriteriaKeyCount : Integer);
begin
  Create(nil);
  FNoCriteriaKeyCount := NoCriteriaKeyCount;
end;

destructor TDefaultCacheLoadPolicy.Destroy;
begin
  inherited Destroy;
end;

// ----------------------------- TRowSource ------------------------------------

constructor TRowSource.Create(DataTable : TDataTable; OperationType : TOperationType);
begin
  inherited Create;

  FDataTable := DataTable;
  FOperationType := OperationType;

  FDestinations := TList.Create;
end;

destructor TRowSource.Destroy;
begin
  inherited Destroy;
  FDestinations.Free;
end;

procedure TRowSource.AddDestination(ADest : TObject);
begin
  FDestinations.Add(ADest);
end;

procedure TRowSource.RemoveDestination(ADest : TObject);
begin
  FDestinations.Remove(ADest);
end;

function TRowSource.GetDataTable : TDataTable;
begin
  Result := FDataTable;
end;

procedure TRowSource.FinishOperations;
begin
  if FDestinations.Count = 0 then
    FreeRowMemory;
end;

procedure TRowSource.FreeRowMemory;
begin
  // No code at this level
end;

function TRowSource.GetSortOrder : TRowSortOrder;
begin
  Result := nil;
end;

function TRowSource.GetProperties : String;
begin
  Result := DataTable.TableName;
end;

procedure TRowSource.GotRow(Row : TDataRow; RowCount : Integer);
begin
(*  if Row <> nil then
    FResultRows.AddObject('', Row); *)
end;

procedure TRowSource.DoStartOperations(ARowSource : TRowSource);
begin
//  FResultRows.Clear;

  if ARowSource.Interruptable.Interrupted then
    Exit;

  ARowSource.Interruptable.InitParams(ARowSource.OperationType, Self);

  ARowSource.AddDestination(Self);

  ARowSource.FGotRows := 0;
  ARowSource.StartOperations;
end;

procedure TRowSource.DoFinishOperations(ARowSource : TRowSource);
begin
  ARowSource.RemoveDestination(Self);
  ARowSource.FinishOperations;
end;

function TRowSource.DoGetNextRow(ARowSource : TRowSource) : TDataRow;
begin
  if ARowSource.Interruptable.Interrupted then
  begin
    Result := nil;
    Exit;
  end;

  Result := ARowSource.GetNextRow;
  if Result <> nil then
    Inc(ARowSource.FGotRows);

  if Self <> nil then
    GotRow(Result, ARowSource.FGotRows);

  if Result = nil then
    ARowSource.Interruptable.Finished(ARowSource.OperationType, Self)
  else
    ARowSource.Interruptable.AddRow(ARowSource.OperationType, Self);
end;

function TRowSource.AddToLog(Components, Log : TStrings; Indentation : Integer) : Boolean;
var
  idx : Integer;
begin
  idx := Components.IndexOfObject(Self);
  Result := idx < 0;
  if not Result then
  begin
    Log.Add(StringOfChar(' ', 2 * Indentation) + '[' + Components.Strings[idx] + ']');
  end
  else
  begin
    idx := Components.AddObject(IntToStr(Components.Count + 1), Self);
    Log.Add(StringOfChar(' ', 2 * Indentation) + Self.ClassName + '(' + Properties + ') Output: ' +
            IntToStr(Self.ProcessedRowCount) + ' [' + Components.Strings[idx] + ']');
  end;
end;

// ----------------------- TRowSortOrderRule -----------------------------------

constructor TRowSortOrderRule.Create(ADataField : TDataField; ASortOrder : TSortOrder; Owner : TRowSortOrder);
begin
  FOwner := Owner;
  FDataField := ADataField;
  FSortOrder := ASortOrder;
  FFieldIndex := -1;
end;

destructor TRowSortOrderRule.Destroy;
begin
  inherited Destroy;
end;

procedure TRowSortOrderRule.ApplyToTable(ADataTable : TDataTable);
begin
  if ADataTable <> nil then
    FFieldIndex := ADataTable.IndexOfField(FDataField)
  else
    FFieldIndex := -1;
end;

procedure TRowSortOrderRule.SetDataField(ADataField : TDataField);
begin
  if ADataField = nil then
    Log(ltError, 'SortOrder', 'TRowSortOrderRule.SetDataField: Field cannot be nil!');

  if ADataField <> FDataField then
  begin
    FDataField := ADataField;
    FFieldIndex := -1;
  end;
end;

function TRowSortOrderRule.GetValue(ARow : TAbstractRow) : TValue;
begin
  if FFieldIndex = -1 then
    Result := ARow[FDataField]
  else if FOwner.FDataTable <> ARow.DataTable then
    Result := ARow[FDataField]
  else
    Result := ARow.ValueByIndex[FFieldIndex];
end;

function TRowSortOrderRule.Equals(ARule : TRowSortOrderRule) : Boolean;
begin
  Result := (Self.DataField = ARule.DataField) and
            (Self.SortOrder = ARule.SortOrder);
end;

// ------------------------- TRowSortOrder -------------------------------------

function TRowSortOrder.IsSubSetOf(MainOrder : TRowSortOrder) : Boolean;
var
  i : Integer;
begin
  if MainOrder = nil then
    Result := false
  else if MainOrder.RuleCount < Self.RuleCount then
    Result := false
  else
  begin
    Result := True;
    for i := 0 to Self.RuleCount - 1 do
      if not Self.Rule[i].Equals(MainOrder.Rule[i]) then
      begin
        Result := False;
        Exit;
      end;
  end;
end;

constructor TRowSortOrder.Create;
begin
  FOrder := TList.Create;
  FDataTable := nil;
end;

constructor TRowSortOrder.CreateCopy(ARowSortOrder : TRowSortOrder);
var
  i : Integer;
begin
  Create;

  for i := 0 to ARowSortOrder.RuleCount - 1 do
    with ARowSortOrder.Rule[i] do
      Self.AddRule(DataField, SortOrder);
end;

constructor TRowSortOrder.CreateFromKeys(ADataTable : TDataTable; SortOrder : TSortOrder);
var
  i : Integer;
begin
  Create;

  for i := 0 to ADataTable.KeyCount - 1 do
    Self.AddRule(ADataTable.Field[i], SortOrder);
end;

constructor TRowSortOrder.CreateCopyKeepKeysOnly(ASortOrder : TRowSortOrder; ADataTable : TDataTable);
var
  i : Integer;
begin
  Create;

  for i := 0 to ASortOrder.RuleCount - 1 do
    with ASortOrder.Rule[i] do
      if ADataTable.TableHasKey(DataField) then
        Self.AddRule(DataField, SortOrder);
end;

destructor TRowSortOrder.Destroy;
var
  i : Integer;
begin
  for i := 0 to FOrder.Count - 1 do
    TRowSortOrderRule(FOrder.Items[i]).Free;
  FOrder.Free;
  inherited Destroy; //LAA.added
end;

procedure TRowSortOrder.Clear;
var
  idx : Integer;
begin
  for idx := RuleCount -1 downto 0 do
    Remove(idx);
end;

procedure TRowSortOrder.CopyFrom(ARowSortOrder : TRowSortOrder);
var
  i : Integer;
begin
  Self.Clear;

  for i := 0 to ARowSortOrder.RuleCount - 1 do
    with ARowSortOrder.Rule[i] do
      Self.AddRule(DataField, SortOrder);
end;

procedure TRowSortOrder.AddDefaultRule(ADataField : TDataField);
begin
  AddRule(ADataField, soAscending);
end;

procedure TRowSortOrder.AddRule(ADataField : TDataField; ASortOrder : TSortOrder);
begin
  FOrder.Add(TRowSortOrderRule.Create(ADataField, ASortOrder, Self));
end;

procedure TRowSortOrder.Remove(Index : Integer);
begin
  TRowSortOrderRule(FOrder.Items[Index]).Free;
  FOrder.Delete(Index);
end;

procedure TRowSortOrder.RemoveRule(ADataField : TDataField);
var
  i : Integer;
begin
  for i := 0 to FOrder.Count - 1 do
    if Rule[i].DataField = ADataField then
      Remove(i);
end;

procedure TRowSortOrder.Move(CurrIndex, NewIndex : Integer);
begin
  FOrder.Move(CurrIndex, NewIndex);
end;

procedure TRowSortOrder.MoveRule(ADataField : TDataField; NewIndex : Integer);
var
  i : Integer;
begin
  for i := 0 to FOrder.Count - 1 do
    if Rule[i].DataField = ADataField then
    begin
      Move(i, NewIndex);
      Exit;
    end;
  Log(ltError, 'MoveRule', 'TRowSortOrder.MoveRule: Field ' + ADataField.FieldName + ' not found!');
end;

function TRowSortOrder.GetIndexOfField(ADataField : TDataField) : Integer;
var
  iRule : Integer;
begin
  Result := -1;
  for iRule := 0 to RuleCount -1 do
    if Rule[iRule].DataField = ADataField then
    begin
      Result := iRule;
      Break;
    end;
end;

function TRowSortOrder.GetRule(idx : Integer) : TRowSortOrderRule;
begin
  Result := TRowSortOrderRule(FOrder.Items[idx])
end;

function TRowSortOrder.RuleCount : Integer;
begin
  Result := FOrder.Count;
end;

procedure TRowSortOrder.SetDataTable(ADataTable : TDataTable);
var
  i : Integer;
begin
  if ADataTable <> FDataTable then
  begin
    FDataTable := ADataTable;

    for i := 0 to RuleCount - 1 do
      Rule[i].ApplyToTable(ADataTable);
  end;
end;

procedure TRowSortOrder.OrderRows(ADataTable : TDataTable; Rows : TStrings);
begin
  SetDataTable(ADataTable);
  OrderRowsInternal(Rows, 0, Rows.Count - 1);
end;

procedure TRowSortOrder.OrderRowsBetween(ADataTable : TDataTable; Rows : TStrings; Low, High : Integer);
begin
  SetDataTable(ADataTable);
  OrderRowsInternal(Rows, Low, High);
end;

procedure TRowSortOrder.OrderRowsInternal(Rows : TStrings; Low, High : Integer);
var
  CmpRow, TmpRow : TAbstractRow;
  i, iMax, iMin, iMaxEqual, iMove : Integer;
  CmpRes : Integer;
begin
  {Assert((Rows <> nil) and (MaxIndex >= 0) and (MaxIndex < Rows.Count) and (MinIndex >= 0) and (MinIndex < Rows.Count),
         'TDataRowList.OrderRows: (Rows <> nil) and (MaxIndex >= 0) and (MaxIndex < Rows.Count) and (MinIndex >= 0) and (MinIndex < Rows.Count)');}

  if High - Low < 2 then
  begin
    if (High > Low) and (TAbstractRow(Rows.Objects[High]).Compare(TAbstractRow(Rows.Objects[Low]), Self) < 0) then
      Rows.Exchange(Low, High);
    Exit;
  end;

  Rows.Exchange(High, Low + Random(High - Low));

  CmpRow := TAbstractRow(Rows.Objects[High]);
  iMaxEqual := High;
  iMax := High - 1;
  iMin := Low;
  i := iMax;

  // CompRes:
  // -1: TmpRow shall be moved to iMax
  //  0: Equal
  //  1: TmpRow shall be moved to iMin

  while iMax > iMin do
  begin
    TmpRow := TAbstractRow(Rows.Objects[i]);
    CmpRes := CmpRow.Compare(TmpRow, Self); // < 0;
    if i = iMax then
    begin
      if CmpRes < 0 then
      begin
        Dec(iMax);
        Dec(i);
      end
      else if CmpRes > 0 then
      begin
        Rows.Exchange(iMax, iMin);
        Inc(iMin);
        i := iMin;
      end
      else
      begin
        Dec(iMaxEqual);
        if iMax <> iMaxEqual then
          Rows.Exchange(iMax, iMaxEqual);
        Dec(iMax);
        i := iMax;
      end;
    end
    else
    begin
      if CmpRes > 0 then
      begin
        Inc(iMin);
        Inc(i);
      end
      else if CmpRes < 0 then
      begin
        Rows.Exchange(iMax, iMin);
        Dec(iMax);
        i := iMax;
      end
      else
      begin
        Rows.Exchange(iMax, iMin);
        Dec(iMaxEqual);
        if iMax <> iMaxEqual then
          Rows.Exchange(iMax, iMaxEqual);
        Dec(iMax);
        i := iMin;
      end;
    end;
  end;

  TmpRow := TAbstractRow(Rows.Objects[i]);
  CmpRes := CmpRow.Compare(TmpRow, Self); // < 0;
  if CmpRes < 0 then
  begin
    for iMove := 0 to High - iMaxEqual do
      Rows.Exchange(i + iMove, iMaxEqual + iMove);
    OrderRowsInternal(Rows, Low, i - 1);
    OrderRowsInternal(Rows, i + High - iMaxEqual + 1, High);
  end
  else if CmpRes > 0 then
  begin
    for iMove := 0 to High - iMaxEqual do
      Rows.Exchange(i + 1 + iMove, iMaxEqual + iMove);
    OrderRowsInternal(Rows, Low, i);
    OrderRowsInternal(Rows, i + High - iMaxEqual + 2, High);
  end
  else
  begin
    for iMove := 0 to High - iMaxEqual do
      Rows.Exchange(i + 1 + iMove, iMaxEqual + iMove);
    OrderRowsInternal(Rows, Low, i - 1);
    OrderRowsInternal(Rows, i + High - iMaxEqual + 2, High);
  end;
end;

// --------------------------- TCallbackCommands -------------------------------

procedure TCallbackCommandRemoveRow.Execute;
begin
  if FRow.SubTotalRow <> nil then
    FRow.SubTotalRow.DeleteRow(FRow, False);
end;

constructor TCallbackCommandRemoveRow.Create(Row : TAbstractRow);
begin
  inherited Create;
  FRow := Row;
end;

destructor TCallbackCommandRemoveRow.Destroy;
begin
  inherited Destroy;
end;

procedure TCallbackCommandRestoreValue.Execute;
begin
  FRow[FDataField] := FOldValue;
end;

constructor TCallbackCommandRestoreValue.Create(Row : TAbstractRow; DataField : TDataField; OldValue : TValue);
begin
  inherited Create;
  FRow := Row;
  FDataField := DataField;
  FOldValue := OldValue;
end;

destructor TCallbackCommandRestoreValue.Destroy;
begin
  inherited Destroy;
end;

// ---------------------------- TSubTotalHandlerMark ---------------------------

constructor TSubTotalHandlerMark.Create(Storage : TAbstractRowStorage; DataField : TDataField);
begin
  inherited Create;
  FStorage := Storage;
  FDataField := DataField;
  FCallbackCommands := TList.Create;
  FSubTotalRows := TList.Create;
end;

destructor TSubTotalHandlerMark.Destroy;
var
  i : Integer;
begin
  inherited Destroy;
  for i := 0 to FCallbackCommands.Count - 1 do
    TCallbackCommand(FCallbackCommands.Items[i]).Free;
  FCallbackCommands.Free;
  FSubTotalRows.Free;
end;

procedure TSubTotalHandlerMark.Callback;
var
  i : Integer;
begin
  for i := FCallbackCommands.Count - 1 downto 0 do
    with TCallbackCommand(FCallbackCommands.Items[i]) do
    begin
      Execute;
      Free;
    end;

  FCallbackCommands.Clear;
end;

procedure TSubTotalHandlerMark.AddCallbackCommand(Cmd : TCallbackCommand);
begin
  FCallbackCommands.Add(Cmd);
end;

function TSubTotalHandlerMark.GetSubTotalRow : TSubTotalRow;
begin
  if FSubTotalRows.Count = 0 then
    Result := nil
  else
    Result := TSubTotalRow(FSubTotalRows.Items[FSubTotalRows.Count - 1]);
end;

procedure TSubTotalHandlerMark.Push(SubTotalRow : TSubTotalRow);
begin
  FSubTotalRows.Add(SubTotalRow);
end;

function TSubTotalHandlerMark.Pop : TSubTotalRow;
begin
  if FSubTotalRows.Count = 0 then
    Result := nil
  else
  begin
    Result := TSubTotalRow(FSubTotalRows.Items[FSubTotalRows.Count - 1]);
    FSubTotalRows.Delete(FSubTotalRows.Count - 1);
  end;
end;

// ---------------------------- TDefaultSubTotalHandler ------------------------

constructor TDefaultSubTotalHandler.Create;
begin
  inherited Create(CallBackOnException);
end;

function TDefaultSubTotalHandler.LevelEditable(Sender : TAbstractRowStorage; SubTotalLevel : Integer;
                     var ReadOnlyReason : String) : Boolean;
begin
  ReadOnlyReason := 'Subtotals are by default read-only. Subclass TSubTotalHandler for writable subtotals!';
  Result := False;
end;


// ------------------------------ TSubTotalHandler -----------------------------

constructor TSubTotalHandler.Create(CallBackOnException : Boolean);
begin
  inherited Create;
  FCallBackOnException := CallBackOnException;
  FMarks := TList.Create;
end;

destructor TSubTotalHandler.Destroy;
begin
  inherited Destroy;
  FMarks.Free; // We suppose this list is empty.
               // If there should be any objects in it; freeing them would probably cause major exceptions...
end;

function TSubTotalHandler.GetNextKeyLevel : TDataField;
(*var
  idx : Integer;*)
begin
  Result := SubTotalRow.SubTotalKey.TreeKey;
(*  idx := SubTotalRow.SubTotalKey.TreeKeyIndex; // ActiveMark.Storage.TreeKeyByField(ActiveMark.DataField).TreeKeyIndex;
  Result := ActiveMark.Storage.TreeKey[idx + 1].TreeKey; *)
end;

function TSubTotalHandler.GetStorage : TAbstractRowStorage;
begin
  Result := ActiveMark.Storage;
end;

function TSubTotalHandler.GetDataField : TDataField;
begin
  Result := ActiveMark.DataField;
end;

function TSubTotalHandler.GetSubTotalRow : TSubTotalRow;
begin
  Result := ActiveMark.SubTotalRow;
end;

function TSubTotalHandler.GetChildRowCount : Integer;
begin
  Result := SubTotalRow.SubRowCount;
end;

function TSubTotalHandler.GetChildRow(idx : Integer) : TAbstractRow;
begin
  Result := SubTotalRow.SubRows[idx];
end;

function TSubTotalHandler.GetChildRowByKey(KeyValue : TValue) : TAbstractRow;
var
  idx : Integer;
begin
  if SubTotalRow.FindValue(KeyValue, idx) then
    Result := SubTotalRow.SubRows[idx]
  else
    Result := nil;
end;

procedure TSubTotalHandler.UpdateSubTotals(Sender : TAbstractRowStorage; SubTotalRow : TSubTotalRow; StoreValue : TStoreValueMethod);
var
  i, j : Integer;
  Sum : TValue;
begin
  for i := Sender.DataTable.KeyCount to Sender.DataTable.FieldCount - 1 do
  if Sender.DataTable.Field[i].IsAggregable then
  begin
    if ChildRowCount = 0 then
      StoreValue(i, ZeroVal)
    else
    begin
      Sum := ChildRow[0].ValueByIndex[i];
      for j := 1 to ChildRowCount - 1 do
        Sum := Sum.DataType.Sum(Sum, ChildRow[j].ValueByIndex[i]);
      StoreValue(i, Sum);
    end;
  end;
end;

function TSubTotalHandler.SetValue(KeyValue : TValue; NewValue : TValue) : TAbstractRow;
var
  ChildRow : TAbstractRow;
  i : Integer;
  OldValue : TValue;
begin
  (* 1.  Skaka nästa level
     1a.   Skapa om ej finns, lägg till i callbacken
     2.  Ifall subtotal
     2a.   Pusha
     2b.   Kalla på Distributevalue
     2c.   Poppa
     3.  Ifall detail
     3a.   Lägg till värde
     3b.   Lägg till i callbacken
  *)

  ChildRow := ChildRowByKey[KeyValue];
  if ChildRow = nil then
  begin
    if SubTotalRow.IsLastTreeNode then
    begin
      ChildRow := Storage.CreateNewRow(nil); // Fixa LGE
      for i := 0 to Storage.TreeKeyCount - 2 do
        if Storage.TreeKey[i].RowFieldIndex >= 0 then
          ChildRow.StoreValue(Storage.TreeKey[i].RowFieldIndex, SubTotalRow.FetchValue(Storage.TreeKey[i].RowFieldIndex));

      if Storage.TreeKey[Storage.TreeKeyCount - 1].RowFieldIndex >= 0 then
        ChildRow.StoreValue(Storage.TreeKey[Storage.TreeKeyCount - 1].RowFieldIndex, KeyValue);

      ChildRow.FStorage := SubTotalRow.Storage; // Fixa LGE kolla när vi sätter FStorage och FSubTotalRow
      ChildRow.FSubTotalRow := SubTotalRow;
      TDataRow(ChildRow).FStatus := rsNew;
    end
    else
    begin
      ChildRow := Storage.CreateSubTotal(SubTotalRow, KeyValue, Storage.TreeKey[SubTotalRow.SubTotalKey.TreeKeyIndex+1].Visible);
    end;
    SubTotalRow.AddChildRow(KeyValue, ChildRow);
    ActiveMark.AddCallbackCommand(TCallbackCommandRemoveRow.Create(ChildRow));
  end;

  if SubTotalRow.IsLastTreeNode then
  begin
    if CallbackOnException then
    begin
      OldValue := ChildRow[DataField];
      ChildRow[DataField] := NewValue;
      ActiveMark.AddCallbackCommand(TCallbackCommandRestoreValue.Create(ChildRow, DataField, OldValue));
    end
    else
    begin
      ChildRow[DataField] := NewValue;
    end;
  end
  else
  begin
    ActiveMark.Push(TSubTotalRow(ChildRow));
    DistributeValue(Storage, SubTotalRow, DataField, NewValue);
    ActiveMark.Pop;
  end;

  Result := ChildRow;
end;

function TSubTotalHandler.IncreaseValue(KeyValue : TValue; IncreaseBy : TValue) : TAbstractRow;
var
  ChildRow : TAbstractRow;
begin
  ChildRow := ChildRowByKey[KeyValue];
  if ChildRow <> nil then
    SetValue(KeyValue, DataField.DataType.Sum(ChildRow[DataField], IncreaseBy))
  else
    ChildRow := SetValue(KeyValue, IncreaseBy);
  Result := ChildRow;
end;

procedure TSubTotalHandler.SetMark(Storage : TAbstractRowStorage; SubTotalRow : TSubTotalRow; DataField : TDataField);
var
  Mark : TSubTotalHandlerMark;
begin
  Mark := TSubTotalHandlerMark.Create(Storage, DataField);
  Mark.Push(SubTotalRow);
  FMarks.Add(Mark);
end;

function TSubTotalHandler.ActiveMark : TSubTotalHandlerMark;
begin
  if FMarks.Count > 0 then
    Result := TSubTotalHandlerMark(FMarks.Items[FMarks.Count - 1])
  else
    Result := nil;
end;

procedure TSubTotalHandler.UnSetMark;
var
  Mark : TSubTotalHandlerMark;
begin
  Mark := ActiveMark;
  if ActiveMark <> nil then
  begin
    Mark.Free;
    FMarks.Delete(FMarks.Count - 1);
  end;
end;

function TSubTotalHandler.InternalDistributeValue(Sender : TAbstractRowStorage; SubTotalRow : TSubTotalRow; Field : TDataField; Value : TValue; Action : TSetAction) : TSetResult;
var
  ErrorMsg : String;
begin
  SetMark(Sender, SubTotalRow, Field);
  try
    if not CanEditValue(Sender, SubTotalRow, Field, ErrorMsg) then
      raise Exception.Create(ErrorMsg);

    try
      BeforeDistributeValue(Sender, SubTotalRow, Field, Value);
      DistributeValue(Sender, SubTotalRow, Field, Value);
    except
      if CallBackOnException then
        ActiveMark.Callback;
      raise;
    end;
  finally
    UnSetMark;
  end;

  Result := srOk;
end;

procedure TSubTotalHandler.InternalUpdateSubTotals(Sender : TAbstractRowStorage; SubTotalRow : TSubTotalRow; StoreValue : TStoreValueMethod);
begin
  SetMark(Sender, SubTotalRow, nil);
  try
    UpdateSubTotals(Sender, SubTotalRow, StoreValue);
  finally
    UnSetMark;
  end;
end;

function TSubTotalHandler.InternalCanEditValue(Sender : TAbstractRowStorage; SubTotalRow : TSubTotalRow;
                            DataField : TDataField; var ReadOnlyReason : String) : Boolean;
begin
  SetMark(Sender, SubTotalRow, DataField);
  try
    Result := CanEditValue(Sender, SubTotalRow, DataField, ReadOnlyReason);
  finally
    UnSetMark;
  end;
end;

function TSubTotalHandler.CanEditValue(Sender : TAbstractRowStorage; SubTotalRow : TSubTotalRow;
                            DataField : TDataField; var ReadOnlyReason : String) : Boolean;
begin
  Result := False;
  if Sender = nil then
    ReadOnlyReason := 'The subtotalrow isn''t in a storage.'
  else if (Sender.TreeKeyByField[DataField] <> nil) then
    ReadOnlyReason := 'The subtotalrow''s keys cannot be updated.'
  else if not DataField.IsAggregable then
    ReadOnlyReason := DataField.FieldName + ' isn''t aggregable.'
  else if Sender.InternalSubTotalHandler.LevelEditable(Sender, SubTotalRow.SubTotalKey.TreeKeyIndex, ReadOnlyReason) and
          Sender.InternalSubTotalHandler.SubTotalEditable(Sender, SubTotalRow, DataField, ReadOnlyReason) then
    Result := True;
end;

procedure TSubTotalHandler.DistributeValue(Sender : TAbstractRowStorage; SubTotalRow : TSubTotalRow; DataField : TDataField; Value : TValue);
var
  ValuePerRow : TValue;
  i : Integer;
begin
  // Default implementation
  if ChildRowCount = 0 then
    raise Exception.Create('Cannot distribute value because the subtotal has no child rows!');

  ValuePerRow := DataField.DataType.Quota(DataField.DataType.Difference(Value,
                     SubTotalRow[DataField]), ValueFromInteger(ChildRowCount));
  for i := 0 to ChildRowCount - 1 do
    IncreaseValue(ChildRow[i][NextKeyLevel], ValuePerRow);
end;

procedure TSubTotalHandler.BeforeDistributeValue(Sender : TAbstractRowStorage; SubTotalRow : TSubTotalRow; DataField : TDataField; var Value : TValue);
begin
end;

function TSubTotalHandler.LevelEditable(Sender : TAbstractRowStorage; SubTotalLevel : Integer; var ReadOnlyReason : String) : Boolean;
begin
  Result := True; // Default behaviour
end;

function TSubTotalHandler.SubTotalEditable(Sender : TAbstractRowStorage; SubTotalRow : TSubTotalRow; Field : TDataField; var ReadOnlyReason : String) : Boolean;
begin
  Result := SubTotalRow.SubRowCount > 0;
  if not Result then
    ReadOnlyReason := 'Cannot distribute value because the subtotal has no child rows!';
end;

// -------------------------- TRowStorageTreeKey -------------------------------

constructor TRowStorageTreeKey.CreateDetailLevel(Storage : TAbstractRowStorage);
begin
  CreateSorted(Storage, nil, nil, soAscending);
end;

constructor TRowStorageTreeKey.Create(Storage : TAbstractRowStorage; TreeKey : TDataField);
begin
  CreateSorted(Storage, Treekey, TreeKey.SortField, TreeKey.SortOrder);
end;

constructor TRowStorageTreeKey.CreateSorted(Storage : TAbstractRowStorage; TreeKey : TDataField; SortField : TDataField; SortOrder : TSortOrder);
begin
  inherited Create;

  FStorage := Storage;
  FTreeKey := TreeKey;
  FSortField := SortField;
  FSortOrder := SortOrder;
  FVisible := IsDetailLevel;
  FTreeKeyIndex := -1;
  FRowFieldIndex := -1;
end;

destructor TRowStorageTreeKey.Destroy;
begin
  // No code needed
  inherited Destroy;
end;

procedure TRowStorageTreeKey.ResetSortField;
begin
  Self.SortField := TreeKey.SortField;
  Self.SortOrder := TreeKey.SortOrder;
end;

procedure TRowStorageTreeKey.SetSortField(SortField : TDataField);
begin
  if FSortField <> SortField then
  begin
    FSortField := SortField;

    if (FStorage is TCustomRowStorage) and TCustomRowStorage(FStorage).AutoArrange then
      FStorage.FRowListUpToDate := False
  end;
end;

procedure TRowStorageTreeKey.SetSortOrder(SortOrder : TSortOrder);
begin
  if FSortOrder <> SortOrder then
  begin
    FSortOrder := SortOrder;

    if (FStorage is TCustomRowStorage) and TCustomRowStorage(FStorage).AutoArrange then
      FStorage.FRowListUpToDate := False
  end;
end;

procedure TRowStorageTreeKey.SetVisible(V : Boolean);
begin
  // Fixa LGE

  begin
    FVisible := V;
    // Fixa LGE
    if Self.IsDetailLevel then
      FStorage.Total.UpdateRowVisibility(FStorage.TreeKeyCount, V)
    else
      FStorage.Total.UpdateRowVisibility(Self.TreeKeyIndex, V);
    FStorage.FLastChanged := Now;
  end;
end;

function TRowStorageTreeKey.GetVisible : Boolean;
begin
  // Fixa LGE

    Result := FVisible;
end;

function TRowStorageTreeKey.IsDetailLevel : Boolean;
begin
  Result := FTreeKey = nil;
end;

function TRowStorageTreeKey.CreateCopy(Storage : TAbstractRowStorage) : TRowStorageTreeKey;
begin
  Result := TRowStorageTreeKey.CreateSorted(Storage, FTreeKey, FSortField, FSortOrder);
  Result.FVisible := FVisible;
  Result.FTreeKeyIndex := FTreeKeyIndex;
  Result.FRowFieldIndex := FRowFieldIndex;
end;

procedure TRowStorageTreeKey.SetTable(Table : TAbstractRowStorage);
begin
  Assert(Table <> nil, 'TRowStorageTreeKey.SetTable: Table <> nil');

  FTreeKeyIndex := Table.FTree.IndexOf(Self);
  if Self.FTreeKey <> nil then
    FRowFieldIndex := Table.DataTable.IndexOfField(Self.FTreeKey)
  else
    FRowFieldIndex := -1;
end;

function TRowStorageTreeKey.GetValue(ARow : TAbstractRow) : TValue;
begin
  Assert(ARow <> nil, 'TRowStorageTreeKey.GetString: ARow <> nil');

  if RowFieldIndex >= 0 then
    Result := ARow.ValueByIndex[RowFieldIndex]
  else if ARow is TDataRow then
    Result := ARow[TreeKey]
  else if ARow is TSubTotalRow then
  begin
    if not TSubTotalRow(ARow).IsTreeKeyOnly(TreeKey, Result) then
      Log(ltError, 'RowStorageTree', 'Fatal error: RowStorageTree has become corrupt!');
  end
  else
  begin
    Result := ValueFromString('');
    Log(ltError, 'Row', 'Invalid RowType: ' + ARow.ClassName); //LGELAA!!!
//    Result := ValueFromString(0);
  end;
end;

// ---------------------------- TParamList -------------------------------------



// -------------------------------- TDataFieldSet ------------------------------

constructor TDataFieldSet.Create;
begin
  inherited Create;

  FFields := TIndexContainer.Create(HASHSIZE, False);
//  FCurrDataTable := nil;
end;

constructor TDataFieldSet.CreateFromFieldArray(Fields : array of TDataField);
var
  i : Integer;
begin
  Create;

  for i := Low(Fields) to High(Fields) do
    if Fields[i] <> nil then
      AddField(Fields[i]);
end;

constructor TDataFieldSet.CreateFromTable(Table : TDataTable);
begin
  Create;
  Table.AddFieldsToFieldSet(Self);
end;

constructor TDataFieldSet.CreateDiff(PositiveSet, NegativeSet : TDataFieldSet);
var
  Iterator : TIndexContainerIterator;
begin
  Create;
  CopyFrom(PositiveSet);

  Iterator := TIndexContainerIterator.Create(NegativeSet.FFields);
  while not Iterator.EOF do
  begin
    Self.RemoveField(TDataField(Iterator.Index));
    Iterator.Next;
  end;

  Iterator.Free;

(*  for i := 0 to NegativeSet.FFields.Count - 1 do
    Self.RemoveField(TDataField(NegativeSet.FFields.Items[i])); *)
end;

destructor TDataFieldSet.Destroy;
begin
  inherited Destroy;

  FFields.Free;
end;

procedure TDataFieldSet.CopyFrom(Fields : TDataFieldSet);
var
  Iterator : TIndexContainerIterator;
begin
  if Fields = Self then
    Exit;

  FFields.Clear;

  Iterator := TIndexContainerIterator.Create(Fields.FFields);
  while not Iterator.EOF do
  begin
    Self.AddField(TDataField(Iterator.Index));
    Iterator.Next;
  end;

  Iterator.Free;

(*  for i := 0 to Fields.FFields.Count - 1 do
    AddField(Fields.FFields.Items[i]);

//  FCurrDataTable := Fields.FCurrDataTable; *)
end;

procedure TDataFieldSet.AddFrom(Fields : TDataFieldSet);
var
  Iterator : TIndexContainerIterator;
begin
  if Fields = Self then
    Exit;

  Iterator := TIndexContainerIterator.Create(Fields.FFields);
  while not Iterator.EOF do
  begin
    Self.AddField(TDataField(Iterator.Index));
    Iterator.Next;
  end;

  Iterator.Free;

(*  for i := 0 to Fields.FFields.Count - 1 do
    if not Self.ContainsField(Fields.FFields.Items[i]) then
      AddField(Fields.FFields.Items[i]);

//  FCurrDataTable := nil; *)
end;

procedure TDataFieldSet.Clear;
begin
  FFields.Clear;
//  FCurrDataTable := nil;
end;

function TDataFieldSet.FieldCount : Integer;
begin
  Result := FFields.ItemCount;
end;

function TDataFieldSet.RowsEqual(Row1, Row2 : TAbstractRow) : Boolean;
var
  i : Integer;
  Iterator : TDataFieldSetIterator;
begin
  if Self = nil then
  begin
    if Row1.DataTable <> Row2.DataTable then
      raise Exception.Create('Datatables differ...'); // LGE: Drunk, fix later... ;)

    Result := True;

    for i := 0 to Row1.DataTable.FieldCount - 1 do
      if (not Row1.DataTable.Field[i].DataType.Equals(Row1.ValueByIndex[i], Row2.ValueByIndex[i])) then
      begin
        Result := False;
        Break;
      end;
  end
  else
  begin
    Result := True;
    Iterator := TDataFieldSetIterator.Create(Self);
    while not Iterator.EOF do
    begin
      if not Iterator.Field.DataType.Equals(Row1[Iterator.Field], Row2[Iterator.Field]) then
      begin
        Result := False;
        Break;
      end;
      Iterator.Next;
    end;
    Iterator.Free;
  end;
end;

(*
function TDataFieldSet.GetField(idx : Integer) : TDataField;
var
  i : Integer;
begin
  i := 0;
  Result := nil;

  while idx >= 0 do
  begin
    if i >= FFields.Count then
      Log(ltError, 'TDataFieldSet', 'TDataFieldSet.Field: Index out of bounds!')
    else if FFields.Items[i] <> nil then
    begin
      if idx = 0 then
      begin
        Result := TDataField(FFields.Items[i]);
        Exit;
      end
      else
      begin
        Dec(idx);
      end;
    end;

    Inc(i);
  end;
end;
*)

(*
procedure TDataFieldSet.ApplyToTable(DataTable : TDataTable);
var
  iField, idx : Integer;
begin
  if FCurrDataTable = DataTable then
    Exit;

  for iField := 0 to DataTable.FieldCount - 1 do
  begin
    idx := FFields.IndexOf(DataTable.Field[iField]);
    if idx = -1 then
    begin
      if FFields.Count = iField then
        FFields.Add(nil)
      else if FFields.Items[iField] <> nil then
        FFields.Insert(iField, nil);
    end
    else if idx > iField then
    begin
      FFields.Exchange(idx, iField);
    end;
  end;

  for iField := FFields.Count - 1 downto DataTable.FieldCount do
    if FFields.Items[iField] = nil then
      FFields.Delete(iField);

  FCurrDataTable := DataTable;
end;
*)

function TDataFieldSet.IncludeField(DataTable : TDataTable; Index : Integer) : Boolean;
begin
  if Self <> nil then
  begin
    (*ApplyToTable(DataTable);
    Result := FFields.Items[Index] <> nil;*)
{$ifdef D4_OR_HIGHER}
    Result := FFields.Contains(DataTable.Field[Index]);
{$else}
    Result := FFields.ContainsOL(DataTable.Field[Index]);
{$endif D4_OR_HIGHER}
  end
  else
  begin
    Result := True;
  end;
end;

function TDataFieldSet.IncludeFieldAcceptKeys(DataTable : TDataTable; Index : Integer) : Boolean;
begin
  if Index < DataTable.KeyCount then
    Result := True
  else
    Result := IncludeField(DataTable, Index);
end;

procedure TDataFieldSet.AddFieldsFromCondition(ACond : TCondition);
begin
  if ACond <> nil then
    ACond.ProcessFields(Self.AddConditionField);
end;

procedure TDataFieldSet.AddConditionField(Field : TDataField; Dummy : Boolean);
begin
  AddField(Field);
end;

procedure TDataFieldSet.AddField(Field : TDataField);
begin
  if not ContainsField(Field) then
  begin
{$ifdef D4_OR_HIGHER}
    FFields.Add(Field);
{$else}
    FFields.AddOL(Field);
{$endif D4_OR_HIGHER}
  end;
end;

procedure TDataFieldSet.AddFromFieldArray(Fields : array of TDataField);
var
  i : Integer;
begin
  for i := Low(Fields) to High(Fields) do
    if Fields[i] <> nil then
      AddField(Fields[i]);
end;

{$ifdef D4_OR_HIGHER}
procedure TDataFieldSet.AddFromTable(Table : TDataTable; Keys : Boolean = True; Fields : Boolean = True; IgnoreFields : Boolean = False);
{$else}
procedure TDataFieldSet.AddFromTable(Table : TDataTable; Keys, Fields, IgnoreFields : Boolean);
{$endif D4_OR_HIGHER}
var
  i : Integer;
begin
  if Keys then
    for i := 0 to Table.KeyCount - 1 do
      AddField(Table.Field[i]);

  if Fields then
    for i := Table.KeyCount to Table.FieldCount - 1 do
      AddField(Table.Field[i]);

  if IgnoreFields then
    for i := Table.FieldCount to Table.FieldCount + Table.IgnoreFieldCount- 1 do
      AddField(Table.Field[i]);
end;

procedure TDataFieldSet.RemoveField(Field : TDataField);
begin
{$ifdef D4_OR_HIGHER}
  FFields.Remove(Field);
{$else}
  FFields.RemoveOL(Field);
{$endif D4_OR_HIGHER}

(*  idx := FFields.IndexOf(Field);
  if idx >= 0 then
  begin
    FFields.Items[idx] := nil;
    // FCurrDataTable := nil; behövs inte!
  end; *)
end;

function TDataFieldSet.ContainsField(Field : TDataField) : Boolean;
begin
  if Self = nil then
    Result := True
  else
//    Result := FFields.IndexOf(Field) >= 0;
{$ifdef D4_OR_HIGHER}
    Result := FFields.Contains(Field);
{$else}
    Result := FFields.ContainsOL(Field);
{$endif D4_OR_HIGHER}
end;

function TDataFieldSet.ContainsAllFields(DataTable : TDataTable) : Boolean;
var
  i : Integer;
begin
  Result := True;

  for i := 0 to DataTable.FieldCount - 1 do
    if not Self.IncludeField(DataTable, i) then
    begin
      Result := False;
      Exit;
    end;
end;

procedure TDataFieldSet.ProcFields(Proc : TProcDataField);
begin
  FFields.ProcIndexContents(TProcIndex(Proc));
end;

{ TDataFieldSetIterator }

function TDataFieldSetIterator.GetField : TDataField;
begin
  Result := TDataField(FIterator.Index);
end;

constructor TDataFieldSetIterator.Create(ASet : TDataFieldSet);
begin
  inherited Create;
  if ASet <> nil then
    FIterator := TIndexContainerIterator.Create(ASet.FFields)
  else
    FIterator := nil;
end;

destructor TDataFieldSetIterator.Destroy;
begin
  inherited Destroy;
  FIterator.Free;
end;

procedure TDataFieldSetIterator.First;
begin
  if FIterator <> nil then
    FIterator.First;
end;

procedure TDataFieldSetIterator.Next;
begin
  if FIterator <> nil then
    FIterator.Next;
end;

function TDataFieldSetIterator.EOF : Boolean;
begin
  if FIterator <> nil then
    Result := FIterator.EOF
  else
    Result := True;
end;

// ---------------------------- TKeyField --------------------------------------

constructor TKeyField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsAggregable := False;

  FHasCombo := True;
  FAllowSelections := True;
  FIsReportable := True;

  FDisplayWidth := -1;
  FDependsOn := nil;
end;

constructor TKeyField.CreateNonAuxTabled(const FieldName : String; DataType : TDataType);
begin
  CreateOld(FieldName, DataType, [nil], dvKeyOnly, False, -1, nil);
end;

constructor TKeyField.CreateOld(const FieldName : String; DataType : TDataType;
                             TextFields : array of TDataField; DisplayValues : TDisplayValues; HasCombo : Boolean;
                             DisplayWidth : Integer; DependsOn : TDataField);
begin
  inherited CreateOld(FieldName, DataType);
  IsAggregable := False;
  FDisplayValues := DisplayValues;

  FHasCombo := HasCombo;
  FAllowSelections := True;
  FIsReportable := True;

  if DisplayWidth <> -1 then
    FDisplayWidth := DisplayWidth
  else
    FDisplayWidth := DataType.DataSize + 25;

  SetAllTextFields(TextFields);

  Self.DependParent := DependsOn;
end;

procedure TDataField.SetDependParent(AParent : TDataField);
begin
  if FDependsOn <> AParent then
  begin
    if FDependsOn <> nil then
      FDependsOn.RemoveDependentChild(Self);

    FDependsOn := AParent;

    if FDependsOn <> nil then
      FDependsOn.AddDependentChild(Self);
  end;
end;

procedure TDataField.SetAllTextFields(const TextFields : array of TDataField);
begin
  FTextFields.SetAllFields(TextFields);
  FShortTextFields.SetAllFields(TextFields);

{  FTextFields.Clear;
  FShortTextFields.Clear;
  for i := Low(TextFields) to High(TextFields) do
  begin
    FTextFields.Add(TextFields[i]);
    FShortTextFields.Add(TextFields[i]);
  end; }

//  FIsLanguageDependent := (Low(TextFields) < High(TextFields));
end;

(*
procedure TDataField.SetTextFields(TextFields, ShortTextFields : TList);
var
  i : Integer;
begin
  FTextFields.Count := TextFields.Count;
  for i := 0 to TextFields.Count - 1 do
    FTextFields.AddField(TextFields.Items[i]);

  FShortTextFields.Count := ShortTextFields.Count;
  for i := 0 to ShortTextFields.Count - 1 do
    FShortTextFields.AddField(ShortTextFields.Items[i]); }

{  FTextFields.Clear;
  FShortTextFields.Clear;

  if TextFields <> nil then
    for i := 0 to TextFields.Count - 1 do
      FTextFields.Add(TextFields.Items[i]);

  if ShortTextFields <> nil then
    for i := 0 to ShortTextFields.Count - 1 do
      FShortTextFields.Add(ShortTextFields.Items[i]); }

  FIsLanguageDependent := (TextFields.Count > 1) or (ShortTextFields.Count > 1);
end;
*)



{procedure TDataField.GetTextFields(TextFields, ShortTextFields : TList);
var
  i : Integer;
begin
  for i := 0 to Singletons.LanguageCount - 1 do
  begin
    if i < FTextFields.Count then
      TextFields.Add(FTextFields.Items[i])
    else
      TextFields.Add(nil);

    if i < FShortTextFields.Count then
      ShortTextFields.Add(FShortTextFields.Items[i])
    else
      ShortTextFields.Add(nil);
  end;
end;
}
constructor TKeyField.CreateWithSortField(const FieldName : String; DataType : TDataType;
                                          TextFields : array of TDataField; DisplayValues : TDisplayValues; HasCombo : Boolean;
                                          DisplayWidth : Integer; DependsOn : TDataField;
                                          SortField : TDataField; SortOrder : TSortOrder);
begin
  CreateOld(FieldName, DataType, TextFields, DisplayValues, HasCombo, DisplayWidth, DependsOn);

  if SortField = nil then
    FSortField := Self
  else
    FSortField := SortField;
  FSortOrder := SortOrder;
end;

constructor TKeyField.CreateDependent(const FieldName : String; AuxTableField : TDataField; Criteria : TCondition; LookupKey : TDataField);
var
  MySortField : TDataField;
  FreeQuilt : Boolean;
begin
  Assert(AuxTableField <> nil, 'TKeyField.CreateDependent: AuxTableField <> nil');

(*  if (Criteria <> nil) and not (Criteria is TCriteria) then
    raise Exception.Create('Criteria expected!');
*)
  if AuxTableField.SortField = AuxTableField then
    MySortField := Self
  else
    MySortField := AuxTableField.SortField;

  CreateWithSortField(FieldName, AuxTableField.DataType, [nil],
                      AuxTableField.DisplayValues, AuxTableField.FHasCombo,
                      AuxTableField.DisplayWidth, AuxTableField.DependParent,
                      MySortField, AuxTableField.SortOrder);

//  SetTextFields(AuxTableField.FTextFields, AuxTableField.FShortTextFields);
  Self.TextFields := AuxTableField.TextFields;
  Self.ShortTextFields := AuxTableField.ShortTextFields;

  FDirectAuxTableField := AuxTableField;
  FAuxTableField := AuxTableField.AuxTableField;
  AuxTableField.FDerivedFields.Add(Self);

  if (Criteria = nil) and (AuxTableField.Criteria <> nil) then
    Criteria := AuxTableField.Criteria.CreateCopy;

  if Criteria = nil then
    FCriteria := nil
  else
  begin
    FCriteria := Criteria.GetCommonQuilt(FreeQuilt);
    if FreeQuilt then
      Criteria.Free;
  end;

  FDependentLookupField := LookupKey;

  // if AuxTableField.AuxTable <> nil then
  Self.SetAuxTable(AuxTableField.AuxTable);

  // Fixa LGE blir lookuptable alltid satt ifall detta fält skapas efter tabellen?
end;

destructor TKeyField.Destroy;
begin
  inherited Destroy;
end;

function TDataField.GetDerivedFieldCount : Integer;
begin
  Result := FDerivedFields.Count;
end;

function TDataField.GetDerivedField(idx : Integer) : TDataField;
begin
  Assert((idx >= 0) and (idx < FDerivedFields.Count),
         'TKeyField.GetDerivedField: (idx >= 0) and (idx < FDerivedFields.Count), idx: ' + IntToStr(idx));

  Result := TDataField(FDerivedFields.Items[idx]);
end;

function TDataField.IsDerivedWithCriteria : Boolean;
begin
  if AuxTableField = nil then
    Result := False
  else if AuxTableField = Self then
    Result := False
  else if Criteria = nil then
    Result := False
  else
    Result := not FCriteria.AcceptsAll;
end;

procedure TDataField.AddDependentChild(Child : TDataField);
begin
  FDependentChildren.Add(Child);
end;

procedure TDataField.RemoveDependentChild(Child : TDataField);
begin
  FDependentChildren.Remove(Child);
end;

function TDataField.GetDependentChildCount : Integer;
begin
  Result := FDependentChildren.Count;
end;

function TDataField.GetDependentChild(idx : Integer) : TDataField;
begin
  Assert((idx >= 0) and (idx < FDependentChildren.Count),
         'TKeyField.GetDependentChild: (idx >= 0) and (idx < FDependentChildren.Count), idx: ' + IntToStr(idx));

  Result := TDataField(FDependentChildren.Items[idx]);
end;

function TDataField.IsDependentField(AKeyField : TDataField) : Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 0 to DependentFieldCount - 1 do
  begin
    if AKeyField = DependentField[i] then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TDataField.DependentHasOneValue(ADependentField : TDataField) : Boolean;
var
  AValue : TValue;
//  ACritField : TCriteriaField;
begin
  if Self.Criteria <> nil then
  begin
    Result := Criteria.AcceptsExactlyOneValue(ADependentField, AValue);
(*    ACritField := TCriteriaLink(Self.Criteria).FindCriteriaField(ADependentField, False);
    Result := (ACritField <> nil) and ACritField.HasExactlyOneValue;*)
  end
  else
    Result := False;
end;

function TDataField.GetDependentFieldCount : Integer;
var
  i : Integer;
begin
  Result := 0;

  if HasAuxtable then
  for i := AuxTable.KeyCount to AuxTable.FieldCount - 1 do
    if (not DependentHasOneValue(AuxTable.Field[i])) then
      Inc(Result);
end;

function TDataField.GetDependentField(idx : Integer) : TDataField;
var
  i, iDep : Integer;
  AField : TDataField;
begin
  Assert(idx >= 0, 'TKeyField.GetDependentField: idx >= 0, idx: ' + IntToStr(idx));

  Result := nil;

  if HasAuxtable then
    for i := AuxTable.KeyCount to AuxTable.FieldCount - 1 do
    begin
      AField := AuxTable.Field[i];
      if not DependentHasOneValue(AField) then
      begin
        if idx = 0 then
        begin
          Result := AField;
          for iDep := 0 to Result.DerivedFieldCount-1 do
            if Result.DerivedField[iDep].DependentLookupField = Self then
            begin
              result := Result.DerivedField[iDep];
              break;
            end;

          Break;
        end
        else
          Dec(idx);
      end;
    end;
end;

function TDataField.GetDependentFieldByDerivedField(AField : TDataField) : TDataField; // Fixa LGE bättre namn sku va på plats...
var
  i, iDep : Integer;
  DataField : TDataField;
  Accept : Boolean;
begin
  Result := nil;

  if HasAuxtable then
    for i := AuxTable.KeyCount to AuxTable.FieldCount - 1 do
    begin
      DataField := AuxTable.Field[i];
      if not DependentHasOneValue(AField) then
      begin
        Accept := (DataField = AField);
        if (DataField.DependentLookupField = Self) or
           ((DataField.DependentLookupField = nil) and (Self.AuxTableField = Self)) then
          Result := DataField;

        for iDep := 0 to DataField.DerivedFieldCount-1 do
        begin
          if DataField.DerivedField[iDep] = AField then
            Accept := True;

          if (DataField.DerivedField[iDep].DependentLookupField = Self) or
             ((DataField.DerivedField[iDep].DependentLookupField = nil) and (Self.AuxTableField = Self)) then
            Result := DataField.DerivedField[iDep];
        end;

        if Accept then
          Break
        else
          Result := nil;
      end;
    end;
end;

function TDataField.GetAncestorCount : Integer;
var
  KeyField : TDataField;
begin
  Result := 0;
  KeyField := Self.DependParent;
  while KeyField <> nil do
  begin
    Inc(result);
    KeyField := KeyField.DependParent;
  end;
end;

function TDataField.GetAncestor(idx : Integer) : TDataField;
var
  ParentCount : Integer;
begin
  ParentCount := GetAncestorCount;

  Result := Self;
  while (result <> nil) and (idx < ParentCount) do
  begin
    Result := Result.DependParent;
    Inc(idx);
  end;
end;

function TDataField.GetHasAuxTable : Boolean;
begin
  Result := FAuxTable <> nil;
end;

function TDataField.GetValuesToMethod(GV : TGetValues) : TMethod;
begin
  Result := TMethod(GV);
end;

function TDataField.GetValuesOverridden : Boolean;
begin
  Result := (GetValuesToMethod(Self.GetValues).Code <> GetValuesToMethod(FieldValue.GetValues).Code);
end;

function TDataField.DoDependentRefill(CurrentCondition, FillCondition: TCondition): Boolean;
var
  FreeCurr, FreeFill : Boolean;
  CurrentQuilt, FillQuilt : TCommonQuilt;
  AuxFields : TDataFieldSet;
begin
  if HasAuxtable and
     Assigned(CurrentCondition) and
     // if we have subclassed GetValues -> do not care about auxFields...
     // FieldValue is just used as a reference and MUST have ClassType = TDataField (not a descendent!)
     not GetValuesOverridden then
//     (GetValuesToMethod(Self.GetValues).Code = GetValuesToMethod(FieldValue.GetValues).Code) then
  begin
    if not Assigned(FillCondition) then
      Result := True
    else
    begin
      CurrentQuilt := CurrentCondition.GetCommonQuilt(FreeCurr);
      FillQuilt := FillCondition.GetCommonQuilt(FreeFill);

      AuxFields := TDataFieldSet.Create;
      AuxFields.AddFromTable(AuxTable);
      Result := CurrentQuilt.Compare( FillQuilt, AuxFields) <> crEqual;
      AuxFields.Free;

      if FreeCurr then
        CurrentQuilt.Free;

      if FreeFill then
        FillQuilt.Free;
    end;
  end
  else
    Result := True;
end;

function TDataField.DoTimeStampRefill( FillTimeStamp: TDateTime): Boolean;
begin
  if HasAuxtable and
     (FillTimeStamp > 0) then
    Result := FillTimeStamp < Auxtable.Cache.LastChanged
  else
    Result := False;
end;

function TDataField.DoRefill( CurrentCondition, FillCondition : TCondition;
  FillTimeStamp : TDateTime ) : Boolean;
begin
  case RefillType of
    rtNever : Result := DoTimeStampRefill(FillTimeStamp);
    rtDependentChange : Result := DoTimeStampRefill(FillTimeStamp) or DoDependentRefill(CurrentCondition, FillCondition);
  else {rtAlways}
    Result := True;
  end;
end;

procedure TDataField.SetRefillType(const Value: TRefillType);
begin
  FRefillType := Value;
end;

procedure TDataField.GetDefaultTextField(Sender : TLangFieldArray; var Field : TDataField);
begin
  Field := TextFields.Field;
end;

procedure TDataField.GetValues(Results : TStrings; Table : TDataTable; Row : TAbstractRow; Condition : TCondition);
begin
  GetRows(Results, Condition);
end;

procedure TDataField.GetRows(Results : TStrings; Condition : TCondition);
var
  AppliedCondition : TCondition;

begin
  Assert(Results <> nil, 'TDataField.GetRows: Results <> nil');

  if not Self.HasAuxTable then
    Exit;

  if Condition = nil then
    Self.AuxTable.Cache.GetRows(Results, Self.Criteria, gaReference)
  else if Self.AuxTableField = Self then
    Self.AuxTable.Cache.GetRows(Results, Condition, gaReference)
  else
  begin
    AppliedCondition := Condition.CreateFieldTranslatedCopy(Self.GetRowsFieldTranslator);

    if Self.Criteria <> nil then
    begin
      if AppliedCondition = nil then
        Self.AuxTable.Cache.GetRows(Results, Self.Criteria, gaReference)
      else
      begin

      end;
    end
    else
      Self.AuxTable.Cache.GetRows(Results, AppliedCondition, gaReference);
    AppliedCondition.Free;
  end;
end;

procedure TDataField.GetRowsByDependeeFields(Results : TStrings; Condition : TCondition);

begin
  Assert(Results <> nil, 'TDataField.GetRows: Results <> nil');

  if not Self.HasAuxTable then
    Exit;

  if Self.Criteria = nil then
  begin
    Self.AuxTable.Cache.GetRows(Results, Condition, gaReference);
  end
  else if Condition = nil then
  begin
    Self.AuxTable.Cache.GetRows(Results, Self.Criteria, gaReference);
  end
  else
  begin

  end;
end;

function TDataField.GetTextField : TDataField;
begin
  if TextFields = nil then
    Result := nil
  else
    Result := TextFields.Field;

(*  if FTextFields.Count = 0 then
    Result := nil
  else if (not IsLanguageDependent) or (Singletons.LanguageIndex >= FTextFields.Count) then
    Result := TDataField(FTextFields.Items[0])
  else
    Result := TDataField(FTextFields.Items[Singletons.LanguageIndex]); *)
end;

function TDataField.GetShortTextField : TDataField;
begin
  Result := ShortTextFields.Field;

(*  if FShortTextFields.Count = 0 then
    Result := nil
  else if (not IsLanguageDependent) or (Singletons.LanguageIndex >= FShortTextFields.Count) then
    Result := TDataField(FShortTextFields.Items[0])
  else
    Result := TDataField(FShortTextFields.Items[Singletons.LanguageIndex]); *)
end;

procedure TDataField.SetAuxTable(Table : TAuxTable);
var
  i : Integer;
begin
  if Table <> nil then
  begin
    if not Table.TableHasKey(AuxTableField) then
      Log(ltError, 'SetAuxTable', Table.TableName + ' doesn''t have ' + AuxTableField.FieldName + ' as a key!');
  end;

  FAuxTable := Table;

  for i := 0 to Self.DerivedFieldCount - 1 do
    Self.DerivedField[i].AuxTable := Table;
end;

function TDataField.GetIntervals : Boolean;
begin
  Result := (not FDisableIntervals) and
            (Self.SortField = Self);
end;

procedure TDataField.SetShortDescriptions(ADesr : TLangArray);
begin
  FShortDescriptions.Assign(ADesr);
end;

procedure TDataField.SetLongDescriptions(ADesr : TLangArray);
begin
  FLongDescriptions.Assign(ADesr);
end;

procedure TDataField.SetTextFields(AFields : TLangFieldArray);
begin
  FTextFields.Assign(AFields);
end;

procedure TDataField.SetShortTextFields(AFields : TLangFieldArray);
begin
  FShortTextFields.Assign(AFields);
end;

procedure TDataField.SetAuxTableField(AField : TDataField);
begin
  if FDirectAuxTableField = AField then
    Exit;

  if FDirectAuxTableField <> nil then
    FDirectAuxTableField.FDerivedFields.Remove(Self);

  if AField = Self then
  begin
    FAuxTableField := Self;
    FDirectAuxTableField := Self;
    Self.AuxTable := nil
  end
  else
  begin
    FDirectAuxTableField := AField;
    if AField <> nil then
    begin
      FAuxTableField := AField.AuxTableField;
      AField.FDerivedFields.Add(Self);
      Self.AuxTable := AField.AuxTable;
//      Self.SetTextFields(AField.FTextFields, AField.FShortTextFields);
      Self.TextFields := AField.TextFields;
      Self.ShortTextFields := AField.ShortTextFields;
      
      Self.DisplayValues := AField.DisplayValues;
      Self.HasCombo := AField.HasCombo;
      Self.DisplayWidth := AField.DisplayWidth; // Fixa LGE vill vi ännu sätta nå andra properties?
      Self.DisableIntervals := AField.DisableIntervals;
    end
    else
    begin
      FAuxTableField := Self;
      Self.AuxTable := nil;
    end;
  end;
end;

(*
procedure TDataField.SetIsLanguageDependent(AValue : Boolean);
begin
  FIsLanguageDependent := AValue;
end;

function TDataField.GetIsLanguageDependent : Boolean;
begin
  Result := FIsLanguageDependent;
end;
*)

function TKeyField.ValueDefined(ARow : TDataRow) : Boolean;
begin
  if not HasAuxTable then
    Result := True
  else if (Criteria <> nil) and
          (not Criteria.AcceptsRow(ARow)) then
    Result := False
  else
  try
    Result := AuxTable.Cache.LocateByRowValues(ARow, [Self]) <> nil;
  except
    Log(ltError, 'ValueDefined', 'TKeyField.ValueDefined' + ARow.DataTable.TableName + 'Row cannot be checked for KeyField ' + Self.FieldName);
    Result := False;
  end;
end;

function TDataField.GetRowsFieldTranslator(Field : TDataField; var ConflictAction : TConflictAction; var KeepOld : Boolean) : TDataField;
var
  i, iDep {, iDerivedAuxField} : Integer;
begin
  ConflictAction := caIntersection;
  Result := Field;
  KeepOld := False;

  if HasAuxTable then
  begin
    for i := 0 to Self.AuxTable.KeyCount - 2 do // Keep criteria on higher keys
      if Field = Self.AuxTable.Field[i] then
        Exit;

    if Field = Self then                        // Use field in auxtable instead of dependent field
    begin
      Result := Self.AuxtableField;
      Exit;
    end
    else if Field = Self.AuxtableField then     // don't use CC criteria in RC.GetValues
    begin
      Result := nil;
      Exit;
    end;

    for i := 0 to Self.DependentFieldCount - 1 do // convert dep.field -> field ONLY IF it's in auxtable
      if Field = Self.DependentField[i] then
      begin
        if AuxTable.TableHasField(Self.DependentField[i].AuxTableField) then
          Result := Self.DependentField[i].AuxTableField;
        Exit;
      end;

    for i := Self.Auxtable.KeyCount to Self.AuxTable.FieldCount - 1 do // Remove ORG1 crit if ORG1RC is dependent
      if (Field = Self.AuxTable.Field[i]) then
      begin
        for iDep := 0 to Self.DependentFieldCount - 1 do
          if (Field = Self.DependentField[iDep].AuxTableField) and
             (Field <> Self.DependentField[iDep]) and
             AuxTable.TableHasField(Self.DependentField[i].AuxTableField) then
          begin
            Result := nil;
            Exit;
          end;

        Exit;

(*        // For derived fields, we have to check that we primarly use the dependent fields
        // for the field in question, rather than the dependent field of the auxtable field
        // E.g. when self is RC don't return ORG1 for field ORG1 since there is a ORG1RC
        // as a derived field, but nil instead. Otherwise selections on ORG1 would
        // implicitely mean ORG1RC selections.
        if (Self.AuxTableField <> Self) then
          for iDerivedAuxField := 0 to Self.AuxTableField.DerivedFieldCount -1 do
            if Self.AuxTableField.DerivedField[iDerivedAuxField] = Self then
            begin
              Result := nil;
              Break;
            end;

        Exit; *)
      end;
  end;
end;

{function TDataField.GetRowsFieldTranslator(Field : TDataField; var ConflictAction : TConflictAction; var KeepOld : Boolean) : TDataField;
var
  i : Integer;
begin
  ConflictAction := caIntersection;
  Result := Field;
  KeepOld := False;

  if HasAuxTable then
  begin
    for i := 0 to Self.AuxTable.KeyCount - 2 do
      if Field = Self.AuxTable.Field[i] then
        Exit;

    if Field = Self then
    begin
      Result := Self.AuxTableField;
      Exit;
    end;

    for i := 0 to Self.DependentFieldCount - 1 do
      if Field = Self.DependentField[i] then
      begin
        Result := Self.DependentField[i].AuxTableField;
        Exit;
      end;

    for i := Self.Auxtable.KeyCount to Self.AuxTable.FieldCount - 1 do
      if Field = Self.AuxTable.Field[i] then
        Exit;

    Result := nil;
  end;
end;}

// -------------------------- TRunningNumberField ------------------------------

constructor TRunningNumberField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRunningNumberGenerator := FDefaultRunningNumberGenerator;
  IsAggregable := False;
end;

constructor TRunningNumberField.CreateOld(const FieldName : String; DataType : TDataType;
                                          TextFields : array of TDataField; DisplayValues : TDisplayValues;
                                          HasCombo : Boolean;
                                          DisplayWidth : Integer; DependsOn : TDataField);
begin
  inherited CreateOld(FieldName, DataType, TextFields, DisplayValues, HasCombo, DisplayWidth, DependsOn);
  FRunningNumberGenerator := FDefaultRunningNumberGenerator;
  IsAggregable := False;
end;

constructor TRunningNumberField.CreateWithSortField(FieldName : String; DataType : TDataType;
                                    TextFields : array of TDataField; DisplayValues : TDisplayValues;
                                    HasCombo : Boolean;
                                    DisplayWidth : Integer; DependsOn : TDataField;
                                    SortField : TDataField; SortOrder : TSortOrder);
begin
  inherited CreateWithSortField(FieldName, DataType, TextFields, DisplayValues, HasCombo,
                                DisplayWidth, DependsOn, SortField, SortOrder);
  FRunningNumberGenerator := FDefaultRunningNumberGenerator;
  IsAggregable := False;
end;

destructor TRunningNumberField.Destroy;
begin
  inherited Destroy;
end;

procedure TRunningNumberField.SetRunningNumberGenerator(ARNG : TRunningNumberGenerator);
begin
  if ARNG = nil then
    FRunningNumberGenerator := FDefaultRunningNumberGenerator
  else
    FRunningNumberGenerator := ARNG;
end;

// ----------------------------- TDataField ------------------------------------

function TDataField.GetReadOnly(ARow : TAbstractRow) : Boolean;
begin
  Result := FDefaultReadOnly;
end;

constructor TDataField.DataFieldCommonCreate(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  FIsLanguageDependent := False;

  FDerivedFields := TList.Create;
  FDependentChildren := TList.Create;
  FDisableIntervals := False;
  FTablesUsingMe := TIndexContainer.Create(HASHSIZE, False);

  FDependentLookupField := nil;
  FCriteria := nil;

  FSortField := Self;
  FSortOrder := soAscending;

  FCanGenerateCondSQL := True;
  FCanGenerateValueSQL := True;

  FDirectAuxTableField := Self;
  FAuxTableField := Self;

  FDefaultReadOnly := False;
  if (DataType is TStringType) then
    FDisplayValues := dvKeyAndText
  else
  begin
    FDisplayValues := dvDefault;
    FDisplayValues := GetDefaultDisplayValues;
  end;

  FFieldName := '';
  FDataType := nil;

{  FShortDescriptions := TStringList.Create;
  FLongDescriptions := TStringList.Create; }

  FShortDescriptions := TLangArray.Create(FieldName);
  FShortDescriptions.OnGetCurrentLanguage := GetCurrentLanguageIndex;
  FLongDescriptions := TLangArray.Create(FieldName);
  FLongDescriptions.OnGetCurrentLanguage := GetCurrentLanguageIndex;

{  FTextFields := TList.Create;
  FShortTextFields := TList.Create; }

  FTextFields := TLangFieldArray.Create;
  FTextFields.OnGetCurrentLanguage := GetCurrentLanguageIndex;
  FShortTextFields := TLangFieldArray.Create;
  FShortTextFields.OnGetCurrentLanguage := GetCurrentLanguageIndex;
  FShortTextFields.OnGetDefaultField := GetDefaultTextField;

  // Default behaviour
  FIsAggregable := False;
  FDisplayWidth := -1;

  FHasCombo := False;
  FAllowSelections := False;
  FIsReportable := False;

  // Default
  FLookupTable := nil;
  FDataType := StringType(30, True); //???
  FDefaultValue := FDataType.DefaultValue;

  FRefillType := rtDependentChange;
end;

constructor TDataField.Create(AOwner: TComponent);
begin
  DataFieldCommonCreate(AOwner);
end;

constructor TDataField.CreateOld(const FieldName : String; DataType : TDataType);
begin
  DataFieldCommonCreate(nil);

  FFieldName := FieldName;

  FShortDescriptions.DefaultTextValue := FieldName;
  FLongDescriptions.DefaultTextValue := FieldName;
  HelpTexts.DefaultTextValue := FieldName;

  FDataType := DataType;
  if DataType <> nil then
    FDefaultValue := DataType.DefaultValue
  else
    FDefaultValue := EmptyString;

  // Default behaviour
  FIsAggregable := DataType.DefaultAggregable;
  FDisplayWidth := DataType.DisplayWidth;

  // Default descriptions
{  for i := 0 to Singletons.LanguageCount - 1 do
  begin
    SetShortDescription(i, FieldName);
    SetLongDescription(i, FieldName);
  end; }

  try
    if FieldName <> '' then
    begin
      if UFieldList = nil then
        CreateUList(UFieldList);
      UFieldList.AddValue('', ValueFromString(FieldName), Self);
    end;
  except
    Log(ltWarning, 'Duplicates', 'Duplicate fields: ' + FieldName);
  end;
end;

destructor TDataField.Destroy;
var
  idx : Integer;
begin
  inherited Destroy;

  FTablesUsingMe.Free; // do magic...

  FCriteria.Free;
  FDependentChildren.Free;
  FShortDescriptions.Free;
  FLongDescriptions.Free;
  FTextFields.Free;
  FShortTextFields.Free;

  if (UFieldList <> nil) then
  begin
    idx := UFieldList.IndexOfValue(ValueFromString(Self.FieldName));
    if (idx = -1) or (UFieldList.Objects[idx] <> Self) then
      idx := UFieldList.IndexOfObject(Self);

    if idx >= 0 then
      UFieldList.Delete( idx );
  end;

  for idx := 0 to FDerivedFields.Count - 1 do
    TDataField(FDerivedFields.Items[idx]).FDirectAuxTableField := FDerivedFields.Items[idx];

  if (FDirectAuxTableField <> nil) and (FDirectAuxTableField <> Self) then
    FDirectAuxTableField.FDerivedFields.Remove(Self);

  FDerivedFields.Free;
end;

procedure TDataField.SetName(const NewName: TComponentName);
var
  FUpdate : Boolean;
  FOldName, FFieldPrefix : String;
begin

    FFieldPrefix := '';
  FUpdate :=  ( FieldName = Name )
              or
              ( ( FFieldPrefix = Copy(Name, 1, Length(FFieldPrefix)) ) and
                ( FieldName = Copy(Name, Length(FFieldPrefix) + 1, Length(Name)) ) );
  if FUpdate then
    FOldName := Name;

  inherited SetName(NewName);

  if FUpdate and (Name <> FOldName) then
  begin
    if FFieldPrefix = Copy(Name, 1, Length(FFieldPrefix)) then
      FieldName := Copy(Name, Length(FFieldPrefix) + 1, Length(Name))
    else
      FieldName := Name;
  end;
end;

procedure TDataField.SetFieldName(NewName : String);
var
  Index : Integer;
  FOldFieldName : String;
begin
  if UFieldList = nil then
    CreateUList(UFieldList)
  else if UFieldList.Find(ValueFromString(FFieldName), Index) then
    UFieldList.Delete(Index)
  else
  begin
    Index := UFieldList.IndexOfObject(Self);
    if Index >= 0 then
      UFieldList.Delete(Index);
  end;

  FOldFieldName := FFieldName;
  FFieldName := NewName;
  if FOldFieldName <> NewName then
  begin
    if FShortDescriptions.DefaultTextValue = FOldFieldName then
      FShortDescriptions.DefaultTextValue := NewName;
    if FLongDescriptions.DefaultTextValue = FOldFieldName then
      FLongDescriptions.DefaultTextValue := NewName;
    if HelpTexts.DefaultTextValue = FOldFieldName then
      HelpTexts.DefaultTextValue := NewName;

  {  for i := 0 to FLongDescriptions.Count - 1 do
      if (FLongDescriptions.Strings[i] = FOldFieldName) then
        FLongDescriptions.Strings[i] := NewName;
    for i := 0 to FShortDescriptions.Count - 1 do
      if (FShortDescriptions.Strings[i] = FOldFieldName) then
        FShortDescriptions.Strings[i] := NewName; }
  end;
  try
    if NewName <> '' then
      UFieldList.AddValue('', ValueFromString(NewName), Self);
  except
    Log(ltWarning, 'Duplicates', 'Duplicate fields: ' + NewName);
  end;
end;

procedure TDataField.DefineProperties(Filer: TFiler);
begin

  inherited DefineProperties(Filer);
end;





































procedure TDataField.SetAllDescriptions(const Descriptions : array of String);
begin
  SetDescriptions(Descriptions, Descriptions);
end;

procedure TDataField.SetDescriptions(const ShortDescriptions, LongDescriptions : array of String);
{var
  i, sl, ll : Integer; }
begin
  FShortDescriptions.SetAllTexts(ShortDescriptions);
  FLongDescriptions.SetAllTexts(LongDescriptions);

{  sl := Low(ShortDescriptions);
  ll := Low(LongDescriptions);

  for i := 0 to Singletons.LanguageCount - 1 do
  begin
    if sl + i <= High(ShortDescriptions) then
      SetShortDescription(i, ShortDescriptions[sl+i])
    else
      SetShortDescription(i, ShortDescriptions[sl]);

    if ll + i <= High(LongDescriptions) then
      SetLongDescription(i, LongDescriptions[ll+i])
    else
      SetLongDescription(i, LongDescriptions[ll]);
  end; }
end;

procedure TDataField.SetDisplayWidth(AWidth : Integer);
begin
  FDisplayWidth := AWidth;
end;

{
function TDataField.GetShortDescByIndex(Language : Integer) : String;
begin
  Assert(((Language >= 0) and (Language < Singletons.LanguageCount)), 'TDataField.GetShortDescByIndex: ((Language >= 0) and (Language < Singletons.LanguageCount))' +', Language := ' +IntToStr(Language) +', LanguageCount := ' +IntToStr(Singletons.LanguageCount));

  if Language < FShortDescriptions.Count then
    Result := FShortDescriptions.Strings[Language]
  else if FShortDescriptions.Count > 0 then
    Result := FShortDescriptions.Strings[0]
  else
    Result := FieldName;
end;
}

{
function TDataField.GetLongDescByIndex(Language : Integer) : String;
begin
  Assert(((Language >= 0) and (Language < Singletons.LanguageCount)), 'TDataField.GetLongDescByIndex: ((Language >= 0) and (Language < Singletons.LanguageCount))' +', Language := ' +IntToStr(Language) +', LanguageCount := ' +IntToStr(Singletons.LanguageCount));

  if Language < FLongDescriptions.Count then
    Result := FLongDescriptions.Strings[Language]
  else if FLongDescriptions.Count > 0 then
    Result := FLongDescriptions.Strings[0]
  else
    Result := FieldName;
end;
}

{
function TDataField.GetTextFieldByIndex(Language : Integer) : TDataField;
begin
  Assert(((Language >= 0) and (Language < Singletons.LanguageCount)), 'TDataField.GetTextFieldByIndex: ((Language >= 0) and (Language < Singletons.LanguageCount))' +', Language := ' +IntToStr(Language) +', LanguageCount := ' +IntToStr(Singletons.LanguageCount));
  if Language < FTextFields.Count then
    Result := TDataField(FTextFields.Items[Language])
  else if FTextFields.Count > 0 then
    Result := TDataField(FTextFields.Items[0])
  else
    Result := nil;
end;

function TDataField.GetShortTextFieldByIndex(Language : Integer) : TDataField;
begin
  Assert(((Language >= 0) and (Language < Singletons.LanguageCount)), 'TDataField.GetShortTextFieldByIndex: ((Language >= 0) and (Language < Singletons.LanguageCount))' +', Language := ' +IntToStr(Language) +', LanguageCount := ' +IntToStr(Singletons.LanguageCount));
  if Language < FShortTextFields.Count then
    Result := TDataField(FShortTextFields.Items[Language])
  else if FShortTextFields.Count > 0 then
    Result := TDataField(FTextFields.Items[0])
  else
    Result := nil;
end;
}

procedure TDataField.CopyDescriptionsFrom(AField : TDataField);
{var
  i : Integer; }
begin
  Self.ShortDescriptions := AField.ShortDescriptions;
  Self.LongDescriptions := AField.LongDescriptions;


{  for i := 0 to AField.FShortDescriptions.Count - 1 do
  begin
    if Self.FShortDescriptions.Count > i then
      Self.FShortDescriptions.Strings[i] := AField.FShortDescriptions.Strings[i]
    else
      Self.FShortDescriptions.Add(AField.FShortDescriptions.Strings[i]);
  end;
  for i := 0 to AField.FLongDescriptions.Count - 1 do
  begin
    if Self.FLongDescriptions.Count > i then
      Self.FLongDescriptions.Strings[i] := AField.FLongDescriptions.Strings[i]
    else
      Self.FLongDescriptions.Add(AField.FLongDescriptions.Strings[i]);
  end; }
end;

function TDataField.GetShortDescription : String;
begin
  Result := FShortDescriptions.Text;

{  if Singletons.LanguageIndex < FShortDescriptions.Count then
    Result := FShortDescriptions.Strings[Singletons.LanguageIndex]
  else
    Result := ''; }
end;

function TDataField.GetLongDescription : String;
begin
  Result := FLongDescriptions.Text;

{  if Singletons.LanguageIndex < FLongDescriptions.Count then
    Result := FLongDescriptions.Strings[Singletons.LanguageIndex]
  else
    Result := ''; }
end;

(*
{ifdef VER130}
function TDataField.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
const
  E_NOINTERFACE = HResult($80004002);
{else}
function TDataField.QueryInterface(const IID: TGUID; out Obj): Integer; stdcall;
const
  E_NOINTERFACE = $80004002;
{endif VER130}
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

function TDataField._AddRef: Integer; stdcall;
begin
  result := 1;
end;

function TDataField._Release: Integer; stdcall;
begin
  result := 1;
end;
*)

{
procedure TDataField.SetShortDescription(Language : Integer; const Descr : String);
var
  FDefaultString : String;
begin
  Assert(((Language >= 0) and (Language < Singletons.LanguageCount)), 'TDataField.SetShortDescription: ((Language >= 0) and (Language < Singletons.LanguageCount))' +', Language := ' +IntToStr(Language) +', LanguageCount := ' +IntToStr(Singletons.LanguageCount));
  if (FShortDescriptions.Count > 0) then
    FDefaultString := FShortDescriptions.Strings[0]
  else
    FDefaultString := FFieldName;
  while FShortDescriptions.Count <= Language do
    FShortDescriptions.Add(FDefaultString);
  FShortDescriptions.Strings[Language] := Descr;
end;

procedure TDataField.SetLongDescription(Language : Integer; const Descr : String);
var
  FDefaultString : String;
begin
  Assert(((Language >= 0) and (Language < Singletons.LanguageCount)), 'TDataField.SetLongDescription: ((Language >= 0) and (Language < Singletons.LanguageCount))' +', Language := ' +IntToStr(Language) +', LanguageCount := ' +IntToStr(Singletons.LanguageCount));
  if (FLongDescriptions.Count > 0) then
    FDefaultString := FLongDescriptions.Strings[0]
  else
    FDefaultString := FFieldName;
  while FLongDescriptions.Count <= Language do
    FLongDescriptions.Add(FDefaultString);
  FLongDescriptions.Strings[Language] := Descr;
end;
}

{
procedure TDataField.SetTextField(Language : Integer; AField : TDataField);
var
  FDefaultField : TDataField;
begin
  Assert(((Language >= 0) and (Language < Singletons.LanguageCount)), 'TDataField.SetTextField: ((Language >= 0) and (Language < Singletons.LanguageCount))' +', Language := ' +IntToStr(Language) +', LanguageCount := ' +IntToStr(Singletons.LanguageCount));

  if FTextFields.Count > 0 then
    FDefaultField := TDataField(FTextFields.Items[0])
  else
    FDefaultField := nil;
  while FTextFields.Count <= Language do
    FTextFields.Add(FDefaultField);
  FTextFields.Items[Language] := AField;
end;

procedure TDataField.SetShortTextField(Language : Integer; AField : TDataField);
var
  FDefaultField : TDataField;
begin
  Assert(((Language >= 0) and (Language < Singletons.LanguageCount)), 'TDataField.SetShortTextField: ((Language >= 0) and (Language < Singletons.LanguageCount))' +', Language := ' +IntToStr(Language) +', LanguageCount := ' +IntToStr(Singletons.LanguageCount));
  if FShortTextFields.Count > 0 then
    FDefaultField := TDataField(FShortTextFields.Items[0])
  else
    FDefaultField := nil;
  while FShortTextFields.Count <= Language do
    FShortTextFields.Add(FDefaultField);
  FShortTextFields.Items[Language] := AField;
end;
}

function TDataField.FindDerivedSource(ADataField : TDataField) : TDataField;
begin
  if ADataField = nil then
    Result := nil
  else if Self = ADataField then
    Result := ADataField
  else
    Result := FindDerivedSource(ADataField.LookupField);
end;

function TDataField.GetCanBeInDB : Boolean;
begin
  Result := DataType.CanBeInDB;
end;

procedure TDataField.SetDataType(NewDataType : TDataType);
begin
//  ShowMessage('TDataField.SetDataType(NewDataType)');
  if NewDataType <> FDataType then
  begin
    // Fixa LGE här måste vi pinga alla befintliga tables och storages...
    FDataType := NewDataType;
    if NewDataType <> nil then
      FDefaultValue := NewDataType.DefaultValue;
    Modified; //MAB refreshar displayen med denna
  end;
end;

procedure TDataField.SetLookupTable(AuxTable : TAuxTable);
begin
  Assert(AuxTable <> nil, 'TDataField.SetLookupTable: <> nil');
  FLookupTable := AuxTable;
end;

procedure TDataField.AddToList(AList : TList; AddCalcFields : Boolean);
begin
  Assert(AList <> nil, 'TDataField.AddToList: AList <> nil');
  AList.Add(Self);
end;

function TDataField.GetDefaultDisplayValues : TDisplayValues;
begin
  Result := DisplayValues;

  if Result = dvDefault then
  begin
    if (DataType is TStringType) then
      Result := dvKeyAndText
    else
      Result := dvKeyOnly;
  end;

  if (TextField = nil) then
    Result := dvKeyOnly;
end;

function TDataField.DisplayTextDataType(DisplayValues : TDisplayValues) : TDataType;
begin
  if DisplayValues = dvDefault then
    DisplayValues := Self.DisplayValues;

  if DisplayValues = dvDefault then
  begin
    if (DataType is TStringType) then
      DisplayValues := dvKeyAndText
    else
      DisplayValues := dvKeyOnly;
  end;

  if (TextField = nil) then
    DisplayValues := dvKeyOnly;

  case DisplayValues of
    dvTextOnly:   Result := TextField.DataType;
    dvKeyOnly:    Result := Self.DataType;
    dvKeyAndText: Result := MemoType;
  else
    Result := MemoType;
  end;
end;

function TDataField.GetIsRunningNumber : Boolean;
begin
  Result := (Self is TRunningNumberField);
end;

function TDataField.GetLookupKey(idxKey : Integer) : TDataField;
var
  i : Integer;
begin
  if LookupTable = nil then
    Result := nil
  else
  begin
    Result := LookupTable.Field[idxKey];

    if Result <> nil then
    begin
      if (not Result.IsDependentField(Self)) then
      begin
        for i := 0 to Result.DerivedFieldCount - 1 do
          if Result.DerivedField[i].IsDependentField(Self) then
          begin
            Result := Result.DerivedField[i];
            Exit;
          end;
        Log(ltWarning, 'GetLookupField', 'TDataField.GetLookupKey: Uncertain result for field ' + Self.FieldName);
      end;
    end;
  end;
end;

function TDataField.GetLookupField : TDataField;
var
  i : Integer;
begin
  if LookupTable = nil then
    Result := nil
  else
  begin
    Result := LookupTable.AuxTableKey;

    if Result <> nil then
    begin
      if (not Result.IsDependentField(Self)) then
      begin
        for i := 0 to Result.DerivedFieldCount - 1 do
          if Result.DerivedField[i].IsDependentField(Self) then
          begin
            Result := Result.DerivedField[i];
            Exit;
          end;
        Log(ltWarning, 'GetLookupField', 'TDataField.GetLookupField: Uncertain result for field ' + Self.FieldName);
      end;
    end;
  end;
end;

class function TDataField.FieldByName(const Name : String) : TDataField;
var
  idx : Integer;
begin
  if UFieldList.Find(ValueFromString(Name), idx) then
    Result := TDataField(UFieldList.Objects[idx])
  else
    Result := nil;
end;

class function TDataField.InstanceCount : Integer;
begin
  Result := UFieldList.Count;
end;

class function TDataField.Instance(idx : Integer) : TDataField;
begin
  Assert((idx >= 0) and (idx < UFieldList.Count),
         'TDataField.Instance: (idx >= 0) and (idx < UFieldList.Count), idx: ' + IntToStr(idx));
  Result := TDataField(UFieldList.Objects[idx]);
end;

procedure TDataField.SortList(Values : TStrings; Order : TSortOrder);
begin
  SortInterval(Values, Order, 0, Values.Count - 1);
end;

procedure TDataField.SortInterval(Values : TStrings; Order : TSortOrder; Low, High : Integer);
var
  CmpVal, TmpVal : TValue;
  i, iMax, iMin : Integer;
  CmpRes : Boolean;
begin
  {Assert((Rows <> nil) and (MaxIndex >= 0) and (MaxIndex < Rows.Count) and (MinIndex >= 0) and (MinIndex < Rows.Count),
         'TDataRowList.OrderRows: (Rows <> nil) and (MaxIndex >= 0) and (MaxIndex < Rows.Count) and (MinIndex >= 0) and (MinIndex < Rows.Count)');}

  if High - Low < 2 then
  begin
    if (High > Low) and
       ((Self.DataType.Compare(ValueFromString(Values.Strings[High]), ValueFromString(Values.Strings[Low])) < 0) xor (Order = soDescending)) then
      Values.Exchange(Low, High);
    Exit;
  end;

  Values.Exchange(High, Low + Random(High - Low));

  CmpVal := ValueFromString(Values.Strings[High]);
  iMax := High - 1;
  iMin := Low;
  i := iMax;

  while iMax > iMin do
  begin
    TmpVal := ValueFromString(Values.Strings[i]);
    CmpRes := (Self.DataType.Compare(CmpVal, TmpVal) < 0) xor (Order = soDescending);
    if i = iMax then
    begin
      if CmpRes then
      begin
        Dec(iMax);
        Dec(i);
      end
      else
      begin
        Values.Exchange(iMax, iMin);
        Inc(iMin);
        i := iMin;
      end;
    end
    else
    begin
      if not CmpRes then
      begin
        Inc(iMin);
        Inc(i);
      end
      else
      begin
        Values.Exchange(iMax, iMin);
        Dec(iMax);
        i := iMax;
      end;
    end;
  end;

  TmpVal := ValueFromString(Values.Strings[i]);
  CmpRes := (Self.DataType.Compare(CmpVal, TmpVal) < 0) xor (Order = soDescending);
  if CmpRes then
  begin
    Values.Exchange(i, High);
    SortInterval(Values, Order, Low, i - 1);
    SortInterval(Values, Order, i + 1, High);
  end
  else
  begin
    Values.Exchange(i + 1, High);
    SortInterval(Values, Order, Low, i);
    SortInterval(Values, Order, i + 2, High);
  end;
end;

function TDataField.GetSQLString(FullFieldName : TQueryStringFunction; DoAggregate : Boolean) : String;
begin
  if DoAggregate then
    Result := 'SUM(' + FullFieldName(Self) + ')'
  else
    Result := FullFieldName(Self);
end;

(*
function TDataField.ConditionSQL(CriteriaField : TCriteriaField; FullFieldName : TQueryStringFunction; Params : TParamList) : String;
begin
  Result := CriteriaField.FieldParamSQL(Self.SQLString[FullFieldName, False], Params);
end;
*)



// ------------------------------- TDataTableType ----------------------------------

constructor TDataTableType.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tables := TStringList.Create;
  Tables.Sorted := False;
  FDescriptions := TLangArray.Create('');
  FDescriptions.OnGetCurrentLanguage := Self.GetCurrentLanguageIndex;
end;

constructor TDataTableType.CreateOld(Descriptions : array of String);
begin
  Create(nil);

  FDescriptions.SetAllTexts(Descriptions);

  if UTableTypeList = nil then
    CreateUList(UTableTypeList, False);
  UTableTypeList.AddValue('', EmptyString, Self);
end;

function TDataTableType.GetSortTables : Boolean;
begin
  Result := Tables.Sorted;
end;

procedure TDataTableType.SetSortTables(AValue : Boolean);
begin
  Tables.Sorted := AValue;
end;

function TDataTableType.GetTypeIndex : Integer;
begin
  Result := UTableTypeList.IndexOfObject(Self);
end;

function TDataTableType.GetTableCount : Integer;
begin
  Result := Tables.Count;
end;

function TDataTableType.GetTable(idx : Integer) : TDataTable;
begin
  Result := TDataTable(Tables.Objects[idx]);
end;

procedure TDataTableType.SetTable(AIndex : Integer; ADataTable : TDataTable);
begin
  if Tables = nil then
    Tables := TStringList.Create;
  while AIndex >= Tables.Count do
    Tables.AddObject('',nil);
  Tables.Objects[AIndex] := ADataTable;
end;

function TDataTableType.GetTableByName(Name : string) : TDataTable;
var
  iTable : integer;
begin
  iTable := Tables.IndexOf(Name);
  if iTable>=0 then
    Result := Table[iTable]
  else
    result := nil;
end;

destructor TDataTableType.Destroy;
var
  i : Integer;
begin
  inherited Destroy;

  if TableCount <> 0 then
  begin
    Log(ltWarning, 'TDataTableType', 'TDataTableType.Destroy: TableList still contains ' + IntToStr(Tables.Count) +
                                 ' table(s). The first one is ' + Table[0].TableName);
    for i := TableCount - 1 downto 0 do
      Table[i].FTableType := nil;
  end;

  if (UTableTypeList <> nil) and (Self.TypeIndex >= 0) then
    UTableTypeList.Delete(Self.TypeIndex);

  Tables.Free;
  FDescriptions.Free;
end;

class function TDataTableType.InstanceCount : Integer;
begin
  Result := UTableTypeList.Count;
end;

class function TDataTableType.Instance(idx : Integer) : TDataTableType;
begin
  Result := TDataTableType(UTableTypeList.Objects[idx]);
end;

procedure TDataTableType.AddTable(Table : TDataTable);
begin
  if (Table <> nil) and (Tables.IndexOfObject(Table) < 0) then
    Tables.AddObject(Table.TableName, Table);
end;

procedure TDataTableType.RemoveTable(Table : TDataTable);
var
  FIndex : integer;
begin
  FIndex := Tables.IndexOfObject(Table);
  while FIndex >= 0 do
  begin
    Tables.Delete(FIndex);
    FIndex := Tables.IndexOfObject(Table);
  end;
end;

procedure TDataTableType.UpdateTableName(Table : TDataTable);
var
  FIndex : integer;
begin
  if Tables.Sorted then
  begin
    RemoveTable(Table);
    AddTable(Table);
  end
  else
  begin
    FIndex := Tables.IndexOfObject(Table);
    if FIndex >= 0 then
      Tables.Strings[FIndex] := Table.TableName;
  end;
end;

procedure TDataTableType.SetDescriptions(ADescriptions : TLangArray);
begin
  FDescriptions.Assign(ADescriptions);
end;

procedure TDataTableType.CleanupList;
var
  i : Integer;
begin
  for i := Tables.Count - 1 downto 0 do
    if Tables.Objects[i] = nil then
      Tables.Delete(i)
end;



// -------------------------------- TDataTable ---------------------------------

type
  PFieldAndProc = ^TFieldAndProc;
  TFieldAndProc = record
    FField : TDataField;
    FProc : TAutoUpdateProc;
  end;

function FieldAndProc(AField : TDataField; AProc : TAutoUpdateProc) : TFieldAndProc;
begin
  with Result do
  begin
    FField := AField;
    FProc := AProc;
  end;
end;

constructor TDataTable.CreateEmpty(TableName : String; TableType : TDataTableType; DataBridge : TDataBridge);
begin
  CreateOld(TableName, TableType, [nil], [nil], DataBridge);
end;

constructor TDataTable.CreateCopy(Table : TDataTable; TableType : TDataTableType; DataBridge : TDataBridge);
var
  i : Integer;
begin
  Assert(Table <> nil, 'TDynaTable.CreateCopy: Table <> nil');

  CreateEmpty('', TableType, DataBridge);

  for i := 0 to Table.KeyCount - 1 do
    AddKey(Table.Field[i]);
  for i := Table.KeyCount to Table.FieldCount - 1 do
    AddField(Table.Field[i]);
  for i := Table.FieldCount to Table.FieldCount + Table.IgnoreFieldCount - 1 do
    AddIgnore(Table.Field[i]);

//  CalculateStoragePositions;

  for i := 0 to Table.FLookupFields.Count - 1 do
    Self.FLookupFields.Add(Table.FLookupFIelds.Items[i]);
  for i := 0 to Table.FLookupSources.Count - 1 do
    Self.FLookupSources.Add(Table.FLookupSources.Items[i]);

  Self.FRunningNumberField := Table.FRunningNumberField;
  Self.fDescriptions.Assign( Table.fDescriptions );
end;

procedure TDataTable.CopyFrom(ADataTable : TDataTable);
var
  i : Integer;
begin
  CheckLocked;

  Clear;
  for i := 0 to ADataTable.FFields.Count - 1 do
  begin
    FFields.Add(ADataTable.Field[i]);
{$ifdef D4_OR_HIGHER}
    ADataTable.Field[i].FTablesUsingMe.Add(Self);
{$else}
    ADataTable.Field[i].FTablesUsingMe.AddOL(Self);
{$endif D4_OR_HIGHER}
  end;

  FKeyCount := ADataTable.KeyCount;
  FFieldCount := ADataTable.FieldCount;

  CalculateStoragePositions;

  for i := 0 to ADataTable.FLookupFields.Count - 1 do
    Self.FLookupFields.Add(ADataTable.FLookupFIelds.Items[i]);
  for i := 0 to ADataTable.FLookupSources.Count - 1 do
    Self.FLookupSources.Add(ADataTable.FLookupSources.Items[i]);
end;

procedure TDataTable.CheckLocked;
begin
  if FLocked then
    Log(ltError, 'DynaTable', 'TDynaTable.CheckLocked: DynaTable''s structure is locked! It cannot be changed!');
end;

procedure TDataTable.Lock;
begin
  FLocked := True;
end;

procedure TDataTable.AddKey(AKeyField : TDataField);
var
  OldIndex : Integer;
begin
//  Assert(AKeyField <> nil, 'TDynaTable.AddKey: AKeyField <> nil');
  CheckLocked;

  if AKeyField <> nil then
  begin
    OldIndex := FFields.IndexOf(AKeyField);
    if OldIndex >= FieldCount then
      FFields.Remove(AKeyField)
    else if OldIndex >= 0 then
      Log(ltError, 'AddField', 'TDynaTable.AddKey: Field ' + AKeyField.FieldName + ' already exists in table!');

    if (OldIndex = -1) and (AKeyField <> nil) then
{$ifdef D4_OR_HIGHER}
      AKeyField.FTablesUsingMe.Add(Self);
{$else}
      AKeyField.FTablesUsingMe.AddOL(Self);
{$endif D4_OR_HIGHER}
  end;

  FFields.Insert(FKeyCount, AKeyField);
  Inc(FKeyCount);
  Inc(FFieldCount);
  CalculateStoragePositions;
end;

procedure TDataTable.AddField(ADataField : TDataField);
var
  OldIndex : Integer;
begin
//  Assert(ADataField <> nil, 'TDynaTable.AddField: ADataField <> nil');
  CheckLocked;

  if ADataField <> nil then
  begin
    OldIndex := FFields.IndexOf(ADataField);
    if OldIndex >= FieldCount then
      FFields.Remove(ADataField)
    else if OldIndex >= 0 then
      Log(ltError, 'AddField', 'TDynaTable.AddField: Field ' + ADataField.FieldName + ' already exists in table!');

    if (OldIndex = -1) and (ADataField <> nil) then
{$ifdef D4_OR_HIGHER}
      ADataField.FTablesUsingMe.Add(Self);
{$else}
      ADataField.FTablesUsingMe.AddOL(Self);
{$endif D4_OR_HIGHER}
  end;

  FFields.Insert(FFieldCount, ADataField);
  Inc(FFieldCount);
  CalculateStoragePositions;
end;

procedure TDataTable.AddIgnore(AIgnoreField : TDataField);
begin
//  Assert(AIgnoreField <> nil, 'TDynaTable.AddField: ADataField <> nil');
  CheckLocked;

  if (AIgnoreField <> nil) and (FFields.IndexOf(AIgnoreField) >= 0) then
      Log(ltError, 'AddField', 'TDynaTable.AddIgnore: Field ' + AIgnoreField.FieldName + ' already exists in table!');

  if AIgnoreField <> nil then
{$ifdef D4_OR_HIGHER}
    AIgnoreField.FTablesUsingMe.Add(Self);
{$else}
    AIgnoreField.FTablesUsingMe.AddOL(Self);
{$endif D4_OR_HIGHER}
  FFields.Add(AIgnoreField);
  CalculateStoragePositions;
end;

procedure TDataTable.Clear;
var
  i : Integer;
begin
  CheckLocked;

  for i := 0 to FFields.Count - 1 do
{$ifdef D4_OR_HIGHER}
    Field[i].FTablesUsingMe.Remove(Self);
{$else}
    Field[i].FTablesUsingMe.RemoveOL(Self);
{$endif D4_OR_HIGHER}

  FFields.Clear;
  FKeyCount := 0;
  FFieldCount := 0;
  Self.RunningNumberField := nil;

  Self.FLookupFields.Clear;
  Self.FLookupSources.Clear;

  CalculateStoragePositions;
end;

procedure TDataTable.RemoveDataField(AField : TDataField);
var
  idx : Integer;
begin
  Assert(AField <> nil, 'TDynaTable.RemoveDataField: AField <> nil');
  CheckLocked;

  idx := FFields.IndexOf(AField);
  if idx >= 0 then
  begin
{$ifdef D4_OR_HIGHER}
    Field[idx].FTablesUsingMe.Remove(Self);
{$else}
    Field[idx].FTablesUsingMe.RemoveOL(Self);
{$endif D4_OR_HIGHER}

    FFields.Delete(idx);
    if idx < KeyCount then
      Dec(FKeyCount);
    if idx < FieldCount then
      Dec(FFieldCount);

    CalculateStoragePositions;
  end;
end;

procedure TDataTable.RemoveDataFieldByIndex(AIndex : Integer);
begin
  CheckLocked;

  if AIndex >= 0 then
  begin
//ivt
    if Field[AIndex] <> nil then
{$ifdef D4_OR_HIGHER}
      Field[AIndex].FTablesUsingMe.Remove(Self);
{$else}
      Field[AIndex].FTablesUsingMe.RemoveOL(Self);
{$endif D4_OR_HIGHER}
    FFields.Delete(AIndex);
    if AIndex < KeyCount then
      Dec(FKeyCount);
    if AIndex < FieldCount then
      Dec(FFieldCount);

    CalculateStoragePositions;
  end;
end;

procedure TDataTable.MoveKey(AKeyField : TDataField; NewPos : Integer);
var
  idx : Integer;
begin
  Assert(AKeyField <> nil, 'TDynaTable.MoveKey: AKeyField <> nil');
  CheckLocked;

  idx := FFields.IndexOf(AKeyField);
  if idx = -1 then
  begin
    if NewPos <= KeyCount then
    begin
{$ifdef D4_OR_HIGHER}
      AKeyField.FTablesUsingMe.Add(Self);
{$else}
      AKeyField.FTablesUsingMe.AddOL(Self);
{$endif D4_OR_HIGHER}
      FFields.Insert(NewPos, AKeyField);
      Inc(FKeyCount);
      Inc(FFieldCount);
    end
    else
      Log(ltError, 'DynaTable', 'TDynaTable.MoveKey: Cannot add key into field area');
  end
  else if idx < KeyCount then
  begin
    if NewPos < KeyCount then
      FFields.Move(idx, NewPos)
    else
      Log(ltError, 'DynaTable', 'TDynaTable.MoveKey: Cannot move key into field area');
  end
  else
  begin
    if NewPos <= KeyCount then
    begin
      FFields.Move(idx, NewPos);
      Inc(FKeyCount);
      if idx >= FieldCount then
        Inc(FFieldCount);
    end
    else
      Log(ltError, 'DynaTable', 'TDynaTable.MoveKey: Cannot move key into field area');
  end;

  CalculateStoragePositions;
end;

procedure TDataTable.MoveField(ADataField : TDataField; NewPos : Integer);
var
  idx : Integer;
begin
  Assert(ADataField <> nil, 'TDynaTable.MoveField: ADataField <> nil');
  CheckLocked;

  idx := FFields.IndexOf(ADataField);

  if idx = -1 then
  begin
    if NewPos >= KeyCount then
    begin
{$ifdef D4_OR_HIGHER}
      ADataField.FTablesUsingMe.Add(Self);
{$else}
      ADataField.FTablesUsingMe.AddOL(Self);
{$endif D4_OR_HIGHER}

      FFields.Insert(NewPos, ADataField);

      if NewPos <= FFieldCount then
        Inc(FFieldCount);
    end
    else
      Log(ltError, 'DynaTable', 'TDynaTable.MoveField: Cannot add field into key area');
  end
  else if idx < KeyCount then
  begin
    if NewPos >= KeyCount -1 then
    begin
      FFields.Move(idx, NewPos);
      Dec(FKeyCount);
    end
    else
      Log(ltError, 'DynaTable', 'TDynaTable.MoveField: Cannot move field into key area');
  end
  else
  begin
    if NewPos >= KeyCount then
    begin
      FFields.Move(idx, NewPos);
      if idx >= FieldCount then
        Inc(FFieldCount);
    end
    else
      Log(ltError, 'DynaTable', 'TDynaTable.MoveKey: Cannot move key into field area');
  end;
  CalculateStoragePositions;
end;

function TDataTable.GetField(idx : Integer) : TDataField;
begin
  Assert((idx >= 0) and (idx < FieldCount + IgnoreFieldCount),
         'TDataTable.GetField: (idx >= 0) and (idx < FieldCount + IgnoreFieldCount), idx: ' + IntToStr(idx));
  Result := TDataField(FFields[idx]);
end;

function TDataTable.FindDependentSource(ADataField : TDataField) : TDataField;
begin
  if ADataField = nil then
    Result := nil
  else if TableHasField(ADataField) then
    Result := ADataField
  else
    Result := FindDependentSource(ADataField.LookupField); // Fixa LGE ska dependentlookupfield användas?
end;

function TDataTable.DefaultFieldName(Field : TDataField) : String;
begin
  Result := Field.FieldName;
end;

procedure TDataTable.AddIgnoreFields(Fields : array of TDataField);
var
  i : Integer;
begin
  // Fixa MAB or IVT: shouldn't we check that these fields doesn't already exist AND
  //                  call CalcStoragePositions...?
  for i := Low(Fields) to High(Fields) do
  begin
    FFields.Add(Fields[i]);
{$ifdef D4_OR_HIGHER}
    Fields[i].FTablesUsingMe.Add(Self);
{$else}
    Fields[i].FTablesUsingMe.AddOL(Self);
{$endif D4_OR_HIGHER}
  end;
end;

constructor TDataTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FRunningNumberField := nil;
  FLocked := False;
  FFieldLists := TList.Create;
  FLookupFields := TList.Create;
  FLookupSources := TList.Create;
  FAutoUpdateFieldAndProcs := TList.Create;

  FFields := TList.Create;
  FFieldsFastSearchList := TValueList.Create(IntegerType);

  FFieldData := TList.Create;
  IDefaultRow := nil;

  FTableType := nil;
  FTableName := '';

  //FDescriptions := TStringList.Create;
  FDescriptions := TLangArray.Create(FTableName);
  FDescriptions.OnGetCurrentLanguage := GetCurrentLanguageIndex;
  HelpTexts.DefaultTextValue := FTableName;
  FKeyCount := 0;
  FFieldCount := 0;

  CalculateStoragePositions;
end;

constructor TDataTable.CreateOld(TableName : String; TableType : TDataTableType;
                              Keys : array of TDataField; Fields : array of TDataField;
                              DataBridge : TDataBridge);
var
  i : Integer;
begin
  Create(nil);

  Self.TableType := TableType;
  Self.TableName := TableName;

  FDataBridge := DataBridge;

  for i := Low(Keys) to High(Keys) do
    if Keys[i] <> nil then
      Keys[i].AddToList(FFields, True);

  FKeyCount := FFields.Count;

  for i := Low(Fields) to High(Fields) do
  if Fields[i] <> nil then
  begin
    Fields[i].AddToList(FFields, True);
    if Fields[i] is TDataFieldList then
      FFieldLists.Add(Fields[i]);
  end;
  FFieldCount := FFields.Count;

  for i := 0 to Self.FieldCount - 1 do
{$ifdef D4_OR_HIGHER}
    Self.Field[i].FTablesUsingMe.Add(Self);
{$else}
    Self.Field[i].FTablesUsingMe.AddOL(Self);
{$endif D4_OR_HIGHER}

  CalculateStoragePositions;
  CheckDoubleFields;
end;

procedure TDataTable.CheckDoubleFields;
var
  i : Integer;
begin
  for i := 0 to FieldCount - 1 do
    if Self.IndexOfField(Field[i]) <> i then
      raise Exception.Create('Double field: ' + Field[i].FieldName + ' ' + Field[i].LongDescription + ' (index: ' + IntToStr(i) + ')');
end;

procedure TDataTable.SetDescriptions(Descriptions : array of String);
{var
  i : Integer;
  l : Integer; }
begin
  FDescriptions.SetAllTexts(Descriptions);

{  l := Low(Descriptions);

  for i := 0 to Singletons.LanguageCount - 1 do
  begin
    if l + i <= High(Descriptions) then
      SetDescription(i, Descriptions[l+i])
    else
      SetDescription(i, Descriptions[l]);
  end; }
end;

function TDataTable.GetPresentDescription : String;
begin
  Result := FDescriptions.Text;

{  if Singletons.LanguageIndex < FDescriptions.Count then
    Result := FDescriptions.Strings[Singletons.LanguageIndex]
  else if FDescriptions.Count > 0 then
    Result := FDescriptions.Strings[0]
  else
    Result := TableName; }
end;

{
function TDataTable.GetDescription(Language : Integer) : String;
begin
  Assert(((Language >= 0) and (Language < Singletons.LanguageCount)), 'TDataTable.GetDescription: ((Language >= 0) and (Language < Singletons.LanguageCount))' +', Language := ' +IntToStr(Language) +', LanguageCount := ' +IntToStr(Singletons.LanguageCount));

  if Language < FDescriptions.Count then
    Result := FDescriptions.Strings[Language]
  else if FDescriptions.Count > 0 then
    Result := FDescriptions.Strings[0]
  else
    Result := TableName;
end;
}

{
procedure TDataTable.SetDescription(Language : Integer; Descr : String);
var
  FDefaultString : String;
begin
  Assert(((Language >= 0) and (Language < Singletons.LanguageCount)), 'TDataTable.SetDescription: ((Language >= 0) and (Language < Singletons.LanguageCount))' +', Language := ' +IntToStr(Language) +', LanguageCount := ' +IntToStr(Singletons.LanguageCount));

  if FDescriptions.Count > 0 then
    FDefaultString := FDescriptions.Strings[0]
  else
    FDefaultString := TableName;
  while (FDescriptions.Count <= Language) do
    FDescriptions.Add(FDefaultString);
  FDescriptions.Strings[Language] := Descr;
end;
}

procedure TDataTable.AddLookupRule(LookupField : TDataField; ValueField : TDataField);
var
  idx : Integer;
begin
  idx := FLookupFields.IndexOf(LookupField);

  if idx = -1 then
  begin
    FLookupFields.Add(LookupField);
    FLookupSources.Add(ValueField);
  end
  else
  begin
    FLookupSources.Items[idx] := ValueField;
  end;
end;

procedure TDataTable.InsertLookupRule(AIndex : Integer; LookupField : TDataField; ValueField : TDataField);
var
  idx : Integer;
begin
  idx := FLookupFields.IndexOf(LookupField);

  if (idx = -1) or (LookupField = nil) then
  begin
    FLookupFields.Insert(AIndex,LookupField);
    FLookupSources.Insert(AIndex,ValueField);
  end;
end;

procedure TDataTable.RemoveLookupRule(LookupField : TDataField);
var
  idx : Integer;
begin
  idx := FLookupFields.IndexOf(LookupField);

  if idx > -1 then
  begin
    FLookupFields.Delete(idx);
    FLookupSources.Delete(idx);
  end;
end;
procedure TDataTable.RemoveLookupRuleByIndex(AIndex : Integer);
begin
  if AIndex < FLookupFields.Count then
  begin
    FLookupFields.Delete(AIndex);
    FLookupSources.Delete(AIndex);
  end;
end;

function TDataTable.LookupRulesCount : Integer;
begin
  Assert((FLookupFields <> nil) and (FLookupSources <> nil),
         'TDataTable.LookupRulesCount: FLookupFields or FLookupSources is nil ');
  Assert(FLookupFields.Count = FLookupSources.Count,
         'TDataTable.LookupRulesCount: FLookupFields.Count <> FLookupSources.Count ');
  result := FLookupFields.Count;
end;

function TDataTable.IndexOfLookupRule(LookupField: TDataField): Integer;
begin
  Assert((FLookupFields <> nil) and (FLookupSources <> nil),
         'TDataTable.IndexOfLookupRule: FLookupFields or FLookupSources is nil ');
  Assert(FLookupFields.Count = FLookupSources.Count,
         'TDataTable.IndexOfLookupRule: FLookupFields.Count <> FLookupSources.Count ');
  result := FLookupFields.IndexOf( LookupField );
end;

function TDataTable.GetLookupRuleField(index: integer): TDataField;
begin
  Assert((FLookupFields <> nil) and (FLookupSources <> nil),
         'TDataTable.GetLookupRuleField: FLookupFields or FLookupSources is nil ');
  Assert(FLookupFields.Count = FLookupSources.Count,
         'TDataTable.GetLookupRuleField: FLookupFields.Count <> FLookupSources.Count ');

  result := TDataField( FLookupFields[index] );
end;

function TDataTable.GetLookupRuleValue(index: integer): TDataField;
begin
  Assert((FLookupFields <> nil) and (FLookupSources <> nil),
         'TDataTable.GetLookupRuleValue: FLookupFields or FLookupSources is nil ');
  Assert(FLookupFields.Count = FLookupSources.Count,
         'TDataTable.GetLookupRuleValue: FLookupFields.Count <> FLookupSources.Count ');

  result := TDataField( FLookupSources[index] );
end;


procedure TDataTable.CalculateStoragePositions;

  procedure AddToFastSearch(AField : TDataField; ATableIndex : Integer);
  var
    AIndex : Integer;
  begin
    AIndex := FastSearchIndex(AField);
    while FFieldsFastSearchList.Count <= AIndex do
      FFieldsFastSearchList.AddValue('', ZeroVal, nil);

    FFieldsFastSearchList.Values[AIndex] := ValueFromInteger(ATableIndex);
    FFieldsFastSearchList.Objects[AIndex] := AField;
  end;

var
  i, Pos : Integer;
begin
  FFieldsFastSearchList.Clear;

  for i := 0 to FFieldData.Count - 1 do
    TObject(FFieldData.Items[i]).Free;
  FFieldData.Clear;

  Pos := 0;
  for i := 0 to Self.FieldCount - 1 do
  begin
    FFieldData.Add(TFieldDataObject.Create(Pos));
    if (Self.Field[i] <> nil) and (Self.Field[i].DataType <> nil) then
      Pos := Pos + Self.Field[i].DataType.DataSize;
    AddToFastSearch(Self.Field[i], i);
  end;

  IDefaultRow.Free;
  IDefaultRow := TDataRow.CreateDefault(Self);
end;

function TDataTable.CompatibleWith(DataTable : TDataTable) : Boolean;
  function Compare(DT1, DT2 : TDataTable; Start, Finish : Integer) : Boolean;
  var
    i, idx : Integer;
  begin
    Result := True;

    for i := Start to Finish do
    begin
      idx := DT2.IndexOfField(DT1.Field[i]);
      if (idx < Start) or
         (idx > Finish) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;

begin
  Assert(DataTable <> nil, 'TDataTable.CompatibleWith: DataTable <> nil');
  if DataTable = Self then
    Result := True
  else if (DataTable.KeyCount <> Self.KeyCount) or
          (DataTable.FieldCount <> Self.FieldCount) then
    Result := False
  else
    Result := Compare(Self, DataTable, 0, KeyCount - 1) and
              Compare(DataTable, Self, 0, KeyCount - 1) and
              Compare(Self, DataTable, KeyCount, FieldCount - 1) and
              Compare(DataTable, Self, KeyCount, FieldCount - 1);
end;

function TAbstractRowStorage.CanDefaultSort : Boolean;
var
  i, j, idx : Integer;
  LookupTable : TAuxTable;
begin
  Result := True;

  for i := 0 to Self.TreeKeyCount - 1 do
    if TreeKey[i].SortField <> TreeKey[i].TreeKey then
    begin
      if DataTable.IndexOfField(TreeKey[i].SortField) >= DataTable.KeyCount then
        Continue // Ok
      else if TreeKey[i].SortField is TCalcField then
        Continue;

      LookupTable := TreeKey[i].SortField.LookupTable;
      if LookupTable = nil then
      begin
        Result := False;
        Exit;
      end
      else
        for j := 0 to LookupTable.KeyCount - 1 do
        begin
          idx := DataTable.FLookupFields.IndexOf(LookupTable.Field[j]);
          if idx >= 0 then
            Continue; // Fusk LGE... =)

          idx := DataTable.IndexOfFieldOrDependent(LookupTable.Field[j]);
          if idx = -1 then
            Result := False
          else if idx > i then
            Result := False
          else
            Result := True;

          if Result = False then
            Exit;
        end;
    end;
end;

function TDataTable.GetFieldsAsText : String;
var
  i : Integer;
begin
  Result := 'Keys: ' + Field[0].FieldName;

  for i := 1 to KeyCount - 1 do
    Result := Result + '-' + Field[i].FieldName;

  if KeyCount = FieldCount then
    Result := Result + ', No Fields'
  else
  begin
   Result := Result + ', Fields: ' + Field[KeyCount].FieldName;
   for i := KeyCount + 1 to FieldCount - 1 do
     Result := Result + '-' + Field[i].FieldName;
  end;
end;

destructor TDataTable.Destroy;
var
  i : Integer;
begin
  IDefaultRow.Free;

  FFieldLists.Free;
  FLookupFields.Free;
  FLookupSources.Free;

  for i := 0 to FFieldData.Count - 1 do
    TObject(FFieldData.Items[i]).Free;
  FFieldData.Free;

  FFields.Free;
  FFieldsFastSearchList.Free;

  if TableType <> nil then
    TableType.RemoveTable(Self);

  FDescriptions.Free;
  inherited Destroy;

  for i := 0 to FAutoUpdateFieldAndProcs.Count - 1 do
    Dispose( FAutoUpdateFieldAndProcs[i] );
  FAutoUpdateFieldAndProcs.Free;

  if (UTableList <> nil) and (UTableList.IndexOfObject(Self) >= 0) then
    UTableList.Delete(UTableList.IndexOfObject(Self));
end;

function TDataTable.GetFieldCount : Integer;
begin
  Result := FFieldCount;
end;

function TDataTable.GetIgnoreFieldCount : Integer;
begin
  Result := FFields.Count - FFieldCount;
end;

function TDataTable.GetFieldListCount : Integer;
begin
  Result := FFieldLists.Count;
end;

function TDataTable.GetFieldList(idx : Integer) : TDataFieldList;
begin
  Assert((idx >= 0) and (idx < FFieldLists.Count),
         'TDataTable.GetFieldList: (idx >= 0) and (idx < FFieldLists.Count), idx: ' + IntToStr(idx));

  Result := TDataFieldList(FFieldLists.Items[idx]);
end;

procedure TDataTable.SetTableName(NewName : String);
var
  Index : Integer;
begin
  if FTableName <> NewName then
  begin
    if FDescriptions.DefaultTextValue = FTableName then
      FDescriptions.DefaultTextValue := NewName;
    if HelpTexts.DefaultTextValue = FTableName then
      HelpTexts.DefaultTextValue := NewName;

    if UTableList = nil then
      CreateUList(UTableList)
    else if UTableList.Find(ValueFromString(FTableName), Index) then
      UTableList.Delete(Index)
    else
    begin
      Index := UTableList.IndexOfObject(Self);
      if Index >= 0 then
        UTableList.Delete(Index);
    end;

    FTableName := NewName;

    try
      if (NewName <> '') and (TableType <> nil) then // This is little kludgy, but for the moment a must...
        UTableList.AddValue('', ValueFromString(NewName), Self);
    except
      Log(ltWarning, 'Duplicates', 'Duplicate tables: ' + NewName);
    end;

    if (TableType <> nil) then
      TableType.UpdateTableName(Self);
  end;
end;

procedure TDataTable.SetDataTableType(NewType : TDataTableType);
begin
  if NewType <> Self.TableType then
  begin
    if Self.TableType <> nil then
      Self.TableType.RemoveTable(Self);

    FTableType := NewType;

    if NewType <> nil then
      NewType.AddTable(Self);
  end;
end;

{procedure TDataTable.SetDataTableType2(NewType : TDataTableType);
begin
  if NewType <> Self.TableType then
  begin
    if Self.TableType <> nil then
    begin
      Self.TableType.RemoveTable(Self);

      if (NewType = nil) and
         (UTableList <> nil) and
         (UTableList.IndexOfObject(Self) >= 0) then
        UTableList.Delete(UTableList.IndexOfObject(Self)); 
    end
    else if (NewType <> nil) and
            (TableName <> '') then
    try
      UTableList.AddValue('', ValueFromString(TableName), Self);
    except
      Log(ltWarning, 'Duplicates', 'Duplicate tables: ' + TableName);
    end;

    FTableType := NewType;
  end;
end;}

(*
procedure TDataTable.SeTDataTableTypeAndName(NewType : TDataTableType; NewName : String);
var
  Index : Integer;
begin
  if FTableName <> NewName then
  begin
    if UTableList = nil then
      CreateUList(UTableList)
    else if UTableList.Find(ValueFromString(FTableName), Index) then
      UTableList.Delete(Index)
    else
    begin
      Index := UTableList.IndexOfObject(Self);
      if Index >= 0 then
        UTableList.Delete(Index);
    end;

    FTableName := NewName;

    if NewType <> Self.TableType then
    begin
      if Self.TableType <> nil then
        Self.TableType.RemoveTable(Self);

      if NewType <> nil then
      begin
        NewType.AddTable(Self);
        FTableType := NewType;
      end;
    end;

    if Self.TableType <> nil then
    begin
      try
        if NewName <> '' then
          UTableList.AddValue('', ValueFromString(NewName), Self);
      except
        Log(ltWarning, 'Duplicates', 'Duplicate tables: ' + NewName);
      end;
    end;
  end;
end;
*)

function TDataTable.GetDataLength : Integer;
var
  i : Integer;
begin
  if Self.FieldCount = 0 then
    Result := 0
  else
  begin
    i := Self.FieldCount - 1;
    Result := Self.ReadPos[i];
    if (Self.Field[i] <> nil) and (Self.Field[i].DataType <> nil) then
      Result := Result + Self.Field[i].DataType.DataSize;
  end;
end;

function TDataTable.PrimaryTable(Field : TDataField) : TDataTable;
begin                            // Fixa LGE
  if Self.TableHasField(Field) then
    Result := Self
  else if not (Field is TKeyField) then
    Result := Self
  else
    Result := Self.FindDependentSource(Field).AuxTable;
end;

function TDataTable.FullFieldName(Field : TDataField; FieldNames : TStringList) : String;
  function GetTablePrefix(Field : TDataField; Dot : Boolean) : String;
  var
    idx : Integer;
  begin
    idx := FieldNames.IndexOfObject(Field);
    Result := FieldNames.Strings[idx];

    if Dot and (Result <> '') then
      Result := Result + '.';
  end;

begin
  if Self.PrimaryTable(Field).TableHasField(Field) then
    Result := GetTablePrefix(Field, true) + Field.FieldName
  else if not (Field is TKeyField) then
    Result := GetTablePrefix(Field, true) + Field.FieldName // Fixa LGE
  else
    Result := GetTablePrefix(Field, true) + Field.DirectAuxTableField.FieldName;
end;






procedure TDataTable.DefineProperties(Filer : TFiler);
begin


  inherited DefineProperties(Filer);
end;





procedure TDataTable.CleanupList;
var
  FIndex : Integer;
begin
  FIndex := FFields.IndexOf(nil);
  while FIndex > -1 do
  begin
    // do not, it's nil! ;) // Field[FIndex].FTablesUsingMe.Remove(Self);
    FFields.Delete(FIndex);

    if FIndex < FKeyCount then
      Dec(FKeyCount);
    if FIndex < FFieldCount then
      Dec(FFieldCount);
    FIndex := FFields.IndexOf(nil);
  end;
  FIndex := FLookupFields.IndexOf(nil);
  while FIndex > -1 do
  begin
    FLookupFields.Delete(FIndex);
    if FIndex < FLookupSources.Count then
      FLookupSources.Delete(FIndex);
    FIndex := FLookupFields.IndexOf(nil);
  end;
  FIndex := FLookupSources.IndexOf(nil);
  while FIndex > -1 do
  begin
    FLookupSources.Delete(FIndex);
    if FIndex < FLookupFields.Count then
      FLookupFields.Delete(FIndex);
    FIndex := FLookupSources.IndexOf(nil);
  end;
end;

























procedure TDataTable.SetField(AList : TList; ADataField : TDataField; AIndex : Integer);
begin
  if AList = nil then
    AList := TList.Create;
  while AIndex >= AList.Count do
    AList.Add(nil);
  AList.Items[AIndex] := ADataField;
end;

procedure TDataTable.SetLookupField(AIndex : Integer; AField : TDataField);
begin
  SetField(FLookupFields, AField, AIndex);
end;

procedure TDataTable.SetLookupSource(AIndex : Integer; AField : TDataField);
begin
  SetField(FLookupSources, AField, AIndex);
end;

function TDataTable.GetLookupField(AIndex : Integer) : TDataField;
begin
  Assert((AIndex >= 0) and (AIndex < FLookupFields.Count),
         'TDataTable.GetLookupField: (idx >= 0) and (idx < FLookupFields.Count), idx: ' + IntToStr(AIndex));
  Result := TDataField(FLookupFields[AIndex]);
end;
function TDataTable.GetLookupSource(AIndex : Integer) : TDataField;
begin
  Assert((AIndex >= 0) and (AIndex < FLookupSources.Count),
         'TDataTable.GetLookupSource: (idx >= 0) and (idx < FLookupSources.Count), idx: ' + IntToStr(AIndex));
  Result := TDataField(FLookupSources[AIndex]);
end;

function TDataTable.IndexOfField(Field : TDataField) : Integer;
var
  AIndex : Integer;
  AObj : TObject;
begin
  Assert(Field <> nil, 'TDataTable.IndexOfField: Field <> nil');

  AIndex := FastSearchIndex(Field);
  if (AIndex < FFieldsFastSearchList.Count) then
  begin
    AObj := FFieldsFastSearchList.Objects[AIndex];
    if AObj = nil then
      Result := -1
    else if AObj = Field then
      Result := AsInteger(FFieldsFastSearchList.Values[AIndex])
    else // two fields with same fastsearchindex; we have to do it the hard way...
    begin
      Result := FFields.IndexOf(Field);
      if Result >= FieldCount then
        Result := -1;
    end;
  end
  else
    Result := -1;
end;

function TDataTable.IndexOfFieldOrDependent(Field : TDataField) : Integer;
var
  i : Integer;
begin
  Result := IndexOfField(Field);
  if Result >= FieldCount then
    Result := -1;

  if Result = -1 then
  for i := 0 to Self.FieldCount - 1 do
    if (Self.Field[i].AuxTableField = Field) then
    begin
      Result := i;
      Break;
    end;
end;

procedure TDataTable.SetRunningNumberField(AField : TRunningNumberField);
var
  fieldIdx : Integer;
begin
  if AField = nil then
    Self.FRunningNumberField := nil
  else
  begin
    fieldIdx := IndexOfField(AField);
    if (fieldIdx >= 0) and (fieldIdx < KeyCount) then
      Self.FRunningNumberField := AField
    else
      raise Exception.Create('TDataTable.SetRunningNumberField: ' + AField.FieldName +
                             ' must be a key of table ' + TableName +
                             ' in order to set it as running number field!');
  end;
  // Fixa LGE vad händer då tabellens struktur ändrar???
end;

function TDataTable.TableHasField(Field : TDataField) : Boolean;
begin
  Assert(Field <> nil, 'TDataTable.TableHasField: Field <> nil');

  Result := IndexOfField(Field) >= 0;
end;

{/** Does the table contain a field that has this field as it's derived field? */}
function TDataTable.TableHasParentField(ADependentField : TDataField) : Boolean;

  function CheckField(AField : TDataField) : Boolean;
  var iDerived : integer;
  begin
    if ADependentField = AField then
      Result := True
    else
    begin
      Result := False;
      for iDerived := 0 to AField.DependentFieldCount - 1 do
      begin
        Result := CheckField( AField.DependentField[iDerived] );
        if Result then Break;
      end;
    end;
  end;

var iField : integer;
begin
  Result := False;
  for iField := 0 to FieldCount - 1 do
  begin
    Result := CheckField( Field[iField] );
    if Result then Break;
  end;
end;

{
function TDataTable.TableHasDependent(Field : TDataField; var OnWhichKey : TDataField) : Boolean;
var
  i : Integer;
begin
  Assert(Field <> nil, 'TDataTable.TableHasDependent: Field <> nil');

  Result := False;
  OnWhichKey := nil;

  for i := 0 to FieldCount - 1 do
    if Field = Self.Field[i].AuxTableField then
    begin
      Result := True;
      OnWhichKey := Self.Field[i];
      Exit;
    end;
end;
}

function TDataTable.TableHasFieldList(Field : TDataField) : Boolean;
begin
  Result := Self.FFieldLists.IndexOf(Field) >= 0;
end;

function TDataTable.TableHasKey(Field : TDataField) : Boolean;
var
  idx : Integer;
begin
  Assert(Field <> nil, 'TDataTable.TableHasKey: Field <> nil');

  idx := IndexOfField(Field);
  Result := (idx >= 0) and (idx < KeyCount);
end;

function TDataTable.TableHasNonKey(Field : TDataField) : Boolean;
var
  idx : Integer;
begin
  Assert(Field <> nil, 'TDataTable.TableHasNonKey: Field <> nil');

  idx := IndexOfField(Field);
  Result := (idx >= KeyCount);
end;

function TDataTable.FieldIsKey(DataField : TDataField) : Boolean;
var
  i : Integer;
begin
  Assert(DataField <> nil, 'TDataTable.FieldIsKey: DataField <> nil');

  i := IndexOfField(DataField);
  Result := (i < KeyCount) and (i > -1);
end;

function TDataTable.GetReadPos(idx : Integer) : Integer;
begin
  Result := TFieldDataObject(FFieldData.Items[idx]).ReadPos;
end;

class function TDataTable.TableByName(Name : String) : TDataTable;
var
  idx : Integer;
begin
  if UTableList.Find(ValueFromString(Name), idx) then
    Result := TDataTable(UTableList.Objects[idx])
  else
    Result := nil;
end;

class function TDataTable.InstanceCount : Integer;
begin
  Result := UTableList.Count;
end;

class function TDataTable.Instance(idx : Integer) : TDataTable;
begin
  Assert((idx >= 0) and (idx < UTableList.Count),
         'TDataTable.Instance: (idx >= 0) and (idx < UTableList.Count), idx: ' + IntToStr(idx));

  Result := TDataTable(UTableList.Objects[idx]);
end;

function TDataTable.ClearDb(ClearCriteria : TCondition) : Integer;
var
  Interruptable : TInterruptable;
begin
  Interruptable := TInterruptable.Create;
  if ClearCriteria = nil then
  begin
    ClearCriteria := TQuiltPatch.Create;
    DataBridge.DeleteData(Self, ClearCriteria, Interruptable);
    ClearCriteria.Free;
  end
  else
    DataBridge.DeleteData(Self, ClearCriteria, Interruptable);

  Result := Interruptable.RowsAffected[ otClear ];
  Interruptable.Free;
end;











{ TRunningNumberGenerator }

procedure TRunningNumberGenerator.SaveFailed(Sender : TDataBridge; Row : TDataRow);
begin
  // Nothing here
end;



function TDefaultRunningNumberGenerator.IsTempRunningNumber(Row : TDataRow) : Boolean;
begin
  Result := (Row.IntValue[Row.DataTable.RunningNumberField] <= 0);
end;

function TDefaultRunningNumberGenerator.GetNextTempRunningNumber(Table : TDataTable; const Values : TValueList) : TValue;
var
  i : Integer;
  iNo : Integer;
  Res : Integer;
begin
//  if not SubTotalKey.TreeKey.IsRunningNumber then
//    Log(ltError, 'RunningNumber', 'TSubTotalRow.GetNextNegativeRunningNumber: Not a running number!');

  Res := 0;

  for i := 0 to Values.Count - 1 do
  begin
    iNo := AsInteger(Values.Values[i]);
    if iNo < Res then
      Res := iNo;
  end;

  Result := ValueFromInteger(Res-1);
end;

{

function TSubTotalRow.GetNextNegativeRunningNumber : Integer;
var
  i : Integer;
  iNo : Integer;
begin
  if not SubTotalKey.TreeKey.IsRunningNumber then
    Log(ltError, 'RunningNumber', 'TSubTotalRow.GetNextNegativeRunningNumber: Not a running number!');

  Result := 0;

  for i := 0 to FSubRows.Count - 1 do
  begin
    iNo := AsInteger(FSubRows.Values[i]);
    if iNo < Result then
      Result := iNo;
  end;

  Dec(Result);
end;

function TDataRow.NextRunningNumber(Query : TDataQuery; FieldSet : TDataFieldSet) : Integer;
var
  Field : TField;
begin
  SetParams(Query, FieldSet, False, False, False);
  Query.Open;
  Field := Query.FieldByName('MAX_' + DataTable.RunningNumberField.FieldName);
  if Field.IsNull then
    Result := 1
  else
    Result := Field.AsInteger + 1;
  Query.Close;
end;
procedure TDataTable.GenerateMaxNoSQL(Query : TDataQuery);
var
  i : Integer;
  c : Integer;
  RN : TRunningNumberField;
  SQL : TStringList;
begin
  RN := RunningNumberField;
  SQL := TStringList.Create;

  if RN = nil then
    Exit;

  SQL.Add('Select max(' + RN.FieldName + ') as MAX_' +
                 RN.FieldName + ' from ' + TableName);

  c := 0;
  for i := 0 to KeyCount - 1 do
  if Field[i].CanBeInDB and
     (Field[i] <> RN) then
  begin
    SQL.Add(WhereCond(c) + ' ' + Field[i].FieldName + ' = :' + LowerCase(Field[i].FieldName))
  end;

  Query.SQL := SQL;
  SQL.Free;
end;
}

procedure TDataTable.FieldsToList(AList : TList);
var
  i : Integer;
begin
  AList.Clear;

  for i := 0 to FieldCount - 1 do
    AList.Add(Field[i]);
end;

procedure TDataTable.KeysToList(AList : TList; AddFictiveFields:Boolean);
var
  i : Integer;
  AField : TDataField;
begin
  AList.Clear;
  for i := 0 to KeyCount - 1 do
  begin
    AField := Field[i];
    if AddFictiveFields or AField.CanBeInDB then
      AList.Add(AField);
  end;
end;

// Fixa LGE: Motsvarande metod finns redan i TDataFieldSet...
procedure TDataTable.AddFieldsToFieldSet(FieldSet : TDataFieldSet);
var
  i : Integer;
begin
  for i := 0 to FieldCount - 1 do
    FieldSet.AddField(Field[i]);
end;

procedure TDataTable.AddNonAggregablesToFieldSet(FieldSet : TDataFieldSet);
var
  i : Integer;
begin
  for i := 0 to FieldCount - 1 do
    if not Field[i].IsAggregable then
      FieldSet.AddField(Field[i]);
end;

procedure TDataTable.AutoUpdateRow(ARow: TDataRow);
var
  i : Integer;
  PItem : PFieldAndProc;
  AField : TDataField;
begin
  for i := 0 to FAutoUpdateFieldAndProcs.Count -1 do
  begin
    try
      PItem := PFieldAndProc(FAutoUpdateFieldAndProcs[i]);
      AField := PItem^.FField;
      ARow[AField] := PItem^.FProc(Self, ARow, AField);
    except
      on E : Exception do
        Log(ltWarning, 'AutoUpdateRow', E.Message);
    end;
  end;
end;

function TDataTable.IndexOfFieldAndProc(AField : TDataField) : Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to FAutoUpdateFieldAndProcs.Count -1 do
  begin
    if PFieldAndProc(FAutoUpdateFieldAndProcs[i])^.FField = AField then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TDataTable.GetAutoUpdateProc(AField: TDataField): TAutoUpdateProc;
var
  idx : Integer;
begin
  idx := IndexOfFieldAndProc(AField);
  if idx >= 0 then
    Result := PFieldAndProc(FAutoUpdateFieldAndProcs[idx])^.FProc
  else
    Result := nil;
end;

procedure TDataTable.SetAutoUpdateProc(AField: TDataField;
  const Value: TAutoUpdateProc);
var
  idx : Integer;
  PItem : PFieldAndProc;
begin
  Assert(TableHasField(AField));

  idx := IndexOfFieldAndProc(AField);
  if idx >= 0 then
  begin
    PItem := PFieldAndProc(FAutoUpdateFieldAndProcs[idx]);
    if Assigned(Value) then
      PItem^.FProc := Value
    else
    begin
      Dispose(PItem);
      FAutoUpdateFieldAndProcs.Delete(idx);
    end;
  end
  else if Assigned(Value) then
  begin
    New(PItem);
    PItem^.FField := AField;
    PItem^.FProc := Value;
    FAutoUpdateFieldAndProcs.Add(PItem);
  end;
end;



// ------------------------ TFieldDataObject -----------------------------------

constructor TFieldDataObject.Create(ReadPos : Integer);
begin
  Self.ReadPos := ReadPos;
end;

function WhereCond(var i : Integer) : String;
begin
  if i = 0 then
    Result := 'WHERE'
  else
    Result := 'AND';

  Inc(i);
end;

// ------------------------- TVirtualAuxTable ----------------------------------



// ----------------------- TAuxTable ---------------------------

constructor TAuxTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Fixa LGE
  FAuxTableCache := nil;
end;

constructor TAuxTable.CreateOld(TableName : String; TableType : TDataTableType;
                                Keys : array of TDataField; Fields : array of TDataField;
                                AuxTableField : TDataField;
                                LoadPolicy : TCacheLoadPolicy; DataBridge : TDataBridge; CachingNonKeys : TDataFieldSet = nil);
var
  i, j : Integer;
begin
  inherited CreateOld(TableName, TableType, Keys, Fields, DataBridge);

  if AuxTableField <> nil then
  begin
    AuxTableField.SetAuxTable(Self);
    for i := 0 to AuxTableField.DerivedFieldCount - 1 do
      AuxTableField.DerivedField[i].SetAuxTable(Self);
  end;

  for i := KeyCount to FieldCount - 1 do
  begin
    if Field[i].LookupTable = nil then
    begin
      Field[i].LookupTable := Self;
      if Field[i] is TKeyField then // Fixa LGE
        for j := 0 to Field[i].DerivedFieldCount - 1 do
          if Field[i].DerivedField[j].LookupTable = nil then
            Field[i].DerivedField[j].LookupTable := Self;
    end;
    {else
      ShowMessage(Field[i].FieldName + ' in both ' + Field[i].LookupTable.TableName +
                  ' and ' + Self.TableName);}
  end;

  FAuxTableCache := CreateCache(LoadPolicy, CachingNonKeys);
end;

constructor TAuxTable.CreateCopy(Table : TDataTable; TableType : TDataTableType; DataBridge : TDataBridge);
var
  ASet : TDataFieldSet;
begin
  inherited CreateCopy(Table, TableType, DataBridge);

  if (Table is TAuxTable) and (TAuxTable(Table).Cache <> nil) then
  begin
    ASet := TDataFieldSet.Create;
    ASet.AddFrom(TAuxTable(Table).Cache.CachingNonKeys);
    FAuxTableCache := CreateCache(TAuxTable(Table).Cache.LoadPolicy, ASet);
  end;
end;

function TAuxTable.GetAuxTableCache : TCachingStorage;
begin
  if FAuxTableCache = nil then
    FAuxTableCache := CreateCache(nil);

  Result := FAuxTableCache;
end;

function TAuxTable.CreateCache(LoadPolicy : TCacheLoadPolicy; CachingNonKeys : TDataFieldSet = nil) : TCachingStorage;
begin
  Result := TAuxTableCache.Create(Self, LoadPolicy, CachingNonKeys);
end;

destructor TAuxTable.Destroy;
begin
  FAuxTableCache.Free;
  inherited Destroy;
end;

function TAuxTable.GetAuxTableKey : TDataField;
begin
  Result := Field[KeyCount - 1];
  if Result.AuxTable <> Self then
    Result := nil;
end;

procedure TAuxTable.SetCacheLoadPolicy(ALoadPolicy : TCacheLoadPolicy);
begin
  CalculateStoragePositions;

  if Cache = nil then
    FAuxTableCache := CreateCache(ALoadPolicy)
  else if Cache.LoadPolicy <> ALoadPolicy then
    Cache.LoadPolicy := ALoadPolicy;
end;

function TAuxTable.GetCacheLoadPolicy : TCacheLoadPolicy;
begin
  if Cache <> nil then
    Result := Cache.LoadPolicy
  else
    Result := nil;
end;



function TAuxTable.GetIsVirtual : Boolean;
begin
  Result := False;
end;

{procedure TAuxTable.DefineProperties(Filer : TFiler);
begin
  Filer.DefineProperty('IsVirtual', ReadIsVirtual, WriteIsVirtual, True);

  inherited DefineProperties(Filer);
end;

procedure TAuxTable.ReadIsVirtual(Reader : TReader);
begin

end;

procedure TAuxTable.WriteIsVirtual(Writer : TWriter);
begin

end;
}
// ----------------------- TBulkTable ---------------------------
(*
constructor TBulkTable.Create(TableName : String; TableType : TDataTableType;
                             Keys : array of TDataField; Fields : array of TDataField;
                             DataBridge : TDataBridge);
begin
  inherited CreateOld(TableName, TableType, Keys, Fields, DataBridge);
end;

destructor TBulkTable.Destroy;
begin
  inherited Destroy;
end;
*)

// -------------------------- TCachingStorage -----------------------------------

function TCachingStorage.GetAutoCalcSubTotals : Boolean;
begin
  Result := FAutoCalcSubTotals;
end;

procedure TCachingStorage.SetAutoCalcSubTotals(AutoCalc : Boolean);
begin
  if AutoCalc <> FAutoCalcSubTotals then
  begin
    if (not AutoCalc) and (not FTotal.FSubTotalsUptodate) then
      FTotal.UpdateSubTotals;

    FAutoCalcSubTotals := AutoCalc;
  end;
end;

function TCachingStorage.GetCanHaveTotals : Boolean;
begin
  Result := False;
end;

{$ifdef D4_OR_HIGHER}
constructor TCachingStorage.Create(DataTable : TDataTable; LoadPolicy : TCacheLoadPolicy; CachingNonKeys : TDataFieldSet = nil);
{$else}
constructor TCachingStorage.Create(DataTable : TDataTable; LoadPolicy : TCacheLoadPolicy; CachingNonKeys : TDataFieldSet);
{$endif D4_OR_HIGHER}
begin
  Assert(DataTable <> nil, 'TCachingStorage.Create: DataTable <> nil');

  FAutoCalcSubTotals := False;

  FLoadPolicy := LoadPolicy;
  if CachingNonKeys = nil then
    FCachingNonKeys := TDataFieldSet.Create
  else
    FCachingNonKeys := CachingNonKeys;

  inherited Create( DataTable );
  FRowSourceRows := nil;
end;

(*
constructor TCachingStorage.CreateWithKeyOrder(DataTable : TDataTable; KeyOrder : TRowSortOrder; LoadPolicy : TCacheLoadPolicy; CachingNonKeys : TDataFieldSet = nil);
begin
  Assert(DataTable <> nil, 'TCachingStorage.CreateWithKeyOrder: DataTable <> nil');

  Assert(DataTable <> nil, 'TCachingStorage.Create: DataTable <> nil');

  FAutoCalcSubTotals := False;

  FLoadPolicy := LoadPolicy;

  if CachingNonKeys = nil then
    FCachingNonKeys := TDataFieldSet.Create
  else
    FCachingNonKeys := CachingNonKeys;

  inherited CreateWithKeySortOrder(DataTable, True, KeyOrder);
  FRowSourceRows := nil;
end;
*)

constructor TCachingStorage.CreateVirtual(DataTable : TDataTable);
begin
  Assert(DataTable <> nil, 'TCachingStorage.CreateVirtual: DataTable <> nil');

  FAutoCalcSubTotals := False;

  FLoadPolicy := nil;
  FCachingNonKeys := TDataFieldSet.Create;
  inherited Create( DataTable );
  FRowSourceRows := nil;

end;

function TCachingStorage.GetReadOnly : Boolean;
begin

  Result := False;

end;

function TCachingStorage.AddVirtualRow(Row : TDataRow) : Boolean;
begin
  result := not (PutRow(Row, paOverwriteOnKeyChange) in IllegalPutResults);
end;

destructor TCachingStorage.Destroy;
begin
  inherited Destroy;

  FCachingNonKeys.Free;
end;



function TCachingStorage.PutRow(Row : TDataRow; Action : TPutAction) : TPutResult;
begin
  Assert(Row <> nil, 'TCachingStorage.PutRow: Row <> nil');

(*  if ReadOnly and (Action <> paInternal) then
  begin
    Log(ltError, 'Virtual AuxTable', 'Tried to put a row to virtual AuxTable ' + DataTable.TableName);
  end; *)

  if Self.DataTable <> Row.DataTable then
    Log(ltError, 'Different DataTables', 'Row and RowStorage types differ! (' +
                           Row.DataTable.TableName + ' <> ' + Self.DataTable.TableName + ')');

  if Action <> paInternal then
  begin
    if Row.Status <> rsExternControlled then
      Log(ltWarning, 'Internal', 'Trying to add a row to AuxTableCache that is already in the Storage!');
  end;

  Self.BeforeGetDataByRow(Row, [nil]);
  Result := FTotal.InternalPutRow(Row, Action);

  if (Action <> paInternal) and (Result in [prOk, prKeyOverwrited]) then
  begin
    Row.FStatus := rsNew;
  end;
end;

{
function TCachingStorage.GetShowSubTotal(idx : Integer) : Boolean;
begin
  Result := True;
end;
}
(*
procedure TCachingStorage.Load(Criteria : TCondition {TCriteria});
begin
  if Criteria = nil then
  begin
    Criteria := TQuiltPatch.Create;
    LoadRows(Criteria);
    Criteria.Free;
  end
  else
    LoadRows(Criteria);
end;
*)



procedure TCachingStorage.DiscardChanges;
var
  i : Integer;
begin
  for i := 0 to FDeletedRows.Count - 1 do
    TObject(FDeletedRows.Items[i]).Free;
  FDeletedRows.Clear;

  for i := 0 to FUnacceptedRows.Count - 1 do
    TObject(FUnacceptedRows.Items[i]).Free;
  FUnacceptedRows.Clear;

  FTotal.ClearAll;

  ClearAllRowLists;
end;

function TCachingStorage.FirstRow : TDataRow;
begin
  Result := FTotal.GetFirstRow;
end;

procedure TCachingStorage.GetLargestValues(ResultRow : TDataRow; Condition : TCondition);
var
  i : Integer;
  List : TStrings;
begin
  if ResultRow.DataTable <> Self.DataTable then
    Log(ltError, 'LargestValues', 'TRowStorage.GetLargestValues: DataTables differ');

  ResultRow.CopyContents(DataTable.IDefaultRow);

  List := TStringList.Create;
  GetRows(List, Condition, gaReference);
  for i := 0 to List.Count - 1 do
    MapLargestValues(ResultRow, TDataRow(List.Objects[i]));
  List.Free;
end;

procedure TCachingStorage.BeforeGetDataByCrit(Condition : TCondition);
var
  Quilt : TCommonQuilt;
begin
  inherited BeforeGetDataByCrit(Condition);

  if LoadPolicy = nil then
    Exit;

  Quilt := LoadPolicy.CreateCritCriteria(DataTable, Condition, CachingNonKeys);
  CachingLoadRows(Quilt);
end;

procedure TCachingStorage.BeforeGetDataByArray(GetValues : array of String);
var
  Quilt : TCommonQuilt;
begin
  inherited BeforeGetDataByArray(GetValues);

  if LoadPolicy = nil then
    Exit;

  Quilt := LoadPolicy.CreateArrayCriteria(DataTable, GetValues);
  CachingLoadRows(Quilt);
end;

procedure TCachingStorage.BeforeGetDataByRow(Row : TAbstractRow; DerivedKeys : array of TDataField);
var
  Quilt : TCommonQuilt;
begin
  inherited BeforeGetDataByRow(Row, DerivedKeys);

  if LoadPolicy = nil then
    Exit;

  Quilt := LoadPolicy.CreateRowCriteria(DataTable, Row, DerivedKeys, CachingNonKeys);
  CachingLoadRows(Quilt);
end;

procedure TCachingStorage.CachingLoadRows(Quilt : TCommonQuilt);

begin

end;

// -------------------------- TAbstractRowStorage -------------------------------

procedure TAbstractRowStorage.CopyFromSource(ASource : TAbstractRowStorage);
var
  AList : TStrings;
  i : Integer;
  DataRow, SrcRow : TDataRow;
begin
  AList := TStringList.Create;
  ASource.GetRows(AList, nil, gaReference);
  for i := 0 to AList.Count - 1 do
  begin
    SrcRow := TDataRow( AList.Objects[i] );
    DataRow := Self.CreateNewRow(SrcRow.GetFieldValue);
    DataRow.CopyContents(SrcRow);
    DataRow.Visible := SrcRow.Visible;
    Self.PutRow(DataRow, paDontOverwriteKeys);
    DataRow.FStatus := SrcRow.FStatus;
  end;
  AList.Free;

  for i := 0 to ASource.FDeletedRows.Count - 1 do
  begin
    DataRow := Self.CreateNewRow(TDataRow(ASource.FDeletedRows.Items[i]).GetFieldValue);
    DataRow.CopyContents(TDataRow(ASource.FDeletedRows.Items[i]));
    DataRow.Visible := TDataRow(ASource.FDeletedRows.Items[i]).Visible;
    Self.FDeletedRows.Add(DataRow);
    DataRow.FStatus := rsDeleted;
  end;
end;

function TAbstractRowStorage.GetTotal : TSubTotalRow;
begin
  Result := FTotal;
end;

function TAbstractRowStorage.CopyOfTreeOrder(Storage : TAbstractRowStorage) : TList;
var
  i : Integer;
begin
  Result := TList.Create;
  for i := 0 to FTree.Count - 1 do
  begin
    Result.Add(TRowStorageTreeKey(FTree.Items[i]).CreateCopy(Storage));
  end;
end;

procedure TAbstractRowStorage.NewUnacceptedRow(Index : Integer; Row : TDataRow);
begin
  if (Row.Status <> rsExternControlled) or
     (Row.DataTable <> Self.DataTable) or
     RowIsUnaccepted(Row) then
    Log(ltError, 'UnacceptedRow', 'TRowStorage.NewUnacceptedRow: Invalid row status!');

  FUnacceptedRows.Add(Row);
  Row.FStorage := Self;
  Row.FStatus := rsUnaccepted;

  Changed(kcNewRow, Row, Index);
end;

function TAbstractRowStorage.NewUnacceptedSubTotal(Index : Integer; SubTotalKey : TRowStorageTreeKey) : TSubTotalRow;
begin
  Result := Self.CreateSubTotal(nil, SubTotalKey.TreeKey.DataType.DefaultValue, True);

  Result.FSubTotalKey := SubTotalKey;

  Result.FSubRows.Free;
  Result.FSubRows := TValueList.Create(SubTotalKey.TreeKey.DataType);
  Result.FSubRows.Sorted := True;

  FUnacceptedRows.Add(Result);
  Result.FStorage := Self;

  Changed(kcNewRow, Result, Index);
end;

function TAbstractRowStorage.AcceptRow(Row : TAbstractRow) : TPutResult;
begin
  if RowIsUnaccepted(Row) then
  begin
    if Row is TDataRow then
    begin
      Row.FStorage := nil;
      TDataRow(Row).FStatus := rsExternControlled;
{      if Self is TRowStorage then
        Result := TRowStorage(Self).InsertRow(TDataRow(Row), paDontOverwriteKeys, -1)
      else }
        Result := PutRow( TDataRow(Row), paDontOverwriteKeys);
    end
    else
    begin
      Result := FTotal.AddSubtotalRow(TSubTotalRow(Row));
    end;

    if Result in [prValuesMissing, prKeyConflict, prIllegalKeyValue, prCannotAdd] then
    begin
      if Row is TDataRow then
        TDataRow(Row).FStatus := rsUnaccepted;
      Row.FStorage := Self
    end
    else
      FUnacceptedRows.Remove(Row);
  end
  else
    Result := prKeyConflict;
end;

procedure TAbstractRowStorage.Changed(ChangeType : TChangeType; Row : TAbstractRow; Index : Integer);
var

  aIndex : Integer;
begin
  if Self is TCachingStorage then
    // Don't care
  else if TCustomRowStorage(Self).AutoArrange then
    FRowListUpToDate := False
  else if (FRowListUpToDate) and
          (not Row.IsUpdating) then
  case ChangeType of
    kcNewRow:
    begin
      if (Index >= 0) and
         Row.Visible then
        FRowList.InsertObject(Index, '', Row);
    end;
    kcKeyUpdate:
    begin
      // Nothing here for the moment
    end;
    kcDeletedRow:
    begin
      aIndex := FRowList.IndexOfObject(Row);
      if aIndex >= 0 then
        FRowList.Delete(aIndex);
    end;
    kcVisibility:
    begin
      // Nothing here for the moment
    end;
  end;

  if ChangeType = kcDeletedRow then
    Self.FUnacceptedRows.Remove(Row);



  FLastChanged := Now;
end;

function TAbstractRowStorage.GetUnacceptedRow(idx : Integer) : TAbstractRow;
begin
  Result := TAbstractRow(FUnacceptedRows.Items[idx]);
end;

function TAbstractRowStorage.GetUnacceptedRowCount : Integer;
begin
  Result := FUnacceptedRows.Count;
end;

function TAbstractRowStorage.RowIsUnaccepted(ARow : TAbstractRow) : Boolean;
begin
  Result := (FUnacceptedRows.IndexOf(ARow) >= 0);
end;

function TAbstractRowStorage.SkakaUnacceptedIndex(Field : TDataField) : Integer;
var
  i : Integer;
begin
  Result := 0;

  for i := 0 to TreeKeyCount - 1 do
    if not DataTable.TableHasField(TreeKey[i].TreeKey) then
    begin
      if TreeKey[i].TreeKey = Field then
        Exit
      else
        inc(Result);
    end;
end;

function TAbstractRowStorage.GetInternalSubTotalHandler : TSubTotalHandler;
begin
  Result := FSubTotalHandler;
  if Result = nil then
  begin
    if FDefaultSubTotalHandler = nil then
      FDefaultSubTotalHandler := TDefaultSubTotalHandler.Create;
    Result := FDefaultSubTotalHandler;
  end;
end;

procedure TAbstractRowStorage.SetSubTotalsUnder(Under : Boolean);
begin
  if ISubTotalsUnder <> Under then
  begin
    ISubTotalsUnder := Under;
    ClearAllRowLists;
    FLastChanged := Now;
  end;
end;

procedure TAbstractRowStorage.SetShowSubTotals(Show : Boolean);
var
  i : Integer;
begin
  for i := 0 to TreeKeyCount - 1 do // not details
    TreeKey[i].Visible := Show; // Fixa LGE optimera

  ClearAllRowLists;
  FLastChanged := Now;
end;

{
function TAbstractRowStorage.GetShowSubTotal(idx : Integer) : Boolean;
begin
  Assert((idx >= 0) and (idx < FTree.Count),
         'TRowStorage.GetShowSubTotal: (idx >= 0) and (idx < FTree.Count), idx: ' + IntToStr(idx));

  Result := Self.TreeKey[idx].Visible;
end;
}

{
procedure TAbstractRowStorage.SetShowSubTotal(idx : Integer; Show : Boolean);
begin
  Assert((idx >= 0) and (idx < FTree.Count),
         'TRowStorage.SetShowSubTotal: (idx >= 0) and (idx < FTree.Count), idx: ' + IntToStr(idx));

  Self.TreeKey[idx].Visible := Show;
  FTotal.UpdateRowVisibility(idx, Show);
  FLastChanged := Now;
end;
}

{
function TAbstractRowStorage.GetShowFieldSubTotal(Field : TDataField) : Boolean;
var
  i : Integer;
begin
  Result := False;

  for i := 0 to Self.TreeKeyCount - 1 do
  if Self.TreeKey[i].TreeKey = Field then
  begin
    Result := Self.GetShowSubTotal(i);
    Exit;
  end;
end;
}

{
procedure TAbstractRowStorage.SetShowFieldSubTotal(Field : TDataField; Show : Boolean);
var
  i : Integer;
begin
  for i := 0 to Self.TreeKeyCount - 1 do
  if Self.TreeKey[i].TreeKey = Field then
  begin
    ShowSubTotal[i] := Show;
    Exit;
  end;
end;
}

function TAbstractRowStorage.GetIsAbsoluteSource : Boolean;
begin
  Result := True;
end;

procedure TAbstractRowStorage.ClearAllRowLists;

begin
  FRowListUpToDate := False;

end;

function TAbstractRowStorage.CreateSubTotal(DirectOwner : TSubTotalRow; OwnKeyValue : TValue; Visible : Boolean) : TSubTotalRow;
begin
  Result := TSubTotalRow.Create(Self, DirectOwner, OwnKeyValue, Visible);
end;

procedure TAbstractRowStorage.CreatePrepare(DataTable : TDataTable);
begin
  FLastChanged := Now;

  DataTable.Lock;

  FMonitors := TList.Create;

  FRowList := TStringList.Create;
  FRowListUpToDate := False;
  FUnacceptedRows := TList.Create;

  FDeletedRows := TList.Create;

  ISubTotalsUnder := False;

  FTree := TList.Create;
end;

constructor TAbstractRowStorage.Create(DataTable : TDataTable);
var
  i : Integer;
  KeyField : TDataField;
  TreeKeys : TRowSortOrder;
begin
  FAutoUpdateEnabled := True;

  Assert(DataTable <> nil, 'TAbstractRowStorage.Create: DataTable <> nil');

  inherited Create(DataTable, otCalcOperation);
  CreatePrepare(DataTable);

  TreeKeys := TRowSortOrder.Create;
  for i := 0 to DataTable.KeyCount - 1 do
  begin
    KeyField := DataTable.Field[i];
    TreeKeys.AddRule(KeyField, KeyField.SortOrder);
  end;
  SetTreeKeys(TreeKeys);
  TreeKeys.Free;

{  for i := 0 to DataTable.KeyCount - 1 do
  begin
    KeyField := DataTable.Field[i];
    FTree.Add(TRowStorageTreeKey.Create(Self, KeyField));
  end;
  FTree.Add(TRowStorageTreeKey.CreateDetailLevel(Self));

  CreateFinish; }
end;

function TAbstractRowStorage.AllowUpdateTreeKeys : Boolean;
begin
  Result := (FDeletedRows.Count = 0) and
            ((FTotal = nil) or (FTotal.FSubRows.Count = 0)) and
            (FMonitors.Count = 0);
end;

procedure TAbstractRowStorage.SetTreeKeys(TreeKeys : TRowSortOrder);
var
  i : Integer;
begin
  if TreeKeys = nil then
    Exit; // borde betyda default...

  if not AllowUpdateTreeKeys then
    Log(ltError, 'SetTreeKeys', 'TAbstractRowStorage.SetTreeKeys: Storage must be empty (unused) when settings TreeKeys!');

  for i := 0 to FTree.Count - 1 do
    TObject(FTree.Items[i]).Free;
  FTree.Clear;

  for i := 0 to TreeKeys.RuleCount - 1 do
  begin
    FTree.Add(TRowStorageTreeKey.CreateSorted(Self,
                                              TreeKeys.Rule[i].DataField,
                                              TreeKeys.Rule[i].DataField.SortField,
                                              TreeKeys.Rule[i].SortOrder));
  end;

  FTree.Add(TRowStorageTreeKey.CreateDetailLevel(Self));
  CalcTreeFieldPositions;

  FTreeKeys.Free;
  FTreeKeys := TRowSortOrder.CreateCopy(TreeKeys);

//  AfterSortOrderCreate( FTreeKeys, fTree );

  FCustomSortOrder.Free;
  FCustomSortOrder := TRowSortOrder.CreateCopy(FTreeKeys);
  FUsesCustomSortOrder := False;

  FTotal.Free;
  FTotal := CreateSubTotal(nil, ValueFromString(''), TreeKey[0].Visible);
end;

{
procedure TAbstractRowStorage.CreateFinish;
var
  i : Integer;
begin
  CalcTreeFieldPositions;

  FSortOrder := TRowSortOrder.Create;

  for i := 0 to TreeKeyCount - 1 do
  begin
    if TreeKey[i].SortField <> TreeKey[i].TreeKey then
      FSortOrder.AddRule(TreeKey[i].SortField, TreeKey[i].SortOrder);

    FSortOrder.AddRule(TreeKey[i].TreeKey, TreeKey[i].SortOrder);
  end;
  AfterSortOrderCreate( FSortOrder, fTree );

  FCustomSortOrder := TRowSortOrder.CreateCopy(FSortOrder);
  FUsesCustomSortOrder := False;

  FTotal := CreateSubTotal(nil, ValueFromString(''), TreeKey[0].Visible);
end;
}

{
constructor TAbstractRowStorage.CreateWithKeySortOrder(DataTable : TDataTable; KeysEditable : Boolean; KeySortOrder : TRowSortOrder);
var
  i : Integer;
begin
  Assert(DataTable <> nil, 'TAbstractRowStorage.CreateTreeKeys: DataTable <> nil');
  Assert(KeySortOrder <> nil, 'TAbstractRowStorage.CreateTreeKeys: KeySortOrder <> nil');

  inherited Create(DataTable, otCalcOperation);
  FKeysEditable := KeysEditable;
  CreatePrepare(DataTable);

  for i := 0 to KeySortOrder.RuleCount - 1 do
  begin
    FTree.Add(TRowStorageTreeKey.CreateSorted(Self,
                                              KeySortOrder.Rule[i].DataField,
                                              KeySortOrder.Rule[i].DataField.SortField,
                                              KeySortOrder.Rule[i].SortOrder));
  end;

  FTree.Add(TRowStorageTreeKey.CreateDetailLevel(Self));
  KeySortOrder.Free;

  CreateFinish;
end;


constructor TAbstractRowStorage.CreateKeySortOrder(DataTable : TDataTable; KeysEditable : Boolean; TreeKeysAndOrder : TList);
var
  i : Integer;
begin
  Assert(DataTable <> nil, 'TAbstractRowStorage.CreateKeySortOrder: DataTable <> nil');
  Assert(TList <> nil, 'TAbstractRowStorage.CreateKeySortOrder: TList <> nil');

  inherited Create(DataTable, otCalcOperation);
  FKeysEditable := KeysEditable;
  CreatePrepare(DataTable);

  for i := 0 to TreeKeysAndOrder.Count - 1 do
  begin
    if not (TObject(TreeKeysAndOrder.Items[i]) is TRowStorageTreeKey) then
      raise Exception.Create('TList TreeKeysAndOrder doesn''t contain objects of type TRowStorageTreeKey');

    FTree.Add(TreeKeysAndOrder.Items[i]);
  end;

  if (TreeKeysAndOrder.Count = 0) or
     (not TreeKey[TreeKeysAndOrder.Count - 1].IsDetailLevel) then
    FTree.Add(TRowStorageTreeKey.CreateDetailLevel(Self));
  TreeKeysAndOrder.Free;

  CreateFinish;
end;
}



destructor TAbstractRowStorage.Destroy;
var
  i : Integer;
begin
  FTotal.Free;


  FMonitors.Free;

  for i := 0 to FDeletedRows.Count - 1 do
    TObject(FDeletedRows.Items[i]).Free;
  FDeletedRows.Free;

  for i := 0 to FTree.Count - 1 do
    TObject(FTree.Items[i]).Free;
  FTree.Free;

  for i := 0 to UnacceptedRowCount - 1 do
    UnacceptedRows[i].Free;
  FUnacceptedRows.Free;

  FRowList.Free;
  // FSortOrder.Free;
  FCustomSortOrder.Free;
  fTreeKeys.Free; // LAA.added

  inherited Destroy;
end;



function TAbstractRowStorage.GetMatchCount(Cond : TCondition) : Integer;
var
  List : TStrings;
begin
  List := TStringList.Create;
  GetRows(List, Cond, gaReference);
  Result := List.Count;
  List.Free;
end;

function TAbstractRowStorage.SumValues(Cond : TCondition; AField : TDataField) : TValue;
var
  List : TStrings;
  i : Integer;
begin
  List := TStringList.Create;
  GetRows(List, Cond, gaReference);
  Result := AField.DataType.DefaultValue;
  for i := 0 to List.Count - 1 do
    Result := AField.DataType.Sum(Result, TDataRow(List.Objects[i])[AField]);
  List.Free;
end;

          // Fixa LGE titta var frowlist används och se om vi behöver kolla frowlistuptodate

function TAbstractRowStorage.GetSortOrder : TRowSortOrder;
begin
  Result := FTreeKeys;
//  Result := FSortOrder;
end;



procedure TAbstractRowStorage.DeleteAll;
var
  Patch : TQuiltPatch;
  List : TStrings;
begin
  if Self = nil then
    Exit;

  List := TStringList.Create;
  Patch := TQuiltPatch.Create;
  GetRows(List, Patch, gaDelete);
  Patch.Free;
  List.Free;
end;

procedure TAbstractRowStorage.GetRows(Results : TStrings; Condition : TCondition; Action : TGetAction);
var
  Handler : TGetRowsHandler;
//  DefaultSort : Boolean;
//  DelCond : Boolean;
  Dummy : TStringList;
begin
  Dummy := nil;
  if (Action = gaDelete) then
    if Results = nil then
    begin
      Dummy := TStringList.Create;
      Results := Dummy;
    end
  else
    Assert(Results <> nil, 'TAbstractRowStorage.GetRows: Results <> nil');

  Handler := TGetRowsHandler.Create(Self, Condition, Results, Action);
  Self.BeforeGetDataByCrit(Handler.Condition);
  FTotal.GetRows(Handler);
  Handler.Free;
  Dummy.Free;
end;

function TAbstractRowStorage.ForcePutRow(ARow : TDataRow; PutAction : TPutAction) : TPutResult;
begin
  Result := FTotal.ForcePutRow(ARow, PutAction);
end;

procedure TAbstractRowStorage.ForceRemoveRow(ARow : TDataRow);
begin
  if FTotal.ForceRemoveRow(ARow) then
    ARow.FStatus := rsExternControlled;
end;

// Fixa LGE ta veke
procedure TAbstractRowStorage.GetRowsByKeyBegin(Results : TStrings; Criteria : TCondition; Action : TGetAction; BeginField : TDataField; BeginWith : String);
var
  AQuilt : TCommonQuilt;
  TmpQField : TQuiltField;
begin
  if BeginWith = '' then
    AQuilt := nil
  else
  begin
    if Criteria <> nil then
      AQuilt := Criteria.CreateCommonQuilt
    else
      AQuilt := TQuiltPatch.Create;


    if Length(BeginWith) >= BeginField.DataType.DataSize then
    begin
      TmpQField := TQuiltField.Create(BeginField);
      TmpQField.Add(ValueFromString(BeginWith));
    end
    else
    begin
      TmpQField := TQuiltField.Create(BeginField);
      TmpQField.AcceptAll;
      AQuilt := AQuilt.DoCreateUnion(TmpQField);

      TmpQField := TQuiltField.Create(BeginField);
      AddBeginInterval( TmpQField, BeginWith );
    end;

    AQuilt := AQuilt.DoCreateIntersection(TmpQField);
  end;

  GetRows(Results, AQuilt, Action);

  AQuilt.Free;
end;
(*
var
  DefaultSort : Boolean;
  CritCopy : TCriteria;
  DelCrit : Boolean;
begin
  if (Criteria <> nil) and not (Criteria is TCriteria) then
    raise Exception.Create('Criteria expected!');

  Assert(Results <> nil, 'TAbstractRowStorage.GetRowsByKeyBegin: Results <> nil');
  Assert(BeginField <> nil, 'TAbstractRowStorage.GetRowsByKeyBegin: BeginField <> nil');

  if Action = gaDelete then
    DefaultSort := False
  else
    DefaultSort := {DataTable.} CanDefaultSort;

  DelCrit := (Criteria = nil);
  if DelCrit then
    Patch := TQuiltPatch.Create;

  CritCopy := TCriteria(Criteria).CreateCopyOfCriteria;
  if BeginField <> nil then
    CritCopy[BeginField].Reset;
  Self.BeforeGetDataByCrit(CritCopy);
  CritCopy.Free;

  FTotal.GetRowsByKeyBegin(Results, Criteria, Action, DataTable.IndexOfField(BeginField), BeginWith, True, True, DefaultSort);

  if DelCrit then
    Patch.Free;
end;
*)

(*
procedure TAbstractRowStorage.GetAndOrderRows(Results : TStrings; Condition : TCondition;
                                         Order : array of TOrderField; Action : TGetAction);
var
  i : Integer;
  DefaultSort : Boolean;
  DelCond : Boolean;
begin
  Assert(Results <> nil, 'TAbstractRowStorage.GetAndOrderRows: Results <> nil');

  {if Action = gaDelete then
    Action := gaCut; }

  DefaultSort := True;

  for i := Low(Order) to High(Order) do
    if Order[i].Field <> nil then
    begin
      DefaultSort := False;
      break;
    end;

  DefaultSort := DefaultSort and DataTable.CanDefaultSort;

  DelCond := (Condition = nil);
  if DelCond then
    Condition := TQuiltPatch.Create;

  Self.BeforeGetDataByCrit(Condition);
  FTotal.GetRows(Results, Condition, Action, DefaultSort);

  if not DefaultSort then
    TDataRowList.OrderRows(Results, Order, 0, Results.Count - 1);

  if DelCond then
    Condition.Free;
end;
*)

procedure TAbstractRowStorage.DeleteRows(Condition : TCondition);
var
  Rows : TStrings;
  i : Integer;
begin
  Rows := TStringList.Create;
  Self.GetRows(Rows, Condition, gaReference);
  for i := 0 to Rows.Count - 1 do
    TDataRow(Rows.Objects[i]).Delete;
  Rows.Free;
end;

procedure TAbstractRowStorage.CalcTreeFieldPositions;
var
  i : Integer;
begin
  for i := 0 to TreeKeyCount - 1 do
    TreeKey[i].SetTable(Self);
end;

function TAbstractRowStorage.GetTreeKey(idx : Integer) : TRowStorageTreeKey;
begin
  Assert((idx >= 0) and (idx < FTree.Count),
         'TAbstractRowStorage.GetTreeKey: (idx >= 0) and (idx < FTree.Count), idx: ' + IntToStr(idx));

  Result := TRowStorageTreeKey(FTree.Items[idx]);
end;

function TAbstractRowStorage.GetTreeKeyByField(Field : TDataField) : TRowStorageTreeKey;
var
  i : Integer;
begin
  for i := 0 to TreeKeyCount do
    if TreeKey[i].TreeKey = Field then
    begin
      Result := TreeKey[i];
      Exit;
    end;

  Result := nil;
end;

function TAbstractRowStorage.GetDetailTreeKey : TRowStorageTreeKey;
begin
  Result := TreeKey[TreeKeyCount];
end;

function TAbstractRowStorage.GetTreeKeyCount : Integer;
begin
  Result := FTree.Count - 1; // Note: It shall be -1!!
end;

function TAbstractRowStorage.CreateNewRow(QueryValueFunction : TQueryValueFunction) : TDataRow;
begin
  Result := TDataRow.Create(Self.DataTable);
  Result.Visible := DetailTreeKey.Visible;
end;

procedure TAbstractRowStorage.MapLargestValues(ResultRow : TDataRow; SrcRow : TAbstractRow);
  procedure SetLargest(ResultRow : TDataRow; SrcRow : TAbstractRow; FieldIdx : Integer);
  begin
    // JagLAA är medveten om att det är dumt att söka displaytext med field och
    // inte index men duLGE har inte kodat det åt mig :)

    if (DataTable.Field[FieldIdx].TextField<>nil) or (DataTable.Field[FieldIdx].DataType is TStringType) then
    begin
      if Length(SrcRow.DisplayText[DataTable.Field[FieldIdx]]) >
         Length(ResultRow.DisplayText[DataTable.Field[FieldIdx]]) then
        ResultRow.ValueByIndex[FieldIdx] := SrcRow.ValueByIndex[FieldIdx];
    end
    else if DataTable.Field[FieldIdx].DataType is TDateTimeType then
    begin
      if Length(AsString(SrcRow.ValueByIndex[FieldIdx])) >
         Length(AsString(ResultRow.ValueByIndex[FieldIdx])) then
        ResultRow.ValueByIndex[FieldIdx] := SrcRow.ValueByIndex[FieldIdx];
    end
    else if DataTable.Field[FieldIdx].DataType is TNumericType then
    begin
      if Abs(AsDouble(SrcRow.ValueByIndex[FieldIdx])) >
         Abs(AsDouble(ResultRow.ValueByIndex[FieldIdx])) then
        ResultRow.ValueByIndex[FieldIdx] := SrcRow.ValueByIndex[FieldIdx];
    end;
  end;

var
  idx : Integer;
begin
{  for idx := 0 to SrcRow.ValidKeyCount - 1 do
    if TreeKey[idx].RowFieldIndex >= 0 then
      SetLargest(ResultRow, SrcRow, TreeKey[idx].RowFieldIndex);

  for idx := DataTable.KeyCount to DataTable.FieldCount -1 do
    SetLargest(ResultRow, SrcRow, idx); }

  for idx := 0 to DataTable.FieldCount - 1 do
    if SrcRow.FieldHasValue(DataTable.Field[idx]) then
      SetLargest(ResultRow, SrcRow, idx);
end;







(*
procedure TAbstractRowStorage.LoadFromPRS(FileName : String);
var
  BytesRead : Integer;
  InFile : TFileStream;
  Row : TDataRow;
begin
  InFile := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  while True do
  begin
    Row := TDataRow.Create(DataTable);

    BytesRead := InFile.Read(Row.FData^, DataTable.DataLength);
    if BytesRead < DataTable.DataLength then
    begin
      Row.Free;
      Break;
    end
    else
    begin
      ForcePutRow(Row, paDontOverwriteKeys);
      Row.FStatus := rsNew; // rsUnchanged;
    end;
  end;

  InFile.Free;
end;

procedure TAbstractRowStorage.SaveToPRS(FileName : String);
var
  i : Integer;
  List : TStringList;
  Crit : TQuiltPatch;
  OutFile : TFileStream;
begin
  Crit := TQuiltPatch.Create;
  List := TStringList.Create;
  GetRows(List, Crit, gaReference);
  OutFile := TFileStream.Create(FileName, fmCreate or fmShareExclusive);

  for i := 0 to List.Count - 1 do
  begin
    OutFile.Write(TDataRow(List.Objects[i]).FData^, DataTable.DataLength);
  end;

  OutFile.Free;
  Crit.Free;
  List.Free;
end;
*)













function TAbstractRowStorage.ContainsChanges : Boolean;
begin
  if UnacceptedRowCount > 0 then
    Result := True
  else
    Result := (FDeletedRows.Count > 0) or
              FTotal.ContainsChanges;
end;

function TAbstractRowStorage.GetStatusCount(Status : TRowStatus) : Integer;
var
  i : Integer;
begin
  case Status of
    rsExternControlled: if Self is TCustomRowStorage then
                          Result := TCustomRowStorage(Self).UnacceptedRowCount
                        else
                          Result := 0;
    rsUnaccepted: begin
                    Result := 0;
                    for i := 0 to UnacceptedRowCount - 1 do
                      if UnAcceptedRows[i] is TDataRow then
                        Inc(Result);
                  end;

    rsDeleted: Result := FDeletedRows.Count;
  else
    Result := FTotal.GetStatusCount(Status);
  end;
end;

procedure TAbstractRowStorage.AddRowsToList(AList : TStrings; RowStatus : TRowStatus);
var
  i : Integer;
begin
  case RowStatus of
    rsExternControlled: ; // No code needed
    rsUnaccepted: for i := 0 to UnacceptedRowCount - 1 do
                    if UnAcceptedRows[i] is TDataRow then
                      AList.AddObject('', UnAcceptedRows[i]);

    rsDeleted: for i := 0 to FDeletedRows.Count - 1 do
                 AList.AddObject('', TDataRow(FDeletedRows.Items[i]));
  else
    FTotal.AddRowsToList(AList, RowStatus);
  end;
end;

function TAbstractRowStorage.ContainsRow(ARow : TDataRow) : Boolean;
begin
  if ARow = nil then
    Result := False
  else
    Result := (ARow.Storage = Self);
end;







function TAbstractRowStorage.LocateRow(Keys : array of String) : TDataRow;
begin
  if High(Keys) - Low(Keys) + 1 < DataTable.KeyCount then
  begin
    Log(ltWarning, 'LocateRow', 'LocateRow: Too few elements in array!');
    Result := nil;
  end
  else
  begin
    Self.BeforeGetDataByArray(Keys);
    Result := FTotal.LocateRow(Keys, Low(Keys));
  end;
end;

function TAbstractRowStorage.LocateByRowValues(ValueRow : TAbstractRow; DerivedKeys : array of TDataField) : TDataRow;
begin
  Self.BeforeGetDataByRow(ValueRow, DerivedKeys);
  Result := FTotal.LocateByRowValues(ValueRow, DerivedKeys);
end;

{$ifdef D4_OR_HIGHER}
function TAbstractRowStorage.LocateRowByCriteria(Crit : TCondition; UseFirstRowOnMultipleHits : Boolean = False) : TDataRow;
{$else}
function TAbstractRowStorage.LocateRowByCriteria(Crit : TCondition; UseFirstRowOnMultipleHits : Boolean) : TDataRow;
{$endif D4_OR_HIGHER}
var
  List : TStringList;
begin
  List := TStringList.Create;
  GetRows(List, Crit, gaReference);
  if List.Count = 1 then
    Result := TDataRow(List.Objects[0])
  else if List.Count = 0 then
    Result := nil
  else if UseFirstRowOnMultipleHits and (List.Count > 0) then
    Result := TDataRow(List.Objects[0])
  else
  begin
    List.Free;
    raise Exception.Create('More than one match!');
  end;

  List.Free;
end;

function TAbstractRowStorage.KeysExist(Keys : array of String) : Boolean;
begin
  Self.BeforeGetDataByArray(Keys);
  Result := FTotal.KeysExist(Keys, Low(Keys));
end;

function TAbstractRowStorage.HasRowsThatMatch(Condition : TCondition) : Boolean;
var
  Handler : TGetRowsHandler;
begin
  Handler := TGetRowsHandler.Create(Self, Condition, nil, gaDelete);
  Self.BeforeGetDataByCrit(Handler.Condition);
  Result := FTotal.HasRowsThatMatch(Handler);
  Handler.Free;
end;



procedure TAbstractRowStorage.StartOperations;
var
  NoPatch : TQuiltPatch;
begin
  FRowSourceRows := TStringList.Create;
  NoPatch := TQuiltPatch.Create;
  GetRows(FRowSourceRows, NoPatch, gaReference);
  NoPatch.Free;
  FCurrentRow := 0;
end;

function TAbstractRowStorage.GetNextRow : TDataRow;
begin
  if FCurrentRow < FRowSourceRows.Count then
  begin
    Result := TDataRow(FRowSourceRows.Objects[FCurrentRow]);
    Inc(FCurrentRow);
  end
  else
  begin
    FRowSourceRows.Free;
    FRowSourceRows := nil;
    Result := nil;
  end;
end;

(*
procedure TAbstractRowStorage.DoBeforeGetData(GetCriteria : TCriteria);
begin
  if Assigned(FBeforeGetData) then
    FBeforeGetData(Self, GetCriteria);
end;
*)

procedure TAbstractRowStorage.BeforeGetDataByCrit(Condition : TCondition);
begin
end;

procedure TAbstractRowStorage.BeforeGetDataByArray(GetValues : array of String);
begin
end;

procedure TAbstractRowStorage.BeforeGetDataByRow(Row : TAbstractRow; DerivedKeys : array of TDataField);
begin
end;

procedure TAbstractRowStorage.SetCustomSortOrder(ARowSortOrder : TRowSortOrder);
begin
  FCustomSortOrder.CopyFrom(ARowSortOrder);
end;

procedure TAbstractRowStorage.SetUsesCustomSortOrder(Value : Boolean);
begin
  FUsesCustomSortOrder := Value;
end;

{
procedure TAbstractRowStorage.AfterSortOrderCreate(SortOrder : TRowSortOrder; Tree : TList);
begin
end;
}

// -------------------------- TAbstractRow -------------------------------------

constructor TAbstractRow.Create(DataTable : TDataTable);
begin
  inherited Create;

  FSpecialValueList := nil;
  FStorage := nil;
  FSubTotalRow := nil;
  FVisible := True;
  FDataTable := DataTable;
  GetMem(Self.FData, DataTable.DataLength);
end;

destructor TAbstractRow.Destroy;
begin
  inherited Destroy;

  FreeMem(Self.FData);
  FSpecialValueList.Free;
end;

function TAbstractRow.AllFieldValues(ATable:TDataTable=nil) : String;
var
  i : Integer;
  Field : TDataField;
begin
  Result := KeyFieldValues(ATable) + '/';

  for i := DataTable.KeyCount to DataTable.FieldCount - 1 do
  begin
    Field := DataTable.Field[i];
    if (ATable=nil) or (ATable.TableHasField(Field)) then
      Result := Result + Field.FieldName + '=' + Field.DataType.AsSQL( Value[Field] ) + ', ';
  end;

  if Length(Result) > 0 then
    Result := Copy(Result, 1, Length(Result) - 2);
end;

{/** Prints the values for each key in the row. If a table is given
     as parameter, only the fields present in the tables are considered. */}
function TAbstractRow.KeyFieldValues(ATable:TDataTable=nil) : String;
var
  i : Integer;
  Field : TDataField;
begin
  Result := '';
  for i := 0 to ValidKeyCount - 1 do
  begin
    if Storage = nil then
      Field := DataTable.Field[i]
    else
      Field := Storage.TreeKey[i].TreeKey;

    if (ATable=nil) or (ATable.TableHasField(Field)) then
      Result := Result + Field.FieldName + '=' + Field.DataType.AsSQL(ValidKeyValue[i]) + ', ';
  end;

{  for i := DataTable.KeyCount to DataTable.FieldCount - 1 do
  //if DataTable.Field[i] is TKeyField then
  begin
    Result := Result + DataTable.Field[i].FieldName + '=' + SQLByIndex[i] + ', ';
  end; }

  if Length(Result) > 0 then
    Result := Copy(Result, 1, Length(Result) - 2);
end;

{
function TAbstractRow.KeyFieldValuesOnly : String;
var
  i : Integer;
begin
  Result := '';
  for i := 0 to ValidKeyCount - 1 do
  begin
    Result := Result + DataTable.Field[i].FieldName + '=' + SQLByIndex[i] + ', ';
  end;

  if Length(Result) > 0 then
    Result := Copy(Result, 1, Length(Result) - 2);
end;
}

procedure TAbstractRow.SetVisible(Vis : Boolean);
begin
  if Vis <> FVisible then
  begin
    FVisible := Vis;
    if FStorage <> nil then
      FStorage.Changed(kcVisibility, Self, -1);
  end;
end;

// ---- Primitive Get/Set methods

procedure TAbstractRow.SetIValue(idx : Integer; Value : TValue);
begin
  SetIndexValue(idx, Value, saDontOverwriteKeys);
end;

procedure TAbstractRow.StoreValue(idx : Integer; Value : TValue);
begin
  DataTable.Field[idx].DataType.StoreValue(@Self.FData[DataTable.ReadPos[idx]], Value, StoreSpecialValue, idx);
end;

function TAbstractRow.FetchValue(idx : Integer) : TValue;
var
  AField : TDataField;
begin
  AField := DataTable.Field[idx];
  if AField is TFictiveField then
  begin
    Result := GetFieldValue(TFictiveField(AField).CalcField);
    Exit;
  end;

  BeforeFetchValue(AField);

  if not ((Self is TSubTotalRow) and
          (Storage is TRowStorage) and
          (idx >= 0) and (idx < DataTable.KeyCount) and
          TRowStorage(Storage).KeyCriteria.AcceptsExactlyOneValue(AField, Result)) then
    Result := AField.FetchValue(Self, idx);
end;

function TDataField.FetchValue(ARow : TAbstractRow; idx : Integer) : TValue;
begin
  Result := Self.DataType.FetchValue(@ARow.FData[ARow.DataTable.ReadPos[idx]], ARow.ReadSpecialValue, idx);
end;

procedure TAbstractRow.BeforeFetchValue(Field : TDataField);
begin
  // Nothing by default
end;

procedure TAbstractRow.SetFValue(Field : TDataField; Value : TValue);
begin
  SetFieldValue(Field, Value, saDontOverwriteKeys);
end;

procedure TAbstractRow.SetFStringValue(Field : TDataField; Value : String);
begin
  Assert(Field <> nil, 'TDataRow.SetFStringValue: Field <> nil');

  SetFieldStringValue(Field, Value, saDontOverwriteKeys);
end;

procedure TAbstractRow.SetFDoubleValue(Field : TDataField; Value : Double);
begin
  Assert(Field <> nil, 'TDataRow.SetFDoubleValue: Field <> nil');

  SetFieldDoubleValue(Field, Value, saDontOverwriteKeys);
end;

procedure TAbstractRow.SetFIntValue(Field : TDataField; Value : Integer);
begin
  Assert(Field <> nil, 'TDataRow.SetFIntValue: Field <> nil');

  SetFieldIntValue(Field, Value, saDontOverwriteKeys);
end;

procedure TAbstractRow.SetFCurrencyValue(Field : TDataField; Value : Currency);
begin
  Assert(Field <> nil, 'TDataRow.SetFCurrencyValue: Field <> nil');

  SetFieldCurrencyValue(Field, Value, saDontOverwriteKeys);
end;

procedure TAbstractRow.SetFDateTimeValue(Field : TDataField; Value : TDateTime);
begin
  Assert(Field <> nil, 'TDataRow.SetFDateTimeValue: Field <> nil');

  SetFieldDateTimeValue(Field, Value, saDontOverwriteKeys);
end;

procedure TAbstractRow.SetFBooleanValue(Field : TDataField; Value : Boolean);
begin
  Assert(Field <> nil, 'TDataRow.SetFBooleanValue: Field <> nil');

  SetFieldBooleanValue(Field, Value, saDontOverwriteKeys);
end;

procedure TAbstractRow.SetFPointerValue(Field : TDataField; Value : Pointer);
begin
  Assert(Field <> nil, 'TDataRow.SetFPointerValue: Field <> nil');

  SetFieldPointerValue(Field, Value, saDontOverwriteKeys);
end;

function TAbstractRow.Lookup(var AField : TDataField) : TAbstractRow;
var
  CurrField : TDataField;
  LookupTable : TAuxTable;
  i, j : Integer;
  Patch : TQuiltPatch;
  List : TStringList;
begin
  i := DataTable.FLookupFields.IndexOf(AField);
  if i > -1 then
  begin
    AField := TDataField(DataTable.FLookupSources[i]);
    Result := Self;
    Exit;
  end;

  if (AField.AuxTable = Self.DataTable) and
     (AField.AuxTableField <> nil) then
  begin
    i := IndexOfField(AField.AuxTableField);
    if i >= 0 then
    begin
      AField := AField.AuxTableField;
      Result := Self;
      Exit;
    end;
  end;

  if AField.LookupTable = nil then
  begin
    Log(ltError, 'Lookup', 'Not enough info on field to perform lookup! Row: ' + Self.DataTable.TableName + 'Row, Field: ' + AField.FieldName);
    Result := nil;
    Exit;
  end;

  LookupTable := AField.LookupTable;

  Patch := TQuiltPatch.Create;
  for i := 0 to LookupTable.KeyCount - 1 do
  begin
    CurrField := LookupTable.Field[i];

    if (AField.DependentLookupField <> nil) and
       (AField.DependentLookupField.AuxTableField = CurrField) and
       ((Self.IndexOfField(AField.DependentLookupField) >= 0) or
        (Self.DataTable.FindDependentSource(AField.DependentLookupField) <> nil)) then
    begin
      CurrField := AField.DependentLookupField;
    end
    // Ifall CurrField är t.ex. CC, men vår rad bara innehåller RC, så använder vi RC:s värde för CC istället
    else if Self.IndexOfField(CurrField) < 0 then
    begin
      for j := 0 to CurrField.DerivedFieldCount - 1 do               // fixa LGE vi borde kunna skaka rätt fält i buacre-fallet
        if (Self.IndexOfField(CurrField.DerivedField[j]) >= 0) or
           (Self.DataTable.FindDependentSource(CurrField.DerivedField[j]) <> nil) then
        begin
          CurrField := CurrField.DerivedField[j];
          Break;
        end;
      // Ifall vi inte hittade CC försöker den looka up vidare (Self.StringValue[] nedan).
      // I fallet CC kommer det dock att excepta, eftersom CC har LookupTable = nil.
    end;

    Patch[LookupTable.Field[i]].Add(Self[CurrField]);
  end;

  List := TStringList.Create;
  LookupTable.Cache.GetRows(List, Patch, gaReference);
  if List.Count = 0 then
    Result := nil
  else
    Result := TDataRow(List.Objects[0]);

  // Result := List.FirstRow;
  List.Free;
  Patch.Free;
end;

// ---- Standard methods

function TAbstractRow.IndexOfField(Field : TDataField) : Integer;
begin
  Assert(Field <> nil, 'TAbstractRow.IndexOfField: Field <> nil');

  if Self = nil then
    Result := -1
  else
    Result := DataTable.IndexOfField(Field);
end;

function TAbstractRow.LegalIndexOfField(Field : TDataField) : Integer;
begin
  Result := IndexOfField(Field);
  if Result = -1 then
    Log(ltError, 'Fields', 'Row doesn''t contain field ' + Field.FieldName);
end;

function TDataField.GetExternValue(ARow : TAbstractRow) : TValue;
var
  Field : TDataField;
  ParamRow : TAbstractRow;
  idx : Integer;
begin
  if (ARow is TSubTotalRow) and TSubTotalRow(ARow).IsTreeKeyOnly(Self, Result) then
  begin
    // Ok
  end
  else if ARow = nil then
  begin
    raise Exception.Create('TDataField.GetExternValue: ARow = nil! (' + Self.ClassName + ' : ' + Self.FieldName + ')');
  end
  else if (ARow.Storage is TRowStorage) and
          (TRowStorage(ARow.Storage).KeyCriteria <> nil) and
          TRowStorage(ARow.Storage).KeyCriteria.AcceptsExactlyOneValue(Self, Result) then
  begin
    // Ok
  end
  else
  begin
    // LGE FIXA
    // LAA+MVJ testar lite. När man lookuppar ORG1RC ur BUTCCE så skiter det sig
    // Dvs den genererar en evig loop eftersom man i Lookup(field) går till
    // LookupTable (BUTCCE) men inte heller där finns ORG1RC.
    Field := Self;
    ParamRow := ARow;

    if (ARow.DataTable = Field.LookupTable) then
    begin
      idx := ParamRow.IndexOfField(Field.AuxTableField );
      if idx >= 0 then
      begin
        Result := ParamRow.ValueByIndex[idx];
        Exit;
      end;
    end;

    ARow := ParamRow.Lookup(Field);
    if ARow = nil then
      Result := DataType.DefaultValue
    else
      Result := ARow.Value[Field];
  end;
end;

// ---- Midlayer Get/Set methods

function TAbstractRow.GetFieldValue(Field : TDataField) : TValue;
{$ifndef COMMONLASKU}
var
  idx : Integer;
{$endif COMMONLASKU}
begin
  Assert(Field <> nil, 'TAbstractRow.GetFieldValue: Field <> nil');
{$ifdef COMMONLASKU}
  if Self = nil then
    Result := GetFValue(Field)
  else
    Result := GetFValueVirt(Field);
end;

function TAbstractRow.GetFValue(Field : TDataField) : TValue;
var
  idx : Integer;
begin
{$endif COMMONLASKU}
  if Field is TFictiveField then
  begin
    Result := GetFieldValue(TFictiveField(Field).CalcField);
  end
  else
  begin
    if Self = nil then
      idx := -1
    else
      idx := IndexOfField(Field);

    if (idx >= 0) then
      Result := ValueByIndex[idx]
    else if (Self = nil) then
      Result := Field.GetExternValue(Self)
    else if (DataTable.IndexOfField(Field) >= 0) then
    begin
      if not ((Storage is TRowStorage) and
               TRowStorage(Storage).KeyCriteria.AcceptsExactlyOneValue(Field, Result)) then
        Result := Field.DataType.DefaultValue;
    end
    else if (Self is TSubTotalRow) and
            Storage.RowIsUnaccepted(Self) and
            (Storage.TreeKeyByField[Field] <> nil) and
            (TSubTotalRow(Self).__UnacceptedRowExternSkipField <> Field) then
      Result := TSubTotalRow(Self).FUnacceptedRowExternValues.Values[Storage.SkakaUnacceptedIndex(Field)]
    else
      Result := Field.GetExternValue(Self);
  end;
end;

{$ifdef COMMONLASKU}
function TAbstractRow.GetFValueVirt(Field : TDataField) : TValue;
begin
  Result := GetFValue(Field);
end;
{$endif COMMONLASKU}

function TAbstractRow.GetFieldStringValue(Field : TDataField) : String;
begin
  Result := AsString(GetFieldValue(Field));
end;

function TAbstractRow.GetFieldIntValue(Field : TDataField) : Integer;
begin
  Result := AsInteger(GetFieldValue(Field));
end;

function TAbstractRow.GetFieldDoubleValue(Field : TDataField) : Double;
begin
  Result := AsDouble(GetFieldValue(Field));
end;

function TAbstractRow.GetFieldDateTimeValue(Field : TDataField) : TDateTime;
begin
  Result := AsDateTime(GetFieldValue(Field));
end;

function TAbstractRow.GetFieldCurrencyValue(Field : TDataField) : Currency;
begin
  Result := AsCurrency(GetFieldValue(Field));
end;

function TAbstractRow.GetFieldBooleanValue(Field : TDataField) : Boolean;
begin
  Result := AsBoolean(GetFieldValue(Field));
end;

function TAbstractRow.GetFieldPointerValue(Field : TDataField) : Pointer;
begin
  Result := AsPointer(GetFieldValue(Field));
end;

function TAbstractRow.IndexEqual(idx : Integer; CmpRow : TAbstractRow) : Boolean;
begin
  Assert(CmpRow <> nil,
         'TAbstractRow.IndexEqual: CmpRow <> nil');
  Assert((idx >= 0) and (idx < DataTable.FieldCount),
         'TAbstractRow.IndexEqual: (idx >= 0) and (idx < DataTable.FieldCount), idx: ' + IntToStr(idx));

  Result := DataTable.Field[idx].DataType.Equals(Self.ValueByIndex[idx], CmpRow.ValueByIndex[idx]);
end;

(*
function BitCompare(P1, P2 : PChar; CompareLength : Integer) : Integer;
var
  i : Integer;
begin
  Result := 0;

  for i := 0 to CompareLength - 1 do
  if P1[i] <> P2[i] then
  begin
    Result := Ord(P2[i]) - Ord(P1[i]);
    Break;
  end;
end;
*)

function TAbstractRow.CompareToValues(Values : TValueList; Order : TRowSortOrder) : Integer;
var
  ThisVal, CompVal : TValue;
  i : Integer;
begin
  Order.DataTable := DataTable;

  for i := 0 to Order.RuleCount - 1 do
  begin
    try
      ThisVal := Order[i].GetValue(Self);
      CompVal := Values[i]
    except
      Continue; // Fixa LGE man kunde ju fundera hur det skall bete sig ifall vi jmf en subtotal med en detail
    end;

    Result := Order[i].DataField.DataType.Compare(ThisVal, CompVal);
    if Result <> 0 then
    begin
      if Order[i].SortOrder <> soAscending then
        Result := -Result;
      Exit;
    end;
  end;

  Result := 0;
end;

function TAbstractRow.Compare(ARow : TAbstractRow; Order : TRowSortOrder) : Integer;
var
  ThisVal, CompVal : TValue;
  i : Integer;
begin
  Order.DataTable := DataTable;
  Assert(ARow <> nil, 'TAbstractRow.Before: ARow <> nil');

  for i := 0 to Order.RuleCount - 1 do
  begin
    try
      ThisVal := Order[i].GetValue(Self);
      CompVal := Order[i].GetValue(ARow);
    except
      Continue; // Fixa LGE man kunde ju fundera hur det skall bete sig ifall vi jmf en subtotal med en detail
    end;

    Result := Order[i].DataField.DataType.Compare(ThisVal, CompVal);
    if Result <> 0 then
    begin
      if Order[i].SortOrder <> soAscending then
        Result := -Result;
      Exit;
    end;
  end;

  Result := 0;
end;

function TAbstractRow.GetKeyAndDescription(AField : TDataField) : String;
begin
  Result := GetKeyAndDescriptionWithTextField(AField, AField.TextField);
end;

function TAbstractRow.GetKeyAndDescriptionWithTextField(AField, ATextField : TDataField) : String;
var
  AuxTableRow : TAbstractRow;
  CanHaveText : Boolean;
begin
  Assert(AField <> nil, 'TDataRow.GetKeyAndDescription: AField <> nil');

  if ATextField = nil then
    Result := StringValue[AField]
  else
  begin
    AuxTableRow := GetAuxTableRow(AField, ATextField, CanHaveText);

    {if not AField.HasAuxTable then
      AuxTableRow := nil
    else if AField.AuxTable = DataTable then
    begin
      AuxTableRow := Self;
    end
    else if DataTable.TableHasField(ATextField) then
    begin
      AuxTableRow := Self;
    end
    else
      AuxTableRow := AField.AuxTable.Cache.LocateByRowValues(Self, [AField]); }

    if not CanHaveText then
      Result := Trim(StringValue[AField])
    else if ATextField is TCalcField then
      Result := Trim(Self.StringValue[AField] + ' ' + Self.StringValue[ATextField])
    else if AuxTableRow <> nil then
      Result := Trim(Self.StringValue[AField] + ' ' + AuxTableRow.StringValue[ATextField])
    else
      Result := Trim(StringValue[AField]);
  end;
end;

function TAbstractRow.IsClosable : Boolean;
begin
  Result := FSubTotalRow <> nil;
end;

function TAbstractRow.GetDisplayText(AField : TDataField) : String;
begin
  Result := GetDisplayString(AField, AField.DisplayValues);
end;

function TAbstractRow.GetDisplayString(AField : TDataField; DisplayValues : TDisplayValues) : String;
begin
  Result := GetDisplayStringWithTextField(AField, AField.TextField, DisplayValues);
end;

function TAbstractRow.GetAuxTableRow(AField, ATextField : TDataField; out CanHavetext : Boolean) : TAbstractRow;
begin
  CanHaveText := True;

  if AField.HasAuxTable then
  begin
    if Self.DataTable.TableHasField(ATextField) or (AField.AuxTable = DataTable) then
      Result := Self
    else
    begin
      Result := AField.AuxTable.Cache.LocateByRowValues(Self, [AField]);

      if (Result = nil) and
         (not (AField is TCalcField)) and // Fixa LGE Funtsa är detta vettigt
         (ATextField.LookupTable = AField.AuxTable) then
        CanHaveText := False;
    end;
  end
  else
    Result := nil;
end;

function TAbstractRow.GetDisplayStringWithTextField(AField, ATextField : TDataField; DisplayValues : TDisplayValues) : String;
var
  AuxTableRow : TAbstractRow;
  CanHaveText : Boolean;
begin
  if DisplayValues = dvDefault then
    DisplayValues := AField.GetDefaultDisplayValues;

  if (ATextField = nil) and
     (DisplayValues in [dvKeyAndText, dvTextOnly]) then
    DisplayValues := dvKeyOnly;

  case DisplayValues of
    dvKeyOnly:    Result := Self.StringValue[AField];

    dvKeyAndText:
    begin
      Result := GetKeyAndDescriptionWithTextField(AField, ATextField);
    end;

    dvTextOnly:
    begin
      AuxTableRow := GetAuxTableRow(AField, ATextField, CanHaveText);

      if not CanHaveText then
        Result := Self.StringValue[AField]
      else if ATextField is TCalcField then
        Result := Self.StringValue[ATextField]
      else if AuxTableRow <> nil then
        Result := AuxTableRow.StringValue[ATextField]
      else
        Result := Self.StringValue[AField];
    end;
  else
    Result := Self.StringValue[AField];
  end;
end;

function TAbstractRow.GetShortDisplayString(AField : TDataField; DisplayValues : TDisplayValues) : String;
begin
  Result := GetDisplayStringWithTextField(AField, AField.ShortTextField, DisplayValues);
end;

function TAbstractRow.GetShortDisplayText(AField : TDataField) : String;
{var
  AuxTableRow : TDataRow;
  DisplayValues : TDisplayValues; }
begin
  Result := GetShortDisplayString(AField, AField.DisplayValues);
end;

function TAbstractRow.GetEditable(Field : TDataField) : Boolean;
var
  ReadOnlyReason : String;
begin
  Result := CanEditValue(Field, ReadOnlyReason);
end;

function TAbstractRow.GetFieldSQLValue(Field : TDataField) : String;
begin
  Result := Field.DataType.AsSQL(Self.Value[Field]);
end;

function TAbstractRow.GetSQLIndexValue(idx : Integer) : String;
begin
  Result := DataTable.Field[idx].DataType.AsSQL(Self.ValueByIndex[idx]);
end;

function TAbstractRow.SpecialValueList(MinIndex : Integer) : TStrings;
begin
  if FSpecialValueList = nil then
    FSpecialValueList := TStringList.Create;

  if FSpecialValueList.Capacity < MinIndex + 1 then
    FSpecialValueList.Capacity := MinIndex + 1;

  while FSpecialValueList.Count < MinIndex + 1 do
    FSpecialValueList.Add('');

  Result := FSpecialValueList;
end;

procedure TAbstractRow.ReadSpecialValue(Index : Integer; out Value : String);
begin
  Value := SpecialValueList(Index)[Index];
end;

procedure TAbstractRow.StoreSpecialValue(Index : Integer; Value : String);
begin
  SpecialValueList(Index)[Index] := Value;
end;

function TAbstractRow.SetFieldValue(Field : TDataField; Value : TValue; Action : TSetAction) : TSetResult;
{$ifdef COMMONLASKU}
begin
  if Self = nil then
    Result := DoSetFieldValue(Field, Value, Action)
  else
    Result := SetFValueVirt(Field, Value, Action);
end;

function TAbstractRow.DoSetFieldValue(Field : TDataField; Value : TValue; Action : TSetAction) : TSetResult;
{$endif COMMONLASKU}
var
  idx : Integer;
begin
  Assert(Field <> nil, 'TDataRow.SetFieldValue: Field <> nil');

  idx := IndexOfField(Field);
  if idx > -1 then
    Result := SetIndexValue(idx, Value, Action)
  else if (Self is TSubTotalRow) and
          Storage.RowIsUnaccepted(Self) and
          (Storage.TreeKeyByField[Field] <> nil) and
          (TSubTotalRow(Self).__UnacceptedRowExternSkipField <> Field) then
  begin
    TSubTotalRow(Self).FUnacceptedRowExternValues.Values[Storage.SkakaUnacceptedIndex(Field)] := Value;
    TSubTotalRow(Self).UpdateUnacceptedRowExternValues;
    Result := srOk;
  end
  else
    Result := Field.SetExternValue(Self, Value, Action);
end;

{$ifdef COMMONLASKU}
function TAbstractRow.SetFValueVirt(Field : TDataField; Value : TValue; Action : TSetAction) : TSetResult;
begin
  Result := DoSetFieldValue(Field, Value, Action);
end;
{$endif COMMONLASKU}

function TAbstractRow.SetFieldStringValue(Field : TDataField; Value : String; Action : TSetAction) : TSetResult;
begin
  Result := SetFieldValue(Field, ValueFromString(Value), Action);
end;

function TAbstractRow.SetFieldIntValue(Field : TDataField; Value : Integer; Action : TSetAction) : TSetResult;
begin
  Result := SetFieldValue(Field, ValueFromInteger(Value), Action);
end;

function TAbstractRow.SetFieldDoubleValue(Field : TDataField; Value : Double; Action : TSetAction) : TSetResult;
begin
  Result := SetFieldValue(Field, ValueFromDouble(Value), Action);
end;

function TAbstractRow.SetFieldDateTimeValue(Field : TDataField; Value : TDateTime; Action : TSetAction) : TSetResult;
begin
  Result := SetFieldValue(Field, ValueFromDateTime(Value), Action);
end;

function TAbstractRow.SetFieldCurrencyValue(Field : TDataField; Value : Currency; Action : TSetAction) : TSetResult;
begin
  Result := SetFieldValue(Field, ValueFromCurrency(Value), Action);
end;

function TAbstractRow.SetFieldBooleanValue(Field : TDataField; Value : Boolean; Action : TSetAction) : TSetResult;
begin
  Result := SetFieldValue(Field, ValueFromBoolean(Value), Action);
end;

function TAbstractRow.SetFieldPointerValue(Field : TDataField; Value : Pointer; Action : TSetAction) : TSetResult;
begin
  Result := SetFieldValue(Field, ValueFromPointer(Value), Action);
end;

// ------------------------ TDataRow ------------------------------

procedure TDataRow.WriteDataToStream(AStream : TStream);
begin
  AStream.Write(Self.FData^, DataTable.DataLength);
  // Fixa LGE denna borde beakta MemoDataType...
end;

(*
procedure TDataRow.SetValueFrom(DataField : TDataField; SrcRow : TAbstractRow);
begin
  Assert(DataField <> nil,
         'TDataRow.SetValueFrom: DataField <> nil');
  Assert(SrcRow <> nil,
         'TDataRow.SetValueFrom: SrcRow <> nil');

  Self.Value[DataField] := SrcRow.Value[DataField];
end;

procedure TDataRow.SetValueFromDifferentField(DataField : TDataField; SrcRow : TAbstractRow; SrcField : TDataField);
begin
  Assert(DataField <> nil, 'TDataRow.SetValueFrom: DataField <> nil');
  Assert(SrcRow <> nil, 'TDataRow.SetValueFrom: SrcRow <> nil');
  Assert(SrcField <> nil, 'TDataRow.SetValueFrom: SrcField <> nil');

  Self.Value[DataField] := SrcRow.Value[SrcField];
end;

procedure TDataRow.CopyIndexValue(idx : Integer; SourceRow : TAbstractRow);
begin
  Assert(SourceRow <> nil, 'TDataRow.CopyIndexValue: SourceRow <> nil');
  Assert((idx >= 0) and (idx < DataTable.FieldCount),
          'TDataRow.CopyIndexValue: (idx >= 0) and (idx < DataTable.FieldCount), idx: ' + IntToStr(idx));

  Self.ValueByIndex[idx] := SourceRow.FetchValue(idx);
end;

procedure TDataRow.AddIndexValue(idx : Integer; SourceRow : TAbstractRow);
begin
  Assert(SourceRow <> nil, 'TDataRow.AddIndexValue: SourceRow <> nil');
  Assert((idx >= 0) and (idx < DataTable.FieldCount),
         'TDataRow.AddIndexValue: (idx >= 0) and (idx < DataTable.FieldCount), idx: ' + IntToStr(idx));

  if not DataTable.Field[idx].IsAggregable then
  begin
    Log(ltWarning, 'AddValue', 'Cannot add a nonaggregable field');
    Exit;
  end;

  ValueByIndex[idx] := DataTable.Field[idx].DataType.Sum(Self.FetchValue(idx), SourceRow.FetchValue(idx));
end;
*)

function TDataField.SetExternValue(ARow : TAbstractRow; Value : TValue; Action : TSetAction) : TSetResult;
begin
  raise Exception.Create('Cannot set value ' + AsString(Value) + ' to Row ' +
                         ARow.DataTable.TableName + ', Field ' + Self.FieldName);
end;

function TDataRow.FieldHasValue(Field : TDataField) : Boolean;
begin
  // Fixa LGE;
  Result := True;
  try
    Self.GetFieldValue(Field);
  except
    Result := False;
  end;
end;

constructor TDataRow.CreateDefault(DataTable : TDataTable);
var
  i : Integer;
begin
  inherited Create(DataTable);

  for i := 0 to DataTable.FieldCount - 1 do
    if (DataTable.Field[i] <> nil) and (DataTable.Field[i].DataType <> nil) then
      Self.StoreValue(i, DataTable.Field[i].DefaultValue);

  FStatus := rsExternControlled;
  FUpdateDepth := 0;
end;

constructor TDataRow.CreateFromByteData(DataTable : TDataTable; Data : Pointer; var Position : Integer);
begin
  inherited Create(DataTable);

  Move(Pointer(Integer(Data) + Position)^, FData^, DataTable.DataLength);
  // Fixa LGE denna borde beakta MemoDataType...
  Inc(Position, DataTable.DataLength);

  FStatus := rsExternControlled;
  FUpdateDepth := 0;
end;

constructor TDataRow.Create(DataTable : TDataTable);
begin
  Assert(DataTable <> nil, 'TDataRow.Create: DataTable <> nil');

  inherited Create(DataTable);

  RawCopyContents(DataTable.IDefaultRow);

  FStatus := rsExternControlled;
end;

procedure TDataRow.SetDefaultsFrom(DefaultRow : TAbstractRow);
var
  i : Integer;
begin
  Assert(DefaultRow <> nil, 'TDataRow.SetDefaultsFrom: DefaultRow <> nil');

  if not (Status in [rsExternControlled, rsUnaccepted]) then
    Log(ltWarning, 'SetDefaultsFrom', 'SetDefaultsFrom called with nonExternControlled DataRow!');

  for i := 0 to DataTable.FieldCount - 1 do
  if not (DataTable.Field[i] is TFictiveField) then
  begin
    if DefaultRow.IndexOfField(DataTable.Field[i]) >= 0 then
      Self.ValueByIndex[i] := DefaultRow.Value[DataTable.Field[i]];
  end;
end;

function TDataRow.ContainsChanges : Boolean;
begin
  Result := Status in [rsChanged, rsNew, rsDeleted];
end;

function TDataRow.GetStatusCount(Status : TRowStatus) : Integer;
begin
  if Status = FStatus then
    Result := 1
  else
    Result := 0;
end;

procedure TDataRow.AddRowsToList(AList : TStrings; RowStatus : TRowStatus);
begin
  if RowStatus = FStatus then
    AList.AddObject('', Self);
end;

procedure TDataRow.BeginUpdate;
begin
  Inc(FUpdateDepth);
end;

procedure TDataRow.EndUpdate;
begin
  Dec(FUpdateDepth);
end;

function TDataRow.IsUpdating : Boolean;
begin
  Result := FUpdateDepth > 0;
end;

procedure TDataRow.ReplaceWithDetails(RowList : TStrings; var Index : Integer);
begin
  Inc(Index);
end;

function TDataRow.ContainsSubRows : Boolean;
begin
  Result := True;
end;

procedure TDataRow.CopyFromCompatible(ACompatibleRow : TDataRow);
var
  i : Integer;
begin
  Assert(ACompatibleRow <> nil, 'TDataRow.CopyFromCompatible: ACompatibleRow <> nil');

  for i := 0 to Self.DataTable.FieldCount - 1 do
    if not (DataTable.Field[i] is TFictiveField) then
      Self.ValueByIndex[i] := ACompatibleRow.Value[Self.DataTable.Field[i]];
end;







(*
procedure TDataRow.LoadJoinedGrouped(Query : TDataQuery; GroupedFieldPrefix : String; FieldNames : TStringList; DbDataTable : TDataTable);
var
  i : Integer;
  FieldName : String;
begin
  Assert(Query <> nil, 'TDataRow.Load: Query <> nil');

  if Status <> rsExternControlled then
    Log(ltError, 'Load', 'TDataRow.Load called with nonExternControlled DataRow!');

  for i := 0 to DataTable.FieldCount - 1 do
    if not (DataTable.Field[i] is TFictiveField) then
    begin
      if (i >= DataTable.KeyCount) and DataTable.Field[i].IsAggregable then
        FieldName := GroupedFieldPrefix + DataTable.Field[i].FieldName
      else
        FieldName := DbDataTable.FullFieldName(DataTable.Field[i], FieldNames);

      if DataTable.Field[i].CanBeInDB then
        Self.StoreValue(i, DataTable.Field[i].DataType.ValueOfQuery(Query, FieldName));
    end;
  FStatus := rsExternControlled;
end;
*)

destructor TDataRow.Destroy;
begin
  inherited Destroy;
end;

procedure TDataRow.NegateValue(Field : TDataField);
begin
  NegateByIndex(IndexOfField(Field));
end;

procedure TDataRow.NegateByIndex(idx : Integer);
begin
  if DataTable.Field[idx].DataType.IsNumeric then
    ValueByIndex[idx] := DataTable.Field[idx].DataType.NegateValue(ValueByIndex[idx]);
end;

procedure TDataRow.KeysChanged;
begin
  FStorage.Changed(kcKeyUpdate, Self, -1);
end;

function TDataRow.ContainsNewRunningNumbers : Boolean;
begin
  if DataTable.RunningNumberField <> nil then
    Result := DataTable.RunningNumberField.RunningNumberGenerator.IsTempRunningNumber(Self) // Self.IntValue[DataTable.RunningNumberField] <= 0
  else
    Result := False;
end;

function TSubTotalRow.SetIndexValue(idx : Integer; Value : TValue; Action : TSetAction) : TSetResult;
begin
  if Storage <> nil then
  begin
    if Storage.RowIsUnaccepted(Self) then
    begin
      if Storage.TreeKeyByField[DataTable.Field[idx]] <> nil then
      begin
        StoreValue(idx, Value);
        UpdateUnacceptedRowExternValues;
        Result := srOk;
      end
      else
      begin
        Result := srReadOnly;
      end;
    end
    else
    begin
      Result := Storage.InternalSubTotalHandler.InternalDistributeValue(Storage, Self, DataTable.Field[idx], Value, Action);
      // SubTotalsNotUptodate; LGE: denna borde ej behövas
    end;
  end
  else
  begin
    Log(ltError, 'Subtotals', 'Subtotal isn''t in a storage!');
    Result := srReadOnly; // just to avoid compilerjårs
  end;
end;

function TSubTotalRow.IndexOfSubRow(ARow : TAbstractRow) : Integer;
begin
  try
    Result := FSubRows.IndexOfValue(SubTotalKey.GetValue(ARow) {ARow[SubTotalKey.TreeKey]});
    if (Result >= 0) and (FSubRows.Objects[Result] <> ARow) then
      Result := FSubRows.IndexOfObject(ARow);
  except
    Result := FSubRows.IndexOfObject(ARow);
  end;
end;

procedure TSubTotalRow.UpdateUnacceptedRowExternValues;
var
  i : Integer;
  Value : TValue;
begin
  for i := 0 to Self.SubTotalKey.TreeKeyIndex - 2 do
    if not DataTable.TableHasField(Storage.TreeKey[i].TreeKey) then
    begin
      __UnacceptedRowExternSkipField := Storage.TreeKey[i].TreeKey;
      try
        Value := Self[Storage.TreeKey[i].TreeKey];
      except
      end;
      __UnacceptedRowExternSkipField := nil;
      FUnacceptedRowExternValues.Values[Storage.SkakaUnacceptedIndex(Storage.TreeKey[i].TreeKey)] := Value;
    end;
end;

function TDataRow.ConditionAcceptsUpdatedValue(Condition : TCondition; fieldidx : Integer; Value : TValue) : Boolean;
var
  Row : TDataRow;
begin
  Row := TDataRow.Create(DataTable);
  Row.CopyContents(Self);
  Row.ValueByIndex[fieldidx] := Value;
  Result := Condition.AcceptsRow(Row);
  Row.Free;
end;

function TDataRow.SetIndexValue(idx : Integer; Value : TValue; Action : TSetAction) : TSetResult;
  function SetNewValue(ARow : TDataRow; V : TValue) : Boolean;
  begin
    if DataTable.RunningNumberField = DataTable.Field[idx] then
    begin
      Result := False;
    end
    else
    begin
      Result := True;
      ARow.StoreValue(idx, V);
    end;
  end;

var
  PutAction : TPutAction;
  PutResult : TPutResult;
  CopyOfRow : TDataRow;
  SelfStatus : TRowStatus;
  CurrentSubTotal : TSubTotalRow;
  CurrValue : TValue;
  selfidx : Integer;
begin
  Assert((idx >= 0) and (idx < DataTable.FieldCount),
         'TDataRow.SetIndexValue: (idx >= 0) and (idx < DataTable.FieldCount), idx: ' + IntToStr(idx));

  if DataTable.Field[idx] is TFictiveField then
    Log(ltWarning, 'FictiveField', 'Assignment to fictive fields are not allowed');

  try
    CurrValue := ValueByIndex[idx];
    if DataTable.Field[idx].DataType.EqualsMatchCase(CurrValue, Value) then
    begin
      Result := srOk;
      Exit;
    end
    else if (Storage is TRowStorage) then
    begin
      if TRowStorage(Storage).KeysEditable then
      begin
        if (not TRowstorage(Storage).RowIsUnaccepted(Self)) and
           (not ConditionAcceptsUpdatedValue(TRowStorage(Storage).KeyCriteria, idx, Value)) then
        begin
          Result := srInvalidValue;
          Exit;
        end;
      end
      else if idx < DataTable.KeyCount then
      begin
        // Fixa LGE: Här sku vi kunna tillåta byte till befintlig nyckelkombination...
        Result := srInvalidValue;
        Exit;
      end;
    end;

    if (not (FStatus in [rsExternControlled, rsUnaccepted])) and (Storage.TreeKeyByField[DataTable.Field[idx]] <> nil) {(idx < DataTable.KeyCount)} {IsKey} then
    begin
      if Action = saDontOverwriteKeys then
        PutAction := paDontOverwriteKeys
      else if Action = saOverwriteOnKeyChange then
        PutAction := paOverwriteOnKeyChange
      else
      begin
        Log(ltError, 'KeyUpdate', 'Unknown TSetAction ' + IntToStr(Ord(Action)) + ')!');
        PutAction := paOverwriteOnKeyChange;
      end;

      // Create a new row with old keys and mark as deleted
      // Change this row's keys and move it in structure

      BeginUpdate;

      CurrentSubTotal := Self.FSubTotalRow;
      SelfStatus := Self.Status;
      CopyOfRow := Storage.CreateNewRow(Self.GetFieldValue);
      CopyOfRow.CopyContents(Self);

      selfidx := CurrentSubTotal.IndexOfSubRow(Self);
      if selfidx >= 0 then
        CurrentSubTotal.FSubRows.Delete(selfidx);

      if SetNewValue(Self, Value) then
      begin
        Self.FStatus := rsExternControlled;

        PutResult := Storage.PutRow(Self, PutAction);

        case PutResult of
          prOk:
          begin
            Result := srOk;
            Storage.ForcePutRow(CopyOfRow, paInternal); // Simulate deleting of the moved row!
            CopyOfRow.FStatus := SelfStatus;
            CopyOfRow.DeleteSelf(True);
            EndUpdate;
            Self.KeysChanged;
          end;

          prValuesMissing, prIllegalKeyValue:
          begin
            Result := srInvalidValue;
            SetNewValue(Self, CurrValue); // Move unsuccessful: Undo
            Self.FStatus := rsExternControlled;
            Storage.ForcePutRow(Self, paInternal);
            Self.FStatus := SelfStatus;
            CopyOfRow.Free;

            EndUpdate;
          end;

          prKeyConflict:
          begin
            Result := srKeyConflict;
            SetNewValue(Self, CurrValue); // Move unsuccessful: Undo
            Self.FStatus := rsExternControlled;
            Storage.ForcePutRow(Self, paInternal);
            Self.FStatus := SelfStatus;
            CopyOfRow.Free;

            EndUpdate;
          end;

          prKeyOverwrited:
          begin
            Result := srKeyOverwrited;
            Storage.ForcePutRow(CopyOfRow, paInternal); // Simulate deleting of the moved row!
            CopyOfRow.FStatus := SelfStatus;
            CopyOfRow.DeleteSelf(True);

            EndUpdate;
            Self.KeysChanged;
          end;
        else
          begin
            EndUpdate;

            Log(ltError, 'KeyUpdate', 'Unknown TPutResult ' + IntToStr(Ord(PutResult)) + ')!');
            Result := srReadOnly; // Just avoid compilerjårs
          end;
        end;
      end
      else
      begin
        Result := srInvalidValue;
      end;

      if FStorage <> nil then
        FStorage.FLastChanged := Now;
    end
    else
    begin
      if DataTable.Field[idx].IsAggregable then      // Fixa LGE: optimera; vad händer om det pangar?
        SubTotalRow.SubTotalsNotUptodate; //(nil, Self);

      SetNewValue(Self, Value);

      if FStatus = rsUnchanged then
        FStatus := rsChanged;

      if DataTable.Field[idx].IsAggregable then
        SubTotalRow.SubTotalsNotUptodate; //(Self, nil);

      Result := srOk;
      if FStorage <> nil then
        FStorage.FLastChanged := Now;
    end;
  except
    on TypeException do
      raise;
    else
      Result := srInvalidValue;
  end;
end;

procedure TDataRow.AddValues(SourceRow : TDataRow; AddAction : TAddAction);
var
  i : Integer;
begin
  Assert(SourceRow <> nil, 'TDataRow.AddValues: SourceRow <> nil');

  if AddAction = aaExceptOnDifferentValues then
    for i := DataTable.KeyCount to DataTable.FieldCount - 1 do
      if (not DataTable.Field[i].IsAggregable) and (not IndexEqual(i, SourceRow)) then
      begin
        Log(ltError, 'AddValues', 'AddValues: Different nonaggregable values in rows');
        Exit;
      end;

  for i := DataTable.KeyCount to DataTable.FieldCount - 1 do
  begin
    if AddAction = aaReplaceAllValues then
      Self.ValueByIndex[i] := SourceRow.ValueByIndex[i]
    else if DataTable.Field[i].IsAggregable then
      Self.ValueByIndex[i] := DataTable.Field[i].DataType.Sum(Self.ValueByIndex[i], SourceRow.ValueByIndex[i])
    else if AddAction = aaReplaceNonAggregable then
      Self.ValueByIndex[i] := SourceRow.ValueByIndex[i]
    else if (AddAction = aaReplaceBlank) and
            DataTable.Field[i].DataType.ValueBlank(FetchValue(i)) then
      Self.ValueByIndex[i] := SourceRow.ValueByIndex[i]
  end;
end;

procedure TDataRow.ClearValues;
var
  i : Integer;
begin
  for i := DataTable.KeyCount to DataTable.FieldCount - 1 do
    Self.ValueByIndex[i] := DataTable.Field[i].DefaultValue;
end;








procedure TDataRow.UpdateRunningNumber(Field : TRunningNumberField; NewValue : TValue);
begin
  if Field <> DataTable.RunningNumberField then
    Log(ltError, 'Running number', 'TDataRow.UpdateRunningNumber: ' + Field.FieldName + ' isn''t a running number in ' +
                                   DataTable.TableName);

  if Storage <> nil then
    Storage.ForceRemoveRow(Self);

  Self.StoreValue(DataTable.IndexOfField(Field), NewValue);

  if Storage <> nil then
  begin
    Storage.ForcePutRow(Self, paInternal);
    Self.FStatus := rsNew;
  end;
end;

procedure TDataRow.CopyContents(SrcRow : TDataRow);
begin
  Assert(SrcRow <> nil, 'TDataRow.CopyContents: SrcRow <> nil');

  if Self.FStatus <> rsExternControlled then
  begin
    Log(ltError, 'CopyRow', 'Row must be rsExternControlled in order to perform CopyContents!');
    Exit;
  end;

  if SrcRow.DataTable <> Self.DataTable then
  begin
    Log(ltError, 'CopyRow', 'Tried to copy rows of different type!');
    Exit;
  end;

  RawCopyContents(SrcRow);
end;

function TDataRow.CreateCopy : TDataRow;
begin
  Result := TDataRow.Create(DataTable);
  Result.CopyContents(Self);
end;

function TDataRow.KeySQLString : String;
var
  i : Integer;
begin
  Result := '';
  for i := 0 to DataTable.KeyCount - 1 do
  if not (DataTable.Field[i] is TFictiveField) then
  begin
    if i > 0 then
      Result := Result + ' and ';

    Result := Result + DataTable.Field[i].FieldName + '=' + Self.SQLByIndex[i];
  end;
end;

function TDataRow.KeysEqual(ARow : TDataRow) : Boolean;
var
  i : Integer;
begin
  if ARow.DataTable <> Self.DataTable then
  begin
    Log(ltWarning, 'KeysEqual', 'TDataRow.KeysEqual: DataTables differ: ' +
          ARow.DataTable.TableName + ' <> ' + Self.DataTable.TableName);
    Result := False;
  end
  else
  begin
    Result := True;

    for i := 0 to DataTable.KeyCount - 1 do
      if not Self.IndexEqual(i, ARow) then
      begin
        Result := False;
        Exit;
      end;
  end;
end;

procedure TDataRow.FillRowList(ARowList : TStrings; ExtraCondition : TCondition; DefaultSort : Boolean);
begin
  if Self.Visible then
  begin
    if (ExtraCondition = nil) or
       ExtraCondition.AcceptsRow(Self) then
      ARowList.AddObject('', Self);
  end;
end;

procedure TDataRow.Delete;
begin
  DeleteSelf(True);
end;

procedure TDataRow.DeleteSelf(AllowTreeDelete : Boolean);
var
  idx : Integer;
begin
  if Storage <> nil then
    Storage.Changed(kcDeletedRow, Self, -1);

  if not (Status in [rsExternControlled, rsUnaccepted]) then
  begin
    if AllowTreeDelete then
      FSubTotalRow.DeleteRow(Self, AllowTreeDelete)
    else
    begin
      FSubTotalRow.SubTotalsNotUptodate;
      idx := FSubTotalRow.IndexOfSubRow(Self);
      if idx >= 0 then
        FSubTotalRow.FSubRows.Objects[idx] := nil;
    end;
  end;

  case Status of
    rsUnchanged, rsChanged:
    begin
      FStatus := rsDeleted;
      Storage.FDeletedRows.Add(Self);
    end;

    rsNew:
    begin
      FStatus := rsDeleted;
      Free;
    end;

    rsExternControlled, rsUnaccepted:
    begin
{      if (Storage <> nil) and
         (Storage is TRowStorage) then
        TRowStorage(Storage).FUnacceptedRows.Remove(Self); }

      FStatus := rsDeleted;
      Free;
    end;

  else
    FStatus := rsDeleted;
  end;
end;

function TDataRow.GetValidKeyCount : Integer;
begin
  if Storage <> nil then
    Result := Storage.TreeKeyCount
  else
    Result := DataTable.KeyCount;
end;

function TDataRow.BuildingBlocksVisible : Boolean;
begin
  Result := Self.Visible;
end;

function TDataRow.CanEditValue(Field : TDataField; var ReadOnlyReason : String) : Boolean;
begin
  Result := True; // Fixa LGE alla editerbara tillsvidare
end;

// ------------------------------- TDummyRow -----------------------------------

{procedure TDummyRow.FillRowList(ARowList : TStrings; ExtraPatch : TQuiltPatch; DefaultSort : Boolean);
begin
  // Do not add self!
end;

function TDummyRow.ContainsSubRows : Boolean;
begin
  Result := False;
end;

function TDummyRow.NoChanges : TRowStatus;
begin
  Result := rsUnchanged;
end;

function TDummyRow.Save(UpdateQuery, InsertQuery, MaxNoQuery : TDataQuery; RunningNumberIndex : Integer; Fields : TDataFieldSet) : Integer;
begin
  Result := 0;
end;

procedure TDummyRow.SaveInterrupt(UpdateQuery, InsertQuery, MaxNoQuery : TDataQuery; RunningNumberIndex : Integer;
                                  Interruptable : TInterruptable; AffectedFields : TDataFieldSet);
begin

end;
 }
// ------------------------ TSubTotalRow ---------------------------------------

function TSubTotalRow.CanEditValue(Field : TDataField; var ReadOnlyReason : String) : Boolean;
begin
  Result := Storage.InternalSubTotalHandler.InternalCanEditValue(Storage, Self, Field, ReadOnlyReason);
end;

procedure TSubTotalRow.BeforeFetchValue(Field : TDataField);
begin
  if Field.IsAggregable and (not FSubTotalsUptodate) then
    UpdateSubTotals;
end;

function TSubTotalRow.FieldHasValue(Field : TDataField) : Boolean;
var
  i : Integer;
  Index : Integer;
  Value : TValue;
begin
  Assert(Field <> nil, 'TSubTotalRow.FieldHasValue: Field <> nil');

  Index := DataTable.IndexOfField(Field);

  if Index = -1 then
  begin
    if (Field is TClosedField) then
    begin
      Result := True;
      for i := 0 to TClosedField(Field).FieldCount - 1 do
      begin
        Result := Self.FieldHasValue(TClosedField(Field).Field[i]);
        if not Result then
           break;
      end;
    end
    else
      Result := (Field is TCalcField) and TCalcField(Field).LegalForSubTotals;
  end
  else if Index >= DataTable.KeyCount then
  begin
    Result := Field.IsAggregable;
  end
  else
  begin
    if (Storage is TRowStorage) and
       TRowStorage(Storage).KeyCriteria.AcceptsExactlyOneValue(Field, Value) then
    begin
      Result := True;
    end
    else
    begin
      for i := 0 to SubTotalKey.TreeKeyIndex - 1 do
        if FStorage.TreeKey[i].TreeKey = Field then
        begin
          Result := True;
          Exit;
        end;

      Result := False;
    end;
  end;
end;

function TSubTotalRow.IndexOfField(Field : TDataField) : Integer;
var
  i : Integer;
  Value : TValue;
begin
  Assert(Field <> nil, 'TSubTotalRow.IndexOfField: Field <> nil');

  if Self = nil then
  begin
    Result := -1;
    Exit;
  end;

  Result := DataTable.IndexOfField(Field);
  if (Result >= DataTable.KeyCount) or
     (Result = -1) then
    Exit;

  for i := 0 to SubTotalKey.TreeKeyIndex - 1 do
  begin
    if FStorage.TreeKey[i].TreeKey = Field then
      Exit;
  end;

  if (Storage is TRowStorage) and
     TRowStorage(Storage).KeyCriteria.AcceptsExactlyOneValue(Field, Value) then
       Exit;

  Result := -1;
end;

procedure TSubTotalRow.DeleteRow(ARow : TAbstractRow; RecursiveDelete : Boolean);
var
  idx : Integer;
begin
  if Self <> nil then
  begin
    idx := IndexOfSubRow(ARow);
    if idx >= 0 then
    begin
      Storage.Changed(kcDeletedRow, ARow, -1);

      if FSubRows.Objects[idx] is TSubTotalRow then
        TSubTotalRow(FSubRows.Objects[idx]).Free;

      FSubRows.Delete(idx);
    end;

    if FSubRows.Count = 0 then
    begin
      if RecursiveDelete then
        FSubTotalRow.DeleteRow(Self, RecursiveDelete);
    end
    else
      SubTotalsNotUptodate; // endast ifall detail
  end;
end;

procedure TSubTotalRow.UpdateSubTotals; // Fixa LGE flytta till STHandler
begin
  if FSubTotalsUptodate then
    Exit;

  Storage.InternalSubTotalHandler.InternalUpdateSubTotals(Storage, Self, Self.StoreValue);
  FSubTotalsUptodate := True;
end;

procedure TSubTotalRow.SubTotalsNotUptodate;
begin
  if Self <> nil then
  begin
    FSubTotalsUptodate := False;
    FSubTotalRow.SubTotalsNotUptodate;
  end;
end;

// ---- Tree building blocks

procedure TAbstractRow.RawCopyContents(ASrcRow : TAbstractRow);
begin
  CopyMemory(Self.FData, ASrcRow.FData, DataTable.DataLength);

  if ASrcRow.FSpecialValueList <> nil then
  begin
    if FSpecialValueList = nil then
      FSpecialValueList := TStringList.Create;
    FSpecialValueList.Assign(ASrcRow.FSpecialValueList);
  end
  else
  begin
    FSpecialValueList.Free;
    FSpecialValueList := nil;
  end;
end;

constructor TSubTotalRow.Create(Storage : TAbstractRowStorage; DirectOwner : TSubTotalRow; OwnKeyValue : TValue; Visible : Boolean);
var
  i : Integer;
  Value : TValue;
begin
  Assert(Storage <> nil, 'TSubTotalRow.Create: Storage <> nil');

  inherited Create(Storage.DataTable);

  RawCopyContents(DataTable.IDefaultRow);

  FStorage := Storage;
  FSubTotalRow := DirectOwner;
  FVisible := Visible;
  FUnacceptedRowExternValues := TValueList.Create(nil);
  for i := 0 to Storage.TreeKeyCount - 1 do
    if not DataTable.TableHasField(Storage.TreeKey[i].TreeKey) then
      FUnacceptedRowExternValues.AddVal(Storage.TreeKey[i].TreeKey.DataType.DefaultValue);

  if DirectOwner = nil then
  begin
    FSubTotalKey := Storage.TreeKey[0];
  end
  else
  begin
    FSubTotalKey := Storage.TreeKey[DirectOwner.SubTotalKey.TreeKeyIndex + 1];

    // Set key values (data values are defaulted to 0)
    for i := 0 to FSubTotalKey.TreeKeyIndex - 2 do // Obs -2! Index - 1 sätts separat nedanför. Index skall ej ha värde, eftersom det varierar
      if Storage.TreeKey[i].RowFieldIndex >= 0 then
        CopyMemory(@Self.FData[DataTable.ReadPos[Storage.TreeKey[i].RowFieldIndex]], // Fixa LGE använd hellre befintlig kod -> detta kan skita sig
                   @DirectOwner.FData[DataTable.ReadPos[Storage.TreeKey[i].RowFieldIndex]],
                   DataTable.Field[Storage.TreeKey[i].RowFieldIndex].DataType.DataSize);

    if DirectOwner.SubTotalKey.RowFieldIndex >= 0 then
      StoreValue(DirectOwner.SubTotalKey.RowFieldIndex, OwnKeyValue);
  end;

  if (Storage is TRowStorage) and
     (TRowStorage(Storage).KeyCriteria <> nil) then
    for i := 0 to DataTable.KeyCount - 1 do
      if TRowStorage(Storage).KeyCriteria.AcceptsExactlyOneValue(DataTable.Field[i], Value) then
        StoreValue(i, Value);


  FSubRows := TValueList.Create(FSubTotalKey.TreeKey.DataType);
  FSubRows.Sorted := True;

  FSubTotalsUptodate := True;
end;

procedure TSubTotalRow.UpdateRowVisibility(Level : Integer; Show : Boolean);
var
  i : Integer;
begin
  if Self.FSubTotalKey.TreeKeyIndex = Level then
    Self.Visible := Show
  else if Self.FSubTotalKey.TreeKeyIndex = Level - 1 then
  begin
    for i := 0 to FSubRows.Count - 1 do
      SubRows[i].Visible := Show;
  end
  else if (not Self.IsLastTreeNode) and
          (Self.FSubTotalKey.TreeKeyIndex < Level) then
  begin
    for i := 0 to FSubRows.Count - 1 do
      TSubTotalRow(FSubRows.Objects[i]).UpdateRowVisibility(Level, Show);
  end
end;

destructor TSubTotalRow.Destroy;
var
  i : Integer;
begin
  for i := 0 to FSubRows.Count - 1 do
    FSubRows.Objects[i].Free;
  FSubRows.Free;
  FUnacceptedRowExternValues.Free;

  inherited Destroy;
end;

procedure TSubTotalRow.ClearAll;
var
  i : Integer;
begin
  for i := 0 to FSubRows.Count - 1 do
    FSubRows.Objects[i].Free;
  FSubRows.Clear;

  SubTotalsNotUptodate;
end;

function TSubTotalRow.GetSubRowCount : Integer;
begin
  Result := FSubRows.Count;
end;

function TSubTotalRow.GetSubRow(idx : Integer) : TAbstractRow;
begin
  Result := TAbstractRow(FSubRows.Objects[idx]);

end;

function TSubTotalRow.IsLastTreeNode : Boolean;
begin
  Result := (SubTotalKey.TreeKeyIndex = Storage.TreeKeyCount - 1);
end;

function TSubTotalRow.ContainsChanges : Boolean;
var
  i : Integer;
begin
  Result := False;

  for i := 0 to FSubRows.Count - 1 do
    if SubRows[i].ContainsChanges then
    begin
      Result := True;
      Break;
    end;
end;

procedure TSubTotalRow.ReplaceWithDetails(RowList : TStrings; var Index : Integer);
var
  i : Integer;
  Row : TAbstractRow;
begin
  RowList.Delete(Index);
  for i := 0 to FSubRows.Count - 1 do
  begin
    Row := SubRows[i];
    RowList.InsertObject(Index, '', Row);
    Row.ReplaceWithDetails(RowList, Index);
  end;
end;

function TSubTotalRow.GetStatusCount(Status : TRowStatus) : Integer;
var
  i : Integer;
begin
  Result := 0;

  for i := 0 to FSubRows.Count - 1 do
    Result := Result + SubRows[i].GetStatusCount(Status);
end;

procedure TSubTotalRow.AddRowsToList(AList : TStrings; RowStatus : TRowStatus);
var
  i : Integer;
begin
  for i := 0 to FSubRows.Count - 1 do
    SubRows[i].AddRowsToList(AList, RowStatus);
end;

function TSubTotalRow.IsTreeKeyOnly(AField : TDataField; var KeyValue : TValue) : Boolean;
var
  ASubTotalKey : TRowStorageTreeKey;
begin
  if FSubTotalRow = nil then
  begin
    ASubTotalKey := Storage.TreeKeyByField[AField];
    if Storage.RowIsUnaccepted(Self) and
       (ASubTotalKey <> nil) and
       (__UnacceptedRowExternSkipField <> AField) then
    begin
      Result := True;
      if ASubTotalKey.TreeKeyIndex > Self.SubTotalKey.TreeKeyIndex then
        KeyValue := AField.DataType.DefaultValue // fixa lge logga fel
      else if DataTable.TableHasField(AField) then
        KeyValue := Self[AField] // suspekt!
      else
        KeyValue := FUnacceptedRowExternValues.Values[Storage.SkakaUnacceptedIndex(AField)]
    end
    else
      Result := False;
  end
  else if FSubTotalRow.SubTotalKey.TreeKey = AField then
  begin
    KeyValue := FSubTotalRow.FSubRows.Values[FSubTotalRow.FSubRows.IndexOfObject(Self)];
    Result := True;
  end
  else
    Result := FSubTotalRow.IsTreeKeyOnly(AField, KeyValue);
end;

function TSubTotalRow.SumValues(Field : TDataField {TCalcField}) : TValue; // Fixa LGE flytta till STHandler
var
  i : Integer;
begin
  if FSubRows.Count = 0 then
    Result := ValueFromDouble(0.0)
  else
  begin
    Result := SubRows[0].Value[Field];
    for i := 1 to FSubRows.Count - 1 do
      Result := Field.DataType.Sum(Result, SubRows[i].Value[Field]);
  end;
end;

function TSubTotalRow.IsUpdating : Boolean;
begin
  Result := False;
end;

function TSubTotalRow.GetFirstRow : TDataRow;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to FSubRows.Count - 1 do
  begin
    if FSubRows.Objects[i] is TDataRow then
      Result := TDataRow(FSubRows.Objects[i])
    else
      Result := TSubTotalRow(FSubRows.Objects[i]).GetFirstRow;

    if Result <> nil then
      Exit;
  end;
end;



procedure TSubTotalRow.FillRowList(ARowList : TStrings; ExtraCondition : TCondition; DefaultSort : Boolean);
var
  i : Integer;
  TmpList : TStringList;
  Order : TRowSortOrder;
begin
  if (ExtraCondition <> nil) and
     (not ExtraCondition.AcceptsRow(Self)) then
    Exit;

  if Self.Visible and
     (not Storage.UsesCustomSortOrder) and
     (not Storage.ISubTotalsUnder) then
    ARowList.AddObject('', Self);

  TmpList := TStringList.Create;
  for i := 0 to FSubRows.Count - 1 do
    TmpList.AddObject('', FSubRows.Objects[i]);

  if DefaultSort and (SubTotalKey.SortField <> SubTotalKey.TreeKey) then
  begin
    Order := TRowSortOrder.Create;
    Order.AddRule(SubTotalKey.SortField, SubTotalKey.SortOrder);
    if SubTotalKey.SortField <> SubTotalKey.TreeKey then
      Order.AddRule(SubTotalKey.TreeKey, soAscending);

    Order.OrderRows(DataTable, TmpList);
    Order.Free;
  end
  else if DefaultSort and (SubTotalKey.SortOrder = soDescending) then
  begin
    if TmpList.Count > 1 then
      for i := 0 to (TmpList.Count - 1) div 2 do
        TmpList.Exchange(i, TmpList.Count - 1 - i);
  end;

  for i := 0 to TmpList.Count - 1 do
    TAbstractRow(TmpList.Objects[i]).FillRowList(ARowList, ExtraCondition, DefaultSort);
  TmpList.Free;

  if Self.Visible and
     (not Storage.UsesCustomSortOrder) and
     Storage.ISubTotalsUnder then
    ARowList.AddObject('', Self);
end;

// This should be used only when all concerned keys are loaded
function TSubTotalRow.ContainsSubRows : Boolean;
var
  i : Integer;
begin
  Result := False;

  for i := 0 to FSubRows.Count - 1 do
  begin
    Result := SubRows[i].ContainsSubRows;
    if Result then
      Exit;
  end;
end;

function TSubTotalRow.KeysExist(Keys : array of String; Index : Integer) : Boolean;
var
  ThisValue : TValue;
  idx : Integer;
begin
  if Index > High(Keys) then
  begin
    Result := ContainsSubRows;
  end
  else
  begin
    ThisValue := ValueFromString(Keys[Index]);

    if not FindValue(ThisValue, idx) then
      Result := False // Row didn't exist
    else if Self.IsLastTreeNode then
      Result := True // (FSubRows.Count > 0)
    else
      Result := TSubTotalRow(FSubRows.Objects[idx]).KeysExist(Keys, Index + 1);
  end;
end;

{Obs: LocateRow fungerar enligt träduppbyggnaden, ej enligt nycklarna i DataTable}
function TSubTotalRow.LocateRow(Keys : array of String; Index : Integer) : TDataRow;
var
  ThisValue : TValue;
  idx : Integer;
begin
  ThisValue := ValueFromString(Keys[Index]);

  if not FindValue(ThisValue, idx) then
    Result := nil // Row didn't exist
  else if Self.IsLastTreeNode then
    Result := TDataRow(FSubRows.Objects[idx])
  else
    Result := TSubTotalRow(FSubRows.Objects[idx]).LocateRow(Keys, Index + 1);
end;

function TSubTotalRow.LocateByRowValues(ValueRow : TAbstractRow; DerivedKeys : array of TDataField) : TDataRow;

  procedure LogWarning;
  begin
    Log(ltWarning, 'LocateByRowValues', 'TSubTotalRow.LocateByRowValues: ' +
                          'Found multiple matching values for ' + ValueRow.DataTable.TableName +
                          'Row ' + ValueRow.KeyFieldValues + ' when looking for match in table ' +
                          Self.DataTable.TableName + '.');
  end;
var
  ThisValue : TValue;
  ThisField : TDataField;
  HasValueForThisField : Boolean;
  ThisFieldSrc : TDataField;
  i, idx : Integer;
  FoundRow : TDataRow;
begin
  ThisField := SubTotalKey.TreeKey;
  ThisFieldSrc := ThisField;
    for idx := Low(DerivedKeys) to High(DerivedKeys) do
      if (DerivedKeys[idx] <> nil) and
         (DerivedKeys[idx].AuxTableField = ThisField) then
      begin
        ThisFieldSrc := DerivedKeys[idx];
        Break;
      end;

  try
    ThisValue := ValueRow[ThisFieldSrc];
    HasValueForThisField := True;
  except
    HasValueForThisField := False;
  end;

  if not HasValueForThisField then
  begin
    if Self.IsLastTreeNode then
    begin
      {if (FSubRows.Count = 0) or
         ((FSubRows.Count = 1) and (FSubRows.Objects[0] is TDummyRow)) then
        Result := nil
      else} if FSubRows.Count = 1 then
        Result := TDataRow(FSubRows.Objects[0])
      else
      begin
        Result := nil;
        LogWarning;
      end;
    end
    else
    begin
      Result := nil;
      for i := 0 to FSubRows.Count - 1 do
      begin
        FoundRow := TSubTotalRow(FSubRows.Objects[i]).LocateByRowValues(ValueRow, DerivedKeys);
        if (Result <> nil) and (FoundRow <> nil) then
        begin
          Result := nil;
          LogWarning;
          Exit;
        end;

        if FoundRow <> nil then
        Result := FoundRow;
      end;
    end;
  end
  else
  begin
    if not FindValue(ThisValue, idx) then
      Result := nil // Row didn't exist
    else if Self.IsLastTreeNode then
      Result := TDataRow(FSubRows.Objects[idx])
    else
      Result := TSubTotalRow(FSubRows.Objects[idx]).LocateByRowValues(ValueRow, DerivedKeys);
  end;
end;

procedure TSubTotalRow.SetChildVisibility(Vis, Recursive : Boolean);
var
  i : Integer;
  LastLevel : Boolean;
begin
  Lastlevel := Self.IsLastTreeNode;

  for i := 0 to FSubRows.Count - 1 do
  begin
    SubRows[i].Visible := Vis;
    if Recursive and (not LastLevel) then
      TSubTotalRow(FSubRows.Objects[i]).SetChildVisibility(Vis, Recursive);
  end;
end;

function TSubTotalRow.BuildingBlocksVisible : Boolean;
var
  i : Integer;
begin
  Result := True;
  for i := 0 to FSubRows.Count - 1 do
  begin
    if (not SubRows[i].Visible) and
       (not SubRows[i].BuildingBlocksVisible) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function TSubTotalRow.__CheckSortingNeeded(DefaultSort : Boolean) : Boolean;
begin
  Result := DefaultSort and
            ((SubTotalKey.SortField <> SubTotalKey.TreeKey) or
             (SubTotalKey.SortOrder <> soAscending));
end;

procedure TSubTotalRow.__AddToSortList(Matches : TStrings; ARow : TAbstractRow);
begin
  Matches.AddObject('', ARow);
end;

procedure TSubTotalRow.__DirectAdd(Handler : TGetRowsHandler; ARow : TAbstractRow); // (ARow : TAbstractRow; Results : TStrings; Condition : TCondition; Action : TGetAction; ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean; DefaultSort : Boolean);
var
  DataRow, CopyOfRow : TDataRow;
begin
  if ARow is TSubTotalRow then
    TSubTotalRow(ARow).GetRows(Handler) // ( Results, Condition, Action, ExcludeOnSubTotalLevel, LastLevelCheckAllFields, DefaultSort)
  else
  begin
    DataRow := TDataRow(ARow);
    case Handler.GetAction of
      gaReference:
      begin
{        if DataRow = nil then
          ShowMessage('nilrow')
        else }
          Handler.ResultList.AddObject('', DataRow);
      end;
      gaCopy:
      begin
        CopyOfRow := Storage.CreateNewRow(DataRow.GetFieldValue);
        CopyOfRow.CopyContents(DataRow);
        Handler.ResultList.AddObject('', CopyOfRow);
      end;
      gaCut:
      begin
        CopyOfRow := Storage.CreateNewRow(DataRow.GetFieldValue);
        CopyOfRow.CopyContents(DataRow);
        Handler.ResultList.AddObject('', CopyOfRow);
        DataRow.DeleteSelf(False);
      end;
      gaDelete:
      begin
        DataRow.DeleteSelf(False);
      end;
    end;
  end;
end;

procedure TSubTotalRow.__AddRowCheckDetails(Handler : TGetRowsHandler; Matches : TStrings; ARow : TAbstractRow; SortingNeeded : Boolean);
begin
  if (not (ARow is TSubTotalRow)) and // detailrows
     (not Handler.Condition.AcceptsRow(ARow)) then // skip illegal values; this method is only called if checkonchildlevel=True
    Exit;

  __AddRow(Handler, Matches, ARow, SortingNeeded);
end;

procedure TSubTotalRow.__AddRow(Handler : TGetRowsHandler; Matches : TStrings; ARow : TAbstractRow; SortingNeeded : Boolean);
begin
  if SortingNeeded then
    __AddToSortList(Matches, ARow)
  else
    __DirectAdd(Handler, ARow); // (ARow, Results, Condition, Action, ExcludeOnSubTotalLevel, LastLevelCheckAllFields, DefaultSort);
end;

procedure TSubTotalRow.__AddValue(Handler : TGetRowsHandler; AValue : TValue); // (AValue : TValue; Results : TStrings; Condition : TCondition; Action : TGetAction; ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean; DefaultSort : Boolean);
var
  Index : Integer;
begin
  if FindValue(AValue, Index) then
    __DirectAdd(Handler, SubRows[Index]); // (SubRows[Index], Results, Condition, Action, ExcludeOnSubTotalLevel, LastLevelCheckAllFields, DefaultSort);
end;

procedure TSubTotalRow.__SortAndAdd(Handler : TGetRowsHandler; Matches : TStrings); // (Matches : TStrings; Results : TStrings; Condition : TCondition; Action : TGetAction; ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean; DefaultSort : Boolean);
var
  Order : TRowSortOrder;
  j : Integer;
begin
  Order := TRowSortOrder.Create;
  Order.AddRule(SubTotalKey.SortField, SubTotalKey.SortOrder);
  if SubTotalKey.SortField <> SubTotalKey.TreeKey then
    Order.AddRule(SubTotalKey.TreeKey, soAscending);
  Order.OrderRows(DataTable, Matches);
  Order.Free;

  for j := 0 to Matches.Count - 1 do
    __DirectAdd(Handler, TAbstractRow(Matches.Objects[j])); // (TAbstractRow(Matches.Objects[j]), Results, Condition, Action, ExcludeOnSubTotalLevel, LastLevelCheckAllFields, DefaultSort);

  Matches.Free;
end;

procedure TSubTotalRow.__AddIfLegal(Handler : TGetRowsHandler; Matches : TStrings; ARow : TAbstractRow; SortingNeeded : Boolean); // (Matches : TStrings; SortingNeeded : Boolean; ARow : TAbstractRow; ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean; Results : TStrings; Condition : TCondition; Action : TGetAction; DefaultSort : Boolean);
begin
  if not Handler.Condition.AcceptsValue(SubTotalKey.TreeKey, SubTotalKey.GetValue(ARow)) then
    Exit; // Not legal

  if Handler.LastLevelNeedCheckAllFields and Self.IsLastTreeNode then
  begin
    if Handler.Condition.AcceptsRow(ARow) then
      __AddRow(Handler, Matches, ARow, SortingNeeded);
  end
  else
    __AddRow(Handler, Matches, ARow, SortingNeeded);
end;

procedure TSubTotalRow.__RemoveEmptyChilds;
var
  i : Integer;
begin
  if Self.IsLastTreeNode then
  begin
    for i := FSubRows.Count - 1 downto 0 do
      if FSubRows.Objects[i] = nil then
        FSubRows.Delete(i);
  end
  else
  begin
    for i := FSubRows.Count - 1 downto 0 do
      if TSubTotalRow(FSubRows.Objects[i]).FSubRows.Count = 0 then
        Self.DeleteRow(SubRows[i], False);
  end;
end;

procedure TSubTotalRow.GetRows(Handler : TGetRowsHandler); // (Results : TStrings; Condition : TCondition; Action : TGetAction; ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean; DefaultSort : Boolean);
begin
  Handler.Condition.InternalGetRows(Handler, Self); // (Self, Results, Action, DefaultSort, ExcludeOnSubTotalLevel, LastLevelCheckAllFields);

  if Handler.GetAction in [gaCut, gaDelete] then
    __RemoveEmptyChilds;
end;

{ TGetRowsHandler }

{$ifdef D4_OR_HIGHER}
constructor TGetRowsHandler.Create(Storage : TAbstractRowStorage; Condition : TCondition; ResultList : TStrings = nil; GetAction : TGetAction = gaDelete);
{$else}
constructor TGetRowsHandler.Create(Storage : TAbstractRowStorage; Condition : TCondition; ResultList : TStrings; GetAction : TGetAction);
{$endif D4_OR_HIGHER}
begin
  inherited Create;
  FDataTable := Storage.DataTable;
  FOwningCondition := (Condition = nil);
  if FOwningCondition then
    FCondition := TQuiltPatch.Create
  else
    FCondition := Condition;

  FResultList := ResultList;
  FGetAction := GetAction;

  if GetAction in [gaDelete] then
    FDefaultSort := False
  else
    FDefaultSort := Storage.CanDefaultSort;

  FAllowExcludeOnSubTotalLevel := FCondition.IsPureAndCondition;
  FLastLevelNeedCheckAllFields := Self.DoLastLevelNeedCheckAllFields;
end;

destructor TGetRowsHandler.Destroy;
begin
  inherited Destroy;

  if FOwningCondition then
    FCondition.Free;
end;

function TGetRowsHandler.DoLastLevelNeedCheckAllFields : Boolean;
var
  i : Integer;
begin
  if not Condition.IsPureAndCondition then
    Result := True
  else
  begin
    Result := False;

    // Check for conditions on nonkey fields
    for i := DataTable.KeyCount to DataTable.FieldCount - 1 do
    begin
      if not Condition.AcceptsAllForField(DataTable.Field[i]) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

{ TCondition }

class function TCondition.GetNextStreamItem(AStream : TStream; var ConditionClass : TConditionClass) : Boolean;
var
  AClassName : String;
begin
  Result := GetStreamTextTo(AStream, '(', ')', AClassName);
  if Result then
    ConditionClass := TConditionClass(FindClass(Trim(AClassName)));
end;

class function TCondition.GetStreamTextTo(AStream : TStream; Separator, ExitFalseAt : Char; var ReadText : String) : Boolean;
var
  OldPos, NewPos : Integer;
  IsQuoted : Boolean;
  AChar : Char;
begin
  Result := True;
  OldPos := AStream.Position;
  IsQuoted := False;
  repeat
    if AStream.Read(AChar, 1) = 0 then
    begin
      Result := False;
      Break;
    end;

    if (not IsQuoted) and (Separator = AChar) then
    begin
      Result := True;
      Break;
    end;

    if (not IsQuoted) and (ExitFalseAt = AChar) then
    begin
      Result := False;
      Break;
    end;

    if AChar = '''' then
      IsQuoted := not IsQuoted;
  until (False);

  NewPos := AStream.Position;

  AStream.Position := OldPos;

  if Result then
  begin
    if NewPos - OldPos - 1 > 0 then
    begin
      SetLength(ReadText, NewPos - OldPos - 1);
      AStream.Read(ReadText[1], NewPos - OldPos - 1);
    end
    else
      ReadText := '';

    AStream.Read(AChar, 1);
  end;
end;

class function TCondition.SkipToObjectEnd(AStream : TStream) : Boolean;
var
  Dummy : String;
begin
  Result := GetStreamTextTo(AStream, ')', #0, Dummy);
end;

procedure TCondition.WriteName(AStream : TStream);
var
  Data : String;
begin
  Data := ClassName + '(';
  AStream.Write(Data[1], Length(Data));
end;

procedure TCondition.WriteNewLine(AStream : TStream; Indent : Integer);
var
  Data : String;
begin
  Data := #13#10 + StringOfChar(' ', Indent);
  AStream.Write(Data[1], Length(Data));
end;

procedure TCondition.WriteCloseParenteses(AStream : TStream);
var
  Data : String;
begin
  Data := ')';
  AStream.Write(Data[1], 1);
end;

procedure TCondition.WriteToStream(AStream : TStream);
begin
  DoWriteToStream(AStream, 0);
end;

class function TCondition.CreateFromStream(AStream : TStream) : TCondition;
var
  ConditionClass : TConditionClass;
begin
  if GetNextStreamItem(AStream, ConditionClass) then
    Result := ConditionClass.DoCreateFromStream(AStream)
  else
    Result := nil;
end;
(*
function TCondition.CopyValues( FromField, ToField : TDataField; CreateUnion, KeepOld : Boolean ) : TCondition;
begin
  Result := DoCopyValues( FromField, ToField, CreateUnion, KeepOld );
  if Result <> nil then
    Result := Result.DoCopyValues( nil, nil, CreateUnion, KeepOld );
  if Result = nil then
    Result := TQuiltPatch.Create;
end;
*)
function TCondition.InternalHasRowsThatMatch(Handler : TGetRowsHandler; SubTotal : TSubTotalRow) : Boolean; // (SubTotal : TSubTotalRow; ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean) : Boolean;
var
  KeyField : TDataField;
  i : Integer;
begin
  KeyField := SubTotal.SubTotalKey.TreeKey;

  Result := False;
  for i := 0 to SubTotal.FSubRows.Count - 1 do
  begin
    if SubTotal.IsLastTreeNode then
    begin
      if Handler.LastLevelNeedCheckAllFields then
        Result := Self.AcceptsRow(SubTotal.SubRows[i])
      else
        Result := AcceptsValue(KeyField, SubTotal.FSubRows.Values[i]);
    end
    else if Handler.AllowExcludeOnSubTotalLevel then
      Result := Self.AcceptsValue(KeyField, SubTotal.FSubRows.Values[i] {, SubTotal.SubRows[i]}) and
                TSubTotalRow(SubTotal.FSubRows.Objects[i]).HasRowsThatMatch(Handler)
    else
      Result := TSubTotalRow(SubTotal.FSubRows.Objects[i]).HasRowsThatMatch(Handler);

    if Result then
      Break;
  end;
end;

function TCondition.GetOnlyValue(AField : TDataField) : TValue;
begin
  if not AcceptsExactlyOneValue( AField, Result ) then
    raise Exception.Create( AField.FieldName + ' doesn''t have one legal value!' );
end;



function TCondition.IsPureAndCondition : Boolean;
begin
  Result := False;
end;

function TSubTotalRow.HasRowsThatMatch(Handler : TGetRowsHandler) : Boolean; // (Condition : TCondition; ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean) : Boolean;
begin
  Result := Handler.Condition.InternalHasRowsThatMatch(Handler, Self); // ( Self, ExcludeOnSubTotalLevel, LastLevelCheckAllFields);
end;
(*
procedure TSubTotalRow.GetRowsByKeyBegin(Results : TStrings; Criteria : TCondition; Action : TGetAction; FieldIndex : Integer; BeginWith : String; ExcludeOnSubTotalLevel, LastLevelCheckAllFields, DefaultSort : Boolean);
var
  KeyField : TDataField;
  i : Integer;
//  LoopAllFields : Boolean;
  Matches : TStringList;
  SortingNeeded : Boolean;

  procedure DirectAdd(ARow : TAbstractRow);
  var
    DataRow, CopyOfRow : TDataRow;
  begin
    if ARow is TSubTotalRow then
      TSubTotalRow(ARow).GetRowsByKeyBegin(Results, Criteria, Action, FieldIndex, BeginWith, ExcludeOnSubTotalLevel, LastLevelCheckAllFields, DefaultSort)
    else
    begin
      DataRow := TDataRow(ARow);
      case Action of
        gaReference: Results.AddObject('', DataRow);
        gaCopy:
        begin
          CopyOfRow := Storage.CreateNewRow(DataRow.GetFieldValue);
          CopyOfRow.CopyContents(DataRow);
          Results.AddObject('', CopyOfRow);
        end;
        gaCut:
        begin
          CopyOfRow := Storage.CreateNewRow(DataRow.GetFieldValue);
          CopyOfRow.CopyContents(DataRow);
          Results.AddObject('', CopyOfRow);
          DataRow.DeleteSelf(False);
        end;
        gaDelete:
        begin
          DataRow.DeleteSelf(False);
        end;
      end;
    end;
  end;

  procedure AddValue(AValue : TValue);
  var
    Index : Integer;
  begin
    if FindValue(AValue, Index) then
      DirectAdd(SubRows[Index]);
  end;

  procedure AddRow(ARow : TAbstractRow);
  begin
    if SortingNeeded then
      __AddToSortList(Matches, ARow)
    else
      DirectAdd(ARow);
  end;

  procedure AddIfLegal(ARow : TAbstractRow);
  var
    j : Integer;
    ThisField : TDataField;
    ThisValue : TValue;
    CanAdd : Boolean;
  begin
    if LastLevelCheckAllFields and Self.IsLastTreeNode {LoopAllFields} then
    begin
      if (SubTotalKey.RowFieldIndex = FieldIndex) then
      begin
        if (Copy(AsString(ARow.ValueByIndex[SubTotalKey.RowFieldIndex]), 1, Length(BeginWith)) <> BeginWith) then
          Exit;
      end
      else if (Criteria <> nil) and (not TCriteria(Criteria).ValueLegal(KeyField, SubTotalKey.GetValue(ARow))) then
        Exit;

      CanAdd := True;

      for j := DataTable.KeyCount to DataTable.FieldCount - 1 do
      begin
        ThisField := DataTable.Field[j];
        ThisValue := ARow.ValueByIndex[j];
        if (j = FieldIndex) then
        begin
          if (Copy(AsString(ThisValue), 1, Length(BeginWith)) <> BeginWith) then
          begin
            CanAdd := False;
            Break;
          end;
        end
        else if not TCriteria(Criteria).ValueLegal(ThisField, ThisValue) then
        begin
          CanAdd := False;
          Break;
        end;
      end;

      if CanAdd then
        AddRow(ARow);
    end
    else
    begin
      if (SubTotalKey.RowFieldIndex = FieldIndex) then
      begin
        if (Copy(AsString(ARow.ValueByIndex[SubTotalKey.RowFieldIndex]), 1, Length(BeginWith)) = BeginWith) then
          AddRow(ARow);
      end
      else if (Criteria = nil) or TCriteria(Criteria).ValueLegal(KeyField, SubTotalKey.GetValue(ARow)) then
        AddRow(ARow);
    end;
  end;

  procedure SortAndAdd;
  var
    Order : TRowSortOrder;
    j : Integer;
  begin
    Order := TRowSortOrder.Create;
    Order.AddRule(SubTotalKey.SortField, SubTotalKey.SortOrder);
    if SubTotalKey.SortField <> SubTotalKey.TreeKey then
      Order.AddRule(SubTotalKey.TreeKey, soAscending);
    Order.OrderRows(DataTable, Matches);
    Order.Free;

    for j := 0 to Matches.Count - 1 do
      DirectAdd(TAbstractRow(Matches.Objects[j]));

    Matches.Free;
  end;

var
  ACritField : TCriteriaField;
  SkipSorting : Boolean;
begin
  KeyField := SubTotalKey.TreeKey;

  if (not (LastLevelCheckAllFields and Self.IsLastTreeNode)) and
     (FieldIndex <> SubTotalKey.RowFieldIndex) and
     (Criteria <> nil) then
  begin
    ACritField := TCriteriaLink(Criteria).FindCriteriaField(KeyField, False);
    SkipSorting := (ACritField <> nil) and ACritField.HasExactlyOneValue;
  end
  else
  begin
    SkipSorting := False;
    ACritField := nil;
  end;

  if SkipSorting then
  begin
    AddValue(ACritField.OnlyValue);
  end
  else
  begin
    SortingNeeded := __CheckSortingNeeded(DefaultSort);
    if SortingNeeded then
      Matches := TStringList.Create
    else
      Matches := nil;

    for i := 0 to FSubRows.Count - 1 do
      AddIfLegal(SubRows[i]);

    if SortingNeeded then
      SortAndAdd;
  end;

  if Action in [gaCut, gaDelete] then
    __RemoveEmptyChilds;
end;
*)
function TSubTotalRow.AddSubTotalRow(ARow : TSubtotalRow) : TPutResult;
var
  ThisField : TDataField;
  ThisValue : TValue;
  idx : Integer;
begin
  ThisField := SubTotalKey.TreeKey;

  try
    if DataTable.RunningNumberField = ThisField then
      ARow.StoreValue(SubTotalKey.RowFieldIndex, DataTable.RunningNumberField.RunningNumberGenerator.GetNextTempRunningNumber(DataTable, FSubRows));

    ThisValue := SubTotalKey.GetValue(ARow);

    if (Storage is TRowStorage) and
       (not TRowStorage(Storage).KeyCriteria.AcceptsRow(ARow)) then
    begin
      Result := prIllegalKeyValue;
    end
    else
    begin
      if not FindValue(ThisValue, idx) then
        idx := -1;

      if not Self.SubTotalKey.TreeKeyIndex >= ARow.SubTotalKey.TreeKeyIndex then
        Result := prIllegalKeyValue
      else if Self.SubTotalKey.TreeKeyIndex < ARow.SubTotalKey.TreeKeyIndex - 1 then
      begin
        if idx = -1 then
          idx := AddChildRow(ThisValue, FStorage.CreateSubTotal(Self, ThisValue, FStorage.TreeKey[SubTotalKey.TreeKeyIndex+1].Visible));
        Result := TSubTotalRow(FSubRows.Objects[idx]).AddSubTotalRow(ARow);
      end
      else if idx >= 0 then
      begin
        Result := prIllegalKeyValue;
      end
      else
      begin
        AddChildRow(ThisValue, ARow);
        ARow.FStorage := Storage;
        ARow.FSubTotalRow := Self;
        Result := prOk;
      end;
    end;
  finally
    // ??
  end;
end;

function TSubTotalRow.PutRowConflict(Row : TDataRow; idx : Integer; Action : TPutAction; Force : Boolean) : TPutResult;
var
  Differs : Boolean;
  iField : Integer;
  AField : TDataField;
  ThisValue : TValue;
  AddAction : TAddAction;
begin
  ThisValue := SubTotalKey.GetValue(Row);

  case Action of
    paDontOverwriteKeys, paInternal: Result := prKeyConflict;
    paOverwriteOnKeyChange, paOverwriteOnDifferingField:
    begin
      if Action = paOverwriteOnKeyChange then
        Differs := True
      else
      begin
        Differs := False;
        for iField := Row.Datatable.KeyCount to Row.Datatable.FieldCount-1 do
        begin
          AField := Row.Datatable.Field[iField];
          if not AField.DataType.Equals( Row[AField], TDataRow(FSubRows.Objects[idx])[AField] ) then
          begin
            Differs := True;
            Break;
          end;
        end;
      end;

      if Differs then
      begin
        Result := prKeyOverwrited;

        if FSubRows.Objects[idx] is TDataRow then
          TDataRow(FSubRows.Objects[idx]).DeleteSelf(False);
        AddChildRow(ThisValue, Row);
        Row.FStorage := Storage;
        Row.FSubTotalRow := Self;
        SubTotalsNotUptodate;
      end
      else
        Result := prCannotAdd;
    end;
    paAddReplaceNone, paAddReplaceBlank, paAddReplaceNonAggregable, paAddExcept:
    begin
      try
        case Action of
          paAddReplaceNone:          AddAction := aaReplaceNone;
          paAddReplaceBlank:         AddAction := aaReplaceBlank;
          paAddReplaceNonAggregable: AddAction := aaReplaceNonAggregable;
          paAddExcept:               AddAction := aaExceptOnDifferentValues;
        else
          // Foo
          AddAction := aaExceptOnDifferentValues;
        end;

        Row.AddValues(TDataRow(FSubRows.Objects[idx]), AddAction);
        PutRowConflict(Row, idx, paOverwriteOnKeyChange, Force);
        Result := prRowsAdded;
      except
        Result := prCannotAdd;
      end;
    end;
  else
    begin
      Log(ltError, 'PutRow', 'Unknown PutAction (' + IntToStr(Ord(Action)) + ')!');
      Result := prOk; // Avoid compilerjårs;
    end;
  end;
end;

function TSubTotalRow.ForcePutRow(Row : TDataRow; PutAction : TPutAction) : TPutResult;
var
  ThisValue : TValue;
  idx : Integer;
begin
  ThisValue := SubTotalKey.GetValue(Row);

  if not FindValue(ThisValue, idx) then
    idx := -1;

  if not Self.IsLastTreeNode then
  begin
    if idx = -1 then
      idx := AddChildRow(ThisValue, FStorage.CreateSubTotal(Self, ThisValue, FStorage.TreeKey[SubTotalKey.TreeKeyIndex+1].Visible));

    Result := TSubTotalRow(FSubRows.Objects[idx]).ForcePutRow(Row, PutAction);
  end
  else if idx = -1 then
  begin
    AddChildRow(ThisValue, Row);
    SubTotalsNotUptodate;
    Row.FStorage := Storage;
    Row.FSubTotalRow := Self;
    Result := prOk;
  end
  else
  begin
    // Result := prKeyConflict;
    Result := PutRowConflict(Row, idx, PutAction, True);
  end;
end;


function TSubTotalRow.InternalPutRow(Row : TDataRow; Action : TPutAction) : TPutResult;
var
  ThisField : TDataField;
  ThisValue : TValue;
  idx : Integer;
  OldRunningNumber : TValue;
begin
  Result := prOk;
  ThisField := SubTotalKey.TreeKey;
  OldRunningNumber := Row[ThisField];

  try
    if DataTable.RunningNumberField = ThisField then
      Row.StoreValue(SubTotalKey.RowFieldIndex,
                     DataTable.RunningNumberField.RunningNumberGenerator.GetNextTempRunningNumber(DataTable, FSubRows));

    ThisValue := SubTotalKey.GetValue(Row);

    if not FindValue(ThisValue, idx) then
      idx := -1;

    if not Self.IsLastTreeNode then
    begin
      if idx = -1 then
        idx := AddChildRow(ThisValue, FStorage.CreateSubTotal(Self, ThisValue, FStorage.TreeKey[SubTotalKey.TreeKeyIndex+1].Visible));
      Result := TSubTotalRow(FSubRows.Objects[idx]).InternalPutRow(Row, Action);
    end
    else if idx = -1 then
    begin
      AddChildRow(ThisValue, Row);
      Row.FStorage := Storage;
      Row.FSubTotalRow := Self;
      SubTotalsNotUptodate;
      Result := prOk;
    end
    else
    begin
      Result := PutRowConflict(Row, idx, Action, False);
    end;
  finally
    if (Result in [prValuesMissing, prKeyConflict, prIllegalKeyValue, prCannotAdd]) and
       (DataTable.RunningNumberField = ThisField) then
      Row.StoreValue(SubTotalKey.RowFieldIndex, OldRunningNumber);
  end;
end;

function TSubTotalRow.FindValue(Value : TValue; var idx : Integer) : Boolean;
begin
  Result := FSubRows.Find(Value, idx);
end;

function TSubTotalRow.ForceRemoveRow(ARow : TDataRow) : Boolean;
var
  ThisValue : TValue;
  idx : Integer;
begin
  ThisValue := SubTotalKey.GetValue(ARow);

  if not FSubRows.Find(ThisValue, idx) then
    Result := False
  else if not Self.IsLastTreeNode then
  begin
    Result := TSubTotalRow(FSubRows.Objects[idx]).ForceRemoveRow(ARow);
    if Result and (TSubTotalRow(FSubRows.Objects[idx]).FSubRows.Count = 0) then
    begin
      TSubTotalRow(FSubRows.Objects[idx]).Free;
      FSubRows.Delete(idx);
    end;
  end
  else if idx = -1 then
    Result := False
  else
  begin
    FSubRows.Delete(idx);
    SubTotalsNotUptodate;
    Result := True;
  end;
end;

function TSubTotalRow.AddChildRow(KeyValue : TValue; Row : TAbstractRow) : Integer;
begin


  Result := FSubRows.IndexOfValue(KeyValue);
  if (Result >= 0) and (FSubRows.Objects[Result] = nil) then
    FSubRows.Objects[Result] := Row
  else
    Result := FSubRows.AddValue('', KeyValue, Row);
end;

function TAbstractRow.GetValidKeyValue(idx : Integer) : TValue;
begin
  if Storage = nil then
    Result := ValueByIndex[idx]
  else
    Result := Storage.TreeKey[idx].GetValue(Self);
end;

function TAbstractRow.GetValidKey(idx : Integer) : TDataField;
begin
  if Storage = nil then
    Result := DataTable.Field[idx]
  else
    Result := Storage.TreeKey[idx].TreeKey;
end;

function TSubTotalRow.GetValidKeyCount : Integer;
begin
  Result := SubTotalKey.TreeKeyIndex;
end;

// --------------------------------- TCondition --------------------------------

constructor TCondition.Create(Owner : TCondition);
begin
  inherited Create;
  FOwner := Owner;
  FOnChange := nil;
  FBeginUpdateCount := 0;
  FChanged := False;
end;

destructor TCondition.Destroy;
begin
  inherited Destroy;
end;

procedure TCondition.SetOwner(AOwner: TCondition);
begin
  FOwner := AOwner;
end;

procedure TCondition.Changed;
begin
  if FBeginUpdateCount > 0 then
    FChanged := True
  else if Assigned(OnChange) then
   OnChange( Self );
end;

procedure TCondition.BeginUpdate;
begin
  Inc(FBeginUpdateCount);
end;

procedure TCondition.EndUpdate;
begin
  Dec(FBeginUpdateCount);
  if FBeginUpdateCount <= 0 then
  begin
    FBeginUpdateCount := 0;
    Changed;
    FChanged := False;
  end;
end;

procedure TCondition.InternalGetRows(Handler : TGetRowsHandler; SubTotal : TSubTotalRow); // (SubTotal : TSubTotalRow; Results : TStrings; Action : TGetAction; DefaultSort, ExcludeOnSubTotalLevel, LastLevelCheckAllFields : Boolean);
var
  Matches : TStringList;
  SortingNeeded : Boolean;
  i : Integer;
begin
  SortingNeeded := SubTotal.__CheckSortingNeeded(Handler.DefaultSort);
  if SortingNeeded then
    Matches := TStringList.Create
  else
    Matches := nil;

  if Handler.AllowExcludeOnSubTotalLevel then
  begin
    for i := 0 to SubTotal.FSubRows.Count - 1 do
      SubTotal.__AddIfLegal(Handler, Matches, SubTotal.SubRows[i], SortingNeeded); // (Matches, SortingNeeded, SubTotal.SubRows[i], ExcludeOnSubTotalLevel, LastLevelCheckAllFields, Results, Self, Action, DefaultSort);
  end
  else
  begin
    for i := 0 to SubTotal.FSubRows.Count - 1 do
      SubTotal.__AddRowCheckDetails(Handler, Matches, SubTotal.SubRows[i], SortingNeeded); // (Matches, SortingNeeded, SubTotal.SubRows[i], Results, Self, Action, ExcludeOnSubTotalLevel, LastLevelCheckAllFields, DefaultSort);
  end;

  if SortingNeeded then
    SubTotal.__SortAndAdd(Handler, Matches); // ( Matches, Results, Self, Action, ExcludeOnSubTotalLevel, LastLevelCheckAllFields, DefaultSort);
end;



// ----------------------------- TLexer ---------------------------------------



function TCondition.CreateCommonQuilt: TCommonQuilt;
var
  FreeQuilt : Boolean;
begin
  Result := GetCommonQuilt(FreeQuilt);
  if not FreeQuilt then
    Result := Result.CreateCommonCopy;
end;







{ TCommonQuilt }

procedure E;
begin
  raise Exception.Create( 'Not implemented!' );
end;

function TCommonQuilt.ConcernsField(DataField: TDataField): Boolean;
begin
  Result := AffectedFieldSet.ContainsField(DataField);
end;

function TCommonQuilt.ConcernsTable(DataTable: TDataTable): Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 0 to DataTable.FieldCount -1 do
  begin
    if AffectedFieldSet.ContainsField(DataTable.Field[i]) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

constructor TCommonQuilt.Create;
begin
  inherited Create( nil );
end;

class function TCommonQuilt.CreateDifference(Condition1, Condition2 : TCondition): TCommonQuilt;
var
  tmpQuilt : TCommonQuilt;
  FreeParam : Boolean;
begin
  tmpQuilt := Condition2.GetCommonQuilt(FreeParam);
  Result := Condition1.CreateCommonQuilt.DoCreateDifference(tmpQuilt, FreeParam);
end;

class function TCommonQuilt.CreateIntersection(Conditions : array of TCondition): TCommonQuilt;
var
  iCond : Integer;
  FreeParam : Boolean;
  ACommon : TCommonQuilt;
begin
  Result := Conditions[Low(Conditions)].CreateCommonQuilt;
  for iCond := Low(Conditions) + 1 to High(Conditions) do
  begin
    ACommon := Conditions[iCond].GetCommonQuilt(FreeParam);
    Result := Result.DoCreateIntersection(ACommon, FreeParam);
  end;
end;

class function TCommonQuilt.CreateUnion(Conditions : array of TCondition): TCommonQuilt;
var
  iCond : Integer;
  FreeParam : Boolean;
  ACommon : TCommonQuilt;
begin
  Result := Conditions[Low(Conditions)].CreateCommonQuilt;
  for iCond := Low(Conditions) + 1 to High(Conditions) do
  begin
    ACommon := Conditions[iCond].GetCommonQuilt(FreeParam);
    Result := Result.DoCreateUnion(ACommon, FreeParam);
  end;
end;

destructor TCommonQuilt.Destroy;
begin
  inherited;
end;

constructor TCommonQuilt.DoCreateFromStream(AStream: TStream);
begin
  E;
end;

procedure TCommonQuilt.DoWriteToStream(AStream: TStream; Indent: Integer);
begin
  E;
end;

function TCommonQuilt.GetLargerCriteria(
  var FreeCriteria: Boolean): TCondition {TCriteria};
begin
  Result := nil;
  E;
end;

function TCommonQuilt.GetSmallerCriteria(
  var FreeCriteria: Boolean): TCondition {TCriteria};
begin
  Result := nil;
  E;
end;

{ TLangFieldArray }

constructor TLangFieldArray.Create;
begin
  inherited Create;
end;

constructor TLangFieldArray.CreateWithFields(Fields : array of TDataField);
begin
  Create;
  SetAllFields(Fields);
end;

procedure TLangFieldArray.SetField(Index : Integer; Field : TDataField);
begin
  if Index >= Count then
    AddDefaultValues(Index);

  FLanguages.Objects[Index] := Field;
end;

function TLangFieldArray.GetField(Index : Integer) : TDataField;
begin
  if Index < Count then
    Result := TDataField(FLanguages.Objects[Index])
  else if Count > 0 then
    Result := TDataField(FLanguages.Objects[0])
  else
    Result := TDataField(Self.DefaultObjValue);
end;

function TLangFieldArray.GetCurrentField : TDataField;
begin
  Result := Fields[CurrentIndex];
end;

procedure TLangFieldArray.SetAllFields(Fields : array of TDataField);
var
  i : Integer;
begin
  FLanguages.Clear;
  for i := Low(Fields) to High(Fields) do
    AddField(Fields[i]);
end;

procedure TLangFieldArray.AddField(Field : TDataField);
begin
  FLanguages.AddObject('', Field);
end;

function TLangFieldArray.GetDefaultObjValue : TObject;
var
  AField : TDataField;
begin
  AField := nil;
  if Assigned(OnGetDefaultField) then
    OnGetDefaultField(Self, AField);

  Result := AField;
end;

initialization



  LanguageIndex := 0;
  FDefaultSubTotalHandler := nil;
  FMinimumLoadPolicy := TDefaultCacheLoadPolicy.Create(nil);
  FMinimumLoadPolicy.NoCriteriaKeyCount := 0;
  FDefaultRunningNumberGenerator := TDefaultRunningNumberGenerator.Create;

  CreateUList(UTableTypeList, False);
  CreateUList(UTableList);
  CreateUList(UFieldList);

  FieldCallingObject := TKeyField.CreateNonAuxtabled('', ObjectType);
  FieldPropInfo := TKeyField.CreateNonAuxtabled('', StringType(255, False));
  FieldMissingProperty := TKeyField.CreateNonAuxtabled('', StringType(255, False));
  FieldIndex := TKeyField.CreateNonAuxtabled('', IntegerType);
  FieldValue := TDataField.CreateOld('', StringType(255, False));

  FixupTable := TDataTable.CreateOld('', nil,
                         [FieldMissingProperty, FieldPropInfo, FieldCallingObject, FieldIndex],
                         [FieldValue],
                         nil);
  FixupStorage := TRowStorage.Create(FixupTable); //, nil, nil, nil, False);



finalization

  FreeUList(UTableTypeList);
  FreeUList(UTableList);
  FreeUList(UFieldList);

  FDefaultSubTotalHandler.Free;
  FMinimumLoadPolicy.Free;
  FDefaultRunningNumberGenerator.Free;

  FieldCallingObject.Free;
  FieldMissingProperty.Free;
  FieldPropInfo.Free;
  FieldIndex.Free;
  FieldValue.Free;

  FixupTable.Free;
  FixupStorage.Free;


end.



