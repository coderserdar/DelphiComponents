{***************************************************************************}
{ TMS Data Modeler Library for Delphi & C++Builder                          }
{ version 1.7                                                               }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 1997 - 2010                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such.                             }
{***************************************************************************}

unit uGDAO;

{$I tmsdefs.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF DELPHI6_LVL}
  Variants,
  {$ENDIF}
  Contnrs, dgConsts;

const
  SProp_SequenceSeed = 'SequenceSeed';

type
  ERecursiveRelException = class(Exception);

  TGDD                        = class;
  TGDAOTable                  = class;
  TGDAOTables                 = class;
  TGDAOFields                 = class;
  TGDAOField                  = class;
  TGDAOIndexes                = class;
  TGDAOIndex                  = class;
  TGDAOIFields                = class;
  TGDAOIField                 = class;
  TGDAORelationship           = class;
  TGDAORelationships          = class;
  TGDAOConstraints            = class;
  TGDAOConstraint             = class;
  TGDAOTriggers               = class;
  TGDAOTrigger                = class;
  TGDAORelationshipFieldLinks = class;
  TGDAORelationshipFieldLink  = class;
  TGDAODataTypes              = class;
  TGDAODataType               = class;
  TGDAODomains                = class;
  TGDAODomain                 = class;
  TGDAOCategories             = class;
  TGDAOCategory               = class;
  TGDAOObjects                = class;
  TGDAOObject                 = class;

  {TGDD holds information about the database model defined in the DM project.
  It contains the tables, fields, procedures, indexes, and all other information about the database model}
  TGDD = class(TPersistent)
  private
    FOwner             : TPersistent;
    FTables            : TGDAOTables;
    FRelationships     : TGDAORelationships;
    FDomains           : TGDAODomains;
    FCategories        : TGDAOCategories;
    FNextTableID       : integer;
    FNextFieldID       : integer;
    FNextIndexID       : integer;
    FNextConstraintID  : integer;
    FNextRelationshipID: integer;
    FDatabaseTypeID: string;
    FDataTypes: TGDAODataTypes;
    procedure SetRelationships(const Value: TGDAORelationships);
    procedure SetCategories(const Value: TGDAOCategories);
    procedure SetTables(const Value: TGDAOTables);
    procedure SetDataTypes(const Value: TGDAODataTypes);
    procedure SetDomains(const Value: TGDAODomains);
    function GetNextIndexID: integer;
    function GetNextConstraintID: integer;
    function GetNextRelationshipID: integer;
    function GetNextTableID: integer;
    function GetNextFieldID: integer;
    function GetDatabaseTypeID: string;
    procedure SetDatabaseTypeID(const Value: string);
    function GetDataTypes: TGDAODataTypes;
  protected
    procedure FieldDestroyed(AField: TGDAOField);
  public
    constructor Create(AOwner: TPersistent = nil);
    destructor Destroy; override;
    {Internal use}
    procedure Loaded;
    {Internal use}
    function GetOwner: TPersistent; override;
    {Assign information from another TGDD object}
    procedure Assign(Source: TPersistent); override;
    {Returns the index, in the Tables collection, of the table specified by ATableName}
    function IndexOfTable(ATableName: string): integer;
    {Returns the TGDAOTable object associated with the table specified by ATableName}
    function TableByName(ATableName: string): TGDAOTable;
    {Returns the TGDAORelationship object associated with the relationship specified by ARelationshipName}
    function RelationshipByName(ARelationshipName: string): TGDAORelationship;
    {Returns true an index with the name AIndexName exists}
    function IndexExists(AIndexName: string): boolean;
  published
    {DatabaseTypeID identified the target database. It must be the VERY FIRST property
     of the list, because several other properties (datatypes, categories) depend on
     this property to work}
    property DatabaseTypeID: string read GetDatabaseTypeID write SetDatabaseTypeID;
    {internal use}
    property NextTableID: integer read FNextTableID write FNextTableID;
    {internal use}
    property NextFieldID: integer read FNextFieldID write FNextFieldID;
    {internal use}
    property NextConstraintID: integer read FNextConstraintID write FNextConstraintID;
    {internal use}
    property NextIndexID: integer read FNextIndexID write FNextIndexID;
    {internal use}
    property NextRelationshipID: integer read FNextRelationshipID write FNextRelationshipID;

    {Contains a collection of TGDAODataType objects that holds information about all data types
     supported by the database type}
    property DataTypes: TGDAODataTypes read GetDataTypes write SetDataTypes stored false;
    {Contains a collection of TGDAODomain objects that holds information about the domains defined
     in the data dictionary}
    property Domains: TGDAODomains read FDomains write SetDomains;
    {Contains a collection of TGDAOTable objects that holds information about the tables defined
     in the data dictionary}
    property Tables: TGDAOTables read FTables write SetTables;
    {Contains a collection of TGDAORelationship objects that holds information about the relationships defined
     in the data dictionary}              
    property Relationships: TGDAORelationships read FRelationships write SetRelationships;
    {Contains a collection of TGDAOCategory object that holds information about the object categories supported
     by the database. Objects like stored procedures, views, sequences, are defined in this collection}
    property Categories: TGDAOCategories read FCategories write SetCategories;
  end;

  {TGDAODatabase holds information about the database model defined in the DM project.
  It contains the tables, fields, procedures, indexes, and all other information about the database model}
  TGDAODatabase = class(TGDD);

  {TGDAOTables is a collection of TGDAOTable that holds information about all tables defined
   in the data dictionary}
  TGDAOTables = class(TCollection)
  private
    FOwnerDatabase: TGDD;
    function GetItem(i:integer):TGDAOTable;
    procedure SetItem(i: integer; const Value: TGDAOTable);
  public
    constructor Create( AOwnerDatabase:TGDD );
    function GetOwner: TPersistent; override;
    {Adds a new table}
    function Add: TGDAOTable; overload;
    {Adds a new table}
    function Add(ATableName: string): TGDAOTable; overload;
    {Returns the index of the table specified by ATableName in the Tables collection}
    function IndexOf(ATableName: string): integer;
    {Returns the index, in the Tables collection, of the table specified by the internal identifier ATID} 
    function IndexOfTid(ATID: integer): integer;
    {Retrieves the TGDAOTable object associated with the table specified by ATableName}
    function FindByName(ATableName: string): TGDAOTable;
    {Retrieves the TGDAOTable object assocaited with the table specified by the internal identifier ATID}
    function FindByID(ATID: integer): TGDAOTable;
    {Returns a reference to the data dictionary object TGDD that holds this table collection}
    property OwnerDatabase:TGDD read FOwnerDatabase;
    {Provides indexed access to the tables in the TGDAOTables collection}
    property Items[i:integer]:TGDAOTable read GetItem write SetItem; default;
  end;

  {TGDAOTable object holds all the information (fields, triggers, indexes, etc.) about a table in the data dictionary}
  TGDAOTable = class(TCollectionItem)
  private
    FTableName: string;
    FDescription: string;
    FTID: integer;
    FOidIndex: integer;
    FExclusiveRecordMask: string;
    FFields: TGDAOFields;
    FIndexes: TGDAOIndexes;
    FConstraints: TGDAOConstraints;
    FTriggers: TGDAOTriggers;
    FData: TObject;
    FPrimaryKeyIndex: TGDAOIndex;
    FTableCaption: string;
    FRestriction: TTableRestriction;
    procedure SetFields(const Value: TGDAOFields);
    procedure SetIndexes(const Value: TGDAOIndexes);
    procedure SetConstraints(const Value: TGDAOConstraints);
    procedure SetTriggers(const Value: TGDAOTriggers);
    procedure FieldDestroyed(AField: TGDAOField);
    function GetPrimaryKeyIndex: TGDAOIndex;
    procedure SetPrimaryKeyIndex(const Value: TGDAOIndex);
    function GetTableCaption: string;
    procedure SetTableCaption(const Value: string);
    function StoreTableCaption: Boolean;
    function GetReadOnly: boolean;
    procedure SetReadOnly(const Value: boolean);
    function GetVisible: boolean;
    procedure SetVisible(const Value: boolean);
    function GetRestriction: TTableRestriction;
  public
    constructor Create( ACollection:TCollection ); override;
    procedure BeforeDestruction; override;
    destructor Destroy; override;
    function  GetDisplayName: string; override;
    procedure Assign(Source: TPersistent); override;
    {Returns a TGDAOField object associated with the field specified by AFieldName}
    function FieldByName(AFieldName:string):TGDAOField;
    {Adds a new index of type AIndexType, and name AIndexName}
    function AddIndex(AIndexName: string; AIndexType: TIndexType): TGDAOIndex;
    {internal use only}
    procedure UpdateID;
    {Returns a reference to the Data Dictionary object which this table belongs to}
    function OwnerDatabase:TGDD;
    {Returns true if the table contain any field that is a foreign key}
    function HasForeignFields: boolean;
    {Returns true if the table contains a primary key}
    function HasPrimaryKey: boolean;
    {Returns a TGDAOTrigger object associated with the trigger specified by ATriggerName}
    function TriggerByName(ATriggerName: string): TGDAOTrigger;
    {Placeholder for a generic TObject}
    property Data: TObject read FData write FData;
    {Returns true if Restriction = trReadOnly}
    property ReadOnly: boolean read GetReadOnly write SetReadOnly;
    {Returns true if Restriction <> trHidden}
    property Visible: boolean read GetVisible write SetVisible;
  published
    {Contains the name of the table in the database}
    property TableName: string read FTableName write FTableName;
    {Contains the description/comments for the table}
    property Description: string read FDescription write FDescription;
    {TID is the internal identifier of the table}
    property TID: integer read FTID write FTID;
    {not used}
    property OidIndex: integer read FOidIndex write FOidIndex;
    {not used}
    property ExclusiveRecordMask: string read FExclusiveRecordMask write FExclusiveRecordMask;
    {Fields collection contains the list of the table fields}
    property Fields: TGDAOFields read FFields write SetFields;
    {Indexes property contains the list of the table indexes}
    property Indexes:TGDAOIndexes read FIndexes write SetIndexes;
    {Constraints property holds the list of the table constraints}
    property Constraints:TGDAOConstraints read FConstraints write SetConstraints;
    {Triggers property holds the list of the table triggers}
    property Triggers: TGDAOTriggers read FTriggers write SetTriggers;
    {PrimaryKeyIndex is a TGDAOIndex object that holds information about the primary key of the table}
    property PrimaryKeyIndex: TGDAOIndex read GetPrimaryKeyIndex write SetPrimaryKeyIndex;
    {Contains the caption of the table}
    property TableCaption: string read GetTableCaption write SetTableCaption stored StoreTableCaption;
    {Restrictions over table object (read only/hidden) when project is open for editing}
    property Restriction: TTableRestriction read GetRestriction write FRestriction default trNone;
  end;

  {TGDAOFields is a collection of TGDAOField objects that holds information about all the
   fields in the table}
  TGDAOFields = class(TCollection)
  private
    FOwnerTable : TGDAOTable;
    function GetItem(i:integer):TGDAOField;
    procedure SetItem(i: integer; const Value: TGDAOField);
  public
    constructor Create( AOwnerTable:TGDAOTable );
    function GetOwner: TPersistent; override;
    {Adds a new field object}
    function Add: TGDAOField; overload;
    {Adds a new field with the specified parameters}
    function Add(AFieldName: string; ADataType:TGDAODataType;
      ASize, ASize2: integer; ARequired: boolean ):TGDAOField; overload;
    {Returns the index of the field object specified by field name AFieldName}
    function IndexOf(AFieldName:string):integer;
    {Returns the index of the field object, in the Fields collection, specified by the field ID AFID}
    function IndexOfFid(AFID: integer): integer;
    {Returns the TGDAOField object associated with the field specified by the field name AFieldName}
    function FindByName(AFieldName: String): TGDAOField;
    {Returns the TGDAOField object associated with the field specified by the internal field identifier AFID}
    function FindByID(AFID: integer): TGDAOField;
    {Returns the TGDAOTable object of table which owns this field object}
    property OwnerTable:TGDAOTable read FOwnerTable;
    {Provides indexed access to the fields in the TGDAOFields collection}
    property Items[i:integer]:TGDAOField read GetItem write SetItem; default;
  end;

  {TRelationshipList holds a list of TGDAORelationship objects}
  TRelationshipList = class(TList)
  private
    function GetItem(i: integer): TGDAORelationship;
  public
    {Provides indexed access to the relationships in the TRelationshipList list}
    property Item[i: integer]: TGDAORelationship read GetItem; default;
  end;

  {TGDAOField object holds all the information about a single field in a table in the data dictionary}
  TGDAOField = class(TCollectionItem)
  private
    FFieldName: string;
    FSize: integer;
    FSize2: integer;
    FRequired: boolean;
    FDescription: string;
    FDefaultValue: string;
    FFID: integer;
    FConstraintExpr: string;
    FConstraintName: string;
    FDefaultValueSpecific: boolean;
    FConstraintExprSpecific: boolean;
    FSeedValue: integer;
    FIncrementValue: integer;
    FGeneratedByRelationship: boolean;
    FDomainName: string;
    FConstraintDefaultName: string;
    FConstraintNotNullName: string;
    FDataType: TGDAODataType;
    FDomain: TGDAODomain;
    FData: TObject;
    FExpression: string;
    FFieldCaption: string;
    FRestriction: TFieldRestriction;
    function GetDataType: TGDAODataType;
    procedure SetFieldName(const AName: string);
    function GetConstraintExpr: string;
    function GetDataTypeName: string;
    function GetDefaultValue: string;
    function GetDomainName: string;
    function GetIncrementValue: integer;
    function GetSeedValue: integer;
    function GetSize: integer;
    function GetSize2: integer;
    procedure SetDataTypeName(const Value: string);
    procedure SetDefaultValue(const Value: string);
    procedure SetDomainName(const AValue: string);
    procedure SetDataType(const Value: TGDAODataType);
    function GetInPrimaryKey: boolean;
    procedure SetDomain(const Value: TGDAODomain);
    function GetFieldCaption: string;
    procedure SetFieldCaption(const Value: string);
    function StoreFieldCaption: boolean;
    function GetRestriction: TFieldRestriction;
    procedure AssignDomainInternalFields(ADomain: TGDAODomain);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDisplayName: string; override;
    {Returns a reference to the data dictionary object which this table field belongs to}
    function OwnerDatabase: TGDD;
    {Returns a reference to the table object which this field belongs to}
    function OwnerTable: TGDAOTable;
    {Returns true if the field belongs to a foreign key. If AParents parameter is provided, the AParents
     list is filled with all relationships which contains the specified field as a child field}
    function IsForeignKey(AParents: TRelationshipList = nil): boolean;
    {Returns true if the field belongs to a parent key in a relationship}
    function IsRelationshipParentKey: boolean;
    {Returns true if the field belongs to a relationship in the data dictionary (as parent or child)}
    function IsInRelationship: boolean;
    {Returns a reference to the domain object associated with this field. This property is nil if
     the field has no domain associated with it}
    property Domain: TGDAODomain read FDomain write SetDomain;
    {Returns the data type object which holds information about the field data type}
    property DataType: TGDAODataType read GetDataType write SetDataType;
    {Placeholder for a generic object}
    property Data: TObject read FData write FData;
    {Returns true if the field belongs to a primary key}
    property InPrimaryKey : Boolean read GetInPrimaryKey;
  published
    {Contains the name of the field in the data dictionary}
    property FieldName:string read FFieldName write SetFieldName;
    {Contains the name of the data type of the field. The name is not the physical type in the database, but instead
     a reference to the internal data types in the TGDAODatabase.DataTypes collection}
    property DataTypeName:String read GetDataTypeName write SetDataTypeName;
    {Contains the size of the field}
    property Size:integer read GetSize write FSize;
    {Contains the secondary size of the field (precision, for example, in numeric fields)}
    property Size2:integer read GetSize2 write FSize2;
    {Contains the description/comments of the field}
    property Description: string read FDescription write FDescription;
    {Contains the default value expression for the field}
    property DefaultValue:string read GetDefaultValue write SetDefaultValue;
    {Returns true if the field is not null (required) in the database}
    property Required:boolean read FRequired write FRequired;
    {Contains the internal field identifier}
    property FID: integer read FFID write FFID;
    {Contains the name of the domain associated with the field}
    property DomainName: String read GetDomainName write SetDomainName;
    {For internal use only}
    property DefaultValueSpecific: Boolean read FDefaultValueSpecific write FDefaultValueSpecific;
    {Contains the field expression, in case of a computed (calculated) field}
    property Expression: string read FExpression write FExpression;
    {Contains the check constraint expression of the field}
    property ConstraintExpr : String read GetConstraintExpr write FConstraintExpr;
    {Contains the check constraint name of the field}
    property ConstraintName : String read FConstraintName write FConstraintName;
    {For internal use only}
    property ConstraintExprSpecific : Boolean read FConstraintExprSpecific write FConstraintExprSpecific;
    {Contains the default constraint name of the field}
    property ConstraintDefaultName: string read FConstraintDefaultName write FConstraintDefaultName;
    {Contains the not null constraint name of the field}
    property ConstraintNotNullName: string read FConstraintNotNullName write FConstraintNotNullName;
    {Contains the seed (initial) value of the field, for autoincrement fields}
    property SeedValue : Integer read GetSeedValue write FSeedValue;
    {Contains the increment value of the field, for autoincrement fields}
    property IncrementValue : Integer read GetIncrementValue write FIncrementValue;
    {For internal use only}
    property GeneratedByRelationship: Boolean read FGeneratedByRelationship write FGeneratedByRelationship;
    {Contains the caption of the field}
    property FieldCaption: string read GetFieldCaption write SetFieldCaption stored StoreFieldCaption;
    {Restrictions over field object (read only) when project is open for editing}
    property Restriction: TFieldRestriction read GetRestriction write FRestriction default frNone;
  end;

  {TGDAOIndexes is a collection of TGDAOIndex objects that holds information about all the
   indexes in the table}
  TGDAOIndexes = class(TCollection)
  private
    FOwnerTable : TGDAOTable;
    function GetItem(i:integer):TGDAOIndex;
    procedure SetItem(i: integer; const Value: TGDAOIndex);
  public
    constructor Create( AOwnerTable:TGDAOTable );
    function GetOwner: TPersistent; override;
    {Adds a new index object}
    function Add: TGDAOIndex; overload;
    {Adds a new index object with name AName}
    function Add(AName: string):TGDAOIndex; overload;
    {Returns a reference to the data dictionary (TGDD) object which the index list to}
    function OwnerDatabase:TGDD;
    {Returns the TGDAOIndex object associated with the index specified by the index name AIndexName}
    function FindByName(AIndexName: string): TGDAOIndex;
    {Returns the index, in the Indexes collection, of the index specified by AIndexName}
    function IndexOf(AIndexName:string):integer;
    {Returns the index, in the Indexes collection, of the index specified by the internal indentifier AIID}
    function IndexOfIId(AIID: Integer): integer;
    {Provides indexed access to the indexes in the TGDAOIndexes collection}
    property Items[i:integer]:TGDAOIndex read GetItem write SetItem; default;
  end;

  {TGDAOIndex object holds all the information about a single index in a table in the data dictionary}
  TGDAOIndex = class(TCollectionItem)
  private
    FIndexName  : string;
    FIndexOrder : TIndexOrder;
    FIID        : Integer;
    FIndexType  : TIndexType;
    FIFields    : TGDAOIFields;
    FData: TObject;
    FOwnerTable: TGDAOTable;
    procedure SetIFields(const Value: TGDAOIFields );
    function GetIsPrimary: boolean;
  public
    constructor Create(ACollection:TCollection);  override;
    constructor CreateFromTable(AOwnerTable: TGDAOTable);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function  GetDisplayName: string; override;
    {Returns a reference to the data dictionary (TGDD) object that holds this index}
    function OwnerDatabase: TGDD;
    {Returns the TGDAOTable object of the table which owns this index object}
    function OwnerTable: TGDAOTable;
    {Returns the name of the table which this index belongs to}
    function TableName: string;
    {Returns true if the index contains the field specified by AField}
    function HasField(AField: TGDAOField): boolean;
    {Returns true if the index is a primary key}
    property IsPrimary: boolean read GetIsPrimary;
    {Placeholder for a generic TObject}
    property Data: TObject read FData write FData;
  published
    {Contains the name of the index in the database}
    property IndexName: string read FIndexName write FIndexName;
    {Contains the index type, which specifies if the index is unique (exclusive) or not}
    property IndexType: TIndexType read FIndexType write FIndexType;
    {Contains the order of index (ascending/descending)}
    property IndexOrder: TIndexOrder read FIndexOrder write FIndexOrder;
    {Contains the internal identifier of the index}
    property IID: Integer read FIID write FIID;
    {Contains the list of fields which belongs to the index}
    property IFields: TGDAOIFields read FIFields write SetIFields;
  end;

  {TGDAOIFields is a collection of TGDAOIField object which holds information about all the fields
  belonging to the index}
  TGDAOIFields = class(TCollection)
  private
    FOwnerIndex : TGDAOIndex;
    function GetItem(i:integer):TGDAOIField;
    procedure SetItem(i: integer; const Value: TGDAOIField);
    function GetField(i: integer): TGDAOField;
    procedure SetField(i: integer; const Value: TGDAOField);
  public
    constructor Create( AOwnerIndex:TGDAOIndex );
    function GetOwner: TPersistent; override;
    {Adds a new field to the index}
    function Add: TGDAOIField; overload;
    {Adds a new field to the index. The field is specified by field name AFieldName}
    function Add(AFieldName: string):TGDAOIField; overload;
    {Adds a new field to the index. The field is specified by the object AField, with order specified by AOrder}
    function Add(AField: TGDAOField; AOrder: TIndexFieldOrder = ioAsc): TGDAOIField; overload;
    {Returns the TGDAOIField object associated with the field AField}
    function FindByField(AField: TGDAOField): TGDAOIField;
    {Remove from the index the field specified by AField. If the field doesn't belong to the index, nothing happens}
    procedure RemoveField(AField: TGDAOField);
    {Returns the index field, in the IFields collection, of the index field specified by field name AFieldName}
    function IndexOf( AFieldName:string ):integer;
    {Provides indexed access to the index field object in the TGDAOIFields collection}
    property Items[i:integer]:TGDAOIField read GetItem write SetItem; default;
    {Provides indexed accesso to the field object in the TGDAOIFields collection}
    property Field[i:integer]:TGDAOField read GetField write SetField;
  end;

  {TGDAOIField object holds information about a field belonging to an index}
  TGDAOIField = class(TCollectionItem)
  private
    FFieldOrder: TIndexFieldOrder;
    FField : TGDAOField;
    FKeyByRelationship: boolean;
    function GetFieldName: string;
    procedure SetFieldName(const Value: string);
    function GetFieldIndex: integer;
    procedure SetFieldIndex(const Value: integer);
    procedure SetField(const Value: TGDAOField);
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDisplayName: string; override;
    {Returns the TGDAOTable object of the table which owns this index field}
    function OwnerTable: TGDAOTable;
    {Returns the TGDAOIndex object to which this collection belongs to}
    function OwnerIndex: TGDAOIndex;
    {Returns a reference to the TGDAOField object associated with this index field} 
    property Field: TGDAOField read FField write SetField;
    {Returns the name of the field associated with this index field} 
    property FieldName:string read GetFieldName write SetFieldName;
  published
    {Returns the index of the field in the TGDAOTable.Fields collection, assocaited with this index field}
    property FieldIndex:integer read GetFieldIndex write SetFieldIndex;
    {Specifies the sort order of this field in the index (ascending/descending)}
    property FieldOrder: TIndexFieldOrder read FFieldOrder write FFieldOrder;
    {For internal use only}
    property KeyByRelationship: boolean read FKeyByRelationship write FKeyByRelationship;
  end;

  {TGDAORelationshipFieldLinks collection holds information about all field link (parent/child) in a relationship.
   A relationship can have more than one field link, so this object is a collection}
  TGDAORelationshipFieldLinks = class(TCollection)
  private
    FRelationship: TGDAORelationship;
    function GetItem(i: integer): TGDAORelationshipFieldLink;
    procedure SetItem(i: integer; const Value: TGDAORelationshipFieldLink);
  public
    constructor Create(ARelationship: TGDAORelationship);
    function GetOwner: TPersistent; override;
    {Adds a new relationship link}
    function Add: TGDAORelationshipFieldLink;
    {Returns the index, in the field links collection, of the field link which has the field AField as a parent field}
    function IndexOfParentField(AField: TGDAOField): integer;
    {Returns the index, in the field links collection, of the field link which has the field AField as a child field}
    function IndexOfChildField(AField: TGDAOField): integer;
    {Provides indexed access to the relationship field links in the TGDAORelationshipFieldLinks collection}
    property Items[i: integer]: TGDAORelationshipFieldLink read GetItem write SetItem; default;
  end;

  {TGDAORelationshipFieldLink object holds information about a field link (parent/child) in a relationship.
   A relationship can have more than one field link}
  TGDAORelationshipFieldLink = class(TCollectionItem)
  private
    FParentField: TGDAOField;
    FChildField: TGDAOField;
    function GetChildField: TGDAOField;
    function GetChildFieldName: string;
    function GetParentField: TGDAOField;
    function GetParentFieldName: string;
    procedure SetChildFieldName(const Value: string);
    procedure SetParentFieldName(const Value: string);
    procedure SetChildField(const Value: TGDAOField);
    procedure SetParentField(const Value: TGDAOField);
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDisplayName: string; override;
    {Returns a reference of the relationship object which this field link belongs to}
    function OwnerRelationship: TGDAORelationship;
    {Returns a reference to the TGDAOField object associated with the parent field of this link}
    property ParentField: TGDAOField read GetParentField write SetParentField;
    {Returns a reference to the TGDAOField object associated with the child field of this link}
    property ChildField: TGDAOField read GetChildField write SetChildField;
  published
    {Contains the name of the parent field of this relationship link} 
    property ParentFieldName: string read GetParentFieldName write SetParentFieldName;
    {Contains the name of the child field of this relationship link} 
    property ChildFieldName: string read GetChildFieldName write SetChildFieldName;
  end;

  {TGDAORelationships is a collection of TGDAORelationship that holds information about all relationships defined
   in the data dictionary}
  TGDAORelationships = class(TCollection)
  private
    FOwnerDatabase : TGDD;
    function GetItem(i:integer):TGDAORelationship;
    procedure SetItem(i: integer; const Value: TGDAORelationship);
  public
    constructor Create( AOwnerDatabase:TGDD );
    function GetOwner: TPersistent; override;
    {Adds a new relationship}
    function Add(ARelationshipName, AParentTableName, AChildTableName: string;
      AUpdateMethod: TUpdateMethod; ADeleteMethod: TDeleteMethod ): TGDAORelationship; overload;
    {Adds a new relationship}
    function Add: TGDAORelationship; overload;
    {Returns the index, in the Relationships collection, of the relatinship specified by ARelationshipName}
    function IndexOf(ARelationshipName: string): integer;
    {Returns the index, in the Relationships collection, of the relatinship specified by the internal identifier AID}
    function IndexOfRelID(AID: integer): integer;
    {Returns the TGDAORelationship object associated with the relationship specified by the internal identifier AID}
    function FindByID(AID: integer): TGDAORelationship;
    {Returns a reference to the data dictionary (TGDD) object that holds this relationship collection}
    property OwnerDatabase:TGDD read FOwnerDatabase;
    {Provides indexed access to the relationships in the TGDAORelationships collection}
    property Items[i:integer]: TGDAORelationship read GetItem write SetItem; default;
  end;

  {TGDAORelationship object holds all the information about a single relationship in the data dictionary}
  TGDAORelationship = class(TCollectionItem)
  private
    FParentTable: TGDAOTable;
    FChildTable: TGDAOTable;
    FFieldLinks: TGDAORelationshipFieldLinks;
    FData: TObject;
    FDescription: string;
    FRelID: integer;
    FRelationshipName: string;
    FDeleteMethod: TDeleteMethod;
    FUpdateMethod: TUpdateMethod;
    FParentIndex: TGDAOIndex;
    FRelationshipType: TGDAORelationshipType;
    function GetParentTableName: string;
    function GetChildTableName: string;
    procedure SetParentTableName(const Value: string);
    procedure SetChildTableName(const Value: string);
    procedure SetChildTable(const Value: TGDAOTable);
    procedure SetParentTable(const Value: TGDAOTable);
    procedure SetFieldLinks(const Value: TGDAORelationshipFieldLinks);
    function GetParentTableIndex: integer;
    function GetChildTableIndex: integer;
    procedure SetParentTableIndex(const Value: integer);
    procedure SetChildTableIndex(const Value: integer);
    function GetParentIndexID: integer;
    procedure SetParentIndexID(const Value: integer);
    function GetKeyLink(i: integer): TGDAORelationshipFieldLink;
    function GetKeyLinkCount: integer;
  public
    constructor Create( ACollection:TCollection ); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function  GetDisplayName: string; override;
    {Returns a reference to the data dictionary (TGDD) object that holds this relationship}
    function OwnerDatabase:TGDD;
    {Contains the name of the relationship's parent table}
    property ParentTableName: string read GetParentTableName write SetParentTableName;
    {Contains the name of the relationship's child table}
    property ChildTableName: string read GetChildTableName write SetChildTableName;
    {Contains a reference of the TGDAOTable object associated with the relationship's parent table}
    property ParentTable: TGDAOTable read FParentTable write SetParentTable;
    {Contains a reference of the TGDAOTable object associated with the relationship's child table}
    property ChildTable: TGDAOTable read FChildTable write SetChildTable;
    {Contains a reference of the index in the parent table to which the relationship is associated to}
    property ParentIndex: TGDAOIndex read FParentIndex write FParentIndex;
    {Placeholder for a generic TObject}
    property Data: TObject read FData write FData;
    {Returns the number of field links}
    property KeyLinkCount: integer read GetKeyLinkCount;
    {Provides indexed access to the field links of the relationship}
    property KeyLinks[i: integer]: TGDAORelationshipFieldLink read GetKeyLink;
  published
    {Contains the name of the relationship object in the data dictionary}
    property RelationshipName: string read FRelationshipName write FRelationshipName;
    {Contains the index value of the parent table}
    property ParentTableIndex: integer read GetParentTableIndex write SetParentTableIndex;
    {Contains the index value of the child table}
    property ChildTableIndex: integer read GetChildTableIndex write SetChildTableIndex;
    {Specifies the update method of the relationship}
    property UpdateMethod: TUpdateMethod read FUpdateMethod write FUpdateMethod;
    {Specifies the delete method of the relationship}
    property DeleteMethod: TDeleteMethod read FDeleteMethod write FDeleteMethod;
    {Contains the description/comments for the relationship}
    property Description: string read FDescription write FDescription;
    {Contains the internal identifier of the relationship}
    property RelID: integer read FRelID write FRelID;
    {Contains the list of field links of the relationship. This is for internal use, to access the list
    of field links, use KeyLinks property}
    property FieldLinks: TGDAORelationshipFieldLinks read FFieldLinks write SetFieldLinks;
    {Contains the indexed value of the TGDAOIndex object associated with the parent key of the relationship.
     See ParentIndex property}
    property ParentIndexID: integer read GetParentIndexID write SetParentIndexID;
    {Contains the type of the relationship}
    property RelationshipType: TGDAORelationshipType read FRelationshipType write FRelationshipType;
  end;

  {TGDAOConstraints is a collection of TGDAOConstraint that holds information about all constraints defined
   in a table in the data dictionary}
  TGDAOConstraints = class(TCollection)
  private
    FOwnerTable : TGDAOTable;
    function GetItem(i:integer): TGDAOCOnstraint;
    procedure SetItem(i:integer; const Value: TGDAOCOnstraint);
  public
    constructor Create(AOwnerTable:TGDAOTable);
    function GetOwner: TPersistent; override;
    {Adds a new constraint} 
    function Add:TGDAOConstraint;
    {Returns the constraint, in the Constraints collection, of the constraint specified by AConstraintName}
    function IndexOf(AConstraintName:string):integer;
    {Returns the TGDAOConstraint object associated with the constraint specified by the constraint name AConstraintName}
    function FindByName(AConstraintName: string): TGDAOConstraint;
    function IndexOfCID(ACID: Integer): integer;
    {Adds a new constraint}
    function AddConstraint(AName,AExpression:string):TGDAOConstraint;
    {Returns the TGDAOTable object of the table which owns this constraint object}
    property OwnerTable: TGDAOTable read FOwnerTable;
    {Provides indexed access to the constraints in the TGDAOConstraints collection}
    property Items[i:integer]:TGDAOCOnstraint read GetItem write SetItem; default;
  end;

  {TGDAOConstraint object holds all the information about a single constraint in a table in the data dictionary}
  TGDAOConstraint = class(TCollectionItem)
  private
    FConstraintName : string;
    FExpression : string;
    FCID : Integer;
    FData: TObject;
  public
    procedure Assign(Source:TPersistent); override;
    function GetDisplayName: string; override;
    {Returns the TGDAOTable object of the table which owns this constraint object}
    function OwnerTable: TGDAOTable;
    {Placeholder for a generic TObject}
    property Data: TObject read FData write FData;
  published
    {Contains the name of the constraint in the data dictionary}
    property ConstraintName:string read FConstraintName write FConstraintName;
    {Contains the check constraint expression}
    property Expression:string read FExpression write FExpression;
    {Contains the internal identifier of the constraint}
    property CID: Integer read FCID write FCID;
  end;

  {TGDAOTriggers is a collection of TGDAOTrigger that holds information about all triggers defined
   in a table in the data dictionary}
  TGDAOTriggers = class(TCollection)
  private
    FOwnerTable: TGDAOTable;
    function GetItem(i: integer): TGDAOTrigger;
    procedure SetItem(i: integer; const Value: TGDAOTrigger);
  public
    constructor Create(AOwnerTable: TGDAOTable);
    function GetOwner: TPersistent; override;
    {Adds a new trigger}
    function Add: TGDAOTrigger; overload;
    {Adds a new trigger}
    function Add(AName: string; AImplementation: string=''): TGDAOTrigger; overload;
    {Returns the index, in the Triggers collection, of the trigger specified by AName}
    function IndexOf(AName: string): integer;
    {Returns the TGDAOTrigger object associated with the trigger specified by the name AName}
    function FindByName(AName: string): TGDAOTrigger;
    {Returns the TGDAOTable object of the table which owns this trigger collection}
    property OwnerTable: TGDAOTable read FOwnerTable;
    {Provides indexed access to the triggers in the TGDAOTriggers collection}
    property Items[i: integer]: TGDAOTrigger read GetItem write SetItem; default;
  end;

  {TGDAOTrigger object holds all the information about a single trigger in a table in the data dictionary}
  TGDAOTrigger = class(TCollectionItem)
  private
    FName: string;
    FDescription: string;
    FImplementation: string;
    FData: TObject;
    function GetTableName: string;
    function GetTable: TGDAOTable;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function GetDisplayName: string; override;
    {Returns a reference to the data dictionary (TGDD) object that holds this trigger}
    function OwnerDatabase: TGDD;
    {Returns the TGDAOTable object of the table which owns this trigger object}
    function OwnerTable: TGDAOTable;
    {Returns the TGDAOTable object of the table which owns this trigger object}
    property Table: TGDAOTable read GetTable;
    {Returns the name of the table which owns this trigger object}
    property TableName: string read GetTableName;
    {Placeholder for a generic TObject}
    property Data: TObject read FData write FData;
  published
    {Contains the name of the trigger}
    property Name: string read FName write FName;
    {Contains the comments/description for the trigger}
    property Description: string read FDescription write FDescription;
    {Contains the trigger source code (implementation code)}
    property ImplementationCode: string read FImplementation write FImplementation;
  end;

  {TGDAODataTypes is a collection of TGDAODataType objects that holds information about all data types available in the data dictionary}
  TGDAODataTypes = class(TCollection)
  private
    FOwnerDatabase: TGDD;
    function GetItem(i: integer): TGDAODataType;
    procedure SetItem(i: integer; const Value: TGDAODataType);
  public
    constructor Create(AOwnerDatabase: TGDD);
    {Adds a new data type}
    function Add: TGDAODataType; overload;
    {Adds a new data type}
    function Add(AName, APhysical: String; FSizeReq, FSize2Req: Boolean;
                 ANativeDataType: TNativeDataType; ANativeSubType: TNativeSubType;
                 ACounter: Boolean = false; ASeed: Boolean = false; AIncrement: Boolean = False): TGDAODataType; overload;
    function GetOwner: TPersistent; override;
    {Returns the TGDAODataType object associated with the data type specified by the name AName}
    function FindByName(AName: String): TGDAODataType;
    {Provides indexed access to the data types in the TGDAODataTypes collection}
    property Items[i: integer]: TGDAODataType read GetItem write SetItem; default;
  end;

  {TGDAODataType object holds all the information about an available data type in the data dictionary}
  TGDAODataType = class(TCollectionItem)
  private
    FForeignDataType: TGDAODataType;
    FName: String;
    FSizeIsRequired: Boolean;
    FNativeSubType: TNativeSubType;
    FNativeDataType: TNativeDataType;
    FSeedIsRequired: Boolean;
    FCounter: Boolean;
    FIncrementIsRequired: Boolean;
    FSize2IsRequired: Boolean;
    FPhysical: String;
    FComputed: boolean;
    FMinSize: integer;
    FCheckSize: boolean;
    FMaxSize: integer;
    FDefaultSize: integer;
    FDefaultSize2: integer;
    function GetForeignDataTypeName: string;
    procedure SetForeignDataTypeName(const AName: string);
    function CheckSizeStored: boolean;
  public
    procedure Assign(Source: TPersistent); override;
    {Set some properties value associated with size settings. See the individual properties for more information}
    procedure SetSizeSettings(ADefaultSize, ADefaultSize2: integer;
      ACheckSize: boolean; AMinSize, AMaxSize: integer);
  published
    {Name of the data type. It's not related to the physical name of the data type, instead it's a logical name}
    property Name: String read FName write FName;
    {Contains the physical name of the data type in the database}
    property Physical: String read FPhysical write FPhysical;
    {SizeIsRequired true if the data type requires a size}
    property SizeIsRequired: Boolean read FSizeIsRequired write FSizeIsRequired;
    {Size2IsRequired true if the data type requires a secondary size (precision)}
    property Size2IsRequired: Boolean read FSize2IsRequired write FSize2IsRequired;
    {Counter property is true if the data type is an autoincrement type}
    property Counter: Boolean read FCounter write FCounter;
    {SeedIsRequired is true if the data type requires a seed value (for autoincrement types)}
    property SeedIsRequired: Boolean read FSeedIsRequired write FSeedIsRequired;
    {IncrementIsRequired is true if the data type requires an increment value (for autoincrement types)}
    property IncrementIsRequired: Boolean read FIncrementIsRequired write FIncrementIsRequired;
    {Internal use only}
    property ForeignDataTypeName: String read GetForeignDataTypeName write SetForeignDataTypeName;
    {Contains the native data type}
    property NativeDataType: TNativeDataType read FNativeDataType write FNativeDataType;
    {Contains the native sub data type}
    property NativeSubType: TNativeSubType read FNativeSubType write FNativeSubType;
    {Computed is true if the data type is a computed (expression) value}
    property Computed: boolean read FComputed write FComputed;
    {CheckSize is true if the data type has a restricted range for the size}
    property CheckSize: boolean read FCheckSize write FCheckSize;
    {MinSize contains the minimum size value of the data type}
    property MinSize: integer read FMinSize write FMinSize stored CheckSizeStored;
    {MaxSize contains the maximum size value of the data type}
    property MaxSize: integer read FMaxSize write FMaxSize stored CheckSizeStored;
    {DefaultSize contains the default size value}
    property DefaultSize: integer read FDefaultSize write FDefaultSize;
    {DefaultSize2 contains the default size2 value}
    property DefaultSize2: integer read FDefaultSize2 write FDefaultSize2;
  end;

  {TGDAODomains is a collection of TGDAODomain objects that holds information about all domains available in the data dictionary}
  TGDAODomains = class(TCollection)
  private
    FOwnerDatabase: TGDD;
    function GetItem(i: integer): TGDAODomain;
    procedure SetItem(i: integer; const Value: TGDAODomain);
  public
    constructor Create(AOwnerDatabase: TGDD);
    function Add: TGDAODomain;
    {Returns the TGDAODomain object associated with the domain specified by the name AName}
    function FindByName(AName: String): TGDAODomain;
    function GetOwner: TPersistent; override;
    {Returns the index, in the Domains collection, of the domains specified by AName}
    function IndexOf(AName: String): Integer;
    {Provides indexed access to the domains in the TGDAODomains collection}
    property Items[i: integer]: TGDAODomain read GetItem write SetItem; default;
  end;

  {TGDAODomain object holds all the information about a domain in the data dictionary}
  TGDAODomain = class(TCollectionItem)
  private
    FDataType: TGDAODataType;
    FInformation: String;
    FName: String;
    FDefaultValue: String;
    FSize2: Integer;
    FConstraintExpr: String;
    FSeedValue: Integer;
    FSize: Integer;
    FIncrementValue: Integer;
    FInDatabase: boolean;
    FData: TObject;
    function GetDataTypeName: String;
    procedure SetDataTypeName(const Value: String);
    function OwnerDatabase: TGDD;
    procedure SetDataType(const Value: TGDAODataType);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {Returns true if the domain is being used in any field of the database}
    function IsBeingUsed: boolean;
    {Returns true if the domain is being used in any field of the database
     and the field is part of a relationship}
    function IsInRelationship: boolean;
    {Fill the list of field objects where the domain is being used}
    procedure FillUsedFields(AFieldList: TObjectList);
    {Contains the data type information of this domain}
    property DataType: TGDAODataType read FDataType write SetDataType;
    {Placeholder for a generic TObject}
    property Data: TObject read FData write FData;
  published
    {Contains the name of the domain}
    property Name : String read FName write FName;
    {Contains the check constrain expression of the domain}
    property ConstraintExpr: String read FConstraintExpr write FConstraintExpr;
    {Contains the size of the domain data type}
    property Size: Integer read FSize write FSize;
    {Contains the secondary size of the domain data type}
    property Size2: Integer read FSize2 write FSize2;
    {Contains the default value expression of this domain}
    property DefaultValue: String read FDefaultValue write FDefaultValue;
    {Contains the name of the data type of this domain}
    property DataTypeName: String read GetDataTypeName write SetDataTypeName;
    {Contains the description/comments of the domain}
    property Information: String read FInformation write FInformation;
    {Contains the seed (initial) value of the domain}
    property SeedValue: Integer read FSeedValue write FSeedValue;
    {Contains the increment value of the domain, for autoincrement fields}
    property IncrementValue: Integer read FIncrementValue write FIncrementValue;
    {InDatabase is true when the domain is actually in the physical database}
    property InDatabase: boolean read FInDatabase write FInDatabase;
  end;

  TGDAOPropDefType = (pdtInteger, pdtString);

  TGDAOPropDef = class(TCollectionItem)
  private
    FDefaultValue: Variant;
    FPropName: string;
    FDataType: TGDAOPropDefType;
  published
  public
    property PropName: string read FPropName write FPropName;
    property DefaultValue: Variant read FDefaultValue write FDefaultValue;
    property DataType: TGDAOPropDefType read FDataType write FDataType;
  end;

  TGDAOPropDefs = class(TCollection)
  private
    function GetItem(Index: integer): TGDAOPropDef;
    function ReadProp(APropName: string; APropValues: TStrings): Variant;
    function WriteProp(APropName: string; APropValues: TStrings; AValue: Variant): Variant;
    procedure GetPropParams(APropName: string; var AType: TGDAOPropDefType; var ADefValue: Variant);
  public
    function FindProp(APropName: string): TGDAOPropDef;
    function Add(APropName: string; AType: TGDAOPropDefType; ADefValue: Variant): TGDAOPropDef; overload;
    function Add: TGDAOPropDef; overload;
    {Provides indexed access to the category property definition in the TGDAOCategoryPropDefs collection}
    property Items[Index: integer]: TGDAOPropDef read GetItem;
  end;

  {TGDAOCategories is a collection of TGDAOCategory objects that holds information about all object categories
   in the data dictionary. An example of category is procedures, views, etc.}
  TGDAOCategories      = class(TCollection)
  private
    FOwnerDatabase: TGDD;
    function GetItem(i: integer): TGDAOCategory;
    procedure SetItem(i: integer; const Value: TGDAOCategory);
  public
    constructor Create(AOwnerDatabase: TGDD);
    {Adds a new category}
    function Add(AType: TGDAOCategoryType; ANameS, ANameP: string; ACreate, ADrop: String): TGDAOCategory;
    {Returns the TGDAOCategory object associated with the category specified by the name AName, in plural ("procedures", for example}
    function _FindByNameP(AName: String): TGDAOCategory;
    {Find a category object with the type specified by AType}
    function FindByType(AType: TGDAOCategoryType): TGDAOCategory;
    function GetOwner: TPersistent; override;
    {Provides indexed access to the categories in the TGDAOCategories collection}
    property Items[i: integer]: TGDAOCategory read GetItem write SetItem; default;
  end;

  {TGDAOCategory object holds all the information about a category in the data dictionary. A category is a group of
   database objects that doesn't fit in the other available objects (tables, fields, relationships, etc.). An example
   of category is "procedure" (for stored procedures), "view" (for views), etc.}
  TGDAOCategory = class(TCollectionItem)
  private
    FObjects      : TGDAOObjects;
    FDropTemplate: String;
    FCreateTemplate: String;
    FCategoryNameP: String;
    FCategoryNameS: String;
    FData: TObject;
    FCategoryType: TGDAOCategoryType;
    FPropDefs: TGDAOPropDefs;
    procedure SetObjects(const Value: TGDAOObjects);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {Placeholder for a generic TObject}
    property Data: TObject read FData write FData;
    {Contains custom properties of the category}
    property PropDefs: TGDAOPropDefs read FPropDefs;
  published
    {not used}
    property CategoryNameP: String read FCategoryNameP write FCategoryNameP stored false;
    {not used}
    property CategoryNameS: String read FCategoryNameS write FCategoryNameS stored false;
    {not used}
    property CreateTemplate : String read FCreateTemplate write FCreateTemplate stored false;
    {not used}
    property DropTemplate : String read FDropTemplate write FDropTemplate stored false;
    {Contains the type of the category}
    property CategoryType: TGDAOCategoryType read FCategoryType write FCategoryType;
    {Contains the list of objects belonging to this category}
    property Objects : TGDAOObjects read FObjects write SetObjects;
  end;

  {TGDAOObjects is a collection of TGDAOObject objects that holds information about all database objects
   in the data dictionary. Examples of object are procedures, views, etc.}
  TGDAOObjects = class(TCollection)
  private
    FOwnerDatabase: TGDD;
    FOwnerCategory: TGDAOCategory;
    function GetItem(i: integer): TGDAOObject;
    procedure SetItem(i: integer; const Value: TGDAOObject);
  public
    constructor Create(AOwnerCategory: TGDAOCategory);
    {Adds a new object}
    function Add(AName: String): TGDAOObject;
    {Returns the TGDAOObject object associated with the object specified by the name AName}
    function FindByName(AName: String): TGDAOObject;
    function GetOwner: TPersistent; override;
    {Returns the index, in the Objects collection, of the object specified by AName}
    function IndexOf(AName: String): Integer;
    {Returns a reference to the category object to which this object belongs to}
    property OwnerCategory: TGDAOCategory read FOwnerCategory;
    {Provides indexed access to the objects in the TGDAOObjects collection}
    property Items[i: integer]: TGDAOObject read GetItem write SetItem; default;
  end;

  {TGDAOObject object holds all the information about a database object in the data dictionary.
   A database object can be a stored procedure, a view, etc.. Each object belongs to a category}
  TGDAOObject = class(TCollectionItem)
  private
    FData: TObject;
    FDescription: string;
    FDropImplementation: string;
    FCreateImplementation: string;
    FObjectName: string;
    FCustomProps: TStrings;
    FRestriction: TObjectRestriction;
    function GetDropImplementation: string;
    procedure SetCustomProps(const Value: TStrings);
    function GetReadOnly: boolean;
    function GetVisible: boolean;
    procedure SetReadOnly(const Value: boolean);
    procedure SetVisible(const Value: boolean);
    function GetRestriction: TObjectRestriction;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {Reads the value of the custom property specified by APropName}
    function ReadProp(APropName: string): variant;
    {Writes the value of the custom property specified by APropName}
    procedure WriteProp(APropName: string; AValue: Variant);
    function OwnerCategory: TGDAOCategory;
    {Placeholder for a generic TObject}
    property Data: TObject read FData write FData;
    {Returns true if Restriction = orReadOnly}
    property ReadOnly: boolean read GetReadOnly write SetReadOnly;
    {Returns true if Restriction <> orHidden}
    property Visible: boolean read GetVisible write SetVisible;
  published
    {Contains the name of the database object}
    property ObjectName: string read FObjectName write FObjectName;
    {Contains the description/comments of the object}
    property Description: string read FDescription write FDescription;
    {Contains the source code of the object (implementation)}
    property CreateImplementation : string read FCreateImplementation write FCreateImplementation;
    {Contains the source code of the Drop command of the object}
    property DropImplementation : string read GetDropImplementation write FDropImplementation stored false;
    {Contains custom properties for the object}
    property CustomProps: TStrings read FCustomProps write SetCustomProps;
    {Restrictions over object (read only/hidden) when project is open for editing}
    property Restriction: TObjectRestriction read GetRestriction write FRestriction default orNone;
  end;

implementation
uses
  uDBProperties;

constructor TGDD.Create(AOwner:TPersistent);
begin
   inherited Create;
   FOwner := AOwner;
   FTables  := TGDAOTables.Create(self);
   FRelationships := TGDAORelationships.Create(self);
   FDomains       := TGDAODomains.Create(Self);
   FCategories    := TGDAOCategories.Create(Self);
   FDataTypes := TGDAODataTypes.Create(Self);
   // next ids
   NextTableID      := 1;
   NextFieldID      := 1;
   NextIndexID      := 1;
   NextConstraintID := 1;
   NextRelationshipID := 1;
end;

destructor TGDD.Destroy;
begin
  {First clean all objects. Then destroy it. This is useful because
   when some objects are destroyed, they try to remove their reference
   from other objects. So, first we clear without destroying to avoid AV in this
   cleaning routines. Then after everything is empty, we can destroy the collections}
   FRelationships.Clear;
   FTables.Clear;
   FDomains.Clear;
   FCategories.Clear;
   FDataTypes.Clear;

   FRelationships.Free;
   FTables.Free;
   FDomains.Free;
   FCategories.Free;
   FDataTypes.Free;
   inherited;
end;

procedure TGDD.FieldDestroyed(AField: TGDAOField);
var
  c: integer;
  idx: integer;
  ATable: TGDAOTable;
begin
  {Iterate through relationships and remove references to
   the field being destroyed}
  ATable := AField.OwnerTable;
  for c := 0 to Relationships.Count - 1 do
  begin
    if Relationships[c].ParentTable = ATable then
    begin
      idx := Relationships[c].FieldLinks.IndexOfParentField(AField);
      if idx > -1 then
        {Set the parentfield field directly, not property, because
         we just need to set the pointer. If we set the property,
         a lot of undesired calls can happen. Same for ChildField below}
        Relationships[c].FieldLinks[idx].FParentField := nil;
    end;

    if Relationships[c].ChildTable = ATable then
    begin
      idx := Relationships[c].FieldLinks.IndexOfChildField(AField);
      if idx > -1 then
        Relationships[c].FieldLinks[idx].FChildField := nil;
    end;
  end;
end;

procedure TGDD.Assign(Source: TPersistent);
begin
  DatabaseTypeID    := TGDD(Source).DatabaseTypeID;
  NextTableID       := TGDD(Source).NextTableID;
  NextFieldID       := TGDD(Source).NextFieldID;
  NextConstraintID  := TGDD(Source).NextConstraintID;
  NextIndexID       := TGDD(Source).NextIndexID;
  Domains           := TGDD(Source).Domains;
  Tables            := TGDD(Source).Tables;
  Relationships     := TGDD(Source).Relationships;
  Categories        := TGDD(Source).Categories;
  FNextRelationshipID := TGDD(Source).FNextRelationshipID;
  FNextTableID       := TGDD(Source).FNextTableID;
  FNextFieldID       := TGDD(Source).FNextFieldID;
  FNextIndexID       := TGDD(Source).FNextIndexID;
  FNextConstraintID  := TGDD(Source).FNextConstraintID;
end;

function TGDD.IndexOfTable( ATableName:string):integer;
begin
  result := Tables.IndexOf(ATableName);
end;

procedure TGDD.Loaded;
var
  c: integer;
  d: integer;
  i: integer;
  ATable: TGDAOTable;
  AIndex: TGDAOIndex;
begin
  {Perform some checking to clean up the dictionary after loading}
  for c := 0 to Tables.Count - 1 do
  begin
    ATable := Tables[c];
    for d := 0 to ATable.Indexes.Count - 1 do
    begin
      AIndex := ATable.Indexes[d];
      for i := AIndex.IFields.Count - 1 downto 0 do
      begin
        if AIndex.IFields[i].Field = nil then
          AIndex.IFields[i].Free;
      end;
    end;
  end;
end;

procedure TGDD.SetRelationships(const Value: TGDAORelationships);
begin
   FRelationships.Assign(Value);
end;

procedure TGDD.SetTables(const Value: TGDAOTables);
begin
   FTables.Assign(Value);
end;

function TGDD.TableByName(ATableName: string): TGDAOTable;
begin
  result := FTables.FindByName(ATableName);
end;

function TGDD.RelationshipByName(ARelationshipName: string): TGDAORelationship;
var
  iRelationship: integer;
begin
   iRelationship:=Relationships.IndexOf(ARelationshipName);
   if iRelationship>=0 then
      result:=Relationships[iRelationship]
   else
      result:=nil;
end;

function TGDD.GetOwner: TPersistent;
begin
   result := FOwner;
end;

function TGDD.IndexExists(AIndexName: string): boolean;
var iTab, iInd: integer;
begin
   for iTab:=0 to Tables.Count-1 do
      with Tables[iTab] do
         for iInd:=0 to Indexes.Count-1 do
            if CompareText(AIndexName,Indexes[iInd].IndexName)=0 then
            begin
               result:=true;
               exit;
            end;
   result:=false;
end;

procedure TGDD.SetDatabaseTypeID(const Value: string);
begin
  FDatabaseTypeID := Value;
  TDBProperties.FillDataTypesObject(FDatabaseTypeID, DataTypes);
end;

procedure TGDD.SetDataTypes(const Value: TGDAODataTypes);
begin
end;

procedure TGDD.SetDomains(const Value: TGDAODomains);
begin
  FDomains.Assign(Value);
end;

procedure TGDD.SetCategories(const Value: TGDAOCategories);
begin
   FCategories.Assign(Value);
end;

function TGDD.GetNextRelationshipID: integer;
begin
   Result := NextRelationshipID;
   NextRelationshipID := NextRelationshipID + 1;
end;

function TGDD.GetNextTableID: integer;
begin
   result := NextTableID;
   NextTableID := NextTableID + 1;
end;

function TGDD.GetDatabaseTypeID: string;
begin
  result := FDatabaseTypeID;
end;

function TGDD.GetDataTypes: TGDAODataTypes;
begin
  result := FDataTypes;
end;

function TGDD.GetNextConstraintID: integer;
begin
   Result := NextConstraintID;
   NextConstraintID := NextConstraintID + 1;
end;

function TGDD.GetNextFieldID: integer;
begin
   result := NextFieldID;
   NextFieldID := NextFieldID + 1;
end;

function TGDD.GetNextIndexID: integer;
begin
  Result := NextIndexID;
  NextIndexID := NextIndexID + 1;
end;

{ TGDAOTables }

constructor TGDAOTables.Create( AOwnerDatabase:TGDD );
begin
   FOwnerDatabase:=AOwnerDatabase;
   inherited Create( TGDAOTable );
end;

function TGDAOTables.FindByID(ATID: integer): TGDAOTable;
var
  i: integer;
begin
   i := IndexOfTid(ATID);
   if i > -1 then
      result := Items[i]
   else
      result := nil;
end;

function TGDAOTables.FindByName(ATableName: string): TGDAOTable;
var
  i: integer;
begin
   i := IndexOf(ATableName);
   if i > -1 then
      result := Items[i]
   else
      result := nil;
end;

function TGDAOTables.GetItem( i:integer ):TGDAOTable;
begin
   result:=TGDAOTable( inherited Items[i] );
end;

function TGDAOTables.Add: TGDAOTable;
begin
  result := TGDAOTable(inherited Add);
  result.UpdateID;
end;

function TGDAOTables.Add(ATableName: string): TGDAOTable;
begin
   result := Add;
   with result do
   begin
      TableName := ATableName;
   end;
end;

procedure TGDAOTables.SetItem(i: integer; const Value: TGDAOTable);
begin
   Items[i].Assign(Value);
end;

function TGDAOTables.IndexOf(ATableName:string):integer;
begin
   for result:=0 to Count-1 do
      if CompareText(ATableName,Items[result].TableName)=0 then
         Exit;
   result:=-1;
end;

function TGDAOTables.GetOwner: TPersistent;
begin
   result:=FOwnerDatabase;
end;

function TGDAOTables.IndexOfTid(ATID: integer): integer;
begin
   for result:=0 to Count-1 do
      if ATID=Items[result].TID then
         exit;
   result:=-1;
end;

{ TGDAOTable }

constructor TGDAOTable.Create( ACollection:TCollection );
begin
   inherited Create( ACollection );
   FRestriction := trNone;

   FFields:=TGDAOFields.Create( self );
   FIndexes:=TGDAOIndexes.Create( self );
   FConstraints:=TGDAOConstraints.Create( self );
   FTriggers:=TGDAOTriggers.Create( self );

   {Create primary key. Should never be destroyed}
   FPrimaryKeyIndex := TGDAOIndex.CreateFromTable(Self);
end;

destructor TGDAOTable.Destroy;
var r: integer;
begin
   // remove table relationships
   with OwnerDatabase.Relationships do
     for r := Count-1 downto 0 do
       if (Items[r].ParentTable = Self) or (Items[r].ChildTable = Self) then
         Delete(r);

   FTriggers.Free;
   FConstraints.Free;
   FIndexes.Free;
   FFields.Free;
   FPrimaryKeyIndex.Free;
   inherited;
end;

function TGDAOTable.OwnerDatabase:TGDD;
begin
  if Collection is TGDAOTables then
    result := TGDAOTables(Collection).FOwnerDatabase
  else
    result := nil;
end;

function TGDAOTable.FieldByName(AFieldName:string):TGDAOField;
var c: integer;
begin
   for c:=0 to FFields.Count-1 do
      if CompareText(FFields[c].FieldName,AFieldName)=0 then
      begin
         result:=FFields[c];
         Exit;
      end;
   result:=nil;
end;

procedure TGDAOTable.FieldDestroyed(AField: TGDAOField);
var
  c: integer;
  d: integer;
begin
  {Remove references to index fields}
  for c := 0 to Indexes.Count - 1 do
    for d := Indexes[c].IFields.Count - 1 downto 0 do
      if Indexes[c].IFields[d].Field = AField then
        Indexes[c].IFields[d].Free;

  {Remove references to primary key fields}
  for d := PrimaryKeyIndex.IFields.Count - 1 downto 0 do
    if PrimaryKeyIndex.IFields[d].Field = AField then
      PrimaryKeyIndex.IFields[d].Free;

  if OwnerDatabase <> nil then
    OwnerDatabase.FieldDestroyed(AField);
end;

procedure TGDAOTable.Assign(Source: TPersistent);
begin
  TableName   := TGDAOTable(Source).TableName;
  Description := TGDAOTable(Source).Description;
  TID         := TGDAOTable(Source).TID;
  OidIndex    := TGDAOTable(Source).OidIndex;
  ExclusiveRecordMask := TGDAOTable(Source).ExclusiveRecordMask;
  Fields      := TGDAOTable(Source).Fields;
  Indexes     := TGDAOTable(Source).Indexes;
  Constraints := TGDAOTable(Source).Constraints;
  Triggers    := TGDAOTable(Source).Triggers;
  PrimaryKeyIndex.Assign(TGDAOTable(Source).PrimaryKeyIndex);
  TableCaption := TGDAOTable(Source).TableCaption;
  Restriction := TGDAOTable(Source).Restriction;
end;

procedure TGDAOTable.BeforeDestruction;
begin
  inherited;
end;

procedure TGDAOTable.SetFields(const Value: TGDAOFields);
begin
   FFields.Assign(Value);
end;

procedure TGDAOTable.SetIndexes(const Value: TGDAOIndexes);
begin
   FIndexes.Assign(Value);
end;

procedure TGDAOTable.SetPrimaryKeyIndex(const Value: TGDAOIndex);
begin
  FPrimaryKeyIndex.Assign(Value);
end;

procedure TGDAOTable.SetReadOnly(const Value: boolean);
begin
  if Restriction <> trHidden then
    if Value then
      Restriction := trReadOnly
    else
      Restriction := trNone;
end;

procedure TGDAOTable.SetConstraints(const Value: TGDAOConstraints);
begin
   FConstraints.Assign(Value);
end;

function TGDAOTable.AddIndex(AIndexName: string; AIndexType: TIndexType): TGDAOIndex;
begin
   result := Indexes.Add;
   with result do
   begin
      IndexName:=AIndexName;
      IndexType:=AIndexType;
   end;
end;

function TGDAOTable.HasForeignFields: boolean;
var i: integer;
begin
  for i := 0 to Fields.Count - 1 do
    if Fields[i].IsForeignKey then
    begin
      result := True;
      exit;
    end;
  result := False;
end;

function TGDAOTable.HasPrimaryKey: boolean;
begin
  result := (PrimaryKeyIndex <> nil) and (PrimaryKeyIndex.IFields.Count > 0);
end;

procedure TGDAOTable.SetTableCaption(const Value: string);
begin
  FTableCaption := Value;
end;

procedure TGDAOTable.SetTriggers(const Value: TGDAOTriggers);
begin
   FTriggers.Assign(Value);
end;

procedure TGDAOTable.SetVisible(const Value: boolean);
begin
  if not Value then
    Restriction := trHidden
  else if Restriction <> trReadOnly then
    Restriction := trNone;
end;

function TGDAOTable.StoreTableCaption: Boolean;
begin
  result := (FTableCaption > '') and (FTableCaption <> FTableName);
end;

function TGDAOTable.TriggerByName(ATriggerName: string): TGDAOTrigger;
var iTrigger: integer;
begin
   iTrigger:=Triggers.IndexOf(ATriggerName);
   if iTrigger>=0 then
      result:=Triggers[iTrigger]
   else
      result:=nil;
end;

procedure TGDAOTable.UpdateID;
begin
  TID := OwnerDatabase.GetNextTableID;
end;

function TGDAOTable.GetDisplayName: string;
begin
   if TableName = '' then
      Result := inherited GetDisplayName
   else
      result := TableName;
end;

function TGDAOTable.GetPrimaryKeyIndex: TGDAOIndex;
begin
  result := FPrimaryKeyIndex;
end;

function TGDAOTable.GetReadOnly: boolean;
begin
  result := Restriction = trReadOnly;
end;

function TGDAOTable.GetRestriction: TTableRestriction;
begin
  result := FRestriction;
end;

function TGDAOTable.GetTableCaption: string;
begin
  if FTableCaption > '' then
    result := FTableCaption
  else
    result := FTableName;
end;

function TGDAOTable.GetVisible: boolean;
begin
  result := Restriction <> trHidden;
end;

{ TGDAORelationships }

constructor TGDAORelationships.Create( AOwnerDatabase:TGDD );
begin
   FOwnerDatabase:=AOwnerDatabase;
   inherited Create( TGDAORelationship );
end;

function TGDAORelationships.FindByID(AID: integer): TGDAORelationship;
var
  i: integer;
begin
   i := IndexOfRelID(AID);
   if i > -1 then
      result := Items[i]
   else
      result := nil;
end;

function TGDAORelationships.GetItem( i:integer ):TGDAORelationship;
begin
   result:=TGDAORelationship( inherited Items[i] );
end;

function TGDAORelationships.Add: TGDAORelationship;
begin
  result := TGDAORelationship(inherited Add);
  result.RelID:=OwnerDatabase.GetNextRelationshipID;
end;

function TGDAORelationships.Add(ARelationshipName, AParentTableName, AChildTableName: string;
  AUpdateMethod:TUpdateMethod;ADeleteMethod:TDeleteMethod ) :TGDAORelationship;
begin
   result := Add;
   with result do
   begin
      RelationshipName:=ARelationshipName;
      ParentTableName:=AParentTableName;
      ChildTableName:=AChildTableName;
      UpdateMethod:=AUpdateMethod;
      DeleteMethod:=ADeleteMethod;
   end;
end;

procedure TGDAORelationships.SetItem(i: integer; const Value: TGDAORelationship);
begin
   Items[i].Assign(Value);
end;

function TGDAORelationships.IndexOf(ARelationshipName: string): integer;
begin
   for result:=0 to Count-1 do
      if CompareText(ARelationshipName,Items[result].RelationshipName)=0 then
         exit;
   result:=-1;
end;

function TGDAORelationships.GetOwner: TPersistent;
begin
   result:=FOwnerDatabase;
end;

function TGDAORelationships.IndexOfRelID(AID: integer): integer;
begin
   for result:=0 to Count-1 do
      if AID=Items[result].RelID then
         exit;
   result:=-1;
end;

{ TGDAORelationship }

procedure TGDAORelationship.Assign(Source: TPersistent);
begin
  RelationshipName := TGDAORelationship(Source).RelationshipName;
  ParentTableIndex := TGDAORelationship(Source).ParentTableIndex;
  ChildTableIndex  := TGDAORelationship(Source).ChildTableIndex;
  UpdateMethod     := TGDAORelationship(Source).UpdateMethod;
  DeleteMethod     := TGDAORelationship(Source).DeleteMethod;
  Description      := TGDAORelationship(Source).Description;
  RelID            := TGDAORelationship(Source).RelID;
  FieldLinks       := TGDAORelationship(Source).FieldLinks;
  ParentIndexID    := TGDAORelationship(Source).ParentIndexID;
  RelationshipType := TGDAORelationship(Source).RelationshipType;
end;

constructor TGDAORelationship.Create(ACollection: TCollection);
begin
  inherited;
  FFieldLinks := TGDAORelationshipFieldLinks.Create(Self);
  UpdateMethod:=umRestrict;
  DeleteMethod:=dmRestrict;
end;

function TGDAORelationship.OwnerDatabase:TGDD;
begin
   result:=TGDAORelationships(Collection).FOwnerDatabase;
end;

function TGDAORelationship.GetChildTableName: string;
begin
   if Assigned(ChildTable) then
      result:=FChildTable.TableName
   else
      result:='';
end;

function TGDAORelationship.GetParentTableName: string;
begin
   if Assigned(ParentTable) then
      result:=ParentTable.TableName
   else
      result:='';
end;

procedure TGDAORelationship.SetChildTableName(const Value: string);
begin
   ChildTable:=OwnerDatabase.TableByName(Value);
end;

procedure TGDAORelationship.SetFieldLinks(const Value: TGDAORelationshipFieldLinks);
begin
  FFieldLinks.Assign(Value);
end;

procedure TGDAORelationship.SetParentTableName(const Value: string);
begin
   ParentTable:=OwnerDatabase.TableByName(Value);
end;

function TGDAORelationship.GetChildTableIndex: integer;
begin
   if Assigned(ChildTable) then
      result:=ChildTable.Index
   else
      result:=-1;
end;

function TGDAORelationship.GetParentTableIndex: integer;
begin
   if Assigned(ParentTable) then
      result:=ParentTable.Index
   else
      result:=-1;
end;

procedure TGDAORelationship.SetChildTableIndex(const Value: integer);
begin
   if Value=-1 then
      ChildTable:=nil
   else
      ChildTable:=OwnerDatabase.Tables[Value];
end;

procedure TGDAORelationship.SetParentTableIndex(const Value: integer);
begin
   if Value=-1 then
      ParentTable:=nil
   else
      ParentTable:=OwnerDatabase.Tables[Value];
end;

destructor TGDAORelationship.Destroy;
begin
  FFieldLinks.Free;
  inherited;
end;

procedure TGDAORelationship.SetChildTable(const Value: TGDAOTable);
begin
  FChildTable := Value;
end;

procedure TGDAORelationship.SetParentTable(const Value: TGDAOTable);
begin
  FParentTable := Value;
end;

procedure TGDAORelationship.SetParentIndexID(const Value: integer);
var
  i: integer;
begin
  if Value = 0 then
    FParentIndex := FParentTable.PrimaryKeyIndex
  else
  begin
    i := FParentTable.Indexes.IndexOfIId(Value);
    if i >= 0 then
      FParentIndex := FParentTable.Indexes[i]
    else
      FParentIndex := nil;
  end;
end;

function TGDAORelationship.GetParentIndexID: integer;
begin
  if Assigned(FParentIndex) then
  begin
    if FParentIndex.IsPrimary then
      result := 0
    else
      result := FParentIndex.IID;
  end
  else
    result := -1;
end;

function TGDAORelationship.GetDisplayName: string;
begin
   if RelationshipName = '' then
      Result := inherited GetDisplayName
   else
      result:=RelationshipName;
end;

function TGDAORelationship.GetKeyLink(i: integer): TGDAORelationshipFieldLink;
var ilink: integer;
begin
  if ParentIndex <> nil then
  begin
    ilink := FieldLinks.IndexOfParentField(ParentIndex.IFields[i].Field);
    if ilink >= 0 then
      result := FieldLinks[ilink]
    else
      result := nil;
  end
  else
    result := FieldLinks[i];
end;

function TGDAORelationship.GetKeyLinkCount: integer;
var i: integer;
begin
  if ParentIndex <> nil then
  begin
    result := ParentIndex.IFields.Count;

    // refresh relationship links with fields from parent index
    for i := FieldLinks.Count-1 downto 0 do
      if ParentIndex.IFields.FindByField(FieldLinks[i].ParentField) = nil then
        FieldLinks.Delete(i);
    for i := 0 to ParentIndex.IFields.Count - 1 do
      if FieldLinks.IndexOfParentField(ParentIndex.IFields[i].Field) < 0 then
        FieldLinks.Add.ParentFieldName := ParentIndex.IFields[i].FieldName;
  end
  else
    result := FieldLinks.Count;
end;

{ TGDAOIndexes }

constructor TGDAOIndexes.Create( AOwnerTable: TGDAOTable );
begin
   FOwnerTable:=AOwnerTable;
   inherited Create( TGDAOIndex );
end;

function TGDAOIndexes.FindByName(AIndexName: string): TGDAOIndex;
var
  i: integer;
begin
  i := IndexOf(AIndexName);
  if i >= 0 then
    result := Items[i]
  else
    result := nil;
end;

function TGDAOIndexes.GetItem(i:integer):TGDAOIndex;
begin
   result:=TGDAOIndex( inherited Items[i] );
end;

procedure TGDAOIndexes.SetItem(i: integer; const Value: TGDAOIndex);
begin
   Items[i].Assign(Value);
end;

function TGDAOIndexes.OwnerDatabase: TGDD;
begin
   result:=FOwnerTable.OwnerDatabase;
end;

function TGDAOIndexes.IndexOf(AIndexName:string): integer;
begin
   for result:=0 to Count-1 do
      if CompareText(AIndexName,Items[result].IndexName)=0 then
         Exit;
   result:=-1;
end;

function TGDAOIndexes.Add: TGDAOIndex;
begin
  result := TGDAOIndex(inherited Add);
  result.IID := TGDAOTables(TGDAOTable(GetOwner).Collection).OwnerDatabase.GetNextIndexID;
end;

function TGDAOIndexes.Add(AName:string): TGDAOIndex;
begin
  result := Add;
  result.IndexName := AName;
end;

function TGDAOIndexes.GetOwner: TPersistent;
begin
   result:=FOwnertable;
end;

function TGDAOIndexes.IndexOfIId(AIID: Integer): integer;
begin
   for result:=0 to Count-1 do
      if AIID=Items[result].IID then
         exit;
   result:=-1;
end;

{ TGDAOIndex }

procedure TGDAOIndex.Assign(Source: TPersistent);
begin
   IndexName  := TGDAOIndex(Source).IndexName;
   IndexType  := TGDAOIndex(Source).IndexType;
   IndexOrder := TGDAOIndex(Source).IndexOrder;
   IID        := TGDAOIndex(Source).IID;
   IFields    := TGDAOIndex(Source).IFields;
end;

constructor TGDAOIndex.Create(ACollection:TCollection);
begin
   inherited Create( ACollection );
   FIFields := TGDAOIFields.Create(self);
end;

constructor TGDAOIndex.CreateFromTable(AOwnerTable: TGDAOTable);
begin
  FOwnerTable := AOwnerTable;
  Create(nil);
end;

destructor TGDAOIndex.Destroy;
var
  r: integer;
begin
  // index destroy: update relationships
  with OwnerDatabase.Relationships do
  begin
    for r := 0 to Count-1 do
      if Items[r].ParentIndex = Self then
        Items[r].ParentIndex := nil;
  end;

  FIFields.Free;
  inherited;
end;

function TGDAOIndex.TableName: string;
begin
  if OwnerTable <> nil then
    result := OwnerTable.TableName
  else
    result := '';
end; 

function TGDAOIndex.OwnerDatabase:TGDD;
begin
  result := OwnerTable.OwnerDatabase;
end;

procedure TGDAOIndex.SetIFields(const Value: TGDAOIFields );
begin
   FIFields.Assign(Value);
end;

function TGDAOIndex.OwnerTable: TGDAOTable;
begin
  if (Collection <> nil) then
    result := TGDAOIndexes(Collection).FOwnerTable
  else
  if FOwnerTable <> nil then
    result := FOwnerTable
  else
    result := nil;
end;

function TGDAOIndex.HasField(AField: TGDAOField): boolean;
var
  i: integer;
begin
  if AField <> nil then
    with IFields do
      for i := 0 to Count - 1 do
         if Field[i] = AField then
         begin
            result:=true;
            exit;
         end;
   result := false;
end;

function TGDAOIndex.GetDisplayName: string;
begin
   if IndexName = '' then
      Result := inherited GetDisplayName
   else
      result:=IndexName;
end;

function TGDAOIndex.GetIsPrimary: boolean;
begin
  result := FOwnerTable <> nil;
end;

{ TGDAOFields }

constructor TGDAOFields.Create( AOwnerTable:TGDAOTable );
begin
   inherited Create( TGDAOField );
   FOwnerTable:=AOwnerTable;
end;

function TGDAOFields.GetItem(i:integer):TGDAOField;
begin
   result:=TGDAOField( inherited Items[i] );
end;

function TGDAOFields.Add: TGDAOField;
begin
  result := TGDAOField(inherited Add);
  result.FID := result.OwnerDatabase.GetNextFieldID;
end;

function TGDAOFields.Add( AFieldName:string; ADataType:TGDAODataType; ASize, ASize2:integer;ARequired:boolean ):TGDAOField;
begin
   result := Add;
   with result do
   begin
      FieldName:=AFieldName;
      Size:=ASize;
      Size2:=ASize2;
      Required:=ARequired;
      if ADataType <> nil then
      begin
        DataTypeName := ADataType.Name;
      end;
   end;
end;

procedure TGDAOFields.SetItem(i: integer; const Value: TGDAOField);
begin
   Items[i].Assign(Value);
end;

function TGDAOFields.IndexOf(AFieldName: string): integer;
begin
   for result:=0 to Count-1 do
      if CompareText(AFieldName,Items[result].FieldName)=0 then
         Exit;
   result:=-1;
end;

function TGDAOFields.GetOwner: TPersistent;
begin
   result:=FOwnerTable;
end;

function TGDAOFields.IndexOfFid(AFID: integer): integer;
begin
   for result:=0 to Count-1 do
      if AFID=Items[result].FID then
         exit;
   result:=-1;
end;

function TGDAOFields.FindByID(AFID: integer): TGDAOField;
var
  i: integer;
begin
   i := IndexOfFid(AFID);
   if i > -1 then
      result := Items[i]
   else
      result := nil;
end;

function TGDAOFields.FindbyName(AFieldName: String): TGDAOField;
var i : Integer;
begin
  Result := nil;
  for i := 0 to count-1 do
    if CompareText(AFieldName, Items[i].FieldName) = 0 then
    begin
      Result := Items[i];
      break;
    end;
end;

{ TGDAOField }

procedure TGDAOField.Assign(Source: TPersistent);
begin
  FieldName          := TGDAOField(Source).FieldName;
  DataTypeName       := TGDAOField(Source).DataTypeName;
  Size               := TGDAOField(Source).Size;
  Size2              := TGDAOField(Source).Size2;
  Description        := TGDAOField(Source).Description;
  DefaultValue       := TGDAOField(Source).DefaultValue;
  Required           := TGDAOField(Source).Required;
  FID                := TGDAOField(Source).FID;
  DomainName         := TGDAOField(Source).DomainName;
  DefaultValueSpecific := TGDAOField(Source).DefaultValueSpecific;
  Expression := TGDAOField(Source).Expression;
  ConstraintExpr     := TGDAOField(Source).ConstraintExpr;
  ConstraintName     := TGDAOField(Source).ConstraintName;
  ConstraintExprSpecific := TGDAOField(Source).ConstraintExprSpecific;
  ConstraintDefaultName := TGDAOField(Source).ConstraintDefaultName;
  ConstraintNotNullName := TGDAOField(Source).ConstraintNotNullName;
  SeedValue          := TGDAOField(Source).SeedValue;
  IncrementValue     := TGDAOField(Source).IncrementValue;
  GeneratedByRelationship := TGDAOField(Source).GeneratedByRelationship;
  FieldCaption := TGDAOField(Source).FieldCaption;
  Restriction := TGDAOField(Source).Restriction;
end;

constructor TGDAOField.Create(ACollection: TCollection);
begin
   inherited Create(ACollection);
   FRestriction := frNone;
   FSeedValue := 0;
   FIncrementValue := 1;
end;

function TGDAOField.OwnerDatabase: TGDD;
begin
  result := nil;
  if OwnerTable <> nil then
    result := OwnerTable.OwnerDatabase;
end;

function TGDAOField.OwnerTable: TGDAOTable;
begin
  result := nil;
  if (Collection <> nil) and (Collection is TGDAOFields) then
    result := TGDAOFields(Collection).FOwnerTable;
end;

destructor TGDAOField.Destroy;
begin
  if OwnerTable <> nil then
    OwnerTable.FieldDestroyed(Self);
  inherited;
end;

procedure TGDAOField.SetDataType(const Value: TGDAODataType);
begin
  if Value <> FDataType then
  begin
    FDataType := Value;

    {Apply default size values, but only if size is 0}
    if Assigned(FDataType) then
    begin
      if not FDataType.SizeIsRequired then
        Size := 0
      else
        if Size = 0 then
          Size := FDataType.DefaultSize;

      if not FDataType.Size2IsRequired then
        Size2 := 0
      else
        if Size2 = 0 then
          Size2 := FDataType.DefaultSize2;
    end;
  end;
end;

procedure TGDAOField.SetDataTypeName(const Value: String);
var
  obj: TGDAODataType;
begin
  if OwnerDatabase <> nil then
  begin
    obj := OwnerDatabase.DataTypes.FindByName(Value);
    if Assigned(obj) then
    begin
      if obj <> FDataType then
        DataType := obj;
    end
    else
      raise Exception.Create('Cannot find datatype: ' + Value);
  end else
    raise Exception.Create('Cannot find datatype: ' + Value);
end;

procedure TGDAOField.SetDefaultValue(const Value: string);
begin
   FDefaultValue := Value;
end;

function TGDAOField.GetDisplayName: string;
begin
   if FieldName = '' then
      Result := inherited GetDisplayName
   else
      result:=FieldName;
end;

function TGDAOField.GetDataTypeName: String;
begin
  Result := '';
  if Assigned(DataType) then
    Result := DataType.Name;
end;

function TGDAOField.GetDomainName: String;
begin
  Result := '';
  if Assigned(FDomain) then
    Result := FDomain.Name
  else
    result := FDomainName;
end;

function TGDAOField.GetFieldCaption: string;
begin
  if FFieldCaption > '' then
    result := FFieldCaption
  else
    result := FFieldName;
end;

procedure TGDAOField.SetDomain(const Value: TGDAODomain);
begin
  if FDomain <> Value then
  begin
    if Value = nil then
      AssignDomainInternalFields(FDomain);
    FDomain := Value;
  end;
end;

procedure TGDAOField.SetDomainName(const AValue: String);
begin
  if OwnerDatabase <> nil then
    Domain := OwnerDatabase.Domains.FindByName(AValue);
end;

function TGDAOField.GetDefaultValue: String;
begin
  if Assigned(FDomain) and not DefaultValueSpecific then
    Result := FDomain.DefaultValue
  else
    Result := FDefaultValue;
end;

function TGDAOField.GetSize: Integer;
begin
  if FDomain <> nil then
    Result := FDomain.Size
  else
    result := FSize;
end;

function TGDAOField.GetSize2: Integer;
begin
  if FDomain <> nil then
    Result := FDomain.Size2
  else
    result := FSize2;
end;

function TGDAOField.GetDataType: TGDAODataType;
begin
  if FDomain <> nil then
    Result := FDomain.DataType
  else
    Result := FDataType;
end;

procedure TGDAOField.AssignDomainInternalFields(ADomain: TGDAODomain);
begin
  if Assigned(ADomain) then
  begin
    FDataType := ADomain.DataType;
    FSize := ADomain.Size;
    FSize2 := ADomain.Size2;
    FSeedValue := ADomain.SeedValue;
    FIncrementValue := ADomain.IncrementValue;
    if not DefaultValueSpecific then
      FDefaultValue := ADomain.DefaultValue;
    FDefaultValueSpecific := false;
  end;
end;

function TGDAOField.GetConstraintExpr: String;
begin
  if Assigned(FDomain) and not ConstraintExprSpecific then
    Result := FDomain.ConstraintExpr
  else
    result := FConstraintExpr;
end;

function TGDAOField.IsForeignKey(AParents: TRelationshipList): boolean;
var
  i: integer;
begin
  result := False;
  if AParents <> nil then
    AParents.Clear;

  with TGDAOFields(Collection).OwnerTable.OwnerDatabase.Relationships do
  begin
    for i := 0 to Count - 1 do
      if Items[i].FieldLinks.IndexOfChildField(Self) >= 0 then
      begin
        result := True;
        if AParents <> nil then
          AParents.Add(Items[i])
        else
          break;
      end;
  end;
end;

function TGDAOField.IsRelationshipParentKey: boolean;
var
  i: integer;
begin
  result := False;

  with TGDAOFields(Collection).OwnerTable.OwnerDatabase.Relationships do
  begin
    for i := 0 to Count - 1 do
      if Items[i].FieldLinks.IndexOfParentField(Self) >= 0 then
      begin
        result := True;
        break;
      end;
  end;
end;

function TGDAOField.IsInRelationship: boolean;
begin
  result := IsForeignKey or IsRelationshipParentKey;
end;

procedure TGDAOField.SetFieldCaption(const Value: string);
begin
  FFieldCaption := Value;
end;

procedure TGDAOField.SetFieldName(const AName: String);
var idx, i : Integer;
begin
  if FFieldName <> AName then
  begin

    { Check if the field is parent key of a relationship. If yes, and the child field
     was generated automatically, and child field had same name of parent field,
     then rename child field }
    with OwnerDatabase.Relationships do
      for i := 0 to Count-1 do
      begin
        idx := Items[i].FieldLinks.IndexOfParentField(Self);
        if idx > -1 then
        begin
          if (Items[i].FieldLinks[idx].ChildField <> nil) and Items[i].FieldLinks[idx].ChildField.GeneratedByRelationship
           and (Items[i].FieldLinks[idx].ChildField.FieldName = FFieldName) then
            Items[i].FieldLinks[idx].ChildField.FieldName := AName;
        end;
      end;

    FFieldName := AName;
  end;
end;

function TGDAOField.StoreFieldCaption: Boolean;
begin
  result := (FFieldCaption > '') and (FFieldCaption <> FFieldName);
end;

function TGDAOField.GetIncrementValue: Integer;
begin
  if FDomain <> nil then
    Result := FDomain.IncrementValue
  else
    result := FIncrementValue;
end;

function TGDAOField.GetInPrimaryKey: Boolean;
begin
  result := (OwnerTable <> nil) and (OwnerTable.PrimaryKeyIndex <> nil) and
    (OwnerTable.PrimaryKeyIndex.HasField(Self));
end;

function TGDAOField.GetRestriction: TFieldRestriction;
begin
  result := FRestriction;
end;

function TGDAOField.GetSeedValue: Integer;
begin
  if FDomain <> nil then
    Result := FDomain.SeedValue
  else
    result := FSeedValue;
end;

{ TGDAOIFields }

function TGDAOIFields.Add(AFieldName: string): TGDAOIField;
begin
   result := Add;
   result.FieldName := AFieldName;
end;

function TGDAOIFields.Add: TGDAOIField;
begin
  result := TGDAOIField(inherited Add);
end;

function TGDAOIFields.Add(AField: TGDAOField; AOrder: TIndexFieldOrder): TGDAOIField;
begin
   result := Add;
   with result do
   begin
    Field := AField;
    FieldOrder := AOrder;
   end;
end;

constructor TGDAOIFields.Create(AOwnerIndex: TGDAOIndex);
begin
   inherited Create( TGDAOIField );
   FOwnerIndex:=AOwnerIndex;
end;

function TGDAOIFields.FindByField(AField: TGDAOField): TGDAOIField;
var
  c: integer;
begin
  for c := 0 to Count - 1 do
    if Items[c].Field = AField then
    begin
      result := Items[c];
      exit;
    end;
  result := nil;
end;

function TGDAOIFields.GetField(i: integer): TGDAOField;
begin
   result:=TGDAOIField( inherited Items[i] ).Field;
end;

function TGDAOIFields.GetItem(i: integer): TGDAOIField;
begin
   result:=TGDAOIField( inherited Items[i] );
end;

function TGDAOIFields.GetOwner: TPersistent;
begin
   result:=FOwnerIndex;
end;

function TGDAOIFields.IndexOf(AFieldName: string): integer;
begin
   for result:=0 to Count-1 do
      if CompareText(Items[result].FieldNAme,AFieldName)=0 then
         Exit;
   result:=-1;
end;

procedure TGDAOIFields.RemoveField(AField: TGDAOField);
var
  IField: TGDAOIField;
begin
  IField := FindByField(AField);
  if IField <> nil then
    IField.Free;
end;

procedure TGDAOIFields.SetField(i: integer; const Value: TGDAOField);
begin
   TGDAOIField( inherited Items[i] ).Field:=Value;
end;

procedure TGDAOIFields.SetItem(i: integer; const Value: TGDAOIField);
begin
   TGDAOIField(inherited Items[i]).Assign( Value );
end;

{ TGDAOIField }

function TGDAOIField.GetFieldIndex: integer;
begin
   if Assigned(FField) then
      result := FField.Index
   else
      result := -1;
end;

function TGDAOIField.GetFieldName: string;
begin
   if Assigned(FField) then
      result:=FField.FieldName
   else
      result:='';
end;

procedure TGDAOIField.Assign(Source: TPersistent);
begin
  FieldIndex := TGDAOIField(Source).FieldIndex;
  FieldOrder := TGDAOIField(Source).FieldOrder;
  KeyByRelationship := TGDAOIField(Source).KeyByRelationship;
end;

destructor TGDAOIField.Destroy;
begin
  // index destroy: update relationships
  SetField(nil);

  inherited;
end;

function TGDAOIField.GetDisplayName: string;
begin
   if not Assigned(FField) then
      Result := inherited GetDisplayName
   else
      result:=FField.Fieldname;
end;

function TGDAOIField.OwnerIndex: TGDAOIndex;
begin
  result := TGDAOIFields(Collection).FOwnerIndex;
end;

function TGDAOIField.OwnerTable: TGDAOTable;
begin
   result:=TGDAOIFields(Collection).FOwnerIndex.OwnerTable;
end;

procedure TGDAOIField.SetField(const Value: TGDAOField);
begin
  FField := Value;
end;

procedure TGDAOIField.SetFieldIndex(const Value: integer);
begin
   {Must set the field property, not FField field}
   if Value = -1 then
      Field := nil
   else
      Field := OwnerTable.Fields[Value];
end;

procedure TGDAOIField.SetFieldName(const Value: string);
begin
   {Must set the field property, not FField field}
   Field := OwnerTable.FieldByName(Value);
end;

{ TGDAOConstraints }

constructor TGDAOConstraints.Create(AOwnerTable:TGDAOTable);
begin
   inherited Create(TGDAOConstraint);
   FOwnerTable:=AOwnerTable;
end;

function TGDAOConstraints.FindByName(AConstraintName: string): TGDAOConstraint;
var
  i: integer;
begin
  i := IndexOf(AConstraintName);
  if i >= 0 then
    result := items[i]
  else
    result := nil;
end;

function TGDAOConstraints.Add: TGDAOConstraint;
begin
  result:=TGDAOCOnstraint( inherited Add );
  result.CID := TGDAOTables(TGDAOTable(GetOwner).Collection).OwnerDatabase.GetNextConstraintID;
end;

function TGDAOConstraints.AddConstraint(AName, AExpression: string): TGDAOConstraint;
begin
   Result := Add;
   with result do
   begin
      ConstraintName := AName;
      Expression:=AExpression;
   end;
end;

function TGDAOConstraints.GetItem(i:integer): TGDAOCOnstraint;
begin
   result:=TGDAOConstraint( inherited Items[i] );
end;

procedure TGDAOConstraints.SetItem(i:integer; const Value: TGDAOCOnstraint);
begin
   Items[i].Assign(Value);
end;

function TGDAOConstraints.GetOwner: TPersistent;
begin
   result:=FOwnerTable;
end;

function TGDAOConstraints.IndexOf(AConstraintName: string): integer;
begin
  for Result := 0 to Count-1 do
    if CompareText(Items[Result].ConstraintName, AConstraintName) = -0 then
      exit;
  Result := -1;
end;

function TGDAOConstraints.IndexOfCID(ACID: Integer): integer;
begin
   for result:=0 to Count-1 do
      if ACID=Items[result].CID then
         exit;
   result:=-1;
end;

{ TGDAOTriggers }

function TGDAOTriggers.Add: TGDAOTrigger;
begin
   result:=TGDAOTrigger( inherited Add );
end;

constructor TGDAOTriggers.Create(AOwnerTable: TGDAOTable);
begin
   FOwnerTable:=AOwnerTable;
   inherited Create(TGDAOTrigger);
end;

function TGDAOTriggers.FindByName(AName: string): TGDAOTrigger;
var
  i: integer;
begin
  i := IndexOf(AName);
  if i >= 0 then
    result := Items[i]
  else
    result := nil;
end;

function TGDAOTriggers.GetItem(i: integer): TGDAOTrigger;
begin
   result:=TGDAOTrigger( inherited Items[i] );
end;

function TGDAOTriggers.IndexOf(AName: string): integer;
begin
   for result:=0 to Count-1 do
      if CompareText(AName,Items[result].Name)=0 then
         Exit;
   result:=-1;
end;

procedure TGDAOTriggers.SetItem(i: integer; const Value: TGDAOTrigger);
begin
   Items[i].Assign(Value);
end;

function TGDAOTriggers.Add(AName: string; AImplementation: string): TGDAOTrigger;
begin
   result:=Add;
   with result do
   begin
      Name:=AName;
      ImplementationCode:=AImplementation;
   end;
end;

function TGDAOTriggers.GetOwner: TPersistent;
begin
   result:=FOwnerTable; 
end;

{ TGDAOTrigger }

procedure TGDAOTrigger.Assign(Source: TPersistent);
begin                                         
   Name := TGDAOTrigger(Source).Name;
   Description := TGDAOTrigger(Source).Description;
   ImplementationCode := TGDAOTrigger(Source).ImplementationCode;
end;

constructor TGDAOTrigger.Create(ACollection: TCollection);
begin
   inherited Create(ACollection);
end;

function TGDAOTrigger.GetDisplayName: string;
begin
   if Name = '' then
      Result := inherited GetDisplayName
   else
      result := Name;
end;

function TGDAOTrigger.GetTable: TGDAOTable;
begin
   if TGDAOTriggers(Collection).OwnerTable<>nil then
      result:=TGDAOTriggers(Collection).OwnerTable
   else
      result:=nil;
end;

function TGDAOTrigger.GetTableName: string;
begin
   if TGDAOTriggers(Collection).OwnerTable<>nil then
      result := TGDAOTriggers(Collection).OwnerTable.TableName
   else
      result:='';
end;

function TGDAOTrigger.OwnerDatabase: TGDD;
begin
   result := TGDAOTriggers(Collection).OwnerTable.OwnerDatabase;
end;

function TGDAOTrigger.OwnerTable: TGDAOTable;
begin
   result:=TGDAOTriggers(Collection).FOwnerTable;
end;

{ TGDAODataTypes }

function TGDAODataTypes.Add: TGDAODataType;
begin
  Result := TGDAODataType(inherited Add);
end;

constructor TGDAODataTypes.Create(AOwnerDatabase: TGDD);
begin
  FOwnerDatabase := AOwnerDatabase;
  inherited Create(TGDAODataType);
end;

function TGDAODataTypes.GetItem(i: integer): TGDAODataType;
begin
  Result := TGDAODataType(inherited Items[i]);
end;

function TGDAODataTypes.GetOwner: TPersistent;
begin
  Result := FOwnerDatabase;
end;

function TGDAODataTypes.FindByName(AName: String): TGDAODataType;
var i : Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(Items[i].Name, AName) = 0 then
    begin
      Result := Items[i];
      break;
    end;
end;

procedure TGDAODataTypes.SetItem(i: integer; const Value: TGDAODataType);
begin
   Items[i].Assign(Value);
end;

function TGDAODataTypes.Add( AName, APhysical: String; FSizeReq, FSize2Req: Boolean;
                             ANativeDataType: TNativeDataType; ANativeSubType: TNativeSubType;
                             ACounter: Boolean = false; ASeed: Boolean = false;
                             AIncrement: Boolean = False): TGDAODataType;
begin
  Result := Add;
  with Result do
  begin
    Name := AName;
    Physical := APhysical;
    SizeIsRequired := FSizeReq;
    Size2IsRequired := FSize2Req;
    Counter := ACounter;
    SeedIsRequired := ASeed;
    IncrementIsRequired := AIncrement;
    NativeDataType := ANativeDataType;
    NativeSubType := ANativeSubType;
  end;
end;

{ TGDAODomains }

function TGDAODomains.Add: TGDAODomain;
begin
  Result := TGDAODomain(inherited Add);
end;

constructor TGDAODomains.Create(AOwnerDatabase: TGDD);
begin
  FOwnerDatabase := AOwnerDatabase;
  inherited Create(TGDAODomain);
end;

function TGDAODomains.FindByName(AName: String): TGDAODomain;
var i : Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(Items[i].Name, AName) = 0 then
    begin
      Result := Items[i];
      break;
    end;
end;

function TGDAODomains.GetItem(i: integer): TGDAODomain;
begin
  Result := TGDAODomain(inherited Items[i]);
end;

function TGDAODomains.GetOwner: TPersistent;
begin
  Result := FOwnerDatabase;
end;

function TGDAODomains.IndexOf(AName: String): Integer;
begin
  for Result := 0 to count - 1 do
    if CompareText(Items[Result].Name, AName) = 0 then
      Exit; 

  Result := -1;
end;

procedure TGDAODomains.SetItem(i: integer; const Value: TGDAODomain);
begin
   Items[i].Assign(Value);
end;

{ TGDAODomain }

procedure TGDAODomain.Assign(Source: TPersistent);
begin
  Name           := TGDAODomain(Source).Name;
  ConstraintExpr := TGDAODomain(Source).ConstraintExpr;
  Size           := TGDAODomain(Source).Size;
  Size2          := TGDAODomain(Source).Size2;
  DefaultValue   := TGDAODomain(Source).DefaultValue;
  DataTypeName   := TGDAODomain(Source).DataTypeName;
  Information    := TGDAODomain(Source).Information;
  SeedValue      := TGDAODomain(Source).SeedValue;
  IncrementValue := TGDAODomain(Source).IncrementValue;
  InDatabase     := TGDAODomain(Source).InDatabase;
end;

constructor TGDAODomain.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSeedValue := 0;
  FIncrementValue := 1;
end;

destructor TGDAODomain.Destroy;
var
  AFldList: TObjectList;
  c: integer;
begin
  AFldList := TObjectList.Create(false);
  try
    FillUsedFields(AFldList);
    for c := 0 to AFldList.Count - 1 do
      TGDAOField(AFldList[c]).Domain := nil;
  finally
    AFldList.Free;
  end;
  inherited;
end;

function TGDAODomain.GetDataTypeName: String;
begin
  Result := '';
  if Assigned(FDataType) then
    Result := FDatatype.Name;
end;

function TGDAODomain.IsBeingUsed: boolean;
var
  AFldList: TObjectList;
begin
  AFldList := TObjectList.Create(false);
  try
    FillUsedFields(AFldList);
    result := AFldList.Count > 0;
  finally
    AFldList.Free;
  end;
end;

function TGDAODomain.IsInRelationship: boolean;
var
  AFldList: TObjectList;
  c: Integer;
begin
  result := false;
  AFldList := TObjectList.Create(false);
  try
    FillUsedFields(AFldList);
    for c := 0 to AFldList.Count - 1 do
      if TGDAOField(AFldList[c]).IsInRelationship then
      begin
        result := true;
        break;
      end;
  finally
    AFldList.Free;
  end;
end;

procedure TGDAODomain.FillUsedFields(AFieldList: TObjectList);
var
  i, j: integer;
begin
  AFieldList.Clear;
  if (OwnerDatabase <> nil) then
    for i := 0 to OwnerDatabase.Tables.Count - 1 do
      for j := 0 to OwnerDatabase.Tables[i].Fields.count - 1 do
        if OwnerDatabase.Tables[i].Fields[j].Domain = Self then
          AFieldList.Add(OwnerDatabase.Tables[i].Fields[j]);
end;

function TGDAODomain.OwnerDatabase: TGDD;
begin
  if (Collection is TGDAODomains) then
    result := TGDAODomains(Collection).FOwnerDatabase
  else
    result := nil;
end;

procedure TGDAODomain.SetDataType(const Value: TGDAODataType);
begin
  if Value <> FDataType then
  begin
    FDataType := Value;

    {Apply default size values, but only if size is 0}
    if Assigned(FDataType) then
    begin
      if not FDataType.SizeIsRequired then
        Size := 0
      else
        if Size = 0 then
          Size := FDataType.DefaultSize;

      if not FDataType.Size2IsRequired then
        Size2 := 0
      else
        if Size2 = 0 then
          Size2 := FDataType.DefaultSize2;
    end;
  end;
end;

procedure TGDAODomain.SetDataTypeName(const Value: String);
var
  obj: TGDAODataType;
begin
  if OwnerDatabase <> nil then
  begin
    obj := OwnerDatabase.DataTypes.FindByName(Value);
    if Assigned(obj) then
    begin
      if obj <> DataType then
        DataType := obj;
    end
    else
      raise Exception.Create('Cannot find datatype: ' + Value);
  end else
    raise Exception.Create('Cannot find datatype: ' + Value);
end;

{ TGDAODataType }

procedure TGDAODataType.Assign(Source: TPersistent);
begin
  Name := TGDAODataType(Source).Name;
  Physical := TGDAODataType(Source).Physical;
  SizeIsRequired := TGDAODataType(Source).SizeIsRequired;
  Size2IsRequired := TGDAODataType(Source).Size2IsRequired;
  Counter := TGDAODataType(Source).Counter;
  SeedIsRequired := TGDAODataType(Source).SeedIsRequired;
  IncrementIsRequired := TGDAODataType(Source).IncrementIsRequired;
  ForeignDataTypeName := TGDAODataType(Source).ForeignDataTypeName;
  NativeDataType := TGDAODataType(Source).NativeDataType;
  NativeSubType := TGDAODataType(Source).NativeSubType;
  Computed := TGDAODataType(Source).Computed;
  CheckSize := TGDAODataType(Source).CheckSize;
  MinSize := TGDAODataType(Source).MinSize;
  MaxSize := TGDAODataType(Source).MaxSize;
  DefaultSize := TGDAODataType(Source).DefaultSize;
  DefaultSize2 := TGDAODataType(Source).DefaultSize2;
end;

function TGDAODataType.CheckSizeStored: boolean;
begin
  result := FCheckSize;
end;

function TGDAODataType.GetForeignDataTypeName: String;
begin
  Result := '';
  if (FForeignDataType <> nil) and (Assigned(FForeignDataType)) then
    Result := FForeignDataType.Name;
end;

procedure TGDAODataType.SetForeignDataTypeName(const AName: String);
begin
  FForeignDataType := TGDAODataTypes(Collection).FindByName(AName);
end;

procedure TGDAODataType.SetSizeSettings(ADefaultSize, ADefaultSize2: integer;
  ACheckSize: boolean; AMinSize, AMaxSize: integer);
begin
  FDefaultSize := ADefaultSize;
  FDefaultSize2 := ADefaultSize2;
  FCheckSize := ACheckSize;
  FMinSize := AMinSize;
  FMaxSize := AMaxSize;
end;

{ TGDAOCategories }

function TGDAOCategories.Add(AType: TGDAOCategoryType; ANameS, ANameP: String;
  ACreate, ADrop: String): TGDAOCategory;
begin
  Result := TGDAOCategory(inherited Add);
  with Result do
  begin
    CategoryType := AType;
    CategoryNameP := ANameP;
    CategoryNameS := ANameS;
    CreateTemplate := ACreate;
    DropTemplate   := ADrop;
  end;
end;

constructor TGDAOCategories.Create(AOwnerDatabase: TGDD);
begin
  FOwnerDatabase := AOwnerDatabase;
  inherited Create(TGDAOCategory);
end;

function TGDAOCategories._FindByNameP(AName: String): TGDAOCategory;
var i : Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
    if CompareText(Items[i].CategoryNameP, AName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TGDAOCategories.FindByType(AType: TGDAOCategoryType): TGDAOCategory;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].CategoryType = AType then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TGDAOCategories.GetItem(i: integer): TGDAOCategory;
begin
  Result := TGDAOCategory(inherited Items[i]);
end;

function TGDAOCategories.GetOwner: TPersistent;
begin
  Result := FOwnerDatabase;
end;

procedure TGDAOCategories.SetItem(i: integer; const Value: TGDAOCategory);
begin
   Items[i].Assign(Value);
end;

{ TGDAOCategory }

procedure TGDAOCategory.Assign(Source: TPersistent);
begin
  CategoryType := TGDAOCAtegory(Source).CategoryType;
  Objects        := TGDAOCategory(Source).Objects;
  CategoryNameS   := TGDAOCategory(Source).CategoryNameS;
  CategoryNameP   := TGDAOCategory(Source).CategoryNameP;
  CreateTemplate := TGDAOCategory(Source).CreateTemplate;
  DropTemplate   := TGDAOCategory(Source).DropTemplate;
end;

constructor TGDAOCategory.Create(Collection: TCollection);
begin
  inherited;
  FPropDefs := TGDAOPropDefs.Create(TGDAOPropDef);
  FObjects := TGDAOObjects.Create(Self);
end;

destructor TGDAOCategory.Destroy;
begin
  FPropDefs.Free;
  FObjects.Free;
  inherited;
end;

procedure TGDAOCategory.SetObjects(const Value: TGDAOObjects);
begin
   FObjects.Assign(Value);
end;

{ TGDAOObjects }

function TGDAOObjects.Add(AName: String): TGDAOObject;
begin
  Result := TGDAOObject(inherited Add);
  Result.ObjectName := AName;
end;

constructor TGDAOObjects.Create(AOwnerCategory: TGDAOCategory);
begin
  FOwnerCategory := AOwnerCategory;
  FOwnerDatabase := TGDD(TGDAOCategories(FOwnerCategory.Collection).GetOwner);
  inherited Create(TGDAOObject);
end;

function TGDAOObjects.FindByName(AName: String): TGDAOObject;
var i : Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
    if CompareText(AName, Items[i].ObjectName)=0 then
    begin
      Result := Items[i];
      break;
    end;
end;

function TGDAOObjects.GetItem(i: integer): TGDAOObject;
begin
  Result := TGDAOObject(inherited Items[i]);
end;

function TGDAOObjects.GetOwner: TPersistent;
begin
  Result := FOwnerDatabase;
end;

function TGDAOObjects.IndexOf(AName: String): Integer;
begin
  for Result := 0 to Count-1 do
    if CompareText(Items[Result].ObjectName, AName) = 0 then
      exit;
  Result := -1;
end;

procedure TGDAOObjects.SetItem(i: integer; const Value: TGDAOObject);
begin
   Items[i].Assign(Value);
end;

{ TGDAOObject }

procedure TGDAOObject.Assign(Source: TPersistent);
begin
  ObjectName  := TGDAOObject(Source).ObjectName;
  Description := TGDAOObject(Source).Description;
  CreateImplementation := TGDAOObject(Source).CreateImplementation;
  DropImplementation   := TGDAOObject(Source).DropImplementation;
  CustomProps := TGDAOObject(Source).CustomProps;
  Restriction := TGDAOObject(Source).Restriction;
end;

constructor TGDAOObject.Create(Collection: TCollection);
begin
  inherited;
  FRestriction := orNone;
  FCustomProps := TStringList.Create;
end;

destructor TGDAOObject.Destroy;
begin
  FCustomProps.Free;
  inherited;
end;

function TGDAOObject.GetDropImplementation: String;
begin
  if OwnerCategory <> nil then
    result := OwnerCategory.DropTemplate
  else
    result := '';
end;

function TGDAOObject.GetReadOnly: boolean;
begin
  result := Restriction = orReadOnly;
end;

function TGDAOObject.GetRestriction: TObjectRestriction;
begin
  result := FRestriction;
end;

function TGDAOObject.GetVisible: boolean;
begin
  result := Restriction <> orHidden;
end;

function TGDAOObject.OwnerCategory: TGDAOCategory;
begin
  result := TGDAOObjects(Collection).OwnerCategory;
end;

function TGDAOObject.ReadProp(APropName: string): variant;
begin
  if OwnerCategory <> nil then
    result := OwnerCategory.PropDefs.ReadProp(APropName, FCustomProps)
  else
    result := NULL;
end;

procedure TGDAOObject.SetCustomProps(const Value: TStrings);
begin
  FCustomProps.Assign(Value);
end;

procedure TGDAOObject.SetReadOnly(const Value: boolean);
begin
  if Restriction <> orHidden then
    if Value then
      Restriction := orReadOnly
    else
      Restriction := orNone;
end;

procedure TGDAOObject.SetVisible(const Value: boolean);
begin
  if not Value then
    Restriction := orHidden
  else if Restriction <> orReadOnly then
    Restriction := orNone;
end;

procedure TGDAOObject.WriteProp(APropName: string; AValue: Variant);
begin
  if OwnerCategory <> nil then
    OwnerCategory.PropDefs.WriteProp(APropName, FCustomProps, AValue);
end;

{ TGDAOConstraint }

procedure TGDAOConstraint.Assign(Source: TPersistent);
begin
  ConstraintName := TGDAOConstraint(Source).ConstraintName;
  Expression     := TGDAOConstraint(Source).Expression;
  CID            := TGDAOConstraint(Source).CID;
end;

function TGDAOConstraint.GetDisplayName: string;
begin
  if ConstraintName = '' then
    result := inherited GetDisplayName
  else
    result := ConstraintName;
end;

function TGDAOConstraint.OwnerTable: TGDAOTable;
begin
  result := TGDAOConstraints(Collection).OwnerTable;
end;

{ TGDAORelationshipFieldLink }

procedure TGDAORelationshipFieldLink.Assign(Source: TPersistent);
begin
  ParentFieldName := TGDAORelationshipFieldLink(Source).ParentFieldName;
  ChildFieldName := TGDAORelationshipFieldLink(Source).ChildFieldName;
end;

destructor TGDAORelationshipFieldLink.Destroy;
begin
  {Restore all things to normal}
  ChildField := nil;
  inherited;
end;

function TGDAORelationshipFieldLink.GetChildField: TGDAOField;
begin
  result := FChildField;
end;

function TGDAORelationshipFieldLink.GetChildFieldName: string;
begin
  if Assigned(FChildField) then
    result := FChildField.FieldName
  else
    result := '';
end;

function TGDAORelationshipFieldLink.GetDisplayName: string;
begin
  result := Format('%s x %s', [ParentFieldName, ChildFieldName]);
end;

function TGDAORelationshipFieldLink.GetParentField: TGDAOField;
begin
  result := FParentField;
end;

function TGDAORelationshipFieldLink.GetParentFieldName: string;
begin
  if Assigned(FParentField) then
    result := FParentField.FieldName
  else
    result := '';
end;

function TGDAORelationshipFieldLink.OwnerRelationship: TGDAORelationship;
begin
  result := TGDAORelationshipFieldLinks(Collection).FRelationship;
end;

procedure TGDAORelationshipFieldLink.SetChildField(const Value: TGDAOField);
begin
  {XXXX SetChildField reduced}
  if Value <> FChildField then
  begin
    FChildField := Value;
  end;
end;

procedure TGDAORelationshipFieldLink.SetChildFieldName(const Value: string);
begin
  if Value <> ChildFieldName then
  begin
    if (OwnerRelationship <> nil) and (OwnerRelationship.ChildTable <> nil) then
      ChildField := OwnerRelationship.ChildTable.FieldByName(Value)
    else
      ChildField := nil;
  end;                  
end;

procedure TGDAORelationshipFieldLink.SetParentField(const Value: TGDAOField);
begin
  if Value <> FParentField then
  begin
    if (FParentField <> nil) and FParentField.GeneratedByRelationship then
      FParentField.Free;
    FParentField := Value;
  end;
end;

procedure TGDAORelationshipFieldLink.SetParentFieldName(const Value: string);
begin
  if Value <> ParentFieldName then
  begin
    if (OwnerRelationship <> nil) and (OwnerRelationship.ParentTable <> nil) then
      ParentField := OwnerRelationship.ParentTable.FieldByName(Value)
    else
      ParentField := nil;
  end;
end;

{ TGDAORelationshipFieldLinks }

function TGDAORelationshipFieldLinks.Add: TGDAORelationshipFieldLink;
begin
  result := TGDAORelationshipFieldLink(inherited Add);
end;

constructor TGDAORelationshipFieldLinks.Create(ARelationship: TGDAORelationship);
begin
  inherited Create(TGDAORelationshipFieldLink);
  FRelationship := ARelationship;
end;

function TGDAORelationshipFieldLinks.GetItem(i: integer): TGDAORelationshipFieldLink;
begin
  result := TGDAORelationshipFieldLink(inherited Items[i]);
end;

function TGDAORelationshipFieldLinks.GetOwner: TPersistent;
begin
  result := FRelationship;
end;

function TGDAORelationshipFieldLinks.IndexOfChildField(AField: TGDAOField): integer;
begin
  for result := 0 to Count - 1 do
    if Items[result].ChildField = AField then
      exit;
  result := -1;
end;

function TGDAORelationshipFieldLinks.IndexOfParentField(AField: TGDAOField): integer;
begin
  for result := 0 to Count - 1 do
    if Items[result].ParentField = AField then
      exit;
  result := -1;
end;

procedure TGDAORelationshipFieldLinks.SetItem(i: integer; const Value: TGDAORelationshipFieldLink);
begin
  Items[i].Assign(Value);
end;

{ TRelationshipList }

function TRelationshipList.GetItem(i: integer): TGDAORelationship;
begin
  result := TGDAORelationship(Items[i]);
end;

{ TGDAOPropDefs }

function TGDAOPropDefs.Add: TGDAOPropDef;
begin
  result := TGDAOPropDef(inherited Add);
end;

function TGDAOPropDefs.FindProp(APropName: string): TGDAOPropDef;
var
  c: integer;
begin
  result := nil;
  for c := 0 to Count - 1 do
    if SameText(APropName, Items[c].PropName) then
    begin
      result := Items[c];
      break;
    end;
end;

function TGDAOPropDefs.Add(APropName: string; AType: TGDAOPropDefType; ADefValue: Variant): TGDAOPropDef;
begin
  result := Add;
  result.PropName := APropName;
  result.DataType := AType;
  result.DefaultValue := ADefValue;
end;

function TGDAOPropDefs.GetItem(Index: integer): TGDAOPropDef;
begin
  result := TGDAOPropDef(inherited Items[Index]);
end;

procedure TGDAOPropDefs.GetPropParams(APropName: string; var AType: TGDAOPropDefType; var ADefValue: Variant);
var
  AProp: TGDAOPropDef;
begin
  {Set initial type and default value}
  AType := pdtString;
  ADefValue := NULL;

  {if the object has a property specified, then find the correct type
   and default value}
  AProp := FindProp(APropName);
  if AProp <> nil then
  begin
    AType := AProp.DataType;
    ADefValue := AProp.DefaultValue;
  end;
end;

function TGDAOPropDefs.ReadProp(APropName: string; APropValues: TStrings): Variant;
var
  AType: TGDAOPropDefType;
  ADefValue: Variant;
  i: integer;
  AStrValue: string;
begin
  {Get the type and default value of the property}
  GetPropParams(APropName, AType, ADefValue);

  {Set the default value as result}
  result := ADefValue;

  {Find the property. If found, then convert it and return the final value}
  i := APropValues.IndexOfName(APropName);
  if i >= 0 then
  begin
    AStrValue := APropValues.Values[APropName];

    {now return the property value}
    case AType of
      pdtInteger:
        result := StrToIntDef(AStrValue, ADefValue);
    else
      {default pdtString}
      result := AStrValue;
    end;
  end;
end;

function TGDAOPropDefs.WriteProp(APropName: string; APropValues: TStrings; AValue: Variant): Variant;
var
  AType: TGDAOPropDefType;
  ADefValue: Variant;
  AStrValue: string;
begin
  {Get the type and default value of the property}
  GetPropParams(APropName, AType, ADefValue);

  {Set the property value}
  case AType of
    pdtInteger:
      AStrValue := IntToStr(AValue);
    else {default pdtString}
      AStrValue := VarToStr(AValue);
  end;

  APropValues.Values[APropName] := AStrValue;
end;

end.

