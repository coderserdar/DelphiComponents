(*
 *   InstantObjects
 *   Core Classes
 *)

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is: Seleqt InstantObjects
 *
 * The Initial Developer of the Original Code is: Seleqt
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Carlo Barazzetta, Andrea Petrelli, Nando Dessena, Steven Mitchell,
 * Joao Morais, Cesar Coll, Uberto Barbini, David Taylor, Hanedi Salas,
 * Riceball Lee, David Moorhouse
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantPersistence;

{$IFDEF LINUX}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Graphics,
{$ENDIF}
{$IFDEF LINUX}
  QGraphics,
{$ENDIF}
{$IFDEF FPC}
  InstantFpcUtils,
{$ENDIF}
  Classes, Contnrs, SysUtils, DB, InstantClasses, InstantCommand, InstantConsts;

type
  TInstantWarningEvent = procedure (const Sender: TObject;
    const AWarningText: string) of object;

  TInstantMetadatas = class;

  TInstantMetadata = class(TInstantCollectionItem)
  private
    function GetCollection: TInstantMetadatas;
    procedure SetCollection(Value: TInstantMetadatas); {$IFDEF D6+}reintroduce;{$ENDIF}
  protected
    function InternalEquals(const Other: TInstantMetadata): Boolean; virtual;
  public
    function Equals(const Other: TInstantMetadata): Boolean;
    property Collection: TInstantMetadatas read GetCollection write SetCollection;
  end;

  TInstantMetadatas = class(TInstantOwnedCollection)
  private
    function GetItems(Index: Integer): TInstantMetadata;
    procedure SetItems(Index: Integer; const Value: TInstantMetadata);
  public
    function Find(const AName: string): TInstantMetadata;
    property Items[Index: Integer]: TInstantMetadata read GetItems write SetItems; default;
  end;

  TInstantClassMetadata = class;
  TInstantAttributeMetadatas = class;
  TInstantObject = class;
  TInstantObjectClass = class of TInstantObject;
  TInstantAttribute = class;
  TInstantAttributeClass = class of TInstantAttribute;
  TInstantObjectReferenceList = class;

  TInstantStorageKind = (skEmbedded, skExternal);
  TInstantAttributeType = (atUnknown, atInteger, atFloat, atCurrency, atBoolean, atString,
    atDateTime, atBlob, atMemo, atGraphic, atPart, atReference, atParts, atReferences);
  TInstantAttributeCategory = (acUnknown, acSimple, acElement, acContainer);

  TInstantGraphicFileFormat = (gffUnknown, gffBmp, gffTiff, gffJpeg, gffPng,
    gffDcx, gffPcx, gffEmf, gffGif);

  TInstantAttributeMetadata = class(TInstantMetadata)
  private
    FAttributeType: TInstantAttributeType;
    FDefaultValue: string;
    FDisplayWidth: Integer;
    FEditMask: string;
    FIsIndexed: Boolean;
    FIsRequired: Boolean;
    FObjectClassName: string;
    FSize: Integer;
    FStorageName: string;
    FValidChars: TChars;
    FStorageKind: TInstantStorageKind;
    FExternalStorageName: string;
    function GetAttributeClass: TInstantAttributeClass;
    function GetAttributeClassName: string;
    function GetAttributeTypeName: string;
    function GetCategory: TInstantAttributeCategory;
    function GetClassMetadata: TInstantClassMetadata;
    function GetClassMetadataName: string;
    function GetCollection: TInstantAttributeMetadatas;
    function GetFieldName: string;
    function GetHasValidChars: Boolean;
    function GetIsDefault: Boolean;
    function GetObjectClass: TInstantObjectClass;
    function GetObjectClassMetadata: TInstantClassMetadata;
    function GetTableName: string;
    function GetValidChars: TChars;
    function GetValidCharsString: string;
    procedure SetAttributeClass(AClass: TInstantAttributeClass);
    procedure SetAttributeClassName(const Value: string);
    procedure SetAttributeTypeName(const Value: string);
    procedure SetCollection(Value: TInstantAttributeMetadatas);
    procedure SetFieldName(const Value: string);
    procedure SetIsDefault(const Value: Boolean);
    procedure SetValidCharsString(const Value: string);
  public
    function CreateAttribute(AObject: TInstantObject): TInstantAttribute;
    procedure Assign(Source: TPersistent); override;
    procedure CheckAttributeClass(AClass: TInstantAttributeClass);
    procedure CheckCategory(ACategory: TInstantAttributeCategory);
    procedure CheckIsIndexed;
    function IsAttributeClass(AClass: TInstantAttributeClass): Boolean;
    property AttributeClass: TInstantAttributeClass read GetAttributeClass write SetAttributeClass;
    property AttributeClassName: string read GetAttributeClassName write SetAttributeClassName;
    property Category: TInstantAttributeCategory read GetCategory;
    property ClassMetadataName: string read GetClassMetadataName;
    property Collection: TInstantAttributeMetadatas read GetCollection write SetCollection;
    property IsDefault: Boolean read GetIsDefault write SetIsDefault;
    property ObjectClass: TInstantObjectClass read GetObjectClass;
    property ObjectClassMetadata: TInstantClassMetadata read GetObjectClassMetadata;
    property FieldName: string read GetFieldName write SetFieldName;
    property HasValidChars: Boolean read GetHasValidChars;
    property TableName: string read GetTableName;
    property ValidChars: TChars read GetValidChars write FValidChars;
  published
    property AttributeType: TInstantAttributeType read FAttributeType write FAttributeType default atUnknown;
    property AttributeTypeName: string read GetAttributeTypeName write SetAttributeTypeName stored False;
    property ClassMetadata: TInstantClassMetadata read GetClassMetadata;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
    property DisplayWidth: Integer read FDisplayWidth write FDisplayWidth default 0;
    property EditMask: string read FEditMask write FEditMask;
    property ExternalStorageName: string read FExternalStorageName write FExternalStorageName;
    property StorageKind: TInstantStorageKind read FStorageKind write FStorageKind default skEmbedded;
    property IsIndexed: Boolean read FIsIndexed write FIsIndexed;
    property IsRequired: Boolean read FIsRequired write FIsRequired;
    property ObjectClassName: string read FObjectClassName write FObjectClassName;
    property Size: Integer read FSize write FSize default 0;
    property StorageName: string read FStorageName write FStorageName;
    property ValidCharsString: string read GetValidCharsString write SetValidCharsString;
  end;

  TInstantAttributeMetadatas = class(TInstantMetadatas)
  private
    function GetItems(Index: Integer): TInstantAttributeMetadata;
    procedure SetItems(Index: Integer; Value: TInstantAttributeMetadata);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TInstantClassMetadata);
    function Add: TInstantAttributeMetadata;
    procedure Clear;
    function Find(const AName: string): TInstantAttributeMetadata;
    procedure Remove(Item: TInstantAttributeMetadata);
    property Items[Index: Integer]: TInstantAttributeMetadata read GetItems write SetItems; default;
    function Owner: TInstantClassMetadata;
  end;

  TInstantClassMetadatas = class;
  TInstantAttributeMap = class;
  TInstantAttributeMaps = class;

  TInstantPersistence = (peEmbedded, peStored);

  TInstantClassMetadata = class(TInstantMetadata)
  private
    FAttributeMetadatas: TInstantAttributeMetadatas;
    FDefaultContainerName: string;
    FMemberMap: TInstantAttributeMap;
    FParent: TInstantClassMetadata;
    FParentName: string;
    FPersistence: TInstantPersistence;
    FStorageMap: TInstantAttributeMap;
    FStorageMaps: TInstantAttributeMaps;
    FStorageName: string;
    procedure BuildAttributeMap(Map: TInstantAttributeMap; Complete: Boolean);
    procedure BuildStorageMaps(Maps: TInstantAttributeMaps);
    procedure DestroyAttributeViews;
    function GetAttributeMetadatas: TInstantAttributeMetadatas;
    function GetCollection: TInstantClassMetadatas;
    procedure GetDistinctAttributeMetadatas(Map: TInstantAttributeMap);
    function GetIsEmpty: Boolean;
    function GetIsStored: Boolean;
    function GetMemberMap: TInstantAttributeMap;
    function GetParent: TInstantClassMetadata;
    function GetParentName: string;
    function GetStorageMap: TInstantAttributeMap;
    function GetStorageMaps: TInstantAttributeMaps;
    function GetTableName: string;
    procedure SetCollection(Value: TInstantClassMetadatas);
    procedure SetParent(Value: TInstantClassMetadata);
    procedure SetParentName(const Value: string);
    procedure SetTableName(const Value: string);
  protected
    procedure ClearParent;
    class procedure ConvertToBinary(Converter: TInstantTextToBinaryConverter); override;
    class procedure ConvertToText(Converter: TInstantBinaryToTextConverter); override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property AttributeMetadatas: TInstantAttributeMetadatas read GetAttributeMetadatas;
    property Collection: TInstantClassMetadatas read GetCollection write SetCollection;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsStored: Boolean read GetIsStored;
    property MemberMap: TInstantAttributeMap read GetMemberMap;
    property Parent: TInstantClassMetadata read GetParent write SetParent;
    property StorageMap: TInstantAttributeMap read GetStorageMap;
    property StorageMaps: TInstantAttributeMaps read GetStorageMaps;
    property TableName: string read GetTableName write SetTableName;
  published
    property DefaultContainerName: string read FDefaultContainerName write FDefaultContainerName;
    property ParentName: string read GetParentName write SetParentName;
    property Persistence: TInstantPersistence read FPersistence write FPersistence;
    property StorageName: string read FStorageName write FStorageName;
  end;

  TInstantClassMetadatas = class(TInstantMetadatas)
  private
    function GetItems(Index: Integer): TInstantClassMetadata;
    procedure SetItems(Index: Integer; Value: TInstantClassMetadata);
  protected
    class function CreateInstance(Arg: Pointer = nil): TInstantCollection; override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TInstantClassMetadata;
    function Find(const AName: string): TInstantClassMetadata;
    property Items[Index: Integer]: TInstantClassMetadata read GetItems write SetItems; default;
  end;

  TInstantModel = class(TPersistent)
  private
    FClassMetadatas: TInstantClassMetadatas;
    function GetClassMetadatas: TInstantClassMetadatas;
  protected
    procedure DestroyClassMetadatas;
  public
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromResFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure SaveToResFile(const FileName: string);
    property ClassMetadatas: TInstantClassMetadatas read GetClassMetadatas;
  end;

  TInstantTableMetadata = class;
  TInstantFieldMetadatas = class;

  TInstantDataType = (dtInteger, dtFloat, dtCurrency, dtBoolean, dtString, dtMemo, dtDateTime, dtBlob);
  TInstantDataTypes = set of TInstantDataType;
  TInstantFieldOption = (foRequired, foIndexed);
  TInstantFieldOptions = set of TInstantFieldOption;

  TInstantFieldMetadata = class(TInstantMetadata)
  private
    FDataType: TInstantDataType;
    FOptions: TInstantFieldOptions;
    FSize: Integer;
    FAlternateDataTypes: TInstantDataTypes;
    function GetCollection: TInstantFieldMetadatas;
    function GetTableMetadata: TInstantTableMetadata;
  protected
    function InternalEquals(const Other: TInstantMetadata): Boolean; override;
  public
    constructor Create(ACollection: TInstantFieldMetadatas); reintroduce;
    procedure Assign(Source: TPersistent); override;
    // Returns True if one of the data types of Other (Other.DataType and
    // Other.AlternateDataTypes) equals one of the data types of Self.
    function DataTypesEqual(const Other: TInstantFieldMetadata): Boolean;
    property Collection: TInstantFieldMetadatas read GetCollection;
    property DataType: TInstantDataType read FDataType write FDataType;
    // When field metadata is gathered from a database, there might be more
    // TInstantDataType values that apply (for example when the database
    // represents dtBoolean and dtInteger attributes with the same column type).
    // In that case, a datatype is chosen as the value of the DataType
    // property, and the others are put in AlternateDataTypes. The
    // DataTypesEqual method considers both DataType and AlternateDataTypes when
    // deciding upon data type "equality".
    property AlternateDataTypes: TInstantDataTypes
      read FAlternateDataTypes write FAlternateDataTypes;
    property Options: TInstantFieldOptions read FOptions write FOptions;
    property Size: Integer read FSize write FSize;
    property TableMetadata: TInstantTableMetadata read GetTableMetadata;
  end;

  TInstantFieldMetadatas = class(TInstantMetadatas)
  private
    function GetItems(Index: Integer): TInstantFieldMetadata;
    procedure SetItems(Index: Integer; Value: TInstantFieldMetadata);
  public
    constructor Create(AOwner: TInstantTableMetadata);
    procedure AddFieldMetadata(const AName: string; ADataType: TInstantDataType;
      ASize: Integer; AOptions: TInstantFieldOptions = []);
    function Add: TInstantFieldMetadata;
    function Find(const AName: string): TInstantFieldMetadata;
    property Items[Index: Integer]: TInstantFieldMetadata read GetItems write SetItems; default;
    function Owner: TInstantTableMetadata;
  end;

  TInstantIndexMetadatas = class;

  TInstantIndexMetadata = class(TInstantMetadata)
  private
    FFields: string;
    FOptions: TIndexOptions;
    function GetCollection: TInstantIndexMetadatas;
    function GetTableMetadata: TInstantTableMetadata;
  protected
    function InternalEquals(const Other: TInstantMetadata): Boolean; override;
  public
    constructor Create(ACollection: TInstantMetadatas); reintroduce;
    procedure Assign(Source: TPersistent); override;
    property Collection: TInstantIndexMetadatas read GetCollection;
    // Returns True if the field identified by AFieldMetadata is part of this
    // index.
    function IsFieldIndexed(const AFieldMetadata: TInstantFieldMetadata): Boolean;
    property TableMetadata: TInstantTableMetadata read GetTableMetadata;
  published
    property Fields: string read FFields write FFields;
    property Options: TIndexOptions read FOptions write FOptions;
  end;

  TInstantIndexMetadatas = class(TInstantMetadatas)
  private
    function GetItems(Index: Integer): TInstantIndexMetadata;
    procedure SetItems(Index: Integer; Value: TInstantIndexMetadata);
  public
    constructor Create(AOwner: TInstantTableMetadata);
    procedure AddIndexMetadata(const AName, AFields: string;
      AOptions: TIndexOptions);
    function Add: TInstantIndexMetadata;
    function Find(const AName: string): TInstantIndexMetadata;
    // Returns True if the field identified by AFieldMetadata is part of a
    // defined index.
    function IsFieldIndexed(const AFieldMetadata: TInstantFieldMetadata): Boolean;
    property Items[Index: Integer]: TInstantIndexMetadata read GetItems write SetItems; default;
    function Owner: TInstantTableMetadata;
  end;

  TInstantScheme = class;

  TInstantTableMetadata = class(TInstantMetadata)
  private
    FFieldMetadatas: TInstantFieldMetadatas;
    FIndexMetadatas: TInstantIndexMetadatas;
    function GetFieldMetadatas: TInstantFieldMetadatas;
    function GetIndexMetadatas: TInstantIndexMetadatas;
    function GetScheme: TInstantScheme;
    function GetFieldMetadataCount: Integer;
    function GetIndexMetadataCount: Integer;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Scheme: TInstantScheme read GetScheme;
  published
    property FieldMetadataCount: Integer read GetFieldMetadataCount;
    property FieldMetadatas: TInstantFieldMetadatas read GetFieldMetadatas;
    function FindFieldMetadata(const AName: string): TInstantFieldMetadata;
    function FindIndexMetadata(const AName: string): TInstantIndexMetadata;
    property IndexMetadataCount: Integer read GetIndexMetadataCount;
    property IndexMetadatas: TInstantIndexMetadatas read GetIndexMetadatas;
  end;

  TInstantTableMetadatas = class(TInstantMetadatas)
  private
    function GetItems(Index: Integer): TInstantTableMetadata;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TInstantTableMetadata;
    function Find(const AName: string): TInstantTableMetadata;
    property Items[Index: Integer]: TInstantTableMetadata read GetItems; default;
  end;

  TInstantCatalogFeature = (cfReadTableInfo, cfReadColumnInfo, cfReadIndexInfo);
  TInstantCatalogFeatures = set of TInstantCatalogFeature;
  // An object that provides the metadata info used by a TInstantScheme object
  // to build itself. It abstracts the way the information is fetched and its
  // source. It always works with a TInstantScheme. It is usually created
  // together with a TInstantScheme object:
  //   Scheme := TInstantScheme.Create;
  //   Scheme.Catalog := Broker.CreateCatalog(Scheme);
  // and the object ownership is transferred to Scheme, which is then
  // responsible for destroying the catalog object.
  TInstantCatalog = class
  private
    FScheme: TInstantScheme;
    FOnWarning: TInstantWarningEvent;
  protected
    function GetFeatures: TInstantCatalogFeatures; virtual;
    procedure DoWarning(const WarningText: string);
  public
    // Creates an instance and binds it to the specified TInstantScheme object.
    constructor Create(const AScheme: TInstantScheme);
    // A reference to the TInstantScheme object to which the current object is
    // bound, assigned on creation. The TInstantScheme object is responsible for
    // the current object's lifetime.
    property Scheme: TInstantScheme read FScheme;
    // Initializes ATableMetadatas from the catalog.
    procedure InitTableMetadatas(ATableMetadatas: TInstantTableMetadatas);
      virtual; abstract;
    // Returns a set of supported features. The predefined implementation
    // says that the catalog support everything; derived classes might not
    // support all features.
    property Features: TInstantCatalogFeatures read GetFeatures;
    // Triggered when the catalog has something to report about its activity,
    // typically during InitTableMetadatas, which is not a fatal error.
    property OnWarning: TInstantWarningEvent
      read FOnWarning write FOnWarning;
  end;

  // A TInstantCatalog that gathers its info from a TInstantModel.
  TInstantModelCatalog = class(TInstantCatalog)
  private
    FModel: TInstantModel;
  public
    // Creates an instance and binds it to the specified TInstantScheme object.
    // AModel is written to the Model property.
    constructor Create(const AScheme: TInstantScheme;
      const AModel: TInstantModel);
    // Initializes ATableMetadatas reading maps from the model.
    procedure InitTableMetadatas(ATableMetadatas: TInstantTableMetadatas);
      override;
    // A reference to the TInstantModel from which the catalog reads metadata
    // info.
    property Model: TInstantModel read FModel;
  end;

  TInstantScheme = class(TInstantStreamable)
  private
    FOnWarning: TInstantWarningEvent;
    FCatalog: TInstantCatalog;
    FTableMetadataCollection: TInstantTableMetadatas;
    FBlobStreamFormat: TInstantStreamFormat;
    FIdSize: Integer;
    FIdDataType: TInstantDataType;
    function GetTableMetadataCollection: TInstantTableMetadatas;
    function GetTableMetadatas(Index: Integer): TInstantTableMetadata;
    function GetTableMetadataCount: Integer;
    procedure SetCatalog(const Value: TInstantCatalog);
    procedure CatalogWarningEventHandler(const Sender: TObject;
      const AWarningText: string);
  protected
    procedure DoWarning(const AWarningText: string);
    function AttributeTypeToDataType(
      AttributeType: TInstantAttributeType): TInstantDataType; virtual;
    property TableMetadataCollection: TInstantTableMetadatas read GetTableMetadataCollection;
  public
    constructor Create;
    destructor Destroy; override;
    property Catalog: TInstantCatalog read FCatalog write SetCatalog;
    function FindTableMetadata(const AName: string): TInstantTableMetadata;
    property TableMetadataCount: Integer read GetTableMetadataCount;
    property TableMetadatas[Index: Integer]: TInstantTableMetadata read GetTableMetadatas;
    property BlobStreamFormat: TInstantStreamFormat read FBlobStreamFormat write FBlobStreamFormat default sfBinary;
    property IdDataType: TInstantDataType read FIdDataType write FIdDataType default dtString;
    property IdSize: Integer read FIdSize write FIdSize default InstantDefaultFieldSize;
    // Triggered when the scheme has something to report about its activity,
    // typically during database building/evolution, which is not a fatal error.
    property OnWarning: TInstantWarningEvent read FOnWarning write FOnWarning;
  end;

  TInstantAttributeMap = class(TInstantNamedList)
  private
    FClassMetadata: TInstantClassMetadata;
    FName: string;
    function GetItems(Index: Integer): TInstantAttributeMetadata;
    procedure SetItems(Index: Integer; Value: TInstantAttributeMetadata);
    function GetIsRootMap: Boolean;
  protected
    function GetName: string; override;
    procedure SetName(const Value: string); override;
  public
    constructor Create(AClassMetadata: TInstantClassMetadata);
    function Add(Item: TInstantAttributeMetadata): Integer;
    function AddUnique(Item: TInstantAttributeMetadata): Integer;
    function Find(const AName: string): TInstantAttributeMetadata;
    function IndexOf(Item: TInstantAttributeMetadata): Integer;
    procedure Insert(Index: Integer; Item: TInstantAttributeMetadata);
    function Remove(Item: TInstantAttributeMetadata): Integer;
    property ClassMetadata: TInstantClassMetadata read FClassMetadata;
    property IsRootMap: Boolean read GetIsRootMap;
    property Items[Index: Integer]: TInstantAttributeMetadata read GetItems write SetItems; default;
  end;

  TInstantAttributeMaps = class(TObjectList)
  private
    FClassMetadata: TInstantClassMetadata;
    function GetItems(Index: Integer): TInstantAttributeMap;
    function GetRootMap: TInstantAttributeMap;
    procedure SetItems(Index: Integer; Value: TInstantAttributeMap);
  public
    constructor Create(AClassMetadata: TInstantClassMetadata);
    function Add: TInstantAttributeMap; overload;
    function Add(Item: TInstantAttributeMap): Integer; overload;
    function Find(const AName: string): TInstantAttributeMap;
    function FindMap(const AttributeName: string): TInstantAttributeMap;
    function EnsureMap(const AName: string): TInstantAttributeMap;
    function IndexOf(Item: TInstantAttributeMap): Integer;
    procedure Insert(Index: Integer; Item: TInstantAttributeMap);
    function Remove(Item: TInstantAttributeMap): Integer;
    property ClassMetadata: TInstantClassMetadata read FClassMetadata;
    property RootMap: TInstantAttributeMap read GetRootMap;
    property Items[Index: Integer]: TInstantAttributeMap read GetItems write SetItems; default;
  end;

  TInstantConnector = class;
  TInstantComplex = class;

  TInstantObjectReference = class(TInstantStreamable)
  private
    FInstance: TInstantObject;
    FObjectClassName: string;
    FObjectId: string;
    FOwner: TInstantComplex;
    FOwnsInstance: Boolean;
    procedure ClearReference;
    procedure DoAssignInstance(AInstance: TInstantObject; AOwnsInstance: Boolean);
    function GetInstance: TInstantObject;
    function GetObjectClass: TInstantObjectClass;
    function GetObjectClassName: string;
    function GetObjectId: string;
    function GetSignature: string;
    procedure SetInstance(Value: TInstantObject);
    procedure SetOwnsInstance(const Value: Boolean);
  protected
    class function CreateInstance(Arg: Pointer = nil): TInstantStreamable; override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    constructor Create(AInstance: TInstantObject = nil; AOwnsInstance: Boolean = False;
      AOwner: TInstantComplex = nil);
    constructor Clone(Source: TInstantObjectReference; AOwnsInstance: Boolean;
      AOwner: TInstantComplex = nil);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignInstance(AInstance: TInstantObject);
    function Dereference(Connector: TInstantConnector = nil;
      AOwnsInstance: Boolean = True; Retry: Boolean = False): TInstantObject;
    procedure DestroyInstance;
    function Equals(const AObjectClassName, AObjectId: string): Boolean; overload;
    function Equals(AObject: TInstantObject): Boolean; overload;
    function HasInstance: Boolean;
    function HasReference: Boolean;
    function IsBroken: Boolean;
    procedure ReadAsObject(Reader: TInstantReader); virtual;
    procedure Reset;
    procedure ReferenceObject(const AObjectClassName,
      AObjectId: string); overload;
    procedure ReferenceObject(AObjectClass: TInstantObjectClass;
      const AObjectId: string); overload;
    procedure WriteAsObject(Writer: TInstantWriter); virtual;
    property Instance: TInstantObject read GetInstance write SetInstance;
    property ObjectClass: TInstantObjectClass read GetObjectClass;
    property ObjectClassName: string read GetObjectClassName;
    property ObjectId: string read GetObjectId;
    property OwnsInstance: Boolean read FOwnsInstance write SetOwnsInstance;
    property Signature: string read GetSignature;
  end;

  EInstantAccessError = class(EInstantError)
  end;

  TInstantAttribute = class(TInstantStreamable)
  private
    FIsChanged: Boolean;
    FMetadata: TInstantAttributeMetadata;
    FOwner: TInstantObject;
    function GetIsIndexed: Boolean;
    function GetIsRequired: Boolean;
    function GetName: string;
    function GetValue: Variant;
    procedure SetMetadata(Value: TInstantAttributeMetadata);
    procedure SetOwner(AOwner: TInstantObject);
    procedure SetValue(AValue: Variant);
  protected
    function AccessError(const TypeName: string): EInstantAccessError;
    class function AttributeType: TInstantAttributeType; virtual; abstract;
    function ConversionError(E: Exception): EInstantConversionError;
    function GetAsBoolean: Boolean; virtual;
    function GetAsCurrency: Currency; virtual;
    function GetAsDateTime: TDateTime; virtual;
    function GetAsFloat: Double; virtual;
    function GetAsInteger: Integer; virtual;
    function GetAsObject: TInstantObject; virtual;
    function GetAsString: string; virtual;
    function GetAsVariant: Variant; virtual;
    function GetDisplayText: string; virtual;
    function GetIsChanged: Boolean; virtual;
    function GetIsDefault: Boolean; virtual;
    function GetIsMandatory: Boolean; virtual;
    function GetOwner: TPersistent; override;
    procedure Initialize; virtual;
    procedure ReadName(Reader: TInstantReader);
    procedure SetAsBoolean(AValue: Boolean); virtual;
    procedure SetAsCurrency(AValue: Currency); virtual;
    procedure SetAsDateTime(AValue: TDateTime); virtual;
    procedure SetAsFloat(AValue: Double); virtual;
    procedure SetAsInteger(AValue: Integer); virtual;
    procedure SetAsObject(AValue: TInstantObject); virtual;
    procedure SetAsString(const AValue: string); virtual;
    procedure SetAsVariant(AValue: Variant); virtual;
    procedure SetIsChanged(Value: Boolean);
    procedure StringValidationError(InvalidChar: Char);
    procedure WriteName(Writer: TInstantWriter);
  public
    constructor Create(AOwner: TInstantObject = nil;
      AMetadata: TInstantAttributeMetadata = nil); virtual;
    procedure Changed;
    procedure CheckHasMetadata;
    procedure Reset; virtual;
    procedure Unchanged; virtual;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsObject: TInstantObject read GetAsObject write SetAsObject;
    property AsString: string read GetAsString write SetAsstring;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property DisplayText: string read GetDisplayText;
    property IsChanged: Boolean read GetIsChanged write SetIsChanged;
    property IsDefault: Boolean read GetIsDefault;
    property IsIndexed: Boolean read GetIsIndexed;
    property IsMandatory: Boolean read GetIsMandatory;
    property IsRequired: Boolean read GetIsRequired;
    property Name: string read GetName;
    property Metadata: TInstantAttributeMetadata read FMetadata write SetMetadata;
    property Owner: TInstantObject read FOwner;
    property Value: Variant read GetValue write SetValue;
  end;

  PInstantAttribute = ^TInstantAttribute;

  TInstantSimple = class(TInstantAttribute)
  protected
    function InvalidValueError(AValue: Variant; E: Exception): EInstantError;
  end;

  TInstantNumeric = class(TInstantSimple)
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsDateTime: TDateTime; override;
    function GetDisplayText: string; override;
    function GetIsDefault: Boolean; override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsDateTime(AValue: TDateTime); override;
  end;

  TInstantInteger = class(TInstantNumeric)
  private
    FValue: Integer;
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetValue: Integer; virtual;
    procedure Initialize; override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetAsFloat(AValue: Double); override;
    procedure SetAsInteger(AValue: Integer); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetAsVariant(AValue: Variant); override;
    procedure SetValue(AValue: Integer); virtual;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Value: Integer read GetValue write SetValue;
  end;

  TInstantFloat = class(TInstantNumeric)
  private
    FValue: Double;
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetValue: Double; virtual;
    procedure Initialize; override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetAsFloat(AValue: Double); override;
    procedure SetAsInteger(AValue: Integer); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetAsVariant(AValue: Variant); override;
    procedure SetValue(AValue: Double); virtual;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Value: Double read GetValue write SetValue;
  end;

  TInstantCurrency = class(TInstantNumeric)
  private
    FValue: Currency;
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAsBoolean: Boolean; override;
    function GetAsDateTime: TDateTime; override;
    function GetDisplayText: string; override;
    function GetIsDefault: Boolean; override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsDateTime(AValue: TDateTime); override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetValue: Currency; virtual;
    procedure Initialize; override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetAsFloat(AValue: Double); override;
    procedure SetAsInteger(AValue: Integer); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetAsVariant(AValue: Variant); override;
    procedure SetValue(AValue: Currency); virtual;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Value: Currency read GetValue write SetValue;
  end;

  TInstantBoolean = class(TInstantSimple)
  private
    FValue: Boolean;
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetIsDefault: Boolean; override;
    function GetValue: Boolean; virtual;
    procedure Initialize; override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsFloat(AValue: Double); override;
    procedure SetAsInteger(AValue: Integer); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetAsVariant(AValue: Variant); override;
    procedure SetValue(AValue: Boolean); virtual;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Value: Boolean read GetValue write SetValue;
  end;

  TInstantString = class(TInstantSimple)
  private
    FValue: string;
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetValue: string; virtual;
    procedure Initialize; override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsCurrency(AValue: Currency); override;
    procedure SetAsDateTime(AValue: TDateTime); override;
    procedure SetAsFloat(AValue: Double); override;
    procedure SetAsInteger(AValue: Integer); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetAsVariant(AValue: Variant); override;
    procedure SetValue(const AValue: string); virtual;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Value: string read GetValue write SetValue;
  end;

  TInstantDateTime = class(TInstantSimple)
  private
    FValue: TDateTime;
    function DefaultValue: TDateTime;
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDisplayText: string; override;
    function GetIsDefault: Boolean; override;
    function GetValue: TDateTime; virtual;
    procedure Initialize; override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetAsDateTime(AValue: TDateTime); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetAsVariant(AValue: Variant); override;
    procedure SetValue(AValue: TDateTime); virtual;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Value: TDateTime read GetValue write SetValue;
  end;

  TInstantBlob = class(TInstantSimple)
  private
    FStream: TMemoryStream;
    function GetSize: Integer;
    function GetStream: TMemoryStream;
    function GetValue: string;
    procedure SetValue(const AValue: string);
    property Stream: TMemoryStream read GetStream;
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    procedure Initialize; override;
    function Read(var Buffer; Position, Count: Integer): Integer; virtual;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetAsVariant(AValue: Variant); override;
    function Write(const Buffer; Position, Count: Integer): Integer; virtual;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignPicture(Source: TPicture);
    procedure AssignToPicture(Dest: TPicture);
    procedure Clear;
    procedure LoadDataFromStream(AStream: TStream);
    function ReadBuffer(var Buffer; Position, Count: Integer): Integer;
    procedure Reset; override;
    procedure SaveDataToStream(AStream: TStream);
    function WriteBuffer(const Buffer; Position, Count: Integer): Integer;
    property Size: Integer read GetSize;
  published
    property Value: string read GetValue write SetValue;
  end;

  TInstantMemo = class(TInstantBlob)
  protected
    class function AttributeType: TInstantAttributeType; override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure WriteObject(Writer: TInstantWriter); override;
  end;

  TInstantGraphic = class(TInstantBlob)
  protected
    class function AttributeType: TInstantAttributeType; override;
  end;

  TInstantComplex = class(TInstantAttribute)
  private
    function GetRequiredClass: TInstantObjectClass;
    function GetRequiredClassName: string;
  protected
    function GetAllowOwned: Boolean; virtual;
    function GetConnector: TInstantConnector; virtual;
    procedure ValidateObject(AObject: TInstantObject); virtual;
    procedure ValidateObjectClassType(AClass: TInstantObjectClass);
  public
    function AttachObject(AObject: TInstantObject): Boolean; virtual;
    property Connector: TInstantConnector read GetConnector;
    function DetachObject(AObject: TInstantObject): Boolean; virtual;
    property AllowOwned: Boolean read GetAllowOwned;
    property RequiredClass: TInstantObjectClass read GetRequiredClass;
    property RequiredClassName: string read GetRequiredClassName;
  end;

  TInstantElement = class(TInstantComplex)
  protected
    function CreateObject: TInstantObject; virtual;
    function GetAsObject: TInstantObject; override;
    function GetValue: TInstantObject; virtual; abstract;
    procedure SetAsObject(AValue: TInstantObject); override;
    procedure SetValue(AValue: TInstantObject); virtual;
  public
    function AttachObject(AObject: TInstantObject): Boolean; override;
    function DetachObject(AObject: TInstantObject): Boolean; override;
    function HasValue: Boolean; virtual;
    procedure LoadObjectFromStream(AStream: TStream); virtual;
    procedure SaveObjectToStream(AStream: TStream); virtual;
  published
    property Value: TInstantObject read GetValue write SetValue;
  end;

  TInstantPart = class(TInstantElement)
  private
    FValue: TInstantObject;
    procedure ClearOwnerContext(AObject: TInstantObject);
    procedure DestroyObject;
    procedure SetOwnerContext(AObject: TInstantObject);
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetIsChanged: Boolean; override;
    function GetIsDefault: Boolean; override;
    function GetValue: TInstantObject; override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetValue(AValue: TInstantObject); override;
    procedure ValidateObject(AObject: TInstantObject); override;
    procedure WriteObject(Writer: TInstantWriter); override;
    function GetAllowOwned: Boolean; override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function HasValue: Boolean; override;
    procedure Reset; override;
    procedure Unchanged; override;
  end;

  TInstantReference = class(TInstantElement)
  private
    FAllowOwned: Boolean;
    FConnector: TInstantConnector;
    FObjectReference: TInstantObjectReference;
    procedure DestroyObjectReference;
    function GetInstance: TInstantObject;
    function GetObjectClass: TInstantObjectClass;
    function GetObjectClassName: string;
    function GetObjectId: string;
    function GetObjectReference: TInstantObjectReference;
    function SameInstance(AObject: TInstantObject): Boolean; overload;
    function SameInstance(const AObjectClassName,
      AObjectId: string): Boolean; overload;
    function SameInstance(AObjectClass: TInstantObjectClass;
      AObjectId: string): Boolean; overload;
    procedure SetInstance(AValue: TInstantObject);
    property ObjectReference: TInstantObjectReference read GetObjectReference;
  protected
    class function AttributeType: TInstantAttributeType; override;
    function CreateObject: TInstantObject; override;
    function GetAllowOwned: Boolean; override;
    function GetConnector: TInstantConnector; override;
    function GetIsBroken: Boolean; virtual;
    function GetIsDefault: Boolean; override;
    function GetValue: TInstantObject; override;
    procedure ReadObject(Reader: TInstantReader); override;
    function RetrieveObject: TInstantObject; virtual;
    procedure SetAllowOwned(Value: Boolean); virtual;
    procedure SetConnector(Value: TInstantConnector);
    procedure SetValue(AValue: TInstantObject); override;
    procedure ValidateObject(AObject: TInstantObject); override;
    procedure WriteObject(Writer: TInstantWriter); override;
    property Instance: TInstantObject read GetInstance write SetInstance;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DestroyObject;
    function HasReference: Boolean;
    function HasValue: Boolean; override;
    procedure LoadObjectFromStream(AStream: TStream); override;
    procedure ReferenceObject(AObjectClass: TInstantObjectClass;
      const AObjectId: string); overload;
    procedure ReferenceObject(const AObjectClassName, AObjectId: string); overload;
    procedure Reset; override;
    property AllowOwned write SetAllowOwned;
    property Connector write SetConnector;
    property IsBroken: Boolean read GetIsBroken;
    property ObjectClass: TInstantObjectClass read GetObjectClass;
    property ObjectClassName: string read GetObjectClassName;
    property ObjectId: string read GetObjectId;
  end;

  TInstantSortCompare = function(Holder, Obj1, Obj2: TInstantObject): Integer of object;
  TInstantContentChangeType = (ctAdd, ctRemove, ctReplace, ctClear);

  TInstantContainer = class(TInstantComplex)
  private
    function GetItems(Index: Integer): TInstantObject;
    function GetChangeCount: Integer;
    procedure SetItems(Index: Integer; AValue: TInstantObject);
    procedure QuickSort(L, R: Integer; Compare: TInstantSortCompare);
  protected
    procedure AfterContentChange(ChangeType: TInstantContentChangeType;
      Index: Integer; AObject: TInstantObject);
    procedure BeforeContentChange(ChangeType: TInstantContentChangeType;
      Index: Integer; AObject: TInstantObject);
    procedure CheckRange(Index: Integer);
    function GetCount: Integer; virtual; abstract;
    function GetInstances(Index: Integer): TInstantObject; virtual;
    function GetIsDefault: Boolean; override;
    function InternalAdd(AObject: TInstantObject): Integer; virtual; abstract;
    procedure InternalClear; virtual; abstract;
    procedure InternalDelete(Index: Integer); virtual; abstract;
    function InternalGetItems(Index: Integer): TInstantObject; virtual; abstract;
    function InternalIndexOf(AObject: TInstantObject): Integer; virtual; abstract;
    function InternalIndexOfInstance(Instance: Pointer): Integer; virtual; abstract;
    procedure InternalExchange(Index1, Index2: Integer); virtual; abstract;
    procedure InternalInsert(Index: Integer; AObject: TInstantObject); virtual; abstract;
    procedure InternalMove(CurIndex, NewIndex: Integer); virtual; abstract;
    procedure InternalSetItems(Index: Integer; AValue: TInstantObject); virtual; abstract;
    procedure ValidateObject(AObject: TInstantObject); override;
    property Instances[Index: Integer]: TInstantObject read GetInstances;
  public
    function Add(AObject: TInstantObject): Integer;
    function AttachObject(AObject: TInstantObject): Boolean; override;
    procedure Clear;
    procedure Delete(Index: Integer);
    function DetachObject(AObject: TInstantObject): Boolean; override;
    procedure Exchange(Index1, Index2: Integer);
    function HasItem(Index: Integer): Boolean;
    function IndexOf(AObject: TInstantObject): Integer;
    function IndexOfInstance(Instance: Pointer): Integer;
    procedure Insert(Index: Integer; AObject: TInstantObject);
    procedure LoadObjectsFromStream(AStream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(AObject: TInstantObject): Integer;
    procedure Reset; override;
    procedure SaveObjectsToStream(AStream: TStream);
    procedure Sort(Compare: TInstantSortCompare);
    property ChangeCount: Integer read GetChangeCount;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TInstantObject read GetItems write SetItems; default;
  end;

  TInstantParts = class(TInstantContainer)
  private
    FAllowOwned: Boolean;
    FObjectReferenceList: TObjectList;
    FObjectList: TObjectList;
    function GetObjectList: TObjectList;
    procedure ClearOwnerContext(AObject: TInstantObject);
    procedure SetOwnerContext(AObject: TInstantObject);
    property ObjectList: TObjectList read GetObjectList;
    function CreateObjectReference(AObject: TInstantObject): TInstantObjectReference;
    function GetObjectReferenceList: TObjectList;
    function GetObjectReferences(Index: Integer): TInstantObjectReference;
    procedure SetObjectReferences(Index: Integer; Value: TInstantObjectReference);
    property ObjectReferenceList: TObjectList read GetObjectReferenceList;
    property ObjectReferences[Index: Integer]: TInstantObjectReference read GetObjectReferences write SetObjectReferences;
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAllowOwned: Boolean; override;
    function GetCount: Integer; override;
    function GetIsChanged: Boolean; override;
    function GetInstances(Index: Integer): TInstantObject; override;
    function InternalAdd(AObject: TInstantObject): Integer; override;
    procedure InternalClear; override;
    procedure InternalDelete(Index: Integer); override;
    function InternalGetItems(Index: Integer): TInstantObject; override;
    function InternalIndexOf(AObject: TInstantObject): Integer; override;
    function InternalIndexOfInstance(Instance: Pointer): Integer; override;
    procedure InternalExchange(Index1, Index2: Integer); override;
    procedure InternalInsert(Index: Integer; AObject: TInstantObject); override;
    procedure InternalMove(CurIndex, NewIndex: Integer); override;
    procedure InternalSetItems(Index: Integer; AValue: TInstantObject); override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetAllowOwned(Value: Boolean); virtual;
    procedure ValidateObject(AObject: TInstantObject); override;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Unchanged; override;
    procedure DestroyObject(Index: Integer); overload;
    function DestroyObject(AObject: TInstantObject): Boolean; overload;
    property AllowOwned write SetAllowOwned;
  end;

  TInstantReferences = class(TInstantContainer)
  private
    FAllowOwned: Boolean;
    FConnector: TInstantConnector;
    FObjectReferenceList: TInstantObjectReferenceList;
    function GetObjectReferenceList: TInstantObjectReferenceList;
    function GetRefItems(Index: Integer): TInstantObjectReference;
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAllowOwned: Boolean; override;
    function GetConnector: TInstantConnector; override;
    function GetCount: Integer; override;
    function GetInstances(Index: Integer): TInstantObject; override;
    function InternalAdd(AObject: TInstantObject): Integer; override;
    procedure InternalClear; override;
    procedure InternalDelete(Index: Integer); override;
    function InternalGetItems(Index: Integer): TInstantObject; override;
    function InternalIndexOf(AObject: TInstantObject): Integer; override;
    function InternalIndexOfInstance(Instance: Pointer): Integer; override;
    procedure InternalExchange(Index1, Index2: Integer); override;
    procedure InternalInsert(Index: Integer; AObject: TInstantObject); override;
    procedure InternalMove(CurIndex, NewIndex: Integer); override;
    procedure InternalSetItems(Index: Integer; AValue: TInstantObject); override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetAllowOwned(Value: Boolean); virtual;
    procedure ValidateObject(AObject: TInstantObject); override;
    procedure WriteObject(Writer: TInstantWriter); override;
    property ObjectReferenceList: TInstantObjectReferenceList read
        GetObjectReferenceList;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DestroyObject(Index: Integer); overload;
    function DestroyObject(AObject: TInstantObject): Boolean; overload;
    procedure LoadObjectsFromStream(AStream: TStream); override;
    procedure LoadReferencesFromStream(AStream: TStream);
    procedure SaveReferencesToStream(AStream: TStream);
    property AllowOwned write SetAllowOwned;
    property Connector write FConnector;
    property RefItems[Index: Integer]: TInstantObjectReference read GetRefItems;
  end;

  TInstantObjectState = class(TPersistent)
  private
    FIsChanged: Boolean;
    FPersistentId: string;
    FUpdateCount: Integer;
  public
    procedure Assign(Source: TPersistent); override;
    property IsChanged: Boolean read FIsChanged write FIsChanged;
    property PersistentId: string read FPersistentId write FPersistentId;
    property UpdateCount: Integer read FUpdateCount write FUpdateCount;
  end;

  TInstantOperationType = (otNone, otCreate, otStore, otRetrieve, otRefresh, otDispose);
  TInstantErrorAction = (eaRetry, eaIgnore, eaError, eaRevert, eaCancel);
  TInstantVerificationResult = (vrOk, vrCancel, vrAbort, vrError);
  TInstantConflictAction = (caIgnore, caFail);

  TInstantAttributeChangeEvent = procedure(Sender: TInstantObject; Attribute: TInstantAttribute) of object;
  TInstantContentChangeEvent = procedure(Sender: TInstantObject; Container: TInstantContainer;
    ChangeType: TInstantContentChangeType; Index: Integer; AObject: TInstantObject) of object;
  TInstantErrorEvent = procedure(Sender: TObject; OperationType: TInstantOperationType; E: Exception;
    var Action: TInstantErrorAction) of object;
  TInstantNotifyEvent = procedure(Sender: TInstantObject) of object;
  TInstantProgressEvent = procedure(Sender: TObject; Count: Integer; var Continue: Boolean) of object;
  TInstantUpdateOperation = procedure(ConflictAction: TInstantConflictAction) of object;
  TInstantInvokableMethod = function(Params: TParams): string of object;

  TInstantObjectStore = class;

  TInstantObject = class(TInstantStreamable, IUnknown)
  private
    FDefaultContainer: TInstantContainer;
    FChangeCount: Integer;
    FChangesDisabledCount: Integer;
    FConnector: TInstantConnector;
    FId: string;
    FInUpdate: Boolean;
    FObjectStore: TInstantObjectStore;
    FOwner: TInstantObject;
    FOwnerAttribute: TInstantComplex;
    FRefBy: TObjectList;
    FRefCount: Integer;
    FSavedState: TInstantObjectState;
    FSaveStateLevel: Integer;
    FState: TInstantObjectState;
    FOnAfterContentChange: TInstantContentChangeEvent;
    FOnAttributeChanged: TInstantAttributeChangeEvent;
    FOnBeforeContentChange: TInstantContentChangeEvent;
    FOnChange: TInstantNotifyEvent;
    FOnError: TInstantErrorEvent;
    procedure ClearOwnerContext;
    procedure CreateAttributes;
    procedure DestroyAttributes;
    procedure DestroyInternalFields;
    procedure DoAfterContentChange(Container: TInstantContainer;
      ChangeType: TInstantContentChangeType; Index: Integer; AObject: TInstantObject);
    procedure DoAfterCreate;
    procedure DoAfterDispose;
    procedure DoAfterRefresh;
    procedure DoAfterRetrieve;
    procedure DoAfterStore;
    procedure DoAttributeChanged(Attribute: TInstantAttribute);
    procedure DoBeforeContentChange(Container: TInstantContainer;
      ChangeType: TInstantContentChangeType; Index: Integer; AObject: TInstantObject);
    procedure DoBeforeDispose;
    procedure DoBeforeRefresh;
    procedure DoBeforeStore;
    procedure DoChange;
    procedure DoDispose(ConflictAction: TInstantConflictAction);
    procedure DoHandleError(OperationType: TInstantOperationType; E: Exception;
      var Action: TInstantErrorAction);
    procedure DoOwnedObjectChanged(AObject: TInstantObject);
    procedure DoOwnerChanged(OldOwner: TInstantObject; OldOwnerAttribute: TInstantComplex);
    procedure DoRefresh;
    function DoRelease: Integer;
    procedure DoStore(ConflictAction: TInstantConflictAction);
    procedure DoUnchange;
    function FindDefaultContainer: TInstantContainer;
    procedure FreeCircularReferences;
    function GetClassId: string;
    function GetDefaultContainer: TInstantContainer;
    function GetHasDefaultContainer: Boolean;
    function GetIsAbandoned: Boolean;
    function GetIsChanged: Boolean;
    function GetIsDefault: Boolean;
    function GetIsIdChanged: Boolean;
    function GetIsOwned: Boolean;
    function GetIsPersistent: Boolean;
    function GetObjectChangeCount: Integer;
    function GetObjectCount: Integer;
    function GetObjects(Index: Integer): TInstantObject;
    function GetObjectStore: TInstantObjectStore;
    function GetPersistentId: string;
    function GetRefBy(Index: Integer): TInstantComplex;
    function GetRefByCount: Integer;
    function GetRefByList: TObjectList;
    function GetSavedState: TInstantObjectState;
    function GetState: TInstantObjectState;
    function GetUpdateCount: Integer;
    procedure Init;
    procedure Finit;
    procedure PerformUpdate(Operation: TInstantUpdateOperation; OperationType: TInstantOperationType;
      ConflictAction: TInstantConflictAction);
    procedure ReadAttributes(Reader: TInstantReader);
    procedure SetConnector(AConnector: TInstantConnector);
    procedure SetIsChanged(Value: Boolean);
    procedure SetObjects(Index: Integer; Value: TInstantObject);
    procedure SetOwnerContext(AOwner: TInstantObject; Attribute: TInstantComplex);
    procedure SetPersistentId(const Value: string);
    procedure SetUpdateCount(Value: Integer);
    procedure WriteAttributes(Writer: TInstantWriter);
    property ObjectStore: TInstantObjectStore read GetObjectStore;
    property SavedState: TInstantObjectState read GetSavedState;
    property State: TInstantObjectState read GetState;
  protected
    procedure Abandon;
    procedure AfterAddRef; virtual;
    procedure AfterAssign; virtual;
    procedure AfterContentChange(Container: TInstantContainer; ChangeType: TInstantContentChangeType;
      Index: Integer; AObject: TInstantObject); virtual;
    procedure AfterCreate; virtual;
    procedure AfterCreateAttributes; virtual;
    procedure AfterDispose; virtual;
    procedure AfterRefresh; virtual;
    procedure AfterRetrieve; virtual;
    procedure AfterStore; virtual;
    procedure ApplyState; virtual;
    function AttributeAddress(const AttributeName: string): PInstantAttribute; virtual;
    procedure AttributeChanged(Attribute: TInstantAttribute); virtual;
    procedure BeforeAssign; virtual;
    procedure BeforeContentChange(Container: TInstantContainer; ChangeType: TInstantContentChangeType;
      Index: Integer; AObject: TInstantObject); virtual;
    procedure BeforeCreateAttributes; virtual;
    procedure BeforeDispose; virtual;
    procedure BeforeRelease; virtual;
    procedure BeforeRefresh; virtual;
    procedure BeforeStore; virtual;
    class procedure ConvertToBinary(Converter: TInstantTextToBinaryConverter); override;
    class procedure ConvertToText(Converter: TInstantBinaryToTextConverter); override;
    class function CreateInstance(Arg: Pointer = nil): TInstantStreamable; override;
    procedure Destruct; virtual;
    procedure DisposeOwnedObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction);
    procedure Finalize; virtual;
    function GenerateId: string; virtual;
    function GetCaption: string; virtual;
    function GetId: string; virtual;
    function GetObjectClass: TInstantObjectClass; virtual;
    function GetOwner: TPersistent; override;
    procedure HandleError(OperationType: TInstantOperationType; E: Exception;
      var Action: TInstantErrorAction); virtual;
    procedure Initialize; virtual;
    procedure OwnedObjectChanged(AObject: TInstantObject); virtual;
    procedure OwnerChanged(OldOwner: TInstantObject; OldOwnerAttribute: TInstantComplex); virtual;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure RestoreState; virtual;
    procedure SaveState; virtual;
    procedure SetId(const Value: string); virtual;
    function VerifyOperation(OperationType: TInstantOperationType): TInstantVerificationResult; virtual;
    procedure WriteObject(Writer: TInstantWriter); override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    property DefaultContainer: TInstantContainer read GetDefaultContainer;
    property RefByList: TObjectList read GetRefByList;
  public
    constructor Clone(Source: TInstantObject;
      AConnector: TInstantConnector = nil); overload; virtual;
    constructor Create(AConnector: TInstantConnector = nil);
    constructor Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
      Refresh: Boolean = False; AConnector: TInstantConnector = nil);
    function AddObject(AObject: TInstantObject): Integer; overload;
    function AddRef: Integer;
    procedure Assign(Source: TPersistent); override;
    function AttributeByName(const AttributeName: string): TInstantAttribute;
    function CanDispose: Boolean;
    function CanStore: Boolean;
    procedure Changed; virtual;
    function ChangesDisabled: Boolean;
    procedure CheckId;
{$IFDEF FPC}
    class function ClassType: TInstantObjectClass;
{$ELSE}
    function ClassType: TInstantObjectClass;
{$ENDIF}
    procedure ClearObjects;
    function Clone(AConnector: TInstantConnector = nil): TInstantObject; overload;
    function ContainerByName(const ContainerName: string): TInstantContainer;
    procedure DeleteObject(Index: Integer);
    procedure DisableChanges;
    procedure Dispose(ConflictAction: TInstantConflictAction = caFail);
    procedure EnableChanges;
    function EqualsSignature(const AObjectClassName, AObjectId: string): Boolean;
    function EqualsPersistentSignature(const AObjectClassName,
      AObjectId: string): Boolean;
    function FindAttribute(const AttributeName: string): TInstantAttribute;
    function FindContainer(const ContainerName: string): TInstantContainer;
    procedure FreeInstance; override;
    function GetNamePath: string; override;
    function IndexOfObject(AObject: TInstantObject): Integer;
    procedure InsertObject(Index: Integer; AObject: TInstantObject);
    function IsOperationAllowed(OperationType: TInstantOperationType): Boolean;
    function Invoke(const MethodName: string; Params: TParams): string;
    class function Metadata: TInstantClassMetadata; virtual;
    class function NewInstance: TObject; override;
    procedure Refresh;
    class procedure RefreshAll(AConnector: TInstantConnector = nil; Progress: TInstantProgressEvent = nil);
    function Release: Integer;
    function RemoveObject(AObject: TInstantObject): Integer;
    procedure ResetAttributes;
    procedure Store(ConflictAction: TInstantConflictAction = caFail);
    procedure Unchanged;
    property Caption: string read GetCaption;
    property ClassId: string read GetClassId;
    property Connector: TInstantConnector read FConnector;
    property HasDefaultContainer: Boolean read GetHasDefaultContainer;
    property IsChanged: Boolean read GetIsChanged write SetIsChanged;
    property IsDefault: Boolean read GetIsDefault;
    property IsIdChanged: Boolean read GetIsIdChanged;
    property IsAbandoned: Boolean read GetIsAbandoned;
    property IsOwned: Boolean read GetIsOwned;
    property IsPersistent: Boolean read GetIsPersistent;
    property ObjectChangeCount: Integer read GetObjectChangeCount;
    property ObjectClass: TInstantObjectClass read GetObjectClass;
    property ObjectCount: Integer read GetObjectCount;
    property Id: string read GetId write SetId;
    property Objects[Index: Integer]: TInstantObject read GetObjects write SetObjects;
    property Owner: TInstantObject read FOwner;
    property OwnerAttribute: TInstantComplex read FOwnerAttribute;
    property PersistentId: string read GetPersistentId;
    property RefCount: Integer read FRefCount;
    property RefBy[Index: Integer]: TInstantComplex read GetRefBy;
    property RefByCount: Integer read GetRefByCount;
    property UpdateCount: Integer read GetUpdateCount;
    property OnAfterContentChange: TInstantContentChangeEvent read FOnAfterContentChange write FOnAfterContentChange;
    property OnAttributeChanged: TInstantAttributeChangeEvent read FOnAttributeChanged write FOnAttributeChanged;
    property OnBeforeContentChange: TInstantContentChangeEvent read FOnBeforeContentChange write FOnBeforeContentChange;
    property OnChange: TInstantNotifyEvent read FOnChange write FOnChange;
    property OnError: TInstantErrorEvent read FOnError write FOnError;
  end;

  TInstantConnectorClass = class of TInstantConnector;

  TInstantConnectionDefClass = class of TInstantConnectionDef;

  TInstantConnectionDef = class(TInstantCollectionItem)
  private
    FIsBuilt: Boolean;
    FBlobStreamFormat: TInstantStreamFormat;
    FIdSize: Integer;
    FIdDataType: TInstantDataType;
  protected
    function GetCaption: string; virtual;
    procedure InitConnector(Connector: TInstantConnector); virtual;
  public
    constructor Create(Collection: TCollection); override;
    class function ConnectionTypeName: string; virtual; abstract;
    class function ConnectorClass: TInstantConnectorClass; virtual; abstract;
    function CreateConnector(AOwner: TComponent): TInstantConnector;
    function Edit: Boolean; virtual; abstract;
    property Caption: string read GetCaption;
  published
    property IsBuilt: Boolean read FIsBuilt write FIsBuilt;
    property BlobStreamFormat: TInstantStreamFormat read FBlobStreamFormat write FBlobStreamFormat default sfBinary;
    property IdDataType: TInstantDataType read FIdDataType write FIdDataType default dtString;
    property IdSize: Integer read FIdSize write FIdSize default InstantDefaultFieldSize;
  end;

  TInstantConnectionDefs = class(TInstantCollection)
  private
    function GetItems(Index: Integer): TInstantConnectionDef;
    procedure SetItems(Index: Integer; const Value: TInstantConnectionDef);
  public
    constructor Create;
    property Items[Index: Integer]: TInstantConnectionDef read GetItems write SetItems; default;
  end;

  TInstantBroker = class;

  // A TInstantCatalog that gathers its info from an existing database (through
  // a TInstantBroker). The broker knows how to read the metadata information
  // depending on which particular database is used as back-end. This is an
  // abstract class. A concrete derived class for each supported back-end
  // must be developed.
  TInstantBrokerCatalog = class(TInstantCatalog)
  private
    FBroker: TInstantBroker;
    function GetBroker: TInstantBroker;
  public
    // Creates an instance and binds it to the specified TInstantScheme object.
    // ABroker is written to the Broker property.
    constructor Create(const AScheme: TInstantScheme;
      const ABroker: TInstantBroker); virtual;
    // A reference to the broker through which the metadata info is read.
    property Broker: TInstantBroker read GetBroker;
  end;

  TInstantObjectStores = class;
  TInstantQuery = class;

  TInstantSchemeEvent = procedure(Sender: TObject; Scheme: TInstantScheme) of object;
  TInstantGenerateIdEvent = procedure(Sender: TObject; const AObject: TInstantObject;
    var Id: string) of object;

  TInstantConnector = class(TComponent)
  private
    FBroker: TInstantBroker;
    FClientList: TList;
    FObjectStores: TInstantObjectStores;
    FTransactionLevel: Integer;
    FTransactedObjectList: TList;
    FUseTransactions: Boolean;
    FBeforeBuildDatabase: TInstantSchemeEvent;
    FBlobStreamFormat: TInstantStreamFormat;
    FOnGenerateId: TInstantGenerateIdEvent;
    FIdSize: Integer;
    FIdDataType: TInstantDataType;
    procedure AbandonObjects;
    procedure ApplyTransactedObjectStates;
    procedure ClearTransactedObjects;
    procedure DoBeforeBuildDatabase(Scheme: TInstantScheme);
    function GetBroker: TInstantBroker;
    function GetClient(Index: Integer): TObject;
    function GetClientCount: Integer;
    function GetClientList: TList;
    function GetInTransaction: Boolean;
    function GetInUse: Boolean;
    function GetIsDefault: Boolean;
    function GetObjectCount: Integer;
    function GetObjects(Index: Integer): TInstantObject;
    function GetObjectStores: TInstantObjectStores;
    function GetTransactedObjectCount: Integer;
    function GetTransactedObjectList: TList;
    function GetTransactedObjects(Index: Integer): TInstantObject;
    procedure RestoreTransactedObjectStates;
    procedure SetConnected(Value: Boolean);
    procedure SetIsDefault(const Value: Boolean);
    property TransactedObjectList: TList read GetTransactedObjectList;
  protected
    function AddTransactedObject(AObject: TInstantObject): Integer;
    function CreateBroker: TInstantBroker; virtual; abstract;
    function GetConnected: Boolean; virtual;
    function GetDatabaseExists: Boolean; virtual;
    function GetDatabaseName: string; virtual;
    function GetDDLTransactionSupported: Boolean; virtual;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); virtual;
    procedure InternalConnect; virtual; abstract;
    procedure InternalCommitTransaction; virtual;
    procedure InternalCreateDatabase; virtual;
    function InternalCreateQuery: TInstantQuery; virtual;
    function InternalCreateScheme(Model: TInstantModel): TInstantScheme; virtual; abstract;
    procedure InternalDisconnect; virtual; abstract;
    function InternalGenerateId(const AObject: TInstantObject = nil): string; virtual;
    procedure InternalRollbackTransaction; virtual;
    procedure InternalStartTransaction; virtual;
    function RemoveTransactedObject(AObject: TInstantObject): Integer;
    property ClientList: TList read GetClientList;
    property TransactedObjectCount: Integer read GetTransactedObjectCount;
    property TransactedObjects[Index: Integer]: TInstantObject read GetTransactedObjects;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildDatabase(Model: TInstantModel = nil); overload;
    procedure BuildDatabase(AClasses: array of TInstantObjectClass); overload;
    procedure CommitTransaction;
    procedure Connect;
    class function ConnectionDefClass: TInstantConnectionDefClass; virtual; abstract;
    procedure CreateDatabase;
    function CreateScheme(Model: TInstantModel = nil): TInstantScheme;
    function CreateQuery: TInstantQuery;
    procedure Disconnect;
    function EnsureObjectStore(AClass: TInstantObjectClass): TInstantObjectStore;
    function GenerateId(const AObject: TInstantObject = nil): string;
    class procedure RegisterClass;
    procedure RegisterClient(Client: TObject);
    procedure RollbackTransaction;
    procedure StartTransaction;
    class procedure UnregisterClass;
    procedure UnregisterClient(Client: TObject);
    property Broker: TInstantBroker read GetBroker;
    property ClientCount: Integer read GetClientCount;
    property Clients[Index: Integer]: TObject read GetClient;
    property DatabaseExists: Boolean read GetDatabaseExists;
    property DatabaseName: string read GetDatabaseName;
    property DDLTransactionSupported: Boolean read GetDDLTransactionSupported;
    property InTransaction: Boolean read GetInTransaction;
    property InUse: Boolean read GetInUse;
    property ObjectCount: Integer read GetObjectCount;
    property Objects[Index: Integer]: TInstantObject read GetObjects;
    property ObjectStores: TInstantObjectStores read GetObjectStores;
  published
    property Connected: Boolean read GetConnected write SetConnected stored False;
    property IsDefault: Boolean read GetIsDefault write SetIsDefault default False;
    property UseTransactions: Boolean read FUseTransactions write FUseTransactions default True;
    property BeforeBuildDatabase: TInstantSchemeEvent read FBeforeBuildDatabase write FBeforeBuildDatabase;
    property BlobStreamFormat: TInstantStreamFormat read FBlobStreamFormat write FBlobStreamFormat default sfBinary;
    property OnGenerateId: TInstantGenerateIdEvent read FOnGenerateId write FOnGenerateId;
    property IdDataType: TInstantDataType read FIdDataType write FIdDataType default dtString;
    property IdSize: Integer read FIdSize write FIdSize default InstantDefaultFieldSize;
  end;

  TInstantConnectorEvent = procedure(Sender: TObject;
    Connector: TInstantConnector) of object;

  TInstantCacheNodeColor = (ncRed, ncBlack);

  TInstantCacheNode = class(TObject)
  private
    FColor: TInstantCacheNodeColor;
    FItem: TInstantObject;
    FLeft: TInstantCacheNode;
    FParent: TInstantCacheNode;
    FRight: TInstantCacheNode;
    function GetSibling: TInstantCacheNode;
  protected
    function IsRed: Boolean;
    function IsBlack: Boolean;
    property Left: TInstantCacheNode read FLeft write FLeft;
    property Right: TInstantCacheNode read FRight write FRight;
    property Item: TInstantObject read FItem write FItem;
    property Color: TInstantCacheNodeColor read FColor write FColor;
    property Parent: TInstantCacheNode read FParent write FParent;
  public
    constructor Create(AItem: TInstantObject; ALeaf,
      AParent: TInstantCacheNode; AColor: TInstantCacheNodeColor);
    function IsLeftChild: Boolean;
    function IsRightChild: Boolean;
    property Sibling: TInstantCacheNode read GetSibling;
  end;

  TInstantObjectProc = procedure(AObject: TInstantOBject) of object;

  TInstantCache = class(TObject)
  private
    FCount: Integer;
    FLeaf: TInstantCacheNode;
    FRoot: TInstantCacheNode;
    function GetItems(Index: Integer): TInstantObject;
  protected
    function CompareIds(const Id1, Id2: string): Integer;
    function CompareObjects(Object1, Object2: TInstantObject): Integer;
    procedure DeleteFixup(ANode: TInstantCacheNode);
    function FindNode(const AObjectId: string): TInstantCacheNode;
    procedure LeftRotate(ANode: TInstantCacheNode);
    function Pred(ANode: TInstantCacheNode): TInstantCacheNode;
    procedure Rebalance(ANode: TInstantCacheNode);
    procedure RightRotate(ANode: TInstantCacheNode);
    function Succ(ANode: TInstantCacheNode): TInstantCacheNode;
    property Leaf: TInstantCacheNode read FLeaf;
    property Root: TInstantCacheNode read FRoot write FRoot;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AObject: TInstantObject);
    procedure Clear;
    function Find(const AObjectId: string): TInstantObject;
    procedure ForEach(Proc: TInstantObjectProc);
    procedure Remove(AObject: TInstantObject);
    property Count: Integer read FCount;
    property Items[Index: Integer]: TInstantObject read GetItems;
  end;

  TInstantCacheEnumerator = class(TObject)
  private
    FCache: TInstantCache;
    FNode: TInstantCacheNode;
  protected
    property Node: TInstantCacheNode read FNode write FNode;
    property Cache: TInstantCache read FCache;
  public
    constructor Create(ACache: TInstantCache);
    function HasNext: Boolean;
    function Next: TInstantCacheNode;
    procedure Reset;
  end;

  TInstantObjectStore = class(TInstantCollectionItem)
  private
    FObjectClass: TInstantObjectClass;
    FConnector: TInstantConnector;
    FCache: TInstantCache;
    procedure AddToCache(AObject: TInstantObject);
    procedure CheckBroker(ABroker: TInstantBroker);
    function GetBroker: TInstantBroker;
    function GetCount: Integer;
    function GetItems(Index: Integer): TInstantObject;
    procedure RemoveFromCache(AObject: TInstantObject);
  protected
    procedure AbandonObject(AObject: TInstantObject);
    function GetName: string; override;
    property Broker: TInstantBroker read GetBroker;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure AbandonObjects;
    procedure DisposeObject(AObject: TInstantObject; ConflictAction: TInstantConflictAction);
    function Find(const AObjectId: string): TInstantObject;
    procedure ObjectDestroyed(AObject: TInstantObject);
    procedure RefreshObject(AObject: TInstantObject);
    function RetrieveObject(const AObjectId: string; AObject: TInstantObject): Boolean;
    procedure StoreObject(AObject: TInstantObject; ConflictAction: TInstantConflictAction);
    property Connector: TInstantConnector read FConnector write FConnector;
    property ObjectClass: TInstantObjectClass read FObjectClass write FObjectClass;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TInstantObject read GetItems;
  end;

  TInstantObjectStores = class(TInstantCollection)
  private
    function GetItems(Index: Integer): TInstantObjectStore;
    procedure SetItems(Index: Integer; Value: TInstantObjectStore);
  public
    constructor Create;
    function AddObjectStore: TInstantObjectStore;
    function FindObjectStore(AClass: TInstantObjectClass): TInstantObjectStore;
    property Items[Index: Integer]: TInstantObjectStore read GetItems write SetItems; default;
  end;

  TInstantQueryCommand = class(TInstantIQLCommand)
  private
    function FindAttributeMetadata(const PathText: string): TInstantAttributeMetadata;
    function GetObjectClassMetadata: TInstantClassMetadata;
  protected
    function GetResultClassName: string; override;
  public
    property ObjectClassMetadata: TInstantClassMetadata read GetObjectClassMetadata;
  end;

  TInstantQueryTranslator = class(TInstantIQLTranslator)
  private
    FQuery: TInstantQuery;
    function GetQuery: TInstantQuery;
  protected
    function CreateCommand: TInstantIQLCommand; override;
    function GetResultClassName: string; override;
  public
    constructor Create(AQuery: TInstantQuery);
    property Query: TInstantQuery read GetQuery;
  end;

  TInstantDBBuildCommandType = (ctAddTable, ctDropTable, ctAddField, ctAlterField,
    ctDropField, ctAddIndex, ctAlterIndex, ctDropIndex);

  EInstantDBBuildError = class(EInstantError);

  // Abstract; represents a step of the database build sequence.
  TInstantDBBuildCommand = class(TObject)
  private
    FEnabled: Boolean;
    FCommandType: TInstantDBBuildCommandType;
    FOldMetadata: TInstantMetadata;
    FNewMetadata: TInstantMetadata;
    FConnector: TInstantConnector;
    function GetConnector: TInstantConnector;
  protected
    // Executes the database build step.
    procedure InternalExecute; virtual; abstract;
    // Computes and returns the step's description.
    function GetDescription: string; virtual;
    // Called if an exception occurs in InternalExecute. It should do
    // whatever is needed to handle the error and return True if the exception
    // needs to be trapped, otherwise it's re-raised. The default implementation
    // just returns False.
    function InternalExecuteHandleError(const E: Exception): Boolean; virtual;
  public
    constructor Create(const ACommandType: TInstantDBBuildCommandType;
      const AConnector: TInstantConnector = nil);
    // Type of the step, usually set on creation.
    property CommandType: TInstantDBBuildCommandType read FCommandType;
    // A description of what the step will consist of.
    property Description: string read GetDescription;
    destructor Destroy; override;
    property OldMetadata: TInstantMetadata read FOldMetadata write FOldMetadata;
    property NewMetadata: TInstantMetadata read FNewMetadata write FNewMetadata;
    property Connector: TInstantConnector read GetConnector;
    // Executes the database build step.
    procedure Execute;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
  end;
  TInstantDBBuildCommandClass = class of TInstantDBBuildCommand;

  // An unsupported step. Raises an exception when executed.
  TInstantUnsupportedDBBuildCommand = class(TInstantDBBuildCommand)
  protected
    procedure InternalExecute; override;
  end;

  TInstantBroker = class(TInstantStreamable)
  private
    FConnector: TInstantConnector;
    function GetConnector: TInstantConnector;
  protected
    // Creates an instance of TInstantCatalog suited for the broker and
    // back-end database engine. Must be overridden in derived classes that
    // intend to support catalog-based functionality, like database structure
    // evolution. The predefined implementation just returns nil. Call
    // IsCatalogSupported to know if CreateCatalog will return an instance of
    // the catalog object or nil.
    function CreateCatalog(const AScheme: TInstantScheme): TInstantCatalog; virtual;
    function GetDatabaseName: string; virtual;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); virtual;
    function InternalCreateQuery: TInstantQuery; virtual;
    function InternalDisposeObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction): Boolean; virtual; abstract;
    function InternalRetrieveObject(AObject: TInstantObject; const AObjectId: string;
      ConflictAction: TInstantConflictAction): Boolean; virtual; abstract;
    function InternalStoreObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction): Boolean; virtual; abstract;
  public
    constructor Create(AConnector: TInstantConnector); virtual;
    destructor Destroy; override;
    // Legacy database building code, to be removed when all brokers will have
    // catalogs.
    procedure BuildDatabase(Scheme: TInstantScheme);
    // Creates a database build command object that can perform the build
    // operation represented by CommandType. The predefined implementation
    // creates an instance of TInstantUnsupportedDBBuildCommand, which just raises
    // an exception when executed. Derived classes should create and return
    // instances of specific TInstantDBBuildCommand and fall back to inherited
    // for unsupported values of CommandType.
    function CreateDBBuildCommand(
      const CommandType: TInstantDBBuildCommandType): TInstantDBBuildCommand; virtual;
    function CreateQuery: TInstantQuery;
    function DisposeObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction): Boolean;
    // Creates and returns a TInstantScheme object that represents the current
    // database scheme (which may differ from the model-derived scheme).
    // If the broker doesn't have a catalog, calling this method will raise
    // an exception. Call IsCatalogSupported if you need to know in advance
    // whether you can safely call ReadDatabaseScheme or not.
    function ReadDatabaseScheme(
      const AWarningEventHandler: TInstantWarningEvent): TInstantScheme; virtual;
    // Returns True if the broker supports creating a valid catalog. If this
    // method returns False, it means that calling CreateCatalog will yield nil,
    // and calling ReadDatabaseSchema will raise an exception.
    function IsCatalogSupported: Boolean;
    function RetrieveObject(AObject: TInstantObject; const AObjectId: string;
      ConflictAction: TInstantConflictAction): Boolean;
    procedure SetObjectUpdateCount(AObject: TInstantObject; Value: Integer);
    function StoreObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction): Boolean;
    property Connector: TInstantConnector read GetConnector;
    property DatabaseName: string read GetDatabaseName;
  end;

  TInstantQuery = class(TPersistent)
  private
    FCommand: string;
    FConnector: TInstantConnector;
    FMaxCount: Integer;
    function GetConnector: TInstantConnector;
    function GetObjectCount: Integer;
    function GetObjects(Index: Integer): TObject;
    procedure SetActive(Value: Boolean);
    procedure SetCommand(const Value: string);
  protected
    function GetActive: Boolean; virtual;
    function GetObjectClass: TClass; virtual;
    function GetObjectClassName: string; virtual;
    function GetParams: TParams; virtual;
    function InternalAddObject(AObject: TObject): Integer; virtual;
    procedure InternalClose; virtual;
    procedure InternalFetchParams(AParams: TParams); virtual;
    function InternalGetObjectCount: Integer; virtual;
    function InternalGetObjects(Index: Integer): TObject; virtual;
    function InternalIndexOfObject(AObject: TObject): Integer; virtual;
    procedure InternalInsertObject(Index: Integer; AObject: TObject); virtual;
    procedure InternalOpen; virtual;
    procedure InternalRefreshObjects; virtual;
    procedure InternalReleaseObject(AObject: TObject); virtual;
    function InternalRemoveObject(AObject: TObject): Integer; virtual;
    function ObjectFetched(Index: Integer): Boolean; virtual;
    procedure SetParams(Value: TParams); virtual;
    procedure TranslateCommand; virtual;
  public
    constructor Create(AConnector: TInstantConnector); virtual;
    function AddObject(AObject: TObject): Integer;
    procedure ApplyChanges;
    procedure Close;
    procedure FetchParams(const ACommandText: string; AParams: TParams);
    function HasChangedObject: Boolean;
    function HasObject(AObject: TObject): Boolean;
    function IndexOfObject(AObject: TObject): Integer;
    procedure InsertObject(Index: Integer; AObject: TObject);
    procedure Open;
    procedure RefreshObjects;
    procedure ReleaseObject(AObject: TObject);
    function RemoveObject(AObject: TObject): Integer;
    property Active: Boolean read GetActive write SetActive;
    property Command: string read FCommand write SetCommand;
    property Connector: TInstantConnector read GetConnector;
    property MaxCount: Integer read FMaxCount write FMaxCount;
    property ObjectClass: TClass read GetObjectClass;
    property ObjectClassName: string read GetObjectClassName;
    property ObjectCount: Integer read GetObjectCount;
    property Objects[Index: Integer]: TObject read GetObjects;
    property Params: TParams read GetParams write SetParams;
  end;

  TInstantCustomRelationalBroker = class;

  TInstantGetDataSetEvent = procedure(Sender: TObject; const CommandText: string; var DataSet: TDataset) of object;
  TInstantInitDataSetEvent = procedure(Sender: TObject; const CommandText: string; DataSet: TDataSet) of object;

  TInstantRelationalConnector = class(TInstantConnector)
  private
    FOnGetDataSet: TInstantGetDataSetEvent;
    FOnInitDataSet: TInstantInitDataSetEvent;
  protected
    procedure DoGetDataSet(const CommandText: string; var DataSet: TDataSet);
    procedure DoInitDataSet(const CommandText: string; DataSet: TDataSet);
    function GetBroker: TInstantCustomRelationalBroker;
    procedure GetDataSet(const CommandText: string; var DataSet: TDataSet); virtual;
    function GetDBMSName: string; virtual;
    procedure InitDataSet(const CommandText: string; DataSet: TDataSet); virtual;
    function InternalCreateScheme(Model: TInstantModel): TInstantScheme; override;
  public
    property Broker: TInstantCustomRelationalBroker read GetBroker;
    property DBMSName: string read GetDBMSName;
  published
    property OnGetDataSet: TInstantGetDataSetEvent read FOnGetDataSet write FOnGetDataSet;
    property OnInitDataSet: TInstantInitDataSetEvent read FOnInitDataSet write FOnInitDataSet;
  end;

  TInstantCustomRelationalQuery = class;

  TInstantRelationalTranslatorClass = class of TInstantRelationalTranslator;

  TInstantRelationalTranslator = class(TInstantQueryTranslator)
  private
    FCriteriaList: TStringList;
    FTablePathList: TStringList;
    procedure AddJoin(const FromPath, FromField, ToPath, ToField: string);
    function ConcatPath(const APathText, AttribName: string): string;
    procedure DestroyCriteriaList;
    procedure DestroyTablePathList;
    function ExtractTarget(const PathStr: string): string;
    function RootAttribToFieldName(const AttribName: string): string;
    function GetClassTablePath: string;
    function GetCriteriaCount: Integer;
    function GetCriteriaList: TStringList;
    function GetCriterias(Index: Integer): string;
    function GetObjectClassMetadata: TInstantClassMetadata;
    function GetQuery: TInstantCustomRelationalQuery;
    function GetTablePathAliases(Index: Integer): string;
    function GetTablePathCount: Integer;
    function GetTablePathList: TStringList;
    function GetTablePaths(Index: Integer): string;
    function PathToTablePath(const PathText: string): string;
    function PathToTarget(const PathText: string;
      out TablePath, FieldName: string): TInstantAttributeMetadata;
    function Qualify(const TablePath, FieldName: string): string;
    function QualifyPath(const PathText: string): string;
    function ReplaceWildcard(const Str: string): string;
    function TablePathToAlias(const TablePath: string): string;
    procedure WriteAnd(Writer: TInstantIQLWriter);
    function WriteCriterias(Writer: TInstantIQLWriter; IncludeWhere: Boolean): Boolean;
    procedure WriteTables(Writer: TInstantIQLWriter);
    property CriteriaList: TStringList read GetCriteriaList;
    property TablePathList: TStringList read GetTablePathList;
    function GetConnector: TInstantRelationalConnector;
  protected
    function AddCriteria(const Criteria: string): Integer;
    function AddTablePath(const TablePath: string): Integer;
    procedure BeforeTranslate; override;
    procedure Clear; override;
    procedure CollectObjects(AObject: TInstantIQLObject;
      AClassType: TInstantIQLObjectClass; AList: TList);
    procedure CollectPaths(AObject: TInstantIQLObject; APathList: TList);
    function GetDelimiters: string; virtual;
    function GetQuote: Char; virtual;
    function GetWildcard: string; virtual;
    function HasConnector: Boolean;
    function IncludeOrderFields: Boolean; virtual;
    function IndexOfCriteria(const Criteria: string): Integer;
    function IndexOfTablePath(const TablePath: string): Integer;
    function InternalGetObjectClassMetadata: TInstantClassMetadata; virtual;
    function IsRootAttribute(const AttributeName: string): Boolean;
    function IsPrimary(AObject: TInstantIQLObject): Boolean;
    procedure MakeJoins(Path: TInstantIQLPath);
    procedure MakeTablePaths(Path: TInstantIQLPath);
    function TranslateClassRef(ClassRef: TInstantIQLClassRef; Writer: TInstantIQLWriter): Boolean; virtual;
    function TranslateClause(Clause: TInstantIQLClause; Writer: TInstantIQLWriter): Boolean; virtual;
    function TranslateConstant(Constant: TInstantIQLConstant; Writer: TInstantIQLWriter): Boolean; virtual;
    function TranslateFunction(AFunction: TInstantIQLFunction; Writer: TInstantIQLWriter): Boolean; virtual;
    function TranslateFunctionName(const FunctionName: string; Writer: TInstantIQLWriter): Boolean; virtual;
    function TranslateKeyword(const Keyword: string; Writer: TInstantIQLWriter): Boolean; override;
    function TranslateObject(AObject: TInstantIQLObject; Writer: TInstantIQLWriter): Boolean; override;
    function TranslatePath(Path: TInstantIQLPath; Writer: TInstantIQLWriter): Boolean; virtual;
    function TranslateSpecifier(Specifier: TInstantIQLSpecifier; Writer: TInstantIQLWriter): Boolean; virtual;
    property ClassTablePath: string read GetClassTablePath;
    property Connector: TInstantRelationalConnector read GetConnector;
    property CriteriaCount: Integer read GetCriteriaCount;
    property Criterias[Index: Integer]: string read GetCriterias;
    property Delimiters: string read GetDelimiters;
    property ObjectClassMetadata: TInstantClassMetadata read GetObjectClassMetadata;
    property Quote: Char read GetQuote;
    property TablePathAliases[Index: Integer]: string read GetTablePathAliases;
    property TablePathCount: Integer read GetTablePathCount;
    property TablePaths[Index: Integer]: string read GetTablePaths;
    property Wildcard: string read GetWildcard;
  public
    destructor Destroy; override;
    function QuoteString(const Str: string): string;
    property Query: TInstantCustomRelationalQuery read GetQuery;
  end;

  PInstantOperationInfo = ^TInstantOperationInfo;
  TInstantOperationInfo = record
    Success: Boolean;
    Conflict: Boolean;
  end;

  TInstantStatement = class
  private
    FStatementImplementation: TComponent;
  public
    constructor Create(const AStatementImplementation: TComponent);
    destructor Destroy; override;
    property StatementImplementation: TComponent read FStatementImplementation;
  end;

  TInstantStatementCache = class(TComponent)
  private
    FStatements: TStringList;
    FCapacity: Integer;
    procedure DeleteStatement(const Index: Integer);
    procedure DeleteAllStatements;
    procedure Shrink;
    procedure SetCapacity(const Value: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Capacity: Integer read FCapacity write SetCapacity;
    destructor Destroy; override;
    function GetStatement(const StatementText: string): TInstantStatement;
    function AddStatement(const StatementText: string;
      const StatementImplementation: TComponent): Integer;
    function RemoveStatement(const StatementText: string): Boolean;
  end;

  TInstantCustomResolver = class;

  TInstantBrokerOperation = procedure(AObject: TInstantObject; const AObjectId: string;
    Map: TInstantAttributeMap; ConflictAction: TInstantConflictAction = caFail; Info: PInstantOperationInfo = nil) of object;

  TInstantCustomRelationalBroker = class(TInstantBroker)
  private
    FStatementCache: TInstantStatementCache;
    FStatementCacheCapacity: Integer;
    procedure DisposeMap(AObject: TInstantObject; const AObjectId: string;
      Map: TInstantAttributeMap; ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo);
    function GetConnector: TInstantRelationalConnector;
    function PerformOperation(AObject: TInstantObject; const AObjectId: string;
      OperationType: TInstantOperationType; Operation: TInstantBrokerOperation;
      ConflictAction: TInstantConflictAction): Boolean;
    procedure RetrieveMap(AObject: TInstantObject; const AObjectId: string;
      Map: TInstantAttributeMap; ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo);
    procedure StoreMap(AObject: TInstantObject; const AObjectId: string;
      Map: TInstantAttributeMap; ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo);
    function GetStatementCache: TInstantStatementCache;
    procedure SetStatementCacheCapacity(const Value: Integer);
  protected
    property StatementCache: TInstantStatementCache read GetStatementCache;
    function EnsureResolver(Map: TInstantAttributeMap): TInstantCustomResolver; virtual; abstract;
    function GetDBMSName: string; virtual;
    function GetSQLDelimiters: string; virtual;
    function GetSQLQuote: Char; virtual;
    function GetSQLWildcard: string; virtual;
    function InternalDisposeObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction): Boolean; override;
    function InternalRetrieveObject(AObject: TInstantObject; const AObjectId: string;
      ConflictAction: TInstantConflictAction): Boolean; override;
    function InternalStoreObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction): Boolean; override;
  public
    constructor Create(AConnector: TInstantConnector); override;
    destructor Destroy; override;
    function Execute(const AStatement: string; AParams: TParams = nil): Integer; virtual;
    property Connector: TInstantRelationalConnector read GetConnector;
    property DBMSName: string read GetDBMSName;
    property SQLDelimiters: string read GetSQLDelimiters;
    property SQLQuote: Char read GetSQLQuote;
    property SQLWildcard: string read GetSQLWildCard;
    property StatementCacheCapacity: Integer read FStatementCacheCapacity
      write SetStatementCacheCapacity;
  end;

  EInstantConflict = class(EInstantError)
  end;

  EInstantKeyViolation = class(EInstantConflict)
  end;

  TInstantCustomResolver = class(TInstantStreamable)
  private
    FBroker: TInstantCustomRelationalBroker;
  protected
    function KeyViolation(AObject: TInstantObject; const AObjectId: string; E: Exception): EInstantKeyViolation;
    procedure InternalDisposeMap(AObject: TInstantObject; Map: TInstantAttributeMap;
      ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo); virtual;
    procedure InternalRetrieveMap(AObject: TInstantObject; const AObjectId: string;
      Map: TInstantAttributeMap; ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo); virtual;
    procedure InternalStoreMap(AObject: TInstantObject; Map: TInstantAttributeMap;
      ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo); virtual;
  public
    constructor Create(ABroker: TInstantCustomRelationalBroker);
    procedure DisposeMap(AObject: TInstantObject; Map: TInstantAttributeMap;
      ConflictAction: TInstantConflictAction;Info: PInstantOperationInfo);
    procedure RetrieveMap(AObject: TInstantObject; const AObjectId: string;
      Map: TInstantAttributeMap; ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo);
    procedure StoreMap(AObject: TInstantObject; Map: TInstantAttributeMap;
      ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo);
    property Broker: TInstantCustomRelationalBroker read FBroker;
  end;

  TInstantCustomRelationalQueryClass = class of TInstantCustomRelationalQuery;

  TInstantCustomRelationalQuery = class(TInstantQuery)
  private
    function GetConnector: TInstantRelationalConnector;
  protected
    function GetStatement: string; virtual;
    procedure InternalGetInstantObjectRefs(List: TInstantObjectReferenceList); virtual;
    procedure InternalRefreshObjects; override;
    procedure SetStatement(const Value: string); virtual;
    procedure TranslateCommand; override;
    class function TranslatorClass: TInstantRelationalTranslatorClass; virtual;
  public
    function CreateTranslator: TInstantRelationalTranslator;
    property Statement: string read GetStatement write SetStatement;
    property Connector: TInstantRelationalConnector read GetConnector;
  end;

  TInstantRelationalConnectionDef = class(TInstantConnectionDef)
  end;

  TInstantConnectionBasedConnector = class(TInstantRelationalConnector)
  private
    FConnection: TCustomConnection;
    FLoginPrompt: Boolean;
    procedure DoAfterConnectionChange;
    procedure DoBeforeConnectionChange;
    function GetConnection: TCustomConnection;
    function GetLoginPrompt: Boolean;
    procedure SetConnection(Value: TCustomConnection);
    procedure SetLoginPrompt(const Value: Boolean);
  protected
    procedure AssignLoginOptions; virtual;
    procedure AfterConnectionChange; virtual;
    procedure BeforeConnectionChange; virtual;
    procedure CheckConnection;
    function GetConnected: Boolean; override;
    procedure InternalConnect; override;
    procedure InternalDisconnect; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property Connection: TCustomConnection read GetConnection write SetConnection;
    function HasConnection: Boolean;
    constructor Create(AOwner: TComponent); override;
  published
    property LoginPrompt: Boolean read GetLoginPrompt write SetLoginPrompt default True;
  end;

  TInstantConnectionBasedConnectionDef = class(TInstantRelationalConnectionDef)
  private
    FLoginPrompt: Boolean;
  protected
    function CreateConnection(AOwner: TComponent): TCustomConnection; virtual; abstract;
    procedure InitConnector(Connector: TInstantConnector); override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt default True;
  end;

  TInstantNavigationalResolver = class;
  
  TInstantNavigationalBroker = class(TInstantCustomRelationalBroker)
  private
    FResolverList: TObjectList;
    function GetResolverCount: Integer;
    function GetResolverList: TObjectList;
    function GetResolvers(Index: Integer): TInstantnavigationalResolver;
    property ResolverList: TObjectList read GetResolverList;
  protected
    function CreateResolver(const TableName: string): TInstantNavigationalResolver; virtual; abstract;
    function EnsureResolver(Map: TInstantAttributeMap): TInstantCustomResolver; override;
    function FindResolver(const TableName: string): TInstantNavigationalResolver;
    property ResolverCount: Integer read GetResolverCount;
    property Resolvers[Index: Integer]: TInstantNavigationalResolver read GetResolvers;
  public
    destructor Destroy; override;
  end;

  //Backwards compatibility
  TInstantRelationalBroker = TInstantNavigationalBroker;

  TInstantNavigationalResolverOperation = procedure(AObject: TInstantObject; AttributeMetadata: TInstantAttributeMetadata) of object;

  PObjectRow = ^TObjectRow;
  TObjectRow = record
    Row: Integer;
    Instance: TObject;
  end;

  TInstantNavigationalResolverClass = class of TInstantNavigationalResolver;

  TInstantNavigationalResolver = class(TInstantCustomResolver)
  private
    FDataSet: TDataSet;
    FFreeDataSet: Boolean;
    FTableName: string;
    function CheckConflict(AObject: TInstantObject; const AObjectId: string;
      ConflictAction: TInstantConflictAction): Boolean;
    procedure ClearAttribute(AObject: TInstantObject;
      AttributeMetadata: TInstantAttributeMetadata);
    function FieldByName(const FieldName: string): TField;
    procedure FreeDataSet;
    function GetBroker: TInstantNavigationalBroker;
    function GetDataSet: TDataSet;
    procedure PerformOperation(AObject: TInstantObject;
      Map: TInstantAttributeMap; Operation: TInstantNavigationalResolverOperation);
    procedure ReadAttribute(AObject: TInstantObject;
      AttributeMetadata: TInstantAttributeMetadata);
    procedure ResetAttribute(AObject: TInstantObject;
      AttributeMetadata: TInstantAttributeMetadata);
    procedure SetDataSet(Value: TDataset);
    procedure WriteAttribute(AObject: TInstantObject;
      AttributeMetadata: TInstantAttributeMetadata);
  protected
    procedure Append; virtual;
    procedure Cancel; virtual;
    procedure ClearBlob(Attribute: TInstantBlob); virtual;
    procedure ClearBoolean(Attribute: TInstantBoolean); virtual;
    procedure ClearDateTime(Attribute: TInstantDateTime); virtual;
    procedure ClearInteger(Attribute: TInstantInteger); virtual;
    procedure ClearFloat(Attribute: TInstantFloat); virtual;
    procedure ClearCurrency(Attribute: TInstantCurrency); virtual;
    procedure ClearMemo(Attribute: TInstantMemo); virtual;
    procedure ClearPart(Attribute: TInstantPart); virtual;
    procedure ClearParts(Attribute: TInstantParts); virtual;
    procedure ClearReference(Attribute: TInstantReference); virtual;
    procedure ClearReferences(Attribute: TInstantReferences); virtual;
    procedure ClearString(Attribute: TInstantString); virtual;
    procedure Close; virtual;
    function CreateDataSet: TDataSet; virtual; abstract;
    function CreateLocateVarArray(const AObjectClassName, AObjectId: string): Variant;
    procedure Delete; virtual;
    procedure Edit; virtual;
    function FieldHasObjects(Field: TField): Boolean; virtual;
    procedure InternalDisposeMap(AObject: TInstantObject; Map: TInstantAttributeMap;
      ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo); override;
    procedure InternalRetrieveMap(AObject: TInstantObject; const AObjectId: string;
      Map: TInstantAttributeMap; ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo); override;
    procedure InternalStoreMap(AObject: TInstantObject; Map: TInstantAttributeMap;
      ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo); override;
    function Locate(const AObjectClassName, AObjectId: string): Boolean; virtual; abstract;
    procedure Open; virtual;
    procedure Post; virtual;
    procedure ReadBlob(Attribute: TInstantBlob); virtual;
    procedure ReadBoolean(Attribute: TInstantBoolean); virtual;
    procedure ReadDateTime(Attribute: TInstantDateTime); virtual;
    procedure ReadInteger(Attribute: TInstantInteger); virtual;
    procedure ReadFloat(Attribute: TInstantFloat); virtual;
    procedure ReadCurrency(Attribute: TInstantCurrency); virtual;
    procedure ReadMemo(Attribute: TInstantMemo); virtual;
    procedure ReadPart(Attribute: TInstantPart); virtual;
    procedure ReadParts(Attribute: TInstantParts); virtual;
    procedure ReadReference(Attribute: TInstantReference); virtual;
    procedure ReadReferences(Attribute: TInstantReferences); virtual;
    procedure ReadString(Attribute: TInstantString); virtual;
    procedure ResetAttributes(AObject: TInstantObject; Map: TInstantAttributeMap);
    procedure SetObjectUpdateCount(AObject: TInstantObject; Value: Integer);
    function TranslateError(AObject: TInstantObject; E: Exception): Exception; virtual;
    procedure WriteBlob(Attribute: TInstantBlob); virtual;
    procedure WriteBoolean(Attribute: TInstantBoolean); virtual;
    procedure WriteDateTime(Attribute: TInstantDateTime); virtual;
    procedure WriteFloat(Attribute: TInstantFloat); virtual;
    procedure WriteCurrency(Attribute: TInstantCurrency); virtual;
    procedure WriteInteger(Attribute: TInstantInteger); virtual;
    procedure WriteMemo(Attribute: TInstantMemo); virtual;
    procedure WritePart(Attribute: TInstantPart); virtual;
    procedure WriteParts(Attribute: TInstantParts); virtual;
    procedure WriteReference(Attribute: TInstantReference); virtual;
    procedure WriteReferences(Attribute: TInstantReferences); virtual;
    procedure WriteString(Attribute: TInstantString); virtual;
    property DataSet: TDataset read GetDataSet write SetDataSet;
  public
    constructor Create(ABroker: TInstantNavigationalBroker;
      const ATableName: string);
    destructor Destroy; override;
    property Broker: TInstantNavigationalBroker read GetBroker;
    property TableName: string read FTableName;
  end;

  //Backwards compatibility
  TInstantResolver = TInstantNavigationalResolver;

  TInstantNavigationalQuery = class(TInstantCustomRelationalQuery)
  private
    FObjectRowList: TList;
    function CreateObject(Row: Integer): TObject;
    procedure DestroyObjectRowList;
    function GetObjectRowList: TList;
    function GetObjectRowCount: Integer;
    function GetObjectRows(Index: Integer): PObjectRow;
    procedure InitObjectRows(List: TList; FromIndex, ToIndex: Integer);
    property ObjectRowList: TList read GetObjectRowList;
  protected
    function GetActive: Boolean; override;
    function GetDataSet: TDataSet; virtual;
    function GetRowCount: Integer; virtual;
    function GetRowNumber: Integer; virtual;
    function InternalAddObject(AObject: TObject): Integer; override;
    procedure InternalClose; override;
    procedure InternalGetInstantObjectRefs(List: TInstantObjectReferenceList); override;
    function InternalGetObjectCount: Integer; override;
    function InternalGetObjects(Index: Integer): TObject; override;
    function InternalIndexOfObject(AObject: TObject): Integer; override;
    procedure InternalInsertObject(Index: Integer; AObject: TObject); override;
    procedure InternalOpen; override;
    procedure InternalRefreshObjects; override;
    procedure InternalReleaseObject(AObject: TObject); override;
    function InternalRemoveObject(AObject: TObject): Integer; override;
    function IsSequenced: Boolean; virtual;
    function ObjectFetched(Index: Integer): Boolean; override;
    function RecNoOfObject(AObject: TInstantObject): Integer; virtual;
    procedure SetRowNumber(Value: Integer); virtual;
    property DataSet: TDataSet read GetDataSet;
    property ObjectRowCount: Integer read GetObjectRowCount;
    property ObjectRows[Index: Integer]: PObjectRow read GetObjectRows;
  public
    destructor Destroy; override;
    property RowCount: Integer read GetRowCount;
    property RowNumber: Integer read GetRowNumber write SetRowNumber;
  end;

  //Backwards compatibility
  TInstantRelationalQuery = TInstantNavigationalQuery;

  TInstantSQLBroker = class;

  // A TInstantBrokerCatalog that works with a SQL broker only.
  TInstantSQLBrokerCatalog = class(TInstantBrokerCatalog)
  private
    function GetBroker: TInstantSQLBroker;
  public
    property Broker: TInstantSQLBroker read GetBroker;
  end;

  TInstantStringFunc = function(const S: string): string of object;

  TInstantSQLGeneratorClass = class of TInstantSQLGenerator;

  TInstantSQLGenerator = class(TObject)
  private
    FBroker: TInstantSQLBroker;
  protected
    function BuildList(Map: TInstantAttributeMap; Additional: array of string;
      StringFunc: TInstantStringFunc = nil; const Delimiter: string = ','): string;
    function BuildAssignment(const AName: string): string;
    function BuildAssignmentList(Map: TInstantAttributeMap;
      Additional: array of string): string;
    function BuildConcurrencyCriteria: string;
    function BuildPersistentIdCriteria: string;
    function BuildFieldList(Map: TInstantAttributeMap;
      Additional: array of string): string; overload;
    function BuildFieldList(const S: string): string; overload;
    function BuildParam(const AName: string): string; virtual;
    function BuildParamList(Map: TInstantAttributeMap;
      Additional: array of string): string;
    function BuildWhereStr(Fields: array of string): string;
    function EmbraceField(const FieldName: string): string; virtual;
    function EmbraceTable(const TableName: string): string; virtual;
    function GetDelimiters: string; virtual;
    function InternalGenerateAddFieldSQL(Metadata: TInstantFieldMetadata): string; virtual;
    function InternalGenerateAlterFieldSQL(OldMetadata, NewMetadata: TInstantFieldMetadata): string; virtual;
    function InternalGenerateCreateIndexSQL(Metadata: TInstantIndexMetadata): string; virtual;
    function InternalGenerateCreateTableSQL(Metadata: TInstantTableMetadata): string; virtual;
    function InternalGenerateDeleteConcurrentSQL(Map: TInstantAttributeMap): string; virtual;
    function InternalGenerateDeleteSQL(Map: TInstantAttributeMap): string; virtual;
    function InternalGenerateDeleteExternalSQL(Map: TInstantAttributeMap): string; virtual;
    function InternalGenerateDropFieldSQL(Metadata: TInstantFieldMetadata): string; virtual;
    function InternalGenerateDropIndexSQL(Metadata: TInstantIndexMetadata): string; virtual;
    function InternalGenerateDropTableSQL(Metadata: TInstantTableMetadata): string; virtual;
    function InternalGenerateInsertSQL(Map: TInstantAttributeMap): string; virtual;
    function InternalGenerateInsertExternalSQL(Map: TInstantAttributeMap): string; virtual;
    function InternalGenerateSelectSQL(Map: TInstantAttributeMap): string; virtual;
    function InternalGenerateSelectExternalSQL(Map: TInstantAttributeMap): string; virtual;
    function InternalGenerateSelectExternalPartSQL(Map: TInstantAttributeMap): string; virtual;
    function InternalGenerateSelectTablesSQL: string; virtual;
    function InternalGenerateUpdateConcurrentSQL(Map: TInstantAttributeMap): string; virtual;
    function InternalGenerateUpdateFieldCopySQL(OldMetadata, NewMetadata:
        TInstantFieldMetadata): string; virtual;
    function InternalGenerateUpdateSQL(Map: TInstantAttributeMap): string; virtual;
    property Delimiters: string read GetDelimiters;
    property Broker: TInstantSQLBroker read FBroker;
  public
    constructor Create(ABroker: TInstantSQLBroker);
    function GenerateAddFieldSQL(Metadata: TInstantFieldMetadata): string;
    function GenerateAlterFieldSQL(OldMetadata, NewMetadata: TInstantFieldMetadata): string;
    function GenerateCreateIndexSQL(Metadata: TInstantIndexMetadata): string;
    function GenerateCreateTableSQL(Metadata: TInstantTableMetadata): string;
    function GenerateDeleteConcurrentSQL(Map: TInstantAttributeMap): string;
    function GenerateDeleteSQL(Map: TInstantAttributeMap): string;
    function GenerateDeleteExternalSQL(Map: TInstantAttributeMap): string;
    function GenerateDropFieldSQL(Metadata: TInstantFieldMetadata): string;
    function GenerateDropIndexSQL(Metadata: TInstantIndexMetadata): string;
    function GenerateDropTableSQL(Metadata: TInstantTableMetadata): string;
    function GenerateInsertSQL(Map: TInstantAttributeMap): string;
    function GenerateInsertExternalSQL(Map: TInstantAttributeMap): string;
    function GenerateSelectSQL(Map: TInstantAttributeMap): string;
    function GenerateSelectExternalSQL(Map: TInstantAttributeMap): string;
    function GenerateSelectExternalPartSQL(Map: TInstantAttributeMap): string;
    function GenerateSelectTablesSQL: string;
    function GenerateUpdateConcurrentSQL(Map: TInstantAttributeMap): string;
    function GenerateUpdateFieldCopySQL(OldMetadata, NewMetadata:
        TInstantFieldMetadata): string;
    function GenerateUpdateSQL(Map: TInstantAttributeMap): string;
  end;

  TInstantSQLResolver = class;

  TInstantSQLBroker = class(TInstantCustomRelationalBroker)
  private
    FGenerator: TInstantSQLGenerator;
    FResolverList: TObjectList;
    function GetResolverList: TObjectList;
    function GetResolverCount: Integer;
    function GetResolvers(Index: Integer): TInstantSQLResolver;
    function GetGenerator: TInstantSQLGenerator;
  protected
    function CreateResolver(Map: TInstantAttributeMap): TInstantSQLResolver; virtual; abstract;
    function EnsureResolver(AMap: TInstantAttributeMap): TInstantCustomResolver; override;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); override;
    property ResolverList: TObjectList read GetResolverList;
    procedure AssignDataSetParams(DataSet : TDataSet; AParams: TParams); virtual;
    function CreateDataSet(const AStatement: string; AParams: TParams = nil): TDataSet; virtual; abstract;
  public
    destructor Destroy; override;
    function AcquireDataSet(const AStatement: string; AParams: TParams = nil): TDataSet; virtual;
    procedure ReleaseDataSet(const ADataSet: TDataSet); virtual;
    function DataTypeToColumnType(DataType: TInstantDataType;
      Size: Integer): string; virtual; abstract;
    function FindResolver(AMap: TInstantAttributeMap): TInstantSQLResolver;
    class function GeneratorClass: TInstantSQLGeneratorClass; virtual;
    property Generator: TInstantSQLGenerator read GetGenerator;
    property ResolverCount: Integer read GetResolverCount;
    property Resolvers[Index: Integer]: TInstantSQLResolver read GetResolvers;
  end;

  TInstantSQLResolver = class(TInstantCustomResolver)
  private
    FMap: TInstantAttributeMap;
    FDeleteSQL: string;
    FDeleteConcurrentSQL: string;
    FInsertSQL: string;
    FSelectSQL: string;
    FUpdateSQL: string;
    FUpdateConcurrentSQL: string;
    FSelectExternalSQL: string;
    FSelectExternalPartSQL: string;
    FDeleteExternalSQL: string;
    FInsertExternalSQL: string;
    procedure AddIntegerParam(Params: TParams; const ParamName: string; Value: Integer);
    procedure AddStringParam(Params: TParams; const ParamName, Value: string);
    // Adds an "Id" param, whose data type and size depends on connector settings.
    procedure AddIdParam(Params: TParams; const ParamName, Value: string);
    procedure CheckConflict(Info: PInstantOperationInfo; AObject: TInstantObject);
    function ExecuteStatement(const AStatement: string; AParams: TParams;
      Info: PInstantOperationInfo; ConflictAction: TInstantConflictAction;
      AObject: TInstantObject): Integer;
    function GetDeleteConcurrentSQL: string;
    function GetDeleteSQL: string;
    function GetInsertSQL: string;
    function GetSelectSQL: string;
    function GetUpdateConcurrentSQL: string;
    function GetUpdateSQL: string;
    function GetBroker: TInstantSQLBroker;
    function GetSelectExternalSQL: string;
    function GetSelectExternalPartSQL: string;
    function GetDeleteExternalSQL: string;
    function GetInsertExternalSQL: string;
  protected
    procedure AddAttributeParam(Attribute: TInstantAttribute;
      Params: TParams); virtual;
    procedure AddAttributeParams(Params: TParams; AObject: TInstantObject;
      Map: TInstantAttributeMap);
    procedure AddBaseParams(Params: TParams; AClassName, AObjectId: string;
      AUpdateCount: Integer = -1);
    procedure AddConcurrencyParam(Params: TParams; AUpdateCount: Integer);
    function AddParam(Params: TParams; const ParamName: string;
      ADataType: TFieldType): TParam;
    procedure AddPersistentIdParam(Params: TParams; APersistentId: string);
    procedure InternalDisposeMap(AObject: TInstantObject; Map: TInstantAttributeMap;
      ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo); override;
    procedure InternalRetrieveMap(AObject: TInstantObject; const AObjectId: string;
      Map: TInstantAttributeMap; ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo); override;
    procedure InternalStoreMap(AObject: TInstantObject; Map: TInstantAttributeMap;
      ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo); override;
    procedure ReadAttribute(AObject: TInstantObject; const AObjectId: string;
      AttributeMetadata: TInstantAttributeMetadata; DataSet: TDataSet); virtual;
    procedure ReadAttributes(AObject: TInstantObject; const AObjectId: string;
      Map: TInstantAttributeMap; DataSet: TDataSet);
    function ReadBlobField(DataSet: TDataSet; const FieldName: string): string; virtual;
    function ReadBooleanField(DataSet: TDataSet; const FieldName: string): Boolean; virtual;
    function ReadDateTimeField(DataSet: TDataSet; const FieldName: string): TDateTime; virtual;
    function ReadFloatField(DataSet: TDataSet; const FieldName: string): Double; virtual;
    function ReadCurrencyField(DataSet: TDataSet; const FieldName: string): Currency; virtual;
    function ReadIntegerField(DataSet: TDataSet; const FieldName: string): Integer; virtual;
    function ReadMemoField(DataSet: TDataSet; const FieldName: string): string; virtual;
    function ReadStringField(DataSet: TDataSet; const FieldName: string): string; virtual;
    procedure RemoveConcurrencyParam(Params: TParams);
    procedure RemovePersistentIdParam(Params: TParams);
    function TranslateError(AObject: TInstantObject;
      E: Exception): Exception; virtual;
  public
    constructor Create(ABroker: TInstantSQLBroker; AMap: TInstantAttributeMap);
    property Broker: TInstantSQLBroker read GetBroker;
    property DeleteConcurrentSQL: string read GetDeleteConcurrentSQL write FDeleteConcurrentSQL;
    property DeleteSQL: string read GetDeleteSQL write FDeleteSQL;
    property DeleteExternalSQL: string read GetDeleteExternalSQL write FDeleteExternalSQL;
    property InsertSQL: string read GetInsertSQL write FInsertSQL;
    property InsertExternalSQL: string read GetInsertExternalSQL write FInsertExternalSQL;
    property Map: TInstantAttributeMap read FMap;
    property SelectSQL: string read GetSelectSQL write FSelectSQL;
    property SelectExternalSQL: string read GetSelectExternalSQL write FSelectExternalSQL;
    property SelectExternalPartSQL: string read GetSelectExternalPartSQL write FSelectExternalPartSQL;
    property UpdateConcurrentSQL: string read GetUpdateConcurrentSQL write FUpdateConcurrentSQL;
    property UpdateSQL: string read GetUpdateSQL write FUpdateSQL;
  end;

  TInstantSQLQuery = class(TInstantCustomRelationalQuery)
  private
    FObjectReferenceList: TInstantObjectReferenceList;
    FParamsObject: TParams;
    FStatement: string;
    procedure DestroyObjectReferenceList;
    function GetObjectReferenceCount: Integer;
    function GetObjectReferenceList: TInstantObjectReferenceList;
    function GetParamsObject: TParams;
    procedure InitObjectReferences(DataSet: TDataSet);
  protected
    function GetActive: Boolean; override;
    function AcquireDataSet(const AStatement: string; AParams: TParams): TDataSet; virtual;
    procedure ReleaseDataSet(const DataSet: TDataSet);
    function GetParams: TParams; override;
    function GetStatement: string; override;
    function InternalAddObject(AObject: TObject): Integer; override;
    procedure InternalClose; override;
    procedure InternalGetInstantObjectRefs(List: TInstantObjectReferenceList); override;
    function InternalGetObjectCount: Integer; override;
    function InternalGetObjects(Index: Integer): TObject; override;
    function InternalIndexOfObject(AObject: TObject): Integer; override;
    procedure InternalInsertObject(Index: Integer; AObject: TObject); override;
    procedure InternalOpen; override;
    procedure InternalReleaseObject(AObject: TObject); override;
    function InternalRemoveObject(AObject: TObject): Integer; override;
    procedure SetParams(Value: TParams); override;
    function ObjectFetched(Index: Integer): Boolean; override;
    procedure SetStatement(const Value: string); override;
    property ObjectReferenceCount: Integer read GetObjectReferenceCount;
    property ObjectReferenceList: TInstantObjectReferenceList read
        GetObjectReferenceList;
    property ParamsObject: TParams read GetParamsObject;
  public
    destructor Destroy; override;
  end;

  TInstantObjectNotification = (onChanged, onCreated, onDisposed, onRefreshed, onRetrieved, onStored);

  TInstantObjectNotifyEvent = procedure(Sender: TInstantObject;
    Notification: TInstantObjectNotification) of object;

  TInstantObjectNotifiers = class;

  TInstantObjectNotifier = class(TObject)
  private
    FDisableCount: Integer;
    FIncludeOwned: Boolean;
    FObjectClass: TInstantObjectClass;
    FOwner: TInstantObjectNotifiers;
    FOnObjectChanged: TInstantNotifyEvent;
    FOnObjectCreated: TInstantNotifyEvent;
    FOnObjectDisposed: TInstantNotifyEvent;
    FOnObjectRefreshed: TInstantNotifyEvent;
    FOnObjectRetrieved: TInstantNotifyEvent;
    FOnObjectStored: TInstantNotifyEvent;
    FOnNotify: TInstantObjectNotifyEvent;
    procedure DoObjectChanged(AObject: TInstantObject);
    procedure DoObjectCreated(AObject: TInstantObject);
    procedure DoObjectDisposed(AObject: TInstantObject);
    procedure DoObjectRefreshed(AObject: TInstantObject);
    procedure DoObjectRetrieved(AObject: TInstantObject);
    procedure DoObjectStored(AObject: TInstantObject);
    function GetObjectClass: TInstantObjectClass;
    function GetOwner: TInstantObjectNotifiers;
  protected
    function IncludeObject(AObject: TInstantObject): Boolean; virtual;
    procedure ObjectChanged(AObject: TInstantObject); virtual;
    procedure ObjectCreated(AObject: TInstantObject); virtual;
    procedure ObjectDisposed(AObject: TInstantObject); virtual;
    procedure ObjectRefreshed(AObject: TInstantObject); virtual;
    procedure ObjectRetrieved(AObject: TInstantObject); virtual;
    procedure ObjectStored(AObject: TInstantObject); virtual;
  public
    constructor Create(AObjectClass: TInstantObjectClass = nil;
      IncludeOwned: Boolean = False; AOwner: TInstantObjectNotifiers = nil);
    destructor Destroy; override;
    procedure Disable;
    function Disabled: Boolean;
    procedure Enable;
    procedure Notify(AObject: TInstantObject;
      Notification: TInstantObjectNotification);
    property IncludeOwned: Boolean read FIncludeOwned write FIncludeOwned;
    property ObjectClass: TInstantObjectClass read GetObjectClass;
    property Owner: TInstantObjectNotifiers read GetOwner;
    property OnObjectChanged: TInstantNotifyEvent read FOnObjectChanged write FOnObjectChanged;
    property OnObjectCreated: TInstantNotifyEvent read FOnObjectCreated write FOnObjectCreated;
    property OnObjectDisposed: TInstantNotifyEvent read FOnObjectDisposed write FOnObjectDisposed;
    property OnObjectRefreshed: TInstantNotifyEvent read FOnObjectRefreshed write FOnObjectRefreshed;
    property OnObjectRetrieved: TInstantNotifyEvent read FOnObjectRetrieved write FOnObjectRetrieved;
    property OnObjectStored: TInstantNotifyEvent read FOnObjectStored write FOnObjectStored;
    property OnNotify: TInstantObjectNotifyEvent read FOnNotify write FOnNotify;
  end;

  TInstantObjectNotifiers = class(TObject)
  private
    FNotifyingCount: Integer;
    FItemList: TList;
    FDisableCount: Integer;
    FObjectClass: TInstantObjectClass;
    procedure BeginNotify;
    procedure EndNotify;
    function GetCount: Integer;
    function GetIsNotifying: Boolean;
    function GetItemList: TList;
    function GetItems(Index: Integer): TInstantObjectNotifier;
    function GetObjectClass: TInstantObjectClass;
  protected
    function AcceptItem(Item: TInstantObjectNotifier): Boolean; virtual;
    property ItemList: TList read GetItemList;
    property IsNotifying: Boolean read GetIsNotifying;
  public
    constructor Create(AObjectClass: TInstantObjectClass = nil);
    destructor Destroy; override;
    function Add(Item: TInstantObjectNotifier): Integer;
    procedure Disable;
    function Disabled: Boolean;
    procedure Enable;
    procedure Extract(Item: TInstantObjectNotifier);
    procedure Notify(AObject: TInstantObject;
      Notification: TInstantObjectNotification);
    function Remove(Item: TInstantObjectNotifier): Integer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TInstantObjectNotifier read GetItems; default;
    property ObjectClass: TInstantObjectClass read GetObjectClass;
  end;

  TInstantProgressLoop = class(TObject)
  private
    FLow, FHigh, FStep: Integer;
    FOnProgress: TInstantProgressEvent;
  public
    constructor Create(ALow, AHigh: Integer;
      ProgressEvent: TInstantProgressEvent);
    function Done: Boolean;
    procedure Next;
    procedure Progress(Sender: TObject; Count: Integer; var Continue: Boolean);
    property Step: Integer read FStep;
  end;

  TInstantObjectReferenceList = class(TObject)
  private
    FConnector: TInstantConnector;
    FList: TObjectList;
    FRefOwner: TInstantComplex;
    FRefOwnsInstance: Boolean;
    function CreateObjectReference(AObject: TInstantObject):
        TInstantObjectReference;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetItems(Index: Integer): TInstantObject;
    function GetRefItems(Index: Integer): TInstantObjectReference;
    procedure SetCapacity(Value: Integer);
    procedure SetItems(Index: Integer; const Value: TInstantObject);
  public
    constructor Create(ARefOwnsInstance: Boolean; AConnector: TInstantConnector =
        nil; ARefOwner: TInstantComplex = nil);
    destructor Destroy; override;
    function Add: TInstantObjectReference; overload;
    function Add(Item: TInstantObject): Integer; overload;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function IndexOf(Item: TInstantObject; NeedInstance: Boolean = False): Integer;
    function IndexOfInstance(Item: TInstantObject): Integer;
    procedure Insert(Index: Integer; Item: TInstantObject);
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: TInstantObject): Integer;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TInstantObject read GetItems write SetItems;
        default;
    property RefItems[Index: Integer]: TInstantObjectReference read GetRefItems;
  end;

procedure AssignInstantStreamFormat(Strings: TStrings);
function InstantAttributeTypeToDataType(AttributeType: TInstantAttributeType;
  BlobStreamFormat: TInstantStreamFormat = sfBinary): TInstantDataType;
function InstantConnectorClasses: TList;
procedure InstantCheckConnection(Connection: TCustomConnection);
function InstantCheckConnector(var Connector: TInstantConnector): TInstantConnector;
function InstantClassNameToName(const ClassName: string): string;
function InstantCreateStorageMaps(Metadatas: TInstantClassMetadatas = nil): TInstantAttributeMaps;
function InstantDefaultConnector: TInstantConnector;
procedure InstantDisableNotifiers;
procedure InstantEnableNotifiers;
function InstantFindAttribute(const Path: string; AObject: TInstantObject; RaiseExceptions: Boolean = True): TInstantAttribute;
function InstantFindClass(const ClassName: string): TInstantObjectClass;
function InstantFindClassMetadata(const ClassName: string): TInstantClassMetadata;
function InstantGetClass(const ClassName: string): TInstantObjectClass;
procedure InstantGetClasses(Classes: TList; BaseClass: TInstantObjectClass = nil);
function InstantGetClassMetadata(const ClassName: string): TInstantClassMetadata;
function InstantIsValidObjectId(const AObjectId: string): Boolean;
function InstantModel: TInstantModel;
function InstantNameToClassName(const Name: string): string;
procedure InstantRefreshObjects(Classes: array of TInstantObjectClass;
  Progress: TInstantProgressEvent = nil; Connector: TInstantConnector = nil);
procedure InstantRegisterClass(AClass: TInstantObjectClass);
procedure InstantRegisterClasses(AClasses: array of TInstantObjectClass);
procedure InstantUnregisterClass(AClass: TInstantObjectClass);
procedure InstantUnregisterClasses(AClasses: array of TInstantObjectClass);

function InstantResolveGraphicFileType(AStream: TStream ): TInstantGraphicFileFormat;
procedure InstantRegisterGraphicClass(InstantGraphicFileFormat : TInstantGraphicFileFormat;
  AGraphicClass: TGraphicClass);
function InstantGraphicFileFormatToGraphicClass(InstantGraphicFileFormat : TInstantGraphicFileFormat) : TGraphicClass;

// Converts a TInstantDataType to a type that can be used to create TField and
// TParam instances. Raises an exception for unsupported datatypes (at the time
// of writing, all datatypes are supported).
function InstantDataTypeToFieldType(const InstantDataType: TInstantDataType): TFieldType;

const
  InstantDataTypeStrings: array[TInstantDataType] of string =
    ('Integer', 'Float', 'Currency', 'Boolean', 'String', 'Memo', 'DateTime', 'Blob');

procedure AssignInstantDataTypeStrings(Strings: TStrings);

const
  InstantClassPrefix: string = 'T';
  InstantAttributePrefix: string = '_';

var
  InstantLogProc: procedure (const AString: string) of object;
  
implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF LINUX}
  Types,
{$ENDIF}
  TypInfo,
{$IFDEF D6+}
  MaskUtils,
  Variants,
{$ELSE}
  Mask,
{$ENDIF}
  InstantUtils, InstantRtti, InstantDesignHook, InstantCode;

const
  AttributeClasses: array[TInstantAttributeType] of TInstantAttributeClass = (
    nil, TInstantInteger, TInstantFloat, TInstantCurrency, TInstantBoolean, TInstantString,
    TInstantDateTime, TInstantBlob, TInstantMemo, TInstantGraphic, TInstantPart,
    TInstantReference, TInstantParts, TInstantReferences);
  ConcurrencyParamName = 'IO_Concur';
  PersistentIdParamName = 'IO_PersId';

var
  ConnectorClasses: TList;
  ClassList: TList;
  GraphicClassList: array[TInstantGraphicFileFormat] OF TGraphicClass;
  RuntimeModel: TInstantModel;
  ObjectNotifiers: TInstantObjectNotifiers;
  DefaultConnector: TInstantConnector;

{ Local Routines }

{$IFDEF IO_STATEMENT_LOGGING}
procedure InstantLogStatement(const Caption, AStatement: string; AParams: TParams = nil);
var
  S: string;
  g: Integer;
begin
  S := Caption + AStatement;
  if Assigned(AParams) then
  begin
    for g := 0 to AParams.Count - 1 do begin
      S := S + sLineBreak + '  ' +
      AParams[g].Name + ': ' + GetEnumName(TypeInfo(TFieldType), Ord(AParams[g].DataType)) +
      ' = ' + AParams[g].AsString;
    end;
  end;
{$IFDEF MSWINDOWS}
  OutputDebugString(PChar(S));
{$ENDIF}
  if Assigned(InstantLogProc) then
    InstantLogProc(S);
end;
{$ENDIF}

function ValidateChars(Buffer: PChar; BufferLength: Integer;
  ValidChars: TChars; var InvalidChar: Char): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Pred(BufferLength) do
    if not (Buffer[I] in ValidChars + [#8, #10, #13]) then
    begin
      Result := False;
      InvalidChar := Buffer[I];
      Break;
    end;
end;

function CreateObjectFromDataSet(AClass: TClass; DataSet: TDataSet): TObject;
var
  I: Integer;
  FieldName: string;
begin
  if AClass = nil then
    raise Exception.Create(SUnassignedClass)
  else if AClass.InheritsFrom(TInstantObject) then
    Result := TInstantObjectClass(AClass).Create
  else
    Result := AClass.Create;
  for I := 0 to Pred(DataSet.FieldCount) do
  begin
    FieldName := StringReplace(
      DataSet.Fields[I].FieldName, '_', '.', [rfReplaceAll]);
    InstantSetProperty(Result, FieldName, DataSet.Fields[I].Value);
  end;
end;

{ Global routines }

procedure AssignInstantStreamFormat(Strings: TStrings);
var
  i: TInstantStreamFormat;
begin
  if Assigned(Strings) then
  begin
    Strings.Clear;
    for i := Low(TInstantStreamFormat) to High(TInstantStreamFormat) do
    begin
      Strings.Add(AInstantStreamFormatStr[i]);
    end;
  end;
end;

procedure AssignInstantDataTypeStrings(Strings: TStrings);
var
  i: TInstantDataType;
begin
  if Assigned(Strings) then
  begin
    Strings.Clear;
    for i := Low(TInstantDataType) to High(TInstantDataType) do
    begin
      Strings.Add(InstantDataTypeStrings[i]);
    end;
  end;
end;

function InstantAttributeTypeToDataType(AttributeType: TInstantAttributeType;
  BlobStreamFormat: TInstantStreamFormat = sfBinary): TInstantDataType;
const
  DataTypesBinary: array[TInstantAttributeType] of TInstantDataType = (
    dtString,     //atUnknown
    dtInteger,    //atInteger
    dtFloat,      //atFloat
    dtCurrency,   //atCurrency
    dtBoolean,    //atBoolean
    dtString,     //atString
    dtDateTime,   //atDateTime
    dtBlob,       //atBlob
    dtMemo,       //atMemo
    dtBlob,       //atGraphic
    dtBlob,       //atPart
    dtString,     //atReference
    dtBlob,       //atParts
    dtBlob);      //atReferences

  DataTypesXML: array[TInstantAttributeType] of TInstantDataType = (
    dtString,     //atUnknown
    dtInteger,    //atInteger
    dtFloat,      //atFloat
    dtCurrency,   //atCurrency
    dtBoolean,    //atBoolean
    dtString,     //atString
    dtDateTime,   //atDateTime
    dtBlob,       //atBlob
    dtMemo,       //atMemo
    dtBlob,       //atGraphic
    dtMemo,       //atPart
    dtString,     //atReference
    dtMemo,       //atParts
    dtMemo);      //atReferences
begin
  if BlobStreamFormat = sfBinary then
    Result := DataTypesBinary[AttributeType]
  else
    Result := DataTypesXML[AttributeType];
end;

function InstantDataTypeToFieldType(const InstantDataType: TInstantDataType): TFieldType;
begin
  case InstantDataType of
    dtInteger: Result := ftInteger;
    dtFloat: Result := ftFloat;
    dtCurrency: Result := ftCurrency;
    dtBoolean: Result := ftBoolean;
    dtString: Result := ftString;
    dtMemo: Result := ftMemo;
    dtDateTime: Result := ftDateTime;
    dtBlob: Result := ftBlob;
  else
    raise EInstantError.CreateFmt(SUnsupportedDataType,
      [GetEnumName(TypeInfo(TInstantDataType), Ord(InstantDataType))]);
  end;
end;

function InstantConnectorClasses: TList;
begin
  Result := ConnectorClasses;
end;

procedure InstantCheckConnection(Connection: TCustomConnection);
begin
  if not Assigned(Connection) then
    raise EInstantError.Create(SUnassignedConnection);
end;

function InstantCheckConnector(
  var Connector: TInstantConnector): TInstantConnector;
begin
  if Assigned(Connector) then
  begin
    Result := Connector;
    Exit;
  end;
  Connector := DefaultConnector;
  if not Assigned(Connector) then
    raise EInstantError.Create(SUnassignedConnector);
  Result := Connector;
end;

function InstantClassNameToName(const ClassName: string): string;
var
  PrefixLen: Integer;
begin
  PrefixLen := Length(InstantClassPrefix);
  if SameText(Copy(ClassName, 1, PrefixLen), InstantClassPrefix) then
    Result := Copy(ClassName, PrefixLen + 1, Length(ClassName) - PrefixLen)
  else
    Result := ClassName;
end;

function InstantCreateStorageMaps(
  Metadatas: TInstantClassMetadatas): TInstantAttributeMaps;
var
  I, J, K: Integer;
  SourceMap, TargetMap: TInstantAttributeMap;
begin
  if not Assigned(Metadatas) then
    Metadatas := InstantModel.ClassMetadatas;
  if not Assigned(Metadatas) then
  begin
    Result := nil;
    Exit;
  end;
  Result := TInstantAttributeMaps.Create(nil);
  try
    for I := 0 to Pred(Metadatas.Count) do
    begin
      with Metadatas[I] do
        if IsStored then
          for J := 0 to Pred(StorageMaps.Count) do
          begin
            SourceMap := StorageMaps[J];
            TargetMap := Result.EnsureMap(SourceMap.Name);
            for K := 0 to Pred(SourceMap.Count) do
              TargetMap.AddUnique(SourceMap[K]);
          end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function InstantDefaultConnector: TInstantConnector;
begin
  Result := DefaultConnector;
end;

procedure InstantDisableNotifiers;
begin
  ObjectNotifiers.Disable;
end;

procedure InstantEnableNotifiers;
begin
  ObjectNotifiers.Enable;
end;

function InstantFindAttribute(const Path: string; AObject: TInstantObject;
  RaiseExceptions: Boolean = True): TInstantAttribute;
var
  I: Integer;
  AttribName: string;
begin
  Result := nil;
  if (Path = '') or not Assigned(AObject) then
    Exit;
  I := 1;
  AttribName := InstantPartStr(Path, I, InstantDot);
  while (AttribName <> '') and Assigned(AObject) do
  begin
    if RaiseExceptions then
      Result := AObject.AttributeByName(AttribName)
    else
      Result := AObject.FindAttribute(AttribName);
    if not Assigned(Result) then
      Exit;
    Inc(I);
    AttribName := InstantPartStr(Path, I, InstantDot);
    if (AttribName <> '') and (Result is TInstantElement) then
      AObject := TInstantElement(Result).Value;
  end;
end;

function InstantFindClass(const ClassName: string): TInstantObjectClass;
begin
  Result := InstantGetClass(Classname);
  if not Assigned(Result) then
    raise EInstantError.CreateFmt(SClassNotRegistered, [ClassName]);
end;

function InstantFindClassMetadata(
  const ClassName: string): TInstantClassMetadata;
begin
  Result := InstantGetClassMetadata(ClassName);
  if not Assigned(Result) then
    raise EInstantError.CreateFmt(SClassNotRegistered, [ClassName]);
end;

function InstantGetClass(const ClassName: string): TInstantObjectClass;
var
  ObjectClass: TClass;
begin
  ObjectClass := GetClass(ClassName);
  if Assigned(ObjectClass) and ObjectClass.InheritsFrom(TInstantObject) then
    Result := TInstantObjectClass(ObjectClass)
  else
    Result := nil;
end;

procedure InstantGetClasses(Classes: TList; BaseClass: TInstantObjectClass);
var
  I: Integer;
begin
  if not Assigned(Classes) then
    Exit;
  if not Assigned(BaseClass) then
    BaseClass := TInstantObject;
  for I := 0 to Pred(ClassList.Count) do
    if TInstantObjectClass(ClassList[I]).InheritsFrom(BaseClass) then
      Classes.Add(ClassList[I]);
end;

function InstantGetClassMetadata(
  const ClassName: string): TInstantClassMetadata;
var
  CodeClass: TInstantCodeClass;
begin
  if Assigned(DesignModel) then
  begin
    CodeClass := DesignModel^.FindClass(ClassName);
    if Assigned(CodeClass) then
      Result := CodeClass.MetadataInfo.Metadata
    else
      Result := nil;
  end else
    Result := InstantModel.ClassMetadatas.Find(ClassName);
end;

function InstantIsValidObjectId(const AObjectId: string): Boolean;
begin
  Result := Trim(AObjectId) <> '';
end;

function InstantModel: TInstantModel;
begin
  if not Assigned(RuntimeModel) then
    RuntimeModel := TInstantModel.Create;
  Result := RuntimeModel;
end;

function InstantNameToClassName(const Name: string): string;
begin
  Result := InstantClassPrefix + Name;
end;

procedure InstantRefreshObjects(Classes: array of TInstantObjectClass;
  Progress: TInstantProgressEvent; Connector: TInstantConnector);
begin
  with TInstantProgressLoop.Create(Low(Classes), High(Classes), Progress) do
  try
    while not Done do
    begin
      Classes[Step].RefreshAll(Connector, Progress);
      Next;
    end;
  finally
    Free;
  end;
end;

procedure InstantRegisterClass(AClass: TInstantObjectClass);
begin
  Classes.RegisterClass(AClass);
  if ClassList.IndexOf(AClass) = -1 then
    ClassList.Add(AClass);
end;

procedure InstantRegisterClasses(AClasses: array of TInstantObjectClass);
var
  I: Integer;
begin
  for I := Low(AClasses) to High(AClasses) do
    InstantRegisterClass(AClasses[I]);
end;

procedure InstantUnregisterClass(AClass: TInstantObjectClass);
begin
  Classes.UnregisterClass(AClass);
  ClassList.Remove(AClass);
end;

procedure InstantUnregisterClasses(AClasses: array of TInstantObjectClass);
var
  I: Integer;
begin
  for I := Low(AClasses) to High(AClasses) do
    InstantUnRegisterClass(AClasses[I]);
end;

function InstantResolveGraphicFileType(AStream: TStream): TInstantGraphicFileFormat;
const
  SizeOfGraphicHeader = 8;
  MinimumBytesToRead = 10;
var
  P: array [0..MinimumBytesToRead - 1] of Char;
  StreamLength: Longint;
  BytesRetrieved: Integer;
begin
  Result := gffUnknown;
  if not Assigned(AStream) then
    Exit;
  StreamLength := AStream.Size;
  AStream.Position := 0;
  FillChar(P, SizeOf(P), #0);
  BytesRetrieved := AStream.Read(P[0], SizeOf(P));
  AStream.Position := 0;
  if BytesRetrieved < MinimumBytesToRead then
    Exit;
  // bitmap format
  if (P[0] = #66) and (P[1] = #77) then
    Result := gffBmp
  // tiff format
  else if ((P[0] = #73) and (P[1] = #73) and (P[2] = #42) and (P[3] = #0))
   or ((P[0] = #77) and (P[1] = #77) and (P[2] = #42) and (P[3] = #0)) then
    Result := gffTiff
  // jpg format
  else if (P[6] = #74) and (P[7] = #70) and (P[8] = #73) and (P[9] = #70)
   or (P[6] = #69) and (P[7] = #120) and (P[8] = #105) and (P[9] = #102) then
    Result := gffJpeg
  // png format
  else if (P[0] = #137 ) and (P[1] = #80) and (P[2] = #78) and (P[3] = #71)
   and (P[4] = #13) and (P[5] = #10) and (P[6] = #26) and (P[7] = #10) then
    Result := gffPng
  // dcx format
  else if (P[0] = #177) and (P[1] = #104) and (P[2] = #222) and (P[3] = #58) then
    Result := gffDcx
  // pcx format
  else if p[0] = #10 then
    Result := gffPcx
  // emf format
  else if ((P[0] = #215) and (P[1] = #205) and (P[2] = #198) and (P[3] = #154))
   or ((P[0] = #1) and (P[1] = #0) and (P[2] = #0) and (P[3] = #0)) then
    Result := gffEmf
  // gif format
  else if (P[0] = #$47) and (P[1] = #$49) and (P[2] = #$46) then
    Result := gffGif
  // bitmap format with TGraphicHeader header
  else if (P[0] = #01) and (P[1] = #00) and (P[2] = #00) and (P[3] = #01)
   and (PLongint(@p[4])^ = StreamLength - SizeOfGraphicHeader) then
  begin
    Result := gffBmp;
    AStream.Position := SizeOfGraphicHeader;
  end;
end;

procedure InstantRegisterGraphicClass(InstantGraphicFileFormat : TInstantGraphicFileFormat;
  AGraphicClass: TGraphicClass);
begin
  GraphicClassList[InstantGraphicFileFormat] := AGraphicClass;
end;

function InstantGraphicFileFormatToGraphicClass(InstantGraphicFileFormat : TInstantGraphicFileFormat) : TGraphicClass;
begin
  Result := GraphicClassList[InstantGraphicFileFormat];
end;

{ TInstantMetadata }

function TInstantMetadata.Equals(const Other: TInstantMetadata): Boolean;
begin
  Result := InternalEquals(Other);
end;

function TInstantMetadata.GetCollection: TInstantMetadatas;
begin
  Result := inherited Collection as TInstantMetadatas;
end;

function TInstantMetadata.InternalEquals(
  const Other: TInstantMetadata): Boolean;
begin
  { TODO : This only works for case-insensitive object names! }
  Result := SameText(Other.Name, Name);
end;

procedure TInstantMetadata.SetCollection(Value: TInstantMetadatas);
begin
  inherited Collection := Value;
end;

{ TInstantMetadatas }

function TInstantMetadatas.Find(const AName: string): TInstantMetadata;
begin
  Result := TInstantMetadata(inherited Find(AName));
end;

function TInstantMetadatas.GetItems(Index: Integer): TInstantMetadata;
begin
  Result := inherited Items[Index] as TInstantMetadata;
end;

procedure TInstantMetadatas.SetItems(Index: Integer;
  const Value: TInstantMetadata);
begin
  inherited Items[Index] := Value;
end;

{ TInstantAttributeMetadata }

procedure TInstantAttributeMetadata.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantAttributeMetadata then
    with TInstantAttributeMetadata(Source) do
    begin
      Self.FAttributeType := FAttributeType;
      Self.FDefaultValue := FDefaultValue;
      Self.FDisplayWidth := FDisplayWidth;
      Self.FEditMask := FEditMask;
      Self.FIsIndexed := FIsIndexed;
      Self.FIsRequired := FIsRequired;
      Self.FObjectClassName := FObjectClassName;
      Self.FSize := FSize;
      Self.FStorageName := FStorageName;
      Self.FStorageKind := FStorageKind;
      Self.FExternalStorageName := FExternalStorageName;
      Self.FValidChars := FValidChars;
    end;
end;

procedure TInstantAttributeMetadata.CheckAttributeClass(
  AClass: TInstantAttributeClass);
begin
  if Assigned(AClass) and not IsAttributeClass(AClass) then
    raise EInstantError.CreateFmt(SUnexpectedAttributeClass,
      [AttributeClassName, Name, ClassMetadataName, AClass.ClassName]);
end;

procedure TInstantAttributeMetadata.CheckCategory(
  ACategory: TInstantAttributeCategory);

  function CategoryName(Cat: TInstantAttributeCategory): string;
  begin
    Result := GetEnumName(TypeInfo(TInstantAttributeCategory), Ord(Cat));
  end;

begin
  if Category <> ACategory then
    raise EInstantError.CreateFmt(SUnexpectedAttributeCategory,
      [CategoryName(Category), AttributeClassName, Name,
      ClassMetadataName, CategoryName(ACategory)]);
end;

procedure TInstantAttributeMetadata.CheckIsIndexed;
begin
  if not IsIndexed then
    raise EInstantError.CreateFmt(SAttributeNotIndexed,
      [AttributeClassName, Name, ClassMetadataName]);
end;

function TInstantAttributeMetadata.CreateAttribute(
  AObject: TInstantObject): TInstantAttribute;
var
  AClass: TInstantAttributeClass;
begin
  AClass := AttributeClass;
  if not Assigned(AClass) then
    raise EInstantError.CreateFmt(SUnsupportedType, [AttributeTypeName]);
  Result := AClass.Create(AObject, Self);
end;

function TInstantAttributeMetadata.GetAttributeClass: TInstantAttributeClass;
begin
  Result := AttributeClasses[AttributeType];
end;

function TInstantAttributeMetadata.GetAttributeClassName: string;
begin
  if Assigned(AttributeClass) then
    Result := AttributeClass.ClassName
  else
    Result := '';
end;

function TInstantAttributeMetadata.GetAttributeTypeName: string;
begin
  Result := GetEnumName(TypeInfo(TInstantAttributeType),
    Ord(AttributeType));
  Result := Copy(Result, 3, Length(Result) - 2);
end;

function TInstantAttributeMetadata.GetCategory: TInstantAttributeCategory;
begin
  if Assigned(AttributeClass) then
  begin
    if AttributeClass.InheritsFrom(TInstantElement) then
      Result := acElement
    else if AttributeClass.InheritsFrom(TInstantContainer) then
      Result := acContainer
    else
      Result := acSimple;
  end else
    Result := acUnknown;
end;

function TInstantAttributeMetadata.GetClassMetadata: TInstantClassMetadata;
begin
  if Assigned(Collection) then
    Result := Collection.Owner
  else
    Result := nil;
end;

function TInstantAttributeMetadata.GetClassMetadataName: string;
begin
  if Assigned(ClassMetadata) then
    Result := ClassMetadata.Name
  else
    Result := SUnassigned;
end;

function TInstantAttributeMetadata.GetCollection: TInstantAttributeMetadatas;
begin
  Result := inherited Collection as TInstantAttributeMetadatas;
end;

function TInstantAttributeMetadata.GetFieldName: string;
begin
  if FStorageName = '' then
    Result := Name
  else
    Result := FStorageName;
end;

function TInstantAttributeMetadata.GetHasValidChars: Boolean;
begin
  Result := FValidChars <> [];
end;

function TInstantAttributeMetadata.GetIsDefault: Boolean;
begin
  Result := Assigned(ClassMetadata) and
    (ClassMetadata.DefaultContainerName <> '') and
    (ClassMetadata.DefaultContainerName = Name);
end;

function TInstantAttributeMetadata.GetObjectClass: TInstantObjectClass;
begin
  if ObjectClassName = '' then
    Result := TInstantObject
  else
    Result := InstantFindClass(ObjectClassName);
end;

function TInstantAttributeMetadata.GetObjectClassMetadata: TInstantClassMetadata;
begin
  Result := InstantGetClassMetadata(ObjectClassName);
end;

function TInstantAttributeMetadata.GetTableName: string;
begin
  if Assigned(ClassMetadata) then
    Result := ClassMetadata.TableName
  else
    Result := '';
end;

function TInstantAttributeMetadata.GetValidChars: TChars;
begin
  if FValidChars = [] then
    Result := [#0..#255]
  else
    Result := FValidChars;
end;

function TInstantAttributeMetadata.GetValidCharsString: string;
begin
  Result := InstantCharSetToStr(FValidChars);
end;

function TInstantAttributeMetadata.IsAttributeClass(
  AClass: TInstantAttributeClass): Boolean;
begin
  Result := Assigned(AttributeClass) and AttributeClass.InheritsFrom(AClass);
end;

procedure TInstantAttributeMetadata.SetAttributeClass(
  AClass: TInstantAttributeClass);
var
  AttribType: TInstantAttributeType;
begin
  for AttribType := Low(AttribType) to High(AttribType) do
    if AttributeClasses[AttribType] = AClass then
    begin
      AttributeType := AttribType;
      Exit;
    end;
  AttributeType := atUnknown;
end;

procedure TInstantAttributeMetadata.SetAttributeClassName(const Value: string);
var
  AttribType: TInstantAttributeType;
  AClass: TInstantAttributeClass;
begin
  for AttribType := Low(AttribType) to High(AttribType) do
  begin
    AClass := AttributeClasses[AttribType];
    if Assigned(AClass) and SameText(AClass.ClassName, Value) then
    begin
      AttributeType := AttribType;
      Exit;
    end;
  end;
  AttributeType := atUnknown;
end;

procedure TInstantAttributeMetadata.SetAttributeTypeName(const Value: string);
var
  I: Integer;
begin
  if Value = '' then
    Exit;
  I := GetEnumValue(TypeInfo(TInstantAttributeType), 'at' + Value);
  if I <> -1 then
    AttributeType := TInstantAttributeType(I)
  else
    raise EInstantError.CreateFmt(SUnsupportedType, [Value]);
end;

procedure TInstantAttributeMetadata.SetCollection(
  Value: TInstantAttributeMetadatas);
begin
  inherited Collection := Value;
end;

procedure TInstantAttributeMetadata.SetFieldName(const Value: string);
begin
  if Value = Name then
    FStorageName := ''
  else
    FStorageName := Value;
end;

procedure TInstantAttributeMetadata.SetIsDefault(const Value: Boolean);
begin
  if (Value <> IsDefault) and Assigned(ClassMetadata) then
  begin
    if Value then
      ClassMetadata.DefaultContainerName := Name
    else
      ClassMetadata.DefaultContainerName := '';
  end;
end;

procedure TInstantAttributeMetadata.SetValidCharsString(const Value: string);
begin
  FValidChars := InstantStrToCharSet(Value);
end;

{ TInstantAttributeMetadatas }

function TInstantAttributeMetadatas.Add: TInstantAttributeMetadata;
begin
  Result := TInstantAttributeMetadata(inherited Add);
  Changed;
end;

procedure TInstantAttributeMetadatas.Changed;
begin
  if Owner <> nil then
    Owner.DestroyAttributeViews;
end;

procedure TInstantAttributeMetadatas.Clear;
begin
  inherited Clear;
  Changed;
end;

constructor TInstantAttributeMetadatas.Create(AOwner: TInstantClassMetadata);
begin
  inherited Create(AOwner, TInstantAttributeMetadata);
end;

function TInstantAttributeMetadatas.Find(
  const AName: string): TInstantAttributeMetadata;
begin
  Result := TInstantAttributeMetadata(inherited Find(AName));
end;

function TInstantAttributeMetadatas.GetItems(
  Index: Integer): TInstantAttributeMetadata;
begin
  Result := TInstantAttributeMetadata(inherited Items[Index]);
end;

function TInstantAttributeMetadatas.Owner: TInstantClassMetadata;
begin
  Result := inherited Owner as TInstantClassMetadata;
end;

procedure TInstantAttributeMetadatas.Remove(Item: TInstantAttributeMetadata);
begin
  inherited Remove(Item);
  Changed;
end;

procedure TInstantAttributeMetadatas.SetItems(Index: Integer;
  Value: TInstantAttributeMetadata);
begin
  inherited Items[Index] := Value;
end;

{ TInstantClassMetadata }

procedure TInstantClassMetadata.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantClassMetadata then
    with TInstantClassMetadata(Source) do
    begin
      Self.FDefaultContainerName := FDefaultContainerName;
      Self.FStorageName := FStorageName;
      Self.FPersistence := FPersistence;
    end;
end;

procedure TInstantClassMetadata.BuildAttributeMap(Map: TInstantAttributeMap;
  Complete: Boolean);
begin
  GetDistinctAttributeMetadatas(Map);
  if Assigned(Parent) and (Complete or not Parent.IsStored) then
    Parent.BuildAttributeMap(Map, Complete);
end;

procedure TInstantClassMetadata.BuildStorageMaps(Maps: TInstantAttributeMaps);
var
  I: Integer;
  Map: TInstantAttributeMap;
begin
  if IsStored then
  begin
    Map := Maps.EnsureMap(TableName);
    for I := 0 to Pred(StorageMap.Count) do
      Map.AddUnique(StorageMap[I]);
  end;
  if Assigned(Parent) then
    Parent.BuildStorageMaps(Maps)
end;

procedure TInstantClassMetadata.ClearParent;
begin
  FParent := nil;
end;

class procedure TInstantClassMetadata.ConvertToBinary(
  Converter: TInstantTextToBinaryConverter);
begin
  with Converter do
  begin
    ConvertProperties(InstantBuildStartTag(InstantAttributeMetadatasTagName));
    Processor.ReadTag;
    if (Processor.Token = xtTag) and not SameText(Processor.PeekTag,
      InstantBuildEndTag(InstantAttributeMetadatasTagName)) then
      Convert;
    Processor.ReadTag;
  end;
end;

class procedure TInstantClassMetadata.ConvertToText(
  Converter: TInstantBinaryToTextConverter);
begin
  inherited;
  with Converter do
  begin
    Producer.WriteStartTag(InstantAttributeMetadatasTagName);
    if not Reader.EndOfList then
      Convert;
    Producer.WriteEndTag;
  end;
end;

destructor TInstantClassMetadata.Destroy;
begin
  FAttributeMetadatas.Free;
  DestroyAttributeViews;
  inherited;
end;

procedure TInstantClassMetadata.DestroyAttributeViews;
begin
  FreeAndNil(FStorageMap);
  FreeAndNil(FStorageMaps);
  FreeAndNil(FMemberMap);
end;

function TInstantClassMetadata.GetAttributeMetadatas: TInstantAttributeMetadatas;
begin
  if not Assigned(FAttributeMetadatas) then
    FAttributeMetadatas := TInstantAttributeMetadatas.Create(Self);
  Result := FAttributeMetadatas;
end;

function TInstantClassMetadata.GetCollection: TInstantClassMetadatas;
begin
  Result := inherited Collection as TInstantClassMetadatas;
end;

procedure TInstantClassMetadata.GetDistinctAttributeMetadatas(
  Map: TInstantAttributeMap);
var
  I: Integer;
  AttribMeta: TInstantAttributeMetadata;
begin
  with AttributeMetadatas do
    for I := 0 to Pred(Count) do
    begin
      AttribMeta := Items[I];
      if not Assigned(Map.Find(AttribMeta.Name)) then
        Map.Add(AttribMeta);
    end;
end;

function TInstantClassMetadata.GetIsEmpty: Boolean;
begin
  Result := (Persistence = peEmbedded) and (AttributeMetadatas.Count = 0)
    and (StorageName = '');
end;

function TInstantClassMetadata.GetIsStored: Boolean;
begin
  Result := Persistence = peStored;
end;

function TInstantClassMetadata.GetMemberMap: TInstantAttributeMap;
begin
  if not Assigned(FMemberMap) then
  begin
    FMemberMap := TInstantAttributeMap.Create(Self);
    BuildAttributeMap(FMemberMap, True);
  end;
  Result := FMemberMap;
end;

function TInstantClassMetadata.GetParent: TInstantClassMetadata;
begin
  if not Assigned(FParent) then
    FParent := Collection.Find(FParentName);
  Result := FParent;
end;

function TInstantClassMetadata.GetParentName: string;
begin
  if Assigned(FParent) then
    Result := FParent.Name
  else
    Result := FParentName;
end;

function TInstantClassMetadata.GetStorageMap: TInstantAttributeMap;
begin
  if not Assigned(FStorageMap) and IsStored then
  begin
    FStorageMap := TInstantAttributeMap.Create(Self);
    BuildAttributeMap(FStorageMap, False);
  end;
  Result := FStorageMap;
end;

function TInstantClassMetadata.GetStorageMaps: TInstantAttributeMaps;
begin
  if not Assigned(FStorageMaps) and IsStored then
  begin
    FStorageMaps := TInstantAttributeMaps.Create(Self);
    BuildStorageMaps(FStorageMaps);
  end;
  Result := FStorageMaps;
end;

function TInstantClassMetadata.GetTableName: string;
begin
  if FStorageName = '' then
    Result := InstantClassNameToName(Name)
  else
    Result := FStorageName;
end;

procedure TInstantClassMetadata.ReadObject(Reader: TInstantReader);
begin
  inherited;
  if not Reader.EndOfList then
    Reader.ReadObject(AttributeMetadatas);
end;

procedure TInstantClassMetadata.SetCollection(Value: TInstantClassMetadatas);
begin
  if Value <> Collection then
  begin
    ClearParent;
    inherited Collection := Value;
  end;
end;

procedure TInstantClassMetadata.SetParent(Value: TInstantClassMetadata);
begin
  if Value <> FParent then
  begin
    FParent := Value;
    FParentName := '';
  end;
end;

procedure TInstantClassMetadata.SetParentName(const Value: string);
begin
  if Value <> ParentName then
  begin
    ClearParent;
    FParentName := Value;
  end;
end;

procedure TInstantClassMetadata.SetTableName(const Value: string);
begin
  if InstantNameToClassName(Value) = Name then
    StorageName := ''
  else
    StorageName := Value;
end;

procedure TInstantClassMetadata.WriteObject(Writer: TInstantWriter);
begin
  inherited;
  if AttributeMetadatas.Count > 0 then
    Writer.WriteObject(AttributeMetadatas);
end;

{ TInstantClassMetadatas }

function TInstantClassMetadatas.Add: TInstantClassMetadata;
begin
  Result := TInstantClassMetadata(inherited Add);
end;

constructor TInstantClassMetadatas.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TInstantClassMetadata);
end;

class function TInstantClassMetadatas.CreateInstance(
  Arg: Pointer): TInstantCollection;
begin
  Result := Create(Arg);
end;

function TInstantClassMetadatas.Find(
  const AName: string): TInstantClassMetadata;
begin
  Result := TInstantClassMetadata(inherited Find(AName));
end;

function TInstantClassMetadatas.GetItems(Index: Integer): TInstantClassMetadata;
begin
  Result := TInstantClassMetadata(inherited Items[Index]);
end;

procedure TInstantClassMetadatas.SetItems(Index: Integer;
  Value: TInstantClassMetadata);
begin
  inherited Items[Index] := Value;
end;

{ TInstantModel }

destructor TInstantModel.Destroy;
begin
  DestroyClassMetadatas;
  inherited;
end;

procedure TInstantModel.DestroyClassMetadatas;
begin
  FreeAndNil(FClassMetadatas);
end;

function TInstantModel.GetClassMetadatas: TInstantClassMetadatas;
begin
  if not Assigned(FClassMetadatas) then
    FClassMetadatas := TInstantClassMetadatas.Create(nil);
  Result := FClassMetadatas;
end;

procedure TInstantModel.LoadFromFile(const FileName: string);
var
  Stream: TInstantFileStream;
begin
  Stream := TInstantFileStream.Create(FileName, fmOpenRead);
  try
    InstantReadObject(Stream, sfXML, ClassMetadatas);
  finally
    Stream.Free;
  end;
end;

procedure TInstantModel.LoadFromResFile(const FileName: string);
var
  Stream: TInstantFileStream;
begin
  Stream := TInstantFileStream.Create(FileName, fmOpenRead);
  try
    Stream.ReadResourceFileHeader;
    Stream.ReadObjectRes(ClassMetadatas);
  finally
    Stream.Free;
  end;
end;

procedure TInstantModel.SaveToFile(const FileName: string);
var
  Stream: TInstantFileStream;
begin
  Stream := TInstantFileStream.Create(FileName, fmCreate);
  try
    InstantWriteObject(Stream, sfXML, ClassMetadatas);
  finally
    Stream.Free;
  end;
end;

procedure TInstantModel.SaveToResFile(const FileName: string);
var
  Stream: TInstantFileStream;
begin
  Stream := TInstantFileStream.Create(FileName, fmCreate);
  try
    Stream.WriteResourceFileHeader;
    Stream.WriteObjectRes(InstantModelResourceName, ClassMetadatas);
    Stream.AlignStream;
  finally
    Stream.Free;
  end;
end;

{ TInstantFieldMetadata }

procedure TInstantFieldMetadata.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantFieldMetadata then
    with TInstantFieldMetadata(Source) do
    begin
      Self.FDataType := FDataType;
      Self.FAlternateDataTypes := FAlternateDataTypes;
      Self.FOptions := FOptions;
      Self.FSize := FSize;
    end;
end;

constructor TInstantFieldMetadata.Create(ACollection: TInstantFieldMetadatas);
begin
  inherited Create(ACollection);
end;

function TInstantFieldMetadata.DataTypesEqual(
  const Other: TInstantFieldMetadata): Boolean;
begin
  Result := (DataType = Other.DataType) or
    (DataType in Other.AlternateDataTypes) or
    (Other.DataType in AlternateDataTypes);
end;

function TInstantFieldMetadata.GetCollection: TInstantFieldMetadatas;
begin
  Result := inherited Collection as TInstantFieldMetadatas;
end;

function TInstantFieldMetadata.GetTableMetadata: TInstantTableMetadata;
begin
  Result := Collection.Owner;
end;

function TInstantFieldMetadata.InternalEquals(
  const Other: TInstantMetadata): Boolean;
begin
  Result := inherited InternalEquals(Other);
  if Result then
    Result := (Other is TInstantFieldMetadata) and
      (DataTypesEqual(TInstantFieldMetadata(Other)));
  if DataType = dtString then
    Result := Result and (Size = TInstantFieldMetadata(Other).Size);
end;

{ TInstantFieldMetadatas }

function TInstantFieldMetadatas.Add: TInstantFieldMetadata;
begin
  Result := TInstantFieldMetadata(inherited Add);
end;

procedure TInstantFieldMetadatas.AddFieldMetadata(const AName: string;
  ADataType: TInstantDataType; ASize: Integer;
  AOptions: TInstantFieldOptions = []);
begin
  with Add do
  begin
    Name := AName;
    DataType := ADataType;
    Size := ASize;
    Options := AOptions;
  end;
end;

constructor TInstantFieldMetadatas.Create(AOwner: TInstantTableMetadata);
begin
  inherited Create(AOwner, TInstantFieldMetadata);
end;

function TInstantFieldMetadatas.Find(
  const AName: string): TInstantFieldMetadata;
begin
  Result := inherited Find(AName) as TInstantFieldMetadata;
end;

function TInstantFieldMetadatas.GetItems(Index: Integer): TInstantFieldMetadata;
begin
  Result := TInstantFieldMetadata(inherited Items[Index]);
end;

function TInstantFieldMetadatas.Owner: TInstantTableMetadata;
begin
  Result := inherited Owner as TInstantTableMetadata;
end;

procedure TInstantFieldMetadatas.SetItems(Index: Integer;
  Value: TInstantFieldMetadata);
begin
  inherited Items[Index] := Value;
end;

{ TInstantIndexMetadata }

procedure TInstantIndexMetadata.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantIndexMetadata then
    with TInstantIndexMetadata(Source) do
    begin
      Self.FFields := FFields;
      Self.FOptions := FOptions;
    end;
end;

constructor TInstantIndexMetadata.Create(ACollection: TInstantMetadatas);
begin
  inherited Create(ACollection);
end;

function TInstantIndexMetadata.GetCollection: TInstantIndexMetadatas;
begin
  Result := inherited Collection as TInstantIndexMetadatas;
end;

function TInstantIndexMetadata.GetTableMetadata: TInstantTableMetadata;
begin
  Result := Collection.Owner;
end;

function TInstantIndexMetadata.InternalEquals(
  const Other: TInstantMetadata): Boolean;
begin
  Result := inherited InternalEquals(Other);
  if Result then
    Result := (Other is TInstantIndexMetadata) and
      (TInstantIndexMetadata(Other).Options = Options) and
      { TODO : This only works for case-insensitive field names! }
      SameText(TInstantIndexMetadata(Other).Fields, Fields);
end;

function TInstantIndexMetadata.IsFieldIndexed(
  const AFieldMetadata: TInstantFieldMetadata): Boolean;
var
  I: Integer;
  List: TStringList;
begin
  List := TStringList.Create;
  try
    InstantStrToList(Fields, List, [';']);
    Result := False;
    for I := 0 to Pred(List.Count) do
    begin
      { TODO : This only works for case-insensitive field names! }
      Result := SameText(List[I], AFieldMetadata.Name);
      if Result then
        Break;
    end;
  finally
    List.Free;
  end;
end;

{ TInstantIndexMetadatas }

function TInstantIndexMetadatas.Add: TInstantIndexMetadata;
begin
  Result := TInstantIndexMetadata(inherited Add);
end;

procedure TInstantIndexMetadatas.AddIndexMetadata(const AName, AFields: string;
  AOptions: TIndexOptions);
begin
  with Add do
  begin
    Name := AName;
    Fields := AFields;
    Options := AOptions;
  end;
end;

constructor TInstantIndexMetadatas.Create(AOwner: TInstantTableMetadata);
begin
  inherited Create(AOwner, TInstantIndexMetadata);
end;

function TInstantIndexMetadatas.Find(
  const AName: string): TInstantIndexMetadata;
begin
  Result := inherited Find(AName) as TInstantIndexMetadata;
end;

function TInstantIndexMetadatas.GetItems(Index: Integer): TInstantIndexMetadata;
begin
  Result := TInstantIndexMetadata(inherited Items[Index]);
end;

function TInstantIndexMetadatas.IsFieldIndexed(
  const AFieldMetadata: TInstantFieldMetadata): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Pred(Count) do
  begin
    Result := Items[I].IsFieldIndexed(AFieldMetadata);
    if Result then
      Break;
  end;
end;

function TInstantIndexMetadatas.Owner: TInstantTableMetadata;
begin
  Result := inherited Owner as TInstantTableMetadata;
end;

procedure TInstantIndexMetadatas.SetItems(Index: Integer;
  Value: TInstantIndexMetadata);
begin
  inherited Items[Index] := Value
end;

{ TInstantTableMetadata }

procedure TInstantTableMetadata.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantTableMetadata then
    with TInstantTableMetadata(Source) do
    begin
      Self.FieldMetadatas.Assign(FieldMetadatas);
      Self.IndexMetadatas.Assign(IndexMetadatas);
    end;
end;

destructor TInstantTableMetadata.Destroy;
begin
  FFieldMetadatas.Free;
  FIndexMetadatas.Free;
  inherited;
end;

function TInstantTableMetadata.FindFieldMetadata(
  const AName: string): TInstantFieldMetadata;
begin
  Result := FieldMetadatas.Find(AName);
end;

function TInstantTableMetadata.FindIndexMetadata(
  const AName: string): TInstantIndexMetadata;
begin
  Result := IndexMetadatas.Find(AName);
end;

function TInstantTableMetadata.GetFieldMetadataCount: Integer;
begin
  Result := FieldMetadatas.Count;
end;

function TInstantTableMetadata.GetFieldMetadatas: TInstantFieldMetadatas;
begin
  if not Assigned(FFieldMetadatas) then
    FFieldMetadatas := TInstantFieldMetadatas.Create(Self);
  Result := FFieldMetadatas;
end;

function TInstantTableMetadata.GetIndexMetadataCount: Integer;
begin
  Result := IndexMetadatas.Count;
end;

function TInstantTableMetadata.GetIndexMetadatas: TInstantIndexMetadatas;
begin
  if not Assigned(FIndexMetadatas) then
    FIndexMetadatas := TInstantIndexMetadatas.Create(Self);
  Result := FIndexMetadatas;
end;

function TInstantTableMetadata.GetScheme: TInstantScheme;
begin
  if Assigned(Collection) and (Collection.Owner is TInstantScheme) then
    Result := TInstantScheme(Collection.Owner)
  else
    Result := nil;
end;

{ TInstantTableMetadatas }

function TInstantTableMetadatas.Add: TInstantTableMetadata;
begin
  Result := inherited Add as TInstantTableMetadata;
end;

constructor TInstantTableMetadatas.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TInstantTableMetadata);
end;

function TInstantTableMetadatas.Find(
  const AName: string): TInstantTableMetadata;
begin
  Result := inherited Find(AName) as TInstantTableMetadata;
end;

function TInstantTableMetadatas.GetItems(Index: Integer): TInstantTableMetadata;
begin
  Result := inherited Items[Index] as TInstantTableMetadata;
end;

{ TInstantScheme }

function TInstantScheme.AttributeTypeToDataType(
  AttributeType: TInstantAttributeType): TInstantDataType;
begin
  Result := InstantAttributeTypeToDataType(AttributeType, BlobStreamFormat);
end;

procedure TInstantScheme.CatalogWarningEventHandler(const Sender: TObject;
  const AWarningText: string);
begin
  DoWarning(AWarningText);
end;

constructor TInstantScheme.Create;
begin
  inherited Create;
  FBlobStreamFormat := sfBinary;
  FIdDataType := dtString;
  FIdSize := InstantDefaultFieldSize; 
end;

destructor TInstantScheme.Destroy;
begin
  FCatalog.Free;
  FTableMetadataCollection.Free;
  inherited;
end;

procedure TInstantScheme.DoWarning(const AWarningText: string);
begin
  if Assigned(FOnWarning) then
    FOnWarning(Self, AWarningText);
end;

function TInstantScheme.FindTableMetadata(
  const AName: string): TInstantTableMetadata;
begin
  Result := TableMetadataCollection.Find(AName);
end;

function TInstantScheme.GetTableMetadataCollection: TInstantTableMetadatas;
begin
  if not Assigned(FTableMetadataCollection) then
    FTableMetadataCollection := TInstantTableMetadatas.Create(Self);
  Result := FTableMetadataCollection;
end;

function TInstantScheme.GetTableMetadataCount: Integer;
begin
  Result := TableMetadataCollection.Count;
end;

function TInstantScheme.GetTableMetadatas(
  Index: Integer): TInstantTableMetadata;
begin
  Result := TableMetadataCollection[Index];
end;

procedure TInstantScheme.SetCatalog(const Value: TInstantCatalog);
begin
  FreeAndNil(FCatalog);
  FCatalog := Value;
  if Assigned(FCatalog) then
  begin
    FCatalog.OnWarning := CatalogWarningEventHandler;
    FCatalog.InitTableMetadatas(TableMetadataCollection);
  end;
end;

{ TInstantAttributeMap }

function TInstantAttributeMap.Add(Item: TInstantAttributeMetadata): Integer;
begin
  Result := inherited Add(Item);
end;

function TInstantAttributeMap.AddUnique(
  Item: TInstantAttributeMetadata): Integer;
begin
  if not Assigned(Find(Item.Name)) then
    Result := Add(Item)
  else
    Result := -1;
end;

constructor TInstantAttributeMap.Create(
  AClassMetadata: TInstantClassMetadata);
begin
  inherited Create;
  FClassMetadata := AClassMetadata;
end;

function TInstantAttributeMap.Find(
  const AName: string): TInstantAttributeMetadata;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := Items[I];
    if SameText(Result.Name, AName) then
      Exit;
  end;
  Result := nil;
end;

function TInstantAttributeMap.GetIsRootMap: Boolean;
begin
  Result := Assigned(ClassMetadata) and (Name = ClassMetadata.TableName);
end;

function TInstantAttributeMap.GetItems(
  Index: Integer): TInstantAttributeMetadata;
begin
  Result := inherited Items[Index];
end;

function TInstantAttributeMap.GetName: string;
begin
  Result := FName;
end;

function TInstantAttributeMap.IndexOf(Item: TInstantAttributeMetadata): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TInstantAttributeMap.Insert(Index: Integer;
  Item: TInstantAttributeMetadata);
begin
  inherited Insert(Index, Item);
end;

function TInstantAttributeMap.Remove(Item: TInstantAttributeMetadata): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TInstantAttributeMap.SetItems(Index: Integer;
  Value: TInstantAttributeMetadata);
begin
  inherited Items[Index] := Value;
end;

procedure TInstantAttributeMap.SetName(const Value: string);
begin
  FName := Value;
end;

{ TInstantAttributeMaps }

function TInstantAttributeMaps.Add: TInstantAttributeMap;
begin
  Result := TInstantAttributeMap.Create(ClassMetadata);
  try
    Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TInstantAttributeMaps.Add(Item: TInstantAttributeMap): Integer;
begin
  Result := inherited Add(Item)
end;

constructor TInstantAttributeMaps.Create(
  AClassMetadata: TInstantClassMetadata);
begin
  inherited Create;
  FClassMetadata := AClassMetadata;
end;

function TInstantAttributeMaps.EnsureMap(
  const AName: string): TInstantAttributeMap;
begin
  Result := Find(AName);
  if not Assigned(Result) then
  begin
    Result := Add;
    Result.Name := AName;
  end
end;

function TInstantAttributeMaps.Find(const AName: string): TInstantAttributeMap;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := Items[I];
    if SameText(Result.Name, AName) then
      Exit;
  end;
  Result := nil;
end;

function TInstantAttributeMaps.FindMap(
  const AttributeName: string): TInstantAttributeMap;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := Items[I];
    if Assigned(Result.Find(AttributeName)) then
      Exit;
  end;
  Result := nil;
end;

function TInstantAttributeMaps.GetItems(Index: Integer): TInstantAttributeMap;
begin
  Result := inherited Items[Index] as TInstantAttributeMap;
end;

function TInstantAttributeMaps.GetRootMap: TInstantAttributeMap;
var
  I: Integer;
begin
  if Assigned(ClassMetadata) then
    for I := 0 to Pred(Count) do
    begin
      Result := Items[I];
      if Result.Name = ClassMetadata.TableName then
        Exit;
    end;
  Result := nil
end;

function TInstantAttributeMaps.IndexOf(Item: TInstantAttributeMap): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TInstantAttributeMaps.Insert(Index: Integer;
  Item: TInstantAttributeMap);
begin
  inherited Insert(Index, Item);
end;

function TInstantAttributeMaps.Remove(Item: TInstantAttributeMap): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TInstantAttributeMaps.SetItems(Index: Integer;
  Value: TInstantAttributeMap);
begin
  inherited Items[Index] := Value;
end;

{ TInstantObjectReference }

procedure TInstantObjectReference.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantObjectReference then
    with TInstantObjectReference(Source) do
    begin
      Self.FObjectClassName := FObjectClassName;
      Self.FObjectId := FObjectId;
      Self.Instance := Instance;
    end;
end;

procedure TInstantObjectReference.AssignInstance(AInstance: TInstantObject);
begin
  DoAssignInstance(AInstance, OwnsInstance);
end;

procedure TInstantObjectReference.ClearReference;
begin
  FObjectClassName := '';
  FObjectId := '';
end;

constructor TInstantObjectReference.Clone(Source: TInstantObjectReference;
  AOwnsInstance: Boolean; AOwner: TInstantComplex);
begin
  Create;
  FOwner := AOwner;
  Assign(Source);
  OwnsInstance := AOwnsInstance;
end;

constructor TInstantObjectReference.Create(AInstance: TInstantObject;
  AOwnsInstance: Boolean; AOwner: TInstantComplex);
begin
  inherited Create;
  FOwner := AOwner;
  DoAssignInstance(AInstance, AOwnsInstance);
end;

class function TInstantObjectReference.CreateInstance(
  Arg: Pointer): TInstantStreamable;
begin
  Result := Create(Arg);
end;

function TInstantObjectReference.Dereference(Connector: TInstantConnector;
  AOwnsInstance: Boolean; Retry: Boolean): TInstantObject;
var
  Obj: TInstantObject;
begin
  if not Assigned(Instance) or (IsBroken and Retry) then
  begin
    InstantCheckConnector(Connector);
    if HasReference then
    begin
      Obj := ObjectClass.Retrieve(ObjectId, False, False, Connector);
      DoAssignInstance(Obj, AOwnsInstance);
    end;
    if Assigned(FInstance) then
      FInstance.Release
    else
      Integer(FInstance) := -1;
  end;
  Result := Instance;
end;

destructor TInstantObjectReference.Destroy;
begin
  DestroyInstance;
  inherited;
end;

function TInstantObjectReference.Equals(const AObjectClassName,
  AObjectId: string): Boolean;
begin
  Result := SameText(AObjectClassName, FObjectClassName) and
    (AObjectId = FObjectId);
end;

procedure TInstantObjectReference.DestroyInstance;
begin
  if HasInstance and FOwnsInstance then
  begin
    if Assigned(FOwner) then
      FInstance.RefByList.Remove(FOwner);
    FInstance.Free;
  end;
  FInstance := nil;
end;

procedure TInstantObjectReference.DoAssignInstance(AInstance: TInstantObject;
    AOwnsInstance: Boolean);
begin
  if FInstance <> AInstance then
  begin
    DestroyInstance;
    FInstance := AInstance;
    FOwnsInstance := False;
  end;
  OwnsInstance := AOwnsInstance;
end;

function TInstantObjectReference.Equals(AObject: TInstantObject): Boolean;
begin
  if Assigned(AObject) then
    if Assigned(Instance) then
      Result := Instance = AObject
    else
      Result := AObject.EqualsPersistentSignature(ObjectClassName, ObjectId)
  else
    Result := not HasReference;
end;

function TInstantObjectReference.GetInstance: TInstantObject;
begin
  if Integer(FInstance) <= 0 then
    Result := nil
  else
    Result := FInstance;
end;

function TInstantObjectReference.GetObjectClass: TInstantObjectClass;
begin
  Result := InstantFindClass(ObjectClassName);
end;

function TInstantObjectReference.GetObjectClassName: string;
begin
  if Assigned(Instance) then
    Result := Instance.ClassName
  else
    Result := FObjectClassName;
end;

function TInstantObjectReference.GetObjectId: string;
begin
  if Assigned(Instance) then
    Result := Instance.Id
  else
    Result := FObjectId;
end;

function TInstantObjectReference.GetSignature: string;
begin
  if HasReference then
    Result := Format('%s.%s', [ObjectClassName, ObjectId])
  else
    Result := '';
end;

function TInstantObjectReference.HasInstance: Boolean;
begin
  Result := Assigned(Instance);
end;

function TInstantObjectReference.HasReference: Boolean;
begin
  Result := ObjectClassName <> '';
end;

function TInstantObjectReference.IsBroken: Boolean;
begin
  Result := Integer(FInstance) = -1;
end;

procedure TInstantObjectReference.ReadAsObject(Reader: TInstantReader);
var
  AObjectClassName, AObjectId: string;
begin
  Reset;
  with Reader do
  begin
    AObjectClassName := ReadStr;
    AObjectId := ReadStr;
    ReferenceObject(AObjectClassName, AObjectId);
    ReadListEnd;
    ReadListEnd;
  end;
end;

procedure TInstantObjectReference.ReadObject(Reader: TInstantReader);
begin
  inherited;
  DestroyInstance;
  Reader.ReadStr;
  Reader.ReadStr;
  FObjectClassName := Reader.ReadStr;
  FObjectId := Reader.ReadStr;
end;

procedure TInstantObjectReference.ReferenceObject(const AObjectClassName,
  AObjectId: string);
begin
  if not Equals(AObjectClassName, AObjectId) then
  begin
    DestroyInstance;
    FObjectClassName := AObjectClassName;
    FObjectId := AObjectId;
  end;
end;

procedure TInstantObjectReference.ReferenceObject(
  AObjectClass: TInstantObjectClass; const AObjectId: string);
begin
  ReferenceObject(AObjectClass.ClassName, AObjectId);
end;

procedure TInstantObjectReference.Reset;
begin
  DestroyInstance;
  ClearReference;
end;

procedure TInstantObjectReference.SetInstance(Value: TInstantObject);
begin
  if not Equals(Value) then
    ClearReference;
  DoAssignInstance(Value, OwnsInstance);
end;

procedure TInstantObjectReference.SetOwnsInstance(const Value: Boolean);
begin
  if Assigned(Instance) then
    if Value and not OwnsInstance then
    begin
      FInstance.AddRef;
      if Assigned(FOwner) then
        FInstance.RefByList.Add(FOwner);
    end
    else if not Value and OwnsInstance then
    begin
      FInstance.Release;
      if Assigned(FOwner) then
        FInstance.RefByList.Remove(FOwner);
    end;
  FOwnsInstance := Value;
end;

procedure TInstantObjectReference.WriteAsObject(Writer: TInstantWriter);
begin
  with Writer do
  begin
    WriteStr(ObjectClassName);
    WriteStr(ObjectId);
    WriteListEnd;
    WriteListEnd;
  end;
end;

procedure TInstantObjectReference.WriteObject(Writer: TInstantWriter);
begin
  inherited;
  Writer.WriteStr('');
  Writer.WriteStr('');
  Writer.WriteStr(ObjectClassName);
  Writer.WriteStr(ObjectId);
end;

{ TInstantAttribute }

function TInstantAttribute.AccessError(
  const TypeName: string): EInstantAccessError;
begin
  Result := EInstantAccessError.CreateFmt(SAccessError,
    [ClassName, Name, TypeName]);
end;

procedure TInstantAttribute.Changed;
begin
  IsChanged := True;
end;

procedure TInstantAttribute.CheckHasMetadata;
begin
  if not Assigned(Metadata) then
    raise EInstantError.CreateFmt(SUnassignedAttributeMetadata,
      [ClassName, Name]);
end;

function TInstantAttribute.ConversionError(
  E: Exception): EInstantConversionError;
begin
  Result := EInstantConversionError.CreateFmt(SAttributeConversionError,
    [ClassName, Name, E.Message], E);
end;

constructor TInstantAttribute.Create(AOwner: TInstantObject;
  AMetadata: TInstantAttributeMetadata);
begin
  inherited Create;
  FOwner := AOwner;
  FMetadata := AMetadata;
  Initialize;
end;

function TInstantAttribute.GetAsBoolean: Boolean;
begin
  raise AccessError('Boolean');
end;

function TInstantAttribute.GetAsCurrency: Currency;
begin
  Result := AsFloat;
end;

function TInstantAttribute.GetAsDateTime: TDateTime;
begin
  raise AccessError('DateTime');
end;

function TInstantAttribute.GetAsFloat: Double;
begin
  raise AccessError('Float');
end;

function TInstantAttribute.GetAsInteger: Integer;
begin
  raise AccessError('Integer');
end;

function TInstantAttribute.GetAsObject: TInstantObject;
begin
  raise AccessError('Object');
end;

function TInstantAttribute.GetAsString: string;
begin
  raise AccessError('string');
end;

function TInstantAttribute.GetAsVariant: Variant;
begin
  raise AccessError('Variant');
end;

function TInstantAttribute.GetDisplayText: string;
begin
  if Assigned(Metadata) and (Metadata.EditMask <> '') then
    Result := FormatMaskText(Metadata.EditMask, AsString)
  else
    Result := AsString;
end;

function TInstantAttribute.GetIsChanged: Boolean;
begin
  Result := FIsChanged;
end;

function TInstantAttribute.GetIsDefault: Boolean;
var
  DefaultStr, ValueStr: string;
  L: Integer;
begin
  L := Length(AsString);
  if Assigned(Metadata) then
  begin
    Result := Length(Metadata.DefaultValue) = L;
    if Result then
    begin
      DefaultStr := Metadata.DefaultValue;
      ValueStr := AsString;
      // Fixed Range Check Error when one or both strings are ''.
      //Result := CompareMem(@DefaultStr[1], @ValueStr[1], L);
      Result := CompareStr(DefaultStr, ValueStr) = 0;
    end;
  end else
    Result := L = 0;
end;

function TInstantAttribute.GetIsIndexed: Boolean;
begin
  Result := Assigned(Metadata) and Metadata.IsIndexed;
end;

function TInstantAttribute.GetIsMandatory: Boolean;
begin
  Result := IsRequired or IsIndexed;
end;

function TInstantAttribute.GetIsRequired: Boolean;
begin
  Result := Assigned(Metadata) and Metadata.IsRequired;
end;

function TInstantAttribute.GetName: string;
begin
  if Assigned(Metadata) then
    Result := Metadata.Name
  else
    Result := '';
end;

function TInstantAttribute.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TInstantAttribute.GetValue: Variant;
begin
  Result := AsVariant;
end;

procedure TInstantAttribute.Initialize;
begin
end;

procedure TInstantAttribute.ReadName(Reader: TInstantReader);
begin
  Reader.ReadStr;
end;

procedure TInstantAttribute.Reset;
begin
end;

procedure TInstantAttribute.SetAsBoolean(AValue: Boolean);
begin
  raise AccessError('Boolean');
end;

procedure TInstantAttribute.SetAsCurrency(AValue: Currency);
begin
  AsFloat := AValue;
end;

procedure TInstantAttribute.SetAsDateTime(AValue: TDateTime);
begin
  raise AccessError('DateTime');
end;

procedure TInstantAttribute.SetAsFloat(AValue: Double);
begin
  raise AccessError('Float');
end;

procedure TInstantAttribute.SetAsInteger(AValue: Integer);
begin
  raise AccessError('Integer');
end;

procedure TInstantAttribute.SetAsObject(AValue: TInstantObject);
begin
  raise AccessError('Object');
end;

procedure TInstantAttribute.SetAsString(const AValue: string);
begin
  raise AccessError('string');
end;

procedure TInstantAttribute.SetAsVariant(AValue: Variant);
begin
  raise AccessError('Variant');
end;

procedure TInstantAttribute.SetIsChanged(Value: Boolean);
begin
  if Assigned(FOwner) and Value then
    if FOwner.ChangesDisabled then
      Exit
    else
      FOwner.DoAttributeChanged(Self);
  FIsChanged := Value;
end;

procedure TInstantAttribute.SetMetadata(Value: TInstantAttributeMetadata);
begin
  if Assigned(Value) and (Value.AttributeType <> AttributeType) then
    raise EInstantError.CreateFmt(SMetadataMismatch, [ClassName, Name]);
  FMetadata := Value;
  Initialize;
end;

procedure TInstantAttribute.SetOwner(AOwner: TInstantObject);
begin
  FOwner := AOwner;
end;

procedure TInstantAttribute.SetValue(AValue: Variant);
begin
  AsVariant := AValue;
end;

procedure TInstantAttribute.StringValidationError(InvalidChar: Char);
begin
  raise EInstantValidationError.CreateFmt(SInvalidChar,
    [InvalidChar, Ord(InvalidChar), ClassName, Name]);
end;

procedure TInstantAttribute.Unchanged;
begin
  IsChanged := False;
end;

procedure TInstantAttribute.WriteName(Writer: TInstantWriter);
begin
  Writer.WriteStr(Name);
end;

{ TInstantSimple }

function TInstantSimple.InvalidValueError(AValue: Variant;
  E: Exception): EInstantError;
begin
  Result := EInstantError.CreateFmt(SInvalidAttributeValue,
    [VarToStr(AValue), ClassName, Name], E);
end;

{ TInstantNumeric }

function TInstantNumeric.GetAsBoolean: Boolean;
begin
  Result := AsInteger <> 0;
end;

function TInstantNumeric.GetAsDateTime: TDateTime;
begin
  Result := AsFloat;
end;

function TInstantNumeric.GetDisplayText: string;
begin
  if Assigned(Metadata) and (Metadata.EditMask <> '') then
    Result := FormatFloat(Metadata.EditMask, AsFloat)
  else
    Result := inherited GetDisplayText;
end;

function TInstantNumeric.GetIsDefault: Boolean;
begin
  if Assigned(Metadata) and (Metadata.Defaultvalue <> '') then
    Result := inherited GetIsDefault
  else
    Result := AsFloat = 0;
end;

procedure TInstantNumeric.SetAsBoolean(AValue: Boolean);
begin
  AsInteger := Integer(AValue);
end;

procedure TInstantNumeric.SetAsDateTime(AValue: TDateTime);
begin
  AsFloat := AValue;
end;

{ TInstantInteger }

procedure TInstantInteger.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantInteger then
    Value := TInstantInteger(Source).Value;
end;

class function TInstantInteger.AttributeType: TInstantAttributeType;
begin
  Result := atInteger;
end;

function TInstantInteger.GetAsFloat: Double;
begin
  Result := Value;
end;

function TInstantInteger.GetAsInteger: Integer;
begin
  Result := Value;
end;

function TInstantInteger.GetAsString: string;
begin
  Result := IntToStr(Value);
end;

function TInstantInteger.GetAsVariant: Variant;
begin
  Result := Value;
end;

function TInstantInteger.GetValue: Integer;
begin
  Result := FValue;
end;

procedure TInstantInteger.Initialize;
begin
  if Assigned(Metadata) and (Metadata.DefaultValue <> '') then
    try
      FValue := StrToInt(Metadata.DefaultValue);
    except
      on E: Exception do
        raise ConversionError(E);
    end
  else
    FValue := 0;
end;

procedure TInstantInteger.ReadObject(Reader: TInstantReader);
begin
  ReadName(Reader);
  Value := Reader.ReadInteger;
end;

procedure TInstantInteger.Reset;
begin
  if Assigned(Metadata) then
    Initialize
  else
    FValue := 0;
  Changed;
end;

procedure TInstantInteger.SetAsFloat(AValue: Double);
begin
  Value := Round(AValue);
end;

procedure TInstantInteger.SetAsInteger(AValue: Integer);
begin
  Value := AValue;
end;

procedure TInstantInteger.SetAsString(const AValue: string);
begin
  try
    Value := StrToInt(AValue);
  except
    on E: Exception do
      raise ConversionError(E);
  end;
end;

procedure TInstantInteger.SetAsVariant(AValue: Variant);
begin
  try
    Value := AValue;
  except
    on E: Exception do
      raise InvalidValueError(AValue, E);
  end;
end;

procedure TInstantInteger.SetValue(AValue: Integer);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    Changed;
  end;
end;

procedure TInstantInteger.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteInteger(Value);
end;

{ TInstantFloat }

procedure TInstantFloat.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantFloat then
    Value := TInstantFloat(Source).Value;
end;

class function TInstantFloat.AttributeType: TInstantAttributeType;
begin
  Result := atFloat;
end;

function TInstantFloat.GetAsFloat: Double;
begin
  Result := Value;
end;

function TInstantFloat.GetAsInteger: Integer;
begin
  Result := Round(Value);
end;

function TInstantFloat.GetAsString: string;
begin
  Result := FloatToStr(Value);
end;

function TInstantFloat.GetAsVariant: Variant;
begin
  Result := Value;
end;

function TInstantFloat.GetValue: Double;
begin
  Result := FValue;
end;

procedure TInstantFloat.Initialize;
begin
  if Assigned(Metadata) and (Metadata.DefaultValue <> '') then
    try
      FValue := StrToFloat(Metadata.DefaultValue);
    except
      on E: Exception do
        raise ConversionError(E);
    end;
end;

procedure TInstantFloat.ReadObject(Reader: TInstantReader);
begin
  ReadName(Reader);
  Value := Reader.ReadFloat;
end;

procedure TInstantFloat.Reset;
begin
  if Assigned(Metadata) then
    Initialize
  else
    FValue := 0;
  Changed;
end;

procedure TInstantFloat.SetAsFloat(AValue: Double);
begin
  Value := AValue;
end;

procedure TInstantFloat.SetAsInteger(AValue: Integer);
begin
  Value := AValue;
end;

procedure TInstantFloat.SetAsString(const AValue: string);
begin
  try
    Value := StrToFloat(AValue)
  except
    on E: Exception do
      raise ConversionError(E);
  end;
end;

procedure TInstantFloat.SetAsVariant(AValue: Variant);
begin
  try
    Value := AValue;
  except
    on E: Exception do
      raise InvalidValueError(AValue, E);
  end;
end;

procedure TInstantFloat.SetValue(AValue: Double);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    Changed;
  end;
end;

procedure TInstantFloat.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteFloat(Value);
end;

{ TInstantCurrency }

procedure TInstantCurrency.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantCurrency then
    Value := TInstantCurrency(Source).Value;
end;

class function TInstantCurrency.AttributeType: TInstantAttributeType;
begin
  Result := atCurrency;
end;

function TInstantCurrency.GetAsBoolean: Boolean;
begin
  Result := Value <> 0;
end;

function TInstantCurrency.GetAsDateTime: TDateTime;
begin
  Result := Value;
end;

function TInstantCurrency.GetAsFloat: Double;
begin
  Result := Value;
end;

function TInstantCurrency.GetAsInteger: Integer;
begin
  Result := Round(Value);
end;

function TInstantCurrency.GetAsString: string;
begin
  Result := CurrToStr(Value);
end;

function TInstantCurrency.GetAsVariant: Variant;
begin
  Result := Value;
end;

function TInstantCurrency.GetDisplayText: string;
begin
  if Assigned(Metadata) and (Metadata.EditMask <> '') then
    Result := FormatCurr(Metadata.EditMask, Value)
  else
    Result := inherited GetDisplayText;
end;

function TInstantCurrency.GetIsDefault: Boolean;
begin
  if Assigned(Metadata) and (Metadata.Defaultvalue <> '') then
    Result := inherited GetIsDefault
  else
    Result := Value = 0;
end;

function TInstantCurrency.GetValue: Currency;
begin
  Result := FValue;
end;

procedure TInstantCurrency.Initialize;
begin
  if Assigned(Metadata) and (Metadata.DefaultValue <> '') then
    try
      FValue := StrToCurr(Metadata.DefaultValue);
    except
      on E: Exception do
        raise ConversionError(E);
    end;
end;

procedure TInstantCurrency.ReadObject(Reader: TInstantReader);
begin
  ReadName(Reader);
  Value := Reader.ReadCurrency;
end;

procedure TInstantCurrency.Reset;
begin
  inherited;
  if Assigned(Metadata) then
    Initialize
  else
    FValue := 0;
  Changed;
end;

procedure TInstantCurrency.SetAsBoolean(AValue: Boolean);
begin
  inherited;
  Value := Ord(AValue);
end;

procedure TInstantCurrency.SetAsDateTime(AValue: TDateTime);
begin
  inherited;
  Value := AValue;
end;

procedure TInstantCurrency.SetAsFloat(AValue: Double);
begin
  Value := AValue;
end;

procedure TInstantCurrency.SetAsInteger(AValue: Integer);
begin
  Value := AValue;
end;

procedure TInstantCurrency.SetAsString(const AValue: string);
begin
  try
    Value := StrToCurr(AValue)
  except
    on E: Exception do
      raise ConversionError(E);
  end;
end;

procedure TInstantCurrency.SetAsVariant(AValue: Variant);
begin
  try
    Value := AValue;
  except
    on E: Exception do
      raise InvalidValueError(AValue, E);
  end;
end;

procedure TInstantCurrency.SetValue(AValue: Currency);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    Changed;
  end;
end;

procedure TInstantCurrency.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteCurrency(Value);
end;

{ TInstantBoolean }

procedure TInstantBoolean.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantBoolean then
    Value := TInstantBoolean(Source).Value;
end;

class function TInstantBoolean.AttributeType: TInstantAttributeType;
begin
  Result := atBoolean;
end;

function TInstantBoolean.GetAsBoolean: Boolean;
begin
  Result := Value;
end;

function TInstantBoolean.GetAsFloat: Double;
begin
  Result := AsInteger;
end;

function TInstantBoolean.GetAsInteger: Integer;
begin
  Result := Integer(Value);
end;

function TInstantBoolean.GetAsString: string;
begin
  if FValue then
    Result := STrueString
  else
    Result := SFalseString;
end;

function TInstantBoolean.GetAsVariant: Variant;
begin
  Result := Value;
end;

function TInstantBoolean.GetIsDefault: Boolean;
begin
  if Assigned(Metadata) then
    Result := Value = (SameText(Metadata.DefaultValue, InstantTrueString))
  else
    Result := False;
end;

function TInstantBoolean.GetValue: Boolean;
begin
  Result := FValue;
end;

procedure TInstantBoolean.Initialize;
begin
  if Assigned(Metadata) then
    try
      FValue := SameText(Metadata.DefaultValue, InstantTrueString);
    except
      on E: Exception do
        raise ConversionError(E);
    end;
end;

procedure TInstantBoolean.ReadObject(Reader: TInstantReader);
begin
  ReadName(Reader);
  Value := Reader.ReadBoolean
end;

procedure TInstantBoolean.Reset;
begin
  if Assigned(Metadata) then
    Initialize
  else
    FValue := False;
  Changed;
end;

procedure TInstantBoolean.SetAsBoolean(AValue: Boolean);
begin
  Value := AValue;
end;

procedure TInstantBoolean.SetAsFloat(AValue: Double);
begin
  AsInteger := Round(AValue);
end;

procedure TInstantBoolean.SetAsInteger(AValue: Integer);
begin
  Value := Boolean(AValue);
end;

procedure TInstantBoolean.SetAsString(const AValue: string);
begin
  if SameText(AValue, STrueString) then
    Value := True
  else if SameText(AValue, SFalseString) then
    Value := False
  else
    raise ConversionError(nil);
end;

procedure TInstantBoolean.SetAsVariant(AValue: Variant);
begin
  try
    Value := AValue;
  except
    on E: Exception do
      raise InvalidValueError(AValue, E);
  end;
end;

procedure TInstantBoolean.SetValue(AValue: Boolean);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    Changed;
  end;
end;

procedure TInstantBoolean.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteBoolean(Value);
end;

{ TInstantString }

procedure TInstantString.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantString then
    Value := TInstantString(Source).Value;
end;

class function TInstantString.AttributeType: TInstantAttributeType;
begin
  Result := atString;
end;

function TInstantString.GetAsBoolean: Boolean;
begin
  if SameText(Value, STrueString) then
    Result := True
  else if SameText(Value, SFalseString) then
    Result := False
  else
    raise ConversionError(nil);
end;

function TInstantString.GetAsCurrency: Currency;
begin
  Result := AsFloat;
end;

function TInstantString.GetAsDateTime: TDateTime;
begin
  try
    Result := StrToDateTime(Value);
  except
    on E: Exception do
      raise ConversionError(E);
  end;
end;

function TInstantString.GetAsFloat: Double;
begin
  try
    Result := StrToFloat(Value);
  except
    on E: Exception do
      raise ConversionError(E);
  end;
end;

function TInstantString.GetAsInteger: Integer;
begin
  try
    Result := StrToInt(Value);
  except
    on E: Exception do
      raise ConversionError(E);
  end;
end;

function TInstantString.GetAsString: string;
begin
  Result := Value;
end;

function TInstantString.GetAsVariant: Variant;
begin
  Result := Value;
end;

function TInstantString.GetValue: string;
begin
  Result:= FValue;
end;

procedure TInstantString.Initialize;
begin
  if Assigned(Metadata) then
    FValue := Metadata.DefaultValue;
end;

procedure TInstantString.ReadObject(Reader: TInstantReader);
begin
  ReadName(Reader);
  Value := Reader.ReadString;
end;

procedure TInstantString.Reset;
begin
  if Assigned(Metadata) then
    Initialize
  else
    FValue := '';
  Changed;
end;

procedure TInstantString.SetAsBoolean(AValue: Boolean);
begin
  if AValue then
    Value := STrueString
  else
    Value := SFalseString;
end;

procedure TInstantString.SetAsCurrency(AValue: Currency);
begin
  AsFloat := AValue;
end;

procedure TInstantString.SetAsDateTime(AValue: TDateTime);
begin
  Value := DateTimeToStr(AValue);
end;

procedure TInstantString.SetAsFloat(AValue: Double);
begin
  Value := FloatToStr(AValue);
end;

procedure TInstantString.SetAsInteger(AValue: Integer);
begin
  Value := IntToStr(AValue);
end;

procedure TInstantString.SetAsString(const AValue: string);
begin
  Value := AValue;
end;

procedure TInstantString.SetAsVariant(AValue: Variant);
begin
  try
    Value := AValue;
  except
    on E: Exception do
      raise InvalidValueError(AValue, E);
  end;
end;

procedure TInstantString.SetValue(const AValue: string);
var
  InvalidChar: Char;
begin
  if Assigned(Metadata) and not ValidateChars(PChar(AValue), Length(AValue),
    Metadata.ValidChars, InvalidChar) then
    StringValidationError(InvalidChar);
  if AValue <> FValue then
  begin
    FValue:= AValue;
    Changed;
  end;
end;

procedure TInstantString.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteString(Value);
end;

{ TInstantDateTime }

procedure TInstantDateTime.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantDateTime then
    Value := TInstantDateTime(Source).Value
end;

class function TInstantDateTime.AttributeType: TInstantAttributeType;
begin
  Result := atDateTime;
end;

function TInstantDateTime.DefaultValue: TDateTime;
begin
  if Assigned(Metadata) and (Metadata.DefaultValue <> '') then
    if SameText(Metadata.DefaultValue, InstantNowString) then
      Result := Now
    else if SameText(Metadata.DefaultValue, InstantDateString) then
      Result := Date
    else if SameText(Metadata.DefaultValue, InstantTimeString) then
      Result := Time
    else
      try
        Result := InstantStrToDateTime(Metadata.DefaultValue);
      except
        on E: Exception do
          raise ConversionError(E);
      end
  else
    Result := 0;
end;

function TInstantDateTime.GetAsDateTime: TDateTime;
begin
  Result := Value;
end;

function TInstantDateTime.GetAsString: string;
begin
  Result := DateTimeToStr(Value);
end;

function TInstantDateTime.GetAsVariant: Variant;
begin
  Result := Value;
end;

function TInstantDateTime.GetDisplayText: string;
begin
  if AsDateTime = 0 then
    Result := ''
  else begin
    if Assigned(Metadata) and (Metadata.EditMask <> '') then
      DateTimeToString(Result, Metadata.EditMask, AsDateTime)
    else
      Result := inherited GetDisplayText;
  end
end;

function TInstantDateTime.GetIsDefault: Boolean;
begin
  Result := Value = DefaultValue;
end;

function TInstantDateTime.GetValue: TDateTime;
begin
  Result := FValue;
end;

procedure TInstantDateTime.Initialize;
begin
  FValue := DefaultValue;
end;

procedure TInstantDateTime.ReadObject(Reader: TInstantReader);
begin
  ReadName(Reader);
  Value := Reader.ReadDate;
end;

procedure TInstantDateTime.Reset;
begin
  if Assigned(Metadata) then
    Initialize
  else
    FValue := 0;
  Changed;
end;

procedure TInstantDateTime.SetAsDateTime(AValue: TDateTime);
begin
  Value := AValue;
end;

procedure TInstantDateTime.SetAsString(const AValue: string);
begin
  try
    Value := StrToDateTime(AValue)
  except
    on E: Exception do
      raise ConversionError(E);
  end;
end;

procedure TInstantDateTime.SetAsVariant(AValue: Variant);
begin
  try
    Value := AValue;
  except
    on E: Exception do
      raise InvalidValueError(AValue, E);
  end;
end;

procedure TInstantDateTime.SetValue(AValue: TDateTime);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    Changed;
  end;
end;

procedure TInstantDateTime.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteDate(Value);
end;

{ TInstantBlob }

procedure TInstantBlob.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantBlob then
    LoadDataFromStream(TInstantBlob(Source).Stream)
end;

class function TInstantBlob.AttributeType: TInstantAttributeType;
begin
  Result := atBlob;
end;

procedure TInstantBlob.Clear;
begin
  Stream.Clear;
  Changed;
end;

destructor TInstantBlob.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TInstantBlob.GetAsString: string;
begin
  Result := Value;
end;

function TInstantBlob.GetAsVariant: Variant;
begin
  Result := Value;
end;

function TInstantBlob.GetSize: Integer;
begin
  Result := Stream.Size;
end;

function TInstantBlob.GetStream: TMemoryStream;
begin
  if not Assigned(FStream) then
    FStream := TMemoryStream.Create;
  Result := FStream;
end;

function TInstantBlob.GetValue: string;
begin
  if Size > 0 then
  begin
    SetLength(Result, Size);
    Read(Result[1], 0, Size);
  end else
    Result := '';
end;

procedure TInstantBlob.Initialize;
begin
  if Assigned(Metadata) then
    if (Metadata.Defaultvalue <> '') then
    begin
      try
        Write(Metadata.DefaultValue[1], 0, Length(Metadata.DefaultValue));
      except
        on E: Exception do
          raise ConversionError(E);
      end;
    end
    else
      Stream.Clear;
end;

procedure TInstantBlob.LoadDataFromStream(AStream: TStream);
begin
  if not Assigned(AStream) then
    Exit;
  Stream.LoadFromStream(AStream);
  Changed;
end;

function TInstantBlob.Read(var Buffer; Position, Count: Integer): Integer;
begin
  Stream.Position := Position;
  Result := Stream.Read(Buffer, Count);
end;

function TInstantBlob.ReadBuffer(var Buffer; Position, Count: Integer): Integer;
begin
  Result := Read(Buffer, Position, Count);
end;

procedure TInstantBlob.ReadObject(Reader: TInstantReader);
begin
  ReadName(Reader);
  Reader.ReadBinary(LoadDataFromStream);
end;

procedure TInstantBlob.Reset;
begin
  if Assigned(Metadata) then
  begin
    Initialize;
    Changed;
  end else
    Clear;
end;

procedure TInstantBlob.SaveDataToStream(AStream: TStream);
begin
  if Assigned(AStream) then
    Stream.SaveToStream(AStream);
end;

procedure TInstantBlob.SetAsString(const AValue: string);
begin
  Value := AValue;
end;

procedure TInstantBlob.SetAsVariant(AValue: Variant);
begin
  try
    Value := AValue;
  except
    on E: Exception do
      raise InvalidValueError(AValue, E);
  end;
end;

procedure TInstantBlob.SetValue(const AValue: string);
var
  L: Integer;
begin
  L := Length(AValue);
  if L > 0 then
  begin
    WriteBuffer(AValue[1], 0, L);
    Stream.Size := L;
  end else
    Clear;
end;

function TInstantBlob.Write(const Buffer; Position, Count: Integer): Integer;
var
  C: Char;

  function CompareBuffers: Boolean;
  var
    I: Integer;
  begin
    Stream.Position := Position;
    for I := 0 to Pred(Count) do
    begin
      Result := (Stream.Read(C, 1) = 1) and (C = PChar(@Buffer)[I]);
      if not Result then
        Exit;
    end;
    Result := True;
  end;

begin
  if not ValidateChars(PChar(@Buffer), Count, Metadata.ValidChars, C) then
    StringValidationError(C);
  if not CompareBuffers then
  begin
    Stream.Position := Position;
    Result := Stream.Write(Buffer, Count);
  end else
    Result := 0;
end;

function TInstantBlob.WriteBuffer(const Buffer; Position,
  Count: Integer): Integer;
begin
  Result := Write(Buffer, Position, Count);
  IsChanged := Result > 0;
end;

procedure TInstantBlob.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteBinary(SaveDataToStream);
end;

procedure TInstantBlob.AssignPicture(Source: TPicture);
begin
  if Assigned(Source.Graphic) then
  begin
    Stream.Clear;
    Source.Graphic.SaveToStream(Stream);
  end
  else
    Clear;
  Changed;
end;

procedure TInstantBlob.AssignToPicture(Dest: TPicture);
var
  Image : TGraphic;
  GraphicClass : TGraphicClass;
  InstantGraphicFileFormat : TInstantGraphicFileFormat;
begin
  if Stream.Size > 0 then
  begin
    Stream.Position := 0;
    InstantGraphicFileFormat := InstantResolveGraphicFileType(Stream);
    if InstantGraphicFileFormat = gffUnknown then
      raise EInstantError.Create(SUnsupportedGraphicStream);
    GraphicClass := InstantGraphicFileFormatToGraphicClass(InstantGraphicFileFormat);
    if not Assigned(GraphicClass) then
      raise EInstantError.Create(SUnsupportedGraphicClass);
    Image := GraphicClass.Create;
    Try
      Image.LoadFromStream(Stream);
      Dest.Assign(Image);
    Finally
      Image.Free;
    End;
  end
  else if Assigned(Dest.Graphic) then
    Dest.Graphic := nil;
end;

{ TInstantMemo }

class function TInstantMemo.AttributeType: TInstantAttributeType;
begin
  Result := atMemo;
end;

procedure TInstantMemo.ReadObject(Reader: TInstantReader);
begin
  ReadName(Reader);
  Value := Reader.ReadString;
end;

procedure TInstantMemo.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteString(Value);
end;

{ TInstantComplex }

function TInstantComplex.AttachObject(AObject: TInstantObject): Boolean;
begin
  Result := False;
end;

function TInstantComplex.DetachObject(AObject: TInstantObject): Boolean;
begin
  Result := False;
end;

function TInstantComplex.GetAllowOwned: Boolean;
begin
  Result := False;
end;

function TInstantComplex.GetConnector: TInstantConnector;
begin
  if Assigned(Owner) then
    Result := Owner.Connector
  else
    Result := nil;
  InstantCheckConnector(Result);
end;

function TInstantComplex.GetRequiredClass: TInstantObjectClass;
begin
  if Assigned(Metadata) then
    Result := Metadata.ObjectClass
  else
    Result := TInstantObject;
end;

function TInstantComplex.GetRequiredClassName: string;
begin
  if Assigned(Metadata) then
    Result := Metadata.ObjectClassName
  else
    Result := TInstantObject.ClassName;
end;

procedure TInstantComplex.ValidateObject(AObject: TInstantObject);
begin
  if Assigned(AObject) then
  begin
    ValidateObjectClassType(AObject.ClassType);
    if (AObject.Connector <> Connector) then
      raise EInstantValidationError.CreateFmt(SInvalidConnector,
        [AObject.ClassName, AObject.Id, ClassName, Name]);
  end;
end;

procedure TInstantComplex.ValidateObjectClassType(
  AClass: TInstantObjectClass);
begin
  if not (AClass.InheritsFrom(RequiredClass)) then
    raise EInstantValidationError.CreateFmt(SInvalidObjectClass,
      [AClass.ClassName, ClassName, Name, RequiredClass.ClassName]);
end;

{ TInstantElement }

function TInstantElement.AttachObject(AObject: TInstantObject): Boolean;
begin
  Value  := AObject;
  Result := Value = AObject;
end;

function TInstantElement.CreateObject: TInstantObject;
begin
  Result := RequiredClass.Create(Connector);
end;

function TInstantElement.DetachObject(AObject: TInstantObject): Boolean;
begin
  Result := Value = AObject;
  if Result then
    Value := nil;
end;

function TInstantElement.GetAsObject: TInstantObject;
begin
  Result := Value;
end;

function TInstantElement.HasValue: Boolean;
begin
  Result := False;
end;

procedure TInstantElement.LoadObjectFromStream(AStream: TStream);
var
  Obj: TPersistent;
  MemoryStream : TMemoryStream;
begin
  if not Assigned(AStream) then
    Exit;

  if Connector.BlobStreamFormat = sfBinary then
    Obj := InstantReadObjectFromStream(AStream, nil, nil, Connector)
  else
  begin
    MemoryStream := TMemoryStream.Create;
    try
      InstantObjectTextToBinary(AStream, MemoryStream);
      MemoryStream.Position := 0;
      Obj := InstantReadObjectFromStream(MemoryStream, nil, nil, Connector);
    finally
      MemoryStream.Free;
    end;
  end;

  try
    Value := TInstantObject(Obj) as TInstantObject
  except
    Obj.Free;
    raise;
  end;
end;

procedure TInstantElement.SaveObjectToStream(AStream: TStream);
var
  MemoryStream : TMemoryStream;
begin
  if not (Assigned(AStream) and Assigned(Value)) then
    Exit;

  if Connector.BlobStreamFormat = sfBinary then
    InstantWriteObjectToStream(AStream, Value)
  else
  begin
    MemoryStream := TMemoryStream.Create;
    try
      InstantWriteObjectToStream(MemoryStream, Value);
      MemoryStream.Position := 0;
      InstantObjectBinaryToText(MemoryStream, AStream);
    finally
      MemoryStream.Free;
    end;
  end;
end;

procedure TInstantElement.SetAsObject(AValue: TInstantObject);
begin
  Value := AValue as TInstantObject;
end;

procedure TInstantElement.SetValue(AValue: TInstantObject);
begin
  ValidateObject(AValue);
end;

{ TInstantPart }

procedure TInstantPart.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantPart then
    with TInstantPart(Source) do
      if Assigned(FValue) then
        Self.Value := FValue.Clone(Self.Connector)
      else
        Self.Value := nil;
end;

class function TInstantPart.AttributeType: TInstantAttributeType;
begin
  Result := atPart;
end;

procedure TInstantPart.ClearOwnerContext(AObject: TInstantObject);
begin
  if Assigned(AObject) then
    AObject.ClearOwnerContext;
end;

destructor TInstantPart.Destroy;
begin
  DestroyObject;
  inherited;
end;

procedure TInstantPart.DestroyObject;
begin
  FreeAndNil(FValue);
end;

function TInstantPart.GetAllowOwned: Boolean;
begin
  if Metadata.StorageKind = skExternal then
    Result := True
  else
    Result := inherited GetAllowOwned();
end;

function TInstantPart.GetIsChanged: Boolean;
begin
  Result := inherited GetIsChanged or (Assigned(FValue) and FValue.IsChanged);
end;

function TInstantPart.GetIsDefault: Boolean;
begin
  if Assigned(FValue) then
    Result := (FValue.ClassType = RequiredClass) and FValue.IsDefault
  else
    Result := True;
end;

function TInstantPart.GetValue: TInstantObject;
begin
  if not Assigned(FValue) then
  begin
    FValue := CreateObject;
    SetOwnerContext(FValue);
  end;
  Result := FValue;
end;

function TInstantPart.HasValue: Boolean;
begin
  Result := Assigned(FValue);
end;

procedure TInstantPart.ReadObject(Reader: TInstantReader);
var
  Obj: TPersistent;
begin
  ReadName(Reader);
  Reader.ReadIdent;
  Obj := Reader.ReadObject(nil, Connector);
  try
    Value := Obj as TInstantObject;
  except
    Obj.Free;
    raise;
  end;
end;

procedure TInstantPart.Reset;
begin
  if not IsDefault then
  begin
    DestroyObject;
    Changed;
  end;
end;

procedure TInstantPart.SetOwnerContext(AObject: TInstantObject);
begin
  if Assigned(AObject) then
    AObject.SetOwnerContext(Owner, Self)
end;

procedure TInstantPart.SetValue(AValue: TInstantObject);
begin
  if AValue <> FValue then
  begin
    inherited;
    ClearOwnerContext(FValue);
    DestroyObject;
    FValue := AValue;
    SetOwnerContext(FValue);
    Changed;
  end;
end;

procedure TInstantPart.Unchanged;
begin
  inherited;
  if HasValue then
    Value.Unchanged;
end;

procedure TInstantPart.ValidateObject(AObject: TInstantObject);
begin
  inherited;
  if Assigned(AObject) then
  begin
    if Metadata.StorageKind = skEmbedded then
      if AObject.IsPersistent then
        raise EInstantValidationError.CreateFmt(SPersistentObjectNotAllowed,
          [AObject.ClassName, AObject.Id]);
    if Assigned(Owner) and (AObject = Owner) then
      raise EInstantValidationError.CreateFmt(SOwnershipRecursion,
        [AObject.ClassName, AObject.Id]);
  end;
end;

procedure TInstantPart.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteIdent('');
  Writer.WriteObject(Value);
end;

{ TInstantReference }

procedure TInstantReference.Assign(Source: TPersistent);
begin
  if Source is TInstantReference then
  begin
    with TInstantReference(Source) do
    begin
      // cross-connector object assignment must be supported for InstantPump.
      if Self.Connector <> Connector then
      begin
        if Assigned(Value) then
          Self.Value := Value.Clone(Self.Connector)
        else
          Self.Value := nil;  
      end
      else
        Self.Value := Value;
    end;
  end;
end;

class function TInstantReference.AttributeType: TInstantAttributeType;
begin
  Result := atReference;
end;

function TInstantReference.CreateObject: TInstantObject;
begin
  if Assigned(Metadata) and (Metadata.DefaultValue <> '') then
    Result := RequiredClass.Retrieve(Metadata.Defaultvalue, False, False,
      Connector)
  else
    Result := nil;
end;

destructor TInstantReference.Destroy;
begin
  DestroyObjectReference;
  inherited;
end;

procedure TInstantReference.DestroyObject;
begin
  ObjectReference.DestroyInstance;
end;

procedure TInstantReference.DestroyObjectReference;
begin
  FreeAndNil(FObjectReference);
end;

function TInstantReference.GetAllowOwned: Boolean;
begin
  Result := FAllowOwned;
end;

function TInstantReference.GetConnector: TInstantConnector;
begin
  Result := FConnector;
  if not Assigned(Result) then
    Result := inherited GetConnector;
end;

function TInstantReference.GetInstance: TInstantObject;
begin
  Result := ObjectReference.Instance;
end;

function TInstantReference.GetIsBroken: Boolean;
begin
  Result := ObjectReference.IsBroken;
end;

function TInstantReference.GetIsDefault: Boolean;
var
  DefaultObjectId: string;
begin
  if Assigned(Metadata) then
  begin
    DefaultObjectId := Metadata.DefaultValue;
    if DefaultObjectId = '' then
      Result := ObjectClassName = ''
    else
      Result := (ObjectClass = RequiredClass) and
        (ObjectId = DefaultObjectId)
  end else
    Result := ObjectClassName = '';
end;

function TInstantReference.GetObjectClass: TInstantObjectClass;
begin
  Result := ObjectReference.ObjectClass;
end;

function TInstantReference.GetObjectClassName: string;
begin
  Result := ObjectReference.ObjectClassName;
end;

function TInstantReference.GetObjectId: string;
begin
  Result := ObjectReference.ObjectId;
end;

function TInstantReference.GetObjectReference: TInstantObjectReference;
begin
  if not Assigned(FObjectReference) then
    FObjectReference := TInstantObjectReference.Create(nil, True, Self);
  Result := FObjectReference;
end;

function TInstantReference.GetValue: TInstantObject;
var
  Obj: TInstantObject;
begin
  if not Assigned(Instance) then
  begin
    if HasReference then
    begin
      if not IsBroken then
        RetrieveObject;
    end else
    begin
      Obj := CreateObject;
      if Assigned(Obj) then
      begin
        ObjectReference.AssignInstance(Obj);
        Obj.Release;
      end;
    end;
  end;
  Result := Instance;
end;

function TInstantReference.HasReference: Boolean;
begin
  Result := ObjectReference.HasReference;
end;

function TInstantReference.HasValue: Boolean;
begin
  Result := ObjectReference.HasInstance;
end;

procedure TInstantReference.LoadObjectFromStream(AStream: TStream);
begin
  inherited;
  if Assigned(Value) then
    Value.Release;
end;

procedure TInstantReference.ReadObject(Reader: TInstantReader);
begin
  ReadName(Reader);
  Reader.ReadIdent;
  ObjectReference.ReadAsObject(Reader);
  Changed;
end;

procedure TInstantReference.ReferenceObject(AObjectClass: TInstantObjectClass;
  const AObjectId: string);
begin
  if SameInstance(AObjectClass, AObjectId) then
    Exit;
  if Assigned(AObjectClass) then
  begin
    ValidateObjectClassType(AObjectClass);
    ObjectReference.ReferenceObject(AObjectClass, AObjectId);
    Changed;
  end else
    Reset;
end;

procedure TInstantReference.ReferenceObject(const AObjectClassName,
  AObjectId: string);
var
  AObjectClass: TInstantObjectClass;
begin
  if SameInstance(AObjectClassName, AObjectId) then
    Exit;
  if AObjectClassName = '' then
    AObjectClass := nil
  else
    AObjectClass := InstantFindClass(AObjectClassName);
  ReferenceObject(AObjectClass, AObjectId);
end;

procedure TInstantReference.Reset;
begin
  if not IsDefault then
  begin
    DestroyObjectReference;
    Changed;
  end;
end;

function TInstantReference.RetrieveObject: TInstantObject;
begin
  Result := ObjectReference.Dereference(Connector);
end;

function TInstantReference.SameInstance(AObject: TInstantObject): Boolean;
begin
  if Assigned(AObject) then
    Result := ObjectReference.Equals(AObject)
  else
    Result := not ObjectReference.HasReference;
end;

function TInstantReference.SameInstance(const AObjectClassName,
  AObjectId: string): Boolean;
begin
  Result := ObjectReference.Equals(AObjectClassName, AObjectId);
end;

function TInstantReference.SameInstance(AObjectClass: TInstantObjectClass;
  AObjectId: string): Boolean;
begin
  if Assigned(AObjectClass) then
    Result := SameInstance(AObjectClass.ClassName, AObjectId)
  else
    Result := SameInstance(nil)
end;

procedure TInstantReference.SetAllowOwned(Value: Boolean);
begin
  FAllowOwned := Value;
end;

procedure TInstantReference.SetConnector(Value: TInstantConnector);
begin
  if Value = FConnector then
    Exit;
  Reset;
  FConnector := Value;
end;

procedure TInstantReference.SetInstance(AValue: TInstantObject);
begin
  ObjectReference.Instance := AValue;
end;

procedure TInstantReference.SetValue(AValue: TInstantObject);
begin
  if SameInstance(AValue) then
    Exit;
  inherited;
  Instance := AValue;
  Changed;
end;

procedure TInstantReference.ValidateObject(AObject: TInstantObject);
begin
  inherited;
  if Assigned(AObject) then
    AObject.CheckId;
end;

procedure TInstantReference.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteIdent('');
  ObjectReference.WriteAsObject(Writer);
end;

{ TInstantContainer }

function TInstantContainer.Add(AObject: TInstantObject): Integer;
begin
  ValidateObject(AObject);
  BeforeContentChange(ctAdd, -1, AObject);
  Result := InternalAdd(AObject);
  AfterContentChange(ctAdd, Result, AObject);
end;

procedure TInstantContainer.AfterContentChange(
  ChangeType: TInstantContentChangeType; Index: Integer;
  AObject: TInstantObject);
begin
  Changed;
  if Assigned(FOwner) then
    Owner.DoAfterContentChange(Self, ChangeType, Index, AObject);
end;

function TInstantContainer.AttachObject(AObject: TInstantObject): Boolean;
begin
  Result := Add(AObject) <> -1;
end;

procedure TInstantContainer.BeforeContentChange(
  ChangeType: TInstantContentChangeType; Index: Integer;
  AObject: TInstantObject);
begin
  if Assigned(FOwner) then
    Owner.DoBeforeContentChange(Self, ChangeType, Index, AObject);
end;

procedure TInstantContainer.CheckRange(Index: Integer);
begin
  if (Index < 0) or (Index >= Count) then
    raise EInstantRangeError.CreateFmt(SIndexOutOfBounds, [Index]);
end;

procedure TInstantContainer.Clear;
begin
  BeforeContentChange(ctClear, -1, nil);
  InternalClear;
  AfterContentChange(ctClear, -1, nil);
end;

procedure TInstantContainer.Delete(Index: Integer);
var
  Obj: TInstantObject;
begin
  CheckRange(Index);
  Obj := Instances[Index];
  if Assigned(Obj) then
    Obj.AddRef;
  try
    BeforeContentChange(ctRemove, Index, Obj);
    InternalDelete(Index);
    AfterContentChange(ctRemove, Index, Obj);
  finally
    Obj.Free;
  end;
end;

function TInstantContainer.DetachObject(AObject: TInstantObject): Boolean;
begin
  Result := Remove(AObject) <> -1;
end;

procedure TInstantContainer.Exchange(Index1, Index2: Integer);
begin
  CheckRange(Index1);
  CheckRange(Index2);
  InternalExchange(Index1, Index2);
  Changed;
end;

function TInstantContainer.GetChangeCount: Integer;
var
  I: Integer;
  Obj: TInstantObject;
begin
  Result := 0;
  for I := 0 to Pred(Count) do
  begin
    Obj := Items[I];
    if Assigned(Obj) and Obj.IsChanged then
      Inc(Result);
  end;
end;

function TInstantContainer.GetInstances(Index: Integer): TInstantObject;
begin
  Result := Items[Index];
end;

function TInstantContainer.GetIsDefault: Boolean;
begin
  Result := Count = 0;
end;

function TInstantContainer.GetItems(
  Index: Integer): TInstantObject;
begin
  CheckRange(Index);
  Result := InternalGetItems(Index);
end;

function TInstantContainer.HasItem(Index: Integer): Boolean;
begin
  Result := Assigned(Instances[Index]);
end;

function TInstantContainer.IndexOf(AObject: TInstantObject): Integer;
begin
  Result := InternalIndexOf(AObject);
end;

function TInstantContainer.IndexOfInstance(Instance: Pointer): Integer;
begin
  Result := InternalIndexOfInstance(Instance);
end;

procedure TInstantContainer.Insert(Index: Integer;
  AObject: TInstantObject);
begin
  if Index <> 0 then
    CheckRange(Index);
  ValidateObject(AObject);
  BeforeContentChange(ctAdd, Index, AObject);
  InternalInsert(Index, AObject);
  AfterContentChange(ctAdd, Index, AObject);
end;

procedure TInstantContainer.LoadObjectsFromStream(AStream: TStream);
var
  Obj: TPersistent;
  MemoryStream : TMemoryStream;
begin
  if not Assigned(AStream) then
    Exit;
  Clear;
  if Connector.BlobStreamFormat = sfBinary then
  begin
    while AStream.Position < AStream.Size do
    begin
      Obj := InstantReadObjectFromStream(AStream, nil, nil, Connector);
      try
        Add(Obj as TInstantObject);
      except
        Obj.Free;
        raise;
      end;
    end;
  end
  else
  begin
    MemoryStream := TMemoryStream.Create;
    try
      //CB: I don't know why MS-SQL via ADO or via DBX returns a stream with wrong size (+1)
      //so I've changed this test adding -1 (for other brokers this is not a problem)
      while AStream.Position < AStream.Size -1 do
      begin
        MemoryStream.Clear;
        InstantObjectTextToBinary(AStream, MemoryStream);
        MemoryStream.Position := 0;
        Obj := InstantReadObjectFromStream(MemoryStream, nil, nil, Connector);
        try
          Add(Obj as TInstantObject);
        except
          Obj.Free;
          raise;
        end;
      end;
    finally
      MemoryStream.Free;
    end;
  end;
  Changed;
end;

procedure TInstantContainer.Move(CurIndex, NewIndex: Integer);
begin
  CheckRange(CurIndex);
  CheckRange(NewIndex);
  InternalMove(CurIndex, NewIndex);
  Changed;
end;

procedure TInstantContainer.QuickSort(L, R: Integer;
  Compare: TInstantSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while Compare(Owner, Items[I], Items[P]) < 0 do
        Inc(I);
      while Compare(Owner, Items[J], Items[P]) > 0 do
        Dec(J);
      if I <= J then
      begin
        Exchange(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J, Compare);
    L := I;
  until I >= R;
end;

function TInstantContainer.Remove(AObject: TInstantObject): Integer;
begin
  Result := IndexOf(AObject);
  if Result <> -1 then
    Delete(Result);
end;

procedure TInstantContainer.Reset;
begin
  Clear;
end;

procedure TInstantContainer.SaveObjectsToStream(AStream: TStream);
var
  I, P: Integer;
  MemoryStream : TMemoryStream;
begin
  if not Assigned(AStream) then
    Exit;
  if Connector.BlobStreamFormat = sfBinary then
  begin
    for I := 0 to Pred(Count) do
      InstantWriteObjectToStream(AStream, Items[I]);
  end
  else
  begin
    MemoryStream := TMemoryStream.Create;
    try
      for I := 0 to Pred(Count) do
      begin
        P := MemoryStream.Position;
        InstantWriteObjectToStream(MemoryStream, Items[I]);
        MemoryStream.Position := P;
        InstantObjectBinaryToText(MemoryStream, AStream);
      end;
    finally
      MemoryStream.Free;
    end;
  end;
end;

procedure TInstantContainer.SetItems(Index: Integer;
  AValue: TInstantObject);
var
  Obj: TInstantObject;
begin
  CheckRange(Index);
  ValidateObject(AValue);
  Obj := Instances[Index];
  if Assigned(Obj) then
    Obj.AddRef;
  try
    BeforeContentChange(ctReplace, Index, AValue);
    InternalSetItems(Index, AValue);
    AfterContentChange(ctReplace, Index, AValue);
  finally
    Obj.Free;
  end;
end;

procedure TInstantContainer.Sort(Compare: TInstantSortCompare);
begin
  if Count > 1 then
    QuickSort(0, Pred(Count), Compare);
end;

procedure TInstantContainer.ValidateObject(AObject: TInstantObject);
begin
  inherited;
  if not Assigned(AObject) then
    raise EInstantError.CreateFmt(SUnassignedObjectInAttribute,
      [ClassName, Name]);
end;

{ TInstantParts }

procedure TInstantParts.Assign(Source: TPersistent);
var
  I: Integer;
  Obj: TInstantObject;
begin
  inherited;
  if Source is TInstantParts then
  begin
    Clear;
    with TInstantParts(Source) do
    begin
      for I := 0 to Pred(Count) do
      begin
        Obj := Items[I].Clone(Self.Connector);
        try
          Self.Add(Obj);
        except
          Obj.Free;
          raise;
        end;
      end;
    end;
  end;
end;

class function TInstantParts.AttributeType: TInstantAttributeType;
begin
  Result := atParts;
end;

procedure TInstantParts.ClearOwnerContext(AObject: TInstantObject);
begin
  if Assigned(AObject) then
    AObject.ClearOwnerContext;
end;

function TInstantParts.CreateObjectReference(
  AObject: TInstantObject): TInstantObjectReference;
begin
  Result := TInstantObjectReference.Create(AObject, True);
end;

destructor TInstantParts.Destroy;
begin
  if Metadata.StorageKind = skEmbedded then
    FObjectList.Free
  else
    FObjectReferenceList.Free;
  inherited;
end;

procedure TInstantParts.DestroyObject(Index: Integer);
begin
  if Metadata.StorageKind <> skEmbedded then
    ObjectReferences[Index].DestroyInstance;
end;

function TInstantParts.DestroyObject(AObject: TInstantObject): Boolean;
var
  Index: Integer;
begin
  if Metadata.StorageKind <> skEmbedded then
  begin
    Index := IndexOf(AObject);
    Result := Index >= 0;
    if Result then
      ObjectReferences[Index].DestroyInstance;
  end else
    Result := False;
end;

function TInstantParts.GetAllowOwned: Boolean;
begin
  Result := FAllowOwned;
end;

function TInstantParts.GetCount: Integer;
begin
  if Metadata.StorageKind = skEmbedded then
    Result := ObjectList.Count
  else
    Result := ObjectReferenceList.Count;
end;

function TInstantParts.GetInstances(Index: Integer): TInstantObject;
begin
  if Metadata.StorageKind = skEmbedded then
    Result := inherited GetInstances(Index)
  else
    Result := ObjectReferences[Index].Instance;
end;

function TInstantParts.GetIsChanged: Boolean;
var
  I: Integer;
begin
  Result := inherited GetIsChanged;
  if not Result then
    for I := 0 to Pred(Count) do
    begin
      Result := Items[I].IsChanged;
      if Result then
        Exit;
    end;
end;

function TInstantParts.GetObjectList: TObjectList;
begin
  if not Assigned(FObjectList) then
    FObjectList := TObjectList.Create;
  Result := FObjectList;
end;

function TInstantParts.GetObjectReferenceList: TObjectList;
begin
  if not Assigned(FObjectReferenceList) then
    FObjectReferenceList := TObjectList.Create;
  Result := FObjectReferenceList;
end;

function TInstantParts.GetObjectReferences(
  Index: Integer): TInstantObjectReference;
begin
  Result := TInstantObjectReference(ObjectReferenceList[Index]);
end;

function TInstantParts.InternalAdd(AObject: TInstantObject): Integer;
var
  Ref: TInstantObjectReference;
begin
  if Metadata.StorageKind = skEmbedded then
  begin
    Result := ObjectList.Add(AObject);
    SetOwnerContext(AObject);
  end
  else
  begin
    Ref := CreateObjectReference(AObject);
    try
      Result := ObjectReferenceList.Add(Ref);
      AObject.Free;
      SetOwnerContext(AObject);
    except
      Ref.Free;
      raise;
    end;
  end;
end;

procedure TInstantParts.InternalClear;
var
  I: Integer;
begin
  if Metadata.StorageKind = skEmbedded then
  begin
    for I := 0 to Pred(ObjectList.Count) do
      ClearOwnerContext(ObjectList[I] as TInstantObject);
    ObjectList.Clear;
  end
  else
  begin
    for I := 0 to Pred(ObjectReferenceList.Count) do
      if Assigned(ObjectReferences[I].Instance) then
        ClearOwnerContext(ObjectReferences[I].Instance);
    ObjectReferenceList.Clear;
  end;
end;

procedure TInstantParts.InternalDelete(Index: Integer);
var
  Ref: TInstantObjectReference;
begin
  if Metadata.StorageKind = skEmbedded then
  begin
    ClearOwnerContext(ObjectList[Index] as TInstantObject);
    ObjectList.Delete(Index)
  end
  else
  begin
    Ref := ObjectReferences[Index];
    if assigned(Ref.Instance) then
      ClearOwnerContext(Ref.Instance);
    ObjectReferenceList.Delete(Index);
  end;
end;

procedure TInstantParts.InternalExchange(Index1, Index2: Integer);
begin
  if Metadata.StorageKind = skEmbedded then
    ObjectList.Exchange(Index1, Index2)
  else
    ObjectReferenceList.Exchange(Index1, Index2);
end;

function TInstantParts.InternalGetItems(Index: Integer): TInstantObject;
var
  Ref: TInstantObjectReference;
begin
  if Metadata.StorageKind = skEmbedded then
    Result := ObjectList[Index] as TInstantObject
  else
  begin
    Ref := ObjectReferences[Index];
    if not Assigned(Ref.Instance) then
      Result := Ref.Dereference(Connector)
    else
      Result := Ref.Instance;
    if not Result.IsOwned then
      SetOwnerContext(Result);
  end;
end;

function TInstantParts.InternalIndexOf(AObject: TInstantObject): Integer;
var
  Ref: TInstantObjectReference;
begin
  if Metadata.StorageKind = skEmbedded then
    Result := ObjectList.IndexOf(AObject)
  else
  begin
    for Result := 0 to Pred(Count) do
    begin
      Ref := ObjectReferences[Result];
      if Ref.Equals(AObject) then
        Exit;
    end;
    Result := -1;
  end;
end;

function TInstantParts.InternalIndexOfInstance(Instance: Pointer): Integer;
var
  Ref: TInstantObjectReference;
begin
  if Metadata.StorageKind = skEmbedded then
    Result := InternalIndexOf(TInstantObject(Instance))
  else
  begin
    for Result := 0 to Pred(Count) do
    begin
      Ref := ObjectReferences[Result];
      if Ref.Instance = Instance then
        Exit;
    end;
    Result := -1;
  end;
end;

procedure TInstantParts.InternalInsert(Index: Integer; AObject: TInstantObject);
var
  Ref: TInstantObjectReference;
begin
  if Metadata.StorageKind = skEmbedded then
  begin
    ObjectList.Insert(Index, AObject);
    SetOwnerContext(AObject);
  end
  else
  begin
    Ref := CreateObjectReference(AObject);
    try
      ObjectReferenceList.Insert(Index, Ref);
      AObject.Free;
      SetOwnerContext(AObject);
    except
      Ref.Free;
      raise;
    end;
  end;
end;

procedure TInstantParts.InternalMove(CurIndex, NewIndex: Integer);
begin
  if Metadata.StorageKind = skEmbedded then
    ObjectList.Move(CurIndex, NewIndex)
  else
    ObjectReferenceList.Move(CurIndex, NewIndex);
end;

procedure TInstantParts.InternalSetItems(Index: Integer;
  AValue: TInstantObject);
begin
  if Metadata.StorageKind = skEmbedded then
  begin
    ClearOwnerContext(ObjectList[Index] as TInstantObject);
    ObjectList[Index] := AValue;
    SetOwnerContext(AValue);
  end
  else
    ObjectReferences[Index].Instance := AValue;
end;

procedure TInstantParts.ReadObject(Reader: TInstantReader);
var
  Obj: TPersistent;
begin
  Clear;
  ReadName(Reader);
  Reader.ReadValue;
  while not Reader.EndOfList do
  begin
    Obj := Reader.ReadObject(nil, Connector);
    try
      Add(Obj as TInstantObject);
    except
      Obj.Free;
      raise;
    end;
  end;
  Reader.ReadListEnd;
end;

procedure TInstantParts.SetAllowOwned(Value: Boolean);
begin
  FAllowOwned := Value;
end;

procedure TInstantParts.SetObjectReferences(Index: Integer;
  Value: TInstantObjectReference);
begin
  ObjectReferenceList[Index] := Value;
end;

procedure TInstantParts.SetOwnerContext(AObject: TInstantObject);
begin
  if Assigned(AObject) then
    AObject.SetOwnerContext(Owner, Self);
end;

procedure TInstantParts.Unchanged;
var
  I: Integer;
begin
  inherited;
  for I := 0 to Pred(Count) do
    Items[I].Unchanged;
end;

procedure TInstantParts.ValidateObject(AObject: TInstantObject);
begin
  inherited;
  if Metadata.StorageKind = skEmbedded then
    if AObject.IsPersistent then
      raise EInstantValidationError.CreateFmt(SPersistentObjectNotAllowed,
        [AObject.ClassName, AObject.Id]);
  if Assigned(Owner) and (AObject = Owner) then
    raise EInstantError.CreateFmt(SOwnershipRecursion,
      [AObject.ClassName, AObject.Id]);
end;

procedure TInstantParts.WriteObject(Writer: TInstantWriter);
var
  I: Integer;
begin
  WriteName(Writer);
  Writer.WriteValue(vaCollection);
  for I := 0 to Pred(Count) do
    Writer.WriteObject(Items[I]);
  Writer.WriteListEnd;
end;

{ TInstantReferences }

procedure TInstantReferences.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited;
  if Source is TInstantReferences then
  begin
    Clear;
    with TInstantReferences(Source) do
    begin
      for I := 0 to Pred(Count) do
      begin
        // cross-connector object assignment must be supported for InstantPump.
        if Connector <> Self.Connector then
          Self.Add(Items[I].Clone(Self.Connector))
        else
          Self.Add(Items[I]);
      end;
    end;
  end;
end;

class function TInstantReferences.AttributeType: TInstantAttributeType;
begin
  Result := atReferences;
end;

destructor TInstantReferences.Destroy;
begin
  FObjectReferenceList.Free;
  inherited;
end;

procedure TInstantReferences.DestroyObject(Index: Integer);
begin
  ObjectReferenceList.RefItems[Index].DestroyInstance;
end;

function TInstantReferences.DestroyObject(AObject: TInstantObject): Boolean;
var
  Index: Integer;
begin
  Index := ObjectReferenceList.IndexOfInstance(AObject);
  Result := Index >= 0;
  if Result then
    ObjectReferenceList.RefItems[Index].DestroyInstance;
end;

function TInstantReferences.GetAllowOwned: Boolean;
begin
  Result := FAllowOwned;
end;

function TInstantReferences.GetConnector: TInstantConnector;
begin
  if Assigned(FConnector) then
    Result := FConnector
  else
    Result := inherited GetConnector;
end;

function TInstantReferences.GetCount: Integer;
begin
  Result := ObjectReferenceList.Count;
end;

function TInstantReferences.GetInstances(Index: Integer): TInstantObject;
begin
  Result := ObjectReferenceList[Index];
end;

function TInstantReferences.GetObjectReferenceList: TInstantObjectReferenceList;
begin
  if not Assigned(FObjectReferenceList) then
    FObjectReferenceList := TInstantObjectReferenceList.Create(
            True, Connector, Self);
  Result := FObjectReferenceList;
end;

function TInstantReferences.GetRefItems(Index: Integer): TInstantObjectReference;
begin
  Result := ObjectReferenceList.RefItems[Index];
end;

function TInstantReferences.InternalAdd(AObject: TInstantObject): Integer;
begin
  Result := ObjectReferenceList.Add(AObject);
end;

procedure TInstantReferences.InternalClear;
begin
  ObjectReferenceList.Clear;
end;

procedure TInstantReferences.InternalDelete(Index: Integer);
begin
  ObjectReferenceList.Delete(Index);
end;

procedure TInstantReferences.InternalExchange(Index1, Index2: Integer);
begin
  ObjectReferenceList.Exchange(Index1, Index2);
end;

function TInstantReferences.InternalGetItems(Index: Integer): TInstantObject;
begin
  Result := ObjectReferenceList[Index];
end;

function TInstantReferences.InternalIndexOf(
  AObject: TInstantObject): Integer;
begin
  Result := ObjectReferenceList.IndexOf(AObject);
end;

function TInstantReferences.InternalIndexOfInstance(
  Instance: Pointer): Integer;
begin
  Result := ObjectReferenceList.IndexOf(TInstantObject(Instance));
end;

procedure TInstantReferences.InternalInsert(Index: Integer;
  AObject: TInstantObject);
begin
  ObjectReferenceList.Insert(Index, AObject);
end;

procedure TInstantReferences.InternalMove(CurIndex, NewIndex: Integer);
begin
  ObjectReferenceList.Move(CurIndex, NewIndex);
end;

procedure TInstantReferences.InternalSetItems(Index: Integer;
  AValue: TInstantObject);
begin
  ObjectReferenceList[Index] := AValue;
end;

procedure TInstantReferences.LoadObjectsFromStream(AStream: TStream);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Pred(Count) do
    if HasItem(I) then
      Items[I].Release;
end;

procedure TInstantReferences.LoadReferencesFromStream(AStream: TStream);
var
  Obj: TPersistent;
  ObjReference : TInstantObjectReference;
  Processor : TInstantXMLProcessor;
  ObjectEnd, ObjClassName, ObjId : string;
  XMLReferencesTag : string;
begin
  if not Assigned(AStream) then
    Exit;
  Clear;

  if Connector.BlobStreamFormat = sfBinary then
  begin
    while AStream.Position < AStream.Size do
    begin
      Obj := InstantReadObjectFromStream(AStream);
      try
        ObjReference := ObjectReferenceList.Add;
        try
          ObjReference.ReferenceObject(
                  TInstantObjectReference(Obj).ObjectClassName,
                  TInstantObjectReference(Obj).ObjectId);
        except
          ObjectReferenceList.Delete(Pred(Count));
          raise;
        end;
      finally
        Obj.Free;
      end;
    end;
  end
  else
  begin
    XMLReferencesTag := Self.ClassName;
    ObjectEnd := InstantBuildEndTag(XMLReferencesTag);
    Processor := TInstantXMLProcessor.Create(AStream);
    try
      //Skip XMLReferencesTag
      if not SameText(Processor.ReadTagName, XMLReferencesTag) then
        exit;
      while not SameText(Processor.PeekTag, ObjectEnd) do
      begin
        ObjClassName := Processor.ReadTagName; //Tag = classname
        ObjId := Processor.ReadData; //Data = ObjectId
        Processor.ReadTag; //closing tag
        ObjReference := ObjectReferenceList.Add;
        try
          ObjReference.ReferenceObject(ObjClassName, ObjId);
        except
          ObjectReferenceList.Delete(Pred(Count));
          raise;
        end;
      end;
    finally
      Processor.Free;
      end;
    end;
  Changed;
end;

procedure TInstantReferences.ReadObject(Reader: TInstantReader);
var
  Ref: TInstantObjectReference;
begin
  Clear;
  ReadName(Reader);
  Reader.ReadValue;
  while not Reader.EndOfList do
  begin
    Ref := ObjectReferenceList.Add;
    try
      Ref.ReadAsObject(Reader);
    except
      ObjectReferenceList.Delete(Pred(Count));
      raise;
    end;
  end;
  Reader.ReadListEnd;
end;

procedure TInstantReferences.SaveReferencesToStream(AStream: TStream);
var
  I: Integer;
  InstantXMLProducer : TInstantXMLProducer;
  XMLReferencesTag : string;
begin
  if not Assigned(AStream) then
    Exit;

  if Connector.BlobStreamFormat = sfBinary then
  begin
    for I := 0 to Pred(Count) do
      InstantWriteObjectToStream(AStream, ObjectReferenceList.RefItems[I]);
  end
  else
  begin
    if Count = 0 then exit;
    InstantXMLProducer := TInstantXMLProducer.Create(AStream);
    Try
      XMLReferencesTag := Self.ClassName;
      InstantXMLProducer.WriteStartTag(XMLReferencesTag);
      InstantXMLProducer.WriteEscapedData(sLineBreak);
      for I := 0 to Pred(Count) do
      begin
        InstantXMLProducer.WriteStartTag(
                ObjectReferenceList.RefItems[I].ObjectClassName);
        InstantXMLProducer.WriteData(ObjectReferenceList.RefItems[I].ObjectId);
        InstantXMLProducer.WriteEndTag;
      end;
      InstantXMLProducer.WriteEndTag;
    Finally
      InstantXMLProducer.Free;
    End;
  end;
end;

procedure TInstantReferences.SetAllowOwned(Value: Boolean);
begin
  FAllowOwned := Value;
end;

procedure TInstantReferences.ValidateObject(AObject: TInstantObject);
begin
  inherited;
  if Assigned(AObject) then
    AObject.CheckId;
end;

procedure TInstantReferences.WriteObject(Writer: TInstantWriter);
var
  I: Integer;
begin
  WriteName(Writer);
  Writer.WriteValue(vaCollection);
  for I := 0 to Pred(Count) do
    ObjectReferenceList.RefItems[I].WriteAsObject(Writer);
  Writer.WriteListEnd;
end;

{ TInstantObjectState }

procedure TInstantObjectState.Assign(Source: TPersistent);
begin
  if Source is TInstantObjectState then
    with TInstantObjectState(Source) do
    begin
      Self.FIsChanged := FIsChanged;
      Self.FPersistentId := FPersistentId;
      Self.FUpdateCount := FUpdateCount;
    end
  else
    inherited;
end;

{ TInstantObject }

procedure TInstantObject.Abandon;
begin
  SetConnector(nil);
end;

function TInstantObject.AddObject(AObject: TInstantObject): Integer;
begin
  Result := DefaultContainer.Add(AObject);
end;

function TInstantObject.AddRef: Integer;
begin
  Inc(FRefCount);
  Result:= FRefCount;
  AfterAddRef;
end;

procedure TInstantObject.AfterAddRef;
begin
end;

procedure TInstantObject.AfterAssign;
begin
end;

procedure TInstantObject.AfterContentChange(Container: TInstantContainer;
  ChangeType: TInstantContentChangeType; Index: Integer;
  AObject: TInstantObject);
begin
end;

procedure TInstantObject.AfterCreate;
begin
end;

procedure TInstantObject.AfterCreateAttributes;
begin
end;

procedure TInstantObject.AfterDispose;
begin
end;

procedure TInstantObject.AfterRefresh;
begin
end;

procedure TInstantObject.AfterRetrieve;
begin
end;

procedure TInstantObject.AfterStore;
begin
end;

procedure TInstantObject.ApplyState;
begin
  if FSaveStateLevel = 0 then
    Exit;
  if FSaveStateLevel = 1 then
    try
      SavedState.Assign(State);
    except
      on E: Exception do
        raise EInstantError.CreateFmt(SFatalError, [ClassName, Id, E.Message]);
    end;
  Dec(FSaveStateLevel);
end;

procedure TInstantObject.Assign(Source: TPersistent);
var
  I: Integer;
  Obj: TInstantObject;
  Map: TInstantAttributeMap;
  AttribName: string;
  Attrib, SourceAttrib: TInstantAttribute;
begin
  BeforeAssign;
  inherited;
  if Source is ClassType then
  begin
    Obj := TInstantObject(Source);
    Map := Obj.Metadata.MemberMap;
    if Assigned(Map) then
      with Map do
        for I := 0 to Pred(Count) do
        begin
          AttribName := Items[I].Name;
          Attrib := FindAttribute(AttribName);
          if Assigned(Attrib) then
          begin
            SourceAttrib := Obj.AttributeByName(AttribName);
            Attrib.Assign(SourceAttrib);
          end;
        end;
  end;
  AfterAssign;
end;

function TInstantObject.AttributeAddress(
  const AttributeName: string): PInstantAttribute;
begin
  Result := FieldAddress(InstantAttributePrefix + AttributeName);
end;

function TInstantObject.AttributeByName(
  const AttributeName: string): TInstantAttribute;
begin
  Result := FindAttribute(AttributeName);
  if not Assigned(Result) then
    raise EInstantError.CreateFmt(SAttributeNotFound, [AttributeName,
      ClassName]);
end;

procedure TInstantObject.AttributeChanged(Attribute: TInstantAttribute);
begin
end;

procedure TInstantObject.BeforeAssign;
begin
end;

procedure TInstantObject.BeforeContentChange(Container: TInstantContainer;
  ChangeType: TInstantContentChangeType; Index: Integer;
  AObject: TInstantObject);
begin
end;

procedure TInstantObject.BeforeCreateAttributes;
begin
end;

procedure TInstantObject.BeforeDispose;
begin
end;

procedure TInstantObject.BeforeRefresh;
begin
end;

procedure TInstantObject.BeforeRelease;
begin
end;

procedure TInstantObject.BeforeStore;
begin
end;

function TInstantObject.CanDispose: Boolean;
begin
  if IsOwned then
    Result := Owner.CanDispose
  else
    Result := CanStore;
end;

function TInstantObject.CanStore: Boolean;
begin
  Result := True;
end;

procedure TInstantObject.Changed;
begin
  DoChange;
end;

function TInstantObject.ChangesDisabled: Boolean;
begin
  Result := FChangesDisabledCount <> 0;
end;

procedure TInstantObject.CheckId;
begin
  if not InstantIsValidObjectId(Id) then
  begin
    if FId = '' then
      FId := GenerateId;
    if not InstantIsValidObjectId(Id) then
      raise EInstantError.CreateFmt(SInvalidObjectId, [Id, ClassName]);
  end;
end;

{$IFDEF FPC}
class function TInstantObject.ClassType: TInstantObjectClass;
{$ELSE}
function TInstantObject.ClassType: TInstantObjectClass;
{$ENDIF}
begin
  Result := TInstantObjectClass(inherited ClassType);
end;

procedure TInstantObject.ClearObjects;
begin
  DefaultContainer.Clear;
end;

procedure TInstantObject.ClearOwnerContext;
begin
  SetOwnerContext(nil, nil);
end;

constructor TInstantObject.Clone(Source: TInstantObject;
  AConnector: TInstantConnector);
begin
  Create(AConnector);
  if Assigned(Source) then
  begin
    Assign(Source);
    if not Source.IsOwned then
      Id := Source.Id;
  end;
end;

function TInstantObject.Clone(
  AConnector: TInstantConnector = nil): TInstantObject;
begin
  if not Assigned(AConnector) then
    AConnector := Connector;
  Result := ClassType.Clone(Self, AConnector);
end;

function TInstantObject.ContainerByName(
  const ContainerName: string): TInstantContainer;
begin
  Result := FindContainer(ContainerName);
  if not Assigned(Result) then
    if ContainerName = '' then
      raise EInstantError.CreateFmt(SDefaultContainerNotFound,
        [ClassName])
    else
      raise EInstantError.CreateFmt(SContainerNotFound, [ContainerName,
        ClassName]);
end;

class procedure TInstantObject.ConvertToBinary(Converter: TInstantTextToBinaryConverter);

  procedure ConvertAttribute(AttributeMetadata: TInstantAttributeMetadata);
  var
    Count: Integer;
    AttributeName, AttributeEnd, Bin, Hex: string;
  begin
    with Converter do
    begin
      AttributeName := Processor.ReadTagName;
      Writer.WriteStr(AttributeName);
      case AttributeMetadata.AttributeType of
        atInteger:
          Writer.WriteInteger(StrToInt(Processor.ReadData));
        atFloat:
          Writer.WriteFloat(StrToFloat(Processor.ReadData));
        atCurrency:
          Writer.WriteCurrency(StrToCurr(Processor.ReadData));
        atBoolean:
          Writer.WriteBoolean(SameText(Processor.ReadData, InstantTrueString));
        atString, atMemo:
          Writer.WriteString(Processor.ReadData);
        atDateTime:
          Writer.WriteDate(InstantStrToDateTime(Processor.ReadData));
        atBlob, atGraphic:
          begin
            Hex := Processor.ReadData;
            Count := Length(Hex) div 2;
            SetLength(Bin, Count);
            HexToBin(PChar(Hex), PChar(Bin), Count);
            Writer.WriteValue(vaBinary);
            Writer.Write(Count, SizeOf(Count));
            Writer.Write(Bin[1], Count);
          end;
        atPart, atReference:
          begin
            Writer.WriteIdent('');
            Convert;
          end;
        atReferences, atParts:
          begin
            Writer.WriteValue(vaCollection);
            AttributeEnd := InstantBuildEndTag(AttributeName);
            while not SameText(Processor.PeekTag, AttributeEnd) do
              Convert;
            Writer.WriteListEnd;
          end;
      end;
      Processor.ReadTag;
    end;
  end;

  procedure ConvertAttributes;
  var
    AttribMeta: TInstantAttributeMetadata;
    Map: TInstantAttributeMap;
    TagName, ObjectEnd: string;
  begin
    with Converter do
    begin
      ObjectEnd := InstantBuildEndTag(ObjectClassName);
      while not SameText(Processor.PeekTag, ObjectEnd) do
      begin
        InstantCheckClass(ObjectClass, TInstantObject);
        Map := TInstantObjectClass(ObjectClass).Metadata.MemberMap;
        if Assigned(Map) then
        begin
          TagName := Processor.PeekTagName;
          AttribMeta := Map.Find(TagName);
          if Assigned(AttribMeta) then
            ConvertAttribute(AttribMeta)
          else
            Processor.Skip;
        end else
          Processor.Skip;
      end;
      Writer.WriteListEnd;
    end;
  end;

  procedure ConvertId;
  begin
    with Converter do
    begin
      if Processor.PeekTagName = InstantIdFieldName then
      begin
        Processor.ReadTag;
        if (Processor.Token = xtTag) and SameText(Processor.ReadTagName,
          InstantBuildEndTag(InstantIdFieldName)) then
          Writer.WriteStr('')
        else begin
          Writer.WriteStr(Processor.ReadData);
          Processor.ReadTag;
        end;
      end else
        Writer.WriteStr('');
    end;
  end;

begin
  ConvertId;
  ConvertAttributes;
end;

class procedure TInstantObject.ConvertToText(
  Converter: TInstantBinaryToTextConverter);

  procedure ConvertValue;
  var
    Count: Integer;
    Bin, Hex: string;
  begin
    with Converter do
    begin
      case Reader.NextValue of
        vaInt8, vaInt16, vaInt32:
          Producer.WriteData(IntToStr(Reader.ReadInteger));
        vaExtended:
          Producer.WriteData(FloatToStr(Reader.ReadFloat));
        vaDate:
          Producer.WriteData(InstantDateTimeToStr(Reader.ReadDate));
        vaString, vaLString:
          Producer.WriteEscapedData(Reader.ReadString);
        vaCurrency:
          Producer.WriteEscapedData(CurrToStr(Reader.ReadCurrency));
        vaIdent:
          begin
            Reader.ReadIdent;
            Convert;
          end;
        vaFalse:
          begin
            Reader.ReadValue;
            Producer.WriteData(InstantFalseString);
          end;
        vaTrue:
          begin
            Reader.ReadValue;
            Producer.WriteData(InstantTrueString);
          end;
        vaBinary:
          begin
            Reader.ReadValue;
            Reader.Read(Count, SizeOf(Integer));
            SetLength(Bin, Count);
            SetLength(Hex, 2 * Count);
            Reader.Read(Bin[1], Count);
            BinToHex(PChar(Bin), PChar(Hex), Count);
            Producer.WriteData(Hex);
          end;
        vaCollection:
          begin
            Reader.ReadValue;
             while not Reader.EndOfList do
               Convert;
             Reader.ReadListEnd;
          end;
      else
        raise EInstantStreamError.Create(SInvalidValueType);
      end;
    end;
  end;

  procedure ConvertAttributes;
  begin
    with Converter do
    begin
      while not Reader.EndOfList do
      begin
        Producer.WriteStartTag(Reader.ReadStr);
        ConvertValue;
        Producer.WriteEndTag;
      end;
      Reader.ReadListEnd;
    end;
  end;

  procedure ConvertId;
  var
    ObjectId: string;
  begin
    with Converter do
    begin
      ObjectId := Reader.ReadStr;
      if ObjectId = '' then
        Exit;
      Producer.WriteStartTag(InstantIdFieldName);
      Producer.WriteEscapedData(ObjectId);
      Producer.WriteEndTag;
    end;
  end;

begin
  ConvertId;
  ConvertAttributes;
end;

constructor TInstantObject.Create(AConnector: TInstantConnector);
begin
  inherited Create;
  InstantCheckConnector(AConnector);
  SetConnector(AConnector);
  Init;
  DoAfterCreate;
end;

procedure TInstantObject.CreateAttributes;
var
  I: Integer;
  AttribMeta: TInstantAttributeMetadata;
  AttribPtr: PInstantAttribute;
begin
  with Metadata.MemberMap do
    for I := 0 to Pred(Count) do
    begin
      AttribMeta := Items[I];
      AttribPtr := AttributeAddress(AttribMeta.Name);
      if Assigned(AttribPtr) then
        if Assigned(AttribPtr^) then
        begin
          if not Assigned(AttribPtr^.Metadata) then
            AttribPtr^.Metadata := AttribMeta;
          if not Assigned(AttribPtr^.Owner) then
            AttribPtr^.SetOwner(Self);
        end else
          AttribPtr^ := AttribMeta.CreateAttribute(Self);
    end;
end;

class function TInstantObject.CreateInstance(
  Arg: Pointer): TInstantStreamable;
begin
  if Assigned(Arg) and not (TObject(Arg) is TInstantConnector) then
    raise EInstantError.CreateFmt(SInvalidArgument, [ClassName,
      TInstantConnector.ClassName]);
  Result := Create(Arg);
end;

procedure TInstantObject.DeleteObject(Index: Integer);
begin
  DefaultContainer.Delete(Index);
end;

procedure TInstantObject.DestroyAttributes;
var
  I: Integer;
  Attrib: TInstantAttribute;
begin
  with Metadata.MemberMap do
    for I := 0 to Pred(Count) do
    begin
      Attrib := FindAttribute(Items[I].Name);
      if Assigned(Attrib) and (Attrib.Owner = Self) then
        FreeAndNil(Attrib);
    end;
end;

procedure TInstantObject.DestroyInternalFields;
begin
  FreeAndNil(FRefBy);
  FreeAndNil(FState);
  FreeAndNil(FSavedState);
end;

procedure TInstantObject.Destruct;
begin
end;

procedure TInstantObject.DisableChanges;
begin
  Inc(FChangesDisabledCount);
end;

procedure TInstantObject.Dispose(ConflictAction: TInstantConflictAction);
begin
  PerformUpdate(DoDispose, otDispose, ConflictAction);
end;

procedure TInstantObject.DisposeOwnedObject(AObject: TInstantObject;
  ConflictAction: TInstantConflictAction);
begin
  if not Assigned(AObject) or (AObject.Owner <> Self) then
    Exit;
  if AObject.OwnerAttribute.DetachObject(AObject) and IsPersistent then
    Store(ConflictAction);
end;

procedure TInstantObject.DoAfterContentChange(Container: TInstantContainer;
  ChangeType: TInstantContentChangeType; Index: Integer;
  AObject: TInstantObject);
begin
  if ChangesDisabled then
    Exit;
  AfterContentChange(Container, ChangeType, Index, AObject);
  if Assigned(FOnAfterContentChange) then
    FOnAfterContentChange(Self, Container, ChangeType, Index, AObject);
end;

procedure TInstantObject.DoAfterCreate;
begin
  AfterCreate;
  ObjectNotifiers.Notify(Self, onCreated);
end;

procedure TInstantObject.DoAfterDispose;
begin
  AfterDispose;
  ObjectNotifiers.Notify(Self, onDisposed);
end;

procedure TInstantObject.DoAfterRefresh;
begin
  AfterRefresh;
  ObjectNotifiers.Notify(Self, onRefreshed);
end;

procedure TInstantObject.DoAfterRetrieve;
begin
  AfterRetrieve;
  ObjectNotifiers.Notify(Self, onRetrieved);
end;

procedure TInstantObject.DoAfterStore;
begin
  AfterStore;
  ObjectNotifiers.Notify(Self, onStored);
end;

procedure TInstantObject.DoAttributeChanged(Attribute: TInstantAttribute);
begin
  if ChangesDisabled then
    Exit;
  AttributeChanged(Attribute);
  if Assigned(FOnAttributeChanged) then
    FOnAttributeChanged(Self, Attribute);
  Changed;
end;

procedure TInstantObject.DoBeforeContentChange(
  Container: TInstantContainer; ChangeType: TInstantContentChangeType;
  Index: Integer; AObject: TInstantObject);
begin
  if ChangesDisabled then
    Exit;
  BeforeContentChange(Container, ChangeType, Index, AObject);
  if Assigned(FOnBeforeContentChange) then
    FOnBeforeContentChange(Self, Container, ChangeType, Index, AObject);
end;

procedure TInstantObject.DoBeforeDispose;
begin
  BeforeDispose;
end;

procedure TInstantObject.DoBeforeRefresh;
begin
  BeforeRefresh;
end;

procedure TInstantObject.DoBeforeStore;
begin
  BeforeStore;
end;

procedure TInstantObject.DoChange;
begin
  if ChangesDisabled then
    Exit;
  State.IsChanged := True;
  Inc(FChangeCount);
  if IsOwned then
    Owner.DoOwnedObjectChanged(Self);
  if Assigned(FOnChange) then
    FOnChange(Self);
  ObjectNotifiers.Notify(Self, onChanged);
end;

procedure TInstantObject.DoDispose(ConflictAction: TInstantConflictAction);
begin
  if not IsPersistent or IsAbandoned then
    Exit;
  case VerifyOperation(otDispose) of
    vrCancel:
      Exit;
    vrAbort:
      Abort;
    vrError:
      raise EInstantError.CreateFmt(SDeniedDispose, [ClassName, Id]);
  end;
  DoBeforeDispose;
  if IsOwned then
    Owner.DisposeOwnedObject(Self, ConflictAction)
  else
  begin
    DisableChanges;
    try
      ObjectStore.DisposeObject(Self, ConflictAction);
    finally
      EnableChanges;
    end;
  end;
  DoAfterDispose;
end;

procedure TInstantObject.DoHandleError(OperationType: TInstantOperationType;
  E: Exception; var Action: TInstantErrorAction);
begin
  if Assigned(FOnError) then
    FOnError(Self, OperationType, E, Action)
  else
    HandleError(OperationType, E, Action);
end;

procedure TInstantObject.DoOwnedObjectChanged(AObject: TInstantObject);
begin
  if ChangesDisabled then
    Exit;
  Changed;
  OwnedObjectChanged(AObject);
end;

procedure TInstantObject.DoOwnerChanged(OldOwner: TInstantObject;
  OldOwnerAttribute: TInstantComplex);
begin
  OwnerChanged(OldOwner, OldOwnerAttribute);
end;

procedure TInstantObject.DoRefresh;
begin
  if not IsPersistent or IsAbandoned then
    Exit;
  case VerifyOperation(otRefresh) of
    vrCancel:
      Exit;
    vrAbort:
      Abort;
    vrError:
      raise EInstantError.CreateFmt(SDeniedRefresh, [ClassName, Id]);
  end;
  DoBeforeRefresh;
  DisableChanges;
  try
    ObjectStore.RefreshObject(Self);
  finally
    EnableChanges;
  end;
  Unchanged;
  DoAfterRefresh;
end;

function TInstantObject.DoRelease: Integer;
begin
  if FRefCount > 0 then
  begin
    BeforeRelease;
    Dec(FRefCount);
  end;
  Result:= FRefCount;
end;

procedure TInstantObject.DoStore(ConflictAction: TInstantConflictAction);
begin
  if (IsPersistent and not IsChanged) or IsAbandoned then
    Exit;
  case VerifyOperation(otStore) of
    vrCancel:
       Exit;
     vrAbort:
       Abort;
     vrError:
       raise EInstantError.CreateFmt(SDeniedStore, [ClassName, Id]);
  end;
  DoBeforeStore;
  if not IsOwned then
  begin
    CheckId;
    DisableChanges;
    try
      ObjectStore.StoreObject(Self, ConflictAction);
    finally
      EnableChanges;
    end;
    Unchanged;
    FChangeCount := 0;
  end;
  DoAfterStore;
end;

procedure TInstantObject.DoUnchange;
var
  I: Integer;
begin
  State.IsChanged := False;
  with Metadata.MemberMap do
    for I := 0 to Pred(Count) do
      AttributeByName(Items[I].Name).Unchanged;
end;

procedure TInstantObject.EnableChanges;
begin
  if FChangesDisabledCount > 0 then
    Dec(FChangesDisabledCount);
end;

function TInstantObject.EqualsPersistentSignature(const AObjectClassName,
  AObjectId: string): Boolean;
begin
  Result := IsPersistent and (AObjectClassName = ClassName) and
    (AObjectId = PersistentId);
end;

function TInstantObject.EqualsSignature(const AObjectClassName,
  AObjectId: string): Boolean;
begin
  Result := (AObjectClassName = ClassName) and (AObjectId = Id);
end;

procedure TInstantObject.Finalize;
begin
end;

function TInstantObject.FindAttribute(
  const AttributeName: string): TInstantAttribute;
var
  AttribPtr: PInstantAttribute;
begin
  AttribPtr := AttributeAddress(AttributeName);
  if Assigned(AttribPtr) then
    Result := AttribPtr^
  else
    Result := nil;
end;

function TInstantObject.FindContainer(
  const ContainerName: string): TInstantContainer;
begin
  if ContainerName = '' then
    Result := FindDefaultContainer
  else
    Result := FindAttribute(ContainerName) as TInstantContainer;
end;

function TInstantObject.FindDefaultContainer: TInstantContainer;

  function FindDefaultContainerForClass(AClass: TInstantObjectClass): TInstantContainer;
  var
    ContainerAttributeName: String;
  begin
    ContainerAttributeName := AClass.Metadata.DefaultContainerName;
    if ContainerAttributeName = '' then
      Result := nil
    else
      Result := AttributeByName(ContainerAttributeName) as TInstantContainer;
  end;

var
  AClass: TInstantObjectClass;
begin
  AClass := ClassType;
  while AClass <> TInstantObject do
  begin
    Result := FindDefaultContainerForClass(AClass);
    if Assigned(Result) then
      Exit;
    AClass := TInstantObjectClass(AClass.Classparent);
  end;
  Result := nil;
end;

procedure TInstantObject.Finit;
begin
  try
    Finalize;
    if not IsPersistent then
      Destruct;
  except
    on E: Exception do
      if (E is EInstantError) or (E is EAbort) then
        raise
      else
        raise EInstantError.CreateFmt(SFinalizationFailed,
          [ClassName, Id, E.Message], E);
  end;
  if not IsAbandoned then
  begin
    if Connector.InTransaction then
      Connector.RemoveTransactedObject(Self);
    if IsPersistent then
      ObjectStore.ObjectDestroyed(Self);
  end;
  DestroyAttributes;
  DestroyInternalFields;
end;

procedure TInstantObject.FreeCircularReferences;
var
  CheckedObjects: TObjectList;

  function IsInsideCircularReference(const AItem: TInstantComplex): Boolean;

    function CanUnassign(const AOwner: TInstantObject): Boolean;
    var
      CurrentItemOwner: TInstantObject;
      I: Integer;
    begin
      Result := True;
      for I := 0 to Pred(AOwner.RefByCount) do
        if AOwner.FRefBy[I] is TInstantComplex then
        begin
          CurrentItemOwner := TInstantComplex(AOwner.FRefBy[I]).Owner;
          Result := TInstantComplex(AOwner.FRefBy[0]).Owner = CurrentItemOwner;
          if not Result and Assigned(CurrentItemOwner) and
           (CurrentItemOwner.RefByCount = 1) then
            Result := (CurrentItemOwner.RefCount = 1) or
             ((CurrentItemOwner.RefCount = 2) and (CurrentItemOwner = Self));
          if not Result then
            Exit;
        end;
    end;

  var
    ItemOwner: TInstantObject;
    I: Integer;
  begin
    Result := Assigned(AItem) and Assigned(AItem.Owner);
    if not Result then
      Exit;
    ItemOwner := AItem.Owner;
    Result := (ItemOwner = Self) or
     (CheckedObjects.IndexOf(ItemOwner) > -1) or
     IsInsideCircularReference(ItemOwner.OwnerAttribute);
    if not Result and Assigned(ItemOwner.FRefBy) and CanUnassign(ItemOwner) then
    begin
      CheckedObjects.Add(ItemOwner);
      for I := 0 to Pred(ItemOwner.RefByCount) do
        if ItemOwner.FRefBy[I] is TInstantComplex then
        begin
          Result := (ItemOwner.RefCount = ItemOwner.RefByCount) and
           IsInsideCircularReference(TInstantComplex(ItemOwner.FRefBy[I]));
          if Result then
            Exit;
        end;
      end;
  end;

var
  I: Integer;
begin
  CheckedObjects := TObjectList.Create(False);
  try
    if RefByCount = RefCount - 1 then
      for I := Pred(RefByCount) downto 0 do
        if (FRefBy[I] is TInstantComplex) and
         IsInsideCircularReference(TInstantComplex(FRefBy[I])) then
          case TInstantComplex(FRefBy[I]).AttributeType of
            atReference:
              TInstantReference(FRefBy[I]).ObjectReference.DestroyInstance;
            atReferences:
              TInstantReferences(FRefBy[I]).DestroyObject(Self);
          end;
  finally
    CheckedObjects.Free;
  end;
end;

procedure TInstantObject.FreeInstance;
begin
  FreeCircularReferences;
  DoRelease;
  if FRefCount = 0 then
    try
      Finit;
    finally
      inherited;
    end;
end;

function TInstantObject.GenerateId: string;
begin
  if IsAbandoned then
    Result := InstantGenerateId
  else
    Result := Connector.GenerateId;
end;

function TInstantObject.GetCaption: string;
begin
  Result := Id;
end;

function TInstantObject.GetClassId: string;
begin
  Result := ClassName;
end;

function TInstantObject.GetDefaultContainer: TInstantContainer;
begin
  if not Assigned(FDefaultContainer) then
    FDefaultContainer := FindDefaultContainer;
  Result := FDefaultContainer;
  if not Assigned(Result) then
    raise EInstantError.CreateFmt(SDefaultContainerNotSpecified, [ClassName]);
end;

function TInstantObject.GetHasDefaultContainer: Boolean;
begin
  Result := FindDefaultContainer <> nil;
end;

function TInstantObject.GetId: string;
begin
  Result := FId;
end;

function TInstantObject.GetIsAbandoned: Boolean;
begin
  Result := not Assigned(Connector);
end;

function TInstantObject.GetIsChanged: Boolean;
begin
  Result := State.IsChanged;
end;

function TInstantObject.GetIsDefault: Boolean;
var
  I: Integer;
begin
  Result := Id = '';
  if Result then
    with Metadata.MemberMap do
      for I := 0 to Pred(Count) do
      begin
        Result := AttributeByName(Items[I].Name).IsDefault;
        if not Result then
          Break;
      end;
end;

function TInstantObject.GetIsIdChanged: Boolean;
begin
  Result := Id <> PersistentId;
end;

function TInstantObject.GetIsOwned: Boolean;
begin
  Result := Assigned(FOwner);
end;

function TInstantObject.GetIsPersistent: Boolean;
begin
  Result := PersistentId <> '';
end;

function TInstantObject.GetNamePath: string;
begin
  Result := Format('%s.%s', [ClassName, Id]);
end;

function TInstantObject.GetObjectChangeCount: Integer;
begin
  Result := DefaultContainer.ChangeCount;
end;

function TInstantObject.GetObjectClass: TInstantObjectClass;
begin
  Result := DefaultContainer.RequiredClass;
end;

function TInstantObject.GetObjectCount: Integer;
begin
  Result := DefaultContainer.Count;
end;

function TInstantObject.GetObjects(Index: Integer): TInstantObject;
begin
  Result := DefaultContainer.Items[Index];
end;

function TInstantObject.GetObjectStore: TInstantObjectStore;
begin
  if not Assigned(FObjectStore) and not IsAbandoned then
    FObjectStore := Connector.EnsureObjectStore(ClassType);
  Result := FObjectStore;
end;

function TInstantObject.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TInstantObject.GetPersistentId: string;
begin
  Result := State.PersistentId;
end;

function TInstantObject.GetRefBy(Index: Integer): TInstantComplex;
begin
  Result := RefByList[Index] as TInstantComplex;
end;

function TInstantObject.GetRefByCount: Integer;
begin
  if Assigned(FRefBy) then
    Result := FRefBy.Count
  else
    Result := 0;
end;

function TInstantObject.GetRefByList: TObjectList;
begin
  if not Assigned(FRefBy) then
    FRefBy := TObjectList.Create(False);
  Result := FRefBy;
end;

function TInstantObject.GetSavedState: TInstantObjectState;
begin
  if not Assigned(FSavedState) then
    FSavedState := TInstantObjectState.Create;
  Result := FSavedState;
end;

function TInstantObject.GetState: TInstantObjectState;
begin
  if not Assigned(FState) then
    FState := TInstantObjectState.Create;
  Result := FState;
end;

function TInstantObject.GetUpdateCount: Integer;
begin
  Result := State.UpdateCount;
end;

procedure TInstantObject.HandleError(OperationType: TInstantOperationType;
  E: Exception; var Action: TInstantErrorAction);
begin
end;

function TInstantObject.IndexOfObject(AObject: TInstantObject): Integer;
begin
  Result := DefaultContainer.IndexOf(AObject);
end;

procedure TInstantObject.Init;
begin
  FRefCount := 1;
  BeforeCreateAttributes;
  CreateAttributes;
  AfterCreateAttributes;
  DisableChanges;
  try
    try
      Initialize;
    except
      on E: Exception do
        if (E is EInstantError) or (E is EAbort) then
          raise
        else
          raise EInstantError.CreateFmt(SInitializationFailed,
            [ClassName, E.Message], E);
    end;
  finally
    EnableChanges;
  end;
end;

procedure TInstantObject.Initialize;
begin
end;

procedure TInstantObject.InsertObject(Index: Integer; AObject: TInstantObject);
begin
  DefaultContainer.Insert(Index, AObject);
end;

function TInstantObject.Invoke(const MethodName: string;
  Params: TParams): string;
var
  Method: TInstantInvokableMethod;
  MethodAddr: Pointer;
begin
  MethodAddr := MethodAddress(MethodName);
  if Assigned(MethodAddr) then
  begin
    TMethod(Method).Code := MethodAddr;
    TMethod(Method).Data := Self;
    Result := Method(Params);
  end else
    Result := Format(SMethodNotFound, [MethodName, ClassName]);
end;

function TInstantObject.IsOperationAllowed(
  OperationType: TInstantOperationType): Boolean;
begin
  Result := VerifyOperation(OperationType) = vrOk;
end;

class function TInstantObject.Metadata: TInstantClassMetadata;
begin
  Result := InstantModel.ClassMetadatas.Find(ClassName);
  if not Assigned(Result) then
  begin
    Result := InstantModel.ClassMetadatas.Add;
    with Result do
    try
      Name := Self.ClassName;
    except
      InstantModel.ClassMetadatas.Remove(Result);
      raise;
    end;
  end;
end;

class function TInstantObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
end;

procedure TInstantObject.OwnedObjectChanged(AObject: TInstantObject);
begin
end;

procedure TInstantObject.OwnerChanged(OldOwner: TInstantObject;
  OldOwnerAttribute: TInstantComplex);
begin
end;

procedure TInstantObject.PerformUpdate(Operation: TInstantUpdateOperation;
 OperationType: TInstantOperationType; ConflictAction: TInstantConflictAction);
var
  Action: TInstantErrorAction;
  Transact: Boolean;
begin
  Transact := not Connector.InTransaction;
  if Transact then
    Connector.StartTransaction;
  SaveState;
  try
    Operation(ConflictAction);
    if Transact then
      Connector.CommitTransaction;
    ApplyState;
  except
    on E: Exception do
    begin
      RestoreState;
      if Transact then
        Connector.RollbackTransaction;
      if E is EAbort then
        raise
      else begin
        Action := eaError;
        DoHandleError(OperationType, E, Action);
        case Action of
          eaRetry:
            PerformUpdate(Operation, OperationType, ConflictAction);
          eaIgnore:
            PerformUpdate(Operation, OperationType, caFail);
          eaRevert:
            Refresh;
          eaCancel:
            ;
        else
          raise;
        end;
      end;
    end;
  end;
end;

function TInstantObject.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TInstantObject.ReadAttributes(Reader: TInstantReader);
var
  AttributeName: string;
  Attribute: TInstantAttribute;
  Pos: Integer;
begin
  with Reader do
  begin
    while not EndOfList do
    begin
      Pos := Position;
      AttributeName := ReadStr;
      Attribute := FindAttribute(AttributeName);
      if Assigned(Attribute) then
      begin
        Position := Pos;
        Attribute.ReadObject(Reader);
      end else
        Reader.SkipValue;
    end;
    ReadListEnd;
  end;
end;

procedure TInstantObject.ReadObject(Reader: TInstantReader);
begin
  DisableChanges;
  try
    Id := Reader.ReadStr;
    ReadAttributes(Reader);
  finally
    EnableChanges;
  end;
end;

procedure TInstantObject.Refresh;
var
  Action: TInstantErrorAction;
begin
  if IsOwned then
    Owner.Refresh
  else
    try
      SaveState;
      DoRefresh;
      ApplyState;
    except
      on E: Exception do
      begin
        RestoreState;
        if E is EAbort then
          raise
        else begin
          Action := eaError;
          DoHandleError(otRefresh, E, Action);
          case Action of
            eaRetry, eaRevert:
              Refresh;
            eaCancel, eaIgnore:
              ;
          else
            raise;
          end;
        end;
      end;
    end;
end;

class procedure TInstantObject.RefreshAll(AConnector: TInstantConnector;
  Progress: TInstantProgressEvent);
var
  I: Integer;
  Continue: Boolean;
  Store: TInstantObjectStore;
  Enumerator: TInstantCacheEnumerator;
  Node: TInstantCacheNode;
begin
  InstantCheckConnector(AConnector);
  Store := AConnector.EnsureObjectStore(Self);
  Enumerator := TInstantCacheEnumerator.Create(Store.FCache);
  try
    Continue := True;
    I := 0;
    Node := Enumerator.Next;
    while Assigned(Node) do
    begin
      Node.Item.Refresh;
      if Assigned(Progress) then
      begin
        Inc(I);
        Progress(Store, (I + 1) * 100 div Store.Count, Continue);
        if not Continue then
          Break;
      end;
      Node := Enumerator.Next;
    end;
  finally
    Enumerator.Free;
  end;
end;

function TInstantObject.Release: Integer;
begin
  Result := DoRelease;
end;

function TInstantObject.RemoveObject(AObject: TInstantObject): Integer;
begin
  Result := DefaultContainer.Remove(AObject);
end;

procedure TInstantObject.ResetAttributes;
var
  I: Integer;
begin
  with Metadata.MemberMap do
    for I := 0 to Pred(Count) do
      AttributeByName(Items[I].Name).Reset;
end;

procedure TInstantObject.RestoreState;
var
  vInCache: Boolean;
begin
  if FSaveStateLevel = 0 then
    Exit;
  if (FSaveStateLevel = 1) and not IsAbandoned then
    try
      vInCache := ObjectStore.Find(Self.PersistentId) = Self;
      if vInCache then
      begin
        ObjectStore.RemoveFromCache(Self);
      end;
      State.Assign(SavedState);
      if vInCache and IsPersistent then
        ObjectStore.AddToCache(Self);
    except
      on E: Exception do
        raise EInstantError.CreateFmt(SFatalError, [ClassName, Id, E.Message]);
    end;
  Dec(FSaveStateLevel);
end;

{$O-}
constructor TInstantObject.Retrieve(const AObjectId: string;
  CreateIfMissing: Boolean; Refresh: Boolean; AConnector: TInstantConnector);

  procedure RetrieveDenied;
  begin
    raise EInstantError.CreateFmt(SDeniedRetrieve, [ClassName, AObjectId]);
  end;

var
  Instance: TInstantObject;
  Exists: Boolean;
  VerificationResult: TInstantVerificationResult;
begin
  inherited Create;
  InstantCheckConnector(AConnector);
  SetConnector(AConnector);
  Instance := ObjectStore.Find(AObjectId);
  if Assigned(Instance) then
  begin
    inherited FreeInstance;
    Self := Instance;
    AddRef;
  end else
  begin
    Init;
    Exists := ObjectStore.RetrieveObject(AObjectId, Self);
    VerificationResult := VerifyOperation(otRetrieve);
    case VerificationResult of
      vrAbort:
        Abort;
      vrError:
        RetrieveDenied;
    end;
    if Exists and (VerificationResult = vrOk) then
      DoAfterRetrieve
    else if CreateIfMissing and (VerificationResult = vrOk) then
    begin
      Id := AObjectId;
      DoAfterCreate;
    end else
    begin
      Destroy;
      Self := nil;
      {$IFNDEF FPC} //UB it raise an error in FPC, surely there'd be better ways to accomplish this
      asm
        MOV     [EBP - $09], EAX // Avoid calling AfterConstruction
        POP     dword ptr fs:[$00000000]
        ADD     ESP, $0C
      end;
      {$ENDIF}
    end;
  end;
end;
{$O+}

procedure TInstantObject.SaveState;
begin
  if State.PersistentId = '' then
    Exit;
  if FSaveStateLevel = 0 then
    try
      SavedState.Assign(State);
      if not IsAbandoned and Connector.InTransaction then
      begin
        Connector.AddTransactedObject(Self);
        Inc(FSaveStateLevel);
      end;
    except
      on E: Exception do
        raise EInstantError.CreateFmt(SFatalError, [ClassName, Id, E.Message]);
    end;
  Inc(FSaveStateLevel);
end;

procedure TInstantObject.SetConnector(AConnector: TInstantConnector);
begin
  FConnector := AConnector;
  FObjectStore := nil;
end;

procedure TInstantObject.SetId(const Value: string);
begin
  if Value <> FId then
  begin
    FId := Value;
    Changed;
  end;
end;

procedure TInstantObject.SetIsChanged(Value: Boolean);
begin
  if Value <> IsChanged then
    if Value then
      Changed
    else
      Unchanged;
end;

procedure TInstantObject.SetObjects(Index: Integer; Value: TInstantObject);
begin
  DefaultContainer.Items[Index] := Value;
end;

procedure TInstantObject.SetOwnerContext(AOwner: TInstantObject;
  Attribute: TInstantComplex);
var
  OldOwner: TInstantObject;
  OldOwnerAttribute: TInstantComplex;
begin
  if (AOwner <> FOwner) or (Attribute <> FOwnerAttribute) then
  begin
    OldOwner := FOwner;
    OldOwnerAttribute := FOwnerAttribute;
    FOwner := AOwner;
    FOwnerAttribute := Attribute;
    DoOwnerChanged(OldOwner, OldOwnerAttribute);
  end;
end;

procedure TInstantObject.SetPersistentId(const Value: string);
begin
  State.PersistentId := Value;
  if Value = '' then
    Exit;
  DisableChanges;
  try
    Id := PersistentId;
  finally
    EnableChanges;
  end;
end;

procedure TInstantObject.SetUpdateCount(Value: Integer);
begin
  State.UpdateCount := Value;
end;

procedure TInstantObject.Store(ConflictAction: TInstantConflictAction);
begin
  PerformUpdate(DoStore, otStore, ConflictAction);
end;

procedure TInstantObject.Unchanged;
begin
  DoUnchange;
end;

function TInstantObject.VerifyOperation(
  OperationType: TInstantOperationType): TInstantVerificationResult;
begin
  Result := vrOk;
end;

procedure TInstantObject.WriteAttributes(Writer: TInstantWriter);
var
  I: Integer;
  Attrib: TInstantAttribute;
begin
  with Metadata.MemberMap do
    for I := 0 to Pred(Count) do
    begin
      Attrib := AttributeByName(Items[I].Name);
      if Attrib.IsMandatory or not Attrib.IsDefault then
        Attrib.WriteObject(Writer);
    end;
  Writer.WriteListEnd;
end;

procedure TInstantObject.WriteObject(Writer: TInstantWriter);
begin
  Writer.WriteStr(Id);
  WriteAttributes(Writer);
end;

function TInstantObject._AddRef: Integer;
begin
  Result := AddRef;
end;

function TInstantObject._Release: Integer;
begin
  FreeCircularReferences;
  Result := DoRelease;
  if FRefCount = 0 then
    try
      Finit;
    finally
      inherited FreeInstance;
    end;
end;

{ TInstantConnectionDef }

constructor TInstantConnectionDef.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FBlobStreamFormat := sfBinary;
  FIdDataType := dtString;
  FIdSize := InstantDefaultFieldSize;
end;

function TInstantConnectionDef.CreateConnector(AOwner: TComponent): TInstantConnector;
begin
  Result := ConnectorClass.Create(AOwner);
  try
    InitConnector(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TInstantConnectionDef.GetCaption: string;
var
  Connector: TInstantConnector;
begin
  Connector := CreateConnector(nil);
  try
    if Assigned(Connector) then
      Result := Connector.DatabaseName
    else
      Result := ''
  finally
    Connector.Free;
  end;
end;

procedure TInstantConnectionDef.InitConnector(Connector: TInstantConnector);
begin
  Connector.BlobStreamFormat := BlobStreamFormat;
  Connector.IdDataType := IdDataType;
  Connector.IdSize := IdSize;
end;

{ TInstantConnectionDefs }

constructor TInstantConnectionDefs.Create;
begin
  inherited Create(TInstantConnectionDef);
end;

function TInstantConnectionDefs.GetItems(Index: Integer): TInstantConnectionDef;
begin
  Result := inherited Items[Index] as TInstantConnectionDef;
end;

procedure TInstantConnectionDefs.SetItems(Index: Integer;
  const Value: TInstantConnectionDef);
begin
  inherited Items[Index] := Value;
end;

{ TInstantConnector }

procedure TInstantConnector.AbandonObjects;
var
  I: Integer;
  Obj: TObject;
begin
  for I := 0 to Pred(ClientCount) do
  begin
    Obj := Clients[I];
    if Obj is TInstantObjectStore then
      TInstantObjectStore(Obj).AbandonObjects;
  end;
end;

function TInstantConnector.AddTransactedObject(
  AObject: TInstantObject): Integer;
begin
  if Assigned(AObject) then
    Result := TransactedObjectList.Add(AObject)
  else
    Result := -1;
end;

procedure TInstantConnector.ApplyTransactedObjectStates;
var
  I: Integer;
begin
  for I := 0 to Pred(TransactedObjectCount) do
    TransactedObjects[I].ApplyState;
end;

procedure TInstantConnector.BuildDatabase(Model: TInstantModel);
var
  Scheme: TInstantScheme;
begin
  CreateDatabase;
  Connect;
  try
    if Model = nil then
      Model := InstantModel;
    Scheme := CreateScheme(Model);
    try
      DoBeforeBuildDatabase(Scheme);
      InternalBuildDatabase(Scheme);
    finally
      Scheme.Free;
    end;
  finally
    Disconnect;
  end;
end;

procedure TInstantConnector.BuildDatabase(
  AClasses: array of TInstantObjectClass);
var
  I: Integer;
  Model: TInstantModel;
begin
  Model := TInstantModel.Create;
  try
    for I := Low(AClasses) to High(AClasses) do
      if Assigned(AClasses[I]) then
        Model.ClassMetadatas.Add.Assign(AClasses[I].Metadata);
    BuildDatabase(Model);
  finally
    Model.Free;
  end;
end;

procedure TInstantConnector.ClearTransactedObjects;
begin
  TransactedObjectList.Clear;
end;

procedure TInstantConnector.CommitTransaction;
begin
  if not InTransaction then
    Exit;
  Dec(FTransactionLevel);
  if FTransactionLevel = 0 then
    try
      InternalCommitTransaction;
      ApplyTransactedObjectStates;
    finally
      ClearTransactedObjects;
    end;
end;

procedure TInstantConnector.Connect;
begin
  InternalConnect;
end;

constructor TInstantConnector.Create(AOwner: TComponent);
begin
  inherited;
  FUseTransactions := True;
  FIdDataType := dtString;
  FIdSize := InstantDefaultFieldSize;
end;

procedure TInstantConnector.CreateDatabase;
begin
  if Connected or DatabaseExists then
    Exit;
  InternalCreateDatabase;
end;

function TInstantConnector.CreateQuery: TInstantQuery;
begin
  Result := InternalCreateQuery;
  if not Assigned(Result) then
    raise EInstantError.Create(SCapabilityNotSuppported);
end;

function TInstantConnector.CreateScheme(Model: TInstantModel): TInstantScheme;
begin
  Result := InternalCreateScheme(Model);
end;

destructor TInstantConnector.Destroy;
begin
  AbandonObjects;
  IsDefault := False;
  FClientList.Free;
  FBroker.Free;
  FObjectStores.Free;
  FTransactedObjectList.Free;
  inherited;
end;

procedure TInstantConnector.Disconnect;
begin
  InternalDisconnect;
end;

procedure TInstantConnector.DoBeforeBuildDatabase(Scheme: TInstantScheme);
begin
  if Assigned(FBeforeBuildDatabase) then
    FBeforeBuildDatabase(Self, Scheme);
end;

function TInstantConnector.EnsureObjectStore(
  AClass: TInstantObjectClass): TInstantObjectStore;
begin
  Result := ObjectStores.FindObjectStore(AClass);
  if not Assigned(Result) then
  begin
    Result := ObjectStores.AddObjectStore;
    with Result do
    begin
      Connector := Self;
      ObjectClass := AClass;
    end;
  end;
end;

function TInstantConnector.GenerateId(const AObject: TInstantObject = nil): string;
begin
  Result := InternalGenerateId(AObject);
end;

function TInstantConnector.GetBroker: TInstantBroker;
begin
  if not Assigned(FBroker) then
    FBroker := CreateBroker;
  Result := FBroker;
end;

function TInstantConnector.GetClient(Index: Integer): TObject;
begin
  Result := ClientList[Index];
end;

function TInstantConnector.GetClientCount: Integer;
begin
  Result := ClientList.Count;
end;

function TInstantConnector.GetClientList: TList;
begin
  if not Assigned(FClientList) then
    FClientList := TList.Create;
  Result := FClientList;
end;

function TInstantConnector.GetConnected: Boolean;
begin
  Result := False;
end;

function TInstantConnector.GetDatabaseExists: Boolean;
begin
  Result := True;
end;

function TInstantConnector.GetDatabaseName: string;
begin
  Result := Broker.DatabaseName;
end;

function TInstantConnector.GetDDLTransactionSupported: Boolean;
begin
  Result := True;
end;

function TInstantConnector.GetInTransaction: Boolean;
begin
  Result := FTransactionLevel > 0;
end;

function TInstantConnector.GetInUse: Boolean;
begin
  Result := ClientCount > 0;
end;

function TInstantConnector.GetIsDefault: Boolean;
begin
  Result := Self = DefaultConnector;
end;

function TInstantConnector.GetObjectCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Pred(ObjectStores.Count) do
    Inc(Result, ObjectStores[I].Count);
end;

function TInstantConnector.GetObjects(Index: Integer): TInstantObject;
var
  I, EndIndex, Count: Integer;
  ObjectStore: TInstantObjectStore;
begin
  EndIndex := -1;
  for I := 0 to Pred(ObjectStores.Count) do
  begin
    ObjectStore := ObjectStores[I];
    Count := ObjectStore.Count;
    EndIndex := EndIndex + Count;
    if Index <= EndIndex then
    begin
      Result := ObjectStore.Items[Pred(Count - (EndIndex - Index))];
      Exit;
    end;
  end;
  raise EInstantError.CreateFmt(SIndexOutOfBounds, [Index]);
end;

function TInstantConnector.GetObjectStores: TInstantObjectStores;
begin
  if not Assigned(FObjectStores) then
    FObjectStores := TInstantObjectStores.Create;
  Result := FObjectStores;
end;

function TInstantConnector.GetTransactedObjectCount: Integer;
begin
  Result := TransactedObjectList.Count;
end;

function TInstantConnector.GetTransactedObjectList: TList;
begin
  if not Assigned(FTransactedObjectList) then
    FTransactedObjectList := TList.Create;
  Result := FTransactedObjectList;
end;

function TInstantConnector.GetTransactedObjects(Index: Integer): TInstantObject;
begin
  Result := TransactedObjectList[Index];
end;

procedure TInstantConnector.InternalBuildDatabase(Scheme: TInstantScheme);
begin
  if Assigned(Scheme) then
  begin
    Scheme.BlobStreamFormat := BlobStreamFormat;
    Scheme.IdDataType := IdDataType;
    Scheme.IdSize := IdSize;
  end;
  Broker.BuildDatabase(Scheme);
end;

procedure TInstantConnector.InternalCommitTransaction;
begin
end;

procedure TInstantConnector.InternalCreateDatabase;
begin
end;

function TInstantConnector.InternalCreateQuery: TInstantQuery;
begin
  Result := Broker.CreateQuery;
end;

function TInstantConnector.InternalGenerateId(const AObject: TInstantObject = nil): string;
begin
  if Assigned(FOnGenerateId) then
  begin
    Result := '';
    FOnGenerateId(Self, AObject, Result);
  end
  else
    Result := InstantGenerateId;
end;

procedure TInstantConnector.InternalRollbackTransaction;
begin
end;

procedure TInstantConnector.InternalStartTransaction;
begin
end;

class procedure TInstantConnector.RegisterClass;
begin
  ConnectorClasses.Add(Self);
end;

procedure TInstantConnector.RegisterClient(Client: TObject);
begin
  if not Assigned(Client) then
    Exit;
  ClientList.Add(Client);
end;

function TInstantConnector.RemoveTransactedObject(
  AObject: TInstantObject): Integer;
begin
  if Assigned(AObject) then
    Result := TransactedObjectList.Remove(AObject)
  else
    Result := -1
end;

procedure TInstantConnector.RestoreTransactedObjectStates;
var
  I: Integer;
begin
  for I := 0 to Pred(TransactedObjectCount) do
    TransactedObjects[I].RestoreState;
end;

procedure TInstantConnector.RollbackTransaction;
begin
  if not InTransaction then
    Exit;
  Dec(FTransactionLevel);
  if FTransactionLevel = 0 then
    try
      InternalRollbackTransaction;
      RestoreTransactedObjectStates;
    finally
      ClearTransactedObjects;
    end;
end;

procedure TInstantConnector.SetConnected(Value: Boolean);
begin
  if Value then
    Connect
  else
    Disconnect;
end;

procedure TInstantConnector.SetIsDefault(const Value: Boolean);
begin
  if Value <> IsDefault then
    if Value then
      DefaultConnector := Self
    else
      DefaultConnector := nil;
end;

procedure TInstantConnector.StartTransaction;
begin
  if not UseTransactions then
    Exit;
  Inc(FTransactionLevel);
  if FTransactionLevel = 1 then
    InternalStartTransaction;
end;

class procedure TInstantConnector.UnregisterClass;
begin
  ConnectorClasses.Remove(Self);
end;

procedure TInstantConnector.UnregisterClient(Client: TObject);
begin
  if not Assigned(Client) then
    Exit;
  ClientList.Remove(Client);
end;

{ TInstantCacheNode }

constructor TInstantCacheNode.Create(AItem: TInstantObject; ALeaf,
  AParent: TInstantCacheNode; AColor: TInstantCacheNodeColor);
begin
  inherited Create;
  FItem := AItem;
  FParent := AParent;
  FLeft := ALeaf;
  FRight := ALeaf;
  Color := AColor;
end;

function TInstantCacheNode.GetSibling: TInstantCacheNode;
begin
  if IsLeftChild then
    Result := Parent.Right
  else
    Result := Parent.Left;
end;

function TInstantCacheNode.IsBlack: Boolean;
begin
  Result := Color = ncBlack;
end;

function TInstantCacheNode.IsLeftChild: Boolean;
begin
  Result := Assigned(Parent) and (Parent.Left = Self);
end;

function TInstantCacheNode.IsRed: Boolean;
begin
  Result := Color = ncRed;
end;

function TInstantCacheNode.IsRightChild: Boolean;
begin
  Result := Assigned(Parent) and (Parent.Right = Self);
end;

{ TInstantCache }

procedure TInstantCache.Add(AObject: TInstantObject);
var
  Node: TInstantCacheNode;
  ParentNode: TInstantCacheNode;
  Compare: Integer;
begin
  ParentNode := Leaf;
  Node := Root;
  while Node <> Leaf do
  begin
    ParentNode := Node;
    Compare := CompareObjects(AObject, Node.Item);
    if Compare < 0 then
      Node := Node.Left
    else if Compare > 0 then
      Node := Node.Right
    else
      Exit;
  end;

  Node := TInstantCacheNode.Create(AObject, Leaf, ParentNode, ncRed);
  try
    if ParentNode = Leaf then
      Root := Node
    else if CompareObjects(AObject, ParentNode.Item) < 0 then
      ParentNode.Left := Node
    else
      ParentNode.Right := Node;
  except
    Node.Free;
    raise;
  end;

  Rebalance(Node);
  Root.Color := ncBlack;
  Inc(FCount);
end;

procedure TInstantCache.Clear;

  procedure ClearBranch(Node: TInstantCacheNode);
  begin
    if Node <> Leaf then
    begin
      ClearBranch(Node.Left);
      ClearBranch(Node.Right);
      Dec(FCount);
      Node.Free;
    end;
  end;

begin
  ClearBranch(Root);
  Root := Leaf;
end;

function TInstantCache.CompareIds(const Id1, Id2: string): Integer;
begin
  Result := CompareStr(Id1, Id2);
end;

function TInstantCache.CompareObjects(Object1,
  Object2: TInstantObject): Integer;
begin
  Result := CompareIds(Object1.PersistentId, Object2.PersistentId);
end;

constructor TInstantCache.Create;
begin
  inherited Create;
  FLeaf := TInstantCacheNode.Create(nil, nil, nil, ncBlack);
  FRoot := FLeaf;
end;

procedure TInstantCache.DeleteFixup(ANode: TInstantCacheNode);
var
  Temp: TInstantCacheNode;
begin
  while (ANode <> Root) and ANode.IsBlack do
  begin
    if ANode.IsLeftChild then
    begin
      Temp := ANode.Sibling;
      if Temp.IsRed then
      begin
        Temp.Color := ncBlack;
        ANode.Parent.Color := ncRed;
        LeftRotate(ANode.Parent);
        Temp := ANode.Parent.Right;
      end;
      if Temp.Left.IsBlack and Temp.Right.IsBlack then
      begin
        Temp.Color := ncRed;
        ANode := ANode.Parent;
      end else
      begin
        if Temp.Right.IsBlack then
        begin
          Temp.Left.Color := ncBlack;
          Temp.Color := ncRed;
          RightRotate(Temp);
          Temp := ANode.Parent.Right;
        end;
        Temp.Color := ANode.Parent.Color;
        ANode.Parent.Color := ncBlack;
        Temp.Right.Color := ncBlack;
        LeftRotate(ANode.Parent);
        ANode := Root;
      end;
    end else
    begin
      Temp := ANode.Sibling;
      if Temp.IsRed then
      begin
        Temp.Color := ncBlack;
        ANode.Parent.Color := ncRed;
        RightRotate(ANode.Parent);
        Temp := ANode.Parent.Left;
      end;
      if Temp.Right.IsBlack and Temp.Left.IsBlack then
      begin
        Temp.Color := ncRed;
        ANode := ANode.Parent;
      end else
      begin
        if Temp.Left.IsBlack then
        begin
          Temp.Right.Color := ncBlack;
          Temp.Color := ncRed;
          LeftRotate(Temp);
          Temp := ANode.Parent.Left;
        end;
        Temp.Color := ANode.Parent.Color;
        ANode.Parent.Color := ncBlack;
        Temp.Left.Color := ncBlack;
        RightRotate(ANode.Parent);
        ANode := Root;
      end;
    end;
  end;
  ANode.Color := ncBlack;
end;

destructor TInstantCache.Destroy;
begin
  Clear;
  Leaf.Free;
  inherited;
end;

function TInstantCache.Find(const AObjectId: string): TInstantObject;
begin
  Result := FindNode(AObjectId).Item;
end;

function TInstantCache.FindNode(const AObjectId: string): TInstantCacheNode;
var
  Compare: Integer;
begin
  Result := Root;
  while Result <> Leaf do
  begin
    Compare := CompareIds(AObjectId, Result.Item.PersistentId);
    if Compare < 0 then
      Result := Result.Left
    else if Compare > 0 then
      Result := Result.Right
    else
      Exit;
  end;
end;

procedure TInstantCache.ForEach(Proc: TInstantObjectProc);
var
  Enumerator: TInstantCacheEnumerator;
  Node: TInstantCacheNode;
begin
  if not Assigned(Proc) then
    Exit;
  Enumerator := TInstantCacheEnumerator.Create(Self);
  try
    Node := Enumerator.Next;
    while Assigned(Node) do
    begin
      Proc(Node.Item);
      Node := Enumerator.Next;
    end;
  finally
    Enumerator.Free;
  end;
end;

function TInstantCache.GetItems(Index: Integer): TInstantObject;
var
  I: Integer;
  Enumerator: TInstantCacheEnumerator;
  Node: TInstantCacheNode;
begin
  if Index >= Count then
    raise EInstantError.CreateFmt(SIndexOutOfBounds, [Index]);
  Enumerator := TInstantCacheEnumerator.Create(Self);
  try
    I := 0;
    Node := Enumerator.Next;
    while I < Index do
    begin
      Node := Enumerator.Next;
      Inc(I);
    end;
    Result := Node.Item;
  finally
    Enumerator.Free;
  end;
end;

procedure TInstantCache.LeftRotate(ANode: TInstantCacheNode);
var
  Child: TInstantCacheNode;
begin
  Child := ANode.Right;

  ANode.Right := Child.Left;
  if Child.Left <> Leaf then
    Child.Left.Parent := ANode;

  Child.Parent := ANode.Parent;
  if ANode.Parent = Leaf then // ANode is root
    Root := Child
  else if ANode.IsLeftChild then
    ANode.Parent.Left := Child
  else
    ANode.Parent.Right := Child;

  Child.Left := ANode;
  ANode.Parent := Child;
end;

function TInstantCache.Pred(ANode: TInstantCacheNode): TInstantCacheNode;
begin
  if ANode.Left <> Leaf then
  begin
    Result := ANode.Left;
    while Result.Right <> Leaf do
      Result := Result.Right;
  end else
  begin
    while (ANode <> Root) and ANode.IsLeftChild do
      ANode := ANode.Parent;
    if ANode = Root then
      Result := Leaf
    else
      Result := ANode.Parent;
  end;
end;

procedure TInstantCache.Rebalance(ANode: TInstantCacheNode);
var
  Node: TInstantCacheNode;
  ParentSibling: TInstantCacheNode;
begin
  Node := ANode;
  while (Node <> Root) and Node.Parent.IsRed do
  begin
    if Node.Parent.IsLeftChild then
    begin
      ParentSibling := Node.Parent.Sibling;
      if ParentSibling.IsRed then
      begin
        Node.Parent.Color := ncBlack;
        ParentSibling.Color := ncBlack;
        Node.Parent.Parent.Color := ncRed;
        Node := Node.Parent.Parent;
      end else
      begin
        if Node.Parent.Right = Node then
        begin
          Node := Node.Parent;
          LeftRotate(Node);
        end;
        Node.Parent.Color := ncBlack;
        Node.Parent.Parent.Color := ncRed;
        RightRotate(Node.Parent.Parent);
      end;
    end else
    begin
      ParentSibling := Node.Parent.Sibling;
      if ParentSibling.IsRed then
      begin
        Node.Parent.Color := ncBlack;
        ParentSibling.Color := ncBlack;
        Node.Parent.Parent.Color := ncRed;
        Node := Node.Parent.Parent;
      end else
      begin
        if Node.Parent.Left = Node then
        begin
          Node := Node.Parent;
          RightRotate(Node);
        end;
        Node.Parent.Color := ncBlack;
        Node.Parent.Parent.Color := ncRed;
        LeftRotate(Node.Parent.Parent);
      end;
    end;
  end;
end;

procedure TInstantCache.Remove(AObject: TInstantObject);
var
  ANode, Delete, Child: TInstantCacheNode;
begin
  ANode := FindNode(AObject.PersistentId);
  if (ANode.Left = Leaf) or (ANode.Right = Leaf) then
    Delete := ANode
  else
    Delete := Succ(ANode);

  if Delete.Left <> Leaf then
    Child := Delete.Left
  else
    Child := Delete.Right;

  Child.Parent := Delete.Parent;
  if Delete.Parent = Leaf then
    Root := Child
  else if Delete = Delete.Parent.Left then
    Delete.Parent.Left := Child
  else
    Delete.Parent.Right := Child;

  if Delete <> ANode then
    ANode.Item := Delete.Item;

  if Delete.IsBlack then
    DeleteFixup(Child);

  Delete.Free;
  Dec(FCount);
end;

procedure TInstantCache.RightRotate(ANode: TInstantCacheNode);
var
  Child: TInstantCacheNode;
begin
  Child := ANode.Left;

  ANode.Left := Child.Right;
  if Child.Right <> Leaf then
    Child.Right.Parent := ANode;

  Child.Parent := ANode.Parent;
  if ANode.Parent = Leaf then // ANode is root.
    Root := Child
  else if ANode.IsLeftChild then
    ANode.Parent.Left := Child
  else
    ANode.Parent.Right := Child;

  Child.Right := ANode;
  ANode.Parent := Child;
end;

function TInstantCache.Succ(ANode: TInstantCacheNode): TInstantCacheNode;
begin
  if ANode.Right <> Leaf then
  begin
    Result := ANode.Right;
    while Result.Left <> Leaf do
      Result := Result.Left;
  end else
  begin
    while (ANode <> Root) and (ANode.IsRightChild) do
      ANode := ANode.Parent;
    if ANode = Root then
      Result := Leaf
    else
      Result := ANode.Parent;
  end;
end;

{ TInstantCacheEnumerator }

constructor TInstantCacheEnumerator.Create(ACache: TInstantCache);
begin
  inherited Create;
  FCache := ACache;
  FNode := Cache.Leaf;
end;

function TInstantCacheEnumerator.HasNext: Boolean;
begin
  if Node = nil then
    Result := False
  else if Node <> Cache.Leaf then
    Result := Cache.Succ(Node) <> Cache.Leaf
  else
    Result := not (Cache.Count = 0)
end;

function TInstantCacheEnumerator.Next: TInstantCacheNode;
begin
  if Node = nil then
  begin
    Result := nil;
    Exit;
  end;

  if Node = Cache.Leaf then
  begin
    Node := Cache.Root;
    while (Node <> Cache.Leaf) and (Node.Left <> Cache.Leaf) do
      Node := Node.Left;
  end else
    Node := Cache.Succ(Node);

  if Node <> Cache.Leaf then
    Result := Node
  else begin
    Result := nil;
    Node := nil; //Finished
  end;
end;

procedure TInstantCacheEnumerator.Reset;
begin
  Node := Cache.Leaf;
end;

{ TInstantObjectStore }

procedure TInstantObjectStore.AbandonObject(AObject: TInstantObject);
begin
  if Assigned(AObject) then
    AObject.Abandon;
end;

procedure TInstantObjectStore.AbandonObjects;
begin
  FCache.ForEach(AbandonObject);
end;

procedure TInstantObjectStore.AddToCache(AObject: TInstantObject);
begin
  FCache.Add(AObject);
  if Assigned(Connector) and (Count = 1) then
    Connector.RegisterClient(Self);
end;

procedure TInstantObjectStore.CheckBroker(ABroker: TInstantBroker);
begin
  if not Assigned(ABroker) then
    raise EInstantError.Create(SUnassignedBroker);
end;

constructor TInstantObjectStore.Create(Collection: TCollection);
begin
  inherited;
  FCache := TInstantCache.Create;
end;

destructor TInstantObjectStore.Destroy;
begin
  FCache.Free;
  inherited;
end;

procedure TInstantObjectStore.DisposeObject(AObject: TInstantObject;
  ConflictAction: TInstantConflictAction);
begin
  if not (Assigned(AObject) and AObject.Metadata.IsStored) then
    Exit;
  CheckBroker(Broker);
  try
    Broker.DisposeObject(AObject, ConflictAction);
    RemoveFromCache(AObject);
    AObject.SetPersistentId('');
    AObject.SetUpdateCount(0);
  except
    on E: Exception do
      if (E is EInstantError) or (E is EAbort) then
        raise
      else
        raise EInstantError.CreateFmt(SErrorDisposingObject,
          [AObject.ClassName, AObject.Id, E.Message], E);
  end;
end;

function TInstantObjectStore.Find(const AObjectId: string): TInstantObject;
begin
  Result := FCache.Find(AObjectId);
end;

function TInstantObjectStore.GetBroker: TInstantBroker;
begin
  if Assigned(Connector) then
    Result := Connector.Broker
  else
    Result := nil;
end;

function TInstantObjectStore.GetCount: Integer;
begin
  Result := FCache.Count;
end;

function TInstantObjectStore.GetItems(Index: Integer): TInstantObject;
begin
  Result := FCache.Items[Index];
end;

function TInstantObjectStore.GetName: string;
begin
  if Assigned(Connector) and Assigned(ObjectClass) then
    Result := Connector.Name + ObjectClass.ClassName
  else
    Result := '';
end;

procedure TInstantObjectStore.ObjectDestroyed(AObject: TInstantObject);
begin
  if Assigned(AObject) and AObject.IsPersistent then
    RemoveFromCache(AObject);
end;

procedure TInstantObjectStore.RefreshObject(AObject: TInstantObject);
begin
  if not (Assigned(AObject) and AObject.Metadata.IsStored) then
    Exit;
  CheckBroker(Broker);
  try
    if Broker.RetrieveObject(AObject, AObject.PersistentId, caFail) then
      AObject.SetPersistentId(AObject.PersistentId)
    else begin
      RemoveFromCache(AObject);
      AObject.ResetAttributes;
      AObject.SetPersistentId('');
      AObject.SetUpdateCount(0);
    end;
  except
    on E: Exception do
      if (E is EInstantError) or (E is EAbort) then
        raise
      else
        raise EInstantError.CreateFmt(SErrorRefreshingObject,
          [AObject.ClassName, AObject.Id, E.Message], E);
  end;
end;

procedure TInstantObjectStore.RemoveFromCache(AObject: TInstantObject);
begin
  FCache.Remove(AObject);
  if Assigned(Connector) and (Count = 0) then
    Connector.UnregisterClient(Self);
end;

function TInstantObjectStore.RetrieveObject(const AObjectId: string;
  AObject: TInstantObject): Boolean;
begin
  Result := Assigned(AObject) and AObject.Metadata.IsStored;
  if not Result then
    Exit;
  CheckBroker(Broker);
  try
    AObject.DisableChanges;
    try
      Result := Broker.RetrieveObject(AObject, AObjectId, caFail);
      if Result then
        AObject.SetPersistentId(AObjectId)
      else begin
        AObject.SetPersistentId('');
        AObject.SetUpdateCount(0);
      end
    finally
      AObject.EnableChanges;
    end;
    if Result then
      AddToCache(AObject);
  except
    on E: Exception do
      if (E is EInstantError) or (E is EAbort) then
        raise
      else
        raise EInstantError.CreateFmt(SErrorRetrievingObject,
          [AObject.ClassName, AObjectId, E.Message], E);
  end;
end;

procedure TInstantObjectStore.StoreObject(AObject: TInstantObject;
  ConflictAction: TInstantConflictAction);
var
  MustAdd: Boolean;
begin
  if not (Assigned(AObject) and AObject.Metadata.IsStored) then
    Exit;
  CheckBroker(Broker);
  AObject.FInUpdate := True;
  try
    try
      if Broker.StoreObject(AObject, ConflictAction) then
      begin
        if AObject.IsPersistent then
        begin
          MustAdd := AObject.Id <> AObject.PersistentId;
          if MustAdd then
            RemoveFromCache(AObject);
        end else
          MustAdd := True;
        AObject.SetPersistentId(AObject.Id);
        if MustAdd then
          AddToCache(AObject);
      end;
    except
      on E: Exception do
        if (E is EInstantError) or (E is EAbort) then
          raise
        else
          raise EInstantError.CreateFmt(SErrorStoringObject,
            [AObject.ClassName, AObject.Id, E.Message], E);
    end;
  finally
    AObject.FInUpdate := False;
  end;
end;

{ TInstantObjectStores }

function TInstantObjectStores.AddObjectStore: TInstantObjectStore;
begin
  Result := TInstantObjectStore(inherited Add)
end;

constructor TInstantObjectStores.Create;
begin
  inherited Create(TInstantObjectStore);
end;

function TInstantObjectStores.FindObjectStore(
  AClass: TInstantObjectClass): TInstantObjectStore;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := Items[I];
    if Result.ObjectClass = AClass then
      Exit;
  end;
  Result := nil;
end;

function TInstantObjectStores.GetItems(Index: Integer): TInstantObjectStore;
begin
  Result := TInstantObjectStore(inherited Items[Index]);
end;

procedure TInstantObjectStores.SetItems(Index: Integer;
  Value: TInstantObjectStore);
begin
  Items[Index] := Value;
end;

{ TInstantQueryCommand }

function TInstantQueryCommand.FindAttributeMetadata(
  const PathText: string): TInstantAttributeMetadata;
var
  I: Integer;
  AClassMetadata: TInstantClassMetadata;
  List: TStringList;
  AttribName: string;
begin
  List := TStringList.Create;
  try
    AClassMetadata := ObjectClassMetadata;
    Result := nil;
    InstantStrToList(PathText, List, [InstantDot]);
    for I := 0 to Pred(List.Count) do
    begin
      AttribName := List[I];
      Result := AClassMetadata.MemberMap.Find(AttribName);
      if not Assigned(Result) then
        raise EInstantError.CreateFmt(SAttributeNotFound,
          [AttribName, AClassMetadata.Name]);
      if Result.Category = acElement then
        AClassMetadata := Result.ObjectClassMetadata;
    end;
  finally
    List.Free;
  end;
end;

function TInstantQueryCommand.GetObjectClassMetadata: TInstantClassMetadata;
begin
  Result := InstantFindClassMetadata(ObjectClassName);
end;

function TInstantQueryCommand.GetResultClassName: string;
var
  AttribMeta: TInstantAttributeMetadata;
begin
  if Assigned(Specifier) and Specifier.IsPath then
  begin
    AttribMeta := FindAttributeMetadata(Specifier.Text);
    if Assigned(AttribMeta) and (AttribMeta.Category = acElement) then
    begin
      Result := AttribMeta.ObjectClassName;
      Exit;
    end;
  end;
  Result := inherited GetResultClassName;
end;

{ TInstantQueryTranslator }

constructor TInstantQueryTranslator.Create(AQuery: TInstantQuery);
begin
  inherited Create;
  FQuery := AQuery;
end;

function TInstantQueryTranslator.CreateCommand: TInstantIQLCommand;
begin
  Result := TInstantQueryCommand.Create(nil);
end;

function TInstantQueryTranslator.GetQuery: TInstantQuery;
begin
  Result := FQuery;
  if not Assigned(Result) then
    raise EInstantError.Create(SUnassignedQuery);
end;

function TInstantQueryTranslator.GetResultClassName: string;
begin
  Result := (Command as TInstantQueryCommand).ResultClassName;
end;

{ TInstantBroker }

procedure TInstantBroker.BuildDatabase(Scheme: TInstantScheme);
begin
  InternalBuildDatabase(Scheme);
end;

constructor TInstantBroker.Create(AConnector: TInstantConnector);
begin
  inherited Create;
  FConnector := AConnector;
end;

function TInstantBroker.CreateDBBuildCommand(
  const CommandType: TInstantDBBuildCommandType): TInstantDBBuildCommand;
begin
  Result := TInstantUnsupportedDBBuildCommand.Create(CommandType,
    Connector);
end;

function TInstantBroker.CreateQuery: TInstantQuery;
begin
  Result := InternalCreateQuery;
end;

destructor TInstantBroker.Destroy;
begin
  inherited;
end;

function TInstantBroker.DisposeObject(AObject: TInstantObject;
  ConflictAction: TInstantConflictAction): Boolean;
begin
  Result := InternalDisposeObject(AObject, ConflictAction);
end;

function TInstantBroker.GetConnector: TInstantConnector;
begin
  Result := FConnector;
  if not Assigned(Result) then
    raise EInstantError.Create(SUnassignedConnector);
end;

function TInstantBroker.GetDatabaseName: string;
begin
  Result := '';
end;

function TInstantBroker.CreateCatalog(const AScheme: TInstantScheme): TInstantCatalog;
begin
  Result := nil;
end;

procedure TInstantBroker.InternalBuildDatabase(Scheme: TInstantScheme);
begin
end;

function TInstantBroker.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantQuery.Create(Connector);
end;

function TInstantBroker.ReadDatabaseScheme(
  const AWarningEventHandler: TInstantWarningEvent): TInstantScheme;
begin
  Result := TInstantScheme.Create;
  try
    Result.OnWarning := AWarningEventHandler;
    Result.Catalog := CreateCatalog(Result);
    if Result.Catalog = nil then
      raise Exception.CreateFmt(SUndefinedCatalog, [ClassName]);
  except
    Result.Free;
    raise;
  end;
end;

function TInstantBroker.RetrieveObject(AObject: TInstantObject;
  const AObjectId: string; ConflictAction: TInstantConflictAction): Boolean;
begin
  Result := InternalRetrieveObject(AObject, AObjectId, ConflictAction);
end;

procedure TInstantBroker.SetObjectUpdateCount(AObject: TInstantObject;
  Value: Integer);
begin
  if Assigned(AObject) then
    AObject.SetUpdateCount(Value);
end;

function TInstantBroker.StoreObject(AObject: TInstantObject;
  ConflictAction: TInstantConflictAction): Boolean;
begin
  Result := InternalStoreObject(AObject, ConflictAction);
end;

function TInstantBroker.IsCatalogSupported: Boolean;
var
  vCatalog: TInstantCatalog;
begin
  vCatalog := CreateCatalog(nil);
  try
    Result := Assigned(vCatalog);
  finally
    vCatalog.Free;
  end;
end;

{ TInstantQuery }

function TInstantQuery.AddObject(AObject: TObject): Integer;
begin
  Result := InternalAddObject(AObject);
end;

procedure TInstantQuery.ApplyChanges;
var
  I: Integer;
begin
  for I := 0 to Pred(ObjectCount) do
    if ObjectFetched(I) and (Objects[I] is TInstantObject) then
      TInstantObject(Objects[I]).Store;
end;

procedure TInstantQuery.Close;
begin
  InternalClose;
end;

constructor TInstantQuery.Create(AConnector: TInstantConnector);
begin
  inherited Create;
  FConnector := AConnector;
end;

procedure TInstantQuery.FetchParams(const ACommandText: string;
  AParams: TParams);
begin
  InternalFetchParams(AParams);
end;

function TInstantQuery.GetActive: Boolean;
begin
  Result := False;
end;

function TInstantQuery.GetConnector: TInstantConnector;
begin
  Result := FConnector;
  InstantCheckConnector(Result);
end;

function TInstantQuery.GetObjectClass: TClass;
begin
  Result := FindClass(ObjectClassName);
end;

function TInstantQuery.GetObjectClassName: string;
begin
  with TInstantQueryCommand.Create(nil) do
  try
    Text := Self.Command;
    Result := ResultClassName;
  finally
    Free;
  end;
end;

function TInstantQuery.GetObjectCount: Integer;
begin
  Result := InternalGetObjectCount;
  if (MaxCount <> 0) and (Result > MaxCount) then
    Result := MaxCount;
end;

function TInstantQuery.GetObjects(Index: Integer): TObject;
begin
  Result := InternalGetObjects(Index);
end;

function TInstantQuery.GetParams: TParams;
begin
  Result := nil;
end;

function TInstantQuery.HasChangedObject: Boolean;
var
  I: Integer;
begin
  for I := 0 to Pred(ObjectCount) do
    if ObjectFetched(I) and (Objects[I] is TInstantObject) then
    begin
      Result := TInstantObject(Objects[I]).IsChanged;
      if Result then
        Exit;
    end;
  Result := False;
end;

function TInstantQuery.HasObject(AObject: TObject): Boolean;
var
  I: Integer;
begin
  for I := 0 to Pred(ObjectCount) do
    if ObjectFetched(I) then
    begin
      Result := Objects[I] = AObject;
      if Result then
        Exit;
    end;
  Result := False;
end;

function TInstantQuery.IndexOfObject(AObject: TObject): Integer;
begin
  Result := InternalIndexOfObject(AObject);
end;

procedure TInstantQuery.InsertObject(Index: Integer; AObject: TObject);
begin
  InternalInsertObject(Index, AObject);
end;

function TInstantQuery.InternalAddObject(AObject: TObject): Integer;
begin
  Result := -1;
end;

procedure TInstantQuery.InternalClose;
begin
end;

procedure TInstantQuery.InternalFetchParams(AParams: TParams);

  procedure FetchParamsForObject(AObject: TInstantIQLObject);
  var
    I: Integer;
    Obj: TInstantIQLObject;
    ParamName: string;
  begin
    if Assigned(AObject) then
      for I := 0 to Pred(AObject.ObjectCount) do
      begin
        Obj := AObject.Objects[I];
        if Obj is TInstantIQLParam then
        begin
          ParamName := TInstantIQLParam(Obj).ParamName;
          if not Assigned(AParams.FindParam(ParamName)) then
            AParams.CreateParam(ftUnknown,ParamName, ptInput)
        end;
        FetchParamsForObject(Obj);
      end;
  end;

var
  ACommand: TInstantIQLCommand;
begin
  if Assigned(AParams) then
  begin
    ACommand := TInstantQueryCommand.Create(nil);
    try
      ACommand.Text := Self.Command;
      FetchParamsForObject(ACommand);
    finally
      ACommand.Free;
    end;
  end;
end;

function TInstantQuery.InternalGetObjectCount: Integer;
begin
  Result := 0;
end;

function TInstantQuery.InternalGetObjects(Index: Integer): TObject;
begin
  Result := nil;
end;

function TInstantQuery.InternalIndexOfObject(AObject: TObject): Integer;
begin
  Result := -1;
end;

procedure TInstantQuery.InternalInsertObject(Index: Integer;
  AObject: TObject);
begin
end;

procedure TInstantQuery.InternalOpen;
begin
end;

procedure TInstantQuery.InternalRefreshObjects;
begin
end;

procedure TInstantQuery.InternalReleaseObject(AObject: TObject);
begin
end;

function TInstantQuery.InternalRemoveObject(AObject: TObject): Integer;
begin
  Result := -1;
end;

function TInstantQuery.ObjectFetched(Index: Integer): Boolean;
begin
  Result := True;
end;

procedure TInstantQuery.Open;
begin
  if Command <> '' then
    TranslateCommand;
  InternalOpen;
end;

procedure TInstantQuery.RefreshObjects;
begin
  InternalRefreshObjects;
end;

procedure TInstantQuery.ReleaseObject(AObject: TObject);
begin
  InternalReleaseObject(AObject);
end;

function TInstantQuery.RemoveObject(AObject: TObject): Integer;
begin
  Result := InternalRemoveObject(AObject);
end;

procedure TInstantQuery.SetActive(Value: Boolean);
begin
  if Value <> Active then
    if Value then
      Open
    else
      Close;
end;

procedure TInstantQuery.SetCommand(const Value: string);
begin
  if Value <> FCommand then
  begin
    FCommand := Value;
    TranslateCommand;
  end;
end;

procedure TInstantQuery.SetParams(Value: TParams);
begin
end;

procedure TInstantQuery.TranslateCommand;
begin
end;

{ TInstantRelationalConnector }

procedure TInstantRelationalConnector.DoGetDataSet(const CommandText: string;
  var DataSet: TDataSet);
begin
  if Assigned(FOnGetDataSet) then
    FOnGetDataSet(Self, CommandText, DataSet)
  else
    GetDataSet(CommandText, DataSet);
end;

procedure TInstantRelationalConnector.DoInitDataSet(
  const CommandText: string; DataSet: TDataSet);
begin
  if Assigned(FOnInitDataSet) then
    FOnInitDataSet(Self, CommandText, DataSet)
  else
    InitDataSet(CommandText, DataSet);
end;

function TInstantRelationalConnector.GetBroker: TInstantCustomRelationalBroker;
begin
  Result := inherited Broker as TInstantCustomRelationalBroker;
end;

procedure TInstantRelationalConnector.GetDataSet(const CommandText: string;
  var DataSet: TDataSet);
begin
end;

function TInstantRelationalConnector.GetDBMSName: string;
begin
  Result := Broker.DBMSName;
end;

procedure TInstantRelationalConnector.InitDataSet(const CommandText: string;
  DataSet: TDataSet);
begin
end;

function TInstantRelationalConnector.InternalCreateScheme(
  Model: TInstantModel): TInstantScheme;
begin
  Result := TInstantScheme.Create;
  Result.IdDataType := IdDataType;
  Result.IdSize     := IdSize;      
  Result.Catalog := TInstantModelCatalog.Create(Result, Model);
end;

{ TInstantRelationalTranslator }

function TInstantRelationalTranslator.AddCriteria(
  const Criteria: string): Integer;
begin
  if IndexOfCriteria(Criteria) = -1 then
    Result := CriteriaList.Add(Criteria)
  else
    Result := -1;
end;

procedure TInstantRelationalTranslator.AddJoin(const FromPath, FromField,
  ToPath, ToField: string);
begin
  AddCriteria(Format('%s = %s', [Qualify(FromPath, FromField),
    Qualify(ToPath, ToField)]));
end;

function TInstantRelationalTranslator.AddTablePath(
  const TablePath: string): Integer;
begin
  if IndexOfTablePath(TablePath) = -1 then
    Result := TablePathList.Add(TablePath)
  else
    Result := -1;
end;

procedure TInstantRelationalTranslator.BeforeTranslate;

  procedure InitClassTablePath(List: TList);

    function FindAttributePath: TInstantIQLPath;
    var
     I: Integer;
    begin
      for I := 0 to Pred(List.Count) do
      begin
        Result := List[I];
        if not IsRootAttribute(Result.Text) then
          Exit;
      end;
      Result := nil;
    end;

  var
   TablePath: string;
   Path: TInstantIQLPath;
  begin
    if Command.ClassRef.Any then
      TablePath := ObjectClassMetadata.TableName
    else begin
      Path := FindAttributePath;
      if Assigned(Path) then
        TablePath := PathToTablePath(Path.Attributes[0])
      else
        TablePath := ObjectClassMetadata.TableName;
    end;
    AddTablePath(TablePath);
  end;

  procedure InitCommandCriterias;
  begin
    if not Command.ClassRef.Any then
      AddCriteria(Format('%s = %s',
        [Qualify(ClassTablePath, InstantClassFieldName),
        QuoteString(Command.ClassRef.ObjectClassName)]));
    if Command.Specifier.IsPath then
      AddCriteria(Format('%s <> %s%s',
        [QualifyPath(ConcatPath(Command.Specifier.Text, InstantIdFieldName)),
          Quote, Quote]));
  end;

var
  I: Integer;
  PathList: TList;
begin
  if not Assigned(Command.ClassRef) then
    Exit;
  PathList := TList.Create;
  try
    CollectPaths(Command, PathList);
    InitClassTablePath(PathList);
    for I := 0 to Pred(PathList.Count) do
    begin
      MakeTablePaths(PathList[I]);
      MakeJoins(PathList[I]);
    end;
    InitCommandCriterias;
  finally
    PathList.Free;
  end;
end;

procedure TInstantRelationalTranslator.Clear;
begin
  inherited;
  DestroyTablePathList;
  DestroyCriteriaList;
end;

procedure TInstantRelationalTranslator.CollectObjects(
  AObject: TInstantIQLObject; AClassType: TInstantIQLObjectClass; AList: TList);
var
  I: Integer;
  Obj: TInstantIQLObject;
begin
  if not (Assigned(AObject) and Assigned(AList)) then
    Exit;
  for I := 0 to Pred(AObject.ObjectCount) do
  begin
    Obj := AObject[I];
    if Obj is AClassType then
      AList.Add(Obj);
    CollectObjects(Obj, AClassType, AList)
  end;
end;

procedure TInstantRelationalTranslator.CollectPaths(
  AObject: TInstantIQLObject; APathList: TList);
begin
  CollectObjects(AObject, TInstantIQLPath, APathList);
end;

function TInstantRelationalTranslator.ConcatPath(const APathText,
  AttribName: string): string;
begin
  Result := Format('%s%s%s', [APathText, InstantDot, AttribName]);
end;

destructor TInstantRelationalTranslator.Destroy;
begin
  DestroyTablePathList;
  DestroyCriteriaList;
  inherited;
end;

procedure TInstantRelationalTranslator.DestroyCriteriaList;
begin
  FreeAndNil(FCriteriaList);
end;

procedure TInstantRelationalTranslator.DestroyTablePathList;
begin
  FreeAndNil(FTablePathList)
end;

function TInstantRelationalTranslator.ExtractTarget(
  const PathStr: string): string;
var
  I: Integer;
begin
  I := InstantRightPos(InstantDot, PathStr);
  Result := Copy(PathStr, I + 1, Length(PathStr) - I)
end;

function TInstantRelationalTranslator.GetClassTablePath: string;
begin
  Result := TablePaths[0];
end;

function TInstantRelationalTranslator.GetConnector: TInstantRelationalConnector;
begin
  if HasConnector then
    Result := Query.Connector
  else
    Result := nil;
end;

function TInstantRelationalTranslator.GetCriteriaCount: Integer;
begin
  Result := CriteriaList.Count;
end;

function TInstantRelationalTranslator.GetCriteriaList: TStringList;
begin
  if not Assigned(FCriteriaList) then
    FCriteriaList := TStringList.Create;
  Result := FCriteriaList;
end;

function TInstantRelationalTranslator.GetCriterias(Index: Integer): string;
begin
  Result := CriteriaList.Strings[Index];
end;

function TInstantRelationalTranslator.GetDelimiters: string;
begin
  if HasConnector then
    Result := Connector.Broker.SQLDelimiters
  else
    Result := '';
end;

function TInstantRelationalTranslator.GetObjectClassMetadata: TInstantClassMetadata;
begin
  Result := InternalGetObjectClassMetadata;
  if not Assigned(Result) then
    raise EInstantError.CreateFmt(SUnassignedClassMetadata,
     [Command.ObjectClassName]);
end;

function TInstantRelationalTranslator.GetQuery: TInstantCustomRelationalQuery;
begin
  Result := inherited Query as TInstantCustomRelationalQuery;
end;

function TInstantRelationalTranslator.GetQuote: Char;
begin
  if HasConnector then
    Result := Connector.Broker.SQLQuote
  else
    Result := '"';
end;

function TInstantRelationalTranslator.GetTablePathAliases(
  Index: Integer): string;
begin
  if Index < TablePathList.Count then
    Result := Format('t%d', [Succ(Index)])
  else
    Result := '';
end;

function TInstantRelationalTranslator.GetTablePathCount: Integer;
begin
  Result := TablePathList.Count;
end;

function TInstantRelationalTranslator.GetTablePathList: TStringList;
begin
  if not Assigned(FTablePathList) then
    FTablePathList := TStringList.Create;
  Result := FTablePathList;
end;

function TInstantRelationalTranslator.GetTablePaths(
  Index: Integer): string;
begin
  Result := TablePathList[Index];
end;

function TInstantRelationalTranslator.GetWildcard: string;
begin
  if HasConnector then
    Result := Connector.Broker.SQLWildcard
  else
    Result := '%';
end;

function TInstantRelationalTranslator.HasConnector: Boolean;
begin
  Result := Assigned(Query) and Assigned(Query.Connector);
end;

function TInstantRelationalTranslator.IncludeOrderFields: Boolean;
begin
  Result := False;
end;

function TInstantRelationalTranslator.IndexOfCriteria(
  const Criteria: string): Integer;
begin
  Result := CriteriaList.IndexOf(Criteria);
end;

function TInstantRelationalTranslator.IndexOfTablePath(
  const TablePath: string): Integer;
begin
  Result := TablePathList.IndexOf(TablePath);
end;

function TInstantRelationalTranslator.InternalGetObjectClassMetadata: TInstantClassMetadata;
begin
  if Command is TInstantQueryCommand then
    Result := TInstantQueryCommand(Command).ObjectClassMetadata
  else
    Result := nil;
end;

function TInstantRelationalTranslator.IsPrimary(
  AObject: TInstantIQLObject): Boolean;
begin
  Result := Assigned(AObject) and Assigned(Command) and
    ((AObject = Command) or (AObject.Owner = Command));
end;

function TInstantRelationalTranslator.IsRootAttribute(
  const AttributeName: string): Boolean;
begin
  Result := SameText(AttributeName, InstantClassFieldName) or
    SameText(AttributeName, InstantIdFieldName);
end;

procedure TInstantRelationalTranslator.MakeJoins(Path: TInstantIQLPath);

  procedure MakePathJoins(Path: TInstantIQLPath);
  var
    I: Integer;
    PathText, FromPath, ToPath, FromField, ToField: string;
  begin
    if Path.AttributeCount > 1 then
    begin
      PathToTarget(Path.SubPath[0], FromPath, FromField);
      for I := 1 to Pred(Path.AttributeCount) do
      begin
        PathText := Path.SubPath[I];
        if not IsRootAttribute(ExtractTarget(PathText)) then
        begin
          PathToTarget(PathText, ToPath, ToField);
          AddJoin(FromPath, FromField + InstantClassFieldName, ToPath,
            InstantClassFieldName);
          AddJoin(FromPath, FromField + InstantIdFieldName, ToPath,
            InstantIdFieldName);
          FromPath := ToPath;
          FromField := ToField;
        end;
      end;
    end;
  end;

  procedure MakeClassJoin(Path: TInstantIQLPath);
  var
    TablePath: string;
  begin
    if Path.HasAttributes then
    begin
      TablePath := PathToTablePath(Path.SubPath[0]);
      if TablePath <> ClassTablePath then
      begin
        AddJoin(ClassTablePath, InstantClassFieldName,
          TablePath, InstantClassFieldName);
        AddJoin(ClassTablePath, InstantIdFieldName,
          TablePath, InstantIdFieldName);
      end;
    end;
  end;

begin
  if not Assigned(Path) then
    Exit;
  MakeClassJoin(Path);
  MakePathJoins(Path);
end;

procedure TInstantRelationalTranslator.MakeTablePaths(Path: TInstantIQLPath);
var
  I: Integer;
  TablePath: string;
begin
  if not Assigned(Path) or IsRootAttribute(Path.Text) then
    Exit;
  for I := 0 to Pred(Path.AttributeCount) do
  begin
    TablePath := PathToTablePath(Path.SubPath[I]);
    if IndexOfTablePath(TablePath) = -1 then
      AddTablePath(TablePath);
  end;
end;

function TInstantRelationalTranslator.PathToTablePath(
  const PathText: string): string;
var
  FieldName: string;
begin
  PathToTarget(Pathtext, Result, FieldName);
end;

function TInstantRelationalTranslator.PathToTarget(const PathText: string;
  out TablePath, FieldName: string): TInstantAttributeMetadata;
var
  I: Integer;
  AttribList: TStringList;
  ClassMeta: TInstantClassMetadata;
  AttribName: string;
  Map: TInstantAttributeMap;
begin
  Result := nil;
  if IsRootAttribute(PathText) then
  begin
    TablePath := ClassTablePath;
    FieldName := RootAttribToFieldName(PathText);
  end else
  begin
    ClassMeta := ObjectClassMetadata;
    AttribList := TStringList.Create;
    try
      InstantStrToList(PathText, AttribList, [InstantDot]);
      for I := 0 to Pred(AttribList.Count) do
      begin
        AttribName := AttribList[I];
        if IsRootAttribute(AttribName) then
        begin
          if Assigned(Result) and
            not Result.IsAttributeClass(TInstantReference) then
            raise EInstantError.CreateFmt(SUnableToQueryAttribute,
              [Result.ClassMetadataName, Result.Name]);
          FieldName := FieldName + RootAttribToFieldName(AttribName);
        end else
        begin
          Result := ClassMeta.MemberMap.Find(AttribName);
          if Assigned(Result) then
          begin
            while Assigned(ClassMeta) and not ClassMeta.IsStored do
              ClassMeta := ClassMeta.Parent;
            if Assigned(ClassMeta) then
            begin
              Map := ClassMeta.StorageMaps.FindMap(AttribName);
              if Assigned(Map) then
              begin
                if I > 0 then
                  TablePath := TablePath + InstantDot;
                TablePath := TablePath + Map.Name;
                FieldName := Result.FieldName;
                ClassMeta := Result.ObjectClassMetadata;
              end else
                raise EInstantError.CreateFmt(SAttributeNotQueryable,
                  [Result.ClassName, Result.Name, Result.ClassMetadataName]);
            end else
              raise EInstantError.CreateFmt(SClassNotQueryable,
                [Result.ClassMetadataName]);
          end else
            raise EInstantError.CreateFmt(SAttributeNotFound,
              [AttribName, ClassMeta.Name]);
        end;
      end;
    finally
      AttribList.Free;
    end;
  end;
end;

function TInstantRelationalTranslator.Qualify(const TablePath,
  FieldName: string): string;
begin
  Result := Format('%s.%s', [TablePathToAlias(TablePath),
    InstantEmbrace(FieldName, Delimiters)]);
end;

function TInstantRelationalTranslator.QualifyPath(const
  PathText: string): string;
var
  TablePath, FieldName: string;
begin
  PathToTarget(PathText, TablePath, FieldName);
  Result := Qualify(TablePath, FieldName);
end;

function TInstantRelationalTranslator.QuoteString(const Str: string): string;
begin
  Result := InstantQuote(Str, Quote);
end;

function TInstantRelationalTranslator.ReplaceWildcard(
  const Str: string): string;
var
  S: string;
begin
  if Wildcard <> '%' then
  begin
    S := StringReplace(Str, '%%', #1, [rfReplaceAll]);
    S := StringReplace(S, '%', WildCard, [rfReplaceAll]);
    Result := StringReplace(S, #1, '%', [rfReplaceAll]);
  end else
    Result := Str;
end;

function TInstantRelationalTranslator.RootAttribToFieldName(
  const AttribName: string): string;
begin
  if SameText(AttribName, InstantClassFieldName) then
    Result := InstantClassFieldName
  else if SameText(AttribName, InstantIdFieldName) then
    Result := InstantIdFieldName;
end;

function TInstantRelationalTranslator.TablePathToAlias(
  const TablePath: string): string;
begin
  Result := TablePathAliases[IndexOfTablePath(TablePath)];
end;

function TInstantRelationalTranslator.TranslateClassRef(
  ClassRef: TInstantIQLClassRef; Writer: TInstantIQLWriter): Boolean;
begin
  Result := Assigned(ClassRef) and IsPrimary(ClassRef) and Assigned(Writer);
  if Result then
  begin
    WriteTables(Writer);
    if not Assigned(Command.Clause) then
      WriteCriterias(Writer, True);
  end;
end;

function TInstantRelationalTranslator.TranslateClause(
  Clause: TInstantIQLClause; Writer: TInstantIQLWriter): Boolean;
begin
  Result := Assigned(Clause) and IsPrimary(Clause) and Assigned(Writer);
  if Result then
  begin
    if WriteCriterias(Writer, False) then
      WriteAnd(Writer);
    Writer.WriteString('(');
    WriteObject(Clause, Writer);
    Writer.WriteString(')');
  end
end;

function TInstantRelationalTranslator.TranslateConstant(
  Constant: TInstantIQLConstant; Writer: TInstantIQLWriter): Boolean;
var
  S: string;
begin
  if Assigned(Constant) and Assigned(Writer) then
  begin
    S := Constant.Value;
    if SameText(S, 'NIL') then
    begin
      Writer.WriteString('NULL');
      Result := True;
    end else if SameText(S, 'SELF') then
    begin
      Writer.WriteString(Qualify(ClassTablePath, InstantIdFieldName));
      Result := True;
    end else if (Length(S) > 0) and (S[1] = '"') then
    begin
      S := InstantUnquote(S, S[1]);
      S := ReplaceWildCard(S);
      Writer.WriteString(QuoteString(S));
      Result := True;
    end else
      Result := False;
  end else
    Result := False;
end;

function TInstantRelationalTranslator.TranslateFunction(
  AFunction: TInstantIQLFunction; Writer: TInstantIQLWriter): Boolean;
begin
  Result := TranslateFunctionName(AFunction.FunctionName, Writer);
  if Result then
  begin
    Writer.WriteChar('(');
    AFunction.Parameters.Write(Writer);
    Writer.WriteChar(')');
  end;
end;

function TInstantRelationalTranslator.TranslateFunctionName(
  const FunctionName: string; Writer: TInstantIQLWriter): Boolean;
begin
  Result := False;
end;

function TInstantRelationalTranslator.TranslateKeyword(
  const Keyword: string; Writer: TInstantIQLWriter): Boolean;
begin
  if SameText(Keyword, 'ANY') then
    Result := True
  else
    Result := inherited TranslateKeyword(Keyword, Writer);
end;

function TInstantRelationalTranslator.TranslateObject(
  AObject: TInstantIQLObject; Writer: TInstantIQLWriter): Boolean;
begin
  if AObject is TInstantIQLSpecifier then
    Result := TranslateSpecifier(TInstantIQLSpecifier(AObject), Writer)
  else if AObject is TInstantIQLClassRef then
    Result := TranslateClassRef(TInstantIQLClassRef(AObject), Writer)
  else if AObject is TInstantIQLClause then
    Result := TranslateClause(TInstantIQLClause(AObject), Writer)
  else if AObject is TInstantIQLPath then
    Result := TranslatePath(TInstantIQLPath(AObject), Writer)
  else if AObject is TInstantIQLConstant then
    Result := TranslateConstant(TInstantIQLConstant(AObject), Writer)
  else if AObject is TInstantIQLFunction then
    Result := TranslateFunction(TInstantIQLFunction(AObject), Writer)
  else
    Result := inherited TranslateObject(AObject, Writer);
end;

function TInstantRelationalTranslator.TranslatePath(Path: TInstantIQLPath;
  Writer: TInstantIQLWriter): Boolean;
var
  PathText, TablePath, FieldName: string;
  AttribMeta: TInstantAttributeMetadata;
begin
  Result := Assigned(Path) and Assigned(Writer);
  if Result then
  begin
    PathText := Path.Text;
    AttribMeta := PathToTarget(PathText, TablePath, FieldName);
    if Assigned(AttribMeta) and (AttribMeta.Category = acElement) and
      not IsRootAttribute(ExtractTarget(PathText)) then
      FieldName := FieldName + InstantIdFieldName;
    Writer.WriteString(Qualify(TablePath, FieldName));
  end;
end;

function TInstantRelationalTranslator.TranslateSpecifier(
  Specifier: TInstantIQLSpecifier; Writer: TInstantIQLWriter): Boolean;

  function IncludeOperand(Operand: TInstantIQLOperand): Boolean;
  begin
    if Operand is TInstantIQLPath then
      if Specifier.IsPath then
        Result := not SameText(Specifier.Text, Operand.Text)
      else
        Result := not IsRootAttribute(Operand.Text)
    else if Operand is TInstantIQLConstant then
      Result := not TInstantIQLConstant(Operand).IsSelf and not
        Specifier.IsPath
    else
      Result := False;
  end;

  procedure WriteOrderFields(Writer: TInstantIQLWriter);
  var
    I: Integer;
    PathList: TList;
    Operand: TInstantIQLOperand;
  begin
    PathList := TList.Create;
    try
      CollectObjects(Command.Order, TInstantIQLOperand, PathList);
      for I := 0 to Pred(PathList.Count) do
      begin
        Operand := PathList[I];
        if IncludeOperand(Operand) then
        begin
          Writer.WriteString(', ');
          if Operand is TInstantIQLPath then
            TranslatePath(TInstantIQLPath(Operand), Writer)
          else if Operand is TInstantIQLConstant then
            TranslateConstant(TInstantIQLConstant(Operand), Writer);
        end;
      end;
    finally
      PathList.Free;
    end;
  end;

var
  ClassQual, IdQual, PathText: string;
begin
  Result := Assigned(Specifier) and IsPrimary(Specifier) and Assigned(Writer);
  if Result then
  begin
    if Specifier.Operand is TInstantIQLPath then
    begin
      PathText := TInstantIQLPath(Specifier.Operand).Text;
      ClassQual := QualifyPath(ConcatPath(PathText, InstantClassFieldName));
      IdQual := QualifyPath(ConcatPath(PathText, InstantIdFieldName));
    end else
    begin
      ClassQual := QualifyPath(InstantClassFieldName);
      IdQual := QualifyPath(InstantIdFieldName);
    end;
    Writer.WriteString(Format('%s AS %s, %s AS %s', [ClassQual,
      InstantClassFieldName, IdQual, InstantIdFieldName]));
    if IncludeOrderFields then
      WriteOrderFields(Writer);
  end;
end;

procedure TInstantRelationalTranslator.WriteAnd(Writer: TInstantIQLWriter);
begin
  Writer.WriteString(' AND ');
end;

function TInstantRelationalTranslator.WriteCriterias(
  Writer: TInstantIQLWriter; IncludeWhere: Boolean): Boolean;
var
  I: Integer;
begin
  Result := CriteriaCount > 0;
  if Result then
  begin
    if IncludeWhere then
      Writer.WriteString(' WHERE ');
    Writer.WriteChar('(');
    for I := 0 to Pred(CriteriaCount) do
    begin
      if I > 0 then
        WriteAnd(Writer);
      Writer.WriteString(Criterias[I]);
    end;
    Writer.WriteChar(')');
  end;
end;

procedure TInstantRelationalTranslator.WriteTables(
  Writer: TInstantIQLWriter);
var
  I: Integer;
begin
  for I := 0 to Pred(TablePathCount) do
  begin
    if I > 0 then
      Writer.WriteString(', ');
    Writer.WriteString(Format('%s %s',[InstantEmbrace(
      ExtractTarget(TablePaths[I]), Delimiters), TablePathAliases[I]]));
  end;
end;

{ TInstantCustomRelationalBroker }

constructor TInstantCustomRelationalBroker.Create(AConnector: TInstantConnector);
begin
  inherited;
  FStatementCacheCapacity := 0;
end;

destructor TInstantCustomRelationalBroker.Destroy;
begin
  FreeAndNil(FStatementCache);
  inherited;
end;

procedure TInstantCustomRelationalBroker.DisposeMap(AObject: TInstantObject;
  const AObjectId: string; Map: TInstantAttributeMap;
  ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo);
begin
  EnsureResolver(Map).DisposeMap(AObject, Map, ConflictAction, Info);
end;

function TInstantCustomRelationalBroker.Execute(const AStatement: string;
  AParams: TParams): Integer;
begin
  Result := 0;
end;

function TInstantCustomRelationalBroker.GetConnector: TInstantRelationalConnector;
begin
  Result := inherited Connector as TInstantRelationalConnector;
end;

function TInstantCustomRelationalBroker.GetDBMSName: string;
begin
  Result := '';
end;

function TInstantCustomRelationalBroker.GetSQLDelimiters: string;
begin
  Result := '';
end;

function TInstantCustomRelationalBroker.GetSQLQuote: Char;
begin
  Result := '"';
end;

function TInstantCustomRelationalBroker.GetSQLWildcard: string;
begin
  Result := '%';
end;

function TInstantCustomRelationalBroker.GetStatementCache: TInstantStatementCache;
begin
  if not Assigned(FStatementCache) then
  begin
    FStatementCache := TInstantStatementCache.Create(nil);
    FStatementCache.Capacity := FStatementCacheCapacity;
  end;
  Result := FStatementCache;
end;

function TInstantCustomRelationalBroker.InternalDisposeObject(
  AObject: TInstantObject; ConflictAction: TInstantConflictAction): Boolean;
begin
  Result := PerformOperation(AObject, AObject.Id, otDispose, DisposeMap,
    ConflictAction);
end;

function TInstantCustomRelationalBroker.InternalRetrieveObject(
  AObject: TInstantObject; const AObjectId: string;
  ConflictAction: TInstantConflictAction): Boolean;
begin
  Result := PerformOperation(AObject, AObjectId, otRetrieve, RetrieveMap,
    ConflictAction);
end;

function TInstantCustomRelationalBroker.InternalStoreObject(
  AObject: TInstantObject; ConflictAction: TInstantConflictAction): Boolean;
begin
  Result := PerformOperation(AObject, AObject.Id, otStore, StoreMap,
    ConflictAction);
end;

function TInstantCustomRelationalBroker.PerformOperation(
  AObject: TInstantObject; const AObjectId: string;
  OperationType: TInstantOperationType; Operation: TInstantBrokerOperation;
  ConflictAction: TInstantConflictAction): Boolean;

  function OperationRequired(Map: TInstantAttributeMap): Boolean;
  var
    I: Integer;
    Attrib: TInstantAttribute;
  begin
    case OperationType of
      otStore:
        begin
          Result := not AObject.IsPersistent;
          if not Result then
            for I := 0 to Pred(Map.Count) do
            begin
              Attrib := AObject.AttributeByName(Map[I].Name);
              Result := Attrib.IsMandatory or Attrib.IsChanged;
              if Result then
                Exit;
            end;
        end;
      otRetrieve, otDispose:
        Result := True;
    else
      Result := False;
    end;
  end;

var
  I: Integer;
  RootMap, Map: TInstantAttributeMap;
  Info: TInstantOperationInfo;
begin
  with Info do
  begin
    Success := False;
    Conflict := False;
  end;
  with AObject.Metadata do
  begin
    RootMap := StorageMaps.RootMap;
    Operation(AObject, AObjectId, RootMap, ConflictAction, @Info);
    Result := Info.Success;
    if Result then
      for I := 0 to Pred(StorageMaps.Count) do
      begin
        Map := StorageMaps[I];
        if (Map <> RootMap) and (Info.Conflict or OperationRequired(Map)) then
          Operation(AObject, AObjectId, Map);
      end;
  end;
end;

procedure TInstantCustomRelationalBroker.RetrieveMap(AObject: TInstantObject;
  const AObjectId: string; Map: TInstantAttributeMap;
  ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo);
begin
  EnsureResolver(Map).RetrieveMap(AObject, AObjectId, Map, ConflictAction, Info);
end;

procedure TInstantCustomRelationalBroker.SetStatementCacheCapacity(const Value: Integer);
begin
  FStatementCacheCapacity := Value;
  if FStatementCacheCapacity = 0 then
    FreeAndNil(FStatementCache)
  else if Assigned(FStatementCache) then
    FStatementCache.Capacity := FStatementCacheCapacity;
end;

procedure TInstantCustomRelationalBroker.StoreMap(AObject: TInstantObject;
  const AObjectId: string; Map: TInstantAttributeMap;
  ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo);

  // Always storing fixes #880713
  {function MustStoreMap: Boolean;
  var
    I: Integer;
    Attrib: TInstantAttribute;
  begin
    Result := Map.IsRootMap;
    if Result then
      Exit;
    for I := 0 to Pred(Map.Count) do
    begin
      Attrib := AObject.AttributeByName(Map[I].Name);
      Result := Attrib.IsMandatory or not Attrib.IsDefault;
      if Result then
        Exit;
    end;
    Result := False;
  end;}

var
  Resolver: TInstantCustomResolver;
  {MustStore: Boolean;}
begin
  {MustStore := MustStoreMap;}
  {if MustStore or AObject.IsPersistent then
  begin}
    Resolver := EnsureResolver(Map);
    {if MustStore then}
      Resolver.StoreMap(AObject, Map, ConflictAction, Info)
    {else if AObject.IsPersistent then
      Resolver.DisposeMap(AObject, Map, ConflictAction, Info);
  end;}
end;

{ TInstantCustomResolver }

constructor TInstantCustomResolver.Create(
  ABroker: TInstantCustomRelationalBroker);
begin
  if not Assigned(ABroker) then
    raise EInstantError.Create(SUnassignedBroker);
  inherited Create;
  FBroker := ABroker;
end;

procedure TInstantCustomResolver.DisposeMap(AObject: TInstantObject;
  Map: TInstantAttributeMap; ConflictAction: TInstantConflictAction;
  Info: PInstantOperationInfo);
begin
  InternalDisposeMap(AObject, Map, ConflictAction, Info);
end;

procedure TInstantCustomResolver.InternalDisposeMap(
  AObject: TInstantObject; Map: TInstantAttributeMap;
  ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo);
begin
end;

procedure TInstantCustomResolver.InternalRetrieveMap(
  AObject: TInstantObject; const AObjectId: string;
  Map: TInstantAttributeMap; ConflictAction: TInstantConflictAction;
  Info: PInstantOperationInfo);
begin
end;

procedure TInstantCustomResolver.InternalStoreMap(AObject: TInstantObject;
  Map: TInstantAttributeMap; ConflictAction: TInstantConflictAction;
  Info: PInstantOperationInfo);
begin
end;

function TInstantCustomResolver.KeyViolation(AObject: TInstantObject;
  const AObjectId: string; E: Exception): EInstantKeyViolation;
var
  ObjectClassName: string;
begin
  if Assigned(AObject) then
    ObjectClassName := AObject.ClassName
  else
    ObjectClassName := '';
  Result := EInstantKeyViolation.CreateFmt(SKeyViolation,
    [ObjectClassName, AObjectId], E);
end;

procedure TInstantCustomResolver.RetrieveMap(AObject: TInstantObject;
  const AObjectId: string; Map: TInstantAttributeMap;
  ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo);
begin
  InternalRetrieveMap(AObject, AObjectId, Map, ConflictAction, Info);
end;

procedure TInstantCustomResolver.StoreMap(AObject: TInstantObject;
  Map: TInstantAttributeMap; ConflictAction: TInstantConflictAction;
  Info: PInstantOperationInfo);
begin
  InternalStoreMap(AObject, Map, ConflictAction, Info);
end;

{ TInstantCustomRelationalQuery }

function TInstantCustomRelationalQuery.CreateTranslator: TInstantRelationalTranslator;
begin
  Result := TranslatorClass.Create(Self);
end;

function TInstantCustomRelationalQuery.GetConnector: TInstantRelationalConnector;
begin
  Result := inherited Connector as TInstantRelationalConnector;
end;

function TInstantCustomRelationalQuery.GetStatement: string;
begin
  Result := '';
end;

procedure TInstantCustomRelationalQuery.InternalGetInstantObjectRefs(List:
    TInstantObjectReferenceList);
begin
end;

procedure TInstantCustomRelationalQuery.InternalRefreshObjects;
var
  BusyObjectRefs: TInstantObjectReferenceList;
  Obj: TInstantObject;
  ObjStore: TInstantObjectStore;

  procedure RemoveRefsOfDeletedObjectsFromList;
  var
    I: Integer;
  begin
    for I := Pred(BusyObjectRefs.Count) downto 0 do
      with BusyObjectRefs.RefItems[I] do
      begin
        ObjStore := Connector.ObjectStores.FindObjectStore(ObjectClass);
        if not (Assigned(ObjStore) and Assigned(ObjStore.Find(ObjectId))) then
          BusyObjectRefs.Delete(I);
      end;
  end;

  procedure RefreshObjectsInList;
  var
    I: Integer;
  begin
    for I := 0 to Pred(BusyObjectRefs.Count) do
    begin
      Obj := BusyObjectRefs[I];
      if Assigned(Obj) then
        Obj.Refresh;
    end;
  end;

begin
  BusyObjectRefs := TInstantObjectReferenceList.Create(False, Connector);
  try
    // Collect a reference to all InstantObjects in query.
    // Note: In this list of TInstantObjectReferences
    // OwnsInstance is false.
    InternalGetInstantObjectRefs(BusyObjectRefs);

    Close;

    // Remove references from the BusyList for objects destroyed
    // when the query was closed.
    RemoveRefsOfDeletedObjectsFromList;
        
    Open;

    // Refresh objects in the BusyList that were not destroyed
    // when the query was closed.
    RefreshObjectsInList;
  finally
    BusyObjectRefs.Free;
  end;
end;

procedure TInstantCustomRelationalQuery.SetStatement(const Value: string);
begin
end;

procedure TInstantCustomRelationalQuery.TranslateCommand;
begin
  if TranslatorClass <> nil then
    with TranslatorClass.Create(Self) do
    try
      CommandText := Self.Command;
      Statement := StatementText;
    finally
      Free;
    end;
end;

class function TInstantCustomRelationalQuery.TranslatorClass: TInstantRelationalTranslatorClass;
begin
  Result := TInstantRelationalTranslator;
end;

{ TInstantConnectionBasedConnector }

procedure TInstantConnectionBasedConnector.AfterConnectionChange;
begin
end;

procedure TInstantConnectionBasedConnector.BeforeConnectionChange;
begin
end;

procedure TInstantConnectionBasedConnector.CheckConnection;
begin
  InstantCheckConnection(FConnection);
end;

procedure TInstantConnectionBasedConnector.DoAfterConnectionChange;
begin
  if Assigned(FConnection) then
    FConnection.FreeNotification(Self);
  AfterConnectionChange;
end;

procedure TInstantConnectionBasedConnector.DoBeforeConnectionChange;
begin
  try
    BeforeConnectionChange;
  finally
    if Assigned(FConnection) then
      FConnection.RemoveFreeNotification(Self);
  end;
end;

function TInstantConnectionBasedConnector.GetConnected: Boolean;
begin
  if HasConnection then
    Result := Connection.Connected
  else
    Result := inherited GetConnected;
end;

function TInstantConnectionBasedConnector.GetConnection: TCustomConnection;
begin
  if not (csDesigning in ComponentState) then
    CheckConnection;
  AssignLoginOptions;
  Result := FConnection;
end;

function TInstantConnectionBasedConnector.HasConnection: Boolean;
begin
  Result := Assigned(FConnection);
end;

procedure TInstantConnectionBasedConnector.InternalConnect;
begin
  CheckConnection;
  Connection.Open;
end;

procedure TInstantConnectionBasedConnector.InternalDisconnect;
begin
  if HasConnection then
    Connection.Close;
end;

procedure TInstantConnectionBasedConnector.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FConnection) and (Operation = opRemove) then
  begin
    Disconnect;
    FConnection := nil;
  end;
end;

procedure TInstantConnectionBasedConnector.SetConnection(
  Value: TCustomConnection);
begin
  if Value <> FConnection then
  begin
    Disconnect;
    DoBeforeConnectionChange;
    FConnection := Value;
    AssignLoginOptions;
    DoAfterConnectionChange;
  end;
end;

procedure TInstantConnectionBasedConnector.AssignLoginOptions;
begin
  if HasConnection then
  begin
    FConnection.LoginPrompt := FLoginPrompt;
  end;
end;

function TInstantConnectionBasedConnector.GetLoginPrompt: Boolean;
begin
  if HasConnection then
    FLoginPrompt := FConnection.LoginPrompt;
  Result := FLoginPrompt;
end;

procedure TInstantConnectionBasedConnector.SetLoginPrompt(
  const Value: Boolean);
begin
  FLoginPrompt := Value;
  AssignLoginOptions;
end;

constructor TInstantConnectionBasedConnector.Create(AOwner: TComponent);
begin
  inherited;
  FLoginPrompt := True;
end;

{ TInstantConnectionBasedConnectionDef }

constructor TInstantConnectionBasedConnectionDef.Create(
  Collection: TCollection);
begin
  inherited;
  FLoginPrompt := True;
end;

procedure TInstantConnectionBasedConnectionDef.InitConnector(Connector: TInstantConnector);
var
  Connection: TCustomConnection;
begin
  inherited;
  Connection := CreateConnection(Connector);
  try
    (Connector as TInstantConnectionBasedConnector).Connection := Connection;
    (Connector as TInstantConnectionBasedConnector).LoginPrompt := LoginPrompt;
  except
    Connection.Free;
    raise;
  end;
end;

{ TInstantNavigationalBroker }

destructor TInstantNavigationalBroker.Destroy;
begin
  FResolverList.Free;
  inherited;
end;

function TInstantNavigationalBroker.EnsureResolver(
  Map: TInstantAttributeMap): TInstantCustomResolver;
var
  TableName: string;
begin
  TableName := Map.Name;
  Result := FindResolver(TableName);
  if not Assigned(Result) then
  begin
    Result := CreateResolver(TableName);
    ResolverList.Add(Result);
  end;
end;

function TInstantNavigationalBroker.FindResolver(
  const TableName: string): TInstantNavigationalResolver;
var
  I: Integer;
begin
  for I := 0 to Pred(ResolverCount) do
  begin
    Result := Resolvers[I];
    if SameText(TableName, Result.TableName) then
      Exit;
  end;
  Result := nil;
end;

function TInstantNavigationalBroker.GetResolverCount: Integer;
begin
 Result := ResolverList.Count;
end;

function TInstantNavigationalBroker.GetResolverList: TObjectList;
begin
  if not Assigned(FResolverList) then
    FResolverList := TObjectList.Create;
  Result := FResolverList;
end;

function TInstantNavigationalBroker.GetResolvers(
  Index: Integer): TInstantNavigationalResolver;
begin
  Result := ResolverList[Index] as TInstantNavigationalResolver;
end;

{ TInstantNavigationalResolver }

procedure TInstantNavigationalResolver.Append;
begin
  DataSet.Append;
end;

procedure TInstantNavigationalResolver.Cancel;
begin
  DataSet.Cancel;
end;

function TInstantNavigationalResolver.CheckConflict(AObject: TInstantObject;
  const AObjectId: string; ConflictAction: TInstantConflictAction): Boolean;
var
  Field: TField;
begin
  Field := FieldByName(InstantUpdateCountFieldName);
  Result := Field.AsInteger <> AObject.UpdateCount;
  if Result and (ConflictAction = caFail) then
    raise EInstantConflict.CreateFmt(SUpdateConflict,
      [AObject.ClassName, AObjectId]);
end;

procedure TInstantNavigationalResolver.ClearAttribute(AObject: TInstantObject;
  AttributeMetadata: TInstantAttributeMetadata);
var
  Attribute: TInstantAttribute;
begin
  with AttributeMetadata do
  begin
    Attribute := AObject.AttributeByName(Name);
    case AttributeType of
      atInteger:
        ClearInteger(Attribute as TInstantInteger);
      atFloat:
        ClearFloat(Attribute as TInstantFloat);
      atCurrency:
        ClearCurrency(Attribute as TInstantCurrency);
      atBoolean:
        ClearBoolean(Attribute as TInstantBoolean);
      atString:
        ClearString(Attribute as TInstantString);
      atDateTime:
        ClearDateTime(Attribute as TInstantDateTime);
      atBlob:
        ClearBlob(Attribute as TInstantBlob);
      atGraphic:
        ClearBlob(Attribute as TInstantGraphic);
      atMemo:
        ClearMemo(Attribute as TInstantMemo);
      atPart:
        ClearPart(Attribute as TInstantPart);
      atReference:
        ClearReference(Attribute as TInstantReference);
      atParts:
        ClearParts(Attribute as TInstantParts);
      atReferences:
        ClearReferences(Attribute as TInstantReferences);
    end;
  end;
end;

procedure TInstantNavigationalResolver.ClearBlob(Attribute: TInstantBlob);
begin
end;

procedure TInstantNavigationalResolver.ClearBoolean(Attribute: TInstantBoolean);
begin
end;

procedure TInstantNavigationalResolver.ClearDateTime(Attribute: TInstantDateTime);
begin
end;

procedure TInstantNavigationalResolver.ClearFloat(Attribute: TInstantFloat);
begin
end;

procedure TInstantNavigationalResolver.ClearCurrency(Attribute: TInstantCurrency);
begin
end;

procedure TInstantNavigationalResolver.ClearInteger(Attribute: TInstantInteger);
begin
end;

procedure TInstantNavigationalResolver.ClearMemo(Attribute: TInstantMemo);
begin
end;

procedure TInstantNavigationalResolver.ClearPart(Attribute: TInstantPart);
begin
end;

procedure TInstantNavigationalResolver.ClearParts(Attribute: TInstantParts);
begin
end;

procedure TInstantNavigationalResolver.ClearReference(
  Attribute: TInstantReference);
begin
end;

procedure TInstantNavigationalResolver.ClearReferences(
  Attribute: TInstantReferences);
begin
end;

procedure TInstantNavigationalResolver.ClearString(Attribute: TInstantString);
begin
end;

procedure TInstantNavigationalResolver.Close;
begin
  DataSet.Close;
end;

constructor TInstantNavigationalResolver.Create(
  ABroker: TInstantNavigationalBroker; const ATableName: string);
begin
  inherited Create(ABroker);
  FTableName := ATableName;
end;

function TInstantNavigationalResolver.CreateLocateVarArray(
  const AObjectClassName,
  AObjectId: string): Variant;
begin
  Result := VarArrayOf([AObjectClassName, AObjectId]);
end;

procedure TInstantNavigationalResolver.Delete;
begin
  DataSet.Delete;
end;

destructor TInstantNavigationalResolver.Destroy;
begin
  FreeDataSet;
  inherited;
end;

procedure TInstantNavigationalResolver.Edit;
begin
  DataSet.Edit;
end;

function TInstantNavigationalResolver.FieldByName(
  const FieldName: string): TField;
begin
  Result := DataSet.FieldByName(FieldName);
end;

function TInstantNavigationalResolver.FieldHasObjects(Field: TField): Boolean;
begin
  Result := Length(Field.AsString) > 1;
end;

procedure TInstantNavigationalResolver.FreeDataSet;
begin
  if FFreeDataSet then
    FreeAndNil(FDataSet);
end;

function TInstantNavigationalResolver.GetBroker: TInstantNavigationalBroker;
begin
  Result := inherited Broker as TInstantNavigationalBroker;
end;

function TInstantNavigationalResolver.GetDataSet: TDataSet;
begin
  if not Assigned(FDataSet) then
  begin
    Broker.Connector.DoGetDataSet(TableName, FDataSet);
    if not Assigned(FDataSet) then
    begin
      FDataSet := CreateDataSet;
      FFreeDataSet := True;
    end;
    Broker.Connector.DoInitDataSet(TableName, FDataSet);
  end;
  Result := FDataSet;
end;

procedure TInstantNavigationalResolver.InternalDisposeMap(
  AObject: TInstantObject; Map: TInstantAttributeMap;
  ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo);
var
  TransError: Exception;
  AInfo: TInstantOperationInfo;
begin
  if not Assigned(Info) then
    Info := @AInfo;
  Open;
  if Locate(AObject.ClassName, AObject.PersistentId) then
  begin
    if Map.IsRootMap then
      Info.Conflict := CheckConflict(AObject, AObject.PersistentId,
        ConflictAction);
    PerformOperation(AObject, Map, ClearAttribute);
    try
      Delete;
      Info.Success := True;
    except
      on EAbort do
        raise;
      on E: Exception do
      begin
        TransError := TranslateError(AObject, E);
        if Assigned(TransError) then
          raise TransError
        else
          raise;
      end;
    end;
  end else if Map.IsRootMap and (ConflictAction = caFail) then
    raise EInstantConflict.CreateFmt(SDisposeConflict,
      [AObject.ClassName, AObject.PersistentId])
end;

procedure TInstantNavigationalResolver.InternalRetrieveMap(
  AObject: TInstantObject; const AObjectId: string; Map: TInstantAttributeMap;
  ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo);
var
  AInfo: TInstantOperationInfo;
begin
  if not Assigned(Info) then
    Info := @AInfo;
  Open;
  Info.Success := Locate(AObject.ClassName, AObjectId);
  if Info.Success then
  begin
    if Map.IsRootMap then
    begin
      Info.Conflict := CheckConflict(AObject, AObjectId, caIgnore);
      SetObjectUpdateCount(AObject,
        FieldByName(InstantUpdateCountFieldName).AsInteger);
    end;
    PerformOperation(AObject, Map, ReadAttribute);
  end else
    ResetAttributes(AObject, Map);
end;

procedure TInstantNavigationalResolver.InternalStoreMap(
  AObject: TInstantObject; Map: TInstantAttributeMap;
  ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo);

  procedure AppendMap;
  begin
    Append;
  end;

  procedure EditMap;
  begin
    if Locate(AObject.ClassName, AObject.PersistentId) then
    begin
      if Map.IsRootMap then
        Info.Conflict := CheckConflict(AObject, AObject.Id, ConflictAction);
      Edit;
    end else
      AppendMap;
  end;

var
  NewId: string;
  AInfo: TInstantOperationInfo;
  TransError: Exception;
begin
  { Avoid any interference with storage when performing store operation }
  NewId := AObject.Id;

  if not Assigned(Info) then
    Info := @AInfo;
  Open;
  if AObject.IsPersistent then
    EditMap
  else
    AppendMap;
  try
    FieldByName(InstantClassFieldName).AsString := AObject.ClassName;
    FieldByName(InstantIdFieldName).AsString := NewId;
    with FieldByName(InstantUpdateCountFieldName) do
    begin
      if AsInteger = High(Integer) then
        AsInteger := 1
      else
        AsInteger := AsInteger + 1;
      if Map.IsRootMap then
        SetObjectUpdateCount(AObject, AsInteger);
    end;
    PerformOperation(AObject, Map, WriteAttribute);
    Post;
    Info.Success := True;
  except
    on E: Exception do
    begin
      Cancel;
      if E is EAbort then
        raise
      else begin
        TransError := TranslateError(AObject, E);
        if Assigned(TransError) then
          raise TransError
        else
          raise;
      end;
    end;
  end;
end;

procedure TInstantNavigationalResolver.Open;
begin
  DataSet.Open;
end;

procedure TInstantNavigationalResolver.PerformOperation(AObject: TInstantObject;
  Map: TInstantAttributeMap; Operation: TInstantNavigationalResolverOperation);
var
  I: Integer;
  AttribMeta: TInstantAttributeMetadata;
begin
  if not Assigned(Operation) then
    Exit;
  for I := 0 to Pred(Map.Count) do
  begin
    AttribMeta := Map.Items[I];
    Operation(AObject, AttribMeta);
  end;
end;

procedure TInstantNavigationalResolver.Post;
begin
  DataSet.Post;
end;

procedure TInstantNavigationalResolver.ReadAttribute(AObject: TInstantObject;
  AttributeMetadata: TInstantAttributeMetadata);
var
  Attribute: TInstantAttribute;
begin
  with AttributeMetadata do
  begin
    Attribute := AObject.AttributeByName(Name);
    case AttributeType of
      atInteger:
        ReadInteger(Attribute as TInstantInteger);
      atFloat:
        ReadFloat(Attribute as TInstantFloat);
      atCurrency:
        ReadCurrency(Attribute as TInstantCurrency);
      atBoolean:
        ReadBoolean(Attribute as TInstantBoolean);
      atString:
        ReadString(Attribute as TInstantString);
      atDateTime:
        ReadDateTime(Attribute as TInstantDateTime);
      atBlob:
        ReadBlob(Attribute as TInstantBlob);
      atGraphic:
        ReadBlob(Attribute as TInstantGraphic);
      atMemo:
        ReadMemo(Attribute as TInstantMemo);
      atPart:
        ReadPart(Attribute as TInstantPart);
      atReference:
        ReadReference(Attribute as TInstantReference);
      atParts:
        ReadParts(Attribute as TInstantParts);
      atReferences:
        ReadReferences(Attribute as TInstantReferences);
    end;
  end;
end;

procedure TInstantNavigationalResolver.ReadBlob(Attribute: TInstantBlob);
begin
  with Attribute do
    Value := FieldByName(Metadata.FieldName).AsString;
end;

procedure TInstantNavigationalResolver.ReadBoolean(Attribute: TInstantBoolean);
begin
  with Attribute do
    Value := FieldByName(Metadata.FieldName).AsBoolean;
end;

procedure TInstantNavigationalResolver.ReadDateTime(
  Attribute: TInstantDateTime);
begin
  with Attribute do
    Value := FieldByName(Metadata.FieldName).AsDateTime;
end;

procedure TInstantNavigationalResolver.ReadFloat(Attribute: TInstantFloat);
begin
  with Attribute do
    Value := FieldByName(Metadata.FieldName).AsFloat;
end;

procedure TInstantNavigationalResolver.ReadCurrency(Attribute: TInstantCurrency);
begin
  with Attribute do
    Value := FieldByName(Metadata.FieldName).AsCurrency;
end;

procedure TInstantNavigationalResolver.ReadInteger(Attribute: TInstantInteger);
begin
  with Attribute do
    Value := FieldByName(Metadata.FieldName).AsInteger;
end;

procedure TInstantNavigationalResolver.ReadMemo(Attribute: TInstantMemo);
begin
  with Attribute do
    Value := TrimRight(FieldByName(Metadata.FieldName).AsString);
end;

procedure TInstantNavigationalResolver.ReadPart(Attribute: TInstantPart);
var
  Field: TField;
  Stream: TInstantStringStream;
begin
  with Attribute do
  begin
    Field := FieldByName(Metadata.FieldName);
    if not FieldHasObjects(Field) then
      Exit;
    Stream := TInstantStringStream.Create(Field.AsString);
    try
      LoadObjectFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TInstantNavigationalResolver.ReadParts(Attribute: TInstantParts);
var
  Field: TField;
  Stream: TInstantStringStream;
begin
  with Attribute do
  begin
    Field := FieldByName(Metadata.FieldName);
    if not FieldHasObjects(Field) then
      Exit;
    Stream := TInstantStringStream.Create(Field.AsString);
    try
      LoadObjectsFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TInstantNavigationalResolver.ReadReference(
  Attribute: TInstantReference);
begin
  with Attribute do
  begin
    ReferenceObject(
      TrimRight(
        FieldByName(Metadata.FieldName + InstantClassFieldName).AsString),
      TrimRight(
        FieldByName(Metadata.FieldName + InstantIdFieldName).AsString));
  end;
end;

procedure TInstantNavigationalResolver.ReadReferences(
  Attribute: TInstantReferences);
var
  Field: TField;
  Stream: TInstantStringStream;
begin
  with Attribute do
  begin
    Field := FieldByName(Metadata.FieldName);
    if not FieldHasObjects(Field) then
      Exit;
    Stream := TInstantStringStream.Create(Field.AsString);
    try
      LoadReferencesFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TInstantNavigationalResolver.ReadString(Attribute: TInstantString);
begin
  with Attribute do
    Value := TrimRight(FieldByName(Metadata.FieldName).AsString);
end;

procedure TInstantNavigationalResolver.ResetAttribute(AObject: TInstantObject;
  AttributeMetadata: TInstantAttributeMetadata);
begin
  AObject.AttributeByName(AttributeMetadata.Name).Reset;
end;

procedure TInstantNavigationalResolver.ResetAttributes(AObject: TInstantObject;
  Map: TInstantAttributeMap);
begin
  PerformOperation(AObject, Map, ResetAttribute);
end;

procedure TInstantNavigationalResolver.SetDataSet(Value: TDataset);
begin
  if Value <> FDataSet then
  begin
    FreeDataSet;
    FDataSet := Value;
  end;
end;

procedure TInstantNavigationalResolver.SetObjectUpdateCount(
  AObject: TInstantObject; Value: Integer);
begin
  Broker.SetObjectUpdateCount(AObject, Value);
end;

function TInstantNavigationalResolver.TranslateError(AObject: TInstantObject;
  E: Exception): Exception;
begin
  Result := nil;
end;

procedure TInstantNavigationalResolver.WriteAttribute(AObject: TInstantObject;
  AttributeMetadata: TInstantAttributeMetadata);
var
  Attribute: TInstantAttribute;
begin
  with AttributeMetadata do
  begin
    Attribute := AObject.AttributeByName(Name);
    case AttributeType of
      atInteger:
        WriteInteger(Attribute as TInstantInteger);
      atFloat:
        WriteFloat(Attribute as TInstantFloat);
      atCurrency:
        WriteCurrency(Attribute as TInstantCurrency);
      atBoolean:
        WriteBoolean(Attribute as TInstantBoolean);
      atString:
        WriteString(Attribute as TInstantString);
      atDateTime:
        WriteDateTime(Attribute as TInstantDateTime);
      atBlob:
        WriteBlob(Attribute as TInstantBlob);
      atGraphic:
        WriteBlob(Attribute as TInstantGraphic);
      atMemo:
        WriteMemo(Attribute as TInstantMemo);
      atPart:
        WritePart(Attribute as TInstantPart);
      atReference:
        WriteReference(Attribute as TInstantReference);
      atParts:
        WriteParts(Attribute as TInstantParts);
      atReferences:
        WriteReferences(Attribute as TInstantReferences);
    end;
  end;
end;

procedure TInstantNavigationalResolver.WriteBlob(Attribute: TInstantBlob);
begin
  with Attribute do
    FieldByName(Metadata.FieldName).AsString := Value;
end;

procedure TInstantNavigationalResolver.WriteBoolean(Attribute: TInstantBoolean);
begin
  with Attribute do
    FieldByName(Metadata.FieldName).AsBoolean := Value;
end;

procedure TInstantNavigationalResolver.WriteDateTime(
  Attribute: TInstantDateTime);
begin
  with Attribute do
    FieldByName(Metadata.FieldName).AsDateTime := Value;
end;

procedure TInstantNavigationalResolver.WriteFloat(Attribute: TInstantFloat);
begin
  with Attribute do
    FieldByName(Metadata.FieldName).AsFloat := Value;
end;

procedure TInstantNavigationalResolver.WriteCurrency(Attribute: TInstantCurrency);
begin
  with Attribute do
{$IFDEF FPC}
    FieldByName(Metadata.FieldName).AsFloat := Value;
{$ELSE}
    FieldByName(Metadata.FieldName).AsCurrency := Value;
{$ENDIF}
end;

procedure TInstantNavigationalResolver.WriteInteger(Attribute: TInstantInteger);
begin
  with Attribute do
    FieldByName(Metadata.FieldName).AsInteger := Value;
end;

procedure TInstantNavigationalResolver.WriteMemo(Attribute: TInstantMemo);
begin
  WriteBlob(Attribute);
end;

procedure TInstantNavigationalResolver.WritePart(Attribute: TInstantPart);
var
  Field: TField;
  Stream: TInstantStringStream;
begin
  with Attribute do
  begin
    Field := FieldByName(Metadata.FieldName);
    Stream := TInstantStringStream.Create('');
    try
      SaveObjectToStream(Stream);
      Field.AsString := Stream.DataString;
    finally
      Stream.Free;
    end;
  end;
end;

procedure TInstantNavigationalResolver.WriteParts(Attribute: TInstantParts);
var
  Field: TField;
  Stream: TInstantStringStream;
begin
  with Attribute do
  begin
    Field := FieldByName(Metadata.FieldName);
    Stream := TInstantStringStream.Create('');
    try
      Attribute.SaveObjectsToStream(Stream);
      Field.AsString := Stream.DataString;
    finally
      Stream.Free;
    end;
  end;
end;

procedure TInstantNavigationalResolver.WriteReference(
  Attribute: TInstantReference);
begin
  with Attribute do
  begin
    FieldByName(Metadata.FieldName + InstantClassFieldName).AsString :=
      ObjectClassName;
    FieldByName(Metadata.FieldName + InstantIdFieldName).AsString := ObjectId;
  end;
end;

procedure TInstantNavigationalResolver.WriteReferences(
  Attribute: TInstantReferences);
var
  Field: TField;
  Stream: TInstantStringStream;
begin
  with Attribute do
  begin
    Field := FieldByName(Metadata.FieldName);
    Stream := TInstantStringStream.Create('');
    try
      SaveReferencesToStream(Stream);
      Field.AsString := Stream.DataString;
    finally
      Stream.Free;
    end;
  end;
end;

procedure TInstantNavigationalResolver.WriteString(Attribute: TInstantString);
begin
  with Attribute do
    FieldByName(Metadata.FieldName).AsString := Value;
end;

{ TInstantNavigationalQuery }

function TInstantNavigationalQuery.CreateObject(Row: Integer): TObject;
var
  ClassNameField, ObjectNameField: TField;
  AClass: TInstantObjectClass;
begin
  RowNumber := Row;
  ObjectNameField := DataSet.FindField(InstantIdFieldName);
  if Assigned(ObjectNameField) and
    ObjectClass.InheritsFrom(TInstantObject) then
  begin
    ClassNameField := DataSet.FindField(InstantClassFieldName);
    if Assigned(ClassNameField) then
      AClass := InstantGetClass(ClassNameField.AsString)
    else
      AClass := TInstantObjectClass(ObjectClass);
    if Assigned(AClass) then
      Result := AClass.Retrieve(ObjectNameField.AsString, False, False,
        Connector)
    else
      Result := nil;
  end else
    Result := CreateObjectFromDataSet(ObjectClass, DataSet);
end;

destructor TInstantNavigationalQuery.Destroy;
begin
  DestroyObjectRowList;
  inherited;
end;

procedure TInstantNavigationalQuery.DestroyObjectRowList;
var
  I: Integer;
  ObjectRow: PObjectRow;
begin
  if not Assigned(FObjectRowList) then
    Exit;
  for I := 0 to Pred(FObjectRowList.Count) do
  begin
    ObjectRow := FObjectRowList[I];
    FreeAndNil(ObjectRow.Instance);
    Dispose(ObjectRow);
  end;
  FreeAndNil(FObjectRowList);
end;

function TInstantNavigationalQuery.GetActive: Boolean;
begin
  Result := DataSet.Active;
end;

function TInstantNavigationalQuery.GetDataSet: TDataSet;
begin
  Result := nil;
end;

function TInstantNavigationalQuery.GetObjectRowCount: Integer;
begin
  Result := ObjectRowList.Count;
end;

function TInstantNavigationalQuery.GetObjectRowList: TList;
begin
  if not Assigned(FObjectRowList) then
  begin
    FObjectRowList := TList.Create;
    FObjectRowList.Count := RowCount;
    InitObjectRows(FObjectRowList, 0, Pred(FObjectRowList.Count));
  end;
  Result := FObjectRowList;
end;

function TInstantNavigationalQuery.GetObjectRows(Index: Integer): PObjectRow;
begin
  Result := ObjectRowList[Index];
end;

function TInstantNavigationalQuery.GetRowCount: Integer;
begin
  if Assigned(DataSet) then
    Result := DataSet.RecordCount
  else
    Result := 0;
end;

function TInstantNavigationalQuery.GetRowNumber: Integer;
begin
  Result := DataSet.RecNo;
end;

procedure TInstantNavigationalQuery.InitObjectRows(List: TList; FromIndex,
  ToIndex: Integer);
var
  I: Integer;
  ObjectRow: PObjectRow;
begin
  for I := FromIndex to ToIndex do
  begin
    ObjectRow := List[I];
    if not Assigned(ObjectRow) then
    begin
      New(ObjectRow);
      List[I] := ObjectRow;
    end;
    ObjectRow.Row := Succ(I);
    ObjectRow.Instance := nil;
  end;
end;

function TInstantNavigationalQuery.InternalAddObject(AObject: TObject): Integer;
var
  ObjectRow: PObjectRow;
begin
  New(ObjectRow);
  try
    ObjectRow.Row := -1;
    ObjectRow.Instance := AObject;
    if AObject is TInstantObject then
      TInstantObject(AObject).AddRef;
    Result := ObjectRowList.Add(ObjectRow);
  except
    Dispose(ObjectRow);
    raise;
  end;
end;

procedure TInstantNavigationalQuery.InternalClose;
begin
  inherited;
  if Assigned(DataSet) then
    DataSet.Close;
  DestroyObjectRowList;
end;

procedure TInstantNavigationalQuery.InternalGetInstantObjectRefs(List:
    TInstantObjectReferenceList);
var
  I: Integer;
begin
  for I := 0 to Pred(ObjectRowCount) do
    if (ObjectRows[I]^.Instance is TInstantObject) then
      List.Add(TInstantObject(ObjectRows[I]^.Instance));
end;

function TInstantNavigationalQuery.InternalGetObjectCount: Integer;
begin
  if DataSet.Active then
    Result := ObjectRowList.Count
  else
    Result := 0;
end;

function TInstantNavigationalQuery.InternalGetObjects(Index: Integer): TObject;
var
  ObjectRow: PObjectRow;
begin
  ObjectRow := ObjectRows[Index];
  if not Assigned(ObjectRow.Instance) then
    ObjectRow.Instance := CreateObject(ObjectRow.Row);
  Result := ObjectRow.Instance;
end;

function TInstantNavigationalQuery.InternalIndexOfObject(
  AObject: TObject): Integer;
begin
  for Result := 0 to Pred(ObjectRowCount) do
    if ObjectRows[Result].Instance = AObject then
      Exit;
  if AObject is TInstantObject then
    Result := Pred(RecNoOfObject(TInstantObject(AObject)))
  else
    Result := -1;
end;

procedure TInstantNavigationalQuery.InternalInsertObject(Index: Integer;
  AObject: TObject);
var
  ObjectRow: PObjectRow;
begin
  New(ObjectRow);
  try
    ObjectRow.Row := -1;
    ObjectRow.Instance := AObject;
    if AObject is TInstantObject then
      TInstantObject(AObject).AddRef;
    ObjectRowList.Insert(Index, ObjectRow);
  except
    Dispose(ObjectRow);
    raise;
  end;
end;

procedure TInstantNavigationalQuery.InternalOpen;
begin
  inherited;
  if Assigned(DataSet) then
    DataSet.Open;
end;

procedure TInstantNavigationalQuery.InternalRefreshObjects;
begin
  if not DataSet.Active then
    Exit;

  inherited;
end;

procedure TInstantNavigationalQuery.InternalReleaseObject(AObject: TObject);
var
  I: Integer;
  ObjectRow: PObjectRow;
begin
  for I := 0 to Pred(ObjectRowCount) do
  begin
    ObjectRow := ObjectRows[I];
    if ObjectRow.Instance = AObject then
    begin
      AObject.Free;
      ObjectRow.Instance := nil;
      Exit;
    end;
  end;
end;

function TInstantNavigationalQuery.InternalRemoveObject(
  AObject: TObject): Integer;
var
  ObjectRow: PObjectRow;
begin
  Result := IndexOfObject(AObject);
  if Result <> -1 then
  begin
    ObjectRow := ObjectRows[Result];
    ObjectRowList.Delete(Result);
    FreeAndNil(ObjectRow.Instance);
    Dispose(ObjectRow);
  end;
end;

function TInstantNavigationalQuery.IsSequenced: Boolean;
begin
  Result := True;
end;

function TInstantNavigationalQuery.ObjectFetched(Index: Integer): Boolean;
begin
  Result := Assigned(ObjectRows[Index].Instance);
end;

function TInstantNavigationalQuery.RecNoOfObject(
  AObject: TInstantObject): Integer;
var
  ClassField, IdField: TField;
begin
  if DataSet.IsSequenced then
  begin
    if DataSet.Locate(InstantClassFieldName + ';' + InstantIdFieldName,
      VarArrayOf([AObject.ClassName, AObject.Id]), []) then
    begin
      Result := Pred(DataSet.RecNo);
      Exit;
    end;
  end else
  begin
    ClassField := DataSet.FieldByName(InstantClassFieldName);
    IdField := DataSet.FieldByName(InstantIdFieldName);
    if Assigned(ClassField) and Assigned(IdField) then
    begin
      DataSet.First;
      Result := 1;
      while not DataSet.Eof do
      begin
        if (ClassField.AsString = AObject.ClassName) and
          (IdField.AsString = AObject.Id) then
          Exit;
        DataSet.Next;
        Inc(Result);
      end;
    end;
  end;
  Result := 0;
end;

procedure TInstantNavigationalQuery.SetRowNumber(Value: Integer);
begin
  if IsSequenced then
    DataSet.RecNo := Value
  else begin
    DataSet.First;
    while (not Dataset.Eof) and (Value > 1) do
    begin
      DataSet.Next;
      Dec(Value);
    end;
  end;
end;

{ TInstantSQLGenerator }

function TInstantSQLGenerator.BuildAssignment(const AName: string): string;
begin
  Result := EmbraceField(AName) + ' = ' + BuildParam(AName);
end;

function TInstantSQLGenerator.BuildAssignmentList(Map: TInstantAttributeMap;
  Additional: array of string): string;
begin
  Result := BuildList(Map, Additional, BuildAssignment);
end;

function TInstantSQLGenerator.BuildConcurrencyCriteria: string;
begin
  Result := Format(' AND %s = %s', [EmbraceField(InstantUpdateCountFieldName),
    BuildParam(ConcurrencyParamName)]);
end;

function TInstantSQLGenerator.BuildFieldList(Map: TInstantAttributeMap;
  Additional: array of string): string;
begin
  Result := BuildList(Map, Additional, EmbraceField);
end;

function TInstantSQLGenerator.BuildFieldList(const S: string): string;
var
  I: Integer;
  List: TStringList;
begin
  List := TStringList.Create;
  try
    InstantStrToList(S, List, [';']);
    Result := '';
    for I := 0 to Pred(List.Count) do
      Result := Result + InstantEmbrace(List[I], Broker.SQLDelimiters) + ', ';
    if Length(Result) > 0 then
      Delete(Result, Length(Result) - 1, 2);
  finally
    List.Free;
  end;
end;

function TInstantSQLGenerator.BuildList(Map: TInstantAttributeMap;
  Additional: array of string; StringFunc: TInstantStringFunc;
  const Delimiter: string): string;

  function SpaceDelimiter: string;
  begin
    Result := Format(' %s ', [Delimiter]);
  end;

var
  I: Integer;
  AttributeMetadata: TInstantAttributeMetadata;
  FieldName, RefClassFieldName, RefIdFieldName: string;
begin
  if not Assigned(StringFunc) then
    StringFunc := EmbraceField;
  Result := '';
  if Assigned(Map) then
    for I := 0 to Pred(Map.Count) do
    begin
      AttributeMetadata := Map[I];
      FieldName := AttributeMetadata.FieldName;
      if AttributeMetadata.AttributeType = atReference then
      begin
        RefClassFieldName := FieldName + InstantClassFieldName;
        RefIdFieldName := FieldName + InstantIdFieldName;
        Result := Result + StringFunc(RefClassFieldName) + ', ' +
          StringFunc(RefIdFieldName);
        Result := Result + SpaceDelimiter;
      end
      else if AttributeMetadata.AttributeType = atPart then
      begin
        if AttributeMetadata.StorageKind = skExternal then
        begin
          RefClassFieldName := FieldName + InstantClassFieldName;
          RefIdFieldName := FieldName + InstantIdFieldName;
          Result := Result + StringFunc(RefClassFieldName) + ', ' +
            StringFunc(RefIdFieldName);
          Result := Result + SpaceDelimiter;
        end
        else if AttributeMetadata.StorageKind = skEmbedded then
        begin
          Result := Result + StringFunc(FieldName);
          Result := Result + SpaceDelimiter;
        end;
      end
      else if AttributeMetadata.AttributeType in [atParts, atReferences] then
      begin
        if AttributeMetadata.StorageKind = skEmbedded then
        begin
          Result := Result + StringFunc(FieldName);
          Result := Result + SpaceDelimiter;
        end;
      end
      else
      begin
        Result := Result + StringFunc(FieldName);
        Result := Result + SpaceDelimiter;
      end;
    end;
  for I := Low(Additional) to High(Additional) do
    Result := Result + StringFunc(Additional[I]) + SpaceDelimiter;
  Delete(Result, Length(Result) - Length(Delimiter), Length(Delimiter) + 2);
end;

function TInstantSQLGenerator.BuildParam(const AName: string): string;
begin
  Result := ':' + AName;
end;

function TInstantSQLGenerator.BuildParamList(Map: TInstantAttributeMap;
  Additional: array of string): string;
begin
  Result := BuildList(Map, Additional, BuildParam);
end;

function TInstantSQLGenerator.BuildPersistentIdCriteria: string;
begin
  Result := Format(' AND %s = %s', [EmbraceField(InstantIdFieldName),
    BuildParam(PersistentIdParamName)]);
end;

function TInstantSQLGenerator.BuildWhereStr(Fields: array of string): string;
begin
  Result := BuildList(nil, Fields, BuildAssignment, 'AND');
end;

constructor TInstantSQLGenerator.Create(ABroker: TInstantSQLBroker);
begin
  inherited Create;
  FBroker := ABroker;
end;

function TInstantSQLGenerator.EmbraceField(const FieldName: string): string;
begin
  Result := InstantEmbrace(FieldName, Delimiters);
end;

function TInstantSQLGenerator.EmbraceTable(const TableName: string): string;
begin
  Result := InstantEmbrace(TableName, Delimiters);
end;

function TInstantSQLGenerator.GenerateAddFieldSQL(
  Metadata: TInstantFieldMetadata): string;
begin
  Result := InternalGenerateAddFieldSQL(Metadata);
end;

function TInstantSQLGenerator.GenerateAlterFieldSQL(OldMetadata,
  NewMetadata: TInstantFieldMetadata): string;
begin
  Result := InternalGenerateAlterFieldSQL(OldMetadata, NewMetadata);
end;

function TInstantSQLGenerator.GenerateCreateIndexSQL(
  Metadata: TInstantIndexMetadata): string;
begin
  Result := InternalGenerateCreateIndexSQL(Metadata);
end;

function TInstantSQLGenerator.GenerateCreateTableSQL(
  Metadata: TInstantTableMetadata): string;
begin
  Result := InternalGenerateCreateTableSQL(Metadata);
end;

function TInstantSQLGenerator.GenerateDeleteConcurrentSQL
  (Map: TInstantAttributeMap): string;
begin
  Result := InternalGenerateDeleteConcurrentSQL(Map);
end;

function TInstantSQLGenerator.GenerateDeleteExternalSQL(
  Map: TInstantAttributeMap): string;
begin
  Result := InternalGenerateDeleteExternalSQL(Map);
end;

function TInstantSQLGenerator.GenerateDeleteSQL
  (Map: TInstantAttributeMap): string;
begin
  Result := InternalGenerateDeleteSQL(Map);
end;

function TInstantSQLGenerator.GenerateDropFieldSQL(
  Metadata: TInstantFieldMetadata): string;
begin
  Result := InternalGenerateDropFieldSQL(Metadata);
end;

function TInstantSQLGenerator.GenerateDropIndexSQL(
  Metadata: TInstantIndexMetadata): string;
begin
  Result := InternalGenerateDropIndexSQL(Metadata);
end;

function TInstantSQLGenerator.GenerateDropTableSQL(
  Metadata: TInstantTableMetadata): string;
begin
  Result := InternalGenerateDropTableSQL(Metadata);
end;

function TInstantSQLGenerator.GenerateInsertExternalSQL(
  Map: TInstantAttributeMap): string;
begin
  Result := InternalGenerateInsertExternalSQL(Map)
end;

function TInstantSQLGenerator.GenerateInsertSQL
  (Map: TInstantAttributeMap): string;
begin
  Result := InternalGenerateInsertSQL(Map);
end;

function TInstantSQLGenerator.GenerateSelectExternalPartSQL(
  Map: TInstantAttributeMap): string;
begin
  Result := InternalGenerateSelectExternalPartSQL(Map);
end;

function TInstantSQLGenerator.GenerateSelectExternalSQL(
  Map: TInstantAttributeMap): string;
begin
  Result := InternalGenerateSelectExternalSQL(Map);
end;

function TInstantSQLGenerator.GenerateSelectSQL
  (Map: TInstantAttributeMap): string;
begin
  Result := InternalGenerateSelectSQL(Map);
end;

function TInstantSQLGenerator.GenerateSelectTablesSQL: string;
begin
  Result := InternalGenerateSelectTablesSQL;
end;

function TInstantSQLGenerator.GenerateUpdateConcurrentSQL
  (Map: TInstantAttributeMap): string;
begin
  Result := InternalGenerateUpdateConcurrentSQL(Map);
end;

function TInstantSQLGenerator.GenerateUpdateFieldCopySQL(OldMetadata,
    NewMetadata: TInstantFieldMetadata): string;
begin
  Result := InternalGenerateUpdateFieldCopySQL(OldMetadata, NewMetadata);
end;

function TInstantSQLGenerator.GenerateUpdateSQL
  (Map: TInstantAttributeMap): string;
begin
  Result := InternalGenerateUpdateSQL(Map);
end;

function TInstantSQLGenerator.GetDelimiters: string;
begin
  Result := Broker.SQLDelimiters;
end;

function TInstantSQLGenerator.InternalGenerateAddFieldSQL(
  Metadata: TInstantFieldMetadata): string;
begin
  with Metadata do
    Result := Format('ALTER TABLE %s ADD %s %s',
      [EmbraceTable(TableMetadata.Name),
       EmbraceField(Name),
       Broker.DataTypeToColumnType(DataType, Size)]);
end;

function TInstantSQLGenerator.InternalGenerateAlterFieldSQL(OldMetadata,
  NewMetadata: TInstantFieldMetadata): string;
begin
  Result := Format('ALTER TABLE %s ALTER COLUMN %s TYPE %s',
    [EmbraceTable(OldMetadata.TableMetadata.Name),
     EmbraceField(OldMetadata.Name),
     Broker.DataTypeToColumnType(NewMetadata.DataType, NewMetadata.Size)]);
end;

function TInstantSQLGenerator.InternalGenerateCreateIndexSQL(
  Metadata: TInstantIndexMetadata): string;
var
  Modifier, Columns, TableName: string;
begin
  if ixUnique in Metadata.Options then
    Modifier := 'UNIQUE '
  else
    Modifier := '';
  if ixDescending in Metadata.Options then
    Modifier := Modifier + 'DESCENDING ';
  Columns := BuildFieldList(Metadata.Fields);
  TableName := Metadata.TableMetadata.Name;
  Result := Format('CREATE %sINDEX %s ON %s (%s)',
    [Modifier, Metadata.Name, EmbraceTable(TableName), Columns]);
end;

function TInstantSQLGenerator.InternalGenerateCreateTableSQL(
  Metadata: TInstantTableMetadata): string;
var
  I: Integer;
  FieldMetadata: TInstantFieldMetadata;
  Columns, PrimaryKey: string;
begin
  Columns := '';
  with Metadata do
    for I := 0 to Pred(FieldMetadatas.Count) do
    begin
      FieldMetadata := FieldMetadatas[I];
      with FieldMetadata do
      begin
        if I > 0 then
          Columns := Columns + ', ';
        Columns := Columns + EmbraceField(Name) + ' ' + Broker.DataTypeToColumnType(DataType, Size);
        if foRequired in Options then
          Columns := Columns + ' NOT NULL';
      end;
    end;
  PrimaryKey := '';
  with Metadata do
    for I := 0 to Pred(IndexMetadatas.Count) do
      with IndexMetadatas[I] do
        if ixPrimary in Options then
        begin
          PrimaryKey := BuildFieldList(Fields);
          Break;
        end;
  if PrimaryKey <> '' then
    Columns := Columns + ', PRIMARY KEY (' + PrimaryKey + ')';
  Result := Format('CREATE TABLE %s (%s)',
    [EmbraceTable(Metadata.Name), Columns]);
end;

function TInstantSQLGenerator.InternalGenerateDeleteConcurrentSQL
  (Map: TInstantAttributeMap): string;
begin
  Result := InternalGenerateDeleteSQL(Map) + BuildConcurrencyCriteria;
end;

function TInstantSQLGenerator.InternalGenerateDeleteExternalSQL(
  Map: TInstantAttributeMap): string;
var
  WhereStr: string;
begin
  WhereStr := BuildWhereStr([InstantParentClassFieldName,
    InstantParentIdFieldName]);
  Result := Format('DELETE FROM %s WHERE %s',
    [EmbraceTable('%s'), WhereStr]);
end;

function TInstantSQLGenerator.InternalGenerateDeleteSQL
  (Map: TInstantAttributeMap): string;
var
  WhereStr: string;
begin
  WhereStr := BuildWhereStr([InstantClassFieldName, InstantIdFieldName]);
  Result := Format('DELETE FROM %s WHERE %s',
    [EmbraceTable(Map.Name), WhereStr]);
end;

function TInstantSQLGenerator.InternalGenerateDropFieldSQL(
  Metadata: TInstantFieldMetadata): string;
begin
  with Metadata do
    Result := Format('ALTER TABLE %s DROP %s',
      [EmbraceTable(TableMetadata.Name),
       EmbraceField(Name)]);
end;

function TInstantSQLGenerator.InternalGenerateDropIndexSQL(
  Metadata: TInstantIndexMetadata): string;
begin
  Result := Format('DROP INDEX %s', [Metadata.Name]);
end;

function TInstantSQLGenerator.InternalGenerateDropTableSQL(
  Metadata: TInstantTableMetadata): string;
begin
  Result := Format('DROP TABLE %s', [EmbraceTable(Metadata.Name)]);
end;

function TInstantSQLGenerator.InternalGenerateInsertExternalSQL(
  Map: TInstantAttributeMap): string;
var
  FieldStr, ParamStr: string;
begin
  FieldStr := Format('%s, %s, %s, %s, %s, %s',
    [EmbraceField(InstantIdFieldName),
    EmbraceField(InstantParentClassFieldName),
    EmbraceField(InstantParentIdFieldName),
    EmbraceField(InstantChildClassFieldName),
    EmbraceField(InstantChildIdFieldName),
    EmbraceField(InstantSequenceNoFieldName)]);
  ParamStr := Format(':%s, :%s, :%s, :%s, :%s, :%s',
    [InstantIdFieldName,
    InstantParentClassFieldName, InstantParentIdFieldName,
    InstantChildClassFieldName, InstantChildIdFieldName,
    InstantSequenceNoFieldName]);
  Result := Format('INSERT INTO %s (%s) VALUES (%s)',
    [EmbraceTable('%s'), FieldStr, ParamStr]);
end;

function TInstantSQLGenerator.InternalGenerateInsertSQL
  (Map: TInstantAttributeMap): string;
var
  FieldStr, ParamStr: string;
begin
  FieldStr := BuildFieldList(Map,
    [InstantClassFieldName, InstantIdFieldName, InstantUpdateCountFieldName]);
  ParamStr := BuildParamList(Map,
    [InstantClassFieldName, InstantIdFieldName, InstantUpdateCountFieldName]);
  Result := Format('INSERT INTO %s (%s) VALUES (%s)',
    [EmbraceTable(Map.Name), FieldStr, ParamStr]);
  Result := Result + ' ';
end;

function TInstantSQLGenerator.InternalGenerateSelectExternalPartSQL(
  Map: TInstantAttributeMap): string;
var
  FieldStr, WhereStr: string;
begin
  FieldStr := Format('%s, %s', [EmbraceField('%s'), EmbraceField('%s')]);
  WhereStr := Format('%s = :%s AND %s = :%s',
    [EmbraceField(InstantClassFieldName), InstantClassFieldName,
     EmbraceField(InstantIdFieldName), InstantIdFieldName]);
  Result := Format('SELECT %s FROM %s WHERE %s',
    [FieldStr, EmbraceTable('%s'), WhereStr]);
end;

function TInstantSQLGenerator.InternalGenerateSelectExternalSQL(
  Map: TInstantAttributeMap): string;
var
  FieldStr, WhereStr: string;
begin
  FieldStr := Format('%s, %s, %s', [EmbraceField(InstantChildClassFieldName),
    EmbraceField(InstantChildIdFieldName), EmbraceField(InstantSequenceNoFieldName)]);
  WhereStr := Format('%s = :%s AND %s = :%s AND %s = :%s',
    [EmbraceField(InstantParentClassFieldName), InstantParentClassFieldName,
    EmbraceField(InstantParentIdFieldName), InstantParentIdFieldName,
    EmbraceField(InstantChildClassFieldName), InstantChildClassFieldName]);
  Result := Format('SELECT %s FROM %s WHERE %s ORDER BY %s',
    [FieldStr, EmbraceTable('%s'), WhereStr, EmbraceField(InstantSequenceNoFieldName)]);
end;

function TInstantSQLGenerator.InternalGenerateSelectSQL
  (Map: TInstantAttributeMap): string;
var
  FieldStr, WhereStr: string;
begin
  FieldStr := BuildFieldList(Map, [InstantUpdateCountFieldName]);
  WhereStr := BuildWhereStr([InstantClassFieldName, InstantIdFieldName]);
  Result := Format('SELECT %s FROM %s WHERE %s',
    [FieldStr, EmbraceTable(Map.Name), WhereStr]);
end;

function TInstantSQLGenerator.InternalGenerateSelectTablesSQL: string;
begin
  raise EInstantError.CreateFmt(SUnsupportedOperation,
    ['InternalGenerateSelectTablesSQL']);
end;

function TInstantSQLGenerator.InternalGenerateUpdateConcurrentSQL
  (Map: TInstantAttributeMap): string;
begin
  Result := InternalGenerateUpdateSQL(Map) + BuildConcurrencyCriteria;
end;

function TInstantSQLGenerator.InternalGenerateUpdateFieldCopySQL(OldMetadata,
    NewMetadata: TInstantFieldMetadata): string;
begin
  Result := Format('UPDATE %s SET %s = %s',
                    [EmbraceTable(OldMetadata.TableMetadata.Name),
                     EmbraceField(NewMetadata.Name),
                     EmbraceField(OldMetadata.Name)]);
end;

function TInstantSQLGenerator.InternalGenerateUpdateSQL
  (Map: TInstantAttributeMap): string;
var
  AssignmentStr, WhereStr: string;
begin
  AssignmentStr := BuildAssignmentList(Map,
    [InstantIdFieldName, InstantUpdateCountFieldName]);
  WhereStr := BuildWhereStr([InstantClassFieldName]) +
    BuildPersistentIdCriteria;
  Result := Format('UPDATE %s SET %s WHERE %s',
    [EmbraceTable(Map.Name), AssignmentStr, WhereStr]);
end;

{ TInstantSQLBroker }

function TInstantSQLBroker.AcquireDataSet(const AStatement: string;
  AParams: TParams): TDataSet;
var
  CachedStatement: TInstantStatement;
begin
  Result := nil;
  if FStatementCacheCapacity <> 0 then
  begin
    CachedStatement := StatementCache.GetStatement(AStatement);
    if Assigned(CachedStatement) then
    begin
      Result := TDataSet(CachedStatement.StatementImplementation);
      AssignDataSetParams(Result, AParams);
    end;
  end;
  if not Assigned(Result) then
  begin
    Result := CreateDataSet(AStatement, AParams);
    try
      if Assigned(AParams) and (FStatementCacheCapacity <> 0) then
        StatementCache.AddStatement(AStatement, Result);
    except
      if FStatementCacheCapacity <> 0 then
        StatementCache.RemoveStatement(AStatement);
      Result.Free;
      raise;
    end;
  end;
end;

procedure TInstantSQLBroker.AssignDataSetParams(DataSet: TDataSet; AParams: TParams);
begin
  raise EInstantError.CreateFmt(SMissingImplementation, ['AssignDataSetParams', ClassName]);
end;

destructor TInstantSQLBroker.Destroy;
begin
  FGenerator.Free;
  FResolverList.Free;
  inherited;
end;

function TInstantSQLBroker.EnsureResolver(
  AMap: TInstantAttributeMap): TInstantCustomResolver;
begin
  Result := FindResolver(AMap);
  if not Assigned(Result) then
  begin
    Result := CreateResolver(AMap);
    ResolverList.Add(Result)
  end;
end;

function TInstantSQLBroker.FindResolver(
  AMap: TInstantAttributeMap): TInstantSQLResolver;
var
  I: Integer;
begin
  for I := 0 to Pred(ResolverCount) do
  begin
    Result := Resolvers[I];
    if Result.Map = AMap then
      Exit;
  end;
  Result := nil;
end;

class function TInstantSQLBroker.GeneratorClass: TInstantSQLGeneratorClass;
begin
  Result := TInstantSQLGenerator;
end;

function TInstantSQLBroker.GetGenerator: TInstantSQLGenerator;
begin
  if not Assigned(FGenerator) then
    FGenerator := GeneratorClass.Create(Self);
  Result := FGenerator;
end;

function TInstantSQLBroker.GetResolverCount: Integer;
begin
  Result := ResolverList.Count;
end;

function TInstantSQLBroker.GetResolverList: TObjectList;
begin
  if not Assigned(FResolverList) then
    FResolverList := TObjectList.Create;
  Result := FResolverList;
end;

function TInstantSQLBroker.GetResolvers(
  Index: Integer): TInstantSQLResolver;
begin
  Result := ResolverList[Index] as TInstantSQLResolver;
end;

procedure TInstantSQLBroker.InternalBuildDatabase(Scheme: TInstantScheme);
var
  I, J: Integer;
  TableMetadata: TInstantTableMetadata;
  IndexMetadata: TInstantIndexMetadata;
begin
  if not Assigned(Scheme) then
    Exit;
  with Scheme do
  begin
    for I := 0 to Pred(TableMetadataCount) do
    begin
      TableMetadata := TableMetadatas[I];
      try
        Execute(Generator.GenerateDropTableSQL(TableMetadata));
      except
      end;
      Execute(Generator.GenerateCreateTableSQL(TableMetadata));
      with TableMetadata do
      begin
        for J := 0 to Pred(IndexMetadatas.Count) do
        begin
          IndexMetadata := IndexMetadatas[J];
          if not (ixPrimary in IndexMetadata.Options) then
            Execute(Generator.GenerateCreateIndexSQL(IndexMetadata));
        end;
      end;
    end;
  end;
end;

procedure TInstantSQLBroker.ReleaseDataSet(const ADataSet: TDataSet);
begin
  if FStatementCacheCapacity <> 0 then
    ADataSet.Close
  else
    ADataSet.Free;
end;

{ TInstantSQLResolver }

procedure TInstantSQLResolver.AddAttributeParam(Attribute: TInstantAttribute;
  Params: TParams);
var
  FieldName: string;

  procedure AddBlobParam(const AFieldName, Value: string);
  var
    Param: TParam;
  begin
    Param := AddParam(Params, AFieldName, ftBlob);
    if Value <> '' then
      Param.AsBlob := Value
  end;

  procedure AddMemoParam(const AFieldName, Value: string);
  var
    Param: TParam;
  begin
    Param := AddParam(Params, AFieldName, ftMemo);
    if Value <> '' then
      Param.AsMemo := Value
  end;

  procedure AddBlobAttributeParam;
  begin
    AddBlobParam(FieldName, (Attribute as TInstantBlob).Value);
  end;

  procedure AddBooleanAttributeParam;
  begin
    AddParam(Params, FieldName, ftBoolean).AsBoolean :=
      (Attribute as TInstantBoolean).Value;
  end;

  procedure AddDateTimeAttributeParam;
  begin
    AddParam(Params, FieldName, ftDateTime).AsDateTime :=
      (Attribute as TInstantDateTime).Value;
  end;

  procedure AddFloatAttributeParam;
  begin
    AddParam(Params, FieldName, ftFloat).AsFloat :=
      (Attribute as TInstantFloat).Value;
  end;

  procedure AddCurrencyAttributeParam;
  begin
    AddParam(Params, FieldName, ftBCD).AsCurrency :=
      (Attribute as TInstantCurrency).Value;
  end;

  procedure AddIntegerAttributeParam;
  begin
    AddIntegerParam(Params, FieldName, (Attribute as TInstantInteger).Value);
  end;

  procedure AddMemoAttributeParam;
  var
    Param: TParam;
    MemoAttrib: TInstantMemo;
  begin
    Param := AddParam(Params, FieldName, ftMemo);
    MemoAttrib := (Attribute as TInstantMemo);
    if MemoAttrib.Size <> 0 then
      Param.AsMemo := MemoAttrib.Value;
  end;

  procedure AddPartAttributeParam;
  var
    Stream: TInstantStringStream;
    Part: TInstantPart;
  begin
    if Attribute.Metadata.StorageKind = skExternal then
    begin
      Part := Attribute as TInstantPart;
      AddStringParam(Params, FieldName + InstantClassFieldName, Part.Value.ClassName);
      Part.Value.CheckId;
      AddIdParam(Params, FieldName + InstantIdFieldName, Part.Value.Id);
    end
    else
    begin
      Stream := TInstantStringStream.Create('');
      try
        (Attribute as TInstantPart).SaveObjectToStream(Stream);
        if Broker.Connector.BlobStreamFormat = sfBinary then
          AddBlobParam(FieldName, Stream.DataString)
        else
          AddMemoParam(FieldName, Stream.DataString);
      finally
        Stream.Free;
      end;
    end;
  end;

  procedure AddPartsAttributeParam;
  var
    Stream: TInstantStringStream;
  begin
    Stream := TInstantStringStream.Create('');
    try
      (Attribute as TInstantParts).SaveObjectsToStream(Stream);
      if Broker.Connector.BlobStreamFormat = sfBinary then
        AddBlobParam(FieldName, Stream.DataString)
      else
        AddMemoParam(FieldName, Stream.DataString);
    finally
      Stream.Free;
    end;
  end;

  procedure AddReferenceAttributeParams;
  var
    Reference: TInstantReference;
  begin
    Reference := Attribute as TInstantReference;
    AddStringParam(Params, FieldName + InstantClassFieldName,
      Reference.ObjectClassName);
    AddIdParam(Params, FieldName + InstantIdFieldName, Reference.ObjectId);
  end;

  procedure AddReferencesAttributeParam;
  var
    Stream: TInstantStringStream;
  begin
    Stream := TInstantStringStream.Create('');
    try
      (Attribute as TInstantReferences).SaveReferencesToStream(Stream);
      if Broker.Connector.BlobStreamFormat = sfBinary then
        AddBlobParam(FieldName, Stream.DataString)
      else
        AddMemoParam(FieldName, Stream.DataString);
    finally
      Stream.Free;
    end;
  end;

  procedure AddStringAttributeParam;
  begin
    AddStringParam(Params, FieldName, (Attribute as TInstantString).Value);
  end;

begin
  FieldName := Attribute.Metadata.FieldName;
  case Attribute.Metadata.AttributeType of
    atBlob, atGraphic:
      AddBlobAttributeParam;
    atBoolean:
      AddBooleanAttributeParam;
    atDateTime:
      AddDateTimeAttributeParam;
    atFloat:
      AddFloatAttributeParam;
    atCurrency:
      AddCurrencyAttributeParam;
    atInteger:
      AddIntegerAttributeParam;
    atMemo:
      AddMemoAttributeParam;
    atPart:
      AddPartAttributeParam;
    atParts:
      AddPartsAttributeParam;
    atReference:
      AddReferenceAttributeParams;
    atReferences:
      AddReferencesAttributeParam;
    atString:
      AddStringAttributeParam;
  end;
end;

procedure TInstantSQLResolver.AddAttributeParams(Params: TParams;
  AObject: TInstantObject; Map: TInstantAttributeMap);
var
  I: Integer;
  Attribute: TInstantAttribute;
begin
  if Assigned(Params) and Assigned(AObject) and Assigned(Map) then
    for I := 0 to Pred(Map.Count) do
    begin
      Attribute := AObject.AttributeByName(Map[I].Name); 
      if (Attribute.Metadata.StorageKind = skEmbedded) or
         ((Attribute.Metadata.StorageKind = skExternal) and (Attribute.Metadata.AttributeType = atPart)) then
        AddAttributeParam(Attribute, Params);
    end;
end;

procedure TInstantSQLResolver.AddBaseParams(Params: TParams; AClassName,
  AObjectId: string; AUpdateCount: Integer);
begin
  if Assigned(Params) then
  begin
    AddStringParam(Params, InstantClassFieldName, AClassName);
    AddIdParam(Params, InstantIdFieldName, AObjectId);
    if AUpdateCount <> -1 then
      AddIntegerParam(Params, InstantUpdateCountFieldName, AUpdateCount);
  end;
end;

procedure TInstantSQLResolver.AddConcurrencyParam(Params: TParams;
  AUpdateCount: Integer);
begin
  AddIntegerParam(Params, ConcurrencyParamName, AUpdateCount);
end;

procedure TInstantSQLResolver.AddIntegerParam(Params: TParams;
  const ParamName: string; Value: Integer);
begin
  AddParam(Params, ParamName, ftInteger).AsInteger := Value;
end;

function TInstantSQLResolver.AddParam(Params: TParams;
  const ParamName: string; ADataType: TFieldType): TParam;
begin
  Result := TParam(Params.Add);
  Result.Name := ParamName;
  Result.DataType := ADataType;
end;

procedure TInstantSQLResolver.AddPersistentIdParam(Params: TParams;
  APersistentId: string);
begin
  AddIdParam(Params, PersistentIdParamName, APersistentId);
end;

procedure TInstantSQLResolver.AddStringParam(Params: TParams;
  const ParamName, Value: string);
var
  Param: TParam;
begin
  Param := AddParam(Params, ParamName, ftString);
  if Value <> '' then
    Param.AsString := Value;
end;

procedure TInstantSQLResolver.AddIdParam(Params: TParams;
  const ParamName, Value: string);
var
  Param: TParam;
begin
  Param := AddParam(Params, ParamName, InstantDataTypeToFieldType(Broker.Connector.IdDataType));
  if Value <> '' then
    Param.Value := Value;
end;

procedure TInstantSQLResolver.CheckConflict(Info: PInstantOperationInfo;
 AObject: TInstantObject);
begin
  if Info.Conflict then
    raise EInstantConflict.CreateFmt(SUpdateConflict,
      [AObject.ClassName, AObject.Id]);
end;

constructor TInstantSQLResolver.Create(ABroker: TInstantSQLBroker;
  AMap: TInstantAttributeMap);
begin
  if not Assigned(AMap) then
    raise EInstantError.Create(SUnassignedMap);
  inherited Create(ABroker);
  FMap := AMap;
end;

function TInstantSQLResolver.ExecuteStatement(const AStatement: string;
  AParams: TParams; Info: PInstantOperationInfo;
  ConflictAction: TInstantConflictAction; AObject: TInstantObject): Integer;
var
  TransError: Exception;
begin
  {$IFDEF IO_STATEMENT_LOGGING}
  InstantLogStatement('Before: ', AStatement, AParams);
  {$ENDIF}
  try
    Result := Broker.Execute(AStatement, AParams);
    Info.Success := Result >= 1;
    Info.Conflict := not (Info.Success or (ConflictAction = caIgnore));
  except
    on EAbort do
      raise;
    on E: Exception do
    begin
      TransError := TranslateError(AObject, E);
      if Assigned(TransError) then
        raise TransError
      else
        raise;
    end;
  end;
end;

function TInstantSQLResolver.GetBroker: TInstantSQLBroker;
begin
  Result := inherited Broker as TInstantSQLBroker;
end;

function TInstantSQLResolver.GetDeleteConcurrentSQL: string;
begin
  if FDeleteConcurrentSQL = '' then
    FDeleteConcurrentSQL := Broker.Generator.GenerateDeleteConcurrentSQL(Map);
  Result := FDeleteConcurrentSQL;
end;

function TInstantSQLResolver.GetDeleteSQL: string;
begin
  if FDeleteSQL = '' then
    FDeleteSQL := Broker.Generator.GenerateDeleteSQL(Map);
  Result := FDeleteSQL;
end;

function TInstantSQLResolver.GetDeleteExternalSQL: string;
begin
  if FDeleteExternalSQL = '' then
    FDeleteExternalSQL := Broker.Generator.GenerateDeleteExternalSQL(Map);
  Result := FDeleteExternalSQL;
end;

function TInstantSQLResolver.GetInsertSQL: string;
begin
  if FInsertSQL = '' then
    FInsertSQL := Broker.Generator.GenerateInsertSQL(Map);
  Result := FInsertSQL;
end;

function TInstantSQLResolver.GetInsertExternalSQL: string;
begin
  if FInsertExternalSQL = '' then
    FInsertExternalSQL := Broker.Generator.GenerateInsertExternalSQL(Map);
  Result := FInsertExternalSQL;
end;

function TInstantSQLResolver.GetSelectSQL: string;
begin
  if FSelectSQL = '' then
    FSelectSQL := Broker.Generator.GenerateSelectSQL(Map);
  Result := FSelectSQL;
end;

function TInstantSQLResolver.GetSelectExternalSQL: string;
begin
  if FSelectExternalSQL = '' then
    FSelectExternalSQL := Broker.Generator.GenerateSelectExternalSQL(Map);
  Result := FSelectExternalSQL;
end;

function TInstantSQLResolver.GetSelectExternalPartSQL: string;
begin
  if FSelectExternalPartSQL = '' then
    FSelectExternalPartSQL := Broker.Generator.GenerateSelectExternalPartSQL(Map);
  Result := FSelectExternalPartSQL;
end;

function TInstantSQLResolver.GetUpdateConcurrentSQL: string;
begin
  if FUpdateConcurrentSQL = '' then
    FUpdateConcurrentSQL := Broker.Generator.GenerateUpdateConcurrentSQL(Map);
  Result := FUpdateConcurrentSQL;
end;

function TInstantSQLResolver.GetUpdateSQL: string;
begin
  if FUpdateSQL = '' then
    FUpdateSQL := Broker.Generator.GenerateUpdateSQL(Map);
  Result := FUpdateSQL;
end;

procedure TInstantSQLResolver.InternalDisposeMap(AObject: TInstantObject;
  Map: TInstantAttributeMap; ConflictAction: TInstantConflictAction;
  Info: PInstantOperationInfo);
var
  Params: TParams;
  Statement: string;
  AInfo: TInstantOperationInfo;

  procedure DeleteExternalPartMap;
  var
    i: Integer;
    PartObject: TInstantObject;
    SelectParams: TParams;
    SelectStatement: string;
    AttributeMetadata: TInstantAttributeMetadata;
    DataSet: TDataSet;
  begin
    for i := 0 to Pred(Map.Count) do
    begin
      AttributeMetadata := Map[i];
      if Map[i].AttributeType = atPart then
      begin
        if Map[i].StorageKind = skExternal then
        begin
          // Dispose object
          SelectParams := TParams.Create;
          try
            SelectStatement := Format(SelectExternalPartSQL,
              [AttributeMetadata.FieldName + InstantClassFieldName,
               AttributeMetadata.FieldName + InstantIdFieldName,
               AttributeMetadata.ClassMetadata.TableName]);
            AddStringParam(SelectParams, InstantClassFieldName, AObject.ClassName);
            AddIdParam(SelectParams, InstantIdFieldName, AObject.Id);
            DataSet := Broker.AcquireDataSet(SelectStatement, SelectParams);
            try
              DataSet.Open;
              try
                if not DataSet.IsEmpty then
                begin
                  PartObject := AttributeMetadata.ObjectClass.Retrieve(
                    DataSet.Fields[1].AsString, False, False, AObject.Connector);
                  try
                    if Assigned(PartObject) then
                      PartObject.ObjectStore.DisposeObject(PartObject, caIgnore);
                  finally
                    PartObject.Free;
                  end;
                end;
              finally
                DataSet.Close;
              end;
            finally
              Broker.ReleaseDataSet(DataSet);
            end;
          finally
            SelectParams.Free;
          end;

        end;
      end;
    end;
  end;

  procedure DeleteExternalPartsMap;
  var
    i: Integer;
    PartObject: TInstantObject;
    SelectParams, DeleteParams: TParams;
    SelectStatement, DeleteStatement: string;
    AttributeMetadata: TInstantAttributeMetadata;
    DataSet: TDataSet;
  begin
    for i := 0 to Pred(Map.Count) do
    begin
      AttributeMetadata := Map[i];
      if Map[i].AttributeType = atParts then
      begin
        if Map[i].StorageKind = skExternal then
        begin
          // dispose all objects
          SelectParams := TParams.Create;
          try
            SelectStatement := Format(SelectExternalSQL, [AttributeMetadata.ExternalStorageName]);
            AddIdParam(SelectParams, InstantParentIdFieldName, AObject.Id);
            AddStringParam(SelectParams, InstantParentClassFieldName, AObject.ClassName);
            AddStringParam(SelectParams, InstantChildClassFieldName, AttributeMetadata.ObjectClassName);
            DataSet := Broker.AcquireDataSet(SelectStatement, SelectParams);
            try
              DataSet.Open;
              try
                while not DataSet.Eof do
                begin
                  PartObject := AttributeMetadata.ObjectClass.Retrieve(
                    DataSet.Fields[1].AsString, False, False, AObject.Connector);
                  try
                    if Assigned(PartObject) then
                      PartObject.ObjectStore.DisposeObject(PartObject, caIgnore);
                  finally
                    PartObject.Free;
                  end;
                  DataSet.Next;
                end;
              finally
                DataSet.Close;
              end;
            finally
              Broker.ReleaseDataSet(DataSet);
            end;
          finally
            SelectParams.Free;
          end;

          // Delete all links
          DeleteParams := TParams.Create;
          try
            DeleteStatement := Format(DeleteExternalSQL,
              [AttributeMetadata.ExternalStorageName,
              InstantParentClassFieldName,
              InstantParentIdFieldName]);
            AddStringParam(DeleteParams, InstantParentClassFieldName, AObject.ClassName);
            AddIdParam(DeleteParams, InstantParentIdFieldName, AObject.Id);
            Broker.Execute(DeleteStatement, DeleteParams);
          finally
            DeleteParams.Free;
          end;
        end;
      end;
    end;
  end;

  procedure DeleteExternalReferencesMap;
  var
    i: Integer;
    DeleteParams: TParams;
    DeleteStatement: string;
    AttributeMetadata: TInstantAttributeMetadata;
  begin
    for i := 0 to Pred(Map.Count) do
    begin
      AttributeMetadata := Map[i];
      if AttributeMetadata.AttributeType = atReferences then
      begin
        if AttributeMetadata.StorageKind = skExternal then
        begin
          // Delete all links
          DeleteParams := TParams.Create;
          try
            DeleteStatement := Format(DeleteExternalSQL,
              [AttributeMetadata.ExternalStorageName,
              InstantParentClassFieldName]);
            AddStringParam(DeleteParams, InstantParentClassFieldName, AObject.ClassName);
            AddIdParam(DeleteParams, InstantParentIdFieldName, AObject.Id);
            Broker.Execute(DeleteStatement, DeleteParams);
          finally
            DeleteParams.Free;
          end;
        end;
      end;
    end;
  end;

begin
  if not Assigned(Info) then
    Info := @AInfo;
  DeleteExternalPartMap;
  DeleteExternalPartsMap;
  DeleteExternalReferencesMap;
  Params := TParams.Create;
  try
    AddBaseParams(Params, AObject.ClassName, AObject.PersistentId);
    if Map.IsRootMap and (ConflictAction = caFail) then
    begin
      Statement := DeleteConcurrentSQL;
      AddConcurrencyParam(Params, AObject.UpdateCount);
      ExecuteStatement(Statement, Params, Info, ConflictAction, AObject);
      CheckConflict(Info, AObject);
    end else
    begin
      Statement := DeleteSQL;
      ExecuteStatement(Statement, Params, Info, ConflictAction, AObject)
    end;
  finally
    Params.Free;
  end;
end;

procedure TInstantSQLResolver.InternalRetrieveMap(AObject: TInstantObject;
  const AObjectId: string; Map: TInstantAttributeMap;
  ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo);

  procedure ResetAttributes;
  var
    I: Integer;
  begin
    for I := 0 to Pred(Map.Count) do
      AObject.AttributeByName(Map[I].Name).Reset
  end;

var
  DataSet: TDataSet;
  Params: TParams;
  AInfo: TInstantOperationInfo;
begin
  if not Assigned(Info) then
    Info := @AInfo;
  Params := TParams.Create;
  try
    AddBaseParams(Params, AObject.ClassName, AObjectId);
    DataSet := Broker.AcquireDataSet(SelectSQL, Params);
    try
      DataSet.Open;
      Info.Success := not DataSet.EOF;
      Info.Conflict := not Info.Success;
      if Info.Success then
      begin
        if Map.IsRootMap then
          Broker.SetObjectUpdateCount(AObject, DataSet.FieldByName(InstantUpdateCountFieldName).AsInteger);
        ReadAttributes(AObject, AObjectId, Map, DataSet);
      end else
        ResetAttributes;
    finally
      Broker.ReleaseDataSet(DataSet);
    end;
  finally
    Params.Free;
  end;
end;

procedure TInstantSQLResolver.InternalStoreMap(AObject: TInstantObject;
  Map: TInstantAttributeMap; ConflictAction: TInstantConflictAction;
  Info: PInstantOperationInfo);
var
  Params: TParams;
  AInfo: TInstantOperationInfo;
  NewUpdateCount, RowsAffected: Integer;

  procedure InsertMap;
  begin
    if AObject.IsPersistent then
    begin
      RemoveConcurrencyParam(Params);
      RemovePersistentIdParam(Params);
    end;
    RowsAffected := ExecuteStatement(InsertSQL, Params, Info, ConflictAction, AObject);
  end;

  procedure UpdateMap;
  var
    Statement: string;
  begin
    AddPersistentIdParam(Params, AObject.PersistentId);
    if Map.IsRootMap and (ConflictAction = caFail) then
    begin
      Statement := UpdateConcurrentSQL;
      AddConcurrencyParam(Params, AObject.UpdateCount);
      ExecuteStatement(Statement, Params, Info, ConflictAction, AObject);
      CheckConflict(Info, AObject);
    end else
    begin
      Statement := UpdateSQL;
      ExecuteStatement(Statement, Params, Info, ConflictAction, AObject);
    end;
    if not Info.Success then
      InsertMap;
  end;

  procedure UpdateExternalPartMap;
  var
    i: Integer;
    PartObject: TInstantObject;
    PartAttribute: TInstantPart;
    AttributeMetadata: TInstantAttributeMetadata;
    SelectParams: TParams;
    SelectStatement: string;
    DataSet: TDataSet;
  begin
    for i := 0 to Pred(Map.Count) do
    begin
      AttributeMetadata := Map[i];
      if AttributeMetadata.AttributeType = atPart then
      begin
        PartAttribute := TInstantPart(AObject.AttributeByName(AttributeMetadata.Name));
        if PartAttribute.IsChanged or not PartAttribute.Value.IsPersistent then
        begin
          if Map[i].StorageKind = skExternal then
          begin
            // Delete object
            SelectParams := TParams.Create;
            try
              SelectStatement := Format(SelectExternalPartSQL,
                [AttributeMetadata.FieldName + InstantClassFieldName,
                 AttributeMetadata.FieldName + InstantIdFieldName,
                 AttributeMetadata.ClassMetadata.TableName]);
              AddStringParam(SelectParams, InstantClassFieldName, AObject.ClassName);
              AddIdParam(SelectParams, InstantIdFieldName, AObject.Id);
              DataSet := Broker.AcquireDataSet(SelectStatement, SelectParams);
              try
                DataSet.Open;
                try
                  if not DataSet.IsEmpty then
                  begin
                    PartObject := AttributeMetadata.ObjectClass.Retrieve(
                      DataSet.Fields[1].AsString, False, False, AObject.Connector);
                    try
                      if Assigned(PartObject) then
                        PartObject.ObjectStore.DisposeObject(PartObject, caIgnore);
                    finally
                      PartObject.Free;
                    end;
                  end;
                finally
                  DataSet.Close;
                end;
              finally
                Broker.ReleaseDataSet(DataSet);
              end;
            finally
              SelectParams.Free;
            end;

            // Store object
            PartObject := PartAttribute.Value;
            PartObject.CheckId;
            PartObject.ObjectStore.StoreObject(PartObject, caIgnore);
          end;
        end;
      end;
    end;
  end;

  procedure UpdateExternalPartsMap;
  var
    i, ii: Integer;
    PartObject: TInstantObject;
    PartsAttribute: TInstantParts;
    AttributeMetadata: TInstantAttributeMetadata;
    SelectParams, DeleteParams, InsertParams: TParams;
    SelectStatement, DeleteStatement, InsertStatement: string;
    DataSet: TDataSet;
  begin
    for i := 0 to Pred(Map.Count) do
    begin
      AttributeMetadata := Map[i];
      if AttributeMetadata.AttributeType = atParts then
      begin
        PartsAttribute := TInstantParts(AObject.AttributeByName(AttributeMetadata.Name));
        if PartsAttribute.IsChanged then
        begin
          if Map[i].StorageKind = skExternal then
          begin
            // Make sure that all the items are in memory because they will be
            // accessed later, after the database records have been deleted.
            for ii := 0 to Pred(PartsAttribute.Count) do
              PartsAttribute.Items[ii];
            // Delete all objects
            SelectParams := TParams.Create;
            try
              SelectStatement := Format(SelectExternalSQL, [AttributeMetadata.ExternalStorageName]);
              AddIdParam(SelectParams, InstantParentIdFieldName, AObject.Id);
              AddStringParam(SelectParams, InstantParentClassFieldName, AObject.ClassName);
              AddStringParam(SelectParams, InstantChildClassFieldName, AttributeMetadata.ObjectClassName);
              DataSet := Broker.AcquireDataSet(SelectStatement, SelectParams);
              try
                DataSet.Open;
                try
                  while not DataSet.Eof do
                  begin
                    PartObject := AttributeMetadata.ObjectClass.Retrieve(
                      DataSet.Fields[1].AsString, False, False, AObject.Connector);
                    try
                      if Assigned(PartObject) and
                          (PartsAttribute.IndexOf(PartObject) = -1) then
                        PartObject.ObjectStore.DisposeObject(PartObject, caIgnore);
                    finally
                      PartObject.Free;
                    end;
                    DataSet.Next;
                  end;
                finally
                  DataSet.Close;
                end;
              finally
                Broker.ReleaseDataSet(DataSet);
              end;
            finally
              SelectParams.Free;
            end;

            // Delete all links
            DeleteParams := TParams.Create;
            try
              DeleteStatement := Format(DeleteExternalSQL,
                [AttributeMetadata.ExternalStorageName,
                InstantParentClassFieldName]);
              AddStringParam(DeleteParams, InstantParentClassFieldName, AObject.ClassName);
              AddIdParam(DeleteParams, InstantParentIdFieldName, AObject.Id);
              Broker.Execute(DeleteStatement, DeleteParams);
            finally
              DeleteParams.Free;
            end;

            // Store all objects and links
            for ii := 0 to Pred(PartsAttribute.Count) do
            begin
              // Store object
              PartObject := PartsAttribute.Items[ii];
              PartObject.CheckId;
              PartObject.ObjectStore.StoreObject(PartObject, caIgnore);

              // Insert link
              InsertParams := TParams.Create;
              try
                InsertStatement := Format(InsertExternalSQL,
                  [AttributeMetadata.ExternalStorageName]);
                AddIdParam(InsertParams, InstantIdFieldName, AObject.GenerateId);
                AddStringParam(InsertParams, InstantParentClassFieldName, AObject.ClassName);
                AddIdParam(InsertParams, InstantParentIdFieldName, AObject.Id);
                AddStringParam(InsertParams, InstantChildClassFieldName,
                  PartsAttribute.Items[ii].ClassName);
                AddIdParam(InsertParams, InstantChildIdFieldName,
                  PartsAttribute.Items[ii].Id);
                AddIntegerParam(InsertParams, InstantSequenceNoFieldName, Succ(ii));
                Broker.Execute(InsertStatement, InsertParams);
              finally
                InsertParams.Free;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  procedure UpdateExternalReferencesMap;
  var
    i, ii: integer;
    AttributeMetadata: TInstantAttributeMetadata;
    ReferenceObject: TInstantObject;
    ReferencesAttribute: TInstantReferences;
    DeleteParams, InsertParams: TParams;
    DeleteStatement, InsertStatement: string;
  begin
    for i := 0 to Pred(Map.Count) do
    begin
      AttributeMetadata := Map[i];
      if AttributeMetadata.AttributeType = atReferences then
      begin
        ReferencesAttribute := TInstantReferences(AObject.AttributeByName(AttributeMetadata.Name));
        if ReferencesAttribute.IsChanged then
        begin
          if AttributeMetadata.StorageKind = skExternal then
          begin
            // Delete all links
            DeleteParams := TParams.Create;
            try
              DeleteStatement := Format(DeleteExternalSQL,
                [AttributeMetadata.ExternalStorageName,
                InstantParentClassFieldName,
                InstantParentIdFieldName]);
              AddStringParam(DeleteParams, InstantParentClassFieldName, AObject.ClassName);
              AddIdParam(DeleteParams, InstantParentIdFieldName, AObject.Id);
              Broker.Execute(DeleteStatement, DeleteParams);
            finally
              DeleteParams.Free;
            end;
            // Store all links
            for ii := 0 to Pred(ReferencesAttribute.Count) do
            begin
              ReferenceObject := ReferencesAttribute.Items[ii];
              if ReferenceObject.FInUpdate then     // prevent recursion
                Continue;
              ReferenceObject.CheckId;
              ReferenceObject.ObjectStore.StoreObject(ReferenceObject, caIgnore);

              InsertParams := TParams.Create;
              try
                InsertStatement := Format(InsertExternalSQL,
                  [AttributeMetadata.ExternalStorageName]);
                AddIdParam(InsertParams, InstantIdFieldName, AObject.GenerateId);
                AddStringParam(InsertParams, InstantParentClassFieldName, AObject.ClassName);
                AddIdParam(InsertParams, InstantParentIdFieldName, AObject.Id);
                AddStringParam(InsertParams, InstantChildClassFieldName,
                  ReferencesAttribute.Items[ii].ClassName);
                AddIdParam(InsertParams, InstantChildIdFieldName,
                  ReferencesAttribute.Items[ii].Id);
                AddIntegerParam(InsertParams, InstantSequenceNoFieldName, Succ(ii));
                Broker.Execute(InsertStatement, InsertParams);
              finally
                InsertParams.Free;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

begin
  if not Assigned(Info) then
    Info := @AInfo;
  Params := TParams.Create;
  try
    if Map.IsRootMap then
      if ConflictAction = caIgnore then
      begin
        Randomize;
        NewUpdateCount := Random(High(Integer)) + 1;
      end else if AObject.UpdateCount = High(Integer) then
        NewUpdateCount := 1
      else
        NewUpdateCount := AObject.UpdateCount + 1
    else
      NewUpdateCount := AObject.UpdateCount;
    AddBaseParams(Params, AObject.ClassName, AObject.Id, NewUpdateCount);
    AddAttributeParams(Params, AObject, Map);
    UpdateExternalPartMap;
    if AObject.IsPersistent then
      UpdateMap
    else
      InsertMap;
    if Map.IsRootMap then
      Broker.SetObjectUpdateCount(AObject, NewUpdateCount);

    UpdateExternalPartsMap;
    UpdateExternalReferencesMap;
  finally
    Params.Free;
  end;
end;

procedure TInstantSQLResolver.ReadAttribute(AObject: TInstantObject;
  const AObjectId: string; AttributeMetadata: TInstantAttributeMetadata;
  DataSet: TDataSet);
var
  Attribute: TInstantAttribute;
  AFieldName: string;

  procedure ReadBlobAttribute;
  begin
    (Attribute as TInstantBlob).Value := ReadBlobField(DataSet, AFieldName);
  end;

  procedure ReadBooleanAttribute;
  begin
    (Attribute as TInstantBoolean).Value :=
      ReadBooleanField(DataSet, AFieldName);
  end;

  procedure ReadDateTimeAttribute;
  begin
    (Attribute as TInstantDateTime).Value :=
      ReadDateTimeField(DataSet, AFieldName);
  end;

  procedure ReadFloatAttribute;
  begin
    (Attribute as TInstantFloat).Value := ReadFloatField(DataSet, AFieldName);
  end;

  procedure ReadCurrencyAttribute;
  begin
    (Attribute as TInstantCurrency).Value := ReadCurrencyField(DataSet, AFieldName);
  end;

  procedure ReadIntegerAttribute;
  begin
    (Attribute as TInstantInteger).Value :=
      ReadIntegerField(DataSet, AFieldName);
  end;

  procedure ReadMemoAttribute;
  begin
    (Attribute as TInstantMemo).Value := ReadMemoField(DataSet, AFieldName);
  end;

  procedure ReadPartAttribute;
  var
    Stream: TInstantStringStream;
    LPartClassName: string;
    LPartId: string;
  begin
    if AttributeMetadata.StorageKind = skExternal then
    begin
      with (Attribute as TInstantPart) do
      begin
        // Must clear Value first to avoid leak for Refresh operation
        // as OldValue = NewValue.
        Value := nil;
        LPartClassName := ReadStringField(DataSet, AFieldName +
          InstantClassFieldName);
        LPartId := ReadStringField(DataSet, AFieldName +
          InstantIdFieldName);
        // LPartClassName and LPartId will be empty if the attribute was
        // added to a class with existing instances in the database.
        if (LPartClassName = '') and (LPartId = '') then
           (Attribute as TInstantPart).Reset
        else
          Value := InstantFindClass(LPartClassName).Retrieve(LPartId,
            False, False, AObject.Connector);
      end;
    end
    else
    begin
      Stream := TInstantStringStream.Create(ReadBlobField(DataSet,
        AFieldName));
      try
        if Stream.Size = 0 then
          (Attribute as TInstantPart).Reset
        else
          (Attribute as TInstantPart).LoadObjectFromStream(Stream);
      finally
        Stream.Free;
      end;
    end;
  end;

  procedure ReadPartsAttribute;
  var
    //PartObject: TInstantObject;
    Statement: string;
    Params: TParams;
    Stream: TInstantStringStream;
    RefObject:TInstantObjectReference;
  begin
    if AttributeMetadata.StorageKind = skExternal then
    begin
      with (Attribute as TInstantParts) do
      begin
        Clear;
        Params := TParams.Create;
        try
          Statement := Format(SelectExternalSQL, [AttributeMetadata.ExternalStorageName]);
          AddIdParam(Params, InstantParentIdFieldName, AObjectId);
          AddStringParam(Params, InstantParentClassFieldName, AObject.ClassName);
          AddStringParam(Params, InstantChildClassFieldName, AttributeMetadata.ObjectClassName);
          DataSet := Broker.AcquireDataSet(Statement, Params);
          try
            DataSet.Open;
            try
              while not DataSet.Eof do
              begin
                RefObject := TInstantObjectReference.Create(nil, True);
                RefObject.ReferenceObject(DataSet.Fields[0].AsString, DataSet.Fields[1].AsString);
                (Attribute as TInstantParts).ObjectReferenceList.Add(RefObject);
                {PartObject := AttributeMetadata.ObjectClass.Retrieve(Fields[1].AsString, False, False, AObject.Connector);
                if Assigned(PartObject) then
                  Add(PartObject)
                else
                  PartObject.Free;}
                DataSet.Next;
              end;
            finally
              DataSet.Close;
            end;
          finally
            Broker.ReleaseDataSet(DataSet);
          end;
        finally
          Params.Free;
        end;
        //Changed;
      end;
    end
    else
    begin
      Stream := TInstantStringStream.Create(ReadBlobField(DataSet, AFieldName));
      try
        if Stream.Size = 0 then
          (Attribute as TInstantParts).Reset
        else
          (Attribute as TInstantParts).LoadObjectsFromStream(Stream);
      finally
        Stream.Free;
      end;
    end;
  end;

  procedure ReadReferenceAttribute;
  begin
    (Attribute as TInstantReference).ReferenceObject(
      ReadStringField(DataSet, AFieldName + InstantClassFieldName),
      ReadStringField(DataSet, AFieldName + InstantIdFieldName));
  end;

  procedure ReadReferencesAttribute;
  var
    RefObject:TInstantObjectReference;
    Stream: TInstantStringStream;
    Statement: string;
    Params: TParams;
  begin
    if AttributeMetadata.StorageKind = skExternal then
    begin
      with (Attribute as TInstantReferences) do
      begin
        Clear;
        Params := TParams.Create;
        try
          Statement:=Format(SelectExternalSQL, [AttributeMetadata.ExternalStorageName]);
          AddIdParam(Params, InstantParentIdFieldName, AObjectId);
          AddStringParam(Params, InstantParentClassFieldName, AObject.ClassName);
          AddStringParam(Params, InstantChildClassFieldName, AttributeMetadata.ObjectClassName);
          DataSet := Broker.AcquireDataSet(Statement, Params);
          try
            DataSet.Open;
            try
              while not DataSet.Eof do
              begin
                RefObject :=
                    (Attribute as TInstantReferences).ObjectReferenceList.Add;
                RefObject.ReferenceObject(Metadata.ObjectClass, DataSet.Fields[1].AsString);
                DataSet.Next;
              end;
            finally
              DataSet.Close;
            end;
          finally
            Broker.ReleaseDataSet(DataSet);
          end;
        finally
          Params.Free;
        end;
      end;
    end
    else
    begin
      Stream := TInstantStringStream.Create(ReadBlobField(DataSet, AFieldName));
      try
        if Stream.Size = 0 then
          (Attribute as TInstantReferences).Reset
        else
          (Attribute as TInstantReferences).LoadReferencesFromStream(Stream);
      finally
        Stream.Free;
      end;
    end;
  end;

  procedure ReadStringAttribute;
  begin
    (Attribute as TInstantString).Value :=
      ReadStringField(DataSet, AFieldName);
  end;

begin
  with AttributeMetadata do
  begin
    Attribute := AObject.AttributeByName(Name);
    AFieldName := FieldName;
    case AttributeType of
      atInteger:
        ReadIntegerAttribute;
      atFloat:
        ReadFloatAttribute;
      atCurrency:
        ReadCurrencyAttribute;
      atBoolean:
        ReadBooleanAttribute;
      atString:
        ReadStringAttribute;
      atDateTime:
        ReadDateTimeAttribute;
      atBlob, atGraphic:
        ReadBlobAttribute;
      atMemo:
        ReadMemoAttribute;
      atPart:
        ReadPartAttribute;
      atReference:
        ReadReferenceAttribute;
      atParts:
        ReadPartsAttribute;
      atReferences:
        ReadReferencesAttribute;
    end;
  end;
end;

procedure TInstantSQLResolver.ReadAttributes(AObject: TInstantObject;
  const AObjectId: string; Map: TInstantAttributeMap; DataSet: TDataSet);
var
  I: Integer;
begin
  if Assigned(AObject) and Assigned(Map) and Assigned(DataSet) then
    for I := 0 to Pred(Map.Count) do
      ReadAttribute(AObject, AObjectId, Map[I], DataSet);
end;

function TInstantSQLResolver.ReadBlobField(DataSet: TDataSet;
  const FieldName: string): string;
begin
  Result := DataSet.FieldByName(FieldName).AsString
end;

function TInstantSQLResolver.ReadBooleanField(DataSet: TDataSet;
  const FieldName: string): Boolean;
begin
  Result := DataSet.FieldByName(FieldName).AsBoolean;
end;

function TInstantSQLResolver.ReadDateTimeField(DataSet: TDataSet;
  const FieldName: string): TDateTime;
begin
  Result := DataSet.FieldByName(FieldName).AsDateTime;
end;

function TInstantSQLResolver.ReadFloatField(DataSet: TDataSet;
  const FieldName: string): Double;
begin
  Result := DataSet.FieldByName(FieldName).AsFloat;
end;

function TInstantSQLResolver.ReadCurrencyField(DataSet: TDataSet;
  const FieldName: string): Currency;
begin
  Result := DataSet.FieldByName(FieldName).AsCurrency;
end;

function TInstantSQLResolver.ReadIntegerField(DataSet: TDataSet;
  const FieldName: string): Integer;
begin
  Result := DataSet.FieldByName(FieldName).AsInteger;
end;

function TInstantSQLResolver.ReadMemoField(DataSet: TDataSet;
  const FieldName: string): string;
begin
  Result := TrimRight(DataSet.FieldByName(FieldName).AsString);
end;

function TInstantSQLResolver.ReadStringField(DataSet: TDataSet;
  const FieldName: string): string;
begin
  Result := TrimRight(DataSet.FieldByName(FieldName).AsString);
end;

procedure TInstantSQLResolver.RemoveConcurrencyParam(Params: TParams);
var
  Param: TParam;
begin
  Param := Params.FindParam(ConcurrencyParamName);
  if Assigned(Param) then
    Params.Delete(Param.Index);
end;

procedure TInstantSQLResolver.RemovePersistentIdParam(Params: TParams);
var
  Param: TParam;
begin
  Param := Params.FindParam(PersistentIdParamName);
  if Assigned(Param) then
    Params.Delete(Param.Index);
end;

function TInstantSQLResolver.TranslateError(AObject: TInstantObject;
  E: Exception): Exception;
begin
  Result := nil;
end;

{ TInstantSQLQuery }

function TInstantSQLQuery.AcquireDataSet(const AStatement: string;
  AParams: TParams): TDataSet;
begin
  Result := (Connector.Broker as TInstantSQLBroker).AcquireDataSet(AStatement,
    AParams);
end;

procedure TInstantSQLQuery.ReleaseDataSet(const DataSet: TDataSet);
begin
  (Connector.Broker as TInstantSQLBroker).ReleaseDataSet(DataSet);
end;

destructor TInstantSQLQuery.Destroy;
begin
  DestroyObjectReferenceList;
  FParamsObject.Free;
  inherited;
end;

procedure TInstantSQLQuery.DestroyObjectReferenceList;
begin
  FreeAndNil(FObjectReferenceList);
end;

function TInstantSQLQuery.GetActive: Boolean;
begin
  Result := Assigned(FObjectReferenceList);
end;

function TInstantSQLQuery.GetObjectReferenceCount: Integer;
begin
  Result := ObjectReferenceList.Count;
end;

function TInstantSQLQuery.GetObjectReferenceList: TInstantObjectReferenceList;
begin
  if not Assigned(FObjectReferenceList) then
    FObjectReferenceList :=
        TInstantObjectReferenceList.Create(True, Connector);
  Result := FObjectReferenceList;
end;

function TInstantSQLQuery.GetParams: TParams;
begin
  Result := ParamsObject;
end;

function TInstantSQLQuery.GetParamsObject: TParams;
begin
  if not Assigned(FParamsObject) then
    FParamsObject := TParams.Create;
  Result := FParamsObject;
end;

function TInstantSQLQuery.GetStatement: string;
begin
  Result := FStatement;
end;

procedure TInstantSQLQuery.InitObjectReferences(DataSet: TDataSet);
var
  ObjRef: TInstantObjectReference;
begin
  if Assigned(DataSet) then
  begin
    DataSet.DisableControls;
    try
      while not DataSet.Eof do
      begin
        ObjRef := ObjectReferenceList.Add;
        try
          ObjRef.ReferenceObject(
            DataSet.FieldByName(InstantClassFieldName).AsString,
            DataSet.FieldByName(InstantIdFieldName).AsString);
        except
          ObjRef.Free;
          raise;
        end;
        if (MaxCount > 0) and (ObjectReferenceList.Count = MaxCount) then break;
        DataSet.Next;
      end;
    finally
      DataSet.EnableControls;
    end;
  end;
end;

function TInstantSQLQuery.InternalAddObject(AObject: TObject): Integer;
begin
  Result := ObjectReferenceList.Add(AObject as TInstantObject);
end;

procedure TInstantSQLQuery.InternalClose;
begin
  DestroyObjectReferenceList;
  inherited;
end;

procedure TInstantSQLQuery.InternalGetInstantObjectRefs(List:
    TInstantObjectReferenceList);
var
  I: Integer;
begin
  for I := 0 to Pred(ObjectReferenceCount) do
    if ObjectReferenceList.RefItems[I].HasInstance then
      List.Add(ObjectReferenceList[I]);
end;

function TInstantSQLQuery.InternalGetObjectCount: Integer;
begin
  Result := ObjectReferenceCount;
end;

function TInstantSQLQuery.InternalGetObjects(Index: Integer): TObject;
begin
  Result := ObjectReferenceList[Index];
end;

function TInstantSQLQuery.InternalIndexOfObject(AObject: TObject): Integer;
begin
  if AObject is TInstantObject then
    Result := ObjectReferenceList.IndexOf(TInstantObject(AObject))
  else
    Result := -1;
end;

procedure TInstantSQLQuery.InternalInsertObject(Index: Integer;
  AObject: TObject);
begin
  ObjectReferenceList.Insert(Index, AObject as TInstantObject);
end;

procedure TInstantSQLQuery.InternalOpen;
var
  DataSet: TDataSet;
begin
  inherited;
  DataSet := AcquireDataSet(Statement, ParamsObject);
  if Assigned(DataSet) then
  try
    if not DataSet.Active then
      DataSet.Open;
    InitObjectReferences(DataSet);
  finally
    ReleaseDataSet(DataSet);
  end;
end;

procedure TInstantSQLQuery.InternalReleaseObject(AObject: TObject);
var
  Index: Integer;
begin
  Index := IndexOfObject(AObject);
  if Index <> -1 then
    ObjectReferenceList.RefItems[Index].DestroyInstance;
end;

function TInstantSQLQuery.InternalRemoveObject(AObject: TObject): Integer;
begin
  Result := ObjectReferenceList.Remove(AObject as TInstantObject);
end;

function TInstantSQLQuery.ObjectFetched(Index: Integer): Boolean;
begin
  Result := ObjectReferenceList.RefItems[Index].HasInstance;
end;

procedure TInstantSQLQuery.SetParams(Value: TParams);
begin
  ParamsObject.Assign(Value);
end;

procedure TInstantSQLQuery.SetStatement(const Value: string);
begin
  FStatement := Value;
end;

{ TInstantObjectNotifier }

constructor TInstantObjectNotifier.Create(AObjectClass: TInstantObjectClass;
  IncludeOwned: Boolean; AOwner: TInstantObjectNotifiers);
begin
  FObjectClass := AObjectClass;
  FIncludeOwned := IncludeOwned;
  FOwner := AOwner;
  Owner.Add(Self);
end;

destructor TInstantObjectNotifier.Destroy;
begin
  Owner.Extract(Self);
  inherited;
end;

procedure TInstantObjectNotifier.Disable;
begin
  Inc(FDisableCount);
end;

function TInstantObjectNotifier.Disabled: Boolean;
begin
  Result := FDisableCount <> 0;
end;

procedure TInstantObjectNotifier.DoObjectChanged(AObject: TInstantObject);
begin
  if Assigned(FOnObjectChanged) then
    FOnObjectChanged(AObject);
  ObjectChanged(AObject);
end;

procedure TInstantObjectNotifier.DoObjectCreated(AObject: TInstantObject);
begin
  if Assigned(FOnObjectCreated) then
    FOnObjectCreated(AObject);
  ObjectCreated(AObject);
end;

procedure TInstantObjectNotifier.DoObjectDisposed(AObject: TInstantObject);
begin
  if Assigned(FOnObjectDisposed) then
    FOnObjectDisposed(AObject);
  ObjectDisposed(AObject);
end;

procedure TInstantObjectNotifier.DoObjectRefreshed(AObject: TInstantObject);
begin
  if Assigned(FOnObjectRefreshed) then
    FOnObjectRefreshed(AObject);
  ObjectRefreshed(AObject);
end;

procedure TInstantObjectNotifier.DoObjectRetrieved(AObject: TInstantObject);
begin
  if Assigned(FOnObjectRetrieved) then
    FOnObjectRetrieved(AObject);
  ObjectRetrieved(AObject);
end;

procedure TInstantObjectNotifier.DoObjectStored(AObject: TInstantObject);
begin
  if Assigned(FOnObjectStored) then
    FOnObjectStored(AObject);
  ObjectStored(AObject);
end;

procedure TInstantObjectNotifier.Enable;
begin
  if Disabled then
    Dec(FDisableCount);
end;

function TInstantObjectNotifier.GetObjectClass: TInstantObjectClass;
begin
  if Assigned(FObjectClass) then
    Result := FObjectClass
  else
    Result := TInstantObject;
end;

function TInstantObjectNotifier.GetOwner: TInstantObjectNotifiers;
begin
  if Assigned(FOwner) then
    Result := FOwner
  else
    Result := ObjectNotifiers;
end;

function TInstantObjectNotifier.IncludeObject(AObject: TInstantObject): Boolean;
begin
  Result := not AObject.IsOwned or (AObject.IsOwned = IncludeOwned);
end;

procedure TInstantObjectNotifier.Notify(AObject: TInstantObject;
  Notification: TInstantObjectNotification);
begin
  if Disabled or not (AObject is ObjectClass) or not IncludeObject(AObject) then
    Exit;
  if Assigned(FOnNotify) then
    FOnNotify(AObject, Notification);
  case Notification of
    onCreated:
      DoObjectCreated(AObject);
    onChanged:
      DoObjectChanged(AObject);
    onDisposed:
      DoObjectDisposed(AObject);
    onRefreshed:
      DoObjectRefreshed(AObject);
    onRetrieved:
      DoObjectRetrieved(AObject);
    onStored:
      DoObjectStored(AObject);
  end;
end;

procedure TInstantObjectNotifier.ObjectChanged(AObject: TInstantObject);
begin
end;

procedure TInstantObjectNotifier.ObjectCreated(AObject: TInstantObject);
begin
end;

procedure TInstantObjectNotifier.ObjectDisposed(AObject: TInstantObject);
begin
end;

procedure TInstantObjectNotifier.ObjectRefreshed(AObject: TInstantObject);
begin
end;

procedure TInstantObjectNotifier.ObjectRetrieved(AObject: TInstantObject);
begin
end;

procedure TInstantObjectNotifier.ObjectStored(AObject: TInstantObject);
begin
end;

{ TInstantObjectNotifiers }

function TInstantObjectNotifiers.AcceptItem(
  Item: TInstantObjectNotifier): Boolean;
begin
  Result := True;
end;

function TInstantObjectNotifiers.Add(
  Item: TInstantObjectNotifier): Integer;
begin
  if AcceptItem(Item) then
    Result := ItemList.Add(Item)
  else
    Result := -1;
end;

procedure TInstantObjectNotifiers.BeginNotify;
begin
  Inc(FNotifyingCount);
end;

constructor TInstantObjectNotifiers.Create(AObjectClass: TInstantObjectClass);
begin
  inherited Create;
  FObjectClass := AObjectClass;
end;

destructor TInstantObjectNotifiers.Destroy;
begin
  FItemList.Free;
  inherited;
end;

procedure TInstantObjectNotifiers.Disable;
begin
  Inc(FDisableCount);
end;

function TInstantObjectNotifiers.Disabled: Boolean;
begin
  Result := FDisableCount <> 0;
end;

procedure TInstantObjectNotifiers.Enable;
begin
  if Disabled then
    Dec(FDisableCount);
end;

procedure TInstantObjectNotifiers.EndNotify;
begin
  if IsNotifying then
  begin
    Dec(FNotifyingCount);
    if not IsNotifying then
      ItemList.Pack;
  end;
end;

procedure TInstantObjectNotifiers.Extract(Item: TInstantObjectNotifier);
var
  Index: Integer;
begin
  if IsNotifying then
  begin
    Index := ItemList.IndexOf(Item);
    if Index <> -1 then
      ItemList.Insert(Index, nil);
  end;
  ItemList.Extract(Item);
end;

function TInstantObjectNotifiers.GetCount: Integer;
begin
  Result := ItemList.Count;
end;

function TInstantObjectNotifiers.GetIsNotifying: Boolean;
begin
  Result := FNotifyingCount <> 0;
end;

function TInstantObjectNotifiers.GetItemList: TList;
begin
  if not Assigned(FItemList) then
    FItemList := TObjectList.Create;
  Result := FItemList;
end;

function TInstantObjectNotifiers.GetItems(
  Index: Integer): TInstantObjectNotifier;
begin
  Result := ItemList[Index];
end;

function TInstantObjectNotifiers.GetObjectClass: TInstantObjectClass;
begin
  if Assigned(FObjectClass) then
    Result := FObjectClass
  else
    Result := TInstantObject;
end;

procedure TInstantObjectNotifiers.Notify(AObject: TInstantObject;
  Notification: TInstantObjectNotification);
var
  I: Integer;
begin
  if Disabled or not (AObject is ObjectClass) then
    Exit;
  BeginNotify;
  try
    for I := 0 to Pred(Count) do
      if Assigned(Items[I]) then
        Items[I].Notify(AObject, Notification);
  finally
    EndNotify;
  end;
end;

function TInstantObjectNotifiers.Remove(Item: TInstantObjectNotifier): Integer;
begin
  if IsNotifying then
  begin
    Result := ItemList.IndexOf(Item);
    if Result <> -1 then
      ItemList[Result] := nil;
  end else
    Result := ItemList.Remove(Item);
end;

{ TInstantProgressLoop }

constructor TInstantProgressLoop.Create(ALow, AHigh: Integer;
  ProgressEvent: TInstantProgressEvent);
begin
  FLow := ALow;
  FHigh := AHigh;
  FStep := FLow;
  FOnProgress := ProgressEvent;
end;

function TInstantProgressLoop.Done: Boolean;
begin
  Result := FStep > FHigh;
end;

procedure TInstantProgressLoop.Next;
begin
  Inc(FStep);
end;

procedure TInstantProgressLoop.Progress(Sender: TObject; Count: Integer;
  var Continue: Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Count div (FHigh - FStep + 1), Continue);
end;


procedure LoadClassMetadatas;

  function HasModelResource(AInstance: Cardinal): Boolean;
  begin
    Result := (AInstance <> 0) and (FindResource(AInstance,
      PChar(InstantModelResourceName), RT_RCDATA) <> 0);
  end;

  procedure LoadModelFromResource(AInstance: Cardinal);
  begin
    with TInstantResourceStream.Create(AInstance, InstantModelResourceName,
      RT_RCDATA) do
    try
      ReadObject(InstantModel.ClassMetadatas);
    finally
      Free;
    end;
  end;

var
  Instance: Cardinal;
{$IFNDEF FPC}
  LibModule: PLibModule;
{$ENDIF}
begin
  if HasModelResource(HInstance)then
    LoadModelFromResource(HInstance)
  else if HasModelResource(MainInstance) then
    LoadModelFromResource(MainInstance)
  else begin
{$IFNDEF FPC}
    LibModule := LibModuleList;
    while LibModule <> nil do
    begin
      Instance := LibModule.Instance;
      if (Instance <> HInstance) and (Instance <> MainInstance) and
        HasModelResource(Instance) then
      begin
        LoadModelFromResource(Instance);
        Exit;
      end;
      LibModule := LibModule.Next;
    end;
{$ENDIF}
  end;
end;

{ TInstantGraphic }

class function TInstantGraphic.AttributeType: TInstantAttributeType;
begin
  Result := atGraphic;
end;

{ TInstantStatementCache }

constructor TInstantStatementCache.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatements := TStringList.Create;
  FStatements.Sorted := True;
  FStatements.Duplicates := dupError;
  FCapacity := 0;
end;

destructor TInstantStatementCache.Destroy;
begin
  DeleteAllStatements;
  FStatements.Free;
  inherited;
end;

function TInstantStatementCache.AddStatement(const StatementText: string;
  const StatementImplementation: TComponent): Integer;
var
  StatementObject: TInstantStatement;
begin
  if Assigned(StatementImplementation) then
  begin
    Shrink;
    StatementObject := TInstantStatement.Create(StatementImplementation);
    Result := FStatements.AddObject(StatementText, StatementObject);
    StatementImplementation.FreeNotification(Self);
  end
  else
    Result := -1;
end;

function TInstantStatementCache.RemoveStatement(const StatementText: string): Boolean;
var
  Index: Integer;
begin
  Index := FStatements.IndexOf(StatementText);
  if Index >= 0 then
  begin
    DeleteStatement(Index);
    Result := True;
  end
  else
    Result := False;
end;

function TInstantStatementCache.GetStatement(const StatementText: string): TInstantStatement;
var
  Index: Integer;
begin
  Index := FStatements.IndexOf(StatementText);
  if Index >= 0 then
    Result := TInstantStatement(FStatements.Objects[Index])
  else
    Result := nil;
end;

procedure TInstantStatementCache.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  if Operation = opRemove then
    for I := FStatements.Count - 1 downto 0 do
      if TInstantStatement(FStatements.Objects[I]).StatementImplementation = AComponent then
        DeleteStatement(I);
end;

procedure TInstantStatementCache.DeleteStatement(const Index: Integer);
var
  AObject: TObject;
begin
  // Avoid looping with Notification.
  AObject := FStatements.Objects[Index];
  FStatements.Delete(Index);
  AObject.Free;
end;

procedure TInstantStatementCache.DeleteAllStatements;
var
  I: Integer;
begin
  for I := FStatements.Count - 1 downto 0 do
    DeleteStatement(I);
end;

procedure TInstantStatementCache.SetCapacity(const Value: Integer);
begin
  FCapacity := Value;
  Shrink;
end;

procedure TInstantStatementCache.Shrink;
begin
  // TODO : implement a shrink policy here when the cache can be limited in size.
  // currently the cache is always unlimited if enabled (Capacity <> 0).
end;

{ TInstantStatement }

constructor TInstantStatement.Create(const AStatementImplementation: TComponent);
begin
  inherited Create;
  FStatementImplementation := AStatementImplementation;
end;

destructor TInstantStatement.Destroy;
begin
  FStatementImplementation.Free;
  inherited;
end;

{ TInstantDBBuildCommand }

constructor TInstantDBBuildCommand.Create(
  const ACommandType: TInstantDBBuildCommandType;
  const AConnector: TInstantConnector = nil);
const
  // Default values for the Enabled property
  // depending on the CommandType.
  InstantDBBuildCommandEnabledDefaults: array[TInstantDBBuildCommandType] of Boolean =
    (True, False, True, True, True, True, True, False);
begin
  inherited Create;
  FCommandType := ACommandType;
  FConnector := AConnector;
  FEnabled := InstantDBBuildCommandEnabledDefaults[FCommandType];
end;

destructor TInstantDBBuildCommand.Destroy;
begin
  inherited;
end;

procedure TInstantDBBuildCommand.Execute;
begin
  try
    InternalExecute;
  except
    on E: Exception do
      if not InternalExecuteHandleError(E) then
        raise;
  end;
end;

function TInstantDBBuildCommand.GetConnector: TInstantConnector;
begin
  if Assigned(FConnector) then
    Result := FConnector
  else
    Result := InstantDefaultConnector;
end;

function TInstantDBBuildCommand.GetDescription: string;
begin
  Result := ClassName + ' [' + GetEnumName(TypeInfo(TInstantDBBuildCommandType), Ord(FCommandType));
  if Assigned(OldMetadata) then
    Result := Result + ', ' + OldMetadata.Name;
  if Assigned(NewMetadata) then
    Result := Result + ', ' + NewMetadata.Name;
  Result := Result + ']';
end;

function TInstantDBBuildCommand.InternalExecuteHandleError(
  const E: Exception): Boolean;
begin
  Result := False;
end;

{ TInstantBrokerCatalog }

constructor TInstantBrokerCatalog.Create(const AScheme: TInstantScheme;
  const ABroker: TInstantBroker);
begin
  inherited Create(AScheme);
  FBroker := ABroker;
end;

function TInstantBrokerCatalog.GetBroker: TInstantBroker;
begin
  if not Assigned(FBroker) then
    raise EInstantError.Create(SUnassignedBroker);
  Result := FBroker;
end;

{ TInstantModelCatalog }

constructor TInstantModelCatalog.Create(const AScheme: TInstantScheme;
  const AModel: TInstantModel);
begin
  inherited Create(AScheme);
  FModel := AModel;
end;

procedure TInstantModelCatalog.InitTableMetadatas(ATableMetadatas:
  TInstantTableMetadatas);
var
  Maps: TInstantAttributeMaps;
  I: Integer;

  procedure AddMap(Map: TInstantAttributeMap);
  var
    I: Integer;
    TableMetadata: TInstantTableMetadata;
    AttributeMetadata: TInstantAttributeMetadata;
    Options: TInstantFieldOptions;

    // Adds a table metadata definition matching AttributeMetadata, which
    // must have StorageKind = skEmbedded.
    procedure AddExternalTableMetadata;
    var
      TableMetadata: TInstantTableMetadata;
    begin
      TableMetadata := ATableMetadatas.Add;
      with TableMetadata do
      begin
        Name := AttributeMetadata.ExternalStorageName;
        // The structure of an external table is fixed.
        FieldMetadatas.AddFieldMetadata(InstantIdFieldName, Scheme.IdDataType,
          Scheme.IdSize, [foRequired, foIndexed]);
        FieldMetadatas.AddFieldMetadata(InstantParentClassFieldName, dtString,
          InstantDefaultFieldSize);
        FieldMetadatas.AddFieldMetadata(InstantParentIdFieldName, Scheme.IdDataType,
          Scheme.IdSize);
        FieldMetadatas.AddFieldMetadata(InstantChildClassFieldName, dtString,
          InstantDefaultFieldSize);
        FieldMetadatas.AddFieldMetadata(InstantChildIdFieldName, Scheme.IdDataType,
          Scheme.IdSize);
        FieldMetadatas.AddFieldMetadata(InstantSequenceNoFieldName, dtInteger,
          InstantDefaultFieldSize);
        IndexMetadatas.AddIndexMetadata('', InstantIdFieldName,
          [ixPrimary, ixUnique]);
      end;
    end;

  begin
    TableMetadata := ATableMetadatas.Add;
    with TableMetadata do
    begin
      Name := Map.Name;

      // Class + Id + UpdateCount.
      FieldMetadatas.AddFieldMetadata(InstantClassFieldName, dtString,
        InstantDefaultFieldSize, [foRequired, foIndexed]);
      FieldMetadatas.AddFieldMetadata(InstantIdFieldName, Scheme.IdDataType,
        Scheme.IdSize, [foRequired, foIndexed]);
      FieldMetadatas.AddFieldMetadata(InstantUpdateCountFieldName, dtInteger, 0);
      IndexMetadatas.AddIndexMetadata('', InstantIndexFieldNames,
        [ixPrimary, ixUnique]);

      // Other.
      for I := 0 to Pred(Map.Count) do
      begin
        Options := [];
        AttributeMetadata := Map[I];
        if AttributeMetadata.AttributeType = atReference then
        begin
          FieldMetadatas.AddFieldMetadata(AttributeMetadata.FieldName + InstantClassFieldName,
            Scheme.AttributeTypeToDataType(atString), InstantDefaultFieldSize);
          FieldMetadatas.AddFieldMetadata(AttributeMetadata.FieldName + InstantIdFieldName,
            Scheme.IdDataType, Scheme.IdSize);
        end
        else if AttributeMetadata.AttributeType = atPart then
        begin
          if AttributeMetadata.StorageKind = skEmbedded then
            FieldMetadatas.AddFieldMetadata(AttributeMetadata.FieldName,
              Scheme.AttributeTypeToDataType(AttributeMetadata.AttributeType),
              AttributeMetadata.Size)
          else if AttributeMetadata.StorageKind = skExternal then
          begin
            FieldMetadatas.AddFieldMetadata(AttributeMetadata.FieldName + InstantClassFieldName,
              Scheme.AttributeTypeToDataType(atString), InstantDefaultFieldSize);
            FieldMetadatas.AddFieldMetadata(AttributeMetadata.FieldName + InstantIdFieldName,
              Scheme.IdDataType, Scheme.IdSize);
          end;
        end
        else if AttributeMetadata.AttributeType in [atParts, atReferences] then
        begin
          if AttributeMetadata.StorageKind = skEmbedded then
            FieldMetadatas.AddFieldMetadata(AttributeMetadata.FieldName,
              Scheme.AttributeTypeToDataType(AttributeMetadata.AttributeType),
              AttributeMetadata.Size)
          else if AttributeMetadata.StorageKind = skExternal then
            AddExternalTableMetadata;
        end
        else
        begin
          if AttributeMetadata.IsIndexed then
          begin
            IndexMetadatas.AddIndexMetadata(Map.Name +
              AttributeMetadata.FieldName, AttributeMetadata.FieldName, []);
            Options := Options + [foIndexed];
          end
          else if AttributeMetadata.IsRequired then
          begin
            Options := Options + [foRequired];
          end;
          FieldMetadatas.AddFieldMetadata(AttributeMetadata.FieldName,
            Scheme.AttributeTypeToDataType(AttributeMetadata.AttributeType),
            AttributeMetadata.Size, Options);
        end;
      end;
    end;
  end;

begin
  Maps := InstantCreateStorageMaps(Model.ClassMetadatas);
  try
    if Assigned(Maps) then
    begin
      for I := 0 to Pred(Maps.Count) do
        AddMap(Maps[I]);
    end;
  finally
    Maps.Free;
  end;
end;

{ TInstantUnsupportedDBBuildCommand }

procedure TInstantUnsupportedDBBuildCommand.InternalExecute;
begin
  raise EInstantDBBuildError.CreateFmt(SCannotBuildDB, [Description]);
end;

{ TInstantCatalog }

constructor TInstantCatalog.Create(const AScheme: TInstantScheme);
begin
  inherited Create;
  FScheme := AScheme;
end;

procedure TInstantCatalog.DoWarning(const WarningText: string);
begin
  if Assigned(FOnWarning) then
    FOnWarning(Self, WarningText);
end;

function TInstantCatalog.GetFeatures: TInstantCatalogFeatures;
begin
  Result := [cfReadTableInfo, cfReadColumnInfo, cfReadIndexInfo];
end;

{ TInstantSQLBrokerCatalog }

function TInstantSQLBrokerCatalog.GetBroker: TInstantSQLBroker;
begin
  Result := inherited Broker as TInstantSQLBroker;
end;

{ TObjectReferenceList }

constructor TInstantObjectReferenceList.Create(ARefOwnsInstance: Boolean; AConnector:
    TInstantConnector; ARefOwner: TInstantComplex);
begin
  inherited Create;
  FList := TObjectList.Create;
  FRefOwnsInstance := ARefOwnsInstance;
  FRefOwner := ARefOwner;
  FConnector := AConnector;
end;

destructor TInstantObjectReferenceList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TInstantObjectReferenceList.Add: TInstantObjectReference;
begin
  Result := CreateObjectReference(nil);
  try
    FList.Add(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TInstantObjectReferenceList.Add(Item: TInstantObject): Integer;
var
  Ref: TInstantObjectReference;
begin
  Ref := CreateObjectReference(Item);
  try
    Result := FList.Add(Ref);
  except
    Ref.Free;
    raise;
  end;
end;

procedure TInstantObjectReferenceList.Clear;
begin
  FList.Clear;
end;

function TInstantObjectReferenceList.CreateObjectReference(AObject: TInstantObject):
    TInstantObjectReference;
begin
  Result := TInstantObjectReference.Create(nil, FRefOwnsInstance, FRefOwner);
  if Assigned(AObject) then
  begin
    if FRefOwnsInstance then
      Result.Instance := AObject
    else
      Result.ReferenceObject(AObject.ClassName, AObject.Id);
  end;
end;

procedure TInstantObjectReferenceList.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

procedure TInstantObjectReferenceList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TInstantObjectReferenceList.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

function TInstantObjectReferenceList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TInstantObjectReferenceList.GetItems(Index: Integer): TInstantObject;
var
  Ref: TInstantObjectReference;
begin
  Ref := FList.Items[Index] as TInstantObjectReference;
  Result := Ref.Dereference(FConnector, FRefOwnsInstance);
end;

function TInstantObjectReferenceList.GetRefItems(Index: Integer):
    TInstantObjectReference;
begin
  Result := FList.Items[Index] as TInstantObjectReference;
end;

function TInstantObjectReferenceList.IndexOf(Item: TInstantObject;
  NeedInstance: Boolean = False): Integer;
var
  Ref: TInstantObjectReference;
begin
  for Result := 0 to Pred(Count) do
  begin
    Ref := RefItems[Result];
    if Ref.Equals(Item) and (not NeedInstance or Ref.HasInstance) then
      Exit;
  end;
  Result := -1;
end;

function TInstantObjectReferenceList.IndexOfInstance(Item: TInstantObject): Integer;
begin
  Result := IndexOf(Item, True);
end;

procedure TInstantObjectReferenceList.Insert(Index: Integer; Item: TInstantObject);
var
  Ref: TInstantObjectReference;
begin
  Ref := CreateObjectReference(Item);
  try
    FList.Insert(Index, Ref);
  except
    Ref.Free;
    raise;
  end;
end;

procedure TInstantObjectReferenceList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

function TInstantObjectReferenceList.Remove(Item: TInstantObject): Integer;
begin
  Result := IndexOf(Item);
  if Result > -1 then
    FList.Delete(Result);
end;

procedure TInstantObjectReferenceList.SetCapacity(Value: Integer);
begin
  FList.Capacity := Value;
end;

procedure TInstantObjectReferenceList.SetItems(Index: Integer; const Value:
    TInstantObject);
var
  Ref: TInstantObjectReference;
begin
  Ref := CreateObjectReference(Value);
  try
    FList.Items[Index] := Ref;
  except
    Ref.Free;
    raise;
  end;
end;

initialization
  RegisterClasses([TInstantClassMetadatas, TInstantClassMetadata,
    TInstantAttributeMetadatas, TInstantAttributeMetadata,
    TInstantObjectReference, TInstantConnectionDefs, TInstantConnectionDef]);
  ClassList := TList.Create;
{$IFDEF MSWINDOWS}
  GraphicClassList[gffBmp] := Graphics.TBitmap;
  {$IFNDEF FPC}
    GraphicClassList[gffEmf] := Graphics.TMetaFile;
  {$ENDIF}
{$ENDIF}
{$IFDEF LINUX}
  GraphicClassList[gffBmp] := QGraphics.TBitmap;
  GraphicClassList[gffPng] := QGraphics.TBitmap;
  GraphicClassList[gffJpeg]:= QGraphics.TBitmap;
{$ENDIF}
  ConnectorClasses := TList.Create;
  LoadClassMetadatas;
  ObjectNotifiers := TInstantObjectNotifiers.Create;

finalization
  ClassList.Free;
  ConnectorClasses.Free;
  RuntimeModel.Free;
  ObjectNotifiers.Free;

end.
