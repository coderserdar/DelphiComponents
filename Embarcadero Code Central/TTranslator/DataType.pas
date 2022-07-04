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

{ $Id: DataType.pas,v 1.115 2003/03/31 09:11:31 mvj Exp $}

{ -------------------------------------------------------------------------
  DataType         Polycons DataElements

  What             TDataType (with descendents)

  Company          Polycon
  Authors          LGE
-------------------------------------------------------------------------}

unit DataType;

interface
{$i common.inc}

uses
  SysUtils, Classes;

type

  TDataType = class;
  TDataTypeClass = class of TDataType;
  TDataFormatter = class;
  TFieldWrapper = class;

  Byte8 = array[0..7] of Byte;

  TValue = record
    DataType : TDataType;
    CommonData : Byte8;
    sForInternalUseOnly : String;
  end;

  TDataTypeFactory = class
  private
    FDataTypes : TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterDataType(AClass : TDataTypeClass);
    procedure PropertiesForType(AType : TDataType; AProperties : TStrings);

    function ClassByName(AName : String) : TDataTypeClass;
    function DataTypeByName(AName : String; AProperties : TStrings = nil) : TDataType;
    function DataTypeByClass(AClass : TDataTypeClass; AProperties : TStrings = nil) : TDataType;
  end;

  TSetSpecialValueProc = procedure(Index : Integer; Value : String) of object;
  TGetSpecialValueProc = procedure(Index : Integer; out Value : String) of object;



  TDataType = class(TPersistent)
  protected
    FDefaultTransferValueAsParam : Boolean;
    FAllowsNullValue : Boolean;
    FNullValue : TValue;

    constructor Create(DefaultTransferValueAsParam : Boolean);

    function GetAsString(Value : TValue) : String; virtual; abstract;
    function GetAsBoolean(Value : TValue) : Boolean; virtual;
    function GetAsDateTime(Value : TValue) : TDateTime; virtual; abstract;
    function GetAsDate(Value : TValue) : TDateTime; virtual;
    function GetAsTime(Value : TValue) : TDateTime; virtual;
    function GetAsPointer(Value : TValue) : Pointer; virtual; abstract;
    function GetAsObject(Value : TValue) : TObject; virtual;
    function GetAsDouble(Value : TValue) : Double; virtual; abstract;
    function GetAsInteger(Value : TValue) : Integer; virtual;
    function GetAsCurrency(Value : TValue) : Currency; virtual;
    function GetAsPercent(Value : TValue) : Double; virtual;
    function MakeValueNative(Value : TValue) : TValue; virtual; abstract;

    function GetSQLDataTypeString : String; virtual; abstract;
    function GetDataSize : Integer; virtual; abstract;
    function GetDisplayWidth : Integer; virtual; abstract;

    function GetCanBeInDB : Boolean; virtual;
    function GetIsNumeric : Boolean; virtual;
    function GetIsNegative(AValue : TValue) : Boolean; virtual;
    function GetDefaultDecimalCount : Integer; virtual;
    function GetDefaultAggregable : Boolean; virtual;
    function GetAlignment : TAlignment; virtual;
    function GetDataTypeName : String; virtual; abstract;
    function GetDynamicSize: Boolean;  virtual;

    procedure GetDataTypeProperties(AProperties : TStrings); virtual;
    class function GetDataTypeWithProperties(AProperties : TStrings) : TDataType; virtual; abstract;
  public
    function ValueInInterval(AValue, IntervalFrom, IntervalTo : TValue) : Boolean;

    function IsNullValue(AValue : TValue) : Boolean;
    property AllowsNullValue : Boolean read FAllowsNullValue;
    property NullValue : TValue read FNullValue;

    function TransferValueAsParam(Value : TValue) : Boolean; virtual;
    function Compare(Value1, Value2 : TValue) : Integer; virtual; abstract;
    function Max(Value1, Value2 : TValue) : TValue;
    function Min(Value1, Value2 : TValue) : TValue;
    function Equals(Value1, Value2 : TValue) : Boolean;
    function EqualsMatchCase(Value1, Value2 : TValue) : Boolean; virtual;
    function Sum(Value1, Value2 : TValue) : TValue; virtual; abstract;
    function Difference(Value1, Value2 : TValue) : TValue; virtual; abstract;
    function Product(Value1, Value2 : TValue) : TValue; virtual; abstract;
    function Quota(Value1, Value2 : TValue) : TValue; virtual; abstract;
    function AndOperator(Value1, Value2 : TValue) : TValue; virtual;
    function OrOperator(Value1, Value2 : TValue) : TValue; virtual;
    function NegateValue(Value : TValue) : TValue; virtual; abstract;
    function ValueBlank(Value : TValue) : Boolean; virtual;
    function DefaultValue : TValue; virtual; abstract;
    function AsSQL(Value : TValue) : String; virtual; abstract;

    function Optimize(Value : TValue) : TValue;

    function FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue; virtual; abstract;
    function ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean; virtual; abstract;

    property DataSize : Integer read GetDataSize;
    property DisplayWidth : Integer read GetDisplayWidth;
    property SQLDataTypeString : String read GetSQLDataTypeString;
    property CanBeInDB : Boolean read GetCanBeInDB;
    property IsNumeric : Boolean read GetIsNumeric;
    property IsNegative[AValue : TValue] : Boolean read GetIsNegative;
    property DefaultAggregable : Boolean read GetDefaultAggregable;
    property DefaultDecimalCount : Integer read GetDefaultDecimalCount;
    property Alignment : TAlignment read GetAlignment;
    property DataTypeName : String read GetDataTypeName;
    property DynamicSize : Boolean read GetDynamicSize;

    procedure StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer); virtual; abstract;
    function FetchValue(Address : Pointer; GetSpecialValue : TGetSpecialValueProc; SpecialValueIndex : Integer) : TValue; virtual;
    destructor Destroy; override;

    function GenerateError(InvalidValue : String) : String; virtual;
  end;



  TUndefinedType = class(TDataType)
  private
    FDynamicSize : Boolean;
    FSize : Integer;
  protected
    constructor Create(Size : Integer; DynamicSize : Boolean);

    procedure Error;

    function GetAsString(Value : TValue) : String; override;
    function GetAsDateTime(Value : TValue) : TDateTime; override;
    function GetAsDate(Value : TValue) : TDateTime; override;
    function GetAsTime(Value : TValue) : TDateTime; override;
    function GetAsPointer(Value : TValue) : Pointer; override;
    function GetAsDouble(Value : TValue) : Double; override;
    function GetAsInteger(Value : TValue) : Integer; override;
    function GetAsCurrency(Value : TValue) : Currency; override;
    function GetAsPercent(Value : TValue) : Double; override;
    function MakeValueNative(Value : TValue) : TValue; override;

    function GetSQLDataTypeString : String; override;
    function GetDataSize : Integer; override;
    function GetDisplayWidth : Integer; override;
    function GetDataTypeName : String; override;
    function GetDynamicSize: Boolean;  override;

    procedure GetDataTypeProperties(AProperties : TStrings); override;
    class function GetDataTypeWithProperties(AProperties : TStrings) : TDataType; override;
  public
    function TransferValueAsParam(Value : TValue) : Boolean; override;
    function EqualsMatchCase(Value1, Value2 : TValue) : Boolean; override;
    function Compare(Value1, Value2 : TValue) : Integer; override;
    function DefaultValue : TValue; override;
    function AsSQL(Value : TValue) : String; override;
    function Value(Val : String) : TValue; virtual;
    function ValueBlank(Value : TValue) : Boolean; override;

    function Sum(Value1, Value2 : TValue) : TValue; override;
    function Difference(Value1, Value2 : TValue) : TValue; override;
    function Product(Value1, Value2 : TValue) : TValue; override;
    function Quota(Value1, Value2 : TValue) : TValue; override;
    function NegateValue(Value : TValue) : TValue; override;

    function FetchValue(Address : Pointer; GetSpecialValue : TGetSpecialValueProc; SpecialValueIndex : Integer) : TValue; override;
    procedure StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer); override;


    function FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue; override;
    function ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean; override;
  end;

  TStringType = class(TDataType)
  private
    FSize : Integer;
    FCaseSensitive : Boolean;
    function GetSizeStorageSize : Byte;
    function GetMaxSize : Integer;
  protected
    constructor Create(Size : Integer; CaseSensitive : Boolean);

    function GetAsString(Value : TValue) : String; override;
    function GetAsDateTime(Value : TValue) : TDateTime; override;
    function GetAsDate(Value : TValue) : TDateTime; override;
    function GetAsTime(Value : TValue) : TDateTime; override;
    function GetAsPointer(Value : TValue) : Pointer; override;
    function GetAsDouble(Value : TValue) : Double; override;
    function GetAsInteger(Value : TValue) : Integer; override;
    function GetAsCurrency(Value : TValue) : Currency; override;
    function GetAsPercent(Value : TValue) : Double; override;
    function MakeValueNative(Value : TValue) : TValue; override;

    function GetSQLDataTypeString : String; override;
    function GetDataSize : Integer; override;
    function GetDisplayWidth : Integer; override;
    function GetDataTypeName : String; override;

    procedure GetDataTypeProperties(AProperties : TStrings); override;
    class function GetDataTypeWithProperties(AProperties : TStrings) : TDataType; override;
  public
    class function MaxLength : Integer; virtual;
    property Size : Integer read FSize;
    property CaseSensitive : Boolean read FCaseSensitive;

    function TransferValueAsParam(Value : TValue) : Boolean; override;
    function EqualsMatchCase(Value1, Value2 : TValue) : Boolean; override;
    function Compare(Value1, Value2 : TValue) : Integer; override;
    function DefaultValue : TValue; override;
    function AsSQL(Value : TValue) : String; override;
    function Value(Val : String) : TValue; virtual;
    function ValueBlank(Value : TValue) : Boolean; override;

    function Sum(Value1, Value2 : TValue) : TValue; override;
    function Difference(Value1, Value2 : TValue) : TValue; override;
    function Product(Value1, Value2 : TValue) : TValue; override;
    function Quota(Value1, Value2 : TValue) : TValue; override;
    function NegateValue(Value : TValue) : TValue; override;

    function FetchValue(Address : Pointer; GetSpecialValue : TGetSpecialValueProc; SpecialValueIndex : Integer) : TValue; override;
    procedure StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer); override;


    function FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue; override;
    function ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean; override;
  end;

  TMemoType = class(TStringType)
  protected
    constructor Create(CaseSensitive : Boolean = False);
    function GetSQLDataTypeString : String; override;
    function GetDataSize : Integer; override;
    function GetDisplayWidth : Integer; override;
    function GetDataTypeName : String; override;
    function GetDynamicSize: Boolean;  override;

    procedure GetDataTypeProperties(AProperties : TStrings); override;
    class function GetDataTypeWithProperties(AProperties : TStrings) : TDataType; override;
  public
    class function MaxLength : Integer; override;
    function TransferValueAsParam(Value : TValue) : Boolean; override;
    function AsSQL(Value : TValue) : String; override;
    function Value(Val : String) : TValue; override;
    function FetchValue(Address : Pointer; GetSpecialValue : TGetSpecialValueProc; SpecialValueIndex : Integer) : TValue; override;
    procedure StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer); override;

  end;

  TBooleanType = class(TDataType)
  private
  protected
    constructor Create;

    function GetAsString(Value : TValue) : String; override;
    function GetAsBoolean(Value : TValue) : Boolean; override;
    function GetAsDateTime(Value : TValue) : TDateTime; override;
    function GetAsPointer(Value : TValue) : Pointer; override;
    function GetAsDouble(Value : TValue) : Double; override;
    function MakeValueNative(Value : TValue) : TValue; override;

    function GetSQLDataTypeString : String; override;
    function GetDataSize : Integer; override;
    function GetDisplayWidth : Integer; override;
    function GetDataTypeName : String; override;

    class function GetDataTypeWithProperties(AProperties : TStrings) : TDataType; override;
  public
    function Compare(Value1, Value2 : TValue) : Integer; override;
    function DefaultValue : TValue; override;
    function AsSQL(Value : TValue) : String; override;
    function Value(Val : Boolean) : TValue; virtual;
    function ConvertFromString(StrVal : String) : Boolean; virtual;

    function Sum(Value1, Value2 : TValue) : TValue; override;
    function Difference(Value1, Value2 : TValue) : TValue; override;
    function Product(Value1, Value2 : TValue) : TValue; override;
    function Quota(Value1, Value2 : TValue) : TValue; override;
    function NegateValue(Value : TValue) : TValue; override;

    function AndOperator(Value1, Value2 : TValue) : TValue; override;
    function OrOperator(Value1, Value2 : TValue) : TValue; override;

    procedure StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer); override;


    function FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue; override;
    function ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean; override;
  end;

  TCustomBooleanType = class(TBooleanType)
  private
  protected
  public
  end;

  TDateTimeType = class(TDataType)
  private
  protected
    constructor Create;

    function GetAsString(Value : TValue) : String; override;
    function GetAsBoolean(Value : TValue) : Boolean; override;
    function GetAsDateTime(Value : TValue) : TDateTime; override;
    function GetAsPointer(Value : TValue) : Pointer; override;
    function GetAsDouble(Value : TValue) : Double; override;
    function MakeValueNative(Value : TValue) : TValue; override;
    function GetAlignment : TAlignment; override;

    function GetSQLDataTypeString : String; override;
    function GetDataSize : Integer; override;
    function GetDisplayWidth : Integer; override;
    function GetDataTypeName : String; override;

    class function GetDataTypeWithProperties(AProperties : TStrings) : TDataType; override;
  public
    function Compare(Value1, Value2 : TValue) : Integer; override;
    function DefaultValue : TValue; override;
    function AsSQL(Value : TValue) : String; override;
    function Value(Val : TDateTime) : TValue; virtual;
    function ConvertFromString(StrVal : String) : TDateTime; virtual;

    function Sum(Value1, Value2 : TValue) : TValue; override;
    function Difference(Value1, Value2 : TValue) : TValue; override;
    function Product(Value1, Value2 : TValue) : TValue; override;
    function Quota(Value1, Value2 : TValue) : TValue; override;
    function NegateValue(Value : TValue) : TValue; override;
    function ValueBlank(Value : TValue) : Boolean; override;

    procedure StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer); override;


    function FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue; override;
    function ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean; override;

    function GenerateError(InvalidValue : String) : String; override;
  end;

  TDateType = class(TDateTimeType)
  private
  protected
    constructor Create;

    function GetSQLDataTypeString : String; override;
    function GetAsString(Value : TValue) : String; override;
    function GetDisplayWidth : Integer; override;
    function MakeValueNative(Value : TValue) : TValue; override;
    function GetDataTypeName : String; override;

    class function GetDataTypeWithProperties(AProperties : TStrings) : TDataType; override;
  public
    function DefaultValue : TValue; override;
    function AsSQL(Value : TValue) : String; override;
    function Value(Val : TDateTime) : TValue; override;
    function ConvertFromString(StrVal : String) : TDateTime; override;

    procedure StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer); override;


    function FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue; override;
    function ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean; override;
  end;

  TTimeType = class(TDateTimeType)
  private
  protected
    constructor Create;

    function GetSQLDataTypeString : String; override;
    function GetAsString(Value : TValue) : String; override;
    function GetDefaultAggregable : Boolean; override;
    function GetIsNumeric : Boolean; override;
    function GetDisplayWidth : Integer; override;
    function MakeValueNative(Value : TValue) : TValue; override;
    function GetDataTypeName : String; override;

    class function GetDataTypeWithProperties(AProperties : TStrings) : TDataType; override;
  public
    function DefaultValue : TValue; override;
    function AsSQL(Value : TValue) : String; override;
    function ConvertFromString(StrVal : String) : TDateTime; override;

    procedure StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer); override;


    function FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue; override;
    function ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean; override;

    function GenerateError(InvalidValue : String) : String; override;
  end;

  TPointerType = class(TDataType)
  private
  protected
    constructor Create;

    function GetAsString(Value : TValue) : String; override;
    function GetAsBoolean(Value : TValue) : Boolean; override;
    function GetAsDateTime(Value : TValue) : TDateTime; override;
    function GetAsPointer(Value : TValue) : Pointer; override;
    function GetAsDouble(Value : TValue) : Double; override;
    function GetAsInteger(Value : TValue) : Integer; override;
    function MakeValueNative(Value : TValue) : TValue; override;

    function GetSQLDataTypeString : String; override;
    function GetDataSize : Integer; override;
    function GetDisplayWidth : Integer; override;
    function GetCanBeInDB : Boolean; override;
    function GetDataTypeName : String; override;

    class function GetDataTypeWithProperties(AProperties : TStrings) : TDataType; override;
  public
    function Compare(Value1, Value2 : TValue) : Integer; override;
    function DefaultValue : TValue; override;
    function AsSQL(Value : TValue) : String; override;
    function Value(Val : Pointer) : TValue;
    function ValueBlank(Value : TValue) : Boolean; override;

    function Sum(Value1, Value2 : TValue) : TValue; override;
    function Difference(Value1, Value2 : TValue) : TValue; override;
    function Product(Value1, Value2 : TValue) : TValue; override;
    function Quota(Value1, Value2 : TValue) : TValue; override;
    function NegateValue(Value : TValue) : TValue; override;

    procedure StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer); override;


    function FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue; override;
    function ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean; override;
  end;

  TObjectType = class(TPointerType)
  private
  protected
    constructor Create;

    function GetAsString(Value : TValue) : String; override;
    function GetAsPointer(Value : TValue) : Pointer; override;

    function GetDataSize : Integer; override;
    function GetDisplayWidth : Integer; override;
    function GetDataTypeName : String; override;
    function GetAlignment : TAlignment; override;

    class function GetDataTypeWithProperties(AProperties : TStrings) : TDataType; override;
  public
    function Value(Val : TObject) : TValue;

    function FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue; override;
    function ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean; override;
  end;

  TNumericType = class(TDataType)
  protected
    constructor Create(DefaultTransferValueAsParam : Boolean);

    function GetAsBoolean(Value : TValue) : Boolean; override;
    function GetIsNumeric : Boolean; override;
    function GetIsNegative(AValue : TValue) : Boolean; override;
    function GetDefaultAggregable : Boolean; override;
    function GetAlignment : TAlignment; override;
  public
    function GenerateError(InvalidValue : String) : String; override;
  end;

  TIntegerType = class(TNumericType)
  private
  protected
    constructor Create;

    function GetAsString(Value : TValue) : String; override;
    function GetAsBoolean(Value : TValue) : Boolean; override;
    function GetAsDateTime(Value : TValue) : TDateTime; override;
    function GetAsPointer(Value : TValue) : Pointer; override;
    function GetAsDouble(Value : TValue) : Double; override;
    function GetAsInteger(Value : TValue) : Integer; override;
    function GetAsCurrency(Value : TValue) : Currency; override;
    function GetAsPercent(Value : TValue) : Double; override;
    function MakeValueNative(Value : TValue) : TValue; override;

    function GetSQLDataTypeString : String; override;
    function GetDataSize : Integer; override;
    function GetDisplayWidth : Integer; override;
    function GetDataTypeName : String; override;

    class function GetDataTypeWithProperties(AProperties : TStrings) : TDataType; override;
  public
    function Compare(Value1, Value2 : TValue) : Integer; override;
    function DefaultValue : TValue; override;
    function AsSQL(Value : TValue) : String; override;
    function Value(Val : Integer) : TValue;
    function ConvertFromString(StrVal : String) : Integer; virtual;

    function Sum(Value1, Value2 : TValue) : TValue; override;
    function Difference(Value1, Value2 : TValue) : TValue; override;
    function Product(Value1, Value2 : TValue) : TValue; override;
    function Quota(Value1, Value2 : TValue) : TValue; override;
    function NegateValue(Value : TValue) : TValue; override;

    procedure StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer); override;


    function FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue; override;
    function ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean; override;

    function GenerateError(InvalidValue : String) : String; override;
  end;

  TDoubleType = class(TNumericType)
  private
  protected
    constructor Create;

    function GetDefaultDecimalCount : Integer; override;

    function GetAsString(Value : TValue) : String; override;
    function GetAsDateTime(Value : TValue) : TDateTime; override;
    function GetAsPointer(Value : TValue) : Pointer; override;
    function GetAsDouble(Value : TValue) : Double; override;
    function MakeValueNative(Value : TValue) : TValue; override;

    function GetSQLDataTypeString : String; override;
    function GetDataSize : Integer; override;
    function GetDisplayWidth : Integer; override;
    function GetDataTypeName : String; override;

    class function GetDataTypeWithProperties(AProperties : TStrings) : TDataType; override;
  public
    function Compare(Value1, Value2 : TValue) : Integer; override;
    function DefaultValue : TValue; override;
    function AsSQL(Value : TValue) : String; override;
    function Value(Val : Double) : TValue;
    function ConvertFromString(StrVal : String) : Double; virtual;

    function Sum(Value1, Value2 : TValue) : TValue; override;
    function Difference(Value1, Value2 : TValue) : TValue; override;
    function Product(Value1, Value2 : TValue) : TValue; override;
    function Quota(Value1, Value2 : TValue) : TValue; override;
    function NegateValue(Value : TValue) : TValue; override;

    procedure StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer); override;


    function FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue; override;
    function ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean; override;
  end;

  TCurrencyType = class(TNumericType)
  private
  protected
    constructor Create;

    function GetDefaultDecimalCount : Integer; override;

    function GetAsString(Value : TValue) : String; override;
    function GetAsDateTime(Value : TValue) : TDateTime; override;
    function GetAsPointer(Value : TValue) : Pointer; override;
    function GetAsDouble(Value : TValue) : Double; override;
    function GetAsPercent(Value : TValue) : Double; override;
    function GetAsCurrency(Value : TValue) : Currency; override;
    function MakeValueNative(Value : TValue) : TValue; override;

    function GetSQLDataTypeString : String; override;
    function GetDataSize : Integer; override;
    function GetDisplayWidth : Integer; override;
    function GetDataTypeName : String; override;

    class function GetDataTypeWithProperties(AProperties : TStrings) : TDataType; override;
  public
    function Compare(Value1, Value2 : TValue) : Integer; override;
    function DefaultValue : TValue; override;
    function Value(Val : Currency) : TValue;
    function AsSQL(Value : TValue) : String; override;
    function ConvertFromString(StrVal : String) : Currency; virtual;

    function Sum(Value1, Value2 : TValue) : TValue; override;
    function Difference(Value1, Value2 : TValue) : TValue; override;
    function Product(Value1, Value2 : TValue) : TValue; override;
    function Quota(Value1, Value2 : TValue) : TValue; override;
    function NegateValue(Value : TValue) : TValue; override;

    procedure StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer); override;


    function FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue; override;
    function ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean; override;

    function GenerateError(InvalidValue : String) : String; override;
  end;

  TPercentType = class(TNumericType)
  private
  protected
    constructor Create;

    function GetDefaultDecimalCount : Integer; override;

    function GetAsString(Value : TValue) : String; override;
    function GetAsBoolean(Value : TValue) : Boolean; override;
    function GetAsDateTime(Value : TValue) : TDateTime; override;
    function GetAsPointer(Value : TValue) : Pointer; override;
    function GetAsDouble(Value : TValue) : Double; override;
    function GetAsInteger(Value : TValue) : Integer; override;
    function GetAsCurrency(Value : TValue) : Currency; override;
    function GetAsPercent(Value : TValue) : Double; override;
    function MakeValueNative(Value : TValue) : TValue; override;

    function GetSQLDataTypeString : String; override;
    function GetDataSize : Integer; override;
    function GetDisplayWidth : Integer; override;
    function GetDefaultAggregable : Boolean; override;
    function GetDataTypeName : String; override;

    class function GetDataTypeWithProperties(AProperties : TStrings) : TDataType; override;
  public
    function Compare(Value1, Value2 : TValue) : Integer; override;
    function DefaultValue : TValue; override;
    function AsSQL(Value : TValue) : String; override;
    function Value(Val : Double) : TValue;
    function ConvertFromString(StrVal : String) : Double; virtual;

    function Sum(Value1, Value2 : TValue) : TValue; override;
    function Difference(Value1, Value2 : TValue) : TValue; override;
    function Product(Value1, Value2 : TValue) : TValue; override;
    function Quota(Value1, Value2 : TValue) : TValue; override;
    function NegateValue(Value : TValue) : TValue; override;

    procedure StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer); override;


    function FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue; override;
    function ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean; override;

    function GenerateError(InvalidValue : String) : String; override;
  end;

  PValueItem = ^TValueItem;
  TValueItem = record
    FString: string;
    FObject: TObject;
    FValue: TValue;
  end;

  ValueItemBytes = array[0..SizeOf(TValueItem)-1] of Byte;
  ValueBytes = array[0..SizeOf(TValue)-1] of Byte;

  PValueItemList = ^TValueItemList;
  TValueItemList = array[0..(MaxListSize div 2)] of TValueItem;

  TValueList = class(TStrings)
  private
    FList: PValueItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FDataType : TDataType;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer);
    procedure InsertValueItem(Index: Integer; const S: string; const V: TValue);
    procedure SetSorted(Value: Boolean);
  protected
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetValue(Index: Integer): TValue;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutValue(Index: Integer; const AValue: TValue);
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    function SearchValue(const V: TValue) : Integer;
  public
    constructor Create(DataType : TDataType);
    constructor CreateAnyType;
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    function AddVal(V : TValue): Integer;
    function AddValue(const S: string; const V: TValue; const O: TObject): Integer;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Assign(Source : TPersistent); override;
    function Find(const V: TValue; var Index: Integer): Boolean;
    function IndexOfValue(const V: TValue): Integer;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Sort; virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property DataType : TDataType read FDataType;
    property Values[Index : Integer] : TValue read GetValue write PutValue; default;
  end;

  TDataFormatter = class
  private
    FDecimalCount : Integer;
    FDecimalFactor : Longint;
    FDivisor : Extended;
    FFormatMask : String;
    FDefaultFormatMask : String;
    FIsKey : Boolean;
  public
    constructor Create;

    class function RemoveThousandSeparator(Str : String) : String;
    class function RemoveSign(Str, Sign : String) : String;

    procedure SetDecimalCount(MaxCount, MinCount : Integer);

    property Divisor : Extended read FDivisor write FDivisor;
    property DecimalCount : Integer read FDecimalCount;
    property DecimalFactor : Longint read FDecimalFactor;
    property FormatMask : String read FFormatMask write FFormatMask;
    property IsKey : Boolean read FIsKey write FIsKey;
  end;

  ETypeConversionError = class(Exception)
  end;

  TFieldWrapper = class
  public
    function AsString : string; virtual; abstract;
    function AsDateTime : TDateTime; virtual; abstract;
    function AsFloat : Double; virtual; abstract;
    function AsCurrency : currency; virtual; abstract;
    function AsInteger : integer; virtual; abstract;
    function IsNull : Boolean; virtual; abstract;
  end;

  function DataTypeFactory : TDataTypeFactory;

  function AsString(Value : TValue) : String;
  function AsMemo(Value : TValue) : String;
  function AsBoolean(Value : TValue) : Boolean;
  function AsDateTime(Value : TValue) : TDateTime;
  function AsDate(Value : TValue) : TDateTime;
  function AsTime(Value : TValue) : TDateTime;
  function AsPointer(Value : TValue) : Pointer;
  function AsObject(Value : TValue) : TObject;
  function AsDouble(Value : TValue) : Double;
  function AsInteger(Value : TValue) : Integer;
  function AsCurrency(Value : TValue) : Currency;
  function AsPercent(Value : TValue) : Double;

  function ValueFromString(Val : String) : TValue;
  function ValueFromMemo(Val : String) : TValue;
  function ValueFromMemoCS(Val : String) : TValue;
  function ValueFromBoolean(Val : Boolean) : TValue;
  function ValueFromDateTime(Val : TDateTime) : TValue;
  function ValueFromDate(Val : TDateTime) : TValue;
  function ValueFromTime(Val : TDateTime) : TValue;
  function ValueFromPointer(Val : Pointer) : TValue;
  function ValueFromObject(Val : TObject) : TValue;
  function ValueFromDouble(Val : Double) : TValue;
  function ValueFromInteger(Val : Integer) : TValue;
  function ValueFromCurrency(Val : Currency) : TValue;
  function ValueFromPercent(Val : Double) : TValue;

  function ValueIsDefined(AValue : TValue) : Boolean;
  procedure SetValueUndefined(var AValue : TValue);

  function GetStringTypeFromString(AValue : String) : TDataType;
  function GetDataTypeFromString(AValue : String) : TDataType;

  function StrToBool(const AValue : String) : Boolean;
  function StrToBoolDef(const AValue : String; Default:Boolean) : Boolean;
  function BoolToStr(BoolVal : Boolean) : String;

const
  MATCHCASENAME = 'String';
  NOCASENAME = 'Text';
  PROP_SIZE = 'SIZE';
  PROP_DYNAMICSIZE = 'DYNSIZE';
  PROP_CASESENS = 'CS';

var
  EmptyString, TrueValue, FalseValue,
{$ifndef LINUX}
  OneValue, ZeroVal,
{$endif LINUX}
  NilValue : TValue;

  BooleanType : TBooleanType;
  DateTimeType : TDateTimeType;
  DateType : TDateType;
  TimeType : TTimeType;
  PointerType : TPointerType;
  DoubleType : TDoubleType;
  CurrencyType : TCurrencyType;
  PercentType : TPercentType;
  MemoType, MemoTypeCS : TMemoType;
  AnyStringType : TStringType;

{$ifndef LINUX}
  DataTypeList : TList;
  ObjectType : TObjectType;
  IntegerType : TIntegerType;
{$else LINUX}
  function DataTypeList : TList;
  function ObjectType : TObjectType;
  function IntegerType : TIntegerType;

  function ZeroVal : TValue;
  function OneValue : TValue;
{$endif LINUX}

  function NowVal : TValue;

  function StringType(Size : Integer; CaseSensitive : Boolean) : TStringType;
  function UndefinedType(Size : Integer = -1; DynamicSize : Boolean = False) : TUndefinedType;

implementation

uses
{$ifndef LINUX}
  Windows, Consts,
{$endif LINUX}
  Math, DataLib, DataTranslations;

var
  FCaseSensitiveList, FCaseInSensitiveList : TList;
  FUndefinedTypeList : TStringList;
  FDataTypeFactory : TDataTypeFactory;

{$ifdef LINUX}
  FDataTypeList : TList;
  FObjectType : TObjectType;
  FIntegerType : TIntegerType;

  FZeroVal : TValue;
  FOneValue : TValue;

function DataTypeList : TList;
begin
  if FDataTypeList = nil then
    FDataTypeList := TList.Create;

  Result := FDataTypeList;
end;

function ObjectType : TObjectType;
begin
  if FObjectType = nil then
    FObjectType := TObjectType.Create;

  Result := FObjectType;
end;

function IntegerType : TIntegerType;
begin
  if FIntegerType = nil then
    FIntegerType := TIntegerType.Create;

  Result := FIntegerType;
end;

function ZeroVal : TValue;
begin
  if (FZeroVal.DataType = nil) or
     (FZeroVal.DataType <> IntegerType) then
    FZeroVal := ValueFromInteger(0);
  Result := FZeroVal;
end;

function OneValue : TValue;
begin
  if (FOneValue.DataType = nil) or
     (FOneValue.DataType <> IntegerType) then
    FOneValue := ValueFromInteger(1);
  Result := FOneValue;
end;

{$endif LINUX}

function NowVal : TValue;
begin
  Result := ValueFromDateTime(Now);
end;

procedure DoInitialize;
begin
  if FCaseSensitiveList <> nil then
    Exit;

  FCaseSensitiveList := TList.Create;
  FCaseInSensitiveList := TList.Create;

  FUndefinedTypeList := TStringList.Create;
  FUndefinedTypeList.Sorted := True;
  FUndefinedTypeList.Duplicates := dupAccept;

{$ifndef LINUX}
  DataTypeList := TList.Create;
  ObjectType := TObjectType.Create;
  IntegerType := TIntegerType.Create;
{$else LINUX}
// Lazy create instead
{$endif LINUX}

  BooleanType := TBooleanType.Create;
  DateTimeType := TDateTimeType.Create;
  DateType := TDateType.Create;
  TimeType := TTimeType.Create;
  PointerType := TPointerType.Create;
  DoubleType := TDoubleType.Create;
  CurrencyType := TCurrencyType.Create;
  PercentType := TPercentType.Create;
  MemoType := TMemoType.Create;
  MemoTypeCS := TMemoType.Create(True);

  AnyStringType := MemoType;

  EmptyString := ValueFromString('');

  TrueValue := ValueFromBoolean(True);
  FalseValue := ValueFromBoolean(False);
{$ifndef LINUX}
  ZeroVal := ValueFromInteger(0);
  OneValue := ValueFromInteger(1);
{$else LINUX}
// Lazy create instead
{$endif LINUX}
  NilValue := ValueFromPointer(nil);
end;

function PadString(str : String; padChar : Char; minLength : Integer; padLeft : Boolean = True) : String;
var
  diff : Integer;
  pad : String;
begin
  Result := str;
  diff := minLength - Length(str);
  if diff > 0 then
  begin
    pad := StringOfChar(padChar, diff);
    if padLeft then
      Result := pad + Result
    else
      Result := Result + pad;
  end;
end;

{ TDBProperties }



{ TDataTypeFactory }

function DataTypeFactory : TDataTypeFactory;
begin
  if not Assigned(FDataTypeFactory) then
    FDataTypeFactory := TDataTypeFactory.Create;

  Result := FDataTypeFactory;
end;

constructor TDataTypeFactory.Create;
begin
  inherited;

  FDataTypes := TStringList.Create;
  FDataTypes.Sorted := True;
  FDataTypes.Duplicates := dupIgnore;
end;

destructor TDataTypeFactory.Destroy;
begin
  FDataTypes.Free;

  inherited;
end;

function TDataTypeFactory.ClassByName(AName: String): TDataTypeClass;
var
  idx : Integer;
begin
  if FDataTypes.Find(AName, idx) then
    Result := TDataTypeClass(FDataTypes.Objects[idx])
  else
    Result := TUndefinedType;
end;

function TDataTypeFactory.DataTypeByClass(AClass: TDataTypeClass;
  AProperties: TStrings = nil): TDataType;
begin
  if AClass <> nil then
    Result := AClass.GetDataTypeWithProperties(AProperties)
  else
    Result := nil;
end;

function TDataTypeFactory.DataTypeByName(AName: String;
  AProperties: TStrings = nil): TDataType;
begin
  Result := DataTypeByClass(ClassByName(AName), AProperties);
end;

procedure TDataTypeFactory.RegisterDataType(AClass: TDataTypeClass);
begin
  FDataTypes.AddObject(AClass.ClassName, TObject(AClass));
end;

procedure TDataTypeFactory.PropertiesForType(AType: TDataType;
  AProperties: TStrings);
begin
  AType.GetDataTypeProperties(AProperties);
end;

{ TDataType }

function TDataType.ValueInInterval(AValue, IntervalFrom, IntervalTo : TValue) : Boolean;
begin
  Result := (Compare(IntervalFrom, AValue) >= 0) and
            (Compare(AValue, IntervalTo) >= 0);
end;

function TDataType.IsNullValue(AValue : TValue) : Boolean;
begin
  Result := Self.AllowsNullValue and
            Self.Equals(AValue, Self.NullValue);
end;

constructor TDataType.Create(DefaultTransferValueAsParam : Boolean);
begin
  inherited Create;
  FDefaultTransferValueAsParam := DefaultTransferValueAsParam;
  if DataTypeList.IndexOf(Self) < 0 then
    DataTypeList.Add(Self);
  DataTypeFactory.RegisterDataType(TDataTypeClass(Self.ClassType));

  FAllowsNullValue := False;
  FNullValue := Self.DefaultValue;
end;

destructor TDataType.Destroy;
var
  FIndex : Integer;
begin
  FIndex := DataTypeList.IndexOf(Self);
  while not (FIndex < 0) do
  begin
    DataTypeList.Delete(FIndex);
    FIndex := DataTypeList.IndexOf(Self);
  end;
  inherited Destroy;
end;

function TDataType.TransferValueAsParam(Value : TValue) : Boolean;
begin
  Result := FDefaultTransferValueAsParam;
end;

function TDataType.GetAsBoolean(Value : TValue) : Boolean;
var
  StrVal : String;
begin
  StrVal := GetAsString(Value);
  Result := BooleanType.ConvertFromString(StrVal);
end;

function TDataType.GetAsDate(Value : TValue) : TDateTime;
begin
  Result := Trunc(GetAsDateTime(Value));
end;

function TDataType.GetAsTime(Value : TValue) : TDateTime;
begin
  Result := GetAsDateTime(Value);
end;

function TDataType.GetAsObject(Value : TValue) : TObject;
begin
  Result := TObject(GetAsPointer(Value));
end;

function TDataType.GetAsInteger(Value : TValue) : Integer;
begin
  Result := CRound(GetAsDouble(Value));
end;

function TDataType.GetAsCurrency(Value : TValue) : Currency;
begin
  Result := GetAsDouble(Value);
end;

function TDataType.GetAsPercent(Value : TValue) : Double;
begin
  Result := GetAsDouble(Value);
end;

function TDataType.Equals(Value1, Value2 : TValue) : Boolean;
begin
  try
    Result := (Compare(Value1, Value2) = 0);
  except
    Result := False;
  end;
end;

function TDataType.EqualsMatchCase(Value1, Value2 : TValue) : Boolean;
begin
  Result := Equals(Value1, Value2);
end;

function TDataType.GetCanBeInDB : Boolean;
begin
  Result := True;
end;

function TDataType.GetIsNumeric : Boolean;
begin
  Result := False;
end;

function TDataType.GetIsNegative(AValue : TValue) : Boolean;
begin
  Result := False;
end;

function TDataType.GetDefaultDecimalCount : Integer;
begin
  REsult := 0;
end;

function TDataType.Max(Value1, Value2 : TValue) : TValue;
begin
  if Compare(Value1, Value2) < 0 then
    Result := Value2
  else
    Result := Value1;
end;

function TDataType.Min(Value1, Value2 : TValue) : TValue;
begin
  if Compare(Value1, Value2) < 0 then
    Result := Value1
  else
    Result := Value2;
end;

function TDataType.GetDefaultAggregable : Boolean;
begin
  Result := False;
end;

function TDataType.GetAlignment : TAlignment;
begin
  Result := taLeftJustify;
end;

function TDataType.FetchValue(Address : Pointer; GetSpecialValue : TGetSpecialValueProc; SpecialValueIndex : Integer) : TValue;
begin
  Result.DataType := Self;
  Result.sForInternalUseOnly := '';
  CopyMemory(@Result.CommonData, Address, DataSize);
end;

function TDataType.Optimize(Value : TValue) : TValue;
begin
  if (Self = nil) or (Self = Value.DataType) then
    Result := Value
  else
    Result := MakeValueNative(Value);
end;

function TDataType.ValueBlank(Value : TValue) : Boolean;
begin
  Result := False;
end;

function TDataType.AndOperator(Value1, Value2 : TValue) : TValue;
begin
  Result := DefaultValue;
end;

function TDataType.OrOperator(Value1, Value2 : TValue) : TValue;
begin
  Result := DefaultValue;
end;

procedure TDataType.GetDataTypeProperties(AProperties: TStrings);
begin
  // nothing
end;

function TDataType.GetDynamicSize: Boolean;
begin
  Result := False;
end;

function TDataType.GenerateError(InvalidValue: String): String;
begin
  Result := TranslateMessage(E_ValueInvalidType, [InvalidValue]);
end;

{ TUndefinedType }

procedure TUndefinedType.Error;
begin
  raise Exception.Create('Undefined DataType!');
end;

function TUndefinedType.AsSQL(Value: TValue): String;
begin
  Result := '';
  Error;
end;

function TUndefinedType.Compare(Value1, Value2: TValue): Integer;
begin
  Result := 0;
  Error;
end;

constructor TUndefinedType.Create(Size: Integer; DynamicSize: Boolean);
begin
  inherited Create(False);

  FSize := Size;
  FDynamicSize := DynamicSize;
end;

function TUndefinedType.DefaultValue: TValue;
begin
  Result := NilValue;
end;

function TUndefinedType.Difference(Value1, Value2: TValue): TValue;
begin
  Result := NilValue;
  Error;
end;

function TUndefinedType.EqualsMatchCase(Value1, Value2: TValue): Boolean;
begin
  Result := False;
  Error;
end;

function TUndefinedType.FetchValue(Address: Pointer;
  GetSpecialValue: TGetSpecialValueProc; SpecialValueIndex : Integer): TValue;
begin
  Result := NilValue;
  Error;
end;

function TUndefinedType.FormattedValue(AValue: TValue;
  DataFormatter: TDataFormatter): TValue;
begin
  Result := NilValue;
  Error;
end;

function TUndefinedType.GetAsCurrency(Value: TValue): Currency;
begin
  Result := 0;
  Error;
end;

function TUndefinedType.GetAsDate(Value: TValue): TDateTime;
begin
  Result := 0;
  Error;
end;

function TUndefinedType.GetAsDateTime(Value: TValue): TDateTime;
begin
  Result := 0;
  Error;
end;

function TUndefinedType.GetAsDouble(Value: TValue): Double;
begin
  Result := 0;
  Error;
end;

function TUndefinedType.GetAsInteger(Value: TValue): Integer;
begin
  Result := 0;
  Error;
end;

function TUndefinedType.GetAsPercent(Value: TValue): Double;
begin
  Result := 0;
  Error;
end;

function TUndefinedType.GetAsPointer(Value: TValue): Pointer;
begin
  Result := nil;
  Error;
end;

function TUndefinedType.GetAsString(Value: TValue): String;
begin
  Result := '';
  Error;
end;

function TUndefinedType.GetAsTime(Value: TValue): TDateTime;
begin
  Result := 0;
  Error;
end;

function TUndefinedType.GetDataSize: Integer;
begin
  Result := FSize;
end;

function TUndefinedType.GetDataTypeName: String;
begin
  Result := 'Undefined';
  if DynamicSize then
    Result := Result + 'DS';
  Result := Result + '(' + IntToStr(DataSize) + ')';
end;

procedure TUndefinedType.GetDataTypeProperties(AProperties: TStrings);
begin
  AProperties.Add(PROP_SIZE + '=' + IntToStr(DataSize));
  AProperties.Add(PROP_DYNAMICSIZE + '=' + AsString(ValueFromBoolean(DynamicSize)));
end;

class function TUndefinedType.GetDataTypeWithProperties(
  AProperties: TStrings): TDataType;
var
  val : String;
  aSize : Integer;
  aDS : Boolean;
begin
  aSize := -1;
  aDS := False;

  val := AProperties.Values[PROP_SIZE];
  if val <> '' then
    try
      aSize := StrToInt(val);
    except
    end;

  val := AProperties.Values[PROP_DYNAMICSIZE];
  if val <> '' then
    aDS := AsBoolean(ValueFromString(val));

  Result := UndefinedType(aSize, aDS);
end;

function TUndefinedType.GetDisplayWidth: Integer;
begin
  Result := 0;
  Error;
end;

function TUndefinedType.GetSQLDataTypeString: String;
begin
  Result := '';
  Error;
end;

function TUndefinedType.MakeValueNative(Value: TValue): TValue;
begin
  Result := NilValue;
  Error;
end;

function TUndefinedType.NegateValue(Value: TValue): TValue;
begin
  Result := NilValue;
  Error;
end;

function TUndefinedType.ParseText(AString: String; var ParsedValue: TValue;
  DataFormatter: TDataFormatter): Boolean;
begin
  Result := False;
  Error;
end;

function TUndefinedType.Product(Value1, Value2: TValue): TValue;
begin
  Result := NilValue;
  Error;
end;

function TUndefinedType.Quota(Value1, Value2: TValue): TValue;
begin
  Result := NilValue;
  Error;
end;



procedure TUndefinedType.StoreValue(Address: Pointer; Value: TValue;
  SetSpecialValue: TSetSpecialValueProc; SpecialValueIndex: Integer);
begin
  Error;
end;

function TUndefinedType.Sum(Value1, Value2: TValue): TValue;
begin
  Result := NilValue;
  Error;
end;

function TUndefinedType.TransferValueAsParam(Value: TValue): Boolean;
begin
  Result := False;
  Error;
end;

function TUndefinedType.Value(Val: String): TValue;
begin
  Result := NilValue;
  Error;
end;

function TUndefinedType.ValueBlank(Value: TValue): Boolean;
begin
  Result := False;
  Error;
end;



function TUndefinedType.GetDynamicSize: Boolean;
begin
  Result := FDynamicSize;
end;

{ TStringType }

constructor TStringType.Create(Size : Integer; CaseSensitive : Boolean);
begin
  inherited Create(False);
  FSize := Size;
  FCaseSensitive := CaseSensitive;
end;

function TStringType.TransferValueAsParam(Value : TValue) : Boolean;
begin
  if Pos('''', GetAsString(Value)) >= 1 then
    Result := True
  else
    Result := inherited TransferValueAsParam(Value);
end;

function TStringType.Compare(Value1, Value2 : TValue) : Integer;
var
  V1, V2 : String;
begin
  V1 := Value1.DataType.GetAsString(Value1);
  V2 := Value2.DataType.GetAsString(Value2);

  if CaseSensitive then
    Result := CompareStr(V1, V2)
  else
    Result := CompareText(V1, V2);
end;

function TStringType.EqualsMatchCase(Value1, Value2 : TValue) : Boolean;
var
  V1, V2 : String;
begin
  try
    V1 := Value1.DataType.GetAsString(Value1);
    V2 := Value2.DataType.GetAsString(Value2);

    Result := (CompareStr(V1, V2) = 0);
  except
    Result := False;
  end;
end;

function TStringType.DefaultValue : TValue;
begin
  Result := Value('');
end;

function TStringType.Value(Val : String) : TValue;
begin
  if (Self.Size > 0) and (Length(Val) > Self.Size) then
    Val := Copy(Val, 1, Self.Size);

  Result.DataType := Self;
  Result.sForInternalUseOnly := Val;
end;

function TStringType.GetAsString(Value : TValue) : String;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsString(Value)
  else
    Result := Value.sForInternalUseOnly;
end;

function TStringType.GetAsDateTime(Value : TValue) : TDateTime;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsDateTime(Value)
  else
    // MVJ 19.3.2003: Catching an exception here doesn't let higher levels
    // in the application know that the given value was illegal!
    Result := DateTimeType.ConvertFromString(Value.sForInternalUseOnly);
(*
  try
      Result := DateTimeType.ConvertFromString(Value.sForInternalUseOnly);
    except
      Result := 0.0;
    end;
*)
end;

function TStringType.GetAsDate(Value : TValue) : TDateTime;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsDate(Value)
  else
    Result := DateType.ConvertFromString(Value.sForInternalUseOnly);
end;

function TStringType.GetAsTime(Value : TValue) : TDateTime;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsTime(Value)
  else
    Result := TimeType.ConvertFromString(Value.sForInternalUseOnly);
end;

function TStringType.GetAsPointer(Value : TValue) : Pointer;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsPointer(Value)
  else
    Result := nil;
end;

function TStringType.GetAsDouble(Value : TValue) : Double;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsDouble(Value)
  else
    Result := DoubleType.ConvertFromString(Value.sForInternalUseOnly);
end;

function TStringType.GetAsInteger(Value : TValue) : Integer;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsInteger(Value)
  else
    Result := IntegerType.ConvertFromString(Value.sForInternalUseOnly);
end;

function TStringType.GetAsCurrency(Value : TValue) : Currency;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsCurrency(Value)
  else
    Result := CurrencyType.ConvertFromString(Value.sForInternalUseOnly);
end;

function TStringType.GetAsPercent(Value : TValue) : Double;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsPercent(Value)
  else
    Result := PercentType.ConvertFromString(Value.sForInternalUseOnly);
end;

function TStringType.MakeValueNative(Value : TValue) : TValue;
begin
  if Value.DataType = Self then
    Result := Value
  else
    Result := Self.Value(AsString(Value));
end;

function TStringType.GetSizeStorageSize : Byte;
begin
  if Self.Size <= 255 then
    Result := 1
  else
    Result := 2;
end;

function TStringType.GetMaxSize : Integer; 
begin
  Result := Self.Size;
end;

class function TStringType.MaxLength : Integer;
begin
  Result := Floor(Power(2, 16) - 1);
end;

function TStringType.GetDataSize : Integer;
begin
  Result := Self.Size + GetSizeStorageSize;
end;

function TStringType.GetDisplayWidth : Integer;
begin
  Result := GetMaxSize;
end;

function TStringType.GetDataTypeName : String;
begin
  if Self = AnyStringType then
    Result := 'AnyStringType'
  else if Self.CaseSensitive then
    Result := MATCHCASENAME +'(' +IntToStr(Self.Size) +')'
  else
    Result := NOCASENAME +'(' +IntToStr(Self.Size) +')';
end;

function TStringType.GetSQLDataTypeString : String;
begin
  Result := 'char(' + IntToStr(Self.Size) + ')';
end;

function TStringType.AsSQL(Value : TValue) : String;
var
  SV : String;
begin
  SV := AsString(Value);
  if (Length(SV) > Self.Size) then
    SV := Copy(SV, 1, Self.Size);
  Result := QuotedStr(SV);
end;

function TStringType.FetchValue(Address : Pointer; GetSpecialValue : TGetSpecialValueProc; SpecialValueIndex : Integer) : TValue;
var
  StrData : String;
  S : Byte;
  SLen : Integer;
begin
  S := GetSizeStorageSize;
  case S of
    1: SLen := PByte(Address)^;
    2: SLen := PWord(Address)^;
  else
    raise Exception.Create('Internal error!');
  end;

  StrData := StringOfChar(' ', SLen);
  if SLen > 0 then
    CopyMemory(@StrData[1], Pointer(Integer(Address) + S), SLen);

  Result.DataType := Self;
  Result.sForInternalUseOnly := StrData;
end;

procedure TStringType.StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer);
  function Min(i, j : Integer) : Integer;
  begin
    if i < j then
      Result := i
    else
      Result := j;
  end;

var
  StrVal : String;
  s : Byte;
  l : Integer;
  lb : Byte;
  lw : Word;
begin
  StrVal := AsString(Value);
  l := Min(Length(StrVal), GetMaxSize);

  s := GetSizeStorageSize;
  case s of
    1: begin lb := Byte(l); CopyMemory(Address, @lb, 1); end;
    2: begin lw := Word(l); CopyMemory(Address, @lw, 2); end;
  else
    raise Exception.Create('Internal error!');
  end;

  if l > 0 then
    CopyMemory(Pointer(Integer(Address) + s), @StrVal[1], l);
end;





function TStringType.ValueBlank(Value : TValue) : Boolean;
begin
  try
    Result := (AsString(Value) = '');
  except
    Result := False;
  end;
end;

function TStringType.Sum(Value1, Value2 : TValue) : TValue;
begin
  Result := Value(AsString(Value1) + AsString(Value2));
end;

function TStringType.Difference(Value1, Value2 : TValue) : TValue;
begin
  raise Exception.Create('TStringType.Difference: Operation not supported!');
end;

function TStringType.Product(Value1, Value2 : TValue) : TValue;
begin
  raise Exception.Create('TStringType.Product: Operation not supported!');
end;

function TStringType.Quota(Value1, Value2 : TValue) : TValue;
begin
  raise Exception.Create(Self.ClassName + '.Quota: Operation not supported!');
end;

function TStringType.NegateValue(Value : TValue) : TValue;
begin
  Result := Value;
end;

function TStringType.FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue;
begin
  Result := ValueFromString( Trim(AsString(AValue)) );
end;

function TStringType.ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean;
begin
  Result := True;
  ParsedValue := Self.Value(AString);
end;

procedure TStringType.GetDataTypeProperties(AProperties: TStrings);
begin
  AProperties.Add(PROP_SIZE + '=' + IntToStr(Size));
  AProperties.Add(PROP_CASESENS + '=' + AsString(ValueFromBoolean(CaseSensitive)));
end;

class function TStringType.GetDataTypeWithProperties(
  AProperties: TStrings): TDataType;
var
  val : String;
  aSize : Integer;
  aCS : Boolean;
begin
  aSize := -1;
  aCS := True;

  val := AProperties.Values[PROP_SIZE];
  if val <> '' then
    try
      aSize := StrToInt(val);
    except
    end;

  if aSize > 0 then
  begin
    val := AProperties.Values[PROP_CASESENS];
    if val <> '' then
      aCS := AsBoolean(ValueFromString(val));

    Result := StringType(aSize, aCS)
  end
  else
    Result := TMemoType.GetDataTypeWithProperties(AProperties);
end;

{ TMemoType }

const
  MEMOMAXDATAROWSTORAGESIZE = 254;
  MEMODISPLAYWIDTH = 80;

class function TMemoType.MaxLength : Integer;
begin
  Result := Floor(Power(2, 31) - 1);
end;

constructor TMemoType.Create(CaseSensitive : Boolean = False);
begin
  inherited Create(MaxLength, CaseSensitive);
  FDefaultTransferValueAsParam := True;
end;

function TMemoType.GetDataTypeName : String;
begin
  Result := 'Memo';
end;

function TMemoType.GetSQLDataTypeString : String;
begin
  Result := 'text';
end;



function TMemoType.Value(Val : String) : TValue;
begin
  Result.DataType := Self;
  Result.sForInternalUseOnly := Val;
end;



function TMemoType.TransferValueAsParam(Value : TValue) : Boolean;
begin
  Result := True;
end;

function TMemoType.FetchValue(Address : Pointer; GetSpecialValue : TGetSpecialValueProc; SpecialValueIndex : Integer) : TValue;
var
  StrData : String;
  StorageSize : Byte;
begin
  Result.DataType := Self;

  StorageSize := PByte(Address)^;
  if StorageSize = 0 then // storing in datarow
  begin
    Result.sForInternalUseOnly := '';
    if Assigned(GetSpecialValue) then
      GetSpecialValue(SpecialValueIndex, Result.sForInternalUseOnly);
  end
  else
  begin
    StorageSize := StorageSize - 1;
    StrData := StringOfChar(' ', StorageSize);
    if StorageSize > 0 then
      CopyMemory(@StrData[1], Pointer(Integer(Address) + 1), StorageSize);

    Result.sForInternalUseOnly := StrData;
  end;
end;

procedure TMemoType.StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer);
var
  StrVal : String;
  len : Integer;
  lb : Byte;
begin
  StrVal := AsMemo(Value);
  len := Length(StrVal);

  if len <= MEMOMAXDATAROWSTORAGESIZE then
  begin
    lb := Byte(len + 1);
    CopyMemory(Address, @lb, 1);
    if len > 0 then
      CopyMemory(Pointer(Integer(Address) + 1), @StrVal[1], len);
  end
  else
  begin
    lb := 0;
    CopyMemory(Address, @lb, 1);

    if Assigned(SetSpecialValue) then
      SetSpecialValue(SpecialValueIndex, StrVal);
  end;
end;

function TMemoType.GetDataSize : Integer;
begin
  Result := 1 + MEMOMAXDATAROWSTORAGESIZE;
end;

function TMemoType.GetDisplayWidth : Integer;
begin
  Result := MEMODISPLAYWIDTH;
end;

function TMemoType.AsSQL(Value : TValue) : String;
var
  SV : String;
begin
  SV := AsMemo(Value);
  Result := QuotedStr(SV);
end;

procedure TMemoType.GetDataTypeProperties(AProperties: TStrings);
begin
  inherited GetDataTypeProperties(AProperties);
  AProperties.Add(PROP_DYNAMICSIZE + '=' + AsString(ValueFromBoolean(DynamicSize)));
end;

class function TMemoType.GetDataTypeWithProperties(
  AProperties: TStrings): TDataType;
var
  val : String;
  aCS : Boolean;
begin
  aCS := True;

  val := AProperties.Values[PROP_CASESENS];
  if val <> '' then
    try
    except
      aCS := AsBoolean(ValueFromString(val));
    end;

  if aCS then
    Result := MemoTypeCS
  else
    Result := MemoType;
end;

function TMemoType.GetDynamicSize: Boolean;
begin
  Result := True;
end;

{ TBooleanType }

function StrToBool(const AValue : String) : Boolean;
var
  StrVal : string;
begin
  StrVal := UpperCase( AValue );
  if (StrVal = 'T') or (StrVal = 'TRUE') then
    Result := True
  else if (StrVal = 'F') or (StrVal = 'FALSE') then
    Result := False
  else if (Trim(StrVal) = '') or (StrVal = 'N') or (StrVal = 'NO') then
    Result := False
  else
    Result := True;
end;

function StrToBoolDef(const AValue : String; Default:Boolean) : Boolean;
var
  StrVal : string;
begin
  StrVal := UpperCase( AValue );
  if (StrVal = 'T') or (StrVal = 'TRUE') then
    Result := True
  else if (StrVal = 'F') or (StrVal = 'FALSE') then
    Result := False
  else
    Result := Default;
end;

function BoolToStr(BoolVal : Boolean) : String;
begin
  if BoolVal then
    Result := 'T'
  else
    Result := 'F';
end;

constructor TBooleanType.Create;
begin
  inherited Create(True);
end;

function TBooleanType.DefaultValue : TValue;
begin
  Result := Value(False);
end;

function TBooleanType.Value(Val : Boolean) : TValue;
begin
  Result.DataType := Self;
  Result.sForInternalUseOnly := '';
  CopyMemory(@Result.CommonData, @Val, SizeOf(Val));
end;

function TBooleanType.ConvertFromString(StrVal : String) : Boolean;
begin
  Result := (StrVal = 'T') or (StrVal = 't') or
            (StrVal = 'true') or (StrVal = 'TRUE') or
            (StrVal = 'True') or (StrVal = 'X') or
            (StrVal = 'x');
end;

function TBooleanType.GetAsString(Value : TValue) : String;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsString(Value)
  else if GetAsBoolean(Value) = True then
    Result := 'T'
  else
    Result := 'F';
end;

function TBooleanType.GetAsBoolean(Value : TValue) : Boolean;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsBoolean(Value)
  else
    CopyMemory(@Result, @Value.CommonData, SizeOf(Result));
end;

function TBooleanType.GetAsDateTime(Value : TValue) : TDateTime;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsDateTime(Value)
  else
    raise ETypeConversionError.Create('Cannot convert Boolean to DateTime!');
end;

function TBooleanType.GetAsPointer(Value : TValue) : Pointer;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsPointer(Value)
  else
    raise ETypeConversionError.Create('Cannot convert Boolean to Pointer!');
end;

function TBooleanType.GetAsDouble(Value : TValue) : Double;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsDouble(Value)
  else if GetAsBoolean(Value) then
    Result := 1.0
  else
    Result := 0.0;
end;

function TBooleanType.MakeValueNative(Value : TValue) : TValue;
begin
  if Value.DataType = Self then
    Result := Value
  else
    Result := Self.Value(AsBoolean(Value));
end;

function TBooleanType.Compare(Value1, Value2 : TValue) : Integer;
var
  V1, V2 : Boolean;
begin
  V1 := AsBoolean(Value1);
  V2 := AsBoolean(Value2);

  if V1 = V2 then
    Result := 0
  else if V1 = False then
    Result := -1
  else
    Result := 1;
end;

function TBooleanType.GetDataSize : Integer;
begin
  Result := SizeOf(Boolean);
end;

function TBooleanType.GetDisplayWidth : Integer;
begin
  Result := 5;
end;

function TBooleanType.GetDataTypeName : String;
begin
  Result := 'Boolean';
end;

function TBooleanType.GetSQLDataTypeString : String;
begin
  Result := 'char(1)';
end;

function TBooleanType.AsSQL(Value : TValue) : String;
begin
  Result := QuotedStr(AsString(Value));
end;

procedure TBooleanType.StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer);
var
  BoolVal : Boolean;
begin
  if Value.DataType = Self then
  begin
    CopyMemory(Address, @Value.CommonData, DataSize);
  end
  else
  begin
    BoolVal := AsBoolean(Value);
    CopyMemory(Address, @BoolVal, DataSize);
  end;
end;





function TBooleanType.AndOperator(Value1, Value2 : TValue) : TValue;
begin
  Result := Self.Value(AsBoolean(Value1) and AsBoolean(Value2));
end;

function TBooleanType.OrOperator(Value1, Value2 : TValue) : TValue;
begin
  Result := Self.Value(AsBoolean(Value1) or AsBoolean(Value2));
end;

function TBooleanType.Sum(Value1, Value2 : TValue) : TValue;
begin
  raise Exception.Create('TBooleanType.Sum: Operation not supported!');
end;              

function TBooleanType.Difference(Value1, Value2 : TValue) : TValue;
begin
  Result := Value(not Equals(Value1, Value2));
end;

function TBooleanType.Product(Value1, Value2 : TValue) : TValue;
begin
  raise Exception.Create('TBooleanType.Product: Operation not supported!');
end;

function TBooleanType.Quota(Value1, Value2 : TValue) : TValue;
begin
  raise Exception.Create('TBooleanType.Quota: Operation not supported!');
end;

function TBooleanType.NegateValue(Value : TValue) : TValue;
begin
  Result := Self.Value(not AsBoolean(Value));
end;

function TBooleanType.FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue;
begin
  Result := AnyStringType.MakeValueNative(AValue);
end;

function TBooleanType.ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean;
begin
  Result := True;
  ParsedValue := MakeValueNative( AnyStringType.Value(AString) );
end;

class function TBooleanType.GetDataTypeWithProperties(
  AProperties: TStrings): TDataType;
begin
  Result := BooleanType;
end;

{ TNumericType }

constructor TNumericType.Create(DefaultTransferValueAsParam : Boolean);
begin
  inherited Create(DefaultTransferValueAsParam);
end;

function TNumericType.GetIsNumeric : Boolean;
begin
  Result := True;
end;

function TNumericType.GetIsNegative(AValue : TValue) : Boolean;
begin
  try
    Result := (Compare(AValue, Self.DefaultValue) < 0);
  except
    Result := False;
  end;
end;

function TNumericType.GetDefaultAggregable : Boolean;
begin
  Result := True;
end;

function TNumericType.GetAlignment : TAlignment;
begin
  Result := taRightJustify;
end;

function TNumericType.GetAsBoolean(Value: TValue): Boolean;
var
  aDouble : Double;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsBoolean(Value)
  else
  begin
    aDouble := GetAsDouble(Value);
    Result := aDouble <> 0;
  end;
end;

function TNumericType.GenerateError(InvalidValue: String): String;
begin
  Result := TranslateMessage(E_ValueInvalidNumber, [InvalidValue]);
end;

{ TCurrencyType }

constructor TCurrencyType.Create;
begin
  inherited Create(True);
end;

function TCurrencyType.GetDefaultDecimalCount : Integer;
begin
  Result := 2;
end;

function TCurrencyType.DefaultValue : TValue;
begin
  Result := Value(0.0);
end;

function TCurrencyType.Value(Val : Currency) : TValue;
begin
  Result.DataType := Self;
  Result.sForInternalUseOnly := '';
  CopyMemory(@Result.CommonData, @Val, SizeOf(Val));
end;

function TCurrencyType.ConvertFromString(StrVal : String) : Currency;
begin
  StrVal := Trim(StrVal);

  if Length(StrVal) = 0 then
    Result := 0.0
  else
    Result := StrToCurr(StrVal);
end;

function TCurrencyType.Compare(Value1, Value2 : TValue) : Integer;
var
  V1, V2 : Currency;
begin
  V1 := AsCurrency(Value1);
  V2 := AsCurrency(Value2);

  if V1 = V2 then
    Result := 0
  else if V1 < V2 then
    Result := -1
  else
    Result := 1;
end;

function TCurrencyType.GetAsString(Value : TValue) : String;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsString(Value)
  else
    Result := CurrToStr(GetAsCurrency(Value));
end;

function TCurrencyType.GetAsDateTime(Value : TValue) : TDateTime;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsDateTime(Value)
  else
    raise ETypeConversionError.Create('Cannot convert Currency to DateTime!');
end;

function TCurrencyType.GetAsPointer(Value : TValue) : Pointer;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsPointer(Value)
  else
    raise ETypeConversionError.Create('Cannot convert Currency to Pointer!');
end;

function TCurrencyType.GetAsDouble(Value : TValue) : Double;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsDouble(Value)
  else
    Result := GetAsCurrency(Value);
end;

function TCurrencyType.GetAsPercent(Value : TValue) : Double;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsPercent(Value)
  else
    Result := AsCurrency(Value);
end;

function TCurrencyType.GetAsCurrency(Value : TValue) : Currency;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsCurrency(Value)
  else
    CopyMemory(@Result, @Value.CommonData, SizeOf(Result));
end;

function TCurrencyType.MakeValueNative(Value : TValue) : TValue;
begin
  if Value.DataType = Self then
    Result := Value
  else
    Result := Self.Value(AsCurrency(Value));
end;

function TCurrencyType.GetDataSize : Integer;
begin
  Result := SizeOf(Currency);
end;

function TCurrencyType.GetDisplayWidth : Integer;
begin
  Result := 16;
end;

function TCurrencyType.GetDataTypeName : String;
begin
  Result := 'Currency';
end;

function TCurrencyType.GetSQLDataTypeString : String;
begin
  // Result := 'currency'; // Fixa LGE SQL Server
  Result := 'money';
end;

function TCurrencyType.AsSQL(Value : TValue) : String;
begin
  Result := AsString(Value);
end;

procedure TCurrencyType.StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer);
var
  CurrVal : Currency;
begin
  if Value.DataType = Self then
  begin
    CopyMemory(Address, @Value.CommonData, DataSize);
  end
  else
  begin
    CurrVal := AsCurrency(Value);
    CopyMemory(Address, @CurrVal, DataSize);
  end;
end;



function TCurrencyType.Sum(Value1, Value2 : TValue) : TValue;
begin
  Result := ValueFromCurrency(AsCurrency(Value1) + AsCurrency(Value2));
end;

function TCurrencyType.Difference(Value1, Value2 : TValue) : TValue;
begin
  Result := ValueFromCurrency(AsCurrency(Value1) - AsCurrency(Value2));
end;

function TCurrencyType.Product(Value1, Value2 : TValue) : TValue;
begin
  Result := ValueFromCurrency(AsCurrency(Value1) * AsCurrency(Value2));
end;

function TCurrencyType.Quota(Value1, Value2 : TValue) : TValue;
var
  curr1, curr2 : Currency;
begin
  try
    if AsCurrency(Value2) = 0 then
      Result := DefaultValue
    else
    begin
      curr1 := AsCurrency(Value1);
      curr2 := AsCurrency(Value2);
      Result := ValueFromCurrency( Curr1 / Curr2);
    end;
  except
    Result := DefaultValue;
  end;
end;

function TCurrencyType.NegateValue(Value : TValue) : TValue;
begin
  Result := ValueFromCurrency(- AsCurrency(Value));
end;



function TCurrencyType.FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue;
var
  ACurr : Currency;
  AStr : String;
begin
  if DataFormatter.IsKey then
    Result := ValueFromString( Trim(FormatCurr( DataFormatter.RemoveSign(DataFormatter.FormatMask, SysUtils.ThousandSeparator),
                               CDecimalRound( AsCurrency(AValue), DataFormatter.DecimalFactor ))) )
  else
  begin
    ACurr := CDecimalRound( AsCurrency(AValue) / DataFormatter.Divisor, DataFormatter.DecimalFactor ) ;
    AStr := Trim( FormatCurr( DataFormatter.FormatMask, ACurr ) );
    Result := ValueFromString( AStr );
  end;
end;

function TCurrencyType.ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean;
var
  ValueAsCurr, CurrPart : Currency;
  IntPart : Extended;
begin
  Result := True;
  try
    ValueAsCurr := StrToCurr( DataFormatter.RemoveThousandSeparator(AString) ) * DataFormatter.Divisor;

    if DataFormatter.DecimalCount >= 0 then
    begin
      IntPart := Trunc(ValueAsCurr);
      CurrPart := ValueAsCurr - IntPart;
      CurrPart := CDecimalRound(CurrPart, DataFormatter.DecimalFactor);

      ValueAsCurr := IntPart + CurrPart;
    end;

    ParsedValue := Value(ValueAsCurr);
  except on EConvertError do
    begin
      ParsedValue := AnyStringType.Value(AString);
      Result := False;
    end;
  end;
end;

class function TCurrencyType.GetDataTypeWithProperties(
  AProperties: TStrings): TDataType;
begin
  Result := CurrencyType;
end;

function TCurrencyType.GenerateError(InvalidValue: String): String;
begin
  Result := TranslateMessage(E_ValueInvalidCurrency, [InvalidValue]);
end;

{ TDoubleType }

constructor TDoubleType.Create;
begin
  inherited Create(True);
end;

function TDoubleType.GetDefaultDecimalCount : Integer;
begin
  Result := 1;
end;

function TDoubleType.DefaultValue : TValue;
begin
  Result := Value(0.0);
end;

function TDoubleType.Value(Val : Double) : TValue;
begin
  Result.DataType := Self;
  Result.sForInternalUseOnly := '';
  CopyMemory(@Result.CommonData, @Val, SizeOf(Val));
end;

function TDoubleType.ConvertFromString(StrVal : String) : Double;
begin
  StrVal := Trim(StrVal);
  if Length(StrVal) = 0 then
    Result := 0.0
  else
    Result := StrToFloat(StrVal);
end;

function TDoubleType.Compare(Value1, Value2 : TValue) : Integer;
var
  V1, V2 : Double;
begin
  V1 := AsDouble(Value1);
  V2 := AsDouble(Value2);

  if V1 = V2 then
    Result := 0
  else if V1 < V2 then
    Result := -1
  else
    Result := 1;
end;

function TDoubleType.GetAsString(Value : TValue) : String;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsString(Value)
  else
    Result := FloatToStr(GetAsDouble(Value));
end;

function TDoubleType.GetAsDateTime(Value : TValue) : TDateTime;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsDateTime(Value)
  else
    Result := GetAsDouble(Value);
end;

function TDoubleType.GetAsPointer(Value : TValue) : Pointer;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsPointer(Value)
  else
    raise ETypeConversionError.Create('Cannot convert Double to Pointer!');
end;

function TDoubleType.GetAsDouble(Value : TValue) : Double;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsDouble(Value)
  else
    CopyMemory(@Result, @Value.CommonData, SizeOf(Result));
end;

function TDoubleType.MakeValueNative(Value : TValue) : TValue;
begin
  if Value.DataType = Self then
    Result := Value
  else
    Result := Self.Value(AsDouble(Value));
end;

function TDoubleType.GetDataSize : Integer;
begin
  Result := SizeOf(Double);
end;

function TDoubleType.GetDisplayWidth : Integer;
begin
  Result := 16;
end;

function TDoubleType.GetDataTypeName : String;
begin
  Result := 'Float';
end;

function TDoubleType.GetSQLDataTypeString : String;
begin
  Result := 'float';
end;

function TDoubleType.AsSQL(Value : TValue) : String;
begin
  Result := AsString(Value);
end;



procedure TDoubleType.StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer);
var
  DoubleVal : Double;
begin
  if Value.DataType = Self then
  begin
    CopyMemory(Address, @Value.CommonData, DataSize);
  end
  else
  begin
    DoubleVal := AsDouble(Value);
    CopyMemory(Address, @DoubleVal, DataSize);
  end;  
end;

function TDoubleType.Sum(Value1, Value2 : TValue) : TValue;
begin
  Result := ValueFromDouble(AsDouble(Value1) + AsDouble(Value2));
end;

function TDoubleType.Difference(Value1, Value2 : TValue) : TValue;
begin
  Result := ValueFromDouble(AsDouble(Value1) - AsDouble(Value2));
end;

function TDoubleType.Product(Value1, Value2 : TValue) : TValue;
begin
  Result := ValueFromDouble(AsDouble(Value1) * AsDouble(Value2));
end;

function TDoubleType.Quota(Value1, Value2 : TValue) : TValue;
begin
  try
    if not (AsDouble(Value2) = 0) then
      Result := ValueFromDouble(AsDouble(Value1) / AsDouble(Value2))
    else
      Result := DefaultValue;
  except
    Result := DefaultValue;
  end;
end;

function TDoubleType.NegateValue(Value : TValue) : TValue;
begin
  Result := ValueFromDouble(- AsDouble(Value));
end;



function TDoubleType.FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue;
begin
  if DataFormatter.IsKey then
    Result := ValueFromString( Trim(FormatFloat( DataFormatter.RemoveSign(DataFormatter.FormatMask, SysUtils.ThousandSeparator),
                                CDecimalRound( AsDouble(AValue), DataFormatter.DecimalFactor ))) )
  else
    try
      Result := ValueFromString( Trim(FormatFloat(DataFormatter.FormatMask, CDecimalRound( AsDouble(AValue) / DataFormatter.Divisor, DataFormatter.DecimalFactor ))) );
    except
      Result := AnyStringType.MakeValueNative(AValue);
    end;
end;

function TDoubleType.ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean;
var
  ValueAsDouble, DoublePart : Double;
  IntPart : Extended;
begin
  Result := True;
  try
    ValueAsDouble := StrToFloat( DataFormatter.RemoveThousandSeparator(AString) ) * DataFormatter.Divisor;
    if DataFormatter.DecimalCount >= 0 then
    begin
      IntPart := Trunc(ValueAsDouble);
      DoublePart := ValueAsDouble - IntPart;
      DoublePart := CDecimalRound(DoublePart, DataFormatter.DecimalFactor);

      ValueAsDouble := IntPart + DoublePart;
    end;

    ParsedValue := Value(ValueAsDouble);
  except on EConvertError do
    begin
      ParsedValue := AnyStringType.Value(AString);
      Result := False;
    end;
  end;
end;

class function TDoubleType.GetDataTypeWithProperties(
  AProperties: TStrings): TDataType;
begin
  Result := DoubleType;
end;

{ TPercentType }

constructor TPercentType.Create;
begin
  inherited Create(True);
end;

function TPercentType.GetDefaultDecimalCount : Integer;
begin
  Result := 1;
end;

function TPercentType.DefaultValue : TValue;
begin
  Result := Value(0.0);
end;

function TPercentType.Value(Val : Double) : TValue;
begin
  Result.DataType := Self;
  Result.sForInternalUseOnly := '';
  CopyMemory(@Result.CommonData, @Val, SizeOf(Val));
end;

function TPercentType.ConvertFromString(StrVal : String) : Double;
begin
  if Length(StrVal) = 0 then
  begin
    Result := 0.0;
    Exit;
  end;

  if StrVal[Length(StrVal)] = '%' then
    StrVal := Copy(StrVal, 1, Length(StrVal) -1);

  StrVal := Trim(StrVal);

  if Length(StrVal) = 0 then
    Result := 0.0
  else
    Result := StrToFloat(StrVal);
end;

function TPercentType.Compare(Value1, Value2 : TValue) : Integer;
var
  V1, V2 : Double;
begin
  V1 := AsPercent(Value1);
  V2 := AsPercent(Value2);

  if V1 = V2 then
    Result := 0
  else if V1 < V2 then
    Result := -1
  else
    Result := 1;
end;

function TPercentType.GetAsString(Value : TValue) : String;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsString(Value)
  else
    Result := FloatToStr(GetAsPercent(Value));
end;

function TPercentType.GetAsBoolean(Value : TValue) : Boolean;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsBoolean(Value)
  else
    raise ETypeConversionError.Create('Cannot convert Percent to Boolean!');
end;

function TPercentType.GetAsDateTime(Value : TValue) : TDateTime;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsDateTime(Value)
  else
    raise ETypeConversionError.Create('Cannot convert Percent to DateTime!');
end;

function TPercentType.GetAsPointer(Value : TValue) : Pointer;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsPointer(Value)
  else
    raise ETypeConversionError.Create('Cannot convert Percent to Pointer!');
end;

function TPercentType.GetAsDouble(Value : TValue) : Double;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsDouble(Value)
  else
    Result := GetAsPercent(Value);
end;

function TPercentType.GetAsInteger(Value : TValue) : Integer;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsInteger(Value)
  else
    raise ETypeConversionError.Create('Cannot convert Percent to Integer!');
end;

function TPercentType.GetAsCurrency(Value : TValue) : Currency;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsCurrency(Value)
  else
    Result := GetAsPercent(Value);
end;

function TPercentType.GetAsPercent(Value : TValue) : Double;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsPercent(Value)
  else
    CopyMemory(@Result, @Value.CommonData, SizeOf(Result));
end;

function TPercentType.MakeValueNative(Value : TValue) : TValue;
begin
  if Value.DataType = Self then
    Result := Value
  else
    Result := Self.Value(AsPercent(Value));
end;

function TPercentType.GetDataSize : Integer;
begin
  Result := SizeOf(Double);
end;

function TPercentType.GetDisplayWidth : Integer;
begin
  Result := 10;
end;

function TPercentType.GetDataTypeName : String;
begin
  Result := 'Percent';
end;

function TPercentType.GetSQLDataTypeString : String;
begin
  Result := 'float';
end;

function TPercentType.AsSQL(Value : TValue) : String;
begin
  Result := FloatToStr(AsPercent(Value));
end;

procedure TPercentType.StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer);
var
  PercVal : Double;
begin
  if Value.DataType = Self then
  begin
    CopyMemory(Address, @Value.CommonData, DataSize);
  end
  else
  begin
    PercVal := AsPercent(Value);
    CopyMemory(Address, @PercVal, DataSize);
  end;
end;



function TPercentType.GetDefaultAggregable : Boolean;
begin
  Result := False;
end;

function TPercentType.Sum(Value1, Value2 : TValue) : TValue;
begin
  Result := ValueFromPercent(AsPercent(Value1) + AsPercent(Value2));
end;

function TPercentType.Difference(Value1, Value2 : TValue) : TValue;
begin
  Result := ValueFromPercent(AsPercent(Value1) - AsPercent(Value2));
end;

function TPercentType.Product(Value1, Value2 : TValue) : TValue;
begin
  Result := ValueFromPercent(AsPercent(Value1) * AsPercent(Value2));
end;

function TPercentType.Quota(Value1, Value2 : TValue) : TValue;
begin
  Result := ValueFromPercent(AsDouble(Value1) / AsDouble(Value2));
end;

function TPercentType.NegateValue(Value : TValue) : TValue;
begin
  Result := ValueFromPercent(- AsPercent(Value));
end;



function TPercentType.FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue;
var
  PctValue : Double;
begin
  if AValue.DataType.IsNumeric then
  begin
    PctValue := 100.0 * AsPercent(AValue);

    if DataFormatter.IsKey then
      Result := ValueFromString( Trim(FormatFloat( DataFormatter.RemoveSign(DataFormatter.FormatMask + ' %', SysUtils.ThousandSeparator),
                                  CDecimalRound( PctValue, DataFormatter.DecimalFactor ))) )
    else
      try
        Result := ValueFromString( Trim(FormatFloat(DataFormatter.FormatMask + ' %',
                                    CDecimalRound( PctValue / DataFormatter.Divisor, DataFormatter.DecimalFactor ))) );
      except
        Result := AnyStringType.MakeValueNative(AValue);
      end;
  end    
  else
    Result := AnyStringType.MakeValueNative(AValue);
end;

function TPercentType.ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean;
var
  ValueAsDouble, DoublePart : Double;
  IntPart : Extended;
begin
  Result := True;
  try
    ValueAsDouble := ConvertFromString( DataFormatter.RemoveThousandSeparator(AString) ) * DataFormatter.Divisor;

    if DataFormatter.DecimalCount >= 0 then
    begin
      IntPart := Trunc(ValueAsDouble);
      DoublePart := ValueAsDouble - IntPart;
      DoublePart := CDecimalRound(DoublePart, DataFormatter.DecimalFactor);

      ValueAsDouble := IntPart + DoublePart;
    end;

    ParsedValue := Value(ValueAsDouble/100.0);
  except on EConvertError do
    begin
      ParsedValue := AnyStringType.Value(AString);
      Result := False;
    end;
  end;
end;

class function TPercentType.GetDataTypeWithProperties(
  AProperties: TStrings): TDataType;
begin
  Result := PercentType;
end;

function TPercentType.GenerateError(InvalidValue: String): String;
begin
  Result := TranslateMessage(E_ValueInvalidPercent, [InvalidValue]);
end;

{ TIntegerType }

constructor TIntegerType.Create;
begin
  inherited Create(False);
end;

function TIntegerType.DefaultValue : TValue;
begin
  Result := Value(0);
end;

function TIntegerType.Value(Val : Integer) : TValue;
begin
  Result.DataType := Self;
  Result.sForInternalUseOnly := '';
  CopyMemory(@Result.CommonData, @Val, SizeOf(Val));
end;

function TIntegerType.ConvertFromString(StrVal : String) : Integer;
begin
  StrVal := Trim(StrVal);

  if Length(StrVal) = 0 then
    Result := 0
  else
    Result := StrToInt(StrVal);
end;

function TIntegerType.Compare(Value1, Value2 : TValue) : Integer;
var
  V1, V2 : Integer;
begin
  V1 := AsInteger(Value1);
  V2 := AsInteger(Value2);

  Result := V1 - V2;
end;

function TIntegerType.GetAsString(Value : TValue) : String;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsString(Value)
  else
    Result := IntToStr(GetAsInteger(Value));
end;

function TIntegerType.GetAsBoolean(Value : TValue) : Boolean;
var
  IVal : Integer;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsBoolean(Value)
  else
  begin
    IVal := GetAsInteger(Value);
    Result := IVal <> 0;
  end;
end;

function TIntegerType.GetAsDateTime(Value : TValue) : TDateTime;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsDateTime(Value)
  else
    Result := GetAsInteger(Value);
end;

function TIntegerType.GetAsPointer(Value : TValue) : Pointer;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsPointer(Value)
  else
    Result := Pointer(GetAsInteger(Value));
end;

function TIntegerType.GetAsDouble(Value : TValue) : Double;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsDouble(Value)
  else
    Result := GetAsInteger(Value);
end;

function TIntegerType.GetAsInteger(Value : TValue) : Integer;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsInteger(Value)
  else
    CopyMemory(@Result, @Value.CommonData, SizeOf(Result));
end;

function TIntegerType.GetAsCurrency(Value : TValue) : Currency;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsCurrency(Value)
  else
    Result := GetAsInteger(Value);
end;

function TIntegerType.GetAsPercent(Value : TValue) : Double;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsPercent(Value)
  else
    Result := GetAsInteger(Value);
end;

function TIntegerType.MakeValueNative(Value : TValue) : TValue;
begin
  if Value.DataType = Self then
    Result := Value
  else
    Result := Self.Value(AsInteger(Value));
end;

function TIntegerType.GetDataSize : Integer;
begin
  Result := SizeOf(Integer);
end;

function TIntegerType.GetDisplayWidth : Integer;
begin
  Result := 12;
end;

function TIntegerType.GetDataTypeName : String;
begin
  Result := 'Integer';
end;

function TIntegerType.GetSQLDataTypeString : String;
begin
  Result := 'int';
end;

function TIntegerType.AsSQL(Value : TValue) : String;
begin
  Result := AsString(Value);
end;

procedure TIntegerType.StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer);
var
  IntVal : Integer;
begin
  if Value.DataType = Self then
  begin
    CopyMemory(Address, @Value.CommonData, DataSize);
  end
  else
  begin
    IntVal := AsInteger(Value);
    CopyMemory(Address, @IntVal, DataSize);
  end;
end;



function TIntegerType.Sum(Value1, Value2 : TValue) : TValue;
begin
  Result := ValueFromInteger(AsInteger(Value1) + AsInteger(Value2));
end;

function TIntegerType.Difference(Value1, Value2 : TValue) : TValue;
begin
  Result := ValueFromInteger(AsInteger(Value1) - AsInteger(Value2));
end;

function TIntegerType.Product(Value1, Value2 : TValue) : TValue;
begin
  Result := ValueFromInteger(AsInteger(Value1) * AsInteger(Value2));
end;

function TIntegerType.Quota(Value1, Value2 : TValue) : TValue;
begin
  Result := ValueFromInteger(AsInteger(Value1) div AsInteger(Value2));
end;

function TIntegerType.NegateValue(Value : TValue) : TValue;
begin
  Result := ValueFromInteger(- AsInteger(Value));
end;



function TIntegerType.FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue;
begin
  Result := AnyStringType.MakeValueNative( ValueFromInteger( CRound(AsInteger(AValue) / DataFormatter.Divisor) ) );
end;

function TIntegerType.ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean;
var
  ValueAsDouble : Double;
begin
  Result := True;
  try
    ValueAsDouble := StrToFloat( DataFormatter.RemoveThousandSeparator(AString) );
    ParsedValue := Value( CRound(ValueAsDouble * DataFormatter.Divisor) );
  except on EConvertError do
    begin
      ParsedValue := AnyStringType.Value(AString);
      Result := False;
    end;
  end;
end;

class function TIntegerType.GetDataTypeWithProperties(
  AProperties: TStrings): TDataType;
begin
  Result := IntegerType;
end;

function TIntegerType.GenerateError(InvalidValue: String): String;
begin
  Result := TranslateMessage(E_ValueInvalidInteger, [InvalidValue]);
end;

{ TDateTimeType }

function TDateTimeType.ValueBlank(Value : TValue) : Boolean;
begin
  Result := Self.Equals(Value, ValueFromDate(0.0));
end;

constructor TDateTimeType.Create;
begin
  inherited Create(True);
end;

function TDateTimeType.DefaultValue : TValue;
begin
  Result := Value(Now);
end;

function TDateTimeType.Value(Val : TDateTime) : TValue;
begin
  Result.DataType := Self;
  Result.sForInternalUseOnly := '';
  CopyMemory(@Result.CommonData, @Val, SizeOf(Val));
end;

function TDateTimeType.ConvertFromString(StrVal : String) : TDateTime;
begin
  StrVal := Trim(StrVal);
  if Length(StrVal) = 0 then
    Result := 0.0
  else
    // MVJ 19.3.2003: Catching an exception here doesn't let higher levels
    // in the application know that the given value was illegal!
    Result := StrToDateTime(StrVal);

  (*
    try
      Result := StrToDateTime(StrVal);
    except
      Result := 0.0;
    end;
*)
end;

function TDateTimeType.GetAsString(Value : TValue) : String;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsString(Value)
  else
    Result := DateTimeToStr(GetAsDateTime(Value));
end;

function TDateTimeType.GetAsBoolean(Value : TValue) : Boolean;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsBoolean(Value)
  else
    raise ETypeConversionError.Create('Cannot convert DateTime to Boolean!');
end;

function TDateTimeType.GetAsDateTime(Value : TValue) : TDateTime;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsDateTime(Value)
  else
    CopyMemory(@Result, @Value.CommonData, SizeOf(Result));
end;

function TDateTimeType.GetAsPointer(Value : TValue) : Pointer;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsPointer(Value)
  else
    raise ETypeConversionError.Create('Cannot convert DateTime to Pointer!');
end;

function TDateTimeType.GetAsDouble(Value : TValue) : Double;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsDouble(Value)
  else
    Result := GetAsDateTime(Value);
end;

function TDateTimeType.Compare(Value1, Value2 : TValue) : Integer;
var
  V1, V2 : TDateTime;
begin
  V1 := AsDateTime(Value1);
  V2 := AsDateTime(Value2);

  if V1 = V2 then
    Result := 0
  else if V1 < V2 then
    Result := -1
  else
    Result := 1;
end;

function TDateTimeType.GetAlignment : TAlignment;
begin
  Result := taRightJustify;
end;

function TDateTimeType.GetDataSize : Integer;
begin
  Result := SizeOf(TDateTime);
end;

function TDateTimeType.GetDisplayWidth : Integer;
begin
  Result := 20;
end;

function TDateTimeType.GetDataTypeName : String;
begin
  Result := 'DateTime';
end;

function TDateTimeType.GetSQLDataTypeString : String;
begin
  Result := 'datetime';
end;

function TDateTimeType.MakeValueNative(Value : TValue) : TValue;
begin
  if Value.DataType = Self then
    Result := Value
  else
    Result := Self.Value(AsDateTime(Value));
end;

function TDateTimeType.AsSQL(Value : TValue) : String;
var
  TimeVal : TDateTime;
  Hour, Min, Sec, MSec : Word;
begin
  try
    TimeVal := AsTime(Value);
    DecodeTime(TimeVal, Hour, Min, Sec, MSec);
    Result := PadString(IntToStr(Hour), '0', 2) + ':' +
              PadString(IntToStr(Min), '0', 2) + ':' +
              PadString(IntToStr(Sec), '0', 2) + '.' +
              PadString(IntToStr(MSec), '0', 4);
    Result := QuotedStr(DateType.AsSQL(Value) + ' ' + Result);
  except
    Result := QuotedStr(AsString(Value));
  end;
end;

procedure TDateTimeType.StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer);
var
  DTVal : TDateTime;
begin
  if Value.DataType = Self then
  begin
    CopyMemory(Address, @Value.CommonData, DataSize);
  end
  else
  begin
    DTVal := AsDateTime(Value);
    CopyMemory(Address, @DTVal, DataSize);
  end;
end;





function TDateTimeType.Sum(Value1, Value2 : TValue) : TValue;
begin
  Result := Value(AsDateTime(Value1) + AsDateTime(Value2));
end;

function TDateTimeType.Difference(Value1, Value2 : TValue) : TValue;
begin
  Result := Value(AsDateTime(Value1) - AsDateTime(Value2));
end;

function TDateTimeType.Product(Value1, Value2 : TValue) : TValue;
begin
  raise Exception.Create('TDateTimeType.Quota: Operation not supported!');
end;

function TDateTimeType.Quota(Value1, Value2 : TValue) : TValue;
begin
  raise Exception.Create('TDateTimeType.Quota: Operation not supported!');
end;

function TDateTimeType.NegateValue(Value : TValue) : TValue;
begin
  Result := Self.Value(- AsDateTime(Value));
end;

function TDateTimeType.FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue;
begin
  Result := AnyStringType.MakeValueNative(AValue);
end;

function TDateTimeType.ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean;
begin
  Result := True;
  ParsedValue := MakeValueNative( AnyStringType.Value(AString) );
end;

class function TDateTimeType.GetDataTypeWithProperties(
  AProperties: TStrings): TDataType;
begin
  Result := DateTimeType;
end;

function TDateTimeType.GenerateError(InvalidValue: String): String;
begin
  Result := TranslateMessage(E_ValueInvalidDate, [InvalidValue]);
end;

{ TDateType }

constructor TDateType.Create;
begin
  inherited Create;
end;

function TDateType.DefaultValue : TValue;
begin
  Result := Value(Now);
end;

function TDateType.Value(Val : TDateTime) : TValue;
begin
  Val := Trunc(Val);

  Result.DataType := Self;
  Result.sForInternalUseOnly := '';
  CopyMemory(@Result.CommonData, @Val, SizeOf(Val));
end;

function TDateType.ConvertFromString(StrVal : String) : TDateTime;
begin
  StrVal := Trim(StrVal);
  Result := StrToDate(StrVal);
end;

function TDateType.GetAsString(Value : TValue) : String;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsString(Value)
  else
    Result := DateTimeToStr(GetAsDate(Value));
end;

function TDateType.GetSQLDataTypeString : String;
begin
  Result := 'datetime';
end;

function TDateType.AsSQL(Value : TValue) : String;
var
  DateVal : TDateTime;
  Year, Month, Day : Word;
begin
  try
    DateVal := AsDate(Value);
    DecodeDate(DateVal, Year, Month, Day);
    Result := PadString(IntToStr(Year), '0', 4) + '-' +
              PadString(IntToStr(Month), '0', 2) + '-' +
              PadString(IntToStr(Day), '0', 2);
    Result := QuotedStr(Result);
  except
    Result := QuotedStr(AsString(Value));
  end;
end;

procedure TDateType.StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer);
var
  DateVal : TDateTime;
begin
  if Value.DataType = Self then
  begin
    CopyMemory(Address, @Value.CommonData, DataSize);
  end
  else
  begin
    DateVal := AsDate(Value);
    CopyMemory(Address, @DateVal, DataSize);
  end;
end;



function TDateType.GetDisplayWidth : Integer;
begin
  Result := 10;
end;

function TDateType.GetDataTypeName : String;
begin
  Result := 'Date';
end;

function TDateType.MakeValueNative(Value : TValue) : TValue;
begin
  if Value.DataType = Self then
    Result := Value
  else
    Result := Self.Value(AsDate(Value));
end;

function TDateType.FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue;
begin
  Result := AnyStringType.MakeValueNative(AValue);
end;

function TDateType.ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean;
begin
  Result := True;
  ParsedValue := MakeValueNative( AnyStringType.Value(AString) );
end;

class function TDateType.GetDataTypeWithProperties(
  AProperties: TStrings): TDataType;
begin
  Result := DateType;
end;

{ TTimeType }

constructor TTimeType.Create;
begin
  inherited Create;
end;

function TTimeType.DefaultValue : TValue;
begin
  Result := Value(0.0);
end;

function TTimeType.ConvertFromString(StrVal : String) : TDateTime;
begin
  StrVal := Trim(StrVal);

  if Length(StrVal) = 0 then
    Result := 0.0
  else
    Result := StrToTime(StrVal);
end;

function TTimeType.GetAsString(Value : TValue) : String;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsString(Value)
  else
    Result := TimeToStr(GetAsTime(Value));
end;

function TTimeType.GetSQLDataTypeString : String;
begin
  Result := 'float';
end;

function TTimeType.AsSQL(Value : TValue) : String;
begin
  Result := FloatToStr(AsTime(Value));
end;

procedure TTimeType.StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer);
var
  TimeVal : TDateTime;
begin
  if Value.DataType = Self then
  begin
    CopyMemory(Address, @Value.CommonData, DataSize);
  end
  else
  begin
    TimeVal := AsTime(Value);
    CopyMemory(Address, @TimeVal, DataSize);
  end;
end;





function TTimeType.GetDefaultAggregable : Boolean;
begin
  Result := True;
end;

function TTimeType.GetIsNumeric : Boolean;
begin
  Result := True;
end;

function TTimeType.GetDisplayWidth : Integer;
begin
  Result := 10;
end;

function TTimeType.GetDataTypeName : String;
begin
  Result := 'Time';
end;

function TTimeType.MakeValueNative(Value : TValue) : TValue;
begin
  if Value.DataType = Self then
    Result := Value
  else
    Result := Self.Value(AsTime(Value));
end;

function TTimeType.FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue;
begin
  Result := AnyStringType.MakeValueNative(AValue);
end;

function TTimeType.ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean;
begin
  Result := True;
  ParsedValue := MakeValueNative( AnyStringType.Value(AString) );
end;

class function TTimeType.GetDataTypeWithProperties(
  AProperties: TStrings): TDataType;
begin
  Result := TimeType;
end;

function TTimeType.GenerateError(InvalidValue: String): String;
begin
  Result := TranslateMessage(E_ValueInvalidTime, [InvalidValue]);
end;

{ TPointerType }

constructor TPointerType.Create;
begin
  inherited Create(True);
end;

function TPointerType.DefaultValue : TValue;
begin
  Result := Value(nil);
end;

function TPointerType.Value(Val : Pointer) : TValue;
begin
  Result.DataType := Self;
  Result.sForInternalUseOnly := '';
  CopyMemory(@Result.CommonData, @Val, SizeOf(Val));
end;

function TPointerType.Compare(Value1, Value2 : TValue) : Integer;
var
  V1, V2 : Pointer;
begin
  V1 := AsPointer(Value1);
  V2 := AsPointer(Value2);

  if V1 = V2 then
    Result := 0
  else if Integer(V1) < Integer(V2) then
    Result := -1
  else
    Result := 1;
end;

function TPointerType.GetAsString(Value : TValue) : String;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsString(Value)
  else
    Result := 'Pointer($' + IntToHex(Integer(AsPointer(Value)), 8) + ')';
end;

function TPointerType.GetAsBoolean(Value : TValue) : Boolean;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsBoolean(Value)
  else
    raise ETypeConversionError.Create('Cannot convert Pointer to Boolean!');
end;

function TPointerType.GetAsDateTime(Value : TValue) : TDateTime;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsDateTime(Value)
  else
    raise ETypeConversionError.Create('Cannot convert Pointer to DateTime!');
end;

function TPointerType.GetAsPointer(Value : TValue) : Pointer;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsPointer(Value)
  else
    CopyMemory(@Result, @Value.CommonData, SizeOf(Result));
end;

function TPointerType.GetAsDouble(Value : TValue) : Double;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsDouble(Value)
  else
    raise ETypeConversionError.Create('Cannot convert Pointer to Double!');
end;

function TPointerType.GetAsInteger(Value : TValue) : Integer;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsInteger(Value)
  else
    Result := Integer(GetAsPointer(Value));
end;

function TPointerType.MakeValueNative(Value : TValue) : TValue;
begin
  if Value.DataType = Self then
    Result := Value
  else
    Result := Self.Value(AsPointer(Value));
end;

function TPointerType.GetDataSize : Integer;
begin
  Result := SizeOf(Pointer);
end;

function TPointerType.GetDisplayWidth : Integer;
begin
  Result := 0;
end;

function TPointerType.GetDataTypeName : String;
begin
  Result := 'Pointer';
end;

function TPointerType.GetSQLDataTypeString : String;
begin
  Result := 'pointer';
end;

function TPointerType.GetCanBeInDB : Boolean;
begin
  Result := False;
end;

function TPointerType.AsSQL(Value : TValue) : String;
begin
  Result := QuotedStr('');
end;

procedure TPointerType.StoreValue(Address : Pointer; Value : TValue; SetSpecialValue : TSetSpecialValueProc; SpecialValueIndex : Integer);
var
  PVal : Pointer;
begin
  if Value.DataType = Self then
  begin
    CopyMemory(Address, @Value.CommonData, DataSize);
  end
  else
  begin
    PVal := AsPointer(Value);
    CopyMemory(Address, @PVal, DataSize);
  end;
end;





function TPointerType.ValueBlank(Value : TValue) : Boolean;
begin
  try
    Result := (AsPointer(Value) = nil);
  except
    Result := False;
  end;
end;

function TPointerType.Sum(Value1, Value2 : TValue) : TValue;
begin
  raise Exception.Create('TPointerType.Sum: Operation not supported!');
end;

function TPointerType.Difference(Value1, Value2 : TValue) : TValue;
begin
  raise Exception.Create('TPointerType.Difference: Operation not supported!');
end;

function TPointerType.Product(Value1, Value2 : TValue) : TValue;
begin
  raise Exception.Create('TPointerType.Product: Operation not supported!');
end;

function TPointerType.Quota(Value1, Value2 : TValue) : TValue;
begin
  raise Exception.Create('TPointerType.Quota: Operation not supported!');
end;

function TPointerType.NegateValue(Value : TValue) : TValue;
begin
  Result := Value;
end;

function TPointerType.FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue;
begin
  Result := AValue;
end;

function TPointerType.ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean;
begin
  Result := True;
  ParsedValue := MakeValueNative( AnyStringType.Value(AString) );
end;

class function TPointerType.GetDataTypeWithProperties(
  AProperties: TStrings): TDataType;
begin
  Result := PointerType;
end;

{ TObjectType }

constructor TObjectType.Create;
begin
  inherited Create;
end;

function TObjectType.Value(Val : TObject) : TValue;
begin
  Result.DataType := Self;
  Result.sForInternalUseOnly := '';
  CopyMemory(@Result.CommonData, @Val, SizeOf(Val));
end;

function TObjectType.GetAsString(Value : TValue) : String;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsString(Value)
  else
    Result := 'TObject($' + IntToHex(Integer(AsObject(Value)), 8) + ')';
end;

function TObjectType.GetAsPointer(Value : TValue) : Pointer;
begin
  if Value.DataType <> Self then
    Result := Value.DataType.GetAsPointer(Value)
  else
    CopyMemory(@Result, @Value.CommonData, SizeOf(Result));
end;

function TObjectType.GetDataSize : Integer;
begin
  Result := SizeOf(TObject);
end;

function TObjectType.GetDisplayWidth : Integer;
begin
  Result := 0;
end;

function TObjectType.GetDataTypeName : String;
begin
  Result := 'Object';
end;

function TObjectType.FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue;
begin
  Result := AValue;
end;

function TObjectType.ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean;
begin
  Result := True;
  ParsedValue := MakeValueNative( AnyStringType.Value(AString) );
end;

class function TObjectType.GetDataTypeWithProperties(
  AProperties: TStrings): TDataType;
begin
  Result := ObjectType;
end;

function TObjectType.GetAlignment: TAlignment;
begin
  Result := taCenter;
end;

{ TValueList }

constructor TValueList.Create(DataType : TDataType);
begin
  inherited Create;
  FDataType := DataType;
end;

constructor TValueList.CreateAnyType;
begin
  Create(nil);
end;

destructor TValueList.Destroy;
begin
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TValueList.AddVal(V : TValue): Integer;
begin
  Result := AddValue('', V, nil);
end;

function TValueList.AddValue(const S: string; const V: TValue; const O: TObject): Integer;
var
  Val : TValue;
begin
  if DataType <> nil then
    Val := DataType.Optimize(V)
  else
    Val := V;

  if not Sorted then
    Result := FCount
  else
  begin
    if Find(Val, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error({$ifdef USE_RESOURCESTRINGS}SDuplicateString{$else}'SDuplicateString'{$endif LINUX}, 0);
      end;
  end;
  InsertValueItem(Result, S, Val);
  PutObject(Result, O);
end;

function TValueList.Add(const S: string): Integer;
var
  Value : TValue;
begin
  Value := ValueFromString(S);

  if not Sorted then
    Result := FCount
  else
  begin
    if Find(Value, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error({$ifdef USE_RESOURCESTRINGS}SDuplicateString{$else}'SDuplicateString'{$endif LINUX}, 0);
      end;
  end;

  InsertValueItem(Result, S, Value);
end;

procedure TValueList.Clear;
begin
  if FCount <> 0 then
  begin
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
  end;
end;

procedure TValueList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}SListIndexError{$else}'SListIndexError'{$endif LINUX}, Index);
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TValueItem));
end;

procedure TValueList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}SListIndexError{$else}'SListIndexError'{$endif LINUX}, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}SListIndexError{$else}'SListIndexError'{$endif LINUX}, Index2);
  ExchangeItems(Index1, Index2);
end;

procedure TValueList.ExchangeItems(Index1, Index2: Integer);
var
  TempValueItemBytes : ValueItemBytes;
(*  Temp: Integer;
  TempBytes : Byte8; *)
  Item1, Item2: PValueItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];

  TempValueItemBytes := ValueItemBytes(Item1^);
  ValueItemBytes(Item1^) := ValueItemBytes(Item2^);
  ValueItemBytes(Item2^) := TempValueItemBytes;
end;

function TValueList.Find(const V: TValue; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  if not Sorted then
    raise Exception.Create(Self.ClassName + '.Find: Shouldn''t use Find if list isn''t sorted!');

  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := DataType.Compare(Values[I], V);
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

function TValueList.Get(Index: Integer): string;
begin
  if (Index < 0) or (Index >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS}, Index);
  Result := FList^[Index].FString;
end;

function TValueList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TValueList.GetCount: Integer;
begin
  Result := FCount;
end;

function TValueList.GetValue(Index: Integer): TValue;
begin
  if (Index < 0) or (Index >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}SListIndexError{$else}'SListIndexError'{$endif LINUX}, Index);
  Result := FList^[Index].FValue;
end;

function TValueList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}SListIndexError{$else}'SListIndexError'{$endif LINUX}, Index);
  Result := FList^[Index].FObject;
end;

procedure TValueList.Grow;
var
  NewSize: Integer;
begin
  if FCapacity < 16 then
    NewSize := 16
  else
    NewSize := FCapacity * 2;

  SetCapacity(NewSize);

(*  if FCapacity > 64 then
    Delta := FCapacity div 4
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  SetCapacity(FCapacity + Delta); *)
end;

function TValueList.IndexOfValue(const V: TValue): Integer;
var
  Val : TValue;
begin
  if DataType <> nil then
    Val := DataType.Optimize(V)
  else
    Val := V;

  if not Sorted then
    Result := SearchValue(Val)
  else if not Find(Val, Result) then
    Result := -1;
end;

function TValueList.SearchValue(const V: TValue) : Integer;
var
  SearchDataType : TDataType;
begin
  if DataType = nil then
    SearchDataType := V.DataType
  else
    SearchDataType := DataType;

  for Result := 0 to GetCount - 1 do
    if SearchDataType.Compare(GetValue(Result), V) = 0 then
      Exit;

  Result := -1;
end;

procedure TValueList.Insert(Index: Integer; const S: string);
begin
  if Sorted then Error({$ifdef USE_RESOURCESTRINGS}SSortedListError{$else}'SSortedListError'{$endif USE_RESOURCESTRINGS}, 0);
  if (Index < 0) or (Index > FCount) then Error({$ifdef USE_RESOURCESTRINGS}SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS}, Index);
  InsertValueItem(Index, S, ValueFromString(S));
end;

procedure TValueList.InsertValueItem(Index: Integer; const S: string; const V: TValue);
var
  Item : PValueItem;
  Val : TValue;
begin
  if DataType <> nil then
    Val := DataType.Optimize(V)
  else
    Val := V;

  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TValueItem));

  Item := @FList^[Index];

  Pointer(Item^.FString) := nil;
  Item^.FString := S;
  Item^.FObject := nil;
  ValueBytes(Item^.FValue) := ValueBytes(Val);

  // kluderi
  Pointer(Item^.FValue.sForInternalUseOnly) := nil;
  Item^.FValue.sForInternalUseOnly := Val.sForInternalUseOnly;

  Inc(FCount);
end;

procedure TValueList.Put(Index: Integer; const S: string);
begin
  if (Index < 0) or (Index >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS}, Index);
  FList^[Index].FString := S;
end;

procedure TValueList.PutValue(Index: Integer; const AValue: TValue);
var
  Val : TValue;
begin
  if DataType <> nil then
    Val := DataType.Optimize(AValue)
  else
    Val := AValue;

  if Sorted then Error({$ifdef USE_RESOURCESTRINGS}SSortedListError{$else}'SSortedListError'{$endif USE_RESOURCESTRINGS}, 0);
  if (Index < 0) or (Index >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS}, Index);
  FList^[Index].FValue := AValue;
end;

procedure TValueList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS}, Index);
  FList^[Index].FObject := AObject;
end;

procedure TValueList.QuickSort(L, R: Integer);
var
  I, J: Integer;
  V: TValue;
begin
  repeat
    I := L;
    J := R;
    V := Values[(L + R) shr 1];
    repeat
      while DataType.Compare(Values[I], V) < 0 do Inc(I);
      while DataType.Compare(Values[J], V) > 0 do Dec(J);
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

procedure TValueList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TValueItem));
  FCapacity := NewCapacity;
end;

procedure TValueList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value and (DataType = nil) then
      raise Exception.Create('Cannot sort a list with unspecified datatype!');

    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TValueList.Sort;
begin
  if not Sorted and (FCount > 1) then
  begin
    QuickSort(0, FCount - 1);
  end;
end;

procedure TValueList.Assign(Source: TPersistent);
var
  i : Integer;
begin
  if Source = nil then
  begin
    Clear;
  end
  else if Source is TValueList then
  begin
    BeginUpdate;
    try
      Clear;

      for i := 0 to TValueList(Source).Count - 1 do
        AddValue(TValueList(Source).Strings[i], TValueList(Source).Values[i], TValueList(Source).Objects[i]);
    finally
      EndUpdate;
    end;
    Exit;
  end
  else
    inherited Assign(Source);
end;


{ TDataFormatter }

constructor TDataFormatter.Create;
begin
  Inherited Create;

  FDefaultFormatMask := ',0';
  FDecimalCount := 0;
  FDivisor := 1.0;
  FIsKey := False;
end;

(**
   MinCount is the minimum amount of decimals to be shown and MaxCount the
   maximal amount of decimals to show. A negative number means that we don't
   care how many that are shown, so as many as possible will be shown.
   *)
procedure TDataFormatter.SetDecimalCount(MaxCount, MinCount : Integer);
var
  i : Integer;
begin
  if MaxCount >= 0 then
  begin
    MinCount := Min(MinCount, MaxCount);
    FDecimalFactor := Round( IntPower( 10, MaxCount ) );
  end
  else
  begin
    MaxCount := 15;
    FDecimalFactor := 0;
  end;

(*  if MinCount < 0 then
    MinCount := MaxCount;
*)
  FDecimalCount := MaxCount;

  if MaxCount = 0 then
    FFormatMask := FDefaultFormatMask
  else if MaxCount > 0 then
    FFormatMask := FDefaultFormatMask + '.';

  for i := 1 to MinCount do
    FFormatMask := FFormatMask + '0';

  for i := MinCount + 1 to MaxCount do
    FFormatMask := FFormatMask + '#';

(*
var
  i : Integer;
begin
  if Value >= 0 then
    FDecimalFactor := Round( IntPower( 10, Value ) )
  else
    FDecimalFactor := 1;

  if Value >= 0 then
    AlwaysShow := Min( Value, AlwaysShow )
  else
    Value := 15;

  if AlwaysShow < 0 then
    AlwaysShow := Value;

  FDecimalCount := Value;

  if Value = 0 then
    FFormatMask := FDefaultFormatMask
  else if Value > 0 then
    FFormatMask := FDefaultFormatMask + '.';

  for i := 1 to AlwaysShow do
    FFormatMask := FFormatMask + '0';

  for i := AlwaysShow + 1 to Value do
    FFormatMask := FFormatMask + '#';

(*
  for i := 0 to FDecimalCount -1 do
    FDecimalFactor := FDecimalFactor * 10;
    *)
end;

class function TDataFormatter.RemoveThousandSeparator(Str : String) : String;
begin
  Str := RemoveSign(Str, ' ');
  Str := RemoveSign(Str, SysUtils.ThousandSeparator);
  Result := Str;
end;

class function TDataFormatter.RemoveSign(Str, Sign : String) : String;
var
  idx : Integer;
begin
  repeat
    idx := Pos(Sign, Str);
    if idx <> 0 then
      Delete(Str, idx, 1);
  until idx <= 0;

  Result := Str;
end;

{ TValue manipulating methods }

function AsString(Value : TValue) : String;
begin
  Result := Value.DataType.GetAsString(Value);
end;

function AsMemo(Value : TValue) : String;
begin
  Result := AsString(Value);
end;

function AsBoolean(Value : TValue) : Boolean;
begin
  Result := Value.DataType.GetAsBoolean(Value);
end;

function AsDateTime(Value : TValue) : TDateTime;
begin
  Result := Value.DataType.GetAsDateTime(Value);
end;

function AsDate(Value : TValue) : TDateTime;
begin
  Result := Value.DataType.GetAsDate(Value);
end;

function AsTime(Value : TValue) : TDateTime;
begin
  Result := Value.DataType.GetAsTime(Value);
end;

function AsPointer(Value : TValue) : Pointer;
begin
  Result := Value.DataType.GetAsPointer(Value);
end;

function AsObject(Value : TValue) : TObject;
begin
  Result := Value.DataType.GetAsObject(Value);
end;

function AsDouble(Value : TValue) : Double;
begin
  Result := Value.DataType.GetAsDouble(Value);
end;

function AsInteger(Value : TValue) : Integer;
begin
  Result := Value.DataType.GetAsInteger(Value);
end;

function AsCurrency(Value : TValue) : Currency;
begin
  Result := Value.DataType.GetAsCurrency(Value);
end;

function AsPercent(Value : TValue) : Double;
begin
  Result := Value.DataType.GetAsPercent(Value);
end;

function ValueFromString(Val : String) : TValue;
begin
  Result := StringType(Length(Val), True).Value(Val);
end;

function ValueFromMemo(Val : String) : TValue;
begin
  Result := ValueFromString(Val);
end;

function ValueFromMemoCS(Val : String) : TValue;
begin
  Result := MemoTypeCS.Value(Val);
end;

function ValueFromBoolean(Val : Boolean) : TValue;
begin
  Result := BooleanType.Value(Val);
end;

function ValueFromDateTime(Val : TDateTime) : TValue;
begin
  Result := DateTimeType.Value(Val);
end;

function ValueFromDate(Val : TDateTime) : TValue;
begin
  Result := DateType.Value(Val);
end;

function ValueFromTime(Val : TDateTime) : TValue;
begin
  Result := TimeType.Value(Val);
end;

function ValueFromPointer(Val : Pointer) : TValue;
begin
  Result := PointerType.Value(Val);
end;

function ValueFromObject(Val : TObject) : TValue;
begin
  Result := ObjectType.Value(Val);
end;

function ValueFromDouble(Val : Double) : TValue;
begin
  Result := DoubleType.Value(Val);
end;

function ValueFromInteger(Val : Integer) : TValue;
begin
  Result := IntegerType.Value(Val);
end;

function ValueFromCurrency(Val : Currency) : TValue;
begin
  Result := CurrencyType.Value(Val);
end;

function ValueFromPercent(Val : Double) : TValue;
begin
  Result := PercentType.Value(Val);
end;

{ UndefinedTypeList methods }

function UndefinedType(Size : Integer = -1; DynamicSize : Boolean = False) : TUndefinedType;
var
  idx : Integer;
  sIndex : String;
begin
  sIndex := BoolToStr(DynamicSize) + IntToStr(Size);

  if FUndefinedTypeList.Find(sIndex, idx) then
    Result := FUndefinedTypeList.Objects[idx] as TUndefinedType
  else
  begin
    Result := TUndefinedType.Create(Size, DynamicSize);
    FUndefinedTypeList.AddObject(sIndex, Result);
  end;
end;

{ StringTypeList methods }

function StringType(Size : Integer; CaseSensitive : Boolean) : TStringType;
const
  MAXUNPACKED = 1024;
var
  List : TList;
  i : Integer;
begin
  if FCaseSensitiveList = nil then // LGE: Delphi puckar sig på någå vis och kör ej initialize i tid...
    DoInitialize;

  if CaseSensitive then
    List := FCaseSensitiveList
  else
    List := FCaseInSensitiveList;

  if Size <= MAXUNPACKED then
  begin
    if Size < List.Count then
    begin
      if List.Items[Size] = nil then
        List.Items[Size] := TStringType.Create(Size, CaseSensitive);
    end
    else
    begin
      for i := List.Count to Size - 1 do
        List.Add(nil);
      List.Add(TStringType.Create(Size, CaseSensitive));
    end;

    Result := List.Items[Size];
  end
  else
  begin
    for i := MAXUNPACKED + 1 to List.Count - 1 do
      if TStringType(List.Items[i]).Size = Size then
      begin
        Result := List.Items[i];
        Exit;
      end;

    for i := List.Count to MAXUNPACKED do
      List.Add(nil);

    Result := TStringType.Create(Size, CaseSensitive);
    List.Add(Result);
  end;
end;

procedure FreeUndefinedTypeLists;
var
  i : Integer;
begin
  for i := 0 to FUndefinedTypeList.Count -1 do
    FUndefinedTypeList.Objects[i].Free;
  FreeAndNil(FUndefinedTypeList);
end;

procedure FreeStringTypeLists;
var
  i : Integer;
begin
  for i := 0 to FCaseSensitiveList.Count - 1 do
    TObject(FCaseSensitiveList.Items[i]).Free;
  FCaseSensitiveList.Free;

  for i := 0 to FCaseInSensitiveList.Count - 1 do
    TObject(FCaseInSensitiveList.Items[i]).Free;
  FCaseInSensitiveList.Free;
end;

function ValueIsDefined(AValue : TValue) : Boolean;
begin
  result := not (AValue.DataType is TUndefinedType);
end;

procedure SetValueUndefined(var AValue : TValue);
begin
  AValue.DataType := UndefinedType;
end;

function GetDataTypeFromString(AValue : String) : TDataType;
var
  i : Integer;
  FDataType : TDataType;
begin
  Result := nil;
  for i := 0 to DataTypeList.Count - 1 do
  begin
    FDataType := TDataType(DataTypeList.Items[i]);
    if FDataType.DataTypeName = AValue then
    begin
      Result := FDataType;
      Break;
    end;
  end;
  if Result = nil then
    Result := GetStringTypeFromString(AValue);
end;

function GetStringTypeFromString(AValue : String) : TDataType;
var
  FSize, FCount : Integer;
  FName : String;
begin
  Result := nil;
  FName := '';
  FCount := 1;
  FSize := 0;
  while not (FCount > Length(AValue)) and (AValue[FCount] <> '(') do
  begin
    FName := FName + AValue[FCount];
    Inc(FCount);
  end;
  if (FName <> MATCHCASENAME) and (FName <> NOCASENAME) then Exit;
  Inc(FCount);
  while not (FCount > Length(AValue)) and (AValue[FCount] <> ')') do
  begin
    if (AValue[FCount] < '0') or (AValue[FCount] > '9') then Exit
    else FSize := FSize * 10 + StrToInt(AValue[FCount]);
    Inc(FCount);
  end;
  if not (FCount > Length(AValue)) and (AValue[FCount] = ')') then
    Result := StringType(FSize, (FName = MATCHCASENAME));
end;

initialization
  DoInitialize;

finalization

  BooleanType.Free;
  DateTimeType.Free;
  DateType.Free;
  TimeType.Free;
  PointerType.Free;
  ObjectType.Free;
  IntegerType.Free;
  DoubleType.Free;
  CurrencyType.Free;
  PercentType.Free;
  MemoTypeCS.Free;
  MemoType.Free;

  FreeUndefinedTypeLists;
  FreeStringTypeLists;
  DataTypeList.Free;
  DataTypeFactory.Free;
end.

