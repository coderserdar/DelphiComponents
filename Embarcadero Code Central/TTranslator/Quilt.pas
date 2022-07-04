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


{ $Id: Quilt.pas,v 1.31 2003/01/09 15:36:48 mvj Exp $}

{-------------------------------------------------------------------------
  Quilt            MVJ???

  What

  Company          Polycon
  Authors          MVJ
-------------------------------------------------------------------------}

unit Quilt;

interface
{$i common.inc}

uses
  Classes, DataElements, DataType, DataTypes, IndexContainer;

type
  TQuilt = class; // forward
  TQuiltPatch = class; // forward
  TQuiltField = class; // forward

  TCustomQuilt = class( TCommonQuilt )
  public
{$ifdef D4_OR_HIGHER}
    function Compare(AQuilt : TCommonQuilt; AffectedFields : TDataFieldSet=nil) : TCompareResult; override;
{$else}
    function Compare(AQuilt : TCommonQuilt; AffectedFields : TDataFieldSet) : TCompareResult; override;
{$endif D4_OR_HIGHER}
    function GetQuilt( var FreeQuilt : Boolean ) : TQuilt; virtual; abstract;
    function Equals(Condition : TCondition) : Boolean; override;
    function GetCommonQuilt(out FreeQuilt : Boolean) : TCommonQuilt; override;
  end;

  TQuiltPatchList = class
  private
    FList : TList;
    FAffectedFieldSet: TDataFieldSet;
    FAffectedFieldSetUpToDate : Boolean;
    FOnChange: TNotifyEvent;

    function GetPatch(Index: Integer): TQuiltPatch;
    procedure UpdateAffectedFieldSet;
    function GetCount: Integer;
    function GetAffectedFieldSet: TDataFieldSet;
    procedure DoUpdateAffectedFieldSet;
    procedure Changed;
    procedure SetOnChange(const Value: TNotifyEvent);
  public
    constructor Create;
    destructor Destroy; override;

    function Add( APatch : TQuiltPatch ) : Integer;
    function IndexOf(APatch: TQuiltPatch): Integer;
    function Remove(APatch: TQuiltPatch): Integer;
    function Extract(APatch: TQuiltPatch): TQuiltPatch;
    procedure Delete(Index: Integer);
    procedure Clear;

    property Count: Integer read GetCount;
    property Patch[Index: Integer] : TQuiltPatch read GetPatch; default;
    property AffectedFieldSet: TDataFieldSet read GetAffectedFieldSet;
    property OnChange : TNotifyEvent read FOnChange write SetOnChange;
  end;

  TQuilt = class( TCustomQuilt )
  private
    FPatchList : TQuiltPatchList;
    FAcceptNonePatches : Boolean;

    function GetCount: Integer;
    function GetPatch(Index: Integer): TQuiltPatch;
    procedure UnionWithQuilt(AQuilt: TQuilt);
    procedure InternalDifference( AQuilt : TQuilt; var HadMatch : Boolean );
    procedure DifferenceWithPatch(AQuiltPatch: TQuiltPatch; var HadMatch: Boolean);
    procedure IntersectionWithPatch(APatch: TQuiltPatch);
    procedure IntersectionWithQuilt(AQuilt: TQuilt);
    procedure InternalAddFrom(AQuilt: TQuilt; NeedMerge: Boolean);
    procedure PatchListChange(Sender : TObject);
    procedure SetAcceptNonePatches(const Value: Boolean);
    procedure ExchangeSelections(AQuilt: TQuilt);
  protected
    function GetAffectedFieldSet : TDataFieldSet; override;
{$ifdef D4_OR_HIGHER}
    function IsSubSet(AQuilt : TQuilt; var FoundMatch : Boolean; AffectedFields : TDataFieldSet = nil) : Boolean;
{$else}
    function IsSubSet(AQuilt : TQuilt; var FoundMatch : Boolean; AffectedFields : TDataFieldSet) : Boolean;
{$endif D4_OR_HIGHER}

    function TryMergePatch(APatch: TQuiltPatch): Boolean;
{$ifdef D4_OR_HIGHER}
    function DoCreateUnion( AQuilt : TCommonQuilt; FreeParam : Boolean = True ) : TCommonQuilt; override;
    function DoCreateDifference( AQuilt : TCommonQuilt; FreeParam : Boolean = True ) : TCommonQuilt; override;
    function DoCreateIntersection( AQuilt : TCommonQuilt; FreeParam : Boolean = True ) : TCommonQuilt; override;
{$else}
    function DoCreateUnion( AQuilt : TCommonQuilt; FreeParam : Boolean ) : TCommonQuilt; override;
    function DoCreateDifference( AQuilt : TCommonQuilt; FreeParam : Boolean ) : TCommonQuilt; override;
    function DoCreateIntersection( AQuilt : TCommonQuilt; FreeParam : Boolean ) : TCommonQuilt; override;
{$endif D4_OR_HIGHER}

    procedure InternalAddPatch(APatch : TQuiltPatch);
    function Extract(APatch : TQuiltPatch) : TQuiltPatch;

    property AcceptNonePatches : Boolean read FAcceptNonePatches write SetAcceptNonePatches;
  public
    constructor Create;
    destructor Destroy; override;

    procedure KeepDimensions(AffectedFields : TDataFieldSet); override;
    procedure CopyFrom( AQuilt : TQuilt );
    procedure CopyFromCommon( ACommonQuilt : TCommonQuilt );
    function CreateQuiltCopy: TQuilt;
    function CreateCommonCopy : TCommonQuilt; override;
    function ContainsCommon(AQuilt : TCommonQuilt) : Boolean; override;
{$ifdef D4_OR_HIGHER}
    function CompareQuilt(AQuilt : TQuilt; AffectedFields : TDataFieldSet = nil) : TCompareResult;
    function ContainsQuilt(AQuilt : TQuilt; AffectedFields : TDataFieldSet = nil) : Boolean;
    function ContainsPatch(APatch : TQuiltPatch; AffectedFields : TDataFieldSet = nil) : Boolean;
{$else}
    function CompareQuilt(AQuilt : TQuilt; AffectedFields : TDataFieldSet) : TCompareResult;
    function ContainsQuilt(AQuilt : TQuilt; AffectedFields : TDataFieldSet) : Boolean;
    function ContainsPatch(APatch : TQuiltPatch; AffectedFields : TDataFieldSet) : Boolean;
{$endif D4_OR_HIGHER}
    function GetQuilt( var FreeQuilt : Boolean ) : TQuilt; override;

    // Returns the lowest level of Quilts with equal selections
    function ExtractOptimalQuilt  : TCommonQuilt; override;

    procedure Intersection( ACommonQuilt : TCommonQuilt );
    procedure Union( ACommonQuilt : TCommonQuilt );
    procedure Difference( ACommonQuilt : TCommonQuilt );

    procedure AddPatch(APatch : TQuiltPatch);

    // Inherited from TCondition
    function CreateFieldTranslatedCopy(TranslateField : TQueryDataFieldFunction) : TCondition; override;
    procedure ProcessFields(Proc : TProcFieldCond); override;
    function IsPureAndCondition : Boolean; override;

    function AcceptsAllInTable(DataTable : TDataTAble) : Boolean; override;
    function AcceptsNoValuesForTable(DataTable : TDataTable) : Boolean; override;
    function AcceptsAllForField(DataField : TDataField) : Boolean; override;
    function AcceptsNoValuesForField(DataField : TDataField) : Boolean; override;

    // Inherited from TCommonQuilt
    function AcceptsValue(AField : TDataField; AValue : TValue) : Boolean; override;
{$ifdef D4_OR_HIGHER}
    function AcceptsRow(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean; override;
    function AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean; override;
    function AcceptsAll( AffectedFields : TDataFieldSet = nil) : Boolean; override;
    function AcceptsNone( AffectedFields : TDataFieldSet = nil) : Boolean; override;
{$else}
    function AcceptsRow(ARow : TAbstractRow; AffectedFields : TDataFieldSet) : Boolean; override;
    function AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet) : Boolean; override;
    function AcceptsAll( AffectedFields : TDataFieldSet) : Boolean; override;
    function AcceptsNone( AffectedFields : TDataFieldSet) : Boolean; override;
{$endif D4_OR_HIGHER}

    function CreateCopy : TCondition; override;
    procedure Clear; override;


    function AcceptsExactlyOneValue(DataField : TDataField; var Value : TValue) : Boolean; override;

    property Patch[Index : Integer] : TQuiltPatch read GetPatch;
    property Count : Integer read GetCount;
  end;

  TQuiltFieldIterator = class(TIndexContainerIterator)
  private
    function GetQuiltField: TQuiltField;
  published
    property QuiltField : TQuiltField read GetQuiltField;
  end;

  TQuiltFieldList = class( TIndexContainer )
  private
    FAffectedFieldSet: TDataFieldSet;
    FAffectedFieldSetUpToDate : Boolean;
    FOnChange: TNotifyEvent;

    procedure UpdateAffectedFieldSet;
    function GetAffectedFieldSet: TDataFieldSet;
    procedure DoUpdateAffectedFieldSet;
    procedure Changed;
    procedure SetOnChange(const Value: TNotifyEvent);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Index: Pointer; Data: TObject); override;
{$ifdef D4_OR_HIGHER}
    function Remove(Index: Pointer): Boolean; overload; override;
    function Remove(Index: Pointer; var Data: TObject): Boolean; overload; override;
    procedure Clear; reintroduce;
{$endif D4_OR_HIGHER}
    procedure ClearAndFreeData; override;

    function CreateIterator : TQuiltFieldIterator;
    property AffectedFieldSet: TDataFieldSet read GetAffectedFieldSet;
    property OnChange : TNotifyEvent read FOnChange write SetOnChange;
  end;

  TQuiltPatch = class( TCustomQuilt )
  private
    FFields : TQuiltFieldList;

    procedure InternalAddQuiltField(AQuiltField : TQuiltField);
    function FindQuiltField(Field : TDataField; DoCreate : Boolean) : TQuiltField;
    function GetQuiltField(ADataField: TDataField): TQuiltField;
    procedure SetQuiltField(ADataField: TDataField;
      const Value: TQuiltField);
{$ifdef D4_OR_HIGHER}
    procedure InternalAddFrom(AQuiltPatch: TQuiltPatch; NeedMerge: Boolean = True);
{$else}
    procedure InternalAddFrom(AQuiltPatch: TQuiltPatch; NeedMerge: Boolean);
{$endif D4_OR_HIGHER}
  protected
    procedure AddFrom( AQuiltPatch : TQuiltPatch );
    function GetAffectedFieldSet : TDataFieldSet; override;
    function DifferenceMatched(AQuiltPatch: TQuiltPatch; ResultPieces : TList; var Overlap : TQuiltPatch) : Boolean;
    procedure IntersectionWithField(AQuiltField : TQuiltField);
    function AllAcceptsNone : Boolean;
{$ifdef D4_OR_HIGHER}
    function IsSubSet(AQuiltPatch : TQuiltPatch; var FoundMatch : Boolean; AffectedFields : TDataFieldSet = nil) : Boolean;
    function DoCreateUnion( AQuilt : TCommonQuilt; FreeParam : Boolean = True ) : TCommonQuilt; override;
    function DoCreateDifference( AQuilt : TCommonQuilt; FreeParam : Boolean = True ) : TCommonQuilt; override;
    function DoCreateIntersection( AQuilt : TCommonQuilt; FreeParam : Boolean = True ) : TCommonQuilt; override;
{$else}
    function IsSubSet(AQuiltPatch : TQuiltPatch; var FoundMatch : Boolean; AffectedFields : TDataFieldSet) : Boolean;
    function DoCreateUnion( AQuilt : TCommonQuilt; FreeParam : Boolean ) : TCommonQuilt; override;
    function DoCreateDifference( AQuilt : TCommonQuilt; FreeParam : Boolean ) : TCommonQuilt; override;
    function DoCreateIntersection( AQuilt : TCommonQuilt; FreeParam : Boolean ) : TCommonQuilt; override;
{$endif D4_OR_HIGHER}
    procedure InternalGetRows(Handler: TGetRowsHandler;
      SubTotal: TSubTotalRow); override;
  public
    constructor Create;
    destructor Destroy; override;

    function CreateQuiltPatchCopy: TQuiltPatch;
    function CreateFieldTranslatedPatchCopy(TranslateField : TQueryDataFieldFunction) : TQuiltPatch;
    function CreateCommonCopy : TCommonQuilt; override;

    function GetQuilt( var FreeQuilt : Boolean ) : TQuilt; override;
    function ContainsCommon(AQuilt : TCommonQuilt) : Boolean; override;
    function HasConditionOnField(Field: TDataField): Boolean;

    procedure CopyFrom( AQuiltPatch : TQuiltPatch );
    procedure KeepDimensions(AffectedFields : TDataFieldSet); override;

    procedure Intersection(APatch : TQuiltPatch);
    procedure AddValuesFromRowKeys(ARow: TAbstractRow);
    procedure AddValuesFromRowFields(ARow: TAbstractRow);

    // Returns the lowest level of Quilts with equal selections
    function ExtractOptimalQuilt  : TCommonQuilt; override;

    // Inherited from TCondition
    function IsPureAndCondition : Boolean; override;
    function CreateFieldTranslatedCopy(TranslateField : TQueryDataFieldFunction) : TCondition; override;
    procedure ProcessFields(Proc : TProcFieldCond); override;

    function AcceptsAllInTable(DataTable : TDataTAble) : Boolean; override;
    function AcceptsNoValuesForTable(DataTable : TDataTable) : Boolean; override;
    function AcceptsAllForField(DataField : TDataField) : Boolean; override;
    function AcceptsNoValuesForField(DataField : TDataField) : Boolean; override;

    // Inherited from TCommonQuilt
    function AcceptsValue(AField : TDataField; AValue : TValue) : Boolean; override;
{$ifdef D4_OR_HIGHER}
    function AcceptsRow(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean; override;
    function AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean; override;
    function AcceptsAll( AffectedFields : TDataFieldSet = nil) : Boolean; override;
    function AcceptsNone( AffectedFields : TDataFieldSet = nil) : Boolean; override;
{$else}
    function AcceptsRow(ARow : TAbstractRow; AffectedFields : TDataFieldSet) : Boolean; override;
    function AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet) : Boolean; override;
    function AcceptsAll( AffectedFields : TDataFieldSet ) : Boolean; override;
    function AcceptsNone( AffectedFields : TDataFieldSet ) : Boolean; override;
{$endif D4_OR_HIGHER}

    function CreateCopy : TCondition; override;
    procedure Clear; override;


    function AcceptsExactlyOneValue(DataField : TDataField; var Value : TValue) : Boolean; override;

    function FieldCount : Integer;
    function CreateIterator : TQuiltFieldIterator;
    property QuiltField[ ADataField : TDataField ] : TQuiltField read GetQuiltField write SetQuiltField; default;
  end;

  TQuiltPoint = record
    FValue : TValue;
    FClosed : Boolean;
    FInfinity : Boolean;
  end;
  PQuiltPoint = ^TQuiltPoint;

  TQuiltValueItem = record
    FStart: TQuiltPoint;
    FEnd: TQuiltPoint;
  end;
  PQuiltValueItem = ^TQuiltValueItem;

  TQuiltValueItemList = array[0..(MaxListSize div 4)] of TQuiltValueItem;
  PQuiltValueItemList = ^TQuiltValueItemList;

  TSmallerOrTouchResult = (sotSmaller, sotTouch, sotLarger);
  
  TQuiltValueList = class
  private
    FList: PQuiltValueItemList;
    FCount: Integer;
    FCapacity: Integer;
    FDataField : TDataField;
    FInitialized : Boolean;
    FOnChange: TNotifyEvent;

    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer);
    procedure InsertItem(Index: Integer; const S: TQuiltPoint);
    procedure Error(const Msg: string);
    function Compare(V1, V2: TQuiltPoint): Integer;
    function IntervalAtIdxAcceptsInterval(Idx: Integer; Low,
      High: TQuiltPoint): Boolean;
    function CompareEndToHigh(EndPoint, High: TQuiltPoint): Integer;
    function CompareStartToHigh(StartPoint, High: TQuiltPoint): Integer;
    function CompareStartToLow(StartPoint, Low: TQuiltPoint): Integer;
    procedure CheckIntervalLegal(Low, High: TQuiltPoint);
    function InternalAddInterval(Low, High: TQuiltPoint): Integer;
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure JoinIntervals(StartIndex: Integer);
    function SmallerOrTouch(Point1, Point2: TQuiltPoint): TSmallerOrTouchResult;
  protected
    function AddStartPoint(const S: TQuiltPoint): Integer;
    procedure SetStartPoint(Index: Integer; const S: TQuiltPoint);
    procedure SetEndPoint(Index: Integer; const S: TQuiltPoint);
    procedure DoClear;
    procedure Changed;

    function Get1(Index: Integer): TQuiltPoint;
    function Get2(Index: Integer): TQuiltPoint;
    function GetCapacity: Integer;
    function GetObject(Index: Integer): TObject;
    procedure SetCapacity(NewCapacity: Integer);
    function AcceptsValue(Value : TValue) : Boolean;
    function AcceptsInterval(Low, High : TQuiltPoint) : Boolean;

    property Initialized : Boolean read FInitialized write FInitialized;
  public
    constructor Create(ADataField : TDataField);
    destructor Destroy; override;

    function AddValue(Value : TValue) : Integer;
    function AddInterval(Low, High : TQuiltPoint) : Integer;

    procedure RemoveInterval(Low, High : TQuiltPoint);

{$ifdef D4_OR_HIGHER}
    procedure Split(SplitList: TQuiltValueList; out HadIntersect, HadDiff, HadSplitDiff : Boolean;
      IntersectList: TQuiltValueList = nil; DiffList : TQuiltValueList = nil; SplitDiffList : TQuiltValueList = nil);
{$else}
    procedure Split(SplitList: TQuiltValueList; out HadIntersect, HadDiff, HadSplitDiff : Boolean;
      IntersectList: TQuiltValueList; DiffList : TQuiltValueList; SplitDiffList : TQuiltValueList);
{$endif D4_OR_HIGHER}

    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function Find(const S: TQuiltPoint; var Index: Integer): Boolean;
    function IndexOf(const S: TQuiltPoint): Integer;
    property StartPoint[Idx : Integer] : TQuiltPoint read Get1;
    property EndPoint[Idx : Integer] : TQuiltPoint read Get2;
    property Count : Integer read FCount;
    property DataField : TDataField read FDataField;
    property OnChange : TNotifyEvent read FOnChange write SetOnChange;
  end;

  TQuiltField = class( TCustomQuilt )
  private
    FAffectedFieldSet: TDataFieldSet;

    function GetDataField: TDataField;
{$ifdef QUILTCAPTION}
    function GetCaption: String;
    procedure SetCaption(const Value: String);
{$endif QUILTCAPTION}
    function GetCount: Integer;
    function GetEnd(Index: Integer): TQuiltPoint;
    function GetStart(Index: Integer): TQuiltPoint;
    procedure InternalAddFrom(AQuiltField: TQuiltField;
      NeedMerge: Boolean);
    procedure ExchangeSelections(AField: TQuiltField);
    procedure ValueListChange(Sender: TObject);
    function GetIsInterval(Index: Integer): Boolean;
    function GetSingleValue(idx: Integer): TValue;
  protected
    FValues : TQuiltValueList;

    constructor CreateOwned( Owner : TQuiltPatch; ADataField : TDataField );

    function GetAffectedFieldSet : TDataFieldSet; override;

{$ifdef D4_OR_HIGHER}
    function IsSubSet(AQuilt : TQuiltField; var FoundMatch : Boolean; AffectedFields : TDataFieldSet = nil) : Boolean;
    function DoCreateUnion( AQuilt : TCommonQuilt; FreeParam : Boolean = True ) : TCommonQuilt; override;
    function DoCreateDifference( AQuilt : TCommonQuilt; FreeParam : Boolean = True ) : TCommonQuilt; override;
    function DoCreateIntersection( AQuilt : TCommonQuilt; FreeParam : Boolean = True ) : TCommonQuilt; override;
{$else}
    function IsSubSet(AQuilt : TQuiltField; var FoundMatch : Boolean; AffectedFields : TDataFieldSet) : Boolean;
    function DoCreateUnion( AQuilt : TCommonQuilt; FreeParam : Boolean ) : TCommonQuilt; override;
    function DoCreateDifference( AQuilt : TCommonQuilt; FreeParam : Boolean ) : TCommonQuilt; override;
    function DoCreateIntersection( AQuilt : TCommonQuilt; FreeParam : Boolean ) : TCommonQuilt; override;
{$endif D4_OR_HIGHER}

    function DoAdd(const Value: TValue): Integer;
    function DoAddInterval(Low, High : TQuiltPoint) : Integer;
    procedure DoRemove(const Value: TValue);
    procedure DoRemoveInterval(Low, High : TQuiltPoint);

{$ifdef D4_OR_HIGHER}
    procedure DoSplit(SplitField : TQuiltField; out HadIntersect, HadDiff, HadSplitDiff : Boolean;
      aIntersect : TQuiltField = nil; aDiff : TQuiltField = nil; aSplitDiff : TQuiltField = nil);
    procedure Split(SplitField : TQuiltField; aIntersect : TQuiltField; aDiff : TQuiltField = nil; aSplitDiff : TQuiltField = nil);
{$else}
    procedure DoSplit(SplitField : TQuiltField; out HadIntersect, HadDiff, HadSplitDiff : Boolean;
      aIntersect : TQuiltField; aDiff : TQuiltField; aSplitDiff : TQuiltField);
    procedure Split(SplitField : TQuiltField; aIntersect : TQuiltField; aDiff : TQuiltField; aSplitDiff : TQuiltField);
{$endif D4_OR_HIGHER}
  public
    constructor Create( ADataField : TDataField );
    destructor Destroy; override;

    function CreateQuiltFieldCopy : TQuiltField;
    function CreateCommonCopy : TCommonQuilt; override;

    function GetQuilt( var FreeQuilt : Boolean ) : TQuilt; override;
    procedure CopyFrom( AQuiltField : TQuiltField );
    procedure KeepDimensions(AffectedFields : TDataFieldSet); override;

    // Returns the lowest level of Quilts with equal selections
    function ExtractOptimalQuilt  : TCommonQuilt; override;

    procedure Intersection( AQuiltField : TQuiltField );
    procedure Union( AQuiltField : TQuiltField );
    procedure Difference( AQuiltField : TQuiltField );

    procedure AcceptNone;
    procedure AcceptAll;

    function ContainsCommon(AQuilt : TCommonQuilt) : Boolean; override;
    function CompareQuiltField(AQuiltField : TQuiltField) : TCompareResult;
    function QuiltFieldsEqual( AQuiltField : TQuiltField ) : Boolean;

    // Inherited from TCondition
    function IsPureAndCondition : Boolean; override;
    function CreateFieldTranslatedCopy(TranslateField : TQueryDataFieldFunction) : TCondition; override;
    procedure ProcessFields(Proc : TProcFieldCond); override;

    function AcceptsAllInTable(DataTable : TDataTAble) : Boolean; override;
    function AcceptsNoValuesForTable(DataTable : TDataTable) : Boolean; override;
    function AcceptsAllForField(DataField : TDataField) : Boolean; override;
    function AcceptsNoValuesForField(DataField : TDataField) : Boolean; override;

    // Inherited from TCommonQuilt
    function AcceptsValue(AField : TDataField; AValue : TValue) : Boolean; override;
{$ifdef D4_OR_HIGHER}
    function AcceptsRow(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean; override;
    function AcceptsRowDependent(ARow: TAbstractRow; AffectedFields: TDataFieldSet = nil): Boolean; override;
    function AcceptsAll( AffectedFields : TDataFieldSet = nil) : Boolean; override;
    function AcceptsNone( AffectedFields : TDataFieldSet = nil) : Boolean; override;
{$else}
    function AcceptsRow(ARow : TAbstractRow; AffectedFields : TDataFieldSet) : Boolean; override;
    function AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet) : Boolean; override;
    function AcceptsAll( AffectedFields : TDataFieldSet) : Boolean; override;
    function AcceptsNone( AffectedFields : TDataFieldSet) : Boolean; override;
{$endif D4_OR_HIGHER}

    function CreateCopy : TCondition; override;
    procedure Clear; override;

    function Add(const Value: TValue): Integer;
    function AddInterval(Low, High : TQuiltPoint) : Integer;
    procedure Remove(const Value: TValue);
    procedure RemoveInterval(Low, High : TQuiltPoint);


    function AcceptsExactlyOneValue(DataField : TDataField; var Value : TValue) : Boolean; override;

    property DataField : TDataField read GetDataField;
{$ifdef QUILTCAPTION}
    property Caption : String read GetCaption write SetCaption;
{$endif QUILTCAPTION}
    property Count : Integer read GetCount;
    property Value[idx : Integer] : TValue read GetSingleValue;
    property StartPoint[Index : Integer] : TQuiltPoint read GetStart;
    property EndPoint[Index : Integer] : TQuiltPoint read GetEnd;
    property IsInterval[Index: Integer] : Boolean read GetIsInterval;
  end;

{$ifdef D4_OR_HIGHER}
  function QuiltPoint( Value : TValue; Closed : Boolean = True ) : TQuiltPoint;
{$else}
  function QuiltPoint( Value : TValue; Closed : Boolean ) : TQuiltPoint;
{$endif D4_OR_HIGHER}
  function QuiltPointEquals( P1, P2 : TQuiltPoint ): Boolean;
  function GetEndValue(ADataType : TDataType; StartValue : String) : String;
  procedure AddBeginInterval(AField : TQuiltField; StartValue : String);

var
  NegInfinity : TQuiltPoint;
  PosInfinity : TQuiltPoint;

const
  INTERVAL_SEPARATOR = ',';
  INTERVAL_DELIMITER = '-';
  INFINITY = 'Inf';
  INFINITY_POS = 'P_' + INFINITY;
  INFINITY_NEG = 'N_' + INFINITY;
  WORD_ALL = 'All';
  BRACKET_LEFT = '[';
  BRACKET_RIGHT = ']';

implementation

uses
{$ifdef D4_OR_HIGHER}
  Contnrs,
{$endif D4_OR_HIGHER}
{$ifndef LINUX}
  Windows,
{$ifdef USERESOURCESTRING}
  Consts,
{$endif USERESOURCESTRING}
{$endif LINUX}
{$ifdef QUILTCAPTION}
  QuiltCaptionHandler,
{$endif QUILTCAPTION}
  SysUtils, 
  CommonLib;

const
  HASHSIZE = 101{53}; // number isn't important as long as it's of reasonable size and a prime

{$ifdef D4_OR_HIGHER}
function QuiltPointWithInfinity(Value : TValue; Closed : Boolean = True; Infinity : Boolean = False ) : TQuiltPoint;
{$else}
function QuiltPointWithInfinity(Value : TValue; Closed : Boolean; Infinity : Boolean ) : TQuiltPoint;
{$endif D4_OR_HIGHER}
begin
  with Result do
  begin
    FValue := Value;
    FClosed := Closed;
    FInfinity := Infinity;
  end;
end;

function QuiltPoint( Value : TValue; Closed : Boolean ) : TQuiltPoint;
begin
{$ifdef D4_OR_HIGHER}
  Result := QuiltPointWithInfinity( Value, Closed );
{$else}
  Result := QuiltPointWithInfinity( Value, Closed, False );
{$endif D4_OR_HIGHER}
end;

function QuiltPointEquals( P1, P2 : TQuiltPoint ): Boolean;
begin
  Result := (P1.FClosed = P2.FClosed) and                          // Both has to be either closed or open
            (P1.FInfinity = P2.FInfinity) and                      // Both are either infinity or not
            ( P1.FInfinity or                                      // If they are infinity, thats fine
              (P1.FValue.DataType.Equals(P1.FValue, P2.FValue)) ); // Otherwise, the values must equal
end;

function ToggleClosed( APoint : TQuiltPoint ) : TQuiltPoint;
begin
  with APoint do
    if FInfinity then
      Result := APoint
    else
      Result := QuiltPointWithInfinity( FValue, not FClosed, FInfinity );
end;

procedure E;
begin
  raise Exception.Create( 'Not implemented!' );
end;

function GetEndValue(ADataType : TDataType; StartValue : String) : String;
var
  EndValue : String;
  LastChar, OldLastChar : Char;
  StrLength, iInc : Integer;
  IsCaseSensitive : Boolean;
begin
  EndValue := StartValue;
  StrLength := Length(EndValue);
  if StrLength = 0 then
  begin
    Result := StartValue;
    Exit;
  end;

  if ADataType is TStringType then
    IsCaseSensitive := TStringType(ADataType).CaseSensitive
  else
    IsCaseSensitive := False;

  iInc := 0;

  LastChar := EndValue[StrLength];
  OldLastChar := LastChar;
  repeat
  begin
    while (LastChar = High(LastChar)) do
    begin
      Delete(EndValue, StrLength, 1);
      Dec(StrLength);
      if StrLength = 0 then
        Break;
      iInc := 0;
      LastChar := EndValue[StrLength];
      OldLastChar := LastChar;
    end;

    if StrLength > 0 then
    begin
      Inc(iInc);
      LastChar := OldLastChar;
      Inc(LastChar, iInc);
      EndValue[StrLength] := LastChar;
    end
    else
      Break;
  end
  until IsCaseSensitive or
        (ADataType.Compare(ValueFromString(LastChar), ValueFromString(OldLastChar)) > 0);

  Result := EndValue;
end;

procedure AddBeginInterval(AField : TQuiltField; StartValue : String);
var
  EndValue : String;
begin
  if Length(StartValue) = 0 then
    AField.AcceptAll
  else
  begin
    EndValue := GetEndValue(AField.DataField.DataType, StartValue);
    if Length(EndValue) > 0 then
{$ifdef D4_OR_HIGHER}
      AField.DoAddInterval( QuiltPoint(ValueFromString(StartValue)), QuiltPoint(ValueFromString(EndValue), False))
    else
      AField.DoAddInterval( QuiltPoint(ValueFromString(StartValue)), PosInfinity );
{$else}
      AField.DoAddInterval( QuiltPoint(ValueFromString(StartValue),True), QuiltPoint(ValueFromString(EndValue), False))
    else
      AField.DoAddInterval( QuiltPoint(ValueFromString(StartValue),True), PosInfinity );
{$endif D4_OR_HIGHER}
  end;
end;

{ TCustomQuilt }

{$ifdef D4_OR_HIGHER}
function TCustomQuilt.Compare(AQuilt: TCommonQuilt;   // LGE: Skulle vi inte kunna "uppgradera" dem till "mgn",
  AffectedFields: TDataFieldSet = nil): TCompareResult;     //      dvs om vi t.ex. har två Patchar är det ju onödigt att skapa nya objekt...
{$else}
function TCustomQuilt.Compare(AQuilt: TCommonQuilt;   // LGE: Skulle vi inte kunna "uppgradera" dem till "mgn",
  AffectedFields: TDataFieldSet): TCompareResult;     //      dvs om vi t.ex. har två Patchar är det ju onödigt att skapa nya objekt...
{$endif D4_OR_HIGHER}
var
  SelfQuilt, ParamQuilt : TQuilt;
  FreeQuilt, FreeParamQuilt : Boolean;
begin
  SelfQuilt := GetQuilt( FreeQuilt );
  Assert(AQuilt is TCustomQuilt);
  ParamQuilt := TCustomQuilt(AQuilt).GetQuilt( FreeParamQuilt );

  Result := SelfQuilt.CompareQuilt( ParamQuilt, AffectedFields );

  if FreeQuilt then
    SelfQuilt.Free;
  if FreeParamQuilt then
    ParamQuilt.Free;
end;

function TCustomQuilt.Equals(Condition: TCondition): Boolean;
var
  ACommonQuilt : TCommonQuilt;
  FreeCommon : Boolean;
begin
  ACommonQuilt := Condition.GetCommonQuilt(FreeCommon);
{$ifdef D4_OR_HIGHER}
  Result := Compare(ACommonQuilt) = crEqual;
{$else}
  Result := Compare(ACommonQuilt,nil) = crEqual;
{$endif D4_OR_HIGHER}
  if FreeCommon then
    ACommonQuilt.Free;
end;

function TCustomQuilt.GetCommonQuilt(out FreeQuilt: Boolean): TCommonQuilt;
begin
  Result := Self;
  FreeQuilt := False;
end;

{ TQuilt }

function TQuilt.AcceptsExactlyOneValue(DataField: TDataField;
  var Value: TValue): Boolean;
var
  i : Integer;
  tmpValue : TValue;
  NoLegalValue : Boolean;
  APatch : TQuiltPatch;
  ASet : TDataFieldSet;
begin
  NoLegalValue := True;
  Result := False;
  ASet := TDataFieldSet.Create;
  ASet.AddField(DataField);

  for i := 0 to Count -1 do
  begin
    APatch := Patch[i];
    if not APatch.AcceptsNone(ASet) and
       APatch.AcceptsExactlyOneValue(DataField, Value) then
    begin
      if NoLegalValue then
      begin
        NoLegalValue := False;
        Result := True;
      end
      else if not DataField.DataType.Equals(Value, tmpValue) then
      begin
        Result := False;
        Break;
      end;
      tmpValue := Value;
    end;
  end;

  ASet.Free;
end;

{$ifdef D4_OR_HIGHER}
function TQuilt.AcceptsRow(ARow: TAbstractRow; AffectedFields: TDataFieldSet = nil): Boolean;
{$else}
function TQuilt.AcceptsRow(ARow: TAbstractRow; AffectedFields: TDataFieldSet): Boolean;
{$endif D4_OR_HIGHER}
var
  i : Integer;
begin
  Result := False;
  for i := 0 to Count -1 do
    if Patch[i].AcceptsRow(ARow, AffectedFields) then
    begin
      Result := True;
      Break;
    end;
end;

{$ifdef D4_OR_HIGHER}
function TQuilt.AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean;
{$else}
function TQuilt.AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet) : Boolean;
{$endif D4_OR_HIGHER}
var
  i : Integer;
begin
  Result := False;
  for i := 0 to Count -1 do
    if Patch[i].AcceptsRowDependent(ARow, AffectedFields) then
    begin
      Result := True;
      Break;
    end;
end;

function TQuilt.AcceptsValue(AField: TDataField; AValue: TValue): Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 0 to Count -1 do
    if Patch[i].AcceptsValue( AField, AValue ) then
    begin
      Result := True;
      Break;
    end;
end;

{$ifdef D4_OR_HIGHER}
function TQuilt.IsSubSet(AQuilt: TQuilt; var FoundMatch: Boolean;
  AffectedFields: TDataFieldSet = nil): Boolean;
var
  tmpQuilt : TQuilt;
begin
  if AcceptsAll(AffectedFields) or AQuilt.AcceptsNone(AffectedFields) then
  begin
    FoundMatch := True;
    Result := True;
  end
  else if AcceptsNone(AffectedFields) or AQuilt.AcceptsAll(AffectedFields) then
  begin
    Result := False;
    FoundMatch := True;
  end
  else
  begin
    if ContainsQuilt(AQuilt, AffectedFields) then
    begin
      Result := True;
      FoundMatch := True;
    end
    else
    begin
      tmpQuilt := AQuilt.CreateQuiltCopy;
      tmpQuilt.KeepDimensions(AffectedFields);
      tmpQuilt.InternalDifference( Self, FoundMatch );
      Result := FoundMatch and tmpQuilt.AcceptsNone( AffectedFields );
      tmpQuilt.Free;
    end;
  end;
end;
{$endif D4_OR_HIGHER}



constructor TQuilt.Create;
begin
  inherited Create;
  FAcceptNonePatches := False;
  FPatchList := TQuiltPatchList.Create;
  FPatchList.OnChange := PatchListChange;
end;

function TQuilt.CreateQuiltCopy: TQuilt;
begin
  Result := TQuilt.Create;
  Result.CopyFrom(Self);
end;

function TQuilt.CreateCopy: TCondition;
begin
  Result := CreateQuiltCopy;
end;

destructor TQuilt.Destroy;
begin
  inherited;
  FPatchList.Free;
end;

function TQuilt.TryMergePatch(APatch: TQuiltPatch) : Boolean;
var
  i : Integer;
  ThisPatch : TQuiltPatch;
  ThisField, PatchField : TQuiltField;
  ASet : TDataFieldSet;
  AField, MergeField : TDataField;
  CanMerge : Boolean;
  CompRes, ParamToThis : TCompareResult;
begin
  Result := False;
{$ifdef D4_OR_HIGHER}
  if AcceptsNone then
{$else}
  if AcceptsNone(nil) then
{$endif D4_OR_HIGHER}
  begin
    FPatchList.Clear;
    Exit;
  end;

  ASet := TDataFieldSet.Create;
  try
    for i := Count -1 downto 0 do
    begin
      ThisPatch := Patch[i];

{$ifdef D4_OR_HIGHER}
      if ThisPatch.AcceptsAll then
{$else}
      if ThisPatch.AcceptsAll(nil) then
{$endif D4_OR_HIGHER}
      begin
        Result := True;
        Break;
      end;

      ParamToThis := crEqual;
      CanMerge := False;
      MergeField := nil;

      ASet.CopyFrom(APatch.AffectedFieldSet);
      ASet.AddFrom(Self.AffectedFieldSet);
      with TDataFieldSetIterator.Create(ASet) do
      try
        while not EOF do
        begin
          AField := Field;
          ThisField := ThisPatch.FindQuiltField(AField, False);
          PatchField := APatch.FindQuiltField(AField, False);

          if ThisField = nil then
          begin
{$ifdef D4_OR_HIGHER}
            if (PatchField = nil) or (PatchField.AcceptsAll) then
{$else}
            if (PatchField = nil) or (PatchField.AcceptsAll(nil)) then
{$endif D4_OR_HIGHER}
              CompRes := crEqual
            else
              CompRes := crSuperSet;
          end
          else if PatchField = nil then
          begin
{$ifdef D4_OR_HIGHER}
            if ThisField.AcceptsAll then
{$else}
            if ThisField.AcceptsAll(nil) then
{$endif D4_OR_HIGHER}
              CompRes := crEqual
            else
              CompRes := crSubSet;
          end
          else
            CompRes := ThisField.CompareQuiltField(PatchField);
            
          if CompRes <> crEqual then
          begin
            if MergeField = nil  then
            begin
              CanMerge := True;
              MergeField := AField;
            end
            else
              CanMerge := False;

            if ParamToThis <> CompRes then
            begin
              if CompRes = crSeparate then
                ParamToThis := crSeparate
              else if ParamToThis = crEqual then
                ParamToThis := CompRes
              else
                ParamToThis := crOverlapping;
            end;

            if not CanMerge and (ParamToThis in [crOverlapping, crSeparate]) then
              Break;
          end;
          Next;
        end;
      finally
        Free;
      end;

      if ParamToThis = crSuperSet then
      begin
        Result := False;
        FPatchList.Delete(i);
      end
      else if ParamToThis in [crSubset, crEqual] then
      begin
        Result := True;
        Break;
      end
      else if CanMerge then
      begin
        Result := True;
        Extract(ThisPatch);
        ThisPatch.QuiltField[MergeField].Union( APatch.QuiltField[MergeField] );
        AddPatch( ThisPatch );
        Break;
      end;
    end;
  finally
    ASet.Free;
  end;
end;

function TQuilt.Extract(APatch : TQuiltPatch) : TQuiltPatch;
begin
  APatch.SetOwner(nil);
  Result := FPatchList.Extract(APatch);
end;

procedure TQuilt.AddPatch(APatch: TQuiltPatch);
begin
{$ifdef D4_OR_HIGHER}
  if (not AcceptNonePatches and APatch.AcceptsNone) or
{$else}
  if (not AcceptNonePatches and APatch.AcceptsNone(nil)) or
{$endif D4_OR_HIGHER}
     APatch.AllAcceptsNone then
  begin
    APatch.Free;
    Exit;
  end;

  BeginUpdate;
  try
    if APatch.Owner <> nil then
      raise Exception.Create( Self.ClassName + '.AddPatch: APatch is already owned by another TCondition!' );

{$ifdef D4_OR_HIGHER}
    if ContainsPatch(APatch) or
{$else}
    if ContainsPatch(APatch,nil) or
{$endif D4_OR_HIGHER}
       TryMergePatch( APatch ) then
      APatch.Free
    else
      InternalAddPatch(APatch);
  finally
    EndUpdate;
  end;
end;

procedure TQuilt.InternalAddPatch(APatch : TQuiltPatch);
begin
{$ifdef D4_OR_HIGHER}
  if (not AcceptNonePatches and APatch.AcceptsNone) or
{$else}
  if (not AcceptNonePatches and APatch.AcceptsNone(nil)) or
{$endif D4_OR_HIGHER}
     APatch.AllAcceptsNone then
  begin
    APatch.Free;
    Exit;
  end;

  BeginUpdate;
  try
    APatch.SetOwner( Self );
    FPatchList.Add( APatch );
  finally
    EndUpdate;
  end;
end;

function TQuilt.GetCount: Integer;
begin
  Result := FPatchList.Count;
end;

function TQuilt.GetPatch(Index: Integer): TQuiltPatch;
begin
  Result := FPatchList[Index];
end;

{$ifdef D4_OR_HIGHER}
function TQuilt.AcceptsAll(AffectedFields: TDataFieldSet = nil): Boolean;
{$else}
function TQuilt.AcceptsAll(AffectedFields: TDataFieldSet): Boolean;
{$endif D4_OR_HIGHER}
var
  i : Integer;
begin
  Result := False;
  for i := 0 to Count -1 do
  begin
    if Patch[i].AcceptsAll(AffectedFields) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TQuilt.UnionWithQuilt(AQuilt : TQuilt);
begin
  InternalAddFrom(AQuilt, True);
end;

procedure TQuilt.InternalAddFrom(AQuilt : TQuilt; NeedMerge : Boolean);
var
  APatch : TQuiltPatch;
  i : Integer;
begin
  BeginUpdate;
  try
    for i := 0 to AQuilt.Count -1 do
    begin
      APatch := AQuilt.Patch[i].CreateQuiltPatchCopy;
      if NeedMerge then
        AddPatch( APatch )
      else
        InternalAddPatch( APatch );
    end;
  finally
    EndUpdate;
  end;
end;

procedure TQuilt.CopyFrom(AQuilt : TQuilt);
begin
  BeginUpdate;
  try
    Clear;
    InternalAddFrom(AQuilt, False);
  finally
    EndUpdate;
  end;
end;

procedure TQuilt.CopyFromCommon( ACommonQuilt : TCommonQuilt );
var
  AQuilt : TQuilt;
  FreeQuilt : Boolean;
begin
  Assert(ACommonQuilt is TCustomQuilt);
  AQuilt := TCustomQuilt(ACommonQuilt).GetQuilt( FreeQuilt );
  CopyFrom( AQuilt );

  if FreeQuilt then
    AQuilt.Free;
end;

procedure TQuilt.DifferenceWithPatch(AQuiltPatch: TQuiltPatch;
  var HadMatch: Boolean);
var
  i, j : Integer;
  ResultPieces : TList;
  APatch, Overlap : TQuiltPatch;
begin
  BeginUpdate;
  try
    ResultPieces := TList.Create;

    for i := Count -1 downto 0 do
    begin
      APatch := Patch[i];
      if APatch.DifferenceMatched( AQuiltPatch, ResultPieces, Overlap ) then
        HadMatch := True
      else
        ResultPieces.Add( Extract(APatch) );
      Overlap.Free;
    end;

    FPatchList.Clear;
    for j := 0 to ResultPieces.Count -1 do
      AddPatch( TQuiltPatch(ResultPieces[j]) );
    ResultPieces.Free;
  finally
    EndUpdate;
  end;
end;

procedure TQuilt.InternalDifference(AQuilt: TQuilt;
  var HadMatch: Boolean);
var
  i : Integer;
begin
  BeginUpdate;
  try
    HadMatch := False;
    for i := 0 to AQuilt.Count -1 do
    begin
      DifferenceWithPatch(AQuilt.Patch[i], HadMatch);
      if Count = 0 then
        Break;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TQuilt.Difference(ACommonQuilt: TCommonQuilt);
var
  AQuilt : TQuilt;
  FreeQuilt, Dummy : Boolean;
begin
  Assert(ACommonQuilt is TCustomQuilt);
  AQuilt := TCustomQuilt(ACommonQuilt).GetQuilt( FreeQuilt );
  InternalDifference( AQuilt, Dummy );
  if FreeQuilt then
    AQuilt.Free;
end;

procedure TQuilt.ExchangeSelections(AQuilt: TQuilt);
var
  AList : TQuiltPatchList;
begin
  BeginUpdate;
  try
    AList := AQuilt.FPatchList;
    AQuilt.FPatchList := FPatchList;
    AQuilt.FPatchList.OnChange := FPatchList.OnChange;
    FPatchList := AList;
    FPatchList.OnChange := PatchListChange;
    Changed;
    AQuilt.Changed;
  finally
    EndUpdate;
  end;
end;

procedure TQuilt.IntersectionWithQuilt(AQuilt: TQuilt);
var
  i	 : Integer;
  NewQuilt, Temp : TQuilt;
begin
  BeginUpdate;
  NewQuilt := TQuilt.Create;
  try
    for i := 0 to AQuilt.Count -1 do
    begin
      Temp := CreateQuiltCopy;
      Temp.IntersectionWithPatch( AQuilt.Patch[i] );
      NewQuilt.UnionWithQuilt(Temp);
      Temp.Free;
    end;

    ExchangeSelections(NewQuilt);
  finally
    NewQuilt.Free;
    EndUpdate;
  end;
end;

procedure TQuilt.IntersectionWithPatch(APatch: TQuiltPatch);
var
  i	 : Integer;
begin
  BeginUpdate;
  try
    for i := 0 to Count -1 do
      Patch[i].Intersection(APatch)
  finally
    EndUpdate;
  end;
end;

procedure TQuilt.Intersection(ACommonQuilt: TCommonQuilt);
var
  Other : TQuilt;
  FreeQuilt : Boolean;
begin
  BeginUpdate;
  try
    if ACommonQuilt is TQuilt then
      IntersectionWithQuilt(TQuilt(ACommonQuilt))
    else if ACommonQuilt is TQuiltPatch then
      IntersectionWithPatch(TQuiltPatch(ACommonQuilt))
    else
    begin
      Assert(ACommonQuilt is TCustomQuilt);
      Other := TCustomQuilt(ACommonQuilt).GetQuilt(FreeQuilt);
      IntersectionWithQuilt(Other);
      if FreeQuilt then
        Other.Free;

  (*    NewQuilt := TQuilt.Create;
      try
        for i := 0 to Other.Count -1 do
        begin
          Temp := CreateQuiltCopy;
          Temp.IntersectionWithPatch( Other.Patch[i] );
          NewQuilt.UnionWithQuilt(Temp);
          Temp.Free;
        end;

        AList := NewQuilt.FPatchList;
        NewQuilt.FPatchList := FPatchList;
        NewQuilt.FPatchList.OnChange := FPatchList.OnChange;
        FPatchList := AList;
        FPatchList.OnChange := PatchListChange;
        Changed;
      finally
        if FreeQuilt then
          Other.Free;
        NewQuilt.Free;
        EndUpdate;
      end;
      *)
    end;
  finally
    EndUpdate;
  end;
end;

procedure TQuilt.Union(ACommonQuilt: TCommonQuilt);
var
  AQuilt : TQuilt;
  FreeQuilt : Boolean;
begin
  Assert( ACommonQuilt is TCustomQuilt );
  AQuilt := TCustomQuilt(ACommonQuilt).GetQuilt( FreeQuilt );
  UnionWithQuilt( AQuilt );
  if FreeQuilt then
    AQuilt.Free;
end;

{$ifdef D4_OR_HIGHER}
function TQuilt.AcceptsNone(AffectedFields: TDataFieldSet = nil): Boolean;
{$else}
function TQuilt.AcceptsNone(AffectedFields: TDataFieldSet): Boolean;
{$endif D4_OR_HIGHER}
var
  i : Integer;
begin
  Result := True;
  for i := 0 to Count -1 do
  begin
    Result := Patch[i].AcceptsNone(AffectedFields);
    if not Result then
      Break;
  end;
end;

procedure TQuilt.Clear;
begin
  BeginUpdate;
  try
    FPatchList.Clear;
  finally
    EndUpdate;
  end;
end;

function TQuilt.GetAffectedFieldSet: TDataFieldSet;
begin
  Result := FPatchList.AffectedFieldSet;
end;

procedure TQuilt.KeepDimensions(AffectedFields: TDataFieldSet);
var
  i : Integer;
  APatch : TQuiltPatch;
begin
  BeginUpdate;
  try
    for i := Count -1 downto 0 do
    begin
      APatch := Patch[i];
      if not APatch.FFields.Empty then
      begin
        APatch.KeepDimensions(AffectedFields);
{$ifdef D4_OR_HIGHER}
        if (not AcceptNonePatches and APatch.AcceptsNone) or
{$else}
        if (not AcceptNonePatches and APatch.AcceptsNone(nil)) or
{$endif D4_OR_HIGHER}
           APatch.AllAcceptsNone then
          FPatchList.Delete(i);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

function TQuilt.GetQuilt(var FreeQuilt: Boolean): TQuilt;
begin
  Result := Self;
  FreeQuilt := False;
end;

{$ifdef D4_OR_HIGHER}
function TQuilt.ContainsQuilt(AQuilt : TQuilt; AffectedFields : TDataFieldSet = nil) : Boolean;
{$else}
function TQuilt.ContainsQuilt(AQuilt : TQuilt; AffectedFields : TDataFieldSet) : Boolean;
{$endif D4_OR_HIGHER}
var
  i : Integer;
begin
  if AQuilt.Count = 0 then
    Result := True
  else if Self.Count = 0 then
    Result := False
  else
  begin
    Result := True;
    for i := 0 to AQuilt.Count -1 do
      if not ContainsPatch(AQuilt.Patch[i], AffectedFields) then
      begin
        Result := False;
        Break;
      end;
  end;
end;

{$ifdef D4_OR_HIGHER}
function TQuilt.ContainsPatch(APatch : TQuiltPatch; AffectedFields : TDataFieldSet = nil) : Boolean;
{$else}
function TQuilt.ContainsPatch(APatch : TQuiltPatch; AffectedFields : TDataFieldSet) : Boolean;
{$endif D4_OR_HIGHER}
var
  i : Integer;
  Dummy : Boolean;
begin
  Result := False;
  for i := 0 to Count -1 do
  begin
    if Patch[i].IsSubSet(APatch, Dummy, AffectedFields) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

{$ifdef D4_OR_HIGHER}
function TQuilt.CompareQuilt(AQuilt: TQuilt; AffectedFields : TDataFieldSet = nil): TCompareResult;
{$else}
function TQuilt.CompareQuilt(AQuilt: TQuilt; AffectedFields : TDataFieldSet): TCompareResult;
{$endif D4_OR_HIGHER}
var
  HadMatch : Boolean;
begin
  if IsSubSet(AQuilt, HadMatch, AffectedFields) then
  begin
    if AQuilt.IsSubSet(Self, HadMatch, AffectedFields) then
      Result := crEqual
    else
      Result := crSuperset;
  end
  else if not HadMatch then
    Result := crSeparate
  else if AQuilt.IsSubSet(Self, HadMatch, AffectedFields) then
    Result := crSubset
  else
    Result := crOverlapping;
end;

procedure TQuilt.PatchListChange(Sender: TObject);
begin
  Changed;
end;

procedure TQuilt.SetAcceptNonePatches(const Value: Boolean);
begin
  FAcceptNonePatches := Value;
end;

procedure TQuilt.ProcessFields(Proc: TProcFieldCond);
var
  i : Integer;
begin
  for i := 0 to Count -1 do
    Patch[i].ProcessFields( Proc );
end;

function TQuilt.ContainsCommon(AQuilt: TCommonQuilt): Boolean;
begin
  Result := False;
  if AQuilt is TQuilt then
{$ifdef D4_OR_HIGHER}
    Result := ContainsQuilt(TQuilt(AQuilt))
{$else}
    Result := ContainsQuilt(TQuilt(AQuilt),nil)
{$endif D4_OR_HIGHER}

  else if AQuilt is TQuiltPatch then
{$ifdef D4_OR_HIGHER}
    Result := ContainsPatch(TQuiltPatch(AQuilt))
{$else}
    Result := ContainsPatch(TQuiltPatch(AQuilt),nil)
{$endif D4_OR_HIGHER}

  else if AQuilt is TQuiltField then
{$ifdef D4_OR_HIGHER}
    Result := Compare(AQuilt) in [crEqual, crSuperset];
{$else}
    Result := Compare(AQuilt,nil) in [crEqual, crSuperset];
{$endif D4_OR_HIGHER}
end;

function TQuilt.CreateCommonCopy: TCommonQuilt;
begin
  Result := CreateQuiltCopy;
end;

function TQuilt.DoCreateUnion(AQuilt: TCommonQuilt; FreeParam : Boolean): TCommonQuilt;
begin
  Union(AQuilt);
  Result := Self;
  if FreeParam then
    AQuilt.Free;
end;

function TQuilt.DoCreateDifference(AQuilt: TCommonQuilt; FreeParam : Boolean): TCommonQuilt;
begin
  Difference(AQuilt);
  Result := Self;
  if FreeParam then
    AQuilt.Free;
end;

function TQuilt.DoCreateIntersection(AQuilt: TCommonQuilt; FreeParam : Boolean): TCommonQuilt;
begin
  Intersection(AQuilt);
  Result := Self;
  if FreeParam then
    AQuilt.Free;
end;

function TQuilt.CreateFieldTranslatedCopy(
  TranslateField: TQueryDataFieldFunction): TCondition;
var
  ACopy : TQuilt;
  NewPatch : TQuiltPatch;
  i : Integer;
begin
  ACopy := TQuilt.Create;
  for i := 0 to Count-1 do
  begin
    NewPatch := Patch[i].CreateFieldTranslatedPatchCopy(TranslateField);
    ACopy.AddPatch(NewPatch);
  end;
  Result := ACopy;
end;

function TQuilt.AcceptsAllForField(DataField: TDataField): Boolean;
var
  i : Integer;
begin
  Result := False;

  for i:= 0 to Count -1 do
    if Patch[i].AcceptsAllForField(DataField) then
    begin
      Result := True;
      Break;
    end;
end;

function TQuilt.AcceptsAllInTable(DataTable: TDataTAble): Boolean;
var
  i : Integer;
begin
  Result := False;

  for i:= 0 to Count -1 do
    if Patch[i].AcceptsAllInTable(DataTable) then
    begin
      Result := True;
      Break;
    end;
end;

function TQuilt.AcceptsNoValuesForField(DataField: TDataField): Boolean;
var
  i : Integer;
begin
  Result := True;

  for i:= 0 to Count -1 do
    if not Patch[i].AcceptsNoValuesForField(DataField) then
    begin
      Result := False;
      Break;
    end;
end;

function TQuilt.AcceptsNoValuesForTable(DataTable: TDataTable): Boolean;
var
  i : Integer;
begin
  Result := True;

  for i:= 0 to Count -1 do
    if not Patch[i].AcceptsNoValuesForTable(DataTable) then
    begin
      Result := False;
      Break;
    end;
end;

function TQuilt.IsPureAndCondition: Boolean;
begin
  Result := Count <= 1; 
end;

function TQuilt.ExtractOptimalQuilt: TCommonQuilt;
begin
  if Count = 1 then
  begin
    Result := Extract(Patch[0]).ExtractOptimalQuilt;
    Self.Free;
  end
  else
    Result := Self;
end;

{ TQuiltPatch }

function TQuiltPatch.HasConditionOnField(Field : TDataField) : Boolean;
var
  QuiltField : TQuiltField;
begin
  QuiltField := FindQuiltField(Field, False);
  if Assigned(QuiltField) then
{$ifdef D4_OR_HIGHER}
    Result := not QuiltField.AcceptsAll
{$else}
    Result := not QuiltField.AcceptsAll(nil)
{$endif D4_OR_HIGHER}
  else
    Result := False;
end;

{$ifdef D4_OR_HIGHER}
function TQuiltPatch.AcceptsAll(AffectedFields: TDataFieldSet = nil): Boolean;
{$else}
function TQuiltPatch.AcceptsAll(AffectedFields: TDataFieldSet): Boolean;
{$endif D4_OR_HIGHER}
var
  AQuiltField : TObject;
  ASet : TDataFieldSet;
begin
  Result := True;
  if FFields.Empty then
    Exit;

  if Assigned(AffectedFields) then
    ASet := AffectedFields
  else
    ASet := AffectedFieldSet;

  with TDataFieldSetIterator.Create(ASet) do
  begin
    while not EOF do
    begin
      if FFields.Contains( Field, AQuiltField ) and
{$ifdef D4_OR_HIGHER}
         not TQuiltField(AQuiltField).AcceptsAll then
{$else}
         not TQuiltField(AQuiltField).AcceptsAll(nil) then
{$endif D4_OR_HIGHER}
      begin
        Result := False;
        Break;
      end;
      Next;
    end;

    Free;
  end;
end;

function TQuiltPatch.AcceptsExactlyOneValue(DataField: TDataField;
  var Value: TValue): Boolean;
var
  AField : TQuiltField;
begin
  AField := FindQuiltField(DataField, False);
  if Assigned(AField) then
    Result := AField.AcceptsExactlyOneValue(DataField, Value)
  else
    Result := False;
end;

function TQuiltPatch.AllAcceptsNone: Boolean;
begin
  Result := False;
  with FFields.CreateIterator do
  begin
    while not EOF do
    begin
{$ifdef D4_OR_HIGHER}
      Result := QuiltField.AcceptsNone;
{$else}
      Result := QuiltField.AcceptsNone(nil);
{$endif D4_OR_HIGHER}
      if not Result then
        Break;
      Next;
    end;
    Free;
  end;
end;

{$ifdef D4_OR_HIGHER}
function TQuiltPatch.AcceptsNone(AffectedFields: TDataFieldSet = nil): Boolean;
{$else}
function TQuiltPatch.AcceptsNone(AffectedFields: TDataFieldSet): Boolean;
{$endif D4_OR_HIGHER}
begin
  Result := False;
  with FFields.CreateIterator do
  begin
    while not EOF do
    begin
      Result := QuiltField.AcceptsNone(AffectedFields);
      if Result then
        Break;
      Next;
    end;
    Free;
  end;
end;

function TQuiltPatch.AcceptsValue(AField: TDataField; AValue: TValue): Boolean;
var
  QuiltField : TQuiltField;
begin
  QuiltField := FindQuiltField( AField, False);
  if Assigned(QuiltField) then
    Result := QuiltField.AcceptsValue(AField, AValue)
  else
    Result := True;
end;

procedure TQuiltPatch.Clear;
begin
  BeginUpdate;
  try
    FFields.ClearAndFreeData;
  finally
    EndUpdate;
  end;
end;



procedure TQuiltPatch.InternalAddFrom(AQuiltPatch: TQuiltPatch; NeedMerge : Boolean);
var
  AField : TQuiltField;
begin
  BeginUpdate;
  try
    with AQuiltPatch.FFields.CreateIterator do
    begin
      while not EOF do
      begin
        AField := QuiltField;
        if NeedMerge then
          Self[AField.DataField].Union(AField)
        else
          InternalAddQuiltField(AField.CreateQuiltFieldCopy);
        Next;
      end;
      Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TQuiltPatch.AddFrom(AQuiltPatch: TQuiltPatch);
begin
{$ifdef D4_OR_HIGHER}
  InternalAddFrom(AQuiltPatch);
{$else}
  InternalAddFrom(AQuiltPatch,True);
{$endif D4_OR_HIGHER}
end;

procedure TQuiltPatch.CopyFrom(AQuiltPatch: TQuiltPatch);
begin
  BeginUpdate;
  try
    FFields.ClearAndFreeData;
    InternalAddFrom(AQuiltPatch, False);
  finally
    EndUpdate;
  end;
end;

procedure TQuiltPatch.IntersectionWithField(AQuiltField : TQuiltField);
begin
  BeginUpdate;
  try
    Self[AQuiltField.DataField].Intersection(AQuiltField);
  finally
    EndUpdate;
  end;
end;

procedure TQuiltPatch.Intersection(APatch : TQuiltPatch);
begin
  BeginUpdate;
  try
    with APatch.FFields.CreateIterator do
    begin
      while not EOF do
      begin
        IntersectionWithField(QuiltField);
        Next;
      end;
      Free;
    end;
  finally
    EndUpdate;
  end;
end;

constructor TQuiltPatch.Create;
begin
  inherited Create;

  FFields := TQuiltFieldList.Create;
end;

function TQuiltPatch.CreateQuiltPatchCopy: TQuiltPatch;
begin
  Result := TQuiltPatch.Create;
  Result.CopyFrom(Self);
end;

function TQuiltPatch.CreateCopy: TCondition;
begin
  Result := CreateQuiltPatchCopy;
end;

destructor TQuiltPatch.Destroy;
begin
  inherited;
  FFields.Free;
end;

procedure TQuiltPatch.InternalAddQuiltField(AQuiltField: TQuiltField);
begin
  if AQuiltField.Owner = nil then
    AQuiltField.SetOwner(Self)
  else if AQuiltField.Owner <> Self then
    raise Exception.Create( Self.ClassName + '.InternalAddQuiltField: AQuiltField already has an owner!' );
  FFields.Add(AQuiltField.DataField, AQuiltField);
end;

function TQuiltPatch.FindQuiltField(Field: TDataField;
  DoCreate: Boolean): TQuiltField;
var
  Data : TObject;
begin
  Data := nil;
  if FFields.Contains(Field, Data) then
    Result := TQuiltField(Data)
  else if DoCreate then
  begin
    Result := TQuiltField.Create(Field);
    InternalAddQuiltField(Result);
  end
  else
    Result := nil;
end;

function TQuiltPatch.GetQuiltField(ADataField: TDataField): TQuiltField;
begin
  Result := FindQuiltField( ADataField, True );
end;

procedure TQuiltPatch.SetQuiltField(ADataField: TDataField;
  const Value: TQuiltField);
begin
  Self[ADataField].CopyFrom( Value );
end;

function TQuiltPatch.GetAffectedFieldSet: TDataFieldSet;
begin
  Result := FFields.AffectedFieldSet;
end;

function TQuiltPatch.DifferenceMatched(AQuiltPatch: TQuiltPatch;
  ResultPieces : TList; var Overlap : TQuiltPatch) : Boolean;
var
  NewPatch : TQuiltPatch;
  TmpPieces : TList;
  AField : TDataField;
begin
  Overlap := nil;
{$ifdef D4_OR_HIGHER}
  if AQuiltPatch.AcceptsNone then
{$else}
  if AQuiltPatch.AcceptsNone(nil) then
{$endif D4_OR_HIGHER}
    Result := False
  else
  begin
    Result := True;
    TmpPieces := TList.Create;
    with TDataFieldSetIterator.Create( AQuiltPatch.AffectedFieldSet ) do
    begin
      Overlap := Self.CreateQuiltPatchCopy;
      while not EOF do
      begin
        AField := Field;
        NewPatch := Overlap.CreateQuiltPatchCopy;

{$ifdef D4_OR_HIGHER}
        Self[AField].Split(AQuiltPatch[AField],Overlap[AField], NewPatch[AField]);
{$else}
        Self[AField].Split(AQuiltPatch[AField],Overlap[AField], NewPatch[AField], nil, nil, nil);
{$endif D4_OR_HIGHER}
        Result := not Overlap[AField].AcceptsNone;
        if NewPatch.AcceptsNone then
          Newpatch.Free
        else
          TmpPieces.Add(NewPatch);

        if not Result then
        begin
          EmptyListWithObjects( TmpPieces );
          FreeAndNil(Overlap);
          Break;
        end;

        Next;
      end;
      Free;
    end;
    CopyListContent(TmpPieces, ResultPieces);
    TmpPieces.Free;
  end;
end;

procedure TQuiltPatch.KeepDimensions(AffectedFields: TDataFieldSet);
var
  AField : TDataField;
begin
  BeginUpdate;
  try
    if Assigned(AffectedFields) then
      with TDataFieldSetIterator.Create(AffectedFieldSet) do
      begin
        while not EOF do
        begin
          AField := Field;
          if not AffectedFields.ContainsField( AField ) then
            FFields.Remove( AField );
          Next;
        end;
        Free;
      end;
  finally
    EndUpdate;
  end;
end;

{$ifdef D4_OR_HIGHER}
function TQuiltPatch.AcceptsRow(ARow: TAbstractRow; AffectedFields: TDataFieldSet = nil): Boolean;
{$else}
function TQuiltPatch.AcceptsRow(ARow: TAbstractRow; AffectedFields: TDataFieldSet): Boolean;
{$endif D4_OR_HIGHER}
var
  QuiltField : TQuiltField;
  i : Integer;
  AField : TDataField;
begin
  Result := True;

  for i := 0 to ARow.ValidKeyCount -1 do
  begin
    AField := ARow.ValidKey[i];
    if not Assigned(AffectedFields) or AffectedFields.ContainsField(AField) then
    begin
      QuiltField := FindQuiltField( AField, False);
      if Assigned(QuiltField) then
        Result := QuiltField.AcceptsValue(AField, ARow[AField])
      else
        Result := True;
      if not Result then
        Break;
    end;
  end;

  if Result and (ARow is TDataRow) then
    for i := ARow.DataTable.KeyCount to ARow.DataTable.FieldCount -1 do
    begin
      AField := ARow.DataTable.Field[i];
      if not Assigned(AffectedFields) or AffectedFields.ContainsField(AField) then
      begin
        QuiltField := FindQuiltField( AField, False);
        if Assigned(QuiltField) then
          Result := QuiltField.AcceptsValue(AField, ARow[AField])
        else
          Result := True;
        if not Result then
          Break;
      end;
    end;
end;

{$ifdef D4_OR_HIGHER}
function TQuiltPatch.AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet = nil) : Boolean;
{$else}
function TQuiltPatch.AcceptsRowDependent(ARow : TAbstractRow; AffectedFields : TDataFieldSet) : Boolean;
{$endif D4_OR_HIGHER}
begin
  Result := True;
  with CreateIterator do
  begin
    while not EOF do
    begin
      if not QuiltField.AcceptsRowDependent(ARow, AffectedFields) then
      begin
        Result := False;
        Break;
      end;
      Next;
    end;
    Free;
  end;
end;

function TQuiltPatch.IsSubSet(AQuiltPatch: TQuiltPatch;
  var FoundMatch: Boolean; AffectedFields: TDataFieldSet = nil): Boolean;
var
  ASet : TDataFieldSet;
  SelfField, ParamField : TQuiltField;
  AField : TDataField;
begin
  if Assigned(AffectedFields) then
    ASet := AffectedFields
  else
    ASet := AffectedFieldSet;

  Result := True;
  with TDataFieldSetIterator.Create(ASet) do
  begin
    while not EOF and Result do
    begin
      AField := Field;
      SelfField := FindQuiltField(AField, False);
      if SelfField <> nil then
      begin
        if SelfField.AcceptsAll then
          Result := True
        else
      	 begin
          ParamField := AQuiltPatch.FindQuiltField(AField, False);
          if ParamField <> nil then
            Result := SelfField.IsSubSet(ParamField, FoundMatch)
      	   else
            Result := False;
        end;
      end
      else
        Result := True;
      Next;
    end;

    Free;
  end;
end;

function TQuiltPatch.GetQuilt(var FreeQuilt: Boolean): TQuilt;
begin
  Result := TQuilt.Create;
  Result.InternalAddPatch( CreateQuiltPatchCopy );
  FreeQuilt := True;
end;

procedure TQuiltPatch.ProcessFields(Proc: TProcFieldCond);
begin
  with FFields.CreateIterator do
  begin
    while not EOF do
    begin
      QuiltField.ProcessFields( Proc );
      Next;
    end;
    Free;
  end;
end;

function TQuiltPatch.ContainsCommon(AQuilt: TCommonQuilt): Boolean;
var
  ThisField, AField : TQuiltField;
begin
  if AQuilt is TQuilt then
  begin
    if TQuilt(AQuilt).Count = 0 then
    begin
      Result := True;
      Exit;
    end
    else if TQuilt(AQuilt).Count = 1 then
      AQuilt := TQuilt(AQuilt).Patch[0];
  end;

  if AQuilt is TQuiltPatch then
  begin
    with FFields.CreateIterator do
    begin
      Result := True;
      while not EOF do
      begin
        ThisField := QuiltField;
        AField := TQuiltPatch(AQuilt).FindQuiltField(ThisField.DataField, False);
        if (AField = nil) or
           (ThisField.CompareQuiltField(AField) in [crSeparate, crOverlapping, crSubSet]) then
        begin
          Result := False;
          Break;
        end;
        Next;
      end;

      Free;
    end;
  end
  else if AQuilt is TQuiltField then
  begin
    ThisField := TQuiltField(AQuilt);
    AField := FindQuiltField(ThisField.DataField, False);
    if AField = nil then
      Result := True
    else
      Result := ThisField.CompareQuiltField(AField) in [crEqual, crSuperSet];
  end
  else
    Result := False;
end;

function TQuiltPatch.CreateCommonCopy: TCommonQuilt;
begin
  Result := CreateQuiltPatchCopy;
end;

function TQuiltPatch.DoCreateUnion(AQuilt: TCommonQuilt; FreeParam : Boolean): TCommonQuilt;
var
  TmpQuilt : TQuilt;
begin
  if AQuilt is TQuilt then
  begin
    TmpQuilt := TQuilt(AQuilt);
    if not FreeParam then
      TmpQuilt := TmpQuilt.CreateQuiltCopy;
    TmpQuilt.AddPatch(Self);
    Result := AQuilt;
  end
  else
  begin
    TmpQuilt := TQuilt.Create;
    TmpQuilt.InternalAddPatch( Self );
    TmpQuilt.Union(AQuilt);
    if TmpQuilt.Count = 1 then
    begin
      Result := TmpQuilt.Extract(TmpQuilt.Patch[0]);
      TmpQuilt.Free;
    end
    else
      Result := TmpQuilt;

    if FreeParam then
      AQuilt.Free;
  end;
end;

function TQuiltPatch.DoCreateDifference(AQuilt: TCommonQuilt; FreeParam : Boolean): TCommonQuilt;
var
  AField : TQuiltField;
  TmpQuilt : TQuilt;
begin
  if (AQuilt is TQuiltField) then
  begin
    AField := TQuiltField(AQuilt);
    QuiltField[AField.DataField].Difference(AField);
    if FreeParam then
      AQuilt.Free;
    Result := Self;
  end
  else
  begin
    TmpQuilt := TQuilt.Create;
    TmpQuilt.InternalAddPatch( Self );
    TmpQuilt.Difference(AQuilt);
    if TmpQuilt.Count = 1 then
    begin
      Result := TmpQuilt.Extract(TmpQuilt.Patch[0]);
      TmpQuilt.Free;
    end
    else
      Result := TmpQuilt;

    if FreeParam then
      AQuilt.Free;
  end;
end;

function TQuiltPatch.DoCreateIntersection(AQuilt: TCommonQuilt; FreeParam : Boolean): TCommonQuilt;
var
  TmpQuilt : TQuilt;
  TmpField : TQuiltField;
begin
  if AQuilt is TQuilt then
  begin
    TmpQuilt := TQuilt(AQuilt);
    if not FreeParam then
      TmpQuilt := TmpQuilt.CreateQuiltCopy;
    TmpQuilt.Intersection(Self);
    Result := AQuilt;
    Self.Free;
  end
  else if AQuilt is TQuiltField then
  begin
    TmpField := TQuiltField(AQuilt);
    IntersectionWithField(TmpField);
    Result := Self;
    if FreeParam then
      AQuilt.Free;
  end
  else
  begin
    TmpQuilt := TQuilt.Create;
    TmpQuilt.InternalAddPatch( Self );
    TmpQuilt.Intersection(AQuilt);
    if TmpQuilt.Count = 1 then
    begin
      Result := TmpQuilt.Extract(TmpQuilt.Patch[0]);
      TmpQuilt.Free;
    end
    else
      Result := TmpQuilt;

    if FreeParam then
      AQuilt.Free;
  end;
end;

function TQuiltPatch.IsPureAndCondition: Boolean;
begin
  Result := True;
end;

function TQuiltPatch.CreateFieldTranslatedCopy(
  TranslateField: TQueryDataFieldFunction): TCondition;
begin
  Result := CreateFieldTranslatedPatchCopy(TranslateField);
end;

function TQuiltPatch.CreateFieldTranslatedPatchCopy(
  TranslateField: TQueryDataFieldFunction): TQuiltPatch;
var
  Copy : TQuiltPatch;
  SrcQuiltField : TQuiltField;
  SrcField, TransField : TDataField;
  ConflictAction : TConflictAction;
  KeepSrc, DestExist : Boolean;
begin
  Copy := TQuiltPatch.Create;

  with FFields.CreateIterator do
  begin
    while not EOF do
    begin
      ConflictAction := caOverwrite;
      KeepSrc := False;

      SrcQuiltField := QuiltField;
      SrcField := SrcQuiltField.DataField;

      TransField := TranslateField(SrcField, ConflictAction, KeepSrc);

      if TransField <> nil then
      begin
        DestExist := Copy.FindQuiltField(TransField, False) <> nil;

        if DestExist then
          case ConflictAction of
            caCurrent :;
            caOverwrite :
              Copy[TransField] := SrcQuiltField;
            caUnion :
              Copy[TransField].Union(SrcQuiltField);
            caIntersection :
              Copy[TransField].Intersection(SrcQuiltField);
            else
              raise Exception.Create( Self.ClassName + '.CreateFieldTranslatedPatchCopy: Unknown ConflictAction!' );
          end
        else
          Copy[TransField] := SrcQuiltField;

        if KeepSrc then
          Copy[SrcField].Union(SrcQuiltField);
      end;

      Next;
    end;
    Free;
  end;

  Result := Copy;
end;

procedure TQuiltPatch.AddValuesFromRowFields(ARow: TAbstractRow);
var
  i : Integer;
begin
  Assert(ARow <> nil, Self.ClassName + '.AddValuesFromRowFields: ARow = nil!');

  if ARow is TDataRow then
    for i := 0 to ARow.DataTable.FieldCount - 1 do
      QuiltField[ARow.DataTable.Field[i]].Add(ARow[ARow.DataTable.Field[i]])
  else
    for i := 0 to ARow.ValidKeyCount - 1 do
      QuiltField[ARow.ValidKey[i]].Add(ARow.ValidKeyValue[i]);
end;

procedure TQuiltPatch.AddValuesFromRowKeys(ARow : TAbstractRow);
var
  i : Integer;
begin
  Assert(ARow <> nil, Self.ClassName + '.AddValuesFromRowKeys: ARow = nil!');


  if ARow is TDataRow then
    for i := 0 to ARow.DataTable.KeyCount - 1 do
      QuiltField[ARow.DataTable.Field[i]].Add(ARow[ARow.DataTable.Field[i]])
  else
    for i := 0 to ARow.ValidKeyCount - 1 do
      QuiltField[ARow.ValidKey[i]].Add(ARow.ValidKeyValue[i]);
end;

function TQuiltPatch.FieldCount: Integer;
begin
  Result := FFields.ItemCount;
end;

function TQuiltPatch.CreateIterator: TQuiltFieldIterator;
begin
  Result := FFields.CreateIterator;
end;

type
  TSubTotalRowLink = class(TSubTotalRow);

procedure TQuiltPatch.InternalGetRows(Handler : TGetRowsHandler; SubTotal : TSubTotalRow);
var
  KeyField : TDataField;
  AValue : TValue;
begin
  if Handler.AllowExcludeOnSubTotalLevel then
  begin
    KeyField := SubTotal.SubTotalKey.TreeKey;

    if (not (Handler.LastLevelNeedCheckAllFields and SubTotal.IsLastTreeNode)) then
    begin
      if AcceptsExactlyOneValue(KeyField, AValue) then
        TSubTotalRowLink(SubTotal).__AddValue(Handler, AValue)
      else
        inherited InternalGetRows(Handler, SubTotal);
    end
    else
      inherited InternalGetRows(Handler, SubTotal);
  end
  else
    inherited InternalGetRows(Handler, SubTotal);
end;

function TQuiltPatch.AcceptsAllForField(DataField: TDataField): Boolean;
var
  tmpQField : TQuiltField;
begin
  tmpQField := FindQuiltField(DataField, False);
  if tmpQField <> nil then
    Result := tmpQField.AcceptsAll
  else
    Result := True;
end;

function TQuiltPatch.AcceptsAllInTable(DataTable: TDataTAble): Boolean;
begin
  Result := True;
  with CreateIterator do
  try
    while not EOF do
    begin
      if not QuiltField.AcceptsAllInTable(DataTable) then
      begin
        Result := False;
        Break;
      end;
      Next;
    end;
  finally
    Free;
  end;
end;

function TQuiltPatch.AcceptsNoValuesForField(
  DataField: TDataField): Boolean;
var
  tmpQField : TQuiltField;
begin
  tmpQField := FindQuiltField(DataField, False);
  if tmpQField <> nil then
    Result := tmpQField.AcceptsNone
  else
    Result := False;
end;

function TQuiltPatch.AcceptsNoValuesForTable(
  DataTable: TDataTable): Boolean;
begin
  Result := False;
  with CreateIterator do
  try
    while not EOF do
    begin
      if QuiltField.AcceptsNoValuesForTable(DataTable) then
      begin
        Result := True;
        Break;
      end;
      Next;
    end;
  finally
    Free;
  end;
end;

function TQuiltPatch.ExtractOptimalQuilt: TCommonQuilt;
var
  resQField, tmpQField : TQuiltField;
  FreeSelf : Boolean;
  Data : TObject;
begin
  Result := Self;
  resQField := nil;
  FreeSelf := False;
  with TDataFieldSetIterator.Create(AffectedFieldSet) do
    try
      while not EOF do
      begin
        tmpQField := FindQuiltField(Field, False);

        if tmpQField<>nil then
        begin
          if tmpQField.FValues.Initialized then
          begin
            if resQField <> nil then
            begin
              FreeSelf := False;
              Break;
            end
            else
            begin
              FreeSelf := True;
              resQField := tmpQField;
            end;
          end
          else
            FFields.Remove(Field);
        end;
        Next;
      end;
    finally;
      Free;
    end;

  if FreeSelf then
  begin
    FFields.Remove(resQField.DataField, Data);
    Result := TCommonQuilt(Data);
    Self.Free;
  end
  else if resQField = nil then
    Clear;
end;

{ TQuiltField }

constructor TQuiltField.Create(ADataField: TDataField);
begin
  CreateOwned(nil, ADataField);
end;

constructor TQuiltField.CreateOwned(Owner: TQuiltPatch;
  ADataField: TDataField);
begin
  inherited Create;

  FValues := TQuiltValueList.Create( ADataField );
  FValues.OnChange := ValueListChange;
  FAffectedFieldSet := TDataFieldSet.Create;
  FAffectedFieldSet.AddField(ADataField);
  Clear;
end;

destructor TQuiltField.Destroy;
begin
  inherited;
  FValues.Free;
  FAffectedFieldSet.Free;
end;

procedure TQuiltField.ValueListChange( Sender : TObject );
begin
  Changed;
end;

procedure TQuiltField.ExchangeSelections(AField : TQuiltField);
var
  AValues : TQuiltValueList;
  ANotifyEvent : TNotifyEvent;
begin
  BeginUpdate;
  try
    AValues := FValues;
    ANotifyEvent := FValues.OnChange;

    FValues := AField.FValues;
    AField.FValues := AValues;

    FValues.OnChange := AField.FValues.OnChange;
    AField.FValues.OnChange := ANotifyEvent;

    Changed;
    AField.Changed;
  finally
    EndUpdate;
  end;
end;

function TQuiltField.AcceptsAll(AffectedFields: TDataFieldSet = nil): Boolean;
begin
  if not FValues.Initialized then
    Result := True
  else if not Assigned(AffectedFields) or AffectedFields.ContainsField(DataField) then
  begin
    if FValues.Count <> 1 then
      Result := False
    else
      Result := QuiltPointEquals(StartPoint[0], NegInfinity) and
                QuiltPointEquals(EndPoint[0], PosInfinity);
  end
  else
    Result := True;
end;

{$ifdef D4_OR_HIGHER}
function TQuiltField.AcceptsRow(ARow: TAbstractRow; AffectedFields: TDataFieldSet = nil): Boolean;
{$else}
function TQuiltField.AcceptsRow(ARow: TAbstractRow; AffectedFields: TDataFieldSet): Boolean;
{$endif D4_OR_HIGHER}
begin
  if not FValues.Initialized then
    Result := True
//  else if (ARow is TSubtotalRow) and ARow.DataTable.TableHasNonKey(DataField) then
  else if (ARow is TSubTotalRow) and (not TSubTotalRow(ARow).FieldHasValue(DataField)) then
    Result := True
  else if not Assigned(AffectedFields) or AffectedFields.ContainsField(DataField) then
    Result := FValues.AcceptsValue(ARow[DataField])
  else
    Result := True;
end;

{$ifdef D4_OR_HIGHER}
function TQuiltField.AcceptsRowDependent(ARow: TAbstractRow; AffectedFields: TDataFieldSet = nil): Boolean;
{$else}
function TQuiltField.AcceptsRowDependent(ARow: TAbstractRow; AffectedFields: TDataFieldSet): Boolean;
{$endif D4_OR_HIGHER}
begin
  try
    Result := AcceptsRow(ARow, AffectedFields);
  except
    Result := True;
  end;
end;

function TQuiltField.AcceptsExactlyOneValue(DataField: TDataField;
  var Value: TValue): Boolean;
var
  AStartPoint : TQuiltPoint;
begin
  if (DataField <> Self.DataField) then
    Result := False
  else if (FValues.Count=1) then
  begin
    AStartPoint := FValues.StartPoint[0];
    Result := QuiltPointEquals( AStartPoint, FValues.EndPoint[0] );
    if Result then
      Value := AStartPoint.FValue;
  end
  else
    Result := False;
end;

function TQuiltField.AcceptsNone(AffectedFields: TDataFieldSet = nil): Boolean;
begin
  if not FValues.Initialized then
    Result := False
  else if not Assigned(AffectedFields) or AffectedFields.ContainsField(DataField) then
    Result := (FValues.Count=0)
  else
    Result := False;
end;

function TQuiltField.AcceptsValue(AField: TDataField; AValue: TValue): Boolean;
begin
  if not FValues.Initialized or (AField <> DataField) then
    Result := True
  else
    Result := FValues.AcceptsValue(AValue);
end;

function TQuiltField.DoAdd(const Value: TValue): Integer;
begin
  Result := DoAddInterval(QuiltPoint(Value), QuiltPoint(Value));
end;

function TQuiltField.DoAddInterval(Low, High: TQuiltPoint): Integer;
begin
  BeginUpdate;
  try
    Result := FValues.AddInterval( Low, High );
  finally
    EndUpdate;
  end;
end;

procedure TQuiltField.Clear;
begin
  BeginUpdate;
  try
    FValues.Clear;
  finally
    EndUpdate;
  end;
end;

procedure TQuiltField.CopyFrom(AQuiltField: TQuiltField);
begin
  if (AQuiltField.Count > 0) and (DataField.DataType <> AQuiltField.DataField.DataType) then
  begin
    Log(ltError, 'CopyFrom', 'TQuiltField.CopyFrom: Different DataField types!');
    Exit;
  end;

  BeginUpdate;
  try
    FValues.Clear;
    InternalAddFrom(AQuiltField, False);
  finally
    EndUpdate;
  end;
end;

function TQuiltField.CreateCopy: TCondition;
begin
  Result := CreateQuiltFieldCopy;
end;

function TQuiltField.CreateQuiltFieldCopy : TQuiltField;
begin
  Result := TQuiltField.Create(DataField);
  Result.CopyFrom(Self);
end;

procedure TQuiltField.Difference(AQuiltField: TQuiltField);
var
  ACopy : TQuiltField;
begin
  Assert( AQuiltField.DataField.DataType = DataField.DataType );
  BeginUpdate;
  try
    ACopy := TQuiltField.Create(DataField);
    Split(AQuiltField, nil, ACopy);
    ExchangeSelections(ACopy);
    ACopy.Free;
  finally
    EndUpdate;
  end;
end;

function TQuiltField.CompareQuiltField(AQuiltField : TQuiltField) : TCompareResult;
var
  HadIntersect, HadDiff, HadSplitDiff : Boolean;
begin
  if AQuiltField.DataField <> DataField then
    Result := crOverlapping
  else
  begin
    AQuiltField.DoSplit(Self, HadIntersect, HadDiff, HadSplitDiff);//intsectField, diffField, splitDiffField );
    if not HadIntersect then
      Result := crSeparate
    else if not HadDiff then
    begin
      if not HadSplitDiff then
        Result := crEqual
      else
        Result := crSubset;
    end
    else if not HadSplitDiff then
      Result := crSuperset
    else
      Result := crOverlapping;
  end;
end;

function TQuiltField.QuiltFieldsEqual( AQuiltField : TQuiltField ) : Boolean;
begin
  Result := Compare( AQuiltField ) = crEqual;
end;

function TQuiltField.GetAffectedFieldSet: TDataFieldSet;
begin
  Result := FAffectedFieldSet;
end;

{$ifdef QUILTCAPTION}
function TQuiltField.GetCaption: String;
begin
  Result := QCaptionHandler.GenerateCaptionFromQuiltField(Self);
end;
{$endif QUILTCAPTION}

function TQuiltField.GetDataField: TDataField;
begin
  Result := FValues.DataField;
end;

procedure TQuiltField.Split(SplitField : TQuiltField; aIntersect : TQuiltField;
  aDiff : TQuiltField = nil; aSplitDiff : TQuiltField = nil);
var
  dummy1, dummy2, dummy3 : Boolean;
begin
  if not Assigned(aDiff) and not Assigned(aIntersect) then
    raise Exception.Create( Self.ClassName + '.Split: You have to supply at least one of ' +
                            'the TQuiltFields to put the intersection or difference in!' );

  DoSplit( SplitField, dummy1, dummy2, dummy3, aIntersect, aDiff, aSplitDiff );
end;

procedure TQuiltField.DoSplit(SplitField : TQuiltField; out HadIntersect, HadDiff, HadSplitDiff : Boolean;
  aIntersect : TQuiltField = nil; aDiff : TQuiltField = nil; aSplitDiff : TQuiltField = nil);
var
  intsectValues, diffValues, splitDiffValues : TQuiltValueList;
begin
  HadIntersect := False;
  HadDiff := False;
  HadSplitDiff := False;

  Assert( Assigned(SplitField) and (SplitField.DataField.DataType = DataField.DataType) );

  if Assigned(aIntersect) then
  begin
    Assert( aIntersect.DataField.DataType = DataField.DataType );
    aIntersect.AcceptNone;
  end;

  if Assigned(aDiff) then
  begin
    Assert( aDiff.DataField.DataType = DataField.DataType );
    aDiff.AcceptNone;
  end;

  if Assigned(aSplitDiff) then
  begin
    Assert( aSplitDiff.DataField.DataType = DataField.DataType );
    aSplitDiff.AcceptNone;
  end;

  if AcceptsNone then
  begin
    HadSplitDiff := True;
    if aSplitDiff <> nil then
      aSplitDiff.CopyFrom(SplitField)
  end
  else if SplitField.AcceptsNone then
  begin
    HadDiff := True;
    if aDiff <> nil then
      aDiff.CopyFrom(Self);
  end
  else if AcceptsAll and SplitField.AcceptsAll then
  begin
    HadIntersect := True;
    if Assigned(aIntersect) then
      aIntersect.DoAddInterval(NegInfinity, PosInfinity);
  end
  else
  begin
    if aIntersect <> nil then
      intsectValues := aIntersect.FValues
    else
      intsectValues := nil;

    if aDiff <> nil then
      diffValues := aDiff.FValues
    else
      diffValues := nil;

    if aSplitDiff <> nil then
      splitDiffValues := aSplitDiff.FValues
    else
      splitDiffValues := nil;

    FValues.Split(SplitField.FValues, HadIntersect, HadDiff, HadSplitDiff, intsectValues, diffValues, splitDiffValues)
  end;
end;

procedure TQuiltField.Intersection(AQuiltField: TQuiltField);
var
  ACopy : TQuiltField;
begin
  Assert( AQuiltField.DataField.DataType = DataField.DataType );
  BeginUpdate;
  try
    ACopy := TQuiltField.Create(DataField);
    Split( AQuiltField, ACopy );
    ExchangeSelections(ACopy);
    ACopy.Free;
  finally
    EndUpdate;
  end;
end;

procedure TQuiltField.DoRemove(const Value: TValue);
begin
  DoRemoveInterval(QuiltPoint(Value), QuiltPoint(Value));
end;

procedure TQuiltField.KeepDimensions(AffectedFields: TDataFieldSet);
begin
  if not AffectedFields.ContainsField( DataField ) then
    Clear;
end;

procedure TQuiltField.DoRemoveInterval(Low, High: TQuiltPoint);
begin
  BeginUpdate;
  try
    FValues.RemoveInterval( Low, High );
  finally
    EndUpdate;
  end;
end;

procedure TQuiltField.Union(AQuiltField: TQuiltField);
begin
  InternalAddFrom(AQuiltField, True);
end;

procedure TQuiltField.InternalAddFrom(AQuiltField: TQuiltField; NeedMerge : Boolean);
var
  i : Integer;
begin
  if (AQuiltField.Count > 0) and (DataField.DataType <> AQuiltField.DataField.DataType) then
  begin
    Log(ltError, 'CopyFrom', 'TQuiltField.InternalAddFrom: Different DataField types!');
    Exit;
  end;

  BeginUpdate;
  try
    if AQuiltField.FValues.Initialized then
    begin
      if not FValues.Initialized then
        FValues.Initialized := True;

      if NeedMerge then
        for i := 0 to AQuiltField.FValues.Count -1 do
          DoAddInterval( AQuiltField.FValues.StartPoint[i], AQuiltField.FValues.EndPoint[i] )
      else
        for i := 0 to AQuiltField.FValues.Count -1 do
          FValues.InternalAddInterval( AQuiltField.FValues.StartPoint[i], AQuiltField.FValues.EndPoint[i] )
    end;
  finally
    EndUpdate;
  end;
end;

function TQuiltField.IsSubSet(AQuilt: TQuiltField; var FoundMatch: Boolean;
  AffectedFields: TDataFieldSet = nil): Boolean;
var
  dummy : Boolean;
begin
  if (AQuilt.DataField <> DataField) or
     (Assigned(AffectedFields) and not AffectedFields.ContainsField(DataField)) then
  begin
    Result := True;
    FoundMatch := True;
  end
  else
  begin
    if AcceptsAll( AffectedFields ) then
    begin
      Result := True;
      FoundMatch := True;
    end
    else
    begin
      AQuilt.DoSplit(Self, FoundMatch, Result, dummy);
      Result := not Result;
    end;
  end;
end;

function TQuiltField.GetCount: Integer;
begin
  Result := FValues.Count;
end;

function TQuiltField.GetEnd(Index: Integer): TQuiltPoint;
begin
  Result := FValues.EndPoint[Index];
end;

function TQuiltField.GetStart(Index: Integer): TQuiltPoint;
begin
  Result := FValues.StartPoint[Index];
end;

procedure TQuiltField.AcceptNone;
begin
  BeginUpdate;
  try
    FValues.DoClear;
    FValues.FInitialized := True;
  finally
    EndUpdate;
  end;
end;

procedure TQuiltField.AcceptAll;
begin
  BeginUpdate;
  try
    FValues.AddInterval(NegInfinity, PosInfinity);
    FValues.FInitialized := True;
  finally
    EndUpdate;
  end;
end;

function TQuiltField.GetQuilt(var FreeQuilt: Boolean): TQuilt;
var
  APatch : TQuiltPatch;
begin
  Result := TQuilt.Create;
  APatch := TQuiltPatch.Create;
  APatch[DataField] := Self;
  Result.InternalAddPatch(APatch);

  FreeQuilt := True;
end;

procedure TQuiltField.ProcessFields(Proc: TProcFieldCond);
begin
  Proc( DataField, AcceptsAll );
end;

function TQuiltField.GetIsInterval( Index : Integer ) : Boolean;
begin
  if (Index < 0) or (Index > FValues.Count) then
    raise Exception.Create( Self.ClassName + '.GetIsInterval: Index + ''' + IntToStr(Index) + ''' out of bounds!' );

  Result := not QuiltPointEquals( StartPoint[index], EndPoint[index] );
end;



function TQuiltField.GetSingleValue(idx: Integer): TValue;
begin
  if (idx < 0) or (idx > FValues.Count) then
    raise Exception.Create( Self.ClassName + '.GetSingleValue: Index + ''' + IntToStr(idx) + ''' out of bounds!' );

  if IsInterval[idx] then
    Log(ltError, 'Quilt', Self.ClassName + '.GetSingleValue: Tried to get single value from interval');

  Result := FValues.StartPoint[idx].FValue;
end;

function TQuiltField.ContainsCommon(AQuilt: TCommonQuilt): Boolean;
begin
  if AQuilt is TQuiltField then
    Result := CompareQuiltField(TQuiltField(AQuilt)) in [crEqual, crSuperset]
  else
    Result := False;
end;

function TQuiltField.CreateCommonCopy: TCommonQuilt;
begin
  Result := CreateQuiltFieldCopy;
end;

function TQuiltField.DoCreateUnion(AQuilt: TCommonQuilt; FreeParam : Boolean): TCommonQuilt;
var
  FreeQuilt : Boolean;
  TmpQuilt : TQuilt;
begin
  if AQuilt is TQuilt then
  begin
    TmpQuilt := TQuilt(AQuilt);
    if not FreeParam then
      TmpQuilt := TmpQuilt.CreateQuiltCopy;
    Result := TmpQuilt.DoCreateUnion(Self)
  end
  else if (AQuilt is TQuiltField) and (TQuiltField(AQuilt).DataField = DataField) then
  begin
    Result := Self;
    Self.Union(TQuiltField(AQuilt));
    if FreeParam then
      AQuilt.Free;
  end
  else
  begin
    TmpQuilt := GetQuilt(FreeQuilt);
    TmpQuilt.Union(AQuilt);
    Result := TmpQuilt;
    if FreeQuilt then
      Self.Free;
    if FreeParam then
      AQuilt.Free;
  end;
end;

function TQuiltField.DoCreateDifference(AQuilt: TCommonQuilt;
  FreeParam: Boolean): TCommonQuilt;
var
  FreeQuilt : Boolean;
  TmpQuilt : TQuilt;
begin
  if (AQuilt is TQuiltField) and (TQuiltField(AQuilt).DataField = DataField) then
  begin
    Result := Self;
    Difference(TQuiltField(AQuilt));
  end
  else
  begin
    TmpQuilt := GetQuilt(FreeQuilt);
    TmpQuilt.Difference(AQuilt);
    Result := TmpQuilt;
    if FreeQuilt then
      Self.Free;
  end;
  
  if FreeParam then
    AQuilt.Free;
end;

function TQuiltField.DoCreateIntersection(AQuilt: TCommonQuilt;
  FreeParam: Boolean): TCommonQuilt;
var
  FreeQuilt : Boolean;
  TmpQuilt : TQuilt;
  TmpPatch : TQuiltPatch;
begin
  if AQuilt is TQuilt then
  begin
    TmpQuilt := TQuilt(AQuilt);
    if not FreeParam then
      TmpQuilt := TmpQuilt.CreateQuiltCopy;
    Result := TmpQuilt.DoCreateIntersection(Self)
  end
  else if AQuilt is TQuiltPatch then
  begin
    TmpPatch := TQuiltPatch(AQuilt);
    if not FreeParam then
      TmpPatch := TmpPatch.CreateQuiltPatchCopy;
    Result := TmpPatch.DoCreateIntersection(Self)
  end
  else if (AQuilt is TQuiltField) and (TQuiltField(AQuilt).DataField = DataField) then
  begin
    Result := Self;
    Self.Intersection(TQuiltField(AQuilt));
    if FreeParam then
      AQuilt.Free;
  end
  else
  begin
    TmpQuilt := GetQuilt(FreeQuilt);
    TmpQuilt.Intersection(AQuilt);
    Result := TmpQuilt;
    if FreeQuilt then
      Self.Free;
    if FreeParam then
      AQuilt.Free;
  end;

  Result := Result.ExtractOptimalQuilt;
end;

function TQuiltField.IsPureAndCondition: Boolean;
begin
  Result := True;
end;

function TQuiltField.CreateFieldTranslatedCopy(
  TranslateField: TQueryDataFieldFunction): TCondition;
var
  NewField : TDataField;
  Copy : TQuiltField;
  ConflictAction : TConflictAction;
  KeepOld : Boolean;
begin
  // Fixa LGE jårsa ifall dest inte tillåter intervall men source gör det...

  NewField := TranslateField(Self.DataField, ConflictAction, KeepOld);
  if NewField = nil then
    Result := nil
  else
  begin
    Copy := TQuiltField.Create(NewField);
    Copy.CopyFrom(Self);
    Result := Copy;
  end;
end;

function TQuiltField.Add(const Value: TValue): Integer;
begin
  Result := DoAdd(Value);
end;

function TQuiltField.AddInterval(Low, High: TQuiltPoint): Integer;
begin
  if not DataField.Intervals and
     not QuiltPointEquals( Low, High ) then
    raise Exception.Create( Self.ClassName + '.AddInterval: Intervals not supported for the field ''' + DataField.FieldName + '''!' )
  else
    Result := DoAddInterval(Low, High);
end;

procedure TQuiltField.Remove(const Value: TValue);
begin
  DoRemove(Value);
end;

procedure TQuiltField.RemoveInterval(Low, High: TQuiltPoint);
begin
  if not DataField.Intervals and
     not QuiltPointEquals( Low, High ) then
    raise Exception.Create( Self.ClassName + '.RemoveInterval: Intervals not supported for the field ''' + DataField.FieldName + '''!' )
  else
    DoRemoveInterval(Low, High);
end;

{$ifdef QUILTCAPTION}
procedure TQuiltField.SetCaption(const Value: String);
begin
  QCaptionHandler.FillQuiltFieldFromCaption(Self, Value);
end;
{$endif QUILTCAPTION}

function TQuiltField.AcceptsAllForField(DataField: TDataField): Boolean;
begin
  if (Self.DataField <> DataField) then
    Result := True
  else
    Result := AcceptsAll;
end;

function TQuiltField.AcceptsAllInTable(DataTable: TDataTAble): Boolean;
begin
  if not DataTable.TableHasField(Self.DataField) then
    Result := True
  else
    Result := AcceptsAll;
end;

function TQuiltField.AcceptsNoValuesForField(
  DataField: TDataField): Boolean;
begin
  if (Self.DataField <> DataField) then
    Result := False
  else
    Result := AcceptsNone;
end;

function TQuiltField.AcceptsNoValuesForTable(
  DataTable: TDataTable): Boolean;
begin
  if not DataTable.TableHasField(Self.DataField) then
    Result := False
  else
    Result := AcceptsNone;
end;

function TQuiltField.ExtractOptimalQuilt: TCommonQuilt;
begin
  Result := Self;
end;

{ TQuiltPatchList }

function TQuiltPatchList.Add(APatch: TQuiltPatch): Integer;
begin
  Result := FList.Add(APatch);
  Changed;
end;

procedure TQuiltPatchList.Clear;
begin
  FList.Clear;
  Changed;
end;

constructor TQuiltPatchList.Create;
begin
  inherited;
  FList := TObjectList.Create(True);
  FAffectedFieldSetUpToDate := False;
end;

procedure TQuiltPatchList.Delete(Index: Integer);
begin
  FList.Delete(Index);
  Changed;
end;

destructor TQuiltPatchList.Destroy;
begin
  inherited;
  FAffectedFieldSet.Free;
  FList.Free;
end;

function TQuiltPatchList.Extract(APatch: TQuiltPatch): TQuiltPatch;
begin
  Result := TQuiltPatch( FList.Extract(APatch) );
  Changed;
end;

function TQuiltPatchList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TQuiltPatchList.GetPatch(Index: Integer): TQuiltPatch;
begin
  Result := TQuiltPatch(FList[Index]);
end;

function TQuiltPatchList.IndexOf(APatch: TQuiltPatch): Integer;
begin
  Result := FList.IndexOf(APatch);
end;

function TQuiltPatchList.Remove(APatch: TQuiltPatch): Integer;
begin
  Result := FList.Remove(APatch);
  Changed;
end;

function TQuiltPatchList.GetAffectedFieldSet: TDataFieldSet;
begin
  if not Assigned(FAffectedFieldSet) then
    FAffectedFieldSet := TDataFieldSet.Create;

  if not FAffectedFieldSetUpToDate then
    DoUpdateAffectedFieldSet;

  Result := FAffectedFieldSet;
end;

procedure TQuiltPatchList.UpdateAffectedFieldSet;
begin
  FAffectedFieldSetUpToDate := False;
end;

procedure TQuiltPatchList.DoUpdateAffectedFieldSet;
var
  i : Integer;
begin
  FAffectedFieldSet.Clear;
  for i := 0 to Count-1 do
    FAffectedFieldSet.AddFrom( Patch[i].AffectedFieldSet );
  FAffectedFieldSetUpToDate := True;
end;

procedure TQuiltPatchList.Changed;
begin
  UpdateAffectedFieldSet;
  if Assigned( OnChange ) then
    OnChange( Self );
end;

procedure TQuiltPatchList.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

{ TQuiltFieldList }

procedure TQuiltFieldList.Clear;
begin
  ClearAndFreeData;
end;

constructor TQuiltFieldList.Create;
begin
  inherited Create(HASHSIZE, True);
  FAffectedFieldSetUpToDate := False;
end;

function TQuiltFieldList.CreateIterator: TQuiltFieldIterator;
begin
  Result := TQuiltFieldIterator.Create( Self );
end;

destructor TQuiltFieldList.Destroy;
begin
  ClearAndFreeData;
  inherited;
  FAffectedFieldSet.Free;
end;

procedure TQuiltFieldList.Add(Index: Pointer; Data: TObject);
begin
  inherited;
  Changed;
end;

function TQuiltFieldList.Remove(Index: Pointer): Boolean;
var
  Data : TObject;
begin
  Result := Remove(Index, Data);
  if Result and Assigned(Data) then
    Data.Free;
end;

function TQuiltFieldList.Remove(Index: Pointer;
  var Data: TObject): Boolean;
begin
  Result := inherited Remove(Index, Data);
  Changed;
end;

procedure TQuiltFieldList.DoUpdateAffectedFieldSet;
begin
  FAffectedFieldSet.Clear;
  with CreateIterator do
  begin
    while not EOF do
    begin
      FAffectedFieldSet.AddField( QuiltField.DataField );
      Next;
    end;
    Free;
  end;
  FAffectedFieldSetUpToDate := True;
end;

procedure TQuiltFieldList.UpdateAffectedFieldSet;
begin
  FAffectedFieldSetUpToDate := False;
end;

procedure TQuiltFieldList.Changed;
begin
  UpdateAffectedFieldSet;
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TQuiltFieldList.ClearAndFreeData;
begin
  inherited;
  Changed;
end;

function TQuiltFieldList.GetAffectedFieldSet: TDataFieldSet;
begin
  if not Assigned(FAffectedFieldSet) then
    FAffectedFieldSet := TDataFieldSet.Create;

  if not FAffectedFieldSetUpToDate then
    DoUpdateAffectedFieldSet;

  Result := FAffectedFieldSet;
end;

procedure TQuiltFieldList.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

{ TQuiltValueList }

constructor TQuiltValueList.Create(ADataField : TDataField);
begin
  Assert(ADataField <> nil, 'TQuiltValueList.Create: DataField should not be nil!');

  inherited Create;

  FDataField := ADataField;
  FInitialized := False;
end;

destructor TQuiltValueList.Destroy;
begin
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

procedure TQuiltValueList.Error(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

function TQuiltValueList.AddStartPoint(const S: TQuiltPoint): Integer;
begin
  if Find(S, Result) then
  begin
    if StartPoint[Result].FClosed = S.FClosed then
      raise Exception.Create( Self.ClassName + '.AddStartPoint: Start point already exists!' )
    else if not S.FClosed then
      Inc(Result);
  end;

  InsertItem(Result, S);
end;

procedure TQuiltValueList.SetStartPoint(Index: Integer;
  const S: TQuiltPoint);
begin
  if (Index < 0) or (Index >= FCount) then
{$ifdef USERESOURCESTRING}
    Error(SListIndexError);
{$else}
    Error('SListIndexError');
{$endif USERESOURCESTRING}

  FList^[Index].FStart := S;
end;

procedure TQuiltValueList.SetEndPoint(Index: Integer; const S: TQuiltPoint);
begin
  if (Index < 0) or (Index >= FCount) then
{$ifdef USERESOURCESTRING}
    Error(SListIndexError);
{$else}
    Error('SListIndexError');
{$endif LINUX}

  FList^[Index].FEnd := S;
end;

procedure TQuiltValueList.InsertItem(Index: Integer; const S: TQuiltPoint);
begin
{$ifndef LINUX}
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TQuiltValueItem));

  FillMemory(@FList^[Index], SizeOf(TQuiltValueItem), Byte(0));

  with FList^[Index] do
  begin
    FStart := S;
    FEnd := S;
  end;
  Inc(FCount);
{$endif LINUX}
end;

procedure TQuiltValueList.Clear;
begin
  if Initialized then
  begin
    Initialized := False;
    DoClear;
    Changed;
  end;
end;

procedure TQuiltValueList.DoClear;
begin
  if FCount <> 0 then
  begin
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
  end;
end;

function TQuiltValueList.CompareEndToHigh( EndPoint, High : TQuiltPoint ) : Integer;
begin
  if EndPoint.FInfinity then
  begin
    if High.FInfinity then
      Result := 0
    else
      Result := 1;
  end
  else if High.FInfinity then
    Result := -1
  else
  begin
    Result := Compare(EndPoint, High);
    if (Result=0) and (EndPoint.FClosed <> High.FClosed) then
    begin
      if EndPoint.FClosed then
        Result := 1
      else
        Result := -1;
    end;
  end;
end;

function TQuiltValueList.CompareStartToLow( StartPoint, Low : TQuiltPoint ) : Integer;
begin
  if StartPoint.FInfinity then
  begin
    if Low.FInfinity then
      Result := 0
    else
      Result := -1;
  end
  else if Low.FInfinity then
    Result := 1
  else
  begin
    Result := Compare(StartPoint, Low);
    if (Result=0) and (StartPoint.FClosed <> Low.FClosed) then
    begin
      if StartPoint.FClosed then
        Result := -1
      else
        Result := 1;
    end;
  end;
end;

function TQuiltValueList.CompareStartToHigh( StartPoint, High : TQuiltPoint ) : Integer;
begin
  if StartPoint.FInfinity and High.FInfinity then
    Result := -1
  else
  begin
    Result := Compare(StartPoint, High);
    if (Result=0) then
    begin
      if StartPoint.FClosed and High.FClosed then
        Result := 0
      else if not StartPoint.FClosed then
        Result := 1
      else
        Result := 1;
    end;
  end;
end;

procedure TQuiltValueList.CheckIntervalLegal(Low, High: TQuiltPoint);
var
  compRes : Integer;
begin
  compRes := CompareStartToHigh( Low, High );
  if compRes > 0 then
    Log(ltError, 'Intervals', 'Illegal interval from ''' + AsString(Low.FValue) + ''' to ''' + AsString(High.FValue) +'''!')
  else if compRes = 0 then
  begin
    if Low.FInfinity then
      Log(ltError, 'Intervals', 'Interval can not have both end points in positive or negative infinity!')
    else if not Low.FClosed or not High.FClosed then
      Log(ltError, 'Intervals', 'Single value can not have open end points!');
  end;
//  else if not DataField.Intervals then
//    Log(ltError, 'Intervals', 'Intervals not supported!');
end;

procedure TQuiltValueList.RemoveInterval(Low, High: TQuiltPoint);
var
  ACopy, RemoveList : TQuiltValueList;
  dummy1, dummy2, dummy3 : Boolean;
begin
  if not Low.FInfinity then
    Low.FValue := DataField.DataType.Optimize(Low.FValue);
  if not High.FInfinity then
    High.FValue := DataField.DataType.Optimize(High.FValue);

  RemoveList := TQuiltValueList.Create(DataField);
  RemoveList.InternalAddInterval(Low, High);

  ACopy := TQuiltValueList.Create(DataField);
  ACopy.FList := FList;
  ACopy.FCount := FCount;
  ACopy.FCapacity := FCapacity;
  ACopy.FInitialized := FInitialized;

  FList := nil;
  FCount := 0;
  FCapacity := 0;
  FInitialized := True;
  ACopy.Split(RemoveList, dummy1, dummy2, dummy3, nil, Self);

  ACopy.Free;
  RemoveList.Free;
end;

type
  TIsLowType = (iltSelf, iltSplit, iltNone);

procedure TQuiltValueList.Split(SplitList: TQuiltValueList;
  out HadIntersect, HadDiff, HadSplitDiff : Boolean;
  IntersectList: TQuiltValueList = nil; DiffList : TQuiltValueList = nil; SplitDiffList : TQuiltValueList = nil);

  procedure AddSelfDiff( Low, High : TQuiltPoint );
  begin
    HadDiff := True;
    if Assigned( DiffList ) then
      DiffList.InternalAddInterval(Low, High);
  end;

  procedure AddSplitDiff( Low, High : TQuiltPoint );
  begin
    HadSplitDiff := True;
    if Assigned( SplitDiffList ) then
      SplitDiffList.InternalAddInterval(Low, High);
  end;

  procedure AddIntersection( Low, High : TQuiltPoint );
  begin
    HadIntersect := True;
    if Assigned( IntersectList ) then
      IntersectList.InternalAddInterval(Low, High);
  end;

  function DoExit : Boolean;
  begin
    Result := HadDiff and HadIntersect and HadSplitDiff and
       not Assigned(DiffList) and not Assigned(IntersectList) and not Assigned(SplitDiffList);
  end;
  
var
  CurrLow : TQuiltPoint;
  iSelf, iSplit, compRes, compHighSelfVsSplit : Integer;
  LowSelf, LowSplit, HighSelf, HighSplit : TQuiltPoint;
  IsLow : TIsLowType;
begin
  HadIntersect := False;
  HadDiff := False;
  HadSplitDiff := False;

  // Kludge for uninitialized lists
  if not Initialized then
  begin
    iSelf := AddStartPoint(NegInfinity);
    SetEndPoint(iSelf, PosInfinity);
  end;

  if not SplitList.Initialized then
  begin
    iSplit := SplitList.AddStartPoint(NegInfinity);
    SplitList.SetEndPoint(iSplit, PosInfinity);
  end;


  iSelf := 0;
  iSplit := 0;
  IsLow := iltNone;

  while (iSplit < SplitList.Count) and (iSelf < Count) do
  begin
    case IsLow of
      iltSelf :
      begin
        LowSelf := CurrLow;
        LowSplit := SplitList.StartPoint[iSplit];
        HighSplit := SplitList.EndPoint[iSplit];
      end;
      iltSplit :
      begin
        LowSplit := CurrLow;
        LowSelf := StartPoint[iSelf];
        HighSelf := EndPoint[iSelf];
      end;
      else
      begin
        LowSelf := StartPoint[iSelf];
        HighSelf := EndPoint[iSelf];
        LowSplit := SplitList.StartPoint[iSplit];
        HighSplit := SplitList.EndPoint[iSplit];
      end;
    end;

    compRes := CompareStartToLow(LowSelf, LowSplit);
    compHighSelfVsSplit := CompareEndToHigh(HighSelf, HighSplit);

    if compRes <= 0 then
    begin
      if CompareStartToHigh(LowSplit, HighSelf) <= 0 then
      begin
        if compRes < 0 then
          AddSelfDiff( LowSelf, ToggleClosed(LowSplit) );

        if compHighSelfVsSplit <= 0 then
        begin
          AddIntersection(LowSplit, HighSelf);
          if compHighSelfVsSplit = 0 then
            IsLow := iltNone
          else
          begin
            CurrLow := ToggleClosed(HighSelf);
            IsLow := iltSplit;
          end;
        end
        else
        begin
          AddIntersection(LowSplit, HighSplit);
          CurrLow := ToggleClosed(HighSplit);
          IsLow := iltSelf;
        end;
      end
      else
      begin
        AddSelfDiff( LowSelf, HighSelf );
        IsLow := iltNone;
      end;
    end
    else if CompareStartToHigh(LowSelf, HighSplit) <= 0 then
    begin
      if compRes > 0 then
        AddSplitDiff( LowSplit, ToggleClosed(LowSelf) );

      if compHighSelfVsSplit <= 0 then
      begin
        AddIntersection(LowSelf, HighSelf);
        if compHighSelfVsSplit = 0 then
          IsLow := iltNone
        else
        begin
          CurrLow := ToggleClosed(HighSelf);
          IsLow := iltSplit;
        end;
      end
      else
      begin
        AddIntersection(LowSelf, HighSplit);
        CurrLow := ToggleClosed(HighSplit);
        IsLow := iltSelf;
      end;
    end
    else
    begin
      AddSplitDiff( LowSplit, HighSplit );
      IsLow := iltNone;
    end;

    if DoExit then
      Break;

    if compHighSelfVsSplit = 0 then
    begin
      Inc(iSelf);
      Inc(iSplit);
    end
    else if compHighSelfVsSplit > 0 then
      Inc(iSplit)
    else
      Inc(iSelf);
  end;

  if not DoExit then
  begin
    if (iSplit = SplitList.Count) and (IsLow = iltSelf) then
    begin
      AddSelfDiff( CurrLow, HighSelf );
      Inc(iSelf);
    end
    else if (iSelf = Count) and (IsLow = iltSplit) then
    begin
      AddSplitDiff( CurrLow, HighSplit );
      Inc(iSplit);
    end;

    while (iSelf < Count) and (Assigned(DiffList) or not HadDiff) do
    begin
      AddSelfDiff( StartPoint[iSelf], EndPoint[iSelf] );
      Inc(iSelf);
    end;

    while (iSplit < SplitList.Count) and (Assigned(SplitList) or not HadSplitDiff) do
    begin
      AddSplitDiff( SplitList.StartPoint[iSplit], SplitList.EndPoint[iSplit] );
      Inc(iSplit);
    end;
  end;

  // Kludge for uninitialized lists
  if not Initialized then
    DoClear;

  if not SplitList.Initialized then
    SplitList.DoClear;
end;

function TQuiltValueList.InternalAddInterval(Low, High : TQuiltPoint) : Integer;
begin
  FInitialized := True;

  Result := AddStartPoint(Low);
  SetEndPoint(Result, High);
  Changed;
end;

function TQuiltValueList.SmallerOrTouch(Point1, Point2 : TQuiltPoint ) : TSmallerOrTouchResult;
var
  compRes : Integer;
begin
  compRes := Compare( Point1, Point2 );
  if compRes < 0 then
    Result := sotSmaller
  else if (compRes=0) and (Point1.FClosed or Point2.FClosed) then
    Result := sotTouch
  else
    Result := sotLarger;
end;

procedure TQuiltValueList.JoinIntervals(StartIndex : Integer);
var
  compRes, idx : Integer;
  CurrentEnd, AEnd : TQuiltPoint;
begin
  if (StartIndex < 0) or (StartIndex >= Count) then
    Exit;

  CurrentEnd := EndPoint[StartIndex];
  idx := StartIndex + 1;

  while (idx < Count) and
        (SmallerOrTouch(StartPoint[idx], CurrentEnd) in [sotSmaller, sotTouch]) do
  begin
    AEnd := EndPoint[idx];
    compRes := CompareEndToHigh(AEnd, CurrentEnd);
    Inc(idx);
    if compRes < 0 then
      Continue;

    CurrentEnd := AEnd;
    Break;
  end;

  if idx - StartIndex > 1 then
  begin
    Dec(idx);
    while idx > StartIndex do
    begin
      Delete(idx);
      Dec(idx);
    end;
    SetEndPoint(StartIndex, CurrentEnd);
  end;
end;

function TQuiltValueList.AddValue(Value: TValue): Integer;
var
  match : boolean;
  APoint : TQuiltPoint;
  sotResult : TSmallerOrTouchResult;
begin
  FInitialized := True;

  Value := DataField.DataType.Optimize(Value);
  APoint := QuiltPoint(Value);

  if Count=0 then
    InternalAddInterval(APoint, APoint)
  else
  begin
    match := Find(APoint, Result);

    if (Result>0) then
      sotResult := SmallerOrTouch(APoint, EndPoint[Result-1])
    else
      sotResult := sotLarger;

    if match then
    begin
      if sotResult in [sotSmaller, sotTouch] then
      begin
        SetEndPoint(Result-1,EndPoint[Result]);
        Delete(Result);
        Dec(Result);
      end
      else
        SetStartPoint(Result, APoint);
    end
    else if sotResult = sotTouch then
    begin
      Dec(Result);
      SetEndPoint(Result, APoint);
    end
    else if sotResult = sotLarger then
      Result := InternalAddInterval(APoint, APoint)
    else
      Exit;
  end;
  Changed;
end;

function TQuiltValueList.AddInterval(Low, High : TQuiltPoint) : Integer;
var
  match : boolean;
begin
  FInitialized := True;
  if not Low.FInfinity then
    Low.FValue := DataField.DataType.Optimize(Low.FValue);
  if not High.FInfinity then
    High.FValue := DataField.DataType.Optimize(High.FValue);

  CheckIntervalLegal(Low, High);

  if Count=0 then
    InternalAddInterval(Low, High)
  else
  begin
    match := Find(Low, Result);

    if (Result>0) and (SmallerOrTouch(Low, EndPoint[Result-1]) in [sotSmaller, sotTouch]) then
    begin
      Match := True;
      Dec(Result)
    end;

    if Match then
    begin
     if CompareStartToLow(Low, StartPoint[Result]) < 0 then
       SetStartPoint(Result, Low);

     if CompareEndToHigh( High, EndPoint[Result] ) > 0 then
       SetEndPoint(Result, High);
    end
    else
    begin
      Result := AddStartPoint(Low);
      SetEndPoint(Result, High);
    end;

    JoinIntervals(Result);
  end;
  Changed;
end;

function TQuiltValueList.AcceptsValue(Value : TValue) : Boolean;
var
  pos : Integer;
  APoint : TQuiltPoint;
begin
  APoint := QuiltPoint(Value);

  if not Initialized then
    Result := True
  else if Find(APoint, pos) then
  begin
    if StartPoint[pos].FClosed then
      Result := True
(*    else if pos > 0 then
      Result := CompareEndToHigh(EndPoint[pos-1], APoint) >= 0*)
    else
      Result := False;
  end
  else if pos > 0 then
    Result := IntervalAtIdxAcceptsInterval(pos-1, APoint, APoint)
  else
    Result := False;
end;

function TQuiltValueList.IntervalAtIdxAcceptsInterval(Idx : Integer; Low, High : TQuiltPoint) : Boolean;
begin
  if not Initialized then
    Result := True
  else
    Result := (CompareStartToLow(StartPoint[idx], Low) <= 0) and
              (CompareEndToHigh(EndPoint[idx],High) >= 0);
end;

function TQuiltValueList.AcceptsInterval(Low, High : TQuiltPoint) : Boolean;
var
  pos : Integer;
begin
  if not Initialized then
    Result := True
  else if Find(Low, pos) then
    Result := IntervalAtIdxAcceptsInterval(pos, Low, High)
  else if pos > 0 then
    Result := IntervalAtIdxAcceptsInterval(pos-1, Low, High)
  else
    Result := False;
end;

procedure TQuiltValueList.Delete(Index: Integer);
begin
  Assert((Index >= 0) and (Index < FCount),
         'TQuiltValueList.Delete: (Index >= 0) and (Index < FCount), Index: ' + IntToStr(Index));

  if (Index < 0) or (Index >= FCount) then Error({$ifdef USERESOURCESTRING}SListIndexError{$else}'SListIndexError'{$endif USERESOURCESTRING});
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TQuiltValueItem));
  Changed;
end;

procedure TQuiltValueList.Exchange(Index1, Index2: Integer);
begin
  Assert((Index1 >= 0) and (Index1 < FCount),
         'TQuiltValueList.Exchange: (Index1 >= 0) and (Index1 < FCount), Index1: ' + IntToStr(Index1));
  Assert((Index2 >= 0) and (Index2 < FCount),
         'TQuiltValueList.Exchange: (Index2 >= 0) and (Index2 < FCount), Index2: ' + IntToStr(Index2));

  if (Index1 < 0) or (Index1 >= FCount) then Error({$ifdef USERESOURCESTRING}SListIndexError{$else}'SListIndexError'{$endif USERESOURCESTRING});
  if (Index2 < 0) or (Index2 >= FCount) then Error({$ifdef USERESOURCESTRING}SListIndexError{$else}'SListIndexError'{$endif USERESOURCESTRING});
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TQuiltValueList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: TQuiltPoint;
  Item1, Item2: PQuiltValueItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];

  Temp := Item1^.FStart;
  Item1^.FStart := Item2^.FStart;
  Item2^.FStart := Temp;

  Temp := Item1^.FEnd;
  Item1^.FEnd := Item2^.FEnd;
  Item2^.FEnd := Temp;
end;

function TQuiltValueList.Find(const S: TQuiltPoint; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Compare(FList^[I].FStart, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TQuiltValueList.Get1(Index: Integer): TQuiltPoint;
begin
  if (Index < 0) or (Index >= FCount) then Error({$ifdef USERESOURCESTRING}SListIndexError{$else}'SListIndexError'{$endif USERESOURCESTRING});
  Result := FList^[Index].FStart;
end;

function TQuiltValueList.Get2(Index: Integer): TQuiltPoint;
begin
  Assert((Index >= 0) and (Index < FCount),
         'TQuiltValueList.Get2: (Index >= 0) and (Index < FCount), Index: ' + IntToStr(Index));

  if (Index < 0) or (Index >= FCount) then Error({$ifdef USERESOURCESTRING}SListIndexError{$else}'SListIndexError'{$endif USERESOURCESTRING});
  Result := FList^[Index].FEnd;
end;

function TQuiltValueList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TQuiltValueList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error({$ifdef USERESOURCESTRING}SListIndexError{$else}'SListIndexError'{$endif USERESOURCESTRING});
  Result := nil;
end;

procedure TQuiltValueList.Grow;
var
  NewSize: Integer;
begin
  if FCapacity < 16 then
    NewSize := 16
  else
    NewSize := FCapacity * 2;

  SetCapacity(NewSize);
end;

function TQuiltValueList.IndexOf(const S: TQuiltPoint): Integer;
begin
  if not Find(S, Result) then
    Result := -1;
end;

{ Compare V1 to V2
   - If V1 > V2 then Result > 1
   - If V1 = V2 then Result = 0
   - If V1 < V2 then Result < 0

   Negative infinity has the closed boolean true and
   positive infinity has it false.                  }
function TQuiltValueList.Compare(V1, V2 : TQuiltPoint) : Integer;
begin
  if V1.FInfinity then
  begin
    if V2.FInfinity and (V1.FClosed = V2.FClosed) then
      Result := 0
    else if V1.FClosed then
      Result := -1
    else
      Result := 1;
  end
  else if V2.FInfinity then
  begin
    if V2.FClosed then
      Result := 1
    else
      Result := -1;
  end
  else
    Result := DataField.DataType.Compare(V1.FValue, V2.FValue);
end;

procedure TQuiltValueList.QuickSort(L, R: Integer);
var
  I, J: Integer;
  P: TQuiltPoint;
begin
  repeat
    I := L;
    J := R;
    P := FList^[(L + R) shr 1].FStart;
    repeat
      while Compare(FList^[I].FStart, P) < 0 do Inc(I);
      while Compare(FList^[J].FStart, P) > 0 do Dec(J);
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

procedure TQuiltValueList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TQuiltValueItem));
  FCapacity := NewCapacity;
end;

procedure TQuiltValueList.Changed;
begin
  if Assigned( OnChange ) then
    OnChange( Self );
end;

procedure TQuiltValueList.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

{ TQuiltFieldIterator }

function TQuiltFieldIterator.GetQuiltField: TQuiltField;
begin
  Result := TQuiltField(Data);
end;

initialization
  NegInfinity := QuiltPointWithInfinity(ZeroVal, True, True);
  PosInfinity := QuiltPointWithInfinity(ZeroVal, False, True);

end.



