unit DFilters;

interface

uses Classes, Graphics, SysUtils, DB, DUtils;

const
  cnsWordBoolTrue       = '-1';
  cnsWordBoolFalse      = '0';

  cnsAllItem            = '<all>';
  cnsLogNone            = 'NONE';
  cnsLogEnd             = 'END';
  cnsLogAnd             = 'AND';
  cnsLogOr              = 'OR';
  cnsLogAndNot          = 'AND NOT';
  cnsLogOrNot           = 'OR NOT';

  cnsSignNone           = '  ';
  cnsSignEqual          = '= ';
  cnsSignNotEqual       = '<>';
  cnsSignBigger         = '> ';
  cnsSignBiggerEqual    = '>=';
  cnsSignSmaller        = '< ';
  cnsSignSmallerEqual   = '<=';
  cnsSignJokerRight     = 'A*';
  cnsSignJokerLeft      = '*A';
  cnsSignJokerSub       = '**';
  cnsSignSpecEqual      = '==';

type
  TFilterType   = (ffNone,
                   ffString,
                   ffNumeric,
                   ffDate,
                   ffLogical,
                   ffCustom,
                   ffMarker);
  TRelationType = (frNone,
                   frEqual,
                   frNotEqual,
                   frBigger,
                   frBiggerEqual,
                   frSmaller,
                   frSmallerEqual,
                   frJokerRight,
                   frJokerLeft,
                   frJokerSub,
                   frSpecEqual,
                   frEmpty);
  TLogicalType  = (flNone,
                   flEnd,
                   flAnd,
                   flAndNot,
                   flOr,
                   flOrNot);
  TFilterMode = (fmSQL, fmADO);

  // Filter Collection
  TDSubFilter = class(TCollectionItem)
  private
    FFieldName  : String;
    FFilterType : TFilterType;
    FNegation   : Boolean;
    FRelation   : TRelationType;
    FLowerValue : String;
    FUpperValue : String;
    FLogicalOp  : TLogicalType;

    // Handle properties
    procedure SetLowerValue(const Value: String);
    procedure SetUpperValue(const Value: String);

  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;

 published

    property FieldName  : String        read FFieldName     write FFieldName;
    property FilterType : TFilterType   read FFilterType    write FFilterType default ffNone;
    property Negation   : Boolean       read FNegation      write FNegation   default False;
    property Relation   : TRelationType read FRelation      write FRelation   default frNone;
    property LowerValue : String        read FLowerValue    write SetLowerValue;
    property UpperValue : String        read FUpperValue    write SetUpperValue;
    property LogicalOp  : TLogicalType  read FLogicalOp     write FLogicalOp  default flNone;

  end;

  // Filter Collection
  TDSubFilters = class(TCollection)
  protected
    FOwner: TPersistent;
    function  GetOwner: TPersistent; override;
    function  GetItem(Index: Integer): TDSubFilter;
    procedure SetItem(Index: Integer; Value: TDSubFilter);

  public
    constructor Create(AOwner: TPersistent);
    procedure Assign(Source: TPersistent); override;
    function  Add: TDSubFilter; overload;
    function  Add(Item: TDSubFilter): TDSubFilter; overload;
    procedure Delete; overload;
    procedure Delete(Item: TDSubFilter); overload;
    procedure Delete(Index: Integer); overload;

    property Items[Index: Integer]: TDSubFilter read GetItem     write SetItem; default;

  end;

  // Filter Collection
  TDFilter = class(TCollectionItem)
  private
    FName       : String;
    FLevels     : TDSubFilters;
    FCaption    : String;
    FBackGround : TColor;
    FForeGround : TColor;
    FSelected   : String;  // Selected fieldList for color filters

    // Handle properties
    function  GetSelected: String;
    procedure SetSelected(Value: String);
    function  FltToSQL(SubFilter: TDSubFilter; FilterMode: TFilterMode; Insensitive: Boolean = True): String;
    procedure SetCaption(const Value: String);

  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDisplayName: String; override;
    procedure Clear;
    function  SQLText(FilterMode: TFilterMode; InSensitive: Boolean = True): String;
    function  CountOR: Integer;
    function  CountAND: Integer;

 published

    property Name       : String       read FName          write FName;
    property Caption    : String       read FCaption       write SetCaption;
    property BackGround : TColor       read FBackGround    write FBackGround;
    property ForeGround : TColor       read FForeGround    write FForeGround;
    property Levels     : TDSubFilters read FLevels        write FLevels;
    property Selected   : String       read GetSelected    write SetSelected;

  end;

  // Filter Collection
  TDFilters = class(TCollection)
  protected
    FOwner: TComponent;
    function  GetOwner: TPersistent; override;
    function  GetItem(Index: Integer): TDFilter;
    procedure SetItem(Index: Integer; Value: TDFilter);
    function  GetCaptions(Index: Integer): String;
    procedure SetCaptions(Index: Integer; Value: String);
    function  GetLastItem: TDFilter;
    procedure SetLastItem(Value: TDFilter);

    function  EvalOne(SubFilter: TDSubFilter; DataSet: TDataSet): Boolean;

  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
    function  Add: TDFilter; overload;
    function  Add(SubFilter: TDSubFilter): TDFilter; overload;
    function  Add(SubFilters: TDSubFilters): TDFilter; overload;
    function  Add(Item: TDFilter): TDFilter; overload;
    procedure Delete; overload;
    procedure Delete(Item: TDFilter); overload;
    procedure Delete(Index: Integer); overload;
    procedure Delete(const ItemName: String); overload;
    procedure DeleteLevel(const ItemName: String; Level: Integer);
    procedure ClearLevel(Index: Integer); overload;
    procedure ClearLevel(const ItemName: String); overload;
    function  FindItem(const ItemName: String): TDFilter;

    procedure SetMenuItems(List: TStrings);
    function  Evaluate(Index: Integer; DataSet: TDataSet): Boolean;
    function  SQLText(Index: Integer; FilterMode: TFilterMode; InSensitive: Boolean = True): String; overload;
    function  SQLText(FilterMode: TFilterMode; InSensitive: Boolean = True): String; overload;

    property LastItem: TDFilter read GetLastItem write SetLastItem;
    property Captions[Index: Integer]: String read GetCaptions write SetCaptions;
    property Items[Index: Integer]: TDFilter read GetItem  write SetItem; default;

  end;

// Filter type from Field
function FieldToFilterType(Field: TField): TFilterType;
// Conversion in filter
function StringToRelation(Relation: String): TRelationType;
function RelationToString(Relation: TRelationType): String;
function LogicalToString(LogicalOp: TLogicalType): String;
function StringToLogical(LogicalOp: String): TLogicalType;
// Negation functions
function RelationNegation(Relation: TRelationType): TRelationType;
function LogicalNegation(LogicalOp: TLogicalType): TLogicalType;
// StringList functions
procedure RelationStrings(List: TStrings; FilterType: TFilterType); overload;
procedure RelationStrings(List: TStrings; Field: TField); overload;
procedure LogicalOpStrings(List: TStrings);
// Analyze the type of relation
function AnalyseRelation(const From, Till: String): TRelationType;

implementation


{******************************************************************************}
{***                          ADO Filter implementation                     ***}
{******************************************************************************}


function FieldToFilterType(Field: TField): TFilterType;
begin
     Result := ffNone;
     if Assigned(Field) then
     begin
          case Field.DataType of
               ftString,
               ftFixedChar,
               ftWideString,
               ftMemo,
               ftFmtMemo  : Result := ffString;
               ftSmallint,
               ftInteger,
               ftWord,
               ftAutoInc,
               ftLargeInt,
               ftFloat,
               ftCurrency,
               ftBCD : Result := ffNumeric;
               ftDate,
               ftDateTime : Result := ffDate;
               ftBoolean  : Result := ffLogical;
          end;
     end;
end;


// Conversion between Type and String
function RelationToString(Relation: TRelationType): String;
begin
     case Relation of
          frNone         : Result := cnsSignNone;
          frEqual        : Result := cnsSignEqual;
          frNotEqual     : Result := cnsSignNotEqual;
          frBigger       : Result := cnsSignBigger;
          frBiggerEqual  : Result := cnsSignBiggerEqual;
          frSmaller      : Result := cnsSignSmaller;
          frSmallerEqual : Result := cnsSignSmallerEqual;
          frJokerRight   : Result := cnsSignJokerRight;
          frJokerLeft    : Result := cnsSignJokerLeft;
          frJokerSub     : Result := cnsSignJokerSub;
          frSpecEqual    : Result := cnsSignSpecEqual;
     end;
end;


function StringToRelation(Relation: String): TRelationType;
begin
     Result := frNone;
     if Relation = cnsSignEqual        then Result := frEqual;
     if Relation = cnsSignNotEqual     then Result := frNotEqual;
     if Relation = cnsSignBigger       then Result := frBigger;
     if Relation = cnsSignBiggerEqual  then Result := frBiggerEqual;
     if Relation = cnsSignSmaller      then Result := frSmaller;
     if Relation = cnsSignSmallerEqual then Result := frSmallerEqual;
     if Relation = cnsSignJokerRight   then Result := frJokerRight;
     if Relation = cnsSignJokerLeft    then Result := frJokerLeft;
     if Relation = cnsSignJokerSub     then Result := frJokerSub;
     if Relation = cnsSignSpecEqual    then Result := frSpecEqual;
end;


// Conversion between Type and String
function LogicalToString(LogicalOp: TLogicalType): String;
begin
     Result := cnsLogNone;
     case LogicalOp of
          flEnd    : Result := cnsLogEnd;
          flAnd    : Result := cnsLogAnd;
          flAndNot : Result := cnsLogAndNot;
          flOr     : Result := cnsLogOr;
          flOrNot  : Result := cnsLogOrNot;
     end;
end;


function StringToLogical(LogicalOp: String): TLogicalType;
begin
     Result := flNone;
     if LogicalOp = cnsLogEnd    then Result := flEnd;
     if LogicalOp = cnsLogAnd    then Result := flAnd;
     if LogicalOp = cnsLogAndNot then Result := flAndNot;
     if LogicalOp = cnsLogOr     then Result := flOr;
     if LogicalOp = cnsLogOrNot  then Result := flOrNot;
end;


// Negate function for Relation
function RelationNegation(Relation: TRelationType): TRelationType;
begin
     Result := frNone;
     case Relation of
          frEqual        : Result := frNotEqual;
          frNotEqual     : Result := frEqual;
          frBigger       : Result := frSmaller;
          frBiggerEqual  : Result := frSmallerEqual;
          frSmaller      : Result := frBigger;
          frSmallerEqual : Result := frBiggerEqual;
          frJokerRight   : Result := frJokerRight;
          frJokerLeft    : Result := frJokerLeft;
          frJokerSub     : Result := frJokerSub;
          frEmpty        : Result := frEmpty;
          frSpecEqual    : Result := frSpecEqual;
     end;
end;


// Negate function for LogicalOp
function LogicalNegation(LogicalOp: TLogicalType): TLogicalType;
begin
     Result := flNone;
     case LogicalOp of
          flEnd    : Result := flEnd;
          flAnd    : Result := flAndNot;
          flAndNot : Result := flAnd;
          flOr     : Result := flOrNot;
          flOrNot  : Result := flOr;
     end;
end;


procedure RelationStrings(List: TStrings; FilterType: TFilterType);
begin
     with List do
     begin
          Clear;
          Add(cnsSignEqual       );  // =
          Add(cnsSignNotEqual    );  // <>
          Add(cnsSignBigger      );  // >
          Add(cnsSignBiggerEqual );  // >=
          Add(cnsSignSmaller     );  // <
          Add(cnsSignSmallerEqual);  // <=
          Add(cnsSignSpecEqual   );  // ==
          if FilterType = ffString then
          begin
               Add(cnsSignJokerRight); // A?
               Add(cnsSignJokerLeft ); // ?A
               Add(cnsSignJokerSub  ); // ??
          end;
     end;
end;


procedure RelationStrings(List: TStrings; Field: TField);
var
   xFilter: TFilterType;
begin
     xFilter := FieldToFilterType(Field);
     RelationStrings(List, xFilter);
end;


procedure LogicalOpStrings(List: TStrings);
begin
     with List do
     begin
          Clear;
          Add(cnsLogEnd   );  // END
          Add(cnsLogAnd   );  // AND
          Add(cnsLogOr    );  // OR
          Add(cnsLogAndNot);  // AND NOT
          Add(cnsLogOrNot );  // OR NOT
     end;
end;


// Analyze the type of relation
function AnalyseRelation(const From, Till: String): TRelationType;
var
   iTmp : Byte;
begin
     Result := frEqual;
     if Till = '' then
     begin
          iTmp := StrCount(From, '%');
          case iTmp of
               0 : Result := frEqual;
               1 :
               begin
                    if From = '%' then Result := frEmpty else
                    begin
                         if LeftStr(From, 1)  = '%' then Result := frJokerLeft else
                         begin
                              if RightStr(From, 1) = '%' then Result := frJokerRight;
                         end;
                    end;
               end;
          else
              Result := frJokerSub;
          end;
          if StrCount(From, '_') > 0 then Result := frJokerSub;
     end;
end;


{******************************************************************************}


constructor TDSubFilter.Create(Collection: TCollection);
begin
     inherited Create(Collection);
     Clear;
end;


procedure TDSubFilter.Assign(Source: TPersistent);
begin
     if Source is TDSubFilter then
     begin
          FieldName  := TDSubFilter(Source).FieldName;
          FilterType := TDSubFilter(Source).FilterType;
          LowerValue := TDSubFilter(Source).LowerValue;
          UpperValue := TDSubFilter(Source).UpperValue;
          Relation   := TDSubFilter(Source).Relation;
          Negation   := TDSubFilter(Source).Negation;
          LogicalOp  := TDSubFilter(Source).LogicalOp;
     end
     else inherited Assign(Source);
end;


procedure TDSubFilter.Clear;
begin
     FFieldName  := '';
     FFilterType := ffNone;
     FLowerValue := '';
     FUpperValue := '';
     FRelation   := frNone;
     FNegation   := False;
     FLogicalOp  := flNone;
end;


procedure TDSubFilter.SetLowerValue(const Value: String);
begin
     if FLowerValue <> Value then FLowerValue := StrTran(StrTran(Value, '%'), '_');
end;


procedure TDSubFilter.SetUpperValue(const Value: String);
begin
     if FUpperValue <> Value then FUpperValue := StrTran(StrTran(Value, '%'), '_');
end;


{******************************************************************************}


constructor TDSubFilters.Create(AOwner: TPersistent);
begin
     inherited Create(TDSubFilter);
     FOwner := AOwner;
end;


procedure TDSubFilters.Assign(Source: TPersistent);
var
   i     : Integer;
   oItem : TDSubFilter;
begin
     if Source is TDSubFilters then
     begin
          Clear;
          for i := 0 to TDSubFilters(Source).Count-1 do
          begin
               oItem := Add;
               oItem.Assign(TDSubFilters(Source).Items[i]);
          end;
     end
     else inherited Assign(Source);
end;


function TDSubFilters.Add: TDSubFilter;
begin
     Result := TDSubFilter(inherited Add);
end;


function TDSubFilters.Add(Item: TDSubFilter): TDSubFilter;
begin
     Result := Add;
     Result.Assign(Item);
end;


procedure TDSubFilters.Delete(Item: TDSubFilter);
begin
     if Assigned(Item) then Item.Destroy;
     if (Count > 0) and (Items[Count-1].FieldName <> '') then Items[Count-1].LogicalOp := flEnd;
end;


procedure TDSubFilters.Delete;
begin
     Delete(Count-1);
end;


procedure TDSubFilters.Delete(Index: Integer);
begin
     if (Index >= 0) and (Index < Count) then Delete(Items[Index]);
end;


function TDSubFilters.GetOwner:TPersistent;
begin
     Result := FOwner;
end;


function TDSubFilters.GetItem(Index: Integer): TDSubFilter;
begin
     Result := TDSubFilter(inherited GetItem(Index));
end;


procedure TDSubFilters.SetItem(Index: Integer; Value: TDSubFilter);
begin
     inherited SetItem(Index, Value);
end;


{******************************************************************************}


constructor TDFilter.Create(Collection: TCollection);
begin
     inherited Create(Collection);
     FLevels := TDSubFilters.Create(Self);
     Clear;
end;


destructor TDFilter.Destroy;
begin
     FLevels.Free;
     inherited Destroy;
end;


procedure TDFilter.Assign(Source: TPersistent);
begin
     if Source is TDFilter then
     begin
          BackGround := TDFilter(Source).BackGround;
          ForeGround := TDFilter(Source).ForeGround;
          Selected   := TDFilter(Source).Selected;
          Caption    := TDFilter(Source).Caption;
          Levels.Assign(TDFilter(Source).Levels);
     end
     else inherited Assign(Source);
end;


procedure TDFilter.Clear;
begin
     FBackGround := 0;
     FForeGround := 0;
     FSelected   := cnsAllItem;
     FCaption    := '';
     FLevels.Clear;
end;


procedure TDFilter.SetCaption(const Value: String);
begin
     if FCaption <> Value then
     begin
          FCaption := Value;
          if FName = '' then FName := Value;
     end;
end;


// Handle Selected property
function TDFilter.GetSelected: String;
begin
     if FSelected = '' then FSelected := cnsAllItem;
     Result := FSelected;
end;


procedure TDFilter.SetSelected(Value: String);
begin
     if FSelected <> Value then FSelected := Value;
     if FSelected = '' then FSelected := cnsAllItem;
end;


function TDFilter.CountOR: Integer;
var
   i : Integer;
begin
     Result := 0;
     for i := 0 to FLevels.Count-1 do
     begin
          if FLevels.Items[i].LogicalOp in [flOr,flOrNot] then Inc(Result);
          if FLevels.Items[i].LogicalOp in [flEnd,flNone] then Break;
     end;
end;


function TDFilter.CountAND: Integer;
var
   i : Integer;
begin
     Result := 0;
     for i := 0 to FLevels.Count-1 do
     begin
          if FLevels.Items[i].LogicalOp in [flAnd,flAndNot] then Inc(Result);
          if FLevels.Items[i].LogicalOp in [flEnd,flNone]   then Break;
     end;
end;


function TDFilter.GetDisplayName: String;
begin
     Result := FName;
     if Result = '' then Result := inherited GetDisplayName;
end;


function TDFilter.SQLText(FilterMode: TFilterMode; InSensitive: Boolean = True): String;
var
   i: Integer;
begin
     Result := '';
     for i := 0 to FLevels.Count-1 do
     begin
          if FLevels.Items[i].LogicalOp <> flNone then Result := Result + FltToSQL(FLevels[i], FilterMode, InSensitive);
          if FLevels.Items[i].LogicalOp in [flEnd,flNone] then Break;
     end;
end;


function TDFilter.FltToSQL(SubFilter: TDSubFilter; FilterMode: TFilterMode; Insensitive: Boolean): String;
var
   cLower : Currency;
   dLower : TDateTime;
   cUpper : Currency;
   dUpper : TDateTime;
begin
     Result := '';
     with SubFilter do
     begin
          // Selection by FilterType
          case FilterType of
               ffString :
               begin
                    if UpperValue = '' then // Empty the second bound = LIKE searching
                    begin
                         // The ADO can't accept the LIKE '%what' form!!! -> change to LIKE '%what%' form
                         if (FilterMode = fmADO) and (Relation = frJokerLeft) then Relation := frJokerSub; // Less worse

                         // Selection By Relation type
                         case Relation of
                              frEmpty      :
                                   Result := ' (' + FieldName + IIF(Negation, ' NOT', '') + ' = '''') ';
                              frJokerSub   :
                                   Result := ' (' + IIF(Insensitive, '{fn UCASE(' + FieldName + ')}', FieldName) +
                                                    IIF(Negation, ' NOT', '') + ' LIKE ''%' +
                                                    IIF(Insensitive, AnsiUpperCase(LowerValue), LowerValue) + '%'') ' +
                                                    IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                              frJokerRight :
                                   Result := ' (' + IIF(Insensitive, '{fn UCASE(' + FieldName + ')}', FieldName) +
                                                    IIF(Negation, ' NOT', '') + ' LIKE ''' +
                                                    IIF(Insensitive, AnsiUpperCase(LowerValue), LowerValue) + '%'') ' +
                                                    IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));

                              frJokerLeft  :
                                   Result := ' (' + IIF(Insensitive, '{fn UCASE(' + FieldName + ')}', FieldName) +
                                                    IIF(Negation, ' NOT', '') + ' LIKE ''%' +
                                                    IIF(Insensitive, AnsiUpperCase(LowerValue), LowerValue) + ''') ' +
                                                    IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                              frSpecEqual  :
                                   Result := ' (' + IIF(Insensitive, '{fn UCASE(' + FieldName + ')}', FieldName) +
                                                    IIF(Negation, ' NOT', '') + ' = ' + LowerValue + ') ' +
                                                    IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                         else
                              Result := ' (' + IIF(Insensitive, '{fn UCASE(' + FieldName + ')}', FieldName) + ' ' +
                                               IIF(Negation, RelationToString(RelationNegation(Relation)), RelationToString(Relation)) + ' ''' +
                                               IIF(Insensitive, AnsiUpperCase(LowerValue), LowerValue) + ''') ' +
                                               IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                         end;
                    end
                    else
                    begin
                         case FilterMode of
                              fmADO :
                              begin
                                   Result := ' (' + IIF(Insensitive, '{fn UCASE(' + FieldName + ')}', FieldName) +
                                                    IIF(Negation, ' < ', ' >= ''') +
                                                    IIF(Insensitive, AnsiUpperCase(LowerValue), LowerValue) + ''' AND ' +
                                                    IIF(Insensitive, '{fn UCASE(' + FieldName + ')}', FieldName) +
                                                    IIF(Negation, ' > ', ' <= ''') +
                                                    IIF(Insensitive, AnsiUpperCase(UpperValue), UpperValue) + ''') ' +
                                                    IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                              end;
                              fmSQL :
                              begin
                                   Result := ' (' + IIF(Insensitive, '{fn UCASE(' + FieldName + ')}', FieldName) +
                                                    IIF(Negation, ' NOT', '') + ' BETWEEN ' +
                                                    IIF(Insensitive, AnsiUpperCase(LowerValue), LowerValue) + ''' AND ' +
                                                    IIF(Insensitive, AnsiUpperCase(UpperValue), UpperValue) + ''') ' +
                                                    IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                              end;
                         end;
                    end;
               end;
               ffNumeric :
               begin
                    if UpperValue = '' then
                    begin
                         if Relation = frSpecEqual then
                         begin
                              Result := ' (' + FieldName + ' = ' + LowerValue + ') ' +
                                        IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                         end
                         else
                         begin
                              Result := ' (' + FieldName + ' ' +
                                        IIF(Negation, RelationToString(RelationNegation(Relation)), RelationToString(Relation)) + ' ' +
                                        LowerValue + ') ' +
                                        IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                         end;
                    end
                    else
                    begin
                         cLower := StrToCurr(LowerValue);
                         cUpper := StrToCurr(UpperValue);
                         if cLower = cUpper then       // Controll the input data
                         begin
                              Result := ' (' + FieldName + ' ' +
                                               IIF(Negation, RelationToString(RelationNegation(Relation)), RelationToString(Relation)) + ' ' +
                                               LowerValue + ') ' +
                                               IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                         end
                         else
                         begin
                              if (cLower > cUpper) then   // Must change, because the order is wrong
                              begin
                                   case FilterMode of
                                        fmADO :
                                        begin
                                             Result := ' (' + FieldName +
                                                              IIF(Negation, ' < ', ' >= ') +
                                                              UpperValue + ' AND ' +
                                                              FieldName +
                                                              IIF(Negation, ' > ', ' <= ') +
                                                              LowerValue + ') ' +
                                                              IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                                        end;
                                        fmSQL :
                                        begin
                                             Result := ' (' + FieldName +
                                                              IIF(Negation, ' NOT', '') + ' BETWEEN ' +
                                                              UpperValue + ' AND ' +
                                                              LowerValue + ') ' +
                                                              IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                                        end;
                                   end;
                              end
                              else
                              begin
                                   case FilterMode of
                                        fmADO :
                                        begin
                                             Result := ' (' + FieldName +
                                                              IIF(Negation, ' < ', ' >= ') +
                                                              LowerValue + ' AND ' +
                                                              FieldName +
                                                              IIF(Negation, ' > ', ' <= ') + 
                                                              UpperValue + ') ' +
                                                              IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                                        end;
                                        fmSQL :
                                        begin
                                             Result := ' (' + FieldName +
                                                              IIF(Negation, ' NOT', '') + ' BETWEEN ' +
                                                              LowerValue + ' AND ' +
                                                              UpperValue + ') ' +
                                                              IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                                        end;
                                   end;
                              end;
                         end;
                    end;
               end;
               ffDate:
               begin
                    if UpperValue = '' then
                    begin
                         Result := ' (' + FieldName + ' ' +
                                          IIF(Negation, RelationToString(RelationNegation(Relation)), RelationToString(Relation)) + ' ''' +
                                          LowerValue + ''') ' +
                                          IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                    end
                    else
                    begin
                         dLower := StdToDate(LowerValue);
                         dUpper := StdToDate(UpperValue);
                         if dLower = dUpper then       // Controll the input data
                         begin
                              Result := ' (' + FieldName + ' ' +
                                               IIF(Negation, RelationToString(RelationNegation(Relation)), RelationToString(Relation)) + ' #' +
                                               LowerValue + '#) ' +
                                               IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                         end
                         else
                         begin
                              if (dLower > dUpper) then   // Must change, because the order is wrong
                              begin
                                   case FilterMode of
                                        fmADO :
                                        begin
                                             Result := ' (' + FieldName +
                                                              IIF(Negation, ' < ', ' >= #') +
                                                              UpperValue + '# AND ' +
                                                              FieldName +
                                                              IIF(Negation, ' > ', ' <= #') +
                                                              LowerValue + '#) ' +
                                                              IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                                        end;
                                        fmSQL :
                                        begin
                                             Result := ' (' + FieldName +
                                                              IIF(Negation, ' NOT', '') + ' BETWEEN #' +
                                                              UpperValue + '# AND #' +
                                                              LowerValue + '#) ' +
                                                              IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                                        end;
                                   end;
                              end
                              else
                              begin
                                   case FilterMode of
                                        fmADO :
                                        begin
                                             Result := ' (' + FieldName +
                                                              IIF(Negation, ' < ', ' >= #') +
                                                              LowerValue + '# AND ' +
                                                              FieldName +
                                                              IIF(Negation, ' > ', ' <= #') +
                                                              UpperValue + '#) ' +
                                                              IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                                        end;
                                        fmSQL :
                                        begin
                                             Result := ' (' + FieldName +
                                                              IIF(Negation, ' NOT', '') + ' BETWEEN #' +
                                                              LowerValue + '# AND #' +
                                                              UpperValue + '#) ' +
                                                              IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
                                        end;
                                   end;
                              end;
                         end;
                    end;
               end;
               ffLogical:
               begin
                    Result := ' (' + FieldName + IIF(Negation, ' <> ', ' = ') + IIF(StrToLog(LowerValue), cnsWordBoolTrue, cnsWordBoolFalse) + ') ';
               end;
               ffCustom:
               begin
                    Result := ' (' + FieldName + ' ' +
                                     IIF(Negation, RelationToString(RelationNegation(Relation)), RelationToString(Relation)) + ' ' +
                                     LowerValue + ') ' +
                                     IIF(LogicalOp = flEnd, '', LogicalToString(LogicalOp));
               end;
          end;
     end;
end;


{******************************************************************************}


constructor TDFilters.Create(AOwner: TComponent);
begin
     inherited Create(TDFilter);
     FOwner := AOwner;
end;


procedure TDFilters.Assign(Source: TPersistent);
var
   i     : Integer;
   oItem : TDFilter;
begin
     if Source is TDFilters then
     begin
          Clear;
          for i := 0 to TDFilters(Source).Count-1 do
          begin
               oItem := Add;
               oItem.Assign(TDFilters(Source).Items[i]);
               oItem.Name := TDFilters(Source).Items[i].Name;
          end;
     end
     else inherited Assign(Source);
end;


function TDFilters.Add: TDFilter;
begin
     Result := TDFilter(inherited Add);
     Result.Name := 'Level' + IntToStr(Count);
end;


function TDFilters.Add(SubFilter: TDSubFilter): TDFilter;
var
   oSub : TDSubFilter;
begin
     Result := Add;
     oSub := Result.Levels.Add;
     oSub.Assign(SubFilter);
end;


function TDFilters.Add(SubFilters: TDSubFilters): TDFilter;
begin
     Result := Add;
     Result.Levels.Assign(SubFilters);
end;


function TDFilters.Add(Item: TDFilter): TDFilter;
begin
     Result := Add;
     Result.Assign(Item);
end;


procedure TDFilters.Delete(Item: TDFilter);
begin
     if Assigned(Item) then Item.Destroy;
end;


procedure TDFilters.Delete;
begin
     Delete(Count-1);
end;


procedure TDFilters.Delete(Index: Integer);
begin
     if (Index >= 0) and (Index < Count) then Delete(Items[Index]);
end;


procedure TDFilters.Delete(const ItemName: String);
begin
     Delete(FindItem(ItemName));
end;


procedure TDFilters.DeleteLevel(const ItemName: String; Level: Integer);
var
   oItem : TDFilter;
begin
     oItem := FindItem(ItemName);
     if Assigned(oItem) then oItem.Levels.Delete(Level);
end;


procedure TDFilters.ClearLevel(Index: Integer);
begin
     if (Index > -1) and (Index < Count) then Items[Index].Clear;
end;


procedure TDFilters.ClearLevel(const ItemName: String);
var
   oItem : TDFilter;
begin
     oItem := FindItem(ItemName);
     if Assigned(oItem) then oItem.Clear;
end;


function TDFilters.GetOwner:TPersistent;
begin
     Result := FOwner;
end;


function TDFilters.GetItem(Index: Integer): TDFilter;
begin
     Result := TDFilter(inherited GetItem(Index));
end;


procedure TDFilters.SetItem(Index: Integer; Value: TDFilter);
begin
     inherited SetItem(Index, Value);
end;


function TDFilters.FindItem(const ItemName: String): TDFilter;
var
   i : Integer;
begin
     Result := nil;
     if ItemName = '' then Exit;
     for i := 0 to Count-1 do
     begin
          if (AnsiUpperCase(Items[i].Name) = AnsiUpperCase(ItemName)) or
             (AnsiUpperCase(Items[i].Caption)  = AnsiUpperCase(ItemName)) then
          begin
               Result := Items[i];
               Break;
          end;
     end;
end;


function TDFilters.GetLastItem: TDFilter;
begin
     Result := nil;
     if Count > 0 then Result := Items[Count-1];
end;


procedure TDFilters.SetLastItem(Value: TDFilter);
begin
     if Count > 0 then Items[Count-1] := Value;
end;


function TDFilters.GetCaptions(Index: Integer): String;
begin
     if (Index > -1) and (Index < Count) then Result := Items[Index].Caption;
end;


procedure TDFilters.SetCaptions(Index: Integer; Value: String);
begin
     if (Index > -1) and (Index < Count) then Items[Index].Caption := Value;
end;


procedure TDFilters.SetMenuItems(List: TStrings);
var
   i: Integer;
begin
     List.Clear;
     for i := 0 to Count-1 do
     begin
          List.Add(Items[i].Caption);
     end;
end;


function TDFilters.Evaluate(Index: Integer; DataSet: TDataSet): Boolean;
var
   i        : Integer;
   iTmp     : Integer;
   xPrevLog : TLogicalType;
   iAndLevel: Integer;
   iOrLevel : Integer;
begin
     Result := False;
     if Assigned(DataSet) and (Index < Count) and (Index > -1) then
     begin
          iOrLevel  := Items[Index].CountOR;
          iAndLevel := Items[Index].CountAND;
          iTmp      := 0;
          if (iAndLevel = 0) and (iOrLevel = 0) then iTmp := 1;
          if (iAndLevel > 0) and (iOrLevel = 0) then iTmp := 2;
          if (iAndLevel = 0) and (iOrLevel > 0) then iTmp := 3;
          if (iAndLevel > 0) and (iOrLevel > 0) then iTmp := 4;

          case iTmp of
               1 : // Only one level
               begin
                    // Get the logical result in one level
                    Result := EvalOne(Items[Index].Levels[0], DataSet);
               end;
               2 : // Only AND levels
               begin
                    xPrevLog := flEnd;
                    for i := 1 to 9 do
                    begin
                         // Get the logical result in one level
                         if Items[Index].Levels[i].LogicalOp <> flNone then
                            Result := EvalOne(Items[Index].Levels[i], DataSet);

                         // If the previous level was "and not" must reverse the value
                         if xPrevLog = flAndNot then Result := not Result;

                         // If that's the last level or not true the value go
                         if not Result or (Items[Index].Levels[i].LogicalOp in [flEnd,flNone]) then Break;
                         xPrevLog := Items[Index].Levels[i].LogicalOp;
                    end;
               end;
               3 : // Only OR levels
               begin
                    xPrevLog := flEnd;
                    for i := 1 to 9 do
                    begin
                         // Get the logical result in one level
                         if Items[Index].Levels[i].LogicalOp <> flNone then
                            Result := EvalOne(Items[Index].Levels[i], DataSet);

                         // If the previous level was "or not" must reverse the value
                         if xPrevLog = flOrNot then Result := not Result;

                         // If that's the last level or not true the value go
                         if Result or (Items[Index].Levels[i].LogicalOp in [flEnd,flNone]) then Break;
                         xPrevLog := Items[Index].Levels[i].LogicalOp;
                    end;
               end;
               4 : // All possible
               begin
                    xPrevLog := flEnd;
                    i := 0; // start from the first level
                    repeat
                         // Get the logical result in one level
                         if Items[Index].Levels[i].LogicalOp <> flNone then
                            Result := EvalOne(Items[Index].Levels[i], DataSet);

                         // If the previous level was "AND NOT" or "OR NOT" must to reverse the value
                         if (xPrevLog = flAndNot) or (xPrevLog = flOrNot) then Result := not Result;

                         // Must to to continue? If the first part TRUE not!
                         if Result and (Items[Index].Levels[i].LogicalOp in [flOr,flOrNot,flEnd,flNone]) then Break;

                         // Save the value for the next level
                         xPrevLog := Items[Index].Levels[i].LogicalOp;

                         // Step to next level
                         Inc(i);
                    until (Items[Index].Levels[i].LogicalOp in [flEnd,flNone]); // Continue?
               end;
          end;
     end;
end;


function TDFilters.EvalOne(SubFilter: TDSubFilter; DataSet: TDataSet): Boolean;
var
   oField : TField;
   sValue : String;
   iValue : Integer;
   cValue : Currency;
   dValue : TDateTime;
   bValue : Boolean;
   sInput : String;
   iInput : Integer;
   cInput : Currency;
   dInput : TDateTime;
   bInput : Boolean;
begin
     Result := False;
     with SubFilter do
     begin
          if IsEmptyStr(FieldName) then Exit;
          oField := DataSet.FieldByName(FieldName);
          case oField.DataType of
               ftString,
               ftFixedChar,
               ftWideString,
               ftMemo,
               ftFmtMemo  : // Strings
               begin
                    sInput := AnsiUpperCase(oField.AsString);
                    sValue := AnsiUpperCase(LowerValue);

                    case Relation of
                         frEqual        : Result := (sInput =  sValue);
                         frNotEqual     : Result := (sInput <> sValue);
                         frBigger       : Result := (sInput >  sValue);
                         frBiggerEqual  : Result := (sInput >= sValue);
                         frSmaller      : Result := (sInput <  sValue);
                         frSmallerEqual : Result := (sInput <= sValue);
                         frJokerRight   : Result := (LeftStr(sInput, Length(sValue)) = sValue);
                         frJokerLeft    : Result := (RightStr(sInput, Length(sValue)) = sValue);
                         frJokerSub     : Result := (Pos(sValue, sInput) > 0);
                         frSpecEqual    : Result := (sInput = DataSet.FieldByName(LowerValue).AsString);
                    end;
               end;
               ftSmallint,
               ftInteger,
               ftWord,
               ftAutoInc,
               ftLargeInt : // Integer numbers
               begin
                    iInput := oField.AsInteger;
                    iValue := 0;
                    if Relation <> frSpecEqual then iValue := StrToInt(LowerValue);
                    case Relation of
                         frEqual        : Result := (iInput =  iValue);
                         frNotEqual     : Result := (iInput <> iValue);
                         frBigger       : Result := (iInput >  iValue);
                         frBiggerEqual  : Result := (iInput >= iValue);
                         frSmaller      : Result := (iInput <  iValue);
                         frSmallerEqual : Result := (iInput <= iValue);
                         frSpecEqual    : Result := (iInput = DataSet.FieldByName(LowerValue).AsInteger);
                    end;
               end;
               ftFloat,
               ftCurrency,
	       ftBCD : // Float Numbers
               begin
                    cInput := oField.AsCurrency;
                    cValue := 0;
                    if Relation <> frSpecEqual then cValue := StrToCurr(LowerValue);
                    case Relation of
                         frEqual        : Result := (cInput =  cValue);
                         frNotEqual     : Result := (cInput <> cValue);
                         frBigger       : Result := (cInput >  cValue);
                         frBiggerEqual  : Result := (cInput >= cValue);
                         frSmaller      : Result := (cInput <  cValue);
                         frSmallerEqual : Result := (cInput <= cValue);
                         frSpecEqual    : Result := (cInput = DataSet.FieldByName(LowerValue).AsCurrency);
                    end;
               end;
               ftDate,
               ftDateTime : // Inpute
               begin
                    dInput := oField.AsDateTime;
                    dValue := Now;
                    if Relation <> frSpecEqual then dValue := StrToDate(LowerValue);
                    case Relation of
                         frEqual        : Result := (dInput =  dValue);
                         frNotEqual     : Result := (dInput <> dValue);
                         frBigger       : Result := (dInput >  dValue);
                         frBiggerEqual  : Result := (dInput >= dValue);
                         frSmaller      : Result := (dInput <  dValue);
                         frSmallerEqual : Result := (dInput <= dValue);
                         frSpecEqual    : Result := (dInput = DataSet.FieldByName(LowerValue).AsDateTime);
                    end;
               end;
               ftBoolean  : // Logical
               begin
                    bInput := oField.AsBoolean;
                    bValue := False;
                    if Relation <> frSpecEqual then bValue := StrToLog(LowerValue);
                    case Relation of
                         frEqual    : Result := (bInput =  bValue);
                         frNotEqual : Result := (bInput <> bValue);
                         frSpecEqual: Result := (bInput = DataSet.FieldByName(LowerValue).AsBoolean);
                    end;
               end;
          end;
     end;
end;


function TDFilters.SQLText(Index: Integer; FilterMode: TFilterMode; InSensitive: Boolean = True): String;
begin
     Result := '';
     if (Index < Count) and (Index > -1) then
     begin
          Result := Items[Index].SQLText(FilterMode, InSensitive);
     end;
end;


function TDFilters.SQLText(FilterMode: TFilterMode; InSensitive: Boolean = True): String;
var
   i : Integer;
begin
     Result := '';
     for i := 0 to Count-1 do
     begin
          Result := Result + SQLText(i, FilterMode, InSensitive) + ' AND ';
     end;
     if RightStr(Result, 5) = ' AND ' then Result := LeftStr(Result, Length(Result)-5);
end;


end.
 