{+--------------------------------------------------------------------------+
 | Unit:        mwPasTokenList
 | Created:     10.97
 | Author:      Martin Waldenburg
 | Copyright    1997, all rights reserved.
 | Description: TLongIntList is a dynamic array of LongInts.
 |              TmSearcher is a specialized version of the turbo search engine,
 |              which is based on an article in the German magazine c't (8/97).
 |              TPasTokenList scans a PChar for Pascal tokens and gives full access.
 | Version:     1.2
 | DISCLAIMER:  This is provided as is, expressly without a warranty of any kind.
 |              You use it at your own risc.
 +--------------------------------------------------------------------------+}
 // Fixes by Stefan (4/18/98): //StH!

unit mwPasTokenList;

{$R-}

interface

uses Windows, SysUtils, Classes;

type
  TTokenKind = (tkAbsolute, tkAbstract, tkAnd, tkAnsiComment, tkArray, tkAs,
    tkAsciiChar, tkAsm, tkAssembler, tkAssign, tkAutomated, tkBegin, tkBadString,
    tkBorComment, tkCase, tkCdecl, tkClass, tkColon, tkComma, tkCompDirect,
    tkConst, tkConstructor, tkCRLF, tkCRLFCo, tkDefault, tkDestructor, tkDispid,
    tkDispinterface, tkDiv, tkDo, tkDotDot, tkDownto, tkDynamic, tkElse, tkEnd,
    tkEqual, tkError, tkExcept, tkExport, tkExports, tkExternal, tkFar, tkFile,
    tkFinalization, tkFinally, tkFloat, tkFor, tkForward, tkFunction, tkGoto,
    tkGreater, tkGreaterEqual, tkIdentifier, tkIf, tkImplementation, tkIn,
    tkIndex, tkInherited, tkInitialization, tkInline, tkInteger, tkInterface,
    tkIs, tkKeyString, tkLabel, tkLibrary, tkLower, tkLowerEqual, tkMessage,
    tkMinus, tkMod, tkName, tkNear, tkNil, tkNodefault, tkNone, tkNot,
    tkNotEqual, tkNull, tkNumber, tkObject, tkOf, tkOr, tkOut, tkOverride,
    tkPacked, tkPascal, tkPlus, tkPoint, tkPrivate, tkProcedure, tkProgram,
    tkProperty, tkProtected, tkPublic, tkPublished, tkRaise, tkRead, tkReadonly,
    tkRecord, tkRegister, tkRepeat, tkResident, tkResourcestring, tkRoundClose,
    tkRoundOpen, tkSafecall, tkSemiColon, tkSet, tkShl, tkShr, tkSlash,
    tkSlashesComment, tkSquareClose, tkSquareOpen, tkSpace, tkStar, tkStdcall,
    tkStored, tkString, tkStringresource, tkSymbol, tkThen, tkThreadvar, tkTo,
    tkTry, tkType, tkUnit, tkUnknown, tkUntil, tkUses, tkVar, tkVirtual, tkWhile,
    tkWith, tkWrite, tkWriteonly, tkXor);

  TIdentDirect = Set of TTokenKind;

  TCommentState = (csAnsi, csBor, csNo, csSlashes);

  PLongIntArray = ^TLongIntArray;
  TLongIntArray = array[0..0] of LongInt;

  TLongIntList = class(TObject)
  private
    FCapacity: Integer;
    FCount: Integer;
    FLongIntList: PLongIntArray;
  protected
    function GetItems(Index: Integer): LongInt;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    procedure SetItems(Index: Integer; Item: LongInt);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Item: LongInt): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function First: LongInt;
    function IndexOf(Item: LongInt): Integer;
    procedure Insert(Index: Integer; Item: LongInt);
    function Last: LongInt;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: LongInt): Integer;
    procedure Sort;
    procedure DeleteGroup(StartIndex: LongInt; GroupCount: LongInt);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: LongInt read GetItems write SetItems; default;
    property LongIntList: PLongIntArray read FLongIntList;
  end; { TLongIntList }

  TPasTokenList = class;

  TmSearcher = class(Tobject)
  private
    FPasTokenList: TPasTokenList;
    FSearchOrigin: PChar;
    Pat: String;
    fPos: Integer;
    HalfLen: Integer;
    PatLenPlus: Integer;
    SearchLen: Integer;
    Shift: array[0..255] of Integer;
    CompTable: array[#0..#255] of byte;
    fFinished: Boolean;
    fFound: Boolean;
    fPosition: Integer;
    FFoundList: TLongIntList;
    function GetFinished: Boolean;
    function GetItems(Index: integer): Integer;
    function GetCount: Integer;
  protected
  public
    ClassList: TLongIntList;
    ImplementationsList: TLongIntList;
    InterfaceList: TLongIntList;
    MethodList: TLongIntList;
    PatLen: Integer;
    constructor Create(Value: TPasTokenList);
    destructor Destroy; override;
    function GetImplementationsIndex: LongInt;
    function Next: Integer;
    procedure Add(aPosition: Integer);
    procedure FillClassList;
    procedure Init(NewPattern: String);
    procedure Retrive(aToken: String);
    function GetMethodImplementation(aClassName, aMethodIdentifier: String): LongInt;
    procedure FillMethodList;
    procedure FillInterfaceList;
    function GetMethodImpLine(aClassName: String; aMethodIdentifier: String): LongInt;
    property Finished: Boolean read GetFinished;
    property Found: Boolean read fFound;
    property Position: Integer read fPosition write fPos;
    property Items[Index: Integer]: Integer read GetItems; default;
    property Count: Integer read GetCount;
  published
  end; { TmSearcher }

  TPasTokenList = class(TObject)
  private
    FTokenPositionsList: TLongIntList;
    fOrigin: PChar;
    fPCharSize: Longint;
    fPCharCapacity: Longint;
    FComment: TCommentState;
    FEndCount: Integer;
    Run: LongInt;
    Walker: LongInt;
    FRoundCount: Integer;
    FSquareCount: Integer;
    FVisibility: TTokenKind;
    FLines: TLongIntList;
    procedure WriteTo(InsPos, DelPos: LongInt; Item: string);
    function GetCount: Integer;
    procedure SetCount(value: Integer);
    function GetCapacity: Integer;
    procedure ResetPositionsFrom(Index, Value: LongInt);
    procedure ResetLines(Index, Value: LongInt);
    function GetIsJunk: Boolean;
    function IdentKind(Index: LongInt): TTokenKind;
    procedure SetRunIndex(NewPos: LongInt);
    procedure HandleComments;
    function GetTokenID(Index: LongInt): TTokenKind;
    function GetTokenPosition(Index: integer): Longint;
    function GetRunID: TTokenKind;
    function GetRunPosition: LongInt;
    function GetRunToken: string;
    function GetTokenLine(anIndex: Integer): LongInt;
    function GetRunLine: LongInt;
  protected
    function GetToken(Index: Integer): string;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetToken(Index: Integer; Item: string);
  public
    Searcher: TmSearcher;
    constructor Create;
    destructor Destroy; override;
    procedure SetOrigin(NewOrigin: PChar; NewSize: LongInt);
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function First: string;
    function IndexOf(Item: string): Integer;
    procedure Insert(Index: Integer; Item: string);
    function Last: string;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: string): Integer;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Token[Index: Integer]: string read GetToken write SetToken; default;
    property TokenPositionsList: TLongIntList read FTokenPositionsList;
    property Origin: PChar read fOrigin;
    property PCharSize: Longint read fPCharSize;
    property PCharCapacity: Longint read fPCharCapacity;
    function GetSubString(StartPos, EndPos: LongInt): string;
    procedure ScanForLines;
    procedure Next;
    procedure Previous;
    procedure NextID(ID: TTokenKind);
    procedure NextNonComment;
    procedure NextNonJunk;
    procedure NextNonSpace;
    procedure Tokenize;
    procedure ToLineStart;
    procedure PreviousID(ID: TTokenKind);
    procedure PreviousNonComment;
    procedure PreviousNonJunk;
    procedure PreviousNonSpace;
    function PositionAtLine(aPosition: LongInt): LongInt;
    function IndexAtLine(anIndex: LongInt): LongInt;
    function PositionToIndex(aPosition: LongInt): LongInt;
    procedure DeleteGroup(StartIndex: LongInt; GroupCount: LongInt);
    function InsertString(StartIndex: LongInt; ToInsert: String): LongInt;
    function MoveGroup(OldStartIndex: LongInt; NewStartIndex: LongInt; GroupCount: LongInt): Boolean;
    property Comments: TCommentState read FComment write FComment;
    property EndCount: Integer read FEndCount write FEndCount;
    property IsJunk: Boolean read GetIsJunk;
    property RunIndex: LongInt read Run write SetRunIndex;
    property RoundCount: Integer read FRoundCount write FRoundCount;
    property SquareCount: Integer read FSquareCount write FSquareCount;
    property Visibility: TTokenKind read FVisibility write FVisibility;
    property TokenID[Index: LongInt]: TTokenKind read GetTokenID;
    property TokenPosition[Index: LongInt]: LongInt read GetTokenPosition;
    property RunID: TTokenKind read GetRunID;
    property RunPosition: LongInt read GetRunPosition;
    property RunToken: string read GetRunToken;
    property Lines: TLongIntList read FLines;
    property TokenLine[Index: Integer]: LongInt read GetTokenLine;
    property RunLine: LongInt read GetRunLine;
  published
  end; { TPasTokenList }

Const
  IdentDirect: TIdentDirect = [tkAbsolute, tkAbstract, tkAssembler, tkCdecl,
    tkDefault, tkDispid, tkDynamic, tkExport, tkExternal, tkFar, tkForward,
    tkIdentifier, tkIndex, tkMessage, tkName, tkNear, tkNodefault, tkOverride,
    tkPascal, tkRead, tkReadonly, tkRegister, tkResident, tksafecall, tkstdcall,
    tkStored, tkVirtual, tkWrite, tkWriteonly];

  BigIdentDirect: TIdentDirect = [tkAbsolute, tkAbstract, tkAssembler,
    tkAutomated, tkCdecl, tkDefault, tkDispid, tkDynamic, tkExport, tkExternal,
    tkFar, tkForward, tkIdentifier, tkIndex, tkMessage, tkName, tkNear,
    tkNodefault, tkOverride, tkPascal, tkPrivate, tkProtected, tkPublic,
    tkPublished, tkRead, tkReadonly, tkRegister, tkResident, tksafecall,
    tkstdcall, tkStored, tkVirtual, tkWrite, tkWriteonly];

implementation



constructor TLongIntList.Create;
begin
  inherited Create;
end; { Create }

destructor TLongIntList.Destroy;
begin
  Clear;
  inherited Destroy;
end; { Destroy }

{ Based on a non-recursive QuickSort from the SWAG-Archive.
  ( TV Sorting Unit by Brad Williams ) }

procedure TLongIntList.Sort;
var
  Left, Right, SubArray, SubLeft, SubRight, Temp, Pivot: LongInt;
  Stack: array[1..32] of record First, Last: LongInt; end;
begin
  if Count > 1 then
  begin
    SubArray := 1;
    Stack[SubArray].First := 0;
    Stack[SubArray].Last := Count - 1;
    repeat
      Left := Stack[SubArray].First;
      Right := Stack[SubArray].Last;
      Dec(SubArray);
      repeat
        SubLeft := Left;
        SubRight := Right;
        Pivot := FLongIntList[(Left + Right) shr 1];
        repeat
          while FLongIntList[SubLeft] < Pivot do Inc(SubLeft);
          while FLongIntList[SubRight] > Pivot do Dec(SubRight);
          IF SubLeft <= SubRight then
          begin
            Temp := FLongIntList[SubLeft];
            FLongIntList[SubLeft] := FLongIntList[SubRight];
            FLongIntList[SubRight] := Temp;
            Inc(SubLeft);
            Dec(SubRight);
          end;
        until SubLeft > SubRight;
        IF SubLeft < Right then
        begin
          Inc(SubArray);
          Stack[SubArray].First := SubLeft;
          Stack[SubArray].Last := Right;
        end;
        Right := SubRight;
      until Left >= Right;
    until SubArray = 0;
  end;
end; { Sort }

function TLongIntList.GetItems(Index: Integer): LongInt;
begin
  Result := FLongIntList[Index];
end; { GetItems }

procedure TLongIntList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then FCount := NewCapacity;
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FLongIntList, NewCapacity * SizeOf(LongInt));
    FCapacity := NewCapacity;
  end;
end; { SetCapacity }

procedure TLongIntList.SetCount(NewCount: Integer);
begin
  if NewCount > FCapacity then SetCapacity(NewCount);
  FCount := NewCount;
end; { SetCount }

procedure TLongIntList.SetItems(Index: Integer; Item: LongInt);
begin
  FLongIntList[Index] := Item;
end; { SetItems }

function TLongIntList.Add(Item: LongInt): Integer;
begin
  Result := FCount;
  if Result + 1 >= FCapacity then SetCapacity(FCapacity + 1024);
  FLongIntList[Result] := Item;
  Inc(FCount);
end; { Add }

procedure TLongIntList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end; { Clear }

procedure TLongIntList.Delete(Index: Integer);
begin
  Dec(FCount);
  if Index < FCount then
    System.Move(FLongIntList[Index + 1], FLongIntList[Index],
      (FCount - Index) * SizeOf(LongInt));
end; { Delete }

procedure TLongIntList.DeleteGroup(StartIndex: LongInt; GroupCount: LongInt);
begin
  Dec(FCount, GroupCount);
  if StartIndex < FCount then
    System.Move(FLongIntList[StartIndex + GroupCount], FLongIntList[StartIndex],
      (FCount - StartIndex) * SizeOf(LongInt));
end;  { DeleteGroup }

procedure TLongIntList.Exchange(Index1, Index2: Integer);
var
  Item: LongInt;
begin
  Item := FLongIntList[Index1];
  FLongIntList[Index1] := FLongIntList[Index2];
  FLongIntList[Index2] := Item;
end; { Exchange }

function TLongIntList.First: LongInt;
begin
  Result := GetItems(0);
end; { First }

function TLongIntList.IndexOf(Item: LongInt): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FLongIntList[Result] <> Item) do Inc(Result);
  if Result = FCount then Result := -1;
end; { IndexOf }

procedure TLongIntList.Insert(Index: Integer; Item: LongInt);
begin
  if FCount = FCapacity then SetCapacity(FCapacity + 1024);
  if Index < FCount then
    System.Move(FLongIntList[Index], FLongIntList[Index + 1],
      (FCount - Index) * SizeOf(LongInt));
  FLongIntList[Index] := Item;
  Inc(FCount);
end; { Insert }

function TLongIntList.Last: LongInt;
begin
  Result := GetItems(FCount - 1);
end; { Last }

procedure TLongIntList.Move(CurIndex, NewIndex: Integer);
var
  Item: LongInt;
begin
  if CurIndex <> NewIndex then
  begin
    Item := GetItems(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, Item);
  end;
end; { Move }

function TLongIntList.Remove(Item: LongInt): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then Delete(Result);
end; { Remove }

constructor TmSearcher.Create(Value: TPasTokenList);
begin
  inherited Create;
  FPasTokenList := Value;
  Pat := '';
  PatLen := 0;
  HalfLen := 0;
  SearchLen := 0;
  fPos := -1;
  fFound := False;
  FFoundList := TLongIntList.Create;
  ClassList := TLongIntList.Create;
  ImplementationsList := TLongIntList.Create;
  InterfaceList := TLongIntList.Create;
  MethodList := TLongIntList.Create;
end; { Create }

destructor TmSearcher.Destroy;
begin
  FFoundList.Free;
  ClassList.Free;
  ImplementationsList.Free;
  InterfaceList.Free;
  MethodList.Free;
  inherited Destroy;
end; { Destroy }

function TmSearcher.GetFinished: Boolean;
begin
  fFinished := False;
  if fPos >= SearchLen - 1 then fFinished := True;
  if PatLen > SearchLen then fFinished := True;
  Result := fFinished;
end; { GetFinished }

procedure TmSearcher.Init(NewPattern: String);
var
  I: Byte;
begin
  FFoundList.Clear;
  SearchLen := FPasTokenList.PCharSize;
  FSearchOrigin := FPasTokenList.Origin;
  Pat := NewPattern;
  PatLen := Length(Pat);
  PatLenPlus := PatLen + 1;
  HalfLen := PatLen div 2;
  for I := 0 to 255 do
    CompTable[Char(I)] := ord(AnsiLowerCase(Char(I))[1]);
  for I := 0 to 255 do Shift[I] := PatLenPlus;
  for I := 1 to PatLen do Shift[CompTable[Pat[i]]] := PatLenPlus - I;
  fPos := -1;
end; { Init }

function TmSearcher.Next: Integer;
var
  I, J: Integer;
begin
  Result := -1;
  fFound := False;
  inc(fPos, PatLen);
  fPosition := -1;
  while fPos <= SearchLen do
  begin
    I := PatLen;
    if (CompTable[Pat[I]] <> CompTable[FSearchOrigin[fPos]]) then
      inc(fPos, Shift[CompTable[FSearchOrigin[fPos + 1]]])
    else
    begin
      J := fPos;
      repeat
        dec(I); dec(J);
      until (I = 0) or (CompTable[Pat[I]] <> CompTable[FSearchOrigin[J]]);
      if I = 0 then
      begin
        fFound := True;
        fPosition := fPos - Patlen + 1;
        Result := fPosition;
        break;
      end else if I < HalfLen then inc(fPos, PatLenPlus)
      else inc(fPos, Shift[CompTable[FSearchOrigin[J + 1]]]);
    end;
  end;
end; { Next }

function TmSearcher.GetItems(Index: integer): Integer;
begin
  if (Index >= FFoundList.Count) or (Index < 0) then Result := -1 else
    Result := FFoundList[Index];
end; { GetItems }

function TmSearcher.GetCount: Integer;
begin
  Result := FFoundList.Count;
end; { GetCount }

procedure TmSearcher.Add(aPosition: Integer);
begin
  FFoundList.Add(aPosition);
end; { Add }

procedure TmSearcher.FillClassList;
var
  RPos: LongInt;
  RIndex: LongInt;
begin
  ClassList.Clear;
  Init('CLASS');
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FPasTokenList.PositionToIndex(RPos);
      if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
      begin
        FPasTokenList.RunIndex := RIndex;
        FPasTokenList.PreviousNonJunk;
        if FPasTokenList.RunId = tkEqual then
        begin
          if 'CLASS' = UpperCase(FPasTokenList[RIndex]) then
          begin
            RIndex := FPasTokenList.RunIndex - 1;
            while not (FPasTokenList.TokenID[RIndex] in BigIdentDirect) do dec(RIndex);
            ClassList.Add(RIndex);
          end;
        end;
      end;
    end;
  end;
end; { FillClassList }

procedure TmSearcher.FillInterfaceList;
var
  RPos: LongInt;
  RIndex: LongInt;
begin
  InterfaceList.Clear;
  Init('INTERFACE');
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FPasTokenList.PositionToIndex(RPos);
      if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
      begin
        FPasTokenList.RunIndex := RIndex;
        FPasTokenList.PreviousNonJunk;
        if FPasTokenList.RunId = tkEqual then
        begin
          if 'INTERFACE' = UpperCase(FPasTokenList[RIndex]) then
          begin
            RIndex := FPasTokenList.RunIndex - 1;
            while not (FPasTokenList.TokenID[RIndex] in BigIdentDirect) do dec(RIndex);
            InterfaceList.Add(RIndex);
          end;
        end;
      end;
    end;
  end;
  Init('DISPINTERFACE');
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FPasTokenList.PositionToIndex(RPos);
      if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
      begin
        FPasTokenList.RunIndex := RIndex;
        FPasTokenList.PreviousNonJunk;
        if FPasTokenList.RunId = tkEqual then
        begin
          if 'DISPINTERFACE' = UpperCase(FPasTokenList[RIndex]) then
          begin
            RIndex := FPasTokenList.RunIndex - 1;
            while not (FPasTokenList.TokenID[RIndex] in BigIdentDirect) do dec(RIndex);
            InterfaceList.Add(RIndex);
          end;
        end;
      end;
    end;
  end;
  InterfaceList.Sort;
end; { FillInterfaceList }

procedure TmSearcher.FillMethodList;
var
  RPos: LongInt;
  RIndex: LongInt;
begin
  MethodList.Clear;
  Init('CONSTRUCTOR');
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FPasTokenList.PositionToIndex(RPos);
      if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
        if 'CONSTRUCTOR' = UpperCase(FPasTokenList[RIndex]) then
        begin
          while FPasTokenList.TokenID[RIndex - 1] <> tkCRLF do dec(RIndex);
          MethodList.Add(RIndex);
        end;
    end;
  end;
  Init('DESTRUCTOR');
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FPasTokenList.PositionToIndex(RPos);
      if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
        if 'DESTRUCTOR' = UpperCase(FPasTokenList[RIndex]) then
        begin
          while FPasTokenList.TokenID[RIndex - 1] <> tkCRLF do dec(RIndex);
          MethodList.Add(RIndex);
        end;
    end;
  end;
  Init('FUNCTION');
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FPasTokenList.PositionToIndex(RPos);
      if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
        if 'FUNCTION' = UpperCase(FPasTokenList[RIndex]) then
        begin
          while FPasTokenList.TokenID[RIndex - 1] <> tkCRLF do dec(RIndex);
          MethodList.Add(RIndex);
        end;
    end;
  end;
  Init('PROCEDURE');
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FPasTokenList.PositionToIndex(RPos);
      if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
        if 'PROCEDURE' = UpperCase(FPasTokenList[RIndex]) then
        begin
          while FPasTokenList.TokenID[RIndex - 1] <> tkCRLF do dec(RIndex);
          MethodList.Add(RIndex);
        end;
    end;
  end;
  MethodList.Sort;
end; { FillMethodList }

procedure TmSearcher.Retrive(aToken: String);
var
  RPos: LongInt;
  RIndex: LongInt;
begin
  aToken := UpperCase(aToken);
  Init(aToken);
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FPasTokenList.PositionToIndex(RPos);
      if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
        if aToken = UpperCase(FPasTokenList[RIndex]) then Add(RIndex);
    end;
  end;
end; { Retrive }

function TmSearcher.GetImplementationsIndex: LongInt;
var
  RPos: LongInt;
  RIndex: LongInt;
begin
  Result := -1;
  Init('IMPLEMENTATION');
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FPasTokenList.PositionToIndex(RPos);
      if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
        if 'IMPLEMENTATION' = UpperCase(FPasTokenList[RIndex]) then
        begin
          Result := RIndex;
          break;
        end;
    end;
  end;
end; { GetImplementationsIndex }

function TmSearcher.GetMethodImplementation(aClassName, aMethodIdentifier: String): LongInt;
var
  RPos: LongInt;
  RIndex: LongInt;
  ToFind: String;
  Found: Boolean;
begin
  Result := -1;
  ImplementationsList.Clear;
  ToFind := aClassName + '.' + aMethodIdentifier;
  if ToFind <> '.' then
  begin
    Init(aClassName + '.' + aMethodIdentifier);
    aMethodIdentifier := UpperCase(aMethodIdentifier);
    while not Finished do
    begin
      RPos := Next;
      Found := False;
      if RPos <> -1 then
      begin
        RIndex := FPasTokenList.PositionToIndex(RPos);
        if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
          if aMethodIdentifier = UpperCase(FPasTokenList[RIndex + 2]) then
          begin
            while FPasTokenList.TokenId[RIndex - 1] <> tkCRLF do
            begin
              dec(RIndex);
              if FPasTokenList.TokenId[RIndex] in [tkConstructor, tkDestructor, tkFunction, tkProcedure] then
                Found := True;
            end;
            if Found then
            begin
              Result := RIndex;
              ImplementationsList.Add(RIndex);
              break;
            end;
          end;
      end;
    end;
    while not Finished do
    begin
      Found := False;
      RPos := Next;
      if RPos <> -1 then
      begin
        RIndex := FPasTokenList.PositionToIndex(RPos);
        if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
          if aMethodIdentifier = UpperCase(FPasTokenList[RIndex + 2]) then
          begin
            while FPasTokenList.TokenId[RIndex - 1] <> tkCRLF do
            begin
              dec(RIndex);
              if FPasTokenList.TokenId[RIndex] in [tkConstructor, tkDestructor, tkFunction, tkProcedure] then
                Found := True;
            end;
            if Found then ImplementationsList.Add(RIndex);
          end;
      end;
    end;
  end;
end; { GetMethodImplementation }

function TmSearcher.GetMethodImpLine(aClassName: String; aMethodIdentifier: String): LongInt;
var
  ImpIndex: LongInt;
begin
  ImpIndex := GetMethodImplementation(aClassName, aMethodIdentifier);
  Result := FPasTokenList.IndexAtLine(ImpIndex);
end; { GetMethodImpLine }

constructor TPasTokenList.Create;
begin
  inherited Create;
  FTokenPositionsList := TLongIntList.Create;
  FTokenPositionsList.Add(0);
  FComment := csNo;
  FEndCount := 0;
  Visibility := tkUnknown;
  Searcher := TmSearcher.Create(Self);
  FLines := TLongIntList.Create;
end; { Create }

destructor TPasTokenList.Destroy;
begin
  FTokenPositionsList.Free;
  Searcher.Free;
  FLines.Free;
  inherited Destroy;
end; { Destroy }

procedure TPasTokenList.SetOrigin(NewOrigin: PChar; NewSize: LongInt);
begin
  FOrigin := NewOrigin;
  Run := 0;
  fPCharSize := NewSize;
  fPCharCapacity := fPCharSize;
  Tokenize;
  Searcher.FillClassList;
  FRoundCount := 0;
  FSquareCount := 0;
end; { SetOrigin }

procedure TPasTokenList.WriteTo(InsPos, DelPos: LongInt;
  Item: string);
var
  StringCount, NewSize: Longint;
  aString: string;
begin
  aString := Item + (FOrigin + DelPos);
  StringCount := Length(aString);
  if (InsPos >= 0) and (StringCount >= 0) then
  begin
    NewSize := InsPos + StringCount;
    if NewSize > 0 then
    begin
      if NewSize >= FPCharCapacity then
      begin
        try
          FPCharCapacity := FPCharCapacity + StringCount +1024;
          ReAllocMem(FOrigin, PCharCapacity);
        except
          raise exception.Create('unable to reallocate PChar');
        end;
      end;
      StrECopy((FOrigin + InsPos), PChar(aString));
      FPCharSize := NewSize;
      FOrigin[FPCharSize] := #0;
      aString := '';
    end;
  end;
end; { WriteTo }

function TPasTokenList.GetCount: Integer;
begin
  Result := FTokenPositionsList.Count - 1;
end; { GetCount }

procedure TPasTokenList.SetCount(value: Integer);
begin
  FTokenPositionsList.Count := Value + 1;
end; { SetCount }

function TPasTokenList.GetCapacity: Integer;
begin
  Result := FTokenPositionsList.Capacity;
end; { GetCapacity }

procedure TPasTokenList.ResetPositionsFrom(Index, Value: LongInt);
begin
  while Index < FTokenPositionsList.Count do
  begin
    FTokenPositionsList[Index] := FTokenPositionsList[Index] + Value;
    inc(Index);
  end
end; { ResetPositionsFrom }

procedure TPasTokenList.ResetLines(Index, Value: LongInt);
begin
  Index:= IndexAtLine(Index) +1;
  while Index < FLines.Count do
  begin
    FLines[Index] := FLines[Index] + Value;
    inc(Index);
  end
end; { ResetPositionsFrom }

procedure TPasTokenList.ScanForLines;
begin
end; { ScanForLines }

function TPasTokenList.GetToken(Index: Integer): string;
var
  StartPos, EndPos, StringLen: LongInt;
begin
  StartPos := FTokenPositionsList[Index];
  EndPos := FTokenPositionsList[Index + 1];
  StringLen := EndPos - StartPos;
  SetString(Result, (FOrigin + StartPos), StringLen);
end; { GetToken }

function TPasTokenList.GetTokenPosition(Index: integer): Longint;
begin
  Result := FTokenPositionsList[Index];
end; { GetTokenPosition }

procedure TPasTokenList.SetCapacity(NewCapacity: Integer);
begin
  FTokenPositionsList.Capacity := NewCapacity;
end; { SetCapacity }

procedure TPasTokenList.SetToken(Index: Integer; Item: string);
var
  StartPos, EndPos, OldLen, NewLen, Diff: LongInt;
begin
  StartPos := FTokenPositionsList[Index];
  EndPos := FTokenPositionsList[Index + 1];
  OldLen := EndPos - StartPos;
  NewLen := Length(Item);
  Diff := NewLen - OldLen;
  ResetLines(Index, Diff);
  WriteTo(StartPos, EndPos, Item);
  ResetPositionsFrom(Index + 1, Diff);
end; { SetItems }

procedure TPasTokenList.Clear;
begin
  SetCount(0);
  FTokenPositionsList.Capacity := 1;
  Run := 0;
end; { Clear }

procedure TPasTokenList.Delete(Index: Integer);
var
  StartPos, EndPos, OldLen: LongInt;
begin
  StartPos := FTokenPositionsList[Index];
  EndPos := FTokenPositionsList[Index + 1];
  OldLen := EndPos - StartPos;
  ResetLines(Index -1, -OldLen);
  WriteTo(StartPos, EndPos, '');
  FTokenPositionsList.Delete(Index);
  ResetPositionsFrom(Index, -OldLen);
end; { Delete }

procedure TPasTokenList.DeleteGroup(StartIndex: LongInt; GroupCount: LongInt);
var
  StartPos, EndPos, OldLen: LongInt;
begin
  StartPos := FTokenPositionsList[StartIndex];
  EndPos := FTokenPositionsList[StartIndex + GroupCount];
  OldLen := EndPos - StartPos;
  ResetLines(StartIndex -1, -OldLen);
  WriteTo(StartPos, EndPos, '');
  FTokenPositionsList.DeleteGroup(StartIndex, GroupCount);
  ResetPositionsFrom(StartIndex, -OldLen);
end;  { DeleteGroup }

procedure TPasTokenList.Exchange(Index1, Index2: Integer);
var
  Item: string;
begin
  Item := GetToken(Index1);
  SetToken(Index1, GetToken(Index2));
  SetToken(Index2, Item);
end; { Exchange }

function TPasTokenList.First: string;
begin
  Result := GetToken(0);
end; { First }

function TPasTokenList.IndexOf(Item: string): Integer;
begin
  Result := 0;
  while (Result < Count) and (GetToken(Result) <> Item) do Inc(Result);
  if Result = Count then Result := -1;
end; { IndexOf }

procedure TPasTokenList.Insert(Index: Integer; Item: string);
var
  StartPos, EndPos, ItemLen: LongInt;
begin
  ItemLen := Length(Item);
  StartPos := FTokenPositionsList[Index];
  EndPos := StartPos + ItemLen;
  ResetLines(Index, ItemLen);
  WriteTo(StartPos, StartPos, Item);
  ResetPositionsFrom(Index + 1, ItemLen);
  FTokenPositionsList.Insert(Index + 1, EndPos);
end; { Insert }

function TPasTokenList.InsertString(StartIndex: LongInt; ToInsert: String): LongInt;
var
  I, StartPos, EndPos, ItemLen: LongInt;
  TempHelper: TPasTokenList;
begin
  TempHelper := TPasTokenList.Create;
  ItemLen := Length(ToInsert);
  TempHelper.SetOrigin(PChar(ToInsert), ItemLen);
  Result := TempHelper.Count;
  StartPos := FTokenPositionsList[StartIndex];
  ResetLines(StartIndex, ItemLen);
  WriteTo(StartPos, StartPos, ToInsert);
  ResetPositionsFrom(StartIndex + 1, ItemLen);
  for I := 0 to TempHelper.Count -1 do
  begin
    StartPos := FTokenPositionsList[StartIndex + I];
    EndPos := StartPos + Length(TempHelper[I]);
    FTokenPositionsList.Insert(StartIndex + I + 1, EndPos);
  end;
  TempHelper.Free;
end;   { InsertString }

function TPasTokenList.Last: string;
begin
  Result := GetToken(Count - 1);
end; { Last }

procedure TPasTokenList.Move(CurIndex, NewIndex: Integer);
var
  Item: string;
begin
  if CurIndex <> NewIndex then
  begin
    Item := GetToken(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, Item);
  end;
end; { Move }

function TPasTokenList.MoveGroup(OldStartIndex: LongInt; NewStartIndex: LongInt; GroupCount: LongInt): Boolean;
var
  TempString: String;
  TempStringLen, StartPos, EndPos: LongInt;
begin
  Result:= False;
  if NewStartIndex < OldStartIndex then
  begin
    Result:= True;
    StartPos := FTokenPositionsList[OldStartIndex];
    EndPos := FTokenPositionsList[OldStartIndex + GroupCount];
    TempStringLen := EndPos - StartPos;
    SetString(TempString, (FOrigin + StartPos), TempStringLen);
    InsertString(NewStartIndex, TempString);
    OldStartIndex := OldStartIndex + GroupCount;
    DeleteGroup(OldStartIndex, GroupCount);
    TempString := '';
  end;

  if (NewStartIndex > OldStartIndex + GroupCount)then
  begin
    Result:= True;
    StartPos := FTokenPositionsList[OldStartIndex];
    EndPos := FTokenPositionsList[OldStartIndex + GroupCount];
    TempStringLen := EndPos - StartPos;
    SetString(TempString, (FOrigin + StartPos), TempStringLen);
    InsertString(NewStartIndex, TempString);
    DeleteGroup(OldStartIndex, GroupCount);
    TempString := '';
  end;
end;  { MoveGroup }

function TPasTokenList.Remove(Item: string): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then Delete(Result);
end; { Remove }

function TPasTokenList.GetSubString(StartPos, EndPos: LongInt): string;
var
  SubLen: Integer;
begin
  if FOrigin[EndPos] = #10 then inc(EndPos);
  SubLen := EndPos - StartPos;
  SetString(Result, (FOrigin + StartPos), SubLen);
end; { GetSubString }

procedure TPasTokenList.SetRunIndex(NewPos: LongInt);
begin
  Run := NewPos;
end; { SetRunPos }

procedure TPasTokenList.HandleComments;
begin
  case FComment of
    csAnsi:
      begin
        while FOrigin[Walker] <> #0 do
        begin
          case FOrigin[Walker] of
            '*': if FOrigin[Walker + 1] = ')' then
              begin
                inc(Walker, 2);
                FComment := csNo;
                break;
              end;
            #10:
              begin
                inc(Walker);
                fLines.Add(Walker);
              end;

            #13:
              begin
                // StH!:
                if FOrigin[Walker + 1] = #10 then inc(Walker);
                fLines.Add(Walker);
                //if FOrigin[Walker + 1] = #10 then inc(Walker, 2) else inc(Walker);
                //fLines.Add(Walker);
              end;
          end;
          inc(Walker);
        end;
      end;

    csBor:
      begin
        while FOrigin[Walker] <> #0 do
        begin
          case FOrigin[Walker] of
            '}':
              begin
                inc(Walker);
                FComment := csNo;
                break;
              end;
            #10:
              begin
                inc(Walker);
                fLines.Add(Walker);
              end;

            #13:
              begin
                // StH!:
                if FOrigin[Walker + 1] = #10 then inc(Walker);
                fLines.Add(Walker);
                //if FOrigin[Walker + 1] = #10 then inc(Walker, 2) else inc(Walker);
                //fLines.Add(Walker);
              end;
          end;
          inc(Walker);
        end;
      end;

    csSlashes:
      begin
        while FOrigin[Walker] <> #0 do
        begin
          inc(Walker);
          case FOrigin[Walker] of
            #0, #10, #13:
              begin
                FComment := csNo;
                break;
              end;
          end;
        end;
      end;
  end;
  FTokenPositionsList.Add(Walker);
end; { HandleComments }

function TPasTokenList.IdentKind(Index: LongInt): TTokenKind;
var
  HashKey: Integer;
  aToken: String;
  StartPos, EndPos, StringLen: LongInt;

  function KeyHash: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for i := 1 to StringLen do
      Result := Result + Ord(aToken[i]);
  end; { KeyHash }
begin
  Result := tkIdentifier;
  StartPos := FTokenPositionsList[Index];
  EndPos := FTokenPositionsList[Index + 1];
  StringLen := EndPos - StartPos;
  SetString(aToken, (FOrigin + StartPos), StringLen);
  aToken := UpperCase(aToken);
  HashKey := KeyHash;
  case HashKey of
    143: if aToken = 'IF' then Result := tkIf;
    147: if aToken = 'DO' then Result := tkDo;
    148: if aToken = 'AS' then Result := tkAs;
    149: if aToken = 'OF' then Result := tkOf;
    151: if aToken = 'IN' then Result := tkIn;
    156: if aToken = 'IS' then Result := tkIs;
    161: if aToken = 'OR' then Result := tkOr;
    163: if aToken = 'TO' then Result := tkTo;
    211: if aToken = 'AND' then Result := tkAnd;
    215: if aToken = 'END' then Result := tkEnd;
    217: if aToken = 'FAR' then Result := tkFar;
    224: if aToken = 'MOD' then Result := tkMod;
    225: if aToken = 'ASM' then Result := tkAsm;
    227:
      begin
        if aToken = 'DIV' then Result := tkDiv else
          if aToken = 'NIL' then Result := tkNil;
      end;
    231:
      begin
        if aToken = 'FOR' then Result := tkFor else
          if aToken = 'SHL' then Result := tkShl;
      end;
    233: if aToken = 'VAR' then Result := tkVar;
    236: if aToken = 'SET' then Result := tkSet;
    237: if aToken = 'SHR' then Result := tkShr;
    241: if aToken = 'NOT' then Result := tkNot;
    248: if aToken = 'OUT' then Result := tkOut;
    249: if aToken = 'XOR' then Result := tkXor;
    255: if aToken = 'TRY' then Result := tkTry;
    284:
      begin
        if aToken = 'CASE' then Result := tkCase else
          if aToken = 'READ' then Result := tkRead;
      end;
    288: if aToken = 'FILE' then Result := tkFile;
    289: if aToken = 'NAME' then Result := tkName;
    294: if aToken = 'NEAR' then Result := tkNear;
    297: if aToken = 'ELSE' then Result := tkElse;
    303: if aToken = 'THEN' then Result := tkThen;
    313: if aToken = 'GOTO' then Result := tkGoto;
    316: if aToken = 'WITH' then Result := tkWith;
    320:
      begin
        if aToken = 'UNIT' then Result := tkUnit else
          if aToken = 'USES' then Result := tkUses;
      end;
    322: if aToken = 'TYPE' then Result := tkType;
    347: if aToken = 'CDECL' then Result := tkCdecl;
    352: if aToken = 'LABEL' then Result := tkLabel;
    357: if aToken = 'BEGIN' then Result := tkBegin;
    372: if aToken = 'RAISE' then Result := tkRaise;
    374: if aToken = 'CLASS' then Result := tkClass;
    376: if aToken = 'INDEX' then Result := tkIndex;
    377: if aToken = 'WHILE' then Result := tkWhile;
    383: if aToken = 'ARRAY' then Result := tkArray;
    391: if aToken = 'CONST' then Result := tkConst;
    395: if aToken = 'WRITE' then Result := tkWrite;
    396: if aToken = 'UNTIL' then Result := tkUntil;
    424: if aToken = 'PACKED' then Result := tkPacked;
    436: if aToken = 'PASCAL' then Result := tkPascal;
    439: if aToken = 'OBJECT' then Result := tkObject;
    445: if aToken = 'DISPID' then Result := tkDispid;
    447:
      begin
        if aToken = 'INLINE' then Result := tkInline else
          if aToken = 'PUBLIC' then Result := tkPublic else
            if aToken = 'RECORD' then Result := tkRecord;
      end;
    449: if aToken = 'REPEAT' then Result := tkRepeat;
    457: if aToken = 'EXCEPT' then Result := tkExcept;
    465: if aToken = 'STORED' then Result := tkStored;
    471: if aToken = 'STRING' then Result := tkKeyString;
    475: if aToken = 'DOWNTO' then Result := tkDownto;
    482: if aToken = 'EXPORT' then Result := tkExport;
    517:
      begin
        if aToken = 'DEFAULT' then Result := tkDefault else
          if aToken = 'DYNAMIC' then Result := tkDynamic else
            if aToken = 'MESSAGE' then Result := tkMessage;
      end;
    519: if aToken = 'STDCALL' then Result := tkStdcall;
    527: if aToken = 'FINALLY' then Result := tkFinally;
    533:
      begin
        if aToken = 'FORWARD' then Result := tkForward else
          if aToken = 'LIBRARY' then Result := tkLibrary;
      end;
    536: if aToken = 'PROGRAM' then Result := tkProgram;
    539: if aToken = 'PRIVATE' then Result := tkPrivate;
    551: if aToken = 'VIRTUAL' then Result := tkVirtual;
    565: if aToken = 'EXPORTS' then Result := tkExports;
    571: if aToken = 'SAFECALL' then Result := tkSafecall;
    596: if aToken = 'ABSTRACT' then Result := tkAbstract;
    606:
      begin
        if aToken = 'READONLY' then Result := tkReadonly else
          if aToken = 'RESIDENT' then Result := tkResident;
      end;
    607: if aToken = 'ABSOLUTE' then Result := tkAbsolute;
    608: if aToken = 'OVERRIDE' then Result := tkOverride;
    611: if aToken = 'EXTERNAL' then Result := tkExternal;
    613: if aToken = 'REGISTER' then Result := tkRegister;
    614: if aToken = 'FUNCTION' then Result := tkFunction;
    645: if aToken = 'PROPERTY' then Result := tkProperty;
    657: if aToken = 'INTERFACE' then Result := tkInterface;
    668: if aToken = 'INHERITED' then Result := tkInherited;
    670: if aToken = 'ASSEMBLER' then Result := tkAssembler;
    672: if aToken = 'PUBLISHED' then Result := tkPublished;
    673: if aToken = 'THREADVAR' then Result := tkThreadvar;
    674: if aToken = 'NODEFAULT' then Result := tkNodefault;
    676: if aToken = 'AUTOMATED' then Result := tkAutomated;
    681: if aToken = 'PROCEDURE' then Result := tkProcedure;
    682: if aToken = 'PROTECTED' then Result := tkProtected;
    717: if aToken = 'WRITEONLY' then Result := tkWriteonly;
    783: if aToken = 'DESTRUCTOR' then Result := tkDestructor;
    870: if aToken = 'CONSTRUCTOR' then Result := tkConstructor;
    904: if aToken = 'FINALIZATION' then Result := tkFinalization;
    961: if aToken = 'DISPINTERFACE' then Result := tkDispinterface;
    1062: if aToken = 'IMPLEMENTATION' then Result := tkImplementation;
    1064: if aToken = 'INITIALIZATION' then Result := tkInitialization;
    1087:
      begin
        if aToken = 'RESOURCESTRING' then Result := tkResourcestring else
          if aToken = 'STRINGRESOURCE' then Result := tkStringresource;
      end;
  end;
  case Result of
    tkCase: inc(FEndCount);
    tkClass: FEndCount := 1;
    tkBegin: inc(FEndCount);
    tkEnd: dec(FEndCount);
    tkRecord: inc(FEndCount);
    tkObject: inc(FEndCount);
  end;
end; { IdentKind }

procedure TPasTokenList.Tokenize;
begin
  Walker := 0;
  FLines.Clear;
  fLines.Add(0);
  Clear;
  while FOrigin[Walker] <> #0 do
  begin
    case FOrigin[Walker] of
      #10:
        begin
          inc(Walker);
          FTokenPositionsList.Add(Walker);
          fLines.Add(Walker);
        end;

      #13:
        begin
          if FOrigin[Walker + 1] = #10 then inc(Walker, 2) else inc(Walker);
          FTokenPositionsList.Add(Walker);
          fLines.Add(Walker);
        end;

      #1..#9, #11, #12, #14..#32:
        begin
          inc(Walker);
          while FOrigin[Walker] in [#1..#9, #11, #12, #14..#32] do inc(Walker);
          FTokenPositionsList.Add(Walker);
        end;

      '/':
        case FOrigin[Walker + 1] of
          '/':
            begin
              FComment := csSlashes;
              HandleComments;
            end;
        else
          begin
            inc(Walker);
            FTokenPositionsList.Add(Walker);
          end;
        end;

      'A'..'Z', 'a'..'z', '_':
        begin
          inc(Walker);
          while FOrigin[Walker] in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do inc(Walker);
          FTokenPositionsList.Add(Walker);
        end;

      '0'..'9':
        begin
          inc(Walker);
          while FOrigin[Walker] in ['0'..'9', '.', 'e', 'E'] do
          begin
            case FOrigin[Walker] of
              '.':
                if FOrigin[Walker + 1] = '.' then break;
            end;
            inc(Walker);
          end;
          FTokenPositionsList.Add(Walker);
        end;

      '{':
        begin
          FComment := csBor;
          HandleComments;
        end;

      '!', '"', '%', '&', '('..'.', ':'..'@', '['..'^', '`', '~':
        begin
          case FOrigin[Walker] of
            '(':
              case FOrigin[Walker + 1] of
                '*':
                  begin
                    FComment := csAnsi;
                    HandleComments;
                  end;
                '.': inc(Walker);
              end;

            '.':
              case FOrigin[Walker + 1] of
                '.': inc(Walker);
                ')': inc(Walker);
              end;

            ':':
              case FOrigin[Walker + 1] of
                '=': inc(Walker);
              end;

            '<':
              case FOrigin[Walker + 1] of
                '=': inc(Walker);

                '>': inc(Walker);
              end;

            '>':
              case FOrigin[Walker + 1] of
                '=': inc(Walker);
              end;
          end;
          inc(Walker);
          FTokenPositionsList.Add(Walker);
        end;

      #39:
        begin
          if (FOrigin[Walker + 1] = #39) and (FOrigin[Walker + 2] = #39) then inc(Walker, 2);
          repeat
            case FOrigin[Walker] of
              #0, #10, #13: break;
            end;
            inc(Walker);
          until FOrigin[Walker] = #39;
          if FOrigin[Walker] <> #0 then inc(Walker);
          FTokenPositionsList.Add(Walker);
        end;

      '#':
        begin
          inc(Walker);
          while FOrigin[Walker] in ['0'..'9'] do inc(Walker);
          FTokenPositionsList.Add(Walker);
        end;

      '$':
        begin
          inc(Walker);
          while FOrigin[Walker] in ['0'..'9', 'A'..'F', 'a'..'f'] do inc(Walker);
          FTokenPositionsList.Add(Walker);
        end;

    else
      begin
        inc(Walker);
        FTokenPositionsList.Add(Walker);
      end;
    end;
  end;
end; {Tokenize}

function TPasTokenList.GetTokenID(Index: LongInt): TTokenKind;
var
  Running: LongInt;
begin
  Result := tkUnknown;
  Running := FTokenPositionsList[Index];
  case FOrigin[Running] of
    #0: Result := tkNull;

    #10: Result := tkCRLF;

    #13: Result := tkCRLF;

    #1..#9, #11, #12, #14..#32: Result := tkSpace;

    'A'..'Z', 'a'..'z', '_': Result := IdentKind(Index);

    '0'..'9':
      begin
        inc(Running);
        Result := tkNumber;
        while FOrigin[Running] in ['0'..'9', '.'] do
        begin
          case FOrigin[Running] of
            '.':
              if FOrigin[Running + 1] <> '.' then Result := tkFloat else break;
          end;
          inc(Running);
        end;
      end;

    '{':
      begin
        Result := tkBorComment;
        if FOrigin[Running + 1] = '$' then Result := tkCompDirect;
      end;

    '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
      begin
        case FOrigin[Running] of
          '(':
            case FOrigin[Running + 1] of
              '*':
                begin
                  Result := tkAnsiComment;
                  if FOrigin[Running + 2] = '$' then Result := tkCompDirect;
                end;
              '.':
                begin
                  Result := tkSquareOpen;
                  inc(FSquareCount);
                end;
            else
              begin
                Result := tkRoundOpen;
                inc(FRoundCount);
              end;
            end;

          ')':
            begin
              Result := tkRoundClose;
              dec(FRoundCount);
            end;
          '*': Result := tkStar;
          '+': Result := tkPlus;
          ',': Result := tkComma;
          '-': Result := tkMinus;

          '.':
            case FOrigin[Running + 1] of
              '.': Result := tkDotDot;
              ')':
                begin
                  Result := tkSquareClose;
                  dec(FSquareCount);
                end;
            else Result := tkPoint;
            end;

          '/':
            case FOrigin[Running + 1] of
              '/': Result := tkSlashesComment;
            else Result := tkSlash;
            end;

          ':':
            case FOrigin[Running + 1] of
              '=': Result := tkAssign;
            else Result := tkColon;
            end;

          ';': Result := tkSemiColon;

          '<':
            case FOrigin[Running + 1] of
              '=': Result := tkLowerEqual;
              '>': Result := tkNotEqual;
            else Result := tkLower;
            end;

          '=': Result := tkEqual;

          '>':
            case FOrigin[Running + 1] of
              '=': Result := tkGreaterEqual;
            else Result := tkGreater;
            end;

          '[':
            begin
              Result := tkSquareOpen;
              inc(FSquareCount);
            end;
          ']':
            begin
              Result := tkSquareClose;
              dec(FSquareCount);
            end;

        else Result := tkSymbol;
        end;
      end;

    #39: Result := tkString;

    '#': Result := tkAsciiChar;

    '$': Result := tkInteger;

  end;
end; { GetTokenID }

procedure TPasTokenList.Next;
begin
  if Run < Count then inc(Run);
end; { Next }

procedure TPasTokenList.Previous;
begin
  if Run > 0 then dec(Run);
end; { Previous }

procedure TPasTokenList.NextID(ID: TTokenKind);
begin
  repeat
    case TokenID[Run] of
      tkNull: break;
    else inc(Run);
    end;
  until TokenID[Run] = ID;
end; { NextID }

function TPasTokenList.GetIsJunk: Boolean;
begin
  case TokenID[Run] of
    tkAnsiComment, tkBorComment, tkCRLF, tkSlashesComment, tkSpace:
      Result := True;
  else Result := False;
  end;
end;

procedure TPasTokenList.NextNonComment;
begin
  repeat
    case TokenID[Run] of
      tkNull: break;
    else inc(Run);
    end;
  until not (TokenID[Run] in [tkAnsiComment, tkBorComment, tkSlashesComment]);
end; { NextNonComCRLF }

procedure TPasTokenList.NextNonJunk;
begin
  repeat
    case TokenID[Run] of
      tkNull: break;
    else inc(Run);
    end;
  until not (TokenID[Run] in [tkAnsiComment, tkBorComment, tkCRLF,
    tkSlashesComment, tkSpace]);
end; { NextNonJunk }

procedure TPasTokenList.NextNonSpace;
begin
  repeat
    case TokenID[Run] of
      tkNull: break;
    else inc(Run);
    end;
  until not (TokenID[Run] = tkSpace);
end; { NextNonSpace }

procedure TPasTokenList.ToLineStart;
begin
  while TokenID[Run] <> tkCRLF do
  begin
    if Run <= 0 then break;
    dec(Run);
  end;
  inc(Run);
end; { ToLineStart }

procedure TPasTokenList.PreviousID(ID: TTokenKind);
begin
  repeat
    case Run of
      0: break;
    else dec(Run);
    end;
  until TokenID[Run] = ID;
end; { PreviousID }

procedure TPasTokenList.PreviousNonComment;
begin
  repeat
    case Run of
      0: break;
    else dec(Run);
    end;
  until not (TokenID[Run] in [tkAnsiComment, tkBorComment, tkSlashesComment]);
end; { PreviousNonComment }


procedure TPasTokenList.PreviousNonJunk;
begin
  repeat
    case Run of
      0: break;
    else dec(Run);
    end;
  until not (TokenID[Run] in [tkAnsiComment, tkBorComment, tkCRLF,
    tkSlashesComment, tkSpace]);
end; { PreviousNonJunk }

procedure TPasTokenList.PreviousNonSpace;
begin
  repeat
    case Run of
      0: break;
    else dec(Run);
    end;
  until not (TokenID[Run] = tkSpace);
end; { PreviousNonSpace }

function TPasTokenList.PositionAtLine(aPosition: LongInt): LongInt;
var
  First, Last, I: LongInt;
begin
  Result := -1;
  I := 0;
  if (aPosition >= 0) and (aPosition <= fPCharSize) then
  begin
    First := 0;
    Last := FLines.Count - 2;
    while First <= Last do
    begin
      I := (First + Last) shr 1;
      if aPosition < FLines[I] then Last := I - 1 else
      begin
        if aPosition < FLines[I + 1] then break;
        First := I + 1;
      end;
    end;
    Result := I;
  end;
end;

function TPasTokenList.IndexAtLine(anIndex: LongInt): LongInt;
begin
  Result := PositionAtLine(TokenPosition[anIndex]);
end;

function TPasTokenList.GetRunID: TTokenKind;
begin
  Result := GetTokenID(Run);
end; { GetRunID }

function TPasTokenList.GetRunPosition: LongInt;
begin
  Result := FTokenPositionsList[Run];
end; { GetRunPosition }

function TPasTokenList.GetRunToken: string;
var
  StartPos, EndPos, StringLen: LongInt;
begin
  StartPos := FTokenPositionsList[Run];
  EndPos := FTokenPositionsList[Run + 1];
  StringLen := EndPos - StartPos;
  SetString(Result, (FOrigin + StartPos), StringLen);
end; { GetRunToken }

function TPasTokenList.PositionToIndex(aPosition: LongInt): LongInt;
var
  First, Last, I: LongInt;
begin
  Result := -1;
  I := 0;
  if (aPosition >= 0) and (aPosition <= fPCharSize) then
  begin
    First := 0;
    Last := FTokenPositionsList.Count - 2;
    while First <= Last do
    begin
      I := (First + Last) shr 1;
      if aPosition < FTokenPositionsList[I] then Last := I - 1 else
      begin
        if aPosition < FTokenPositionsList[I + 1] then break;
        First := I + 1;
      end;
    end;
    Result := I;
  end;
end; { PositionToIndex }

function TPasTokenList.GetTokenLine(anIndex: LongInt): LongInt;
begin
  Result:= IndexAtLine(anIndex);
end;  { GetTokenLine }

function TPasTokenList.GetRunLine: LongInt;
begin
  Result:= IndexAtLine(Run);
end;  { GetRunLine }

end.

