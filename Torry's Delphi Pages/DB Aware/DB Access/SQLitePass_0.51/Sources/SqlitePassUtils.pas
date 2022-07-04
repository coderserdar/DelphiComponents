{     The unit SqlitePassUtils implement missing classes and functions for
      the Delphi4 or Delphi 6 VCL. It uses mostly original code from FCL.
      implements :
      - TObjectList for Delphi 4
      - TSqlitePassBitArray (Bit Array <-> Integer converter)
      - TSqlitePassList (A TList with less properties but faster)
      - TSqlitePassAnsiStringList (A TStringList with less properties but faster)
      - TSqlitePassWideStringList (A TStringList with less properties but faster)
      - Strings functions (String <-> UTF8 converter)
      - TSqlitePassIntegerList (a mini TList implementation to manage Integers)


   ---------------------------------------------------------------------------
      
   This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation; either version 2.1 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

   ---------------------------------------------------------------------------

    Author : Luc DAVID Email: luckylazarus@free.fr
    Last update : 10.03.2010

   --------------------------------------------------------------------------- }

unit SqlitePassUtils;
{$i SqlitePassDbo.inc}
interface

uses

{$IFDEF Delphi2009}
 WideStrUtils,
{$ENDIF}

{$IFNDEF FPC}
 Consts,
{$ENDIF}

{$IFDEF WIN32}
 Windows,
 {$IFDEF Delphi6}
  RtlConsts,
 {$ENDIF}
{$ENDIF}

SqlitePassConst, SqlitePassErrorLang, Classes, SysUtils, Db;


Type
{$IFDEF Delphi2009}
  SizeInt     = Longint;
  SizeUInt    = DWord;
  PRecBuffer  = TRecordBuffer;
  UTF16WideString = String;
  PUTF16WideString = ^UTF16WideString;
  PDateTimeRec = ^TDateTimeRec;
  PPAnsiChar  = ^PAnsiChar;
{$ELSE}
  {$IFNDEF FPC}
  SizeInt      = Longint;
  SizeUInt     = DWord;
  UTF8String   = AnsiString; //type AnsiString(65001);
  PUTF8String  = ^UTF8String;
  PCardinal    = ^Cardinal;
  PDateTimeRec = ^TDateTimeRec;
  PPointer     = ^Pointer;
  PWordBool    = ^WordBool;
  PPAnsiChar   = ^PAnsiChar;
  {$ENDIF FPC}
  PRecBuffer   = PAnsiChar;
  UTF16WideString = WideString;
  PUTF16WideString = ^UTF16WideString;
{$ENDIF Delphi2009}


  PPRecBuffer   = ^PRecBuffer;
  PSqliteValue  = Pointer;
  PPSqliteValue = ^PSqliteValue;
  PByteBool     = ^ByteBool;

  PRecBufferList = ^TRecBufferList;
  TRecBufferList = array[0..MaxListSize - 1] of PRecBuffer;

  PIntegerList = ^TIntegerList;
  TIntegerList = array[0..MaxListSize - 1] of Integer;

  TSqlitePassList = class(TObject)
  private
    FList: PPointerList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer);
    procedure SetCapacity(NewCapacity: Integer);
    function GetFCount: Integer;
    procedure SetCount(NewCount: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TSqlitePassList;
    function First: Pointer;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Pack;
    function Remove(Item: Pointer): Integer;
    property Capacity: Integer read FCapacity write SetCapacity;
    property List: PPointerList read FList;
    public
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure Delete(Index: Integer);
    procedure Clear; dynamic;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    end;

  TSqlitePassObjectList = class(TSqlitePassList)
    Protected
    function GetItem(Index: Integer): TObject;
    Procedure SetItem(Index: Integer; AObject: TObject);
    function IndexOf(AObject: TObject): Integer;
    Procedure Insert(Index: Integer; AObject: TObject);
    function First: TObject;
    Function Last: TObject;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
    public
    // TODO
    function Add(AObject: TObject): Integer; // Moved to Public section to enable
                                             // access MyIndexDef.Columns.Add(MyIndexColumn;
                                             // demo program...
    function Remove(AObject: TObject): Integer; // Moved to Public section...same
  end;


  PSqlitePassAnsiStringItem = ^TSqlitePassAnsiStringItem;
  TSqlitePassAnsiStringItem = record FString: AnsiString; end;

  PSqlitePassAnsiStringItemList = ^TSqlitePassAnsiStringItemList;
  TSqlitePassAnsiStringItemList = array[0..MaxListSize] of TSqlitePassAnsiStringItem;

  TSqlitePassAnsiStringList = class
  private
    FList: PSqlitePassAnsiStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    procedure SetCapacity(NewCapacity: Integer);
  protected
    function   Get(Index: Integer): Ansistring;
    procedure  Put(Index: Integer; const S: Ansistring);
  public
    destructor Destroy; override;
    procedure  Add(const S: AnsiString);
    procedure  Clear;
    procedure  Delete(Index: Integer);
    procedure  Grow;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property Strings[Index: Integer]: Ansistring read Get write Put; default;
  end;


  PSqlitePassWideStringItem = ^TSqlitePassWideStringItem;
  TSqlitePassWideStringItem = record FWideString: UTF16WideString; end;

  PSqlitePassWideStringItemList = ^TSqlitePassWideStringItemList;
  TSqlitePassWideStringItemList = array[0..MaxListSize] of TSqlitePassWideStringItem;

  TSqlitePassWideStringList = class
  private
    FList: PSqlitePassWideStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    procedure SetCapacity(NewCapacity: Integer);
  protected
    function   Get(Index: Integer): UTF16WideString;
    procedure  Put(Index: Integer; const S: UTF16WideString);
  public
    destructor Destroy; override;
    procedure  Add(const S: UTF16WideString);
    procedure  Clear;
    procedure  Delete(Index: Integer);
    procedure  Grow;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property Strings[Index: Integer]: UTF16WideString read Get write Put; default;
  end;

  TSqlitePassBitArray = class
  private
  FIsZeroBased: Boolean;
  FItems: String;
  FInternalItems: TStringList;
  FMaxSize: Word;
  FOffset: Word;
  FValue: LongWord;
  FUpdating: Boolean;
  procedure CheckFirstItem;
  function  GetFValue: LongWord;
  procedure SetFValue(Value: LongWord);
  Procedure SetFIsZeroBased(Const Value: Boolean);
  Procedure SetFItems(Const Value: String);
  Procedure Reset;
  public
  constructor Create;
  destructor Destroy; override;
  function IsTrue(Const Item: String): Boolean;
  procedure SetItemValue(Const Item: String; Const Value: Boolean);
  property IsZeroBased: Boolean Read FIsZeroBased Write SetFIsZeroBased;
  property Items: String Read FItems Write SetFItems;
  property Value: LongWord Read GetFValue Write SetFValue;
  end;


  TSqlitePassIntegerList = class(TObject)
  private
    FList: PIntegerList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Integer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Integer);
    procedure SetCapacity(NewCapacity: Integer);
    function GetFCount: Integer;
    procedure SetCount(NewCount: Integer);
    property Capacity: Integer read FCapacity write SetCapacity;
    property List: PIntegerList read FList;
    public
    function Add(Item: Integer): Integer;
    procedure Clear; dynamic;
    procedure Delete(Index: Integer);
    destructor Destroy; override;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Integer read Get write Put; default;
    end;

  { ------------------------------------------------------------------------ }

  //function GetExeDir: String;
  //function GetWorkDir: String;

  Procedure OutputDebugString(S: String);

  function FieldTypeToString(FieldType: TFieldType): String;
  function StringToFieldType(FieldTypeName: String): TFieldType;
  function BoolAsString(Value: Boolean): String;

  function UTF8ToAnsi(const S: PAnsiChar): AnsiString;
  function AnsiToUTF8(const S: AnsiString): UTF8String;
  function WStrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar;
  function UTF8Encode(const s : WideString) : UTF8String;
  function UTF8Decode(const s : UTF8String): WideString;
  { Same as Pos but from Right to Left }
  function RPos(const aSubStr, aString : String; const aStartPos: Integer): Integer;



  {$IFDEF NeedSqlitePassWideStringUtils}
   //  Procedure SetWideString (Out S : WideString; Buf : PWideChar; Len : SizeInt);
   function _WStrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar;
  {$ENDIF NeedSqlitePassWideStringUtils}

  {$IFDEF NeedSqlitePassUnicodeUtils}
   { Conversion UTF8 <-> AnsiString }
   function _UTF8ToAnsi(const Source: AnsiString): AnsiString;
   function _AnsiToUTF8(const Source: AnsiString): UTF8String;
   { Conversion UTF16 <-> UTF8 }
   function _UTF8Encode(const s : WideString) : UTF8String;
   function _UTF8Decode(const s : UTF8String): WideString;
   function _UnicodeToUtf8(Dest: PChar; Source: PWideChar; MaxBytes: SizeInt): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif} overload;
   function _UnicodeToUtf8(Dest: PChar; MaxDestBytes: SizeUInt; Source: PWideChar; SourceChars: SizeUInt): SizeUInt; overload;
   function _Utf8ToUnicode(Dest: PWideChar; Source: PChar; MaxChars: SizeInt): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif} overload;
   function _Utf8ToUnicode(Dest: PWideChar; MaxDestChars: SizeUInt; Source: PChar; SourceBytes: SizeUInt): SizeUInt; overload;
  {$ENDIF NeedSqlitePassUnicode}


implementation



{ ----- Directories Utilities ----- }

(*function GetExeDir: String;
begin
 SetLength(Result,1001);
 {$IFDEF WIN32}
 GetModuleFileName(HInstance,PChar(Result),1000);
 {$ENDIF};
  Result := ExtractFilePath(StrPas(PChar(Result)));
end;

function GetWorkDir: String;
begin
 GetDir(0, Result);
 if Result[Length(Result)] <> '\' then Result:=Result+'\';
end;*)

{ ----- Debug utilities ----- }
Procedure OutputDebugString(S: String);
begin
 UniqueString(S);
 {$IFDEF WIN32}
 Windows.OutputDebugString(PChar(S));
 {$ENDIF}
end;

{ ----- Fields utilities ----- }

function FieldTypeToString(FieldType: TFieldType): String;
begin
Case FieldType of
     ftString      : Result := 'String';
     ftSmallint    : Result := 'Smallint';
     ftInteger     : Result := 'Integer';
     ftWord        : Result := 'Word';
     ftBoolean     : Result := 'Boolean';
     ftFloat       : Result := 'Float';
     ftCurrency    : Result := 'Currency';
     ftBCD         : Result := 'BCD';
     ftDate        : Result := 'Date';
     ftTime        : Result := 'Time';
     ftDateTime    : Result := 'DateTime';
     ftBytes       : Result := 'Bytes';
     ftVarBytes    : Result := 'VarBytes';
     ftAutoInc     : Result := 'AutoInc';
     ftBlob        : Result := 'Blob';
     ftMemo        : Result := 'Memo';
     ftGraphic     : Result := 'Graphic';
     ftFmtMemo     : Result := 'FmtMemo';
     ftParadoxOle  : Result := 'ParadoxOle';
     ftDBaseOle    : Result := 'DBaseOle';
     ftTypedBinary : Result := 'TypedBinary';
     ftCursor      : Result := 'Cursor';
     ftFixedChar   : Result := 'FixedChar';
     ftWideString  : Result := 'WideString';
     ftLargeint    : Result := 'Largeint';
     ftADT         : Result := 'ADT';
     ftArray       : Result := 'Array';
     ftReference   : Result := 'Reference';
     ftDataSet     : Result := 'DataSet';
     else
     Result := 'Unknown';
     end;
end;

function StringToFieldType(FieldTypeName: String): TFieldType;
var
i: TFieldType;
begin
Result := ftUnknown;
for i := Low(TFieldType) to High(TFieldType) do
    if AnsiCompareText(FieldTypeName, FieldTypeNames[i]) = 0 then
       begin
       Result := i;
       Break
       end;
end;

function BoolAsString(Value: Boolean): String;
begin
Case Value of
     True  : Result := 'True';
     False : Result := 'False';
     end;
end;


{ TSqlitePassList }

destructor TSqlitePassList.Destroy;
begin
  Clear;
end;

function TSqlitePassList.Add(Item: Pointer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TSqlitePassList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TSqlitePassList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then raise EListError.CreateFmt(SqlitePassListIndexError, [Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Pointer));
end;


procedure TSqlitePassList.Exchange(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then EListError.CreateFmt(SqlitePassListIndexError, [Index1]);
  if (Index2 < 0) or (Index2 >= FCount) then EListError.CreateFmt(SqlitePassListIndexError, [Index2]);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TSqlitePassList.Expand: TSqlitePassList;
begin
  if FCount = FCapacity then Grow;
  Result := Self;
end;

function TSqlitePassList.First: Pointer;
begin
  Result := Get(0);
end;

function TSqlitePassList.Get(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then EListError.CreateFmt(SqlitePassListIndexError, [IntToStr(Index)]);
  Result := FList^[Index];
end;

procedure TSqlitePassList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TSqlitePassList.IndexOf(Item: Pointer): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do Inc(Result);
  if Result = FCount then Result := -1;
end;

procedure TSqlitePassList.Insert(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index > FCount) then EListError.CreateFmt(SqlitePassListIndexError, [Index]);
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  FList^[Index] := Item;
  Inc(FCount);
end;

function TSqlitePassList.Last: Pointer;
begin
  Result := Get(FCount - 1);
end;

procedure TSqlitePassList.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then EListError.CreateFmt(SqlitePassListIndexError, [NewIndex]);
    Item := Get(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, Item);
  end;
end;

procedure TSqlitePassList.Put(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then EListError.CreateFmt(SqlitePassListIndexError, [Index]);
  FList^[Index] := Item;
end;

function TSqlitePassList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then Delete(Result);
end;

procedure TSqlitePassList.Pack;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do if Items[I] = nil then Delete(I);
end;

procedure TSqlitePassList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    EListError.CreateFmt(SqlitePassListCapacityError, [NewCapacity]);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;
  end;
end;

procedure TSqlitePassList.SetCount(NewCount: Integer);
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    EListError.CreateFmt(SqlitePassListCountError, [NewCount]);
  if NewCount > FCapacity then SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0);
  FCount := NewCount;
end;

function TSqlitePassList.GetFCount: Integer;
begin
Result := FCount;
end;


{ TSqlitePassObjectList }

Function TSqlitePassObjectList.GetItem(Index: Integer): TObject;
begin
  Result:=TObject(Inherited Get(Index));
end;

Procedure TSqlitePassObjectList.SetItem(Index: Integer; AObject: TObject);
begin
  Put(Index,Pointer(AObject));
end;

Function TSqlitePassObjectList.Add(AObject: TObject): Integer;
begin
  Result:=Inherited Add(Pointer(AObject));
end;

Function TSqlitePassObjectList.Remove(AObject: TObject): Integer;
begin
  Result:=Inherited Remove(Pointer(AObject));
end;

Function TSqlitePassObjectList.IndexOf(AObject: TObject): Integer;
begin
  Result:=Inherited indexOF(Pointer(AObject));
end;

procedure TSqlitePassObjectList.Insert(Index: Integer; AObject: TObject);
begin
  Inherited Insert(Index,Pointer(AObject));
end;

function TSqlitePassObjectList.First: TObject;
begin
  Result := TObject(Inherited First);
end;

function TSqlitePassObjectList.Last: TObject;
begin
  Result := TObject(Inherited Last);
end;


{ TSqlitePassAnsiStringList }

destructor TSqlitePassAnsiStringList.Destroy;
begin
  inherited Destroy;
  Clear;
end;

procedure TSqlitePassAnsiStringList.Add(const S: AnsiString);
begin
  if FCount = FCapacity then Grow;
  with FList^[FCount] do
       begin
       Pointer(FString) := nil;
       FString := S;
       end;
  Inc(FCount);
end;

procedure TSqlitePassAnsiStringList.Clear;
begin
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

procedure TSqlitePassAnsiStringList.Delete(Index: Integer);
begin
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount
     then System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(TSqlitePassAnsiStringItem));
end;

procedure TSqlitePassAnsiStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
       Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TSqlitePassAnsiStringList.Get(Index: Integer): AnsiString;
begin
  Result := FList^[Index].FString;
end;

procedure TSqlitePassAnsiStringList.Put(Index: Integer; const S: AnsiString);
begin
  FList^[Index].FString := S;
end;

procedure TSqlitePassAnsiStringList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TSqlitePassAnsiStringItem));
  FCapacity := NewCapacity;
end;



{ TSqlitePassWideStringList }

destructor TSqlitePassWideStringList.Destroy;
begin
  inherited Destroy;
  Clear;
end;

procedure TSqlitePassWideStringList.Add(const S: UTF16WideString);
begin
  if FCount = FCapacity then Grow;
  with FList^[FCount] do
       begin
       Pointer(FWideString) := nil;
       FWideString := S;
       end;
  Inc(FCount);
end;

procedure TSqlitePassWideStringList.Clear;
begin
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

procedure TSqlitePassWideStringList.Delete(Index: Integer);
begin
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount
     then System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(TSqlitePassWideStringItem));
end;

procedure TSqlitePassWideStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TSqlitePassWideStringList.Get(Index: Integer): UTF16WideString;
begin
  Result := FList^[Index].FWideString;
end;

procedure TSqlitePassWideStringList.Put(Index: Integer; const S: UTF16WideString);
begin
  FList^[Index].FWideString := S;
end;

procedure TSqlitePassWideStringList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TSqlitePassWideStringItem));
  FCapacity := NewCapacity;
end;


{ TSqlitePassBitArray }

{ Utility object to manipulate an array of boolean values
  manipulated with a stringlist. This implementation is 'the easy way'
  and is surely not the fastest way to do it, but as we are limited up to
  32 (LongWord bit size) iterations, speed gain shouldn't be very important.

  Usage:
  1 - Create your TSqlitePassBitArray
    MyBooleanList := TSqlitePassBitArray.Create;

  2 - Set your list of items (number of items is limited to 32 in current
      implementation)
    MyBooleanList.Items := (Item1,Item2,Item3,Item4,Item5,Item6);
    or
    MyBooleanList.Items := (Item1;Item2;item3;item4;Item5;Item6);
      Item1=0
      Item2=0
      Item3=0
      Item4=0
      Item5=0
      Item6=0

  3 - Change the IsZeroBased propery if needed.
    if IsZeroBased is True
      then the first item of your list indicates whether
      or not ALL the others boolean values are false or if at
      least one item is set to True.
      In our example Item1=1 indicates that
      Item2=0
      Item3=0
      Item4=0
      Item5=0
      Item6=0

      In our example, we consider IsZeroBased = True,
      so Item1=0 indicates that at least one item is set to True :
      Item2=0
      Item3=0
      Item4=0
      Item5=1
      Item6=0

  4 - Set the value to set up the corresponding boolean state of each item
    MyBooleanList.Value := 11; ( 1011 in binary )
    will gives the resulting stringList :
      Item1=0 (at least one of the others items is set to True)
      --------
      Item2=1
      Item3=1
      Item4=0
      Item5=1
      --------
      Item6=0

  5 - Manipulate the values according to your needs :
    MyBooleanList.SetItemValue('Item6',True);
    MyBooleanList.SetItemValue('Item3',False);
    will give
      Item1=0 (at least one of the others items is set to True)
      Item2=1
      Item3=0
      Item4=0
      Item5=1
      Item6=1

  6 - Retrieve the integer representation of your boolean values
      MyValueToStore := MyBooleanList.Value;
      The value returned is 25 (1+8+16) }

constructor TSqlitePassBitArray.Create;
begin
FInternalItems:=TStringList.Create;
FUpdating := False;
FMaxSize := (SizeOf(Value)*8)-1;
IsZeroBased := True;
end;

destructor TSqlitePassBitArray.Destroy;
begin
FInternalItems.Free;
end;

{ We convert the Boolean values of the stringlist
  to a LongWord. This value will be easily stored in a
  field or in any integer variable }
function TSqlitePassBitArray.GetFValue: LongWord;
var
i: Integer;
begin
Result := 0;
for i := Pred(FInternalItems.Count) Downto FOffset do
    if FInternalItems.Values[FInternalItems.Names[i]] = '1'
       then Result := Result + (1 shl (i-Foffset));
end;

{ Returns True if the Item value is True otherwise returns False }
function TSqlitePassBitArray.IsTrue(Const Item: String): Boolean;
begin
Try
{ do Not Use Result := Boolean(StrToInt(FInternalItems.Values[Item]));
   Compiler doesn't warn but Boolean Transtypage is not safe }
Result := StrToInt(FInternalItems.Values[Item]) <> 0;
Except
Result := False;
End;
end;

{ Change the boolean value of an item }
procedure TSqlitePassBitArray.SetItemValue(Const Item: String; Const Value: Boolean);
var
i: Integer;
begin
i := FInternalItems.IndexOfName(Item);
if i > -1 then
   begin
   FInternalItems.Strings[i] := FInternalItems.Names[i] + '=' + IntToStr(Integer(Value));
   If not FUpdating then CheckFirstItem;
   end;
end;

{ Set the global value. This value is converted to binary
  and every item boolean value is updated }
procedure TSqlitePassBitArray.SetFValue(Value: LongWord);
var
i: Word;
begin
FValue := Value;
Reset;
i := FOffset;
While Value > 0 do
  begin
  SetItemValue(FInternalItems.Names[i], (Value mod 2) = 1);
  Value := Value div 2;
  Inc(i)
  end;
end;

{ Create the list and the items }
procedure TSqlitePassBitArray.SetFItems(Const Value: String);
begin
FInternalItems.BeginUpdate;
try
FItems := Value;
Reset;
If not FUpdating then CheckFirstItem;
finally
FInternalItems.EndUpdate;
end;
end;

procedure TSqlitePassBitArray.SetFIsZeroBased(const Value: Boolean);
begin
FIsZeroBased := Value;
if FIsZeroBased
   then FOffset := 1
   else FOffset := 0;
{ Recalc the items boolean values }
SetFValue(FValue);
end;

procedure TSqlitePassBitArray.CheckFirstItem;
var
i: integer;
begin
Try
{ Internal flag to avoid SetItemValue and CheckFirstItem
  to be called recursively }
FUpdating := True;

if IsZeroBased
   then begin
   SetItemValue(FInternalItems.Names[0], True);
   for i := FOffset to Pred(FInternalItems.Count) do
   if IsTrue(FinternalItems.Names[i])
      then begin
           SetItemValue(FInternalItems.Names[0], False);
           Exit;
           end;
   end;
finally
FUpdating := False;
end;
end;

procedure TSqlitePassBitArray.Reset;
var
Start, i, FItemsLength: integer;
begin
FInternalItems.Clear;
i := 1;
FItemsLength := Length(FItems);
While i < FItemsLength do
    begin
    Start := i;
    While i <= FItemsLength do
    if (not (FItems[i] in [',' , ';']))
       then Inc(i)
       else Break;
    FInternalItems.Add(System.Copy(FItems, Start, i-Start)+'=0');
    Inc(i);
    end;
end;



{ TSqlitePassIntegerList }

destructor TSqlitePassIntegerList.Destroy;
begin
  Clear;
end;

function TSqlitePassIntegerList.Add(Item: Integer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TSqlitePassIntegerList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TSqlitePassIntegerList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then EListError.CreateFmt(SqlitePassListIndexError, [IntToStr(Index)]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Integer));
end;

function TSqlitePassIntegerList.Get(Index: Integer): Integer;
begin
  if (Index < 0) or (Index >= FCount) then EListError.CreateFmt(SqlitePassListIndexError, [Index]);
  Result := FList^[Index];
end;

procedure TSqlitePassIntegerList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TSqlitePassIntegerList.Put(Index: Integer; Item: Integer);
begin
  if (Index < 0) or (Index >= FCount) then EListError.CreateFmt(SqlitePassListIndexError, [Index]);
  FList^[Index] := Item;
end;

procedure TSqlitePassIntegerList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    EListError.CreateFmt(SqlitePassListCapacityError, [NewCapacity]);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Integer));
    FCapacity := NewCapacity;
  end;
end;

procedure TSqlitePassIntegerList.SetCount(NewCount: Integer);
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    EListError.CreateFmt(SqlitePassListCountError, [NewCount]);
  if NewCount > FCapacity then SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Integer), 0);
  FCount := NewCount;
end;

function TSqlitePassIntegerList.GetFCount: Integer;
begin
Result := FCount;
end;


{ --------------------------------------------------------------------- }

function WStrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar;
begin
{$IFDEF Delphi2009 }
 Result := WideStrUtils.WStrCopy(Dest, Source);
{$ELSE}
 Result :=  _WStrCopy(Dest, Source);
{$ENDIF}
end;

{ Same as Pos but from right to left - Code extracted from StackOverflow http://www.deltics.co.nz/blog/ }
function RPos(const aSubStr, aString : String; const aStartPos: Integer): Integer; overload;
var
  i: Integer;
  pStr: PChar;
  pSub: PChar;
begin
  pSub := Pointer(aSubStr);

  for i := aStartPos downto 1 do
  begin
    pStr := @(aString[i]);
    if (pStr^ = pSub^) then
    begin
      if CompareMem(pSub, pStr, Length(aSubStr)) then
      begin
        result := i;
        EXIT;
      end;
    end;
  end;

  result := 0;
end;




{ AnsiString <-> UTF8 Conversion }
function UTF8ToAnsi(const S: PAnsiChar): AnsiString;
begin
{$IFDEF FPC}
  { We could use UTF8ToSys(S) instead but speed test gives
    almost the same result as Utf8ToAnsi(S) }
  Result := System.Utf8ToAnsi(S);
  {$ELSE}
  {$IFDEF DELPHI7}
  Result := System.Utf8ToAnsi(S);
  {$ELSE}
    {$IFDEF DELPHI4}
     Result := _Utf8ToAnsi(S);
     {$ELSE}
     {Delphi 2009}
     Result := System.Utf8ToAnsi(S);
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
end;

function AnsiToUTF8(const S: AnsiString): UTF8String;
begin
{$IFDEF FPC}
  Result := System.AnsiToUtf8(S);
{$ELSE}
  {$IFDEF DELPHI7}
  Result := System.AnsiToUtf8(S);
  {$ELSE}
    {$IFDEF DELPHI4}
     Result := _AnsiToUtf8(S);
     {$ELSE}
     { Delphi 2009 }
     Result := System.AnsiToUtf8(S);
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
end;

function UTF8Encode(const s : WideString) : UTF8String ;
begin
{$IFDEF FPC}
  Result := System.UTF8Encode(S);
{$ELSE}
  {$IFDEF DELPHI6}
  Result := System.UTF8Encode(S);
  {$ELSE}
    {$IFDEF DELPHI4}
     Result := _UTF8Encode(S);
     {$ELSE}
     { Delphi 2009 }
     Result := System.UTF8Encode(S);
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
end;

function UTF8Decode(const s : UTF8String): WideString;
begin
{$IFDEF FPC}
  Result := System.UTF8Decode(S);
{$ELSE}
  {$IFDEF DELPHI6}
  Result := System.UTF8Decode(S);
  {$ELSE}
    {$IFDEF DELPHI4}
     Result := _UTF8Decode(S);
     {$ELSE}
     { Delphi 2009 }
     Result := System.UTF8Decode(S);
     //System.Utf8ToWideString
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
end;


{ WideStrings Utility functions }

{$IFDEF NeedSqlitePassWideStringUtils}

(*Procedure SetWideString (Out S : WideString; Buf : PWideChar; Len : SizeInt);
var
  BufLen: SizeInt;
begin
  SetLength(S,Len);
  If (Buf<>Nil) and (Len>0) then
    begin
      BufLen := IndexWord(Buf^, Len+1, 0);
      If (BufLen>0) and (BufLen < Len) then
        Len := BufLen;
      Move (Buf[0],S[1],Len*sizeof(WideChar));
      PWideChar(Pointer(S)+Len*sizeof(WideChar))^:=#0;
    end;
end;*)


function _WStrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar;
var
  Src : PWideChar;
begin
  Result := Dest;
  Src := Source;
  while (Src^ <> #$00) do
  begin
    Dest^ := Src^;
    Inc(Src);
    Inc(Dest);
  end;
  Dest^ := #$00;
end;
{$ENDIF NeedSqlitePassWideStringUtils}


{ Unicode Utility functions - Mainly Code Extraction from FPC wustrings.inc }

//  TODO SpeedTest

{$IFDEF NeedSqlitePassUnicodeUtils}
function _UTF8ToAnsi(const Source: AnsiString): AnsiString;
begin
 Result:= _Utf8Decode(Source);
end;

function _AnsiToUTF8(const Source: AnsiString): UTF8String;
begin
 Result:= _Utf8Encode(Source);
end;

function _UTF8Encode(const s : WideString) : UTF8String;
var
i : SizeInt;
hs : UTF8String;
begin
result:='';
if s='' then exit;
SetLength(hs,length(s)*3);
i:=SqlitePassUtils._UnicodeToUtf8(PAnsiChar(hs),length(hs)+1,PWideChar(s),length(s));
if i>0 then
  begin
    SetLength(hs,i-1);
    result:=hs;
  end;
end;

function _UTF8Decode(const s : UTF8String): WideString;
var
  i : SizeInt;
  hs : WideString;
begin
  result:='';
  if s='' then
    exit;
  SetLength(hs,length(s));
  i:=_Utf8ToUnicode(PWideChar(hs),length(hs)+1,PAnsiChar(s),length(s));

  if i>0 then
    begin
      SetLength(hs,i-1);
      result:=hs;
    end;
end;

function _UnicodeToUtf8(Dest: PAnsiChar; Source: PWideChar; MaxBytes: SizeInt): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  if assigned(Source)
     then Result:=_UnicodeToUtf8(Dest,MaxBytes,Source,Length(Source^))
     else Result:=0;
end;

function _UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: SizeUInt; Source: PWideChar; SourceChars: SizeUInt): SizeUInt;
  var
    i,j : SizeUInt;
    w : word;
  begin
    result:=0;
    if source=nil then
      exit;
    i:=0;
    j:=0;
    if assigned(Dest) then
      begin
        while (i<SourceChars) and (j<MaxDestBytes) do
          begin
            w:=word(Source[i]);
            case w of
              0..$7f:
                begin
                  Dest[j]:=char(w);
                  inc(j);
                end;
              $80..$7ff:
                begin
                  if j+1>=MaxDestBytes then
                    break;
                  Dest[j]:=char($c0 or (w shr 6));
                  Dest[j+1]:=char($80 or (w and $3f));
                  inc(j,2);
                end;
              else
                begin
                    if j+2>=MaxDestBytes then
                      break;
                    Dest[j]:=char($e0 or (w shr 12));
                    Dest[j+1]:=char($80 or ((w shr 6)and $3f));
                    Dest[j+2]:=char($80 or (w and $3f));
                    inc(j,3);
                end;
            end;
            inc(i);
          end;

        if j>MaxDestBytes-1 then
          j:=MaxDestBytes-1;

        Dest[j]:=#0;
      end
    else
      begin
        while i<SourceChars do
          begin
            case word(Source[i]) of
              $0..$7f:
                inc(j);
              $80..$7ff:
                inc(j,2);
              else
                inc(j,3);
            end;
            inc(i);
          end;
      end;
    result:=j+1;
  end;

function _Utf8ToUnicode(Dest: PWideChar; Source: PAnsiChar; MaxChars: SizeInt): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  if assigned(Source)
     then Result:=_Utf8ToUnicode(Dest,MaxChars,Source,strlen(Source))
     else Result:=0;
end;

function _Utf8ToUnicode(Dest: PWideChar; MaxDestChars: SizeUInt; Source: PAnsiChar; SourceBytes: SizeUInt): SizeUInt;
var
  i,j : SizeUInt;
  w: SizeUInt;
  b : byte;
begin
  if not assigned(Source) then
  begin
    result:=0;
    exit;
  end;
  result:=SizeUInt(-1);
  i:=0;
  j:=0;
  if assigned(Dest) then
    begin
      while (j<MaxDestChars) and (i<SourceBytes) do
        begin
          b:=byte(Source[i]);
          w:=b;
          inc(i);
          // 2 or 3 bytes?
          if b>=$80 then
            begin
              w:=b and $3f;
              if i>=SourceBytes then
                exit;
              // 3 bytes?
              if (b and $20)<>0 then
                begin
                  b:=byte(Source[i]);
                  inc(i);
                  if i>=SourceBytes then
                    exit;
                  if (b and $c0)<>$80 then
                    exit;
                  w:=(w shl 6) or (b and $3f);
                end;
              b:=byte(Source[i]);
              w:=(w shl 6) or (b and $3f);
              if (b and $c0)<>$80 then
                exit;
              inc(i);
            end;
          Dest[j]:=WideChar(w);
          inc(j);
        end;
      if j>=MaxDestChars then j:=MaxDestChars-1;
      Dest[j]:=#0;
    end
  else
    begin
      while i<SourceBytes do
        begin
          b:=byte(Source[i]);
          inc(i);
          // 2 or 3 bytes?
          if b>=$80 then
            begin
              if i>=SourceBytes then
                exit;
              // 3 bytes?
              b := b and $3f;
              if (b and $20)<>0 then
                begin
                  b:=byte(Source[i]);
                  inc(i);
                  if i>=SourceBytes then
                    exit;
                  if (b and $c0)<>$80 then
                    exit;
                end;
              if (byte(Source[i]) and $c0)<>$80 then
                exit;
              inc(i);
            end;
          inc(j);
        end;
    end;
  result:=j+1;
end;
{$ENDIF NeedSqlitePassUnicode}


// Delphi 2009

(*function UTF8ToString(const S: PAnsiChar): AnsiString;
begin
{$IFDEF UNICODE}
  Result := UTF8ToUnicodeString(S);
{$ELSE}
  Result := UTf8ToAnsi(S);
{$ENDIF}
end;

function UTF8ToUnicodeString(const S: PAnsiChar): UnicodeString; overload;
var
  L: Integer;
  Temp: UnicodeString;
begin
  Result := '';
  if S = '' then Exit;
  L := _strlenA(S);
  SetLength(Temp, L);

  L := Utf8ToUnicode(PWideChar(Temp), L + 1, S, L);
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;
------------------------------

{$IFDEF NEED_UTF8_SUPPORT}
type
  UTF8String = type string;
  PUTF8String = ^UTF8String;
function UTF8Encode(const WS: WideString): UTF8String;
function UTF8Decode(const S: UTF8String): WideString;
{$ENDIF}

implementation

uses SysUtils;

{$IFDEF NEED_UTF8_SUPPORT}
function UTF8Encode(const WS: WideString): UTF8String;
var
  i: cardinal;
  Pos: cardinal;
begin
  Pos:= 1;
  SetLength(Result, Length(WS) * 3);
  for i:= 1 to Length(WS) do
    case Ord(WS[i]) of
      $0000..$007F:
        begin
          Result[Pos]:= Char(Ord(WS[i]));
          inc(Pos);
        end;
      $0080..$07FF:
        begin
          Result[Pos]:= Char($C0 or Ord(WS[i]) shr 6); //110xxxxx
          Result[Pos + 1]:= Char($80 or Ord(WS[i]) and $3F); //10xxxxxx
          inc(Pos, 2);
        end;
      $0800..$FFFF:
        begin
          Result[Pos]:= Char($E0 or Ord(WS[i]) shr 12);
//1110xxxx
          Result[Pos + 1]:= Char($80 or Ord(WS[i]) shr 6 and $3F);
//10xxxxxx
          Result[Pos + 2]:= Char($80 or Ord(WS[i]) and $3F);
//10xxxxxx
          inc(Pos, 3);
        end;
    end;
  SetLength(Result, Pos - 1);
end;

function UTF8Decode(const S: UTF8String): WideString;
var
  SrcPos: integer;
  ResLen: integer;
resourcestring
  sInvalidStr = 'Invalid UTF8-string format.';
begin
  SrcPos:= 1;
  ResLen:= 0;
  SetLength(Result, Length(S));
  while SrcPos <= Length(S) do
  begin
    Inc(ResLen);
    if (Ord(S[SrcPos]) and $80) = 0 then // $0000..$007F - 0xxxxxxx
    begin
      Result[ResLen]:= WideChar(Ord(S[SrcPos]));
      Inc(SrcPos);
    end
    else if (Ord(S[SrcPos]) and $E0) = $C0 then // $0080..$07FF - 110xxxxx
10xxxxxx
    begin
      if (SrcPos + 1) > Length(S) then
        raise EConvertError.Create(sInvalidStr);
      if (Ord(S[SrcPos + 1]) and $C0) <> $80 then
        raise EConvertError.Create(sInvalidStr);
      Result[ResLen]:= WideChar($1F and Ord(S[SrcPos]) shl 6
        or Ord(S[SrcPos + 1]) and $3F);
      Inc(SrcPos, 2);
    end
    else if (Ord(S[SrcPos]) and $F0) = $E0 then // $0800..$FFFF - 1110xxxx
10xxxxxx 10xxxxxx
    begin
      if (SrcPos + 2) > Length(S) then
        raise EConvertError.Create(sInvalidStr);
      if (Ord(S[SrcPos + 1]) and $C0) <> $80 then
        raise EConvertError.Create(sInvalidStr);
      if (Ord(S[SrcPos + 2]) and $C0) <> $80 then
        raise EConvertError.Create(sInvalidStr);
      Result[ResLen]:= WideChar($0F and Ord(S[SrcPos]) shl 12
        or Ord(S[SrcPos + 1]) and $3F shl 6
        or Ord(S[SrcPos + 2]) and $3F);
      Inc(SrcPos, 3);
    end
    else
      raise EConvertError.Create(sInvalidStr);
  end;
  SetLength(Result, ResLen);
end;
{$ENDIF} 
//


(*

{:Converts Unicode string to Ansi string using specified code page.
  @param   ws       Unicode string.
  @param   codePage Code page to be used in conversion.
  @returns Converted ansi string.
}

function WideStringToString(const ws: WideString; codePage: Word): AnsiString;
var
  l: integer;
begin
  if ws = ' then
    Result := '
  else
  begin
    l := WideCharToMultiByte(codePage,
      WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
      @ws[1], - 1, nil, 0, nil, nil);
    SetLength(Result, l - 1);
    if l > 1 then
      WideCharToMultiByte(codePage,
        WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
        @ws[1], - 1, @Result[1], l - 1, nil, nil);
  end;
end; { WideStringToString }


{:Converts Ansi string to Unicode string using specified code page.
  @param   s        Ansi string.
  @param   codePage Code page to be used in conversion.
  @returns Converted wide string.
}
function StringToWideString(const s: AnsiString; codePage: Word): WideString;
var
  l: integer;
begin
  if s = ' then
    Result := '
  else
  begin
    l := MultiByteToWideChar(codePage, MB_PRECOMPOSED, PChar(@s[1]), - 1, nil, 0);
    SetLength(Result, l - 1);
    if l > 1 then
      MultiByteToWideChar(CodePage, MB_PRECOMPOSED, PChar(@s[1]),
        - 1, PWideChar(@Result[1]), l - 1);
  end;
end; { StringToWideString }
*)

(*function _UTF8ToAnsi(const Source: AnsiString): AnsiString;
var
WS: WideString;
S: String;
  l: integer;
begin
  if Source = '' then
    Result := ''
  else
  begin
    l := WideCharToMultiByte(CP_ACP,
      WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
      @Source[1], - 1, nil, 0, nil, nil);
    SetLength(WS, l);
    if l > 1 then
      WideCharToMultiByte(CP_ACP,
        WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
        @Source[1], - 1, @WS[1], l - 1, nil, nil);
  end;
s := Ws;
Result := s;

end;*)

(*function _UTF8ToAnsi(const Source: AnsiString): AnsiString;
var
WideDest: Widestring;
DestLen: Integer;
begin
  Result := '';
  If Length(Source) > 0 then
     begin
     { UTF8 -> UCS2 }
     DestLen := MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(Source), -1, nil, 0);
     //DestLen := MultiByteToWideChar(1251, 0, PAnsiChar(Source), -1, nil, 0);
     if DestLen = 0
        then RaiseLastWin32Error;

     SetLength(WideDest, DestLen);
     //MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(Source), -1, PWideChar(Widedest), DestLen);
     MultiByteToWideChar(1251, 0, PAnsiChar(Source), -1, PWideChar(Widedest), DestLen);
     Result := WideDest; { UCS2 -> ANSI }
     end;
end;


(*function _AnsiToUTF8(const Source: AnsiString): UTF8String;
var
WideSrc: Widestring;
DestLen: Integer;
begin
  Result := '';
  if Length(Source) > 0 then
     begin
     { ANSI -> UCS2 }
     WideSrc := Source;
     { UCS2 -> UTF8 }
     DestLen := WideCharToMultiByte(CP_UTF8, 0, PWideChar(WideSrc), -1, nil, 0, nil, nil);
     if DestLen = 0
       then RaiseLastWin32Error;

    SetLength(Result, DestLen);
    WideCharToMultiByte(CP_UTF8, 0, PWideChar(WideSrc), -1, PAnsiChar(Result), DestLen, nil, nil);
    end;
end;*)

end.
