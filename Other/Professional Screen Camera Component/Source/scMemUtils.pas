unit scMemUtils;

{$B-}

interface

uses Windows, SysUtils, Types, Math;

procedure Add(Target, Source: Pointer; TargetSize, SourceSize: Integer); overload;
function Add(var DynArray: TInt64DynArray; const Value: Int64): Integer; overload;
function Add(var DynArray: TIntegerDynArray; const Value: Integer): Integer; overload;
function Add(var DynArray: TByteDynArray; const Value: Byte): Integer; overload;
function Add(var DynArray: TSingleDynArray; const Value: Single): Integer; overload;
function Add(var DynArray: TDoubleDynArray; const Value: Double): Integer; overload;

function Delete(Target: Pointer; Index, Count, TargetSize: Integer): Boolean; overload;
function Delete(var DynArray: TInt64DynArray; Index: Integer): Boolean; overload;
function Delete(var DynArray: TIntegerDynArray; Index: Integer): Boolean; overload;
function Delete(var DynArray: TByteDynArray; Index: Integer): Boolean; overload;
function Delete(var DynArray: TSingleDynArray; Index: Integer): Boolean; overload;
function Delete(var DynArray: TDoubleDynArray; Index: Integer): Boolean; overload;

function IndexOf(Target, Source: Pointer; Count, SourceSize: Integer): Integer; overload;
function IndexOf(const DynArray: TInt64DynArray; Value: Int64): Integer; overload;
function IndexOf(const DynArray: TIntegerDynArray; Value: Integer): Integer; overload;
function IndexOf(const DynArray: TByteDynArray; Value: Byte): Integer; overload;
function IndexOf(const DynArray: TSingleDynArray; Value: Single): Integer; overload;
function IndexOf(const DynArray: TDoubleDynArray; Value: Double): Integer; overload;

function Insert(Target, Source: Pointer; Index, TargetSize, SourceSize: Integer): Boolean; overload;
function Insert(var DynArray: TInt64DynArray; const Value: Int64; Index: Integer): Boolean; overload;
function Insert(var DynArray: TIntegerDynArray; const Value: Integer; Index: Integer): Boolean; overload;
function Insert(var DynArray: TByteDynArray; const Value: Byte; Index: Integer): Boolean; overload;
function Insert(var DynArray: TSingleDynArray; const Value: Single; Index: Integer): Boolean; overload;
function Insert(var DynArray: TDoubleDynArray; const Value: Double; Index: Integer): Boolean; overload;

procedure Resize(Target: Pointer; PreviousTargetSize, TargetSize: Integer); overload;
procedure Resize(var DynArray: TInt64DynArray; Size: Longword); overload;
procedure Resize(var DynArray: TIntegerDynArray; Size: Longword); overload;
procedure Resize(var DynArray: TByteDynArray; Size: Longword); overload;
procedure Resize(var DynArray: TSingleDynArray; Size: Longword); overload;
procedure Resize(var DynArray: TDoubleDynArray; Size: Longword); overload;

procedure SetRandom(var DynArray: TIntegerDynArray);

implementation

procedure Add(Target, Source: Pointer; TargetSize, SourceSize: Integer);
begin
  CopyMemory(Pointer(Integer(Target) + TargetSize), Source, SourceSize);
end;

function Add(var DynArray: TInt64DynArray; const Value: Int64): Integer;
begin
  Result := Length(DynArray);
  SetLength(DynArray, Result + 1);
  Add(DynArray, @Value, Result * SizeOf(Int64), SizeOf(Int64));
end;

function Add(var DynArray: TIntegerDynArray; const Value: Integer): Integer;
begin
  Result := Length(DynArray);
  SetLength(DynArray, Result + 1);
  Add(DynArray, @Value, Result * SizeOf(Integer), SizeOf(Integer));
end;

function Add(var DynArray: TByteDynArray; const Value: Byte): Integer;
begin
  Result := Length(DynArray);
  SetLength(DynArray, Result + 1);
  Add(DynArray, @Value, Result * SizeOf(Byte), SizeOf(Byte));
end;

function Add(var DynArray: TSingleDynArray; const Value: Single): Integer;
begin
  Result := Length(DynArray);
  SetLength(DynArray, Result + 1);
  Add(DynArray, @Value, Result * SizeOf(Single), SizeOf(Single));
end;

function Add(var DynArray: TDoubleDynArray; const Value: Double): Integer;
begin
  Result := Length(DynArray);
  SetLength(DynArray, Result + 1);
  Add(DynArray, @Value, Result * SizeOf(Double), SizeOf(Double));
end;

function Delete(Target: Pointer; Index, Count, TargetSize: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index + Count <= TargetSize);
  if Result and (Index + Count < TargetSize) then
    MoveMemory(Pointer(Integer(Target) + Index), Pointer(Integer(Target) +
      Index + Count), TargetSize - Index - Count);
end;

function Delete(var DynArray: TInt64DynArray; Index: Integer): Boolean;
var
  I: Integer;
begin
  I := Length(DynArray);
  Result := Delete(DynArray, Index * SizeOf(Int64), SizeOf(Int64),
    I * SizeOf(Int64));
  if Result then SetLength(DynArray, I - 1);
end;

function Delete(var DynArray: TIntegerDynArray; Index: Integer): Boolean;
var
  I: Integer;
begin
  I := Length(DynArray);
  Result := Delete(DynArray, Index * SizeOf(Integer), SizeOf(Integer),
    I * SizeOf(Integer));
  if Result then SetLength(DynArray, I - 1);
end;

function Delete(var DynArray: TByteDynArray; Index: Integer): Boolean;
var
  I: Integer;
begin
  I := Length(DynArray);
  Result := Delete(DynArray, Index * SizeOf(Byte), SizeOf(Byte),
    I * SizeOf(Byte));
  if Result then SetLength(DynArray, I - 1);
end;

function Delete(var DynArray: TSingleDynArray; Index: Integer): Boolean;
var
  I: Integer;
begin
  I := Length(DynArray);
  Result := Delete(DynArray, Index * SizeOf(Single), SizeOf(Single),
    I * SizeOf(Single));
  if Result then SetLength(DynArray, I - 1);
end;

function Delete(var DynArray: TDoubleDynArray; Index: Integer): Boolean;
var
  I: Integer;
begin
  I := Length(DynArray);
  Result := Delete(DynArray, Index * SizeOf(Double), SizeOf(Double),
    I * SizeOf(Double));
  if Result then SetLength(DynArray, I - 1);
end;

function IndexOf(Target, Source: Pointer; Count, SourceSize: Integer): Integer;
var
  I, J: Integer;
begin
  I := 0;
  J := 0;
  while I < Count do
  begin
    if CompareMem(Source, Pointer(Integer(Target) + J), SourceSize) then
    begin
      Result := I;
      Exit;
    end;
    Inc(I);
    Inc(J, SourceSize);
  end;
  Result := -1;
end;

function IndexOf(const DynArray: TInt64DynArray; Value: Int64): Integer;
begin
  Result := IndexOf(DynArray, @Value, Length(DynArray), SizeOf(Int64));
end;

function IndexOf(const DynArray: TIntegerDynArray; Value: Integer): Integer;
begin
  Result := IndexOf(DynArray, @Value, Length(DynArray), SizeOf(Integer));
end;

function IndexOf(const DynArray: TByteDynArray; Value: Byte): Integer;
begin
  Result := IndexOf(DynArray, @Value, Length(DynArray), SizeOf(Byte));
end;

function IndexOf(const DynArray: TSingleDynArray; Value: Single): Integer;
begin
  Result := IndexOf(DynArray, @Value, Length(DynArray), SizeOf(Single));
end;

function IndexOf(const DynArray: TDoubleDynArray; Value: Double): Integer;
begin
  Result := IndexOf(DynArray, @Value, Length(DynArray), SizeOf(Double));
end;

function Insert(Target, Source: Pointer; Index, TargetSize, SourceSize: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index <= TargetSize);
  if not Result then Exit;
  if Index < TargetSize then
    MoveMemory(Pointer(Integer(Target) + SourceSize + Index),
      Pointer(Integer(Target) + Index), TargetSize - Index);
  CopyMemory(Pointer(Integer(Target) + Index), Source, SourceSize);
end;

function Insert(var DynArray: TInt64DynArray; const Value: Int64; Index: Integer): Boolean;
var
  I: Integer;
begin
  I := Length(DynArray);
  SetLength(DynArray, I + 1);
  Result := Insert(DynArray, @Value, Index * SizeOf(Int64), I * SizeOf(Int64),
    SizeOf(Int64));
end;

function Insert(var DynArray: TIntegerDynArray; const Value: Integer; Index: Integer): Boolean;
var
  I: Integer;
begin
  I := Length(DynArray);
  SetLength(DynArray, I + 1);
  Result := Insert(DynArray, @Value, Index * SizeOf(Integer),
    I * SizeOf(Integer), SizeOf(Integer));
end;

function Insert(var DynArray: TByteDynArray; const Value: Byte; Index: Integer): Boolean;
var
  I: Integer;
begin
  I := Length(DynArray);
  SetLength(DynArray, I + 1);
  Result := Insert(DynArray, @Value, Index * SizeOf(Byte),
    I * SizeOf(Byte), SizeOf(Byte));
end;

function Insert(var DynArray: TSingleDynArray; const Value: Single; Index: Integer): Boolean;
var
  I: Integer;
begin
  I := Length(DynArray);
  SetLength(DynArray, I + 1);
  Result := Insert(DynArray, @Value, Index * SizeOf(Single),
    I * SizeOf(Single), SizeOf(Single));
end;

function Insert(var DynArray: TDoubleDynArray; const Value: Double; Index: Integer): Boolean;
var
  I: Integer;
begin
  I := Length(DynArray);
  SetLength(DynArray, I + 1);
  Result := Insert(DynArray, @Value, Index * SizeOf(Double),
    I * SizeOf(Double), SizeOf(Double));
end;

procedure Resize(Target: Pointer; PreviousTargetSize, TargetSize: Integer);
begin
  if TargetSize > PreviousTargetSize then
    ZeroMemory(Pointer(Integer(Target) + PreviousTargetSize),
      TargetSize - PreviousTargetSize);
end;

procedure Resize(var DynArray: TInt64DynArray; Size: Longword);
var
  I: Integer;
begin
  I := Length(DynArray);
  SetLength(DynArray, Size);
  Resize(DynArray, I * SizeOf(Int64), Size * SizeOf(Int64));
end;

procedure Resize(var DynArray: TIntegerDynArray; Size: Longword);
var
  I: Integer;
begin
  I := Length(DynArray);
  SetLength(DynArray, Size);
  Resize(DynArray, I * SizeOf(Integer), Size * SizeOf(Integer));
end;

procedure Resize(var DynArray: TByteDynArray; Size: Longword);
var
  I: Integer;
begin
  I := Length(DynArray);
  SetLength(DynArray, Size);
  Resize(DynArray, I * SizeOf(Byte), Size * SizeOf(Byte));
end;

procedure Resize(var DynArray: TSingleDynArray; Size: Longword);
var
  I: Integer;
begin
  I := Length(DynArray);
  SetLength(DynArray, Size);
  Resize(DynArray, I * SizeOf(Single), Size * SizeOf(Single));
end;

procedure Resize(var DynArray: TDoubleDynArray; Size: Longword);
var
  I: Integer;
begin
  I := Length(DynArray);
  SetLength(DynArray, Size);
  Resize(DynArray, I * SizeOf(Double), Size * SizeOf(Double));
end;

procedure SetRandom(var DynArray: TIntegerDynArray);
var
  I, J, K: Integer;
  Numbers: TIntegerDynArray;
begin
  SetLength(Numbers, Length(DynArray));
  try
    for I := Low(Numbers) to High(Numbers) do Numbers[I] := I;
    I := 0;
    K := Length(Numbers);
    while K > 0 do
    begin
      J := Random(K);
      DynArray[I] := Numbers[J];
      Delete(Numbers, J);
      Inc(I);
      K := Length(Numbers);
    end;
  finally
    Numbers := nil;
  end;
end;

initialization
  Randomize;

end.
