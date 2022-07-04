{$INCLUDE ..\cDefines.inc}
unit cSparseArrays;

{                                                                              }
{                     Data structures: Sparse Arrays v3.03                     }
{                                                                              }
{             This unit is copyright © 2003-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                 Its original file name is cSparseArrays.pas                  }
{                      It was generated 1 Apr 2004 02:45.                      }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{                                                                              }
{ Description:                                                                 }
{   Sparse arrays are associative arrays where the index value is an           }
{   arbitrary integer.                                                         }
{                                                                              }
{   Associative arrays, also referred to as mappings, are unordered            }
{   collections where an arbitrary key can be used to index a value.           }
{                                                                              }
{   This unit implements sparse arrays that can hold the following values:     }
{     + String                                                                 }
{     + WideString                                                             }
{     + Int64                                                                  }
{     + TObject                                                                }
{                                                                              }
{   For example, the class TSparseStringArray is used where the key is an      }
{   arbitrary integer and the value a string.                                  }
{                                                                              }
{       Names := TSparseStringArray.Create;                                    }
{       Names[123] := 'John';                                                  }
{       Names[999] := 'Tori';                                                  }
{       if Names.HasItem(123) then                                             }
{         Names.Delete(123);                                                   }
{       Names.Free;                                                            }
{                                                                              }
{                                                                              }
{ Revision history:                                                            }
{   27/07/2003  0.01  Initial version (sparse object array).                   }
{   09/10/2003  3.02  Revised for Fundamentals 3.                              }
{   31/03/2004  3.03  Added sparse String, WideString and Int64 arrays.        }
{                                                                              }

interface

uses
  { Delphi }
  SysUtils,

  { Fundamentals }
  cTypes;



{                                                                              }
{ ASparseArray                                                                 }
{   Sparse array base class.                                                   }
{                                                                              }
type
  ASparseArray = class(AType)
  protected
    procedure IndexError(const ErrorClass: ExceptClass);
    function  GetCount: Integer; virtual; abstract;

  public
    property  Count: Integer read GetCount;
    function  IsEmpty: Boolean; override;
    procedure Delete(const Idx: Integer); virtual; abstract;
    function  HasItem(const Idx: Integer): Boolean; virtual; abstract;
  end;
  ESparseArray = class(EType);



{                                                                              }
{ TSparseStringArray                                                           }
{   Sparse array that holds String values.                                     }
{                                                                              }
type
  TSparseStringRecord = record
    Idx   : Integer;
    Value : String;
  end;
  PSparseStringRecord = ^TSparseStringRecord;
  TSparseStringRecordArray = Array of TSparseStringRecord;
  TSparseStringArrayHashList = Array of TSparseStringRecordArray;

  TSparseStringArray = class(ASparseArray)
  private
    FHashList : TSparseStringArrayHashList;
    FHashSize : Integer;
    FCount    : Integer;

  protected
    function  LocateItemRecord(const Idx: Integer;
              var LookupIdx, ChainIdx: Integer): PSparseStringRecord;
    procedure Rehash;

    function  GetCount: Integer; override;
    function  GetItem(const Idx: Integer): String;
    procedure SetItem(const Idx: Integer; const Value: String);

  public
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    property  Item[const Idx: Integer]: String read GetItem write SetItem; default;
    function  LocateItem(const Idx: Integer; var Value: String): Boolean;

    property  Count: Integer read FCount;
    function  IsEmpty: Boolean; override;
    procedure Clear; override;

    procedure Delete(const Idx: Integer); override;

    function  HasItem(const Idx: Integer): Boolean; override;
    function  FindFirst(var Idx: Integer; var Value: String): Boolean;
    function  FindNext(var Idx: Integer; var Value: String): Boolean;
  end;
  ESparseStringArray = class(ESparseArray);



{                                                                              }
{ TSparseWideStringArray                                                       }
{   Sparse array that holds WideString values.                                 }
{                                                                              }
type
  TSparseWideStringRecord = record
    Idx   : Integer;
    Value : WideString;
  end;
  PSparseWideStringRecord = ^TSparseWideStringRecord;
  TSparseWideStringRecordArray = Array of TSparseWideStringRecord;
  TSparseWideStringArrayHashList = Array of TSparseWideStringRecordArray;

  TSparseWideStringArray = class(ASparseArray)
  private
    FHashList : TSparseWideStringArrayHashList;
    FHashSize : Integer;
    FCount    : Integer;

  protected
    function  LocateItemRecord(const Idx: Integer;
              var LookupIdx, ChainIdx: Integer): PSparseWideStringRecord;
    procedure Rehash;

    function  GetCount: Integer; override;
    function  GetItem(const Idx: Integer): WideString;
    procedure SetItem(const Idx: Integer; const Value: WideString);

  public
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    property  Item[const Idx: Integer]: WideString read GetItem write SetItem; default;
    function  LocateItem(const Idx: Integer; var Value: WideString): Boolean;

    property  Count: Integer read FCount;
    function  IsEmpty: Boolean; override;
    procedure Clear; override;

    procedure Delete(const Idx: Integer); override;

    function  HasItem(const Idx: Integer): Boolean; override;
    function  FindFirst(var Idx: Integer; var Value: WideString): Boolean;
    function  FindNext(var Idx: Integer; var Value: WideString): Boolean;
  end;
  ESparseWideStringArray = class(ESparseArray);



{                                                                              }
{ TSparseInt64Array                                                            }
{   Sparse array that holds Int64 values.                                      }
{                                                                              }
type
  TSparseInt64Record = record
    Idx   : Integer;
    Value : Int64;
  end;
  PSparseInt64Record = ^TSparseInt64Record;
  TSparseInt64RecordArray = Array of TSparseInt64Record;
  TSparseInt64ArrayHashList = Array of TSparseInt64RecordArray;

  TSparseInt64Array = class(ASparseArray)
  private
    FHashList : TSparseInt64ArrayHashList;
    FHashSize : Integer;
    FCount    : Integer;

  protected
    function  LocateItemRecord(const Idx: Integer;
              var LookupIdx, ChainIdx: Integer): PSparseInt64Record;
    procedure Rehash;

    function  GetCount: Integer; override;
    function  GetItem(const Idx: Integer): Int64;
    procedure SetItem(const Idx: Integer; const Value: Int64);

  public
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    property  Item[const Idx: Integer]: Int64 read GetItem write SetItem; default;
    function  LocateItem(const Idx: Integer; var Value: Int64): Boolean;

    property  Count: Integer read FCount;
    function  IsEmpty: Boolean; override;
    procedure Clear; override;

    procedure Delete(const Idx: Integer); override;

    function  HasItem(const Idx: Integer): Boolean; override;
    function  FindFirst(var Idx: Integer; var Value: Int64): Boolean;
    function  FindNext(var Idx: Integer; var Value: Int64): Boolean;
  end;
  ESparseInt64Array = class(ESparseArray);



{                                                                              }
{ TSparseObjectArray                                                           }
{   Sparse array that holds TObject values.                                    }
{                                                                              }
type
  TSparseObjectRecord = record
    Idx   : Integer;
    Value : TObject;
  end;
  PSparseObjectRecord = ^TSparseObjectRecord;
  TSparseObjectRecordArray = Array of TSparseObjectRecord;
  TSparseObjectArrayHashList = Array of TSparseObjectRecordArray;

  TSparseObjectArray = class(ASparseArray)
  private
    FHashList    : TSparseObjectArrayHashList;
    FHashSize    : Integer;
    FCount       : Integer;
    FIsItemOwner : Boolean;

  protected
    procedure Init; override;

    function  LocateItemRecord(const Idx: Integer;
              var LookupIdx, ChainIdx: Integer): PSparseObjectRecord;
    procedure Rehash;

    function  GetCount: Integer; override;
    function  GetItem(const Idx: Integer): TObject;
    procedure SetItem(const Idx: Integer; const Value: TObject);

  public
    constructor Create(const IsItemOwner: Boolean = False);
    destructor Destroy; override;

    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    property  IsItemOwner: Boolean read FIsItemOwner write FIsItemOwner;
    property  Item[const Idx: Integer]: TObject read GetItem write SetItem; default;
    function  LocateItem(const Idx: Integer; var Value: TObject): Boolean;

    property  Count: Integer read FCount;
    function  IsEmpty: Boolean; override;
    procedure Clear; override;

    procedure Delete(const Idx: Integer); override;
    function  ReleaseItem(const Idx: Integer): TObject;

    function  HasItem(const Idx: Integer): Boolean; override;
    function  FindFirst(var Idx: Integer; var Value: TObject): Boolean;
    function  FindNext(var Idx: Integer; var Value: TObject): Boolean;
  end;
  ESparseObjectArray = class(ESparseArray);



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
procedure SelfTest;



implementation

uses
  { Fundamentals }
  cUtils;



{                                                                              }
{ Sparse array functions                                                       }
{                                                                              }
const
  AverageHashChainSize = 4;
  
function SparseArrayRehashSize(const Count: Integer): Integer;
var L : Integer;
begin
  L := Count div AverageHashChainSize; // Number of "slots"
  if L <= $10 then                     // Rehash in powers of 16
    Result := $10 else
  if L <= $100 then
    Result := $100 else
  if L <= $1000 then
    Result := $1000 else
  if L <= $10000 then
    Result := $10000 else
  if L <= $100000 then
    Result := $100000 else
  if L <= $1000000 then
    Result := $1000000 else
    Result := $10000000;
end;



{                                                                              }
{ ASparseArray                                                                 }
{                                                                              }
procedure ASparseArray.IndexError(const ErrorClass: ExceptClass);
begin
  RaiseTypeError('Index not found', nil, ErrorClass);
end;

function ASparseArray.IsEmpty: Boolean;
begin
  Result := GetCount = 0;
end;



{                                                                              }
{ TSparseStringArray                                                           }
{                                                                              }
procedure TSparseStringArray.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is TSparseStringArray then
    begin
      Clear;
      L := Length(TSparseStringArray(Source).FHashList);
      SetLength(FHashList, L);
      For I := 0 to L - 1 do
        FHashList[I] := Copy(TSparseStringArray(Source).FHashList[I]);
      FHashSize := TSparseStringArray(Source).FHashSize;
      FCount := TSparseStringArray(Source).FCount;
    end
  else
    inherited Assign(Source);
end;

procedure TSparseStringArray.Clear;
begin
  FHashList := nil;
  FHashSize := 0;
  FCount := 0;
end;

function TSparseStringArray.IsEqual(const V: TObject): Boolean;
var I, J : Integer;
    F, G : Integer;
    P, Q : PSparseStringRecord;
begin
  if V is TSparseStringArray then
    begin
      if FCount <> TSparseStringArray(V).FCount then
        begin
          Result := False;
          exit;
        end;
      For I := 0 to Length(FHashList) - 1 do
        For J := 0 to Length(FHashList[I]) - 1 do
          begin
            Q := @FHashList[I][J];
            P := TSparseStringArray(V).LocateItemRecord(Q^.Idx, F, G);
            if not Assigned(P) or (P^.Value <> Q^.Value) then
              begin
                Result := False;
                exit;
              end;
          end;
      Result := True;
    end
  else
    Result := inherited IsEqual(V);
end;

function TSparseStringArray.LocateItemRecord(const Idx: Integer;
    var LookupIdx, ChainIdx: Integer): PSparseStringRecord;
var H, I, J : Integer;
    P : PPointer;
    L : PInteger;
begin
  I := FHashSize;
  if (I = 0) or (FCount = 0) then
    begin
      LookupIdx := -1;
      ChainIdx := -1;
      Result := nil;
      exit;
    end;
  H := Integer(HashInteger(Idx) and (I - 1));
  LookupIdx := H;
  P := Pointer(FHashList);
  Inc(P, H);
  Result := P^;
  if Assigned(Result) then
    begin
      L := P^;
      Dec(L);
      J := Idx;
      For I := 0 to L^ - 1 do
        if Result^.Idx = J then
          begin
            ChainIdx := I;
            exit;
          end
        else
          Inc(Result);
      Result := nil;
    end;
  ChainIdx := -1;
end;

procedure TSparseStringArray.Rehash;
var I, J, R, F, H : Integer;
    N    : TSparseStringArrayHashList;
    P, Q : PSparseStringRecord;
begin
  R := SparseArrayRehashSize(FCount);
  SetLength(N, R);
  For I := 0 to Length(FHashList) - 1 do
    For J := 0 to Length(FHashList[I]) - 1 do
      begin
        P := @FHashList[I][J];
        H := Integer(HashInteger(P^.Idx) and (R - 1));
        F := Length(N[H]);
        SetLength(N[H], F + 1);
        Q := @N[H][F];
        Q^.Idx := P^.Idx;
        Q^.Value := P^.Value;
      end;
  FHashList := N;
  FHashSize := R;
end;

function TSparseStringArray.GetCount: Integer;
begin
  Result := FCount;
end;

function TSparseStringArray.GetItem(const Idx: Integer): String;
var P    : PSparseStringRecord;
    I, J : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError(ESparseStringArray);
  Result := P^.Value;
end;

function TSparseStringArray.LocateItem(const Idx: Integer; var Value: String): Boolean;
var P    : PSparseStringRecord;
    I, J : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if Assigned(P) then
    begin
      Value := P^.Value;
      Result := True;
    end
  else
    begin
      Value := '';
      Result := False;
    end;
end;

procedure TSparseStringArray.SetItem(const Idx: Integer; const Value: String);
var P    : PSparseStringRecord;
    I, J : Integer;
    L    : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if Assigned(P) then
    P^.Value := Value
  else
    begin
      L := FHashSize;
      if L = 0 then
        begin
          Rehash;
          L := FHashSize;
          Assert(L > 0);
        end;
      I := Integer(HashInteger(Idx) and (L - 1));
      J := Length(FHashList[I]);
      SetLength(FHashList[I], J + 1);
      P := @FHashList[I][J];
      P^.Idx := Idx;
      P^.Value := Value;
      Inc(FCount);
      if (FCount + 1) div AverageHashChainSize > L then
        Rehash;
    end;
end;

function TSparseStringArray.HasItem(const Idx: Integer): Boolean;
var I, J : Integer;
begin
  Result := Assigned(LocateItemRecord(Idx, I, J));
end;

function TSparseStringArray.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TSparseStringArray.FindFirst(var Idx: Integer; var Value: String): Boolean;
var I : Integer;
    P : PSparseStringRecord;
begin
  For I := 0 to Length(FHashList) - 1 do
    if Length(FHashList[I]) > 0 then
      begin
        P := @FHashList[I][0];
        Idx := P^.Idx;
        Value := P^.Value;
        Result := True;
        exit;
      end;
  Idx := -1;
  Value := '';
  Result := False;
end;

function TSparseStringArray.FindNext(var Idx: Integer; var Value: String): Boolean;
var P : PSparseStringRecord;
    I, J, L : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError(ESparseStringArray);
  Inc(J);
  if J >= Length(FHashList[I]) then
    begin
      J := 0;
      L := Length(FHashList);
      Inc(I);
      While I < L do
        if Length(FHashList[I]) > 0 then
          break
        else
          Inc(I);
      if I >= L then
        begin
          Idx := -1;
          Value := '';
          Result := False;
          exit;
        end;
    end;
  P := @FHashList[I][J];
  Idx := P^.Idx;
  Value := P^.Value;
  Result := True;
end;

procedure TSparseStringArray.Delete(const Idx: Integer);
var P    : PSparseStringRecord;
    I, J : Integer;
    L    : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError(ESparseStringArray);
  P^.Value := '';
  L := Length(FHashList[I]);
  if J < L - 1 then
    begin
      Move(FHashList[I][J + 1], FHashList[I][J], (L - J - 1) * Sizeof(TSparseStringRecord));
      ZeroMem(FHashList[I][L - 1], Sizeof(TSparseStringRecord));
    end;
  SetLength(FHashList[I], L - 1);
  Dec(FCount);
end;



{                                                                              }
{ TSparseWideStringArray                                                       }
{                                                                              }
procedure TSparseWideStringArray.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is TSparseWideStringArray then
    begin
      Clear;
      L := Length(TSparseWideStringArray(Source).FHashList);
      SetLength(FHashList, L);
      For I := 0 to L - 1 do
        FHashList[I] := Copy(TSparseWideStringArray(Source).FHashList[I]);
      FHashSize := TSparseWideStringArray(Source).FHashSize;
      FCount := TSparseWideStringArray(Source).FCount;
    end
  else
    inherited Assign(Source);
end;

procedure TSparseWideStringArray.Clear;
begin
  FHashList := nil;
  FHashSize := 0;
  FCount := 0;
end;

function TSparseWideStringArray.IsEqual(const V: TObject): Boolean;
var I, J : Integer;
    F, G : Integer;
    P, Q : PSparseWideStringRecord;
begin
  if V is TSparseWideStringArray then
    begin
      if FCount <> TSparseWideStringArray(V).FCount then
        begin
          Result := False;
          exit;
        end;
      For I := 0 to Length(FHashList) - 1 do
        For J := 0 to Length(FHashList[I]) - 1 do
          begin
            Q := @FHashList[I][J];
            P := TSparseWideStringArray(V).LocateItemRecord(Q^.Idx, F, G);
            if not Assigned(P) or (P^.Value <> Q^.Value) then
              begin
                Result := False;
                exit;
              end;
          end;
      Result := True;
    end
  else
    Result := inherited IsEqual(V);
end;

function TSparseWideStringArray.LocateItemRecord(const Idx: Integer;
    var LookupIdx, ChainIdx: Integer): PSparseWideStringRecord;
var H, I, J : Integer;
    P : PPointer;
    L : PInteger;
begin
  I := FHashSize;
  if (I = 0) or (FCount = 0) then
    begin
      LookupIdx := -1;
      ChainIdx := -1;
      Result := nil;
      exit;
    end;
  H := Integer(HashInteger(Idx) and (I - 1));
  LookupIdx := H;
  P := Pointer(FHashList);
  Inc(P, H);
  Result := P^;
  if Assigned(Result) then
    begin
      L := P^;
      Dec(L);
      J := Idx;
      For I := 0 to L^ - 1 do
        if Result^.Idx = J then
          begin
            ChainIdx := I;
            exit;
          end
        else
          Inc(Result);
      Result := nil;
    end;
  ChainIdx := -1;
end;

procedure TSparseWideStringArray.Rehash;
var I, J, R, F, H : Integer;
    N    : TSparseWideStringArrayHashList;
    P, Q : PSparseWideStringRecord;
begin
  R := SparseArrayRehashSize(FCount);
  SetLength(N, R);
  For I := 0 to Length(FHashList) - 1 do
    For J := 0 to Length(FHashList[I]) - 1 do
      begin
        P := @FHashList[I][J];
        H := Integer(HashInteger(P^.Idx) and (R - 1));
        F := Length(N[H]);
        SetLength(N[H], F + 1);
        Q := @N[H][F];
        Q^.Idx := P^.Idx;
        Q^.Value := P^.Value;
      end;
  FHashList := N;
  FHashSize := R;
end;

function TSparseWideStringArray.GetCount: Integer;
begin
  Result := FCount;
end;

function TSparseWideStringArray.GetItem(const Idx: Integer): WideString;
var P    : PSparseWideStringRecord;
    I, J : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError(ESparseWideStringArray);
  Result := P^.Value;
end;

function TSparseWideStringArray.LocateItem(const Idx: Integer; var Value: WideString): Boolean;
var P    : PSparseWideStringRecord;
    I, J : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if Assigned(P) then
    begin
      Value := P^.Value;
      Result := True;
    end
  else
    begin
      Value := '';
      Result := False;
    end;
end;

procedure TSparseWideStringArray.SetItem(const Idx: Integer; const Value: WideString);
var P    : PSparseWideStringRecord;
    I, J : Integer;
    L    : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if Assigned(P) then
    P^.Value := Value
  else
    begin
      L := FHashSize;
      if L = 0 then
        begin
          Rehash;
          L := FHashSize;
          Assert(L > 0);
        end;
      I := Integer(HashInteger(Idx) and (L - 1));
      J := Length(FHashList[I]);
      SetLength(FHashList[I], J + 1);
      P := @FHashList[I][J];
      P^.Idx := Idx;
      P^.Value := Value;
      Inc(FCount);
      if (FCount + 1) div AverageHashChainSize > L then
        Rehash;
    end;
end;

function TSparseWideStringArray.HasItem(const Idx: Integer): Boolean;
var I, J : Integer;
begin
  Result := Assigned(LocateItemRecord(Idx, I, J));
end;

function TSparseWideStringArray.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TSparseWideStringArray.FindFirst(var Idx: Integer; var Value: WideString): Boolean;
var I : Integer;
    P : PSparseWideStringRecord;
begin
  For I := 0 to Length(FHashList) - 1 do
    if Length(FHashList[I]) > 0 then
      begin
        P := @FHashList[I][0];
        Idx := P^.Idx;
        Value := P^.Value;
        Result := True;
        exit;
      end;
  Idx := -1;
  Value := '';
  Result := False;
end;

function TSparseWideStringArray.FindNext(var Idx: Integer; var Value: WideString): Boolean;
var P : PSparseWideStringRecord;
    I, J, L : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError(ESparseWideStringArray);
  Inc(J);
  if J >= Length(FHashList[I]) then
    begin
      J := 0;
      L := Length(FHashList);
      Inc(I);
      While I < L do
        if Length(FHashList[I]) > 0 then
          break
        else
          Inc(I);
      if I >= L then
        begin
          Idx := -1;
          Value := '';
          Result := False;
          exit;
        end;
    end;
  P := @FHashList[I][J];
  Idx := P^.Idx;
  Value := P^.Value;
  Result := True;
end;

procedure TSparseWideStringArray.Delete(const Idx: Integer);
var P    : PSparseWideStringRecord;
    I, J : Integer;
    L    : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError(ESparseWideStringArray);
  P^.Value := '';
  L := Length(FHashList[I]);
  if J < L - 1 then
    begin
      Move(FHashList[I][J + 1], FHashList[I][J], (L - J - 1) * Sizeof(TSparseWideStringRecord));
      ZeroMem(FHashList[I][L - 1], Sizeof(TSparseWideStringRecord));
    end;
  SetLength(FHashList[I], L - 1);
  Dec(FCount);
end;



{                                                                              }
{ TSparseInt64Array                                                            }
{                                                                              }
procedure TSparseInt64Array.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is TSparseInt64Array then
    begin
      Clear;
      L := Length(TSparseInt64Array(Source).FHashList);
      SetLength(FHashList, L);
      For I := 0 to L - 1 do
        FHashList[I] := Copy(TSparseInt64Array(Source).FHashList[I]);
      FHashSize := TSparseInt64Array(Source).FHashSize;
      FCount := TSparseInt64Array(Source).FCount;
    end
  else
    inherited Assign(Source);
end;

procedure TSparseInt64Array.Clear;
begin
  FHashList := nil;
  FHashSize := 0;
  FCount := 0;
end;

function TSparseInt64Array.IsEqual(const V: TObject): Boolean;
var I, J : Integer;
    F, G : Integer;
    P, Q : PSparseInt64Record;
begin
  if V is TSparseInt64Array then
    begin
      if FCount <> TSparseInt64Array(V).FCount then
        begin
          Result := False;
          exit;
        end;
      For I := 0 to Length(FHashList) - 1 do
        For J := 0 to Length(FHashList[I]) - 1 do
          begin
            Q := @FHashList[I][J];
            P := TSparseInt64Array(V).LocateItemRecord(Q^.Idx, F, G);
            if not Assigned(P) or (P^.Value <> Q^.Value) then
              begin
                Result := False;
                exit;
              end;
          end;
      Result := True;
    end
  else
    Result := inherited IsEqual(V);
end;

function TSparseInt64Array.LocateItemRecord(const Idx: Integer;
    var LookupIdx, ChainIdx: Integer): PSparseInt64Record;
var H, I, J : Integer;
    P : PPointer;
    L : PInteger;
begin
  I := FHashSize;
  if (I = 0) or (FCount = 0) then
    begin
      LookupIdx := -1;
      ChainIdx := -1;
      Result := nil;
      exit;
    end;
  H := Integer(HashInteger(Idx) and (I - 1));
  LookupIdx := H;
  P := Pointer(FHashList);
  Inc(P, H);
  Result := P^;
  if Assigned(Result) then
    begin
      L := P^;
      Dec(L);
      J := Idx;
      For I := 0 to L^ - 1 do
        if Result^.Idx = J then
          begin
            ChainIdx := I;
            exit;
          end
        else
          Inc(Result);
      Result := nil;
    end;
  ChainIdx := -1;
end;

procedure TSparseInt64Array.Rehash;
var I, J, R, F, H : Integer;
    N    : TSparseInt64ArrayHashList;
    P, Q : PSparseInt64Record;
begin
  R := SparseArrayRehashSize(FCount);
  SetLength(N, R);
  For I := 0 to Length(FHashList) - 1 do
    For J := 0 to Length(FHashList[I]) - 1 do
      begin
        P := @FHashList[I][J];
        H := Integer(HashInteger(P^.Idx) and (R - 1));
        F := Length(N[H]);
        SetLength(N[H], F + 1);
        Q := @N[H][F];
        Q^.Idx := P^.Idx;
        Q^.Value := P^.Value;
      end;
  FHashList := N;
  FHashSize := R;
end;

function TSparseInt64Array.GetCount: Integer;
begin
  Result := FCount;
end;

function TSparseInt64Array.GetItem(const Idx: Integer): Int64;
var P    : PSparseInt64Record;
    I, J : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError(ESparseInt64Array);
  Result := P^.Value;
end;

function TSparseInt64Array.LocateItem(const Idx: Integer; var Value: Int64): Boolean;
var P    : PSparseInt64Record;
    I, J : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if Assigned(P) then
    begin
      Value := P^.Value;
      Result := True;
    end
  else
    begin
      Value := 0;
      Result := False;
    end;
end;

procedure TSparseInt64Array.SetItem(const Idx: Integer; const Value: Int64);
var P    : PSparseInt64Record;
    I, J : Integer;
    L    : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if Assigned(P) then
    P^.Value := Value
  else
    begin
      L := FHashSize;
      if L = 0 then
        begin
          Rehash;
          L := FHashSize;
          Assert(L > 0);
        end;
      I := Integer(HashInteger(Idx) and (L - 1));
      J := Length(FHashList[I]);
      SetLength(FHashList[I], J + 1);
      P := @FHashList[I][J];
      P^.Idx := Idx;
      P^.Value := Value;
      Inc(FCount);
      if (FCount + 1) div AverageHashChainSize > L then
        Rehash;
    end;
end;

function TSparseInt64Array.HasItem(const Idx: Integer): Boolean;
var I, J : Integer;
begin
  Result := Assigned(LocateItemRecord(Idx, I, J));
end;

function TSparseInt64Array.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TSparseInt64Array.FindFirst(var Idx: Integer; var Value: Int64): Boolean;
var I : Integer;
    P : PSparseInt64Record;
begin
  For I := 0 to Length(FHashList) - 1 do
    if Length(FHashList[I]) > 0 then
      begin
        P := @FHashList[I][0];
        Idx := P^.Idx;
        Value := P^.Value;
        Result := True;
        exit;
      end;
  Idx := -1;
  Value := 0;
  Result := False;
end;

function TSparseInt64Array.FindNext(var Idx: Integer; var Value: Int64): Boolean;
var P : PSparseInt64Record;
    I, J, L : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError(ESparseInt64Array);
  Inc(J);
  if J >= Length(FHashList[I]) then
    begin
      J := 0;
      L := Length(FHashList);
      Inc(I);
      While I < L do
        if Length(FHashList[I]) > 0 then
          break
        else
          Inc(I);
      if I >= L then
        begin
          Idx := -1;
          Value := 0;
          Result := False;
          exit;
        end;
    end;
  P := @FHashList[I][J];
  Idx := P^.Idx;
  Value := P^.Value;
  Result := True;
end;

procedure TSparseInt64Array.Delete(const Idx: Integer);
var P    : PSparseInt64Record;
    I, J : Integer;
    L    : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError(ESparseInt64Array);
  L := Length(FHashList[I]);
  if J < L - 1 then
    Move(FHashList[I][J + 1], FHashList[I][J], (L - J - 1) * Sizeof(TSparseInt64Record));
  SetLength(FHashList[I], L - 1);
  Dec(FCount);
end;



{                                                                              }
{ TSparseObjectArray                                                           }
{                                                                              }
constructor TSparseObjectArray.Create(const IsItemOwner: Boolean);
begin
  inherited Create;
  FIsItemOwner := IsItemOwner;
end;

destructor TSparseObjectArray.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TSparseObjectArray.Init;
begin
  inherited Init;
  FIsItemOwner := False;
end;

procedure TSparseObjectArray.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is TSparseObjectArray then
    begin
      Clear;
      L := Length(TSparseObjectArray(Source).FHashList);
      SetLength(FHashList, L);
      For I := 0 to L - 1 do
        FHashList[I] := Copy(TSparseObjectArray(Source).FHashList[I]);
      FHashSize := TSparseObjectArray(Source).FHashSize;
      FCount := TSparseObjectArray(Source).FCount;
      FIsItemOwner := False;
    end
  else
    inherited Assign(Source);
end;

procedure TSparseObjectArray.Clear;
var I, J : Integer;
begin
  if FIsItemOwner then
    For I := 0 to Length(FHashList) - 1 do
      For J := 0 to Length(FHashList[I]) - 1 do
        FreeAndNil(FHashList[I][J].Value);
  FHashList := nil;
  FHashSize := 0;
  FCount := 0;
end;

function TSparseObjectArray.IsEqual(const V: TObject): Boolean;
var I, J : Integer;
    F, G : Integer;
    P, Q : PSparseObjectRecord;
begin
  if V is TSparseObjectArray then
    begin
      if FCount <> TSparseObjectArray(V).FCount then
        begin
          Result := False;
          exit;
        end;
      For I := 0 to Length(FHashList) - 1 do
        For J := 0 to Length(FHashList[I]) - 1 do
          begin
            Q := @FHashList[I][J];
            P := TSparseObjectArray(V).LocateItemRecord(Q^.Idx, F, G);
            if not Assigned(P) or (P^.Value <> Q^.Value) then
              begin
                Result := False;
                exit;
              end;
          end;
      Result := True;
    end
  else
    Result := inherited IsEqual(V);
end;

function TSparseObjectArray.LocateItemRecord(const Idx: Integer;
    var LookupIdx, ChainIdx: Integer): PSparseObjectRecord;
var H, I, J : Integer;
    P : PPointer;
    L : PInteger;
begin
  I := FHashSize;
  if (I = 0) or (FCount = 0) then
    begin
      LookupIdx := -1;
      ChainIdx := -1;
      Result := nil;
      exit;
    end;
  H := Integer(HashInteger(Idx) and (I - 1));
  LookupIdx := H;
  P := Pointer(FHashList);
  Inc(P, H);
  Result := P^;
  if Assigned(Result) then
    begin
      L := P^;
      Dec(L);
      J := Idx;
      For I := 0 to L^ - 1 do
        if Result^.Idx = J then
          begin
            ChainIdx := I;
            exit;
          end
        else
          Inc(Result);
      Result := nil;
    end;
  ChainIdx := -1;
end;

procedure TSparseObjectArray.Rehash;
var I, J, R, F, H : Integer;
    N    : TSparseObjectArrayHashList;
    P, Q : PSparseObjectRecord;
begin
  R := SparseArrayRehashSize(FCount);
  SetLength(N, R);
  For I := 0 to Length(FHashList) - 1 do
    For J := 0 to Length(FHashList[I]) - 1 do
      begin
        P := @FHashList[I][J];
        H := Integer(HashInteger(P^.Idx) and (R - 1));
        F := Length(N[H]);
        SetLength(N[H], F + 1);
        Q := @N[H][F];
        Q^.Idx := P^.Idx;
        Q^.Value := P^.Value;
      end;
  FHashList := N;
  FHashSize := R;
end;

function TSparseObjectArray.GetCount: Integer;
begin
  Result := FCount;
end;

function TSparseObjectArray.GetItem(const Idx: Integer): TObject;
var P    : PSparseObjectRecord;
    I, J : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError(ESparseObjectArray);
  Result := P^.Value;
end;

function TSparseObjectArray.LocateItem(const Idx: Integer; var Value: TObject): Boolean;
var P    : PSparseObjectRecord;
    I, J : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if Assigned(P) then
    begin
      Value := P^.Value;
      Result := True;
    end
  else
    begin
      Value := nil;
      Result := False;
    end;
end;

procedure TSparseObjectArray.SetItem(const Idx: Integer; const Value: TObject);
var P    : PSparseObjectRecord;
    I, J : Integer;
    L    : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if Assigned(P) then
    P^.Value := Value
  else
    begin
      L := FHashSize;
      if L = 0 then
        begin
          Rehash;
          L := FHashSize;
          Assert(L > 0);
        end;
      I := Integer(HashInteger(Idx) and (L - 1));
      J := Length(FHashList[I]);
      SetLength(FHashList[I], J + 1);
      P := @FHashList[I][J];
      P^.Idx := Idx;
      P^.Value := Value;
      Inc(FCount);
      if (FCount + 1) div AverageHashChainSize > L then
        Rehash;
    end;
end;

function TSparseObjectArray.HasItem(const Idx: Integer): Boolean;
var I, J : Integer;
begin
  Result := Assigned(LocateItemRecord(Idx, I, J));
end;

function TSparseObjectArray.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TSparseObjectArray.FindFirst(var Idx: Integer; var Value: TObject): Boolean;
var I : Integer;
    P : PSparseObjectRecord;
begin
  For I := 0 to Length(FHashList) - 1 do
    if Length(FHashList[I]) > 0 then
      begin
        P := @FHashList[I][0];
        Idx := P^.Idx;
        Value := P^.Value;
        Result := True;
        exit;
      end;
  Idx := -1;
  Value := nil;
  Result := False;
end;

function TSparseObjectArray.FindNext(var Idx: Integer; var Value: TObject): Boolean;
var P : PSparseObjectRecord;
    I, J, L : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError(ESparseObjectArray);
  Inc(J);
  if J >= Length(FHashList[I]) then
    begin
      J := 0;
      L := Length(FHashList);
      Inc(I);
      While I < L do
        if Length(FHashList[I]) > 0 then
          break
        else
          Inc(I);
      if I >= L then
        begin
          Idx := -1;
          Value := nil;
          Result := False;
          exit;
        end;
    end;
  P := @FHashList[I][J];
  Idx := P^.Idx;
  Value := P^.Value;
  Result := True;
end;

procedure TSparseObjectArray.Delete(const Idx: Integer);
var P    : PSparseObjectRecord;
    I, J : Integer;
    L    : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError(ESparseObjectArray);
  if FIsItemOwner then
    FreeAndNil(P^.Value);
  L := Length(FHashList[I]);
  if J < L - 1 then
    Move(FHashList[I][J + 1], FHashList[I][J], (L - J - 1) * Sizeof(TSparseObjectRecord));
  SetLength(FHashList[I], L - 1);
  Dec(FCount);
end;

function TSparseObjectArray.ReleaseItem(const Idx: Integer): TObject;
var P    : PSparseObjectRecord;
    I, J : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError(ESparseObjectArray);
  Result := P^.Value;
  P^.Value := nil;
end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
procedure SelfTest;
var A, D : TSparseObjectArray;
    B : Array[0..2] of TObject;
    I, J : Integer;
    V : TObject;
    S, T : TSparseStringArray;
begin
  B[0] := TObject.Create;
  B[1] := TObject.Create;
  B[2] := TObject.Create;
  A := TSparseObjectArray.Create;
  try
    Assert(A.Count = 0);
    Assert(A.IsEmpty);
    Assert(not A.FindFirst(I, V));
    Assert(A.IsEqual(A));
    Assert(not A.LocateItem(0, V));
    Assert(not Assigned(V));
    A[100] := B[0];
    Assert(A.Count = 1);
    Assert(not A.IsEmpty);
    Assert(A[100] = B[0]);
    Assert(not A.LocateItem(0, V));
    Assert(A.LocateItem(100, V));
    Assert(V = B[0]);
    Assert(not A.HasItem(1000));
    A[1000] := B[1];
    Assert(A.HasItem(1000));
    Assert(A.Count = 2);
    Assert(A[1000] = B[1]);
    A[-50000] := B[2];
    Assert(A.Count = 3);
    Assert(A[100] = B[0]);
    Assert(A[1000] = B[1]);
    Assert(A[-50000] = B[2]);
    Assert(A.IsEqual(A));
    A[100] := B[1];
    Assert(A[100] = B[1]);
    A.Delete(1000);
    Assert(A.Count = 2);
    Assert(not A.HasItem(1000));
    Assert(A.FindFirst(I, V));
    Assert((I = 100) or (I = -50000));
    J := I;
    Assert(A.FindNext(I, V));
    Assert(((I = 100) or (I = -50000)) and (I <> J));
    Assert(not A.FindNext(I, V));
    A.Clear;
    Assert(A.Count = 0);
    Assert(A.IsEmpty);
    Assert(not A.FindFirst(I, V));

    A[0] := B[0];
    A[-10] := B[1];
    A[20] := B[2];
    Assert(A.Count = 3);
    Assert((A[0] = B[0]) and (A[-10] = B[1]) and (A[20] = B[2]));
    D := A.Duplicate as TSparseObjectArray;
    Assert(D.Count = 3);
    Assert((D[0] = B[0]) and (D[-10] = B[1]) and (D[20] = B[2]));
    Assert(A.IsEqual(D));
    Assert(D.IsEqual(A));
    D[0] := B[1];
    Assert(not A.IsEqual(D));
    Assert(not D.IsEqual(A));
    D[1] := B[1];
    Assert(not A.IsEqual(D));
    Assert(D.Count = 4);
    Assert((D[0] = B[1]) and (D[1] = B[1]));
    Assert(A.Count = 3);
    Assert((A[0] = B[0]) and (A[-10] = B[1]) and (A[20] = B[2]));
    Assert(not A.HasItem(1));
    D.Delete(1);
    Assert(D.Count = 3);
    Assert(not D.HasItem(1));
    D[0] := B[0];
    Assert(D.IsEqual(A));
    D.Free;
    Assert((A[0] = B[0]) and (A[-10] = B[1]) and (A[20] = B[2]));
  finally
    A.Free;
    B[2].Free;
    B[1].Free;
    B[0].Free;
  end;

  S := TSparseStringArray.Create;
  T := TSparseStringArray.Create;
  try
    Assert(S.IsEmpty);
    Assert(S.Count = 0);
    Assert(S.IsEqual(T));
    For I := 1 to 1000 do
      begin
        S[I * 3] := IntToStr(I);
        T[I] := IntToStr(I);
        Assert(S.HasItem(I * 3));
        Assert(not S.HasItem(I * 3 + 1));
      end;
    Assert(S.Count = 1000);
    Assert(T.Count = 1000);
    For I := 1 to 1000 do
      begin
        Assert(S[I * 3] = IntToStr(I));
        Assert(T[I] = IntToStr(I));
      end;
    For I := 1 to 1000 do
      begin
        S[I * 3] := IntToStr(I + 1);
        S[I * 3 - 1] := IntToStr(I);
        T[1000 + I * 2] := IntToStr(I);
      end;
    Assert(S.Count = 2000);
    Assert(T.Count = 2000);
    For I := 1 to 1000 do
      begin
        Assert(S[I * 3] = IntToStr(I + 1));
        Assert(S[I * 3 - 1] = IntToStr(I));
        Assert(T[I] = IntToStr(I));
        Assert(T[1000 + I * 2] = IntToStr(I));
      end;
    Assert(not S.IsEqual(T));
    S.Clear;
    Assert(S.Count = 0);
  finally
    FreeAndNil(T);
    FreeAndNil(S);
  end;
end;



end.

