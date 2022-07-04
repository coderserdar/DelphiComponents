{$INCLUDE ..\cDefines.inc}
unit cLinkedLists;

{                                                                              }
{                     Data structures: Linked lists v3.03                      }
{                                                                              }
{             This unit is copyright © 2000-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                  Its original file name is cLinkedLists.pas                  }
{                      It was generated 1 Aug 2004 23:30.                      }
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
{ Revision history:                                                            }
{   [ cUtils ]                                                                 }
{   2000/06/10  1.01  Added linked lists.                                      }
{   [ cLinkedLists ]                                                           }
{   2002/05/31  3.02  Created cLinkedLists from cUtils.                        }
{   2002/11/02  3.03  Revision.                                                }
{                                                                              }
interface

const
  UnitName      = 'cLinkedLists';
  UnitVersion   = '3.03';
  UnitDesc      = 'Data structures: Linked Lists';
  UnitCopyright = 'Copyright (c) 2000-2004 David J Butler';



{                                                                              }
{ Linked lists                                                                 }
{                                                                              }
type
  TDoublyLinkedItem = class
  protected
    FNext : TDoublyLinkedItem;
    FPrev : TDoublyLinkedItem;

  public
    destructor DestroyList;

    property  Next: TDoublyLinkedItem read FNext write FNext;
    property  Prev: TDoublyLinkedItem read FPrev write FPrev;

    function  HasNext: Boolean;
    function  HasPrev: Boolean;
    function  Last: TDoublyLinkedItem;
    function  First: TDoublyLinkedItem;
    function  Count: Integer;

    procedure Remove;
    function  RemoveNext: TDoublyLinkedItem;
    procedure DeleteNext;
    function  RemovePrev: TDoublyLinkedItem;
    procedure DeletePrev;
    procedure InsertAfter(const Item: TDoublyLinkedItem);
    procedure InsertBefore(const Item: TDoublyLinkedItem);
    procedure Delete;
  end;

  TDoublyLinkedInteger = class(TDoublyLinkedItem)
  public
    Value : Integer;

    constructor Create(const V: Integer);

    procedure InsertAfter(const V: Integer); overload;
    procedure InsertBefore(const V: Integer); overload;
    procedure InsertFirst(const V: Integer);
    procedure Append(const V: Integer);
    function  FindNext(const Find: Integer): TDoublyLinkedInteger;
    function  FindPrev(const Find: Integer): TDoublyLinkedInteger;
  end;

  TDoublyLinkedExtended = class(TDoublyLinkedItem)
  public
    Value : Extended;

    constructor Create(const V: Extended);

    procedure InsertAfter(const V: Extended); overload;
    procedure InsertBefore(const V: Extended); overload;
    procedure InsertFirst(const V: Extended);
    procedure Append(const V: Extended);
    function  FindNext(const Find: Extended): TDoublyLinkedExtended;
    function  FindPrev(const Find: Extended): TDoublyLinkedExtended;
  end;

  TDoublyLinkedString = class(TDoublyLinkedItem)
  public
    Value : String;

    constructor Create(const V: String);

    procedure InsertAfter(const V: String); overload;
    procedure InsertBefore(const V: String); overload;
    procedure InsertFirst(const V: String);
    procedure Append(const V: String);
    function  FindNext(const Find: String): TDoublyLinkedString;
    function  FindPrev(const Find: String): TDoublyLinkedString;
  end;

  TDoublyLinkedObject = class(TDoublyLinkedItem)
  public
    Value : TObject;

    constructor Create(const V: TObject);

    procedure InsertAfter(const V: TObject); overload;
    procedure InsertBefore(const V: TObject); overload;
    procedure InsertFirst(const V: TObject);
    procedure Append(const V: TObject);
    function  FindNext(const Find: TObject): TDoublyLinkedObject;
    function  FindPrev(const Find: TObject): TDoublyLinkedObject;
  end;


function  AsDoublyLinkedIntegerList(const V: Array of Integer): TDoublyLinkedInteger;
function  AsDoublyLinkedExtendedList(const V: Array of Extended): TDoublyLinkedExtended;
function  AsDoublyLinkedStringList(const V: Array of String): TDoublyLinkedString;



{                                                                              }
{ TDoublyLinkedList                                                            }
{                                                                              }
type
  TDoublyLinkedList = class
  protected
    FFirst : TDoublyLinkedItem;
    FLast  : TDoublyLinkedItem;
    FCount : Integer;

  public
    destructor Destroy; override;

    property  First: TDoublyLinkedItem read FFirst;
    property  Last: TDoublyLinkedItem read FLast;
    function  IsEmpty: Boolean;
    property  Count: Integer read FCount;

    procedure Remove(const Item: TDoublyLinkedItem);
    function  RemoveFirst: TDoublyLinkedItem;
    function  RemoveLast: TDoublyLinkedItem;

    procedure Delete(const Item: TDoublyLinkedItem);
    procedure DeleteFirst;
    procedure DeleteLast;
    procedure DeleteList;

    procedure Append(const Item: TDoublyLinkedItem);
    procedure InsertFront(const Item: TDoublyLinkedItem);
  end;



implementation



{                                                                              }
{ TDoublyLinkedItem                                                            }
{                                                                              }
function TDoublyLinkedItem.HasNext: Boolean;
begin
  Result := Assigned(Next);
end;

function TDoublyLinkedItem.Last: TDoublyLinkedItem;
var P : TDoublyLinkedItem;
begin
  P := self;
  Repeat
    Result := P;
    P := P.Next;
  Until not Assigned(P);
end;

function TDoublyLinkedItem.Count: Integer;
var N : TDoublyLinkedItem;
begin
  Result := 1;
  N := FNext;
  While Assigned(N) do
    begin
      Inc(Result);
      N := N.Next;
    end;
end;

function TDoublyLinkedItem.HasPrev: Boolean;
begin
  Result := Assigned(FPrev);
end;

function TDoublyLinkedItem.First: TDoublyLinkedItem;
var P : TDoublyLinkedItem;
begin
  P := self;
  Repeat
    Result := P;
    P := P.Prev;
  Until not Assigned(P);
end;

procedure TDoublyLinkedItem.Delete;
begin
  Remove;
  Free;
end;

procedure TDoublyLinkedItem.Remove;
begin
  if Assigned(Next) then
    Next.Prev := FPrev;
  if Assigned(Prev) then
    Prev.Next := FNext;
end;

function TDoublyLinkedItem.RemoveNext: TDoublyLinkedItem;
begin
  Result := FNext;
  if Assigned(Result) then
    begin
      FNext := Result.Next;
      if Assigned(FNext) then
        FNext.Prev := self;
    end;
end;

procedure TDoublyLinkedItem.DeleteNext;
begin
  RemoveNext.Free;
end;

function TDoublyLinkedItem.RemovePrev: TDoublyLinkedItem;
begin
  Result := FPrev;
  if Assigned(Result) then
    begin
      FPrev := Result.Prev;
      if Assigned(FPrev) then
        FPrev.Next := self;
    end;
end;

procedure TDoublyLinkedItem.DeletePrev;
begin
  RemovePrev.Free;
end;

procedure TDoublyLinkedItem.InsertAfter(const Item: TDoublyLinkedItem);
begin
  Assert(Assigned(Item));
  Item.Next := FNext;
  Item.Prev := self;
  FNext := Item;
end;

procedure TDoublyLinkedItem.InsertBefore(const Item: TDoublyLinkedItem);
begin
  Assert(Assigned(Item));
  Item.Next := self;
  Item.Prev := FPrev;
  FPrev := Item;
end;

destructor TDoublyLinkedItem.DestroyList;
var N : TDoublyLinkedItem;
begin
  While Assigned(FNext) do
    begin
      N := FNext;
      FNext := N.Next;
      N.Free;
    end;
  inherited Destroy;
end;



{                                                                              }
{ TDoublyLinkedInteger                                                         }
{                                                                              }
constructor TDoublyLinkedInteger.Create(const V: Integer);
begin
  inherited Create;
  Value := V;
end;

procedure TDoublyLinkedInteger.InsertAfter(const V: Integer);
begin
  inherited InsertAfter(TDoublyLinkedInteger.Create(V));
end;

procedure TDoublyLinkedInteger.InsertBefore(const V: Integer);
begin
  inherited InsertBefore(TDoublyLinkedInteger.Create(V));
end;

procedure TDoublyLinkedInteger.InsertFirst(const V: Integer);
begin
  TDoublyLinkedInteger(First).InsertBefore(V);
end;

procedure TDoublyLinkedInteger.Append(const V: Integer);
begin
  TDoublyLinkedInteger(Last).InsertAfter(V);
end;

function TDoublyLinkedInteger.FindNext(const Find: Integer): TDoublyLinkedInteger;
begin
  Result := self;
  Repeat
    if Result.Value = Find then
      exit;
    Result := TDoublyLinkedInteger(Result.Next);
  Until not Assigned(Result);
end;

function TDoublyLinkedInteger.FindPrev(const Find: Integer): TDoublyLinkedInteger;
begin
  Result := self;
  Repeat
    if Result.Value = Find then
      exit;
    Result := TDoublyLinkedInteger(Result.Prev);
  Until not Assigned(Result);
end;



{                                                                              }
{ TDoublyLinkedExtended                                                        }
{                                                                              }
constructor TDoublyLinkedExtended.Create(const V: Extended);
begin
  inherited Create;
  Value := V;
end;

procedure TDoublyLinkedExtended.InsertAfter(const V: Extended);
begin
  inherited InsertAfter(TDoublyLinkedExtended.Create(V));
end;

procedure TDoublyLinkedExtended.InsertBefore(const V: Extended);
begin
  inherited InsertBefore(TDoublyLinkedExtended.Create(V));
end;

procedure TDoublyLinkedExtended.InsertFirst(const V: Extended);
begin
  TDoublyLinkedExtended(First).InsertBefore(V);
end;

procedure TDoublyLinkedExtended.Append(const V: Extended);
begin
  TDoublyLinkedExtended(Last).InsertAfter(V);
end;

function TDoublyLinkedExtended.FindNext(const Find: Extended): TDoublyLinkedExtended;
begin
  Result := self;
  Repeat
    if Result.Value = Find then
      exit;
    Result := TDoublyLinkedExtended(Result.Next);
  Until not Assigned(Result);
end;

function TDoublyLinkedExtended.FindPrev(const Find: Extended): TDoublyLinkedExtended;
begin
  Result := self;
  Repeat
    if Result.Value = Find then
      exit;
    Result := TDoublyLinkedExtended(Result.Prev);
  Until not Assigned(Result);
end;



{                                                                              }
{ TDoublyLinkedString                                                          }
{                                                                              }
constructor TDoublyLinkedString.Create(const V: String);
begin
  inherited Create;
  Value := V;
end;

procedure TDoublyLinkedString.InsertAfter(const V: String);
begin
  inherited InsertAfter(TDoublyLinkedString.Create(V));
end;

procedure TDoublyLinkedString.InsertBefore(const V: String);
begin
  inherited InsertBefore(TDoublyLinkedString.Create(V));
end;

procedure TDoublyLinkedString.InsertFirst(const V: String);
begin
  TDoublyLinkedString(First).InsertBefore(V);
end;

procedure TDoublyLinkedString.Append(const V: String);
begin
  TDoublyLinkedString(Last).InsertAfter(V);
end;

function TDoublyLinkedString.FindNext(const Find: String): TDoublyLinkedString;
begin
  Result := self;
  Repeat
    if Result.Value = Find then
      exit;
    Result := TDoublyLinkedString(Result.Next);
  Until not Assigned(Result);
end;

function TDoublyLinkedString.FindPrev(const Find: String): TDoublyLinkedString;
begin
  Result := self;
  Repeat
    if Result.Value = Find then
      exit;
    Result := TDoublyLinkedString(Result.Prev);
  Until not Assigned(Result);
end;



{                                                                              }
{ TDoublyLinkedObject                                                          }
{                                                                              }
constructor TDoublyLinkedObject.Create(const V: TObject);
begin
  inherited Create;
  Value := V;
end;

procedure TDoublyLinkedObject.InsertAfter(const V: TObject);
begin
  inherited InsertAfter(TDoublyLinkedObject.Create(V));
end;

procedure TDoublyLinkedObject.InsertBefore(const V: TObject);
begin
  inherited InsertBefore(TDoublyLinkedObject.Create(V));
end;

procedure TDoublyLinkedObject.InsertFirst(const V: TObject);
begin
  TDoublyLinkedObject(First).InsertBefore(V);
end;

procedure TDoublyLinkedObject.Append(const V: TObject);
begin
  TDoublyLinkedObject(Last).InsertAfter(V);
end;

function TDoublyLinkedObject.FindNext(const Find: TObject): TDoublyLinkedObject;
begin
  Result := self;
  Repeat
    if Result.Value = Find then
      exit;
    Result := TDoublyLinkedObject(Result.Next);
  Until not Assigned(Result);
end;

function TDoublyLinkedObject.FindPrev(const Find: TObject): TDoublyLinkedObject;
begin
  Result := self;
  Repeat
    if Result.Value = Find then
      exit;
    Result := TDoublyLinkedObject(Result.Prev);
  Until not Assigned(Result);
end;



{                                                                              }
{ Open array to Linked list                                                    }
{                                                                              }
function AsDoublyLinkedIntegerList(const V: Array of Integer): TDoublyLinkedInteger;
var I, L : TDoublyLinkedInteger;
    F   : Integer;
begin
  Result := nil;
  L := nil;
  For F := 0 to High(V) do
    begin
      I := TDoublyLinkedInteger.Create(V [F]);
      if not Assigned(L) then
        begin
          L := I;
          Result := I;
        end else
        begin
          L.InsertAfter(I);
          L := I;
        end;
    end;
end;

function AsDoublyLinkedExtendedList(const V: Array of Extended): TDoublyLinkedExtended;
var I, L : TDoublyLinkedExtended;
    F   : Integer;
begin
  Result := nil;
  L := nil;
  For F := 0 to High(V) do
    begin
      I := TDoublyLinkedExtended.Create(V [F]);
      if not Assigned(L) then
        begin
          L := I;
          Result := I;
        end else
        begin
          L.InsertAfter(I);
          L := I;
        end;
    end;
end;

function AsDoublyLinkedStringList(const V: Array of String): TDoublyLinkedString;
var I, L : TDoublyLinkedString;
    F   : Integer;
begin
  Result := nil;
  L := nil;
  For F := 0 to High(V) do
    begin
      I := TDoublyLinkedString.Create(V [F]);
      if not Assigned(L) then
        begin
          L := I;
          Result := I;
        end else
        begin
          L.InsertAfter(I);
          L := I;
        end;
    end;
end;



{                                                                              }
{ TDoublyLinkedList                                                            }
{                                                                              }
Destructor TDoublyLinkedList.Destroy;
begin
  DeleteList;
  inherited Destroy;
end;

function TDoublyLinkedList.IsEmpty: Boolean;
begin
  Result := not Assigned(FFirst);
end;

procedure TDoublyLinkedList.Append(const Item: TDoublyLinkedItem);
begin
  if not Assigned(Item) then
    exit;
  if not Assigned(FLast) then
    begin
      FFirst := Item;
      FLast := Item;
      Item.Prev := nil;
      Item.Next := nil;
    end else
    begin
      FLast.InsertAfter(Item);
      FLast := Item;
    end;
  Inc(FCount);
end;

procedure TDoublyLinkedList.InsertFront(const Item: TDoublyLinkedItem);
begin
  if not Assigned(Item) then
    exit;
  if not Assigned(FFirst) then
    begin
      FFirst := Item;
      FLast := Item;
      Item.Prev := nil;
      Item.Next := nil;
    end else
    begin
      FFirst.InsertBefore(Item);
      FFirst := Item;
    end;
  Inc(FCount);
end;

procedure TDoublyLinkedList.Remove(const Item: TDoublyLinkedItem);
begin
  if not Assigned(Item) then
    exit;
  if FFirst = Item then
    FFirst := Item.Next;
  if FLast = Item then
    FLast := Item.Prev;
  Item.Remove;
  Dec(FCount);
end;

function TDoublyLinkedList.RemoveFirst: TDoublyLinkedItem;
var N : TDoublyLinkedItem;
begin
  Result := FFirst;
  if not Assigned(Result) then
    exit;
  if Result = FLast then
    begin
      FFirst := nil;
      FLast := nil;
    end else
    begin
      N := Result.Next;
      Result.Remove;
      FFirst := N;
    end;
  Dec(FCount);
end;

function TDoublyLinkedList.RemoveLast: TDoublyLinkedItem;
var P : TDoublyLinkedItem;
begin
  Result := FLast;
  if not Assigned(Result) then
    exit;
  if Result = FFirst then
    begin
      FFirst := nil;
      FLast := nil;
    end
  else
    begin
      P := Result.Prev;
      Result.Remove;
      FLast := P;
    end;
  Dec(FCount);
end;

procedure TDoublyLinkedList.Delete(const Item: TDoublyLinkedItem);
begin
  Remove(Item);
  Item.Free;
end;

procedure TDoublyLinkedList.DeleteFirst;
begin
  RemoveFirst.Free;
end;

procedure TDoublyLinkedList.DeleteLast;
begin
  RemoveLast.Free;
end;

procedure TDoublyLinkedList.DeleteList;
var F : TDoublyLinkedItem;
begin
  F := FFirst;
  FFirst := nil;
  FLast := nil;
  if Assigned(F) then
    F.DestroyList;
end;



end.

