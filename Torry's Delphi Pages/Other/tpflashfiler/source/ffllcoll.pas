{*********************************************************}
{* FlashFiler: Collection class                          *}
{*********************************************************}

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
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit ffllcoll;

interface

uses
  Classes,
  ffllbase;

type
  TffCollection = class; {forward declaration}

  TffCollectionItem = class    {class of item appearing in collection}
    protected {private}
      ciContainer : TffCollection;
      ciParent    : TObject;
    protected
      function ciGetIdentifier : integer;
    public
      constructor Create(aParent : TObject; aContainer : TffCollection);
      destructor Destroy; override;

      property Container : TffCollection read ciContainer;
      property Identifier : integer read ciGetIdentifier;
      property Parent : TObject read ciParent;
  end;

  {note: rewritted to use TList                                       !!.06}
  TffCollection = class
    protected {private}
      FItems : TList;
    protected
      function tcGet(aIndex : integer) : TffCollectionItem;
      function tcGetCapacity : integer;
      function tcGetCount : integer;
      procedure tcPut(aIndex : integer; aItem : TffCollectionItem);
      procedure tcSetCapacity(aNewCapacity : integer);
    public
      constructor Create;
      destructor Destroy; override;

      procedure InsertAt(aIndex: integer; aItem : TffCollectionItem);
      procedure Delete(aItem : TffCollectionItem);
      procedure FreeAll;
      function IndexOf(aItem : TffCollectionItem) : integer; virtual;
      function Insert(aItem : TffCollectionItem) : boolean; virtual;

      property Count: integer read tcGetCount;
      property Items[aIndex : integer] : TffCollectionItem
         read tcGet write tcPut; default;
  end;

  TffSortedCollection = class(TffCollection)
    private
      tscAllowDups : boolean;
    public
      constructor Create(aAllowDups : boolean);

      function KeyOf(aItem : TffCollectionItem) : pointer; virtual;
      function Compare(aKey1, aKey2 : Pointer) : integer; virtual; abstract;
      function Insert(aItem : TffCollectionItem) : boolean; override;
      function Search(aKey : Pointer; var aIndex : integer): boolean; virtual;

      property AllowDups : boolean read tscAllowDups;
  end;

implementation

uses
  SysUtils,
  ffconst,
  ffclbase;

{===TffCollectionItem================================================}
constructor TffCollectionItem.Create(aParent    : TObject;
                                     aContainer : TffCollection);
begin
  inherited Create;
  ciParent := aParent;
  ciContainer := aContainer;
  if (aContainer <> nil) then
    if not aContainer.Insert(Self) then
      raise Exception.Create(ffStrResClient[ffccDupItemInColl]);
end;
{--------}
destructor TffCollectionItem.Destroy;
begin
  if (ciContainer <> nil) then
    ciContainer.Delete(Self);
  inherited Destroy;
end;
{--------}
function TffCollectionItem.ciGetIdentifier : integer;
begin
  if (ciContainer <> nil) then
    Result := ciContainer.IndexOf(Self)
  else
    Result := 0;
end;
{====================================================================}


{===TffCollection====================================================}
constructor TffCollection.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;
{--------}
destructor TffCollection.Destroy;
begin
  if (FItems <> nil) then begin
    FreeAll;
    FItems.Free;
  end;
  inherited Destroy;
end;
{--------}
procedure TffCollection.InsertAt(aIndex : integer; aItem : TffCollectionItem);
begin
  FItems.Insert(aIndex, aItem);
end;
{--------}
procedure TffCollection.Delete(aItem : TffCollectionItem);
var
  Inx : integer;
begin
  Inx := FItems.IndexOf(aItem);
  if (Inx <> -1) then
    FItems.Delete(Inx);
end;
{--------}
procedure TffCollection.FreeAll;
var
  Inx : integer;
begin
  {note: the downto is required because the base item class will
         delete itself from the collection when freed}
  for Inx := pred(FItems.Count) downto 0 do
    TObject(FItems[Inx]).Free;
  FItems.Clear;
end;
{--------}
function TffCollection.IndexOf(aItem : TffCollectionItem) : integer;
begin
  Result := FItems.IndexOf(aItem);
end;
{--------}
function TffCollection.Insert(aItem : TffCollectionItem) : boolean;
begin
  FItems.Add(aItem);
  Result := true;
end;
{--------}
function TffCollection.tcGet(aIndex : integer) : TffCollectionItem;
begin
  Result := TffCollectionItem(FItems[aIndex]);
end;
{--------}
function TffCollection.tcGetCount: integer;
begin
  Result := FItems.Count;
end;
{--------}
function TffCollection.tcGetCapacity : integer;
begin
  Result := FItems.Capacity;
end;
{--------}
procedure TffCollection.tcPut(aIndex : integer; aItem : TffCollectionItem);
begin
  FItems[aIndex] := aItem;
end;
{--------}
procedure TffCollection.tcSetCapacity(aNewCapacity : integer);
begin
  FItems.Capacity := aNewCapacity;
end;
{====================================================================}


{===TffSortedCollection==============================================}
constructor TffSortedCollection.Create(aAllowDups : boolean);
begin
  inherited Create;
  tscAllowDups := aAllowDups;
end;
{--------}
function TffSortedCollection.Insert(aItem : TffCollectionItem) : boolean;
var
  Inx : integer;
begin
  if (not Search(KeyOf(aItem), Inx)) or AllowDups then begin
    InsertAt(Inx, aItem);
    Result := true;
  end else
    Result := false;
end;
{--------}
function TffSortedCollection.KeyOf(aItem : TffCollectionItem) : pointer;
begin
  Result := aItem;
end;
{--------}
function TffSortedCollection.Search(aKey : pointer; var aIndex : integer) : boolean;
var
  L, R, M : integer;
  CmpRes  : integer;
begin
  Result := false;
  L := 0;
  R := pred(Count);
  while (L <= R) do begin
    M := (L + R) div 2;
    CmpRes := Compare(KeyOf(FItems[M]), aKey);
    if (CmpRes < 0) then
      L := succ(M)
    else if (CmpRes > 0) then
      R := pred(M)
    else {CmpRes = 0} begin
      Result := true;
      if not AllowDups then begin
        aIndex := M;
        Exit;
      end;
      R := pred(M); {need to find the first dup item}
    end;
  end;
  aIndex := L;
end;
{====================================================================}


end.

