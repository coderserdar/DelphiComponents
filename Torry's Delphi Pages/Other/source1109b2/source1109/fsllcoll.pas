{$I fsdefine.inc}

Unit fsllcoll;

Interface

Uses
  Classes,
  fsllbase;

Type
  TffCollection = Class; {forward declaration}

  TffCollectionItem = Class {class of item appearing in collection}
  Protected {private}
    ciContainer: TffCollection;
    ciParent: TObject;
  Protected
    Function ciGetIdentifier: Integer;
  Public
    Constructor Create(aParent: TObject; aContainer: TffCollection);
    Destructor Destroy; Override;

    Property Container: TffCollection Read ciContainer;
    Property Identifier: Integer Read ciGetIdentifier;
    Property Parent: TObject Read ciParent;
  End;

  {note: rewritted to use TList                                       !!.06}
  TffCollection = Class
  Protected {private}
    FItems: TList;
  Protected
    Function tcGet(aIndex: Integer): TffCollectionItem;
    Function tcGetCapacity: Integer;
    Function tcGetCount: Integer;
    Procedure tcPut(aIndex: Integer; aItem: TffCollectionItem);
    Procedure tcSetCapacity(aNewCapacity: Integer);
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure InsertAt(aIndex: Integer; aItem: TffCollectionItem);
    Procedure Delete(aItem: TffCollectionItem);
    Procedure FreeAll;
    Function IndexOf(aItem: TffCollectionItem): Integer; Virtual;
    Function Insert(aItem: TffCollectionItem): boolean; Virtual;

    Property Count: Integer Read tcGetCount;
    Property Items[aIndex: Integer]: TffCollectionItem
    Read tcGet Write tcPut; Default;
  End;

  TffSortedCollection = Class(TffCollection)
  Private
    tscAllowDups: boolean;
  Public
    Constructor Create(aAllowDups: boolean);

    Function KeyOf(aItem: TffCollectionItem): pointer; Virtual;
    Function Compare(aKey1, aKey2: Pointer): Integer; Virtual; Abstract;
    Function Insert(aItem: TffCollectionItem): boolean; Override;
    Function Search(aKey: Pointer; Var aIndex: Integer): boolean; Virtual;

    Property AllowDups: boolean Read tscAllowDups;
  End;

Implementation

Uses
  SysUtils,
  fsconst,
  fsclbase;

{===TffCollectionItem================================================}

Constructor TffCollectionItem.Create(aParent: TObject;
  aContainer: TffCollection);
Begin
  Inherited Create;
  ciParent := aParent;
  ciContainer := aContainer;
  If (aContainer <> Nil) Then
    If Not aContainer.Insert(Self) Then
      Raise Exception.Create(fsStrResClient[fsccDupItemInColl]);
End;
{--------}

Destructor TffCollectionItem.Destroy;
Begin
  If (ciContainer <> Nil) Then
    ciContainer.Delete(Self);
  Inherited Destroy;
End;
{--------}

Function TffCollectionItem.ciGetIdentifier: Integer;
Begin
  If (ciContainer <> Nil) Then
    Result := ciContainer.IndexOf(Self)
  Else
    Result := 0;
End;
{====================================================================}

{===TffCollection====================================================}

Constructor TffCollection.Create;
Begin
  Inherited Create;
  FItems := TList.Create;
End;
{--------}

Destructor TffCollection.Destroy;
Begin
  If (FItems <> Nil) Then
    Begin
      FreeAll;
      FItems.Free;
    End;
  Inherited Destroy;
End;
{--------}

Procedure TffCollection.InsertAt(aIndex: Integer; aItem: TffCollectionItem);
Begin
  FItems.Insert(aIndex, aItem);
End;
{--------}

Procedure TffCollection.Delete(aItem: TffCollectionItem);
Var
  Inx: Integer;
Begin
  Inx := FItems.IndexOf(aItem);
  If (Inx <> -1) Then
    FItems.Delete(Inx);
End;
{--------}

Procedure TffCollection.FreeAll;
Var
  Inx: Integer;
Begin
  {note: the downto is required because the base item class will
         delete itself from the collection when freed}
  For Inx := pred(FItems.Count) Downto 0 Do
    TObject(FItems[Inx]).Free;
  FItems.Clear;
End;
{--------}

Function TffCollection.IndexOf(aItem: TffCollectionItem): Integer;
Begin
  Result := FItems.IndexOf(aItem);
End;
{--------}

Function TffCollection.Insert(aItem: TffCollectionItem): boolean;
Begin
  FItems.Add(aItem);
  Result := True;
End;
{--------}

Function TffCollection.tcGet(aIndex: Integer): TffCollectionItem;
Begin
  Result := TffCollectionItem(FItems[aIndex]);
End;
{--------}

Function TffCollection.tcGetCount: Integer;
Begin
  Result := FItems.Count;
End;
{--------}

Function TffCollection.tcGetCapacity: Integer;
Begin
  Result := FItems.Capacity;
End;
{--------}

Procedure TffCollection.tcPut(aIndex: Integer; aItem: TffCollectionItem);
Begin
  FItems[aIndex] := aItem;
End;
{--------}

Procedure TffCollection.tcSetCapacity(aNewCapacity: Integer);
Begin
  FItems.Capacity := aNewCapacity;
End;
{====================================================================}

{===TffSortedCollection==============================================}

Constructor TffSortedCollection.Create(aAllowDups: boolean);
Begin
  Inherited Create;
  tscAllowDups := aAllowDups;
End;
{--------}

Function TffSortedCollection.Insert(aItem: TffCollectionItem): boolean;
Var
  Inx: Integer;
Begin
  If (Not Search(KeyOf(aItem), Inx)) Or AllowDups Then
    Begin
      InsertAt(Inx, aItem);
      Result := True;
    End
  Else
    Result := False;
End;
{--------}

Function TffSortedCollection.KeyOf(aItem: TffCollectionItem): pointer;
Begin
  Result := aItem;
End;
{--------}

Function TffSortedCollection.Search(aKey: pointer; Var aIndex: Integer): boolean;
Var
  L, R, M: Integer;
  CmpRes: Integer;
Begin
  Result := False;
  L := 0;
  R := pred(Count);
  While (L <= R) Do
    Begin
      M := (L + R) Div 2;
      CmpRes := Compare(KeyOf(FItems[M]), aKey);
      If (CmpRes < 0) Then
        L := succ(M)
      Else If (CmpRes > 0) Then
        R := pred(M)
      Else {CmpRes = 0}
        Begin
          Result := True;
          If Not AllowDups Then
            Begin
              aIndex := M;
              Exit;
            End;
          R := pred(M); {need to find the first dup item}
        End;
    End;
  aIndex := L;
End;
{====================================================================}

End.

