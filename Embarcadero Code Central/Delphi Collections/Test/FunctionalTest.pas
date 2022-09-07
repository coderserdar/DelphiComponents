unit FunctionalTest;

(*****************************************************************************
 * Copyright 2003 by Matthew Greet
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details. (http://opensource.org/licenses/lgpl-license.php)
 * 
 * See http://www.warmachine.u-net.com/delphi_collections for updates and downloads.
 *
 * $Version: v1.0 $
 * $Revision: 1.0.1.2 $
 * $Log: D:\QVCS Repositories\Delphi Collections\Test\FunctionalTest.qbt $
 * 
 *   Initial version.
 * 
 * Revision 1.0.1.2  by: Matthew Greet  Rev date: 14/03/05 23:31:38
 *   Test for lists check unsorted and sorted version.
 * 
 * Revision 1.0.1.1  by: Matthew Greet  Rev date: 24/10/03 15:26:42
 *   v1.0 branch.
 * 
 * Revision 1.0  by: Matthew Greet  Rev date: 06/04/03 11:22:24
 *   Initial revision.
 * 
 * $Endlog$
 *****************************************************************************)

interface

uses
    Classes, SysUtils,
    Collections, CollWrappers, CollArray, CollPArray, CollHash, CollList, CollLibrary;

type
    TFunctionalTestResult = class
    private
        FName: String;
        FSuccess: Boolean;
    public
        constructor Create(Name: String);
        property Name: String read FName;
        property Success: Boolean read FSuccess write FSuccess;
    end;

    TFunctionalTest = class
    private
        FNonNaturalObject1: ICollectable;
        FNonNaturalObject2: ICollectable;
        FNonNaturalObject3: ICollectable;
        FNonNaturalObject4: ICollectable;
        FEquatableObject1: ICollectable;
        FEquatableObject2: ICollectable;
        FEquatableObject3: ICollectable;
        FEquatableObject4: ICollectable;
        FComparableObject1: ICollectable;
        FComparableObject2: ICollectable;
        FComparableObject3: ICollectable;
        FComparableObject4: ICollectable;
        FHashableObject1: ICollectable;
        FHashableObject2: ICollectable;
        FHashableObject3: ICollectable;
        FHashableObject4: ICollectable;
        FGeneralObject1: ICollectable;
        FGeneralObject2: ICollectable;
        FGeneralObject3: ICollectable;
        FGeneralObject4: ICollectable;
        FFNonNaturalKeyMapObject1: ICollectable;
        FFNonNaturalKeyMapObject2: ICollectable;
        FFNonNaturalKeyMapObject3: ICollectable;
        FFNonNaturalKeyMapObject4: ICollectable;
        FNaturalKeyMapObject1: ICollectable;
        FNaturalKeyMapObject2: ICollectable;
        FNaturalKeyMapObject3: ICollectable;
        FNaturalKeyMapObject4: ICollectable;
        FIntegerMapObject1: ICollectable;
        FIntegerMapObject2: ICollectable;
        FIntegerMapObject3: ICollectable;
        FIntegerMapObject4: ICollectable;
        FStringMapObject1: ICollectable;
        FStringMapObject2: ICollectable;
        FStringMapObject3: ICollectable;
        FStringMapObject4: ICollectable;

        FEmptyArray: TCollectableArray;
        FEmptyFIntegerArray: array of Integer;
        FEmptyFStringArray: array of String;
        FNonNaturalArray12: TCollectableArray;
        FNonNaturalArray34: TCollectableArray;
        FNonNaturalArray123: TCollectableArray;
        FNonNaturalArray1234: TCollectableArray;
        FEquatableArray12: TCollectableArray;
        FComparableArray12: TCollectableArray;
        FHashableArray12: TCollectableArray;
        FGeneralArray12: TCollectableArray;
        FGeneralArray23: TCollectableArray;
        FGeneralArray34: TCollectableArray;
        FGeneralArray123: TCollectableArray;
        FGeneralArray1234: TCollectableArray;
        FNaturalMapItemArray12: TCollectableArray;
        FNaturalMapItemArray23: TCollectableArray;
        FNaturalMapItemArray123: TCollectableArray;
        FNaturalMapItemArray1234: TCollectableArray;
        FIntegerArray12: array of Integer;
        FIntegerArray23: array of Integer;
        FIntegerArray34: array of Integer;
        FIntegerArray123: array of Integer;
        FIntegerArray1234: array of Integer;
        FIntegerMapItemArray12: TCollectableArray;
        FIntegerMapItemArray23: TCollectableArray;
        FIntegerMapItemArray123: TCollectableArray;
        FIntegerMapItemArray1234: TCollectableArray;
        FStringArray12: array of String;
        FStringArray23: array of String;
        FStringArray34: array of String;
        FStringArray123: array of String;
        FStringArray1234: array of String;
        FStringMapItemArray12: TCollectableArray;
        FStringMapItemArray23: TCollectableArray;
        FStringMapItemArray123: TCollectableArray;
        FStringMapItemArray1234: TCollectableArray;

        FEmptyCollection: ICollection;
        FEmptySet: ISet;
        FNonNaturalCollection12: ICollection;
        FNonNaturalCollection34: ICollection;
        FNonNaturalCollection123: ICollection;
        FNonNaturalCollection1234: ICollection;
        FEquatableCollection12: ICollection;
        FComparableCollection12: ICollection;
        FHashableCollection12: ICollection;
        FGeneralCollection12: ICollection;
        FGeneralCollection23: ICollection;
        FGeneralCollection34: ICollection;
        FGeneralCollection123: ICollection;
        FGeneralCollection1234: ICollection;
        FNonNaturalKeyMap12: IMap;
        FNaturalKeyMap12: IMap;
        FNaturalItemMap12: IMap;
        FNonNaturalItemIntMap12: IIntegerMap;
        FNaturalItemIntMap12: IIntegerMap;
        FNonNaturalItemStrMap12: IStringMap;
        FNaturalItemStrMap12: IStringMap;

        FCollectionClass: TAbstractCollectionClass;
        // It might seem strange that this doesn't use my own collection classes
        // but a functional test can't start with the assumption that they work.
        FResultList: TList;
        procedure DoTest(TestResult: TFunctionalTestResult);
        procedure ClearTestList;
        procedure AddTestList;
        procedure AddTestListBag;
        procedure AddTestListSet;
        procedure AddTestListList;
        procedure AddTestListMap;
        procedure AddTestListIntegerMap;
        procedure AddTestListStringMap;
        function NonNaturalFilter12(const Item: ICollectable): Boolean;
        function NonNaturalFilter34(const Item: ICollectable): Boolean;
        function NaturalFilter12(const Item: ICollectable): Boolean;
        function NaturalFilter34(const Item: ICollectable): Boolean;
    public
        constructor Create;
        destructor Destroy; override;
        procedure TestClass;
        property CollectionClass: TAbstractCollectionClass read FCollectionClass write FCollectionClass;
        property ResultList: TList read FResultList;
    published
        // Constructors tests
        function Create_Def: Boolean;
        function Create_Arr: Boolean;
        function Create_ArrEqu: Boolean;
        function Create_ArrComp: Boolean;
        function Create_ArrHash: Boolean;
        function Create_ArrMap: Boolean;
        function Create_ArrIntMap: Boolean;
        function Create_ArrStrMap: Boolean;
        function Create_IF: Boolean;
        function Create_IFEqu: Boolean;
        function Create_IFComp: Boolean;
        function Create_IFHash: Boolean;
        function Create_IFMap: Boolean;
        function Create_IFIntMap: Boolean;
        function Create_IFStrMap: Boolean;
        function Create_IMap: Boolean;
        function Create_IMapNat: Boolean;
        function Create_IIntMap: Boolean;
        function Create_IStrMap: Boolean;
        function Create_ArrArr: Boolean;
        function Create_ArrArrNat: Boolean;
        function Create_ArrArrInt: Boolean;
        function Create_ArrArrStr: Boolean;
        // ICollection function tests
        function Add_Fix: Boolean;
        function Add_Bag: Boolean;
        function Add_Set: Boolean;
        function Add_List: Boolean;
        function Add_Map: Boolean;
        function Add_IntMap: Boolean;
        function Add_StrMap: Boolean;
        function Add_Arr: Boolean;
        function Add_ArrEqu: Boolean;
        function Add_ArrComp: Boolean;
        function Add_ArrHash: Boolean;
        function Add_ArrMap: Boolean;
        function Add_ArrIntMap: Boolean;
        function Add_ArrStrMap: Boolean;
        function Add_IF: Boolean;
        function Add_IFEqu: Boolean;
        function Add_IFComp: Boolean;
        function Add_IFHash: Boolean;
        function Add_IFMap: Boolean;
        function Add_IFIntMap: Boolean;
        function Add_IFStrMap: Boolean;
        function Clear_NonFix: Boolean;
        function Clear_NonFixMap: Boolean;
        function Clear_NonFixIntMap: Boolean;
        function Clear_NonFixStrMap: Boolean;
        function Clear_Fix: Boolean;
        function Clone: Boolean;
        function Clone_Map: Boolean;
        function Clone_IntMap: Boolean;
        function Clone_StrMap: Boolean;
        function CloneAsBag: Boolean;
        function CloneAsSet: Boolean;
        function CloneAsList: Boolean;
        function CloneAsMap: Boolean;
        function CloneAsIntegerMap: Boolean;
        function CloneAsStringMap: Boolean;
        function Contains_NonNat: Boolean;
        function Contains_NonNatList: Boolean;
        function Contains_NonNatMap: Boolean;
        function Contains_NonNatIntMap: Boolean;
        function Contains_NonNatStrMap: Boolean;
        function Contains_Nat: Boolean;
        function Contains_NatList: Boolean;
        function Contains_NatMap: Boolean;
        function Contains_NatIntMap: Boolean;
        function Contains_NatStrMap: Boolean;
        function Contains_Arr: Boolean;
        function Contains_ArrList: Boolean;
        function Contains_ArrMap: Boolean;
        function Contains_ArrIntMap: Boolean;
        function Contains_ArrStrMap: Boolean;
        function Contains_IF: Boolean;
        function Contains_IFList: Boolean;
        function Contains_IFMap: Boolean;
        function Contains_IFIntMap: Boolean;
        function Contains_IFStrMap: Boolean;
        function Equals_Bag: Boolean;
        function Equals_Set: Boolean;
        function Equals_List: Boolean;
        function Equals_Map: Boolean;
        function Equals_IntMap: Boolean;
        function Equals_StrMap: Boolean;
        function Find_FilterNonNat: Boolean;
        function Find_FilterNonNatList: Boolean;
        function Find_FilterNonNatMap: Boolean;
        function Find_FilterNonNatIntMap: Boolean;
        function Find_FilterNonNatStrMap: Boolean;
        function Find_FilterNat: Boolean;
        function Find_FilterNatList: Boolean;
        function Find_FilterNatMap: Boolean;
        function Find_FilterNatIntMap: Boolean;
        function Find_FilterNatStrMap: Boolean;
        function Find_FuncNonNat: Boolean;
        function Find_FuncNonNatList: Boolean;
        function Find_FuncNonNatMap: Boolean;
        function Find_FuncNonNatIntMap: Boolean;
        function Find_FuncNonNatStrMap: Boolean;
        function FindAll_FilterNonNat: Boolean;
        function FindAll_FilterNonNatList: Boolean;
        function FindAll_FilterNonNatMap: Boolean;
        function FindAll_FilterNonNatIntMap: Boolean;
        function FindAll_FilterNonNatStrMap: Boolean;
        function FindAll_FilterNat: Boolean;
        function FindAll_FilterNatList: Boolean;
        function FindAll_FilterNatMap: Boolean;
        function FindAll_FilterNatIntMap: Boolean;
        function FindAll_FilterNatStrMap: Boolean;
        function GetAsArray: Boolean;
        function GetAsArrayList: Boolean;
        function GetAsArray_Map: Boolean;
        function GetAsArray_IntMap: Boolean;
        function GetAsArray_StrMap: Boolean;
        function GetComparator_NonNat: Boolean;
        function GetComparator_Nat: Boolean;
        function GetFixedSize_NonFix: Boolean;
        function GetFixedSize_Fix: Boolean;
        function GetIgnoreErrors: Boolean;
        function GetInstance: Boolean;
        function GetIterator: Boolean;
        function GetIterator_Map: Boolean;
        function GetIterator_IntMap: Boolean;
        function GetIterator_StrMap: Boolean;
        function GetIterator_Rem: Boolean;
        function GetIterator_RemMap: Boolean;
        function GetIterator_RemIntMap: Boolean;
        function GetIterator_RemStrMap: Boolean;
        function GetIterator_Filter: Boolean;
        function GetIterator_FilterMap: Boolean;
        function GetIterator_FilterIntMap: Boolean;
        function GetIterator_FilterStrMap: Boolean;
        function GetIterator_Func: Boolean;
        function GetIterator_FuncMap: Boolean;
        function GetIterator_FuncIntMap: Boolean;
        function GetIterator_FuncStrMap: Boolean;
        function GetNaturalItemIID_Equ: Boolean;
        function GetNaturalItemIID_Comp: Boolean;
        function GetNaturalItemIID_Hash: Boolean;
        function GetNaturalItemIID_Map: Boolean;
        function GetNaturalItemIID_IntMap: Boolean;
        function GetNaturalItemIID_StrMap: Boolean;
        function GetNaturalItemsOnly_Both: Boolean;
        function GetNaturalItemsOnly_Nat: Boolean;
        function GetSize_NonFix: Boolean;
        function GetSize_NonFixMap: Boolean;
        function GetSize_NonFixIntMap: Boolean;
        function GetSize_NonFixStrMap: Boolean;
        function GetSize_Fix: Boolean;
        function IsEmpty: Boolean;
        function IsEmpty_Map: Boolean;
        function IsEmpty_IntMap: Boolean;
        function IsEmpty_StrMap: Boolean;
        function IsNaturalItem: Boolean;
        function IsNaturalItem_Map: Boolean;
        function IsNaturalItem_IntMap: Boolean;
        function IsNaturalItem_StrMap: Boolean;
        function ItemAllowed_NonNat: Boolean;
        function ItemAllowed_Nat: Boolean;
        function ItemAllowed_NatMap: Boolean;
        function ItemAllowed_NatIntMap: Boolean;
        function ItemAllowed_NatStrMap: Boolean;
        function ItemCount_NonNatBagList: Boolean;
        function ItemCount_NonNatSet: Boolean;
        function ItemCount_NonNatMap: Boolean;
        function ItemCount_NonNatIntMap: Boolean;
        function ItemCount_NonNatStrMap: Boolean;
        function ItemCount_NatBagList: Boolean;
        function ItemCount_NatSet: Boolean;
        function ItemCount_NatMap: Boolean;
        function ItemCount_NatIntMap: Boolean;
        function ItemCount_NatStrMap: Boolean;
        function ItemCount_Arr: Boolean;
        function ItemCount_ArrMap: Boolean;
        function ItemCount_ArrIntMap: Boolean;
        function ItemCount_ArrStrMap: Boolean;
        function ItemCount_IF: Boolean;
        function ItemCount_IFMap: Boolean;
        function ItemCount_IFIntMap: Boolean;
        function ItemCount_IFStrMap: Boolean;
        function Matching_ArrNonNat: Boolean;
        function Matching_ArrNonNatList: Boolean;
        function Matching_ArrNonNatMap: Boolean;
        function Matching_ArrNonNatIntMap: Boolean;
        function Matching_ArrNonNatStrMap: Boolean;
        function Matching_ArrNat: Boolean;
        function Matching_ArrNatList: Boolean;
        function Matching_ArrNatMap: Boolean;
        function Matching_ArrNatIntMap: Boolean;
        function Matching_ArrNatStrMap: Boolean;
        function Matching_IFNonNat: Boolean;
        function Matching_IFNonNatList: Boolean;
        function Matching_IFNonNatMap: Boolean;
        function Matching_IFNonNatIntMap: Boolean;
        function Matching_IFNonNatStrMap: Boolean;
        function Matching_IFNat: Boolean;
        function Matching_IFNatList: Boolean;
        function Matching_IFNatMap: Boolean;
        function Matching_IFNatIntMap: Boolean;
        function Matching_IFNatStrMap: Boolean;
        function Remove_Fix: Boolean;
        function Remove_Dup: Boolean;
        function Remove_NoDup: Boolean;
        function Remove_List: Boolean;
        function Remove_Map: Boolean;
        function Remove_IntMap: Boolean;
        function Remove_StrMap: Boolean;
        function RemoveAll_Dup: Boolean;
        function RemoveAll_NoDup: Boolean;
        function RemoveAll_List: Boolean;
        function RemoveAll_Map: Boolean;
        function RemoveAll_IntMap: Boolean;
        function RemoveAll_StrMap: Boolean;
        function Remove_Arr: Boolean;
        function Remove_ArrList: Boolean;
        function Remove_ArrMap: Boolean;
        function Remove_ArrIntMap: Boolean;
        function Remove_ArrStrMap: Boolean;
        function Remove_IF: Boolean;
        function Remove_IFList: Boolean;
        function Remove_IFMap: Boolean;
        function Remove_IFIntMap: Boolean;
        function Remove_IFStrMap: Boolean;
        function Retain_Arr: Boolean;
        function Retain_ArrList: Boolean;
        function Retain_ArrMap: Boolean;
        function Retain_ArrIntMap: Boolean;
        function Retain_ArrStrMap: Boolean;
        function Retain_IF: Boolean;
        function Retain_IFList: Boolean;
        function Retain_IFMap: Boolean;
        function Retain_IFIntMap: Boolean;
        function Retain_IFStrMap: Boolean;
        function SetComparator: Boolean;
        function SetIgnoreErrors: Boolean;
        // ISet function tests
        function Complement: Boolean;
        function Intersect: Boolean;
        function Union: Boolean;
        // IList function tests
        function Delete_Fix: Boolean;
        function Delete_NonFix: Boolean;
        function Exchange: Boolean;
        function First: Boolean;
        function GetDuplicates: Boolean;
        function GetSorted: Boolean;
        function IndexOf: Boolean;
        function Insert_Fix: Boolean;
        function Insert_NonFix: Boolean;
        function Insert_Arr: Boolean;
        function Insert_IF: Boolean;
        function Last: Boolean;
        function SetDuplicates: Boolean;
        function SetItem: Boolean;
        function SetSorted: Boolean;
        function Sort: Boolean;
        // IMap function tests
        function ContainsKey_NonNat: Boolean;
        function ContainsKey_Nat: Boolean;
        function ContainsKey_Arr: Boolean;
        function ContainsKey_IF: Boolean;
        function Get_NonNat: Boolean;
        function Get_Nat: Boolean;
        function GetKeyComparator_NonNat: Boolean;
        function GetKeyComparator_Nat: Boolean;
        function GetKeyIterator: Boolean;
        function GetKeys: Boolean;
        function GetMapIterator: Boolean;
        function GetMapIteratorByKey_Filter: Boolean;
        function GetMapIteratorByKey_Func: Boolean;
        function GetNaturalKeyIID_Equ: Boolean;
        function GetNaturalKeyIID_Comp: Boolean;
        function GetNaturalKeyIID_Hash: Boolean;
        function GetNaturalKeysOnly_Both: Boolean;
        function GetNaturalKeysOnly_Nat: Boolean;
        function GetValues: Boolean;
        function KeyAllowed_NonNat: Boolean;
        function KeyAllowed_Nat: Boolean;
        function MatchingKey_ArrNonNat: Boolean;
        function MatchingKey_ArrNat: Boolean;
        function MatchingKey_IFNonNat: Boolean;
        function MatchingKey_IFNat: Boolean;
        function Put_Item: Boolean;
        function Put_KeyItem: Boolean;
        function Put_Arr: Boolean;
        function Put_IF: Boolean;
        function Put_IMap: Boolean;
        function RemoveKey_Key: Boolean;
        function RemoveKey_Arr: Boolean;
        function RemoveKey_IF: Boolean;
        function RetainKey_Arr: Boolean;
        function RetainKey_IF: Boolean;
        // IIntegerMap function tests
        function ContainsKey_Int: Boolean;
        function ContainsKey_ArrInt: Boolean;
        function Get_Int: Boolean;
        function GetKeys_Int: Boolean;
        function GetMapIterator_Int: Boolean;
        function GetValues_Int: Boolean;
        function Put_ItemInt: Boolean;
        function Put_KeyItemInt: Boolean;
        function Put_ArrInt: Boolean;
        function Put_IFInt: Boolean;
        function Put_IIntMap:Boolean;
        function RemoveKey_KeyInt: Boolean;
        function RemoveKey_ArrInt: Boolean;
        function RetainKey_ArrInt: Boolean;
        // IStringMap function tests
        function ContainsKey_Str: Boolean;
        function ContainsKey_ArrStr: Boolean;
        function Get_Str: Boolean;
        function GetKeys_Str: Boolean;
        function GetMapIterator_Str: Boolean;
        function GetValues_Str: Boolean;
        function Put_ItemStr: Boolean;
        function Put_KeyItemStr: Boolean;
        function Put_ArrStr: Boolean;
        function Put_IFStr: Boolean;
        function Put_IStrMap:Boolean;
        function RemoveKey_KeyStr: Boolean;
        function RemoveKey_ArrStr: Boolean;
        function RetainKey_ArrStr: Boolean;
    end;

implementation

type
    TTestMethod = function: Boolean of object;

    TNonNaturalItem = class(TAbstractItem)
    private
        FName: String;
    public
        constructor Create(Name: String);
        property Name: String read FName;
    end;

    TTestComparator_Nat = class(TAbstractComparator)
    public
        function Compare(const Item1, Item2: ICollectable): Integer; override;
        function Equals(const Item1, Item2: ICollectable): Boolean; override;
    end;

    TTestFilter_NonNat = class(TAbstractFilter)
    private
        FAllowedChars: String;
    public
        constructor Create(AllowedChars: String);
        function Accept(const Item: ICollectable): Boolean; override;
    end;

    TTestFilter_Nat = class(TAbstractFilter)
    private
        FAllowedChars: String;
    public
        constructor Create(AllowedChars: String);
        function Accept(const Item: ICollectable): Boolean; override;
    end;

    TTestFilter_NatMap = class(TAbstractFilter)
    private
        FAllowedChars: String;
    public
        constructor Create(AllowedChars: String);
        function Accept(const Item: ICollectable): Boolean; override;
    end;

    TTestFilter_NatIntMap = class(TAbstractFilter)
    private
        FAllowedChars: String;
    public
        constructor Create(AllowedChars: String);
        function Accept(const Item: ICollectable): Boolean; override;
    end;

    TTestFilter_NatStrMap = class(TAbstractFilter)
    private
        FAllowedChars: String;
    public
        constructor Create(AllowedChars: String);
        function Accept(const Item: ICollectable): Boolean; override;
    end;


{ TFunctionalTestResult }
constructor TFunctionalTestResult.Create(Name: String);
begin
    FName := Name;
end;

{ TFunctionalTest }
constructor TFunctionalTest.Create;
begin
    FResultList := TList.Create;

    FNonNaturalObject1 := TNonNaturalItem.Create('Item1');
    FNonNaturalObject2 := TNonNaturalItem.Create('Item2');
    FNonNaturalObject3 := TNonNaturalItem.Create('Item3');
    FNonNaturalObject4 := TNonNaturalItem.Create('Item4');
    FEquatableObject1 := TStringWrapper.Create('Item1');
    FEquatableObject2 := TStringWrapper.Create('Item2');
    FEquatableObject3 := TStringWrapper.Create('Item3');
    FEquatableObject4 := TStringWrapper.Create('Item4');
    FComparableObject1 := TStringWrapper.Create('Item1');
    FComparableObject2 := TStringWrapper.Create('Item2');
    FComparableObject3 := TStringWrapper.Create('Item3');
    FComparableObject4 := TStringWrapper.Create('Item4');
    FHashableObject1 := TStringWrapper.Create('Item1');
    FHashableObject2 := TStringWrapper.Create('Item2');
    FHashableObject3 := TStringWrapper.Create('Item3');
    FHashableObject4 := TStringWrapper.Create('Item4');
    FGeneralObject1 := TStringWrapper.Create('Item1');
    FGeneralObject2 := TStringWrapper.Create('Item2');
    FGeneralObject3 := TStringWrapper.Create('Item3');
    FGeneralObject4 := TStringWrapper.Create('Item4');
    FFNonNaturalKeyMapObject1 := TAssociationWrapper.Create(FNonNaturalObject1, TStringWrapper.Create('Item1'));
    FFNonNaturalKeyMapObject2 := TAssociationWrapper.Create(FNonNaturalObject2, TStringWrapper.Create('Item2'));
    FFNonNaturalKeyMapObject3 := TAssociationWrapper.Create(FNonNaturalObject3, TStringWrapper.Create('Item3'));
    FFNonNaturalKeyMapObject4 := TAssociationWrapper.Create(FNonNaturalObject4, TStringWrapper.Create('Item4'));
    FNaturalKeyMapObject1 := TAssociationWrapper.Create('Item1', TStringWrapper.Create('Item1'));
    FNaturalKeyMapObject2 := TAssociationWrapper.Create('Item2', TStringWrapper.Create('Item2'));
    FNaturalKeyMapObject3 := TAssociationWrapper.Create('Item3', TStringWrapper.Create('Item3'));
    FNaturalKeyMapObject4 := TAssociationWrapper.Create('Item4', TStringWrapper.Create('Item4'));
    FIntegerMapObject1 := TIntegerAssociationWrapper.Create(1, TStringWrapper.Create('Item1'));
    FIntegerMapObject2 := TIntegerAssociationWrapper.Create(2, TStringWrapper.Create('Item2'));
    FIntegerMapObject3 := TIntegerAssociationWrapper.Create(3, TStringWrapper.Create('Item3'));
    FIntegerMapObject4 := TIntegerAssociationWrapper.Create(4, TStringWrapper.Create('Item4'));
    FStringMapObject1 := TStringAssociationWrapper.Create('Item1', TStringWrapper.Create('Item1'));
    FStringMapObject2 := TStringAssociationWrapper.Create('Item2', TStringWrapper.Create('Item2'));
    FStringMapObject3 := TStringAssociationWrapper.Create('Item3', TStringWrapper.Create('Item3'));
    FStringMapObject4 := TStringAssociationWrapper.Create('Item4', TStringWrapper.Create('Item4'));

    SetLength(FEmptyArray, 0);
    SetLength(FEmptyFIntegerArray, 0);
    SetLength(FEmptyFStringArray, 0);
    SetLength(FNonNaturalArray12, 2);
    FNonNaturalArray12[0] := FNonNaturalObject1;
    FNonNaturalArray12[1] := FNonNaturalObject2;
    SetLength(FNonNaturalArray34, 2);
    FNonNaturalArray34[0] := FNonNaturalObject3;
    FNonNaturalArray34[1] := FNonNaturalObject4;
    SetLength(FNonNaturalArray123, 3);
    FNonNaturalArray123[0] := FNonNaturalObject1;
    FNonNaturalArray123[1] := FNonNaturalObject2;
    FNonNaturalArray123[2] := FNonNaturalObject3;
    SetLength(FNonNaturalArray1234, 4);
    FNonNaturalArray1234[0] := FNonNaturalObject1;
    FNonNaturalArray1234[1] := FNonNaturalObject2;
    FNonNaturalArray1234[2] := FNonNaturalObject3;
    FNonNaturalArray1234[3] := FNonNaturalObject4;
    SetLength(FEquatableArray12, 2);
    FEquatableArray12[0] := FEquatableObject1;
    FEquatableArray12[1] := FEquatableObject2;
    SetLength(FComparableArray12, 2);
    FComparableArray12[0] := FComparableObject1;
    FComparableArray12[1] := FComparableObject2;
    SetLength(FHashableArray12, 2);
    FHashableArray12[0] := FHashableObject1;
    FHashableArray12[1] := FHashableObject2;
    SetLength(FGeneralArray12, 2);
    FGeneralArray12[0] := FGeneralObject1;
    FGeneralArray12[1] := FGeneralObject2;
    SetLength(FGeneralArray23, 2);
    FGeneralArray23[0] := FGeneralObject2;
    FGeneralArray23[1] := FGeneralObject3;
    SetLength(FGeneralArray34, 2);
    FGeneralArray34[0] := FGeneralObject3;
    FGeneralArray34[1] := FGeneralObject4;
    SetLength(FGeneralArray123, 3);
    FGeneralArray123[0] := FGeneralObject1;
    FGeneralArray123[1] := FGeneralObject2;
    FGeneralArray123[2] := FGeneralObject3;
    SetLength(FGeneralArray1234, 4);
    FGeneralArray1234[0] := FGeneralObject1;
    FGeneralArray1234[1] := FGeneralObject2;
    FGeneralArray1234[2] := FGeneralObject3;
    FGeneralArray1234[3] := FGeneralObject4;
    SetLength(FNaturalMapItemArray12, 2);
    FNaturalMapItemArray12[0] := FNaturalKeyMapObject1;
    FNaturalMapItemArray12[1] := FNaturalKeyMapObject2;
    SetLength(FNaturalMapItemArray23, 2);
    FNaturalMapItemArray23[0] := FNaturalKeyMapObject2;
    FNaturalMapItemArray23[1] := FNaturalKeyMapObject3;
    SetLength(FNaturalMapItemArray123, 3);
    FNaturalMapItemArray123[0] := FNaturalKeyMapObject1;
    FNaturalMapItemArray123[1] := FNaturalKeyMapObject2;
    FNaturalMapItemArray123[2] := FNaturalKeyMapObject3;
    SetLength(FNaturalMapItemArray1234, 4);
    FNaturalMapItemArray1234[0] := FNaturalKeyMapObject1;
    FNaturalMapItemArray1234[1] := FNaturalKeyMapObject2;
    FNaturalMapItemArray1234[2] := FNaturalKeyMapObject3;
    FNaturalMapItemArray1234[3] := FNaturalKeyMapObject4;
    SetLength(FIntegerArray12, 2);
    FIntegerArray12[0] := 1;
    FIntegerArray12[1] := 2;
    SetLength(FIntegerArray23, 2);
    FIntegerArray23[0] := 2;
    FIntegerArray23[1] := 3;
    SetLength(FIntegerArray34, 2);
    FIntegerArray34[0] := 3;
    FIntegerArray34[1] := 4;
    SetLength(FIntegerArray123, 3);
    FIntegerArray123[0] := 1;
    FIntegerArray123[1] := 2;
    FIntegerArray123[2] := 3;
    SetLength(FIntegerArray1234, 4);
    FIntegerArray1234[0] := 1;
    FIntegerArray1234[1] := 2;
    FIntegerArray1234[2] := 3;
    FIntegerArray1234[3] := 4;
    SetLength(FIntegerMapItemArray12, 2);
    FIntegerMapItemArray12[0] := FIntegerMapObject1;
    FIntegerMapItemArray12[1] := FIntegerMapObject2;
    SetLength(FIntegerMapItemArray23, 2);
    FIntegerMapItemArray23[0] := FIntegerMapObject2;
    FIntegerMapItemArray23[1] := FIntegerMapObject3;
    SetLength(FIntegerMapItemArray123, 3);
    FIntegerMapItemArray123[0] := FIntegerMapObject1;
    FIntegerMapItemArray123[1] := FIntegerMapObject2;
    FIntegerMapItemArray123[2] := FIntegerMapObject3;
    SetLength(FIntegerMapItemArray1234, 4);
    FIntegerMapItemArray1234[0] := FIntegerMapObject1;
    FIntegerMapItemArray1234[1] := FIntegerMapObject2;
    FIntegerMapItemArray1234[2] := FIntegerMapObject3;
    FIntegerMapItemArray1234[3] := FIntegerMapObject4;
    SetLength(FStringArray12, 2);
    FStringArray12[0] := 'Item1';
    FStringArray12[1] := 'Item2';
    SetLength(FStringArray23, 2);
    FStringArray23[0] := 'Item2';
    FStringArray23[1] := 'Item3';
    SetLength(FStringArray34, 2);
    FStringArray34[0] := 'Item3';
    FStringArray34[1] := 'Item4';
    SetLength(FStringArray123, 3);
    FStringArray123[0] := 'Item1';
    FStringArray123[1] := 'Item2';
    FStringArray123[2] := 'Item3';
    SetLength(FStringArray1234, 4);
    FStringArray1234[0] := 'Item1';
    FStringArray1234[1] := 'Item2';
    FStringArray1234[2] := 'Item3';
    FStringArray1234[3] := 'Item4';
    SetLength(FStringMapItemArray12, 2);
    FStringMapItemArray12[0] := FStringMapObject1;
    FStringMapItemArray12[1] := FStringMapObject2;
    SetLength(FStringMapItemArray23, 2);
    FStringMapItemArray23[0] := FStringMapObject2;
    FStringMapItemArray23[1] := FStringMapObject3;
    SetLength(FStringMapItemArray123, 3);
    FStringMapItemArray123[0] := FStringMapObject1;
    FStringMapItemArray123[1] := FStringMapObject2;
    FStringMapItemArray123[2] := FStringMapObject3;
    SetLength(FStringMapItemArray1234, 4);
    FStringMapItemArray1234[0] := FStringMapObject1;
    FStringMapItemArray1234[1] := FStringMapObject2;
    FStringMapItemArray1234[2] := FStringMapObject3;
    FStringMapItemArray1234[3] := FStringMapObject4;

    FEmptyCollection := TPArrayBag.Create(False);
    FEmptySet := TPArraySet.Create(False);
    FNonNaturalCollection12 := TPArrayBag.Create(False);
    FNonNaturalCollection12.Add(FNonNaturalObject1);
    FNonNaturalCollection12.Add(FNonNaturalObject2);
    FNonNaturalCollection34 := TPArrayBag.Create(False);
    FNonNaturalCollection34.Add(FNonNaturalObject3);
    FNonNaturalCollection34.Add(FNonNaturalObject4);
    FNonNaturalCollection123 := TPArrayBag.Create(False);
    FNonNaturalCollection123.Add(FNonNaturalObject1);
    FNonNaturalCollection123.Add(FNonNaturalObject2);
    FNonNaturalCollection123.Add(FNonNaturalObject3);
    FNonNaturalCollection1234 := TPArrayBag.Create(False);
    FNonNaturalCollection1234.Add(FNonNaturalObject1);
    FNonNaturalCollection1234.Add(FNonNaturalObject2);
    FNonNaturalCollection1234.Add(FNonNaturalObject3);
    FNonNaturalCollection1234.Add(FNonNaturalObject4);
    FEquatableCollection12 := TPArrayBag.Create(True);
    FEquatableCollection12.Add(FEquatableObject1);
    FEquatableCollection12.Add(FEquatableObject2);
    FComparableCollection12 := TPArrayList.Create(True);
    FComparableCollection12.Add(FComparableObject1);
    FComparableCollection12.Add(FComparableObject2);
    FHashableCollection12 := THashSet.Create(True);
    FHashableCollection12.Add(FHashableObject1);
    FHashableCollection12.Add(FHashableObject2);
    FGeneralCollection12 := TPArrayList.Create(True);
    FGeneralCollection12.Add(FGeneralObject1);
    FGeneralCollection12.Add(FGeneralObject2);
    FGeneralCollection23 := TPArrayList.Create(True);
    FGeneralCollection23.Add(FGeneralObject2);
    FGeneralCollection23.Add(FGeneralObject3);
    FGeneralCollection34 := TPArrayList.Create(True);
    FGeneralCollection34.Add(FGeneralObject3);
    FGeneralCollection34.Add(FGeneralObject4);
    FGeneralCollection123 := TPArrayList.Create(True);
    FGeneralCollection123.Add(FGeneralObject1);
    FGeneralCollection123.Add(FGeneralObject2);
    FGeneralCollection123.Add(FGeneralObject3);
    FGeneralCollection1234 := TPArrayList.Create(True);
    FGeneralCollection1234.Add(FGeneralObject1);
    FGeneralCollection1234.Add(FGeneralObject2);
    FGeneralCollection1234.Add(FGeneralObject3);
    FGeneralCollection1234.Add(FGeneralObject4);
    FNonNaturalKeyMap12 := TPArrayMap.Create(False, False);
    FNonNaturalKeyMap12.Put(FNonNaturalObject1, FNonNaturalObject1);
    FNonNaturalKeyMap12.Put(FNonNaturalObject2, FNonNaturalObject2);
    FNaturalKeyMap12 := TPArrayMap.Create(False, True);
    FNaturalKeyMap12.Put(FGeneralObject1, FNonNaturalObject1);
    FNaturalKeyMap12.Put(FGeneralObject2, FNonNaturalObject2);
    FNaturalItemMap12 := TPArrayMap.Create(True, True);
    FNaturalItemMap12.Put(FNaturalKeyMapObject1);
    FNaturalItemMap12.Put(FNaturalKeyMapObject2);
    FNonNaturalItemIntMap12 := THashIntegerMap.Create(False);
    FNonNaturalItemIntMap12.Put(1, FNonNaturalObject1);
    FNonNaturalItemIntMap12.Put(2, FNonNaturalObject2);
    FNaturalItemIntMap12 := THashIntegerMap.Create(True);
    FNaturalItemIntMap12.Put(FIntegerMapObject1);
    FNaturalItemIntMap12.Put(FIntegerMapObject2);
    FNonNaturalItemStrMap12 := THashStringMap.Create(False);
    FNonNaturalItemStrMap12.Put('Item1', FNonNaturalObject1);
    FNonNaturalItemStrMap12.Put('Item2', FNonNaturalObject2);
    FNaturalItemStrMap12 := THashStringMap.Create(True);
    FNaturalItemStrMap12.Put(FStringMapObject1);
    FNaturalItemStrMap12.Put(FStringMapObject2);
end;

destructor TFunctionalTest.Destroy;
begin
    ClearTestList;
    FResultList.Free;
end;

procedure TFunctionalTest.TestClass;
var
    I: Integer;
begin
    AddTestList;
    for I := 0 to ResultList.Count - 1 do
    begin
        DoTest(ResultList[I]);
    end;
end;

procedure TFunctionalTest.DoTest(TestResult: TFunctionalTestResult);
var
    TestMethod: TTestMethod;
    Method: TMethod;
    P: Pointer;
    TestName: String;
begin
    TestName := TestResult.Name;
    P := TFunctionalTest.MethodAddress(TestName);
    if Assigned(P) then
    begin
        Method.Code := P;
        Method.Data := Self;
        TestMethod := TTestMethod(Method);
        try
            TestResult.Success := TestMethod;
        except
            TestResult.Success := False;
        end;
    end
    else
        TestResult.Success := False;
end;

procedure TFunctionalTest.ClearTestList;
var
    TestResult: TFunctionalTestResult;
    I: Integer;
begin
    for I := 0 to FResultList.Count - 1 do
    begin
        TestResult := TFunctionalTestResult(FResultList[I]);
        TestResult.Free;
    end;
    FResultList.Clear;
end;

procedure TFunctionalTest.AddTestList;
var
    Collection: ICollection;
begin
    Collection := CollectionClass.Create;
    case Collection.GetType of
    ctBag: AddTestListBag;
    ctSet: AddTestListSet;
    ctList: AddTestListList;
    ctMap: AddTestListMap;
    ctIntegerMap: AddTestListIntegerMap;
    ctStringMap: AddTestListStringMap;
    end;
end;

procedure TFunctionalTest.AddTestListBag;
var
    Collection: ICollection;
    FixedSize: Boolean;
    AlwaysNaturalItems: Boolean;
    NaturalItemIID: TGUID;
begin
    Collection := CollectionClass.Create;
    FixedSize := Collection.GetFixedSize;
    NaturalItemIID := Collection.GetNaturalItemIID;
    AlwaysNaturalItems := CollectionClass.GetAlwaysNaturalItems;
    // Constructor tests
    ResultList.Add(TFunctionalTestResult.Create('Create_Def'));
    if not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Create_Arr'));
        ResultList.Add(TFunctionalTestResult.Create('Create_IF'));
    end
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IEquatable) then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Create_ArrEqu'));
        ResultList.Add(TFunctionalTestResult.Create('Create_IFEqu'));
    end
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IComparable) then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Create_ArrComp'));
        ResultList.Add(TFunctionalTestResult.Create('Create_IFComp'));
    end
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IHashable) then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Create_ArrHash'));
        ResultList.Add(TFunctionalTestResult.Create('Create_IFHash'));
    end;

    // ICollection tests
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('Add_Fix'))
    else
        ResultList.Add(TFunctionalTestResult.Create('Add_Bag'));
    if not FixedSize and not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Add_Arr'));
        ResultList.Add(TFunctionalTestResult.Create('Add_IF'));
    end;
    if FixedSize then
    begin
        // No test here
    end
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IEquatable) then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Add_ArrEqu'));
        ResultList.Add(TFunctionalTestResult.Create('Add_IFEqu'));
    end
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IComparable) then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Add_ArrComp'));
        ResultList.Add(TFunctionalTestResult.Create('Add_IFComp'));
    end
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IHashable) then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Add_ArrHash'));
        ResultList.Add(TFunctionalTestResult.Create('Add_IFHash'));
    end;
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('Clear_Fix'))
    else
        ResultList.Add(TFunctionalTestResult.Create('Clear_NonFix'));
    ResultList.Add(TFunctionalTestResult.Create('Clone'));
    ResultList.Add(TFunctionalTestResult.Create('CloneAsBag'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('Contains_NonNat'));
    ResultList.Add(TFunctionalTestResult.Create('Contains_Nat'));
    ResultList.Add(TFunctionalTestResult.Create('Contains_Arr'));
    ResultList.Add(TFunctionalTestResult.Create('Contains_IF'));
    ResultList.Add(TFunctionalTestResult.Create('Equals_Bag'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('Find_FilterNonNat'));
    ResultList.Add(TFunctionalTestResult.Create('Find_FilterNat'));
    if not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Find_FuncNonNat'));
        ResultList.Add(TFunctionalTestResult.Create('FindAll_FilterNonNat'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('FindAll_FilterNat'));
    ResultList.Add(TFunctionalTestResult.Create('GetAsArray'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('GetComparator_NonNat'));
    ResultList.Add(TFunctionalTestResult.Create('GetComparator_Nat'));
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('GetFixedSize_Fix'))
    else
        ResultList.Add(TFunctionalTestResult.Create('GetFixedSize_NonFix'));
    ResultList.Add(TFunctionalTestResult.Create('GetIgnoreErrors'));
    ResultList.Add(TFunctionalTestResult.Create('GetInstance'));
    ResultList.Add(TFunctionalTestResult.Create('GetIterator'));
    if not FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('GetIterator_Rem'));
    ResultList.Add(TFunctionalTestResult.Create('GetIterator_Filter'));
    ResultList.Add(TFunctionalTestResult.Create('GetIterator_Func'));
    if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IEquatable) then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemIID_Equ'))
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IComparable) then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemIID_Comp'))
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IHashable) then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemIID_Hash'));
    if AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemsOnly_Nat'))
    else
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemsOnly_Both'));
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('GetSize_Fix'))
    else
        ResultList.Add(TFunctionalTestResult.Create('GetSize_NonFix'));
    ResultList.Add(TFunctionalTestResult.Create('IsEmpty'));
    ResultList.Add(TFunctionalTestResult.Create('IsNaturalItem'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('ItemAllowed_NonNat'));
    ResultList.Add(TFunctionalTestResult.Create('ItemAllowed_Nat'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('ItemCount_NonNatBagList'));
    ResultList.Add(TFunctionalTestResult.Create('ItemCount_NatBagList'));
    ResultList.Add(TFunctionalTestResult.Create('ItemCount_Arr'));
    ResultList.Add(TFunctionalTestResult.Create('ItemCount_IF'));
    ResultList.Add(TFunctionalTestResult.Create('Matching_ArrNonNat'));
    ResultList.Add(TFunctionalTestResult.Create('Matching_IFNonNat'));
    ResultList.Add(TFunctionalTestResult.Create('Matching_ArrNat'));
    ResultList.Add(TFunctionalTestResult.Create('Matching_IFNat'));
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('Remove_Fix'))
    else
    begin
        ResultList.Add(TFunctionalTestResult.Create('Remove_Dup'));
        ResultList.Add(TFunctionalTestResult.Create('RemoveAll_Dup'));
        ResultList.Add(TFunctionalTestResult.Create('Remove_Arr'));
        ResultList.Add(TFunctionalTestResult.Create('Remove_IF'));
        ResultList.Add(TFunctionalTestResult.Create('Retain_Arr'));
        ResultList.Add(TFunctionalTestResult.Create('Retain_IF'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('SetComparator'));
    ResultList.Add(TFunctionalTestResult.Create('SetIgnoreErrors'));

end;

procedure TFunctionalTest.AddTestListSet;
var
    Collection: ICollection;
    FixedSize: Boolean;
    AlwaysNaturalItems: Boolean;
    NaturalItemIID: TGUID;
begin
    Collection := CollectionClass.Create;
    FixedSize := Collection.GetFixedSize;
    NaturalItemIID := Collection.GetNaturalItemIID;
    AlwaysNaturalItems := CollectionClass.GetAlwaysNaturalItems;
    // Constructor tests
    ResultList.Add(TFunctionalTestResult.Create('Create_Def'));
    if not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Create_Arr'));
        ResultList.Add(TFunctionalTestResult.Create('Create_IF'));
    end
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IEquatable) then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Create_ArrEqu'));
        ResultList.Add(TFunctionalTestResult.Create('Create_IFEqu'));
    end
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IComparable) then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Create_ArrComp'));
        ResultList.Add(TFunctionalTestResult.Create('Create_IFComp'));
    end
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IHashable) then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Create_ArrHash'));
        ResultList.Add(TFunctionalTestResult.Create('Create_IFHash'));
    end;

    // ICollection tests
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('Add_Fix'))
    else
        ResultList.Add(TFunctionalTestResult.Create('Add_Set'));
    if not FixedSize and not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Add_Arr'));
        ResultList.Add(TFunctionalTestResult.Create('Add_IF'));
    end;
    if FixedSize then
    begin
        // No test here
    end
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IEquatable) then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Add_ArrEqu'));
        ResultList.Add(TFunctionalTestResult.Create('Add_IFEqu'));
    end
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IComparable) then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Add_ArrComp'));
        ResultList.Add(TFunctionalTestResult.Create('Add_IFComp'));
    end
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IHashable) then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Add_ArrHash'));
        ResultList.Add(TFunctionalTestResult.Create('Add_IFHash'));
    end;
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('Clear_Fix'))
    else
        ResultList.Add(TFunctionalTestResult.Create('Clear_NonFix'));
    ResultList.Add(TFunctionalTestResult.Create('Clone'));
    ResultList.Add(TFunctionalTestResult.Create('CloneAsSet'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('Contains_NonNat'));
    ResultList.Add(TFunctionalTestResult.Create('Contains_Nat'));
    ResultList.Add(TFunctionalTestResult.Create('Contains_Arr'));
    ResultList.Add(TFunctionalTestResult.Create('Contains_IF'));
    ResultList.Add(TFunctionalTestResult.Create('Equals_Set'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('Find_FilterNonNat'));
    ResultList.Add(TFunctionalTestResult.Create('Find_FilterNat'));
    if not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Find_FuncNonNat'));
        ResultList.Add(TFunctionalTestResult.Create('FindAll_FilterNonNat'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('FindAll_FilterNat'));
    ResultList.Add(TFunctionalTestResult.Create('GetAsArray'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('GetComparator_NonNat'));
    ResultList.Add(TFunctionalTestResult.Create('GetComparator_Nat'));
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('GetFixedSize_Fix'))
    else
        ResultList.Add(TFunctionalTestResult.Create('GetFixedSize_NonFix'));
    ResultList.Add(TFunctionalTestResult.Create('GetIgnoreErrors'));
    ResultList.Add(TFunctionalTestResult.Create('GetInstance'));
    ResultList.Add(TFunctionalTestResult.Create('GetIterator'));
    if not FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('GetIterator_Rem'));
    ResultList.Add(TFunctionalTestResult.Create('GetIterator_Filter'));
    ResultList.Add(TFunctionalTestResult.Create('GetIterator_Func'));
    if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IEquatable) then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemIID_Equ'))
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IComparable) then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemIID_Comp'))
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IHashable) then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemIID_Hash'));
    if AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemsOnly_Nat'))
    else
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemsOnly_Both'));
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('GetSize_Fix'))
    else
        ResultList.Add(TFunctionalTestResult.Create('GetSize_NonFix'));
    ResultList.Add(TFunctionalTestResult.Create('IsEmpty'));
    ResultList.Add(TFunctionalTestResult.Create('IsNaturalItem'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('ItemAllowed_NonNat'));
    ResultList.Add(TFunctionalTestResult.Create('ItemAllowed_Nat'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('ItemCount_NonNatSet'));
    ResultList.Add(TFunctionalTestResult.Create('ItemCount_NatSet'));
    ResultList.Add(TFunctionalTestResult.Create('ItemCount_Arr'));
    ResultList.Add(TFunctionalTestResult.Create('ItemCount_IF'));
    if not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Matching_ArrNonNat'));
        ResultList.Add(TFunctionalTestResult.Create('Matching_IFNonNat'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('Matching_ArrNat'));
    ResultList.Add(TFunctionalTestResult.Create('Matching_IFNat'));
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('Remove_Fix'))
    else
    begin
        ResultList.Add(TFunctionalTestResult.Create('Remove_NoDup'));
        ResultList.Add(TFunctionalTestResult.Create('RemoveAll_NoDup'));
        ResultList.Add(TFunctionalTestResult.Create('Remove_Arr'));
        ResultList.Add(TFunctionalTestResult.Create('Remove_IF'));
        ResultList.Add(TFunctionalTestResult.Create('Retain_Arr'));
        ResultList.Add(TFunctionalTestResult.Create('Retain_IF'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('SetComparator'));
    ResultList.Add(TFunctionalTestResult.Create('SetIgnoreErrors'));

    // ISet functions
    if not FixedSize then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Complement'));
        ResultList.Add(TFunctionalTestResult.Create('Intersect'));
        ResultList.Add(TFunctionalTestResult.Create('Union'));
    end;

end;

procedure TFunctionalTest.AddTestListList;
var
    Collection: ICollection;
    FixedSize: Boolean;
    AlwaysNaturalItems: Boolean;
    NaturalItemIID: TGUID;
begin
    Collection := CollectionClass.Create;
    FixedSize := Collection.GetFixedSize;
    NaturalItemIID := Collection.GetNaturalItemIID;
    AlwaysNaturalItems := CollectionClass.GetAlwaysNaturalItems;
    // Constructor tests
    ResultList.Add(TFunctionalTestResult.Create('Create_Def'));
    if not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Create_Arr'));
        ResultList.Add(TFunctionalTestResult.Create('Create_IF'));
    end
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IEquatable) then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Create_ArrEqu'));
        ResultList.Add(TFunctionalTestResult.Create('Create_IFEqu'));
    end
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IComparable) then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Create_ArrComp'));
        ResultList.Add(TFunctionalTestResult.Create('Create_IFComp'));
    end
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IHashable) then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Create_ArrHash'));
        ResultList.Add(TFunctionalTestResult.Create('Create_IFHash'));
    end;

    // ICollection tests
    if FixedSize then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Add_Fix'));
        ResultList.Add(TFunctionalTestResult.Create('Clear_Fix'));
    end
    else
    begin
        ResultList.Add(TFunctionalTestResult.Create('Add_List'));
        if not AlwaysNaturalItems then
        begin
            ResultList.Add(TFunctionalTestResult.Create('Add_Arr'));
            ResultList.Add(TFunctionalTestResult.Create('Add_IF'));
        end;
        if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IEquatable) then
        begin
            ResultList.Add(TFunctionalTestResult.Create('Add_ArrEqu'));
            ResultList.Add(TFunctionalTestResult.Create('Add_IFEqu'));
        end
        else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IComparable) then
        begin
            ResultList.Add(TFunctionalTestResult.Create('Add_ArrComp'));
            ResultList.Add(TFunctionalTestResult.Create('Add_IFComp'));
        end
        else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IHashable) then
        begin
            ResultList.Add(TFunctionalTestResult.Create('Add_ArrHash'));
            ResultList.Add(TFunctionalTestResult.Create('Add_IFHash'));
        end;
        ResultList.Add(TFunctionalTestResult.Create('Clear_NonFix'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('Clone'));
    ResultList.Add(TFunctionalTestResult.Create('CloneAsList'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('Contains_NonNatList'));
    ResultList.Add(TFunctionalTestResult.Create('Contains_NatList'));
    ResultList.Add(TFunctionalTestResult.Create('Contains_ArrList'));
    ResultList.Add(TFunctionalTestResult.Create('Contains_IFList'));
    ResultList.Add(TFunctionalTestResult.Create('Equals_List'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('Find_FilterNonNatList'));
    ResultList.Add(TFunctionalTestResult.Create('Find_FilterNatList'));
    if not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Find_FuncNonNatList'));
        ResultList.Add(TFunctionalTestResult.Create('FindAll_FilterNonNatList'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('FindAll_FilterNatList'));
    ResultList.Add(TFunctionalTestResult.Create('GetAsArrayList'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('GetComparator_NonNat'));
    ResultList.Add(TFunctionalTestResult.Create('GetComparator_Nat'));
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('GetFixedSize_Fix'))
    else
        ResultList.Add(TFunctionalTestResult.Create('GetFixedSize_NonFix'));
    ResultList.Add(TFunctionalTestResult.Create('GetIgnoreErrors'));
    ResultList.Add(TFunctionalTestResult.Create('GetInstance'));
    ResultList.Add(TFunctionalTestResult.Create('GetIterator'));
    if not FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('GetIterator_Rem'));
    ResultList.Add(TFunctionalTestResult.Create('GetIterator_Filter'));
    ResultList.Add(TFunctionalTestResult.Create('GetIterator_Func'));
    if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IEquatable) then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemIID_Equ'))
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IComparable) then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemIID_Comp'))
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IHashable) then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemIID_Hash'));
    if AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemsOnly_Nat'))
    else
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemsOnly_Both'));
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('GetSize_Fix'))
    else
        ResultList.Add(TFunctionalTestResult.Create('GetSize_NonFix'));
    ResultList.Add(TFunctionalTestResult.Create('IsEmpty'));
    ResultList.Add(TFunctionalTestResult.Create('IsNaturalItem'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('ItemAllowed_NonNat'));
    ResultList.Add(TFunctionalTestResult.Create('ItemAllowed_Nat'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('ItemCount_NonNatBagList'));
    ResultList.Add(TFunctionalTestResult.Create('ItemCount_NatBagList'));
    ResultList.Add(TFunctionalTestResult.Create('ItemCount_Arr'));
    ResultList.Add(TFunctionalTestResult.Create('ItemCount_IF'));
    if not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Matching_ArrNonNatList'));
        ResultList.Add(TFunctionalTestResult.Create('Matching_IFNonNatList'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('Matching_ArrNatList'));
    ResultList.Add(TFunctionalTestResult.Create('Matching_IFNatList'));
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('Remove_Fix'))
    else
    begin
        ResultList.Add(TFunctionalTestResult.Create('Remove_List'));
        ResultList.Add(TFunctionalTestResult.Create('RemoveAll_List'));
        ResultList.Add(TFunctionalTestResult.Create('Remove_ArrList'));
        ResultList.Add(TFunctionalTestResult.Create('Remove_IFList'));
        ResultList.Add(TFunctionalTestResult.Create('Retain_ArrList'));
        ResultList.Add(TFunctionalTestResult.Create('Retain_IFList'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('SetComparator'));
    ResultList.Add(TFunctionalTestResult.Create('SetIgnoreErrors'));

    // IList functions
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('Delete_Fix'))
    else
        ResultList.Add(TFunctionalTestResult.Create('Delete_NonFix'));
    ResultList.Add(TFunctionalTestResult.Create('Exchange'));
    ResultList.Add(TFunctionalTestResult.Create('First'));
    ResultList.Add(TFunctionalTestResult.Create('GetDuplicates'));
    ResultList.Add(TFunctionalTestResult.Create('GetSorted'));
    ResultList.Add(TFunctionalTestResult.Create('IndexOf'));
    if FixedSize then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Insert_Fix'));
    end
    else
    begin
        ResultList.Add(TFunctionalTestResult.Create('Insert_NonFix'));
        ResultList.Add(TFunctionalTestResult.Create('Insert_Arr'));
        ResultList.Add(TFunctionalTestResult.Create('Insert_IF'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('Last'));
    ResultList.Add(TFunctionalTestResult.Create('SetDuplicates'));
    ResultList.Add(TFunctionalTestResult.Create('SetItem'));
    ResultList.Add(TFunctionalTestResult.Create('SetSorted'));
    ResultList.Add(TFunctionalTestResult.Create('Sort'));
end;

procedure TFunctionalTest.AddTestListMap;
var
    Collection: ICollection;
    Map: IMap;
    FixedSize: Boolean;
    AlwaysNaturalItems: Boolean;
    AlwaysNaturalKeys: Boolean;
    NaturalItemIID: TGUID;
    NaturalKeyIID: TGUID;
begin
    Collection := CollectionClass.Create;
    FixedSize := Collection.GetFixedSize;
    NaturalItemIID := Collection.GetNaturalItemIID;
    AlwaysNaturalItems := CollectionClass.GetAlwaysNaturalItems;
    Map := Collection as IMap;
    NaturalKeyIID := Map.GetNaturalKeyIID;
    AlwaysNaturalKeys := TAbstractMapClass(CollectionClass).GetAlwaysNaturalKeys;
    // Constructor tests
    ResultList.Add(TFunctionalTestResult.Create('Create_Def'));
    ResultList.Add(TFunctionalTestResult.Create('Create_ArrMap'));
    ResultList.Add(TFunctionalTestResult.Create('Create_IFMap'));
    if not AlwaysNaturalKeys then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Create_IMap'));
        ResultList.Add(TFunctionalTestResult.Create('Create_ArrArr'));
    end
    else
    begin
        ResultList.Add(TFunctionalTestResult.Create('Create_IMapNat'));
        ResultList.Add(TFunctionalTestResult.Create('Create_ArrArrNat'));
    end;

    // ICollection tests
    if FixedSize then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Add_Fix'));
        ResultList.Add(TFunctionalTestResult.Create('Clear_Fix'));
        ResultList.Add(TFunctionalTestResult.Create('Clear_NonFixMap'));
    end
    else
    begin
        ResultList.Add(TFunctionalTestResult.Create('Add_Map'));
        ResultList.Add(TFunctionalTestResult.Create('Add_ArrMap'));
        ResultList.Add(TFunctionalTestResult.Create('Add_IFMap'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('Clone_Map'));
    ResultList.Add(TFunctionalTestResult.Create('CloneAsMap'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('Contains_NonNatMap'));
    ResultList.Add(TFunctionalTestResult.Create('Contains_NatMap'));
    ResultList.Add(TFunctionalTestResult.Create('Contains_ArrMap'));
    ResultList.Add(TFunctionalTestResult.Create('Contains_IFMap'));
    ResultList.Add(TFunctionalTestResult.Create('Equals_Map'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('Find_FilterNonNatMap'));
    ResultList.Add(TFunctionalTestResult.Create('Find_FilterNatMap'));
    if not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Find_FuncNonNatMap'));
        ResultList.Add(TFunctionalTestResult.Create('FindAll_FilterNonNatMap'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('FindAll_FilterNatMap'));
    ResultList.Add(TFunctionalTestResult.Create('GetAsArray_Map'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('GetComparator_NonNat'));
    ResultList.Add(TFunctionalTestResult.Create('GetComparator_Nat'));
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('GetFixedSize_Fix'))
    else
        ResultList.Add(TFunctionalTestResult.Create('GetFixedSize_NonFix'));
    ResultList.Add(TFunctionalTestResult.Create('GetIgnoreErrors'));
    ResultList.Add(TFunctionalTestResult.Create('GetInstance'));
    ResultList.Add(TFunctionalTestResult.Create('GetIterator_Map'));
    if not FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('GetIterator_RemMap'));
    if not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('GetIterator_FilterMap'));
        ResultList.Add(TFunctionalTestResult.Create('GetIterator_FuncMap'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemIID_Map'));
    if AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemsOnly_Nat'))
    else
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemsOnly_Both'));
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('GetSize_Fix'))
    else
        ResultList.Add(TFunctionalTestResult.Create('GetSize_NonFixMap'));
    ResultList.Add(TFunctionalTestResult.Create('IsEmpty_Map'));
    ResultList.Add(TFunctionalTestResult.Create('IsNaturalItem_Map'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('ItemAllowed_NonNat'));
    ResultList.Add(TFunctionalTestResult.Create('ItemAllowed_NatMap'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('ItemCount_NonNatMap'));
    ResultList.Add(TFunctionalTestResult.Create('ItemCount_NatMap'));
    ResultList.Add(TFunctionalTestResult.Create('ItemCount_ArrMap'));
    ResultList.Add(TFunctionalTestResult.Create('ItemCount_IFMap'));
    if not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Matching_ArrNonNatMap'));
        ResultList.Add(TFunctionalTestResult.Create('Matching_IFNonNatMap'));
    end;
        ResultList.Add(TFunctionalTestResult.Create('Matching_ArrNatMap'));
        ResultList.Add(TFunctionalTestResult.Create('Matching_IFNatMap'));
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('Remove_Fix'))
    else
    begin
        ResultList.Add(TFunctionalTestResult.Create('Remove_Map'));
        ResultList.Add(TFunctionalTestResult.Create('RemoveAll_Map'));
        ResultList.Add(TFunctionalTestResult.Create('Remove_ArrMap'));
        ResultList.Add(TFunctionalTestResult.Create('Remove_IFMap'));
        ResultList.Add(TFunctionalTestResult.Create('Retain_ArrMap'));
        ResultList.Add(TFunctionalTestResult.Create('Retain_IFMap'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('SetComparator'));
    ResultList.Add(TFunctionalTestResult.Create('SetIgnoreErrors'));

    // IMap functions
    if not AlwaysNaturalKeys then
        ResultList.Add(TFunctionalTestResult.Create('ContainsKey_NonNat'));
    ResultList.Add(TFunctionalTestResult.Create('ContainsKey_Nat'));
    ResultList.Add(TFunctionalTestResult.Create('ContainsKey_Arr'));
    ResultList.Add(TFunctionalTestResult.Create('ContainsKey_IF'));
    if not AlwaysNaturalKeys then
        ResultList.Add(TFunctionalTestResult.Create('Get_NonNat'));
    ResultList.Add(TFunctionalTestResult.Create('Get_Nat'));
    if not AlwaysNaturalKeys then
        ResultList.Add(TFunctionalTestResult.Create('GetKeyComparator_NonNat'));
    ResultList.Add(TFunctionalTestResult.Create('GetKeyComparator_Nat'));
    ResultList.Add(TFunctionalTestResult.Create('GetKeyIterator'));
    ResultList.Add(TFunctionalTestResult.Create('GetKeys'));
    ResultList.Add(TFunctionalTestResult.Create('GetMapIterator'));
    if not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('GetMapIteratorByKey_Filter'));
        ResultList.Add(TFunctionalTestResult.Create('GetMapIteratorByKey_Func'));
    end;
    if TMiscCollectionLibrary.EqualIID(NaturalKeyIID, IEquatable) then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalKeyIID_Equ'))
    else if TMiscCollectionLibrary.EqualIID(NaturalKeyIID, IComparable) then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalKeyIID_Comp'))
    else if TMiscCollectionLibrary.EqualIID(NaturalKeyIID, IHashable) then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalKeyIID_Hash'));
    if AlwaysNaturalKeys then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalKeysOnly_Nat'))
    else
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalKeysOnly_Both'));
    ResultList.Add(TFunctionalTestResult.Create('GetValues'));
    if not AlwaysNaturalKeys then
        ResultList.Add(TFunctionalTestResult.Create('KeyAllowed_NonNat'));
    ResultList.Add(TFunctionalTestResult.Create('KeyAllowed_Nat'));
    if not AlwaysNaturalKeys then
        ResultList.Add(TFunctionalTestResult.Create('MatchingKey_ArrNonNat'));
    ResultList.Add(TFunctionalTestResult.Create('MatchingKey_ArrNat'));
    if not AlwaysNaturalKeys then
        ResultList.Add(TFunctionalTestResult.Create('MatchingKey_IFNonNat'));
    ResultList.Add(TFunctionalTestResult.Create('MatchingKey_IFNat'));
    ResultList.Add(TFunctionalTestResult.Create('Put_Item'));
    ResultList.Add(TFunctionalTestResult.Create('Put_KeyItem'));
    ResultList.Add(TFunctionalTestResult.Create('Put_Arr'));
    ResultList.Add(TFunctionalTestResult.Create('Put_IF'));
    ResultList.Add(TFunctionalTestResult.Create('Put_IMap'));
    if not FixedSize then
    begin
        ResultList.Add(TFunctionalTestResult.Create('RemoveKey_Key'));
        ResultList.Add(TFunctionalTestResult.Create('RemoveKey_Arr'));
        ResultList.Add(TFunctionalTestResult.Create('RemoveKey_IF'));
        ResultList.Add(TFunctionalTestResult.Create('RetainKey_Arr'));
        ResultList.Add(TFunctionalTestResult.Create('RetainKey_IF'));
    end;
end;

procedure TFunctionalTest.AddTestListIntegerMap;
var
    Collection: ICollection;
    FixedSize: Boolean;
    AlwaysNaturalItems: Boolean;
    NaturalItemIID: TGUID;
begin
    Collection := CollectionClass.Create;
    FixedSize := Collection.GetFixedSize;
    NaturalItemIID := Collection.GetNaturalItemIID;
    AlwaysNaturalItems := CollectionClass.GetAlwaysNaturalItems;
    
    // Constructor tests
    ResultList.Add(TFunctionalTestResult.Create('Create_Def'));
    ResultList.Add(TFunctionalTestResult.Create('Create_IIntMap'));
    ResultList.Add(TFunctionalTestResult.Create('Create_ArrArrInt'));

    // ICollection tests
    if FixedSize then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Add_Fix'));
        ResultList.Add(TFunctionalTestResult.Create('Clear_Fix'));
    end
    else
    begin
        ResultList.Add(TFunctionalTestResult.Create('Add_IntMap'));
        ResultList.Add(TFunctionalTestResult.Create('Add_ArrIntMap'));
        ResultList.Add(TFunctionalTestResult.Create('Add_IFIntMap'));
        ResultList.Add(TFunctionalTestResult.Create('Clear_NonFixIntMap'))
    end;
    
    ResultList.Add(TFunctionalTestResult.Create('Clone_IntMap'));
    ResultList.Add(TFunctionalTestResult.Create('CloneAsIntegerMap'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('Contains_NonNatIntMap'));
    ResultList.Add(TFunctionalTestResult.Create('Contains_NatIntMap'));
    ResultList.Add(TFunctionalTestResult.Create('Contains_ArrIntMap'));
    ResultList.Add(TFunctionalTestResult.Create('Contains_IFIntMap'));
    ResultList.Add(TFunctionalTestResult.Create('Equals_IntMap'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('Find_FilterNonNatIntMap'));
    ResultList.Add(TFunctionalTestResult.Create('Find_FilterNatIntMap'));
    if not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Find_FuncNonNatIntMap'));
        ResultList.Add(TFunctionalTestResult.Create('FindAll_FilterNonNatIntMap'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('FindAll_FilterNatIntMap'));
    ResultList.Add(TFunctionalTestResult.Create('GetAsArray_IntMap'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('GetComparator_NonNat'));
    ResultList.Add(TFunctionalTestResult.Create('GetComparator_Nat'));
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('GetFixedSize_Fix'))
    else
        ResultList.Add(TFunctionalTestResult.Create('GetFixedSize_NonFix'));
    ResultList.Add(TFunctionalTestResult.Create('GetIgnoreErrors'));
    ResultList.Add(TFunctionalTestResult.Create('GetInstance'));
    ResultList.Add(TFunctionalTestResult.Create('GetIterator_IntMap'));
    if not FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('GetIterator_RemIntMap'));
    if not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('GetIterator_FilterIntMap'));
        ResultList.Add(TFunctionalTestResult.Create('GetIterator_FuncIntMap'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemIID_IntMap'));
    if AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemsOnly_Nat'))
    else
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemsOnly_Both'));
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('GetSize_Fix'))
    else 
        ResultList.Add(TFunctionalTestResult.Create('GetSize_NonFixIntMap'));
    ResultList.Add(TFunctionalTestResult.Create('IsEmpty_IntMap'));
    ResultList.Add(TFunctionalTestResult.Create('IsNaturalItem_IntMap'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('ItemAllowed_NonNat'));
    ResultList.Add(TFunctionalTestResult.Create('ItemAllowed_NatIntMap'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('ItemCount_NonNatIntMap'));
    ResultList.Add(TFunctionalTestResult.Create('ItemCount_NatIntMap'));
    ResultList.Add(TFunctionalTestResult.Create('ItemCount_ArrIntMap'));
    ResultList.Add(TFunctionalTestResult.Create('ItemCount_IFIntMap'));
    if not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Matching_ArrNonNatIntMap'));
        ResultList.Add(TFunctionalTestResult.Create('Matching_IFNonNatIntMap'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('Matching_ArrNatIntMap'));
    ResultList.Add(TFunctionalTestResult.Create('Matching_IFNatIntMap'));
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('Remove_Fix'))
    else
    begin
        ResultList.Add(TFunctionalTestResult.Create('Remove_IntMap'));
        ResultList.Add(TFunctionalTestResult.Create('RemoveAll_IntMap'));
        ResultList.Add(TFunctionalTestResult.Create('Remove_ArrIntMap'));
        ResultList.Add(TFunctionalTestResult.Create('Remove_IFIntMap'));
        ResultList.Add(TFunctionalTestResult.Create('Retain_ArrIntMap'));
        ResultList.Add(TFunctionalTestResult.Create('Retain_IFIntMap'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('SetComparator'));
    ResultList.Add(TFunctionalTestResult.Create('SetIgnoreErrors'));

    // IIntegerMap functions
    ResultList.Add(TFunctionalTestResult.Create('ContainsKey_Int'));
    ResultList.Add(TFunctionalTestResult.Create('ContainsKey_ArrInt'));
    ResultList.Add(TFunctionalTestResult.Create('Get_Int'));
    ResultList.Add(TFunctionalTestResult.Create('GetKeys_Int'));
    ResultList.Add(TFunctionalTestResult.Create('GetMapIterator_Int'));
    ResultList.Add(TFunctionalTestResult.Create('GetValues_Int'));
    ResultList.Add(TFunctionalTestResult.Create('Put_ItemInt'));
    ResultList.Add(TFunctionalTestResult.Create('Put_KeyItemInt'));
    ResultList.Add(TFunctionalTestResult.Create('Put_ArrInt'));
    ResultList.Add(TFunctionalTestResult.Create('Put_IFInt'));
    ResultList.Add(TFunctionalTestResult.Create('Put_IIntMap'));
    if not FixedSize then
    begin
        ResultList.Add(TFunctionalTestResult.Create('RemoveKey_KeyInt'));
        ResultList.Add(TFunctionalTestResult.Create('RemoveKey_ArrInt'));
        ResultList.Add(TFunctionalTestResult.Create('RetainKey_ArrInt'));
    end;
end;

procedure TFunctionalTest.AddTestListStringMap;
var
    Collection: ICollection;
    FixedSize: Boolean;
    AlwaysNaturalItems: Boolean;
    NaturalItemIID: TGUID;
begin
    Collection := CollectionClass.Create;
    FixedSize := Collection.GetFixedSize;
    NaturalItemIID := Collection.GetNaturalItemIID;
    AlwaysNaturalItems := CollectionClass.GetAlwaysNaturalItems;
    
    // Constructor tests
    ResultList.Add(TFunctionalTestResult.Create('Create_Def'));
    ResultList.Add(TFunctionalTestResult.Create('Create_ArrStrMap'));
    ResultList.Add(TFunctionalTestResult.Create('Create_IFStrMap'));
    ResultList.Add(TFunctionalTestResult.Create('Create_IStrMap'));
    ResultList.Add(TFunctionalTestResult.Create('Create_ArrArrStr'));

    // ICollection tests
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('Add_Fix'))
    else
        ResultList.Add(TFunctionalTestResult.Create('Add_StrMap'));
    if not FixedSize then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Add_ArrStrMap'));
        ResultList.Add(TFunctionalTestResult.Create('Add_IFStrMap'));
    end;
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('Clear_Fix'))
    else
        ResultList.Add(TFunctionalTestResult.Create('Clear_NonFixStrMap'));
    ResultList.Add(TFunctionalTestResult.Create('Clone_StrMap'));
    ResultList.Add(TFunctionalTestResult.Create('CloneAsStringMap'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('Contains_NonNatStrMap'));
    ResultList.Add(TFunctionalTestResult.Create('Contains_NatStrMap'));
    ResultList.Add(TFunctionalTestResult.Create('Contains_ArrStrMap'));
    ResultList.Add(TFunctionalTestResult.Create('Contains_IFStrMap'));
    ResultList.Add(TFunctionalTestResult.Create('Equals_StrMap'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('Find_FilterNonNatStrMap'));
    ResultList.Add(TFunctionalTestResult.Create('Find_FilterNatStrMap'));
    if not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Find_FuncNonNatStrMap'));
        ResultList.Add(TFunctionalTestResult.Create('FindAll_FilterNonNatStrMap'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('FindAll_FilterNatStrMap'));
    ResultList.Add(TFunctionalTestResult.Create('GetAsArray_StrMap'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('GetComparator_NonNat'));
    ResultList.Add(TFunctionalTestResult.Create('GetComparator_Nat'));
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('GetFixedSize_Fix'))
    else
        ResultList.Add(TFunctionalTestResult.Create('GetFixedSize_NonFix'));
    ResultList.Add(TFunctionalTestResult.Create('GetIgnoreErrors'));
    ResultList.Add(TFunctionalTestResult.Create('GetInstance'));
    ResultList.Add(TFunctionalTestResult.Create('GetIterator_StrMap'));
    if not FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('GetIterator_RemStrMap'));
    if not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('GetIterator_FilterStrMap'));
        ResultList.Add(TFunctionalTestResult.Create('GetIterator_FuncStrMap'));
    end;
    if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IEquatable) then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemIID_Equ'))
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IComparable) then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemIID_Comp'))
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IHashable) then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemIID_Hash'))
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IMappable) then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemIID_Map'))
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IIntegerMappable) then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemIID_IntMap'))
    else if TMiscCollectionLibrary.EqualIID(NaturalItemIID, IStringMappable) then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemIID_StrMap'));
    if AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemsOnly_Nat'))
    else
        ResultList.Add(TFunctionalTestResult.Create('GetNaturalItemsOnly_Both'));
        
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('GetSize_Fix'))
    else
        ResultList.Add(TFunctionalTestResult.Create('GetSize_NonFixStrMap'));
    ResultList.Add(TFunctionalTestResult.Create('IsEmpty_StrMap'));
    ResultList.Add(TFunctionalTestResult.Create('IsNaturalItem_StrMap'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('ItemAllowed_NonNat'));
    ResultList.Add(TFunctionalTestResult.Create('ItemAllowed_NatStrMap'));
    if not AlwaysNaturalItems then
        ResultList.Add(TFunctionalTestResult.Create('ItemCount_NonNatStrMap'));
    ResultList.Add(TFunctionalTestResult.Create('ItemCount_NatStrMap'));
    ResultList.Add(TFunctionalTestResult.Create('ItemCount_ArrStrMap'));
    ResultList.Add(TFunctionalTestResult.Create('ItemCount_IFStrMap'));
    if not AlwaysNaturalItems then
    begin
        ResultList.Add(TFunctionalTestResult.Create('Matching_ArrNatStrMap'));
        ResultList.Add(TFunctionalTestResult.Create('Matching_IFNatStrMap'));
    end;
    if FixedSize then
        ResultList.Add(TFunctionalTestResult.Create('Remove_Fix'))
    else
    begin
        ResultList.Add(TFunctionalTestResult.Create('Remove_StrMap'));
        ResultList.Add(TFunctionalTestResult.Create('RemoveAll_StrMap'));
        ResultList.Add(TFunctionalTestResult.Create('Remove_ArrStrMap'));
        ResultList.Add(TFunctionalTestResult.Create('Remove_IFStrMap'));
        ResultList.Add(TFunctionalTestResult.Create('Retain_ArrStrMap'));
        ResultList.Add(TFunctionalTestResult.Create('Retain_IFStrMap'));
    end;
    ResultList.Add(TFunctionalTestResult.Create('SetComparator'));
    ResultList.Add(TFunctionalTestResult.Create('SetIgnoreErrors'));

    // IStringMap functions
    ResultList.Add(TFunctionalTestResult.Create('ContainsKey_Str'));
    ResultList.Add(TFunctionalTestResult.Create('ContainsKey_ArrStr'));
    ResultList.Add(TFunctionalTestResult.Create('Get_Str'));
    ResultList.Add(TFunctionalTestResult.Create('GetKeys_Str'));
    ResultList.Add(TFunctionalTestResult.Create('GetMapIterator_Str'));
    ResultList.Add(TFunctionalTestResult.Create('GetValues_Str'));
    ResultList.Add(TFunctionalTestResult.Create('Put_ItemStr'));
    ResultList.Add(TFunctionalTestResult.Create('Put_KeyItemStr'));
    ResultList.Add(TFunctionalTestResult.Create('Put_ArrStr'));
    ResultList.Add(TFunctionalTestResult.Create('Put_IFStr'));
    ResultList.Add(TFunctionalTestResult.Create('Put_IStrMap'));
    if not FixedSize then
    begin
        ResultList.Add(TFunctionalTestResult.Create('RemoveKey_KeyStr'));
        ResultList.Add(TFunctionalTestResult.Create('RemoveKey_ArrStr'));
        ResultList.Add(TFunctionalTestResult.Create('RetainKey_ArrStr'));
    end;
end;


function TFunctionalTest.NonNaturalFilter12(const Item: ICollectable): Boolean;
var
    Value1, Value2: String;
    I: Integer;
    Success: Boolean;
begin
    Value1 := '12';
    Value2 := TNonNaturalItem(Item.GetInstance).Name;
    Success := False;
    for I := 1 to Length(Value1) do
    begin
        Success := (Pos(Value1[I], Value2) > 0);
        if Success then
            break;
    end;
    Result := Success;
end;

function TFunctionalTest.NonNaturalFilter34(const Item: ICollectable): Boolean;
var
    Value1, Value2: String;
    I: Integer;
    Success: Boolean;
begin
    Value1 := '34';
    Value2 := TNonNaturalItem(Item.GetInstance).Name;
    Success := False;
    for I := 1 to Length(Value1) do
    begin
        Success := (Pos(Value1[I], Value2) > 0);
        if Success then
            break;
    end;
    Result := Success;
end;

function TFunctionalTest.NaturalFilter12(const Item: ICollectable): Boolean;
var
    Value1, Value2: String;
    I: Integer;
    Success: Boolean;
begin
    Value1 := '12';
    Value2 := (Item as IString).Value;
    Success := False;
    for I := 1 to Length(Value1) do
    begin
        Success := (Pos(Value1[I], Value2) > 0);
        if Success then
            break;
    end;
    Result := Success;
end;

function TFunctionalTest.NaturalFilter34(const Item: ICollectable): Boolean;
var
    Value1, Value2: String;
    I: Integer;
    Success: Boolean;
begin
    Value1 := '34';
    Value2 := (Item as IString).Value;
    Success := False;
    for I := 1 to Length(Value1) do
    begin
        Success := (Pos(Value1[I], Value2) > 0);
        if Success then
            break;
    end;
    Result := Success;
end;

// Functional tests
function TFunctionalTest.Create_Def: Boolean;
var
    Collection: TAbstractCollection;
    Success: Boolean;
begin
    try
        Collection := CollectionClass.Create;
        Collection.Free;
        Success := True;
    except
        Success := False;
    end;
    Result := Success;
end;

function TFunctionalTest.Create_Arr: Boolean;
var
    Collection: TAbstractCollection;
    Success: Boolean;
begin
    try
        Collection := CollectionClass.Create(FNonNaturalArray12);
        Collection.Free;
        Success := True;
    except
        Success := False;
    end;
    Result := Success;
end;

function TFunctionalTest.Create_ArrEqu: Boolean;
var
    Collection: TAbstractCollection;
    Success1, Success2: Boolean;
begin
    try
        // This should fail
        Collection := CollectionClass.Create(FNonNaturalArray12);
        Collection.Free;
        Success1 := False;
    except
        Success1 := True;
    end;

    try
        Collection := CollectionClass.Create(FEquatableArray12);
        Collection.Free;
        Success2 := True;
    except
        Success2 := False;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.Create_ArrComp: Boolean;
var
    Collection: TAbstractCollection;
    Success1, Success2: Boolean;
begin
    try
        // This should fail
        Collection := CollectionClass.Create(FNonNaturalArray12);
        Collection.Free;
        Success1 := False;
    except
        Success1 := True;
    end;

    try
        Collection := CollectionClass.Create(FComparableArray12);
        Collection.Free;
        Success2 := True;
    except
        Success2 := False;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.Create_ArrHash: Boolean;
var
    Collection: TAbstractCollection;
    Success1, Success2: Boolean;
begin
    try
        // This should fail
        Collection := CollectionClass.Create(FNonNaturalArray12);
        Collection.Free;
        Success1 := False;
    except
        Success1 := True;
    end;

    try
        Collection := CollectionClass.Create(FHashableArray12);
        Collection.Free;
        Success2 := True;
    except
        Success2 := False;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.Create_ArrMap: Boolean;
var
    Collection: TAbstractCollection;
    Success1, Success2: Boolean;
begin
    try
        // This should fail
        Collection := CollectionClass.Create(FNonNaturalArray12);
        Collection.Free;
        Success1 := False;
    except
        Success1 := True;
    end;

    try
        Collection := CollectionClass.Create(FNaturalMapItemArray12);
        Collection.Free;
        Success2 := True;
    except
        Success2 := False;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.Create_ArrIntMap: Boolean;
var
    Collection: TAbstractCollection;
    Success1, Success2: Boolean;
begin
    try
        // This should fail
        Collection := CollectionClass.Create(FNonNaturalArray12);
        Collection.Free;
        Success1 := False;
    except
        Success1 := True;
    end;

    try
        Collection := CollectionClass.Create(FIntegerMapItemArray12);
        Collection.Free;
        Success2 := True;
    except
        Success2 := False;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.Create_ArrStrMap: Boolean;
var
    Collection: TAbstractCollection;
    Success1, Success2: Boolean;
begin
    try
        // This should fail
        Collection := CollectionClass.Create(FNonNaturalArray12);
        Collection.Free;
        Success1 := False;
    except
        Success1 := True;
    end;

    try
        Collection := CollectionClass.Create(FStringMapItemArray12);
        Collection.Free;
        Success2 := True;
    except
        Success2 := False;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.Create_IF: Boolean;
var
    Collection: TAbstractCollection;
    Success: Boolean;
begin
    try
        Collection := CollectionClass.Create(FNonNaturalCollection12);
        Collection.Free;
        Success := True;
    except
        Success := False;
    end;
    Result := Success;
end;

function TFunctionalTest.Create_IFEqu: Boolean;
var
    Collection: TAbstractCollection;
    Success1, Success2: Boolean;
begin
    try
        // This should fail
        Collection := CollectionClass.Create(FNonNaturalCollection12);
        Collection.Free;
        Success1 := False;
    except
        Success1 := True;
    end;

    try
        Collection := CollectionClass.Create(FEquatableCollection12);
        Collection.Free;
        Success2 := True;
    except
        Success2 := False;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.Create_IFComp: Boolean;
var
    Collection: TAbstractCollection;
    Success1, Success2: Boolean;
begin
    try
        // This should fail
        Collection := CollectionClass.Create(FNonNaturalCollection12);
        Collection.Free;
        Success1 := False;
    except
        Success1 := True;
    end;

    try
        Collection := CollectionClass.Create(FComparableCollection12);
        Collection.Free;
        Success2 := True;
    except
        Success2 := False;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.Create_IFHash: Boolean;
var
    Collection: TAbstractCollection;
    Success1, Success2: Boolean;
begin
    try
        // This should fail
        Collection := CollectionClass.Create(FNonNaturalCollection12);
        Collection.Free;
        Success1 := False;
    except
        Success1 := True;
    end;

    try
        Collection := CollectionClass.Create(FHashableCollection12);
        Collection.Free;
        Success2 := True;
    except
        Success2 := False;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.Create_IFMap: Boolean;
var
    Collection: TAbstractCollection;
    Success1, Success2: Boolean;
begin
    try
        // This should fail
        Collection := CollectionClass.Create(FNonNaturalCollection12);
        Collection.Free;
        Success1 := False;
    except
        Success1 := True;
    end;

    try
        Collection := CollectionClass.Create(FNaturalItemMap12);
        Collection.Free;
        Success2 := True;
    except
        Success2 := False;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.Create_IFIntMap: Boolean;
var
    Collection: TAbstractCollection;
    Success1, Success2: Boolean;
begin
    try
        // This should fail
        Collection := CollectionClass.Create(FNonNaturalCollection12);
        Collection.Free;
        Success1 := False;
    except
        Success1 := True;
    end;

    try
        Collection := CollectionClass.Create(FNaturalItemIntMap12);
        Collection.Free;
        Success2 := True;
    except
        Success2 := False;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.Create_IFStrMap: Boolean;
var
    Collection: TAbstractCollection;
    Success1, Success2: Boolean;
begin
    try
        // This should fail
        Collection := CollectionClass.Create(FNonNaturalCollection12);
        Collection.Free;
        Success1 := False;
    except
        Success1 := True;
    end;

    try
        Collection := CollectionClass.Create(FNaturalItemStrMap12);
        Collection.Free;
        Success2 := True;
    except
        Success2 := False;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.Create_IMap: Boolean;
var
    Collection: TAbstractMap;
    Success: Boolean;
begin
    try
        Collection := TAbstractMapClass(CollectionClass).Create(FNonNaturalKeyMap12);
        Collection.Free;
        Success := True;
    except
        Success := False;
    end;
    Result := Success;
end;

function TFunctionalTest.Create_IIntMap: Boolean;
var
    Collection: TAbstractIntegerMap;
    Success: Boolean;
begin
    try
        Collection := TAbstractIntegerMapClass(CollectionClass).Create(FNonNaturalItemIntMap12);
        Collection.Free;
        Success := True;
    except
        Success := False;
    end;
    Result := Success;
end;

function TFunctionalTest.Create_IStrMap: Boolean;
var
    Collection: TAbstractStringMap;
    Success: Boolean;
begin
    try
        Collection := TAbstractStringMapClass(CollectionClass).Create(FNonNaturalItemStrMap12);
        Collection.Free;
        Success := True;
    except
        Success := False;
    end;
    Result := Success;
end;

function TFunctionalTest.Create_IMapNat: Boolean;
var
    Collection: TAbstractMap;
    Success1, Success2: Boolean;
begin
    try
        // This should fail
        Collection := TAbstractMapClass(CollectionClass).Create(FNonNaturalKeyMap12);
        Collection.Free;
        Success1 := False;
    except
        Success1 := True;
    end;

    try
        Collection := TAbstractMapClass(CollectionClass).Create(FNaturalKeyMap12);
        Collection.Free;
        Success2 := True;
    except
        Success2 := False;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.Create_ArrArr: Boolean;
var
    Map: TAbstractMap;
    Success1, Success2: Boolean;
begin
    try
        Map := TAbstractMapClass(CollectionClass).Create(FNonNaturalArray12, FNonNaturalArray12, False, False);
        Success1 := not Map.GetNaturalItemsOnly and not Map.GetNaturalKeysOnly;
        Map.Free;
    except
        Success1 := False;
    end;
    try
        Map := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12, False, True);
        Success2 := not Map.GetNaturalItemsOnly and Map.GetNaturalKeysOnly;
        Map.Free;
    except
        Success2 := False;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.Create_ArrArrNat: Boolean;
var
    Map: TAbstractMap;
    Success1, Success2: Boolean;
begin
    try
        // This should fail
        Map := TAbstractMapClass(CollectionClass).Create(FNonNaturalArray12, FNonNaturalArray12, False, False);
        Success1 := False;
        Map.Free;
    except
        Success1 := True;
    end;
    try
        Map := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12, False, True);
        Success2 := not Map.GetNaturalItemsOnly and Map.GetNaturalKeysOnly;
        Map.Free;
    except
        Success2 := False;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.Create_ArrArrInt: Boolean;
var
    Map: TAbstractIntegerMap;
    Success2: Boolean;
begin
    try
        Map := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FNonNaturalArray12, False);
        Success2 := not Map.GetNaturalItemsOnly;
        Map.Free;
    except
        Success2 := False;
    end;
    Result := Success2;
end;

function TFunctionalTest.Create_ArrArrStr: Boolean;
var
    Map: TAbstractStringMap;
    Success2: Boolean;
begin
    try
        Map := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FNonNaturalArray12, False);
        Success2 := not Map.GetNaturalItemsOnly;
        Map.Free;
    except
        Success2 := False;
    end;
    Result := Success2;
end;

function TFunctionalTest.Add_Fix: Boolean;
var
    TestCollection: ICollection;
    Success1, Success2: Boolean;
begin
    try
        TestCollection := CollectionClass.Create(FEquatableCollection12);
        TestCollection.IgnoreErrors := [ceFixedSize];
        Success1 := not TestCollection.Add(FEquatableObject3);
        Success2 := (TestCollection.GetSize = FEquatableCollection12.GetSize);
        Result := Success1 and Success2;
    except
        Result := False;
    end;
end;

function TFunctionalTest.Add_Bag: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    try
        Collection := CollectionClass.Create(True);
        Success1 := Collection.Add(FEquatableObject1);
        Success2 := Collection.Add(FEquatableObject1);
        Success3 := (Collection.GetSize = 2);
        Result := Success1 and Success2 and Success3;
    except
        Result := False;
    end;
end;

function TFunctionalTest.Add_Set: Boolean;
var
    Collection: ISet;
    Success1, Success2, Success3: Boolean;
begin
    try
        Collection := TAbstractSetClass(CollectionClass).Create(True);
        Collection.IgnoreErrors := [ceDuplicate];
        Success1 := Collection.Add(FEquatableObject1);
        Success2 := not Collection.Add(FEquatableObject1);
        Success3 := (Collection.GetSize = 1);
        Result := Success1 and Success2 and Success3;
    except
        Result := False;
    end;
end;

function TFunctionalTest.Add_List: Boolean;
var
    TestList: IList;
    Success1, Success2, Success3, Success4, Success5, Success6: Boolean;
begin
    try
        TestList := TAbstractListClass(CollectionClass).Create(True);
//        TestList.SetIgnoreErrors(True);
        TestList.Add(FComparableObject1);
        Success1 := TestList.Add(FComparableObject3);
        Success2 := (TestList.Last = FComparableObject3);
        Success3 := (TestList.GetSize = 2);
        TestList.SetSorted(True);
        Success4 := TestList.Add(FComparableObject2);
        Success5 := (TestList[1] = FComparableObject2);
        Success6 := (TestList.GetSize = 3);
        Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6;
    except
        Result := False;
    end;
end;

function TFunctionalTest.Add_Map: Boolean;
var
    Collection: IMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    Collection := TAbstractMapClass(CollectionClass).Create(True, False);
    Success1 := False;
    try
        // This should fail
        Collection.Add(FEquatableObject1);
    except
        Success1 := True;
    end;

    Collection.IgnoreErrors := [ceDuplicateKey];
    Success2 := Collection.Add(FNaturalKeyMapObject1);
    Success3 := not Collection.Add(FNaturalKeyMapObject1);
    Success4 := (Collection.GetSize = 1);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Add_IntMap: Boolean;
var
    Collection: IIntegerMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    Collection := TAbstractIntegerMapClass(CollectionClass).Create(True);
    Success1 := False;
    try
        // This should fail
        Collection.Add(FEquatableObject1);
    except
        Success1 := True;
    end;

    Collection.IgnoreErrors := [ceDuplicateKey];
    Success2 := Collection.Add(FIntegerMapObject1);
    Success3 := not Collection.Add(FIntegerMapObject1);
    Success4 := (Collection.GetSize = 1);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Add_StrMap: Boolean;
var
    Collection: IStringMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    Collection := TAbstractStringMapClass(CollectionClass).Create(True);
    Success1 := False;
    try
        // This should fail
        Collection.Add(FEquatableObject1);
    except
        Success1 := True;
    end;

    Collection.IgnoreErrors := [ceDuplicateKey];
    Success2 := Collection.Add(FStringMapObject1);
    Success3 := not Collection.Add(FStringMapObject1);
    Success4 := (Collection.GetSize = 1);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Add_Arr: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    Collection := CollectionClass.Create;
    Success1 := (Collection.Add(FNonNaturalArray12) = 2);
    Success2 := Collection.Contains(FNonNaturalObject1);
    Success3 := Collection.Contains(FNonNaturalObject2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Add_ArrEqu: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Success1 := (Collection.Add(FEquatableArray12) = 2);
    Success2 := Collection.Contains(FEquatableObject1);
    Success3 := Collection.Contains(FEquatableObject2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Add_ArrComp: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Success1 := (Collection.Add(FComparableArray12) = 2);
    Success2 := Collection.Contains(FComparableObject1);
    Success3 := Collection.Contains(FComparableObject2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Add_ArrHash: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Success1 := (Collection.Add(FHashableArray12) = 2);
    Success2 := Collection.Contains(FHashableObject1);
    Success3 := Collection.Contains(FHashableObject2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Add_ArrMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Success1 := (Collection.Add(FNaturalMapItemArray12) = 2);
    Success2 := Collection.Contains(FNaturalKeyMapObject1);
    Success3 := Collection.Contains(FNaturalKeyMapObject2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Add_ArrIntMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Success1 := (Collection.Add(FIntegerMapItemArray12) = 2);
    Success2 := Collection.Contains(FIntegerMapObject1);
    Success3 := Collection.Contains(FIntegerMapObject2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Add_ArrStrMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Success1 := (Collection.Add(FStringMapItemArray12) = 2);
    Success2 := Collection.Contains(FStringMapObject1);
    Success3 := Collection.Contains(FStringMapObject2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Add_IF: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    Collection := CollectionClass.Create;
    Success1 := (Collection.Add(FNonNaturalCollection12) = 2);
    Success2 := Collection.Contains(FNonNaturalObject1);
    Success3 := Collection.Contains(FNonNaturalObject2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Add_IFEqu: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Success1 := (Collection.Add(FEquatableCollection12) = 2);
    Success2 := Collection.Contains(FEquatableObject1);
    Success3 := Collection.Contains(FEquatableObject2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Add_IFComp: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Success1 := (Collection.Add(FComparableCollection12) = 2);
    Success2 := Collection.Contains(FComparableObject1);
    Success3 := Collection.Contains(FComparableObject2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Add_IFHash: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Success1 := (Collection.Add(FHashableCollection12) = 2);
    Success2 := Collection.Contains(FHashableObject1);
    Success3 := Collection.Contains(FHashableObject2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Add_IFMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Success1 := (Collection.Add(FNaturalItemMap12) = 2);
    Success2 := Collection.Contains(FNaturalKeyMapObject1);
    Success3 := Collection.Contains(FNaturalKeyMapObject2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Add_IFIntMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Success1 := (Collection.Add(FNaturalItemIntMap12) = 2);
    Success2 := Collection.Contains(FIntegerMapObject1);
    Success3 := Collection.Contains(FIntegerMapObject2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Add_IFStrMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Success1 := (Collection.Add(FNaturalItemStrMap12) = 2);
    Success2 := Collection.Contains(FStringMapObject1);
    Success3 := Collection.Contains(FStringMapObject2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Clear_NonFix: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(FHashableArray12);
    Success1 := (Collection.Clear = 2);
    Success2 := (Collection.GetSize = 0);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Clear_NonFixMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(FNaturalMapItemArray12);
    Success1 := (Collection.Clear = 2);
    Success2 := (Collection.GetSize = 0);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Clear_NonFixIntMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(FIntegerMapItemArray12);
    Success1 := (Collection.Clear = 2);
    Success2 := (Collection.GetSize = 0);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Clear_NonFixStrMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(FStringMapItemArray12);
    Success1 := (Collection.Clear = 2);
    Success2 := (Collection.GetSize = 0);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Clear_Fix: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(FGeneralArray12);
    Collection.IgnoreErrors := [ceFixedSize];
    Success1 := (Collection.Clear = 0);
    Success2 := (Collection.GetSize = 2);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Clone: Boolean;
var
    Collection, CloneCollection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    Collection := CollectionClass.Create(FGeneralArray12);
    CloneCollection := Collection.Clone;
    if not Collection.FixedSize then
        Collection.Add(FHashableObject3);
    Success1 := CloneCollection.Contains(FGeneralObject1);
    Success2 := CloneCollection.Contains(FGeneralObject2);
    if not not Collection.FixedSize then
        Success3 := not CloneCollection.Contains(FGeneralObject3)
    else
        Success3 := True;
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Clone_Map: Boolean;
var
    TestMap, CloneMap: IMap;
    Success1, Success2, Success3: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12);
    CloneMap := TestMap.Clone as IMap;
    TestMap.Put(FGeneralObject2, FNonNaturalObject3);
    Success1 := CloneMap.ContainsKey(FGeneralObject1);
    Success2 := CloneMap.ContainsKey(FGeneralObject2);
    Success3 := CloneMap.Get(FGeneralObject2) = FNonNaturalObject2;
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Clone_IntMap: Boolean;
var
    TestMap, CloneMap: IIntegerMap;
    Success1, Success2, Success3: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FNonNaturalArray12);
    CloneMap := TestMap.Clone as IIntegerMap;
    TestMap.Put(2, FNonNaturalObject3);
    Success1 := CloneMap.ContainsKey(1);
    Success2 := CloneMap.ContainsKey(2);
    Success3 := CloneMap.Get(2) = FNonNaturalObject2;
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Clone_StrMap: Boolean;
var
    TestMap, CloneMap: IStringMap;
    Success1, Success2, Success3: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FNonNaturalArray12);
    CloneMap := TestMap.Clone as IStringMap;
    TestMap.Put('Item2', FNonNaturalObject3);
    Success1 := CloneMap.ContainsKey('Item1');
    Success2 := CloneMap.ContainsKey('Item2');
    Success3 := CloneMap.Get('Item2') = FNonNaturalObject2;
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.CloneAsBag: Boolean;
var
    Collection, CloneBag: IBag;
    Success1, Success2, Success3: Boolean;
begin
    Collection := TAbstractBagClass(CollectionClass).Create(FGeneralArray12);
    CloneBag := Collection.CloneAsBag;
    if not Collection.FixedSize then
        Collection.Add(FGeneralObject3);
    Success1 := CloneBag.Contains(FGeneralObject1);
    Success2 := CloneBag.Contains(FGeneralObject2);
    if not Collection.FixedSize then
        Success3 := not CloneBag.Contains(FGeneralObject3)
    else
        Success3 := True;
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.CloneAsSet: Boolean;
var
    Collection, CloneSet: ISet;
    Success1, Success2, Success3: Boolean;
begin
    Collection := TAbstractSetClass(CollectionClass).Create(FGeneralArray12);
    CloneSet := Collection.CloneAsSet;
    if not Collection.FixedSize then
        Collection.Add(FGeneralObject3);
    Success1 := CloneSet.Contains(FGeneralObject1);
    Success2 := CloneSet.Contains(FGeneralObject2);
    if not Collection.FixedSize then
        Success3 := not CloneSet.Contains(FGeneralObject3)
    else
        Success3 := True;
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.CloneAsList: Boolean;
var
    TestList, CloneList: IList;
    Success1, Success2, Success3: Boolean;
    Success4, Success5, Success6: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    TestList.Sorted := False;
    CloneList := TestList.CloneAsList;
    TestList[1] := FGeneralObject3;
    Success1 := CloneList.Contains(FGeneralObject1);
    Success2 := CloneList.Contains(FGeneralObject2);
    Success3 := not CloneList.Contains(FGeneralObject3);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    TestList.Sorted := True;
    CloneList := TestList.CloneAsList;
    TestList[1] := FGeneralObject3;
    Success4 := CloneList.Contains(FGeneralObject1);
    Success5 := CloneList.Contains(FGeneralObject2);
    Success6 := not CloneList.Contains(FGeneralObject3);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6;
end;

function TFunctionalTest.CloneAsMap: Boolean;
var
    TestMap, CloneMap: IMap;
    Success1, Success2, Success3: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12);
    CloneMap := TestMap.CloneAsMap;
    TestMap.Put(FGeneralObject2, FNonNaturalObject3);
    Success1 := CloneMap.ContainsKey(FGeneralObject1);
    Success2 := CloneMap.ContainsKey(FGeneralObject2);
    Success3 := CloneMap.Get(FGeneralObject2) = FNonNaturalObject2;
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.CloneAsIntegerMap: Boolean;
var
    TestMap, CloneMap: IIntegerMap;
    Success1, Success2, Success3: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FNonNaturalArray12);
    CloneMap := TestMap.CloneAsIntegerMap;
    TestMap.Put(2, FNonNaturalObject3);
    Success1 := CloneMap.ContainsKey(1);
    Success2 := CloneMap.ContainsKey(2);
    Success3 := CloneMap.Get(2) = FNonNaturalObject2;
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.CloneAsStringMap: Boolean;
var
    TestMap, CloneMap: IStringMap;
    Success1, Success2, Success3: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FNonNaturalArray12);
    CloneMap := TestMap.CloneAsStringMap;
    TestMap.Put('Item2', FNonNaturalObject3);
    Success1 := CloneMap.ContainsKey('Item1');
    Success2 := CloneMap.ContainsKey('Item2');
    Success3 := CloneMap.Get('Item2') = FNonNaturalObject2;
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Contains_NonNat: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(FNonNaturalArray12);
    Success1 := Collection.Contains(FNonNaturalObject1);
    Success2 := not Collection.Contains(FNonNaturalObject3);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Contains_NonNatList: Boolean;
var
    TestList: IList;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray12);
    TestList.Sorted := False;
    Success1 := TestList.Contains(FNonNaturalObject1);
    Success2 := not TestList.Contains(FNonNaturalObject3);
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray12);
    TestList.Sorted := True;
    Success3 := TestList.Contains(FNonNaturalObject1);
    Success4 := not TestList.Contains(FNonNaturalObject3);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Contains_NonNatMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12, False, True);
    Success1 := Collection.Contains(FNonNaturalObject1);
    Success2 := not Collection.Contains(FNonNaturalObject3);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Contains_NonNatIntMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FNonNaturalArray12, False);
    Success1 := Collection.Contains(FNonNaturalObject1);
    Success2 := not Collection.Contains(FNonNaturalObject3);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Contains_NonNatStrMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FNonNaturalArray12, False);
    Success1 := Collection.Contains(FNonNaturalObject1);
    Success2 := not Collection.Contains(FNonNaturalObject3);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Contains_Nat: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(FGeneralArray12);
    Success1 := Collection.Contains(FGeneralObject1);
    Success2 := not Collection.Contains(FGeneralObject3);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Contains_NatList: Boolean;
var
    TestList: IList;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    TestList.Sorted := False;
    Success1 := TestList.Contains(FGeneralObject1);
    Success2 := not TestList.Contains(FGeneralObject3);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    TestList.Sorted := True;
    Success3 := TestList.Contains(FGeneralObject1);
    Success4 := not TestList.Contains(FGeneralObject3);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Contains_NatMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(FNaturalMapItemArray12, True);
    Success1 := Collection.Contains(FNaturalKeyMapObject1);
    Success2 := not Collection.Contains(FNaturalKeyMapObject3);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Contains_NatIntMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(FIntegerMapItemArray12, True);
    Success1 := Collection.Contains(FIntegerMapObject1);
    Success2 := not Collection.Contains(FIntegerMapObject3);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Contains_NatStrMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(FStringMapItemArray12, True);
    Success1 := Collection.Contains(FStringMapObject1);
    Success2 := not Collection.Contains(FStringMapObject3);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Contains_Arr: Boolean;
var
    Collection: ICollection;
    Array1, Array2, Array3: TCollectableArray;
    Success1, Success2, Success3, Success4: Boolean;
begin
    SetLength(Array1, 0);
    SetLength(Array2, 2);
    Array2[0] := FGeneralObject3;
    Array2[1] := FGeneralObject4;
    SetLength(Array3, 2);
    Array3[0] := FGeneralObject2;
    Array3[1] := FGeneralObject3;
    Collection := CollectionClass.Create(FGeneralArray12);
    Success1 := Collection.Contains(Array1);
    Success2 := not Collection.Contains(Array2);
    Success3 := not Collection.Contains(Array3);
    Success4 := Collection.Contains(FGeneralArray12);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Contains_ArrList: Boolean;
var
    TestList: IList;
    Array1, Array2, Array3: TCollectableArray;
    Success1, Success2, Success3, Success4: Boolean;
    Success5, Success6, Success7, Success8: Boolean;
begin
    SetLength(Array1, 0);
    SetLength(Array2, 2);
    Array2[0] := FGeneralObject3;
    Array2[1] := FGeneralObject4;
    SetLength(Array3, 2);
    Array3[0] := FGeneralObject2;
    Array3[1] := FGeneralObject3;
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    TestList.Sorted := False;
    Success1 := TestList.Contains(Array1);
    Success2 := not TestList.Contains(Array2);
    Success3 := not TestList.Contains(Array3);
    Success4 := TestList.Contains(FGeneralArray12);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    TestList.Sorted := True;
    Success5 := TestList.Contains(Array1);
    Success6 := not TestList.Contains(Array2);
    Success7 := not TestList.Contains(Array3);
    Success8 := TestList.Contains(FGeneralArray12);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6 and Success7 and Success8;
end;

function TFunctionalTest.Contains_ArrMap: Boolean;
var
    TestMap: IMap;
    Array1, Array2, Array3: TCollectableArray;
    Success1, Success2, Success3, Success4: Boolean;
begin
    SetLength(Array1, 0);
    SetLength(Array2, 2);
    Array2[0] := FNonNaturalObject3;
    Array2[1] := FNonNaturalObject4;
    SetLength(Array3, 2);
    Array3[0] := FNonNaturalObject2;
    Array3[1] := FNonNaturalObject3;
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12);
    Success1 := TestMap.Contains(Array1);
    Success2 := not TestMap.Contains(Array2);
    Success3 := not TestMap.Contains(Array3);
    Success4 := TestMap.Contains(FNonNaturalArray12);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Contains_ArrIntMap: Boolean;
var
    TestMap: IIntegerMap;
    Array1, Array2, Array3: TCollectableArray;
    Success1, Success2, Success3, Success4: Boolean;
begin
    SetLength(Array1, 0);
    SetLength(Array2, 2);
    Array2[0] := FNonNaturalObject3;
    Array2[1] := FNonNaturalObject4;
    SetLength(Array3, 2);
    Array3[0] := FNonNaturalObject2;
    Array3[1] := FNonNaturalObject3;
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FNonNaturalArray12);
    Success1 := TestMap.Contains(Array1);
    Success2 := not TestMap.Contains(Array2);
    Success3 := not TestMap.Contains(Array3);
    Success4 := TestMap.Contains(FNonNaturalArray12);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Contains_ArrStrMap: Boolean;
var
    TestMap: IStringMap;
    Array1, Array2, Array3: TCollectableArray;
    Success1, Success2, Success3, Success4: Boolean;
begin
    SetLength(Array1, 0);
    SetLength(Array2, 2);
    Array2[0] := FNonNaturalObject3;
    Array2[1] := FNonNaturalObject4;
    SetLength(Array3, 2);
    Array3[0] := FNonNaturalObject2;
    Array3[1] := FNonNaturalObject3;
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FNonNaturalArray12);
    Success1 := TestMap.Contains(Array1);
    Success2 := not TestMap.Contains(Array2);
    Success3 := not TestMap.Contains(Array3);
    Success4 := TestMap.Contains(FNonNaturalArray12);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Contains_IF: Boolean;
var
    Collection: ICollection;
    Collection1, Collection2, Collection3: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    Collection1 := TPArrayBag.Create(True);
    Collection2 := TPArrayBag.Create(True);
    Collection2.Add(FGeneralObject3);
    Collection2.Add(FGeneralObject4);
    Collection3 := TPArrayBag.Create(True);
    Collection3.Add(FGeneralObject2);
    Collection3.Add(FGeneralObject3);
    Collection := CollectionClass.Create(FGeneralCollection12);
    Success1 := Collection.Contains(Collection1);
    Success2 := not Collection.Contains(Collection2);
    Success3 := not Collection.Contains(Collection3);
    Success4 := Collection.Contains(FGeneralCollection12);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Contains_IFList: Boolean;
var
    TestList: IList;
    Collection1, Collection2, Collection3: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
    Success5, Success6, Success7, Success8: Boolean;
begin
    Collection1 := TPArrayBag.Create(True);
    Collection2 := TPArrayBag.Create(True);
    Collection2.Add(FGeneralObject3);
    Collection2.Add(FGeneralObject4);
    Collection3 := TPArrayBag.Create(True);
    Collection3.Add(FGeneralObject2);
    Collection3.Add(FGeneralObject3);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralCollection12);
    TestList.Sorted := False;
    Success1 := TestList.Contains(Collection1);
    Success2 := not TestList.Contains(Collection2);
    Success3 := not TestList.Contains(Collection3);
    Success4 := TestList.Contains(FGeneralCollection12);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralCollection12);
    TestList.Sorted := True;
    Success5 := TestList.Contains(Collection1);
    Success6 := not TestList.Contains(Collection2);
    Success7 := not TestList.Contains(Collection3);
    Success8 := TestList.Contains(FGeneralCollection12);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6 and Success7 and Success8;
end;

function TFunctionalTest.Contains_IFMap: Boolean;
var
    Collection: ICollection;
    Collection1, Collection2, Collection3: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    Collection1 := TPArrayBag.Create(True);
    Collection2 := TPArrayBag.Create(True);
    Collection2.Add(FNaturalKeyMapObject3);
    Collection2.Add(FNaturalKeyMapObject4);
    Collection3 := TPArrayBag.Create(True);
    Collection3.Add(FNaturalKeyMapObject2);
    Collection3.Add(FNaturalKeyMapObject3);
    Collection := CollectionClass.Create(FNaturalItemMap12);
    Success1 := Collection.Contains(Collection1);
    Success2 := not Collection.Contains(Collection2);
    Success3 := not Collection.Contains(Collection3);
    Success4 := Collection.Contains(FNaturalItemMap12);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Contains_IFIntMap: Boolean;
var
    Collection: ICollection;
    Collection1, Collection2, Collection3: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    Collection1 := TPArrayBag.Create(True);
    Collection2 := TPArrayBag.Create(True);
    Collection2.Add(FIntegerMapObject3);
    Collection2.Add(FIntegerMapObject4);
    Collection3 := TPArrayBag.Create(True);
    Collection3.Add(FIntegerMapObject2);
    Collection3.Add(FIntegerMapObject3);
    Collection := CollectionClass.Create(FNaturalItemIntMap12);
    Success1 := Collection.Contains(Collection1);
    Success2 := not Collection.Contains(Collection2);
    Success3 := not Collection.Contains(Collection3);
    Success4 := Collection.Contains(FNaturalItemIntMap12);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Contains_IFStrMap: Boolean;
var
    Collection: ICollection;
    Collection1, Collection2, Collection3: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    Collection1 := TPArrayBag.Create(True);
    Collection2 := TPArrayBag.Create(True);
    Collection2.Add(FStringMapObject3);
    Collection2.Add(FStringMapObject4);
    Collection3 := TPArrayBag.Create(True);
    Collection3.Add(FStringMapObject2);
    Collection3.Add(FStringMapObject3);
    Collection := CollectionClass.Create(FNaturalItemStrMap12);
    Success1 := Collection.Contains(Collection1);
    Success2 := not Collection.Contains(Collection2);
    Success3 := not Collection.Contains(Collection3);
    Success4 := Collection.Contains(FNaturalItemStrMap12);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Equals_Bag: Boolean;
var
    TestBag: IBag;
    Bag1, Bag2, Bag3: IBag;
    Set1: ISet;
    Success1, Success2, Success3, Success4, Success5, Success6: Boolean;
begin
    Bag1 := TAbstractBagClass(CollectionClass).Create;
    Bag2 := TAbstractBagClass(CollectionClass).Create(FGeneralArray12);
    Bag3 := TAbstractBagClass(CollectionClass).Create(FGeneralArray23);
    Set1 := TPArraySet.Create(FGeneralArray12);
    TestBag := TAbstractBagClass(CollectionClass).Create(FGeneralArray12);
    Success1 := not TestBag.Equals(Set1);
    TestBag := TAbstractBagClass(CollectionClass).Create;
    Success2 := TestBag.Equals(Bag1);
    Success3 := not TestBag.Equals(Bag2);
    TestBag := TAbstractBagClass(CollectionClass).Create(FGeneralArray12);
    Success4 := not TestBag.Equals(Bag1);
    Success5 := not TestBag.Equals(Bag3);
    Success6 := TestBag.Equals(Bag2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6;
end;

function TFunctionalTest.Equals_Set: Boolean;
var
    TestSet: ISet;
    Set1, Set2, Set3: ISet;
    Bag1: Ibag;
    Success1, Success2, Success3, Success4, Success5, Success6: Boolean;
begin
    Set1 := TAbstractSetClass(CollectionClass).Create;
    Set2 := TAbstractSetClass(CollectionClass).Create(FGeneralArray12);
    Set3 := TAbstractSetClass(CollectionClass).Create(FGeneralArray23);
    Bag1 := TPArrayBag.Create(FGeneralArray12);
    TestSet := TAbstractSetClass(CollectionClass).Create(FGeneralArray12);
    Success1 := not TestSet.Equals(Bag1);
    TestSet := TAbstractSetClass(CollectionClass).Create;
    Success2 := TestSet.Equals(Set1);
    Success3 := not TestSet.Equals(Set2);
    TestSet := TAbstractSetClass(CollectionClass).Create(FGeneralArray12);
    Success4 := not TestSet.Equals(Set1);
    Success5 := not TestSet.Equals(Set3);
    Success6 := TestSet.Equals(Set2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6;
end;

function TFunctionalTest.Equals_List: Boolean;
var
    TestList: IList;
    List1, List2, List3: IList;
    Set1: ISet;
    Success1, Success2, Success3, Success4, Success5, Success6: Boolean;
begin
    List1 := TAbstractListClass(CollectionClass).Create;
    List2 := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    List3 := TAbstractListClass(CollectionClass).Create(FGeneralArray23);
    Set1 := TPArraySet.Create(FGeneralArray12);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    Success1 := not TestList.Equals(Set1);
    TestList := TAbstractListClass(CollectionClass).Create;
    Success2 := TestList.Equals(List1);
    Success3 := not TestList.Equals(List2);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    Success4 := not TestList.Equals(List1);
    Success5 := not TestList.Equals(List3);
    Success6 := TestList.Equals(List2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6;
end;

function TFunctionalTest.Equals_Map: Boolean;
var
    TestMap: IMap;
    Map1, Map2, Map3: IMap;
    Set1: ISet;
    Success1, Success2, Success3, Success4, Success5, Success6: Boolean;
begin
    Map1 := TAbstractMapClass(CollectionClass).Create;
    Map2 := TAbstractMapClass(CollectionClass).Create(FNaturalMapItemArray12);
    Map3 := TAbstractMapClass(CollectionClass).Create(FNaturalMapItemArray23);
    Set1 := TPArraySet.Create(FNaturalMapItemArray12);
    TestMap := TAbstractMapClass(CollectionClass).Create(FNaturalMapItemArray12);
    Success1 := not TestMap.Equals(Set1);
    TestMap := TAbstractMapClass(CollectionClass).Create;
    Success2 := TestMap.Equals(Map1);
    Success3 := not TestMap.Equals(Map2);
    TestMap := TAbstractMapClass(CollectionClass).Create(FNaturalMapItemArray12);
    Success4 := not TestMap.Equals(Map1);
    Success5 := not TestMap.Equals(Map3);
    Success6 := TestMap.Equals(Map2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6;
end;

function TFunctionalTest.Equals_IntMap: Boolean;
var
    TestMap: IIntegerMap;
    Map1, Map2, Map3: IIntegerMap;
    Set1: ISet;
    Success1, Success2, Success3, Success4, Success5, Success6: Boolean;
begin
    Map1 := TAbstractIntegerMapClass(CollectionClass).Create;
    Map2 := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerMapItemArray12);
    Map3 := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerMapItemArray23);
    Set1 := TPArraySet.Create(FNaturalMapItemArray12);
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerMapItemArray12);
    Success1 := not TestMap.Equals(Set1);
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create;
    Success2 := TestMap.Equals(Map1);
    Success3 := not TestMap.Equals(Map2);
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerMapItemArray12);
    Success4 := not TestMap.Equals(Map1);
    Success5 := not TestMap.Equals(Map3);
    Success6 := TestMap.Equals(Map2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6;
end;

function TFunctionalTest.Equals_StrMap: Boolean;
var
    TestMap: IStringMap;
    Map1, Map2, Map3: IStringMap;
    Set1: ISet;
    Success1, Success2, Success3, Success4, Success5, Success6: Boolean;
begin
    Map1 := TAbstractStringMapClass(CollectionClass).Create;
    Map2 := TAbstractStringMapClass(CollectionClass).Create(FStringMapItemArray12);
    Map3 := TAbstractStringMapClass(CollectionClass).Create(FStringMapItemArray23);
    Set1 := TPArraySet.Create(FNaturalMapItemArray12);
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringMapItemArray12);
    Success1 := not TestMap.Equals(Set1);
    TestMap := TAbstractStringMapClass(CollectionClass).Create;
    Success2 := TestMap.Equals(Map1);
    Success3 := not TestMap.Equals(Map2);
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringMapItemArray12);
    Success4 := not TestMap.Equals(Map1);
    Success5 := not TestMap.Equals(Map3);
    Success6 := TestMap.Equals(Map2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6;
end;

function TFunctionalTest.Find_FilterNonNat: Boolean;
var
    TestCollection: ICollection;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
begin
    TestFilter12 := TTestFilter_NonNat.Create('12');
    TestFilter34 := TTestFilter_NonNat.Create('34');
    TestCollection := CollectionClass.Create;
    Success1 := (TestCollection.Find(TestFilter12) = nil);
    TestCollection := CollectionClass.Create(FNonNaturalArray12);
    Success2 := (TestCollection.Find(TestFilter34) = nil);
    Success3 := (TestCollection.Find(TestFilter12) <> nil);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Find_FilterNonNatList: Boolean;
var
    TestList: IList;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
    Success4, Success5, Success6: Boolean;
begin
    TestFilter12 := TTestFilter_NonNat.Create('12');
    TestFilter34 := TTestFilter_NonNat.Create('34');
    TestList := TAbstractListClass(CollectionClass).Create;
    TestList.Sorted := False;
    Success1 := (TestList.Find(TestFilter12) = nil);
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray12);
    TestList.Sorted := False;
    Success2 := (TestList.Find(TestFilter34) = nil);
    Success3 := (TestList.Find(TestFilter12) <> nil);
    TestList := TAbstractListClass(CollectionClass).Create;
    TestList.Sorted := True;
    Success4 := (TestList.Find(TestFilter12) = nil);
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray12);
    TestList.Sorted := True;
    Success5 := (TestList.Find(TestFilter34) = nil);
    Success6 := (TestList.Find(TestFilter12) <> nil);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6;
end;

function TFunctionalTest.Find_FilterNonNatMap: Boolean;
var
    TestMap: IMap;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
begin
    TestFilter12 := TTestFilter_NonNat.Create('12');
    TestFilter34 := TTestFilter_NonNat.Create('34');
    TestMap := TAbstractMapClass(CollectionClass).Create;
    Success1 := (TestMap.Find(TestFilter12) = nil);
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12, False);
    Success2 := (TestMap.Find(TestFilter34) = nil);
    Success3 := (TestMap.Find(TestFilter12) <> nil);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Find_FilterNonNatIntMap: Boolean;
var
    TestMap: IIntegerMap;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
begin
    TestFilter12 := TTestFilter_NonNat.Create('12');
    TestFilter34 := TTestFilter_NonNat.Create('34');
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create;
    Success1 := (TestMap.Find(TestFilter12) = nil);
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FNonNaturalArray12, False);
    Success2 := (TestMap.Find(TestFilter34) = nil);
    Success3 := (TestMap.Find(TestFilter12) <> nil);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Find_FilterNonNatStrMap: Boolean;
var
    TestMap: IStringMap;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
begin
    TestFilter12 := TTestFilter_NonNat.Create('12');
    TestFilter34 := TTestFilter_NonNat.Create('34');
    TestMap := TAbstractStringMapClass(CollectionClass).Create;
    Success1 := (TestMap.Find(TestFilter12) = nil);
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FNonNaturalArray12, False);
    Success2 := (TestMap.Find(TestFilter34) = nil);
    Success3 := (TestMap.Find(TestFilter12) <> nil);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Find_FilterNat: Boolean;
var
    TestCollection: ICollection;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
begin
    TestFilter12 := TTestFilter_Nat.Create('12');
    TestFilter34 := TTestFilter_Nat.Create('34');
    TestCollection := CollectionClass.Create;
    Success1 := (TestCollection.Find(TestFilter12) = nil);
    TestCollection := CollectionClass.Create(FGeneralArray12);
    Success2 := (TestCollection.Find(TestFilter34) = nil);
    Success3 := (TestCollection.Find(TestFilter12) <> nil);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Find_FilterNatList: Boolean;
var
    TestList: IList;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
    Success4, Success5, Success6: Boolean;
begin
    TestFilter12 := TTestFilter_Nat.Create('12');
    TestFilter34 := TTestFilter_Nat.Create('34');
    TestList := TAbstractListClass(CollectionClass).Create;
    TestList.Sorted := False;
    Success1 := (TestList.Find(TestFilter12) = nil);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    TestList.Sorted := False;
    Success2 := (TestList.Find(TestFilter34) = nil);
    Success3 := (TestList.Find(TestFilter12) <> nil);
    TestList := TAbstractListClass(CollectionClass).Create;
    TestList.Sorted := True;
    Success4 := (TestList.Find(TestFilter12) = nil);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    TestList.Sorted := True;
    Success5 := (TestList.Find(TestFilter34) = nil);
    Success6 := (TestList.Find(TestFilter12) <> nil);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6;
end;

function TFunctionalTest.Find_FilterNatMap: Boolean;
var
    TestMap: IMap;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
begin
    TestFilter12 := TTestFilter_NatMap.Create('12');
    TestFilter34 := TTestFilter_NatMap.Create('34');
    TestMap := TAbstractMapClass(CollectionClass).Create(True);
    Success1 := (TestMap.Find(TestFilter12) = nil);
    TestMap := TAbstractMapClass(CollectionClass).Create(FNaturalMapItemArray12);
    Success2 := (TestMap.Find(TestFilter34) = nil);
    Success3 := (TestMap.Find(TestFilter12) <> nil);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Find_FilterNatIntMap: Boolean;
var
    TestMap: IIntegerMap;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
begin
    TestFilter12 := TTestFilter_NatIntMap.Create('12');
    TestFilter34 := TTestFilter_NatIntMap.Create('34');
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(True);
    Success1 := (TestMap.Find(TestFilter12) = nil);
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerMapItemArray12);
    Success2 := (TestMap.Find(TestFilter34) = nil);
    Success3 := (TestMap.Find(TestFilter12) <> nil);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Find_FilterNatStrMap: Boolean;
var
    TestMap: IStringMap;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
begin
    TestFilter12 := TTestFilter_NatStrMap.Create('12');
    TestFilter34 := TTestFilter_NatStrMap.Create('34');
    TestMap := TAbstractStringMapClass(CollectionClass).Create(True);
    Success1 := (TestMap.Find(TestFilter12) = nil);
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringMapItemArray12);
    Success2 := (TestMap.Find(TestFilter34) = nil);
    Success3 := (TestMap.Find(TestFilter12) <> nil);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Find_FuncNonNat: Boolean;
var
    TestCollection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    TestCollection := CollectionClass.Create;
    Success1 := (TestCollection.Find(NonNaturalFilter12) = nil);
    TestCollection := CollectionClass.Create(FNonNaturalArray12);
    Success2 := (TestCollection.Find(NonNaturalFilter34) = nil);
    Success3 := (TestCollection.Find(NonNaturalFilter12) <> nil);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Find_FuncNonNatList: Boolean;
var
    TestList: IList;
    Success1, Success2, Success3: Boolean;
    Success4, Success5, Success6: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create;
    TestList.Sorted := False;
    Success1 := (TestList.Find(NonNaturalFilter12) = nil);
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray12);
    TestList.Sorted := False;
    Success2 := (TestList.Find(NonNaturalFilter34) = nil);
    Success3 := (TestList.Find(NonNaturalFilter12) <> nil);
    TestList := TAbstractListClass(CollectionClass).Create;
    TestList.Sorted := True;
    Success4 := (TestList.Find(NonNaturalFilter12) = nil);
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray12);
    TestList.Sorted := True;
    Success5 := (TestList.Find(NonNaturalFilter34) = nil);
    Success6 := (TestList.Find(NonNaturalFilter12) <> nil);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6;
end;

function TFunctionalTest.Find_FuncNonNatMap: Boolean;
var
    TestMap: IMap;
    Success1, Success2, Success3: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create;
    Success1 := (TestMap.Find(NonNaturalFilter12) = nil);
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12, False);
    Success2 := (TestMap.Find(NonNaturalFilter34) = nil);
    Success3 := (TestMap.Find(NonNaturalFilter12) <> nil);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Find_FuncNonNatIntMap: Boolean;
var
    TestMap: IIntegerMap;
    Success1, Success2, Success3: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create;
    Success1 := (TestMap.Find(NonNaturalFilter12) = nil);
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FNonNaturalArray12, False);
    Success2 := (TestMap.Find(NonNaturalFilter34) = nil);
    Success3 := (TestMap.Find(NonNaturalFilter12) <> nil);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Find_FuncNonNatStrMap: Boolean;
var
    TestMap: IStringMap;
    Success1, Success2, Success3: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create;
    Success1 := (TestMap.Find(NonNaturalFilter12) = nil);
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FNonNaturalArray12, False);
    Success2 := (TestMap.Find(NonNaturalFilter34) = nil);
    Success3 := (TestMap.Find(NonNaturalFilter12) <> nil);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.FindAll_FilterNonNat: Boolean;
var
    TestCollection: ICollection;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
begin
    TestFilter12 := TTestFilter_NonNat.Create('12');
    TestFilter34 := TTestFilter_NonNat.Create('34');
    TestCollection := CollectionClass.Create;
    Success1 := (TestCollection.FindAll(TestFilter12).GetSize = 0);
    TestCollection := CollectionClass.Create(FNonNaturalArray12);
    Success2 := (TestCollection.FindAll(TestFilter34).GetSize = 0);
    Success3 := (TestCollection.FindAll(TestFilter12).GetSize <> 0);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.FindAll_FilterNonNatList: Boolean;
var
    TestList: IList;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
    Success4, Success5, Success6: Boolean;
begin
    TestFilter12 := TTestFilter_NonNat.Create('12');
    TestFilter34 := TTestFilter_NonNat.Create('34');
    TestList := TAbstractListClass(CollectionClass).Create;
    TestList.Sorted := False;
    Success1 := (TestList.FindAll(TestFilter12).GetSize = 0);
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray12);
    TestList.Sorted := False;
    Success2 := (TestList.FindAll(TestFilter34).GetSize = 0);
    Success3 := (TestList.FindAll(TestFilter12).GetSize <> 0);
    TestList := TAbstractListClass(CollectionClass).Create;
    TestList.Sorted := True;
    Success4 := (TestList.FindAll(TestFilter12).GetSize = 0);
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray12);
    TestList.Sorted := True;
    Success5 := (TestList.FindAll(TestFilter34).GetSize = 0);
    Success6 := (TestList.FindAll(TestFilter12).GetSize <> 0);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6;
end;

function TFunctionalTest.FindAll_FilterNonNatMap: Boolean;
var
    TestMap: IMap;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
begin
    TestFilter12 := TTestFilter_NonNat.Create('12');
    TestFilter34 := TTestFilter_NonNat.Create('34');
    TestMap := TAbstractMapClass(CollectionClass).Create;
    Success1 := (TestMap.FindAll(TestFilter12).GetSize = 0);
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12, False);
    Success2 := (TestMap.FindAll(TestFilter34).GetSize = 0);
    Success3 := (TestMap.FindAll(TestFilter12).GetSize <> 0);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.FindAll_FilterNonNatIntMap: Boolean;
var
    TestMap: IIntegerMap;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
begin
    TestFilter12 := TTestFilter_NonNat.Create('12');
    TestFilter34 := TTestFilter_NonNat.Create('34');
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create;
    Success1 := (TestMap.FindAll(TestFilter12).GetSize = 0);
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FNonNaturalArray12, False);
    Success2 := (TestMap.FindAll(TestFilter34).GetSize = 0);
    Success3 := (TestMap.FindAll(TestFilter12).GetSize <> 0);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.FindAll_FilterNonNatStrMap: Boolean;
var
    TestMap: IStringMap;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
begin
    TestFilter12 := TTestFilter_NonNat.Create('12');
    TestFilter34 := TTestFilter_NonNat.Create('34');
    TestMap := TAbstractStringMapClass(CollectionClass).Create;
    Success1 := (TestMap.FindAll(TestFilter12).GetSize = 0);
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FNonNaturalArray12, False);
    Success2 := (TestMap.FindAll(TestFilter34).GetSize = 0);
    Success3 := (TestMap.FindAll(TestFilter12).GetSize <> 0);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.FindAll_FilterNat: Boolean;
var
    TestCollection: ICollection;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
begin
    TestFilter12 := TTestFilter_Nat.Create('12');
    TestFilter34 := TTestFilter_Nat.Create('34');
    TestCollection := CollectionClass.Create(True);
    Success1 := (TestCollection.FindAll(TestFilter12).GetSize = 0);
    TestCollection := CollectionClass.Create(FGeneralArray12, True);
    Success2 := (TestCollection.FindAll(TestFilter34).GetSize = 0);
    Success3 := (TestCollection.FindAll(TestFilter12).GetSize <> 0);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.FindAll_FilterNatList: Boolean;
var
    TestList: IList;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
    Success4, Success5, Success6: Boolean;
begin
    TestFilter12 := TTestFilter_Nat.Create('12');
    TestFilter34 := TTestFilter_Nat.Create('34');
    TestList := TAbstractListClass(CollectionClass).Create(True);
    TestList.Sorted := False;
    Success1 := (TestList.FindAll(TestFilter12).GetSize = 0);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12, True);
    TestList.Sorted := False;
    Success2 := (TestList.FindAll(TestFilter34).GetSize = 0);
    Success3 := (TestList.FindAll(TestFilter12).GetSize <> 0);
    TestList := TAbstractListClass(CollectionClass).Create(True);
    TestList.Sorted := True;
    Success4 := (TestList.FindAll(TestFilter12).GetSize = 0);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12, True);
    TestList.Sorted := True;
    Success5 := (TestList.FindAll(TestFilter34).GetSize = 0);
    Success6 := (TestList.FindAll(TestFilter12).GetSize <> 0);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6;
end;

function TFunctionalTest.FindAll_FilterNatMap: Boolean;
var
    TestMap: IMap;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
begin
    TestFilter12 := TTestFilter_NatMap.Create('12');
    TestFilter34 := TTestFilter_NatMap.Create('34');
    TestMap := TAbstractMapClass(CollectionClass).Create(True);
    Success1 := (TestMap.FindAll(TestFilter12).GetSize = 0);
    TestMap := TAbstractMapClass(CollectionClass).Create(FNaturalMapItemArray12, True);
    Success2 := (TestMap.FindAll(TestFilter34).GetSize = 0);
    Success3 := (TestMap.FindAll(TestFilter12).GetSize <> 0);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.FindAll_FilterNatIntMap: Boolean;
var
    TestMap: IIntegerMap;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
begin
    TestFilter12 := TTestFilter_NatIntMap.Create('12');
    TestFilter34 := TTestFilter_NatIntMap.Create('34');
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(True);
    Success1 := (TestMap.FindAll(TestFilter12).GetSize = 0);
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerMapItemArray12, True);
    Success2 := (TestMap.FindAll(TestFilter34).GetSize = 0);
    Success3 := (TestMap.FindAll(TestFilter12).GetSize <> 0);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.FindAll_FilterNatStrMap: Boolean;
var
    TestMap: IStringMap;
    TestFilter12, TestFilter34: IFilter;
    Success1, Success2, Success3: Boolean;
begin
    TestFilter12 := TTestFilter_NatStrMap.Create('12');
    TestFilter34 := TTestFilter_NatStrMap.Create('34');
    TestMap := TAbstractStringMapClass(CollectionClass).Create(True);
    Success1 := (TestMap.FindAll(TestFilter12).GetSize = 0);
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringMapItemArray12, True);
    Success2 := (TestMap.FindAll(TestFilter34).GetSize = 0);
    Success3 := (TestMap.FindAll(TestFilter12).GetSize <> 0);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.GetAsArray: Boolean;
var
    Collection: ICollection;
    Array1: TCollectableArray;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(FGeneralArray12);
    Array1 := Collection.GetAsArray;
    Success1 := ((Array1[0] = FGeneralObject1) or (Array1[1] = FGeneralObject1));
    Success2 := ((Array1[0] = FGeneralObject2) or (Array1[1] = FGeneralObject2));
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetAsArrayList: Boolean;
var
    TestList: IList;
    Array1: TCollectableArray;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    TestList.Sorted := False;
    Array1 := TestList.GetAsArray;
    Success1 := ((Array1[0] = FGeneralObject1) or (Array1[1] = FGeneralObject1));
    Success2 := ((Array1[0] = FGeneralObject2) or (Array1[1] = FGeneralObject2));
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    TestList.Sorted := True;
    Array1 := TestList.GetAsArray;
    Success3 := ((Array1[0] = FGeneralObject1) or (Array1[1] = FGeneralObject1));
    Success4 := ((Array1[0] = FGeneralObject2) or (Array1[1] = FGeneralObject2));
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.GetAsArray_Map: Boolean;
var
    Collection: ICollection;
    Array1: TCollectableArray;
    Success1, Success2: Boolean;
begin
    Collection := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12);
    Array1 := Collection.GetAsArray;
    Success1 := ((Array1[0] = FNonNaturalObject1) or (Array1[1] = FNonNaturalObject1));
    Success2 := ((Array1[0] = FNonNaturalObject2) or (Array1[1] = FNonNaturalObject2));
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetAsArray_IntMap: Boolean;
var
    Collection: ICollection;
    Array1: TCollectableArray;
    Success1, Success2: Boolean;
begin
    Collection := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FNonNaturalArray12);
    Array1 := Collection.GetAsArray;
    Success1 := ((Array1[0] = FNonNaturalObject1) or (Array1[1] = FNonNaturalObject1));
    Success2 := ((Array1[0] = FNonNaturalObject2) or (Array1[1] = FNonNaturalObject2));
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetAsArray_StrMap: Boolean;
var
    Collection: ICollection;
    Array1: TCollectableArray;
    Success1, Success2: Boolean;
begin
    Collection := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FNonNaturalArray12);
    Array1 := Collection.GetAsArray;
    Success1 := ((Array1[0] = FNonNaturalObject1) or (Array1[1] = FNonNaturalObject1));
    Success2 := ((Array1[0] = FNonNaturalObject2) or (Array1[1] = FNonNaturalObject2));
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetComparator_NonNat: Boolean;
var
    Collection: ICollection;
begin
    Collection := CollectionClass.Create(False);
    Result := (Collection.GetComparator = TAbstractComparator.GetDefaultComparator);
end;

function TFunctionalTest.GetComparator_Nat: Boolean;
var
    Collection: ICollection;
begin
    Collection := CollectionClass.Create(True);
    Result := (Collection.GetComparator = TAbstractComparator.GetNaturalComparator);
end;

function TFunctionalTest.GetFixedSize_NonFix: Boolean;
var
    Collection: ICollection;
begin
    Collection := CollectionClass.Create(True);
    Result := (Collection.GetFixedSize = False);
end;

function TFunctionalTest.GetFixedSize_Fix: Boolean;
var
    Collection: ICollection;
begin
    Collection := CollectionClass.Create(True);
    Result := (Collection.GetFixedSize = True);
end;

function TFunctionalTest.GetIgnoreErrors: Boolean;
var
    Collection: ICollection;
begin
    Collection := CollectionClass.Create;
    Result := (Collection.GetIgnoreErrors = [ceDuplicate]);
end;

function TFunctionalTest.GetInstance: Boolean;
var
    Collection: TAbstractCollection;
begin
    Collection := CollectionClass.Create;
    try
        Result := (Collection.GetInstance = Collection);
    finally
        Collection.Free;
    end;
end;

function TFunctionalTest.GetIterator: Boolean;
var
    Collection: ICollection;
    Iterator: IIterator;
    Success1, Success2: Boolean;
begin
    Success1 := False;
    Success2 := False;
    Collection := CollectionClass.Create(FGeneralArray12);
    Iterator := Collection.GetIterator;
    while not Iterator.EOF do
    begin
        if Iterator.CurrentItem = FGeneralObject1 then
            Success1 := True
        else if Iterator.CurrentItem = FGeneralObject2 then
            Success2 := True;
        Iterator.Next;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetIterator_Map: Boolean;
var
    TestMap: IMap;
    Iterator: IIterator;
    Success1, Success2: Boolean;
begin
    Success1 := False;
    Success2 := False;
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FGeneralArray12);
    Iterator := TestMap.GetIterator;
    while not Iterator.EOF do
    begin
        if Iterator.CurrentItem = FGeneralObject1 then
            Success1 := True
        else if Iterator.CurrentItem = FGeneralObject2 then
            Success2 := True;
        Iterator.Next;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetIterator_IntMap: Boolean;
var
    TestMap: IIntegerMap;
    Iterator: IIterator;
    Success1, Success2: Boolean;
begin
    Success1 := False;
    Success2 := False;
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FGeneralArray12);
    Iterator := TestMap.GetIterator;
    while not Iterator.EOF do
    begin
        if Iterator.CurrentItem = FGeneralObject1 then
            Success1 := True
        else if Iterator.CurrentItem = FGeneralObject2 then
            Success2 := True;
        Iterator.Next;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetIterator_StrMap: Boolean;
var
    TestMap: IStringMap;
    Iterator: IIterator;
    Success1, Success2: Boolean;
begin
    Success1 := False;
    Success2 := False;
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FGeneralArray12);
    Iterator := TestMap.GetIterator;
    while not Iterator.EOF do
    begin
        if Iterator.CurrentItem = FGeneralObject1 then
            Success1 := True
        else if Iterator.CurrentItem = FGeneralObject2 then
            Success2 := True;
        Iterator.Next;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetIterator_Rem: Boolean;
var
    Collection: ICollection;
    Iterator: IIterator;
    Success1: Boolean;
begin
    Collection := CollectionClass.Create(FGeneralArray12);
    Iterator := Collection.GetIterator;
    while not Iterator.EOF do
    begin
        if Iterator.CurrentItem = FGeneralObject1 then
            Iterator.Remove;
        Iterator.Next;
    end;
    Success1 := not Collection.Contains(FGeneralObject1);
    Result := Success1;
end;

function TFunctionalTest.GetIterator_RemMap: Boolean;
var
    TestMap: IMap;
    Iterator: IIterator;
    Success1: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FGeneralArray12);
    Iterator := TestMap.GetIterator;
    while not Iterator.EOF do
    begin
        if Iterator.CurrentItem = FGeneralObject1 then
            Iterator.Remove;
        Iterator.Next;
    end;
    Success1 := not TestMap.Contains(FGeneralObject1);
    Result := Success1;
end;

function TFunctionalTest.GetIterator_RemIntMap: Boolean;
var
    TestMap: IIntegerMap;
    Iterator: IIterator;
    Success1: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FGeneralArray12);
    Iterator := TestMap.GetIterator;
    while not Iterator.EOF do
    begin
        if Iterator.CurrentItem = FGeneralObject1 then
            Iterator.Remove;
        Iterator.Next;
    end;
    Success1 := not TestMap.Contains(FGeneralObject1);
    Result := Success1;
end;

function TFunctionalTest.GetIterator_RemStrMap: Boolean;
var
    TestMap: IStringMap;
    Iterator: IIterator;
    Success1: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FGeneralArray12);
    Iterator := TestMap.GetIterator;
    while not Iterator.EOF do
    begin
        if Iterator.CurrentItem = FGeneralObject1 then
            Iterator.Remove;
        Iterator.Next;
    end;
    Success1 := not TestMap.Contains(FGeneralObject1);
    Result := Success1;
end;

function TFunctionalTest.GetIterator_Filter: Boolean;
var
    Collection: ICollection;
    Iterator: IIterator;
    Success1, Success2, Failed1: Boolean;
begin
    Success1 := False;
    Success2 := False;
    Failed1 := False;
    Collection := CollectionClass.Create(FGeneralArray1234);
    Iterator := Collection.GetIterator(TTestFilter_Nat.Create('13') as IFilter);
    while not Iterator.EOF do
    begin
        if Iterator.CurrentItem = FGeneralObject1 then
            Success1 := True
        else if Iterator.CurrentItem = FGeneralObject3 then
            Success2 := True
        else
            Failed1 := True;
        Iterator.Next;
    end;
    Result := Success1 and Success2 and not Failed1;
end;

function TFunctionalTest.GetIterator_FilterMap: Boolean;
var
    TestMap: IMap;
    Iterator: IIterator;
    Success1, Success2, Failed1: Boolean;
begin
    Success1 := False;
    Success2 := False;
    Failed1 := False;
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray1234, FGeneralArray1234);
    Iterator := TestMap.GetIterator(TTestFilter_Nat.Create('13') as IFilter);
    while not Iterator.EOF do
    begin
        if Iterator.CurrentItem = FGeneralObject1 then
            Success1 := True
        else if Iterator.CurrentItem = FGeneralObject3 then
            Success2 := True
        else
            Failed1 := True;
        Iterator.Next;
    end;
    Result := Success1 and Success2 and not Failed1;
end;

function TFunctionalTest.GetIterator_FilterIntMap: Boolean;
var
    TestMap: IIntegerMap;
    Iterator: IIterator;
    Success1, Success2, Failed1: Boolean;
begin
    Success1 := False;
    Success2 := False;
    Failed1 := False;
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray1234, FGeneralArray1234);
    Iterator := TestMap.GetIterator(TTestFilter_Nat.Create('13') as IFilter);
    while not Iterator.EOF do
    begin
        if Iterator.CurrentItem = FGeneralObject1 then
            Success1 := True
        else if Iterator.CurrentItem = FGeneralObject3 then
            Success2 := True
        else
            Failed1 := True;
        Iterator.Next;
    end;
    Result := Success1 and Success2 and not Failed1;
end;

function TFunctionalTest.GetIterator_FilterStrMap: Boolean;
var
    TestMap: IStringMap;
    Iterator: IIterator;
    Success1, Success2, Failed1: Boolean;
begin
    Success1 := False;
    Success2 := False;
    Failed1 := False;
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray1234, FGeneralArray1234);
    Iterator := TestMap.GetIterator(TTestFilter_Nat.Create('13') as IFilter);
    while not Iterator.EOF do
    begin
        if Iterator.CurrentItem = FGeneralObject1 then
            Success1 := True
        else if Iterator.CurrentItem = FGeneralObject3 then
            Success2 := True
        else
            Failed1 := True;
        Iterator.Next;
    end;
    Result := Success1 and Success2 and not Failed1;
end;

function TFunctionalTest.GetIterator_Func: Boolean;
var
    Collection: ICollection;
    Iterator: IIterator;
    Success1, Success2, Failed1: Boolean;
begin
    Success1 := False;
    Success2 := False;
    Failed1 := False;
    Collection := CollectionClass.Create(FGeneralArray1234);
    Iterator := Collection.GetIterator(NaturalFilter34);
    while not Iterator.EOF do
    begin
        if Iterator.CurrentItem = FGeneralObject3 then
            Success1 := True
        else if Iterator.CurrentItem = FGeneralObject4 then
            Success2 := True
        else
            Failed1 := True;
        Iterator.Next;
    end;
    Result := Success1 and Success2 and not Failed1;
end;

function TFunctionalTest.GetIterator_FuncMap: Boolean;
var
    TestMap: IMap;
    Iterator: IIterator;
    Success1, Success2, Failed1: Boolean;
begin
    Success1 := False;
    Success2 := False;
    Failed1 := False;
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray1234, FGeneralArray1234);
    Iterator := TestMap.GetIterator(NaturalFilter12);
    while not Iterator.EOF do
    begin
        if Iterator.CurrentItem = FGeneralObject1 then
            Success1 := True
        else if Iterator.CurrentItem = FGeneralObject2 then
            Success2 := True
        else
            Failed1 := True;
        Iterator.Next;
    end;
    Result := Success1 and Success2 and not Failed1;
end;

function TFunctionalTest.GetIterator_FuncIntMap: Boolean;
var
    TestMap: IIntegerMap;
    Iterator: IIterator;
    Success1, Success2, Failed1: Boolean;
begin
    Success1 := False;
    Success2 := False;
    Failed1 := False;
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray1234, FGeneralArray1234);
    Iterator := TestMap.GetIterator(NaturalFilter34);
    while not Iterator.EOF do
    begin
        if Iterator.CurrentItem = FGeneralObject3 then
            Success1 := True
        else if Iterator.CurrentItem = FGeneralObject4 then
            Success2 := True
        else
            Failed1 := True;
        Iterator.Next;
    end;
    Result := Success1 and Success2 and not Failed1;
end;

function TFunctionalTest.GetIterator_FuncStrMap: Boolean;
var
    TestMap: IStringMap;
    Iterator: IIterator;
    Success1, Success2, Failed1: Boolean;
begin
    Success1 := False;
    Success2 := False;
    Failed1 := False;
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray1234, FGeneralArray1234);
    Iterator := TestMap.GetIterator(NaturalFilter34);
    while not Iterator.EOF do
    begin
        if Iterator.CurrentItem = FGeneralObject3 then
            Success1 := True
        else if Iterator.CurrentItem = FGeneralObject4 then
            Success2 := True
        else
            Failed1 := True;
        Iterator.Next;
    end;
    Result := Success1 and Success2 and not Failed1;
end;

function TFunctionalTest.GetNaturalItemIID_Equ: Boolean;
var
    Collection: ICollection;
begin
    Collection := CollectionClass.Create;
    Result := TMiscCollectionLibrary.EqualIID(Collection.GetNaturalItemIID, EquatableIID);
end;

function TFunctionalTest.GetNaturalItemIID_Comp: Boolean;
var
    Collection: ICollection;
begin
    Collection := CollectionClass.Create;
    Result := TMiscCollectionLibrary.EqualIID(Collection.GetNaturalItemIID, ComparableIID);
end;

function TFunctionalTest.GetNaturalItemIID_Hash: Boolean;
var
    Collection: ICollection;
begin
    Collection := CollectionClass.Create;
    Result := TMiscCollectionLibrary.EqualIID(Collection.GetNaturalItemIID, HashableIID);
end;

function TFunctionalTest.GetNaturalItemIID_Map: Boolean;
var
    Collection: ICollection;
begin
    Collection := CollectionClass.Create;
    Result := TMiscCollectionLibrary.EqualIID(Collection.GetNaturalItemIID, MappableIID);
end;

function TFunctionalTest.GetNaturalItemIID_IntMap: Boolean;
var
    Collection: ICollection;
begin
    Collection := CollectionClass.Create;
    Result := TMiscCollectionLibrary.EqualIID(Collection.GetNaturalItemIID, IntegerMappableIID);
end;

function TFunctionalTest.GetNaturalItemIID_StrMap: Boolean;
var
    Collection: ICollection;
begin
    Collection := CollectionClass.Create;
    Result := TMiscCollectionLibrary.EqualIID(Collection.GetNaturalItemIID, StringMappableIID);
end;

function TFunctionalTest.GetNaturalItemsOnly_Both: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(False);
    Success1 := (Collection.GetNaturalItemsOnly = False);
    Collection := CollectionClass.Create(True);
    Success2 := (Collection.GetNaturalItemsOnly = True);
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetNaturalItemsOnly_Nat: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(False);
    Success1 := (Collection.GetNaturalItemsOnly = True);
    Collection := CollectionClass.Create(True);
    Success2 := (Collection.GetNaturalItemsOnly = True);
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetSize_NonFix: Boolean;
var
    Collection: ICollection;
begin
    Collection := CollectionClass.Create;
    Collection.Add(FGeneralObject1);
    Collection.Add(FGeneralObject2);
    Result := (Collection.GetSize = 2);
end;

function TFunctionalTest.GetSize_NonFixMap: Boolean;
var
    TestMap: IMap;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create;
    TestMap.Put(FGeneralObject1, FGeneralObject1);
    TestMap.Put(FGeneralObject2, FGeneralObject2);
    Result := (TestMap.GetSize = 2);
end;

function TFunctionalTest.GetSize_NonFixIntMap: Boolean;
var
    TestMap: IIntegerMap;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create;
    TestMap.Put(1, FGeneralObject1);
    TestMap.Put(2, FGeneralObject2);
    Result := (TestMap.GetSize = 2);
end;

function TFunctionalTest.GetSize_NonFixStrMap: Boolean;
var
    TestMap: IStringMap;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create;
    TestMap.Put('Item1', FGeneralObject1);
    TestMap.Put('Item2', FGeneralObject2);
    Result := (TestMap.GetSize = 2);
end;

function TFunctionalTest.GetSize_Fix: Boolean;
var
    Collection: ICollection;
begin
    Collection := CollectionClass.Create(FGeneralArray12);
    Result := (Collection.GetSize = 2);
end;

function TFunctionalTest.IsEmpty: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create;
    Success1 := Collection.IsEmpty;
    Collection := CollectionClass.Create(FGeneralArray12);
    Success2 := not Collection.IsEmpty;
    Result := Success1 and Success2;
end;

function TFunctionalTest.IsEmpty_Map: Boolean;
var
    TestMap: IMap;
    Success1, Success2: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create;
    Success1 := TestMap.IsEmpty;
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12);
    Success2 := not TestMap.IsEmpty;
    Result := Success1 and Success2;
end;

function TFunctionalTest.IsEmpty_IntMap: Boolean;
var
    TestMap: IIntegerMap;
    Success1, Success2: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create;
    Success1 := TestMap.IsEmpty;
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FNonNaturalArray12);
    Success2 := not TestMap.IsEmpty;
    Result := Success1 and Success2;
end;

function TFunctionalTest.IsEmpty_StrMap: Boolean;
var
    TestMap: IStringMap;
    Success1, Success2: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create;
    Success1 := TestMap.IsEmpty;
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FNonNaturalArray12);
    Success2 := not TestMap.IsEmpty;
    Result := Success1 and Success2;
end;

function TFunctionalTest.IsNaturalItem: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create;
    Success1 := not Collection.IsNaturalItem(FNonNaturalObject1);
    Success2 := Collection.IsNaturalItem(FGeneralObject1);
    Result := Success1 and Success2;
end;

function TFunctionalTest.IsNaturalItem_Map: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create;
    Success1 := not Collection.IsNaturalItem(FNonNaturalObject1) and not Collection.IsNaturalItem(FGeneralObject1);
    Success2 := Collection.IsNaturalItem(FNaturalKeyMapObject1);
    Result := Success1 and Success2;
end;

function TFunctionalTest.IsNaturalItem_IntMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create;
    Success1 := not Collection.IsNaturalItem(FNonNaturalObject1) and not Collection.IsNaturalItem(FGeneralObject1);
    Success2 := Collection.IsNaturalItem(FIntegerMapObject1);
    Result := Success1 and Success2;
end;

function TFunctionalTest.IsNaturalItem_StrMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create;
    Success1 := not Collection.IsNaturalItem(FNonNaturalObject1) and not Collection.IsNaturalItem(FGeneralObject1);
    Success2 := Collection.IsNaturalItem(FStringMapObject1);
    Result := Success1 and Success2;
end;

function TFunctionalTest.ItemAllowed_NonNat: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(False);
    Success1 := (Collection.ItemAllowed(FNonNaturalObject1) = ceOK);
    Success2 := (Collection.ItemAllowed(FGeneralObject1) = ceOK);
    Result := Success1 and Success2;
end;

function TFunctionalTest.ItemAllowed_Nat: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Success1 := (Collection.ItemAllowed(FNonNaturalObject1) = ceNotNaturalItem);
    Success2 := (Collection.ItemAllowed(FGeneralObject1) = ceOK);
    Result := Success1 and Success2;
end;

function TFunctionalTest.ItemAllowed_NatMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Success1 := (Collection.ItemAllowed(FNonNaturalObject1) = ceNotNaturalItem);
    Success2 := (Collection.ItemAllowed(FGeneralObject1) = ceNotNaturalItem);
    Success3 := (Collection.ItemAllowed(FNaturalKeyMapObject1) = ceOK);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.ItemAllowed_NatIntMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Success1 := (Collection.ItemAllowed(FNonNaturalObject1) = ceNotNaturalItem);
    Success2 := (Collection.ItemAllowed(FGeneralObject1) = ceNotNaturalItem);
    Success3 := (Collection.ItemAllowed(FIntegerMapObject1) = ceOK);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.ItemAllowed_NatStrMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Success1 := (Collection.ItemAllowed(FNonNaturalObject1) = ceNotNaturalItem);
    Success2 := (Collection.ItemAllowed(FGeneralObject1) = ceNotNaturalItem);
    Success3 := (Collection.ItemAllowed(FStringMapObject1) = ceOK);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.ItemCount_NonNatBagList: Boolean;
var
    SourceArray: TCollectableArray;
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    SetLength(SourceArray, 3);
    SourceArray[0] := FNonNaturalObject1;
    SourceArray[1] := FNonNaturalObject2;
    SourceArray[2] := FNonNaturalObject2;
    Collection := CollectionClass.Create(SourceArray);
    Success1 := (Collection.ItemCount(FNonNaturalObject1) = 1);
    Success2 := (Collection.ItemCount(FNonNaturalObject2) = 2);
    Success3 := (Collection.ItemCount(FNonNaturalObject3) = 0);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.ItemCount_NonNatSet: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(FNonNaturalArray12);
    Success1 := (Collection.ItemCount(FNonNaturalObject1) = 1);
    Success2 := (Collection.ItemCount(FNonNaturalObject3) = 0);
    Result := Success1 and Success2;
end;

function TFunctionalTest.ItemCount_NonNatMap: Boolean;
var
    KeyArray, SourceArray: TCollectableArray;
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    SetLength(KeyArray, 3);
    KeyArray[0] := FGeneralObject1;
    KeyArray[1] := FGeneralObject2;
    KeyArray[2] := FGeneralObject3;
    SetLength(SourceArray, 3);
    SourceArray[0] := FNonNaturalObject1;
    SourceArray[1] := FNonNaturalObject2;
    SourceArray[2] := FNonNaturalObject2;
    Collection := TAbstractMapClass(CollectionClass).Create(KeyArray, SourceArray, False, True);
    Success1 := (Collection.ItemCount(FNonNaturalObject1) = 1);
    Success2 := (Collection.ItemCount(FNonNaturalObject2) = 2);
    Success3 := (Collection.ItemCount(FNonNaturalObject3) = 0);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.ItemCount_NonNatIntMap: Boolean;
var
    KeyArray: TIntegerArray;
    SourceArray: TCollectableArray;
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    SetLength(KeyArray, 3);
    KeyArray[0] := 1;
    KeyArray[1] := 2;
    KeyArray[2] := 3;
    SetLength(SourceArray, 3);
    SourceArray[0] := FNonNaturalObject1;
    SourceArray[1] := FNonNaturalObject2;
    SourceArray[2] := FNonNaturalObject2;
    Collection := TAbstractIntegerMapClass(CollectionClass).Create(KeyArray, SourceArray, False);
    Success1 := (Collection.ItemCount(FNonNaturalObject1) = 1);
    Success2 := (Collection.ItemCount(FNonNaturalObject2) = 2);
    Success3 := (Collection.ItemCount(FNonNaturalObject3) = 0);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.ItemCount_NonNatStrMap: Boolean;
var
    KeyArray: TStringArray;
    SourceArray: TCollectableArray;
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    SetLength(KeyArray, 3);
    KeyArray[0] := 'Item1';
    KeyArray[1] := 'Item2';
    KeyArray[2] := 'Item3';
    SetLength(SourceArray, 3);
    SourceArray[0] := FNonNaturalObject1;
    SourceArray[1] := FNonNaturalObject2;
    SourceArray[2] := FNonNaturalObject2;
    Collection := TAbstractStringMapClass(CollectionClass).Create(KeyArray, SourceArray, False);
    Success1 := (Collection.ItemCount(FNonNaturalObject1) = 1);
    Success2 := (Collection.ItemCount(FNonNaturalObject2) = 2);
    Success3 := (Collection.ItemCount(FNonNaturalObject3) = 0);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.ItemCount_NatBagList: Boolean;
var
    SourceArray: TCollectableArray;
    Collection: ICollection;
    Success1, Success2, Success3: Boolean;
begin
    SetLength(SourceArray, 3);
    SourceArray[0] := FGeneralObject1;
    SourceArray[1] := FGeneralObject2;
    SourceArray[2] := FGeneralObject2;
    Collection := CollectionClass.Create(SourceArray);
    Success1 := (Collection.ItemCount(FGeneralObject1) = 1);
    Success2 := (Collection.ItemCount(FGeneralObject2) = 2);
    Success3 := (Collection.ItemCount(FGeneralObject3) = 0);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.ItemCount_NatSet: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(FGeneralArray12);
    Success1 := (Collection.ItemCount(FGeneralObject1) = 1);
    Success2 := (Collection.ItemCount(FGeneralObject3) = 0);
    Result := Success1 and Success2;
end;

function TFunctionalTest.ItemCount_NatMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(FNaturalMapItemArray12, True);
    Success1 := (Collection.ItemCount(FNaturalKeyMapObject1) = 1);
    Success2 := (Collection.ItemCount(FNaturalKeyMapObject3) = 0);
    Result := Success1 and Success2;
end;

function TFunctionalTest.ItemCount_NatIntMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(FIntegerMapItemArray12, True);
    Success1 := (Collection.ItemCount(FIntegerMapObject1) = 1);
    Success2 := (Collection.ItemCount(FIntegerMapObject3) = 0);
    Result := Success1 and Success2;
end;

function TFunctionalTest.ItemCount_NatStrMap: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(FStringMapItemArray12, True);
    Success1 := (Collection.ItemCount(FStringMapObject1) = 1);
    Success2 := (Collection.ItemCount(FStringMapObject3) = 0);
    Result := Success1 and Success2;
end;

function TFunctionalTest.ItemCount_Arr: Boolean;
var
    Collection: ICollection;
    Array1, Array2, Array3: TCollectableArray;
    Success1, Success2, Success3, Success4: Boolean;
begin
    SetLength(Array1, 0);
    SetLength(Array2, 2);
    Array2[0] := FGeneralObject3;
    Array2[1] := FGeneralObject4;
    SetLength(Array3, 2);
    Array3[0] := FGeneralObject2;
    Array3[1] := FGeneralObject3;
    Collection := CollectionClass.Create(FGeneralArray12);
    Success1 := (Collection.ItemCount(Array1) = 0);
    Success2 := (Collection.ItemCount(Array2) = 0);
    Success3 := (Collection.ItemCount(Array3) = 1);
    Success4 := (Collection.ItemCount(FGeneralArray12) = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.ItemCount_ArrMap: Boolean;
var
    TestMap: IMap;
    Array1, Array2, Array3: TCollectableArray;
    Success1, Success2, Success3, Success4: Boolean;
begin
    SetLength(Array1, 0);
    SetLength(Array2, 2);
    Array2[0] := FNonNaturalObject3;
    Array2[1] := FNonNaturalObject4;
    SetLength(Array3, 2);
    Array3[0] := FNonNaturalObject2;
    Array3[1] := FNonNaturalObject3;
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12);
    Success1 := (TestMap.ItemCount(Array1) = 0);
    Success2 := (TestMap.ItemCount(Array2) = 0);
    Success3 := (TestMap.ItemCount(Array3) = 1);
    Success4 := (TestMap.ItemCount(FNonNaturalArray12) = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.ItemCount_ArrIntMap: Boolean;
var
    TestMap: IIntegerMap;
    Array1, Array2, Array3: TCollectableArray;
    Success1, Success2, Success3, Success4: Boolean;
begin
    SetLength(Array1, 0);
    SetLength(Array2, 2);
    Array2[0] := FNonNaturalObject3;
    Array2[1] := FNonNaturalObject4;
    SetLength(Array3, 2);
    Array3[0] := FNonNaturalObject2;
    Array3[1] := FNonNaturalObject3;
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FNonNaturalArray12);
    Success1 := (TestMap.ItemCount(Array1) = 0);
    Success2 := (TestMap.ItemCount(Array2) = 0);
    Success3 := (TestMap.ItemCount(Array3) = 1);
    Success4 := (TestMap.ItemCount(FNonNaturalArray12) = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.ItemCount_ArrStrMap: Boolean;
var
    TestMap: IStringMap;
    Array1, Array2, Array3: TCollectableArray;
    Success1, Success2, Success3, Success4: Boolean;
begin
    SetLength(Array1, 0);
    SetLength(Array2, 2);
    Array2[0] := FNonNaturalObject3;
    Array2[1] := FNonNaturalObject4;
    SetLength(Array3, 2);
    Array3[0] := FNonNaturalObject2;
    Array3[1] := FNonNaturalObject3;
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FNonNaturalArray12);
    Success1 := (TestMap.ItemCount(Array1) = 0);
    Success2 := (TestMap.ItemCount(Array2) = 0);
    Success3 := (TestMap.ItemCount(Array3) = 1);
    Success4 := (TestMap.ItemCount(FNonNaturalArray12) = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.ItemCount_IF: Boolean;
var
    Collection: ICollection;
    Collection1, Collection2, Collection3: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    Collection1 := TPArrayBag.Create(True);
    Collection2 := TPArrayBag.Create(True);
    Collection2.Add(FGeneralObject3);
    Collection2.Add(FGeneralObject4);
    Collection3 := TPArrayBag.Create(True);
    Collection3.Add(FGeneralObject2);
    Collection3.Add(FGeneralObject3);
    Collection := CollectionClass.Create(FGeneralCollection12);
    Success1 := (Collection.ItemCount(Collection1) = 0);
    Success2 := (Collection.ItemCount(Collection2) = 0);
    Success3 := (Collection.ItemCount(Collection3) = 1);
    Success4 := (Collection.ItemCount(FGeneralCollection12) = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.ItemCount_IFMap: Boolean;
var
    Collection: ICollection;
    Collection1, Collection2, Collection3: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    Collection1 := TPArrayBag.Create(True);
    Collection2 := TPArrayBag.Create(True);
    Collection2.Add(FNaturalKeyMapObject3);
    Collection2.Add(FNaturalKeyMapObject4);
    Collection3 := TPArrayBag.Create(True);
    Collection3.Add(FNaturalKeyMapObject2);
    Collection3.Add(FNaturalKeyMapObject3);
    Collection := CollectionClass.Create(FNaturalItemMap12);
    Success1 := (Collection.ItemCount(Collection1) = 0);
    Success2 := (Collection.ItemCount(Collection2) = 0);
    Success3 := (Collection.ItemCount(Collection3) = 1);
    Success4 := (Collection.ItemCount(FNaturalItemMap12) = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.ItemCount_IFIntMap: Boolean;
var
    Collection: ICollection;
    Collection1, Collection2, Collection3: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    Collection1 := TPArrayBag.Create(True);
    Collection2 := TPArrayBag.Create(True);
    Collection2.Add(FIntegerMapObject3);
    Collection2.Add(FIntegerMapObject4);
    Collection3 := TPArrayBag.Create(True);
    Collection3.Add(FIntegerMapObject2);
    Collection3.Add(FIntegerMapObject3);
    Collection := CollectionClass.Create(FNaturalItemIntMap12);
    Success1 := (Collection.ItemCount(Collection1) = 0);
    Success2 := (Collection.ItemCount(Collection2) = 0);
    Success3 := (Collection.ItemCount(Collection3) = 1);
    Success4 := (Collection.ItemCount(FNaturalItemIntMap12) = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.ItemCount_IFStrMap: Boolean;
var
    Collection: ICollection;
    Collection1, Collection2, Collection3: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    Collection1 := TPArrayBag.Create(True);
    Collection2 := TPArrayBag.Create(True);
    Collection2.Add(FStringMapObject3);
    Collection2.Add(FStringMapObject4);
    Collection3 := TPArrayBag.Create(True);
    Collection3.Add(FStringMapObject2);
    Collection3.Add(FStringMapObject3);
    Collection := CollectionClass.Create(FNaturalItemStrMap12);
    Success1 := (Collection.ItemCount(Collection1) = 0);
    Success2 := (Collection.ItemCount(Collection2) = 0);
    Success3 := (Collection.ItemCount(Collection3) = 1);
    Success4 := (Collection.ItemCount(FNaturalItemStrMap12) = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Matching_ArrNonNat: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    Collection := CollectionClass.Create(False);
    Success1 := Collection.Matching(FEmptyArray).IsEmpty;
    Success2 := Collection.Matching(FNonNaturalArray12).IsEmpty;
    Collection := CollectionClass.Create(FNonNaturalArray1234);
    Success3 := Collection.Matching(FEmptyArray).IsEmpty;
    Success4 := (Collection.Matching(FNonNaturalArray12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Matching_ArrNonNatList: Boolean;
var
    TestList: IList;
    Success1, Success2, Success3, Success4: Boolean;
    Success5, Success6, Success7, Success8: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(False);
    TestList.Sorted := False;
    Success1 := TestList.Matching(FEmptyArray).IsEmpty;
    Success2 := TestList.Matching(FNonNaturalArray12).IsEmpty;
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray1234);
    TestList.Sorted := False;
    Success3 := TestList.Matching(FEmptyArray).IsEmpty;
    Success4 := (TestList.Matching(FNonNaturalArray12).GetSize = 2);
    TestList := TAbstractListClass(CollectionClass).Create(False);
    TestList.Sorted := True;
    Success5 := TestList.Matching(FEmptyArray).IsEmpty;
    Success6 := TestList.Matching(FNonNaturalArray12).IsEmpty;
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray1234);
    TestList.Sorted := True;
    Success7 := TestList.Matching(FEmptyArray).IsEmpty;
    Success8 := (TestList.Matching(FNonNaturalArray12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6 and Success7 and Success8;
end;

function TFunctionalTest.Matching_ArrNonNatMap: Boolean;
var
    TestMap: IMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(False);
    Success1 := TestMap.Matching(FEmptyArray).IsEmpty;
    Success2 := TestMap.Matching(FNonNaturalArray12).IsEmpty;
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray1234, FNonNaturalArray1234, False);
    Success3 := TestMap.Matching(FEmptyArray).IsEmpty;
    Success4 := (TestMap.Matching(FNonNaturalArray12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Matching_ArrNonNatIntMap: Boolean;
var
    TestMap: IIntegerMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(False);
    Success1 := TestMap.Matching(FEmptyArray).IsEmpty;
    Success2 := TestMap.Matching(FNonNaturalArray12).IsEmpty;
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray1234, FNonNaturalArray1234, False);
    Success3 := TestMap.Matching(FEmptyArray).IsEmpty;
    Success4 := (TestMap.Matching(FNonNaturalArray12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Matching_ArrNonNatStrMap: Boolean;
var
    TestMap: IStringMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(False);
    Success1 := TestMap.Matching(FEmptyArray).IsEmpty;
    Success2 := TestMap.Matching(FNonNaturalArray12).IsEmpty;
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray1234, FNonNaturalArray1234, False);
    Success3 := TestMap.Matching(FEmptyArray).IsEmpty;
    Success4 := (TestMap.Matching(FNonNaturalArray12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Matching_ArrNat: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Success1 := Collection.Matching(FEmptyArray).IsEmpty;
    Success2 := Collection.Matching(FGeneralArray12).IsEmpty;
    Collection := CollectionClass.Create(FGeneralArray1234, True);
    Success3 := Collection.Matching(FEmptyArray).IsEmpty;
    Success4 := (Collection.Matching(FGeneralArray12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Matching_ArrNatList: Boolean;
var
    TestList: IList;
    Success1, Success2, Success3, Success4: Boolean;
    Success5, Success6, Success7, Success8: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(True);
    TestList.Sorted := False;
    Success1 := TestList.Matching(FEmptyArray).IsEmpty;
    Success2 := TestList.Matching(FGeneralArray12).IsEmpty;
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray1234, True);
    TestList.Sorted := False;
    Success3 := TestList.Matching(FEmptyArray).IsEmpty;
    Success4 := (TestList.Matching(FGeneralArray12).GetSize = 2);
    TestList := TAbstractListClass(CollectionClass).Create(True);
    TestList.Sorted := True;
    Success5 := TestList.Matching(FEmptyArray).IsEmpty;
    Success6 := TestList.Matching(FGeneralArray12).IsEmpty;
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray1234, True);
    TestList.Sorted := True;
    Success7 := TestList.Matching(FEmptyArray).IsEmpty;
    Success8 := (TestList.Matching(FGeneralArray12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6 and Success7 and Success8;
end;

function TFunctionalTest.Matching_ArrNatMap: Boolean;
var
    TestMap: IMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(True);
    Success1 := TestMap.Matching(FEmptyArray).IsEmpty;
    Success2 := TestMap.Matching(FNaturalMapItemArray12).IsEmpty;
    TestMap := TAbstractMapClass(CollectionClass).Create(FNaturalMapItemArray1234, True);
    Success3 := TestMap.Matching(FEmptyArray).IsEmpty;
    Success4 := (TestMap.Matching(FNaturalMapItemArray12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Matching_ArrNatIntMap: Boolean;
var
    TestMap: IIntegerMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(True);
    Success1 := TestMap.Matching(FEmptyArray).IsEmpty;
    Success2 := TestMap.Matching(FIntegerMapItemArray12).IsEmpty;
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerMapItemArray1234, True);
    Success3 := TestMap.Matching(FEmptyArray).IsEmpty;
    Success4 := (TestMap.Matching(FIntegerMapItemArray12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Matching_ArrNatStrMap: Boolean;
var
    TestMap: IStringMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(True);
    Success1 := TestMap.Matching(FEmptyArray).IsEmpty;
    Success2 := TestMap.Matching(FStringMapItemArray12).IsEmpty;
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringMapItemArray1234, True);
    Success3 := TestMap.Matching(FEmptyArray).IsEmpty;
    Success4 := (TestMap.Matching(FStringMapItemArray12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Matching_IFNonNat: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    Collection := CollectionClass.Create(False);
    Success1 := Collection.Matching(FEmptyCollection).IsEmpty;
    Success2 := Collection.Matching(FGeneralCollection12).IsEmpty;
    Collection := CollectionClass.Create(FNonNaturalCollection1234);
    Success3 := Collection.Matching(FEmptyCollection).IsEmpty;
    Success4 := (Collection.Matching(FNonNaturalCollection12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Matching_IFNonNatList: Boolean;
var
    TestList: IList;
    Success1, Success2, Success3, Success4: Boolean;
    Success5, Success6, Success7, Success8: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(False);
    TestList.Sorted := False;
    Success1 := TestList.Matching(FEmptyCollection).IsEmpty;
    Success2 := TestList.Matching(FGeneralCollection12).IsEmpty;
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalCollection1234);
    TestList.Sorted := False;
    Success3 := TestList.Matching(FEmptyCollection).IsEmpty;
    Success4 := (TestList.Matching(FNonNaturalCollection12).GetSize = 2);
    TestList := TAbstractListClass(CollectionClass).Create(False);
    TestList.Sorted := True;
    Success5 := TestList.Matching(FEmptyCollection).IsEmpty;
    Success6 := TestList.Matching(FGeneralCollection12).IsEmpty;
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalCollection1234);
    TestList.Sorted := True;
    Success7 := TestList.Matching(FEmptyCollection).IsEmpty;
    Success8 := (TestList.Matching(FNonNaturalCollection12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6 and Success7 and Success8;
end;

function TFunctionalTest.Matching_IFNonNatMap: Boolean;
var
    TestMap: IMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(False);
    Success1 := TestMap.Matching(FEmptyCollection).IsEmpty;
    Success2 := Testmap.Matching(FGeneralCollection12).IsEmpty;
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray1234, FNonNaturalArray1234);
    Success3 := TestMap.Matching(FEmptyCollection).IsEmpty;
    Success4 := (TestMap.Matching(FNonNaturalCollection12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Matching_IFNonNatIntMap: Boolean;
var
    TestMap: IIntegerMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(False);
    Success1 := TestMap.Matching(FEmptyCollection).IsEmpty;
    Success2 := Testmap.Matching(FGeneralCollection12).IsEmpty;
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray1234, FNonNaturalArray1234);
    Success3 := TestMap.Matching(FEmptyCollection).IsEmpty;
    Success4 := (TestMap.Matching(FNonNaturalCollection12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Matching_IFNonNatStrMap: Boolean;
var
    TestMap: IStringMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(False);
    Success1 := TestMap.Matching(FEmptyCollection).IsEmpty;
    Success2 := Testmap.Matching(FGeneralCollection12).IsEmpty;
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray1234, FNonNaturalArray1234);
    Success3 := TestMap.Matching(FEmptyCollection).IsEmpty;
    Success4 := (TestMap.Matching(FNonNaturalCollection12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Matching_IFNat: Boolean;
var
    Collection: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Success1 := Collection.Matching(FEmptyCollection).IsEmpty;
    Success2 := Collection.Matching(FGeneralCollection12).IsEmpty;
    Collection := CollectionClass.Create(FGeneralCollection1234);
    Success3 := Collection.Matching(FEmptyCollection).IsEmpty;
    Success4 := (Collection.Matching(FGeneralCollection12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Matching_IFNatList: Boolean;
var
    TestList: IList;
    Success1, Success2, Success3, Success4: Boolean;
    Success5, Success6, Success7, Success8: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(True);
    TestList.Sorted := False;
    Success1 := TestList.Matching(FEmptyCollection).IsEmpty;
    Success2 := TestList.Matching(FGeneralCollection12).IsEmpty;
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralCollection1234);
    TestList.Sorted := False;
    Success3 := TestList.Matching(FEmptyCollection).IsEmpty;
    Success4 := (TestList.Matching(FGeneralCollection12).GetSize = 2);
    TestList := TAbstractListClass(CollectionClass).Create(True);
    TestList.Sorted := True;
    Success5 := TestList.Matching(FEmptyCollection).IsEmpty;
    Success6 := TestList.Matching(FGeneralCollection12).IsEmpty;
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralCollection1234);
    TestList.Sorted := True;
    Success7 := TestList.Matching(FEmptyCollection).IsEmpty;
    Success8 := (TestList.Matching(FGeneralCollection12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6 and Success7 and Success8;
end;

function TFunctionalTest.Matching_IFNatMap: Boolean;
var
    TestMap: IMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(True);
    Success1 := TestMap.Matching(FEmptyCollection).IsEmpty;
    Success2 := TestMap.Matching(FNaturalMapItemArray12).IsEmpty;
    TestMap := TAbstractMapClass(CollectionClass).Create(FNaturalMapItemArray1234);
    Success3 := TestMap.Matching(FEmptyCollection).IsEmpty;
    Success4 := (TestMap.Matching(FNaturalMapItemArray12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Matching_IFNatIntMap: Boolean;
var
    TestMap: IIntegerMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(True);
    Success1 := TestMap.Matching(FEmptyCollection).IsEmpty;
    Success2 := TestMap.Matching(FIntegerMapItemArray12).IsEmpty;
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerMapItemArray1234);
    Success3 := TestMap.Matching(FEmptyCollection).IsEmpty;
    Success4 := (TestMap.Matching(FIntegerMapItemArray12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Matching_IFNatStrMap: Boolean;
var
    TestMap: IStringMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(True);
    Success1 := TestMap.Matching(FEmptyCollection).IsEmpty;
    Success2 := TestMap.Matching(FStringMapItemArray12).IsEmpty;
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringMapItemArray1234);
    Success3 := TestMap.Matching(FEmptyCollection).IsEmpty;
    Success4 := (TestMap.Matching(FStringMapItemArray12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Remove_Fix: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(FGeneralCollection12);
    Collection.IgnoreErrors := [];
    try
        Collection.Remove(FGeneralObject1);
        Success1 := False;
    except
        Success1 := True;
    end;
    Collection.IgnoreErrors := [ceFixedSize];
    Success2 := (Collection.Remove(FGeneralObject1) = nil);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Remove_Dup: Boolean;
var
    Collection: ICollection;
    Array1: TCollectableArray;
    Success1, Success2, Success3: Boolean;
begin
    SetLength(Array1, 2);
    Array1[0] := FGeneralObject1;
    Array1[1] := FGeneralObject1;
    Collection := CollectionClass.Create(Array1);
    Success1 := (Collection.Remove(FGeneralObject1) = FGeneralObject1);
    Success2 := Collection.Contains(FGeneralObject1);
    Success3 := (Collection.GetSize = 1);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Remove_NoDup: Boolean;
var
    Collection: ICollection;
    Array1: TCollectableArray;
    Success1, Success2, Success3: Boolean;
begin
    SetLength(Array1, 1);
    Array1[0] := FGeneralObject1;
    Collection := CollectionClass.Create(Array1);
    Success1 := (Collection.Remove(FGeneralObject1) = FGeneralObject1);
    Success2 := not Collection.Contains(FGeneralObject1);
    Success3 := (Collection.GetSize = 0);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Remove_List: Boolean;
var
    TestList: IList;
    Array1: TCollectableArray;
    Success1, Success2, Success3: Boolean;
    Success4, Success5, Success6: Boolean;
begin
    SetLength(Array1, 4);
    Array1[0] := FGeneralObject2;
    Array1[1] := FGeneralObject1;
    Array1[2] := FGeneralObject1;
    Array1[3] := FGeneralObject2;
    TestList := TAbstractListClass(CollectionClass).Create(Array1);
    TestList.Sorted := False;
    Success1 := (TestList.Remove(FGeneralObject1) = FGeneralObject1);
    Success2 := TestList.Contains(FGeneralObject1);
    Success3 := (TestList.GetSize = 3);
    TestList := TAbstractListClass(CollectionClass).Create(Array1);
    TestList.Sorted := True;
    Success4 := (TestList.Remove(FGeneralObject1) = FGeneralObject1);
    Success5 := TestList.Contains(FGeneralObject1);
    Success6 := (TestList.GetSize = 3);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6;
end;

function TFunctionalTest.Remove_Map: Boolean;
var
    TestMap: IMap;
    Array1: TCollectableArray;
    Success1, Success2, Success3: Boolean;
begin
    SetLength(Array1, 2);
    Array1[0] := FGeneralObject1;
    Array1[1] := FGeneralObject1;
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, Array1);
    Success1 := (TestMap.Remove(FGeneralObject1) = FGeneralObject1);
    Success2 := TestMap.Contains(FGeneralObject1);
    Success3 := (TestMap.GetSize = 1);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Remove_IntMap: Boolean;
var
    TestMap: IIntegerMap;
    Array1: TCollectableArray;
    Success1, Success2, Success3: Boolean;
begin
    SetLength(Array1, 2);
    Array1[0] := FGeneralObject1;
    Array1[1] := FGeneralObject1;
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, Array1);
    Success1 := (TestMap.Remove(FGeneralObject1) = FGeneralObject1);
    Success2 := TestMap.Contains(FGeneralObject1);
    Success3 := (TestMap.GetSize = 1);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Remove_StrMap: Boolean;
var
    TestMap: IStringMap;
    Array1: TCollectableArray;
    Success1, Success2, Success3: Boolean;
begin
    SetLength(Array1, 2);
    Array1[0] := FGeneralObject1;
    Array1[1] := FGeneralObject1;
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, Array1);
    Success1 := (TestMap.Remove(FGeneralObject1) = FGeneralObject1);
    Success2 := TestMap.Contains(FGeneralObject1);
    Success3 := (TestMap.GetSize = 1);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.RemoveAll_Dup: Boolean;
var
    Collection: ICollection;
    Array1: TCollectableArray;
    Success1, Success2, Success3: Boolean;
begin
    SetLength(Array1, 4);
    Array1[0] := FGeneralObject2;
    Array1[1] := FGeneralObject1;
    Array1[2] := FGeneralObject1;
    Array1[3] := FGeneralObject2;
    Collection := CollectionClass.Create(Array1);
    Success1 := (Collection.RemoveAll(FGeneralObject1).GetSize = 2);
    Success2 := not Collection.Contains(FGeneralObject1);
    Success3 := (Collection.GetSize = 2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.RemoveAll_NoDup: Boolean;
var
    Collection: ICollection;
    Array1: TCollectableArray;
    Success1, Success2, Success3: Boolean;
begin
    SetLength(Array1, 3);
    Array1[0] := FGeneralObject1;
    Array1[1] := FGeneralObject2;
    Array1[2] := FGeneralObject3;
    Collection := CollectionClass.Create(Array1);
    Success1 := (Collection.RemoveAll(FGeneralObject2).GetSize = 1);
    Success2 := not Collection.Contains(FGeneralObject2);
    Success3 := (Collection.GetSize = 2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.RemoveAll_List: Boolean;
var
    TestList: IList;
    Array1: TCollectableArray;
    Success1, Success2, Success3, Success4, Success5, Success6: Boolean;
begin
    SetLength(Array1, 4);
    Array1[0] := FGeneralObject2;
    Array1[1] := FGeneralObject1;
    Array1[2] := FGeneralObject1;
    Array1[3] := FGeneralObject2;
    TestList := TAbstractListClass(CollectionClass).Create(Array1);
    TestList.Sorted := False;
    Success1 := (TestList.RemoveAll(FGeneralObject1).GetSize = 2);
    Success2 := not TestList.Contains(FGeneralObject1);
    Success3 := (TestList.GetSize = 2);
    TestList := TAbstractListClass(CollectionClass).Create(Array1);
    TestList.Sorted := True;
    Success4 := (TestList.RemoveAll(FGeneralObject1).GetSize = 2);
    Success5 := not TestList.Contains(FGeneralObject1);
    Success6 := (TestList.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6;
end;

function TFunctionalTest.RemoveAll_Map: Boolean;
var
    TestMap: IMap;
    Array1: TCollectableArray;
    Success1, Success2, Success3: Boolean;
begin
    SetLength(Array1, 4);
    Array1[0] := FGeneralObject2;
    Array1[1] := FGeneralObject1;
    Array1[2] := FGeneralObject1;
    Array1[3] := FGeneralObject2;
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray1234, Array1);
    Success1 := (TestMap.RemoveAll(FGeneralObject1).GetSize = 2);
    Success2 := not TestMap.Contains(FGeneralObject1);
    Success3 := (TestMap.GetSize = 2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.RemoveAll_StrMap: Boolean;
var
    TestMap: IStringMap;
    Array1: TCollectableArray;
    Success1, Success2, Success3: Boolean;
begin
    SetLength(Array1, 4);
    Array1[0] := FGeneralObject2;
    Array1[1] := FGeneralObject1;
    Array1[2] := FGeneralObject1;
    Array1[3] := FGeneralObject2;
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray1234, Array1);
    Success1 := (TestMap.RemoveAll(FGeneralObject1).GetSize = 2);
    Success2 := not TestMap.Contains(FGeneralObject1);
    Success3 := (TestMap.GetSize = 2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.RemoveAll_IntMap: Boolean;
var
    TestMap: IIntegerMap;
    Array1: TCollectableArray;
    Success1, Success2, Success3: Boolean;
begin
    SetLength(Array1, 4);
    Array1[0] := FGeneralObject2;
    Array1[1] := FGeneralObject1;
    Array1[2] := FGeneralObject1;
    Array1[3] := FGeneralObject2;
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray1234, Array1);
    Success1 := (TestMap.RemoveAll(FGeneralObject1).GetSize = 2);
    Success2 := not TestMap.Contains(FGeneralObject1);
    Success3 := (TestMap.GetSize = 2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Remove_Arr: Boolean;
var
    Collection: ICollection;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5: Boolean;
begin
    Collection := CollectionClass.Create(FGeneralArray1234);
    RemovedCollection := Collection.Remove(FGeneralArray12);
    Success1 := RemovedCollection.Contains(FGeneralObject1);
    Success2 := RemovedCollection.Contains(FGeneralObject2);
    Success3 := not Collection.Contains(FGeneralObject1);
    Success4 := not Collection.Contains(FGeneralObject2);
    Success5 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5;
end;

function TFunctionalTest.Remove_ArrList: Boolean;
var
    TestList: IList;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5: Boolean;
    Success6, Success7, Success8, Success9, Success10: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray1234);
    TestList.Sorted := False;
    RemovedCollection := TestList.Remove(FGeneralArray12);
    Success1 := RemovedCollection.Contains(FGeneralObject1);
    Success2 := RemovedCollection.Contains(FGeneralObject2);
    Success3 := not TestList.Contains(FGeneralObject1);
    Success4 := not TestList.Contains(FGeneralObject2);
    Success5 := (RemovedCollection.GetSize = 2);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray1234);
    TestList.Sorted := True;
    RemovedCollection := TestList.Remove(FGeneralArray12);
    Success6 := RemovedCollection.Contains(FGeneralObject1);
    Success7 := RemovedCollection.Contains(FGeneralObject2);
    Success8 := not TestList.Contains(FGeneralObject1);
    Success9 := not TestList.Contains(FGeneralObject2);
    Success10 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and
        Success6 and Success7 and Success8 and Success9 and Success10;
end;

function TFunctionalTest.Remove_ArrMap: Boolean;
var
    TestMap: IMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray1234, FGeneralArray1234);
    RemovedCollection := TestMap.Remove(FGeneralArray12);
    Success1 := RemovedCollection.Contains(FGeneralObject1);
    Success2 := RemovedCollection.Contains(FGeneralObject2);
    Success3 := not TestMap.Contains(FGeneralObject1);
    Success4 := not TestMap.Contains(FGeneralObject2);
    Success5 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5;
end;

function TFunctionalTest.Remove_ArrIntMap: Boolean;
var
    TestMap: IIntegerMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray1234, FGeneralArray1234);
    RemovedCollection := TestMap.Remove(FGeneralArray12);
    Success1 := RemovedCollection.Contains(FGeneralObject1);
    Success2 := RemovedCollection.Contains(FGeneralObject2);
    Success3 := not TestMap.Contains(FGeneralObject1);
    Success4 := not TestMap.Contains(FGeneralObject2);
    Success5 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5;
end;

function TFunctionalTest.Remove_ArrStrMap: Boolean;
var
    TestMap: IStringMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray1234, FGeneralArray1234);
    RemovedCollection := TestMap.Remove(FGeneralArray12);
    Success1 := RemovedCollection.Contains(FGeneralObject1);
    Success2 := RemovedCollection.Contains(FGeneralObject2);
    Success3 := not TestMap.Contains(FGeneralObject1);
    Success4 := not TestMap.Contains(FGeneralObject2);
    Success5 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5;
end;

function TFunctionalTest.Remove_IF: Boolean;
var
    Collection: ICollection;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5: Boolean;
begin
    Collection := CollectionClass.Create(FGeneralCollection1234);
    RemovedCollection := Collection.Remove(FGeneralCollection12);
    Success1 := RemovedCollection.Contains(FGeneralObject1);
    Success2 := RemovedCollection.Contains(FGeneralObject2);
    Success3 := not Collection.Contains(FGeneralObject1);
    Success4 := not Collection.Contains(FGeneralObject2);
    Success5 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5;
end;

function TFunctionalTest.Remove_IFList: Boolean;
var
    TestList: IList;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5: Boolean;
    Success6, Success7, Success8, Success9, Success10: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralCollection1234);
    TestList.Sorted := False;
    RemovedCollection := TestList.Remove(FGeneralCollection12);
    Success1 := RemovedCollection.Contains(FGeneralObject1);
    Success2 := RemovedCollection.Contains(FGeneralObject2);
    Success3 := not TestList.Contains(FGeneralObject1);
    Success4 := not TestList.Contains(FGeneralObject2);
    Success5 := (RemovedCollection.GetSize = 2);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralCollection1234);
    TestList.Sorted := True;
    RemovedCollection := TestList.Remove(FGeneralCollection12);
    Success6 := RemovedCollection.Contains(FGeneralObject1);
    Success7 := RemovedCollection.Contains(FGeneralObject2);
    Success8 := not TestList.Contains(FGeneralObject1);
    Success9 := not TestList.Contains(FGeneralObject2);
    Success10 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and Success6 and Success7 and Success8 and Success9 and Success10;
end;

function TFunctionalTest.Remove_IFMap: Boolean;
var
    TestMap: IMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray1234, FGeneralArray1234);
    RemovedCollection := TestMap.Remove(FGeneralCollection12);
    Success1 := RemovedCollection.Contains(FGeneralObject1);
    Success2 := RemovedCollection.Contains(FGeneralObject2);
    Success3 := not TestMap.Contains(FGeneralObject1);
    Success4 := not TestMap.Contains(FGeneralObject2);
    Success5 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5;
end;

function TFunctionalTest.Remove_IFIntMap: Boolean;
var
    TestMap: IIntegerMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray1234, FGeneralArray1234);
    RemovedCollection := TestMap.Remove(FGeneralCollection12);
    Success1 := RemovedCollection.Contains(FGeneralObject1);
    Success2 := RemovedCollection.Contains(FGeneralObject2);
    Success3 := not TestMap.Contains(FGeneralObject1);
    Success4 := not TestMap.Contains(FGeneralObject2);
    Success5 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5;
end;

function TFunctionalTest.Remove_IFStrMap: Boolean;
var
    TestMap: IStringMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray1234, FGeneralArray1234);
    RemovedCollection := TestMap.Remove(FGeneralCollection12);
    Success1 := RemovedCollection.Contains(FGeneralObject1);
    Success2 := RemovedCollection.Contains(FGeneralObject2);
    Success3 := not TestMap.Contains(FGeneralObject1);
    Success4 := not TestMap.Contains(FGeneralObject2);
    Success5 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5;
end;

function TFunctionalTest.Retain_Arr: Boolean;
var
    Collection: ICollection;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5, Success6, Success7: Boolean;
begin
    Collection := CollectionClass.Create(FGeneralArray1234);
    RemovedCollection := Collection.Retain(FGeneralArray12);
    Success1 := RemovedCollection.Contains(FGeneralObject3);
    Success2 := RemovedCollection.Contains(FGeneralObject4);
    Success3 := Collection.Contains(FGeneralObject1);
    Success4 := Collection.Contains(FGeneralObject2);
    Success5 := not Collection.Contains(FGeneralObject3);
    Success6 := not Collection.Contains(FGeneralObject4);
    Success7 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and
        Success6 and Success7;
end;

function TFunctionalTest.Retain_ArrList: Boolean;
var
    TestList: IList;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5, Success6, Success7: Boolean;
    Success8, Success9, Success10, Success11, Success12, Success13, Success14: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray1234);
    TestList.Sorted := False;
    RemovedCollection := TestList.Retain(FGeneralArray12);
    Success1 := RemovedCollection.Contains(FGeneralObject3);
    Success2 := RemovedCollection.Contains(FGeneralObject4);
    Success3 := TestList.Contains(FGeneralObject1);
    Success4 := TestList.Contains(FGeneralObject2);
    Success5 := not TestList.Contains(FGeneralObject3);
    Success6 := not TestList.Contains(FGeneralObject4);
    Success7 := (RemovedCollection.GetSize = 2);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray1234);
    TestList.Sorted := True;
    RemovedCollection := TestList.Retain(FGeneralArray12);
    Success8 := RemovedCollection.Contains(FGeneralObject3);
    Success9 := RemovedCollection.Contains(FGeneralObject4);
    Success10 := TestList.Contains(FGeneralObject1);
    Success11 := TestList.Contains(FGeneralObject2);
    Success12 := not TestList.Contains(FGeneralObject3);
    Success13 := not TestList.Contains(FGeneralObject4);
    Success14 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and
        Success6 and Success7 and Success8 and Success9 and Success10 and
        Success11 and Success12 and Success13 and Success14;
end;

function TFunctionalTest.Retain_ArrMap: Boolean;
var
    TestMap: IMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5, Success6, Success7: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray1234, FGeneralArray1234);
    RemovedCollection := TestMap.Retain(FGeneralArray12);
    Success1 := RemovedCollection.Contains(FGeneralObject3);
    Success2 := RemovedCollection.Contains(FGeneralObject4);
    Success3 := TestMap.Contains(FGeneralObject1);
    Success4 := TestMap.Contains(FGeneralObject2);
    Success5 := not TestMap.Contains(FGeneralObject3);
    Success6 := not TestMap.Contains(FGeneralObject4);
    Success7 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and
        Success6 and Success7;
end;

function TFunctionalTest.Retain_ArrIntMap: Boolean;
var
    TestMap: IIntegerMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5, Success6, Success7: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray1234, FGeneralArray1234);
    RemovedCollection := TestMap.Retain(FGeneralArray12);
    Success1 := RemovedCollection.Contains(FGeneralObject3);
    Success2 := RemovedCollection.Contains(FGeneralObject4);
    Success3 := TestMap.Contains(FGeneralObject1);
    Success4 := TestMap.Contains(FGeneralObject2);
    Success5 := not TestMap.Contains(FGeneralObject3);
    Success6 := not TestMap.Contains(FGeneralObject4);
    Success7 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and
        Success6 and Success7;
end;

function TFunctionalTest.Retain_ArrStrMap: Boolean;
var
    TestMap: IStringMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5, Success6, Success7: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray1234, FGeneralArray1234);
    RemovedCollection := TestMap.Retain(FGeneralArray12);
    Success1 := RemovedCollection.Contains(FGeneralObject3);
    Success2 := RemovedCollection.Contains(FGeneralObject4);
    Success3 := TestMap.Contains(FGeneralObject1);
    Success4 := TestMap.Contains(FGeneralObject2);
    Success5 := not TestMap.Contains(FGeneralObject3);
    Success6 := not TestMap.Contains(FGeneralObject4);
    Success7 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and
        Success6 and Success7;
end;

function TFunctionalTest.Retain_IF: Boolean;
var
    Collection: ICollection;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5, Success6, Success7: Boolean;
begin
    Collection := CollectionClass.Create(FGeneralCollection1234);
    RemovedCollection := Collection.Retain(FGeneralCollection12);
    Success1 := RemovedCollection.Contains(FGeneralObject3);
    Success2 := RemovedCollection.Contains(FGeneralObject4);
    Success3 := Collection.Contains(FGeneralObject1);
    Success4 := Collection.Contains(FGeneralObject2);
    Success5 := not Collection.Contains(FGeneralObject3);
    Success6 := not Collection.Contains(FGeneralObject4);
    Success7 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and
        Success6 and Success7;
end;

function TFunctionalTest.Retain_IFList: Boolean;
var
    TestList: IList;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5, Success6, Success7: Boolean;
    Success8, Success9, Success10, Success11, Success12, Success13, Success14: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralCollection1234);
    TestList.Sorted := False;
    RemovedCollection := TestList.Retain(FGeneralCollection12);
    Success1 := RemovedCollection.Contains(FGeneralObject3);
    Success2 := RemovedCollection.Contains(FGeneralObject4);
    Success3 := TestList.Contains(FGeneralObject1);
    Success4 := TestList.Contains(FGeneralObject2);
    Success5 := not TestList.Contains(FGeneralObject3);
    Success6 := not TestList.Contains(FGeneralObject4);
    Success7 := (RemovedCollection.GetSize = 2);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralCollection1234);
    TestList.Sorted := True;
    RemovedCollection := TestList.Retain(FGeneralCollection12);
    Success8 := RemovedCollection.Contains(FGeneralObject3);
    Success9 := RemovedCollection.Contains(FGeneralObject4);
    Success10 := TestList.Contains(FGeneralObject1);
    Success11 := TestList.Contains(FGeneralObject2);
    Success12 := not TestList.Contains(FGeneralObject3);
    Success13 := not TestList.Contains(FGeneralObject4);
    Success14 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and
        Success6 and Success7 and Success8 and Success9 and Success10 and
        Success11 and Success12 and Success13 and Success14;
end;

function TFunctionalTest.Retain_IFMap: Boolean;
var
    TestMap: IMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5, Success6, Success7: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray1234, FGeneralArray1234);
    RemovedCollection := TestMap.Retain(FGeneralCollection12);
    Success1 := RemovedCollection.Contains(FGeneralObject3);
    Success2 := RemovedCollection.Contains(FGeneralObject4);
    Success3 := TestMap.Contains(FGeneralObject1);
    Success4 := TestMap.Contains(FGeneralObject2);
    Success5 := not TestMap.Contains(FGeneralObject3);
    Success6 := not TestMap.Contains(FGeneralObject4);
    Success7 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and
        Success6 and Success7;
end;

function TFunctionalTest.Retain_IFIntMap: Boolean;
var
    TestMap: IIntegerMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5, Success6, Success7: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray1234, FGeneralArray1234);
    RemovedCollection := TestMap.Retain(FGeneralCollection12);
    Success1 := RemovedCollection.Contains(FGeneralObject3);
    Success2 := RemovedCollection.Contains(FGeneralObject4);
    Success3 := TestMap.Contains(FGeneralObject1);
    Success4 := TestMap.Contains(FGeneralObject2);
    Success5 := not TestMap.Contains(FGeneralObject3);
    Success6 := not TestMap.Contains(FGeneralObject4);
    Success7 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and
        Success6 and Success7;
end;

function TFunctionalTest.Retain_IFStrMap: Boolean;
var
    TestMap: IStringMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5, Success6, Success7: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray1234, FGeneralArray1234);
    RemovedCollection := TestMap.Retain(FGeneralCollection12);
    Success1 := RemovedCollection.Contains(FGeneralObject3);
    Success2 := RemovedCollection.Contains(FGeneralObject4);
    Success3 := TestMap.Contains(FGeneralObject1);
    Success4 := TestMap.Contains(FGeneralObject2);
    Success5 := not TestMap.Contains(FGeneralObject3);
    Success6 := not TestMap.Contains(FGeneralObject4);
    Success7 := (RemovedCollection.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5 and
        Success6 and Success7;
end;

function TFunctionalTest.SetComparator: Boolean;
var
    Collection: ICollection;
    TestComparator: IComparator;
begin
    Collection := CollectionClass.Create;
    TestComparator := TTestComparator_Nat.Create;
    Collection.SetComparator(TestComparator);
    Result := (Collection.GetComparator = TestComparator);
end;

function TFunctionalTest.SetIgnoreErrors: Boolean;
var
    Collection: ICollection;
    Success1, Success2: Boolean;
begin
    Collection := CollectionClass.Create(True);
    Collection.IgnoreErrors := [];
    Success1 := (Collection.GetIgnoreErrors = []);
    Collection.IgnoreErrors := [ceDuplicate, ceDuplicateKey, ceFixedSize, ceNilNotAllowed, ceNotNaturalItem, ceOutOfRange];
    Success2 := (Collection.GetIgnoreErrors = [ceDuplicate, ceDuplicateKey, ceFixedSize, ceNilNotAllowed, ceNotNaturalItem, ceOutOfRange]);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Complement: Boolean;
var
    TestSet, Universe, ResultSet: ISet;
    Success1: Boolean;
begin
    Universe := TPArraySet.Create(FGeneralArray1234);
    TestSet := TAbstractSetClass(CollectionClass).Create(FGeneralArray12);
    ResultSet := TestSet.Complement(Universe);
    Success1 := not ResultSet.Contains(FGeneralObject1) and
        not ResultSet.Contains(FGeneralObject2) and
        ResultSet.Contains(FGeneralObject3) and
        ResultSet.Contains(FGeneralObject4);
    Result := Success1;
end;

function TFunctionalTest.Intersect: Boolean;
var
    TestSet, Set2, Set3, ResultSet: ISet;
    Success1, Success2, Success3, Success4, Success5: Boolean;
begin
    Set2 := TPArraySet.Create;
    Set2.Add(FGeneralObject3);
    Set2.Add(FGeneralObject4);
    Set3 := TPArraySet.Create;
    Set3.Add(FGeneralObject2);
    Set3.Add(FGeneralObject3);
    // Empty receiving collection/ empty parameter
    TestSet := TAbstractSetClass(CollectionClass).Create;
    ResultSet := TestSet.Intersect(FEmptySet);
    Success1 := ResultSet.IsEmpty;
    // Empty receiving collection/ non-empty parameter
    TestSet := TAbstractSetClass(CollectionClass).Create;
    ResultSet := TestSet.Intersect(Set2);
    Success2 := ResultSet.IsEmpty;
    // Non-empty receiving collection/ empty parameter
    TestSet := TAbstractSetClass(CollectionClass).Create(FGeneralArray12);
    ResultSet := TestSet.Intersect(FEmptySet);
    Success3 := ResultSet.IsEmpty;
    // Non-empty receiving collection/ non-empty parameter with no common items
    TestSet := TAbstractSetClass(CollectionClass).Create(FGeneralArray12);
    ResultSet := TestSet.Intersect(Set2);
    Success4 := ResultSet.IsEmpty;
    // Non-empty receiving collection/ non-empty parameter with common items
    TestSet := TAbstractSetClass(CollectionClass).Create(FGeneralArray12);
    ResultSet := TestSet.Intersect(Set3);
    Success5 := not ResultSet.Contains(FGeneralObject1) and
        ResultSet.Contains(FGeneralObject2) and
        not ResultSet.Contains(FGeneralObject3) and
        not ResultSet.Contains(FGeneralObject4);
    Result := Success1 and Success2 and Success3 and Success4 and Success5;
end;

function TFunctionalTest.Union: Boolean;
var
    TestSet, Set2, Set3, ResultSet: ISet;
    Success1, Success2, Success3, Success4, Success5: Boolean;
begin
    Set2 := TPArraySet.Create;
    Set2.Add(FGeneralObject3);
    Set2.Add(FGeneralObject4);
    Set3 := TPArraySet.Create;
    Set3.Add(FGeneralObject2);
    Set3.Add(FGeneralObject3);
    // Empty receiving collection/ empty parameter
    TestSet := TAbstractSetClass(CollectionClass).Create;
    ResultSet := TestSet.Union(FEmptySet);
    Success1 := ResultSet.IsEmpty;
    // Empty receiving collection/ non-empty parameter
    TestSet := TAbstractSetClass(CollectionClass).Create;
    ResultSet := TestSet.Union(Set2);
    Success2 := (ResultSet.Size = 2) and ResultSet.Contains(FGeneralObject3) and ResultSet.Contains(FGeneralObject4);
    // Non-empty receiving collection/ empty parameter
    TestSet := TAbstractSetClass(CollectionClass).Create(FGeneralArray12);
    ResultSet := TestSet.Union(FEmptySet);
    Success3 := (ResultSet.Size = 2) and ResultSet.Contains(FGeneralObject1) and ResultSet.Contains(FGeneralObject2);
    // Non-empty receiving collection/ non-empty parameter with no common items
    TestSet := TAbstractSetClass(CollectionClass).Create(FGeneralArray12);
    ResultSet := TestSet.Union(Set2);
    Success4 := (ResultSet.Size = 4) and ResultSet.Contains(FGeneralObject1) and
        ResultSet.Contains(FGeneralObject2) and ResultSet.Contains(FGeneralObject3) and
        ResultSet.Contains(FGeneralObject4);
    // Non-empty receiving collection/ non-empty parameter with common items
    TestSet := TAbstractSetClass(CollectionClass).Create(FGeneralArray12);
    ResultSet := TestSet.Union(Set3);
    Success5 := (ResultSet.Size = 3) and ResultSet.Contains(FGeneralObject1) and
        ResultSet.Contains(FGeneralObject2) and ResultSet.Contains(FGeneralObject3) and
        not ResultSet.Contains(FGeneralObject4);

    Result := Success1 and Success2 and Success3 and Success4 and Success5;
end;

function TFunctionalTest.Delete_Fix: Boolean;
var
    TestList: IList;
    Success1, Success2: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    TestList.IgnoreErrors := [ceFixedSize];
    Success1 := (TestList.Delete(0) = nil);
    Success2 := (TestList.GetSize = 2);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Delete_NonFix: Boolean;
var
    TestList: IList;
    Success1, Success2: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    Success1 := (TestList.Delete(0) = FGeneralObject1);
    Success2 := (TestList.GetSize = 1);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Exchange: Boolean;
var
    TestList: IList;
    Success1, Success2: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    TestList.Exchange(0, 1);
    Success1 := (TestList[0] = FGeneralObject2);
    Success2 := (TestList[1] = FGeneralObject1);
    Result := Success1 and Success2;
end;

function TFunctionalTest.First: Boolean;
var
    TestList: IList;
    Success1, Success2: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create;
    Success1 := (TestList.First = nil);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    Success2 := (TestList.First = FGeneralObject1);
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetDuplicates: Boolean;
var
    TestList: IList;
begin
    TestList := TAbstractListClass(CollectionClass).Create;
    Result := TestList.GetDuplicates;
end;

function TFunctionalTest.GetSorted: Boolean;
var
    TestList: IList;
begin
    TestList := TAbstractListClass(CollectionClass).Create;
    Result := not TestList.GetSorted;
end;

function TFunctionalTest.IndexOf: Boolean;
var
    TestList: IList;
    Success1, Success2: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create;
    Success1 := (TestList.First = nil);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    Success2 := (TestList.First = FGeneralObject1);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Insert_Fix: Boolean;
var
    TestList: IList;
    Success1, Success2: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    TestList.IgnoreErrors := [ceFixedSize];
    Success1 := not (TestList.Insert(0, FGeneralObject3));
    Success2 := (TestList.GetSize = 2);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Insert_NonFix: Boolean;
var
    TestList: IList;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    TestList.SetSorted(False);
    Success1 := TestList.Insert(1, FGeneralObject3);
    Success2 := (TestList.GetSize = 3) and (TestList[1] = FGeneralObject3);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    TestList.SetSorted(True);
    Success3 := TestList.Insert(1, FGeneralObject3);
    Success4 := (TestList.GetSize = 3) and (TestList[1] = FGeneralObject3) and
        not TestList.GetSorted;
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Insert_Arr: Boolean;
var
    TestList: IList;
    Success1, Success2, Success3: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    Success1 := (TestList.Insert(1, FGeneralArray34) = 2);
    Success2 := (TestList[1] = FGeneralObject3);
    Success3 := (TestList[2] = FGeneralObject4);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Insert_IF: Boolean;
var
    TestList: IList;
    Success1, Success2, Success3: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralCollection12);
    Success1 := (TestList.Insert(1, FGeneralCollection34) = 2);
    Success2 := (TestList[1] = FGeneralObject3);
    Success3 := (TestList[2] = FGeneralObject4);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Last: Boolean;
var
    TestList: IList;
    Success1, Success2: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create;
    Success1 := (TestList.Last = nil);
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    Success2 := (TestList.Last = FGeneralObject2);
    Result := Success1 and Success2;
end;

function TFunctionalTest.SetDuplicates: Boolean;
var
    TestList: IList;
    Success1, Success2: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(True);
    TestList.SetDuplicates(False);
    Success1 := (TestList.GetDuplicates = False);
    TestList.SetDuplicates(True);
    Success2 := (TestList.GetDuplicates = True);
    Result := Success1 and Success2;
end;

function TFunctionalTest.SetItem: Boolean;
var
    TestList: IList;
    Success1, Success2: Boolean;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12);
    TestList.SetDuplicates(False);
    TestList.IgnoreErrors := [ceDuplicate];
    TestList.SetItem(1, FGeneralObject3);
    Success1 := (TestList[1] = FGeneralObject3);
    TestList.SetItem(1, FGeneralObject1);
    Success2 := (TestList[1] <> FGeneralObject1);
    Result := Success1 and Success2;
end;

function TFunctionalTest.SetSorted: Boolean;
var
    TestList: IList;
    Array1: TCollectableArray;
    Success1, Success2: Boolean;
begin
    SetLength(Array1, 2);
    Array1[0] := FGeneralObject2;
    Array1[1] := FGeneralObject1;
    TestList := TAbstractListClass(CollectionClass).Create(Array1, True);
    TestList.SetSorted(False);
    Success1 := (TestList.GetSorted = False) and (TestList.First = FGeneralObject2);
    TestList.SetSorted(True);
    Success2 := (TestList.GetSorted = True) and (TestList.First = FGeneralObject1);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Sort: Boolean;
var
    TestList: IList;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FGeneralArray12, True);
    TestList.Sort(TAbstractComparator.GetReverseNaturalComparator);
    Result := (TestList.First = FGeneralObject2) and (TestList.Last = FGeneralObject1);
end;

function TFunctionalTest.ContainsKey_NonNat: Boolean;
var
    TestMap: IMap;
    Success1, Success2: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNonNaturalArray12, FNonNaturalArray12, False, False);
    Success1 := not TestMap.ContainsKey(FNonNaturalObject3);
    Success2 := TestMap.ContainsKey(FNonNaturalObject1);
    Result := Success1 and Success2;
end;

function TFunctionalTest.ContainsKey_Nat: Boolean;
var
    TestMap: IMap;
    Success1, Success2: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12, False, True);
    Success1 := not TestMap.ContainsKey(FGeneralObject3);
    Success2 := TestMap.ContainsKey(FGeneralObject1);
    Result := Success1 and Success2;
end;

function TFunctionalTest.ContainsKey_Arr: Boolean;
var
    TestMap: IMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12);
    Success1 := TestMap.ContainsKey(FEmptyArray);
    Success2 := not TestMap.ContainsKey(FGeneralArray34);
    Success3 := not TestMap.ContainsKey(FGeneralArray23);
    Success4 := TestMap.ContainsKey(FGeneralArray12);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.ContainsKey_IF: Boolean;
var
    TestMap: IMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12);
    Success1 := TestMap.ContainsKey(FEmptyCollection);
    Success2 := not TestMap.ContainsKey(FGeneralCollection34);
    Success3 := not TestMap.ContainsKey(FGeneralCollection23);
    Success4 := TestMap.ContainsKey(FGeneralCollection12);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Get_NonNat: Boolean;
var
    TestMap: IMap;
    Success1, Success2: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNonNaturalArray12, FNonNaturalArray12, False, False);
    Success1 := (TestMap.Get(FNonNaturalObject3) = nil);
    Success2 := (TestMap.Get(FNonNaturalObject1) <> nil);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Get_Nat: Boolean;
var
    TestMap: IMap;
    Success1, Success2: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12, False, True);
    Success1 := (TestMap.Get(FGeneralObject3) = nil);
    Success2 := (TestMap.Get(FGeneralObject1) <> nil);
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetKeyComparator_NonNat: Boolean;
var
    TestMap: IMap;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(False, False);
    Result := (TestMap.GetKeyComparator = TAbstractComparator.GetDefaultComparator);
end;

function TFunctionalTest.GetKeyComparator_Nat: Boolean;
var
    TestMap: IMap;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(False, True);
    Result := (TestMap.GetKeyComparator = TAbstractComparator.GetNaturalComparator);
end;

function TFunctionalTest.GetKeyIterator: Boolean;
var
    TestMap: IMap;
    Iterator: IIterator;
    Success1, Success2: Boolean;
begin
    Success1 := False;
    Success2 := False;
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12);
    Iterator := TestMap.GetKeyIterator;
    while not Iterator.EOF do
    begin
        if Iterator.CurrentItem = FGeneralObject1 then
            Success1 := True
        else if Iterator.CurrentItem = FGeneralObject2 then
            Success2 := True;
        Iterator.Next;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetKeys: Boolean;
var
    TestMap: IMap;
    KeySet: ISet;
    Success1: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12);
    KeySet := TestMap.GetKeys;
    Success1 := KeySet.Contains(FGeneralObject1) and KeySet.Contains(FGeneralObject2);
    Result := Success1;
end;

function TFunctionalTest.GetMapIterator: Boolean;
var
    TestMap: IMap;
    Iterator: IMapIterator;
    Success1, Success2: Boolean;
begin
    Success1 := False;
    Success2 := False;
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FGeneralArray34);
    Iterator := TestMap.GetMapIterator;
    while not Iterator.EOF do
    begin
        if (Iterator.CurrentKey = FGeneralObject1) and (Iterator.CurrentItem = FGeneralObject3) then
            Success1 := True
        else if (Iterator.CurrentKey = FGeneralObject2) and (Iterator.CurrentItem = FGeneralObject4) then
            Success2 := True;
        Iterator.Next;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetMapIteratorByKey_Filter: Boolean;
var
    TestMap: IMap;
    Iterator: IMapIterator;
    Success1, Success2, Failed1: Boolean;
begin
    Success1 := False;
    Success2 := False;
    Failed1 := False;
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray1234, FGeneralArray1234);
    Iterator := TestMap.GetMapIteratorByKey(TTestFilter_Nat.Create('13') as IFilter);
    while not Iterator.EOF do
    begin
        if (Iterator.CurrentKey = FGeneralObject1) and (Iterator.CurrentItem = FGeneralObject1) then
            Success1 := True
        else if (Iterator.CurrentKey = FGeneralObject3) and (Iterator.CurrentItem = FGeneralObject3) then
            Success2 := True
        else
            Failed1 := True;
        Iterator.Next;
    end;
    Result := Success1 and Success2 and not Failed1;
end;

function TFunctionalTest.GetMapIteratorByKey_Func: Boolean;
var
    TestMap: IMap;
    Iterator: IMapIterator;
    Success1, Success2, Failed1: Boolean;
begin
    Success1 := False;
    Success2 := False;
    Failed1 := False;
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray1234, FGeneralArray1234);
    Iterator := TestMap.GetMapIteratorByKey(NaturalFilter34);
    while not Iterator.EOF do
    begin
        if (Iterator.CurrentKey = FGeneralObject3) and (Iterator.CurrentItem = FGeneralObject3) then
            Success1 := True
        else if (Iterator.CurrentKey = FGeneralObject4) and (Iterator.CurrentItem = FGeneralObject4) then
            Success2 := True
        else
            Failed1 := True;
        Iterator.Next;
    end;
    Result := Success1 and Success2 and not Failed1;
end;

function TFunctionalTest.GetNaturalKeyIID_Equ: Boolean;
var
    TestMap: IMap;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create;
    Result := TMiscCollectionLibrary.EqualIID(TestMap.GetNaturalKeyIID, EquatableIID);
end;

function TFunctionalTest.GetNaturalKeyIID_Comp: Boolean;
var
    TestMap: IMap;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create;
    Result := TMiscCollectionLibrary.EqualIID(TestMap.GetNaturalKeyIID, ComparableIID);
end;

function TFunctionalTest.GetNaturalKeyIID_Hash: Boolean;
var
    TestMap: IMap;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create;
    Result := TMiscCollectionLibrary.EqualIID(TestMap.GetNaturalKeyIID, HashableIID);
end;

function TFunctionalTest.GetNaturalKeysOnly_Both: Boolean;
var
    TestMap: IMap;
    Success1, Success2: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(False, False);
    Success1 := (TestMap.GetNaturalKeysOnly = False);
    TestMap := TAbstractMapClass(CollectionClass).Create(False, True);
    Success2 := (TestMap.GetNaturalKeysOnly = True);
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetNaturalKeysOnly_Nat: Boolean;
var
    TestMap: IMap;
    Success1, Success2: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(False, False);
    Success1 := (TestMap.GetNaturalKeysOnly = True);
    TestMap := TAbstractMapClass(CollectionClass).Create(False, True);
    Success2 := (TestMap.GetNaturalKeysOnly = True);
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetValues: Boolean;
var
    TestMap: IMap;
    ValueCollection: ICollection;
    Success1: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12);
    ValueCollection := TestMap.GetValues;
    Success1 := ValueCollection.Contains(FNonNaturalObject1) and ValueCollection.Contains(FNonNaturalObject2);
    Result := Success1;
end;

function TFunctionalTest.KeyAllowed_NonNat: Boolean;
var
    TestMap: IMap;
    Success1, Success2: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(False, False);
    Success1 := (TestMap.KeyAllowed(FNonNaturalObject1) = ceOK);
    Success2 := (TestMap.KeyAllowed(FGeneralObject1) = ceOK);
    Result := Success1 and Success2;
end;

function TFunctionalTest.KeyAllowed_Nat: Boolean;
var
    TestMap: IMap;
    Success1, Success2: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(False, True);
    Success1 := (TestMap.KeyAllowed(FNonNaturalObject1) = ceNotNaturalItem);
    Success2 := (TestMap.KeyAllowed(FGeneralObject1) = ceOK);
    Result := Success1 and Success2;
end;

function TFunctionalTest.MatchingKey_ArrNonNat: Boolean;
var
    TestMap: IMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(False, False);
    Success1 := TestMap.MatchingKey(FEmptyArray).IsEmpty;
    Success2 := TestMap.MatchingKey(FNonNaturalArray12).IsEmpty;
    TestMap := TAbstractMapClass(CollectionClass).Create(FNonNaturalArray1234, FNonNaturalArray1234, False, False);
    Success3 := TestMap.MatchingKey(FEmptyArray).IsEmpty;
    Success4 := (TestMap.MatchingKey(FNonNaturalArray12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.MatchingKey_ArrNat: Boolean;
var
    TestMap: IMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(False, True);
    Success1 := TestMap.MatchingKey(FEmptyArray).IsEmpty;
    Success2 := TestMap.MatchingKey(FGeneralArray12).IsEmpty;
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray1234, FNonNaturalArray1234, False, True);
    Success3 := TestMap.MatchingKey(FEmptyArray).IsEmpty;
    Success4 := (TestMap.MatchingKey(FGeneralArray12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.MatchingKey_IFNonNat: Boolean;
var
    TestMap: IMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(False, False);
    Success1 := TestMap.MatchingKey(FEmptyCollection).IsEmpty;
    Success2 := TestMap.MatchingKey(FNonNaturalCollection12).IsEmpty;
    TestMap := TAbstractMapClass(CollectionClass).Create(FNonNaturalArray1234, FNonNaturalArray1234, False, False);
    Success3 := TestMap.MatchingKey(FEmptyCollection).IsEmpty;
    Success4 := (TestMap.MatchingKey(FNonNaturalCollection12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.MatchingKey_IFNat: Boolean;
var
    TestMap: IMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(False, True);
    Success1 := TestMap.MatchingKey(FEmptyCollection).IsEmpty;
    Success2 := TestMap.MatchingKey(FGeneralCollection12).IsEmpty;
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray1234, FNonNaturalArray1234, False, True);
    Success3 := TestMap.MatchingKey(FEmptyCollection).IsEmpty;
    Success4 := (TestMap.MatchingKey(FGeneralCollection12).GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Put_Item: Boolean;
var
    TestMap: IMap;
    Success1, Success2, Success3: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create;
    Success1 := False;
    try
        // This should fail
        TestMap.Put(FGeneralObject1);
    except
        Success1 := True;
    end;

    Success2 := (TestMap.Put(FNaturalKeyMapObject1) = nil) and TestMap.Contains(FNaturalKeyMapObject1);
    Success3 := (TestMap.Put(FNaturalKeyMapObject1) <> nil) and TestMap.Contains(FNaturalKeyMapObject1);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Put_KeyItem: Boolean;
var
    TestMap: IMap;
    Success1, Success2: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create;
    Success1 := (TestMap.Put(FGeneralObject1, FNonNaturalObject1) = nil) and TestMap.Contains(FNonNaturalObject1);
    Success2 := (TestMap.Put(FGeneralObject1, FNonNaturalObject2) = FNonNaturalObject1) and TestMap.Contains(FNonNaturalObject2);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Put_Arr: Boolean;
var
    Testmap: IMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create;
    RemovedCollection := TestMap.Put(FNaturalMapItemArray12);
    Success1 := RemovedCollection.IsEmpty;
    Success2 := TestMap.Contains(FNaturalKeyMapObject1) and TestMap.Contains(FNaturalKeyMapObject2);
    TestMap := TAbstractMapClass(CollectionClass).Create(FNaturalMapItemArray12);
    RemovedCollection := TestMap.Put(FNaturalMapItemArray12);
    Success3 := RemovedCollection.Contains(FNaturalKeyMapObject1) and RemovedCollection.Contains(FNaturalKeyMapObject2);
    Success4 := TestMap.Contains(FNaturalKeyMapObject1) and TestMap.Contains(FNaturalKeyMapObject2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Put_IF: Boolean;
var
    Testmap: IMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create;
    RemovedCollection := TestMap.Put(FNaturalItemMap12 as ICollection);
    Success1 := RemovedCollection.IsEmpty;
    Success2 := TestMap.Contains(FNaturalKeyMapObject1) and TestMap.Contains(FNaturalKeyMapObject2);
    TestMap := TAbstractMapClass(CollectionClass).Create(FNaturalMapItemArray12);
    RemovedCollection := TestMap.Put(FNaturalItemMap12 as ICollection);
    Success3 := RemovedCollection.Contains(FNaturalKeyMapObject1) and RemovedCollection.Contains(FNaturalKeyMapObject2);
    Success4 := TestMap.Contains(FNaturalKeyMapObject1) and TestMap.Contains(FNaturalKeyMapObject2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Put_IMap: Boolean;
var
    TestMap: IMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create;
    RemovedCollection := TestMap.Put(FNaturalItemMap12 as IMap);
    Success1 := RemovedCollection.IsEmpty;
    Success2 := TestMap.Contains(FNaturalKeyMapObject1) and TestMap.Contains(FNaturalKeyMapObject2);
    TestMap := TAbstractMapClass(CollectionClass).Create(FNaturalMapItemArray12);
    RemovedCollection := TestMap.Put(FNaturalItemMap12 as IMap);
    Success3 := RemovedCollection.Contains(FNaturalKeyMapObject1) and RemovedCollection.Contains(FNaturalKeyMapObject2);
    Success4 := TestMap.Contains(FNaturalKeyMapObject1) and TestMap.Contains(FNaturalKeyMapObject2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.RemoveKey_Key: Boolean;
var
    TestMap: IMap;
    Success1, Success2, Success3: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray12, FNonNaturalArray12);
    Success1 := (TestMap.RemoveKey(FGeneralObject3) = nil);
    Success2 := (TestMap.RemoveKey(FGeneralObject2) = FNonNaturalObject2);
    Success3 := not TestMap.ContainsKey(FGeneralObject2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.RemoveKey_Arr: Boolean;
var
    TestMap: IMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray1234, FNonNaturalArray1234);
    RemovedCollection := TestMap.RemoveKey(FGeneralArray12);
    Success1 := RemovedCollection.Contains(FNonNaturalObject1);
    Success2 := RemovedCollection.Contains(FNonNaturalObject2);
    Success3 := not TestMap.ContainsKey(FGeneralObject1);
    Success4 := not TestMap.ContainsKey(FGeneralObject2);
    Success5 := (RemovedCollection.GetSize = 2) and (TestMap.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5;
end;

function TFunctionalTest.RemoveKey_IF: Boolean;
var
    TestMap: IMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray1234, FNonNaturalArray1234);
    RemovedCollection := TestMap.RemoveKey(FGeneralCollection12);
    Success1 := RemovedCollection.Contains(FNonNaturalObject1);
    Success2 := RemovedCollection.Contains(FNonNaturalObject2);
    Success3 := not TestMap.ContainsKey(FGeneralObject1);
    Success4 := not TestMap.ContainsKey(FGeneralObject2);
    Success5 := (RemovedCollection.GetSize = 2) and (TestMap.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5;
end;

function TFunctionalTest.RetainKey_Arr: Boolean;
var
    TestMap: IMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray1234, FNonNaturalArray1234);
    RemovedCollection := TestMap.RetainKey(FGeneralArray12);
    Success1 := RemovedCollection.Contains(FNonNaturalObject3);
    Success2 := RemovedCollection.Contains(FNonNaturalObject4);
    Success3 := TestMap.ContainsKey(FGeneralObject1);
    Success4 := TestMap.ContainsKey(FGeneralObject2);
    Success5 := (RemovedCollection.GetSize = 2) and (TestMap.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5;
end;

function TFunctionalTest.RetainKey_IF: Boolean;
var
    TestMap: IMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5: Boolean;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FGeneralArray1234, FNonNaturalArray1234);
    RemovedCollection := TestMap.RetainKey(FGeneralCollection12);
    Success1 := RemovedCollection.Contains(FNonNaturalObject3);
    Success2 := RemovedCollection.Contains(FNonNaturalObject4);
    Success3 := TestMap.ContainsKey(FGeneralObject1);
    Success4 := TestMap.ContainsKey(FGeneralObject2);
    Success5 := (RemovedCollection.GetSize = 2) and (TestMap.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5;
end;

function TFunctionalTest.ContainsKey_Int: Boolean;
var
    TestMap: IIntegerMap;
    Success1, Success2: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FNonNaturalArray12, False);
    Success1 := not TestMap.ContainsKey(3);
    Success2 := TestMap.ContainsKey(1);
    Result := Success1 and Success2;
end;

function TFunctionalTest.ContainsKey_ArrInt: Boolean;
var
    TestMap: IIntegerMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FNonNaturalArray12);
    Success1 := TestMap.ContainsKey(FEmptyFIntegerArray);
    Success2 := not TestMap.ContainsKey(FIntegerArray34);
    Success3 := not TestMap.ContainsKey(FIntegerArray23);
    Success4 := TestMap.ContainsKey(FIntegerArray12);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Get_Int: Boolean;
var
    TestMap: IIntegerMap;
    Success1, Success2: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FNonNaturalArray12, False);
    Success1 := (TestMap.Get(3) = nil);
    Success2 := (TestMap.Get(1) <> nil);
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetKeys_Int: Boolean;
var
    TestMap: IIntegerMap;
    KeySet: ISet;
    Success1: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FNonNaturalArray12);
    KeySet := TestMap.GetKeys;
    Success1 := KeySet.Contains(TIntegerWrapper.Create(1) as ICollectable) and KeySet.Contains(TIntegerWrapper.Create(2) as ICollectable);
    Result := Success1;
end;

function TFunctionalTest.GetMapIterator_Int: Boolean;
var
    TestMap: IIntegerMap;
    Iterator: IIntegerMapIterator;
    Success1, Success2: Boolean;
begin
    Success1 := False;
    Success2 := False;
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FGeneralArray34);
    Iterator := TestMap.GetMapIterator;
    while not Iterator.EOF do
    begin
        if (Iterator.CurrentKey = 1) and (Iterator.CurrentItem = FGeneralObject3) then
            Success1 := True
        else if (Iterator.CurrentKey = 2) and (Iterator.CurrentItem = FGeneralObject4) then
            Success2 := True;
        Iterator.Next;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetValues_Int: Boolean;
var
    TestMap: IIntegerMap;
    ValueCollection: ICollection;
    Success1: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FNonNaturalArray12);
    ValueCollection := TestMap.GetValues;
    Success1 := ValueCollection.Contains(FNonNaturalObject1) and ValueCollection.Contains(FNonNaturalObject2);
    Result := Success1;
end;

function TFunctionalTest.Put_ItemInt: Boolean;
var
    TestMap: IIntegerMap;
    Success1, Success2, Success3: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create;
    Success1 := False;
    try
        // This should fail
        TestMap.Put(FGeneralObject1);
    except
        Success1 := True;
    end;

    Success2 := (TestMap.Put(FIntegerMapObject1) = nil) and TestMap.Contains(FIntegerMapObject1);
    Success3 := (TestMap.Put(FIntegerMapObject1) <> nil) and TestMap.Contains(FIntegerMapObject1);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Put_KeyItemInt: Boolean;
var
    TestMap: IIntegerMap;
    Success1, Success2: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create;
    Success1 := (TestMap.Put(1, FNonNaturalObject1) = nil) and TestMap.Contains(FNonNaturalObject1);
    Success2 := (TestMap.Put(1, FNonNaturalObject2) = FNonNaturalObject1) and TestMap.Contains(FNonNaturalObject2);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Put_ArrInt: Boolean;
var
    Testmap: IIntegerMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create;
    RemovedCollection := TestMap.Put(FIntegerMapItemArray12);
    Success1 := RemovedCollection.IsEmpty;
    Success2 := TestMap.Contains(FIntegerMapObject1) and TestMap.Contains(FIntegerMapObject2);
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerMapItemArray12);
    RemovedCollection := TestMap.Put(FIntegerMapItemArray12);
    Success3 := RemovedCollection.Contains(FIntegerMapObject1) and RemovedCollection.Contains(FIntegerMapObject2);
    Success4 := TestMap.Contains(FIntegerMapObject1) and TestMap.Contains(FIntegerMapObject2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Put_IFInt: Boolean;
var
    Testmap: IIntegerMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create;
    RemovedCollection := TestMap.Put(FNaturalItemIntMap12 as ICollection);
    Success1 := RemovedCollection.IsEmpty;
    Success2 := TestMap.Contains(FIntegerMapObject1) and TestMap.Contains(FIntegerMapObject2);
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerMapItemArray12);
    RemovedCollection := TestMap.Put(FNaturalItemIntMap12 as ICollection);
    Success3 := RemovedCollection.Contains(FIntegerMapObject1) and RemovedCollection.Contains(FIntegerMapObject2);
    Success4 := TestMap.Contains(FIntegerMapObject1) and TestMap.Contains(FIntegerMapObject2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Put_IIntMap: Boolean;
var
    TestMap: IIntegerMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create;
    RemovedCollection := TestMap.Put(FNaturalItemIntMap12 as IIntegerMap);
    Success1 := RemovedCollection.IsEmpty;
    Success2 := TestMap.Contains(FIntegerMapObject1) and TestMap.Contains(FIntegerMapObject2);
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerMapItemArray12);
    RemovedCollection := TestMap.Put(FNaturalItemIntMap12 as IIntegerMap);
    Success3 := RemovedCollection.Contains(FIntegerMapObject1) and RemovedCollection.Contains(FIntegerMapObject2);
    Success4 := TestMap.Contains(FIntegerMapObject1) and TestMap.Contains(FIntegerMapObject2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.RemoveKey_KeyInt: Boolean;
var
    TestMap: IIntegerMap;
    Success1, Success2, Success3: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray12, FNonNaturalArray12);
    Success1 := (TestMap.RemoveKey(3) = nil);
    Success2 := (TestMap.RemoveKey(2) = FNonNaturalObject2);
    Success3 := not TestMap.ContainsKey(2);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.RemoveKey_ArrInt: Boolean;
var
    TestMap: IIntegerMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray1234, FNonNaturalArray1234);
    RemovedCollection := TestMap.RemoveKey(FIntegerArray12);
    Success1 := RemovedCollection.Contains(FNonNaturalObject1);
    Success2 := RemovedCollection.Contains(FNonNaturalObject2);
    Success3 := not TestMap.ContainsKey(1);
    Success4 := not TestMap.ContainsKey(2);
    Success5 := (RemovedCollection.GetSize = 2) and (TestMap.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5;
end;

function TFunctionalTest.RetainKey_ArrInt: Boolean;
var
    TestMap: IIntegerMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5: Boolean;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray1234, FNonNaturalArray1234);
    RemovedCollection := TestMap.RetainKey(FIntegerArray12);
    Success1 := RemovedCollection.Contains(FNonNaturalObject3);
    Success2 := RemovedCollection.Contains(FNonNaturalObject4);
    Success3 := TestMap.ContainsKey(1);
    Success4 := TestMap.ContainsKey(2);
    Success5 := (RemovedCollection.GetSize = 2) and (TestMap.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5;
end;

function TFunctionalTest.ContainsKey_Str: Boolean;
var
    TestMap: IStringMap;
    Success1, Success2: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FNonNaturalArray12, False);
    Success1 := not TestMap.ContainsKey('Item3');
    Success2 := TestMap.ContainsKey('Item1');
    Result := Success1 and Success2;
end;

function TFunctionalTest.ContainsKey_ArrStr: Boolean;
var
    TestMap: IStringMap;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FNonNaturalArray12);
    Success1 := TestMap.ContainsKey(FEmptyFStringArray);
    Success2 := not TestMap.ContainsKey(FStringArray34);
    Success3 := not TestMap.ContainsKey(FStringArray23);
    Success4 := TestMap.ContainsKey(FStringArray12);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Get_Str: Boolean;
var
    TestMap: IStringMap;
    Success1, Success2: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FNonNaturalArray12, False);
    Success1 := (TestMap.Get('Item33') = nil);
    Success2 := (TestMap.Get('Item1') <> nil);
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetKeys_Str: Boolean;
var
    TestMap: IStringMap;
    KeySet: ISet;
    Success1: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FNonNaturalArray12);
    KeySet := TestMap.GetKeys;
    Success1 := KeySet.Contains(TStringWrapper.Create('Item1') as ICollectable) and KeySet.Contains(TStringWrapper.Create('Item2') as ICollectable);
    Result := Success1;
end;

function TFunctionalTest.GetMapIterator_Str: Boolean;
var
    TestMap: IStringMap;
    Iterator: IStringMapIterator;
    Success1, Success2: Boolean;
begin
    Success1 := False;
    Success2 := False;
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FGeneralArray34);
    Iterator := TestMap.GetMapIterator;
    while not Iterator.EOF do
    begin
        if (Iterator.CurrentKey = 'Item1') and (Iterator.CurrentItem = FGeneralObject3) then
            Success1 := True
        else if (Iterator.CurrentKey = 'Item2') and (Iterator.CurrentItem = FGeneralObject4) then
            Success2 := True;
        Iterator.Next;
    end;
    Result := Success1 and Success2;
end;

function TFunctionalTest.GetValues_Str: Boolean;
var
    TestMap: IStringMap;
    ValueCollection: ICollection;
    Success1: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FNonNaturalArray12);
    ValueCollection := TestMap.GetValues;
    Success1 := ValueCollection.Contains(FNonNaturalObject1) and ValueCollection.Contains(FNonNaturalObject2);
    Result := Success1;
end;

function TFunctionalTest.Put_ItemStr: Boolean;
var
    TestMap: IStringMap;
    Success1, Success2, Success3: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create;
    Success1 := False;
    try
        // This should fail
        TestMap.Put(FGeneralObject1);
    except
        Success1 := True;
    end;

    Success2 := (TestMap.Put(FStringMapObject1) = nil) and TestMap.Contains(FStringMapObject1);
    Success3 := (TestMap.Put(FStringMapObject1) <> nil) and TestMap.Contains(FStringMapObject1);
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.Put_KeyItemStr: Boolean;
var
    TestMap: IStringMap;
    Success1, Success2: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create;
    Success1 := (TestMap.Put('Item1', FNonNaturalObject1) = nil) and TestMap.Contains(FNonNaturalObject1);
    Success2 := (TestMap.Put('Item1', FNonNaturalObject2) = FNonNaturalObject1) and TestMap.Contains(FNonNaturalObject2);
    Result := Success1 and Success2;
end;

function TFunctionalTest.Put_ArrStr: Boolean;
var
    Testmap: IStringMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create;
    RemovedCollection := TestMap.Put(FStringMapItemArray12);
    Success1 := RemovedCollection.IsEmpty;
    Success2 := TestMap.Contains(FStringMapObject1) and TestMap.Contains(FStringMapObject2);
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringMapItemArray12);
    RemovedCollection := TestMap.Put(FStringMapItemArray12);
    Success3 := RemovedCollection.Contains(FStringMapObject1) and RemovedCollection.Contains(FStringMapObject2);
    Success4 := TestMap.Contains(FStringMapObject1) and TestMap.Contains(FStringMapObject2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Put_IFStr: Boolean;
var
    Testmap: IStringMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create;
    RemovedCollection := TestMap.Put(FNaturalItemStrMap12 as ICollection);
    Success1 := RemovedCollection.IsEmpty;
    Success2 := TestMap.Contains(FStringMapObject1) and TestMap.Contains(FStringMapObject2);
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringMapItemArray12);
    RemovedCollection := TestMap.Put(FNaturalItemStrMap12 as ICollection);
    Success3 := RemovedCollection.Contains(FStringMapObject1) and RemovedCollection.Contains(FStringMapObject2);
    Success4 := TestMap.Contains(FStringMapObject1) and TestMap.Contains(FStringMapObject2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.Put_IStrMap: Boolean;
var
    TestMap: IStringMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create;
    RemovedCollection := TestMap.Put(FNaturalItemStrMap12 as IStringMap);
    Success1 := RemovedCollection.IsEmpty;
    Success2 := TestMap.Contains(FStringMapObject1) and TestMap.Contains(FStringMapObject2);
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringMapItemArray12);
    RemovedCollection := TestMap.Put(FNaturalItemStrMap12 as IStringMap);
    Success3 := RemovedCollection.Contains(FStringMapObject1) and RemovedCollection.Contains(FStringMapObject2);
    Success4 := TestMap.Contains(FStringMapObject1) and TestMap.Contains(FStringMapObject2);
    Result := Success1 and Success2 and Success3 and Success4;
end;

function TFunctionalTest.RemoveKey_KeyStr: Boolean;
var
    TestMap: IStringMap;
    Success1, Success2, Success3: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray12, FNonNaturalArray12);
    Success1 := (TestMap.RemoveKey('Item3') = nil);
    Success2 := (TestMap.RemoveKey('Item2') = FNonNaturalObject2);
    Success3 := not TestMap.ContainsKey('Item2');
    Result := Success1 and Success2 and Success3;
end;

function TFunctionalTest.RemoveKey_ArrStr: Boolean;
var
    TestMap: IStringMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray1234, FNonNaturalArray1234);
    RemovedCollection := TestMap.RemoveKey(FStringArray12);
    Success1 := RemovedCollection.Contains(FNonNaturalObject1);
    Success2 := RemovedCollection.Contains(FNonNaturalObject2);
    Success3 := not TestMap.ContainsKey('Item1');
    Success4 := not TestMap.ContainsKey('Item2');
    Success5 := (RemovedCollection.GetSize = 2) and (TestMap.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5;
end;

function TFunctionalTest.RetainKey_ArrStr: Boolean;
var
    TestMap: IStringMap;
    RemovedCollection: ICollection;
    Success1, Success2, Success3, Success4, Success5: Boolean;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray1234, FNonNaturalArray1234);
    RemovedCollection := TestMap.RetainKey(FStringArray12);
    Success1 := RemovedCollection.Contains(FNonNaturalObject3);
    Success2 := RemovedCollection.Contains(FNonNaturalObject4);
    Success3 := TestMap.ContainsKey('Item1');
    Success4 := TestMap.ContainsKey('Item2');
    Success5 := (RemovedCollection.GetSize = 2) and (TestMap.GetSize = 2);
    Result := Success1 and Success2 and Success3 and Success4 and Success5;
end;


{ TNonNaturalItem }
constructor TNonNaturalItem.Create(Name: String);
begin
    FName := Name;
end;

{ TTestComparator_Nat }
function TTestComparator_Nat.Compare(const Item1, Item2: ICollectable): Integer;
begin
    // Not used
    Result := 0;
end;

function TTestComparator_Nat.Equals(const Item1, Item2: ICollectable): Boolean;
begin
    // Not used
    Result := True;
end;


{ TTestFilter_NonNat }
constructor TTestFilter_NonNat.Create(AllowedChars: String);
begin
    FAllowedChars := AllowedChars;
end;

function TTestFilter_NonNat.Accept(const Item: ICollectable): Boolean;
var
    Value: String;
    I: Integer;
    Success: Boolean;
begin
    Value := TNonNaturalItem(Item.GetInstance).Name;
    Success := False;
    for I := 1 to Length(Value) do
    begin
        Success := (Pos(Value[I], FAllowedChars) > 0);
        if Success then
            break;
    end;
    Result := Success;
end;

{ TTestFilter_Nat }
constructor TTestFilter_Nat.Create(AllowedChars: String);
begin
    FAllowedChars := AllowedChars;
end;

function TTestFilter_Nat.Accept(const Item: ICollectable): Boolean;
var
    Value: String;
    I: Integer;
    Success: Boolean;
begin
    Value := (Item as IString).Value;
    Success := False;
    for I := 1 to Length(Value) do
    begin
        Success := (Pos(Value[I], FAllowedChars) > 0);
        if Success then
            break;
    end;
    Result := Success;
end;

{ TTestFilter_NatMap }
constructor TTestFilter_NatMap.Create(AllowedChars: String);
begin
    FAllowedChars := AllowedChars;
end;

function TTestFilter_NatMap.Accept(const Item: ICollectable): Boolean;
var
    Value: String;
    I: Integer;
    Success: Boolean;
begin
    Value := TStringWrapper((Item as IAssociationWrapper).Value).Value;
    Success := False;
    for I := 1 to Length(Value) do
    begin
        Success := (Pos(Value[I], FAllowedChars) > 0);
        if Success then
            break;
    end;
    Result := Success;
end;

{ TTestFilter_NatIntMap }
constructor TTestFilter_NatIntMap.Create(AllowedChars: String);
begin
    FAllowedChars := AllowedChars;
end;

function TTestFilter_NatIntMap.Accept(const Item: ICollectable): Boolean;
var
    Value: String;
    I: Integer;
    Success: Boolean;
begin
    Value := TStringWrapper((Item as IIntegerAssociationWrapper).Value).Value;
    Success := False;
    for I := 1 to Length(Value) do
    begin
        Success := (Pos(Value[I], FAllowedChars) > 0);
        if Success then
            break;
    end;
    Result := Success;
end;

{ TTestFilter_NatStrMap }
constructor TTestFilter_NatStrMap.Create(AllowedChars: String);
begin
    FAllowedChars := AllowedChars;
end;

function TTestFilter_NatStrMap.Accept(const Item: ICollectable): Boolean;
var
    Value: String;
    I: Integer;
    Success: Boolean;
begin
    Value := TStringWrapper((Item as IStringAssociationWrapper).Value).Value;
    Success := False;
    for I := 1 to Length(Value) do
    begin
        Success := (Pos(Value[I], FAllowedChars) > 0);
        if Success then
            break;
    end;
    Result := Success;
end;


end.
