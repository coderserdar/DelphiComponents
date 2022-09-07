unit PerformanceTest;

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
 * $Version: v1.0.3 $
 * $Revision: 1.0 $
 * $Log: D:\QVCS Repositories\Delphi Collections\Test\PerformanceTest.qbt $
 * 
 *   Initial version.
 * 
 * Revision 1.0  by: Matthew Greet  Rev date: 06/04/03 11:22:26
 *   Initial revision.
 * 
 * $Endlog$
 *****************************************************************************)

interface

uses
    Classes, SysUtils, Math, Dialogs,
    Collections, CollWrappers, CollArray, CollPArray, CollHash, CollList, CollLibrary;

type
    TPerformanceTestDesc = class(TAbstractStringMappable)
    private
        FTestName: String;
        FDisplayGroup: Integer;
        FDisplayGroupOrder: Integer;
        FDescription: String;
    protected
        function MakeKey: String; override;
    public
        constructor Create(TestName: String; DisplayGroup, DisplayGroupOrder: Integer; Description: String);
        property TestName: String read FTestName;
        property DisplayGroup: Integer read FDisplayGroup;
        property DisplayGroupOrder: Integer read FDisplayGroupOrder;
        property Description: String read FDescription;
    end;

    TPerformanceTestClassDesc = class(TAbstractStringMappable)
    private
        FTestClassName: String;
        FNonCollection: Boolean;
        FCollectionType: TCollectionType;
    protected
        function MakeKey: String; override;
    public
        constructor Create(TestClassName: String; NonCollection: Boolean; CollectionType: TCollectionType);
        property TestClassName: String read FTestClassName;
        property NonCollection: Boolean read FNonCollection;
        property CollectionType: TCollectionType read FCollectionType;
    end;

    TPerformanceTestResult = class(TAbstractStringMappable)
    private
        FTestClassName: String;
        FTestName: String;
        FMilliseconds: Integer;
    protected
        function MakeKey: String; override;
    public
        constructor Create(TestClass: TAbstractCollectionClass; TestName: String); overload;
        constructor Create(TestClassName: String; TestName: String); overload;
        property TestClassName: String read FTestClassName;
        property TestName: String read FTestName;
        property Milliseconds: Integer read FMilliseconds write FMilliseconds;
    end;

    TPerformanceTest = class
    private
        FCollectionClass: TAbstractCollectionClass;
        FCollectionSize: Integer;
        FIterations: Integer;
        FResultDescTable: IStringMap;
        FResultTable: IStringMap;
        FUseArray: Boolean;
        FUseTList: Boolean;
        FCurrentTestName: String;
        FCurrentIteration: Integer;
        FOnIterationStart: TNotifyEvent;
        FOnIterationComplete: TNotifyEvent;
        // It would seem strange not to use my own collections, even after
        // successful functional tests, but arrays are the simplest and fastest
        // collections and will affect timings the least.
        FNonNaturalArray: TCollectableArray;
        FRandomNonNaturalArray: TCollectableArray;
        FNaturalArray: TCollectableArray;
        FRandomNaturalArray: TCollectableArray;
        FIntegerArray: TIntegerArray;
        FRandomIntegerArray: TIntegerArray;
        FStringArray: TStringArray;
        FRandomStringArray: TStringArray;
        FMappableItemArray: TCollectableArray;
        FRandomMappableItemArray: TCollectableArray;
        FIntegerMappableItemArray: TCollectableArray;
        FRandomIntegerMappableItemArray: TCollectableArray;
        FStringMappableItemArray: TCollectableArray;
        FRandomStringMappableItemArray: TCollectableArray;
        FRandomIndexArray: array of Integer;
        procedure SetCollectionClass(Value: TAbstractCollectionClass);
        procedure SetUseArray(Value: Boolean);
        procedure SetUseTList(Value: Boolean);
        procedure AddTestList;
        procedure AddArrayTestList;
        procedure AddTListTestList;
        procedure DoTest(TestResult: TPerformanceTestResult);
        procedure InitializeTest;
        procedure MakeResultDescTable;
        function NonNaturalCompare(Item1, Item2: Pointer): Integer;
        procedure ShuffleRandomArrays;
    protected
        procedure NotifyIterationStart(TestName: String; Iteration: Integer);
        procedure NotifyIterationComplete(TestName: String; Iteration: Integer);
    public
        constructor Create;
        procedure TestClass;
        property CollectionClass: TAbstractCollectionClass read FCollectionClass write SetCollectionClass;
        property CollectionSize: Integer read FCollectionSize write FCollectionSize;
        property Iterations: Integer read FIterations write FIterations;
        property ResultDescTable: IStringMap read FResultDescTable;
        property ResultTable: IStringMap read FResultTable;
        property UseArray: Boolean read FUseArray write SetUseArray;
        property UseTList: Boolean read FUseTList write SetUseTList;
        property CurrentTestName: String read FCurrentTestname;
        property CurrentIteration: Integer read FCurrentIteration;
        property OnIterationStart: TNotifyEvent read FOnIterationStart write FOnIterationStart;
        property OnIterationComplete: TNotifyEvent read FOnIterationComplete write FOnIterationComplete;
    published
        function CreateNonNatural: Integer;
        function CreateNatural: Integer;
        function AddNonNaturalRandom: Integer;
        function AddNonNaturalAscending: Integer;
        function AddNonNaturalDescending: Integer;
        function AddNaturalRandom: Integer;
        function AddNaturalAscending: Integer;
        function AddNaturalDescending: Integer;
        function AddSortedNonNaturalRandom: Integer;
        function AddSortedNonNaturalAscending: Integer;
        function AddSortedNonNaturalDescending: Integer;
        function AddSortedNaturalRandom: Integer;
        function AddSortedNaturalAscending: Integer;
        function AddSortedNaturalDescending: Integer;
        function InsertNonNaturalRandom: Integer;
        function InsertNonNaturalBegin: Integer;
        function InsertNonNaturalEnd: Integer;
        function InsertNaturalRandom: Integer;
        function InsertNaturalBegin: Integer;
        function InsertNaturalEnd: Integer;
        function PutNonNaturalRandom: Integer;
        function PutNonNaturalAscending: Integer;
        function PutNonNaturalDescending: Integer;
        function PutNaturalRandom: Integer;
        function PutNaturalAscending: Integer;
        function PutNaturalDescending: Integer;
        function PutNaturalRandomInt: Integer;
        function PutNaturalAscendingInt: Integer;
        function PutNaturalDescendingInt: Integer;
        function PutNaturalRandomStr: Integer;
        function PutNaturalAscendingStr: Integer;
        function PutNaturalDescendingStr: Integer;
        function RemoveNonNaturalRandom: Integer;
        function RemoveNonNaturalAscending: Integer;
        function RemoveNonNaturalDescending: Integer;
        function RemoveNaturalRandom: Integer;
        function RemoveNaturalAscending: Integer;
        function RemoveNaturalDescending: Integer;
        function RemoveSortedNonNaturalRandom: Integer;
        function RemoveSortedNonNaturalAscending: Integer;
        function RemoveSortedNonNaturalDescending: Integer;
        function RemoveSortedNaturalRandom: Integer;
        function RemoveSortedNaturalAscending: Integer;
        function RemoveSortedNaturalDescending: Integer;
        function RemoveKeyNonNaturalRandom: Integer;
        function RemoveKeyNonNaturalAscending: Integer;
        function RemoveKeyNonNaturalDescending: Integer;
        function RemoveKeyNaturalRandom: Integer;
        function RemoveKeyNaturalAscending: Integer;
        function RemoveKeyNaturalDescending: Integer;
        function RemoveKeyNaturalRandomInt: Integer;
        function RemoveKeyNaturalAscendingInt: Integer;
        function RemoveKeyNaturalDescendingInt: Integer;
        function RemoveKeyNaturalRandomStr: Integer;
        function RemoveKeyNaturalAscendingStr: Integer;
        function RemoveKeyNaturalDescendingStr: Integer;
        function ContainsNonNaturalRandom: Integer;
        function ContainsNonNaturalFirst: Integer;
        function ContainsNonNaturalLast: Integer;
        function ContainsNaturalRandom: Integer;
        function ContainsNaturalFirst: Integer;
        function ContainsNaturalLast: Integer;
        function ContainsSortedNonNaturalRandom: Integer;
        function ContainsSortedNonNaturalFirst: Integer;
        function ContainsSortedNonNaturalLast: Integer;
        function ContainsSortedNaturalRandom: Integer;
        function ContainsSortedNaturalFirst: Integer;
        function ContainsSortedNaturalLast: Integer;
        function ContainsKeyNonNaturalRandom: Integer;
        function ContainsKeyNonNaturalFirst: Integer;
        function ContainsKeyNonNaturalLast: Integer;
        function ContainsKeyNaturalRandom: Integer;
        function ContainsKeyNaturalFirst: Integer;
        function ContainsKeyNaturalLast: Integer;
        function ContainsKeyNaturalRandomInt: Integer;
        function ContainsKeyNaturalFirstInt: Integer;
        function ContainsKeyNaturalLastInt: Integer;
        function ContainsKeyNaturalRandomStr: Integer;
        function ContainsKeyNaturalFirstStr: Integer;
        function ContainsKeyNaturalLastStr: Integer;
        function GetItemRandom: Integer;
        function GetItemFirst: Integer;
        function GetItemLast: Integer;
        function GetNonNaturalRandom: Integer;
        function GetNonNaturalFirst: Integer;
        function GetNonNaturalLast: Integer;
        function GetNaturalRandom: Integer;
        function GetNaturalFirst: Integer;
        function GetNaturalLast: Integer;
        function GetNaturalRandomInt: Integer;
        function GetNaturalFirstInt: Integer;
        function GetNaturalLastInt: Integer;
        function GetNaturalRandomStr: Integer;
        function GetNaturalFirstStr: Integer;
        function GetNaturalLastStr: Integer;
        function Iterator: Integer;
        function Array_CreateNonNatural: Integer;
        function Array_CreateNatural: Integer;
        function Array_ContainsNonNaturalRandom: Integer;
        function Array_ContainsNonNaturalFirst: Integer;
        function Array_ContainsNonNaturalLast: Integer;
        function Array_ContainsNaturalRandom: Integer;
        function Array_ContainsNaturalFirst: Integer;
        function Array_ContainsNaturalLast: Integer;
        function Array_GetItemRandom: Integer;
        function Array_GetItemFirst: Integer;
        function Array_GetItemLast: Integer;
        function Array_Iterator: Integer;
        function TList_CreateNonNatural: Integer;
        function TList_CreateNatural: Integer;
        function TList_AddNonNaturalRandom: Integer;
        function TList_AddNonNaturalAscending: Integer;
        function TList_AddNonNaturalDescending: Integer;
        function TList_AddNaturalRandom: Integer;
        function TList_AddNaturalAscending: Integer;
        function TList_AddNaturalDescending: Integer;
        function TList_AddSortedNonNaturalRandom: Integer;
        function TList_AddSortedNonNaturalAscending: Integer;
        function TList_AddSortedNonNaturalDescending: Integer;
        function TList_AddSortedNaturalRandom: Integer;
        function TList_AddSortedNaturalAscending: Integer;
        function TList_AddSortedNaturalDescending: Integer;
        function TList_InsertNonNaturalRandom: Integer;
        function TList_InsertNonNaturalBegin: Integer;
        function TList_InsertNonNaturalEnd: Integer;
        function TList_InsertNaturalRandom: Integer;
        function TList_InsertNaturalBegin: Integer;
        function TList_InsertNaturalEnd: Integer;
        function TList_RemoveNonNaturalRandom: Integer;
        function TList_RemoveNonNaturalAscending: Integer;
        function TList_RemoveNonNaturalDescending: Integer;
        function TList_RemoveNaturalRandom: Integer;
        function TList_RemoveNaturalAscending: Integer;
        function TList_RemoveNaturalDescending: Integer;
        function TList_RemoveSortedNonNaturalRandom: Integer;
        function TList_RemoveSortedNonNaturalAscending: Integer;
        function TList_RemoveSortedNonNaturalDescending: Integer;
        function TList_RemoveSortedNaturalRandom: Integer;
        function TList_RemoveSortedNaturalAscending: Integer;
        function TList_RemoveSortedNaturalDescending: Integer;
        function TList_ContainsNonNaturalRandom: Integer;
        function TList_ContainsNonNaturalFirst: Integer;
        function TList_ContainsNonNaturalLast: Integer;
        function TList_ContainsNaturalRandom: Integer;
        function TList_ContainsNaturalFirst: Integer;
        function TList_ContainsNaturalLast: Integer;
        function TList_GetItemRandom: Integer;
        function TList_GetItemFirst: Integer;
        function TList_GetItemLast: Integer;
        function TList_Iterator: Integer;
    end;

implementation

type
    TTestMethod = function: Integer of object;

    TNonNaturalItem = class(TAbstractItem)
    private
        FName: String;
    public
        constructor Create(Name: String);
        property Name: String read FName;
    end;

    TTestComparator_NonNat = class(TAbstractComparator)
    public
        function Compare(const Item1, Item2: ICollectable): Integer; override;
        function Equals(const Item1, Item2: ICollectable): Boolean; override;
    end;


{ TPerformanceTestDesc }
constructor TPerformanceTestDesc.Create(TestName: String; DisplayGroup, DisplayGroupOrder: Integer; Description: String);
begin
    inherited Create;
    FTestName := TestName;
    FDisplayGroup := DisplayGroup;
    FDisplayGroupOrder := DisplayGroupOrder;
    FDescription := Description;
end;

function TPerformanceTestDesc.MakeKey: String;
begin
    Result := FTestName;
end;


{ TPerformanceTestClassDesc }
constructor TPerformanceTestClassDesc.Create(TestClassName: String; NonCollection: Boolean; CollectionType: TCollectionType);
begin
    inherited Create;
    FTestClassName := TestClassName;
    FNonCollection := NonCollection;
    FCollectionType := CollectionType;
end;

function TPerformanceTestClassDesc.MakeKey: String;
begin
    Result := TestClassName;
end;


{ TPerformanceTestResult }
constructor TPerformanceTestResult.Create(TestClass: TAbstractCollectionClass; TestName: String);
begin
    inherited Create;
    FTestClassName := TestClass.ClassName;
    FTestName := TestName;
    FMilliseconds := 0;
end;

constructor TPerformanceTestResult.Create(TestClassName: String; TestName: String);
begin
    inherited Create;
    FTestClassName := TestClassName;
    FTestName := TestName;
    FMilliseconds := 0;
end;

function TPerformanceTestResult.MakeKey: String;
begin
    Result := FTestClassName + '.' + FTestName;
end;


{ TPerformanceTest }
constructor TPerformanceTest.Create;
begin
    FResultTable := THashStringMap.Create;
    MakeResultDescTable;
end;

procedure TPerformanceTest.SetCollectionClass(Value: TAbstractCollectionClass);
begin
    FCollectionClass := Value;
    FUseArray := false;
    FUseTList := false;
end;

procedure TPerformanceTest.SetUseArray(Value: Boolean);
begin
    FUseArray := Value;
    FUseTList := false;
end;

procedure TPerformanceTest.SetUseTList(Value: Boolean);
begin
    FUseArray := false;
    FUseTList := Value;
end;

procedure TPerformanceTest.MakeResultDescTable;
begin
    FResultDescTable := THashStringMap.Create;
    FResultDescTable.Add(TPerformanceTestDesc.Create('CreateNonNatural',                1, 1, 'Create - non-natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('CreateNatural',                   1, 2, 'Create - natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('AddNonNaturalRandom',             2, 1, 'Add - random order, non-natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('AddNonNaturalAscending',          2, 2, 'Add - ascending order, non-natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('AddNonNaturalDescending',         2, 3, 'Add - descending order, non-natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('AddNaturalRandom',                2, 4, 'Add - random order, natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('AddNaturalAscending',             2, 5, 'Add - ascending order, natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('AddNaturalDescending',            2, 6, 'Add - descending order, natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('AddSortedNonNaturalRandom',       3, 1, 'Add - random order, non-natural items, sorted list') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('AddSortedNonNaturalAscending',    3, 2, 'Add - ascending order, non-natural items, sorted list') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('AddSortedNonNaturalDescending',   3, 3, 'Add - descending order, non-natural items, sorted list') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('AddSortedNaturalRandom',          3, 4, 'Add - random order, natural items, sorted list') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('AddSortedNaturalAscending',       3, 5, 'Add - ascending order, natural items, sorted list') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('AddSortedNaturalDescending',      3, 6, 'Add - descending order, natural items, sorted list') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('InsertNonNaturalRandom',          4, 1, 'Insert - random order, non-natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('InsertNonNaturalBegin',           4, 2, 'Insert - at start, non-natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('InsertNonNaturalEnd',             4, 3, 'Insert - at end, non-natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('InsertNaturalRandom',             4, 4, 'Insert - random order, natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('InsertNaturalBegin',              4, 5, 'Insert - at start, natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('InsertNaturalEnd',                4, 6, 'Insert - at end, natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('PutNonNaturalRandom',             5, 1, 'Put - random order, non-natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('PutNonNaturalAscending',          5, 2, 'Put - ascending order, non-natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('PutNonNaturalDescending',         5, 3, 'Put - descending order, non-natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('PutNaturalRandom',                5, 4, 'Put - random order, natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('PutNaturalAscending',             5, 5, 'Put - ascending order, natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('PutNaturalDescending',            5, 6, 'Put - descending order, natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('PutNaturalRandomInt',             6, 1, 'Put - random order, integer keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('PutNaturalAscendingInt',          6, 2, 'Put - ascending order, integer keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('PutNaturalDescendingInt',         6, 3, 'Put - descending order, integer keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('PutNaturalRandomStr',             7, 1, 'Put - random order, string keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('PutNaturalAscendingStr',          7, 2, 'Put - ascending order, string keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('PutNaturalDescendingStr',         7, 3, 'Put - descending order, string keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveNonNaturalRandom',          8, 1, 'Remove - random order, non-natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveNonNaturalAscending',       8, 2, 'Remove - ascending order, non-natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveNonNaturalDescending',      8, 3, 'Remove - descending order, non-natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveNaturalRandom',             8, 4, 'Remove - random order, natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveNaturalAscending',          8, 5, 'Remove - ascending order, natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveNaturalDescending',         8, 6, 'Remove - descending order, natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveSortedNonNaturalRandom',    9, 1, 'Remove - random order, non-natural items, sorted list') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveSortedNonNaturalAscending', 9, 2, 'Remove - ascending order, non-natural items, sorted list') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveSortedNonNaturalDescending', 9, 3, 'Remove - descending order, non-natural items, sorted list') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveSortedNaturalRandom',       9, 4, 'Remove - random order, natural items, sorted list') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveSortedNaturalAscending',    9, 5, 'Remove - ascending order, natural items, sorted list') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveSortedNaturalDescending',   9, 6, 'Remove - descending order, natural items, sorted list') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveKeyNonNaturalRandom',       10, 1, 'RemoveKey - random order, non-natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveKeyNonNaturalAscending',    10, 2, 'RemoveKey - ascending order, non-natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveKeyNonNaturalDescending',   10, 3, 'RemoveKey - descending order, non-natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveKeyNaturalRandom',          10, 4, 'RemoveKey - random order, natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveKeyNaturalAscending',       10, 5, 'RemoveKey - ascending order, natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveKeyNaturalDescending',      10, 6, 'RemoveKey - descending order, natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveKeyNaturalRandomInt',       11, 1, 'RemoveKey - random order, integer keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveKeyNaturalAscendingInt',    11, 2, 'RemoveKey - ascending order, integer keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveKeyNaturalDescendingInt',   11, 3, 'RemoveKey - descending order, integer keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveKeyNaturalRandomStr',       12, 1, 'RemoveKey - random order, string keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveKeyNaturalAscendingStr',    12, 2, 'RemoveKey - ascending order, string keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('RemoveKeyNaturalDescendingStr',   12, 3, 'RemoveKey - descending order, string keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsNonNaturalRandom',        13, 1, 'Contains - random order, non-natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsNonNaturalFirst',         13, 2, 'Contains - first item, non-natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsNonNaturalLast',          13, 3, 'Contains - last item, non-natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsNaturalRandom',           13, 4, 'Contains - random order, natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsNaturalFirst',            13, 5, 'Contains - first item, natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsNaturalLast',             13, 6, 'Contains - last item, natural items') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsSortedNonNaturalRandom',  14, 1, 'Contains - random order, non-natural items, sorted list') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsSortedNonNaturalFirst',   14, 2, 'Contains - first item, non-natural items, sorted list') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsSortedNonNaturalLast',    14, 3, 'Contains - last item, non-natural items, sorted list') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsSortedNaturalRandom',     14, 4, 'Contains - random order, natural items, sorted list') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsSortedNaturalFirst',      14, 5, 'Contains - first item, natural items, sorted list') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsSortedNaturalLast',       14, 6, 'Contains - last item, natural items, sorted list') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsKeyNonNaturalRandom',     15, 1, 'ContainsKey - random order, non-natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsKeyNonNaturalFirst',      15, 2, 'ContainsKey - first item, non-natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsKeyNonNaturalLast',       15, 3, 'ContainsKey - last item, non-natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsKeyNaturalRandom',        15, 4, 'ContainsKey - random order, natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsKeyNaturalFirst',         15, 5, 'ContainsKey - first item, natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsKeyNaturalLast',          15, 6, 'ContainsKey - last item, natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsKeyNaturalRandomInt',     16, 1, 'ContainsKey - random order, integer keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsKeyNaturalFirstInt',      16, 2, 'ContainsKey - first item, integer keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsKeyNaturalLastInt',       16, 3, 'ContainsKey - last item, integer keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsKeyNaturalRandomStr',     17, 1, 'ContainsKey - random order, string keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsKeyNaturalFirstStr',      17, 2, 'ContainsKey - first item, string keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('ContainsKeyNaturalLastStr',       17, 3, 'ContainsKey - last item, string keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('GetItemRandom',                   18, 1, 'GetItem - random order') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('GetItemFirst',                    18, 2, 'GetItem - first item') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('GetItemLast',                     18, 3, 'GetItem - last item') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('GetNonNaturalRandom',             19, 1, 'Get - random order, non-natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('GetNonNaturalFirst',              19, 2, 'Get - first item, non-natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('GetNonNaturalLast',               19, 3, 'Get - last item, non-natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('GetNaturalRandom',                19, 4, 'Get - random order, natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('GetNaturalFirst',                 19, 5, 'Get - first item, natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('GetNaturalLast',                  19, 6, 'Get - last item, natural keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('GetNaturalRandomInt',             20, 1, 'Get - random order, integer keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('GetNaturalFirstInt',              20, 2, 'Get - first item, integer keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('GetNaturalLastInt',               20, 3, 'Get - last item, integer keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('GetNaturalRandomStr',             21, 1, 'Get - random order, string keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('GetNaturalFirstStr',              21, 2, 'Get - first item, string keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('GetNaturalLastStr',               21, 3, 'Get - last item, string keys') as ICollectable);
    FResultDescTable.Add(TPerformanceTestDesc.Create('Iterator',                        22, 1, 'Iterator') as ICollectable);
end;

procedure TPerformanceTest.NotifyIterationStart(TestName: String; Iteration: Integer);
begin
    FCurrentTestName := TestName;
    FCurrentIteration := Iteration;
    FOnIterationStart(Self);
end;

procedure TPerformanceTest.NotifyIterationComplete(TestName: String; Iteration: Integer);
begin
    FCurrentTestName := TestName;
    FCurrentIteration := Iteration;
    FOnIterationComplete(Self);
end;

procedure TPerformanceTest.TestClass;
var
    TestResult: TPerformanceTestResult;
    Iterator: IIterator;
    I: Integer;
begin
    InitializeTest;
    ResultTable.Clear;
    if FUseArray then
        AddArrayTestList
    else if FUseTList then
        AddTListTestList
    else
        AddTestList;
    Iterator := ResultTable.GetIterator;
    for I := 1 to Iterations do
    begin
        Iterator.First;
        ShuffleRandomArrays;
        while not Iterator.EOF do
        begin
            TestResult := TPerformanceTestResult(Iterator.CurrentItem.GetInstance);
            NotifyIterationStart(TestResult.TestName, I);
            DoTest(TestResult);
            NotifyIterationComplete(TestResult.TestName, I);
            Iterator.Next;
        end;
    end;
    Iterator.First;
    while not Iterator.EOF do
    begin
        TestResult := TPerformanceTestResult(Iterator.CurrentItem.GetInstance);
        TestResult.Milliseconds := TestResult.Milliseconds div Iterations;
        Iterator.Next;
    end;
end;

procedure TPerformanceTest.InitializeTest;
var
    Item: ICollectable;
    ItemText, NumberText: String;
    I, Digits: Integer;
begin
    SetLength(FNonNaturalArray, CollectionSize);
    SetLength(FRandomNonNaturalArray, CollectionSize);
    SetLength(FNaturalArray, CollectionSize);
    SetLength(FRandomNaturalArray, CollectionSize);
    SetLength(FIntegerArray, CollectionSize);
    SetLength(FRandomIntegerArray, CollectionSize);
    SetLength(FStringArray, CollectionSize);
    SetLength(FRandomStringArray, CollectionSize);
    SetLength(FMappableItemArray, CollectionSize);
    SetLength(FRandomMappableItemArray, CollectionSize);
    SetLength(FIntegerMappableItemArray, CollectionSize);
    SetLength(FRandomIntegerMappableItemArray, CollectionSize);
    SetLength(FStringMappableItemArray, CollectionSize);
    SetLength(FRandomStringMappableItemArray, CollectionSize);
    SetLength(FRandomIndexArray, CollectionSize);
    Digits := Trunc(Log10(CollectionSize));
    for I := 0 to CollectionSize - 1 do
    begin
        NumberText := IntToStr(I + 1);
        ItemText := 'Item' + StringOfChar('0', Digits - Length(NumberText)) + NumberText;

        Item := TNonNaturalItem.Create(ItemText);
        FNonNaturalArray[I] := Item;
        FRandomNonNaturalArray[I] := Item;

        Item := TStringWrapper.Create(ItemText);
        FNaturalArray[I] :=Item;
        FRandomNaturalArray[I] :=Item;

        FIntegerArray[I] := I;
        FRandomIntegerArray[I] := I;
        FStringArray[I] := ItemText;
        FRandomStringArray[I] := ItemText;

        Item := TAssociationWrapper.Create(ItemText, TObject.Create);
        FMappableItemArray[I] := Item;
        FRandomMappableItemArray[I] := Item;

        Item := TIntegerAssociationWrapper.Create(I, TObject.Create);
        FIntegerMappableItemArray[I] := Item;
        FRandomIntegerMappableItemArray[I] := Item;

        Item := TStringAssociationWrapper.Create(ItemText, TObject.Create);
        FStringMappableItemArray[I] := Item;
        FRandomStringMappableItemArray[I] := Item;

        FRandomIndexArray[I] := I;
    end;
end;

function TPerformanceTest.NonNaturalCompare(Item1, Item2: Pointer): Integer;
var
    Value1, Value2: String;
begin
    Value1 := TNonNaturalItem(ICollectable(Item1).GetInstance).Name;
    Value2 := TNonNaturalItem(ICollectable(Item2).GetInstance).Name;
    if Value1 < Value2 then
        Result := -1
    else if Value1 > Value2 then
        Result := 1
    else
        Result := 0;
end;

procedure TPerformanceTest.ShuffleRandomArrays;
var
    ArraySize, I, Index, Temp: Integer;
begin
    TMiscCollectionLibrary.ShuffleArray(FRandomNonNaturalArray);
    TMiscCollectionLibrary.ShuffleArray(FRandomNaturalArray);
    TMiscCollectionLibrary.ShuffleArray(FRandomMappableItemArray);
    ArraySize := Length(FRandomIndexArray);
    for I := 0 to ArraySize - 1 do
    begin
        Index := (I + Random(ArraySize - 1) + 1) mod ArraySize;
        Temp := FRandomIndexArray[I];
        FRandomIndexArray[I] := FRandomIndexArray[Index];
        FRandomIndexArray[Index] := Temp;
    end;
end;

procedure TPerformanceTest.DoTest(TestResult: TPerformanceTestResult);
var
    TestMethod: TTestMethod;
    Method: TMethod;
    P: Pointer;
    TestName: String;
begin
    TestName := TestResult.TestName;
    if FUseArray then
        TestName := 'Array_' + TestName
    else if FUseTList then
        TestName := 'TList_' + TestName;
    P := TPerformanceTest.MethodAddress(TestName);
    if Assigned(P) then
    begin
        Method.Code := P;
        Method.Data := Self;
        TestMethod := TTestMethod(Method);
        try
            TestResult.Milliseconds := TestResult.Milliseconds + TestMethod;
        except
            on E: Exception do
            begin
                ShowMessage(E.Message);
                TestResult.Milliseconds := -1;
            end
        end;
    end
    else
        TestResult.Milliseconds := -1;
end;

procedure TPerformanceTest.AddTestList;
var
    Collection: ICollection;
    CollectionType: TCollectionType;
    FixedSize: Boolean;
    AlwaysNaturalItems: Boolean;
    AlwaysNaturalKeys: Boolean;
begin
    if CollectionClass <> nil then
    begin
        try
            Collection := CollectionClass.Create;
        except
            Exit;
        end;
    end;
    CollectionType := Collection.GetType;
    FixedSize := Collection.GetFixedSize;
    AlwaysNaturalItems := CollectionClass.GetAlwaysNaturalItems;
    if CollectionType = ctMap then
        AlwaysNaturalKeys := TAbstractMapClass(CollectionClass).GetAlwaysNaturalKeys
    else
        AlwaysnaturalKeys := false;

    if not AlwaysNaturalItems and ((CollectionType = ctBag) or (CollectionType = ctSet) or (CollectionType = ctList)) then
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'CreateNonNatural') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'CreateNatural') as ICollectable);
    if not FixedSize then
    begin
        if not AlwaysNaturalItems and((CollectionType = ctBag) or (CollectionType = ctSet) or (CollectionType = ctList)) then
        begin
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'AddNonNaturalRandom') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'AddNonNaturalAscending') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'AddNonNaturalDescending') as ICollectable);
        end;
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'AddNaturalRandom') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'AddNaturalAscending') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'AddNaturalDescending') as ICollectable);
        if CollectionType = ctList then
        begin
            if not AlwaysNaturalItems then
            begin
                FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'AddSortedNonNaturalRandom') as ICollectable);
                FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'AddSortedNonNaturalAscending') as ICollectable);
                FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'AddSortedNonNaturalDescending') as ICollectable);
            end;
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'AddSortedNaturalRandom') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'AddSortedNaturalAscending') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'AddSortedNaturalDescending') as ICollectable);
            if not AlwaysNaturalItems then
            begin
                FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'InsertNonNaturalRandom') as ICollectable);
                FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'InsertNonNaturalBegin') as ICollectable);
                FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'InsertNonNaturalEnd') as ICollectable);
            end;
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'InsertNaturalRandom') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'InsertNaturalBegin') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'InsertNaturalEnd') as ICollectable);
        end;
        if (CollectionType = ctMap) then
        begin
            if not AlwaysNaturalKeys then
            begin
                FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'PutNonNaturalRandom') as ICollectable);
                FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'PutNonNaturalAscending') as ICollectable);
                FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'PutNonNaturalDescending') as ICollectable);
            end;
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'PutNaturalRandom') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'PutNaturalAscending') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'PutNaturalDescending') as ICollectable);
        end;
        if (CollectionType = ctIntegerMap) then
        begin
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'PutNaturalRandomInt') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'PutNaturalAscendingInt') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'PutNaturalDescendingInt') as ICollectable);
        end;
        if (CollectionType = ctStringMap) then
        begin
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'PutNaturalRandomStr') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'PutNaturalAscendingStr') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'PutNaturalDescendingStr') as ICollectable);
        end;
        if not AlwaysNaturalItems then
        begin
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveNonNaturalRandom') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveNonNaturalAscending') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveNonNaturalDescending') as ICollectable);
        end;
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveNaturalRandom') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveNaturalAscending') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveNaturalDescending') as ICollectable);
        if CollectionType = ctList then
        begin
            if not AlwaysNaturalItems then
            begin
                FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveSortedNonNaturalRandom') as ICollectable);
                FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveSortedNonNaturalAscending') as ICollectable);
                FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveSortedNonNaturalDescending') as ICollectable);
            end;
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveSortedNaturalRandom') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveSortedNaturalAscending') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveSortedNaturalDescending') as ICollectable);
        end;
        if (CollectionType = ctMap) then
        begin
            if not AlwaysNaturalKeys then
            begin
                FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveKeyNonNaturalRandom') as ICollectable);
                FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveKeyNonNaturalAscending') as ICollectable);
                FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveKeyNonNaturalDescending') as ICollectable);
            end;
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveKeyNaturalRandom') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveKeyNaturalAscending') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveKeyNaturalDescending') as ICollectable);
        end;
        if (CollectionType = ctIntegerMap) then
        begin
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveKeyNaturalRandomInt') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveKeyNaturalAscendingInt') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveKeyNaturalDescendingInt') as ICollectable);
        end;
        if (CollectionType = ctStringMap) then
        begin
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveKeyNaturalRandomStr') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveKeyNaturalAscendingStr') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'RemoveKeyNaturalDescendingStr') as ICollectable);
        end;
    end;
    if not AlwaysNaturalItems then
    begin
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsNonNaturalRandom') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsNonNaturalFirst') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsNonNaturalLast') as ICollectable);
    end;
    FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsNaturalRandom') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsNaturalFirst') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsNaturalLast') as ICollectable);
    if CollectionType = ctList then
    begin
        if not AlwaysNaturalItems then
        begin
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsSortedNonNaturalRandom') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsSortedNonNaturalFirst') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsSortedNonNaturalLast') as ICollectable);
        end;
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsSortedNaturalRandom') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsSortedNaturalFirst') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsSortedNaturalLast') as ICollectable);
    end;
    if (CollectionType = ctMap) then
    begin
        if not AlwaysNaturalKeys then
        begin
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsKeyNonNaturalRandom') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsKeyNonNaturalFirst') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsKeyNonNaturalLast') as ICollectable);
        end;
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsKeyNaturalRandom') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsKeyNaturalFirst') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsKeyNaturalLast') as ICollectable);
    end;
    if (CollectionType = ctIntegerMap) then
    begin
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsKeyNaturalRandomInt') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsKeyNaturalFirstInt') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsKeyNaturalLastInt') as ICollectable);
    end;
    if (CollectionType = ctStringMap) then
    begin
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsKeyNaturalRandomStr') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsKeyNaturalFirstStr') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'ContainsKeyNaturalLastStr') as ICollectable);
    end;
    if CollectionType = ctList then
    begin
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'GetItemRandom') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'GetItemFirst') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'GetItemLast') as ICollectable);
    end;
    if (CollectionType = ctMap) then
    begin
        if not AlwaysNaturalKeys then
        begin
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'GetNonNaturalRandom') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'GetNonNaturalFirst') as ICollectable);
            FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'GetNonNaturalLast') as ICollectable);
        end;
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'GetNaturalRandom') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'GetNaturalFirst') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'GetNaturalLast') as ICollectable);
    end;
    if (CollectionType = ctIntegerMap) then
    begin
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'GetNaturalRandomInt') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'GetNaturalFirstInt') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'GetNaturalLastInt') as ICollectable);
    end;
    if (CollectionType = ctStringMap) then
    begin
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'GetNaturalRandomStr') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'GetNaturalFirstStr') as ICollectable);
        FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'GetNaturalLastStr') as ICollectable);
    end;
    FResultTable.Add(TPerformanceTestResult.Create(CollectionClass, 'Iterator') as ICollectable);
end;

procedure TPerformanceTest.AddArrayTestList;
begin
    FResultTable.Add(TPerformanceTestResult.Create('(Array)', 'CreateNonNatural') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(Array)', 'CreateNatural') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(Array)', 'ContainsNonNaturalRandom') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(Array)', 'ContainsNonNaturalFirst') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(Array)', 'ContainsNonNaturalLast') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(Array)', 'ContainsNaturalRandom') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(Array)', 'ContainsNaturalFirst') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(Array)', 'ContainsNaturalLast') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(Array)', 'GetItemRandom') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(Array)', 'GetItemFirst') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(Array)', 'GetItemLast') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(Array)', 'Iterator') as ICollectable);
end;

procedure TPerformanceTest.AddTListTestList;
begin
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'CreateNonNatural') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'CreateNatural') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'AddNonNaturalRandom') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'AddNonNaturalAscending') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'AddNonNaturalDescending') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'AddNaturalRandom') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'AddNaturalAscending') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'AddNaturalDescending') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'AddSortedNonNaturalRandom') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'AddSortedNonNaturalAscending') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'AddSortedNonNaturalDescending') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'AddSortedNaturalRandom') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'AddSortedNaturalAscending') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'AddSortedNaturalDescending') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'InsertNonNaturalRandom') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'InsertNonNaturalBegin') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'InsertNonNaturalEnd') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'InsertNaturalRandom') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'InsertNaturalBegin') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'InsertNaturalEnd') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'RemoveNonNaturalRandom') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'RemoveNonNaturalAscending') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'RemoveNonNaturalDescending') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'RemoveNaturalRandom') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'RemoveNaturalAscending') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'RemoveNaturalDescending') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'RemoveSortedNonNaturalRandom') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'RemoveSortedNonNaturalAscending') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'RemoveSortedNonNaturalDescending') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'RemoveSortedNaturalRandom') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'RemoveSortedNaturalAscending') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'RemoveSortedNaturalDescending') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'ContainsNonNaturalRandom') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'ContainsNonNaturalFirst') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'ContainsNonNaturalLast') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'ContainsNaturalRandom') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'ContainsNaturalFirst') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'ContainsNaturalLast') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'GetItemRandom') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'GetItemFirst') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'GetItemLast') as ICollectable);
    FResultTable.Add(TPerformanceTestResult.Create('(TList)', 'Iterator') as ICollectable);
end;

function TPerformanceTest.CreateNonNatural: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    StartTime, EndTime, DiffTime, Count: Integer;
begin
    TestCollection := CollectionClass.Create;
    SourceArray := FNonNaturalArray;
    TestCollection := nil;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        TestCollection := CollectionClass.Create(SourceArray);
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.CreateNatural: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartTime, EndTime, DiffTime, Count: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    if CollectionType = ctMap then
        SourceArray := FMappableItemArray
    else if CollectionType = ctIntegerMap then
        SourceArray := FIntegerMappableItemArray
    else if CollectionType = ctStringMap then
        SourceArray := FStringMappableItemArray
    else
        SourceArray := FNaturalArray;
    TestCollection := nil;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        TestCollection := CollectionClass.Create(SourceArray);
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.AddNonNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    SourceArray := FRandomNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestCollection.Add(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.AddNonNaturalAscending: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestCollection.Add(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.AddNonNaturalDescending: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := CollectionSize - 1 downto 0 do
    begin
        TestCollection.Add(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.AddNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    if CollectionType = ctMap then
        SourceArray := FRandomMappableItemArray
    else if CollectionType = ctIntegerMap then
        SourceArray := FRandomIntegerMappableItemArray
    else if CollectionType = ctStringMap then
        SourceArray := FRandomStringMappableItemArray
    else
        SourceArray := FRandomNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestCollection.Add(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.AddNaturalAscending: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    if CollectionType = ctMap then
        SourceArray := FMappableItemArray
    else if CollectionType = ctIntegerMap then
        SourceArray := FIntegerMappableItemArray
    else if CollectionType = ctStringMap then
        SourceArray := FStringMappableItemArray
    else
        SourceArray := FNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestCollection.Add(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.AddNaturalDescending: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    if CollectionType = ctMap then
        SourceArray := FMappableItemArray
    else if CollectionType = ctIntegerMap then
        SourceArray := FIntegerMappableItemArray
    else if CollectionType = ctStringMap then
        SourceArray := FStringMappableItemArray
    else
        SourceArray := FNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := CollectionSize - 1 downto 0 do
    begin
        TestCollection.Add(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.AddSortedNonNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create;
    TestList.SetComparator(TTestComparator_NonNat.Create as IComparator);
    TestList.SetSorted(true);
    SourceArray := FRandomNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestList.Add(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.AddSortedNonNaturalAscending: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create;
    TestList.SetComparator(TTestComparator_NonNat.Create as IComparator);
    TestList.SetSorted(true);
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestList.Add(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.AddSortedNonNaturalDescending: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create;
    TestList.SetComparator(TTestComparator_NonNat.Create as IComparator);
    TestList.SetSorted(true);
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := CollectionSize - 1 downto 0 do
    begin
        TestList.Add(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.AddSortedNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create;
    TestList.SetSorted(true);
    SourceArray := FRandomNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestList.Add(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.AddSortedNaturalAscending: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create;
    TestList.SetSorted(true);
    SourceArray := FNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestList.Add(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.AddSortedNaturalDescending: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create;
    TestList.SetSorted(true);
    SourceArray := FNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := CollectionSize - 1 downto 0 do
    begin
        TestList.Add(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.InsertNonNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create;
    SourceArray := FNonNaturalArray;
    Randomize;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestList.Insert(Random(TestList.GetSize), SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.InsertNonNaturalBegin: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create;
    SourceArray := FNonNaturalArray;
    Randomize;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        TestList.Clear;
        for I := 0 to CollectionSize - 1 do
        begin
            TestList.Insert(0, SourceArray[I]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.InsertNonNaturalEnd: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create;
    SourceArray := FNonNaturalArray;
    Randomize;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        TestList.Clear;
        for I := 0 to CollectionSize - 1 do
        begin
            TestList.Insert(TestList.GetSize, SourceArray[I]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.InsertNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create;
    SourceArray := FNaturalArray;
    Randomize;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        TestList.Clear;
        for I := 0 to CollectionSize - 1 do
        begin
            TestList.Insert(Random(TestList.GetSize), SourceArray[I]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.InsertNaturalBegin: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create;
    SourceArray := FNaturalArray;
    Randomize;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        TestList.Clear;
        for I := 0 to CollectionSize - 1 do
        begin
            TestList.Insert(0, SourceArray[I]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.InsertNaturalEnd: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create;
    SourceArray := FNaturalArray;
    Randomize;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        TestList.Clear;
        for I := 0 to CollectionSize - 1 do
        begin
            TestList.Insert(TestList.GetSize, SourceArray[I]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.PutNonNaturalRandom: Integer;
var
    KeyArray, ItemArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(false, false);
    KeyArray := FRandomNonNaturalArray;
    ItemArray := FRandomNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        Testmap.Put(KeyArray[I], ItemArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.PutNonNaturalAscending: Integer;
var
    KeyArray, ItemArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(false, false);
    KeyArray := FNonNaturalArray;
    ItemArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        Testmap.Put(KeyArray[I], ItemArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.PutNonNaturalDescending: Integer;
var
    KeyArray, ItemArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(false, false);
    KeyArray := FRandomNonNaturalArray;
    ItemArray := FRandomNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := CollectionSize - 1 downto 0 do
    begin
        Testmap.Put(KeyArray[I], ItemArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.PutNaturalRandom: Integer;
var
    KeyArray, ItemArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create;
    KeyArray := FRandomNaturalArray;
    ItemArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        Testmap.Put(KeyArray[I], ItemArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.PutNaturalAscending: Integer;
var
    KeyArray, ItemArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create;
    KeyArray := FNaturalArray;
    ItemArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        Testmap.Put(KeyArray[I], ItemArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.PutNaturalDescending: Integer;
var
    KeyArray, ItemArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create;
    KeyArray := FNaturalArray;
    ItemArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := CollectionSize - 1 downto 0 do
    begin
        Testmap.Put(KeyArray[I], ItemArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.PutNaturalRandomInt: Integer;
var
    KeyArray: TIntegerArray;
    ItemArray: TCollectableArray;
    TestMap: IIntegerMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create;
    KeyArray := FRandomIntegerArray;
    ItemArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestMap.Put(KeyArray[I], ItemArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.PutNaturalAscendingInt: Integer;
var
    KeyArray: TIntegerArray;
    ItemArray: TCollectableArray;
    TestMap: IIntegerMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create;
    KeyArray := FIntegerArray;
    ItemArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        Testmap.Put(KeyArray[I], ItemArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.PutNaturalDescendingInt: Integer;
var
    KeyArray: TIntegerArray;
    ItemArray: TCollectableArray;
    TestMap: IIntegerMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create;
    KeyArray := FIntegerArray;
    ItemArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := CollectionSize - 1 downto 0 do
    begin
        TestMap.Put(KeyArray[I], ItemArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.PutNaturalRandomStr: Integer;
var
    KeyArray: TStringArray;
    ItemArray: TCollectableArray;
    TestMap: IStringMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create;
    KeyArray := FRandomStringArray;
    ItemArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestMap.Put(KeyArray[I], ItemArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.PutNaturalAscendingStr: Integer;
var
    KeyArray: TStringArray;
    ItemArray: TCollectableArray;
    TestMap: IStringMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create;
    KeyArray := FStringArray;
    ItemArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        Testmap.Put(KeyArray[I], ItemArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.PutNaturalDescendingStr: Integer;
var
    KeyArray: TStringArray;
    ItemArray: TCollectableArray;
    TestMap: IStringMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create;
    KeyArray := FStringArray;
    ItemArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := CollectionSize - 1 downto 0 do
    begin
        TestMap.Put(KeyArray[I], ItemArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveNonNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    if CollectionType = ctMap then
        TestCollection := TAbstractMapClass(CollectionClass).Create(FNaturalArray, FNonNaturalArray)
    else if CollectionType = ctIntegerMap then
        TestCollection := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray, FNonNaturalArray)
    else if CollectionType = ctStringMap then
        TestCollection := TAbstractStringMapClass(CollectionClass).Create(FStringArray, FNonNaturalArray)
    else
        TestCollection := CollectionClass.Create(FNonNaturalArray);
    SourceArray := FRandomNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestCollection.Remove(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveNonNaturalAscending: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    if CollectionType = ctMap then
        TestCollection := TAbstractMapClass(CollectionClass).Create(FNaturalArray, FNonNaturalArray)
    else if CollectionType = ctIntegerMap then
        TestCollection := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray, FNonNaturalArray)
    else if CollectionType = ctStringMap then
        TestCollection := TAbstractStringMapClass(CollectionClass).Create(FStringArray, FNonNaturalArray)
    else
        TestCollection := CollectionClass.Create(FNonNaturalArray);
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestCollection.Remove(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveNonNaturalDescending: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    if CollectionType = ctMap then
        TestCollection := TAbstractMapClass(CollectionClass).Create(FNaturalArray, FNonNaturalArray)
    else if CollectionType = ctIntegerMap then
        TestCollection := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray, FNonNaturalArray)
    else if CollectionType = ctStringMap then
        TestCollection := TAbstractStringMapClass(CollectionClass).Create(FStringArray, FNonNaturalArray)
    else
        TestCollection := CollectionClass.Create(FNonNaturalArray);
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := CollectionSize - 1 downto 0 do
    begin
        TestCollection.Remove(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    if CollectionType = ctMap then
        TestCollection := TAbstractMapClass(CollectionClass).Create(FNaturalArray, FNaturalArray)
    else if CollectionType = ctIntegerMap then
        TestCollection := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray, FNaturalArray)
    else if CollectionType = ctStringMap then
        TestCollection := TAbstractStringMapClass(CollectionClass).Create(FStringArray, FNaturalArray)
    else
        TestCollection := CollectionClass.Create(FNaturalArray);
    SourceArray := FRandomNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestCollection.Remove(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveNaturalAscending: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    if CollectionType = ctMap then
        TestCollection := TAbstractMapClass(CollectionClass).Create(FNaturalArray, FNaturalArray)
    else if CollectionType = ctIntegerMap then
        TestCollection := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray, FNaturalArray)
    else if CollectionType = ctStringMap then
        TestCollection := TAbstractStringMapClass(CollectionClass).Create(FStringArray, FNaturalArray)
    else
        TestCollection := CollectionClass.Create(FNaturalArray);
    SourceArray := FNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestCollection.Remove(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveNaturalDescending: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    if CollectionType = ctMap then
        TestCollection := TAbstractMapClass(CollectionClass).Create(FNaturalArray, FNaturalArray)
    else if CollectionType = ctIntegerMap then
        TestCollection := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray, FNaturalArray)
    else if CollectionType = ctStringMap then
        TestCollection := TAbstractStringMapClass(CollectionClass).Create(FStringArray, FNaturalArray)
    else
        TestCollection := CollectionClass.Create(FNaturalArray);
    SourceArray := FRandomNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := CollectionSize - 1 downto 0 do
    begin
        TestCollection.Remove(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveSortedNonNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray);
    TestList.SetComparator(TTestComparator_NonNat.Create as IComparator);
    TestList.SetSorted(true);
    SourceArray := FRandomNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestList.Remove(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveSortedNonNaturalAscending: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray);
    TestList.SetComparator(TTestComparator_NonNat.Create as IComparator);
    TestList.SetSorted(true);
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestList.Remove(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveSortedNonNaturalDescending: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray);
    TestList.SetComparator(TTestComparator_NonNat.Create as IComparator);
    TestList.SetSorted(true);
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := CollectionSize - 1 downto 0 do
    begin
        TestList.Remove(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveSortedNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FNaturalArray);
    TestList.SetSorted(true);
    SourceArray := FRandomNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestList.Remove(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveSortedNaturalAscending: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FNaturalArray);
    TestList.SetSorted(true);
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestList.Remove(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveSortedNaturalDescending: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FNaturalArray);
    TestList.SetSorted(true);
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := CollectionSize - 1 downto 0 do
    begin
        TestList.Remove(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveKeyNonNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNonNaturalArray, FNonNaturalArray, false, false);
    SourceArray := FRandomNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestMap.RemoveKey(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveKeyNonNaturalAscending: Integer;
var
    SourceArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNonNaturalArray, FNonNaturalArray, false, false);
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestMap.RemoveKey(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveKeyNonNaturalDescending: Integer;
var
    SourceArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNonNaturalArray, FNonNaturalArray, false, false);
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := CollectionSize - 1 downto 0 do
    begin
        TestMap.RemoveKey(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveKeyNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNaturalArray, FNonNaturalArray, false, true);
    SourceArray := FRandomNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestMap.RemoveKey(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveKeyNaturalAscending: Integer;
var
    SourceArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNaturalArray, FNonNaturalArray, false, true);
    SourceArray := FNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestMap.RemoveKey(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveKeyNaturalDescending: Integer;
var
    SourceArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNaturalArray, FNonNaturalArray, false, true);
    SourceArray := FNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := CollectionSize - 1 downto 0 do
    begin
        TestMap.RemoveKey(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveKeyNaturalRandomInt: Integer;
var
    SourceArray: TIntegerArray;
    TestMap: IIntegerMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray, FNonNaturalArray, false);
    SourceArray := FRandomIntegerArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestMap.RemoveKey(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveKeyNaturalAscendingInt: Integer;
var
    SourceArray: TIntegerArray;
    TestMap: IIntegerMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray, FNonNaturalArray, false);
    SourceArray := FIntegerArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestMap.RemoveKey(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveKeyNaturalDescendingInt: Integer;
var
    SourceArray: TIntegerArray;
    TestMap: IIntegerMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray, FNonNaturalArray, false);
    SourceArray := FIntegerArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := CollectionSize - 1 downto 0 do
    begin
        TestMap.RemoveKey(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveKeyNaturalRandomStr: Integer;
var
    SourceArray: TStringArray;
    TestMap: IStringMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray, FNonNaturalArray, false);
    SourceArray := FRandomStringArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestMap.RemoveKey(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveKeyNaturalAscendingStr: Integer;
var
    SourceArray: TStringArray;
    TestMap: IStringMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray, FNonNaturalArray, false);
    SourceArray := FStringArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := 0 to CollectionSize - 1 do
    begin
        TestMap.RemoveKey(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.RemoveKeyNaturalDescendingStr: Integer;
var
    SourceArray: TStringArray;
    TestMap: IStringMap;
    StartTime, EndTime, DiffTime: Integer;
    I: Integer;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray, FNonNaturalArray, false);
    SourceArray := FStringArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    for I := CollectionSize - 1 downto 0 do
    begin
        TestMap.RemoveKey(SourceArray[I]);
    end;
    EndTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := EndTime - StartTime;
    Result := DiffTime;
end;

function TPerformanceTest.ContainsNonNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    if CollectionType = ctMap then
        TestCollection := TAbstractMapClass(CollectionClass).Create(FNaturalArray, FNonNaturalArray)
    else if CollectionType = ctIntegerMap then
        TestCollection := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray, FNonNaturalArray)
    else if CollectionType = ctStringMap then
        TestCollection := TAbstractStringMapClass(CollectionClass).Create(FStringArray, FNonNaturalArray)
    else
        TestCollection := CollectionClass.Create(FNonNaturalArray);
    SourceArray := FRandomNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestCollection.Contains(SourceArray[I]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsNonNaturalFirst: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    if CollectionType = ctMap then
        TestCollection := TAbstractMapClass(CollectionClass).Create(FNaturalArray, FNonNaturalArray)
    else if CollectionType = ctIntegerMap then
        TestCollection := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray, FNonNaturalArray)
    else if CollectionType = ctStringMap then
        TestCollection := TAbstractStringMapClass(CollectionClass).Create(FStringArray, FNonNaturalArray)
    else
        TestCollection := CollectionClass.Create(FNonNaturalArray);
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestCollection.Contains(SourceArray[0]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsNonNaturalLast: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    if CollectionType = ctMap then
        TestCollection := TAbstractMapClass(CollectionClass).Create(FNaturalArray, FNonNaturalArray)
    else if CollectionType = ctIntegerMap then
        TestCollection := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray, FNonNaturalArray)
    else if CollectionType = ctStringMap then
        TestCollection := TAbstractStringMapClass(CollectionClass).Create(FStringArray, FNonNaturalArray)
    else
        TestCollection := CollectionClass.Create(FNonNaturalArray);
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestCollection.Contains(SourceArray[CollectionSize - 1]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    if CollectionType = ctMap then
    begin
        SourceArray := FRandomMappableItemArray;
        TestCollection := CollectionClass.Create(FMappableItemArray, true);
    end
    else if CollectionType = ctIntegerMap then
    begin
        SourceArray := FRandomIntegerMappableItemArray;
        TestCollection := CollectionClass.Create(FIntegerMappableItemArray, true);
    end
    else if CollectionType = ctStringMap then
    begin
        SourceArray := FRandomStringMappableItemArray;
        TestCollection := CollectionClass.Create(FStringMappableItemArray, true);
    end
    else
    begin
        SourceArray := FNaturalArray;
        TestCollection := CollectionClass.Create(FNaturalArray, true);
    end;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestCollection.Contains(SourceArray[I]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsNaturalFirst: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    if CollectionType = ctMap then
        SourceArray := FMappableItemArray
    else if CollectionType = ctIntegerMap then
        SourceArray := FIntegerMappableItemArray
    else if CollectionType = ctStringMap then
        SourceArray := FStringMappableItemArray
    else
        SourceArray := FNaturalArray;
    TestCollection := CollectionClass.Create(SourceArray, true);
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestCollection.Contains(SourceArray[0]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsNaturalLast: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    if CollectionType = ctMap then
        SourceArray := FMappableItemArray
    else if CollectionType = ctIntegerMap then
        SourceArray := FIntegerMappableItemArray
    else if CollectionType = ctStringMap then
        SourceArray := FStringMappableItemArray
    else
        SourceArray := FNaturalArray;
    TestCollection := CollectionClass.Create(SourceArray, true);
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestCollection.Contains(SourceArray[CollectionSize - 1]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsSortedNonNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray);
    TestList.SetSorted(true);
    SourceArray := FRandomNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestList.Contains(SourceArray[I]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsSortedNonNaturalFirst: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray);
    TestList.SetSorted(true);
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestList.Contains(SourceArray[0]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsSortedNonNaturalLast: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray);
    TestList.SetSorted(true);
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestList.Contains(SourceArray[CollectionSize - 1]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsSortedNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    SourceArray := FNaturalArray;
    TestList := TAbstractListClass(CollectionClass).Create(SourceArray, true);
    TestList.SetSorted(true);
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestList.Contains(SourceArray[I]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsSortedNaturalFirst: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    SourceArray := FNaturalArray;
    TestList := TAbstractListClass(CollectionClass).Create(SourceArray, true);
    TestList.SetSorted(true);
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestList.Contains(SourceArray[0]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsSortedNaturalLast: Integer;
var
    SourceArray: TCollectableArray;
    TestList: IList;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    SourceArray := FNaturalArray;
    TestList := TAbstractListClass(CollectionClass).Create(SourceArray, true);
    TestList.SetSorted(true);
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestList.Contains(SourceArray[CollectionSize - 1]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsKeyNonNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNonNaturalArray, FNonNaturalArray, false, false);
    SourceArray := FRandomNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.ContainsKey(SourceArray[I]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsKeyNonNaturalFirst: Integer;
var
    SourceArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNonNaturalArray, FNonNaturalArray, false, false);
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.ContainsKey(SourceArray[0]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsKeyNonNaturalLast: Integer;
var
    SourceArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNonNaturalArray, FNonNaturalArray, false, false);
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.ContainsKey(SourceArray[CollectionSize - 1]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsKeyNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNaturalArray, FNonNaturalArray, false, true);
    SourceArray := FRandomNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.ContainsKey(SourceArray[I]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsKeyNaturalFirst: Integer;
var
    SourceArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNaturalArray, FNonNaturalArray, false, true);
    SourceArray := FNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.ContainsKey(SourceArray[0]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsKeyNaturalLast: Integer;
var
    SourceArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNaturalArray, FNonNaturalArray, false, true);
    SourceArray := FNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.ContainsKey(SourceArray[CollectionSize - 1]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsKeyNaturalRandomInt: Integer;
var
    SourceArray: TIntegerArray;
    TestMap: IIntegerMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray, FNonNaturalArray, false);
    SourceArray := FRandomIntegerArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.ContainsKey(SourceArray[I]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsKeyNaturalFirstInt: Integer;
var
    SourceArray: TIntegerArray;
    TestMap: IIntegerMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray, FNonNaturalArray, false);
    SourceArray := FIntegerArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.ContainsKey(SourceArray[0]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsKeyNaturalLastInt: Integer;
var
    SourceArray: TIntegerArray;
    TestMap: IIntegerMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray, FNonNaturalArray, false);
    SourceArray := FIntegerArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.ContainsKey(SourceArray[CollectionSize - 1]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsKeyNaturalRandomStr: Integer;
var
    SourceArray: TStringArray;
    TestMap: IStringMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray, FNonNaturalArray, false);
    SourceArray := FRandomStringArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.ContainsKey(SourceArray[I]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsKeyNaturalFirstStr: Integer;
var
    SourceArray: TStringArray;
    TestMap: IStringMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray, FNonNaturalArray, false);
    SourceArray := FStringArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.ContainsKey(SourceArray[0]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.ContainsKeyNaturalLastStr: Integer;
var
    SourceArray: TStringArray;
    TestMap: IStringMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray, FNonNaturalArray, false);
    SourceArray := FStringArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.ContainsKey(SourceArray[CollectionSize - 1]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.GetItemRandom: Integer;
var
    TestList: IList;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray);
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestList.GetItem(FRandomIndexArray[I]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.GetItemFirst: Integer;
var
    TestList: IList;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray);
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestList.GetItem(0);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.GetItemLast: Integer;
var
    TestList: IList;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestList := TAbstractListClass(CollectionClass).Create(FNonNaturalArray);
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestList.GetItem(CollectionSize - 1);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.GetNonNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNonNaturalArray, FNonNaturalArray, false, false);
    SourceArray := FRandomNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.Get(SourceArray[I]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.GetNonNaturalFirst: Integer;
var
    SourceArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNonNaturalArray, FNonNaturalArray, false, false);
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.Get(SourceArray[0]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.GetNonNaturalLast: Integer;
var
    SourceArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNonNaturalArray, FNonNaturalArray, false, false);
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.Get(SourceArray[CollectionSize - 1]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.GetNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNaturalArray, FNonNaturalArray, false, true);
    SourceArray := FRandomNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.Get(SourceArray[I]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.GetNaturalFirst: Integer;
var
    SourceArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNaturalArray, FNonNaturalArray, false, true);
    SourceArray := FNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.Get(SourceArray[0]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.GetNaturalLast: Integer;
var
    SourceArray: TCollectableArray;
    TestMap: IMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractMapClass(CollectionClass).Create(FNaturalArray, FNonNaturalArray, false, true);
    SourceArray := FNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.Get(SourceArray[CollectionSize - 1]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.GetNaturalRandomInt: Integer;
var
    SourceArray: TIntegerArray;
    TestMap: IIntegerMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray, FNonNaturalArray, false);
    SourceArray := FRandomIntegerArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.Get(SourceArray[I]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.GetNaturalFirstInt: Integer;
var
    SourceArray: TIntegerArray;
    TestMap: IIntegerMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray, FNonNaturalArray, false);
    SourceArray := FIntegerArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.Get(SourceArray[0]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.GetNaturalLastInt: Integer;
var
    SourceArray: TIntegerArray;
    TestMap: IIntegerMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerArray, FNonNaturalArray, false);
    SourceArray := FIntegerArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.Get(SourceArray[CollectionSize - 1]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.GetNaturalRandomStr: Integer;
var
    SourceArray: TStringArray;
    TestMap: IStringMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray, FNonNaturalArray, false);
    SourceArray := FRandomStringArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.Get(SourceArray[I]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.GetNaturalFirstStr: Integer;
var
    SourceArray: TStringArray;
    TestMap: IStringMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray, FNonNaturalArray, false);
    SourceArray := FStringArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.Get(SourceArray[0]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.GetNaturalLastStr: Integer;
var
    SourceArray: TStringArray;
    TestMap: IStringMap;
    StartTime, EndTime, DiffTime, Count: Integer;
    I: Integer;
begin
    TestMap := TAbstractStringMapClass(CollectionClass).Create(FStringArray, FNonNaturalArray, false);
    SourceArray := FStringArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to CollectionSize - 1 do
        begin
            TestMap.Get(SourceArray[CollectionSize - 1]);
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.Iterator: Integer;
var
    TestCollection: ICollection;
    TestIterator: IIterator;
    Item: ICollectable;
    CollectionType: TCollectionType;
    StartTime, EndTime, DiffTime, Count: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    if CollectionType = ctMap then
        TestCollection := CollectionClass.Create(FMappableItemArray, true)
    else if CollectionType = ctIntegerMap then
        TestCollection := CollectionClass.Create(FIntegerMappableItemArray, true)
    else if CollectionType = ctStringMap then
        TestCollection := CollectionClass.Create(FStringMappableItemArray, true)
    else
        TestCollection := CollectionClass.Create(FNaturalArray, true);
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        TestIterator := TestCollection.GetIterator;
        while not TestIterator.EOF do
        begin
            Item := TestIterator.CurrentItem;
            TestIterator.Next;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.Array_CreateNonNatural: Integer;
var
    SourceArray: TCollectableArray;
    TestArray: TCollectableArray;
    StartTime, EndTime, DiffTime, Count, I: Integer;
begin
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        SetLength(TestArray, Length(SourceArray));
        for I := 0 to Length(SourceArray) - 1 do
        begin
            TestArray[I] := SourceArray[I];
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.Array_CreateNatural: Integer;
var
    SourceArray: TCollectableArray;
    TestArray: TCollectableArray;
    StartTime, EndTime, DiffTime, Count, I: Integer;
begin
    SourceArray := FNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        SetLength(TestArray, Length(SourceArray));
        for I := 0 to Length(SourceArray) - 1 do
        begin
            TestArray[I] := SourceArray[I];
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.Array_ContainsNonNaturalRandom: Integer;
var
    ConsArray, SourceArray: TCollectableArray;
    TestArray: TCollectableArray;
    StartTime, EndTime, DiffTime, Count, I, J: Integer;
    Success: Boolean;
begin
    ConsArray := FNonNaturalArray;
    SourceArray := FRandomNonNaturalArray;
    SetLength(TestArray, Length(ConsArray));
    for I := 0 to Length(ConsArray) - 1 do
    begin
        TestArray[I] := ConsArray[I];
    end;

    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Success := false;
            for J := 0 to Length(TestArray) - 1 do
            begin
                if TestArray[J] = SourceArray[I] then
                begin
                    Success := true;
                    break;
                end;
            end;
            if Success then // Suppreses hints about Success not being used
            begin
            end;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.Array_ContainsNonNaturalFirst: Integer;
var
    ConsArray, SourceArray: TCollectableArray;
    TestArray: TCollectableArray;
    StartTime, EndTime, DiffTime, Count, I, J: Integer;
    Success: Boolean;
begin
    ConsArray := FNonNaturalArray;
    SourceArray := FNonNaturalArray;
    SetLength(TestArray, Length(ConsArray));
    for I := 0 to Length(ConsArray) - 1 do
    begin
        TestArray[I] := ConsArray[I];
    end;

    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Success := false;
            for J := 0 to Length(TestArray) - 1 do
            begin
                if TestArray[I] = SourceArray[0] then
                begin
                    Success := true;
                    break;
                end;
            end;
            if Success then // Suppreses hints about Success not being used
            begin
            end;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.Array_ContainsNonNaturalLast: Integer;
var
    ConsArray, SourceArray: TCollectableArray;
    TestArray: TCollectableArray;
    StartTime, EndTime, DiffTime, Count, I, J: Integer;
    Success: Boolean;
begin
    ConsArray := FNonNaturalArray;
    SourceArray := FNonNaturalArray;
    SetLength(TestArray, Length(ConsArray));
    for I := 0 to Length(ConsArray) - 1 do
    begin
        TestArray[I] := ConsArray[I];
    end;

    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Success := false;
            for J := 0 to Length(TestArray) - 1 do
            begin
                if TestArray[I] = SourceArray[Length(SourceArray) - 1] then
                begin
                    Success := true;
                    break;
                end;
            end;
            if Success then // Suppreses hints about Success not being used
            begin
            end;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.Array_ContainsNaturalRandom: Integer;
var
    ConsArray, SourceArray: TCollectableArray;
    TestArray: TCollectableArray;
    StartTime, EndTime, DiffTime, Count, I, J: Integer;
    Success: Boolean;
begin
    ConsArray := FNaturalArray;
    SourceArray := FRandomNaturalArray;
    SetLength(TestArray, Length(ConsArray));
    for I := 0 to Length(ConsArray) - 1 do
    begin
        TestArray[I] := ConsArray[I];
    end;

    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Success := false;
            for J := 0 to Length(TestArray) - 1 do
            begin
                if (TestArray[J] as IString).Value = (SourceArray[I] as IString).Value then
                begin
                    Success := true;
                    break;
                end;
            end;
            if Success then // Suppreses hints about Success not being used
            begin
            end;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.Array_ContainsNaturalFirst: Integer;
var
    ConsArray, SourceArray: TCollectableArray;
    TestArray: TCollectableArray;
    StartTime, EndTime, DiffTime, Count, I, J: Integer;
    Success: Boolean;
begin
    ConsArray := FNaturalArray;
    SourceArray := FNaturalArray;
    SetLength(TestArray, Length(ConsArray));
    for I := 0 to Length(ConsArray) - 1 do
    begin
        TestArray[I] := ConsArray[I];
    end;

    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Success := false;
            for J := 0 to Length(TestArray) - 1 do
            begin
                if (TestArray[J] as IString).Value = (SourceArray[0] as IString).Value then
                begin
                    Success := true;
                    break;
                end;
            end;
            if Success then // Suppreses hints about Success not being used
            begin
            end;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.Array_ContainsNaturalLast: Integer;
var
    ConsArray, SourceArray: TCollectableArray;
    TestArray: TCollectableArray;
    StartTime, EndTime, DiffTime, Count, I, J: Integer;
    Success: Boolean;
begin
    ConsArray := FNaturalArray;
    SourceArray := FRandomNaturalArray;
    SetLength(TestArray, Length(ConsArray));
    for I := 0 to Length(ConsArray) - 1 do
    begin
        TestArray[I] := ConsArray[I];
    end;

    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Success := false;
            for J := 0 to Length(TestArray) - 1 do
            begin
                if (TestArray[J] as IString).Value = (SourceArray[Length(SourceArray) - 1] as IString).Value then
                begin
                    Success := true;
                    break;
                end;
            end;
            if Success then // Suppreses hints about Success not being used
            begin
            end;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.Array_GetItemRandom: Integer;
var
    ConsArray: TCollectableArray;
    TestArray: TCollectableArray;
    StartTime, EndTime, DiffTime, Count, I: Integer;
    Item: ICollectable;
begin
    ConsArray := FNonNaturalArray;
    SetLength(TestArray, Length(ConsArray));
    for I := 0 to Length(ConsArray) - 1 do
    begin
        TestArray[I] := ConsArray[I];
    end;

    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to Length(FRandomIndexArray) - 1 do
        begin
            Item := TestArray[FRandomIndexArray[I]];
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.Array_GetItemFirst: Integer;
var
    ConsArray: TCollectableArray;
    TestArray: TCollectableArray;
    StartTime, EndTime, DiffTime, Count, I: Integer;
    Item: ICollectable;
begin
    ConsArray := FNonNaturalArray;
    SetLength(TestArray, Length(ConsArray));
    for I := 0 to Length(ConsArray) - 1 do
    begin
        TestArray[I] := ConsArray[I];
    end;

    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to Length(TestArray) - 1 do
        begin
            Item := TestArray[0];
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.Array_GetItemLast: Integer;
var
    ConsArray: TCollectableArray;
    TestArray: TCollectableArray;
    StartTime, EndTime, DiffTime, Count, I: Integer;
    Item: ICollectable;
begin
    ConsArray := FNonNaturalArray;
    SetLength(TestArray, Length(ConsArray));
    for I := 0 to Length(ConsArray) - 1 do
    begin
        TestArray[I] := ConsArray[I];
    end;

    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to Length(TestArray) - 1 do
        begin
            Item := TestArray[Length(TestArray) - 1];
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.Array_Iterator: Integer;
var
    ConsArray: TCollectableArray;
    TestArray: TCollectableArray;
    StartTime, EndTime, DiffTime, Count, I: Integer;
    Item: ICollectable;
begin
    ConsArray := FNonNaturalArray;
    SetLength(TestArray, Length(ConsArray));
    for I := 0 to Length(ConsArray) - 1 do
    begin
        TestArray[I] := ConsArray[I];
    end;

    StartTime := DateTimeToTimeStamp(Now).Time;
    DiffTime := 0;
    Count := 0;
    while DiffTime = 0 do
    begin
        for I := 0 to Length(TestArray) - 1 do
        begin
            Item := TestArray[I];
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
        Inc(Count);
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.TList_CreateNonNatural: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    SourceArray := FNonNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;

    TestList := TList.Create;
    try
        TestList.Capacity := Length(SourceArray);
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_CreateNatural: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    SourceArray := FNaturalArray;
    StartTime := DateTimeToTimeStamp(Now).Time;

    TestList := TList.Create;
    try
        TestList.Capacity := Length(SourceArray);
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_AddNonNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    SourceArray := FRandomNonNaturalArray;
    TestList := TList.Create;

    StartTime := DateTimeToTimeStamp(Now).Time;
    try
        TestList.Capacity := Length(SourceArray);
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_AddNonNaturalAscending: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    SourceArray := FNonNaturalArray;
    TestList := TList.Create;

    StartTime := DateTimeToTimeStamp(Now).Time;
    try
        TestList.Capacity := Length(SourceArray);
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_AddNonNaturalDescending: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    SourceArray := FNonNaturalArray;
    TestList := TList.Create;

    StartTime := DateTimeToTimeStamp(Now).Time;
    try
        TestList.Capacity := Length(SourceArray);
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_AddNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    SourceArray := FRandomNaturalArray;
    TestList := TList.Create;

    StartTime := DateTimeToTimeStamp(Now).Time;
    try
        TestList.Capacity := Length(SourceArray);
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_AddNaturalAscending: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    SourceArray := FNaturalArray;
    TestList := TList.Create;

    StartTime := DateTimeToTimeStamp(Now).Time;
    try
        TestList.Capacity := Length(SourceArray);
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_AddNaturalDescending: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    SourceArray := FNaturalArray;
    TestList := TList.Create;

    StartTime := DateTimeToTimeStamp(Now).Time;
    try
        TestList.Capacity := Length(SourceArray);
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_AddSortedNonNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I, J: Integer;
    Found: Boolean;
begin
    SourceArray := FRandomNonNaturalArray;
    TestList := TList.Create;

    StartTime := DateTimeToTimeStamp(Now).Time;
    try
        TestList.Capacity := Length(SourceArray);
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            // TAbstractList uses binary searching for sorted lists, making this
            // algorithm look bad, but few developers would write that much
            // client code to improve performance.
            Found := false;
            for J := 0 to TestList.Count - 1 do
            begin
                if NonNaturalCompare(Pointer(Item), TestList[J]) >= 0 then
                begin
                    Found := true;
                    break;
                end;
            end;

            if Found then
                TestList.Insert(J, Pointer(Item))
            else
                TestList.Add(Pointer(Item));
            Item._AddRef;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_AddSortedNonNaturalAscending: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I, J: Integer;
    Found: Boolean;
begin
    SourceArray := FNonNaturalArray;
    TestList := TList.Create;

    StartTime := DateTimeToTimeStamp(Now).Time;
    try
        TestList.Capacity := Length(SourceArray);
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            // TAbstractList uses binary searching for sorted lists, making this
            // algorithm look bad, but few developers would write that much
            // client code to improve performance.
            Found := false;
            for J := 0 to TestList.Count - 1 do
            begin
                if NonNaturalCompare(Pointer(Item), TestList[J]) >= 0 then
                begin
                    Found := true;
                    break;
                end;
            end;

            if Found then
                TestList.Insert(J, Pointer(Item))
            else
                TestList.Add(Pointer(Item));
            Item._AddRef;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_AddSortedNonNaturalDescending: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I, J: Integer;
    Found: Boolean;
begin
    SourceArray := FNonNaturalArray;
    TestList := TList.Create;

    StartTime := DateTimeToTimeStamp(Now).Time;
    try
        TestList.Capacity := Length(SourceArray);
        for I := Length(SourceArray) - 1 downto 0 do
        begin
            Item := SourceArray[I];
            // TAbstractList uses binary searching for sorted lists, making this
            // algorithm look bad, but few developers would write that much
            // client code to improve performance.
            Found := false;
            for J := 0 to TestList.Count - 1 do
            begin
                if NonNaturalCompare(Pointer(Item), TestList[J]) >= 0 then
                begin
                    Found := true;
                    break;
                end;
            end;

            if Found then
                TestList.Insert(J, Pointer(Item))
            else
                TestList.Add(Pointer(Item));
            Item._AddRef;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_AddSortedNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I, J: Integer;
    Found: Boolean;
begin
    SourceArray := FRandomNaturalArray;
    TestList := TList.Create;

    StartTime := DateTimeToTimeStamp(Now).Time;
    try
        TestList.Capacity := Length(SourceArray);
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            // TAbstractList uses binary searching for sorted lists, making this
            // algorithm look bad, but few developers would write that much
            // client code to improve performance.
            Found := false;
            for J := 0 to TestList.Count - 1 do
            begin
                if (Item as IString).Value >=
                    (ICollectable(TestList[J]) as IString).Value then
                begin
                    Found := true;
                    break;
                end;
            end;

            if Found then
                TestList.Insert(J, Pointer(Item))
            else
                TestList.Add(Pointer(Item));
            Item._AddRef;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_AddSortedNaturalAscending: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I, J: Integer;
    Found: Boolean;
begin
    SourceArray := FNaturalArray;
    TestList := TList.Create;

    StartTime := DateTimeToTimeStamp(Now).Time;
    try
        TestList.Capacity := Length(SourceArray);
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            // TAbstractList uses binary searching for sorted lists, making this
            // algorithm look bad, but few developers would write that much
            // client code to improve performance.
            Found := false;
            for J := 0 to TestList.Count - 1 do
            begin
                if (Item as IString).Value >=
                    (ICollectable(TestList[J]) as IString).Value then
                begin
                    Found := true;
                    break;
                end;
            end;

            if Found then
                TestList.Insert(J, Pointer(Item))
            else
                TestList.Add(Pointer(Item));
            Item._AddRef;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_AddSortedNaturalDescending: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I, J: Integer;
    Found: Boolean;
begin
    SourceArray := FNaturalArray;
    TestList := TList.Create;

    StartTime := DateTimeToTimeStamp(Now).Time;
    try
        TestList.Capacity := Length(SourceArray);
        for I := Length(SourceArray) - 1 downto 0 do
        begin
            Item := SourceArray[I];
            // TAbstractList uses binary searching for sorted lists, making this
            // algorithm look bad, but few developers would write that much
            // client code to improve performance.
            Found := false;
            for J := 0 to TestList.Count - 1 do
            begin
                if (Item as IString).Value >=
                    (ICollectable(TestList[J]) as IString).Value then
                begin
                    Found := true;
                    break;
                end;
            end;

            if Found then
                TestList.Insert(J, Pointer(Item))
            else
                TestList.Add(Pointer(Item));
            Item._AddRef;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_InsertNonNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    SourceArray := FNonNaturalArray;
    TestList := TList.Create;

    StartTime := DateTimeToTimeStamp(Now).Time;
    try
        TestList.Capacity := Length(SourceArray);
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            Item._AddRef;
            TestList.Insert(Random(TestList.Count), Pointer(Item));
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_InsertNonNaturalBegin: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    SourceArray := FNonNaturalArray;
    TestList := TList.Create;

    StartTime := DateTimeToTimeStamp(Now).Time;
    try
        TestList.Capacity := Length(SourceArray);
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            Item._AddRef;
            TestList.Insert(0, Pointer(Item));
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_InsertNonNaturalEnd: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    SourceArray := FNonNaturalArray;
    TestList := TList.Create;

    StartTime := DateTimeToTimeStamp(Now).Time;
    try
        TestList.Capacity := Length(SourceArray);
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_InsertNaturalRandom: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    SourceArray := FNaturalArray;
    TestList := TList.Create;

    StartTime := DateTimeToTimeStamp(Now).Time;
    try
        TestList.Capacity := Length(SourceArray);
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            Item._AddRef;
            TestList.Insert(Random(TestList.Count), Pointer(Item));
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_InsertNaturalBegin: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    SourceArray := FNaturalArray;
    TestList := TList.Create;

    StartTime := DateTimeToTimeStamp(Now).Time;
    try
        TestList.Capacity := Length(SourceArray);
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            Item._AddRef;
            TestList.Insert(0, Pointer(Item));
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_InsertNaturalEnd: Integer;
var
    SourceArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    SourceArray := FNaturalArray;
    TestList := TList.Create;

    StartTime := DateTimeToTimeStamp(Now).Time;
    try
        TestList.Capacity := Length(SourceArray);
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_RemoveNonNaturalRandom: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    ConsArray := FNonNaturalArray;
    SourceArray := FRandomNonNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            TestList.Remove(Pointer(Item));
            Item._Release;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_RemoveNonNaturalAscending: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    ConsArray := FNonNaturalArray;
    SourceArray := FNonNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            TestList.Remove(Pointer(Item));
            Item._Release;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_RemoveNonNaturalDescending: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    ConsArray := FNonNaturalArray;
    SourceArray := FNonNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        for I := Length(SourceArray) - 1 downto 0 do
        begin
            Item := SourceArray[I];
            TestList.Remove(Pointer(Item));
            Item._Release;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_RemoveNaturalRandom: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I, J: Integer;
begin
    ConsArray := FNaturalArray;
    SourceArray := FRandomNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        for I := 0 to Length(SourceArray) - 1 do
        begin
            for J := 0 to TestList.Count - 1 do
            begin
                Item := ICollectable(TestList[J]);
                if (SourceArray[I] as IString).Value = (Item as IString).Value then
                begin
                    TestList.Delete(J);
                    Item._Release;
                    break;
                end;
            end;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_RemoveNaturalAscending: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I, J: Integer;
begin
    ConsArray := FNaturalArray;
    SourceArray := FNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        for I := 0 to Length(SourceArray) - 1 do
        begin
            for J := 0 to TestList.Count - 1 do
            begin
                Item := ICollectable(TestList[J]);
                if (SourceArray[I] as IString).Value = (Item as IString).Value then
                begin
                    TestList.Delete(J);
                    Item._Release;
                    break;
                end;
            end;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_RemoveNaturalDescending: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I, J: Integer;
begin
    ConsArray := FNaturalArray;
    SourceArray := FNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        for I := Length(SourceArray) - 1 downto 0 do
        begin
            for J := 0 to TestList.Count - 1 do
            begin
                Item := ICollectable(TestList[J]);
                if (SourceArray[I] as IString).Value = (Item as IString).Value then
                begin
                    TestList.Delete(J);
                    Item._Release;
                    break;
                end;
            end;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_RemoveSortedNonNaturalRandom: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    ConsArray := FNonNaturalArray;
    SourceArray := FRandomNonNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            // TAbstractList uses binary searching for sorted lists, making this
            // algorithm look bad, but few developers would write that much
            // client code to improve performance.
            TestList.Remove(Pointer(Item));
            Item._Release;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_RemoveSortedNonNaturalAscending: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    ConsArray := FNonNaturalArray;
    SourceArray := FNonNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        for I := 0 to Length(SourceArray) - 1 do
        begin
            Item := SourceArray[I];
            // TAbstractList uses binary searching for sorted lists, making this
            // algorithm look bad, but few developers would write that much
            // client code to improve performance.
            TestList.Remove(Pointer(Item));
            Item._Release;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_RemoveSortedNonNaturalDescending: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I: Integer;
begin
    ConsArray := FNonNaturalArray;
    SourceArray := FNonNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        for I := Length(SourceArray) - 1 downto 0 do
        begin
            Item := SourceArray[I];
            // TAbstractList uses binary searching for sorted lists, making this
            // algorithm look bad, but few developers would write that much
            // client code to improve performance.
            TestList.Remove(Pointer(Item));
            Item._Release;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_RemoveSortedNaturalRandom: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I, J: Integer;
begin
    ConsArray := FNaturalArray;
    SourceArray := FRandomNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        for I := 0 to Length(SourceArray) - 1 do
        begin
            // TAbstractList uses binary searching for sorted lists, making this
            // algorithm look bad, but few developers would write that much
            // client code to improve performance.
            for J := 0 to TestList.Count - 1 do
            begin
                Item := ICollectable(TestList[J]);
                if (SourceArray[I] as IString).Value = (Item as IString).Value then
                begin
                    TestList.Delete(J);
                    Item._Release;
                    break;
                end;
            end;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_RemoveSortedNaturalAscending: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I, J: Integer;
begin
    ConsArray := FNaturalArray;
    SourceArray := FNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        for I := 0 to Length(SourceArray) - 1 do
        begin
            // TAbstractList uses binary searching for sorted lists, making this
            // algorithm look bad, but few developers would write that much
            // client code to improve performance.
            for J := 0 to TestList.Count - 1 do
            begin
                Item := ICollectable(TestList[J]);
                if (SourceArray[I] as IString).Value = (Item as IString).Value then
                begin
                    TestList.Delete(J);
                    Item._Release;
                    break;
                end;
            end;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_RemoveSortedNaturalDescending: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, I, J: Integer;
begin
    ConsArray := FNaturalArray;
    SourceArray := FNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        for I := Length(SourceArray) - 1 downto 0 do
        begin
            // TAbstractList uses binary searching for sorted lists, making this
            // algorithm look bad, but few developers would write that much
            // client code to improve performance.
            for J := 0 to TestList.Count - 1 do
            begin
                Item := ICollectable(TestList[J]);
                if (SourceArray[I] as IString).Value = (Item as IString).Value then
                begin
                    TestList.Delete(J);
                    Item._Release;
                    break;
                end;
            end;
        end;
        EndTime := DateTimeToTimeStamp(Now).Time;
        DiffTime := EndTime - StartTime;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime;
end;

function TPerformanceTest.TList_ContainsNonNaturalRandom: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, Count, I: Integer;
    Success: Boolean;
begin
    ConsArray := FNonNaturalArray;
    SourceArray := FRandomNonNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        Count := 0;
        DiffTime := 0;
        while DiffTime = 0 do
        begin
            Success := false;
            for I := 0 to Length(SourceArray) - 1 do
            begin
                Success := (TestList.IndexOf(Pointer(SourceArray[I])) <> -1);
            end;
            EndTime := DateTimeToTimeStamp(Now).Time;
            DiffTime := EndTime - StartTime;
            Inc(Count);
            if Success then // Suppreses hints about Success not being used
            begin
            end;
        end;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.TList_ContainsNonNaturalFirst: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, Count, I: Integer;
    Success: Boolean;
begin
    ConsArray := FNonNaturalArray;
    SourceArray := FNonNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        Count := 0;
        DiffTime := 0;
        while DiffTime = 0 do
        begin
            Success := false;
            for I := 0 to Length(SourceArray) - 1 do
            begin
                Success := (TestList.IndexOf(Pointer(SourceArray[0])) <> -1);
            end;
            EndTime := DateTimeToTimeStamp(Now).Time;
            DiffTime := EndTime - StartTime;
            Inc(Count);
            if Success then // Suppreses hints about Success not being used
            begin
            end;
        end;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.TList_ContainsNonNaturalLast: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, Count, I: Integer;
    Success: Boolean;
begin
    ConsArray := FNonNaturalArray;
    SourceArray := FNonNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        Count := 0;
        DiffTime := 0;
        while DiffTime = 0 do
        begin
            Success := false;
            for I := 0 to Length(SourceArray) - 1 do
            begin
                Success := (TestList.IndexOf(Pointer(SourceArray[Length(SourceArray) - 1])) <> -1);
            end;
            EndTime := DateTimeToTimeStamp(Now).Time;
            DiffTime := EndTime - StartTime;
            Inc(Count);
            if Success then // Suppreses hints about Success not being used
            begin
            end;
        end;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.TList_ContainsNaturalRandom: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, Count, I, J: Integer;
    Success: Boolean;
begin
    ConsArray := FNaturalArray;
    SourceArray := FRandomNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        Success := false;
        StartTime := DateTimeToTimeStamp(Now).Time;
        Count := 0;
        DiffTime := 0;
        while DiffTime = 0 do
        begin
            for I := 0 to Length(SourceArray) - 1 do
            begin
                for J := 0 to TestList.Count - 1 do
                begin
                    Success := false;
                    if (SourceArray[I] as IString).Value =
                        (ICollectable(TestList[J]) as IString).Value then
                    begin
                        Success := true;
                        break;
                    end;
                end;
                if Success then // Suppreses hints about Success not being used
                begin
                end;
            end;
            EndTime := DateTimeToTimeStamp(Now).Time;
            DiffTime := EndTime - StartTime;
            Inc(Count);
        end;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.TList_ContainsNaturalFirst: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, Count, I, J: Integer;
    Success: Boolean;
begin
    ConsArray := FNaturalArray;
    SourceArray := FNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        Success := false;
        StartTime := DateTimeToTimeStamp(Now).Time;
        Count := 0;
        DiffTime := 0;
        while DiffTime = 0 do
        begin
            for I := 0 to Length(SourceArray) - 1 do
            begin
                for J := 0 to TestList.Count - 1 do
                begin
                    Success := false;
                    if (SourceArray[0] as IString).Value =
                        (ICollectable(TestList[J]) as IString).Value then
                    begin
                        Success := true;
                        break;
                    end;
                end;
                if Success then // Suppreses hints about Success not being used
                begin
                end;
            end;
            EndTime := DateTimeToTimeStamp(Now).Time;
            DiffTime := EndTime - StartTime;
            Inc(Count);
        end;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.TList_ContainsNaturalLast: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, Count, I, J: Integer;
    Success: Boolean;
begin
    ConsArray := FNaturalArray;
    SourceArray := FNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        Success := false;
        StartTime := DateTimeToTimeStamp(Now).Time;
        Count := 0;
        DiffTime := 0;
        while DiffTime = 0 do
        begin
            for I := 0 to Length(SourceArray) - 1 do
            begin
                for J := 0 to TestList.Count - 1 do
                begin
                    Success := false;
                    if (SourceArray[Length(SourceArray) - 1] as IString).Value =
                        (ICollectable(TestList[J]) as IString).Value then
                    begin
                        Success := true;
                        break;
                    end;
                end;
                if Success then // Suppreses hints about Success not being used
                begin
                end;
            end;
            EndTime := DateTimeToTimeStamp(Now).Time;
            DiffTime := EndTime - StartTime;
            Inc(Count);
        end;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.TList_GetItemRandom: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, Count, I: Integer;
begin
    ConsArray := FNonNaturalArray;
    SourceArray := FRandomNonNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        Count := 0;
        DiffTime := 0;
        while DiffTime = 0 do
        begin
            for I := 0 to Length(SourceArray) - 1 do
            begin
                Item := ICollectable(TestList.Items[I]);
            end;
            EndTime := DateTimeToTimeStamp(Now).Time;
            DiffTime := EndTime - StartTime;
            Inc(Count);
        end;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.TList_GetItemFirst: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, Count, I: Integer;
begin
    ConsArray := FNonNaturalArray;
    SourceArray := FNonNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        Count := 0;
        DiffTime := 0;
        while DiffTime = 0 do
        begin
            for I := 0 to Length(SourceArray) - 1 do
            begin
                Item := ICollectable(TestList.Items[0]);
            end;
            EndTime := DateTimeToTimeStamp(Now).Time;
            DiffTime := EndTime - StartTime;
            Inc(Count);
        end;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.TList_GetItemLast: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, Count, I: Integer;
begin
    ConsArray := FNonNaturalArray;
    SourceArray := FNonNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        Count := 0;
        DiffTime := 0;
        while DiffTime = 0 do
        begin
            for I := 0 to Length(SourceArray) - 1 do
            begin
                Item := ICollectable(TestList.Items[Length(SourceArray) - 1]);
            end;
            EndTime := DateTimeToTimeStamp(Now).Time;
            DiffTime := EndTime - StartTime;
            Inc(Count);
        end;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime div Count;
end;

function TPerformanceTest.TList_Iterator: Integer;
var
    SourceArray, ConsArray: TCollectableArray;
    Item: ICollectable;
    TestList: TList;
    StartTime, EndTime, DiffTime, Count, I: Integer;
begin
    ConsArray := FNonNaturalArray;
    SourceArray := FNonNaturalArray;
    TestList := TList.Create;

    try
        TestList.Capacity := Length(ConsArray);
        for I := 0 to Length(ConsArray) - 1 do
        begin
            Item := ConsArray[I];
            Item._AddRef;
            TestList.Add(Pointer(Item));
        end;
        StartTime := DateTimeToTimeStamp(Now).Time;
        Count := 0;
        DiffTime := 0;
        while DiffTime = 0 do
        begin
            for I := 0 to TestList.Count - 1 do
            begin
                Item := ICollectable(TestList.Items[I]);
            end;
            EndTime := DateTimeToTimeStamp(Now).Time;
            DiffTime := EndTime - StartTime;
            Inc(Count);
        end;
    finally
        for I := 0 to TestList.Count - 1 do
        begin
            Item := ICollectable(TestList[I]);
            Item._Release;
        end;
        TestList.Free;
    end;
    Result := DiffTime div Count;
end;


{ TNonNaturalItem }
constructor TNonNaturalItem.Create(Name: String);
begin
    FName := Name;
end;

{ TTestComparator_NonNat }
function TTestComparator_NonNat.Compare(const Item1, Item2: ICollectable): Integer;
var
    Value1, Value2: String;
begin
    Value1 := TNonNaturalItem(Item1.GetInstance).Name;
    Value2 := TNonNaturalItem(Item2.GetInstance).Name;
    if Value1 < Value2 then
        Result := -1
    else if Value1 > Value2 then
        Result := 1
    else
        Result := 0;
end;

function TTestComparator_NonNat.Equals(const Item1, Item2: ICollectable): Boolean;
var
    Value1, Value2: String;
begin
    Value1 := TNonNaturalItem(Item1.GetInstance).Name;
    Value2 := TNonNaturalItem(Item2.GetInstance).Name;
    Result := (Value1 = Value2);
end;


end.
