unit MemoryLeakTest;

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
 * $Version: v1.1 $
 * $Revision: 1.1 $
 * $Log: D:\QVCS Repositories\Delphi Collections\Test\MemoryLeakTest.qbt $
 * 
 *   Memory leak test.
 * 
 * Revision 1.1  by: Matthew Greet  Rev date: 14/10/04 16:32:54
 *   Memory leak tests ContainsKey for string map and integer maps.
 * 
 * Revision 1.0  by: Matthew Greet  Rev date: 06/06/04 00:24:36
 *   Initial revision.
 * 
 * $Endlog$
 *****************************************************************************)

interface

uses
    Classes, SysUtils, Math, Dialogs,
    Collections, CollWrappers, CollArray, CollPArray, CollHash, CollList, CollLibrary;

type
    IMemoryLeakTestDesc = interface
        ['{EA9BE6E3-A852-11D8-B93A-0002E3165EF8}']
        function GetTestName: String;
        function GetDisplayGroup: Integer;
        function GetDisplayGroupOrder: Integer;
        function GetDescription: String;
        property TestName: String read GetTestName;
        property DisplayGroup: Integer read GetDisplayGroup;
        property DisplayGroupOrder: Integer read GetDisplayGroupOrder;
        property Description: String read GetDescription;
    end;

    TMemoryLeakTestDesc = class(TAbstractStringMappable, IMemoryLeakTestDesc)
    private
        FTestName: String;
        FDisplayGroup: Integer;
        FDisplayGroupOrder: Integer;
        FDescription: String;
    protected
        function MakeKey: String; override;
    public
        constructor Create(TestName: String; DisplayGroup, DisplayGroupOrder: Integer; Description: String);
        function GetTestName: String;
        function GetDisplayGroup: Integer;
        function GetDisplayGroupOrder: Integer;
        function GetDescription: String;
        property TestName: String read GetTestName;
        property Description: String read GetDescription;
    end;

    IMemoryLeakTestResult = interface
        ['{ADD50E00-A6A7-11D8-B93A-0002E3165EF8}']
        function GetMemoryLeak: Integer;
        function GetTestDesc: IMemoryLeakTestDesc;
        procedure SetMemoryLeak(Value: Integer);
        property MemoryLeak: Integer read GetMemoryLeak write SetMemoryLeak;
        property TestDesc: IMemoryLeakTestDesc read GetTestDesc;
    end;

    TMemoryLeakTestResult = class(TAbstractStringMappable, IMemoryLeakTestResult)
    private
        FTestClassName: String;
        FMemoryLeak: Integer;
        FTestDesc: IMemoryLeakTestDesc;
    protected
        function MakeKey: String; override;
    public
        constructor Create(TestClass: TAbstractCollectionClass; TestDesc: IMemoryLeakTestDesc);
        function GetMemoryLeak: Integer;
        function GetTestDesc: IMemoryLeakTestDesc;
        procedure SetMemoryLeak(Value: Integer);
        property MemoryLeak: Integer read GetMemoryLeak write SetMemoryLeak;
        property TestDesc: IMemoryLeakTestDesc read GetTestDesc;
    end;

    TMemoryLeakTest = class
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

        FNonNaturalArray: TCollectableArray;
        FNaturalArray: TCollectableArray;
        FNonNaturalKeyArray: TCollectableArray;
        FNaturalKeyArray: TCollectableArray;
        FIntegerKeyArray: TIntegerArray;
        FStringKeyArray: TStringArray;
        FMappableItemArray: TCollectableArray;
        FIntegerMappableItemArray: TCollectableArray;
        FStringMappableItemArray: TCollectableArray;

        procedure SetCollectionClass(Value: TAbstractCollectionClass);
        procedure SetUseArray(Value: Boolean);
        procedure SetUseTList(Value: Boolean);
        procedure AddTestList;
        procedure AddArrayTestList;
        procedure AddTListTestList;
        procedure DoTest(TestResult: IMemoryLeakTestResult);
        procedure InitializeTest;
        procedure MakeResultDescTable;
        procedure ClearTestList;
    public
        constructor Create;
        destructor Destroy; override;
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
    published
        function CreateNatural: Integer;
        function CreateNonNatural: Integer;
        function AddNatural: Integer;
        function AddNonNatural: Integer;
        function ContainsNatural: Integer;
        function ContainsNonNatural: Integer;
        function Iterator: Integer;
        function PutNatural: Integer;
        function PutNonNatural: Integer;
        function ContainsKeyNatural: Integer;
        function ContainsKeyNonNatural: Integer;
    end;

implementation

type
    TTestMethod = function: Cardinal of object;

    TNonNaturalItem = class(TAbstractItem)
    private
        FName: String;
    public
        constructor Create(Name: String);
        property Name: String read FName;
    end;


{ TMemoryLeakTestDesc }
constructor TMemoryLeakTestDesc.Create(TestName: String; DisplayGroup, DisplayGroupOrder: Integer; Description: String);
begin
    inherited Create;
    FTestName := TestName;
    FDisplayGroup := DisplayGroup;
    FDisplayGroupOrder := DisplayGroupOrder;
    FDescription := Description;
end;

function TMemoryLeakTestDesc.GetTestName: String;
begin
    Result := FTestName;
end;

function TMemoryLeakTestDesc.GetDisplayGroup: Integer;
begin
    Result := FDisplayGroup;
end;

function TMemoryLeakTestDesc.GetDisplayGroupOrder: Integer;
begin
    Result := FDisplayGroupOrder;
end;

function TMemoryLeakTestDesc.GetDescription: String;
begin
    Result := FDescription;
end;

function TMemoryLeakTestDesc.MakeKey: String;
begin
    Result := FTestName;
end;


{ TMemoryLeakTestResult }
constructor TMemoryLeakTestResult.Create(TestClass: TAbstractCollectionClass; TestDesc: IMemoryLeakTestDesc);
begin
    FTestClassName := TestClass.ClassName;
    FTestDesc := TestDesc;
end;

function TMemoryLeakTestResult.MakeKey: String;
begin
    Result := FTestClassName + '.' + FTestDesc.TestName;
end;

function TMemoryLeakTestResult.GetTestDesc: IMemoryLeakTestDesc;
begin
    Result := FTestDesc;
end;

function TMemoryLeakTestResult.GetMemoryLeak: Integer;
begin
    Result := FMemoryLeak;
end;

procedure TMemoryLeakTestResult.SetMemoryLeak(Value: Integer);
begin
    FMemoryLeak := Value;
end;

{ TMemoryLeakTest }
constructor TMemoryLeakTest.Create;
begin
    FResultTable := THashStringMap.Create;
    FCollectionSize := 10;
    MakeResultDescTable;
    InitializeTest;
end;

destructor TMemoryLeakTest.Destroy;
begin
    ClearTestList;
end;

procedure TMemoryLeakTest.SetCollectionClass(Value: TAbstractCollectionClass);
begin
    FCollectionClass := Value;
    FUseArray := false;
    FUseTList := false;
end;

procedure TMemoryLeakTest.SetUseArray(Value: Boolean);
begin
    FUseArray := Value;
    FUseTList := false;
end;

procedure TMemoryLeakTest.SetUseTList(Value: Boolean);
begin
    FUseArray := false;
    FUseTList := Value;
end;

procedure TMemoryLeakTest.MakeResultDescTable;
begin
    FResultDescTable := THashStringMap.Create;
    FResultDescTable.Add(TMemoryLeakTestDesc.Create('CreateNatural',        1, 1, 'Create - natural items') as ICollectable);
    FResultDescTable.Add(TMemoryLeakTestDesc.Create('CreateNonNatural',     1, 2, 'Create - non-natural items') as ICollectable);
    FResultDescTable.Add(TMemoryLeakTestDesc.Create('AddNatural',           2, 1, 'Add/Remove - natural items') as ICollectable);
    FResultDescTable.Add(TMemoryLeakTestDesc.Create('AddNonNatural',        2, 2, 'Add/Remove - non-natural items') as ICollectable);
    FResultDescTable.Add(TMemoryLeakTestDesc.Create('ContainsNatural',      2, 3, 'Contains - natural items') as ICollectable);
    FResultDescTable.Add(TMemoryLeakTestDesc.Create('ContainsNonNatural',   2, 4, 'Contains - non-natural items') as ICollectable);
    FResultDescTable.Add(TMemoryLeakTestDesc.Create('Iterator',             2, 5, 'Iterator') as ICollectable);
    FResultDescTable.Add(TMemoryLeakTestDesc.Create('PutNatural',           3, 1, 'Put/RemoveKey - natural keys') as ICollectable);
    FResultDescTable.Add(TMemoryLeakTestDesc.Create('PutNonNatural',        3, 2, 'Put/RemoveKey - non-natural keys') as ICollectable);
    FResultDescTable.Add(TMemoryLeakTestDesc.Create('ContainsKeyNatural',   3, 3, 'ContainsKey - natural keys') as ICollectable);
    FResultDescTable.Add(TMemoryLeakTestDesc.Create('ContainsKeyNonNatural',3, 4, 'ContainsKey - non-natural keys') as ICollectable);
end;

procedure TMemoryLeakTest.TestClass;
var
    TestResult: IMemoryLeakTestResult;
    Iterator: IIterator;
begin
//    InitializeTest;
    ResultTable.Clear;
    if FUseArray then
        AddArrayTestList
    else if FUseTList then
        AddTListTestList
    else
        AddTestList;

    Iterator := ResultTable.GetIterator;
    Iterator.First;
    while not Iterator.EOF do
    begin
        TestResult := Iterator.CurrentItem as IMemoryLeakTestResult;
        DoTest(TestResult);
        Iterator.Next;
    end;
end;

procedure TMemoryLeakTest.DoTest(TestResult: IMemoryLeakTestResult);
var
    TestMethod: TTestMethod;
    Method: TMethod;
    P: Pointer;
    TestName: String;
begin
    TestName := TestResult.TestDesc.TestName;
    P := TMemoryLeakTest.MethodAddress(TestName);
    if Assigned(P) then
    begin
        Method.Code := P;
        Method.Data := Self;
        TestMethod := TTestMethod(Method);
        try
            TestResult.MemoryLeak := TestMethod;
        except
            TestResult.MemoryLeak := 999999;
        end;
    end
    else
        TestResult.MemoryLeak := 999999;
end;

procedure TMemoryLeakTest.InitializeTest;
var
    Item, Key: ICollectable;
    ItemText, KeyText, NumberText: String;
    I, Digits: Integer;
begin
    SetLength(FNonNaturalArray, CollectionSize);
    SetLength(FNaturalArray, CollectionSize);
    SetLength(FNonNaturalKeyArray, CollectionSize);
    SetLength(FNaturalKeyArray, CollectionSize);
    SetLength(FIntegerKeyArray, CollectionSize);
    SetLength(FStringKeyArray, CollectionSize);
    SetLength(FMappableItemArray, CollectionSize);
    SetLength(FIntegerMappableItemArray, CollectionSize);
    SetLength(FStringMappableItemArray, CollectionSize);
    Digits := Trunc(Log10(CollectionSize));
    for I := 0 to CollectionSize - 1 do
    begin
        NumberText := IntToStr(I + 1);
        ItemText := 'Item' + StringOfChar('0', Digits - Length(NumberText)) + NumberText;
        KeyText := 'Key' + StringOfChar('0', Digits - Length(NumberText)) + NumberText;

        Item := TNonNaturalItem.Create(ItemText);
        FNonNaturalArray[I] := Item;
        Key := TNonNaturalItem.Create(KeyText);
        FNonNaturalKeyArray[I] := Key;

        Item := TStringWrapper.Create(ItemText);
        FNaturalArray[I] :=Item;
        Key := TStringWrapper.Create(KeyText);
        FNaturalKeyArray[I] := Key;

        FIntegerKeyArray[I] := I;
        FStringKeyArray[I] := KeyText;

        Item := TAssociationWrapper.Create(ItemText, TObject.Create);
        FMappableItemArray[I] := Item;

        Item := TIntegerAssociationWrapper.Create(I, TObject.Create);
        FIntegerMappableItemArray[I] := Item;

        Item := TStringAssociationWrapper.Create(ItemText, TObject.Create);
        FStringMappableItemArray[I] := Item;
    end;
end;

procedure TMemoryLeakTest.ClearTestList;
begin
    FResultTable.Clear;
end;

procedure TMemoryLeakTest.AddArrayTestList;
begin
end;

procedure TMemoryLeakTest.AddTListTestList;
begin
end;

procedure TMemoryLeakTest.AddTestList;
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
        AlwaysNaturalKeys := false;

    FResultTable.Add(TMemoryLeakTestResult.Create(CollectionClass, ResultDescTable['CreateNatural'] as IMemoryLeakTestDesc) as ICollectable);
    if not AlwaysNaturalItems and ((CollectionType = ctBag) or (CollectionType = ctSet) or (CollectionType = ctList)) then
        FResultTable.Add(TMemoryLeakTestResult.Create(CollectionClass, ResultDescTable['CreateNonNatural'] as IMemoryLeakTestDesc) as ICollectable);

    if not FixedSize then
    begin
        FResultTable.Add(TMemoryLeakTestResult.Create(CollectionClass, ResultDescTable['AddNatural'] as IMemoryLeakTestDesc) as ICollectable);
        if not AlwaysNaturalItems and ((CollectionType = ctBag) or (CollectionType = ctSet) or (CollectionType = ctList)) then
           FResultTable.Add(TMemoryLeakTestResult.Create(CollectionClass, ResultDescTable['AddNonNatural'] as IMemoryLeakTestDesc) as ICollectable);
    end;

    FResultTable.Add(TMemoryLeakTestResult.Create(CollectionClass, ResultDescTable['ContainsNatural'] as IMemoryLeakTestDesc) as ICollectable);
    if not AlwaysNaturalItems then
        FResultTable.Add(TMemoryLeakTestResult.Create(CollectionClass, ResultDescTable['ContainsNonNatural'] as IMemoryLeakTestDesc) as ICollectable);
    FResultTable.Add(TMemoryLeakTestResult.Create(CollectionClass, ResultDescTable['Iterator'] as IMemoryLeakTestDesc) as ICollectable);

    if not FixedSize and ((CollectionType = ctMap) or (CollectionType = ctIntegerMap) or (CollectionType = ctStringMap)) then
    begin
        FResultTable.Add(TMemoryLeakTestResult.Create(CollectionClass, ResultDescTable['PutNatural'] as IMemoryLeakTestDesc) as ICollectable);
    end;
    if not FixedSize and (CollectionType = ctMap) and not AlwaysNaturalKeys then
    begin
        FResultTable.Add(TMemoryLeakTestResult.Create(CollectionClass, ResultDescTable['PutNonNatural'] as IMemoryLeakTestDesc) as ICollectable);
    end;

    if (CollectionType = ctMap) or (CollectionType = ctIntegerMap) or (CollectionType = ctStringMap) then
    begin
        FResultTable.Add(TMemoryLeakTestResult.Create(CollectionClass, ResultDescTable['ContainsKeyNatural'] as IMemoryLeakTestDesc) as ICollectable);
    end;
    if (CollectionType = ctMap) and not AlwaysNaturalKeys then
    begin
        FResultTable.Add(TMemoryLeakTestResult.Create(CollectionClass, ResultDescTable['ContainsKeyNonNatural'] as IMemoryLeakTestDesc) as ICollectable);
    end;
end;

function TMemoryLeakTest.CreateNatural: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartAllocated, EndAllocated: Cardinal;
    DiffAllocated: Integer;
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
    TestCollection := nil;
    StartAllocated := GetHeapStatus.TotalAllocated;

    for I := 1 to FIterations do
    begin
        TestCollection := CollectionClass.Create(SourceArray);
        TestCollection := nil;
    end;

    EndAllocated := GetHeapStatus.TotalAllocated;
    DiffAllocated := EndAllocated - StartAllocated;
    Result := DiffAllocated;
end;

function TMemoryLeakTest.CreateNonNatural: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    StartAllocated, EndAllocated: Cardinal;
    DiffAllocated: Cardinal;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    SourceArray := FNonNaturalArray;
    TestCollection := nil;
    StartAllocated := GetHeapStatus.TotalAllocated;

    for I := 1 to FIterations do
    begin
        TestCollection := CollectionClass.Create(SourceArray);
        TestCollection := nil;
    end;

    EndAllocated := GetHeapStatus.TotalAllocated;
    DiffAllocated := EndAllocated - StartAllocated;
    Result := DiffAllocated;
end;

function TMemoryLeakTest.AddNatural: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection, RemoveCollection: ICollection;
    CollectionType: TCollectionType;
    StartAllocated, EndAllocated: Cardinal;
    DiffAllocated: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    // Capacity preset first to avoid appearing as a memory leak
    TestCollection.Capacity := 2 * FCollectionSize;
    CollectionType := TestCollection.GetType;
    if CollectionType = ctMap then
        SourceArray := FMappableItemArray
    else if CollectionType = ctIntegerMap then
        SourceArray := FIntegerMappableItemArray
    else if CollectionType = ctStringMap then
        SourceArray := FStringMappableItemArray
    else
        SourceArray := FNaturalArray;

    StartAllocated := GetHeapStatus.TotalAllocated;
    for I := 1 to FIterations do
    begin
        TestCollection.Add(SourceArray);
        RemoveCollection := TestCollection.Remove(SourceArray);
    end;
    // Remove result must be removed here, not at stack windup, otherwise it
    // appears as a leak.
    RemoveCollection := nil;

    EndAllocated := GetHeapStatus.TotalAllocated;
    DiffAllocated := EndAllocated - StartAllocated;
    TestCollection := nil;
    Result := DiffAllocated;
end;

function TMemoryLeakTest.AddNonNatural: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection, RemoveCollection: ICollection;
    StartAllocated, EndAllocated: Cardinal;
    DiffAllocated: Integer;
    I: Integer;
begin
    TestCollection := CollectionClass.Create;
    // Capacity preset first to avoid appearing as a memory leak
    TestCollection.Capacity := 2 * FCollectionSize;
    SourceArray := FNonNaturalArray;
    StartAllocated := GetHeapStatus.TotalAllocated;

    for I := 1 to FIterations do
    begin
        TestCollection.Add(SourceArray);
        RemoveCollection := TestCollection.Remove(SourceArray);
    end;
    // Remove result must be removed here, not at stack windup, otherwise it
    // appears as a leak.
    RemoveCollection := nil;

    EndAllocated := GetHeapStatus.TotalAllocated;
    DiffAllocated := EndAllocated - StartAllocated;
    TestCollection := nil;
    Result := DiffAllocated;
end;

function TMemoryLeakTest.ContainsNatural: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartAllocated, EndAllocated: Cardinal;
    DiffAllocated: Integer;
    I, J: Integer;
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
    TestCollection := CollectionClass.Create(SourceArray);

    StartAllocated := GetHeapStatus.TotalAllocated;
    for I := 1 to FIterations do
    begin
        for J := Low(SourceArray) to High(SourceArray) do
        begin
            TestCollection.Contains(SourceArray[J]);
        end;
    end;

    EndAllocated := GetHeapStatus.TotalAllocated;
    DiffAllocated := EndAllocated - StartAllocated;
    TestCollection := nil;
    Result := DiffAllocated;
end;

function TMemoryLeakTest.ContainsNonNatural: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    CollectionType: TCollectionType;
    StartAllocated, EndAllocated: Cardinal;
    DiffAllocated: Integer;
    I, J: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    SourceArray := FNonNaturalArray;
    if CollectionType = ctMap then
        TestCollection := TAbstractMapClass(CollectionClass).Create(FNaturalKeyArray, SourceArray, false, true)
    else if CollectionType = ctIntegerMap then
        TestCollection := TAbstractIntegerMapClass(CollectionClass).Create(FIntegerKeyArray, SourceArray, false)
    else if CollectionType = ctStringMap then
        TestCollection := TAbstractStringMapClass(CollectionClass).Create(FStringKeyArray, SourceArray, false)
    else
        TestCollection := CollectionClass.Create(SourceArray);

    StartAllocated := GetHeapStatus.TotalAllocated;
    for I := 1 to FIterations do
    begin
        for J := Low(SourceArray) to High(SourceArray) do
        begin
            TestCollection.Contains(SourceArray[J]);
        end;
    end;

    EndAllocated := GetHeapStatus.TotalAllocated;
    DiffAllocated := EndAllocated - StartAllocated;
    TestCollection := nil;
    Result := DiffAllocated;
end;

function TMemoryLeakTest.Iterator: Integer;
var
    SourceArray: TCollectableArray;
    TestCollection: ICollection;
    Item: ICollectable;
    Iterator: IIterator;
    CollectionType: TCollectionType;
    StartAllocated, EndAllocated: Cardinal;
    DiffAllocated: Integer;
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
    TestCollection := CollectionClass.Create(SourceArray);

    StartAllocated := GetHeapStatus.TotalAllocated;
    for I := 1 to FIterations do
    begin
        Iterator := TestCollection.GetIterator;
        while not Iterator.EOF do
        begin
            Item := Iterator.CurrentItem;
            Iterator.Next;
        end;
    end;
    // Stack variables must be removed here, not at stack windup, otherwise it
    // appears as a leak.
    Iterator := nil;
    Item := nil;

    EndAllocated := GetHeapStatus.TotalAllocated;
    DiffAllocated := EndAllocated - StartAllocated;
    TestCollection := nil;
    Result := DiffAllocated;
end;

function TMemoryLeakTest.PutNatural: Integer;
var
    SourceArray: TCollectableArray;
    KeyArray: TCollectableArray;
    IntegerKeyArray: TIntegerArray;
    StringKeyArray: TStringArray;
    TestCollection: ICollection;
    RemoveItem: ICollectable;
    TestMap: IMap;
    TestIntegerMap: IIntegerMap;
    TestStringMap: IStringMap;
    CollectionType: TCollectionType;
    StartAllocated, EndAllocated: Cardinal;
    DiffAllocated: Integer;
    I, J: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    SourceArray := FNonNaturalArray;
    case CollectionType of
    ctMap: begin
        KeyArray := FNaturalKeyArray;
        TestMap := TAbstractMapClass(CollectionClass).Create(false, true);
        // Capacity preset first to avoid appearing as a memory leak
        TestMap.Capacity := 2 * FCollectionSize;
    end;
    ctIntegerMap: begin
        IntegerKeyArray := FIntegerKeyArray;
        TestIntegerMap := TAbstractIntegerMapClass(CollectionClass).Create(false);
        TestIntegerMap.Capacity := 2 * FCollectionSize;
    end;
    ctStringMap: begin
        StringKeyArray := FStringKeyArray;
        TestStringMap := TAbstractStringMapClass(CollectionClass).Create(false);
        TestStringMap.Capacity := 2 * FCollectionSize;
    end;
    end;

    StartAllocated := GetHeapStatus.TotalAllocated;
    for I := 1 to FIterations do
    begin
        case CollectionType of
        ctMap: begin
            for J := Low(KeyArray) to High(KeyArray) do
            begin
                RemoveItem := TestMap.Put(KeyArray[J], SourceArray[J]);
                RemoveItem := TestMap.RemoveKey(KeyArray[J]);
            end;
        end;
        ctIntegerMap: begin
            for J := Low(IntegerKeyArray) to High(IntegerKeyArray) do
            begin
                RemoveItem := TestIntegerMap.Put(IntegerKeyArray[J], SourceArray[J]);
                RemoveItem := TestIntegerMap.RemoveKey(IntegerKeyArray[J]);
            end;
        end;
        ctStringMap: begin
            for J := Low(StringKeyArray) to High(StringKeyArray) do
            begin
                RemoveItem := TestStringMap.Put(StringKeyArray[J], SourceArray[J]);
                RemoveItem := TestStringMap.RemoveKey(StringKeyArray[J]);
            end;
        end;
        end;
    end;
    // Item removed here, not at stack windup.  Does not affect test as it was
    // created elsewhere but it's good practice.
    RemoveItem := nil;

    EndAllocated := GetHeapStatus.TotalAllocated;
    DiffAllocated := EndAllocated - StartAllocated;
    TestCollection := nil;
    Result := DiffAllocated;
end;

function TMemoryLeakTest.PutNonNatural: Integer;
var
    SourceArray: TCollectableArray;
    KeyArray: TCollectableArray;
    RemoveItem: ICollectable;
    TestMap: IMap;
    StartAllocated, EndAllocated: Cardinal;
    DiffAllocated: Integer;
    I, J: Integer;
begin
    SourceArray := FNonNaturalArray;
    KeyArray := FNaturalKeyArray;
    TestMap := TAbstractMapClass(CollectionClass).Create(false, false);
    // Capacity preset first to avoid appearing as a memory leak
    TestMap.Capacity := 2 * FCollectionSize;

    StartAllocated := GetHeapStatus.TotalAllocated;
    for I := 1 to FIterations do
    begin
        for J := Low(KeyArray) to High(KeyArray) do
        begin
            RemoveItem := TestMap.Put(KeyArray[J], SourceArray[J]);
            RemoveItem := TestMap.RemoveKey(KeyArray[J]);
        end;
    end;
    // Item removed here, not at stack windup.  Does not affect test as it was
    // created elsewhere but it's good practice.
    RemoveItem := nil;

    EndAllocated := GetHeapStatus.TotalAllocated;
    DiffAllocated := EndAllocated - StartAllocated;
    Result := DiffAllocated;
end;

function TMemoryLeakTest.ContainsKeyNatural: Integer;
var
    SourceArray: TCollectableArray;
    KeyArray: TCollectableArray;
    IntegerKeyArray: TIntegerArray;
    StringKeyArray: TStringArray;
    TestCollection: ICollection;
    TestMap: IMap;
    TestIntegerMap: IIntegerMap;
    TestStringMap: IStringMap;
    CollectionType: TCollectionType;
    StartAllocated, EndAllocated: Cardinal;
    DiffAllocated: Integer;
    I, J: Integer;
begin
    TestCollection := CollectionClass.Create;
    CollectionType := TestCollection.GetType;
    SourceArray := FNonNaturalArray;
    case CollectionType of
    ctMap: begin
        KeyArray := FNaturalKeyArray;
        TestMap := TAbstractMapClass(CollectionClass).Create(KeyArray, SourceArray, false, true);
    end;
    ctIntegerMap: begin
        IntegerKeyArray := FIntegerKeyArray;
        TestIntegerMap := TAbstractIntegerMapClass(CollectionClass).Create(IntegerKeyArray, SourceArray, false);
    end;
    ctStringMap: begin
        StringKeyArray := FStringKeyArray;
        TestStringMap := TAbstractStringMapClass(CollectionClass).Create(StringKeyArray, SourceArray, false);
    end;
    end;

    StartAllocated := GetHeapStatus.TotalAllocated;
    for I := 1 to FIterations do
    begin
        case CollectionType of
        ctMap: begin
            for J := Low(KeyArray) to High(KeyArray) do
            begin
                TestMap.ContainsKey(KeyArray[J]);
            end;
        end;
        ctIntegerMap: begin
            for J := Low(IntegerKeyArray) to High(IntegerKeyArray) do
            begin
                TestIntegerMap.ContainsKey(IntegerKeyArray[J]);
            end;
        end;
        ctStringMap: begin
            for J := Low(StringKeyArray) to High(StringKeyArray) do
            begin
                TestStringMap.ContainsKey(StringKeyArray[J]);
            end;
        end;
        end;
    end;

    EndAllocated := GetHeapStatus.TotalAllocated;
    DiffAllocated := EndAllocated - StartAllocated;
    TestCollection := nil;
    Result := DiffAllocated;
end;

function TMemoryLeakTest.ContainsKeyNonNatural: Integer;
var
    SourceArray: TCollectableArray;
    KeyArray: TCollectableArray;
    TestCollection: ICollection;
    TestMap: IMap;
    StartAllocated, EndAllocated: Cardinal;
    DiffAllocated: Integer;
    I, J: Integer;
begin
    TestCollection := CollectionClass.Create;
    SourceArray := FNonNaturalArray;
    KeyArray := FNonNaturalKeyArray;
    TestMap := TAbstractMapClass(CollectionClass).Create(KeyArray, SourceArray, false, false);

    StartAllocated := GetHeapStatus.TotalAllocated;
    for I := 1 to FIterations do
    begin
        for J := Low(KeyArray) to High(KeyArray) do
        begin
            TestMap.ContainsKey(KeyArray[J]);
        end;
    end;

    EndAllocated := GetHeapStatus.TotalAllocated;
    DiffAllocated := EndAllocated - StartAllocated;
    TestCollection := nil;
    Result := DiffAllocated;
end;


{ TNonNaturalItem }
constructor TNonNaturalItem.Create(Name: String);
begin
    FName := Name;
end;

end.
