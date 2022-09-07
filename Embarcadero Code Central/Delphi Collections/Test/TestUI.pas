unit TestUI;

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
 * $Revision: 1.0.1.2 $
 * $Log: D:\QVCS Repositories\Delphi Collections\Test\TestUI.qbt $
 * 
 *   Initial version.
 * 
 * Revision 1.0.1.2  by: Matthew Greet  Rev date: 12/06/04 19:47:58
 *   Memory leak tests.
 * 
 * Revision 1.0.1.1  by: Matthew Greet  Rev date: 24/10/03 15:39:18
 *   v1.0 branch.
 * 
 * Revision 1.0  by: Matthew Greet  Rev date: 06/04/03 11:22:28
 *   Initial revision.
 * 
 * $Endlog$
 *****************************************************************************)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids,
  FunctionalTest, PerformanceTest, MemoryLeakTest,
  Collections, CollArray, CollHash, CollList,
  CollPArray, CollWrappers, CollLibrary, ComCtrls;

type
  TTestForm = class(TForm)
    TestSubjectGroupBox: TGroupBox;
    ClassNameComboBox: TComboBox;
    ClassNameLabel: TLabel;
    TypeLabel: TLabel;
    TypeEdit: TEdit;
    Label1: TLabel;
    NaturalItemTypeEdit: TEdit;
    FixedSizeCheckBox: TCheckBox;
    NaturalKeyTypeEdit: TEdit;
    Label2: TLabel;
    FixedSizeLabel: TLabel;
    CloseButton: TButton;
    TestPageControl: TPageControl;
    FunctionalTabSheet: TTabSheet;
    PerformanceTabSheet: TTabSheet;
    FunctionalTestButton: TButton;
    FunctionalTestListBox: TListBox;
    CollectionSizeLabel: TLabel;
    CollectionSizeComboBox: TComboBox;
    ComparisonTestButton: TButton;
    IterationsLabel: TLabel;
    PerfIterationsComboBox: TComboBox;
    PerformanceTestButton: TButton;
    PerfTestStatusLabel: TLabel;
    PerformanceTestStringGrid: TStringGrid;
    MemoryLeakTabSheet: TTabSheet;
    Iterations2Label: TLabel;
    LeakIterationsComboBox: TComboBox;
    LeakTestStatusLabel: TLabel;
    MemoryLeakButton: TButton;
    MemoryLeakTestStringGrid: TStringGrid;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ClassNameComboBoxChange(Sender: TObject);
    procedure FunctionalTestButtonClick(Sender: TObject);
    procedure PerformanceTestButtonClick(Sender: TObject);
    procedure ComparisonTestButtonClick(Sender: TObject);
    procedure MemoryLeakButtonClick(Sender: TObject);
  private
    procedure PerformanceIterationStart(Sender: TObject);
    procedure PerformanceIterationComplete(Sender: TObject);
    procedure PerformanceIterationStart2(Sender: TObject);
    procedure PerformanceIterationComplete2(Sender: TObject);
    function PerfTestDescOrderCompare(const Item1, Item2: ICollectable): Integer;
    function LeakTestDescOrderCompare(const Item1, Item2: ICollectable): Integer;
  public
    TestCollectionClassType: TAbstractCollectionClass;
    TestCollectionType: TCollectionType;
    TestCollectionFixedSize: Boolean;
    TestCollectionNaturalItemIID: TGUID;
    TestCollectionNaturalKeyIID: TGUID;
  end;

var
  TestForm: TTestForm;

implementation

uses ComparisonUI;

{$R *.DFM}

procedure TTestForm.PerformanceIterationStart(Sender: TObject);
var
    Test: TPerformanceTest;
begin
    Test := Sender as TPerformanceTest;
    PerfTestStatusLabel.Caption := Test.CurrentTestName + ' iteration ' +
        IntToStr(Test.CurrentIteration) + ' of ' + IntToStr(Test.Iterations) +
        ' started...';
    PerfTestStatusLabel.Update;
end;

procedure TTestForm.PerformanceIterationComplete(Sender: TObject);
var
    Test: TPerformanceTest;
begin
    Test := Sender as TPerformanceTest;
    PerfTestStatusLabel.Caption := Test.CurrentTestName + ' iteration ' +
        IntToStr(Test.CurrentIteration) + ' of ' + IntToStr(Test.Iterations) +
        ' complete.';
    PerfTestStatusLabel.Update;
end;

procedure TTestForm.PerformanceIterationStart2(Sender: TObject);
var
    Test: TPerformanceTest;
    TestClassName: String;
begin
    Test := Sender as TPerformanceTest;
    if Test.UseArray then
        TestClassname := '(Array)'
    else if Test.UseTList then
        TestClassName := '(TList)'
    else
        TestClassName := Test.CollectionClass.ClassName;

    PerfTestStatusLabel.Caption := TestClassName + '.' + Test.CurrentTestName + '  ' +
        IntToStr(Test.CurrentIteration) + ' of ' + IntToStr(Test.Iterations) +
        ' started...';
    PerfTestStatusLabel.Update;
end;

procedure TTestForm.PerformanceIterationComplete2(Sender: TObject);
var
    Test: TPerformanceTest;
    TestClassName: String;
begin
    Test := Sender as TPerformanceTest;
    if Test.UseArray then
        TestClassname := '(Array)'
    else if Test.UseTList then
        TestClassName := '(TList)'
    else
        TestClassName := Test.CollectionClass.ClassName;

    PerfTestStatusLabel.Caption := TestClassName + '.' + Test.CurrentTestName + '  ' +
        IntToStr(Test.CurrentIteration) + ' of ' + IntToStr(Test.Iterations) +
        ' complete.';
    PerfTestStatusLabel.Update;
end;

function TTestForm.PerfTestDescOrderCompare(const Item1, Item2: ICollectable): Integer;
var
    TestResult1, TestResult2: TPerformanceTestDesc;
begin
    TestResult1 := TPerformanceTestDesc(Item1.GetInstance);
    TestResult2 := TPerformanceTestDesc(Item2.GetInstance);
    if TestResult1.DisplayGroup < TestResult2.DisplayGroup then
        Result := -1
    else if TestResult1.DisplayGroup > TestResult2.DisplayGroup then
        Result := 1
    else
    begin
        if TestResult1.DisplayGroupOrder < TestResult2.DisplayGroupOrder then
            Result := -1
        else if TestResult1.DisplayGroupOrder > TestResult2.DisplayGroupOrder then
            Result := 1
        else
            Result := 0;
    end;
end;

function TTestForm.LeakTestDescOrderCompare(const Item1, Item2: ICollectable): Integer;
var
    TestResult1, TestResult2: IMemoryLeakTestDesc;
begin
    TestResult1 := (Item1 as IMemoryLeakTestResult).TestDesc;
    TestResult2 := (Item2 as IMemoryLeakTestResult).TestDesc;
    if TestResult1.DisplayGroup < TestResult2.DisplayGroup then
        Result := -1
    else if TestResult1.DisplayGroup > TestResult2.DisplayGroup then
        Result := 1
    else
    begin
        if TestResult1.DisplayGroupOrder < TestResult2.DisplayGroupOrder then
            Result := -1
        else if TestResult1.DisplayGroupOrder > TestResult2.DisplayGroupOrder then
            Result := 1
        else
            Result := 0;
    end;
end;

procedure TTestForm.CloseButtonClick(Sender: TObject);
begin
    Close;
end;

procedure TTestForm.FormCreate(Sender: TObject);
begin
    ClassNameComboBox.ItemIndex := 0;
    ClassNameComboBoxChange(ClassNameComboBox);
    PerfTestStatusLabel.Caption := '';
end;

procedure TTestForm.ClassNameComboBoxChange(Sender: TObject);
var
    Collection: ICollection;
begin
    if ClassNameComboBox.Text = '(Array)' then
        TestCollectionClassType := nil
    else if ClassNameComboBox.Text = '(TList)' then
        TestCollectionClassType := nil
    else
        TestCollectionClassType := TMiscCollectionLibrary.ClassNameToClassType(ClassNameComboBox.Text);
    if TestCollectionClassType <> nil then
    begin
        try
            Collection := TestCollectionClassType.Create;
            TestCollectionType := Collection.GetType;
            TestCollectionFixedSize := Collection.GetFixedSize;
            TestCollectionNaturalItemIID := Collection.GetNaturalItemIID;
            if TestCollectionType = ctMap then
                TestCollectionNaturalKeyIID := (Collection as IMap).GetNaturalKeyIID
            else
                TestCollectionNaturalKeyIID := IUnknown;
        except
            // Ignore errors
        end;

        case TestCollectionType of
            ctBag: TypeEdit.Text := 'Bag';
            ctSet: TypeEdit.Text := 'Set';
            ctList: TypeEdit.Text := 'List';
            ctMap: TypeEdit.Text := 'Map';
            ctIntegerMap: TypeEdit.Text := 'Integer map';
            ctStringMap: TypeEdit.Text := 'String map';
        end;
        FixedSizeCheckBox.Checked := TestCollectionFixedSize;
        if TMiscCollectionLibrary.EqualIID(TestCollectionNaturalItemIID, EquatableIID) then
            NaturalItemTypeEdit.Text := 'IEquatable'
        else if TMiscCollectionLibrary.EqualIID(TestCollectionNaturalItemIID, ComparableIID) then
            NaturalItemTypeEdit.Text := 'IComparable'
        else if TMiscCollectionLibrary.EqualIID(TestCollectionNaturalItemIID, HashableIID) then
            NaturalItemTypeEdit.Text := 'IHashable'
        else if TMiscCollectionLibrary.EqualIID(TestCollectionNaturalItemIID, MappableIID) then
            NaturalItemTypeEdit.Text := 'IMappable'
        else if TMiscCollectionLibrary.EqualIID(TestCollectionNaturalItemIID, IntegerMappableIID) then
            NaturalItemTypeEdit.Text := 'IIntegerMappable'
        else if TMiscCollectionLibrary.EqualIID(TestCollectionNaturalItemIID, StringMappableIID) then
            NaturalItemTypeEdit.Text := 'IStringMappable'
        else
            NaturalItemTypeEdit.Text := 'Error';
        if TMiscCollectionLibrary.EqualIID(TestCollectionNaturalKeyIID, EquatableIID) then
            NaturalKeyTypeEdit.Text := 'IEquatable'
        else if TMiscCollectionLibrary.EqualIID(TestCollectionNaturalKeyIID, ComparableIID) then
            NaturalKeyTypeEdit.Text := 'IComparable'
        else if TMiscCollectionLibrary.EqualIID(TestCollectionNaturalKeyIID, HashableIID) then
            NaturalKeyTypeEdit.Text := 'IHashable'
        else if TMiscCollectionLibrary.EqualIID(TestCollectionNaturalKeyIID, MappableIID) then
            NaturalKeyTypeEdit.Text := 'IMappable'
        else
            NaturalKeyTypeEdit.Text := '';
    end
    else
    begin
        TypeEdit.Text := '';
        NaturalItemTypeEdit.Text := '';
        NaturalKeyTypeEdit.Text := '';
    end;
end;

procedure TTestForm.FunctionalTestButtonClick(Sender: TObject);
var
    FunctionalTest: TFunctionalTest;
    TestResult: TFunctionalTestResult;
    TestClassName: String;
    I: Integer;
    TotalSuccess: Boolean;
begin
    Screen.Cursor := crHourGlass;
    TestClassName := ClassNameComboBox.Text;
    FunctionalTest := TFunctionalTest.Create;
    try
        if TestCollectionClassType <> nil then
        begin
            FunctionalTest.CollectionClass := TestCollectionClassType;
            FunctionalTest.TestClass;

            with FunctionalTestListBox do
            begin
                Items.Clear;
                TotalSuccess := true;
                for I := 0 to FunctionalTest.ResultList.Count - 1 do
                begin
                    TestResult := TFunctionalTestResult(FunctionalTest.ResultList[I]);
                    TotalSuccess := TotalSuccess and TestResult.Success;
                    if TestResult.Success then
                        Items.Add(TestResult.Name + ' successful')
                    else
                        Items.Add(TestResult.Name + ' failed');
                end;
                Items.Add('');
                if TotalSuccess then
                    Items.Add(TestClassName + ' functional test passed.')
                else
                    Items.Add(TestClassName + ' functional test failed.');
                ItemIndex := Items.Count - 1;
            end;
        end
        else
        begin
            with FunctionalTestListBox do
            begin
                Items.Clear;
                Items.Add('No test for ' + TestClassName);
            end;
        end;
    finally
        FunctionalTest.Free;
        Screen.Cursor := crDefault;
    end;
end;

procedure TTestForm.PerformanceTestButtonClick(Sender: TObject);
var
    ResultTable: IStringMap;
    SortedTests: IList;
    Item: ICollectable;
    PerformanceTest: TPerformanceTest;
    TestDesc: TPerformanceTestDesc;
    TestResult: TPerformanceTestResult;
    Key, TestClassName, TimeText: String;
    I, J: Integer;
    CanTest: Boolean;
begin
    PerformanceTest := TPerformanceTest.Create;
    try
        Screen.Cursor := crHourGlass;

        TestClassName := ClassNameComboBox.Text;
        PerformanceTest.CollectionSize := StrToInt(CollectionSizeComboBox.Text);
        PerformanceTest.Iterations := StrToInt(PerfIterationsComboBox.Text);
        PerformanceTest.OnIterationStart := PerformanceIterationStart;
        PerformanceTest.OnIterationComplete := PerformanceIterationComplete;

        PerfTestStatusLabel.Caption := 'Test starting...';
        PerfTestStatusLabel.Update;
        if TestClassName = '(Array)' then
        begin
            PerformanceTest.UseArray := true;
            CanTest := true;
        end
        else if TestClassName = '(TList)' then
        begin
            PerformanceTest.UseTList := true;
            CanTest := true;
        end
        else if TestCollectionClassType <> nil then
        begin
            PerformanceTest.CollectionClass := TestCollectionClassType;
            CanTest := true;
        end
        else
            CanTest := false;

        if CanTest then
        begin
            PerformanceTest.TestClass;
            ResultTable := PerformanceTest.ResultTable;
            SortedTests := TPArrayList.Create(PerformanceTest.ResultDescTable);
            SortedTests.Sort(PerfTestDescOrderCompare);
        end
        else
        begin
            ResultTable := THashStringMap.Create;
            ResultTable.Add(TPerformanceTestResult.Create(TestCollectionClassType, 'NoTest') as ICollectable);
            SortedTests := TPArrayList.Create;
            SortedTests.Add(TPerformanceTestDesc.Create('NoTest', 1, 1, 'No test for ' + TestClassName + ':') as ICollectable);
        end;
        PerfTestStatusLabel.Caption := 'Test complete.';
        PerfTestStatusLabel.Update;

        with PerformanceTestStringGrid do
        begin
            RowCount := 0;
            RowCount := SortedTests.GetSize;
            ColWidths[0] := 260;
            ColWidths[1] := 110;
            J := 0;
            for I := 0 to SortedTests.GetSize - 1 do
            begin
                TestDesc := (SortedTests.GetItem(I).GetInstance) as TPerformanceTestDesc;
                Key := TestClassName + '.' + TestDesc.TestName;
                Item := ResultTable.Get(Key);
                // If test was run for this class
                if Item <> nil then
                begin
                    TestResult := (Item.GetInstance) as TPerformanceTestResult;
                    Cells[0, J] := TestDesc.Description;
                    TimeText := IntToStr(TestResult.Milliseconds);
                    Cells[1, J] := StringOfChar(' ', 12 - 2 * Length(TimeText)) + TimeText + ' milliseconds';
                    Inc(J);
                end;
            end;
        end;
    finally
        Screen.Cursor := crDefault;
        PerformanceTest.Free;
    end;
end;

procedure TTestForm.ComparisonTestButtonClick(Sender: TObject);
var
    ResultTable: IStringMap;
    TotalResultsTable: IStringMap;
    ClassDescTable: IStringMap;
    TestCollection: ICollection;
    PerformanceTest: TPerformanceTest;
    CollectionClass: TAbstractCollectionClass;
    TestClassName: String;
    I: Integer;
    CanTest: Boolean;
begin
    PerformanceTest := TPerformanceTest.Create;
    ClassDescTable := THashStringMap.Create;
    TotalResultsTable := THashStringMap.Create;
    try
        Screen.Cursor := crHourGlass;

        TestClassName := ClassNameComboBox.Text;
        PerformanceTest.CollectionSize := StrToInt(CollectionSizeComboBox.Text);
        PerformanceTest.Iterations := StrToInt(PerfIterationsComboBox.Text);
        PerformanceTest.OnIterationStart := PerformanceIterationStart2;
        PerformanceTest.OnIterationComplete := PerformanceIterationComplete2;

        PerfTestStatusLabel.Caption := 'Test starting...';
        PerfTestStatusLabel.Update;
        for I := 0 to ClassNameComboBox.Items.Count - 1 do
        begin
            TestClassName := ClassNameComboBox.Items[I];
            if TestClassName = '(Array)' then
            begin
                PerformanceTest.UseArray := true;
                CanTest := true;
                ClassDescTable.Add(TPerformanceTestClassDesc.Create(TestClassName, true, ctBag) as ICollectable);
            end
            else if TestClassName = '(TList)' then
            begin
                PerformanceTest.UseTList := true;
                CanTest := true;
                ClassDescTable.Add(TPerformanceTestClassDesc.Create(TestClassName, true, ctBag) as ICollectable);
            end
            else
            begin
                CollectionClass := TMiscCollectionLibrary.ClassNameToClassType(TestClassName);
                if CollectionClass <> nil then
                begin
                    TestCollection := CollectionClass.Create;
                    PerformanceTest.CollectionClass := CollectionClass;
                    ClassDescTable.Add(TPerformanceTestClassDesc.Create(TestClassName, false, TestCollection.GetType) as ICollectable);
                end;
                CanTest := CollectionClass <> nil;
            end;

            if CanTest then
            begin
                PerformanceTest.TestClass;
                ResultTable := PerformanceTest.ResultTable;
                TotalResultsTable.Add(ResultTable);
            end;
        end;
        PerfTestStatusLabel.Caption := 'Test complete.';
        PerfTestStatusLabel.Update;

        ComparisonTestForm.ShowComparisonResults(PerformanceTest.ResultDescTable, ClassDescTable, TotalResultsTable);
    finally
        Screen.Cursor := crDefault;
        PerformanceTest.Free;
    end;

end;

procedure TTestForm.MemoryLeakButtonClick(Sender: TObject);
var
    ResultTable: IStringMap;
    SortedTests: IList;
    Item: ICollectable;
    MemoryLeakTest: TMemoryLeakTest;
    TestDesc: IMemoryLeakTestDesc;
    TestResult: IMemoryLeakTestResult;
    Key, TestClassName, LeakText: String;
    I, J: Integer;
    CanTest: Boolean;
begin
    MemoryLeakTest := TMemoryLeakTest.Create;
    try
        Screen.Cursor := crHourGlass;

        TestClassName := ClassNameComboBox.Text;
        MemoryLeakTest.CollectionSize := StrToInt(CollectionSizeComboBox.Text);
        MemoryLeakTest.Iterations := StrToInt(LeakIterationsComboBox.Text);

        LeakTestStatusLabel.Caption := 'Test starting...';
        LeakTestStatusLabel.Update;
        if TestClassName = '(Array)' then
        begin
            MemoryLeakTest.UseArray := true;
            CanTest := true;
        end
        else if TestClassName = '(TList)' then
        begin
            MemoryLeakTest.UseTList := true;
            CanTest := true;
        end
        else if TestCollectionClassType <> nil then
        begin
            MemoryLeakTest.CollectionClass := TestCollectionClassType;
            CanTest := true;
        end
        else
            CanTest := false;

        if CanTest then
        begin
            MemoryLeakTest.TestClass;
            ResultTable := MemoryLeakTest.ResultTable;
        end
        else
        begin
            ResultTable := THashStringMap.Create;
            ResultTable.Add(TMemoryLeakTestResult.Create(TestCollectionClassType, TMemoryLeakTestDesc.Create('NoTest', 1, 1, 'No test for ' + TestClassName + ':')) as ICollectable);
        end;
        SortedTests := TPArrayList.Create(ResultTable);
        SortedTests.Sort(LeakTestDescOrderCompare);
        LeakTestStatusLabel.Caption := 'Test complete.';
        LeakTestStatusLabel.Update;

        with MemoryLeakTestStringGrid do
        begin
            Cells[0,0] := '';
            Cells[1,0] := '';
            RowCount := SortedTests.GetSize;
            ColWidths[0] := 260;
            ColWidths[1] := 110;
            J := 0;
            for I := 0 to SortedTests.GetSize - 1 do
            begin
                TestDesc := (SortedTests.GetItem(I) as IMemoryLeakTestResult).TestDesc;
                Key := TestClassName + '.' + TestDesc.TestName;
                Item := ResultTable.Get(Key);
                // If test was run for this class
                if Item <> nil then
                begin
                    TestResult := (Item.GetInstance) as TMemoryLeakTestResult;
                    Cells[0, J] := TestDesc.Description;
                    LeakText := IntToStr(TestResult.MemoryLeak);
                    Cells[1, J] := StringOfChar(' ', 12 - 2 * Length(LeakText)) + LeakText + ' bytes lost';
                    Inc(J);
                end;
            end;
        end;
    finally
        Screen.Cursor := crDefault;
        MemoryLeakTest.Free;
    end;
end;

end.
