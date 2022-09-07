unit ComparisonUI;

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
 * $Log: D:\QVCS Repositories\Delphi Collections\Test\ComparisonUI.qbt $
 * 
 *   Initial version.
 * 
 * Revision 1.0  by: Matthew Greet  Rev date: 06/04/03 11:22:24
 *   Initial revision.
 * 
 * $Endlog$
 *****************************************************************************)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids,
  PerformanceTest, Collections, CollPArray, CollWrappers, ExtCtrls;

type
  TComparisonTestForm = class(TForm)
    ComparisonTestStringGrid: TStringGrid;
    Panel1: TPanel;
    Panel2: TPanel;
    CloseButton: TButton;
    procedure ComparisonTestStringGridDrawCell(Sender: TObject; ACol,
      ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSortedTests: IList;
    FSortedClasses: IList;
    function ClassDescOrderCompare(const Item1, Item2: ICollectable): Integer;
    function TestDescOrderCompare(const Item1, Item2: ICollectable): Integer;
  public
    procedure ShowComparisonResults(TestDescTable, ClassDescTable, TotalResultsTable: IStringMap);
  end;

var
  ComparisonTestForm: TComparisonTestForm;

implementation

{$R *.DFM}

function TComparisonTestForm.TestDescOrderCompare(const Item1, Item2: ICollectable): Integer;
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

function TComparisonTestForm.ClassDescOrderCompare(const Item1, Item2: ICollectable): Integer;
var
    TestResult1, TestResult2: TPerformanceTestClassDesc;
begin
    TestResult1 := TPerformanceTestClassDesc(Item1.GetInstance);
    TestResult2 := TPerformanceTestClassDesc(Item2.GetInstance);
    if not TestResult1.NonCollection and TestResult2.NonCollection then
        Result := -1
    else if TestResult1.NonCollection and not TestResult2.NonCollection then
        Result := 1
    else
    begin
        if TestResult1.CollectionType < TestResult2.CollectionType then
            Result := -1
        else if TestResult1.CollectionType > TestResult2.CollectionType then
            Result := 1
        else
            Result := CompareStr(TestResult1.TestClassName, TestResult2.TestClassName);
    end;
end;

procedure TComparisonTestForm.ShowComparisonResults(TestDescTable, ClassDescTable, TotalResultsTable: IStringMap);
var
    Item: ICollectable;
    TestDescription: TPerformanceTestDesc;
    TestResult: TPerformanceTestResult;
    Key: String;
    I, J: Integer;
begin
    with ComparisonTestStringGrid do
    begin
        FixedRows := 1;
        FixedCols := 1;
        RowCount := TestDescTable.GetSize + 1;
        ColCount := ClassDescTable.GetSize + 1;
        ColWidths[0] := 110;
        FSortedTests.Clear;
        FSortedTests.Add(TestDescTable);
        FSortedTests.Sort(TestDescOrderCompare);
        FSortedClasses.Clear;
        FSortedClasses.Add(ClassDescTable);
        FSortedClasses.Sort(ClassDescOrderCompare);

        for J := 0 to FSortedClasses.GetSize - 1 do
        begin
            ColWidths[J + 1] := 64;
            Cells[J + 1, 0] := TPerformanceTestClassDesc(FSortedClasses.GetItem(J).GetInstance).TestClassName;
        end;

        for I := 0 to FSortedTests.GetSize - 1 do
        begin
            TestDescription := TPerformanceTestDesc(FSortedTests.GetItem(I).GetInstance);
            Cells[0, I + 1] := TestDescription.Description;
            for J := 0 to FSortedClasses.GetSize - 1 do
            begin
                Key := TPerformanceTestClassDesc(FSortedClasses.GetItem(J).GetInstance).TestClassName +
                    '.' + TestDescription.TestName;
                Item := TotalResultsTable.Get(Key);
                if Item <> nil then
                begin
                    // Test applies to this class
                    TestResult := TPerformanceTestResult(Item.GetInstance);
                    Cells[J + 1, I + 1] := IntToStr(TestResult.Milliseconds);
                end
                else
                    Cells[J + 1, I + 1] := '';
            end;
        end;
    end;
    ShowModal;
end;

procedure TComparisonTestForm.ComparisonTestStringGridDrawCell(
  Sender: TObject; ACol, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
var
    ClassDesc: TPerformanceTestClassDesc;
    TestDesc: TPerformanceTestDesc;
    ColGrey, RowGrey: Boolean;
    TempRect: TRect;
    Text: String;
begin
    with Sender as TStringGrid do
    begin
        if (gdSelected in State) and (not (gdFocused in State) or
            ([goDrawFocusSelected, goRowSelect] * Options <> [])) then
        begin
            Canvas.Brush.Color := clHighlight;
            Canvas.Font.Color := clHighlightText;
        end
        else if gdFixed in State then
        begin
            Canvas.Brush.Color := FixedColor;
            Canvas.Font.Color := Font.Color;
        end
        else
        begin
            ClassDesc := TPerformanceTestClassDesc(FSortedClasses.GetItem(ACol - 1).GetInstance);
            TestDesc := TPerformanceTestDesc(FSortedTests.GetItem(ARow - 1).GetInstance);
            ColGrey := ((Ord(ClassDesc.CollectionType) and 1) = 1) and
                not ClassDesc.NonCollection;
            RowGrey := ((TestDesc.DisplayGroup and 1) = 1);

            if ColGrey and RowGrey then
                Canvas.Brush.Color := $00CCCCCC // 20% grey
            else if ColGrey or RowGrey then
                Canvas.Brush.Color := $00E5E5E5 // 10% grey
            else
                Canvas.Brush.Color := Color;;

            Canvas.Font.Color := Font.Color;
        end;
        Canvas.FillRect(Rect);

        if (gdFixed in State) and Ctl3D then
        begin
            TempRect := Rect;
            DrawEdge(Canvas.Handle, TempRect, BDR_RAISEDINNER, BF_RIGHT or BF_BOTTOM);
            DrawEdge(Canvas.Handle, TempRect, BDR_RAISEDINNER, BF_LEFT or BF_TOP);
        end;
        if (gdFocused in State) and([goEditing, goAlwaysShowEditor] * Options <>
                [goEditing, goAlwaysShowEditor])
            and not (goRowSelect in Options) then
        begin
            DrawFocusRect(Canvas.Handle, Rect)
        end;

        Text := Cells[ACol, ARow];
        TempRect.Left := Rect.Left + 2;
        TempRect.Right := Rect.Right - 2;
        TempRect.Top := Rect.Top + 2;
        TempRect.Bottom := Rect.Bottom - 2;
        if gdFixed in State then
            DrawText(Canvas.Handle, PChar(Text), Length(Text), TempRect,
                DT_LEFT or DT_NOPREFIX or DT_WORDBREAK)
        else
            DrawText(Canvas.Handle, PChar(Text), Length(Text), TempRect,
                DT_RIGHT or DT_NOPREFIX or DT_WORDBREAK);
    end;
end;

procedure TComparisonTestForm.FormCreate(Sender: TObject);
begin
    FSortedTests := TPArrayList.Create;
    FSortedClasses := TPArrayList.Create;
end;

procedure TComparisonTestForm.FormDestroy(Sender: TObject);
begin
    FSortedTests := nil;
    FSortedClasses := nil;
end;

end.
