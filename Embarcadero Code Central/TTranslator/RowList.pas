{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: RowList.pas,v 1.8 2002/12/27 15:07:32 laa Exp $}

unit RowList;

interface

uses
  Classes,
  DataTypes, DataType, DataElements;

type
  TDataRowList = class;

  TDataRowListSortCompare = function(List: TDataRowList; Index1, Index2: Integer): Integer;

  TDataRowList = class(TStrings)
  private
    FIsUpdating : Boolean;

    // Copied from TStringList
    FList: PStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TDataRowListSortCompare);
    procedure InsertItem(Index: Integer; const S: string);
    procedure SetSorted(Value: Boolean);

    // New methods
    function GetDataRow(idx : Integer) : TDataRow;
    function GetAbstractRow(idx : Integer) : TAbstractRow;
  protected
    // All protected methods are copied from TStringList
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    // Methods copied from TStringList
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: string; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TDataRowListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;

    // New methods
    constructor Create;
    {/** DataRow number idx */}
    property DataRows[idx : Integer] : TDataRow read GetDataRow; default;
    property AbstractRows[idx : Integer] : TAbstractRow read GetAbstractRow;
    {/** FirstRow or nil if count = 0 */}
    function FirstRow : TDataRow;
    {/** LastRow or nil if count = 0 */}
    function LastRow : TDataRow;
    {/** Fill with Description */}
    procedure FillStringsWithDescription(DescrField : TDataField);
    {/** Fill with Key and Description */}
    procedure FillStringsWithKeyAndDescription(KeyField : TDataField);
    procedure FillStringsOptional(AField : TDataField; DisplayValues : TDisplayValues);
    procedure FillStrings(AField : TDataField);
    procedure RemoveInvisible;
    {/** Free all rows in list and set count to 0 */}
    procedure FreeAndClearContents;
    {/** Add to list from RowSource. Note that you have to free the rows yourself! */}
    class procedure AddFromSource(Source : TRowSource; Dest : TStrings);
    {/** GetRows */}
    procedure GetRows(Results : TStrings; Condition : TCondition; Action : TGetAction); virtual;

    procedure ReplaceSubTotalsWithDetails;
    procedure RemoveDuplicatesByFields(Fields : TDataFieldSet);
    procedure RemoveDuplicates;
    function SearchOrderedList(SortOrder : TRowSortOrder; SearchValues : TValueList; var FirstHit, LastHit : Integer) : Boolean;

    function HasMatchingRows(CompareFields : TDataFieldSet; CompareRow : TAbstractRow) : Boolean;
  end;

implementation

uses

  SysUtils, Criteria;

{ TDataRowList }

constructor TDataRowList.Create;
begin
  inherited Create;
  FIsUpdating := False;
end;

// Methods from TStringList

destructor TDataRowList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TDataRowList.Add(const S: string): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error({$ifdef USE_RESOURCESTRINGS}@SDuplicateString{$else}'SDuplicateString'{$endif}, 0);
      end;
  InsertItem(Result, S);
end;

procedure TDataRowList.Changed;
begin
  if (not FIsUpdating) and Assigned(FOnChange) then FOnChange(Self);
end;

procedure TDataRowList.Changing;
begin
  if (not FIsUpdating) and Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TDataRowList.Clear;
begin
  if FCount <> 0 then
  begin
    Changing;
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

procedure TDataRowList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}@SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS}, Index);
  Changing;
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TStringItem));
  Changed;
end;

procedure TDataRowList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}@SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS}, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}@SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS}, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TDataRowList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Integer;
  Item1, Item2: PStringItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := Integer(Item1^.FString);
  Integer(Item1^.FString) := Integer(Item2^.FString);
  Integer(Item2^.FString) := Temp;
  Temp := Integer(Item1^.FObject);
  Integer(Item1^.FObject) := Integer(Item2^.FObject);
  Integer(Item2^.FObject) := Temp;
end;

function TDataRowList.Find(const S: string; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := AnsiCompareText(FList^[I].FString, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TDataRowList.Get(Index: Integer): string;
begin
  if (Index < 0) or (Index >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}@SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS}, Index);
  Result := FList^[Index].FString;
end;

function TDataRowList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TDataRowList.GetCount: Integer;
begin
  Result := FCount;
end;

function TDataRowList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}@SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS}, Index);
  Result := FList^[Index].FObject;
end;

procedure TDataRowList.Grow;
var
  NewSize: Integer;
begin
  if FCapacity < 16 then
    NewSize := 16
  else
    NewSize := FCapacity * 2;

  SetCapacity(NewSize);
end;

function TDataRowList.IndexOf(const S: string): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S) else
    if not Find(S, Result) then Result := -1;
end;

procedure TDataRowList.Insert(Index: Integer; const S: string);
begin
  if Sorted then Error({$ifdef USE_RESOURCESTRINGS}@SSortedListError{$else}'SSortedListError'{$endif USE_RESOURCESTRINGS}, 0);
  if (Index < 0) or (Index > FCount) then Error({$ifdef USE_RESOURCESTRINGS}@SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS}, Index);
  InsertItem(Index, S);
end;

procedure TDataRowList.InsertItem(Index: Integer; const S: string);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TStringItem));
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FObject := nil;
    FString := S;
  end;
  Inc(FCount);
  Changed;
end;

procedure TDataRowList.Put(Index: Integer; const S: string);
begin
  if Sorted then Error({$ifdef USE_RESOURCESTRINGS}@SSortedListError{$else}'SSortedListError'{$endif USE_RESOURCESTRINGS}, 0);
  if (Index < 0) or (Index >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}@SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS}, Index);
  Changing;
  FList^[Index].FString := S;
  Changed;
end;

procedure TDataRowList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error({$ifdef USE_RESOURCESTRINGS}@SListIndexError{$else}'SListIndexError'{$endif USE_RESOURCESTRINGS}, Index);
  Changing;
  FList^[Index].FObject := AObject;
  Changed;
end;

procedure TDataRowList.QuickSort(L, R: Integer; SCompare: TDataRowListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TDataRowList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TStringItem));
  FCapacity := NewCapacity;
end;

procedure TDataRowList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TDataRowList.SetUpdateState(Updating: Boolean);
begin
  FIsUpdating := Updating;

  if Updating then Changing else Changed;
end;

function DataRowListAnsiCompare(List: TDataRowList; Index1, Index2: Integer): Integer;
begin
  Result := AnsiCompareText(List.FList^[Index1].FString,
                            List.FList^[Index2].FString);
end;

procedure TDataRowList.Sort;
begin
  CustomSort(DataRowListAnsiCompare);
end;

procedure TDataRowList.CustomSort(Compare: TDataRowListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

// -- New methods

type
  TRowSourceLink = class(TRowSource);

class procedure TDataRowList.AddFromSource(Source : TRowSource; Dest : TStrings);
var
  Row, CopyOfRow : TDataRow;
  GotRow : Boolean;
begin
  Assert(Source <> nil, 'TDataRowList.AddFromSource: Source <> nil');

  TRowSourceLink(nil).DoStartOperations(Source);

  repeat
    Row := TRowSourceLink(nil).DoGetNextRow(Source);
    GotRow := Row <> nil;
    if GotRow then
    begin
      CopyOfRow := TDataRow.Create(Row.DataTable);
      CopyOfRow.CopyContents(Row);
      Dest.AddObject('', CopyOfRow);
    end;
  until (not GotRow);

  TRowSourceLink(nil).DoFinishOperations(Source);
end;

function TDataRowList.GetDataRow(idx : Integer) : TDataRow;
begin
  Assert((idx >= 0) and (idx < Count), 'TDataRowList.GetDataRow: (idx >= 0) and (idx < Count), idx: ' + IntToStr(idx));

  Result := TDataRow(Objects[idx]);
end;

function TDataRowList.GetAbstractRow(idx : Integer) : TAbstractRow;
begin
  Assert((idx >= 0) and (idx < Count), 'TDataRowList.GetAbstractRow: (idx >= 0) and (idx < Count), idx: ' + IntToStr(idx));

  Result := TAbstractRow(Objects[idx]);
end;

procedure TDataRowList.RemoveInvisible;
var
  i : Integer;
begin
  for i := Count - 1 downto 0 do
    if not DataRows[i].Visible then
      Self.Delete(i);
end;

function TDataRowList.FirstRow : TDataRow;
begin
  if Count = 0 then
    Result := nil
  else
    Result := DataRows[0];
end;

function TDataRowList.LastRow : TDataRow;
begin
  if Count = 0 then
    Result := nil
  else
    Result := DataRows[Count - 1];
end;

procedure TDataRowList.FreeAndClearContents;
var
  i : Integer;
begin
  for i := 0 to Count - 1 do
    Objects[i].Free;
  Clear;
end;

function TDataRowList.HasMatchingRows(CompareFields : TDataFieldSet; CompareRow : TAbstractRow) : Boolean;
var
  iRow, iField : Integer;
  AllEqual : Boolean;
  Iterator : TDataFieldSetIterator;
begin
  Result := False;

  Iterator := TDataFieldSetIterator.Create(CompareFields);
  for iRow := Count - 1 downto 0 do
  begin
    AllEqual := True;

    if CompareRow.DataTable = AbstractRows[iRow].DataTable then
    begin
      for iField := 0 to CompareRow.DataTable.FieldCount - 1 do
        if CompareFields.IncludeField(CompareRow.DataTable, iField) and
           not CompareRow.DataTable.Field[iField].DataType.Equals(CompareRow.ValueByIndex[iField], AbstractRows[iRow].ValueByIndex[iField]) then
        begin
          AllEqual := False;
          Break;
        end;
    end
    else
    begin
      Iterator.First;
      while not Iterator.EOF do
      begin
        if not Iterator.Field.DataType.Equals(CompareRow[Iterator.Field], AbstractRows[iRow][Iterator.Field]) then
        begin
          AllEqual := False;
          Break;
        end;

        Iterator.Next;
      end;
    end;

    if AllEqual then
    begin
      Result := True;
      Exit;
    end;
  end;

  Iterator.Free;
end;

procedure TDataRowList.FillStringsWithDescription(DescrField : TDataField);
var
  i : Integer;
begin
  Assert(DescrField <> nil, 'TDataRowList.FillStringsWithDescription: DescrField <> nil');

  for i := 0 to Count - 1 do
  begin
    Strings[i] := DataRows[i].StringValue[DescrField];
  end;
end;

procedure TDataRowList.FillStringsOptional(AField : TDataField; DisplayValues : TDisplayValues);
var
  i : Integer;
  ARow : TAbstractRow;
begin
  for i := 0 to Count - 1 do
  begin
    ARow := DataRows[i];
    if ARow <> nil then
      Strings[i] := ARow.DisplayString[AField, DisplayValues];
  end;
end;

procedure TDataRowList.FillStrings(AField : TDataField);
begin
  FillStringsOptional(AField, AField.DisplayValues);
end;

procedure TDataRowList.FillStringsWithKeyAndDescription(KeyField : TDataField);
var
  i : Integer;
begin
  Assert(KeyField <> nil, 'TDataRowList.FillStringsWithKeyAndDescription: KeyField <> nil');

  if KeyField.TextField = nil then
    FillStringsWithDescription(KeyField)
  else
  begin
    for i := 0 to Count - 1 do
    begin
      Strings[i] := DataRows[i].KeyAndDescription[KeyField]

{      idx := DataRows[i].IndexOfField(DescrField);
      if idx = -1 then
      else
        Strings[i] := DataRows[i].StringValue[KeyField] + ' ' + DataRows[i].StringByIndex[idx]; }
    end;
  end;
end;

(*
class procedure TDataRowList.OrderRowsByIndex(Rows : TStrings; Order : array of TOrderInt; MinIndex, MaxIndex : Integer);
var
  CmpRow, TmpRow : TDataRow;
  i, iMax, iMin : Integer;
  CmpRes : Boolean;
begin
  {Assert((Rows <> nil) and (MaxIndex >= 0) and (MaxIndex < Rows.Count) and (MinIndex >= 0) and (MinIndex < Rows.Count),
         'TDataRowList.OrderRows: (Rows <> nil) and (MaxIndex >= 0) and (MaxIndex < Rows.Count) and (MinIndex >= 0) and (MinIndex < Rows.Count)');}

  if MaxIndex - MinIndex < 2 then
  begin
    if (MaxIndex > MinIndex) and TAbstractRow(Rows.Objects[MaxIndex]).BeforeByIndex(TDataRow(Rows.Objects[MinIndex]), Order) then
      Rows.Exchange(MinIndex, MaxIndex);
    Exit;
  end;

  Rows.Exchange(MaxIndex, MinIndex + Random(MaxIndex - MinIndex));

  CmpRow := TDataRow(Rows.Objects[MaxIndex]);
  iMax := MaxIndex - 1;
  iMin := MinIndex;
  i := iMax;

  while iMax > iMin do
  begin
    TmpRow := TDataRow(Rows.Objects[i]);
    CmpRes := CmpRow.BeforeByIndex(TmpRow, Order);
    if i = iMax then
    begin
      if CmpRes then
      begin
        Dec(iMax);
        Dec(i);
      end
      else
      begin
        Rows.Exchange(iMax, iMin);
        Inc(iMin);
        i := iMin;
      end;
    end
    else
    begin
      if not CmpRes then
      begin
        Inc(iMin);
        Inc(i);
      end
      else
      begin
        Rows.Exchange(iMax, iMin);
        Dec(iMax);
        i := iMax;
      end;
    end;
  end;

  TmpRow := TDataRow(Rows.Objects[i]);
  CmpRes := CmpRow.BeforeByIndex(TmpRow, Order);
  if CmpRes then
  begin
    Rows.Exchange(i, MaxIndex);
    OrderRowsByIndex(Rows, Order, MinIndex, i - 1);
    OrderRowsByIndex(Rows, Order, i + 1, MaxIndex);
  end
  else
  begin
    Rows.Exchange(i + 1, MaxIndex);
    OrderRowsByIndex(Rows, Order, MinIndex, i);
    OrderRowsByIndex(Rows, Order, i + 2, MaxIndex);
  end;
end;
*)

(*
class procedure TDataRowList.OrderRows(Rows : TStrings; Order : array of TOrderField; MinIndex, MaxIndex : Integer);
var
  RowSortOrder : TRowSortOrder;
  i : Integer;
begin
  RowSortOrder := TRowSortOrder.Create;
  for i := Low(Order) to High(Order) do
    RowSortOrder.AddRule(Order[i].Field, Order[i].Order);

  RowSortOrder.OrderRowsInternal(Rows, MinIndex, MaxIndex);
  RowSortOrder.Free;
end;
*)


(*
var
  CmpRow, TmpRow : TDataRow;
  i, iMax, iMin : Integer;
  CmpRes : Boolean;
begin
  {Assert((Rows <> nil) and (MaxIndex >= 0) and (MaxIndex < Rows.Count) and (MinIndex >= 0) and (MinIndex < Rows.Count),
         'TDataRowList.OrderRows: (Rows <> nil) and (MaxIndex >= 0) and (MaxIndex < Rows.Count) and (MinIndex >= 0) and (MinIndex < Rows.Count)');}

  if MaxIndex - MinIndex < 2 then
  begin
    if (MaxIndex > MinIndex) and TAbstractRow(Rows.Objects[MaxIndex]).Before_Old(TDataRow(Rows.Objects[MinIndex]), Order) then
      Rows.Exchange(MinIndex, MaxIndex);
    Exit;
  end;

  Rows.Exchange(MaxIndex, MinIndex + Random(MaxIndex - MinIndex));

  CmpRow := TDataRow(Rows.Objects[MaxIndex]);
  iMax := MaxIndex - 1;
  iMin := MinIndex;
  i := iMax;

  while iMax > iMin do
  begin
    TmpRow := TDataRow(Rows.Objects[i]);
    CmpRes := CmpRow.Before_Old(TmpRow, Order);
    if i = iMax then
    begin
      if CmpRes then
      begin
        Dec(iMax);
        Dec(i);
      end
      else
      begin
        Rows.Exchange(iMax, iMin);
        Inc(iMin);
        i := iMin;
      end;
    end
    else
    begin
      if not CmpRes then
      begin
        Inc(iMin);
        Inc(i);
      end
      else
      begin
        Rows.Exchange(iMax, iMin);
        Dec(iMax);
        i := iMax;
      end;
    end;
  end;

  TmpRow := TDataRow(Rows.Objects[i]);
  CmpRes := CmpRow.Before_Old(TmpRow, Order);
  if CmpRes then
  begin
    Rows.Exchange(i, MaxIndex);
    OrderRows(Rows, Order, MinIndex, i - 1);
    OrderRows(Rows, Order, i + 1, MaxIndex);
  end
  else
  begin
    Rows.Exchange(i + 1, MaxIndex);
    OrderRows(Rows, Order, MinIndex, i);
    OrderRows(Rows, Order, i + 2, MaxIndex);
  end;
end;
*)

procedure TDataRowList.ReplaceSubTotalsWithDetails;
var
  iRow : Integer;
begin
  iRow := 0;
  while iRow < Self.Count do
  begin
    Self.AbstractRows[iRow].ReplaceWithDetails(Self, iRow);
  end;
end;

procedure TDataRowList.RemoveDuplicatesByFields(Fields : TDataFieldSet);
var
  Order : TRowSortOrder;
  DataTable : TDataTable;
  i : Integer;
begin
  if Self.Count = 0 then
    Exit;

  Order := TRowSortOrder.Create;

  DataTable := AbstractRows[0].DataTable;

  if Fields = nil then
  begin
    for i := 0 to DataTable.FieldCount - 1 do
      if Fields.IncludeField(DataTable, i) then
        Order.AddRule(DataTable.Field[i], DataTable.Field[i].SortOrder);
  end
  else
    Fields.ProcFields(Order.AddDefaultRule);

  Order.OrderRows(DataTable, Self);
  Order.Free;

  for i := Self.Count - 2 downto 0 do
    if Fields.RowsEqual(DataRows[i], DataRows[i+1]) then
      Delete(i+1);
end;

procedure TDataRowList.RemoveDuplicates;
var
  Order : TRowSortOrder;
  DataTable : TDataTable;
  i : Integer;
begin
  if Self.Count = 0 then
    Exit;

  DataTable := AbstractRows[0].DataTable;
  Order := TRowSortOrder.Create;
  for i := 0 to DataTable.KeyCount - 1 do
    Order.AddRule(DataTable.Field[i], DataTable.Field[i].SortOrder);

  Order.OrderRows(DataTable, Self);
  Order.Free;

  for i := Self.Count - 2 downto 0 do
  begin
    if DataRows[i] = DataRows[i+1] then
      Delete(i+1);
  end;
end;

procedure TDataRowList.GetRows(Results : TStrings; Condition : TCondition; Action : TGetAction);
var
  DelCrit : Boolean;
  i : Integer;
begin
  DelCrit := (Condition = nil);
  if DelCrit then
    Condition := TCriteria.Create;

  for i := Count - 1 downto 0 do
    if Condition.AcceptsRow(DataRows[i]) then
    begin
      case Action of
        gaReference: Results.InsertObject(0, '', DataRows[i]);
        gaCopy:      Results.InsertObject(0, '', DataRows[i].CreateCopy);
        gaCut:       begin
                       Results.InsertObject(0, '', DataRows[i].CreateCopy);
                       DataRows[i].Delete;
                       Delete(i);
                     end;
        gaDelete:    begin
                       DataRows[i].Delete;
                       Delete(i);
                     end;
      end;
    end;

  if DelCrit then
    Condition.Free;
end;

function TDataRowList.SearchOrderedList(SortOrder : TRowSortOrder; SearchValues : TValueList; var FirstHit, LastHit : Integer) : Boolean;

  function FindFirst(Low, High : Integer) : Integer;
  var
    iCurr : Integer;
    CompRes : Integer;
  begin
    while True do
    begin
      if Low = High then
      begin
        Result := High;
        Exit;
      end;

      iCurr := (Low + High) div 2;
      CompRes := DataRows[iCurr].CompareToValues(SearchValues, SortOrder);

      if CompRes <> 0 then
        Low := iCurr + 1
      else
        High := iCurr;
    end;
  end;

  function FindLast(Low, High : Integer) : Integer;
  var
    iCurr : Integer;
    CompRes : Integer;
  begin
    while True do
    begin
      if Low = High then
      begin
        Result := Low;
        Exit;
      end;

      iCurr := (Low + High + 1) div 2;
      CompRes := DataRows[iCurr].CompareToValues(SearchValues, SortOrder);

      if CompRes <> 0 then
        High := iCurr - 1
      else
        Low := iCurr;
    end;
  end;

var
  iCurr : Integer;
  CompRes : Integer;
begin
  FirstHit := 0;
  LastHit := Self.Count - 1;

  while True do
  begin
    if LastHit < FirstHit then
    begin
      Result := False;
      Exit;
    end;

    iCurr := (FirstHit + LastHit) div 2;
    CompRes := DataRows[iCurr].CompareToValues(SearchValues, SortOrder);

    if CompRes > 0 then
      LastHit := iCurr - 1
    else if CompRes < 0 then
      FirstHit := iCurr + 1
    else
    begin
      FirstHit := FindFirst(FirstHit, iCurr);
      LastHit := FindLast(iCurr, LastHit);
      Result := True;
      Exit;
    end;
  end;
end;
(*
procedure TDataRowList.WriteToStream(AStream : TStream);
const
  NewLine : String = #13#10;
var
  i : Integer;
begin
  i := Self.Count;
  AStream.Write(i, SizeOf(Integer));
  AStream.Write(NewLine[1], Length(NewLine));
  for i := 0 to Count - 1 do
  begin
    AStream.Write(AbstractRows[i].FData^, AbstractRows[i].DataTable.DataLength);
    AStream.Write(NewLine[1], Length(NewLine));
  end;
end;

procedure TDataRowList.ReadFromStream(DataTable : TDataTable; AStream : TStream);
const
  NewLine : String = #13#10;
var
  i : Integer;
  ACount : Integer;
  Data : PByte;
  Pos : Integer;
begin
  GetMem(Data, DataTable.DataLength + Length(NewLine));
  AStream.Read(ACount, SizeOf(Integer));
  AStream.Read(Data^, Length(NewLine));
  for i := 0 to ACount - 1 do
  begin
    AStream.Read(Data^, DataTable.DataLength + Length(NewLine));
    Pos := 0;
    Self.AddObject('', TDataRow.CreateFromByteData(DataTable, Data, Pos));
  end;
  FreeMem(Data);
end;
*)



end.

