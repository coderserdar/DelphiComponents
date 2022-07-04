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

{ $Id: Storages.pas,v 1.29 2003/03/28 12:18:06 lge Exp $}

unit Storages;

interface
{$i common.inc}

uses
  Classes,
  DataElements, DataType, DataTypes, Interruptable;

type
  TCustomRowStorage = class(TAbstractRowStorage)
  private
    procedure SetAutoArrange(Arrange : Boolean);
  protected
    FAutoArrange : Boolean;
    procedure FillRowList; virtual;
    function GetCommonCriteria : TCondition; virtual;
    function GetRow(idx : Integer) : TAbstractRow; virtual;
    function GetRowCount : Integer; virtual;
  public
    {/** Publish at this level... */}
    property Total;
    property CommonCriteria : TCondition read GetCommonCriteria;
    {/** Destructor */}
    destructor Destroy; override;
    {/** Number of rows in the storage */}
    property RowCount : Integer read GetRowCount;
    {/** Row in table, contains sums if ShowSubTotals is set to true */}
    property Rows[idx : Integer] : TAbstractRow read GetRow; default;
    procedure Move(OldIndex, NewIndex : Integer);
    procedure MoveRow(ARow : TAbstractRow; NewIndex : Integer);
    {/** Index of a row in the rows' interface */}
    function IndexOfRow(ARow : TAbstractRow) : Integer;
    {/** Add new row to storage */}
    function PutRow(Row : TDataRow; Action : TPutAction) : TPutResult; override;
    {/** Add a new row to the storage. If autoarrange is false it's added to RowList at position index */}
    function InsertRow(Row : TDataRow; Action : TPutAction; Position : Integer) : TPutResult; virtual;
    {/** Filter for rows to show in Rows[] and RowCount.  */}
    {/** Should the Storage keep the Rows-interface in order constantly */}
    property AutoArrange : Boolean read FAutoArrange write SetAutoArrange;
    {/** This method arranges to Rows in a non-autoarranged Storage */}
    procedure ArrangeRows;
    {/** LAA-ordered procedure for gettings the longest (nr of DisplayBytes) values of the storage */}
    procedure GetLargestValues(ResultRow : TDataRow);
    procedure GetLargestValuesRestrictCount(ResultRow: TDataRow; MaxRowCount: Integer);
    {/** Remove all rows which's aggregable values are all zero. If the table has no aggregable values
         no rows will be removed */}
    procedure RemoveEmptyAggregables;
    {/** Create copy */}
    function Copy : TCustomRowStorage; virtual; abstract;
    {/** Get the first row from this storage */}
    function FirstRow : TDataRow; override;

    function CanCloseRow(ARow : TAbstractRow) : Boolean; virtual;
    function CloseRow(ARow : TAbstractRow) : TSubTotalRow; virtual;
    function CanOpenRow(ARow : TAbstractRow) : Boolean; virtual;
    procedure OpenRow(ARow : TSubTotalRow); virtual;

    property AutoUpdateEnabled;
  end;

  TRowStorage = class(TCustomRowStorage)
  private
    FOpenCriteria : TCondition;
    FKeyCriteria : TCondition;
    FKeysEditable : Boolean;


    FAutoCalcSubTotals : Boolean;

    class function AllowEditKeys(DataTable : TDataTable; Condition : TCondition) : Boolean;
    function CreateKeyCriteria(OpenCriteria : TCondition) : TCondition;
    function ClearNonKeys(Field: TDataField;
      var ConflictAction: TConflictAction;
      var KeepSrc: Boolean): TDataField;
    procedure ResetAll(NewOpenCriteria : TCondition);
  protected
    function GetCanHaveTotals : Boolean; override;
    function GetAutoCalcSubTotals : Boolean; override;
    procedure SetAutoCalcSubTotals(AutoCalc : Boolean); override;



    function GetCommonCriteria : TCondition; override;
  public
    {/** Constructors */}
    constructor Create(DataTable : TDataTable);
    constructor CreateAndLoad(DataTable : TDataTable; OpenCriteria : TCondition; AInterruptable : TInterruptable = nil; AffectedFields : TDataFieldSet = nil);



(*    constructor Create(DataTable : TDataTable; OpenCriteria : TCondition; AInterruptable : TInterruptable; AffectedFields : TDataFieldSet; InitialLoad : Boolean);
    {/** Constructor (TreeKeys : TList of TKeyField) */}
    constructor CreateWithKeySortOrder(DataTable : TDataTable; OpenCriteria : TCondition; AInterruptable : TInterruptable; TreeKeys : TRowSortOrder; InitialLoad : Boolean); overload;
    {/** Constructor (TreeKeysAndOrder : TList of TRowStorageTreeKey) */}
    constructor CreateWithKeySortOrder(DataTable : TDataTable; OpenCriteria : TCondition; AInterruptable : TInterruptable; TreeKeysAndOrder : TList; InitialLoad : Boolean); overload;
    {/** Constructor */}
    constructor CreateFromFile(DataTable : TDataTable; TextFile : String);
    {/** Load a storage from a Polycon RowStorage filr (.prs) */}

    {/** Constructor */}
    constructor CreateFromSQL(DataTable : TDataTable; AInterruptable : TInterruptable;
                              SQL : TStrings; Params : TParamList; TreeKeys : TRowSortOrder = nil); *)
    {/** Constuctor */}
    constructor CreateCopy(Src : TRowStorage); virtual;
    {/** Destructor */}
    destructor Destroy; override;



    {/** Add a new row to the storage. If autoarrange is false it's added to RowList at position index */}
    function InsertRow(Row : TDataRow; Action : TPutAction; Position : Integer) : TPutResult; override;
    {/** Save + Reload from db */}

    {/** Create a copy of this RowStorage and it's contents. All RowStatuses etc are copied */}
    function Copy : TCustomRowStorage; override;
    {/** Criteria for the open budget */}
    property OpenCriteria : TCondition read FOpenCriteria;
    property KeyCriteria : TCondition read FKeyCriteria;
    property KeysEditable : Boolean read FKeysEditable;
  end;





implementation

uses
{$ifndef D4_OR_HIGHER}
  CommonLib, // Min&Max
{$endif D4_OR_HIGHER}
  SysUtils, Math, Criteria, RowList;

type
  TAbstractRowStorageLink = class(TAbstractRowStorage);
  TSubTotalRowLink = class(TSubTotalRow);
  TDataRowLink = class(TDataRow);
  TDataFieldLink = class(TDataField);

// --------------------------TMonitorRowStorage --------------------------------



// ----------------------------- TPreCalcedRowStorage --------------------------



// -------------------------- TRowStorage --------------------------------------

class function TRowStorage.AllowEditKeys(DataTable : TDataTable; Condition : TCondition) : Boolean;
var
  i : Integer;
begin
  Result := True;

  if DataTable.RunningNumberField = nil then
    for i := DataTable.KeyCount to DataTable.FieldCount - 1 do
      if not Condition.AcceptsAllForField(DataTable.Field[i]) then
      begin
        Result := False;
        Break;
      end;
end;

(*function TRowStorage.IllegalKeyValueCheck(SubTotal : TSubTotalRow; AValue : TValue; var PutResult : TPutResult) : Boolean;
begin
  if (Self.KeyCriteria <> nil) and
      not Self.KeyCriteria.AcceptsRowValue(SubTotal.SubTotalKey.TreeKey, AValue, SubTotal) then
  begin
    PutResult := prIllegalKeyValue;
    Result := True;
  end
  else
    Result := False;
end;
*)
function TRowStorage.ClearNonKeys(Field : TDataField; var ConflictAction : TConflictAction;
  var KeepSrc : Boolean) : TDataField;
begin
  KeepSrc := False;
  ConflictAction := caCurrent;
  if not DataTable.TableHasKey(Field) then
    Result := nil
  else
    Result := Field;
end;

function TRowStorage.CreateKeyCriteria(OpenCriteria : TCondition) : TCondition;
(*
  function HasNonKeysConditions(ACond : TCondition; out Field : TDataField) : Boolean;
  var
    FieldSet : TDataFieldSet;
    ifs : TDataFieldSetIterator;
  begin
    Result := False;
    Field := nil;
    FieldSet := TDataFieldSet.Create;
    FieldSet.AddFieldsFromCondition(ACond);
    ifs := TDataFieldSetIterator.Create(FieldSet);
    while not ifs.EOF do
    begin
      if not DataTable.TableHasKey(ifs.Field) then
      begin
        Field := ifs.Field;
        Result := True;
        Break;
      end;
      ifs.Next;
    end;

    ifs.Free;
    FieldSet.Free;
  end;

var
  Field : TDataField;
*)
begin
  Result := OpenCriteria.CreateFieldTranslatedCopy(ClearNonKeys);
  if Result = nil then
    Result := TCriteria.Create;
(*  Result := OpenCriteria.CreateCopy;

  while HasNonKeysConditions(Result, Field) do
    Result := Result.CopyValues(Field, nil, False, False);
  *)
end;

function TRowStorage.GetAutoCalcSubTotals : Boolean;
begin
  Result := FAutoCalcSubTotals;
end;

procedure TRowStorage.SetAutoCalcSubTotals(AutoCalc : Boolean);
begin
  if AutoCalc <> FAutoCalcSubTotals then
  begin
    if (not AutoCalc) and (not TSubTotalRowLink(FTotal).SubTotalsUptodate) then
      TSubTotalRowLink(FTotal).UpdateSubTotals;

    FAutoCalcSubTotals := AutoCalc;
  end;
end;

constructor TRowStorage.Create(DataTable : TDataTable);
begin
  Assert(DataTable <> nil, 'TRowStorage.Create: DataTable <> nil');

  inherited Create( DataTable );

  FAutoCalcSubTotals := False;

  FAutoArrange := True;

  ResetAll(TCriteria.Create);

  {FOpenCriteria := TCriteria.Create;
  FKeyCriteria := TCriteria.Create;
  FKeysEditable := True;}
end;

constructor TRowStorage.CreateAndLoad(DataTable : TDataTable; OpenCriteria : TCondition; AInterruptable : TInterruptable = nil; AffectedFields : TDataFieldSet = nil);
begin
  Create(DataTable);

end;

procedure TRowStorage.ResetAll(NewOpenCriteria : TCondition);
begin
  FOpenCriteria.Free;
  FOpenCriteria := NewOpenCriteria;

  FKeyCriteria.Free;
  FKeyCriteria := CreateKeyCriteria(NewOpenCriteria);
  FKeysEditable := AllowEditKeys(DataTable, NewOpenCriteria);

  TSubTotalRowLink(FTotal).ClearAll;
  ClearAllRowLists;
end;









(*
constructor TRowStorage.Create(DataTable : TDataTable; OpenCriteria : TCondition; AInterruptable : TInterruptable; AffectedFields : TDataFieldSet; InitialLoad : Boolean);
begin
  Assert(DataTable <> nil, 'TRowStorage.Create: DataTable <> nil');

  if OpenCriteria = nil then
    OpenCriteria := TCriteria.Create;

  inherited Create(DataTable, AllowEditKeys(DataTable, OpenCriteria));
  FAutoCalcSubTotals := False;

  FOpenCriteria := OpenCriteria;
  FKeyCriteria := CreateKeyCriteria(OpenCriteria);

  FReloadObject := nil;
  FAutoArrange := True;
  if InitialLoad then
    LoadRows(OpenCriteria, AInterruptable, AffectedFields);
end;

constructor TRowStorage.CreateWithKeySortOrder(DataTable : TDataTable; OpenCriteria : TCondition; AInterruptable : TInterruptable;
                                               TreeKeys : TRowSortOrder; InitialLoad : Boolean);
begin
  Assert(DataTable <> nil, 'TRowStorage.CreateWithKeyOrder: DataTable <> nil');
  Assert(TreeKeys <> nil, 'TRowStorage.CreateWithKeyOrder: TreeKeys <> nil');

  if OpenCriteria = nil then
    OpenCriteria := TCriteria.Create;

  inherited CreateWithKeySortOrder(DataTable, AllowEditKeys(DataTable, OpenCriteria), TreeKeys);

  FAutoCalcSubTotals := False;

  if OpenCriteria = nil then
    OpenCriteria := TCriteria.Create;

  FOpenCriteria := OpenCriteria;
  FKeyCriteria := CreateKeyCriteria(OpenCriteria);

  FReloadObject := nil;
  FAutoArrange := True;

  if InitialLoad then
    LoadRows(OpenCriteria, AInterruptable, nil);
end;

constructor TRowStorage.CreateFromSQL(DataTable : TDataTable; AInterruptable : TInterruptable; SQL : TStrings; Params : TParamList; TreeKeys : TRowSortOrder = nil);
begin
  Assert(DataTable <> nil, 'TRowStorage.CreateFromSQL: DataTable <> nil');
  Assert(SQL <> nil, 'TRowStorage.CreateFromSQL: SQL <> nil');

  if TreeKeys = nil then
    inherited Create(DataTable, True)
  else
    inherited CreateWithKeySortOrder(DataTable, True, TreeKeys);

  FAutoCalcSubTotals := False;

  FOpenCriteria := TCriteria.Create;
  FKeyCriteria := TCriteria.Create;
  FReloadObject := nil;
  FAutoArrange := True;

  LoadRowsSQL(SQL, Params, AInterruptable, nil);
end;

constructor TRowStorage.CreateWithKeySortOrder(DataTable : TDataTable; OpenCriteria : TCondition; AInterruptable : TInterruptable;
                                               TreeKeysAndOrder : TList; InitialLoad : Boolean);
begin
  Assert(DataTable <> nil, 'TRowStorage.CreateWithKeySortOrder: DataTable <> nil');
  Assert(TreeKeysAndOrder <> nil, 'TRowStorage.CreateWithKeySortOrder: TreeKeysAndOrder <> nil');

  inherited CreateKeySortOrder(DataTable, AllowEditKeys(DataTable, OpenCriteria), TreeKeysAndOrder);

  FAutoCalcSubTotals := False;

  if OpenCriteria = nil then
    OpenCriteria := TCriteria.Create;

  FOpenCriteria := OpenCriteria;
  FKeyCriteria := CreateKeyCriteria(OpenCriteria);

  FReloadObject := nil;
  FAutoArrange := True;

  if InitialLoad then
    LoadRows(OpenCriteria, AInterruptable, nil);
end;



constructor TRowStorage.CreateFromFile(DataTable : TDataTable; TextFile : String);
begin
  Assert(DataTable <> nil, 'TRowStorage.CreateFromFile: DataTable <> nil');

  inherited Create(DataTable, True);

  FAutoCalcSubTotals := False;

  FOpenCriteria := TCriteria.Create;
  FKeyCriteria := TCriteria.Create;

  FReloadObject := nil;
  FAutoArrange := True;

  LoadRowsFromFile(TextFile);
end;
*)

destructor TRowStorage.Destroy;
begin
  FOpenCriteria.Free;
  FKeyCriteria.Free;



  inherited Destroy;
end;



function TRowStorage.GetCommonCriteria : TCondition;
begin
  Result := FOpenCriteria;
end;

function TRowStorage.Copy : TCustomRowStorage;
begin
  Result := TRowStorage.CreateCopy(Self);
end;

constructor TRowStorage.CreateCopy(Src : TRowStorage);
begin
  Assert(Src <> nil, 'TRowStorage.CreateCopy: Src <> nil');

  Create(Src.DataTable);
  Self.TreeKeys := Src.TreeKeys;

  FOpenCriteria.Free;
  FOpenCriteria := Src.OpenCriteria.CreateCopy;

  FKeyCriteria.Free;
  FKeyCriteria := Src.KeyCriteria.CreateCopy;

  FKeysEditable := Src.KeysEditable;

  ISubTotalsUnder := Src.ISubTotalsUnder;
  FAutoArrange := Src.FAutoArrange;

  Self.CopyFromSource(Src);




{  Assert(Src <> nil, 'TRowStorage.CreateCopy: Src <> nil');

  CreateWithKeySortOrder(Src.DataTable, Src.OpenCriteria.CreateCopy, nil, Src.CopyOfTreeOrder(Self), False);

  ISubTotalsUnder := Src.ISubTotalsUnder;
  FAutoArrange := Src.FAutoArrange;

  Self.CopyFromSource(Src);

  if Src.FReloadObject <> nil then
    Self.FReloadObject := Src.FReloadObject.CreateCopy; }
end;





function TRowStorage.GetCanHaveTotals : Boolean;
begin
  Result := True;
end;

function TRowStorage.InsertRow(Row : TDataRow; Action : TPutAction; Position : Integer) : TPutResult;
begin
  Assert(Row <> nil, 'TRowStorage.InsertRow: Row <> nil');

  if Self.DataTable <> Row.DataTable then
    Log(ltError, 'Different DataTables', 'Row and RowStorage types differ! (' +
                           Row.DataTable.TableName + ' <> ' + Self.DataTable.TableName + ')');

  if Action <> paInternal then
  begin
    if Row.Status <> rsExternControlled then
      Log(ltWarning, 'Internal', 'Trying to add a row to RowStorage that is already in the Storage!');
  end;

  // Fixa LGE; här ska vi tillåta ifall det finns rader med samma nyckelkombinationer...
  if not Self.KeysEditable then
  begin
    Result := prCannotAdd;
    Exit;
  end;

  if (Action <> paInternal) and
     (KeyCriteria <> nil) and
{$ifdef D4_ORHIGHER}
     (not KeyCriteria.AcceptsRow(Row)) then
{$else}
     (not KeyCriteria.AcceptsRow(Row,nil)) then
{$endif D4_ORHIGHER}
    Result := prIllegalKeyValue
  else
    Result := TSubTotalRowLink(FTotal).InternalPutRow(Row, Action);


  if not (Result in IllegalPutResults) then
  begin
    Changed(kcNewRow, Row, Position);

    if (Action <> paInternal) then
      TDataRowLink(Row).FStatus := rsNew;

    FLastChanged := Now;
  end;
  // Else some error: couldn't add
end;



{ TESTHandler }



{ TEditableSubTotal }



{ TCustomRowStorage }

destructor TCustomRowStorage.Destroy;
begin
  inherited Destroy;
end;

function TCustomRowStorage.FirstRow : TDataRow;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to RowCount - 1 do
    if Rows[i] is TDataRow then
    begin
      Result := TDataRow(Rows[i]);
      Exit;
    end;
end;

function TCustomRowStorage.IndexOfRow(ARow : TAbstractRow) : Integer;
begin
  Result := FRowList.IndexOfObject(ARow);
end;

procedure TCustomRowStorage.ArrangeRows;
begin
  FillRowList;
end;

procedure TCustomRowStorage.Move(OldIndex, NewIndex : Integer);
begin
  if not AutoArrange then
  begin
    if (OldIndex < 0) or
       (NewIndex < 0) then
      raise ERangeError.Create('TRowStorage.Move: Index out of bounds!');

    FRowList.Move(OldIndex, NewIndex);
  end;
end;

procedure TCustomRowStorage.MoveRow(ARow : TAbstractRow; NewIndex : Integer);
begin
  Move(FRowList.IndexOfObject(ARow), NewIndex);
  FLastChanged := Now;
end;

procedure TCustomRowStorage.SetAutoArrange(Arrange : Boolean);
begin
  if (not FAutoArrange) and
     Arrange then
  begin
    ArrangeRows;
  end;

  FAutoArrange := Arrange;
end;

function TCustomRowStorage.PutRow(Row : TDataRow; Action : TPutAction) : TPutResult;
begin
  Assert(Row <> nil, 'TRowStorage.PutRow: Row <> nil');

  Result := InsertRow(Row, Action, -1);
end;

function TCustomRowStorage.InsertRow(Row : TDataRow; Action : TPutAction; Position : Integer) : TPutResult;
begin
  Assert(Row <> nil, 'TRowStorage.InsertRow: Row <> nil');

  if Self.DataTable <> Row.DataTable then
    Log(ltError, 'Different DataTables', 'Row and RowStorage types differ! (' +
                           Row.DataTable.TableName + ' <> ' + Self.DataTable.TableName + ')');

  if (Action <> paInternal) and
     (Row.Status <> rsExternControlled) then
    Log(ltWarning, 'Internal', 'Trying to add a row to RowStorage that is already in the Storage!');

  Result := TSubTotalRowLink(FTotal).InternalPutRow(Row, Action);

  case Result of
    prOk, prKeyOverwrited:
    begin
      Changed(kcNewRow, Row, Position);

      if (Action <> paInternal) then
        TDataRowLink(Row).FStatus := rsNew;
    end;
  end;

  FLastChanged := Now;
end;

function TCustomRowStorage.GetCommonCriteria : TCondition;
begin
  Result := nil;
end;

procedure TCustomRowStorage.FillRowList;
var
  i : Integer;
  AbsoluteMonitorCriteria : TCondition;
begin
  // Fixa LGE ej snyggt!

    AbsoluteMonitorCriteria := nil;

  FRowList.Clear;
  Self.BeforeGetDataByCrit(AbsoluteMonitorCriteria);

  for i := 0 to UnacceptedRowCount - 1 do
    FRowList.AddObject('', UnacceptedRows[i]);

  if Self.UsesCustomSortOrder then
  begin
    TSubTotalRowLink(Total).FillRowList(FRowList, AbsoluteMonitorCriteria, False);
    Self.CustomSortOrder.OrderRows(Self.DataTable, FRowList);
  end
  else
    TSubTotalRowLink(Total).FillRowList(FRowList, AbsoluteMonitorCriteria, {DataTable.} CanDefaultSort);

  FRowListUpToDate := True;
end;

function TCustomRowStorage.GetRow(idx : Integer) : TAbstractRow;
begin
  Assert((idx >= 0) and (idx < Self.RowCount),
         'TRowStorage.GetRow: (idx >= 0) and (idx < FRowList.Count), idx: ' + IntToStr(idx) +
         ', FRowList.Count: ' + IntToStr(FRowList.Count));

  if not FRowListUpToDate then
    FillRowList;

  Result := TAbstractRow(FRowList.Objects[idx]);
end;

procedure TCustomRowStorage.GetLargestValues(ResultRow : TDataRow);
begin
  GetLargestValuesRestrictCount( ResultRow, RowCount - 1 )
end;

procedure TCustomRowStorage.GetLargestValuesRestrictCount(ResultRow : TDataRow; MaxRowCount : Integer);
var
  i : Integer;
begin
  if ResultRow.DataTable <> Self.DataTable then
    Log(ltError, 'LargestValues', 'TRowStorage.GetLargestValues: DataTables differ');

  ResultRow.CopyContents(DataTable.IDefaultRow);
  for i := 0 to Min( RowCount - 1, MaxRowCount ) do
    // MVJ 29.08.1999 Pga blanka nycklar kan felakiga värden hamna i ResultRow. Kopierar därför då i = 0
    if (i = 0) and (Rows[i] is TDataRow) then
      ResultRow.CopyContents(TDataRow(Rows[i]))
    else
      MapLargestValues(ResultRow, Rows[i]);
end;

procedure TCustomRowStorage.RemoveEmptyAggregables;
var
  Criteria : TCriteria;
  List : TDataRowList;
  i : Integer;
begin
  Criteria := TCriteria.Create;

  for i := DataTable.KeyCount to DataTable.FieldCount - 1 do
    if DataTable.Field[i].IsAggregable then
      Criteria[DataTable.Field[i]].AddValue(ValueFromInteger(0));

  if Criteria.FieldCount > 0 then
  begin
    List := TDataRowList.Create;
    Self.GetRows(List, Criteria, gaReference);
    for i := 0 to List.Count - 1 do
      List.DataRows[i].Delete;
    List.Free;
  end;

  Criteria.Free;
end;

function TCustomRowStorage.GetRowCount : Integer;
begin
  if not FRowListUpToDate then
    FillRowList;

  Result := FRowList.Count;
end;

function TCustomRowStorage.CanCloseRow(ARow : TAbstractRow) : Boolean;
begin
  Result := (ARow <> nil) and (ARow.Storage = Self) and ARow.IsClosable;
end;

function TCustomRowStorage.CloseRow(ARow : TAbstractRow) : TSubTotalRow;
var
  ParentTotal : TSubTotalRow;
begin
  if CanCloseRow(ARow) then
  begin
    ParentTotal := ARow.SubTotalRow;
    ParentTotal.Visible := True;
    TSubTotalRowLink(ParentTotal).SetChildVisibility(False, True);
    Result := ParentTotal;
    FLastChanged := Now;
  end
  else
    Result := nil;
end;

function TCustomRowStorage.CanOpenRow(ARow : TAbstractRow) : Boolean;
begin
  Result := (ARow.Storage = Self) and
            (ARow is TSubTotalRow);
end;

procedure TCustomRowStorage.OpenRow(ARow : TSubTotalRow);
begin
  if CanOpenRow(ARow) then
  begin
    ARow.Visible := False;
    TSubTotalRowLink(ARow).SetChildVisibility(True, False);
    FLastChanged := Now;
  end;
end;

end.

