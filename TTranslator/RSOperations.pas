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

{ $Id: RSOperations.pas,v 1.151 2002/12/27 15:07:32 laa Exp $}

unit RSOperations;

interface

uses
  RowList, Classes,
  CalcField, Interruptable, Storages, DataElements, DataTypes, DataType;

type
  TRSOperation = class;
  TRowEvent = procedure(Sender : TRSOperation; Row : TDataRow) of object;


  TListSource = class(TRowSource)
  private
    FCurrentRow : Integer;
    FList : TStrings;
  protected
    procedure StartOperations; override;
    function GetNextRow : TDataRow; override;
    function GetSortOrder : TRowSortOrder; override;
    function GetIsAbsoluteSource : Boolean; override;
  public
    constructor Create(List : TStrings; DataTable : TDataTable);
    destructor Destroy; override;
  end;

  TRSOperation = class(TRowSource)
  private
    FOwnedObjects : TList;
    FOwnedRows : TList;
    fIdentifier: string;
    procedure SetPreviousOperationsInterruptable(Interruptable : TInterruptable);
  protected
    FPrimarySource : TRowSource;

    constructor Create(PrimarySource : TRowSource; DataTable : TDataTable);
    procedure AddOwnedObject(AObject : TObject);
    procedure AddOwnedRow(ARow : TDataRow);
    procedure ReadAll(Source : TRowSource; List : TDataRowList);
    procedure GotRow(Row : TDataRow);
    function GetIsAbsoluteSource : Boolean; override;
    function GetSourceCount : Integer; virtual;
    function GetSource(idx : Integer) : TRowSource; virtual;
    procedure FreeRowMemory; override;
  public
    destructor Destroy; override;

    procedure LogOperationResults(Log : TStrings);
    function AddToLog(Components, Log : TStrings; Indentation : Integer) : Boolean; override;

    property SourceCount : Integer read GetSourceCount;
    property Source[idx : Integer] : TRowSource read GetSource;

    property PreviousOperationsInterruptable : TInterruptable write SetPreviousOperationsInterruptable;
    property Identifier : string read fIdentifier write fIdentifier;
  end;



  TLookupItem = class
  private
    FDestFieldList : TList;
    FSrcFieldList : TList;
    FAuxTable : TAuxTable;
  public
    constructor Create(DestField, SrcField : TDataField; Auxtable : TAuxTable);
    destructor Destroy; override;
    procedure AddField(DestField, SrcField : TDataField);
    function LookupsField(Field : TDataField) : Boolean;
    property Table : TAuxTable read FAuxTable;
  end;

  {Innefattar lägg till spalt, ta veck spalt, döp om, omräkna värde, group}
  TRSRearrange = class(TRSOperation)
  private
    FPutAction : TPutAction;
    FDestFields, FSrcFields : TList;
    FLookupList : TList;
    FInStorage : TRowStorage;
    FCurrentRow : Integer;
    FOperationsStarted : Boolean;
    FGroup : Boolean;
    FOnOutputRow : TRowEvent;

//    FToExistingStorage : Boolean;
    function ReadRow : TDataRow;
    procedure PrepareConversions;
    function KeysUnchanged : Boolean;
    function HasLookupOnField(AField : TDataField) : Boolean;
  protected
    procedure StartOperations; override;
    function GetNextRow : TDataRow; override;
    procedure NewRowBeforeAccept(NewRow, OldRow : TDataRow); virtual;
    function GetSortOrder : TRowSortOrder; override;
    procedure FreeRowMemory; override;
  public
    constructor Create(Source : TRowSource);
    constructor CreateWithTable(Source : TRowSource; Table : TDataTable);
//    constructor CreateOutputToStorage(Source : TRowSource; Storage : TRowStorage; PutAction : TPutAction);
    destructor Destroy; override;
    property PutAction : TPutAction read FPutAction write FPutAction;
    property Group : Boolean read FGroup write FGroup;
    procedure AddConversion(DestField : TDataField; SrcField : TDataField);
    procedure AddConstantConversion(DestField : TDataField; Constant : TValue);
    procedure LookupField(DestField, SrcField : TDataField; AuxTable : TAuxTable);
    procedure ResetField(Field : TDataField);
    procedure GetAllInput;
    function RowCount : Integer;
    property OnOutputRow : TRowEvent read FOnOutputRow write FOnOutputRow;
  end;

  TRSFilter = class(TRSOperation)
  private
    FCrit : TCondition;
    SourceRows : TDataRowList;
    SourceRowPos : Integer;
    function SourceIsStorage : Boolean;
    // function GetCriteria : TCriteria;
  protected
    procedure StartOperations; override;
    function GetNextRow : TDataRow; override;
    function GetSortOrder : TRowSortOrder; override;
  public
    constructor Create(Source : TRowSource);
    constructor CreateWithCondition(Source : TRowSource; Condition : TCondition);
    constructor CreateWithBoolean(Source : TRowSource; BooleanField : TDataField; AcceptanceValue : Boolean);
//    constructor CreateWithCriteriaField(Source : TRowSource; CriteriaField : TCriteriaField);
    destructor Destroy; override;
    property Condition : TCondition read FCrit;
  end;



  procedure StartLog;
  procedure EndLog(const FileName : String);

implementation

uses
  SysUtils, Math,
  Criteria, CommonLib, CommonCalcFields;

var
  LogList : TStringList;
  LogStart : TDateTime;



procedure StartLog;
begin
  LogList := TStringList.Create;
  LogStart := Now;
end;

procedure EndLog(const FileName : String);
begin
  LogList.SaveToFile(FileName);
  LogList.Free;
  LogList := nil;
end;

procedure LogMsg(Msg : String);
var
  ms : Integer;
begin
  try
    if LogList <> nil then
    begin
      ms := Round((Now - LogStart) * 24.0 * 60.0 * 60.0 * 1000.0);
      LogList.Add(IntToStr(ms) + ' ms: ' + Msg);
    end;
  except

  end;
end;

// ------------------------------- TAggregablesZeroField -----------------------



// --------------------------------- TListSource -------------------------------

procedure TListSource.StartOperations;
begin
  FCurrentRow := 0;
end;

function TListSource.GetNextRow : TDataRow;
var
  AObject : TObject;
begin
  Result := nil;

  while (Result = nil) and
        (FCurrentRow < FList.Count) do
  begin
    AObject := FList.Objects[FCurrentRow];
    if (AObject is TDataRow) and
       (TDataRow(AObject).DataTable = Self.DataTable) then
      Result := TDataRow(AObject);

    Inc(FCurrentRow);
  end;
end;

function TListSource.GetSortOrder : TRowSortOrder;
begin
  Result := nil;
end;

constructor TListSource.Create(List : TStrings; DataTable : TDataTable);
begin
  inherited Create(DataTable, otCalcOperation);
  FList := List;
end;

destructor TListSource.Destroy;
begin
  inherited Destroy;
end;

function TListSource.GetIsAbsoluteSource : Boolean;
begin
  Result := True;
end;


// -------------------------------- TRSUnacross --------------------------------



// -------------------------- TRSOperation -------------------------------------

function TRSOperation.GetIsAbsoluteSource : Boolean;
begin
  Result := False;
end;

constructor TRSOperation.Create(PrimarySource : TRowSource; DataTable : TDataTable);
begin
  inherited Create(DataTable, otCalcOperation);

  if PrimarySource <> nil then
    Interruptable := PrimarySource.Interruptable;
  FPrimarySource := PrimarySource;
  fIdentifier := '';
end;

destructor TRSOperation.Destroy;
var
  i : Integer;
begin
  FreeRowMemory;
  FOwnedRows.Free;

  if FOwnedObjects <> nil then
  begin
    for i := 0 to FOwnedObjects.Count - 1 do
      TObject(FOwnedObjects.Items[i]).Free;

    FOwnedObjects.Free;
  end;

  inherited Destroy;
end;

procedure TRSOperation.FreeRowMemory;
var
  i : Integer;
begin
  inherited FreeRowMemory;

  if FOwnedRows <> nil then
  begin
    for i := 0 to FOwnedRows.Count - 1 do
      TObject(FOwnedRows.Items[i]).Free;
    FOwnedRows.Clear;
  end;
end;

procedure TRSOperation.SetPreviousOperationsInterruptable(Interruptable : TInterruptable);
var
  i : Integer;
begin
  for i := 0 to SourceCount - 1 do
  begin
    Source[i].Interruptable := Interruptable;

    if Source[i] is TRSOperation then
      TRSOperation(Source[i]).SetPreviousOperationsInterruptable(Interruptable);
  end;
end;

procedure TRSOperation.GotRow(Row : TDataRow);
{$ifdef DEBUG}
const
  SPACESIZE = 1;
var
  Line : String;
  Size, j : Integer;
{$endif DEBUG}
begin
{$ifdef DEBUG}
  if Row <> nil then
  begin
    Line := '';
    for j := 0 to DataTable.FieldCount - 1 do
    begin
      Size := DataTable.Field[j].DataType.DisplayWidth;
      Line := Line + Copy(AsString(Row.ValueByIndex[j]) + StringOfChar(' ', Size), 1, Size) + StringOfChar(' ', SPACESIZE);
    end;
    LogMsg(Self.ClassName + ', ' + Row.DataTable.TableName + ': ' + Line);
  end;
{$endif DEBUG}
end;

procedure TRSOperation.AddOwnedObject(AObject : TObject);
begin
  if FOwnedObjects = nil then
    FOwnedObjects := TList.Create;

  FOwnedObjects.Add(AObject);
end;

procedure TRSOperation.AddOwnedRow(ARow : TDataRow);
begin
  if FOwnedRows = nil then
    FOwnedRows := TList.Create;

  FOwnedRows.Add(ARow);
end;

procedure TRSOperation.ReadAll(Source : TRowSource; List : TDataRowList);
var
  InRow : TDataRow;
begin
  while true do
  begin
    InRow := DoGetNextRow(Source);
    if InRow <> nil then
      List.AddObject('', InRow)
    else
      Exit;
  end;
end;

procedure TRSOperation.LogOperationResults(Log : TStrings);
var
  Components : TStringList;
begin
  Components := TStringList.Create;
  Self.AddToLog(Components, Log, 0);

  Components.Free;
end;

function TRSOperation.AddToLog(Components, Log : TStrings; Indentation : Integer) : Boolean;
var
  i : Integer;
begin
  Result := inherited AddToLog(Components, Log, Indentation);
  if Result then
    for i := 0 to SourceCount - 1 do
      Source[i].AddToLog(Components, Log, Indentation + 1);
end;

function TRSOperation.GetSourceCount : Integer;
begin
  Result := 1;
end;

function TRSOperation.GetSource(idx : Integer) : TRowSource;
begin
  Result := FPrimarySource;
end;

// ----------------------------- TRSMultiOperation -----------------------------



// --------------------------- TLookupItem -------------------------------------

constructor TLookupItem.Create(DestField, SrcField : TDataField; Auxtable : TAuxTable);
begin
  if AuxTable = nil then
    raise Exception.Create('TRSRearrange: Calc definition error: Will not be able to lookup field ' + SrcField.FieldName);

  FSrcFieldList := TList.Create;
  FDestFieldList := TList.Create;
  FAuxTable := AuxTable;
  AddField(DestField, SrcField);
end;

destructor TLookupItem.Destroy;
begin
  FSrcFieldList.Free;
  FDestFieldList.Free;
end;

procedure TLookupItem.AddField(DestField, SrcField : TDataField);
begin
  FSrcFieldList.Add(SrcField);
  FDestFieldList.Add(DestField);
end;

function TLookupItem.LookupsField(Field : TDataField) : Boolean;
var
  i : Integer;
begin
  Result := False;

// Här var jagLAA och ändrade SrcFieldList till DestFieldList
{  for i := 0 to FSrcFieldList.Count - 1 do
    if FSrcFieldList.Items[i] = Field then
    begin
      Result := True;
      Exit;
    end; }
  for i := 0 to FDestFieldList.Count - 1 do
    if FDestFieldList.Items[i] = Field then
    begin
      Result := True;
      Exit;
    end;
end;

// --------------------------- TRSRearrange ----------------------------------------

constructor TRSRearrange.Create(Source : TRowSource);
var
  ADynaTable : TDataTable;
begin
  ADynaTable := TDataTable.CreateCopy(Source.DataTable, nil, nil);
  inherited Create(Source, ADynaTable);
  AddOwnedObject(ADynaTable);

  FOperationsStarted := False;
  // FGroup := False;
  FGroup := True;

  FDestFields := TList.Create;
  FSrcFields := TList.Create;
  FPutAction := paAddReplaceBlank;
  FLookupList := TList.Create;

//  FToExistingStorage := False;
  FInStorage := nil;
end;

constructor TRSRearrange.CreateWithTable(Source : TRowSource; Table : TDataTable);
begin
  inherited Create(Source, Table);

  FOperationsStarted := False;
  // FGroup := False;
  FGroup := True;

  FDestFields := TList.Create;
  FSrcFields := TList.Create;
  FPutAction := paAddReplaceBlank;
  FLookupList := TList.Create;

//  FToExistingStorage := False;
  FInStorage := nil;
end;

(*
constructor TRSRearrange.CreateOutputToStorage(Source : TRowSource; Storage : TRowStorage; PutAction : TPutAction);
begin
  inherited Create(Source, Storage.DataTable);

  FOperationsStarted := False;
  FGroup := True;
  FDestFields := TList.Create;
  FSrcFields := TList.Create;
  FPutAction := PutAction;
  FLookupList := TList.Create;

  FToExistingStorage := True;
  FInStorage := Storage;

  StartOperations;
end;
*)

destructor TRSRearrange.Destroy;
var
  i : Integer;
begin
  FreeRowMemory;

  FDestFields.Free;
  FSrcFields.Free;

  for i := 0 to FLookupList.Count - 1 do
    TObject(FLookupList.Items[i]).Free;
  FLookupList.Free;

  inherited Destroy;
end;

procedure TRSRearrange.FreeRowMemory;
begin
  inherited FreeRowMemory;

(*  if not FToExistingStorage then
  begin *)
//    if FInStorage <> nil then
//      FInStorage.SaveToFile('C:\temp\' + FInStorage.DataTable.TableName + FloatToStr(Now) + '.txt');
    FInStorage.Free;
    FInStorage := nil;
(*  end; *)
end;

procedure TRSRearrange.AddConversion(DestField : TDataField; SrcField : TDataField);
begin
  FDestFields.Add(DestField);
  FSrcFields.Add(SrcField);
end;

procedure TRSRearrange.AddConstantConversion(DestField : TDataField; Constant : TValue);
var
  ConstantField : TCalcField;
begin
  ConstantField := TConstantField.CreateNoName(Constant);
  AddOwnedObject(ConstantField);

  AddConversion(DestField, ConstantField);
end;

procedure TRSRearrange.ResetField(Field : TDataField);
begin
  AddConstantConversion(Field, Field.DataType.DefaultValue);
end;

procedure TRSRearrange.LookupField(DestField, SrcField : TDataField; AuxTable : TAuxTable);
var
  i : Integer;
begin
  for i := 0 to FLookupList.Count - 1 do
    if TLookupItem(FLookupList.Items[i]).Table = AuxTable then
    begin
      TLookupItem(FLookupList.Items[i]).AddField(DestField, SrcField);
      Exit;
    end;

  FLookupList.Add(TLookupItem.Create(DestField, SrcField, AuxTable));
end;

function TRSRearrange.HasLookupOnField(AField : TDataField) : Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 0 to FLookupList.Count - 1 do
    if TLookupItem(FLookupList.Items[i]).LookupsField(AField) then
    begin
      Result := True;
      Exit;
    end;
end;

// Fixa LGE: Detta skulle säkert kunna optimeras med en lista på index istf. fält
procedure TRSRearrange.PrepareConversions;
var
  i, idx : Integer;
begin
  for i := 0 to DataTable.FieldCount - 1 do
  begin
    if HasLookupOnField(DataTable.Field[i]) and
       (FDestFields.IndexOf(DataTable.Field[i]) = -1) then
      AddConversion(DataTable.Field[i], nil);

    idx := FDestFields.IndexOf(DataTable.Field[i]);
    if idx = -1 then
    begin
      if (not (DataTable.Field[i] is TCalcField)) and
         (not FPrimarySource.DataTable.TableHasField(DataTable.Field[i])) then
      begin
        LookupField(DataTable.Field[i], DataTable.Field[i], DataTable.Field[i].LookupTable);
        FSrcFields.Insert(i, nil);
      end
      else
      begin
        FSrcFields.Insert(i, DataTable.Field[i]);
      end;

      FDestFields.Insert(i, DataTable.Field[i]);
    end
    else
    begin
      FDestFields.Move(idx, i);
      FSrcFields.Move(idx, i);

      if (FSrcFields.Items[i] <> nil) and
         (not (TDataField(FSrcFields.Items[i]) is TCalcField)) and
         (not FPrimarySource.DataTable.TableHasField(TDataField(FSrcFields.Items[i]))) then
      begin
        LookupField(TDataField(FDestFields.Items[i]),
                    TDataField(FSrcFields.Items[i]),
                    TDataField(FSrcFields.Items[i]).LookupTable);
        FSrcFields.Items[i] := nil;
      end;
    end;
  end;
end;

function TRSRearrange.KeysUnchanged : Boolean;
var
  i : Integer;
begin
  Result := (DataTable.KeyCount = FPrimarySource.DataTable.KeyCount);
  if Result then
  begin
    for i := 0 to DataTable.KeyCount - 1 do
      if (DataTable.Field[i] <> FPrimarySource.DataTable.Field[i]) or
         (DataTable.Field[i] <> FDestFields.Items[i]) then
      begin
        Result := False;
        Exit;
      end;
  end;
end;

function TRSRearrange.ReadRow : TDataRow;
var
  InRow, RSRow : TDataRow;
  i : Integer;
  Field : TDataField;
begin
  Result := nil;

  InRow := DoGetNextRow(FPrimarySource);
  if InRow = nil then
    Exit;

  RSRow := TDataRow.Create(DataTable);

  for i := 0 to DataTable.FieldCount - 1 do
  begin
    Field := TDataField(FSrcFields.Items[i]);

    if Field <> nil then
      RSRow.ValueByIndex[i] := InRow[Field]; // Fixa LGE: Optimera; index? GetExternValue?
  end;

  NewRowBeforeAccept(RSRow, InRow);

  if FGroup then
    FInStorage.PutRow(RSRow, FPutAction);

  Result := RSRow;
end;

procedure TRSRearrange.StartOperations;
var
  ResultRow : TDataRow;
begin
  if FOperationsStarted then
  begin
    FOperationsStarted := False;
    Exit;
  end;

  LogMsg('*** Rearrange');

  if FInStorage = nil then
  begin
    DoStartOperations(FPrimarySource);

    if FGroup (* and (not FToExistingStorage) *) then
    begin
      FInStorage := TRowStorage.Create(DataTable); // , nil, nil, nil, false);
      FInStorage.ShowSubTotals := False;
    end;

    PrepareConversions;

    if FGroup then
    begin
      LogMsg('Rearrange (Group) start');
      repeat
        ResultRow := ReadRow;
      until (ResultRow = nil) or Interruptable.Interrupted;
      LogMsg('Rearrange (Group) done');
    end;
  end;

  FCurrentRow := 0;
end;

function TRSRearrange.RowCount : Integer;
begin
  if FInStorage = nil then
    Result := 0
  else
    Result := FInStorage.RowCount;
end;

procedure TRSRearrange.GetAllInput;
begin
  if (not FGroup) (* or FToExistingStorage *) then
  begin
    raise Exception.Create('TRSRearrange.GetAllInput: This command is only available when grouping!');
  end;

  StartOperations;
  FOperationsStarted := True;
end;

function TRSRearrange.GetNextRow : TDataRow;
begin
  if FGroup then
  begin
    if FCurrentRow < FInStorage.RowCount then
    begin
      Result := TDataRow(FInStorage.Rows[FCurrentRow]);
      Inc(FCurrentRow);
    end
    else
    begin
      Result := nil;
      DoFinishOperations(FPrimarySource);
      LogMsg('--- Rearrange');
    end;
  end
  else
  begin
    Result := ReadRow;
    if Result <> nil then
      AddOwnedRow(Result)
    else
    begin
      LogMsg('--- Rearrange');
      DoFinishOperations(FPrimarySource);
    end;
  end;

  if (Result <> nil) and Assigned(OnOutputRow) then
    OnOutputRow(Self, Result);

  Self.GotRow(Result);
end;

// FIXA LGE: Denna funkar inte rikit bra längre!!!
procedure TRSRearrange.NewRowBeforeAccept(NewRow, OldRow : TDataRow);
var
  i, j : Integer;
  LookupRow : TDataRow;
  tmpCrit : TCriteria;
  Fuska : Boolean;
  LookupItem : TLookupItem;
  SrcField, DestField : TDataField;
begin
  for i := 0 to FLookupList.Count - 1 do
  begin
    LookupItem := TLookupItem( FLookupList.Items[i] );
    Fuska := True;

    for j := 0 to LookupItem.FDestFieldList.Count - 1 do
      if (TDataField(LookupItem.FDestFieldList.Items[j]) <>
          TDataField(LookupItem.FSrcFieldList.Items[j])) or
          not OldRow.FieldHasValue(TDataField(LookupItem.FSrcFieldList.Items[j])) then
      begin
        Fuska := False;
        Break;
      end;

    if Fuska then
      for j := 0 to LookupItem.FDestFieldList.Count - 1 do
        NewRow[TDataField(LookupItem.FDestFieldList.Items[j])] :=
             OldRow[TDataField(LookupItem.FSrcFieldList.Items[j])]
    else
    begin
  //    LookupRow := TLookupItem(FLookupList.Items[i]).Table.Cache.LocateByRowValues(OldRow, [nil]);
      tmpCrit := TCriteria.CreateFromRowFields(OldRow);
      try
        LookupRow := LookupItem.Table.Cache.LocateRowByCriteria(tmpCrit);
      finally
        tmpCrit.Free;
      end;

      if LookupRow <> nil then
      begin
        for j := 0 to LookupItem.FSrcFieldList.Count - 1 do
        begin
          DestField := TDataField(LookupItem.FDestFieldList.Items[j]);
          SrcField := TDataField(LookupItem.FSrcFieldList.Items[j]);
          NewRow[DestField] := LookupRow[SrcField];
        end;
      end
      else
      begin

        DataElements.Log(ltWarning, 'Lookup', 'Rearrange.LookupRow: Couldn''t find a matching value for field ' +
                            TDataField( LookupItem.FSrcFieldList.Items[0]).FieldName +
                            ' from ' + LookupItem.Table.TableName + '. ' +
                            'Used lookupvalues [' + OldRow.AllFieldValues(LookupItem.Table) + '].');
      end;
    end;
  end;
end;

function TRSRearrange.GetSortOrder : TRowSortOrder;
begin
  if FGroup and (FInStorage <> nil) then
    Result := FInStorage.RowOrder
  else if KeysUnchanged then
    Result := FPrimarySource.RowOrder
  else
    Result := nil;
end;

// ---------------------- TRSFilter --------------------------------------------

constructor TRSFilter.Create(Source : TRowSource);
begin
  inherited Create(Source, Source.DataTable);
  FCrit := TCriteria.Create;
  AddOwnedObject(FCrit);
  SourceRows := nil;
end;

constructor TRSFilter.CreateWithCondition(Source : TRowSource; Condition : TCondition);
begin
  inherited Create(Source, Source.DataTable);
  FCrit := Condition;
  SourceRows := nil;
end;

constructor TRSFilter.CreateWithBoolean(Source : TRowSource; BooleanField : TDataField; AcceptanceValue : Boolean);
var
  CriteriaField : TCriteriaField;
begin
  CriteriaField := TCriteriaField.Create(BooleanField);
  CriteriaField.AddValue(ValueFromBoolean(AcceptanceValue));

  CreateWithCondition(Source, CriteriaField);
  AddOwnedObject(CriteriaField);
end;

{
constructor TRSFilter.CreateWithCriteriaField(Source : TRowSource; CriteriaField : TCriteriaField);
begin
  inherited Create(Source, Source.DataTable);
  FCrit := CriteriaField;
  SourceRows := nil;
end;
 }
destructor TRSFilter.Destroy;
begin
  inherited Destroy;
end;

{
function TRSFilter.GetCriteria : TCriteria;
begin
  if FCrit is TCriteria then
    Result := TCriteria(FCrit)
  else
    Result := nil;
end;
}

procedure TRSFilter.StartOperations;
begin
  LogMsg('*** Filter');

  if SourceIsStorage then
  begin
    LogMsg('Filter GetRows start');
    SourceRows := TDataRowList.Create;
    TAbstractRowStorage(FPrimarySource).GetRows(SourceRows, FCrit, gaReference);
    SourceRowPos := 0;
    LogMsg('Filter GetRows end');
  end
  else
  begin
    DoStartOperations(FPrimarySource);
  end;
end;

function TRSFilter.GetNextRow : TDataRow;
var
  Row : TDataRow;
begin
  Result := nil;

  if SourceIsStorage then
  begin
    if SourceRowPos < SourceRows.Count then
    begin
      Result := SourceRows[SourceRowPos];
      Inc(SourceRowPos);
    end
    else
    begin
      SourceRows.Free;
      SourceRows := nil;
      LogMsg('--- Filter');
    end;
  end
  else
  begin
    while true do
    begin
      Row := Self.DoGetNextRow(FPrimarySource);
      if Row = nil then
      begin
        LogMsg('--- Filter');
        Break;
      end
      else if FCrit.AcceptsRow(Row) then
      begin
        Result := Row;
        Break;
      end;
    end;
  end;

  Self.GotRow(Result);
end;

function TRSFilter.SourceIsStorage : Boolean;
begin
  Result := False; // LGE 160501: Ta bort pervobeteende... // FPrimarySource is TAbstractRowStorage;
end;

function TRSFilter.GetSortOrder : TRowSortOrder;
begin
  Result := FPrimarySource.RowOrder;
end;

// ---------------------------- TRSAppend --------------------------------------






end.


