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

{ $Id: DataClipboard.pas,v 1.25 2002/12/30 09:32:05 laa Exp $ }

unit DataClipboard;

interface

uses

{$ifndef LINUX}
  Windows,
{$else LINUX}
{$endif LINUX}
  Classes, DataElements, RowList, ClipboardInterface;

type
  TDataClipboard = class; // forward

  TClipboardAddable = class
  private
    FOwner : TDataClipboard;
    FSource : TObject;
    FDescription : String;
  protected
    property Owner : TDataClipboard read FOwner;
    procedure InsertToClipboard(AClipboard : TDataClipboard; Position : Integer); virtual; abstract;
  public
    property Source : TObject read FSource;
    property Description : String read FDescription write FDescription;
    constructor Create(Source : TObject);
    destructor Destroy; override;
  end;

  TClipboardEntry = class(TClipboardAddable)
  private
    FDataTable : TDataTable;
    FDataRows : TDataRowList;
    FUISelectedFields, FActualSelectedFields : TList;
    FOneRowSelected : Boolean;
    function GetIndex : Integer;
    procedure SetIndex(NewIndex : Integer);
    function GetIndexByTable : Integer;
    procedure SetIndexByTable(NewIndex : Integer);
    procedure OnInsertCalculations;
  protected
    procedure InsertToClipboard(AClipboard : TDataClipboard; Position : Integer); override;
    procedure AddToStream(AStream : TMemoryStream);
    function CreateCopyOfListAndRows(AList : TDataRowList) : TDataRowList;
    constructor CreateFromClipboardData(Data : Pointer);
    constructor CreateCopy(Source : TClipboardEntry);
  public
    constructor Create(Source : TObject; DataTable : TDataTable; AddAllFields : Boolean); virtual;
    destructor Destroy; override;
    procedure Delete;
    property DataTable : TDataTable read FDataTable;
    property DataRows : TDataRowList read FDataRows;
    property ActualSelectedFields : TList read FActualSelectedFields;
    property UISelectedFields : TList read FUISelectedFields;
    property Index : Integer read GetIndex write SetIndex;
    property IndexByTable : Integer read GetIndexByTable write SetIndexByTable;
    property OneRowSelected : Boolean read FOneRowSelected;
    procedure PasteKeyConvert(ResultRows : TStrings; ConvertCriteria : TCondition);
    procedure PasteKeyConvertFieldMove(ResultRows : TStrings; ConvertCriteria : TCondition; FromFields, ToFields : TList);
    function PasteOneRow(ConvertCriteria : TCondition) : TDataRow;

    procedure AddToClipbrdInterface; virtual;
  end;

  TClipboardEntryClass = class of TClipboardEntry;

  TClipboardEntryList = class(TClipboardAddable)
  private
    FEntryList : TList;
    FAddAllFields : Boolean;
    FEntryClass : TClipboardEntryClass;
    function GetCount : Integer;
    function GetEntry(DataTable : TDataTable) : TClipboardEntry;
    function GetEntryByIndex(idx : Integer) : TClipboardEntry;
  protected
    procedure InsertToClipboard(AClipboard : TDataClipboard; Position : Integer); override;
  public
    constructor Create(Source : TObject; AddAllFields : Boolean);
    destructor Destroy; override;
    property Count : Integer read GetCount;
    property EntryByIndex[idx : Integer] : TClipboardEntry read GetEntryByIndex;
    property Entry[DataTable : TDataTable] : TClipboardEntry read GetEntry; default;
    property EntryClass : TClipboardEntryClass read FEntryClass write FEntryClass;
  end;

  TDataClipboard = class
  private
    FEntries : TList;
    FEmptyOnNewEntry : Boolean;
    FAllowTableDuplicates : Boolean;

    function GetTopEntry : TClipboardEntry;
    function GetEntryCount : Integer;
    function GetEntry(idx : Integer) : TClipboardEntry;
    function GetTableEntryCount(Table : TDataTable) : Integer;
    function GetTableEntry(Table : TDataTable; idx : Integer) : TClipboardEntry;
    procedure EntryDestroyed(Entry : TClipboardEntry);
    function IndexOfEntry(Entry : TClipboardEntry) : Integer;
    procedure InsertToClipboard(Entry : TClipboardAddable; Index : Integer);
    function GetIsEmpty : Boolean;
  protected
    procedure DoInsertEntry(Entry : TClipboardEntry; Position : Integer);
  public
    constructor Create(EmptyOnNewEntry, AllowTableDuplicates : Boolean);
    destructor Destroy; override;

    procedure AddToClipboard(Entry : TClipboardAddable);
    procedure DeleteAllTableEntries(DataTable : TDataTable);
    procedure EmptyClipboard;

    procedure MoveEntry(Entry : TClipboardEntry; NewIndex : Integer);
    procedure MoveIndex(OldIndex, NewIndex : Integer);

    property TopEntry : TClipboardEntry read GetTopEntry;
    property EntryCount : Integer read GetEntryCount;
    property Entry[idx : Integer] : TClipboardEntry read GetEntry;
    property TableEntryCount[Table : TDataTable] : Integer read GetTableEntryCount;
    property TableEntry[Table : TDataTable; idx : Integer] : TClipboardEntry read GetTableEntry;

    property EmptyOnNewEntry : Boolean read FEmptyOnNewEntry;
    property AllowTableDuplicates : Boolean read FAllowTableDuplicates;
    property IsEmpty : Boolean read GetIsEmpty;
  end;

  TEntryFormatObject = class(TStreamFormatObject)
  private
    FEntry : TClipboardEntry;
  public
    constructor Create(AEntry : TClipboardEntry);
    destructor Destroy; override;

    class function GetFormat : UINT;
    class function CreateEntryFromClipboard(AFormat : UINT) : TClipboardEntry;
    procedure SetData(RenderNow : Boolean); override;
  end;

var
  EntryFormatObjectFormat : UINT;
  KeyValueFormat : UINT;

implementation

uses
{$ifndef LINUX}
  Forms,
{$else LINUX}
  QForms,
{$endif LINUX}
  SysUtils,
  DataType, CommonLib, RSOperations, DataStreamer;

const
  KEY_NULL : char = #0;


// ---------------------------- TClipboardAddable ------------------------------

constructor TClipboardAddable.Create(Source : TObject);
begin
  inherited Create;

  FOwner := nil;
  FSource := Source;
  FDescription := '';
end;

destructor TClipboardAddable.Destroy;
begin
  inherited Destroy;
end;

// -------------------------------------- TClipboardEntry ----------------------

function TClipboardEntry.GetIndex : Integer;
begin
  if Owner = nil then
    Result := -1
  else
    Result := FOwner.IndexOfEntry(Self);
end;

procedure TClipboardEntry.SetIndex(NewIndex : Integer);
begin
  if Owner = nil then
    raise Exception.Create('Cannot set index because this Entry isn''t yet added to a clipboard!')
  else
    Owner.MoveEntry(Self, NewIndex);
end;

function TClipboardEntry.GetIndexByTable : Integer;
var
  i : Integer;
begin
  if Owner = nil then
    Result := -1
  else
  begin
    Result := 0;

    for i := 0 to Owner.EntryCount - 1 do
      if Owner.Entry[i].DataTable = Self.DataTable then
      begin
        if Owner.Entry[i] = Self then
          Exit
        else
          Inc(Result);
      end;
  end;
end;

procedure TClipboardEntry.SetIndexByTable(NewIndex : Integer);
var
  i : Integer;
  CurrCount : Integer;
begin
  if NewIndex = 0 then
    SetIndex(0)
  else if Owner = nil then
    raise Exception.Create('Cannot set index because this Entry isn''t yet added to a clipboard!')
  else
  begin
    CurrCount := 0;

    for i := 0 to Owner.EntryCount - 1 do
      if (Owner.Entry[i].DataTable = Self.DataTable) and
         (Owner.Entry[i] <> Self) then
      begin
        Inc(CurrCount);

        if NewIndex = CurrCount then
        begin
          SetIndex(i+1);
          Exit;
        end;
      end;
  end;
end;

constructor TClipboardEntry.Create(Source : TObject; DataTable : TDataTable; AddAllFields : Boolean);
var
  i : Integer;
begin
  FSource := Source;
  FDataTable := DataTable;

  FDataRows := TDataRowList.Create;

  FUISelectedFields := TList.Create;
  FActualSelectedFields := TList.Create;

  FOneRowSelected := False;
  FDescription := DataTable.Description;
  FOwner := nil;

  if AddAllFields then
    for i := 0 to DataTable.FieldCount - 1 do
      FActualSelectedFields.Add(DataTable.Field[i]);
end;

procedure TClipboardEntry.Delete;
begin
  Self.Free;
end;

destructor TClipboardEntry.Destroy;
begin
  FUISelectedFields.Free;
  FActualSelectedFields.Free;

  FDataRows.FreeAndClearContents;
  FDataRows.Free;

  if FOwner <> nil then
    FOwner.EntryDestroyed(Self);
end;

procedure TClipboardEntry.InsertToClipboard(AClipboard : TDataClipboard; Position : Integer);
begin
  AClipBoard.DoInsertEntry(Self, Position);
  OnInsertCalculations;
end;

procedure TClipboardEntry.OnInsertCalculations;

  procedure ReplaceRowsWithCopies(AList : TDataRowList);
  var
    TempRows : TDataRowList;
  begin
    TempRows := CreateCopyOfListAndRows(DataRows);
    DataRows.Clear;
    DataRows.Assign(TempRows);
    TempRows.Free;
  end;

begin
  FOneRowSelected := (DataRows.Count = 1) and
                     (DataRows.AbstractRows[0] is TDataRow);

  DataRows.ReplaceSubTotalsWithDetails;
  DataRows.RemoveDuplicates;
  ReplaceRowsWithCopies(DataRows);
end;

function TClipboardEntry.CreateCopyOfListAndRows(AList : TDataRowList) : TDataRowList;
var
  iRow : Integer;
begin
  Result := TDataRowList.Create;
  for iRow := 0 to AList.Count -1 do
    Result.AddObject('', AList.DataRows[iRow].CreateCopy );
end;

function TClipboardEntry.PasteOneRow(ConvertCriteria : TCondition) : TDataRow;
var
  ResultRows : TDataRowList;
  i : Integer;
  AValue : TValue;
begin
  if not OneRowSelected then
    raise Exception.Create('TClipboardEntry.PasteOneRow: This method is only legal when exactly one row is selected');

  if DataRows.Count = 0 then
  begin
    Result := nil;
    Exit;
  end;

  ResultRows := TDataRowList.Create;
  PasteKeyConvert(ResultRows, ConvertCriteria);
  if ResultRows.Count = 0 then
  begin
    Result := DataRows.DataRows[0].CreateCopy;
    for i := 0 to DataTable.KeyCount - 1 do
    begin
      if ConvertCriteria.AcceptsExactlyOneValue(DataTable.Field[i], AValue) then
        Result.ValueByIndex[i] := AValue
      else
        Result.ValueByIndex[i] := DataTable.Field[i].DataType.DefaultValue;
    end;
  end
  else
  begin
    Result := ResultRows.DataRows[0];
  end;

  ResultRows.Free;
end;

procedure TClipboardEntry.PasteKeyConvert(ResultRows : TStrings; ConvertCriteria : TCondition);
var
  EmptyList : TList;
begin
  EmptyList := TList.Create;
  PasteKeyConvertFieldMove(ResultRows, ConvertCriteria, EmptyList, EmptyList);
  EmptyList.Free;
end;

procedure TClipboardEntry.PasteKeyConvertFieldMove(ResultRows : TStrings; ConvertCriteria : TCondition; FromFields, ToFields : TList);
var
  SourceCriteria : TCondition;
  i : Integer;

  ListSource : TListSource;
  KeyTranslate : TRSRearrange;
  Filter : TRSFilter;
  Field : TDataField;
  CValue : TValue;
begin
  SourceCriteria := CreateListCriteria(DataRows, True);

  ListSource := TListSource.Create(DataRows, DataTable);

  KeyTranslate := TRSRearrange.CreateWithTable(ListSource, DataTable);
  for i := 0 to DataTable.KeyCount - 1 do
    if {SourceCriteria.AcceptsExactlyOneValue(DataTable.Field[i], SValue) and}
       ConvertCriteria.AcceptsExactlyOneValue(DataTable.Field[i], CValue) then
      KeyTranslate.AddConstantConversion(DataTable.Field[i], CValue);

  for i := DataTable.KeyCount to DataTable.FieldCount - 1 do
  begin
    Field := DataTable.Field[i];

    if ToFields.IndexOf(Field) >= 0 then
      KeyTranslate.AddConversion(Field, TDataField(FromFields.Items[ToFields.IndexOf(Field)]))
    else if FromFields.IndexOf(Field) >= 0 then
      KeyTranslate.ResetField(Field)
    else if ActualSelectedFields.IndexOf(Field) >= 0 then
      // keep value
    else
      KeyTranslate.ResetField(DataTable.Field[i]);
  end;

  Filter := TRSFilter.CreateWithCondition(KeyTranslate, ConvertCriteria);
  TDataRowList.AddFromSource(Filter, ResultRows);

  ListSource.Free;
  KeyTranslate.Free;
  Filter.Free;
  SourceCriteria.Free;
end;

procedure TClipboardEntry.AddToClipbrdInterface;
begin
  TEntryFormatObject.Create(Self);
end;

constructor TClipboardEntry.CreateCopy(Source : TClipboardEntry);

  function CreateCopyOfList(AList : TList) : TList;
  var
    iItem : Integer;
  begin
    Result := TList.Create;
    for iItem := 0 to AList.Count -1 do
      Result.Add(AList[iItem]);
  end;

begin
  Inherited Create(Source.Source);

  FOwner := Source.Owner;
  FDescription := Source.Description;
  FDataTable := Source.DataTable;
  FDataRows := CreateCopyOfListAndRows(Source.DataRows);
  FUISelectedFields := CreateCopyOfList(Source.UISelectedFields);
  FActualSelectedFields := CreateCopyOfList(Source.ActualSelectedFields);
  FOneRowSelected := Source.OneRowSelected;
end;

type
  TRowListStreamerLink = class(TRowListStreamer);

constructor TClipboardEntry.CreateFromClipboardData(Data : Pointer);
var
  AStream : TMemoryStream;
//  Position : Integer;

  procedure Read(var Buffer; Bytes : Integer);
  begin
    AStream.Read(Buffer, Bytes);
//    Move(Pointer(Integer(Data) + Position)^, Buffer, Bytes);
//    Inc(Position, Bytes);
  end;

  function ReadText : String;
  begin
    Result := PChar(Pointer(Integer(AStream.Memory) + AStream.Position));
    AStream.Position := AStream.Position + Length(Result) + 1;
//    Inc(Position, Length(Result) + 1);
    if Result = '' then
      raise Exception.Create('Invalid clipboard format!');
  end;

var
  SrcHandle : HWND;
  ASource : TObject;
  i, IntVal : Integer;
  StrVal : String;
begin
  Move(Data^, IntVal, SizeOf(Integer));

  AStream := TMemoryStream.Create;
  try
    AStream.Size := IntVal;
    Move(Data^, AStream.Memory^, IntVal);

  //  Position := 0;

    Read(SrcHandle, SizeOf(HWND));
    Read(ASource, SizeOf(TObject));

  {$ifndef LINUX}
    if Application.Handle = SrcHandle then
      inherited Create(ASource)
    else
      inherited Create(nil);
  {$else LINUX}
    inherited Create(nil);
  {$endif LINUX}

    FDataRows := TDataRowList.Create;
    FUISelectedFields := TList.Create;
    FActualSelectedFields := TList.Create;
    FOwner := nil;

    FDescription := ReadText;

    StrVal := ReadText;
    if StrVal = '' then
        raise Exception.Create('Cannot paste this table!');
    FDataTable := TDataTable.TableByName(StrVal);
    if FDataTable = nil then
        raise Exception.Create('Cannot paste this table!');

    Read(IntVal, SizeOf(Integer));
    for i := 0 to IntVal - 1 do
    begin
      StrVal := ReadText;
      if StrVal = '' then
        raise Exception.Create('Cannot paste this table!');

      FUISelectedFields.Add(TDataField.FieldByName(StrVal));
    end;

    Read(IntVal, SizeOf(Integer));
    for i := 0 to IntVal - 1 do
    begin
      StrVal := ReadText;
      if StrVal = '' then
        raise Exception.Create('Cannot paste this table!');

      FActualSelectedFields.Add(TDataField.FieldByName(StrVal));
    end;

    Read(FOneRowSelected, SizeOf(Boolean));

    with TRowListStreamerLink.Create(DataTable) do
      try
        Stream := AStream;
        LoadListFromStream(DataTable, FDataRows);
      finally
        Free;
      end;

    (*
    Read(IntVal, SizeOf(Integer));
    for i := 0 to IntVal - 1 do
      FDataRows.AddObject('', TDataRow.CreateFromByteData(DataTable, Data, Position));
      *)
  finally
    AStream.Free;
  end;
end;

procedure TClipboardEntry.AddToStream(AStream : TMemoryStream);

  procedure WriteNonEmpty(AStr : String);
  begin
    if AStr = '' then
      raise Exception.Create('Cannot paste this table!');

    AStream.Write(AStr[1], Length(AStr));
    AStream.Write(KEY_NULL, 1);
  end;

var
  i : Integer;
  IntVal : Integer;
begin
  // Make space for size-info
  IntVal := 0;
  AStream.Write(IntVal, SizeOf(Integer));
  AStream.Write(Application.Handle, SizeOf(HWND));
  AStream.Write(Pointer(FSource), SizeOf(Pointer));
  AStream.Write(FDescription[1], Length(FDescription));
  AStream.Write(KEY_NULL, 1);

  WriteNonEmpty(FDataTable.TableName);

  IntVal := FUISelectedFields.Count;
  AStream.Write(IntVal, SizeOf(Integer));
  for i := 0 to FUISelectedFields.Count - 1 do
    WriteNonEmpty(TDataField(FUISelectedFields.Items[i]).FieldName);

  IntVal := FActualSelectedFields.Count;
  AStream.Write(IntVal, SizeOf(Integer));
  for i := 0 to FActualSelectedFields.Count - 1 do
    WriteNonEmpty(TDataField(FActualSelectedFields.Items[i]).FieldName);

  AStream.Write(FOneRowSelected, SizeOf(Boolean));

  with TRowListStreamerLink.Create(DataTable) do
    try
      Stream := AStream;
      SaveListToStream(FDataRows);
    finally
      Free;
    end;
  (*
  IntVal := FDataRows.Count;
  AStream.Write(IntVal, SizeOf(Integer));
  for i := 0 to FDataRows.Count - 1 do
    FDataRows.DataRows[i].WriteDataToStream(AStream);
  *)
  AStream.Write(KEY_NULL, 1);

  // Save size info
  IntVal := AStream.Position;
  AStream.Position := 0;
  AStream.Write(IntVal, SizeOf(Integer));
  AStream.Position := IntVal;
end;

// ----------------------------- TClipboardEntryList ---------------------------

function TClipboardEntryList.GetCount : Integer;
begin
  Result := FEntryList.Count;
end;

function TClipboardEntryList.GetEntry(DataTable : TDataTable) : TClipboardEntry;
var
  i : Integer;
begin
  for i := 0 to Count - 1 do
    if EntryByIndex[i].DataTable = DataTable then
    begin
      Result := EntryByIndex[i];
      Exit;
    end;

  Result := FEntryClass.Create(Source, DataTable, FAddAllFields);
  FEntryList.Add(Result);
end;

function TClipboardEntryList.GetEntryByIndex(idx : Integer) : TClipboardEntry;
begin
  Result := TClipboardEntry(FEntryList.Items[idx]);
end;

constructor TClipboardEntryList.Create(Source : TObject; AddAllFields : Boolean);
begin
  inherited Create(Source);
  FAddAllFields := AddAllFields;

  FEntryClass := TClipboardEntry;
  FEntryList := TList.Create;
end;

destructor TClipboardEntryList.Destroy;
var
  i : Integer;
begin
  inherited Destroy;

  for i := 0 to FEntryList.Count - 1 do
    EntryByIndex[i].Free;
  FEntryList.Free;
end;

procedure TClipboardEntryList.InsertToClipboard(AClipboard : TDataClipboard; Position : Integer);
var
  i : Integer;
begin
  for i := 0 to Self.Count - 1 do
    EntryByIndex[i].InsertToClipboard(AClipBoard, Position);

  FEntryList.Clear;
  Free;  
end;

// ----------------------------- TDataClipboard --------------------------------

constructor TDataClipboard.Create(EmptyOnNewEntry, AllowTableDuplicates : Boolean);
begin
  inherited Create;

  FEntries := TList.Create;
  FEmptyOnNewEntry := EmptyOnNewEntry;
  FAllowTableDuplicates := AllowTableDuplicates;
end;

destructor TDataClipboard.Destroy;
begin
  EmptyClipboard;
  FEntries.Free;
end;

procedure TDataClipboard.InsertToClipboard(Entry : TClipboardAddable; Index : Integer);
begin
  if FEmptyOnNewEntry then
  begin
    EmptyClipboard;
    Entry.InsertToClipboard(Self, 0);
  end
  else
  begin
    Entry.InsertToClipboard(Self, Index);
  end;
end;

procedure TDataClipboard.AddToClipboard(Entry : TClipboardAddable);
begin
  InsertToClipboard(Entry, 0);
end;

procedure TDataClipboard.EmptyClipboard;
var
  i : Integer;
begin
  for i := EntryCount - 1 downto 0 do
  begin
    Entry[i].FOwner := nil;
    Entry[i].Free;
  end;

  FEntries.Clear;
end;

procedure TDataClipboard.MoveEntry(Entry : TClipboardEntry; NewIndex : Integer);
begin
  if Entry.Owner <> Self then
    raise Exception.Create('This Entry isn''t in the Clipboard!');

  FEntries.Move(Entry.Index, NewIndex);
end;

procedure TDataClipboard.MoveIndex(OldIndex, NewIndex : Integer);
begin
  FEntries.Move(Oldindex, NewIndex);
end;

function TDataClipboard.GetTopEntry : TClipboardEntry;
begin
  if EntryCount = 0 then
    Result := nil
  else
    Result := Entry[0];
end;

function TDataClipboard.GetEntryCount : Integer;
begin
  Result := FEntries.Count;
end;

function TDataClipboard.GetIsEmpty : Boolean;
begin
  Result := (EntryCount = 0);
end;

function TDataClipboard.GetEntry(idx : Integer) : TClipboardEntry;
begin
  Result := TClipboardEntry(FEntries.Items[idx]);
end;

function TDataClipboard.GetTableEntryCount(Table : TDataTable) : Integer;
var
  i : Integer;
begin
  Result := 0;

  for i := 0 to EntryCount - 1 do
    if Entry[i].DataTable = Table then
      Inc(Result);
end;

function TDataClipboard.GetTableEntry(Table : TDataTable; idx : Integer) : TClipboardEntry;
var
  i : Integer;
  Counter : Integer;
begin
  Counter := 0;

  for i := 0 to EntryCount - 1 do
    if Entry[i].DataTable = Table then
    begin
      if idx = Counter then
      begin
        Result := Entry[i];
        Exit;
      end
      else
        Inc(Counter);
    end;

  raise Exception.Create('Index out of bounds!');
end;

procedure TDataClipboard.EntryDestroyed(Entry : TClipboardEntry);
begin
  FEntries.Remove(Entry);
end;

function TDataClipboard.IndexOfEntry(Entry : TClipboardEntry) : Integer;
begin
  Result := FEntries.IndexOf(Entry);
end;

procedure TDataClipboard.DoInsertEntry(Entry : TClipboardEntry; Position : Integer);
begin
  if not FAllowTableDuplicates then
    DeleteAllTableEntries(Entry.DataTable);
  Entry.FOwner := Self;

  FEntries.Insert(Position, Entry);
end;

procedure TDataClipboard.DeleteAllTableEntries(DataTable : TDataTable);
var
  i : Integer;
  AEntry : TClipboardEntry;
begin
  for i := EntryCount - 1 downto 0 do
    if Entry[i].DataTable = DataTable then
    begin
      AEntry := Entry[i];
      FEntries.Delete(i);
      AEntry.FOwner := nil;
      AEntry.Delete;
    end;
end;

{ TEntryFormatObject }

constructor TEntryFormatObject.Create(AEntry : TClipboardEntry);
begin
  Inherited Create(GetFormat, nil);

  FEntry := TClipboardEntry.CreateCopy(AEntry);
end;

class function TEntryFormatObject.GetFormat : UINT;
begin
  Result := EntryFormatObjectFormat;
end;

class function TEntryFormatObject.CreateEntryFromClipboard(AFormat: UINT): TClipboardEntry;
var
  Data: THandle;
  GlobalData : Pointer;
begin
  Result := nil;
  OpenClipboard(ClipbrdInterface.Handle);
{$ifndef LINUX}
  Data := GetClipboardData(AFormat);
  if Data <> 0 then
  begin
    try
      GlobalData := GlobalLock(Data);
      try
        Result := TClipboardEntry.CreateFromClipboardData(GlobalData);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  end;
{$endif LINUX}
  CloseClipboard;
end;

destructor TEntryFormatObject.Destroy;
begin
  inherited Destroy;
  FEntry.Free;
end;

procedure TEntryFormatObject.SetData(RenderNow: Boolean);
var
  AStream : TMemoryStream;
begin
  if RenderNow then
  begin
    AStream := TMemoryStream.Create;
    FEntry.AddToStream(AStream);
    SetStream(AStream);
  end;

  Inherited SetData(RenderNow);
end;

initialization

end.

