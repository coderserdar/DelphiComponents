unit NewTableFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls, StdCtrls,
  Db, ImgList, TinyDB, BaseFrm;

type
  PFieldItemInfo = ^TFieldItemInfo;
  TFieldItemInfo = record
    Idx: Integer;
    FieldName: string;
    FieldType: TFieldType;
    DataSize: Integer;
    DPMode: TFieldDataProcessMode;
    Comments: string;
  end;

  PIndexItemInfo = ^TIndexItemInfo;
  TIndexItemInfo = record
    IndexName: string;
    IndexOptions: TTDIndexOptions;
    IndexFields: string;
  end;

  TNewTableForm = class(TBaseForm)
    TableNameLabel: TLabel;
    TableNameEdit: TEdit;
    FieldGroupBox: TGroupBox;
    FieldListView: TListView;
    AddFieldButton: TButton;
    DeleteFieldButton: TButton;
    IndexGroupBox: TGroupBox;
    IndexListView: TListView;
    AddIndexButton: TButton;
    DeleteIndexButton: TButton;
    MoveDownFieldButton: TButton;
    ClearIndexButton: TButton;
    CreateTableButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    MoveUpFieldButton: TButton;
    ImageList: TImageList;
    procedure AddFieldButtonClick(Sender: TObject);
    procedure DeleteFieldButtonClick(Sender: TObject);
    procedure MoveDownFieldButtonClick(Sender: TObject);
    procedure AddIndexButtonClick(Sender: TObject);
    procedure DeleteIndexButtonClick(Sender: TObject);
    procedure ClearIndexButtonClick(Sender: TObject);
    procedure CreateTableButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure MoveUpFieldButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FieldListViewDblClick(Sender: TObject);
    procedure IndexListViewDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FModifyMode: Boolean;
    FTinyDB: TTinyDatabase;
    FTableName: string;

    procedure AddFieldNodeData(ListItem: TListItem; FieldItemInfo: TFieldItemInfo);
    procedure FreeFieldNodeData(ListItem: TListItem);
    procedure ClearFieldNodeData;
    function GetFieldNodeData(ListItem: TListItem): TFieldItemInfo;

    procedure AddIndexNodeData(ListItem: TListItem; IndexItemInfo: TIndexItemInfo);
    procedure FreeIndexNodeData(ListItem: TListItem);
    procedure ClearIndexNodeData;
    function GetIndexNodeData(ListItem: TListItem): TIndexItemInfo;

    procedure AdjustFieldIdxList;
    procedure AdjustIndexIdxList;
    function GetCanIndexedFields: string;
    function GetStrByIndexFieldIdxes(TinyTable: TTinyTable; FieldIdxes: array of Integer): string;
    function GetTempFileName: string;

    function CheckValid: Boolean;
    function CreateTableFromUI(ATinyDB: TTinyDatabase; TableName: string): Boolean;
    function CreateTable: Boolean;
    function ModifyTable: Boolean;

  public
    { Public declarations }
    procedure TransLanguage; override;
    procedure SetData(TinyDB: TTinyDatabase; TableName: string);

    procedure InsertFieldToListView(Item: TFieldItemInfo; Index: Integer);
    procedure AddFieldToListView(Item: TFieldItemInfo);

    procedure InsertIndexToListView(Item: TIndexItemInfo; Index: Integer);
    procedure AddIndexToListView(Item: TIndexItemInfo);

    property ModifyMode: Boolean read FModifyMode write FModifyMode;
  end;

var
  NewTableForm: TNewTableForm;

function ShowNewTableForm: Boolean;
function ShowDesignTableForm(TinyDB: TTinyDatabase; TableName: string): Boolean;

implementation

uses MainFrm, Misc, AddFieldFrm, AddIndexFrm, LangMgr;

{$R *.DFM}

function ShowNewTableForm: Boolean;
var
  Frm: TNewTableForm;
begin
  Frm := TNewTableForm.Create(Application);
  Result := Frm.ShowModal = mrOk;
  Frm.Free;
end;

function ShowDesignTableForm(TinyDB: TTinyDatabase; TableName: string): Boolean;
var
  Frm: TNewTableForm;
begin
  Frm := TNewTableForm.Create(Application);
  Frm.Caption := AppLangMgr.Trans('Design Table');
  Frm.CreateTableButton.Caption := AppLangMgr.Trans('OK');
  Frm.ModifyMode := True;
  Frm.SetData(TinyDB, TableName);
  Result := Frm.ShowModal = mrOk;
  Frm.Free;
end;

procedure TNewTableForm.FormCreate(Sender: TObject);
begin
  AppLangMgr.Trans(Self);
end;

procedure TNewTableForm.FormDestroy(Sender: TObject);
begin
  ClearFieldNodeData;
end;

procedure TNewTableForm.TransLanguage;
begin
  inherited;
  FieldListView.Columns[0].Caption := AppLangMgr.Trans('NO.');
  FieldListView.Columns[1].Caption := AppLangMgr.Trans('Field Name');
  FieldListView.Columns[2].Caption := AppLangMgr.Trans('Field Type');
  FieldListView.Columns[3].Caption := AppLangMgr.Trans('Field Size');
  FieldListView.Columns[4].Caption := AppLangMgr.Trans('Data Processing');
  FieldListView.Columns[5].Caption := AppLangMgr.Trans('Note');

  IndexListView.Columns[0].Caption := AppLangMgr.Trans('NO.');
  IndexListView.Columns[1].Caption := AppLangMgr.Trans('Index Name');
  IndexListView.Columns[2].Caption := AppLangMgr.Trans('Index Field');
  IndexListView.Columns[3].Caption := AppLangMgr.Trans('Primary');
  IndexListView.Columns[4].Caption := AppLangMgr.Trans('Unique');
  IndexListView.Columns[5].Caption := AppLangMgr.Trans('Case Sensitive');
  IndexListView.Columns[6].Caption := AppLangMgr.Trans('Sort Mode');

  TableNameEdit.BoundsRect := Rect(TableNameLabel.BoundsRect.Right + 4,
    TableNameEdit.Top, TableNameEdit.BoundsRect.Right, TableNameEdit.BoundsRect.Bottom);
end;

procedure TNewTableForm.SetData(TinyDB: TTinyDatabase; TableName: string);
var
  FieldItem: TFieldItemInfo;
  IndexItem: TIndexItemInfo;
  TinyTable: TTinyTable;
  I, AutoIncNum: Integer;
begin
  FTinyDB := TinyDB;
  FTableName := TableName;
  TableNameEdit.Text := TableName;

  FieldListView.Items.BeginUpdate;
  FieldListView.Items.Clear;
  IndexListView.Items.BeginUpdate;
  IndexListView.Items.Clear;
  TinyTable := TTinyTable.Create(nil);
  try
    TinyTable.DatabaseName := TinyDB.DatabaseName;
    TinyTable.TableName := FTableName;
    TinyTable.Open;
    AutoIncNum := 0;
    for I := 0 to TinyTable.FieldDefs.Count - 1 do
    begin
      FieldItem.Idx := I;
      FieldItem.FieldName := TinyTable.TableIO.FieldDefs[I].Name;
      FieldItem.FieldType := TinyTable.TableIO.FieldDefs[I].FieldType;
      FieldItem.DataSize := TinyTable.TableIO.FieldDefs[I].FieldSize;
      FieldItem.DPMode := TinyTable.TableIO.FieldDefs[I].DPMode;
      FieldItem.Comments := MainForm.GetFieldCommentsByType(FieldItem.FieldType);
      AddFieldToListView(FieldItem);
      if FieldItem.FieldType = ftAutoInc then Inc(AutoIncNum);
    end;
    for I := 0 to TinyTable.IndexDefs.Count - 1 do
    begin
      IndexItem.IndexName := TinyTable.IndexDefs[I].Name;
      IndexItem.IndexOptions := TinyTable.TableIO.IndexDefs[I].Options;
      IndexItem.IndexFields := GetStrByIndexFieldIdxes(TinyTable, TinyTable.TableIO.IndexDefs[I].FieldIdxes);
      if (AutoIncNum = 0) or not (tiPrimary in IndexItem.IndexOptions) then
        AddIndexToListView(IndexItem);
    end;
  finally
    FieldListView.Items.EndUpdate;
    IndexListView.Items.EndUpdate;
    TinyTable.Close;
    TinyTable.Free;
  end;
end;

procedure TNewTableForm.AddFieldNodeData(ListItem: TListItem; FieldItemInfo: TFieldItemInfo);
var
  ItemPtr: PFieldItemInfo;
begin
  New(ItemPtr);
  ItemPtr^ := FieldItemInfo;
  ListItem.Data := ItemPtr;
end;

procedure TNewTableForm.FreeFieldNodeData(ListItem: TListItem);
begin
  Dispose(PFieldItemInfo(ListItem.Data));
end;

procedure TNewTableForm.ClearFieldNodeData;
var
  I: Integer;
begin
  for I := 0 to FieldListView.Items.Count - 1 do
    Dispose(PFieldItemInfo(FieldListView.Items[I].Data));
end;

function TNewTableForm.GetFieldNodeData(ListItem: TListItem): TFieldItemInfo;
begin
  Result := PFieldItemInfo(ListItem.Data)^;
end;

procedure TNewTableForm.AddIndexNodeData(ListItem: TListItem; IndexItemInfo: TIndexItemInfo);
var
  ItemPtr: PIndexItemInfo;
begin
  New(ItemPtr);
  ItemPtr^ := IndexItemInfo;
  ListItem.Data := ItemPtr;
end;

procedure TNewTableForm.FreeIndexNodeData(ListItem: TListItem);
begin
  Dispose(PIndexItemInfo(ListItem.Data));
end;

procedure TNewTableForm.ClearIndexNodeData;
var
  I: Integer;
begin
  for I := 0 to IndexListView.Items.Count - 1 do
    Dispose(PIndexItemInfo(IndexListView.Items[I].Data));
end;

function TNewTableForm.GetIndexNodeData(ListItem: TListItem): TIndexItemInfo;
begin
  Result := PIndexItemInfo(ListItem.Data)^;
end;

procedure TNewTableForm.AdjustFieldIdxList;
var
  I: Integer;
begin
  for I := 0 to FieldListView.Items.Count - 1 do
    FieldListView.Items[I].Caption := IntToStr(I);
end;

procedure TNewTableForm.AdjustIndexIdxList;
var
  I: Integer;
begin
  for I := 0 to IndexListView.Items.Count - 1 do
    IndexListView.Items[I].Caption := IntToStr(I);
end;

function TNewTableForm.GetCanIndexedFields: string;
var
  I: Integer;
  List: TStrings;
begin
  List := TStringList.Create;
  for I := 0 to FieldListView.Items.Count - 1 do
    List.Add(GetFieldNodeData(FieldListView.Items[I]).FieldName);
  Result := List.CommaText;
  List.Free;
end;

function TNewTableForm.GetStrByIndexFieldIdxes(TinyTable: TTinyTable; FieldIdxes: array of Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(FieldIdxes) do
  begin
    if I <> 0 then Result := Result + ',';
    Result := Result + TinyTable.FieldDefs[FieldIdxes[I]].Name;
  end;
end;

function TNewTableForm.GetTempFileName: string;
var
  Buf: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, Buf);
  Windows.GetTempFileName(Buf, 'TDB', 0, Buf);
  Result := Buf;
end;

function TNewTableForm.CheckValid: Boolean;
var
  I, J, AutoIncNum: Integer;
begin
  Result := True;
  if not IsValidDBName(TableNameEdit.Text) then
  begin
    if TableNameEdit.Text <> '' then
      Application.MessageBox(PChar(AppLangMgr.Trans('Invalid identifier.')), PChar(Application.Title), 48)
    else
      Application.MessageBox(PChar(AppLangMgr.Trans('Please input table name.')), PChar(Application.Title), 48);
    TableNameEdit.SetFocus;
    Result := False;
    Exit;
  end;

  for I := 0 to FieldListView.Items.Count - 2 do
    for J := I + 1 to FieldListView.Items.Count - 1 do
    begin
      if CompareText(GetFieldNodeData(FieldListView.Items[I]).FieldName,
        GetFieldNodeData(FieldListView.Items[J]).FieldName) = 0 then
      begin
        Application.MessageBox(PChar(AppLangMgr.Trans('Duplicate field name.')), PChar(Application.Title), 48);
        FieldListView.SetFocus;
        FieldListView.Items[I].Selected := True;
        Result := False;
        Exit;
      end;
    end;

  for I := 0 to IndexListView.Items.Count - 2 do
    for J := I + 1 to IndexListView.Items.Count - 1 do
    begin
      if CompareText(GetIndexNodeData(IndexListView.Items[I]).IndexName,
        GetIndexNodeData(IndexListView.Items[J]).IndexName) = 0 then
      begin
        Application.MessageBox(PChar(AppLangMgr.Trans('Duplicate index name.')), PChar(Application.Title), 48);
        IndexListView.SetFocus;
        IndexListView.Items[I].Selected := True;
        Result := False;
        Exit;
      end;
    end;

  AutoIncNum := 0;
  for I := 0 to FieldListView.Items.Count - 1 do
    if GetFieldNodeData(FieldListView.Items[I]).FieldType = ftAutoInc then
      Inc(AutoIncNum);
  if AutoIncNum > 1 then
  begin
    Application.MessageBox(PChar(AppLangMgr.Trans('Too many AutoInc fields.')), PChar(Application.Title), 48);
    FieldListView.SetFocus;
    Result := False;
    Exit;
  end;
  if AutoIncNum = 1 then
  begin
    for I := 0 to IndexListView.Items.Count - 1 do
    begin
      if tiPrimary in GetIndexNodeData(IndexListView.Items[I]).IndexOptions then
      begin
        Application.MessageBox(PChar(AppLangMgr.Trans('AutoInc field is primary key, so you cannot create the second primary index.')), PChar(Application.Title), 48);
        IndexListView.Items[I].Selected := True;
        IndexListView.SetFocus;
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function TNewTableForm.CreateTableFromUI(ATinyDB: TTinyDatabase; TableName: string): Boolean;
var
  FieldItems: array of TFieldItem;
  FieldNames: array of string;
  FieldInfo: TFieldItemInfo;
  IndexInfo: TIndexItemInfo;
  I, Idx: Integer;
  List: TStrings;
begin
  SetLength(FieldItems, FieldListView.Items.Count);
  for I := 0 to FieldListView.Items.Count - 1 do
  begin
    FieldInfo := GetFieldNodeData(FieldListView.Items[I]);
    FieldItems[I].FieldName := ShortString(FieldInfo.FieldName);
    FieldItems[I].FieldType := FieldInfo.FieldType;
    FieldItems[I].DataSize := FieldInfo.DataSize;
    FieldItems[I].DPMode := FieldInfo.DPMode;
  end;
  Result := ATinyDB.CreateTable(TableName, FieldItems);

  List := TStringList.Create;
  try
    try
      for Idx := 0 to IndexListView.Items.Count - 1 do
      begin
        IndexInfo := GetIndexNodeData(IndexListView.Items[Idx]);
        List.Clear;
        List.CommaText := IndexInfo.IndexFields;
        SetLength(FieldNames, List.Count);
        for I := 0 to List.Count - 1 do
          FieldNames[I] := List[I];
        Result := ATinyDB.CreateIndex(TableName, IndexInfo.IndexName, IndexInfo.IndexOptions, FieldNames);
      end;
    except
      ATinyDB.DeleteTable(TableName);
      raise;
    end;
  finally
    List.Free;
  end;
end;

function TNewTableForm.CreateTable: Boolean;
begin
  Result := CreateTableFromUI(MainForm.TinyDatabase, TableNameEdit.Text);
end;

function TNewTableForm.ModifyTable: Boolean;
var
  TempFile: string;
  TempTinyDB: TTinyDatabase;
  SrcTinyTable, DstTinyTable: TTinyTable;
  NewTableName: string;
  I, J, RecIdx: Integer;
begin
  Result := True;
  TempFile := GetTempFileName;
  TempTinyDB := TTinyDatabase.Create(nil);
  TempTinyDB.CreateDatabase(TempFile);
  SrcTinyTable := TTinyTable.Create(nil);
  DstTinyTable := TTinyTable.Create(nil);
  MainForm.BeginProgress;
  try
    try
      NewTableName := TableNameEdit.Text;
      TempTinyDB.DatabaseName := TempFile;
      CreateTableFromUI(TempTinyDB, NewTableName);

      //Copy records to temporary table
      SrcTinyTable.DatabaseName := FTinyDB.DatabaseName;
      SrcTinyTable.TableName := FTableName;
      SrcTinyTable.Open;

      DstTinyTable.DatabaseName := TempFile;
      DstTinyTable.TableName := NewTableName;
      DstTinyTable.Open;
      DstTinyTable.BeginUpdate;
      for RecIdx := 0 to SrcTinyTable.RecordCount - 1 do
      begin
        MainForm.DoProgress(Trunc((RecIdx + 1) / SrcTinyTable.RecordCount * 100));
        DstTinyTable.Append;
        for I := 0 to SrcTinyTable.Fields.Count - 1 do
          for J := 0 to FieldListView.Items.Count - 1 do
          begin
            if I = GetFieldNodeData(FieldListView.Items[J]).Idx then
            begin
              DstTinyTable.Fields[J].Value := SrcTinyTable.Fields[I].Value;
              Break;
            end;
          end;
        DstTinyTable.Post;
        SrcTinyTable.Next;
      end;
      SrcTinyTable.Close;
      DstTinyTable.EndUpdate;
      DstTinyTable.Close;

      //Delete old table
      FTinyDB.DeleteTable(FTableName);
      //Create new table
      CreateTableFromUI(FTinyDB, NewTableName);

      //Copy records from temporary table to new table
      SrcTinyTable.DatabaseName := TempFile;
      SrcTinyTable.TableName := NewTableName;
      SrcTinyTable.Open;

      DstTinyTable.DatabaseName := FTinyDB.DatabaseName;
      DstTinyTable.TableName := NewTableName;
      DstTinyTable.Open;
      DstTinyTable.BeginUpdate;
      for RecIdx := 0 to SrcTinyTable.RecordCount - 1 do
      begin
        MainForm.DoProgress(Trunc((RecIdx + 1) / SrcTinyTable.RecordCount * 100));
        DstTinyTable.Append;
        for I := 0 to SrcTinyTable.Fields.Count - 1 do
          DstTinyTable.Fields[I].Value := SrcTinyTable.Fields[I].Value;
        DstTinyTable.Post;
        SrcTinyTable.Next;
      end;
      SrcTinyTable.Close;
      DstTinyTable.EndUpdate;
      DstTinyTable.Close;
    finally
      DeleteFile(TempFile);
      TempTinyDB.Free;
      SrcTinyTable.Free;
      DstTinyTable.Free;
      MainForm.EndProgress;
    end;
  except
    Application.MessageBox(PChar(AppLangMgr.Trans('Fail to modify table structure. Perhaps, field type cannot be converted, or database cannot be written.')), PChar(Application.Title), 48);
    Result := False;
  end;
end;

procedure TNewTableForm.InsertFieldToListView(Item: TFieldItemInfo; Index: Integer);
var
  DPModeStr: array[TFieldDataProcessMode] of string;
  ListItem: TListItem;
begin
  DPModeStr[fdDefault] := AppLangMgr.Trans('fdDefault');
  DPModeStr[fdOriginal] := AppLangMgr.Trans('fdOriginal');

  ListItem := FieldListView.Items.Insert(Index);
  ListItem.Caption := IntToStr(Index);
  ListItem.SubItems.Add(Item.FieldName);
  ListItem.SubItems.Add(MainForm.GetFieldStrByType(Item.FieldType));
  ListItem.SubItems.Add(IntToStr(Item.DataSize));
  ListItem.SubItems.Add(DPModeStr[Item.DPMode]);
  ListItem.SubItems.Add(Item.Comments);
  ListItem.ImageIndex := 1;

  AddFieldNodeData(ListItem, Item);
  AdjustFieldIdxList;
end;

procedure TNewTableForm.AddFieldToListView(Item: TFieldItemInfo);
begin
  InsertFieldToListView(Item, FieldListView.Items.Count);
end;

procedure TNewTableForm.InsertIndexToListView(Item: TIndexItemInfo; Index: Integer);
var
  YesStr, NoStr: string;
  AscStr, DescStr: string;
  ListItem: TListItem;
begin
  YesStr := AppLangMgr.Trans('Yes');
  NoStr := AppLangMgr.Trans('No');
  AscStr := AppLangMgr.Trans('Ascending');
  DescStr := AppLangMgr.Trans('Descending');

  ListItem := IndexListView.Items.Insert(Index);
  ListItem.Caption := IntToStr(Index);
  ListItem.SubItems.Add(Item.IndexName);
  ListItem.SubItems.Add(Item.IndexFields);
  ListItem.SubItems.Add(Iif(tiPrimary in Item.IndexOptions, YesStr, NoStr));
  ListItem.SubItems.Add(Iif(tiUnique in Item.IndexOptions, YesStr, NoStr));
  ListItem.SubItems.Add(Iif(tiCaseInsensitive in Item.IndexOptions, NoStr, YesStr));
  ListItem.SubItems.Add(Iif(tiDescending in Item.IndexOptions, DescStr, AscStr));
  ListItem.ImageIndex := 2;

  AddIndexNodeData(ListItem, Item);
  AdjustIndexIdxList;
end;

procedure TNewTableForm.AddIndexToListView(Item: TIndexItemInfo);
begin
  InsertIndexToListView(Item, IndexListView.Items.Count);
end;

procedure TNewTableForm.AddFieldButtonClick(Sender: TObject);
var
  Value: TAddFieldFormData;
begin
  Value.NewTableForm := Self;
  Value.FieldInfo.DPMode := fdDefault;
  ShowAddFieldForm(Value);
end;

procedure TNewTableForm.DeleteFieldButtonClick(Sender: TObject);
var
  ListItem: TListItem;
begin
  ListItem := FieldListView.Selected;
  if ListItem = nil then Exit;

  FreeFieldNodeData(ListItem);
  ListItem.Delete;
  AdjustFieldIdxList;
end;

procedure TNewTableForm.MoveUpFieldButtonClick(Sender: TObject);
var
  ListItem: TListItem;
  FieldItemInfo: TFieldItemInfo;
  Idx: Integer;
begin
  ListItem := FieldListView.Selected;
  if ListItem = nil then Exit;
  if ListItem.Index = 0 then Exit;

  Idx := ListItem.Index;
  FieldItemInfo := GetFieldNodeData(ListItem);
  ListItem.Delete;
  if Idx > 0 then Dec(Idx);
  InsertFieldToListView(FieldItemInfo, Idx);
  FieldListView.Items[Idx].Selected := True;
end;

procedure TNewTableForm.MoveDownFieldButtonClick(Sender: TObject);
var
  ListItem: TListItem;
  FieldItemInfo: TFieldItemInfo;
  Idx: Integer;
begin
  ListItem := FieldListView.Selected;
  if ListItem = nil then Exit;
  if ListItem.Index = FieldListView.Items.Count - 1 then Exit;

  Idx := ListItem.Index;
  FieldItemInfo := GetFieldNodeData(ListItem);
  if Idx < FieldListView.Items.Count - 1 then Inc(Idx);
  ListItem.Delete;
  InsertFieldToListView(FieldItemInfo, Idx);
  FieldListView.Items[Idx].Selected := True;
end;

procedure TNewTableForm.AddIndexButtonClick(Sender: TObject);
var
  Value: TAddIndexFormData;
begin
  Value.NewTableForm := Self;
  Value.AvailableFields := GetCanIndexedFields;
  ShowAddIndexForm(Value);
end;

procedure TNewTableForm.DeleteIndexButtonClick(Sender: TObject);
var
  ListItem: TListItem;
begin
  ListItem := IndexListView.Selected;
  if ListItem = nil then Exit;

  FreeIndexNodeData(ListItem);
  ListItem.Delete;
  AdjustIndexIdxList;
end;

procedure TNewTableForm.ClearIndexButtonClick(Sender: TObject);
begin
  ClearIndexNodeData;
  IndexListView.Items.Clear;
end;

procedure TNewTableForm.CreateTableButtonClick(Sender: TObject);
begin
  if CheckValid then
  begin
    if FModifyMode then
    begin
      if ModifyTable then ModalResult := mrOk;
    end
    else
    begin
      if CreateTable then ModalResult := mrOk;
    end;
  end;
end;

procedure TNewTableForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TNewTableForm.HelpButtonClick(Sender: TObject);
begin
  ShowHelp;
end;

procedure TNewTableForm.FieldListViewDblClick(Sender: TObject);
var
  ListItem: TListItem;
  Idx: Integer;
  Value: TAddFieldFormData;
begin
  ListItem := FieldListView.Selected;
  if ListItem = nil then Exit;

  Idx := ListItem.Index;
  Value.FieldInfo := GetFieldNodeData(ListItem);
  if ShowAddFieldForm(Value, True) then
  begin
    FieldListView.Items.BeginUpdate;
    FreeFieldNodeData(ListItem);
    ListItem.Delete;
    InsertFieldToListView(Value.FieldInfo, Idx);
    FieldListView.Items[Idx].Selected := True;
    FieldListView.Items.EndUpdate;
  end;
end;

procedure TNewTableForm.IndexListViewDblClick(Sender: TObject);
var
  ListItem: TListItem;
  Idx: Integer;
  Value: TAddIndexFormData;
begin
  ListItem := IndexListView.Selected;
  if ListItem = nil then Exit;

  Idx := ListItem.Index;
  Value.NewTableForm := Self;
  Value.AvailableFields := GetCanIndexedFields;
  Value.IndexInfo := GetIndexNodeData(ListItem);
  if ShowAddIndexForm(Value, True) then
  begin
    FreeIndexNodeData(ListItem);
    ListItem.Delete;
    InsertIndexToListView(Value.IndexInfo, Idx);
    IndexListView.Items[Idx].Selected := True;
  end;
end;

end.
