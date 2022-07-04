
{*******************************************************}
{                                                       }
{       AXIOMA Delphi Visual Component Library          }
{       AOraUpdateSQL Component Editor                  }
{                                                       }
{       Copyright (c) 1997,2001 AXIOMA Corp             }
{                                                       }
{*******************************************************}

unit AOraUpdateSQLEd;

{$INCLUDE dOCI.inc}

interface

uses Forms, DB, ExtCtrls, StdCtrls, Controls, ComCtrls,
     Classes, SysUtils, Windows, Menus, OraDB, AOraUpdateSQL, OraSQL,
     AOraSQL, VirtualDataSet, DataSetQuery, dOCIMessages
     {$IFDEF D4} ,Dsgnintf {$ENDIF}
     {$IFDEF D5} ,Dsgnintf {$ENDIF}
     {$IFDEF D6} ,DesignIntf, DesignEditors {$ENDIF}
     {$IFDEF D7} ,DesignIntf, DesignEditors {$ENDIF}
     ;


type                   

  TWaitMethod = procedure of object;

  TAOraUpdateSQLEditForm = class(TForm)
    OkButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    GenerateButton: TButton;
    PrimaryKeyButton: TButton;
    DefaultButton: TButton;
    UpdateTableName: TComboBox;
    FieldsPage: TTabSheet;
    SQLPage: TTabSheet;
    PageControl: TPageControl;
    KeyFieldList: TListBox;
    UpdateFieldList: TListBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    SQLMemo: TMemo;
    StatementType: TRadioGroup;
    QuoteFields: TCheckBox;
    GetTableFieldsButton: TButton;
    FieldListPopup: TPopupMenu;
    miSelectAll: TMenuItem;
    miClearAll: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure StatementTypeClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure DefaultButtonClick(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
    procedure PrimaryKeyButtonClick(Sender: TObject);
    procedure PageControlChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure GetTableFieldsButtonClick(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure UpdateTableNameChange(Sender: TObject);
    procedure UpdateTableNameClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
    procedure ClearAllClick(Sender: TObject);
    procedure SQLMemoKeyPress(Sender: TObject; var Key: Char);
  private
    StmtIndex: Integer;
    DataSet: TOraSQL;//TDBDataSet;
//    Database: TOraDB;//TDatabase
//    DatabaseOpened: Boolean;
    DatabaseSelfOpen:Boolean;
    UpdateSQL: TAOraUpdateSQL;//TUpdateSQL
    FSettingsChanged: Boolean;
    FDatasetDefaults: Boolean;
    FTempSQL:TOraSQL;
    SQLText: array[TUpdateKind] of TStrings;
    function GetTableRef(const TabName, QuoteChar: string): string;
//    function DatabaseOpen: Boolean;
    function Edit: Boolean;
    procedure GenWhereClause(const TabAlias, QuoteChar: string;
      KeyFields, SQL: TStrings);
    procedure GenDeleteSQL(const TableName, QuoteChar: string;
      KeyFields, SQL: TStrings);
    procedure GenInsertSQL(const TableName, QuoteChar: string;
      UpdateFields, SQL: TStrings);
    procedure GenModifySQL(const TableName, QuoteChar: string;
      KeyFields, UpdateFields, SQL: TStrings);
    procedure GenerateSQL;
    procedure GetDataSetFieldNames;
    procedure GetTableFieldNames;
    procedure InitGenerateOptions;
    procedure InitUpdateTableNames;
    procedure SetButtonStates;
    procedure SelectPrimaryKeyFields;
    procedure SetDefaultSelections;
    procedure ShowWait(WaitMethod: TWaitMethod);
    function TempTable: TOraSQL;
  end;

{ TSQLParser }

  TSQLToken = (stSymbol, stAlias, stNumber, stComma, stEQ, stOther, stLParen,
    stRParen, stEnd);

  TSQLParser = class
  private
    FText: string;
    FSourcePtr: PChar;
    FTokenPtr: PChar;
    FTokenString: string;
    FToken: TSQLToken;
    FSymbolQuoted: Boolean;
    function NextToken: TSQLToken;
    function TokenSymbolIs(const S: string): Boolean;
    procedure Reset;
  public
    constructor Create(const Text: string);
    procedure GetSelectTableNames(List: TStrings);
    procedure GetUpdateTableName(var TableName: string);
    procedure GetUpdateFields(List: TStrings);
    procedure GetWhereFields(List: TStrings);
  end;

  TAOraUpdateSQLEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

//function EditUpdateSQL(AUpdateSQL: TUpdateSQL): Boolean;
function EditUpdateSQL(AOraUpdateSQL: TAOraUpdateSQL): Boolean;
procedure Register;

implementation

{$R *.DFM}

uses Dialogs;//, LibHelp;//, TypInfo;

procedure Register;
begin
  RegisterComponentEditor(TAOraUpdateSQL,TAOraUpdateSQLEditor);
end;


{ Global Interface functions }

function EditUpdateSQL(AOraUpdateSQL: TAOraUpdateSQL): Boolean;
begin
  with TAOraUpdateSQLEditForm.Create(Application) do
  try
    UpdateSQL := AOraUpdateSQL;
    Result := Edit;
  finally
    Free;
  end;
end;

{ Utility Routines }

procedure GetSelectedItems(ListBox: TListBox; List: TStrings);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to ListBox.Items.Count - 1 do
    if ListBox.Selected[I] then
      List.Add(ListBox.Items[I]);
end;

function SetSelectedItems(ListBox: TListBox; List: TStrings): Integer;
var
  I: Integer;
begin
  Result := 0;
  ListBox.Items.BeginUpdate;
  try
    for I := 0 to ListBox.Items.Count - 1 do
      if List.IndexOf(ListBox.Items[I]) > -1 then
      begin
        ListBox.Selected[I] := True;
        Inc(Result);
      end
      else
        ListBox.Selected[I] := False;
    if ListBox.Items.Count > 0 then
    begin
      ListBox.ItemIndex := 0;
      ListBox.TopIndex := 0;
    end;
  finally
    ListBox.Items.EndUpdate;
  end;
end;

procedure SelectAll(ListBox: TListBox);
var
  I: Integer;
begin
  ListBox.Items.BeginUpdate;
  try
    with ListBox do
      for I := 0 to Items.Count - 1 do
        Selected[I] := True;
    if ListBox.Items.Count > 0 then
    begin
      ListBox.ItemIndex := 0;
      ListBox.TopIndex := 0;
    end;
  finally
    ListBox.Items.EndUpdate;
  end;
end;

procedure GetDataFieldNames(Dataset: TDataset; ErrorName: string; List: TStrings);
var
  I: Integer;
begin
  with Dataset do
  try
    FieldDefs.Update;
    List.BeginUpdate;
    try
      List.Clear;
      for I := 0 to FieldDefs.Count - 1 do
        List.Add(FieldDefs[I].Name);
    finally
      List.EndUpdate;
    end;
  except
    if ErrorName <> '' then
      MessageDlg(Format(SSQLDataSetOpen, [ErrorName]), mtError, [mbOK], 0);
  end;
end;

procedure GetSQLTableNames(const SQL: string; List: TStrings);
begin
  with TSQLParser.Create(SQL) do
  try
    GetSelectTableNames(List);
  finally
    Free;
  end;
end;

procedure ParseUpdateSQL(const SQL: string; var TableName: string;
  UpdateFields: TStrings; WhereFields: TStrings);
begin
  with TSQLParser.Create(SQL) do
  try
    GetUpdateTableName(TableName);
    if Assigned(UpdateFields) then
    begin
      Reset;
      GetUpdateFields(UpdateFields);
    end;
    if Assigned(WhereFields) then
    begin
      Reset;
      GetWhereFields(WhereFields);
    end;
  finally
    Free;
  end;
end;

  { TUpdateSQLEditor }

procedure TAOraUpdateSQLEditor.ExecuteVerb(Index: Integer);
begin
  if EditUpdateSQL(TAOraUpdateSQL(Component)) then Designer.Modified;
end;

function TAOraUpdateSQLEditor.GetVerb(Index: Integer): string;
begin
  if Component=nil
   then Result := Format(SUpdateSQLEditor,['AOraUpdateSQL'])
   else Result := Format(SUpdateSQLEditor,[(Component as TAOraUpdateSQL).Name]);
end;

function TAOraUpdateSQLEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TSQLParser }

constructor TSQLParser.Create(const Text: string);
begin
  FText := Text;
  FSourcePtr := PChar(Text);
  NextToken;
end;

function TSQLParser.NextToken: TSQLToken;
var
  P, TokenStart: PChar;
  QuoteChar: Char;
  IsParam: Boolean;

  function IsKatakana(const Chr: Byte): Boolean;
  begin
    Result := (SysLocale.PriLangID = LANG_JAPANESE) and (Chr in [$A1..$DF]);
  end;

begin
  if FToken = stEnd then SysUtils.Abort;
  FTokenString := '';
  FSymbolQuoted := False;
  P := FSourcePtr;
  while (P^ <> #0) and (P^ <= ' ') do Inc(P);
  FTokenPtr := P;
  case P^ of
    'A'..'Z', 'a'..'z', '_', '$', #127..#255:
      begin
        TokenStart := P;
        if not SysLocale.FarEast then
        begin
          Inc(P);
          while P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '"', '$', #127..#255] do Inc(P);
        end
        else
          begin
            while TRUE do
            begin
              if (P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '"', '$']) or
                 IsKatakana(Byte(P^)) then
                Inc(P)
              else
                if P^ in LeadBytes then
                  Inc(P, 2)
                else
                  Break;
            end;
          end;
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := stSymbol;
      end;
    '''', '"':
      begin
        QuoteChar := P^;
        Inc(P);
        IsParam := P^ = ':';
        if IsParam then Inc(P);
        TokenStart := P;
        while not (P^ in [QuoteChar, #0]) do Inc(P);
        SetString(FTokenString, TokenStart, P - TokenStart);
        Inc(P);
        Trim(FTokenString);
        FToken := stSymbol;
        FSymbolQuoted := True;
      end;
    '-', '0'..'9':
      begin
        TokenStart := P;
        Inc(P);
        while P^ in ['0'..'9', '.', 'e', 'E', '+', '-'] do Inc(P);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := stNumber;
      end;
    ',':
      begin
        Inc(P);
        FToken := stComma;
      end;
    '=':
      begin
        Inc(P);
        FToken := stEQ;
      end;
    '(':
      begin
        Inc(P);
        FToken := stLParen;
      end;
    ')':
      begin
        Inc(P);
        FToken := stRParen;
      end;
    #0:
      FToken := stEnd;
  else
    begin
      FToken := stOther;
      Inc(P);
    end;
  end;
  FSourcePtr := P;
  if (FToken = stSymbol) and
    (FTokenString[Length(FTokenString)] = '.') then FToken := stAlias;
  Result := FToken;
end;

procedure TSQLParser.Reset;
begin
  FSourcePtr := PChar(FText);
  FToken := stSymbol;
  NextToken;
end;

function TSQLParser.TokenSymbolIs(const S: string): Boolean;
begin
  Result := (FToken = stSymbol) and (CompareText(FTokenString, S) = 0);
end;

procedure TSQLParser.GetSelectTableNames(List: TStrings);
begin
  List.BeginUpdate;
  try
    List.Clear;
    if TokenSymbolIs('SELECT') then { Do not localize }
    try
      while not TokenSymbolIs('FROM') do NextToken; { Do not localize }
      NextToken;
      while FToken = stSymbol do
      begin
        List.AddObject(FTokenString, Pointer(Integer(FSymbolQuoted)));
        if NextToken = stSymbol then NextToken;
        if FToken = stComma then NextToken
        else break;
      end;
    except
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TSQLParser.GetUpdateTableName(var TableName: string);
begin
  if TokenSymbolIs('UPDATE') and (NextToken = stSymbol) then { Do not localize }
    TableName := FTokenString else
    TableName := '';
end;

procedure TSQLParser.GetUpdateFields(List: TStrings);
begin
  List.BeginUpdate;
  try
    List.Clear;
    if TokenSymbolIs('UPDATE') then { Do not localize }
    try
      while not TokenSymbolIs('SET') do NextToken; { Do not localize }
      NextToken;
      while True do
      begin
        if FToken = stAlias then NextToken;
        if FToken <> stSymbol then Break;
        List.Add(FTokenString);
        if NextToken <> stEQ then Break;
        while NextToken <> stComma do
          if TokenSymbolIs('WHERE') then Exit;{ Do not localize }
        NextToken;
      end;
    except
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TSQLParser.GetWhereFields(List: TStrings);
begin
  List.BeginUpdate;
  try
    List.Clear;
    if TokenSymbolIs('UPDATE') then { Do not localize }
    try
      while not TokenSymbolIs('WHERE') do NextToken; { Do not localize }
      NextToken;
      while True do
      begin
        while FToken in [stLParen, stAlias] do NextToken;
        if FToken <> stSymbol then Break;
        List.Add(FTokenString);
        if NextToken <> stEQ then Break;
        while true do
        begin
          NextToken;
          if FToken = stEnd then Exit;
          if TokenSymbolIs('AND') then Break; { Do not localize }
        end;
        NextToken;
      end;
    except
    end;
  finally
    List.EndUpdate;
  end;
end;

{ TUpdateSQLEditor }

{ Private Methods }

{function TAOraUpdateSQLEditForm.DatabaseOpen: Boolean;
begin
  if Assigned(Database) then
    Result := True
  else
  begin
    Result := False;
    if not Assigned(DataSet) then Exit;
    if Assigned(DataSet.Database) then
    begin
      Database := TOraDB(DataSet.Database);
      Result := True;
    end
{    else
    begin
      Database := DataSet.OpenDatabase;
      DatabaseOpened := True;
      Result := True;
    end;}
{  end;
end;}

function TAOraUpdateSQLEditForm.Edit: Boolean;
var
  Index: TUpdateKind;
  DataSetName: string;
begin
  Result := False;
  try
    FTempSQL:=TOraSQL.Create(Self);

    if Assigned(UpdateSQL.DataSet){ and (UpdateSQL.DataSet is TOraSQL)} then
    begin
     DataSet := TOraSQL(UpdateSQL.DataSet);
//    FTempTable.SessionName := DataSet.SessionName;
     FTempSQL.Database := DataSet.Database;
     if DataSet.Database=nil
       then DatabaseSelfOpen := false
       else DatabaseSelfOpen := not DataSet.Database.Active;
     DataSetName := Format('%s.%s', [DataSet.Owner.Name, DataSet.Name]);
    end else
     DataSetName := SNoDataSet;

    Caption := Format('%s.%s (%s)', [UpdateSQL.Owner.Name, UpdateSQL.Name, DataSetName]);

    for Index := Low(TUpdateKind) to High(TUpdateKind) do
    begin
      SQLText[Index] := TStringList.Create;
      SQLText[Index].Assign(UpdateSQL.SQL[Index]);
    end;

    StatementTypeClick(Self);
    InitUpdateTableNames;
    ShowWait(InitGenerateOptions);
    PageControl.ActivePage := PageControl.Pages[0];
    if ShowModal = mrOk then
    begin
      for Index := low(TUpdateKind) to high(TUpdateKind) do
        UpdateSQL.SQL[Index] := SQLText[Index];
      Result := True;
    end;
  finally
    FTempSQL.Free;
    for Index := Low(TUpdateKind) to High(TUpdateKind) do
      SQLText[Index].Free;
  end;
end;

procedure TAOraUpdateSQLEditForm.GenWhereClause(const TabAlias, QuoteChar: string;
  KeyFields, SQL: TStrings);
var
  I: Integer;
  BindText: string;
  FieldName: string;
begin
  SQL.Add('where'); { Do not localize }
  for I := 0 to KeyFields.Count - 1 do
  begin
    FieldName := KeyFields[I];
    BindText := Format('  %s%s%s%1:s = :%1:sOLD_%2:s%1:s', { Do not localize }
      [TabAlias, QuoteChar, FieldName]);
    if I < KeyFields.Count - 1 then
      BindText := Format('%s and',[BindText]); { Do not localize }
    SQL.Add(BindText);
  end;
end;

procedure TAOraUpdateSQLEditForm.GenDeleteSQL(const TableName, QuoteChar: string;
  KeyFields, SQL: TStrings);
begin
  SQL.Clear;
  SQL.Add(Format('delete from %s', [TableName])); { Do not localize }
  GenWhereClause(GetTableRef(TableName, QuoteChar), QuoteChar, KeyFields, SQL);
end;

procedure TAOraUpdateSQLEditForm.GenInsertSQL(const TableName, QuoteChar: string;
  UpdateFields, SQL: TStrings);

  procedure GenFieldList(const TabName, ParamChar, QuoteChar: String);
  var
    L: string;
    I: integer;
    Comma: string;
  begin
    L := '  (';
    Comma := ', ';
    for I := 0 to UpdateFields.Count - 1 do
    begin
      if I = UpdateFields.Count - 1 then Comma := '';
      L := Format('%s%s%s%s%s%3:s%5:s',
        [L, TabName, ParamChar, QuoteChar, UpdateFields[I], Comma]);
      if (Length(L) > 70) and (I <> UpdateFields.Count - 1) then
      begin
        SQL.Add(L);
        L := '   ';
      end;
    end;
    SQL.Add(L+')');
  end;

begin
  SQL.Clear;
  SQL.Add(Format('insert into %s', [TableName])); { Do not localize }
  GenFieldList(GetTableRef(TableName, QuoteChar), '', QuoteChar);
  SQL.Add('values'); { Do not localize }
  GenFieldList('', ':', QuoteChar);
end;

procedure TAOraUpdateSQLEditForm.GenModifySQL(const TableName, QuoteChar: string;
  KeyFields, UpdateFields, SQL: TStrings);
var
  I: integer;
  Comma: string;
  TableRef: string;
begin
  SQL.Clear;
  SQL.Add(Format('update %s', [TableName]));  { Do not localize }
  SQL.Add('set');                             { Do not localize }
  Comma := ',';
  TableRef := GetTableRef(TableName, QuoteChar);
  for I := 0 to UpdateFields.Count - 1 do
  begin
    if I = UpdateFields.Count -1 then Comma := '';
    SQL.Add(Format('  %s%s%s%1:s = :%1:s%2:s%1:s%3:s',
      [TableRef, QuoteChar, UpdateFields[I], Comma]));
  end;
  GenWhereClause(TableRef, QuoteChar, KeyFields, SQL);
end;

procedure TAOraUpdateSQLEditForm.GenerateSQL;

  function QuotedTableName(const BaseName: string): string;
  begin
    with UpdateTableName do
      if ((ItemIndex <> -1) and (Items.Objects[ItemIndex] <> nil)) //or
         //(DatabaseOpen {and not Database.IsSQLBased} and (Pos('.', BaseName) > 0))
         then
         Result := Format('"%s"', [BaseName]) else
         Result := BaseName;
  end;

var
  KeyFields: TStringList;
  UpdateFields: TStringList;
  QuoteChar, TableName: string;
begin
  if (KeyFieldList.SelCount = 0) or (UpdateFieldList.SelCount = 0) then
    raise Exception.Create(SSQLGenSelect); // CreateRes(@SSQLGenSelect);
  KeyFields := TStringList.Create;
  try
    GetSelectedItems(KeyFieldList, KeyFields);
    UpdateFields := TStringList.Create;
    try
      GetSelectedItems(UpdateFieldList, UpdateFields);
      TableName := QuotedTableName(UpdateTableName.Text);
      if QuoteFields.Checked then
        QuoteChar := '"' else
        QuoteChar := '';
      GenDeleteSQL(TableName, QuoteChar, KeyFields, SQLText[ukDelete]);
      GenInsertSQL(TableName, QuoteChar, UpdateFields, SQLText[ukInsert]);
      GenModifySQL(TableName, QuoteChar, KeyFields, UpdateFields,
        SQLText[ukModify]);
      SQLMemo.Modified := False;
      StatementTypeClick(Self);
      PageControl.SelectNextPage(True);
    finally
      UpdateFields.Free;
    end;
  finally
    KeyFields.Free;
  end;
end;

procedure TAOraUpdateSQLEditForm.GetDataSetFieldNames;
begin
  if Assigned(DataSet) then
  begin
    GetDataFieldNames(DataSet, DataSet.Name, KeyFieldList.Items);
    UpdateFieldList.Items.Assign(KeyFieldList.Items);
  end;
end;

procedure TAOraUpdateSQLEditForm.GetTableFieldNames;
begin
  GetDataFieldNames(TempTable, UpdateTableName.Text {TempTable.TableName}, KeyFieldList.Items);
  UpdateFieldList.Items.Assign(KeyFieldList.Items);
  FDatasetDefaults := False;
end;

function TAOraUpdateSQLEditForm.GetTableRef(const TabName, QuoteChar: string): string;
begin
  if QuoteChar <> '' then
    Result :=  TabName + '.' else
    REsult := '';
end;

procedure TAOraUpdateSQLEditForm.InitGenerateOptions;
var
  UpdTabName: string;

  procedure InitFromDataSet;
  begin
    // If this is a Query with more than 1 table in the "from" clause then
    //  initialize the list of fields from the table rather than the dataset.
    if (UpdateTableName.Items.Count > 1) then
      GetTableFieldNames
    else
    begin
      GetDataSetFieldNames;
      FDatasetDefaults := True;
    end;
    SetDefaultSelections;
  end;

  procedure InitFromUpdateSQL;
  var
    UpdFields,
    WhFields: TStrings;
  begin
    UpdFields := TStringList.Create;
    try
      WhFields := TStringList.Create;
      try
        ParseUpdateSQL(SQLText[ukModify].Text, UpdTabName, UpdFields, WhFields);
        GetDataSetFieldNames;
        if SetSelectedItems(UpdateFieldList, UpdFields) < 1 then
          SelectAll(UpdateFieldList);
        if SetSelectedItems(KeyFieldList, WhFields) < 1 then
          SelectAll(KeyFieldList);
      finally
        WhFields.Free;
      end;
    finally
      UpdFields.Free;
    end;
  end;

begin
  // If there are existing update SQL statements, try to initialize the
  // dialog with the fields that correspond to them.
  if SQLText[ukModify].Count > 0 then
  begin
    ParseUpdateSQL(SQLText[ukModify].Text, UpdTabName, nil, nil);
    // If the table name from the update statement is not part of the
    // dataset, then initialize from the dataset instead.
    if (UpdateTableName.Items.Count > 0) and
       (UpdateTableName.Items.IndexOf(UpdTabName) > -1) then
    begin
      UpdateTableName.Text := UpdTabName;
      InitFromUpdateSQL;
    end else
    begin
      InitFromDataSet;
      UpdateTableName.Items.Add(UpdTabName);
    end;
  end else
    InitFromDataSet;
  SetButtonStates;
end;

procedure TAOraUpdateSQLEditForm.InitUpdateTableNames;
begin
  UpdateTableName.Items.Clear;
  if Assigned(DataSet) then
  begin
//    if DataSet is TQuery then
      GetSQLTableNames(TOraSQL(DataSet).SQL.Text, UpdateTableName.Items)
//    else if (DataSet is TTable) and (TTable(DataSet).TableName <> '') then
//      UpdateTableName.Items.Add(TTable(DataSet).TableName);
  end;
  if UpdateTableName.Items.Count > 0 then
     UpdateTableName.ItemIndex := 0;
end;

procedure TAOraUpdateSQLEditForm.SetButtonStates;
begin
  GetTableFieldsButton.Enabled := UpdateTableName.Text <> '';
  PrimaryKeyButton.Enabled := GetTableFieldsButton.Enabled and
    (KeyFieldList.Items.Count > 0);
  GenerateButton.Enabled := GetTableFieldsButton.Enabled and
    (UpdateFieldList.Items.Count > 0) and (KeyFieldList.Items.Count > 0);
  DefaultButton.Enabled := Assigned(DataSet) and not FDatasetDefaults;
end;

procedure TAOraUpdateSQLEditForm.SelectPrimaryKeyFields;
var
  SepPos, I, Index: Integer;
  FName, FieldNames: string;
begin
  if KeyFieldList.Items.Count < 1 then Exit;
{  with TempTable do
  begin
    IndexDefs.Update;
    for I := 0 to KeyFieldList.Items.Count - 1  do
      KeyFieldList.Selected[I] := False;
    for I := 0 to IndexDefs.Count - 1  do
      if ixPrimary in IndexDefs[I].Options then
      begin
        FieldNames := IndexDefs[I].Fields + ';';
        while Length(FieldNames) > 0 do
        begin
          SepPos := Pos(';', FieldNames);
          if SepPos < 1 then Break;
          FName := Copy(FieldNames, 1, SepPos - 1);
          System.Delete(FieldNames, 1, SepPos);
          Index := KeyFieldList.Items.IndexOf(FName);
          if Index > -1 then KeyFieldList.Selected[Index] := True;
        end;
        break;
      end;
  end;}
end;

procedure TAOraUpdateSQLEditForm.SetDefaultSelections;
var
  DSFields: TStringList;
begin
  if FDatasetDefaults or not Assigned(DataSet) then
  begin
    SelectAll(UpdateFieldList);
    SelectAll(KeyFieldList);
  end
  else if (DataSet.FieldDefs.Count > 0) then
  begin
    DSFields := TStringList.Create;
    try
      GetDataFieldNames(DataSet, '', DSFields);
      SetSelectedItems(KeyFieldList, DSFields);
      SetSelectedItems(UpdateFieldList, DSFields);
    finally
      DSFields.Free;
    end;
  end;
end;

procedure TAOraUpdateSQLEditForm.ShowWait(WaitMethod: TWaitMethod);
begin
  Screen.Cursor := crHourGlass;
  try
    WaitMethod;
  finally
    Screen.Cursor := crDefault;
  end;
end;

function TAOraUpdateSQLEditForm.TempTable: TOraSQL;
var s:string;
begin
  s:=Format('SELECT * FROM %s',[UpdateTableName.Text]);
  if FTempSQL.SQL.Text <> s then begin
    FTempSQL.Close;
    FTempSQL.SQL.Text := s;
  end;
  Result := FTempSQL;
{  if FTempTable.TableName <> UpdateTableName.Text then
  begin
    FTempTable.Close;
    FTempTable.TableName := UpdateTableName.Text;
  end;
  Result := FTempTable;}
end;

{ Event Handlers }

procedure TAOraUpdateSQLEditForm.FormCreate(Sender: TObject);
begin
  //HelpContext := hcDUpdateSQL;
end;

procedure TAOraUpdateSQLEditForm.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TAOraUpdateSQLEditForm.StatementTypeClick(Sender: TObject);
begin
  if SQLMemo.Modified then
    SQLText[TUpdateKind(StmtIndex)].Assign(SQLMemo.Lines);
  StmtIndex := StatementType.ItemIndex;
  SQLMemo.Lines.Assign(SQLText[TUpdateKind(StmtIndex)]);
end;

procedure TAOraUpdateSQLEditForm.OkButtonClick(Sender: TObject);
begin
  if SQLMemo.Modified then
    SQLText[TUpdateKind(StmtIndex)].Assign(SQLMemo.Lines);
end;

procedure TAOraUpdateSQLEditForm.DefaultButtonClick(Sender: TObject);
begin
  with UpdateTableName do
    if Items.Count > 0 then ItemIndex := 0;
  ShowWait(GetDataSetFieldNames);
  FDatasetDefaults := True;
  SetDefaultSelections;
  KeyfieldList.SetFocus;
  SetButtonStates;
end;

procedure TAOraUpdateSQLEditForm.GenerateButtonClick(Sender: TObject);
begin
  GenerateSQL;
  FSettingsChanged := False;
end;

procedure TAOraUpdateSQLEditForm.PrimaryKeyButtonClick(Sender: TObject);
begin
  ShowWait(SelectPrimaryKeyFields);
  SettingsChanged(Sender);
end;

procedure TAOraUpdateSQLEditForm.PageControlChanging(Sender: TObject;
  var AllowChange: Boolean);
begin                  
  if (PageControl.ActivePage = PageControl.Pages[0]) and
    not SQLPage.Enabled then
    AllowChange := False;
end;

procedure TAOraUpdateSQLEditForm.FormDestroy(Sender: TObject);
begin
//  if DatabaseOpened then
//    Database.Session.CloseDatabase(Database);
// if Dataset.Database<>nil then showmessage('database<>nil');
 if (DatabaseSelfOpen)and(DataSet.Database<>nil) then
   DataSet.Database.Close;
end;

procedure TAOraUpdateSQLEditForm.GetTableFieldsButtonClick(Sender: TObject);
begin
  ShowWait(GetTableFieldNames);
  SetDefaultSelections;
  SettingsChanged(Sender);
end;

procedure TAOraUpdateSQLEditForm.SettingsChanged(Sender: TObject);
begin
  FSettingsChanged := True;
  FDatasetDefaults := False;
  SetButtonStates;
end;

procedure TAOraUpdateSQLEditForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (ModalResult = mrOK) and FSettingsChanged then
    CanClose := MessageDlg(SSQLNotGenerated, mtConfirmation,
      mbYesNoCancel, 0) = mrYes;
end;

procedure TAOraUpdateSQLEditForm.UpdateTableNameChange(Sender: TObject);
begin
  SettingsChanged(Sender);
end;

procedure TAOraUpdateSQLEditForm.UpdateTableNameClick(Sender: TObject);
begin
  if not Visible then Exit;
  GetTableFieldsButtonClick(Sender);
end;

procedure TAOraUpdateSQLEditForm.SelectAllClick(Sender: TObject);
begin
  SelectAll(FieldListPopup.PopupComponent as TListBox);
end;

procedure TAOraUpdateSQLEditForm.ClearAllClick(Sender: TObject);
var
  I: Integer;
begin
  with FieldListPopup.PopupComponent as TListBox do
  begin
    Items.BeginUpdate;
    try
      for I := 0 to Items.Count - 1 do
        Selected[I] := False;
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TAOraUpdateSQLEditForm.SQLMemoKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #27 then Close;
end;

end.
