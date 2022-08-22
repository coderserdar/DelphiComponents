unit DBInfoFrm;
{$IFDEF CONDITIONALEXPRESSIONS}
 {$WARN SYMBOL_PLATFORM OFF}
 {$IF COMPILERVERSION > 17.0}
  {$DEFINE EXT_WIDESTR_SUPP}
 {$IFEND}
 {$IF COMPILERVERSION >= 23.0}
  {$DEFINE XE2_UP}
 {$IFEND}
{$ENDIF}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, ComCtrls,
  Db, TinyDB, ImgList, Menus, BaseFrm;

type
  TNodeType = (ntTable, ntField, ntIndex);
  PNodeData = ^TNodeData;
  TNodeData = record
    NodeType: TNodeType;
    TableName: string;
    SelfName: string;
  end;

  TDBInfoForm = class(TBaseForm)
    DBStructTreeView: TTreeView;
    TinyTable: TTinyTable;
    ImageList: TImageList;
    TablePopupMenu: TPopupMenu;
    TableMenuOpenItem: TMenuItem;
    TableMenuDeleteItem: TMenuItem;
    TableMenuRenameItem: TMenuItem;
    TableMenuN1: TMenuItem;
    TableMenuRefreshItem: TMenuItem;
    TableMenuN2: TMenuItem;
    TableMenuNewItem: TMenuItem;
    FieldPopupMenu: TPopupMenu;
    FieldMenuRenameItem: TMenuItem;
    FieldMenuN1: TMenuItem;
    FieldMenuRefreshItem: TMenuItem;
    FieldMenuN2: TMenuItem;
    FieldMenuNewItem: TMenuItem;
    IndexPopupMenu: TPopupMenu;
    IndexMenuRenameItem: TMenuItem;
    IndexMenuN1: TMenuItem;
    IndexMenuRefreshItem: TMenuItem;
    IndexMenuN2: TMenuItem;
    IndexMenuNewItem: TMenuItem;
    IndexMenuDeleteItem: TMenuItem;
    TableMenuEmptyItem: TMenuItem;
    FieldMenuPropItem: TMenuItem;
    IndexMenuPropItem: TMenuItem;
    TableMenuDesignItem: TMenuItem;
    Exportdata1: TMenuItem;
    Importdata1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DBStructTreeViewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TableMenuOpenItemClick(Sender: TObject);
    procedure TableMenuRenameItemClick(Sender: TObject);
    procedure TableMenuRefreshItemClick(Sender: TObject);
    procedure TableMenuNewItemClick(Sender: TObject);
    procedure TableMenuDeleteItemClick(Sender: TObject);
    procedure TableMenuEmptyItemClick(Sender: TObject);
    procedure IndexMenuDeleteItemClick(Sender: TObject);
    procedure IndexMenuRenameItemClick(Sender: TObject);
    procedure DBStructTreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure DBStructTreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure FieldMenuRenameItemClick(Sender: TObject);
    procedure FieldMenuPropItemClick(Sender: TObject);
    procedure IndexMenuPropItemClick(Sender: TObject);
    procedure DBStructTreeViewDblClick(Sender: TObject);
    procedure TableMenuDesignItemClick(Sender: TObject);
    procedure Exportdata1Click(Sender: TObject);
    procedure Importdata1Click(Sender: TObject);
  private
    { Private declarations }
    FTinyDatabase: TTinyDatabase;
    FCurNodeData: TNodeData;
    FCurNode: TTreeNode;

    procedure FillDBStructToTreeView;
    procedure AddNodeData(TreeNode: TTreeNode; NodeType: TNodeType; TableName, SelfName: string);
    procedure FreeNodeData(TreeNode: TTreeNode);
    procedure ClearNodeData;
    function GetNodeData(TreeNode: TTreeNode): TNodeData;
  public
    { Public declarations }
    procedure SetData(ATinyDB: TTinyDatabase);
    procedure InvokeNewTableForm;
    procedure RefreshDBInfo;
    procedure OpenTable(TableName: string);
    procedure DeleteTable(TableName: string);
    procedure EmptyTable(TableName: string);
    procedure DesignTable(TableName: string);
    procedure LocateNodeOfTableName(TableName: string);
    procedure DeleteIndex(TableName, IndexName: string);
    procedure RenameTable(OldTableName, NewTableName: string);
    procedure RenameField(TableName, OldFieldName, NewFieldName: string);
    procedure RenameIndex(TableName, OldIndexName, NewIndexName: string);
    procedure RefreshFormOfTableName(TableName: string);
    procedure CloseFormOfTableName(TableName: string);
    procedure CloseAllTable;
    procedure ExportDBToText(FileName: string);
  end;

var
  DBInfoForm: TDBInfoForm;

//==============================================================================
// TPgCSV component V 1.31
// Khashayar Sadjadi (khashi@pragena.8m.com)
// Pragena's Delphi Home (http://pragena.8m.com)
// All rights reserved
// (c) 2012 Jaro.Benes - adapt into TinyDB framework - DBD.exe
//                       TinyTable modifications
//==============================================================================
type
  {  TDB2CSVErrorResponse  }

  TDB2CSVErrorResponse = (db2csvAbort, db2csvIgnore);

  {  TDB2CSVProgressEvent  }

  TDB2CSVProgressEvent = procedure(Sender: TObject; AProgress: LongInt; var StopIt: Boolean) of object;

  {  TDB2CSVExportErrorEvent  }

  TDB2CSVExportErrorEvent = procedure(Sender: TObject; Mess: string; RecNo: LongInt; var Response: TDB2CSVErrorResponse) of object;

  {  TDB2CSV  }

  TDB2CSV = class(TComponent)
  private
    FTinyTable: TTinyTable;
    FCSVMap: string;
    FCSVFile: string;
    FDateFormat: string;
    FIgnoreStr: string;
    FSeprator: Char;
    FDelimiter: Char;
    FFieldIndicator: Char;
    FAutoOpen: Boolean;
    FUseDelimiter: Boolean;
    FSilentExport: Boolean;
    FTrimData: Boolean;
    FStop: Boolean;
    FEmptyTable: Boolean;
    FBeforeOpenTable: TNotifyEvent;
    FAfterOpenTable: TNotifyEvent;
    FBeforeCloseTable: TNotifyEvent;
    FAfterCloseTable: TNotifyEvent;
    FBeforeEmptyTable: TNotifyEvent;
    FAfterEmptyTable: TNotifyEvent;
    FBeforeExport: TNotifyEvent;
    FAfterExport: TNotifyEvent;
    FBeforeImport: TNotifyEvent;
    FAfterImport: TNotifyEvent;
    FOnAddRecord: TNotifyEvent;
    FExportProgress: TDB2CSVProgressEvent;
    FImportProgress: TDB2CSVProgressEvent;
    FExportError: TDB2CSVExportErrorEvent;
    FMapItems: Integer;
    FDefaultInt: Integer;
    FBufferSize: LongInt;
  protected
    FFile: TextFile;
    function CountMapItems: Integer;
    function GetMapItem(FieldCache: TStringList; ItemIndex: Integer; var AField: Boolean): string;
    function GetCSVRecordItem(ItemIndex: Integer; CSVRecord: string): string;
    function BuildMap: string;
    function ExtractWord(Item: Integer; S, WordDelim: string): string;
    function WordCount(const S, WordDelim: string): Integer;
    function WordPosition(Item: Integer; const S, SubStr: string): Integer;
  public
    constructor Create(AOwner: TComponent); override;
  published
    //properties
    property TinyTable: TTinyTable read FTinyTable write FTinyTable;
    property CSVMap: string read FCSVMap write FCSVMap;
    property CSVFile: string read FCSVFile write FCSVFile;
    property Seprator: Char read FSeprator write FSeprator;
    property FieldIndicator: Char read FFieldIndicator write FFieldIndicator;
    property AutoOpen: Boolean read FAutoOpen write FAutoOpen;
    property IgnoreString: string read FIgnoreStr write FIgnoreStr;
    property Delimiter: Char read FDelimiter write FDelimiter;
    property EmptyTable: Boolean read FEmptyTable write FEmptyTable;
    property UseDelimiter: Boolean read FUseDelimiter write FUseDelimiter;
    property SilentExport: Boolean read FSilentExport write FSilentExport;
    property DateFormat: string read FDateFormat write FDateFormat;
    property TrimData: Boolean read FTrimData write FTrimData;
    property DefaultInt: Integer read FDefaultInt write FDefaultInt;
    property BufferSize: LongInt read FBufferSize write FBufferSize;
    //events
    property BeforeOpenTable: TNotifyEvent read FBeforeOpenTable write FBeforeOpenTable;
    property AfterOpenTable: TNotifyEvent read FAfterOpenTable write FAfterOpenTable;
    property BeforeCloseTable: TNotifyEvent read FBeforeCloseTable write FBeforeCloseTable;
    property AfterCloseTable: TNotifyEvent read FAfterCloseTable write FAfterCloseTable;
    property BeforeEmptyTable: TNotifyEvent read FBeforeEmptyTable write FBeforeEmptyTable;
    property AfterEmptyTable: TNotifyEvent read FAfterEmptyTable write FAfterEmptyTable;
    property BeforeImport: TNotifyEvent read FBeforeImport write FBeforeImport;
    property AfterImport: TNotifyEvent read FAfterImport write FAfterImport;
    property BeforeExport: TNotifyEvent read FBeforeExport write FBeforeExport;
    property AfterExport: TNotifyEvent read FAfterExport write FAfterExport;
    property ExportProgress: TDB2CSVProgressEvent read FExportProgress write FExportProgress;
    property ImportProgress: TDB2CSVProgressEvent read FImportProgress write FImportProgress;
    property OnAddRecord: TNotifyEvent read FOnAddRecord write FOnAddRecord;
    property ExportError: TDB2CSVExportErrorEvent read FExportError write FExportError;
    //methodes
    function CSVToDataset: Boolean;
    function DatasetToCSV: Boolean;
  end;

procedure ShowDBInfoForm(ATinyDB: TTinyDatabase);

implementation

uses
  LangMgr, MainFrm, DBTableFrm, DBGridFrm, DBCardFrm, NewTableFrm,
  FieldPropFrm, IndexPropFrm, TableListFrm;

{$R *.DFM}

{ support routines by JB.}

function StringToHexStr(const Data: AnsiString): AnsiString;
begin //JB.
  SetLength(Result, Length(Data) * 2); // misto
  if Length(Data) > 0 then
    BinToHex(PAnsiChar(Data), PAnsiChar(Result), Length(Data));
end;

function HexStrToString(const Value: AnsiString): AnsiString;
begin //JB.
  SetLength(Result, Length(Value) div 2); // misto
  if Length(Value) > 0 then
    HexToBin(PAnsiChar(Value), PAnsiChar(Result), Length(Value));
end;

{  TDB2CSV  }

function TDB2CSV.CSVToDataset: Boolean;
{-------------------------------------------------------------------------------
  Procedure: TDB2CSV.CSVToDataset
  Author:
  Adapted:   Jaro Benes
  DateTime:  2013.07.01
  Arguments: None
  Result:    Boolean
  Purpose:   convert csv file into dataset with blob accepts
-------------------------------------------------------------------------------}
var
  RecordString, Temp: string;
  S: string;
  blob: AnsiString;
  i: Integer;
  C: LongInt;
  D: Boolean;
  F: Real;
  DT: TDateTime;
  ErrorResponse: TDB2CSVErrorResponse;
  //Buffer: Pointer;
  formatSettings: TFormatSettings;
  FFieldCache: TStringList;
begin
  Result := True;
  try
    //must be the same locale as PC
    {$IFDEF XE2_UP}
    formatSettings := TFormatSettings.Create(LOCALE_SYSTEM_DEFAULT);
    {$ELSE}
    GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, formatSettings);
    {$ENDIF}
    //create field cache
    FFieldCache := TStringList.Create;
    try
      //initiate map items
      FMapItems := 0;
      //allocate buffer size
      //GetMem(Buffer, FBufferSize);
      //try
       //assign and open CSV file
      AssignFile(FFile, FCSVFile);
        //SetTextBuf(FFile, Buffer^, FBufferSize);
      Reset(FFile);
      try
          //open table if nessecary
        if FAutoOpen then
        begin
          if Assigned(FBeforeOpenTable) then
            FBeforeOpenTable(Self);
          FTinyTable.Open;
          if Assigned(FAfterOpenTable) then
            FAfterOpenTable(Self);
        end;
        if FEmptyTable then
        begin
          if Assigned(FBeforeEmptyTable) then
            FBeforeEmptyTable(Self);
          if not FTinyTable.IsEmpty then
          begin
            FTinyTable.EmptyTable;
          end;
          if Assigned(FAfterEmptyTable) then
            FAfterEmptyTable(Self);
        end;
          //read map as first line of the file
          //can be ignored and map rebuild
        ReadLn(FFile, FCSVMap);
          //export to table from CSV file
        if Assigned(FBeforeExport) then
          FBeforeExport(Self);
          //set the counter to zero
        C := 0;
        Temp := {$IFDEF XE2_UP}FormatSettings.{$ENDIF}ShortDateFormat;
        {$IFDEF XE2_UP}FormatSettings.{$ENDIF}ShortDateFormat := FDateFormat;
        FTinyTable.DisableControls;
        try
          while (not Eof(FFile)) and (not FStop) do
          begin
            //read from CSV
            ReadLn(FFile, RecordString);
            //add new record
            try
              FTinyTable.Append;
              for i := 1 to CountMapItems do
                if UpperCase(GetMapItem(FFieldCache, i, D)) <> UpperCase(FIgnoreStr) then
                  case FTinyTable.FieldByName(GetMapItem(FFieldCache, i, D)).DataType of
                    ftDateTime, ftDate, ftTime:
                      begin
                        S := Trim(GetCSVRecordItem(i, RecordString));
                        if TryStrToDateTime(S, DT, formatSettings) then
                          FTinyTable.FieldByName(GetMapItem(FFieldCache, i, D)).AsDateTime := DT;
                      end;
                    ftInteger:
                      FTinyTable.FieldByName(GetMapItem(FFieldCache, i, D)).AsInteger := StrToIntDef(Trim(GetCSVRecordItem(i, RecordString)), FDefaultInt);
                    ftFloat:
                      begin
                        try
                          F := StrToFloat(Trim(GetCSVRecordItem(i, RecordString)));
                        except
                          F := FDefaultInt;
                        end;
                        FTinyTable.FieldByName(GetMapItem(FFieldCache, i, D)).AsFloat := F;
                      end;
                    ftBlob:
                      begin
                        blob := HexStrToString(AnsiString(GetCSVRecordItem(i, RecordString)));
                        FTinyTable.FieldByName(GetMapItem(FFieldCache, i, D)).{$IFDEF UNICODE}AsAnsiString{$ELSE}AsString{$ENDIF} := blob;
                      end;
                  else
                    if FTrimData then
                      FTinyTable.FieldByName(GetMapItem(FFieldCache, i, D)).AsString := Trim(GetCSVRecordItem(i, RecordString))
                    else
                      FTinyTable.FieldByName(GetMapItem(FFieldCache, i, D)).AsString := GetCSVRecordItem(i, RecordString);
                  end;
              //post record
              FTinyTable.Post;
            except
              on E: Exception do
                if not FSilentExport then
                begin
                  //ShowMessage(E.Message);
                  if Pos('Invalid unique field value', E.Message) = 1 then
                    FTinyTable.Cancel
                  else
                    raise
                end
                else if Assigned(FExportError) then
                begin
                  FExportError(Self, E.Message, C, ErrorResponse);
                  if ErrorResponse = db2csvAbort then
                    Break;
                end
                else
                  if Pos('Invalid unique field value', E.Message) = 1 then
                    FTinyTable.Cancel
                  else
                    raise;
            end;
            if Assigned(FOnAddRecord) then
              FOnAddRecord(Self);
            if Assigned(FExportProgress) then
              FExportProgress(Self, C, FStop);
            Inc(C);
          end;
        finally
          FTinyTable.EnableControls;
          {$IFDEF XE2_UP}FormatSettings.{$ENDIF}ShortDateFormat := Temp;
        end;
        if Assigned(FAfterExport) then
          FAfterExport(Self);
         //close table if nessecary
        if FAutoOpen then
        begin
          if Assigned(FBeforeCloseTable) then
            FBeforeCloseTable(Self);
          FTinyTable.Close;
          if Assigned(FAfterCloseTable) then
            FAfterCloseTable(Self);
        end;
      finally
          //close CSV file
        CloseFile(FFile);
      end;
//      finally
//        //disallocate buffer
//        FreeMem(Buffer);
//      end;
    finally
      //free cache
//      for i := FFieldCache.Count - 1 downto 0 do
//        Dispose(FFieldCache.Items[i]);
      FFieldCache.Free;
    end;
  except
    Result := False;
  end;
end;

function TDB2CSV.BuildMap: string;
var
  i: Integer;
  S: string;
begin
  S := '';
  for i := 0 to FTinyTable.FieldCount - 1 do
    S := S + FFieldIndicator + FTinyTable.Fields[i].FieldName + FSeprator;
  Delete(S, Length(S), 1);
  Result := S;
end;

function TDB2CSV.DatasetToCSV: Boolean;
{-------------------------------------------------------------------------------
  Procedure: TDB2CSV.DatasetToCSV
  Author:
  Adapted:   Jaro Benes
  DateTime:  2013.07.01
  Arguments: None
  Result:    Boolean
  Purpose:   convert dataset into csv file with blob accepts
-------------------------------------------------------------------------------}

var
  S, D, T: string;
  i: Integer;
  C: LongInt;
  B: Boolean;
  DD: TDateTime;
  //Buffer: Pointer;
  formatSettings: TFormatSettings;
  FFieldCache: TStringList;
begin
  Result := True;
  try
    //must be the same locale as PC
    {$IFDEF XE2_UP}
    formatSettings := TFormatSettings.Create(LOCALE_SYSTEM_DEFAULT);
    {$ELSE}
    GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, formatSettings);
    {$ENDIF}
    //create field cache
    FFieldCache := TStringList.Create;
    try
      //initiate map items
      FMapItems := 0;
      //allocate buffer
//      GetMem(Buffer, FBufferSize);
//      try
        //assign and open CSV file
      AssignFile(FFile, FCSVFile);
        //SetTextBuf(FFile, Buffer^, FBufferSize);
      System.ReWrite(FFile);
      try
          //empty CSV if nessecary
        if FEmptyTable then
        begin
          if Assigned(FBeforeEmptyTable) then
            FBeforeEmptyTable(Self);
          if Assigned(FAfterEmptyTable) then
            FAfterEmptyTable(Self);
        end;

          //open table if nessecary
        if FAutoOpen then
        begin
          if Assigned(FBeforeOpenTable) then
            FBeforeOpenTable(Self);
          FTinyTable.Open;
          if Assigned(FAfterOpenTable) then
            FAfterOpenTable(Self);
        end;
          //import from table to CSV file
        if Assigned(FBeforeImport) then
          FBeforeImport(Self);
        C := 0;
        FTinyTable.First;
        if Trim(FCSVMap) = '' then
          FCSVMap := BuildMap;
        WriteLn(FFile, FCSVMap);
        FTinyTable.DisableControls;
        try
          while (not FTinyTable.Eof) and (not FStop) do
          begin
            S := '';
            for i := 1 to CountMapItems do
            begin
              D := GetMapItem(FFieldCache, i, B);
              if B then //is it a field definition
              begin
                if FTinyTable.FieldByName(D).IsBlob then
                  if FTinyTable.FieldByName(D).IsNull then
                    T := ''
                  else
                    T := string(StringToHexStr(FTinyTable.FieldByName(D).{$IFDEF UNICODE}AsAnsiString{$ELSE}AsString{$ENDIF}))
                else
                begin
                  case FTinyTable.FieldByName(D).DataType of
                    ftDateTime, ftDate, ftTime:
                      begin
                        DD := FTinyTable.FieldByName(D).AsDateTime;
                        if DD = 0 then
                          T := ''
                        else if (Trunc(DD) = 0) and (DD > 0.0) then
                          T := TimeToStr(DD, formatSettings)
                        else
                          T := DateTimeToStr(DD, formatSettings);
                      end;
                  else
                    if FTrimData then
                      T := Trim(FTinyTable.FieldByName(D).AsString)
                    else
                      T := FTinyTable.FieldByName(D).AsString;
                  end;
                end;
              end
              else
                T := D;
              if FUseDelimiter then
                T := FDelimiter + T + FDelimiter;
              S := S + T + FSeprator;
            end;
              //delete last seprator
            Delete(S, Length(S), 1);
            WriteLn(FFile, S);
            FTinyTable.Next;
              //call progress event
            if Assigned(FImportProgress) then
              FImportProgress(Self, C, FStop);
            Inc(C);
          end;
        finally
          FTinyTable.EnableControls;
        end;

        if Assigned(FAfterImport) then
          FAfterImport(Self);
          //close table if nessecary
        if FAutoOpen then
        begin
          if Assigned(FBeforeCloseTable) then
            FBeforeCloseTable(Self);
          FTinyTable.Close;
          if Assigned(FAfterCloseTable) then
            FAfterCloseTable(Self);
        end;
      finally
        //close CSV file
        CloseFile(FFile);
      end;
//      finally
//        //free buffer
//        FreeMem(Buffer);
//      end;
    finally
      //free field cache
      FFieldCache.Free;
    end;
  except
    Result := False;
  end;
end;

//internal methodes

function TDB2CSV.CountMapItems: Integer;
begin
  if FMapItems = 0 then
    FMapItems := WordCount(FCSVMap, FSeprator);
  Result := FMapItems;
end;

function TDB2CSV.GetMapItem(FieldCache: TStringList; ItemIndex: Integer; var AField: Boolean): string;
var
  S: string;
  //P: ^ShortString;
begin
  if FieldCache.Count < ItemIndex then
  begin
    S := ExtractWord(ItemIndex, FCSVMap, FSeprator);
    //New(P);
    //P^ := ShortString(S);
    FieldCache.Add(S);
  end
  else
    S := FieldCache[ItemIndex - 1];
  AField := True;
  if (Length(S) >= 1) and (S[1] = FFieldIndicator) then
    Result := Copy(S, 2, Length(S) - 1)
  else
  begin
    AField := False;
    Result := S;
  end;
end;

function TDB2CSV.GetCSVRecordItem(ItemIndex: Integer; CSVRecord: string): string;
var
  S: string;
begin
  if FUseDelimiter then
    S := ExtractWord(ItemIndex, CSVRecord, FDelimiter + FSeprator + FDelimiter)
  else
    S := ExtractWord(ItemIndex, CSVRecord, FSeprator);
  if (FUseDelimiter) then
  begin
    if (ItemIndex = 1) and (S[1] = FDelimiter) then
      Delete(S, 1, 1);
    if (ItemIndex = WordCount(CSVRecord, FDelimiter + FSeprator + FDelimiter)) and (S[Length(S)] = FDelimiter) then
      Delete(S, Length(S), 1);
  end;
  Result := S;
end;

function TDB2CSV.WordPosition(Item: Integer; const S, SubStr: string): Integer;
var
  i, Count: Integer;
begin
  Count := 0;
  Result := 0;
  for i := 1 to Length(S) do
  begin
    if Copy(S, i, Length(SubStr)) = SubStr then
      Inc(Count);
    if Count = Item then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TDB2CSV.ExtractWord(Item: Integer; S, WordDelim: string): string;
var
  First, Second: Integer;
begin
  First := WordPosition(Item - 1, S, WordDelim);
  Second := WordPosition(Item, S, WordDelim);
  if Second = 0 then
    Second := Length(S) + Length(WordDelim);
  if First = 1 then
    First := -Length(WordDelim);
  Result := Copy(S, First + Length(WordDelim), Second - (First + Length(WordDelim)));
  if Item = 1 then
    Delete(Result, Length(Result), 1);
end;

function TDB2CSV.WordCount(const S, WordDelim: string): Integer;
var
  i, Count: Integer;
begin
  Count := 0;
  for i := 1 to Length(S) do
    if Copy(S, i, Length(WordDelim)) = WordDelim then
      Inc(Count);
  Result := Count + 1;
end;

constructor TDB2CSV.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDelimiter := '"';
  FIgnoreStr := '(ignore)';
  FAutoOpen := True;
  FUseDelimiter := True;
  FSilentExport := False;
  FEmptyTable := False;
  FSeprator := ',';
  FFieldIndicator := '$';
  FDateFormat := 'MM/DD/YY';
  FBufferSize := 1024;
  FStop := False;
end;

procedure ShowDBInfoForm(ATinyDB: TTinyDatabase);
begin
  if DBInfoForm = nil then
  begin
    DBInfoForm := TDBInfoForm.Create(Application);
    DBInfoForm.ManualDock(MainForm.LeftDockPanel);
  end;
  DBInfoForm.CloseAllTable;
  DBInfoForm.SetData(ATinyDB);
  DBInfoForm.Show;
end;

{  TDBInfoForm  }

procedure TDBInfoForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  I: Integer;
begin
  if (HostDockSite is TPanel) then
    MainForm.ShowDockPanel(HostDockSite as TPanel, False, nil);

  MainForm.AdjustUI(False);
  for I := 0 to FTinyDatabase.TableDefs.Count - 1 do
    CloseFormOfTableName(FTinyDatabase.TableDefs[I].Name);
  ClearNodeData;
  FTinyDatabase.Close;
  Action := caFree;
  DBInfoForm := nil;
end;

procedure TDBInfoForm.SetData(ATinyDB: TTinyDatabase);
begin
  FTinyDatabase := ATinyDB;
  TinyTable.DatabaseName := FTinyDatabase.DatabaseName;
  FillDBStructToTreeView;
end;

procedure TDBInfoForm.InvokeNewTableForm;
begin
  if ShowNewTableForm then
    FillDBStructToTreeView;
end;

procedure TDBInfoForm.RefreshDBInfo;
begin
  FillDBStructToTreeView;
end;

procedure TDBInfoForm.OpenTable(TableName: string);
var
  Value: TDBTableFormData;
begin
  Value.TinyDatabase := FTinyDatabase;
  Value.TableName := TableName;
  case MainForm.OpenTableMode of
    otGrid:
      ShowDBGridForm(Value);
    otCard:
      ShowDBCardForm(Value);
  end;
  LocateNodeOfTableName(TableName);
end;

procedure TDBInfoForm.DeleteTable(TableName: string);
var
  R: Integer;
begin
  R := Application.MessageBox(PChar(AppLangMgr.Trans('Are you sure to delete?')), PChar(Application.Title), 36);
  if R = ID_YES then
  begin
    CloseFormOfTableName(TableName);
    FTinyDatabase.DeleteTable(TableName);
    FillDBStructToTreeView;
  end;
end;

procedure TDBInfoForm.EmptyTable(TableName: string);
var
  R: Integer;
begin
  R := Application.MessageBox(PChar(AppLangMgr.Trans('Are you sure to clear all records?')), PChar(Application.Title), 36);
  if R = ID_YES then
  begin
    TinyTable.TableName := TableName;
    TinyTable.Open;
    TinyTable.EmptyTable;
    TinyTable.Close;
    RefreshFormOfTableName(TableName);
  end;
end;

procedure TDBInfoForm.DesignTable(TableName: string);
begin
  CloseFormOfTableName(TableName);
  if ShowDesignTableForm(FTinyDatabase, TableName) then
    FillDBStructToTreeView;
end;

procedure TDBInfoForm.FillDBStructToTreeView;
const
  FieldsStr = 'Fields';
  IndexesStr = 'Indexes';
var
  I, J: Integer;
  TableNode, FieldNode, IndexNode, TempNode: TTreeNode;
  TableName, FieldName, IndexName: string;
begin
  TinyTable.Close;
  TinyTable.DatabaseName := FTinyDatabase.DatabaseName;
  DBStructTreeView.Items.BeginUpdate;
  ClearNodeData;
  DBStructTreeView.Items.Clear;
  for I := 0 to FTinyDatabase.TableDefs.Count - 1 do
  begin
    TableName := FTinyDatabase.TableDefs[I].Name;
    TableNode := DBStructTreeView.Items.AddChild(nil, TableName);
    TableNode.ImageIndex := 0;
    TableNode.SelectedIndex := 0;
    AddNodeData(TableNode, ntTable, TableName, TableName);

    FieldNode := DBStructTreeView.Items.AddChild(TableNode, FieldsStr);
    FieldNode.ImageIndex := 1;
    FieldNode.SelectedIndex := 1;
    FieldNode.Data := nil;

    IndexNode := DBStructTreeView.Items.AddChild(TableNode, IndexesStr);
    IndexNode.ImageIndex := 2;
    IndexNode.SelectedIndex := 2;
    IndexNode.Data := nil;

    TinyTable.TableName := TableName;
    TinyTable.Open;
    for J := 0 to TinyTable.FieldDefs.Count - 1 do
    begin
      FieldName := TinyTable.FieldDefs[J].Name;
      TempNode := DBStructTreeView.Items.AddChild(FieldNode, FieldName);
      TempNode.ImageIndex := 1;
      TempNode.SelectedIndex := 1;
      AddNodeData(TempNode, ntField, TableName, FieldName);
    end;
    for J := 0 to TinyTable.IndexDefs.Count - 1 do
    begin
      IndexName := TinyTable.IndexDefs[J].Name;
      TempNode := DBStructTreeView.Items.AddChild(IndexNode, IndexName);
      TempNode.ImageIndex := 2;
      TempNode.SelectedIndex := 2;
      AddNodeData(TempNode, ntIndex, TableName, IndexName);
    end;
    TinyTable.Close;
  end;
  DBStructTreeView.Items.EndUpdate;
  DBStructTreeView.FullExpand;
end;

procedure TDBInfoForm.AddNodeData(TreeNode: TTreeNode; NodeType: TNodeType; TableName, SelfName: string);
var
  NodeDataPtr: PNodeData;
begin
  New(NodeDataPtr);
  NodeDataPtr^.NodeType := NodeType;
  NodeDataPtr^.TableName := TableName;
  NodeDataPtr^.SelfName := SelfName;
  TreeNode.Data := NodeDataPtr;
end;

procedure TDBInfoForm.FreeNodeData(TreeNode: TTreeNode);
begin
  if TreeNode.Data = nil then Exit;
  Dispose(PNodeData(TreeNode.Data));
end;

procedure TDBInfoForm.ClearNodeData;
var
  I: Integer;
begin
  for I := 0 to DBStructTreeView.Items.Count - 1 do
    FreeNodeData(DBStructTreeView.Items[I]);
end;

function TDBInfoForm.GetNodeData(TreeNode: TTreeNode): TNodeData;
begin
  Result := PNodeData(TreeNode.Data)^;
end;

procedure TDBInfoForm.RefreshFormOfTableName(TableName: string);
var
  I: Integer;
  DBTableForm: TDBTableForm;
  DBTableForms: array of TDBTableForm;
begin
  for I := 0 to MainForm.MDIChildCount - 1 do
  begin
    if MainForm.MDIChildren[I] is TDBTableForm then
    begin
      DBTableForm := MainForm.MDIChildren[I] as TDBTableForm;
      if DBTableForm.TableName = TableName then
      begin
        SetLength(DBTableForms, Length(DBTableForms) + 1);
        DBTableForms[High(DBTableForms)] := DBTableForm;
      end;
    end;
  end;
  for I := 0 to High(DBTableForms) do
    DBTableForms[I].TinyTable.Refresh;
  SetLength(DBTableForms, 0);
end;

procedure TDBInfoForm.CloseFormOfTableName(TableName: string);
var
  I: Integer;
  DBTableForm: TDBTableForm;
  DBTableForms: array of TDBTableForm;
begin
  for I := 0 to MainForm.MDIChildCount - 1 do
  begin
    if MainForm.MDIChildren[I] is TDBTableForm then
    begin
      DBTableForm := MainForm.MDIChildren[I] as TDBTableForm;
      if DBTableForm.TableName = TableName then
      begin
        SetLength(DBTableForms, Length(DBTableForms) + 1);
        DBTableForms[High(DBTableForms)] := DBTableForm;
      end;
    end;
  end;
  for I := 0 to High(DBTableForms) do
    DBTableForms[I].Close;
  SetLength(DBTableForms, 0);
end;

procedure TDBInfoForm.CloseAllTable;
var
  I: Integer;
  DBTableForm: TDBTableForm;
  DBTableForms: array of TDBTableForm;
begin
  for I := 0 to MainForm.MDIChildCount - 1 do
  begin
    if MainForm.MDIChildren[I] is TDBTableForm then
    begin
      DBTableForm := MainForm.MDIChildren[I] as TDBTableForm;
      SetLength(DBTableForms, Length(DBTableForms) + 1);
      DBTableForms[High(DBTableForms)] := DBTableForm;
    end;
  end;
  for I := 0 to High(DBTableForms) do
    DBTableForms[I].Close;
  SetLength(DBTableForms, 0);
end;

procedure TDBInfoForm.ExportDBToText(FileName: string);
var
  TableIdx, RecordIdx, FieldIdx: Integer;
  Lines: TStrings;
  TinyTable: TTinyTable;
  S: string;
begin
  Lines := TStringList.Create;
  TinyTable := TTinyTable.Create(nil);
  MainForm.BeginProgress;
  try
    TinyTable.DatabaseName := FTinyDatabase.DatabaseName;
    for TableIdx := 0 to FTinyDatabase.TableDefs.Count - 1 do
    begin
      TinyTable.Close;
      TinyTable.TableName := FTinyDatabase.TableDefs[TableIdx].Name;
      TinyTable.Open;
      Lines.Add('Table Name: [' + TinyTable.TableName + ']');
      for RecordIdx := 0 to TinyTable.RecordCount - 1 do
      begin
        for FieldIdx := 0 to TinyTable.FieldDefs.Count - 1 do
        begin
          S := '[' + TinyTable.Fields[FieldIdx].FieldName + ']: ';
          if TinyTable.Fields[FieldIdx].IsBlob then
            S := S + string(StringToHexStr(TinyTable.Fields[FieldIdx].{$IFDEF UNICODE}AsAnsiString{$ELSE}AsString{$ENDIF}))
          else
            S := S + TinyTable.Fields[FieldIdx].{$IFDEF EXT_WIDESTR_SUPP}AsWideString{$ELSE}AsString{$ENDIF};
          Lines.Add(S);
        end;
        Lines.Add('');
        TinyTable.Next;
        MainForm.DoProgress(Trunc((RecordIdx + 1) / TinyTable.RecordCount * 100));
      end;
      Lines.Add('--------------------------------');
    end;
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
    TinyTable.Close;
    TinyTable.Free;
    MainForm.EndProgress;
  end;
end;

procedure TDBInfoForm.Exportdata1Click(Sender: TObject);
  //export data
var
  Table_name: string;
  T: TTinyTable;
  DB2CSV: TDB2CSV;
  OK: Boolean;
begin
  //export data
  Table_name := FCurNodeData.SelfName;
  T := TTinyTable.Create(nil);
  DB2CSV := TDB2CSV.Create(nil);
  try
    T.DatabaseName := FTinyDatabase.Filename;
    T.TableName := Table_name;
    T.Password := FTinyDatabase.Password;
    with DB2CSV do
    begin
      TinyTable := T;
      //CSVFile := ExtractFilePath(FTinyDatabase.FileName) + Table_name + '.csv';
      Name := 'DB2CSV1';
      Seprator := ';';
      FieldIndicator := '$';
      AutoOpen := True;
      Delimiter := '"';
      EmptyTable := False;
      UseDelimiter := True;
      SilentExport := True;
      TrimData := False;
      DefaultInt := 0;
      BufferSize := 4096;
    end;
    SaveDialog1.FileName := Table_name;
    if SaveDialog1.Execute then
    begin
      DB2CSV.CSVFile := SaveDialog1.FileName;
      if FileExists(DB2CSV.CSVFile) then
        if MessageDlg(Format('File %s exists. Overwrite?', [DB2CSV.CSVFile]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then
          Exit;
      Screen.Cursor := crHourGlass;
      try
        OK := DB2CSV.DatasetToCSV;
      finally
        Screen.Cursor := crDefault;
      end;
      if OK then
        MessageDlg(Format('Data saved into %s.', [DB2CSV.CSVFile]), mtInformation, [mbOK], 0)
      else
        MessageDlg('Any error occurred during saving.', mtError, [mbOK], 0)
    end;
  finally
    DB2CSV.Free;
    T.Free;
  end;
end;

procedure TDBInfoForm.LocateNodeOfTableName(TableName: string);
var
  I: Integer;
  NodeData: TNodeData;
begin
  for I := 0 to DBStructTreeView.Items.Count - 1 do
  begin
    if DBStructTreeView.Items[I].Data = nil then Continue;
    NodeData := GetNodeData(DBStructTreeView.Items[I]);
    if (NodeData.NodeType = ntTable) and (CompareText(NodeData.SelfName, TableName) = 0) then
    begin
      DBStructTreeView.Items[I].Selected := True;
      DBStructTreeView.Items[I].MakeVisible;
      FCurNode := DBStructTreeView.Items[I];
      Break;
    end;
  end;
end;

procedure TDBInfoForm.DeleteIndex(TableName, IndexName: string);
var
  R: Integer;
begin
  R := Application.MessageBox(PChar(AppLangMgr.Trans('Are you sure to delete?')), PChar(Application.Title), 36);
  if R = ID_YES then
  begin
    CloseFormOfTableName(TableName);
    FTinyDatabase.DeleteIndex(TableName, IndexName);
    FillDBStructToTreeView;
  end;
end;

procedure TDBInfoForm.RenameTable(OldTableName, NewTableName: string);
begin
  CloseFormOfTableName(OldTableName);
  FTinyDatabase.RenameTable(OldTableName, NewTableName);
end;

procedure TDBInfoForm.RenameField(TableName, OldFieldName, NewFieldName: string);
begin
  CloseFormOfTableName(TableName);
  FTinyDatabase.RenameField(TableName, OldFieldName, NewFieldName);
end;

procedure TDBInfoForm.RenameIndex(TableName, OldIndexName, NewIndexName: string);
begin
  CloseFormOfTableName(TableName);
  FTinyDatabase.RenameIndex(TableName, OldIndexName, NewIndexName);
end;

procedure TDBInfoForm.DBStructTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node, TempNode: TTreeNode;
  NodeData: TNodeData;
  Pos: TPoint;
  HitTest: THitTests;
  Empty: Boolean;
begin
  Pos := DBStructTreeView.ClientToScreen(Point(X, Y));
  HitTest := DBStructTreeView.GetHitTestInfoAt(X, Y);
  if (htOnIcon in HitTest) or (htOnLabel in HitTest) then
  begin
    TempNode := DBStructTreeView.GetNodeAt(X, Y);
    if TempNode <> nil then TempNode.Selected := True;
  end;

  Node := DBStructTreeView.Selected;
  if Node = nil then Exit;
  if Node.Data = nil then Exit;

  NodeData := GetNodeData(Node);
  FCurNodeData := NodeData;
  FCurNode := Node;

  if Button = mbRight then
  begin
    Empty := DBStructTreeView.Items.Count = 0;
    TableMenuOpenItem.Enabled := not Empty;
    TableMenuDeleteItem.Enabled := not Empty;
    TableMenuRenameItem.Enabled := not Empty;
    TableMenuEmptyItem.Enabled := not Empty;
    if Empty then TablePopupMenu.Popup(Pos.x, Pos.y);

    case NodeData.NodeType of
      ntTable:
        TablePopupMenu.Popup(Pos.x, Pos.y);
      ntField:
        FieldPopupMenu.Popup(Pos.x, Pos.y);
      ntIndex:
        IndexPopupMenu.Popup(Pos.x, Pos.y);
    end;
  end;
end;

procedure TDBInfoForm.DBStructTreeViewDblClick(Sender: TObject);
var
  Node: TTreeNode;
  NodeData: TNodeData;
begin
  Node := DBStructTreeView.Selected;
  if Node = nil then Exit;
  if Node.Data = nil then Exit;

  NodeData := GetNodeData(Node);
  case NodeData.NodeType of
    ntField:
      FieldMenuPropItemClick(nil);
    ntIndex:
      IndexMenuPropItemClick(nil);
  end;
end;

procedure TDBInfoForm.TableMenuOpenItemClick(Sender: TObject);
begin
  OpenTable(FCurNodeData.SelfName);
end;

procedure TDBInfoForm.TableMenuDeleteItemClick(Sender: TObject);
begin
  DeleteTable(FCurNodeData.SelfName);
end;

procedure TDBInfoForm.TableMenuRenameItemClick(Sender: TObject);
begin
  FCurNode.EditText;
end;

procedure TDBInfoForm.TableMenuRefreshItemClick(Sender: TObject);
begin
  FillDBStructToTreeView;
end;

procedure TDBInfoForm.IndexMenuPropItemClick(Sender: TObject);
var
  Value: TIndexPropFormData;
begin
  Value.TableName := FCurNodeData.TableName;
  Value.CurIndexName := FCurNodeData.SelfName;
  ShowIndexPropForm(Value);
end;

procedure TDBInfoForm.FieldMenuPropItemClick(Sender: TObject);
var
  Value: TFieldPropFormData;
begin
  Value.TableName := FCurNodeData.TableName;
  Value.CurFieldName := FCurNodeData.SelfName;
  ShowFieldPropForm(Value);
end;

procedure TDBInfoForm.TableMenuNewItemClick(Sender: TObject);
begin
  InvokeNewTableForm;
end;

procedure TDBInfoForm.TableMenuEmptyItemClick(Sender: TObject);
var
  Value: TTableListFormData;
  TableName: string;
begin
  if FCurNodeData.SelfName = '' then
  begin
    Value.TinyDatabase := FTinyDatabase;
    Value.TableName := '';
    if ShowTableListForm(Value) then
      TableName := Value.TableName
    else
      Exit;
  end
  else
    TableName := FCurNodeData.SelfName;

  EmptyTable(TableName);
end;

procedure TDBInfoForm.TableMenuDesignItemClick(Sender: TObject);
begin
  DesignTable(FCurNodeData.SelfName);
end;

procedure TDBInfoForm.Importdata1Click(Sender: TObject);
  //import data
var
  Table_name: string;
  T: TTinyTable;
  DB2CSV: TDB2CSV;
  OK: Boolean;
begin
  //export data
  Table_name := FCurNodeData.SelfName;
  T := TTinyTable.Create(nil);
  DB2CSV := TDB2CSV.Create(nil);
  try
    T.DatabaseName := FTinyDatabase.Filename;
    T.TableName := Table_name;
    T.Password := FTinyDatabase.Password;
    with DB2CSV do
    begin
      TinyTable := T;
      //CSVFile := ExtractFilePath(FTinyDatabase.FileName) + Table_name + '.csv';
      Name := 'DB2CSV1';
      Seprator := ';';
      FieldIndicator := '$';
      AutoOpen := True;
      Delimiter := '"';
      EmptyTable := True;
      UseDelimiter := True;
      SilentExport := True;
      TrimData := False;
      DefaultInt := 0;
      BufferSize := 4096;
    end;
    OpenDialog1.FileName := Table_name;
    if OpenDialog1.Execute then
    begin
      DB2CSV.CSVFile := OpenDialog1.FileName;
      OK := FileExists(DB2CSV.CSVFile);
      if AnsiCompareText(ExtractFileName(OpenDialog1.FileName), Table_name + '.csv') <> 0 then
        OK := MessageDlg('File name does not corresponding with table name.'#13#10'Continue?', mtWarning, [mbYes, mbNo], 0) = mrYes;
      if not OK then Exit;
      Screen.Cursor := crHourGlass;
      try
        OK := DB2CSV.CSVToDataset;
      finally
        Screen.Cursor := crDefault;
      end;
      if OK then
        MessageDlg('Data suscessfuly loaded into table ' + Table_name, mtInformation, [mbOK], 0)
      else
        MessageDlg('Wrong data or table scheme', mtError, [mbOK], 0)
    end;
  finally
    DB2CSV.Free;
    T.Free;
  end;

end;

procedure TDBInfoForm.IndexMenuDeleteItemClick(Sender: TObject);
begin
  DeleteIndex(FCurNodeData.TableName, FCurNodeData.SelfName);
end;

procedure TDBInfoForm.IndexMenuRenameItemClick(Sender: TObject);
begin
  FCurNode.EditText;
end;

procedure TDBInfoForm.FieldMenuRenameItemClick(Sender: TObject);
begin
  FCurNode.EditText;
end;

procedure TDBInfoForm.DBStructTreeViewEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
begin
  if Node.Data = nil then AllowEdit := False;
end;

procedure TDBInfoForm.DBStructTreeViewEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
var
  NodeData: TNodeData;
  TempNode: TTreeNode;
  I: Integer;
begin
  NodeData := GetNodeData(Node);
  case NodeData.NodeType of
    ntTable:
      begin
        RenameTable(NodeData.SelfName, S);
        MainForm.InitTableComboxBox;
        PNodeData(Node.Data)^.SelfName := S;
        for I := 0 to DBStructTreeView.Items.Count - 1 do
        begin
          TempNode := DBStructTreeView.Items[I];
          if TempNode.Data <> nil then
            if CompareText(GetNodeData(TempNode).TableName, NodeData.TableName) = 0 then
              PNodeData(TempNode.Data)^.TableName := S;
        end;
      end;
    ntField:
      begin
        RenameField(NodeData.TableName, NodeData.SelfName, S);
        PNodeData(Node.Data)^.SelfName := S;
      end;
    ntIndex:
      begin
        RenameIndex(NodeData.TableName, NodeData.SelfName, S);
        PNodeData(Node.Data)^.SelfName := S;
      end;
  end;
end;

end.
