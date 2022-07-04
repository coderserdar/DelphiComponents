unit DTables;
    
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF MSWINDOWS} // Delphi 6 and up
  Variants,
  {$ENDIF}
  DB, DDB, DMaster, ADOBase, DFilters, DUtils;

type

  // Standard Table
  TDTableDataLink = class;
  TDTable = class(TDADODataSet)
  private
    { Private declarations }
    FTableName : TFileName;
    FDataLink  : TDTableDatalink;
    FRelation  : String;

    // Handle properties
    procedure SetTableName(const Value: TFileName);
    procedure SetDataSource(Value: TDataSource);
    procedure SetRelation(const Value: String);

    // DataSet event handlers
    procedure MasterChanged(Sender: TObject);
    procedure MasterDisabled(Sender: TObject);

  protected
    { Protecte declarations }
    function  GetDataSource: TDataSource; override;
    procedure UpdateRelation(Enabled: Boolean); virtual;

    procedure InternalOpen; override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ModifyRelation(Source: TDataSource = nil; RelationStr: String = '');

  published
    { Published declarations }
    property TableName   : TFileName   read FTableName    write SetTableName;
    property DataSource  : TDataSource read GetDataSource write SetDataSource;
    property Relation    : String      read FRelation     write SetRelation;

    // ADO Settings
    property ADOCommandOption;
    property ADOCursorType;
    property ADOCursorLocation;
    property ADOLockType;

  end;

{******************************************************************************}

  // Standard Query
  TDQuery = class(TDADODataSet)
  private
    { Private declarations }
    FSQL        : TStrings;
    FParams     : TParams;
    FText       : String;
    FDataLink   : TDataLink;
    FRows       : Integer;
    FSQLBinary  : PChar;
    FParamCheck : Boolean;

    procedure QueryChanged(Sender: TObject);
    procedure ReadBinaryData(Stream: TStream);
    procedure ReadParamData(Reader: TReader);
    procedure SetDataSource(Value: TDataSource);
    procedure SetQuery(Value: TStrings);
    procedure SetText(const Value: String);
    procedure SetParamsList(Value: TParams);
    procedure RefreshParams;
    procedure SetParams;
    function  SwapParams(const SQLText: String): String;
    procedure WriteBinaryData(Stream: TStream);
    procedure WriteParamData(Writer: TWriter);

  protected
    { Protecte declarations }
    function  GetDataSource: TDataSource; override;
    function  GetParamsCount: Word;
    procedure InternalOpen; override;

    property DataLink: TDataLink read FDataLink;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ExecSQL; overload;
    procedure ExecSQL(SQLList: TStrings); overload;
    procedure ExecSQL(const SQLText: String); overload;

    procedure DefineProperties(Filer: TFiler); override;
    procedure GetDetailLinkFields(MasterFields, DetailFields: TList); override;
    function ParamByName(const Value: string): TParam;

    property ParamCount    : Word        read GetParamsCount;
    property Text          : String      read FText           write SetText;
    property RowsAffected  : Integer     read FRows;
    property SQLBinary     : PChar       read FSQLBinary      write FSQLBinary;

  published
    { Published declarations }
    property DataSource    : TDataSource read GetDataSource   write SetDataSource;
    property ParamCheck    : Boolean     read FParamCheck     write FParamCheck default True;
    property SQL           : TStrings    read FSQL            write SetQuery;
    property Params        : TParams     read FParams         write SetParamsList stored False;

    // ADO Settings
    property ADOCommandOption;
    property ADOCursorType;
    property ADOCursorLocation;
    property ADOLockType;

  end;

{******************************************************************************}

  // TTable Datalink
  TDTableDataLink = class(TDetailDataLink)
  private
    FDataSet: TDataSet;
    FOnMasterChange  : TNotifyEvent;
    FOnMasterDisable : TNotifyEvent;

  protected
    procedure ActiveChanged; override;
    procedure CheckBrowseMode; override;
    function  GetDetailDataSet: TDataSet; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;

  public
    constructor Create(DataSet: TDataSet);

    property OnMasterChange  : TNotifyEvent read FOnMasterChange  write FOnMasterChange;
    property OnMasterDisable : TNotifyEvent read FOnMasterDisable write FOnMasterDisable;

  end;

  // Query DataLink
  TDQueryDataLink = class(TDetailDataLink)
  private
    FDataSet: TDQuery;

  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
    function GetDetailDataSet: TDataSet; override;
    procedure CheckBrowseMode; override;

  public
    constructor Create(ADataSet: TDQuery);

  end;

{******************************************************************************}

  // Update object to Query and Table
  TDUpdateSQL = class(TDUpdateObject)
  private
    { Private declarations }
    FDataSet: TDADODataSet;
    FQueries: Array[TUpdateKind] of TDQuery;
    FSQLText: Array[TUpdateKind] of TStrings;

    evOnModifyVerbose: TVerboseEvent;
    evOnInsertVerbose: TVerboseEvent;
    evOnDeleteVerbose: TVerboseEvent;

    function  GetQuery(UpdateKind: TUpdateKind): TDQuery;
    function  GetSQL(UpdateKind: TUpdateKind): TStrings;
    function  GetSQLIndex(Index: Integer): TStrings;
    procedure SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
    procedure SetSQLIndex(Index: Integer; Value: TStrings);

  protected
    { Protected declarations }
    function GetDataSet: TDADODataSet; override;
    procedure SetDataSet(ADataSet: TDADODataSet); override;
    procedure SQLChanged(Sender: TObject);

    procedure DoInsertVerbose(const Text: String; Mode: TVerboseMode);
    procedure DoDeleteVerbose(const Text: String; Mode: TVerboseMode);
    procedure DoModifyVerbose(const Text: String; Mode: TVerboseMode);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Apply(UpdateKind: TUpdateKind); override;
    procedure ExecSQL(UpdateKind: TUpdateKind);
    procedure SetParams(UpdateKind: TUpdateKind);

    property DataSet;
    property Query[UpdateKind: TUpdateKind]: TDQuery read GetQuery;
    property SQL[UpdateKind: TUpdateKind]: TStrings  read GetSQL  write SetSQL;

  published
    { Published declarations }
    property ModifySQL: TStrings index 0 read GetSQLIndex write SetSQLIndex;
    property InsertSQL: TStrings index 1 read GetSQLIndex write SetSQLIndex;
    property DeleteSQL: TStrings index 2 read GetSQLIndex write SetSQLIndex;

    property OnModifyVerbose: TVerboseEvent read evOnModifyVerbose write evOnModifyVerbose;
    property OnInsertVerbose: TVerboseEvent read evOnInsertVerbose write evOnInsertVerbose;
    property OnDeleteVerbose: TVerboseEvent read evOnDeleteVerbose write evOnDeleteVerbose;

  end;

implementation

{$R *.dcr}

{******************************************************************************}
{***                               Table component                          ***}
{******************************************************************************}
constructor TDTable.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     FDataLink := TDTableDataLink.Create(Self);
     FDataLink.OnMasterChange  := MasterChanged;
     FDataLink.OnMasterDisable := MasterDisabled;
     FTableName := '';
     ADOCommandOption := coTable;
end;


destructor TDTable.Destroy;
begin
     FDataLink.Free;
     inherited Destroy;
end;



procedure TDTable.SetTableName(const Value: TFileName);
var
   sOld : String;
begin
     if FTableName <> Value then
     begin
          sOld := FTableName;
          FTableName := Value;
          if Active or (csDesigning in ComponentState) then Close;
          if sOld <> '' then
          begin
               Sort   := '';
               Filter := '';
               Filters.Clear;
          end;
          if not StoreDefs then FieldDefs.Clear;
          if Assigned(Master) then
          begin
               if Master.SourceType = stAccess then
               begin
                    if (Pos(' ', FTableName) > 0) or
                       (Pos('-', FTableName) > 0)
                    then
                        Command := '[' + FTableName + ']'
                    else
                        Command := FTableName;
               end
               else
               begin
                    if (Pos(' ', FTableName) > 0) or
                       (Pos('-', FTableName) > 0)
                    then
                        Command := '"' + FTableName + '"'
                    else
                        Command := FTableName;
               end;
          end
          else
          begin
               if (Pos(' ', FTableName) > 0) or
                  (Pos('-', FTableName) > 0)
               then
                   Command := '"' + FTableName + '"'
               else
                   Command := FTableName;
          end;
     end;
end;


procedure TDTable.SetDataSource(Value: TDataSource);
begin
     if IsLinkedTo(Value) then DatabaseError(cnsCircularDataLink, Self);
     FDataLink.DataSource := Value;
end;


function TDTable.GetDataSource: TDataSource;
begin
     Result := FDataLink.DataSource;
end;


procedure TDTable.SetRelation(const Value: String);
begin
     if FRelation <> Value then
     begin
          FRelation := Value;
          UpdateRelation(True);
     end;
end;


procedure TDTable.MasterChanged(Sender: TObject);
begin
     CheckBrowseMode;
     UpdateRelation(FDataLink.Active);
end;


procedure TDTable.MasterDisabled(Sender: TObject);
begin
     UpdateRelation(FDataLink.Active);
end;


procedure TDTable.UpdateRelation(Enabled: Boolean);
var
   iPos : Integer;
   sFld : String;
   oFld : TField;
   sVal : String;
   sTmp : String;
begin
     sTmp := Relation;
     if Enabled and (sTmp <> '') and FDataLink.Active then
     begin
          repeat
          begin
               iPos := Pos(':', sTmp);
               if iPos > 0 then
               begin
                    sFld := Copy(sTmp, iPos + 1, Length(sTmp));
                    if Pos(' ', sFld) > 0 then sFld := Copy(sFld, 1, Pos(' ', sFld)-1);
                    oFld := FDataLink.DataSet.FindField(sFld);
                    sVal := '';
                    if Assigned(oFld) then
                    begin
                         case oFld.DataType of
                              ftFmtMemo,
                              ftFixedChar,
                              ftWideString,
                              ftMemo,
                              ftString   : sVal := '''' + oFld.AsString + '''';
                              ftTime     : sVal := '{t ''' + TimeToStr(oFld.AsDateTime) + ''' }';
                              ftDate     : sVal := '{d ''' + DateToStd(oFld.AsDateTime, '-') + ''' }';
                              ftDateTime : sVal := '{d ''' + DateToStd(oFld.AsDateTime, '-') + ''' }';
                              ftBoolean  : sVal := IIF(oFld.AsBoolean, cnsWordBoolTrue, cnsWordBoolFalse);
                              ftSmallint,
                              ftInteger,
                              ftWord,
                              ftFloat,
                              ftCurrency,
                              ftBCD,
                              ftAutoInc,
                              ftLargeint :
                              begin
                                   sVal := oFld.AsString;
                                   if sVal = '' then sVal := '0';
                              end;
                         else sVal := oFld.AsString;
                         end;
                         sTmp := StrTran(sTmp, ':' + sFld, sVal);
                    end;
               end;
          end;
          until iPos = 0;
          Filter := sTmp;
     end
     else Filter := '';
end;


procedure TDTable.ModifyRelation(Source: TDataSource = nil; RelationStr: String = '');
begin
     DataSource := nil;
     Relation   := '';
     DataSource := Source;
     Relation   := RelationStr;
end;


procedure TDTable.InternalOpen;
begin
     if Assigned(Master) then
     begin
          if not Master.Connected then Master.Connected := True;
          if Master.SourceType = stAccess then
          begin
               if (Pos(' ', FTableName) > 0) or
                  (Pos('-', FTableName) > 0)
               then
                   Command := '[' + FTableName + ']'
               else
                   Command := FTableName;
          end
          else
          begin
               if (Pos(' ', FTableName) > 0) or
                  (Pos('-', FTableName) > 0)
               then
                   Command := '"' + FTableName + '"'
               else
               Command := FTableName;
          end;
     end
     else
     begin
          if (Pos(' ', FTableName) > 0) or
             (Pos('-', FTableName) > 0)
          then
              Command := '"' + FTableName + '"'
          else
              Command := FTableName;
     end;
     inherited;
end;


{******************************************************************************}
{***                               Query component                          ***}
{******************************************************************************}


constructor TDQuery.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     FSQL := TStringList.Create;
     TStringList(SQL).OnChange := QueryChanged;
     FParams    := TParams.Create(Self);
     FDataLink  := TDQueryDataLink.Create(Self);
     ParamCheck := True;
     FRows      := -1;
end;


destructor TDQuery.Destroy;
begin
     Close;
     FSQL.Free;
     FSQL := nil;
     FParams.Free;
     FDataLink.Free;
     StrDispose(SQLBinary);
     inherited Destroy;
end;


procedure TDQuery.InternalOpen;
begin
     SetParams;
     Command := SwapParams(FText);
     inherited InternalOpen;
end;


procedure TDQuery.SetText(const Value: String);
begin
     if FText <> Value then
     begin
          FText := Value;
          SetParams;
          Command := SwapParams(FText);
     end;
end;


procedure TDQuery.SetDataSource(Value: TDataSource);
begin
     if IsLinkedTo(Value) then DatabaseError(cnsCircularDataLink, Self);
     FDataLink.DataSource := Value;
end;


function TDQuery.GetDataSource: TDataSource;
begin
     Result := FDataLink.DataSource;
end;


procedure TDQuery.SetQuery(Value: TStrings);
begin
     if SQL.Text <> Value.Text then
     begin
          Close;
          SQL.BeginUpdate;
          try
               SQL.Assign(Value);
          finally
               SQL.EndUpdate;
          end;
     end;
end;


procedure TDQuery.QueryChanged(Sender: TObject);
var
  List: TParams;
  sTmp: String;
begin
     if not (csReading in ComponentState) then
     begin
          Close;
          StrDispose(SQLBinary);
          SQLBinary := nil;
          if ParamCheck or (csDesigning in ComponentState) then
          begin
               List := TParams.Create(Self);
               try
               begin
                    sTmp := List.ParseSQL(SQL.Text, True);
                    List.AssignValues(FParams);
                    FParams.Clear;
                    FParams.Assign(List);
                    Text := sTmp;
               end;
               finally
                    List.Free;
               end;
          end
          else Text := SQL.Text;
          DataEvent(dePropertyChange, 0);
     end
     else Text := FParams.ParseSQL(SQL.Text, False);
end;


procedure TDQuery.SetParamsList(Value: TParams);
begin
     FParams.AssignValues(Value);
end;


function TDQuery.GetParamsCount: Word;
begin
     Result := FParams.Count;
end;


procedure TDQuery.DefineProperties(Filer: TFiler);

  function WriteData: Boolean;
  begin
       if Filer.Ancestor <> nil
       then
           Result := not FParams.IsEqual(TDQuery(Filer.Ancestor).FParams)
       else
           Result := FParams.Count > 0;
  end;

begin
     inherited DefineProperties(Filer);
     Filer.DefineBinaryProperty('Data', ReadBinaryData, WriteBinaryData, SQLBinary <> nil);
     Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData);
end;


procedure TDQuery.ReadParamData(Reader: TReader);
begin
     Reader.ReadValue;
     Reader.ReadCollection(FParams);
end;


procedure TDQuery.WriteParamData(Writer: TWriter);
begin
     Writer.WriteCollection(Params);
end;


procedure TDQuery.ReadBinaryData(Stream: TStream);
begin
     SQLBinary := StrAlloc(Stream.Size);
     Stream.ReadBuffer(SQLBinary^, Stream.Size);
end;


procedure TDQuery.WriteBinaryData(Stream: TStream);
begin
     Stream.WriteBuffer(SQLBinary^, StrBufSize(SQLBinary));
end;


procedure TDQuery.SetParams;
var
  i: Integer;
  DataSet: TDataSet;
begin
     if FDataLink.DataSource <> nil then
     begin
          DataSet := FDataLink.DataSource.DataSet;
          if DataSet <> nil then
          begin
               DataSet.FieldDefs.Update;
               for i := 0 to FParams.Count - 1 do
               begin
                    with FParams[i] do
                    begin
                         if not Bound then
                         begin
                              AssignField(DataSet.FieldByName(Name));
                              Bound := False;
                         end;
                    end;
               end;
          end;
     end;
end;


function TDQuery.SwapParams(const SQLText: String): String;
var
   iPos : Integer;
   sVal : String;
   i    : Integer;
begin
     Result := SQLText;
     if (csReading in ComponentState) then Exit;
     i := 0;
     repeat
     begin
          iPos := Pos('?', Result);
          if (iPos > 0) and (i < FParams.count) then
          begin
               case FParams.Items[i].DataType of
                    ftMemo,
                    ftString   : sVal := '''' + FParams.Items[i].AsString + '''';
                    ftTime     : sVal := '{t ''' + TimeToStr(FParams.Items[i].AsDateTime) + ''' }';
                    ftDate     : sVal := '{d ''' + DateToStd(FParams.Items[i].AsDateTime, '-') + ''' }';
                    ftDateTime : sVal := '{d ''' + DateToStd(FParams.Items[i].AsDateTime, '-') + ''' }';
                    ftBoolean  : sVal := IIF(FParams.Items[i].AsBoolean, cnsWordBoolTrue, cnsWordBoolFalse);
               else sVal := FParams.Items[i].AsString;
               end;
               if FParams.Items[i].IsNull then sVal := 'NULL';
               Result := Copy(Result, 1, iPos-1) + ' ' + sVal + ' ' + Copy(Result, iPos+1, Length(Result));
               Inc(i);
          end;
     end;
     until (Pos('?', Result) = 0) or (i >= FParams.count);
     Result := Trim(Result);
end;


procedure TDQuery.RefreshParams;
var
  DataSet: TDataSet;
begin
     DisableControls;
     try
     begin
          if FDataLink.DataSource <> nil then
          begin
               DataSet := FDataLink.DataSource.DataSet;
               if DataSet <> nil then
               begin
                    if DataSet.Active and (DataSet.State <> dsSetKey) then
                    begin
                         Close;
                         Open;
                    end;
               end;
          end;
     end;
     finally
           EnableControls;
     end;
end;


function TDQuery.ParamByName(const Value: string): TParam;
begin
     Result := FParams.ParamByName(Value);
end;


procedure TDQuery.ExecSQL;
begin
     SetParams;
     FRows := InternalSQL(SwapParams(Text));
end;


procedure TDQuery.ExecSQL(SQLList: TStrings);
begin
     SetParams;
     FRows := InternalSQL(SwapParams(SQLList.Text));
end;


procedure TDQuery.ExecSQL(const SQLText: String);
begin
     SetParams;
     FRows := InternalSQL(SwapParams(SQLText));
end;


procedure TDQuery.GetDetailLinkFields(MasterFields, DetailFields: TList);

  function AddFieldToList(const FieldName: string; DataSet: TDataSet; List: TList): Boolean;
  var
    Field: TField;
  begin
       Field := DataSet.FindField(FieldName);
       if (Field <> nil) then List.Add(Field);
       Result := Field <> nil;
  end;

var
  i: Integer;
begin
     MasterFields.Clear;
     DetailFields.Clear;
     if (DataSource <> nil) and (DataSource.DataSet <> nil) then
     begin
          for i := 0 to Params.Count - 1 do
          begin
               if AddFieldToList(Params[i].Name, DataSource.DataSet, MasterFields) then AddFieldToList(Params[i].Name, Self, DetailFields);
          end;
     end;
end;


{******************************************************************************}
{***                         DataSet DataLink component                     ***}
{******************************************************************************}


constructor TDTableDataLink.Create(DataSet: TDataSet);
begin
     inherited Create;
     FDataSet := DataSet;
end;


procedure TDTableDataLink.ActiveChanged;
begin
     if FDataSet.Active and not (csDestroying in FDataSet.ComponentState) then
     begin
          if Active then
          begin
               if Assigned(FOnMasterChange) then FOnMasterChange(Self);
          end
          else
          begin
               if Assigned(FOnMasterDisable) then FOnMasterDisable(Self);
          end;
     end;
end;


procedure TDTableDataLink.CheckBrowseMode;
begin
     if FDataSet.Active then FDataSet.CheckBrowseMode;
end;


function TDTableDataLink.GetDetailDataSet: TDataSet;
begin
     Result := FDataSet;
end;

procedure TDTableDataLink.LayoutChanged;
begin
     ActiveChanged;
end;


procedure TDTableDataLink.RecordChanged(Field: TField);
begin
     if (DataSource.State <> dsSetKey) and
        FDataSet.Active and
        Assigned(FOnMasterChange) then FOnMasterChange(Self);
end;


{******************************************************************************}


constructor TDQueryDataLink.Create(ADataSet: TDQuery);
begin
     inherited Create;
     FDataSet := ADataSet;
end;


procedure TDQueryDataLink.ActiveChanged;
begin
     if FDataSet.Active then FDataSet.RefreshParams;
end;


function TDQueryDataLink.GetDetailDataSet: TDataSet;
begin
     Result := FDataSet;
end;


procedure TDQueryDataLink.RecordChanged(Field: TField);
begin
     if (Field = nil) and FDataSet.Active then FDataSet.RefreshParams;
end;


procedure TDQueryDataLink.CheckBrowseMode;
begin
     if FDataSet.Active then FDataSet.CheckBrowseMode;
end;


{******************************************************************************}
{***                              Update SQL                                ***}
{******************************************************************************}


constructor TDUpdateSQL.Create(AOwner: TComponent);
var
  UpdateKind: TUpdateKind;
begin
     inherited Create(AOwner);
     for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
     begin
          FSQLText[UpdateKind] := TStringList.Create;
          TStringList(FSQLText[UpdateKind]).OnChange := SQLChanged;
     end;
end;


destructor TDUpdateSQL.Destroy;
var
  UpdateKind: TUpdateKind;
begin
     if Assigned(FDataSet) and
        (FDataSet.UpdateObject = Self) then FDataSet.UpdateObject := nil;
     for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
     begin
          FSQLText[UpdateKind].Free;
     end;
     inherited Destroy;
end;


procedure TDUpdateSQL.ExecSQL(UpdateKind: TUpdateKind);
begin
     Query[UpdateKind].ExecSQL;
     if Query[UpdateKind].RowsAffected <> 1 then DatabaseError(cnsUpdateFailed);
end;


function TDUpdateSQL.GetQuery(UpdateKind: TUpdateKind): TDQuery;
begin
     if not Assigned(FQueries[UpdateKind]) then
     begin
          FQueries[UpdateKind] := TDQuery.Create(Self);
          case UpdateKind of
               ukModify : FQueries[UpdateKind].OnVerbose := DoModifyVerbose;
               ukInsert : FQueries[UpdateKind].OnVerbose := DoInsertVerbose;
               ukDelete : FQueries[UpdateKind].OnVerbose := DoDeleteVerbose;
          end;
          FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
          if (FDataSet is TDADODataSet) then
          begin
               FQueries[UpdateKind].Connection := TDADODataSet(FDataSet).Connection;
               FQueries[UpdateKind].Master     := TDADODataSet(FDataSet).Master;
          end;
     end;
     Result := FQueries[UpdateKind];
end;


function TDUpdateSQL.GetSQL(UpdateKind: TUpdateKind): TStrings;
begin
     Result := FSQLText[UpdateKind];
end;


function TDUpdateSQL.GetSQLIndex(Index: Integer): TStrings;
begin
     Result := FSQLText[TUpdateKind(Index)];
end;


function TDUpdateSQL.GetDataSet: TDADODataSet;
begin
     Result := FDataSet;
end;


procedure TDUpdateSQL.SetDataSet(ADataSet: TDADODataSet);
begin
     FDataSet := ADataSet;
end;


procedure TDUpdateSQL.SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
begin
     FSQLText[UpdateKind].Assign(Value);
end;


procedure TDUpdateSQL.SetSQLIndex(Index: Integer; Value: TStrings);
begin
     SetSQL(TUpdateKind(Index), Value);
end;


procedure TDUpdateSQL.SQLChanged(Sender: TObject);
var
  UpdateKind: TUpdateKind;
begin
     for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
     begin
          if Sender = FSQLText[UpdateKind] then
          begin
               if Assigned(FQueries[UpdateKind]) then
               begin
                    FQueries[UpdateKind].Params.Clear;
                    FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
               end;
               Break;
          end;
     end;
end;


procedure TDUpdateSQL.SetParams(UpdateKind: TUpdateKind);
var
  i    : Integer;
  Old  : Boolean;
  Param: TParam;
  PName: string;
  Field: TField;
  Value: Variant;
begin
     if not Assigned(FDataSet) then Exit;
     with Query[UpdateKind] do
     begin
          for i := 0 to Params.Count-1 do
          begin
               Param := Params[i];
               PName := Param.Name;
               Old := CompareText(Copy(AnsiUpperCase(PName), 1, 4), 'OLD_') = 0;
               if Old then System.Delete(PName, 1, 4);
               Field := FDataSet.FindField(PName);
               if not Assigned(Field) then Continue;
               if Old then Param.AssignFieldValue(Field, Field.OldValue) else
               begin
                    Value := Field.NewValue;
                    if VarIsEmpty(Value) then Value := Field.OldValue;
                    Param.AssignFieldValue(Field, Value);
               end;
          end;
     end;
end;


procedure TDUpdateSQL.Apply(UpdateKind: TUpdateKind);
begin
     SetParams(UpdateKind);
     ExecSQL(UpdateKind);
end;


procedure TDUpdateSQL.DoDeleteVerbose(const Text: String; Mode: TVerboseMode);
begin
     if Assigned(evOnDeleteVerbose) then OnDeleteVerbose(Text, Mode);
end;


procedure TDUpdateSQL.DoInsertVerbose(const Text: String; Mode: TVerboseMode);
begin
     if Assigned(evOnInsertVerbose) then OnInsertVerbose(Text, Mode);
end;


procedure TDUpdateSQL.DoModifyVerbose(const Text: String; Mode: TVerboseMode);
begin
     if Assigned(evOnModifyVerbose) then OnModifyVerbose(Text, Mode);
end;


end.
