{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Cached updates utility routines               }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L- }

unit vgCachUp;

interface
uses DB, DBTables, vgDB;

{ Getting queryies }
type
  TQueryParam      = (qpRequestLive, qpCachedUpdates, qpActive);
  TQueryParams     = set of TQueryParam;

function CreateHookedQuery(ADatabase: TDatabase; ASQL: String; DataSetHook: TDataSetHook;
  ParamName: String; AParams: Variant; QueryParams: TQueryParams): TQuery;

function CreateTableHookedQuery(Table: TTable; DataSetHook: TDataSetHook;
  Condition: String; AParams: Variant; QueryParams: TQueryParams): TQuery;

function CreateTableDefaultHookedQuery(Table: TTable; DataSetHook: TDataSetHook;
  Params: Variant; QueryParams: TQueryParams): TQuery;

function CreateTableActiveRecordQuery(DataSet: TDataSet; Table: TTable; DataSetHook: TDataSetHook; QueryParams: TQueryParams): TQuery;

implementation
uses SysUtils, vgUtils, vgDBUtl, vgDBPrms, vgBDEUtl;

function CreateHookedQuery(ADatabase: TDatabase; ASQL: String; DataSetHook: TDataSetHook;
  ParamName: String; AParams: Variant; QueryParams: TQueryParams): TQuery;
begin
  Result := CreateQuery(ADatabase, ASQL, ParamName, AParams);
  with Result do
  try
    if Assigned(DataSetHook) then
    with TDataSetHook(CreateCloneOwner(DataSetHook, Result)) do
    begin
      DataSet := Result;
      AssignEventsTo(DataSetHook);
      Active := True;
    end;
    if (qpCachedUpdates in QueryParams) then
      CachedUpdates := True;

    if (qpRequestLive in QueryParams) then
      RequestLive := True;

    if (qpActive in QueryParams) then
      Active := True;
  except
    Free;
    raise;
  end;
end;

function CreateTableHookedQuery(Table: TTable; DataSetHook: TDataSetHook;
  Condition: String; AParams: Variant; QueryParams: TQueryParams): TQuery;
var
  SQL: String;
begin
  SQL := GetSelect(Table, Table.TableName, Condition);

  Result := CreateHookedQuery(FindDatabase(Table.SessionName, Table.DatabaseName),
    SQL, DataSetHook, '', Null, []);
  with Result do
  try
    CopyFields(Table, Result);

    if (qpCachedUpdates in QueryParams) then
      CachedUpdates := True;

    if (qpRequestLive in QueryParams) then
      RequestLive := True;

    SetParams(Params, '', AParams);

    if (qpActive in QueryParams) then
      Active := True;
  except
    Result.Free;
    raise;
  end;
end;

function CreateTableDefaultHookedQuery(Table: TTable; DataSetHook: TDataSetHook;
  Params: Variant; QueryParams: TQueryParams): TQuery;
begin
  Result := CreateTableHookedQuery(Table, DataSetHook, Format('%s = :%s', [QuoteSQLName(DefaultKeyFields), QuoteSQLName(DefaultKeyFields)]), Params, QueryParams);
end;

function CreateTableActiveRecordQuery(DataSet: TDataSet; Table: TTable; DataSetHook: TDataSetHook; QueryParams: TQueryParams): TQuery;
begin
  Result := CreateTableDefaultHookedQuery(Table, DataSetHook, DataSet.FieldValues[DefaultKeyFields], QueryParams);
end;

end.
