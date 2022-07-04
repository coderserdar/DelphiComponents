{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         vgBDEUtl unit                                 }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L- }

unit vgBDEIB;

interface
uses Classes, DB, DBTables;

{ IB - syntax compatible SPs }
type
  TStoredProcType = (spSelect, spExecute);
  TFieldKind      = (fkNormal, fkReadOnly);
  TFieldKinds     = set of TFieldKind;

function IB_CreateStoredProcSQL(ProcName, InParams, OutParams: String; ProcType: TStoredProcType): String;
function IB_CreateStoredProcQuery(ADatabase: TDatabase; StoredProc: TStoredProc; ProcType: TStoredProcType): TQuery;

{ Min/Max dates }
function IB_MinDate: TDateTime;
function IB_MaxDate: TDateTime;

{ Special queries }
procedure GetTableFields(Database: TDatabase; const TableName: string;
  FieldKinds: TFieldKinds; FieldNames: TStrings);

implementation
uses SysUtils, vgDBUtl, vgBDEUtl;

{ IB - syntax compatible SPs }
function IB_CreateStoredProcSQL(ProcName, InParams, OutParams: String; ProcType: TStoredProcType): String;
begin
  case ProcType of
    spSelect:
      begin
        Result := Format('SELECT %s FROM %s', [OutParams, ProcName]);
        if InParams <> '' then
          Result := Format('%s(%s);', [Result, InParams]);
      end;
    spExecute:
      begin
        Result := Format('EXECUTE PROCEDURE %s %s', [ProcName, InParams]);
      end;
  end;
end;

function IB_CreateStoredProcQuery(ADatabase: TDatabase; StoredProc: TStoredProc; ProcType: TStoredProcType): TQuery;
var
  I: Integer;
  OutParams, InParams: String;
  Param: TParam;
begin
  Result := CreateQuery(ADatabase, '', '', Null);
  try
    StoredProc.Prepare;
    InParams := ''; OutParams := '';
    for I := 0 to StoredProc.ParamCount - 1 do
    begin
      Param := StoredProc.Params[I];
      if Param.ParamType = ptInput then
        AddSQLFieldName(InParams, ':' + Param.Name)
      else if Param.ParamType = ptOutput then
        AddSQLFieldName(OutParams, Param.Name);
    end;
    Result.SQL.Text := IB_CreateStoredProcSQL(StoredProc.StoredProcName, InParams, OutParams, ProcType);
    for I := 0 to StoredProc.ParamCount - 1 do
    begin
      Param := StoredProc.Params[I];
      if Param.ParamType = ptInput then
        Result.ParamByName(Param.Name).DataType := Param.DataType;
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ Interbase Min/Max dates }

function IB_MinDate: TDateTime;
begin
   Result := EncodeDate(100, 1, 1);
end;

function IB_MaxDate: TDateTime;
begin
   IB_MaxDate := EncodeDate(5941, 12, 11);
end;

procedure GetTableFields(Database: TDatabase; const TableName: string;
  FieldKinds: TFieldKinds; FieldNames: TStrings);
const
  SelectSQL =
    'select RF.rdb$field_name as FieldName ' +
    'from rdb$relations R ' +
      'inner join rdb$relation_fields RF on R.rdb$relation_name = RF.rdb$relation_name ' +
      'inner join rdb$fields F on RF.rdb$field_source = F.rdb$field_name ' +
    'where R.rdb$relation_name = "%s" and (0=1 %s %s)';
  Fields1 = 'or F.rdb$computed_blr is Null';
  Fields2 = 'or F.rdb$computed_blr is not Null';
var
  Query: TQuery;
  S1, S2: string;
begin
  if fkNormal in FieldKinds then S1 := Fields1 else S1 := '';
  if fkReadOnly in FieldKinds then S2 := Fields2 else S2 := '';
  Query := CreateQuery(Database, Format(SelectSQL, [AnsiUpperCase(TableName), S1, S2]), '', Null);
  with Query do
  try
    Open;
    FieldNames.Clear;
    DataSetToStrings(Query, '', '', flNone, FieldNames, -1);
  finally
    Free;
  end;
end;

end.
