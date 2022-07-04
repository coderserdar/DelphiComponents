unit OraSQL;

{$INCLUDE dOCI.inc}

interface

{
  ValuesNull contains True - if data exists, False - if no data

  from Oracle this flags comes as : -1 - this is NUll, >=0 - NOT NULL
  for parameters flags stored such as they comes from Oracle
}

uses Classes, DB, AOraUpdateSQL, AOraSQL, ADataSet, DataSetQuery, OraDB
     {$IFDEF D7} ,Variants {$ENDIF}
     ;

type
  TOraSQL=class(TDataSetQuery)
  private
   FOraUpdate:TAOraUpdateSQL;
   procedure SetOraUpdateSQL(Value:TAOraUpdateSQL);
  protected
  public
   constructor Create(AOwner:TComponent);override;
   procedure ApplyUpdates; override;
   procedure InternalRefresh;override;
  published
   property UpdateSQLs:TAOraUpdateSQL read FOraUpdate write SetOraUpdateSQL;
   property Database;
   property FetchCount;
   property SQL;
   property Params;
   property OnUpdateRecord;
  end;

{  TAStringProperty = class(TStringProperty)
    public
      function GetAttributes: TPropertyAttributes; override;
      procedure GetValueList(List: TStrings); virtual; abstract;
      procedure GetValues(Proc: TGetStrProc); override;
    end;

  TDataSetQueryDatabase = class(TAStringProperty)
    public
      procedure GetValueList(List: TStrings); override;
    end;
 }
procedure Register;

implementation

{$R dOCIIcons.res}

uses Dialogs, SysUtils, GoodDate {$IFDEF D6} ,Variants {$ENDIF};

procedure Register;
begin
  RegisterComponents('Data Access', [TOraSQL]);
//  RegisterPropertyEditor(TypeInfo(string), TOraSQL, 'Database', TDataSetQueryDatabase);
end;


{
function TAStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TAStringProperty.GetValues(Proc: TGetStrProc);
var
    I: Integer;
    Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

procedure TDataSetQueryDatabase.GetValueList(List: TStrings);
var i, j: integer;
begin
  for i := 0 to Screen.FormCount - 1 do
    for j := 0 to Screen.Forms[i].ComponentCount - 1 do
      if Screen.Forms[i].Components[j] is TOraDB then
    List.Add(TOraDB(Screen.Forms[i].Components[j]).Database.ClassName);
end;
}

 { TOraSQL }

constructor TOraSQL.Create(AOwner: TComponent);
begin
 inherited CreateSet(AOwner,qtOracle);
end;

procedure TOraSQL.ApplyUpdates;
var sql:TStrings;
    q:TAOraSQL;
    i:integer;
    p:TParams;
    fid:integer;
    pname,fname:string;
    pt:TAParamType;
    old:boolean;
    v:variant;
begin
 q:=TAOraSQL.Create(self);
 p:=TParams.Create;
try
 if not Assigned(FOraUpdate) then begin raise Exception.Create('UpdateSQLs not defined!'); end;
 sql:=nil;
 case updType of
  ukDelete: sql:=FOraUpdate.DeleteSQL;
  ukModify: sql:=FOraUpdate.ModifySQL;
  ukInsert: sql:=FOraUpdate.InsertSQL;
 end;
 q.Database:=TOraDB(Database);
 q.SQL.Assign(sql);
 p.ParseSQL(sql.Text,true);
 for i:=0 to p.Count-1 do begin // assign field values
  pname:=p[i].Name;
  if pname='=' then continue;
  old:=False;
  if copy(pname,1,4)='OUT_'
   then begin pt:=ptoOutput; fname:=copy(pname,5,256); end
  else if copy(pname,1,4)='OLD_'
   then begin pt:=ptoInput; fname:=copy(pname,5,256); old:=True; end
  else begin pt:=ptoInput; fname:=pname; end;
  fid:=FieldID[fname];
  if q.ParamExists(pname) then continue;
  q.AddParam(pname,TypeDelphiToA(Fields[fid].DataType),pt);
  if old then begin
   if Modified
    then v:=Fields[fid].OldValue
    else v:=Fields[fid].Value;
   if VarIsNull(v) then q.ParamByName[pname].Clear
    else
     case q.ParamByName[pname].FieldType of
      ftoDate:    q.ParamByName[pname].AsDate:=DateTimeToGoodDate(v);
      ftoInteger: q.ParamByName[pname].AsInteger:=v;
      ftoString:  q.ParamByName[pname].AsString:=v;
      ftoDouble:  q.ParamByName[pname].AsDouble :=v;
     end;
  end else begin
   if Fields[fid].IsNull then q.ParamByName[pname].Clear
    else
     case q.ParamByName[pname].FieldType of
      ftoDate:    q.ParamByName[pname].AsDate:=DateTimeToGoodDate(Fields[fid].AsDateTime);
      ftoInteger: q.ParamByName[pname].AsInteger:=Fields[fid].AsInteger;
      ftoString:  q.ParamByName[pname].AsString:=Fields[fid].AsString;
      ftoDouble:  q.ParamByName[pname].AsDouble :=Fields[fid].AsFloat;
     end;
  end;
 end;
 q.ExecSQL;
 for i:=0 to p.Count-1 do begin // assign field values
  pname:=p[i].Name;
  if pname='=' then continue;
  if copy(pname,1,4)<>'OUT_' then continue;
  fname:=copy(pname,5,256);
  fid:=FieldID[fname];
  if q.ParamByName[pname].IsNull then Fields[fid].Clear
   else
    case q.ParamByName[pname].FieldType of
     ftoInteger: Fields[fid].AsInteger:=q.ParamByName[pname].AsInteger;
     ftoString:  Fields[fid].AsString:=q.ParamByName[pname].AsString;
     ftoDouble:  Fields[fid].AsFloat:=q.ParamByName[pname].AsDouble;
    end;
 end;
finally
 p.Free;
 q.Free;
end;
end;

procedure TOraSQL.InternalRefresh;
begin
 DisableControls;
 ReOpen;
 EnableControls;
end;

procedure TOraSQL.SetOraUpdateSQL(Value: TAOraUpdateSQL);
begin
  if Value <> FOraUpdate then
  begin
    if Assigned(FOraUpdate) and (FOraUpdate.DataSet = Self) then
      FOraUpdate.DataSet := nil;
    FOraUpdate := Value;
    if Assigned(FOraUpdate) then begin
      { If another dataset already references this updateobject, then
        remove the reference }
      if Assigned(FOraUpdate.DataSet) and
        (FOraUpdate.DataSet <> Self) then
        TOraSQL(FOraUpdate.DataSet).UpdateSQLs := nil;
      FOraUpdate.DataSet := Self;
    end;
  end;
end;

end.

