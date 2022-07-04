unit metadatasqlgenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, jvuib,
  jvuibmetadata;

type
  TGenSQLSOption = (gsqlIncludeDescriptions, gsqlSetGenValue,
        gsqlIncludeUserName, gsqlIncludeDateTime, gsqlIncludeGrant);

  TGenSQLSOptions = set of TGenSQLSOption;

  TGenAllParam = (gaDomains, gaTables, gaViews, gaTrigers, gaStoredProc,
     gaGenerators, gaExceptions, gaUdf, gaRoles, gaIndexses);

  TGenAllParams = set of TGenAllParam;
  
type

  { TMetaDataSQLGenerator }

  TMetaDataSQLGenerator = class(TComponent)
  private
    FDataBase: TJvUIBDataBase;
    FGenAllParams: TGenAllParams;
    FGenSQLSOptions: TGenSQLSOptions;
    procedure SetDataBase(const AValue: TJvUIBDataBase);
    procedure SetGenAllParams(const AValue: TGenAllParams);
    procedure SetGenSQLSOptions(const AValue: TGenSQLSOptions);
  protected
    procedure GenAllGenerators;
    function CommentLine:string;
  public
    procedure GenSqlGenerators(GeneratorsList:TStrings; Results:TStrings);overload;
    procedure GenSqlGenerators(Generator:string; Results:TStrings);overload;
    procedure GenSqlExceptions(ExceptionsList:TStrings; Results:TStrings);overload;
    procedure GenSqlExceptions(Exception:string; Results:TStrings);overload;
    procedure GenSqlRole(RolesList:TStrings; Results:TStrings);overload;
    procedure GenSqlRole(Role:string; Results:TStrings);overload;
    procedure GenAll(Results:TStrings);
  published
    property DataBase:TJvUIBDataBase read FDataBase write SetDataBase;
    property GenSQLSOptions:TGenSQLSOptions read FGenSQLSOptions write SetGenSQLSOptions;
    property GenAllParams:TGenAllParams read FGenAllParams write SetGenAllParams;
  end;

procedure Register;

resourcestring
  metaDescribeException =
                  'update RDB$EXCEPTIONS set RDB$DESCRIPTION = %s where RDB$EXCEPTION_NAME=''%s''';
implementation
uses StrUtils;

procedure Register;
begin
  RegisterComponents('Jv UIB',[TMetaDataSQLGenerator]);
end;

{ TMetaDataSQLGenerator }

procedure TMetaDataSQLGenerator.SetDataBase(const AValue: TJvUIBDataBase);
begin
  if FDataBase=AValue then exit;
  FDataBase:=AValue;
end;

procedure TMetaDataSQLGenerator.SetGenAllParams(const AValue: TGenAllParams);
begin
  if FGenAllParams=AValue then exit;
  FGenAllParams:=AValue;
end;

procedure TMetaDataSQLGenerator.SetGenSQLSOptions(const AValue: TGenSQLSOptions
  );
begin
  if FGenSQLSOptions=AValue then exit;
  FGenSQLSOptions:=AValue;
end;

procedure TMetaDataSQLGenerator.GenAllGenerators;
var
  MetaData:TMetaDataBase;
begin
//  MetaData:=FDataBase.GetMetadata;
end;

function TMetaDataSQLGenerator.CommentLine: string;
var
  C:integer;
begin
  Result:='';
  if gsqlIncludeUserName in FGenSQLSOptions then
    Result:=DateTimeToStr(Now);
  if gsqlIncludeDateTime in FGenSQLSOptions then
    Result:=Result+' '+FDataBase.UserName;
  C:=(74 - Length(Result)) div 2;
  Result:='/*' + DupeString('*', C) + ' '+Result+' ' + DupeString('*', C) + '*/';
end;

procedure TMetaDataSQLGenerator.GenSqlGenerators(GeneratorsList: TStrings; Results:TStrings);
var
  MetaData:TMetaDataBase;
  MetaGenerator:TMetaGenerator;
  i:integer;
begin
  MetaData:=FDataBase.GetMetadata as TMetaDataBase;
  for i:=0 to GeneratorsList.Count-1 do
  begin
    MetaGenerator:=MetaData.FindGeneratorName(GeneratorsList[i]);
    Results.Add(CommentLine);
    Results.Add(MetaGenerator.AsCreateDLL);
    if gsqlSetGenValue in FGenSQLSOptions then
      Results.Add(MetaGenerator.AsAlterDDL);
  end;
end;

procedure TMetaDataSQLGenerator.GenSqlGenerators(Generator: string;
  Results: TStrings);
var
  List:TStringList;
begin
  List:=TStringList.Create;
  try
    List.Add(Generator);
    GenSqlGenerators(List, Results);
  finally
    List.Free;
  end;
end;

procedure TMetaDataSQLGenerator.GenSqlExceptions(ExceptionsList: TStrings;
  Results: TStrings);
var
  MetaData:TMetaDataBase;
  MetaException:TMetaException;
  i:integer;
begin
  MetaData:=FDataBase.GetMetadata as TMetaDataBase;
  for i:=0 to ExceptionsList.Count-1 do
  begin
    MetaException:=MetaData.FindExceptionName(ExceptionsList[i]);
    Results.Add(CommentLine);
    if Assigned(MetaException) then
      Results.Add(MetaException.AsDDL);
{    if gsqlIncludeDescriptions in GenSQLSOptions then
      Results.Add(Format(metaDescribeException);}
  end;
end;

procedure TMetaDataSQLGenerator.GenSqlExceptions(Exception: string;
  Results: TStrings);
var
  List:TStringList;
begin
  List:=TStringList.Create;
  try
    List.Add(Exception);
    GenSqlExceptions(List, Results);
  finally
    List.Free;
  end;
end;

procedure TMetaDataSQLGenerator.GenSqlRole(RolesList: TStrings;
  Results: TStrings);
var
  MetaData:TMetaDataBase;
  MetaRole:TMetaRole;
  i:integer;
begin
  MetaData:=FDataBase.GetMetadata as TMetaDataBase;
  for i:=0 to RolesList.Count-1 do
  begin
    MetaRole:=MetaData.FindRoleName(RolesList[i]);
    Results.Add(CommentLine);
    Results.Add(MetaRole.AsDDL);
{    if gsqlIncludeDescriptions in GenSQLSOptions then
      Results.Add(Format(metaDescribeException);}
  end;
end;

procedure TMetaDataSQLGenerator.GenSqlRole(Role: string; Results: TStrings);
var
  List:TStringList;
begin
  List:=TStringList.Create;
  try
    List.Add(Role);
    GenSqlRole(List, Results);
  finally
    List.Free;
  end;
end;

procedure TMetaDataSQLGenerator.GenAll(Results: TStrings);
begin
  if gaGenerators in FGenAllParams then
    ;
end;

end.
