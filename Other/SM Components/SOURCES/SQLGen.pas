{ Copyright (C) 1998-2008, written by Mike Shkolnik, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
  WEB: http://www.scalabium.com

  This components allow to generate INSERT/UPDATE/DELETE/CREATE TABLE statements
  by TDataset

}
unit SQLGen;

interface

uses Classes, DB;

{$I SMVersion.inc}

type
  TStrFields = (sfReestr, sfList, sfWhere, sfInsert, sfCreate);

  TSMSQLGenerator = class(TComponent)
  private
    FDataset: TDataset;
    FTableName: string;

    function GetDataSet: TDataSet;
    procedure SetDataSet(AValue: TDataSet);

    function GetInsertSQL: string;
    function GetUpdateSQL: string;
    function GetDeleteSQL: string;
    function GetCreateTableSQL: string;

    function GetStrFields(stType: TStrFields): string;
    function GetFieldTypeName(AType: TFieldType): string;
    function GetFields(stType: TStrFields): string;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property InsertSQL: string read GetInsertSQL;
    property UpdateSQL: string read GetUpdateSQL;
    property DeleteSQL: string read GetDeleteSQL;
    property CreateTableSQL: string read GetCreateTableSQL;
  published
    property DataSet: TDataSet read GetDataSet write SetDataSet;
    property TableName: string read FTableName write FTableName;
  end;

procedure Register;

implementation
uses SysUtils;

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMSQLGenerator]);
end;

procedure TSMSQLGenerator.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent = FDataSet) then
      FDataSet := nil
  end;
end;

function TSMSQLGenerator.GetDataSet: TDataSet;
begin
  Result := FDataSet;
end;

procedure TSMSQLGenerator.SetDataSet(AValue: TDataSet);
begin
  FDataSet := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
end;

function TSMSQLGenerator.GetStrFields(stType: TStrFields): string;
var
  i: Integer;
  str: string;
begin
  Result := '';
  try
    for i := 0 to DataSet.FieldCount-1 do
    begin
      str := DataSet.Fields[i].FieldName;
      case stType of
        sfList: str := str + ' = <' + str + '>';
        sfInsert: str := '<' + str + '>';
      end;
      if i <> DataSet.FieldCount-1 then
        str := str + ', ';
      Result := Result + str;
    end;
  except
    Result := '';
  end;
end;

function TSMSQLGenerator.GetFieldTypeName(AType: TFieldType): string;
const
  FieldTypes: array [TFieldType] of PChar =
    ('?Unknown?', 'Char', 'Smallint', 'Integer', 'Word', 'Boolean',
     'Float', 'Currency', 'BCD', 'Date', 'Time', 'DateTime',
     'Bytes', 'VarBytes', 'AutoInc', 'Blob', 'Memo', 'Graphic',
     'Blob', 'Blob', 'Blob', 'Blob', 'Cursor'

{$IFDEF SMForDelphi4}
     ,
     'FixedChar', 'WideString', 'Largeint', 'ADT', 'Array', 'Reference',
     'DataSet'
{$ENDIF}

{$IFDEF SMForDelphi5}
     ,
     'OraBlob', 'OraClob', 'Variant', 'Interface',
     'IDispatch', 'Guid'
{$ENDIF}

{$IFDEF SMForDelphi6}
     ,
     'TimeStamp', 'FMTBcd'
{$ENDIF}

{$IFDEF SMForDelphi2006}
     ,
     'FixedWideChar',
     'WideMemo',
     'OraTimeStamp',
     'OraInterval'
{$ENDIF}

    {$IFDEF SMForDelphi2009}
     ,
     'LongWord',
     'Shortint',
     'Byte',
     'Extended',
     'Connection',
     'Params',
     'Stream'
    {$ENDIF}
     );

begin
  if AType < Low(FieldTypes) then
    AType := Low(FieldTypes)
  else
    if AType > High(FieldTypes) then
      AType := Low(FieldTypes);
  Result := UpperCase(StrPas(FieldTypes[AType]));
end;

function TSMSQLGenerator.GetFields(stType: TStrFields): string;
var
  i: Integer;
  str: string;
begin
  Result := '';

  for i := 0 to DataSet.FieldCount-1 do
  begin
    str := DataSet.Fields[i].FieldName;
    case stType of
      sfReestr: str := '   ' + str;
      sfList: str := '   ' + str + ' = <' + str + '>';
      sfWhere: if i = 0 then
                 str := '      (' + str + ' = <' + str + '>)'
               else
                 str := '  AND (' + str + ' = <' + str + '>)';
      sfInsert: str := '   ' + '<' + str + '>';
      sfCreate: with DataSet.Fields[i] do
                  begin
                    str := '   ' + str + ' ' + GetFieldTypeName(DataType);
                    if DataType = ftString then
                      str := str + '(' + IntToStr(Size) + ')';
                    if Required then
                      str := str + ' NOT';
                    str := str + ' NULL';
                  end;
    end;
    if (stType <> sfWhere) and (i <> DataSet.FieldCount-1) then
      str := str + ',';
    Result := Result + str + #13#10;
  end;
end;

function TSMSQLGenerator.GetInsertSQL: string;
begin
  Result := 'INSERT INTO ' + TableName + #13#10 +
            '   (' + GetStrFields(sfReestr) + ')'#13#10 +
            'VALUES'#13#10 +
            '   (' + GetStrFields(sfInsert) + ')';
end;

function TSMSQLGenerator.GetUpdateSQL: string;
begin
  Result := 'UPDATE ' + TableName + #13#10 +
            'SET'#13#10 +
            GetFields(sfList) + #13#10 +
            'WHERE'#13#10 +
            GetFields(sfWhere);
end;

function TSMSQLGenerator.GetDeleteSQL: string;
begin
  Result := 'DELETE FROM ' + TableName + #13#10 +
            'WHERE'#13#10 +
            GetFields(sfWhere);
end;

function TSMSQLGenerator.GetCreateTableSQL: string;
begin
end;

end.
