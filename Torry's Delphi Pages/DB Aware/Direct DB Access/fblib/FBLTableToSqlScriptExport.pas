{
   Firebird Library
   Open Source Library No Data Aware for direct access to Firebird
   Relational Database from Borland Delphi / Kylix and Freepascal

   File:FBLTableToSqlScript.pas
   Copyright (c) 2003-2004 Alessandro Batisti
   abatisti@tiscali.it
   fblib@altervista.org
   http://fblib.altervista.org

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
}
{$I fbl.inc}
unit FBLTableToSqlScriptExport;

interface

uses
  SysUtils, Classes, FBLDatabase, FBLTransaction, FBLDsql, FBLExcept, ibase_h;

type
  TFBLTableToSqlScriptExport = class(TObject)
  private
    FTableName: string;
    FDatabase: TFBLDatabase;
    FQuery: TFBLDsql;
    FTransaction: TFBLTransaction;
    FEOF: boolean;
    FCurrentLine: string;
    FSqlInsertInfo: string;
    FLineNumber: integer;
    FPrepared: boolean;
    FFieldsIn: TStringList;
    procedure Prepare;
  public
    constructor Create(ADatabase: TFBLDatabase; ATableName: string);
    destructor Destroy; override;
    procedure NextLine;
    procedure Reset;
    property EOF: boolean read FEOF write FEOF;
    property CurrentLine: string read FCurrentLine;
    property LineNumber: integer read FLineNumber;
  end;

implementation

uses
  FBLmixf;

constructor TFBLTableToSqlScriptExport.Create(ADatabase: TFBLDatabase;
  ATableName: string);
begin
  FDatabase := ADatabase;
  FTableName := ATableName;
  FTransaction := TFBLTransaction.Create(nil);
  FQuery := TFBLDsql.Create(nil);
  FTransaction.Database := FDatabase;
  FQuery.Transaction := FTransaction;
  FFieldsIn := TStringList.Create;
  FPrepared := False;
end;

//------------------------------------------------------------------------------

destructor TFBLTableToSqlScriptExport.Destroy;
begin
  FQuery.Free;
  FTransaction.Free;
  FFieldsIn.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TFBLTableToSqlScriptExport.Prepare;
var
  CanInsert: boolean;
begin
  if not FDatabase.Connected then
    FBLExcept.FBLError('Database is not connected');
  if not FTransaction.InTransaction then
    FTransaction.StartTransaction;
  FQuery.AutoFetchFirst := True;
  FQuery.SQL.Text := 'SELECT ' +
    'A.RDB$FIELD_NAME,' +        //0
    'B.RDB$FIELD_TYPE,' +        //1
    'B.RDB$COMPUTED_SOURCE ' +   //2
    'FROM RDB$RELATION_FIELDS A ' +
    'INNER JOIN  RDB$FIELDS B ON A.RDB$FIELD_SOURCE = B.RDB$FIELD_NAME ' +
    'WHERE RDB$RELATION_NAME = ? ' +
    'ORDER BY A.RDB$FIELD_POSITION';
  FQuery.Prepare;
  FQuery.ParamAsString(0,FTableName);
  FQuery.ExecSQL;
  FSqlInsertInfo := 'INSERT INTO ' + FTableName + '(';
  FFieldsIn.Clear;
  while not FQuery.EOF do
  begin
    if FQuery.FieldIsNull(2) and
      (FQuery.FieldAsInteger(1) <> 9) and     {9 = QUAD(Array) ; 261 = BLOB}
      (FQuery.FieldAsInteger(1) <> 261) then
    begin
      FFieldsIn.Add('Y');
      CanInsert := True;
      FSqlInsertInfo := FSqlInsertInfo + Trim(FQuery.FieldAsString(0));
    end
    else
    begin
      CanInsert := False;
      FFieldsIn.Add('N');
    end;
    FQuery.Next;
    if CanInsert then
      FSqlInsertInfo := FSqlInsertInfo + ',';
  end;

  if FQuery.FetchCount = 0 then
  begin
    FQuery.Close;
    FTransaction.RollBack;
    FBLExcept.FBLError('Table "%s" does not exist', [FTablename]);
    Fprepared := False;
  end
  else
  begin
    FSqlInsertInfo[Length(FSqlInsertInfo)] := ')';
    FQuery.UnPrepare;
    FQuery.AutoFetchFirst := False;
    FQuery.SQL.Text := 'SELECT * FROM ' + FTableName;
    FQuery.ExecSQL;
    FPrepared := True;
    FLineNumber := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TFBLTableToSqlScriptExport.NextLine;
var
  i: integer;
  SqlValues: string;
  TempDecimalSeparator: char;
begin
  TempDecimalSeparator := DecimalSeparator;
  if FEOF then Exit;
  if not FPrepared then
    Prepare;
  SqlValues := '(';
  FQuery.Next;
  if not FQuery.EOF then
  begin
    for i := 0 to FQuery.FieldCount - 1 do
    begin
      if FFieldsIn.Strings[i] = 'Y' then
      begin
        if FQuery.FieldIsNull(i) then
          SqlValues := SqlValues + 'NULL'
        else
          case FQuery.FieldType(i) of
            SQL_VARYING, SQL_TEXT:
              SqlValues := SqlValues + QuotedStr(Trim(FQuery.FieldAsString(i)));
            SQL_TYPE_DATE:
              SqlValues := SqlValues + QuotedStr(DateToSql(FQuery.FieldAsDateTime(i)));
            SQL_TYPE_TIME:
              SqlValues := SqlValues + QuotedStr(TimeToSql(FQuery.FieldAsDateTime(i)));
            SQL_TIMESTAMP:
              SqlValues := SqlValues + QuotedStr(DateTimeToSql(FQuery.FieldAsDateTime(i)));
            else
              begin
                try
                  DecimalSeparator := '.';
                  SqlValues := SqlValues + FQuery.FieldAsString(i);
                finally
                  DecimalSeparator := TempDecimalSeparator;
                end;
              end;
          end;
        SqlValues := SqlValues + ',';
      end;
    end;
    Inc(FLineNumber);
    SqlValues[Length(SqlValues)] := ')';
    FCurrentLine := FSqlInsertInfo + ' VALUES ' + SqlValues + ';';
  end
  else
  begin
    FEOF := True;
    FQuery.UnPrepare;
    FTransaction.RollBack;
    FCurrentLine := '';
  end;
end;

//------------------------------------------------------------------------------

procedure TFBLTableToSqlScriptExport.Reset;
begin
  FPrepared := False;
  FEOF := False;
  FLineNumber := 0;
  FCurrentLine := '';
end;

//------------------------------------------------------------------------------

end.
