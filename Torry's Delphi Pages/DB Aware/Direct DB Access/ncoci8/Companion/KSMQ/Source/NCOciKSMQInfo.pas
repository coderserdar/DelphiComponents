{*************************************************************}
{               Simple Query pack version 1.7                 }
{    Copyright © 1998,99 Sergey Korzh, Sergey Lelyushkin      }
{            http://www.korzh.com/simplequery.htm             }
{                   mailto:info@korzh.com                     }
{*************************************************************}
{*******************************************************}
{File:      NCOciKSMQInfo.PAS                           }
{Revision:  0.01.00 / 10.05.2000                        }
{Comment:   NC OCI8 VCL: KSMQ integration with NCOCI8   }
{Copyright: (c) 1999-2000, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, dmitrya@inthink.com         }
{*******************************************************}

unit NCOciKSMQInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, KDbInfo, NCOci, NCOciWrapper, NCOciDB;

type
  TOciKSMQInfo = class(TKDBInfo)
  private
    FShowSystemTables: Boolean;
    FShowViews: Boolean;
    FShowSpecialFields: Boolean;
  protected
    procedure TablesByDatabase(TablesList : TStrings); override;
    procedure FieldsByTable(const TableName : string; FieldsList : TDBFieldList); override;
  public
    procedure GetDatabaseNames(AList : TStrings); override;
    procedure GetSQLValues(SQL : String; AValues, AItems : TStrings); override;
  published
    property DatabaseName;
    property ShowSystemTables: Boolean read FShowSystemTables write FShowSystemTables default False;
    property ShowViews: Boolean read FShowViews write FShowViews default False;
    property ShowSpecialFields: Boolean read FShowSpecialFields write FShowSpecialFields default False; 
  end;

procedure Register;

implementation

procedure TOciKSMQInfo.GetDatabaseNames(AList : TStrings);
begin
  TOCIDatabase.GetObjectsList('', AList, '', okDatabase, ShowSystemTables);
end;

procedure TOciKSMQInfo.TablesByDatabase(TablesList : TStrings);
begin
  TOCIDatabase.GetObjectsList(DatabaseName, TablesList,
    '', okSelectable, ShowSystemTables);
end;

procedure TOciKSMQInfo.FieldsByTable(const TableName : string; FieldsList : TDBFieldList);
var
  FI: TDBFieldInfo;
  Q: TOCIQuery;
  i: integer;
  S: String;
begin
  Q := TOCIQuery.Create(nil);
  try
    Q.DatabaseName := DatabaseName;
    Q.FetchParams.RowsetSize := 1;
    S := 'select t.*';
    if ShowSpecialFields then
      S := S + ', t.rowid, t.rownum';
    S := S + ' from ' + TableName + ' t where 1 = 0';
    Q.SQL.Text := S;
    Q.Open;
    FieldsList.Clear;
    for i := 0 to Q.FieldCount - 1 do
    begin
      FI := TDBFieldInfo.Create;
      FI.FieldName := Q.FieldDefs[i].Name;
      FI.FieldType := Q.FieldDefs[i].DataType;
      FI.FieldSize := Q.FieldDefs[i].Size;
      FieldsList.Add(FI);
    end;
    Q.Close;
  finally
    Q.Free;
  end;
end;

procedure TOciKSMQInfo.GetSQLValues(SQL : String; AValues, AItems : TStrings);
var
  Q: TOCIQuery;
begin
  Screen.Cursor := crHourGlass;
  AValues.Clear;
  AItems.Clear;
  Q := TOCIQuery.Create(nil);
  try
    Q.DatabaseName := DatabaseName;
    Q.SQL.Text := SQL;
    Q.Open;
    if Q.FieldCount < 2 then
      raise EKDBInfoError.Create('Field count must be >= 2');
    while not Q.EOF do begin
      AValues.Add(Q.Fields[0].AsString);
      AItems.Add(Q.Fields[1].AsString);
      Q.Next;
    end;
  finally
    Q.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure Register;
begin
  RegisterComponents('SimpleQuery', [TOciKSMQInfo]);
end;

end.
