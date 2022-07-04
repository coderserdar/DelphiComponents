unit SQLScript;

{*************************************************************}
{*                 Freeware dbExpress Plus                   *}
{* Copyright (c) Business Software Systems, Inc. 1995 - 2001 *}
{*                   All rights reserved.                    *}
{*************************************************************}

{$N+,P+,S-,R-}
                                
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, DBxpress, SqlExpr;

type
  TSQLScript = class(TComponent)
  private
    FAbout: String;
    FCommitEach: Boolean;
    FCurrentSQL: String;
    FDebug: Boolean;
    FSQLProc: Boolean;
    FRecordsAffected: LongWord;
    FSQL: TStrings;
    FSQLConnection: TSQLConnection;
    FSQLCount: Integer;
    FSQLCurrentLine: Integer;
    FSQLCurrentPos: Integer;
    FTranDesc: TTransactionDesc;
    procedure SetQuery(Value: TStrings);
    procedure SetSQLProc(Value: Boolean);
    procedure SetSQLCount;
    function GetSQLStatement: String;
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CurrentSQL: String read FCurrentSQL;
    property TransDesc: TTransactionDesc read FTranDesc write FTranDesc;
    property RecordsAffected: LongWord read FRecordsAffected;
    function ExecuteDirect: LongWord;
    { Public declarations }
  published
    property About: String read FAbout;
    property CommitEach: Boolean read FCommitEach write FCommitEach;
    property Debug: Boolean read FDebug write FDebug;
    property Name;
    property SQLProc: Boolean read FSQLProc write SetSQLProc;
    property SQLConnection: TSQLConnection read FSQLConnection write FSQLConnection;
    property SQL: TStrings read FSQL write SetQuery;
    property Tag;
  end;

implementation

constructor TSQLScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAbout := 'Ver 0.8.0.7';
  FSQL := TStringList.Create;
end;

destructor TSQLScript.Destroy;
begin
  FSQL.Free;
  inherited Destroy;
end;

procedure TSQLScript.SetQuery(Value: TStrings);
begin
  if SQL.Text <> Value.Text then SQL.Assign(Value);
end;

procedure TSQLScript.SetSQLProc(Value: Boolean);
begin
  if Value <> FSQLProc then begin
    FSQLProc := Value;
    FCommitEach := False;
  end;
end;

procedure TSQLScript.SetSQLCount;
var
  SQLLineNum, SQLPos : Integer;
  SQLTestStr : String;
  IsStringExp: Boolean;
begin
  IsStringExp := False;
  for SQLLineNum := 0 to SQL.Count-1 do begin
    SQLTestStr := Trim(SQL[SQLLineNum]);
    for SQLPos := 1 to Length(SQLTestStr) do begin
      if SQLTestStr[SQLPos] = '''' then IsStringExp := not(IsStringExp);
      if (SQLTestStr[SQLPos] = ';') and (IsStringExp = False) then
         FSQLCount := FSQLCount+1;
    end;
  end;
  if SQLTestStr > #32 then begin
    if SQLTestStr[Length(SQLTestStr)] <> ';' then FSQLCount := FSQLCount+1;
  end;
end;

function TSQLScript.GetSQLStatement : String;
var
  SQLLineNum, SQLPos : Integer;
  TempSQL, SQLTestStr : String;
  BreakStatement, IsStringExp: Boolean;
begin
  BreakStatement := False;
  IsStringExp := False;
  SQLTestStr := '';
  TempSQL := '';
  for SQLLineNum := FSQLCurrentLine to SQL.Count-1 do begin
    TempSQL := TempSQL + ' ';
    SQLTestStr := Trim(SQL[SQLLineNum]);
    for SQLPos := 1 to Length(SQLTestStr) do begin
      if ((SQLLineNum = FSQLCurrentLine) and (SQLPos > FSQLCurrentPos)) or
         (SQLLineNum > FSQLCurrentLine) then begin
        if SQLTestStr[SQLPos] = '''' then IsStringExp := not(IsStringExp);
        if (SQLTestStr[SQLPos] = ';') and (IsStringExp = False) then begin
          FSQLCurrentLine := SQLLineNum;
          FSQLCurrentPos := SQLPos;
          TempSQL := Trim(TempSQL);
          BreakStatement := True;
          Break;
          end
        else begin
          TempSQL := TempSQL + SQLTestStr[SQLPos];
        end;
      end;
    end;
    if BreakStatement = True then Break;
  end;
  Result := TempSQL;
end;

function TSQLScript.ExecuteDirect: LongWord;
var
  SQLStatementNum: Integer;
begin
  Result := 0;
  if SQL.Count > 0 then begin

    if SQLConnection = nil then
       raise Exception.Create('SQLConnection unassigned.');

    FSQLCount := 0;
    FSQLCurrentLine := 0;
    FSQLCurrentPos := 0;
    SetSQLCount;

    if FSQLProc = True then begin
      try
        FCurrentSQL := SQL.Text;
        if FDebug = True then ShowMessage(FCurrentSQL);
        Result := SQLConnection.ExecuteDirect(FCurrentSQL);
        if (FCommitEach = True) and (SQLConnection.InTransaction = True) then
           SQLConnection.Commit(FTranDesc);
      except
        on E: Exception do begin
          if (FCommitEach = True) then SQLConnection.RollBack(FTranDesc);
          raise;
        end;
      end;
      end
    else begin
      try
        for SQLStatementNum := 1 to FSQLCount do begin
          FCurrentSQL := GetSQLStatement;
          if FDebug = True then ShowMessage(FCurrentSQL);
          Result := SQLConnection.ExecuteDirect(FCurrentSQL);
          if (FCommitEach = True) and (SQLConnection.InTransaction = True) then begin
            SQLConnection.Commit(FTranDesc);
          end;
        end;
      except
        on E: Exception do begin
          if (FCommitEach = True) then SQLConnection.RollBack(FTranDesc);
          raise;
        end;
      else
        raise;
      end;
    end;
  end;
end;

end.


