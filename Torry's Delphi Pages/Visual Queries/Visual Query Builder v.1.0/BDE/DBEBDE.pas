unit DBEBDE;

interface
Uses VQBuilder, VQBConst, DBTables, DB, Classes, VQBField, SysUtils;

Type
   TDBEngineBDE = class(TDBEngine)
   Private
     FQuery: TQuery;
     FTable: TTable;
     FDataBase: TDataBase;
     FShowSystemTables: boolean;
     procedure SetDataBase(const Value: TDataBase);
     procedure SetShowSystemTables(const Value: boolean);
   Protected
     procedure SetQuerySQL(const Value: string); override;
     Procedure ReadFieldList(const ATableName: string); override;
     Procedure ReadTableList; override;
     function ResultQuery: TDataSet; override;
     procedure OpenResultQuery; override;
     procedure CloseResultQuery; override;
     procedure ClearQuerySQL; override;
     Function ConvertTableName(const AName: string): string; override;
     Function ConvertFieldName(const AName: string): string; override;
     Function FindFullName(ListBox: TCheckListBoxEx): String; override;
   Public
     Constructor Create(AOwner: TComponent); override;
     Destructor Destroy; override;
   Published
     Property DataBase: TDataBase read FDataBase write SetDataBase;
     Property ShowSystemTables: boolean read FShowSystemTables write SetShowSystemTables;
   end;

const
   BDEKeyWords = ' ACTIVE ADD ALL AFTER ALTER AND ANY AS ASC ASCENDING AT AUTO AUTOINC AVG ' +

' BASE_NAME BEFORE BEGIN BETWEEN BLOB BOOLEAN BOTH BY BYTES CACHE CAST CHAR CHARACTER CHECK ' +
' CHECK_POINT_LENGTH COLLATE COLUMN COMMIT COMMITTED COMPUTED CONDITIONAL CONSTRAINT CONTAINING ' +
' COUNT CREATE CSTRING CURRENT CURSOR DATABASE DATE DAY DEBUG DEC DECIMAL DECLARE DEFAULT DELETE ' +
' DESC DESCENDING DISTINCT DO DOMAIN DOUBLE DROP ELSE END ENTRY_POINT ESCAPE EXCEPTION EXECUTE '+
' EXISTS EXIT EXTERNAL EXTRACT FILE FILTER FLOAT FOR FOREIGN FROM FULL FUNCTION GDSCODE GENERATOR '+
' GEN_ID GRANT GROUP GROUP_COMMIT_WAIT_TIME HAVING, HOUR IF IN INT INACTIVE INDEX INNER INPUT_TYPE '+
' INSERT INTEGER INTO IS ISOLATION JOIN KEY LONG LENGTH LOGFILE LOWER LEADING LEFT LEVEL LIKE '+
' LOG_BUFFER_SIZE MANUAL MAX MAXIMUM_SEGMENT MERGE MESSAGE MIN MINUTE MODULE_NAME MONEY MONTH '+
' NAMES NATIONAL NATURAL NCHAR NO NOT NULL NUM_LOG_BUFFERS NUMERIC OF ON ONLY OPTION OR ORDER '+
' OUTER OUTPUT_TYPE OVERFLOW PAGE_SIZE PAGE PAGES PARAMETER PASSWORD PLAN POSITION POST_EVENT '+
' PRECISION PROCEDURE PROTECTED PRIMARY PRIVILEGES RAW_PARTITIONS RDB$DB_KEY READ REAL RECORD_VERSION '+
' REFERENCES RESERV RESERVING RETAIN RETURNING_VALUES RETURNS REVOKE RIGHT ROLLBACK SECOND SEGMENT '+
' SELECT SET SHARED SHADOW SCHEMA SINGULAR SIZE SMALLINT SNAPSHOT SOME SORT SQLCODE STABILITY STARTING '+
' STARTS STATISTICS SUB_TYPE SUBSTRING SUM SUSPEND TABLE THEN TIME TIMESTAMP TIMEZONE_HOUR TIMEZONE_MINUTE '+
' TO TRAILING TRANSACTION TRIGGER TRIM UNCOMMITTED UNION UNIQUE UPDATE UPPER USER VALUE VALUES VARCHAR '+
' VARIABLE VARYING VIEW WAIT WHEN WHERE WHILE WITH WORK WRITE YEAR ';

implementation

{ TDBEngineBDE }

procedure TDBEngineBDE.ClearQuerySQL;
begin
   FQuery.SQL.Clear;
end;

procedure TDBEngineBDE.CloseResultQuery;
begin
   FQuery.Close;
end;

function TDBEngineBDE.ConvertFieldName(const AName: string): string;
var
  i: integer;
begin
   Result:= AName;
   if (Pos(#32 + AnsiUpperCase(AName) + #32, BDEKeyWords) = 0) then
   begin
      for i:= 1 to Length(AName) do
      begin
         if  (not (AName[i] in ['A'..'Z', 'a'..'z', '0'..'9', '$', '_'])) then
         begin
            Result:= '"' + AName + '"';
            exit;
         end;
      end;
   end
     else
   begin
      Result:= '"' + AName + '"';
   end;
end;

function TDBEngineBDE.ConvertTableName(const AName: string): string;
   Function MoreThanOne(Name: String): boolean;
   var
     i,j: integer;
   begin
      Result:= False;
      j:= 0;
      for i:= 1 to Length(Name) do
      begin
         if Name[i] = '.' then inc(j);
         if j> 1 then
         begin
            Result:= True;
            exit;
         end;
      end;
   end;
var
  i: integer;
begin
   Result:= AName;
   if (Pos(#32 + AName + #32, BDEKeyWords) = 0) and (not MoreThanOne(AName)) then
   begin
      for i:= 1 to Length(AName) do
      begin
         if  (not (AName[i] in ['A'..'Z', 'a'..'z', '.', '0'..'9', '$', '_'])) then
         begin
            Result:= '"' + AName + '"';
            exit;
         end;
      end;
   end
     else
   begin
      Result:= '"' + AName + '"';
   end;
end;

constructor TDBEngineBDE.Create(AOwner: TComponent);
begin
  inherited;
  FShowSystemTables:= False;
  FQuery:= TQuery.Create(Self);
  FTable:= TTable.Create(Self);
end;

destructor TDBEngineBDE.Destroy;
begin
  FQuery.Free;
  FTable.Free;
  inherited;
end;

function TDBEngineBDE.FindFullName(ListBox: TCheckListBoxEx): String;
begin
    Result:= ConvertTableName(ListBox.TableName) + ' ';
    if ListBox.TableName <> ListBox.TableAlias then
    Result:= Result + ConvertTableName(ListBox.TableAlias) + ' ';
end;

procedure TDBEngineBDE.OpenResultQuery;
begin
   FQuery.Open;
   FQuery.FetchAll;
end;

procedure TDBEngineBDE.ReadFieldList(const ATableName: string);
var
  i: integer;
  FType: String;
begin
   FieldList.Clear;
   SystemList.Clear;
   if FTable.Active then FTable.Active:= False;
   FTable.TableName:= ATableName;
   FTable.Open;
   for i:= 0 to FTable.FieldCount - 1 do
   begin
      FType:= FieldType[Ord(FTable.FieldByName(FTable.Fields[i].FieldName).DataType)];
      FieldList.Add(FTable.Fields[i].FieldName);
      SystemList.Add(FType);
   end;
end;

procedure TDBEngineBDE.ReadTableList;
begin
  TableList.Clear;
  DataBase:= Session.OpenDatabase(DataBase.DatabaseName);
  Try
    Session.GetTableNames(DataBase.DatabaseName, '', True, ShowSystemTables, TableList);
  Finally
    Session.CloseDatabase(DataBase);
  End;
end;

function TDBEngineBDE.ResultQuery: TDataSet;
begin
   Result:= FQuery;
end;

procedure TDBEngineBDE.SetDataBase(const Value: TDataBase);
begin
   FDataBase:= Value;
   FQuery.DatabaseName:= Value.DatabaseName;
   FTable.DatabaseName:= Value.DatabaseName;
end;

procedure TDBEngineBDE.SetQuerySQL(const Value: string);
begin
  FQuery.SQL.Text:= Value;
end;

procedure TDBEngineBDE.SetShowSystemTables(const Value: boolean);
begin
   FShowSystemTables := Value;
end;

end.
