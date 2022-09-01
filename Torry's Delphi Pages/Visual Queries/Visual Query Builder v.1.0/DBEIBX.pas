unit DBEIBX;

interface

Uses VQBuilder, IBDataBase, IBQuery, Classes, IBTable, VQBConst, DB, DBTables, VQBField,
     SysUtils;

Type
   TDBEngineIBX = class(TDBEngine)
   Private
     FQuery: TIBQuery;
     FUDF1: TIBQuery;
     FTable: TIBTable;
     FDataBase: TIBDataBase;
     FTransaction: TIBTransaction;
     FShowSystemTables: Boolean;
     procedure SetDataBase(const Value: TIBDataBase);
     procedure SetShowSystemTables(const Value: boolean);
   Protected
     procedure SetQuerySQL(const Value: string); override;
     Procedure ReadFieldList(const ATableName: string); override;
     Procedure ReadTableList; override;
     function ResultQuery: TDataSet; override;
     procedure OpenResultQuery; override;
     procedure CloseResultQuery; override;
     procedure ClearQuerySQL; override;
     Procedure ReadUDF;
     Function ConvertTableName(const AName: string): string; override;
     Function ConvertFieldName(const AName: string): string; override;
     Function FindFullName(ListBox: TCheckListBoxEx): String; override;
   Public
     Constructor Create(AOwner: TComponent); override;
     Destructor Destroy; override;
   Published
     Property DataBase: TIBDataBase read FDataBase write SetDataBase;
     Property ShowSystemTables: boolean read FShowSystemTables write SetShowSystemTables default false;
   end;

const
   IBXKeyWords = ' ACTION ACTIVE ADD ADMIN AFTER ALL ALTER AND ANY AS ASC ASCENDING '+

' AT AUTO AUTODDL AVG BASED BASENAME BASE_NAME BEFORE BEGIN BETWEEN BLOB BLOBEDIT ' +
' BUFFER BY CACHE CASCADE CAST CHAR CHARACTER CHARACTER_LENGTH CHAR_LENGTH CHECK ' +
' CHECK_POINT_LEN CHECK_POINT_LENGTH COLLATE COLLATION COLUMN COMMIT COMMITTED COMPILETIME '+
' COMPUTED CLOSE CONDITIONAL CONNECT CONSTRAINT CONTAINING CONTINUE COUNT CREATE '+
' CSTRING CURRENT CURRENT_DATE CURRENT_TIME CURRENT_TIMESTAMP CURSOR DATABASE DATE DAY '+
' DB_KEY DEBUG DEC DECIMAL DECLARE DEFAULT DELETE DESC DESCENDING DESCRIBE DESCRIPTOR DISCONNECT '+
' DISPLAY DISTINCT DO DOMAIN DOUBLE DROP ECHO EDIT ELSE END ENTRY_POINT ESCAPE ' +
' EVENT EXCEPTION EXECUTE EXISTS EXIT EXTERN EXTERNAL EXTRACT FETCH FILE FILTER FLOAT ' +
' FOR FOREIGN FOUND FREE_IT FROM FULL FUNCTION GDSCODE GENERATOR GEN_ID GLOBAL GOTO '+
' GRANT GROUP GROUP_COMMIT_WAIT GROUP_COMMIT_ WAIT_TIME HAVING HELP HOUR IF IMMEDIATE IN INACTIVE '+
' INDEX INDICATOR INIT INNER INPUT INPUT_TYPE INSERT INT INTEGER INTO IS ISOLATION ISQL JOIN KEY '+
' LC_MESSAGES LC_TYPE LEFT LENGTH LEV LEVEL LIKE LOGFILE LOG_BUFFER_SIZE LOG_BUF_SIZE LONG MANUAL '+
' MAX MAXIMUM MAXIMUM_SEGMENT MAX_SEGMENT MERGE MESSAGE MIN MINIMUM MINUTE MODULE_NAME MONTH NAMES '+
' NATIONAL NATURAL NCHAR NO NOAUTO NOT NULL NUMERIC NUM_LOG_BUFS NUM_LOG_BUFFERS OCTET_LENGTH OF '+
' ON ONLY OPEN OPTION OR ORDER OUTER OUTPUT OUTPUT_TYPE OVERFLOW PAGE PAGELENGTH PAGES PAGE_SIZE PARAMETER '+
' PASSWORD PLAN POSITION POST_EVENT PRECISION PREPARE PROCEDURE PROTECTED PRIMARY PRIVILEGES PUBLIC QUIT '+
' RAW_PARTITIONS RDB$DB_KEY READ REAL RECORD_VERSION REFERENCES RELEASE RESERV RESERVING '+
' RESTRICT RETAIN RETURN RETURNING_VALUES RETURNS REVOKE RIGHT ROLE ROLLBACK RUNTIME SCHEMA SECOND SEGMENT SELECT SET '+
' SHADOW SHARED SHELL SHOW SINGULAR SIZE SMALLINT SNAPSHOT SOME SORT SQLCODE SQLERROR SQLWARNING STABILITY STARTING '+
' STARTS STATEMENT STATIC STATISTICS SUB_TYPE SUM SUSPEND TABLE TERMINATOR THEN TIME TIMESTAMP '+
' TO TRANSACTION TRANSLATE TRANSLATION TRIGGER TRIM TYPE UNCOMMITTED UNION UNIQUE UPDATE UPPER '+
' USER USING VALUE VALUES VARCHAR VARIABLE VARYING VERSION VIEW WAIT WEEKDAY WHEN WHENEVER WHERE WHILE WITH WORK WRITE YEAR YEARDAY ';

IBXFunc: array[0..9] of string = ('CAST ', 'SOME ', 'CONTAINING ', 'STARTING ', 'WITH ',
                                  'UPPER ', 'LOWER ', 'TRIM ', 'SUBSTRING ', 'EXTRACT ');
IBXConst: array[0..6] of String = (' || ', 'YEAR ', 'MONTH ', 'DAY ', 'HOUR ', 'MINUTE ', 'SECOND ');

implementation

{ TDBEngineIBX }

procedure TDBEngineIBX.ClearQuerySQL;
begin
   FQuery.SQL.Clear;
end;

procedure TDBEngineIBX.CloseResultQuery;
begin
  FQuery.Close;
end;

function TDBEngineIBX.ConvertFieldName(const AName: string): string;
begin
   Result:=ConvertTableName(AName);
end;

function TDBEngineIBX.ConvertTableName(const AName: string): string;
var
  i: integer;
begin
   Result:= AName;
   if Pos(#32 + AName + #32, IBXKeyWords) = 0 then
   begin
      for i:= 1 to Length(AName) do
      begin
         if  (not (AName[i] in ['A'..'Z', '0'..'9', '$', '_'])) then
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

constructor TDBEngineIBX.Create(AOwner: TComponent);
begin
  inherited;
  FShowSystemTables:= False;
  FQuery:= TIBQuery.Create(Self);
  FTable:= TIBTable.Create(Self);
  FUDF1:= TIBQuery.Create(Self);
  FTransaction:= TIBTransaction.Create(Self);
  FQuery.Transaction:= FTransaction;
  FTable.Transaction:= FTransaction;
  FUDF1.Transaction:= FTransaction;
end;

destructor TDBEngineIBX.Destroy;
begin
  FQuery.Free;
  FTransaction.Free;
  FTable.Free;
  FUDF1.Free;
  inherited;
end;

function TDBEngineIBX.FindFullName(ListBox: TCheckListBoxEx): String;
begin
    Result:= ConvertTableName(ListBox.TableName) + ' ';
    if ListBox.TableName <> ListBox.TableAlias then
    Result:= Result + ConvertTableName(ListBox.TableAlias) + ' ';
end;

procedure TDBEngineIBX.OpenResultQuery;
begin
  FQuery.Open;
  FQuery.FetchAll;
end;

procedure TDBEngineIBX.ReadFieldList(const ATableName: string);
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

procedure TDBEngineIBX.ReadTableList;
begin
   TableList.Clear;
   FQuery.Database.GetTableNames(TableList, ShowSystemTables);
end;

procedure TDBEngineIBX.ReadUDF;
var
  ts: string;
begin
   FUDF1.SQL.Text:= 'Select RDB$FUNCTION_NAME, RDB$DESCRIPTION, RDB$MODULE_NAME FROM RDB$FUNCTIONS ORDER BY RDB$FUNCTION_NAME';
   FUDF1.Open;
   FUDF1.First;
   UDFList.Clear;
   While not FUDF1.Eof do
   begin
      TS:= Trim(FUDF1.fieldByName('RDB$FUNCTION_NAME').AsString)+ ' @'+
           'Module: '+TRIM(FUDF1.fieldByName('RDB$MODULE_NAME').AsString)+'@'+
           FUDF1.fieldByName('RDB$DESCRIPTION').AsString;
      UDFList.Add(TS);
      FUDF1.Next;
   end;
end;

function TDBEngineIBX.ResultQuery: TDataSet;
begin
   Result:= FQuery;
end;

procedure TDBEngineIBX.SetDataBase(const Value: TIBDataBase);

begin
  FDataBase := Value;
  FDataBase.AddTransaction(FTransaction);
  FTransaction.AddDatabase(FDataBase);
  FQuery.Database:= Value;
  FTable.Database:= Value;
  FUDF1.Database:= Value;
  ReadUDF;
end;

procedure TDBEngineIBX.SetQuerySQL(const Value: string);
begin
   FQuery.SQL.Text:= Value;
end;

procedure TDBEngineIBX.SetShowSystemTables(const Value: boolean);
begin
  FShowSystemTables := Value;
end;

end.
