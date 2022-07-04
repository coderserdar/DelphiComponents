unit dbExpressPlus;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DBXpress, DB, SqlExpr, SQLMetaData,
  SQLScript, ComCtrls, FMTBcd, Grids, DBGrids, Provider, DBClient, DBLocal,
  DBLocalS, SQLDataPump;

type
  TF_DbExpPlus = class(TForm)
    SQLMetaData1: TSQLMetaData;
    SQLScript1: TSQLScript;
    SQLConnection1: TSQLConnection;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    SQLDataPump: TTabSheet;
    m_SQLScript: TMemo;
    b_SQLExec: TButton;
    b_MDLevel1: TButton;
    lb_MDLevel1: TListBox;
    cb_MDLevel1: TComboBox;
    b_MDLevel2: TButton;
    cb_MDLevel2: TComboBox;
    l_MDLevel1: TLabel;
    l_MDLevel2: TLabel;
    lb_MDLevel2: TListBox;
    l_MDLevel3: TLabel;
    cb_MDLevel3: TComboBox;
    b_MDLevel3: TButton;
    lb_MDLevel3: TListBox;
    b_SQLScript1: TButton;
    b_SQLScript2: TButton;
    b_SQLScript3: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    SQLClientDataSet1: TSQLClientDataSet;
    SQLClientDataSet1KEY_FIELD: TStringField;
    SQLClientDataSet1KEY_DESC: TStringField;
    SQLDataPump1: TSQLDataPump;
    b_DPCreate: TButton;
    md_Destination: TSQLMetaData;
    md_Source: TSQLMetaData;
    b_DPDrop: TButton;
    b_DPDataPump: TButton;
    DataSource2: TDataSource;
    SQLClientDataSet2: TSQLClientDataSet;
    SQLClientDataSet2EMP_NO: TSmallintField;
    SQLClientDataSet2FIRST_NAME: TStringField;
    SQLClientDataSet2LAST_NAME: TStringField;
    SQLClientDataSet2PHONE_EXT: TStringField;
    SQLClientDataSet2HIRE_DATE: TSQLTimeStampField;
    SQLClientDataSet2DEPT_NO: TStringField;
    SQLClientDataSet2JOB_CODE: TStringField;
    SQLClientDataSet2JOB_GRADE: TSmallintField;
    SQLClientDataSet2JOB_COUNTRY: TStringField;
    SQLClientDataSet2SALARY: TFMTBCDField;
    SQLClientDataSet2FULL_NAME: TStringField;
    SQLClientDataSet2LITERAL_EXAMPLE: TStringField;
    SQLClientDataSet2EVENT_EXAMPLE: TStringField;
    DBGrid2: TDBGrid;
    procedure cb_MDLevel1Change(Sender: TObject);
    procedure b_MDLevel1Click(Sender: TObject);
    procedure b_MDLevel2Click(Sender: TObject);
    procedure b_MDLevel3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure b_SQLExecClick(Sender: TObject);
    procedure b_SQLScript1Click(Sender: TObject);
    procedure b_SQLScript2Click(Sender: TObject);
    procedure b_SQLScript3Click(Sender: TObject);
    procedure m_SQLScriptEnter(Sender: TObject);
    procedure b_DPCreateClick(Sender: TObject);
    procedure b_DPDataPumpClick(Sender: TObject);
    procedure b_DPDropClick(Sender: TObject);
    procedure SQLDataPump1DestinationFields11BeforeFieldPump(
      Index: Integer; Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    i_ShowGrid: boolean;
  end;

var
  F_DbExpPlus: TF_DbExpPlus;

implementation

{$R *.dfm}

procedure TF_DbExpPlus.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
  i_ShowGrid := False;
  DBGrid1.Visible := False;
end;

procedure TF_DbExpPlus.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SQLClientDataSet1.Close;
  SQLClientDataSet2.Close;
  SQLConnection1.Close;  
end;

procedure TF_DbExpPlus.cb_MDLevel1Change(Sender: TObject);
begin
  cb_MDLevel2.ItemIndex := -1;
  cb_MDLevel3.ItemIndex := -1;
  lb_MDLevel1.Items.Clear;
  lb_MDLevel2.Items.Clear;
  lb_MDLevel3.Items.Clear;  
end;

procedure TF_DbExpPlus.b_MDLevel1Click(Sender: TObject);
begin
  if cb_MDLevel1.ItemIndex = -1 then Exit;
  lb_MDLevel1.Items.Clear;
  lb_MDLevel2.Items.Clear;
  cb_MDLevel2.ItemIndex := -1;
  lb_MDLevel3.Items.Clear;
  cb_MDLevel3.ItemIndex := -1;
  case cb_MDLevel1.ItemIndex of
  0: SQLMetaData1.GetTableNames(lb_MDLevel1.Items);
  1: SQLMetaData1.GetSysTableNames(lb_MDLevel1.Items);
  2: SQLMetaData1.GetViewNames(lb_MDLevel1.Items);
  end;
  lb_MDLevel1.ItemIndex := 0;
end;

procedure TF_DbExpPlus.b_MDLevel2Click(Sender: TObject);
var
  aStr, aLevel1: string;
begin
  if cb_MDLevel2.ItemIndex = -1 then Exit;
  if not(lb_MDLevel1.Items.Count > 0) then begin
    Application.MessageBox(pchar('Database Object must be selected.'),
       'SQLMetaData', mb_Ok+mb_IconInformation);
    Exit;
  end;
  lb_MDLevel3.Items.Clear;
  cb_MDLevel3.ItemIndex := -1;
  aLevel1 := lb_MDLevel1.Items[lb_MDLevel1.ItemIndex];
  lb_MDLevel1.Items.Clear;
  lb_MDLevel1.Items.Add(aLevel1);
  lb_MDLevel1.ItemIndex := 0;
  lb_MDLevel2.Items.Clear;
  case cb_MDLevel2.ItemIndex of
  0: SQLMetaData1.GetFieldNames(aLevel1, lb_MDLevel2.Items);
  1: SQLMetaData1.GetPrimaryKeyFieldNames(aLevel1, lb_MDLevel2.Items);
  2: begin
       aStr := SQLMetaData1.GetPrimaryKeyFields(aLevel1);
       if aStr > #32 then lb_MDLevel2.Items.Add(aStr);
     end;
  3: SQLMetaData1.GetInsertStatement(aLevel1, lb_MDLevel2.Items);
  4: SQLMetaData1.GetUpdateStatement(aLevel1, lb_MDLevel2.Items);
  5: SQLMetaData1.GetSelectStatement(aLevel1, lb_MDLevel2.Items);
  6: SQLMetaData1.GetIndexNames(aLevel1, lb_MDLevel2.Items);
  end;
  lb_MDLevel2.ItemIndex := 0;
end;

procedure TF_DbExpPlus.b_MDLevel3Click(Sender: TObject);
var
  aFieldMetaData: TFieldMetaData;
  aLevel2: string;
begin
  if cb_MDLevel3.ItemIndex = -1 then Exit;
  if not(lb_MDLevel1.Items.Count > 0) or
     not(lb_MDLevel2.Items.Count > 0) then begin
    Application.MessageBox(pchar('Database and Table Objects must be selected.'),
       'SQLMetaData', mb_Ok+mb_IconInformation);
    Exit;
  end;
  aLevel2 := lb_MDLevel2.Items[lb_MDLevel2.ItemIndex];
  lb_MDLevel2.Items.Clear;
  lb_MDLevel2.Items.Add(aLevel2);
  lb_MDLevel2.ItemIndex := 0;
  lb_MDLevel3.Items.Clear;
  case cb_MDLevel3.ItemIndex of
  0: begin
       aFieldMetaData :=
          SQLMetaData1.GetFieldMetaData(lb_MDLevel1.Items[lb_MDLevel1.ItemIndex], aLevel2);
       lb_MDLevel3.Items.Add('ColumnName = ' + aFieldMetaData.ColumnName);
       lb_MDLevel3.Items.Add('ColumnPosition = ' + IntToStr(aFieldMetaData.ColumnPosition));
       lb_MDLevel3.Items.Add('ColumnDataType = ' + IntToStr(aFieldMetaData.ColumnDataType));
       lb_MDLevel3.Items.Add('ColumnTypeName = ' + aFieldMetaData.ColumnTypeName);
       lb_MDLevel3.Items.Add('ColumnSubtype = ' + IntToStr(aFieldMetaData.ColumnSubtype));
       lb_MDLevel3.Items.Add('ColumnLength = ' + IntToStr(aFieldMetaData.ColumnLength));
       lb_MDLevel3.Items.Add('ColumnPrecision = ' + IntToStr(aFieldMetaData.ColumnPrecision));
       lb_MDLevel3.Items.Add('ColumnScale = ' + IntToStr(aFieldMetaData.ColumnScale));
       lb_MDLevel3.Items.Add('ColumnNullable = ' + IntToStr(aFieldMetaData.ColumnNullable));
     end;
  1: begin
       SQLMetaData1.GetIndexFieldNames(lb_MDLevel1.Items[lb_MDLevel1.ItemIndex],
          aLevel2, lb_MDLevel3.Items);
     end;
  end;
  lb_MDLevel3.ItemIndex := 0;
end;

procedure TF_DbExpPlus.b_SQLExecClick(Sender: TObject);
begin
  if not(m_SQLScript.Lines.Count > 0) then Exit;
  SQLScript1.SQL.Assign(m_SQLScript.Lines);
  SQLScript1.ExecuteDirect;
  if i_ShowGrid = True then begin
    SQLClientDataSet1.Close;
    SQLClientDataSet1.Open;
  end;
end;

procedure TF_DbExpPlus.b_SQLScript1Click(Sender: TObject);
begin
  m_SQLScript.Lines.Clear;
  m_SQLScript.Lines.Add('CREATE TABLE MYTABLE');
  m_SQLScript.Lines.Add('(KEY_FIELD VARCHAR(10) NOT NULL PRIMARY KEY,');
  m_SQLScript.Lines.Add('KEY_DESC VARCHAR(45));');
  m_SQLScript.Lines.Add('');
  m_SQLScript.Lines.Add('INSERT INTO');
  m_SQLScript.Lines.Add('MYTABLE');
  m_SQLScript.Lines.Add('(KEY_FIELD,');
  m_SQLScript.Lines.Add('KEY_DESC)');
  m_SQLScript.Lines.Add('VALUES      ');
  m_SQLScript.Lines.Add('(''TO UPDATE'',');
  m_SQLScript.Lines.Add('''this record will be UPDATED by SCRIPT 2'');');
  m_SQLScript.Lines.Add('');
  m_SQLScript.Lines.Add('INSERT INTO');
  m_SQLScript.Lines.Add('MYTABLE');
  m_SQLScript.Lines.Add('(KEY_FIELD,');
  m_SQLScript.Lines.Add('KEY_DESC)');
  m_SQLScript.Lines.Add('VALUES      ');
  m_SQLScript.Lines.Add('(''TO DELETE'',');
  m_SQLScript.Lines.Add('''this record will be DELETED by SCRIPT 2'');');
  i_ShowGrid := True;
  DBGrid1.Visible := True;
end;

procedure TF_DbExpPlus.b_SQLScript2Click(Sender: TObject);
begin
  m_SQLScript.Lines.Clear;
  m_SQLScript.Lines.Add('INSERT INTO');
  m_SQLScript.Lines.Add('MYTABLE');
  m_SQLScript.Lines.Add('(KEY_FIELD,');
  m_SQLScript.Lines.Add('KEY_DESC)');
  m_SQLScript.Lines.Add('VALUES');
  m_SQLScript.Lines.Add('(''INSERTED'',');
  m_SQLScript.Lines.Add('''this record was INSERTED by SCRIPT 2'');');
  m_SQLScript.Lines.Add('');
  m_SQLScript.Lines.Add('UPDATE MYTABLE');
  m_SQLScript.Lines.Add('SET');
  m_SQLScript.Lines.Add('KEY_DESC = ''this record was UPDATED by SCRIPT 2''');
  m_SQLScript.Lines.Add('WHERE KEY_FIELD = ''TO UPDATE'';');
  m_SQLScript.Lines.Add('');
  m_SQLScript.Lines.Add('DELETE FROM MYTABLE WHERE KEY_FIELD = ''TO DELETE'';');
end;

procedure TF_DbExpPlus.b_SQLScript3Click(Sender: TObject);
begin
  m_SQLScriptEnter(Self);
  m_SQLScript.Lines.Clear;
  m_SQLScript.Lines.Add('DROP TABLE MYTABLE;');
end;

procedure TF_DbExpPlus.m_SQLScriptEnter(Sender: TObject);
begin
  i_ShowGrid := False;
  DBGrid1.Visible := False;
  SQLClientDataSet1.Close;
  SQLConnection1.Close;
end;

procedure TF_DbExpPlus.b_DPCreateClick(Sender: TObject);
begin
  SQLScript1.SQL.Clear;
  SQLScript1.SQL.Add('CREATE TABLE EMPLOYEE_DEST');
  SQLScript1.SQL.Add('(');
  SQLScript1.SQL.Add('EMP_NO	EMPNO NOT NULL,');
  SQLScript1.SQL.Add('FIRST_NAME	FIRSTNAME NOT NULL,');
  SQLScript1.SQL.Add('LAST_NAME	LASTNAME NOT NULL,');
  SQLScript1.SQL.Add('PHONE_EXT	VARCHAR(4),');
  SQLScript1.SQL.Add('HIRE_DATE	TIMESTAMP DEFAULT ''NOW'' NOT NULL,');
  SQLScript1.SQL.Add('DEPT_NO	DEPTNO NOT NULL,');
  SQLScript1.SQL.Add('JOB_CODE	JOBCODE NOT NULL,');
  SQLScript1.SQL.Add('JOB_GRADE	JOBGRADE NOT NULL,');
  SQLScript1.SQL.Add('JOB_COUNTRY	COUNTRYNAME NOT NULL,');
  SQLScript1.SQL.Add('SALARY	SALARY NOT NULL,');
  SQLScript1.SQL.Add('FULL_NAME	 COMPUTED BY (last_name || '', '' || first_name),');
  SQLScript1.SQL.Add('LITERAL_EXAMPLE VARCHAR(10),');
  SQLScript1.SQL.Add('EVENT_EXAMPLE VARCHAR(45),');
  SQLScript1.SQL.Add('PRIMARY KEY (EMP_NO)');
  SQLScript1.SQL.Add(');');
  SQLScript1.ExecuteDirect;
end;

procedure TF_DbExpPlus.b_DPDropClick(Sender: TObject);
begin
  SQLClientDataSet1.Close;
  SQLClientDataSet2.Close;
  SQLConnection1.Close;
  md_Destination.Close;
  SQLScript1.SQL.Clear;
  SQLScript1.SQL.Add('DROP TABLE EMPLOYEE_DEST;');
  SQLScript1.ExecuteDirect;
end;

procedure TF_DbExpPlus.b_DPDataPumpClick(Sender: TObject);
begin
  SQLDataPump1.Execute;
  SQLClientDataSet2.Close;
  SQLConnection1.Close;
  SQLClientDataSet2.Open;
end;

procedure TF_DbExpPlus.SQLDataPump1DestinationFields11BeforeFieldPump(
  Index: Integer; Sender: TObject);
begin
  TDestinationFieldItem(Sender).Value :=
     SQLDataPump1.GetSourceQuery.FieldByName('JOB_CODE').AsString +
     ' - ' +
     FloatToStr(SQLDataPump1.GetSourceQuery.FieldByName('SALARY').AsFloat);   
end;

end.
