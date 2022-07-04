unit AstaDataModule;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, AstaProvider, scktcomp, NCOciDB, NCOci, NCOciWrapper;

type
  TAstaOracleDataModule = class(TDataModule)
    AstaProvider1: TAstaProvider;
    MainSession: TOCIDatabase;
    SelectQuery: TOCIQuery;
    ExecQuery: TOCIQuery;
    qrySpName: TOCIQuery;
    qryFields: TOCIQuery;
    qryMeta: TOCIQuery;
    EmpQuery: TOCIQuery;
    spSpColumn: TOCIStoredProc;
    ExecProc: TOCIStoredProc;
    qryTables: TOCIQuery;
    qryViews: TOCIQuery;
    qryIndexes: TOCIQuery;
    qryPKFields: TOCIQuery;
    procedure AstaOracleDataModuleCreate(Sender: TObject);
    procedure AstaProvider1BeforeInsert(Sender: TObject;
      ClientSocket: TCustomWinSocket; ExecQuery: TComponent;
      CurrentValueDataSet: TDataSet; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    FTransActionStarted:Boolean;
  end;

var
  DM: TAstaOracleDataModule;

implementation

uses AstaServer;

{$R *.DFM}

procedure TAstaOracleDataModule.AstaOracleDataModuleCreate(
  Sender: TObject);
begin
  FTransactionStarted := False;
  MainSession.DatabaseName := Name;
  qrySpName.DatabaseName := Name;
  spSpColumn.DatabaseName := Name;
  qryMeta.DatabaseName := Name;
  qryFields.DatabaseName := Name;
  qryTables.DatabaseName := Name;
  qryViews.DatabaseName := Name;
  qryIndexes.DatabaseName := Name;
  qryPKFields.DatabaseName := Name;
  SelectQuery.DatabaseName := Name;
  ExecQuery.DatabaseName := Name;
  ExecProc.DatabaseName := Name;
  EmpQuery.DatabaseName := Name;
end;

procedure TAstaOracleDataModule.AstaProvider1BeforeInsert(Sender: TObject;
  ClientSocket: TCustomWinSocket; ExecQuery: TComponent;
  CurrentValueDataSet: TDataSet; var Handled: Boolean);
begin
  with SelectQuery do begin
    Active:=False;
    sql.Clear;
    sql.add('select max(empno) empno, sysdate hiredate from emp');
    Active:=True;
    TAstaServerSocket(Sender).RecordServerActivity(ClientSocket,'Next value: '+FieldByName('EmpNo').ASString);
    CurrentValueDataSet.Edit;
    CurrentValueDataSet.FieldByname('EmpNo').AsInteger:=FieldByName('EmpNo').AsInteger;
    CurrentValueDataSet.FieldByname('HireDate').AsdateTime:=FieldByName('HireDate').AsDateTime;
    CurrentValueDataSet.Post;
  end;
end;

end.
