unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  colorado, Db, StdCtrls, DBCtrls, ExtCtrls, Grids, DBGrids;

type
  TMainForm = class(TForm)
    Connection1: TConnection;
    CTable1: TCTable;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    BeginTran: TButton;
    CommitTran: TButton;
    RollbackTran: TButton;
    procedure BeginTranClick(Sender: TObject);
    procedure CommitTranClick(Sender: TObject);
    procedure RollbackTranClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

procedure TMainForm.BeginTranClick(Sender: TObject);
begin
  Connection1.BeginTrans;
end;

procedure TMainForm.CommitTranClick(Sender: TObject);
begin
  Connection1.CommitTrans;
end;

procedure TMainForm.RollbackTranClick(Sender: TObject);
begin
  Connection1.RollbackTrans;
  CTable1.Refresh;
end;

end.
