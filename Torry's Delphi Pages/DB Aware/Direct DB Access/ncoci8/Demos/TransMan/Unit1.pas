// For use parallel (global) transactions, include
// dmDistributed in TOCIDatabase.InitModes

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, NCSQLMon, NCOciDB, NCOci, NCOciWrapper, Grids, DBGrids, Buttons,
  StdCtrls;

type
  TForm1 = class(TForm)
    NCSQLMonitorClient1: TNCSQLMonitorClient;
    qrExec1: TOCIQuery;
    OCIDatabase1: TOCIDatabase;
    OCITransactionManager1: TOCITransactionManager;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    qrExec2: TOCIQuery;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    OCITransactionManager2: TOCITransactionManager;
    qrSel1: TOCIQuery;
    qrSel2: TOCIQuery;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
  private
    { Private declarations }
    FExecQ: TOCIQuery;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

// 24757 - duplicate transaction identifier
// 25352 - no current trans
// 24771 - can't forget, detach, ... local trans
procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
    if DataSource1.DataSet <> nil then
        DataSource1.DataSet.Active := SpeedButton1.Down;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
begin
    if FExecQ <> nil then begin
        FExecQ.ParamByName('EMPNO').AsString := Edit1.Text;
        FExecQ.ParamByName('SAL').AsString := Edit2.Text;
        FExecQ.ExecSQL;
    end;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
    OCIDatabase1.TransactionManager := OCITransactionManager1;
    FExecQ := qrExec1;
    DataSource1.DataSet := qrSel1;
    SpeedButton1.Down := qrSel1.Active;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
    OCIDatabase1.TransactionManager := OCITransactionManager2;
    FExecQ := qrExec2;
    DataSource1.DataSet := qrSel2;
    SpeedButton1.Down := qrSel2.Active;
end;

procedure TForm1.SpeedButton7Click(Sender: TObject);
begin
    OCIDatabase1.StartTransaction;
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
begin
    OCIDatabase1.Commit;
end;

procedure TForm1.SpeedButton6Click(Sender: TObject);
begin
    OCIDatabase1.Rollback;
end;

procedure TForm1.SpeedButton8Click(Sender: TObject);
begin
    OCIDatabase1.TransactionManager.Suspend;
end;

procedure TForm1.SpeedButton9Click(Sender: TObject);
begin
    OCIDatabase1.TransactionManager.Resume;
end;

procedure TForm1.SpeedButton10Click(Sender: TObject);
begin
    OCIDatabase1.TransactionManager.Forget;
end;

end.
