unit unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Grids, DBGrids, Db, AstaDrv2, AstaClientDataset,
  ScktComp, AstaCustomSocket, AstaClientSocket, ExtCtrls;

type
  TForm1 = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DataSource2: TDataSource;
    DBGrid2: TDBGrid;
    BitBtn1: TBitBtn;
    Timer1: TTimer;
    BitBtn2: TBitBtn;
    AstaClientSocket2: TAstaClientSocket;
    AstaClientDataSet1: TAstaClientDataSet;
    AstaClientDataSet2: TAstaClientDataSet;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
AstaClientSocket2.SendDataSetTransactions('test',
[AstaClientdataset1,AstaClientDataset2]);

end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
timer1.enabled:=Not timer1.enabled;

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
astaclientdataset1.refiresql;
end;

end.
