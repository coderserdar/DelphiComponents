unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, ExtCtrls, DBCtrls, DB, DBTables, StdCtrls, Menus;

type
  TFmMain = class(TForm)
    DBNavigator1: TDBNavigator;
    GridCustomers: TDBGrid;
    GridOrders: TDBGrid;
    GridItems: TDBGrid;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure GridOrdersEnter(Sender: TObject);
    procedure GridCustomersEnter(Sender: TObject);
    procedure GridItemsEnter(Sender: TObject);
    procedure GridCustomersExit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FmMain: TFmMain;

implementation

uses DM1;

  {$R *.DFM}

procedure TFmMain.GridOrdersEnter(Sender: TObject);
begin
  DBNavigator1.DataSource := Dm.OrdersSource;
end;

procedure TFmMain.GridCustomersEnter(Sender: TObject);
begin
  DBNavigator1.DataSource := Dm.CustomerSource;
end;

procedure TFmMain.GridItemsEnter(Sender: TObject);
begin
  DBNavigator1.DataSource := Dm.ItemsSource;
end;

procedure TFmMain.GridCustomersExit(Sender: TObject);
begin
  if Dm.OraCustomer.State in [dsEdit,dsInsert] then
    Dm.OraCustomer.Post; {required if user clicks onto details
                       after changing key so that cascaded
                       update displays properly}
end;

end.
