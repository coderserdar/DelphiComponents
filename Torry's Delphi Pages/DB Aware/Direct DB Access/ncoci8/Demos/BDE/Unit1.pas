// This example demonstrates BDE compatibility components TOCIBDEDatabase.
// 1) To run this demo update Database1 connection information to
//    appropriate to your environment.
// 2) You should not work directly with Database1.Connected property.
//    Instead use OCIBDEDatabase1.Connected.

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, Db, NCOci, NCOciWrapper, NCOciDB, DBTables, NCOciBDE,
  StdCtrls;

type
  TForm1 = class(TForm)
    OCIBDEDatabase1: TOCIBDEDatabase;
    Database1: TDatabase;
    OCIQuery1: TOCIQuery;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Button1: TButton;
    Button2: TButton;
    Query1: TQuery;
    DataSource2: TDataSource;
    DBGrid2: TDBGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  OCIBDEDatabase1.Connected := True;
  OCIQuery1.Active := True;
  Query1.Active := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  OCIBDEDatabase1.Connected := False;
end;

end.
