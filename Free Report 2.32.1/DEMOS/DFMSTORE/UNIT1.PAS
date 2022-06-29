//  FastReport 2.3 demo.
//
//  Demonstrates how to create reports with form stored in DFM.
//  Note that "StoreInDFM" option of the TfrReport object
//  must be set to "True".


unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, DBTables, FR_DSet, FR_DBSet, FR_Class, StdCtrls;

type
  TForm1 = class(TForm)
    frReport1: TfrReport;
    frDBDataSet1: TfrDBDataSet;
    Table1: TTable;
    DataSource1: TDataSource;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
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
  frReport1.ShowReport;
end;

end.
