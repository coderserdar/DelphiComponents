//  FastReport 2.3 demo.
//
//  Demonstrates how to create reports with no designer.

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, DBTables, FR_DSet, FR_DBSet, FR_Class, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    frReport1: TfrReport;
    frDBDataSet1: TfrDBDataSet;
    Table1: TTable;
    DataSource1: TDataSource;
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
var
  v: TfrView;
  b: TfrBandView;
  Page: TfrPage;
begin
  frReport1.Pages.Clear;
  frReport1.Pages.Add;              // create page
  Page := frReport1.Pages[0];

  b := TfrBandView(frCreateObject(gtBand, ''));  // create MasterData band
  b.SetBounds(0, 20, 0, 20);
  b.BandType := btMasterData;
  b.Dataset := 'frDBDataSet1';
  Page.Objects.Add(b);

  v := frCreateObject(gtMemo, '');  // create data field
  v.SetBounds(20, 20, 200, 16);
  v.Memo.Add('[Table1."Company"]');
  Page.Objects.Add(v);

  frReport1.ShowReport;
end;

end.
