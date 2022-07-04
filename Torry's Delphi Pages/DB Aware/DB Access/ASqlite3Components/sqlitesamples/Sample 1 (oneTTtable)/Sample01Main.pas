// This is a simple sample of a lookup field.
// aducom software
// (c) 2007
unit Sample01Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ExtCtrls, DBCtrls, Grids, DBGrids, ASGSQLite3, StdCtrls, Mask,
  TntDBCtrls;

type
  TForm1 = class(TForm)
    ASQLite3DB1: TASQLite3DB;
    ASQLite3Table1: TASQLite3Table;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DataSource1: TDataSource;
    RGDate: TRadioGroup;
    TntDBEdit1: TTntDBEdit;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure RGDateClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
 ASQLite3Table1.Post;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 ASQLite3Table1.Open;
end;

procedure TForm1.RGDateClick(Sender: TObject);
begin
 case RGDate.ItemIndex of
  0 : ShortDateFormat := 'dd-mm-yyyy';
  1 : ShortDateFormat := 'yyyy-mm-dd';
  2 : ShortDateFormat := 'mm-dd-yyyy';
 end;
 ASQLite3Table1.Refresh;
end;

end.
