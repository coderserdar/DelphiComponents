// This is a simple master-detail sample and an enhancement of sample1.
// aducom software
// (c) 2007
unit Sample02Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ExtCtrls, DBCtrls, Grids, DBGrids, ASGSQLite3, StdCtrls;

type
  TForm1 = class(TForm)
    ASQLite3DB1: TASQLite3DB;
    ASQLite3Table1: TASQLite3Table;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DataSource1: TDataSource;
    RGDate: TRadioGroup;
    DBGrid2: TDBGrid;
    DBNavigator2: TDBNavigator;
    ASQLite3Table2: TASQLite3Table;
    DataSource2: TDataSource;
    ASQLite3Table2id: TIntegerField;
    ASQLite3Table2name: TStringField;
    ASQLite3Table2gender: TStringField;
    ASQLite3Table2dob: TDateField;
    ASQLite3Table2parent: TIntegerField;
    Button1: TButton;
    Button2: TButton;
    ASQLite3Query1: TASQLite3Query;
    procedure FormCreate(Sender: TObject);
    procedure RGDateClick(Sender: TObject);
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

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
 ASQLite3table2.append;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 ASQLite3table2.post;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 ASQLite3Table1.Open;
 ASQLite3Table2.Open;
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
