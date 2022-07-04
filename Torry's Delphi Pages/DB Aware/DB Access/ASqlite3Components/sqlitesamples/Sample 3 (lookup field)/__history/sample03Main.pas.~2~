// This is a simple sample of a table with a date. This date has a few
// different notations, but all is stored in the default YYYY-MM-DD notation.
// aducom software
// (c) 2007
unit Sample03Main;

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
    TGender: TASQLite3Table;
    ASQLite3Table1number: TIntegerField;
    ASQLite3Table1name: TStringField;
    ASQLite3Table1dob: TDateField;
    ASQLite3Table1gdesc: TStringField;
    ASQLite3Table1gender: TStringField;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure RGDateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
 ASQLite3Table1.Open;
 TGender.Open;
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
