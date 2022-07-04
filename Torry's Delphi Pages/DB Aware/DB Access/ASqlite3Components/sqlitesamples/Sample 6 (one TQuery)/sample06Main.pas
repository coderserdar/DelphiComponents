// This is a simple sample of using a tquery with tupdatesql
// aducom software
// (c) 2007
unit Sample06Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ExtCtrls, DBCtrls, Grids, DBGrids, ASGSQLite3, StdCtrls;

type
  TForm1 = class(TForm)
    ASQLite3DB1: TASQLite3DB;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DataSource1: TDataSource;
    ASQLite3Query1: TASQLite3Query;
    ASQLite3UpdateSQL1: TASQLite3UpdateSQL;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    ASQLite3Query2: TASQLite3Query;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure DBNavigator1Click(Sender: TObject; Button: TNavigateBtn);
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
 with ASQLite3Query2 do begin
   Close;
   SQL.Clear;
   SQL.Add('insert into animal (id, desc) values (:v1, :v2)');
   Params[0].AsString := '99';
   Params[1].AsString := 'ninetynine';
   ExecSQL;
 end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 ASQLite3Query1.Delete;
end;

procedure TForm1.DBNavigator1Click(Sender: TObject; Button: TNavigateBtn);
begin
 Caption := FormatDateTime('hh:nn:ss',now);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 ASQLite3Query1.Open;
end;

end.
