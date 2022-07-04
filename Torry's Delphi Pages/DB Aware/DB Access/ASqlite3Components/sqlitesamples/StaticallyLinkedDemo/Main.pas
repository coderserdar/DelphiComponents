unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ASGSQLite3, Grids, DBGrids, DB;

type
  TFMain = class(TForm)
    ListBox1: TListBox;
    ListBox2: TListBox;
    Label1: TLabel;
    ASQLite3DB1: TASQLite3DB;
    ASQLite3Table1: TASQLite3Table;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    CheckBox1: TCheckBox;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FMain: TFMain;

implementation

{$R *.dfm}

procedure TFMain.FormCreate(Sender: TObject);
begin
 ASQLite3DB1.ShowDatabases(ListBox1.Items);
end;

procedure TFMain.ListBox1Click(Sender: TObject);
begin
 ASQLite3DB1.Database := ListBox1.Items[ListBox1.ItemIndex];
 ASQLite3DB1.GetTableNames(ListBox2.Items, true);
end;

procedure TFMain.ListBox2Click(Sender: TObject);
begin

  ASQLite3Table1.Close;

  ASQLite3Table1.TableName:=
      ListBox2.Items[ListBox2.ItemIndex];

  ASQLite3Table1.Open

end;

procedure TFMain.CheckBox1Click(Sender: TObject);
var
  s: string;
begin
  ASQLite3DB1.StaticallyLinked:=CheckBox1.Checked;

  if ASQLite3DB1.StaticallyLinked then
    s:= 'Statically Linked'
  else
    s:= 'DLL';

  Label2.Caption:= 'SQLite version: ' +ASQLite3DB1.Version+' ' +s
                   
end;

end.
