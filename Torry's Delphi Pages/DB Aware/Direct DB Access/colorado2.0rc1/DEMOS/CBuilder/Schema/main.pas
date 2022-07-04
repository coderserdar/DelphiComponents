unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  colorado, Db, StdCtrls, DBCtrls, ExtCtrls, Grids, DBGrids;

type
  TMainForm = class(TForm)
    Connection1: TConnection;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    CSchema1: TCSchema;
    ComboBox1: TComboBox;
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

procedure TMainForm.ComboBox1Change(Sender: TObject);
begin
  CSchema1.Close;
  CSchema1.SchemaType := Combobox1.Text;
  CSchema1.Open;
end;

end.
