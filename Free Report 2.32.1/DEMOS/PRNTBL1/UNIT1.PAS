unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, DBTables, FR_DSet, FR_DBSet, FR_Class;

type
  TForm1 = class(TForm)
    frReport1: TfrReport;
    frDBDataSet1: TfrDBDataSet;
    frUserDataset1: TfrUserDataset;
    Table1: TTable;
    DataSource1: TDataSource;
    Button1: TButton;
    frUserDataset2: TfrUserDataset;
    procedure Button1Click(Sender: TObject);
    procedure frReport1GetValue(const ParName: String; var ParValue: Variant);
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
  frUserDataset1.RangeEndCount := Table1.FieldCount;
  frUserDataset2.RangeEndCount := Table1.FieldCount;
  frReport1.ShowReport;
end;

procedure TForm1.frReport1GetValue(const ParName: String; var ParValue: Variant);
begin
  if ParName = 'Cell' then
    ParValue := Table1.Fields[frUserDataset1.RecNo].Value;
  if ParName = 'Header' then
    ParValue := Table1.Fields[frUserDataset2.RecNo].FieldName;
end;

end.
