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
    Table1CustNo: TFloatField;
    Table1Company: TStringField;
    Table1Addr1: TStringField;
    Table1Addr2: TStringField;
    Table1City: TStringField;
    Table1State: TStringField;
    Table1Zip: TStringField;
    Table1Country: TStringField;
    Table1Phone: TStringField;
    Table1FAX: TStringField;
    Table1TaxRate: TFloatField;
    Table1Contact: TStringField;
    Table1LastInvoiceDate: TDateTimeField;
    procedure Button1Click(Sender: TObject);
    procedure frReport1GetValue(const ParName: String; var ParValue: Variant);
    procedure frReport1EnterRect(Memo: TStringList; View: TfrView);
    procedure frReport1PrintColumn(ColNo: Integer; var Width: Integer);
  private
    { Private declarations }
    FWidth: Integer;
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

procedure TForm1.frReport1EnterRect(Memo: TStringList; View: TfrView);
begin
  View.dx := FWidth;
end;

procedure TForm1.frReport1PrintColumn(ColNo: Integer; var Width: Integer);
var
  Field: TField;
begin
  Field := Table1.Fields[ColNo - 1];
  if Field is TStringField then
    Width := Field.Size * Canvas.TextWidth('W')
  else if Field is TDateTimeField then
    Width := 15 * Canvas.TextWidth('W')
  else
    Width := 64;
  FWidth := Width;
end;

end.
