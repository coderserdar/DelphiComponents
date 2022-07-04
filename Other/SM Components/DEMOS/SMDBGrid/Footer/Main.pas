unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, Grids, DBGrids, SMDBGrid, DBTables, StdCtrls, ExtCtrls;

type
  TfrmMain = class(TForm)
    DataSource1: TDataSource;
    Table1: TTable;
    SMDBGrid1: TSMDBGrid;
    Table1OrderNo: TFloatField;
    Table1CustNo: TFloatField;
    Table1SaleDate: TDateTimeField;
    Table1PaymentMethod: TStringField;
    Table1ItemsTotal: TCurrencyField;
    Table1TaxRate: TFloatField;
    Table1AmountPaid: TCurrencyField;
    Table2: TTable;
    Table2CustNo: TFloatField;
    Table2Company: TStringField;
    Table2Addr1: TStringField;
    Table2Addr2: TStringField;
    Table2City: TStringField;
    Table2State: TStringField;
    Table2Zip: TStringField;
    Table2Country: TStringField;
    Table2Phone: TStringField;
    Table2FAX: TStringField;
    Table2TaxRate: TFloatField;
    Table2Contact: TStringField;
    Table2LastInvoiceDate: TDateTimeField;
    lblDescription: TLabel;
    lblURL: TLabel;
    Image1: TImage;
    procedure Table1AfterScroll(DataSet: TDataSet);
    procedure lblURLClick(Sender: TObject);
    procedure SMDBGrid1DrawFooterCell(Sender: TObject; Canvas: TCanvas;
      FooterCellRect: TRect; Field: TField; var FooterText: String;
      var DefaultDrawing: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}
uses ShellAPI;

procedure TfrmMain.Table1AfterScroll(DataSet: TDataSet);
var
  i: Integer;
begin
  for i := 0 to SMDBGrid1.Columns.Count-1 do
    with SMDBGrid1.Columns[i] as TSMDBColumn do
      if FieldName = 'AmountPaid' then
        FooterValue := 'Payment: ' + Table1.FieldByName('PaymentMethod').AsString
      else
      if FieldName = 'CustNo' then
      begin
        if Table2.Active and Table2.Locate('CustNo', Field.AsInteger, []) then
          FooterValue := 'Company: ' + Table2.FieldByName('Company').AsString
        else
          FooterValue := ''
      end;
end;

procedure TfrmMain.lblURLClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar((Sender as TLabel).Caption), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmMain.SMDBGrid1DrawFooterCell(Sender: TObject;
  Canvas: TCanvas; FooterCellRect: TRect; Field: TField;
  var FooterText: String; var DefaultDrawing: Boolean);
begin
  if Assigned(Field) and (Field.FieldName = 'AmountPaid') then
  begin
    DefaultDrawing := False;
    Canvas.FillRect(FooterCellRect);

    Canvas.Draw(FooterCellRect.Left + 2, FooterCellRect.Top + 2, Image1.Picture.Graphic);
    FooterCellRect.Left := FooterCellRect.Left + Image1.Width + 5;
    FooterCellRect.Top := FooterCellRect.Top + 2;
    DrawText(Canvas.Handle, PChar(FooterText), Length(FooterText), FooterCellRect, DT_LEFT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX or DT_VCENTER)
  end
end;

end.
