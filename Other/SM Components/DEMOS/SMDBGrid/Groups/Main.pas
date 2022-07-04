unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, SMDBGrid, Db, DBTables, ExtCtrls, StdCtrls;

type
  TfrmMain = class(TForm)
    Query1: TQuery;
    DataSource1: TDataSource;
    SMDBGrid1: TSMDBGrid;
    Image1: TImage;
    Query1CUSTNO: TFloatField;
    Query1COUNTRY: TStringField;
    Query1Flag: TFloatField;
    lblDescription: TLabel;
    lblURL: TLabel;
    procedure SMDBGrid1Expression(Sender: TObject; Expression: String;
      var Text: String; var Value: Boolean);
    procedure SMDBGrid1DrawGroupingCell(Sender: TObject; ACanvas: TCanvas;
      CellRect: TRect; Group: TSMGrouping; Text: String;
      var DefaultDrawing: Boolean);
    procedure lblURLClick(Sender: TObject);
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

procedure TfrmMain.SMDBGrid1Expression(Sender: TObject; Expression: String;
  var Text: String; var Value: Boolean);
begin
  if (Query1.FieldByName(Expression).AsInteger = 1) then
  begin
    Value := True;
    Text := 'Customers from ' + Query1.FieldByName('Country').AsString
  end;
end;

procedure TfrmMain.SMDBGrid1DrawGroupingCell(Sender: TObject;
  ACanvas: TCanvas; CellRect: TRect; Group: TSMGrouping; Text: String;
  var DefaultDrawing: Boolean);
begin
  DefaultDrawing := False;

  ACanvas.Brush.Color := Group.Color;
  ACanvas.Font.Assign(Group.Font);
  ACanvas.FillRect(CellRect);

  ACanvas.Draw(CellRect.Left + 2, CellRect.Top + 2, Image1.Picture.Graphic);
  CellRect.Left := CellRect.Left + Image1.Width + 5;
  CellRect.Top := CellRect.Top + 2;
  DrawText(ACanvas.Handle, PChar(Text), Length(Text), CellRect, DT_LEFT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX or DT_VCENTER)
end;

procedure TfrmMain.lblURLClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar((Sender as TLabel).Caption), nil, nil, SW_SHOWNORMAL);
end;

end.
