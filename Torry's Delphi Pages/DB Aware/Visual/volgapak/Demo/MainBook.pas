unit MainBook;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Db, VolgaTbl, StdCtrls, Mask, VolDBEdit, Grids, VolDBGrid, ComCtrls,
  VolPeriod, ExtCtrls, DBCtrls, VolMeter;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    VolgaDBGrid1: TVolgaDBGrid;
    VolgaDBEdit1: TVolgaDBEdit;
    VolgaDBEdit2: TVolgaDBEdit;
    VolgaDBEdit3: TVolgaDBEdit;
    Book: TVolgaTable;
    DataBook: TDataSource;
    BookBookID: TStringField;
    BookAuthor: TStringField;
    BookBookName: TStringField;
    BookCategory: TStringField;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Cat: TVolgaTable;
    DataCat: TDataSource;
    Period: TVolgaPeriod;
    Label4: TLabel;
    ReadB: TVolgaTable;
    DataReadB: TDataSource;
    VolgaDBGrid2: TVolgaDBGrid;
    ReadBDataStart: TDateField;
    ReadBDataEnd: TDateField;
    ReadBBookID: TStringField;
    CatIndex: TIntegerField;
    CatLevel: TIntegerField;
    CatCategory: TStringField;
    CatCatName: TStringField;
    DBNavigator1: TDBNavigator;
    ReadBMyBook: TStringField;
    ReadBBall: TIntegerField;
    DBNavigator2: TDBNavigator;
    VolgaMeter1: TVolgaMeter;
    procedure BookAfterPost(DataSet: TDataSet);
    procedure PeriodChange(Sender: TObject);
    procedure VolgaDBGrid1TitleClick(Sender: TObject;
      Column: TVolgaColumn);
    procedure VolgaDBGrid1DrawTitleAttr(Sender: TObject;
      Column: TVolgaColumn; AFont: TFont; var AColor: TColor);
    procedure VolgaDBGrid2DrawCellAttr(Sender: TObject;
      Column: TVolgaColumn; AFont: TFont; var AColor: TColor;
      State: TGridDrawState);
    procedure FormShow(Sender: TObject);
    procedure BookNewRecord(DataSet: TDataSet);
    procedure ReadBNewRecord(DataSet: TDataSet);
    procedure VolgaDBGrid1DrawCellAttr(Sender: TObject;
      Column: TVolgaColumn; AFont: TFont; var AColor: TColor;
      State: TGridDrawState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.BookAfterPost(DataSet: TDataSet);
begin
  with TVolgaTable(DataSet) do
    SaveToFile(TableName);
  VolgaMeter1.Max := Book.RecordCount;
  VolgaMeter1.Position := ReadB.RecordCount;
end;

procedure TForm1.PeriodChange(Sender: TObject);
begin
  ReadB.SetRange([Period.StartDate],[Period.EndDate]);
  VolgaMeter1.Max := Book.RecordCount;
  VolgaMeter1.Position := ReadB.RecordCount;
end;

procedure TForm1.VolgaDBGrid1TitleClick(Sender: TObject;
  Column: TVolgaColumn);
begin
  with TVolgaTable(TVolgaDBGrid(Sender).DataSource.DataSet) do
    IndexFieldNames := Column.FieldName;
end;

procedure TForm1.VolgaDBGrid1DrawTitleAttr(Sender: TObject;
  Column: TVolgaColumn; AFont: TFont; var AColor: TColor);
begin
  with TVolgaDBGrid(Sender),TVolgaTable(TVolgaDBGrid(Sender).DataSource.DataSet) do begin
    if Column.FieldName=IndexFieldNames then
      AFont.Color:=clBlue
    else AFont.Color := clBlack;
  end;
end;

procedure TForm1.VolgaDBGrid2DrawCellAttr(Sender: TObject;
  Column: TVolgaColumn; AFont: TFont; var AColor: TColor;
  State: TGridDrawState);
begin
  if Column.FieldName='BookID' then
  case ReadBBall.AsInteger of
  1: begin AFont.Color := clRed; AFont.Style :=[]; end;
  2: begin AFont.Color := clBlack; AFont.Style :=[]; end;
  3: begin AFont.Color := clBlue; AFont.Style :=[]; end;
  4: begin AFont.Color := clBlue; AFont.Style :=[fsBold]; end;
  5: begin AFont.Color := clNavy; AFont.Style :=[fsBold]; AColor := clYellow; end;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage := TabSheet1;
  PeriodChange(Period);
end;

procedure TForm1.BookNewRecord(DataSet: TDataSet);
begin
  VolgaDBEdit1.SetFocus;
end;

procedure TForm1.ReadBNewRecord(DataSet: TDataSet);
begin
  VolgaDBGrid2.SelectedField := ReadBDataStart;
  ReadBBall.Value := 4;
  ReadBMyBook.Value := '1';
end;

procedure TForm1.VolgaDBGrid1DrawCellAttr(Sender: TObject;
  Column: TVolgaColumn; AFont: TFont; var AColor: TColor;
  State: TGridDrawState);
begin
  if gdSelected in State then Exit;
  if Book.RecNo mod 2 = 0 then
    AColor := clBtnFace
  else
    AColor := clWindow;
end;

end.
