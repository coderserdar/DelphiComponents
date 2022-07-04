unit Unit5;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FR_DSet, FR_Class;

type
  TForm5 = class(TForm)
    frReport1: TfrReport;
    CrossRowDataset: TFRUserDataset;
    CrossColumnDataset: TFRUserDataset;
    procedure CrossRowDatasetFirst(Sender: TObject);
    procedure CrossRowDatasetNext(Sender: TObject);
    procedure CrossRowDatasetCheckEOF(Sender: TObject; var EOF: Boolean);
    procedure CrossColumnDatasetFirst(Sender: TObject);
    procedure CrossColumnDatasetNext(Sender: TObject);
    procedure CrossColumnDatasetCheckEOF(Sender: TObject;
      var EOF: Boolean);
    procedure frReport1GetValue(const ParName: string;
      var ParValue: Variant);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure StartReport;
  end;

var
  Form5: TForm5;

implementation

uses datasets, Unit2;

{$R *.DFM}

var i, j: Integer;
    RowCount, ColCount: Integer;

procedure TForm5.FormCreate(Sender: TObject);
begin
  ColCount := 16; RowCount := 20;
end;

procedure TForm5.StartReport;
begin
  if Form2.ShowModal = mrOk then
  begin
    ColCount := StrToInt(Form2.Edit1.Text);
    RowCount := StrToInt(Form2.Edit2.Text);
    frReport1.ShowReport;
  end;
end;

procedure TForm5.CrossRowDatasetFirst(Sender: TObject);
begin
  i := 0;
end;

procedure TForm5.CrossRowDatasetNext(Sender: TObject);
begin
  Inc(i);
end;

procedure TForm5.CrossRowDatasetCheckEOF(Sender: TObject;
  var EOF: Boolean);
begin
  EOF := EOF or (i >= RowCount);
end;

procedure TForm5.CrossColumnDatasetFirst(Sender: TObject);
begin
  j := 0;
end;

procedure TForm5.CrossColumnDatasetNext(Sender: TObject);
begin
  Inc(j);
end;

procedure TForm5.CrossColumnDatasetCheckEOF(Sender: TObject;
  var EOF: Boolean);
begin
  EOF := EOF or (j >= ColCount);
end;

procedure TForm5.frReport1GetValue(const ParName: string;
  var ParValue: Variant);
begin
  if AnsiUpperCase(ParName) = 'CELL' then
    ParValue := IntToStr(i * ColCount + j + 1);
end;

end.
