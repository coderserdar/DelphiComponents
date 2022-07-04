unit DateDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Grids, Calendar, StdCtrls, Buttons, Spin;

type
  TDateDialog = class(TForm)
    Calendar1: TCalendar;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    seYear: TSpinEdit;
    cbMonth: TComboBox;
    bbOk: TBitBtn;
    bbCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure seYearChange(Sender: TObject);
    procedure cbMonthChange(Sender: TObject);
    procedure Calendar1DblClick(Sender: TObject);
  private
    { Private declarations }
    function GetDate : TDateTime;
    procedure SetDate(AValue : TDateTime);
  public
    { Public declarations }
    property Date : TDateTime read GetDate write SetDate;
  end;

var
  DateDialog: TDateDialog;

implementation

{$R *.DFM}

procedure TDateDialog.FormCreate(Sender: TObject);
var
  i : integer;
begin
  cbMonth.Clear;
  for i := 1 to 12 do
    cbMonth.Items.Add(LongMonthNames[i]);
end;

function TDateDialog.GetDate : TDateTime;
begin
  Result := EncodeDate(Calendar1.Year, Calendar1.Month, Calendar1.Day);
end;

procedure TDateDialog.SetDate(AValue : TDateTime);
var
  AYear, AMonth, ADay : word;
begin
  DecodeDate(AValue, AYear, AMonth, ADay);
  seYear.Value := AYear;
  cbMonth.ItemIndex := AMonth - 1;
  Calendar1.Year := AYear;
  Calendar1.Month := AMonth;
  Calendar1.Day := ADay;
end;

procedure TDateDialog.seYearChange(Sender: TObject);
begin
  Calendar1.Year := seYear.Value;
end;

procedure TDateDialog.cbMonthChange(Sender: TObject);
begin
  Calendar1.Month := cbMonth.ItemIndex + 1;
end;

procedure TDateDialog.Calendar1DblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.
