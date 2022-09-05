unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, CnMonthCalendar;

type
  TForm1 = class(TForm)
    pgc1: TPageControl;
    ts1: TTabSheet;
    lblYear: TLabel;
    lblYue: TLabel;
    lblDay: TLabel;
    lblHour: TLabel;
    mmoResult: TMemo;
    edtYear: TEdit;
    edtMonth: TEdit;
    edtDay: TEdit;
    btnCalc: TButton;
    edtHour: TEdit;
    Button1: TButton;
    tsCalendar: TTabSheet;
    CnMonthCalendar1: TCnMonthCalendar;
    chkGanZhi: TCheckBox;
    tsConvert: TTabSheet;
    lbl1: TLabel;
    edtLunarYear: TEdit;
    lbl2: TLabel;
    edtLunarMonth: TEdit;
    lbl3: TLabel;
    edtLunarDay: TEdit;
    chkLeap: TCheckBox;
    btnCalcLunar: TButton;
    mmoLunar: TMemo;
    chkMonthButton: TCheckBox;
    chkYearButton: TCheckBox;
    dtpSet: TDateTimePicker;
    lblDate: TLabel;
    procedure btnCalcClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure chkGanZhiClick(Sender: TObject);
    procedure btnCalcLunarClick(Sender: TObject);
    procedure chkMonthButtonClick(Sender: TObject);
    procedure chkYearButtonClick(Sender: TObject);
    procedure dtpSetChange(Sender: TObject);
    procedure CnMonthCalendar1Change(Sender: TObject);
  private
    { Private declarations }
    procedure ConvertEditToDate;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses CnCalendar, CnCalClass;

{$R *.DFM}

var
  AYear, AMonth, ADay, AHour: Integer;

procedure TForm1.btnCalcClick(Sender: TObject);
var
  I, GanZhi, Gan, Zhi, JiuXing: Integer;
  M1, D1, H1, mi1: Integer;
  M2, D2, H2, mi2: Integer;
  v91, v92: Integer;
  vf1, vf2: Integer;
  rmMonth, rmDay, cmMonth, cmDay: Integer;
  LunarYear, LunarMonth, LunarDay: Integer;
  IsLeap: Boolean;
  FSeq, FDay: Integer;
begin
  mmoResult.Clear;
  ConvertEditToDate;
  ValidDate(AYear, AMonth, ADay);
  
  mmoResult.Lines.Add('����������' + IntToStr(GetAllDays(AYear, AMonth, ADay)));
  mmoResult.Lines.Add('��Ч��׼������' + IntToStr(GetEquStandardDays(AYear, AMonth, ADay)));
  mmoResult.Lines.Add('���ڣ�' + IntToStr(GetWeek(AYear, AMonth, ADay)));
  if GetShu9Day(AYear, AMonth, ADay, v91, v92) then
    mmoResult.Lines.Add(Format('%d�ŵ�%d��', [v91, v92]));
  if Get3FuDay(AYear, AMonth, ADay, vf1, vf2) then
    mmoResult.Lines.Add(Format('%d����%d��', [vf1, vf2]));
  mmoResult.Lines.Add('������' + GetXingZuoFromNumber(GetXingZuoFromMonthDay(AMonth, ADay)));

  mmoResult.Lines.Add('����Ԫ��' + Get3YuanFromNumber(Get3YuanFromYear(AYear, AMonth, ADay)));
  mmoResult.Lines.Add('�˾��ǣ�' + Get9XingFromNumber(GetYun9XingFromYear(AYear, AMonth, ADay)));
  GanZhi := GetGanZhiFromYear(AYear, AMonth, ADay, AHour);
  ExtractGanZhi(GanZhi, Gan, Zhi);
  JiuXing := Get9XingFromYear(AYear, AMonth, ADay);
  mmoResult.Lines.Add('��Ф��' + GetShengXiaoFromNumber(Zhi));
  mmoResult.Lines.Add('�꣺'+ GetGanZhiFromNumber(GanZhi) + '�����У�'
    + Get5XingFromNumber(Get5XingFromGan(Gan)) + Get5XingFromNumber(Get5XingFromZhi(Zhi))
    + '�����ǣ�' + Get9XingFromNumber(JiuXing));

  GanZhi := GetGanZhiFromMonth(AYear, AMonth, ADay, AHour);
  ExtractGanZhi(GanZhi, Gan, Zhi);
  JiuXing := Get9XingFromMonth(AYear, AMonth, ADay);
  mmoResult.Lines.Add('�£�'+ GetGanZhiFromNumber(GanZhi) + '�����У�'
    + Get5XingFromNumber(Get5XingFromGan(Gan)) + Get5XingFromNumber(Get5XingFromZhi(Zhi))
    + '�����ǣ�' + Get9XingFromNumber(JiuXing));

  GanZhi := GetGanZhiFromDay(AYear, AMonth, ADay, AHour);
  ExtractGanZhi(GanZhi, Gan, Zhi);
  JiuXing := Get9XingFromDay(AYear, AMonth, ADay);
  mmoResult.Lines.Add('�գ�'+ GetGanZhiFromNumber(GanZhi) + '�����У�'
    + Get5XingFromNumber(Get5XingFromGan(Gan)) + Get5XingFromNumber(Get5XingFromZhi(Zhi))
    + '�����ǣ�' + Get9XingFromNumber(JiuXing));

  GanZhi := GetGanZhiFromHour(AYear, AMonth, ADay, AHour);
  ExtractGanZhi(GanZhi, Gan, Zhi);
  JiuXing := Get9XingFromHour(AYear, AMonth, ADay, AHour);
  mmoResult.Lines.Add('ʱ��'+ GetGanZhiFromNumber(GanZhi) + '�����У�'
    + Get5XingFromNumber(Get5XingFromGan(Gan)) + Get5XingFromNumber(Get5XingFromZhi(Zhi))
    + '�����ǣ�' + Get9XingFromNumber(JiuXing));
  mmoResult.Lines.Add('ʱ����' + GetDiZhiFromNumber(GetShiChenFromHour(AHour)));
  mmoResult.Lines.Add('��ʮ���ޣ�'+ Get28XiuFromNumber(Get28XiuFromDay(AYear, AMonth, ADay)) + '/' + Get28XiuLongFromNumber(Get28XiuFromDay(AYear, AMonth, ADay)));
  mmoResult.Lines.Add('�����������У�'+ Get5XingFromNumber(Get5XingFromDay(AYear, AMonth, ADay)) + '/' + Get5XingLongFromDay(AYear, AMonth, ADay));
  mmoResult.Lines.Add('ʮ������'+ Get12JianFromNumber(Get12JianFromDay(AYear, AMonth, ADay)));
  mmoResult.Lines.Add('���ս�����' + GetJieQiFromNumber(GetJieQiFromDay(AYear, AMonth, ADay)));
  if GetJieQiFromDay(AYear, AMonth, ADay) >= 0 then
  begin
    if GetJieQiTimeFromDay(AYear, AMonth, ADay, H1, mi1) >= 0 then
      mmoResult.Lines.Add(Format('���ս���ʱ�̣�%d ʱ %d ��', [H1, mi1])); 
  end;
  mmoResult.Lines.Add('ÿ��̥��' + GetTaiShenStringFromDay(AYear, AMonth, ADay));
  if Get3FuDay(AYear, AMonth, ADay, FSeq, FDay) then
    mmoResult.Lines.Add(Format('������ %s �� %d ��', [Get3FuFromNumber(FSeq), FDay]));

  GetRuMeiDay(AYear, rmMonth, rmDay);
  GetChuMeiDay(AYear, cmMonth, cmDay);
  mmoResult.Lines.Add(Format('��÷ %d �� %d �գ���÷ %d �� %d ��', [rmMonth, rmDay, cmMonth, cmDay]));

  if GetLunarFromDay(AYear, AMonth, ADay, LunarYear, LunarMonth, LunarDay, IsLeap) then
  begin
    if IsLeap then
      mmoResult.Lines.Add(Format('ũ�� %d ���� %d �� %s', [LunarYear, LunarMonth, GetLunarDayFromNumber(LunarDay)]))
    else
      mmoResult.Lines.Add(Format('ũ�� %d �� %d �� %s', [LunarYear, LunarMonth, GetLunarDayFromNumber(LunarDay)]));
  end;
  for I := 0 to 11 do
  begin
    GetJieQiInAYear(AYear, 2 * I, M1, D1, H1, mi1);
    GetJieQiInAYear(AYear, 2 * I + 1, M2, D2, H2, mi2);
    mmoResult.Lines.Add(Format('%s��%2d��%2d��:%2dʱ:%2d��    %s��%2d��%2d��:%2dʱ:%2d��',
      [GetJieQiFromNumber((I * 2 + 22) mod 24), M1, D1, H1, mi1,
       GetJieQiFromNumber((I * 2 + 23) mod 24), M2, D2, H2, mi2]));
  end;
end;

procedure TForm1.ConvertEditToDate;
begin
  AYear := StrToIntDef(edtYear.Text, 2000);
  AMonth := StrToIntDef(edtMonth.Text, 12);
  ADay := StrToIntDef(edtDay.Text, 21);
  AHour := StrToIntDef(edtHour.Text, 0);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Year, Month, Day: Word;
  Hour, Min, Sec, Dummy: Word;
begin
  DecodeDate(Date, Year, Month, Day);
  DecodeTime(Time, Hour, Min, Sec, Dummy);
  edtYear.Text := IntToStr(Year);
  edtMonth.Text := IntToStr(Month);
  edtDay.Text := IntToStr(Day);
  edtHour.Text := IntToStr(Hour);

  chkMonthButton.Checked := CnMonthCalendar1.ShowMonthButton;
  chkYearButton.Checked := CnMonthCalendar1.ShowYearButton;
  CnMonthCalendar1.Date := Now;
  pgc1.ActivePageIndex := 0;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  HourObj: TCnHourObj;
begin
  ConvertEditToDate;
  mmoResult.Clear;

  HourObj := TCnHourObj.Create;
  try
    HourObj.Year := AYear;
    HourObj.Month := AMonth;
    HourObj.Day := ADay;
    HourObj.Hour := AHour;

    mmoResult.Lines.Add('���ڣ�' + IntToStr(HourObj.Week));
    mmoResult.Lines.Add('�꣺'+ GetGanZhiFromNumber(HourObj.YearGanZhi));
    mmoResult.Lines.Add('�£�'+ GetGanZhiFromNumber(HourObj.MonthGanZhi));
    mmoResult.Lines.Add('�գ�'+ GetGanZhiFromNumber(HourObj.DayGanZhi));
    mmoResult.Lines.Add('ʱ��'+ GetGanZhiFromNumber(HourObj.HourGanZhi));
    mmoResult.Lines.Add('��ʮ���ޣ�'+Get28XiuFromNumber(HourObj.Day28Xiu));

    if HourObj.IsLeapMonth then
      mmoResult.Lines.Add(Format('ũ���� %d �� %d ��', [HourObj.LunarMonth, HourObj.LunarDay]))
    else
        mmoResult.Lines.Add(Format('ũ�� %d �� %d ��', [HourObj.LunarMonth, HourObj.LunarDay]));

    if HourObj.IsInJiu then
      mmoResult.Lines.Add(Format('%d�ŵ�%d��', [HourObj.Jiu, HourObj.JiuDay]));
    if HourObj.IsInFu then
      mmoResult.Lines.Add(Format('%d����%d��', [HourObj.Fu, HourObj.FuDay]));
    mmoResult.Lines.Add('���ս�����' + GetJieQiFromNumber(HourObj.JieQi));
    mmoResult.Lines.Add(Format('��÷ %d �� %d �գ���÷ %d �� %d ��',
      [HourObj.RuMeiMonth, HourObj.RuMeiDay, HourObj.ChuMeiMonth, HourObj.ChuMeiDay]));

    mmoResult.Lines.Add('������' + GetXingZuoFromNumber(HourObj.XingZuo));
  finally
    HourObj.Free;
  end;
end;

procedure TForm1.chkGanZhiClick(Sender: TObject);
begin
  CnMonthCalendar1.ShowGanZhi := chkGanZhi.Checked;
end;

procedure TForm1.btnCalcLunarClick(Sender: TObject);
var
  Year, Month, Day: Integer;
  IsLeap: Boolean;
begin
//  for I := 620606 to 620608 do
//  begin
//    if GetDayFromEquStandardDays(I, Year, Month, Day) then
//    mmoLunar.Lines.Add(Format('%4.4d-%2.2d-%2.2d', [Year, Month, Day]));
//  end;

  Year := StrToIntDef(edtLunarYear.Text, 2000);
  Month := StrToIntDef(edtLunarMonth.Text, 12);
  Day := StrToIntDef(edtLunarDay.Text, 21);

  ValidLunarDate(Year, Month, Day, chkLeap.Checked);

  if not GetDayFromLunar(Year, Month, Day, chkLeap.Checked, Year, Month, Day) then
  begin
    mmoLunar.Lines.Add('No this Lunar Date.');
    Exit;
  end;

  mmoLunar.Lines.Add(Format('%4.4d-%2.2d-%2.2d', [Year, Month, Day]));
  if GetLunarFromDay(Year, Month, Day, Year, Month, Day, IsLeap) then
  begin
    if IsLeap then
      mmoLunar.Lines.Add(Format('ũ�� %d ���� %d �� %s', [Year, Month, GetLunarDayFromNumber(Day)]))
    else
      mmoLunar.Lines.Add(Format('ũ�� %d �� %d �� %s', [Year, Month, GetLunarDayFromNumber(Day)]));
  end;

  mmoLunar.Lines.Add(Format('��ũ����������%d', [GetLunarMonthDays(Year, Month, chkLeap.Checked)]));
end;

procedure TForm1.chkMonthButtonClick(Sender: TObject);
begin
  Self.CnMonthCalendar1.ShowMonthButton := chkMonthButton.Checked;
end;

procedure TForm1.chkYearButtonClick(Sender: TObject);
begin
  CnMonthCalendar1.ShowYearButton := chkYearButton.Checked;
end;

procedure TForm1.dtpSetChange(Sender: TObject);
begin
  CnMonthCalendar1.Date := dtpSet.Date;
end;

procedure TForm1.CnMonthCalendar1Change(Sender: TObject);
begin
  lblDate.Caption := DateTimeToStr(CnMonthCalendar1.Date);
end;

end.
