//---------------------------------------------------------------------------
//  TVolgaCalendar - inherited from TCustomPanel
//  Today panel, buttons for change months an years
//  properties Date,Day,Month,Year,Text
//  TVolgaCalendar is used in TVolgaDBGrid and TVolgaDBEdit
//---------------------------------------------------------------------------
//  Copyright © 2000-2002, Olga Vlasova, Russia
//  http://www.volgadb.com
//  E-mail: info@volgadb.com
//---------------------------------------------------------------------------
unit VolCalend;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Buttons, ExtCtrls, StdCtrls, Menus, VolDBConst;

type

  TVolgaCalendar = class(TCustomPanel)
  private
    FSelected: Boolean;
    FDate: TDateTime;
    FDay: word;
    FMonth: word;
    FYear: word;
    FSelectDate: TNotifyEvent;
    FChangeDate: TNotifyEvent;
    FMondayWeek: Boolean;
    { Private declarations }
    procedure RefreshCalendar;
    function DaysPerMonth(AYear, AMonth: Integer): Integer;
    function CreateButton(num: integer): TSpeedButton;
    procedure btnMonthClick(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure btnDay11Click(Sender: TObject);
    procedure btnPriorYearClick(Sender: TObject);
    procedure btnNextYearClick(Sender: TObject);
    procedure PanelTodayClick(Sender: TObject);
    procedure btnPriorClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    function GetText: string;
    procedure SetDate(const Value: TDateTime);
    procedure SetDay(const Value: word);
    procedure SetMonth(const Value: word);
    procedure SetText(const Value: string);
    procedure SetYear(const Value: word);
    procedure MainCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
    procedure MainKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function CreateLabel(num: integer): TLabel;
    procedure ChangeWeekStart;
    function GetWeekStart: integer;
  protected
    { Protected declarations }
    PanelMes: TPanel;
    LabelYear: TLabel;
    LabelMon: TLabel;
    Lab: array[1..7] of TLabel;
    PopupMenu1: TPopupMenu;
    BUT: array[1..42] of TSpeedButton;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property Selected: Boolean read FSelected;
  published
    { Published declarations }
    property Day: word read FDay write SetDay;
    property Month: word read FMonth write SetMonth;
    property Year: word read FYear write SetYear;
    property Date: TDateTime read FDate write SetDate;
    property Text: string read GetText write SetText;
    property BevelInner;
    property BevelOuter;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectDate: TNotifyEvent read FSelectDate write FSelectDate;
    property OnChangeDate: TNotifyEvent read FChangeDate write FChangeDate;
  end;

procedure Register;

implementation

type
  TWeekStart = (onSunday, onMonday);

procedure Register;
begin
  RegisterComponents('Volga', [TVolgaCalendar]);
end;

{ TVolgaCalendar }

constructor TVolgaCalendar.Create(AOwner: TComponent);
var i: integer;
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption, csDoubleClicks];
  Height := 160;
  Width := 158;
  BevelInner := bvLowered;
  BevelOuter := bvRaised;
  Caption := '';
  ShowHint := true;
  ParentFont := false;
  Font.Size := 8;
  Font.Style := [];
  FMondayWeek := GetWeekStart <> 6;
  OnCanResize := MainCanResize;
  OnKeyDown := MainKeyDown;
  with TLabel.Create(Self) do begin
    Parent := Self;
    Align := alTop;
    Caption := '';
    Color := clActiveCaption;
    Height := 22;
  end;
  with TSpeedButton.Create(Self) do begin
    Parent := Self;
    Caption := '<<';
    Flat := true;
    Hint := V_PREVYEAR;
    Font.Color := clWhite;
    Font.Size := 10;
    Font.Style := [fsBold];
    SetBounds(2, 2, 18, 22);
    OnClick := btnPriorYearClick;
  end;
  with TSpeedButton.Create(Self) do begin
    Parent := Self;
    Caption := '<';
    Flat := true;
    Hint := V_PREVMON;
    SetBounds(20, 2, 16, 22);
    Font.Color := clWhite;
    Font.Size := 10;
    Font.Style := [fsBold];
    OnClick := btnPriorClick;
  end;
  with TSpeedButton.Create(Self) do begin
    Parent := Self;
    Caption := '>>';
    Flat := true;
    Hint := V_NEXTYEAR;
    Font.Color := clWhite;
    Font.Size := 10;
    Font.Style := [fsBold];
    SetBounds(137, 2, 18, 22);
    OnClick := btnNextYearClick;
  end;
  with TSpeedButton.Create(Self) do begin
    Parent := Self;
    Caption := '>';
    Flat := true;
    Hint := V_NEXTMON;
    Font.Color := clWhite;
    Font.Size := 10;
    Font.Style := [fsBold];
    SetBounds(121, 2, 16, 22);
    OnClick := btnNextClick;
  end;
  LabelYear := TLabel.Create(Self);
  with LabelYear do begin
    Parent := Self;
    SetBounds(92, 6, 24, 13);
    Caption := '2000';
    Transparent := true;
    Font.Size := 8;
    Font.Color := clWhite;
    Font.Style := [];
  end;
  PopupMenu1 := TPopupMenu.Create(Self);
  PopupMenu1.Items.Add(NewItem('01. ' + LongMonthNames[1], 0, false, true, N1Click, 0, 'N1'));
  PopupMenu1.Items.Add(NewItem('02. ' + LongMonthNames[2], 0, false, true, N1Click, 0, 'N2'));
  PopupMenu1.Items.Add(NewItem('03. ' + LongMonthNames[3], 0, false, true, N1Click, 0, 'N3'));
  PopupMenu1.Items.Add(NewItem('04. ' + LongMonthNames[4], 0, false, true, N1Click, 0, 'N4'));
  PopupMenu1.Items.Add(NewItem('05. ' + LongMonthNames[5], 0, false, true, N1Click, 0, 'N5'));
  PopupMenu1.Items.Add(NewItem('06. ' + LongMonthNames[6], 0, false, true, N1Click, 0, 'N6'));
  PopupMenu1.Items.Add(NewItem('07. ' + LongMonthNames[7], 0, false, true, N1Click, 0, 'N7'));
  PopupMenu1.Items.Add(NewItem('08. ' + LongMonthNames[8], 0, false, true, N1Click, 0, 'N8'));
  PopupMenu1.Items.Add(NewItem('09. ' + LongMonthNames[9], 0, false, true, N1Click, 0, 'N9'));
  PopupMenu1.Items.Add(NewItem('10. ' + LongMonthNames[10], 0, false, true, N1Click, 0, 'N10'));
  PopupMenu1.Items.Add(NewItem('11. ' + LongMonthNames[11], 0, false, true, N1Click, 0, 'N11'));
  PopupMenu1.Items.Add(NewItem('12. ' + LongMonthNames[12], 0, false, true, N1Click, 0, 'N12'));
  for i := 1 to 12 do PopupMenu1.Items[i - 1].Tag := i;
  LabelMon := TLabel.Create(Self);
  with LabelMon do begin
    Parent := Self;
    AutoSize := false;
    Alignment := taCenter;
    SetBounds(38, 6, 48, 13);
    Caption := 'September';
    Hint := V_PUSHMON;
    Font.Size := 8;
    Font.Color := clWhite;
    Font.Style := [];
    Transparent := true;
    PopupMenu := PopupMenu1;
    OnClick := btnMonthClick;
  end;
  for i := 1 to 7 do
    Lab[i] := CreateLabel(i);
  with TBevel.Create(Self) do begin
    Parent := Self;
    SetBounds(2, 38, 154, 2);
    Style := bsRaised;
  end;
  with TLabel.Create(Self) do begin
    Parent := Self;
    Align := alBottom;
    Alignment := taCenter;
    Layout := tlCenter;
    Caption := V_TODAY + DateToStr(SysUtils.Date);
    Color := clActiveCaption;
    Font.Color := clWhite;
    Font.Style := [fsBold];
    Height := 20;
    Hint := V_PUSHTODAY;
    OnClick := PanelTodayClick;
  end;
  FSelected := false;
  DecodeDate(SysUtils.Date, FYear, FMonth, FDay);
  for i := 1 to 42 do
    BUT[i] := CreateButton(i);
  Invalidate;
  ChangeWeekStart;
  RefreshCalendar;
end;

function TVolgaCalendar.CreateLabel(num: integer): TLabel;
//var j: integer;
begin
  Result := TLabel.Create(Self);
  with Result do begin
    Parent := Self;
    SetBounds(6 + 22 * (num - 1), 25, 13, 13);
//    if num < 7 then j := num + 1 else j := 1;
//    Caption := ShortDayNames[j];
//    if num > 5 then Font.Color := clRed;
  end;
end;

function TVolgaCalendar.CreateButton(num: integer): TSpeedButton;
begin
  Result := TSpeedButton.Create(self);
  Result.Parent := self; //PanelDay;
  Result.Tag := num;
  Result.Flat := true;
  Result.GroupIndex := 1;
  Result.Spacing := -1;
  Result.Top := (num div 7) * 16;
  Result.SetBounds(((num - 1) mod 7) * 22 + 2, ((num - 1) div 7) * 16 + 41, 22, 16);
  Result.Caption := IntToStr(num);
//  if (num - 1) mod 7 >= 5 then
//    Result.Font.Color := clRed;
  Result.OnClick := btnDay11Click;
end;

procedure TVolgaCalendar.btnDay11Click(Sender: TObject);
begin
  FSelected := true;
  FDay := StrToInt(TSpeedButton(Sender).Caption);
  FDate := EncodeDate(FYear, FMonth, FDay);
  if Assigned(FSelectDate) then FSelectDate(Self);
end;

procedure TVolgaCalendar.btnMonthClick(Sender: TObject);
var P: TPoint;
begin
  P := LabelMon.ClientOrigin;
  PopupMenu1.Popup(P.x, P.y + LabelMon.Height);
end;

procedure TVolgaCalendar.btnNextClick(Sender: TObject);
begin
  if Month < 12 then
    Month := Month + 1
  else begin
    FYear := FYear + 1;
    Month := 1;
  end;
end;

procedure TVolgaCalendar.btnNextYearClick(Sender: TObject);
begin
  Year := FYear + 1;
end;

procedure TVolgaCalendar.btnPriorClick(Sender: TObject);
begin
  if Month > 1 then
    Month := Month - 1
  else begin
    FYear := FYear - 1;
    Month := 12;
  end;
end;

procedure TVolgaCalendar.btnPriorYearClick(Sender: TObject);
begin
  Year := FYear - 1;
end;

function TVolgaCalendar.DaysPerMonth(AYear, AMonth: Integer): Integer;
const
  DaysInMonth: array[1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result := DaysInMonth[AMonth];
  if (AMonth = 2) and IsLeapYear(AYear) then Inc(Result); { leap-year Feb is special }
end;

function TVolgaCalendar.GetText: string;
begin
  Result := DateToStr(FDate);
end;

procedure TVolgaCalendar.N1Click(Sender: TObject);
begin
  Month := TMenuItem(Sender).Tag;
end;

procedure TVolgaCalendar.PanelTodayClick(Sender: TObject);
begin
  DecodeDate(SysUtils.Date, FYear, FMonth, FDay);
  RefreshCalendar;
  FSelected := true;
  if Assigned(FSelectDate) then FSelectDate(Self);
  //Close;
end;

procedure TVolgaCalendar.RefreshCalendar;
var d1, k, DayNum, i: integer;
begin
  k := DaysPerMonth(FYear, FMonth); //число дней в мес€це
  if FDay > k then FDay := k;
  FDate := EncodeDate(FYear, FMonth, FDay);
  LabelMon.Caption := LongMonthNames[FMonth];
  LabelYear.Caption := IntToStr(FYear);
  //обновление буттонов
  d1 := DayOfWeek(EncodeDate(FYear, FMonth, 1)); //день недели 1-го дн€ мес€ца
  if FMondayWeek then
    if d1 = 1 then d1 := 7 else d1 := d1 - 1;
  for i := 1 to 42 do begin
    DayNum := BUT[i].Tag - d1 + 1;
    if (DayNum < 1) or (DayNum > k) then BUT[i].Caption := ''
    else BUT[i].Caption := IntToStr(DayNum);
    BUT[i].Enabled := (BUT[i].Caption <> '');
    //выделенный день
    if BUT[i].Caption = IntToStr(FDay) then BUT[i].Down := true;
  end;
  if Assigned(FChangeDate) then FChangeDate(Self);
end;

procedure TVolgaCalendar.SetDate(const Value: TDateTime);
begin
  if (Value < EncodeDate(1900, 1, 1)) or (Value > EncodeDate(2100, 1, 1)) then
    FDate := SysUtils.Date
  else FDate := Value;
  DecodeDate(FDate, FYear, FMonth, FDay);
  RefreshCalendar;
end;

procedure TVolgaCalendar.SetDay(const Value: word);
begin
  if Value < 1 then FDay := 1
  else if Value > DaysPerMonth(FYear, FMonth) then
    FDay := DaysPerMonth(FYear, FMonth)
  else FDay := Value;
  RefreshCalendar;
end;

procedure TVolgaCalendar.SetMonth(const Value: word);
begin
  if Value < 1 then FMonth := 1
  else if Value > 12 then FMonth := 12
  else FMonth := Value;
  RefreshCalendar;
end;

procedure TVolgaCalendar.SetText(const Value: string);
begin
  try Date := StrToDate(Value);
  except; end;
end;

procedure TVolgaCalendar.SetYear(const Value: word);
begin
  if (Value > 1900) and (Value < 2100) then
    FYear := Value;
  RefreshCalendar;
end;

procedure TVolgaCalendar.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  AHeight := 160;
  AWidth := 158;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TVolgaCalendar.MainCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Resize := false;
end;

procedure TVolgaCalendar.MainKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then begin
    FSelected := true;
    if Selected and Assigned(FSelectDate) then FSelectDate(Self);
  end else if (Key = VK_ESCAPE) then begin
    FSelected := false;
  end;
end;

procedure TVolgaCalendar.ChangeWeekStart;
var i, j: integer;
begin
  for i := 1 to 7 do
  begin
    if FMondayWeek then
    begin
      if i < 7 then j := i + 1 else j := 1;
      Lab[i].Caption := ShortDayNames[j];
      if i > 5 then Lab[i].Font.Color := clRed;
    end else begin
      Lab[i].Caption := ShortDayNames[i];
      if (i = 1) or (i = 7) then Lab[i].Font.Color := clRed;
    end;
  end;
  for i := 1 to 42 do
  begin
    if FMondayWeek then
      if (i - 1) mod 7 >= 5 then
        BUT[i].Font.Color := clRed
      else
    else
      if (i mod 7 = 1) or (i mod 7 = 0) then
        BUT[i].Font.Color := clRed;
  end;
end;

function TVolgaCalendar.GetWeekStart: integer;
var
  A: array[0..1] of char;
begin
  GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IFIRSTDAYOFWEEK, A, SizeOf(A));
  Result := Ord(A[0]) - Ord('0');  //0-Monday, 6-Sunday
end;

end.

