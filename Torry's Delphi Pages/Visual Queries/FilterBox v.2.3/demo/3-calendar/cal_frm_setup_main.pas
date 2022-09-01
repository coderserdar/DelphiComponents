unit cal_frm_setup_main;

interface
{$I psc_defines.inc}

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  Buttons,
  CheckLst,

  psc_calendar,
  psc_procs,

  cal_frm_setup_preferences,
  cal_frm_setup_holidays,
  cal_frm_setup_colors,

  ComCtrls;

type
  Tpsc_frm_setup_main = class(TForm)
    PageControl_Manager: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Panel_Button: TPanel;
    Button_OK: TButton;
    Button_Cancel: TButton;
    Button_Apply: TButton;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure Edit_SelectionLengthKeyPress(Sender: TObject; var Key: Char);
    procedure Button_ApplyClick(Sender: TObject);
  private
    FMYPSCCalendar : TPSCCalendar;
    FMyPSCCalUpdate : TNotifyEvent;
    procedure DisplayPanel(ATabSheet : TTabSheet; APanel : TControl);
  public
    procedure DoPrefChange(Sender : TObject);
    procedure DoHoliChange(Sender : TObject);
    procedure DoColorChange(Sender : TObject);
    property MYPSCCalendar : TPSCCalendar read FMYPSCCalendar write FMYPSCCalendar;
    property MyPSCCalUpdate : TNotifyEvent read FMyPSCCalUpdate write FMyPSCCalUpdate;
  end;

var
  psc_frm_setup_main: Tpsc_frm_setup_main;

procedure PSCCalendarLoad(APSCCalendar : TPSCCalendar);
procedure PSCCalendarSave(APSCCalendar : TPSCCalendar);
procedure AssignCalendar(DestPSCCalendar, APSCCalendar : TPSCCalendar);

implementation

{$R *.dfm}

procedure Tpsc_frm_setup_main.FormCreate(Sender: TObject);
begin
  cal_frm_setup_preferences.psc_frm_setup_preferences := Tpsc_frm_setup_preferences.Create(Application);
  with cal_frm_setup_preferences.psc_frm_setup_preferences do
  begin
    MyPSCCalChange := DoPrefChange;
    DisplayPanel(TabSheet1, Panel_Preferences);
    DisplayPanel(TabSheet2, Panel_AdditionPreferences);
  end;

  cal_frm_setup_holidays.psc_frm_setup_holidays := Tpsc_frm_setup_holidays.Create(Application);
  with cal_frm_setup_holidays.psc_frm_setup_holidays do
  begin
    MyPSCHoliCalChange := DoHoliChange;
    DisplayPanel(TabSheet3, Panel_Holidays);
  end;

  cal_frm_setup_colors.psc_frm_setup_colors := Tpsc_frm_setup_colors.Create(Application);
  with cal_frm_setup_colors.psc_frm_setup_colors do
  begin
    MyPSCColorCalChange := DoColorChange;
    DisplayPanel(TabSheet4, Panel_Colors);
  end;
  MYPSCCalendar := TPSCCalendar.Create(Self);
  MYPSCCalendar.Name := 'MyCalendar';
  MYPSCCalendar.Parent := Self;
  MYPSCCalendar.Visible := false;
end;

procedure Tpsc_frm_setup_main.Edit_SelectionLengthKeyPress(
  Sender: TObject; var Key: Char);
begin
  if (Ord(Key) < 48) or (Ord(Key) > 57) then
    Key := #0;
end;

procedure PSCCalendarLoad(APSCCalendar : TPSCCalendar);
begin
  with psc_frm_setup_main do
  begin
    begin
      AssignCalendar(psc_frm_setup_colors.PSCCalendar_Colors, APSCCalendar);
      psc_frm_setup_preferences.MYPSCPrefCalendar := psc_frm_setup_colors.PSCCalendar_Colors;
      psc_frm_setup_holidays.MYPSCHoliCalendar := psc_frm_setup_colors.PSCCalendar_Colors;
      psc_frm_setup_preferences.PSCCalendarPrefLoad(APSCCalendar);
      psc_frm_setup_holidays.PSCCalendarHoliLoad(APSCCalendar);
      psc_frm_setup_colors.PSCCalendarColorFontLoad(APSCCalendar);
    end;
  end;
end;

procedure Tpsc_frm_setup_main.DisplayPanel(ATabSheet : TTabSheet; APanel: TControl);
begin
  APanel.Parent := ATabSheet;
  APanel.Align:=alClient
end;

procedure PSCCalendarSave(APSCCalendar : TPSCCalendar);
begin
  with psc_frm_setup_main do
  begin
    cal_frm_setup_preferences.psc_frm_setup_preferences.PSCCalendarPrefSave(APSCCalendar);
    cal_frm_setup_holidays.psc_frm_setup_holidays.PSCCalendarHoliSave(APSCCalendar);
    cal_frm_setup_colors.psc_frm_setup_colors.PSCCalendarColorFontSave(APSCCalendar);
  end;
end;

procedure Tpsc_frm_setup_main.Button_ApplyClick(Sender: TObject);
begin
  Button_Apply.Enabled := false;
  MyPSCCalUpdate(Self);
end;

procedure AssignCalendar(DestPSCCalendar, APSCCalendar : TPSCCalendar);
begin
  with DestPSCCalendar do
  begin
    CalendarStyle := APSCCalendar.CalendarStyle;
    SelectStyle := APSCCalendar.SelectStyle;
    HeaderFormat := APSCCalendar.HeaderFormat;
    FirstDayFormat := APSCCalendar.FirstDayFormat;
    WeekLineStyle := APSCCalendar.WeekLineStyle;
    WantEsc := APSCCalendar.WantEsc;
    ShowFocusRect := APSCCalendar.ShowFocusRect;
    TodayStyle := APSCCalendar.TodayStyle;
    ShowHeader := APSCCalendar.ShowHeader;
    HeaderStyle := APSCCalendar.HeaderStyle;
    ShowFooter := APSCCalendar.ShowFooter;
    CalendarsInWidth := APSCCalendar.CalendarsInWidth;
    CalendarsInHeight := APSCCalendar.CalendarsInHeight;
    SizeStyle := APSCCalendar.SizeStyle;
    PastDaysAsGrayed := APSCCalendar.PastDaysAsGrayed;
    WeekDayCase := APSCCalendar.WeekDayCase;
    DayVertAlign := APSCCalendar.DayVertAlign;
    DayHorzAlign := APSCCalendar.DayHorzAlign;
    DayHeaderHorzAlign := APSCCalendar.DayHeaderHorzAlign;
    AlterEvenOdd := APSCCalendar.AlterEvenOdd;
    ShowDayDelimiters := APSCCalendar.ShowDayDelimiters;
    DayHeaderStyle := APSCCalendar.DayHeaderStyle;

    CustomHolidays.Assign(APSCCalendar.CustomHolidays);
    Colors.Assign(APSCCalendar.Colors);
    SelectionAndBorderDists := APSCCalendar.SelectionAndBorderDists;
    with PrintOptions do
    begin
      Header := APSCCalendar.PrintOptions.Header;
      HeaderFont := APSCCalendar.PrintOptions.HeaderFont;
      Title := APSCCalendar.PrintOptions.Title;
      Options := APSCCalendar.PrintOptions.Options;
      Overlap := APSCCalendar.PrintOptions.Overlap;
      ParentHeaderFont := APSCCalendar.PrintOptions.ParentHeaderFont;
    end;
    
    Height := APSCCalendar.Height;
    Width := APSCCalendar.Width;

    HolidayNames.Assign(APSCCalendar.HolidayNames);
    Brush.Assign(APSCCalendar.Brush);

    Color := APSCCalendar.Color;

    Enabled := APSCCalendar.Enabled;
    Font := APSCCalendar.Font;
    ParentColor := APSCCalendar.ParentColor;
    ParentFont := APSCCalendar.ParentFont;
    PopupType := APSCCalendar.PopupType;
    ForceShowHint := APSCCalendar.ForceShowHint;
    AutoSize := APSCCalendar.AutoSize;
    MultiSelect := APSCCalendar.MultiSelect;
    ClearOnKeyMoving := APSCCalendar.ClearOnKeyMoving;
    ShowMonthPopup := APSCCalendar.ShowMonthPopup;
    ShowNavButtons := APSCCalendar.ShowNavButtons;
    ShowToday := APSCCalendar.ShowToday;
    SideWeekSelect := APSCCalendar.SideWeekSelect;
    ShowMonthDividers := APSCCalendar.ShowMonthDividers;
    ShowWeekNumbers := APSCCalendar.ShowWeekNumbers;
    ShowHorzLines := APSCCalendar.ShowHorzLines;
    ShowVertLines := APSCCalendar.ShowVertLines;
    WeekDayNames := APSCCalendar.WeekDayNames;
    FirstWeekOfYear := APSCCalendar.FirstWeekOfYear;
    FirstDayOfWeek := APSCCalendar.FirstDayOfWeek;
    WorkDays :=  APSCCalendar.WorkDays;
    MinDateLimit := APSCCalendar.MinDateLimit;
    MinDate := APSCCalendar.MinDate;
    MaxDateLimit := APSCCalendar.MaxDateLimit;
    MaxDate := APSCCalendar.MaxDate;
    StartDate := APSCCalendar.StartDate;
    Flat := APSCCalendar.Flat;
    BorderStyle := APSCCalendar.BorderStyle;
    BorderEdges := APSCCalendar.BorderEdges;
    MaxSelDates := APSCCalendar.MaxSelDates;
    WeekCursor := APSCCalendar.WeekCursor;
    SelectKind := APSCCalendar.SelectKind;
    ExtendedSelect := APSCCalendar.ExtendedSelect;
    ReadOnly := APSCCalendar.ReadOnly;
    ArrowStyle := APSCCalendar.ArrowStyle;
    ShowArrowEdges := APSCCalendar.ShowArrowEdges;
    LeftArrow := APSCCalendar.LeftArrow;
    LeftArrowHot := APSCCalendar.LeftArrowHot;
    RightArrow := APSCCalendar.RightArrow;
    RightArrowHot := APSCCalendar.RightArrowHot;
    ShowHolidays := APSCCalendar.ShowHolidays;
    HeaderCase := APSCCalendar.HeaderCase;
    ShortMonthName := APSCCalendar.ShortMonthName;
    DefaultHolidays := APSCCalendar.DefaultHolidays;
    CancelPrinting := APSCCalendar.CancelPrinting;
    CursorDate := APSCCalendar.CursorDate;
    SelStart := APSCCalendar.SelStart;
    SelFinish := APSCCalendar.SelFinish;
    WeeksSelected := APSCCalendar.WeeksSelected;
    NavButtonsTimer := APSCCalendar.NavButtonsTimer;
  end;
end;

procedure Tpsc_frm_setup_main.DoPrefChange(Sender: TObject);
begin
  Button_Apply.Enabled := true;
end;

procedure Tpsc_frm_setup_main.DoColorChange(Sender: TObject);
begin
  Button_Apply.Enabled := true;
end;

procedure Tpsc_frm_setup_main.DoHoliChange(Sender: TObject);
begin
  Button_Apply.Enabled := true;
end;

end.
