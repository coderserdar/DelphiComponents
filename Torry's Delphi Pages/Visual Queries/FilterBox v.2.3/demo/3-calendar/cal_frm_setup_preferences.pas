unit cal_frm_setup_preferences;

interface
{$I psc_defines.inc}

uses
  Windows,
  Messages,
  SysUtils,
{$IFDEF D6}
  Variants,
{$ENDIF}
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  ComCtrls,

  myla_interfaces,
  myla_system,

  psc_procs,
  psc_wrapper,
  psc_calendar,
  psc_edit,
  psc_edit_date,

  cal_demo_const;

type
  Tpsc_frm_setup_preferences = class(TForm)
    Panel_AdditionPreferences: TPanel;
    ComboBox_WeekDayCase: TComboBox;
    ComboBox_DayVertAlign: TComboBox;
    ComboBox_DayHorzAlign: TComboBox;
    CheckBox_ShowFocusRect: TCheckBox;
    CheckBox_ShowHeader: TCheckBox;
    CheckBox_ShowFooter: TCheckBox;
    Label_WeekDayCase: TLabel;
    Label_DayVertAlign: TLabel;
    Lable_DayHorzAlign: TLabel;
    ComboBox_SelectKind: TComboBox;
    Label_SelectKind: TLabel;
    ComboBox_HeaderCase: TComboBox;
    Label_HeaderCase: TLabel;
    ComboBox_WeekDayNames: TComboBox;
    Label_WeekDayNames: TLabel;
    Panel_Preferences: TPanel;
    Label_CalendarStyle: TLabel;
    Label_PopupType: TLabel;
    LabelArrowStyle: TLabel;
    Label_SelectionLength: TLabel;
    CheckBox_MultiSelect: TCheckBox;
    ComboBox_CalendarStyle: TComboBox;
    CheckBox_MaxDateLimit: TCheckBox;
    CheckBox_MinDateLimit: TCheckBox;
    PSCDateEdit_MinDate: TPSCDateEdit;
    PSCDateEdit_MaxDate: TPSCDateEdit;
    ComboBox_PopupType: TComboBox;
    CheckBox_Flat: TCheckBox;
    CheckBox_KeyMoving: TCheckBox;
    CheckBox_ReadOnly: TCheckBox;
    CheckBox_ShowToday: TCheckBox;
    ComboBox_ArrowStyle: TComboBox;
    CheckBox_SideClickSelectsWeek: TCheckBox;
    CheckBox_ExtendedSelect: TCheckBox;
    CheckBox_ShortMonth: TCheckBox;
    PSCEdit_SelectionLength: TPSCEdit;
    GroupBox_WorkDays: TGroupBox;
    CheckBox_WorkDay3: TCheckBox;
    CheckBox_WorkDay1: TCheckBox;
    CheckBox_WorkDay2: TCheckBox;
    CheckBox_WorkDay4: TCheckBox;
    CheckBox_WorkDay5: TCheckBox;
    CheckBox_WorkDay6: TCheckBox;
    CheckBox_WorkDay7: TCheckBox;
    ComboBox_SelectStyle: TComboBox;
    Label_SelectStyle: TLabel;
    ComboBox_TodayStyle: TComboBox;
    Label_TodayStyle: TLabel;
    CheckBox_ShowArrowEdges: TCheckBox;
    CheckBox_NavigatuionButtons: TCheckBox;
    CheckBox_MonthDividers: TCheckBox;
    CheckBox_ShowMonthPopup: TCheckBox;
    CheckBox_NavButtonsTimer: TCheckBox;
    CheckBox_WantEsc: TCheckBox;
    CheckBox_PastDaysAsGrayed: TCheckBox;
    CheckBox_AlterEvenOdd: TCheckBox;
    ComboBox_DayHeaderHorzAlign: TComboBox;
    Lable_DayHeaderHorzAlign: TLabel;
    PSCCalendar_Preferences: TPSCCalendar;
    Label_CalInHeigth: TLabel;
    PSCEdit_CalendarsInHeight: TPSCEdit;
    Label_CalInWidth: TLabel;
    PSCEditCalendarsInWidth: TPSCEdit;
    Label_FirstWeekOfYear: TLabel;
    ComboBox_FirstWeekOfYear: TComboBox;
    Label_FirstDayOfWeek: TLabel;
    ComboBox_FirstDayOfWeek: TComboBox;
    CheckBox_ShowVertLines: TCheckBox;
    CheckBox_ShowHorzLines: TCheckBox;
    CheckBox_WeekNumbers: TCheckBox;
    procedure CheckBox_MinDateLimitClick(Sender: TObject);
    procedure CheckBox_MaxDateLimitClick(Sender: TObject);
    procedure PSCEdit_SelectionLengthButtonClick(Sender: TObject;
      ABtnIndex: Integer);
    procedure ComboBox_CalendarStyleChange(Sender: TObject);
    procedure ComboBox_FirstDayOfWeekChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox_WorkDay1Click(Sender: TObject);
    procedure CheckBox_WorkDay2Click(Sender: TObject);
    procedure CheckBox_WorkDay3Click(Sender: TObject);
    procedure CheckBox_WorkDay4Click(Sender: TObject);
    procedure CheckBox_WorkDay5Click(Sender: TObject);
    procedure CheckBox_WorkDay6Click(Sender: TObject);
    procedure CheckBox_WorkDay7Click(Sender: TObject);
    procedure ComboBox_SelectKindChange(Sender: TObject);
    procedure ComboBox_ArrowStyleChange(Sender: TObject);
    procedure ComboBox_WeekCursorChange(Sender: TObject);
    procedure ComboBox_FirstWeekOfYearChange(Sender: TObject);
    procedure ComboBox_HeaderCaseChange(Sender: TObject);
    procedure ComboBox_WeekDayNamesChange(Sender: TObject);
    procedure ComboBox_PopupTypeChange(Sender: TObject);
    procedure PSCEdit_SelectionLengthChange(Sender: TObject);
    procedure CheckBox_MultiSelectClick(Sender: TObject);
    procedure CheckBox_KeyMovingClick(Sender: TObject);
    procedure CheckBox_ExtendedSelectClick(Sender: TObject);
    procedure CheckBox_SideClickSelectsWeekClick(Sender: TObject);
    procedure CheckBox_ShowArrowEdgesClick(Sender: TObject);
    procedure CheckBox_FlatClick(Sender: TObject);
    procedure CheckBox_ShowTodayClick(Sender: TObject);
    procedure CheckBox_NavigatuionButtonsClick(Sender: TObject);
    procedure CheckBox_NavButtonsTimerClick(Sender: TObject);
    procedure CheckBox_MonthDividersClick(Sender: TObject);
    procedure CheckBox_ShowHorzLinesClick(Sender: TObject);
    procedure CheckBox_ShowVertLinesClick(Sender: TObject);
    procedure CheckBox_WeekNumbersClick(Sender: TObject);
    procedure CheckBox_ShowMonthPopupClick(Sender: TObject);
    procedure CheckBox_ShortMonthClick(Sender: TObject);
    procedure CheckBox_ReadOnlyClick(Sender: TObject);
    procedure PSCDateEdit_MinDateChange(Sender: TObject);
    procedure PSCDateEdit_MaxDateChange(Sender: TObject);
    procedure ComboBox_SelectStyleChange(Sender: TObject);
    procedure ComboBox_TodayStyleChange(Sender: TObject);
    procedure ComboBox_WeekDayCaseChange(Sender: TObject);
    procedure ComboBox_DayVertAlignChange(Sender: TObject);
    procedure ComboBox_DayHorzAlignChange(Sender: TObject);
    procedure CheckBox_WantEscClick(Sender: TObject);
    procedure CheckBox_PastDaysAsGrayedClick(Sender: TObject);
    procedure CheckBox_AlterEvenOddClick(Sender: TObject);
    procedure CheckBox_ShowFocusRectClick(Sender: TObject);
    procedure CheckBox_ShowHeaderClick(Sender: TObject);
    procedure CheckBox_ShowFooterClick(Sender: TObject);
    procedure PSCEdit_CalendarsInHeightButtonClick(Sender: TObject;
      ABtnIndex: Integer);
    procedure PSCEditCalendarsInWidthButtonClick(Sender: TObject;
      ABtnIndex: Integer);
    procedure PSCEdit_CalendarsInHeightChange(Sender: TObject);
    procedure PSCEditCalendarsInWidthChange(Sender: TObject);
    procedure ComboBox_DayHeaderHorzAlignChange(Sender: TObject);
    procedure PSCEdit_SelectionLengthKeyPress(Sender: TObject;
      var Key: Char);
  private
    FMYPSCPrefCalendar : TPSCCalendar;
    FMyPSCCalChange : TNotifyEvent;
    procedure CheckBoxPrefLoad(APSCCalendar : TPSCCalendar);
    procedure ComboBoxPrefLoad(APSCCalendar : TPSCCalendar);
    procedure ComboBoxPrefFill;
    procedure ChangeCaption(i : integer; APSCCalendar : TPSCCalendar);
    procedure CheckBoxPrefSave(APSCCalendar : TPSCCalendar);
    procedure ComboboxPrefSave(APSCCalendar : TPSCCalendar);
    procedure SetWorkDays(APSCCalendar : TPSCCalendar; AIsWorked : boolean; ID : integer);
    { Private declarations }
  public
    procedure PSCCalendarPrefLoad(APSCCalendar : TPSCCalendar);
    procedure PSCCalendarPrefSave(APSCCalendar : TPSCCalendar);
    property MYPSCPrefCalendar : TPSCCalendar read FMYPSCPrefCalendar write FMYPSCPrefCalendar;
    property MyPSCCalChange : TNotifyEvent read FMyPSCCalChange write FMyPSCCalChange; 
    { Public declarations }
  end;

var
  psc_frm_setup_preferences: Tpsc_frm_setup_preferences;

//function CursorFill : TStringList;
procedure SetWorkDay(APSCCalendar : TPSCCalendar; AIsWorked : boolean; I : integer);

implementation

{$R *.dfm}

procedure Tpsc_frm_setup_preferences.CheckBox_MinDateLimitClick(
  Sender: TObject);
begin
  PSCDateEdit_MinDate.Enabled := CheckBox_MinDateLimit.Checked;
  MYPSCPrefCalendar.MinDateLimit := CheckBox_MinDateLimit.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_MaxDateLimitClick(
  Sender: TObject);
begin
  PSCDateEdit_MaxDate.Enabled := CheckBox_MaxDateLimit.Checked;
  MYPSCPrefCalendar.MaxDateLimit := CheckBox_MaxDateLimit.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.PSCEdit_SelectionLengthButtonClick(
  Sender: TObject; ABtnIndex: Integer);
begin
  if ABtnIndex > 0 then
    PSCEdit_SelectionLength.Text := IntToStr(StrToInt(PSCEdit_SelectionLength.Text) - 1)
  else
    PSCEdit_SelectionLength.Text := IntToStr(StrToInt(PSCEdit_SelectionLength.Text) + 1);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.ComboBox_CalendarStyleChange(
  Sender: TObject);
begin
  with ComboBox_CalendarStyle do
    PSCSetPropValue(MYPSCPrefCalendar, 'CalendarStyle', Items[ItemIndex]);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.ComboBox_FirstDayOfWeekChange(
  Sender: TObject);
var
  i : integer;
begin
  MYPSCPrefCalendar.FirstDayOfWeek := TPSCFirstDayOfWeek(integer(ComboBox_FirstDayOfWeek.Items.Objects[ComboBox_FirstDayOfWeek.ItemIndex]));
  i := integer(MYPSCPrefCalendar.FirstDayOfWeek);
  if i = 0 then
    i := 2;
  ChangeCaption(i, MYPSCPrefCalendar);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.FormCreate(Sender: TObject);
begin
  ComboBoxPrefFill;
  MYPSCPrefCalendar := TPSCCalendar.Create(Self);
  MYPSCPrefCalendar.Visible := false;
end;

procedure Tpsc_frm_setup_preferences.ComboBoxPrefFill;
var
  i : integer;
  MyComboItems : IPSCStrings;
begin
  MyComboItems := PSCCreateStringList(ioOwned);
  for i := 1 to 7 do
    ComboBox_FirstDayOfWeek.Items.AddObject(FormatSettings.LongDayNames[i], TObject(i));
  PSCGetEnumTypeValueNames(TypeInfo(TPSCCalendarStyle),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_CalendarStyle.Items));
  PSCGetEnumTypeValueNames(TypeInfo(TPSCArrowStyle),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_ArrowStyle.Items));
  PSCGetEnumTypeValueNames(TypeInfo(TPSCTextCase),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_HeaderCase.Items));
  PSCGetEnumTypeValueNames(TypeInfo(TPSCWeekDaysLength),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_WeekDayNames.Items));
  PSCGetEnumTypeValueNames(TypeInfo(TPSCCalSelectKind),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_SelectKind.Items));
  PSCGetEnumTypeValueNames(TypeInfo(TPSCFirstWeekOfYear),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_FirstWeekOfYear.Items));
  PSCGetEnumTypeValueNames(TypeInfo(TPSCMonthPopupType),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_PopupType.Items));
  PSCGetEnumTypeValueNames(TypeInfo(TPSCCalendarWeekLineStyle),MyComboItems);
  PSCGetEnumTypeValueNames(TypeInfo(TPSCCalendarSelStyle),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_SelectStyle.Items));
  PSCGetEnumTypeValueNames(TypeInfo(TPSCCalendarTodayStyle),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_TodayStyle.Items));
  PSCGetEnumTypeValueNames(TypeInfo(TPSCCalendarHeaderStyle),MyComboItems);
  PSCGetEnumTypeValueNames(TypeInfo(TPSCCalendarSizeStyle),MyComboItems);
  PSCGetEnumTypeValueNames(TypeInfo(TPSCCalendarDayHeaderStyle),MyComboItems);
  PSCGetEnumTypeValueNames(TypeInfo(TPSCTextCase),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_WeekDayCase.Items));
  PSCGetEnumTypeValueNames(TypeInfo(TPSCVertAlign),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_DayVertAlign.Items));
  PSCGetEnumTypeValueNames(TypeInfo(TPSCHorzAlign),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_DayHorzAlign.Items));
  PSCGetEnumTypeValueNames(TypeInfo(TPSCHorzAlign),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_DayHeaderHorzAlign.Items));
end;

procedure Tpsc_frm_setup_preferences.CheckBox_WorkDay1Click(
  Sender: TObject);
begin
  SetWorkDays(MYPSCPrefCalendar, CheckBox_WorkDay1.Checked, -1);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_WorkDay2Click(
  Sender: TObject);
begin
  SetWorkDays(MYPSCPrefCalendar, CheckBox_WorkDay2.Checked, 0);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_WorkDay3Click(
  Sender: TObject);
begin
  SetWorkDays(MYPSCPrefCalendar, CheckBox_WorkDay3.Checked, 1);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_WorkDay4Click(
  Sender: TObject);
begin
  SetWorkDays(MYPSCPrefCalendar, CheckBox_WorkDay4.Checked, 2);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_WorkDay5Click(
  Sender: TObject);
begin
  SetWorkDays(MYPSCPrefCalendar, CheckBox_WorkDay5.Checked, 3);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_WorkDay6Click(
  Sender: TObject);
begin
  SetWorkDays(MYPSCPrefCalendar, CheckBox_WorkDay6.Checked, 4);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_WorkDay7Click(
  Sender: TObject);
begin
  SetWorkDays(MYPSCPrefCalendar, CheckBox_WorkDay7.Checked, 5);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.PSCCalendarPrefLoad(APSCCalendar : TPSCCalendar);
var
  i : integer;
begin
  with psc_frm_setup_preferences, APSCCalendar do
  begin
    if MinDate <= 0 then
      PSCDateEdit_MinDate.Date := StrToDate('01' + FormatSettings.DateSeparator + '01' + FormatSettings.DateSeparator + '2002')
    else
      PSCDateEdit_MinDate.Date := MinDate;
    if MaxDate <= 0 then
      PSCDateEdit_MaxDate.Date := StrToDate('01' + FormatSettings.DateSeparator + '01' + FormatSettings.DateSeparator + '2002')
    else
      PSCDateEdit_MaxDate.Date := MaxDate;
    ComboBoxPrefLoad(APSCCalendar);
    CheckBoxPrefLoad(APSCCalendar);
    PSCEdit_CalendarsInHeight.Text := IntToStr(CalendarsInHeight);
    PSCEditCalendarsInWidth.Text := IntToStr(CalendarsInWidth);
    i := integer(FirstDayOfWeek);
    if i = 0 then
      i := 2;
    ChangeCaption(i, APSCCalendar);
  end;
end;

procedure Tpsc_frm_setup_preferences.CheckBoxPrefLoad(
  APSCCalendar: TPSCCalendar);
begin
  with APSCCalendar do
  begin
    CheckBox_SideClickSelectsWeek.Checked := SideWeekSelect;
    CheckBox_ExtendedSelect.Checked := ExtendedSelect;
    CheckBox_ShortMonth.Checked := ShortMonthName;
    CheckBox_MultiSelect.Checked := MultiSelect;
    CheckBox_MinDateLimit.Checked := MinDateLimit;
    CheckBox_MaxDateLimit.Checked := MaxDateLimit;
    CheckBox_Flat.Checked := Flat;
    CheckBox_KeyMoving.Checked := ClearOnKeyMoving;
    CheckBox_ReadOnly.Checked := ReadOnly;
    CheckBox_NavigatuionButtons.Checked := ShowNavButtons;
    CheckBox_NavButtonsTimer.Checked := NavButtonsTimer;
    CheckBox_MonthDividers.Checked := ShowMonthDividers;
    CheckBox_ShowHorzLines.Checked := ShowHorzLines;
    CheckBox_ShowVertLines.Checked := ShowVertLines;
    CheckBox_WeekNumbers.Checked := ShowWeekNumbers;
    CheckBox_ShowArrowEdges.Checked := ShowArrowEdges;
    CheckBox_ShowMonthPopup.Checked := ShowMonthPopup;
    CheckBox_ShowToday.Checked := ShowToday;

    CheckBox_WantEsc.Checked := WantEsc;
    CheckBox_PastDaysAsGrayed.Checked := PastDaysAsGrayed;
    CheckBox_AlterEvenOdd.Checked := AlterEvenOdd;
    CheckBox_ShowFocusRect.Checked := ShowFocusRect;
    CheckBox_ShowHeader.Checked := ShowHeader;
    CheckBox_ShowFooter.Checked := ShowFooter;
  end;
end;

procedure Tpsc_frm_setup_preferences.ComboBoxPrefLoad(
  APSCCalendar: TPSCCalendar);
begin
  with APSCCalendar do
  begin
    ComboBox_CalendarStyle.ItemIndex := integer(CalendarStyle);
    ComboBox_SelectKind.ItemIndex  := integer(SelectKind);
    ComboBox_HeaderCase.ItemIndex := integer(HeaderCase);
    ComboBox_PopupType.ItemIndex := integer(PopupType);
    ComboBox_WeekDayNames.ItemIndex := integer(WeekDayNames);
    ComboBox_ArrowStyle.ItemIndex := integer(ArrowStyle);
    ComboBox_FirstWeekOfYear.ItemIndex := integer(FirstWeekOfYear);
    ComboBox_SelectStyle.ItemIndex := integer(SelectStyle);
    ComboBox_TodayStyle.ItemIndex := integer(TodayStyle);
    ComboBox_WeekDayCase.ItemIndex := integer(WeekDayCase);
    ComboBox_DayVertAlign.ItemIndex := integer(DayVertAlign);
    ComboBox_DayHorzAlign.ItemIndex := integer(DayHorzAlign);
    ComboBox_DayHeaderHorzAlign.ItemIndex := integer(DayHeaderHorzAlign);
    PSCEdit_SelectionLength.Text := IntToStr(MaxSelDates);
    ComboBox_FirstDayOfWeek.ItemIndex := integer(FirstDayOfWeek);
    if ComboBox_FirstDayOfWeek.ItemIndex > 0 then
      ChangeCaption(integer(ComboBox_FirstDayOfWeek.Items.Objects[ComboBox_FirstDayOfWeek.ItemIndex]), APSCCalendar)
    else
      ChangeCaption(2, APSCCalendar);
  end;
end;

procedure Tpsc_frm_setup_preferences.ChangeCaption(i: integer;
  APSCCalendar: TPSCCalendar);
begin
  with APSCCalendar do
  begin
    CheckBox_WorkDay1.Caption := FormatSettings.ShortDayNames[1 + (i - 1) mod 7];
    CheckBox_WorkDay2.Caption := FormatSettings.ShortDayNames[1 + i mod 7];
    CheckBox_WorkDay3.Caption := FormatSettings.ShortDayNames[1 + (1 + i) mod 7];
    CheckBox_WorkDay4.Caption := FormatSettings.ShortDayNames[1 + (2 + i) mod 7];
    CheckBox_WorkDay5.Caption := FormatSettings.ShortDayNames[1 + (3 + i) mod 7];
    CheckBox_WorkDay6.Caption := FormatSettings.ShortDayNames[1 + (4 + i) mod 7];
    CheckBox_WorkDay7.Caption := FormatSettings.ShortDayNames[1 + (5 + i) mod 7];

    CheckBox_WorkDay1.Checked := (TPSCFirstDayOfWeek(1 + (i - 1) mod 7) in WorkDays);
    CheckBox_WorkDay2.Checked := (TPSCFirstDayOfWeek(1 + i mod 7) in WorkDays);
    CheckBox_WorkDay3.Checked := (TPSCFirstDayOfWeek(1 + (1 + i) mod 7) in WorkDays);
    CheckBox_WorkDay4.Checked := (TPSCFirstDayOfWeek(1 + (2 + i) mod 7) in WorkDays);
    CheckBox_WorkDay5.Checked := (TPSCFirstDayOfWeek(1 + (3 + i) mod 7) in WorkDays);
    CheckBox_WorkDay6.Checked := (TPSCFirstDayOfWeek(1 + (4 + i) mod 7) in WorkDays);
    CheckBox_WorkDay7.Checked := (TPSCFirstDayOfWeek(1 + (5 + i) mod 7) in WorkDays);
  end;
end;

procedure Tpsc_frm_setup_preferences.PSCCalendarPrefSave(APSCCalendar : TPSCCalendar);
begin
  with psc_frm_setup_preferences, APSCCalendar do
  begin
    CalendarStyle := TPSCCalendarStyle(ComboBox_CalendarStyle.ItemIndex);
    CheckBoxPrefSave(APSCCalendar);
    ComboBoxPrefSave(APSCCalendar);
    MinDate := PSCDateEdit_MinDate.Date;
    MaxDate := PSCDateEdit_MaxDate.Date;
    CalendarsInHeight := StrToInt(PSCEdit_CalendarsInHeight.Text);
    CalendarsInWidth := StrToInt(PSCEditCalendarsInWidth.Text);
  end;
end;

procedure Tpsc_frm_setup_preferences.CheckBoxPrefSave(
  APSCCalendar: TPSCCalendar);
var
  i : integer;
begin
  with APSCCalendar do
  begin
    i := integer(ComboBox_FirstDayOfWeek.Items.Objects[ComboBox_FirstDayOfWeek.ItemIndex]);
    if i = 0 then
      i := 2;
    SideWeekSelect := CheckBox_SideClickSelectsWeek.Checked;
    ExtendedSelect := CheckBox_ExtendedSelect.Checked;
    ShortMonthName := CheckBox_ShortMonth.Checked;
    MultiSelect := CheckBox_MultiSelect.Checked;
    MinDateLimit := CheckBox_MinDateLimit.Checked;
    MaxDateLimit := CheckBox_MaxDateLimit.Checked;
    SetWorkDay(APSCCalendar, CheckBox_WorkDay1.Checked, 1 + (i - 1) mod 7);
    SetWorkDay(APSCCalendar, CheckBox_WorkDay2.Checked, 1 + i mod 7);
    SetWorkDay(APSCCalendar, CheckBox_WorkDay3.Checked, 1 + (i + 1) mod 7);
    SetWorkDay(APSCCalendar, CheckBox_WorkDay4.Checked, 1 + (i + 2) mod 7);
    SetWorkDay(APSCCalendar, CheckBox_WorkDay5.Checked, 1 + (i + 3) mod 7);
    SetWorkDay(APSCCalendar, CheckBox_WorkDay6.Checked, 1 + (i + 4) mod 7);
    SetWorkDay(APSCCalendar, CheckBox_WorkDay7.Checked, 1 + (i + 5) mod 7);
    Flat := CheckBox_Flat.Checked;
    ClearOnKeyMoving := CheckBox_KeyMoving.Checked;
    ReadOnly := CheckBox_ReadOnly.Checked;
    ShowNavButtons := CheckBox_NavigatuionButtons.Checked;
    NavButtonsTimer := CheckBox_NavButtonsTimer.Checked;
    ShowMonthDividers := CheckBox_MonthDividers.Checked;
    ShowHorzLines := CheckBox_ShowHorzLines.Checked;
    ShowVertLines := CheckBox_ShowVertLines.Checked;
    ShowWeekNumbers := CheckBox_WeekNumbers.Checked;
    ShowArrowEdges := CheckBox_ShowArrowEdges.Checked;
    ShowMonthPopup := CheckBox_ShowMonthPopup.Checked;
    ShowToday := CheckBox_ShowToday.Checked;

    WantEsc := CheckBox_WantEsc.Checked;
    PastDaysAsGrayed := CheckBox_PastDaysAsGrayed.Checked;
    AlterEvenOdd := CheckBox_AlterEvenOdd.Checked;
    ShowFocusRect := CheckBox_ShowFocusRect.Checked;
    ShowHeader := CheckBox_ShowHeader.Checked;
    ShowFooter := CheckBox_ShowFooter.Checked;
  end;
end;

procedure Tpsc_frm_setup_preferences.ComboboxPrefSave(
  APSCCalendar: TPSCCalendar);
begin
  with APSCCalendar do
  begin
    with ComboBox_FirstDayOfWeek do
    FirstDayOfWeek := TPSCFirstDayOfWeek(integer(Items.Objects[ItemIndex]));
    with ComboBox_WeekDayNames do
      PSCSetPropValue(APSCCalendar, 'WeekDayNames', Items[ItemIndex]);
    with ComboBox_SelectKind do
      PSCSetPropValue(APSCCalendar, 'SelectKind', Items[ItemIndex]);
    with ComboBox_FirstWeekOfYear do
      PSCSetPropValue(APSCCalendar, 'FirstWeekOfYear', Items[ItemIndex]);
    with ComboBox_HeaderCase do
      PSCSetPropValue(APSCCalendar, 'HeaderCase', Items[ItemIndex]);
    with ComboBox_PopupType do
      PSCSetPropValue(APSCCalendar, 'PopupType', Items[ItemIndex]);
    with ComboBox_ArrowStyle do
      PSCSetPropValue(APSCCalendar, 'ArrowStyle', Items[ItemIndex]);
    if PSCEdit_SelectionLength.Text <> '' then
      MaxSelDates := StrToInt(PSCEdit_SelectionLength.Text);
    with ComboBox_SelectStyle do
      PSCSetPropValue(APSCCalendar, 'SelectStyle', Items[ItemIndex]);
    with ComboBox_TodayStyle do
      PSCSetPropValue(APSCCalendar, 'TodayStyle', Items[ItemIndex]);
    with ComboBox_WeekDayCase do
      PSCSetPropValue(APSCCalendar, 'WeekDayCase', Items[ItemIndex]);
    with ComboBox_DayVertAlign do
      PSCSetPropValue(APSCCalendar, 'DayVertAlign', Items[ItemIndex]);
    with ComboBox_DayHorzAlign do
      PSCSetPropValue(APSCCalendar, 'DayHorzAlign', Items[ItemIndex]);
    with ComboBox_DayHeaderHorzAlign do
      PSCSetPropValue(APSCCalendar, 'DayHeaderHorzAlign', Items[ItemIndex]);
  end;
end;

procedure SetWorkDay(APSCCalendar : TPSCCalendar; AIsWorked : boolean; I : integer);
begin
  if AIsWorked then
    APSCCalendar.WorkDays := APSCCalendar.WorkDays + [TPSCFirstDayOfWeek(I)]
  else
    APSCCalendar.WorkDays := APSCCalendar.WorkDays - [TPSCFirstDayOfWeek(I)];
end;

procedure Tpsc_frm_setup_preferences.ComboBox_SelectKindChange(
  Sender: TObject);
begin
  with ComboBox_SelectKind do
    PSCSetPropValue(MYPSCPrefCalendar, 'SelectKind', Items[ItemIndex]);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.ComboBox_ArrowStyleChange(
  Sender: TObject);
begin
  with ComboBox_ArrowStyle do
    PSCSetPropValue(MYPSCPrefCalendar, 'ArrowStyle', Items[ItemIndex]);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.ComboBox_WeekCursorChange(
  Sender: TObject);
begin
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.ComboBox_FirstWeekOfYearChange(
  Sender: TObject);
begin
  with ComboBox_FirstWeekOfYear do
    PSCSetPropValue(MYPSCPrefCalendar, 'FirstWeekOfYear', Items[ItemIndex]);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.ComboBox_HeaderCaseChange(
  Sender: TObject);
begin
  with ComboBox_HeaderCase do
    PSCSetPropValue(MYPSCPrefCalendar, 'HeaderCase', Items[ItemIndex]);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.ComboBox_WeekDayNamesChange(
  Sender: TObject);
begin
  with ComboBox_WeekDayNames do
    PSCSetPropValue(MYPSCPrefCalendar, 'WeekDayNames', Items[ItemIndex]);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.PSCEdit_SelectionLengthChange(
  Sender: TObject);
begin
  if PSCEdit_SelectionLength.Text <> '' then
    MYPSCPrefCalendar.MaxSelDates := StrToInt(PSCEdit_SelectionLength.Text);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.ComboBox_PopupTypeChange(
  Sender: TObject);
begin
  with ComboBox_PopupType do
    PSCSetPropValue(MYPSCPrefCalendar, 'PopupType', Items[ItemIndex]);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_MultiSelectClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.MultiSelect := CheckBox_MultiSelect.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_KeyMovingClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.ClearOnKeyMoving := CheckBox_KeyMoving.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_ExtendedSelectClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.ExtendedSelect := CheckBox_ExtendedSelect.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_SideClickSelectsWeekClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.SideWeekSelect := CheckBox_SideClickSelectsWeek.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_ShowArrowEdgesClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.ShowArrowEdges := CheckBox_ShowArrowEdges.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_FlatClick(Sender: TObject);
begin
  MYPSCPrefCalendar.Flat := CheckBox_Flat.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_ShowTodayClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.ShowToday := CheckBox_ShowToday.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_NavigatuionButtonsClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.ShowNavButtons := CheckBox_NavigatuionButtons.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_NavButtonsTimerClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.NavButtonsTimer := CheckBox_NavButtonsTimer.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_MonthDividersClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.ShowMonthDividers := CheckBox_MonthDividers.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_ShowHorzLinesClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.ShowHorzLines := CheckBox_ShowHorzLines.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_ShowVertLinesClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.ShowVertLines := CheckBox_ShowVertLines.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_WeekNumbersClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.ShowWeekNumbers := CheckBox_WeekNumbers.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_ShowMonthPopupClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.ShowMonthPopup := CheckBox_ShowMonthPopup.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_ShortMonthClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.ShortMonthName := CheckBox_ShortMonth.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_ReadOnlyClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.ReadOnly := CheckBox_ReadOnly.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.SetWorkDays(APSCCalendar : TPSCCalendar; AIsWorked : boolean; ID : integer);
var
  i : integer;
begin
  i := ComboBox_FirstDayOfWeek.ItemIndex;
  if i = 0 then
    i := 2;
  SetWorkDay(APSCCalendar, AIsWorked, 1 + (i + ID) mod 7);
end;


procedure Tpsc_frm_setup_preferences.PSCDateEdit_MinDateChange(
  Sender: TObject);
begin
  MYPSCPrefCalendar.MinDate := PSCDateEdit_MinDate.Date;
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.PSCDateEdit_MaxDateChange(
  Sender: TObject);
begin
  MYPSCPrefCalendar.MaxDate := PSCDateEdit_MaxDate.Date;
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.ComboBox_SelectStyleChange(
  Sender: TObject);
begin
  with ComboBox_SelectStyle do
    PSCSetPropValue(MYPSCPrefCalendar, 'SelectStyle', Items[ItemIndex]);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.ComboBox_TodayStyleChange(
  Sender: TObject);
begin
  with ComboBox_TodayStyle do
    PSCSetPropValue(MYPSCPrefCalendar, 'TodayStyle', Items[ItemIndex]);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.ComboBox_WeekDayCaseChange(
  Sender: TObject);
begin
  with ComboBox_WeekDayCase do
    PSCSetPropValue(MYPSCPrefCalendar, 'WeekDayCase', Items[ItemIndex]);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.ComboBox_DayVertAlignChange(
  Sender: TObject);
begin
  with ComboBox_DayVertAlign do
    PSCSetPropValue(MYPSCPrefCalendar, 'DayVertAlign', Items[ItemIndex]);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.ComboBox_DayHorzAlignChange(
  Sender: TObject);
begin
  with ComboBox_DayHorzAlign do
    PSCSetPropValue(MYPSCPrefCalendar, 'DayHorzAlign', Items[ItemIndex]);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_WantEscClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.WantEsc := CheckBox_WantEsc.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_PastDaysAsGrayedClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.PastDaysAsGrayed := CheckBox_PastDaysAsGrayed.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_AlterEvenOddClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.AlterEvenOdd := CheckBox_AlterEvenOdd.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_ShowFocusRectClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.ShowFocusRect := CheckBox_ShowFocusRect.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_ShowHeaderClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.ShowHeader := CheckBox_ShowHeader.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.CheckBox_ShowFooterClick(
  Sender: TObject);
begin
  MYPSCPrefCalendar.ShowFooter := CheckBox_ShowFooter.Checked;
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.PSCEdit_CalendarsInHeightButtonClick(
  Sender: TObject; ABtnIndex: Integer);
var
  MyCalNumber : integer; 
begin
  MyCalNumber := StrToInt(PSCEdit_CalendarsInHeight.Text);
  if ABtnIndex = 0 then
    PSCEdit_CalendarsInHeight.Text := IntToStr(MyCalNumber + 1)
  else
  begin
    if MyCalNumber = 0 then
      exit;
    PSCEdit_CalendarsInHeight.Text := IntToStr(MyCalNumber - 1);
  end;  
end;

procedure Tpsc_frm_setup_preferences.PSCEditCalendarsInWidthButtonClick(
  Sender: TObject; ABtnIndex: Integer);
var
  MyCalNumber : integer;
begin
  MyCalNumber := StrToInt(PSCEditCalendarsInWidth.Text);
  if ABtnIndex = 0 then
    PSCEditCalendarsInWidth.Text := IntToStr(MyCalNumber + 1)
  else
  begin
    if MyCalNumber = 0 then
      exit;
    PSCEditCalendarsInWidth.Text := IntToStr(MyCalNumber - 1);
  end;
end;

procedure Tpsc_frm_setup_preferences.PSCEdit_CalendarsInHeightChange(
  Sender: TObject);
begin
  MYPSCPrefCalendar.CalendarsInHeight := StrToInt(PSCEdit_CalendarsInHeight.Text);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);

end;

procedure Tpsc_frm_setup_preferences.PSCEditCalendarsInWidthChange(
  Sender: TObject);
begin
  MYPSCPrefCalendar.CalendarsInWidth := StrToInt(PSCEditCalendarsInWidth.Text);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.ComboBox_DayHeaderHorzAlignChange(
  Sender: TObject);
begin
  with ComboBox_DayHeaderHorzAlign do
    PSCSetPropValue(MYPSCPrefCalendar, 'DayHeaderHorzAlign', Items[ItemIndex]);
  PSCCalendarPrefLoad(MYPSCPrefCalendar);
  MyPSCCalChange(Self);
end;

procedure Tpsc_frm_setup_preferences.PSCEdit_SelectionLengthKeyPress(
  Sender: TObject; var Key: Char);
begin
  If (Key>#31) and (not (Key in ['0'..'9'])) then
    Key:=#0;
end;

end.
