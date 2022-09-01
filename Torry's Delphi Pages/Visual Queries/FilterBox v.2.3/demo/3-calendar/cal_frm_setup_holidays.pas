unit cal_frm_setup_holidays;

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
  Buttons,
  CheckLst,

  myla_interfaces,
  myla_system,

  psc_procs,
  psc_calendar,
  psc_holidays,

  cal_frm_setup_addholiday;

type
  Tpsc_frm_setup_holidays = class(TForm)
    Panel_Holidays: TPanel;
    CheckBox_DefaultHolidays: TCheckBox;
    CheckBox_ShowHolidays: TCheckBox;
    CheckBox_ShowHints: TCheckBox;
    GroupBox_CountryHolidays: TGroupBox;
    CheckListBox_Countries: TCheckListBox;
    Label_SelectLocations: TLabel;
    Button_AddCountry: TButton;
    Button_DeleteCountry: TButton;
    Button_EditCountry: TButton;
    GroupBox_CustomHolidays: TGroupBox;
    Button_Add: TButton;
    Button_Delete: TButton;
    Button_Edit: TButton;
    ListBox_CustomHolidays: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure Button_DeleteClick(Sender: TObject);
    procedure Button_AddClick(Sender: TObject);
    procedure Button_EditClick(Sender: TObject);
    procedure CheckBox_ShowHolidaysClick(Sender: TObject);
    procedure CheckBox_DefaultHolidaysClick(Sender: TObject);
    procedure CheckBox_ShowHintsClick(Sender: TObject);
    procedure CheckListBox_CountriesClickCheck(Sender: TObject);
  private
    FMyCustomHolidays : TPSCHolidayItems;
    FMYPSCHoliCalendar : TPSCCalendar;
    FMyPSCHoliCalChange : TNotifyEvent;
    FMyIPSCHolidays : IPSCStrings;
    procedure HolidaysSetup(IsEdit : boolean);
    procedure ButtonUpdate;
    procedure CheckBoxLoad(APSCCalendar : TPSCCalendar);
    procedure ListBoxLoad(APSCCalendar : TPSCCalendar);
    procedure CheckBoxSave(APSCCalendar : TPSCCalendar);
    procedure ListBoxSave(APSCCalendar : TPSCCalendar);

    { Private declarations }
  public
    procedure PSCCalendarHoliLoad(APSCCalendar : TPSCCalendar);
    procedure PSCCalendarHoliSave(APSCCalendar: TPSCCalendar);
    property MyCustomHolidays : TPSCHolidayItems read FMyCustomHolidays write FMyCustomHolidays;
    property MYPSCHoliCalendar : TPSCCalendar read FMYPSCHoliCalendar write FMYPSCHoliCalendar;
    property MyPSCHoliCalChange : TNotifyEvent read FMyPSCHoliCalChange write FMyPSCHoliCalChange;
    { Public declarations }
  end;

var
  psc_frm_setup_holidays: Tpsc_frm_setup_holidays;

implementation

{$R *.dfm}

procedure Tpsc_frm_setup_holidays.FormCreate(Sender: TObject);
begin
  FMyIPSCHolidays := PSCCreateStringList(ioOwned);
  PSCSetFormFont(Self);
  MyCustomHolidays := TPSCHolidayItems.Create(nil, TPSCHolidayItem);
  MYPSCHoliCalendar := TPSCCalendar.Create(Self);
  MYPSCHoliCalendar.Visible := false;
  PSCEnumCountriesWithHolidays(FMyIPSCHolidays);
  FMyIPSCHolidays.AssignTo(PSCCreateStringsAdapter(CheckListBox_Countries.Items));
  FMyIPSCHolidays.Clear;
end;

procedure Tpsc_frm_setup_holidays.Button_DeleteClick(Sender: TObject);
begin
  MyCustomHolidays.Items[ListBox_CustomHolidays.ItemIndex].Destroy;
  ListBox_CustomHolidays.Items.Delete(ListBox_CustomHolidays.ItemIndex);
  ButtonUpdate;
end;

procedure Tpsc_frm_setup_holidays.Button_AddClick(Sender: TObject);
begin
  HolidaysSetup(false);
  ButtonUpdate;
end;

procedure Tpsc_frm_setup_holidays.Button_EditClick(Sender: TObject);
begin
  HolidaysSetup(true);
  ButtonUpdate;
end;

procedure Tpsc_frm_setup_holidays.HolidaysSetup(IsEdit : boolean);
var
  i : integer;
begin
  psc_frm_AddHoliday := Tpsc_frm_AddHoliday.Create(Application);
  with psc_frm_AddHoliday do
  begin
    if IsEdit then
    begin
      psc_frm_AddHoliday.Caption := 'Edit Holiday';
      i := ListBox_CustomHolidays.ItemIndex;
      if i < 0 then
        i := 0;
      with MyCustomHolidays.Holidays[i] do
      begin
        case KindOfDay of
          dckDate :
          begin
            RadioButton_Every.Checked := true;
            ComboBox_EveryMonth.ItemIndex := 0;
            Edit_Day.Text := IntToStr(Day);
            ComboBox_Day.ItemIndex := 0;
            ComboBox_WeekDay.ItemIndex := 1;
            ComboBox_EveryMonth.ItemIndex := integer(Month) - 1;
            ComboBox_Month.ItemIndex := 0;            
          end;
          dckDayNumber :
          begin
            RadioButton_The.Checked := true;
            if (Day <= 4) and (Day > 0) then
              ComboBox_Day.ItemIndex := Day - 1
            else
              ComboBox_Day.ItemIndex := 4;
            ComboBox_WeekDay.ItemIndex := integer(WeekDay);
            ComboBox_EveryMonth.ItemIndex := 0;
            ComboBox_Month.ItemIndex := integer(Month) - 1;
            Edit_Day.Text := '1';
          end;
        end;
        Edit_Name.Text := Name;
      end;
    end
    else
    begin
      psc_frm_AddHoliday.Caption := 'Add Holiday';
      RadioButton_Every.Checked := true;
      ComboBox_EveryMonth.ItemIndex := 0;
      Edit_Day.Text := IntToStr(1);
      ComboBox_Day.ItemIndex := 0;
      ComboBox_WeekDay.ItemIndex := 1;
      ComboBox_Month.ItemIndex := 0;
    end;
    if Edit_Day.Text = '' then
      Edit_Day.Text := '0';
    ShowModal;
    if ModalResult = mrOK then
    begin
    if IsEdit then
      MyCustomHolidays.Delete(ListBox_CustomHolidays.ItemIndex);
    MyCustomHolidays.Add;
    with MyCustomHolidays.Holidays[MyCustomHolidays.Count - 1] do
    begin
      Name := Edit_Name.Text;
      if RadioButton_Every.Checked then
      begin
        KindOfDay := dckDate;
        if (StrToInt(Edit_Day.Text) > 31) or (StrToInt(Edit_Day.Text) < -1) then
          Day := TPSCDay(-1)
        else
          Day := TPSCDay(StrToInt(Edit_Day.Text));
        Month := TPSCMonth(ComboBox_EveryMonth.ItemIndex + 1);
      end
      else
      begin
        KindOfDay := dckDayNumber;
        Month := TPSCMonth(ComboBox_Month.ItemIndex + 1);
        WeekDay := TPSCFirstDayOfWeek(ComboBox_WeekDay.ItemIndex + 1);
        Day := TPSCDay(ComboBox_Day.ItemIndex + 1);
      end;
    end;
      if IsEdit then
      begin
        ListBox_CustomHolidays.Items.Delete(ListBox_CustomHolidays.ItemIndex);
        ListBox_CustomHolidays.Items.Add(Edit_Name.Text);
      end
      else
        ListBox_CustomHolidays.Items.Add(Edit_Name.Text);
    end;
  end;
end;

procedure Tpsc_frm_setup_holidays.ButtonUpdate;
begin
  MYPSCHoliCalendar.CustomHolidays.Assign(MyCustomHolidays);
  Button_Delete.Enabled := (ListBox_CustomHolidays.Items.Count > 0);
  Button_Edit.Enabled := (ListBox_CustomHolidays.Items.Count > 0);
  if ListBox_CustomHolidays.ItemIndex < 0 then
    ListBox_CustomHolidays.ItemIndex := 0;
  MyPSCHoliCalChange(Self);
end;

procedure Tpsc_frm_setup_holidays.CheckBox_ShowHolidaysClick(
  Sender: TObject);
begin
  MYPSCHoliCalendar.ShowHolidays := CheckBox_ShowHolidays.Checked;
  PSCCalendarHoliLoad(MYPSCHoliCalendar);
  MyPSCHoliCalChange(Self);
end;

procedure Tpsc_frm_setup_holidays.PSCCalendarHoliLoad(APSCCalendar : TPSCCalendar);
begin
  with psc_frm_setup_holidays do
  begin
    CheckBoxLoad(APSCCalendar);
    ListBoxLoad(APSCCalendar);
  end;
end;

procedure Tpsc_frm_setup_holidays.PSCCalendarHoliSave(APSCCalendar : TPSCCalendar);
begin
  with psc_frm_setup_holidays do
  begin
    CheckBoxSave(APSCCalendar);
    ListBoxSave(APSCCalendar);
    CheckBoxSave(APSCCalendar);
  end;
end;

procedure Tpsc_frm_setup_holidays.CheckBoxLoad(APSCCalendar: TPSCCalendar);
begin
  with APSCCalendar do
  begin
    CheckBox_ShowHints.Checked := ShowHint;
    CheckBox_ShowHolidays.Checked := ShowHolidays;
    CheckBox_DefaultHolidays.Checked := DefaultHolidays;
  end;
end;

procedure Tpsc_frm_setup_holidays.ListBoxLoad(APSCCalendar: TPSCCalendar);
var
  i, j : integer;
begin
  with APSCCalendar do
  begin
    EnumHolidayCountries(FMyIPSCHolidays);
    for i := 0 to FMyIPSCHolidays.Count - 1 do
    begin
      j := CheckListBox_Countries.Items.IndexOf(FMyIPSCHolidays.Strings[i]);
      if j >= 0 then
        CheckListBox_Countries.Checked[j] := true;
    end;
    ListBox_CustomHolidays.Clear;
    for i := 0 to CustomHolidays.Count - 1 do
      ListBox_CustomHolidays.Items.Add(CustomHolidays.Holidays[i].Name);
    Button_Delete.Enabled := (CustomHolidays.Count > 0);
    Button_Edit.Enabled := (CustomHolidays.Count > 0);    
    MyCustomHolidays.Assign(CustomHolidays);
    ListBox_CustomHolidays.ItemIndex := 0;
  end;
end;

procedure Tpsc_frm_setup_holidays.CheckBoxSave(APSCCalendar: TPSCCalendar);
begin
  with APSCCalendar do
  begin
    ShowHolidays := CheckBox_ShowHolidays.Checked;
    DefaultHolidays := CheckBox_DefaultHolidays.Checked;
    ShowHint := CheckBox_ShowHints.Checked;
  end;
end;

procedure Tpsc_frm_setup_holidays.ListBoxSave(APSCCalendar: TPSCCalendar);
var
  i : integer;
begin
  with APSCCalendar do
  begin
    FMyIPSCHolidays.Clear;
    for i := 0 to CheckListBox_Countries.Items.Count - 1 do
      if CheckListBox_Countries.Checked[i] then
        FMyIPSCHolidays.Add(CheckListBox_Countries.Items[i]);
    SetHolidayCountries(FMyIPSCHolidays);
    CustomHolidays.Clear;
    for i := 0 to ListBox_CustomHolidays.Items.Count - 1 do
    begin
      CustomHolidays.Add;
      CustomHolidays.Assign(MyCustomHolidays);
    end;
  end;
end;

procedure Tpsc_frm_setup_holidays.CheckBox_DefaultHolidaysClick(
  Sender: TObject);
begin
  MYPSCHoliCalendar.DefaultHolidays := CheckBox_DefaultHolidays.Checked;
  PSCCalendarHoliLoad(MYPSCHoliCalendar);  
  MyPSCHoliCalChange(Self);
end;

procedure Tpsc_frm_setup_holidays.CheckBox_ShowHintsClick(Sender: TObject);
begin
  MYPSCHoliCalendar.ShowHint := CheckBox_ShowHints.Checked;
  PSCCalendarHoliLoad(MYPSCHoliCalendar);
  MyPSCHoliCalChange(Self);
end;

procedure Tpsc_frm_setup_holidays.CheckListBox_CountriesClickCheck(
  Sender: TObject);
var
  i : integer;
begin
  FMyIPSCHolidays.Clear;
  for i := 0 to CheckListBox_Countries.Items.Count - 1 do
    if CheckListBox_Countries.Checked[i] then
      FMyIPSCHolidays.Add(CheckListBox_Countries.Items[i]);
  MYPSCHoliCalendar.SetHolidayCountries(FMyIPSCHolidays);
  PSCCalendarHoliLoad(MYPSCHoliCalendar);
  MyPSCHoliCalChange(Self);
end;


end.
