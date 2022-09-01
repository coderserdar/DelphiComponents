unit cal_demo_calendar;

interface
{$I psc_defines.inc}

uses
{$IFDEF D6}
  Types,
{$ENDIF}
  Printers,
  ImgList,
  Controls,
  Dialogs,
  ExtCtrls,
  Graphics,
  StdCtrls,
  ComCtrls,
  Buttons,
  Windows,
  Messages,
  SysUtils,
{$IFDEF D6}
  Variants,
{$ENDIF}
  Classes,
  Forms,

  myla_interfaces,
  myla_system,

  psc_calendar,
  psc_procs,
  cal_frm_setup_main,
  cal_frm_changefont,
  ToolWin,
  psc_edit,
  psc_wrapper,
  cal_frm_print_setup,
  cal_demo_const;

type

  Tcal_demo_frm_calendar = class(TForm)
    Panel_Calendar_Main: TPanel;
    Panel_ToolBar: TPanel;
    Panel_Buttons: TPanel;
    SpeedButton_Setup: TSpeedButton;
    Label_CalendarStyle: TLabel;
    ComboBox_CalendarStyle: TComboBox;
    SpeedButton_PrintCalendar: TSpeedButton;
    Bevel1: TBevel;
    procedure Edit_SelectionLengthKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton_SetupClick(Sender: TObject);
    procedure ComboBox_CalendarStyleChange(Sender: TObject);
    procedure SpeedButton_PrintCalendarClick(Sender: TObject);
  private
    FCustomPrint : boolean;
    FMyCustomCalendar : TPSCCalendar;
    procedure SaveCalendar(APSCCalendar : TPSCCalendar);
    procedure CalendarsInit;
    procedure InitControls;
    { Private declarations }
  public
    procedure DoApply(Sender : TObject);
    property MyCustomCalendar : TPSCCalendar read FMyCustomCalendar write FMyCustomCalendar;
    { Public declarations }
  end;

var
  cal_demo_frm_calendar: Tcal_demo_frm_calendar;

implementation


{$R *.dfm}

procedure Tcal_demo_frm_calendar.Edit_SelectionLengthKeyPress(
  Sender: TObject; var Key: Char);
begin
  if (Ord(Key) < 48) or (Ord(Key) > 57) then
    Key := #0;
end;

procedure Tcal_demo_frm_calendar.DoApply(Sender : TObject);
begin
  SaveCalendar(MyCustomCalendar);
  ComboBox_CalendarStyle.ItemIndex := integer(MyCustomCalendar.CalendarStyle);
end;

procedure Tcal_demo_frm_calendar.FormCreate(Sender: TObject);
begin
  CalendarsInit;
  InitControls;
end;

procedure Tcal_demo_frm_calendar.SaveCalendar(APSCCalendar: TPSCCalendar);
begin
  APSCCalendar.BeginUpdate;
  try
    PSCCalendarSave(APSCCalendar);
  finally
    APSCCalendar.EndUpdate;
  end;
end;



procedure Tcal_demo_frm_calendar.InitControls;
var
  MyComboItems : IPSCStrings;
begin
  MyComboItems := PSCCreateStringList(ioOwned);
  PSCGetEnumTypeValueNames(TypeInfo(TPSCCalendarStyle),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_CalendarStyle.Items));
    ComboBox_CalendarStyle.ItemIndex := integer(MyCustomCalendar.CalendarStyle);
  FCustomPrint := false;
//  RichEdit_CalendarDescription.Color := clInfoColor;
end;


procedure Tcal_demo_frm_calendar.SpeedButton_SetupClick(Sender: TObject);
begin
  cal_frm_setup_main.psc_frm_setup_main := Tpsc_frm_setup_main.Create(Application);
  with cal_frm_setup_main.psc_frm_setup_main do
  begin
    MyPSCCalUpdate := DoApply;
    PSCCalendarLoad(MyCustomCalendar);
    ShowModal;
    if ModalResult = mrOk then
      DoApply(Self);
  end;
end;

procedure Tcal_demo_frm_calendar.CalendarsInit;
begin
  MyCustomCalendar := TPSCCalendar.Create(Self);
  with MyCustomCalendar do
  begin
    Parent := Panel_ToolBar;
    Align := alClient;
    CalendarStyle := cstOutlook;
    CalendarsInHeight := 0;
    CalendarsInWidth := 0;
  end;
end;

procedure Tcal_demo_frm_calendar.ComboBox_CalendarStyleChange(
  Sender: TObject);
begin
  PSCSetPropValue(MyCustomCalendar, 'CalendarStyle', ComboBox_CalendarStyle.Items[ComboBox_CalendarStyle.ItemIndex]);
end;

procedure Tcal_demo_frm_calendar.SpeedButton_PrintCalendarClick(Sender: TObject);
begin
  psc_frm_print_setup := Tpsc_frm_print_setup.Create(Application);
  AssignCalendar(psc_frm_print_setup.MyPrintCalendar, MyCustomCalendar);
  psc_frm_print_setup.ShowModal;
end;

end.








