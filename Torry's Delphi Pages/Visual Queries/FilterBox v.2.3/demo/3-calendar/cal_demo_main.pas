unit cal_demo_main;

interface
{$I psc_defines.inc}

uses
{$IFDEF D7}
  XPMan,
{$ENDIF}
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
  ComCtrls,
  ExtCtrls,
  StdCtrls,

  psc_procs,
  psc_calendar,
  cal_frm_changefont;

type
  Tcal_demo_frm_main = class(TForm)
    PageControl_Main: TPageControl;
    TabSheet_Calendar: TTabSheet;
    TabSheet_DateEdit: TTabSheet;
    TabSheet_MonthBox: TTabSheet;
    TabSheet_CalPanel: TTabSheet;
    TabSheet_PSCEdit: TTabSheet;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
  public
  end;

var
  cal_demo_frm_main: Tcal_demo_frm_main;

const
  clInfoColor = $00CCFFFF;

implementation

uses
  cal_demo_calendar,
  cal_demo_calpanel,
  cal_demo_dateedit,
  cal_demo_monthbox,
  cal_demo_dateupdown;

{$R *.dfm}

procedure Tcal_demo_frm_main.FormShow(Sender: TObject);
begin
  cal_demo_frm_calendar.Panel_Calendar_Main.Parent := TabSheet_Calendar;
  cal_demo_frm_datetime.Panel_DateTime_Main.Parent := TabSheet_DateEdit;
  cal_demo_frm_monthbox.Panel_MonthBox_Main.Parent := TabSheet_MonthBox;
  cal_demo_frm_calpanel.Panel_CalPanel_Main.Parent := TabSheet_CalPanel;
  cal_demo_frm_pscdateupdown.Panel_PSCDateUpDown_Main.Parent := TabSheet_PSCEdit;
end;

procedure Tcal_demo_frm_main.FormCreate(Sender: TObject);
begin
  {$IFDEF D7}
  TXPManifest.Create(Self);
  {$ENDIF}
  PageControl_Main.ActivePageIndex := 0;
  KeyPreview := true;
end;

procedure Tcal_demo_frm_main.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssAlt in Shift) and (ssCtrl in Shift) and (Key = Ord('C')) then
  begin
    cal_frm_fontchange := Tcal_frm_fontchange.Create(Application);
    cal_frm_fontchange.ShowModal;
  end;
end;

end.
