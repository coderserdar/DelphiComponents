unit cal_demo_calpanel;

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
  ComCtrls,
  psc_procs,
  myla_interfaces,
  myla_system,
  psc_calendar,
  cal_frm_setup_main,
  Grids,
  psc_wrapper,
  psc_edit_date,
  cal_demo_const;

type
  Tcal_demo_frm_calpanel = class(TForm)
    Panel_CalPanel_Main: TPanel;
    Panel_Manager: TPanel;
    ComboBox_PanelKind: TComboBox;
    CheckBox_ShowDateEdit: TCheckBox;
    Panel_CalendarPanel: TPanel;
    Label_PanelKind: TLabel;
    Panel_Buttons: TPanel;
    PSCCalendarPanel1: TPSCCalendarPanel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    procedure ComboBox_PanelKindChange(Sender: TObject);
    procedure CheckBox_ShowDateEditClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  cal_demo_frm_calpanel: Tcal_demo_frm_calpanel;

implementation

{$R *.dfm}

procedure Tcal_demo_frm_calpanel.ComboBox_PanelKindChange(Sender: TObject);
begin
  with ComboBox_PanelKind do
    PSCSetPropValue(PSCCalendarPanel1, 'PanelKind', Items[ItemIndex]);
end;

procedure Tcal_demo_frm_calpanel.CheckBox_ShowDateEditClick(
  Sender: TObject);
begin
  PSCCalendarPanel1.ShowDateEdit := CheckBox_ShowDateEdit.Checked;
end;

procedure Tcal_demo_frm_calpanel.FormCreate(Sender: TObject);
var
  MyComboItems : IPSCStrings;
begin
  MyComboItems := PSCCreateStringList(ioOwned);
  PSCGetEnumTypeValueNames(TypeInfo(TPSCDateTimeKind),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_PanelKind.Items));
  ComboBox_PanelKind.ItemIndex := integer(PSCCalendarPanel1.PanelKind);
  CheckBox_ShowDateEdit.Checked := PSCCalendarPanel1.ShowDateEdit;
end;

end.
