unit cal_frm_setup_addholiday;

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
  StdCtrls;

type
  Tpsc_frm_addholiday = class(TForm)
    Label_Name: TLabel;
    Edit_Name: TEdit;
    Button_AddOK: TButton;
    Button_AddCancel: TButton;
    ComboBox_Day: TComboBox;
    RadioButton_The: TRadioButton;
    RadioButton_Every: TRadioButton;
    Edit_Day: TEdit;
    Label1: TLabel;
    ComboBox_EveryMonth: TComboBox;
    ComboBox_WeekDay: TComboBox;
    ComboBox_Month: TComboBox;
    procedure Edit_NameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  psc_frm_addholiday: Tpsc_frm_addholiday;

implementation

{$R *.dfm}

procedure Tpsc_frm_addholiday.Edit_NameChange(Sender: TObject);
begin
  Button_AddOK.Enabled := (Edit_Name.Text <> '');
end;

procedure Tpsc_frm_addholiday.FormCreate(Sender: TObject);
var
  i : integer;
begin
  for i := 1 to 12 do
  begin
    ComboBox_Month.Items.Add(FormatSettings.LongMonthNames[i]);
    ComboBox_EveryMonth.Items.Add(FormatSettings.LongMonthNames[i]);
  end;
  for i := 1 to 7 do
    ComboBox_WeekDay.Items.Add(FormatSettings.LongDayNames[i]);
end;

end.
