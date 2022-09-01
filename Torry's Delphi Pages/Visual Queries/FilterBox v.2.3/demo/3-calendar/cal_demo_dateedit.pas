unit cal_demo_dateedit;

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
  cal_frm_setup_main,
  cal_demo_dateupdown,
  psc_wrapper,
  cal_demo_const,

  myla_interfaces,
  myla_system,

  psc_button_color,
  psc_edit_color,
  psc_edit,
  psc_edit_date,
  psc_procs,
  psc_fontbox;

type
  TPSCDateEditKind = (deDate, deDateTime, deTime);

  Tcal_demo_frm_datetime = class(TForm)
    Panel_DateTime_Main: TPanel;
    Panel_Buttons: TPanel;
    Panel_Manager: TPanel;
    Panel7: TPanel;
    Panel6: TPanel;
    Panel_ControlsCenter: TPanel;
    Label_Date: TLabel;
    PSCDateEdit_Date: TPSCDateEdit;
    Panel_ControlsTop: TPanel;
    PSCDateEdit_DateTime: TPSCDateEdit;
    Panel_ControlsBottom: TPanel;
    Label_DateTime: TLabel;
    CheckBox_ButtonsVisible: TCheckBox;
    CheckBox_Enabled: TCheckBox;
    CheckBox_ShowDateEdit: TCheckBox;
    CheckBox_MultiSelect: TCheckBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Label_ThemeName: TLabel;
    Label_Font: TLabel;
    SpeedButton_Bold: TSpeedButton;
    SpeedButton_Italic: TSpeedButton;
    ComboBox_EditThemeNames: TComboBox;
    PSCFontBar_DateEdit: TPSCFontEdit;
    ComboBox_FontSize: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox_ButtonsVisibleClick(Sender: TObject);
    procedure CheckBox_EnabledClick(Sender: TObject);
    procedure CheckBox_ShowDateEditClick(Sender: TObject);
    procedure CheckBox_MultiSelectClick(Sender: TObject);
    procedure PSCFontBar_DateEditChange(Sender: TObject);
    procedure ComboBox_FontSizeChange(Sender: TObject);
    procedure SpeedButton_ItalicClick(Sender: TObject);
    procedure SpeedButton_BoldClick(Sender: TObject);
    procedure ComboBox_EditThemeNamesChange(Sender: TObject);
  private
    procedure EditFontStyleChange(APSCDateEdit : TPSCDateEdit; AddNewStyle: boolean; AFontStyle: TFontStyle);
    procedure DateEditOptionsSetup(AOption : TPSCDateEditOption; AInOption : boolean);
    procedure InitControls;
    procedure SetControlsEnabled(AEnabled : boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  cal_demo_frm_datetime: Tcal_demo_frm_datetime;

implementation

{$R *.dfm}

procedure Tcal_demo_frm_datetime.FormCreate(Sender: TObject);
var
  S : TStrings;
begin
  S := TStringList.Create;
  try
    PSCGetThemeRegister.EnumThemeNames(PSCCreateStringsAdapter(S));
    ComboBox_EditThemeNames.Items.Assign(S);
  finally
    S.Free;
  end;
  InitControls;
//  RichEdit_DateEditDescription.Color := clInfoColor;
end;


procedure Tcal_demo_frm_datetime.CheckBox_ButtonsVisibleClick(
  Sender: TObject);
begin
  PSCDateEdit_DateTime.ButtonsVisible := CheckBox_ButtonsVisible.Checked;
  PSCDateEdit_Date.ButtonsVisible := CheckBox_ButtonsVisible.Checked;
end;



procedure Tcal_demo_frm_datetime.CheckBox_EnabledClick(Sender: TObject);
begin
  if PSCDateEdit_DateTime.Enabled = CheckBox_Enabled.Checked then
    exit;
    SetControlsEnabled(CheckBox_Enabled.Checked);
end;

procedure Tcal_demo_frm_datetime.CheckBox_ShowDateEditClick(
  Sender: TObject);
begin
  PSCDateEdit_DateTime.ShowDateEdit := CheckBox_ShowDateEdit.Checked;
  PSCDateEdit_Date.ShowDateEdit := CheckBox_ShowDateEdit.Checked;
end;

procedure Tcal_demo_frm_datetime.CheckBox_MultiSelectClick(
  Sender: TObject);
begin
  DateEditOptionsSetup(deoMultiSelect, CheckBox_MultiSelect.Checked);
end;

procedure Tcal_demo_frm_datetime.DateEditOptionsSetup(
  AOption: TPSCDateEditOption; AInOption: boolean);
begin
  if AInOption then
  begin
    PSCDateEdit_DateTime.Options := PSCDateEdit_DateTime.Options + [AOption];
    PSCDateEdit_Date.Options := PSCDateEdit_Date.Options + [AOption];
  end
  else
  begin
    PSCDateEdit_DateTime.Options := PSCDateEdit_DateTime.Options - [AOption];
    PSCDateEdit_Date.Options := PSCDateEdit_Date.Options - [AOption];
  end;
end;

procedure Tcal_demo_frm_datetime.PSCFontBar_DateEditChange(
  Sender: TObject);
begin
  PSCDateEdit_DateTime.Font.Name := PSCFontBar_DateEdit.FontName;
  PSCDateEdit_Date.Font.Name := PSCFontBar_DateEdit.FontName;
end;

procedure Tcal_demo_frm_datetime.ComboBox_FontSizeChange(Sender: TObject);
begin
  PSCDateEdit_DateTime.Font.Size := StrToInt(ComboBox_FontSize.Items[ComboBox_FontSize.ItemIndex]);
  PSCDateEdit_Date.Font.Size := StrToInt(ComboBox_FontSize.Items[ComboBox_FontSize.ItemIndex]);
  PSCDateEdit_DateTime.Top :=
   (Panel_ControlsTop.Height - PSCDateEdit_DateTime.Height) div 2;
  Label_DateTime.Top := PSCDateEdit_DateTime.Top - 20;
  PSCDateEdit_Date.Top :=
   (Panel_ControlsCenter.Height - PSCDateEdit_Date.Height) div 2;
  Label_Date.Top := PSCDateEdit_Date.Top - 20;
end;

procedure Tcal_demo_frm_datetime.SpeedButton_ItalicClick(Sender: TObject);
begin
  EditFontStyleChange(PSCDateEdit_DateTime, SpeedButton_Italic.Down, fsItalic);
  EditFontStyleChange(PSCDateEdit_Date, SpeedButton_Italic.Down, fsItalic);
end;

procedure Tcal_demo_frm_datetime.EditFontStyleChange(APSCDateEdit : TPSCDateEdit;
  AddNewStyle: boolean; AFontStyle: TFontStyle);
begin
  if AddNewStyle then
    APSCDateEdit.Font.Style := APSCDateEdit.Font.Style + [AFontStyle]
  else
    APSCDateEdit.Font.Style := APSCDateEdit.Font.Style - [AFontStyle];
end;

procedure Tcal_demo_frm_datetime.SpeedButton_BoldClick(Sender: TObject);
begin
  EditFontStyleChange(PSCDateEdit_DateTime, SpeedButton_Bold.Down, fsBold);
  EditFontStyleChange(PSCDateEdit_Date, SpeedButton_Bold.Down, fsBold);
end;

procedure Tcal_demo_frm_datetime.InitControls;
var
  i : integer;
  MyComboItems : IPSCStrings;
begin
  MyComboItems := PSCCreateStringList(ioOwned);
  CheckBox_ButtonsVisible.Checked := PSCDateEdit_DateTime.ButtonsVisible;
  CheckBox_Enabled.Checked := PSCDateEdit_DateTime.Enabled;
  CheckBox_ShowDateEdit.Checked := PSCDateEdit_DateTime.ShowDateEdit;
  CheckBox_MultiSelect.Checked := (deoMultiSelect in PSCDateEdit_DateTime.Options);
  for i := 1 to CPSCUsedFontSizesCount do
    ComboBox_FontSize.Items.Add(IntToStr(CPSCUsedFontSizes[i]));
  ComboBox_FontSize.ItemIndex := 0;
  for i := 0 to CPSCUsedFontSizesCount - 1 do
    if PSCDateEdit_DateTime.Font.Size = StrToInt(ComboBox_FontSize.Items[i]) then
      ComboBox_FontSize.ItemIndex := i;
  PSCFontBar_DateEdit.FontName := PSCDateEdit_DateTime.Font.Name;
  SpeedButton_Bold.Down := (fsBold in PSCDateEdit_DateTime.Font.Style);
  SpeedButton_Italic.Down := (fsItalic in PSCDateEdit_DateTime.Font.Style);
  with ComboBox_EditThemeNames do
  begin
    i := Items.IndexOf(PSCDateEdit_DateTime.ThemeName);
    if i < 0 then
      i := CPSCDefaultThemeNumber;
    ItemIndex := i;
  end;
end;

procedure Tcal_demo_frm_datetime.ComboBox_EditThemeNamesChange(
  Sender: TObject);
begin
  PSCDateEdit_DateTime.ThemeName :=
   ComboBox_EditThemeNames.Items[ComboBox_EditThemeNames.ItemIndex];
  PSCDateEdit_Date.ThemeName :=
   ComboBox_EditThemeNames.Items[ComboBox_EditThemeNames.ItemIndex];  
end;

procedure Tcal_demo_frm_datetime.SetControlsEnabled(AEnabled: boolean);
begin
  PSCDateEdit_DateTime.Enabled := AEnabled;
  PSCDateEdit_Date.Enabled := AEnabled;
end;

end.
