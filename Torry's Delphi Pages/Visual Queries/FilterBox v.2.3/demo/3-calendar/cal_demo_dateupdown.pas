unit cal_demo_dateupdown;

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
  StdCtrls,

  myla_interfaces,
  myla_system,

  psc_edit,
  psc_procs,
  psc_fontbox,
  psc_edit_color,
  Buttons,
  psc_button_color,
  ComCtrls,
  ExtCtrls,
  psc_parser_date,
  psc_edit_parts,
  psc_wrapper,
  psc_edit_date,
  cal_demo_const;

type
  Tcal_demo_frm_pscdateupdown = class(TForm)
    Panel_PSCDateUpDown_Main: TPanel;
    Panel_Manager: TPanel;
    Panel1: TPanel;
    Panel_Controls: TPanel;
    Panel_ControlsTop: TPanel;
    Panel_ControlsCenter: TPanel;
    Panel_ControlsBottom: TPanel;
    PSCDateTimeUpDown_DateTime: TPSCDateTimeUpDown;
    PSCDateTimeUpDown_Date: TPSCDateTimeUpDown;
    PSCDateTimeUpDown_Time: TPSCDateTimeUpDown;
    Panel_Buttons: TPanel;
    Panel3: TPanel;
    Label_ThemeName: TLabel;
    Label_Font: TLabel;
    SpeedButton_Bold: TSpeedButton;
    SpeedButton_Italic: TSpeedButton;
    Bevel3: TBevel;
    Label_Parts: TLabel;
    CheckBox_ButtonsVisible: TCheckBox;
    ComboBox_EditThemeNames: TComboBox;
    CheckBox_Enabled: TCheckBox;
    PSCFontBar_Edit: TPSCFontEdit;
    ComboBox_FontSize: TComboBox;
    CheckBox_FlatCheckBoxes: TCheckBox;
    CheckBox_ShowCheckBoxes: TCheckBox;
    ComboBox_Parts: TComboBox;
    CheckBox_PartEnabled: TCheckBox;
    CheckBox_PartChecked: TCheckBox;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    CheckBox_LongDateFormat: TCheckBox;
    CheckBox_LongTimeFormat: TCheckBox;
    Label_CurrentSelDateTime: TLabel;
    Label_CurrentSelDate: TLabel;
    Label_CurrentSelTime: TLabel;
    Label_DateTime: TLabel;
    Label_Date: TLabel;
    Label1: TLabel;
    CheckBox_PartVisible: TCheckBox;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox_PartsChange(Sender: TObject);
    procedure CheckBox_PartEnabledClick(Sender: TObject);
    procedure CheckBox_PartCheckedClick(Sender: TObject);
    procedure ComboBox_EditThemeNamesChange(Sender: TObject);
    procedure CheckBox_ButtonsVisibleClick(Sender: TObject);
    procedure CheckBox_EnabledClick(Sender: TObject);
    procedure CheckBox_FlatCheckBoxesClick(Sender: TObject);
    procedure CheckBox_ShowCheckBoxesClick(Sender: TObject);
    procedure PSCFontBar_EditChange(Sender: TObject);
    procedure ComboBox_FontSizeChange(Sender: TObject);
    procedure SpeedButton_BoldClick(Sender: TObject);
    procedure SpeedButton_ItalicClick(Sender: TObject);
    procedure CheckBox_LongDateFormatClick(Sender: TObject);
    procedure CheckBox_LongTimeFormatClick(Sender: TObject);
    procedure PSCDateTimeUpDown_DateTimeChange(Sender: TObject);
    procedure PSCDateTimeUpDown_DateChange(Sender: TObject);
    procedure PSCDateTimeUpDown_TimeChange(Sender: TObject);
    procedure CheckBox_PartVisibleClick(Sender: TObject);
  private
    { Private declarations }
    FMyDateTimeChangesCount : integer;
    FMyDateChangesCount : integer;
    FMyTimeChangesCount : integer;
    procedure UpdateControls;
    procedure SetPartsChecked_Enabled;
    procedure SetDateTimeFormatControls;
    procedure SetLabelCaption(ALabel :TLabel; ADateTimeUpDown : TPSCDateTimeUpDown;
     AChangesCount : integer = 0);
    procedure EnableControls(AEnabled : boolean);     
  public
    { Public declarations }
  end;

function GetDateTimeAndChangesCount(Sender : TObject; AChangesCount : integer) : string;

var
  cal_demo_frm_pscdateupdown: Tcal_demo_frm_pscdateupdown;

implementation

{$R *.dfm}

procedure Tcal_demo_frm_pscdateupdown.FormCreate(Sender: TObject);
var
  S : TStrings;
  i : integer;
  MyComboItems : IPSCStrings;
begin
  FMyDateTimeChangesCount := 0;
  FMyDateChangesCount := 0;
  FMyTimeChangesCount := 0;
  MyComboItems := PSCCreateStringList(ioOwned);
  PSCGetEnumTypeValueNames(TypeInfo(TPSCDateTimePart),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_Parts.Items));
  ComboBox_Parts.ItemIndex := 0;
  for i := 1 to CPSCUsedFontSizesCount do
    ComboBox_FontSize.Items.Add(IntToStr(CPSCUsedFontSizes[i]));
//  RichEdit_DateTimeUpDownDescription.Color := clInfoColor;
  S := TStringList.Create;
  try
    PSCGetThemeRegister.EnumThemeNames(PSCCreateStringsAdapter(S));
    ComboBox_EditThemeNames.Items.Assign(S);
  finally
    S.Free;
  end;
  UpdateControls;
end;

procedure Tcal_demo_frm_pscdateupdown.UpdateControls;
Var
  i : integer;
begin
  CheckBox_ButtonsVisible.Checked := PSCDateTimeUpDown_DateTime.ButtonsVisible;
  CheckBox_Enabled.Checked := PSCDateTimeUpDown_DateTime.Enabled;
  CheckBox_FlatCheckBoxes.Checked := PSCDateTimeUpDown_DateTime.FlatCheckBoxes;
  CheckBox_ShowCheckBoxes.Checked := PSCDateTimeUpDown_DateTime.ShowCheckBoxes;
  PSCFontBar_Edit.FontName := PSCDateTimeUpDown_DateTime.Font.Name;
  ComboBox_FontSize.ItemIndex := 0;
  for i := 0 to CPSCUsedFontSizesCount - 1 do
    if PSCDateTimeUpDown_DateTime.Font.Size = StrToInt(ComboBox_FontSize.Items[i]) then
      ComboBox_FontSize.ItemIndex := i;
  with ComboBox_EditThemeNames do
  begin
    i := Items.IndexOf(PSCDateTimeUpDown_DateTime.ThemeName);
    if i < 0 then
      i := CPSCDefaultThemeNumber;
    ItemIndex := i;
  end;
  SpeedButton_Bold.Down := (fsBold in PSCDateTimeUpDown_DateTime.Font.Style);
  SpeedButton_Italic.Down := (fsItalic in PSCDateTimeUpDown_DateTime.Font.Style);
  SetPartsChecked_Enabled;
  SetDateTimeFormatControls;
end;

procedure Tcal_demo_frm_pscdateupdown.ComboBox_PartsChange(
  Sender: TObject);
begin
  SetPartsChecked_Enabled;
end;


procedure Tcal_demo_frm_pscdateupdown.SetPartsChecked_Enabled;
var
  MyDateTimePart : TPSCDateTimePart;
begin
  MyDateTimePart := TPSCDateTimePart(ComboBox_Parts.ItemIndex);
  if MyDateTimePart in PSCDateTimeUpDown_DateTime.PartsEnabled then
    CheckBox_PartEnabled.Checked := true
  else
    CheckBox_PartEnabled.Checked := false;
  if MyDateTimePart in PSCDateTimeUpDown_DateTime.PartsChecked then
    CheckBox_PartChecked.Checked := true
  else
    CheckBox_PartChecked.Checked := false;
  if MyDateTimePart in PSCDateTimeUpDown_DateTime.PartsCheckVisible then
    CheckBox_PartVisible.Checked := true
  else
    CheckBox_PartVisible.Checked := false;
end;

procedure Tcal_demo_frm_pscdateupdown.CheckBox_PartEnabledClick(
  Sender: TObject);
var
  MyDateTimePart : TPSCDateTimePart;
  MyLeavePart : boolean;

  procedure SetPartEnabled(ADateTimeUpDown : TPSCDateTimeUpDown; ALeave : boolean;
   APart : TPSCDateTimePart);
  begin
    if ALeave then
      ADateTimeUpDown.PartsEnabled := ADateTimeUpDown.PartsEnabled - [APart]
    else
      ADateTimeUpDown.PartsEnabled := ADateTimeUpDown.PartsEnabled + [APart];
  end;

begin
  MyDateTimePart := TPSCDateTimePart(ComboBox_Parts.ItemIndex);
  MyLeavePart := not CheckBox_PartEnabled.Checked;
  SetPartEnabled(PSCDateTimeUpDown_DateTime, MyLeavePart, MyDateTimePart);
  SetPartEnabled(PSCDateTimeUpDown_Date, MyLeavePart, MyDateTimePart);
  SetPartEnabled(PSCDateTimeUpDown_Time, MyLeavePart, MyDateTimePart);
end;

procedure Tcal_demo_frm_pscdateupdown.CheckBox_PartCheckedClick(
  Sender: TObject);
var
  MyDateTimePart : TPSCDateTimePart;

  procedure SetPartChecked(ADateTimeUpDown : TPSCDateTimeUpDown; ALeave : boolean;
   APart : TPSCDateTimePart);
  begin
    if ALeave then
      ADateTimeUpDown.PartsChecked := ADateTimeUpDown.PartsChecked - [APart]
    else
      ADateTimeUpDown.PartsChecked := ADateTimeUpDown.PartsChecked + [APart];
  end;

begin
  MyDateTimePart := TPSCDateTimePart(ComboBox_Parts.ItemIndex);
  SetPartChecked(PSCDateTimeUpDown_DateTime, not CheckBox_PartChecked.Checked, MyDateTimePart);
  SetPartChecked(PSCDateTimeUpDown_Date, not CheckBox_PartChecked.Checked, MyDateTimePart);
  SetPartChecked(PSCDateTimeUpDown_Time, not CheckBox_PartChecked.Checked, MyDateTimePart);
end;

procedure Tcal_demo_frm_pscdateupdown.SetDateTimeFormatControls;
begin
  if PSCDateTimeUpDown_Date.DateTimeFormat.DateKind = dfkLong then
    CheckBox_LongDateFormat.Checked := true;
  if PSCDateTimeUpDown_Date.DateTimeFormat.TimeKind = dfkLong then
    CheckBox_LongTimeFormat.Checked := true;
end;

procedure Tcal_demo_frm_pscdateupdown.ComboBox_EditThemeNamesChange(
  Sender: TObject);
begin
  PSCDateTimeUpDown_DateTime.ThemeName :=
     ComboBox_EditThemeNames.Items[ComboBox_EditThemeNames.ItemIndex];
  PSCDateTimeUpDown_Date.ThemeName :=
     ComboBox_EditThemeNames.Items[ComboBox_EditThemeNames.ItemIndex];
  PSCDateTimeUpDown_Time.ThemeName :=
     ComboBox_EditThemeNames.Items[ComboBox_EditThemeNames.ItemIndex];
end;

procedure Tcal_demo_frm_pscdateupdown.CheckBox_ButtonsVisibleClick(
  Sender: TObject);
begin
  PSCDateTimeUpDown_DateTime.ButtonsVisible := CheckBox_ButtonsVisible.Checked;
  PSCDateTimeUpDown_Date.ButtonsVisible := CheckBox_ButtonsVisible.Checked;
  PSCDateTimeUpDown_Time.ButtonsVisible := CheckBox_ButtonsVisible.Checked;
end;

procedure Tcal_demo_frm_pscdateupdown.CheckBox_EnabledClick(
  Sender: TObject);
begin
  EnableControls(CheckBox_Enabled.Checked);
end;

procedure Tcal_demo_frm_pscdateupdown.CheckBox_FlatCheckBoxesClick(
  Sender: TObject);
begin
  PSCDateTimeUpDown_DateTime.FlatCheckBoxes := CheckBox_FlatCheckBoxes.Checked;
  PSCDateTimeUpDown_Date.FlatCheckBoxes := CheckBox_FlatCheckBoxes.Checked;
  PSCDateTimeUpDown_Time.FlatCheckBoxes := CheckBox_FlatCheckBoxes.Checked;
end;

procedure Tcal_demo_frm_pscdateupdown.CheckBox_ShowCheckBoxesClick(
  Sender: TObject);
begin
  PSCDateTimeUpDown_DateTime.ShowCheckBoxes := CheckBox_ShowCheckBoxes.Checked;
  PSCDateTimeUpDown_Date.ShowCheckBoxes := CheckBox_ShowCheckBoxes.Checked;
  PSCDateTimeUpDown_Time.ShowCheckBoxes := CheckBox_ShowCheckBoxes.Checked;
end;

procedure Tcal_demo_frm_pscdateupdown.PSCFontBar_EditChange(
  Sender: TObject);
begin
  PSCDateTimeUpDown_DateTime.Font.Name := PSCFontBar_Edit.FontName;
  PSCDateTimeUpDown_Date.Font.Name := PSCFontBar_Edit.FontName;
  PSCDateTimeUpDown_Time.Font.Name := PSCFontBar_Edit.FontName;
end;

procedure Tcal_demo_frm_pscdateupdown.ComboBox_FontSizeChange(
  Sender: TObject);
begin
  PSCDateTimeUpDown_DateTime.Font.Size := CPSCUsedFontSizes[ComboBox_FontSize.ItemIndex + 1];
  PSCDateTimeUpDown_Date.Font.Size := CPSCUsedFontSizes[ComboBox_FontSize.ItemIndex + 1];
  PSCDateTimeUpDown_Time.Font.Size := CPSCUsedFontSizes[ComboBox_FontSize.ItemIndex + 1];
  PSCDateTimeUpDown_DateTime.Top :=
   (Panel_ControlsTop.Height - PSCDateTimeUpDown_DateTime.Height) div 2;
  Label_CurrentSelDateTime.Top :=
   PSCDateTimeUpDown_DateTime.Top + PSCDateTimeUpDown_DateTime.Height + 7;
  PSCDateTimeUpDown_Date.Top :=
   (Panel_ControlsCenter.Height - PSCDateTimeUpDown_Date.Height) div 2;
  Label_CurrentSelDate.Top :=
   PSCDateTimeUpDown_Date.Top + PSCDateTimeUpDown_Date.Height + 7;
  PSCDateTimeUpDown_Time.Top :=
   (Panel_ControlsBottom.Height - PSCDateTimeUpDown_Time.Height) div 2;
  Label_CurrentSelTime.Top :=
   PSCDateTimeUpDown_Time.Top + PSCDateTimeUpDown_Time.Height + 7;
end;

procedure Tcal_demo_frm_pscdateupdown.SpeedButton_BoldClick(
  Sender: TObject);

  procedure SetBoldInStyle(ADateTimeUpDown : TPSCDateTimeUpDown; ALeave : boolean);
  begin
    if ALeave then
      ADateTimeUpDown.Font.Style := ADateTimeUpDown.Font.Style - [FontStyle_Bold]
    else
      ADateTimeUpDown.Font.Style := ADateTimeUpDown.Font.Style + [FontStyle_Bold];
  end;

begin
  SetBoldInStyle(PSCDateTimeUpDown_DateTime, not SpeedButton_Bold.Down);
  SetBoldInStyle(PSCDateTimeUpDown_Date, not SpeedButton_Bold.Down);
  SetBoldInStyle(PSCDateTimeUpDown_Time, not SpeedButton_Bold.Down);
end;

procedure Tcal_demo_frm_pscdateupdown.SpeedButton_ItalicClick(
  Sender: TObject);

  procedure SetItalicInStyle(ADateTimeUpDown : TPSCDateTimeUpDown; ALeave : boolean);
  begin
    if ALeave then
      ADateTimeUpDown.Font.Style := ADateTimeUpDown.Font.Style - [FontStyle_Italic]
    else
      ADateTimeUpDown.Font.Style := ADateTimeUpDown.Font.Style + [FontStyle_Italic];
  end;

begin
  SetItalicInStyle(PSCDateTimeUpDown_DateTime, not SpeedButton_Italic.Down);
  SetItalicInStyle(PSCDateTimeUpDown_Date, not SpeedButton_Italic.Down);
  SetItalicInStyle(PSCDateTimeUpDown_Time, not SpeedButton_Italic.Down);
end;

procedure Tcal_demo_frm_pscdateupdown.CheckBox_LongDateFormatClick(
  Sender: TObject);

  procedure SetDateKind(ADateTimeUpDown : TPSCDateTimeUpDown; ALong : boolean);
  begin
    if ALong then
      ADateTimeUpDown.DateTimeFormat.DateKind := dfkLong
    else
      ADateTimeUpDown.DateTimeFormat.DateKind := dfkShort;
  end;

begin
  SetDateKind(PSCDateTimeUpDown_DateTime, CheckBox_LongDateFormat.Checked);
  SetDateKind(PSCDateTimeUpDown_Date, CheckBox_LongDateFormat.Checked);
  SetDateKind(PSCDateTimeUpDown_Time, CheckBox_LongDateFormat.Checked);
end;

procedure Tcal_demo_frm_pscdateupdown.CheckBox_LongTimeFormatClick(
  Sender: TObject);

  procedure SetTimeKind(ADateTimeUpDown : TPSCDateTimeUpDown; ALong : boolean);
  begin
    if ALong then
      ADateTimeUpDown.DateTimeFormat.TimeKind := dfkLong
    else
      ADateTimeUpDown.DateTimeFormat.TimeKind := dfkShort;
  end;

begin
  SetTimeKind(PSCDateTimeUpDown_DateTime, CheckBox_LongTimeFormat.Checked);
  SetTimeKind(PSCDateTimeUpDown_Date, CheckBox_LongTimeFormat.Checked);
  SetTimeKind(PSCDateTimeUpDown_Time, CheckBox_LongTimeFormat.Checked);
end;

procedure Tcal_demo_frm_pscdateupdown.PSCDateTimeUpDown_DateTimeChange(
  Sender: TObject);
begin
  SetPartsChecked_Enabled;
  Inc(FMyDateTimeChangesCount);
  SetLabelCaption(Label_CurrentSelDateTime, PSCDateTimeUpDown_DateTime, FMyDateTimeChangesCount);
end;

procedure Tcal_demo_frm_pscdateupdown.PSCDateTimeUpDown_DateChange(
  Sender: TObject);
begin
  SetPartsChecked_Enabled;
  Inc(FMyDateChangesCount);
  SetLabelCaption(Label_CurrentSelDate, PSCDateTimeUpDown_Date, FMyDateChangesCount);
end;

procedure Tcal_demo_frm_pscdateupdown.PSCDateTimeUpDown_TimeChange(
  Sender: TObject);
begin
  SetPartsChecked_Enabled;
  Inc(FMyTimeChangesCount);
  SetLabelCaption(Label_CurrentSelTime, PSCDateTimeUpDown_Time, FMyTimeChangesCount);
end;

function GetDateTimeAndChangesCount(Sender : TObject; AChangesCount : integer) : string;
begin
  result := '';
{$IFDEF TEST}
  result := DateTimeToStr((Sender as TPSCDateTimeUpDown).DateTime) + ' ' +
   IntToStr(AChangesCount);
{$ENDIF}
end;

procedure Tcal_demo_frm_pscdateupdown.SetLabelCaption(ALabel :TLabel;
 ADateTimeUpDown : TPSCDateTimeUpDown; AChangesCount : integer = 0);
begin
  ALabel.Caption := GetDateTimeAndChangesCount(ADateTimeUpDown, AChangesCount);
end;

procedure Tcal_demo_frm_pscdateupdown.EnableControls(AEnabled: boolean);
begin
  PSCDateTimeUpDown_DateTime.Enabled := AEnabled;
  PSCDateTimeUpDown_Date.Enabled := AEnabled;
  PSCDateTimeUpDown_Time.Enabled := AEnabled;
end;

procedure Tcal_demo_frm_pscdateupdown.CheckBox_PartVisibleClick(
  Sender: TObject);
var
  MyDateTimePart : TPSCDateTimePart;
  MyLeavePart : boolean;

  procedure SetPart(ADateTimeUpDown : TPSCDateTimeUpDown; ALeave : boolean;
   APart : TPSCDateTimePart);
  begin
    if ALeave then
      ADateTimeUpDown.PartsCheckVisible := ADateTimeUpDown.PartsCheckVisible - [APart]
    else
      ADateTimeUpDown.PartsCheckVisible := ADateTimeUpDown.PartsCheckVisible + [APart];
  end;

begin
  MyDateTimePart := TPSCDateTimePart(ComboBox_Parts.ItemIndex);
  MyLeavePart := not CheckBox_PartVisible.Checked;
  SetPart(PSCDateTimeUpDown_DateTime, MyLeavePart, MyDateTimePart);
  SetPart(PSCDateTimeUpDown_Date, MyLeavePart, MyDateTimePart);
  SetPart(PSCDateTimeUpDown_Time, MyLeavePart, MyDateTimePart);
end;

end.
