unit cal_demo_monthbox;

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
  Grids,
  ImgList,
  ComCtrls,
  cal_frm_setup_preferences,
  cal_demo_const,

  myla_interfaces,
  myla_system,

  psc_procs,
  psc_calendar,
  psc_colorbox,
  psc_edit,
  psc_button_color,
  psc_wrapper;

type
  TPSCMonthArrow = (maLeftArrow, maLeftArrowHot, maRightArrow, maRightArrowHot);

  Tcal_demo_frm_monthbox = class(TForm)
    Panel_MonthBox_Main: TPanel;
    ImageList1: TImageList;
    FontDialog1: TFontDialog;
    Panel_Buttons: TPanel;
    Panel_Manager: TPanel;
    LabelArrowStyle: TLabel;
    Label_Colors: TLabel;
    Label_Fonts: TLabel;
    SpeedButton_Fonts: TSpeedButton;
    PSCColorButton_MonthsBoxColors: TPSCColorButton;
    ComboBox_ArrowStyle: TComboBox;
    CheckBox_Flat: TCheckBox;
    CheckBox_ShowHorzLines: TCheckBox;
    CheckBox_ShowVertLines: TCheckBox;
    CheckBox_ShowArrowEdges: TCheckBox;
    ComboBox_Colors: TComboBox;
    ComboBox_Fonts: TComboBox;
    Bevel1: TBevel;
    Panel6: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PSCMonthsBox1: TPSCMonthsBox;
    PSCMonthsBox2: TPSCMonthsBox;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel2: TBevel;
    Panel7: TPanel;
    PSCMonthsBox3: TPSCMonthsBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox_ArrowStyleChange(Sender: TObject);
    procedure CheckBox_FlatClick(Sender: TObject);
    procedure CheckBox_ShowArrowEdgesClick(Sender: TObject);
    procedure CheckBox_ShowHorzLinesClick(Sender: TObject);
    procedure CheckBox_ShowVertLinesClick(Sender: TObject);
    procedure PSCColorButton_MonthsBoxColorsColorSelected(Sender: TObject;
      AColor: TColor);
    procedure SpeedButton_FontsClick(Sender: TObject);
    procedure ComboBox_ColorsChange(Sender: TObject);
    procedure PSCMonthsBox1Click(Sender: TObject);
    procedure PSCMonthsBox3Click(Sender: TObject);
    procedure PSCMonthsBox2Click(Sender: TObject);
  private
    FActiveMonthsBox : TPSCMonthsBox;
    function GetMonthsBoxColor(Id : integer) : TColor;
    function SetMonthsBoxFonts(AFont : TFont) : TFont;
    procedure InitControls;
    procedure FontsAndColorsFill;
    procedure SetMonthsBoxColor(Id : integer);
    procedure UpdateControls(APSCMonthsBox : TPSCMonthsBox);
  public
    property ActiveMonthsBox : TPSCMonthsBox read FActiveMonthsBox write FActiveMonthsBox;
  end;

var
  cal_demo_frm_monthbox: Tcal_demo_frm_monthbox;

implementation

{$R *.dfm}

procedure Tcal_demo_frm_monthbox.FormCreate(Sender: TObject);
begin
  ActiveMonthsBox := PSCMonthsBox1;
  FontsAndColorsFill;
  InitControls;
  UpdateControls(ActiveMonthsBox);
end;

procedure Tcal_demo_frm_monthbox.ComboBox_ArrowStyleChange(
  Sender: TObject);
begin
  if ActiveMonthsBox <> nil then
    ActiveMonthsBox.ArrowStyle := TPSCArrowStyle(ComboBox_ArrowStyle.ItemIndex);
end;

procedure Tcal_demo_frm_monthbox.CheckBox_FlatClick(Sender: TObject);
begin
  if ActiveMonthsBox <> nil then
    ActiveMonthsBox.Flat := CheckBox_Flat.Checked;
end;

procedure Tcal_demo_frm_monthbox.CheckBox_ShowArrowEdgesClick(
  Sender: TObject);
begin
  if ActiveMonthsBox <> nil then
    ActiveMonthsBox.ShowEdges := CheckBox_ShowArrowEdges.Checked;
end;

procedure Tcal_demo_frm_monthbox.CheckBox_ShowHorzLinesClick(
  Sender: TObject);
begin
  if ActiveMonthsBox <> nil then
    ActiveMonthsBox.ShowHorzLines := CheckBox_ShowHorzLines.Checked;
end;

procedure Tcal_demo_frm_monthbox.CheckBox_ShowVertLinesClick(
  Sender: TObject);
begin
  if ActiveMonthsBox <> nil then
    ActiveMonthsBox.ShowVertLines := CheckBox_ShowVertLines.Checked;
end;

procedure Tcal_demo_frm_monthbox.FontsAndColorsFill;
var
  MyComboItems : IPSCStrings;
begin
  MyComboItems := PSCCreateStringList(ioOwned);
  PSCGetEnumTypeValueNames(TypeInfo(TPSCMonthColors),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_Colors.Items));
  PSCGetEnumTypeValueNames(TypeInfo(TPSCMonthFonts),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_Fonts.Items));
end;

procedure Tcal_demo_frm_monthbox.PSCColorButton_MonthsBoxColorsColorSelected(
  Sender: TObject; AColor: TColor);
begin
  if ComboBox_Colors.ItemIndex < 0 then
    exit;
  if ActiveMonthsBox <> nil then
    SetMonthsBoxColor(integer(TPSCMonthColors(ComboBox_Colors.ItemIndex)));
end;

procedure Tcal_demo_frm_monthbox.SpeedButton_FontsClick(Sender: TObject);
begin
  with ActiveMonthsBox do
  begin
    case TPSCMonthFonts(ComboBox_Fonts.ItemIndex) of
      WorkArea: Font := SetMonthsBoxFonts(Font);
      HeaderArea: {HeaderFont := PSCCreateFontAdapter(SetMonthsBoxFonts(HeaderFont))};
    end;
  end;
end;

function Tcal_demo_frm_monthbox.SetMonthsBoxFonts(AFont: TFont) : TFont;
begin
  FontDialog1.Font := AFont;
  if FontDialog1.Execute then
    result :=FontDialog1.Font
  else
    result := AFont;
end;

procedure Tcal_demo_frm_monthbox.ComboBox_ColorsChange(Sender: TObject);
begin
  if ActiveMonthsBox <> nil then
    PSCColorButton_MonthsBoxColors.SelectedColor := GetMonthsBoxColor(integer(TPSCMonthColors(ComboBox_Colors.ItemIndex)));
end;

procedure Tcal_demo_frm_monthbox.SetMonthsBoxColor(Id: integer);
begin
  if ActiveMonthsBox <> nil then
    with ActiveMonthsBox do
      case Id of
        0: Color := PSCColorButton_MonthsBoxColors.SelectedColor;
        1: CaptionColor := PSCColorButton_MonthsBoxColors.SelectedColor;
        2: CaptionTextColor := PSCColorButton_MonthsBoxColors.SelectedColor;
        3: SelectedColor := PSCColorButton_MonthsBoxColors.SelectedColor;
        4: SelectedTextColor := PSCColorButton_MonthsBoxColors.SelectedColor;
        5: HeaderColor := PSCColorButton_MonthsBoxColors.SelectedColor;
        6: ArrowColor := PSCColorButton_MonthsBoxColors.SelectedColor;
      else
        Color := PSCColorButton_MonthsBoxColors.SelectedColor;
      end;
end;

function Tcal_demo_frm_monthbox.GetMonthsBoxColor(Id: integer): TColor;
var
  MyColor : TColor;
begin
  MyColor := clRed;
  if ActiveMonthsBox <> nil then
    with ActiveMonthsBox do
      case Id of
        0: MyColor := Color;
        1: MyColor := CaptionColor;
        2: MyColor := CaptionTextColor;
        3: MyColor := SelectedColor;
        4: MyColor := SelectedTextColor;
        5: MyColor := HeaderColor;
        6: MyColor := ArrowColor;
      else
        MyColor := Color;
      end;
  result := MyColor;
end;

procedure Tcal_demo_frm_monthbox.InitControls;
var
  MyComboItems : IPSCStrings;
begin
  MyComboItems := PSCCreateStringList(ioOwned);
  PSCGetEnumTypeValueNames(TypeInfo(TPSCArrowStyle),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_ArrowStyle.Items));
  while ComboBox_ArrowStyle.Items.Count > 3 do
    ComboBox_ArrowStyle.Items.Delete(ComboBox_ArrowStyle.Items.Count - 1);
end;

procedure Tcal_demo_frm_monthbox.PSCMonthsBox1Click(Sender: TObject);
begin
  if ActiveMonthsBox <> nil then
  begin
    ActiveMonthsBox := PSCMonthsBox1;
    UpdateControls(ActiveMonthsBox);
  end;
end;

procedure Tcal_demo_frm_monthbox.PSCMonthsBox3Click(Sender: TObject);
begin
  if ActiveMonthsBox <> nil then
  begin
    ActiveMonthsBox := PSCMonthsBox3;
    UpdateControls(ActiveMonthsBox);
  end;
end;

procedure Tcal_demo_frm_monthbox.PSCMonthsBox2Click(Sender: TObject);
begin
  if ActiveMonthsBox <> nil then
  begin
    ActiveMonthsBox := PSCMonthsBox2;
    UpdateControls(ActiveMonthsBox);
  end;
end;

procedure Tcal_demo_frm_monthbox.UpdateControls(
  APSCMonthsBox: TPSCMonthsBox);
begin
  CheckBox_Flat.Checked := APSCMonthsBox.Flat;
  CheckBox_ShowArrowEdges.Checked := APSCMonthsBox.ShowEdges;
  CheckBox_ShowHorzLines.Checked := APSCMonthsBox.ShowHorzLines;
  CheckBox_ShowVertLines.Checked := APSCMonthsBox.ShowVertLines;
  ComboBox_Colors.ItemIndex := 1;
  ComboBox_Fonts.ItemIndex := 1;
  ComboBox_ArrowStyle.ItemIndex := integer(APSCMonthsBox.ArrowStyle);
  PSCColorButton_MonthsBoxColors.SelectedColor := GetMonthsBoxColor(integer(TPSCMonthColors(ComboBox_Colors.ItemIndex)));
end;

end.
