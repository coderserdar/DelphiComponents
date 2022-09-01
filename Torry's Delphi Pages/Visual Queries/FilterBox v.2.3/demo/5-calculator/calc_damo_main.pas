unit calc_damo_main;

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

  myla_interfaces,
  myla_system,

  psc_wrapper,
  psc_edit,
  psc_calculator,
  psc_procs,
  psc_edit_color,
  calc_demo_const;

type
  Tpsc_frm_setup_main = class(TForm)
    PageControl_Main: TPageControl;
    TabSheet_Calculator: TTabSheet;
    TabSheet_CalcEdit: TTabSheet;
    Panel_Manager: TPanel;
    Label_ButtonDist: TLabel;
    Label_ButtonLongDist: TLabel;
    Label_Precision: TLabel;
    Label_Colors: TLabel;
    Label_Format: TLabel;
    Label_Digits: TLabel;
    PSCEdit_ButtonLongDist: TPSCEdit;
    PSCEdit_Precision: TPSCEdit;
    PSCEdit_ButtonDist: TPSCEdit;
    CheckBox_Enabled: TCheckBox;
    CheckBox_Flat: TCheckBox;
    CheckBox_ShowDisplay: TCheckBox;
    PSCColorEdit1: TPSCColorEdit;
    ComboBox_Colors: TComboBox;
    ComboBox_Format: TComboBox;
    PSCEdit_Digits: TPSCEdit;
    Panel_CalcEdit: TPanel;
    ComboBox_EditThemeNames: TComboBox;
    PSCEdit_NullValue: TPSCEdit;
    Label_NullValue: TLabel;
    Label_ThemeName: TLabel;
    Bevel2: TBevel;
    CheckBox_UseNullValue: TCheckBox;
    PSCCalcEdit1: TPSCCalcEdit;
    PSCCalculator1: TPSCCalculator;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PSCEdit_ButtonDistButtonClick(Sender: TObject;
      ABtnIndex: Integer);
    procedure PageControl_MainChange(Sender: TObject);
    procedure PSCEdit_ButtonLongDistButtonClick(Sender: TObject;
      ABtnIndex: Integer);
    procedure PSCEdit_PrecisionButtonClick(Sender: TObject;
      ABtnIndex: Integer);
    procedure PSCEdit_DigitsButtonClick(Sender: TObject;
      ABtnIndex: Integer);
    procedure ComboBox_FormatChange(Sender: TObject);
    procedure ComboBox_ColorsChange(Sender: TObject);
    procedure PSCColorEdit1Change(Sender: TObject);
    procedure CheckBox_EnabledClick(Sender: TObject);
    procedure CheckBox_FlatClick(Sender: TObject);
    procedure CheckBox_ShowDisplayClick(Sender: TObject);
    procedure PSCEdit_NullValueButtonClick(Sender: TObject;
      ABtnIndex: Integer);
    procedure ComboBox_EditThemeNamesChange(Sender: TObject);
    procedure CheckBox_UseNullValueClick(Sender: TObject);
  private
    { Private declarations }
    function GetActiveCalculator : TPSCCalculator;
    function GetCalcColor(ACalculator : TPSCCalculator) : TColor;
    procedure SetCalcColor(ACalculator : TPSCCalculator);
    procedure FillComboBox;
    procedure InitControls;
  protected
    property ActiveCalculator : TPSCCalculator read GetActiveCalculator;
  public
    { Public declarations }
  end;

var
  psc_frm_setup_main: Tpsc_frm_setup_main;

implementation

{$R *.dfm}

procedure Tpsc_frm_setup_main.FormCreate(Sender: TObject);
begin
  PageControl_Main.ActivePageIndex := 0;
  FillComboBox;
end;

procedure Tpsc_frm_setup_main.FormShow(Sender: TObject);
begin
  InitControls;
end;

function Tpsc_frm_setup_main.GetActiveCalculator: TPSCCalculator;
begin
  case PageControl_Main.ActivePageIndex of
    0:
      result := PSCCalculator1;
    1:
      result := PSCCalcEdit1.Calculator;
  else
    result := PSCCalculator1;
  end;
end;

procedure Tpsc_frm_setup_main.PSCEdit_ButtonDistButtonClick(
  Sender: TObject; ABtnIndex: Integer);
var
  MyNumber : integer;
begin
  MyNumber := StrToInt(PSCEdit_ButtonDist.Text);
  if ABtnIndex = 0 then
    Inc(MyNumber)
  else
    if MyNumber > 0 then
      Dec(MyNumber);
  PSCEdit_ButtonDist.Text := IntToStr(MyNumber);
  ActiveCalculator.ButtonDist := MyNumber;
end;

procedure Tpsc_frm_setup_main.PageControl_MainChange(Sender: TObject);
begin
  Panel_CalcEdit.Visible := (PageControl_Main.ActivePageIndex = 1);
  InitControls;  
end;

procedure Tpsc_frm_setup_main.PSCEdit_ButtonLongDistButtonClick(
  Sender: TObject; ABtnIndex: Integer);
var
  MyNumber : integer;
begin
  MyNumber := StrToInt(PSCEdit_ButtonLongDist.Text);
  if ABtnIndex = 0 then
    Inc(MyNumber)
  else
    if MyNumber > 0 then
      Dec(MyNumber);
  PSCEdit_ButtonLongDist.Text := IntToStr(MyNumber);
  ActiveCalculator.ButtonLongDist := MyNumber;
end;

procedure Tpsc_frm_setup_main.PSCEdit_PrecisionButtonClick(Sender: TObject;
  ABtnIndex: Integer);
var
  MyNumber : integer;
begin
  MyNumber := StrToInt(PSCEdit_Precision.Text);
  if ABtnIndex = 0 then
    Inc(MyNumber)
  else
    if MyNumber > 0 then
      Dec(MyNumber);
  PSCEdit_Precision.Text := IntToStr(MyNumber);
  ActiveCalculator.Precision := MyNumber;
end;

procedure Tpsc_frm_setup_main.PSCEdit_DigitsButtonClick(Sender: TObject;
  ABtnIndex: Integer);
var
  MyNumber : integer;
begin
  MyNumber := StrToInt(PSCEdit_Digits.Text);
  if ABtnIndex = 0 then
    Inc(MyNumber)
  else
    if MyNumber > 0 then
      Dec(MyNumber);
  PSCEdit_Digits.Text := IntToStr(MyNumber);
  ActiveCalculator.Digits := MyNumber;
  ActiveCalculator.Invalidate;
end;

procedure Tpsc_frm_setup_main.ComboBox_FormatChange(Sender: TObject);
begin
  ActiveCalculator.Format := TFloatFormat(ComboBox_Format.ItemIndex);
end;

procedure Tpsc_frm_setup_main.ComboBox_ColorsChange(Sender: TObject);
begin
  PSCColorEdit1.SelectedColor := GetCalcColor(ActiveCalculator);
end;

procedure Tpsc_frm_setup_main.PSCColorEdit1Change(Sender: TObject);
begin
  SetCalcColor(ActiveCalculator);
end;

procedure Tpsc_frm_setup_main.CheckBox_EnabledClick(Sender: TObject);
begin
  ActiveCalculator.Enabled := CheckBox_Enabled.Checked;
end;

procedure Tpsc_frm_setup_main.CheckBox_FlatClick(Sender: TObject);
begin
  ActiveCalculator.Flat := CheckBox_Flat.Checked;
end;

procedure Tpsc_frm_setup_main.CheckBox_ShowDisplayClick(Sender: TObject);
begin
  ActiveCalculator.ShowDisplay := CheckBox_ShowDisplay.Checked;
end;

procedure Tpsc_frm_setup_main.FillComboBox;
var
  S : TStrings;
  i : integer;
  MyComboItems : IPSCStrings;
begin
  S := TStringList.Create;
  try
    PSCGetThemeRegister.EnumThemeNames(PSCCreateStringsAdapter(S));
    ComboBox_EditThemeNames.Items.Assign(S);
  finally
    S.Free;
  end;
  ComboBox_Colors.Clear;
  for i := Low(CPSCCalculatorColors) to High(CPSCCalculatorColors) do
    ComboBox_Colors.Items.Add(CPSCCalculatorColors[i]);
  MyComboItems := PSCCreateStringList(ioOwned);
  PSCGetEnumTypeValueNames(TypeInfo(TFloatFormat),MyComboItems);
  MyComboItems.AssignTo(PSCCreateStringsAdapter(ComboBox_Format.Items));
end;

function Tpsc_frm_setup_main.GetCalcColor(
  ACalculator: TPSCCalculator): TColor;
var
  i : integer;
  MyIndex : integer;
begin
  result := clWindow;
  MyIndex := ComboBox_Colors.ItemIndex;
  if MyIndex < 0 then
    exit;
  for i := Low(CPSCCalculatorColors) to High(CPSCCalculatorColors) do
  begin
    if (PSCCompareText(CPSCCalculatorColors[i], ComboBox_Colors.Items[MyIndex]) = 0) then
    begin
      case i of
        1:
          result := ACalculator.ActionsColor;
        2:
          result := ACalculator.Color;
        3:
          result := ACalculator.DisplayBackColor;
        4:
          result := ACalculator.DisplayBorderColor;
        5:
          result := ACalculator.NumbersColor;
      end;
      break;
    end;
  end;
end;

procedure Tpsc_frm_setup_main.InitControls;
var
  i: integer;
begin
  PSCEdit_ButtonDist.Text := IntToStr(ActiveCalculator.ButtonDist);
  PSCEdit_ButtonLongDist.Text := IntToStr(ActiveCalculator.ButtonLongDist);
  PSCEdit_Precision.Text  := IntToStr(ActiveCalculator.Precision);
  PSCEdit_Digits.Text := IntToStr(ActiveCalculator.Digits);
  ComboBox_Colors.ItemIndex := 0;
  PSCColorEdit1.SelectedColor := GetCalcColor(ActiveCalculator);
  CheckBox_Enabled.Checked := ActiveCalculator.Enabled;
  CheckBox_Flat.Checked := ActiveCalculator.Flat;
  CheckBox_ShowDisplay.Checked := ActiveCalculator.ShowDisplay;

  CheckBox_UseNullValue.Checked := PSCCalcEdit1.UseNullValue;
  PSCEdit_NullValue.Text := FloatToStr(PSCCalcEdit1.NullValue);
  with ComboBox_EditThemeNames do
  begin
    i := Items.IndexOf(PSCCalcEdit1.ThemeName);
    if i < 0 then
      i := 2;
    ItemIndex := i;
  end;
  ComboBox_Format.ItemIndex := integer(ActiveCalculator.Format);
end;

procedure Tpsc_frm_setup_main.SetCalcColor(ACalculator: TPSCCalculator);
var
  i : integer;
  MyIndex : integer;
begin
  MyIndex := ComboBox_Colors.ItemIndex;
  if MyIndex < 0 then
    exit;
  for i := Low(CPSCCalculatorColors) to High(CPSCCalculatorColors) do
  begin
    if (PSCCompareText(CPSCCalculatorColors[i], ComboBox_Colors.Items[MyIndex]) = 0) then
    begin
      case i of
        1:
          ACalculator.ActionsColor := PSCColorEdit1.SelectedColor;
        2:
          ACalculator.Color := PSCColorEdit1.SelectedColor;
        3:
          ACalculator.DisplayBackColor := PSCColorEdit1.SelectedColor;
        4:
          ACalculator.DisplayBorderColor := PSCColorEdit1.SelectedColor;
        5:
          ACalculator.NumbersColor := PSCColorEdit1.SelectedColor;
      end;
      break;
    end;
  end;
end;

procedure Tpsc_frm_setup_main.PSCEdit_NullValueButtonClick(Sender: TObject;
  ABtnIndex: Integer);
var
  MyNumber : integer;
begin
  MyNumber := StrToInt(PSCEdit_NullValue.Text);
  if ABtnIndex = 0 then
    Inc(MyNumber)
  else
    if MyNumber > 0 then
      Dec(MyNumber);
  PSCEdit_NullValue.Text := IntToStr(MyNumber);
  PSCCalcEdit1.NullValue := MyNumber;
end;

procedure Tpsc_frm_setup_main.ComboBox_EditThemeNamesChange(
  Sender: TObject);
begin
  PSCCalcEdit1.ThemeName :=
   ComboBox_EditThemeNames.Items[ComboBox_EditThemeNames.ItemIndex];
end;

procedure Tpsc_frm_setup_main.CheckBox_UseNullValueClick(Sender: TObject);
begin
  PSCCalcEdit1.UseNullValue :=
   CheckBox_UseNullValue.Checked;
end;

end.
