unit cal_frm_setup_colors;

interface
{$I psc_defines.inc}

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  ExtCtrls,

  psc_calendar,
  psc_colorbox,
  psc_fontbox,
  psc_procs,
  psc_edit,
  psc_button_color,

  cal_demo_const;

type
  Tpsc_frm_setup_colors = class(TForm)
    Panel_Colors: TPanel;
    Label_Item: TLabel;
    ColorDialog1: TColorDialog;
    FontDialog1: TFontDialog;
    ComboBox_PSCCalendarColors: TComboBox;
    Label_Font: TLabel;
    PSCFontBar1: TPSCFontEdit;
    PSCColorButton_Color: TPSCColorButton;
    PSCColorButton_Font: TPSCColorButton;
    ComboBox_FontSize: TComboBox;
    SpeedButton_Bold: TSpeedButton;
    SpeedButton_Italic: TSpeedButton;
    Label_Size: TLabel;
    Label_FontColor: TLabel;
    Label_Color: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox_PSCCalendarColorsChange(Sender: TObject);
    procedure ComboBox_FontSizeChange(Sender: TObject);
    procedure SpeedButton_ItalicClick(Sender: TObject);
    procedure SpeedButton_BoldClick(Sender: TObject);
    procedure PSCColorButton_ColorColorSelected(Sender: TObject;
      AColor: TColor);
    procedure PSCColorButton_FontColorSelected(Sender: TObject;
      AColor: TColor);
    procedure PSCFontBar1Change(Sender: TObject);
  private
    FPSCCalendar_Colors : TPSCCalendar;
    FMyPSCColorCalChange : TNotifyEvent;
    procedure ComboBoxFill(APSCCalendar : TPSCCalendar);
    function GetCalColor(ACalColors : TPSCCalendarColorsPro; Id : integer) : TColor;
    procedure SetCalColor(ACalColors : TPSCCalendarColorsPro; Id : integer; AColor : TColor);
    function GetCalFont(ACalColors : TPSCCalendarColorsPro; Id : integer) : TFont;
    procedure SetCalFont(ACalColors : TPSCCalendarColorsPro; Id : integer; AFont : TFont);
    procedure ColorsFill(APSCCalendar : TPSCCalendar; Id : integer);
    procedure UpdateColors(APSCCalendar : TPSCCalendar);
    { Private declarations }
  public
    procedure PSCCalendarColorFontLoad(APSCCalendar : TPSCCalendar);
    procedure PSCCalendarColorFontSave(APSCCalendar: TPSCCalendar);
    property PSCCalendar_Colors : TPSCCalendar read FPSCCalendar_Colors write FPSCCalendar_Colors;
    property MyPSCColorCalChange : TNotifyEvent read FMyPSCColorCalChange write FMyPSCColorCalChange;
    { Public declarations }
  end;

var
  psc_frm_setup_colors: Tpsc_frm_setup_colors;

implementation

{$R *.dfm}

procedure Tpsc_frm_setup_colors.FormCreate(Sender: TObject);
var
  i : integer;
begin
  PSCCalendar_Colors := TPSCCalendar.Create(Self);
  PSCCalendar_Colors.Visible := false;
  PSCCalendar_Colors.Name := 'MyCustomCalendar';
  PSCCalendar_Colors.Parent := TWinControl(Self);
  for i := 6 to 24 do
    ComboBox_FontSize.Items.AddObject(IntToStr(i), TObject(i));
  ComboBox_FontSize.ItemIndex := 0;
  ComboBoxFill(PSCCalendar_Colors);
end;

procedure Tpsc_frm_setup_colors.PSCCalendarColorFontLoad(APSCCalendar : TPSCCalendar);
begin
  PSCCalendar_Colors.Colors.Assign(TPSCCalendar(APSCCalendar).Colors);
  ComboBox_PSCCalendarColorsChange(ComboBox_PSCCalendarColors);
end;

procedure Tpsc_frm_setup_colors.PSCCalendarColorFontSave(APSCCalendar : TPSCCalendar);
begin
  TPSCCalendar(APSCCalendar).Colors.Assign(PSCCalendar_Colors.Colors);
end;

procedure Tpsc_frm_setup_colors.ComboBoxFill(APSCCalendar : TPSCCalendar);
begin
  ColorsFill(PSCCalendar_Colors, 0);
end;

function Tpsc_frm_setup_colors.GetCalColor(ACalColors : TPSCCalendarColorsPro;
  Id: integer) : TColor;
var
  Color : TColor;
begin
  case Id of
    0: Color := ACalColors.Border;
    1: Color := ACalColors.MonthHeader;
    3: Color := ACalColors.WeekHeader;
    4: Color := ACalColors.Days;
    5: Color := ACalColors.Selected;
    6: Color := ACalColors.SelectedText;
    7: Color := ACalColors.Grayed;
    8: Color := ACalColors.WeekEndText;
    9: Color := ACalColors.NowRect;
    10: Color := ACalColors.WeekSide;
    11: Color := ACalColors.DayLines;
    12: Color := ACalColors.WeekLineColor;
    13: Color := ACalColors.HeaderBorderColor;
    14: Color := ACalColors.GrayedBkColor;
  else
    Color := ACalColors.ArrowColor;
  end;
  result := Color;
end;

procedure Tpsc_frm_setup_colors.SetCalColor(ACalColors : TPSCCalendarColorsPro;
  Id: integer; AColor : TColor);
begin
  case Id of
    0: ACalColors.Border := AColor;
    1: ACalColors.MonthHeader := AColor;
    3: ACalColors.WeekHeader := AColor;
    4: ACalColors.Days := AColor;
    5: ACalColors.Selected := AColor;
    6: ACalColors.SelectedText := AColor;
    7: ACalColors.Grayed := AColor;
    8: ACalColors.WeekEndText := AColor;
    9: ACalColors.NowRect := AColor;
    10: ACalColors.WeekSide := AColor;
    11: ACalColors.DayLines := AColor;
    12: ACalColors.WeekLineColor := AColor;
    13: ACalColors.HeaderBorderColor := AColor;
    14: ACalColors.GrayedBkColor := AColor;
  else
    ACalColors.ArrowColor := AColor;
  end;
end;


function Tpsc_frm_setup_colors.GetCalFont(
  ACalColors: TPSCCalendarColorsPro; Id: integer): TFont;
var
  Font : TFont;
begin
  case Id of
    0: Font := ACalColors.HolidaysFont;
    1: Font := ACalColors.WeekNumbersFont;
    2: Font := ACalColors.WeekDaysFont;
    3: Font := ACalColors.HeaderFont;
    4: Font := ACalColors.DaysFont;
  else
    Font := ACalColors.HolidaysFont;
  end;
  result := Font;
end;

procedure Tpsc_frm_setup_colors.SetCalFont(
  ACalColors: TPSCCalendarColorsPro; Id: integer; AFont: TFont);
begin
  case Id of
    0: ACalColors.HolidaysFont := AFont;
    1: ACalColors.WeekNumbersFont := AFont;
    2: ACalColors.WeekDaysFont := AFont;
    3: ACalColors.HeaderFont := AFont;
  else
    ACalColors.HolidaysFont := AFont;
  end;
end;

procedure Tpsc_frm_setup_colors.ColorsFill(APSCCalendar : TPSCCalendar; Id : integer);
var
  i : integer;
begin
  ComboBox_PSCCalendarColors.Items.Clear;
  with APSCCalendar do
  begin
    for i := 1 to CPSCCalColorsCount do
    begin
      ComboBox_PSCCalendarColors.Items.AddObject(CPSCCalColorsNames[i], TObject(i - 1));
    end;
    for i := 1 to 5 do
    ComboBox_PSCCalendarColors.Items.AddObject(CPSCCalFontNames[i], TObject(GetCalFont(APSCCalendar.Colors, i - 1)));
  end;
  ComboBox_PSCCalendarColors.ItemIndex := Id;
end;

procedure Tpsc_frm_setup_colors.ComboBox_PSCCalendarColorsChange(
  Sender: TObject);
begin
  if (integer(ComboBox_PSCCalendarColors.Items.Objects[ComboBox_PSCCalendarColors.ItemIndex]) > ComboBox_PSCCalendarColors.Items.Count) then
  begin
    PSCFontBar1.Enabled := true;
    PSCColorButton_Font.Enabled := true;
    ComboBox_FontSize.Enabled := true;
    SpeedButton_Bold.Enabled := true;
    SpeedButton_Italic.Enabled := true;
    PSCColorButton_Color.Enabled := false;
    ComboBox_FontSize.ItemIndex := ComboBox_FontSize.Items.IndexOfObject(TObject(TFont(ComboBox_PSCCalendarColors.Items.Objects[ComboBox_PSCCalendarColors.ItemIndex]).Size));
    SpeedButton_Bold.Down := (fsBold in TFont(ComboBox_PSCCalendarColors.Items.Objects[ComboBox_PSCCalendarColors.ItemIndex]).Style);
    SpeedButton_Italic.Down := (fsItalic in TFont(ComboBox_PSCCalendarColors.Items.Objects[ComboBox_PSCCalendarColors.ItemIndex]).Style);
    PSCFontBar1.FontName := TFont(ComboBox_PSCCalendarColors.Items.Objects[ComboBox_PSCCalendarColors.ItemIndex]).Name;
    PSCColorButton_Font.SelectedColor := TFont(ComboBox_PSCCalendarColors.Items.Objects[ComboBox_PSCCalendarColors.ItemIndex]).Color;
  end
  else
  begin
    PSCFontBar1.Enabled := false;
    PSCColorButton_Font.Enabled := false;
    ComboBox_FontSize.Enabled := false;
    SpeedButton_Bold.Enabled := false;
    SpeedButton_Italic.Enabled := false;
    PSCColorButton_Color.Enabled := true;
    PSCColorButton_Color.SelectedColor := GetCalColor(PSCCalendar_Colors.Colors, ComboBox_PSCCalendarColors.ItemIndex);
  end;
end;

procedure Tpsc_frm_setup_colors.ComboBox_FontSizeChange(Sender: TObject);
begin
  TFont(ComboBox_PSCCalendarColors.Items.Objects[ComboBox_PSCCalendarColors.ItemIndex]).Size := integer(ComboBox_FontSize.Items.Objects[ComboBox_FontSize.ItemIndex]);
  UpdateColors(PSCCalendar_Colors);
  MyPSCColorCalChange(Self);
end;

procedure Tpsc_frm_setup_colors.SpeedButton_ItalicClick(Sender: TObject);
begin
  if SpeedButton_Italic.Down then
    TFont(ComboBox_PSCCalendarColors.Items.Objects[ComboBox_PSCCalendarColors.ItemIndex]).Style := TFont(ComboBox_PSCCalendarColors.Items.Objects[ComboBox_PSCCalendarColors.ItemIndex]).Style + [fsItalic]
  else
    TFont(ComboBox_PSCCalendarColors.Items.Objects[ComboBox_PSCCalendarColors.ItemIndex]).Style := TFont(ComboBox_PSCCalendarColors.Items.Objects[ComboBox_PSCCalendarColors.ItemIndex]).Style - [fsItalic];
  UpdateColors(PSCCalendar_Colors);
  MyPSCColorCalChange(Self);
end;

procedure Tpsc_frm_setup_colors.SpeedButton_BoldClick(Sender: TObject);
begin
  if SpeedButton_Bold.Down then
    TFont(ComboBox_PSCCalendarColors.Items.Objects[ComboBox_PSCCalendarColors.ItemIndex]).Style := TFont(ComboBox_PSCCalendarColors.Items.Objects[ComboBox_PSCCalendarColors.ItemIndex]).Style + [fsBold]
  else
    TFont(ComboBox_PSCCalendarColors.Items.Objects[ComboBox_PSCCalendarColors.ItemIndex]).Style := TFont(ComboBox_PSCCalendarColors.Items.Objects[ComboBox_PSCCalendarColors.ItemIndex]).Style - [fsBold];
  UpdateColors(PSCCalendar_Colors);
  MyPSCColorCalChange(Self);
end;

procedure Tpsc_frm_setup_colors.UpdateColors(APSCCalendar: TPSCCalendar);
var
  i : integer;
begin
  with APSCCalendar do
    for i := CPSCCalColorsCount to CPSCCalColorsCount + 3 do
      SetCalFont(PSCCalendar_Colors.Colors, i - CPSCCalColorsCount, TFont(ComboBox_PSCCalendarColors.Items.Objects[i]));
end;

procedure Tpsc_frm_setup_colors.PSCColorButton_ColorColorSelected(
  Sender: TObject; AColor: TColor);
begin
  if (PSCCalendar_Colors <> nil) and (ComboBox_PSCCalendarColors.ItemIndex >= 0) then
  begin
    SetCalColor(PSCCalendar_Colors.Colors, integer(ComboBox_PSCCalendarColors.Items.Objects[ComboBox_PSCCalendarColors.ItemIndex]), PSCColorButton_Color.SelectedColor);
    ColorsFill(PSCCalendar_Colors, ComboBox_PSCCalendarColors.ItemIndex);
    MyPSCColorCalChange(Self);    
  end;
end;

procedure Tpsc_frm_setup_colors.PSCColorButton_FontColorSelected(
  Sender: TObject; AColor: TColor);
begin
  if (PSCCalendar_Colors <> nil) and (ComboBox_PSCCalendarColors.ItemIndex >= 0) then
  begin
    TFont(ComboBox_PSCCalendarColors.Items.Objects[ComboBox_PSCCalendarColors.ItemIndex]).Color := PSCColorButton_Font.SelectedColor;
    UpdateColors(PSCCalendar_Colors);
    MyPSCColorCalChange(Self);
  end;
end;

procedure Tpsc_frm_setup_colors.PSCFontBar1Change(Sender: TObject);
begin
  if PSCCalendar_Colors <> nil then
  begin
    TFont(ComboBox_PSCCalendarColors.Items.Objects[ComboBox_PSCCalendarColors.ItemIndex]).Name := PSCFontBar1.FontName;
    UpdateColors(PSCCalendar_Colors);
    MyPSCColorCalChange(Self);
  end;
end;

end.
