unit cal_frm_print_setup;

interface
{$I psc_defines.inc}

uses
{$IFDEF D6}
  Types,
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
  StdCtrls,
  Buttons,
  ExtCtrls,

  myla_system,
  myla_interfaces,

  psc_procs,
  psc_edit,
  psc_calendar;

type
  Tpsc_frm_print_setup = class(TForm)
    Panel_PrintSetup: TPanel;
    GroupBox_PrintOptions: TGroupBox;
    Label_Header: TLabel;
    Label_Title: TLabel;
    Label_Overlap: TLabel;
    Edit_Header: TEdit;
    CheckBox_Header: TCheckBox;
    CheckBox_ParentHeaderFont: TCheckBox;
    Edit_Title: TEdit;
    Button_HeaderFont: TButton;
    PSCEdit_Overlap: TPSCEdit;
    ProgressBar_Printing: TProgressBar;
    CheckBox_PageNumber: TCheckBox;
    PrintDialog_CalendarPrint: TPrintDialog;
    FontDialog1: TFontDialog;
    Panel_Buttons: TPanel;
    BitBtn_Print: TBitBtn;
    Button_Cancel: TButton;
    CheckBox_ShowProgress: TCheckBox;
    procedure BitBtn_PrintClick(Sender: TObject);
    procedure Edit_HeaderChange(Sender: TObject);
    procedure Edit_TitleChange(Sender: TObject);
    procedure PSCEdit_OverlapChange(Sender: TObject);
    procedure CheckBox_HeaderClick(Sender: TObject);
    procedure CheckBox_ShowProgressClick(Sender: TObject);
    procedure CheckBox_PageNumberClick(Sender: TObject);
    procedure CheckBox_ParentHeaderFontClick(Sender: TObject);
    procedure Button_HeaderFontClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PSCEdit_OverlapButtonClick(Sender: TObject;
      ABtnIndex: Integer);
  private
    FMyPrintCalendar : TPSCCalendar;
    procedure InitControls;
    procedure DoPrintProgress(Sender: TObject);
    { Private declarations }
  public
    property MyPrintCalendar : TPSCCalendar read FMyPrintCalendar write FMyPrintCalendar;
    { Public declarations }
  end;

var
  psc_frm_print_setup: Tpsc_frm_print_setup;

implementation

{$R *.dfm}

procedure Tpsc_frm_print_setup.BitBtn_PrintClick(Sender: TObject);
var
  MyYear, MyMonth, MyDay : word;
  MyWidth, MyHeight : integer;
  MyFooterHeight : integer;
begin
  if PrintDialog_CalendarPrint.Execute then
  begin
    if (MyPrintCalendar.CalendarsInHeight <> 1) and (MyPrintCalendar.CalendarsInWidth <> 1) then
    begin
      PSCDecodeDate(MyPrintCalendar.StartDate, MyYear, MyMonth, MyDay);
      MyMonth := 1;
      MyDay := 1;
      MyPrintCalendar.StartDate := PSCEncodeDate(MyYear, MyMonth, MyDay);
      MyWidth := MyPrintCalendar.GetMonthWidth;
      MyHeight := MyPrintCalendar.GetMonthHeight;
      MyPrintCalendar.CalendarsInHeight := 0;
      MyPrintCalendar.CalendarsInWidth := 0;
      MyPrintCalendar.Height := MyHeight* 4 + 4;
      Canvas.Font.Size := MyPrintCalendar.Font.Size;
      MyFooterHeight := Canvas.TextHeight('T') + 5;
      if MyPrintCalendar.ShowFooter then
        MyPrintCalendar.Height := MyPrintCalendar.Height + MyFooterHeight;
      MyPrintCalendar.Width := MyWidth * 3 + 4;
    end;
    MyPrintCalendar.Print;
  end;
end;

procedure Tpsc_frm_print_setup.Edit_HeaderChange(Sender: TObject);
begin
  MyPrintCalendar.PrintOptions.Header := Edit_Header.Text;
end;

procedure Tpsc_frm_print_setup.Edit_TitleChange(Sender: TObject);
begin
  MyPrintCalendar.PrintOptions.Title := Edit_Title.Text;
end;

procedure Tpsc_frm_print_setup.PSCEdit_OverlapChange(Sender: TObject);
begin
  MyPrintCalendar.PrintOptions.Overlap := StrToInt(PSCEdit_Overlap.Text);
end;

procedure Tpsc_frm_print_setup.CheckBox_HeaderClick(Sender: TObject);
begin
  if CheckBox_Header.Checked then
    MyPrintCalendar.PrintOptions.Options := MyPrintCalendar.PrintOptions.Options + [poHeader]
  else
    MyPrintCalendar.PrintOptions.Options := MyPrintCalendar.PrintOptions.Options - [poHeader];
end;

procedure Tpsc_frm_print_setup.CheckBox_ShowProgressClick(Sender: TObject);
begin
  if CheckBox_ShowProgress.Checked then
    MyPrintCalendar.PrintOptions.Options := MyPrintCalendar.PrintOptions.Options + [poShowProgress]
  else
    MyPrintCalendar.PrintOptions.Options := MyPrintCalendar.PrintOptions.Options - [poShowProgress];
end;

procedure Tpsc_frm_print_setup.CheckBox_PageNumberClick(Sender: TObject);
begin
  if CheckBox_PageNumber.Checked then
    MyPrintCalendar.PrintOptions.Options := MyPrintCalendar.PrintOptions.Options + [poPrintPageNumber]
  else
    MyPrintCalendar.PrintOptions.Options := MyPrintCalendar.PrintOptions.Options - [poPrintPageNumber];
end;

procedure Tpsc_frm_print_setup.CheckBox_ParentHeaderFontClick(
  Sender: TObject);
begin
  MyPrintCalendar.PrintOptions.ParentHeaderFont := CheckBox_ParentHeaderFont.Checked;
end;

procedure Tpsc_frm_print_setup.Button_HeaderFontClick(Sender: TObject);
begin
  if FontDialog1.Execute then
    MyPrintCalendar.PrintOptions.HeaderFont := FontDialog1.Font;
end;


procedure Tpsc_frm_print_setup.InitControls;
begin
  Edit_Header.Text := MyPrintCalendar.PrintOptions.Header;
  Edit_Title.Text := MyPrintCalendar.PrintOptions.Title;
  PSCEdit_Overlap.Text := IntToStr(MyPrintCalendar.PrintOptions.Overlap);
  CheckBox_Header.Checked := poHeader in MyPrintCalendar.PrintOptions.Options;
  CheckBox_ShowProgress.Checked := poShowProgress in MyPrintCalendar.PrintOptions.Options;
  CheckBox_ParentHeaderFont.Checked := MyPrintCalendar.PrintOptions.ParentHeaderFont;
end;

procedure Tpsc_frm_print_setup.DoPrintProgress(Sender: TObject);
begin
  if poShowProgress in (Sender as TPSCCalendar).PrintOptions.Options then
    with (Sender as TPSCCalendar), ProgressBar_Printing do
    begin
      Max := PrintedPageTotal * PrintDialog_CalendarPrint.Copies;
      Visible := true;
      Position := Min;
      while (not CancelPrinting) and (Position < Max) do
        ProgressBar_Printing.StepIt;
      Visible := false;
    end;
end;

procedure Tpsc_frm_print_setup.FormCreate(Sender: TObject);
begin
  MyPrintCalendar := TPSCCalendar.Create(Self);
  MyPrintCalendar.Visible:= true;
  MyPrintCalendar.Parent:=Self;
  MyPrintCalendar.OnPrintProgress := DoPrintProgress;
end;

procedure Tpsc_frm_print_setup.FormShow(Sender: TObject);
begin
  MyPrintCalendar.Visible := false;
  InitControls;
end;

procedure Tpsc_frm_print_setup.PSCEdit_OverlapButtonClick(Sender: TObject;
  ABtnIndex: Integer);
begin
  if ABtnIndex = 0 then
  begin
    if StrToInt(PSCEdit_Overlap.Text) = MaxInt then
      exit;
    PSCEdit_Overlap.Text := IntToStr(StrToInt(PSCEdit_Overlap.Text) + 1);
  end
  else
  begin
    if StrToInt(PSCEdit_Overlap.Text) = 0 then
      exit;
    PSCEdit_Overlap.Text := IntToStr(StrToInt(PSCEdit_Overlap.Text) - 1);
  end;
end;

end.
