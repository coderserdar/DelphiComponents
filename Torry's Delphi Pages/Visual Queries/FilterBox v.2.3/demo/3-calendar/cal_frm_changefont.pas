unit cal_frm_changefont;

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
  ComCtrls,
  StdCtrls,
  ExtCtrls,

  psc_calendar,
  cal_demo_const,
  ImgList;

type
  Tcal_frm_fontchange = class(TForm)
    Button_Font: TButton;
    MonthCalendar1: TMonthCalendar;
    FontDialog1: TFontDialog;
    ImageList1: TImageList;
    Label_PSCCalendar: TLabel;
    Label_MonthCalendar: TLabel;
    Panel1: TPanel;
    Label_PSCCalendarSize: TLabel;
    Label_MonthCalendarSize: TLabel;
    procedure Button_FontClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMyCustomCalendar : TPSCCalendar;
    procedure DoPanelUpdate;
    procedure UpdateForm;
    { Private declarations }
  public
    property MyCustomCalendar : TPSCCalendar read FMyCustomCalendar write FMyCustomCalendar;
    { Public declarations }
  end;

var
  cal_frm_fontchange: Tcal_frm_fontchange;

implementation
{$R *.dfm}

procedure Tcal_frm_fontchange.Button_FontClick(Sender: TObject);
begin
  FontDialog1.Font.Assign(MyCustomCalendar.Font);
  if FontDialog1.Execute then
  begin
    with MyCustomCalendar do
    begin
      MonthCalendar1.Font := FontDialog1.Font;
      Font := FontDialog1.Font;
      Width := GetCalendarWidth;
      Height := GetCalendarHeight;
      Application.ProcessMessages;
      DoPanelUpdate;
      UpdateForm;
    end;
  end;
end;

procedure Tcal_frm_fontchange.DoPanelUpdate;
var
  SizeXY : TSize;
begin
  with MyCustomCalendar do
  begin
    SizeXY := Canvas.TextExtent('s');
    MonthCalendar1.Left := Left + Width + CPSCCalendarsIndent;
    Label_PSCCalendarSize.Left := Left;
    Label_MonthCalendarSize.Left := Left + Width + CPSCCalendarsIndent;
    Label_PSCCalendarSize.Caption :=
     SPSCCalendarWidth + ' ' + IntToStr(Width) + ' ' +
     SPSCCalendarHeight + ' ' + IntToStr(Height{ + Panel_TodayImage.Height});
    Label_MonthCalendarSize.Caption :=
     SPSCMonthCalendarWidth + ' ' + IntToStr(MonthCalendar1.Width) + ' ' +
     SPSCMonthCalendarHeight + ' ' + IntToStr(MonthCalendar1.Height);
   end;
end;

procedure Tcal_frm_fontchange.FormCreate(Sender: TObject);
begin
  MyCustomCalendar := TPSCCalendar.Create(Self);
  with MyCustomCalendar do
  begin
    Parent := Self;
    Left := 30;
    Label_PSCCalendar.Left := Left;
    CalendarStyle := cstMonthCalendar;
    Top := MonthCalendar1.Top;
    Width := 204;//GetCalendarWidth;
    Height := 153;//GetCalendarHeight;
  end;
  Label_MonthCalendar.Left := MonthCalendar1.Left;
  DoPanelUpdate;
  UpdateForm;
end;


procedure Tcal_frm_fontchange.UpdateForm;
var
  MyLeft, MyWidth : integer;
  MyTop, MyHeight : integer;
  MyRect : TRect;
begin
  Label_MonthCalendar.Left := MyCustomCalendar.Left + MyCustomCalendar.Width + CPSCCalendarsIndent;
  MyWidth := MyCustomCalendar.Width * 2 + 100;
  MyHeight := MyCustomCalendar.Height + 200;
  MyLeft := (Screen.Width - MyWidth) div 2;
  MyTop := (Screen.Height - MyHeight) div 2;
  MyRect := Rect(MyLeft, MyTop, MyLeft + MyWidth, MyTop + MyHeight);
  BoundsRect := MyRect;
end;

end.
