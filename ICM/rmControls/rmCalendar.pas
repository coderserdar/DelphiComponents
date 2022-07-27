{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmCalendar
Purpose  : Replacement for the windows comctrl calendar.
           Also has CalendarCombo.
Date     : 01-01-1999
Author   : Ryan J. Mills
Version  : 1.90
================================================================================
}
unit rmCalendar;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Grids, rmBtnEdit, Buttons, rmmsglist, rmScrnCtrls,
  rmLibrary;

type
  TrmCustomCalendar = class;
  
  TCurrentDateValue = (cdvYear, cdvMonth, cdvDay);

  TPaintCellEvent = procedure(Sender:TObject; ARow, ACol:Longint;  Rect: TRect; State: TGridDrawState; Date:TDate) of object;

  TrmCalendarColors = class(TPersistent)
  private
    fWeekendText: TColor;
    fWeekdaybackground: TColor;
    fDayNamesBackground: TColor;
    fWeekdayText: TColor;
    fTodayText: TColor;
    fWeekendBackground: TColor;
    fDayNamesText: TColor;
    fOtherMonthDayBackground: TColor;
    fOtherMonthDayText: TColor;
    fCustomCalendar: TrmCustomCalendar;
    procedure SetDayNamesBackground(const Value: TColor);
    procedure SetDayNamesText(const Value: TColor);
    procedure SetOtherMonthDayBackground(const value : TColor);
    procedure SetOtherMonthDayText(const Value: TColor);
    procedure SetTodayText(const Value: TColor);
    procedure Setweekdaybackground(const Value: TColor);
    procedure SetWeekdayText(const Value: TColor);
    procedure SetWeekendBackground(const Value: TColor);
    procedure SetWeekendText(const Value: TColor);
    procedure UpdateController;
  public
    constructor create;
    procedure Assign(Source: TPersistent); override;
    property CustomCalendar : TrmCustomCalendar read fCustomCalendar write fCustomCalendar;
  published
    property WeekdayBackgroundColor : TColor read fWeekdaybackground write Setweekdaybackground default clwindow;
    property WeekdayTextColor : TColor read fWeekdayText write SetWeekdayText default clWindowText;
    property WeekendBackgroundColor : TColor read fWeekendBackground write SetWeekendBackground default $00E1E1E1;
    property WeekendTextColor : TColor  read fWeekendText write SetWeekendText default clTeal;
    property DayNamesBackgroundColor : TColor read fDayNamesBackground write SetDayNamesBackground default clBtnFace;
    property DayNamesTextColor : TColor read fDayNamesText write SetDayNamesText default clBtnText;
    property TodayTextColor : TColor read fTodayText write SetTodayText default clRed;
    property OtherMonthDayTextColor : TColor read fOtherMonthDayText write SetOtherMonthDayText default clBtnFace;
    property OtherMonthDayBackgroundColor : TColor read fOtherMonthDayBackground write SetOtherMonthDayBackground default clWindow;
  end;

  TrmCustomCalendar = class(TCustomPanel)
  private
    { Private declarations }
    fCalendarGrid: TDrawGrid;
    fLabel1: TLabel;
    fShowWeekends: boolean;
    wYear, //Working Year
      wMonth, //Working Month
      wDay, //Working Day
      wfdow, //Working First Day of the Month (index into sun, mon, tue...)
      wdom: word; //Working Days of Month
    fSelectionValid,
      fBoldSysdate: boolean;
    fSelectedDate,
      fMinSelectDate,
      fMaxSelectDate,
      fworkingdate: TDate;
    fOnWorkingDateChange: TNotifyEvent;
    fOnSelectedDateChange: TNotifyEvent;
    fCalendarFont: TFont;
    fUseDateRanges: boolean;
    fOnPaintCell: TPaintCellEvent;
    fCalendarColors: TrmCalendarColors;
    fShowGridLines: boolean;
    procedure setShowWeekends(const Value: boolean);
    procedure SetSelectedDate(const Value: TDate);
    procedure SetWorkingDate(const Value: TDate);
    procedure SetCalendarFont(const Value: TFont);
    procedure SetMaxDate(const Value: TDate);
    procedure SetMinDate(const Value: TDate);
    procedure SetUseDateRanges(const Value: boolean);
    procedure GetRowColInfo(wDate: TDate; var Row, Col: integer);
    function CheckDateRange(wDate: TDate): TDate;

    function ValidateDOW(row, col: integer; var daynumber: integer): boolean;
    function MyEncodeDate(year, month, day: word): TDateTime;
    function CurrentDateValue(Value: TCurrentDateValue): word;

    procedure wmSize(var Msg: TMessage); message WM_Size;
    procedure wmEraseBkgrnd(var Msg:TMessage); message WM_EraseBkgnd;
    procedure CalendarSelectDate(Sender: TObject; Col, Row: Integer;
      var CanSelect: Boolean);
    procedure SetBoldSystemDate(const Value: boolean);
    function GetCellCanvas: TCanvas;
    procedure SetCalendarColors(const Value: TrmCalendarColors);
    procedure SetGridLines(const Value: boolean);
  protected
    { Protected declarations }
    procedure SetCellSizes;
    procedure PaintCalendarCell(Sender: TObject; Col, Row: Longint; Rect: TRect; State: TGridDrawState);

    procedure CalendarDblClick(Sender: TObject); virtual;
    procedure CalendarGridKeyPress(Sender: TObject; var Key: Char); virtual;
    procedure CalendarKeyMovement(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;

    property ShowGridLines : boolean read fShowGridLines write SetGridLines default false;
    property CalendarColors : TrmCalendarColors read fCalendarColors write SetCalendarColors;
    property BoldSystemDate : boolean read fboldsysdate write SetBoldSystemDate default true;
    property UseDateRanges: boolean read fUseDateRanges write SetUseDateRanges default false;
    property MinDate: TDate read fMinSelectDate write SetMinDate;
    property MaxDate: TDate read fMaxSelectDate write SetMaxDate;
    property CalendarFont: TFont read fCalendarFont write SetCalendarFont;
    property SelectedDate: TDate read fSelectedDate write SetSelectedDate;
    property WorkingDate: TDate read fworkingdate;
    property ShowWeekends: boolean read fShowWeekends write SetShowWeekends default true;
    property OnWorkingDateChange: TNotifyEvent read fOnWorkingDateChange write fOnWorkingDateChange;
    property OnSelectedDateChange: TNotifyEvent read fOnSelectedDateChange write fOnSelectedDateChange;
    property OnPaintCell : TPaintCellEvent read fOnPaintCell write fOnPaintCell;
  public
    { Public declarations }
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    property CellCanvas : TCanvas read GetCellCanvas;
    procedure NextMonth;
    procedure PrevMonth;
    procedure NextYear;
    procedure PrevYear;
    function GoodCalendarSize(NewWidth:integer): boolean;
    procedure Invalidate; override;
  end;

  TrmCalendar = class(TrmCustomCalendar)
  public
    { Public declarations }
  published
    { Published declarations }
    property Align;
    property Anchors;
    property BorderStyle default bsSingle;
    property BevelInner default bvNone;
    property BevelOuter default bvNone;

    property CalendarColors;
    property CalendarFont;
    property SelectedDate;
    property WorkingDate;
    property ShowWeekends;
    property UseDateRanges;
    property MinDate;
    property MaxDate;
    property ShowGridLines;
    property OnWorkingDateChange;
    property OnSelectedDateChange;
    property OnPaintCell;
  end;

  TrmScreenCalendar = class(TrmCalendar)
  private
    { Private declarations }
    fPanel: TPanel;
    fBtn1, fBtn2: TSpeedButton;
    LastState: Word;
    fmsg: TrmMsgEvent;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
  protected
    { Protected declarations }
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure VisibleChanging; override;
    procedure DoBtnClick(Sender: TObject);
    procedure DoPanelSize(Sender: TObject);

    procedure UpdateBtns;
  public
    { Public declarations }
    constructor create(AOwner: TComponent); override;

    procedure CalendarKeyMovement(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure SetFocus; override;
    procedure HandleMessage(var msg: TMessage);
    procedure WndProc(var Message: TMessage); override;
  published
    { Published declarations }
  end;

  TrmCustomComboCalendar = class(TrmCustomBtnEdit)
  private
  { Private declarations }
    fCalendar: TrmScreenCalendar;
    fSelectedDate: TDate;
    fDateFormat: string;
    fDropDownWidth: integer;
    fmsg: TrmMsgEvent;
    procedure SetDate(value: TDate);
    procedure SetDateFormat(value: string);
    function GetDate: TDate;
    procedure SetSelectDate(Sender: TObject);
    procedure ToggleCalendar(Sender: TObject);
    procedure DoMyExit(Sender: Tobject);
    function GetCalendar: TrmCalendar;
    procedure wmKillFocus(var Message: TMessage); message wm_killfocus;
    procedure SetDropDownWidth(const Value: integer);
  protected
    { Protected declarations }
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    property SelectedDate: TDate read GetDate write SetDate;
    property DateFormat: string read fDateformat write SetDateFormat;
    property DropDownWidth: integer read fDropDownWidth write SetDropDownWidth default 180;
    property Calendar: TrmCalendar read GetCalendar;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WndProc(var Message: TMessage); override;
  end;

  TrmComboCalendar = class(TrmCustomComboCalendar)
  published
    { Published declarations }
    property SelectedDate;
    property DateFormat;

{$IFDEF D4_OR_HIGHER}
    property Anchors;
    property Constraints;
{$ENDIF}
    property AutoSelect;
    property AutoSize;
    property BtnWidth;
    property BorderStyle;
    property Calendar;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property DropDownWidth;
    property EditorEnabled;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation

uses
  rmSpeedBtns;

{ TrmCustomCalendar }

constructor TrmCustomCalendar.create(AOwner: TComponent);
begin
  inherited create(AOwner);

  ControlStyle := ControlStyle + [csOpaque]; 

  fCalendarColors := TrmCalendarColors.create;
  fCalendarColors.CustomCalendar := self;

  BorderWidth := 1;
  BorderStyle := bsSingle;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  width := 205;
  height := 158;
  fUseDateRanges := false;
  fMinSelectDate := Now - 365; //Default it to be 1 year back
  fMaxSelectDate := Now + 365; //Default it to be 1 year ahead
  fBoldSysDate := true;

  fCalendarFont := tfont.create;
  fCalendarFont.assign(self.font);

  fLabel1 := TLabel.create(self);
  with fLabel1 do
  begin
    ParentFont := false;
    Parent := self;
    Align := alTop;
    Caption := MonthOfYear[CurrentDateValue(cdvMonth)] + ' ' + inttostr(CurrentDateValue(cdvYear));
    Alignment := taCenter;
    Font.Size := self.font.size + 4;
    Font.Style := [fsBold];
    TabStop := false;
  end;

  fShowGridLines := false;

  fCalendarGrid := TDrawGrid.Create(self);
  with FCalendarGrid do
  begin
    ParentFont := false;
    Parent := self;
    ControlStyle := ControlStyle - [csDesignInteractive];
    Align := alClient;
    BorderStyle := bsNone;
    ColCount := 7;
    FixedCols := 0;
    fixedRows := 0;
    RowCount := 7;
    ScrollBars := ssNone;
    Options := [];
    OnDrawCell := PaintCalendarCell;
    OnSelectCell := CalendarSelectDate;
    OnDblClick := CalendarDblClick;
    OnKeyPress := CalendarGridKeyPress;
    OnKeyDown := CalendarKeyMovement;
  end;

  fShowWeekends := true;

  SetCellSizes;
  SelectedDate := Now;
end;

destructor TrmCustomCalendar.destroy;
begin
  fCalendarGrid.free;
  fLabel1.free;
  fCalendarFont.free;
  fCalendarColors.free;
  inherited;
end;

procedure TrmCustomCalendar.PaintCalendarCell(Sender: TObject; Col, Row: Integer;
  Rect: TRect; State: TGridDrawState);
var
  TextToPaint: string;
  xpos, ypos, wdom, Daynumber: integer;
  NewDayNumber: integer;
  wPaintDate : boolean;
  wPDate : TDate;
begin
  wPDate := now;  //useless, only required to get past a compiler warning in D6
  wPaintDate := false;
  case row of
    0:
      begin
        fCalendarGrid.canvas.brush.color := fCalendarColors.DayNamesBackgroundColor; //clbtnface;
        if ((col = 0) or (col = 6)) and fShowWeekends then
          fCalendarGrid.canvas.font.color := fCalendarColors.WeekendTextColor//fWeekendColor
        else
          fCalendarGrid.canvas.font.color := fCalendarColors.DayNamesTextColor;//clbtntext;
        TextToPaint := WeekDay[col + 1];
      end;
  else
    begin
      if ValidateDOW(row, col, DayNumber) then
      begin
        if (gdFocused in state) then fSelectionValid := true;
        TextToPaint := inttostr(DayNumber);
        wPaintDate := true;
        wPDate := encodeDate(wyear, wmonth, DayNumber);
        if (gdSelected in state) then
        begin
          fCalendarGrid.canvas.font.color := clHighlightText;
          fCalendarGrid.canvas.brush.color := clHighlight;
          fworkingdate := MyEncodeDate(wYear, wMonth, DayNumber);
        end
        else
        begin
          if (((col = 0) or (col = 6)) and fShowWeekends) then
          begin
            fCalendarGrid.canvas.font.color := fCalendarColors.WeekendTextColor; //fWeekendColor;
            fCalendarGrid.canvas.brush.color := fCalendarColors.WeekendBackgroundColor;//fweekendBkColor;
          end
          else
          begin
            fCalendarGrid.canvas.font.color := fCalendarColors.WeekdayTextColor;//clWindowText;
            fCalendarGrid.canvas.brush.color := fCalendarColors.WeekdayBackgroundColor;//clwindow;
          end;
        end;
      end
      else
      begin
        fCalendarGrid.canvas.font.color := fCalendarColors.OtherMonthDayTextColor; //clBtnFace;
        fCalendarGrid.canvas.brush.color := fCalendarColors.OtherMonthDayBackgroundColor;//clWindow;
        wdom := DaysOfMonth[wmonth];
        if (IsLeapYear(wyear)) and (wmonth = 2) then inc(wdom);
        if daynumber > wdom then
        begin
          NewDayNumber := daynumber - wdom;
          if NewDayNumber > wdom then
          begin
            fCalendarGrid.canvas.brush.color := clInactiveCaption;
            fCalendarGrid.canvas.brush.style := bsDiagCross;
            fCalendarGrid.canvas.pen.Style := psClear;
            fCalendarGrid.canvas.Rectangle(rect.left, rect.Top, rect.right + 1, rect.bottom + 1);
            fCalendarGrid.canvas.brush.style := bsClear;
            NewDayNumber := DayNumber;
          end;
        end
        else
        begin
          if (wmonth = 3) and IsLeapYear(wyear) then
          begin
            NewDayNumber := (daynumber + DaysOfMonth[wmonth - 1] + 1);
            if (NewDayNumber > DaysOfMonth[wmonth - 1] + 1) then
            begin
              fCalendarGrid.canvas.brush.color := clInactiveCaption;
              fCalendarGrid.canvas.brush.style := bsDiagCross;
              fCalendarGrid.canvas.pen.Style := psClear;
              fCalendarGrid.canvas.Rectangle(rect.left, rect.Top, rect.right + 1, rect.bottom + 1);
              fCalendarGrid.canvas.brush.style := bsClear;
              NewDayNumber := DayNumber;
            end;
          end
          else
          begin
            NewDayNumber := (daynumber + DaysOfMonth[wmonth - 1]);
            if (NewDayNumber > DaysOfMonth[wmonth - 1]) then
            begin
              fCalendarGrid.canvas.brush.color := clInactiveCaption;
              fCalendarGrid.canvas.brush.style := bsDiagCross;
              fCalendarGrid.canvas.pen.Style := psClear;
              fCalendarGrid.canvas.Rectangle(rect.left, rect.Top, rect.right + 1, rect.bottom + 1);
              fCalendarGrid.canvas.brush.style := bsClear;
              NewDayNumber := DayNumber;
            end;
          end;
        end;

        TextToPaint := inttostr(NewDayNumber);
      end;
      if (CurrentDateValue(cdvYear) = wyear) and (CurrentDateValue(cdvMonth) = wmonth) and (CurrentDateValue(cdvDay) = daynumber) then
      begin
         if (fboldsysdate) then
           fCalendarGrid.canvas.font.Style := [fsBold]
         else
           fCalendarGrid.canvas.font.Style := [];
         if not (gdSelected in state) then
            fCalendarGrid.Canvas.Font.Color := fCalendarColors.TodayTextColor;
      end;
    end;
  end;
  xpos := rect.Left + ((rect.right - rect.left) shr 1) - (fCalendarGrid.canvas.textwidth(TextToPaint) shr 1);
  ypos := rect.Top + ((rect.bottom - rect.top) shr 1) - (fCalendarGrid.canvas.textheight(TextToPaint) shr 1);

  if TextToPaint <> '' then
     fCalendarGrid.canvas.TextRect(rect, xpos, ypos, TextToPaint);

  if wPaintDate and assigned(fonPaintCell) then
     fOnPaintCell(Sender, Col, Row, Rect, State, wPDate);
end;

procedure TrmCustomCalendar.SetCellSizes;
var
  loop: integer;
  h, w: integer;
  mh, mw: integer;
begin
  h := fCalendarGrid.Height div 7;
  mh := fCalendarGrid.Height mod 7;
  w := fCalendarGrid.Width div 7;
  mw := fCalendarGrid.Width mod 7;

  for loop := 0 to 6 do
  begin
    if mw > 0 then
    begin
      dec(mw);
      if fShowGridLines then
         fCalendarGrid.ColWidths[loop] := w
      else
         fCalendarGrid.ColWidths[loop] := w + 1;
    end
    else
      fCalendarGrid.ColWidths[loop] := w;

    if mh > 0 then
    begin
      dec(mh);
      if fShowGridLines then
         fCalendarGrid.RowHeights[loop] := h
      else
         fCalendarGrid.RowHeights[loop] := h + 1;
    end
    else
      fCalendarGrid.RowHeights[loop] := h;
  end;
end;

procedure TrmCustomCalendar.SetShowWeekends(const Value: boolean);
begin
  fShowWeekends := value;
  fCalendarGrid.invalidate;
end;

procedure TrmCustomCalendar.wmSize(var Msg: TMessage);
begin
  inherited;
  SetCellSizes;
end;

function TrmCustomCalendar.ValidateDOW(row, col: integer;
  var daynumber: integer): boolean;
begin
  daynumber := ((col + ((row - 1) * 7)) - wfdow) + 2;
  if (daynumber >= 1) and (daynumber <= wdom) then
    result := true
  else
    result := false;

  if result and fUseDateRanges then
  begin
    result := (MyEncodeDate(wYear, wMonth, daynumber) >= fMinSelectDate) and
      (MyEncodeDate(wYear, wMonth, daynumber) <= fMaxSelectDate);
  end;
end;

function TrmCustomCalendar.MyEncodeDate(year, month, day: word): TDateTime;
begin
  if day > DaysOfMonth[month] then
  begin
    if (month = 2) and IsLeapYear(year) and (day >= 29) then
      day := 29
    else
      day := DaysOfMonth[month];
  end;
  result := encodedate(year, month, day);
end;

function TrmCustomCalendar.CurrentDateValue(
  Value: TCurrentDateValue): word;
var
  y, m, d: word;
begin
  decodeDate(Now, y, m, d);
  case value of
    cdvYear: result := y;
    cdvMonth: result := m;
    cdvDay: result := d;
  else
    raise exception.create('Unknown parameter');
  end;
end;

procedure TrmCustomCalendar.SetSelectedDate(const Value: TDate);
var
  row, col: integer;
begin
  fSelectedDate := CheckDateRange(value);

  GetRowColInfo(fSelectedDate, row, Col);
  DecodeDate(fSelectedDate, wYear, wMonth, wDay);
  wdom := DaysOfMonth[wmonth];
  wfdow := DayOfWeek(MyEncodeDate(wyear, wmonth, 1));
  if (isleapyear(wyear)) and (wmonth = 2) then inc(wdom);

  fLabel1.Caption := MonthOfYear[wMonth] + ' ' + inttostr(wYear);

  fCalendarGrid.Selection := TGridRect(rect(col, row, col, row));
  fCalendarGrid.Invalidate;

  if fworkingdate <> fSelectedDate then
  begin
    fworkingdate := fSelectedDate;
    if assigned(fOnWorkingDateChange) then
      fOnWorkingDateChange(self);
  end;

  if assigned(fOnSelectedDateChange) then
    fOnSelectedDateChange(self);
end;

procedure TrmCustomCalendar.CalendarDblClick(Sender: TObject);
begin
  if fSelectionValid then
    SetSelectedDate(fWorkingDate);
end;

procedure TrmCustomCalendar.CalendarGridKeyPress(Sender: TObject;
  var Key: Char);
begin
  if (key = #13) and fSelectionValid then
    SetSelectedDate(fWorkingDate);
end;

procedure TrmCustomCalendar.CalendarKeyMovement(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  sday, smonth, syear: word;
  dummy: boolean;
  row, col: integer;
begin
  fCalendarGrid.setfocus;
  if key in [vk_left, vk_right, vk_up, vk_down] then
    decodedate(fworkingdate, syear, smonth, sday);
  case key of
    vk_Left:
      begin
        if ssCtrl in Shift then
        begin
          PrevMonth;
          Key := 0;
        end
        else
        begin
          if (fCalendarGrid.col - 1 = -1) then
          begin
            if sDay - 1 >= 1 then
            begin
              GetRowColInfo(MyEncodeDate(sYear, sMonth, sDay - 1), Row, Col);
              CalendarSelectDate(self, Col, Row, dummy);
            end;
            Key := 0;
          end;
        end;
      end;
    vk_Right:
      begin
        if ssCtrl in Shift then
        begin
          NextMonth;
          Key := 0;
        end
        else
        begin
          if (fCalendarGrid.col + 1 = 7) then
          begin
            if sDay + 1 <= wdom then
            begin
              GetRowColInfo(MyEncodeDate(sYear, sMonth, sDay + 1), Row, Col);
              CalendarSelectDate(self, Col, Row, dummy);
            end;
            Key := 0;
          end;
        end;
      end;
    vk_Up:
      begin
        if ssCtrl in Shift then
        begin
          PrevYear;
          key := 0;
        end
        else
        begin
        end;
      end;
    vk_Down:
      begin
        if ssCtrl in Shift then
        begin
          NextYear;
          key := 0;
        end
        else
        begin
        end;
      end;
  end;
end;

procedure TrmCustomCalendar.CalendarSelectDate(Sender: TObject; Col,
  Row: Integer; var CanSelect: Boolean);
var
  day: integer;
begin
  canselect := ValidateDOW(row, col, day);
  if canselect then
    SetWorkingDate(MyEncodeDate(wyear, wmonth, day));
end;

procedure TrmCustomCalendar.SetCalendarFont(const Value: TFont);
begin
  fCalendarFont.assign(value);
  fCalendarGrid.font.assign(fCalendarFont);
  fLabel1.font.assign(fCalendarFont);
  fLabel1.Font.size := fLabel1.Font.size + 4;
  fLabel1.Font.Style := fLabel1.Font.Style + [fsBold];
end;

procedure TrmCustomCalendar.SetMaxDate(const Value: TDate);
var
  wDate: TDate;
begin
  wDate := trunc(value);
  if wDate <> fMaxSelectDate then
  begin
    if wDate <= fMinSelectDate then
      raise Exception.Create('MaxDate value can''t be less than or equal to the MinDate value');
    fMaxSelectDate := wDate;
    if UseDateRanges and (SelectedDate > fMaxSelectDate) then
      SelectedDate := fMaxSelectDate;
    fCalendarGrid.Invalidate;
  end;
end;

procedure TrmCustomCalendar.SetMinDate(const Value: TDate);
var
  wDate: TDate;
begin
  wDate := trunc(value);
  if wDate <> fMinSelectDate then
  begin
    if wDate >= fMaxSelectDate then
      raise Exception.Create('MinDate value can''t be greater than or equal to the MaxDate value');
    fMinSelectDate := wDate;
    if UseDateRanges and (SelectedDate < fMinSelectDate) then
      SelectedDate := fMinSelectDate;
    fCalendarGrid.Invalidate;
  end;
end;

procedure TrmCustomCalendar.SetUseDateRanges(const Value: boolean);
begin
  if value <> fUseDateRanges then
  begin
    fUseDateRanges := Value;

    if fUseDateRanges then
    begin
      if SelectedDate < fMinSelectDate then
        SelectedDate := fMinSelectDate;

      if SelectedDate > fMaxSelectDate then
        SelectedDate := fMaxSelectDate;
    end;
    fCalendarGrid.Invalidate;
  end;
end;

procedure TrmCustomCalendar.NextMonth;
var
  sday, smonth, syear: word;
begin
  decodedate(fworkingdate, syear, smonth, sday);
  inc(sMonth);
  if sMonth > 12 then
  begin
    sMonth := 1;
    inc(sYear);
  end;
  SetWorkingDate(MyEncodeDate(sYear, sMonth, sDay));
end;

procedure TrmCustomCalendar.NextYear;
var
  sday, smonth, syear: word;
begin
  decodedate(fworkingdate, syear, smonth, sday);
  SetWorkingDate(MyEncodeDate(sYear + 1, sMonth, sDay));
end;

procedure TrmCustomCalendar.PrevMonth;
var
  sday, smonth, syear: word;
begin
  decodedate(fworkingdate, syear, smonth, sday);

  dec(sMonth);
  if sMonth < 1 then
  begin
    sMonth := 12;
    dec(sYear);
  end;

  SetWorkingDate(MyEncodeDate(sYear, sMonth, sDay));
end;

procedure TrmCustomCalendar.PrevYear;
var
  sday, smonth, syear: word;
begin
  decodedate(fworkingdate, syear, smonth, sday);
  SetWorkingDate(MyEncodeDate(sYear - 1, sMonth, sDay));
end;

procedure TrmCustomCalendar.GetRowColInfo(wDate: TDate; var Row,
  Col: integer);
var
  wyear, wmonth, wday: word;
  wfdow: integer;
begin
  decodedate(wDate, wYear, wMonth, wDay);
  wfdow := DayOfWeek(MyEncodeDate(wyear, wmonth, 1));
  row := (((wday - 2) + wfdow) div 7) + 1;
  col := (((wday - 2) + wfdow) mod 7);
end;

function TrmCustomCalendar.CheckDateRange(wDate: TDate): TDate;
begin
  if fUseDateRanges then
  begin
    result := trunc(wDate);

    if (result < fMinSelectDate) then
      result := fMinSelectDate;

    if (result > fMaxSelectDate) then
      result := fMaxSelectDate;
  end
  else
    result := trunc(wDate);
end;

procedure TrmCustomCalendar.SetWorkingDate(const Value: TDate);
var
  row, col: integer;
begin
  fworkingdate := CheckDateRange(value);

  GetRowColInfo(fWorkingDate, row, col);

  DecodeDate(fworkingdate, wYear, wMonth, wDay);
  wdom := DaysOfMonth[wmonth];
  wfdow := DayOfWeek(MyEncodeDate(wyear, wmonth, 1));
  if (isleapyear(wyear)) and (wmonth = 2) then inc(wdom);

  fLabel1.Caption := MonthOfYear[wMonth] + ' ' + inttostr(wYear);

  fCalendarGrid.Selection := TGridRect(rect(col, row, col, row));
  fCalendarGrid.Invalidate;
  if assigned(fOnWorkingDateChange) then
    fOnWorkingDateChange(self);
end;

procedure TrmCustomCalendar.SetBoldSystemDate(const Value: boolean);
begin
  fboldsysdate := Value;
  invalidate;
end;

function TrmCustomCalendar.GetCellCanvas: TCanvas;
begin
   result := fCalendarGrid.Canvas;
end;

procedure TrmCustomCalendar.SetCalendarColors(
  const Value: TrmCalendarColors);
begin
  fCalendarColors.Assign(Value);
  fCalendarGrid.Invalidate;
end;

procedure TrmCustomCalendar.SetGridLines(const Value: boolean);
begin
  if fShowGridLines <> Value then
  begin
     fShowGridLines := Value;
     if fShowGridLines then
        fCalendarGrid.Options := [goVertLine, goHorzLine]
     else
        fCalendarGrid.Options := [];
     SetCellSizes;
  end;
end;

function TrmCustomCalendar.GoodCalendarSize(NewWidth:integer): boolean;
begin
   Result := (NewWidth mod 7) = 0;
end;

procedure TrmCustomCalendar.wmEraseBkgrnd(var Msg: TMessage);
begin
   msg.result := 1;
end;

procedure TrmCustomCalendar.Invalidate;
begin
  if fCalendarGrid <> nil then
     fCalendarGrid.Invalidate;
  inherited;
end;

{ TrmCustomComboCalendar }

constructor TrmCustomComboCalendar.Create(AOwner: TComponent);
begin
  inherited create(aowner);

  OnBtn1Click := ToggleCalendar;
  OnExit := DoMyExit;
  readonly := true;
  fDateformat := 'mm/dd/yyyy';
  fDropDownWidth := 180;

  UseDefaultGlyphs := false;

  with GetButton(1) do
  begin
    Font.name := 'Marlett';
    font.size := 10;
    Font.color := clBtnText;
    Caption := '6';
    Glyph := nil;
  end;

  if not (csdesigning in componentstate) then
  begin
    fCalendar := TrmScreenCalendar.create(owner);
    with fCalendar do
    begin
      parent := self;
      width := self.width;
      visible := false;
      OnSelectedDateChange := SetSelectDate;
    end;
  end;

  SelectedDate := Now;
end;

destructor TrmCustomComboCalendar.Destroy;
begin
  fCalendar.free;
  inherited destroy;
end;

function TrmCustomComboCalendar.GetDate: TDate;
begin
  result := fSelectedDate;
end;

procedure TrmCustomComboCalendar.SetDate(value: TDate);
begin
  if trunc(value) <> fSelectedDate then
     fSelectedDate := trunc(value);
  Text := formatdatetime(fDateFormat, fSelectedDate);
end;

procedure TrmCustomComboCalendar.SetDateFormat(value: string);
begin
  if value = '' then value := 'mm/dd/yyyy';
  fDateFormat := value;
  text := formatdatetime(fDateFormat, SelectedDate);
end;

procedure TrmCustomComboCalendar.SetSelectDate(Sender: TObject);
var
  wVisible: boolean;
begin
  wVisible := fCalendar.visible;
  if fCalendar.visible then
    fCalendar.Hide;
  SelectedDate := fCalendar.WorkingDate;
  if wVisible and Self.CanFocus then
    self.setfocus;
end;

procedure TrmCustomComboCalendar.ToggleCalendar(Sender: TObject);
var
  CP, SP: TPoint;
begin
  CP.X := Left;
  CP.Y := Top + Height;
  SP := parent.ClientToScreen(CP);

  SetFocus;
  SelectAll;

  with fCalendar do
  begin
    if fDropDownWidth = 0 then
      Width := self.width
    else
      width := fDropDownWidth;
    fCalendar.SelectedDate := self.SelectedDate;

    Left := SP.X;
    if assigned(screen.ActiveForm) then
    begin
      if (SP.Y + fCalendar.height < screen.activeForm.Monitor.Height) then
        fCalendar.Top := SP.Y
      else
        fCalendar.Top := (SP.Y - self.height) - fCalendar.height;
    end
    else
    begin
      if (SP.Y + fCalendar.height < screen.Height) then
        fCalendar.Top := SP.Y
      else
        fCalendar.Top := (SP.Y - self.height) - fCalendar.height;
    end;
    Show;
    SetWindowPos(handle, hwnd_topMost, 0, 0, 0, 0, swp_nosize or swp_NoMove);
  end; { Calendar }
end;

procedure TrmCustomComboCalendar.WndProc(var Message: TMessage);
begin
  if assigned(fmsg) then
  try
    fmsg(message);
  except
  end;

  case Message.Msg of
    WM_CHAR,
      WM_KEYDOWN,
      WM_KEYUP:
      if fCalendar.visible then
      begin
        fcalendar.HandleMessage(message);
        if message.result = 0 then exit;
      end;
  end;
  inherited WndProc(Message);
end;

procedure TrmCustomComboCalendar.DoMyExit(Sender: Tobject);
begin
  if fCalendar.visible then
    fCalendar.Hide;
end;

procedure TrmCustomComboCalendar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if ((Key = VK_DOWN) and (ssAlt in Shift)) or
    ((key = VK_F4) and (shift = [])) then
    ToggleCalendar(self)
  else
    inherited KeyDown(Key, Shift);
end;

function TrmCustomComboCalendar.GetCalendar: TrmCalendar;
begin
  result := fCalendar;
end;

procedure TrmCustomComboCalendar.wmKillFocus(var Message: TMessage);
begin
  inherited;
  if fCalendar.visible then
    fCalendar.Hide;
end;

procedure TrmCustomComboCalendar.SetDropDownWidth(const Value: integer);
begin
  if (value <> fDropDownWidth) and (value >= 180) then
  begin
     fDropDownWidth := Value;
  end;
end;

{ TrmScreenCalendar }

constructor TrmScreenCalendar.create(AOwner: TComponent);
begin
  inherited create(Aowner);
  BorderWidth := 0;
  fPanel := TPanel.create(self);
  LastState := 0;
  with fPanel do
  begin
    Parent := self;
    Align := alBottom;
    fBtn1 := TSpeedButton.create(self);
    with fBtn1 do
    begin
      Parent := fPanel;
      Flat := true;
      Font.Name := 'Small Font';
      Font.Size := 7;
      font.color := clbtnText;
      Tag := 1;
      OnClick := DoBtnClick;
    end;
    fBtn2 := TSpeedButton.create(self);
    with fBtn2 do
    begin
      Parent := fPanel;
      Flat := true;
      Font.Name := 'Small Font';
      Font.Size := 7;
      font.color := clbtnText;
      Tag := 2;
      OnClick := DoBtnClick;
    end;
    OnResize := DoPanelSize;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    Caption := '';
    Height := 20;
  end;
  UpdateBtns;
end;

procedure TrmScreenCalendar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TrmScreenCalendar.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

procedure TrmScreenCalendar.SetFocus;
begin
  inherited;
  fCalendarGrid.SetFocus;
end;

procedure TrmScreenCalendar.VisibleChanging;
begin
  if Visible = false then
    SetCaptureControl(self)
  else
    ReleaseCapture;
  inherited;
end;

procedure TrmScreenCalendar.HandleMessage(var msg: TMessage);
begin
  UpdateBtns;
  msg.result := SendMessage(fCalendarGrid.Handle, msg.msg, msg.wparam, msg.lparam);
end;

procedure TrmScreenCalendar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  SetCaptureControl(Self);
end;

procedure TrmScreenCalendar.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if not ptInRect(clientrect, point(message.xpos, message.ypos)) then
    Visible := false;
  inherited;
end;

procedure TrmScreenCalendar.WMMouseMove(var Message: TWMMouse);
begin
  if ptInRect(clientrect, point(message.xpos, message.ypos)) then
    ReleaseCapture;
  inherited;
end;

procedure TrmScreenCalendar.CalendarKeyMovement(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case key of
    VK_ESCAPE:
      begin
        key := 0;
        visible := false;
        if owner is TWinControl then
          TWinControl(owner).setfocus;
      end;
  else
    inherited CalendarKeyMovement(sender, key, shift);
  end;
end;

procedure TrmScreenCalendar.WndProc(var Message: TMessage);
begin
  if assigned(fmsg) then
  try
    fmsg(message);
  except
  end;

  case Message.Msg of
    WM_CaptureKeyDown:
      begin
        Message.msg := wm_KeyDown;
      end;
    WM_CaptureKeyup:
      begin
        Message.msg := wm_KeyUp;
      end;
  end;
  inherited WndProc(Message);
end;

procedure TrmScreenCalendar.DoBtnClick(Sender: TObject);
var
  state: Word;
begin
  if Sender is TSpeedButton then
  begin
    state := GetKeyState(vk_control);
    case TSpeedButton(Sender).Tag of
      1:
        begin
          if (state and $8000 <> 0) then
            PrevYear
          else
            PrevMonth;

          UpdateBtns;
        end;
      2:
        begin
          if (state and $8000 <> 0) then
            NextYear
          else
            NextMonth;

          UpdateBtns;
        end;
    else
              //This should never happen.....
    end;
  end;
end;

procedure TrmScreenCalendar.DoPanelSize(Sender: TObject);
begin
  fBtn1.SetBounds(2, 2, 90, 16);
  fBtn2.SetBounds(fPanel.Width - 92, 2, 90, 16);
end;

procedure TrmScreenCalendar.UpdateBtns;
var
  state: short;
  y, m, d : word;
begin
  state := GetKeyState(vk_control);
  DecodeDate(fworkingdate, y, m, d);
  if (state and $8000 <> 0) then
  begin
    fBtn1.Caption := monthofyear[m] +' '+ IntToStr(y-1);
    fBtn2.Caption := monthofyear[m] +' '+ inttostr(y+1);
  end
  else
  begin
    if m-1 = 0 then
       fBtn1.Caption := monthofyear[m-1] +' '+ IntToStr(y-1)
    else
       fBtn1.Caption := monthofyear[m-1] +' '+ IntToStr(y);

    if m+1 = 13 then
       fBtn2.Caption := monthofyear[m+1] +' '+ IntToStr(y+1)
    else
       fBtn2.Caption := monthofyear[m+1] +' '+ IntToStr(y);
  end;
end;

{ TrmCalendarColors }

procedure TrmCalendarColors.Assign(Source: TPersistent);
var
   wColors : TrmCalendarColors;
begin
  inherited;
  if source is TrmCalendarColors then
  begin
     wColors := TrmCalendarColors(source);
     fWeekendText := wColors.WeekdayTextColor;
     fWeekdaybackground := wColors.WeekdayBackgroundColor;
     fDayNamesBackground := wColors.DayNamesBackgroundColor;
     fDayNamesText := wColors.DayNamesTextColor;
     fWeekdayText := wColors.WeekdayTextColor;
     fTodayText := wColors.TodayTextColor;
     fWeekendBackground := wColors.WeekendBackgroundColor;
     fOtherMonthDayBackground := wColors.OtherMonthDayBackgroundColor;
     fOtherMonthDayText := wColors.OtherMonthDayTextColor;
     
     UpdateController;
  end;
end;

constructor TrmCalendarColors.create;
begin
  inherited;
  fWeekendText := clTeal;
  fWeekdaybackground := clWindow;
  fDayNamesBackground := clBtnFace;
  fDayNamesText := clBtnText;
  fWeekdayText := clWindowText;
  fTodayText := clRed;
  fWeekendBackground := $00E1E1E1;
  fOtherMonthDayBackground := clWindow;
  fOtherMonthDayText := clBtnFace;
end;

procedure TrmCalendarColors.SetDayNamesBackground(const Value: TColor);
begin
  if fDayNamesBackground <> Value then
  begin
     fDayNamesBackground := Value;
     fCustomCalendar.fLabel1.Color := Value;
     UpdateController;
  end;
end;

procedure TrmCalendarColors.SetDayNamesText(const Value: TColor);
begin
  if fDayNamesText <> Value then
  begin
     fDayNamesText := Value;
     fCustomCalendar.fLabel1.Font.color := Value;
     UpdateController;
  end;
end;

procedure TrmCalendarColors.SetOtherMonthDayBackground(const Value: TColor);
begin
  if (fOtherMonthDayBackground <> value) then
  begin
     fOtherMonthDayBackground := value;
     UpdateController;
  end;
end;

procedure TrmCalendarColors.SetOtherMonthDayText(const Value: TColor);
begin
  if fOtherMonthDayText <> Value then
  begin
     fOtherMonthDayText := Value;
     UpdateController;
  end;
end;

procedure TrmCalendarColors.SetTodayText(const Value: TColor);
begin
  if fTodayText <> Value then
  begin
     fTodayText := Value;
     UpdateController;
  end;
end;

procedure TrmCalendarColors.Setweekdaybackground(const Value: TColor);
begin
  if fWeekdaybackground <> Value then
  begin
     fWeekdaybackground := Value;
     UpdateController;
  end;
end;

procedure TrmCalendarColors.SetWeekdayText(const Value: TColor);
begin
  if fWeekdayText <> Value then
  begin
     fWeekdayText := Value;
     UpdateController;
  end;
end;

procedure TrmCalendarColors.SetWeekendBackground(const Value: TColor);
begin
  if fWeekendBackground <> Value then
  begin
     fWeekendBackground := Value;
     UpdateController;
  end;
end;

procedure TrmCalendarColors.SetWeekendText(const Value: TColor);
begin
  if fWeekendText <> Value then
  begin
     fWeekendText := Value;
     UpdateController;
  end;
end;

procedure TrmCalendarColors.UpdateController;
begin
   if assigned(fCustomCalendar) then
      fcustomCalendar.fCalendarGrid.Invalidate;
end;

end.

