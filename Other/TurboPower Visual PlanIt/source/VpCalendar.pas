{*********************************************************}
{*                 VPCALENDAR.PAS 1.03                   *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I Vp.INC}

unit VpCalendar;
  {-Calendar component}
                            
interface

uses
  Windows, Buttons, Classes, Controls, Forms, Graphics, Menus, Messages,
  SysUtils, VpBase, VpSR, VpConst, VpMisc, VpBaseDS, VpCanvasUtils,
  VpException;

type
  TVpCalDisplayOption = (cdoShortNames, cdoShowYear, cdoShowInactive,
                         cdoShowRevert, cdoShowToday, cdoShowNavBtns,
                         cdoHideActive, cdoHighlightSat, cdoHighlightSun);

  TVpCalDisplayOptions = set of TVpCalDisplayOption;

  TVpCalColorArray = array[0..6] of TColor;

  TVpCalColorScheme = (cscalCustom, cscalClassic, cscalWindows,
                       cscalGold, cscalOcean, cscalRose);

  TVpCalSchemeArray = array[TVpCalColorScheme] of TVpCalColorArray;

  TRowArray = array[0..8] of Integer;
  TColArray = array[0..6] of Integer;

const
  {ActiveDay, DayNames, Days, InactiveDays, MonthAndYear, Weekend}
  CalScheme : TVpCalSchemeArray =
    ((0, 0, 0, 0, 0, 0, 0),
     (clHighlight, clWindow, clWindow,  clWindow, clWindow, clWindow, clBlack),
     (clRed,       clMaroon, clBlack,   clGray,   clBlue,   clRed, clBlack),
     (clBlack,     clBlack,  clYellow,  clGray,   clBlack,  clTeal, clBlack),
     (clBlack,     clBlack,  clAqua,    clGray,   clBlack,  clNavy, clBlack),
     (clRed,       clRed,    clFuchsia, clGray,   clBlue,   clTeal, clBlack)
    );
  calDefWeekStarts = dtSunday;{ default start of the week               }

type
  TVpCalColors = class(TPersistent)
  protected {private}
    {property variables}
    FUpdating     : Boolean;
    FOnChange     : TNotifyEvent;

    {internal variables}
    SettingScheme : Boolean;

    {property methods}
    function GetColor(Index : Integer) : TColor;
    procedure SetColor(Index : Integer; Value : TColor);
    procedure SetColorScheme(Value : TVpCalColorScheme);

    {internal methods}
    procedure DoOnChange;

  public
    {public property variables}
    FCalColors    : TVpCalColorArray;
    FColorScheme  : TVpCalColorScheme;

    procedure Assign(Source : TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;

    property OnChange : TNotifyEvent read  FOnChange write FOnChange;

  published
    property ActiveDay : TColor index 0 read  GetColor write SetColor;
    property ColorScheme : TVpCalColorScheme read  FColorScheme
      write SetColorScheme;
    property DayNames : TColor index 1 read  GetColor write SetColor;
    property Days : TColor index 2 read  GetColor write SetColor;
    property InactiveDays : TColor index 3 read  GetColor write SetColor;
    property MonthAndYear : TColor index 4 read  GetColor write SetColor;
    property Weekend : TColor index 5 read  GetColor write SetColor;
    property EventDays: TColor index 6 read GetColor write SetColor;
  end;

type
  TDateChangeEvent =
    procedure(Sender : TObject; Date : TDateTime) of object;
  TCalendarDateEvent =
    procedure(Sender : TObject; ADate : TDateTime;
              const Rect : TRect) of object;
  TGetHighlightEvent =
    procedure(Sender : TObject; ADate : TDateTime;
              var Color : TColor) of object;
  TGetDateEnabledEvent =
    procedure(Sender : TObject; ADate : TDateTime;
              var Enabled : Boolean) of object;

  TVpCustomCalendar = class(TVpLinkableControl)
  protected {private}
    {property variables}
    FBorderStyle     : TBorderStyle;
    FBrowsing        : Boolean;
    FColors          : TVpCalColors;
    FOptions         : TVpCalDisplayOptions;
    FDate            : TDateTime;
    FDay             : Integer;               {calendar day}
    FDateFormat      : TVpDateFormat;
    FDayNameWidth    : TVpDayNameWidth;
    FDrawHeader      : Boolean;               {true to draw day name header}
    FMonth           : Integer;               {calendar month}
    FReadOnly        : Boolean;               {true if in read only mode}
    FWantDblClicks   : Boolean;               {true to include cs_dblclks style}
    FWeekStarts      : TVpDayType;           {the day that begins the week}
    FYear            : Integer;               {calendar year}
    FLastRenderX     : Integer;
    FLastRenderY     : Integer;
    FDefaultPopup    : TPopupMenu;

    {event variables}
    FOnChange        : TDateChangeEvent;
    FOnDrawDate      : TCalendarDateEvent;
    FOnDrawItem      : TCalendarDateEvent;
    FOnGetDateEnabled: TGetDateEnabledEvent;
    FOnGetHighlight  : TGetHighlightEvent;

    {internal variables}
    clInLinkHandler  : Boolean;
    clBtnLeft        : TSpeedButton;
    clBtnRevert      : TSpeedButton;
    clBtnRight       : TSpeedButton;
    clBtnToday       : TSpeedButton;
    clInPopup        : Boolean;
    clBtnNextYear    : TSpeedButton;
    clBtnPrevYear    : TSpeedButton;
    clCalendar       : array[1..49] of Byte; {current month grid}
    clDay            : Word;
    clFirst          : Byte; {index for first day in current month}
    clLast           : Byte; {index for last day in current month}
    clMonth          : Word;
    clRowCol         : array[0..8, 0..6] of TRect; {cell TRect info}
    cSettingScheme   : Boolean;
    clYear           : Word;
    clWidth          : Integer;   {client width - margins}
    clMask           : array[0..MaxDateLen] of AnsiChar; {default date mask}
    clPopup          : Boolean;   {true if being created as a popup}
    clRevertDate     : TDateTime; {date on entry}
    clRowCount       : Integer;   {7 if no header, otherwise 8}
    clStartRow       : Integer;   {first row number}

    {property methods}
    function GetDay : Integer;
    function GetMonth : Integer;
    function GetYear : Integer;
    procedure SetBorderStyle(Value : TBorderStyle);
    procedure SetDate(Value : TDateTime);
    procedure SetDateFormat(Value : TVpDateFormat);
    procedure SetDayNameWidth(Value : TVpDayNameWidth);
    procedure SetDisplayOptions(Value : TVpCalDisplayOptions);
    procedure SetDrawHeader(Value : Boolean);
    procedure SetWantDblClicks(Value : Boolean);
    procedure SetWeekStarts(Value : TVpDayType);

    {internal methods}
    procedure PopupToday (Sender : TObject);
    procedure PopupNextMonth (Sender : TObject);
    procedure PopupPrevMonth(Sender : TObject);
    procedure PopupNextYear (Sender : TObject);
    procedure PopupPrevYear (Sender : TObject);
    procedure InitializeDefaultPopup;
    procedure calChangeMonth(Sender : TObject);
    procedure calColorChange(Sender : TObject);
    function calGetCurrentRectangle : TRect;
      {-get bounding rectangle for the current calendar day}
    function calGetValidDate(ADate : TDateTime; Delta : Integer) : TDateTime;
    procedure calRebuildCalArray (ADate : TDateTime);
      {-recalculate the contents of the calendar array}
    procedure CalculateSizes (WorkCanvas  : TCanvas;
                              Angle       : TVpRotationAngle;
                              Rect        : TRect;
                              var Row     : TRowArray;
                              var Col     : TColArray;
                              DisplayOnly : Boolean);
    procedure calRecalcSize (DisplayOnly : Boolean);
      {-calcualte new sizes for rows and columns}

    {VCL control methods}
    procedure CMCtl3DChanged(var Msg : TMessage); message CM_CTL3DCHANGED;
    procedure CMEnter(var Msg : TMessage); message CM_ENTER;
    procedure CMExit(var Msg : TMessage); message CM_EXIT;
    procedure CMFontChanged(var Msg : TMessage); message CM_FONTCHANGED;

    {windows message methods}
    procedure WMEraseBkgnd(var Msg : TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg : TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;

    procedure calBtnClick(Sender : TObject);
    procedure CreateParams(var Params : TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoOnChange(Value : TDateTime); dynamic;
    function DoOnGetDateEnabled(ADate : TDateTime) : Boolean; dynamic;
    procedure DoOnMouseWheel(Shift : TShiftState;
                             Delta, XPos, YPos : SmallInt); override;
    function IsReadOnly : Boolean; dynamic;
      {-return true if the calendar is in read-only mode}
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure KeyPress(var Key : Char); override;
    procedure MouseDown(Button : TMouseButton;
                        Shift : TShiftState;
                        X, Y : Integer); override;
    procedure MouseUp(Button : TMouseButton;
                      Shift : TShiftState; X, Y : Integer); override;
    procedure Paint; override;

  public
    constructor Create(AOwner : TComponent); override;
    constructor CreateEx(AOwner : TComponent; AsPopup : Boolean); virtual;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight : Integer); override;

    function GetControlType : TVpItemType; override;

    procedure IncDay(Delta : Integer);
    procedure IncMonth(Delta : Integer);
    procedure IncYear(Delta : Integer);

    procedure PaintToCanvas (ACanvas : TCanvas;
                             ARect   : TRect;
                             Angle   : TVpRotationAngle;
                             ADate   : TDateTime);
    procedure RenderToCanvas (RenderCanvas : TCanvas;
                              RenderIn     : TRect;
                              Angle        : TVpRotationAngle;
                              Scale        : Extended;
                              RenderDate   : TDateTime;
                              StartLine    : Integer;
                              StopLine     : Integer;
                              UseGran      : TVpGranularity;
                              DisplayOnly  : Boolean); override;
    procedure SetToday;

    { LinkHandler is the method which is called by the ControlLink component,  }
    { it is used to synchronize the calendar's date with other Visual PlanIt   }
    { controls do not call the LinkHandler procedure programatically.          }
    procedure LinkHandler(Sender: TComponent;
      NotificationType: TVpNotificationType;
      const Value: Variant); override;

    property Browsing : Boolean
      read FBrowsing;
    property Canvas;
    property Day : Integer
      read GetDay;
    property Month : Integer
      read GetMonth;
    property Year : Integer
      read GetYear;

    {properties}
    property BorderStyle : TBorderStyle
      read FBorderStyle write SetBorderStyle;
    property Color;
    property Colors : TVpCalColors
      read FColors write FColors;
    property Date : TDateTime
      read FDate write SetDate;
    property DateFormat : TVpDateFormat
      read FDateFormat write SetDateFormat;
    property DayNameWidth : TVpDayNameWidth
      read FDayNameWidth write SetDayNameWidth;
    property Options : TVpCalDisplayOptions
      read FOptions write SetDisplayOptions;
    property ReadOnly : Boolean
      read FReadOnly write FReadOnly;
    property WantDblClicks : Boolean
      read FWantDblClicks write SetWantDblClicks;
    property WeekStarts : TVpDayType
      read FWeekStarts write SetWeekStarts;

    {events}
    property OnChange : TDateChangeEvent
      read  FOnChange write FOnChange;
    property OnDrawDate : TCalendarDateEvent
      read FOnDrawDate write FOnDrawDate;
    property OnDrawItem : TCalendarDateEvent
      read FOnDrawItem write FOnDrawItem;
    property OnGetDateEnabled : TGetDateEnabledEvent
      read FOnGetDateEnabled write FOnGetDateEnabled;
    property OnGetHighlight : TGetHighlightEvent
      read FOnGetHighlight write FOnGetHighlight;
  end;

  TVpCalendar = class(TVpCustomCalendar)
  published
    {properties}
    {$IFDEF VERSION4}
    property Anchors;
    property Constraints;
    property DragKind;
    {$ENDIF}
    property Align;
    property BorderStyle;
    property Color;
    property Colors;
    property Ctl3D;
    property Cursor;
    property DateFormat;
    property DayNameWidth;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Options;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantDblClicks;
    property WeekStarts;
    {events}
    property AfterEnter;
    property AfterExit;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawDate;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetDateEnabled;
    property OnGetHighlight;
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
  VpData;

const
  calMargin = 4; {left, right, and top margin}


{*** TVpCalColors ***}
procedure TVpCalColors.Assign(Source : TPersistent);
begin
  if Source is TVpCalColors then begin
    FCalColors := TVpCalColors(Source).FCalColors;
    FColorScheme := TVpCalColors(Source).FColorScheme;
    FOnChange := TVpCalColors(Source).FOnChange;
  end else
    inherited Assign(Source);
end;
{=====}

procedure TVpCalColors.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TVpCalColors.EndUpdate;
begin
  FUpdating := False;
  DoOnChange;
end;
{=====}

procedure TVpCalColors.DoOnChange;
begin
  if not FUpdating and Assigned(FOnChange) then
    FOnChange(Self);

  if not SettingScheme then
    FColorScheme := cscalCustom;
end;
{=====}

function TVpCalColors.GetColor(Index : Integer) : TColor;
begin
  Result := FCalColors[Index];
end;
{=====}

procedure TVpCalColors.SetColor(Index : Integer; Value : TColor);
begin
  if Value <> FCalColors[Index] then begin
    FCalColors[Index] := Value;
    DoOnChange;
  end;
end;
{=====}

procedure TVpCalColors.SetColorScheme(Value : TVpCalColorScheme);
begin
  if Value <> FColorScheme then begin
    SettingScheme := True;
    try
      FColorScheme := Value;
      if Value <> cscalCustom then begin
        FCalColors := CalScheme[Value];
        DoOnChange;
      end;
    finally
      SettingScheme := False;
    end;
  end;
end;
{=====}




{*** TVpCustomCalendar ***}
procedure TVpCustomCalendar.calBtnClick(Sender : TObject);
var
  Key : Word;
begin
  SetFocus;
  Key := 0;

  if Sender = clBtnLeft then begin
    Key := VK_PRIOR;
    KeyDown(Key, []);
  end else if Sender = clBtnRevert then begin
    Key := VK_ESCAPE;
    KeyDown(Key, []);
  end else if Sender = clBtnRight then begin
    Key := VK_NEXT;
    KeyDown(Key, []);
  end else if Sender = clBtnToday then begin
    Key := VK_BACK;
    KeyDown(Key, [ssAlt]);
  end else if Sender = clBtnNextYear then begin
    Key := VK_NEXT;
    KeyDown(Key, [ssCtrl]);
  end else if Sender = clBtnPrevYear then begin
    Key := VK_PRIOR;
    KeyDown(Key, [ssCtrl]);
  end;
end;
{=====}

procedure TVpCustomCalendar.calChangeMonth(Sender : TObject);
var
  Y, M, D : Word;
  MO      : Integer;
  MI      : TMenuItem;
begin
  MI := (Sender as TMenuItem);
  DecodeDate(FDate, Y, M, D);
  MO := MI.Tag;
  {set month and year}
  if (MO > M) and (MI.HelpContext < 3) then
    Dec(Y)
  else if (MO < M) and (MI.HelpContext > 3) then
    Inc(Y);
  M := M + MO;
  {set day}
  if D > DaysInMonth(Y, MO) then
    D := DaysInMonth(Y, MO);
  SetDate(calGetValidDate(EncodeDate(Y, MO, D)-1, +1));
  if (Assigned(FOnChange)) then
    FOnChange(Self, FDate);
end;
{=====}

procedure TVpCustomCalendar.calColorChange(Sender : TObject);
begin
  Invalidate;
end;
{=====}

function TVpCustomCalendar.calGetCurrentRectangle : TRect;
  {-get bounding rectangle for the current date}
var
  Idx  : Integer;
  R, C : Integer;
begin
  {index into the month grid}
  Idx := clFirst + Pred(clDay) + 13;
  R := (Idx div 7);
  C := (Idx mod 7);
  Result := clRowCol[R,C];
end;
{=====}

function TVpCustomCalendar.calGetValidDate(ADate : TDateTime;
  Delta : Integer) : TDateTime;
var
  I, X : Integer;
  Valid: Boolean;
  Fwd: Boolean;
begin
  Valid := false;
  Fwd := false;
  X := Delta;
  I := 1;
  while not Valid and (I < 1000) do begin
    {If the date is valid then yay!}
    if (DoOnGetDateEnabled(ADate + (X * I))) then begin
      Valid := true;
      Fwd := True;
    end
    {otherwise check the other direction}
    else if (DoOnGetDateEnabled(ADate - (X * I))) then begin
      valid := true;
    end
    else Inc(I);
  end;
  if Valid then
    if Fwd then Result := ADate + (X * I)
    else Result := ADate - (X * I)
  else
    raise(EVpCalendarError.Create(RSInvalidDate));
end;
{=====}

procedure TVpCustomCalendar.calRebuildCalArray (ADate : TDateTime);
var
  Day1 : TVpDayType;
  I, J : Integer;
begin
  HandleNeeded;
  DecodeDate(ADate, clYear, clMonth, clDay);

  {get the first day of the current month and year}
  Day1 := TVpDayType(SysUtils.DayOfWeek(EncodeDate(clYear, clMonth, 1)) -1);

  {find its index}
  I := Byte(Day1) - Byte(WeekStarts) + 1;
  if I < 1 then
    Inc(I, 7);
  clFirst := I;

  {find the index of the last day in the month}
  clLast := clFirst + DaysInMonth(clYear, clMonth) - 1;

  {initialize the first part of the calendar}
  if clMonth = 1 then
    J := DaysInMonth(clYear - 1, 12)
  else
    J := DaysInMonth(clYear, clMonth-1);
  for I := clFirst-1 downto 1 do begin
    clCalendar[I] := J;
    Dec(J);
  end;

  {initialize the rest of the calendar}
  J := 1;
  for I := clFirst to 49 do begin
    clCalendar[I] := J;
    if I = clLast then
      J := 1
    else
      Inc(J);
  end;
end;
{=====}

procedure TVpCustomCalendar.CalculateSizes (WorkCanvas  : TCanvas;
                                            Angle       : TVpRotationAngle;
                                            Rect        : TRect;
                                            var Row     : TRowArray;
                                            var Col     : TColArray;
                                            DisplayOnly : Boolean);

  {-calcualte new sizes for rows and columns}
var
  R   : Integer;
  C   : Integer;
  D1  : Integer;
  D2  : Integer;
  CH  : Integer;
  RH  : Integer;
  LR  : Integer;

  function SumOf(const A : array of Integer; First, Last : Integer) : Integer;
  var
    I : Integer;
  begin
    Result := 0;
    for I := First to Last do
      Result := Result  + A[I];
  end;

begin
  if (Angle = ra90) or (Angle = ra270) then
    clWidth := Rect.Bottom - Rect.Top - 2*calMargin
  else
    clWidth := Rect.Right - Rect.Left - 2*calMargin;
  {store row and column sizes}
  for C := 0 to 6 do
    Col[C] := clWidth div 7;

  if (FDrawHeader) then begin
    {button and date row}
    Row[0] := Round(1.4 * WorkCanvas.TextHeight('Yy'));
    {day name row}
    Row[1] := Round(1.5 * WorkCanvas.TextHeight('Yy'))
  end else begin
    {button and date row}
    Row[0] := Round(1.3 * WorkCanvas.TextHeight('Yy'));
    {day name row}
    Row[1] := 0;
  end;

  if (Angle = ra90) or (Angle = ra270) then
    CH := Rect.Right - Rect.Left - 2*calMargin - Row[0] - Row[1]
  else
    CH := Rect.Bottom - Rect.Top - 2*calMargin - Row[0] - Row[1];
  if ((not (cdoShowRevert in Options)) and
      (not (cdoShowToday in Options))) or
     DisplayOnly then
    LR := 7
  else
    LR := 8;

    RH := CH div (LR - 1);

  for R := 2 to 8 do
    Row[R] := RH;

  {distribute any odd horizontal space equally among the columns}
  for C := 0 to clWidth mod 7 do
    Inc(Col[C]);

  {distribute odd vertical space to top 2 rows}
  D1 := 0;
  for R := 0 to LR do
    D1 := D1 + Row[R];
  if (Angle = ra90) or (Angle = ra270) then
    D1 := Rect.Right - Rect.Left - D1 - 2*calMargin
  else
    D1 := Rect.Bottom - Rect.Top - D1 - 2*calMargin;
  D2 := D1 div 2;
  D1 := D1 - D2;
  Row[0] := Row[0] + D1;
  if (FDrawHeader) then
    Row[1] := Row[1] + D2;

  {initialize each cells TRect structure using}
  {the row heights from the Row[] array and the}
  {column widths from the Col[] array}
  for R := clStartRow to 7 do begin
    for C := 0 to 6 do begin
      clRowCol[R, C].Left := SumOf(Col, 0, C-1) + calMargin;
      clRowCol[R, C].Right := SumOf(Col, 0, C) + calMargin;
      clRowCol[R, C].Top := SumOf(Row, 0, R-1) + calMargin;
      clRowCol[R, C].Bottom := SumOf(Row, 0, R) + calMargin;
    end;
  end;
end;

procedure TVpCustomCalendar.calRecalcSize (DisplayOnly : Boolean);
  {-calcualte new sizes for rows and columns}
var
  Row : TRowArray;
  Col : TColArray;

  function SumOf(const A : array of Integer; First, Last : Integer) : Integer;
  var
    I : Integer;
  begin
    Result := 0;
    for I := First to Last do
      Result := Result  + A[I];
  end;

begin
  if not HandleAllocated then
    Exit;

  {clear row/col position structure}
  FillChar(clRowCol, SizeOf(clRowCol), #0);

  {set the way the buttons should look}
  clBtnLeft.Flat     := not Ctl3D and not clPopup;
  clBtnRevert.Flat   := not Ctl3D and not clPopup;
  clBtnRight.Flat    := not Ctl3D and not clPopup;
  clBtnToday.Flat    := not Ctl3D and not clPopup;
  clBtnNextYear.Flat := not Ctl3D and not clPopup;
  clBtnPrevYear.Flat := not Ctl3D and not clPopup;

  clBtnRevert.Visible := cdoShowRevert in FOptions;
  clBtnToday.Visible := cdoShowToday in FOptions;
  clBtnLeft.Visible := (cdoShowNavBtns in FOptions);
  clBtnRight.Visible := (cdoShowNavBtns in FOptions);
  clBtnNextYear.Visible := (cdoShowNavBtns in FOptions);
  clBtnPrevYear.Visible := (cdoShowNavBtns in FOptions);

  CalculateSizes (Canvas, ra0, Rect (0, 0, Width, Height), Row, Col,
                  DisplayOnly);

  {position and size the left and right month buttons}
  {position and size the next and prev year buttons}
  clBtnNextYear.Height := Row[0] - calMargin;
  clBtnNextYear.Width := Col[1] - calMargin;
  if clBtnNextYear.Width < clBtnNextYear.Glyph.Width + 3 then
    clBtnNextYear.Width := clBtnNextYear.Glyph.Width + 3;
  clBtnNextYear.Top := calMargin;
  clBtnNextYear.Left := ClientWidth - calMargin - clBtnNextYear.Width;

  clBtnPrevYear.Height := Row[0] - calMargin;
  clBtnPrevYear.Width := Col[5] - calMargin;
  if clBtnPrevYear.Width < clBtnPrevYear.Glyph.Width + 3 then
    clBtnPrevYear.Width := clBtnPrevYear.Glyph.Width + 3;
  clBtnPrevYear.Top := calMargin;
  clBtnPrevYear.Left := calMargin;

  clBtnLeft.Height := Row[0] - calMargin;
  clBtnLeft.Width := Col[0] - calMargin;
  if clBtnLeft.Width < clBtnLeft.Glyph.Width + 3 then
    clBtnLeft.Width := clBtnLeft.Glyph.Width + 3;
  clBtnLeft.Top := calMargin;
  clBtnLeft.Left := clBtnPrevYear.Left + clBtnPrevYear.Width;

  clBtnRight.Height := Row[0] - calMargin;
  clBtnRight.Width := Col[6] - calMargin;
  if clBtnRight.Width < clBtnRight.Glyph.Width + 3 then
    clBtnRight.Width := clBtnRight.Glyph.Width + 3;
  clBtnRight.Top := calMargin;
  clBtnRight.Left := clBtnNextYear.Left - clBtnRight.Width;

  {position and size "today" button}
  clBtnToday.Height := Row[8];
  clBtnToday.Width := Col[5] + Col[6] - calMargin;
  clBtnToday.Top := ClientHeight - calMargin - clBtnToday.Height + 1;
  clBtnToday.Left := ClientWidth - calMargin - clBtnToday.Width;


  {position and size "revert" button}
  clBtnRevert.Height := Row[8];
  clBtnRevert.Width := Col[5] + Col[6] - calMargin;
  clBtnRevert.Top := ClientHeight - calMargin - clBtnRevert.Height + 1;
  clBtnRevert.Left := clBtnToday.Left - clBtnRevert.Width - calMargin;
end;
{=====}

procedure TVpCustomCalendar.CMCtl3DChanged(var Msg : TMessage);
begin
  inherited;

  if (csLoading in ComponentState) or not HandleAllocated then
    Exit;

  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;

  calReCalcSize (False);

  Invalidate;
end;
{=====}

procedure TVpCustomCalendar.CMEnter(var Msg : TMessage);
var
  R : TRect;
begin
  inherited;

  clRevertDate := FDate;

  {invalidate the active date to ensure that the focus rect is painted}
  R := calGetCurrentRectangle;
  InvalidateRect(Handle, @R, False);
end;
{=====}

procedure TVpCustomCalendar.CMExit(var Msg : TMessage);
var
  R : TRect;
begin
  inherited;

  {invalidate the active date to ensure that the focus rect is painted}
  R := calGetCurrentRectangle;
  InvalidateRect(Handle, @R, False);
end;
{=====}

procedure TVpCustomCalendar.CMFontChanged(var Msg : TMessage);
begin
  inherited;

  if csLoading in ComponentState then
    Exit;

  calRecalcSize (False);
  Invalidate;
end;
{=====}

constructor TVpCustomCalendar.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csClickEvents, csFramed] - [csCaptureMouse];

  Height         := 140;
  TabStop        := True;
  Width          := 200;
  Font.Name      := 'MS Sans Serif';
  Font.Size      := 8;

  FBorderStyle   := bsNone;
  FDayNameWidth  := 3;
  FDateFormat    := dfLong;
  FOptions       := [cdoShortNames, cdoShowYear, cdoShowRevert, cdoShowToday,
                     cdoShowNavBtns, cdoHighlightSun, cdoHighlightSat];
  FWantDblClicks := True;
  FWeekStarts    := dtSunday;
  FLastRenderX   := 0;
  FLastRenderY   := 0;

  clInLinkHandler := false;

  {create navigation buttons}
  clBtnLeft      := TSpeedButton.Create(Self);
  clBtnLeft.Parent := Self;
  clBtnLeft.Glyph.Handle := LoadBaseBitmap('VPLEFTARROW');
  clBtnLeft.OnClick := calBtnClick;
  clBtnLeft.Hint := RSCalendarPrevMonth;
  clBtnLeft.ShowHint := True;

  clBtnRight := TSpeedButton.Create(Self);
  clBtnRight.Parent := Self;
  clBtnRight.Glyph.Handle := LoadBaseBitmap('VPRIGHTARROW');
  clBtnRight.OnClick := calBtnClick;
  clBtnRight.Hint := RSCalendarNextMonth;
  clBtnRight.ShowHint := True;

  clBtnNextYear := TSpeedButton.Create(Self);
  clBtnNextYear.Parent := Self;
  clBtnNextYear.Glyph.Handle := LoadBaseBitmap('VPRIGHTARROWS');
  clBtnNextYear.OnClick := calBtnClick;
  clBtnNextYear.Hint := RSCalendarNextYear;
  clBtnNextYear.ShowHint := True;

  clBtnPrevYear := TSpeedButton.Create(Self);
  clBtnPrevYear.Parent := Self;
  clBtnPrevYear.Glyph.Handle := LoadBaseBitmap('VPLEFTARROWS');
  clBtnPrevYear.OnClick := calBtnClick;
  clBtnPrevYear.Hint := RSCalendarPrevYear;
  clBtnPrevYear.ShowHint := True;

  {create "revert" button}
  clBtnRevert := TSpeedButton.Create(Self);
  clBtnRevert.Parent := Self;
  clBtnRevert.Glyph.Handle := LoadBaseBitmap('VPREVERT');
  clBtnRevert.OnClick := calBtnClick;
  clBtnRevert.Hint := RSCalendarRevert;
  clBtnRevert.ShowHint := True;

  {create "today" button}
  clBtnToday := TSpeedButton.Create(Self);
  clBtnToday.Parent := Self;
  clBtnToday.Glyph.Handle := LoadBaseBitmap('VPTODAY');
  clBtnToday.OnClick := calBtnClick;
  clBtnToday.Hint := RSCalendarToday;
  clBtnToday.ShowHint := True;

  {assign default color scheme}
  FColors := TVpCalColors.Create;
  FColors.OnChange := calColorChange;
  FColors.FCalColors := CalScheme[cscalWindows];

  {assign default international support object}

  FDrawHeader:= True;
  clRowCount := 8;
  clStartRow := 0;

  FDefaultPopup := TPopupMenu.Create (Self);
  InitializeDefaultPopup;
end;
{=====}

constructor TVpCustomCalendar.CreateEx(AOwner : TComponent; AsPopup : Boolean);
begin
  clPopup := AsPopup;
  Create(AOwner);
end;
{=====}

procedure TVpCustomCalendar.CreateParams(var Params : TCreateParams);
const
  BorderStyles : array[TBorderStyle] of LongInt = (0, WS_BORDER);
begin
  inherited CreateParams(Params);

  with Params do begin
    Style := LongInt(Style) or BorderStyles[FBorderStyle];
    if clPopup then begin
      WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    end;
  end;

  if NewStyleControls and (Ctl3D or clPopup) and (FBorderStyle = bsSingle) then begin
    if not clPopup then
      Params.Style := Params.Style and not WS_BORDER;
    Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
  end;

  {set style to reflect desire for double clicks}
  if FWantDblClicks then
    ControlStyle := ControlStyle + [csDoubleClicks]
  else
    ControlStyle := ControlStyle - [csDoubleClicks];
end;
{=====}

procedure TVpCustomCalendar.CreateWnd;
begin
  inherited CreateWnd;

  calRecalcSize (False);

  {if not set, get current date}
  if FDate = 0 then
    SetDate(calGetValidDate(SysUtils.Date-1, +1));
end;
{=====}

destructor TVpCustomCalendar.Destroy;
begin
  FColors.Free;
  FColors := nil;

  FDefaultPopup.Free;

  inherited Destroy;
end;
{=====}

procedure TVpCustomCalendar.DoOnChange(Value : TDateTime);
begin
  if Assigned(FOnChange) then
    FOnChange(Self, Value);
end;
{=====}

function TVpCustomCalendar.DoOnGetDateEnabled(ADate : TDateTime) : Boolean;
begin
  Result := True;
  if Assigned(FOnGetDateEnabled) then
    FOnGetDateEnabled(Self, ADate, Result);
end;
{=====}

procedure TVpCustomCalendar.DoOnMouseWheel(Shift : TShiftState; Delta, XPos, YPos : SmallInt);
var
  Key : Word;
begin
  inherited DoOnMouseWheel(Shift, Delta, XPos, YPos);

  if Abs(Delta) = WHEEL_DELTA then begin
    {inc/dec month}
    if Delta < 0 then
      Key := VK_NEXT
    else
      Key := VK_PRIOR;
    KeyDown(Key, []);
  end else if Abs(Delta) > WHEEL_DELTA then begin
    {inc/dec year}
    if Delta < 0 then
      Key := VK_NEXT
    else
      Key := VK_PRIOR;
    KeyDown(Key, [ssCtrl]);
  end else if Abs(Delta) < WHEEL_DELTA then begin
    {inc/dec Week}
    if Delta < 0 then
      Key := VK_DOWN
    else
      Key := VK_UP;
    KeyDown(Key, []);
  end;
end;
{=====}

function TVpCustomCalendar.IsReadOnly : Boolean;
begin
  Result := ReadOnly;
end;
{=====}

procedure TVpCustomCalendar.KeyDown(var Key : Word; Shift : TShiftState);
var
  Y          : Word;
  M          : Word;
  D          : Word;
  HD         : TDateTime;
  PopupPoint : TPoint;

begin
  inherited KeyDown(Key, Shift);

  if IsReadOnly then
    Exit;

  HD := FDate;
  case Key of
    VK_LEFT  : if Shift = [] then
                 SetDate(calGetValidDate(FDate, -1))
               else if ssCtrl in Shift then
                 IncMonth (-1)
               else if ssShift in Shift then
                 IncYear (-1);

    VK_RIGHT : if Shift = [] then
                 SetDate(calGetValidDate(FDate, +1))
               else if ssCtrl in Shift then
                 IncMonth (1)
               else if ssShift in Shift then
                 IncYear (1);
    VK_UP    : if Shift = [] then
                 SetDate(calGetValidDate(FDate, -7))
               else if ssCtrl in Shift then
                 IncYear (-1)
               else if ssShift in Shift then
                 IncMonth (-1);
    VK_DOWN  : if Shift = [] then
                 SetDate(calGetValidDate(FDate, +7))
               else if ssCtrl in Shift then
                 IncYear (1)
               else if ssShift in Shift then
                 IncMonth (1);
    VK_HOME  :
      begin
        if ssCtrl in Shift then begin
          DecodeDate(FDate, Y, M, D);
          SetDate(calGetValidDate(EncodeDate(Y, 1, 1)-1, +1));
        end else if Shift = [] then begin
          DecodeDate(FDate, Y, M, D);
          SetDate(calGetValidDate(EncodeDate(Y, M, 1)-1, +1));
        end;
      end;
    VK_END   :
      begin
        if ssCtrl in Shift then begin
          DecodeDate(FDate, Y, M, D);
          SetDate(calGetValidDate(EncodeDate(Y, 12, DaysInMonth(Y, 12))+1, -1));
        end else if Shift = [] then begin
          DecodeDate(FDate, Y, M, D);
          SetDate(calGetValidDate(EncodeDate(Y, M, DaysInMonth(Y, M))+1, -1));
        end;
      end;
    VK_PRIOR :
      begin
        if ssCtrl in Shift then begin
          IncYear(-1);
        end else if Shift = [] then begin
          IncMonth(-1);
        end;
      end;
    VK_NEXT :
      begin
        if ssCtrl in Shift then begin
          IncYear(1);
        end else if Shift = [] then begin
          IncMonth(1);
        end;
      end;
    VK_BACK :
      begin
        if ssAlt in Shift then
          SetDate(calGetValidDate(SysUtils.Date-1, +1));
      end;
    VK_ESCAPE:
      begin
        if Shift = [] then
          SetDate(calGetValidDate(clRevertDate-1, +1));
      end;
    VK_F10   :
      if (ssShift in Shift) and not (Assigned (PopupMenu)) then begin
        PopupPoint := GetClientOrigin;
        FDefaultPopup.Popup (PopupPoint.x + 10,
                             PopupPoint.y + 10);
      end;
    VK_APPS  :
      if not Assigned (PopupMenu) then begin
        PopupPoint := GetClientOrigin;
        FDefaultPopup.Popup (PopupPoint.x + 10,
                             PopupPoint.y + 10);
      end;
  end;

  if HD <> FDate then begin
      FBrowsing := True;
    try
      DoOnChange(FDate);
    finally
      FBrowsing := False;
    end;
  end;
end;
{=====}

procedure TVpCustomCalendar.KeyPress(var Key : Char);
begin
  inherited KeyPress(Key);

  if IsReadOnly then
    Exit;

  case Key of
    '+' : SetDate(calGetValidDate(FDate, +1));
    '-' : SetDate(calGetValidDate(FDate, -1));
    #13 : DoOnChange(FDate);       {date selected}
    #32 : DoOnChange(FDate);       {date selected}
    ^Z  : SetDate(calGetValidDate(SysUtils.Date-1, +1));
  end;
end;
{=====}

procedure TVpCustomCalendar.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
var
  Yr           : Word;
  M            : Word;
  D            : Word;
  Yr2          : Word;
  M2           : Word;
  D2           : Word;
  R, C         : Integer;
  OldIdx       : Integer;
  NewIdx       : Integer;
  Re           : TRect;
  Ignore       : Boolean;
  ClientOrigin : TPoint;

begin
  inherited;

  if (not Assigned (PopupMenu)) and (Button = mbRight) then begin
    if not focused then
      SetFocus;
    ClientOrigin := GetClientOrigin;

    FDefaultPopup.Popup (X + ClientOrigin.x,
                         Y + ClientOrigin.y);
    Exit;
  end;

  {exit if this click happens when the popup menu is active}
  if clInPopup or (not Visible) then 
    Exit;

  SetFocus;

  inherited MouseDown(Button, Shift, X, Y);

  if IsReadOnly then
    Exit;

  {if we have the mouse captured, see if a button was clicked}
  if GetCapture = Handle then begin
    if (cdoShowNavBtns in Options) then begin
      Re := clBtnLeft.ClientRect;
      Re.TopLeft := ScreenToClient(clBtnLeft.ClientToScreen(Re.TopLeft));
      Re.BottomRight := ScreenToClient(clBtnLeft.ClientToScreen(Re.BottomRight));
      if PtInRect(Re, Point(X, Y)) then begin
        clBtnLeft.Click;
        Exit;
      end;

      Re := clBtnRight.ClientRect;
      Re.TopLeft := ScreenToClient(clBtnRight.ClientToScreen(Re.TopLeft));
      Re.BottomRight := ScreenToClient(clBtnRight.ClientToScreen(Re.BottomRight));
      if PtInRect(Re, Point(X, Y)) then begin
        clBtnRight.Click;
        Exit;
      end;

      Re := clBtnNextYear.ClientRect;
      Re.TopLeft := ScreenToClient(clBtnNextYear.ClientToScreen(Re.TopLeft));
      Re.BottomRight := ScreenToClient(clBtnNextYear.ClientToScreen(Re.BottomRight));
      if PtInRect(Re, Point(X, Y)) then begin
        clBtnNextYear.Click;
        Exit;
      end;

      Re := clBtnPrevYear.ClientRect;
      Re.TopLeft := ScreenToClient(clBtnPrevYear.ClientToScreen(Re.TopLeft));
      Re.BottomRight := ScreenToClient(clBtnPrevYear.ClientToScreen(Re.BottomRight));
      if PtInRect(Re, Point(X, Y)) then begin
        clBtnPrevYear.Click;
        Exit;
      end;
    end;

    if (cdoShowRevert in Options) then begin
      Re := clBtnRevert.ClientRect;
      Re.TopLeft := ScreenToClient(clBtnRevert.ClientToScreen(Re.TopLeft));
      Re.BottomRight := ScreenToClient(clBtnRevert.ClientToScreen(Re.BottomRight));
      if PtInRect(Re, Point(X, Y)) then begin
        clBtnRevert.Click;
        Exit;
      end;
    end;

    if (cdoShowToday in Options) then begin
      Re := clBtnToday.ClientRect;
      Re.TopLeft := ScreenToClient(clBtnToday.ClientToScreen(Re.TopLeft));
      Re.BottomRight := ScreenToClient(clBtnToday.ClientToScreen(Re.BottomRight));
      if PtInRect(Re, Point(X, Y)) then begin
        clBtnToday.Click;
        Exit;
      end;
    end;
  end;

  {save current date}
  DecodeDate(FDate, Yr, M, D);
  M2 := M;

  {calculate the row and column clicked on}
  for R := 2 to 8 do begin
    for C := 0 to 6 do begin
      if PtInRect(clRowCol[R,C], Point(X, Y)) then begin
        {convert to an index}
        NewIdx := ((R-2) * 7) + Succ(C);
        OldIdx := clFirst + Pred(clDay);
        Ignore := False;
        if NewIdx <> OldIdx then begin

          {see if this date is disabled - selection not allowed}
          if not DoOnGetDateEnabled(FDate+(NewIdx-OldIdx)) then
            Break;

          DecodeDate(FDate+(NewIdx-OldIdx), Yr2, M2, D2);
          if not (cdoShowInactive in FOptions) then begin
            {will this change the month?}
            if M2 <> M then
              Ignore := True;
          end;
          {convert to a date and redraw}
          if not Ignore then
            SetDate(FDate+(NewIdx-OldIdx));
        end;

        if (not Ignore) and (Button = mbLeft) then begin
          if M2 <> M then begin
              FBrowsing := True;
            try
              DoOnChange(FDate);
            finally
              FBrowsing := False;
            end;
          end else
            DoOnChange(FDate);
        end;

        Break;
      end;
    end;
  end;
end;
{=====}

procedure TVpCustomCalendar.MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
var
  P  : TPoint;
  M  : TPopUpMenu;
  MI : TMenuItem;
  I  : Integer;
  J  : Integer;
  K  : Integer;
  MO : Integer;
  YR : Word;
  MM : Word;
  DA : Word;
  HC : Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);

  if (PopUpMenu = nil) and (Button = mbRight) and
     (Y < clRowCol[1,0].Top) {above day names} and
     (X > clBtnPrevYear.Left + clBtnNextYear.Width) and
     (X < clBtnNextYear.Left) then begin

    if not Focused and CanFocus then
      SetFocus;

    M := TPopupMenu.Create(Self);
    try
      DecodeDate(FDate, YR, MM, DA);
      MO := MM; {convert to integer to avoid wrap-around errors with words}

      {determine the starting month}
      I := MO - 3;
      if I < 1 then
        I := MO - 3 + 12;

      {determine the ending month + 1}
      J := MO + 4;
      if J > 12 then
        J := MO + 4 - 12;

      K := 0;
      {create the menu items}
      repeat
        MI := TMenuItem.Create(M);
        MI.Caption := LongMonthNames[I];
        MI.Enabled := Enabled;
        MI.OnClick := calChangeMonth;
        MI.Tag := I;
        MI.HelpContext := K;
        M.Items.Add(MI);
        Inc(I);
        Inc(K);
        if I > 12 then
          I := 1;
      until I = J;

      HC := GetCapture = Handle;

      P.X := X-20;
      P.Y := Y - ((GetSystemMetrics(SM_CYMENU)*7) div 2);
      P := ClientToScreen(P);
      {move the mouse to cause the menu item to highlight}
      PostMessage(Handle, WM_MOUSEMOVE, 0, MAKELONG(P.X,P.Y+1));

      clInPopup := True;
      try
        M.PopUp(P.X, P.Y);

        Application.ProcessMessages;

        {capture the mouse again}
        if clPopup and HC then
          SetCapture(Handle);
      finally
        clInPopup := false;
      end;
    finally
      M.Free;
    end;
  end;
end;
{=====}

procedure TVpCustomCalendar.IncDay(Delta : Integer);
  {-change the day by Delta (signed) days}
begin
  if Delta > 0 then
    SetDate(calGetValidDate(FDate+Delta-1, +1))
  else
    SetDate(calGetValidDate(FDate+Delta+1, -1));
end;
{=====}

procedure TVpCustomCalendar.IncMonth(Delta : Integer);
  {-change the month by Delta (signed) months}
var
  Y, M, D    : Word;
  iY, iM, iD : Integer;
begin
  DecodeDate(FDate, Y, M, D);
  iY := Y; iM := M; iD := D;
  Inc(iM, Delta);
  if iM > 12 then begin
    iM := iM - 12;
    Inc(iY);
  end else if iM < 1 then begin
    iM := iM + 12;
    Dec(iY);
  end;
  if iD > DaysInMonth(iY, iM) then
    iD := DaysInMonth(iY, iM);

  SetDate(calGetValidDate(EncodeDate(iY, iM, iD)-1, +1));
end;
{=====}

procedure TVpCustomCalendar.IncYear(Delta : Integer);
var
  Y, M, D  : Word;
  iY, iM, iD : Integer;
begin
  DecodeDate(FDate, Y, M, D);
  iY := Y; iM := M; iD := D;
  Inc(iY, Delta);
  if iD > DaysInMonth(iY, iM) then
    iD := DaysInMonth(iY, iM);
  SetDate(calGetValidDate(EncodeDate(iY, iM, iD)-1, +1));
end;
{=====}

procedure TVpCustomCalendar.Paint;
begin
  RenderToCanvas (Canvas,                      // Paint Canvas
                  Rect (0, 0, Width, Height),  // Paint Rectangle
                  ra0,
                  1,                           // Scale
                  Date,                        // Date
                  -1,                          // Start At
                  -1,                          // End At
                  gr30Min,
                  False);                       // Display Only
end;
{=====}

procedure TVpCustomCalendar.LinkHandler(Sender: TComponent;
  NotificationType: TVpNotificationType; const Value: Variant);
begin
  clInLinkHandler := true;
  try
    if NotificationType = neDateChange then
      Date := Value
    else if NotificationType = neInvalidate then
      Invalidate;
  finally
    clInLinkHandler := false;
  end;
end;
{=====}

function TVpCustomCalendar.GetDay : Integer;
begin
  Result := clDay;
end;
{=====}

function TVpCustomCalendar.GetMonth : Integer;
begin
  Result := clMonth;
end;
{=====}

function TVpCustomCalendar.GetYear : Integer;
begin
  Result := clYear;
end;
{=====}

function TVpCustomCalendar.GetControlType : TVpItemType;
begin
  Result := itCalendar;
end;

procedure TVpCustomCalendar.PaintToCanvas (ACanvas : TCanvas;
                                            ARect   : TRect;
                                            Angle   : TVpRotationAngle;
                                            ADate   : TDateTime);
begin
  RenderToCanvas (ACanvas, ARect, Angle, 1, ADate, -1, -1, gr30Min, True);
end;

procedure TVpCustomCalendar.RenderToCanvas (RenderCanvas : TCanvas;
                              RenderIn     : TRect;
                              Angle        : TVpRotationAngle;
                              Scale        : Extended;
                              RenderDate   : TDateTime;
                              StartLine    : Integer;
                              StopLine     : Integer;
                              UseGran      : TVpGranularity;
                              DisplayOnly  : Boolean);
var
  R, C       : Integer;
  I          : Integer;
  SatCol     : Integer;
  SunCol     : Integer;
  DOW        : TVpDayType;
  Y, M, D    : Word;
  lBadDate   : Boolean;
  lDate      : TDateTime;
  RealWidth  : Integer;
  RealHeight : Integer;
  RealLeft   : Integer;
  RealRight  : Integer;
  RealTop    : Integer;
  RealBottom : Integer;

  BevelHighlight   : TColor;
  BevelShadow      : TColor;
  InactiveDayColor : TColor;
  MonthYearColor   : TColor;
  DayNameColor     : TColor;
  LineColor        : TColor;
  EventDayColor    : TColor;
  DayColor         : TColor;
  RealColor        : TColor;
  WeekendColor     : TColor;

  procedure SetMeasurements;
  begin
    RealWidth  := TPSViewportWidth (Angle, RenderIn); 
    RealHeight := TPSViewportHeight (Angle, RenderIn);
    RealLeft   := TPSViewportLeft (Angle, RenderIn);
    RealRight  := TPSViewportRight (Angle, RenderIn);
    RealTop    := TPSViewportTop (Angle, RenderIn);
    RealBottom := TPSViewportBottom (Angle, RenderIn);

    if RenderDate = 0 then
      RenderDate := FDate;
  end;

  procedure DrawDate;
  var
    R : TRect;
    S : string;
  begin
    if FDateFormat = dfLong then
      if cdoShowYear in FOptions then
        S := FormatDateTime('mmmm yyyy', RenderDate)
      else
        S := FormatDateTime('mmmm', RenderDate)
    else
      if cdoShowYear in FOptions then
        S := FormatDateTime('mmm yyyy', RenderDate)
      else
        S := FormatDateTime('mmm', RenderDate);

    R := Rect (clRowCol[0, 1].Left + RealLeft,
               clRowCol[0, 1].Top + RealTop,
               clRowCol[0, 1].Right + RealLeft,
               clRowCol[0, 1].Bottom + RealTop);
    R.Right := clRowCol[0, 6].Left + RealLeft;

    {switch to short date format if string won't fit}
    if FDateFormat = dfLong then
      if RenderCanvas.TextWidth(S) > R.Right-R.Left then
        S := FormatDateTime('mmm yyyy', RenderDate);

    RenderCanvas.Font.Color := MonthYearColor;
    if Assigned(FOnDrawDate) then
      FOnDrawDate(Self, RenderDate, R)
    else
      TPSCenteredTextOut (RenderCanvas, Angle, RenderIn,
                          R, S);
  end;

  procedure DrawDayNames;
  var
    I        : Integer;
    S        : string;
    DrawRect : TRect;

  begin
    {draw the day name column labels}
    RenderCanvas.Font.Color := DayNameColor;
    I := 0;
    DOW := FWeekStarts;
    repeat
      {record columns for weekends}
      if DOW = dtSaturday then
        SatCol := I;
      if DOW = dtSunday then
        SunCol := I;

      {get the day name}
      if cdoShortNames in Options then begin
        if FDayNameWidth < 1 then
          S := ShortDayNames[Ord(DOW)+1]
        else
          S := Copy(ShortDayNames[Ord(DOW)+1], 1, FDayNameWidth)
      end else begin
        if FDayNameWidth < 1 then
          S := LongDayNames[Ord(DOW)+1]
        else
          S := Copy(LongDayNames[Ord(DOW)+1], 1, FDayNameWidth)
      end;

      {draw the day name above each column}
      DrawRect := Rect (clRowCol[1, I].Left + RealLeft,
                        clRowCol[1, I].Top + RealTop,
                        clRowCol[1, I].Right +  RealLeft,
                        clRowCol[1, I].Bottom + RealTop);
      TPSCenteredTextOut (RenderCanvas, Angle, RenderIn,
                          DrawRect, S);
      Inc(I);
      if DOW < High(DOW) then
        Inc(DOW)
      else
        DOW := Low(DOW);
    until DOW = WeekStarts;
  end;

  procedure DrawLine;
  begin
    if (not Ctl3D) then begin  
      RenderCanvas.Pen.Color := LineColor;
      TPSMoveTo (RenderCanvas, Angle, RenderIn,
                 RealLeft, clRowCol[1,0].Bottom-3 + RealTop);
      TPSLineTo (RenderCanvas, Angle, RenderIn,
                 RealRight, clRowCol[1,0].Bottom-3 + RealTop);
    end else if Ctl3D then begin
      RenderCanvas.Pen.Color := BevelHighlight;
      TPSMoveTo (RenderCanvas, Angle, RenderIn,
                 RealLeft, clRowCol[1,0].Bottom-3 + RealTop);
      TPSLineTo (RenderCanvas, Angle, RenderIn,
                 RealRight, clRowCol[1,0].Bottom-3 + RealTop); 
      RenderCanvas.Pen.Color := BevelShadow;
      TPSMoveTo (RenderCanvas, Angle, RenderIn,
                 RealLeft,  clRowCol[1,0].Bottom-2 + RealTop);
      TPSLineTo (RenderCanvas, Angle, RenderIn,
                 RealRight, clRowCol[1,0].Bottom-2 + RealTop);
    end;
  end;

  procedure DrawDay(R, C, I : Integer; Grayed : Boolean);
  var
    Cl       : TColor;
    OldIdx   : Integer;
    NewIdx   : Integer;
    S        : string[10];
    DrawRect : TRect;
    TH       : Integer;

  begin
    {avoid painting day number under buttons}
    if cdoShowRevert in FOptions then
      if (R = 8) and (C >= 3) then
        Exit;
    if cdoShowToday in FOptions then
      if (R = 8) and (C >= 5) then
        Exit;

    {convert to a string and draw it centered in its rectangle}
    S := IntToStr(clCalendar[I]);

    if Grayed then
      RenderCanvas.Font.Color := InactiveDayColor;

    if not Grayed or (cdoShowInactive in FOptions) then begin
      NewIdx := ((R-2) * 7) + Succ(C);
      OldIdx := clFirst + Pred(clDay);
      if Assigned(FOnGetHighlight) then begin
        Cl := RenderCanvas.Font.Color;
        FOnGetHighlight(Self, RenderDate+(NewIdx-OldIdx), Cl);
        RenderCanvas.Font.Color := Cl;
      end;
      if Assigned(FOnDrawItem) then
        FOnDrawItem(Self, RenderDate+(NewIdx-OldIdx), clRowCol[R,C])
      else if clRowCol[R, C].Top <> 0 then begin
        DrawRect := Rect (clRowCol[R, C].Left + RealLeft,
                          clRowCol[R, C].Top + RealTop,
                          clRowCol[R, C].Right + RealLeft,
                          clRowCol[R, C].Bottom + RealTop);
        TH := RenderCanvas.TextHeight (S);
        if TH < DrawRect.Bottom - DrawRect.Top then
          DrawRect.Top := DrawRect.Top +
                          ((DrawRect.Bottom - DrawRect.Top) - TH) div 2;
        TPSCenteredTextOut (RenderCanvas, Angle, RenderIn,
                            DrawRect, S);
      end;
    end;
  end;

  procedure DrawFocusBox;
  var
    R  : TRect;
    S  : string[10];
  begin
    S := IntToStr(clDay);

    { set highlight color and font style for days with events }
    RenderCanvas.Font.Style :=
        RenderCanvas.Font.Style - [fsBold];
    lBadDate := false;

    if (DataStore <> nil) and (DataStore.Resource <> nil) then begin
      DecodeDate(RenderDate, Y, M, D);
      try
        {$IFDEF VERSION6}
        if not TryEncodeDate (Y, M, clDay, lDate) then
          lBadDate := true;
        {$ELSE}
        lDate := EncodeDate(Y, M, clDay);
        {$ENDIF}
      except
        lBadDate := true;
      end;

      if (not lBadDate)
      and (DataStore.Resource.Schedule.EventCountByDay(lDate) > 0)
      then begin
        RenderCanvas.Font.Style :=
            RenderCanvas.Font.Style + [fsBold, fsUnderline];
        RenderCanvas.Font.Color := EventDayColor;
      end else
        RenderCanvas.Font.Style :=
          RenderCanvas.Font.Style - [fsBold, fsUnderline];
    end;

    R := calGetCurrentRectangle;
    R.Left := R.Left + RealLeft;
    R.Top := R.Top + RealTop;
    R.Right := R.Right + RealLeft;
    R.Bottom := R.Bottom + RealTop;

    R := TPSRotateRectangle (Angle, RenderIn, R);
    if not DisplayOnly then begin
      if Focused then
        DrawButtonFace (RenderCanvas, R, 1, bsNew, True, True, False)
      else
        DrawButtonFace (RenderCanvas, R, 1, bsNew, True, False, False);
      R := calGetCurrentRectangle;
      R.Left := R.Left + RealLeft;
      R.Top := R.Top + RealTop;
      R.Right := R.Right + RealLeft;
      R.Bottom := R.Bottom + RealTop;
      TPSCenteredTextOut (RenderCanvas, Angle, RenderIn, R, S);
    end;
  end;

var
  Row : TRowArray;
  Col : TColArray;

begin
  if DisplayOnly then begin
    BevelHighlight   := clBlack;
    BevelShadow      := clBlack;
    InactiveDayColor := clSilver;
    MonthYearColor   := clBlack;
    DayNameColor     := clBlack;
    LineColor        := clBlack;
    EventDayColor    := clBlack;
    DayColor         := clBlack;
    RealColor        := clWhite;
    WeekendColor     := $5f5f5f;
  end else begin
    BevelHighlight   := clBtnHighlight;
    BevelShadow      := clBtnShadow;
    InactiveDayColor := FColors.InactiveDays;
    MonthYearColor   := FColors.MonthAndYear;
    DayNameColor     := FColors.DayNames;
    LineColor        := Font.Color;
    EventDayColor    := FColors.EventDays;
    DayColor         := FColors.Days;
    RealColor        := Color;
    WeekendColor     := FColors.WeekEnd;
  end;

  calRebuildCalArray (RenderDate);

  RenderCanvas.Pen.Style   := psSolid;
  RenderCanvas.Pen.Width   := 1;
  RenderCanvas.Pen.Mode    := pmCopy;
  RenderCanvas.Brush.Style := bsSolid;

  RenderCanvas.Lock;
  try
    SetMeasurements;

    RenderCanvas.Font.Assign (Font);

    if (RealRight - RealLeft <> FLastRenderX) or
       (RealBottom - RealTop <> FLastRenderY) then begin
      FLastRenderX := RealRight - RealLeft;
      FLastRenderY := RealBottom - RealTop;
      CalculateSizes (RenderCanvas, Angle, RenderIn, Row, Col, DisplayOnly);
    end;
    RenderCanvas.Brush.Color := RealColor;
    RenderCanvas.FillRect(RenderIn);

    {draw the month and year at the top of the calendar}
    DrawDate;

    {draw the days of the week}
    DrawDayNames;

    {draw line under day names}
    DrawLine;

    {draw each day}
    I := 1;
    for R := 2 to 8 do
      for C := 0 to 6 do begin
        if ((C = SatCol) and (cdoHighlightSat in Options)) or
           ((C = SunCol) and (cdoHighlightSun in Options)) then
          RenderCanvas.Font.Color := WeekendColor
        else
          RenderCanvas.Font.Color := DayColor;

        { set highlight color and font style for days with events }
        RenderCanvas.Font.Style := RenderCanvas.Font.Style - [fsBold];
        lBadDate := false;
        if (DataStore <> nil)
        and (DataStore.Resource <> nil) then begin
          DecodeDate(RenderDate, Y, M, D);
          try begin
            {$IFDEF VERSION6}
            if not TryEncodeDate (Y, M, clCalendar[I], lDate) then
              lBadDate := True;
            {$ELSE}
            if clCalendar[I] > DaysInMonth(Y, M) then
              lDate := EncodeDate(Y, M, DaysInMonth(Y, M))
            else
              lDate := EncodeDate(Y, M, clCalendar[I]);
            {$ENDIF}
          end;
          except
            lBadDate := true;
          end;

          if (not lBadDate)
          and (DataStore.Resource.Schedule.EventCountByDay(lDate) > 0)
          then begin
            RenderCanvas.Font.Style :=
                RenderCanvas.Font.Style + [fsBold, fsUnderline];
            RenderCanvas.Font.Color := EventDayColor;
          end else
            RenderCanvas.Font.Style :=
                RenderCanvas.Font.Style - [fsBold, fsUnderline];
        end;
        DrawDay(R, C, I, (I < clFirst) or (I > clLast));
        Inc(I);
      end;

    RenderCanvas.Font.Color := DayColor;
    if not Assigned(FOnDrawItem) then
      if not (cdoHideActive in FOptions) then
        DrawFocusBox;
  finally
    RenderCanvas.Unlock;
  end;
end;
{=====}

procedure TVpCustomCalendar.SetBorderStyle(Value : TBorderStyle);
begin
  if Value <> FBorderStyle then begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;
{=====}

procedure TVpCustomCalendar.SetBounds(ALeft, ATop, AWidth, AHeight : Integer);
begin
  inherited Setbounds(ALeft, ATop, AWidth, AHeight);

  if csLoading in ComponentState then
    Exit;
  calRecalcSize (False);
end;
{=====}

procedure TVpCustomCalendar.SetDate(Value : TDateTime);
var
  R : TRect;
  Y : Word;
  M : Word;
  D : Word;
begin
  if Value <> FDate then begin
    {determine if the new date is in the same month}
    DecodeDate(Value, Y, M, D);
    if (clYear = Y) and (clMonth = M) then begin
      {invalidate the old date}
      R := calGetCurrentRectangle;
      InvalidateRect(Handle, @R, False);
    end else
      Invalidate;

    DecodeDate(Value, clYear, clMonth, clDay);
    FDate := Value;
    calRebuildCalArray (FDate);

    {invalidate the new date}
    R := calGetCurrentRectangle;
    InvalidateRect(Handle, @R, False);

    if (not clInLinkHandler) and (ControlLink <> nil) then
      ControlLink.Notify(self, neDateChange, Date);
  end;
end;
{=====}

procedure TVpCustomCalendar.SetDateFormat(Value : TVpDateFormat);
begin
  if Value <> FDateFormat then begin
   FDateFormat := Value;
   Invalidate;
  end;
end;
{=====}

procedure TVpCustomCalendar.SetDayNameWidth(Value : TVpDayNameWidth);
begin
  if Value <> FDayNameWidth then begin
   FDayNameWidth := Value;
   Invalidate;
  end;
end;
{=====}

procedure TVpCustomCalendar.SetDisplayOptions(Value : TVpCalDisplayOptions);
begin
  if Value <> FOptions then begin
    FOptions := Value;
    if csDesigning in ComponentState then begin
      if cdoShowRevert in Options then
        clBtnRevert.Parent := Self
      else
        clBtnRevert.Parent := nil;
      if cdoShowToday in Options then
        clBtnToday.Parent := Self
      else
        clBtnToday.Parent := nil;
      if cdoShowNavBtns in Options then begin
        clBtnLeft.Parent := Self;
        clBtnRight.Parent := Self;
        clBtnNextYear.Parent := Self;
        clBtnPrevYear.Parent := Self;
      end else begin
        clBtnLeft.Parent := nil;
        clBtnRight.Parent := nil;
        clBtnNextYear.Parent := nil;
        clBtnPrevYear.Parent := nil;
      end;
    end;
    calRecalcSize (False);
    Invalidate;
  end;
end;
{=====}

procedure TVpCustomCalendar.SetDrawHeader(Value : Boolean);
  {-set the DrawHeader property value}
begin
  if Value <> FDrawHeader then begin
    FDrawHeader := Value;
    if FDrawHeader then begin
      clStartRow := 0;
      clRowCount := 8;
    end else begin
      clStartRow := 2;
      clRowCount := 7;
    end;
    calRecalcSize (False);
    Refresh;
  end;
end;
{=====}

procedure TVpCustomCalendar.SetToday;
  {-set the calendar to todays date}
begin
  Date := Now;
end;
{=====}

procedure TVpCustomCalendar.SetWantDblClicks(Value : Boolean);
begin
  if Value <> FWantDblClicks then begin
    FWantDblClicks := Value;
    RecreateWnd;
  end;
end;
{=====}

procedure TVpCustomCalendar.SetWeekStarts(Value : TVpDayType);
begin
  if Value <> FWeekStarts then begin
    FWeekStarts := Value;
    if csLoading in ComponentState then
      Exit;
    calRebuildCalArray (FDate);
    Invalidate;
  end;
end;
{=====}

procedure TVpCustomCalendar.WMEraseBkgnd(var Msg : TWMEraseBkgnd);
begin
  Msg.Result := 1;   {don't erase background, just say we did. Shhhhhhh!}
end;
{=====}

procedure TVpCustomCalendar.WMGetDlgCode(var Msg : TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;
{=====}

procedure TVpCustomCalendar.WMKillFocus(var Msg : TWMKillFocus);
begin
  inherited;
  Invalidate;
end;
{=====}
procedure TVpCustomCalendar.InitializeDefaultPopup;
var
  NewItem : TMenuItem;

begin
  if RSCalendarPopupToday <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSCalendarPopupToday;
    NewItem.OnClick := PopupToday;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSCalendarPopupNextMonth <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSCalendarPopupNextMonth;
    NewItem.OnClick := PopupNextMonth;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSCalendarPopupPrevMonth <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSCalendarPopupPrevMonth;
    NewItem.OnClick := PopupPrevMonth;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSCalendarPopupNextYear <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSCalendarPopupNextYear;
    NewItem.OnClick := PopupNextYear;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSCalendarPopupPrevYear <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSCalendarPopupPrevYear;
    NewItem.OnClick := PopupPrevYear;
    FDefaultPopup.Items.Add (NewItem);
  end;
end;
{=====}

procedure TVpCustomCalendar.PopupToday (Sender : TObject);
begin
  SetDate (Now);
end;
{=====}

procedure TVpCustomCalendar.PopupNextMonth (Sender : TObject);
begin
  IncMonth (1);
end;
{=====}

procedure TVpCustomCalendar.PopupPrevMonth(Sender : TObject);
begin
  IncMonth (-1);
end;
{=====}

procedure TVpCustomCalendar.PopupNextYear (Sender : TObject);
begin
  IncYear (1);
end;
{=====}

procedure TVpCustomCalendar.PopupPrevYear (Sender : TObject);
begin
  IncYear (-1);
end;
{=====}

end.
