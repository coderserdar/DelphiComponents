
(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Essentials Vol I
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ES.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{$W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

{$IFNDEF Win32}
{$G+} {286 Instructions}
{$N+} {Numeric Coprocessor}

{$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

unit EsCal;
  {-calendar component}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Buttons, Classes, Controls, Graphics, Forms, Menus, Messages, SysUtils,
  EsBase, EsData, EsUtil;

type
  TEsDateFormat   = (dfShort, dfLong);
  TEsDayNameWidth = 1..3;
  TEsDayType = (dtSunday, dtMonday, dtTuesday, dtWednesday,
                dtThursday, dtFriday, dtSaturday);

const
  {$IFDEF Win32}
  calDefBorderStyle       = bsNone;
  {$ELSE}
  calDefBorderStyle       = bsSingle;
  {$ENDIF Win32}
  calDefColor             = clBtnFace;                                 {!!.01}
  calDefDateFormat        = dfLong;
  calDefDayNameWidth      = 3;
  {$IFDEF Win32}
  calDefHeight            = 140;
  {$ELSE}
  calDefHeight            = 200;
  {$ENDIF Win32}
  calDefShowDate          = True;
  calDefShowInactive      = False;
  calDefShowToday         = True;
  calDefTabStop           = True;
  calDefWeekStarts        = dtSunday;
  {$IFDEF Win32}
  calDefWidth             = 200;
  {$ELSE}
  calDefWidth             = 240;
  {$ENDIF Win32}
  calMargin               = 4;        {left, right, and top margin}

type
  TEsCalColorArray = array[0..5] of TColor;
  TEsCalColorScheme = (csCustom, csWindows, csGold, csOcean, csRose);
  TEsCalSchemeArray = array[TEsCalColorScheme] of TEsCalColorArray;

const
  {ActiveDay, DayNames, Days, InactiveDays, MonthAndYear, Weekend}
  CalScheme : TEsCalSchemeArray =
    ((0, 0, 0, 0, 0, 0),
     (clRed,   clMaroon, clBlack,   clGray, clBlue,  clRed),
     (clBlack, clBlack,  clYellow,  clGray, clBlack, clTeal),
     (clBlack, clBlack,  clAqua,    clGray, clBlack, clNavy),
     (clRed,   clRed,    clFuchsia, clGray, clBlue,  clTeal)
    );

type
  TEsCalColors = class(TPersistent)
  private
    {.Z+}
    {property variables}
    FUpdating     : Boolean;
    FOnChange     : TNotifyEvent;

    {internal variables}
    SettingScheme : Boolean;

    {internal methods}
    procedure DoOnChange;

    {property methods}
    function GetColor(Index : Integer) : TColor;
    procedure SetColor(Index : Integer; Value : TColor);
    procedure SetColorScheme(Value : TEsCalColorScheme);
    {.Z-}

  public
    {.Z+}
    {public property variables}
    FCalColors    : TEsCalColorArray;
    FColorScheme  : TEsCalColorScheme;

    procedure Assign(Source : TPersistent);
      override;
    procedure BeginUpdate;
    procedure EndUpdate;

    property OnChange : TNotifyEvent
      read FOnChange
      write FOnChange;
    {.Z-}

  published
    property ActiveDay : TColor index 0
      read GetColor
      write SetColor;

    property ColorScheme : TEsCalColorScheme
      read FColorScheme
      write SetColorScheme;

    property DayNames : TColor index 1
      read GetColor
      write SetColor;

    property Days : TColor index 2
      read GetColor
      write SetColor;

    property InactiveDays : TColor index 3
      read GetColor
      write SetColor;

    property MonthAndYear : TColor index 4
      read GetColor
      write SetColor;

    property Weekend : TColor index 5
      read GetColor
      write SetColor;
  end;

type
  TDateChangeEvent = procedure(Sender : TObject; Date : TDateTime)
    of object;

  TCalendarDateEvent =                                                   {!!.11}
    procedure(Sender : TObject; ADate : TDateTime; const Rect : TRect)
    of object;

  TEsCustomCalendar = class(TEsBase)
  protected {private}
    {.Z+}
    {property variables}
    FBrowsing      : Boolean;                                            {!!.04}
    FColors        : TEsCalColors;
    FDate          : TDateTime;
    FDateFormat    : TEsDateFormat;
    FDayNameWidth  : TEsDayNameWidth;
    FShowDate      : Boolean;        {true to draw day name header}
    FShowInactive  : Boolean;
    FShowToday     : Boolean;
    FBorderStyle   : TBorderStyle;   {border style}
    FWeekStarts    : TEsDayType;     {the day that begins the week}

    {event variables}
    FOnChange      : TDateChangeEvent;
    FOnDrawDate      : TCalendarDateEvent;                               {!!.11}
    FOnDrawItem      : TCalendarDateEvent;                               {!!.11}

    {internal variables}
    clBtnLeft      : TSpeedButton;
    clBtnRight     : TSpeedButton;
    clBtnToday     : TSpeedButton;
    clInPopup      : Boolean;                                         {!!.02}
    clBtnNextYear  : TSpeedButton;                                    {!!.02}
    clBtnPrevYear  : TSpeedButton;                                    {!!.02}

    clCalendar     : array[1..42] of Byte;        {current month grid}
    clDay          : Word;
    clFirst        : Byte;            {index for first day in current month}
    clLast         : Byte;            {index for last day in current month}
    clMonth        : Word;
    clRowCol       : array[0..7, 0..6] of TRect;  {cell TRect info}
    cSettingScheme : Boolean;
    clYear         : Word;
    clWidth        : Integer;          {client width - margins}
    clPopup        : Boolean;          {true if being created as a popup}

    {property methods}
    procedure SetBorderStyle(Value : TBorderStyle);
    procedure SetDate(Value : TDateTime);
    procedure SetDateFormat(Value : TEsDateFormat);
    procedure SetDayNameWidth(Value : TEsDayNameWidth);
    procedure SetShowDate(Value : Boolean);
    procedure SetShowInactive(Value : Boolean);
    procedure SetShowToday(Value : Boolean);
    procedure SetWeekStarts(Value : TEsDayType);

    {internal methods}
    procedure calBtnClick(Sender : TObject);
    procedure calChangeMonth(Sender : TObject);
    procedure calColorChange(Sender : TObject);
    function calGetCurrentRectangle : TRect;
    procedure calRebuildCalArray;
    procedure calRecalcSize;

    {VCL control methods}
    procedure CMCtl3DChanged(var Msg : TMessage);
      message CM_CTL3DCHANGED;
    procedure CMEnter(var Msg : TMessage);
      message CM_ENTER;
    procedure CMExit(var Msg : TMessage);
      message CM_EXIT;
    procedure CMFontChanged(var Msg : TMessage);
      message CM_FONTCHANGED;

    {windows message methods}
    procedure WMEraseBkgnd(var Msg : TWMEraseBkgnd);
      message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg : TWMGetDlgCode);
      message WM_GETDLGCODE;
    {.Z-}

  protected
    {.Z+}
    procedure CreateParams(var Params : TCreateParams);
      override;
    procedure CreateWnd;
      override;
    procedure DoOnChange(Value : TDateTime);
      dynamic;
    {$IFDEF NeedMouseWheel}                                            {!!.05}
    procedure DoOnMouseWheel(Shift : TShiftState; Delta, XPos, YPos : SmallInt);
      override;
    {$ELSE}                                                            {!!.05}
    {$IFNDEF Windows}                                                  {!!.05}
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
      override;                                                        {!!.05}
    {$ENDIF}                                                           {!!.05}
    {$ENDIF}                                                           {!!.05}
    procedure KeyDown(var Key : Word; Shift : TShiftState);
      override;
    procedure KeyPress(var Key : Char);
      override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
      override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
      override;
    procedure Paint;
      override;
    {.Z-}
  public
    {.Z+}
    constructor Create(AOwner : TComponent);
      override;
    constructor CreateEx(AOwner : TComponent; AsPopup : Boolean);
      virtual;
    destructor Destroy;
      override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight : Integer);       {!!.05}
      override;
    {.Z-}

    {properties}
    property BorderStyle : TBorderStyle
      read FBorderStyle
      write SetBorderStyle
      default calDefBorderStyle;

    {!!.04}
    property Browsing : Boolean
      read FBrowsing;

    property Colors : TEsCalColors
      read FColors
      write FColors;

    property DayNameWidth : TEsDayNameWidth
      read FDayNameWidth
      write SetDayNameWidth
      default calDefDayNameWidth;

    property Date : TDateTime
      read FDate
      write SetDate;

    property DateFormat : TEsDateFormat
      read FDateFormat
      write SetDateFormat
      default calDefDateFormat;

    property ShowDate : Boolean
      read FShowDate
      write SetShowDate
      default calDefShowDate;

    property ShowInactive : Boolean
      read FShowInactive
      write SetShowInactive
      default calDefShowInactive;

    property ShowToday : Boolean
      read FShowToday
      write SetShowToday
      default calDefShowToday;

    property WeekStarts : TEsDayType
      read FWeekStarts
      write SetWeekStarts
      default calDefWeekStarts;

    {events}
    property OnChange : TDateChangeEvent
      read FOnChange
      write FOnChange;

    {!!.11}
    property OnDrawDate : TCalendarDateEvent
      read  FOnDrawDate
      write FOnDrawDate;

    {!!.11}
    property OnDrawItem : TCalendarDateEvent
      read  FOnDrawItem
      write FOnDrawItem;

  end;


  TEsCalendar = class(TEsCustomCalendar)
  published
    {$IFDEF VERSION4}                                                {!!.06}
    property Anchors;                                                {!!.06}
    property Constraints;                                            {!!.06}
    property DragKind;                                               {!!.06}
    {$ENDIF}                                                         {!!.06}
    {properties}
    property Align;
    property BorderStyle;
    property Colors;
    property Ctl3D;
    property Cursor;
    property DayNameWidth;
    property DateFormat;
    property DragCursor;
    property DragMode;
    property Enabled;
    property EsLabelInfo;
    property Font;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowDate;
    property ShowHint;
    property ShowInactive;
    property ShowToday;
    property TabOrder;
    property TabStop default calDefTabStop;
    property Version;
    property Visible;
    property WeekStarts;

    {events}
    property OnChange;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawDate;                                                 {!!.11}
    property OnDrawItem;                                                 {!!.11}
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFNDEF NeedMouseWheel}                                          {!!.05}
    {$IFNDEF Windows}                                                 {!!.05}
    property OnMouseWheelDown;                                        {!!.05}
    property OnMouseWheelUp;                                          {!!.05}
    {$ENDIF}                                                          {!!.05}
    {$ENDIF}                                                          {!!.05}
    {$IFDEF Win32}
    property OnStartDrag;
    {$ENDIF Win32}
  end;


implementation


{*** TEsCalColors ***}

procedure TEsCalColors.Assign(Source : TPersistent);
begin
  if Source is TEsCalColors then begin
    FCalColors := TEsCalColors(Source).FCalColors;
    FColorScheme := TEsCalColors(Source).FColorScheme;
  end else
    inherited Assign(Source);
end;

procedure TEsCalColors.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TEsCalColors.EndUpdate;
begin
  FUpdating := False;
  DoOnChange;
end;

procedure TEsCalColors.DoOnChange;
begin
  if not FUpdating and Assigned(FOnChange) then
    FOnChange(Self);

  if not SettingScheme then
    FColorScheme := csCustom;
end;

function TEsCalColors.GetColor(Index : Integer) : TColor;
begin
  Result := FCalColors[Index];
end;

procedure TEsCalColors.SetColor(Index : Integer; Value : TColor);
begin
  if Value <> FCalColors[Index] then begin
    FCalColors[Index] := Value;
    DoOnChange;
  end;
end;

procedure TEsCalColors.SetColorScheme(Value : TEsCalColorScheme);
begin
  if Value <> FColorScheme then begin
    SettingScheme := True;
    try
      FColorScheme := Value;
      if Value <> csCustom then begin
        FCalColors := CalScheme[Value];
        DoOnChange;
      end;
    finally
      SettingScheme := False;
    end;
  end;
end;


{*** TEsCustomCalendar ***}

procedure TEsCustomCalendar.calBtnClick(Sender : TObject);
var
  Key : Word;
begin
  Key := 0;
  if Sender = clBtnLeft then begin
    Key := VK_PRIOR;
    KeyDown(Key, []);
  end else if Sender = clBtnRight then begin
    Key := VK_NEXT;
    KeyDown(Key, []);
  end else if (Sender = clBtnToday) then begin                         {!!.07}
    if (SysUtils.Date <> FDate) then
      SetDate(SysUtils.Date);
    DoOnChange(FDate);
  end else if Sender = clBtnNextYear then begin                        {!!.02}
    Key := VK_NEXT;
    KeyDown(Key, [ssCtrl]);
  end else if Sender = clBtnPrevYear then begin                        {!!.02}
    Key := VK_PRIOR;
    KeyDown(Key, [ssCtrl]);
  end;
end;

procedure TEsCustomCalendar.calChangeMonth(Sender : TObject);
var
  Y  : Word;
  M  : Word;
  D  : Word;
  MO : Integer;
  MI : TMenuItem;
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
  SetDate(EncodeDate(Y, MO, D));
end;

procedure TEsCustomCalendar.calColorChange(Sender : TObject);
begin
  Invalidate;
end;

function TEsCustomCalendar.calGetCurrentRectangle : TRect;
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

procedure TEsCustomCalendar.calRebuildCalArray;
var
  Day1 : TEsDayType;
  I, J : Integer;
begin
  HandleNeeded;                                                        {!!.04}
  DecodeDate(FDate, clYear, clMonth, clDay);

  {get the first day of the current month and year}
  Day1 := TEsDayType(DayOfWeek(EncodeDate(clYear, clMonth, 1))-1);

  {find its index}
  I := Byte(Day1) - Byte(WeekStarts) + 1;
  if I < 1 then
    Inc(I, 7);
  clFirst := I;

  {find the index of the last day in the month}
  clLast := clFirst+DaysInMonth(clYear, clMonth) - 1;

  {initialize the first part of the calendar}
  if clMonth = 1 then
    J := DaysInMonth(clYear-1, 12)
  else
    J := DaysInMonth(clYear, clMonth-1);
  for I := clFirst-1 downto 1 do begin
    clCalendar[I] := J;
    Dec(J);
  end;

  {initialize the rest of the calendar}
  J := 1;
  for I := clFirst to 42 do begin
    clCalendar[I] := J;
    if I = clLast then
      J := 1
    else
      Inc(J);
  end;
end;

procedure TEsCustomCalendar.calRecalcSize;
  {-calcualte new sizes for rows and columns}
var
  R   : Integer;
  C   : Integer;
  D1  : Integer;
  D2  : Integer;
  CH  : Integer;
  RH  : Integer;
  Row : array[0..7] of Integer;
  Col : array[0..6] of Integer;

  function SumOf(A : array of Integer; First, Last : Integer) : Integer;
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

  clWidth := ClientWidth - 2*calMargin;
  {store row and column sizes}
  for C := 0 to 6 do
    Col[C] := clWidth div 7;

  Canvas.Font := Font;
  Row[0] := Round(1.3 * Canvas.TextHeight('Yy')); {button and date row}
  Row[1] := Round(1.5 * Canvas.TextHeight('Yy'));; {day name row}
  CH := ClientHeight - 2*calMargin - Row[0] - Row[1];
  RH := CH div 6;
  for R := 2 to 7 do
    Row[R] := RH;

  {distribute any odd horizontal space equally among the columns}
  for C := 0 to clWidth mod 7 do
    Inc(Col[C]);

  {distribute odd vertical space to top 2 rows}
  D1 := 0;
  for R := 0 to 7 do
    D1 := D1 + Row[R];
  D1 := ClientHeight - D1 - 2*calMargin;
  D2 := D1 div 2;
  D1 := D1 - D2;
  Row[0] := Row[0] + D1;
  Row[1] := Row[1] + D2;

  {initialize each cells TRect structure using}
  {the row heights from the Row[] array and the}
  {column widths from the Col[] array}
  for R := 0 to 7 do begin
    for C := 0 to 6 do begin
      clRowCol[R,C].Left := SumOf(Col, 0, C-1) + calMargin;
      clRowCol[R,C].Right := SumOf(Col, 0, C) + calMargin;
      clRowCol[R,C].Top := SumOf(Row, 0, R-1) + calMargin;
      clRowCol[R,C].Bottom := SumOf(Row, 0, R) + calMargin;
    end;
  end;

  {position and size the left and right month buttons}
  clBtnLeft.Height := Row[0] - calMargin;
  clBtnLeft.Width := Col[0] - calMargin;
  if clBtnLeft.Width < clBtnLeft.Glyph.Width + 3 then
    clBtnLeft.Width := clBtnLeft.Glyph.Width + 3;
  clBtnLeft.Top := calMargin;
  clBtnLeft.Left := calMargin;

  clBtnRight.Height := Row[0] - calMargin;
  clBtnRight.Width := Col[6] - calMargin;
  if clBtnRight.Width < clBtnRight.Glyph.Width + 3 then
    clBtnRight.Width := clBtnRight.Glyph.Width + 3;
  clBtnRight.Top := calMargin;
  clBtnRight.Left := ClientWidth - calMargin - clBtnRight.Width;

  {!!.02}
  {position and size the next and prev year buttons}
  clBtnNextYear.Height := Row[0] - calMargin;
  clBtnNextYear.Width := Col[1] - calMargin;
  if clBtnNextYear.Width < clBtnNextYear.Glyph.Width + 3 then
    clBtnNextYear.Width := clBtnNextYear.Glyph.Width + 3;
  clBtnNextYear.Top := calMargin;
  clBtnNextYear.Left := clBtnRight.Left - clBtnNextYear.Width;

  clBtnPrevYear.Height := Row[0] - calMargin;
  clBtnPrevYear.Width := Col[5] - calMargin;
  if clBtnPrevYear.Width < clBtnPrevYear.Glyph.Width + 3 then
    clBtnPrevYear.Width := clBtnPrevYear.Glyph.Width + 3;
  clBtnPrevYear.Top := calMargin;
  clBtnPrevYear.Left := clBtnLeft.Left + clBtnLeft.Width;

  {position and size "today" button}
  if Assigned(clBtnToday) then begin
    clBtnToday.Height := Row[7];
    clBtnToday.Width := Col[5] + Col[6] - calMargin;
    clBtnToday.Top := ClientHeight - calMargin - clBtnToday.Height;
    clBtnToday.Left := ClientWidth - calMargin - clBtnToday.Width;
    clBtnToday.Glyph.Handle := LoadBitmap(HInstance, 'ESTODAY');
  end;
end;

procedure TEsCustomCalendar.CMCtl3DChanged(var Msg : TMessage);
begin
  inherited;

  if (csLoading in ComponentState) or not HandleAllocated then
    Exit;

  {$IFDEF Win32}
  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;
  {$ENDIF}

  Invalidate;
end;

procedure TEsCustomCalendar.CMEnter(var Msg : TMessage);
var
  R : TRect;
begin
  inherited;

  {invalidate the active date to ensure that the focus rect is painted}
  R := calGetCurrentRectangle;
  InvalidateRect(Handle, @R, False);
end;

procedure TEsCustomCalendar.CMExit(var Msg : TMessage);
var
  R : TRect;
begin
  inherited;

  {invalidate the active date to ensure that the focus rect is painted}
  R := calGetCurrentRectangle;
  InvalidateRect(Handle, @R, False);
end;

procedure TEsCustomCalendar.CMFontChanged(var Msg : TMessage);
begin
  inherited;

  if csLoading in ComponentState then
    Exit;

  calRecalcSize;
  Invalidate;
end;

constructor TEsCustomCalendar.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csClickEvents, csFramed] - [csCaptureMouse];

  Height        := calDefHeight;
  TabStop       := calDefTabStop;
  Width         := calDefWidth;

  FBorderStyle  := calDefBorderStyle;
  FDayNameWidth := calDefDayNameWidth;
  FDateFormat   := calDefDateFormat;
  FShowDate     := calDefShowDate;
  FShowInactive := calDefShowInactive;
  FShowToday    := calDefShowToday;
  FWeekStarts   := calDefWeekStarts;

  {create navigation buttons}
  clBtnLeft := TSpeedButton.Create(Self);
  clBtnLeft.Parent := Self;
  clBtnLeft.Glyph.Handle := LoadBitmap(HInstance, 'ESLEFTARROW');
  clBtnLeft.OnClick := calBtnClick;

  clBtnRight := TSpeedButton.Create(Self);
  clBtnRight.Parent := Self;
  clBtnRight.Glyph.Handle := LoadBitmap(HInstance, 'ESRIGHTARROW');
  clBtnRight.OnClick := calBtnClick;

  {!!.02}
  clBtnNextYear := TSpeedButton.Create(Self);
  clBtnNextYear.Parent := Self;
  clBtnNextYear.Glyph.Handle := LoadBitmap(HInstance, 'ESRIGHTARROWS');
  clBtnNextYear.OnClick := calBtnClick;

  clBtnPrevYear := TSpeedButton.Create(Self);
  clBtnPrevYear.Parent := Self;
  clBtnPrevYear.Glyph.Handle := LoadBitmap(HInstance, 'ESLEFTARROWS');
  clBtnPrevYear.OnClick := calBtnClick;

  FColors := TEsCalColors.Create;
  FColors.OnChange := calColorChange;

  if FShowToday then begin
    {create "today" button}
    clBtnToday := TSpeedButton.Create(Self);
    clBtnToday.Parent := Self;
    clBtnToday.OnClick := calBtnClick;
  end;

  {assign default color scheme}
  FColors.FCalColors := CalScheme[csWindows];
end;

constructor TEsCustomCalendar.CreateEx(AOwner : TComponent; AsPopup : Boolean);
begin
  clPopup := AsPopup;
  Create(AOwner);
end;

procedure TEsCustomCalendar.CreateParams(var Params : TCreateParams);
const
  BorderStyles : array[TBorderStyle] of DWord = (0, WS_BORDER);        {!!.05}
begin
  inherited CreateParams(Params);

  with Params do begin
    Style := Style or BorderStyles[FBorderStyle];
    {!!.02} {block revised}
    if clPopup then begin
      Style := WS_POPUP or WS_BORDER;
      WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
      {$IFDEF Win32}
      Ctl3D := False;
      if NewStyleControls then
        ExStyle := WS_EX_TOOLWINDOW or WS_EX_CLIENTEDGE;
      {$ENDIF Win32}
    end;
  end;

  {$IFDEF Win32}
  if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then begin
    Params.Style := Params.Style and not WS_BORDER;
    Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
  end;
  {$ENDIF}
end;

procedure TEsCustomCalendar.CreateWnd;
begin
  inherited CreateWnd;

  calRecalcSize;

  {if not set, get current date}
  if FDate = 0 then
    SetDate(SysUtils.Date);

end;

destructor TEsCustomCalendar.Destroy;
begin
  FColors.Free;
  FColors := nil;

  inherited Destroy;
end;

procedure TEsCustomCalendar.DoOnChange(Value : TDateTime);
begin
  if Assigned(FOnChange) then
    FOnChange(Self, Value);
end;

{$IFDEF NeedMouseWheel}                                                {!!.05}
procedure TEsCustomCalendar.DoOnMouseWheel(Shift : TShiftState; Delta, XPos, YPos : SmallInt);
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
{$ELSE}                                                                {!!.05}
{$IFNDEF Windows}                                                      {!!.05}
{!!.05 added}
function TEsCustomCalendar.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  Key : Word;
begin
  // we always return true - if there's an event handler that returns
  // false, we'll do the work; if it returns true, the work has been
  // done, ergo this routine should return true.
  Result := true;
  if not inherited DoMouseWheel(Shift, WheelDelta, MousePos) then begin
    if Abs(WheelDelta) = WHEEL_DELTA then begin
      {inc/dec month}
      if WheelDelta < 0 then
        Key := VK_NEXT
      else
        Key := VK_PRIOR;
      KeyDown(Key, []);
    end else if Abs(WheelDelta) > WHEEL_DELTA then begin
      {inc/dec year}
      if WheelDelta < 0 then
        Key := VK_NEXT
      else
        Key := VK_PRIOR;
      KeyDown(Key, [ssCtrl]);
    end else if Abs(WheelDelta) < WHEEL_DELTA then begin
      {inc/dec Week}
      if WheelDelta < 0 then
        Key := VK_DOWN
      else
        Key := VK_UP;
      KeyDown(Key, []);
    end;
  end;
end;
{$ENDIF}                                                               {!!.05}
{$ENDIF}                                                               {!!.05}

procedure TEsCustomCalendar.KeyDown(var Key : Word; Shift : TShiftState);
var
  Y  : Word;
  M  : Word;
  D  : Word;
  HD : TDateTime;                                                      {!!.04}
begin
  inherited KeyDown(Key, Shift);

  HD := FDate;                                                         {!!.04}
  case Key of
    VK_LEFT  : if Shift = [] then
                 SetDate(FDate-1);
    VK_RIGHT : if Shift = [] then
                 SetDate(FDate+1);
    VK_UP    : if Shift = [] then
                 SetDate(FDate-7);
    VK_DOWN  : if Shift = [] then
                 SetDate(FDate+7);
    VK_HOME  :
      begin
        if Shift = [] then begin
          DecodeDate(FDate, Y, M, D);
          SetDate(EncodeDate(Y, M, 1));
        end;
      end;
    VK_END   :
      begin
        if Shift = [] then begin
          DecodeDate(FDate, Y, M, D);
          SetDate(EncodeDate(Y, M, DaysInMonth(Y, M)));
        end;
      end;
    VK_PRIOR :
      begin
        DecodeDate(FDate, Y, M, D);
        if ssCtrl in Shift then begin
        {  DecodeDate(FDate, Y, M, D); }                             {!!.07}
          Dec(Y);
          if D > DaysInMonth(Y, M) then
            D := DaysInMonth(Y, M);
          SetDate(EncodeDate(Y, M, D));
        end else if Shift = [] then begin
          Dec(M);
          if M < 1 then begin
            M := 12;
            Dec(Y);
          end;
          if D > DaysInMonth(Y, M) then
            D := DaysInMonth(Y, M);
          SetDate(EncodeDate(Y, M, D));
        end;
      end;
    VK_NEXT :
      begin
        DecodeDate(FDate, Y, M, D);
        if ssCtrl in Shift then begin
          Inc(Y);
          if D > DaysInMonth(Y, M) then
            D := DaysInMonth(Y, M);
          SetDate(EncodeDate(Y, M, D));
        end else if Shift = [] then begin
          Inc(M);
          if M > 12 then begin
            M := 1;
            Inc(Y);
          end;
          if D > DaysInMonth(Y, M) then
            D := DaysInMonth(Y, M);
          SetDate(EncodeDate(Y, M, D));
        end;
      end;
    VK_BACK :
      begin
        if ssAlt in Shift then
          SetDate(SysUtils.Date);        {return to today's date}
      end;
  end;

  {!!.04}
  if HD <> FDate then begin
      FBrowsing := True;
    try
      DoOnChange(FDate);
    finally
      FBrowsing := False;
    end;
  end;
end;

procedure TEsCustomCalendar.KeyPress(var Key : Char);
begin
  inherited KeyPress(Key);

  case Key of
    #13 : DoOnChange(FDate);       {date selected}
    #32 : DoOnChange(FDate);       {date selected}
    ^Z  : SetDate(SysUtils.Date);  {return to today's date}
  end;
end;

procedure TEsCustomCalendar.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
var
  Yr     : Word;
  M      : Word;
  D      : Word;
  Yr2    : Word;
  M2     : Word;
  D2     : Word;
  R, C   : Integer;
  OldIdx : Integer;
  NewIdx : Integer;
  Re     : TRect;
  Ignore : Boolean;
begin
  {exit if this click happens when the popup menu is active}           {!!.02}
  if clInPopup then                                                    {!!.02}
    Exit;                                                              {!!.02}

  SetFocus;

  inherited MouseDown(Button, Shift, X, Y);

  {if we have the mouse captured, see if a button was clicked}
  if GetCapture = Handle then begin
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

    {!!.02}
    Re := clBtnNextYear.ClientRect;
    Re.TopLeft := ScreenToClient(clBtnNextYear.ClientToScreen(Re.TopLeft));
    Re.BottomRight := ScreenToClient(clBtnNextYear.ClientToScreen(Re.BottomRight));
    if PtInRect(Re, Point(X, Y)) then begin
      clBtnNextYear.Click;
      Exit;
    end;

    {!!.02}
    Re := clBtnPrevYear.ClientRect;
    Re.TopLeft := ScreenToClient(clBtnPrevYear.ClientToScreen(Re.TopLeft));
    Re.BottomRight := ScreenToClient(clBtnPrevYear.ClientToScreen(Re.BottomRight));
    if PtInRect(Re, Point(X, Y)) then begin
      clBtnPrevYear.Click;
      Exit;
    end;

    if Assigned(clBtnToday) then begin
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

  {calculate the row and column clicked on}
  for R := 2 to 7 do begin
    for C := 0 to 6 do begin
      if PtInRect(clRowCol[R,C], Point(X, Y)) then begin
        {convert to an index}
        NewIdx := ((R-2) * 7) + Succ(C);
        OldIdx := clFirst + Pred(clDay);
        Ignore := False;
        if NewIdx <> OldIdx then begin
          if not FShowInactive then begin
            DecodeDate(FDate+(NewIdx-OldIdx), Yr2, M2, D2);
            {will this change the month?}
            if M2 <> M then
              Ignore := True;
          end;
          {convert to a date and redraw}
          if not Ignore then
            SetDate(FDate+(NewIdx-OldIdx));
        end;

        if (not Ignore) and (Button = mbLeft) then                     {!!.04}
          DoOnChange(FDate);

        Break;
      end;
    end;
  end;
end;

procedure TEsCustomCalendar.MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
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
  if not Focused and CanFocus then
    SetFocus;

  inherited MouseUp(Button, Shift, X, Y);

  if (PopUpMenu = nil) and (Button = mbRight) and
     (Y < clRowCol[1,0].Top) {above day names} and
     (X > clBtnPrevYear.Left + clBtnNextYear.Width) and                {!!.02}
     (X < clBtnNextYear.Left) then begin                               {!!.02}
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

      clInPopup := True;                                               {!!.02}
      try                                                              {!!.02}
        M.PopUp(P.X, P.Y);

        Application.ProcessMessages;

        {capture the mouse again}
        if clPopup and HC then
          SetCapture(Handle);
      finally                                                          {!!.02}
        clInPopup := false;                                            {!!.02}
      end;                                                             {!!.02}
    finally
      M.Free;
    end;
  end;
end;

procedure TEsCustomCalendar.Paint;
var
  R, C     : Integer;
  I        : Integer;
  CurIndex : Integer;
  SatCol   : Integer;
  SunCol   : Integer;
  DOW      : TEsDayType;

  procedure DrawDate;
  var
    R : TRect;
    S : string;
  begin
    if FDateFormat = dfLong then
      S := FormatDateTime('mmmm yyyy', FDate)
    else
      S := FormatDateTime('mmm yyyy', FDate);

    R := clRowCol[0,1];
    R.Right := clRowCol[0,6].Left;

    {switch to short date format if string won't fit}
    if FDateFormat = dfLong then
      if Canvas.TextWidth(S) > R.Right-R.Left then
        S := FormatDateTime('mmm yyyy', FDate);

    Canvas.Font.Color := FColors.MonthAndYear;
    if Assigned(FOnDrawDate) then                                        {!!.11}
      FOnDrawDate(Self, FDate, R)                                        {!!.11}
    else                                                                 {!!.11}
      DrawText(Canvas.Handle, @S[1], Length(S), R,
        DT_SINGLELINE or DT_CENTER or DT_VCENTER);
  end;

  procedure DrawDayNames;
  var
    I : Integer;
    S : string[3];
  begin
    {draw the day name column labels}
    Canvas.Font.Color := FColors.DayNames;
    I := 0;
    DOW := FWeekStarts;
    repeat
      {record columns for weekends}
      if DOW = dtSaturday then
        SatCol := I;
      if DOW = dtSunday then
        SunCol := I;

      {get the day name}
      S := Copy(ShortDayNames[Ord(DOW)+1], 1, FDayNameWidth);

      {draw the day name above each column}
      DrawText(Canvas.Handle, @S[1], Length(S), clRowCol[1,I],
        DT_SINGLELINE or DT_CENTER or DT_VCENTER);

      Inc(I);
      if DOW < High(DOW) then
        Inc(DOW)
      else
        DOW := Low(DOW);
    until DOW = WeekStarts;
  end;

  procedure DrawLine;
  begin
    if Ctl3D then begin
      Canvas.Pen.Color := clBtnHighlight;
      Canvas.MoveTo(0, clRowCol[1,0].Bottom-3);
      Canvas.LineTo(ClientWidth, clRowCol[1,0].Bottom-3);
      Canvas.Pen.Color := clBtnShadow;
      Canvas.MoveTo(0,  clRowCol[1,0].Bottom-2);
      Canvas.LineTo(ClientWidth, clRowCol[1,0].Bottom-2);
    end else if BorderStyle = bsSingle then begin
      Canvas.Pen.Color := Font.Color;
      Canvas.MoveTo(0, clRowCol[1,0].Bottom-3);
      Canvas.LineTo(ClientWidth, clRowCol[1,0].Bottom-3);
    end;
  end;

  procedure DrawDay(R, C, I : Integer; Grayed, Current : Boolean);
  var
    S : string[10];
    Cl     : TColor;
    OldIdx : Integer;
    NewIdx : Integer;
  begin
    {convert to a string and draw it centered in its rectangle}
    S := IntToStr(clCalendar[I]);

    if Grayed then
      Canvas.Font.Color := FColors.InactiveDays;

    if not Grayed or FShowInactive then
      NewIdx := ((R-2) * 7) + Succ(C);                                   {!!.11}
      OldIdx := clFirst + Pred(clDay);                                   {!!.11}
      if Assigned(FOnDrawItem) then                                      {!!.11}
        FOnDrawItem(Self, FDate+(NewIdx-OldIdx), clRowCol[R,C])          {!!.11}
      else                                                               {!!.11}
        DrawText(Canvas.Handle, @S[1], Length(S), clRowCol[R,C],
          DT_SINGLELINE or DT_CENTER or DT_VCENTER);
  end;

  procedure DrawFocusBox;
  var
    R       : TRect;
    S       : string[10];
  begin
    S := IntToStr(clDay);
    if Focused then
      R := DrawButtonFace(Canvas, calGetCurrentRectangle, 1, bsNew, True, True, False)
    else
      R := DrawButtonFace(Canvas, calGetCurrentRectangle, 1, bsNew, True, False, False);
    DrawText(Canvas.Handle, @S[1], Length(S), R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;

begin
  Canvas.Font := Font;
  Canvas.Brush.Color := calDefColor;
  Canvas.FillRect(ClientRect);

  {draw the month and year at the top of the calendar}
  if FShowDate then
    DrawDate;

  {draw the days of the week}
  DrawDayNames;

  {draw line under day names}
  DrawLine;

  {draw each day}
  CurIndex := clFirst + Pred(clDay);
  I := 1;
  for R := 2 to 7 do
    for C := 0 to 6 do begin
      if (C = SatCol) or (C = SunCol) then
        Canvas.Font.Color := FColors.WeekEnd
      else
        Canvas.Font.Color := FColors.Days;
      DrawDay(R, C, I, (I < clFirst) or (I > clLast), I = CurIndex);
      Inc(I);
    end;

  Canvas.Font.Color := FColors.ActiveDay;
  DrawFocusBox;
end;

procedure TEsCustomCalendar.SetBorderStyle(Value : TBorderStyle);
begin
  if Value <> FBorderStyle then begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TEsCustomCalendar.SetBounds(ALeft, ATop, AWidth, AHeight : Integer);
begin
  inherited Setbounds(ALeft, ATop, AWidth, AHeight);

  if csLoading in ComponentState then
    Exit;

  calRecalcSize;
end;

procedure TEsCustomCalendar.SetDate(Value : TDateTime);
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
    calRebuildCalArray;

    {invalidate the new date}
    R := calGetCurrentRectangle;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TEsCustomCalendar.SetDateFormat(Value : TEsDateFormat);
begin
  if Value <> FDateFormat then begin
   FDateFormat := Value;
   Invalidate;
  end;
end;

procedure TEsCustomCalendar.SetDayNameWidth(Value : TEsDayNameWidth);
begin
  if Value <> FDayNameWidth then begin
   FDayNameWidth := Value;
   Invalidate;
  end;
end;

procedure TEsCustomCalendar.SetShowDate(Value : Boolean);
begin
  if Value <> FShowDate then begin
   FShowDate := Value;
   Invalidate;
  end;
end;

procedure TEsCustomCalendar.SetShowInactive(Value : Boolean);
begin
  if Value <> FShowInactive then begin
   FShowInactive := Value;
   Invalidate;
  end;
end;

procedure TEsCustomCalendar.SetShowToday(Value : Boolean);
begin
  if Value <> FShowToday then begin
    FShowToday := Value;

    if FShowToday then begin
      {create "today" button}
      clBtnToday := TSpeedButton.Create(Self);
      clBtnToday.Parent := Self;
      clBtnToday.OnClick := calBtnClick;
    end else begin
      clBtnToday.Free;
      clBtnToday := nil;
    end;

   calReCalcSize;
   Invalidate;
  end;
end;

procedure TEsCustomCalendar.SetWeekStarts(Value : TEsDayType);
begin
  if Value <> FWeekStarts then begin
    FWeekStarts := Value;
    if csLoading in ComponentState then                                {!!.02}
      Exit;                                                            {!!.02}
    calRebuildCalArray;
    Invalidate;
  end;
end;

procedure TEsCustomCalendar.WMEraseBkgnd(var Msg : TWMEraseBkgnd);
begin
  Msg.Result := 1;   {don't erase background, just say we did}
end;

procedure TEsCustomCalendar.WMGetDlgCode(var Msg : TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

end.