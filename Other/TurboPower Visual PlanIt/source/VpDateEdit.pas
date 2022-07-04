{*********************************************************}
{*                 VPDATEEDIT.PAS 1.03                   *}
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

unit VpDateEdit;
  {-date edit field with popup calendar}

interface

uses
  Windows, Buttons, Classes, Controls, Forms, Graphics, Menus, Messages,
  StdCtrls, SysUtils, VpBase, VpCalendar, VpConst, VpEdPop, VpMisc;

type
  TVpDateOrder = (doMDY, doDMY, doYMD);
  TVpRequiredField = (rfYear, rfMonth, rfDay);
  TVpRequiredFields = set of TVpRequiredField;

  TVpGetDateEvent = procedure(Sender : TObject; var Value : string)
    of object;

  TVpCustomDateEdit = class(TVpEdPopup)
  protected {private}
    {property variables}
    FAllowIncDec    : Boolean;
    FDate           : TDateTime;
    FEpoch          : Integer;
    FForceCentury   : Boolean;
    FPopupCalColors : TVpCalColors;
    FPopupCalFont   : TFont;
    FPopupCalHeight : Integer;
    FPopupCalWidth  : Integer;
    FRequiredFields : TVpRequiredFields;
    FTodayString    : string;
    FWeekStarts     : TVpDayType;     {the day that begins the week}

    {event variables}
    FOnGetDate      : TVpGetDateEvent;
    FOnSetDate      : TNotifyEvent;

    {internal variables}
    Calendar        : TVpCalendar;
    DateOrder       : TVpDateOrder;
    GettingDate     : Boolean;
    HoldCursor      : TCursor;
    WasAutoScroll   : Boolean;

    {property methods}
    function GetDate : TDateTime;
    function GetReadOnly : Boolean;
    procedure SetForceCentury(Value : Boolean);
    procedure SetPopupCalFont(Value : TFont);
    procedure SetReadOnly(Value : Boolean);

    {internal methods}
    procedure PopupDateChange(Sender : TObject; Date : TDateTime);
    procedure PopupKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure PopupKeyPress(Sender : TObject; var Key : Char);
    procedure PopupMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);

  protected
    procedure DoExit; override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure KeyPress(var Key : Char); override;
    procedure PopupClose(Sender : TObject);  override;
    procedure SetDate(Value : TDateTime);
    procedure SetDateText(Value : string); dynamic;

    {protected properties}
    property AllowIncDec : Boolean
      read FAllowIncDec write FAllowIncDec default True;
    property Epoch : Integer read FEpoch write FEpoch;
    property ForceCentury : Boolean
      read FForceCentury write SetForceCentury default False;
    property PopupCalColors : TVpCalColors
      read FPopupCalColors write FPopupCalColors;
    property PopupCalFont : TFont read FPopupCalFont write SetPopupCalFont;
    property PopupCalHeight : Integer
      read FPopupCalHeight write FPopupCalHeight default calDefHeight;
    property PopupCalWidth : Integer
      read FPopupCalWidth write FPopupCalWidth default calDefWidth;
    property ReadOnly : Boolean read GetReadOnly write SetReadOnly;
    property RequiredFields : TVpRequiredFields
      read FRequiredFields write FRequiredFields;
    property TodayString : string read FTodayString write FTodayString;
    property WeekStarts : TVpDayType
      read FWeekStarts write FWeekStarts default calDefWeekStarts;
    {protected events}
    property OnGetDate : TVpGetDateEvent read FOnGetDate write FOnGetDate;
    property OnSetDate : TNotifyEvent read FOnSetDate write FOnSetDate;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure PopupOpen; override;
    function FormatDate(Value : TDateTime) : string; dynamic;
    {public properties}
    property Date : TDateTime read GetDate write SetDate;
  end;

  TVpDateEdit = class(TVpCustomDateEdit)
  published
    {properties}
    {$IFDEF VERSION4}
    property Anchors;
    property Constraints;
    property DragKind;
    {$ENDIF}
    property AllowIncDec;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Epoch;
    property Font;
    property ForceCentury;
    property HideSelection;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupCalColors;
    property PopupCalFont;
    property PopupCalHeight;
    property PopupCalWidth;
    property PopupMenu;
    property ReadOnly;
    property RequiredFields;
    property ShowHint;
    property ShowButton;
    property TabOrder;
    property TabStop;
    property TodayString;
    property Version;
    property Visible;
    property WeekStarts;

    {events}
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetDate;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSetDate;
    property OnStartDrag;
  end;


implementation

uses
  VpSR, VpException;

{*** TVpCustomDateEdit ***}

constructor TVpCustomDateEdit.Create(AOwner : TComponent);
var
  C : array[0..1] of Char;
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csSetCaption];

  FAllowIncDec := True;
  FEpoch := DefaultEpoch;
  FForceCentury := False;
  FRequiredFields := [rfMonth, rfDay];
  FTodayString := DateSeparator;
  FPopupCalHeight := calDefHeight;
  FPopupCalWidth := calDefWidth;
  FPopupCalFont := TFont.Create;
  FPopupCalFont.Assign(Font);

  {get the date order from windows}
  C[0] := '0'; {default}
  GetProfileString('intl', 'iDate', '0', C, 2);
  DateOrder := TVpDateOrder(Ord(C[0])-Ord('0'));

  {load button glyph}
  FButton.Glyph.Handle := LoadBaseBitmap('VPBTNCAL');

  {create color class}
  FPopupCalColors := TVpCalColors.Create;
  {assign default color scheme}
  FPopupCalColors.FCalColors := CalScheme[cscalWindows];
  FPopupCalColors.FColorScheme := cscalWindows;

  GettingDate := False;

end;
{=====}

destructor TVpCustomDateEdit.Destroy;
begin
  FPopupCalColors.Free;
  FPopupCalColors := nil;

  FPopupCalFont.Free;
  FPopupCalFont := nil;

  inherited Destroy;
end;
{=====}

procedure TVpCustomDateEdit.DoExit;
begin
  try
    SetDateText(Text);
  except
    SetFocus;
    raise;
  end;

  if not PopupActive then
    inherited DoExit;
end;
{=====}

function TVpCustomDateEdit.GetDate : TDateTime;
begin
  GettingDate := True;
  try
    SetDateText(Text);
  finally
    GettingDate := False;
  end;
  Result := FDate;
end;
{=====}

function TVpCustomDateEdit.GetReadOnly : Boolean;
begin
  Result := inherited ReadOnly;
end;
{=====}

procedure TVpCustomDateEdit.KeyDown(var Key : Word; Shift : TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if (Key = VK_DOWN) and (ssAlt in Shift) then
    PopupOpen;
end;
{=====}

procedure TVpCustomDateEdit.KeyPress(var Key : Char);
var
  D : Word;
  M : Word;
  Y : Word;
begin
  inherited KeyPress(Key);

  if FAllowIncDec  and (Key in ['+', '-']) then begin
    DoExit; {accept current date}
    if FDate = 0 then
      DecodeDate(SysUtils.Date, Y, M, D)
    else
      DecodeDate(FDate, Y, M, D);
    if Key = '+' then begin
      Inc(D);
      if D > DaysInMonth(Y, M) then begin
        D := 1;
        Inc(M);
        if M > 12 then begin
          Inc(Y);
          M := 1;
        end;
      end;
    end else {'-'} begin
      Dec(D);
      if D < 1 then begin
        Dec(M);
        if M < 1 then begin
          M := 12;
          Dec(Y);
        end;
        D := DaysInMonth(Y, M);
      end;
    end;
    SetDate(EncodeDate(Y, M, D));
    Modified := True;

    Key := #0; {clear}
  end;
end;
{=====}

function TVpCustomDateEdit.FormatDate(Value : TDateTime) : string;
var
  S : string;
begin
  S := ShortDateFormat;
  if FForceCentury then
    if Pos('yyyy', S) = 0 then
      Insert('yy', S, Pos('yy', S));
  Result := FormatDateTime(S, Value)
end;
{=====}

procedure TVpCustomDateEdit.PopupClose(Sender : TObject);
begin
  inherited PopupClose(Sender);

  if GetCapture = Calendar.Handle then
    ReleaseCapture;

  SetFocus;
  Calendar.Hide;  {hide the Calendar}
  if (Calendar.Parent <> nil) then
    if (Calendar.Parent is TForm) then
      TForm(Calendar.Parent).AutoScroll := WasAutoScroll
    else if (Calendar.Parent is TScrollBox) then
      TScrollBox(Calendar.Parent).AutoScroll := WasAutoScroll;
  Cursor := HoldCursor;

  {change parentage so that we control the window handle destruction}
  Calendar.Parent := Self;
end;
{=====}

procedure TVpCustomDateEdit.PopupMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
var
  P : TPoint;
  I : Integer;
begin
  P := Point(X,Y);
  if not PtInRect(Calendar.ClientRect, P) then
    PopUpClose(Sender);

  {convert to our coordinate system}
  P := ScreenToClient(Calendar.ClientToScreen(P));

  if PtInRect(ClientRect, P) then begin
    I := SelStart;
    SetFocus;
    SelStart := I;
    SelLength := 0;
  end;
end;
{=====}

procedure TVpCustomDateEdit.PopupOpen;
var
  P           : TPoint;
  MeasureFrom : TPoint;

begin
  inherited PopupOpen;

  DoExit;    {force update of date}

  if not Assigned(Calendar) then begin
    Calendar             := TVpCalendar.CreateEx (Self, True);
    Calendar.OnChange    := PopupDateChange;
    Calendar.OnExit      := PopupClose;
    Calendar.OnKeyDown   := PopupKeyDown;
    Calendar.OnKeyPress  := PopupKeyPress;
    Calendar.OnMouseDown := PopupMouseDown;
    Calendar.Visible     := False; {to avoid flash at 0,0}
    Calendar.BorderStyle := bsSingle;
    Calendar.Height      := FPopupCalHeight;
    Calendar.Width       := FPopupCalWidth;
    Calendar.WeekStarts  := FWeekStarts;
    Calendar.ParentCtl3D := False;
    Calendar.Ctl3D       := Ctl3D;
    Calendar.Font.Assign(FPopupCalFont);
  end;

  if (Parent.Parent <> nil) then
    Calendar.Parent := Parent.Parent
  else if Parent <> nil then
    Calendar.Parent := Parent
  else
    Calendar.Parent := GetParentForm(Self);

  if (Calendar.Parent <> nil) then
    if (Calendar.Parent is TForm) then begin
      WasAutoScroll := TForm(Calendar.Parent).AutoScroll;
      TForm(Calendar.Parent).AutoScroll := False;
    end else if (Calendar.Parent is TScrollBox) then begin
      WasAutoScroll := TScrollBox(Calendar.Parent).AutoScroll;
      TScrollBox(Calendar.Parent).AutoScroll := False;
    end;

  {set colors}
  Calendar.Colors.Assign(FPopupCalColors);
  {determine the proper position}
  P := Point (Left, Top + Height + 2);
  MeasureFrom := Point (0, 0);
  if Assigned (Parent) and (not (Parent is TForm)) then begin
    P.x := P.x + Parent.Left;
    P.y := P.y + Parent.Top;
  end;

  MoveWindow (Calendar.Handle,
              MeasureFrom.x + P.X,
              MeasureFrom.y + P.Y,
              Calendar.Width,
              Calendar.Height,
              False);

  if Text = '' then
    Calendar.Date := Now
  else
    Calendar.Date := FDate;

  HoldCursor := Cursor;
  Cursor := crArrow;
  Calendar.Show;
  Calendar.SetFocus;
  SetCapture(Calendar.Handle);
end;
{=====}

procedure TVpCustomDateEdit.PopupDateChange(Sender : TObject; Date : TDateTime);
begin
  {get the current value}
  SetDate(Calendar.Date);
  Modified := True;

  if Calendar.Browsing then
    Exit;

  {hide the Calendar}
  PopupClose(Sender);
  SetFocus;
  SelStart := Length(Text);
  SelLength := 0;
end;
{=====}

procedure TVpCustomDateEdit.PopupKeyDown(Sender : TObject; var Key : Word;
  Shift : TShiftState);
var
  X : Integer;
begin
  case Key of
    VK_UP : if Shift = [ssAlt] then begin
              PopupClose(Sender);
              X := SelStart;
              SetFocus;
              SelStart := X;
              SelLength := 0;
            end;
  end;
end;
{=====}

procedure TVpCustomDateEdit.PopupKeyPress(Sender : TObject; var Key : Char);
var
  X : Integer;
begin
  case Key of
    #27 :
      begin
        PopupClose(Sender);
        X := SelStart;
        SetFocus;
        SelStart := X;
        SelLength := 0;
      end;
  end;
end;
{=====}

procedure TVpCustomDateEdit.SetDate(Value : TDateTime);
begin
  FDate := Value;
  Modified := True;

  if FDate = 0 then
    Text := ''
  else
    Text := FormatDate(FDate);

  if Assigned(FOnSetDate) then
    FOnSetDate(Self);
end;
{=====}

procedure TVpCustomDateEdit.SetDateText(Value : string);
var
  Field      : Integer;
  I1         : Integer;
  I2         : Integer;
  Error      : Integer;
  ThisYear   : Word;
  ThisMonth  : Word;
  ThisDay    : Word;
  Year       : Word;
  Month      : Word;
  Day        : Word;
  EpochYear  : Integer;
  EpochCent  : Integer;
  StringList : TStringList;
  FieldOrder : string[3];
  S          : string;
const
  ErrorConvertingMonthNumber = 1;
  ErrorConvertingMonthName = 2;
  ErrorConvertingYear = 3;
  ErrorConvertingDay = 4;
  MonthIsRequired = 5;
  DayIsRequired = 6;
  YearIsRequired = 7;
begin
  if Assigned(FOnGetDate) then
    FOnGetDate(Self, Value);

  if (Value = '') and (RequiredFields <> []) then begin
    FDate := 0;
    if not GettingDate then
      Text := '';
    Exit;
  end;

  if AnsiCompareText(Value, TodayString) = 0 then begin
    FDate := SysUtils.Date;
    if not GettingDate then begin
      Text := FormatDate(FDate);
      Modified := True;
    end;
  end else begin
    DecodeDate(SysUtils.Date, ThisYear, ThisMonth, ThisDay);
    Value := UpperCase(Value);
    StringList := TStringList.Create;
    try
      {parse the string into subfields using a string list to hold the parts}
      I1 := 1;
      while (I1 <= Length(Value)) and not (Value[I1] in ['0'..'9', 'A'..'Z']) do
        Inc(I1);
      while I1 <= Length(Value) do begin
        I2 := I1;
        while (I2 <= Length(Value)) and (Value[I2] in ['0'..'9', 'A'..'Z']) do
          Inc(I2);
        StringList.Add(Copy(Value, I1, I2-I1));
        while (I2 <= Length(Value)) and not (Value[I2] in ['0'..'9', 'A'..'Z']) do
          Inc(I2);
        I1 := I2;
      end;

      case DateOrder of
        doMDY : FieldOrder := 'MDY';
        doDMY : FieldOrder := 'DMY';
        doYMD : FieldOrder := 'YMD';
      end;

      Year := 0;
      Month := 0;
      Day := 0;
      Error := 0;
      for Field := 1 to Length(FieldOrder) do begin
        if StringList.Count > 0 then
          S := StringList[0]
        else
          S := '';

        case FieldOrder[Field] of
          'M' :
            begin
              {numeric month}
              if (S = '') or (S[1] in ['0'..'9']) then begin
                try
                  if S = '' then
                    Month := 0
                  else
                    Month := StrToInt(S);
                except
                  Month := 0;
                  {error converting month number}
                  Error := ErrorConvertingMonthNumber;
                end;
                if not (Month in [1..12]) then
                  Month := 0;
              end else begin
                {one or more letters in month}
                Month := 0;
                I1 := 1;
                S := Copy(S, 1, 3);
                {error converting month name}
                Error := ErrorConvertingMonthName;
                repeat
                  if S = UpperCase(Copy(ShortMonthNames[I1], 1, Length(S))) then begin
                    Month := I1;
                    I1 := 13;
                    Error := 0;
                  end else
                    Inc(I1);
                until I1 = 13;
              end;

              if Month = 0 then begin
                if rfMonth in FRequiredFields then
                  {month required}
                  Error := MonthIsRequired
                else
                  Month := ThisMonth;
              end else if StringList.Count > 0 then
                StringList.Delete(0);

              if Error > 0 then
                Break;
            end;
          'Y' :
            begin
              try
                if S = '' then
                  Year := 0
                else
                  Year := StrToInt(S);
              except
                Year := 0;
                {error converting year}
                Error := ErrorConvertingYear;
              end;
              if (FEpoch = 0) and (Year < 100) and (S <> '') then
                {default to current century if Epoch is zero}
                Year := Year + (ThisYear div 100 * 100)
              else if (FEpoch > 0) and (Year < 100) and (S <> '') then begin
                {use epoch}
                EpochYear := FEpoch mod 100;
                EpochCent := (FEpoch div 100) * 100;
                if (Year < EpochYear) then
                  Inc(Year,EpochCent+100)
                else
                  Inc(Year,EpochCent);
              end;
              if Year = 0 then begin
                if rfYear in FRequiredFields then
                  {year is required}
                  Error := YearIsRequired
                else
                  Year := ThisYear;
              end else if StringList.Count > 0 then
                StringList.Delete(0);

              if Error > 0 then
                Break;
            end;
          'D' :
            begin
              try
                if S = '' then
                  Day := 0
                else
                  Day := StrToInt(S);
              except
                Day := 0;
                {error converting day}
                Error := ErrorConvertingDay;
              end;
              if not (Day in [1..31]) then
                Day := 0;
              if Day = 0 then begin
                if rfDay in FRequiredFields then
                  {day is required}
                  Error := DayIsRequired
                else
                  Day := ThisDay;
                end
              else if StringList.Count > 0 then
                StringList.Delete(0);

              if Error > 0 then
                Break;
            end;
        end;
      end;
      case Error of
        ErrorConvertingDay :
          if S = '' then
            raise EVpDateEditError.Create(RSInvalidDay + ' "' + Value + '"')
          else
            raise EVpDateEditError.Create(RSInvalidDay + ' "' + S + '"');
        ErrorConvertingMonthNumber :
          if S = '' then
            raise EVpDateEditError.Create(RSInvalidMonth + ' "' + Value + '"')
          else
            raise EVpDateEditError.Create(RSInvalidMonth + ' "' + S + '"');
        ErrorConvertingMonthName :
          if S = '' then
            raise EVpDateEditError.Create(RSInvalidMonthName + ' "' + Value + '"')
          else
            raise EVpDateEditError.Create(RSInvalidMonthName + ' "' + S + '"');
        ErrorConvertingYear :
          if S = '' then
            raise EVpDateEditError.Create(RSInvalidYear + ' "' + Value + '"')
          else
            raise EVpDateEditError.Create(RSInvalidYear + ' "' + S + '"');
        DayIsRequired :
          raise EVpDateEditError.Create(RSDayIsRequired);
        MonthIsRequired :
          raise EVpDateEditError.Create(RSMonthIsRequired);
        YearIsRequired :
          raise EVpDateEditError.Create(RSYearIsRequired);
      end;

      try
        FDate := EncodeDate(Year, Month, Day);
        if not GettingDate then
          Text := FormatDate(FDate);
      except
        raise EVpDateEditError.Create(RSInvalidDate + ' "' + Value + '"');
      end;

    finally
      StringList.Free;
    end;
  end;
end;
{=====}

procedure TVpCustomDateEdit.SetForceCentury(Value : Boolean);
begin
  if Value <> FForceCentury then begin
    FForceCentury := Value;
    if Assigned(Calendar) then
      SetDate(Calendar.Date);
  end;
end;
{=====}

procedure TVpCustomDateEdit.SetPopupCalFont(Value : TFont);
begin
  if Assigned(Value) then
    FPopupCalFont.Assign(Value);
end;
{=====}

procedure TVpCustomDateEdit.SetReadOnly(Value : Boolean);
begin
  inherited ReadOnly := Value;
  FButton.Enabled := not ReadOnly;
end;
{=====}

end.
