
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

unit EsEdCal;
  {-date edit field with popup calendar}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Buttons, Classes, Controls, Forms, Graphics, Menus, Messages,
  StdCtrls, SysUtils,
  EsBase, EsCal, EsConst, EsEdPop, EsUtil;

type
  TEsDateOrder = (doMDY, doDMY, doYMD);
  TEsRequiredField = (rfYear, rfMonth, rfDay);
  TEsRequiredFields = set of TEsRequiredField;

  TEsGetDateEvent = procedure(Sender : TObject; var Value : string)
    of object;

  TEsCustomDateEdit = class(TEsEdPopup)
  protected {private}
    {.Z+}
    {property variables}
    FAllowIncDec    : Boolean;
    FDate           : TDateTime;
    FEpoch          : Integer;                                         {!!.04}
    FForceCentury   : Boolean;                                         {!!.02}
    FPopupCalColors : TEsCalColors;
    FPopupCalFont   : TFont;
    FPopupCalHeight : Integer;
    FPopupCalWidth  : Integer;
    FRequiredFields : TEsRequiredFields;
    FTodayString    : string;
    FWeekStarts     : TEsDayType;     {the day that begins the week}   {!!.02}

    {event variables}
    FOnGetDate      : TEsGetDateEvent;
    FOnSetDate      : TNotifyEvent;

    {internal variables}
    Calendar        : TEsCalendar;
    DateOrder       : TEsDateOrder;
    GettingDate     : Boolean;                                         {!!.06}
    HoldCursor      : TCursor;                                         {!!.04}
    WasAutoScroll   : Boolean;

    {property methods}
    function GetDate : TDateTime;                                      {!!.04}
    function GetReadOnly : Boolean;
    procedure SetForceCentury(Value : Boolean);                        {!!.02}
    procedure SetPopupCalFont(Value : TFont);
    procedure SetReadOnly(Value : Boolean);

    {internal methods}
    procedure PopupDateChange(Sender : TObject; Date : TDateTime);
    procedure PopupKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure PopupKeyPress(Sender : TObject; var Key : Char);
    procedure PopupMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    {.Z-}

  protected
    {.Z+}
    procedure DoExit;
      override;
    procedure KeyDown(var Key : Word; Shift : TShiftState);
      override;
    procedure KeyPress(var Key : Char);
      override;
    procedure PopupClose(Sender : TObject);
      override;
    procedure SetDate(Value : TDateTime);
    procedure SetDateText(Value : string);
      dynamic;
    {.Z-}

    {protected properties}
    property AllowIncDec : Boolean
      read FAllowIncDec
      write FAllowIncDec
      default True;

    property Epoch : Integer                                           {!!.04}
      read FEpoch
      write FEpoch;

    property ForceCentury : Boolean                                    {!!.02}
      read FForceCentury
      write SetForceCentury
      default False;

    property PopupCalColors : TEsCalColors
      read FPopupCalColors
      write FPopupCalColors;

    property PopupCalFont : TFont
      read FPopupCalFont
      write SetPopupCalFont;

    property PopupCalHeight : Integer
      read FPopupCalHeight
      write FPopupCalHeight
      default calDefHeight;

    property PopupCalWidth : Integer
      read FPopupCalWidth
      write FPopupCalWidth
      default calDefWidth;

    property ReadOnly : Boolean
      read GetReadOnly
      write SetReadOnly;

    property RequiredFields : TEsRequiredFields
      read FRequiredFields
      write FRequiredFields;

    property TodayString : string
      read FTodayString
      write FTodayString;

    property WeekStarts : TEsDayType                                   {!!.02}
      read FWeekStarts
      write FWeekStarts
      default calDefWeekStarts;

    {protected events}
    property OnGetDate : TEsGetDateEvent
      read FOnGetDate
      write FOnGetDate;

    property OnSetDate : TNotifyEvent
      read FOnSetDate
      write FOnSetDate;

  public
    {.Z+}
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
    procedure PopupOpen;
      override;
    {.Z-}

    function FormatDate(Value : TDateTime) : string;
      dynamic;

    {public properties}
    property Date : TDateTime
      read GetDate                                                     {!!.04}
      write SetDate;
  end;

  TEsDateEdit = class(TEsCustomDateEdit)
  published
    {properties}
    {$IFDEF VERSION4}                                                {!!.06}
    property Anchors;                                                {!!.06}
    property Constraints;                                            {!!.06}
    property DragKind;                                               {!!.06}
    {$ENDIF}                                                         {!!.06}
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
    property Epoch;                                                    {!!.04}
    property EsLabelInfo;
    property Font;
    property ForceCentury;                                             {!!.02}
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
    property WeekStarts;                                               {!!.02}

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
    {$IFDEF Win32}
    property OnStartDrag;
    {$ENDIF Win32}
  end;


implementation


{*** TEsCustomDateEdit ***}

constructor TEsCustomDateEdit.Create(AOwner : TComponent);
var
  C : array[0..1] of Char;
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csSetCaption];

  FAllowIncDec := True;
  FEpoch := DefaultEpoch;
  FForceCentury := False;                                              {!!.02}
  FRequiredFields := [rfMonth, rfDay];
  FTodayString := DateSeparator;
  FPopupCalHeight := calDefHeight;
  FPopupCalWidth := calDefWidth;
  FPopupCalFont := TFont.Create;
  FPopupCalFont.Assign(Font);

  {get the date order from windows}
  C[0] := '0'; {default}
  GetProfileString('intl', 'iDate', '0', C, 2);
  DateOrder := TEsDateOrder(Ord(C[0])-Ord('0'));

  {load button glyph}
  FButton.Glyph.Handle := LoadBitmap(HInstance, 'ESSMALLDOWNARROW');

  {create color class}
  FPopupCalColors := TEsCalColors.Create;
  {assign default color scheme}
  FPopupCalColors.FCalColors := CalScheme[csWindows];
  FPopupCalColors.FColorScheme := csWindows;

  GettingDate := False;                                                {!!.06}

end;

destructor TEsCustomDateEdit.Destroy;
begin
  FPopupCalColors.Free;
  FPopupCalColors := nil;

  FPopupCalFont.Free;
  FPopupCalFont := nil;

  inherited Destroy;
end;

procedure TEsCustomDateEdit.DoExit;
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

{!!.04}
function TEsCustomDateEdit.GetDate : TDateTime;
begin
  GettingDate := True;                                                 {!!.06}
  try
    SetDateText(Text);
  finally
    GettingDate := False;
  end;
  Result := FDate;
end;

function TEsCustomDateEdit.GetReadOnly : Boolean;
begin
  Result := inherited ReadOnly;
end;

procedure TEsCustomDateEdit.KeyDown(var Key : Word; Shift : TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if (Key = VK_DOWN) and (ssAlt in Shift) then
    PopupOpen;
end;

procedure TEsCustomDateEdit.KeyPress(var Key : Char);
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
    Modified := True;                                                  {!!.05}

    Key := #0; {clear}
  end;
end;

{!!.02} {revised}
function TEsCustomDateEdit.FormatDate(Value : TDateTime) : string;
var
  S : string;
begin
  S := ShortDateFormat;
  if FForceCentury then
    if Pos('yyyy', S) = 0 then
      Insert('yy', S, Pos('yy', S));
  Result := FormatDateTime(S, Value)                                   {!!.06}
end;

procedure TEsCustomDateEdit.PopupClose(Sender : TObject);
begin
  inherited PopupClose(Sender);

  if GetCapture = Calendar.Handle then
    ReleaseCapture;

  SetFocus;                                                            {!!.05}
  Calendar.Hide;  {hide the Calendar}

  if (Calendar.Parent <> nil) then                                     {!!.06}
    if (Calendar.Parent is TForm) then                                 {!!.05}
      TForm(Calendar.Parent).AutoScroll := WasAutoScroll               {!!.05}
    else if (Calendar.Parent is TScrollBox) then                       {!!.06}
      TScrollBox(Calendar.Parent).AutoScroll := WasAutoScroll;         {!!.06}
  Cursor := HoldCursor;                                                {!!.04}

  {change parentage so that we control the window handle destruction}  {!!.04}
  Calendar.Parent := Self;                                             {!!.04}
end;

procedure TEsCustomDateEdit.PopupKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
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

procedure TEsCustomDateEdit.PopupKeyPress(Sender : TObject; var Key : Char);
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

procedure TEsCustomDateEdit.PopupMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
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

procedure TEsCustomDateEdit.PopupOpen;
var
  P : TPoint;
  {$IFDEF Win32}
  R : TRect;                                                           {!!.04}
  {$ENDIF}
begin
  inherited PopupOpen;

  DoExit;    {force update of date}

  if not Assigned(Calendar) then begin
    Calendar := TEsCalendar.CreateEx(Self, True);
    Calendar.OnChange := PopupDateChange;
    Calendar.OnExit := PopupClose;
    Calendar.OnKeyDown := PopupKeyDown;
    Calendar.OnKeyPress := PopupKeyPress;
    Calendar.OnMouseDown := PopupMouseDown;
    Calendar.Visible := False; {to avoid flash at 0,0}
    Calendar.BorderStyle := bsSingle;
    Calendar.Height := FPopupCalHeight;
    Calendar.Width := FPopupCalWidth;
    Calendar.WeekStarts := FWeekStarts;                                {!!.02}
    Calendar.ParentCtl3D := False;                                     {!!.02}
    Calendar.Ctl3D := Ctl3D;                                           {!!.02}
    Calendar.Font.Assign(FPopupCalFont);
  end;

  if (Parent.Parent <> nil) then                                     {!!.08}
    Calendar.Parent := Parent.Parent                                 {!!.08}
  else if Parent <> nil then                                         {!!.08}
    Calendar.Parent := Parent
  else
    Calendar.Parent := GetParentForm(Self);

  if (Calendar.Parent <> nil) then                                     {!!.06}
    if (Calendar.Parent is TForm) then begin                           {!!.05}
      WasAutoScroll := TForm(Calendar.Parent).AutoScroll;
      TForm(Calendar.Parent).AutoScroll := False;
    end else if (Calendar.Parent is TScrollBox) then begin             {!!.06}
      WasAutoScroll := TScrollBox(Calendar.Parent).AutoScroll;         {!!.06}
      TScrollBox(Calendar.Parent).AutoScroll := False;                 {!!.06}
    end;

  {set colors}
  Calendar.Colors.Assign(FPopupCalColors);
  {determine the proper position}
  {$IFDEF Win32}
  P := ClientToScreen(Point(-2, Height-2));
  {$ELSE}
  P := ClientToScreen(Point(0, Height));
  {$ENDIF}

  {!!.04}
  {$IFDEF Win32}
  SystemParametersInfo(SPI_GETWORKAREA, 0, @R, 0);
  if P.Y + Calendar.Height >= R.Bottom then
    P.Y := P.Y - Calendar.Height - Height - 2;
  if P.X + Calendar.Width >= R.Right then
    P.X := R.Right - Calendar.Width - 1;
  {$ELSE}
  if P.Y + Calendar.Height >= Screen.Height then
    P.Y := P.Y - Calendar.Height - Height - 2;
  if P.X + Calendar.Width >= Screen.Width then
    P.X := Screen.Width - Calendar.Width - 1;
  {$ENDIF}

  MoveWindow(Calendar.Handle, P.X, P.Y, Calendar.Width, Calendar.Height, False);

  if Text = '' then
    Calendar.Date := SysUtils.Date
  else
    Calendar.Date := FDate;

  HoldCursor := Cursor;                                                {!!.04}
  Cursor := crArrow;                                                   {!!.04}
  Calendar.Show;
  Calendar.SetFocus;
  SetCapture(Calendar.Handle);
end;

procedure TEsCustomDateEdit.PopupDateChange(Sender : TObject; Date : TDateTime);
begin
  {get the current value}
  SetDate(Calendar.Date);
  Modified := True;                                                    {!!.04}

  if Calendar.Browsing then                                            {!!.04}
    Exit;                                                              {!!.04}

  {hide the Calendar}
  PopupClose(Sender);
  SetFocus;
  SelStart := Length(Text);
  SelLength := 0;
end;

procedure TEsCustomDateEdit.SetDate(Value : TDateTime);
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

procedure TEsCustomDateEdit.SetDateText(Value : string);
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
  EpochYear  : Integer;                                                {!!.04}
  EpochCent  : Integer;                                                {!!.04}
  StringList : TStringList;
  FieldOrder : string[3];
  S          : string;
begin
  if Assigned(FOnGetDate) then
    FOnGetDate(Self, Value);

  if (Value = '') and (RequiredFields <> []) then begin
    FDate := 0;
    if not GettingDate then                                            {!!.06}
      Text := '';
    Exit;
  end;

  if AnsiCompareText(Value, TodayString) = 0 then begin
    FDate := SysUtils.Date;
    if not GettingDate then begin                                      {!!.06}
      Text := FormatDate(FDate);
      Modified := True;                                                {!!.08}
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
              if (S = '') or (S[1] in ['0'..'9']) then begin {numeric month}
                try
                  if S = '' then
                    Month := 0
                  else
                    Month := StrToInt(S);
                except
                  Month := 0;
                  Error := SCEsMonthConvertError; {error converting month number}
                end;
                if not (Month in [1..12]) then
                  Month := 0;
              end else begin {one or more letters in month}
                Month := 0;
                I1 := 1;
                S := Copy(S, 1, 3);
                Error := SCEsMonthNameConvertError; {error converting month name}
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
                  Error := SCEsMonthRequired {month required}
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
                Error := SCEsYearConvertError; {error converting year}
              end;
              {!!.04}
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
                  Error := SCEsYearRequired {year is required}
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
                Error := SCEsDayConvertError; {error converting day}
              end;
              if not (Day in [1..31]) then
                Day := 0;
              if Day = 0 then begin
                if rfDay in FRequiredFields then
                  Error := SCEsDayRequired {day is required}
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
        SCEsDayConvertError :
          if S = '' then
            raise EEssentialsError.Create(StrRes[SCEsInvalidDay] + ' "' + Value + '"')
          else
            raise EEssentialsError.Create(StrRes[SCEsInvalidDay] + ' "' + S + '"');
        SCEsMonthConvertError :
          if S = '' then
            raise EEssentialsError.Create(StrRes[SCEsInvalidMonth] + ' "' + Value + '"')
          else
            raise EEssentialsError.Create(StrRes[SCEsInvalidMonth] + ' "' + S + '"');
        SCEsMonthNameConvertError :
          if S = '' then
            raise EEssentialsError.Create(StrRes[SCEsInvalidMonthName] + ' "' + Value + '"')
          else
            raise EEssentialsError.Create(StrRes[SCEsInvalidMonthName] + ' "' + S + '"');
        SCEsYearConvertError :
          if S = '' then
            raise EEssentialsError.Create(StrRes[SCEsInvalidYear] + ' "' + Value + '"')
          else
            raise EEssentialsError.Create(StrRes[SCEsInvalidYear] + ' "' + S + '"');
        SCEsDayRequired :
          raise EEssentialsError.Create(StrRes[SCEsDayIsRequired]);
        SCEsMonthRequired :
          raise EEssentialsError.Create(StrRes[SCEsMonthIsRequired]);
        SCEsYearRequired :
          raise EEssentialsError.Create(StrRes[SCEsYearIsRequired]);
      end;

      try
        FDate := EncodeDate(Year, Month, Day);
        if not GettingDate then                                        {!!.06}
          Text := FormatDate(FDate);
      except
        raise EEssentialsError.Create(StrRes[SCEsInvalidDate] + ' "' + Value + '"');
      end;

    finally
      StringList.Free;
    end;
  end;
end;

{!!.02}
procedure TEsCustomDateEdit.SetForceCentury(Value : Boolean);
begin
  if Value <> FForceCentury then begin
    FForceCentury := Value;
    if Assigned(Calendar) then                                         {!!.03}
      SetDate(Calendar.Date);
  end;
end;

procedure TEsCustomDateEdit.SetPopupCalFont(Value : TFont);
begin
  if Assigned(Value) then
    FPopupCalFont.Assign(Value);
end;

procedure TEsCustomDateEdit.SetReadOnly(Value : Boolean);
begin
  inherited ReadOnly := Value;
  FButton.Enabled := not ReadOnly;
end;

end.