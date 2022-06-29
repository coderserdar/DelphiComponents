{   Component(s):
    tcyEditDate

    Description:
    A Edit for dates

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyEditDate;

interface

uses Classes, Windows, Controls, StdCtrls, SysUtils, DateUtils, cyEdit;

type
  TProcEditDateNeedDefaultValue = procedure (Sender: TObject; var Value: TDate) of object;
  TEditDateArrangement = (dtDayMonthYear4Digits, dtYear4DigitsMonthDay, dtMonthDayYear4Digits, dtDayMonthYear2Digits, dtYear2DigitsMonthDay, dtMonthDayYear2Digits);

  TcyEditDate = class(TcyAdvBaseEdit)
  private
    FSystemDateSeparator: Char;
    FAutoComplete: Boolean;
    FCustomDateSeparator: String;
    FDateArrangement: TEditDateArrangement;
    FOnNeedDefaultValue: TProcEditDateNeedDefaultValue;
    FMinValue: TDate;
    FMaxValue: TDate;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetValue: TDate;
    procedure SetValue(const Value: TDate);
    procedure SetCustomDateSeparator(const Value: String);
    procedure SetDateArrangement(const Value: TEditDateArrangement);
    procedure SetAutoComplete(const Value: Boolean);
  protected
    procedure UpdateDateRules;
  public
    constructor Create(AOwner: TComponent); override;
    function ValidateText(aText: String): TEditValidateResult; override;
    procedure ApplyDateFieldDisplayFormat(const DisplayFormat: string);
    procedure GetDateDetailsFromText(aText: String; var StrYear, StrMonth, StrDay: String); overload;
    procedure GetDateDetailsFromText(aText: String; var Year, Month, Day: Integer); overload;
    property Value: TDate read GetValue write SetValue;
    property IgnorePressedKey;
    property MaxChars;
  published
    property AutoComplete: Boolean read FAutoComplete write SetAutoComplete default true;
    property MinValue: TDate read FMinValue write FMinValue;
    property MaxValue: TDate read FMaxValue write FMaxValue;
    property CustomDateSeparator: String read FCustomDateSeparator write SetCustomDateSeparator;
    property DateArrangement: TEditDateArrangement read FDateArrangement write SetDateArrangement default dtDayMonthYear4Digits;
    property OnNeedDefaultValue: TProcEditDateNeedDefaultValue read FOnNeedDefaultValue write FOnNeedDefaultValue;
    // Herited :
    // Do not publish ... property CharRules;
    property AllowEmpty;
    property ChangeDelayInterval;
    property IgnoreRules;
    property ErrorHandling;
    property OnChangeDelay;
    property OnForbiddenCharPressed;
    property OnNotAllowedCharPressed;
    property OnValidateError;
  end;

implementation

{ TcyEditDate }
constructor TcyEditDate.Create(AOwner: TComponent);
var
  fs: TFormatSettings;
begin
  inherited;

  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, fs);
  FSystemDateSeparator := fs.DateSeparator;
  FDateArrangement := dtDayMonthYear4Digits;

  FAutoComplete := true;
  FCustomDateSeparator := '';
  FMinValue := 0;
  FMaxValue := 0;

  UpdateDateRules;
end;

procedure TcyEditDate.UpdateDateRules;
var
  DateSymbol: String;

      procedure CreateDayRules(_fromPosition: Integer);
      begin
        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition; AllowedChars := '0123'; end;
        with CharRules.Add do begin FromPosition := _fromPosition+1; ToPosition := _fromPosition+1; AllowedChars := '0123456789'; end;

        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition+1; ForbiddenCharsSequence := '00'; end;
        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition+1; ForbiddenCharsSequence := '32'; end;
        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition+1; ForbiddenCharsSequence := '33'; end;
        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition+1; ForbiddenCharsSequence := '34'; end;
        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition+1; ForbiddenCharsSequence := '35'; end;
        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition+1; ForbiddenCharsSequence := '36'; end;
        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition+1; ForbiddenCharsSequence := '37'; end;
        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition+1; ForbiddenCharsSequence := '38'; end;
        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition+1; ForbiddenCharsSequence := '39'; end;
      end;

      procedure CreateMonthRules(_fromPosition: Integer);
      begin
        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition; AllowedChars := '01'; end;
        with CharRules.Add do begin FromPosition := _fromPosition+1; ToPosition := _fromPosition+1; AllowedChars := '0123456789'; end;

        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition+1; ForbiddenCharsSequence := '00'; end;   // 2016-03-29
        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition+1; ForbiddenCharsSequence := '13'; end;
        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition+1; ForbiddenCharsSequence := '14'; end;
        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition+1; ForbiddenCharsSequence := '15'; end;
        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition+1; ForbiddenCharsSequence := '16'; end;
        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition+1; ForbiddenCharsSequence := '17'; end;
        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition+1; ForbiddenCharsSequence := '18'; end;
        with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition+1; ForbiddenCharsSequence := '19'; end;
      end;

      procedure CreateYearRules(_fromPosition: Integer);
      begin
        if FDateArrangement in [dtDayMonthYear4Digits, dtYear4DigitsMonthDay, dtMonthDayYear4Digits] then
        begin
          if AutoComplete then  // 2017-01-05 ...
          begin
            with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition; AllowedChars := '0123456789'; end; // 2017-01-05 ...
          end
          else
            with CharRules.Add do begin FromPosition := _fromPosition; ToPosition := _fromPosition; AllowedChars := '12'; end;

          with CharRules.Add do begin FromPosition := _fromPosition+1; ToPosition := _fromPosition+3; AllowedChars := '0123456789'; end;
        end
        else
          with CharRules.Add do begin FromPosition := fromPosition; ToPosition := fromPosition+1; AllowedChars := '0123456789'; end;
      end;

begin
  if FCustomDateSeparator <> ''
  then DateSymbol := FCustomDateSeparator
  else DateSymbol := FSystemDateSeparator;

  CharRules.Clear;

  case FDateArrangement of
    dtDayMonthYear4Digits, dtDayMonthYear2Digits:  // DD MM YY(YY)
    begin
      CreateDayRules(1);
      with CharRules.Add do begin FromPosition := 3; ToPosition := 3; AllowedChars := DateSymbol; AutoInsertChars := AllowedChars; end;
      CreateMonthRules(4);
      with CharRules.Add do begin FromPosition := 6; ToPosition := 6; AllowedChars := DateSymbol; AutoInsertChars := AllowedChars; end;
      CreateYearRules(7);
    end;

    dtYear4DigitsMonthDay, dtYear2DigitsMonthDay:  // YY(YY) MM DD
    begin
      CreateYearRules(1);
      if FDateArrangement = dtYear4DigitsMonthDay then
      begin
        with CharRules.Add do begin FromPosition := 5; ToPosition := 5; AllowedChars := DateSymbol; AutoInsertChars := AllowedChars; end;
        CreateMonthRules(6);
        with CharRules.Add do begin FromPosition := 8; ToPosition := 8; AllowedChars := DateSymbol; AutoInsertChars := AllowedChars; end;
        CreateDayRules(9);
      end
      else begin
        with CharRules.Add do begin FromPosition := 3; ToPosition := 3; AllowedChars := DateSymbol; AutoInsertChars := AllowedChars; end;
        CreateMonthRules(4);
        with CharRules.Add do begin FromPosition := 6; ToPosition := 6; AllowedChars := DateSymbol; AutoInsertChars := AllowedChars; end;
        CreateDayRules(7);
      end;
    end;

    dtMonthDayYear4Digits, dtMonthDayYear2Digits:  // MM DD YY(YY)
    begin
      CreateMonthRules(1);
      with CharRules.Add do begin FromPosition := 3; ToPosition := 3; AllowedChars := DateSymbol; AutoInsertChars := AllowedChars; end;
      CreateDayRules(4);
      with CharRules.Add do begin FromPosition := 6; ToPosition := 6; AllowedChars := DateSymbol; AutoInsertChars := AllowedChars; end;
      CreateYearRules(7);
    end;
  end;

  if FDateArrangement in [dtDayMonthYear4Digits, dtYear4DigitsMonthDay, dtMonthDayYear4Digits] then
  begin
    with CharRules.Add do begin FromPosition := 11; ToPosition := 11; AllowedChars := #0; AutoInsertChars := ''; end;
  end
  else
    with CharRules.Add do begin FromPosition := 9; ToPosition := 9; AllowedChars := #0; AutoInsertChars := ''; end;
end;

procedure TcyEditDate.SetAutoComplete(const Value: Boolean);
begin
  if FAutoComplete = Value then Exit;

  FAutoComplete := Value;
  UpdateDateRules;
end;

procedure TcyEditDate.SetCustomDateSeparator(const Value: String);
var
  NewValue: String;
  Sav: TDate;
  UpdateText: Boolean;
begin
  if Value <> ''
  then NewValue := Value[1]
  else NewValue := '';

  if FCustomDateSeparator = NewValue then Exit;

  UpdateText := false;

  if Text <> '' then
    if IsValidValue then
    begin
      Sav := Self.Value;
      UpdateText := true;
    end;

  FCustomDateSeparator := NewValue;
  UpdateDateRules;

  if UpdateText then
    Self.Value := Sav;
end;

procedure TcyEditDate.SetDateArrangement(const Value: TEditDateArrangement);
var
  Sav: TDate;
  UpdateText: Boolean;
begin
  if FDateArrangement = Value then Exit;

  UpdateText := false;

  if Text <> '' then
    if IsValidValue then
    begin
      Sav := Self.Value;
      UpdateText := true;
    end;

  FDateArrangement := Value;
  UpdateDateRules;

  if UpdateText then
    Self.Value := Sav;
end;

procedure TcyEditDate.SetValue(const Value: TDate);
var
  Year, Month, Day: Word;
  DateSymbol: String;
begin
  DecodeDate(Value, Year, Month, Day);

  if FCustomDateSeparator <> ''
  then DateSymbol := FCustomDateSeparator
  else DateSymbol := FSystemDateSeparator;

  case FDateArrangement of
    dtDayMonthYear4Digits: Text := FormatFloat('00', Day)    + DateSymbol + FormatFloat('00', Month) + DateSymbol + FormatFloat('0000', Year);
    dtYear4DigitsMonthDay: Text := FormatFloat('0000', Year) + DateSymbol + FormatFloat('00', Month) + DateSymbol + FormatFloat('00', Day);
    dtMonthDayYear4Digits: Text := FormatFloat('00', Month)  + DateSymbol + FormatFloat('00', Day)   + DateSymbol + FormatFloat('0000', Year);
    dtDayMonthYear2Digits: Text := FormatFloat('00', Day)    + DateSymbol + FormatFloat('00', Month) + DateSymbol + Copy(FormatFloat('00', Year), 3, 2);
    dtYear2DigitsMonthDay: Text := Copy(FormatFloat('00', Year), 3, 2)   + DateSymbol + FormatFloat('00', Month) + DateSymbol + FormatFloat('00', Day);
    dtMonthDayYear2Digits: Text := FormatFloat('00', Month)  + DateSymbol + FormatFloat('00', Day)   + DateSymbol + Copy(FormatFloat('00', Year), 3, 2);
  end;
end;

procedure TcyEditDate.GetDateDetailsFromText(aText: String; var StrYear, StrMonth, StrDay: String);

    procedure ExtractDay(_fromPosition: Integer);
    begin
      StrDay := copy(aText, _fromPosition, 2);
    end;

    procedure ExtractMonth(_fromPosition: Integer);
    begin
      StrMonth := copy(aText, _fromPosition, 2);
    end;

    procedure ExtractYear(_fromPosition: Integer);
    var
      _Count: Integer;
    begin
      if FDateArrangement in [dtDayMonthYear4Digits, dtYear4DigitsMonthDay, dtMonthDayYear4Digits]
      then _Count := 4
      else _Count := 2;

      StrYear := copy(aText, _fromPosition, _Count);
    end;

begin
  StrYear  := '';
  StrMonth := '';
  StrDay   := '';

  case FDateArrangement of
    dtDayMonthYear4Digits, dtDayMonthYear2Digits:  // DD MM YY(YY)
    begin
      ExtractDay(1);
      ExtractMonth(4);
      ExtractYear(7);
    end;

    dtYear4DigitsMonthDay, dtYear2DigitsMonthDay:  // YY(YY) MM DD
    begin
      ExtractYear(1);
      if FDateArrangement = dtYear4DigitsMonthDay then
      begin
        ExtractMonth(6);
        ExtractDay(9);
      end
      else begin
        ExtractMonth(4);
        ExtractDay(7);
      end;
    end;

    dtMonthDayYear4Digits, dtMonthDayYear2Digits:  // MM DD YY(YY)
    begin
      ExtractMonth(1);
      ExtractDay(4);
      ExtractYear(7);
    end;
  end;
end;

procedure TcyEditDate.GetDateDetailsFromText(aText: String; var Year, Month, Day: Integer);
var
  StrYear, StrMonth, StrDay: String;
begin
  Year  := 0;
  Month := 0;
  Day   := 0;

  GetDateDetailsFromText(aText, StrYear, StrMonth, StrDay);

  if not TryStrToInt(StrYear, Year) then Exit;

  // Complete year :
  if (Year < 100) and (FDateArrangement in [dtDayMonthYear2Digits, dtYear2DigitsMonthDay, dtMonthDayYear2Digits]) then
    if Year < 30
    then Year := 2000 + Year
    else Year := 1900 + Year;


  if not TryStrToInt(StrMonth, Month) then Exit;
  if not TryStrToInt(StrDay, Day) then Exit;
end;

function TcyEditDate.GetValue: TDate;
var
  Year, Month, Day: Integer;
begin
  Result := 0;
  if Text = '' then Exit;

  GetDateDetailsFromText(Text, Year, Month, Day);

  try
    if (Year > 1900) and (Year < 9999) then
      if Month in [1..12] then
        if Day in [1..31] then
          Result := EncodeDate(Year, Month, Day);
  except
  end;
end;

function TcyEditDate.ValidateText(aText: String): TEditValidateResult;
var
  StrYear, StrMonth, StrDay: String;
  Year, Month, Day: Integer;
begin
  Result := evInvalidValue;
  if aText = '' then Exit;

  GetDateDetailsFromText(aText, StrYear, StrMonth, StrDay);

  if TryStrToInt(StrYear, Year) then
  begin
  // Complete year :
  if (Year < 100) and (FDateArrangement in [dtDayMonthYear2Digits, dtYear2DigitsMonthDay, dtMonthDayYear2Digits]) then
    if Year < 30
    then Year := 2000 + Year
    else Year := 1900 + Year;

    if TryStrToInt(StrMonth, Month) then
      if TryStrToInt(StrDay, Day) then
        try
          if (Year > 1900) and (Year < 9999) then
            if Month in [1..12] then
              if Day in [1..31] then
              begin
                EncodeDate(Year, Month, Day);
                Result := evValid;
              end;
        except
        end;
  end;
end;

procedure TcyEditDate.CMExit(var Message: TCMExit);
var
  DefaultValue: TDate;
  StrYear, StrMonth, StrDay: String;
  SavModified: Boolean;
begin
  if FAutoComplete then
  begin
    if (not AllowEmpty) and (Text = '') then
    begin
      DefaultValue := Date;

      if Assigned(FOnNeedDefaultValue) then
        FOnNeedDefaultValue(Self, DefaultValue);

      Value := DefaultValue;
    end
    else begin
      // Complete date :
      if (Length(Text) < MaxChars) and (Text <> '') then
      begin
        GetDateDetailsFromText(Text, StrYear, StrMonth, StrDay);

        if StrDay = '' then StrDay := IntToStr(DayOf(Date));
        if StrMonth = '' then StrMonth := IntToStr(MonthOf(Date));

        if StrYear = '' then
          StrYear := IntToStr(YearOf(Date));

        if Length(StrYear) = 2 then        // 2017-01-05 Complete year !
          if 2000 + StrToInt(StrYear) - 10 > YearOf(Date) // OLD ... if 2000 + StrToInt(StrYear) > YearOf(Date)
          then StrYear := '19' + StrYear
          else StrYear := '20' + StrYear;

        try
          if Value <> EncodeDate(StrToInt(StrYear), StrToInt(StrMonth), StrToInt(StrDay)) then
          begin
            SavModified := Self.Modified;
            Value := EncodeDate(StrToInt(StrYear), StrToInt(StrMonth), StrToInt(StrDay));
            Self.Modified := SavModified;
          end;
        except
        end;
      end;
    end;
  end;

  Inherited;
end;

procedure TcyEditDate.ApplyDateFieldDisplayFormat(const DisplayFormat: string);
var
  NewCustomDateSeparator: String;
  NewDateArrangement: TEditDateArrangement;
  Year4Digits: Boolean;
  i: Integer;
begin
  // Set DateArrangement and CustomDateSeparator :
  NewCustomDateSeparator := '';
  NewDateArrangement := DateArrangement;

  if DisplayFormat <> '' then
  begin
    for i := 1 to Length(DisplayFormat) do
      {$IFDEF UNICODE}
      if not CharInSet(DisplayFormat[i], [' ', 'd', 'm', 'y', 'D', 'M', 'Y']) then
      {$ELSE}
      if not (DisplayFormat[i] in [' ', 'd', 'm', 'y', 'D', 'M', 'Y']) then
      {$ENDIF}
      begin
        NewCustomDateSeparator := DisplayFormat[i];
        Break;
      end;

    Year4Digits := pos('yyyy', DisplayFormat) <> 0;

    case DisplayFormat[1] of
      'd', 'D':
      begin
        if Year4Digits
        then NewDateArrangement := dtDayMonthYear4Digits
        else NewDateArrangement := dtDayMonthYear2Digits;
      end;

      'm', 'M':
      begin
        if Year4Digits
        then NewDateArrangement := dtMonthDayYear4Digits
        else NewDateArrangement := dtMonthDayYear2Digits;
      end;

      'y', 'Y':
      begin
        if Year4Digits
        then NewDateArrangement := dtYear4DigitsMonthDay
        else NewDateArrangement := dtYear2DigitsMonthDay;
      end;
    end;
  end;

  DateArrangement := NewDateArrangement;
  CustomDateSeparator := NewCustomDateSeparator;
end;

end.
