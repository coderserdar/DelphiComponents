{   Component(s):
    tcyEditTime

    Description:
    A Edit for times

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

unit cyEditTime;

interface

uses Classes, Windows, Controls, StdCtrls, SysUtils, cyEdit;

type
  TProcEditTimeNeedDefaultValue = procedure (Sender: TObject; var Value: TDateTime) of object;

  TcyEditTime = class(TcyAdvBaseEdit)
  private
    FSystemHoursSymbol: Char;
    FSystemMinutesSymbol: Char;
    FSystemSecondsSymbol: String;
    FAutoComplete: Boolean;
    FCustomHoursSymbol: String;
    FCustomMinutesSymbol: String;
    FCustomSecondsSymbol: String;
    FSeconds: Boolean;
    FOnNeedDefaultValue: TProcEditTimeNeedDefaultValue;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetValue: TDateTime;
    procedure SetValue(const Value: TDateTime);
    procedure SetCustomHoursSymbol(const Value: String);
    procedure SetCustomMinutesSymbol(const Value: String);
    procedure SetCustomSecondsSymbol(const Value: String);
    procedure SetSeconds(const Value: Boolean);
  protected
    procedure UpdateTimeRules;
  public
    constructor Create(AOwner: TComponent); override;
    function ValidateText(aText: String): TEditValidateResult; override;
    procedure GetTimeDetailsFromText(aText: String; var Hour, Min, Sec: Integer);
    property Value: TDateTime read GetValue write SetValue;
    property IgnorePressedKey;
    property MaxChars;
  published
    property AutoComplete: Boolean read FAutoComplete write FAutoComplete default true;
    property CustomHoursSymbol: String read FCustomHoursSymbol write SetCustomHoursSymbol;
    property CustomMinutesSymbol: String read FCustomMinutesSymbol write SetCustomMinutesSymbol;
    property CustomSecondsSymbol: String read FCustomSecondsSymbol write SetCustomSecondsSymbol;
    property Seconds: Boolean read FSeconds write SetSeconds default false;
    property OnNeedDefaultValue: TProcEditTimeNeedDefaultValue read FOnNeedDefaultValue write FOnNeedDefaultValue;
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

{ TcyEditTime }
constructor TcyEditTime.Create(AOwner: TComponent);
var
  fs: TFormatSettings;
begin
  inherited;

  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, fs);
  FSystemHoursSymbol := fs.TimeSeparator;
  FSystemMinutesSymbol := fs.TimeSeparator;
  FSystemSecondsSymbol := '';   // No prefix

  FAutoComplete := true;
  FSeconds := false;
  FCustomHoursSymbol   := '';
  FCustomMinutesSymbol := '';
  FCustomSecondsSymbol := '';

  UpdateTimeRules;
end;

procedure TcyEditTime.UpdateTimeRules;
var
  HoursSymbol: String;
  MinutesSymbol: String;
  SecondsSymbol: String;
begin
  if FCustomHoursSymbol <> ''
  then HoursSymbol := FCustomHoursSymbol
  else HoursSymbol := FSystemHoursSymbol;

  if FCustomMinutesSymbol <> ''
  then MinutesSymbol := FCustomMinutesSymbol
  else MinutesSymbol := FSystemMinutesSymbol;

  if FCustomSecondsSymbol <> ''
  then SecondsSymbol := FCustomSecondsSymbol
  else SecondsSymbol := FSystemSecondsSymbol;

  CharRules.Clear;

  // Add rules :
  with CharRules.Add do begin FromPosition := 1; ToPosition := 1; AllowedChars := '012'; end;
  with CharRules.Add do begin FromPosition := 2; ToPosition := 2; AllowedChars := '0123456789'; end;
  with CharRules.Add do begin FromPosition := 3; ToPosition := 3; AllowedChars := HoursSymbol; AutoInsertChars := AllowedChars; end;
  with CharRules.Add do begin FromPosition := 4; ToPosition := 4; AllowedChars := '012345'; end;
  with CharRules.Add do begin FromPosition := 5; ToPosition := 5; AllowedChars := '0123456789'; end;

  if FSeconds or (FCustomMinutesSymbol <> '') then
    with CharRules.Add do begin FromPosition := 6; ToPosition := 6; AllowedChars := MinutesSymbol; AutoInsertChars := AllowedChars; end;

  if FSeconds then
  begin
    with CharRules.Add do begin FromPosition := 7; ToPosition := 7; AllowedChars := '012345'; end;
    with CharRules.Add do begin FromPosition := 8; ToPosition := 8; AllowedChars := '0123456789'; end;

    if SecondsSymbol <> '' then
    begin
      with CharRules.Add do begin FromPosition := 9; ToPosition := 9; AllowedChars := SecondsSymbol; AutoInsertChars := AllowedChars; end;
      with CharRules.Add do begin FromPosition :=10; ToPosition :=10; AllowedChars := #0; AutoInsertChars := ''; end;
    end
    else
      with CharRules.Add do begin FromPosition := 9; ToPosition := 9; AllowedChars := #0; AutoInsertChars := ''; end;
  end
  else
    if FCustomMinutesSymbol = '' then
    begin
      with CharRules.Add do begin FromPosition := 6; ToPosition := 6; AllowedChars := #0; AutoInsertChars := ''; end;
    end
    else begin
      with CharRules.Add do begin FromPosition := 7; ToPosition := 7; AllowedChars := #0; AutoInsertChars := ''; end;
    end;

  // Control hour input :
  with CharRules.Add do begin FromPosition := 1; ToPosition := 2; ForbiddenCharsSequence := '24'; end;
  with CharRules.Add do begin FromPosition := 1; ToPosition := 2; ForbiddenCharsSequence := '25'; end;
  with CharRules.Add do begin FromPosition := 1; ToPosition := 2; ForbiddenCharsSequence := '26'; end;
  with CharRules.Add do begin FromPosition := 1; ToPosition := 2; ForbiddenCharsSequence := '27'; end;
  with CharRules.Add do begin FromPosition := 1; ToPosition := 2; ForbiddenCharsSequence := '28'; end;
  with CharRules.Add do begin FromPosition := 1; ToPosition := 2; ForbiddenCharsSequence := '29'; end;
end;

procedure TcyEditTime.SetCustomHoursSymbol(const Value: String);
var
  NewValue: String;
  Sav: TDateTime;
  UpdateText: Boolean;
begin
  if Value <> ''
  then NewValue := Value[1]
  else NewValue := '';

  if FCustomHoursSymbol = NewValue then Exit;

  UpdateText := false;

  if Text <> '' then
    if IsValidValue then
    begin
      Sav := Self.Value;
      UpdateText := true;
    end;

  FCustomHoursSymbol := NewValue;
  UpdateTimeRules;

  if UpdateText then
    Self.Value := Sav;
end;

procedure TcyEditTime.SetCustomMinutesSymbol(const Value: String);
var
  NewValue: String;
  Sav: TDateTime;
  UpdateText: Boolean;
begin
  if Value <> ''
  then NewValue := Value[1]
  else NewValue := '';

  if FCustomMinutesSymbol = NewValue then Exit;

  UpdateText := false;

  if Text <> '' then
    if IsValidValue then
    begin
      Sav := Self.Value;
      UpdateText := true;
    end;

  FCustomMinutesSymbol := NewValue;
  UpdateTimeRules;

  if UpdateText then
    Self.Value := Sav;
end;

procedure TcyEditTime.SetCustomSecondsSymbol(const Value: String);
var
  NewValue: String;
  Sav: TDateTime;
  UpdateText: Boolean;
begin
  if Value <> ''
  then NewValue := Value[1]
  else NewValue := '';

  if FCustomSecondsSymbol = NewValue then Exit;

  UpdateText := false;

  if Text <> '' then
    if IsValidValue then
    begin
      Sav := Self.Value;
      UpdateText := true;
    end;

  FCustomSecondsSymbol := NewValue;
  UpdateTimeRules;

  if UpdateText then
    Self.Value := Sav;
end;

procedure TcyEditTime.SetSeconds(const Value: Boolean);
var
  Sav: TDateTime;
  UpdateText: Boolean;
begin
  if FSeconds = Value then Exit;

  UpdateText := false;

  if Text <> '' then
    if IsValidValue then
    begin
      Sav := Self.Value;
      UpdateText := true;
    end;

  FSeconds := Value;
  UpdateTimeRules;

  if UpdateText then
    Self.Value := Sav;
end;

procedure TcyEditTime.SetValue(const Value: TDateTime);
var
  Str: String;
  HoursSymbol: String;
  MinutesSymbol: String;
  SecondsSymbol: String;

  Hour, Min, Sec, MSec: Word;
begin
  if FCustomHoursSymbol <> ''
  then HoursSymbol := FCustomHoursSymbol
  else HoursSymbol := FSystemHoursSymbol;

  if FCustomMinutesSymbol <> ''
  then MinutesSymbol := FCustomMinutesSymbol
  else MinutesSymbol := FSystemMinutesSymbol;

  if FCustomSecondsSymbol <> ''
  then SecondsSymbol := FCustomSecondsSymbol
  else SecondsSymbol := FSystemSecondsSymbol;

  DecodeTime(Value, Hour, Min, Sec, MSec);
  Str := FormatFloat('00', Hour) + HoursSymbol + FormatFloat('00', Min);

  if FSeconds or (FCustomMinutesSymbol <> '') then
    Str := Str + MinutesSymbol;

  if FSeconds then
    Str := Str + FormatFloat('00', Sec) + SecondsSymbol;

  Text := Str;
end;

procedure TcyEditTime.GetTimeDetailsFromText(aText: String; var Hour, Min, Sec: Integer);
var
  StrHour, StrMin, StrSec: String;
begin
  Hour := 0;
  Min  := 0;
  Sec  := 0;

  StrHour := Copy(aText, 1, 2);
  StrMin := Copy(aText, 4, 2);
  StrSec := Copy(aText, 7, 2);

  if not TryStrToInt(StrHour, Hour) then Exit;
  if not TryStrToInt(StrMin, Min) then Exit;
  if not TryStrToInt(StrSec, Sec) then Exit;
end;

function TcyEditTime.GetValue: TDateTime;
var
  Hour, Min, Sec: Integer;
begin
  Result := 0;
  if Text = '' then Exit;

  GetTimeDetailsFromText(Text, Hour, Min, Sec);

  try
    if Hour in [0..23] then
      if Min in [0..59] then
        if Sec in [0..59] then
          Result := EncodeTime(Hour, Min, Sec, 0);
  except
  end;
end;

function TcyEditTime.ValidateText(aText: String): TEditValidateResult;
var
  StrHour, StrMin, StrSec: String;
  Hour, Min, Sec: Integer;
begin
  Result := evInvalidValue;
  if aText = '' then Exit;

  StrHour := Copy(aText, 1, 2);
  StrMin := Copy(aText, 4, 2);

  if FSeconds
  then StrSec := Copy(aText, 7, 2)
  else StrSec := '00';

  if TryStrToInt(StrHour, Hour) then
    if TryStrToInt(StrMin, Min) then
      if TryStrToInt(StrSec, Sec) then
        try
          if Hour in [0..23] then
            if Min in [0..59] then
              if Sec in [0..59] then
              begin
                EncodeTime(Hour, Min, Sec, 0);
                Result := evValid;
              end;
        except
        end;
end;

procedure TcyEditTime.CMExit(var Message: TCMExit);
var
  DefaultValue: TDateTime;
  Sav: TDateTime;
begin
  if FAutoComplete then
  begin
    if (not AllowEmpty) and (Text = '') then
    begin
      DefaultValue := Now;

      if Assigned(FOnNeedDefaultValue) then
        FOnNeedDefaultValue(Self, DefaultValue);

      Value := DefaultValue;
    end
    else begin
      // Complete time :
      if (Length(Text) < MaxChars) and (Text <> '') then
      begin
        Sav := Value;
        Value := Sav;
      end;
    end;
  end;

  Inherited;
end;

end.
