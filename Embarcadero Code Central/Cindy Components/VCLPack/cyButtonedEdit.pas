{   Component(s):
    tcyButtonedEdit

    Description:
    A ButonnedEdit with CharRules property to define several rules like:
    - Accepted chars (can specify position/range)
    - Reject chars sequences (can specify position/range)
    - Auto chars insertion at position
    - Max char repetition (can specify position/range)

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

unit cyButtonedEdit;

interface

uses Classes, Windows, Messages, Controls, StdCtrls, SysUtils, cyStrUtils, ExtCtrls, Clipbrd, cyEdit;

type
  TcyBaseButtonedEdit = class(TButtonedEdit)
  private
    FTimerChangeDelay: TTimer;
    SavedShift: TShiftState;
    AutoInsertCharsCount: Integer;
    FCharRules: tcyEditRules;
    FIgnorePressedKey: Boolean;
    FIgnoreRules: Boolean;
    FBreakLineReplace: String;
    FOnForbiddenCharPressed: TKeyPressEvent;
    FOnNotAllowedCharPressed: TKeyPressEvent;
    FOverridenCharCase: TEditCharCase;
    FChangeDelayInterval: Cardinal;
    FOnChangeDelay: TNotifyEvent;
    procedure SetCharRules(const Value: tcyEditRules);
    procedure SetIgnoreRules(const Value: Boolean);
    function GetMaxChars: Integer;
    procedure SetOverridenCharCase(const Value: TEditCharCase);
    procedure SetChangeDelayInterval(const Value: Cardinal);
  protected
    CanCallChange: Boolean;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure WMPaste(var Msg: TWMPaste); message WM_PASTE;
    procedure OnTimerChangeDelayEvent(Sender: TObject);
    // 2017-03-16 Override CharCase: TEditCharCase from TCustomEdit class component in order to avoir TextHint display in Lowercase / Uppercase !
    property CharCase: TEditCharCase read FOverridenCharCase write SetOverridenCharCase default ecNormal;
    property CharRules: tcyEditRules read FCharRules write SetCharRules;
    property ChangeDelayInterval: Cardinal read FChangeDelayInterval write SetChangeDelayInterval default 0;
    property IgnorePressedKey: Boolean read FIgnorePressedKey write FIgnorePressedKey;
    property IgnoreRules: Boolean read FIgnoreRules write SetIgnoreRules default false;
    property MaxChars: Integer read GetMaxChars;
    property BreakLineReplace: string read FBreakLineReplace write FBreakLineReplace;
    property OnChangeDelay: TNotifyEvent read FOnChangeDelay write FOnChangeDelay;
    property OnForbiddenCharPressed: TKeyPressEvent read FOnForbiddenCharPressed write FOnForbiddenCharPressed;
    property OnNotAllowedCharPressed: TKeyPressEvent read FOnNotAllowedCharPressed write FOnNotAllowedCharPressed;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ValidRulesForChar(Key: Char; CharPos: Integer): TRuleResult;
    procedure ApplyRulesToChar(var PressedKey: Char; _SelStart, _SelLength: Integer);
    procedure ApplyRulesToText(var TextChanged: Boolean);
    procedure SetTextIgnoringRules(WithValue: String);
  published
  end;


  TcyButtonedEdit = class(TcyBaseButtonedEdit)
  private
  protected
  public
    property IgnorePressedKey;
    property MaxChars;
  published
    property ChangeDelayInterval;
    property CharCase;    // Override CharCase: TEditCharCase from TCustomEdit class component
    property CharRules;
    property IgnoreRules;
    property BreakLineReplace;
    property OnChangeDelay;
    property OnForbiddenCharPressed;
    property OnNotAllowedCharPressed;
  end;


  TcyAdvBaseButtonedEdit = class(TcyBaseButtonedEdit)
  private
    FAllowEmpty: Boolean;
    FErrorHandling: TcyAdvEditErrorHandling;
    FOnValidateError: TProcOnValidateError;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure SetErrorHandling(const Value: TcyAdvEditErrorHandling);
  protected
    procedure HandleInvalidValue; virtual;
    property AllowEmpty: Boolean read FAllowEmpty write FAllowEmpty default true;
    property ErrorHandling: TcyAdvEditErrorHandling read FErrorHandling write SetErrorHandling;
    property OnValidateError: TProcOnValidateError read FOnValidateError write FOnValidateError;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ValidateText(aText: String): TEditValidateResult; virtual;
    function IsValidValue: Boolean; virtual;
  published
  end;


implementation

{ TcyBaseButtonedEdit }
constructor TcyBaseButtonedEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCharRules := tcyEditRules.Create(self, TcyEditRule);
  FIgnoreRules := false;
  FBreakLineReplace := '';
  CanCallChange := true;
  AutoInsertCharsCount := 0;
  FTimerChangeDelay := Nil;
  FChangeDelayInterval := 0;
end;

destructor TcyBaseButtonedEdit.Destroy;
begin
  FCharRules.Free;
  FCharRules := Nil;

  inherited;
end;

function TcyBaseButtonedEdit.GetMaxChars: Integer;
var
  r: Integer;
begin
  Result := MaxLength;

  for r := 0 to FCharRules.Count-1 do
    if FCharRules[r].AllowedChars = #0 then
      if (FCharRules[r].ToPosition-1 < Result) or (Result = 0) then
        Result := FCharRules[r].ToPosition-1;
end;

procedure TcyBaseButtonedEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  SavedShift := Shift;
  inherited;
end;

procedure TcyBaseButtonedEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  SavedShift := Shift;
  inherited;
end;

procedure TcyBaseButtonedEdit.SetCharRules(const Value: tcyEditRules);
begin
  FCharRules := Value;
end;

procedure TcyBaseButtonedEdit.SetIgnoreRules(const Value: Boolean);
begin
  FIgnoreRules := Value;

{ Do not invalidate Text !!!
  if not FIgnoreRules then
  begin
    ApplyRulesToText(TextChanged);

    if TextChanged and (not CanCallChange) then
      if Assigned(OnChange) then
        OnChange(Self);
  end; }
end;

procedure TcyBaseButtonedEdit.SetOverridenCharCase(const Value: TEditCharCase);
begin
  FOverridenCharCase := Value;
end;

procedure TcyBaseButtonedEdit.KeyPress(var Key: Char);
var
  TextChanged: Boolean;
  p: Integer;
begin
  AutoInsertCharsCount := 0;
  IgnorePressedKey := false;  // Let user bypass validation ...
  inherited;  // Let user handle with its own code on OnKeyPress event ...

  if (not (ssCtrl in SavedShift)) and (not IgnorePressedKey) and (not FIgnoreRules) then
  begin
    // Invalidate actual text if needed letting call Change() if modified :
    ApplyRulesToText(TextChanged);

    // Add AutoInsertChars if needed :
    if Key <> #8 then   // Backspace
    begin
      p := SelStart + 1;
      while FCharRules.GetAutoInsertCharAtPos(p) <> #0 do
      begin
        SelText := FCharRules.GetAutoInsertCharAtPos(p);
        inc(p);

        if p >= Length(Text) then
          Break;
      end;
    end;
// OLD 2015-10-15 ...
//    if Key <> #8 then   // Backspace
//      if FCharRules.GetAutoInsertCharAtPos(SelStart + 1) <> #0 then
//        SelText := FCharRules.GetAutoInsertCharAtPos(SelStart + 1);

    ApplyRulesToChar(Key, SelStart, SelLength);
  end;
end;

// Will add AutoInsertChars if needed
// Can be replacing selected text if called on KeyPress event
procedure TcyBaseButtonedEdit.ApplyRulesToChar(var PressedKey: Char; _SelStart, _SelLength: Integer);
var
  _MaxChars, r: Integer;
  _AutoInsertChars, TextAfterCursor: String;
  TextChanged, SaveCanCallChange, HandleForbiddenSequence: Boolean;
  sav: Char;

  // 2015-10-15 ...
  function GetAutoInsertCharsFromPosition(p: Integer): String;
  var
    r: Integer;
  begin
    Result := '';

    for r := 0 to FCharRules.Count-1 do
      if (FCharRules[r].FromPosition = p) and (FCharRules[r].AutoInsertChars <> '') then
      begin
        Result := FCharRules[r].AutoInsertChars + GetAutoInsertCharsFromPosition(p + length(FCharRules[r].AutoInsertChars));
        Break;
      end;
  end;

label
  RestartApplyRulesToChar;
begin
  if PressedKey = #0 then Exit;
  if PressedKey = #8 then Exit;   // Backspace: let user delete, even if we delete an AutoInsertChar (in order to allow delete all chars with backspace) ...

  // Modify char case in order to compare with rules:
  case CharCase of
    ecUpperCase: PressedKey := UpCase(PressedKey);
    ecLowerCase: PressedKey := LowerCase(PressedKey)[1];
  end;

  HandleForbiddenSequence := true;
  AutoInsertCharsCount := 0;
  TextChanged := false;
  _MaxChars := MaxChars;

  RestartApplyRulesToChar:

  // Allow replacing chars if MaxChars reached :
  if (_MaxChars <> 0) and (_SelLength = 0) then
    if (Length(Text) - _SelLength >= _MaxChars) then   // Current Text can contains more chars than _MaxChars !
    begin
      _SelLength := Length(Text) - _MaxChars + 1;
      SelLength  := _SelLength;
    end;

  if (Length(Text) - _SelLength < _MaxChars) or (_MaxChars = 0) then
    for r := 0 to FCharRules.Count-1 do
      case FCharRules[r].RuleAcceptChar(PressedKey, Text, _SelStart, _SelLength, _AutoInsertChars) of
        rrCharAccepted:
        begin
          if _AutoInsertChars <> '' then
          begin
            // Replace selected text with _AutoInsertChars :
            TextAfterCursor := Copy(Text, _SelStart + _SelLength + 1, Length(Text));

            if pos(_AutoInsertChars, TextAfterCursor) <> 1 then
            begin
              SaveCanCallChange := CanCallChange;
              CanCallChange := false;

              SelLength := Length(Text);   // Replace all from SelStart ...
              _AutoInsertChars := _AutoInsertChars + GetAutoInsertCharsFromPosition(_SelStart + 1 + length(_AutoInsertChars) + 1);
              SelText := _AutoInsertChars;
              Inc(AutoInsertCharsCount, length(_AutoInsertChars));
              TextChanged := true;

              // Goto initial position :
              SelStart := _SelStart;
              SelLength := 0;

              CanCallChange := SaveCanCallChange;
            end;
          end;
        end;

        rrForbiddenChar:
        begin
          if Assigned(FOnForbiddenCharPressed) then
          begin
            Sav := PressedKey;
            FOnForbiddenCharPressed(Self, PressedKey);

            if PressedKey = Sav then
              PressedKey := #0;
          end
          else
            PressedKey := #0;

          if PressedKey = #0
          then Break
          else goto RestartApplyRulesToChar;  // Retry ...
        end;

        rrForbiddenSequence:    // Exemple with time: Trying to replace 14:50 by 23:50, pressing "2" will create "24" forbidden sequence!
        begin
          // Select all text after SelStart in order to replace with pressed char:
          if HandleForbiddenSequence then
          begin
            HandleForbiddenSequence := false;
            _SelLength := Length(Text);
            SelLength := _SelLength;
            goto RestartApplyRulesToChar;
          end
          else begin
            PressedKey := #0;
            Break;
          end;
        end;

        rrCharNotAllowed:
        begin
          if Assigned(FOnNotAllowedCharPressed) then
          begin
            Sav := PressedKey;
            FOnNotAllowedCharPressed(Self, PressedKey);

            if PressedKey = Sav then
              PressedKey := #0;
          end
          else
            PressedKey := #0;

          if PressedKey = #0
          then Break
          else goto RestartApplyRulesToChar;  // Retry ...
        end;

        else begin
          PressedKey := #0;
          Break;
        end;
      end;

  if (PressedKey = #0) and (TextChanged) then
    Change;
end;

function TcyBaseButtonedEdit.ValidRulesForChar(Key: Char; CharPos: Integer): TRuleResult;
var
  r: Integer;
  _AutoInsertChars: String;
begin
  Result := rrCharAccepted;
  if Key = #0 then Exit;
  if Key = #8 then Exit;   // Backspace

    for r := 0 to FCharRules.Count-1 do
    begin
      Result := FCharRules[r].RuleAcceptChar(Key, Text, CharPos-1, 1, _AutoInsertChars);

      if Result <> rrCharAccepted then
        Break;
    end;
end;

procedure TcyBaseButtonedEdit.WMPaste(var Msg: TWMPaste);
var
  Str: string;
begin
  Inherited CharCase := FOverridenCharCase;

  if FBreakLineReplace = '' then
  begin
    Inherited;

    Inherited CharCase := ecNormal;
    Exit;
  end;

  Str := Clipboard.AsText;
  Str := String_BoundsCut(Str, #13, [srFromLeft, srFromRight]);
  Str := String_BoundsCut(Str, #10, [srFromLeft, srFromRight]);
  Str := StringReplace(Str, #13#10, FBreakLineReplace, [rfReplaceAll]);
  SelText := Str;

  Inherited CharCase := ecNormal
end;

// Correct Text property in order to respect all rules ...
procedure TcyBaseButtonedEdit.ApplyRulesToText(var TextChanged: Boolean);
var
  i: Integer;
  RuleResult: TRuleResult;
begin
  TextChanged := false;

  for i := 1 to Length(Text) do
  begin
    RuleResult := ValidRulesForChar(Text[i], i);

    if RuleResult <> rrCharAccepted then
    begin
      if SelStart < i-1
      then Text := Copy(Text, 1, SelStart)  // Cut after inserted car ...
      else Text := Copy(Text, 1, i-1);

      // Set position to the end:
      SelStart  := i;
      SelLength := 0;
      TextChanged := true;
      Break;
    end;
  end;
end;

// Also called when adding component, when cutting/pasting ...
procedure TcyBaseButtonedEdit.Change;
var
  TextChanged: Boolean;
begin
  if CanCallChange then
  begin
    if Assigned(Parent) then   // Avoid design time error when addding TcyButtonedEdit component ...
      if not (csLoading in ComponentState) then  // We can' t read SelStart when adding component at design time ...
      begin
        CanCallChange := false;

        // We need to move caret because of AutoInsertChars :
        if AutoInsertCharsCount <> 0 then
          SelStart := SelStart + AutoInsertCharsCount;

        if not FIgnoreRules then
          ApplyRulesToText(TextChanged);

        // Move caret until position with multiple chars choice:
        if SelLength = 0 then
          while FCharRules.GetAutoInsertCharAtPos(SelStart + 1) <> #0 do
          begin
            SelStart := SelStart + 1;
            if SelStart + 1 >= Length(Text) then
              Break;
          end;

        AutoInsertCharsCount := 0;
        CanCallChange := true;
      end;

    if Assigned(FTimerChangeDelay) then
    begin
      // Restart timer :
      FTimerChangeDelay.Enabled := false;
      FTimerChangeDelay.Enabled := true;
    end
    else
      if Assigned(FOnChangeDelay) then
        if not (csDestroying in Self.ComponentState) then
          FOnChangeDelay(Self);
  end;

  inherited;  // Will call OnChange ...
end;

procedure TcyBaseButtonedEdit.SetTextIgnoringRules(WithValue: String);
var
  SaveIgnoreRules: Boolean;
begin
  SaveIgnoreRules := IgnoreRules;
  IgnoreRules := true;
  Text := withValue;
  IgnoreRules := SaveIgnoreRules;
end;


{ TcyAdvBaseButtonedEdit }
constructor TcyAdvBaseButtonedEdit.Create(AOwner: TComponent);
begin
  inherited;
  FAllowEmpty := true;
  FErrorHandling := TcyAdvEditErrorHandling.Create(Self);
end;

destructor TcyAdvBaseButtonedEdit.Destroy;
begin
  FErrorHandling.Free;
  FErrorHandling := Nil;
  inherited;
end;

procedure TcyAdvBaseButtonedEdit.SetErrorHandling(const Value: TcyAdvEditErrorHandling);
begin
  FErrorHandling := Value;
end;

function TcyAdvBaseButtonedEdit.IsValidValue: Boolean;
begin
  Result := ValidateText(Text) = evValid;
end;

function TcyAdvBaseButtonedEdit.ValidateText(aText: String): TEditValidateResult;
begin
  Result := evValid;
end;

procedure TcyAdvBaseButtonedEdit.CMExit(var Message: TCMExit);
begin
  if not FIgnoreRules then
    if not (csDesigning in ComponentState) then
    begin
      if Text = '' then
      begin
        if not FAllowEmpty then
          HandleInvalidValue
      end
      else
        HandleInvalidValue;
    end;

  Inherited;
end;

procedure TcyAdvBaseButtonedEdit.HandleInvalidValue;
var
  Rslt: TEditValidateResult;
  Handled: Boolean;
  Msg: String;
begin
  Rslt := ValidateText(Text);
  if Rslt = evValid then Exit;

  if Assigned(OnValidateError) then
  begin
    Handled := false;
    OnValidateError(Self, Handled);
    if Handled then Exit;
  end;

  if eoBeepSound in ErrorHandling.Options then
    MessageBeep(0);

  if eoClearText in ErrorHandling.Options then
    Text := '';

  if eoKeepFocus in ErrorHandling.Options then
    SetFocus;

  if eoRaiseMsgError in ErrorHandling.Options then
  begin
    case ValidateText(Text) of
      evInvalidValue:
        if ErrorHandling.MsgInvalidValue <> ''
        then Msg := ErrorHandling.MsgInvalidValue
        else Msg := 'Invalid value!';

      evOutOfMinRange:
        if ErrorHandling.MsgOutOfMinRange <> ''
        then Msg := ErrorHandling.MsgOutOfMinRange
        else Msg := 'Value out of range!';

      evOutOfMaxRange:
        if ErrorHandling.MsgOutOfMaxRange <> ''
        then Msg := ErrorHandling.MsgOutOfMaxRange
        else Msg := 'Value out of range!';
    end;

    raise Exception.Create(Msg);
  end;
end;

procedure TcyBaseButtonedEdit.SetChangeDelayInterval(const Value: Cardinal);
begin
  FChangeDelayInterval := Value;

  if Value <> 0 then
  begin
    if not Assigned(FTimerChangeDelay) then
    begin
      FTimerChangeDelay := TTimer.Create(Self);
      FTimerChangeDelay.Enabled := false;
      FTimerChangeDelay.OnTimer  := OnTimerChangeDelayEvent;
    end;

    FTimerChangeDelay.Interval := Value;
  end
  else begin
    if Assigned(FTimerChangeDelay) then
    begin
      FTimerChangeDelay.Enabled := false;
      FTimerChangeDelay.Free;
      FTimerChangeDelay := Nil;
    end;
  end;
end;

procedure TcyBaseButtonedEdit.OnTimerChangeDelayEvent(Sender: TObject);
begin
  // Deactivate until next change :
  FTimerChangeDelay.Enabled := false;

  if Assigned(FOnChangeDelay) then
    if not (csDestroying in Self.ComponentState) then
      FOnChangeDelay(Self);
end;

end.
