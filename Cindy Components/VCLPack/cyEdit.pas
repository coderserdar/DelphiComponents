{   Component(s):
    tcyEdit

    Description:
    A Edit with CharRules property to define several rules like:
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

unit cyEdit;

interface

uses Classes, Windows, Messages, Controls, StdCtrls, SysUtils, cyStrUtils, ExtCtrls, Clipbrd;

type
  TRuleResult = (rrCharAccepted, rrCharNotAllowed, rrCharRepetitionNotAllowed, rrForbiddenChar, rrForbiddenSequence);

  TcyRuleRepeatOptions = class(TPersistent)
  private
    FMaxRepeat: Integer;
    FCharsConcerned: String;
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property MaxRepeat: Integer read FMaxRepeat write FMaxRepeat default 0;
    property CharsConcerned: String read FCharsConcerned write FCharsConcerned;
  end;

  tcyEditRule = class(TCollectionItem)
  private
    FFromPosition: Integer;
    FAllowedChars: String;
    FToPosition: Integer;
    FAutoInsertChars: String;
    FForbiddenCharsSequence: String;
    FRuleRepeatOptions: TcyRuleRepeatOptions;
    FActive: Boolean;
    FForbiddenChars: String;
    FOptional: Boolean;
    procedure SetAllowedChars(const Value: String);
    procedure SetFromPosition(const Value: Integer);
    procedure SetToPosition(const Value: Integer);
    procedure SetAutoInsertChars(const Value: String);
    procedure SetRuleRepeatOptions(const Value: TcyRuleRepeatOptions);
    procedure SetForbiddenChars(const Value: String);
  protected
    function GetDisplayName: string; override;
    property Optional: Boolean read FOptional write FOptional;  // Used on TcyMaskEdit ...
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function RuleAcceptChar(Key:Char; Text: String; TextSelStart, TextSelLength: Integer; var ApplyAutoInsertChars: String): TRuleResult;
  published
    property Active: Boolean read FActive write FActive default true;
    property AllowedChars: String read FAllowedChars write SetAllowedChars;
    property AutoInsertChars: String read FAutoInsertChars write SetAutoInsertChars;
    property FromPosition: Integer read FFromPosition write SetFromPosition default 0;
    property ToPosition: Integer read FToPosition write SetToPosition default 0;
    property ForbiddenChars: String read FForbiddenChars write SetForbiddenChars;
    property ForbiddenCharsSequence: String read FForbiddenCharsSequence write FForbiddenCharsSequence;
    property RepeatOptions: TcyRuleRepeatOptions read FRuleRepeatOptions write SetRuleRepeatOptions;
  end;



  tcyEditRuleClass = class of tcyEditRule;

  tcyEditRules = Class(TCollection)
  private
    FOnChange: TNotifyEvent;
    function GetEditRule(Index: Integer): tcyEditRule;
  protected
    FControl: TControl;
    function GetOwner: TPersistent; Override;
    procedure Update(Item: TCollectionItem); Override;
  public
    constructor Create(aControl: TControl; EditRuleClass: tcyEditRuleClass);
    function Add: tcyEditRule;
    procedure Delete(Index: Integer);
    function GetAutoInsertCharAtPos(CharPos: Integer): Char;
    property Items[Index: Integer]: tcyEditRule read GetEditRule; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  TcyBaseEdit = class(TEdit)
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


  TcyEdit = class(TcyBaseEdit)
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


  TEditValidateResult = (evValid, evInvalidValue, evOutOfMinRange, evOutOfMaxRange);
  TEditErrorOption = (eoBeepSound, eoKeepFocus, eoClearText, eoRaiseMsgError);
  TEditErrorOptions = Set of TEditErrorOption;

  TcyAdvEditErrorHandling = class(TPersistent)
  private
    FMsgOutOfMinRange: String;
    FMsgOutOfMaxRange: String;
    FMsgInvalidValue: String;
    FOptions: TEditErrorOptions;
    procedure SetOptions(const Value: TEditErrorOptions);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property MsgOutOfMinRange: String read FMsgOutOfMinRange write FMsgOutOfMinRange;
    property MsgOutOfMaxRange: String read FMsgOutOfMaxRange write FMsgOutOfMaxRange;
    property MsgInvalidValue: String read FMsgInvalidValue write FMsgInvalidValue;
    property Options: TEditErrorOptions read FOptions write SetOptions default [eoBeepSound, eoKeepFocus, eoRaiseMsgError];
  end;

  // Base for TcyEditInteger, TcyEditFloat, TcyEditDate and TcyEditTime :
  TProcOnValidateError = procedure (Sender: TObject; var Handled: Boolean) of object;

  TcyAdvBaseEdit = class(TcyBaseEdit)
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

{ TcyRuleRepeatOptions }
constructor TcyRuleRepeatOptions.Create(AOwner: TComponent);
begin
  FCharsConcerned := '';
  FMaxRepeat := 0;
end;

procedure TcyRuleRepeatOptions.Assign(Source: TPersistent);
begin
  if Source is TcyRuleRepeatOptions then
  begin
    FMaxRepeat := TcyRuleRepeatOptions(Source).FMaxRepeat;
    FCharsConcerned := TcyRuleRepeatOptions(Source).FCharsConcerned;
  end;

  // inherited;
end;

{ tcyEditRule }
procedure tcyEditRule.Assign(Source: TPersistent);
begin
  if Source is tcyEditRule then
  begin
    FFromPosition := tcyEditRule(Source).FFromPosition;
    FToPosition := tcyEditRule(Source).FToPosition;
    FAllowedChars := tcyEditRule(Source).FAllowedChars;
    FForbiddenChars := tcyEditRule(Source).FForbiddenChars;
    FForbiddenCharsSequence := tcyEditRule(Source).FForbiddenCharsSequence;
    FAutoInsertChars := tcyEditRule(Source).FAutoInsertChars;
    FOptional := tcyEditRule(Source).FOptional;
  end;
//  inherited Assign(Source);
end;

constructor tcyEditRule.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FActive := true;
  FAllowedChars := '';
  FAutoInsertChars := '';
  FFromPosition := 0;
  FToPosition := 0;
  FForbiddenChars := '';
  FForbiddenCharsSequence := '';
  FOptional := false;
  FRuleRepeatOptions := TcyRuleRepeatOptions.Create(TComponent(Collection.Owner));
end;

destructor tcyEditRule.Destroy;
begin
  FRuleRepeatOptions.Free;
  inherited;
end;

function tcyEditRule.GetDisplayName: string;
begin
  Result := '[From pos ' + intToStr(FFromPosition) + ' to ' + intToStr(FToPosition) + ']';

  if FAllowedChars <> '' then
    Result := Result + ' AllowedChars: ' + '''' + FAllowedChars + '''';

  if FForbiddenChars <> '' then
    Result := Result + ' ForbiddenChars: ' + '''' + FForbiddenChars + '''';

  if FAutoInsertChars <> '' then
    Result := Result + ' AutoInsertChars: ' + '''' + AutoInsertChars + '''';

  if FForbiddenCharsSequence <> '' then
    Result := Result + '      [ForbiddenCharsSequence] ' + '''' + FForbiddenCharsSequence + '''';

  if (FRuleRepeatOptions.CharsConcerned <> '') and (FRuleRepeatOptions.FMaxRepeat <> 0) then
    Result := Result + '      [RuleRepeatOptions] ' + ' CharsConcerned: ' + '''' + FRuleRepeatOptions.CharsConcerned + '''' + ' MaxRepeat: ' + intToStr(FRuleRepeatOptions.FMaxRepeat);
end;

procedure tcyEditRule.SetAllowedChars(const Value: String);
begin
  FAllowedChars := Value;
  Changed(false);         // It will call TcyEditRules.Update !
end;

procedure tcyEditRule.SetAutoInsertChars(const Value: String);
begin
  FAutoInsertChars := Value;
  Changed(false);
end;

procedure tcyEditRule.SetForbiddenChars(const Value: String);
begin
  FForbiddenChars := Value;
  Changed(false);
end;

procedure tcyEditRule.SetFromPosition(const Value: Integer);
begin
  FFromPosition := Value;
  Changed(false);
end;

procedure tcyEditRule.SetRuleRepeatOptions(const Value: TcyRuleRepeatOptions);
begin
  FRuleRepeatOptions := Value;
end;

procedure tcyEditRule.SetToPosition(const Value: Integer);
begin
  FToPosition := Value;
  Changed(false);
end;

function tcyEditRule.RuleAcceptChar(Key: Char; Text: String; TextSelStart, TextSelLength: Integer; var ApplyAutoInsertChars: String): TRuleResult;
var
  KeyPosition, p: Integer;
  ResultingText: String;

      function CountOccurences(aChar: Char; aStr: String): Integer;
      var
        i: Integer;
      begin
        Result := 0;
        for i := 1 to Length(aStr) do
          if aStr[i] = aChar then
            Inc(Result);
      end;

begin
  Result := rrCharAccepted;
  if not FActive then Exit;

  KeyPosition := TextSelStart + 1;  // + 1 because SelStart starts at 0

  // Check position in order to know if we have to apply rule in KeyPosition :
  if (FFromPosition = 0) or (FFromPosition <= KeyPosition) then
    if (FToPosition = 0) or (FToPosition >= KeyPosition) then
    begin
      // Allowed char ?
      if (FAllowedChars <> '') and (pos(Key, FAllowedChars) = 0) then
        Result := rrCharNotAllowed;

      // Forbidden char ?
      if (FForbiddenChars <> '') and (pos(Key, FForbiddenChars) <> 0) then
        Result := rrForbiddenChar;

      if (Result = rrCharAccepted) and (FRuleRepeatOptions.FMaxRepeat <> 0) then
      begin
        ResultingText := Copy(Text, 1, TextSelStart) + Key + Copy(Text, TextSelStart + TextSelLength + 1, Length(Text));

        // MaxRepeat ?
        if (FRuleRepeatOptions.FCharsConcerned = '') or (pos(Key, FRuleRepeatOptions.FCharsConcerned) <> 0) then
          if CountOccurences(Key, ResultingText) > FRuleRepeatOptions.FMaxRepeat then
            Result := rrCharRepetitionNotAllowed;
      end;

      if (Result = rrCharAccepted) and (FForbiddenCharsSequence <> '') then
        if pos(Key, FForbiddenCharsSequence) <> 0 then
        begin
          // Forbidden sequence ?
          ResultingText := Copy(Text, 1, TextSelStart) + Key + Copy(Text, TextSelStart + TextSelLength + 1, Length(Text));
          p := String_Pos(FForbiddenCharsSequence, ResultingText, FFromPosition, csCaseSensitive);

          if p <> 0 then
            if (p + length(FForbiddenCharsSequence)-1 <= FToPosition) or (FToPosition = 0) then  // Sequence starting between FFromPosition and FToPosition ...
              Result := rrForbiddenSequence;
        end;
    end;

  // AutoInsertChars AFTER key ?
  ApplyAutoInsertChars := '';

  if Result = rrCharAccepted then
    if FFromPosition = KeyPosition + 1 then
      ApplyAutoInsertChars := AutoInsertChars;
end;


{ tcyEditRules }
constructor tcyEditRules.Create(aControl: TControl; EditRuleClass: tcyEditRuleClass);
begin
  inherited Create(EditRuleClass);
  FControl := aControl;
end;

function tcyEditRules.GetAutoInsertCharAtPos(CharPos: Integer): Char;
var r: Integer;
begin
  Result := #0;

  for r := 0 to Count - 1 do
    if (Items[r].FFromPosition <> 0) and (Items[r].FAutoInsertChars <> '') then
      if (Items[r].FFromPosition <= CharPos) then
        if Items[r].FFromPosition + Length(Items[r].FAutoInsertChars) >= CharPos then
          Result := Items[r].FAutoInsertChars[CharPos - Items[r].FFromPosition + 1];
end;

function tcyEditRules.GetEditRule(Index: Integer): tcyEditRule;
begin
  Result := tcyEditRule(inherited Items[Index]);
end;

function tcyEditRules.GetOwner: TPersistent;
begin
  Result := FControl;
end;

// Event Called by setting properties/events of TcyEditRule :
procedure tcyEditRules.Update(Item: TCollectionItem);
begin
  Inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function tcyEditRules.Add: tcyEditRule;
begin
  Result := tcyEditRule(inherited Add);
  Result.Changed(false);      // It will call TcyEditRules.Update only at run-time!
end;

procedure tcyEditRules.Delete(Index: Integer);
begin
  Inherited;
  Update(Nil);
end;

{ TcyBaseEdit }
constructor TcyBaseEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCharRules := tcyEditRules.Create(self, tcyEditRule);
  FIgnoreRules := false;
  FBreakLineReplace := '';
  CanCallChange := true;
  AutoInsertCharsCount := 0;
  FTimerChangeDelay := Nil;
  FChangeDelayInterval := 0;
end;

destructor TcyBaseEdit.Destroy;
begin
  FCharRules.Free;
  FCharRules := Nil;

  inherited;
end;

function TcyBaseEdit.GetMaxChars: Integer;
var
  r: Integer;
begin
  Result := MaxLength;

  for r := 0 to FCharRules.Count-1 do
    if FCharRules[r].AllowedChars = #0 then
      if (FCharRules[r].FToPosition-1 < Result) or (Result = 0) then
        Result := FCharRules[r].FToPosition-1;
end;

procedure TcyBaseEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  SavedShift := Shift;
  inherited;
end;

procedure TcyBaseEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  SavedShift := Shift;
  inherited;
end;

procedure TcyBaseEdit.SetCharRules(const Value: tcyEditRules);
begin
  FCharRules := Value;
end;

procedure TcyBaseEdit.SetIgnoreRules(const Value: Boolean);
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

procedure TcyBaseEdit.SetOverridenCharCase(const Value: TEditCharCase);
begin
  FOverridenCharCase := Value;
end;

procedure TcyBaseEdit.KeyPress(var Key: Char);
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
procedure TcyBaseEdit.ApplyRulesToChar(var PressedKey: Char; _SelStart, _SelLength: Integer);
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

function TcyBaseEdit.ValidRulesForChar(Key: Char; CharPos: Integer): TRuleResult;
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

procedure TcyBaseEdit.WMPaste(var Msg: TWMPaste);
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
procedure TcyBaseEdit.ApplyRulesToText(var TextChanged: Boolean);
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
procedure TcyBaseEdit.Change;
var
  TextChanged: Boolean;
begin
  if CanCallChange then
  begin
    if Assigned(Parent) then   // Avoid design time error when addding TcyEdit component ...
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

procedure TcyBaseEdit.SetTextIgnoringRules(WithValue: String);
var
  SaveIgnoreRules: Boolean;
begin
  SaveIgnoreRules := IgnoreRules;
  IgnoreRules := true;
  Text := withValue;
  IgnoreRules := SaveIgnoreRules;
end;

{ TcyAdvEditErrorHandling }
procedure TcyAdvEditErrorHandling.Assign(Source: TPersistent);
begin
  if Source is TcyAdvEditErrorHandling then
  begin
    FMsgInvalidValue := TcyAdvEditErrorHandling(Source).FMsgInvalidValue;
    FMsgOutOfMaxRange := TcyAdvEditErrorHandling(Source).FMsgOutOfMaxRange;
    FMsgOutOfMinRange := TcyAdvEditErrorHandling(Source).FMsgOutOfMinRange;
    FOptions := TcyAdvEditErrorHandling(Source).FOptions;
  end;

  // inherited;
end;

constructor TcyAdvEditErrorHandling.Create(AOwner: TComponent);
begin
  FMsgOutOfMinRange := '';
  FMsgOutOfMaxRange := '';
  FMsgInvalidValue  := '';
  FOptions := [eoBeepSound, eoKeepFocus, eoRaiseMsgError];
end;

procedure TcyAdvEditErrorHandling.SetOptions(const Value: TEditErrorOptions);
begin
  FOptions := Value;
end;

{ TcyAdvBaseEdit }
constructor TcyAdvBaseEdit.Create(AOwner: TComponent);
begin
  inherited;
  FAllowEmpty := true;
  FErrorHandling := TcyAdvEditErrorHandling.Create(Self);
end;

destructor TcyAdvBaseEdit.Destroy;
begin
  FErrorHandling.Free;
  FErrorHandling := Nil;
  inherited;
end;

procedure TcyAdvBaseEdit.SetErrorHandling(const Value: TcyAdvEditErrorHandling);
begin
  FErrorHandling := Value;
end;

function TcyAdvBaseEdit.IsValidValue: Boolean;
begin
  Result := ValidateText(Text) = evValid;
end;

function TcyAdvBaseEdit.ValidateText(aText: String): TEditValidateResult;
begin
  Result := evValid;
end;

procedure TcyAdvBaseEdit.CMExit(var Message: TCMExit);
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

procedure TcyAdvBaseEdit.HandleInvalidValue;
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

procedure TcyBaseEdit.SetChangeDelayInterval(const Value: Cardinal);
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

procedure TcyBaseEdit.OnTimerChangeDelayEvent(Sender: TObject);
begin
  // Deactivate until next change :
  FTimerChangeDelay.Enabled := false;

  if Assigned(FOnChangeDelay) then
    if not (csDestroying in Self.ComponentState) then
      FOnChangeDelay(Self);
end;

end.
