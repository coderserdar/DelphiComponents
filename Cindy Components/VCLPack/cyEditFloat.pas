{   Component(s):
    tcyEditFloat

    Description:
    A Edit for floats

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
unit cyEditFloat;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Windows, Messages, Graphics, Controls, StdCtrls, SysUtils, vcl.cyGraphics, vcl.cyTypes, cyEdit;

type
  TProcEditFloatNeedDefaultValue = procedure (Sender: TObject; var Value: Double) of object;

  TcyEditFloat = class(TcyAdvBaseEdit)
  private
    FCanvas: TCanvas;
    FSystemDecimalSeparator: Char;
    FMinValue: Double;
    FAllowNegative: Boolean;
    FMaxValue: Double;
    FOnNeedDefaultValue: TProcEditFloatNeedDefaultValue;
    FPrecision: Word;
    FDisplayFormat: String;
    FOnPaint: TNotifyEvent;
    FAutoComplete: Boolean;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetValue: Double;
    procedure SetValue(const Value: Double);
    procedure SetAllowNegative(const Value: Boolean);
    procedure SetPrecision(const Value: Word);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure Paint;
    procedure PaintWindow(DC: HDC); override;
    procedure KeyPress(var Key: Char); override;
    procedure Change; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    function ValidateText(aText: String): TEditValidateResult; override;
    property Canvas: TCanvas read FCanvas;
    property Value: Double read GetValue write SetValue;
    property IgnorePressedKey;
    property MaxChars;
    destructor Destroy; override;
  published
    property AutoComplete: Boolean read FAutoComplete write FAutoComplete default false;
    property AllowNegative: Boolean read FAllowNegative write SetAllowNegative default true;
    property DisplayFormat: String read FDisplayFormat write FDisplayFormat;
    property MinValue: Double read FMinValue write FMinValue;
    property MaxValue: Double read FMaxValue write FMaxValue;
    property Precision: Word read FPrecision write SetPrecision default 2;
    property OnNeedDefaultValue: TProcEditFloatNeedDefaultValue read FOnNeedDefaultValue write FOnNeedDefaultValue;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
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

{ TcyEditFloat }
constructor TcyEditFloat.Create(AOwner: TComponent);
var
  fs: TFormatSettings;
begin
  inherited;

  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, fs);
  FSystemDecimalSeparator := fs.DecimalSeparator;

  FCanvas := TControlCanvas.Create;
  FAutoComplete := true;
  FAllowNegative := true;
  FMinValue := 0;
  FMaxValue := 0;
  FPrecision := 2;

  // Add rules :
  with CharRules.Add do
  begin
    FromPosition := 1;
    ToPosition := 1;
    AllowedChars := '-0123456789';
  end;

  with CharRules.Add do
  begin
    FromPosition := 2;
    ToPosition := 0;
    AllowedChars := FSystemDecimalSeparator + '0123456789';
  end;

  with CharRules.Add do
  begin
    FromPosition := 0;
    ToPosition := 0;

    // Do not let type negative followed by decimal symbol :
    ForbiddenCharsSequence := '-' + FSystemDecimalSeparator;

    // Do not let type decimal separator twice :
    RepeatOptions.CharsConcerned := FSystemDecimalSeparator;
    RepeatOptions.MaxRepeat := 1;
  end;
end;

destructor TcyEditFloat.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

procedure TcyEditFloat.SetAllowNegative(const Value: Boolean);
begin
  FAllowNegative := Value;

  if AllowNegative
  then CharRules[0].AllowedChars := '-0123456789'
  else CharRules[0].AllowedChars := '0123456789';
end;

procedure TcyEditFloat.SetPrecision(const Value: Word);
var
  NewText: String;
  d, SavSelStart: Integer;
begin
  if FPrecision = Value then Exit;
  FPrecision := Value;

  if FPrecision = 0
  then CharRules[1].AllowedChars := '0123456789'
  else CharRules[1].AllowedChars := FSystemDecimalSeparator + '0123456789';

  if not (csLoading in ComponentState) then
  begin
    // Cut in order to respect precision :
    d := pos(FSystemDecimalSeparator, Text);

    if d <> 0 then
      if Length(Text) - d > FPrecision then
      begin
        // Need to modify text?
        if FPrecision = 0
        then NewText := Copy(Text, 1, d-1)
        else NewText := Copy(Text, 1, d + FPrecision);

        if NewText <> Text then
        begin
          SavSelStart := SelStart;
          Text := NewText;
          SelStart := SavSelStart;
        end;
      end;
  end;
end;

procedure TcyEditFloat.SetValue(const Value: Double);
begin
  Text := FloatToStr(Value);
end;

function TcyEditFloat.GetValue: Double;
var
  Rslt: Extended;
begin
  if Text = '' then
    Result := 0
  else
    if TryStrToFloat(Text, Rslt)
    then Result := Rslt
    else Result := 0;
end;

function TcyEditFloat.ValidateText(aText: String): TEditValidateResult;
var
  Rslt: Extended;
begin
  Result := evValid;

  if aText = '' then
    Result := evInvalidValue
  else
    if not TryStrToFloat(aText, Rslt) then
      Result := evInvalidValue
    else
      if FMinValue <> FMaxValue then
      begin
        if (FMaxValue > FMinValue) or (FMinValue = 0) then
          if Rslt > FMaxValue then
            Result := evOutOfMaxRange;

        if (FMinValue < FMaxValue) or (FMaxValue = 0) then
          if Rslt < FMinValue then
            Result := evOutOfMinRange;
      end;
end;

procedure TcyEditFloat.KeyPress(var Key: Char);
begin
  // Replace char by decimal separator :
  if Key <> FSystemDecimalSeparator then
    {$IFDEF UNICODE}
    if CharInSet(Key, ['.', ',']) then
    {$ELSE}
    if Key in ['.', ','] then
    {$ENDIF}
      Key := FSystemDecimalSeparator;

  inherited;
end;

procedure TcyEditFloat.Loaded;         // !!! Not called for Dynamic controls !!!
var
  WrongSeparator: Char;
begin
  inherited;

  // Text can have wrong decimal seperator after loaded component properties :
  if Text <> '' then
  begin
    if FSystemDecimalSeparator = '.'
    then WrongSeparator := ','
    else WrongSeparator := '.';

    if pos(WrongSeparator, Text) <> 0 then
      Text := StringReplace(Text, WrongSeparator, FSystemDecimalSeparator, []);
  end;
end;

procedure TcyEditFloat.Change;
var
  d, SavSelStart: Integer;
  NewText: String;
begin
  // Cut in order to respect precision :
  d := pos(FSystemDecimalSeparator, Text);

  if d <> 0 then
    if Length(Text) - d > FPrecision then
    begin
      if FPrecision = 0
      then NewText := Copy(Text, 1, d-1)
      else NewText := Copy(Text, 1, d + FPrecision);

      if NewText <> Text then
      begin
        SavSelStart := SelStart;
        Text := NewText;
        SelStart := SavSelStart;
        Exit;
      end;
    end;

  inherited;
end;

procedure TcyEditFloat.CMExit(var Message: TCMExit);
var
  SavModified: Boolean;
  aText: String;
  Sav: TNotifyEvent;
  DefaultValue: Double;
  PosDec, i: Integer;
begin
  if FAutoComplete and (Text <> '') then
  begin
    // Add zero until Precision value :
    if (FAutoComplete) and (FPrecision > 0) and (Text <> '') then
    begin
      aText := Text;
      PosDec := Pos(FSystemDecimalSeparator, aText);

      // Add decimal separator :
      if PosDec = 0 then
      begin
        aText := aText + FSystemDecimalSeparator;
        PosDec := Length(aText);
      end;

      for i := Length(aText) - PosDec to FPrecision do
        aText := aText + '0';

      if Text <> aText then
      begin
        SavModified := Self.Modified;
        Sav := Self.OnChange;
        OnChange := Nil;
        Text := aText;
        Self.OnChange := Sav;
        Self.Modified := SavModified;
      end;
    end
  end;


  if (not AllowEmpty) and (Text = '') then
    if Assigned(FOnNeedDefaultValue) then
    begin
      DefaultValue := 0;
      FOnNeedDefaultValue(Self, DefaultValue);
      Value := DefaultValue;
    end;

  Inherited;
end;

procedure TcyEditFloat.WMPaint(var Message: TWMPaint);
begin
 ControlState := ControlState + [csCustomPaint];
 inherited;
 ControlState := ControlState - [csCustomPaint];
end;

procedure TcyEditFloat.Paint;
var
 R: TRect;
 DisplayText: String;
 DrawStyle: Longint;
begin
  if (DisplayFormat <> '') and (Text <> '') and (not Focused) then
  begin
    R := ClientRect;
    // Dec(R.Left, 3);
    Dec(R.Right, 1);

    FCanvas.Brush.Assign(Self.Brush);
    FCanvas.Font.Assign(Self.Font);

    DisplayText := FormatFloat(DisplayFormat, Value);
    {$IFDEF UNICODE}
    DrawStyle := DT_WORDBREAK or DT_TOP or Alignments[Alignment] or DrawTextBiDiModeFlagsReadingOnly;
    {$ELSE}
    DrawStyle := DT_WORDBREAK or DT_TOP or Alignments[taLeftJustify] or DrawTextBiDiModeFlagsReadingOnly;
    {$ENDIF}
    vcl.cyGraphics.cyDrawText(FCanvas.Handle, DisplayText, R, DrawStyle);
  end;

  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

// Call Paint :
procedure TcyEditFloat.PaintWindow(DC: HDC);
begin
  inherited;

  FCanvas.Lock;                             // Multithread drawing ...
  try
    FCanvas.Handle := DC;
    try
      TControlCanvas(FCanvas).UpdateTextFlags;
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

end.
