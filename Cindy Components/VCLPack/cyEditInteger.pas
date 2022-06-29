{   Component(s):
    tcyEditInteger

    Description:
    A Edit for integers!

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
unit cyEditInteger;

interface

uses Classes, Windows, Controls, StdCtrls, SysUtils, cyEdit;

type
  TProcEditIntegerNeedDefaultValue = procedure (Sender: TObject; var Value: Integer) of object;

  TcyEditInteger = class(TcyAdvBaseEdit)
  private
    FMinValue: Integer;
    FMaxValue: Integer;
    FAllowNegative: Boolean;
    FOnNeedDefaultValue: TProcEditIntegerNeedDefaultValue;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetValue: Integer;
    procedure SetValue(const Value: Integer);
    procedure SetAllowNegative(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    function ValidateText(aText: String): TEditValidateResult; override;
    property Value: Integer read GetValue write SetValue;
    property IgnorePressedKey;
    property MaxChars;
  published
    property AllowEmpty;
    property ChangeDelayInterval;
    property AllowNegative: Boolean read FAllowNegative write SetAllowNegative default true;
    property MinValue: Integer read FMinValue write FMinValue default 0;
    property MaxValue: Integer read FMaxValue write FMaxValue default 0;
    property OnNeedDefaultValue: TProcEditIntegerNeedDefaultValue read FOnNeedDefaultValue write FOnNeedDefaultValue;
    // Herited :
    // Do not publish ... property CharRules;
    property IgnoreRules;
    property ErrorHandling;
    property OnChangeDelay;
    property OnForbiddenCharPressed;
    property OnNotAllowedCharPressed;
    property OnValidateError;
  end;

implementation

{ TcyEditInteger }
constructor TcyEditInteger.Create(AOwner: TComponent);
begin
  inherited;
  FAllowNegative := true;
  FMinValue := 0;
  FMaxValue := 0;

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
    AllowedChars := '0123456789';
  end;

end;

procedure TcyEditInteger.SetAllowNegative(const Value: Boolean);
begin
  FAllowNegative := Value;

  if AllowNegative
  then CharRules[0].AllowedChars := '-0123456789'
  else CharRules[0].AllowedChars := '0123456789';
end;

procedure TcyEditInteger.SetValue(const Value: Integer);
begin
  Text := intToStr(Value);
end;

function TcyEditInteger.GetValue: Integer;
var Rslt: Integer;
begin
  if Text = '' then
    Result := 0
  else
    if TryStrToInt(Text, Rslt)
    then Result := Rslt
    else Result := 0;
end;

function TcyEditInteger.ValidateText(aText: String): TEditValidateResult;
var Rslt: Integer;
begin
  Result := evValid;

  if aText = '' then
    Result := evInvalidValue
  else
    if not TryStrToInt(aText, Rslt) then
      Result := evInvalidValue
    else
      // OLD 2014-07-29 if FMinValue <> FMaxValue then
      if FMinValue <> FMaxValue then      // ((FMaxValue > FMinValue) and (FMaxValue <> 0)) or ((FMinValue = 0) then
      begin
        if (FMaxValue > FMinValue) or (FMinValue = 0) then
          if Rslt > FMaxValue then
            Result := evOutOfMaxRange;

        if (FMinValue < FMaxValue) or (FMaxValue = 0) then
          if Rslt < FMinValue then
            Result := evOutOfMinRange;
      end;
end;

procedure TcyEditInteger.CMExit(var Message: TCMExit);
var
  DefaultValue: Integer;
begin
  if (not AllowEmpty) and (Text = '') then
    if Assigned(FOnNeedDefaultValue) then
    begin
      DefaultValue := 0;
      FOnNeedDefaultValue(Self, DefaultValue);
      Value := DefaultValue;
    end;

  Inherited;
end;

end.
