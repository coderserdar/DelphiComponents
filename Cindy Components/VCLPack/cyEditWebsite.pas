{   Component(s):
    tcyEditWebsite

    Description:
    A Edit for websites !

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
unit cyEditWebsite;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Windows, Controls, StdCtrls, SysUtils, cyStrUtils, cyEdit;

type
  TProcEditWebsiteNeedDefaultValue = procedure (Sender: TObject; var Value: String) of object;

  TcyEditWebsite = class(TcyAdvBaseEdit)
  private
    FOnNeedDefaultValue: TProcEditWebsiteNeedDefaultValue;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetValue: String;
    procedure SetValue(const Value: String);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    function ValidateText(aText: String): TEditValidateResult; override;
    property Value: String read GetValue write SetValue;
    property IgnorePressedKey;
    property MaxChars;
  published
    property AllowEmpty;
    property ChangeDelayInterval;
    property OnNeedDefaultValue: TProcEditWebsiteNeedDefaultValue read FOnNeedDefaultValue write FOnNeedDefaultValue;
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

{ TcyEditWebsite }
constructor TcyEditWebsite.Create(AOwner: TComponent);
var
  Str: String;
  c: Char;
begin
  inherited;

  // Add rules :
  with CharRules.Add do
  begin
    Str := '';
    FromPosition := 0;
    ToPosition := 0;

    // Add all allowed chars :
    {$IFDEF UNICODE}
    for c in Websiteletters do
      Str := Str + c;
    {$ELSE}
    str := '/:._-0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
    {$ENDIF}

    AllowedChars := Str;
  end;

  with CharRules.Add do
  begin
    FromPosition := 1;
    ToPosition := 1;
    ForbiddenChars := '.';
  end;

  with CharRules.Add do
  begin
    FromPosition := 0;
    ToPosition := 0;
    ForbiddenCharsSequence := '..';
  end;
end;

function TcyEditWebsite.GetValue: String;
begin
  if Text = '' then
    Result := ''
  else
    if isValidWebsite(Text)
    then Result := Text
    else Result := '';
end;

procedure TcyEditWebsite.SetValue(const Value: String);
begin
  Text := Value;
end;

function TcyEditWebsite.ValidateText(aText: String): TEditValidateResult;
begin
  Result := evValid;

  if aText = '' then
    Result := evInvalidValue
  else
    if not isValidWebsite(aText) then
      Result := evInvalidValue;
end;

procedure TcyEditWebsite.CMExit(var Message: TCMExit);
var
  DefaultValue: String;
begin
  if (not AllowEmpty) and (Text = '') then
    if Assigned(FOnNeedDefaultValue) then
    begin
      DefaultValue := '';
      FOnNeedDefaultValue(Self, DefaultValue);
      Text := DefaultValue;
    end;

  Inherited;
end;

end.
