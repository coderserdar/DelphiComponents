{   Component(s):
    tcyMaskEdit

    Description:
    A Edit with Mask property that doesn' t show mask while typing!
    See cyStrUtils.pas, function IsMatchMask for more information.



    *** Mask chars definition ***

     'AB'   - Letters AB expected
     N      - Number expected
     A      - Alphabetic expected
     U      - Uppercase letter expected
     L      - Lowercase letter expected
     X      - Alpha numeric expected
     S      - Other char expected
     *      - Any char


     To be done in future :

     [0..999]/[0..999]   - Suffix that can be added to prior rules
     Exemple :
     N1/9   - Undetermined Numbers expected between 1 and 9
     A1/9   - Undetermined Letters expected between 1 and 9



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

unit cyMaskEdit;

interface

uses Classes, Windows, Messages, Controls, StdCtrls, SysUtils, cyStrUtils, cyEdit;

type
  TcyMaskEdit = class(TcyAdvBaseEdit)
  private
    fMask: string;
    procedure SetMask(const Value: string);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    function ValidateText(aText: String): TEditValidateResult; override;
    property IgnorePressedKey;
    property MaxChars;
  published
    // Not visible ... property CharRules;
    // Not visible ... property IgnoreRules;
    property AllowEmpty;
    property BreakLineReplace;
    property ErrorHandling;
    property OnValidateError;
    property Mask: string read fMask write SetMask;
  end;

implementation

{ TcyMaskEdit }
constructor TcyMaskEdit.Create(AOwner: TComponent);
begin
  inherited;
  fMask := '';
end;

// !!! Optional char specified by % not handled for now !!!
procedure TcyMaskEdit.SetMask(const Value: string);
var
  aMask: string;
  MaskPartParamStr: string;
  MaskPartType: TMaskPartType;
  RsltMaskPartOptional: Boolean;

  Rule: TcyEditRule;
  StrMsg: String;
  fromPosition, toPosition, i: Integer;
begin
  fMask := Value;

  // Build rules from mask :
  CharRules.Clear;

  if fMask = '' then Exit;

  Rule := Nil;
  aMask := fMask;
  toPosition := 0;

  while GetNextMaskPart(aMask, MaskPartParamStr, MaskPartType, RsltMaskPartOptional) do
  begin
    fromPosition := toPosition + 1;
    toPosition := fromPosition;

    Rule := CharRules.Add;
    Rule.FromPosition := fromPosition;
    Rule.ToPosition := toPosition;

    case MaskPartType of
      mtMaskPartStringValue:
      begin

        for i := 1 to length(MaskPartParamStr) do
        begin
          if i <> 1 then
          begin
            Rule := CharRules.Add;
            fromPosition := toPosition + 1;
            toPosition := fromPosition;
            Rule.FromPosition := fromPosition;
            Rule.ToPosition := toPosition;
          end;

          Rule.AutoInsertChars := MaskPartParamStr[i];
          Rule.AllowedChars := MaskPartParamStr[i];
        end;
      end;

      mtMaskPartCustomChar:
        Rule.AllowedChars := MaskPartParamStr;

      mtAnyChar:
        ;

      mtAlphaNumChar:
        Rule.AllowedChars := '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';

      mtNumber:                           // Optional number specified by 9 not handled for now
        Rule.AllowedChars := '0123456789';

      mtAlphaChar:
        Rule.AllowedChars := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';

      mtUppLetter:
        Rule.AllowedChars := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';

      mtLowLetter:
        Rule.AllowedChars := 'abcdefghijklmnopqrstuvwxyz';

      else   // Other chars ...
        Rule.ForbiddenChars := '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
    end;
  end;

  if MaskPartType = mtUndefined then
  begin
    StrMsg := 'Char ' + QuotedStr(MaskPartParamStr) + ' not allowed. See below allowed chars: ' + #13#10 +
              MaskRulesMsg;

    raise Exception.Create(StrMsg);
  end;

  if Assigned(Rule) then
    MaxLength := Rule.ToPosition;
end;

function TcyMaskEdit.ValidateText(aText: String): TEditValidateResult;
begin
  if fMask = '' then
    Result := evValid
  else
    if aText = '' then
      Result := evInvalidValue
    else
      if cyStrUtils.IsMatchMask2(Mask, aText)
      then Result := evValid
      else Result := evInvalidValue;
end;

end.
