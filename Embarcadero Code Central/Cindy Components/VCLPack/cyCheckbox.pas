{   Component(s):
    tcyCheckBox

    Description:
    It's a checkBox with OnChange and OnClick (only when user changes value) event !!!

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

unit cyCheckbox;

interface

uses Classes, Types, Controls, Graphics, Messages, Windows, StdCtrls;

type
  TcyCheckBox = class(TCheckBox)
  private
    FOnChange: TNotifyEvent;
    function GetState: TCheckBoxState;
    procedure SetState(const Value: TCheckBoxState);
  protected
    procedure SetChecked(Value: Boolean); override;    // When changing Checked property by code ...
    procedure Click; override;
  public
    property State: TCheckBoxState read GetState write SetState default cbUnchecked;
    property ClicksDisabled;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TcyCheckBox }
procedure TcyCheckBox.Click;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);

  inherited;
end;

// When changing Checked property by code :
procedure TcyCheckBox.SetChecked(Value: Boolean);
var
  SavClicksDisabled: Boolean;
begin
  SavClicksDisabled := ClicksDisabled;
  ClicksDisabled := true;
  inherited;
  ClicksDisabled := SavClicksDisabled;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TcyCheckBox.GetState: TCheckBoxState;
begin
  Result := inherited State;
end;

procedure TcyCheckBox.SetState(const Value: TCheckBoxState);
var
  SavClicksDisabled: Boolean;
begin
  SavClicksDisabled := ClicksDisabled;
  ClicksDisabled := true;
  Inherited State := Value;
  ClicksDisabled := SavClicksDisabled;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
