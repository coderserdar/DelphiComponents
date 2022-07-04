{   Component(s):
    TcyBaseDBLookupComboBox

    Description:
    A base for Cindy DBLookupComboBox .

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

unit cyBaseDBLookupComboBox;

interface

uses Classes, Windows, Messages, Controls, StdCtrls, Forms, Db, DbClient, Provider, Variants, DBCtrls, vcl.cyTypes, cyBaseContainer, cyFlyingContainer;

type
  TcyBaseDBlookupComboBox = class(TDBLookupComboBox)
  private
    SavTickCountCloseUp: Cardinal;
    FDropDownControlOptions: TDropDownControlOptions;
    FDropDownRowsMin: Integer;
    procedure CloseUp(Accept: Boolean); override;
    function GetDropDownControl: TControl;
    procedure SetDropDownControl(const Value: TControl);
    procedure DoDropDownControl;
    procedure OnCloseContainer(Sender: TObject);
    procedure SetDropDownRowsMin(const Value: Integer);
    procedure SetListValue(const Value: String);
    function GetListValue: String;
  protected
    FFlyingContainer: TcyFlyingContainer;
    procedure DropDown; override;
    property DropDownControl: TControl read GetDropDownControl write SetDropDownControl;
    property DropDownControlOptions: TDropDownControlOptions read FDropDownControlOptions write FDropDownControlOptions default [];
    property DropDownRowsMin: Integer read FDropDownRowsMin write SetDropDownRowsMin default 2;
    property ListValue: String read GetListValue write SetListValue;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

implementation

{ TcyBaseDBlookupComboBox }
procedure TcyBaseDBlookupComboBox.CloseUp(Accept: Boolean);
begin
  inherited CloseUp(Accept);
end;

constructor TcyBaseDBlookupComboBox.Create(AOwner: TComponent);
begin
  inherited;

  SavTickCountCloseUp := GetTickCount;

  if csDesigning in ComponentState
  then FFlyingContainer := TcyFlyingContainer.Create(Self)         // Don' t appear at design time !
  else FFlyingContainer := TcyFlyingContainer.Create(Self.Owner);  // Avoid window flickering on DropDown control ...

  FDropDownControlOptions := [];
  FDropDownRowsMin := 2;

  FFlyingContainer.DontActivate := true;
  FFlyingContainer.EnterKeyAction := enterKeyDefault;
  FFlyingContainer.OnClose := OnCloseContainer;
end;

destructor TcyBaseDBlookupComboBox.Destroy;
begin
  FFlyingContainer.Free;
  inherited;
end;


procedure TcyBaseDBlookupComboBox.DropDown;
begin
  if not Assigned(FFlyingContainer.Control) then
  begin
    Inherited;
    Exit;
  end;

  // Avoid DoDropDownControl again after clicking once more (just close it and reopen it) on control :
  if GetTickCount - SavTickCountCloseUp < 200 then   // Works between 100 and 300 ms
  begin
    SavTickCountCloseUp := 0;
    Exit;
  end;

  // 2016-08-04 Only call it after controlling SavTickCountCloseUp ...
  if Assigned(OnDropDown) then
    OnDropDown(Self);

  DoDropDownControl;
end;

procedure TcyBaseDBlookupComboBox.DoDropDownControl;
var
  OffsetLeft, OffsetTop: Integer;
begin
  if DropDownWidth = 0
  then DropDownControl.Width := Self.Width
  else DropDownControl.Width := DropDownWidth;

  case DropDownAlign of
    daLeft:   OffsetLeft := 0;
    daCenter: OffsetLeft := Self.Width div 2 - DropDownControl.Width div 2;
    daRight:  OffsetLeft := Self.Width - DropDownControl.Width;
  end;

  OffsetTop := Self.Height;

  // Drop up if not space :
  if ClientToScreen(Point(0, OffsetTop) ).Y + FFlyingContainer.Control.Height > Screen.Height then
    OffsetTop := FFlyingContainer.Control.Height * (-1);

  FFlyingContainer.ExecuteFromControl(Self, OffsetLeft, OffsetTop, ddoRestoreLastSize in FDropDownControlOptions, ddoRestoreLastPosition in FDropDownControlOptions);
end;

function TcyBaseDBlookupComboBox.GetDropDownControl: TControl;
begin
  Result := FFlyingContainer.Control;
end;

procedure TcyBaseDBlookupComboBox.OnCloseContainer(Sender: TObject);
begin
  SavTickCountCloseUp := GetTickCount;
end;

procedure TcyBaseDBlookupComboBox.SetDropDownControl(const Value: TControl);
begin
  FFlyingContainer.Control := Value;
end;

procedure TcyBaseDBlookupComboBox.SetDropDownRowsMin(const Value: Integer);
begin
  if Value > 0
  then FDropDownRowsMin := Value
  else FDropDownRowsMin := 1;
end;

function TcyBaseDBlookupComboBox.GetListValue: String;
begin
  Result := Self.Text;
end;

procedure TcyBaseDBlookupComboBox.SetListValue(const Value: String);
begin
  if Value = '' then
    KeyValue := Null
  else
    if Self.ListSource.DataSet.Locate(Self.ListField, Value, [loCaseInsensitive])
    then Self.KeyValue := Self.ListSource.DataSet.FieldByName(Self.KeyField).AsString
    else KeyValue := Null;
end;

end.
