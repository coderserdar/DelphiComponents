{   Component(s):
    tcyCombobox

    Description:
    A Combobox with DropDownControl property that can replace Drop down listbox.

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

unit cyBaseCombobox;

interface

uses Classes, Windows, Messages, SysUtils, Forms, Controls, StdCtrls, vcl.cyTypes, cyBaseContainer, cyFlyingContainer;

type
  TcyBaseCombobox = class(TCombobox)
  private
    SavTickCountCloseUp: Cardinal;
    FDropDownWidth: Integer;
    FDropDownControl: TControl;
    FTagStr: String;
    FDropDownControlOptions: TDropDownControlOptions;
    procedure CBNDROPDOWN(var Message: TMessage); message CBN_DROPDOWN;          // CBN_DROPDOWN = WM_SETFOCUS !
    procedure CBSHOWDROPDOWN(var Message: TMessage); message CB_SHOWDROPDOWN;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure OnCloseContainer(Sender: TObject);
  protected
    FFlyingContainer: TcyFlyingContainer;
    FInBuiltDropDownControl: TControl;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DropDown; override;
    procedure DoDropDownControl;
    property DropDownControl: TControl read FDropDownControl write FDropDownControl;
    property DropDownControlOptions: TDropDownControlOptions read FDropDownControlOptions write FDropDownControlOptions default [];
    property DropDownWidth: Integer read FDropDownWidth write FDropDownWidth default 0;
    property TagStr: String read FTagStr write FTagStr;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TcyCombobox = class(TcyBaseCombobox)
  private
  protected
  public
    function GetDropDownControlDefs: TcyFlyingContainer;
  published
    property DropDownControl;
    property DropDownControlOptions;
    property DropDownWidth;
    property TagStr;
  end;

implementation

{ TcyBaseCombobox }

constructor TcyBaseCombobox.Create(AOwner: TComponent);
begin
  inherited;

  FTagStr := '';
  FDropDownWidth := 0;
  SavTickCountCloseUp := GetTickCount;
  FDropDownControl := Nil;
  FInBuiltDropDownControl := Nil;
  FDropDownControlOptions := [];

  if csDesigning in ComponentState
  then FFlyingContainer := TcyFlyingContainer.Create(Self)         // Don' t appear at design time !
  else FFlyingContainer := TcyFlyingContainer.Create(Self.Owner);  // Avoid window flickering on DropDown control ...

  FFlyingContainer.DontActivate := true;
  FFlyingContainer.EnterKeyAction := enterKeyDefault;
  FFlyingContainer.OnClose := OnCloseContainer;
end;

destructor TcyBaseCombobox.Destroy;
begin
  FFlyingContainer.Free;
  inherited;
end;


procedure TcyBaseCombobox.DoDropDownControl;
var
  OffsetLeft, OffsetTop: Integer;
begin
  OffsetLeft := 0;
  OffsetTop := Self.Height;

  if Assigned(FDropDownControl)
  then FFlyingContainer.Control := FDropDownControl            // Custom control
  else FFlyingContainer.Control := FInBuiltDropDownControl;    // Obuilt panel

  if FDropDownWidth = 0
  then FFlyingContainer.Control.Width := Self.Width
  else FFlyingContainer.Control.Width := FDropDownWidth;

  // Drop up if not space :
  if ClientToScreen(Point(0, OffsetTop) ).Y + FFlyingContainer.Control.Height > Screen.Height then
    OffsetTop := FFlyingContainer.Control.Height * (-1);

  FFlyingContainer.ExecuteFromControl(Self, OffsetLeft, OffsetTop, ddoRestoreLastSize in FDropDownControlOptions, ddoRestoreLastPosition in FDropDownControlOptions);
end;

procedure TcyBaseCombobox.OnCloseContainer(Sender: TObject);
begin
  SavTickCountCloseUp := GetTickCount;
end;

function TcyCombobox.GetDropDownControlDefs: TcyFlyingContainer;
begin
  Result := FFlyingContainer;
end;

procedure TcyBaseCombobox.KeyDown(var Key: Word; Shift: TShiftState);
var
  DropControl: Boolean;
begin
  DropControl := false;

  // Cancel Alt + down because it displays list items :
  if Assigned(FDropDownControl) or Assigned(FInBuiltDropDownControl) then
    if (ssAlt in Shift) and (key = VK_DOWN) then
    begin
      Key := 0;
      Shift := [];
      DropControl := true;
    end;

  inherited;

  if DropControl then
    DropDown;
end;

// Every time list items appears ...
procedure TcyBaseCombobox.DropDown;
begin
  if Assigned(FDropDownControl) or Assigned(FInBuiltDropDownControl) then
  begin
    if Assigned(OnDropDown) then
      OnDropDown(Self);

    // Avoid DoDropDownControl again after clicking once more (just close it and reopen it) on control :
    if GetTickCount - SavTickCountCloseUp >= 200 then   // Works between 100 and 300 ms ...
      DoDropDownControl;
  end
  else begin
    if DropDownWidth <> 0 then
      Self.Perform(CB_SETDROPPEDWIDTH, DropDownWidth, 0);

    Inherited;
  end;
end;

// !! CBN_DROPDOWN = WM_SETFOCUS !!
// Message.WParam is the handle of WinControl that loose focus
// Entering combobox by clicking the button or by tab ...
// When user click on right button to drop down and editor has no focus ...
// Also called when clicking outside and DropDownControl already Dropped down ...
procedure TcyBaseCombobox.CBNDROPDOWN(var Message: TMessage);
var
  fromControl: TWinControl;
begin
  if Assigned(FDropDownControl) or Assigned(FInBuiltDropDownControl) then
  begin
    // 2016-05-03 Do not DropDown when entering with tab !      SavTickCountCloseUp := GetTickCount;

    (*fromControl := FindControl(Message.WParam);

    if Assigned(fromControl) then
      if fromControl.Parent = Self.Parent then
        Exit;

    // CBNDROPDOWN also called when clicking outside and DropDownControl already Dropped down ...
    if GetTickCount - SavTickCountCloseUp >= 200 then   // Works between 100 and 300 ms ...
    begin
      // SavTickCountCloseUp := 0;
      //    PostMessage(Handle, WM_CANCELMODE, 0, 0);
      //    PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
      DropDown;
    end;  *)
  end
  else
    inherited;
end;

// Exiting combobox by clicking outside ...
procedure TcyBaseCombobox.CBSHOWDROPDOWN(var Message: TMessage);
begin
  inherited;
end;

// Drop down on mouse down when editor focused !
// Also: because if already focused, can' t dropdrown FFlyingContainer.Control twice ...
procedure TcyBaseCombobox.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if (not Assigned(FDropDownControl)) and (not Assigned(FInBuiltDropDownControl)) then
  begin
    inherited;
    Exit;
  end;

  // Open ?
  if GetTickCount - SavTickCountCloseUp < 200 then   // Works between 100 and 300 ms ...
  begin
    inherited;
    Exit;
  end;

  // inherited; !! Don' t call beucase it will drop down TComboBox list !!

  DropDown;
end;

end.
