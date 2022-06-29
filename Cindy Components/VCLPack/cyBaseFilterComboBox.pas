{   Component(s):
    tcyFilterCombobox

    Description:
    A Combobox with user filter feature.

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

unit cyBaseFilterComboBox;

interface

uses Classes, Windows, Messages, Forms, SysUtils, Graphics, Controls, StdCtrls, cyStrUtils, cyFlyingContainer, cyBaseCombobox, cyPanel, cyEdit;

type
  TFilterChangeOption = (fcKeepSelectedItem, fcSelectFirstItem, fcUnselect);

  TBeforeDropDownEvent = procedure (Sender: TObject; var Allow: Boolean) of object;
  TInbuiltDrawFilteredItem = procedure (Sender: TObject; Canvas: TCanvas; Index: Integer; Rect: TRect; State: TOwnerDrawState) of object;

  TcyBaseFilterCombobox = class(TcyBaseCombobox)
  private
    DiscardResynchItems: Boolean;
    FInbuiltFilter: string;
    FAllItemsList: TListBox;
    FFilterOptions: TStrFilterOptions;
    FBeforeDropDown: TBeforeDropDownEvent;
    FOnInbuiltDrawFilteredItem: TInbuiltDrawFilteredItem;
    FFilterChangeOption: TFilterChangeOption;
    procedure SetInbuiltFilter(const Value: string);
    procedure SetItems(const Value: TStrings);
    function GetItems: TStrings;
    function GetFilteredItems: TStrings;
    function SelectCurrentFListBoxItem(const CloseDropDown: Boolean = true): Integer;
    function GetSorted: Boolean;
    procedure SetSorted(const Value: Boolean);
  protected
    FContainer: TcyPanel;
    FEditFilter: TcyEdit;
    FListBox: TListBox;

    procedure DropDown; override;
    procedure Loaded; override;
    procedure KeyPress(var Key: Char); override;
    procedure SetStyle(Value: TComboBoxStyle); override;
    procedure SetItemIndex(const Value: Integer); override;
    procedure FilterItems(SourceItems: TStrings);
    property InbuiltFilter: string read FInbuiltFilter write SetInbuiltFilter;
    //   FilteredItems can returns inherited Items (TComboBox.Items) or FListBox.Items
    property FilteredItems: TStrings read GetFilteredItems;
    property FilterOptions: TStrFilterOptions read FFilterOptions write FFilterOptions default [strfoCaseInsensitive, strfoAccentsInsensitive];
    // Design time:
    //   Items are still stored in inherited Items (TComboBox.Items)

    // Run time:
    //   Items return FAllItem (unfiltered) !
    property Items: TStrings read GetItems write SetItems;
    property BeforeDropDown: TBeforeDropDownEvent read FBeforeDropDown write FBeforeDropDown;
    property FilterChangeOption: TFilterChangeOption read FFilterChangeOption write FFilterChangeOption default fcKeepSelectedItem;
    property OnInbuiltDrawFilteredItem: TInbuiltDrawFilteredItem read FOnInbuiltDrawFilteredItem write FOnInbuiltDrawFilteredItem;

    procedure EditFilterChange(Sender: TObject);
    procedure EditFiltrerKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditFiltrerKeyPress(Sender: TObject; var Key: Char);
    procedure ListBoxKeyPress(Sender: TObject; var Key: Char);
    procedure ListBoxClick(Sender: TObject);
    procedure ListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure ResynchItems;
  published
    property Sorted: Boolean read GetSorted write SetSorted;
  end;

  TcyFilterCombobox = class(TcyBaseFilterCombobox)
  private
  protected
  public
    function GetDropDownControlDefs: TcyFlyingContainer;
    function GetContainer: TcyPanel;
    function GetEdit: TcyEdit;
    function GetListBox: TListBox;
    property InbuiltFilter;
    property FilteredItems;
  published
    property DropDownControl;
    property DropDownControlOptions;
    property DropDownWidth;
    property TagStr;
    property FilterChangeOption;
    property FilterOptions;
    property Items;
    property BeforeDropDown;
    property OnInbuiltDrawFilteredItem;
  end;

implementation

{ TcyBaseFilterCombobox }
constructor TcyBaseFilterCombobox.Create(AOwner: TComponent);
begin
  inherited;

  DiscardResynchItems := false;
  FFilterOptions := [strfoCaseInsensitive, strfoAccentsInsensitive];
  FAllItemsList := TListBox.Create(Self);
  FAllItemsList.Visible := false;
  FAllItemsList.Parent := Self;
  if csDesigning in ComponentState then
    FAllItemsList.Top := 800;

  FInbuiltFilter := '';

  FContainer := TcyPanel.Create(Self);
  FContainer.Visible := false;
  FContainer.Parent := Self;
  FContainer.Height := 250;
  FContainer.Degrade.FromColor := clWindow;   // Avoid Small different appearence before enter the first time on control at run-time because of FContainer.Parent := Self ...
  FContainer.Degrade.ToColor := clWindow;
  if FContainer.Bevels.Count = 0 then
    FContainer.Bevels.Add;
  if csDesigning in ComponentState then
    FContainer.Top := 800;    // Hide component at design time ...
  FContainer.Bevels[0].HighlightColor := clWindowFrame;
  FContainer.Bevels[0].ShadowColor := clWindowFrame;

  // FFlyingContainer.Control := FContainer;
  FFilterChangeOption := fcKeepSelectedItem;;
  FEditFilter := TcyEdit.Create(FContainer);
  FEditFilter.Parent := FContainer;
  FEditFilter.Align := alTop;
  FEditFilter.OnChange := EditFilterChange;
  FEditFilter.OnKeyDown := EditFiltrerKeyDown;
  FEditFilter.OnKeyPress := EditFiltrerKeyPress;
  FListBox := TListBox.Create(FContainer);
  FListBox.Parent := FContainer;
  FListBox.BorderStyle := bsNone;
  FListBox.Align := alClient;
  FListBox.OnKeyPress := ListBoxKeyPress;
  FListBox.OnClick := ListBoxClick;
  FListBox.OnDrawItem := ListBoxDrawItem;
end;

destructor TcyBaseFilterCombobox.Destroy;
begin
  FListBox.Free;
  FEditFilter.Free;
  FContainer.Free;

  FAllItemsList.Free;

  inherited;
end;

procedure TcyBaseFilterCombobox.Clear;
begin
  FAllItemsList.Items.Clear;

  inherited;   // Will clear inherited Items ...
end;

function TcyBaseFilterCombobox.GetFilteredItems: TStrings;
begin
  if Assigned(FInBuiltDropDownControl)
  then Result := FListBox.Items
  else Result := inherited Items;
end;

function TcyBaseFilterCombobox.GetItems: TStrings;
begin
  if csDesigning in ComponentState then
    Result := inherited Items
  else
    if csLoading in ComponentState then
      Result := inherited Items    // Load stored items at design time !
    else
      Result := FAllItemsList.Items;
end;

function TcyBaseFilterCombobox.GetSorted: Boolean;
begin
  Result := FAllItemsList.Sorted
end;

procedure TcyBaseFilterCombobox.SetItems(const Value: TStrings);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(inherited Items) then
      inherited Items.Assign(Value)
    else
      inherited Items := Value;
  end
  else begin
    if Assigned(FAllItemsList.Items) then
      FAllItemsList.Items.Assign(Value)
    else
      FAllItemsList.Items := Value;
  end;
end;

procedure TcyBaseFilterCombobox.ResynchItems;
begin
  if DiscardResynchItems then Exit;

  if Assigned(inherited Items) then
    inherited Items.Assign(FAllItemsList.Items)
  else
    inherited Items := FAllItemsList.Items;
end;

procedure TcyBaseFilterCombobox.SetItemIndex(const Value: Integer);
begin
  // !!! Need to Resynch inherited Items because we may have change FAllItemsList with property Items at run-time !!!
  if Value <> Self.ItemIndex then
    if not (csDesigning in ComponentState) then
      if not (csLoading in ComponentState) then
        if inherited Items.Text <> FAllItemsList.Items.Text then
          ResynchItems;

  inherited;
end;

procedure TcyBaseFilterCombobox.SetSorted(const Value: Boolean);
begin
  FAllItemsList.Sorted := Value;
  inherited Sorted := Value;

  // !!! Need to Resynch inherited Items because we may have change FAllItemsList with property Items at run-time !!!
  if not (csDesigning in ComponentState) then
    if not (csLoading in ComponentState) then
      if inherited Items.Text <> FAllItemsList.Items.Text then
        ResynchItems;
end;

procedure TcyBaseFilterCombobox.SetStyle(Value: TComboBoxStyle);
begin
  inherited;

  if Style in [csDropDown, csSimple]
  then FInBuiltDropDownControl := Nil
  else FInBuiltDropDownControl := FContainer;
end;

procedure TcyBaseFilterCombobox.Loaded;
begin
  DiscardResynchItems := true;
  inherited;
  DiscardResynchItems := false;

  // Copy inherited Items to FAllItemsList :
  if not (csDesigning in ComponentState) then
    if Assigned(inherited Items) then
      FAllItemsList.Items.Assign(inherited Items);
end;

procedure TcyBaseFilterCombobox.DropDown;
var
  Allow: Boolean;
  SaveSelectedItemIndex, MatchFilterItemIndex, ItemsCount, IncHeight: Integer;
  SaveSelectedText: String;
  SavClick: TNotifyEvent;
begin
  Allow := true;

  if Assigned(FInBuiltDropDownControl) then
  begin
    if Assigned(FOnInbuiltDrawFilteredItem)
    then FListBox.Style := lbOwnerDrawFixed
    else FListBox.Style := lbStandard;

    FListBox.Color := Self.Color;
    FListBox.ParentFont := false;
    FListBox.Font := Self.Font;
    // Not needed ... FListBox.ItemHeight := Self.ItemHeight;

    FEditFilter.Color := Self.Color;
    FEditFilter.ParentFont := false;   // 2017-06-12
    FEditFilter.Font := Self.Font;
  end;

  if Assigned(FBeforeDropDown) then
    FBeforeDropDown(Self, Allow);

  if not Allow then Exit;

  // 2016-05-05 Update inherited Items with FAllItemsList :
  if inherited Items.Text <> FAllItemsList.Items.Text then
  begin
    SaveSelectedItemIndex := Self.ItemIndex;
    SaveSelectedText := Self.Text;

    SavClick := Self.OnClick;
    Self.OnClick := Nil;

    ResynchItems;

    // Reassign ItemIndex (lost updating inherited Items if Style = csDropDownList) :
    if SaveSelectedItemIndex <> -1 then
    begin
      MatchFilterItemIndex := inherited Items.IndexOf(SaveSelectedText);

      if MatchFilterItemIndex <> Self.ItemIndex then
        Self.ItemIndex := MatchFilterItemIndex;
    end;

    Self.OnClick := SavClick;
  end;


  // Clear user edit filter :
  if FInbuiltFilter <> '' then
    InbuiltFilter := ''
  else
    if Assigned(FInBuiltDropDownControl) then
    begin
      FListBox.OnClick := Nil;

      if FListBox.Items.Text <> inherited Items.Text then
        FListBox.Items.Assign(inherited Items);

      FListBox.ItemIndex := ItemIndex;     // Same here ...
      FListBox.OnClick := ListBoxClick;

      // Calc Height :
      ItemsCount := DropDownCount;
      if ItemsCount > FAllItemsList.Items.Count then
        ItemsCount := FAllItemsList.Items.Count;

      IncHeight := FContainer.ClientHeight - FListBox.Height;  // Height used by other controls than the DBGrid ...
      FContainer.Height := (ItemsCount * ItemHeight) + IncHeight + 4;  // 4 for ListBox.BorderStyle ...
    end;

  Inherited;
end;

// TComboBox Editor :
procedure TcyBaseFilterCombobox.KeyPress(var Key: Char);
var
  NewInput: String;
  SavSelStart, SavSelLength: Integer;
begin
  inherited;

  if not Assigned(FInBuiltDropDownControl) then
  begin
    // We need to copy non selected text :
    NewInput := Copy(Text, 1, SelStart);
    if Key <> #0 then
      NewInput := NewInput + Key;

    if FInbuiltFilter <> NewInput then
    begin
      SavSelStart := SelStart;
      SavSelLength := SelLength;

      InbuiltFilter := NewInput;

      SelStart  := SavSelStart;
      SelLength := SavSelLength;
    end;
  end;
end;

procedure TcyBaseFilterCombobox.SetInbuiltFilter(const Value: string);
var
  SaveSelectedText: String;
  MatchFilterItemIndex, i: Integer;
  ReassignAllItems: Boolean;
begin
  if FInbuiltFilter = Value then Exit;
  ReassignAllItems := pos(FInbuiltFilter, Value) <> 1;
  FInbuiltFilter := Value;

  if Assigned(FInBuiltDropDownControl) then
  begin
    SaveSelectedText := Self.Text;     // Select previous selected text by default !!!

    if ReassignAllItems then
    begin
      if FListBox.Items.Text <> FAllItemsList.Items.Text then
        FListBox.Items.Assign(FAllItemsList.Items);
    end;

    FilterItems(FListBox.Items);

    FListBox.OnClick := Nil;

    case FFilterChangeOption of
      fcKeepSelectedItem:
      begin
        if SaveSelectedText <> '' then
        begin
          MatchFilterItemIndex := FListBox.Items.IndexOf(SaveSelectedText);

          if MatchFilterItemIndex = -1 then
            if FListBox.Items.Count > 0 then
            begin
              MatchFilterItemIndex := 0;

              // 2016-05-19 Try finding starting the same :
              for i := 0 to FListBox.Items.Count-1 do
                if String_MatchInput(FListBox.Items[i], FInbuiltFilter, [strfoPartialKey, strfoAccentsInsensitive, strfoCaseInsensitive]) then
                begin
                  MatchFilterItemIndex := i;
                  Break;
                end;
            end;
        end
        else
          MatchFilterItemIndex := -1;
      end;

      fcSelectFirstItem:
      begin
        if FListBox.Items.Count <> 0
        then MatchFilterItemIndex := 0
        else MatchFilterItemIndex := -1;
      end;

      else
        MatchFilterItemIndex := -1;
    end;

    FListBox.ItemIndex := MatchFilterItemIndex;
    FListBox.OnClick := ListBoxClick;
  end
  else begin
    if ReassignAllItems then
      if inherited Items.Text <> FAllItemsList.Items.Text then
        ResynchItems;

    FilterItems(inherited Items);
  end;
end;

procedure TcyBaseFilterCombobox.FilterItems(SourceItems: TStrings);
var
  i: Integer;
begin
  // Remove unmatched items :
  SourceItems.BeginUpdate;

  for i := SourceItems.Count-1 downto 0 do
    if not String_MatchInput(SourceItems[i], FInbuiltFilter, FFilterOptions) then
      SourceItems.Delete(i);

  SourceItems.EndUpdate;
end;

// *** Inbuild DropDown Panel *** //
function TcyBaseFilterCombobox.SelectCurrentFListBoxItem(const CloseDropDown: Boolean = true): Integer;
begin
  if FListBox.ItemIndex <> -1
  then Result := inherited Items.IndexOf(FListBox.Items[FListBox.ItemIndex])
  else Result := -1;

  if Result <> Self.ItemIndex then
  begin
    Self.ItemIndex := Result;

    if Assigned(OnClick) then
      OnClick(Self);

    if Assigned(OnChange) then
      OnChange(Self);
  end;

  if CloseDropDown then
    if FFlyingContainer.Active then
      FFlyingContainer.Close;
end;

procedure TcyBaseFilterCombobox.ListBoxKeyPress(Sender: TObject; var Key: Char);
var
  NewItemIndex: Integer;
begin
  if FListBox.ItemIndex <> -1 then
    if Key = #13 then
    begin
      Key := #0;
      SelectCurrentFListBoxItem;
    end;
end;

procedure TcyBaseFilterCombobox.ListBoxClick(Sender: TObject);
begin
  if FListBox.ItemIndex <> -1 then
    SelectCurrentFListBoxItem;
end;

procedure TcyBaseFilterCombobox.ListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  if Assigned(FOnInbuiltDrawFilteredItem) then
    FOnInbuiltDrawFilteredItem(Self, FListBox.Canvas, Index, Rect, State);
end;

procedure TcyBaseFilterCombobox.EditFiltrerKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  NewItemIndex: Integer;
begin
  NewItemIndex := FListBox.ItemIndex;

  if Shift = [ssCtrl] then
    if Key = VK_HOME   then    // Ctrl + HOME ...
    begin
      Key := 0;
      NewItemIndex := 0;
    end
    else
      if Key = VK_END then   // Ctrl + END ...
      begin
        Key := 0;
        NewItemIndex := FListBox.Items.Count - 1;
      end;


  if Key = VK_PRIOR then // PAGE UP ...
  begin
    Key := 0;
    NewItemIndex := NewItemIndex - (FListBox.ClientHeight div FListBox.ItemHeight);
    if NewItemIndex < 0 then NewItemIndex := 0;
  end
  else
    if Key = VK_NEXT then // PAGE DOWN ...
    begin
      Key := 0;
      NewItemIndex := NewItemIndex + (FListBox.ClientHeight div FListBox.ItemHeight);
    end
    else
      if Key = VK_DOWN then
      begin
        Key := 0;

        if FListBox.ItemIndex = -1
        then NewItemIndex := 0
        else NewItemIndex := NewItemIndex + 1;
      end
      else
        if Key = VK_UP then
        begin
          Key := 0;

          if FListBox.ItemIndex = -1
          then NewItemIndex := FListBox.Items.Count - 1
          else NewItemIndex := NewItemIndex - 1;
        end;

  if NewItemIndex > FListBox.Items.Count -1 then
    NewItemIndex := FListBox.Items.Count -1;

  if NewItemIndex <> FListBox.ItemIndex then
    if NewItemIndex >= 0 then
    begin
      FListBox.OnClick := Nil;
      FListBox.ItemIndex := NewItemIndex;
      FListBox.OnClick := ListBoxClick;
    end;
end;

procedure TcyBaseFilterCombobox.EditFiltrerKeyPress(Sender: TObject; var Key: Char);
begin
  if FListBox.ItemIndex <> -1 then
    if Key = Char(VK_RETURN) then
    begin
      Key := #0;
      SelectCurrentFListBoxItem;
    end;
end;

procedure TcyBaseFilterCombobox.EditFilterChange(Sender: TObject);
begin
  InbuiltFilter := FEditFilter.Text;
end;

{ TcyFilterCombobox }
function TcyFilterCombobox.GetDropDownControlDefs: TcyFlyingContainer;
begin
  Result := FFlyingContainer;
end;

function TcyFilterCombobox.GetContainer: TcyPanel;
begin
  Result := FContainer;
end;

function TcyFilterCombobox.GetEdit: TcyEdit;
begin
  Result := FEditFilter;
end;

function TcyFilterCombobox.GetListBox: TListBox;
begin
  Result := FListBox;
end;

end.
