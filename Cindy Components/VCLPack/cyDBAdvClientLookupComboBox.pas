{   Component(s):
    TcyDBAdvClientLookupComboBox

    Description:
    Like TcyDBClientLookupCombBox with inbuilt dropdown panel with edit for filter and dbgrid column sort

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

unit cyDBAdvClientLookupComboBox;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Windows, Graphics, Messages, Forms, SysUtils, Controls, StdCtrls, DBGrids, Db,
       cyDb, cyBaseDBClientlookupComboBox, cyPanel, cyEdit, cyBaseDBGrid, cyDBAdvGrid, cyStrUtils;

type
  TEditFilterResetMode = (erManual, erOnMouseEnter);

  TEditFilterOptions = class(TPersistent)
  private
    FBaseDBLookup: TcyBaseDBClientLookupComboBox;
    fResetMode: TEditFilterResetMode;
    FIgnoreAccents: Boolean;
    FMargins: Word;
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property IgnoreAccents: Boolean read FIgnoreAccents write FIgnoreAccents default false;
    {$IFDEF UNICODE}
    property Margins: Word read FMargins write FMargins default 2;
    {$ENDIF}
    property ResetMode: TEditFilterResetMode read fResetMode write fResetMode default erManual;
  end;

  TcyDBAdvClientLookupComboBox = class(TcyBaseDBClientLookupComboBox)
  private
    savedDropDownRows: Integer;
    AccentuedEditFilterValue: Boolean;
    FEditFilterField: String;
    fEditFilterOptions: TEditFilterOptions;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure SetEditFilterField(const Value: String);
    function GetListOptions: TDBGridOptions;
    procedure SetListOptions(const Value: TDBGridOptions);
  protected
    FContainer: TcyPanel;
    FEditFilter: TcyEdit;
    FDBGrid: TcyDBAdvGrid;
    procedure UpdateContainer;
    procedure DropDown; override;
    procedure UpdateInternalDataset; override;   // Only called when user Drop down ...
    procedure EditFilterChange(Sender: TObject);
    procedure EditFiltrerKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditFiltrerKeyPress(Sender: TObject; var Key: Char);
    procedure DBGridKeyPress(Sender: TObject; var Key: Char);
    procedure DBGridSelectCitySortColumnUserDefine(Sender: TObject; Column: TColumn; var Ascendant, Accept: Boolean);
    procedure DBGridSelectCitySortColumnsUserChange(Sender: TObject);
    procedure DBGridCellClick(Column: TColumn);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ListValue;
    function GetContainer: TcyPanel;
    function GetEdit: TcyEdit;
    function GetDBGrid: TcyDBAdvGrid;
  published
    property EditFilterField: String read FEditFilterField write SetEditFilterField;
    property EditFilterOptions: TEditFilterOptions read fEditFilterOptions write fEditFilterOptions;
    property ListOptions: TDBGridOptions read GetListOptions write SetListOptions;
    // property DropDownControl;
    // property DropDownControlOptions;
    property DropDownRefreshList;
    property DropDownRowsMin;
    property ListOrderFieldNames;
    property ListFilter;
    property ListFilterOptions;
    property ListSource;  // New ListSource property !
    property OnUpdateInternalDataset;
  end;

implementation

{ TEditFilterOptions }
constructor TEditFilterOptions.Create(AOwner: TComponent);
begin
  FBaseDBLookup := TcyBaseDBClientLookupComboBox(AOwner);
  FIgnoreAccents := false;
  fResetMode := erManual;
  FMargins := 2;
end;

{ TcyDBAdvClientLookupComboBox }
constructor TcyDBAdvClientLookupComboBox.Create(AOwner: TComponent);
begin
  inherited;

  FEditFilterOptions := TEditFilterOptions.Create(self);

  savedDropDownRows := 0;
  FContainer := TcyPanel.Create(Self);
  FContainer.Visible := false;
  FContainer.Parent := Self;
  FContainer.Height := 250;
  if FContainer.Bevels.Count = 0 then
    FContainer.Bevels.Add;
  if csDesigning in ComponentState then
    FContainer.Top := 100;    // Hide component at design time ...
  FContainer.Bevels[0].HighlightColor := clWindowFrame;
  FContainer.Bevels[0].ShadowColor := clWindowFrame;
  FFlyingContainer.Control := FContainer;
  FEditFilter := TcyEdit.Create(FContainer);
  FEditFilter.Parent := FContainer;
  FEditFilter.ParentFont := false;
  FEditFilter.Visible := false;
  FEditFilter.Align := alTop;
  FEditFilter.OnChange := EditFilterChange;
  FEditFilter.OnKeyDown := EditFiltrerKeyDown;
  FEditFilter.OnKeyPress := EditFiltrerKeyPress;

  FDBGrid := TcyDBAdvGrid.Create(FContainer);
  FDBGrid.Parent := FContainer;
  FDBGrid.ParentFont := false;
  FDBGrid.BorderStyle := bsNone;
  FDBGrid.Align := alClient;
  FDBGrid.ClientColumn.Mode := cmByFieldName;
  //  FDBGrid.ClientColumn.Enabled := true;  Can' t do here ...
  FDBGrid.DataSource := FInternalListSource;
  FDBGrid.Options := [dgTitles,dgRowSelect,dgAlwaysShowSelection,dgConfirmDelete,dgCancelOnExit];
  FDBGrid.SortIndicatorsOptions.MultiSelect := true;
  FDBGrid.SortIndicatorsOptions.ReadOnly := false;
  FDBGrid.OnKeyPress := DBGridKeyPress;
  FDBGrid.OnSortColumnUserDefine := DBGridSelectCitySortColumnUserDefine;
  FDBGrid.OnSortColumnsUserChange := DBGridSelectCitySortColumnsUserChange;
  FDBGrid.OnCellClick := DBGridCellClick;
end;

destructor TcyDBAdvClientLookupComboBox.Destroy;
begin
  FEditFilterOptions.Free;

  FDBGrid.Free;
  FEditFilter.Free;
  FContainer.Free;
  inherited;
end;

procedure TcyDBAdvClientLookupComboBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;

  // Remove filter :
  if fEditFilterOptions.fResetMode = erOnMouseEnter then
    if not FFlyingContainer.Active then
      FEditFilter.Text := '';   // Will call onChange ...
end;

procedure TcyDBAdvClientLookupComboBox.DBGridCellClick(Column: TColumn);
begin
  Self.SelectCurrentKeyValue(true);
end;

procedure TcyDBAdvClientLookupComboBox.DBGridKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    Self.SelectCurrentKeyValue(true);
  end;
end;

procedure TcyDBAdvClientLookupComboBox.DBGridSelectCitySortColumnUserDefine(Sender: TObject; Column: TColumn; var Ascendant, Accept: Boolean);
begin
  Accept := cyDB.ValidFieldForClientDatasetIndex(Column.Field);
end;

procedure TcyDBAdvClientLookupComboBox.DBGridSelectCitySortColumnsUserChange(Sender: TObject);
begin
  FDBGrid.ApplySortedColumnsToClientDataset;
end;

procedure TcyDBAdvClientLookupComboBox.DropDown;
begin
  FEditFilter.ParentFont := false;
  FEditFilter.Font := Self.Font;
  FEditFilter.Color := Self.Color;

  {$IFDEF UNICODE}
  FEditFilter.Margins.Left := fEditFilterOptions.FMargins;
  FEditFilter.Margins.Top := fEditFilterOptions.FMargins;
  FEditFilter.Margins.Right := fEditFilterOptions.FMargins;
  FEditFilter.Margins.Bottom := fEditFilterOptions.FMargins;
  FEditFilter.AlignWithMargins := fEditFilterOptions.FMargins <> 0;
  {$ENDIF}

  FDBGrid.ParentFont := false;
  FDBGrid.Font := Self.Font;
  FDBGrid.TitleFont := Self.Font;
  FDBGrid.Color := Self.Color;

  inherited;

  // ReApply index :
  if FDBGrid.SortColumns.Count <> 0 then
    FDBGrid.ApplySortedColumnsToClientDataset;
end;

procedure TcyDBAdvClientLookupComboBox.EditFiltrerKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [ssCtrl] then
    if Key = VK_HOME   then    // Ctrl + HOME ...
    begin
      Key := 0;
      Self.GetInternalDataSet.First;
    end
    else
      if Key = VK_END then   // Ctrl + END ...
      begin
        Key := 0;
        Self.GetInternalDataSet.Last;
      end;

  if Key = VK_PRIOR then // PAGE UP ...
  begin
    Key := 0;
    Self.GetInternalDataSet.MoveBy((-1) * DropDownRows);
  end
  else
    if Key = VK_NEXT then // PAGE DOWN ...
    begin
      Key := 0;
      Self.GetInternalDataSet.MoveBy(DropDownRows);
    end
    else
      if Key = VK_DOWN then
      begin
        Key := 0;
        Self.GetInternalDataSet.Next;
      end
      else
        if Key = VK_UP then
        begin
          Key := 0;
          Self.GetInternalDataSet.Prior;
        end;
end;

procedure TcyDBAdvClientLookupComboBox.EditFiltrerKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Char(VK_RETURN) then
  begin
    Key := #0;
    Self.SelectCurrentKeyValue(True);
  end;
end;

function TcyDBAdvClientLookupComboBox.GetContainer: TcyPanel;
begin
  Result := FContainer;
end;

function TcyDBAdvClientLookupComboBox.GetDBGrid: TcyDBAdvGrid;
begin
  Result := FDBGrid;
end;

function TcyDBAdvClientLookupComboBox.GetEdit: TcyEdit;
begin
  Result := FEditFilter;
end;

function TcyDBAdvClientLookupComboBox.GetListOptions: TDBGridOptions;
begin
  Result := FDBGrid.Options;
end;

procedure TcyDBAdvClientLookupComboBox.SetEditFilterField(const Value: String);
begin
  FEditFilterField := Value;
  FEditFilter.Visible := FEditFilterField <> '';
end;

procedure TcyDBAdvClientLookupComboBox.SetListOptions(const Value: TDBGridOptions);
begin
  FDBGrid.Options := Value;
end;

procedure TcyDBAdvClientLookupComboBox.UpdateContainer;
var
  DataRowHeight, IncHeight, DropDownRows, c, f: Integer;
  Column: TColumn;
begin
  // Display label / display format / display values / Display Width :
  for f := 0 to FInternalDataset.FieldCount-1 do
    if FcyListLink.DataSource.Dataset.FindField(FInternalDataset.Fields[f].FieldName) <> Nil then
      try
        FInternalDataset.Fields[f].DisplayLabel := FcyListLink.DataSource.Dataset.FindField(FInternalDataset.Fields[f].FieldName).DisplayLabel;
        FInternalDataset.Fields[f].DisplayWidth := FcyListLink.DataSource.Dataset.FindField(FInternalDataset.Fields[f].FieldName).DisplayWidth;

        if FInternalDataset.Fields[f] is TIntegerField then
          TIntegerField(FInternalDataset.Fields[f]).DisplayFormat := TIntegerField( FcyListLink.DataSource.Dataset.FindField(FInternalDataset.Fields[f].FieldName) ).DisplayFormat;

        if FInternalDataset.Fields[f] is TFloatField then
          TFloatField(FInternalDataset.Fields[f]).DisplayFormat := TFloatField( FcyListLink.DataSource.Dataset.FindField(FInternalDataset.Fields[f].FieldName) ).DisplayFormat;

        if FInternalDataset.Fields[f] is TBooleanField then
          TBooleanField(FInternalDataset.Fields[f]).DisplayValues := TBooleanField( FcyListLink.DataSource.Dataset.FindField(FInternalDataset.Fields[f].FieldName) ).DisplayValues;
      except
      end;


  // Container Height :
  IncHeight := FContainer.ClientHeight - FDBGrid.Height;  // Height used by other controls than the DBGrid ...

  {$IFDEF UNICODE}
  // 2016-08-05
  IncHeight := IncHeight + FDBGrid.Margins.Top + FDBGrid.Margins.Bottom;
  {$ENDIF}

  DataRowHeight := 0;

  if dgTitles in FDBGrid.Options then
  begin
    if FDBGrid.RowCount > 1 then
    begin
      IncHeight := IncHeight + FDBGrid.RowHeights[0];
      DataRowHeight := FDBGrid.RowHeights[1];
    end;
  end
  else
    if FDBGrid.RowCount > 0 then
      DataRowHeight := FDBGrid.RowHeights[0];

  if DataRowHeight <> 0 then
  begin
    DropDownRows := Self.DropDownRows;

    if FInternalDataset.Active then
      if InternalFilter = '' then
      begin
        if DropDownRows > FInternalDataset.RecordCount then
          DropDownRows := FInternalDataset.RecordCount;
      end
      else
        if (FEditFilter.Text <> '') and (SavedDropDownRows <> 0) then
          if savedDropDownRows > FInternalDataset.RecordCount then
            DropDownRows := savedDropDownRows              // 2017-03-16 Restore previous size because nothing changes ...
          else       // It seems that we have now more records, so we adjust :
            if DropDownRows > FInternalDataset.RecordCount then
              DropDownRows := FInternalDataset.RecordCount;

    if DropDownRows < DropDownRowsMin then
      DropDownRows := DropDownRowsMin;

    savedDropDownRows := DropDownRows;
    FContainer.Height := (DropDownRows * DataRowHeight) + IncHeight;
  end;


  // Columns :
  FDBGrid.Columns.BeginUpdate;
  FDBGrid.Columns.Clear;

  for c := 1 to SubString_Count(ListField, ';') do
  begin
    Column := FDBGrid.Columns.Add;
    Column.FieldName := SubString_Get(ListField, ';', c);
  end;

  FDBGrid.Columns.EndUpdate;

  FDBGrid.ClientColumn.FieldName := SubString_Get(ListField, ';', ListFieldIndex + 1);
  FDBGrid.ClientColumn.Enabled := true;
end;

procedure TcyDBAdvClientLookupComboBox.UpdateInternalDataset;
begin
  inherited;
  UpdateContainer;
end;

procedure TcyDBAdvClientLookupComboBox.EditFilterChange(Sender: TObject);
var
  EditFilterValue, NoAccents, FieldName, UserFilter, LikePrefix: String;
  i: Integer;
  locateOptions: TLocateOptions;

      procedure AddToUserFilter(aWord: string);
      begin
        if aWord = '' then Exit;

        if FcyListLink.DataSource.DataSet.FieldByName(FieldName).DataType in [ftString, ftWideString, ftVariant, ftFixedChar{$IFDEF UNICODE}, ftFixedWideChar {$ENDIF}]
        then AddToFilter(UserFilter,  '[' + FieldName + ']' + ' LIKE ' + QuotedStr(LikePrefix + aWord + '%'), faAnd)
        else AddToFilter(UserFilter, '[' + FieldName + ']' + ' = ' + QuotedStr(aWord), faAnd);
      end;

begin
  FieldName := EditFilterField;

  if FieldName = '' then Exit;

  UserFilter := '';
  AccentuedEditFilterValue := false;
  EditFilterValue := FEditFilter.Text;

  // 2017-03-21 To work, the values on field specified on property EditFilterField must be with no accent !
  if fEditFilterOptions.FIgnoreAccents then
  begin
    NoAccents := cyStrUtils.String_RemoveAccentsFromChars(EditFilterValue);

    for i := Length(NoAccents) downto 1 do
      if EditFilterValue[i] <> NoAccents[i] then
      begin
        AccentuedEditFilterValue := true;
        EditFilterValue[i] := NoAccents[i];
        // Delete(EditFilterValue[i], 1, 1);  // Ignore char ...
      end;
  end;

  if EditFilterValue <> '' then
    if foNoPartialCompare in ListFilterOptions then
    begin
      LikePrefix := '';   // Must start as user filter ...
      AddToUserFilter(EditFilterValue);
    end
    else begin
      LikePrefix := '%';

      // Add filter for each word :
      for i := 1 to SubString_Count(EditFilterValue, ' ') do
        AddToUserFilter( SubString_Get(EditFilterValue, ' ', i) );
    end;

  InternalFilter := UserFilter;

  // 2015-05-12 Locate :
  if (FEditFilterField <> '') and (EditFilterValue <> '') then
    try
      if foCaseInsensitive in ListFilterOptions
      then locateOptions := [loPartialKey, loCaseInsensitive]
      else locateOptions := [loPartialKey];

      FInternalDataset.Locate(FEditFilterField, EditFilterValue, locateOptions);
    except
    end;
end;

end.
