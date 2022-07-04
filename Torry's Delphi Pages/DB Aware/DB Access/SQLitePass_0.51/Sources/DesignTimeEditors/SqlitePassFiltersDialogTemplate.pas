{ This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  ---------------------------------------------------------------------------

    Author : Luc DAVID Email: luckylazarus@free.fr
    2007 - 2010
    Last update : 28/01/2010

  --------------------------------------------------------------------------- }

unit SqlitePassFiltersDialogTemplate;
{$i ..\..\Sources\SqlitePassDbo.inc}

interface

uses
 {$IFDEF FPC}
  LResources,
 {$ELSE}
  Windows, Messages, Math,
 {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, db, SqlitePassDbo, CheckLst, ComCtrls;

type
  TSqlitePassFilterDialogTemplate = class(TForm)
    PanelIndexApplyChanges: TPanel;
    SbCancel: TSpeedButton;
    SbOk: TSpeedButton;
    PageControl: TPageControl;
    tsCustomFilters: TTabSheet;
    PanelIndexesToolBar: TPanel;
    Bevel1: TBevel;
    LabelFiltersDefinitions: TLabel;
    PanelSetFilterText: TPanel;
    Edit1: TEdit;
    Panel1: TPanel;
    SbClearAll: TSpeedButton;
    SbDisableAll: TSpeedButton;
    SbEnableAll: TSpeedButton;
    SbDisplayFilterStmt: TSpeedButton;
    PanelIndexMain: TPanel;
    PanelAvailableIndexes: TPanel;
    PanelCaptionAvailableIFields: TPanel;
    Image5: TImage;
    CheckListBoxFields: TCheckListBox;
    PanelSQL: TPanel;
    SbSelectValueByList1: TSpeedButton;
    SbSelectValueByList2: TSpeedButton;
    Bevel3: TBevel;
    SbClearFilter: TSpeedButton;
    LabelDataFormat: TLabel;
    PanelIndexCreateStmt: TPanel;
    Label21: TLabel;
    Image4: TImage;
    ComboBoxFilterOperator1: TComboBox;
    RbAnd: TRadioButton;
    RbOr: TRadioButton;
    EditFilterValue1: TEdit;
    ComboBoxFilterOperator2: TComboBox;
    EditFilterValue2: TEdit;
    procedure SbCancelClick(Sender: TObject);
    procedure SbClearAllClick(Sender: TObject);
    procedure SbSelectValueByList1Click(Sender: TObject);
    procedure ComboBoxFilterOperator1Change(Sender: TObject);
    procedure ComboBoxFilterOperator2Change(Sender: TObject);
    procedure SbClearFilterClick(Sender: TObject);
    procedure CheckListBoxFieldsClick(Sender: TObject);
    procedure SbDisableAllClick(Sender: TObject);
    procedure SbSelectValueByList2Click(Sender: TObject);
    procedure CheckListBoxFieldsDblClick(Sender: TObject);
    procedure SbEnableAllClick(Sender: TObject);
    procedure SbDisplayFilterStmtClick(Sender: TObject);
    procedure SbOkClick(Sender: TObject);
    procedure EditFilterValue1Click(Sender: TObject);
    procedure EditFilterValue2Click(Sender: TObject);
  private
    PreviousSelectedFieldIndex: Integer;
    procedure ClearFilterDisplay;
    procedure DisplayFilter(Index: Integer);
    procedure GetFilterValues(ComboBox: TComboBox; Edit: TEdit);
    procedure RefreshCheckListBoxFields;
    procedure StoreFilter(Index: Integer);
  protected
    FFilterText: String;
    function GetFilterText: String; virtual;
  public
    MyDb: TSqlitePassDatabase;
    MyDataset: TSqlitePassDataset;
    MyFilters: TSqlitePassFieldFilters;
    constructor Create(AOwner: TComponent; Dataset: TSqlitePassDataset; Filters: TSqlitePassFieldFilters); reintroduce;
    Destructor Destroy; override;
    Property FilterText: String Read FFilterText;
  end;

var
  SqlitePassFilterDialogTemplate: TSqlitePassFilterDialogTemplate;

implementation

uses SqlitePassFilterValues;

{$IFNDEF FPC}
  {$R *.DFM}
{$ENDIF}


constructor TSqlitePassFilterDialogTemplate.Create(AOwner: TComponent; Dataset: TSqlitePassDataset; Filters: TSqlitePassFieldFilters);
var
i: Integer;
begin
  inherited Create(AOwner);
  if Dataset = nil then Exit;
  MyDataset := Dataset;
  MyFilters := TSqlitePassFieldFilters.Create(MyDataset, nil);
  MyFilters.Assign(Filters);
  RefreshCheckListBoxFields;
  if CheckListBoxFields.Items.Count > 0 then
     begin
     for i := 0 to Pred(CheckListBoxFields.Items.Count) do
         if CheckListBoxFields.Checked[i] then
            begin
            CheckListBoxFields.ItemIndex := i;
            Break;
            end;
     PreviousSelectedFieldIndex := -1;
     CheckListBoxFields.ItemIndex := 0;
     CheckListBoxFieldsClick(Self);
     end;
end;

Destructor TSqlitePassFilterDialogTemplate.Destroy;
begin
inherited Destroy;
MyDataset := nil;
MyFilters.Free;
end;

{ Retrieve data on fields and fill the listbox with it }
procedure TSqlitePassFilterDialogTemplate.RefreshCheckListBoxFields;
var
i: integer;
Field: TField;

begin
{ We fill the listboxes with fields names }
CheckListBoxFields.Clear;
CheckListBoxFields.Sorted := False;

For i := 0 to Pred(MyDataset.Fields.count) do
  begin
  { Check Listbox }
  Field := MyDataset.Fields[i];
  CheckListBoxFields.Items.Add(Field.FieldName);
  CheckListBoxFields.Checked[i] := MyFilters.FilterByField(Field).Filtered;
  end;

CheckListBoxFields.Sorted := True;
end;


procedure TSqlitePassFilterDialogTemplate.SbCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

{ Clears ALL the filters }
procedure TSqlitePassFilterDialogTemplate.SbClearAllClick(Sender: TObject);
begin
SbDisableAllClick(Self);
MyFilters.ClearFilters;
SbClearFilterClick(Sender);
end;

{ Clears the currently selected filter }
procedure TSqlitePassFilterDialogTemplate.ClearFilterDisplay;
begin
 ComboBoxFilterOperator1.ItemIndex := 0;
 EditFilterValue1.Text := '';
 ComboBoxFilterOperator2.ItemIndex := 0;
 EditFilterValue2.Text := '';
end;

{ Clears the currently selected filter }
procedure TSqlitePassFilterDialogTemplate.SbClearFilterClick(Sender: TObject);
begin
 CheckListBoxFields.Checked[CheckListBoxFields.ItemIndex] := False;
 ClearFilterDisplay;
end;

{ Displays the Filter content for a given FieldDef (Index = CheckListBox.ItemIndex) }
procedure TSqlitePassFilterDialogTemplate.DisplayFilter(Index: Integer);
var
MyFilter: TSqlitePassFieldFilter;
FilterExpIndex: Integer;

   procedure ConcatFilters(ComboBoxFilterOperator: TComboBox; EditFilterValue: TEdit);
   var
   CmpOp: TSqlitePassFilterComparisonOperators;
   Text: String;
   begin
   Text := '';
   CmpOp := MyFilter[FilterExpIndex].ComparisonOperator;
   ComboBoxFilterOperator.ItemIndex := Ord(CmpOp);
   if CmpOp in [cmpEqual, cmpNotEqual]
      then Repeat
           if Text = ''
              then Text := MyFilter[FilterExpIndex].Value
              else Text := Text + ', ' + MyFilter[FilterExpIndex].Value;
           Inc(FilterExpIndex);
           Until    (FilterExpIndex >= MyFilter.Count)
                or (CmpOp <> MyFilter[FilterExpIndex].ComparisonOperator)
                or (MyFilter[FilterExpIndex].LogicalOperator <> opOr)
      else begin
           Text := MyFilter[FilterExpIndex].Value;
           Inc(FilterExpIndex);
           end;
   EditFilterValue.Text := Text;
   end;

begin
  ClearFilterDisplay;
  MyFilter := MyFilters.FilterByFieldName(CheckListBoxFields.Items[Index]);
  if Not Assigned(MyFilter) or (MyFilter.Count = 0) then Exit;
  FilterExpIndex := 0;
  ConcatFilters(ComboBoxFilterOperator1, EditFilterValue1);
  if FilterExpIndex < MyFilter.Count then
     begin
     RbAnd.Checked := MyFilter[FilterExpIndex].LogicalOperator = opAnd;
     RbOr.Checked := MyFilter[FilterExpIndex].LogicalOperator = opOr;
     ConcatFilters(ComboBoxFilterOperator2, EditFilterValue2);
     end;
end;

Procedure TSqlitePassFilterDialogTemplate.StoreFilter(Index: Integer);
var
MyFilter: TSqlitePassFieldFilter;
LogOperator: TSqlitePassFilterLogicalOperators;
CompOperator: TSqlitePassFilterComparisonOperators;
begin
MyFilter := MyFilters.FilterByFieldName(CheckListBoxFields.Items[Index]);

if Not Assigned(MyFilter) then Exit;

MyFilter.ClearFilter;
{ First Filter Expression }
if ComboBoxFilterOperator1.ItemIndex > 0
   then CompOperator := TSqlitePassFilterComparisonOperators(ComboBoxFilterOperator1.ItemIndex)
   else CompOperator := CmpUnknown;

MyFilter.AddFilterExpression(opNone, CompOperator, EditFilterValue1.Text);

{ Second Filter Expression }
if ComboBoxFilterOperator2.ItemIndex > 0
   then CompOperator := TSqlitePassFilterComparisonOperators(ComboBoxFilterOperator2.ItemIndex)
   else CompOperator := CmpUnknown;

if RbAnd.Checked
   then LogOperator := opAnd
   else LogOperator := opOr;
MyFilter.AddFilterExpression(LogOperator, CompOperator, EditFilterValue2.Text);

end;

function TSqlitePassFilterDialogTemplate.GetFilterText: String;
begin
{ Validate the current filter }
CheckListBoxFieldsClick(nil);
Result := MyFilters.GetFilterText;
end;

procedure TSqlitePassFilterDialogTemplate.CheckListBoxFieldsClick(Sender: TObject);
var
Field: TField;
begin
{ Saves the filters settings for the previous selected field }
if PreviousSelectedFieldIndex > -1
   then StoreFilter(PreviousSelectedFieldIndex);

{ Show the filters of the new selected field }
if PreviousSelectedFieldIndex <> CheckListBoxFields.ItemIndex then
   begin
   PreviousSelectedFieldIndex := CheckListBoxFields.ItemIndex;
   DisplayFilter(PreviousSelectedFieldIndex);
   Field := MyDataset.Fields.FieldByName(CheckListBoxFields.Items[PreviousSelectedFieldIndex]);
   Case Field.Datatype of
       ftString,
       ftWideString : LabelDataFormat.Caption := 'Data format : You can use * or % as WildCard chars';
       ftDateTime : LabelDataFormat.Caption := 'Data format : #' + MyDataset.Database.DatatypeOptions.DateTimeFormat + '#';
       ftDate : LabelDataFormat.Caption := 'Data format : #' + MyDataset.Database.DatatypeOptions.DateFormat + '#';
       ftTime : LabelDataFormat.Caption := 'Data format : #' + MyDataset.Database.DatatypeOptions.TimeFormat + '#';
       else     LabelDataFormat.Caption := '';
       end;
   end;
end;

procedure TSqlitePassFilterDialogTemplate.SbDisableAllClick(Sender: TObject);
var
i: Integer;
begin
 For i := 0 to Pred(CheckListBoxFields.Items.Count)
     do CheckListBoxFields.Checked[i] := False;
end;

procedure TSqlitePassFilterDialogTemplate.SbEnableAllClick(Sender: TObject);
var
i: Integer;
begin
 { Validate the current filter }
 CheckListBoxFieldsClick(nil);
 For i := 0 to Pred(CheckListBoxFields.Items.Count)
   do CheckListBoxFields.Checked[i] := MyFilters.FilterByFieldName(CheckListBoxFields.Items[i]).HasFilterExpression;
end;

procedure TSqlitePassFilterDialogTemplate.CheckListBoxFieldsDblClick(
  Sender: TObject);
begin
 CheckListBoxFields.Checked[CheckListBoxFields.ItemIndex] := Not CheckListBoxFields.Checked[CheckListBoxFields.ItemIndex];
end;

{ Gets one or more filter value from Value list Dialog }

procedure TSqlitePassFilterDialogTemplate.ComboBoxFilterOperator1Change(Sender: TObject);
begin
 SbSelectValueByList1.Enabled := (ComboBoxFilterOperator1.Text = '=')
                              or (ComboBoxFilterOperator1.Text = '<>');
 CheckListBoxFields.Checked[CheckListBoxFields.ItemIndex] := (ComboBoxFilterOperator1.Text <> '');
end;

procedure TSqlitePassFilterDialogTemplate.ComboBoxFilterOperator2Change(Sender: TObject);
begin
 if Not (RbAnd.Checked or RbOr.Checked)
    then if ComboBoxFilterOperator1.Text = '='
            then RbOr.Checked := True
            else RbAnd.Checked := True;
 SbSelectValueByList2.Enabled := (ComboBoxFilterOperator2.Text = '=')
                              or (ComboBoxFilterOperator1.Text = '<>');
end;

procedure TSqlitePassFilterDialogTemplate.SbSelectValueByList1Click(Sender: TObject);
begin
 GetFilterValues(ComboBoxFilterOperator1, EditFilterValue1);
end;

procedure TSqlitePassFilterDialogTemplate.SbSelectValueByList2Click(
  Sender: TObject);
begin
 GetFilterValues(ComboBoxFilterOperator2, EditFilterValue2);
end;

procedure TSqlitePassFilterDialogTemplate.GetFilterValues(ComboBox: TComboBox; Edit: TEdit);
var
ValueDlg: TSqlitePassFilterValuesDlg;
CurrentField: TField;
CurrentFilter: String;
begin
if CheckListBoxFields.ItemIndex > -1 then
   begin
   CurrentField := MyDataset.FindField(CheckListBoxFields.Items[CheckListBoxFields.ItemIndex]);
   if Edit.Text <> ''
      then CurrentFilter := '"' + CurrentField.FieldName + '" ' + ComboBox.Text + ' ' + Edit.Text + ';'
      else CurrentFilter := '';
   ValueDlg := TSqlitePassFilterValuesDlg.Create(Self, MyDataset, CurrentField, CurrentFilter);
   if ValueDlg.ShowModal = mrOk
      then Edit.Text := ValueDlg.FilterText;
   ValueDlg.Free;
   end;
end;

procedure TSqlitePassFilterDialogTemplate.SbDisplayFilterStmtClick(
  Sender: TObject);
var
Text: String;
begin
  Text := GetFilterText;
  if Text = '' then Text := 'No filter defined...';
  ShowMessage(Text);
end;


procedure TSqlitePassFilterDialogTemplate.SbOkClick(Sender: TObject);
begin
 FFilterText := GetFilterText;
 ModalResult := mrOk;
end;

procedure TSqlitePassFilterDialogTemplate.EditFilterValue1Click(
  Sender: TObject);
begin
  ComboBoxFilterOperator1Change(Self);
end;

procedure TSqlitePassFilterDialogTemplate.EditFilterValue2Click(
  Sender: TObject);
begin
  ComboBoxFilterOperator2Change(Self);
end;

initialization
 {$IFDEF FPC}
  {$I SqlitePassFiltersDialogTemplate.lrs}
 {$ENDIF}
end.
