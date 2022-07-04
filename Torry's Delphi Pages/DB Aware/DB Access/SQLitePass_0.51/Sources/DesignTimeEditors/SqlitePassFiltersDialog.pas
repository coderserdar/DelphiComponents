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
  
unit SqlitePassFiltersDialog;
{$i ..\..\Sources\SqlitePassDbo.inc}

interface

uses
 {$IFDEF FPC}
  LResources,
 {$ELSE}
  Windows, Messages, CheckLst,
 {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, SqlitePassDbo,
  Buttons, SqlitePassFiltersDialogTemplate;

type
  TSqlitePassFilterDialog = class(TSqlitePassFilterDialogTemplate)
    tsSQLFilters: TTabSheet;
    Bevel2: TBevel;
    Label1: TLabel;
    SbClearSQLFilter: TSpeedButton;
    Panel2: TPanel;
    Splitter2: TSplitter;
    Panel3: TPanel;
    ListBoxFields: TListBox;
    Panel9: TPanel;
    Image3: TImage;
    Panel10: TPanel;
    CheckBoxUseQuotes: TCheckBox;
    Panel11: TPanel;
    MemoSQLFilterStmt: TMemo;
    Panel12: TPanel;
    Label2: TLabel;
    Image6: TImage;
    tsRangeFilters: TTabSheet;
    Bevel4: TBevel;
    Label3: TLabel;
    LabelLowerLimit: TLabel;
    Label4: TLabel;
    SbClearFilterRanges: TSpeedButton;
    Bevel5: TBevel;
    EditLowerLimit: TEdit;
    EditUpperLimit: TEdit;
    Memo2: TMemo;
    Memo3: TMemo;
    procedure SbOkClick(Sender: TObject);
    procedure SbClearFilterRangesClick(Sender: TObject);
    procedure ListBoxFieldsDblClick(Sender: TObject);
    procedure SbClearSQLFilterClick(Sender: TObject);
  private
     procedure RefreshFieldsList;
     procedure DisplayTabSheets(Visible: Boolean);
  protected
     function GetFilterText: String; override;
  public
    constructor Create(AOwner: TComponent; Dataset: TSqlitePassDataset); reintroduce;
  end;

var
  SqlitePassFilterDialog: TSqlitePassFilterDialog;

implementation

{$IFNDEF FPC}
  {$R *.DFM}
{$ENDIF}

constructor TSqlitePassFilterDialog.Create(AOwner: TComponent;
  Dataset: TSqlitePassDataset);
begin
  inherited Create(AOwner, Dataset, Dataset.Filters);
  Caption := 'Filter Editor for ' + MyDataset.DatasetName;
  tsRangeFilters.TabVisible := True;

  Case MyDataset.FilterMode of
    fmDirect    : begin
                  tsCustomFilters.TabVisible := True;
                  tsSQLFilters.TabVisible := False;
                  PageControl.ActivePage := tsCustomFilters;
                  end;

    fmSQL       : begin
                  tsCustomFilters.TabVisible := False;
                  tsSQLFilters.TabVisible := True;
                  PageControl.ActivePage := tsSQLFilters;
                  end;

    fmSQLDirect : begin
                  tsCustomFilters.TabVisible := True;
                  tsSQLFilters.TabVisible := True;
                  PageControl.ActivePage := tsCustomFilters;
                  end;

    end;
  RefreshFieldsList;
end;

procedure TSqlitePassFilterDialog.RefreshFieldsList;
var
i: Integer;

begin
  inherited;
  { Listbox }
  ListBoxFields.Clear;

  For i := 0 to Pred(MyDataset.Fields.count) do
    begin
    ListBoxFields.items.Add(MyDataset.Fields[i].FieldName);
    end;

  { Ranges }
  EditLowerLimit.Text := IntToStr(MyDataset.FilterRecordLowerLimit);
  EditUpperLimit.Text := IntToStr(MyDataset.FilterRecordUpperLimit);
end;

function TSqlitePassFilterDialog.GetFilterText: String;
begin
 Case MyDataset.FilterMode of
  fmDirect    : Result := inherited GetFilterText;
  fmSQL       : Result := MemoSQLFilterStmt.Text;
  fmSQLDirect : Result := MemoSQLFilterStmt.Text + ';' + inherited GetFilterText;
  end;
  if Result = ';' then Result := '';
end;

{ Add the filters defined to the Dataset.Filters }
procedure TSqlitePassFilterDialog.SbOkClick(Sender: TObject);
var
i: Integer;
begin
FFilterText := GetFilterText;
With MyDataset do
     begin
     Filters.BeginUpdate;
     Filter := FFilterText;
     FilterRecordLowerLimit := StrToIntDef(EditLowerLimit.Text,0);
     FilterRecordUpperLimit := StrToIntDef(EditUpperLimit.Text,0);

     Case FilterMode of
      fmDirect    : Filtered := Filters.Filtered;
      fmSQL       : Filtered := MemoSQLFilterStmt.Text <> '';
      fmSQLDirect : Filtered := Filters.Filtered or (MemoSQLFilterStmt.Text <> '');
      end;

     { Filters are always enabled when FilterText is set.
       We have to disable them, if necessary }
     if FilterMode in [fmDirect, fmSQLDirect] then
        begin
        Filters.DisableFilters;
        for i := 0  to Pred(CheckListBoxFields.Items.Count)
          do MyDataset.Filters.FilterByFieldName(CheckListBoxFields.Items[i]).Filtered := CheckListBoxFields.Checked[i];
        end;

     Filters.EndUpdate;
     end;
ModalResult := mrOk;
end;


procedure TSqlitePassFilterDialog.DisplayTabSheets(Visible: Boolean);
begin
  inherited;
  tsSQLFilters.TabVisible := Visible;
  tsRangeFilters.TabVisible := Visible;
end;

procedure TSqlitePassFilterDialog.SbClearFilterRangesClick(
  Sender: TObject);
begin
EditLowerLimit.Text := '0';
EditUpperLimit.Text := '0';
end;

procedure TSqlitePassFilterDialog.ListBoxFieldsDblClick(Sender: TObject);
var
SelectedField: String;
begin
SelectedField := ListBoxFields.Items[ListBoxFields.ItemIndex];
if CheckBoxUseQuotes.Checked
   then MyDataset.SQLSelectStmt.QuoteString(SelectedField);

MemoSQLFilterStmt.SetFocus;
MemoSQLFilterStmt.SelText := SelectedField;
end;

procedure TSqlitePassFilterDialog.SbClearSQLFilterClick(Sender: TObject);
begin
  MemoSQLFilterStmt.Clear;
end;



initialization

{$IFDEF FPC}
 {$I SqlitePassFiltersDialog.lrs}
{$ENDIF}
end.
