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
    Last update : 02/01/2010

  --------------------------------------------------------------------------- }
unit SqlitePassLocateDialog;

interface

uses
 {$IFDEF FPC}
  LResources,
 {$ELSE}
  Windows, Messages, CheckLst,
 {$ENDIF}  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SqlitePassFiltersDialogTemplate, StdCtrls, ExtCtrls, ComCtrls,
  Buttons, SqlitePassDbo;

type

  { TSqlitePassLocateDlg }

  TSqlitePassLocateDlg = class(TSqlitePassFilterDialogTemplate)
    TabSheet1: TTabSheet;
    Panel8: TPanel;
    Label1: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    TEditKeyValues: TEdit;
    TEditKeyFields: TEdit;
    CbPartialKeys: TCheckBox;
    CbCaseInsensitive: TCheckBox;
    CbUseQuotes: TCheckBox;
    Panel2: TPanel;
    Image2: TImage;
    Label2: TLabel;
    Splitter1: TSplitter;
    Panel4: TPanel;
    ListBoxFields: TListBox;
    Panel5: TPanel;
    Image1: TImage;
    procedure SbOkClick(Sender: TObject);
  private
     procedure RefreshFieldsList;
  public
    constructor Create(AOwner: TComponent; Dataset: TSqlitePassDataset); reintroduce;
  end;

var
  SqlitePassLocateDlg: TSqlitePassLocateDlg;

implementation

{$IFNDEF FPC}
  {$R *.DFM}
{$ENDIF}

{ TSqlitePassLocateDlg }

constructor TSqlitePassLocateDlg.Create(AOwner: TComponent;
  Dataset: TSqlitePassDataset);
begin
  inherited Create(AOwner, Dataset, Dataset.LocateFilters);
  Caption := 'Locate Editor for ' + MyDataset.DatasetName;
  RefreshFieldsList;
end;


procedure TSqlitePassLocateDlg.RefreshFieldsList;
var
i: Integer;

begin
  inherited;
  { Listbox }
  ListBoxFields.Clear;
  For i := 0 to Pred(MyDataset.Fields.count)
    do ListBoxFields.items.Add(MyDataset.Fields[i].FieldName);
end;

procedure TSqlitePassLocateDlg.SbOkClick(Sender: TObject);
var
i: Integer;
begin
Inherited;
With MyDataset do
     begin
     LocateFilters.BeginUpdate;
     LocateFilters.DisableFilters;
     for i := 0  to Pred(CheckListBoxFields.Items.Count)
          do MyDataset.LocateFilters.FilterByFieldName(CheckListBoxFields.Items[i]).Filtered := CheckListBoxFields.Checked[i];
     Filters.EndUpdate;
     end;
     MyDataset.Locate(FilterText, []);
ModalResult := mrOk;
end;
initialization

{$IFDEF FPC}
 {$I SqlitePassLocateDialog.lrs}
{$ENDIF}
end.
