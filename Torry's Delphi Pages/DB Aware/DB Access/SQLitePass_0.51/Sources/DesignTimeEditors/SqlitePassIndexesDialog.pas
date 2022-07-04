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
    2007
    Last update : 28/02/2007

  --------------------------------------------------------------------------- }

unit SqlitePassIndexesDialog;
{$i ..\..\Sources\SqlitePassDbo.inc}

interface

uses
 {$IFDEF FPC}
  LResources,
 {$ELSE}
  Windows, Messages,
 {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, db, Math,
  SqlitePassDbo;

type
  TSqlitePassIndexDefsDialog = class(TForm)
    PanelIndexesToolBar: TPanel;
    PanelIndexMain: TPanel;
    Splitter1: TSplitter;
    PanelAvailableIndexes: TPanel;
    ListBoxIndexDefs: TListBox;
    PanelIndexRight: TPanel;
    MemoSQLIndexCreateStmt: TMemo;
    ScrollBoxIndexedFields: TScrollBox;
    PanelFieldsUsedByIndex: TPanel;
    LabelIndexFieldsUsed: TLabel;
    Image5: TImage;
    ImageIndexDirectionDesc: TImage;
    ImageIndexDirectionAsc: TImage;
    ImageIndexDirectionUnknown: TImage;
    Image7: TImage;
    CheckBoxUniqueIndex: TCheckBox;
    PanelIndexCreateStmt: TPanel;
    Label21: TLabel;
    Image4: TImage;
    PanelIndexApplyChanges: TPanel;
    SbApplyIndexChanges: TSpeedButton;
    SbCancelIndexChanges: TSpeedButton;
    PanelCaptionAvailableIndexes: TPanel;
    SbNew: TSpeedButton;
    SbRefresh: TSpeedButton;
    SbRename: TSpeedButton;
    SbDelete: TSpeedButton;
    SbClose: TSpeedButton;
    procedure ListBoxIndexDefsClick(Sender: TObject);
    procedure SbCloseClick(Sender: TObject);
    procedure SbRefreshClick(Sender: TObject);
    procedure SbRenameClick(Sender: TObject);
    procedure SbDeleteClick(Sender: TObject);
    procedure SbNewClick(Sender: TObject);
    procedure SbApplyIndexChangesClick(Sender: TObject);
    procedure SbCancelIndexChangesClick(Sender: TObject);
    procedure CheckBoxUniqueIndexClick(Sender: TObject);
  private
    CbIndexedFields: TList;
    ImIndexDirection: TList;
    MyDb: TSqlitePassDatabase;
    FDataset: TSqlitePassDataset;
    procedure ClearIndexInfos;
    procedure RefreshIndexList(CurrentIndex: Integer=0);
    procedure RefreshIndexInfos(CurrentIndex: TSqlitePassIndex);
    procedure RefreshIndexSpeedButtons(Enable: Boolean);
    procedure OnCbIndexedFieldClick(Sender: TObject);
    procedure OnImIndexDirectionClick(Sender: TObject);
    procedure SetFDataset(const Value: TSqlitePassDataset);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Property Dataset: TSqlitePassDataset Read FDataset Write SetFDataset;
  end;

var
  SqlitePassIndexDefsDialog: TSqlitePassIndexDefsDialog;

implementation

uses SqlitePassRenameItem, SqlitePassCreateNewIndex, SqlitePassDesignErrorLang;

{$IFNDEF FPC}
  {$R *.DFM}
{$ENDIF}

constructor TSqlitePassIndexDefsDialog.Create(AOwner: TComponent);
begin
 Inherited Create(AOwner);
 CbIndexedFields:= TList.Create;
 ImIndexDirection:= TList.Create;
end;

destructor TSqlitePassIndexDefsDialog.Destroy;
begin
ClearIndexInfos;
CbIndexedFields.Free;
ImIndexDirection.Free;
Inherited Destroy;
end;

procedure TSqlitePassIndexDefsDialog.ClearIndexInfos;
var
i: integer;
begin
ListBoxIndexDefs.Clear;

{ Free Existing checkboxes and Images }
for i:= 0 to Pred(CbIndexedFields.Count)
    do begin
       TCheckBox(CbIndexedFields[i]).Free;
       TImage(ImIndexDirection[i]).Free;
       end;
CbIndexedFields.Clear;
ImIndexDirection.Clear;
end;

procedure TSqlitePassIndexDefsDialog.OnCbIndexedFieldClick(Sender: TObject);
var
i : Integer;
begin
  i := CbIndexedFields.IndexOf(Sender);
  if i = -1 then Exit;
  if TCheckBox(CbIndexedFields[i]).Checked
     then begin
          with TImage(ImIndexDirection[i]) do
          begin
          Picture.Assign(ImageIndexDirectionDesc.Picture);
          Tag := 1;
          Visible := True;
          end;
          end
     else TImage(ImIndexDirection[i]).Visible := False;
RefreshIndexSpeedButtons(True);
end;

procedure TSqlitePassIndexDefsDialog.OnImIndexDirectionClick(Sender: TObject);
begin
With TImage(Sender) do
 begin
 if Tag = 1
    then Tag := 2
    else Tag := 1;

 if Tag = 1
    then Picture.Assign(ImageIndexDirectionAsc.Picture)
    else Picture.Assign(ImageIndexDirectionDesc.Picture);
 end;
RefreshIndexSpeedButtons(True);
end;

procedure TSqlitePassIndexDefsDialog.RefreshIndexInfos(CurrentIndex: TSqlitePassIndex);
var
i: Integer;
MyIndexCol: TSqlitePassIndexColumn;
Image: TImage;
begin
If CurrentIndex = nil then exit;
{ Show SQL Create statement for the selected index }
MemoSQLIndexCreateStmt.Clear;
MemoSqlIndexCreateStmt.Lines.Text := CurrentIndex.Sql;
{ Show fields used by the selected index and sort order }
For i := 0 to Pred(FDataset.FieldCount) do
    begin
    if (FDataset.Fields[i].FieldKind=fkData) and (FDataset.Fields[i].Visible)
    then begin
         MyIndexCol := CurrentIndex.Columns.FindColumn(FDataset.Fields[i].FieldName);
         TCheckBox(CbIndexedFields[i]).Checked := (MyIndexCol <> nil);
         Image := TImage(ImIndexDirection[i]);
         if MyIndexCol <> nil
            then begin
                 Case MyIndexCol.Direction of
                 cidAscending  : begin
                                 Tag := 1;
                                 Image.Picture.Assign(ImageIndexDirectionAsc.Picture);
                                 end;
                 cidDescending : begin
                                 Tag := 2;
                                 Image.Picture.Assign(ImageIndexDirectionDesc.Picture);
                                 end;
                 cidUnknown    : begin
                                 Tag := 3;
                                 Image.Picture.Assign(ImageIndexDirectionUnknown.Picture);
                                 end;
                 end; { Case }
                 Image.Visible := True;
                 end
                 else Image.Visible := False; { if MyIndexCol = nil }
        end; { FDataset.Fields[i]... }
    end; { For... }
CheckBoxUniqueIndex.Checked := CurrentIndex.Unique;
RefreshIndexSpeedButtons(False);
end;

{ Retrieve data on indexes and fill the listbox with it }
procedure TSqlitePassIndexDefsDialog.RefreshIndexList(CurrentIndex: Integer=0);
var
i, VOffset: Integer;
aCheckBox: TCheckBox;
aImage: TImage;

begin
ClearIndexInfos;
FDataset.Database.IndexDefs.Refresh;

{ no index available ? }
if FDataset.IndexDefs.count = 0 then exit;

{ We fill the listbox with indexes names }
For i := 0 to Pred(FDataset.IndexDefs.count) do
ListBoxIndexDefs.items.Add(FDataset.IndexDefs[i].IndexName);

{ Create necessary checkboxes and Images to display info }
VOffset := 0;
for i := 0 to Pred(FDataset.FieldCount) do
    Begin
     if (FDataset.Fields[i].FieldKind=fkData) and (FDataset.Fields[i].Visible)
        then begin
        aCheckBox := TCheckBox.Create(ScrollBoxIndexedFields);
        With aCheckBox do
             begin
             Parent := ScrollBoxIndexedFields;
             Caption:=FDataset.Fields[i].FieldName;
             Top  := (VOffset*20)+5;
             Left := 47;
             Width := 200;
             Height := 16;
             OnClick:=OnCbIndexedFieldClick;
             end;

        aImage := TImage.Create(ScrollBoxIndexedFields);
        With aImage do
             begin
             Parent := ScrollBoxIndexedFields;
             Top  := (VOffset*20)+4;
             Left := 16;
             Width := 16;
             Height := 16;
             Transparent := True;
             OnClick:=OnImIndexDirectionClick;
             end;
        inc(VOffset);
        CbIndexedFields.Add(aCheckBox);
        ImIndexDirection.Add(aImage);
       end;
    end;

{ Select the current index in the listbox }
CurrentIndex := Min(Max(0, CurrentIndex),Pred(ListBoxIndexDefs.Items.Count));
ListBoxIndexDefs.ItemIndex := CurrentIndex;

{ Display corresponding info }
ListBoxIndexDefsClick(Self);
end;

procedure TSqlitePassIndexDefsDialog.RefreshIndexSpeedButtons(Enable: Boolean);
begin
SbApplyIndexChanges.Enabled := Enable;
SbCancelIndexChanges.Enabled := Enable;
end;

procedure TSqlitePassIndexDefsDialog.ListBoxIndexDefsClick(Sender: TObject);
begin
RefreshIndexInfos(MyDb.IndexDefs.FindIndex(ListBoxIndexDefs.Items[ListBoxIndexDefs.ItemIndex]));
end;

procedure TSqlitePassIndexDefsDialog.SbCloseClick(Sender: TObject);
begin
ModalResult := mrOk;
end;

procedure TSqlitePassIndexDefsDialog.SbRefreshClick(Sender: TObject);
begin
RefreshIndexList(ListBoxIndexDefs.ItemIndex);
end;

procedure TSqlitePassIndexDefsDialog.SbRenameClick(Sender: TObject);
begin
FormRenameItem := TSqlitePassFormRenameItem.Create(Self);
FormRenameItem.LabelRenameItem.Caption := 'Rename index '
                               + ListBoxIndexDefs.Items[ListBoxIndexDefs.ItemIndex] + ' as :';
FormRenameItem.EditNewName.Text := ListBoxIndexDefs.Items[ListBoxIndexDefs.ItemIndex];
FormRenameItem.EditNewName.SelectAll;
if FormRenameItem.ShowModal = mrOk
   then begin
   FDataset.Database.IndexDefs.RenameIndex(ListBoxIndexDefs.Items[ListBoxIndexDefs.ItemIndex], FormRenameItem.EditNewName.Text);
   RefreshIndexList(ListBoxIndexDefs.ItemIndex);
   end;
FormRenameItem.Free;
end;

procedure TSqlitePassIndexDefsDialog.SbDeleteClick(Sender: TObject);
begin
FDataset.Database.IndexDefs.DeleteIndex(ListBoxIndexDefs.Items[ListBoxIndexDefs.ItemIndex]);
RefreshIndexList(Pred(ListBoxIndexDefs.ItemIndex));
end;

procedure TSqlitePassIndexDefsDialog.SbNewClick(Sender: TObject);
begin
FormCreateNewIndex := TSqlitePassFormCreateNewIndex.Create(Self, FDataset);
FormCreateNewIndex.EditIndexName.Text := FDataset.DatasetName + '_Index'
                                       + IntToStr(ListBoxIndexDefs.Items.Count+1);
FormCreateNewIndex.EditIndexName.SelectAll;

if FormCreateNewIndex.ShowModal = mrOk
   then RefreshIndexList(Pred(ListBoxIndexDefs.Items.Count));
FormCreateNewIndex.Free;
end;

procedure TSqlitePassIndexDefsDialog.SbApplyIndexChangesClick(
  Sender: TObject);
var
i: integer;
FieldSelected: Boolean;
MyIndexDef: TSqlitePassDatasetIndex;
MyIndexColumn: TSqlitePassIndexColumn;

begin

{ Delete the previous existing index }
FDataset.Database.IndexDefs.DeleteIndex(ListBoxIndexDefs.Items[ListBoxIndexDefs.ItemIndex]);

{ Recreate the new index }
FieldSelected := False;
for i := 0 to Pred(FDataset.FieldCount) do
begin
if TCheckBox(CbIndexedFields[i]).Checked
   then begin
   FieldSelected := True;
   break;
   end;
end;

If not FieldSelected
   then begin
   MessageDlg('No field selected for this index', mtinformation, [mbOk], 0);
   ModalResult := mrNone;
   Abort;
   end;

Try
MyIndexDef := TSQLitePassDatasetIndex.Create(FDataset.IndexDefs);
MyIndexDef.TableName := FDataset.DatasetName;
MyIndexDef.IndexName := ListBoxIndexDefs.Items[ListBoxIndexDefs.ItemIndex];
MyIndexDef.Unique := CheckBoxUniqueIndex.Checked;

for i := 0 to Pred(FDataset.FieldCount) do
begin
if TCheckBox(CbIndexedFields[i]).Checked
   then begin
   MyIndexColumn := TSqlitePassIndexColumn.Create;
   MyIndexColumn.ColumnName := TCheckBox(CbIndexedFields[i]).Caption;
   if TImage(ImIndexDirection[i]).Tag = 1
      then MyIndexColumn.Direction := cidAscending
      else MyIndexColumn.Direction := cidDescending;
   MyIndexDef.Columns.Add(MyIndexColumn);
   end;
end;
FDataset.Database.IndexDefs.CreateIndex(MyIndexDef);
except
raise;
end;
RefreshIndexList(ListBoxIndexDefs.ItemIndex);
RefreshIndexSpeedButtons(False);
end;

procedure TSqlitePassIndexDefsDialog.SbCancelIndexChangesClick(
  Sender: TObject);
begin
RefreshIndexList;
RefreshIndexSpeedButtons(False);
end;

procedure TSqlitePassIndexDefsDialog.CheckBoxUniqueIndexClick(
  Sender: TObject);
begin
RefreshIndexSpeedButtons(True);
end;

procedure TSqlitePassIndexDefsDialog.SetFDataset(const Value: TSqlitePassDataset);
begin
  FDataset := Value;
  ListBoxIndexDefs.Clear;
  if (FDataset = nil) or (FDataset.Database = nil)
     then begin
          DatabaseError(Msg1017, FDataset);
          end
     else begin
          Caption := 'Indexes Toolbox for ' + FDataset.DatasetName;
          MyDb := FDataset.Database;
          RefreshIndexList;
          end;
end;

initialization
 {$IFDEF FPC}
  {$I SqlitePassIndexesDialog.lrs}
 {$ENDIF}
end.
