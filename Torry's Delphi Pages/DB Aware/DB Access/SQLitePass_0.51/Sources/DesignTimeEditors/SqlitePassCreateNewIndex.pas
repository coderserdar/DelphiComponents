{   This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation; either version 2.1 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

   ---------------------------------------------------------------------------

    Messages localisation unit
    Author : Luc DAVID Email: luckylazarus@free.fr
    2006-2007

   --------------------------------------------------------------------------- }
   
unit SqlitePassCreateNewIndex;
{$i ..\..\Sources\SqlitePassDbo.inc}

interface

uses
 {$IFDEF FPC}
  Buttons, LResources,
 {$ELSE}
  Windows,
  Messages,
 {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, db, SqlitePassDbo;

type
  TSqlitePassFormCreateNewIndex = class(TForm)
    Panel1: TPanel;
    ComboBoxTableList: TComboBox;
    Image1: TImage;
    EditIndexName: TEdit;
    BtImageIndexNew: TImage;
    ScrollBoxIndexedFields: TScrollBox;
    Panel2: TPanel;
    Image7: TImage;
    CheckBoxUniqueIndex: TCheckBox;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    ImageIndexDirectionDesc: TImage;
    ImageIndexDirectionAsc: TImage;
    procedure ButtonOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    CbIndexedFields: TList;
    ImIndexDirection: TList;
    MyDb: TSqlitePassDatabase;
    MyDataset: TSqlitePassDataset;
    Procedure OnCbIndexedFieldClick(Sender: TObject);
    procedure OnImIndexDirectionClick(Sender: TObject);
  private
    procedure DisplayAvailableFields;
    procedure ClearIndexInfos;
  public
    constructor Create(AOwner: TComponent; Dataset: TSqlitePassDataset); reintroduce;
    destructor Destroy; override;
  end;

var
  FormCreateNewIndex: TSqlitePassFormCreateNewIndex;

implementation
 {$IFNDEF FPC}
   {$R *.DFM}
 {$ENDIF}


{ --- Constructor / Destructor --- }

constructor TSqlitePassFormCreateNewIndex.Create(AOwner: TComponent;
  Dataset: TSqlitePassDataset);
begin
inherited Create(AOwner);
if Dataset = nil
   then abort
   else begin
        MyDataset := Dataset;
        MyDb := MyDataset.Database;
        CbIndexedFields:= TList.Create;
        ImIndexDirection:= TList.Create;
        end;
end;

destructor TSqlitePassFormCreateNewIndex.Destroy;
begin
ClearIndexInfos;
CbIndexedFields.Free;
ImIndexDirection.Free;
Inherited Destroy;
end;

procedure TSqlitePassFormCreateNewIndex.ClearIndexInfos;
var
i: integer;
begin
{ Free Existing checkboxes and Images }
for i:= 0 to Pred(CbIndexedFields.Count)
    do begin
       TCheckBox(CbIndexedFields[i]).Free;
       TImage(ImIndexDirection[i]).Free;
       end;
CbIndexedFields.Clear;
ImIndexDirection.Clear;
end;

{ --- --- }
procedure TSqlitePassFormCreateNewIndex.FormShow(Sender: TObject);
begin
DisplayAvailableFields;
end;

procedure TSqlitePassFormCreateNewIndex.DisplayAvailableFields;
var
i, VOffset: integer;
ACheckBox: TCheckBox;
AImage: TImage;
begin

if not assigned(FormCreateNewIndex) then exit;

For i:= 0 to Pred(MyDb.TableDefs.Count)
    do if Not MyDb.IsSystemTable(MyDb.TableDefs[i].TableName)
       then ComboBoxTableList.Items.Add(MyDb.TableDefs[i].TableName);

ComboBoxTableList.ItemIndex := ComboBoxTableList.Items.IndexOf(MyDataset.DatasetName);
ComboBoxTableList.Enabled := False;

VOffset := 0;
For i := 0 to Pred(MyDataset.FieldCount) do
    Begin
     if (MyDataset.Fields[i].FieldKind=fkData) and (MyDataset.Fields[i].Visible)
        then begin
        ACheckBox := TCheckBox.Create(ScrollBoxIndexedFields);
        With ACheckBox do
             begin
             Parent := ScrollBoxIndexedFields;
             Caption:= MyDataset.Fields[i].FieldName;
             Top  := (VOffset*20)+5;
             Left := 56;
             Width := 200;
             Height := 16;
             OnClick := OnCbIndexedFieldClick;
             end;
        CbIndexedFields.Add(ACheckBox);

        AImage := TImage.Create(ScrollBoxIndexedFields);
        With AImage do
             begin
             Parent := ScrollBoxIndexedFields;
             Top  := (VOffset*20)+4;
             Left := 32;
             Width := 16;
             Height := 16;
             Transparent := True;
             OnClick := OnImIndexDirectionClick;
             end;
        ImIndexDirection.Add(AImage);
        end;
        Inc(VOffset);
    end;
end;

procedure TSqlitePassFormCreateNewIndex.OnCbIndexedFieldClick(Sender: TObject);
var
i : Integer;
begin
  i := CbIndexedFields.IndexOf(Sender);
  if i =-1 then Exit;
  if TCheckBox(CbIndexedFields[i]).Checked
     then begin
          with TImage(ImIndexDirection[i]) do
          begin
          Picture.Assign(ImageIndexDirectionAsc.Picture);
          Tag := 1;
          Visible := True;
          end;
          end
     else TImage(ImIndexDirection[i]).Visible := False;
end;

procedure TSqlitePassFormCreateNewIndex.OnImIndexDirectionClick(Sender: TObject);
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
end;


procedure TSqlitePassFormCreateNewIndex.ButtonOkClick(Sender: TObject);
var
i: integer;
FieldSelected: Boolean;
MyIndexDef: TSqlitePassDatasetIndex;
MyIndexColumn: TSqlitePassIndexColumn;

begin
If EditIndexName.Text = ''
   then begin
   MessageDlg('The index name is missing.', mtinformation, [mbOk], 0);
   EditIndexName.SetFocus;
   ModalResult := mrNone;
   Abort;
   end;

FieldSelected := False;
for i := 0 to Pred(MyDataset.FieldCount) do
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
MyIndexDef := TSQLitePassDatasetIndex.Create(MyDataset.IndexDefs);
MyIndexDef.TableName := ComboBoxTableList.Text;
MyIndexDef.IndexName := EditIndexName.Text;
MyIndexDef.Unique := CheckBoxUniqueIndex.Checked;

for i := 0 to Pred(MyDataset.FieldCount) do
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
MyDataset.Database.IndexDefs.CreateIndex(MyIndexDef);
except
ModalResult := mrNone;
raise;
end;
end;

initialization
 {$IFDEF FPC}
  {$I SqlitePassCreateNewIndex.lrs}
 {$ENDIF}
end.
 
