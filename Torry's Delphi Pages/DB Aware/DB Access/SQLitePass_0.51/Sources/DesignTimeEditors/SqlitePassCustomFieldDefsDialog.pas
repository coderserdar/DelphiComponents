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
    Last update : 21/12/2007

  --------------------------------------------------------------------------- }

unit SqlitePassCustomFieldDefsDialog;
{$i ..\..\Sources\SqlitePassDbo.inc}

interface

uses
 {$IFDEF FPC}
  LResources,
  LCLType,
 {$ELSE}
  Windows,
  Messages,
  Math,
 {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, DB, ComCtrls, SqlitePassDbo, SqlitePassUtils,
  SqlitePassVisualTools;

type
  TSqlitePassCustomFieldDefsDlg = class(TForm)
    PanelBottom: TPanel;
    PanelDatatypes: TPanel;
    BtOk: TButton;
    BtnSaveToDatabase: TButton;
    BtnLoadFromDatabase: TButton;
    BtnCancel: TButton;
    BtClearAll: TButton;
    PanelCtrlGridHeader: TPanel;
    Bevel5: TBevel;
    Shape5: TShape;
    Label6: TLabel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    PanelCtrlGrid1: TPanel;
    PanelFieldDef: TPanel;
    CbDataType: TComboBox;
    EditSize: TEdit;
    EditPrecision: TEdit;
    PanelCtrlGridNav: TPanel;
    StaticText4: TStaticText;
    CbTableName: TComboBox;
    CbFieldName: TComboBox;
    procedure BtOkClick(Sender: TObject);
    procedure BtnLoadFromDatabaseClick(Sender: TObject);
    procedure BtnSaveToDatabaseClick(Sender: TObject);
    procedure BtClearAllClick(Sender: TObject);
    procedure CbTableNameChange(Sender: TObject);
    procedure CbTableNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditSizeKeyPress(Sender: TObject; var Key: Char);
    procedure CbTableNameEnter(Sender: TObject);
  private
    Db: TSqlitePassDatabase;
    CtrlGrid: TSPVTCtrlGrid;
    Navigator: TSPVTCtrlGridNavigator;
    procedure OnCtrlGridNewRecord(CtrlGrid: TSPVTCtrlGrid);
    procedure DisplayCustomFieldDefs;
    procedure UpdateFieldNames(CurrentCbFieldName: TComboBox; TableDef: TSqlitePassTableDef);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  SqlitePassCustomFieldDefsDlg: TSqlitePassCustomFieldDefsDlg;

implementation

{$IFNDEF FPC}
 {$R *.DFM}
{$ENDIF}

{ Constructor and Destructor }

constructor TSqlitePassCustomFieldDefsDlg.Create(AOwner: TComponent);
var
i: integer;
ft: TFieldType;
begin
inherited Create(nil);
Db := TSqlitePassDatabase(AOwner);

CbTableName.Clear;
For i := 0 to Pred(Db.TableDefs.Count)
    do CbTableName.Items.Add(Db.TableDefs[i].TableName);

CbDataType.Clear;
For ft := Low(TFieldType) to High(TFieldType)
   do CbDataType.Items.Add(FieldTypeToString(ft));

CtrlGrid := TSPVTCtrlGrid.Create(PanelCtrlGrid1);
With CtrlGrid do
     begin
     Align := AlClient;
     OnNewRecord := OnCtrlGridNewRecord;
     Panel := PanelFieldDef;
     AutoScroll := True;
     end;

Navigator := TSPVTCtrlGridNavigator.Create(PanelCtrlGridNav);
Navigator.SetBounds(1,1,Navigator.Width,Navigator.Height);
Navigator.CtrlGrid := CtrlGrid;

DisplayCustomFieldDefs;
end;

procedure TSqlitePassCustomFieldDefsDlg.DisplayCustomFieldDefs;
var
i: Integer;
begin
Try
  CtrlGrid.Visible := False;
  CtrlGrid.Clear;
  For i := 0 to Pred(Db.DatatypeOptions.CustomFieldDefs.Count) do
      begin
      CtrlGrid.Append;
      TComboBox(CtrlGrid.ControlByName('CbTableName')).ItemIndex := TComboBox(CtrlGrid.ControlByName('CbTableName')).Items.IndexOf(Db.DatatypeOptions.CustomFieldDefs[i].TableName);
      UpdateFieldNames(TComboBox(CtrlGrid.ControlByName('CbFieldName')), Db.TableDefs.FindTable(TComboBox(CtrlGrid.ControlByName('CbTableName')).Text));
      TComboBox(CtrlGrid.ControlByName('CbFieldName')).ItemIndex := TComboBox(CtrlGrid.ControlByName('CbFieldName')).Items.IndexOf(Db.DatatypeOptions.CustomFieldDefs[i].FieldName);
      TComboBox(CtrlGrid.ControlByName('CbDataType')).ItemIndex := TComboBox(CtrlGrid.ControlByName('CbDataType')).Items.IndexOf(FieldTypeToString(Db.DatatypeOptions.CustomFieldDefs[i].FieldType));
      TEdit(CtrlGrid.ControlByName('EditSize')).Text := IntToStr(Db.DatatypeOptions.CustomFieldDefs[i].FieldSize);
      TEdit(CtrlGrid.ControlByName('EditPrecision')).Text := IntToStr(Db.DatatypeOptions.CustomFieldDefs[i].FieldPrecision);
      end;
  { To Set State to cgsBrowse }
  CtrlGrid.Post;
Finally
  CtrlGrid.First;
  CtrlGrid.Visible := True;
end;
end;


{ Events }
procedure TSqlitePassCustomFieldDefsDlg.UpdateFieldNames(CurrentCbFieldName: TComboBox; TableDef: TSqlitePassTableDef);
var
i: integer;
begin
CurrentCbFieldName.Clear;
if Assigned(TableDef)
   then for i := 0 to Pred(TableDef.FieldDefs.Count)
            do CurrentCbFieldName.Items.Add(TableDef.FieldDefs[i].FieldName);
end;

procedure TSqlitePassCustomFieldDefsDlg.OnCtrlGridNewRecord(
  CtrlGrid: TSPVTCtrlGrid);
var
Cb: TComboBox;
begin
Cb := TComboBox(CtrlGrid.ControlByName('CbTableName'));
if Cb.Items.Count > 0
   then Cb.ItemIndex := 0
   else Cb.ItemIndex := -1;
CbTableNameChange(Self);

Cb := TComboBox(CtrlGrid.ControlByName('CbFieldName'));
if Cb.Items.Count > 0
   then Cb.ItemIndex := 0
   else Cb.ItemIndex := -1;

Cb := TComboBox(CtrlGrid.ControlByName('CbDataType'));
if Cb.Items.Count > 0
   then Cb.ItemIndex := 0
   else Cb.ItemIndex := -1;

TEdit(CtrlGrid.ControlByName('EditSize')).Text := '';
TEdit(CtrlGrid.ControlByName('EditPrecision')).Text := '';
end;

procedure TSqlitePassCustomFieldDefsDlg.CbTableNameChange(Sender: TObject);
var
CbTable, CbField: TComboBox;
begin
CbTable := TComboBox(CtrlGrid.ControlByName('CbTableName'));
{ Updates Table Fields List }
CbField := TComboBox(CtrlGrid.ControlByName('CbFieldName'));
UpdateFieldNames(CbField, Db.TableDefs.TableByName(CbTable.Text));
end;


{ Buttons Events }
procedure TSqlitePassCustomFieldDefsDlg.BtOkClick(Sender: TObject);
var
i: integer;
NewCustomFieldDef: TSqlitePassCustomFieldDef;
begin
Try
Db.DatatypeOptions.CustomFieldDefs.ClearAndFreeItems;
For i := 0 to Pred(CtrlGrid.RowCount) do
    begin
    CtrlGrid.ActiveRowIndex := i;
    if (TComboBox(CtrlGrid.ControlByName('CbTableName')).Text <> '') and (TComboBox(CtrlGrid.ControlByName('CbFieldName')).Text <> '') then
       begin
       NewCustomFieldDef := TSqlitePassCustomFieldDef.Create;
       With NewCustomFieldDef do
            begin
            TableName := TComboBox(CtrlGrid.ControlByName('CbTableName')).Text;
            FieldName := TComboBox(CtrlGrid.ControlByName('CbFieldName')).Text;
            FieldType := StringToFieldType(TComboBox(CtrlGrid.ControlByName('CbDataType')).Text);
            FieldSize := StrToIntDef(TEdit(CtrlGrid.ControlByName('EditSize')).Text, 0);
            FieldPrecision := StrToIntDef(TEdit(CtrlGrid.ControlByName('EditPrecision')).Text,0);
            end;
       Db.DatatypeOptions.CustomFieldDefs.Add(NewCustomFieldDef);
       Db.DatatypeOptions.ApplyCustomFieldDefs;
       end;
    end;
Except
ModalResult := mrNone;
end;
end;

procedure TSqlitePassCustomFieldDefsDlg.BtnLoadFromDatabaseClick(
  Sender: TObject);
begin
BtClearAllClick(Sender);
Db.DatatypeOptions.LoadFromDatabase([loCustomFieldDefs]);
DisplayCustomFieldDefs;
end;

procedure TSqlitePassCustomFieldDefsDlg.BtnSaveToDatabaseClick(Sender: TObject);
begin
BtOkClick(Sender);
Db.DatatypeOptions.SaveToDatabase([soCustomFieldDefs]);
end;

procedure TSqlitePassCustomFieldDefsDlg.BtClearAllClick(Sender: TObject);
begin
CtrlGrid.Clear;
Db.DatatypeOptions.CustomFieldDefs.ClearAndFreeItems;
end;

procedure TSqlitePassCustomFieldDefsDlg.CbTableNameKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
{ Classic up/down keys with comboboxes }
 if (Sender is TComboBox) then
    if (Key = VK_DOWN) or (Key = VK_UP) then Exit;
    
 CtrlGrid.KeyDown(Key, Shift);
end;

procedure TSqlitePassCustomFieldDefsDlg.EditSizeKeyPress(Sender: TObject;
  var Key: Char);
begin
 if Not (Key in [#8, '0'..'9']) then Key := #0;
end;

procedure TSqlitePassCustomFieldDefsDlg.CbTableNameEnter(Sender: TObject);
begin
  CtrlGrid.FocusActiveRow(Sender);
end;

initialization
 {$IFDEF FPC}
  {$I SqlitePassCustomFieldDefsDialog.lrs}
 {$ENDIF}
end.
  
