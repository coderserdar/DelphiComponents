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
    2008-2009
    Last update : 2009-05-17

  --------------------------------------------------------------------------- }

unit SqlitePassCreateTableDialog;
{$i ..\..\Sources\SqlitePassDbo.inc}

interface

uses
 {$IFDEF FPC}
  LResources,
  LCLType,
 {$ELSE}
  Windows,
  Messages,
  Db,
  Math,
 {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, SqlitePassDbo,
  SqlitePassVisualTools;

type

  { TSqlitePassCreateTableDlg }

  TSqlitePassCreateTableDlg = class(TForm)
    PanelBottom: TPanel;
    BtOk: TButton;
    BtCancel: TButton;
    PageControl: TPageControl;
    TsTableDefinition: TTabSheet;
    TsSQL: TTabSheet;
    MemoSQL: TMemo;
    PanelTableFieldDefs: TPanel;
    Shape4: TShape;
    Shape6: TShape;
    Bevel2: TBevel;
    Bevel4: TBevel;
    Shape1: TShape;
    Label2: TLabel;
    Shape2: TShape;
    Label1: TLabel;
    LabelPrimaryKeyConflict: TLabel;
    LabelPrimaryKeySort: TLabel;
    LabelNotNullConflict: TLabel;
    Bevel3: TBevel;
    LabelUniqueConflict: TLabel;
    CbPrimaryKey: TCheckBox;
    CbAutoInc: TCheckBox;
    CbNotNull: TCheckBox;
    CbUnique: TCheckBox;
    MemoFieldDescription: TMemo;
    PanelCtrlGrid: TPanel;
    PanelCtrlGridNav: TPanel;
    PanelCtrlGridHeader: TPanel;
    Bevel5: TBevel;
    Shape5: TShape;
    Label6: TLabel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText6: TStaticText;
    PanelCtrlGrid1: TPanel;
    CbOnPrimaryKeyConflict: TComboBox;
    RbSortNone: TRadioButton;
    RbSortUp: TRadioButton;
    RbSortDown: TRadioButton;
    CbOnNotNullConflict: TComboBox;
    CBOnUniqueConflict: TComboBox;
    PanelFieldDef: TPanel;
    EditFieldName: TEdit;
    CbDataType: TComboBox;
    EditSize: TEdit;
    EditPrecision: TEdit;
    CbDefaultValue: TComboBox;
    PanelCbPrimaryKey: TCheckBox;
    PanelCbAutoInc: TCheckBox;
    PanelCbNotNull: TCheckBox;
    PanelCbUnique: TCheckBox;
    PanelRbSortNone: TRadioButton;
    PanelRbSortUp: TRadioButton;
    PanelRbSortDown: TRadioButton;
    CbCollatingOrder: TComboBox;
    BtSaveToFile: TButton;
    BtLoadFromFile: TButton;
    BtRefreshSQL: TButton;
    ImagePrimaryKey: TImage;
    ImageAutoinc: TImage;
    LabelManualSQL: TLabel;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    StaticText7: TStaticText;
    EditFieldCheckConstraint: TEdit;
    LabelTableName: TLabel;
    Shape3: TShape;
    Label3: TLabel;
    Shape7: TShape;
    CbTemporaryTable: TCheckBox;
    EditTableName: TEdit;
    CbOverwriteExistingTable: TCheckBox;
    Shape8: TShape;
    PanelOnNotNullConflict: TLabel;
    PanelOnPrimaryKeyConflict: TLabel;
    PanelOnUniqueConflict: TLabel;
    PanelFieldDescription: TLabel;
    StaticText5: TStaticText;
    procedure FormShow(Sender: TObject);
    procedure EditFieldNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditSizeKeyPress(Sender: TObject; var Key: Char);
    procedure EditFieldNameEnter(Sender: TObject);
    procedure BtOkClick(Sender: TObject);
    procedure CbPrimaryKeyClick(Sender: TObject);
    procedure TsSQLShow(Sender: TObject);
    procedure CbAutoIncClick(Sender: TObject);
    procedure MemoSQLKeyPress(Sender: TObject; var Key: Char);
    procedure BtLoadFromFileClick(Sender: TObject);
    procedure BtSaveToFileClick(Sender: TObject);
    procedure MemoSQLKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BtRefreshSQLClick(Sender: TObject);
    procedure CbDataTypeExit(Sender: TObject);
    procedure CbNotNullClick(Sender: TObject);
    procedure CbUniqueClick(Sender: TObject);
  private
    Navigator: TSPVTCtrlGridNavigator;
    FDatabase: TSqlitePassDatabase;
    Function CreateSqlStmt: String;
    procedure InsertImage(Image: TImage; LeftPos: Integer);
    procedure OnCtrlGridNewRecord(CtrlGrid: TSPVTCtrlGrid);
    procedure OnCtrlGridBeforeScroll(CtrlGrid: TSPVTCtrlGrid);
    procedure OnCtrlGridAfterScroll(CtrlGrid: TSPVTCtrlGrid);
  public
    SQL: String;
    CtrlGrid: TSPVTCtrlGrid;
    constructor Create(AOwner: TComponent; Database: TSqlitePassDatabase); Reintroduce;
  end;

var
  SqlitePassCreateTableDlg: TSqlitePassCreateTableDlg;

implementation

{$IFNDEF FPC}
 {$R *.DFM}
{$ENDIF}

constructor TSqlitePassCreateTableDlg.Create(AOwner: TComponent; Database: TSqlitePassDatabase);
var
i: integer;
begin
inherited Create(AOwner);
FDatabase := Database;
PageControl.ActivePage := TsTableDefinition;
LabelManualSQL.Visible := False;

CbDataType.Clear;
for i := 0 to Pred(FDatabase.DatatypeOptions.TranslationRules.Count)
   do CbDataType.Items.Add(FDatabase.DatatypeOptions.TranslationRules[i].DatatypeName);

CtrlGrid := TSPVTCtrlGrid.Create(PanelCtrlGrid1);
With CtrlGrid do
     begin
     Align := AlClient;
     OnNewRecord := OnCtrlGridNewRecord;
     BeforeScroll := OnCtrlGridBeforeScroll;
     AfterScroll := OnCtrlGridAfterScroll;
     Panel := PanelFieldDef;
     AutoScroll := True;
     end;

Navigator := TSPVTCtrlGridNavigator.Create(PanelCtrlGridNav);
Navigator.SetBounds(1,1,Navigator.Width,Navigator.Height);
Navigator.CtrlGrid := CtrlGrid;
EditTableName.Text := 'TABLE_'+ IntToStr(FDatabase.TableDefs.Count);
end;

procedure TSqlitePassCreateTableDlg.OnCtrlGridNewRecord(CtrlGrid: TSPVTCtrlGrid);
var
EditCtrl: TEdit;
CbCtrl: TComboBox;
begin
 { EditFieldName }
 EditCtrl := TEdit(CtrlGrid.ControlByName('EditFieldName'));
 EditCtrl.Text := '';
 { CbDataType }
 CbCtrl := TComboBox(CtrlGrid.ControlByName('CbDataType'));
 CbCtrl.Text := '';
 { EditSize }
 EditCtrl := TEdit(CtrlGrid.ControlByName('EditSize'));
 EditCtrl.Text := '';
 { EditPrecision }
 EditCtrl := TEdit(CtrlGrid.ControlByName('EditPrecision'));
 EditCtrl.Text := '';
 { CbDefaultValue }
 CbCtrl := TComboBox(CtrlGrid.ControlByName('CbDefaultValue'));
 CbCtrl.Text := '';
 { EditFieldCheckConstraint }
 EditCtrl := TEdit(CtrlGrid.ControlByName('EditFieldCheckConstraint'));
 EditCtrl.Text := '';
 { CbCollatingOrder }
 CbCtrl := TComboBox(CtrlGrid.ControlByName('CbCollatingOrder'));
 CbCtrl.ItemIndex := 0;
 { On conflict }
 CbOnPrimaryKeyConflict.ItemIndex := 0;
 CbNotNullClick(Self);
 CbOnNotNullConflict.ItemIndex := 0;
 CbUniqueClick(Self);
 CBOnUniqueConflict.ItemIndex := 0;
 { PrimaryKey }
 CbPrimaryKeyClick(Self);
 { Others }
 MemoFieldDescription.Clear;
 TLabel(CtrlGrid.ControlByName('PanelOnPrimaryKeyConflict')).Caption := '';
 TLabel(CtrlGrid.ControlByName('PanelOnNotNullConflict')).Caption := '';
 TLabel(CtrlGrid.ControlByName('PanelOnUniqueConflict')).Caption := '';
 TLabel(CtrlGrid.ControlByName('PanelFieldDescription')).Caption := '';
end;

procedure TSqlitePassCreateTableDlg.FormShow(Sender: TObject);
begin
 if PageControl.ActivePage = TsTableDefinition
    then CtrlGrid.FocusFirstControl;
end;

procedure TSqlitePassCreateTableDlg.EditFieldNameKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
{ Classic up/down keys with comboboxes }
 if (Sender is TComboBox) then
    if (Key = VK_DOWN) or (Key = VK_UP) then Exit;

 CtrlGrid.KeyDown(Key, Shift);
end;

procedure TSqlitePassCreateTableDlg.EditSizeKeyPress(Sender: TObject;
  var Key: Char);
begin
 if Not (Key in [#8, '0'..'9']) then Key := #0;
end;

procedure TSqlitePassCreateTableDlg.EditFieldNameEnter(Sender: TObject);
begin
  CtrlGrid.FocusActiveRow(Sender);
end;

procedure TSqlitePassCreateTableDlg.OnCtrlGridBeforeScroll(
  CtrlGrid: TSPVTCtrlGrid);
begin
if CtrlGrid.IsEmpty then exit;
if CtrlGrid.ActiveRow = nil then exit;
TCheckBox(CtrlGrid.ControlByName('PanelCbPrimaryKey')).Checked := CbPrimaryKey.Checked;
TCheckBox(CtrlGrid.ControlByName('PanelCbAutoInc')).Checked := CbAutoInc.Checked;
TCheckBox(CtrlGrid.ControlByName('PanelCbNotNull')).Checked := CbNotNull.Checked;
TCheckBox(CtrlGrid.ControlByName('PanelCbUnique')).Checked := CbUnique.Checked;
TRadioButton(CtrlGrid.ControlByName('PanelRbSortNone')).Checked := RbSortNone.Checked;
TRadioButton(CtrlGrid.ControlByName('PanelRbSortUp')).Checked := RbSortUp.Checked;
TRadioButton(CtrlGrid.ControlByName('PanelRbSortDown')).Checked := RbSortDown.Checked;
TLabel(CtrlGrid.ControlByName('PanelOnPrimaryKeyConflict')).Caption := CbOnPrimaryKeyConflict.Text;
TLabel(CtrlGrid.ControlByName('PanelOnNotNullConflict')).Caption := CbOnNotNullConflict.Text;
TLabel(CtrlGrid.ControlByName('PanelOnUniqueConflict')).Caption := CbOnUniqueConflict.Text;
TLabel(CtrlGrid.ControlByName('PanelFieldDescription')).Caption := MemoFieldDescription.Text;
end;

procedure TSqlitePassCreateTableDlg.OnCtrlGridAfterScroll(
  CtrlGrid: TSPVTCtrlGrid);
begin
CbPrimaryKey.Checked := TCheckBox(CtrlGrid.ControlByName('PanelCbPrimaryKey')).Checked;
CbAutoInc.Checked := TCheckBox(CtrlGrid.ControlByName('PanelCbAutoInc')).Checked;
CbNotNull.Checked := TCheckBox(CtrlGrid.ControlByName('PanelCbNotNull')).Checked;
CbUnique.Checked := TCheckBox(CtrlGrid.ControlByName('PanelCbUnique')).Checked;
RbSortNone.Checked := TRadioButton(CtrlGrid.ControlByName('PanelRbSortNone')).Checked;
RbSortUp.Checked := TRadioButton(CtrlGrid.ControlByName('PanelRbSortUp')).Checked;
RbSortDown.Checked := TRadioButton(CtrlGrid.ControlByName('PanelRbSortDown')).Checked;
CbOnPrimaryKeyConflict.ItemIndex := CbOnPrimaryKeyConflict.Items.IndexOf(TLabel(CtrlGrid.ControlByName('PanelOnPrimaryKeyConflict')).Caption);
CbOnNotNullConflict.ItemIndex := CbOnNotNullConflict.Items.IndexOf(TLabel(CtrlGrid.ControlByName('PanelOnNotNullConflict')).Caption);
CbOnUniqueConflict.ItemIndex := CbOnUniqueConflict.Items.IndexOf(TLabel(CtrlGrid.ControlByName('PanelOnUniqueConflict')).Caption);
MemoFieldDescription.Text := TLabel(CtrlGrid.ControlByName('PanelFieldDescription')).Caption;
end;

procedure TSqlitePassCreateTableDlg.BtOkClick(Sender: TObject);
begin
If PageControl.ActivePage = TsTableDefinition
   then SQL := CreateSqlStmt
   else SQL := MemoSQL.Lines.Text;
end;

procedure TSqlitePassCreateTableDlg.CbPrimaryKeyClick(Sender: TObject);
begin
CbAutoInc.Enabled := CbPrimaryKey.Checked;
CbOnPrimaryKeyConflict.Enabled := CbPrimaryKey.Checked;
RbSortNone.Enabled := CbPrimaryKey.Checked;
RbSortUp.Enabled := CbPrimaryKey.Checked;
RbSortDown.Enabled := CbPrimaryKey.Checked;
LabelPrimaryKeyConflict.Enabled := CbPrimaryKey.Checked;
LabelPrimaryKeySort.Enabled := CbPrimaryKey.Checked;

if Not CbAutoInc.Enabled
   then CbAutoInc.Checked := False;

if CbPrimaryKey.Checked
   then InsertImage(ImagePrimaryKey, 18)
   else TImage(CtrlGrid.ControlByName('ImagePrimaryKey')).Free;
end;

procedure TSqlitePassCreateTableDlg.CbAutoIncClick(Sender: TObject);
begin
if CbAutoInc.Checked
   then InsertImage(ImageAutoinc, 36)
   else TImage(CtrlGrid.ControlByName('ImageAutoinc')).Free;
end;

procedure TSqlitePassCreateTableDlg.InsertImage(Image: TImage; LeftPos: Integer);
var
NewImage: TImage;
begin
  NewImage := TImage.Create(CtrlGrid.ActiveRow);
  CtrlGrid.ActiveRow.InsertControl(NewImage);
  With NewImage do
       begin
       Picture.Assign(Image.Picture);
       Name := Image.Name + '_' + IntToStr(Random(MaxInt));
       AutoSize := True;
       Left := LeftPos;
       Top := (CtrlGrid.ActiveRow.Height - Height) div 2;
       end;
end;


procedure TSqlitePassCreateTableDlg.TsSQLShow(Sender: TObject);
begin
 BtRefreshSQLClick(Self);
end;

Function TSqlitePassCreateTableDlg.CreateSqlStmt: String;
var
i: integer;
SqlStmt: TSqlitePassSqlStmt;
SQL: TStringList;
TableName, TempTable, IfNotExists, FieldName, FieldDataType,
FieldSize, FieldPrecision, FieldDefaultValue,
FieldCheck, FieldCollate, FieldDescription,
FieldStmt: String;
begin

CtrlGrid.Post;

SqlStmt := TSqlitePassSqlStmt.Create(FDatabase);

TableName := EditTableName.Text;
SqlStmt.QuoteString(TableName);

If CbTemporaryTable.Checked
   then TempTable := ' TEMPORARY '
   else TempTable := '';

If CbOverwriteExistingTable.Checked
   then IfNotExists := ''
   else IfNotExists := ' IF NOT EXISTS ';

SQL := TStringList.Create;
SQL.BeginUpdate;
SQL.Add('CREATE ' + TempTable + ' TABLE ' + IfNotExists + TableName);
SQL.Add('(');

For i := 0 to Pred(CtrlGrid.RowCount) do
 begin
 CtrlGrid.ActiveRowIndex := i;

 { Name }
 FieldName := TEdit(CtrlGrid.ControlByName('EditFieldName')).Text;
 if FieldName <> '' then SqlStmt.QuoteString(FieldName);

 { Datatype }
 FieldDataType := UpperCase(TComboBox(CtrlGrid.ControlByName('CbDataType')).Text);

 FieldStmt := FieldName + ' ' + FieldDataType;

 { Size }
 FieldSize := TEdit(CtrlGrid.ControlByName('EditSize')).Text;

 if FieldSize <> '' then
    begin
    FieldStmt := FieldStmt + '(' + FieldSize;
    { Precision }
    FieldPrecision := TEdit(CtrlGrid.ControlByName('EditPrecision')).Text;
    if FieldPrecision <> ''
       then FieldStmt := FieldStmt + '.' + FieldPrecision;
    FieldStmt := FieldStmt + ')';
    end;

 { Not NULL }
 if TCheckBox(CtrlGrid.ControlByName('PanelCbNotNull')).Checked then
    begin
    FieldStmt := FieldStmt + ' NOT NULL';
    if CbOnNotNullConflict.Text <> '(DEFAULT)'
       then FieldStmt := FieldStmt + ' ON CONFLICT ' + CbOnNotNullConflict.Text;
    end;

 { Primary Key }
 if TCheckBox(CtrlGrid.ControlByName('PanelCbPrimaryKey')).Checked then
    begin
    FieldStmt := FieldStmt + ' PRIMARY KEY';

    if RbSortUp.Checked
       then FieldStmt := FieldStmt + ' ASC';

    if RbSortDown.Checked
       then FieldStmt := FieldStmt + ' DESC';

    if TCheckBox(CtrlGrid.ControlByName('PanelCbAutoInc')).Checked
       then FieldStmt := FieldStmt + ' AUTOINCREMENT';
    end;

 { Unique }
 if TCheckBox(CtrlGrid.ControlByName('PanelCbUnique')).Checked then
    begin
    FieldStmt := FieldStmt + ' UNIQUE';
    if CbOnNotNullConflict.Text <> '(DEFAULT)'
       then FieldStmt := FieldStmt + ' ON CONFLICT ' + CbOnUniqueConflict.Text;
    end;

 { Check }
 FieldCheck := TEdit(CtrlGrid.ControlByName('EditFieldCheckConstraint')).Text;
 if FieldCheck <> '' then
    begin
    SqlStmt.QuoteString(FieldCheck);
    FieldStmt := FieldStmt + ' CHECK ' + FieldCheck;
    end;

 { Default}
 FieldDefaultValue := TComboBox(CtrlGrid.ControlByName('CbDefaultValue')).Text;
 if FieldDefaultValue <> ''
    then FieldStmt := FieldStmt + ' DEFAULT ' + FieldDefaultValue;

 { Collate }
 FieldCollate := TComboBox(CtrlGrid.ControlByName('CbCollatingOrder')).Text;
 if FieldCollate <> '(DEFAULT)'
    then FieldStmt := FieldStmt + ' COLLATE ' + FieldCollate;

 { If Field Name or Datatype is empty, we skip the line }
 If (FieldName <> '') and (FieldDataType <> '') then
    begin
    FieldDescription := TLabel(CtrlGrid.ControlByName('PanelFieldDescription')).Caption;
    if FieldDescription <> ''
       then SQL.Add('/* ' + FieldDescription + ' */');
    SQL.Add(FieldStmt + ',');
    end;

 end; { For }

FieldStmt := SQL[Pred(SQL.Count)];
System.Delete(FieldStmt, Length(FieldStmt),1);
SQL[Pred(SQL.Count)] := FieldStmt;
SQL.Add(');');
SQL.EndUpdate;
Result := SQL.Text;
SQL.Free;
SqlStmt.Free;
LabelManualSQL.Visible := False;
end;

procedure TSqlitePassCreateTableDlg.MemoSQLKeyPress(Sender: TObject;
  var Key: Char);
begin
  LabelManualSQL.Visible := MemoSQL.Visible;
end;

procedure TSqlitePassCreateTableDlg.MemoSQLKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  LabelManualSQL.Visible := MemoSQL.Visible;
end;

procedure TSqlitePassCreateTableDlg.BtLoadFromFileClick(Sender: TObject);
begin
If OpenDialog.Execute
   then MemoSQL.Lines.LoadFromFile(OpenDialog.filename);
end;

procedure TSqlitePassCreateTableDlg.BtSaveToFileClick(Sender: TObject);
begin
If SaveDialog.Execute
   then MemoSQL.Lines.SaveToFile(SaveDialog.filename);
end;

procedure TSqlitePassCreateTableDlg.BtRefreshSQLClick(Sender: TObject);
begin
if LabelManualSQL.Visible
   then if MessageDlg('Do you want to keep the previous manual changes to the SQL Statement ?',
                      mtWarning, [mbYes, mbNo], -1) = mrYes then Exit;
MemoSQL.Clear;
MemoSQL.Lines.SetText(PChar(CreateSqlStmt));
end;

procedure TSqlitePassCreateTableDlg.CbDataTypeExit(Sender: TObject);
var
FieldType: String;
CbDefValue: TComboBox;
begin
CbDefValue := TComboBox(CtrlGrid.ControlByName('CbDefaultValue'));
CbDefValue.Clear;
FieldType := UpperCase(TComboBox(CtrlGrid.ControlByName('CbDataType')).Text);

if (AnsiPos('DATETIME', FieldType) > 0) or (Pos('TIMESTAMP', FieldType) > 0)
   then begin
        CbDefValue.Items.Add('CURRENT_TIMESTAMP');
        Exit;
        end;

if AnsiPos('DATE', FieldType) > 0
   then CbDefValue.Items.Add('CURRENT_DATE');

if AnsiPos('TIME', FieldType) > 0
   then CbDefValue.Items.Add('CURRENT_TIME');
end;

procedure TSqlitePassCreateTableDlg.CbNotNullClick(Sender: TObject);
begin
LabelNotNullConflict.Enabled := CbNotNull.Checked;
CbOnNotNullConflict.Enabled  := CbNotNull.Checked;
end;

procedure TSqlitePassCreateTableDlg.CbUniqueClick(Sender: TObject);
begin
LabelUniqueConflict.Enabled := CbUnique.Checked;
CbOnUniqueConflict.Enabled  := CbUnique.Checked;
end;

initialization
 {$IFDEF FPC}
  {$I SqlitePassCreateTableDialog.lrs}
 {$ENDIF}
end.
 
