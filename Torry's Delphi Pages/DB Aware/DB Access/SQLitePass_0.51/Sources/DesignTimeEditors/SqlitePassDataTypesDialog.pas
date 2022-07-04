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
    Last update : 31/05/2008

  --------------------------------------------------------------------------- }

unit SqlitePassDataTypesDialog;
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

  { TSqlitePassDatatypesDlg }

  TSqlitePassDatatypesDlg = class(TForm)
    PanelBottom: TPanel;
    PanelDatatypes: TPanel;
    BtOk: TButton;
    BtnResetAll: TButton;
    BtDefault: TButton;
    BtnSaveToDatabase: TButton;
    BtnLoadFromDatabase: TButton;
    PanelCtrlGridHeader: TPanel;
    Bevel5: TBevel;
    Shape5: TShape;
    Label6: TLabel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText6: TStaticText;
    StaticText4: TStaticText;
    PanelCtrlGrid: TPanel;
    PanelMappingRules: TPanel;
    CbMappingMode: TComboBox;
    CbResultDataType: TComboBox;
    PanelCtrlGridNav: TPanel;
    EditDatatypeName: TEdit;
    ImageCustomized: TImage;
    procedure BtOkClick(Sender: TObject);
    procedure BtnResetAllClick(Sender: TObject);
    procedure BtDefaultClick(Sender: TObject);
    procedure BtnLoadFromDatabaseClick(Sender: TObject);
    procedure BtnSaveToDatabaseClick(Sender: TObject);
    procedure EditDatatypeNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditDatatypeNameEnter(Sender: TObject);
  private
    Db: TSqlitePassDatabase;
    CtrlGrid: TSPVTCtrlGrid;
    Navigator: TSPVTCtrlGridNavigator;
    procedure DisplayConversionRules;
    procedure OnCtrlGridNewRecord(CtrlGrid: TSPVTCtrlGrid);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  SqlitePassDatatypesDlg: TSqlitePassDatatypesDlg;

implementation

{$IFNDEF FPC}
 {$R *.DFM}
{$ENDIF}

{ Constructor and Destructor }

constructor TSqlitePassDatatypesDlg.Create(AOwner: TComponent);
var
ft: TFieldType;
begin
inherited Create(nil);
Db := TSqlitePassDatabase(AOwner);
CbMappingMode.Sorted := False;
CbResultDataType.Sorted := True;
CbResultDataType.Clear;
For ft := Low(TFieldType) to High(TFieldType)
   do CbResultDataType.Items.Add(FieldTypeToString(ft));

CtrlGrid := TSPVTCtrlGrid.Create(PanelCtrlGrid);

With CtrlGrid do
     begin
     Align := AlClient;
     OnNewRecord := OnCtrlGridNewRecord;
     Panel := PanelMappingRules;
     AutoScroll := True;
     end;

Navigator := TSPVTCtrlGridNavigator.Create(PanelCtrlGridNav);
Navigator.SetBounds(1,1,Navigator.Width,Navigator.Height);
Navigator.CtrlGrid := CtrlGrid;
DisplayConversionRules;
end;

procedure TSqlitePassDatatypesDlg.DisplayConversionRules;
var
i: Integer;
begin
Try
  CtrlGrid.Visible := False;
  CtrlGrid.Clear;
  For i := 0 to Pred(Db.DatatypeOptions.TranslationRules.Count) do
      begin
      CtrlGrid.Append;
      TEdit(CtrlGrid.ControlByName('EditDatatypeName')).Text := Db.DatatypeOptions.TranslationRules[i].DataTypeName;
      TComboBox(CtrlGrid.ControlByName('CbMappingMode')).ItemIndex := Ord(Db.DatatypeOptions.TranslationRules[i].MappingMode);
      TComboBox(CtrlGrid.ControlByName('CbResultDataType')).ItemIndex := TComboBox(CtrlGrid.ControlByName('CbResultDataType')).Items.IndexOf(FieldTypeToString(Db.DatatypeOptions.TranslationRules[i].FieldType));
      TImage(CtrlGrid.ControlByName('ImageCustomized')).Visible := Db.DatatypeOptions.TranslationRules.IsCustomized(Db.DatatypeOptions.TranslationRules[i]);
      end;
  { To Set State to cgsBrowse }
  CtrlGrid.First;
  if CtrlGrid.RowCount = 0
     then CtrlGrid.Append;
Finally
  CtrlGrid.Visible := True;
end;
end;


{ Buttons Events }

procedure TSqlitePassDatatypesDlg.BtOkClick(Sender: TObject);
var
i: integer;
NewConversionRule: TSqlitePassFieldTypeTranslationRule;
begin
Db.DatatypeOptions.TranslationRules.ClearAndFreeItems;
For i := 0 to Pred(CtrlGrid.RowCount) do
    begin
    CtrlGrid.ActiveRowIndex := i;
    if TEdit(CtrlGrid.ControlByName('EditDatatypeName')).Text <> '' then
       begin
       NewConversionRule := TSqlitePassFieldTypeTranslationRule.Create;
       With NewConversionRule do
            begin
            DataTypeName := TEdit(CtrlGrid.ControlByName('EditDatatypeName')).Text;
            MappingMode := TSqlitePassDataTypeMappingMode(TComboBox(CtrlGrid.ControlByName('CbMappingMode')).ItemIndex);
            FieldType := StringToFieldType(TComboBox(CtrlGrid.ControlByName('ResultDataType')).Text);
            end;
       Db.DatatypeOptions.TranslationRules.Add(NewConversionRule);
       end;
    end;
end;

procedure TSqlitePassDatatypesDlg.OnCtrlGridNewRecord(
  CtrlGrid: TSPVTCtrlGrid);
begin
TEdit(CtrlGrid.ControlByName('EditDatatypeName')).Text := '';
TComboBox(CtrlGrid.ControlByName('CbMappingMode')).ItemIndex := 4;
TComboBox(CtrlGrid.ControlByName('CbResultDataType')).ItemIndex := 0;
TImage(CtrlGrid.ControlByName('ImageCustomized')).Visible := True;
end;

{ Events }

procedure TSqlitePassDatatypesDlg.BtnResetAllClick(Sender: TObject);
begin
DisplayConversionRules;
end;

procedure TSqlitePassDatatypesDlg.BtDefaultClick(Sender: TObject);
begin
Db.DatatypeOptions.TranslationRules.LoadDefaultTranslationRules;
DisplayConversionRules;
end;

procedure TSqlitePassDatatypesDlg.BtnLoadFromDatabaseClick(
  Sender: TObject);
begin
Db.DatatypeOptions.LoadFromDatabase([loTranslationRules]);
DisplayConversionRules;
end;

procedure TSqlitePassDatatypesDlg.BtnSaveToDatabaseClick(Sender: TObject);
begin
BtOkClick(Sender);
Db.DatatypeOptions.SaveToDatabase([soTranslationRules]);
end;

procedure TSqlitePassDatatypesDlg.EditDatatypeNameKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
{ Classic up/down keys with comboboxes }
 if (Sender is TComboBox) then
    if (Key = VK_DOWN) or (Key = VK_UP) then Exit;

 CtrlGrid.KeyDown(Key, Shift);
end;

procedure TSqlitePassDatatypesDlg.EditDatatypeNameEnter(Sender: TObject);
begin
  CtrlGrid.FocusActiveRow(Sender);
end;

initialization
 {$IFDEF FPC}
  {$I SqlitePassDataTypesDialog.lrs}
 {$ENDIF}
end.
    
