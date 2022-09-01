unit UnitSnapAddFieldEditor;

{*******************************************************************}
{                                                                   }
{       SnapObjectDataset Field Editor                              }
{                                                                   }
{       Copyright (c) 2006 by Cosimo De Michele.                    }
{       ALL RIGHTS RESERVED                                         }
{                                                                   }
{   The entire contents of this file is protected by                }
{   International Copyright Laws. Unauthorized reproduction,        }
{   reverse-engineering, and distribution of all or any portion of  }
{   the code contained in this file is strictly prohibited and may  }
{   result in severe civil and criminal penalties and will be       }
{   prosecuted to the maximum extent possible under the law.        }
{                                                                   }
{   RESTRICTIONS                                                    }
{                                                                   }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED      }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE        }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE       }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT  }
{   AND PERMISSION FROM THE AUTHOR.                                 }
{                                                                   }
{*******************************************************************}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, DB, DesignIntf, SnapObjectDataset;

type
  IFormDesigner = IDesigner;

  TFormAddField = class(TForm)
    LookupGroup: TGroupBox;
    DatasetLabel: TLabel;
    KeyFieldsLabel: TLabel;
    LookupKeysLabel: TLabel;
    ResultFieldLabel: TLabel;
    cbDataSet: TComboBox;
    cbKeyField: TComboBox;
    cbLookupField: TComboBox;
    cbResultField: TComboBox;
    btnOk: TButton;
    CancelBtn: TButton;
    FieldGroup: TGroupBox;
    ComponentNameLabel: TLabel;
    FieldNameLabel: TLabel;
    FieldTypeLabel: TLabel;
    SizeEditLabel: TLabel;
    edComponent: TEdit;
    edName: TEdit;
    cbFieldType: TComboBox;
    edSize: TEdit;
    gbFieldtype: TRadioGroup;
    procedure cbFieldTypeChange(Sender: TObject);
    procedure cbDataSetExit(Sender: TObject);
    procedure edComponentChange(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure edSizeKeyPress(Sender: TObject; var Key: Char);
    procedure gbFieldtypeClick(Sender: TObject);
  private
    Data: TSnapObjectDataset;
    LookupDS: TDataSet;
    FormDesigner: IFormDesigner;
    procedure GetDataSets(const AComponentName: string);
  public
    { Public declarations }
  end;

function GetSnapDatasetNewFieldType(Data: TSnapObjectDataset; X, Y: Integer; FormDesigner: IFormDesigner): TField;

implementation

{$R *.dfm}

uses TypInfo, Consts ,RTLConsts;

function GetSnapDatasetNewFieldType(Data: TSnapObjectDataset; X, Y: Integer; FormDesigner: IFormDesigner): TField;
var
  AForm: TFormAddField;
  i: TFieldType;
  j: Integer;
  fc: TFieldClass;
begin
  Result := nil;
  AForm := TFormAddField.Create(Application);
  try
    AForm.Data := Data;
    AForm.FormDesigner := FormDesigner;
    with AForm do
    begin
      for i := Low(TFieldType) to High(TFieldType) do
        if Data.SupportedFieldType(TFieldType(i)) then
          cbFieldType.Items.AddObject( FieldTypeNames[i], TObject(Ord(i)) );

      cbFieldType.ItemIndex := 0;
      with Data do
        for j := 0 to FieldCount - 1 do
          if (Fields[j].Owner = Owner) and (Fields[j].FieldName <> '') then
            cbKeyField.Items.Add(Fields[j].FieldName);

      FormDesigner.GetComponentNames(GetTypeData(TDataset.ClassInfo), GetDataSets);

      Left := X;
      Top := Y;
      if ShowModal = mrOk then
      begin
        i := TFieldType(cbFieldType.Items.Objects[cbFieldType.ItemIndex]);
        fc := DefaultFieldClasses[i];
        if fc<>nil then
        begin
          Result := fc.Create(Data.Owner);
          with Result do
          begin
            try
              FieldName := edName.Text;
              DataSet := Data;
              Name := edComponent.Text;
            except
              Result.Free;
              raise;
            end;
            try
              if edSize.Text <> '' then
                TStringField(Result).Size := StrToInt(edSize.Text);
            except
            end;
            Calculated := gbFieldtype.ItemIndex = 1;
            Lookup := gbFieldtype.ItemIndex = 2;
            if Lookup then
            begin
              KeyFields := cbKeyField.Text;
              LookupDataSet := LookupDS;
              LookupKeyFields := cbLookupField.Text;
              LookupResultField := cbResultField.Text;
            end;
            if FormDesigner <> nil then
              FormDesigner.Modified;
          end;
        end
        else
          raise Exception.CreateFmt('No field class for field type %s', [cbFieldType.Text]);
      end;
    end;
  finally
    AForm.Free;
  end;
end;

procedure TFormAddField.cbFieldTypeChange(Sender: TObject);
begin
  edSize.Enabled := SameText(cbFieldType.Text, 'String') or SameText(cbFieldType.Text, 'WideString');
  if not edSize.Enabled then
    edSize.Text := '';
end;

procedure TFormAddField.gbFieldtypeClick(Sender: TObject);
begin
  cbKeyField.Enabled := gbFieldtype.ItemIndex = 2;
  cbDataSet.Enabled := cbKeyField.Enabled;
  cbLookupField.Enabled := cbKeyField.Enabled;
  cbResultField.Enabled := cbKeyField.Enabled;
  if not cbResultField.Enabled then
  begin
    cbKeyField.ItemIndex := -1;
    cbDataSet.Text := '';
    cbLookupField.ItemIndex := -1;
    cbResultField.ItemIndex := -1;
    LookupDS := nil;
  end;
end;

procedure TFormAddField.edNameChange(Sender: TObject);
begin
  edComponent.Text := Data.Name + edName.Text;
  btnOk.Enabled := (edComponent.Text <> '') and (edName.Text <> '');;
end;

procedure TFormAddField.edSizeKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
  begin
    Key := #0;
    MessageBeep(0);
  end;
end;

procedure TFormAddField.edComponentChange(Sender: TObject);
begin
  btnOk.Enabled := (edComponent.Text <> '') and (edName.Text <> '');
end;

procedure TFormAddField.cbDataSetExit(Sender: TObject);
var
  Component: TComponent;
  i: Integer;
begin
  LookupDS := nil;
  cbLookupField.Items.Clear;
  cbResultField.Items.Clear;
  if not (csDesigning in Data.ComponentState) then
    Exit;
  if cbDataSet.Text = '' then
    Component := nil
  else
  begin
    Component := FormDesigner.GetComponent(cbDataSet.Text);
    if not (Component is TDataSet) then
    begin
      raise EPropertyError.Create(SInvalidPropertyValue);
      Component := nil;
      cbDataSet.Text := '';
    end;
  end;
  if Component <> nil then
  begin
    LookupDS := TDataSet(Component);
    if LookupDS.Active then
    begin
      for i := 0 to LookupDS.FieldCount - 1 do
        if LookupDS.Fields[i].FieldName <> '' then
          cbLookupField.Items.Add(LookupDS.Fields[i].FieldName)
    end
    else
    begin
      LookupDS.FieldDefs.Update;
      for i := 0 to LookupDS.FieldDefs.Count - 1 do
        if LookupDS.FieldDefs[i].Name <> '' then
          cbLookupField.Items.Add(LookupDS.FieldDefs[i].Name);
    end;
    cbResultField.Items.Assign(cbLookupField.Items);
  end;
end;

procedure TFormAddField.GetDataSets(const AComponentName: string);
begin
  cbDataSet.Items.Add(AComponentName);
end;

end.
