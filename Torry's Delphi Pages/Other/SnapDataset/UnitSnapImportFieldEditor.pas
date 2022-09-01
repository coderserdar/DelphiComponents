unit UnitSnapImportFieldEditor;

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
  Dialogs, StdCtrls, ExtCtrls, CheckLst, SnapObjectDataset, DesignIntf,
  UnitSnapClassManager, DB, SnapBaseDataset, ComCtrls;

type
  IFormDesigner = IDesigner;

  TFormImportField = class(TForm)
    Label1: TLabel;
    cmbUnits: TComboBox;
    cmbClasses: TComboBox;
    Label5: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    stType: TStaticText;
    stName: TStaticText;
    cmbFieldType: TComboBox;
    gbProperties: TGroupBox;
    lvProperties: TListView;
    btnCancel: TButton;
    btnOK: TButton;
    ComponentNameLabel: TLabel;
    edComponent: TEdit;
    btnAboutBox: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmbUnitsClick(Sender: TObject);
    procedure cmbClassesClick(Sender: TObject);
    procedure lbPropertiesClick(Sender: TObject);
    procedure lvPropertiesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure edComponentExit(Sender: TObject);
    procedure cmbFieldTypeClick(Sender: TObject);
    procedure btnAboutBoxClick(Sender: TObject);
  private
    fClassManager: TClassManager;
    Data: TSnapObjectDataset;
    FormDesigner: IFormDesigner;
    function GetFieldType(AType: string): TFieldType;
    procedure LoadProperties(AClass: TBrowseClassInfo);
    procedure UnLoadProperties(AClass: TBrowseClassInfo);
    procedure ViewInfo(AMethod: TBrowseMethodInfo);
    constructor Create(AOwner: TComponent; AData: TSnapObjectDataset; AFormDesigner: IFormDesigner); virtual;
  end;

  function GetSnapDatasetImportField(Data: TSnapObjectDataset; X, Y: Integer; FormDesigner: IFormDesigner): boolean;

implementation

{$R *.dfm}

uses
  Registry,
  ExptIntf, UnitSnapAboutBoxEditor;


type
  EnumType = 0..33;

const
  DelphiType: array[EnumType] of string =
                                       ('Char',
                                        'ShortString',
                                        'String',
                                        'AnsiString',
                                        'WideString',
                                        'Boolean',
                                        'ByteBool',
                                        'WordBool',
                                        'LongBool',
                                        'Byte',
                                        'ShortInt',
                                        'Smallint',
                                        'Word',
                                        'DWORD',
                                        'LongWord',
                                        'Integer',
                                        'Cardinal',
                                        'Int64',
                                        'LongInt',
                                        'TDate',
                                        'TTime',
                                        'TDateTime',
                                        'Real',
                                        'Float',
                                        'Single',
                                        'Double',
                                        'Extended',
                                        'Currency',
                                        'Comp',
                                        'TStrings',
                                        'TPicture',
                                        'TCollection',
                                        'TObjectList',
                                        'TObject');

  DelphiFieldType: array[EnumType] of TFieldType =
                                       (ftString,
                                        ftString,
                                        ftstring,
                                        ftString,
                                        ftString,
                                        ftBoolean,
                                        ftBoolean,
                                        ftBoolean,
                                        ftBoolean,
                                        ftSmallInt,
                                        ftSmallInt,
                                        ftSmallInt,
                                        ftWord,
                                        ftInteger,
                                        ftInteger,
                                        ftInteger,
                                        ftLargeInt,
                                        ftLargeInt,
                                        ftLargeInt,
                                        ftDate,
                                        ftTime,
                                        ftDateTime,
                                        ftFloat,
                                        ftFloat,
                                        ftFloat,
                                        ftFloat,
                                        ftFloat,
                                        ftFloat,
                                        ftFloat,
                                        ftMemo,
                                        ftGraphic,
                                        ftDataset,
                                        ftDataset,
                                        ftDataset);

type
  TAdditionalInfo = class
  private
    fComponentName: string;
    fFieldType: TFieldType;
  public
    property ComponentName: string read fComponentName write fComponentName;
    property FieldType: TFieldType read fFieldType write fFieldType;
  end;

function DelphiTypeSupported(ADelphiType: string): boolean;
var
  idx: integer;
begin
  Result := False;
  for idx:=Low(DelphiType) to high(DelphiType) do
    if SameText(ADelphiType, DelphiType[idx]) then
    begin
      Result := True;
      break;
    end;
end;


function GetSnapDatasetImportField(Data: TSnapObjectDataset; X, Y: Integer; FormDesigner: IFormDesigner): boolean;
var
  AForm: TFormImportField;
  ft: TFieldType;
  i, j: Integer;
  fc: TFieldClass;
  f: TField;
  TheType: string;
  TheMethod: TBrowseMethodInfo;
  Info: TAdditionalInfo;
begin
  Result := False;
  AForm := TFormImportField.Create(Application, Data, FormDesigner);
  try
    //AForm.Data := Data;
    //AForm.FormDesigner := FormDesigner;
    with AForm do
    begin
      Left := X;
      Top := Y;
      if ShowModal = mrOk then
      begin
        for i:=0 to (lvProperties.Items.Count - 1) do
          if lvProperties.Items[i].Selected then
          begin
            TheMethod := TBrowseMethodInfo(lvProperties.Items[i].Data);
            Info := TAdditionalInfo(TheMethod.Data);
            if Info<>nil then
            begin
              fc := DefaultFieldClasses[Info.FieldType];
              if fc<>nil then
              begin
                f := fc.Create(Data.Owner);
                try
                  f.FieldName := TheMethod.RName;
                  f.DataSet := Data;
                  f.Name := Info.ComponentName;
                except
                  f.Free;
                  raise;
                end;
                {try
                  if edSize.Text <> '' then
                    TStringField(f).Size := StrToInt(edSize.Text);
                except
                end;}
                if FormDesigner <> nil then
                  FormDesigner.Modified;
              end
              else
                raise Exception.CreateFmt('No field class for field type %s', [TheMethod.TypeName]);
            end;
          end;
        Result := True;
      end;
    end;
  finally
    AForm.Free;
  end;
end;

procedure TFormImportField.FormCreate(Sender: TObject);
var
  i: integer;
  ft: TFieldType;
begin
  fClassManager := TClassManager.Create;

  fClassManager.LoadFromProject;

  cmbFieldType.Clear;
  for ft:=Low(TFieldType) to high(TFieldType) do
    if ft in ftSupportedType then
      cmbFieldType.Items.Add(FieldTypeNames[ft]);

  cmbUnits.Clear;
  for i:=0 to (fClassManager.Count - 1) do
    cmbUnits.Items.AddObject(fClassManager.Items[i].Name, fClassManager.Items[i]);
  if cmbUnits.Items.Count>0 then
  begin
    cmbUnits.ItemIndex := 0;
    cmbUnitsClick(cmbUnits);
  end;
end;

procedure TFormImportField.FormDestroy(Sender: TObject);
var
  i, j: integer;
begin
  for i:=0 to (fClassManager.Count - 1) do
    for j:=0 to (fClassManager.Items[i].ClassCount - 1) do
      UnloadProperties(fClassManager.Items[i].ClassItem[j]);
  fClassManager.Free;
end;

procedure TFormImportField.cmbUnitsClick(Sender: TObject);
var
  TheUnit: TBrowseUnitInfo;
  TheClass: TBrowseClassInfo;
  i: integer;
begin
  cmbClasses.Clear;
  lvProperties.Clear;
  if cmbUnits.ItemIndex<>-1 then
  begin
    TheUnit := TBrowseUnitInfo(cmbUnits.Items.Objects[cmbUnits.ItemIndex]);
    for i:=0 to (TheUnit.ClassCount - 1) do
      cmbClasses.Items.AddObject(TheUnit.ClassItem[i].Name + ' (' + TheUnit.ClassItem[i].ObjectDerivedFrom + ')', TheUnit.ClassItem[i]);
    if cmbClasses.Items.Count>0 then
    begin
      cmbClasses.ItemIndex := 0;
      cmbClassesClick(cmbClasses);
    end;
  end;
end;

procedure TFormImportField.cmbClassesClick(Sender: TObject);
var
  TheMethod: TBrowseMethodInfo;
  TheClass, ci: TBrowseClassInfo;
  i: integer;
  s: string;
begin
  lvProperties.Clear;
  if cmbClasses.ItemIndex<>-1 then
    LoadProperties(TBrowseClassInfo(cmbClasses.Items.Objects[cmbClasses.ItemIndex]))
  else
    LoadProperties(nil);
  btnOK.Enabled := lvProperties.SelCount>0;
end;

procedure TFormImportField.lbPropertiesClick(Sender: TObject);
begin
  btnOK.Enabled := lvProperties.SelCount>0;
end;

procedure TFormImportField.LoadProperties(AClass: TBrowseClassInfo);
var
  i: integer;
  TheMethod: TBrowseMethodInfo;
  TheClass: TBrowseClassInfo;
  s: string;
  Item: TListItem;
  Info: TAdditionalInfo;
begin
  lvProperties.Clear;
  ViewInfo(nil);
  if AClass<>nil then
  begin
    for i:=0 to (AClass.Count - 1) do
    begin
      TheMethod := AClass.Items[i];
      if (TheMethod.MethodType in [ctVariable, ctProperty]) and (TheMethod.MethodDeclare = cdPublished) then
      begin
        s := TheMethod.TypeName;
        TheClass := fClassManager.ClassByName(s);
        if TheClass<>nil then
          s := TheClass.ObjectDerivedFrom;
        Item := lvProperties.Items.Add;
        Item.Caption := TheMethod.RName;
        Item.Data := TheMethod;
        Item.SubItems.Add(s);
        { check if property type is supported }
        if DelphiTypeSupported(s) then
          Item.Selected := True;
        if TheMethod.Data=nil then
        begin
          Info := TAdditionalInfo.Create;
          Info.ComponentName := Data.Name + TheMethod.RName;
          Info.FieldType := GetFieldType(s);
          TheMethod.Data := Info;
        end
        else
          Info := TAdditionalInfo(TheMethod.Data);

        s := FieldTypeNames[Info.FieldType];
        Item.SubItems.Add(s);
        s := Info.ComponentName;
        Item.SubItems.Add(s);
      end;
    end;
    //lvProperties.SetFocus;
  end;
end;

procedure TFormImportField.lvPropertiesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if (Item<>nil) and (Selected) and (lvProperties.SelCount=1) then
  begin
    ViewInfo(Item.Data);
  end
  else
    ViewInfo(nil);
end;

procedure TFormImportField.ViewInfo(AMethod: TBrowseMethodInfo);
var
  Info: TAdditionalInfo;
  s: string;
begin
  if AMethod<>nil then
  begin
    stName.Caption := AMethod.RName;
    stType.Caption := AMethod.TypeName;
    Info := TAdditionalInfo(AMethod.Data);
    if Info<>nil then
    begin
      edComponent.Text := Info.ComponentName;
      s := FieldTypeNames[Info.FieldType];
      cmbFieldType.ItemIndex := cmbFieldType.Items.IndexOf(s);
    end
    else
    begin
      edComponent.Text := '';
      cmbFieldType.ItemIndex := -1;
    end;
  end
  else
  begin
    stName.Caption := '';
    stType.Caption := '';
    edComponent.Text := '';
    cmbFieldType.ItemIndex := -1;
  end;
end;

function TFormImportField.GetFieldType(AType: string): TFieldType;
var
  i: EnumType;
begin
  Result := ftUnknown;
  for i:=Low(EnumType) to high(EnumType) do
    if SameText(AType, DelphiType[i]) then
    begin
      Result := DelphiFieldType[i];
      break;
    end;
end;

procedure TFormImportField.UnLoadProperties(AClass: TBrowseClassInfo);
var
  i: integer;
  TheMethod: TBrowseMethodInfo;
  TheClass: TBrowseClassInfo;
  s: string;
  Item: TListItem;
  Info: TAdditionalInfo;
begin
  if AClass<>nil then
  begin
    for i:=0 to (AClass.Count - 1) do
    begin
      TheMethod := AClass.Items[i];
      if (TheMethod.Data<>nil) then
      begin
        Info := TAdditionalInfo(TheMethod.Data);
        Info.Free;
        TheMethod.Data := nil;
      end;
    end;
  end;
end;

procedure TFormImportField.edComponentExit(Sender: TObject);
var
  TheMethod: TBrowseMethodInfo;
  Info: TAdditionalInfo;
begin
  if (edComponent.Modified) and (lvProperties.Selected<>nil) then
  begin
    TheMethod := lvProperties.Selected.Data;
    if TheMethod<>nil then
    begin
      Info := TAdditionalInfo(TheMethod.Data);
      if Info<>nil then
      begin
        Info.ComponentName := edComponent.Text;
        lvProperties.Selected.SubItems[2] := edComponent.Text;
      end;
    end;
  end;
end;

procedure TFormImportField.cmbFieldTypeClick(Sender: TObject);
var
  TheMethod: TBrowseMethodInfo;
  Info: TAdditionalInfo;
  i: TFieldType;
begin
  if (cmbFieldType.ItemIndex<>-1) and (lvProperties.Selected<>nil) then
  begin
    TheMethod := lvProperties.Selected.Data;
    if TheMethod<>nil then
    begin
      Info := TAdditionalInfo(TheMethod.Data);
      if Info<>nil then
      begin
        for i:=Low(FieldTypeNames) to high(FieldTypeNames) do
          if SameText(FieldTypeNames[i], cmbFieldType.Text) then
          begin
            Info.FieldType := i;
            lvProperties.Selected.SubItems[1] := cmbFieldType.Text;
            break;
          end;
      end;
    end;
  end;
end;

procedure TFormImportField.btnAboutBoxClick(Sender: TObject);
begin
  with TFormEditorAboutBox.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

constructor TFormImportField.Create(AOwner: TComponent;
  AData: TSnapObjectDataset; AFormDesigner: IFormDesigner);
begin
  inherited Create(AOwner);
  Data := AData;
  AFormDesigner := AFormDesigner;
end;

end.
