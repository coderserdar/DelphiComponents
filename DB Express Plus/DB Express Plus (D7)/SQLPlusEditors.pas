unit SQLPlusEditors;

{*************************************************************}
{*                 Freeware dbExpress Plus                   *}
{* Copyright (c) Business Software Systems, Inc. 1995 - 2002 *}
{*                   All rights reserved.                    *}
{*************************************************************}

{$N+,P+,S-,R-}

interface

uses
  Windows, Classes, Forms, Menus, ExtCtrls, ComCtrls, ActnList,
  DesignIntf, DesignEditors, ColnEdit, Dialogs;

type

  TAsciiExportRefTableProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TDestinationTableProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TAsciiDataDefFieldProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TAsciiDataDefCollectionEditor = class(TCollectionEditor)
  protected
    FAddAllMenuItem: TMenuItem;
    {Panel3: TPanel;
    ListView1: TListView;
    ImageList1: TImageList;}
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    AddAllCmd: TAction;
    procedure Loaded; override;
    procedure AddAllClick(Sender: TObject);
  end;

  TAsciiDataDefCollectionProperty = class(TCollectionProperty)
  public
    function GetEditorClass: TCollectionEditorClass; override;
  end;

  TDestinationFieldProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TDestinationFieldsCollectionEditor = class(TCollectionEditor)
  protected
    FAddAllMenuItem: TMenuItem;
    {Panel3: TPanel;
    ListView1: TListView;
    ImageList1: TImageList;}
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    AddAllCmd: TAction;
    procedure Loaded; override;
    procedure AddAllClick(Sender: TObject);
  end;

  TDestinationFieldsCollectionProperty = class(TCollectionProperty)
  public
    function GetEditorClass: TCollectionEditorClass; override;
  end;

procedure Register;

implementation

uses SQLScript, SQLMetaData, SQLDataPump, SQLMultiDBQuery, SQLFilter, DBNavPlus;

{Property Editors}

function TAsciiExportRefTableProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect, paAutoUpdate, paSortList];
end;

procedure TAsciiExportRefTableProperty.GetValues(Proc: TGetStrProc);
var
  TableNames: TStrings;
  TableNum: Integer;
begin
  TableNames := nil;
  try
    TableNames := TStringList.Create;
    if TSQLDataPump(GetComponent(0)).SQLMetaDataSource <> nil then begin
      TSQLDataPump(GetComponent(0)).SQLMetaDataSource.GetTableNames(TableNames, False);
      for TableNum := 0 to TableNames.Count - 1 do begin
        Proc(TableNames[TableNum]);
      end;
      end
    else begin
      Application.MessageBox('Source MetaData Name Missing.',
         'Error', MB_OK + MB_DefButton1 + MB_IconStop);
    end;
  finally
    TableNames.Free;
  end;
end;

function TDestinationTableProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect, paAutoUpdate, paSortList];
end;

procedure TDestinationTableProperty.GetValues(Proc: TGetStrProc);
var
  TableNames: TStrings;
  TableNum: Integer;
begin
  TableNames := nil;
  try
    TableNames := TStringList.Create;
    if TSQLDataPump(GetComponent(0)).SQLMetaDataDestination <> nil then begin
      TSQLDataPump(GetComponent(0)).SQLMetaDataDestination.GetTableNames(TableNames, False);
      for TableNum := 0 to TableNames.Count - 1 do begin
        Proc(TableNames[TableNum]);
      end;
      end
    else begin
      Application.MessageBox('Destination MetaData Name Missing.',
         'Error', MB_OK + MB_DefButton1 + MB_IconStop);
    end;
  finally
    TableNames.Free;
  end;
end;

function TAsciiDataDefFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paAutoUpdate, paSortList];
end;

procedure TAsciiDataDefFieldProperty.GetValues(Proc: TGetStrProc);
var
  FieldNames: TStrings;
  FieldNum: Integer;
begin
  FieldNames := nil;
  try
    FieldNames := TStringList.Create;
    if TSQLDataPump(TAsciiDataDefItem(GetComponent(0)).Collection.Owner).DestinationTable > #32 then
       TSQLDataPump(TAsciiDataDefItem(GetComponent(0)).Collection.Owner).SQLMetaDataDestination.GetFieldNames
          (TSQLDataPump(TAsciiDataDefItem(GetComponent(0)).Collection.Owner).DestinationTable, FieldNames);
    for FieldNum := 0 to FieldNames.Count - 1 do begin
      Proc(FieldNames[FieldNum]);
    end;
  finally
    FieldNames.Free;
  end;
end;

function TDestinationFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paAutoUpdate, paSortList];
end;

procedure TDestinationFieldProperty.GetValues(Proc: TGetStrProc);
var
  FieldNames: TStrings;
  FieldNum: Integer;
begin
  FieldNames := nil;
  try
    FieldNames := TStringList.Create;
    if TSQLDataPump(TDestinationFieldItem(GetComponent(0)).Collection.Owner).DestinationTable > #32 then
       TSQLDataPump(TDestinationFieldItem(GetComponent(0)).Collection.Owner).SQLMetaDataDestination.GetFieldNames
          (TSQLDataPump(TDestinationFieldItem(GetComponent(0)).Collection.Owner).DestinationTable, FieldNames);
    for FieldNum := 0 to FieldNames.Count - 1 do begin
      Proc(FieldNames[FieldNum]);
    end;
  finally
    FieldNames.Free;
  end;
end;

{TAsciiDataDefCollectionEditor}

procedure TAsciiDataDefCollectionEditor.Loaded;
begin
  inherited Loaded;
  FAddAllMenuItem := TMenuItem.Create(PopupMenu1);
  FAddAllMenuItem.Caption := 'Add All';
  FAddAllMenuItem.OnClick := AddAllClick;
  PopupMenu1.Items.Insert(4, FAddAllMenuItem);

  ToolButton6 := TToolButton.Create(Self);
  ToolButton7 := TToolButton.Create(Self);
  ToolButton6.Parent := ToolBar1;
  ToolButton6.Style := tbsDivider;
  ToolButton7.Parent := Self.ToolBar1;
  ToolButton7.Caption := 'Add A&ll';
  ToolButton7.Hint := 'Add All (Ins)';
  ToolButton7.ImageIndex := 0;
  ToolButton7.ComponentIndex := 4;
  ToolButton7.OnClick := AddAllClick;

 { Panel3: TPanel;
  //AddAllCmd := TAction.Create(Self);
  ListView1: TListView;
  ImageList1: TImageList;
  N2: TMenuItem;  }
end;

procedure TAsciiDataDefCollectionEditor.AddAllClick(Sender: TObject);
begin
  TSQLDataPump(Collection.Owner).AsciiDataDef.AddAll;
  Designer.Modified;      {tells designer to overload data (don't know exactly what happens here :))))}
end;

{TAsciiDataDefCollectionProperty}

function TAsciiDataDefCollectionProperty.GetEditorClass: TCollectionEditorClass;
begin
  Result := TAsciiDataDefCollectionEditor;
end;

{TDestinationFieldsCollectionEditor}

procedure TDestinationFieldsCollectionEditor.Loaded;
begin
  inherited Loaded;
  FAddAllMenuItem := TMenuItem.Create(PopupMenu1);
  FAddAllMenuItem.Caption := 'Add All';
  FAddAllMenuItem.OnClick := AddAllClick;
  PopupMenu1.Items.Insert(4, FAddAllMenuItem);

  ToolButton6 := TToolButton.Create(Self);
  ToolButton7 := TToolButton.Create(Self);
  ToolButton6.Parent := ToolBar1;
  ToolButton6.Style := tbsDivider;
  ToolButton7.Parent := Self.ToolBar1;
  ToolButton7.Caption := 'Add A&ll';
  ToolButton7.Hint := 'Add All (Ins)';
  ToolButton7.ImageIndex := 0;
  ToolButton7.ComponentIndex := 4;
  ToolButton7.OnClick := AddAllClick;
end;

procedure TDestinationFieldsCollectionEditor.AddAllClick(Sender: TObject);
begin
  TSQLDataPump(Collection.Owner).DestinationFields.AddAll;
  Designer.Modified;
end;

{TDestinationFieldsCollectionProperty}

function TDestinationFieldsCollectionProperty.GetEditorClass: TCollectionEditorClass;
begin
  Result := TDestinationFieldsCollectionEditor;
end;

procedure Register;
begin
  RegisterComponents('dbExpress', [TSQLScript]);
  RegisterComponents('dbExpress', [TSQLMetaData]);
  RegisterComponents('dbExpress', [TSQLDataPump]);
  RegisterComponents('dbExpress', [TSQLMultiDBQuery]);
  RegisterComponents('dbExpress',[TSQLFilter]);
  RegisterComponents('Data Controls', [TDBNavPlus]);
  RegisterPropertyEditor(TypeInfo(String), TSQLDataPump,
     'AsciiExportRefTable', TAsciiExportRefTableProperty);
  RegisterPropertyEditor(TypeInfo(String), TSQLDataPump,
     'DestinationTable', TDestinationTableProperty);
  RegisterPropertyEditor(TypeInfo(String), TAsciiDataDefItem,
     'FieldName', TAsciiDataDefFieldProperty);
  RegisterPropertyEditor(TypeInfo(TCollection), TSQLDataPump,
     'AsciiDataDef', TAsciiDataDefCollectionProperty);
  RegisterPropertyEditor(TypeInfo(String), TDestinationFieldItem,
     'FieldName', TDestinationFieldProperty);
  RegisterPropertyEditor(TypeInfo(TCollection), TSQLDataPump,
      'DestinationFields', TDestinationFieldsCollectionProperty);
end;

end.

