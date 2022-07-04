{$I fb_define.inc}
unit fbcustomdatasetpropeditorl;

interface
uses Classes, PropEdits, componenteditors, typinfo;

type
  TFBDataSetSQLProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TFBDataSetEditor = class(TComponentEditor)
  public
  {$IFDEF FPC}
    DefaultEditor: TBaseComponentEditor;
    constructor Create(AComponent: TComponent; ADesigner: TComponentEditorDesigner); override;
  {$ELSE}
    DefaultEditor: IComponentEditor;
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
  {$ENDIF}
    destructor Destroy; override;
    function GetVerbCount:integer;override;
    function GetVerb(Index:integer):string;override;
    procedure ExecuteVerb(Index:integer);override;
//    procedure Edit;override;
  end;

  TAutoUpdateOptionsProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string; override;
  end;


procedure Register;
implementation
uses fbcustomdataset, Forms, Controls, fbcustomdatasetautoupdateoptionseditorl,
  DB, SysUtils, fbcustomdatasetsqleditorl, LResources;

procedure Register;
begin
  {$IFDEF FBSQLEditor}
  RegisterPropertyEditor(TypeInfo(TStrings), TFBDataSet, 'SQLSelect', TFBDataSetSQLProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TFBDataSet, 'SQLRefresh', TFBDataSetSQLProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TFBDataSet, 'SQLEdit', TFBDataSetSQLProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TFBDataSet, 'SQLDelete', TFBDataSetSQLProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TFBDataSet, 'SQLInsert', TFBDataSetSQLProperty); {do not localize}
  {$ENDIF}
  RegisterPropertyEditor(TypeInfo(TAutoUpdateOptions), TFBDataSet, 'AutoUpdateOptions', TAutoUpdateOptionsProperty); {do not localize}

  RegisterComponentEditor(TFBDataSet, TFBDataSetEditor);
end;

{ TFBDataSetSQLProperty }

procedure TFBDataSetSQLProperty.Edit;
var
  SQLEditor:TFBCustomDataSetSQLEditor;
begin
  SQLEditor:=TFBCustomDataSetSQLEditor.CreateEditor(GetComponent(0) as TFBDataSet);
  if GetPropInfo^.Name='SQLDelete' then
    SQLEditor.PageControl1.ActivePageIndex:=3
  else
  if GetPropInfo^.Name='SQLEdit' then
    SQLEditor.PageControl1.ActivePageIndex:=2
  else
  if GetPropInfo^.Name='SQLInsert' then
    SQLEditor.PageControl1.ActivePageIndex:=1
  else
  if GetPropInfo^.Name='SQLRefresh' then
    SQLEditor.PageControl1.ActivePageIndex:=4
  else
  if GetPropInfo^.Name='SQLSelect' then
    SQLEditor.PageControl1.ActivePageIndex:=0;
  try
    if SQLEditor.ShowModal=mrOk then
      {inherited Designer.}Modified;
  finally
    SQLEditor.Free;
  end;
end;

function TFBDataSetSQLProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

{ TFBDataSetEditor }

procedure TFBDataSetEditor.ExecuteVerb(Index: integer);
var
  SQLEditor:TFBCustomDataSetSQLEditor;
begin
  if Index < DefaultEditor.GetVerbCount then
    DefaultEditor.ExecuteVerb(Index)
  else
  begin
    case Index - DefaultEditor.GetVerbCount of
      0:begin
          SQLEditor:=TFBCustomDataSetSQLEditor.CreateEditor(Component as TFBDataSet);
          try
            if SQLEditor.ShowModal=mrOk then
              Modified;
          finally
              SQLEditor.Free;
          end;
        end;
    end;
  end;
end;

function TFBDataSetEditor.GetVerb(Index: integer): string;
begin
  if Index < DefaultEditor.GetVerbCount then
    Result := DefaultEditor.GetVerb(Index)
  else
  begin
    case Index - DefaultEditor.GetVerbCount of
      0:Result:='SQL editor';
    end;
  end;
end;

type
  PClass = ^TClass;
constructor TFBDataSetEditor.Create(AComponent: TComponent; ADesigner: TComponentEditorDesigner);
var
  CompClass: TClass;
begin
  inherited Create(AComponent, ADesigner);
  CompClass := PClass(Acomponent)^;
  try
    PClass(AComponent)^ := TDataSet;
    DefaultEditor := GetComponentEditor(AComponent, ADesigner);
  finally
    PClass(AComponent)^ := CompClass;
  end;
end;

destructor TFBDataSetEditor.Destroy;
begin
  DefaultEditor.Free;
  inherited Destroy;
end;

function TFBDataSetEditor.GetVerbCount: integer;
begin
  Result:=DefaultEditor.GetVerbCount + 1;
end;

{ TAutoUpdateOptionsProperty }

procedure TAutoUpdateOptionsProperty.Edit;
begin
  FBCustomDataSetAutoUpdateOptionsEditorForm:=TFBCustomDataSetAutoUpdateOptionsEditorForm.Create(Application);
  try
    if FBCustomDataSetAutoUpdateOptionsEditorForm.ShowEditor(GetComponent(0) as TFBDataSet) then
      Modified;
  finally
    FBCustomDataSetAutoUpdateOptionsEditorForm.Free;
  end;
end;

function TAutoUpdateOptionsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TAutoUpdateOptionsProperty.GetValue: string;
begin
  with TAutoUpdateOptions(GetOrdProp(GetComponent(0), 'AutoUpdateOptions')) do
    if IsComplete then
      Result:={UpdateTableName+'.'+}KeyField+'=GenID('+GeneratorName+', '+IntToStr(IncrementBy)+')'
    else
      Result:='';
end;

initialization
  {$I fb_laz_resourse.inc}
end.
