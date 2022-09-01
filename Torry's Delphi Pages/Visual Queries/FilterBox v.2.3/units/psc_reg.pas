{***************************************************************************}
{                                                                           }
{  Copyright (c) 1999-2015 Sergiy Kurinny                                   }
{                                                                           }
{  This library is free software; you can redistribute it and/or            }
{  modify it under the terms of the GNU Lesser General Public               }
{  License version 2.1 as published by the Free Software Foundation         }
{  and appearing in the file license.txt which is included in the root      }
{  folder of the package.                                                   }
{                                                                           }
{  This library is distributed in the hope that it will be useful,          }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of           }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        }
{  Lesser General Public License for more details.                          }
{                                                                           }
{***************************************************************************}
Unit psc_reg;

Interface
{$I psc_defines.inc}

Procedure Register;

{----------------------------------------------------------}

Implementation

Uses
  psc_listbox,
  psc_button_color,
  psc_edit,
  psc_edit_color,
  psc_expreval,
  psc_edit_parts,
  psc_edit_date,
  psc_fltbox,
  psc_calendar,
  psc_fontbox,
  psc_colorbox,
  psc_wrapper,
  psc_procs,
  psc_const,
  psc_calculator,

  myla_system,
  myla_interfaces,
  myla_parser,

{$IFDEF D6}
  DesignIntf,
  DesignEditors,
  VCLEditors,
  Types,
{$ELSE}
  DsgnIntf,
{$ENDIF}

{$IFDEF DPSOFT}
  ToolsAPI,
{$ENDIF}

  sysutils,

  Forms,
  Menus,
  controls,
  winapi.windows,
  typinfo,
  db,
  classes

  ;

Const
  SPSCCompPage_Internal = 'FilterBox2';//don't resource
  SPSCCompPage_Public = 'FilterBox';//don't resource

{----------------------------------------------------------}

{$IFDEF DPSOFT}

type
  TPSCIDEMessageRec = record
    AFileName: String;
    AMsgStr: String;
    AToolName: string;
    AKind: TOTAMessageKind;
    ALineNumber: Integer;
    AColumnNumber: Integer;
    AParent: Pointer;
  end;

{-----------------------------------------}

function PSCAddIDEMessage(const AMessage: TPSCIDEMessageRec): Pointer;
var
  MyMessageServ: IOTAMessageServices;
begin
  if Supports(BorlandIDEServices, IOTAMessageServices, MyMessageServ) then
    begin
      with AMessage do
        MyMessageServ.AddCompilerMessage(
          AFileName,
          AMsgStr,
          AToolName,
          AKind,
          ALineNumber,
          AColumnNumber,
          AParent,
          Result);
      MyMessageServ.ShowMessageView(nil);
    end;
end;

{-----------------------------------------}

procedure PSCClearAllIDEMessages;
var
  MyMessageServ: IOTAMessageServices;
begin
  if Supports(BorlandIDEServices, IOTAMessageServices, MyMessageServ) then
    MyMessageServ.ClearAllMessages;
end;

{-----------------------------------------}

function GetIDEMainMenu: TMainMenu;
var
  MyNTAServices40: INTAServices40;
begin
  if Supports(BorlandIDEServices, INTAServices40, MyNTAServices40) then
    Result:= MyNTAServices40.MainMenu
  else
    Result:= nil;
end;
{-----------------------------------------}

{$ENDIF}

{----------------------------------------------------------}

Type
  TPSCPickListProp = Class(TStringProperty)
  public
    Function GetAttributes: TPropertyAttributes; override;
    Procedure GetValues(Proc: TGetStrProc); override;
    Procedure GetValueList(const List: IPSCStrings); virtual;
  End;

  TPSCThemeNameEdit = class(TPSCPickListProp)
  public
    procedure GetValueList(const List: IPSCStrings); override;
    function  GetAttributes: TPropertyAttributes; override;
  end;

  TPSCDataFieldProperty = Class(TPSCPickListProp)
  public
    Function GetInstance: TPersistent; virtual;
    Function GetDataSourcePropName: String; virtual;
    Function GetDataSet: TDataSet; virtual;
    Procedure GetValueList(const List: IPSCStrings); override;
  End;

  TPSCDataSetFieldProperty = Class(TPSCDataFieldProperty)
  public
    Function GetDataSetPropName: String; virtual;
    Function GetDataSet: TDataSet; override;
  End;

  TPSCLookupDataSetFieldProperty = Class(TPSCDataSetFieldProperty)
  public
    Function GetDataSetPropName: String; override;
  End;

  TPSCSomeFieldProperty = Class(TPSCDataFieldProperty)
  public
    Procedure GetValueList(const List: IPSCStrings); override;
    Function IsFieldOk(Field: TField): boolean; virtual;
  End;

  TPSCDateFieldProperty = Class(TPSCSomeFieldProperty)
  public
    Function IsFieldOk(Field: TField): boolean; override;
  End;

  TPSCRecordSetDataFieldProperty = Class(TPSCDataSetFieldProperty)
  public
    Function GetInstance: TPersistent; override;
  End;

  TPSCFltBoxEditor = Class(TComponentEditor)
  public
    Procedure ExecuteVerb(Index: Integer); override;
    Function GetVerb(Index: Integer): String; override;
    Function GetVerbCount: Integer; override;
  End;

  TPSCListItemPickProp = Class(TPSCPickListProp)
  public
    Function GetFltBld: TPSCCustomFltBld;
  End;

  TPSCListItemDataFieldProp = Class(TPSCListItemPickProp)
  public
    Procedure GetValueList(const List: IPSCStrings); override;
  End;

  TPSCListItemTemplCatProp = Class(TPSCPickListProp)
  public
    Procedure GetValueList(const List: IPSCStrings); override;
  End;

  TPSCListItemTemplateProp = Class(TPSCListItemPickProp)
  public
    Function GetAttributes: TPropertyAttributes; override;
    Procedure GetValueList(const List: IPSCStrings); override;
  End;

  TPSCListItemOperProp = Class(TPSCListItemPickProp)
  public
    Procedure GetValueList(const List: IPSCStrings); override;
  End;

  TPSCListItemTableNameProp = Class(TPSCListItemPickProp)
  public
    Procedure GetValueList(const List: IPSCStrings); override;
  End;

  TPSCFieldNameMaskProp = Class(TPSCPickListProp)
  public
    Procedure GetValueList(const List: IPSCStrings); override;
  End;

  TPSCCustomSectionEditor = class(TComponentEditor)
  private
    FSlot: TPSCGraphicItem;
    FColorBoxEditor: TComponentEditor;

    procedure GetSlots;
  public
    destructor Destroy; override;

    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TPSCColorBoxEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{-------------------------------------------}

Function TPSCPickListProp.GetAttributes: TPropertyAttributes;
Begin
  Result := Inherited GetAttributes + [paMultiSelect,paValueList,paSortList];
End;

{-------------------------------------------}

Procedure TPSCPickListProp.GetValueList(const List: IPSCStrings);
Begin
End;

{-------------------------------------------}

Procedure TPSCPickListProp.GetValues(Proc: TGetStrProc);
Var
  Temp: IPSCStrings;
Begin
  Temp := PSCCreateStringList;
  GetValueList(Temp);
  PSCStringsToProc(Temp,Proc);
End;

{------------------------------------------------}

Function TPSCDataFieldProperty.GetDataSourcePropName: String;
Begin
  Result := 'DataSource'; //don't resource
End;

{------------------------------------------------}

Function TPSCDataFieldProperty.GetDataSet: TDataSet;
Var
  Instance: TPersistent;
  PropInfo: PPropInfo;
  DataSource: TDataSource;
Begin
  Result := Nil;
  Instance := GetInstance;
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo,GetDataSourcePropName);
  If (PropInfo <> Nil) And (PropInfo^.PropType^.Kind = tkClass) Then
    Begin
      DataSource := GetObjectProp(Instance,PropInfo) As TDataSource;
      If (DataSource <> Nil) Then
        Result := DataSource.DataSet;
    End;
End;

{------------------------------------------------}

Procedure TPSCDataFieldProperty.GetValueList(const List: IPSCStrings);
Var
  DataSet: TDataSet;
  MyList:TStrings;
Begin
  DataSet := GetDataSet;
  If DataSet <> Nil Then
  begin
    MyList:=TStringList.Create;
    DataSet.GetFieldNames(MyList);
    List.Assign(PSCCreateStringsAdapter(MyList,ioOwned));
  end;
End;

{------------------------------------------------------------------}

Function TPSCDataFieldProperty.GetInstance: TPersistent;
Begin
  Result := TPersistent(GetComponent(0));
End;

{---------------------------------}

Procedure TPSCFltBoxEditor.ExecuteVerb(Index: Integer);
Var
  FltBld: TPSCCustomFltBld;

  procedure AddFieldsFromDataSetToFields(ADataSet:TDataSet;
    AFields:TPSCFltBoxFields);
  var
    i:Integer;
    MyField:TPSCFltBoxField;
  begin
    If ADataSet=nil then
      exit;
    With ADataSet do
      for i:=0 to Fields.Count-1 do
      begin
        MyField:=TPSCFltBoxField(AFields.Add);
        MyField.Assign(Fields[i]);
      end;
  end;

Begin
  If Component Is TPSCCustomFltBld Then
    FltBld := TPSCCustomFltBld(Component)
  Else
  If Component Is TPSCCustomFltBox Then
    begin
      FltBld := TPSCCustomFltBox(Component).FilterSource;
      If FltBld=nil then
      begin
        PSCShowMessage(PSCConsts.ErrNoFilterSource);
        exit;
      end;
    end
  Else
    exit;

  If (FltBld.Fields.Count = 0) Then
    Begin
      If (FltBld.DataSet = Nil) Then
        Begin
          PSCShowMessage(PSCConsts.ErrDataSetNilNoFields);
          exit;
        End
      Else
        If (FltBld.DataSet.FieldCount = 0) Then
          Begin
            PSCShowMessage(PSCConsts.ErrNoFields);
            exit;
          End;
    End;

  Case index Of
    0:
      If PSCEditFltBld(FltBld) Then
        Designer.Modified;
    1:
      Begin
        If FltBld.MemorySlots.CallMemoryDlg(True,FltBld.Items) Then
          Designer.Modified;
      End;
    2:
      Begin
        If FltBld.MemorySlots.CallMemoryDlg(False,FltBld.Items) Then
          Designer.Modified;
      End;
    3:
      AddFieldsFromDataSetToFields(FltBld.DataSet,FltBld.Fields);
    4:
      AddFieldsFromDataSetToFields(FltBld.DataSet,FltBld.FieldParams);
  End;
End;

{---------------------------------}

Function TPSCFltBoxEditor.GetVerb(Index: Integer): String;
Begin
  case Index of
    0:Result:=PSCConsts.PrmBoxEditor;
    1:Result:=PSCConsts.PrmBoxSaveToMem;
    2:Result:=PSCConsts.PrmBoxLoadFromMem;
    3:Result:=PSCConsts.AddAllFields;
    4:Result:=PSCConsts.AddAllFieldParams;
  else
    Result:='';
  end;
  Result := PSCRemoveCharSet(['&'],Result);
End;

{---------------------------------}

Function TPSCFltBoxEditor.GetVerbCount: Integer;
Begin
  Result := 5;
End;

{---------------------------------}

Function TPSCRecordSetDataFieldProperty.GetInstance: TPersistent;
Begin
  Result := TPSCRecordSetStyles(TPSCRecordSetStyle(
    GetComponent(0)).Collection).GetOwner;
End;

{---------------------------------}

Procedure TPSCFieldNameMaskProp.GetValueList(const List: IPSCStrings);
Begin
  With List Do
    Begin
      Add(SPSCStdFieldNameMask);
      Add(SPSCStdFieldNameMaskShort);
      Add(SPSCAdoFieldNameMask);
      Add(SPSCAdoFieldNameMaskShort);
      Add('%s'); //don't resource
      Add('%s.%s');//don't resource
    End;
End;

{---------------------------------}

Procedure TPSCListItemTableNameProp.GetValueList(const List: IPSCStrings);
Begin
  List.Assign(PSCCreateStringsAdapter(GetFltBld.TableNames));
End;

{---------------------------------}

Procedure TPSCListItemOperProp.GetValueList(const List: IPSCStrings);
Begin
  GetFltBld.GetOperations(List);
End;

{---------------------------------}

Function TPSCListItemTemplateProp.GetAttributes: TPropertyAttributes;
Begin
  Result := Inherited GetAttributes + [paValueList,paSortList];
End;

{---------------------------------}

Procedure TPSCListItemTemplateProp.GetValueList(const List: IPSCStrings);
Begin
  GetFltBld.GetFieldTemplates(TPSCFltItem(GetComponent(0)).DataField,List,False);
End;

{---------------------------------}

Procedure TPSCListItemTemplCatProp.GetValueList(const List: IPSCStrings);
Begin
  If GetComponent(0) Is TPSCFltBoxTemplate Then
    TPSCFltBoxTemplates(
      TPSCFltBoxTemplate(GetComponent(0)).Collection).EnumTemplCategories(List)
  Else
  If GetComponent(0) Is TPSCFltBoxField Then
    TPSCFltBoxField(GetComponent(0)).GetFltBld.Templates.EnumTemplCategories(List)
  Else
  If GetComponent(0) Is TPSCFltItem then
    TPSCFltItem(GetComponent(0)).GetFltBld.Templates.EnumTemplCategories(List);
End;

{---------------------------------}

Procedure TPSCListItemDataFieldProp.GetValueList(const List: IPSCStrings);
Begin
  GetFltBld.GetFieldsListEx(List,FT_UNK,False);
End;

{---------------------------------}

Function TPSCListItemPickProp.GetFltBld: TPSCCustomFltBld;
Begin
  If GetComponent(0) Is TPSCCustomFltBld Then
    Result := TPSCCustomFltBld(GetComponent(0))
  Else
  If GetComponent(0) Is TPSCFltItem Then
    Result := TPSCFltItem(GetComponent(0)).GetFltBld
  Else
  If GetComponent(0) Is TPSCFltBoxField Then
    Result := TPSCFltBoxField(GetComponent(0)).GetFltBld
  Else
  If GetComponent(0) Is TPSCOrderByItem Then
    Result := TPSCOrderByItem(GetComponent(0)).GetFltBld
  Else
    Result := Nil;
End;

{------------------------------------------------}

Function TPSCLookupDataSetFieldProperty.GetDataSetPropName: String;
Begin
  Result := 'LookupDataSet'; //don't resource
End;

{------------------------------------------------------------------}

Function TPSCDataSetFieldProperty.GetDataSetPropName: String;
Begin
  Result := 'DataSet'; //don't resource
End;

{------------------------------------------------------------------}

Function TPSCDataSetFieldProperty.GetDataSet: TDataSet;
Var
  Instance: TPersistent;
  PropInfo: PPropInfo;
Begin
  Instance := GetInstance;
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo,GetDataSetPropName);
  If (PropInfo <> Nil) And (PropInfo^.PropType^.Kind = tkClass) Then
    Result := GetObjectProp(Instance,PropInfo) As TDataSet
  Else
    Result := Nil;
End;

{--------------------------------------}

Function TPSCSomeFieldProperty.IsFieldOk(Field: TField): boolean;
Begin
  Result := True;
End;

{------------------------------------------------------------------}

Procedure TPSCSomeFieldProperty.GetValueList(const List: IPSCStrings);
Var
  Field: TField;
  i: Integer;
  ADataSet: TDataSet;
Begin
  Inherited;
  ADataSet := GetDataSet;
  If ADataSet <> Nil Then
    For i := List.Count - 1 Downto 0 Do
      Begin
        Field := ADataSet.FindField(List[i]);
        If Field <> Nil Then
          Begin
            If Not IsFieldOk(Field) Then
              List.Delete(i);
          End;
      End;
End;

{------------------------------------------------------------------}

Function TPSCDateFieldProperty.IsFieldOk(Field: TField): boolean;
Begin
  Result := (Field Is TDateTimeField) Or (Field Is TDateField)
    Or (Field Is TTimeField);
End;

{-------------------------------------}

destructor TPSCCustomSectionEditor.Destroy;
begin
  FColorBoxEditor.Free;
  inherited Destroy
end;

{-------------------------------------}

type
  TSectionAccess = class(TPSCCustomSectionControl);

procedure TPSCCustomSectionEditor.GetSlots;
var
  P: TPoint;
  Data: IPSCDataAccess;
begin
  FSlot:=nil;
  if Component is TPSCCustomSectionControl then
     with TSectionAccess(Component) do
     begin
       GetCursorPos(P);
       P:=ScreenToClient(P);
       Data:=(FSection as IPSCBoundsList).GetItemAt(P) as IPSCDataAccess;
       if (Data<>nil) and (Data.AsObject[PRM_OWNER]<>nil)
          then FSlot:=TPSCGraphicItem(Data.AsObject[PRM_OWNER])
          else FSlot:=nil   
     end
end;

{-------------------------------------}

procedure TPSCCustomSectionEditor.Edit;
begin
  GetSlots;
  if FSlot<>nil then Designer.SelectComponent(FSlot)
end;

{-------------------------------------}

procedure TPSCCustomSectionEditor.ExecuteVerb(Index: Integer);
begin
  if (Index>0) and (FSlot=nil) then Index:=Index+1;
  if Component is TPSCCustomSectionControl then
    with TSectionAccess(Component) do
      case Index of
        0: begin
             FSlot:=TPSCGraphicItem(TPSCCustomSection(FSection.GetInstance).Add);
             Designer.SelectComponent(FSlot);
             Designer.Modified;
           end;
        1: begin
             FSlot.Free;
             FSlot:=nil;
             Designer.Modified;
           end
        else
           if FColorBoxEditor<>nil
              then FColorBoxEditor.ExecuteVerb(Index-3)
      end
end;

{-------------------------------------}

function TPSCCustomSectionEditor.GetVerb(Index: Integer): string;
begin
  if (Index>0) and (FSlot=nil) then Index:=Index+1;
  case Index of
    0: Result:=PSCConsts.AddSlot;
    1: Result:=PSCConsts.DeleteSlot;
    2: Result:='-'
  else
    if FColorBoxEditor<>nil then
      Result:=FColorBoxEditor.GetVerb(Index-3)
  end
end;

{-------------------------------------}

function TPSCCustomSectionEditor.GetVerbCount: Integer;
begin
  GetSlots;
  if FSlot=nil then Result:=1 else Result:=2;
  if Component.GetParentComponent is TPSCCustomColorBox then
  begin
    FColorBoxEditor:=TPSCColorBoxEditor.Create(
                        Component.GetParentComponent, Designer
                     );
    Result:=Result+FColorBoxEditor.GetVerbCount+1
  end
end;

{-------------------------------------}

procedure TPSCColorBoxEditor.ExecuteVerb(Index: Integer);
var
  ClassType: TControlClass;
begin
  case Index of
    0: ClassType:=TPSCDefaultSectionControl;
    1: ClassType:=TPSCColorSectionControl;
    2: ClassType:=TPSCButtonSectionControl
    else
       Exit
  end;
  Designer.CreateComponent(ClassType, Component, 0, 0, 0, 0)
end;

{-------------------------------------}

function TPSCColorBoxEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result:=PSCConsts.DefaultSection;
    1: Result:=PSCConsts.ColorSection;
    2: Result:=PSCConsts.ButtonSection;
  end
end;

{-------------------------------------}

function TPSCColorBoxEditor.GetVerbCount: Integer;
begin
  Result:=3;
end;

{-------------------------------------------------------------}

procedure TPSCThemeNameEdit.GetValueList(const List: IPSCStrings);
begin
  PSCGetThemeRegister.EnumThemeNames(List);
end;

{-------------------------------------------------------------}

function  TPSCThemeNameEdit.GetAttributes: TPropertyAttributes;
begin
  result := inherited GetAttributes;
  result := result + [paValueList]-[paMultiSelect];
end;

{------------------------------------------------}

Procedure Register;
Begin
//BeginSkipConst
  RegisterPropertyEditor(TypeInfo(string),TPSCCustomButtonControl,'ThemeName',TPSCThemeNameEdit);

  RegisterPropertyEditor(TypeInfo(TPSCColor),TPersistent,'',TColorProperty);

//------------------------------
//------------------------------
//------------------------------

  RegisterComponentEditor(TPSCFltBld,TPSCFltBoxEditor);
  RegisterComponentEditor(TPSCFltDlg,TPSCFltBoxEditor);
  RegisterComponentEditor(TPSCFltBox,TPSCFltBoxEditor);

  RegisterPropertyEditor(TypeInfo(String),TPSCRecordSetStyle,
    'DataField',TPSCRecordSetDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(String),TPSCFltBld,
    'FieldNameMask',TPSCFieldNameMaskProp);
  RegisterPropertyEditor(TypeInfo(String),TPSCFltItem,
    'DataField',TPSCListItemDataFieldProp);
  RegisterPropertyEditor(TypeInfo(String),TPSCFltItem,
    'Template',TPSCListItemTemplateProp);
  RegisterPropertyEditor(TypeInfo(String),TPSCFltItem,
    'Operation',TPSCListItemOperProp);
  RegisterPropertyEditor(TypeInfo(String),TPSCFltBld,
    'SQLHeadOper',TPSCListItemOperProp);
  RegisterPropertyEditor(TypeInfo(String),TPSCFltBoxField,
    'LookupKeyField',TPSCLookupDataSetFieldProperty);
  RegisterPropertyEditor(TypeInfo(String),TPSCFltBoxField,
    'TableName',TPSCListItemTableNameProp);
  RegisterPropertyEditor(TypeInfo(String),TPSCFltBoxField,
    'DataField',TPSCListItemDataFieldProp);
  RegisterPropertyEditor(TypeInfo(String),TPSCFltBoxField,
    'SQLFieldName',TPSCListItemDataFieldProp);
  RegisterPropertyEditor(TypeInfo(String),TPSCOrderByItem,
    'DataField',TPSCListItemDataFieldProp);
  RegisterPropertyEditor(TypeInfo(String),TPSCFltBoxField,
    'LookupDisplayField',TPSCLookupDataSetFieldProperty);
  RegisterPropertyEditor(TypeInfo(String),TPSCListParam,
    'LookupKeyField',TPSCLookupDataSetFieldProperty);
  RegisterPropertyEditor(TypeInfo(String),TPSCListParam,
    'LookupDisplayField',TPSCLookupDataSetFieldProperty);
  RegisterPropertyEditor(TypeInfo(String),TPSCFltBoxField,
    'TemplCat',TPSCListItemTemplCatProp);
  RegisterPropertyEditor(TypeInfo(String),TPSCFltItem,
    'TemplCat',TPSCListItemTemplCatProp);
  RegisterPropertyEditor(TypeInfo(String),TPSCFltBoxTemplate,
    'Category',TPSCListItemTemplCatProp);

//------------------------------
//------------------------------
//------------------------------

  RegisterPropertyEditor(TypeInfo(String),TPSCDateEdit,
    'DataField',TPSCDateFieldProperty);
  RegisterPropertyEditor(TypeInfo(String),TPSCDBCalendar,
    'BitsFieldName',TPSCDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(String),TPSCDBCalendar,
    'StartFieldName',TPSCDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(String),TPSCDBCalendar,
    'FinishFieldName',TPSCDataFieldProperty);

//------------------------------
//------------------------------
//------------------------------

  RegisterComponentEditor(TPSCCustomSectionControl, TPSCCustomSectionEditor);
  RegisterComponentEditor(TPSCCustomColorBox, TPSCColorBoxEditor);
  Classes.RegisterClass(TPSCColorSectionControl);
  Classes.RegisterClass(TPSCDefaultSectionControl);
  Classes.RegisterClass(TPSCButtonSectionControl);

//------------------------------
//------------------------------
//------------------------------

  RegisterComponents(SPSCCompPage_Public, [
    TPSCDBCalendar,
    TPSCCalendarPanel,
    TPSCMonthsBox,
    TPSCFltDlg,
    TPSCGridColors,
    TPSCExprEval,

    TPSCCalcEdit,
    TPSCDateEdit,
    TPSCDateTimeUpDown,
    TPSCColorEdit,
    TPSCFontEdit,
    TPSCEdit,

    TPSCCalendar,
    TPSCColorBox,
    TPSCFontBox,
    TPSCColorButton,

    TPSCFltBox,
    TPSCListBox,
    TPSCFltBld,
    TPSCCalculator
  ]);
{
  RegisterComponents(SPSCCompPage_Internal, [
    TPSCCheckBox,
    TPSCRadioButton,
    TPSCButton,
    TPSCSpeedButton,
    TPSCPanel,
    TPSCScrollBar,
    TPSCLabel,
    TPSCGroupBox,
    TPSCPageControl
  ]);
}
//EndSkipConst
end;

End.

