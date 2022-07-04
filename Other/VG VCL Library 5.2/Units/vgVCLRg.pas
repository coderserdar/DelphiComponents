{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         VCL registration                              }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgVCLRg;

interface
uses DsgnIntf;

type
{ TListNamesProperty }
  TListNamesProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TFilenameProperty }
  TFilenameProperty = class(TStringProperty)
  protected
    function GetFilter: string; virtual;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure Register;

implementation
uses vgVCLRes, SysUtils, vgUtils, vgVCLUtl, Classes, vgSystem, vgTools, Explorer,
  Controls, vgNLS, ExplCtrl, ExplFile, vgStndrt, vgCtrls, vgItems, ExplEdit, ExplShl,
  Dialogs
  {$IFDEF _D3_}, vgMSScr{$ENDIF}
  {$IFDEF _D4_}, vgPropDs{$ENDIF};

{$R vgDVCL.dcr}

type
{ TExplorerFormClassListEditor }
  TExplorerFormClassListEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{$IFNDEF _D4_}
{ TVariantProperty }
  TVariantProperty = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TVariantProperty }
  TVarTypeProperty = class(TPropertyEditor)
  private
    FVariantProperty: TVariantProperty;
    constructor Create(ADesigner: TFormDesigner; APropList: Pointer; APropCount: Integer; AVariantProperty: TVariantProperty);
  public
    destructor Destroy; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;
{$ENDIF}

{ TListNamesProperty }
function TListNamesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

{ TExplorerFormClassListEditor }
function TExplorerFormClassListEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TExplorerFormClassListEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to ExplorerFormClassList.Count - 1 do
    Proc(ExplorerFormClassList.Items[I].GetClassType.ClassName);
end;

{$IFNDEF _D4_}
{ TVariantProperty }
function TVariantProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties];
end;

type
  TPropertyEditorHack = class
    FDesigner: TFormDesigner;
    FPropList: PInstPropList;
    FPropCount: Integer;
  end;

procedure TVariantProperty.GetProperties(Proc: TGetPropEditProc);
begin
  Proc(TVarTypeProperty.Create(Designer, TPropertyEditorHack(Self).FPropList, PropCount, Self));
end;

function TVariantProperty.GetValue: string;
var
  V: Variant;
begin
  V := GetVarValue;
  case VarType(V) of
    varEmpty:
      Result := '';
    varNull:
      Result := 'Null';
    varByte, varSmallint, varInteger:
      Result := IntToStr(V);
    varSingle, varDouble, varCurrency:
      Result := FloatToStr(V);
    varDate:
      Result := DateTimeToStr(V);
    varString, varOleStr:
      Result := V;
    varBoolean:
      if V then Result := 'True' else Result := 'False';
  else
    Result := '(Variant)';
  end;
end;

procedure TVariantProperty.SetValue(const Value: string);
var
  V: Variant;
begin
  V := GetVarValue;
  if VarIsEmpty(V) or VarIsNull(V) then V := '';
  SetVarValue(VarAsType(Value, VarType(V)));
end;

{ TVarTypeProperty }
var
  VarTypes: TStrings;

constructor TVarTypeProperty.Create(ADesigner: TFormDesigner; APropList: Pointer; APropCount: Integer; AVariantProperty: TVariantProperty);
begin
  TPropertyEditorHack(Self).FDesigner := ADesigner;
  TPropertyEditorHack(Self).FPropList := APropList;
  TPropertyEditorHack(Self).FPropCount := APropCount;
  FVariantProperty := AVariantProperty;
end;

destructor TVarTypeProperty.Destroy;
begin
end;

function TVarTypeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TVarTypeProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to VarTypes.Count - 1 do Proc(VarTypes[I]);
end;

function TVarTypeProperty.GetValue: string;
var
  VarTyp, I: Integer;
begin
  VarTyp := VarType(FVariantProperty.GetVarValue);
  I := VarTypes.IndexOfObject(Pointer(VarTyp));
  try
    Result := VarTypes[I];
  except
    Result := '';
  end;
end;

procedure TVarTypeProperty.SetValue(const Value: string);
var
  I: Integer;
  VT: Integer;
  V: Variant;
begin
  I := VarTypes.IndexOf(Value);
  if I >= 0 then
  begin
    V := FVariantProperty.GetVarValue;
    VT := Integer(VarTypes.Objects[I]);
    try
      case VT of
        varEmpty:
          FVariantProperty.SetVarValue(Unassigned);
        varNull:
          FVariantProperty.SetVarValue(Null);
        varByte, varSmallint, varInteger:
          try
            FVariantProperty.SetVarValue(VarAsType(V, VT));
          except
            FVariantProperty.SetVarValue(VarAsType(0, VT));
          end;
        varSingle, varDouble, varCurrency, varDate:
          try
            FVariantProperty.SetVarValue(VarAsType(V, VT));
          except
            FVariantProperty.SetVarValue(VarAsType(0.0, VT));
          end;
        varBoolean:
          try
            if AnsiCompareText(V, 'True') = 0 then
              FVariantProperty.SetVarValue(True) else
              FVariantProperty.SetVarValue(False);
          except
            FVariantProperty.SetVarValue(False);
          end;
        varString, varOleStr:
          FVariantProperty.SetVarValue(VarAsType(V, VT));
      end;
    except
      FVariantProperty.SetVarValue(VarAsType('', VT))
    end;
  end;
end;
{$ENDIF}

{ TFilenameProperty }
function TFilenameProperty.GetFilter: string;
begin
  Result := '*.*';
end;

procedure TFilenameProperty.Edit;
var
  FileOpen: TOpenDialog;
begin
  FileOpen := TOpenDialog.Create(nil);
  try
    FileOpen.FileName := GetValue;
    FileOpen.InitialDir := ExtractFilePath(FileOpen.FileName);
    if (ExtractFileName(FileOpen.FileName) = '') then
      FileOpen.FileName := '';
    FileOpen.Filter := GetFilter;
    FileOpen.Options := FileOpen.Options + [ofHideReadOnly];
    if FileOpen.Execute then SetValue(FileOpen.FileName);
  finally
    FileOpen.Free;
  end;
end;

function TFilenameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

{ TvgTranslatorEditor }
type
  TvgTranslatorEditor = class (TComponentEditor)
  private
    procedure CreateLanguageFile;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TvgTranslatorEditor }
procedure TvgTranslatorEditor.CreateLanguageFile;
var
  FileOpen: TOpenDialog;
begin
  FileOpen := TOpenDialog.Create(nil);
  try
    FileOpen.FileName := TvgTranslator(Component).LanguageFile;
    FileOpen.InitialDir := ExtractFilePath(FileOpen.FileName);
    if (ExtractFileName(FileOpen.FileName) = '') then
      FileOpen.FileName := '';
    FileOpen.Filter := 'Ini files (*.ini)|*.ini|All files (*.*)|*.*';
    FileOpen.Options := FileOpen.Options + [ofHideReadOnly];
    if FileOpen.Execute then
    begin
      AppSetCursor(crHourglass);
      try
        TvgTranslator(Component).CreateLanguageFile(FileOpen.FileName, True);
      finally
        AppRestoreCursor;
      end;
    end;
  finally
    FileOpen.Free;
  end;
end;


procedure TvgTranslatorEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: CreateLanguageFile;
  end;
end;

function TvgTranslatorEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Create &language file...'
  end;
end;

function TvgTranslatorEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure Register;
begin
  RegisterComponents(LoadStr(SRegSystem), [
    TvgThread, TBroadcaster,
{$IFDEF _D4_}
    TAppIniFile, TPropStorage,
{$ENDIF}
    TDateTimeStorage, TCurrencyStorage, TvgTranslator,
    TMoneyString, TFormLoader
{$IFDEF _D3_},
    TMSSCScript
{$ENDIF}
  ]);

{$IFDEF _D3_}
  RegisterCustomModule(TCustomSheetForm, TCustomModule);
{$ENDIF}

{$IFDEF _D4_}
  RegisterComponentEditor(TPropStorage, TPropStorageEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TPropStorage, 'StoredProps', TStoredPropsProperty);
{$ENDIF}

  RegisterComponents(LoadStr(SRegControls), [
    TvgSplitter, TClickEdit, TJustifyEdit, TTitler, TvgPageControl, TvgNoteBook,
    TvgTabSet, TvgPanel, TvgLabel, TvgListBox, TvgComboBox, TvgTreeView, TvgListView,
    TvgTreeViewCombo
    ]);

  RegisterNoIcon([TvgTabSheet]);

  RegisterComponents(LoadStr(SRegExplorer), [
    TExplorerRootNode, TExplorerSource, TExplorerTreeView, TExplorerListView
    {$IFDEF _D3_}, TExplorerListBox {$ENDIF}, TExplorerTreeCombo]);
  RegisterNoIcon([TExplorerNode]);

  RegisterExplorerNodesClasses([
    TExplorerFolderNode, TExplorerActionNode, TExplorerSeparatorNode,
    TExplorerStringsNode, TExplorerFormNode,
    TExplorerDrivesNode {$IFDEF _D3_}, TExplorerShellFolderNode{$ENDIF}]);

  RegisterComponentEditor(TItemList, TItemsEditor);
  RegisterComponentEditor(TExplorerNodes, TExplorerNodesEditor);
  RegisterComponentEditor(TvgTranslator, TvgTranslatorEditor);
  RegisterPropertyEditor(TypeInfo(TClassName), TExplorerFormNode, '', TExplorerFormClassListEditor);
  RegisterPropertyEditor(TypeInfo(TFileName), TComponent, '', TFileNameProperty);

{$IFNDEF _D4_}
  RegisterPropertyEditor(TypeInfo(Variant), nil, '', TVariantProperty);
{$ENDIF}
end;

initialization
{$IFNDEF _D4_}
  VarTypes := TStringList.Create;

  VarTypes.AddObject('Empty', Pointer(varEmpty));
  VarTypes.AddObject('Null', Pointer(varNull));
  VarTypes.AddObject('Smallint', Pointer(varSmallInt));
  VarTypes.AddObject('Integer', Pointer(varInteger));
  VarTypes.AddObject('Single', Pointer(varSingle));
  VarTypes.AddObject('Double', Pointer(varDouble));
  VarTypes.AddObject('Currency', Pointer(varCurrency));
  VarTypes.AddObject('Date', Pointer(varDate));
  VarTypes.AddObject('OleStr', Pointer(varOleStr));
  VarTypes.AddObject('Dispatch', Pointer(varDispatch));
  VarTypes.AddObject('Error', Pointer(varError));
  VarTypes.AddObject('Boolean', Pointer(varBoolean));
  VarTypes.AddObject('Variant', Pointer(varVariant));
  VarTypes.AddObject('Unknown', Pointer(varUnknown));
  VarTypes.AddObject('Byte', Pointer(varByte));
  VarTypes.AddObject('String', Pointer(varString));
  VarTypes.AddObject('TypeMask', Pointer(varTypeMask));
  VarTypes.AddObject('Array', Pointer(varArray));
  VarTypes.AddObject('ByRef', Pointer(varByRef));
{$ENDIF}

finalization
{$IFNDEF _D4_}
  VarTypes.Free;
{$ENDIF}

end.
