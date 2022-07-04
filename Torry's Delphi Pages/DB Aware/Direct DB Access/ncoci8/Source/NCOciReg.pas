{*******************************************************}
{File:      NCOciReg.PAS                                }
{Revision:  0.05.03 / 04.11.2001                        }
{Comment:   NC OCI8 VCL: registration unit              }
{Copyright: (c) 1999-2001, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, darefiev@da-soft.com        }
{*******************************************************}
{$I NCOciDef.inc}

unit NCOciReg;

interface

    procedure Register;

implementation

{$R NCO8.DCR}

Uses Windows, SysUtils, Classes, DB, Registry, NCOciDB, NCOCIMsg, NCSQLMon,
     NCOciUpdateSQL, NCOciPLSQLGenSetupDlg, NCOciCompNamer, NCOciCompNamerSetupDlg,
     ExptIntf, ToolIntf
{$IFDEF OCI_D3}
     , Menus
{$ENDIF}
{$IFDEF OCI_BDE}
     , NCOciBDE
{$ENDIF}
{$IFDEF OCI_D6}
     , DesignIntf, DesignEditors, PropertyCategories, ToolsAPI
    {$IFNDEF OCI_BCB}
        , DBReg
    {$ENDIF}
{$ELSE}
     , DsgnIntf
{$ENDIF}
     ;           

{ --------------------------------------------------------------------------- }
{ --------------------------------------------------------------------------- }

Type
    TOCIListProperty = class(TStringProperty)
        FObjKind: TOCIObjKind;
        FParentObj: String;
        function GetAttributes: TPropertyAttributes; override;
        procedure GetValues(Proc: TGetStrProc); override;
    end;

    TOCIServerNameProperty = class(TOCIListProperty)
        procedure GetValues(Proc: TGetStrProc); override;
    end;

    TOCIDatabaseNameProperty = class(TOCIListProperty)
        procedure GetValues(Proc: TGetStrProc); override;
    end;

    TOCIStoredProcNameProperty = class(TOCIListProperty)
        procedure GetValues(Proc: TGetStrProc); override;
    end;

    TOCIOPackageNameProperty = class(TOCIListProperty)
        procedure GetValues(Proc: TGetStrProc); override;
    end;

    TOCIOProcedureNameProperty = class(TOCIListProperty)
        procedure GetValues(Proc: TGetStrProc); override;
    end;

{$IFNDEF OCI_D6}
{$IFDEF OCI_D5}
    TOCILoginCategory = class(TPropertyCategory)
    public
        class function Name: String; override;
        class function Description: String; override;
    end;

    TOCIResourceCategory = class(TPropertyCategory)
    public
        class function Name: String; override;
        class function Description: String; override;
    end;

    TOCITransactCategory = class(TPropertyCategory)
    public
        class function Name: String; override;
        class function Description: String; override;
    end;
{$ENDIF}
{$ENDIF}

    TOCISequenceNameProperty = class(TOCIListProperty)
        procedure GetValues(Proc: TGetStrProc); override;
    end;

{ --------------------------------------------------------------------------- }
{ --------------------------------------------------------------------------- }

function TOCIListProperty.GetAttributes: TPropertyAttributes;
begin
    Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TOCIListProperty.GetValues(Proc: TGetStrProc);
var
    List: TStringList;
    i: Integer;
    db: String;
    comp: TPersistent;
begin
    List := TStringList.Create;
    try
        comp := GetComponent(0);
        db := '';
        if not (FObjKind in [okService, okDatabase]) then
            if comp is TOCICustomDatabase then
                db := TOCIDatabase(comp).DatabaseName
            else if comp is TOCIDataSet then
                db := TOCIDataSet(comp).DatabaseName
            else if comp is TOCITransactionManager then
                db := TOCITransactionManager(comp).DatabaseName
            else if comp is TOCISequence then
                db := TOCISequence(comp).DatabaseName;
        TOCICustomDatabase.GetObjectsList(db, List, FParentObj, FObjKind, True);
        for i := 0 to List.Count - 1 do
            Proc(List[i]);
    finally
        List.Free;
    end;
end;

procedure TOCIServerNameProperty.GetValues(Proc: TGetStrProc);
begin
    FObjKind := okService;
    FParentObj := '';
    inherited GetValues(Proc);
end;

procedure TOCIDatabaseNameProperty.GetValues(Proc: TGetStrProc);
begin
    FObjKind := okDatabase;
    FParentObj := '';
    inherited GetValues(Proc);
end;

procedure TOCIStoredProcNameProperty.GetValues(Proc: TGetStrProc);
begin
    FObjKind := okProc;
    FParentObj := '';
    inherited GetValues(Proc);
end;

procedure TOCIOPackageNameProperty.GetValues(Proc: TGetStrProc);
begin
    FObjKind := okPackage;
    FParentObj := '';
    inherited GetValues(Proc);
end;

procedure TOCIOProcedureNameProperty.GetValues(Proc: TGetStrProc);
begin
    FObjKind := okProc;
    FParentObj := (GetComponent(0) as TOCIStoredProc).OPackageName;
    inherited GetValues(Proc);
end;

procedure TOCISequenceNameProperty.GetValues(Proc: TGetStrProc);
begin
    FObjKind := okSeqs;
    FParentObj := (GetComponent(0) as TOCISequence).SequenceName;
    inherited GetValues(Proc);
end;

{ --------------------------------------------------------------------------- }
{ --------------------------------------------------------------------------- }

{$IFNDEF OCI_D6}
{$DEFINE OCI_DSEDIT}
var
    DataSetEditorClass: TComponentEditorClass;

{$WARNINGS OFF}
procedure CaptureDataSetEditorClass;
var
    ds: TDataSet;
    dsEditor: TComponentEditor;
begin
    ds := nil;
    dsEditor := nil;
    try
        ds := TDataSet.Create(nil);
        dsEditor := GetComponentEditor(ds, nil);
        DataSetEditorClass := TComponentEditorClass(dsEditor.ClassType);
    finally
        dsEditor.Free;
        ds.Free;
    end;
end;
{$WARNINGS ON}

type
    TOCIStatementDataSetEditor = class(TComponentEditor)
    private
        FPrevEditor: TComponentEditor;
    public
        constructor Create(AComponent: TComponent; ADesigner:
          {$IFDEF OCI_D4} IFormDesigner {$ELSE} TFormDesigner {$ENDIF}); override;
        destructor Destroy; override;
        procedure ExecuteVerb(Index: Integer); override;
        function GetVerb(Index: Integer): string; override;
        function GetVerbCount: Integer; override;
        procedure Edit; override;
    end;

constructor TOCIStatementDataSetEditor.Create(AComponent: TComponent; ADesigner:
    {$IFDEF OCI_D4} IFormDesigner {$ELSE} TFormDesigner {$ENDIF});
begin
    inherited Create(AComponent, ADesigner);
    FPrevEditor := DataSetEditorClass.Create(AComponent, ADesigner);
end;

destructor TOCIStatementDataSetEditor.Destroy;
begin
    FPrevEditor.Free;
    inherited Destroy;
end;

procedure TOCIStatementDataSetEditor.ExecuteVerb(Index: Integer);
begin
    if Index < FPrevEditor.GetVerbCount then
        FPrevEditor.ExecuteVerb(Index)
    else begin
        Index := Index - FPrevEditor.GetVerbCount;
        case Index of
        0: TOCIQuery(Component).ExecSQL;
        end;
    end;
end;

function TOCIStatementDataSetEditor.GetVerb(Index: Integer): string;
begin
    if Index < FPrevEditor.GetVerbCount then
        Result := FPrevEditor.GetVerb(Index)
    else begin
        Index := Index - FPrevEditor.GetVerbCount;
        case Index of
        0: Result := '&Execute';
        end;
    end;
end;

function TOCIStatementDataSetEditor.GetVerbCount: Integer;
begin
    Result := FPrevEditor.GetVerbCount + 1;
end;

procedure TOCIStatementDataSetEditor.Edit;
begin
    ExecuteVerb(0);
end;

{$ELSE}
    {$IFNDEF OCI_BCB}
{$DEFINE OCI_DSEDIT}
type
    TOCIStatementDataSetEditor = class(TDataSetEditor)
    public
        procedure ExecuteVerb(Index: Integer); override;
        function GetVerb(Index: Integer): string; override;
        function GetVerbCount: Integer; override;
    end;

procedure TOCIStatementDataSetEditor.ExecuteVerb(Index: Integer);
begin
    if Index < inherited GetVerbCount then
        inherited ExecuteVerb(Index)
    else
        case Index - inherited GetVerbCount of
        0: TOCIQuery(Component).ExecSQL;
        end;
end;

function TOCIStatementDataSetEditor.GetVerb(Index: Integer): string;
begin
    if Index < inherited GetVerbCount then
        Result := inherited GetVerb(Index)
    else
        case Index - inherited GetVerbCount of
        0: Result := '&Execute';
        end;
end;

function TOCIStatementDataSetEditor.GetVerbCount: Integer;
begin
    Result := inherited GetVerbCount + 1;
end;
    {$ENDIF}
{$ENDIF}

{ --------------------------------------------------------------------------- }
{ --------------------------------------------------------------------------- }

type
    TOCIParamNameProperty = class(TStringProperty)
        function GetAttributes: TPropertyAttributes; override;
        procedure GetValues(Proc: TGetStrProc); override;
    end;

function TOCIParamNameProperty.GetAttributes: TPropertyAttributes;
begin
    Result := [paValueList, paSortList];
end;

procedure TOCIParamNameProperty.GetValues(Proc: TGetStrProc);
var
    i: Integer;
begin
    with GetComponent(0) as TOCINestedDataSet do
        if ParamDataSet <> nil then
            for i := 0 to ParamDataSet.ParamCount - 1 do
                Proc(ParamDataSet.Params[i].Name);
end;

{ --------------------------------------------------------------------------- }
{ --------------------------------------------------------------------------- }

{$IFNDEF OCI_D6}
{$IFDEF OCI_D5}
class function TOCILoginCategory.Name: String;
begin
    Result := SOCILoginCategoryName;
end;

class function TOCILoginCategory.Description: String;
begin
    Result := SOCILoginCategoryDesc;
end;

class function TOCIResourceCategory.Name: String;
begin
    Result := SOCIResourceCategoryName;
end;

class function TOCIResourceCategory.Description: String;
begin
    Result := SOCIResourceCategoryDesc;
end;

class function TOCITransactCategory.Name: String;
begin
    Result := SOCITransactCategoryName;
end;

class function TOCITransactCategory.Description: String;
begin
    Result := SOCITransactCategoryDesc;
end;
{$ENDIF}
{$ENDIF}

{ --------------------------------------------------------------------------- }
{ --------------------------------------------------------------------------- }

type
    TOciCompNameProperty = class(TStringProperty)
    public
        function GetAttributes: TPropertyAttributes; override;
        function GetEditLimit: Integer; override;
        procedure Edit; override;
        destructor Destroy; override;
        procedure Initialize; override;
        procedure SetValue(const Value: string); override;
        function GetValue: string; override;
    end;

var
    CompNamer: TOciCompNamer = nil;
    CompNameProp: TOciCompNameProperty = nil;

function TOciCompNameProperty.GetAttributes: TPropertyAttributes;
begin
    Result := [paMultiSelect, paDialog];
end;

function TOciCompNameProperty.GetEditLimit: Integer;
begin
    Result := 63;
end;

procedure TOciCompNameProperty.Edit;
var
    i: Integer;
begin
    i := 0;
    while True do
    try
        CompNamer.Component := GetComponent(i) as TComponent;
        CompNamer.Rename;
        Inc(i);
    except
        Designer.Modified;
        Break;
    end;
end;

function TOciCompNameProperty.GetValue: string;
begin
    if PropCount > 1 then
        Result := SCNCallExpertNow
    else
        Result := inherited GetValue;
end;

procedure TOciCompNameProperty.SetValue(const Value: string);
begin
    if PropCount > 1 then
        raise Exception.Create(SCNUseExpertForRename)
    else
        inherited SetValue(Value);
end;

procedure TOciCompNameProperty.Initialize;
begin
    inherited Initialize;
    CompNameProp := Self;
end;

destructor TOciCompNameProperty.Destroy;
begin
    CompNameProp := nil;
    inherited Destroy;
end;

{ --------------------------------------------------------------------------- }
{ --------------------------------------------------------------------------- }
{$IFDEF OCI_D6}
type
    TOciWizard = class(TNotifierObject, IOTAWIzard)
    private
        FNCOCI8Menu: TMenuItem;
        FGenMenuItem: TMenuItem;
        FSplitMenuItem: TMenuItem;
        FCompNamerSetupMenuItem: TMenuItem;
        FCompNamerRenameMenuItem: TMenuItem;
        FNamingHotKey, FOptHotKey: TShortCut;
        procedure GenClick(Sender: TObject);
        function BaseRegKey: String;
        procedure SaveToReg;
        procedure LoadFromReg;
        procedure SetNamingHotKey(AValue: TShortCut);
        procedure SetOptHotKey(AValue: TShortCut);
        procedure RenameComponentsClick(Sender: TObject);
        procedure RenameSetupClick(Sender: TObject);
    public
        constructor Create;
        destructor Destroy; override;
        function GetIDString: string;
        function GetName: string;
        function GetState: TWizardState;
        procedure Execute;
        property NamingHotKey: TShortCut read FNamingHotKey write SetNamingHotKey;
        property OptHotKey: TShortCut read FOptHotKey write SetOptHotKey;
    end;

function TOciWizard.GetIDString: string;
begin
    Result := 'DA-SOFT.NCOCI8.Wizard';
end;

function TOciWizard.GetName: string;
begin
    Result := 'NCOCI8 Wizard';
end;

function TOciWizard.GetState: TWizardState;
begin
    Result := [wsEnabled];
end;

procedure TOciWizard.Execute;
begin
end;

constructor TOciWizard.Create;
var
    oNTAServ: INTAServices;
    oMainMenu: TMainMenu;
    i: Integer;

begin
    inherited Create;
    FNCOCI8Menu := nil;
    FGenMenuItem := nil;

    oNTAServ := BorlandIDEServices as INTAServices;
    oMainMenu := oNTAServ.MainMenu;

    for i := 0 to oMainMenu.Items.Count - 1 do
      if oMainMenu.Items[i].Name = 'ToolsMenu' then begin
        FNCOCI8Menu := NewItem('NCOCI8&', 0, False, True, nil, -1, 'NCOCI8Menu');

        FGenMenuItem := NewItem('PL/SQL &Generator ...', 0, False, True, GenClick, -1, 'NCOCI8PLSQLGenItem');
        FSplitMenuItem := NewLine;
        FCompNamerSetupMenuItem := NewItem('Component namer setup ...', 0, False, True, RenameSetupClick, -1, 'NCOCI8CompNamerSetupItem');
        FCompNamerRenameMenuItem := NewItem('Rename component(s)', 0, False, True, RenameComponentsClick, -1, 'NCOCI8CompNamerRenameItem');

        FNCOCI8Menu.Add([FGenMenuItem, FSplitMenuItem, FCompNamerSetupMenuItem, FCompNamerRenameMenuItem]);
        oMainMenu.Items.Insert(i, FNCOCI8Menu);

        Break;
      end;
      
    LoadFromReg;
end;

destructor TOciWizard.Destroy;
begin
    if FGenMenuItem <> nil then
        FGenMenuItem.Free;
    if FSplitMenuItem <> nil then
        FSplitMenuItem.Free;
    if FCompNamerSetupMenuItem <> nil then
        FCompNamerSetupMenuItem.Free;
    if FCompNamerRenameMenuItem <> nil then
        FCompNamerRenameMenuItem.Free;
    if FNCOCI8Menu <> nil then
        FNCOCI8Menu.Free;
    inherited Destroy;
end;

function TOciWizard.BaseRegKey: String;
begin
    Result := '\Software\Nica-Com\NCVCL\OciCompNamer';
end;

procedure TOciWizard.SaveToReg;
var
    reg: TRegistry;
begin
    reg := TRegistry.Create;
    try
        reg.OpenKey(BaseRegKey, True);
        CompNamer.SaveToReg(reg);
        reg.WriteInteger('NamingHotKey', NamingHotKey);
        reg.WriteInteger('OptionsHotKey', OptHotKey);
    finally
        reg.Free;
    end;
end;

procedure TOciWizard.LoadFromReg;
var
    reg: TRegistry;
begin
    reg := TRegistry.Create;
    try
        if not reg.OpenKey(BaseRegKey, False) then
            Exit;
        CompNamer.LoadFromReg(reg);
        NamingHotKey := reg.ReadInteger('NamingHotKey');
        OptHotKey := reg.ReadInteger('OptionsHotKey');
    finally
        reg.Free;
    end;
end;

procedure TOciWizard.SetNamingHotKey(AValue: TShortCut);
begin
    FNamingHotKey := AValue;
    FCompNamerRenameMenuItem.ShortCut := AValue;
end;

procedure TOciWizard.SetOptHotKey(AValue: TShortCut);
begin
    FOptHotKey := AValue;
    FCompNamerSetupMenuItem.ShortCut := AValue;
end;

procedure TOciWizard.GenClick(Sender: TObject);
begin
    with TOciPLSQLGenSetupFrm.Create(nil) do
    try
        Execute;
    finally
        Free;
    end;
end;

procedure TOciWizard.RenameComponentsClick(Sender: TObject);
begin
    if CompNameProp <> nil then
        CompNameProp.Edit;
end;

procedure TOciWizard.RenameSetupClick(Sender: TObject);
var
    namHK, optHK: TShortCut;
begin
    namHK := NamingHotKey;
    optHK := OptHotKey;
    if TOciCompNamerSetupFrm.Execute(CompNamer, namHK, optHK) then begin
      NamingHotKey := namHK;
      OptHotKey := optHK;
      SaveToReg;
    end;
end;

{ --------------------------------------------------------------------------- }
{ --------------------------------------------------------------------------- }

{$ELSE}
type
    TOciPLSQLGenExpert = class(TIExpert)
    private
        FNCOCI8Menu: TIMenuItemIntf;
        FGenMenuItem: TIMenuItemIntf;
        procedure GenClick(Sender: TIMenuItemIntf);
    public
        constructor Create;
        destructor Destroy; override;
        function GetName: string; override;
        function GetStyle: TExpertStyle; override;
        function GetIDString: string; override;
    end;

    TOciCompNamerExpert = class(TIExpert)
    private
        FNamingHotKey, FOptHotKey: TShortCut;
        FNCOCI8Menu: TIMenuItemIntf;
        FCompNamerSepMenuItem: TIMenuItemIntf;
        FCompNamerSetupMenuItem: TIMenuItemIntf;
        FCompNamerRenameMenuItem: TIMenuItemIntf;
        procedure RenameComponentsClick(Sender: TIMenuItemIntf);
        procedure RenameSetupClick(Sender: TIMenuItemIntf);
        procedure SaveToReg;
        procedure LoadFromReg;
        function BaseRegKey: String;
        procedure SetNamingHotKey(AValue: TShortCut);
        procedure SetOptHotKey(AValue: TShortCut);
    public
        constructor Create;
        destructor Destroy; override;
        function GetName: string; override;
        function GetStyle: TExpertStyle; override;
        function GetIDString: string; override;
        property NamingHotKey: TShortCut read FNamingHotKey write SetNamingHotKey;
        property OptHotKey: TShortCut read FOptHotKey write SetOptHotKey;
    end;

function TOciPLSQLGenExpert.GetName: string;
begin
    Result := 'NCOCI8 PL/SQL Wrapper Objects Generator';
end;

function TOciPLSQLGenExpert.GetStyle: TExpertStyle;
begin
    Result := esAddIn;
end;

function TOciPLSQLGenExpert.GetIDString: string;
begin
    Result := 'DA-SOFT.OciPLSQLGenExpert';
end;

constructor TOciPLSQLGenExpert.Create;
var
    MainMenu: TIMainMenuIntf;
    MenuItems: TIMenuItemIntf;
    ToolsMenu: TIMenuItemIntf;
begin
    inherited Create;
    FNCOCI8Menu := nil;
    FGenMenuItem := nil;
    ToolsMenu := nil;
    MenuItems := nil;
    MainMenu := ToolServices.GetMainMenu;
    if MainMenu <> nil then
    try
        MenuItems := MainMenu.GetMenuItems;
        if MenuItems <> nil then begin
            FNCOCI8Menu := MainMenu.FindMenuItem('NCOCI8Menu');
            if FNCOCI8Menu = nil then begin
                ToolsMenu := MainMenu.FindMenuItem('ToolsMenu');
                if ToolsMenu <> nil then
                    FNCOCI8Menu := MenuItems.InsertItem(ToolsMenu.GetIndex,
                        'NCOCI&8', 'NCOCI8Menu', '', 0, 0, 0, [mfVisible, mfEnabled], nil);
            end;
            FGenMenuItem := MainMenu.FindMenuItem('NCOCI8PLSQLGenItem');
            if FGenMenuItem = nil then
                FGenMenuItem := FNCOCI8Menu.InsertItem(0, 'PL/SQL &Generator ...',
                    'NCOCI8PLSQLGenItem', '', 0, 0, 0, [mfVisible, mfEnabled], GenClick);
        end;
    finally
        if ToolsMenu <> nil then
            ToolsMenu.Free;
        if MenuItems <> nil then
            MenuItems.Free;
        if MainMenu <> nil then
            MainMenu.Free;
    end;
end;

destructor TOciPLSQLGenExpert.Destroy;
begin
    if FGenMenuItem <> nil then
        FGenMenuItem.Free;
    if FNCOCI8Menu <> nil then
        FNCOCI8Menu.Free;
    inherited Destroy;
end;

procedure TOciPLSQLGenExpert.GenClick(Sender: TIMenuItemIntf);
begin
    with TOciPLSQLGenSetupFrm.Create(nil) do
    try
        Execute;
    finally
        Free;
    end;
end;

function TOciCompNamerExpert.GetName: string;
begin
    Result := 'NCOCI8 Component Naming Expert';
end;

function TOciCompNamerExpert.GetStyle: TExpertStyle;
begin
    Result := esAddIn;
end;

function TOciCompNamerExpert.GetIDString: string;
begin
    Result := 'DA-SOFT.OciCompNamerExp';
end;

function TOciCompNamerExpert.BaseRegKey: String;
begin
    Result := '\Software\Nica-Com\NCVCL\OciCompNamer';
end;

procedure TOciCompNamerExpert.SaveToReg;
var
    reg: TRegistry;
begin
    reg := TRegistry.Create;
    try
        reg.OpenKey(BaseRegKey, True);
        CompNamer.SaveToReg(reg);
        reg.WriteInteger('NamingHotKey', NamingHotKey);
        reg.WriteInteger('OptionsHotKey', OptHotKey);
    finally
        reg.Free;
    end;
end;

procedure TOciCompNamerExpert.LoadFromReg;
var
    reg: TRegistry;
begin
    reg := TRegistry.Create;
    try
        if not reg.OpenKey(BaseRegKey, False) then
            Exit;
        CompNamer.LoadFromReg(reg);
        NamingHotKey := reg.ReadInteger('NamingHotKey');
        OptHotKey := reg.ReadInteger('OptionsHotKey');
    finally
        reg.Free;
    end;
end;

procedure TOciCompNamerExpert.SetNamingHotKey(AValue: TShortCut);
begin
    FNamingHotKey := AValue;
    FCompNamerRenameMenuItem.SetShortCut(AValue);
end;

procedure TOciCompNamerExpert.SetOptHotKey(AValue: TShortCut);
begin
    FOptHotKey := AValue;
    FCompNamerSetupMenuItem.SetShortCut(AValue);
end;

constructor TOciCompNamerExpert.Create;
var
    MainMenu: TIMainMenuIntf;
    MenuItems: TIMenuItemIntf;
    ToolsMenu: TIMenuItemIntf;
begin
    inherited Create;
    FNCOCI8Menu := nil;
    FCompNamerSepMenuItem := nil;
    FCompNamerSetupMenuItem := nil;
    FCompNamerRenameMenuItem := nil;
    ToolsMenu := nil;
    MenuItems := nil;
    MainMenu := ToolServices.GetMainMenu;
    if MainMenu <> nil then
    try
        MenuItems := MainMenu.GetMenuItems;
        if MenuItems <> nil then begin
            FNCOCI8Menu := MainMenu.FindMenuItem('NCOCI8Menu');
            if FNCOCI8Menu = nil then begin
                ToolsMenu := MainMenu.FindMenuItem('ToolsMenu');
                if ToolsMenu <> nil then
                    FNCOCI8Menu := MenuItems.InsertItem(ToolsMenu.GetIndex,
                        'NCOCI&8', 'NCOCI8Menu', '', 0, 0, 0, [mfVisible, mfEnabled], nil);
            end;
            FCompNamerSetupMenuItem := MainMenu.FindMenuItem('NCOCI8CompNamerSetupItem');
            if FCompNamerSetupMenuItem = nil then begin
                FCompNamerSepMenuItem := FNCOCI8Menu.InsertItem(0, '-',
                    'NCOCI8CompNamerSepItem', '', 0, 0, 0, [mfVisible, mfEnabled], nil);
                FCompNamerSetupMenuItem := FNCOCI8Menu.InsertItem(0, 'Component namer setup ...',
                    'NCOCI8CompNamerSetupItem', '', 0, 0, 0, [mfVisible, mfEnabled], RenameSetupClick);
                FCompNamerRenameMenuItem := FNCOCI8Menu.InsertItem(0, 'Rename component(s)',
                    'NCOCI8CompNamerRenameItem', '', 0, 0, 0, [mfVisible, mfEnabled], RenameComponentsClick);
            end
            else begin
              FCompNamerRenameMenuItem := MainMenu.FindMenuItem('NCOCI8CompNamerRenameItem');
            end;
        end;
        LoadFromReg;
    finally
        if ToolsMenu <> nil then
            ToolsMenu.Free;
        if MenuItems <> nil then
            MenuItems.Free;
        if MainMenu <> nil then
            MainMenu.Free;
    end;
end;

destructor TOciCompNamerExpert.Destroy;
begin
    if FNCOCI8Menu <> nil then
        FNCOCI8Menu.Free;
    if FCompNamerSepMenuItem <> nil then
        FCompNamerSepMenuItem.Free;
    if FCompNamerSetupMenuItem <> nil then
        FCompNamerSetupMenuItem.Free;
    if FCompNamerRenameMenuItem <> nil then
        FCompNamerRenameMenuItem.Free;
    inherited Destroy;
end;

procedure TOciCompNamerExpert.RenameComponentsClick(Sender: TIMenuItemIntf);
begin
    if CompNameProp <> nil then
        CompNameProp.Edit;
end;

procedure TOciCompNamerExpert.RenameSetupClick(Sender: TIMenuItemIntf);
var
    namHK, optHK: TShortCut;
begin
    namHK := NamingHotKey;
    optHK := OptHotKey;
    if TOciCompNamerSetupFrm.Execute(CompNamer, namHK, optHK) then begin
      NamingHotKey := namHK;
      OptHotKey := optHK;
      SaveToReg;
    end;
end;
{$ENDIF}

{ --------------------------------------------------------------------------- }
{ --------------------------------------------------------------------------- }

procedure Register;
begin
{$IFNDEF OCI_D6}
    CaptureDataSetEditorClass;
{$ENDIF}    

    RegisterPropertyEditor(TypeInfo(String), TOCICustomDatabase, 'ServerName',
        TOCIServerNameProperty);
    RegisterPropertyEditor(TypeInfo(String), TOCIDataSet, 'DatabaseName',
        TOCIDatabaseNameProperty);
    RegisterPropertyEditor(TypeInfo(String), TOCITransactionManager, 'DatabaseName',
        TOCIDatabaseNameProperty);
    RegisterPropertyEditor(TypeInfo(String), TOCIStoredProc, 'StoredProcName',
        TOCIStoredProcNameProperty);
    RegisterPropertyEditor(TypeInfo(String), TOCIStoredProc, 'OPackageName',
        TOCIOPackageNameProperty);
    RegisterPropertyEditor(TypeInfo(String), TOCIStoredProc, 'OProcedureName',
        TOCIOProcedureNameProperty);
    RegisterPropertyEditor(TypeInfo(String), TOCISequence, 'DatabaseName',
        TOCIDatabaseNameProperty);
    RegisterPropertyEditor(TypeInfo(String), TOCISequence, 'SequenceName',
        TOCISequenceNameProperty);
    RegisterPropertyEditor(TypeInfo(String), TOCINestedDataSet, 'ParamName',
        TOCIParamNameProperty);

{$IFDEF OCI_DSEDIT}
    RegisterComponentEditor(TOCIQuery, TOCIStatementDataSetEditor);
    RegisterComponentEditor(TOCIStoredProc, TOCIStatementDataSetEditor);
{$ENDIF}    

{$IFDEF OCI_D5}
    RegisterPropertiesInCategory(
        {$IFDEF OCI_D6} sActionCategoryName {$ELSE} TActionCategory {$ENDIF},
        ['Enabled']);

    RegisterPropertiesInCategory(
        {$IFDEF OCI_D6} SOCITransactCategoryName {$ELSE} TOCITransactCategory {$ENDIF},
        TOCICustomDatabase,
        ['AutoCommit', 'Trans*', 'BeforeCommit', 'AfterCommit',
         'BeforeRollback', 'AfterRollback', 'BeforeStartTransaction',
         'AfterStartTransaction']);
    RegisterPropertiesInCategory(
        {$IFDEF OCI_D6} SOCITransactCategoryName {$ELSE} TOCITransactCategory {$ENDIF},
        TOCIDataSet,
        ['TransactionManager']);
    RegisterPropertiesInCategory(
        {$IFDEF OCI_D6} SOCITransactCategoryName {$ELSE} TOCITransactCategory {$ENDIF},
        TOCITransactionManager,
        ['*']);

    RegisterPropertiesInCategory(
        {$IFDEF OCI_D6} sDatabaseCategoryName {$ELSE} TDatabaseCategory {$ENDIF},
        TOCICustomDatabase,
        ['AuthentMode', 'Connect*', 'DataBaseName', 'Default*',
         'InitModes', 'KeepConnection', 'Login*', 'NonBlockingMode',
         'Password', 'ServerName', 'UserName', 'SilentMode',
         'After*', 'Before*', 'On*', 'CachedUpdates']);
    RegisterPropertiesInCategory(
        {$IFDEF OCI_D6} sDatabaseCategoryName {$ELSE} TDatabaseCategory {$ENDIF},
        TOCIDataSet,
        ['Active', 'DatabaseName', 'DataFormat', 'FetchParams',
         'After*', 'Before*', 'On*', 'UpdateMode', 'RecordCountMode']);
    RegisterPropertiesInCategory(
        {$IFDEF OCI_D6} sDatabaseCategoryName {$ELSE} TDatabaseCategory {$ENDIF},
        TOCICustomQuery,
        ['MacroCheck', 'Macros', 'Prepared', 'Unidirectional']);
    RegisterPropertiesInCategory(
        {$IFDEF OCI_D6} sDatabaseCategoryName {$ELSE} TDatabaseCategory {$ENDIF},
        TOCIStoredProc,
        ['StoredProcName', 'OPackageName', 'OProcedureName',
         'Overload']);
    RegisterPropertiesInCategory(
        {$IFDEF OCI_D6} sDatabaseCategoryName {$ELSE} TDatabaseCategory {$ENDIF},
        TOCITransactionManager,
        ['DatabaseName']);
    RegisterPropertiesInCategory(
        {$IFDEF OCI_D6} sDatabaseCategoryName {$ELSE} TDatabaseCategory {$ENDIF},
        TOCISequence,
        ['SequenceName']);

    RegisterPropertiesInCategory(
        {$IFDEF OCI_D6} sVisualCategoryName {$ELSE} TVisualCategory {$ENDIF},
        TOCICustomDatabase,
        ['Login*', 'SilentMode', 'Show*']);

    RegisterPropertiesInCategory(
        {$IFDEF OCI_D6} sInputCategoryName {$ELSE} TInputCategory {$ENDIF},
        TOCIDataSet,
        ['AllowedOperations']);

    RegisterPropertiesInCategory(
        {$IFDEF OCI_D6} SOCILoginCategoryName {$ELSE} TOCILoginCategory {$ENDIF},
        TOCICustomDatabase,
        ['AuthentMode', 'Connect*', 'Login*', 'Password', 'ServerName',
         'UserName', 'OnLogin', 'OnChangePassword']);

    RegisterPropertiesInCategory(
        {$IFDEF OCI_D6} SOCIResourceCategoryName {$ELSE} TOCIResourceCategory {$ENDIF},
        TOCICustomDatabase,
        ['AutoCommit', 'Connected', 'KeepConnection', 'DefaultFetchParams',
         'InitModes', 'NonBlockingMode', '*Connect', 'Max*']);
    RegisterPropertiesInCategory(
        {$IFDEF OCI_D6} SOCIResourceCategoryName {$ELSE} TOCIResourceCategory {$ENDIF},
        TOCIDataSet,
        ['Active', 'FetchParams', '*Open', '*Close', 'Disconnectable']);
    RegisterPropertiesInCategory(
        {$IFDEF OCI_D6} SOCIResourceCategoryName {$ELSE} TOCIResourceCategory {$ENDIF},
        TOCIStatementDataSet,
        ['Prepared', '*Prepare']);
    RegisterPropertiesInCategory(
        {$IFDEF OCI_D6} SOCIResourceCategoryName {$ELSE} TOCIResourceCategory {$ENDIF},
        TOCICustomQuery,
        ['ParamCheck', 'MacroCheck']);

    RegisterPropertiesInCategory(
        {$IFDEF OCI_D6} sLinkageCategoryName {$ELSE} TLinkageCategory {$ENDIF},
        TOCINestedDataSet,
        ['ParamName']);
{$ENDIF}

    RegisterComponents('Data Access', [TOCIDatabase, TOCIQuery, TOCIStoredProc,
        TNCSQLMonitorClient, TOCIUpdateSQL, TOCISequence, TOCINestedDataSet]);
    RegisterComponents('NCOCI8 Advanced', [TOCITransactionManager,
        {$IFDEF OCI_BDE} TOCIBDEDatabase, {$ENDIF} TOCIImpHndlDatabase]);

    RegisterPropertyEditor(TypeInfo(TComponentName), TComponent, 'Name',
        TOciCompNameProperty);
{$IFDEF OCI_D6}
    RegisterPackageWizard(TOciWizard.Create as IOTAWizard);
{$ELSE}
{$WARNINGS OFF}
    RegisterLibraryExpert(TOciPLSQLGenExpert.Create);
    RegisterLibraryExpert(TOciCompNamerExpert.Create);
{$WARNINGS ON}
{$ENDIF}
end;

initialization

    CompNameProp := nil;
    CompNamer := TOciCompNamer.Create(nil);

finalization

    CompNamer.Free;

end.
