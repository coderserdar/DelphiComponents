{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: Translator.pas,v 1.71 2003/04/17 14:20:59 laa Exp $ }

{-----------------------------------------------------------------------------
  Translator       The Polycon TTranslator component for translating form
                   properties and code strings. See more on
                   http://www.polycon.fi/translator

  What             TTranslator
                   TTranslatorClient
                   TTranslatorRowStorage
                   IEditableTranslatedStrings
                     + TTranslatedStrings

  Company          Polycon
  Authors          LGE
-----------------------------------------------------------------------------}

unit Translator;

interface
{$i common.inc}

uses
  TranslatorFields, ActnList, Classes, Dialogs, Forms, SysUtils, TypInfo,
  DataElements, RowList, Storages,
  StandardView, TranslatorInterfaces;

const
  ANYLANGUAGE : Integer = -1;

type
  TShowProperties = (spAllAdded, spTranslatedOnly);
  TLanguage = String;
  TAboutTranslator = class(TObject);

  TTranslator = class;
  TTranslatorClient = class;
  TTranslatedStrings = class;
  TOnTranslateEvent = procedure(Sender : TTranslatorClient; OldLanguage, NewLanguage : String) of object;

  TTranslatorRowStorage = class(TRowStorage)
  private
    FOwner : TTranslatedStrings;
  public
    property Owner : TTranslatedStrings read FOwner;
    constructor Create(Owner : TTranslatedStrings; DataTable : TDataTable);
  end;

  IEditableTranslatedStrings = interface
    function GetEditorWindow : TForm;
    procedure SetEditorWindow(AForm : TForm);
    property EditorWindow : TForm read GetEditorWindow write SetEditorWindow;

    function GetOwner : TTranslator;

    function GetLanguageCount : Integer;
    function GetLanguages(idx : Integer) : String;
    procedure SetLanguages(idx : Integer; LangName : String);
    function GetLanguageFields(idx : Integer) : TLangNameField;
    function CreateSubClassList(AClassName : string) : TStrings;
    procedure FillClassPropertyList(AClass : TClass; ExampleObject : TObject; AList : TStringList; InitialName : String = '');
    procedure GetTranslatedProperties(Component : TComponent; List : TDataRowList);

    property LanguageCount : Integer read GetLanguageCount;
    property Languages[idx : Integer] : String read GetLanguages write SetLanguages;
    property LanguageFields[idx : Integer] : TLangNameField read GetLanguageFields;
    function IndexOfLanguage(LangName : String) : Integer;

    function GetEditorStandardView : TSingletonStandardView;
    property EditorStandardView : TSingletonStandardView read GetEditorStandardView;

    function GetUnitList : TTranslatorRowStorage;
    property UnitList : TTranslatorRowStorage read GetUnitList;

    function GetTranslations : TTranslatorRowStorage;
    property Translations : TTranslatorRowStorage read GetTranslations;

    function GetStringTranslations : TTranslatorRowStorage;
    property StringTranslations : TTranslatorRowStorage read GetStringTranslations;

    function GetAddedProperties : TTranslatorRowStorage;
    property AddedProperties : TTranslatorRowStorage read GetAddedProperties;

    function GetShowProperties : TShowProperties;
    procedure SetShowProperties(Show : TShowProperties);
    property ShowProperties : TShowProperties read GetShowProperties write SetShowProperties;

    function GetCurrentEditorClientGUID : String;
    procedure SetCurrentEditorClientGUID(AGUID : String);
    property CurrentEditorClientGUID : String read GetCurrentEditorClientGUID write SetCurrentEditorClientGUID;

    procedure InsertLanguage(Index : Integer; LanguageName : String; CopyFromLanguage : Integer);
    procedure RemoveLanguage(Index : Integer);

    procedure FillComponentList(AList : TList);
    procedure UnRegisterClientByGUID(GUID : String);
    procedure RefreshLanguages;

{$ifdef D4_OR_HIGHER}
    procedure FillStrings(Client : TTranslatorClient; ALangIndex : Integer = -1 {ANYLANGUAGE});
{$else}
    procedure FillStrings(Client : TTranslatorClient; ALangIndex : Integer {ANYLANGUAGE});
{$endif D4_OR_HIGHER}

    procedure UpdateLanguage;

    procedure UpdateVisibleItems(DeleteUnused : Boolean);
  end;

  TTranslatedStrings = class(TObject, IEditableTranslatedStrings)
  private
    FTranslationTable : TDataTable;
    FStringTranslationTable : TDataTable;
    FStandardViewLangEditor : TSingletonStandardView;

    FOwner : TTranslator;
    FUnitList : TTranslatorRowStorage;
    FTranslations : TTranslatorRowStorage;
    FAddedProperties : TTranslatorRowStorage;
    FStringTranslations : TTranslatorRowStorage;
    FLanguages : TStringList;
    FShowProperties : TShowProperties;
    FCurrentEditorClientGUID : String;
    FEditorWindow : TForm;

{$ifdef D4_OR_HIGHER}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
{$else}
    function QueryInterface(const IID: TGUID; out Obj): Integer; stdcall;
{$endif D4_OR_HIGHER}
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function GetEditorWindow : TForm;
    procedure SetEditorWindow(AForm : TForm);

    function GetOwner : TTranslator;

    function CreateSubClassList(AClassName : string) : TStrings;
    procedure FillClassPropertyList(AClass : TClass; ExampleObject : TObject; AList : TStringList; InitialName : String = '');

    function GetUnitList : TTranslatorRowStorage;
    function GetTranslations : TTranslatorRowStorage;
    function GetStringTranslations : TTranslatorRowStorage;
    function GetAddedProperties : TTranslatorRowStorage;

    procedure CreateDataTables;
    procedure DestroyDataTables;

    function GetShowProperties : TShowProperties;
    procedure SetShowProperties(Show : TShowProperties);
    function GetCurrentEditorClientGUID : String;
    procedure SetCurrentEditorClientGUID(AGUID : String);

    function GetLanguageCount : Integer;
    function GetLanguages(idx : Integer) : String;
    procedure SetLanguages(idx : Integer; LangName : String);
    function GetLanguageFields(idx : Integer) : TLangNameField;

    function GetTranslationItemByName(Client : TTranslatorClient; Component : TComponent; PropertyName : String) : TDataRow;

    function SetProperty(Persistent : TPersistent; PropertyName, Value : String) : Boolean;
    function GetProperty(Persistent : TPersistent; PropertyName : String; out Value : String) : Boolean;

    function HasObjectProperty(ClassType : TClass; Persistent : TPersistent; PropertyName : String; out Reference) : Boolean;

    function GetEditorStandardView : TSingletonStandardView;

    constructor Create(Owner : TTranslator);

    property AddedProperties : TTranslatorRowStorage read FAddedProperties;

    property TranslationItemByName[Client : TTranslatorClient; Component : TComponent; PropertyName : String] : TDataRow read GetTranslationItemByName;

    function RemoveIndexesFromProperty(AProperty : String) : String;
    function PropertyTranslated(ClassType : TClass; AProperty : String) : Boolean;
    procedure RefreshLanguages;

{$ifdef D4_OR_HIGHER}
    procedure FillStrings(Client : TTranslatorClient; ALangIndex : Integer = -1 {ANYLANGUAGE});

    function GetString(const AProperty : String; LangIndex : Integer) : String; overload;
    function GetString(const AProperty : String; LangIndex : Integer; Variables : array of String) : String; overload;

    function HasTranslation(const AProperty : String; LangIndex : Integer; out Translation : String) : Boolean; overload;
    function HasTranslation(const AProperty : String; LangIndex : Integer; Variables : array of String; out Translation : String) : Boolean; overload;
{$else}
    procedure FillStrings(Client : TTranslatorClient; ALangIndex : Integer {ANYLANGUAGE});
    function GetString(const AProperty : String; LangIndex : Integer) : String;
    function GetStringOL(const AProperty : String; LangIndex : Integer; Variables : array of String) : String;
{$endif D4_OR_HIGHER}
    procedure UpdateVisibleItems(DeleteUnused : Boolean);

    procedure AddTranslation(Client : TTranslatorClient; Component : TComponent; PropertyName, PropertyValue : String; ALangIndex : Integer; DoTranslate : Boolean);
    function GetTranslation(Client : TTranslatorClient; Component : TComponent; PropertyName : String; ALangIndex : Integer; out Value : String) : Boolean;
    procedure GetTranslatedProperties(Component : TComponent; List : TDataRowList);

    procedure UpdateLanguage;

    procedure CheckLanguageNames;


    procedure FillComponentList(AList : TList);
    procedure UnRegisterClientByGUID(GUID : String);
  protected
    function HasActionForProp(Persistent : TPersistent; PropertyName : String; out Action : TAction) : Boolean;
    procedure UpdateActionItems(Sender : TAction);
{$ifdef D4_OR_HIGHER}
    function HasProperty(Persistent : TPersistent; PropertyName : String; out PropInfo : PPropInfo) : Boolean; overload;
    function HasProperty(Persistent : TPersistent; PropertyName : String) : Boolean; overload;
{$else}
    function HasProperty(Persistent : TPersistent; PropertyName : String; out PropInfo : PPropInfo) : Boolean;
    function HasPropertyOL(Persistent : TPersistent; PropertyName : String) : Boolean;
{$endif D4_OR_HIGHER}

    property Translations : TTranslatorRowStorage read GetTranslations;
    property UnitList : TTranslatorRowStorage read GetUnitList;

    property LanguageFields[idx : Integer] : TLangNameField read GetLanguageFields;
    function LanguageIndexByField(AField : TDataField) : Integer;
  public
    class function FormHasComponent(Form : TComponent; ComponentName : String; out Component : TComponent) : Boolean;

    procedure InsertLanguage(Index : Integer; LanguageName : String; CopyFromLanguage : Integer);
    procedure RemoveLanguage(Index : Integer);

    property Owner : TTranslator read GetOwner;
    property ShowProperties : TShowProperties read GetShowProperties write SetShowProperties;

    property LanguageCount : Integer read GetLanguageCount;
    property Languages[idx : Integer] : String read GetLanguages write SetLanguages;
    function IndexOfLanguage(LangName : String) : Integer;

    destructor Destroy; override;
  end;

  TTranslatorClient = class(TComponent, IStringTranslator)
  private
    FReadLanguage : String;
    FTranslateApplication : Boolean;
    FLangIndex : Integer;
    FServer : TTranslator;
    FGUID : String;
    FGUIDFinal : Boolean;

    FBeforeTranslate : TOnTranslateEvent;
    FAfterTranslated : TOnTranslateEvent;

    function GetAbout : TAboutTranslator;
    function GetActiveLanguage : String;
    procedure TranslateTo(LangName : String);
    procedure TranslateRecursive(LangName : String; Recursive : Boolean);
    procedure UpdateCurrentLanguageFromUI;
    procedure UpdateUIFromCurrentLanguage;
    procedure TranslateStrings(ALangIndex : Integer);
    function MainTranslator : TTranslatorClient;
    function GetUniqueIdentifier : String;
    procedure SetUniqueIdentifier(AGUID : String);
    function GetStrings : TTranslatedStrings;
  protected
    procedure SetServer(AServer : TTranslator); virtual;
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure TranslateOneProperty(ARow : TDataRow);
    procedure SetModified;
  public

    class function TranslatorVersion : String;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

{$ifdef D4_OR_HIGHER}
    function GetString(const AProperty : String) : String; overload;
    function GetString(const AProperty : String; Variables : array of String) : String; overload;
{$else}
    function GetString(const AProperty : String) : String;
    function GetStringOL(const AProperty : String; Variables : array of String) : String;
{$endif D4_OR_HIGHER}

    function HasTranslation(const AProperty : String; out Translation : String) : Boolean; overload;
    function HasTranslation(const AProperty : String; Variables : array of String; out Translation : String) : Boolean; overload;
  published
    property About : TAboutTranslator read GetAbout;
    property Server : TTranslator read FServer write SetServer;
    property Language : TLanguage read GetActiveLanguage write TranslateTo;
    property TranslateApplication : Boolean read FTranslateApplication write FTranslateApplication;
    property UniqueIdentifier : String read GetUniqueIdentifier write SetUniqueIdentifier;
    property Strings : TTranslatedStrings read GetStrings;

    property BeforeTranslate : TOnTranslateEvent read FBeforeTranslate write FBeforeTranslate;
    property AfterTranslated : TOnTranslateEvent read FAfterTranslated write FAfterTranslated;
  end;

  TTranslator = class(TTranslatorClient)
  private
    fIgnoreLanguageNameCheck : Boolean;
    FStrings : TTranslatedStrings;

    procedure LoadProperty(Reader : TReader);
    procedure StoreProperty(Writer : TWriter);

    procedure PerformVersionNotificationIfNeeded(AOldVersion : Integer);

    function UnRegisterClient(AClient : TTranslatorClient) : Boolean;
    procedure RegisterClient(AClient : TTranslatorClient);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetServer(AServer : TTranslator); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  function PerformVersionNotificationAtInstall : Boolean;
  procedure GetUnitAndForm(Component : TComponent; out UnitName : String; out Form : TComponent);

implementation

{$R Translator.res}

uses
  Controls, Registry, Windows, ComObj,
  DataTypes, DataType, Criteria;

const
  id_CURRENTVERSION : String = '2.61';
  id_STREAMINGVERSION : String = '2.06';

  id_VERSION = 'Version';
  id_LANGUAGES = 'Languages';
  id_CURRENTLANGUAGE = 'Currentlanguage';
  id_SHOWPROPERTIES = 'ShowProperties';
  id_UNCHANGED = 'UNCHANGED';

  FIRSTLANGPOS_TABLE : Integer = 4;
  FIRSTLANGPOS_VIEW : Integer = 5;
  MAXPROCLEVEL : Integer = 4;

var
  TranslatorList : TList;

type
  TTranslatedProperties = class
  private
    fTranslatedStrings : TTranslatedStrings;
    fList : TStringList;
  public
    constructor Create(TranslatedStrings : TTranslatedStrings);
    destructor Destroy; override;
    function GetList(Component : TComponent) : TDataRowList;
  end;

function PerformVersionNotificationAtInstall : Boolean;
var
  reg : TRegistry;
begin
  Result := True;
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;

    if reg.OpenKey('SOFTWARE\Polycon\Translator', True) then
    begin
      if not reg.ValueExists( 'VersionNotification' ) then
      begin

        if MessageDlg('Translator ' + id_CURRENTVERSION + #13#10#13#10 +
                   'You have now started to use the Translator v' + id_CURRENTVERSION+ '. ' +
                   'Please note that this software is licensed under the GNU General Public License. ' +
                   'You may use, modify and distribute the product as long as the entire source code ' +
                   'of your project is freely available as stated in the GPL license. For more ' +
                   'information about the licensing, please refer to ' + #13#10 +
                   'http://www.polycon.fi/translator/licensing.html' + #13#10#13#10 +
                   'By clicking "OK" now, you agree to all terms of the license.', mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
        begin
          reg.WriteInteger( 'VersionNotification', 2 );
        end
        else
          result := False;

      end;
    end;
  finally
    reg.Free;
  end;
end;

function ClassRefIsClass(AClass, ParentClass : TClass) : Boolean;
begin
  Result := (AClass <> nil) and
            ((AClass = ParentClass) or ClassRefIsClass(AClass.ClassParent, ParentClass));
end;

procedure GetUnitAndForm(Component : TComponent; out UnitName : String; out Form : TComponent);
begin
  Form := Component.Owner;
  if (Form = nil) or (Form = Application) then
    Form := Component;
  UnitName := GetTypeData(Form.ClassInfo)^.UnitName;
end;

{ TTranslatorRowStorage }

constructor TTranslatorRowStorage.Create(Owner : TTranslatedStrings; DataTable : TDataTable);
begin
  inherited Create(DataTable);
  FOwner := Owner;
end;

{ TTranslatedStrings }

function ModifyStorage(NewTable : TDataTable; OldStorage : TTranslatorRowStorage; OldField, NewField : TDataField) : TTranslatorRowStorage;
var
  RowList : TDataRowList;
  iRow : Integer;
  NewRow : TDataRow;
begin
  RowList := TDataRowList.Create;

  Result := TTranslatorRowStorage.Create(OldStorage.Owner, NewTable);
  OldStorage.GetRows(RowList, nil, gaReference);
  for iRow := 0 to RowList.Count - 1 do
  begin
    NewRow := TDataRow.Create(NewTable);
    NewRow.SetDefaultsFrom(RowList[iRow]);
    if OldField <> nil then
      NewRow[NewField] := NewRow[OldField];
    Result.PutRow(NewRow, paDontOverwriteKeys);
  end;

  RowList.Free;
  OldStorage.Free;
end;

{$ifdef D4_OR_HIGHER}
function TTranslatedStrings.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
const
  E_NOINTERFACE = HResult($80004002);
{$else}
function TTranslatedStrings.QueryInterface(const IID: TGUID; out Obj): Integer; stdcall;
const
  E_NOINTERFACE = $80004002;
{$endif D4_OR_HIGHER}
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

function TTranslatedStrings._AddRef: Integer; stdcall;
begin
  result := 1;
end;

function TTranslatedStrings._Release: Integer; stdcall;
begin
  result := 1;
end;

function TTranslatedStrings.GetUnitList : TTranslatorRowStorage;
begin
  Result := FUnitList;
end;

function TTranslatedStrings.GetTranslations : TTranslatorRowStorage;
begin
  Result := FTranslations;
end;

function TTranslatedStrings.GetStringTranslations : TTranslatorRowStorage;
begin
  Result := FStringTranslations;
end;

function TTranslatedStrings.GetAddedProperties : TTranslatorRowStorage;
begin
  Result := FAddedProperties;
end;

procedure TTranslatedStrings.UnRegisterClientByGUID(GUID : String);
var
  Crit : TCriteriaField;
begin
  Crit := TCriteriaField.Create(FieldGUID);
  Crit.AddString(GUID);
  FUnitList.DeleteRows(Crit);
  FTranslations.DeleteRows(Crit);
  Crit.Free;
end;

procedure TTranslatedStrings.FillComponentList(AList : TList);
var

  iComponent : Integer;
  Form : TComponent;
begin

  Form := Self.Owner.Owner;
  AList.Add(Form);
  for iComponent := 0 to Form.ComponentCount - 1 do
    AList.Add(Form.Components[iComponent]);

end;

procedure TTranslatedStrings.InsertLanguage(Index : Integer; LanguageName : String; CopyFromLanguage : Integer);
var
  NewLang, OldLang : TLangNameField;
  NewTranslationTable, NewStringTranslationTable : TDataTable;
  FirstLangField : TLangNameField;
begin
  if (not Owner.fIgnoreLanguageNameCheck) and (IndexOfLanguage(LanguageName) <> ANYLANGUAGE) then
    raise Exception.Create('Languagename ' + QuotedStr(LanguageName) + ' is already used!');

  if (Index < 0) or (Index > LanguageCount) then
    raise Exception.Create('Invalid index ' + IntToStr(Index) + '!');

  if CopyFromLanguage = ANYLANGUAGE then
    OldLang := nil
  else
    OldLang := LanguageFields[CopyFromLanguage];

  FirstLangField := LanguageFields[0];

  NewLang := TLangNameField.CreateOld;
  NewLang.SetDescription(LanguageName);
  FLanguages.InsertObject(Index, '', NewLang);

  NewTranslationTable := TDataTable.CreateCopy(FTranslationTable, nil, nil);
  NewTranslationTable.AddField(NewLang);
  NewTranslationTable.MoveField(NewLang, NewTranslationTable.IndexOfField(FirstLangField) + Index);

  NewStringTranslationTable := TDataTable.CreateCopy(FStringTranslationTable, nil, nil);
  NewStringTranslationTable.AddField(NewLang);
  NewStringTranslationTable.MoveField(NewLang, NewStringTranslationTable.IndexOfField(FirstLangField) + Index);

  FStandardViewLangEditor.AbstractPageView[0].CustomRowView[0].FieldList.Insert(FIRSTLANGPOS_VIEW + Index, NewLang.DisplayField);

  FTranslations := ModifyStorage(NewTranslationTable, FTranslations, OldLang, NewLang);
  FStringTranslations := ModifyStorage(NewStringTranslationTable, FStringTranslations, OldLang, NewLang);

  FTranslationTable.Free;
  FTranslationTable := NewTranslationTable;

  FStringTranslationTable.Free;
  FStringTranslationTable := NewStringTranslationTable;

  if Index <= Owner.FLangIndex then
    Inc(Owner.FLangIndex);
end;

procedure TTranslatedStrings.RemoveLanguage(Index : Integer);
var
  OldLang : TLangNameField;
  NewTranslationTable, NewStringTranslationTable : TDataTable;
begin
  if Index = ANYLANGUAGE then
    Exit;

  if Index = Owner.FLangIndex then
    raise Exception.Create('You are not allowed to remove the currently selected language!');

  OldLang := LanguageFields[Index];

  NewTranslationTable := TDataTable.CreateCopy(FTranslationTable, nil, nil);
  NewTranslationTable.RemoveDataField(OldLang);

  NewStringTranslationTable := TDataTable.CreateCopy(FStringTranslationTable, nil, nil);
  NewStringTranslationTable.RemoveDataField(OldLang);

  FStandardViewLangEditor.AbstractPageView[0].CustomRowView[0].FieldList.Remove(OldLang.DisplayField);

  FTranslations := ModifyStorage(NewTranslationTable, FTranslations, nil, nil);
  FStringTranslations := ModifyStorage(NewStringTranslationTable, FStringTranslations, nil, nil);

  FTranslationTable.Free;
  FTranslationTable := NewTranslationTable;

  FStringTranslationTable.Free;
  FStringTranslationTable := NewStringTranslationTable;

  OldLang.Free;
  FLanguages.Delete(Index);

  if Index <= Owner.FLangIndex  then
    Dec(Owner.FLangIndex);
end;

procedure TTranslatedStrings.CreateDataTables;

  procedure AddLangField(ALangField : TLangNameField);
  begin
    FTranslationTable.AddField(ALangField);
    FTranslationTable.MoveField(ALangField, FIRSTLANGPOS_TABLE + FLanguages.Count);

    FStringTranslationTable.AddField(ALangField);

    FStandardViewLangEditor.SingletonPageView[0].SingletonRowView[0].FieldList.Insert(FIRSTLANGPOS_VIEW + FLanguages.Count, ALangField.DisplayField);

    FLanguages.AddObject('', ALangField);
  end;

var
  LangField : TLangNameField;
  i : Integer;

  MT : TTranslatorClient;
begin
  FLanguages := TStringList.Create;

  FTranslationTable := TDataTable.CreateOld('', nil,
                         [FieldGUID, FieldComponent, FieldProperty],
                         [FieldClass, {langfields here, } FieldComponentPointer, FieldDoTranslate],
                         nil);
  FTranslationTable.SetDescriptions(['Translations']);
  FStringTranslationTable := TDataTable.CreateOld('', nil,
                         [FieldProperty],
                         [{langfields here, } nil],
                         nil);
  FStringTranslationTable.SetDescriptions(['StringTranslations']);

  FStandardViewLangEditor := TSingletonStandardView.Create(FTranslationTable, '', '');
  FStandardViewLangEditor.AddPageView(TSingletonPageView.Create(nil, ''));
  FStandardViewLangEditor.AddRowView(TSingletonRowView.Create([FieldUnit, FieldFormClass, FieldClass, FieldComponent, FieldProperty, {langfields here, } FieldDisplayDoTranslate], nil));
  FStandardViewLangEditor.SetReadOnly([FieldUnit, FieldFormClass, FieldClass, FieldComponent, FieldProperty], True);

  MT := Owner.MainTranslator;
  if (MT <> nil) and (MT.Strings <> nil) then
  begin
    for i := 0 to MT.Strings.LanguageCount - 1 do
    begin
      LangField := TLangNameField.CreateOld;
      LangField.CopyDescriptionsFrom(MT.Strings.LanguageFields[i]);
      LangField.DisplayField.CopyDescriptionsFrom(MT.Strings.LanguageFields[i]);
      AddLangField(LangField);
    end;
  end
  else
  begin
    // we create two languages by default...
    for i := 1 to 2 do
    begin
      LangField := TLangNameField.CreateOld;
      LangField.SetDescription('Language ' + IntToStr(i));
      AddLangField(LangField);
    end;
  end;

  FUnitList := TTranslatorRowStorage.Create(Self, UnitListTable);
  FTranslations := TTranslatorRowStorage.Create(Self, FTranslationTable);
  FAddedProperties := TTranslatorRowStorage.Create(Self, AddedPropertiesTable);
  FStringTranslations := TTranslatorRowStorage.Create(Self, FStringTranslationTable);
end;

procedure TTranslatedStrings.DestroyDataTables;
var
  i : Integer;
begin
  FreeAndNil(FTranslationTable);
  FreeAndNil(FStringTranslationTable);
  FreeAndNil(FStandardViewLangEditor);

  for i := 0 to LanguageCount - 1 do
    LanguageFields[i].Free;

  FLanguages.Free;
end;

function TTranslatedStrings.GetCurrentEditorClientGUID : String;
begin
  Result := FCurrentEditorClientGUID;
end;

procedure TTranslatedStrings.SetCurrentEditorClientGUID(AGUID : String);
begin
  FCurrentEditorClientGUID := AGUID;

  UpdateVisibleItems(False);
end;

function TTranslatedStrings.GetShowProperties : TShowProperties;
begin
  Result := FShowProperties;
end;

procedure TTranslatedStrings.SetShowProperties(Show : TShowProperties);
begin
  FShowProperties := Show;

  UpdateVisibleItems(False);
end;

function TTranslatedStrings.GetLanguageCount : Integer;
begin
  Result := FLanguages.Count;
end;

function TTranslatedStrings.GetLanguages(idx : Integer) : String;
begin
  Result := LanguageFields[idx].ShortDescription;
end;

procedure TTranslatedStrings.SetLanguages(idx : Integer; LangName : String);
begin
  if Languages[idx] = LangName then
    Exit
  else if (not Owner.fIgnoreLanguageNameCheck) and (IndexOfLanguage(LangName) <> ANYLANGUAGE) then
    raise Exception.Create('Languagename ' + QuotedStr(LangName) + ' is already used!')
  else
    LanguageFields[idx].SetDescription(LangName);
end;

function TTranslatedStrings.GetLanguageFields(idx : Integer) : TLangNameField;
begin
  Result := TLangNameField(FLanguages.Objects[idx]);
end;

function TTranslatedStrings.LanguageIndexByField(AField : TDataField) : Integer;
begin
  Result := FLanguages.IndexOfObject(AField);
end;

class function TTranslatedStrings.FormHasComponent(Form : TComponent; ComponentName : String; out Component : TComponent) : Boolean;
begin
  if (Form.Name = ComponentName) then
  begin
    Result := True;
    Component := Form;
  end
  else
  begin
    Component := Form.FindComponent(ComponentName);
    Result := (Component <> nil);
  end;
end;

function TTranslatedStrings.GetEditorWindow : TForm;
begin
  Result := FEditorWindow;
end;

procedure TTranslatedStrings.SetEditorWindow(AForm : TForm);
begin
  FEditorWindow := AForm;
end;

function TTranslatedStrings.GetOwner : TTranslator;
begin
  Result := FOwner;
end;

procedure TTranslatedStrings.UpdateActionItems(Sender : TAction);
var
  i, iField : Integer;
  ComponentName : String;
  RowList : TDataRowList;
  Component : TComponent;
  ActionClient, Client : TTranslatorClient;
  Action : TAction;
  ActionRow : TDataRow;

begin
  if FTranslations = nil then
    Exit;

  RowList := TDataRowList.Create;
  FTranslations.GetRows(RowList, nil, gaReference);

  Client := Owner;


  for i := 0 to RowList.Count - 1 do
  begin


    ComponentName := RowList.DataRows[i].StringValue[FieldComponent];

    if FormHasComponent(Client.Owner, ComponentName, Component) and
       PropertyTranslated(Component.ClassType, RowList.DataRows[i].StringValue[FieldProperty]) and
       HasActionForProp(Component, RowList.DataRows[i].StringValue[FieldProperty], Action) then
    begin

      ActionClient := Client;

      ActionRow := FTranslations.LocateRow([ActionClient.UniqueIdentifier, Action.Name, RowList.DataRows[i].StringValue[FieldProperty]]);
      for iField := 0 to LanguageCount - 1 do
        RowList.DataRows[i][LanguageFields[iField]] := ActionRow[LanguageFields[iField]];
      RowList.DataRows[i][FieldDoTranslate] := ActionRow[FieldDoTranslate];

      if (Sender = Action) and
         RowList.DataRows[i].BooleanValue[FieldDoTranslate] then
        Client.TranslateOneProperty(TDataRow(RowList.DataRows[i]));
    end;
  end;

  RowList.Free;
end;

procedure TTranslatedStrings.UpdateVisibleItems(DeleteUnused : Boolean);
var
  i : Integer;
  ComponentName : String;
  RowList : TDataRowList;
  Component : TComponent;
  Client : TTranslatorClient;

begin
  if FTranslations = nil then
    Exit;

  FTranslations.ShowSubTotals := False;
  FTranslations.DetailTreeKey.Visible := True;

  RowList := TDataRowList.Create;
  FTranslations.GetRows(RowList, nil, gaReference);

  Client := Owner;


  for i := 0 to RowList.Count - 1 do
  begin

    ComponentName := RowList.DataRows[i].StringValue[FieldComponent];

    if FormHasComponent(Client.Owner, ComponentName, Component) and
       PropertyTranslated(Component.ClassType, RowList.DataRows[i].StringValue[FieldProperty]) then // Fixa LGE borde ännu kollas att index är lagliga...
    begin
      if (ShowProperties = spTranslatedOnly) and
         (not RowList.DataRows[i].BooleanValue[FieldDoTranslate]) then
        RowList.DataRows[i].Visible := False;
    end
    else
    begin
      if DeleteUnused then
        RowList.DataRows[i].Delete
      else
        RowList.DataRows[i].Visible := False;
    end;
  end;

  FTranslations.ArrangeRows;
  RowList.Free;
end;

function TTranslatedStrings.RemoveIndexesFromProperty(AProperty : String) : String;
var
  p1, p2 : Integer;
begin
  Result := '';

  while True do
  begin
    p1 := Pos('[', AProperty);
    p2 := Pos(']', AProperty);

    if p1 <= 0 then
    begin
      Result := Result + AProperty;
      Break;
    end;

    Result := Result + Copy(AProperty, 1, p1) + ']';
    if p2 <= 0 then
      Break
    else
      AProperty := Copy(AProperty, p2 + 1, Length(AProperty));
  end;
end;

function TTranslatedStrings.PropertyTranslated(ClassType : TClass; AProperty : String) : Boolean;
var
  Criteria : TCriteria;
  List : TDataRowList;
  AClassName : String;
begin
  AProperty := RemoveIndexesFromProperty(AProperty);

  Criteria := TCriteria.Create;
  List := TDataRowList.Create;
  AClassName := ClassType.ClassName;
  Criteria[FieldClass].AddString(AClassName);
  Criteria[FieldProperty].AddString(AProperty);
  FAddedProperties.GetRows(List, Criteria, gaReference);

  Criteria[FieldSubClasses].AddValue(TrueValue);
  Criteria[FieldClass].AcceptNone;

  ClassType := ClassType.ClassParent;
  while ClassType <> nil do
  begin
    Criteria[FieldClass].AddString(ClassType.ClassName);
    ClassType := ClassType.ClassParent;
  end;
  FAddedProperties.GetRows(List, Criteria, gaReference);
  Criteria.Free;

  Result := (List.Count > 0);
  List.Free;
end;

type
  TPropertyType = (ptString, ptStrings, ptPersistent, ptCollection);

function SplitPropertyName(PropertyName : String; out FirstPart, RestOfProperty : String) : TPropertyType;
var
  b, d : Integer;
begin
  b := Pos('[', PropertyName);
  d := Pos('.', PropertyName);

  if (b <= 0) and (d <= 0) then
  begin
    Result := ptString;
    FirstPart := PropertyName;
    RestOfProperty := '';
  end
  else if (d > 0) and ((b <= 0) or (d < b)) then
  begin
    Result := ptPersistent;
    FirstPart := Copy(PropertyName, 1, d-1);
    RestOfProperty := Copy(PropertyName, d+1, Length(PropertyName));
  end
  else
  begin
    FirstPart := Copy(PropertyName, 1, b-1);
    RestOfProperty := Copy(PropertyName, b+1, Length(PropertyName));
    b := Pos(']', RestOfProperty);
    d := Pos('].', RestOfProperty);
    if b = d then
    begin
      Result := ptCollection;
      RestOfProperty := Copy(RestOfProperty, d+2, Length(RestOfProperty));
    end
    else
    begin
      Result := ptStrings;
      RestOfProperty := Copy(RestOfProperty, b+1, Length(RestOfProperty));
    end
  end;
end;

procedure TTranslatedStrings.RefreshLanguages;

begin

{$ifdef D4_OR_HIGHER}
  FillStrings(Owner);
{$else}
  FillStrings(Owner, -1);
{$endif D4_OR_HIGHER}


  UpdateVisibleItems(False);
end;

// Fixa LGE Proc()-metoderna skulle kunna mergas och ta funktionspointers vad de ska göra

{$ifdef D4_OR_HIGHER}
procedure TTranslatedStrings.FillStrings(Client : TTranslatorClient; ALangIndex : Integer = -1 {ANYLANGUAGE});
{$else}
procedure TTranslatedStrings.FillStrings(Client : TTranslatorClient; ALangIndex : Integer {ANYLANGUAGE});
{$endif D4_OR_HIGHER}

  procedure Proc(OrigComponent : TComponent; InitialPropertyName : String; Persistent : TPersistent; PropertyName : String; InfoRow : TDataRow; Level : Integer);
  var
    PropertyType : TPropertyType;
    FirstPart, RestOfProperty : String;
    PersistentProp : TPersistent;
    Collection : TCollection;
    PropertyValue : String;
    Strings : TStrings;
    iItem : Integer;
  begin
    if Level > MAXPROCLEVEL then
      Exit;

    PropertyType := SplitPropertyName(PropertyName, FirstPart, RestOfProperty);
    case PropertyType of
      ptString:     begin
{$ifdef D4_OR_HIGHER}
                      if HasProperty(Persistent, FirstPart) then
{$else}
                      if HasPropertyOL(Persistent, FirstPart) then
{$endif D4_OR_HIGHER}
                      begin
                        if GetProperty(Persistent, FirstPart, PropertyValue) then
                          AddTranslation(Client, OrigComponent, InitialPropertyName + FirstPart, PropertyValue, ALangIndex,
                                         InfoRow.BooleanValue[FieldDoTranslate]);
                      end;
                    end;

      ptStrings:    begin
                      if HasObjectProperty(TStrings, Persistent, FirstPart, Strings) then
                        for iItem := 0 to Strings.Count - 1 do
                          AddTranslation(Client, OrigComponent, InitialPropertyName + FirstPart + '[' + IntToStr(iItem) + ']',
                                         Strings[iItem], ALangIndex, InfoRow.BooleanValue[FieldDoTranslate]);
                    end;

      ptPersistent: begin
                      if HasObjectProperty(TPersistent, Persistent, FirstPart, PersistentProp) then
                        Proc(OrigComponent, InitialPropertyName + FirstPart + '.', PersistentProp, RestOfProperty, InfoRow, Level+1);
                    end;

      ptCollection: begin
                      if HasObjectProperty(TCollection, Persistent, FirstPart, Collection) then
                        for iItem := 0 to Collection.Count - 1 do
                          Proc(OrigComponent, InitialPropertyName + FirstPart + '[' + IntToStr(iItem) + '].',
                               Collection.Items[iItem], RestOfProperty, InfoRow, Level+1);
                    end;
    end;
  end;

  procedure ProcessComponent(Component : TComponent; List : TDataRowList);
  var
    iProperty : Integer;
  begin
    for iProperty := 0 to List.Count - 1 do
      Proc(Component, '', Component, List.Strings[iProperty], List.DataRows[iProperty], 0);
  end;

var
  iComponent : Integer;
  TranslatedProperties : TTranslatedProperties;
begin
  TranslatedProperties := TTranslatedProperties.Create(Self);

  ProcessComponent(Client.Owner, TranslatedProperties.GetList(Client.Owner));
  for iComponent := 0 to Client.Owner.ComponentCount - 1 do
    ProcessComponent(Client.Owner.Components[iComponent], TranslatedProperties.GetList(Client.Owner.Components[iComponent]));

  TranslatedProperties.Free;
end;

function TTranslatedStrings.CreateSubClassList(AClassName : string) : TStrings;
var
  Form : TComponent;
  i : Integer;
  AList, ABranch : TStrings;

  function IsParentOf(ASubClass : TClass) : boolean;
  begin
    if (TClass(ASubClass.ClassParent).ClassName = AClassName) or
       ((ASubClass.ClassParent <> nil) and
        IsParentOf(ASubClass.ClassParent)) then
      result := True
    else
      result := False;
  end;

  function CreateBranch(ASubClass : TClass) : TStrings;
  var
    tmpList : TStringList;
    tmpClass : TClass;
  begin
    tmpList := TStringList.Create;
    Result := tmpList;
    if TClass(ASubClass.ClassParent).ClassName = AClassName then
      exit;
    tmpClass := ASubClass;
    while tmpClass.ClassParent <> nil do
    begin
      tmpClass := tmpClass.ClassParent;
      tmpList.AddObject(tmpClass.ClassName, TObject(tmpClass));
      if TClass(tmpClass.ClassParent).ClassName = AClassName then
        Break;
    end;
  end;

begin
  AList := TStringList.Create;

  Form := Owner.Owner;
  AList.AddObject(Form.ClassName, TObject(Form.ClassType));

  for i := 0 to Form.ComponentCount - 1 do
    if (AList.IndexOf(Form.Components[i].ClassName) = -1) and
       (not (Form.Components[i] is TTranslatorClient)) then
      AList.AddObject(Form.Components[i].ClassName, TObject(Form.Components[i].ClassType));

  for i := AList.Count - 1 downto 0 do
  begin
    if not IsParentOf(TClass(AList.Objects[i])) then
      AList.Delete(i)
    else
    begin
      ABranch := CreateBranch(TClass(AList.Objects[i]));
      if ABranch <> nil then
        AList.AddStrings(ABranch);
      ABranch.Free;
    end;
  end;
  Result := AList;
end;

procedure TTranslatedStrings.FillClassPropertyList(AClass : TClass; ExampleObject : TObject; AList : TStringList; InitialName : String = '');
var
  ATypeKinds : TTypeKinds;
  APropList : PPropList;
  i, ACount, ASize : Integer;
  ACollection : TCollection;
  APersistent : TPersistent;
begin
  if AClass <> nil then
  begin
    ATypeKinds := [tkClass, tkString, tkLString, tkWString];

    ACount := TypInfo.GetPropList(AClass.ClassInfo, ATypeKinds, nil);
    ASize := ACount * SizeOf(Pointer);
    GetMem(APropList, ASize);

    TypInfo.GetPropList(AClass.ClassInfo, ATypeKinds, APropList);

    for i := 0 to ACount - 1 do
      if APropList^[i].Name <> 'Name' then
      begin
        if (APropList^[i].PropType^.Kind <> tkClass) then
          AList.Add(InitialName + APropList^[i]^.Name)
        else if ClassRefIsClass(GetTypeData(APropList^[i].PropType^).ClassType, TStrings) then
          AList.Add(InitialName + APropList^[i]^.Name + '[]')
        else if ClassRefIsClass(GetTypeData(APropList^[i].PropType^).ClassType, TPersistent) then
        begin
          if not ClassRefIsClass(GetTypeData(APropList^[i].PropType^).ClassType, TComponent) then
          begin
            if ExampleObject = nil then
              APersistent := nil
            else
              APersistent := TPersistent(TypInfo.GetObjectProp( ExampleObject, APropList^[i], TPersistent ));
            FillClassPropertyList(GetTypeData(APropList^[i].PropType^).ClassType, APersistent, AList, InitialName + APropList^[i]^.Name + '.');
          end;

          if ClassRefIsClass(GetTypeData(APropList^[i].PropType^).ClassType, TCollection) and
             (ExampleObject <> nil) then
          begin
            if (APropList^[i].PropType^.Kind = tkClass) then
            begin
              ACollection := TCollection(TypInfo.GetObjectProp( ExampleObject, APropList^[i], TCollection ));
              if ACollection <> nil then
                FillClassPropertyList(ACollection.ItemClass, nil, AList, InitialName + APropList^[i]^.Name + '[].');
            end;
          end;
        end;
      end;

    FreeMem(APropList);
  end;
end;

function TTranslatedStrings.HasProperty(Persistent : TPersistent; PropertyName : String; out PropInfo : PPropInfo) : Boolean;
begin
  // Do not allow translation of the Language property
  if (Persistent is TTranslatorClient) and
     (PropertyName = 'Language') then
  begin
    PropInfo := nil;
    Result := False;
    Exit;
  end;

  PropInfo := TypInfo.GetPropInfo(Persistent.ClassInfo, PropertyName);
  Result := (PropInfo <> nil);
end;

{$ifdef D4_OR_HIGHER}
function TTranslatedStrings.HasProperty(Persistent : TPersistent; PropertyName : String) : Boolean;
{$else}
function TTranslatedStrings.HasPropertyOL(Persistent : TPersistent; PropertyName : String) : Boolean;
{$endif D4_OR_HIGHER}
var
  PropInfo : PPropInfo;
begin
  Result := HasProperty(Persistent, PropertyName, PropInfo);
end;

function TTranslatedStrings.SetProperty(Persistent : TPersistent; PropertyName, Value : String) : Boolean;
var
  PropInfo : PPropInfo;
begin
  Result := HasProperty(Persistent, PropertyName, PropInfo);
  if Result then
    TypInfo.SetStrProp(Persistent, PropInfo, Value);
end;

function TTranslatedStrings.GetProperty(Persistent : TPersistent; PropertyName : String; out Value : String) : Boolean;
var
  PropInfo : PPropInfo;
  Action : TAction;
begin
  if HasActionForProp(Persistent, PropertyName, Action) then
    Persistent := Action;

  Result := HasProperty(Persistent, PropertyName, PropInfo);
  if Result then
    Value := TypInfo.GetStrProp(Persistent, PropInfo)
  else
    Value := '';
end;

function TTranslatedStrings.HasActionForProp(Persistent : TPersistent; PropertyName : String; out Action : TAction) : Boolean;
var
  PropInfo : PPropInfo;
begin
{$ifdef D4_OR_HIGHER}
  if (Pos('.', PropertyName) > 0) or
     (Pos('[', PropertyName) > 0) then
  begin
    Result := False; // Do not check actions for non-string properties
    Action := nil;
    Exit;
  end;

  PropInfo := TypInfo.GetPropInfo(Persistent, 'Action');
  Result := (PropInfo <> nil) and (PropInfo.PropType^.Kind = tkClass);
  if Result then
  begin
    Action := TAction(TypInfo.GetObjectProp( Persistent, PropInfo, TAction ));
    Result := (Action <> nil) and HasProperty(Action, PropertyName, PropInfo);
  end;

  if not Result then
    Action := nil;
{$else}
  Result := False;
  Action := nil;
{$endif D4_OR_HIGHER}
end;

function TTranslatedStrings.HasObjectProperty(ClassType : TClass; Persistent : TPersistent; PropertyName : String; out Reference) : Boolean;
var
  PropInfo : PPropInfo;
begin
{$ifdef D4_OR_HIGHER}
  PropInfo := TypInfo.GetPropInfo(Persistent.ClassInfo, PropertyName);

  Result := (PropInfo <> nil) and (PropInfo.PropType^.Kind = tkClass);
  if Result then
  begin
    TObject(Reference) := TypInfo.GetObjectProp( Persistent, PropInfo, ClassType );
    Result := (TObject(Reference) <> nil);
  end
  else
    TObject(Reference) := nil;

{$else}

  TObject(Reference) := nil;
  Result := False;

{$endif D4_OR_HIGHER}
end;

function TTranslatedStrings.GetEditorStandardView : TSingletonStandardView;
begin
  Result := FStandardViewLangEditor;
end;

function TTranslatedStrings.GetTranslationItemByName(Client : TTranslatorClient; Component : TComponent; PropertyName : String) : TDataRow;
var
  Criteria : TCriteria;
begin
  if (Component = nil) or (Component.Name = '') then
  begin
    Result := nil;
    Exit;
  end;

  Result := FTranslations.LocateRow([Client.UniqueIdentifier, Component.Name, PropertyName]);
  if Result <> nil then
  begin
    Result.PointerValue[FieldComponentPointer] := Component;
    Result.StringValue[FieldClass] := Component.ClassName;
  end
  else if (csDesigning in Owner.ComponentState) then // Only do this when designing because it slown down opening of forms. We do not have any FieldComponentPointer-info when not in design mode!
  begin
    Criteria := TCriteria.Create;
    Criteria[FieldGUID].AddString(Client.UniqueIdentifier);
    Criteria[FieldClass].AddString(Component.ClassName);
    Criteria[FieldProperty].AddString(PropertyName);
    Criteria[FieldComponentPointer].AddValue(ValueFromObject(Component));
    Result := FTranslations.LocateRowByCriteria(Criteria, True);
    Criteria.Free;

    if Result <> nil then
    begin
      Result.SetFieldValue( FieldComponent, ValueFromString(Component.Name), saOverwriteOnKeyChange);
      // correct the component's name-link if changed
    end;
  end;
end;

procedure TTranslatedStrings.AddTranslation(Client : TTranslatorClient; Component : TComponent; PropertyName, PropertyValue : String; ALangIndex : Integer; DoTranslate : Boolean);
var
  Item : TDataRow;
  iLangIndex : Integer;
begin
  Item := TranslationItemByName[Client, Component, PropertyName];
  if Item = nil then
  begin
    Item := TDataRow.Create(FTranslationTable);
    Item.StringValue[FieldGUID] := Client.UniqueIdentifier;
    Item.StringValue[FieldComponent] := Component.Name;
    Item.StringValue[FieldProperty] := PropertyName;
    Item.PointerValue[FieldComponentPointer] := Component;
    Item.StringValue[FieldClass] := Component.ClassName;

    for iLangIndex := 0 to LanguageCount - 1 do
      Item.StringValue[LanguageFields[iLangIndex]] := PropertyValue;

    Item.BooleanValue[FieldDoTranslate] := DoTranslate;
    FTranslations.PutRow(Item, paDontOverwriteKeys);
  end
  else if Item.BooleanValue[FieldDoTranslate] then
  begin
    if ALangIndex <> ANYLANGUAGE then
      Item.StringValue[LanguageFields[ALangIndex]] := PropertyValue;
  end
  else // update all languages if property not translated
  begin
    for iLangIndex := 0 to LanguageCount - 1 do
      Item.StringValue[LanguageFields[iLangIndex]] := PropertyValue;
  end;
end;

constructor TTranslatedStrings.Create(Owner : TTranslator);
begin
  inherited Create;
  FEditorWindow := nil;
  FOwner := Owner;
  FCurrentEditorClientGUID := '';

  CreateDataTables;
end;

destructor TTranslatedStrings.Destroy;
begin
  inherited Destroy;

  if Assigned(FEditorWindow) then
  begin
    FEditorWindow.Close;
    FEditorWindow.Release;
    FEditorWindow := nil;
  end;

  FreeAndNil(FUnitList);
  FreeAndNil(FTranslations);
  FreeAndNil(FAddedProperties);
  FreeAndNil(FStringTranslations);

  DestroyDataTables;
end;

function TTranslatedStrings.HasTranslation(const AProperty : String; LangIndex : Integer; out Translation : String) : Boolean;
var
  Row : TDataRow;
begin
  Row := FStringTranslations.LocateRow([AProperty]);
  if Row = nil then
  begin
    Result := False;
    Translation := '[' + AProperty + ']';
  end
  else
  begin
    Result := True;
    Translation := Row.StringValue[LanguageFields[LangIndex]];
  end;
end;

function TTranslatedStrings.HasTranslation(const AProperty : String; LangIndex : Integer; Variables : array of String; out Translation : String) : Boolean;
begin
  Translation := '';
  Result := HasTranslation(AProperty, LangIndex, Translation);
  if Result then
    Translation := ReplaceVariables(Translation, Variables);
end;

(*
var
  iVar : Integer;
  VarNr : Integer;
  VarPos : Integer;
  VarEndPos : Integer;
begin
  Translation := '';
  Result := HasTranslation(AProperty, LangIndex, Translation);
  if not Result then
    Exit;

  iVar := 1;
  VarPos := 1;
  repeat
    VarPos := SubStrPos(VarPos, '<$', Translation);
    if VarPos < 1 then
      Break;
    VarEndPos := SubStrPos(VarPos, '>', Translation);
    if VarEndPos < 1 then
      Break;

    VarNr := iVar;

    if VarEndPos - VarPos > 2 then
    begin
      try
        VarNr := StrToInt(Copy(Translation, VarPos + 2, VarEndPos - VarPos - 2));
      except
        VarPos := VarEndPos;
        Continue;
      end;
    end;

    if VarNr - 1 <= High(Variables) - Low(Variables) then
      Translation := Copy(Translation, 1, VarPos - 1) +
                     Variables[VarNr - 1 + Low(Variables)] +
                     Copy(Translation, VarEndPos + 1, Length(Translation))
    else
      Translation := Copy(Translation, 1, VarPos - 1) + Copy(Translation, VarEndPos + 1, Length(Translation));

    Inc(iVar);
  until False;
end;
*)
function TTranslatedStrings.GetString(const AProperty : String; LangIndex : Integer) : String;
begin
  HasTranslation(AProperty, Langindex, Result);
end;

{$ifdef D4_OR_HIGHER}
function TTranslatedStrings.GetString(const AProperty : String; LangIndex : Integer; Variables : array of String) : String;
{$else}
function TTranslatedStrings.GetStringOL(const AProperty : String; LangIndex : Integer; Variables : array of String) : String;
{$endif D4_OR_HIGHER}
begin
  HasTranslation(AProperty, LangIndex, Variables, Result);
end;

function TTranslatedStrings.GetTranslation(Client : TTranslatorClient; Component : TComponent; PropertyName : String; ALangIndex : Integer; out Value : String) : Boolean;
var
  Item : TDataRow;
begin
  Item := TranslationItemByName[Client, Component, PropertyName];
  if (Item = nil) or (not Item.BooleanValue[FieldDoTranslate]) then
  begin
    Result := False;
    Value := '';
  end
  else
  begin
    Result := True;
    Value := Item.StringValue[LanguageFields[ALangIndex]];
  end;
end;

function TTranslatedStrings.IndexOfLanguage(LangName : String) : Integer;
var
  i : Integer;
begin
  Result := ANYLANGUAGE;

  for i := 0 to LanguageCount - 1 do
    if Languages[i] = LangName then
    begin
      Result := i;
      Break;
    end;
end;

procedure TTranslatedStrings.GetTranslatedProperties(Component : TComponent; List : TDataRowList);
var
  Criteria : TCriteria;
  AClass : TClass;
  ListSuperClass : TDataRowList;
  i : Integer;
begin
  if (Component = nil) or (Component.Name = '') then
    Exit;

  Criteria := TCriteria.Create;
  Criteria[FieldClass].AddString(Component.ClassName);
  FAddedProperties.GetRows(List, Criteria, gaReference);
  List.FillStringsOptional(FieldProperty, dvKeyOnly);

  Criteria[FieldSubClasses].AddValue(TrueValue);
  Criteria[FieldClass].AcceptNone;

  ListSuperClass := TDataRowList.Create;

  AClass := Component.ClassType.ClassParent;
  while AClass <> nil do
  begin
    ListSuperClass.Clear;
    Criteria[FieldClass].AddString(AClass.ClassName);
    FAddedProperties.GetRows(ListSuperClass, Criteria, gaReference);
    for i := 0 to ListSuperClass.Count - 1 do
      if List.IndexOf(ListSuperClass.DataRows[i].StringValue[FieldProperty]) = -1 then
        List.AddObject(ListSuperClass.DataRows[i].StringValue[FieldProperty], ListSuperClass.DataRows[i]);
    AClass := AClass.ClassParent;
  end;

  Criteria.Free;
  ListSuperClass.Free;
end;

procedure TTranslatedStrings.UpdateLanguage;

begin
  if Owner <> nil then
  begin

    Owner.UpdateCurrentLanguageFromUI;


    Owner.RegisterClient(Owner); // Ensure self is registered (concerns unsaved forms)
    FTranslations.ArrangeRows;
  end;
end;

procedure TTranslatedStrings.CheckLanguageNames;
var
  i, j : Integer;
begin
  for i := 0 to Self.LanguageCount - 2 do
    for j := i+1 to Self.LanguageCount - 1 do
      if Self.Languages[i] = Self.Languages[j] then
        raise Exception.Create('Languagename ' + QuotedStr(Self.Languages[i]) + ' is used more than once!');
end;

{ TTranslatorClient }

class function TTranslatorClient.TranslatorVersion : String;
begin
  Result := id_CURRENTVERSION;
end;

function TTranslatorClient.GetAbout : TAboutTranslator;
begin
  Result := nil; // Result is meaningless...
end;

function TTranslatorClient.GetActiveLanguage : String;
begin
  if Strings <> nil then
    Result := Strings.Languages[FLangIndex]
  else
    Result := '[Select Server]';
end;

procedure TTranslatorClient.TranslateTo(LangName : String);
var
  LangIndex : Integer;
begin
  if (csReading in ComponentState) then
  begin
    if not (Self is TTranslator) then
      FReadLanguage := LangName;
    Exit;
  end
  else if (Strings = nil) then
  begin
    Exit;
  end;

  LangIndex := Strings.IndexOfLanguage(LangName);

  if LangIndex = ANYLANGUAGE then
    raise Exception.Create('Unknown language ' + QuotedStr(LangName));

  TranslateRecursive(LangName, TranslateApplication);
end;

procedure TTranslatorClient.TranslateRecursive(LangName : String; Recursive : Boolean);
var
  LangIndex : Integer;
  // iForm, iControl : Integer;
  iTranslator : Integer;
  OldLangName : String;
begin
  if Strings = nil then
    Exit;

  LangIndex := Strings.IndexOfLanguage(LangName);
  if LangIndex = ANYLANGUAGE then
    Exit;

  if LangIndex <> FLangIndex then
  begin
    OldLangName := Strings.Languages[FLangIndex];

    if Assigned(BeforeTranslate) then
      BeforeTranslate(Self, OldLangName, LangName);

    UpdateCurrentLanguageFromUI;
    TranslateStrings(LangIndex);
    FLangIndex := LangIndex;
  end;

  if Recursive then
    for iTranslator := 0 to TranslatorList.Count - 1 do
      TTranslatorClient(TranslatorList[iTranslator]).TranslateRecursive(LangName, False);

{    if Recursive then
    for iForm := 0 to Application.ComponentCount - 1 do
      for iControl := 0 to Application.Components[iForm].ComponentCount - 1 do
        if Application.Components[iForm].Components[iControl] is TTranslatorClient then
          TTranslatorClient(Application.Components[iForm].Components[iControl]).TranslateRecursive(LangName, False); }

  if (LangIndex <> FLangIndex) and Assigned(AfterTranslated) then
    AfterTranslated(Self, OldLangName, LangName);
end;

procedure TTranslatorClient.SetModified;
var
  Designer : IDesignerNotify;
begin
  Designer := FindRootDesigner(Self);
  if Assigned(Designer) then
    Designer.Modified;
end;

procedure TTranslatorClient.UpdateCurrentLanguageFromUI;
begin
  if Strings <> nil then
    Strings.FillStrings(Self, FLangIndex);
end;

procedure TTranslatorClient.UpdateUIFromCurrentLanguage;
begin
  TranslateStrings(FLangIndex);
end;

procedure TTranslatorClient.TranslateStrings(ALangIndex : Integer);

  procedure Proc(OrigComponent : TComponent; InitialPropertyName : String; Persistent : TPersistent; PropertyName : String; Level : Integer);
  var
    PropertyType : TPropertyType;
    FirstPart, RestOfProperty : String;
    PersistentProp : TPersistent;
    Collection : TCollection;
    StringList : TStrings;
    iItem : Integer;
    Action : TAction;
    Value : String;
  begin
    if Level > MAXPROCLEVEL then
      Exit;

    PropertyType := SplitPropertyName(PropertyName, FirstPart, RestOfProperty);
    case PropertyType of
      ptString:     begin
                      if Strings.HasActionForProp(Persistent, FirstPart, Action) then
                      begin
                        if Strings.GetTranslation(Self, Action, FirstPart, ALangIndex, Value) then
                          Strings.SetProperty(Persistent, FirstPart, Value);
                      end
                      else
                      begin
                        if Strings.GetTranslation(Self, OrigComponent, InitialPropertyName + FirstPart, ALangIndex, Value) then
                          Strings.SetProperty(Persistent, FirstPart, Value);
                      end;
                    end;

      ptStrings:    begin
                      if Strings.HasObjectProperty(TStrings, Persistent, FirstPart, StringList) then
                        for iItem := 0 to StringList.Count - 1 do
                          if Strings.GetTranslation(Self, OrigComponent, InitialPropertyName + FirstPart + '[' + IntToStr(iItem) + ']', ALangIndex, Value) then
                            StringList[iItem] := Value;
                    end;

      ptPersistent: begin
                      if Strings.HasObjectProperty(TPersistent, Persistent, FirstPart, PersistentProp) then
                        Proc(OrigComponent, InitialPropertyName + FirstPart + '.', PersistentProp, RestOfProperty, Level+1);
                    end;

      ptCollection: begin
                      if Strings.HasObjectProperty(TCollection, Persistent, FirstPart, Collection) then
                        for iItem := 0 to Collection.Count - 1 do
                          Proc(OrigComponent, InitialPropertyName + FirstPart + '[' + IntToStr(iItem) + '].',
                               Collection.Items[iItem], RestOfProperty, Level+1);
                    end;
    end;
  end;


  procedure ProcessComponent(Component : TComponent; List : TDataRowList);
  var
    iProperty : Integer;
  begin
    for iProperty := 0 to List.Count - 1 do
      Proc(Component, '', Component, List.Strings[iProperty], 0);
  end;

var
  Form : TComponent;
  iComponent : Integer;
  TranslatedProperties : TTranslatedProperties;
begin
  if Strings = nil then
    Exit;

  TranslatedProperties := TTranslatedProperties.Create(Strings);

  Form := Self.Owner;
  ProcessComponent(Form, TranslatedProperties.GetList(Form));

  for iComponent := 0 to Form.ComponentCount - 1 do
    ProcessComponent(Form.Components[iComponent], TranslatedProperties.GetList(Form.Components[iComponent]));

  TranslatedProperties.Free;
end;

procedure TTranslatorClient.TranslateOneProperty(ARow : TDataRow);
var
  Storage : TTranslatorRowStorage;
  TmpStorage : TTranslatorRowStorage;
begin
  if (Strings = nil) or (ARow = nil) then
    Exit;

  Storage := TTranslatorRowStorage.Create(Strings, ARow.DataTable);
  Storage.PutRow(ARow.CreateCopy, paDontOverWriteKeys);

  // Very kludgy...
  TmpStorage := Strings.FTranslations;
  Strings.FTranslations := Storage;

  Self.TranslateStrings(FLangIndex);

  Strings.FTranslations := TmpStorage;
  Storage.Free;
end;


procedure TTranslatorClient.SetServer(AServer : TTranslator);
begin
  if FServer <> nil then
  begin
    UpdateCurrentLanguageFromUI;
    FServer.UnRegisterClient(Self);
  end;

  FServer := AServer;

  if AServer <> nil then
  begin
    AServer.RegisterClient(Self);

    if not (csReading in ComponentState) then
    begin
      if (FReadLanguage <> '') then
      begin
        FLangIndex := Strings.IndexOfLanguage(FReadLanguage);
        if FLangIndex = ANYLANGUAGE then
          FLangIndex := 0;
        FReadLanguage := '';
      end;

      UpdateUIFromCurrentLanguage;
    end;
  end;
end;

constructor TTranslatorClient.Create(AOwner: TComponent);
var
  i : Integer;
begin
  inherited Create(AOwner);

  FGUIDFinal := False;
  FReadLanguage := '';

  if csDesigning in AOwner.ComponentState then
  begin
    for i := 0 to Owner.ComponentCount - 1 do
      if (Owner.Components[i] is TTranslatorClient) and
         (Owner.Components[i] <> Self) then
      begin
        ShowMessage('Only one Translator or Translator Client component is allowed on each form!');
        Abort;
      end;


    if (not (Self is TTranslator)) and
       (not (csReading in AOwner.ComponentState)) then
      MessageDlg('TTranslatorClient is used to centralize the translations. This is, however, not ' +
                 'enabled in the GPL version and this TTranslatorClient does nothing. You must ' +
                 'acquire a TTranslator license or translate each form separately with a normal ' +
                 'TTranslator component.', mtInformation, [mbOK], 0 );

  end;

  if TranslatorList <> nil then
    TranslatorList.Add(Self);

  FServer := nil;
  FLangIndex := 0;
  FTranslateApplication := False;
end;

destructor TTranslatorClient.Destroy;
begin
  inherited Destroy;

  if TranslatorList <> nil then
    TranslatorList.Remove(Self);
end;

function TTranslatorClient.GetString(const AProperty : String) : String;
begin
  if Strings = nil then
    Result := AProperty + ': [Server missing]'
  else
    Result := Strings.GetString(AProperty, FLangIndex);
end;

{$ifdef D4_OR_HIGHER}
function TTranslatorClient.GetString(const AProperty : String; Variables : array of String) : String;
begin
  if Strings = nil then
    Result := AProperty + ': [Server missing]'
  else
    Result := Strings.GetString(AProperty, FLangIndex, Variables);
end;
{$else}
function TTranslatorClient.GetStringOL(const AProperty : String; Variables : array of String) : String;
begin
  if Strings = nil then
    Result := AProperty + ': [Server missing]'
  else
    Result := Strings.GetStringOL(AProperty, FLangIndex, Variables);
end;
{$endif D4_OR_HIGHER}

function TTranslatorClient.HasTranslation(const AProperty : String; out Translation : String) : Boolean;
begin
  if Strings = nil then
  begin
    Result := False;
    Translation := AProperty + ': [Server missing]';
  end
  else
  begin
    Result := Strings.HasTranslation(AProperty, FLangIndex, Translation);
  end;
end;

function TTranslatorClient.HasTranslation(const AProperty : String; Variables : array of String; out Translation : String) : Boolean;
begin
  if Strings = nil then
  begin
    Result := False;
    Translation := AProperty + ': [Server missing]';
  end
  else
    Result := Strings.HasTranslation(AProperty, FLangIndex, Variables, Translation);
end;

procedure TTranslatorClient.Loaded;
var
  MT : TTranslatorClient;
begin
  inherited Loaded;

  if Strings = nil then
    Exit;

  MT := MainTranslator;

  // When a new form is loaded, we translate it
  // a) to the selected language -- if we are designing or we have no main translator
  // b) to the language of the Main TTranslator -- when running
  // Clients are translated when we assign Server
  // Servers are Translated in their Loaded method.

  if (MT <> nil) and (not (csDesigning in ComponentState)) then
  begin
    FLangIndex := Strings.IndexOfLanguage(MT.Language);
    if FLangIndex = ANYLANGUAGE then
      FLangIndex := 0;
  end
  else if FReadLanguage <> '' then
  begin
    FLangIndex := Strings.IndexOfLanguage(FReadLanguage);
    if FLangIndex = ANYLANGUAGE then
      FLangIndex := 0;
    FReadLanguage := '';
  end;

  UpdateUIFromCurrentLanguage;
end;

procedure TTranslatorClient.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);


  if (Filer is TWriter) then
    UpdateCurrentLanguageFromUI;

end;

function TTranslatorClient.MainTranslator : TTranslatorClient;
var
  i : Integer;
begin
  Result := nil;

  if TranslatorList <> nil then
    for i := 0 to TranslatorList.Count - 1 do
      if (TranslatorList.Items[i] <> Self) and
         TTranslatorClient(TranslatorList.Items[i]).TranslateApplication then
      begin
        Result := TranslatorList[i];
        Exit;
      end;
end;

function TTranslatorClient.GetUniqueIdentifier : String;
begin
  if (FGUID = '') and (not FGUIDFinal) then
  begin
    FGUID := ComObj.CreateClassID;
//    ShowMessage(Self.Name + ' gets a new GUID! Owner = ' + Self.Owner.Name);
  end;

  Result := FGUID;
end;

procedure TTranslatorClient.SetUniqueIdentifier(AGUID : String);
begin
  if (csReading in ComponentState) then
  begin

    if not FGUIDFinal then
    begin
      FGUID := AGUID;
//      ShowMessage(Self.Name + ' gets a GUID assigned! Value = ' + FGUID + '. Owner = ' + Self.Owner.Name);
      if FGUID <> '' then
        FGUIDFinal := True;
    end;
  end
  else if FGUID <> AGUID then
    raise Exception.Create('Cannot change Unique Identifier!');
end;

function TTranslatorClient.GetStrings : TTranslatedStrings;
begin
  if Server = nil then
    Result := nil
  else
    Result := Server.FStrings;
end;



{ TTranslator }

constructor TTranslator.Create(AOwner: TComponent);
begin
  fIgnoreLanguageNameCheck := False;

  inherited Create(AOwner);

  FServer := Self;
  FStrings := TTranslatedStrings.Create(Self);
end;

destructor TTranslator.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FStrings);
end;

procedure TTranslator.SetServer(AServer : TTranslator);
begin
  if (AServer <> Self) and (not (csReading in ComponentState)) then
    raise Exception.Create('Server for TTranslator is always itself!');
end;


procedure TTranslator.PerformVersionNotificationIfNeeded(AOldVersion : Integer);
var
  reg : TRegistry;
begin
  if (csDesigning in ComponentState) and (AOldVersion < 200) then
  begin
    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_CURRENT_USER;

      if reg.OpenKey('SOFTWARE\Polycon\Translator', True) then
      begin
        if not reg.ValueExists( 'VersionNotification' ) then
        begin
          ShowMessage('Translator ' + id_CURRENTVERSION + #13#10#13#10 +
                      'You have now started to use the second version of the Translator. ' +
                      'Please note that the licensing has changed!');
          reg.WriteInteger( 'VersionNotification', 2 );
        end;
      end;
    finally
      reg.Free;
    end;
  end;
end;


function TTranslator.UnRegisterClient(AClient : TTranslatorClient) : Boolean;
begin
  Result := False;
  if csDesigning in AClient.ComponentState then
  begin
    case MessageDlg('Remove all translations for ' + AClient.Owner.Name + '.' + AClient.Name + ' in the old server?',
                    mtConfirmation, mbYesNoCancel, 0) of
      mrYes: begin
               Strings.UnRegisterClientByGUID(AClient.UniqueIdentifier);
               Result := True;
             end;
      mrNo: ; // Nothing
    else
      {mrCancel:} Abort;
    end;
  end;
end;

procedure TTranslator.RegisterClient(AClient : TTranslatorClient);
var
  UnitName, FormClass : String;
  Form : TComponent;
  UnitInfoRow : TDataRow;
begin
  if (Self = nil) or (AClient = nil) then
    Exit;

  GetUnitAndForm(AClient, UnitName, Form);
  if Form <> nil then
    FormClass := Form.ClassName
  else
    FormClass := '';

  UnitInfoRow := TDataRow.Create(UnitListTable);
  UnitInfoRow.StringValue[FieldGUID] := AClient.UniqueIdentifier;
  UnitInfoRow.StringValue[FieldUnit] := UnitName;
  UnitInfoRow.StringValue[FieldFormClass] := FormClass;
  Strings.FUnitList.PutRow(UnitInfoRow, paOverwriteOnKeyChange);
end;

procedure TTranslator.LoadProperty(Reader : TReader);
var
  DfmVersion : Integer;

  function StrAsBool(B : String) : Boolean;
  begin
    Result := (LowerCase(B) = 'true');
  end;

  function StrAsPointer(Str : String) : Pointer;
  begin
    Result := Pointer(StrToInt(Str));
  end;

  function ValueFromNonSepStr(Str : String) : TValue;
  var
    p : Integer;
  begin
    p := 1;
    while p > 0 do
    begin
      p := SubStrPos(p, '/_', Str);
      if p > 0 then
      begin
        Str := Copy(Str, 1, p-1) + '/' + Copy(Str, p+2, Length(Str));
        Inc(p);
      end;
    end;
    Result := ValueFromString(Str);
  end;

  function CutNTrimText(var AStr : String; const Separator : String) : String;
  var
    p : Integer;
  begin
    p := Pos(Separator, AStr);
    if p < 1 then
    begin
      Result := Trim(AStr);
      AStr := '';
    end
    else
    begin
      Result := Trim(Copy(AStr, 1, p-1));
      AStr := Trim(Copy(AStr, p+Length(Separator), Length(AStr)));
    end;
  end;

  function InsertRow(Storage : TTranslatorRowStorage; Row : TDataRow) : TDataRow;
  begin
    Result := Storage.LocateByRowValues(Row, [nil]);
    if Result <> nil then
      Row.Free
    else
    begin
      Storage.PutRow(Row, paDontOverWriteKeys);
      Result := Row;
    end;
  end;

  procedure UpdateBooleanIfNeeded(Row : TDataRow; Field : TDataField; AStr : String);
  begin
    if AStr <> id_UNCHANGED then
      Row.BooleanValue[Field] := StrAsBool(AStr);
  end;

  procedure ProcessRow(Storage : TTranslatorRowStorage; Row : TDataRow);
  var
    OldRow : TDataRow;
  begin
    OldRow := Storage.LocateByRowValues(Row, [nil]);
    if OldRow <> nil then
    begin
      OldRow.AddValues(Row, aaReplaceAllValues);
      Row.Free;
    end
    else
      Storage.PutRow(Row, paDontOverWriteKeys);
  end;

  procedure ReadPropStorage(Storage : TTranslatorRowStorage);
  var
    Row : TDataRow;
    StrVal : String;
  begin
    Reader.ReadListBegin;
    while not Reader.EndOfList do
    begin
      StrVal := Reader.ReadString;

      Row := TDataRow.Create(Storage.DataTable);
      Row.StringValue[FieldClass] := CutNTrimText(StrVal, '.');
      Row.StringValue[FieldProperty] := CutNTrimText(StrVal, ':');

      Row := InsertRow(Storage, Row);

      UpdateBooleanIfNeeded(Row, FieldSubClasses, CutNTrimText(StrVal, ','));
      UpdateBooleanIfNeeded(Row, FieldDoTranslate, Trim(StrVal));
    end;
    Reader.ReadListEnd;
  end;

  procedure ReadComponentStorage(Storage : TTranslatorRowStorage);

    procedure InnerLoop(GUID : String);
    var
      j : Integer;
      Row : TDataRow;
      StrVal : String;
      ClassName : String;
      AlwaysSomething : Boolean;
      WasUnchanged : Boolean;
    begin
      while not Reader.EndOfList do
      begin
        Row := TDataRow.Create(Storage.DataTable);
        Row.StringValue[FieldGUID] := GUID;

        StrVal := Reader.ReadString;
        if DfmVersion >= 102 then
        begin
          ClassName := CutNTrimText(StrVal, '(');
          Row.StringValue[FieldComponent] := CutNTrimText(StrVal, ').');
        end
        else
        begin
          ClassName := CutNTrimText(StrVal, '(');
          {Row.PointerValue[FieldComponentPointer] := StrAsPointer(} CutNTrimText(StrVal, '),'){)};
          Row.StringValue[FieldComponent] := CutNTrimText(StrVal, '.');
        end;

        Row.StringValue[FieldProperty] := CutNTrimText(StrVal, ':');

        Row := InsertRow(Storage, Row);

        Row.StringValue[FieldClass] := ClassName;
        UpdateBooleanIfNeeded(Row, FieldDoTranslate, Trim(StrVal));
        Row.PointerValue[FieldComponentPointer] := nil;

        if DfmVersion >= 104 then
        begin
          AlwaysSomething := DfmVersion > 200;

          if AlwaysSomething or Row.BooleanValue[FieldDoTranslate] then
          begin
            if Reader.NextValue = vaList then
            begin
              WasUnchanged := False;
              Reader.ReadListBegin;
              for j := 0 to FStrings.LanguageCount - 1 do
              begin
                if Reader.EndOfList then
                begin
                  if (not WasUnchanged) and (j > 0) then
                    Row[FStrings.LanguageFields[j]] := Row[FStrings.LanguageFields[0]]
                end
                else if Reader.NextValue = vaIdent then
                begin
                  Reader.ReadIdent; {and do nothing with id_UNCHANGED}
                  WasUnchanged := True;
                end
                else
                  Row.StringValue[FStrings.LanguageFields[j]] := Reader.ReadString;
              end;
              Reader.ReadListEnd;
            end
            else  // property isn't really translated -- use only value...
            begin
              StrVal := Reader.ReadString;
              for j := 0 to FStrings.LanguageCount - 1 do
                Row.StringValue[FStrings.LanguageFields[j]] := StrVal;
            end;
          end;
        end
        else if DfmVersion >= 103 then
        begin
          Reader.ReadListBegin;
          for j := 0 to FStrings.LanguageCount - 1 do
            Row.StringValue[FStrings.LanguageFields[j]] := Reader.ReadString;
          Reader.ReadListEnd;
        end
        else
        begin
          StrVal := Reader.ReadString;
          for j := FIRSTLANGPOS_TABLE to FIRSTLANGPOS_TABLE + 1 do
            Row.ValueByIndex[j] := ValueFromNonSepStr(CutNTrimText(StrVal, '//'));
        end;
      end;
    end;

  var
    GUID : String;

  begin
    Reader.ReadListBegin;

    if DfmVersion >= 203 then
    begin
      while not Reader.EndOfList do
      begin

        Reader.ReadString;
        GUID := Self.UniqueIdentifier;


        Reader.ReadListBegin;
        InnerLoop(GUID);
        Reader.ReadListEnd;
      end;
    end
    else
    begin
      InnerLoop(Self.UniqueIdentifier);
    end;

    Reader.ReadListEnd;
  end;

  procedure ReadTranslationStorage(Storage : TTranslatorRowStorage);
  var
    j : Integer;
    Row : TDataRow;
    StrVal : String;
  begin
    Reader.ReadListBegin;
    while not Reader.EndOfList do
    begin
      Row := TDataRow.Create(Storage.DataTable);

      StrVal := Reader.ReadString;
      if DfmVersion >= 103 then
      begin
        Row.StringValue[FieldProperty] := StrVal;
        Row := InsertRow(Storage, Row);

        Reader.ReadListBegin;
        for j := 0 to FStrings.LanguageCount - 1 do
        begin
          if Reader.NextValue = vaIdent then
            Reader.ReadIdent {and do nothing with id_UNCHANGED}
          else
            Row.StringValue[FStrings.LanguageFields[j]] := Reader.ReadString;
        end;
        Reader.ReadListEnd;
      end
      else
      begin
        Row.StringValue[FieldProperty] := CutNTrimText(StrVal, ':');
        for j := 1 to 2 do
          Row.ValueByIndex[j] := ValueFromNonSepStr(CutNTrimText(StrVal, '//'));
        ProcessRow(Storage, Row);
      end;
    end;
    Reader.ReadListEnd;
  end;

  function VersionToInt(Str : String) : Integer;
  begin
    Result := 100 * StrToInt(CutNTrimText(Str, '.'));
    Result := Result + StrToInt(Str);

    if Length(Str) <> 2 then
      raise Exception.Create('Invalid version number!');
  end;

  procedure LanguageLoop;
  var
    iLanguage, iRemoveLang : Integer;
    ALanguageName : String;
  begin
    Reader.ReadListBegin;
    iLanguage := 0;
    fIgnoreLanguageNameCheck := True;
    try
      while not Reader.EndOfList do
      begin
        ALanguageName := Reader.ReadString;
        if iLanguage < FStrings.LanguageCount then
          FStrings.Languages[iLanguage] := ALanguageName
        else
          FStrings.InsertLanguage(iLanguage, ALanguageName, ANYLANGUAGE);

        Inc(iLanguage);
      end;

      for iRemoveLang := FStrings.LanguageCount - 1 downto iLanguage do
        FStrings.RemoveLanguage(iRemoveLang);
    finally
      fIgnoreLanguageNameCheck := False;
    end;

    FStrings.CheckLanguageNames;

    Reader.ReadListEnd;
  end;

  procedure SelectedLanguageLoop(AStr : String);
  begin
    CutNTrimText(AStr, ':');
    FLangIndex := FStrings.IndexOfLanguage(AStr);
    if FLangIndex = ANYLANGUAGE then
      FLangIndex := 0;
  end;

  procedure ShowPropertiesLoop(AStr : String);
  var
    iShowProperties : TShowProperties;
  begin
    CutNTrimText(AStr, ':');
    for iShowProperties := Low(TShowProperties) to High(TShowProperties) do
      if TypInfo.GetEnumName(TypeInfo(TShowProperties), Ord(iShowProperties)) = AStr then
      begin
        FStrings.ShowProperties := iShowProperties;
        Break;
      end;
  end;

  function ReadString(out OutReadString : String) : Boolean;
  begin
    if Reader.NextValue = vaList then
    begin
      Result := False;
      OutReadString := '';
    end
    else
    begin
      Result := True;
      OutReadString := Reader.ReadString;
    end;
  end;

var
  VersionStr, AllLanguages : String;
  InStr : String;
begin
  Reader.ReadListBegin;
  VersionStr := Reader.ReadString;
  CutNTrimText(VersionStr, ':');

  DfmVersion := VersionToInt(VersionStr);
  if DfmVersion > VersionToInt(id_STREAMINGVERSION) then
    raise Exception.Create('Cannot read dfm! Your TTranslator version is ' + id_CURRENTVERSION +
                           ', but the dfm is saved using a newer version!');

  PerformVersionNotificationIfNeeded(DfmVersion);


  if DfmVersion < 103 then
  begin
    AllLanguages := Reader.ReadString;
    CutNTrimText(AllLanguages, ':');
    FStrings.Languages[0] := CutNTrimText(AllLanguages, ',');
    FStrings.Languages[1] := AllLanguages;

    SelectedLanguageLoop(Reader.ReadString);
  end
  else if DfmVersion > 200 then
  begin
    if ReadString(InStr) and (InStr = id_LANGUAGES) then
    begin
      LanguageLoop;
      ReadString(InStr);
    end;

    if Copy(InStr, 1, Length(id_CURRENTLANGUAGE)) = id_CURRENTLANGUAGE then
    begin
      SelectedLanguageLoop(InStr);
      ReadString(InStr);
    end;

    if Copy(InStr, 1, Length(id_SHOWPROPERTIES)) = id_SHOWPROPERTIES then
    begin
      ShowPropertiesLoop(InStr);
      // ReadString(InStr) {no need; next is list anyway....}
    end;
  end
  else
  begin
    if Reader.ReadString <> id_LANGUAGES then
      raise Exception.Create('Error reading Languages!');

    LanguageLoop;
    SelectedLanguageLoop(Reader.ReadString);
  end;

  if (DfmVersion >= 101) and (DfmVersion <= 200) then
    ShowPropertiesLoop(Reader.ReadString);

  ReadPropStorage(FStrings.AddedProperties);
  ReadComponentStorage(FStrings.FTranslations);
  ReadTranslationStorage(FStrings.FStringTranslations);

  Reader.ReadListEnd;
end;

procedure TTranslator.StoreProperty(Writer : TWriter);
var
  AncestorTranslator : TTranslator;

  procedure BeforeWriteStorage(Storage : TTranslatorRowStorage);
  begin
    Storage.DetailTreeKey.Visible := True;
    Storage.ShowSubTotals := False;
    Storage.UsesCustomSortOrder := False;
    Storage.ArrangeRows;
  end;

  function DiffersFromAncestor(Row : TDataRow; AncestorStorage : TTranslatorRowStorage; out AncestorRow : TDataRow) : Boolean;
  var
    i : Integer;
  begin
    AncestorRow := nil;

    if AncestorStorage = nil then
      Result := True
    else if AncestorStorage.DataTable.FieldCount <> Row.DataTable.FieldCount then
      Result := True
    else
    begin
      AncestorRow := AncestorStorage.LocateByRowValues(Row, [nil]);
      if AncestorRow = nil then
        Result := True
      else
      begin
        Result := False;
        for i := Row.DataTable.KeyCount to Row.DataTable.FieldCount - 1 do
          if (Row.DataTable.Field[i] <> FieldComponentPointer) and
             (not Row.DataTable.Field[i].DataType.Equals(Row.ValueByIndex[i], AncestorRow.ValueByIndex[i])) then
          begin
            Result := True;
            Break;
          end;
      end;                                       // Fixa LGE dellade rader i subklassade Translators!
    end;                                         // Fixa LGE what if man har ändrat på / lagt till / tagit bort language i deriverade translators?
  end;


  function GetBoolStr(Row, AncestorRow : TDataRow; Field : TDataField) : String;
  begin
    if (AncestorRow <> nil) and
       (Row.BooleanValue[Field] = AncestorRow.BooleanValue[Field]) then
      Result := id_UNCHANGED
    else if Row.BooleanValue[Field] then
      Result := 'True'
    else
      Result := 'False';
  end;

  function AncesterValueEquals(Row, AncestorRow : TDataRow; LangIndex : Integer) : Boolean;
  begin
    if (AncestorTranslator = nil) or (AncestorRow = nil) then
      Result := False
    else if AncestorTranslator.Strings.LanguageCount <= LangIndex then
      Result := False
    else
      Result := (Row.StringValue[FStrings.LanguageFields[LangIndex]] = AncestorRow.StringValue[AncestorTranslator.Strings.LanguageFields[LangIndex]]);
  end;

  procedure WritePropStorage(Storage, AncestorStorage : TTranslatorRowStorage);
  var
    i : Integer;
    AncestorRow, Row : TDataRow;
  begin
    BeforeWriteStorage(Storage);

    Writer.WriteListBegin;
    for i := 0 to Storage.RowCount - 1 do
    begin
      Row := TDataRow(Storage.Rows[i]);

      if DiffersFromAncestor(Row, AncestorStorage, AncestorRow) then
        Writer.WriteString(Row.StringValue[FieldClass] + '.' +
                           Row.StringValue[FieldProperty] + ': ' +
                           GetBoolStr(Row, AncestorRow, FieldSubClasses) + ', ' +
                           GetBoolStr(Row, AncestorRow, FieldDoTranslate));
    end;
    Writer.WriteListEnd;
  end;

  function CheckOneValue(Row : TDataRow; Strings : TTranslatedStrings; out OnlyValue : String) : Boolean;
  var
    i : Integer;
  begin
    if (Row = nil) or (Strings.LanguageCount = 0) then
    begin
      Result := False;
      OnlyValue := '';
    end
    else
    begin
      Result := True;
      OnlyValue := Row.StringValue[Strings.LanguageFields[0]];
      for i := 1 to Strings.LanguageCount - 1 do
        if OnlyValue <> Row.StringValue[Strings.LanguageFields[i]] then
        begin
          Result := False;
          Break;
        end;
    end;
  end;

  procedure WriteComponentStorage(Storage, AncestorStorage : TTranslatorRowStorage);
  var
    i, j : Integer;
    AncestorRow, Row, PrevRow : TDataRow;
    OnlyValue, AncestorOnlyValue : String;
    UnitInfoRow : TDataRow;
    UnitInfo : String;
  begin
    BeforeWriteStorage(Storage);

    PrevRow := nil;
    Writer.WriteListBegin;
    for i := 0 to Storage.RowCount - 1 do
    begin
      Row := TDataRow(Storage.Rows[i]);

      if DiffersFromAncestor(Row, AncestorStorage, AncestorRow) then
      begin

        if (PrevRow = nil) then
        begin

          UnitInfoRow := Strings.FUnitList.LocateRow([Row.StringValue[FieldGUID]]);
          if UnitInfoRow = nil then
            UnitInfo := '? in ?'
          else
            UnitInfo := UnitInfoRow.StringValue[FieldFormClass] + ' in ' +
                        UnitInfoRow.StringValue[FieldUnit];
          Writer.WriteString(Row.StringValue[FieldGUID] + ' (' + UnitInfo + ')');
          Writer.WriteListBegin;
        end;

        PrevRow := Row;
        Writer.WriteString(Row.StringValue[FieldClass] + '(' +
                           Row.StringValue[FieldComponent] + ').' +
                           Row.StringValue[FieldProperty] + ': ' +
                           GetBoolStr(Row, AncestorRow, FieldDoTranslate));

        Writer.WriteListBegin;

        if Row.BooleanValue[FieldDoTranslate] then
        begin
          if CheckOneValue(Row, FStrings, OnlyValue) then
          begin
            if (AncestorTranslator <> nil) and
               CheckOneValue(AncestorRow, AncestorTranslator.Strings, AncestorOnlyValue) and
               (OnlyValue = AncestorOnlyValue) then
              Writer.WriteIdent(id_UNCHANGED)
            else
              Writer.WriteString(OnlyValue);
          end
          else
          begin
            for j := 0 to FStrings.LanguageCount - 1 do
            begin
              if AncesterValueEquals(Row, AncestorRow, j) then
                Writer.WriteIdent(id_UNCHANGED)
              else
                Writer.WriteString(Row.StringValue[FStrings.LanguageFields[j]]);
            end;
          end;
        end;

        Writer.WriteListEnd;
      end;
    end;

    if PrevRow <> nil then
      Writer.WriteListEnd;

    Writer.WriteListEnd;
  end;

  procedure WriteTranslationStorage(Storage, AncestorStorage : TTranslatorRowStorage);
  var
    i, j : Integer;
    AncestorRow, Row : TDataRow;
  begin
    BeforeWriteStorage(Storage);

    Writer.WriteListBegin;
    for i := 0 to Storage.RowCount - 1 do
    begin
      Row := TDataRow(Storage.Rows[i]);

      if DiffersFromAncestor(Row, AncestorStorage, AncestorRow) then
      begin
        Writer.WriteString(Trim(Row.StringValue[FieldProperty]));
        Writer.WriteListBegin;
        for j := 0 to FStrings.LanguageCount - 1 do
        begin
          if AncesterValueEquals(Row, AncestorRow, j) then
            Writer.WriteIdent(id_UNCHANGED)
          else
            Writer.WriteString(Row.StringValue[FStrings.LanguageFields[j]]);
        end;
        Writer.WriteListEnd;
      end;
    end;
    Writer.WriteListEnd;
  end;

var
  i : Integer;
  AncestorStorage : TTranslatorRowStorage;
begin
  Writer.WriteListBegin;
  Writer.WriteString(id_VERSION + ': ' + id_STREAMINGVERSION);

  AncestorTranslator := TTranslator(Writer.Ancestor);

  Writer.WriteString(id_LANGUAGES);
  Writer.WriteListBegin;
  for i := 0 to FStrings.LanguageCount - 1 do
    Writer.WriteString(FStrings.Languages[i]);
  Writer.WriteListEnd;

  if (AncestorTranslator = nil) or
     (Self.Language <> AncestorTranslator.Language) then
    Writer.WriteString(id_CURRENTLANGUAGE + ': ' + Self.Language);

  if (AncestorTranslator = nil) or
     (FStrings.ShowProperties <> AncestorTranslator.Strings.ShowProperties) then
    Writer.WriteString(id_SHOWPROPERTIES + ': ' + TypInfo.GetEnumName(TypeInfo(TShowProperties), Ord(FStrings.ShowProperties)));

  if (AncestorTranslator = nil) then
    AncestorStorage := nil
  else
    AncestorStorage := AncestorTranslator.Strings.AddedProperties;
  WritePropStorage(FStrings.AddedProperties, AncestorStorage);

  if (AncestorTranslator <> nil) then
    AncestorStorage := AncestorTranslator.Strings.FTranslations;
  WriteComponentStorage(FStrings.FTranslations, AncestorStorage);

  if (AncestorTranslator <> nil) then
    AncestorStorage := AncestorTranslator.Strings.FStringTranslations;
  WriteTranslationStorage(FStrings.FStringTranslations, AncestorStorage);

  Writer.WriteListEnd;
end;

procedure TTranslator.DefineProperties(Filer: TFiler);

begin
  inherited DefineProperties(Filer);



  Filer.DefineProperty('Translations', LoadProperty, StoreProperty, True);
end;

{ TTranslatedProperties }

constructor TTranslatedProperties.Create(TranslatedStrings : TTranslatedStrings);
begin
  inherited Create;

  fTranslatedStrings := TranslatedStrings;

  fList := TStringList.Create;
  fList.Sorted := True;
  fList.Duplicates := dupError;
end;

destructor TTranslatedProperties.Destroy;
var
  i : Integer;
begin
  for i := 0 to fList.Count - 1 do
    fList.Objects[i].Free;
  fList.Free;

  inherited Destroy;
end;

function TTranslatedProperties.GetList(Component : TComponent) : TDataRowList;
var
  idx : Integer;
begin
  if fList.Find( Component.ClassName, idx ) then
  begin
    Result := TDataRowList(fList.Objects[idx]);
  end
  else
  begin
    Result := TDataRowList.Create;
    fTranslatedStrings.GetTranslatedProperties(Component, Result);
    fList.AddObject(Component.ClassName, Result);
  end;
end;

initialization

  TranslatorList := TList.Create;

finalization

  TranslatorList.Free;
  TranslatorList := nil;

end.


