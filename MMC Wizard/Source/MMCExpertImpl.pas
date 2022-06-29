unit MMCExpertImpl;

interface

uses Windows, SysUtils, Classes, ToolsApi, Controls;

{$R SNIPPETS.RES}

type
TMMCExpert = class (TNotifierObject, IOTAWizard, IOTAProjectWizard, IOTARepositoryWizard, IUnknown)

{ IOTAWizard }
  function GetIDString : string;
  function GetState : TWizardState;
  function GetName : string;

{ IOTAProjectWizard }
  function GetAuthor : string;
  function GetComment : string;
  function GetPage : string;
  function GetGlyph : HICON;
  procedure Execute;
end;

TWizardProjectCreator = class (TNotifierObject, IOTACreator, IOTAProjectCreator, IOTAProjectCreator50)
private
  fCompanyName: string;
  fFileDescription: string;
  fProductName: string;

public

{ IOTACreator }

  function GetCreatorType: string;
  function GetExisting: Boolean;
  function GetFileSystem: string;
  function GetOwner: IOTAModule;
  function GetUnnamed: Boolean;

{ IOTAProjectCreator }

  function GetFileName: string;
  function GetOptionFileName: string;
  function GetShowSource: Boolean;
  procedure NewDefaultModule;
  function NewOptionSource(const ProjectName: string): IOTAFile;
  procedure NewProjectResource(const Project: IOTAProject);
  function NewProjectSource(const ProjectName: string): IOTAFile;

{ IOTAProjectCreator50 }

  procedure NewDefaultProjectModule(const Project: IOTAProject);

  constructor Create (const AProductName, AFileDescription, ACompanyName : string);
  property ProductName : string read fProductName;
  property FileDescription : string read fFileDescription;
  property CompanyName : string read fCompanyName;
end;

TSnapinDataModuleCreator = class (TNotifierObject, IOTACreator, IOTAModuleCreator)
private
  fOwner : IOTAModule;
  fStaticItemText : string;

public
{ IOTACreator }

  function GetCreatorType: string;
  function GetExisting: Boolean;
  function GetFileSystem: string;
  function GetOwner: IOTAModule;
  function GetUnnamed: Boolean;

{ IOTAModuleCreator }

  function GetAncestorName: string;
  function GetImplFileName: string;
  function GetIntfFileName: string;
  function GetFormName: string;
  function GetMainForm: Boolean;
  function GetShowForm: Boolean;
  function GetShowSource: Boolean;
  function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
  function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
  function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
  procedure FormCreated(const FormEditor: IOTAFormEditor);

  constructor Create (AOwner : IOTAModule; const AStaticItemText : string);
end;

TMMCProjectFile = class (TNotifierObject, IOTAFile)
private
  fFileName : string;
public
  function GetSource : string;
  function GetAge : TDateTime;
  constructor Create (const AFileName : string);
end;

TMMCSnapinDataFile = class (TNotifierObject, IOTAFile)
private
  fFileName : string;
public
  function GetSource : string;
  function GetAge : TDateTime;
  constructor Create (const AFileName : string);
end;

TMMCSnapinDataFormFile = class (TNotifierObject, IOTAFile)
private
  fFileName : string;
  fScopeItemText : string;
public
  function GetSource : string;
  function GetAge : TDateTime;
  constructor Create (const AFileName, AScopeItemText : string);
end;

var
  PackageServices : IOTAPackageServices;
  WizardServices : IOTAWizardServices;
  ModuleServices : IOTAModuleServices;

  module : IOTAModule;

procedure Register;

implementation

uses ComObj, ActiveX, MMCExpertForm, unitCodeSnippets, unitVersionInfo, TypInfo; //, mmcExpertTestForm;

procedure Register;
begin
  RegisterPackageWizard (TMMCExpert.Create as IOTAWizard)
end;

procedure InitWizard;
begin
  PackageServices := (BorlandIDEServices as IOTAPackageServices);
  WizardServices := (BorlandIDEServices as IOTAWizardServices);
  ModuleServices := (BorlandIDEServices as IOTAModuleServices);
end;

procedure ExitWizard;
begin
end;

function FormatSnippet (const s : string; const ss : array of string; const rs : array of string) : string;
var
  tempStr : string;
  idx : Integer;
  max : Integer;

  function CreateGUID : string;
  var
    r : TGUID;
  begin
    CoCreateGUID (r);
    result := GUIDToString (r);
  end;

begin
  max := High (ss);
  if High (rs) < max then
    max := High (rs);

  result := s;
  for idx := 0 to max do
    result := StringReplace (result, ss [idx], rs [idx], [rfReplaceAll, rfIgnoreCase]);

  repeat
    tempStr := StringReplace (result, '%CreateGUID%', CreateGuid, [rfIgnoreCase]);
    if tempStr = result then
      break;
    result := tempStr
  until False;
end;

procedure DisplayMessage (const msg : string);
begin
  MessageBox (0, PChar (msg), '', MB_ICONINFORMATION)
end;

const
  TypeNames : array [TTypeKind] of string = (
    'Unknown', 'Integer', 'Char', 'Enumeration', 'Float',
    'String', 'Set', 'Class', 'Method', 'WChar', 'LString', 'WString',
    'Variant', 'Array', 'Record', 'Interface', 'Int64', 'DynArray');

(*
   = (tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
    tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
    tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray);
*)
{ TMMCExpert }

procedure TMMCExpert.Execute;
var
  frm : TfmSnapinWizard;
  prj : IOTAProject;
//  i : Integer;
//  names : TOTAOptionNameArray;
//  opt : TOTAOptionName;
begin
  frm := TfmSnapinWizard.Create (Nil);
  try
    if frm.ShowModal = mrOK then
    begin
      moduleServices.CloseAll;

      prj := moduleServices.CreateModule (
        TWizardProjectCreator.Create (
          frm.edSnapinName.Text,
          frm.edSnapinDescription.Text,
          frm.edSnapinProvider.Text)) as IOTAProject;

      moduleServices.CreateModule (TSnapinDataModuleCreator.Create (prj, frm.edStaticScopeItem.Text));

(*
      with TfmMMCExpertTest.Create (nil) do
      try
        names := prj.ProjectOptions.GetOptionNames;

        for i := Low (names) to High (names) do
        begin
          opt := names [i];
          ListBox1.Items.Add (opt.Name)
        end;

        ShowModal;
      finally
        Free
      end
*)
    end;
  finally
    frm.Free
  end
end;

function TMMCExpert.GetAuthor: string;
begin
  result := 'Colin Wilson'
end;

function TMMCExpert.GetComment: string;
begin
  result := 'MMC Snapin Expert'
end;

function TMMCExpert.GetGlyph: HICON;
begin
  result := LoadIcon (HInstance, 'MMCWIZARD');
end;

function TMMCExpert.GetIDString: string;
begin
  result := 'ColinWilson.Experts.MMCSnapin.1';
end;

function TMMCExpert.GetName: string;
begin
  result := 'MMCExpert';
end;

function TMMCExpert.GetPage: string;
begin
  result := 'Projects';
end;

function TMMCExpert.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

{ TWizardProjectCreator }

constructor TWizardProjectCreator.Create(const AProductName,
  AFileDescription, ACompanyName : string);
begin
  fProductName := AProductName;
  fFileDescription := AFileDescription;
  fCompanyName := ACompanyName;
end;

function TWizardProjectCreator.GetCreatorType: string;
begin
  result := '';         // Do project source ourselves
end;

function TWizardProjectCreator.GetExisting: Boolean;
begin
  result := False;
end;

function TWizardProjectCreator.GetFileName: string;
begin
  result := '';
end;

function TWizardProjectCreator.GetFileSystem: string;
begin
  result := '';  // Return the ID string of a IOTAFileSystem
end;

function TWizardProjectCreator.GetOptionFileName: string;
begin
  result := '';
end;

function TWizardProjectCreator.GetOwner: IOTAModule;
begin
  result := Nil;
end;

function TWizardProjectCreator.GetShowSource: Boolean;
begin
  result := True;
end;

function TWizardProjectCreator.GetUnnamed: Boolean;
begin
  result := True
end;

procedure TWizardProjectCreator.NewDefaultModule;
begin
end;

procedure TWizardProjectCreator.NewDefaultProjectModule(
  const Project: IOTAProject);
begin
  Project.ProjectOptions.Values ['IncludeVersionInfo'] := True;
end;

function TWizardProjectCreator.NewOptionSource(
  const ProjectName: string): IOTAFile;
begin
  result := Nil
end;

procedure TWizardProjectCreator.NewProjectResource(
  const Project: IOTAProject);
var
  i : Integer;
  modEditor : IOTAEditor;
  resEditor : IOTAProjectResource;
  resEntry : IOTAResourceEntry;
  versionInfo : TVersionInfo;
  s : TMemoryStream;
begin
  resEntry := Nil;
  for i := 0 to Project.GetModuleFileCount - 1 do
  begin
    modEditor := Project.GetModuleFileEditor (i);

    if supports (modEditor, IOTAProjectResource, resEditor) then
    begin
      resEntry := resEditor.FindEntry (RT_VERSION, PChar (1));
      if Assigned (resEntry) then
        break
    end
  end;

  if Assigned (resEntry) then
  begin
    s := Nil;
    versionInfo := TVersionInfo.Create (PChar (resEntry.GetData));
    try
      s := TMemoryStream.Create;
      versionInfo.KeyValue ['ProductName'] := ProductName;
      versionInfo.KeyValue ['FileDescription'] := FileDescription;
      versionInfo.KeyValue ['CompanyName'] := CompanyName;
      versionInfo.SaveToStream (s);
      resEntry.DataSize := s.Size;
      Move (s.Memory^, resEntry.GetData^, s.Size);
    finally
      s.Free;
      versionInfo.Free
    end
  end
end;

function TWizardProjectCreator.NewProjectSource(
  const ProjectName: string): IOTAFile;
begin
  result := TMMCProjectFile.Create (ProjectName);
end;

{ TMMCProjectFile }

constructor TMMCProjectFile.Create(const AFileName: string);
begin
  fFileName := AFileName
end;

function TMMCProjectFile.GetAge: TDateTime;
begin
  result := -1;
end;

function TMMCProjectFile.GetSource: string;
begin
  result := FormatSnippet (GetSnippet ('MMCProjectSource'), ['%ProjectName%'], [fFileName]);
end;

{ TSnapinDataModuleCreator }

constructor TSnapinDataModuleCreator.Create(AOwner: IOTAModule; const AStaticItemText : string);
begin
  fOwner := AOwner;
  fStaticItemtext := AStaticItemText
end;

procedure TSnapinDataModuleCreator.FormCreated(
  const FormEditor: IOTAFormEditor);
begin
end;

function TSnapinDataModuleCreator.GetAncestorName: string;
begin
  result := 'DataModule'
end;

function TSnapinDataModuleCreator.GetCreatorType: string;
begin
  result := sForm;
end;

function TSnapinDataModuleCreator.GetExisting: Boolean;
begin
 result := False
end;

function TSnapinDataModuleCreator.GetFileSystem: string;
begin

end;

function TSnapinDataModuleCreator.GetFormName: string;
begin

end;

function TSnapinDataModuleCreator.GetImplFileName: string;
begin

end;

function TSnapinDataModuleCreator.GetIntfFileName: string;
begin

end;

function TSnapinDataModuleCreator.GetMainForm: Boolean;
begin
  result := True
end;

function TSnapinDataModuleCreator.GetOwner: IOTAModule;
begin
  result := fOwner
end;

function TSnapinDataModuleCreator.GetShowForm: Boolean;
begin
  result := True
end;

function TSnapinDataModuleCreator.GetShowSource: Boolean;
begin
  result := True
end;

function TSnapinDataModuleCreator.GetUnnamed: Boolean;
begin
  result := True
end;

function TSnapinDataModuleCreator.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  result := TMMCSnapinDataFormFile.Create (FormIdent, FStaticItemText);
end;

function TSnapinDataModuleCreator.NewImplSource(const ModuleIdent,
  FormIdent, AncestorIdent: string): IOTAFile;
begin
  result := TMMCSnapinDataFile.Create (ModuleIdent);
end;

function TSnapinDataModuleCreator.NewIntfSource(const ModuleIdent,
  FormIdent, AncestorIdent: string): IOTAFile;
begin

end;

{ TMMSnapinDataFile }

constructor TMMCSnapinDataFile.Create(const AFileName: string);
begin
  fFileName := AFileName;
end;

function TMMCSnapinDataFile.GetAge: TDateTime;
begin
  result := -1;
end;

function TMMCSnapinDataFile.GetSource: string;
begin
  result := FormatSnippet (GetSnippet ('MMCSnapinDataImplSource'), ['%FileName%'], [fFileName]);
end;

{ TMMCSnapinDataFormFile }

constructor TMMCSnapinDataFormFile.Create(const AFileName, AScopeItemText: string);
begin
  fScopeItemText := AScopeItemText;
  fFileName := AFileName;
end;

function TMMCSnapinDataFormFile.GetAge: TDateTime;
begin
  result := -1;
end;

function TMMCSnapinDataFormFile.GetSource: string;
begin
  result := FormatSnippet (GetSnippet ('MMCSnapinDataFormSource'), ['%ScopeItemText%'], ['''' + fScopeItemText + '''']);
end;

initialization
  InitWizard;
finalization
  ExitWizard;
end.
