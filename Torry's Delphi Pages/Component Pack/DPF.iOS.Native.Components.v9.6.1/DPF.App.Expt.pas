// ------------------------------------------------------------------------------
// DPF.App.Expt ToolsAPI Project Wizard Class
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
// Email #3: bayaghoobi@gmail.com
//
// ------------------------------------------------------------------------------
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ------------------------------------------------------------------------------
unit DPF.App.Expt;

interface

uses
  System.Classes, System.SysUtils, FMX.Dialogs, Windows, ToolsApi, DCCStrs;

const
  sCategoryDPFNew = 'Embarcadero.Delphi.DPF.New';

{$R DPF.App.Expt.res}

  // -----------------------------------------------------------------------------
resourcestring
  SWizardPage = 'D.P.F Delphi iOS Native Components';
  SAuthorName = 'Babak Yaghoobi';

  SProjectWizardDescription = 'Creates a new D.P.F iOS Project';
  SProjectWizardName = 'DPF iOS Mobile Project';
  SProjectWizardID = 'NewDelphiMobDPFProject';

  SFormWizardDescription = 'Creates a new D.P.F iOS From';
  SFormWizardName = 'DPF iOS Mobile Form';
  SFormWizardID = 'NewDelphiMobDPFForm';

  SFormName = 'DPFForm';
  SProjectResName = 'DPFProject';
  SUnitResName = 'DPFUnit';
  SFormResName = 'DPFForm';

type

  // -----------------------------------------------------------------------------
  // Added by Tyson Stephen
  TDPFAppProjectCreator = class( TInterfacedObject, IOTACreator, IOTAProjectCreator, IOTAProjectCreator50 )
  private
    FUnitIdent: string;
    FClassName: string;
    FFileName : string;
  public
    constructor Create( const UnitIdent, ClassName, FileName: string ); virtual;
    { IOTACreator }
    function GetCreatorType: string; virtual;
    function GetExisting: Boolean; virtual;
    function GetFileSystem: string; virtual;
    function GetOwner: IOTAModule; virtual;
    function GetUnnamed: Boolean; virtual;
    { IOTAProjectCreator }
    function GetFileName: string; virtual;
    function GetOptionFileName: string; virtual;
    function GetShowSource: Boolean; virtual;
    procedure NewDefaultModule; virtual;
    function NewOptionSource( const ProjectName: string ): IOTAFile; virtual;
    procedure NewProjectResource( const Project: IOTAProject ); virtual;
    function NewProjectSource( const ProjectName: string ): IOTAFile; virtual;
    { IOTAProjectCreator50 }
    procedure NewDefaultProjectModule( const Project: IOTAProject ); virtual;
  end;

  // -----------------------------------------------------------------------------
  TDPFAppModuleCreator = class( TInterfacedObject, IOTACreator, IOTAModuleCreator )
  private
    FUnitIdent: string;
    FClassName: string;
    FFileName : string;
  public
    constructor Create( const UnitIdent, ClassName, FileName: string ); virtual;
    function GetAncestorName: string;
    function GetFormName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile( const FormIdent: string; const AncestorIdent: string ): IOTAFile;
    function NewImplSource( const ModuleIdent: string; const FormIdent: string; const AncestorIdent: string ): IOTAFile;
    function NewIntfSource( const ModuleIdent: string; const FormIdent: string; const AncestorIdent: string ): IOTAFile;
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    procedure FormCreated( const FormEditor: IOTAFormEditor );
  end;

  // -----------------------------------------------------------------------------
  TDPFAppProjectWizard = class( TNotifierObject, IOTAWizard, IOTARepositoryWizard, IOTARepositoryWizard60, IOTARepositoryWizard80, IOTAProjectWizard )
  private
    FUnitIdent: string;
    FClassName: string;
    FFileName : string;
  public
    function GetDesigner: string;
    function GetPersonality: string;
    function GetGalleryCategory: IOTAGalleryCategory;
    { IOTAWizard declarations }
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    { IOTARepositoryWizard declarations }
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;

  end;

  // Added by Tyson Stephen
  TDPFAppFormWizard = class( TNotifierObject, IOTAWizard, IOTARepositoryWizard, IOTARepositoryWizard60, IOTARepositoryWizard80, IOTAProjectWizard )
  public
    FUnitIdent: string;
    FClassName: string;
    FFileName : string;
  public
    function GetDesigner: string;
    function GetPersonality: string;
    function GetGalleryCategory: IOTAGalleryCategory;
    { IOTAWizard declarations }
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    { IOTARepositoryWizard declarations }
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;
  end;

  // -----------------------------------------------------------------------------
  // Added by Tyson Stephen
  TBaseFile = class( TInterfacedObject )
  private
    FProjectName : string;
    FModuleName  : string;
    FFormName    : string;
    FAncestorName: string;
  public
    constructor Create( const ProjectName, ModuleName, FormName, AncestorName: string );
  end;

  // -----------------------------------------------------------------------------
  TDPFProjectSource = class( TBaseFile, IOTAFile )
  public
    function GetAge: TDateTime;
    function GetSource: string;
  end;

  // -----------------------------------------------------------------------------
  // Added by Tyson Stephen
  TDPFUnitSource = class( TBaseFile, IOTAFile )
  public
    function GetAge: TDateTime;
    function GetSource: string;
  end;

  // -----------------------------------------------------------------------------
  // Added by Tyson Stephen
  TDPFFormSource = class( TBaseFile, IOTAFile )
  public
    function GetAge: TDateTime;
    function GetSource: string;
  end;

  // -----------------------------------------------------------------------------
procedure Register;

implementation

// -----------------------------------------------------------------------------
procedure Register;
begin
  RegisterPackageWizard( TDPFAppProjectWizard.Create );
  RegisterPackageWizard( TDPFAppFormWizard.Create );
end;

// -----------------------------------------------------------------------------
function FindSource( const res: string ): string;
var
  stream: TResourceStream;
  list  : TStringList;
begin
  stream := TResourceStream.Create( HInstance, res, RT_RCDATA );
  list   := TStringList.Create;
  try
    list.LoadFromStream( stream );
    Result := list.Text;
  finally
    stream.Free;
    list.Free;
  end;
end;

// -----------------------------------------------------------------------------
procedure InitModule;
begin
  with ( BorlandIDEServices as IOTAGalleryCategoryManager ) do
    AddCategory( FindCategory( sCategoryDelphiNew ), sCategoryDPFNew, SWizardPage, 0 );
end;

// -----------------------------------------------------------------------------
procedure DoneModule;
begin
  with ( BorlandIDEServices as IOTAGalleryCategoryManager ) do
    DeleteCategory( FindCategory( sCategoryDPFNew ) );
end;

// -----------------------------------------------------------------------------
{ TDPFAppProjectCreator }
constructor TDPFAppProjectCreator.Create( const UnitIdent, ClassName, FileName: string );
begin
  FUnitIdent := UnitIdent;
  FClassName := ClassName;
  FFileName  := FileName;
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectCreator.GetCreatorType: string;
begin
  Result := sApplication;
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectCreator.GetFileName: string;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectCreator.GetOwner: IOTAModule;
begin
  Result := nil;
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectCreator.GetShowSource: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------
procedure TDPFAppProjectCreator.NewDefaultModule;
begin

end;

// -----------------------------------------------------------------------------
procedure TDPFAppProjectCreator.NewDefaultProjectModule( const Project: IOTAProject );
begin
  with ( BorlandIDEServices as IOTAModuleServices ) do
    CreateModule( TDPFAppModuleCreator.Create( FUnitIdent, FClassName, FFileName ) );
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectCreator.NewOptionSource( const ProjectName: string ): IOTAFile;
begin
  Result := nil;
end;

// -----------------------------------------------------------------------------
procedure TDPFAppProjectCreator.NewProjectResource( const Project: IOTAProject );
var
  Cfg : IOTAProjectOptionsConfigurations;
  i, j: Integer;
begin
  {
    with ( Project.ProjectOptions as IOTAProjectOptionsConfigurations ) do
    Configurations[1].InsertValues( sDefine, ['CONSOLEAPP'] );
  }
  Cfg     := Project.ProjectOptions as IOTAProjectOptionsConfigurations;
  for i   := 0 to Cfg.ConfigurationCount - 1 do
    for j := 0 to Length( Cfg.Configurations[i].Platforms ) - 1 do
      ShowMessage( Cfg.Configurations[i].Platforms[j] );
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectCreator.NewProjectSource( const ProjectName: string ): IOTAFile;
begin
  Result := TDPFProjectSource.Create( ProjectName, FUnitIdent, FClassName, FClassName );
end;

// -----------------------------------------------------------------------------
{ TDPFAppProjectWizard }
function TDPFAppProjectWizard.GetAuthor: string;
begin
  Result := SAuthorName;
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectWizard.GetDesigner: string;
begin
  Result := ToolsApi.dFMX;
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  with ( BorlandIDEServices as IOTAGalleryCategoryManager ) do
    Result := FindCategory( sCategoryDPFNew );
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectWizard.GetPage: string;
begin
  Result := 'New';
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectWizard.GetPersonality: string;
begin
  Result := ToolsApi.sDelphiPersonality;
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

// -----------------------------------------------------------------------------
procedure TDPFAppProjectWizard.Execute;
//var
//  Plats: TArray<string>;
//  S    : string;

//  Cfg : IOTAProjectOptionsConfigurations;
//  i, j: Integer;

begin
  ( BorlandIDEServices as IOTAModuleServices ).GetNewModuleAndClassName( '', FUnitIdent, FClassName, FFileName );
  FClassName := SFormName + Copy( FUnitIdent, 5, Length( FUnitIdent ) );
  with ( BorlandIDEServices as IOTAModuleServices ) do
    CreateModule( TDPFAppProjectCreator.Create( FUnitIdent, FClassName, FFileName ) );

  (* Cfg     := ( ( BorlandIDEServices as IOTAModuleServices ).GetActiveProject.ProjectOptions as IOTAProjectOptionsConfigurations );
    for i   := 0 to Cfg.ConfigurationCount - 1 do
    for j := 0 to Length( Cfg.Configurations[i].Platforms ) - 1 do
    ShowMessage( Cfg.Configurations[i]. Cfg.Configurations[i].Platforms[j] ); *)

  (* S := ( BorlandIDEServices as IOTAModuleServices ).GetActiveProject.GetConfiguration;
    ShowMessage( S );
    Plats := ( BorlandIDEServices as IOTAModuleServices ).GetActiveProject.SupportedPlatforms;
    for S in Plats do
    begin
    { }
    end;
  *)

  ( BorlandIDEServices as IOTAModuleServices ).GetActiveProject.SetPlatform( 'iOSSimulator' );
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectWizard.GetComment: string;
begin
  Result := SProjectWizardDescription;
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectWizard.GetIDString: string;
begin
  Result := SProjectWizardID;
end;

// -----------------------------------------------------------------------------
function TDPFAppProjectWizard.GetName: string;
begin
  Result := SProjectWizardName;
end;

// -----------------------------------------------------------------------------
{ TDPFProjectSource }
function TDPFProjectSource.GetAge: TDateTime;
begin
  Result := -1;
end;

// -----------------------------------------------------------------------------
function TDPFProjectSource.GetSource: string;
begin
  Result := Format( FindSource( SProjectResName ), [FProjectName { , FModuleName, FFormName } ] );
end;

// -----------------------------------------------------------------------------
{ TDPFAppModuleCreator }
constructor TDPFAppModuleCreator.Create( const UnitIdent, ClassName, FileName: string );
begin
  FUnitIdent := UnitIdent;
  FClassName := ClassName;
  FFileName  := FileName;
end;

// -----------------------------------------------------------------------------
procedure TDPFAppModuleCreator.FormCreated( const FormEditor: IOTAFormEditor );
begin

end;

// -----------------------------------------------------------------------------
function TDPFAppModuleCreator.GetAncestorName: string;
begin
  Result := 'Form';
end;

// -----------------------------------------------------------------------------
function TDPFAppModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

// -----------------------------------------------------------------------------
function TDPFAppModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------
function TDPFAppModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------
function TDPFAppModuleCreator.GetFormName: string;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------
function TDPFAppModuleCreator.GetImplFileName: string;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------
function TDPFAppModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------
function TDPFAppModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------
function TDPFAppModuleCreator.GetOwner: IOTAModule;
begin
  Result := GetActiveProject;
end;

// -----------------------------------------------------------------------------
function TDPFAppModuleCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------
function TDPFAppModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------
function TDPFAppModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------
function TDPFAppModuleCreator.NewFormFile( const FormIdent, AncestorIdent: string ): IOTAFile;
begin
  Result := TDPFFormSource.Create( '', '', FormIdent, AncestorIdent );
end;

// -----------------------------------------------------------------------------
function TDPFAppModuleCreator.NewImplSource( const ModuleIdent, FormIdent, AncestorIdent: string ): IOTAFile;
begin
  Result := TDPFUnitSource.Create( '', ModuleIdent, FormIdent, AncestorIdent );
end;

// -----------------------------------------------------------------------------
function TDPFAppModuleCreator.NewIntfSource( const ModuleIdent, FormIdent, AncestorIdent: string ): IOTAFile;
begin
  Result := nil;
end;

// -----------------------------------------------------------------------------
{ TDPFUnitSource }
function TDPFUnitSource.GetAge: TDateTime;
begin
  Result := -1
end;

// -----------------------------------------------------------------------------
function TDPFUnitSource.GetSource: string;
begin
  Result := Format( FindSource( SUnitResName ), [FModuleName, FFormName, FAncestorName] );
end;

// -----------------------------------------------------------------------------
{ TDPFFormSource }
function TDPFFormSource.GetAge: TDateTime;
begin
  Result := -1
end;

// -----------------------------------------------------------------------------
function TDPFFormSource.GetSource: string;
begin
  Result := Format( FindSource( SFormResName ), [FFormName] );
end;

// -----------------------------------------------------------------------------
{ TBaseFile }
constructor TBaseFile.Create( const ProjectName, ModuleName, FormName, AncestorName: string );
begin
  inherited Create;
  FProjectName  := ProjectName;
  FModuleName   := ModuleName;
  FFormName     := FormName;
  FAncestorName := AncestorName;
end;

// -----------------------------------------------------------------------------
{ TDPFAppFormWizard }
procedure TDPFAppFormWizard.Execute;
// var
// Plats: TArray<string>;
// S    : string;
begin
  ( BorlandIDEServices as IOTAModuleServices ).GetNewModuleAndClassName( '', FUnitIdent, FClassName, FFileName );
  FClassName := SFormName + Copy( FUnitIdent, 5, Length( FUnitIdent ) );
  with ( BorlandIDEServices as IOTAModuleServices ) do
    CreateModule( TDPFAppModuleCreator.Create( FUnitIdent, FClassName, FFileName ) );

  { Plats := ( BorlandIDEServices as IOTAModuleServices ).GetActiveProject.SupportedPlatforms;
    for S in Plats do
    ShowMessage( S );

    ( BorlandIDEServices as IOTAModuleServices ).GetActiveProject.SetPlatform( 'iOSSimulator' ); }
end;

// -----------------------------------------------------------------------------
function TDPFAppFormWizard.GetAuthor: string;
begin
  Result := SAuthorName;
end;

// -----------------------------------------------------------------------------
function TDPFAppFormWizard.GetComment: string;
begin
  Result := SFormWizardDescription;
end;

// -----------------------------------------------------------------------------
function TDPFAppFormWizard.GetDesigner: string;
begin
  Result := ToolsApi.dFMX;
end;

// -----------------------------------------------------------------------------
function TDPFAppFormWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  with ( BorlandIDEServices as IOTAGalleryCategoryManager ) do
    Result := FindCategory( sCategoryDPFNew );
end;

// -----------------------------------------------------------------------------
function TDPFAppFormWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

// -----------------------------------------------------------------------------
function TDPFAppFormWizard.GetIDString: string;
begin
  Result := SFormWizardID;
end;

// -----------------------------------------------------------------------------
function TDPFAppFormWizard.GetName: string;
begin
  Result := SFormWizardName;
end;

// -----------------------------------------------------------------------------
function TDPFAppFormWizard.GetPage: string;
begin
  Result := 'New';
end;

// -----------------------------------------------------------------------------
function TDPFAppFormWizard.GetPersonality: string;
begin
  Result := ToolsApi.sDelphiPersonality;
end;

// -----------------------------------------------------------------------------
function TDPFAppFormWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

// -----------------------------------------------------------------------------
initialization

InitModule;

// -----------------------------------------------------------------------------
finalization

DoneModule;

end.
