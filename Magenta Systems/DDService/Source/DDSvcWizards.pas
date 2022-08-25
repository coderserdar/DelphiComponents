{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  DDServiceApplication and DDService IDE wizards.
Creation:     2006
Version:      1.5
EMail:        arno.garrels@gmx.de
Support:      None
Legal issues: Copyright (C) 2006-2011 by Arno Garrels, Berlin

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.


History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit DDSvcWizards;

interface

{$I DDCompilers.inc}   // Compiler Defines
{$R DDSvcWizards.res}     // Wizard Icons

{$IFDEF COMPILER9_UP}
  {$DEFINE SUPPORTS_PERSONALITIES}
  {$IFDEF COMPILER16_UP}
    {$DEFINE SUPPORTS_PLATFORMS}
  {$ENDIF}
{$ENDIF}

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs,
{$IFDEF SUPPORTS_PLATFORMS}
  PlatformAPI,
{$ENDIF}
{$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF}
//{$IFDEF GXDEBUG} DbugIntf, {$ENDIF}
  ToolsApi,
  DDSvcMgr;

type
  TPersonality = (pDelphi, pCBuilder);
  TSourceType = (stIntf, stImpl);

  TDDServiceApplicationWizard = class(TNotifierObject,
                                IOTAWizard,
                                IOTARepositoryWizard,
           {$IFDEF COMPILER6_UP}IOTARepositoryWizard60, {$ENDIF}
 {$IFDEF SUPPORTS_PERSONALITIES}IOTARepositoryWizard80, {$ENDIF}
                                IOTAProjectWizard)
  private
    FPersonality: TPersonality;
  public
    constructor Create(APersonality: TPersonality);
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
{$IFDEF COMPILER6_UP}
    function GetGlyph: Cardinal;
{$ELSE}
    function GetGlyph: HICON;
{$ENDIF}
{$IFDEF COMPILER6_UP}
    // IOTARepositoryWizard60
    function GetDesigner: string;
{$ENDIF}
{$IFDEF SUPPORTS_PERSONALITIES}
    // IOTARepositoryWizard80
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;
{$ENDIF}
{$IFDEF COMPILER6_UP}
    property Designer: string read GetDesigner;
{$ENDIF}
{$IFDEF SUPPORTS_PERSONALITIES}
    property GalleryCategory: IOTAGalleryCategory read GetGalleryCategory;
    property Personality: string read GetPersonality;
{$ENDIF}
  end;

  TDDServiceWizard = class(TNotifierObject,
                                IOTAWizard,
                                IOTARepositoryWizard,
           {$IFDEF COMPILER6_UP}IOTARepositoryWizard60, {$ENDIF}
 {$IFDEF SUPPORTS_PERSONALITIES}IOTARepositoryWizard80, {$ENDIF}
                                IOTAProjectWizard)
  private
    FPersonality: TPersonality;
  public
    constructor Create(const APersonality: TPersonality);
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
{$IFDEF COMPILER6_UP}
    function GetGlyph: Cardinal;
{$ELSE}
    function GetGlyph: HICON;
{$ENDIF}
{$IFDEF COMPILER6_UP}
    // IOTARepositoryWizard60
    function GetDesigner: string;
{$ENDIF}
{$IFDEF SUPPORTS_PERSONALITIES}
    // IOTARepositoryWizard80
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;
{$ENDIF}
{$IFDEF COMPILER6_UP}
    property Designer: string read GetDesigner;
{$ENDIF}
{$IFDEF SUPPORTS_PERSONALITIES}
    property GalleryCategory: IOTAGalleryCategory read GetGalleryCategory;
    property Personality: string read GetPersonality;
{$ENDIF}
  end;

  TDDServiceProjectFile = class(TInterfacedObject, IOTAFile)
  private
    FAge: TDateTime;
    FProjectName: string;
    FPersonality: TPersonality;
  public
    constructor Create(const ProjectName: string; APersonality: TPersonality);
    function GetSource: string;
    function GetAge: TDateTime;
  end;

  TDDServiceDataModuleCreator = class(TInterfacedObject,
                                      IOTACreator,
                                      IOTAModuleCreator)
  private
    FPersonality: TPersonality;
  public
    constructor Create(APersonality: TPersonality);
    // IOTACreator
    function GetCreatorType: string; virtual;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModulCreator
    function GetAncestorName: string; virtual;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string; virtual;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; virtual;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor); virtual;

    property AncestorName: string read GetAncestorName;
    property FormName: string read GetFormName;
    property ImplFileName: string read GetImplFileName;
    property IntfFileName: string read GetIntfFileName;
    property MainForm: Boolean read GetMainForm;
    property ShowForm: Boolean read GetShowForm;
    property ShowSource: Boolean read GetShowSource;
  end;

  TDDServiceProjectCreator = class(TInterfacedObject,
                                  IOTACreator,
                                  IOTAProjectCreator,
                                  IOTAProjectCreator50
 {$IFDEF SUPPORTS_PERSONALITIES}, IOTAProjectCreator80 {$ENDIF}
     {$IFDEF SUPPORTS_PLATFORMS}, IOTAProjectCreator160 {$ENDIF}
                                  )
  private
    FPersonality: TPersonality;
  public
    constructor Create(APersonality: TPersonality);
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAProjectCreator
    function GetFileName: string;
    function GetOptionFileName: string;
    function GetShowSource: Boolean;
    procedure NewDefaultModule;
    function NewOptionSource(const ProjectName: string): IOTAFile;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;
    // IOTAProjectCreator50
    procedure NewDefaultProjectModule(const Project: IOTAProject);
{$IFDEF SUPPORTS_PERSONALITIES}
    // IOTAProjectCreator80
    function GetProjectPersonality: string;
    property ProjectPersonality: string read GetProjectPersonality;
{$ENDIF}
{$IFDEF SUPPORTS_PLATFORMS}
    { Return the framework type string for the framework this application supports }
    function GetFrameworkType: string;
    { Return the platform keys for the platforms this wizard supports }
    function GetPlatforms: TArray<string>;
    { Return the project's "preferred" platform - the one that will be explicitly supported
      when the project is created }
    function GetPreferredPlatform: string;
    procedure SetInitialOptions(const NewProject: IOTAProject);
{$ENDIF}
  end;

  TDDServiceDataModuleFile = class(TInterfacedObject, IOTAFile)
  private
    FAge: TDateTime;
    FModuleIdent,
    FFormIdent,
    FAncestorIdent: string;
    FPersonality: TPersonality;
    FSourceType: TSourceType;
  public
    constructor Create(const ModuleIdent, FormIdent, AncestorIdent: string;
      APersonality: TPersonality; ASourceType: TSourceType);
    function GetSource: string;
    function GetAge: TDateTime;
  end;

  TDDServiceCustomModule = class(TCustomModule)
  public
  {$IFDEF COMPILER7_UP}
    function ValidateComponentClass(ComponentClass: TComponentClass): Boolean; override;
  {$ENDIF}
  end;

  procedure Register;

implementation

const
  sCatPage = 'DDService';
  sAuthor  = 'Portions by A. Garrels';
  sDelphiServiceWizardID = 'DuoData.Delphi.ServiceWizard';
  sDelphiServiceApplicationWizardID = 'DuoData.Delphi.ServiceApplicationWizard';
  sDelphiCategoryServiceWizards = 'DuoData.Delphi.ServiceWizards';
  sCBuilderServiceWizardID = 'DuoData.CPlusPlusBuilder.ServiceWizard';
  sCBuilderServiceApplicationWizardID = 'DuoData.CPlusPlusBuilder.ServiceApplicationWizard';
  sCBuilderCategoryServiceWizards = 'DuoData.CPlusPlusBuilder.ServiceWizards';
  CRLF = #13#10;

  FormCode =
  'unit %ModuleIdent%;                                                          ' + CRLF +
  '                                                                             ' + CRLF +
  'interface                                                                    ' + CRLF +
  '                                                                             ' + CRLF +
  'uses                                                                         ' + CRLF +
{$IFDEF COMPILER16_UP}
  '  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,' + CRLF +
  '  Vcl.Controls, Vcl.Dialogs,                                                 ' + CRLF +
{$ELSE}
  '  Windows, Messages, SysUtils, Classes, Graphics, Controls, Dialogs,         ' + CRLF +
{$ENDIF}
  '  DDWindows, DDSvcMgr;                                                       ' + CRLF +
  '                                                                             ' + CRLF +
  'type                                                                         ' + CRLF +
  '  T%FormIdent% = class(T%AncestorIdent%)                                     ' + CRLF +
  '  private                                                                    ' + CRLF +
  '    { Private declarations }                                                 ' + CRLF +
  '  public                                                                     ' + CRLF +
  '    function GetServiceController: TServiceController; override;             ' + CRLF +
  '    function GetServiceControllerEx: TServiceControllerEx; override;         ' + CRLF +
  '    function GetConsoleCtrlHandler: TServiceConsoleCtrlHandler; override;    ' + CRLF +
  '  end;                                                                       ' + CRLF +
  '                                                                             ' + CRLF +
  'var                                                                          ' + CRLF +
  '  %FormIdent%: T%FormIdent%;                                                 ' + CRLF +
  '                                                                             ' + CRLF +
  'implementation                                                               ' + CRLF +
  '                                                                             ' + CRLF +
  '{$R *.DFM}                                                                   ' + CRLF +
  '                                                                             ' + CRLF +
  '{///////////////////////////////////////////////////////////////////////////}' + CRLF +
  'procedure ServiceController(CtrlCode: DWORD); stdcall;                       ' + CRLF +
  'begin                                                                        ' + CRLF +
  '  %FormIdent%.Controller(CtrlCode);                                          ' + CRLF +
  'end;                                                                         ' + CRLF +
  '                                                                             ' + CRLF +
  'function T%FormIdent%.GetServiceController: TServiceController;              ' + CRLF +
  'begin                                                                        ' + CRLF +
  '  Result := ServiceController;                                               ' + CRLF +
  'end;                                                                         ' + CRLF +
  '                                                                             ' + CRLF +
  'function ServiceControllerEx(CtrlCode, EventType: DWORD;                     ' + CRLF +
  '  EventData, Context: Pointer): DWORD; stdcall;                              ' + CRLF +
  'begin                                                                        ' + CRLF +
  '  Result := %FormIdent%.ControllerEx(CtrlCode, EventType, EventData, Context);' + CRLF +
  'end;                                                                         ' + CRLF +
  '                                                                             ' + CRLF +
  'function T%FormIdent%.GetServiceControllerEx: TServiceControllerEx;          ' + CRLF +
  'begin                                                                        ' + CRLF +
  '  Result := ServiceControllerEx;                                             ' + CRLF +
  'end;                                                                         ' + CRLF +
  '                                                                             ' + CRLF +
  'function ServiceConsoleCtrlHandler(Ctrl: DWord): Bool; stdcall;              ' + CRLF +
  'begin                                                                        ' + CRLF +
  '  Result := %FormIdent%.ConsoleCtrlHandler(Ctrl);                            ' + CRLF +
  'end;                                                                         ' + CRLF +
  '                                                                             ' + CRLF +
  'function T%FormIdent%.GetConsoleCtrlHandler: TServiceConsoleCtrlHandler;     ' + CRLF +
  'begin                                                                        ' + CRLF +
  '  Result := ServiceConsoleCtrlHandler;                                       ' + CRLF +
  'end;                                                                         ' + CRLF +
  '                                                                             ' + CRLF +
  '{///////////////////////////////////////////////////////////////////////////}' + CRLF +
  '                                                                             ' + CRLF +
  'end.                                                                         ' + CRLF;


  ProjectCode =
  'program %ProjectName%;                                                       ' + CRLF +
  '                                                                             ' + CRLF +
  'uses                                                                         ' + CRLF +
  '  DDSvcMgr;                                                                  ' + CRLF +
  '                                                                             ' + CRLF +
  '{$R *.RES}                                                                   ' + CRLF +
  '                                                                             ' + CRLF +
  'begin                                                                        ' + CRLF +
  '  // Windows 2003 Server requires StartServiceCtrlDispatcher to be           ' + CRLF +
  '  // called before CoRegisterClassObject, which can be called indirectly     ' + CRLF +
  '  // by Application.Initialize. TServiceApplication.DelayInitialize allows   ' + CRLF +
  '  // Application.Initialize to be called from TService.Main (after           ' + CRLF +
  '  // StartServiceCtrlDispatcher has been called).                            ' + CRLF +
  '  //                                                                         ' + CRLF +
  '  // Delayed initialization of the Application object may affect             ' + CRLF +
  '  // events which then occur prior to initialization, such as                ' + CRLF +
  '  // TService.OnCreate. It is only recommended if the ServiceApplication     ' + CRLF +
  '  // registers a class object with OLE and is intended for use with          ' + CRLF +
  '  // Windows 2003 Server.                                                    ' + CRLF +
  '  //                                                                         ' + CRLF +
  '  // Application.DelayInitialize := True;                                    ' + CRLF +
  '  //                                                                         ' + CRLF +
  '  if not Application.DelayInitialize or Application.Installing then          ' + CRLF +
  '    Application.Initialize;                                                  ' + CRLF +
  '  Application.Run;                                                           ' + CRLF +
  'end.';

UnitCPP =
 '//---------------------------------------------------------------------------' + CRLF +
 '#include "%ModuleIdent%.h"' + CRLF +
 '//---------------------------------------------------------------------------' + CRLF +
 '#pragma package(smart_init)' + CRLF +
 '#pragma resource "*.dfm"' + CRLF + CRLF +

 'T%FormIdent% *%FormIdent%;' + CRLF +
 '//---------------------------------------------------------------------------' + CRLF +
 '__fastcall T%FormIdent%::T%FormIdent%(TComponent* Owner)' + CRLF +
 '  : TDDService(Owner)' + CRLF +
 '{' + CRLF +
 '}' + CRLF + CRLF +

 'TServiceController __fastcall T%FormIdent%::GetServiceController(void)' + CRLF +
 '{' + CRLF +
 '  return (TServiceController) ServiceController;' + CRLF +
 '}' + CRLF + CRLF +

 'void __stdcall ServiceController(unsigned CtrlCode)' + CRLF +
 '{' + CRLF +
 '  %FormIdent%->Controller(CtrlCode);' + CRLF +
 '}' + CRLF + CRLF +

 'TServiceControllerEx __fastcall T%FormIdent%::GetServiceControllerEx(void)' + CRLF +
 '{' + CRLF +
 '  return (TServiceControllerEx) ServiceControllerEx;' + CRLF +
 '}' + CRLF + CRLF +

 'unsigned __stdcall ServiceControllerEx(unsigned CtrlCode, unsigned EventType,' + CRLF +
  '  void * EventData, void * Context)' + CRLF +
 '{' + CRLF +
 '  return %FormIdent%->ControllerEx(CtrlCode, EventType, EventData, Context);' + CRLF +
 '}' + CRLF + CRLF +

 'TServiceConsoleCtrlHandler __fastcall T%FormIdent%::GetConsoleCtrlHandler(void)' + CRLF +
 '{' + CRLF +
 '  return (TServiceConsoleCtrlHandler) ServiceConsoleCtrlHandler;' + CRLF +
 '}' + CRLF + CRLF +

 'bool __stdcall ServiceConsoleCtrlHandler(unsigned Ctrl)' + CRLF +
 '{' + CRLF +
 '  return %FormIdent%->ConsoleCtrlHandler(Ctrl);' + CRLF +
 '}' + CRLF + CRLF +

 '//---------------------------------------------------------------------------';

 UnitH =
 '//---------------------------------------------------------------------------' + CRLF +
 '#ifndef %ModuleIdent%H' + CRLF +
 '#define %ModuleIdent%H' + CRLF +
 '//---------------------------------------------------------------------------' + CRLF +
//{$IFDEF COMPILER16_UP}
// '#include <System.SysUtils.hpp>' + CRLF +
// '#include <System.Classes.hpp>' + CRLF +
//{$ELSE}
 '#include <SysUtils.hpp>' + CRLF +
 '#include <Classes.hpp>' + CRLF +
//{$ENDIF}
 '#include <DDSvcMgr.hpp>' + CRLF +
 '#include <vcl.h>' + CRLF +
 '//---------------------------------------------------------------------------' + CRLF +
 'class T%FormIdent% : public TDDService' + CRLF +
 '{' + CRLF +
 '__published:    // IDE-managed Components' + CRLF +
 'private:        // User declarations' + CRLF +
 'public:         // User declarations' + CRLF +

 ' __fastcall T%FormIdent%(TComponent* Owner);' + CRLF +

 ' TServiceController __fastcall GetServiceController(void);' + CRLF + CRLF +

 ' TServiceControllerEx __fastcall GetServiceControllerEx(void);' + CRLF + CRLF +
 ' TServiceConsoleCtrlHandler __fastcall GetConsoleCtrlHandler(void);' + CRLF + CRLF +

 ' friend bool __stdcall ServiceConsoleCtrlHandler(unsigned Ctrl);'+ CRLF + CRLF +

 ' friend void __stdcall ServiceController(unsigned CtrlCode);' + CRLF + CRLF +

 ' friend unsigned __stdcall ServiceControllerEx(unsigned CtrlCode,' + CRLF +
 '   unsigned EventType, void * EventData, void * Context);' + CRLF +
 '};' + CRLF +
 '//---------------------------------------------------------------------------' + CRLF +
 'extern PACKAGE T%FormIdent% *%FormIdent%;' + CRLF +
 '//---------------------------------------------------------------------------' + CRLF +
 '#endif';

ProjectCodeCPP =
//{$IFDEF COMPILER16_UP}
//  '#include <System.SysUtils.hpp>' + CRLF +
//{$ELSE}
  '#include <SysUtils.hpp>' + CRLF +
//{$ENDIF}
  '#include <DDSvcMgr.hpp>' + CRLF +
  '#pragma hdrstop' + CRLF +
{$IFDEF COMPILER15_UP}
  '#include <tchar.h>' + CRLF +
{$ENDIF}
  '#define Application Ddsvcmgr::Application' + CRLF +
  // Next line is inserted automatically
  //'USEFORM("Unit1.cpp", DDService1); /* TDDService: File Type */' + CRLF +
  //'//---------------------------------------------------------------------------' + CRLF +
{$IFDEF COMPILER15_UP}
  'WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)' + CRLF +
{$ELSE}
  'WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)' + CRLF +
{$ENDIF}
  '{' + CRLF +
  '    try' + CRLF +
  '    {' + CRLF +
  '        // Windows 2003 Server requires StartServiceCtrlDispatcher to be' + CRLF +
  '        // called before CoRegisterClassObject, which can be called indirectly' + CRLF +
  '        // by Application.Initialize. TServiceApplication->DelayInitialize allows' + CRLF +
  '        // Application->Initialize to be called from TService->Main (after' + CRLF +
  '        // StartServiceCtrlDispatcher has been called).' + CRLF +
  '        //' + CRLF +
  '        // Delayed initialization of the Application object may affect' + CRLF +
  '        // events which then occur prior to initialization, such as' + CRLF +
  '        // TService->OnCreate. It is only recommended if the ServiceApplication' + CRLF +
  '        // registers a class object with OLE and is intended for use with' + CRLF +
  '        // Windows 2003 Server.' + CRLF +
  '        //' + CRLF +
  '        // Application->DelayInitialize = true;' + CRLF +
  '        //' + CRLF +
  '        if ((!Application->DelayInitialize) || (Application->Installing()))' + CRLF +
  '        {' + CRLF +
  '            Application->Initialize();' + CRLF +
  '        }' + CRLF +
           // Next line is inserted automatically
           // Application->CreateForm(__classid(TDDService1), &DDService1);'
  '        Application->Run();' + CRLF +
  '    }' + CRLF +
  '    catch (Exception &exception)' + CRLF +
  '    {' + CRLF +
//{$IFDEF COMPILER16_UP}
//  '        System.Sysutils::ShowException(&exception, System::ExceptAddr());' + CRLF +
//{$ELSE}
  '        Sysutils::ShowException(&exception, System::ExceptAddr());' + CRLF +
//{$ENDIF}
  '    }' + CRLF +
  '        catch(...)' + CRLF +
  '        {' + CRLF +
  '        try' + CRLF +
  '        {' + CRLF +
  '            throw Exception("");' + CRLF +
  '        }' + CRLF +
  '        catch(Exception &exception)' + CRLF +
  '        {' + CRLF +
//{$IFDEF COMPILER16_UP}
//  '            System.Sysutils::ShowException(&exception, System::ExceptAddr());' + CRLF +
//{$ELSE}
  '            Sysutils::ShowException(&exception, System::ExceptAddr());' + CRLF +
//{$ENDIF}
  '        }' + CRLF +
  '        }' + CRLF +
  '    return 0;' + CRLF +
  '}';

{$IFDEF SUPPORTS_PERSONALITIES}
var
  DelphiCategory: IOTAGalleryCategory = nil;
  CBuilderCategory: IOTAGalleryCategory = nil;

{$IFDEF COMPILER10}
function MyPersonalityExists(const Personalities: IOTAPersonalityServices;
  const APersonality: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to Personalities.PersonalityCount - 1 do
  begin
    if Personalities.GetPersonality(I) = APersonality then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;
{$ENDIF}

function HasDelphiPersonality: Boolean;
var
  Personalities: IOTAPersonalityServices;
begin
  Result := Supports(BorlandIDEServices, IOTAPersonalityServices, Personalities) and
        {$IFDEF COMPILER10}
            MyPersonalityExists(Personalities, sDelphiPersonality);
        {$ELSE}
            Personalities.PersonalityExists(sDelphiPersonality);
        {$ENDIF}
end;

function HasCBuilderPersonality: Boolean;
var
  Personalities: IOTAPersonalityServices;
begin
  Result := Supports(BorlandIDEServices, IOTAPersonalityServices, Personalities) and
        {$IFDEF COMPILER10}
           MyPersonalityExists(Personalities, sCBuilderPersonality);
        {$ELSE}
           Personalities.PersonalityExists(sCBuilderPersonality);
        {$ENDIF}
end;

function IsDelphiPersonality: Boolean;
var
  Personalities: IOTAPersonalityServices;
begin
  Result := Supports(BorlandIDEServices, IOTAPersonalityServices, Personalities) and
            (Personalities.CurrentPersonality = sDelphiPersonality);
end;

function IsCBuilderPersonality: Boolean;
var
  Personalities: IOTAPersonalityServices;
begin
  Result := Supports(BorlandIDEServices, IOTAPersonalityServices, Personalities) and
            (Personalities.CurrentPersonality = sCBuilderPersonality);
end;
{$ENDIF}

procedure Register;
begin
{$IFDEF SUPPORTS_PERSONALITIES}
  if HasDelphiPersonality or HasCBuilderPersonality then
  begin
    RegisterCustomModule(TDDService, TDDServiceCustomModule);
    if HasDelphiPersonality then
    begin
      RegisterPackageWizard(TDDServiceApplicationWizard.Create(pDelphi));
      RegisterPackageWizard(TDDServiceWizard.Create(pDelphi));
    end;
    if HasCBuilderPersonality then
    begin
      RegisterPackageWizard(TDDServiceApplicationWizard.Create(pCBuilder));
      RegisterPackageWizard(TDDServiceWizard.Create(pCBuilder));
    end;
  end;
{$ELSE}
  {$IFDEF DELPHI5_UP} // No C++Builder tested
    {$IFNDEF DELPHI6} // Untested Delphi version
      RegisterCustomModule(TDDService, TDDServiceCustomModule);
      RegisterPackageWizard(TDDServiceApplicationWizard.Create(pDelphi));
      RegisterPackageWizard(TDDServiceWizard.Create(pDelphi));
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
end;

function GetCurrentProjectGroup: IOTAProjectGroup;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  IProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  Result := nil;
  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  for i := 0 to IModuleServices.ModuleCount - 1 do
  begin
    IModule := IModuleServices.Modules[i];
    if IModule.QueryInterface(IOTAProjectGroup, IProjectGroup) = S_OK then
    begin
      Result := IProjectGroup;
      Break;
    end;
  end;
end;

function GetCurrentProject: IOTAProject;
var
  ProjectGroup: IOTAProjectGroup;
begin
  Result := nil;
  ProjectGroup := GetCurrentProjectGroup;

  if Assigned(ProjectGroup) then
    if ProjectGroup.ProjectCount > 0 then
      Result := ProjectGroup.ActiveProject
end;


{ TDDServiceApplicationWizard }
constructor TDDServiceApplicationWizard.Create(APersonality: TPersonality);
begin
  inherited Create;
  FPersonality := APersonality;
end;

procedure TDDServiceApplicationWizard.Execute;
var
  ProjectModule,
  DataModule: IOTAModule;
  ProjectCreator: IOTACreator;
begin
  ProjectCreator := TDDServiceProjectCreator.Create(FPersonality);
  ProjectModule := (BorlandIDEServices as IOTAModuleServices).CreateModule(ProjectCreator);
  DataModule := (BorlandIDEServices as IOTAModuleServices).CreateModule(TDDServiceDataModuleCreator.Create(FPersonality));
end;

function TDDServiceApplicationWizard.GetAuthor: string;
begin
  Result := sAuthor
end;

function TDDServiceApplicationWizard.GetComment: string;
begin
  Result := 'Create a new DDService application';
end;

{$IFDEF COMPILER6_UP}
function TDDServiceApplicationWizard.GetGlyph: Cardinal;
begin
  Result := LoadIcon(hInstance, 'SERVICEAPP');
end;
{$ELSE}
function TProjectCreatorWizard.GetGlyph: HICON;
begin
  Result := LoadIcon(hInstance, 'SERVICEAPP');
end;
{$ENDIF}

function TDDServiceApplicationWizard.GetIDString: string;
begin
  if FPersonality = pCBuilder then
    Result := sCBuilderServiceApplicationWizardID
  else
    Result := sDelphiServiceApplicationWizardID;
end;

function TDDServiceApplicationWizard.GetName: string;
begin
  Result := 'DDService Application';
end;

function TDDServiceApplicationWizard.GetPage: string;
begin
  Result := sCatPage
end;

function TDDServiceApplicationWizard.GetState: TWizardState;
begin
  Result := [];
end;

{$IFDEF COMPILER6_UP}
function TDDServiceApplicationWizard.GetDesigner: string;
begin
  Result := dVCL;
end;
{$ENDIF}

{$IFDEF SUPPORTS_PERSONALITIES}
function TDDServiceApplicationWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  if FPersonality = pCBuilder then
    Result := CBuilderCategory
  else
    Result := DelphiCategory;
end;

function TDDServiceApplicationWizard.GetPersonality: string;
begin
  if FPersonality = pCBuilder then
    Result := sCBuilderPersonality
  else
    Result := sDelphiPersonality;
end;
{$ENDIF}


{ TDDServiceWizard }
constructor TDDServiceWizard.Create(const APersonality: TPersonality);
begin
  inherited Create;
  FPersonality := APersonality;
end;

procedure TDDServiceWizard.Execute;
var
  DataModule: IOTAModule;
begin
  DataModule := (BorlandIDEServices as IOTAModuleServices).CreateModule(
                TDDServiceDataModuleCreator.Create(FPersonality));
end;

function TDDServiceWizard.GetAuthor: string;
begin
  Result := sAuthor;
end;

function TDDServiceWizard.GetComment: string;
begin
  Result := 'Add a new DDService to a DDService application';
end;

{$IFDEF COMPILER6_UP}
function TDDServiceWizard.GetGlyph: Cardinal;
begin
  Result := LoadIcon(hInstance, 'SERVICE');
end;
{$ELSE}
function TServiceCreatorWizard.GetGlyph: HICON;
begin
  Result := LoadIcon(hInstance, 'SERVICE');
end;
{$ENDIF}

function TDDServiceWizard.GetIDString: string;
begin
  if FPersonality = pCBuilder then
    Result := sCBuilderServiceWizardID
  else
    Result := sDelphiServiceWizardID;
end;

function TDDServiceWizard.GetName: string;
begin
  Result := 'DDService';
end;

function TDDServiceWizard.GetPage: string;
begin
  Result := sCatPage;
end;

function TDDServiceWizard.GetState: TWizardState;
begin
  Result := [];
end;

{$IFDEF COMPILER6_UP}
function TDDServiceWizard.GetDesigner: string;
begin
  Result := dVCL;
end;
{$ENDIF}

{$IFDEF SUPPORTS_PERSONALITIES}
function TDDServiceWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  if FPersonality = pCBuilder then
      Result := CBuilderCategory
  else
      Result := DelphiCategory;
end;

function TDDServiceWizard.GetPersonality: string;
begin
  if FPersonality = pCBuilder then
    Result := sCBuilderPersonality
  else
    Result := sDelphiPersonality;
end;
{$ENDIF}


{ TDDServiceProjectCreator }
constructor TDDServiceProjectCreator.Create(APersonality: TPersonality);
begin
  inherited Create;
  FPersonality := APersonality;
end;

function TDDServiceProjectCreator.GetCreatorType: string;
begin
  Result := {$IFDEF COMPILER9_UP} sApplication {$ELSE} '' {$ENDIF};
end;

function TDDServiceProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TDDServiceProjectCreator.GetFileName: string;
begin
  Result := '';
end;

function TDDServiceProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TDDServiceProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TDDServiceProjectCreator.GetOwner: IOTAModule;
begin
  Result := GetCurrentProjectGroup;
end;

{$IFDEF SUPPORTS_PERSONALITIES}
function TDDServiceProjectCreator.GetProjectPersonality: string;
begin
  if FPersonality = pDelphi then
    Result := sDelphiPersonality
  else
    Result := sCBuilderPersonality;
end;
{$ENDIF}

function TDDServiceProjectCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TDDServiceProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

procedure TDDServiceProjectCreator.NewDefaultModule;
begin
end;

procedure TDDServiceProjectCreator.NewDefaultProjectModule(
  const Project: IOTAProject);
{$IFDEF NEVER} //  SUPPORTS_PLATFORMS
var
  ProjectPlatforms: IOTAProjectPlatforms;
begin
  if Supports(Project, IOTAProjectPlatforms, ProjectPlatforms) then
  begin
    ProjectPlatforms.Supported[cWin32Platform] := True;
    ProjectPlatforms.Enabled[cWin32Platform] := True;
    ProjectPlatforms.Supported[cWin64Platform] := (FPersonality = pDelphi);
    ProjectPlatforms.Enabled[cWin64Platform] := False;
    ProjectPlatforms.Supported[cOSX32Platform] := False;
  end;
{$ELSE}
begin
{$ENDIF}
end;

function TDDServiceProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TDDServiceProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
end;

function TDDServiceProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  Result := TDDServiceProjectFile.Create(ProjectName, FPersonality);
end;

{$IFDEF SUPPORTS_PLATFORMS}
{ Return the framework type string for the framework this application supports }
function TDDServiceProjectCreator.GetFrameworkType: string;
begin
  Result := sFrameworkTypeVCL;
end;

{ Return the platform keys for the platforms this wizard supports }
function TDDServiceProjectCreator.GetPlatforms: TArray<string>;
begin
  if FPersonality = pDelphi then
    Result := TArray<string>.Create(cWin32Platform, cWin64Platform)
  else
    Result := TArray<string>.Create(cWin32Platform);
end;

{ Return the project's "preferred" platform - the one that will be explicitly supported
  when the project is created }
function TDDServiceProjectCreator.GetPreferredPlatform: string;
begin
  Result := cWin32Platform;
end;

procedure TDDServiceProjectCreator.SetInitialOptions(const NewProject: IOTAProject);
begin
end;
{$ENDIF SUPPORTS_PLATFORMS}


{ TDDServiceProjectFile }
constructor TDDServiceProjectFile.Create(const ProjectName: string;
  APersonality: TPersonality);
begin
  FAge := -1;
  FProjectName := ProjectName;
  FPersonality := APersonality;
end;

function TDDServiceProjectFile.GetAge: TDateTime;
begin
  Result := FAge;
end;

function TDDServiceProjectFile.GetSource: string;
begin
  if FPersonality = pDelphi then
    Result := ProjectCode
  else if FPersonality = pCBuilder then
    Result := ProjectCodeCPP
  else
    Result := '%ProjectName% Error unknown personality';

  Result := StringReplace(Result, '%ProjectName%', FProjectName,
                          [rfReplaceAll, rfIgnoreCase]);
end;


{ TDDServiceDataModuleCreator }
constructor TDDServiceDataModuleCreator.Create(APersonality: TPersonality);
begin
  inherited Create;
  FPersonality := APersonality;
end;

procedure TDDServiceDataModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

function TDDServiceDataModuleCreator.GetAncestorName: string;
begin
  Result := 'DDService';
end;

function TDDServiceDataModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TDDServiceDataModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TDDServiceDataModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TDDServiceDataModuleCreator.GetFormName: string;
begin
  Result := '';
end;

function TDDServiceDataModuleCreator.GetImplFileName: string;
begin
  Result := '';
end;

function TDDServiceDataModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TDDServiceDataModuleCreator.GetMainForm: Boolean;
begin
  Result := True;
end;

function TDDServiceDataModuleCreator.GetOwner: IOTAModule;
begin
  Result := GetCurrentProjectGroup;
  if Assigned(Result) then
    Result := (Result as IOTAProjectGroup).ActiveProject
  else
    Result := GetCurrentProject;
end;

function TDDServiceDataModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TDDServiceDataModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TDDServiceDataModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TDDServiceDataModuleCreator.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TDDServiceDataModuleCreator.NewImplSource(const ModuleIdent,
  FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TDDServiceDataModuleFile.Create(
    ModuleIdent, FormIdent, AncestorIdent, FPersonality, stImpl);
end;

function TDDServiceDataModuleCreator.NewIntfSource(const ModuleIdent,
  FormIdent, AncestorIdent: string): IOTAFile;
begin
  if FPersonality = pDelphi then
    Result := nil
  else
    Result := TDDServiceDataModuleFile.Create(
                  ModuleIdent, FormIdent, AncestorIdent, FPersonality, stIntf);
end;


{ TDDServiceDataModuleFile }
constructor TDDServiceDataModuleFile.Create(const ModuleIdent, FormIdent,
  AncestorIdent: string; APersonality: TPersonality; ASourceType: TSourceType);
begin
  FAge := -1;
  FModuleIdent := ModuleIdent;
  FFormIdent := FormIdent;
  FAncestorIdent := AncestorIdent;
  FPersonality := APersonality;
  FSourceType := ASourceType
end;

function TDDServiceDataModuleFile.GetAge: TDateTime;
begin
  Result := FAge;
end;

function TDDServiceDataModuleFile.GetSource: string;
begin
  if FPersonality = pDelphi then
    Result := FormCode
  else if FSourceType = stIntf then
    Result := UnitH
  else
    Result := UnitCPP;

  if FModuleIdent <> '' then
    Result := StringReplace(Result, '%ModuleIdent%', FModuleIdent,
                            [rfReplaceAll, rfIgnoreCase]);
  if FFormIdent <> '' then
    Result := StringReplace(Result, '%FormIdent%', FFormIdent,
                            [rfReplaceAll, rfIgnoreCase]);
  if FAncestorIdent <> '' then
    Result := StringReplace(Result, '%AncestorIdent%', FAncestorIdent,
                           [rfReplaceAll, rfIgnoreCase]);
end;

{ TDDServiceCustomModule }

{$IFDEF COMPILER7_UP}
const
  ExcludedClasses : array[0..1] of TPersistentClass = (
    TWinControl, TGraphicControl);

function IsExcludedClass(AClass: TPersistentClass): Boolean;
var
  I: Integer;
begin
  for I := Low(ExcludedClasses) to High(ExcludedClasses) do
  begin
    if ExcludedClasses[I] = AClass then
    begin
      Result := True;
      Exit;
    end;
    if ClassInheritsFrom(AClass, ExcludedClasses[I].ClassName) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TDDServiceCustomModule.ValidateComponentClass(
  ComponentClass: TComponentClass): Boolean;
begin
  Result := inherited ValidateComponentClass(ComponentClass) and
            (not IsExcludedClass(ComponentClass));
end;
{$ENDIF}

{$IFDEF SUPPORTS_PERSONALITIES}
procedure InstallCategories;
var
  Manager: IOTAGalleryCategoryManager;
  Category: IOTAGalleryCategory;
begin
  DelphiCategory   := nil;
  CBuilderCategory := nil;
  Manager := BorlandIDEServices as IOTAGalleryCategoryManager;
  if Assigned(Manager) then
  begin
    Category := Manager.FindCategory(sCategoryDelphiNew);
    if Assigned(Category) then
      DelphiCategory := Manager.AddCategory(Category, sDelphiCategoryServiceWizards, sCatPage);

    Category := Manager.FindCategory(sCategoryCBuilderNew);
    if Assigned(Category) then
      CBuilderCategory := Manager.AddCategory(Category, sCBuilderCategoryServiceWizards, sCatPage);
  end;
end;

procedure DeleteCategories;
var
  Manager: IOTAGalleryCategoryManager;
begin
  Manager := BorlandIDEServices as IOTAGalleryCategoryManager;
  if Assigned(Manager) then
  begin
    if Assigned(DelphiCategory) then
      Manager.DeleteCategory(DelphiCategory);

    if Assigned(CBuilderCategory) then
      Manager.DeleteCategory(CBuilderCategory);
  end
end;

initialization
  InstallCategories

finalization
  DeleteCategories
{$ENDIF}

end.
