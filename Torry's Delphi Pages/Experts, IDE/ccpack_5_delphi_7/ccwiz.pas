{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       Custom Containers Pack (CCPack)                 }
{                                                       }
{       Copyright (c) 1997-99, Sergey Orlik             }
{                                                       }
{     Written by:                                       }
{       Sergey Orlik                                    }
{       product manager                                 }
{       Russia, C.I.S. and Baltic States (former USSR)  }
{       Inprise Moscow office                           }
{       e-mail:  sorlik@inprise.ru                      }
{       WWW: http://www.inprise.ru                      }
{                                                       }
{       Personal Home Page:                             }
{       www.geocities.com/SiliconValley/Way/9006/       }
{                                                       }
{*******************************************************}
{$I CCPDEF.INC}

{$IFDEF VER_CB}
  {$ObjExportAll On}
{$ENDIF}

unit ccwiz;
{$R *.res}
interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, ToolsAPI;

type
  TCCWizard = class(TInterfacedObject,
                     IOTAWIzard,
                     IOTARepositoryWizard,
                     IOTAProjectWizard)
    { IOTANotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    { IOTAWizard }
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    { IOTARepositoryWizard }
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: cardinal;
  end;

  TCCModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FBaseClassName : string;
    FClassName : string;
    FUnitIdent : string;
    FFileName : string;
  public
    constructor Create(ABaseClassName: string);
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
  end;

procedure Register;

implementation

uses
  ccreg, ccwdlg, ccwizres;

procedure Register;
begin
  RegisterPackageWizard(TCCWizard.Create as IOTAWizard);
end;

{ TOTAFile }
type
  TOTAFile = class(TInterfacedObject, IOTAFile)
  private
    FSource: string;
  public
    constructor Create(const ASource: string);
    { IOTAFile }
    function GetSource: string;
    function GetAge: TDateTime;
  end;

constructor TOTAFile.Create(const ASource: string);
begin
  inherited Create;
  FSource:=ASource;
end;

function TOTAFile.GetAge: TDateTime;
begin
  Result:=-1;
end;

function TOTAFile.GetSource: string;
begin
  Result:=FSource;
end;

{ TCCWizard }
procedure TCCWizard.AfterSave;
begin
end;

procedure TCCWizard.BeforeSave;
begin
end;

procedure TCCWizard.Destroyed;
begin
end;

procedure TCCWizard.Modified;
begin
end;

function TCCWizard.GetIDString: string;
begin
  Result:='Orlik.CustomContainerWizard';
end;

function TCCWizard.GetName: string;
begin
  Result:='Custom Container';
end;

function TCCWizard.GetState: TWizardState;
begin
  Result:=[wsEnabled];
end;

procedure TCCWizard.Execute;
var
  ModuleServices : IOTAModuleServices;
begin
  CCWDialog:=TCCWDialog.Create(Application);
  try
    if CCWDialog.ShowModal=mrOk then
    begin
      ModuleServices:=BorlandIDEServices as IOTAModuleServices;
      ModuleServices.CreateModule(TCCModuleCreator.Create(CCWDialog.CCtext.Caption));
    end;
  finally
    FreeAndNil(CCWDialog);
  end;
end;

function TCCWizard.GetAuthor: string;
begin
  Result:='Sergey Orlik';
end;

function TCCWizard.GetComment: string;
begin
  Result:='Custom Container Wizard';
end;

function TCCWizard.GetPage: string;
begin
  Result:='New';
end;

function TCCWizard.GetGlyph: cardinal;
begin
  Result:=LoadIcon(HInstance,'CCWizard');
end;

{ TCCModuleCreator }

constructor TCCModuleCreator.Create(ABaseClassName: string);
var
  s: string;
begin
  s:=ABaseClassName;
  System.Delete(s,1,1); // delete 'T' from the class name
  FBaseClassName:=s;
  (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName(ABaseClassName,FUnitIdent,FClassName,FFileName);
  if (Length(CCWDialog.CCeditClassName.Text)>2) and (CCWDialog.CCeditClassName.Text<>'') then
    FClassName:=CCWDialog.CCeditClassName.Text;
end;

procedure TCCModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  { Nothing to do }
end;

function TCCModuleCreator.GetAncestorName: string;
begin
  Result:=FBaseClassName;
end;

function TCCModuleCreator.GetCreatorType: string;
begin
  Result:=sForm;
end;

function TCCModuleCreator.GetExisting: Boolean;
begin
  Result:=False;
end;

function TCCModuleCreator.GetFileSystem: string;
begin
  Result:='';
end;

function TCCModuleCreator.GetFormName: string;
var
  s: string;
begin
  s:=FClassName;
  System.Delete(s,1,1); // delete 'T' from the class name
  Result:=s;
end;

function TCCModuleCreator.GetImplFileName: string;
begin
  Result:=FFileName;
end;

function TCCModuleCreator.GetIntfFileName: string;
begin
  Result:='';
end;

function TCCModuleCreator.GetMainForm: Boolean;
begin
  Result:=False;
end;

function TCCModuleCreator.GetOwner: IOTAModule;
begin
  Result:=nil;
end;

function TCCModuleCreator.GetShowForm: Boolean;
begin
  Result:=True;
end;

function TCCModuleCreator.GetShowSource: Boolean;
begin
  Result:=True;
end;

function TCCModuleCreator.GetUnnamed: Boolean;
begin
  Result:=True;
end;

function TCCModuleCreator.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result:=nil;
end;

function TCCModuleCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
var
  Source : string;
  AddUnit : string;
begin
  if GetBaseContainer(FindCustomContainerClass('T'+GetAncestorName))>1 then // XxxBox
    AddUnit:=', Boxes'
  else
    AddUnit:='';

  if IsBaseContainer(FindCustomContainerClass('T'+GetAncestorName))=-1 then
      AddUnit:=AddUnit+', '+GetCustomContainerUnit('T'+GetAncestorName)
    else
      AddUnit:=AddUnit+'';

  if CCWDialog.CCradio.ItemIndex=1 then
    Source:=sCCSourceContainerReg
  else
    if GetBaseContainer(FindCustomContainerClass('T'+GetAncestorName))<2 then // Form or DataModule
      Source:=sCCSourceForm
    else
      Source:=sCCSourceContainer;

  Source:=StringReplace(Source,'%UnitIdent%',FUnitIdent,[rfReplaceAll]);
  Source:=StringReplace(Source,'%AddUnit%',AddUnit,[rfReplaceAll]);
  Source:=StringReplace(Source,'%ClassName%',FClassName,[rfReplaceAll]);
  Source:=StringReplace(Source,'%FormName%',GetFormName,[rfReplaceAll]);
  Source:=StringReplace(Source,'%AncestorName%',GetAncestorName,[rfReplaceAll]);

  Result:=TOTAFile.Create(Source);
end;

function TCCModuleCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result:=nil;
end;

end.

