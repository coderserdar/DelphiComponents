unit TSRegFrmWiz;
{	The purpose of this Unit is to register selfmade form classes (descendants of TForm)
	that introduce new published properties.
	The registered form classes appear in the "Form Classes" tab of the "New Items" dialog.

	Before you register a form to the IDE, you should make sure, that it is bug free.
	Otherwise it could repeatedly crash the IDE.
	To register a Form to the repository you have to add your form's unit and this
	unit (TSRegFrmWiz.pas) to a design time package.
	In the "Register" procedure of your package you add:

	RegisterNewFormClass(<FormClass>, '<FormUnitName>', '<IconResourceName>', '<Comment>', '<Author>');
	whereas the Parameters are:
		<FormClass>: The class of your new form (e.g. TMyNewForm)
		'<FormUnitName>': The name of the unit, in which your new form is saved.
											If your form uses other units you can also add them here (comma separated).
											This string is simply added to the interface uses section.
		<IconResourceName>: You can add an icon file to your package resource
											D7: Open  TSRegFrmWiz.res with the Borland Image Editor
												Add an Icon and name it.
											Add the resource name here to display your own icon in the repository.
											If you don't supply an IconResourceName, the wizard tries to locate
											and icon resouce of the class name.
											
		<Comment>: If you supply a comment,it is shown in the detail view of the repository.
		<Author>: if you enter your name here, it is shown in the detail view of the repository.

		Enjoy,
		Lutz Kutscher
}

{$R *.res}

interface

uses
	Windows,
	Forms,
	DesignEditors,
	ToolsAPI;

type
	TFrameClass = class of TFrame;
	TTSFrmWiz = class(TInterfacedObject,
											IOTAWIzard,
											IOTARepositoryWizard,
											IOTAFormWizard)
		fFrmClassName: string;
		fFrmUses: string;
		fIconName: string;
		fComment: string;
		fAuthor: string;
		constructor Create(NewFormClassName, NewFormUses: string;
										IconName: string=''; Comment: string=''; Author: string='');
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
		function GetGlyph: Cardinal;
	end;

	TTSFormCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
	private
		fFormClassName : string;
		fUseUnits: string;
	public
		constructor Create(FormClassName, UseUnits: string);
		{ IOTACreator }
		function GetCreatorType: string;
		function GetExisting: Boolean;
		function GetFileSystem: string;
		function GetOwner: IOTAModule;
		function GetUnnamed: Boolean;
		{ IOTAModuleCreator }
		procedure FormCreated(const FormEditor: IOTAFormEditor);
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
	end;

	TFormSource = class(TInterfacedObject, IOTAFile)
	private
		fSource: string;
	public
		constructor Create(const ASource: string);
		{ IOTAFile }
		function GetSource: string;
		function GetAge: TDateTime;
	end;


procedure RegisterNewFormClass(NewFormClass: TFormClass; NewFormUses: string;
						NewFormIcon: string=''; NewFormComment: string=''; NewFormAuthor: string='');

/////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////
implementation

uses
	Classes,
	SysUtils,
	DesignIntf;

{Add one call of RegisterNewFormClass for each new FormClass you want to use.}
//procedure Register;
//begin
//end;


resourcestring
{This string constant holds the source that is shown in the Unit after creation of the Form.
	It uses some template strings that are replaced by the real names:
		%UnitIdent%			-> default name of the unit (e.g. unit1, unit2, ...)
		%FormName%			-> Name of the form variable (like in MyOwnForm: TMyOwnForm)
		%AncestorName%	-> Name of the form classes ancestor (usually TForm)
	}
	tsFormTemplate =
		'unit %UnitIdent%;'#13#10
		+ #13#10
		+ 'interface'#13#10
		+ #13#10
		+ 'uses'#13#10
		+ '	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs'#13#10
		+ '	%UseUnits%;'#13#10
		+ #13#10
		+ #13#10
		+ 'type'#13#10
		+ '	T%FormName% = class(T%AncestorName%)'#13#10
		+ '	private'#13#10
		+ #13#10
		+ '	public'#13#10
		+ #13#10
		+ '	end;'#13#10
		+ #13#10
		+ 'var'#13#10
		+ '	%FormName%: T%FormName%;'#13#10
		+ #13#10
		+ 'implementation'#13#10
		+ #13#10
		+ '{$R *.DFM}'#13#10
		+ #13#10
		+ 'end.'#13#10;


//***************************************************************************************
// 		utility functions
//***************************************************************************************

//----------------------------------------------------------------------
{This procedure registers the "Wizard", that shows the forms Icon in the "File/New" window
	as well as the class of the new form.}
procedure RegisterNewFormClass(NewFormClass: TFormClass; NewFormUses: string;
						NewFormIcon: string=''; NewFormComment: string=''; NewFormAuthor: string='');
begin
	RegisterPackageWizard(TTSFrmWiz.Create(NewFormClass.ClassName
																				, NewFormUses
																				, NewFormIcon
																				, NewFormComment
																				, NewFormAuthor
																				) as IOTAWizard);
	RegisterCustomModule(NewFormClass, TCustomModule)
end;


//----------------------------------------------------------------------
function FindModuleInterface(AInterface: TGUID): IUnknown;
var
	I: Integer;
begin
	Result := nil;
	with BorlandIDEServices as IOTAModuleServices do
		for I := 0 to ModuleCount - 1 do
			if (Modules[I].QueryInterface(AInterface, Result) = S_OK) then
				Break;
end;

//----------------------------------------------------------------------
function GetProjectGroup: IOTAProjectGroup;
begin
	Result := FindModuleInterface(IOTAProjectGroup) as IOTAProjectGroup;
end;

//----------------------------------------------------------------------
function GetCurrentProject: IOTAProject;
var
	ProjectGroup: IOTAProjectGroup;
begin
	Result := nil;
	ProjectGroup := GetProjectGroup;
	if Assigned(ProjectGroup) then
		Result := ProjectGroup.ActiveProject
	else
		Result := FindModuleInterface(IOTAProject) as IOTAProject;
end;



//***************************************************************************************
{ TTSFrmWiz }
//***************************************************************************************

constructor TTSFrmWiz.Create(NewFormClassName, NewFormUses: string;
										IconName: string=''; Comment: string=''; Author: string='');
begin
	inherited Create;
	fFrmClassName := NewFormClassName;
	fFrmUses := NewFormUses;
	fIconName := IconName;
	fComment := Comment;
	fAuthor := Author;
end;

procedure TTSFrmWiz.AfterSave;
begin //Tu Nix
end;

procedure TTSFrmWiz.BeforeSave;
begin //Tu Nix
end;

procedure TTSFrmWiz.Destroyed;
begin //Tu Nix
end;

procedure TTSFrmWiz.Modified;
begin //Tu Nix
end;

function TTSFrmWiz.GetAuthor: string;
begin
	if fAuthor > '' then
		Result := fAuthor
	else
		Result := 'Lutz Kutscher';
end;

function TTSFrmWiz.GetComment: string;
begin
	if fComment > '' then
		Result := fComment
	else
		Result := 'Enhanced Form Type by Tailor Soft';
end;

function TTSFrmWiz.GetGlyph: Cardinal;
begin
	if (fIconName > '') then
		Result := LoadIcon(HInstance, PChar(fIconName))
	else
		Result := 0;
	if Result = 0 then
		Result := LoadIcon(HInstance, PChar(fFrmClassName));
	if Result = 0 then
		Result := LoadIcon(HInstance,'Standard')
end;

function TTSFrmWiz.GetIDString: string;
begin
	Result := '{DD0DEB38-2A7A-4D45-B874-55513438E75F}' + fFrmClassName;
end;

function TTSFrmWiz.GetName: string;
begin
	Result := fFrmClassName;
end;

function TTSFrmWiz.GetPage: string;
begin
	Result := 'Form Classes';
end;

function TTSFrmWiz.GetState: TWizardState;
begin
	Result:=[wsEnabled];
end;

procedure TTSFrmWiz.Execute;
var
	ModuleServices : IOTAModuleServices;
begin
	ModuleServices:=BorlandIDEServices as IOTAModuleServices;
	ModuleServices.CreateModule(TTSFormCreator.Create(fFrmClassName, fFrmUses));
end;

//***************************************************************************************
//***************************************************************************************
{ TFormSource }
//***************************************************************************************

constructor TFormSource.Create(const ASource: string);
begin
	inherited Create;
	fSource := ASource;
end;

function TFormSource.GetAge: TDateTime;
begin
	Result := -1;
end;

function TFormSource.GetSource: string;
begin
	Result := fSource;
end;


//***************************************************************************************
//***************************************************************************************
{ TLKFrmCreator }
//***************************************************************************************

//***************************************************************************************
constructor TTSFormCreator.Create(FormClassName, UseUnits: string);
var
	FrmName: string;
begin
	FrmName := FormClassName;
	if (FrmName>'') and (FrmName[1] in ['t', 'T']) then
		Delete(FrmName, 1, 1);
	fFormClassName := FrmName;
	fUseUnits := UseUnits;
end;

//***************************************************************************************
function TTSFormCreator.GetCreatorType: string;
begin
	Result := sForm;
end;

//***************************************************************************************
function TTSFormCreator.GetExisting: Boolean;
begin
	Result := False;
end;

//***************************************************************************************
function TTSFormCreator.GetFileSystem: string;
begin
	Result := '';
end;

//***************************************************************************************
function TTSFormCreator.GetOwner: IOTAModule;
begin
	Result := GetCurrentProject;
end;

//***************************************************************************************
function TTSFormCreator.GetUnnamed: Boolean;
begin
	Result := True;
end;

//***************************************************************************************
procedure TTSFormCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

//***************************************************************************************
function TTSFormCreator.GetAncestorName: string;
begin
	Result := fFormClassName;
end;

//***************************************************************************************
function TTSFormCreator.GetImplFileName: string;
begin
	Result := '';
end;

//***************************************************************************************
function TTSFormCreator.GetIntfFileName: string;
begin
	Result:='';
end;

//***************************************************************************************
function TTSFormCreator.GetFormName: string;
begin
	Result := '';
end;

//***************************************************************************************
function TTSFormCreator.GetMainForm: Boolean;
begin
	Result := False;
end;

//***************************************************************************************
function TTSFormCreator.GetShowForm: Boolean;
begin
	Result := True;
end;

//***************************************************************************************
function TTSFormCreator.GetShowSource: Boolean;
begin
	Result := True;
end;

//***************************************************************************************
function TTSFormCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
	Result:=nil;
end;

//***************************************************************************************
function TTSFormCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
var
	Src: string;
begin
	Src := string(AnsiString(tsFormTemplate));
	if (fUseUnits>'') and (fUseUnits[1] <> ',') then
		fUseUnits := ',' + fUseUnits;
	Src := StringReplace(Src, '%UseUnits%', fUseUnits, [rfReplaceAll]);
	Src := StringReplace(Src, '%UnitIdent%', ModuleIdent, [rfReplaceAll]);
	Src := StringReplace(Src, '%FormName%', FormIdent, [rfReplaceAll]);
	Src := StringReplace(Src, '%AncestorName%', AncestorIdent, [rfReplaceAll]);
	Result:=TFormSource.Create(Src);
end;

//***************************************************************************************
function TTSFormCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
	Result:=nil;
end;


end.



