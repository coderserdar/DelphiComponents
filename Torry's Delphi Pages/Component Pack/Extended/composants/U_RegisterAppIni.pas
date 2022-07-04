unit U_RegisterAppIni;

// Procédure d'enregistrement de l'application form ini dans les projets
// Classes :
// TAppExpertIni         Module qui permet de créer le projet dans les projets
// Objet : Créer un projet dans les projets
// Créé par Matthieu Giroux le 3/12/2003

interface

procedure Register ;

implementation

uses
  ESBaseApp, U_RegisterIni, ESBaseData, Classes, SysUtils, Windows, ExptIntf, ToolIntf, EditIntf, ToolsApi;

const
  crlf = #13#10;    // Carriage-return line-feed.

resourcestring
  CST_AUTEUR = 'Matthieu Giroux';
  CST_COMMENTAIRE = 'Application avec Form utilisant un fichier INI pour initialiser son descendant et ADO';
  CST_EXPERTNOM = 'Application INI';
  CST_FORMANCETRE = 'F_FormMainIni' ;
  CST_FORMUNIT = 'U_FormMainIni' ;
  CST_NOUVEAUMENU = 'Giroux' ;
  CST_FORMIDSTRING = 'Giroux.App_TF_FormMainIni' ;

type
  { TAppExpertIni
   Module qui permet de créer le projet dans les projets}
  TAppExpertIni = class(TNouvelleApplication)
  private
  public
  // retourne le nom affiché dans les projets
  // Sortie : Le nom du projet dans les projets
    function GetName: string; override;
  // retourne le commentaire affiché dans les projets
  // Sortie : Le commentaire du projet dans les projets
    function GetComment: string; override;
  // retourne l'icône affiché dans les projets
  // Sortie : l'icône du projet dans les projets
    function GetGlyph: HICON; override;
  // retourne le style du projet
  // Sortie : le style du projet : projet expert
    function GetStyle: TExpertStyle; override;
  // retourne l'état du projet
  // Sortie : l'état du projet : csEnabled ou csChecked
    function GetState: TExpertState; override;
  // retourne l'identifiant du projet
  // Sortie : l'identifiant du projet
    function GetIDString: string; override;
  // retourne l'auteur du projet
  // Sortie : l'auteur du projet
    function GetAuthor: string; override;
  // retourne l'onglet du projet dans les projets
  // Sortie : l'onglet du projet dans les projets
    function GetPage: string; override;
  // retourne le texte du menu
  // Sortie : le texte du menu
    function GetMenuText: string; override;
  // exécution du projet
    procedure Execute; override;
  end;

  // Module géré automatiquement
  // qui permet de gérer la création du projet
  { TProjetAppliIni }
  TProjetAppliIni = class(TIProjectCreatorEx)
  public
  // Ne sait pas à quoi ça sert
    function Existing: Boolean; override;
  // Retourne le nom d'unité du projet
    function GetFileName: string; override;
  // Ne sait pas à quoi ça sert
    function GetFileSystem: string; override;
    // Retourne
    // la source du projet formattée
    function NewProjectSource(const ProjectName: string): string; override;
    // Création des modules supplémentaires
    procedure NewDefaultModule; override;
    // Crée un fichier Resource
    procedure NewProjectResource(Module: TIModuleInterface); override;
  // Ne sait pas à quoi ça sert
    function GetOptionName: string; override;
  // Ne sait pas à quoi ça sert
    function NewOptionSource(const ProjectName: string): string; override;
  end;

var
  AppExpertIni : TAppExpertIni;
  AppProjetIni : TProjetAppliIni;
  NouveauDataModule : TESBaseDataCreator ;



procedure TProjetAppliIni.NewDefaultModule;
begin
//original
//  ToolServices.ModuleCreate(NouveauModule, [cmAddToProject, cmShowSource,
//    cmShowForm, cmUnNamed, cmNewFile]);
  ToolServices.ModuleCreateEx(NouveauModule, [cmAddToProject, cmShowSource, cmShowForm, cmNewForm]);
  ToolServices.ModuleCreateEx(NouveauDataModule, [cmAddToProject, cmShowSource,cmShowForm, cmMainForm, cmUnNamed, cmNewFile]);
end;

{ TProjetAppliIni }

  // Ne sait pas à quoi ça sert
function TProjetAppliIni.Existing: Boolean;
begin
  Result:= False;
end;

  // Retourne le nom d'unité du projet
function TProjetAppliIni.GetFileName: string;
begin
  Result:= '';
end;

  // Ne sait pas à quoi ça sert
function TProjetAppliIni.GetFileSystem: string;
begin
  Result:= '';
end;

  // Ne sait pas à quoi ça sert
function TProjetAppliIni.GetOptionName: string;
begin
   Result:= '';
end;
{
procedure TProjetAppliIni.NewDefaultModule;
begin
  ToolServices.ModuleCreateEx(XPAppModule, [cmAddToProject, cmShowSource,
    cmShowForm, cmUnNamed, cmNewFile]);
end;
 }
  // Ne sait pas à quoi ça sert
function TProjetAppliIni.NewOptionSource(const ProjectName: string): string;
begin
  Result:= '';
end;

    // Crée un fichier Resource
procedure TProjetAppliIni.NewProjectResource(Module: TIModuleInterface);
begin
  { Do nothing }
end;

    // Retourne la source du projet formattée
function TProjetAppliIni.NewProjectSource(const ProjectName: string): string;
begin
  Result:= Format(sBasicAppliSource,
    [ProjectName]);
end;


{ TAppExpertIni }


  // exécution du projet
procedure TAppExpertIni.Execute;
begin
// Mettre ici les paramètres de la création du nouveau projet
  ToolServices.ProjectCreate(AppProjetIni, [cpApplication]);
end;

  // retourne l'auteur du projet
  // Sortie : l'auteur du projet
function TAppExpertIni.GetAuthor: string;
begin
  Result:= CST_AUTEUR;
end;

  // retourne le commentaire affiché dans les projets
  // Sortie : Le commentaire du projet dans les projets
function TAppExpertIni.GetComment: string;
begin
  Result:= CST_COMMENTAIRE;
end;

  // retourne l'icône affiché dans les projets
  // Sortie : l'icône du projet dans les projets
function TAppExpertIni.GetGlyph: HICON;
begin
  result := LoadIcon ( 0, idi_Application );
end;

  // retourne l'identifiant du projet
  // Sortie : l'identifiant du projet
function TAppExpertIni.GetIDString: string;
begin
  Result:= CST_FORMIDSTRING;
end;

  // retourne le texte du menu
  // Sortie : le texte du menu
function TAppExpertIni.GetMenuText: string;
begin
  Result:= '';
end;

  // retourne le nom affiché dans les projets
  // Sortie : Le nom du projet dans les projets
function TAppExpertIni.GetName: string;
begin
  Result:= CST_EXPERTNOM;
end;

  // retourne l'onglet du projet dans les projets
  // Sortie : l'onglet du projet dans les projets
function TAppExpertIni.GetPage: string;
begin
  Result:= CST_NOUVEAUMENU ;
end;

  // retourne l'état du projet
  // Sortie : l'état du projet : csEnabled ou csChecked
function TAppExpertIni.GetState: TExpertState;
begin
  Result:= [esEnabled, esChecked];
end;

  // retourne le style du projet
  // Sortie : le style du projet
function TAppExpertIni.GetStyle: TExpertStyle;
begin
  Result:= esProject;
end;

procedure Register ;
begin // Enregistre le nouvel expert de projet
  AppExpertIni := TAppExpertIni.Create ;
  // Un register libère automatiquement la variable à la suppression
  RegisterLibraryExpert( AppExpertIni );
  AppProjetIni:= TProjetAppliIni.Create;
  NouveauModule:= TF_McFormMainIniModule.Create;
  NouveauDataModule:= TESBaseDataCreator.Create;
end;
initialization
finalization
// Un register libère automatiquement la variable
// Mais ces variables n'ont pas été enregistrées
  NouveauDataModule.Free ;
  NouveauModule.Free;
  AppProjetIni.Free;
end.
