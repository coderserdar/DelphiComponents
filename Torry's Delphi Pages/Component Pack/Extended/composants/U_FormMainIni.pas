unit U_FormMainIni;
// Unité de la Version 2 du projet FormMain
// La version 1 TFormMain n'est pas sa fenêtre parente

// Le module crée des propriété servant à la gestion du fichier INI
// Il gère la déconnexion
// Il gère la gestion des touches majuscules et numlock
// Il gère les forms enfants
// créé par Matthieu Giroux en décembre 2007

{$I ..\Compilers.inc}
{$I ..\extends.inc}

interface

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

uses
{$IFDEF SFORM}
  CompSuperForm,
{$ENDIF}
{$IFDEF FPC}
        LCLIntf, LCLType, lmessages, SQLDB,
{$ELSE}
  Windows, OleDb, Messages,
{$ENDIF}
{$IFDEF EADO}
  ADODB,
{$ENDIF}
{$IFDEF ZEOS}
  ZConnection,
{$ELSE}
{$ENDIF}
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
{$IFDEF TNT}
  TNTForms,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, fonctions_init, IniFiles;

{$IFDEF VERSIONS}
  const
    gVer_TFormMainIni : T_Version = ( Component : 'Composant Fenêtre principale' ;
                                               FileUnit : 'U_FormMainIni' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'Fiche principale deuxième version.' ;
                                               BugsStory : 'Version 1.1.0.0 : Passage en générique' + #13#10
                                                   + '1.0.0.0 : Gestion INI, de fiches et du clavier.';
                                               UnitType : 3 ;
                                               Major : 1 ; Minor : 1 ; Release : 0 ; Build : 0 );

{$ENDIF}
type

  { TRegGroup }

  TRegGroup = class
  private
    FClassList: TList;
    FAliasList: TStringList;
    FGroupClasses: TList;
    FActive: Boolean;
    function BestClass(AClass: TPersistentClass): TPersistentClass;
  public
    // constrcuteur
    constructor Create(AClass: TPersistentClass);
    // destructeur
    destructor Destroy; override;
    class function BestGroup(Group1, Group2: TRegGroup; AClass: TPersistentClass): TRegGroup;
    // Ajoute un type de classe
    // AClass : type de classe
    procedure AddClass(AClass: TPersistentClass);
    function GetClass(const AClassName: string): TPersistentClass;
{$IFDEF DELPHI}
    procedure GetClasses(Proc: TGetClass);
    procedure UnregisterModuleClasses(Module: HMODULE);
{$ENDIF}
    function InGroup(AClass: TPersistentClass): Boolean;
    procedure RegisterClass(AClass: TPersistentClass);
    procedure RegisterClassAlias(AClass: TPersistentClass; const Alias: string);
    function Registered(AClass: TPersistentClass): Boolean;
    procedure UnregisterClass(AClass: TPersistentClass);
    property Active: Boolean read FActive write FActive;
  end;

  TRegGroups = class
  private
    FGroups: TList;
    FLock: TRTLCriticalSection;
    FActiveClass: TPersistentClass;
    function FindGroup(AClass: TPersistentClass): TRegGroup;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Activate(AClass: TPersistentClass);
    procedure AddClass(ID: Integer; AClass: TPersistentClass);
    function GetClass(const AClassName: string): TPersistentClass;
    function GroupedWith(AClass: TPersistentClass): TPersistentClass;
    procedure GroupWith(AClass, AGroupClass: TPersistentClass);
    procedure Lock;
    procedure RegisterClass(AClass: TPersistentClass);
    procedure RegisterClassAlias(AClass: TPersistentClass; const Alias: string);
    function Registered(AClass: TPersistentClass): Boolean;
    procedure StartGroup(AClass: TPersistentClass);
    procedure Unlock;
    procedure UnregisterClass(AClass: TPersistentClass);
{$IFDEF DELPHI}
    procedure UnregisterModuleClasses(Module: HMODULE);
{$ENDIF}
    property ActiveClass: TPersistentClass read FActiveClass;
  end;

  { TF_FormMainIni }

  TF_FormMainIni = class({$IFDEF TNT}TTntForm{$ELSE}TForm{$ENDIF})
  private
    {$IFDEF SFORM}
    FBoxChilds : TWinControl;
    {$ENDIF}
    { Déclarations privées }
    // Gestion du clavier
    gEv_OldActivate    ,
    gEv_OldDeActivate  : TNotifyEvent ;
    {$IFNDEF FPC}
    gt_Buffer : TKeyboardState;
    {$ENDIF}
    ge_WriteSessionIni,
    ge_ReadSessionIni,
    ge_WriteMainIni,
    ge_ReadMainIni : TIniEvent ;
    // Composant connection ADO

    FConnection, FConnector : TComponent;
    gh_WindowHandle : HWND;
    FAutoIniDB ,
    FAutoIni    : Boolean ;
    // Retourne la connection ADO
    function p_GetConnection: TComponent;
    // Retourne la connection ADO
    function p_GetConnector: TComponent;
    // Changer la date au moment où on quitte
    procedure p_IniQuitte;
    // Test si on va dans la procédure virtuelle d'initialisation INI
    procedure p_TestInitialisationParamIni;
    // Désactive la connection pour édition
    procedure p_CheckInactive;

    procedure p_modalStart ( Aobj_Objet : Tobject );
    procedure p_modalEnded ( Aobj_Objet : Tobject );
  protected
    // Vérification du fait que des propriétés ne sont pas à nil et n'existent pas
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // Termine l'appli sans sauver le fichier IN
    procedure p_TerminateWithoutIni ;
    // Gestion du clavier à la reprise
    procedure p_ApplicationActivate(Sender: TObject);
    procedure p_ApplicationDeActivate(Sender: TObject);
    // Applique la connection ADO à la variable de la propriété
    procedure p_SetConnection(const Value: TComponent);
    // Applique la connection ADO à la variable de la propriété
    procedure p_SetConnector(const Value: TComponent);

    // A appeler si on n'appelle pas le constructeur
    procedure p_CreeFormMainIni (AOwner:TComponent);

    procedure p_FreeChildForms ;
    procedure p_CloseQueryChildForms ( const ab_Free : Boolean );

  public
    { Déclarations publiques }
    gb_ModalStarted ,
    gb_CloseQuery : Boolean ;
    {$IFDEF FPC}
    function ActiveMDIChild : TCustomForm; virtual;
    procedure WindowMinimizeAll(Sender: TObject);
    {$ENDIF}
    procedure DoClose ( var AAction : TCloseAction ); override;
    function CloseQuery: Boolean; override;
    function fb_ReinitWindow ( var afor_Form : TCustomForm ) : Boolean ;
    // Récupère le code déjà tapé d'une toouche à partir du buffer virtuelle et valide ou non la touche
    // Entrée : Numéro de touche
    function fb_GetKeyState(aby_Key: Integer): Boolean;
    // Modifie la touche
    // Entrée : Numéro de touche
    procedure p_SetKeyState(aby_Key: Integer; ab_TurnOn: Boolean);
    // Touche enfoncée
    function IsShortCut(var ao_Msg: {$IFDEF FPC} TLMKey {$ELSE} TWMKey {$ENDIF}): Boolean; override;
    // Libère le fichier ini en sauvant
    procedure p_LibereSauveIni ;
    // Constructeur et destructeur
    Constructor Create ( AOwner : TComponent ); override;
    Destructor Destroy; override;
    {Lit le fichier ini
    pour le composant form TF_FormMainIni
    avec connexion d'une base ADO
    et appel de la procédure p_InitialisationParamIni dans la form si AutoReadIni,
    de la procédure p_IniInitialisation s'il n'existe pas de fichier INI}
    function f_IniGetConfigFile(acco_Conn: TComponent; as_NomConnexion: string): TMemIniFile; virtual;
    function f_GetIniFile : TMemIniFile; virtual;

    // Création d'une form MDI renvoie la form si existe
    // as_FormNom : Nom de la form ; afor_FormClasse : Classe de la form
    function ffor_CreateMDIChild ( const as_FormNom : string ; afor_FormClasse : TFormClass ): TForm; overload ; virtual;
    function fi_FindForm ( const as_FormNom : string ) : Integer; virtual;

    procedure p_SetChildForm ( const afor_Reference: TCustomForm; const  afs_newFormStyle : TFormStyle ); virtual;

    // Procédure appelée quand il n'y a pas de connexion
    procedure p_NoConnexion; virtual;

    // Création d'une form MDI renvoie True si la form existe
    // as_FormNom : Nom de la form ; afor_FormClasse : Classe de la form ; var afor_Reference : Variable de la form
    function fb_CreateMDIChild ( const as_FormNom : string ; afor_FormClasse : TFormClass ; var afor_Reference; const ab_Ajuster : Boolean ): Boolean; overload ; virtual;

    // Création d'une form MDI renvoie True si la form existe
    // as_FormNom : Nom de la form ; afor_FormClasse : Classe de la form ; var afor_Reference : Variable de la form
    function fb_CreateChild ( const as_FormNom, as_FormClasse : string ; const newFormStyle : TFormStyle; const ab_Ajuster : Boolean ; const aico_Icon : TIcon ): Boolean; overload ; virtual;

    function ffor_getForm   ( const as_FormNom, as_FormClasse : string  ): TForm; overload ; virtual;

    function ffor_getForm   ( afor_FormClasse : TFormClass ): TForm; overload ; virtual;

    // Création d'une form MDI avec changement du style Form
    // renvoie True si la form existe
    // as_FormNom : Nom de la form ; afor_FormClasse : Classe de la form ; var afor_Reference : Variable de la form
    function fb_CreateChild ( afor_FormClasse : TFormClass; var afor_Reference : TCustomForm ; const newFormStyle : TFormStyle; const ab_Ajuster : Boolean ; const aico_Icon : TIcon ) : Boolean ; overload ; virtual;

    // Création d'une form modal
    // renvoie True si la form existe
    // afor_FormClasse : Classe de la form ;
    // var afor_Reference : Variable de la form
    // ab_Ajuster : Ajuster automatiquement
    // aact_Action : Action à la Fermeture
    function fb_CreateModal ( afor_FormClasse : TFormClass ; var afor_Reference : TForm ; const ab_Ajuster : Boolean  ; const aact_Action : TCloseAction ) : Boolean ; virtual;

    // changement du style d'une form
    // afor_Reference    : variable de la form
    // newFormStyle      : style    de la form à mettre
    function fb_setNewFormStyle ( const afor_Reference : TCustomForm; const afs_newFormStyle : TFormStyle; const ab_Ajuster : Boolean  ): Boolean ; overload ; virtual;
    function fb_setNewFormStyle ( const afor_Reference : TCustomForm; const afs_FormStyle: TFormStyle ; const ab_Modal : Boolean ; const awst_WindowState : TWindowState ; const apos_Position : TPosition ): Boolean; overload ; virtual;

    // Procédure mettant à jour la procédure virtuelle p_setMajNumResult
    procedure p_MiseAJourMajNumScroll; virtual;

    // Procédures qui sont appelées automatiquement pour l'initialisation et la sauvegarde
    // Initialisation du fichier INI
    procedure p_InitialisationParamIni; virtual;
    // Sauvegarde du fichier INI
    procedure p_SauvegardeParamIni; virtual;
    // Après la Sauvegarde du fichier INI
    procedure p_ApresSauvegardeParamIni; virtual;
    // Non connecté
    procedure p_PbConnexion; virtual;
    // Connecté
    procedure p_Connectee; virtual;
    // Ecriture de l'ini dans le descendant
    procedure p_WriteDescendantIni(const amif_Init: TMemIniFile); virtual;
    // Lecture de l'ini dans le descendant
    procedure p_ReadDescendantIni(const amif_Init: TMemIniFile); virtual;
    // Gestion du clavier
    // Entrée : les trois touches : MAJ NUM SCROLLLOCK
    procedure p_SortieMajNumScroll ( const ab_MajEnfoncee, ab_NumEnfoncee, ab_ScrollEnfoncee : boolean ) ; virtual;
    // Procédure qui initialise la chaine de connexion de FConnexion
  published
    {$IFDEF SFORM}
    property BoxChilds : TWinControl read FBoxChilds write FBoxChilds stored True ;
    {$ENDIF}
    // Propriété connection ADO
    property Connection : TComponent read p_GetConnection write p_SetConnection stored True ;
    property Connector  : TComponent read p_GetConnector write p_SetConnector stored True ;
    property AutoIniDB : Boolean read FAutoIniDB write FAutoIniDB stored True default True ;
    property AutoIni    : Boolean read FAutoIni write FAutoIni stored True default True ;
    property ReadMainIni : TIniEvent read ge_ReadMainIni write ge_ReadMainIni ;
    property WriteMainIni : TIniEvent read ge_WriteMainIni write ge_WriteMainIni ;
    property ReadSessionIni : TIniEvent read ge_ReadSessionIni write ge_ReadSessionIni ;
    property WriteSessionIni : TIniEvent read ge_WriteSessionIni write ge_WriteSessionIni ;

  end;

  // Procédure d'enregistrement des forms propres à l'application
  procedure p_RegisterClass(AClass: TPersistentClass);
  procedure p_RegisterClasses(AClasses: array of TPersistentClass);
  procedure p_UnRegisterClass(AClass: TPersistentClass);
  procedure p_UnRegisterClasses(AClasses: array of TPersistentClass);
{$IFDEF DELPHI}
  procedure p_UnRegisterModuleClasses(Module: HMODULE);
{$ENDIF}
  function fper_FindClass(const ClassName: string): TPersistentClass;
  function fper_GetClass(const AClassName: string): TPersistentClass;
  function fs_EraseNameSoft ( const as_Path : String ) : String ;

var
  gb_MotPasseEstValide : Boolean ;
  gs_NomApp: string;
  gmif_MainFormIniInit: TMemIniFile = nil ;
//  gb_FreeAllWindowsClosed : Boolean = False ;
  gReg_MainFormIniClassesLocales : TRegGroups = nil ;

procedure p_FreeConfigFile;

{$IFDEF EADO}
procedure p_AsynchronousDataSet(adat_DataSet: TCustomADODataset);
{$ENDIF}

implementation

uses fonctions_proprietes, fonctions_erreurs, TypInfo,
{$IFDEF DBEXPRESS}
     SQLExpr,
{$ENDIF}
{$IFDEF EADO}
     AdoConEd, ADOInt,
{$ENDIF}
{$IFDEF ZEOS}
     U_Zconnection,
{$ENDIF}
     unite_messages, fonctions_db, fonctions_string;

{ fonctions }


// Supprime le nom du fichier exe dans le chemin
function fs_EraseNameSoft ( const as_Path : String ) : String ;
Begin
  if gs_NomApp = '' then
    gs_NomApp := fs_GetNameSoft;
  if pos ( gs_Nomapp, as_Path )> 0 then
    Begin
      Result := copy ( as_Path, pos ( gs_nomapp, as_Path ) + length ( gs_NomApp ) + 1, length ( as_Path ) - length (gs_Nomapp)- pos (gs_NomApp, as_Path ));
    End
   else
    Result := as_Path ;

End;

{$IFDEF EADO}



///////////////////////////////////////////////////////////////////////////////////
// Procédure : p_AsyncDataSet
// Description : Mise en mode asynchrone du Dataset
///////////////////////////////////////////////////////////////////////////////////
procedure p_AsynchronousDataSet(adat_DataSet: TCustomADODataset);
begin
  // On passe en mode asynchrone que si Form Main Ini le veut
  if gb_ApplicationAsynchrone
   Then
    Begin
      p_SetComponentProperty ( adat_DataSet, 'CommandTimeOut', tkInteger, gi_IniDatasourceAsynchroneTimeOut );
      adat_DataSet.ExecuteOptions := adat_DataSet.ExecuteOptions + [eoAsyncExecute,eoAsyncFetch,eoAsyncFetchNonBlocking] ;
    End ;
End ;
{$ENDIF}

{ TRegGroup }

// Ajoute un type de classe
// AClass : type de classe
procedure TRegGroup.AddClass(AClass: TPersistentClass);
begin
  FGroupClasses.Add(AClass);
end;

function TRegGroup.BestClass(AClass: TPersistentClass): TPersistentClass;
var
	I: Integer;
	Current: TPersistentClass;
begin
  Result := nil;
  for I := 0 to FGroupClasses.Count - 1 do
  begin
    Current := FGroupClasses[I];
    if AClass.InheritsFrom(Current) then
      if (Result = nil) or Current.InheritsFrom(Result) then
        Result := Current;
  end;
end;

class function TRegGroup.BestGroup(Group1, Group2: TRegGroup;
	AClass: TPersistentClass): TRegGroup;
var
	Group1Class: TPersistentClass;
	Group2Class: TPersistentClass;
begin
  if Group1 <> nil then
    Group1Class := Group1.BestClass(AClass)
   else
    Group1Class := nil;
  if Group2 <> nil then
    Group2Class := Group2.BestClass(AClass)
   else
    Group2Class := nil;
  if Group1Class = nil then
   if Group2Class = nil then
	// AClass is not in either group, no best group
     Result := nil
    else
      // AClass is in Group2 but not Group1, Group2 is best
     Result := Group2
    else
     if Group2Class = nil then
      // AClass is in Group1 but not Group2, Group1 is best
       Result := Group1
      else
       // AClass is in both groups, select the group with the closest ancestor
       if Group1Class.InheritsFrom(Group2Class) then
         Result := Group1
        else
         Result := Group2;
end;

constructor TRegGroup.Create(AClass: TPersistentClass);
begin
  inherited Create;
  FClassList := TList.Create;
  FAliasList := TStringList.Create;
  FGroupClasses := TList.Create;
  FGroupClasses.Add(AClass);
end;

destructor TRegGroup.Destroy;
begin
  inherited Destroy;
  FClassList.Free;
  FAliasList.Free;
  FGroupClasses.Free;
end;

function TRegGroup.GetClass(const AClassName: string): TPersistentClass;
var
	I: Integer;
begin
  if FClassList <> nil then
    for I := 0 to FClassList.Count - 1 do
    begin
      Result := FClassList[I];
      if Result.ClassNameIs(AClassName) then Exit;
    end;
  if FAliasList <> nil then
  begin
    I := FAliasList.IndexOf(AClassName);
    if I >= 0 then
    begin
      Result := TPersistentClass(FAliasList.Objects[I]);
      Exit;
    end;
  end;
  Result := nil;

end;

{$IFDEF DELPHI}
procedure TRegGroup.GetClasses(Proc: TGetClass);
var
  I: Integer;
begin
  for I := 0 to FClassList.Count - 1 do
    Proc(TPersistentClass(FClassList[I]));
end;
{$ENDIF}

function TRegGroup.InGroup(AClass: TPersistentClass): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to FGroupClasses.Count - 1 do
    if AClass.InheritsFrom(TPersistentClass(FGroupClasses[I])) then Exit;
  Result := False;
end;

procedure TRegGroup.RegisterClass(AClass: TPersistentClass);
var
  LClassName: string;
begin
  LClassName := AClass.ClassName;
  if GetClass(LClassName) = nil then
    FClassList.Add(AClass);
end;

procedure TRegGroup.RegisterClassAlias(AClass: TPersistentClass;
  const Alias: string);
begin
  RegisterClass(AClass);
  FAliasList.AddObject(Alias, TObject(AClass));
end;

function TRegGroup.Registered(AClass: TPersistentClass): Boolean;
begin
  Result := FClassList.IndexOf(AClass) >= 0;
end;

procedure TRegGroup.UnregisterClass(AClass: TPersistentClass);
var
  I: Integer;
begin
  FClassList.Remove(AClass);
  for I := FAliasList.Count - 1 downto 0 do
    if FAliasList.Objects[I] = TObject(AClass) then
      FAliasList.Delete(I);
end;

{$IFDEF DELPHI}


function PointerInModule(Ptr: Pointer; Module: HMODULE): Boolean;
begin
  Result := (Module = 0) or (FindHInstance(Ptr) = Module);
end;

procedure TRegGroup.UnregisterModuleClasses(Module: HMODULE);
var
  I: Integer;
begin
  // Even though the group criterion changes we do not need to recalculate the
  // groups because the groups are based on ancestry. If an ancestor of a class
  // is removed because its module was unloaded we can safely assume that all
  // descendents have also been unloaded or are being unloaded as well. This
  // means that any classes left in the registry are not descendents of the
  // classes being removed and, therefore, will not be affected by the change
  // to the FGroupClasses list.
  for I := FGroupClasses.Count - 1 downto 0 do
    if PointerInModule(FGroupClasses[I], Module) then
      FGroupClasses.Delete(I);
	for I := FClassList.Count - 1 downto 0 do
    if PointerInModule(FClassList[I], Module) then
      FClassList.Delete(I);
  for I := FAliasList.Count - 1 downto 0 do
    if PointerInModule(FAliasList.Objects[I], Module) then
      FAliasList.Delete(I);
end;

{$ENDIF}

{ TRegGroups }

procedure TRegGroups.Activate(AClass: TPersistentClass);
var
  I: Integer;
begin
  if FActiveClass <> AClass then
  begin
    FActiveClass := AClass;
    for I := 0 to FGroups.Count - 1 do
      with TRegGroup(FGroups[I]) do
        Active := InGroup(AClass);
  end;
end;

procedure TRegGroups.AddClass(ID: Integer; AClass: TPersistentClass);
begin
  TRegGroup(FGroups[ID]).AddClass(AClass);
end;

constructor TRegGroups.Create;
var
  Group: TRegGroup;
begin
  FGroups := TList.Create;
  inherited Create;
{$IFDEF FPC}InitCriticalSection{$ELSE}InitializeCriticalSection{$ENDIF}(FLock);
	// Initialize default group
  Group := TRegGroup.Create(TPersistent);
  FGroups.Add(Group);
  Group.Active := True;
end;

destructor TRegGroups.Destroy;
var
  I: Integer;
begin
{$IFDEF FPC}System.LeaveCriticalSection{$ELSE}DeleteCriticalSection{$ENDIF}(FLock);
  for I := 0 to FGroups.Count - 1 do
    TRegGroup(FGroups[I]).Free;
  FGroups.Free;
  inherited;
end;

function TRegGroups.FindGroup(AClass: TPersistentClass): TRegGroup;
var
  I: Integer;
  Current: TRegGroup;
begin
  Result := nil;
  for I := 0 to FGroups.Count - 1 do
  begin
    Current := TRegGroup ( FGroups[I] );
    Result := TRegGroup.BestGroup(Current, Result, AClass);
  end;
end;

function TRegGroups.GetClass(const AClassName: string): TPersistentClass;
var
  I: Integer;
begin
  Result := nil;
	for I := 0 to FGroups.Count - 1 do
    with TRegGroup(FGroups[I]) do
    begin
      if Active then Result := GetClass(AClassName);
      if Result <> nil then Exit;
    end;
end;

function TRegGroups.GroupedWith(AClass: TPersistentClass): TPersistentClass;
var
  Group: TRegGroup;
begin
  Result := nil;
  Group := FindGroup(AClass);
	if Group <> nil then
    Result := TPersistentClass(Group.FGroupClasses[0]);
end;

procedure TRegGroups.GroupWith(AClass, AGroupClass: TPersistentClass);

  procedure Error;
  begin
//    raise EFilerError.CreateFmt(SUnknownGroup, [AGroupClass.ClassName]);
  end;

var
  Group: TRegGroup;
  CurrentGroup: TRegGroup;
  CurrentClass: TPersistentClass;
  I, J: Integer;
begin
  Group := FindGroup(AGroupClass);
  if Group = nil then Error;
  Group.AddClass(AClass);

	// The group criterion has changed. We need to recalculate which groups the
  // classes that have already been registered belong to. We can skip
  // Group since we would just be moving a class to a group it already belongs
  // to. We also only need to find the new group of classes that descend from
  // AClass since that is the only criterion being changed. In other words,
  // we only need to move classes that descend from AClass to Group if they
  // are in another group.
  for I := 0 to FGroups.Count - 1 do
  begin
    CurrentGroup := TRegGroup ( FGroups[I]^ );
    if CurrentGroup <> Group then
      for J := CurrentGroup.FClassList.Count - 1 downto 0 do
      begin
	CurrentClass := TPersistentClass ( CurrentGroup.FClassList[J]^ );
        if CurrentClass.InheritsFrom(AClass) then
					// Check CurrentClass should be put into Group based on the new
          // criterion. Their might be a descendent of AClass registered that
          // overrides Group's criterion.
          if FindGroup(CurrentClass) = Group then
          begin
            CurrentGroup.FClassList.Delete(J);
            Group.FClassList.Add(CurrentClass);
          end;
      end;
  end;
end;

procedure TRegGroups.Lock;
begin
{$IFDEF FPC}
  System.
{$ENDIF}
  EnterCriticalSection(FLock);
end;

procedure TRegGroups.RegisterClass(AClass: TPersistentClass);
var
  Group: TRegGroup;
begin
  Group := FindGroup(AClass);
  if Group <> nil then Group.RegisterClass(AClass);
end;

procedure TRegGroups.RegisterClassAlias(AClass: TPersistentClass;
  const Alias: string);
var
  Group: TRegGroup;
begin
  Group := FindGroup(AClass);
  if Group <> nil then Group.RegisterClassAlias(AClass, Alias);
end;

function TRegGroups.Registered(AClass: TPersistentClass): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to FGroups.Count - 1 do
    if TRegGroup(FGroups[I]).Registered(AClass) then Exit;
  Result := False;
end;

procedure TRegGroups.StartGroup(AClass: TPersistentClass);
var
  I: Integer;
begin
  // Do not start a group that already exists
  for I := 0 to FGroups.Count - 1 do
    if TRegGroup(FGroups[I]).FGroupClasses.IndexOf(AClass) >= 0 then
      Exit;
	// Create the group
  FGroups.Add(TRegGroup.Create(AClass));
end;

procedure TRegGroups.Unlock;
begin
{$IFDEF FPC}
  System.
{$ENDIF}
  EnterCriticalSection(FLock);
end;

procedure TRegGroups.UnregisterClass(AClass: TPersistentClass);
var
  I: Integer;
begin
  for I := 0 to FGroups.Count - 1 do
    TRegGroup(FGroups[I]).UnregisterClass(AClass);
end;

{$IFDEF DELPHI}

procedure TRegGroups.UnregisterModuleClasses(Module: HMODULE);
var
  I: Integer;
  Group: TRegGroup;
begin
  for I := FGroups.Count - 1 downto 0 do
  begin
    Group := TRegGroup(FGroups[I]);
    Group.UnregisterModuleClasses(Module);
    if Group.FGroupClasses.Count = 0 then
    begin
      Group.Free;
      FGroups.Delete(I);
    end;
  end;
end;

procedure p_UnRegisterModuleClasses(Module: HMODULE);
begin
  gReg_MainFormIniClassesLocales.Lock;
  try
    gReg_MainFormIniClassesLocales.UnregisterModuleClasses(Module);
  finally
    gReg_MainFormIniClassesLocales.Unlock;
	end;
end;
{$ENDIF}

  // Procédure d'enregistrement d'une forms propre à l'application
  procedure p_RegisterClass(AClass: TPersistentClass);
begin
  gReg_MainFormIniClassesLocales.Lock;
  try
    while not gReg_MainFormIniClassesLocales.Registered(AClass) do
     begin
      gReg_MainFormIniClassesLocales.RegisterClass(AClass);
      if AClass = TPersistent then Break;
      AClass := TPersistentClass(AClass.ClassParent);
     end;
  finally
    gReg_MainFormIniClassesLocales.Unlock;
  end;
end;

	// Procédure d'enregistrement des forms propres à l'application
procedure p_RegisterClasses(AClasses: array of TPersistentClass);
var
  I: Integer;
begin
  for I := Low(AClasses) to High(AClasses) do p_RegisterClass(AClasses[I]);
end;

procedure p_RegisterClassAlias(AClass: TPersistentClass; const Alias: string);
begin
  gReg_MainFormIniClassesLocales.Lock;
  try
    gReg_MainFormIniClassesLocales.RegisterClassAlias(AClass, Alias);
  finally
		gReg_MainFormIniClassesLocales.Unlock;
  end;
end;

procedure p_UnRegisterClass(AClass: TPersistentClass);
begin
  gReg_MainFormIniClassesLocales.Lock;
  try
    gReg_MainFormIniClassesLocales.UnregisterClass(AClass);
	finally
    gReg_MainFormIniClassesLocales.Unlock;
  end;
end;

procedure p_UnRegisterClasses(AClasses: array of TPersistentClass);
var
  I: Integer;
begin
	for I := Low(AClasses) to High(AClasses) do UnRegisterClass(AClasses[I]);
end;

function fper_GetClass(const AClassName: string): TPersistentClass;
begin
  gReg_MainFormIniClassesLocales.Lock;
	try
    Result := gReg_MainFormIniClassesLocales.GetClass(AClassName);
  finally
    gReg_MainFormIniClassesLocales.Unlock;
  end;
end;

function fper_FindClass(const ClassName: string): TPersistentClass;
begin
  Result := fper_GetClass(ClassName);
end;
{------------------------------------------------------------------------------
 ---------------------- Fin Hook clavier pour le maj et le num ----------------
 ------------------------------------------------------------------------------}

{ TF_FormMainIni }

////////////////////////////////////////////////////////////////////////////////
// Constructeur de l'objet TF_FormMainIni
// Initialise le fichier ini
////////////////////////////////////////////////////////////////////////////////
Constructor TF_FormMainIni.Create(AOwner:TComponent);
begin
  FAutoIniDB := True ;
  FAutoIni    := True ;
  {$IFDEF SFORM}
  FBoxChilds := nil;
  {$ENDIF}
  Inherited create  (AOwner);
  p_CreeFormMainIni (AOwner);
end;
// A appeler si on n'appelle pas le constructeur
procedure TF_FormMainIni.p_CreeFormMainIni (AOwner:TComponent);
begin
  gb_CloseQuery := False ;
  gb_ModalStarted := False ;
  gh_WindowHandle := 0;

  if not (csDesigning in ComponentState) then //si on est pas en mode conception
    begin
      {$IFDEF DELPHI}
      (AOwner as TApplication).OnModalBegin := p_modalStart ;
      (AOwner as TApplication).OnModalEnd   := p_modalEnded ;
      {$ENDIF}

      gEv_OldActivate   := (AOwner as TApplication).OnActivate;
      gEv_OldDeActivate := (AOwner as TApplication).OnDeActivate;
      (AOwner as TApplication).OnActivate   := p_ApplicationActivate;
      (AOwner as TApplication).OnDeActivate := p_ApplicationDeActivate;
    end;
  gs_NomApp := fs_GetNameSoft;
  // Lecture des fichiers INI
  if FAutoIniDB Then
    Begin
      if assigned ( FConnection ) Then
        p_setComponentBoolProperty ( FConnection, 'Connected', False );
      if assigned ( FConnector ) Then
        p_setComponentBoolProperty ( FConnector, 'Connected', False );
      f_IniGetConfigFile(FConnector, gs_NomApp);
    End ;
  if FAutoIni Then
    f_GetIniFile ;
End ;
{Écrit le fichier INI pour le composant form TF_FormMainIni.
Appel de la procédure p_SauvegardeParamIni dans la form si AutoWriteIni,
de la procédure Finifile.Free s'il n'existe pas de fichier INI.}
Destructor TF_FormMainIni.Destroy;
begin
  if not (csDesigning in ComponentState) then // si on est pas en mode conception
    begin
      // Libère et sauve le INI
      p_LibereSauveIni;
    end;

  Inherited Destroy;
end;

// Libère le fichier INI en sauvant
procedure TF_FormMainIni.p_LibereSauveIni;
begin
  if Assigned(FIniFile) then
    begin
      // Enregistre la valeur quitte dans le fichier INI
      p_IniQuitte;

      // Appelle la procédure virtuelle
      p_SauvegardeParamIni;

      // Mise à jour du fichier INI
      fb_iniWriteFile ( FIniFile, False );

      // Appelle la procédure virtuelle
      p_ApresSauvegardeParamIni;

      Application.ProcessMessages;
    end;
end;

// Vérification du fait que des propriétés ne sont pas à nil et n'existent pas
procedure TF_FormMainIni.Notification(AComponent: TComponent; Operation: TOperation);
begin
  // Si le composant est détruit
  inherited Notification(AComponent, Operation);

{$IFNDEF FPC}
  if (Assigned(FConnection)) and (AComponent.IsImplementorOf(Connection)) then
    FConnection := nil;
  if (Assigned(FConnector )) and (AComponent.IsImplementorOf(Connector )) then
    FConnector := nil;
{$ENDIF}
end;


////////////////////////////////////////////////////////////////////////////////
//  Evènements de l'application
////////////////////////////////////////////////////////////////////////////////

//  Désactivation de l'application
// Sender : obligatoire ( l'application )
procedure TF_FormMainIni.p_ApplicationActivate(Sender: TObject);
begin
  p_MiseAJourMajNumScroll;
end;

//  DésActivation de l'application
// Sender : obligatoire ( l'application )
procedure TF_FormMainIni.p_ApplicationDeActivate(Sender: TObject);
begin
  // Enregistrer le clavier
  {$IFDEF DELPHI}
  GetKeyBoardState(gt_Buffer);
  {$ENDIF}
end;

// Modifie la touche
// Entrée : Numéro de touche
procedure TF_FormMainIni.p_SetKeyState(aby_Key: Integer; ab_TurnOn: Boolean);
begin
  // Si windows non nt
  {$IFDEF DELPHI}
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then // Win95/98/ME
    begin
      gt_Buffer[aby_Key] := Ord(ab_TurnOn);
      SetKeyboardState(gt_Buffer);
    end
  // Si windows nt
  else if (fb_GetKeyState(aby_Key) <> ab_TurnOn) then // Procédure spécialisée
    begin
      keybd_event(aby_Key, $45, KEYEVENTF_EXTENDEDKEY, 0); // simulate aby_Key press
      keybd_event(aby_Key, $45, KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0); // simulate aby_Key release
    end;
  {$ENDIF}
end;

// Récupère le code déjà tapé d'une toouche à partir du buffer virtuelle et valide ou non la touche
// Entrée : Numéro de touche
function TF_FormMainIni.fb_GetKeyState(aby_Key: Integer): Boolean;
{$IFDEF DELPHI}
var lt_TempBuffer: TKeyboardState;
{$ENDIF}
begin
  // Si Windows non NT
  {$IFDEF DELPHI}
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then // Win95/98/ME
    begin
      GetKeyboardState ( lt_TempBuffer );
      // Le buffer stocke 2 valeurs dans un mot(entier), on récupère la bonne valeur
      Result := lt_TempBuffer [ aby_Key ] and 1 <> 0;
    end
  // Si Windows NT
  else
    // Le buffer ne fonctionne pas bien avec NT la propriété GetGeyState stocke
    // 2 valeurs dans un mot(entier), on récupère la bonne valeur
    Result := GetKeyState(aby_Key) and 1 <> 0;
  {$ELSE}
   Result := False;
  {$ENDIF}
end;

// Met à jour la procédure virtuelle
procedure TF_FormMainIni.p_MiseAJourMajNumScroll;
Begin
  // Procédure virtuelle appelée
  p_SortieMajNumScroll(fb_GetKeyState(VK_CAPITAL),
                       fb_GetKeyState(VK_NUMLOCK),
                       fb_GetKeyState(VK_SCROLL));
End ;

// Procédure qui sera déclenchée lorsqu'une touche sera tapée dans l'application
// En entrée : le message créé quelconque
function TF_FormMainIni.IsShortCut ( var ao_Msg: {$IFDEF FPC} TLMKey {$ELSE} TWMKey {$ENDIF} ) : Boolean;

begin
  Result := inherited IsShortCut ( ao_Msg );
  // Mise à jour des touches spéciales
  p_MiseAJourMajNumScroll ;
end;
{------------------------------------------------------------------------------
 ---------------------- Fin Hook clavier pour le maj et le num ----------------
 ------------------------------------------------------------------------------}

    // Création d'une form MDI renvoie  la form qui existe
function TF_FormMainIni.ffor_CreateMDIChild ( const as_FormNom : string ; afor_FormClasse : TFormClass ) : TForm;
var
 li_existe : integer;
afor_Reference : Pointer ;
begin
  if ( FormStyle <> fsMDIForm )
   Then
    Begin
      Result := nil ;
      Exit ;
    End ;
  li_existe := fi_FindForm ( as_FormNom );

  if  li_existe = - 1 then
    begin
      Application.CreateForm ( afor_FormClasse, afor_Reference );
      Result := TForm ( afor_Reference );
    end
    else
    begin
      (TCustomForm ( Application.Components [li_existe])).BringToFront;
      Result := TForm ( Application.Components [li_existe]);
    end;
end;

    // Création d'une form MDI renvoie  la form qui existe
function TF_FormMainIni.fi_FindForm ( const as_FormNom : string ) : Integer;
var
i : integer;
begin
  Result := -1;
  // Cette recherche ne fonctionne qu'avec les forms mdi child
  for i := 0 to Application.ComponentCount - 1 do
    if ( Application.Components [ i ] is TCustomForm )
    and ( Application.Components [ i ].Name = as_FormNom ) then Result := i;

end;


procedure TF_FormMainIni.p_SetChildForm(const afor_Reference: TCustomForm; const  afs_newFormStyle : TFormStyle );
begin
{$IFDEF SFORM}
  if afor_Reference is TSuperForm Then
    Begin
      Updating;
      if not assigned ( FBoxChilds ) Then
        Begin
          FBoxChilds := TSuperBox.Create(Self);
          with FBoxChilds as TSuperBox do
            Begin
              Parent := Self;
              AutoScroll:=True;
              Align:=alClient;
            end;
        end;
//       afor_Reference.AutoSize := True;
       ( afor_Reference as TSuperForm ).IncrustMode := aicAllClient;
//       afor_Reference.Align := alClient;
       ( afor_Reference as TSuperForm ).ShowIncrust ( FBoxChilds );
       Updated;
     end
   else
{$ENDIF}
  p_SetComponentProperty ( afor_Reference, 'FormStyle', afs_newFormStyle );
  afor_Reference.BringToFront ;
end;

    // Création d'une form MDI renvoie True si la form existe dans les enfants MDI
    // as_FormNom        : nom      de la form
    // afor_FormClasse   : classe   de la form
    // afor_Reference    : variable de la form
    // newFormStyle      : style    de la form à mettre
function TF_FormMainIni.fb_CreateChild ( afor_FormClasse : TFormClass; var afor_Reference : TCustomForm ; const newFormStyle : TFormStyle; const ab_Ajuster : Boolean; const aico_Icon : TIcon  ) : Boolean ;
var
  li_i : integer;
  lico_icon : Ticon ;
begin
  Result := false ;
  afor_Reference := nil ;
    // Recherche sûre de fiches quelconques
  For li_i := Application.ComponentCount - 1 downto 0
   do if (  Application.Components [ li_i ] is TCustomForm )
     and (( Application.Components [ li_i ] as TCustomForm ).ClassType = afor_FormClasse )
    Then
      Begin
        afor_Reference := TCustomForm ( Application.Components [ li_i ] );
        Result := True ;
      End ;

      //Création si nil
  if ( afor_Reference = nil )
    Then
     Begin
      Application.CreateForm ( afor_FormClasse, afor_Reference );
     End ;
  If  assigned ( aico_Icon      )
  and assigned ( afor_Reference )
  and (fobj_getComponentObjectProperty ( afor_Reference, 'Icon' ) is TIcon )
   Then
    Begin
      lico_icon := TIcon ( fobj_getComponentObjectProperty ( afor_Reference, 'Icon' ));
      if assigned ( lico_icon ) then
        Begin
          lico_icon.Modified := False ;
          lico_icon.PaletteModified := False ;
          if lico_icon.Handle <> 0 Then
            Begin
              lico_icon.ReleaseHandle ;
              lico_icon.CleanupInstance ;
            End ;
          lico_icon.Handle := 0 ;
          lico_icon.Assign ( aico_Icon );
          lico_icon.Modified := True ;
          lico_icon.PaletteModified := True ;

          afor_Reference.Invalidate ;

        End;
    End ;
    // Mise à jour de la form
  if Assigned(afor_Reference)
  and ab_Ajuster
  and ( afor_Reference is TCustomForm ) then
    fb_setNewFormStyle ( afor_Reference as TCustomForm , newFormStyle, ab_Ajuster );
end;

// Création d'une form modal
// renvoie True si la form existe
// afor_FormClasse : Classe de la form ;
// var afor_Reference : Variable de la form
// ab_Ajuster : Ajuster automatiquement
// aact_Action : Action à la Fermeture
function TF_FormMainIni.fb_CreateModal ( afor_FormClasse : TFormClass ; var afor_Reference : TForm ; const ab_Ajuster : Boolean  ; const aact_Action : TCloseAction ) : Boolean ;
begin
  Result := false ;
  afor_Reference := ffor_getForm ( afor_FormClasse );

      //Création si nil
 if ( afor_Reference = nil )
   Then
    Begin
     Application.CreateForm ( afor_FormClasse, afor_Reference );
    End
   Else
    Result := True ;
    // Mise à jour de la form

  afor_Reference.FormStyle := fsNormal ;

  afor_Reference.Hide ;
  if ab_Ajuster
   Then
    Begin
      afor_Reference.Position    := poMainFormCenter ;
      afor_Reference.WindowState := wsNormal ;
      afor_Reference.BorderStyle := bsSingle ;
    End ;
  afor_Reference.Update ;
  afor_Reference.ShowModal;
  // On peut effectuer une action de fermeture après avoir montré une fiche modale
  if aact_Action = caFree
   then
    afor_Reference.Free
   else if aact_Action = caHide
    then
     afor_Reference.Hide
    else if aact_Action = caMiniMize
     then
      afor_Reference.WindowState := wsMiniMized ;
end;

// Récupération d'une form renvoie la form si existe dans les enfants
// as_FormNom        : nom      de la form
// as_FormClasse   : classe   de la form
function TF_FormMainIni.ffor_getForm ( const as_FormNom, as_FormClasse: string ): TForm ;
var
  li_i: integer;

begin
  // Initialisation
  Result          := nil ;
//  if (FormStyle <> fsMDIForm) then
//    Exit;

  for li_i := Application.ComponentCount - 1 downto 0 do
    if (Application.Components[li_i] is TForm) and
       ( lowercase (( Application.Components[li_i] as TForm).ClassName ) = lowercase ( as_FormClasse )) then
      begin
        Result := TForm ( Application.Components[li_i] );
      end;
End ;

// Récupération d'une form renvoie la form si existe dans les enfants
// as_FormNom        : nom      de la form
// as_FormClasse   : classe   de la form
function TF_FormMainIni.ffor_getForm ( afor_FormClasse : TFormClass ): TForm;
var
  li_i: integer;

begin
  Result := nil ;
    // Recherche sûre de fiches quelconques
  For li_i := Application.ComponentCount - 1 downto 0
   do if (  Application.Components [ li_i ] is TForm )
     and (( Application.Components [ li_i ] as TForm ).ClassType = afor_FormClasse )
    Then
      Begin
        Result := TForm ( Application.Components [ li_i ] );
      End ;
End ;
// Création d'une form MDI renvoie True si la form existe dans les enfants MDI
// as_FormNom        : nom      de la form
// afor_FormClasse   : classe   de la form
// newFormStyle      : style    de la form à mettre
function TF_FormMainIni.fb_CreateChild(const as_FormNom, as_FormClasse: string; const newFormStyle: TFormStyle; const ab_Ajuster: Boolean; const aico_Icon : TIcon): Boolean;
var
  lp_Reference: Pointer;
  lper_ClasseForm : TComponentClass ;
  lb_Unload : Boolean ;

begin
  // Initialisation
  Result          := False;
  // Recherche la form
  lp_Reference    := ffor_getForm ( as_FormNom, as_FormClasse );

  // Form non trouvée : on crée
  if not Assigned(lp_Reference) then
    Begin
        // Recherche la classe de la form dans cette unité
        lper_ClasseForm := TComponentClass ( fper_FindClass ( as_FormClasse ));

        if Assigned(lper_ClasseForm)
        // Rapide : on a trouvé la form dans cette unité
         Then Application.CreateForm ( lper_ClasseForm              , lp_Reference )
         Else
           Begin
             try
              // Recherche la classe de la form dans delphi
               lper_ClasseForm := TComponentClass ( FindClass ( as_FormClasse ));
             except
             End ;
             // Lent form trouvée dans delphi
             if Assigned(lper_ClasseForm)
              Then
               Application.CreateForm ( lper_ClasseForm, lp_Reference );
           End ;
    // Assigne l'icône si existe
      If assigned ( aico_Icon    )
      and Assigned( lp_Reference )
       Then
        Begin
          ( TForm ( lp_Reference )).Icon.Modified := False ;
          ( TForm ( lp_Reference )).Icon.PaletteModified := False ;
          if ( TForm ( lp_Reference )).Icon.Handle <> 0 Then
            Begin
              ( TForm ( lp_Reference )).Icon.ReleaseHandle ;
              ( TForm ( lp_Reference )).Icon.CleanupInstance ;
            End ;
          ( TForm ( lp_Reference )).Icon.Handle := 0 ;
          ( TForm ( lp_Reference )).Icon.width  := 16 ;
          ( TForm ( lp_Reference )).Icon.Height := 16 ;
          ( TForm ( lp_Reference )).Icon.Assign ( aico_Icon );
          ( TForm ( lp_Reference )).Icon.Modified := True ;
          ( TForm ( lp_Reference )).Icon.PaletteModified := True ;

          ( TForm ( lp_Reference )).Invalidate ;

        End ;

//      ShowMessage('Fiche ' + afor_FormClasse + ' non enregistrée ( Utiliser RegisterClasses dans la création du projet )');
    end
  else
    // On a trouvé la form sans créer
    Result := True ;

    // Paramètre d'affichage
  if Assigned(lp_Reference)
  and ab_Ajuster then
    Begin
      lb_Unload := fb_getComponentBoolProperty ( TComponent( lp_Reference ), 'DataUnload' );
      if not lb_Unload Then
        fb_setNewFormStyle( TForm ( lp_Reference ), newFormStyle, ab_Ajuster)
      else 
        ( TForm ( lp_Reference )).Free ;
    End ;
end;

// Changement du style d'une form
// afor_Reference    : variable de la form
// newFormStyle      : style    de la form à mettre
// Résultat          : Le style a été changé
function TF_FormMainIni.fb_setNewFormStyle(const afor_Reference: TCustomForm; const afs_FormStyle: TFormStyle ; const ab_Modal : Boolean ; const awst_WindowState : TWindowState ; const apos_Position : TPosition ): Boolean;
begin
  Result := False ;
  if not ( assigned ( afor_Reference )) then
    Exit ;
  try
    // Le style a été changé
    Result := True ;

    if TPosition ( flin_getComponentProperty ( afor_Reference, 'Position' )) <> apos_Position Then
      p_SetComponentProperty ( afor_Reference, 'Position', apos_Position );
    if TWindowState ( flin_getComponentProperty ( afor_Reference ,'WindowState' )) <> awst_WindowState Then
      p_SetComponentProperty ( afor_Reference, 'WindowState', awst_WindowState );

    if not ( afs_FormStyle in [ fsMDIChild ]) Then
      p_SetComponentProperty ( afor_Reference, 'FormStyle', afs_FormStyle );

    // Mise à jour
    afor_Reference.Update ;

    // Affectation
    if ab_Modal
    and ( afs_FormStyle in [ fsNormal ]) Then
      begin
        afor_Reference.ShowModal ;
        Exit ;
      end ;

      // Affiche la fiche après les modifs
    if ( afs_FormStyle in [fsMDIChild]) Then
      p_setChildForm ( afor_Reference, afs_FormStyle )
    Else
      afor_Reference.Show;
  Except
  End ;
End ;

// Changement du style d'une form
// afor_Reference    : variable de la form
// newFormStyle      : style    de la form à mettre
// Résultat          : Le style a été changé
function TF_FormMainIni.fb_setNewFormStyle(const afor_Reference: TCustomForm; const afs_newFormStyle: TFormStyle; const ab_Ajuster: Boolean): Boolean;
//var acla_ClasseForm : TClass ;
begin
  Result := False;
  try
  //  acla_ClasseForm := afor_Reference.ClassType ;
    // Style différent
    if (afs_newFormStyle <> TFormStyle ( flin_getComponentProperty ( afor_Reference, 'FormStyle' ))) then
      begin
        // Le style a été changé
        Result := True ;

        // Affectation
        if gb_ModalStarted
        and ( afs_newFormStyle in [fsMDIChild, fsNormal ]) Then
          begin
            if TPosition ( flin_getComponentProperty ( afor_Reference , 'Position' )) <> poMainFormCenter Then
              p_SetComponentProperty ( afor_Reference, 'Position', poMainFormCenter );
            if TWindowState ( flin_getComponentProperty ( afor_Reference , 'WindowState' )) <> wsNormal Then
              p_SetComponentProperty ( afor_Reference, 'WindowState', wsNormal );
            afor_Reference.ShowModal ;
            Exit ;
          end
        Else
          if not ( afs_newFormStyle in [fsMDIChild]) Then
            p_SetComponentProperty ( afor_Reference, 'FormStyle', afs_newFormStyle );
      end;

    {$IFNDEF SFORM}
      // Option ajuster
    if ab_Ajuster
    and Result   then
      begin
      // Par dessus donc au centre
        if ( TFormStyle ( flin_getComponentProperty ( afor_Reference, 'FormStyle' )) = fsStayOnTop)
        and (    (TWindowState ( flin_getComponentProperty ( afor_Reference , 'WindowState' )) <> wsNormal         )
              or ( TPosition ( flin_getComponentProperty ( afor_Reference , 'Position' ))    <> poMainFormCenter )) then
          begin
            p_SetComponentProperty ( afor_Reference, 'Position', poMainFormCenter );
            p_SetComponentProperty ( afor_Reference, 'WindowState', wsNormal );
          end;

          // MDI enfant donc maximizée
        if  not gb_ModalStarted and ( afs_newFormStyle = fsMDIChild) then
          p_SetComponentProperty ( afor_Reference, 'WindowState', wsMaximized );
        // Mise à jour
        afor_Reference.Update ;
      end;
    {$ENDIF}

      // Affiche la fiche après les modifs
    if not gb_ModalStarted and ( afs_newFormStyle in [fsMDIChild]) Then
      p_setChildForm ( afor_Reference, afs_newFormStyle )
    Else
      afor_Reference.Show;
  Except
  End ;
end;

// Création d'une form MDI renvoie la form si existe
// as_FormNom        : nom      de la form
// afor_FormClasse   : classe   de la form
// afor_Reference    : variable de la form
function TF_FormMainIni.fb_CreateMDIChild ( const as_FormNom : string ; afor_FormClasse : TFormClass ; var afor_Reference ; const ab_Ajuster : Boolean ): Boolean;
var
  li_existe : integer;

begin
  Result := False;
  // Pas mdi quitte sinon erreur
  if (FormStyle <> fsMDIForm) then Exit;

  li_existe := fi_FindForm ( as_FormNom );
    // form pas trouvée
  if (li_existe = - 1) then
    begin
      if not Assigned(TForm(afor_Reference)) then
        Application.CreateForm(afor_FormClasse, afor_Reference);
      Result := False;
    end
  else
  // Si trouvée affiche
    begin
      (TCustomForm ( Application.Components[li_existe])).BringToFront;
      Result := True;
    end;
end;

////////////////////////////////////////////////////////////////////////////////
//  En cas de problème sur la base de données
////////////////////////////////////////////////////////////////////////////////
procedure TF_FormMainIni.p_NoConnexion;
begin
  MessageDlg(GS_PB_CONNEXION, mtWarning, [mbOk], 0);
  // Méthode virtuelle
  p_PbConnexion;
end;

//////////////////////////////////////////////////////////////////////////
// Procédure : p_FreeConfigFile
// Description : Libération de l'ini
//////////////////////////////////////////////////////////////////////////
procedure p_FreeConfigFile;
begin
  gmif_MainFormIniInit.Free;
  gmif_MainFormIniInit := nil;
End ;

//////////////////////////////////////////////////////////////////////////
// Procédure virtuelle : p_WriteDescendantIni
// Description : écriture de l'ini dans le descendant
//////////////////////////////////////////////////////////////////////////
procedure TF_FormMainIni.p_WriteDescendantIni ( const amif_Init : TMemIniFile );
begin
End ;

//////////////////////////////////////////////////////////////////////////
// Procédure virtuelle : p_ReadDescendantIni
// Description : lecture de l'ini dans le descendant
//////////////////////////////////////////////////////////////////////////
procedure TF_FormMainIni.p_ReadDescendantIni ( const amif_Init : TMemIniFile );
begin
End ;

// Fonction de gestion du fichier INI avec nom de connexion (le nom de l'exe)
// Entrée : Le nom de la connexion qui en fait est le nom du fichier INI (en gros)
// Renvoie un fichier INI (même si c'est pas très utile) !!!
function TF_FormMainIni.f_IniGetConfigFile(acco_Conn: TComponent; as_NomConnexion: string): TMemIniFile;
begin
  // On considère que par défaut les infos se trouvent dans un fichier INI dont
  // le nom est dérivé du nom de la machine (paramètrable dans l'INI de connexion)
  gs_ModeConnexion := CST_MACHINE;
  p_IniGetDBConfigFile ( gmif_MainFormIniInit,FConnection,acco_Conn,as_NomConnexion);
  p_WriteDescendantIni ( gmif_MainFormIniInit );
  if assigned ( ge_WriteMainIni ) Then
    ge_WriteMainIni ( Self, gmif_MainFormIniInit );
  Result := gmif_MainFormIniInit;
end;

// Fonction de gestion du fichier INI avec nom de connexion (le nom de l'exe)
// Entrée : Le nom de la connexion qui en fait est le nom du fichier INI (en gros)
// Renvoie un fichier INI (même si c'est pas très utile) !!!
// Init. du fichier INI lié à l'utilisateur
function TF_FormMainIni.f_GetIniFile: TMemIniFile;
begin
  Result := f_GetMainMemIniFile(ge_WriteSessionIni, ge_ReadSessionIni, Self);
  // Lit-on le fichier ini par la prcoédure virtuelle ?
  p_TestInitialisationParamIni;

  // Sauvegarde du fichier INI
  fb_iniWriteFile ( Result, False );
end;

// Propriété connection
// Lecture de Fconnection
function TF_FormMainIni.p_GetConnection: TComponent;
begin
  Result := FConnection;
end;
// Propriété connector
// Lecture de Fconnection
function TF_FormMainIni.p_GetConnector: TComponent;
begin
  Result := FConnector;
end;
// Désactive la connection à l'affectation de la connection en conception
procedure TF_FormMainIni.p_CheckInactive;
begin
  // Désactive la connection à l'affectation de la connection en conception
  if ( assigned ( FConnection )) and fb_getComponentBoolProperty ( FConnection, 'Connected' ) and (csDesigning in ComponentState) then
    p_setComponentBoolProperty ( FConnection, 'Connected', False );
end;

// Affectation de la connection
// Désactive la connection  en conception
procedure TF_FormMainIni.p_SetConnection(const Value: TComponent);
begin
  // Gestion de l'objet détruit
{$IFDEF DELPHI}
  ReferenceInterface ( Connection, opRemove );
{$ENDIF}

  if Connection <> Value then
    begin
      // En mode conception le dataset doit être fermé
      if (csDesigning in ComponentState) then p_CheckInactive;
        // Valeur affectée
        FConnection := Value;
    end;

  // Gestion de l'objet détruit
{$IFDEF DELPHI}
  ReferenceInterface ( Connection, opInsert );
{$ENDIF}
end;

// Affectation de la connection
// Désactive la connection  en conception
procedure TF_FormMainIni.p_SetConnector(const Value: TComponent);
begin
  // Gestion de l'objet détruit
{$IFDEF DELPHI}
  ReferenceInterface ( Connector, opRemove );
{$ENDIF}

  if Connector <> Value then
    begin
      // En mode conception le dataset doit être fermé
      if (csDesigning in ComponentState) then p_CheckInactive;
        // Valeur affectée
        FConnector := Value;
    end;

  // Gestion de l'objet détruit
{$IFDEF DELPHI}
  ReferenceInterface ( Connector, opInsert );
{$ENDIF}
end;

// Termine l'appli sans sauver le fichier INi
procedure TF_FormMainIni.p_TerminateWithoutIni ;
Begin
  FiniFile.Free;
  FiniFile := nil;
  Application.Terminate;
End ;

///////////////////////////////////////////////////////////////////////////////
// Test Initialisation du fichier ini dans la prcoédure virtuelle
///////////////////////////////////////////////////////////////////////////////
procedure TF_FormMainIni.p_TestInitialisationParamIni;
begin
  p_InitialisationParamIni;
end;

// Change la date au moment où on quitte
procedure TF_FormMainIni.p_IniQuitte;
begin
  p_IniWriteSectionStr(INISEC_PAR, INIPAR_QUITTE ,'le ' +  DateToStr(Date)  + ' ' +  TimeToStr(Time) );
end;

// Initialisation du fichier ini
procedure TF_FormMainIni.p_InitialisationParamIni;
begin
// procédure réécrite dans le fils
end;

// Sauvegarde ini
procedure TF_FormMainIni.p_SauvegardeParamIni;
begin
// procédure réécrite dans le fils
end;

// Après la sauvegarde ini
procedure TF_FormMainIni.p_ApresSauvegardeParamIni;
begin
// procédure réécrite dans le fils
end;

// Non connecté
procedure TF_FormMainIni.p_PbConnexion;
begin
// procédure réécrite dans le fils
end;

// Connecté
procedure TF_FormMainIni.p_Connectee;
begin
// procédure réécrite dans le fils
end;

 // Gestion du clavier
 // Entrée : les trois touches : MAJ NUM SCROLLLOCK
procedure TF_FormMainIni.p_SortieMajNumScroll ( const ab_MajEnfoncee, ab_NumEnfoncee, ab_ScrollEnfoncee : boolean ) ;
begin
// procédure réécrite dans le fils
end;

function TF_FormMainIni.CloseQuery: Boolean;
begin
  gb_CloseQuery := True ;
  Result := inherited CloseQuery ;
  p_CloseQueryChildForms ( False );
  gb_CloseQuery := Result ;
end;

procedure TF_FormMainIni.p_modalEnded(Aobj_Objet: Tobject);
begin
  gb_ModalStarted := False ;
end;

procedure TF_FormMainIni.p_modalStart(Aobj_Objet: Tobject);
begin
  gb_ModalStarted := True ;
end;

procedure TF_FormMainIni.p_FreeChildForms;
var lw_i : Word ;
begin
  gb_CloseQuery := True ;
  for lw_i := Application.ComponentCount - 1 downto 0 do
    begin
      if (Application.Components[lw_i] is TForm) and
         (Application.Components[lw_i] <> Self) then
        Begin
          (Application.Components[lw_i] as TForm).Free;
          Application.ProcessMessages ;
        End ;
    end;
  gb_CloseQuery := False ;
end;

procedure TF_FormMainIni.p_CloseQueryChildForms ( const ab_Free : Boolean );
var lw_i : Word ;
begin
  gb_CloseQuery := True ;
  for lw_i := Application.ComponentCount - 1 downto 0 do
    begin
      if (Application.Components[lw_i] is TCustomForm) and
         (Application.Components[lw_i] <> Self) then
          if (Application.Components[lw_i] as TCustomForm).CloseQuery
           Then
            Begin
              if ab_Free
               Then
                Application.Components[lw_i].Free;
            End
          Else
            Begin
              MessageDlg ( GS_DECONNECTER_ANNULE, mtInformation, [ mbOK ], 0 );
              Abort ;
            End ;
    end;
  gb_CloseQuery := False ;
end;


function TF_FormMainIni.fb_ReinitWindow(
  var afor_Form: TCustomForm): Boolean;
var lfs_FormStyle: TFormStyle ;
    lb_Modal : Boolean ;
    lwst_WindowState : TWindowState ;
    lcln_FormName   : String ;
    lico_Icone       : TIcon ;
    lpos_Position : Tposition ;
    lclt_ClassType : TClass ;
begin
  Result := False ;
  if  assigned ( FIniFile  )
  and assigned ( afor_Form ) Then
    Begin
      lclt_ClassType := afor_Form.ClassType ;
      lfs_FormStyle  := TFormStyle ( flin_getComponentProperty ( afor_Form ,'FormStyle' ));
      lcln_FormName  := afor_Form.Name ;
      lpos_Position  := TPosition ( flin_getComponentProperty ( afor_Form ,'Position' ));
      lwst_WindowState := afor_Form.WindowState ;
      lb_Modal := gb_ModalStarted ;
      lico_Icone := TIcon.Create ;
      if ( fobj_getComponentObjectProperty ( afor_Form, 'Icon' ) is TIcon ) then
        Begin
          lico_Icone.Assign ( TIcon ( fobj_getComponentObjectProperty ( afor_Form, 'Icon' )));
        End;
      try
        if afor_Form.CloseQuery  Then
          Begin
            afor_Form.Free ;
            afor_Form := Nil ;
            p_IniDeleteSection ( lcln_FormName );
            fb_CreateChild ( TFormClass ( lclt_ClassType ), afor_Form, lfs_FormStyle, False, lico_Icone );
            if assigned ( afor_Form ) Then
              Begin
                fb_setNewFormStyle ( afor_Form, lfs_FormStyle, lb_Modal, lwst_WindowState, lpos_Position );
                Result := True ;
              End ;
          End ;

      finally
        if lico_Icone.HandleAllocated Then
          Begin
            lico_Icone.ReleaseHandle ;
          End ;
        lico_Icone.Free ;
      End ;
    End ;
end;

{$IFDEF FPC}
procedure TF_FormMainIni.WindowMinimizeAll(Sender: TObject);
var li_i : Integer ;
Begin
  for li_i := 0 to Application.ComponentCount -1 do
    Begin
      If  ( Application.Components[ li_i ] <> Application.MainForm )
      and ( Application.Components[ li_i ] is TCustomForm ) Then
        ( Application.Components[ li_i ] as TCustomForm ).WindowState := wsMinimized;
    End ;
End;

function TF_FormMainIni.ActiveMDIChild : TCustomForm;
var li_i : Integer ;
Begin
  Result := Application.MainForm ;
  for li_i := 0 to Application.ComponentCount -1 do
    Begin
      If  ( Application.Components[ li_i ] <> Application.MainForm )
      and ( Application.Components[ li_i ] is TCustomForm )
      and (( Application.Components[ li_i ] as TCustomForm ).Active ) Then
        Result := Application.Components[ li_i ] as TCustomForm ;
    End ;
End;
{$ENDIF}

procedure TF_FormMainIni.DoClose( var AAction: TCloseAction);
begin
  inherited DoClose(AAction);
  try
    if assigned ( FConnection ) Then p_SetComponentBoolProperty( FConnection, 'Connected', False );
    if assigned ( FConnector  ) Then p_SetComponentBoolProperty( FConnector , 'Connected', False );
  finally
  end;
end;



initialization
{$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_TFormMainIni );
{$ENDIF}
  gReg_MainFormIniClassesLocales := TRegGroups.Create ;
finalization
  gReg_MainFormIniClassesLocales.Free ;
  gReg_MainFormIniClassesLocales := Nil ;
  p_FreeConfigFile;
end.
