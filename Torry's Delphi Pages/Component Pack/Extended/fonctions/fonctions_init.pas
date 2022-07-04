// Unité de gestion du fichier INI dépendant de l'unité FormMainIni
// intégrant une form de gestion de fichier INI
unit fonctions_init;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

{$I ..\Compilers.inc}
{$I ..\extends.inc}

interface
uses
{$IFDEF FPC}
     LCLIntf, SQLDB,
{$ELSE}
     Windows, MaskUtils,
{$ENDIF}
{$IFDEF EADO}
     ADODB, AdoConEd,
{$ENDIF}
{$IFDEF ZEOS}
  ZConnection,
{$ENDIF}
     IniFiles, Forms, sysUtils, classes, DB, ComCtrls,
{$IFDEF VIRTUALTREES}
     virtualtrees,
{$ENDIF}
  dialogs, fonctions_version, unite_messages, DBGrids;

type
  TIniEvent = procedure( const afor_MainObject : TObject ; const aini_iniFile : TCustomInifile ) of object;

const
  gVer_fonctions_init : T_Version = ( Component : 'Gestion du fichier INI' ; FileUnit : 'fonctions_init' ;
           Owner : 'Matthieu Giroux' ;
                              Comment : 'Première version de gestion du fichier INI.' + #13#10 + 'Certaines fonctions sont encore utilisées.' ;
           BugsStory : 'Version 1.0.3.0 : Fonction fb_iniWriteFile' + #13#10 +
                      'Version 1.0.2.0 : Fonctions ini pour les listview,dbgrid, et virtualtrees' + #13#10 +
             'Version 1.0.1.0 : Paramètre Utilisateur.' + #13#10 +
             'Version 1.0.0.0 : La gestion est en place.' + #13#10 +
                               'On utilise plus cette unité complètement mais Fenêtre principale puis plus tard Mc Form Main INI.';
           UnitType : 1 ;
           Major : 1 ; Minor : 0 ; Release : 3 ; Build : 0 );
  // Constantes des sections du fichier ini
  INISEC_PAR = 'parametres';
  INISEC_CON = 'connexion';
  INISEC_UTI = 'Utilisateur' ;

  // Paramètres du fichier ini
  INIPAR_CREATION  = 'creation ini';
  INIPAR_LANCEMENT = 'lancement';
  INIPAR_QUITTE    = 'quitte';
  INIPAR_CONNEXION = 'String de connexion';
  INIPAR_ACCESS    = 'String d''acces';

  CST_MACHINE = 'MACHINE';
  CST_INI_DB   = 'db_';
  CST_INI_SOFT   = 'soft_';
  CST_INI_USERS   = 'user_';
  CST_INI_SQL   = 'sql_';
  CST_EXTENSION_INI = '.ini';
  CST_DBEXPRESS = 'DBEXPRESS' ;
  INIVAL_CDE  = 'cde';


////////////////////////////////////////////////////////////////////////////////
//  Fonctions à appeler pour la gestion des fichiers INI
////////////////////////////////////////////////////////////////////////////////

  // Lit la section des commandes et si elle existe la retourne dans donnees (TStrings)
  function Lecture_ini_sauvegarde_fonctions(sauvegarde: string; donnees: Tstrings): Boolean;

  // Construit dans aListe (TStrings) la liste de toutes les valeurs de la section aTache du fichier INI
  function Lecture_ini_tache_fonctions(aTache: string; aListe: TStrings): Boolean;

  // Retourne l'objet FIniFile représentant le fichier INI
  function f_GetMemIniFile(): TMemIniFile;
  function f_GetMainMemIniFile( ae_WriteSessionIni, ae_ReadSessionIni  : TIniEvent ; const acom_Owner : TComponent ): TMemIniFile;

  // Lecture du fichier SQL dans FSQLFile avec gestion du fichier SQL
  // et lecture de requête à partir de la section parent et de de la clé requete.
  function f_LectureSQLFile(parent, requete: string): string;


////////////////////////////////////////////////////////////////////////////////
//  Fonctions appelées ( Utiliser plutôt les fonctions qui les appellent)
////////////////////////////////////////////////////////////////////////////////
  // Retourne le nom d'utilisateur (string) de la session WINDOWS
  function f_IniFWReadUtilisateurSession: string;

  // Retourne le nom d'ordinateur (string)
  function f_IniFWReadComputerName: string;

  // Initialisation de paramètres du fichier INI
  // (appelée quand il n'existe pas de fichier INI ou pas d'ADO)
  procedure p_IniInitialisation;

  // Mise à jour de la date de lancement du fichier ini
  procedure p_IniMAJ;


////////////////////////////////////////////////////////////////////////////////
//  Fonctions standard de gestion de valeur
////////////////////////////////////////////////////////////////////////////////
  // Retourne un entier à partir de la section et de la clé ainsi que de la valeur par défaut
  function f_IniReadSectionInt(aSection: string; aCle: string; aDefaut: integer): integer;

  // Retourne un booléen à partir de la section et de la clé ainsi que de la valeur par défaut
  function f_IniReadSectionBol(aSection: string; aCle: string; aDefaut: Boolean): Boolean;

  // Retourne une chaîne à partir de la section et de la clé ainsi que de la valeur par défaut
  function f_IniReadSectionStr(aSection: string; aCle: string; aDefaut: string): string;
  // Retourne une chaîne à partir de la section et de la clé ainsi que de la valeur par défaut
  function f_IniReadSection(aSection: string): string;
  function f_IniReadGridFromIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const agd_grid : TDBGrid ): Boolean ;
{$IFDEF VIRTUALTREES}
  function f_IniReadVirtualTreeFromIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const abvt_VirtualTree : TBaseVirtualTree ): Boolean ;
  procedure p_IniWriteVirtualTreeToIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const abvt_VirtualTree : TBaseVirtualTree );
{$ENDIF}
  function f_IniReadListViewFromIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const alv_ListView : TListView ): Boolean ;

  procedure p_IniWriteGridToIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const agd_grid : TDBGrid );
  procedure p_IniWriteListViewToIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const alv_ListView : TListView );
  // Ecrit une chaîne dans le fichier déclaré dans FINIFile
  // à partir de la section et de la clé ainsi que de la valeur à donner.
  procedure p_IniWriteSectionStr(aSection, aCle: string; aDonnee: string);

  // Ecrit un booléen dans le fichier déclaré dans FINIFile
  // à partir de la section et de la clé ainsi que de la valeur à donner.
  procedure p_IniWriteSectionBol(aSection, aCle: string; aDonnee: Boolean);

  // Ecrit un entier dans le fichier déclaré dans FINIFile
  // à partir de la section et de la clé ainsi que de la valeur à donner.
  procedure p_IniWriteSectionInt(aSection, aCle: string; aDonnee: integer);

  // Ecrit une chaîne dans le fichier déclaré dans FSQLFile
  // à partir de la section et de la clé ainsi que de la valeur à donner.
  procedure p_SQLWriteSectionStr(aSection, aCle: string; aDonnee: string);

  // Ecrit un booléen dans le fichier déclaré dans FSQLFile
  // à partir de la section et de la clé ainsi que de la valeur à donner.
  procedure p_SQLWriteSectionBol(aSection, aCle: string; aDonnee: Boolean);

  // Ecrit un entier dans le fichier déclaré dans FSQLFile
  // à partir de la section et de la clé ainsi que de la valeur à donner.
  procedure p_SQLWriteSectionInt(aSection, aCle: string; aDonnee: integer);


////////////////////////////////////////////////////////////////////////////////
//  Fonctions standard de gestion de Clé
////////////////////////////////////////////////////////////////////////////////
  // Efface une clé à partir du nom de la section et du nom de la clé.
  procedure p_IniDeleteKey(aSection, aCle: string);

  // Retourne true si la clé aCle de la section aSection existe
  function f_CleExiste(aSection, aCle: string): Boolean;


////////////////////////////////////////////////////////////////////////////////
//  Fonctions standard de gestion de Section
////////////////////////////////////////////////////////////////////////////////
  // Lit toutes les sections et les retourne dans le TStrings
  procedure p_IniReadSections(aStrings: TStrings);

  // Lit une section et la retourne dans le TStrings.
  procedure p_IniReadSection(aSection: string; aStrings: TStrings);

  // Efface une section à partir du nom de la section
  procedure p_IniDeleteSection(aSection: string);

  // Retourne true si la section aSection existe.
  function f_SectionExiste(aSection: string): Boolean;
  function fb_iniWriteFile( const amem_Inifile : TCustomInifile ; const ab_Afficheerreur : Boolean ):Boolean;

{$IFDEF ZEOS}
  function fb_IniSetZConnection ( const asqc_Connection : TZConnection ; const IniFile : TMemIniFile  ) : Boolean ;
{$ENDIF}
{$IFDEF EADO}
  function fb_IniSetADOConnection ( const aacx_Connection : TADOConnection ) : Boolean ;
{$ENDIF}
{$IFDEF FPC}
  function fb_IniSetSQLConnection ( const asqc_Connection : TSQLConnection ) : Boolean ;
{$ENDIF}
function fs_IniSetConnection ( const accx_Connection : TComponent ) : String ;
procedure p_IniGetDBConfigFile( var amif_Init : TMemIniFile ; {$IFNDEF CSV} const acco_ConnAcces, acco_Conn: TComponent;{$ENDIF} const as_NomConnexion: string);

var
  FIniFile: TMemIniFile = nil;
  FSQLFile: TIniFile = nil;
{$IFDEF EADO}
  gb_IniADOSetKeyset : Boolean = False ;
  gb_IniDirectAccessOnServer : Boolean = False ;
  gi_IniDatasourceAsynchroneEnregistrementsACharger : Integer = 300 ;
  gi_IniDatasourceAsynchroneTimeOut : Integer = CST_ASYNCHRONE_TIMEOUT_DEFAUT ;
  gb_ConnexionAsynchrone : Boolean = False ;
  gb_ApplicationAsynchrone : Boolean = False ;
{$ENDIF}
  gs_ModeConnexion : string;
  // Aide Help
  GS_AIDE           : String = 'aide';
  GS_CHEMIN_AIDE    : String = 'CHM\Aide.chm';

implementation

uses TypInfo, fonctions_string,
{$IFDEF ZEOS}
      U_ZConnection,
{$ENDIF}
      fonctions_proprietes ;

{$IFDEF ZEOS}
function fb_IniSetZConnection ( const asqc_Connection : TZConnection; const IniFile : TMemIniFile ) : Boolean ;
Begin
  Result := False ;
  asqc_Connection.Connected:=False;
  fb_InitZConnection( asqc_Connection, IniFile, False );
End;
{$ENDIF}
{$IFDEF EADO}
function fb_IniSetADOConnection ( const aacx_Connection : TADOConnection ) : Boolean ;
Begin
  Result := False ;
  aacx_Connection.Connected:=False;
  aacx_Connection.ConnectionString := f_IniReadSectionStr( 'parametres' ,'String d''acces', '' );
  // Ouverture de la fenêtre de dialogue de connexion
  if ( aacx_Connection.ConnectionString = '' ) Then
    EdiTConnectionString(aacx_Connection) ;
  Result := aacx_Connection.ConnectionString <> '';
End;
{$ENDIF}
{$IFDEF FPC}
function fb_IniSetSQLConnection ( const asqc_Connection : TSQLConnection ) : Boolean ;
Begin
  Result := False ;
  asqc_Connection.Close;
//  fb_InitConnection( asqc_Connection, FIniFile );
End;
{$ENDIF}

function fs_IniSetConnection ( const accx_Connection : TComponent ) : String ;
Begin
  Result := '' ;
{$IFDEF EADO}
  if accx_Connection is TADOConnection then
    Begin
      if EditConnectionString( accx_Connection as TADOConnection ) Then
        Begin
          Result := ( accx_Connection as TADOConnection ).ConnectionString;
        End;
    End;
{$ENDIF}
{$IFDEF ZEOS}
  if accx_Connection is TZConnection then
    Begin
      Result := fb_InitZConnection( accx_Connection as TZConnection, FIniFile, False );
    End;
{$ENDIF}
{$IFDEF FPC}
  if accx_Connection is TSQLConnection then
    Begin
//      Result := fb_InitSelSQLConnection( accx_Connection as TSQLConnection, FIniFile );
    End;
{$ENDIF}

End;

////////////////////////////////////////////////////////////////////////////////
// Lit le nom de toutes les sections d'un fichier INI dans une liste de chaînes
////////////////////////////////////////////////////////////////////////////////
function fb_iniWriteFile( const amem_Inifile : TCustomInifile ; const ab_Afficheerreur : Boolean ):Boolean;
var
    li_Attr : Integer ;
    lt_Arg  : Array [0..1] of {$IFDEF FPC}Shortstring{$ELSE}String{$ENDIF} ;
begin
  Result := False ;
  if assigned ( amem_Inifile ) Then
    Begin
      li_Attr := 0 ;
      try
        {$IFNDEF LINUX}
         li_Attr := FileGetAttr ( amem_Inifile.FileName );
          if  ( li_Attr and SysUtils.faReadOnly <> 0 ) Then
            FileSetAttr ( amem_Inifile.FileName, li_Attr - SysUtils.faReadOnly );
        {$ENDIF}
        amem_Inifile.UpdateFile ;
        Result := True ;
      Except
        on e: Exception do
          if ab_Afficheerreur Then
            Begin
              lt_Arg [0] := amem_Inifile.FileName ;
              lt_Arg [1] := IntToStr ( li_Attr ) ;
              MessageDlg ( fs_RemplaceMsg ( GS_ECRITURE_IMPOSSIBLE, lt_Arg ) + #13#10
                         + GS_DETAILS_TECHNIQUES + #13#10 + e.Message, mtError, [mbOk], 0 );
            End ;
      End ;
    End ;
end;

////////////////////////////////////////////////////////////////////////////////
// Lit le nom de toutes les sections d'un fichier INI dans une liste de chaînes
////////////////////////////////////////////////////////////////////////////////
procedure p_iniReadSections(aStrings: TStrings);
begin
  If assigned ( FIniFile ) Then
    FIniFile.ReadSections(aStrings);
end;

////////////////////////////////////////////////////////////////////////////////
// Lit tous les noms de clés d'une section donnée d'un fichier INI dans une liste de chaîne
////////////////////////////////////////////////////////////////////////////////
procedure p_iniReadSection(aSection: string; aStrings: TStrings);
begin
  If assigned ( FIniFile ) Then
    FIniFile.ReadSection(aSection, aStrings);
end;

////////////////////////////////////////////////////////////////////////////////
// Efface une clé d'une section dans un fichier INI
////////////////////////////////////////////////////////////////////////////////
procedure p_IniDeleteKey(aSection, aCle: string);
begin
  If assigned ( FIniFile ) Then
    FIniFile.DeleteKey(aSection, aCle);
end;

////////////////////////////////////////////////////////////////////////////////
// Efface une section et toutes ses clés dans un fichier INI
////////////////////////////////////////////////////////////////////////////////
procedure p_IniDeleteSection(aSection: string);
begin
  If assigned ( FIniFile ) Then
    FIniFile.EraseSection(aSection);
end;

////////////////////////////////////////////////////////////////////////////////
// Lit un entier dans paramètre du fichier INI
////////////////////////////////////////////////////////////////////////////////
function f_IniReadSectionInt(aSection: string; aCle: string; aDefaut: integer): integer;
begin
  result := FIniFile.ReadInteger(aSection, aCle, aDefaut);
end;
////////////////////////////////////////////////////////////////////////////////
// Lit une valeur string dans paramètre du fichier INI
////////////////////////////////////////////////////////////////////////////////
function f_IniReadSectionStr(aSection :string; aCle :string; aDefaut :string) : string;
begin
  result := FIniFile.Readstring(aSection, aCle, aDefaut);
end;

////////////////////////////////////////////////////////////////////////////////
// Lit une valeur string dans paramètre du fichier INI
////////////////////////////////////////////////////////////////////////////////
function f_IniReadSection(aSection :string) : string;
var lstr_strings : TStringList ;
begin
  lstr_strings := TStringList.create ;
  FIniFile.ReadSection(aSection, lstr_strings);
  result := lstr_strings.Text;
end;

////////////////////////////////////////////////////////////////////////////////
// Lit une valeur booléenne dans paramètre du fichier INI
////////////////////////////////////////////////////////////////////////////////
function f_IniReadSectionBol(aSection: string; aCle: string; aDefaut: Boolean): Boolean;
begin
  result := FIniFile.ReadBool(aSection, aCle, aDefaut);
end;

////////////////////////////////////////////////////////////////////////////////
// Lit le nom de la session
////////////////////////////////////////////////////////////////////////////////
function f_IniFWReadUtilisateurSession: string;
{$IFDEF FPC}
Begin
//     Result := GetCurrentUSerName;
 Result := '';
{$ELSE}
var
  Buffer: array[0..255] of char;    // tableau de 255 caracteres
  BufferSize: DWORD;                // nombre 16 bits non signé  VL_B_Resultat : Boolean;
begin
  BufferSize := sizeOf(Buffer); // (= 256)
  if GetUserName(@buffer, BufferSize) then ; // (lpBuffer: PChar; var nSize: DWORD)
  result := Buffer; // MEP utilisateur
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////
// Lit le nom de la machine
////////////////////////////////////////////////////////////////////////////////
function f_IniFWReadComputerName : string;
{$IFDEF FPC}
Begin
//     Result := GetCumputerName;
 Result := '';
{$ELSE}
var
  Buffer: array[0..255] of char;    // tableau de 255 caracteres
  BufferSize: DWORD;                // nombre 16 bits non signé  VL_B_Resultat : Boolean;
begin
  BufferSize := sizeOf(Buffer); // (= 256)
  if GetComputerName(@buffer, BufferSize) then ; // (lpBuffer: PChar; var nSize: DWORD)
  result := Buffer; // MEP utilisateur
{$ENDIF}
end;

/////////////////////////////////////////////////////////////////////////////////
// Fonction : f_IniReadGridFromIni
// Description : Affecte les tailles de colonnes d'une grille à partir de l'ini
// Paramètres  : aini_IniFile : L'ini
//               as_FormName  : Le nom de la fiche section de l'ini
//               agd_grid     : La grille
//               Retour       : Une colonne au moins a été affectée
/////////////////////////////////////////////////////////////////////////////////

function f_IniReadGridFromIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const agd_grid : TDBGrid ): Boolean ;
var k, li_Width : Integer ;
begin
  Result := False ;
  for k := 0 to agd_grid.Columns.Count - 1 do
    Begin
{$IFDEF FPC}
      li_Width := aini_IniFile.ReadInteger( as_FormName, agd_grid.Name + '.' + (TColumn(agd_grid.Columns[k])).FieldName, agd_grid.Columns[k].Width);
{$ELSE}
      li_Width := aini_IniFile.ReadInteger( as_FormName, agd_grid.Name + '.' + agd_grid.Columns[k].FieldName, agd_grid.Columns[k].Width);
{$ENDIF}
      if li_Width > 0 Then
        Begin
          Result := True ;
          agd_grid.Columns[k].Width := li_Width ;
        End ;
    End ;
end;

/////////////////////////////////////////////////////////////////////////////////
// Fonction : p_IniWriteGridToIni
// Description : Affecte les tailles de colonnes d'une grille vers l'ini
// Paramètres  : aini_IniFile : L'ini
//               as_FormName  : Le nom de la fiche section de l'ini
//               agd_grid     : La grille
/////////////////////////////////////////////////////////////////////////////////
procedure p_IniWriteGridToIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const agd_grid : TDBGrid );
var k : Integer ;
begin
  for k := 0 to agd_grid.Columns.Count - 1 do
{$IFDEF FPC}
    aini_IniFile.WriteInteger ( as_FormName, agd_grid.Name + '.' + (Tcolumn(agd_grid.Columns[k])).FieldName, agd_grid.Columns[k].Width);
{$ELSE}
    aini_IniFile.WriteInteger ( as_FormName, agd_grid.Name + '.' + agd_grid.Columns[k].FieldName, agd_grid.Columns[k].Width);
{$ENDIF}
End ;

/////////////////////////////////////////////////////////////////////////////////
// Fonction : f_IniReadListViewFromIni
// Description : Affecte les tailles de colonnes d'une liste à partir de l'ini
// Paramètres  : aini_IniFile : L'ini
//               as_FormName  : Le nom de la fiche section de l'ini
//               alv_ListView : La liste
//               Retour       : Une colonne au moins a été affectée
/////////////////////////////////////////////////////////////////////////////////
function f_IniReadListViewFromIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const alv_ListView : TListView ): Boolean ;
var k, li_Width : Integer ;
begin
  Result := False ;
  for k := 0 to alv_ListView.Columns.Count - 1 do
    Begin
      li_Width := aini_IniFile.ReadInteger ( as_FormName, alv_ListView.Name + '.' + alv_ListView.Columns[k].Caption, alv_ListView.Columns[k].Width);
      if li_Width > 0 Then
        Begin
          Result := True ;
          alv_ListView.Columns[k].Width := li_Width ;
        End ;
    End ;
end;

/////////////////////////////////////////////////////////////////////////////////
// Fonction : f_IniReadVirtualTreeFromIni
// Description : Affecte les tailles de colonnes d'un arbre à partir de l'ini
// Paramètres  : aini_IniFile : L'ini
//               as_FormName  : Le nom de la fiche section de l'ini
//               abvt_VirtualTree : L'arbre
//               Retour       : Une colonne au moins a été affectée
/////////////////////////////////////////////////////////////////////////////////

{$IFDEF VIRTUALTREES}
function f_IniReadVirtualTreeFromIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const abvt_VirtualTree : TBaseVirtualTree ): Boolean ;
var
  lvt_EnteteArbre : TVTHeader ;
  k, li_Width : Integer ;
begin
  Result := False ;
  lvt_EnteteArbre := nil ;
  if  IsPublishedProp ( abvt_VirtualTree, 'Header'  )
  and  PropIsType     ( abvt_VirtualTree, 'Header' , tkClass)
  and ( GetObjectProp   ( abvt_VirtualTree, 'Header'  ) is TVTHeader ) Then
    lvt_EnteteArbre := TVTHeader ( GetObjectProp   ( abvt_VirtualTree, 'Header'  ));
  if assigned ( lvt_EnteteArbre ) Then
    for k := 0 to lvt_EnteteArbre.Columns.Count - 1 do
      Begin
        li_Width := aini_IniFile.ReadInteger( as_FormName, abvt_VirtualTree.Name + '.' + lvt_EnteteArbre.Columns[k].Text, lvt_EnteteArbre.Columns[k].Width);
        if li_Width > 0 Then
          Begin
            Result := True ;
            lvt_EnteteArbre.Columns[k].Width := li_Width ;
          End ;
      End ;
end;
/////////////////////////////////////////////////////////////////////////////////
// Fonction : p_IniWriteVirtualTreeToIni
// Description : Affecte les tailles de colonnes d'un arbre vers l'ini
// Paramètres  : aini_IniFile : L'ini
//               as_FormName  : Le nom de la fiche section de l'ini
//               abvt_VirtualTree : L'arbre
/////////////////////////////////////////////////////////////////////////////////
procedure p_IniWriteVirtualTreeToIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const abvt_VirtualTree : TBaseVirtualTree );
var k : Integer ;
    lvt_EnteteArbre : TVTHeader ;
begin
  lvt_EnteteArbre := nil ;
  if  IsPublishedProp ( abvt_VirtualTree, 'Header'  )
  and  PropIsType     ( abvt_VirtualTree, 'Header' , tkClass)
  and ( GetObjectProp   ( abvt_VirtualTree, 'Header'  ) is TVTHeader ) Then
    lvt_EnteteArbre := TVTHeader ( GetObjectProp   ( abvt_VirtualTree, 'Header'  ));
  if assigned ( lvt_EnteteArbre ) Then
    for k := 0 to lvt_EnteteArbre.Columns.Count - 1 do
      aini_IniFile.WriteInteger( as_FormName, abvt_VirtualTree.Name + '.' + lvt_EnteteArbre.Columns[k].Text, lvt_EnteteArbre.Columns[k].Width);
End ;
{$ENDIF}

/////////////////////////////////////////////////////////////////////////////////
// Fonction : p_IniWriteListViewToIni
// Description : Affecte les tailles de colonnes d'une liste vers l'ini
// Paramètres  : aini_IniFile : L'ini
//               as_FormName  : Le nom de la fiche section de l'ini
//               alv_ListView : La liste
/////////////////////////////////////////////////////////////////////////////////
procedure p_IniWriteListViewToIni ( const aini_IniFile : TCustomInifile ; const as_FormName : String ; const alv_ListView : TListView );
var k : Integer ;
begin
  for k := 0 to alv_ListView.Columns.Count - 1 do
    aini_IniFile.WriteInteger( as_FormName, alv_ListView.Name + '.' + alv_ListView.Columns[k].Caption, alv_ListView.Columns[k].Width);
End ;

////////////////////////////////////////////////////////////////////////////////
// Ecrit une valeur chaîne dans un fichier INI
////////////////////////////////////////////////////////////////////////////////
procedure p_IniWriteSectionStr(aSection, aCle, aDonnee: string);
begin
  If assigned ( FIniFile ) Then
    FIniFile.WriteString(aSection, aCle, aDonnee);
end;

////////////////////////////////////////////////////////////////////////////////
// Ecrit une valeur booléenne dans un fichier INI
////////////////////////////////////////////////////////////////////////////////
procedure p_iniWriteSectionBol(aSection, aCle: string; aDonnee: Boolean);
begin
  If assigned ( FIniFile ) Then
    FIniFile.WriteBool(aSection, aCle, aDonnee);
end;

////////////////////////////////////////////////////////////////////////////////
// Ecrit un entier dans un fichier INI
////////////////////////////////////////////////////////////////////////////////
procedure p_iniWriteSectionInt(aSection, aCle : string; aDonnee : integer);
begin
  If assigned ( FIniFile ) Then
    FIniFile.WriteInteger(aSection, aCle, aDonnee);
end;

////////////////////////////////////////////////////////////////////////////////
// Initialisation du fichier INI
////////////////////////////////////////////////////////////////////////////////
procedure p_IniInitialisation;
begin
  p_IniWriteSectionStr(INISEC_PAR, INIPAR_CREATION, 'le ' + DateToStr(Date) + ' ' + TimeToStr(Time));
  p_IniMAJ;
end;

////////////////////////////////////////////////////////////////////////////////
// Mise à jour de la date de lancement du fichier INI
////////////////////////////////////////////////////////////////////////////////
procedure p_IniMAJ;
begin
  p_IniWriteSectionStr(INISEC_PAR, INIPAR_LANCEMENT, 'le ' + DateToStr(Date) + ' ' + TimeToStr(Time));
end;

////////////////////////////////////////////////////////////////////////////////
// Retourne le nom du fichier ini
////////////////////////////////////////////////////////////////////////////////
function f_GetMainMemIniFile( ae_WriteSessionIni, ae_ReadSessionIni  : TIniEvent ; const acom_Owner : TComponent ): TMemIniFile;
begin
  if not Assigned(FIniFile) then
    begin

      if gs_ModeConnexion = CST_MACHINE then
        FIniFile := TMemIniFile.Create(fs_getSoftDir + CST_INI_USERS  + f_IniFWReadComputerName + CST_EXTENSION_INI )
      else
        FIniFile := TMemIniFile.Create(fs_getSoftDir + CST_INI_USERS + f_IniFWReadUtilisateurSession + CST_EXTENSION_INI );

      if not FIniFile.SectionExists(INISEC_PAR) then
        Begin
          FIniFile.WriteString(INISEC_PAR, INIPAR_CREATION, 'le ' +  DateToStr(Date)  + ' ' +  TimeToStr(Time));
          if assigned ( ae_WriteSessionIni ) Then
            ae_WriteSessionIni ( acom_Owner, FIniFile );
        End
      else
        if assigned ( ae_ReadSessionIni ) Then
          ae_ReadSessionIni ( acom_Owner, FIniFile );
      FIniFile.WriteString(INISEC_PAR, INIPAR_LANCEMENT , 'le '  + DateToStr(Date)  + ' ' + TimeToStr(Time));
    end
  else
    if assigned ( acom_Owner ) then
      FIniFile.WriteString(INISEC_PAR, INIPAR_LANCEMENT , 'le '  + DateToStr(Date)  + ' ' + TimeToStr(Time));
  result := FIniFile;
end;
function f_GetMemIniFile( ): TMemIniFile;
begin
  Result := f_GetMainMemIniFile ( nil, nil, nil );
end;

function f_SectionExiste(aSection: string): Boolean;
begin
  result := assigned ( FIniFile ) and FIniFile.SectionExists(aSection);
end;

function f_CleExiste(aSection, aCle: string): Boolean;
begin
  result := assigned ( FIniFile ) and FIniFile.ValueExists(aSection, aCle);
end;

procedure p_SQLWriteSectionStr(aSection, aCle: string; aDonnee: string);
begin
  If assigned ( FSQLFile ) Then
    FSQLFile.WriteString(aSection, aCle, aDonnee);
end;

procedure p_SQLWriteSectionBol(aSection, aCle: string; aDonnee: Boolean);
begin
  If assigned ( FSQLFile ) Then
    FSQLFile.WriteBool(aSection, aCle, aDonnee);
end;

procedure p_SQLWriteSectionInt(aSection, aCle: string; aDonnee: integer);
begin
  FSQLFile.WriteInteger(aSection, aCle, aDonnee);
end;

function f_LectureSQLFile(parent, requete: string): string;
begin
  if FSQLFile = nil then
    FSQLFile := TIniFile.Create(fs_getSoftDir + CST_INI_SQL + 'SQLFILE.INI');
  result := FSQLFile.ReadString(parent, requete, 'fichier non trouvé');
end;

function Lecture_ini_sauvegarde_fonctions(sauvegarde: string; donnees: Tstrings): Boolean;
var
  i: integer;
  ligne: string;

begin
  If not assigned ( FIniFile ) Then
    Exit ;
  result := FIniFile.SectionExists(sauvegarde);
  if not result then exit;

  donnees.Clear;
  i := 0;

  while FIniFile.ValueExists(sauvegarde, INIVAL_CDE + IntToStr(i)) do
    begin
      ligne := FIniFile.ReadString(sauvegarde, INIVAL_CDE + IntToStr(i), '');
      donnees.Add(ligne);
      inc(i);
    end;
end;

///////////////////////////////////////////////////////////////////////////////
//  Construit dans aListe la liste de toutes
//  les commandes de la section du fichier INI
///////////////////////////////////////////////////////////////////////////////
function Lecture_ini_tache_fonctions(aTache: string; aListe: TStrings): Boolean;
var
  i: integer;
  Ligne: String;
  Cles: TStrings;

begin
  If not assigned ( FIniFile ) Then
    Exit ;
   result := FIniFile.SectionExists(aTache);
  if not result then exit;

  aListe.Clear;
  Cles := TStringList.Create;
  Cles.Clear;
  FIniFile.ReadSection(aTache, Cles);

  for i := 0 to Cles.Count - 1 do
    begin
      Ligne := FIniFile.ReadString(aTache, Cles.Strings[i], '');
      aListe.Add(Ligne);
    end;
end;

// Fonction de gestion du fichier INI avec nom de connexion (le nom de l'exe)
// Entrée : Le nom de la connexion qui en fait est le nom du fichier INI (en gros)
// Renvoie un fichier INI (même si c'est pas très utile) !!!
procedure p_IniGetDBConfigFile( var amif_Init : TMemIniFile ;{$IFNDEF CSV} const acco_ConnAcces, acco_Conn: TComponent;{$ENDIF} const as_NomConnexion: string);
begin
  if not Assigned(amif_Init) then
    begin
      amif_Init := TMemIniFile.Create(fs_getSoftDir + CST_INI_SOFT + as_NomConnexion + CST_EXTENSION_INI);
    End;
  // Soit on a une connexion ADO
  if Assigned(acco_Conn) then
    begin
      // Connexion à la base d'accès
      p_SetComponentBoolProperty ( acco_Conn, 'Connected', False );

{$IFDEF ZEOS}
      if ( acco_Conn is TZConnection ) Then
        Begin
          fb_IniSetZConnection ( acco_Conn as TZConnection, amif_Init );
          p_SetComponentBoolProperty ( acco_Conn, 'Connected', True );
        End ;
{$ENDIF}
{$IFDEF EADO}
      if not amif_Init.SectionExists(INISEC_PAR) then
        begin
          p_SetComponentProperty ( acco_Conn, 'ConnectionString', '' );
          // Mise à jour des paramètre
          amif_Init.WriteString (INISEC_PAR, INISEC_CON, CST_MACHINE);
          amif_Init.WriteString (INISEC_PAR, GS_AIDE, GS_CHEMIN_AIDE);

          amif_Init.WriteInteger(INISEC_PAR, GS_CONNECTION_TIMEOUT, CST_CONNECTION_TIMEOUT_DEFAUT);
          amif_Init.WriteBool   (INISEC_PAR, GS_ACCES_DIRECT_SERVEUR, gb_IniDirectAccessOnServer );
          amif_Init.WriteBool   (INISEC_PAR, GS_Set_KEYSET, gb_IniADOSetKeyset );
          amif_Init.WriteInteger(INISEC_PAR, GS_MODE_ASYNCHRONE_NB_ENREGISTREMENTS, CST_ASYNCHRONE_NB_ENREGISTREMENTS);
          amif_Init.WriteBool   (INISEC_PAR, GS_MODE_CONNEXION_ASYNCHRONE, GB_ASYNCHRONE_PAR_DEFAUT);
          amif_Init.WriteBool   (INISEC_PAR, GS_MODE_ASYNCHRONE, GB_ASYNCHRONE_PAR_DEFAUT);
          amif_Init.WriteInteger(INISEC_PAR, GS_MODE_ASYNCHRONE_TIMEOUT, CST_ASYNCHRONE_TIMEOUT_DEFAUT);
          // Ouverture de la fenêtre de dialogue de connexion
          if ( acco_Conn is TADOConnection ) Then
            Begin
              EditConnectionString(acco_Conn);
              amif_Init.WriteString (INISEC_PAR, INIPAR_ACCESS, ( acco_Conn as TADOConnection ).ConnectionString);
            End
        end
          else
            if ( acco_Conn is TADOConnection ) Then
              Begin
                ( acco_Conn as TADOConnection ).ConnectionString := amif_Init.Readstring(INISEC_PAR, INIPAR_ACCESS, '');
                if assigned ( acco_ConnAcces ) Then
                  ( acco_ConnAcces as TADOConnection ).ConnectionTimeout := amif_Init.ReadInteger(INISEC_PAR, GS_CONNECTION_TIMEOUT, CST_CONNECTION_TIMEOUT_DEFAUT);
              End;
{$ENDIF}
{$IFDEF DBEXPRESS}
        if ( acco_Conn is TSQLConnection ) Then
          Begin
            ( acco_Conn as TSQLConnection ).LoadParamsFromIniFile( fs_getSoftName + CST_INI_DB + CST_DBEXPRESS + CST_EXTENSION_INI);
            ( acco_Conn as TSQLConnection ).Open ;
          End ;
{$ENDIF}
{$IFDEF FPC}
        if ( acco_Conn is TSQLConnection ) Then
          Begin
            fb_IniSetSQLConnection ( acco_Conn as TSQLConnection );
            ( acco_Conn as TSQLConnection ).Open ;
          End ;
{$ENDIF}
        gs_aide := GS_CHEMIN_AIDE;
        // Mettre à jour le fichier INI
        fb_iniWriteFile ( amif_Init, True );
      end;
    gs_ModeConnexion := amif_Init.Readstring(INISEC_PAR, INISEC_CON, CST_MACHINE);
    gs_aide := amif_Init.Readstring(INISEC_PAR, GS_AIDE, GS_CHEMIN_AIDE);
{$IFDEF EADO}
    gb_ApplicationAsynchrone := amif_Init.ReadBool(INISEC_PAR, GS_MODE_ASYNCHRONE, GB_ASYNCHRONE_PAR_DEFAUT);
    gb_ConnexionAsynchrone := amif_Init.ReadBool(INISEC_PAR, GS_MODE_ASYNCHRONE, GB_ASYNCHRONE_PAR_DEFAUT);
    gi_IniDatasourceAsynchroneEnregistrementsACharger := amif_Init.ReadInteger(INISEC_PAR, GS_MODE_ASYNCHRONE_NB_ENREGISTREMENTS, CST_ASYNCHRONE_NB_ENREGISTREMENTS);
    gi_IniDatasourceAsynchroneTimeOut                 := amif_Init.ReadInteger(INISEC_PAR, GS_MODE_ASYNCHRONE_TIMEOUT, CST_ASYNCHRONE_TIMEOUT_DEFAUT);
    gb_IniDirectAccessOnServer := amif_Init.ReadBool   (INISEC_PAR, GS_ACCES_DIRECT_SERVEUR, gb_IniDirectAccessOnServer );
    gb_IniADOsetKeySet := amif_Init.ReadBool   (INISEC_PAR, GS_Set_KEYSET, gb_IniADOsetKeySet );
{$ENDIF}
{$IFDEF EADO}
  if gb_ConnexionAsynchrone
  and ( acco_ConnAcces is TADOConnection )
  and ( acco_Conn      is TADOConnection ) Then
    Begin
      ( acco_ConnAcces as TADOConnection ).ConnectOptions := coAsyncConnect ;
      ( acco_Conn      as TADOConnection ).ConnectOptions := coAsyncConnect ;
    End ;
{$ENDIF}
end;



initialization
  p_ConcatVersion ( gVer_fonctions_init );
finalization
  FIniFile.Free;
  FIniFile := nil ;
  FSQLFile.Free;
  FSQLFile := nil ;

end.

