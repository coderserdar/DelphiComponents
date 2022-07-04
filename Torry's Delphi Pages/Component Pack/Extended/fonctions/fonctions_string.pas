// unité contenant des fonctions de traitements de chaine
unit fonctions_string;

interface

{$I ..\Compilers.inc}

uses
{$IFDEF FPC}
        LCLIntf, MaskEdit,
{$ELSE}
  Windows, AdoConEd, MaskUtils,
{$ENDIF}
        IniFiles, Forms, SysUtils, StrUtils, Classes, DB, ComCtrls,
  Dialogs, Math, fonctions_version ;

{$IFNDEF FPC}
const
  DirectorySeparator = '\' ;
{$ENDIF}

{$IFDEF FPC}
  function ExtractFileDir ( const as_FilePath : String ) :String;
{$ELSE}
  function fs_Dos2Win( const aText: string): string;
  function fs_Win2Dos( const aText: string): string;
{$ENDIF}
  function fs_EraseFirstDirectory ( const as_Path : String ) :String;
  function fs_EraseSpecialChars( const aText: string): string;
  function fs_getSoftDir : String;
  function fs_ArgConnectString ( const as_connectstring, as_arg: string): string;
  function fb_stringVide ( const aTexte: string): Boolean;
  function fs_stringDbQuote ( const as_Texte: string): string;
  function fs_stringDbQuoteFilterLike ( const as_Texte: string): string;
  function fs_stringDbQuoteLikeSQLServer ( const as_Texte: string): string;
  function fs_stringDate(): string;
  function fs_stringDateTime(const aDateTime: TDateTime; const aFormat: string): Ansistring;
  function fs_stringCrypte( const as_Text: string): string;
  function fs_stringDecrypte( const as_Text: string): string;
  function fs_stringDecoupe( const aTexte: Tstrings; const aSep: string): string;
  function fs_stringChamp( const aString, aSep: string; aNum: Word): string;
  function ft_stringConstruitListe( const aTexte, aSep: string): TStrings;
  function fs_convertionCoordLambertDMS( const aPosition: string; aLongitude: Boolean): string;
  function fe_convertionCoordLambertDD( const aPosition: string): Extended;
  function fe_distanceEntrePointsCoordLambert( const aLatitudeDep, aLongitudeDep, aLatitudeArr, aLongitudeArr: string): Extended;
  function fb_controleDistanceCoordLambert( const aLatitudeDep, aLongitudeDep, aLatitudeArr, aLongitudeArr: string; const aDistance: Extended): Boolean;
  procedure p_ChampsVersListe(var as_ChampsClePrimaire: TStringList; const aws_ClePrimaire : String ; ach_Separateur : Char );
  function fb_ListeVersSQL(var as_TexteAjoute: String; const astl_Liste: TStringList; const ab_EstChaine: Boolean): Boolean;
  function fs_RemplaceMsg(const as_Texte: String; const aTs_arg: Array of String): String;
  function fs_RemplaceEspace ( const as_Texte : String ; const as_Remplace : String ): String ;

  function fs_RepeteChar     ( ach_Caractere : Char ; const ali_Repete : Longint ):String ;
  function fs_RemplaceChar   ( const as_Texte : String ; ach_Origine, ach_Voulu : Char ) : String ;

  function fs_ReplaceChaine( as_Texte : String ; const as_Origine, as_Voulu : string):string;
  function fs_GetBinOfString ( const astr_Source: AnsiString ): String;
  function fs_Lettrage ( const ach_Lettrage: Char;
                         const ai64_Compteur : Int64 ;
                         const ali_TailleLettrage : Longint ): String ;
  function fs_GetNameSoft : {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
const
    gVer_fonction_string : T_Version = ( Component : 'Gestion des chaînes' ; FileUnit : 'fonctions_string' ;
                        			                 Owner : 'Matthieu Giroux' ;
                        			                 Comment : 'Fonctions de traduction et de formatage des chaînes.' ;
                        			                 BugsStory : 'Version 1.0.2.0 : Fonction fs_GetBinOfString.' + #13#10 + #13#10 +
                        			                	        	 'Version 1.0.1.1 : Paramètres constantes plus rapides.' + #13#10 + #13#10 +
                        			                	        	 'Version 1.0.1.0 : Fonction fs_stringDbQuoteFilter qui ne fonctionne pas mais ne provoque pas d''erreur.' + #13#10 + #13#10 +
                        			                	        	 'Version 1.0.0.1 : Rectifications sur p_ChampsVersListe.' + #13#10 + #13#10 +
                        			                	        	 'Version 1.0.0.0 : Certaines fonctions non utilisées sont à tester.';
                        			                 UnitType : 1 ;
                        			                 Major : 1 ; Minor : 0 ; Release : 2 ; Build : 0 );
    CST_ORD_GUILLEMENT = ord ( '''' );
    CST_ORD_POURCENT   = ord ( '%' );
    CST_ORD_ASTERISC   = ord ( '*' );
    CST_ORD_SOULIGNE   = ord ( '_' );
    CST_ORD_OUVRECROCHET   = ord ( '[' );
    CST_ORD_FERMECROCHET   = ord ( ']' );
implementation

{$IFDEF FPC}
uses LCLType, FileUtil ;
{$ELSE}
uses JclStrings ;
{$ENDIF}

function fs_EraseFirstDirectory ( const as_Path : String ) :String;
Begin
  Result := copy ( as_Path, pos ( DirectorySeparator, as_Path ) + 1, length ( as_Path ) - pos ( DirectorySeparator, as_Path ));
end;

///////////////////////////////////////////////////////////////////////////////
//  FONCTIONS de conversion de caractères Dos <=> Windows et vice-versa
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
// fonction : fs_Dos2Win
// Description : Convertit un texte OEM en ANSI
// aText : Le texte OEM
// Résultat : Le texte transformé en ANSI
///////////////////////////////////////////////////////////////////////////////
{$IFDEF DELPHI}
function fs_Dos2Win( const aText: string): string;
begin
  if aText = '' then Exit;
  SetLength(Result, Length(aText));
  OemToChar(PChar(aText), PChar(Result));
end;

///////////////////////////////////////////////////////////////////////////////
// fonction : fs_Win2Dos
// Description : Convertit un texte ANSI en OEM
// aText : Le texte ANSI
// Résultat : Le texte transformé en OEM
///////////////////////////////////////////////////////////////////////////////
function fs_Win2Dos( const aText: string): string;
begin
  if aText = '' then Exit;
  SetLength(Result, Length(aText));
  CharToOem(PChar(aText), PChar(Result));
end;
{$ENDIF}
///////////////////////////////////////////////////////////////////////////////
// fonction : fs_ArgConnectString
// Description :  Renvoie les données d'un argument d'une chaîne de connexion
// as_connectstring : La chaîne de connexion
// as_arg : Le nom de l'argument à récupérer
// Résultat : Les données de l'argument paramètres
///////////////////////////////////////////////////////////////////////////////
function fs_ArgConnectString ( const as_connectstring, as_arg: string): string;
var
  li_pos: integer;
  ls_chaine: string;

begin
  ls_chaine := as_connectstring;
  li_pos    := Pos(as_arg, ls_chaine);
  ls_chaine := RightStr(ls_chaine, Length(ls_chaine) - (li_pos + Length(as_arg)));
  li_pos    := Pos(';', ls_chaine);
  if li_pos > 0 then
    Result := LeftStr(ls_chaine, li_pos - 1)
  else
    Result := ls_chaine;
end;

///////////////////////////////////////////////////////////////////////////////
// fonction : fb_stringVide
// Description :  Renvoie True si le texte est blanc(s) ou NULL
// aTexte : La chaîne à tester
// Résultat : Renvoie True si le texte est blanc(s) ou NULL
///////////////////////////////////////////////////////////////////////////////
function fb_stringVide( const aTexte: string): Boolean;
begin
  Result := (Trim(aTexte) = '') or (aTexte = EmptyStr);
end;

///////////////////////////////////////////////////////////////////////////////
// Fonction : fs_stringDbQuote
// Description : Cette fonction permet de faire le doublement des ' dans une chaîne de caractères
// as_Texte : Le texte avec des ' éventuels
// Résultat : Le texte avec des '' à la place de '
///////////////////////////////////////////////////////////////////////////////
function fs_stringDbQuote ( const as_Texte: string): string;
begin
  result :=AnsiReplaceStr ( as_Texte, '''' , '''''' );
end;

///////////////////////////////////////////////////////////////////////////////
// fonction : fs_stringDbQuoteFilterLike
// Description : création d'un filtrage ADO en fonction de caractères spéciaux
//  Cette fonction permet de faire le doublement des ' dans une chaîne de caractères
// as_Texte : Le texte qui va devenir filtre
// Résultat : Le filtre du texte
///////////////////////////////////////////////////////////////////////////////
function fs_stringDbQuoteFilterLike ( const as_Texte: string): string;
//var li_i : Integer ;
begin
  Result :=AnsiReplaceStr ( as_Texte, '''' , '''''' );
  if  ( Result     <> ''  )
  and ( Result [1] =  '%' )Then
    Result := '*' + copy ( Result, 2, length ( Result ) - 1 );
  if ( Result = '_' )
  or ( Result = '_*' )
  or ( Result = '*' )
  or ( Result = '**' )
  or ( Result = '%' ) Then
    Result := '' ;
{  for li_i := 1 to length ( as_Texte ) do
    case ord ( as_Texte [ li_i ] ) of
      CST_ORD_GUILLEMENT : Result := Result + '''''' ;
      CST_ORD_ASTERISC   : Result := Result + '*' ;
      CST_ORD_SOULIGNE   : Result := Result + '_' ;
      CST_ORD_POURCENT   : Result := Result + '%' ;
    Else
      Result := Result + as_Texte [ li_i ] ;
    End ;}
end;

function fs_stringDbQuoteLikeSQLServer ( const as_Texte: string): string;
var li_i : Integer ;
begin
  Result := '' ;
  for li_i := 1 to length ( as_Texte ) do
    case ord ( as_Texte [ li_i ] ) of
      CST_ORD_GUILLEMENT : Result := Result + '''''' ;
      CST_ORD_OUVRECROCHET : Result := Result + '[[]' ;
      CST_ORD_FERMECROCHET : Result := Result + '[]]' ;
      CST_ORD_ASTERISC : Result := Result + '[*]' ;
      CST_ORD_SOULIGNE : Result := Result + '[_]' ;
      CST_ORD_POURCENT : Result := Result + '[%]' ;
    Else
      Result := Result + as_Texte [ li_i ] ;
    End ;
end;


///////////////////////////////////////////////////////////////////////////////
//  Cette fonction renvoie la date sous le format standard en string
///////////////////////////////////////////////////////////////////////////////
function fs_stringDate(): string;
begin
  result := DateTimeToStr(Now);
end;

///////////////////////////////////////////////////////////////////////////////
//  Cette fonction renvoie la date ou l'heure sous un format précis en string
///////////////////////////////////////////////////////////////////////////////
function fs_stringDateTime( const aDateTime: TDateTime; const aFormat: string): Ansistring;
begin
  DateTimeToString(result, aFormat, aDateTime);
end;

///////////////////////////////////////////////////////////////////////////////
//  Fonction pour crypter une chaîne
///////////////////////////////////////////////////////////////////////////////
function fs_stringCrypte( const as_Text: string): string;
var
  li_pos, li_i: integer;
  ls_text: string;

begin
  li_i := 62;
  ls_text := as_Text;
  for li_pos := 1 to Length(ls_text) do
    ls_text[li_pos] := Chr(Ord(ls_text[li_pos]) + li_i + li_pos);
  result := ls_text;
end;

///////////////////////////////////////////////////////////////////////////////
//  Fonction pour décrypter une chaîne
///////////////////////////////////////////////////////////////////////////////
function fs_stringDecrypte( const as_Text: string): string;
var
  li_pos, li_i: integer;
  ls_text: string;

begin
  li_i := 62;
  ls_text := as_Text;
  for li_pos := 1 to Length(ls_text) do
    ls_text[li_pos] := Chr(Ord(ls_text[li_pos]) - li_i - li_pos);
  Result := ls_text;
end;

///////////////////////////////////////////////////////////////////////////////
//Fonction qui découpe la chaine suivant le séparateur et renvoie la première partie.
///////////////////////////////////////////////////////////////////////////////
function fs_stringDecoupe( const aTexte: TStrings; const aSep: string): string;
// Cherche la première occurence du séparateur dans la chaine,
// découpe le morceau placé avant et le renvoie.
// La chaine passée en référence ne contient plus que le reste.
var
  i_p: integer;
  s_ret: string;

begin
  // position du séparateur
  i_p := Pos(aSep, aTexte.GetText );

  if i_p = 0 then
    begin
      s_ret := aTexte.GetText;
      aTexte.Text   := '';
    end
  else
    begin
      s_ret := MidStr(aTexte.Strings[0], 1, i_p - 1);
      aTexte.Text := MidStr(aTexte.Text, i_p + Length(aSep), Length(aTexte.GetText));
    end;
  result:= s_ret;
end;

///////////////////////////////////////////////////////////////////////////////
//Fonction ramenant une liste de string en supprimant le séparateur
///////////////////////////////////////////////////////////////////////////////
function ft_stringConstruitListe( const aTexte, aSep: string): TStrings;
var t_liste, t_chaine:TStrings;
begin
  // Exemple:
  // Si aTexte = "aaa;bbbb;cc;ddddd;eeee"
  // et aSep    = ";"
  // alors la fonction renvoie TStrings de 5 lignes
  t_liste := TStringList.Create;
  t_chaine := TStringList.Create;
  t_chaine.Text := aTexte;
  while not fb_stringVide(t_chaine.Text) do
  begin
    t_liste.add(fs_stringDecoupe(t_chaine,aSep));
  end;

  result := t_liste;
  t_liste.Free;
  t_chaine.Free;
end;

///////////////////////////////////////////////////////////////////////////////
//Fonction ramenant le Nieme champ d'une chaîne avec séparateur.
///////////////////////////////////////////////////////////////////////////////
  // Exemple:
  // Si aString = "aaa;bbbb;cc;ddddd;eeee"
  // et aSep    = ";"
  // et aNum    = 3
  // alors la fonction renvoie "cc"
function fs_stringChamp( const aString, aSep: string; aNum: word): string;
var i_pos1, i_pos2, li_compteur: integer;
begin
  // Initialisation
  Result := '';
  li_compteur := 0;
  i_pos1 := 1;
  if aNum < 1 then
    Exit; // Si on cherche à 0 : on quitte

  // Tant qu'on n'est pas rendu à anum et qu'il y a des champs
  while (li_compteur < aNum) and (i_pos1 <> 0) do
    begin
      // Incrémentation
      inc(li_compteur);
      // Si toujours inférieur au suivant
      if li_compteur < aNum then
        begin
          // Incrémente la position au suivant
          i_pos1 := posEx(aSep, aString, i_pos1) + 1;
          // Passe au suivant
          Continue;
        end;
      // Sinon récupération de la position de fin
      i_pos2 := posEx(aSep, aString, i_pos1);
      if i_pos2 = 0 then i_pos2 := Length(aString) + 1;
      // Et de la chaîne incluse
      Result := MidStr(aString, i_pos1, i_pos2 - i_pos1);
    end;
end;

///////////////////////////////////////////////////////////////////////////////
//Fonction de convertion d'une Position degré décimale en degré minutes secondes.
///////////////////////////////////////////////////////////////////////////////
function fs_convertionCoordLambertDMS( const aPosition: string; aLongitude: Boolean): string;
var
  ls_value: Extended;
  ls_Result, ls_mesure, ls_coord: string;

begin
  // Exemple:
  // aPosition = "48.98166666667"
  // aLongitude = true ;dans le cas où il s'agit d'une longitude ou une latitude
  // retourne String = "E 48°58'54''"
  ls_value := StrToFloat (aPosition);
  if aLongitude then
    if ls_value > 0 then
      ls_coord := 'E'
    else
      ls_coord := 'O'
  else
    if ls_value > 0 then
      ls_coord := 'N'
    else
      ls_coord := 'S';

  ls_value := abs(ls_value);

  ls_Result := ls_coord+' ';

  ls_mesure := '°';
  ls_Result := ls_Result + FormatFloat ('00',int(ls_value))+ls_mesure;

  ls_mesure := '''';
  ls_value := Frac(ls_value)*60;
  ls_Result := ls_Result + FormatFloat ('00',int(ls_value))+ls_mesure;

  ls_mesure := '''''';
  ls_value := Frac(ls_value)*60;
  ls_Result := ls_Result + FormatFloat ('00',int(ls_value))+ls_mesure;

  result := ls_Result;
end;

///////////////////////////////////////////////////////////////////////////////
//  Fonction de convertion d'une Position degré minutes secondes en degré décimale
///////////////////////////////////////////////////////////////////////////////
function fe_convertionCoordLambertDD( const aPosition: string): Extended;
var
  ls_string, ls_minutes, ls_degres, ls_secondes, ls_axe: string;
  li_signe, li_pos_deg, li_pos_min, li_pos_sec: integer;

begin
  // Exemple:
  // aPosition = "E 48°58'54''"
  // retourne String = "48.98166666667"
  li_signe :=1;
  ls_string := aPosition;

  ls_axe := MidStr(ls_string,0,1);
  if (ls_axe = 'O') or (ls_axe = 'S') then li_signe := -1;

  li_pos_deg := Pos('°',ls_string);
  li_pos_min := Pos('''',ls_string);
  li_pos_sec := Pos('''''',ls_string);

  ls_degres := MidStr(ls_string,3,li_pos_deg-3);
  ls_minutes  := MidStr(ls_string,li_pos_deg+1,li_pos_min-(li_pos_deg+1));
  ls_secondes:= MidStr(ls_string,li_pos_min+1,li_pos_sec-(li_pos_min+1));

  result := li_signe * (StrToFloat(ls_degres) + (StrToFloat(ls_minutes)/ 60)+(StrToFloat(ls_secondes)/3600));
end;

///////////////////////////////////////////////////////////////////////////////
// Fonction qui calcul la distance entre deux points =  Orthodromie
// Une route orthodromique entre deux points de la surface terrestre est représentée
// par le trajet réél le plus court possible entre ces deux points.
///////////////////////////////////////////////////////////////////////////////
function fe_distanceEntrePointsCoordLambert( const aLatitudeDep, aLongitudeDep, aLatitudeArr, aLongitudeArr: string): Extended;
var le_latitudedep, le_latitudearr, le_longitudedep, le_longitudearr: Extended;
begin
  le_latitudedep  := fe_convertionCoordLambertDD(aLatitudeDep);
  le_latitudearr  := fe_convertionCoordLambertDD(aLatitudeArr);
  le_longitudedep := fe_convertionCoordLambertDD(aLongitudeDep);
  le_longitudearr := fe_convertionCoordLambertDD(aLongitudeArr);

  // 6366 correspond au rayon moyen de la terre en KM.
  // Formule de l'orthodromie :
  // Ortho(A,B)=6366 x acos[cos(LatA) x cos(LatB) x cos(LongB-LongA)+sin(LatA) x sin(LatB)]
  Result := 6366 * ArcCos((sin(DegToRad(le_latitudedep)) * sin(DegToRad(le_latitudearr)))
            + (cos(DegToRad(le_latitudedep)) * cos(DegToRad(le_latitudearr))
            * cos(DegToRad(le_longitudedep) - DegToRad(le_longitudearr))));
end;


///////////////////////////////////////////////////////////////////////////////
// Fonction : fb_controleDistanceCoordLambert
// description : permet de vérifier qu'un point d'arrivée
// se trouve dans le périmètre du cercle dont le centre est le point de départ
// avec un rayon de aDistance KM
// aLatitudeDep : Lattitude de départ
// aLongitudeDep : Longitude de départ
// aLatitudeArr : Lattitude d'arrivée
// aLongitudeArr : Longitude d'arrivée
// aDistance     : Distance minimale reliant les deux points
///////////////////////////////////////////////////////////////////////////////
function fb_controleDistanceCoordLambert( const aLatitudeDep, aLongitudeDep, aLatitudeArr, aLongitudeArr: string; const aDistance: Extended): Boolean;
var le_result: Extended;
begin
  // distance entre les deux points
  le_result := fe_distanceEntrePointsCoordLambert(aLatitudeDep,aLongitudeDep,aLatitudeArr,aLongitudeArr);

  // vérifie si la distance est inférieure ou supérieure au rayon
  if le_result > aDistance then
    result := False
  else
    result := True;
end;

////////////////////////////////////////////////////////////////////////////////
// Procédure   : p_ChampsVersListe
// Description : Création d'une liste à partir d'une chaîne avec des séparateurs
// as_ChampsClePrimaire : Les champs listés en sortie
// as_ClePrimaire       : Les champs en entrée
// as_Separateur        : Le séparateur
////////////////////////////////////////////////////////////////////////////////
procedure p_ChampsVersListe(var as_ChampsClePrimaire: TStringList; const aws_ClePrimaire : String ; ach_Separateur : Char );
var ls_TempoCles: String;
begin
  // Création des champs
  if assigned ( as_ChampsClePrimaire ) Then
    Begin
      as_ChampsClePrimaire.Free;
    End;
  as_ChampsClePrimaire := TStringList.Create;
  ls_TempoCles := aws_ClePrimaire;
  if  pos(ach_Separateur, ls_TempoCles) = 0 then
    // Ajout du champ si un champ
    Begin
      if Trim ( ls_TempoCles ) <> '' Then
        as_ChampsClePrimaire.Add(Trim(ls_TempoCles));
    End
  else
    // si plusieurs champs
    begin
      while pos(ach_Separateur, ls_TempoCles) > 0 do
        begin
          // Ajout des champs
          as_ChampsClePrimaire.Add(Trim(Copy(ls_TempoCles, 1, Pos(ach_Separateur, ls_TempoCles) - 1)));
          ls_TempoCles := Copy(ls_TempoCles, Pos(ach_Separateur, ls_TempoCles) + 1, Length(ls_TempoCles));
        end;
      // Ajout du dernier champ
      as_ChampsClePrimaire.Add(Trim(ls_TempoCles));
    end;
end;

////////////////////////////////////////////////////////////////////////////////
// fonction : fb_ListeVersSQL
// Description : Fonction de transformation d'une string liste en SQL
// as_TexteAjoute : Le texte de la liste séparé par des virgules
// astl_Liste     : La liste
// ab_EstChaine   : La liste de retour sera une liste de chaînes
////////////////////////////////////////////////////////////////////////////////
function fb_ListeVersSQL(var as_TexteAjoute: String; const astl_Liste: TStringList; const ab_EstChaine: Boolean): Boolean;
var li_i: Integer;
begin
  // On a rien
  Result := False;
  // Pas de texte
  as_TexteAjoute := '';
  for li_i := 0 to astl_Liste.Count - 1 do
    begin
      // On a quelque chose
      Result := True;
      if li_i = 0 then // Première ligne
        if ab_EstChaine then // Chaîne
          as_TexteAjoute := '''' + fs_StringDBQuote(astl_Liste[li_i]) + ''''
        else
          as_TexteAjoute := astl_Liste[li_i] // Autre
      else
        if ab_EstChaine then
          as_TexteAjoute := as_TexteAjoute + ',''' + fs_StringDBQuote(astl_Liste[li_i]) + ''''
        else
          as_TexteAjoute := as_TexteAjoute + ',' + astl_Liste[li_i];
    end;
end;

////////////////////////////////////////////////////////////////////////////////
// Fonction : fs_RemplaceMsg
// Description : remplace dans un text @ARG par un tableau d'arguments
// as_Texte : Texte source
// aTs_arg  : Chaînes à mettre à la place de @ARG
// Résultat : la chaîne avec les arguments
////////////////////////////////////////////////////////////////////////////////
function fs_RemplaceMsg(const as_Texte: String; const aTs_arg: Array of String): String;
var
  ls_reduct: String;
  li_pos, li_i: integer;

begin
  Result := '';
  ls_reduct := as_texte;
  li_pos := Pos('@ARG', ls_reduct);
  li_i := 0;

  while li_pos > 0 do
    begin
      Result := Result + LeftStr(ls_reduct, li_pos - 1) + ats_arg[li_i];
      ls_reduct := RightStr(ls_reduct, Length(ls_reduct) - (li_pos + 3));
      li_pos := Pos('@ARG', ls_reduct );
      li_i := li_i + 1;
    end;

  Result := Result + ls_reduct;
end;

////////////////////////////////////////////////////////////////////////////////
// Fonction : fs_RemplaceEspace
// Description : remplace les espaces et le caractère 160 par une chaîne
// as_Texte : Texte source
// as_Remplace : chaîne remplaçant l'espace ou le cractère 160
// Résultat : la chaîne sans les espaces
////////////////////////////////////////////////////////////////////////////////
function fs_RemplaceEspace ( const as_Texte : String ; const as_Remplace : String ): String ;
var lli_i : LongInt ;
Begin

  Result := '' ;
  // scrute la chaîne
  for lli_i := 1 to length ( as_Texte ) do
    Begin
      if  ( as_Texte [ lli_i ] <> ' ' )
      and ( as_Texte [ lli_i ] <> ThousandSeparator {160} ) Then
        Begin
          // la chaîne est retournée comme à l'origine
          Result := Result + as_Texte [ lli_i ];
        End
      Else
        // Le caractère espace est remplacé
        Result := Result + as_Remplace ;
    End ;
End ;

////////////////////////////////////////////////////////////////////////////////
// fonction : fs_RepeteChar
// Description : Répète un carctère n fois
// ach_Caractere  : Le caractère à répéter
// ali_Repete     : Le nombre de répétitions du caractère
// Résultat       : la chaîne avec le caractère répété
////////////////////////////////////////////////////////////////////////////////
function fs_RepeteChar     ( ach_Caractere : Char ; const ali_Repete : Longint ):String ;
var lli_i : Longint ;
Begin
  Result := '' ;
  for lli_i := 1 to ali_Repete do
    Result := Result + ach_Caractere ;
End ;

////////////////////////////////////////////////////////////////////////////////
// fonction : fs_RemplaceChar
// Description : Remplace un caractère par un autre dans une chaîne
// as_Texte       : Le texte à modifier
// ach_Origine    : Le caractère à remplacer
// ach_Voulu      : Le caractère de remplacement
// Résultat       : la chaîne avec le caractère de remplacement
////////////////////////////////////////////////////////////////////////////////
function fs_RemplaceChar   ( const as_Texte : String ; ach_Origine, ach_Voulu : Char ) : String ;
var li_i : Longint ;
Begin
  Result := '' ;
  for li_i := 1 to length ( as_Texte ) do
    if as_Texte [ li_i ] = ach_Origine Then
      Result := Result + ach_Voulu
    Else
      Result := Result + as_Texte [ li_i ];
End ;


////////////////////////////////////////////////////////////////////////////////
// fonction : fs_ReplaceChaine
// Description : Remplace un caractère par un autre dans une chaîne
// as_Texte       : Le texte à modifier
// as_Origine    : La chaîne à remplacer
// as_Voulu      : La chaîne de remplacement
// Résultat       : la chaîne modifiée
////////////////////////////////////////////////////////////////////////////////
function fs_ReplaceChaine( as_Texte : String ; const as_Origine, as_Voulu : string):string;
var li_pos1:integer;
begin
  li_pos1:=pos(as_Origine,as_Texte);

  Result :='';

  while (li_pos1<>0) do
  begin
  Result:= Result +copy(as_Texte,1,li_pos1-1)+ as_Voulu ;
  as_Texte:=copy(as_Texte,li_pos1+length(as_Origine),length(as_Texte)+1-(li_pos1+length(as_Origine)));    //le fait sauf au dernier passage
  li_pos1:=pos(as_Origine,as_Texte);
  end;
  Result := Result +as_Texte;
end;

////////////////////////////////////////////////////////////////////////////////
// Procédure : p_AddBinToString
// Description : Renvoie la version hexadécimale d'une chaine non ansi
// ast8_Abin   : Chaine qui doit être non ansi
// Résultat    : Résultat en hexa
////////////////////////////////////////////////////////////////////////////////

function fs_GetBinOfString ( const astr_Source: AnsiString ): String;
var
  C, L : Integer;
begin
  Result := '';
  if astr_Source <> '' then
  begin
    L := Length(astr_Source);
    C := 1;
    while C <= L do
    begin
      Result := Result + IntToHex( Byte(astr_Source[C]), 2 );
      Inc(C, 1);
    end;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////
// Fonction : fs_Lettrage
// Description : crée un lettrage si le champ compteur est une chaîne
// Paramètres : ach_Lettrage : La lettre du compteur
//              ai64_Compteur : Le nombre du lettrage
//              ali_TailleLettrage : La longueur du champ lettrage
/////////////////////////////////////////////////////////////////////////////////
function fs_Lettrage ( const ach_Lettrage: Char;
                       const ai64_Compteur : Int64 ;
                       const ali_TailleLettrage : Longint ): String ;

Begin
  Result := ach_Lettrage + fs_RepeteChar ( '0', ali_TailleLettrage - length ( IntToStr ( ai64_Compteur )) - 1 ) + IntToStr ( ai64_Compteur );
End ;

function fs_getSoftDir : String;
Begin
  Result := ExtractFileDir( Application.ExeName ) + DirectorySeparator ;
End;

function fs_GetNameSoft : {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
var li_Pos : Integer;
Begin
  Result := ExtractFileName(Application.ExeName);
  li_Pos := Pos ( '.', Result );
  if ( li_Pos > 0 ) then
    Begin
      while PosEx ( '.', Result, li_Pos + 1 )> 0 do
        li_Pos := PosEx ( '.', Result, li_Pos + 1 );
      Result := Copy ( Result, 1, PosEx ( '.', Result, li_Pos )-1 );
    End;
  li_Pos := Pos ( DirectorySeparator, Result );
End;

{$IFDEF FPC}
function ExtractFileDir ( const as_FilePath : String ) :String;
var li_Pos : Integer;
Begin
  Result := as_FilePath;
  li_Pos := Pos ( DirectorySeparator, Result );
  if ( li_Pos > 0 ) then
    Begin
      while PosEx ( DirectorySeparator, Result, li_Pos + 1 )> 0 do
        li_Pos := PosEx ( DirectorySeparator, Result, li_Pos + 1 );
      Result := Copy ( Result, 1, PosEx ( DirectorySeparator, Result, li_Pos ) - 1 );
    End;
End;
{$ENDIF}

function fs_EraseSpecialChars( const aText: string): string;
var li_i : Longint ;
Begin
  for li_i := 1 to length ( aText ) do
    if  ( aText [ li_i ] in ['0'..'9','a'..'z','A'..'Z','-','_'] ) Then
      Result := Result + aText [ li_i ]
     else
    if  ( aText [ li_i ] in [' '] ) Then
      Result := Result + '_';

End;

initialization
  p_ConcatVersion ( gVer_fonction_string );
finalization
end.

