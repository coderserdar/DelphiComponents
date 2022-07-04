unit fonctions_variant;

interface

{$I ..\Compilers.inc}

{
2004-08-27
Création de l'unité par Matthieu Giroux
}
uses Variants, Classes, ComCtrls, fonctions_version ;

const
    gVer_fonctions_variant : T_Version = ( Component : 'Gestion des variants' ; FileUnit : 'fonctions_variant' ;
                        			                 Owner : 'Matthieu Giroux' ;
                        			                 Comment : 'Fonctions de traduction et de formatage des tableaux de variants ( Variables pouvant changer de type ).' ;
                        			                 BugsStory : 'Version 1.0.2.0 : Fonctions fi_CountArrayVariant et fi_CountArrayVarOption (non testée).' + #13#10
                        			                	        	+'Version 1.0.1.0 : Fonctions avec valeurs de retour correctes.' + #13#10
                        			                	        	+ 'Version 1.0.0.0 : Toutes les fonctions sont OK.';
                        			                 UnitType : 1 ;
                        			                 Major : 1 ; Minor : 0 ; Release : 2 ; Build : 0 );


type
  // On utilise les tableaux de variant pour plus tard :
  // gestion des clés à champs multiples
  tt_TableauVarOption = Array of Record
                        			  var_Cle : Variant ;
                        			  i_Option : Byte ;
                        			 End ;

  tt_TableauVariant = Array of Variant ;
  tset_OctetOptions = set of Byte;
function fb_ListeOrigineAffectee(const at_Liste: tt_TableauVarOption ; const ab_TestBool : Boolean ; const ai_OptionLimitDown : Integer ): Boolean;
function fb_VariantsEqual ( const avar_Compare1, avar_Compare2 : Variant ): Boolean ;
// Cherche avar_ATrouver dans la liste alst_liste
// alst_liste   : la liste de recherche
// ab_VarIsNull : Cherche une valeur null
// avar_ATrouver: la variable à trouver ( peut être une chaîne )
function fi_findInList(const alst_liste: tt_TableauVariant;
  const avar_ATrouver: Variant ; const ab_VarIsNull : Boolean ): Integer;

function fi_CountArrayVariant   (const at_Array: tt_TableauVariant) : integer ;
function fi_CountArrayVarOption (const at_Array: tt_TableauVarOption; const ab_TestBool : Boolean ; const ai_Options : tset_OctetOptions ) : integer ;

// Cherche avar_ATrouver dans la liste alst_liste
// alst_liste   : la liste de recherche
// ab_VarIsNull : Cherche une valeur null
// avar_ATrouver: la variable à trouver ( peut être une chaîne )
function fi_findInListVarBool(const alst_liste: tt_TableauVarOption;
  const avar_ATrouver: Variant ; const ab_VarIsNull, ab_TestOption : Boolean; const ai_ValTest : tset_OctetOptions  ): Integer;

// Traduit un tableau de clés de variants en résultat SQL
// as_TexteAjoute  : Le résultat sQL
// at_Liste        : la liste à traduire en SQL
// alst_Cle        : le champ clé correspondant à la liste de clés
function fb_TableauVersSQL ( var as_TexteAjoute : WideString ; const at_Liste : tt_TableauVariant ; const alst_Cle : TStringList ) : Boolean ; overload ;

// Traduit un tableau de clés de variants en résultat SQL
// as_TexteAjoute  : Le résultat sQL
// at_Liste        : la liste à traduire en SQL
// alst_Cle        : le champ clé correspondant à la liste de clés
function fb_TableauVersSQL ( var as_TexteAjoute : WideString ; const at_Liste : tt_TableauVarOption ; const ab_TestOption : Boolean ; const aset_Options : tset_OctetOptions ) : Boolean ; overload ;

// Traduit un tableau de clés de variants en résultat SQL
// as_TexteAjoute  : Le résultat sQL
// at_Liste        : la liste à traduire en SQL
// alst_Cle        : le champ clé correspondant à la liste de clés
// avar_option     : Rectification sur le champ
function fb_TableauVersSQL ( var as_TexteAjoute : WideString ; const at_Liste : tt_TableauVariant ; const alst_Cle : TStringList ; const avar_Option : Variant ) : Boolean ; overload ;

// Ne pas utiliser Traduit un tableau de clés de variants en résultat SQL
// as_TexteAjoute  : Le résultat sQL
// at_Liste        : la liste à traduire en SQL
// alst_Cle        : le champ clé correspondant à la liste de clés
function fb_TableauVersSQL ( var as_TexteAjoute : String ; const at_Liste : tt_TableauVariant ; const alst_Cle : TStringList ) : Boolean ; overload ;

// Ne pas utiliser Traduit un tableau de clés de variants en résultat SQL
// as_TexteAjoute  : Le résultat sQL
// at_Liste        : la liste à traduire en SQL
// alst_Cle        : le champ clé correspondant à la liste de clés
// avar_option     : Rectification sur le champ
function fb_TableauVersSQL ( var as_TexteAjoute : String ; const at_Liste : tt_TableauVariant ; const alst_Cle : TStringList ; const avar_Option : Variant ) : Boolean ; overload ;

// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
procedure p_AjouteListe ( var at_Liste : tt_TableauVariant ; const as_Valeur : String ); overload ;

// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
procedure p_AjouteListe ( var at_Liste : tt_TableauVariant ; const avar_Valeur : Variant ); overload ;

// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
function fi_AjouteListe ( var at_Liste : tt_TableauVariant ; const as_Valeur : String ; const ab_VerifExiste : Boolean ): Integer ; overload ;

// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
function fi_AjouteListe ( var at_Liste : tt_TableauVariant ; const avar_Valeur : Variant ; const ab_VerifExiste : Boolean): Integer ; overload ;

function fi_AjouteListe ( var at_Liste : tt_TableauVarOption ; const avar_Valeur : Variant ; const ab_VerifExiste : Boolean ):Integer;overload;

// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
function fi_EffaceListe ( var at_Liste : tt_TableauVariant ; const avar_Valeur : Variant ) : Integer  ;
function fb_EffaceListe ( var at_Liste : tt_TableauVariant ; const avar_Valeur : Variant ) : Boolean ;

// La liste de variants contient-elle des valeurs
// at_Liste : le tableau de variants
// résultat : True si valeurs non null
function fb_ListeAffectee ( const at_Liste : tt_TableauVariant ) : Boolean ;

// Supprime un item d'un tableau
// at_Liste  : Tableau
// alsi_Item : Item de la liste

implementation

uses fonctions_string, SysUtils ;

// Cherche avar_ATrouver dans la liste alst_liste
// alst_liste   : la liste de recherche
// ab_VarIsNull : Cherche une valeur null
// avar_ATrouver: la variable à trouver ( peut être une chaîne )
function fi_findInList(const alst_liste: tt_TableauVariant;
  const avar_ATrouver: Variant ; const ab_VarIsNull : Boolean ): Integer;
 var li_i : Integer ;
Begin
  Result := -1 ;
  if not ab_VarIsNull
  and ( avar_ATrouver = Null )
   Then
    Exit ;
    // Scrute la liste
  for li_i := 0 to high ( alst_liste ) do
  // c'est un tableau qu'on a affecté ( pas encore mise en place )
   if fb_VariantsEqual ( avar_ATrouver, alst_liste [ li_i ]) Then
     Begin
       Result := li_i ;
       Break ;
     End ;
End ;

// Cherche avar_ATrouver dans la liste alst_liste
// alst_liste   : la liste de recherche
// ab_VarIsNull : Cherche une valeur null
// avar_ATrouver: la variable à trouver ( peut être une chaîne )
function fi_findInListVarBool(const alst_liste: tt_TableauVarOption;
  const avar_ATrouver: Variant ; const ab_VarIsNull, ab_TestOption : Boolean; const ai_ValTest : tset_OctetOptions ): Integer;
 var li_i : Integer ;
Begin
  Result := -1 ;
    // Scrute la liste
  for li_i := 0 to high ( alst_liste ) do
    if ( ab_TestOption and ( alst_liste [ li_i ].i_Option in ai_ValTest ))
    or not ab_TestOption Then
    // c'est un tableau qu'on a affecté ( pas encore mise en place )
     if (( avar_ATrouver = Null ) and not ab_VarIsNull ) or fb_VariantsEqual ( avar_ATrouver, alst_liste [ li_i ].var_Cle ) Then
       Begin
         Result := li_i ;
         Break ;
       End ;
End ;

function fb_VariantsEqual ( const avar_Compare1, avar_Compare2 : Variant ): Boolean ;
var li_j : Integer ;
    ls_Compare1, ls_Compare2 : String ;
Begin
// c'est un tableau qu'on a affecté ( pas encore mise en place )
 Result := False ;
 if VarIsArray ( avar_Compare1 )
  Then
   Begin
     // scrute le tableau
     for li_j := VarArrayLowBound ( avar_Compare2 , 1 ) to  VarArrayHighBound ( avar_Compare2 , 1 ) do
       if avar_Compare2[ li_j ] <> avar_Compare1 [ li_j ]
        Then
         Break
        Else
         if li_j = VarArrayHighBound ( avar_Compare2 , 1 )
          Then
           Begin
             Result := True ;
             Break ;
           End ;
   End
  Else
  // Gestion du null
   If avar_Compare1 = Null
    Then
     Begin
       if avar_Compare1 = avar_Compare2
        Then
         Begin
           // Variable null trouvée
           Result := True ;
         End ;
     End
    Else
     // Sinon on compare des valeurs qui ne doivent pas être null
     if avar_Compare2 <> Null
      Then
       Begin
         // Comparaison
         ls_Compare1 := avar_Compare2;
         ls_Compare2 := avar_Compare1 ;
         if ls_Compare2 = ls_Compare1
          Then
           Begin
             // On a trouvé la valeur
             Result := True ;
           End ;
       End ;
End ;
// Traduit un tableau de clés de variants en résultat SQL
// as_TexteAjoute  : Le résultat sQL
// at_Liste        : la liste à traduire en SQL
// alst_Cle        : le champ clé correspondant à la liste de clés
function fb_TableauVersSQL ( var as_TexteAjoute : String ; const at_Liste : tt_TableauVariant ; const alst_Cle : TStringList ) : Boolean ; overload ;
var ls_Texte : WideString ;
Begin
  ls_Texte := as_TexteAjoute ;
  Result := fb_TableauVersSQL ( ls_Texte, at_Liste, alst_Cle );
  as_TexteAjoute := ls_Texte ;
End ;
// Traduit un tableau de clés de variants en résultat SQL
// as_TexteAjoute  : Le résultat sQL
// at_Liste        : la liste à traduire en SQL
// alst_Cle        : le champ clé correspondant à la liste de clés
function fb_TableauVersSQL ( var as_TexteAjoute : String ; const at_Liste : tt_TableauVariant ; const alst_Cle : TStringList ; const avar_Option : Variant ) : Boolean ;
var ls_Texte : WideString ;
Begin
  ls_Texte := as_TexteAjoute ;
  Result := fb_TableauVersSQL ( ls_Texte, at_Liste, alst_Cle, avar_Option );
  as_TexteAjoute := ls_Texte ;
End ;

function fb_TableauVersSQL ( var as_TexteAjoute : WideString ; const at_Liste : tt_TableauVarOption ; const ab_TestOption : Boolean ; const aset_Options : tset_OctetOptions ) : Boolean ;
var li_i : Integer ;
    lb_PremiereFois : Boolean ;
Begin
  // on a rien
  Result := False ;
  // Pas de texte
  as_TexteAjoute := '' ;
  if Length ( at_Liste ) <= 0
   Then
    Exit ;
  lb_PremiereFois := True ;
  if not VarIsArray ( at_Liste [ 0 ].var_Cle )
   Then
    for li_i := 0 to high ( at_Liste ) do
      Begin
        // On a quelque chose
        if ( at_Liste [ li_i ].var_Cle = Null )
        or ( ab_TestOption and not ( at_Liste [ li_i ].i_Option in aset_Options ))
         Then
          Continue ;
          // on a une clé
        Result := True ;
        // Ajoute
        if lb_PremiereFois // Première ligne
         Then
           Begin
             if VarIsStr ( at_Liste [ li_i ].var_Cle ) // Chaîne
               Then as_TexteAjoute := '''' + fs_StringDBQuote ( at_Liste [ li_i ].var_Cle) + ''''
               Else as_TexteAjoute :=  VarToStr ( at_Liste [ li_i ].var_Cle) ; // AUTRE
           End
         Else
         if VarIsStr ( at_Liste [ li_i ].var_Cle ) // Chaîne
           Then as_TexteAjoute := as_TexteAjoute + ',''' + fs_StringDBQuote ( at_Liste [ li_i ].var_Cle) + ''''
           Else as_TexteAjoute := as_TexteAjoute + ','   +         VarToStr ( at_Liste [ li_i ].var_Cle) ; // AUTRE
        lb_PremiereFois := False ;
      End ;
End ;


function fb_TableauVersSQL ( var as_TexteAjoute : WideString ; const at_Liste : tt_TableauVariant ; const alst_Cle : TStringList ; const avar_Option : Variant ) : Boolean ;
var li_i : Integer ;
    lb_PremiereFois : Boolean ;
Begin
  // on a rien
  Result := False ;
  // Pas de texte
  as_TexteAjoute := '' ;
  if Length ( at_Liste ) <= 0
   Then
    Exit ;
  lb_PremiereFois := True ;
  if not VarIsArray ( at_Liste [ 0 ] )
   Then
    for li_i := 0 to high ( at_Liste ) do
      Begin
        // On a quelque chose
        if at_Liste [ li_i ] = Null
         Then
          Continue ;
          // on a une clé
        Result := True ;
        // Ajoute
        if lb_PremiereFois // Première ligne
         Then
           Begin
             if VarIsNumeric ( avar_Option ) and VarIsStr ( at_Liste [ li_i ] ) Then
               as_TexteAjoute :=  VarToStr ( StrToInt64 ( at_Liste [ li_i ] ) -  avar_Option )// option
             Else if VarIsStr ( at_Liste [ li_i ] ) // Chaîne
               Then as_TexteAjoute := '''' + fs_StringDBQuote ( at_Liste [ li_i ]) + ''''
               Else if VarIsNumeric ( avar_Option ) and VarIsNumeric ( at_Liste [ li_i ] ) Then
                 as_TexteAjoute :=  VarToStr ( at_Liste [ li_i ] -  avar_Option )// option
               Else
                 as_TexteAjoute :=  VarToStr ( at_Liste [ li_i ]) ; // AUTRE
           End
         Else
         if VarIsNumeric ( avar_Option ) and VarIsStr ( at_Liste [ li_i ] ) Then
           as_TexteAjoute := as_TexteAjoute + ','   +         VarToStr ( StrToInt64 ( at_Liste [ li_i ] ) -  avar_Option )// option
         Else if VarIsStr ( at_Liste [ li_i ] ) // Chaîne
           Then as_TexteAjoute := as_TexteAjoute + ',''' + fs_StringDBQuote ( at_Liste [ li_i ]) + ''''
           Else if VarIsNumeric ( avar_Option ) and VarIsNumeric ( at_Liste [ li_i ] ) Then
             as_TexteAjoute := as_TexteAjoute + ','   +         VarToStr ( at_Liste [ li_i ] -  avar_Option )// option
           Else
             as_TexteAjoute := as_TexteAjoute + ','   +         VarToStr ( at_Liste [ li_i ]) ; // AUTRE
        lb_PremiereFois := False ;
      End ;
End ;

// Traduit un tableau de clés de variants en résultat SQL
// as_TexteAjoute  : Le résultat sQL
// at_Liste        : la liste à traduire en SQL
// alst_Cle        : le champ clé correspondant à la liste de clés
function fb_TableauVersSQL ( var as_TexteAjoute : WideString ; const at_Liste : tt_TableauVariant ; const alst_Cle : TStringList ) : Boolean ;
Begin
  Result := fb_TableauVersSQL ( as_TexteAjoute, at_Liste, alst_Cle, Null );
End ;
// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
procedure p_AjouteListe ( var at_Liste : tt_TableauVariant ; const as_Valeur : String );
begin
  fi_AjouteListe ( at_Liste, as_Valeur, True );
End ;
// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
function fi_AjouteListe ( var at_Liste : tt_TableauVariant ; const as_Valeur : String ; const ab_VerifExiste : Boolean ):Integer;
Begin
  Result := -1 ;
   if  ab_VerifExiste
   and ( fi_findInList ( at_Liste, as_Valeur, False ) <> -1 )
    Then
     Exit ;
   Result := fi_findInList ( at_Liste, Null, True );
   If Result = -1
    Then
     Begin
      // On ajoute dans les clés d'exclusion
      SetLength ( at_Liste, high ( at_Liste ) + 2 );


      at_Liste [ high ( at_Liste ) ] := as_Valeur ;
      Result := high ( at_Liste );
     End
    Else
      at_Liste [ Result ] := as_Valeur ;

End ;

// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
procedure p_AjouteListe ( var at_Liste : tt_TableauVariant ; const avar_Valeur : Variant );
begin
  fi_AjouteListe ( at_Liste, avar_Valeur, True );
End ;
// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
function fi_AjouteListe ( var at_Liste : tt_TableauVariant ; const avar_Valeur : Variant ; const ab_VerifExiste : Boolean ):Integer;
Begin
   if  ab_VerifExiste
   and ( fi_findInList ( at_Liste, avar_Valeur, False ) <> -1 )
    Then
      Begin
        Result := -1 ;
        Exit ;
      End ;
   Result := fi_findInList ( at_Liste, Null, True );
   if VarIsArray ( avar_Valeur ) Then
    Begin
    End
   Else
     If Result = -1
      Then
       Begin
        // On ajoute dans les clés d'exclusion
        SetLength ( at_Liste, high ( at_Liste ) + 2 );


        at_Liste [ high ( at_Liste ) ] := avar_Valeur ;
        Result := high ( at_Liste );
       End
      Else
        at_Liste [ Result ] := avar_Valeur ;

End ;

// Ajoute un variant à un tableau de variants
// at_Liste : Le tableau destination
// as_Valeur : La valeur du variant en string
function fi_AjouteListe ( var at_Liste : tt_TableauVarOption ; const avar_Valeur : Variant ; const ab_VerifExiste : Boolean ):Integer;
Begin
   if  ab_VerifExiste
   and ( fi_findInListVarBool ( at_Liste, avar_Valeur, False, False, [] ) <> -1 )
    Then
      Begin
        Result := -1 ;
        Exit ;
      End ;
   Result := fi_findInListVarBool ( at_Liste, Null, True, False, [] );
   if VarIsArray ( avar_Valeur ) Then
    Begin
    End
   Else
     If Result = -1
      Then
       Begin
        // On ajoute dans les clés d'exclusion
        SetLength ( at_Liste, high ( at_Liste ) + 2 );


        at_Liste [ high ( at_Liste ) ].var_Cle := avar_Valeur ;
        Result := high ( at_Liste );
       End
      Else
        at_Liste [ Result ].var_Cle := avar_Valeur ;

End ;


function fb_EffaceListe ( var at_Liste : tt_TableauVariant ; const avar_Valeur : Variant ) : Boolean ;
Begin
  Result := fi_EffaceListe ( at_Liste, avar_Valeur ) <> -1 ;
End ;

function fi_EffaceListe ( var at_Liste : tt_TableauVariant ; const avar_Valeur : Variant ) : Integer ;
Begin
  Result := fi_findInList ( at_Liste, avar_Valeur, False );
  If Result <> -1 Then
    Begin
      at_Liste [ Result ] := Null ;
    End ;
End ;

// La liste de variants contient-elle des valeurs
// at_Liste : le tableau de variants
// résultat : True si valeurs non null
function fb_ListeAffectee ( const at_Liste : tt_TableauVariant ) : Boolean ;
var li_i : integer ;
Begin
 Result := False ;
  // Il peut y avoir des clés à null
  for li_i := 0 to  high ( at_Liste ) do
   // La liste a-t-elle une clé affectée
   if at_Liste [ li_i ] <> Null Then
     Begin
       Result := True ;
       Break ;
     End ;
End ;

// La liste de variants contient-elle des valeurs
// at_Liste : le tableau de variants
// résultat : True si valeurs non null
function fb_ListeOrigineAffectee ( const at_Liste : tt_TableauVarOption ; const ab_TestBool : Boolean ; const ai_OptionLimitDown : Integer ) : Boolean ;
var li_i : integer ;
Begin
 Result := False ;
  // Il peut y avoir des clés à null
  for li_i := 0 to  high ( at_Liste ) do
   // La liste a-t-elle une clé affectée
   if (not ab_TestBool and ( at_Liste [ li_i ].var_Cle <> Null ))
   or ( ab_TestBool and ( at_Liste [ li_i ].i_Option >= ai_OptionLimitDown ) and ( at_Liste [ li_i ].var_Cle <> Null )) Then
     Begin
       Result := True ;
       Break ;
     End ;
End ;

// Compte les variants non null dans La liste de variants
// at_Array : le tableau de variants
// résultat : Nombre de variants non null
function fi_CountArrayVariant (const at_Array: tt_TableauVariant) : integer ;
var li_i : integer ;
Begin
  Result := 0 ;
  for li_i := 0 to high ( at_Array ) do
    if at_Array [ li_i ] <> Null Then
      inc ( Result );
End ;

// Compte les variants non null dans La liste de variants
// at_Array : le tableau de variants
// ab_TestBool : Test supplémentaire
// ai_Options  : Options valide à tester
// résultat : Nombre de variants non null
function fi_CountArrayVarOption (const at_Array: tt_TableauVarOption; const ab_TestBool : Boolean ; const ai_Options : tset_OctetOptions ) : integer ;
var li_i : integer ;
Begin
  Result := 0 ;
  for li_i := 0 to high ( at_Array ) do
    if at_Array [ li_i ].var_Cle <> Null Then
      if ab_TestBool Then
        Begin
          if at_Array [ li_i ].i_Option in ai_Options Then
            inc ( Result );
        End
      Else
        inc ( Result );
End ;

initialization
  p_ConcatVersion ( gVer_fonctions_variant );
finalization
end.

