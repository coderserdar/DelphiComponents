unit fonctions_proprietes;

{$I ..\Compilers.inc}

interface
{
2004-08-27
Création de l'unité par Matthieu Giroux
}
uses Variants, TypInfo, Classes,
{$IFDEF DELPHI_9_UP}
     WideStrings ,
{$ENDIF}
     fonctions_version, Graphics ;

const
    gVer_mc_fonction_proprietes : T_Version = ( Component : 'Gestion des propriétés de zones' ;
                                               FileUnit : 'fonctions_proprietes' ;
             Owner : 'Matthieu Giroux' ;
                                      Comment : 'Fonctions de récupération de propriétés publiées.' ;
                                      BugsStory :  'Version 1.0.0.1 : Test de l''existence de la propiété pour fs_getComponentProperty' +
                                                            'Version 1.0.0.0 : Toutes les fonctions sont OK.';
                        			                 UnitType : 1 ;
                        			                 Major : 1 ; Minor : 0 ; Release : 0 ; Build : 0 );


type
  // On utilise les tableaux de variant pour plus tard :
  // gestion des clés à champs multiples
  tt_TableauVarOption = Array of Record
        var_Cle : Variant ;
        i_Option : Byte ;
       End ;

  tt_TableauVariant = Array of Variant ;
  tset_OctetOptions = set of Byte;

procedure p_SetComponentProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ; const a_ValueToSet : Variant ); overload;
procedure p_SetComponentProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ; const aTyp_PropertyType : TTypeKind ; const a_ValueToSet : Variant ); overload;
function fvar_getComponentProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ; const aTyp_PropertyType : TTypeKind ) : Variant ; overload;
function fvar_getComponentProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ) : Variant ; overload;
function flin_getComponentProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ) : LongInt ; overload;
function fs_getComponentProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ) : String ; overload;
function fb_getComponentBoolProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ) : Boolean ;
procedure p_SetComponentBoolProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ; const a_ValueToSet : Boolean );
procedure p_SetComponentObjectProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ; const a_ValueToSet : TObject );
function fobj_getComponentObjectProperty ( const aComp_Component : TComponent ; const as_PropertyName : String ) : TObject ;
function fcla_getComponentClassProperty ( const aComp_Component : TComponent ; const as_PropertyName : String ) : TClass ;
procedure p_SetComponentMethodProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ; const a_ValueToSet : TMethod );
procedure p_SetComponentMethodNameProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ; const aobj_ComponentMethod : TObject ; const a_MethodToSet : String );
function fmet_getComponentMethodProperty ( const aComp_Component : TComponent ; const as_PropertyName : String ) : TMethod ;
function fobj_getComponentStringsProperty ( const aComp_Component : TComponent ; const as_PropertyName : String ) : TStrings ;
{$IFDEF DELPHI_9_UP}
function fobj_getComponentWideStringsProperty ( const aComp_Component : TComponent ; const as_PropertyName : String ) : TWideStrings ;
{$ENDIF}
procedure p_SetFontColor ( const acom_component : TComponent ; const acol_couleur : TColor );

implementation

uses
{$IFDEF FPC}
{$ELSE}
{$ENDIF}
     unite_messages ;

function fb_getComponentBoolProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ) : Boolean ;
Begin
  Result := False ;
  if   assigned ( GetPropInfo ( aComp_ComponentToSet, as_PropertyName ))
  Then Result := getPropValue    ( aComp_ComponentToSet, as_PropertyName );
End ;
procedure p_SetComponentBoolProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ; const a_ValueToSet : Boolean );
Begin
 if   assigned ( GetPropInfo ( aComp_ComponentToSet, as_PropertyName ))
 then SetPropValue    ( aComp_ComponentToSet, as_PropertyName , a_ValueToSet);
End ;
// Affecte une propriété avec un certain type
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// a_ValueToSet         : Valeur à affecter
procedure p_SetComponentProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ; const a_ValueToSet : Variant ); overload;
Begin
 if   assigned ( GetPropInfo ( aComp_ComponentToSet, as_PropertyName ))
 then SetPropValue    ( aComp_ComponentToSet, as_PropertyName , a_ValueToSet);
End ;
// Affecte une propriété avec un certain type
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// aTyp_PropertyType    : Type de propriété
// a_ValueToSet         : Valeur à affecter
procedure p_SetComponentProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ; const aTyp_PropertyType : TTypeKind ; const a_ValueToSet : Variant ); overload;
Begin
 if   assigned ( GetPropInfo ( aComp_ComponentToSet, as_PropertyName ))
 and  PropIsType      ( aComp_ComponentToSet, as_PropertyName , aTyp_PropertyType)
 then SetPropValue    ( aComp_ComponentToSet, as_PropertyName , a_ValueToSet);
End ;
// récupère une propriété avec un certain type
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// aTyp_PropertyType    : Type de propriété
function fvar_getComponentProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ; const aTyp_PropertyType : TTypeKind ) : Variant ;
Begin
  Result := Null ;
  if   assigned ( GetPropInfo ( aComp_ComponentToSet, as_PropertyName ))
  and  PropIsType      ( aComp_ComponentToSet, as_PropertyName , aTyp_PropertyType)
  Then Result := getPropValue    ( aComp_ComponentToSet, as_PropertyName );
End ;
// récupère une propriété avec un certain type
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
function fvar_getComponentProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ) : Variant ;
Begin
  Result := Null ;
  if   assigned ( GetPropInfo ( aComp_ComponentToSet, as_PropertyName ))
  Then Result := getPropValue    ( aComp_ComponentToSet, as_PropertyName );
End ;
// récupère une propriété avec un certain type
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
function flin_getComponentProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ) : LongInt ;
Begin
  Result := -1 ;
  if   assigned ( GetPropInfo ( aComp_ComponentToSet, as_PropertyName ))
  Then Result := getPropValue    ( aComp_ComponentToSet, as_PropertyName, False );
End ;
// récupère une propriété avec un certain type
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
function fs_getComponentProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ) : String ;
Begin
  Result := '' ;
  if   IsPublishedProp ( aComp_ComponentToSet, as_PropertyName )
    Then Result := getStrProp    ( aComp_ComponentToSet, as_PropertyName );
End ;
// Affecte une propriété d'objet
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// a_ValueToSet         : Valeur à affecter
procedure p_SetComponentObjectProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ; const a_ValueToSet : TObject );
Begin
  if   IsPublishedProp ( aComp_ComponentToSet, as_PropertyName )
  and  PropIsType      ( aComp_ComponentToSet, as_PropertyName , tkClass)
  then SetObjectProp   ( aComp_ComponentToSet, as_PropertyName , a_ValueToSet);
End ;

// récupère une propriété d'objet
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// a_ValueToSet         : Valeur à affecter
function fcla_getComponentClassProperty ( const aComp_Component : TComponent ; const as_PropertyName : String ) : TClass ;
Begin
  Result := nil ;
  if   assigned ( GetPropInfo ( aComp_Component, as_PropertyName ))
  and  PropIsType      ( aComp_Component, as_PropertyName , tkClass)
  then Result := GetObjectPropClass   ( aComp_Component, as_PropertyName );
End ;


// récupère une propriété d'objet
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// a_ValueToSet         : Valeur à affecter
function fmet_getComponentMethodProperty ( const aComp_Component : TComponent ; const as_PropertyName : String ) : TMethod ;
Begin
  if   assigned ( GetPropInfo ( aComp_Component, as_PropertyName ))
  and  PropIsType      ( aComp_Component, as_PropertyName , tkMethod)
  then Result := GetMethodProp   ( aComp_Component, as_PropertyName )
  Else
    Begin
     Result.Data := aComp_Component;
     Result.Code := nil;
    End;
End ;


// Affecte une propriété d'objet
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// a_ValueToSet         : Valeur à affecter
procedure p_SetComponentMethodProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ; const a_ValueToSet : TMethod );
Begin
  if   IsPublishedProp ( aComp_ComponentToSet, as_PropertyName )
  and  PropIsType      ( aComp_ComponentToSet, as_PropertyName , tkMethod)
  then SetMethodProp   ( aComp_ComponentToSet, as_PropertyName , a_ValueToSet);
End ;


procedure p_SetComponentMethodNameProperty ( const aComp_ComponentToSet : TComponent ; const as_PropertyName : String ; const aobj_ComponentMethod : TObject ; const a_MethodToSet : String );
var lmet_MethodeDistribuee : TMethod ;
Begin
  lmet_MethodeDistribuee.Data := aobj_ComponentMethod;
  lmet_MethodeDistribuee .Code := aobj_ComponentMethod.MethodAddress(a_MethodToSet);
  p_SetComponentMethodProperty ( aComp_ComponentToSet, as_PropertyName, lmet_MethodeDistribuee );
End ;
// récupère une propriété d'objet
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// a_ValueToSet         : Valeur à affecter
function fobj_getComponentObjectProperty ( const aComp_Component : TComponent ; const as_PropertyName : String ) : TObject ;
Begin
  Result := nil ;
  if   assigned ( GetPropInfo ( aComp_Component, as_PropertyName ))
  and  PropIsType      ( aComp_Component, as_PropertyName , tkClass)
  then Result := GetObjectProp   ( aComp_Component, as_PropertyName );
End ;


// récupère une propriété d'objet
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// a_ValueToSet         : Valeur à affecter
function fobj_getComponentStringsProperty ( const aComp_Component : TComponent ; const as_PropertyName : String ) : TStrings ;
var fobj_Objet : TObject ;
Begin
  Result := nil ;
  fobj_Objet := fobj_getComponentObjectProperty ( aComp_Component, as_PropertyName );
  if fobj_Objet is TStrings Then
    Result := fobj_Objet as TStrings ;               
End ;

// récupère une propriété d'objet
// aComp_ComponentToSet : Composant cible
// as_PropertyName      : Propriété cible
// a_ValueToSet         : Valeur à affecter
{$IFDEF DELPHI_9_UP}

function fobj_getComponentWideStringsProperty ( const aComp_Component : TComponent ; const as_PropertyName : String ) : TWideStrings ;
var fobj_Objet : TObject ;
Begin
  Result := nil ;
  fobj_Objet := fobj_getComponentObjectProperty ( aComp_Component, as_PropertyName );
  if fobj_Objet is TWideStrings Then
    Result := fobj_Objet as TWideStrings ;
End ;
{$ENDIF}

// assigne une couleur à un tcomponent ayant la propriété font
// acom_component   : le composant
// acol_couleur     : la couleur à affecter
procedure p_SetFontColor ( const acom_component : TComponent ; const acol_couleur : TColor );
var
  lfon_font : TObject ;
begin
  lfon_font := fobj_getComponentObjectProperty ( acom_component, 'Font' );
  if assigned ( lfon_font )
  and ( lfon_font is TFont )
   Then
    ( lfon_font as TFont ).Color := acol_couleur ;
End ;


initialization
  p_ConcatVersion ( gVer_mc_fonction_proprietes );
finalization
end.

