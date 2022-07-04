unit fonctions_components;

interface

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

{$I ..\Compilers.inc}
{$I ..\extends.inc}

uses SysUtils,
  {$IFDEF VERSIONS}
  fonctions_version,
  {$ENDIF}
  Controls, StdCtrls, 
  Classes ;

  {$IFDEF VERSIONS}
const
  gVer_fonctions_components : T_Version = ( Component : 'Fonctions de personnalisation des composants' ;
                                         FileUnit : 'fonctions_components' ;
      			                 Owner : 'Matthieu Giroux' ;
      			                 Comment : 'Fonctions de gestion des composants visuels.' ;
      			                 BugsStory : 'Version 1.0.0.0 : Ajout de fonctions d''automatisation.';
      			                 UnitType : 1 ;
      			                 Major : 1 ; Minor : 0 ; Release : 0 ; Build : 0 );

  {$ENDIF}

procedure p_ComponentSelectAll ( const aobj_Component : TObject );

implementation

uses Variants,  Math, fonctions_erreurs, fonctions_string, unite_messages,
     fonctions_proprietes ;

procedure p_ComponentSelectAll ( const aobj_Component : TObject );
Begin
  if aobj_Component is TCustomEdit Then
    (aobj_Component as TCustomEdit ).SelectAll;
End;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_fonctions_components );
{$ENDIF}
end.
