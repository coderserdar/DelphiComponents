unit fonctions_numedit;

interface

{$I ..\Compilers.inc}
{$I ..\extends.inc}


uses
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
{$IFDEF FPC}
     MaskEdit,
{$ELSE}
     ADODB, Mask,
{$ENDIF}
     SysUtils, DB ;

procedure p_editGridKeyPress ( const aobj_Sender : Tobject ; var ach_Key : Char ; const aby_NbApVirgule , aby_NbAvVirgule : Byte ; const ab_Negatif  : Boolean ; const ai_SelStart : Integer ; const as_Texte , as_SelTexte: String ; const afie_Champ  : TField );
procedure p_editKeyUp ( const aed_Sender : TCustomMaskEdit ;  const afie_Field : TField ; var ach_Key : Word ; const aby_NbApVirgule , aby_NbAvVirgule : Byte ; const ab_Negatif  : Boolean ; const as_Texte     : String );

{$IFDEF VERSIONS}
const
  gVer_fonctions_numedit : T_Version = ( Component : 'Gestion partagée des nombres' ; FileUnit : 'fonctions_numedit' ;
                        			                 Owner : 'Matthieu Giroux' ;
                        			                 Comment : 'Gestion partagée du formatage des nombres' ;
                        			                 BugsStory : 'Version 1.1.0.0 : Passage en Jedi 3.'
                        			                	         + 'Version 1.0.0.0 : Fonctions partagées.';
                        			                 UnitType : 1 ;
                        			                 Major : 1 ; Minor : 1 ; Release : 0 ; Build : 0 );

{$ENDIF}
implementation

uses fonctions_string,
     DBGrids;
// A ne pas utiliser
// Evènement sur touche enlevée d'un dbedit et d'une grille
// Paramètres : pour créer l'évènement
procedure p_editKeyUp ( const aed_Sender : TCustomMaskEdit ; const afie_Field : TField ; var ach_Key : Word ; const aby_NbApVirgule , aby_NbAvVirgule : Byte ; const ab_Negatif  : Boolean ; const as_Texte     : String );
var lli_Position : Longint ;
    lb_Reformate : Boolean ;
Begin
  // Zone d'éditon :
  // Rien alors on met un 0
  if assigned ( afie_Field )
  and not ( afie_Field.DataSet.State in [ dsInsert, dsEdit ]) Then
    Exit ;
  lb_Reformate := False ;
  lli_Position := aed_Sender.SelStart ;
  if  ( AnsiPos ( DecimalSeparator, as_Texte ) > 0 )
  and ( AnsiPos ( DecimalSeparator, as_Texte ) < length ( as_Texte ) - aby_NbApVirgule ) then
    Begin
      aed_Sender.Text := copy ( aed_Sender.Text, 1, AnsiPos ( DecimalSeparator, as_Texte ) + aby_NbApVirgule );
      lb_Reformate := True ;
    End ;
  // si il y a un séparateur de milliers et le texte est reformaté automatiquement
  // La saisie est alors différente
  if  assigned ( afie_Field )
  and ( AnsiPos ( ThousandSeparator, aed_Sender.Text ) > 0 )
  or lb_Reformate  Then
    Begin
      afie_Field.Value := ( fs_RemplaceEspace ( aed_Sender.Text, '' ));
      if lli_Position >= length ( aed_Sender.Text ) Then
        aed_Sender.SelStart := length (aed_Sender.Text ) - 1
      Else
        aed_Sender.SelStart := lli_Position ;
    End ;
End ;
// A ne utiliser si on surcharge l'évènement onkeydown d'une grille
// action sur touche enlevée d'une grille
// Dévalide la suppression et l'insertion
// Paramètres : pour créer l'évènement
procedure p_editGridKeyPress ( const aobj_Sender : Tobject ; var ach_Key : Char ; const aby_NbApVirgule , aby_NbAvVirgule : Byte ; const ab_Negatif  : Boolean; const ai_SelStart : Integer ; const as_Texte , as_SelTexte : String ; const afie_Champ  : TField );
var lby_Signe   : Byte ;
Begin
  if (( AnsiPos ( '+'             , as_Texte ) > 0 ) or ( AnsiPos ( '-'             , as_Texte ) > 0 )) Then
    lby_Signe := 1
  Else
    lby_Signe := 0 ;

  // gestion du + et du -
  if  ( ach_Key in [ '-','+']) Then
    Begin
      if (( ai_SelStart > 0 ) or ( AnsiPos ( '-', as_Texte ) > 0 ) or ( AnsiPos ( '+', as_Texte ) > 0 )) and not (( AnsiPos ( '-', as_SelTexte ) > 0 ) or ( AnsiPos ( '+', as_SelTexte ) > 0 )) then
        Begin
          ach_key := #0;
        End ;
      if ( ach_Key = '-' )
      and not ab_Negatif Then
        Begin
          ach_key := #0;
        End ;
    End ;
  // gestion de la bonne virgule à afficher
  if ( ach_Key in ['.',',']) Then
    ach_Key := DecimalSeparator ;
  // Touches valides : il n'y en a pas d'autres
  if not ( ach_Key in [#8, #13, '0'..'9','-','+',DecimalSeparator]) then
    Begin
      ach_key := #0;
    End
  Else
    // On veut taper un nombre
    if not ( ach_Key in [#8, #13,DecimalSeparator,'-','+'] )
    and ( length ( as_SelTexte ) <= 0 ) Then
      // Gestion du nombre de chiffres après la virgule
      if  ( AnsiPos ( DecimalSeparator, as_Texte ) > 0 )
      and ( length ( as_Texte ) >= AnsiPos ( DecimalSeparator, as_Texte ) + aby_NbApVirgule)
      and ( ai_SelStart = length ( as_Texte ))
      and ( ai_SelStart > AnsiPos ( DecimalSeparator, as_Texte ))
      and ( AnsiPos ( DecimalSeparator, as_SelTexte ) <= 0 ) Then
        Begin
          ach_key := #0;
        End
      Else
      // Gestion du nombre de chiffres avant la virgule
        if  ((     ( AnsiPos ( DecimalSeparator, as_Texte ) > 0 )
              and ( AnsiPos ( DecimalSeparator, as_Texte ) - lby_Signe > aby_NbAvVirgule )
              and ( ai_SelStart < AnsiPos ( DecimalSeparator, as_Texte )))
        or  (     ( AnsiPos ( DecimalSeparator, as_Texte ) <= 0 )
              and ( length ( as_Texte ) - lby_Signe >= aby_NbAvVirgule )))
        and ( AnsiPos ( DecimalSeparator, as_SelTexte ) <= 0 ) Then
          Begin
            ach_key := #0;
          End ;
  // Gestion d'un champ sans virgule
  if  ( ach_Key = DecimalSeparator)
  and ((( AnsiPos(DecimalSeparator, as_Texte ) > 0 ) and ( AnsiPos ( DecimalSeparator, as_SelTexte ) <= 0 )) or ( afie_Champ is TIntegerField )) then
    ach_Key := #0 ;
End ;

initialization
{$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_fonctions_numedit );
{$ENDIF}
finalization
end.
