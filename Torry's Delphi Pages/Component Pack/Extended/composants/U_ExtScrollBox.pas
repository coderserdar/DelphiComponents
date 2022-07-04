{*********************************************************************}
{                                                                     }
{                                                                     }
{             Matthieu Giroux                                         }
{             TExtNumEdit  :                                           }
{             Composant edit de nombre                                }
{             TExtDBNumEdit :                                           }
{             Composant dbedit de nombre }
{             22 octobre 2006                                           }
{                                                                     }
{                                                                     }
{*********************************************************************}

unit U_ExtScrollBox;

interface

{$I ..\Compilers.inc}

uses Windows, Messages, SysUtils, Classes, Graphics, Controls,
     JvScrollBox,
{$IFDEF VERSIONS}
  // Gestion de version
     fonctions_version,
{$ENDIF}
     Forms ;

const
{$IFDEF VERSIONS}
    gVer_TMCScrollBox : T_Version = ( Component : 'Composant TExtSCrollBox' ;
                                               FileUnit : 'U_ExtScrollBox' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'JvScrollBox avec le HotTrack à False qui fonctionne.' ;
                                               BugsStory : '1.0.0.0 : Gestion en place.';
                                               UnitType : 3 ;
                                               Major : 1 ; Minor : 0 ; Release : 0 ; Build : 0 );
{$ENDIF}
    CST_MC_NEGATIVE = True ;
    CST_MC_POSITIVE = True ;
    CST_MC_BEFORECOMMA = 42 ;
    CST_MC_AFTERCOMMA  = 2 ;

type
  TExtSCrollBox   = class(TJvScrollBox)
  private
  protected
    procedure AutoScrollInView(AControl: TControl); override ;
  end;

procedure Register ;

implementation

uses unite_messages;

{ TExtSCrollBox }

procedure TExtSCrollBox.AutoScrollInView(AControl: TControl);
begin
  if HotTrack Then
    inherited;

end;

procedure Register ;
Begin
  RegisterComponents ( CST_PALETTE_COMPOSANTS , [TExtSCrollBox] );
End ;

{$IFDEF VERSIONS}
initialization
  // Gestion de version
  p_ConcatVersion ( gVer_TMCScrollBox );
{$ENDIF}
end.
