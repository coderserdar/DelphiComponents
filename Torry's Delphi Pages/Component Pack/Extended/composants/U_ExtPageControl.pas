{*********************************************************************}
{                                                                     }
{                                                                     }
{             Matthieu Giroux                                         }
{             TExtPageControl :                                       }
{             Objet issu d'un TPageControl                            }
{             qui permet de personnalisé la couleur du titre          }
{             de l'onglet actif                                       }
{             10 Mars 2007                                            }
{                                                             }
{                                                                     }
{*********************************************************************}

unit U_ExtPageControl;

interface


{$I ..\Compilers.inc}

uses
{$IFDEF FPC}
        LCLIntf,
{$ELSE}
  Windows, JvExControls,
{$ENDIF}
        Messages, SysUtils, Classes, Controls,   Dialogs,
      Themes,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
      ComCtrls, Graphics;

{$IFDEF VERSIONS}
  const
    gVer_TExtPageControl : T_Version = ( Component : 'Composant TExtPageControl' ;
                                               FileUnit : 'U_ExtPageControl' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'PageControl avec gestion de couleurs et du retaillage.' ;
                                               BugsStory : '1.0.1.0 : Gestion du thème xp et du retaillage, composant Jedi.' + #13#10
                                                         + '1.0.0.0 : PageControl avec gestion de couleurs.'  ;
                                               UnitType : 3 ;
                                               Major : 1 ; Minor : 0 ; Release : 1 ; Build : 0 );
{$ENDIF}


type
  TExtPageControl = class(TPageControl,IJvExControl)
    { Déclarations privées }
  private

    gcol_Activefont    : TColor;
    gcol_ActiveColor        : TColor;
  protected
    { Déclarations protégées }
    procedure DrawTab(TabIndex: Integer; const Rect: TRect;
      Active: Boolean); override;
    procedure WMSize (var Message: TWMSize ); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;

   public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    procedure SetTabIndex(Value: Integer); override;

  published
    { Déclarations publiées }
    // Propriété qui conserve détermine la couleur du titre de l'onglet actif
    property ActiveFont : TColor read gcol_Activefont write gcol_Activefont default clMaroon ;
    property ActiveColor: TColor read gcol_ActiveColor write gcol_ActiveColor default clMoneyGreen ;
    //    Afficher la propriété ParentBackground
    property ParentBackground default False ;

  end;

procedure Register;

implementation

uses unite_messages;

const
    CST_TAILLE_RETROURNE_PAGE = 4 ;
    gt_RetournePage : array [1..CST_TAILLE_RETROURNE_PAGE] of TPoint = ((X:10;Y:0),(X:2;Y:5),(X:2;Y:0),(X:10;Y:0));

/////////////////////////////////////////////////////////////////////////////////
// Procédure : Register
// Description : Enregistrement du composant
/////////////////////////////////////////////////////////////////////////////////
procedure Register;
begin
  RegisterComponents(CST_PALETTE_COMPOSANTS, [TExtPageControl]);
end;

/////////////////////////////////////////////////////////////////////////////////
// Constructeur : Create
// Description : Initialisation du PageControl
// Paramètres : AOwner : La fiche propriétaire
/////////////////////////////////////////////////////////////////////////////////
constructor TExtPageControl.Create(AOwner: TComponent);
begin
  inherited;
  gcol_ActiveColor := clMoneyGreen ;
  gcol_Activefont  := clMaroon ;
  ControlStyle := ControlStyle - [csParentBackground] ;
  // A enlever : le composant possède aussi des propriétés en mode them xp
//  OwnerDraw := True;
end;

/////////////////////////////////////////////////////////////////////////////////
// Procédure : DrawTab
// Description : redessine une page TTabSheet de l'objet TPageControl
// Paramètres : TabIndex : Le numéro de l'onglet
//              Rect     : Le rectangle du dessin
//              Active   : Onglet actif ou pas
/////////////////////////////////////////////////////////////////////////////////
procedure TExtPageControl.DrawTab(TabIndex: Integer; const Rect: TRect;
  Active: Boolean);
var lt_polygone : array [1..CST_TAILLE_RETROURNE_PAGE] of TPoint ;
    li_i : Integer ;
begin
  inherited;
  with canvas do
  begin
    if TabIndex >= 0 then
    begin
      if Active then
      begin

//        if not ParentBackground Then
        Fillrect(Rect);
        Brush.Color := gcol_ActiveColor ;
        Pen  .Color := gcol_ActiveColor ;
        for li_i := low ( lt_polygone ) to high ( lt_polygone ) do
          Begin
            lt_polygone [ li_i ].X := Rect.Right - gt_RetournePage [ li_i ].X ;
            lt_polygone [ li_i ].Y := (( Rect.Bottom - Rect.Top ) * gt_RetournePage [ li_i ].Y ) div 7 ;
          End ;
        Polygon ( lt_polygone );
{        Brush.Color := clBlack ;
        Pen  .Color := clBlack ;
        for li_i := low ( lt_polygone ) to high ( lt_polygone ) do
          Begin
            lt_polygone [ li_i ].X := Rect.Right - ( gt_RetournePage [ li_i ].X div 2 );
            lt_polygone [ li_i ].Y := (( Rect.Bottom - Rect.Top * gt_RetournePage [ li_i ].Y ) div 8 );
          End ;
        Polygon ( lt_polygone );}
        font.color := gcol_Activefont;
        Brush.Color := Self.Brush.Color ;
//        Pen  .Color := Self.Pen  .Color ;
//        font.style := [fsBold];
        end
      else begin

        fillrect(Rect);
//        font.color := Self.Font.Color;
        font.style := [];

      end;
      TextOut(Rect.left + 4 , Rect.top + 4, Pages[TabIndex].caption);

    end;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////
// Procédure Message : SetTabIndex
// Description : Mois de retaillage au changement d'onglet
// Paramètre : Value : Les paramètres de retaillage
/////////////////////////////////////////////////////////////////////////////////
procedure TExtPageControl.SetTabIndex(Value: Integer);
begin
  DisableAlign ;
  inherited;
  EnableAlign ;
end;

/////////////////////////////////////////////////////////////////////////////////
// Procédure Message : WMSize
// Description : Mois de retaillage au retaillage
// Paramètre : Message : Les paramètres de retaillage
/////////////////////////////////////////////////////////////////////////////////
procedure TExtPageControl.WMSize(var Message: TWMSize);
begin
  DisableAlign ;
  inherited ;
  EnableAlign ;
end;
/////////////////////////////////////////////////////////////////////////////////
// Procédure Message : WMEraseBkgnd
// Description : récupération des propriétés de theme du TWinControl
// Paramètre : Message : Les paramètres d'effacement du fond
/////////////////////////////////////////////////////////////////////////////////
procedure TExtPageControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  with ThemeServices do
  if ThemesEnabled and Assigned(Parent) and (csParentBackground in ControlStyle) then
    begin
      { Get the parent to draw its background into the control's background. }
      DrawParentBackground(Handle, Message.DC, nil, False);
    end
    else
    begin
      { Only erase background if we're not doublebuffering or painting to memory. }
      if not DoubleBuffered or
         (TMessage(Message).wParam = TMessage(Message).lParam) then
        FillRect(Message.DC, ClientRect, Brush.Handle);
    end;

  Message.Result := 1;
end;

{$IFDEF VERSIONS}
initialization
  // Gestion de version
  p_ConcatVersion ( gVer_TExtPageControl );
{$ENDIF}
end.
