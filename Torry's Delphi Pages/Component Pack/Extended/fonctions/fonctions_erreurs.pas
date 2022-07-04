unit fonctions_erreurs;

interface

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

{$I ..\Compilers.inc}
{$I ..\extends.inc}

uses SysUtils,
{$IFDEF EADO}
     ADODB,
{$ENDIF}
{$IFDEF FPC}
        LCLIntf,
{$ELSE}
  Windows,
{$ENDIF}
     Dialogs, DB, fonctions_version, Classes ;

type
  TMessageEvent = procedure( Sender: TObject; Dataset : TDataset; NombreChamps : Integer ; ChampsMessage : String ) of object;
const
  gVer_fonctions_erreurs : T_Version = ( Component : 'Gestion des erreurs' ; FileUnit : 'fonctions_erreurs' ;
                        			                 Owner : 'Matthieu Giroux' ;
                        			                 Comment : 'Fonctions de gestion des erreurs appelées dans un try except ou par la fenêtre principale.' ;
                        			                 BugsStory :  'Version 1.1.3.1 : Mise à jour sur la déconnexion OLE.' + #13#10 +
                        			                	        		'Version 1.1.3.0 : Fonction f_GereException avec 2 paramètres.' + #13#10 +
                        			                	        		'Version 1.1.2.0 : Gestion sans Dataset.' + #13#10 +
                        			                	        		'Version 1.1.1.1 : Moins de code compilé.' + #13#10 +
                        			                	        		'Version 1.1.1.0 : Gestion des erreurs, second dataset de mise à jour.' + #13#10 +
                        			                	        		'Version 1.1.0.0 : Gestion des erreurs Form Dico.' + #13#10 +
                        			                	        		'Version 1.0.2.0 : Gestion des erreurs de rafraîchissement.' + #13#10 +
                        			                	        		'Version 1.0.1.0 : Erreurs de MAJ dans la gestion des erreurs.' + #13#10 +
                        			                	        		'Version 1.0.0.0 : La gestion est en place, ne gérant pas tout.';
                        			                 UnitType : 1 ;
                        			                 Major : 1 ; Minor : 1 ; Release : 3 ; Build : 1 );
function f_GereExceptionEvent ( const aexc_exception : Exception  ; const adat_Dataset : TDataset ; const ae_Evenement : TDataSetErrorEvent ; const ab_PasseErreur : Boolean ) : TClass;

function f_GereException ( const aexc_exception : Exception  ; const adat_Dataset : TDataset ) : TClass; overload ;

// gestion d'une exception delphi
// aexc_exception : l'exception
// aaco_connecteur: la connexion ado
// ab_PasseErreur : Option passer l'erreur
function f_GereException ( const aexc_exception : Exception  ; const adat_Dataset : TDataset ; const aaco_connecteur : TCustomConnection ; const ab_PasseErreur : Boolean ) : TClass; overload ;

{$IFDEF EADO}
// gestion d'une exception ado
// aaco_connecteur: la connexion ado
function f_GereExceptionADO ( const aaco_connecteur : TADOConnection ; const aexc_Exception : Exception ; const adat_Dataset : TCustomADODataset ; var amdt_Icone : TMsgDlgType ) : String;
{$ENDIF}

var gstl_CleEnDoubleErreurs : TStringlist = nil ;
    ge_EvenementCleUtilise : TMessageEvent = nil ;
    gfor_ValidateForm      : Tobject = nil ;
    gb_ShowError32         : Boolean = True ;
{$IFDEF EADO}
    gdat_DatasetRefreshOnError : TCustomADODataset = nil ;
{$ENDIF}
//const GS_MC_CONTACT = #13#10 + #13#10 + 'Contacter votre administrateur pour une vérification ou un retour de ce message.' ;

implementation

uses unite_messages, Forms,
{$IFDEF DELPHI}
     OleDb,
{$ENDIF}
{$IFDEF EADO}
     ADOInt, ComObj,
{$ENDIF}
     fonctions_string ;

// gestion d'une exception delphi
// aexc_exception : l'exception
// adat_Dataset   : la dataset de la connexion ado
// ab_PasseErreur : Option passer l'erreur
function f_GereException ( const aexc_exception : Exception  ; const adat_Dataset : TDataset ) : TClass; overload ;
Begin
  if assigned ( adat_Dataset )
{$IFDEF EADO}
  and ( adat_Dataset is TCustomADODataset )  Then
    Begin
      // Exception avec dataset ado
      Result := f_GereException ( aexc_exception, adat_Dataset,( adat_Dataset as TCustomADODataset ).Connection, False );
    End
  Else
{$ELSE}
         Then
{$ENDIF}
    Begin
      // Exception sans dataset
      Result := f_GereException ( aexc_exception, nil, nil, False );
    End
End ;

// gestion d'une exception delphi
// aexc_exception : l'exception
// adat_Dataset   : la dataset de la connexion ado
// ab_PasseErreur : Option passer l'erreur
function f_GereExceptionEvent ( const aexc_exception : Exception  ; const adat_Dataset : TDataset ; const ae_Evenement : TDataSetErrorEvent ; const ab_PasseErreur : Boolean ) : TClass;
Begin
  if assigned ( adat_Dataset )
{$IFDEF EADO}
  and ( adat_Dataset is TCustomADODataset )
{$ENDIF}
  and not assigned ( ae_Evenement )  Then
{$IFDEF EADO}
    Begin
      // Exception avec dataset ado
      Result := f_GereException ( aexc_exception, adat_Dataset,( adat_Dataset as TCustomADODataset ).Connection, ab_PasseErreur )
    End
  Else
{$ENDIF}
    Begin
      // Exception sans dataset
      Result := f_GereException ( aexc_exception, nil, nil, ab_PasseErreur );
    End
End ;

function f_GereException ( const aexc_exception : Exception ; const adat_Dataset : TDataset ; const aaco_connecteur : TCustomConnection ; const ab_PasseErreur : Boolean ) : TClass;
var ls_Message : String ;
    li_i : Integer ;
    Icone : TMSgDlgType ;
Begin
  Result := aexc_exception.ClassType ;
  Icone := mtError ;
  // L'annulation est une erreur à ne pas traiter
  if ab_PasseErreur
  or ( aexc_exception is EAbort ) Then
   Exit ;
//  ShowMessage ( aexc_exception.ClassName );
   // Texte EConvertError pas compréhensible et message
  if ( aexc_exception is EConvertError )
  or ( pos ( 'valeur en virgule flottante correcte pour', aexc_exception.Message ) > 0 )  Then
    Begin
      ls_Message := aexc_exception.Message ;
      // texte à remplacer
      li_i := pos ( 'e valeur en virgule flottante correcte', ls_Message );
      if li_i > 0 Then
        // Remplacement
        ls_Message := copy ( ls_Message, 1, li_i - 1 ) + ' nombre correct' +  copy ( ls_Message, li_i + 38, length ( ls_Message ) - li_i - 37 );
      // Message Warning
      MessageDlg ( 'Problème de conversion :' + #13#10 + #13#10
                 +  ls_Message
                 , mtWarning, [mbOK], aexc_exception.HelpContext);
      Exit ;
    End ;
  if (( aexc_exception is EDatabaseError ) {$IFDEF EADO} or ( aexc_exception is EOleException ) {$ENDIF})
  and assigned ( aaco_connecteur ) Then
    Begin
//      Showmessage ( aaco_connecteur.ClassName );
      {$IFDEF EADO}
      if  ( aaco_connecteur is TADOConnection ) and ( ( aaco_connecteur as TADOConnection ).Errors.Count > 0 ) Then
//      and ( aexc_exception.HelpContext = aaco_connecteur.Errors [ aaco_connecteur.Errors.Count - 1 ].HelpContext ) Then
        Begin
          if adat_Dataset is TCustomADODataSet Then
            Begin
              ls_Message := f_GereExceptionADO ( aaco_connecteur as TADOConnection, aexc_exception, adat_Dataset as TCustomADODataSet, Icone )
            End
          Else
            ls_Message := f_GereExceptionADO (aaco_connecteur as TADOConnection , aexc_exception, nil, Icone );
          if ls_Message <> '' Then
            MessageDlg (  ls_Message, Icone, [mbOK], aexc_exception.HelpContext);
          gb_ShowError32 := True ;
          Exit ;
        End ;
      {$ENDIF}
    End ;
  MessageDlg ( 'Erreur  : ' + aexc_exception.ClassName + #13#10
             +  aexc_exception.Message
             {+ GS_MC_CONTACT }, Icone, [mbOK], aexc_exception.HelpContext);
End ;

{$IFDEF EADO}

function f_GereExceptionADO ( const aaco_connecteur : TADOConnection ; const aexc_Exception : Exception ; const adat_Dataset : TCustomADODataset ; var amdt_Icone : TMsgDlgType ) : String;
var li_i, li_k : Integer ;
    li_Compteur  : integer;
    lbkm_Enregistrement :  TBookmarkStr ;
    lt_Arg       : Array [ 0..0] of string ;
    ls_Message   : String ;
Begin
  Result := 'Erreur dans les données : ' + #13#10+ #13#10 ;
  aaco_connecteur.Errors.Refresh ;
  for li_i := 0 to aaco_connecteur.Errors.Count - 1 do
    Begin
//      ShowMessage ( IntToStr ( aaco_connecteur.Errors [ li_i ].Number ) + ' ' + IntToStr ( aaco_connecteur.Errors [ li_i ].HelpContext ) + ' ' +  IntToStr ( aaco_connecteur.Errors [ li_i ].NativeError ) );
      if ( aaco_connecteur.Errors [ li_i ].NativeError = 32 )
      // Ligne non trouvée pour la mise à jour
      and assigned ( adat_Dataset )
      and adat_Dataset.Active Then
        Begin
          Result := '' ;
          if gb_ShowError32 Then
            Begin
              MessageDlg (  GS_METTRE_A_JOUR_FICHE, mtWarning, [mbOK], 0);
            End ;

          if assigned ( gdat_DatasetRefreshOnError )
          and gdat_DatasetRefreshOnError.Active Then
            Begin

              lbkm_Enregistrement := gdat_DatasetRefreshOnError.Bookmark ;
              if gdat_DatasetRefreshOnError.State in [dsInsert,dsEdit ] Then
                gdat_DatasetRefreshOnError.Cancel ;
              gdat_DatasetRefreshOnError.Requery ;
              try
                gdat_DatasetRefreshOnError.Bookmark := lbkm_Enregistrement ;
              Except
              End ;
            End ;
          lbkm_Enregistrement := adat_Dataset.Bookmark ;
          if adat_Dataset.State in [dsInsert,dsEdit ] Then
            adat_Dataset.Cancel ;
          adat_Dataset.Requery ;
          try
            adat_Dataset.Bookmark := lbkm_Enregistrement ;
          Except
          End ;
          Exit ;
        End ;
      if ( aaco_connecteur.Errors [ li_i ].NativeError = 2627 )
      // Doublon
      and assigned ( adat_Dataset )
      and assigned ( gstl_CleEnDoubleErreurs )
      and adat_Dataset.Active Then
{        If adat_Dataset.State = dsInsert Then
          Begin
            ls_Message := GS_MC_ERREUR_MISE_A_JOUR ;
            amdt_Icone := mtWarning ;
            Exit ;
          End
        Else}
          Begin
            li_Compteur := 0 ;
            ls_Message := '' ;
            for li_k := 0 to gstl_CleEnDoubleErreurs.Count - 1 do
              Begin
                if assigned ( adat_Dataset.FindField ( gstl_CleEnDoubleErreurs [ li_k ] ))
                 Then
                  begin
                    inc ( li_Compteur );
                    if li_Compteur > 1
                     Then
                      ls_Message := ls_Message + ', ' + Trim ( adat_Dataset.FindField ( gstl_CleEnDoubleErreurs [ li_k ] ).AsString )
                     Else
                      ls_Message :=                     Trim ( adat_Dataset.FindField ( gstl_CleEnDoubleErreurs [ li_k ] ).AsString );
                  end;
              End ;
           if li_Compteur > 0 Then
             Begin
              if assigned ( ge_EvenementCleUtilise ) Then
              ge_EvenementCleUtilise ( gfor_ValidateForm, adat_Dataset, li_Compteur, ls_Message )
               Else
                Begin

                    lt_Arg [ 0 ] := ls_Message ;
                    if li_Compteur = 1
                      Then Result := fs_RemplaceMsg ( GS_VALEUR_UTILISEE  , lt_Arg )
                      Else Result := fs_RemplaceMsg ( GS_VALEURS_UTILISEES, lt_Arg );
                   amdt_Icone := mtWarning ;
                End ;
              Exit ;
             End ;

          End ;
      if aaco_connecteur.Errors [ li_i ].NativeError = 8115 Then
        Begin
          Result := GS_ERREUR_NOMBRE_GRAND + #13#10 ;
        End ;
      if aaco_connecteur.Errors [ li_i ].NativeError = 11 Then
      // Déconnexion réseau
        Begin
          Result := GS_ERREUR_RESEAU + #13#10 ;
          aaco_connecteur.Close ;
        End ;
      if aaco_connecteur.Errors [ li_i ].NativeError = 547 Then
       // Impossible de supprimer l'enregistrement utilisé
        Begin
          amdt_Icone := mtWarning ;
          Result := GS_ERREUR_MODIFICATION_MAJ + #13#10 + #13#10 ;
          Exit ;
        End ;
      if aaco_connecteur.Errors [ li_i ].NativeError = 3641 Then
        // arrêt de l'exécution
        Begin
          Result := Result + 'Arrêt...' + #13#10 ;
        End ;

        /// ERREURS OLE
      if  ( aexc_Exception is EOLEException ) Then
        Begin
          // Erreur sur le lien aux données
          if  ( aaco_connecteur.Errors [ li_i ].SQLState = '08S01' )
          // Il n'y a pas de numéro d'erreur native
          and ( aaco_connecteur.Errors [ li_i ].NativeError = 0 ) Then
          // Déconnexion réseau
            Begin
              Result := GS_ERREUR_CONNEXION + #13#10 ;
              aaco_connecteur.Close ;
            End ;
        End ;
    End ;
  gdat_DatasetRefreshOnError := nil ;
  Result := Result + #13#10 + GS_DETAILS_TECHNIQUES + #13#10 + #13#10 ;
  for li_i := 0 to aaco_connecteur.Errors.Count - 1 do
    Result := Result + 'Erreur ' + IntToStr ( aaco_connecteur.Errors [ li_i ].NativeError )
             + ' ( Etat : ' + aaco_connecteur.Errors [ li_i ].SQLState
             + ' Origine : ' + aaco_connecteur.Errors [ li_i ].Source + ')' + #13#10
                         + aaco_connecteur.Errors [ li_i ].Description + #13#10 + #13#10 ;
End ;
{$ENDIF}

initialization
  p_ConcatVersion ( gVer_fonctions_erreurs );
finalization
  gstl_CleEnDoubleErreurs.Free ;
  gstl_CleEnDoubleErreurs := nil ;
end.
