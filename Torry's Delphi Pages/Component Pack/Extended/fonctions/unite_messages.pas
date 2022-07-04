unit unite_messages;

interface

{$I ..\extends.inc}


{$IFDEF VERSIONS}
uses fonctions_version ;

const
  gVer_unite_messages : T_Version = ( Component : 'Constantes messages' ; FileUnit : 'unite_messages' ;
                        			                 Owner : 'Matthieu Giroux' ;
                        			                 Comment : 'Constantes et variables messages.' ;
                        			                 BugsStory : 'Version 1.0.4.0 : Message d''erreur de sauvegarde ini.' + #13#10
                        			                	         + 'Version 1.0.3.3 : Message GS_MC_ERREUR_CONNEXION.' + #13#10
                        			                	         + 'Version 1.0.3.2 : Modifs GS_MC_VALEUR_UTILISEE et GS_MC_VALEURS_UTILISEES, ajout de GS_MC_DETAILS_TECHNIQUES.' + #13#10
                        			                	         + 'Version 1.0.3.1 : Constante message Form Dico.' + #13#10
                        			                	         + 'Version 1.0.3.0 : Constantes INI.' + #13#10
                        			                	         + 'Version 1.0.2.0 : Plus de messages dans l''unité.' + #13#10
                        			                	         + 'Version 1.0.1.0 : Plus de messages dans l''unité.' + #13#10
                        			                	         + 'Version 1.0.0.0 : Gestion des messages des fenêtres.';
                        			                 UnitType : 1 ;
                        			                 Major : 1 ; Minor : 0 ; Release : 4 ; Build : 0 );

{$ENDIF}

// COmposants
const
  CST_RESSOURCENAV = 'EXTNAV' ;
  CST_RESSOURCENAVMOVE = 'MOVE' ;
  CST_RESSOURCENAVBOOKMARK = 'BOOKMARK' ;
  CST_HC_SUPPRIMER        = 0 ;
  CST_PALETTE_COMPOSANTS_DB = 'ExtDB' ;
  CST_PALETTE_COMPOSANTS    = 'ExtCtrls' ;
  CST_Avant_Fichier = 'MG_';
  CST_ASYNCHRONE_TIMEOUT_DEFAUT = 30 ;
  CST_CONNECTION_TIMEOUT_DEFAUT : Integer = 15 ;
  CST_ASYNCHRONE_NB_ENREGISTREMENTS : Integer = 300 ;



resourcestring
  // Paquet Fonctions
  GS_ECRITURE_IMPOSSIBLE = 'Impossible d''écrire sur le fichier @ARG avec l''attribut de fichier @ARG.' ;
  GS_DETAILS_TECHNIQUES = 'Détails techniques : ' ;

 // Composants
  GS_SUPPRIMER_QUESTION = 'Confirmez-vous l''effacement de l''enregistrement ?' ;
  GS_CHARGEMENT_IMPOSSIBLE_FIELD_IMAGE  = 'Il est impossible de charger l''enregistrement de l''image.' ;
  GS_CHARGEMENT_IMPOSSIBLE_STREAM_IMAGE = 'Il est impossible de charger le flux image.' ;
  GS_CHARGEMENT_IMPOSSIBLE_STREAM_FIELD = 'Il est impossible de charger le flux image dans le champ.' ;
  GS_CHARGEMENT_IMPOSSIBLE_FILE_IMAGE   = 'Il est impossible de charger le fichier image.' ;
  GS_ERREUR_OUVERTURE = 'Erreur à l''ouverture @ARG.' ;
  GS_FORM_ABANDON_OUVERTURE = 'Abandon de l''ouverture de la fiche @ARG...' ;
  GS_FirstRecord = 'Premier enregistrement';
  Gs_GROUPVIEW_Basket = 'Retour origine';
  Gs_GROUPVIEW_Record = 'Enregistrer';
  Gs_GROUPVIEW_Abort  = 'Abandonner';

  GS_PriorRecord = 'Enregistrement précédent';
  GS_NextRecord = 'Enregistrement suivant';
  GS_LastRecord = 'Dernier enregistrement';
  GS_InsertRecord = 'Insérer enregistrement';
  GS_DeleteRecord = 'Supprimer l''enregistrement';
  GS_EditRecord = 'Modifier l''enregistrement';
  GS_PostEdit = 'Valider modifications';
  GS_CancelEdit = 'Annuler les modifications';
  GS_ConfirmCaption = 'Confirmation';
  GS_RefreshRecord = 'Rafraîchir les données';
  GS_SearchRecord = 'Rechercher' ;
  GS_MoveNextRecord = 'Déplacer l''enregistrement au suivant' ;
  GS_MovePreviousRecord = 'Déplacer l''enregistrement au précédent' ;
  GS_SetBookmarkRecord = 'Marquer L''enregistrement' ;
  GS_GotoBookmarkRecord = 'Aller à l''enregistrement Marqué' ;

  // SGBD
 
  GS_CONNECTION_TIMEOUT = 'Connection TimeOut' ;
 {$IFDEF FPC}
  SCloseButton = '&Fermer' ;
 {$ENDIF}
 {$IFDEF EADO}
  GS_Set_KEYSET = 'Set Keyset' ;
  GS_MODE_ASYNCHRONE = 'Mode Asynchrone' ;
  GS_ACCES_DIRECT_SERVEUR = 'Accès directs Serveur' ;
  GS_MODE_CONNEXION_ASYNCHRONE = 'Connection Asynchrone' ;
  GS_MODE_ASYNCHRONE_NB_ENREGISTREMENTS = 'Mode Asynchrone Enregistrements' ;
  GS_MODE_ASYNCHRONE_TIMEOUT = 'Mode Asynchrone TimeOut' ;
{$ENDIF}

  // Erreurs
  GS_ERREUR_NOMBRE_GRAND = 'Problème à la validation du nombre :' + #13#10
                   + 'Un nombre saisi est trop grand.' + #13#10
                   + 'Modifier la saisie ou annuler.' ;
  GS_METTRE_A_JOUR_FICHE = 'L''enregistrement a été effacé ou modifié par un autre utilisateur.' + #13 + #13
                        			+ 'La fiche va être mise à jour.' ;
  GS_VALEUR_UTILISEE   = 'La valeur @ARG est déjà utilisée.' + #13 + #13
                        		+ 'Saisir une valeur différente, annuler ou réeffectuer la validation si une valeur n''est pas modifiable.' ;
  GS_VALEURS_UTILISEES = 'Les valeurs @ARG sont déjà utilisées.' + #13 + #13
                        		+ 'Saisir des valeurs différentes, annuler ou réeffectuer la validation si une valeur n''est pas modifiable.' ;
  GS_ERREUR_RESEAU = 'Erreur réseau.' + #13#10
                        + 'Vérifier la connexion réseau.' ;
  GS_ERREUR_MODIFICATION_MAJ = 'Impossible de supprimer cet enregistrement. ' + #13
               + 'Il est utilisé dans une autre fonction.';
  GS_ERREUR_CONNEXION = 'Un problème est survenu pour la connexion aux données.' + #13#10
                        	 + 'Réessayez d''ouvrir la fiche.' ;
                        //GS_CHANGEMENTS_SAUVER = 'Des changements ont été effectués.' + #13#10 +' Le trie nécessite alors une sauvegarde.'  + #13#10 + 'Voulez-vous enregistrer les changements effectués ?' ;

 // Messages pour les images
  GS_IMAGE_MULTIPLE_5 = 'L''image doit avoir une largeur multiple de ' ;
  GS_IMAGE_DOIT_ETRE = 'La taille de l''image doit être de ' ;
  GS_IMAGE_HAUTEUR = 'L''image doit avoir une hauteur de ' ;
  GS_IMAGE_TROP_PETITE = 'L''image est trop petite.' ;
  GS_IMAGE_TROP_GRANDE = 'L''image est trop grande.' ;

 // Messages pour le navigateur
  GS_INSERER_ENREGISTREMENT = 'Insérer un enregistrement' ;
  GS_VALIDER_MODIFICATIONS  = 'Valider les modifications' ;

  GS_NAVIGATEUR_VERS_LE_BAS  = 'Déplacer la ligne vers le bas' ;
  GS_NAVIGATEUR_VERS_LE_HAUT = 'Déplacer la ligne vers le haut' ;

  GS_PB_CONNEXION = 'La connexion a échouée' + #13 + #13
                      + 'Veuillez contacter votre administrateur';
  GS_DECONNECTER_ANNULE = 'Annulation de la déconnexion';
var
  gb_MainFormIniOneUserOnServer : Boolean = False ;
{$IFDEF EADO}
  GB_ASYNCHRONE_PAR_DEFAUT : Boolean = False ;
{$ENDIF}
implementation

initialization
{$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_unite_messages );
{$ENDIF}
end.
