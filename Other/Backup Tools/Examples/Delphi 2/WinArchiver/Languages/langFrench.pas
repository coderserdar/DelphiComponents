unit langFrench;

interface

procedure SetLanguage;

implementation
uses unTranslation;

procedure SetLanguage;
begin
  // Misc strings
  AddStr(   1, 'Choisissez "Nouveau" pour créer ou "Ouvrir" pour ouvrir une archive' );
  AddStr(   2, 'OK' );
  AddStr(   3, 'Annuler' );
  AddStr(   4, 'Aide' );
  // unit fmAboutBox
  AddStr( 500, 'A propos' );
  AddStr( 501, 'par Morgan Martinet (C)1998' );
  AddStr( 502, 'Ces Composants sont Freeware.' );
  AddStr( 503, 'Copyright (C) 1998 par NOMSSI NZALI Jacques H. C.' );
  AddStr( 504, 'librairie pasZLib :' );
  AddStr( 505, 'mmm@imaginet.fr ou mmm@mcom.fr' );
  AddStr( 506, 'Implémentation BlowFish fournie par Greg Carter, CRYPTOCard' );
  AddStr( 507, 'Code SFX par Oliver Buschjost' );
  AddStr( 508, 'Site Web :' );
  AddStr( 509, 'OK' );
  // unit fmTiming
  AddStr( 600, 'Temps écoulé :' );
  AddStr( 601, 'Temps restant :' );
  // unit fmMain
  AddStr( 700, 'Nouveau...' );
  AddStr( 701, 'Ouvrir...' );
  AddStr( 702, '&Ajouter...' );
  AddStr( 703, '&Extraire...' );
  AddStr( 704, '&Supprimer...' );
  AddStr( 705, '&Annuler' );
  AddStr( 706, 'Nouvelle Archive' );
  AddStr( 707, 'Ouvrir une Archive' );
  AddStr( 708, 'Ajouter des fichiers...' );
  AddStr( 709, 'Extraire des fichiers...' );
  AddStr( 710, 'Supprimer des fichiers...' );
  AddStr( 711, 'Fichiers Archive (*.mmm)|*.mmm|Archives SFX (*.exe)|*.exe|Tous les fichiers (*.*)|*.*' );
  AddStr( 712, 'Fichiers Archive (*.mmm)|*.mmm|Tous les fichiers (*.*)|*.*' );
  AddStr( 713, 'Ouvrir une Archive existante' );
  AddStr( 714, 'Créer une nouvelle Archive' );
  AddStr( 715, 'Ouvrir l''un des segments d''une Archive' );
  AddStr( 718, '%d fichier(s), %s' );
  AddStr( 720, 'Le fichier "%s" existe déjà' );
  AddStr( 721, 'Voulez-vous vider cette archive ?' );
  AddStr( 722, 'Voulez-vous supprimer cette archive ?' );
  AddStr( 723, '%.0n Octet' );
  AddStr( 724, '%.0n Octets' );
  AddStr( 725, '%.0n Ko' );
  AddStr( 726, '%.0n Mo' );
  AddStr( 727, 'Sélection de %d fichier(s), %s' );
  AddStr( 729, 'Indisponible' );
  AddStr( 730, 'Renommer l''archive actuelle par :' );
  AddStr( 731, 'Impossible de renommer l''archive par "%s" !' );
  AddStr( 732, 'Configuration SFX' );
  AddStr( 733, 'Créer une archive Auto-Extractible' );
  AddStr( 734, 'Créer' );
  AddStr( 735, 'Impossible de créer une archive Auto-Extractible !' );
  AddStr( 736, 'Définir un commentaire d''Archive' );
  AddStr( 737, 'Commentaire d''Archive' );
  AddStr( 738, 'Une opération est en cours. Veuillez attendre la fin ou bien cliquer sur Annuler.' );
  AddStr( 739, 'Vous avez exécuté un programme. Quittez-le d''abord et réessayez ensuite.' );
  AddStr( 740, 'Vous devez d''abord ouvrir ou créer une archive.' );
  AddStr( 741, 'Impossible de trouver le programme associé à %s' );
  AddStr( 742, 'Nom' );
  AddStr( 743, 'Date' );
  AddStr( 744, 'Heure' );
  AddStr( 745, 'Taille normale' );
  AddStr( 746, 'Gain' );
  AddStr( 747, 'Taille Compressée' );
  AddStr( 748, 'Seg#' );
  AddStr( 749, 'Chemin' );
  AddStr( 750, '&Fichier' );
  AddStr( 751, '&Actions' );
  AddStr( 752, '&Options' );
  AddStr( 753, 'A&ide' );
  AddStr( 754, '&Nouvelle archive...' );
  AddStr( 755, '&Ouvrir archive...' );
  AddStr( 756, 'Ouvrir &segment...' );
  AddStr( 757, '&Fermer archive' );
  AddStr( 758, '&Information...' );
  AddStr( 759, 'Re&nommer archive' );
  AddStr( 760, '&Vider archive' );
  AddStr( 761, '&Supprimer archive' );
  AddStr( 762, '&Quitter' );
  AddStr( 763, '&Voir...' );
  AddStr( 764, '&Tout sélectionner' );
  AddStr( 765, '&Créer un fichier .EXE' );
  AddStr( 766, 'Définir un commentaire d''Archive...' );
  AddStr( 767, '&Configuration SFX ...' );
  AddStr( 769, '&A propos...' );
  AddStr( 770, 'Créer une nouvelle archive' );
  AddStr( 771, 'Ouvrir une archive existante' );
  AddStr( 772, 'Ouvrir l''un des segments d''une Archive' );
  AddStr( 773, 'Fermer cette archive' );
  AddStr( 774, 'Afficher des informations sur cette archive' );
  AddStr( 775, 'Renommer l''archive courante...' );
  AddStr( 776, 'Vider le contenu de l''archive' );
  AddStr( 777, 'Supprimer l''archive' );
  AddStr( 778, 'Quitter l''application' );
  AddStr( 781, 'Ajouter des fichiers à l''archive' );
  AddStr( 782, 'Extraire des fichiers de l''archive' );
  AddStr( 783, 'Supprimer des fichiers de l''archive' );
  AddStr( 784, 'Voir des fichiers' );
  AddStr( 785, 'Selectionner tous les fichiers de l''archive' );
  AddStr( 786, 'Créer une archive Auto-Extractible' );
  AddStr( 787, 'Définir un commentaire pour l''archive courante' );
  AddStr( 788, 'Changer la configuration' );
  AddStr( 789, 'Changer la configuration pour la créations d''archives Auto-Extractibles' );
  AddStr( 790, 'A propos de l''application' );
  AddStr( 798, '&Configuration...' );
  AddStr( 799, 'Fichier %s' );
  AddStr( 800, 'Femeture de l''archive...' );
  AddStr( 801, 'A&ucune sélection' );
  AddStr( 802, '&Inverser la sélection' );
  AddStr( 803, 'Racine' );
  AddStr( 804, 'Vue arborescente' );
  AddStr( 805, 'Grandes icônes' );
  AddStr( 806, 'Petites icônes' );
  AddStr( 807, 'Liste' );
  AddStr( 808, 'Détail' );
  AddStr( 809, 'Tout déplier' );
  AddStr( 810, 'Tout replier' );
  AddStr( 811, 'Effacement de la liste de fichiers' );
  AddStr( 812, 'Création de la liste de fichiers' );
  AddStr( 813, 'Tri de la liste de fichiers' );
  AddStr( 814, 'L''archive %s n''existe pas !' );
  AddStr( 815, 'Véri&fier l''intégrité' );
  AddStr( 816, 'Vérifier l''intégrité de l''archive courante' );
  AddStr( 817, '&Voir les dernières opérations...' );
  AddStr( 818, 'Voir le résultat des dernières opérations' );
  AddStr( 819, 'Installer' );
  AddStr( 820, 'Extraire le contenu et exécuter le programme d''installation' );
  AddStr( 821, '&Police...' );
  AddStr( 822, '&Trier' );
  AddStr( 823, '&Ordre Original' );
  AddStr( 824, 'Changer la police courante' );
  AddStr( 825, 'Sélectionner un ordre de tri' );
  // unit fmAdd and fmAddDropedFiles
  AddStr( 900, 'Ajouter depuis' );
  AddStr( 901, 'Nom :' );
  AddStr( 902, 'Dossiers' );
  AddStr( 903, 'Inclure les sous-dossiers ?' );
  AddStr( 904, 'Inclure le chemin courant ?' );
  AddStr( 905, 'Stocker les dossiers vides' );
  AddStr( 906, 'Stockage du chemin d''accès :' );
  AddStr( 907, 'Crypter les fichiers ?' );
  AddStr( 908, 'Niveau de compression :' );
  AddStr( 909, 'Ajouter' );
  AddStr( 910, 'Aucun'+#13+
               'Complet'+#13+
               'Relatif' );
  AddStr( 911, 'Maximum (le plus lent)'+#13+
               'Normal'+#13+
               'Rapide'+#13+
               'Super Rapide'+#13+
               'Aucun' );
  AddStr( 912, 'Ajouter les fichiers déposés' );
  AddStr( 913, 'Ajouter les éléments' );
  AddStr( 914, 'Filtre :' );
  AddStr( 915, 'Ajouter au répertoire courant ?' );
  // unit fmConfiguration
  AddStr( 1000, 'Configuration' );
  AddStr( 1001, 'Segmentation' );
  AddStr( 1002, 'Création d''archive' );
  AddStr( 1003, 'Options' );
  AddStr( 1004, 'Segmenter l''archive' );
  AddStr( 1005, 'Taille maximum d''un segment d''archive :' );
  AddStr( 1006, '720 Ko'+#13+
                '1,44 Mo'+#13+
                'Autre (Ko):' );
  AddStr( 1007, 'Comprimer l''archive' );
  AddStr( 1008, 'Crypter l''archive' );
  AddStr( 1009, 'Archive solide' );
  AddStr( 1010, 'Lecture seule' );
  AddStr( 1011, 'Créer une archive SFX' );
  AddStr( 1014, 'Taille de bloc' );
  AddStr( 1015, 'Réserver espace' );
  AddStr( 1016, 'Ko' );
  AddStr( 1017, 'Langage:' );
  AddStr( 1018, 'Automatique'+#13+
                'Anglais'+#13+
                'Français'+#13+
                'Chinois'+#13+
                'Portugais'+#13+
                'Allemand'+#13+
                'Italien'+#13+
                'Russe'+#13+
                'Espagnol' );
  AddStr( 1019, 'Afficher les répertoires vides' );
  AddStr( 1020, 'Montrer l''arborescence' );
  // unit fmCreateFolder
  AddStr( 1100, 'Répertoire courant :' );
  AddStr( 1101, 'Nom :' );
  // unit fmDelete
  AddStr( 1200, 'Supprimer' );
  AddStr( 1201, 'Fichiers' );
  AddStr( 1202, '&Archive complète'+#13+
                'Fichiers &sélectionnés'+#13+
                '&Fichiers:' );
  // unit fmEnterCryptKey
  AddStr( 1300, 'Message système' );
  AddStr( 1301, 'Cacher le mot de passe ?' );
  // unit fmExtract
  AddStr( 1400, 'Extraire' );
  AddStr( 1401, 'Extraire vers :' );
  AddStr( 1402, 'Fichiers' );
  AddStr( 1403, 'Fichiers &sélectionnés'+#13+
                '&Tous les fichiers'+#13+
                'F&ichiers :' );
  AddStr( 1404, '&Recréer les dossiers' );
  AddStr( 1405, 'Ecraser les fichiers existants'+#13+
                'Sauter les fichiers existants'+#13+
                'Mettre à jour les fichiers'+#13+
                'Demander confirmation'+#13+
                'Restorer les fichiers existants seulement'+#13+
                'Mettre à jour les fichiers existants' );
  AddStr( 1406, 'Dossiers / Unités' );
  AddStr( 1407, 'Nouveau dossier...' );
  // unit fmHelpOnSFX
  AddStr( 1500, 'Les mots-clé suivants peuvent être utilisés dans les champs'+#13+
                '"Ligne de commande" et "Répertoire d''extraction par défaut" :' );
  AddStr( 1501, 'sera remplacé par le répertoire Temporaire'+#13+
                '(souvent ''c:\windows\temp'' ou ''c:\win95\temp'' ou ''c:\temp'')' );
  AddStr( 1502, 'sera remplacé par le répertoire Windows'+#13+
                '(souvent ''c:\windows'' ou ''c:\win95'')' );
  AddStr( 1503, 'sera remplacé par le répertoire Système'+#13+
                '(souvent ''c:\windows\system'' ou ''c:\win95\system'')' );
  AddStr( 1504, 'sera remplacé par le répertoire ProgramFiles'+#13+
                '(souvent ''c:\Program Files'' [selon la langue'+#13+
                'utilisée par Windows])' );
  AddStr( 1505, 'sera remplacé par le répertoire dans lequel les fichiers'+#13+
                'ont été extraits (seulement pour les champs'+#13+
                '"Ligne de commande" ou "Arguments")' );
  AddStr( 1506, 'Exemple:' );
  AddStr( 1507, '<PF>MaSociété\MonProduit' );
  // unit fmInformation
  AddStr( 1600, 'Chemin:' );
  AddStr( 1601, 'Nom:' );
  AddStr( 1602, 'Taille du fichier:' );
  AddStr( 1603, 'Fichiers:' );
  AddStr( 1604, 'Compression:' );
  AddStr( 1605, 'Date/Heure:' );
  AddStr( 1606, 'Segment:' );
  AddStr( 1607, 'Attributs' );
  AddStr( 1608, 'Crypté' );
  AddStr( 1609, 'Comprimé' );
  AddStr( 1610, 'Solide' );
  AddStr( 1611, 'Lecture seule' );
  AddStr( 1612, 'Segment final' );
  AddStr( 1613, 'Information' );
  // unit fmSFXComments
  AddStr( 1700, 'Commentaires' );
  AddStr( 1701, 'Commentaire affiché lors de l''ouverture de l''archive SFX' );
  AddStr( 1702, 'Commentaire affiché après l''extraction des fichiers stockés dans l''archive SFX' );
  AddStr( 1703, 'Effacer les commentaires' );
  // unit fmSFXConfig
  AddStr( 1800, 'Configuration SFX' );
  AddStr( 1801, 'Exécuter un fichier après l''extraction ?' );
  AddStr( 1802, 'L''utilisateur choisit les fichiers à extraire ?' );
  AddStr( 1803, 'L''utilisateur choisit le mode de remplacement ?');
  AddStr( 1804, 'Titre:' );
  AddStr( 1805, 'Ligne de commande:' );
  AddStr( 1806, 'Arguments:' );
  AddStr( 1807, 'Répertoire d''extraction par défaut:' );
  AddStr( 1808, 'Mode de remplacement:' );
  AddStr( 1809, 'Commentaires...' );
  AddStr( 1810, 'Demander confirmation'+#13+
                'Remplacer les fichiers existants'+#13+
                'Sauter les fichiers existants'+#13+
                'Remplacer seulement si plus récent'+#13+
                'Restorer les fichiers existants uniquement'+#13+
                'Extraire le fichier s''il existe déjà et s''il est plus récent' );
  AddStr( 1811, 'L''utilisateur peut ne pas exécuter le programme ?' );
  // unit fmTextViewer
  AddStr( 1900, 'Voir: %s' );
  AddStr( 1901, '&Copier dans le presse-papier' );
  AddStr( 1902, '&Police' );
  // unit fmView
  AddStr( 2000, 'Voir : %s' );
  AddStr( 2001, 'En utilisant' );
  AddStr( 2002, 'Voir' );
  AddStr( 2003, '&Programme associé (%s)'+#13+
                '&Afficheur de texte ASCII interne' );
  // unit fmLastOutput
  AddStr( 2100, 'Voir les dernières opérations' );
end;

end.
