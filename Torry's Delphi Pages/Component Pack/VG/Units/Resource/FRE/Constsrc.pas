{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         ConstsRC - French Consts messages             }
{                                                       }
{         Copyright (c) 1997, 1998                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit ConstsRc;

interface

procedure InitResStringsConsts;
procedure FreeResStringsConsts;

implementation
uses Consts, vgUtils;

{$IFDEF _D3_}
resourcestring
  SAssignError = 'Ne peut assigner un %s à un %s';
  SFCreateError = 'Ne peut créer le fichier %s';
  SFOpenError = 'Ne peut ouvrir le fichier %s';
  SReadError = 'Erreur de lecture de flux';
  SWriteError = 'Erreur d''écriture de flux';
  SMemoryStreamError = 'Manque de mémoire en étendant le flux mémoire';
  SCantWriteResourceStreamError = 'Ne peut écrire vers un flux en lecture seule';
  SDuplicateReference = 'WriteObject appellé deux fois par la même instance';
  SClassNotFound = 'Classe %s non-trouvée';
  SInvalidImage = 'Format de flux incorrect';
  SResNotFound = 'Resource %s non-trouvée';
  SClassMismatch = 'Resource %s de classe incorrecte';
  SListIndexError = 'Index de liste hors limites (%d)';
  SListCapacityError = 'Capacité de liste hors limites (%d)';
  SListCountError = 'Nombre de liste hors limites (%d)';
  SSortedListError = 'Operation non-permise sur une stringlist triée';
  SDuplicateString = 'Stringlist ne permet pas de doublons';
  SInvalidTabIndex = 'Index de Tab hors limites';
  SDuplicateName = 'Un composant %s existe déjà';
  SInvalidName = '''''%s'''' n''est pas un nom de composant valide';
  SDuplicateClass = 'Une classe nommée %s existe déjà';
  SNoComSupport = '%s n''a pas été enregistrée comme classe COM';
  SInvalidInteger = '''''%s'''' n''est pas une valeur entière valide';
  SLineTooLong = 'Ligne trop longue';
  SInvalidPropertyValue = 'Valeur de propriété incorrecte';
  SInvalidPropertyPath = 'Chemin de propriété incorrect';
  SUnknownProperty = 'La propriété n''existe pas';
  SReadOnlyProperty = 'La propriété est en lecture seule';
  SPropertyException = 'Erreur en lisant %s.%s: %s';
  SAncestorNotFound = 'Ancêtre de ''%s'' non-trouvé';
  SInvalidBitmap = 'Image bitmap non-valide';
  SInvalidIcon = 'Icône non-valide';
  SInvalidMetafile = 'Metafile non-valide';
  SInvalidPixelFormat = 'Format de pixel invalide';
  SBitmapEmpty = 'Bitmap vide';
  SScanLine = 'Scan line index hors limite';
  SChangeIconSize = 'Ne peut changer la taille d''un Icône';
  SOleGraphic = 'Opération invalide sur un TOleGraphic';
  SUnknownExtension = 'Extension de format d''image inconnu (.%s)';
  SUnknownClipboardFormat = 'Format de presse-papier non-supporté';
  SOutOfResources = 'Manque de ressources système';
  SNoCanvasHandle = 'Canvas ne permet pas le dessin';
  SInvalidImageSize = 'Taille d''image invalide';
  STooManyImages = 'Trop d''images';
  SDimsDoNotMatch = 'Les dimensions de l''image ne correspondent pas aux dimensions de l''imagelist';
  SInvalidImageList = 'ImageList invalide';
  SReplaceImage = 'Impossible de remplacer l''image';
  SImageIndexError = 'Index d''ImageList invalide';
  SImageReadFail = 'Erreur en lisant les données de l''imageList à partir du flux';
  SImageWriteFail = 'Erreur en écrivant les données de l''imageList vers le flux';
  SWindowDCError = 'Erreur en créant le contexte de périphérique windows';
  SClientNotSet = 'Le client du TDrag not initialisé';
  SWindowClass = 'Erreur en créant la classe de fenêtre';
  SWindowCreate = 'Erreur en créant la fenêtre';
  SCannotFocus = 'Ne peut activer une fenêtre désactivée ou invisible';
  SParentRequired = 'Le contrôle ''%s'' n''a pas de fenêtre parente';
  SMDIChildNotVisible = 'Ne peut cacher une fiche enfant MDI';
  SVisibleChanged = 'Ne peut changer Visible dans OnShow ou OnHide';
  SCannotShowModal = 'Ne peut rendre une fenêtre visible modale';
  SScrollBarRange = 'Propriété Scrollbar hors limite';
  SPropertyOutOfRange = 'Propriété %s hors limite';
  SMenuIndexError = 'Index de Menu hors limite';
  SMenuReinserted = 'Menu inséré deux fois';
  SMenuNotFound = 'Le sous-menu n''est pas dans le menu';
  SNoTimers = 'Pas assez de Timers disponibles';
  SNotPrinting = 'L''imprimante n''est pas en cours d''impression';
  SPrinting = 'Impression en cours';
  SPrinterIndexError = 'Index d''imprimante hors limite';
  SInvalidPrinter = 'L''imprimante sélectionnée n''est pas valide';
  SDeviceOnPort = '%s sur %s';
  SGroupIndexTooLow = 'GroupIndex ne peut être plus petit qu''un GroupIndex d''un MenuItem précédent';
  STwoMDIForms = 'Ne peut avoir plus d''une fiche MDI par application';
  SNoMDIForm = 'Ne peut créer la fiche. Aucune fiche MDI disponible';
  SRegisterError = 'Enregistrement de composant invalide';
  SImageCanvasNeedsBitmap = 'Ne peut modifier une image que si elle contient un bitmap';
  SControlParentSetToSelf = 'Un contrôle ne peut être son propre parent';
  SOKButton = 'OK';
  SCancelButton = 'Annuler';
  SYesButton = '&Oui';
  SNoButton = '&Non';
  SHelpButton = '&Aide';
  SCloseButton = '&Ferme';
  SIgnoreButton = '&Ignorer';
  SRetryButton = '&Réessayer';
  SAbortButton = 'Annuler';
  SAllButton = '&Tous';

  SCannotDragForm = 'Ne peut glisser-déplacer une fiche';
  SPutObjectError = 'PutObject vers un objet non-défini';
  SCardDLLNotLoaded = 'Ne peut charger CARDS.DLL';
  SDuplicateCardId = 'Un CardId dupliqué a été trouvé';

  SDdeErr = 'Une erreur retournée par le DDE  ($0%x)';
  SDdeConvErr = 'Erreur DDE - conversation non-établie ($0%x)';
  SDdeMemErr = 'Une erreur est apparue quand le DDE se trouva en manque de mémoire ($0%x)';
  SDdeNoConnect = 'Impossible de se connecter à la conversation DDE';

  SFB = 'FB';
  SFG = 'FG';
  SBG = 'BG';
  SOldTShape = 'Ne peut charger une ancienne version de TShape';
  SVMetafiles = 'Metafichiers';
  SVEnhMetafiles = 'Metafichiers étendus';
  SVIcons = 'Icônes';
  SVBitmaps = 'Bitmaps';
  SGridTooLarge = 'Grille trop grande pour l''opération';
  STooManyDeleted = 'Trop de lignes/colonnes effacées';
  SIndexOutOfRange = 'Index de grille hors limite';
  SFixedColTooBig = 'Le nombre de colonnes fixes ne peut être inférieur au nombre de colonnes';
  SFixedRowTooBig = 'Le nombre de lignes fixes ne peut être inférieur au nombre de lignes';
  SInvalidStringGridOp = 'Ne peut insérer ou supprimer des lignes dans la grille';
  SParseError = '%s dans la ligne %d';
  SIdentifierExpected = 'Identifiant attendu';
  SStringExpected = 'Chaîne attendue';
  SNumberExpected = 'Nombre attendu';
  SCharExpected = '''''%s'''' attendu';
  SSymbolExpected = '%s attendu';
  SInvalidNumber = 'Valeur numérique invalide';
  SInvalidString = 'Constante chaîne invalide';
  SInvalidProperty = 'Valeur de propriété invalide';
  SInvalidBinary = 'Valeur binaire invalide';
  SOutlineIndexError = 'Index d''Outline non trouvé';
  SOutlineExpandError = 'Le Parent doit être étendu';
  SInvalidCurrentItem = 'Valeur incorrecte pour l''élement en cours';
  SMaskErr = 'Valeur d''entrée invalide';
  SMaskEditErr = 'Valeur d''entrée invalide.  Pressez ESC pour annuler les modifications';
  SOutlineError = 'Index d''Outline invalide';
  SOutlineBadLevel = 'Mauvais assignement de niveau';
  SOutlineSelection = 'Sélection invalide';
  SOutlineFileLoad = 'Erreur de chargement de fichier';
  SOutlineLongLine = 'Ligne trop longue';
  SOutlineMaxLevels = 'Profondeur maximale d''Outline dépassée';

  SMsgDlgWarning = 'Avertissement';
  SMsgDlgError = 'Erreur';
  SMsgDlgInformation = 'Information';
  SMsgDlgConfirm = 'Confirmation';
  SMsgDlgYes = '&Oui';
  SMsgDlgNo = '&Non';
  SMsgDlgOK = 'OK';
  SMsgDlgCancel = 'Annuler';
  SMsgDlgHelp = '&Aide';
  SMsgDlgHelpNone = 'Aide non-disponible';
  SMsgDlgHelpHelp = 'Aide';
  SMsgDlgAbort = '&Annuler';
  SMsgDlgRetry = '&Réessayer';
  SMsgDlgIgnore = '&Ignorer';
  SMsgDlgAll = '&Tous';
  SMsgDlgNoToAll = 'N&on à tous';
  SMsgDlgYesToAll = 'O&ui à tous';

  SmkcBkSp = 'BkSp';
  SmkcTab = 'Tab';
  SmkcEsc = 'Esc';
  SmkcEnter = 'Entrée';
  SmkcSpace = 'Espace';
  SmkcPgUp = 'PgUp';
  SmkcPgDn = 'PgDn';
  SmkcEnd = 'Fin';
  SmkcHome = 'Début';
  SmkcLeft = 'Gauche';
  SmkcUp = 'Haut';
  SmkcRight = 'Droite';
  SmkcDown = 'Bas';
  SmkcIns = 'Ins';
  SmkcDel = 'Suppr';
  SmkcShift = 'Maj+';
  SmkcCtrl = 'Ctrl+';
  SmkcAlt = 'Alt+';

  srUnknown = '(Inconnu)';
  srNone = '(Aucun)';
  SOutOfRange = 'La valeur doit se situer entre %d et %d';
  SCannotCreateName = 'Ne peut créer un nom de méthode par défaut pour un composant sans nom';

  SDateEncodeError = 'Argument invalide pour encoder la date';
  STimeEncodeError = 'Argument invalide pour encoder l''heure';
  SInvalidDate = '''''%s'''' n''est pas une date valide';
  SInvalidTime = '''''%s'''' n''est pas une heure valide';
  SInvalidDateTime = '''''%s'''' n''est pas une date et heure valide';
  SInvalidFileName = 'Nom de fichier invalide - %s';
  SDefaultFilter = 'Tous les fichiers (*.*)|*.*';
  sAllFilter = 'Tous';
  SNoVolumeLabel = ': [ - Pas de nom de volume - ]';
  SInsertLineError = 'Impossible d''insérer une ligne';

  SConfirmCreateDir = 'Le répertoire spécifié n''existe pas. Le créer ?';
  SSelectDirCap = 'Choisissez le répertoire';
  SCannotCreateDir = 'Impossible de créer le répertoire';
  SDirNameCap = '&Nom du répertoire :';
  SDrivesCap = 'Lecteu&rs:';
  SDirsCap = 'Ré&pertoires:';
  SFilesCap = '&Fichiers: (*.*)';
  SNetworkCap = 'Ré&seau...';

  SColorPrefix = 'Couleur';
  SColorTags = 'ABCDEFGHIJKLMNOP';

  SInvalidClipFmt = 'Format de presse papier invalide';
  SIconToClipboard = 'Le presse papier ne supporte pas les icônes';

  SDefault = 'Defaut';

  SInvalidMemoSize = 'Le texte dépasse la capacité du mémo';
  SCustomColors = 'Couleurs personnalisées';
  SInvalidPrinterOp = 'Opération non-supportée sur l''imprimante sélectionnée';
  SNoDefaultPrinter = 'Il n''y a pas d''imprimante par défaut de sélectionnée';

  SIniFileWriteError = 'Impossible d''écrire vers %s';

  SBitsIndexError = 'Index de Bits hors limite';

  SUntitled = '(Sans-titre)';

  SInvalidRegType = 'Type de donnée invalide pour ''%s''';
  SRegCreateFailed = 'Erreur en créant la clé %s';
  SRegSetDataFailed = 'Erreur en définissant les données pour ''%s''';
  SRegGetDataFailed = 'Erreur en obtennant les données de ''%s''';

  SUnknownConversion = 'Extension de fichier de conversion RichEdit inconnue (.%s)';
  SDuplicateMenus = 'Le Menu ''%s'' est déjà utilisé par une autre fiche';

{$ENDIF}

procedure InitResStringsConsts;
begin
{$IFDEF _D3_}
  CopyResString(@SAssignError                  , @Consts.SAssignError                  , True);
  CopyResString(@SFCreateError                 , @Consts.SFCreateError                 , True);
  CopyResString(@SFOpenError                   , @Consts.SFOpenError                   , True);
  CopyResString(@SReadError                    , @Consts.SReadError                    , True);
  CopyResString(@SWriteError                   , @Consts.SWriteError                   , True);
  CopyResString(@SMemoryStreamError            , @Consts.SMemoryStreamError            , True);
  CopyResString(@SCantWriteResourceStreamError , @Consts.SCantWriteResourceStreamError , True);
  CopyResString(@SDuplicateReference           , @Consts.SDuplicateReference           , True);
  CopyResString(@SClassNotFound                , @Consts.SClassNotFound                , True);
  CopyResString(@SInvalidImage                 , @Consts.SInvalidImage                 , True);
  CopyResString(@SResNotFound                  , @Consts.SResNotFound                  , True);
  CopyResString(@SClassMismatch                , @Consts.SClassMismatch                , True);
  CopyResString(@SListIndexError               , @Consts.SListIndexError               , True);
  CopyResString(@SSortedListError              , @Consts.SSortedListError              , True);
  CopyResString(@SDuplicateString              , @Consts.SDuplicateString              , True);    
  CopyResString(@SInvalidTabIndex              , @Consts.SInvalidTabIndex              , True);    
  CopyResString(@SDuplicateName                , @Consts.SDuplicateName                , True);    
  CopyResString(@SInvalidName                  , @Consts.SInvalidName                  , True);    
  CopyResString(@SDuplicateClass               , @Consts.SDuplicateClass               , True);    
  CopyResString(@SInvalidInteger               , @Consts.SInvalidInteger               , True);    
  CopyResString(@SLineTooLong                  , @Consts.SLineTooLong                  , True);
  CopyResString(@SInvalidPropertyValue         , @Consts.SInvalidPropertyValue         , True);
  CopyResString(@SInvalidPropertyPath          , @Consts.SInvalidPropertyPath          , True);
  CopyResString(@SUnknownProperty              , @Consts.SUnknownProperty              , True);    
  CopyResString(@SReadOnlyProperty             , @Consts.SReadOnlyProperty             , True);    
  CopyResString(@SPropertyException            , @Consts.SPropertyException            , True);    
  CopyResString(@SAncestorNotFound             , @Consts.SAncestorNotFound             , True);
  CopyResString(@SInvalidBitmap                , @Consts.SInvalidBitmap                , True);    
  CopyResString(@SInvalidIcon                  , @Consts.SInvalidIcon                  , True);    
  CopyResString(@SInvalidMetafile              , @Consts.SInvalidMetafile              , True);    
  CopyResString(@SBitmapEmpty                  , @Consts.SBitmapEmpty                  , True);
  CopyResString(@SChangeIconSize               , @Consts.SChangeIconSize               , True);    
  CopyResString(@SUnknownExtension             , @Consts.SUnknownExtension             , True);
  CopyResString(@SUnknownClipboardFormat       , @Consts.SUnknownClipboardFormat       , True);
  CopyResString(@SOutOfResources               , @Consts.SOutOfResources               , True);
  CopyResString(@SNoCanvasHandle               , @Consts.SNoCanvasHandle               , True);    
  CopyResString(@SInvalidImageSize             , @Consts.SInvalidImageSize             , True);    
  CopyResString(@STooManyImages                , @Consts.STooManyImages                , True);    
  CopyResString(@SDimsDoNotMatch               , @Consts.SDimsDoNotMatch               , True);    
  CopyResString(@SInvalidImageList             , @Consts.SInvalidImageList             , True);
  CopyResString(@SReplaceImage                 , @Consts.SReplaceImage                 , True);    
  CopyResString(@SImageIndexError              , @Consts.SImageIndexError              , True);
  CopyResString(@SWindowDCError                , @Consts.SWindowDCError                , True);
  CopyResString(@SClientNotSet                 , @Consts.SClientNotSet                 , True);
  CopyResString(@SWindowClass                  , @Consts.SWindowClass                  , True);    
  CopyResString(@SWindowCreate                 , @Consts.SWindowCreate                 , True);    
  CopyResString(@SCannotFocus                  , @Consts.SCannotFocus                  , True);    
  CopyResString(@SParentRequired               , @Consts.SParentRequired               , True);
  CopyResString(@SMDIChildNotVisible           , @Consts.SMDIChildNotVisible           , True);
  CopyResString(@SVisibleChanged               , @Consts.SVisibleChanged               , True);    
  CopyResString(@SCannotShowModal              , @Consts.SCannotShowModal              , True);    
  CopyResString(@SScrollBarRange               , @Consts.SScrollBarRange               , True);    
  CopyResString(@SPropertyOutOfRange           , @Consts.SPropertyOutOfRange           , True);    
  CopyResString(@SMenuIndexError               , @Consts.SMenuIndexError               , True);
  CopyResString(@SMenuReinserted               , @Consts.SMenuReinserted               , True);
  CopyResString(@SMenuNotFound                 , @Consts.SMenuNotFound                 , True);
  CopyResString(@SNoTimers                     , @Consts.SNoTimers                     , True);    
  CopyResString(@SNotPrinting                  , @Consts.SNotPrinting                  , True);
  CopyResString(@SPrinting                     , @Consts.SPrinting                     , True);    
  CopyResString(@SPrinterIndexError            , @Consts.SPrinterIndexError            , True);    
  CopyResString(@SInvalidPrinter               , @Consts.SInvalidPrinter               , True);    
  CopyResString(@SDeviceOnPort                 , @Consts.SDeviceOnPort                 , True);    
  CopyResString(@SGroupIndexTooLow             , @Consts.SGroupIndexTooLow             , True);
  CopyResString(@STwoMDIForms                  , @Consts.STwoMDIForms                  , True);
  CopyResString(@SNoMDIForm                    , @Consts.SNoMDIForm                    , True);
  CopyResString(@SRegisterError                , @Consts.SRegisterError                , True);    
  CopyResString(@SImageCanvasNeedsBitmap       , @Consts.SImageCanvasNeedsBitmap       , True);
  CopyResString(@SControlParentSetToSelf       , @Consts.SControlParentSetToSelf       , True);    
  CopyResString(@SOKButton                     , @Consts.SOKButton                     , True);
  CopyResString(@SCancelButton                 , @Consts.SCancelButton                 , True);    
  CopyResString(@SYesButton                    , @Consts.SYesButton                    , True);    
  CopyResString(@SNoButton                     , @Consts.SNoButton                     , True);    
  CopyResString(@SHelpButton                   , @Consts.SHelpButton                   , True);    
  CopyResString(@SCloseButton                  , @Consts.SCloseButton                  , True);    
  CopyResString(@SIgnoreButton                 , @Consts.SIgnoreButton                 , True);
  CopyResString(@SRetryButton                  , @Consts.SRetryButton                  , True);
  CopyResString(@SAbortButton                  , @Consts.SAbortButton                  , True);
  CopyResString(@SAllButton                    , @Consts.SAllButton                    , True);    

  CopyResString(@SCannotDragForm               , @Consts.SCannotDragForm               , True);    
  CopyResString(@SPutObjectError               , @Consts.SPutObjectError               , True);    
  CopyResString(@SCardDLLNotLoaded             , @Consts.SCardDLLNotLoaded             , True);    
  CopyResString(@SDuplicateCardId              , @Consts.SDuplicateCardId              , True);    

  CopyResString(@SDdeErr                       , @Consts.SDdeErr                       , True);
  CopyResString(@SDdeConvErr                   , @Consts.SDdeConvErr                   , True);
  CopyResString(@SDdeMemErr                    , @Consts.SDdeMemErr                    , True);    
  CopyResString(@SDdeNoConnect                 , @Consts.SDdeNoConnect                 , True);    

  CopyResString(@SFB                           , @Consts.SFB                           , True);
  CopyResString(@SFG                           , @Consts.SFG                           , True);    
  CopyResString(@SBG                           , @Consts.SBG                           , True);    
  CopyResString(@SOldTShape                    , @Consts.SOldTShape                    , True);    
  CopyResString(@SVMetafiles                   , @Consts.SVMetafiles                   , True);
  CopyResString(@SVEnhMetafiles                , @Consts.SVEnhMetafiles                , True);    
  CopyResString(@SVIcons                       , @Consts.SVIcons                       , True);
  CopyResString(@SVBitmaps                     , @Consts.SVBitmaps                     , True);
  CopyResString(@SGridTooLarge                 , @Consts.SGridTooLarge                 , True);
  CopyResString(@STooManyDeleted               , @Consts.STooManyDeleted               , True);    
  CopyResString(@SIndexOutOfRange              , @Consts.SIndexOutOfRange              , True);    
  CopyResString(@SFixedColTooBig               , @Consts.SFixedColTooBig               , True);    
  CopyResString(@SFixedRowTooBig               , @Consts.SFixedRowTooBig               , True);    
  CopyResString(@SParseError                   , @Consts.SParseError                   , True);
  CopyResString(@SIdentifierExpected           , @Consts.SIdentifierExpected           , True);    
  CopyResString(@SStringExpected               , @Consts.SStringExpected               , True);
  CopyResString(@SNumberExpected               , @Consts.SNumberExpected               , True);
  CopyResString(@SCharExpected                 , @Consts.SCharExpected                 , True);
  CopyResString(@SSymbolExpected               , @Consts.SSymbolExpected               , True);    
  CopyResString(@SInvalidNumber                , @Consts.SInvalidNumber                , True);    
  CopyResString(@SInvalidString                , @Consts.SInvalidString                , True);    
  CopyResString(@SInvalidProperty              , @Consts.SInvalidProperty              , True);
  CopyResString(@SInvalidBinary                , @Consts.SInvalidBinary                , True);
  CopyResString(@SOutlineIndexError            , @Consts.SOutlineIndexError            , True);    
  CopyResString(@SOutlineExpandError           , @Consts.SOutlineExpandError           , True);    
  CopyResString(@SInvalidCurrentItem           , @Consts.SInvalidCurrentItem           , True);    
  CopyResString(@SMaskErr                      , @Consts.SMaskErr                      , True);    
  CopyResString(@SMaskEditErr                  , @Consts.SMaskEditErr                  , True);
  CopyResString(@SOutlineError                 , @Consts.SOutlineError                 , True);
  CopyResString(@SOutlineBadLevel              , @Consts.SOutlineBadLevel              , True);
  CopyResString(@SOutlineSelection             , @Consts.SOutlineSelection             , True);    
  CopyResString(@SOutlineFileLoad              , @Consts.SOutlineFileLoad              , True);
  CopyResString(@SOutlineLongLine              , @Consts.SOutlineLongLine              , True);    
  CopyResString(@SOutlineMaxLevels             , @Consts.SOutlineMaxLevels             , True);    

  CopyResString(@SMsgDlgWarning                , @Consts.SMsgDlgWarning                , True);    
  CopyResString(@SMsgDlgError                  , @Consts.SMsgDlgError                  , True);
  CopyResString(@SMsgDlgInformation            , @Consts.SMsgDlgInformation            , True);    
  CopyResString(@SMsgDlgConfirm                , @Consts.SMsgDlgConfirm                , True);
  CopyResString(@SMsgDlgYes                    , @Consts.SMsgDlgYes                    , True);    
  CopyResString(@SMsgDlgNo                     , @Consts.SMsgDlgNo                     , True);
  CopyResString(@SMsgDlgOK                     , @Consts.SMsgDlgOK                     , True);    
  CopyResString(@SMsgDlgCancel                 , @Consts.SMsgDlgCancel                 , True);
  CopyResString(@SMsgDlgHelp                   , @Consts.SMsgDlgHelp                   , True);    
  CopyResString(@SMsgDlgHelpNone               , @Consts.SMsgDlgHelpNone               , True);    
  CopyResString(@SMsgDlgHelpHelp               , @Consts.SMsgDlgHelpHelp               , True);    
  CopyResString(@SMsgDlgAbort                  , @Consts.SMsgDlgAbort                  , True);    
  CopyResString(@SMsgDlgRetry                  , @Consts.SMsgDlgRetry                  , True);    
  CopyResString(@SMsgDlgIgnore                 , @Consts.SMsgDlgIgnore                 , True);
  CopyResString(@SMsgDlgAll                    , @Consts.SMsgDlgAll                    , True);

  CopyResString(@SmkcBkSp                      , @Consts.SmkcBkSp                      , True);    
  CopyResString(@SmkcTab                       , @Consts.SmkcTab                       , True);    
  CopyResString(@SmkcEsc                       , @Consts.SmkcEsc                       , True);    
  CopyResString(@SmkcEnter                     , @Consts.SmkcEnter                     , True);    
  CopyResString(@SmkcSpace                     , @Consts.SmkcSpace                     , True);    
  CopyResString(@SmkcPgUp                      , @Consts.SmkcPgUp                      , True);    
  CopyResString(@SmkcPgDn                      , @Consts.SmkcPgDn                      , True);
  CopyResString(@SmkcEnd                       , @Consts.SmkcEnd                       , True);
  CopyResString(@SmkcHome                      , @Consts.SmkcHome                      , True);
  CopyResString(@SmkcLeft                      , @Consts.SmkcLeft                      , True);    
  CopyResString(@SmkcUp                        , @Consts.SmkcUp                        , True);    
  CopyResString(@SmkcRight                     , @Consts.SmkcRight                     , True);    
  CopyResString(@SmkcDown                      , @Consts.SmkcDown                      , True);
  CopyResString(@SmkcIns                       , @Consts.SmkcIns                       , True);    
  CopyResString(@SmkcDel                       , @Consts.SmkcDel                       , True);    
  CopyResString(@SmkcShift                     , @Consts.SmkcShift                     , True);    
  CopyResString(@SmkcCtrl                      , @Consts.SmkcCtrl                      , True);
  CopyResString(@SmkcAlt                       , @Consts.SmkcAlt                       , True);    

  CopyResString(@SrUnknown                     , @Consts.SrUnknown                     , True);
  CopyResString(@SrNone                        , @Consts.SrNone                        , True);
  CopyResString(@SOutOfRange                   , @Consts.SOutOfRange                   , True);    
  CopyResString(@SCannotCreateName             , @Consts.SCannotCreateName             , True);    

  CopyResString(@SDateEncodeError              , @Consts.SDateEncodeError              , True);    
  CopyResString(@STimeEncodeError              , @Consts.STimeEncodeError              , True);
  CopyResString(@SInvalidDate                  , @Consts.SInvalidDate                  , True);    
  CopyResString(@SInvalidTime                  , @Consts.SInvalidTime                  , True);
  CopyResString(@SInvalidDateTime              , @Consts.SInvalidDateTime              , True);    
  CopyResString(@SInvalidFileName              , @Consts.SInvalidFileName              , True);
  CopyResString(@SDefaultFilter                , @Consts.SDefaultFilter                , True);    
  CopyResString(@SAllFilter                    , @Consts.SAllFilter                    , True);    
  CopyResString(@SNoVolumeLabel                , @Consts.SNoVolumeLabel                , True);    
  CopyResString(@SInsertLineError              , @Consts.SInsertLineError              , True);

  CopyResString(@SConfirmCreateDir             , @Consts.SConfirmCreateDir             , True);    
  CopyResString(@SSelectDirCap                 , @Consts.SSelectDirCap                 , True);    
  CopyResString(@SDirNameCap                   , @Consts.SDirNameCap                   , True);    
  CopyResString(@SDrivesCap                    , @Consts.SDrivesCap                    , True);    
  CopyResString(@SDirsCap                      , @Consts.SDirsCap                      , True);
  CopyResString(@SFilesCap                     , @Consts.SFilesCap                     , True);
  CopyResString(@SNetworkCap                   , @Consts.SNetworkCap                   , True);

  CopyResString(@SColorPrefix                  , @Consts.SColorPrefix                  , True);
  CopyResString(@SColorTags                    , @Consts.SColorTags                    , True);    

  CopyResString(@SInvalidClipFmt               , @Consts.SInvalidClipFmt               , True);    
  CopyResString(@SIconToClipboard              , @Consts.SIconToClipboard              , True);    

  CopyResString(@SDefault                      , @Consts.SDefault                      , True);    

  CopyResString(@SInvalidMemoSize              , @Consts.SInvalidMemoSize              , True);    
  CopyResString(@SCustomColors                 , @Consts.SCustomColors                 , True);
  CopyResString(@SInvalidPrinterOp             , @Consts.SInvalidPrinterOp             , True);    
  CopyResString(@SNoDefaultPrinter             , @Consts.SNoDefaultPrinter             , True);

  CopyResString(@SIniFileWriteError            , @Consts.SIniFileWriteError            , True);

  CopyResString(@SBitsIndexError               , @Consts.SBitsIndexError               , True);    

  CopyResString(@SUntitled                     , @Consts.SUntitled                     , True);

  CopyResString(@SInvalidRegType               , @Consts.SInvalidRegType               , True);
  CopyResString(@SRegCreateFailed              , @Consts.SRegCreateFailed              , True);
  CopyResString(@SRegSetDataFailed             , @Consts.SRegSetDataFailed             , True);
  CopyResString(@SRegGetDataFailed             , @Consts.SRegGetDataFailed             , True);

  CopyResString(@SUnknownConversion            , @Consts.SUnknownConversion            , True);
  CopyResString(@SDuplicateMenus               , @Consts.SDuplicateMenus               , True);
{$ENDIF}
end;

procedure FreeResStringsConsts;
begin
{$IFDEF _D3_}
  RestoreResString(@Consts.SAssignError                  );
  RestoreResString(@Consts.SFCreateError                 );
  RestoreResString(@Consts.SFOpenError                   );
  RestoreResString(@Consts.SReadError                    );
  RestoreResString(@Consts.SWriteError                   );
  RestoreResString(@Consts.SMemoryStreamError            );
  RestoreResString(@Consts.SCantWriteResourceStreamError );
  RestoreResString(@Consts.SDuplicateReference           );
  RestoreResString(@Consts.SClassNotFound                );
  RestoreResString(@Consts.SInvalidImage                 );
  RestoreResString(@Consts.SResNotFound                  );
  RestoreResString(@Consts.SClassMismatch                );
  RestoreResString(@Consts.SListIndexError               );
  RestoreResString(@Consts.SSortedListError              );    
  RestoreResString(@Consts.SDuplicateString              );    
  RestoreResString(@Consts.SInvalidTabIndex              );    
  RestoreResString(@Consts.SDuplicateName                );
  RestoreResString(@Consts.SInvalidName                  );    
  RestoreResString(@Consts.SDuplicateClass               );    
  RestoreResString(@Consts.SInvalidInteger               );
  RestoreResString(@Consts.SLineTooLong                  );
  RestoreResString(@Consts.SInvalidPropertyValue         );
  RestoreResString(@Consts.SInvalidPropertyPath          );
  RestoreResString(@Consts.SUnknownProperty              );    
  RestoreResString(@Consts.SReadOnlyProperty             );    
  RestoreResString(@Consts.SPropertyException            );    
  RestoreResString(@Consts.SAncestorNotFound             );
  RestoreResString(@Consts.SInvalidBitmap                );    
  RestoreResString(@Consts.SInvalidIcon                  );
  RestoreResString(@Consts.SInvalidMetafile              );
  RestoreResString(@Consts.SBitmapEmpty                  );
  RestoreResString(@Consts.SChangeIconSize               );    
  RestoreResString(@Consts.SUnknownExtension             );    
  RestoreResString(@Consts.SUnknownClipboardFormat       );    
  RestoreResString(@Consts.SOutOfResources               );    
  RestoreResString(@Consts.SNoCanvasHandle               );    
  RestoreResString(@Consts.SInvalidImageSize             );    
  RestoreResString(@Consts.STooManyImages                );
  RestoreResString(@Consts.SDimsDoNotMatch               );    
  RestoreResString(@Consts.SInvalidImageList             );
  RestoreResString(@Consts.SReplaceImage                 );
  RestoreResString(@Consts.SImageIndexError              );
  RestoreResString(@Consts.SWindowDCError                );
  RestoreResString(@Consts.SClientNotSet                 );
  RestoreResString(@Consts.SWindowClass                  );    
  RestoreResString(@Consts.SWindowCreate                 );    
  RestoreResString(@Consts.SCannotFocus                  );    
  RestoreResString(@Consts.SParentRequired               );
  RestoreResString(@Consts.SMDIChildNotVisible           );
  RestoreResString(@Consts.SVisibleChanged               );
  RestoreResString(@Consts.SCannotShowModal              );
  RestoreResString(@Consts.SScrollBarRange               );    
  RestoreResString(@Consts.SPropertyOutOfRange           );    
  RestoreResString(@Consts.SMenuIndexError               );    
  RestoreResString(@Consts.SMenuReinserted               );    
  RestoreResString(@Consts.SMenuNotFound                 );    
  RestoreResString(@Consts.SNoTimers                     );    
  RestoreResString(@Consts.SNotPrinting                  );
  RestoreResString(@Consts.SPrinting                     );
  RestoreResString(@Consts.SPrinterIndexError            );    
  RestoreResString(@Consts.SInvalidPrinter               );    
  RestoreResString(@Consts.SDeviceOnPort                 );
  RestoreResString(@Consts.SGroupIndexTooLow             );
  RestoreResString(@Consts.STwoMDIForms                  );
  RestoreResString(@Consts.SNoMDIForm                    );
  RestoreResString(@Consts.SRegisterError                );    
  RestoreResString(@Consts.SImageCanvasNeedsBitmap       );
  RestoreResString(@Consts.SControlParentSetToSelf       );    
  RestoreResString(@Consts.SOKButton                     );
  RestoreResString(@Consts.SCancelButton                 );    
  RestoreResString(@Consts.SYesButton                    );
  RestoreResString(@Consts.SNoButton                     );
  RestoreResString(@Consts.SHelpButton                   );    
  RestoreResString(@Consts.SCloseButton                  );    
  RestoreResString(@Consts.SIgnoreButton                 );    
  RestoreResString(@Consts.SRetryButton                  );
  RestoreResString(@Consts.SAbortButton                  );    
  RestoreResString(@Consts.SAllButton                    );    

  RestoreResString(@Consts.SCannotDragForm               );
  RestoreResString(@Consts.SPutObjectError               );    
  RestoreResString(@Consts.SCardDLLNotLoaded             );    
  RestoreResString(@Consts.SDuplicateCardId              );

  RestoreResString(@Consts.SDdeErr                       );
  RestoreResString(@Consts.SDdeConvErr                   );
  RestoreResString(@Consts.SDdeMemErr                    );
  RestoreResString(@Consts.SDdeNoConnect                 );

  RestoreResString(@Consts.SFB                           );
  RestoreResString(@Consts.SFG                           );
  RestoreResString(@Consts.SBG                           );
  RestoreResString(@Consts.SOldTShape                    );
  RestoreResString(@Consts.SVMetafiles                   );
  RestoreResString(@Consts.SVEnhMetafiles                );
  RestoreResString(@Consts.SVIcons                       );
  RestoreResString(@Consts.SVBitmaps                     );
  RestoreResString(@Consts.SGridTooLarge                 );
  RestoreResString(@Consts.STooManyDeleted               );
  RestoreResString(@Consts.SIndexOutOfRange              );
  RestoreResString(@Consts.SFixedColTooBig               );
  RestoreResString(@Consts.SFixedRowTooBig               );    
  RestoreResString(@Consts.SParseError                   );
  RestoreResString(@Consts.SIdentifierExpected           );
  RestoreResString(@Consts.SStringExpected               );
  RestoreResString(@Consts.SNumberExpected               );
  RestoreResString(@Consts.SCharExpected                 );
  RestoreResString(@Consts.SSymbolExpected               );    
  RestoreResString(@Consts.SInvalidNumber                );    
  RestoreResString(@Consts.SInvalidString                );    
  RestoreResString(@Consts.SInvalidProperty              );
  RestoreResString(@Consts.SInvalidBinary                );
  RestoreResString(@Consts.SOutlineIndexError            );
  RestoreResString(@Consts.SOutlineExpandError           );
  RestoreResString(@Consts.SInvalidCurrentItem           );    
  RestoreResString(@Consts.SMaskErr                      );    
  RestoreResString(@Consts.SMaskEditErr                  );    
  RestoreResString(@Consts.SOutlineError                 );    
  RestoreResString(@Consts.SOutlineBadLevel              );    
  RestoreResString(@Consts.SOutlineSelection             );    
  RestoreResString(@Consts.SOutlineFileLoad              );
  RestoreResString(@Consts.SOutlineLongLine              );
  RestoreResString(@Consts.SOutlineMaxLevels             );    

  RestoreResString(@Consts.SMsgDlgWarning                );
  RestoreResString(@Consts.SMsgDlgError                  );
  RestoreResString(@Consts.SMsgDlgInformation            );
  RestoreResString(@Consts.SMsgDlgConfirm                );
  RestoreResString(@Consts.SMsgDlgYes                    );    
  RestoreResString(@Consts.SMsgDlgNo                     );
  RestoreResString(@Consts.SMsgDlgOK                     );    
  RestoreResString(@Consts.SMsgDlgCancel                 );
  RestoreResString(@Consts.SMsgDlgHelp                   );    
  RestoreResString(@Consts.SMsgDlgHelpNone               );
  RestoreResString(@Consts.SMsgDlgHelpHelp               );
  RestoreResString(@Consts.SMsgDlgAbort                  );    
  RestoreResString(@Consts.SMsgDlgRetry                  );    
  RestoreResString(@Consts.SMsgDlgIgnore                 );    
  RestoreResString(@Consts.SMsgDlgAll                    );

  RestoreResString(@Consts.SmkcBkSp                      );    
  RestoreResString(@Consts.SmkcTab                       );
  RestoreResString(@Consts.SmkcEsc                       );
  RestoreResString(@Consts.SmkcEnter                     );    
  RestoreResString(@Consts.SmkcSpace                     );    
  RestoreResString(@Consts.SmkcPgUp                      );
  RestoreResString(@Consts.SmkcPgDn                      );
  RestoreResString(@Consts.SmkcEnd                       );
  RestoreResString(@Consts.SmkcHome                      );
  RestoreResString(@Consts.SmkcLeft                      );    
  RestoreResString(@Consts.SmkcUp                        );    
  RestoreResString(@Consts.SmkcRight                     );    
  RestoreResString(@Consts.SmkcDown                      );
  RestoreResString(@Consts.SmkcIns                       );    
  RestoreResString(@Consts.SmkcDel                       );
  RestoreResString(@Consts.SmkcShift                     );
  RestoreResString(@Consts.SmkcCtrl                      );
  RestoreResString(@Consts.SmkcAlt                       );    

  RestoreResString(@Consts.SrUnknown                     );    
  RestoreResString(@Consts.SrNone                        );    
  RestoreResString(@Consts.SOutOfRange                   );    
  RestoreResString(@Consts.SCannotCreateName             );

  RestoreResString(@Consts.SDateEncodeError              );    
  RestoreResString(@Consts.STimeEncodeError              );
  RestoreResString(@Consts.SInvalidDate                  );
  RestoreResString(@Consts.SInvalidTime                  );
  RestoreResString(@Consts.SInvalidDateTime              );
  RestoreResString(@Consts.SInvalidFileName              );
  RestoreResString(@Consts.SDefaultFilter                );    
  RestoreResString(@Consts.SAllFilter                    );    
  RestoreResString(@Consts.SNoVolumeLabel                );    
  RestoreResString(@Consts.SInsertLineError              );

  RestoreResString(@Consts.SConfirmCreateDir             );
  RestoreResString(@Consts.SSelectDirCap                 );
  RestoreResString(@Consts.SDirNameCap                   );    
  RestoreResString(@Consts.SDrivesCap                    );    
  RestoreResString(@Consts.SDirsCap                      );    
  RestoreResString(@Consts.SFilesCap                     );    
  RestoreResString(@Consts.SNetworkCap                   );    

  RestoreResString(@Consts.SColorPrefix                  );
  RestoreResString(@Consts.SColorTags                    );

  RestoreResString(@Consts.SInvalidClipFmt               );    
  RestoreResString(@Consts.SIconToClipboard              );

  RestoreResString(@Consts.SDefault                      );

  RestoreResString(@Consts.SInvalidMemoSize              );    
  RestoreResString(@Consts.SCustomColors                 );
  RestoreResString(@Consts.SInvalidPrinterOp             );    
  RestoreResString(@Consts.SNoDefaultPrinter             );

  RestoreResString(@Consts.SIniFileWriteError            );

  RestoreResString(@Consts.SBitsIndexError               );    

  RestoreResString(@Consts.SUntitled                     );    

  RestoreResString(@Consts.SInvalidRegType               );    
  RestoreResString(@Consts.SRegCreateFailed              );    
  RestoreResString(@Consts.SRegSetDataFailed             );
  RestoreResString(@Consts.SRegGetDataFailed             );

  RestoreResString(@Consts.SUnknownConversion            );
  RestoreResString(@Consts.SDuplicateMenus               );
{$ENDIF}
end;

{$IFDEF _D3_}
initialization

finalization
  FreeResStringsConsts;
{$ENDIF}

end.
