{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         ConstsRC - Dutch Consts messages              }
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
  SAssignError = 'Kan een %s niet toekennen aan een %s';
  SFCreateError = 'Bestand %s kan niet aangemaakt worden';
  SFOpenError = 'Bestand %s kan niet geopend worden';
  SReadError = 'Stream leesfout';
  SWriteError = 'Stream schrijffout';
  SMemoryStreamError = 'Onvoldoende geheugen bij uitpakken memory stream';
  SCantWriteResourceStreamError = 'Kan niet naar een alleen-lezen resource stream schrijven';
  SDuplicateReference = 'WriteObject tweemaal aangeroepen voor dezelfde instance';
  SClassNotFound = 'Class %s niet gevonden';
  SInvalidImage = 'Ongeldig stream formaat';
  SResNotFound = 'Resource %s niet gevonden';
  SClassMismatch = 'Resource %s niet van de correcte class';
  SListIndexError = 'List index buiten de toegestane grenzen (%d)';
  SListCapacityError = 'List capaciteit buiten de toegestane grenzen (%d)';
  SListCountError = 'List teller buiten de toegestane grenzen (%d)';
  SSortedListError = 'Bewerking niet toegestaan op gesorteerde string lists';
  SDuplicateString = 'String list accepteert geen duplikaten';
  SInvalidTabIndex = 'Tab index buiten de toegestane grenzen';
  SDuplicateName = 'Een component genaamd %s bestaat reeds';
  SInvalidName = '''''%s'''' is geen geldige component naam';
  SDuplicateClass = 'Een class genaamd %s bestaat reeds';
  SNoComSupport = '%s is niet geregistreerd als een COM class';
  SInvalidInteger = '''''%s'''' is geen geldige integer waarde';
  SLineTooLong = 'Regel te lang';
  SInvalidPropertyValue = 'Ongeldige property waarde';
  SInvalidPropertyPath = 'Ongeldig property pad';
  SUnknownProperty = 'Property bestaat niet';
  SReadOnlyProperty = 'Property is alleen lezen';
  SPropertyException = 'Fout bij het lezen van %s.%s: %s';
  SAncestorNotFound = 'Ancestor van ''%s'' niet gevonden';
  SInvalidBitmap = 'Bitmap image is ongeldig';
  SInvalidIcon = 'Icon image is ongeldig';
  SInvalidMetafile = 'Metafile is ongeldig';
  SInvalidPixelFormat = 'Ongeldig pixel formaat';
  SBitmapEmpty = 'Bitmap is leeg';
  SScanLine = 'Scan regel index buiten toegestane bereik';
  SChangeIconSize = 'Cannot change the size of an icon';
  SOleGraphic = 'Ongeldige bewerking op TOleGraphic';
  SUnknownExtension = 'Onbekend grafische bestandsextensie (.%s)';
  SUnknownClipboardFormat = 'Niet ondersteund klembord formaat';
  SOutOfResources = 'Systeembronnen ontoereikend';
  SNoCanvasHandle = 'Canvas niet geschikt voor tekenen';
  SInvalidImageSize = 'Ongeldige grootte voor plaatje';
  STooManyImages = 'Te veel plaatjes';
  SDimsDoNotMatch = 'Kenmerken van het plaatje komen niet overeen met die van de ImageList';
  SInvalidImageList = 'Ongeldige ImageList';
  SReplaceImage = 'Het was niet mogelijk het plaatje te vervangen';
  SImageIndexError = 'Ongeldige ImageList Index';
  SImageReadFail = 'Het was niet mogelijk de ImageList data van een stream te lezen';
  SImageWriteFail = 'Het was niet mogelijk de ImageList data naar een stream te schrijven';
  SWindowDCError = 'Fout bij het aanmaken van een window device context';
  SClientNotSet = 'Client van TDrag is niet geinitialiseerd';
  SWindowClass = 'Fout bij het aanmaken van de window class';
  SWindowCreate = 'Fout bij het aanmaken van het window';
  SCannotFocus = 'Kan de focus niet naar een onzichtbaar of uitgeschakeld window verplaatsen';
  SParentRequired = 'Control ''%s'' heeft geen parent window';
  SMDIChildNotVisible = 'Kan een MDI Child formulier niet verbergen';
  SVisibleChanged = 'Zichtbaarheid kan niet in OnShow or OnHide ingesteld worden';
  SCannotShowModal = 'Een zichtbaar scherm kan niet modaal gemaakt worden';
  SScrollBarRange = 'Scrollbar property buiten het toegestane bereik';
  SPropertyOutOfRange = '%s property buiten het toegestane bereik';
  SMenuIndexError = 'Menu index buiten het toegestane bereik';
  SMenuReinserted = 'Menu tweemaal ingevoegd';
  SMenuNotFound = 'Sub-menu is niet in het menu aanwezig';
  SNoTimers = 'Niet voldoende timers beschikbaar';
  SNotPrinting = 'Printer is op het moment niet aan het afdrukken';
  SPrinting = 'Afdrukken is bezig';
  SPrinterIndexError = 'Printer buiten het toegestane bereik';
  SInvalidPrinter = 'Geselecteerde printer is ongeldig';
  SDeviceOnPort = '%s op %s';
  SGroupIndexTooLow = 'GroupIndex kan niet minder zijn dan het vorige menu item''s GroupIndex';
  STwoMDIForms = 'Meer dan 1 MDI formulier per applicatie is niet mogelijk';
  SNoMDIForm = 'Formulier kan niet aangemaakt worden. Geen MDI formulieren actief';
  SRegisterError = 'Ongeldige component registratie';
  SImageCanvasNeedsBitmap = 'Een plaatje kan alleen aangepast worden als het een Bitmap bevat';
  SControlParentSetToSelf = 'Een control kan niet zijn eigen parent zijn';
  SOKButton = 'OK';
  SCancelButton = 'Annuleren';
  SYesButton = '&Ja';
  SNoButton = '&Nee';
  SHelpButton = '&Help';
  SCloseButton = '&Sluiten';
  SIgnoreButton = 'Ne&geren';
  SRetryButton = '&Opnieuw';
  SAbortButton = 'Afbreken';
  SAllButton = '&Alles';

  SCannotDragForm = 'Formulier kan niet gesleept worden';
  SPutObjectError = 'PutObject naar ongedefinieerd item';
  SCardDLLNotLoaded = 'CARDS.DLL kon niet geladen worden';
  SDuplicateCardId = 'Dubbel CardId gevonden';

  SDdeErr = 'Een foutmelding werd door DDE geretourneerd ($0%x)';
  SDdeConvErr = 'DDE fout - conversatie niet tot stand gekomen ($0%x)';
  SDdeMemErr = 'Foutmelding, DDE heeft niet voldoende geheugen ter beschikking ($0%x)';
  SDdeNoConnect = 'Het is niet mogelijk een DDE conversatie te starten';

  SFB = 'FB';
  SFG = 'FG';
  SBG = 'BG';
  SOldTShape = 'Kan niet een oudere versie van TShape laden';
  SVMetafiles = 'Metafiles';
  SVEnhMetafiles = 'Enhanced Metafiles';
  SVIcons = 'Iconen';
  SVBitmaps = 'Bitmaps';
  SGridTooLarge = 'Grid is te groot voor deze bewerking';
  STooManyDeleted = 'Teveel rijen of kolommen gedetecteerd';
  SIndexOutOfRange = 'Grid index buiten toegestane bereik';
  SFixedColTooBig = 'Het aantal vaste kolommen moet minder zijn dan het totaal aantal kolommen';
  SFixedRowTooBig = 'Het aantal vaste rijen moet minder zijn dan het totaal aantal rijen';
  SInvalidStringGridOp = 'Kan geen rijen toevoegen of wissen in de grid';
  SParseError = '%s op regel %d';
  SIdentifierExpected = 'Identifier verwacht';
  SStringExpected = 'String verwacht';
  SNumberExpected = 'Number verwacht';
  SCharExpected = '''''%s'''' verwacht';
  SSymbolExpected = '%s verwacht';
  SInvalidNumber = 'Ongeldige numerieke waarde';
  SInvalidString = 'Ongeldige string konstante';
  SInvalidProperty = 'Ongeldige property waarde';
  SInvalidBinary = 'Ongeldige binaire waarde';
  SOutlineIndexError = 'Outline index niet gevonden';
  SOutlineExpandError = 'Parent moet geexpandeerd zijn';
  SInvalidCurrentItem = 'Ongeldige waarde voor het huidige item';
  SMaskErr = 'Ongeldige invoer';
  SMaskEditErr = 'Ongeldige invoer.  Gebruik escape om de invoer ongedaan te maken';
  SOutlineError = 'Ongeldige outline index';
  SOutlineBadLevel = 'Incorrecte niveau toewijzing';
  SOutlineSelection = 'Ongeldige selectie';
  SOutlineFileLoad = 'Fout bij het laden van een bestand';
  SOutlineLongLine = 'Regel is te lang';
  SOutlineMaxLevels = 'Maximum diepte van de outline overschreden';

  SMsgDlgWarning = 'Waarschuwing';
  SMsgDlgError = 'Fout';
  SMsgDlgInformation = 'Informatie';
  SMsgDlgConfirm = 'Bevestigen';
  SMsgDlgYes = '&Ja';
  SMsgDlgNo = '&Nee';
  SMsgDlgOK = 'OK';
  SMsgDlgCancel = 'Annuleren';
  SMsgDlgHelp = '&Help';
  SMsgDlgHelpNone = 'Geen help beschikbaar';
  SMsgDlgHelpHelp = 'Help';
  SMsgDlgAbort = '&Afbreken';
  SMsgDlgRetry = '&Opnieuw';
  SMsgDlgIgnore = 'Ne&geren';
  SMsgDlgAll = '&Alles';
  SMsgDlgNoToAll = 'N&ee op Alles';
  SMsgDlgYesToAll = 'Ja& op Alles';

  SmkcBkSp = 'BkSp';
  SmkcTab = 'Tab';
  SmkcEsc = 'Esc';
  SmkcEnter = 'Enter';
  SmkcSpace = 'Space';
  SmkcPgUp = 'PgUp';
  SmkcPgDn = 'PgDn';
  SmkcEnd = 'End';
  SmkcHome = 'Home';
  SmkcLeft = 'Left';
  SmkcUp = 'Up';
  SmkcRight = 'Right';
  SmkcDown = 'Down';
  SmkcIns = 'Ins';
  SmkcDel = 'Del';
  SmkcShift = 'Shift+';
  SmkcCtrl = 'Ctrl+';
  SmkcAlt = 'Alt+';

  srUnknown = '(Onbekend)';
  srNone = '(Geen)';
  SOutOfRange = 'Waarde moet tussen %d en %d liggen';
  SCannotCreateName = 'Kan geen standaard methodenaam voor een component zonder naam maken';

  SDateEncodeError = 'Ongeldige parameter bij datum omzetting';
  STimeEncodeError = 'Ongeldige parameter bij tijd omzetting';
  SInvalidDate = '''''%s'''' is geen geldige datum';
  SInvalidTime = '''''%s'''' is geen geldige tijd';
  SInvalidDateTime = '''''%s'''' is geen geldige datum en tijd';
  SInvalidFileName = 'Ongeldige bestandsnaam - %s';
  SDefaultFilter = 'Alle bestanden (*.*)|*.*';
  sAllFilter = 'Alles';
  SNoVolumeLabel = ': [ - Geen schijf naam - ]';
  SInsertLineError = 'Onmogelijk een regel tussen te voegen';

  SConfirmCreateDir = 'De aangegeven directory bestaat niet. Aanmaken?';
  SSelectDirCap = 'Selecteer Directory';
  SCannotCreateDir = 'Onmogelijk om de directory aan te maken';
  SDirNameCap = 'Directory &Naam:';
  SDrivesCap = 'D&isks:';
  SDirsCap = '&Directory''s:';
  SFilesCap = '&Bestanden: (*.*)';
  SNetworkCap = 'Ne&twerk...';

  SColorPrefix = 'Kleur';
  SColorTags = 'ABCDEFGHIJKLMNOP';

  SInvalidClipFmt = 'Ongeldig klembord formaat';
  SIconToClipboard = 'Klembord ondersteunt geen iconen';

  SDefault = 'Standaard';

  SInvalidMemoSize = 'Tekst is langer dan de capaciteit van het memo';
  SCustomColors = 'Specifieke kleuren';
  SInvalidPrinterOp = 'Bewerking wordt niet ondersteund door de geselecteerde printer';
  SNoDefaultPrinter = 'Er is momenteel geen standaard printer gedefinieerd';

  SIniFileWriteError = 'Onmogelijk om naar %s te schrijven';

  SBitsIndexError = 'Bits index buiten toegestane bereik';

  SUntitled = '(Naamloos)';

  SInvalidRegType = 'Ongeldig data type voor ''%s''';
  SRegCreateFailed = 'Fout bij het aanmaken van key %s';
  SRegSetDataFailed = 'Fout bij het toewijzen van data aan ''%s''';
  SRegGetDataFailed = 'Fout bij het ophalen van data van ''%s''';

  SUnknownConversion = 'Onbekende RichEdit conversie bestandsextensie (.%s)';
  SDuplicateMenus = 'Menu ''%s'' wordt reeds gebruikt door een ander formulier';

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
