{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         ConstsRC - Spanish Consts messages            }
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
         SAssignError = 'No se puede asignar un %s a un %s';
         SFCreateError = 'No puedo crear el fichero %s';
         SFOpenError = 'No puedo abrir el fichero %s';
         SReadError = 'Error de lectura de flujo de datos';
         SWriteError = 'Error de escritura en flujo de datos';
         SMemoryStreamError = 'Memoria agotada al expadir flujo de datos en memoria';
         SCantWriteResourceStreamError = 'No se puede escribir en un flujo de datos de recursos de sóo-lectura';
         SDuplicateReference = 'WriteObject ejecutado dos veces para la misma instancia';
         SClassNotFound = 'Clase %s no encontrada';
         SInvalidImage = 'Formato de flujo de datos incorrecto';
  SResNotFound = 'Recurso %s no encontrado';
  SClassMismatch = 'El recurso %s pertenece a una clase incorrecta';
  SListIndexError = 'Indice de lista fuera de rango (%d)';
  SListCapacityError = 'Capacidad de lista fuera de rango (%d)';
  SListCountError = 'Contador de lista fuera de rango (%d)';
  SSortedListError = 'Operación no permitida en lista ordenada de cadenas';
  SDuplicateString = 'Lista de cadena no admite duplicados';
  SInvalidTabIndex = 'Indice de pestaña fuera de rango';
  SDuplicateName = 'Ya existe un componente de nombre %s';
  SInvalidName = '''''%s'''' no es un nombre correcto de componente';
         SDuplicateClass = 'Ya existe una clase de nombre %s';
  SNoComSupport = '%s no ha sido registrada como clase COM';
  SInvalidInteger = '''''%s'''' no es un valor entero válido';
         SLineTooLong = 'Línea demasiado larga';
  SInvalidPropertyValue = 'Valor de propiedad incorrecto';
  SInvalidPropertyPath = 'Ruta de propiedad incorrecta';
  SUnknownProperty = 'La propiedad no existe';
  SReadOnlyProperty = 'La propiedad es de sólo-lectura';
  SPropertyException = 'Error leyendo %s.%s: %s';
  SAncestorNotFound = 'El antecesor de ''%s'' no ha sido encontrado';
  SInvalidBitmap = 'El mapa de bits no es válido';
  SInvalidIcon = 'El icono no es válido';
  SInvalidMetafile = 'EL metafichero no es válido';
  SInvalidPixelFormat = 'Formato de píxel incorrecto';
  SBitmapEmpty = 'El mapa de bits está vacío';
  SScanLine = 'Indice de línea de exploración fuera de rango';
  SChangeIconSize = 'No se puede cambiar el tamaño de un icono';
  SOleGraphic = 'Operación incorrecta sobre TOleGraphic';
  SUnknownExtension = 'Extensión desconocida de fichero de imagen (.%s)';
  SUnknownClipboardFormat = 'Formato de Portapapeles no admitido';
  SOutOfResources = 'Recursos del sistema agotados';
  SNoCanvasHandle = 'El lienzo no permite dibujar';
  SInvalidImageSize = 'Tamaño de imagen incorrecto';
         STooManyImages = 'Demasiadas imágenes';
  SDimsDoNotMatch = 'Las dimensiones de la imagen no concuerdan con las dimensiones de la lista de imágenes';
  SInvalidImageList = 'Lista de imágenes no válida';
         SReplaceImage = 'Imposible reemplazar imagen';
  SImageIndexError = 'Indice de ImageList incorrecto';
  SImageReadFail = 'Fallo al leer datos de ImageList desde flujo de datos';
  SImageWriteFail = 'Fallo al escribir datos de  ImageList en flujo de datos';
  SWindowDCError = 'Error al crear contexto de dispositivo de ventana';
  SClientNotSet = 'Cliente de TDrag no inicializado';
  SWindowClass = 'Error al crear la clase de ventana';
  SWindowCreate = 'Error al crear ventana';
  SCannotFocus = 'No se puede enfocar una ventana invisible o deshabilitada';
  SParentRequired = 'El control ''%s'' no tiene ventana madre';
  SMDIChildNotVisible = 'No se puede esconder un formulario hijo MDI';
  SVisibleChanged = 'No se puede cambiar Visible dentro de OnShow y OnHide';
  SCannotShowModal = 'No se puede hacer modal a una ventana visible';
  SScrollBarRange = 'Propiedad de barra de desplazamiento fuera de rango';
  SPropertyOutOfRange = 'Propiedad %s fuera de rango';
  SMenuIndexError = 'Indice de menú fuera de rango';
  SMenuReinserted = 'Menú insertado dos veces';
  SMenuNotFound = 'El sub-menú no se encuentra en el menú';
  SNoTimers = 'No hay suficientes temporizadores disponibles';
  SNotPrinting = 'La impresora no está imprimiendo en este momento';
         SPrinting = 'Impresión en marcha';
  SPrinterIndexError = 'Indice de impresora fuera de rango';
  SInvalidPrinter = 'La impresora seleccionada es incorrecta';
         SDeviceOnPort = '%s en %s';
  SGroupIndexTooLow = 'GroupIndex no puede ser menor que el valor de GroupIndex de un comando de menú anterior';
  STwoMDIForms = 'No se puede tener más un formulario MDI por aplicación';
  SNoMDIForm = 'No se puede crear el formulario. No hay formularios MDI activos';
  SRegisterError = 'Operación de registro de componente incorrecta';
  SImageCanvasNeedsBitmap = 'Sólo se puede modificar una imagen si contiene un mapa de bits';
  SControlParentSetToSelf = 'Un control no puede ser su propio padre';
  SOKButton = 'Aceptar';
  SCancelButton = 'Cancelar';
  SYesButton = '&Sí';
  SNoButton = '&No';
  SHelpButton = '&Ayuda';
  SCloseButton = '&Cerrar';
  SIgnoreButton = '&Ignorar';
  SRetryButton = '&Reintentar';
  SAbortButton = 'Abortar';
  SAllButton = '&Todo';

  SCannotDragForm = 'No se puede arrastrar un formulario';
  SPutObjectError = 'PutObject sobre elemento sin definir';
         SCardDLLNotLoaded = 'No se puede cargar CARDS.DLL';
  SDuplicateCardId = 'CardId duplicado';

         SDdeErr = 'Erro devuelto por DDE  ($0%x)';
  SDdeConvErr = 'Error DDE - conversación no establecida ($0%x)';
  SDdeMemErr = 'Ocurrió un error por agotarse la memoriade DDE ($0%x)';
  SDdeNoConnect = 'No se puede conectar con conversación DDE';

  SFB = 'FB';
  SFG = 'FG';
  SBG = 'BG';
  SOldTShape = 'No se puede cargar versión obsoleta de TShape';
  SVMetafiles = 'Metaficheros';
  SVEnhMetafiles = 'Metaficheros mejorados';
  SVIcons = 'Iconos';
  SVBitmaps = 'Mapas de bits';
  SGridTooLarge = 'Rejilla demasiado grande para la operación';
  STooManyDeleted = 'Demasiadas filas o columnas borradas';
  SIndexOutOfRange = 'Indice de rejilla fuera de rango';
  SFixedColTooBig = 'La cantidad de columnas fijas debe ser menor que el número de columnas';
  SFixedRowTooBig = 'La cantidad de filas fijas debe ser menor que el número de filas';
  SInvalidStringGridOp = 'No se puede borrar o insertar filas en la rejilla';
  SParseError = '%s en la línea %d';
         SIdentifierExpected = 'Identificador esperado';
  SStringExpected = 'Cadena esperada';
  SNumberExpected = 'Número esperado';
         SCharExpected = '''''%s'''' esperado';
  SSymbolExpected = '%s esperado';
  SInvalidNumber = 'Valor numérico incorrecto';
  SInvalidString = 'Constante de cadena incorrecta';
  SInvalidProperty = 'Valor de propiedad incorrecto';
  SInvalidBinary = 'Valor binario incorrecto';
  SOutlineIndexError = 'Indice de esquema no encontrado';
  SOutlineExpandError = 'El nodo padre debe ser expandido';
  SInvalidCurrentItem = 'Valor incorrecto para elemento actual';
  SMaskErr = 'Valor de entrada incorrecto';
  SMaskEditErr = 'Valor de entrada incorrecto. Utilice la tecla Escape para abandonar los cambios';
  SOutlineError = 'Indice de esquema incorrecto';
  SOutlineBadLevel = 'Asignación de nivel incorrecta';
  SOutlineSelection = 'Selección incorrecta';
  SOutlineFileLoad = 'Error de carga de fichero';
  SOutlineLongLine = 'Línea demasiado larga';
  SOutlineMaxLevels = 'Profundidad máxima de esquema excedida';

  SMsgDlgWarning = 'Advertencia';
  SMsgDlgError = 'Error';
         SMsgDlgInformation = 'Información';
  SMsgDlgConfirm = 'Confirmar';
  SMsgDlgYes = '&Sí';
         SMsgDlgNo = '&No';
  SMsgDlgOK = 'Aceptar';
  SMsgDlgCancel = 'Cancelar';
  SMsgDlgHelp = '&Ayuda';
  SMsgDlgHelpNone = 'No hay ayuda disponible';
  SMsgDlgHelpHelp = 'Ayuda';
  SMsgDlgAbort = '&Abortar';
  SMsgDlgRetry = '&Reintentar';
  SMsgDlgIgnore = '&Ignorar';
  SMsgDlgAll = '&Todo';
  SMsgDlgNoToAll = 'N&o a todo';
  SMsgDlgYesToAll = 'S&í a todo';

  SmkcBkSp = 'Retro';
  SmkcTab = 'Tab';
  SmkcEsc = 'Esc';
  SmkcEnter = 'Intro';
  SmkcSpace = 'Espacio';
  SmkcPgUp = 'RePg';
  SmkcPgDn = 'AvPg';
         SmkcEnd = 'Fin';
  SmkcHome = 'Inicio';
  SmkcLeft = 'Izq';
         SmkcUp = 'Arriba';
  SmkcRight = 'Der';
  SmkcDown = 'Abajo';
  SmkcIns = 'Ins';
  SmkcDel = 'Supr';
  SmkcShift = 'May+';
  SmkcCtrl = 'Ctrl+';
  SmkcAlt = 'Alt+';

  srUnknown = '(Desconocido)';
  srNone = '(Ninguno)';
  SOutOfRange = 'El valor debe estar entre %d y %d';
  SCannotCreateName = 'No se puede crear un nombre de método por omisión para un componente sin nombre';

  SDateEncodeError = 'Argumento incorrecto para codificar fecha';
  STimeEncodeError = 'Argumento incorrecto para codificar hora';
  SInvalidDate = '''''%s'''' no es una fecha correcta';
  SInvalidTime = '''''%s'''' no es una hora correcta';
  SInvalidDateTime = '''''%s'''' no es una fecha y hora correcta';
  SInvalidFileName = 'Nombre de fichero inválido - %s';
         SDefaultFilter = 'Todos los ficheros (*.*)|*.*';
  sAllFilter = 'Todo';
  SNoVolumeLabel = ': [ - sin etiqueta de volumen - ]';
         SInsertLineError = 'No se puede insertar una línea';

  SConfirmCreateDir = 'El directorio indicado no existe. ¿Desea crearlo?';
  SSelectDirCap = 'Seleccione Directorio';
  SCannotCreateDir = 'No se puede crear directorio';
  SDirNameCap = '&Nombre de directorio:';
  SDrivesCap = 'D&iscos:';
  SDirsCap = '&Directorios:';
  SFilesCap = '&Ficheros: (*.*)';
  SNetworkCap = '&Red...';

  SColorPrefix = 'Color';
  SColorTags = 'ABCDEFGHIJKLMNOP';

  SInvalidClipFmt = 'Formato incorrecto de Portapapeles';
  SIconToClipboard = 'El Portapapeles no admite iconos';

  SDefault = 'Por omisión';

  SInvalidMemoSize = 'El texto excede la capacidad del memo';
         SCustomColors = 'Colores Personalizados';
  SInvalidPrinterOp = 'Operación no permitida por la impresora seleccionada';
  SNoDefaultPrinter = 'No hay impresora por omisión seleccionada en este momento';

  SIniFileWriteError = 'No se puede escribir en %s';

  SBitsIndexError = 'Indice de bits fuera de rango';

  SUntitled = '(Sin título)';

  SInvalidRegType = 'Tipo de dato incorrecto para ''%s''';
  SRegCreateFailed = 'Fallo al crear la clave %s';
  SRegSetDataFailed = 'Fallo al establecer datos para ''%s''';
  SRegGetDataFailed = 'Fallo al recuperar datos de ''%s''';

  SUnknownConversion = 'Extensión desconocida de fichero de conversión para RichEdit (.%s)';
  SDuplicateMenus = 'El menú ''%s'' ya está en uso por otro formulario';

  SPictureLabel = 'Imagen:';
  SPictureDesc = ' (%dx%d)';
  SPreviewLabel = 'Previsualizar';

  SCannotOpenAVI = 'No se puede abrir AVI';

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
