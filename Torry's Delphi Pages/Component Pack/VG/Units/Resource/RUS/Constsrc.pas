{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         ConstsRC - Russian Consts messages            }
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
  SAssignError                  =  'Невозможно присвоить %s %s';
  SFCreateError                 =  'Невозможно создать файл %s';
  SFOpenError                   =  'Невозможно открыть файл %s';
  SReadError                    =  'Ошибка чтения из потока';
  SWriteError                   =  'Ошибка записи в поток';
  SMemoryStreamError            =  'Недостаточно памяти для расширения memory stream';
  SCantWriteResourceStreamError =  'Невозможно писать в resource stream, открытый для чтения';
  SDuplicateReference           =  'WriteObject вызван дважды для одного экземпляра';
  SClassNotFound                =  'Класс %s не найден';
  SInvalidImage                 =  'Неправильный формат потока';
  SResNotFound                  =  'Ресурс %s не найден';
  SClassMismatch                =  'Resource %s неправильного класса';
  SListIndexError               =  'Индекс списка вне границ';
  SSortedListError              =  'Операция недопустима для сортированных списков строк';
  SDuplicateString              =  'Список строк не допускает дубликатов';
  SInvalidTabIndex              =  'Индекс закладки вне границ';
  SDuplicateName                =  'Компонента с именем %s уже существует';
  SInvalidName                  =  '''%s'' - недопустимое имя для компоненты';
  SDuplicateClass               =  'Класс с именем %s уже существует';
  SInvalidInteger               =  '''%s'' не целочисленное значение';
  SLineTooLong                  =  'Слишком длинная строка';
  SInvalidPropertyValue         =  'Неверное значение свойства';
  SInvalidPropertyPath          =  'Неверный путь свойства';
  SUnknownProperty              =  'Свойство не существует';
  SReadOnlyProperty             =  'Свойство только для чтения';
  SPropertyException            =  'Ошибка при чтении %s.%s: %s';
  SAncestorNotFound             =  'Предок для ''%s'' не найден';
  SInvalidBitmap                =  'Неправильная картинка';
  SInvalidIcon                  =  'Неправильная иконка';
  SInvalidMetafile              =  'Неправильный метафайл';
  SBitmapEmpty                  =  'Картинка пуста';
  SChangeIconSize               =  'Нельзя изменить размер иконки';
  SUnknownExtension             =  'Неизвестное расширение для файла с изображением (.%s)';
  SUnknownClipboardFormat       =  'Неподдерживаемый формат буфера обмена';
  SOutOfResources               =  'Кончились системные ресурсы';
  SNoCanvasHandle               =  'Канвас не разрешает рисовать';
  SInvalidImageSize             =  'Неверный размер изображения';
  STooManyImages                =  'Слишком много изображений';
  SDimsDoNotMatch               =   'Размеры изображения не соответствуют размерам списка изображений';
  SInvalidImageList             =  'Неверный список изображений';
  SReplaceImage                 =  'Нельзя заменить изображение';
  SImageIndexError              =  'Неверный индекс списка изображений';
  SWindowDCError                =  'Ошибка создания Device Context для окна';
  SClientNotSet                 =  'Клиент TDrag не инициализирован';
  SWindowClass                  =  'Ошибка создания класса окна';
  SWindowCreate                 =  'Ошибка создания окна';
  SCannotFocus                  =  'Нельзя передать фокус запрещенному или невидимому окну';
  SParentRequired               =  'Элемент управления ''%s'' не имеет окна-родителя';
  SMDIChildNotVisible           =  'Невозможно скрыть дочернюю форму MDI';
  SVisibleChanged               =  'Невозможно изменить Visible в OnShow или OnHide';
  SCannotShowModal              =  'Нельзя сделать видимое окно модальным';
  SScrollBarRange               =  'Значение скроллинга вне границ';
  SPropertyOutOfRange           =  'Свойство %s вне границ';
  SMenuIndexError               =  'Индекс меню вне границ';
  SMenuReinserted               =  'Меню вставлено дважды';
  SMenuNotFound                 =  'Подменю нет в меню';
  SNoTimers                     =  'Нет доступных таймеров';
  SNotPrinting                  =  'Принтер сейчас не печатает';
  SPrinting                     =  'Идет печать';
  SPrinterIndexError            =  'Индекс принтера вне границ';
  SInvalidPrinter               =  'Выбранный принтер неверен';
  SDeviceOnPort                 =  '%s on %s';
  SGroupIndexTooLow             =  'GroupIndex не может быть меньше чем  GroupIndex элемента предыдущего меню';
  STwoMDIForms                  =  'Нельзя иметь более чем одну MDI форму на приложение';
  SNoMDIForm                    =  'Невозможно создать форму. Нет активной MDI формы';
  SRegisterError                =  'Неправильная регистрация компоненты';
  SImageCanvasNeedsBitmap       =  'Изображение можно модифицировать, только если оно содержит картинку';
  SControlParentSetToSelf       =  'Элемент управления не может быть сам себе родителем';
  SOKButton                     =  'OK';
  SCancelButton                 =  'Отмена';
  SYesButton                    =  '&Да';
  SNoButton                     =  '&Нет';
  SHelpButton                   =  '&Справка';
  SCloseButton                  =  '&Закрыть';
  SIgnoreButton                 =  '&Игнор.';
  SRetryButton                  =  '&Повторить';
  SAbortButton                  =  'Прервать';
  SAllButton                    =  '&Все';

  SCannotDragForm               =  'Невозможно переместить форму';
  SPutObjectError               =  'PutObject для неопределенного элемента';
  SCardDLLNotLoaded             =  'Невозможно загрузить CARDS.DLL';
  SDuplicateCardId              =  'Найден дубликат CardId';

  SDdeErr                       =  'DDE вернул ошибку ($0%x)';
  SDdeConvErr                   =  'Ошибка DDE - связь не установлена ($0%x)';
  SDdeMemErr                    =  'Произошла ошибка при нехватке памяти для DDE ($0%x)';
  SDdeNoConnect                 =  'Нет возможности установить DDE связь';

  SFB                           =  'ПЗ';
  SFG                           =  'ПП';
  SBG                           =  'ЗП';
  SOldTShape                    =  'Нельзя загружать старую версию TShape';
  SVMetafiles                   =  'Метафайлы';
  SVEnhMetafiles                =  'Расширенные метафайлы';
  SVIcons                       =  'Иконки';
  SVBitmaps                     =  'Картинки';
  SGridTooLarge                 =  'Таблица слишком большая для операции';
  STooManyDeleted               =  'Слишком много строк или столбцов удалено';
  SIndexOutOfRange              =  'Индекс таблицы вне границ';
  SFixedColTooBig               =  'Число фиксированных столбцов должно быть меньше чем число столбцов';
  SFixedRowTooBig               =  'Число фиксированных строк должно быть меньше чем число строк';
  SParseError                   =  '%s в строке %d';
  SIdentifierExpected           =  'Ожидался идентификатор';
  SStringExpected               =  'Ожидалась строка';
  SNumberExpected               =  'Ожидалось число';
  SCharExpected                 =  'Ожидалось''%s'' ';
  SSymbolExpected               =  'Ожидалось %s';
  SInvalidNumber                =  'Неверное число';
  SInvalidString                =  'Неверная строковая константа';
  SInvalidProperty              =  'Неверное значение свойства';
  SInvalidBinary                =  'Неверное бинарное значение';
  SOutlineIndexError            =  'Outline: Индекс не найден';
  SOutlineExpandError           =  'Родитель должен быть раскрыт';
  SInvalidCurrentItem           =  'Неверное значение для текущего элемента';
  SMaskErr                      =  'Введено неверное значение';
  SMaskEditErr                  =  'Введено неверное значение. Используйте Esc для отмены изменений';
  SOutlineError                 =  'Outline: неверный индекс';
  SOutlineBadLevel              =  'Неверное присвоение уровня';
  SOutlineSelection             =  'Неверное выделение';
  SOutlineFileLoad              =  'Ошбка загрузки файла';
  SOutlineLongLine              =  'Слишком длинная строка';
  SOutlineMaxLevels             =  'Outline: Достигнута максимальная вложенность';

  SMsgDlgWarning                =  'Предупреждение';
  SMsgDlgError                  =  'Ошибка';
  SMsgDlgInformation            =  'Информация';
  SMsgDlgConfirm                =  'Подтверждение';
  SMsgDlgYes                    =  '&Да';
  SMsgDlgNo                     =  '&Нет';
  SMsgDlgOK                     =  'OK';
  SMsgDlgCancel                 =  'Отмена';
  SMsgDlgHelp                   =  '&Справка';
  SMsgDlgHelpNone               =  'Справка не доступна';
  SMsgDlgHelpHelp               =  'Справка';
  SMsgDlgAbort                  =  'Прервать';
  SMsgDlgRetry                  =  '&Повторить';
  SMsgDlgIgnore                 =  '&Игнор.';
  SMsgDlgAll                    =  '&Все';

  SmkcBkSp                      =  'BkSp';
  SmkcTab                       =  'Tab';
  SmkcEsc                       =  'Esc';
  SmkcEnter                     =  'Enter';
  SmkcSpace                     =  'Space';
  SmkcPgUp                      =  'PgUp';
  SmkcPgDn                      =  'PgDn';
  SmkcEnd                       =  'End';
  SmkcHome                      =  'Home';
  SmkcLeft                      =  'Left';
  SmkcUp                        =  'Up';
  SmkcRight                     =  'Right';
  SmkcDown                      =  'Down';
  SmkcIns                       =  'Ins';
  SmkcDel                       =  'Del';
  SmkcShift                     =  'Shift+';
  SmkcCtrl                      =  'Ctrl+';
  SmkcAlt                       =  'Alt+';

  srUnknown                     =  '(Неизвестно)';
  srNone                        =  '(Нет)';
  SOutOfRange                   =  'Значение должно быть между %d и %d';
  SCannotCreateName             =  'Нельзя создать имя метода по умолчанию для компоненты без имени';

  SDateEncodeError              =  'Неверный аргумент для преобразования в дату';
  STimeEncodeError              =  'Неверный аргумент для преобразования во время';
  SInvalidDate                  =  '''%s'' - неправильная дата';
  SInvalidTime                  =  '''%s'' - непраильное время';
  SInvalidDateTime              =  '''%s'' - неправильные дата или время';
  SInvalidFileName              =  'Неправильное имя файла - %s';
  SDefaultFilter                =  'Все файлы (*.*)|*.*';
  sAllFilter                    =  'Все';
  SNoVolumeLabel                =  ': [ - Без метки тома - ]';
  SInsertLineError              =  'Невозможно вставить строку';

  SConfirmCreateDir             =  'Указанная папка не существует. Создать ?';
  SSelectDirCap                 =  'Выбор папки';
  SDirNameCap                   =  'Имя  &Папки:';
  SDrivesCap                    =  '&Диски:';
  SDirsCap                      =  '&Папки:';
  SFilesCap                     =  '&Файлы: (*.*)';
  SNetworkCap                   =  '&Сеть...';

  SColorPrefix                  =  'Цвет';
  SColorTags                    =  'ABCDEFGHIJKLMNOP';

  SInvalidClipFmt               =  'Неверный формат буфера обмена';
  SIconToClipboard              =  'Буфер обмена не поддерживает иконки';

  SDefault                      =  'Умолчание';

  SInvalidMemoSize              =  'Текст больше чем 32 кб';
  SCustomColors                 =  'Цвета пользователя';
  SInvalidPrinterOp             =  'Операция не поддерживается на выбранном принтере';
  SNoDefaultPrinter             =  'Принтер по умолчанию не выбран';

  SIniFileWriteError            =  'Нет возможности писать в %s';

  SBitsIndexError               =  'Битовый индекс вне границ';

  SUntitled                     =  '(Без имени)';

  SInvalidRegType               =  'Неверный тип данных для ''%s''';
  SRegCreateFailed              =  'Сбой при создании ключа для %s';
  SRegSetDataFailed             =  'Сбой при установке даты для ''%s''';
  SRegGetDataFailed             =  'Сбой при получении даты для ''%s''';

  SUnknownConversion            =  'RichEdit: неизвестен способ преобразования для расширения (.%s)';
  SDuplicateMenus               =  'Меню ''%s'' уже используется в другой форме';

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
