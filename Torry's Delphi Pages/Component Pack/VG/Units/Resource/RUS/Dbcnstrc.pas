{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         DBCnstRC - Russian DBConsts messages          }
{                                                       }
{         Copyright (c) 1997, 1998                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit DBCnstRC;

interface
uses SysUtils, Forms, Dialogs;

procedure InitResStringsDBConsts;
procedure FreeResStringsDBConsts;

implementation
uses DBConsts, vgUtils;

{$IFDEF _D3_}

resourcestring
//  SDuplicateDatabaseName         = 'Дублированное имя базы данных ''%s''';
//  SDuplicateSessionName          = 'Дублированное имя сессии ''%s''';
//  SInvalidSessionName            = 'Неверное имя сессии %s';
//  SDatabaseNameMissing           = 'Не хватает имени базы данных';
//  SSessionNameMissing            = 'Отсутствует имя сессии';
//  SDatabaseOpen                  = 'Нельзя выполнить данную операцию на открытой базе данных';
//  SDatabaseClosed                = 'Нельзя выполнить данную операцию на закрытой базе данных';
//  SDatabaseHandleSet             = 'Handle принадлежит другой сессии';
  SDataSetOpen                   = 'Нельзя выполнить данную операцию на открытом наборе данных';
  SDataSetClosed                 = 'Нельзя выполнить данную операцию на закрытом наборе данных';
//  SSessionActive                 = 'Нельзя выполнить данную операцию при активной сессии';
//  SHandleError                   = 'Ошибка создания обработчика курсора';
//  SUnknownFieldType              = 'Поле ''%s'' незнакомого типа';
{$IFDEF _D4_}
  SFieldNotFound                 = 'Поле ''%s'' не найдено';
{$ELSE}
  SFieldNotFound                 = '%s: Поле ''%s'' не найдено';
{$ENDIF}
  SDataSetMissing                = 'Поля ''%s''  не связано с набором данных';
  SDataSetEmpty                  = 'Нельзя выполнить данную операцию на пустом наборе данных';
  SFieldTypeMismatch             = 'Поле ''%s'' не ожидаемого типа';
//  SInvalidFloatField             = 'Невозможно преобразовать поле ''%s'' в число с плавающей точкой';
//  SInvalidIntegerField           = 'Невозможно преобразовать поле ''%s'' в целое число';
  SFieldRangeError               = '%g неверное значение для поля ''%s''. Допустимый диапазон от %g до %g';
  SInvalidIntegerValue           = '''%s'' неверное целочисленное значение для поля ''%s''';
  SInvalidFloatValue             = '''%s'' неверное значение с плавающей точкой для поля ''%s''';
  SInvalidBoolValue              = '''%s'' неверное логическое значение для поля ''%s''';
  SNotEditing                    = 'Набор данных не находится в режиме вставки или правки';
//  STableMismatch                 = 'Исходная и выходная таблицы несовместимы';
  SDataSetReadOnly               = 'Нельзя модифицировать таблицу "только для чтения"';
  SFieldReadOnly                 = 'Поле ''%s'' нельзя изменять';
  SNotIndexField                 = 'Поле ''%s'' не индексировано и не может быть изменено';
  SFieldRequired                 = 'Поле ''%s'' должно иметь значение';
  SFieldAccessError              = 'Нельзя обратиться к полю ''%s'' как к типу %s';
//  SFieldAssignError              = 'Поля ''%s'' и ''%s'' не совместимы по присвоению';
  SFieldValueError               = 'Недопустимое значение для поля ''%s''';
//  SFieldUndefinedType            = 'Поле ''%s'' незнакомого типа';
//  SFieldUnsupportedType          = 'Поле ''%s'' неподдерживаемого типа';
  SInvalidCalcType               = 'Поле ''%s'' не может быть вычисляемым';
  SInvalidFieldSize              = 'Недопустимый размер поля';
  SCircularDataLink              = 'Круговые связи не разрешены';
  SFieldIndexError               = 'Индекс поля вне диапазона';
//  SCompositeIndexError           = 'Cannot use array of Field values with Expression Indices';
  SNoFieldIndexes                = 'Нет активных индексов';
  SIndexFieldMissing             = 'Нет доступа к индексному полю ''%s''';
  SDuplicateFieldName            = 'Дублированное имя поля ''%s''';
  SDuplicateIndexName            = 'Дублированное имя индекса ''%s''';
  SFieldNameMissing              = 'Отсутствует имя поля';
  SNoIndexForFields              = 'У таблицы ''%s'' нет индекса по полю ''%s''';
  STextFalse                     = 'Ложь';
  STextTrue                      = 'Истина';
//  SInvalidBatchMove              = 'Неверный параметр пакетной операции';
//  SEmptySQLStatement             = 'Нет операторов подлежащих подготовке';
//  SNoParameterValue              = 'Не задано значение параметра ''%s''';
//  SNoParameterType               = 'Не задан тип параметра ''%s''';
//  SParameterNotFound             = 'Параметр ''%s'' не найден';
//  SParamAccessError              = 'Неверный тип при обращении к параметру ''%s'' ';
//  SLoginError                    = 'Невозможно установить связь с базой данных ''%s''';
//  SBeginTransError               = 'База данных %s уже имеет активную транзакцию';
//  SEndTransError                 = 'База данных %s не имеет активных транзакций';
//  SInitError                     = 'Ошибка при попытке инициализации Borland Database Engine ( $%.4x)';
//  SDatasetDesigner               = 'Fields &Editor...';
//  SDatabaseEditor                = 'Database &Editor...';
//  SExplore                       = 'E&xplore';
//  SDesignLoadFailed              = 'Нет возможности загрузить RPTSMITH.EXE';
//  SRunLoadFailed                 = 'Нет возможности загрузить RS_RUN.EXE';
//  SInvalidServer                 = 'Неверный тип сервера';
//  SReportVerb                    = 'Edit Report...';
//  SIncorrectVersion              = 'Компоненте нужно более новую версию ReportSmith';
//  SCannotGetVersionInfo          = 'Невозможно достать информацию о версии из %s';
//  SUnableToLoadAPIDLL            = 'Нет возможности загрузить ReportSmith DLL: %s';
//  SNoFile                        = 'Требуемый Reportsmith файл ''%s'' не существует';
  SFirstRecord                   = 'Первая запись';
  SPriorRecord                   = 'Предыдущая запись';
  SNextRecord                    = 'Следующая запись';
  SLastRecord                    = 'Последняя запись';
  SInsertRecord                  = 'Вставить запись';
  SDeleteRecord                  = 'Удалить запись';
  SEditRecord                    = 'Править запись';
  SPostEdit                      = 'Зафиксировать изменения';
  SCancelEdit                    = 'Отменить изменения';
  SRefreshRecord                 = 'Освежить данные';
  SDeleteRecordQuestion          = 'Удалить запись?';
  SDeleteMultipleRecordsQuestion = 'Удалить все выбранные записи?';
//  SReportFilter                  = 'Файлы отчетов (*.rpt)|*.rpt';
//  SInvalidFieldRegistration      = 'Неверная регистрация поля';
//  SLinkDesigner                  = 'Поле ''%s'', из списка Detail Fields, должно быть привязано';
//  SLinkDetail                    = 'Таблица ''%s'' не может быть открыта';
//  SLinkMasterSource              = 'Свойство MasterSource у ''%s'' должно быть привязано к DataSource';
//  SLinkMaster                    = 'Нет возможности открыть MasterSource таблицу';
  STooManyColumns                = 'Grid получил запрос на отображение более чем 256 столбцов';
//  SIDAPILangID                   = '0009';
//  SDisconnectDatabase            = 'База данных сейчас подсоединена. Отсоединить и продолжить?';
//  SBDEError                      = 'Ошибка BDE $%.4x';
//  SLookupSourceError             = 'Нельзя использовать дублированные DataSource и LookupSource';
//  SLookupTableError              = 'LookupSource должен быть подсоединен к компоненте TTable';
//  SLookupIndexError              = '%s должен быть активным индексом в таблице для поиска';
//  SParameterTypes                = ';Input;Output;Input/Output;Result';
//  SInvalidParamFieldType         = 'Должен быть выбран верный тип поля';
//  STruncationError               = 'Параметр ''%s'' усечен при выводе';
//  SInvalidVersion                = 'Нет возможности загрузить параметры связки';
//  SNoIndexFiles                  = '(Нет)';
//  SIndexDoesNotExist             = 'Индекс не существует. Индекс: %s';
//  SNoTableName                   = 'Не хватает свойства TableName';
//  SBatchExecute                  = 'Execute';
//  SNoCachedUpdates               = 'Не в режиме cached update';
//  SNoOldValueUpdate              = 'Нет возможности изменить старое значение';
  SLookupInfoError               = 'Lookup-информация для поля ''%s''  недостаточна';
//  SInvalidAliasName              = 'Неверное имя псевдонима %s';
  SDataSourceChange              = 'DataSource не может быть изменен';
  SExprTermination               = 'Неправильно завершено выражение фильтра';
  SExprNameError                 = 'Незавершено имя поля';
  SExprStringError               = 'Незавершена строковая константа';
  SExprInvalidChar               = 'Неправильный символ в выражении фильтра: ''%s''';
  SExprNoRParen                  = ''')'' пропущено, но найдено %s';
  SExprExpected                  = 'Выражение отсутствует, но найдено %s';
//  SExprBadCompare                = 'Оператору отношения требуются поле и константа';
  SExprBadField                  = 'Поле ''%s'' не может быть использовано в выражении фильтра';
  SExprBadNullTest               = 'Слово Null допустимо лишь совместно с "=" и "<>"';
  SExprRangeError                = 'Значение константы вне границ';
  SExprNotBoolean                = 'Поле ''%s'' не логического типа';
  SExprIncorrect                 = 'Неверно сформировано выражение фильтра';
  SExprNothing                   = 'nothing';
//  SNoFieldAccess                 = 'Нет доступа к полю ''%s'' при фильтрации';
  SNotReplicatable               = 'Элемент управления не может использоваться совместно с DBCtrlGrid';
  SPropDefByLookup               = 'Свойство уже определено lookup-полем';
  SDataSourceFixed               = 'Операция не допускается в DBCtrlGrid';
  SFieldOutOfRange               = 'Значение поля ''%s'' вне границ';
  SBCDOverflow                   = '(Переполнение)';
//  SUpdateWrongDB                 = 'Нет возможности корректировать, %s не владеет %s';
//  SUpdateFailed                  = 'Сбой при коррекции';
  SInvalidVarByteArray           = 'Invalid variant type or size';
//  SSQLGenSelect                  = 'Необходимо выбрать по крайней мере одно ключевое поле и одно изменяемое поле';
//  SSQLNotGenerated               = 'UpdateSQL- выражение не сгенерировано, все равно выйти?';
//  SSQLDataSetOpen                = 'Невозможно определить имена полей для %s';
//  SOldNewNonData                 = 'Поле %s - это не поле данных';  SLocalTransDirty               = 'The transaction isolation level must be dirty read for local databases';
//  SMemoTooLarge                  = '(Memo too large)';

{$ENDIF}

procedure InitResStringsDBConsts;
begin
{$IFDEF _D3_}
  CopyResString(@SDataSetOpen                   , @DBConsts.SDataSetOpen                   , True);
  CopyResString(@SDataSetClosed                 , @DBConsts.SDataSetClosed                 , True);
  CopyResString(@SUnknownFieldType              , @DBConsts.SUnknownFieldType              , True);
  CopyResString(@SFieldNotFound                 , @DBConsts.SFieldNotFound                 , True);
  CopyResString(@SDataSetMissing                , @DBConsts.SDataSetMissing                , True);
  CopyResString(@SDataSetEmpty                  , @DBConsts.SDataSetEmpty                  , True);
  CopyResString(@SFieldTypeMismatch             , @DBConsts.SFieldTypeMismatch             , True);
  CopyResString(@SFieldRangeError               , @DBConsts.SFieldRangeError               , True);
  CopyResString(@SInvalidIntegerValue           , @DBConsts.SInvalidIntegerValue           , True);
  CopyResString(@SInvalidFloatValue             , @DBConsts.SInvalidFloatValue             , True);
  CopyResString(@SInvalidBoolValue              , @DBConsts.SInvalidBoolValue              , True);
  CopyResString(@SNotEditing                    , @DBConsts.SNotEditing                    , True);
  CopyResString(@SDataSetReadOnly               , @DBConsts.SDataSetReadOnly               , True);
  CopyResString(@SFieldReadOnly                 , @DBConsts.SFieldReadOnly                 , True);
  CopyResString(@SNotIndexField                 , @DBConsts.SNotIndexField                 , True);
  CopyResString(@SFieldRequired                 , @DBConsts.SFieldRequired                 , True);
  CopyResString(@SFieldAccessError              , @DBConsts.SFieldAccessError              , True);
  CopyResString(@SFieldValueError               , @DBConsts.SFieldValueError               , True);
  CopyResString(@SInvalidCalcType               , @DBConsts.SInvalidCalcType               , True);
  CopyResString(@SInvalidFieldSize              , @DBConsts.SInvalidFieldSize              , True);
  CopyResString(@SCircularDataLink              , @DBConsts.SCircularDataLink              , True);
  CopyResString(@SFieldIndexError               , @DBConsts.SFieldIndexError               , True);
  CopyResString(@SNoFieldIndexes                , @DBConsts.SNoFieldIndexes                , True);
  CopyResString(@SIndexFieldMissing             , @DBConsts.SIndexFieldMissing             , True);
  CopyResString(@SDuplicateFieldName            , @DBConsts.SDuplicateFieldName            , True);
  CopyResString(@SDuplicateIndexName            , @DBConsts.SDuplicateIndexName            , True);
  CopyResString(@SFieldNameMissing              , @DBConsts.SFieldNameMissing              , True);
  CopyResString(@SNoIndexForFields              , @DBConsts.SNoIndexForFields              , True);
  CopyResString(@STextFalse                     , @DBConsts.STextFalse                     , True);
  CopyResString(@STextTrue                      , @DBConsts.STextTrue                      , True);
  CopyResString(@SFirstRecord                   , @DBConsts.SFirstRecord                   , True);
  CopyResString(@SPriorRecord                   , @DBConsts.SPriorRecord                   , True);
  CopyResString(@SNextRecord                    , @DBConsts.SNextRecord                    , True);
  CopyResString(@SLastRecord                    , @DBConsts.SLastRecord                    , True);
  CopyResString(@SInsertRecord                  , @DBConsts.SInsertRecord                  , True);
  CopyResString(@SDeleteRecord                  , @DBConsts.SDeleteRecord                  , True);
  CopyResString(@SEditRecord                    , @DBConsts.SEditRecord                    , True);
  CopyResString(@SPostEdit                      , @DBConsts.SPostEdit                      , True);
  CopyResString(@SCancelEdit                    , @DBConsts.SCancelEdit                    , True);
  CopyResString(@SRefreshRecord                 , @DBConsts.SRefreshRecord                 , True);
  CopyResString(@SDeleteRecordQuestion          , @DBConsts.SDeleteRecordQuestion          , True);
  CopyResString(@SDeleteMultipleRecordsQuestion , @DBConsts.SDeleteMultipleRecordsQuestion , True);
  CopyResString(@STooManyColumns                , @DBConsts.STooManyColumns                , True);
  CopyResString(@SLookupInfoError               , @DBConsts.SLookupInfoError               , True);
  CopyResString(@SDataSourceChange              , @DBConsts.SDataSourceChange              , True);
  CopyResString(@SExprTermination               , @DBConsts.SExprTermination               , True);
  CopyResString(@SExprNameError                 , @DBConsts.SExprNameError                 , True);
  CopyResString(@SExprStringError               , @DBConsts.SExprStringError               , True);
  CopyResString(@SExprInvalidChar               , @DBConsts.SExprInvalidChar               , True);
  CopyResString(@SExprNoRParen                  , @DBConsts.SExprNoRParen                  , True);
  CopyResString(@SExprExpected                  , @DBConsts.SExprExpected                  , True);
  CopyResString(@SExprBadField                  , @DBConsts.SExprBadField                  , True);
  CopyResString(@SExprBadNullTest               , @DBConsts.SExprBadNullTest               , True);
  CopyResString(@SExprRangeError                , @DBConsts.SExprRangeError                , True);
  CopyResString(@SExprNotBoolean                , @DBConsts.SExprNotBoolean                , True);
  CopyResString(@SExprIncorrect                 , @DBConsts.SExprIncorrect                 , True);
  CopyResString(@SExprNothing                   , @DBConsts.SExprNothing                   , True);
  CopyResString(@SNotReplicatable               , @DBConsts.SNotReplicatable               , True);
  CopyResString(@SPropDefByLookup               , @DBConsts.SPropDefByLookup               , True);
  CopyResString(@SDataSourceFixed               , @DBConsts.SDataSourceFixed               , True);
  CopyResString(@SFieldOutOfRange               , @DBConsts.SFieldOutOfRange               , True);
  CopyResString(@SBCDOverflow                   , @DBConsts.SBCDOverflow                   , True);
  CopyResString(@SInvalidVarByteArray           , @DBConsts.SInvalidVarByteArray           , True);
{$ENDIF}
end;

procedure FreeResStringsDBConsts;
begin
{$IFDEF _D3_}
  RestoreResString(@DBConsts.SDataSetOpen                   );
  RestoreResString(@DBConsts.SDataSetClosed                 );
  RestoreResString(@DBConsts.SUnknownFieldType              );
  RestoreResString(@DBConsts.SFieldNotFound                 );
  RestoreResString(@DBConsts.SDataSetMissing                );
  RestoreResString(@DBConsts.SDataSetEmpty                  );
  RestoreResString(@DBConsts.SFieldTypeMismatch             );
  RestoreResString(@DBConsts.SFieldRangeError               );
  RestoreResString(@DBConsts.SInvalidIntegerValue           );
  RestoreResString(@DBConsts.SInvalidFloatValue             );
  RestoreResString(@DBConsts.SInvalidBoolValue              );
  RestoreResString(@DBConsts.SNotEditing                    );
  RestoreResString(@DBConsts.SDataSetReadOnly               );
  RestoreResString(@DBConsts.SFieldReadOnly                 );
  RestoreResString(@DBConsts.SNotIndexField                 );
  RestoreResString(@DBConsts.SFieldRequired                 );
  RestoreResString(@DBConsts.SFieldAccessError              );
  RestoreResString(@DBConsts.SFieldValueError               );
  RestoreResString(@DBConsts.SInvalidCalcType               );
  RestoreResString(@DBConsts.SInvalidFieldSize              );
  RestoreResString(@DBConsts.SCircularDataLink              );
  RestoreResString(@DBConsts.SFieldIndexError               );
  RestoreResString(@DBConsts.SNoFieldIndexes                );
  RestoreResString(@DBConsts.SIndexFieldMissing             );
  RestoreResString(@DBConsts.SDuplicateFieldName            );
  RestoreResString(@DBConsts.SDuplicateIndexName            );
  RestoreResString(@DBConsts.SFieldNameMissing              );
  RestoreResString(@DBConsts.SNoIndexForFields              );
  RestoreResString(@DBConsts.STextFalse                     );
  RestoreResString(@DBConsts.STextTrue                      );
  RestoreResString(@DBConsts.SFirstRecord                   );
  RestoreResString(@DBConsts.SPriorRecord                   );
  RestoreResString(@DBConsts.SNextRecord                    );
  RestoreResString(@DBConsts.SLastRecord                    );
  RestoreResString(@DBConsts.SInsertRecord                  );
  RestoreResString(@DBConsts.SDeleteRecord                  );
  RestoreResString(@DBConsts.SEditRecord                    );
  RestoreResString(@DBConsts.SPostEdit                      );
  RestoreResString(@DBConsts.SCancelEdit                    );
  RestoreResString(@DBConsts.SRefreshRecord                 );
  RestoreResString(@DBConsts.SDeleteRecordQuestion          );
  RestoreResString(@DBConsts.SDeleteMultipleRecordsQuestion );
  RestoreResString(@DBConsts.STooManyColumns                );
  RestoreResString(@DBConsts.SLookupInfoError               );
  RestoreResString(@DBConsts.SDataSourceChange              );
  RestoreResString(@DBConsts.SExprTermination               );
  RestoreResString(@DBConsts.SExprNameError                 );
  RestoreResString(@DBConsts.SExprStringError               );
  RestoreResString(@DBConsts.SExprInvalidChar               );
  RestoreResString(@DBConsts.SExprNoRParen                  );
  RestoreResString(@DBConsts.SExprExpected                  );
  RestoreResString(@DBConsts.SExprBadField                  );
  RestoreResString(@DBConsts.SExprBadNullTest               );
  RestoreResString(@DBConsts.SExprRangeError                );
  RestoreResString(@DBConsts.SExprNotBoolean                );
  RestoreResString(@DBConsts.SExprIncorrect                 );
  RestoreResString(@DBConsts.SExprNothing                   );
  RestoreResString(@DBConsts.SNotReplicatable               );
  RestoreResString(@DBConsts.SPropDefByLookup               );
  RestoreResString(@DBConsts.SDataSourceFixed               );
  RestoreResString(@DBConsts.SFieldOutOfRange               );
  RestoreResString(@DBConsts.SBCDOverflow                   );
  RestoreResString(@DBConsts.SInvalidVarByteArray           );
{$ENDIF}
end;

{$IFDEF _D3_}
initialization

finalization
  FreeResStringsDBConsts;
{$ENDIF}
end.
