{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         DBCnstRC - Spanish DBConsts messages          }
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
         SDataSetOpen                   = 'Operaciуn no aplicable a un conjunto de datos abierto';
         SDataSetClosed                 = 'Operaciуn no aplicable a un conjunto de datos cerrado';
//  SSessionActive                 = 'Нельзя выполнить данную операцию при активной сессии';
//  SHandleError                   = 'Ошибка создания обработчика курсора';
//  SUnknownFieldType              = 'Поле ''%s'' незнакомого типа';
{$IFDEF _D4_}
         SFieldNotFound                 = 'Campo ''%s'' no encontrado';
{$ELSE}
         SFieldNotFound                 = '%s: Campo ''%s'' no encontrado';
{$ENDIF}
         SDataSetMissing                = 'El campo ''%s'' no tiene conjunto de datos';
         SDataSetEmpty                  = 'Operaciуn no aplicable a un conjunto de datos vacнo';
         SFieldTypeMismatch             = 'El campo ''%s'' no es del tipo esperado';
//  SInvalidFloatField             = 'Невозможно преобразовать поле ''%s'' в число с плавающей точкой';
//  SInvalidIntegerField           = 'Невозможно преобразовать поле ''%s'' в целое число';
         SFieldRangeError               = '%g no es un valor correcto para el campo ''%s''. El rango admitido es desde %g hasta %g';
         SInvalidIntegerValue           =  '''%s'' no es un valor entero correcto para el campo ''%s''';
         SInvalidFloatValue             = '''%s'' no es un valor flotante correcto para el campo ''%s''';
         SInvalidBoolValue              =  '''%s'' no es un valor lуgico correcto para el campo ''%s''';
         SNotEditing                    = 'El conjunto de datos no esta en modo de ediciуn o inserciуn';
//  STableMismatch                 = 'Исходная и выходная таблицы несовместимы';
         SDataSetReadOnly               = 'No se puede modificar un conjunto de datos de sуlo lectura';
         SFieldReadOnly                 = 'El campo ''%s'' no puede ser modificado';
         SNotIndexField                 = 'El campo ''%s'' no estб indexado y no puede modificarse';
         SFieldRequired                 = 'El campo ''%s'' necesita un valor';
         SFieldAccessError              = 'No puedo convertir el valor del campo ''%s'' al tipo %s';
//  SFieldAssignError              = 'Поля ''%s'' и ''%s'' не совместимы по присвоению';
         SFieldValueError               = 'Valor incorrecto para el campo ''%s''';
//  SFieldUndefinedType            = 'Поле ''%s'' незнакомого типа';
//  SFieldUnsupportedType          = 'Поле ''%s'' неподдерживаемого типа';
         SInvalidCalcType               = 'El campo ''%s'' no puede ser calculado o de bъsqueda';
         SInvalidFieldSize              = 'Tamaсo invбlido de campo';
         SCircularDataLink              = 'Referencia circular en el enlace de datos';
         SFieldIndexError               = 'Indice de campo fuera de rango';
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
  SFirstRecord = 'Primer registro';
  SPriorRecord = 'Registro anterior';
  SNextRecord = 'Registro siguiente';
  SLastRecord = 'Ultimo registro';
  SInsertRecord = 'Insertar registro';
  SDeleteRecord = 'Eliminar registro';
         SEditRecord = 'Editar registro';
  SPostEdit = 'Grabar cambios';
  SCancelEdit = 'Cancelar cambios';
  SRefreshRecord = 'Actualizar datos';
  SDeleteRecordQuestion = 'їEliminar este registro?';
  SDeleteMultipleRecordsQuestion = 'їEliminar todos los registros seleccionados?';

//  SReportFilter                  = 'Файлы отчетов (*.rpt)|*.rpt';
//  SInvalidFieldRegistration      = 'Неверная регистрация поля';
//  SLinkDesigner                  = 'Поле ''%s'', из списка Detail Fields, должно быть привязано';
//  SLinkDetail                    = 'Таблица ''%s'' не может быть открыта';
//  SLinkMasterSource              = 'Свойство MasterSource у ''%s'' должно быть привязано к DataSource';
//  SLinkMaster                    = 'Нет возможности открыть MasterSource таблицу';
         STooManyColumns                =  'La rejilla intenta mostrar mбs de 256 columnas';
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
         SLookupInfoError               = 'La informaciуn de bъsqueda para el campo ''%s'' estб incompleta';
//  SInvalidAliasName              = 'Неверное имя псевдонима %s';
         SDataSourceChange              = 'DataSource no puede cambiarse';
         SExprTermination               = 'Expresiуn de filtro terminada incorrectamente';
         SExprNameError = 'Nombre de campo incompleto';
         SExprStringError = 'Constante de cadena sin terminar';
         SExprInvalidChar = 'Carбcter incorrecto en expresiуn de filtro: ''%s''';
         SExprNoRParen = ''')'' esperado, pero %s encontrado';
         SExprExpected = 'Expresiуn esperada, pero %s encontrado';
//  SExprBadCompare                = 'Оператору отношения требуются поле и константа';
         SExprBadField = 'El campo ''%s'' no puede utilizarse e una expresiуn de filtro';
         SExprBadNullTest = 'NULL solo se permite con ''='' y ''<>''';
         SExprRangeError = 'Constante fuera de rango';
         SExprNotBoolean = 'El campo ''%s'' no es de tipo Boolean';
         SExprIncorrect = 'Expresiуn de filtro incorrectamente formada';
         SExprNothing = 'nada';
//  SNoFieldAccess                 = 'Нет доступа к полю ''%s'' при фильтрации';
         SNotReplicatable               = 'El control no puede utilizarse en un DBCtrlGrid';
         SPropDefByLookup               = 'Propiedad ya definida por campo de bъsqueda';
         SDataSourceFixed               = 'Operaciуn no permitida en un DBCtrlGrid';
         SFieldOutOfRange               = 'El valor del campo ''%s'' estб fuera de rango';
         SBCDOverflow                   = '(Desbordamiento)';
//  SUpdateWrongDB                 = 'Нет возможности корректировать, %s не владеет %s';
//  SUpdateFailed                  = 'Сбой при коррекции';
         SInvalidVarByteArray           = 'Tipo o tamaсo del invariante invбlido';
//  SSQLGenSelect                  = 'Необходимо выбрать по крайней мере одно ключевое поле и одно изменяемое поле';
//  SSQLNotGenerated               = 'UpdateSQL- выражение не сгенерировано, все равно выйти?';
//  SSQLDataSetOpen                = 'Невозможно определить имена полей для %s';
//  SOldNewNonData                 = 'Поле %s - это не поле данных';  SLocalTransDirty               = 'The transaction isolation level must be dirty read for local databases';
//  SMemoTooLarge                  = '(Memo demasiado grande)';

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
