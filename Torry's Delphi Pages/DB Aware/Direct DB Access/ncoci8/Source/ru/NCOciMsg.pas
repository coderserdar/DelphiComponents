{*******************************************************}
{File:      NCOciBuff.PAS                               }
{Revision:  0.03.05 / 17.08.2000                        }
{Comment:   NC OCI8 VCL: string resources               }
{Copyright: (c) 1999, Dmitry Arefiev                    }
{Author:    Dmitry Arefiev, darefiev@da-soft.com        }
{*******************************************************}
{$I NCOciDef.inc}

unit NCOciMsg;

interface

resourcestring
    // NCOciWrapper
    msgOCINotInstalled = 'NOE1/INIT - OCI установлен не корректно';
    msgOCIBadVersion = 'NOE2/INIT - Плохая версия OCI [%s]. NCOCI8 требуется версия не ниже 8.0.3';
    msgOCINotLoaded = 'NOE3/INIT - Ошибка при загрузке OCI dll. %s';
    msgOCIOutOfCount = 'NOE4/VAR - Индекс находится вне диапазона массива';
    msgOCIBadValueSize = 'NOE5/VAR - Плохой или неопределенный размер значения переменной';
    msgOCIBadArrayLen = 'NOE6/VAR - Плохая или неопределенная переменная длинны массива';
    msgOCIBadValueType = 'NOE7/VAR - Плохая или неопределенная переменная [%s] тип значения %d';
    msgOCIUnsupValueType = 'NOE8/VAR - Неподдерживаемый тип данных';
    // NOE9/VAR
    msgOCIDataToLarge = 'NOE10/VAR - Значение слишком велико для переменной';
    msgOCIVarWithoutStmt = 'NOE11/VAR - Переменная требует подготовки для операции';
    msgOCIBadVarType = 'NOE12/VAR - Плохой или неопределенный тип параметра';
    msgOCIUndefPWVar = 'NOE13/STMT - Piece wiese operation for unknown variable';
    msgOCIBadTypePWVar = 'NOE14/STMT - Piece wiese operation for invalid variable';
    msgOCIFuncNotFound = 'NOE15/INIT - Точка входа [%s] не найдена в OCI DLL';
    msgOCINumInvVarArray = 'NOE16/NUM - Неправильный VARARRAY размер. Не могу получтиь число из варианта';
    msgOCINumInvVar = 'NOE17/NUM - Неправильный вариантный тип. Не могу получтиь число из варианта';
    msgOCITooLongGTRID = 'NOE18/TX - Максимальная длина (%d) GTRID превышает - %d';
    msgOCITooLongBQUAL = 'NOE19/TX - Максимальная длина (%d) BQUAL превышает - %d';
    msgOCITooLongTXName = 'NOE20/TX - Максимальная длина (%d) имени транзакции превышает - %d';

    // NCOciBuff
    msgOCIBuffMBOpen = 'NOE50/CRS - OCI курсор должен быть открыт';
    msgOCIBuffMBClose = 'NOE51/CRS - OCI курсор должен быть закрыт';
    msgOCIBadDT4CU = 'NOE52/CRS - Невозможно использовать кешированные изменения для OCI курсора с полем типа BLOB';
    msgOCIInvalidVarIndex = 'NOE53/CRS - Неправильно определено положение переменной';
    msgOCIInvalidRNO = 'NOE60/DEADCRS - Неправильный номер записи';
    msgOCIModifyNotDelphiBuff = 'NOE61/DEADCRS - TOCIDataSet буферная операция с не [rsDelphiBuff] буфером';
    msgOCICantModifyBuff = 'NOE62/DEADCRS - Невозможно удалить/изменить [rsEmpty, rsNew*Applyed, rsDelphiBuff] буфер';
    msgOCIInvalidBmk = 'NOE63/DEADCRS - Неправильная закладка';
    msgOCIApplyFailed = 'NOE64/DEADCRS - Сбой применения кешированых изменений';
    msgOCIRecurseApply = 'NOE65/DEADCRS - Нельзя повторно применить кешированные изменения';
    msgOCINoApplyCallback = 'NOE66/DEADCRS - Для применения изменений требуется обратный вызов';
    msgOCIMustBeBidir = 'NOE67/DEADCRS - OCI курсор должен быть двунаправленный';

    // NCOciParams
    msgOCIBadParamVal = 'NOE80/PAR - Невозможно назначить значение параметру';
    msgOCIComplexType = 'NOE81/PAR - Для сложного типа параметров используйте определенные методы';
    msgOCIMacroNotFound = 'NOE82/PAR - Макрос [%s] не найден';
    msgOCICantAssignByRef = 'NOE83/PAR - Невозможно присвоить значение параметру по ссылке';
    msgOCIParamIsNotHandle = 'NOE84/PAR - Значение паркметра не является хендлом';

    // NCOciDB
    msgOCIDBmbActive = 'NOE100/DB - База данных должна быть активной';
    msgOCIDBmbInactive = 'NOE101/DB - База данных должна быть неактивной';
    msgOCIDBLoginRetries = 'NOE102/DB - Невозможно соединиться с ORACLE после %d попыток';
    msgOCIDBUnknown = 'NOE103/DB - База данных [%s] неизвестна';
    msgOCIStmtCantDescribe = 'NOE104/Q - Невозможно получить описание SELECT списка для команды без набора данных';
    msgOCIDBTNManyClBraces = 'NOE105/DB - Слишком много закрывающих скобок в файле имен после алиаса [%s]';
    msgInvFetchPar = 'NOE107/FP - Неправильное значение fetch параметра [%s]';
    msgStmtExecFailed = 'NOE108/SDS - Возникла ошибка при исполнении команды';
    msgDataSetNotEditing = 'NOE110/DS - DataSet не в режиме редактирования';
    msgOCICachUpdMBAct = 'NOE111/DS - Кешированные изменения должны быть активными';
    msgOCIExpNotFnd = 'NOE112/DS - Найдено нескомпилированное выражение. Переоткройте набор данных.';
    msgOCIRecConstCompFail = 'NOE113/DS - Компиляция ограничения записи неуспешна. %s';
    msgOCIFieldConstCompFail = 'NOE114/DS - Компиляция ограничения поля неуспешна. %s';
    msgOCIFieldDefCompFail = 'NOE115/DS - Компиляция значения по умолчанию поля неуспешна. %s';
    msgOCIRecConstFail = 'NOE116/DS - Нарушено ограничение записи. %s';
    msgOCIFieldConstFail = 'NOE117/DS - Нарушено ограничение поля. %s';
    msgOCIFieldDefFail = 'NOE118/DS - Невозможно присвоить значение по умолчанию полю. %s';
    msgOCICantExecOper = 'NOE119/DS - Невозможно выполнить %s. Эта операция запрещена';
    msgOCIBlobReadOnly = 'NOE120/BLOB - BLOB поле [%s] только для чтения';
    msgOCILongStreamInvalid = 'NOE121/BLOB - TOCILongStream не может быть использовано для поля [%s]';
    msgOCILOBStreamInvalid = 'NOE122/BLOB - TOCILobStream не может быть использовано для поля [%s]';
    msgOCIFieldIsNotBLOB = 'NOE123/BLOB - Поле не BLOB field';
    msgOCIStmtCantOpen = 'NOE127/Q - Нельзя вызвать метод Open для объявления без rowset';
    msgOCIStmtCantExec = 'NOE128/Q - Нельзя вызвать метод ExecSQL для объявления с rowset';
    msgOCIKeyFieldsEmpty = 'NOE129/Q - Нельзя обновить rowset, неопределены ключевые поля';
    msgOCINotPLSQLObj = 'NOE130/SP - [%s] не является вызываемым PL/SQL объектом';
    msgOCIPLSQLObjNameEmpty = 'NOE131/SP - Имя PL/SQL объекта пустое';
    msgOCIPLSQLNotRemote = 'NOE132/SP - Удаленные PL/SQL объекты не поддерживаються (Используйте TOCIQuery)';
    msgOCIPLSQLNameError = 'NOE133/SP - Cannot parse PL/SQL objects name';
    msgOCINotPackageProc = 'NOE134/SP - [%s] не процедура пакета [%s]';
    msgOCIBadTableType = 'NOE135/SP - Параметр с типом TABLE OF BOOLEAN/RECORD не поддерживается (Используйте TOCIQuery)';
    msgOCITransMBInAct = 'NOE136/TX - Транзакция должна быть неактивной';
    msgOCIDBNameMismatch = 'NOE137/TX - DatabaseName несоответствует - [%s] и [%s]';
    msgOCITransMBStarted = 'NOE138/TX - Для вызова Suspend транзакция должна быть запущена явно';
    msgOCICannotChangeTM = 'NOE139/TX - Нельзя подключить другие TM потому, что имеется текущая активная транзакция';
    msgOCICantChProp = 'NOE140/UPD - Невозможно изменить свойство когда имеются кэшированные изменения';
    msgOCIParentDSRequired = 'NOE141/NDS - Требуется корректная информация о родительском наборе данных';
    msgOCIUnNamedRecParam = 'NOE142/SP - Параметр с типом RECORD должен быть именовонного типа (или используйте TOCIQuery)';
    SOCIOperations = 'Блокировка;Разблокировка;Обновление;Вставка;Удаление;Освежение';

    // NCOciFilter
    msgOCIFilterNotFound = 'NOE150/FLT - Фильтр [%s] не найден';
{$IFNDEF OCI_D4}
    SExprNoLParen = '''('' ожидается, но %s найдено';
    SExprNoRParenOrComma = ''')'' или '','' ожидается, но %s найдено';
    SExprTypeMis = 'Несогласование типа в выражении';
    SExprBadScope = 'Операция не может смешивать аггрегативные значения с значениями зависящими от записи';
    SExprNoArith = 'Арифметика в выражении фильтра не поддерживается';
    SExprNotAgg = 'Выражение не аггрегативное';
    SExprNoAggFilter = 'Аггрегативные выражения не допускаются в фильтре';
    SExprEmptyInList = 'Список предикатов IN ен может быть пустым';
    SInvalidKeywordUse = 'Некорректное использование ключевого слова';
{$ENDIF}    

    // NCOciReg
    SOCILoginCategoryName = 'OCI Логин';
    SOCILoginCategoryDesc = 'OCI Логин свойства и/или события';
    SOCIResourceCategoryName = 'OCI Ресурс';
    SOCIResourceCategoryDesc = 'OCI Ресурс свойства и/или события';
    SOCITransactCategoryName = 'OCI Транзакция';
    SOCITransactCategoryDesc = 'OCI Транзакция свойства и/или события';

    // NCOciUpdateSQL
    msgOCICantGenQuery = 'NOE170/UPS - Нельзя сгенерировать команду %s для набора данных';
    msgOCIRecordDeleted = 'NOE171/UPS - Запись была удалена другим пользователем';
    msgOCIRecordChanged = 'NOE172/UPS - Запись была изменена другим пользователем';
    msgOCIRecordLocked = 'NOE173/UPS - Запись заблокирована другим пользователем';
    msgOCICantUseSQL4Refresh = 'NOE174/UPS - Нельзя использовать специфический SQL для обновления команды';

    // NCOciMove
    msgOCIDPColumnAccessError = '';
    msgOCIDPColumnNotFound = '';
    msgOCIDPTabNameError = '';
    msgOCIDPNotRemote = '';
    msgOCIDPMustBeUnprepared = '';

    // NCOciBDE
{*} msgOCIBDEMBInActive = 'NOE180/BDE BDE соединение должно быть не активным';

    // NCOciCompNamer
    SCNBadPropName = 'Некорректное имя свойства [%s]';
    SCNBadStripPref = 'Ошибка в StripPrefixs. Отсутствует [}]';
    SCNEnterText = 'Введите текст:';
    SCNBadVFormat = 'Пропущен аргумент для формата [V] в [%s]';
    SCNUnknownCommand = 'Встречен неизвестный символ формата в [%s]';
    SCNNilComp = 'Не задан компонент';
    SCNUnknownFormat = 'Для класса [%s] не задан формат';
    SCNCallExpertNow = '<Эксперт переименования ...>';
    SCNUseExpertForRename = 'Используйте эксперт для переименования нескольких компонетов';

implementation

end.

