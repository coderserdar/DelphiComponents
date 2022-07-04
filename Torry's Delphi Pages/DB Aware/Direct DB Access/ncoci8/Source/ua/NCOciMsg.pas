{*******************************************************}
{File:      NCOciBuff.PAS                               }
{Revision:  0.03.03 / 17.11.1999                        }
{Comment:   NC OCI8 VCL: string resources               }
{Copyright: (c) 1999, Dmitry Arefiev                    }
{Author:    Dmitry Arefiev, darefiev@da-soft.com        }
{*******************************************************}
{$I NCOciDef.inc}

unit NCOciMsgUA;

interface

resourcestring
    // NCOciWrapper
    msgOCINotInstalled = 'NOE1/INIT - OCI встановлено невірно';
    msgOCIBadVersion = 'NOE2/INIT - Невідповідна версія OCI [%s]. NCOCI8 необхідна версія не нижче ніж 8.0.3';
    msgOCINotLoaded = 'NOE3/INIT - Помилка при завантажені OCI dll. %s';
    msgOCIOutOfCount = 'NOE4/VAR - Индекс знаходиться за межами діапазону масиву';
    msgOCIBadValueSize = 'NOE5/VAR - Невідповідний або невизначений розмір значення змінной';
    msgOCIBadArrayLen = 'NOE6/VAR - Невідповідная або невизначеная змінна дліна массиву';
    msgOCIBadValueType = 'NOE7/VAR - Невідповідная або невизначена змінна [%s] тип значення %d';
    msgOCIUnsupValueType = 'NOE8/VAR - Несупроводжеємий тип даних';
    // NOE9/VAR
    msgOCIDataToLarge = 'NOE10/VAR - Значення дуже велике для змінной';
    msgOCIVarWithoutStmt = 'NOE11/VAR - Змінна потребує підготовки до операції';
    msgOCIBadVarType = 'NOE12/VAR - Невідповідний або невизначений тип параметру';
    msgOCIUndefPWVar = 'NOE13/STMT - Piece wiese operation for unknown variable';
    msgOCIBadTypePWVar = 'NOE14/STMT - Piece wiese operation for invalid variable';
    msgOCIFuncNotFound = 'NOE15/INIT - Крапку входу [%s] не знайдено в OCI DLL';
    msgOCINumInvVarArray = 'NOE16/NUM - Невірний VARARRAY розмір. Неможливо отримати число із варианта';
    msgOCINumInvVar = 'NOE17/NUM - Невірний вариантний тип. Неможливо отримати число із варианта';
    msgOCITooLongGTRID = 'NOE18/TX - Максимальна дліна (%d) GTRID перевищує - %d';
    msgOCITooLongBQUAL = 'NOE19/TX - Максимальна дліна (%d) BQUAL перевищує - %d';
    msgOCITooLongTXName = 'NOE20/TX - Максимальна дліна (%d) имені транзакції перевищує - %d';

    // NCOciBuff
    msgOCIBuffMBOpen = 'NOE50/CRS - OCI курсор повинен бути відкритим';
    msgOCIBuffMBClose = 'NOE51/CRS - OCI курсор повинен бути закрит';
{*} msgOCIBadDT4CU = 'NOE52/CRS - Cannot use cached updates for OCI cursor with BLOB define variables';
    msgOCIInvalidVarIndex = 'NOE53/CRS - Невiрно визначено знаходження змiнной';
    msgOCIInvalidRNO = 'NOE60/DEADCRS - Невірний номер запису';
    msgOCIModifyNotDelphiBuff = 'NOE61/DEADCRS - TOCIDataSet буферна операція с не [rsDelphiBuff] буфером';
    msgOCICantModifyBuff = 'NOE62/DEADCRS - Неможливо вилучити/змінити [rsEmpty, rsNew*Applyed, rsDelphiBuff] буфер';
    msgOCIInvalidBmk = 'NOE63/DEADCRS - Невірна закладка';
    msgOCIApplyFailed = 'NOE64/DEADCRS - Аварійне завершення застосування кешированих змін';
    msgOCIRecurseApply = 'NOE65/DEADCRS - Неможливе повторне застосування кешированих змін';
    msgOCINoApplyCallback = 'NOE66/DEADCRS - Застосування змін потребує зворотній виклик';
    msgOCIMustBeBidir = 'NOE67/DEADCRS - OCI курсор повинен бути двонаправленим';

    // NCOciParams
    msgOCIBadParamVal = 'NOE80/PAR - Неможливо призначити значення параметру';
    msgOCIComplexType = 'NOE81/PAR - Для складного типу параметрів використовуйте відповідні методи';
    msgOCIMacroNotFound = 'NOE82/PAR - Макрос [%s] не знайдено';
{*}    msgOCICantAssignByRef = 'NOE83/PAR - Can''t assign value to parameter by reference';
{*}    msgOCIParamIsNotHandle = 'NOE84/PAR - Param is not of handle type';

    // NCOciDB
    msgOCIDBmbActive = 'NOE100/DB - База данних повинна бути активною';
    msgOCIDBmbInactive = 'NOE101/DB - База данних повинна бути неактивною';
    msgOCIDBLoginRetries = 'NOE102/DB - Неможливо підключитися до ORACLE після %d спроб';
    msgOCIDBUnknown = 'NOE103/DB - База данних [%s] невизначена';
    msgOCIStmtCantDescribe = 'NOE104/Q - Can not describe select list for a statement without rowset';
{*} msgOCIDBTNManyClBraces = 'NOE105/DB - Too many close braces in names file after alias [%s]';
    msgInvFetchPar = 'NOE107/FP - Невірне значення fetch параметру [%s]';
{*} msgStmtExecFailed = 'NOE108/SDS - Statement execution failed';
    msgDataSetNotEditing = 'NOE110/DS - DataSet не є в режимі змінення';
    msgOCICachUpdMBAct = 'NOE111/DS - Кешировані зміни повинні бути активовані';
{*} msgOCIExpNotFnd = 'NOE112/DS - Found uncompiled expression. Reopen dataset';
{*} msgOCIRecConstCompFail = 'NOE113/DS - Compilation of record constraint failed. %s';
{*} msgOCIFieldConstCompFail = 'NOE114/DS - Compilation of field constraint failed. %s';
{*} msgOCIFieldDefCompFail = 'NOE115/DS - Compilation of field default value failed. %s';
{*} msgOCIRecConstFail = 'NOE116/DS - Record constraint failed. %s';
{*} msgOCIFieldConstFail = 'NOE117/DS - Field constraint failed. %s';
{*} msgOCIFieldDefFail = 'NOE118/DS - Field default value failed. %s';
{*} msgOCICantExecOper = 'NOE119/DS - Can not %s. It is disabled';
    msgOCIBlobReadOnly = 'NOE120/BLOB - BLOB поле [%s] тільки для читання';
    msgOCILongStreamInvalid = 'NOE121/BLOB - TOCILongStream не може бути використано для поля [%s]';
    msgOCILOBStreamInvalid = 'NOE122/BLOB - TOCILobStream не може бути використано для поля [%s]';
    msgOCIFieldIsNotBLOB = 'NOE123/BLOB - Поле не є BLOB field';
    msgOCIStmtCantOpen = 'NOE127/Q - Не можна визвати метод Open для об"яви без rowset';
    msgOCIStmtCantExec = 'NOE128/Q - Не можна визвати метод ExecSQL для об"яви с rowset';
    msgOCIKeyFieldsEmpty = 'NOE129/Q - Неможливо поновити rowset, невизначено ключеві поля';
    msgOCINotPLSQLObj = 'NOE130/SP - [%s] не є викликаємим PL/SQL об"ектом';
    msgOCIPLSQLObjNameEmpty = 'NOE131/SP - Им"я PL/SQL об"екту не визначено';
    msgOCIPLSQLNotRemote = 'NOE132/SP - Вилучені PL/SQL об"екты не підтримуються (Використовуйте TOCIQuery)';
    msgOCIPLSQLNameError = 'NOE133/SP - Cannot parse PL/SQL objects name';
    msgOCINotPackageProc = 'NOE134/SP - [%s] не є процедурою пакету [%s]';
    msgOCIBadTableType = 'NOE135/SP - Параметр с типом TABLE OF BOOLEAN/RECORD не ідтримується (Використовуйте TOCIQuery)';
    msgOCITransMBInAct = 'NOE136/TX - Транзакция повинна бути неактивной';
    msgOCIDBNameMismatch = 'NOE137/TX - DatabaseName невідповідає - [%s] та [%s]';
    msgOCITransMBStarted = 'NOE138/TX - Для виклика Suspend транзакция повинна бути запущена явно';
    msgOCICannotChangeTM = 'NOE139/TX - Неможливо підключити іньши TM тому, що є поточна активна транзакція';
{*}    msgOCICantChProp = 'NOE140/UPD - Can not change property, when updates pending';
{*}    msgOCIParentDSRequired = 'NOE141/NDS - Parent data set valid info required';
{*} msgOCIUnNamedRecParam = 'NOE142/SP - Parameter with type RECORD must be of named type (use TOCIQuery)';
{*} SOCIOperations = 'Lock;Unlock;Update;Insert;Delete;Refresh';

    // NCOciFilter
    msgOCIFilterNotFound = 'NOE150/FLT - Фільтр [%s] не знайдено';
{$IFNDEF OCI_D4}
    SExprNoLParen = '''('' expected but %s found';
    SExprNoRParenOrComma = ''')'' or '','' expected but %s found';
    SExprTypeMis = 'Type mismatch in expression';
    SExprBadScope = 'Operation cannot mix aggregate value with record-varying value';
    SExprNoArith = 'Arithmetic in filter expressions not supported';
    SExprNotAgg = 'Expression is not an aggregate expression';
    SExprNoAggFilter = 'Aggregate expressions not allowed in filters';
    SExprEmptyInList = 'IN predicate list may not be empty';
    SInvalidKeywordUse = 'Invalid use of keyword';
{$ENDIF}    

    // NCOciReg
    SOCILoginCategoryName = 'OCI Логін';
    SOCILoginCategoryDesc = 'OCI Логін свойства та/або собитія';
    SOCIResourceCategoryName = 'OCI Ресурс';
    SOCIResourceCategoryDesc = 'OCI Ресурс свойства та/або собитія';
    SOCITransactCategoryName = 'OCI Транзакція';
    SOCITransactCategoryDesc = 'OCI Транзакція свойства та/або собитія';

    // NCOciUpdateSQL
    msgOCICantGenQuery = 'NOE170/UPS - Неможливо сгенерувати команду %s для набора даних';
    msgOCIRecordDeleted = 'NOE171/UPS - Запис було вилучено іньшим користувачем';
    msgOCIRecordChanged = 'NOE172/UPS - Запис було змінено іньшим користувачем';
    msgOCIRecordLocked = 'NOE173/UPS - Запис заблоковано іньшим користувачем';
    msgOCICantUseSQL4Refresh = 'NOE174/UPS - Неможливо використовувати специфічний SQL для поновленя команди';

    // NCOciMove
    msgOCIDPColumnAccessError = '';
    msgOCIDPColumnNotFound = '';
    msgOCIDPTabNameError = '';
    msgOCIDPNotRemote = '';
    msgOCIDPMustBeUnprepared = '';

    // NCOciBDE
{*} msgOCIBDEMBInActive = 'NOE180/BDE BDE connection must be inactive';

    // NCOciCompNamer
{*} SCNBadPropName = 'Invalid property name [%s]';
{*} SCNBadStripPref = 'Error in StripPrefixs. Missing [}]';
{*} SCNEnterText = 'Enter text:';
{*} SCNBadVFormat = 'Missing argument for [V] format in [%s]';
{*} SCNUnknownCommand = 'Encountered unknown format item in [%s]';
{*} SCNNilComp = 'Undefined component';
{*} SCNUnknownFormat = 'For class [%s] format is missed';
{*} SCNCallExpertNow = '<Call expert now ...>';
{*} SCNUseExpertForRename = 'Use expert to rename multiple components';

implementation

end.

