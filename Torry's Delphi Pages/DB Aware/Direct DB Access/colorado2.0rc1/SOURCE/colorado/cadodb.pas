unit cadodb;

{ ************************************************************************ }
{ Microsoft ActiveX Data Objects 2.5 Library                               }
{ Version:    2.5                                                          }
{ ************************************************************************ }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:      //
//   Type Libraries     : LIBID_xxxx                                    //
//   CoClasses          : CLASS_xxxx                                    //
//   DISPInterfaces     : DIID_xxxx                                     //
//   Non-DISP interfaces: IID_xxxx                                      //
// *********************************************************************//
const
  LIBID_ADODB: TGUID = '{00000205-0000-0010-8000-00AA006D2EA4}';
  IID_ICollection: TGUID = '{00000512-0000-0010-8000-00AA006D2EA4}';
  IID_IDynaCollection: TGUID = '{00000513-0000-0010-8000-00AA006D2EA4}';
  IID_IADO: TGUID = '{00000534-0000-0010-8000-00AA006D2EA4}';
  IID_IADOProperties: TGUID = '{00000504-0000-0010-8000-00AA006D2EA4}';
  IID_IADOProperty: TGUID = '{00000503-0000-0010-8000-00AA006D2EA4}';
  IID_IADOError: TGUID = '{00000500-0000-0010-8000-00AA006D2EA4}';
  IID_IADOErrors: TGUID = '{00000501-0000-0010-8000-00AA006D2EA4}';
  IID_IADOCommand15: TGUID = '{00000508-0000-0010-8000-00AA006D2EA4}';
  IID_IADOConnection15: TGUID = '{00000515-0000-0010-8000-00AA006D2EA4}';
  IID_IADOConnection: TGUID = '{00000550-0000-0010-8000-00AA006D2EA4}';
  IID_IADORecordset15: TGUID = '{0000050E-0000-0010-8000-00AA006D2EA4}';
  IID_IADORecordset20: TGUID = '{0000054F-0000-0010-8000-00AA006D2EA4}';
  IID_IADORecordset21: TGUID = '{00000555-0000-0010-8000-00AA006D2EA4}';
  IID_IADORecordset: TGUID = '{00000556-0000-0010-8000-00AA006D2EA4}';
  IID_IADOFields15: TGUID = '{00000506-0000-0010-8000-00AA006D2EA4}';
  IID_IADOFields20: TGUID = '{0000054D-0000-0010-8000-00AA006D2EA4}';
  IID_IADOFields: TGUID = '{00000564-0000-0010-8000-00AA006D2EA4}';
  IID_IADOField20: TGUID = '{0000054C-0000-0010-8000-00AA006D2EA4}';
  IID_IADOField: TGUID = '{00000569-0000-0010-8000-00AA006D2EA4}';
  IID_IADOParameter: TGUID = '{0000050C-0000-0010-8000-00AA006D2EA4}';
  IID_IADOParameters: TGUID = '{0000050D-0000-0010-8000-00AA006D2EA4}';
  IID_IADOCommand: TGUID = '{0000054E-0000-0010-8000-00AA006D2EA4}';
  IID_IADOConnectionEventsVt: TGUID = '{00000402-0000-0010-8000-00AA006D2EA4}';
  IID_IADORecordsetEventsVt: TGUID = '{00000403-0000-0010-8000-00AA006D2EA4}';
  IID_IDBAsynchNotifyVt: TGUID = '{0C733A96-2A1C-11CE-ADE5-00AA0044773D}';
  DIID_IADOConnectionEvents: TGUID = '{00000400-0000-0010-8000-00AA006D2EA4}';
  DIID_IADORecordsetEvents: TGUID = '{00000266-0000-0010-8000-00AA006D2EA4}';
  IID_IADOConnectionConstruction15: TGUID = '{00000516-0000-0010-8000-00AA006D2EA4}';
  IID_IADOConnectionConstruction: TGUID = '{00000551-0000-0010-8000-00AA006D2EA4}';
  CLASS_Connection: TGUID = '{00000514-0000-0010-8000-00AA006D2EA4}';
  IID_IADORecord: TGUID = '{00000562-0000-0010-8000-00AA006D2EA4}';
  CLASS_Record: TGUID = '{00000560-0000-0010-8000-00AA006D2EA4}';
  IID_IADORecFields: TGUID = '{00000563-0000-0010-8000-00AA006D2EA4}';
  IID_IADOStream: TGUID = '{00000565-0000-0010-8000-00AA006D2EA4}';
  CLASS_Stream: TGUID = '{00000566-0000-0010-8000-00AA006D2EA4}';
  IID_IADORecordConstruction: TGUID = '{00000567-0000-0010-8000-00AA006D2EA4}';
  IID_IADOStreamConstruction: TGUID = '{00000568-0000-0010-8000-00AA006D2EA4}';
  IID_IADOCommandConstruction: TGUID = '{00000517-0000-0010-8000-00AA006D2EA4}';
  CLASS_Command: TGUID = '{00000507-0000-0010-8000-00AA006D2EA4}';
  CLASS_Recordset: TGUID = '{00000535-0000-0010-8000-00AA006D2EA4}';
  IID_IADORecordsetConstruction: TGUID = '{00000283-0000-0010-8000-00AA006D2EA4}';
  IID_IADOField15: TGUID = '{00000505-0000-0010-8000-00AA006D2EA4}';
  CLASS_Parameter: TGUID = '{0000050B-0000-0010-8000-00AA006D2EA4}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                  //
// *********************************************************************//
// CursorTypeEnum constants
type
  CursorTypeEnum = TOleEnum;
const
  adOpenUnspecified = $FFFFFFFF;
  adOpenForwardOnly = $00000000;
  adOpenKeyset = $00000001;
  adOpenDynamic = $00000002;
  adOpenStatic = $00000003;

// CursorOptionEnum constants
type
  CursorOptionEnum = TOleEnum;
const
  adHoldRecords = $00000100;
  adMovePrevious = $00000200;
  adAddNew = $01000400;
  adDelete = $01000800;
  adUpdate = $01008000;
  adBookmark = $00002000;
  adApproxPosition = $00004000;
  adUpdateBatch = $00010000;
  adResync = $00020000;
  adNotify = $00040000;
  adFind = $00080000;
  adSeek = $00400000;
  adIndex = $00800000;

// LockTypeEnum constants
type
  LockTypeEnum = TOleEnum;
const
  adLockUnspecified = $FFFFFFFF;
  adLockReadOnly = $00000001;
  adLockPessimistic = $00000002;
  adLockOptimistic = $00000003;
  adLockBatchOptimistic = $00000004;

// ExecuteOptionEnum constants
type
  ExecuteOptionEnum = TOleEnum;
const
  adOptionUnspecified = $FFFFFFFF;
  adAsyncExecute = $00000010;
  adAsyncFetch = $00000020;
  adAsyncFetchNonBlocking = $00000040;
  adExecuteNoRecords = $00000080;

// ConnectOptionEnum constants
type
  ConnectOptionEnum = TOleEnum;
const
  adConnectUnspecified = $FFFFFFFF;
  adAsyncConnect = $00000010;

// ObjectStateEnum constants
type
  ObjectStateEnum = TOleEnum;
const
  adStateClosed = $00000000;
  adStateOpen = $00000001;
  adStateConnecting = $00000002;
  adStateExecuting = $00000004;
  adStateFetching = $00000008;

// CursorLocationEnum constants
type
  CursorLocationEnum = TOleEnum;
const
  adUseNone = $00000001;
  adUseServer = $00000002;
  adUseClient = $00000003;
  adUseClientBatch = $00000003;

// DataTypeEnum constants
type
  DataTypeEnum = TOleEnum;
const
  adEmpty = $00000000;
  adTinyInt = $00000010;
  adSmallInt = $00000002;
  adInteger = $00000003;
  adBigInt = $00000014;
  adUnsignedTinyInt = $00000011;
  adUnsignedSmallInt = $00000012;
  adUnsignedInt = $00000013;
  adUnsignedBigInt = $00000015;
  adSingle = $00000004;
  adDouble = $00000005;
  adCurrency = $00000006;
  adDecimal = $0000000E;
  adNumeric = $00000083;
  adBoolean = $0000000B;
  adError = $0000000A;
  adUserDefined = $00000084;
  adVariant = $0000000C;
  adIDispatch = $00000009;
  adIUnknown = $0000000D;
  adGUID = $00000048;
  adDate = $00000007;
  adDBDate = $00000085;
  adDBTime = $00000086;
  adDBTimeStamp = $00000087;
  adBSTR = $00000008;
  adChar = $00000081;
  adVarChar = $000000C8;
  adLongVarChar = $000000C9;
  adWChar = $00000082;
  adVarWChar = $000000CA;
  adLongVarWChar = $000000CB;
  adBinary = $00000080;
  adVarBinary = $000000CC;
  adLongVarBinary = $000000CD;
  adChapter = $00000088;
  adFileTime = $00000040;
  adPropVariant = $0000008A;
  adVarNumeric = $0000008B;
  adArray = $00002000;

// FieldAttributeEnum constants
type
  FieldAttributeEnum = TOleEnum;
const
  adFldUnspecified = $FFFFFFFF;
  adFldMayDefer = $00000002;
  adFldUpdatable = $00000004;
  adFldUnknownUpdatable = $00000008;
  adFldFixed = $00000010;
  adFldIsNullable = $00000020;
  adFldMayBeNull = $00000040;
  adFldLong = $00000080;
  adFldRowID = $00000100;
  adFldRowVersion = $00000200;
  adFldCacheDeferred = $00001000;
  adFldIsChapter = $00002000;
  adFldNegativeScale = $00004000;
  adFldKeyColumn = $00008000;
  adFldIsRowURL = $00010000;
  adFldIsDefaultStream = $00020000;
  adFldIsCollection = $00040000;

// EditModeEnum constants
type
  EditModeEnum = TOleEnum;
const
  adEditNone = $00000000;
  adEditInProgress = $00000001;
  adEditAdd = $00000002;
  adEditDelete = $00000004;

// RecordStatusEnum constants
type
  RecordStatusEnum = TOleEnum;
const
  adRecOK = $00000000;
  adRecNew = $00000001;
  adRecModified = $00000002;
  adRecDeleted = $00000004;
  adRecUnmodified = $00000008;
  adRecInvalid = $00000010;
  adRecMultipleChanges = $00000040;
  adRecPendingChanges = $00000080;
  adRecCanceled = $00000100;
  adRecCantRelease = $00000400;
  adRecConcurrencyViolation = $00000800;
  adRecIntegrityViolation = $00001000;
  adRecMaxChangesExceeded = $00002000;
  adRecObjectOpen = $00004000;
  adRecOutOfMemory = $00008000;
  adRecPermissionDenied = $00010000;
  adRecSchemaViolation = $00020000;
  adRecDBDeleted = $00040000;

// GetRowsOptionEnum constants
type
  GetRowsOptionEnum = TOleEnum;
const
  adGetRowsRest = $FFFFFFFF;

// PositionEnum constants
type
  PositionEnum = TOleEnum;
const
  adPosUnknown = $FFFFFFFF;
  adPosBOF = $FFFFFFFE;
  adPosEOF = $FFFFFFFD;

// BookmarkEnum constants
type
  BookmarkEnum = TOleEnum;
const
  adBookmarkCurrent = $00000000;
  adBookmarkFirst = $00000001;
  adBookmarkLast = $00000002;

// MarshalOptionsEnum constants
type
  MarshalOptionsEnum = TOleEnum;
const
  adMarshalAll = $00000000;
  adMarshalModifiedOnly = $00000001;

// AffectEnum constants
type
  AffectEnum = TOleEnum;
const
  adAffectCurrent = $00000001;
  adAffectGroup = $00000002;
  adAffectAll = $00000003;
  adAffectAllChapters = $00000004;

// ResyncEnum constants
type
  ResyncEnum = TOleEnum;
const
  adResyncUnderlyingValues = $00000001;
  adResyncAllValues = $00000002;

// CompareEnum constants
type
  CompareEnum = TOleEnum;
const
  adCompareLessThan = $00000000;
  adCompareEqual = $00000001;
  adCompareGreaterThan = $00000002;
  adCompareNotEqual = $00000003;
  adCompareNotComparable = $00000004;

// FilterGroupEnum constants
type
  FilterGroupEnum = TOleEnum;
const
  adFilterNone = $00000000;
  adFilterPendingRecords = $00000001;
  adFilterAffectedRecords = $00000002;
  adFilterFetchedRecords = $00000003;
  adFilterPredicate = $00000004;
  adFilterConflictingRecords = $00000005;

// SearchDirectionEnum constants
type
  SearchDirectionEnum = TOleEnum;
const
  adSearchForward = $00000001;
  adSearchBackward = $FFFFFFFF;

// PersistFormatEnum constants
type
  PersistFormatEnum = TOleEnum;
const
  adPersistADTG = $00000000;
  adPersistXML = $00000001;

// StringFormatEnum constants
type
  StringFormatEnum = TOleEnum;
const
  adClipString = $00000002;

// ConnectPromptEnum constants
type
  ConnectPromptEnum = TOleEnum;
const
  adPromptAlways = $00000001;
  adPromptComplete = $00000002;
  adPromptCompleteRequired = $00000003;
  adPromptNever = $00000004;

// ConnectModeEnum constants
type
  ConnectModeEnum = TOleEnum;
const
  adModeUnknown = $00000000;
  adModeRead = $00000001;
  adModeWrite = $00000002;
  adModeReadWrite = $00000003;
  adModeShareDenyRead = $00000004;
  adModeShareDenyWrite = $00000008;
  adModeShareExclusive = $0000000C;
  adModeShareDenyNone = $00000010;
  adModeRecursive = $00400000;

// RecordCreateOptionsEnum constants
type
  RecordCreateOptionsEnum = TOleEnum;
const
  adCreateCollection = $00002000;
  adCreateStructDoc = $80000000;
  adCreateNonCollection = $00000000;
  adOpenIfExists = $02000000;
  adCreateOverwrite = $04000000;
  adFailIfNotExists = $FFFFFFFF;

// RecordOpenOptionsEnum constants
type
  RecordOpenOptionsEnum = TOleEnum;
const
  adOpenRecordUnspecified = $FFFFFFFF;
  adOpenSource = $00800000;
  adOpenAsync = $00001000;
  adDelayFetchStream = $00004000;
  adDelayFetchFields = $00008000;

// IsolationLevelEnum constants
type
  IsolationLevelEnum = TOleEnum;
const
  adXactUnspecified = $FFFFFFFF;
  adXactChaos = $00000010;
  adXactReadUncommitted = $00000100;
  adXactBrowse = $00000100;
  adXactCursorStability = $00001000;
  adXactReadCommitted = $00001000;
  adXactRepeatableRead = $00010000;
  adXactSerializable = $00100000;
  adXactIsolated = $00100000;

// XactAttributeEnum constants
type
  XactAttributeEnum = TOleEnum;
const
  adXactCommitRetaining = $00020000;
  adXactAbortRetaining = $00040000;
  adXactAsyncPhaseOne = $00080000;
  adXactSyncPhaseOne = $00100000;

// PropertyAttributesEnum constants
type
  PropertyAttributesEnum = TOleEnum;
const
  adPropNotSupported = $00000000;
  adPropRequired = $00000001;
  adPropOptional = $00000002;
  adPropRead = $00000200;
  adPropWrite = $00000400;

// ErrorValueEnum constants
type
  ErrorValueEnum = TOleEnum;
const
  adErrProviderFailed = $00000BB8;
  adErrInvalidArgument = $00000BB9;
  adErrOpeningFile = $00000BBA;
  adErrReadFile = $00000BBB;
  adErrWriteFile = $00000BBC;
  adErrNoCurrentRecord = $00000BCD;
  adErrIllegalOperation = $00000C93;
  adErrCantChangeProvider = $00000C94;
  adErrInTransaction = $00000CAE;
  adErrFeatureNotAvailable = $00000CB3;
  adErrItemNotFound = $00000CC1;
  adErrObjectInCollection = $00000D27;
  adErrObjectNotSet = $00000D5C;
  adErrDataConversion = $00000D5D;
  adErrObjectClosed = $00000E78;
  adErrObjectOpen = $00000E79;
  adErrProviderNotFound = $00000E7A;
  adErrBoundToCommand = $00000E7B;
  adErrInvalidParamInfo = $00000E7C;
  adErrInvalidConnection = $00000E7D;
  adErrNotReentrant = $00000E7E;
  adErrStillExecuting = $00000E7F;
  adErrOperationCancelled = $00000E80;
  adErrStillConnecting = $00000E81;
  adErrInvalidTransaction = $00000E82;
  adErrNotExecuting = $00000E83;
  adErrUnsafeOperation = $00000E84;
  adwrnSecurityDialog = $00000E85;
  adwrnSecurityDialogHeader = $00000E86;
  adErrIntegrityViolation = $00000E87;
  adErrPermissionDenied = $00000E88;
  adErrDataOverflow = $00000E89;
  adErrSchemaViolation = $00000E8A;
  adErrSignMismatch = $00000E8B;
  adErrCantConvertvalue = $00000E8C;
  adErrCantCreate = $00000E8D;
  adErrColumnNotOnThisRow = $00000E8E;
  adErrURLDoesNotExist = $00000E8F;
  adErrTreePermissionDenied = $00000E90;
  adErrInvalidURL = $00000E91;
  adErrResourceLocked = $00000E92;
  adErrResourceExists = $00000E93;
  adErrCannotComplete = $00000E94;
  adErrVolumeNotFound = $00000E95;
  adErrOutOfSpace = $00000E96;
  adErrResourceOutOfScope = $00000E97;
  adErrUnavailable = $00000E98;
  adErrURLNamedRowDoesNotExist = $00000E99;
  adErrDelResOutOfScope = $00000E9A;
  adErrPropInvalidColumn = $00000E9B;
  adErrPropInvalidOption = $00000E9C;
  adErrPropInvalidValue = $00000E9D;
  adErrPropConflicting = $00000E9E;
  adErrPropNotAllSettable = $00000E9F;
  adErrPropNotSet = $00000EA0;
  adErrPropNotSettable = $00000EA1;
  adErrPropNotSupported = $00000EA2;
  adErrCatalogNotSet = $00000EA3;
  adErrCantChangeConnection = $00000EA4;
  adErrFieldsUpdateFailed = $00000EA5;
  adErrDenyNotSupported = $00000EA6;
  adErrDenyTypeNotSupported = $00000EA7;

// ParameterAttributesEnum constants
type
  ParameterAttributesEnum = TOleEnum;
const
  adParamSigned = $00000010;
  adParamNullable = $00000040;
  adParamLong = $00000080;

// ParameterDirectionEnum constants
type
  ParameterDirectionEnum = TOleEnum;
const
  adParamUnknown = $00000000;
  adParamInput = $00000001;
  adParamOutput = $00000002;
  adParamInputOutput = $00000003;
  adParamReturnValue = $00000004;

// CommandTypeEnum constants
type
  CommandTypeEnum = TOleEnum;
const
  adCmdUnspecified = $FFFFFFFF;
  adCmdUnknown = $00000008;
  adCmdText = $00000001;
  adCmdTable = $00000002;
  adCmdStoredProc = $00000004;
  adCmdFile = $00000100;
  adCmdTableDirect = $00000200;

// EventStatusEnum constants
type
  EventStatusEnum = TOleEnum;
const
  adStatusOK = $00000001;
  adStatusErrorsOccurred = $00000002;
  adStatusCantDeny = $00000003;
  adStatusCancel = $00000004;
  adStatusUnwantedEvent = $00000005;

// EventReasonEnum constants
type
  EventReasonEnum = TOleEnum;
const
  adRsnAddNew = $00000001;
  adRsnDelete = $00000002;
  adRsnUpdate = $00000003;
  adRsnUndoUpdate = $00000004;
  adRsnUndoAddNew = $00000005;
  adRsnUndoDelete = $00000006;
  adRsnRequery = $00000007;
  adRsnResynch = $00000008;
  adRsnClose = $00000009;
  adRsnMove = $0000000A;
  adRsnFirstChange = $0000000B;
  adRsnMoveFirst = $0000000C;
  adRsnMoveNext = $0000000D;
  adRsnMovePrevious = $0000000E;
  adRsnMoveLast = $0000000F;

// SchemaEnum constants
type
  SchemaEnum = TOleEnum;
const
  adSchemaProviderSpecific = $FFFFFFFF;
  adSchemaAsserts = $00000000;
  adSchemaCatalogs = $00000001;
  adSchemaCharacterSets = $00000002;
  adSchemaCollations = $00000003;
  adSchemaColumns = $00000004;
  adSchemaCheckConstraints = $00000005;
  adSchemaConstraintColumnUsage = $00000006;
  adSchemaConstraintTableUsage = $00000007;
  adSchemaKeyColumnUsage = $00000008;
  adSchemaReferentialContraints = $00000009;
  adSchemaReferentialConstraints = $00000009;
  adSchemaTableConstraints = $0000000A;
  adSchemaColumnsDomainUsage = $0000000B;
  adSchemaIndexes = $0000000C;
  adSchemaColumnPrivileges = $0000000D;
  adSchemaTablePrivileges = $0000000E;
  adSchemaUsagePrivileges = $0000000F;
  adSchemaProcedures = $00000010;
  adSchemaSchemata = $00000011;
  adSchemaSQLLanguages = $00000012;
  adSchemaStatistics = $00000013;
  adSchemaTables = $00000014;
  adSchemaTranslations = $00000015;
  adSchemaProviderTypes = $00000016;
  adSchemaViews = $00000017;
  adSchemaViewColumnUsage = $00000018;
  adSchemaViewTableUsage = $00000019;
  adSchemaProcedureParameters = $0000001A;
  adSchemaForeignKeys = $0000001B;
  adSchemaPrimaryKeys = $0000001C;
  adSchemaProcedureColumns = $0000001D;
  adSchemaDBInfoKeywords = $0000001E;
  adSchemaDBInfoLiterals = $0000001F;
  adSchemaCubes = $00000020;
  adSchemaDimensions = $00000021;
  adSchemaHierarchies = $00000022;
  adSchemaLevels = $00000023;
  adSchemaMeasures = $00000024;
  adSchemaProperties = $00000025;
  adSchemaMembers = $00000026;
  adSchemaTrustees = $00000027;

// FieldStatusEnum constants
type
  FieldStatusEnum = TOleEnum;
const
  adFieldOK = $00000000;
  adFieldCantConvertValue = $00000002;
  adFieldIsNull = $00000003;
  adFieldTruncated = $00000004;
  adFieldSignMismatch = $00000005;
  adFieldDataOverflow = $00000006;
  adFieldCantCreate = $00000007;
  adFieldUnavailable = $00000008;
  adFieldPermissionDenied = $00000009;
  adFieldIntegrityViolation = $0000000A;
  adFieldSchemaViolation = $0000000B;
  adFieldBadStatus = $0000000C;
  adFieldDefault = $0000000D;
  adFieldIgnore = $0000000F;
  adFieldDoesNotExist = $00000010;
  adFieldInvalidURL = $00000011;
  adFieldResourceLocked = $00000012;
  adFieldResourceExists = $00000013;
  adFieldCannotComplete = $00000014;
  adFieldVolumeNotFound = $00000015;
  adFieldOutOfSpace = $00000016;
  adFieldCannotDeleteSource = $00000017;
  adFieldReadOnly = $00000018;
  adFieldResourceOutOfScope = $00000019;
  adFieldAlreadyExists = $0000001A;
  adFieldPendingInsert = $00010000;
  adFieldPendingDelete = $00020000;
  adFieldPendingChange = $00040000;
  adFieldPendingUnknown = $00080000;
  adFieldPendingUnknownDelete = $00100000;

// SeekEnum constants
type
  SeekEnum = TOleEnum;
const
  adSeekFirstEQ = $00000001;
  adSeekLastEQ = $00000002;
  adSeekAfterEQ = $00000004;
  adSeekAfter = $00000008;
  adSeekBeforeEQ = $00000010;
  adSeekBefore = $00000020;

// ADCPROP_UPDATECRITERIA_ENUM constants
type
  ADCPROP_UPDATECRITERIA_ENUM = TOleEnum;
const
  adCriteriaKey = $00000000;
  adCriteriaAllCols = $00000001;
  adCriteriaUpdCols = $00000002;
  adCriteriaTimeStamp = $00000003;

// ADCPROP_ASYNCTHREADPRIORITY_ENUM constants
type
  ADCPROP_ASYNCTHREADPRIORITY_ENUM = TOleEnum;
const
  adPriorityLowest = $00000001;
  adPriorityBelowNormal = $00000002;
  adPriorityNormal = $00000003;
  adPriorityAboveNormal = $00000004;
  adPriorityHighest = $00000005;

// ADCPROP_AUTORECALC_ENUM constants
type
  ADCPROP_AUTORECALC_ENUM = TOleEnum;
const
  adRecalcUpFront = $00000000;
  adRecalcAlways = $00000001;

// ADCPROP_UPDATERESYNC_ENUM constants
type
  ADCPROP_UPDATERESYNC_ENUM = TOleEnum;
const
  adResyncNone = $00000000;
  adResyncAutoIncrement = $00000001;
  adResyncConflicts = $00000002;
  adResyncUpdates = $00000004;
  adResyncInserts = $00000008;
  adResyncAll = $0000000F;

// MoveRecordOptionsEnum constants
type
  MoveRecordOptionsEnum = TOleEnum;
const
  adMoveUnspecified = $FFFFFFFF;
  adMoveOverWrite = $00000001;
  adMoveDontUpdateLinks = $00000002;
  adMoveAllowEmulation = $00000004;

// CopyRecordOptionsEnum constants
type
  CopyRecordOptionsEnum = TOleEnum;
const
  adCopyUnspecified = $FFFFFFFF;
  adCopyOverWrite = $00000001;
  adCopyAllowEmulation = $00000004;
  adCopyNonRecursive = $00000002;

// StreamTypeEnum constants
type
  StreamTypeEnum = TOleEnum;
const
  adTypeBinary = $00000001;
  adTypeText = $00000002;

// LineSeparatorEnum constants
type
  LineSeparatorEnum = TOleEnum;
const
  adLF = $0000000A;
  adCR = $0000000D;
  adCRLF = $FFFFFFFF;

// StreamOpenOptionsEnum constants
type
  StreamOpenOptionsEnum = TOleEnum;
const
  adOpenStreamUnspecified = $FFFFFFFF;
  adOpenStreamAsync = $00000001;
  adOpenStreamFromRecord = $00000004;

// StreamWriteEnum constants
type
  StreamWriteEnum = TOleEnum;
const
  adWriteChar = $00000000;
  adWriteLine = $00000001;
  stWriteChar = $00000000;
  stWriteLine = $00000001;

// SaveOptionsEnum constants
type
  SaveOptionsEnum = TOleEnum;
const
  adSaveCreateNotExist = $00000001;
  adSaveCreateOverWrite = $00000002;

// FieldEnum constants
type
  FieldEnum = TOleEnum;
const
  adDefaultStream = $FFFFFFFF;
  adRecordURL = $FFFFFFFE;

// StreamReadEnum constants
type
  StreamReadEnum = TOleEnum;
const
  adReadAll = $FFFFFFFF;
  adReadLine = $FFFFFFFE;

// RecordTypeEnum constants
type
  RecordTypeEnum = TOleEnum;
const
  adSimpleRecord = $00000000;
  adCollectionRecord = $00000001;
  adStructDoc = $00000002;

type

// *********************************************************************//
// Forward declaration of interfaces defined in Type Library            //
// *********************************************************************//
  IADOCollection = interface;
  IADOCollectionDisp = dispinterface;
  IADODynaCollection = interface;
  IADODynaCollectionDisp = dispinterface;
  IADO = interface;
  IADODisp = dispinterface;
  IADOProperties = interface;
  IADOPropertiesDisp = dispinterface;
  IADOProperty = interface;
  IADOPropertyDisp = dispinterface;
  IADOError = interface;
  IADOErrorDisp = dispinterface;
  IADOErrors = interface;
  IADOErrorsDisp = dispinterface;
  IADOCommand15 = interface;
  IADOCommand15Disp = dispinterface;
  IADOConnection15 = interface;
  IADOConnection15Disp = dispinterface;
  IADOConnection = interface;
  IADOConnectionDisp = dispinterface;
  IADORecordset15 = interface;
  IADORecordset15Disp = dispinterface;
  IADORecordset20 = interface;
  IADORecordset20Disp = dispinterface;
  IADORecordset21 = interface;
  IADORecordset21Disp = dispinterface;
  IADORecordset = interface;
  IADORecordsetDisp = dispinterface;
  IADOFields15 = interface;
  IADOFields15Disp = dispinterface;
  IADOFields20 = interface;
  IADOFields20Disp = dispinterface;
  IADOFields = interface;
  IADOFieldsDisp = dispinterface;
  IADOField20 = interface;
  IADOField20Disp = dispinterface;
  IADOField = interface;
  IADOFieldDisp = dispinterface;
  IADOParameter = interface;
  IADOParameterDisp = dispinterface;
  IADOParameters = interface;
  IADOParametersDisp = dispinterface;
  IADOCommand = interface;
  IADOCommandDisp = dispinterface;
  IADOConnectionEventsVt = interface;
  IADORecordsetEventsVt = interface;
  IDBAsynchNotifyVt = interface;
  IADOConnectionEvents = dispinterface;
  IADORecordsetEvents = dispinterface;
  IADOConnectionConstruction15 = interface;
  IADOConnectionConstruction = interface;
  IADORecord = interface;
  IADORecordDisp = dispinterface;
  IRecFields = interface;
  IRecFieldsDisp = dispinterface;
  IADOStream = interface;
  IADOStreamDisp = dispinterface;
  IADORecordConstruction = interface;
  IADOStreamConstruction = interface;
  IADOCommandConstruction = interface;
  IADORecordsetConstruction = interface;
  IADOField15 = interface;
  IADOField15Disp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                     //
// (NOTE: Here we map each CoClass to its Default Interface)            //
// *********************************************************************//
//  Connection = IADOConnection;
//  Record_ = IADORecord;
//  Stream = IADOStream;
//  Command = IADOCommand;
//  Recordset = IADORecordset;
//  Parameter = IADOParameter;


// *********************************************************************//
// Declaration of structures, unions and aliases.                       //
// *********************************************************************//
  POleVariant1 = ^OleVariant; {*}

  SearchDirection = SearchDirectionEnum; 

// *********************************************************************//
// Interface: IADOCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000512-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOCollection = interface(IDispatch)
    ['{00000512-0000-0010-8000-00AA006D2EA4}']
    function Get_Count: Integer; safecall;
    function _NewEnum: IUnknown; safecall;
    procedure Refresh; safecall;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IADOCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000512-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOCollectionDisp = dispinterface
    ['{00000512-0000-0010-8000-00AA006D2EA4}']
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: IADODynaCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000513-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADODynaCollection = interface(IADOCollection)
    ['{00000513-0000-0010-8000-00AA006D2EA4}']
    procedure Append(const Object_: IDispatch); safecall;
    procedure Delete(Index: OleVariant); safecall;
  end;

// *********************************************************************//
// DispIntf:  IADODynaCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000513-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADODynaCollectionDisp = dispinterface
    ['{00000513-0000-0010-8000-00AA006D2EA4}']
    procedure Append(const Object_: IDispatch); dispid 1610809344;
    procedure Delete(Index: OleVariant); dispid 1610809345;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: IADO
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000534-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADO = interface(IDispatch)
    ['{00000534-0000-0010-8000-00AA006D2EA4}']
    function Get_Properties: IADOProperties; safecall;
    property Properties: IADOProperties read Get_Properties;
  end;

// *********************************************************************//
// DispIntf:  IADODisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000534-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADODisp = dispinterface
    ['{00000534-0000-0010-8000-00AA006D2EA4}']
    property Properties: IADOProperties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: IADOProperties
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000504-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOProperties = interface(IADOCollection)
    ['{00000504-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): IADOProperty; safecall;
    property Item[Index: OleVariant]: IADOProperty read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  IADOPropertiesDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000504-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOPropertiesDisp = dispinterface
    ['{00000504-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: IADOProperty readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: IADOProperty
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000503-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOProperty = interface(IDispatch)
    ['{00000503-0000-0010-8000-00AA006D2EA4}']
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(pval: OleVariant); safecall;
    function Get_Name: WideString; safecall;
    function Get_Type_: DataTypeEnum; safecall;
    function Get_Attributes: Integer; safecall;
    procedure Set_Attributes(plAttributes: Integer); safecall;
    property Value: OleVariant read Get_Value write Set_Value;
    property Name: WideString read Get_Name;
    property Type_: DataTypeEnum read Get_Type_;
    property Attributes: Integer read Get_Attributes write Set_Attributes;
  end;

// *********************************************************************//
// DispIntf:  IADOPropertyDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000503-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOPropertyDisp = dispinterface
    ['{00000503-0000-0010-8000-00AA006D2EA4}']
    property Value: OleVariant dispid 0;
    property Name: WideString readonly dispid 1610743810;
    property Type_: DataTypeEnum readonly dispid 1610743811;
    property Attributes: Integer dispid 1610743812;
  end;

// *********************************************************************//
// Interface: IADOError
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000500-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOError = interface(IDispatch)
    ['{00000500-0000-0010-8000-00AA006D2EA4}']
    function Get_Number: Integer; safecall;
    function Get_Source: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_HelpContext: Integer; safecall;
    function Get_SQLState: WideString; safecall;
    function Get_NativeError: Integer; safecall;
    property Number: Integer read Get_Number;
    property Source: WideString read Get_Source;
    property Description: WideString read Get_Description;
    property HelpFile: WideString read Get_HelpFile;
    property HelpContext: Integer read Get_HelpContext;
    property SQLState: WideString read Get_SQLState;
    property NativeError: Integer read Get_NativeError;
  end;

// *********************************************************************//
// DispIntf:  IADOErrorDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000500-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOErrorDisp = dispinterface
    ['{00000500-0000-0010-8000-00AA006D2EA4}']
    property Number: Integer readonly dispid 1;
    property Source: WideString readonly dispid 2;
    property Description: WideString readonly dispid 0;
    property HelpFile: WideString readonly dispid 3;
    property HelpContext: Integer readonly dispid 4;
    property SQLState: WideString readonly dispid 5;
    property NativeError: Integer readonly dispid 6;
  end;

// *********************************************************************//
// Interface: IADOErrors
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000501-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOErrors = interface(IADOCollection)
    ['{00000501-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): IADOError; safecall;
    procedure Clear; safecall;
    property Item[Index: OleVariant]: IADOError read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  IADOErrorsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000501-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOErrorsDisp = dispinterface
    ['{00000501-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: IADOError readonly dispid 0; default;
    procedure Clear; dispid 1610809345;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: IADOCommand15
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000508-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOCommand15 = interface(IADO)
    ['{00000508-0000-0010-8000-00AA006D2EA4}']
    function Get_ActiveConnection: IADOConnection; safecall;
    procedure Set_ActiveConnection(const ppvObject: IADOConnection); safecall;
    procedure _Set_ActiveConnection(ppvObject: OleVariant); safecall;
    function Get_CommandText: WideString; safecall;
    procedure Set_CommandText(const pbstr: WideString); safecall;
    function Get_CommandTimeout: Integer; safecall;
    procedure Set_CommandTimeout(pl: Integer); safecall;
    function Get_Prepared: WordBool; safecall;
    procedure Set_Prepared(pfPrepared: WordBool); safecall;
    function Execute(out RecordsAffected: OleVariant; var Parameters: OleVariant; Options: Integer): IADORecordset; safecall;
    function CreateParameter(const Name: WideString; Type_: DataTypeEnum;
                             Direction: ParameterDirectionEnum; Size: Integer; Value: OleVariant): IADOParameter; safecall;
    function Get_Parameters: IADOParameters; safecall;
    procedure Set_CommandType(plCmdType: CommandTypeEnum); safecall;
    function Get_CommandType: CommandTypeEnum; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const pbstrName: WideString); safecall;
    property CommandText: WideString read Get_CommandText write Set_CommandText;
    property CommandTimeout: Integer read Get_CommandTimeout write Set_CommandTimeout;
    property Prepared: WordBool read Get_Prepared write Set_Prepared;
    property Parameters: IADOParameters read Get_Parameters;
    property CommandType: CommandTypeEnum read Get_CommandType write Set_CommandType;
    property Name: WideString read Get_Name write Set_Name;
  end;

// *********************************************************************//
// DispIntf:  IADOCommand15Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000508-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOCommand15Disp = dispinterface
    ['{00000508-0000-0010-8000-00AA006D2EA4}']
    function ActiveConnection: IADOConnection; dispid 1;
    property CommandText: WideString dispid 2;
    property CommandTimeout: Integer dispid 3;
    property Prepared: WordBool dispid 4;
    function Execute(out RecordsAffected: OleVariant; var Parameters: OleVariant; Options: Integer): IADORecordset; dispid 5;
    function CreateParameter(const Name: WideString; Type_: DataTypeEnum;
                             Direction: ParameterDirectionEnum; Size: Integer; Value: OleVariant): IADOParameter; dispid 6;
    property Parameters: IADOParameters readonly dispid 0;
    property CommandType: CommandTypeEnum dispid 7;
    property Name: WideString dispid 8;
    property Properties: IADOProperties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: IADOConnection15
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {00000515-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOConnection15 = interface(IADO)
    ['{00000515-0000-0010-8000-00AA006D2EA4}']
    function Get_ConnectionString: WideString; safecall;
    procedure Set_ConnectionString(const pbstr: WideString); safecall;
    function Get_CommandTimeout: Integer; safecall;
    procedure Set_CommandTimeout(plTimeout: Integer); safecall;
    function Get_ConnectionTimeout: Integer; safecall;
    procedure Set_ConnectionTimeout(plTimeout: Integer); safecall;
    function Get_Version: WideString; safecall;
    procedure Close; safecall;
    function Execute(const CommandText: WideString; out RecordsAffected: OleVariant;
                     Options: Integer): IADORecordset; safecall;
    function BeginTrans: Integer; safecall;
    procedure CommitTrans; safecall;
    procedure RollbackTrans; safecall;
    procedure Open(const ConnectionString: WideString; const UserID: WideString;
                   const Password: WideString; Options: Integer); safecall;
    function Get_Errors: IADOErrors; safecall;
    function Get_DefaultDatabase: WideString; safecall;
    procedure Set_DefaultDatabase(const pbstr: WideString); safecall;
    function Get_IsolationLevel: IsolationLevelEnum; safecall;
    procedure Set_IsolationLevel(Level: IsolationLevelEnum); safecall;
    function Get_Attributes: Integer; safecall;
    procedure Set_Attributes(plAttr: Integer); safecall;
    function Get_CursorLocation: CursorLocationEnum; safecall;
    procedure Set_CursorLocation(plCursorLoc: CursorLocationEnum); safecall;
    function Get_Mode: ConnectModeEnum; safecall;
    procedure Set_Mode(plMode: ConnectModeEnum); safecall;
    function Get_Provider: WideString; safecall;
    procedure Set_Provider(const pbstr: WideString); safecall;
    function Get_State: Integer; safecall;
    function OpenSchema(Schema: SchemaEnum; Restrictions: OleVariant; SchemaID: OleVariant): IADORecordset; safecall;
    property ConnectionString: WideString read Get_ConnectionString write Set_ConnectionString;
    property CommandTimeout: Integer read Get_CommandTimeout write Set_CommandTimeout;
    property ConnectionTimeout: Integer read Get_ConnectionTimeout write Set_ConnectionTimeout;
    property Version: WideString read Get_Version;
    property Errors: IADOErrors read Get_Errors;
    property DefaultDatabase: WideString read Get_DefaultDatabase write Set_DefaultDatabase;
    property IsolationLevel: IsolationLevelEnum read Get_IsolationLevel write Set_IsolationLevel;
    property Attributes: Integer read Get_Attributes write Set_Attributes;
    property CursorLocation: CursorLocationEnum read Get_CursorLocation write Set_CursorLocation;
    property Mode: ConnectModeEnum read Get_Mode write Set_Mode;
    property Provider: WideString read Get_Provider write Set_Provider;
    property State: Integer read Get_State;
  end;

// *********************************************************************//
// DispIntf:  IADOConnection15Disp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {00000515-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOConnection15Disp = dispinterface
    ['{00000515-0000-0010-8000-00AA006D2EA4}']
    property ConnectionString: WideString dispid 0;
    property CommandTimeout: Integer dispid 2;
    property ConnectionTimeout: Integer dispid 3;
    property Version: WideString readonly dispid 4;
    procedure Close; dispid 5;
    function Execute(const CommandText: WideString; out RecordsAffected: OleVariant; 
                     Options: Integer): IADORecordset; dispid 6;
    function BeginTrans: Integer; dispid 7;
    procedure CommitTrans; dispid 8;
    procedure RollbackTrans; dispid 9;
    procedure Open(const ConnectionString: WideString; const UserID: WideString; 
                   const Password: WideString; Options: Integer); dispid 10;
    property Errors: IADOErrors readonly dispid 11;
    property DefaultDatabase: WideString dispid 12;
    property IsolationLevel: IsolationLevelEnum dispid 13;
    property Attributes: Integer dispid 14;
    property CursorLocation: CursorLocationEnum dispid 15;
    property Mode: ConnectModeEnum dispid 16;
    property Provider: WideString dispid 17;
    property State: Integer readonly dispid 18;
    function OpenSchema(Schema: SchemaEnum; Restrictions: OleVariant; SchemaID: OleVariant): IADORecordset; dispid 19;
    property Properties: IADOProperties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: IADOConnection
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {00000550-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOConnection = interface(IADOConnection15)
    ['{00000550-0000-0010-8000-00AA006D2EA4}']
    procedure Cancel; safecall;
  end;

// *********************************************************************//
// DispIntf:  IADOConnectionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {00000550-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOConnectionDisp = dispinterface
    ['{00000550-0000-0010-8000-00AA006D2EA4}']
    procedure Cancel; dispid 21;
    property ConnectionString: WideString dispid 0;
    property CommandTimeout: Integer dispid 2;
    property ConnectionTimeout: Integer dispid 3;
    property Version: WideString readonly dispid 4;
    procedure Close; dispid 5;
    function Execute(const CommandText: WideString; out RecordsAffected: OleVariant; 
                     Options: Integer): IADORecordset; dispid 6;
    function BeginTrans: Integer; dispid 7;
    procedure CommitTrans; dispid 8;
    procedure RollbackTrans; dispid 9;
    procedure Open(const ConnectionString: WideString; const UserID: WideString;
                   const Password: WideString; Options: Integer); dispid 10;
    property Errors: IADOErrors readonly dispid 11;
    property DefaultDatabase: WideString dispid 12;
    property IsolationLevel: IsolationLevelEnum dispid 13;
    property Attributes: Integer dispid 14;
    property CursorLocation: CursorLocationEnum dispid 15;
    property Mode: ConnectModeEnum dispid 16;
    property Provider: WideString dispid 17;
    property State: Integer readonly dispid 18;
    function OpenSchema(Schema: SchemaEnum; Restrictions: OleVariant; SchemaID: OleVariant): IADORecordset; dispid 19;
    property Properties: IADOProperties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: IADORecordset15
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000050E-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADORecordset15 = interface(IADO)
    ['{0000050E-0000-0010-8000-00AA006D2EA4}']
    function Get_AbsolutePosition: PositionEnum; safecall;
    procedure Set_AbsolutePosition(pl: PositionEnum); safecall;
    procedure Set_ActiveConnection(const pvar: IDispatch); safecall;
    procedure _Set_ActiveConnection(pvar: OleVariant); safecall;
    function Get_ActiveConnection: OleVariant; safecall;
    function Get_BOF: WordBool; safecall;
    function Get_Bookmark: OleVariant; safecall;
    procedure Set_Bookmark(pvBookmark: OleVariant); safecall;
    function Get_CacheSize: Integer; safecall;
    procedure Set_CacheSize(pl: Integer); safecall;
    function Get_CursorType: CursorTypeEnum; safecall;
    procedure Set_CursorType(plCursorType: CursorTypeEnum); safecall;
    function Get_EOF: WordBool; safecall;
    function Get_Fields: IADOFields; safecall;
    function Get_LockType: LockTypeEnum; safecall;
    procedure Set_LockType(plLockType: LockTypeEnum); safecall;
    function Get_MaxRecords: Integer; safecall;
    procedure Set_MaxRecords(plMaxRecords: Integer); safecall;
    function Get_RecordCount: Integer; safecall;
    procedure Set_Source(const pvSource: IDispatch); safecall;
    procedure _Set_Source(const pvSource: WideString); safecall;
    function Get_Source: OleVariant; safecall;
    procedure AddNew(FieldList: OleVariant; Values: OleVariant); safecall;
    procedure CancelUpdate; safecall;
    procedure Close; safecall;
    procedure Delete(AffectRecords: AffectEnum); safecall;
    function GetRows(Rows: Integer; Start: OleVariant; Fields: OleVariant): OleVariant; safecall;
    procedure Move(NumRecords: Integer; Start: OleVariant); safecall;
    procedure MoveNext; safecall;
    procedure MovePrevious; safecall;
    procedure MoveFirst; safecall;
    procedure MoveLast; safecall;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; CursorType: CursorTypeEnum;
                   LockType: LockTypeEnum; Options: Integer); safecall;
    procedure Requery(Options: Integer); safecall;
    procedure _xResync(AffectRecords: AffectEnum); safecall;
    procedure Update(Fields: OleVariant; Values: OleVariant); safecall;
    function Get_AbsolutePage: PositionEnum; safecall;
    procedure Set_AbsolutePage(pl: PositionEnum); safecall;
    function Get_EditMode: EditModeEnum; safecall;
    function Get_Filter: OleVariant; safecall;
    procedure Set_Filter(Criteria: OleVariant); safecall;
    function Get_PageCount: Integer; safecall;
    function Get_PageSize: Integer; safecall;
    procedure Set_PageSize(pl: Integer); safecall;
    function Get_Sort: WideString; safecall;
    procedure Set_Sort(const Criteria: WideString); safecall;
    function Get_Status: Integer; safecall;
    function Get_State: Integer; safecall;
    function _xClone: IADORecordset; safecall;
    procedure UpdateBatch(AffectRecords: AffectEnum); safecall;
    procedure CancelBatch(AffectRecords: AffectEnum); safecall;
    function Get_CursorLocation: CursorLocationEnum; safecall;
    procedure Set_CursorLocation(plCursorLoc: CursorLocationEnum); safecall;
    function NextRecordset(out RecordsAffected: OleVariant): IADORecordset; safecall;
    function Supports(CursorOptions: CursorOptionEnum): WordBool; safecall;
    function Get_Collect(Index: OleVariant): OleVariant; safecall;
    procedure Set_Collect(Index: OleVariant; pvar: OleVariant); safecall;
    function Get_MarshalOptions: MarshalOptionsEnum; safecall;
    procedure Set_MarshalOptions(peMarshal: MarshalOptionsEnum); safecall;
    procedure Find(const Criteria: WideString; SkipRecords: Integer;
                   SearchDirection: SearchDirectionEnum; Start: OleVariant); safecall;
    property AbsolutePosition: PositionEnum read Get_AbsolutePosition write Set_AbsolutePosition;
    property BOF: WordBool read Get_BOF;
    property Bookmark: OleVariant read Get_Bookmark write Set_Bookmark;
    property CacheSize: Integer read Get_CacheSize write Set_CacheSize;
    property CursorType: CursorTypeEnum read Get_CursorType write Set_CursorType;
    property EOF: WordBool read Get_EOF;
    property Fields: IADOFields read Get_Fields;
    property LockType: LockTypeEnum read Get_LockType write Set_LockType;
    property MaxRecords: Integer read Get_MaxRecords write Set_MaxRecords;
    property RecordCount: Integer read Get_RecordCount;
    property AbsolutePage: PositionEnum read Get_AbsolutePage write Set_AbsolutePage;
    property EditMode: EditModeEnum read Get_EditMode;
    property Filter: OleVariant read Get_Filter write Set_Filter;
    property PageCount: Integer read Get_PageCount;
    property PageSize: Integer read Get_PageSize write Set_PageSize;
    property Sort: WideString read Get_Sort write Set_Sort;
    property Status: Integer read Get_Status;
    property State: Integer read Get_State;
    property CursorLocation: CursorLocationEnum read Get_CursorLocation write Set_CursorLocation;
    property Collect[Index: OleVariant]: OleVariant read Get_Collect write Set_Collect;
    property MarshalOptions: MarshalOptionsEnum read Get_MarshalOptions write Set_MarshalOptions;
  end;

// *********************************************************************//
// DispIntf:  IADORecordset15Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000050E-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADORecordset15Disp = dispinterface
    ['{0000050E-0000-0010-8000-00AA006D2EA4}']
    property AbsolutePosition: PositionEnum dispid 1000;
    function ActiveConnection: IDispatch; dispid 1001;
    property BOF: WordBool readonly dispid 1002;
    property Bookmark: OleVariant dispid 1003;
    property CacheSize: Integer dispid 1004;
    property CursorType: CursorTypeEnum dispid 1005;
    property EOF: WordBool readonly dispid 1006;
    property Fields: IADOFields readonly dispid 0;
    property LockType: LockTypeEnum dispid 1008;
    property MaxRecords: Integer dispid 1009;
    property RecordCount: Integer readonly dispid 1010;
    function Source: IDispatch; dispid 1011;
    procedure AddNew(FieldList: OleVariant; Values: OleVariant); dispid 1012;
    procedure CancelUpdate; dispid 1013;
    procedure Close; dispid 1014;
    procedure Delete(AffectRecords: AffectEnum); dispid 1015;
    function GetRows(Rows: Integer; Start: OleVariant; Fields: OleVariant): OleVariant; dispid 1016;
    procedure Move(NumRecords: Integer; Start: OleVariant); dispid 1017;
    procedure MoveNext; dispid 1018;
    procedure MovePrevious; dispid 1019;
    procedure MoveFirst; dispid 1020;
    procedure MoveLast; dispid 1021;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; CursorType: CursorTypeEnum; 
                   LockType: LockTypeEnum; Options: Integer); dispid 1022;
    procedure Requery(Options: Integer); dispid 1023;
    procedure _xResync(AffectRecords: AffectEnum); dispid 1610809378;
    procedure Update(Fields: OleVariant; Values: OleVariant); dispid 1025;
    property AbsolutePage: PositionEnum dispid 1047;
    property EditMode: EditModeEnum readonly dispid 1026;
    property Filter: OleVariant dispid 1030;
    property PageCount: Integer readonly dispid 1050;
    property PageSize: Integer dispid 1048;
    property Sort: WideString dispid 1031;
    property Status: Integer readonly dispid 1029;
    property State: Integer readonly dispid 1054;
    function _xClone: IADORecordset; dispid 1610809392;
    procedure UpdateBatch(AffectRecords: AffectEnum); dispid 1035;
    procedure CancelBatch(AffectRecords: AffectEnum); dispid 1049;
    property CursorLocation: CursorLocationEnum dispid 1051;
    function NextRecordset(out RecordsAffected: OleVariant): IADORecordset; dispid 1052;
    function Supports(CursorOptions: CursorOptionEnum): WordBool; dispid 1036;
    property Collect[Index: OleVariant]: OleVariant dispid -8;
    property MarshalOptions: MarshalOptionsEnum dispid 1053;
    procedure Find(const Criteria: WideString; SkipRecords: Integer; 
                   SearchDirection: SearchDirectionEnum; Start: OleVariant); dispid 1058;
    property Properties: IADOProperties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: IADORecordset20
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054F-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADORecordset20 = interface(IADORecordset15)
    ['{0000054F-0000-0010-8000-00AA006D2EA4}']
    procedure Cancel; safecall;
    function Get_DataSource: IUnknown; safecall;
    procedure Set_DataSource(const ppunkDataSource: IUnknown); safecall;
    procedure _xSave(const FileName: WideString; PersistFormat: PersistFormatEnum); safecall;
    function Get_ActiveCommand: IDispatch; safecall;
    procedure Set_StayInSync(pbStayInSync: WordBool); safecall;
    function Get_StayInSync: WordBool; safecall;
    function GetString(StringFormat: StringFormatEnum; NumRows: Integer;
                       const ColumnDelimeter: WideString; const RowDelimeter: WideString; 
                       const NullExpr: WideString): WideString; safecall;
    function Get_DataMember: WideString; safecall;
    procedure Set_DataMember(const pbstrDataMember: WideString); safecall;
    function CompareBookmarks(Bookmark1: OleVariant; Bookmark2: OleVariant): CompareEnum; safecall;
    function Clone(LockType: LockTypeEnum): IADORecordset; safecall;
    procedure Resync(AffectRecords: AffectEnum; ResyncValues: ResyncEnum); safecall;
    property DataSource: IUnknown read Get_DataSource write Set_DataSource;
    property ActiveCommand: IDispatch read Get_ActiveCommand;
    property StayInSync: WordBool read Get_StayInSync write Set_StayInSync;
    property DataMember: WideString read Get_DataMember write Set_DataMember;
  end;

// *********************************************************************//
// DispIntf:  IADORecordset20Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054F-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADORecordset20Disp = dispinterface
    ['{0000054F-0000-0010-8000-00AA006D2EA4}']
    procedure Cancel; dispid 1055;
    property DataSource: IUnknown dispid 1056;
    procedure _xSave(const FileName: WideString; PersistFormat: PersistFormatEnum); dispid 1610874883;
    property ActiveCommand: IDispatch readonly dispid 1061;
    property StayInSync: WordBool dispid 1063;
    function GetString(StringFormat: StringFormatEnum; NumRows: Integer; 
                       const ColumnDelimeter: WideString; const RowDelimeter: WideString; 
                       const NullExpr: WideString): WideString; dispid 1062;
    property DataMember: WideString dispid 1064;
    function CompareBookmarks(Bookmark1: OleVariant; Bookmark2: OleVariant): CompareEnum; dispid 1065;
    function Clone(LockType: LockTypeEnum): IADORecordset; dispid 1034;
    procedure Resync(AffectRecords: AffectEnum; ResyncValues: ResyncEnum); dispid 1024;
    property AbsolutePosition: PositionEnum dispid 1000;
    function ActiveConnection: IDispatch; dispid 1001;
    property BOF: WordBool readonly dispid 1002;
    property Bookmark: OleVariant dispid 1003;
    property CacheSize: Integer dispid 1004;
    property CursorType: CursorTypeEnum dispid 1005;
    property EOF: WordBool readonly dispid 1006;
    property Fields: IADOFields readonly dispid 0;
    property LockType: LockTypeEnum dispid 1008;
    property MaxRecords: Integer dispid 1009;
    property RecordCount: Integer readonly dispid 1010;
    function Source: IDispatch; dispid 1011;
    procedure AddNew(FieldList: OleVariant; Values: OleVariant); dispid 1012;
    procedure CancelUpdate; dispid 1013;
    procedure Close; dispid 1014;
    procedure Delete(AffectRecords: AffectEnum); dispid 1015;
    function GetRows(Rows: Integer; Start: OleVariant; Fields: OleVariant): OleVariant; dispid 1016;
    procedure Move(NumRecords: Integer; Start: OleVariant); dispid 1017;
    procedure MoveNext; dispid 1018;
    procedure MovePrevious; dispid 1019;
    procedure MoveFirst; dispid 1020;
    procedure MoveLast; dispid 1021;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; CursorType: CursorTypeEnum; 
                   LockType: LockTypeEnum; Options: Integer); dispid 1022;
    procedure Requery(Options: Integer); dispid 1023;
    procedure _xResync(AffectRecords: AffectEnum); dispid 1610809378;
    procedure Update(Fields: OleVariant; Values: OleVariant); dispid 1025;
    property AbsolutePage: PositionEnum dispid 1047;
    property EditMode: EditModeEnum readonly dispid 1026;
    property Filter: OleVariant dispid 1030;
    property PageCount: Integer readonly dispid 1050;
    property PageSize: Integer dispid 1048;
    property Sort: WideString dispid 1031;
    property Status: Integer readonly dispid 1029;
    property State: Integer readonly dispid 1054;
    function _xClone: IADORecordset; dispid 1610809392;
    procedure UpdateBatch(AffectRecords: AffectEnum); dispid 1035;
    procedure CancelBatch(AffectRecords: AffectEnum); dispid 1049;
    property CursorLocation: CursorLocationEnum dispid 1051;
    function NextRecordset(out RecordsAffected: OleVariant): IADORecordset; dispid 1052;
    function Supports(CursorOptions: CursorOptionEnum): WordBool; dispid 1036;
    property Collect[Index: OleVariant]: OleVariant dispid -8;
    property MarshalOptions: MarshalOptionsEnum dispid 1053;
    procedure Find(const Criteria: WideString; SkipRecords: Integer; 
                   SearchDirection: SearchDirectionEnum; Start: OleVariant); dispid 1058;
    property Properties: IADOProperties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: IADORecordset21
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000555-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADORecordset21 = interface(IADORecordset20)
    ['{00000555-0000-0010-8000-00AA006D2EA4}']
    procedure Seek(KeyValues: OleVariant; SeekOption: SeekEnum); safecall;
    procedure Set_Index(const pbstrIndex: WideString); safecall;
    function Get_Index: WideString; safecall;
    property Index: WideString read Get_Index write Set_Index;
  end;

// *********************************************************************//
// DispIntf:  IADORecordset21Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000555-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADORecordset21Disp = dispinterface
    ['{00000555-0000-0010-8000-00AA006D2EA4}']
    procedure Seek(KeyValues: OleVariant; SeekOption: SeekEnum); dispid 1066;
    property Index: WideString dispid 1067;
    procedure Cancel; dispid 1055;
    property DataSource: IUnknown dispid 1056;
    procedure _xSave(const FileName: WideString; PersistFormat: PersistFormatEnum); dispid 1610874883;
    property ActiveCommand: IDispatch readonly dispid 1061;
    property StayInSync: WordBool dispid 1063;
    function GetString(StringFormat: StringFormatEnum; NumRows: Integer;
                       const ColumnDelimeter: WideString; const RowDelimeter: WideString;
                       const NullExpr: WideString): WideString; dispid 1062;
    property DataMember: WideString dispid 1064;
    function CompareBookmarks(Bookmark1: OleVariant; Bookmark2: OleVariant): CompareEnum; dispid 1065;
    function Clone(LockType: LockTypeEnum): IADORecordset; dispid 1034;
    procedure Resync(AffectRecords: AffectEnum; ResyncValues: ResyncEnum); dispid 1024;
    property AbsolutePosition: PositionEnum dispid 1000;
    function ActiveConnection: IDispatch; dispid 1001;
    property BOF: WordBool readonly dispid 1002;
    property Bookmark: OleVariant dispid 1003;
    property CacheSize: Integer dispid 1004;
    property CursorType: CursorTypeEnum dispid 1005;
    property EOF: WordBool readonly dispid 1006;
    property Fields: IADOFields readonly dispid 0;
    property LockType: LockTypeEnum dispid 1008;
    property MaxRecords: Integer dispid 1009;
    property RecordCount: Integer readonly dispid 1010;
    function Source: IDispatch; dispid 1011;
    procedure AddNew(FieldList: OleVariant; Values: OleVariant); dispid 1012;
    procedure CancelUpdate; dispid 1013;
    procedure Close; dispid 1014;
    procedure Delete(AffectRecords: AffectEnum); dispid 1015;
    function GetRows(Rows: Integer; Start: OleVariant; Fields: OleVariant): OleVariant; dispid 1016;
    procedure Move(NumRecords: Integer; Start: OleVariant); dispid 1017;
    procedure MoveNext; dispid 1018;
    procedure MovePrevious; dispid 1019;
    procedure MoveFirst; dispid 1020;
    procedure MoveLast; dispid 1021;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; CursorType: CursorTypeEnum;
                   LockType: LockTypeEnum; Options: Integer); dispid 1022;
    procedure Requery(Options: Integer); dispid 1023;
    procedure _xResync(AffectRecords: AffectEnum); dispid 1610809378;
    procedure Update(Fields: OleVariant; Values: OleVariant); dispid 1025;
    property AbsolutePage: PositionEnum dispid 1047;
    property EditMode: EditModeEnum readonly dispid 1026;
    property Filter: OleVariant dispid 1030;
    property PageCount: Integer readonly dispid 1050;
    property PageSize: Integer dispid 1048;
    property Sort: WideString dispid 1031;
    property Status: Integer readonly dispid 1029;
    property State: Integer readonly dispid 1054;
    function _xClone: IADORecordset; dispid 1610809392;
    procedure UpdateBatch(AffectRecords: AffectEnum); dispid 1035;
    procedure CancelBatch(AffectRecords: AffectEnum); dispid 1049;
    property CursorLocation: CursorLocationEnum dispid 1051;
    function NextRecordset(out RecordsAffected: OleVariant): IADORecordset; dispid 1052;
    function Supports(CursorOptions: CursorOptionEnum): WordBool; dispid 1036;
    property Collect[Index: OleVariant]: OleVariant dispid -8;
    property MarshalOptions: MarshalOptionsEnum dispid 1053;
    procedure Find(const Criteria: WideString; SkipRecords: Integer;
                   SearchDirection: SearchDirectionEnum; Start: OleVariant); dispid 1058;
    property Properties: IADOProperties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: IADORecordset
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000556-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADORecordset = interface(IADORecordset21)
    ['{00000556-0000-0010-8000-00AA006D2EA4}']
    procedure Save(Destination: OleVariant; PersistFormat: PersistFormatEnum); safecall;
  end;

// *********************************************************************//
// DispIntf:  IADORecordsetDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000556-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADORecordsetDisp = dispinterface
    ['{00000556-0000-0010-8000-00AA006D2EA4}']
    procedure Save(Destination: OleVariant; PersistFormat: PersistFormatEnum); dispid 1057;
    procedure Seek(KeyValues: OleVariant; SeekOption: SeekEnum); dispid 1066;
    property Index: WideString dispid 1067;
    procedure Cancel; dispid 1055;
    property DataSource: IUnknown dispid 1056;
    procedure _xSave(const FileName: WideString; PersistFormat: PersistFormatEnum); dispid 1610874883;
    property ActiveCommand: IDispatch readonly dispid 1061;
    property StayInSync: WordBool dispid 1063;
    function GetString(StringFormat: StringFormatEnum; NumRows: Integer; 
                       const ColumnDelimeter: WideString; const RowDelimeter: WideString; 
                       const NullExpr: WideString): WideString; dispid 1062;
    property DataMember: WideString dispid 1064;
    function CompareBookmarks(Bookmark1: OleVariant; Bookmark2: OleVariant): CompareEnum; dispid 1065;
    function Clone(LockType: LockTypeEnum): IADORecordset; dispid 1034;
    procedure Resync(AffectRecords: AffectEnum; ResyncValues: ResyncEnum); dispid 1024;
    property AbsolutePosition: PositionEnum dispid 1000;
    function ActiveConnection: IDispatch; dispid 1001;
    property BOF: WordBool readonly dispid 1002;
    property Bookmark: OleVariant dispid 1003;
    property CacheSize: Integer dispid 1004;
    property CursorType: CursorTypeEnum dispid 1005;
    property EOF: WordBool readonly dispid 1006;
    property Fields: IADOFields readonly dispid 0;
    property LockType: LockTypeEnum dispid 1008;
    property MaxRecords: Integer dispid 1009;
    property RecordCount: Integer readonly dispid 1010;
    function Source: IDispatch; dispid 1011;
    procedure AddNew(FieldList: OleVariant; Values: OleVariant); dispid 1012;
    procedure CancelUpdate; dispid 1013;
    procedure Close; dispid 1014;
    procedure Delete(AffectRecords: AffectEnum); dispid 1015;
    function GetRows(Rows: Integer; Start: OleVariant; Fields: OleVariant): OleVariant; dispid 1016;
    procedure Move(NumRecords: Integer; Start: OleVariant); dispid 1017;
    procedure MoveNext; dispid 1018;
    procedure MovePrevious; dispid 1019;
    procedure MoveFirst; dispid 1020;
    procedure MoveLast; dispid 1021;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; CursorType: CursorTypeEnum; 
                   LockType: LockTypeEnum; Options: Integer); dispid 1022;
    procedure Requery(Options: Integer); dispid 1023;
    procedure _xResync(AffectRecords: AffectEnum); dispid 1610809378;
    procedure Update(Fields: OleVariant; Values: OleVariant); dispid 1025;
    property AbsolutePage: PositionEnum dispid 1047;
    property EditMode: EditModeEnum readonly dispid 1026;
    property Filter: OleVariant dispid 1030;
    property PageCount: Integer readonly dispid 1050;
    property PageSize: Integer dispid 1048;
    property Sort: WideString dispid 1031;
    property Status: Integer readonly dispid 1029;
    property State: Integer readonly dispid 1054;
    function _xClone: IADORecordset; dispid 1610809392;
    procedure UpdateBatch(AffectRecords: AffectEnum); dispid 1035;
    procedure CancelBatch(AffectRecords: AffectEnum); dispid 1049;
    property CursorLocation: CursorLocationEnum dispid 1051;
    function NextRecordset(out RecordsAffected: OleVariant): IADORecordset; dispid 1052;
    function Supports(CursorOptions: CursorOptionEnum): WordBool; dispid 1036;
    property Collect[Index: OleVariant]: OleVariant dispid -8;
    property MarshalOptions: MarshalOptionsEnum dispid 1053;
    procedure Find(const Criteria: WideString; SkipRecords: Integer; 
                   SearchDirection: SearchDirectionEnum; Start: OleVariant); dispid 1058;
    property Properties: IADOProperties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: IADOFields15
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000506-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOFields15 = interface(IADOCollection)
    ['{00000506-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): IADOField; safecall;
    property Item[Index: OleVariant]: IADOField read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  IADOFields15Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000506-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOFields15Disp = dispinterface
    ['{00000506-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: IADOField readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: IADOFields20
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054D-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOFields20 = interface(IADOFields15)
    ['{0000054D-0000-0010-8000-00AA006D2EA4}']
    procedure _Append(const Name: WideString; Type_: DataTypeEnum; DefinedSize: Integer;
                      Attrib: FieldAttributeEnum); safecall;
    procedure Delete(Index: OleVariant); safecall;
  end;

// *********************************************************************//
// DispIntf:  IADOFields20Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054D-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOFields20Disp = dispinterface
    ['{0000054D-0000-0010-8000-00AA006D2EA4}']
    procedure _Append(const Name: WideString; Type_: DataTypeEnum; DefinedSize: Integer;
                      Attrib: FieldAttributeEnum); dispid 1610874880;
    procedure Delete(Index: OleVariant); dispid 4;
    property Item[Index: OleVariant]: IADOField readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: IADOFields
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000564-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOFields = interface(IADOFields20)
    ['{00000564-0000-0010-8000-00AA006D2EA4}']
    procedure Append(const Name: WideString; Type_: DataTypeEnum; DefinedSize: Integer;
                     Attrib: FieldAttributeEnum; FieldValue: OleVariant); safecall;
    procedure Update; safecall;
    procedure Resync(ResyncValues: ResyncEnum); safecall;
    procedure CancelUpdate; safecall;
  end;

// *********************************************************************//
// DispIntf:  IADOFieldsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000564-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOFieldsDisp = dispinterface
    ['{00000564-0000-0010-8000-00AA006D2EA4}']
    procedure Append(const Name: WideString; Type_: DataTypeEnum; DefinedSize: Integer;
                     Attrib: FieldAttributeEnum; FieldValue: OleVariant); dispid 3;
    procedure Update; dispid 5;
    procedure Resync(ResyncValues: ResyncEnum); dispid 6;
    procedure CancelUpdate; dispid 7;
    procedure _Append(const Name: WideString; Type_: DataTypeEnum; DefinedSize: Integer;
                      Attrib: FieldAttributeEnum); dispid 1610874880;
    procedure Delete(Index: OleVariant); dispid 4;
    property Item[Index: OleVariant]: IADOField readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: IADOField20
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054C-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOField20 = interface(IADO)
    ['{0000054C-0000-0010-8000-00AA006D2EA4}']
    function Get_ActualSize: Integer; safecall;
    function Get_Attributes: Integer; safecall;
    function Get_DefinedSize: Integer; safecall;
    function Get_Name: WideString; safecall;
    function Get_Type_: DataTypeEnum; safecall;
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(pvar: OleVariant); safecall;
    function Get_Precision: Byte; safecall;
    function Get_NumericScale: Byte; safecall;
    procedure AppendChunk(Data: OleVariant); safecall;
    function GetChunk(Length: Integer): OleVariant; safecall;
    function Get_OriginalValue: OleVariant; safecall;
    function Get_UnderlyingValue: OleVariant; safecall;
    function Get_DataFormat: IUnknown; safecall;
    procedure Set_DataFormat(const ppiDF: IUnknown); safecall;
    procedure Set_Precision(pbPrecision: Byte); safecall;
    procedure Set_NumericScale(pbNumericScale: Byte); safecall;
    procedure Set_Type_(pDataType: DataTypeEnum); safecall;
    procedure Set_DefinedSize(pl: Integer); safecall;
    procedure Set_Attributes(pl: Integer); safecall;
    property ActualSize: Integer read Get_ActualSize;
    property Attributes: Integer read Get_Attributes write Set_Attributes;
    property DefinedSize: Integer read Get_DefinedSize write Set_DefinedSize;
    property Name: WideString read Get_Name;
    property Type_: DataTypeEnum read Get_Type_ write Set_Type_;
    property Value: OleVariant read Get_Value write Set_Value;
    property Precision: Byte read Get_Precision write Set_Precision;
    property NumericScale: Byte read Get_NumericScale write Set_NumericScale;
    property OriginalValue: OleVariant read Get_OriginalValue;
    property UnderlyingValue: OleVariant read Get_UnderlyingValue;
    property DataFormat: IUnknown read Get_DataFormat write Set_DataFormat;
  end;

// *********************************************************************//
// DispIntf:  IADOField20Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054C-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOField20Disp = dispinterface
    ['{0000054C-0000-0010-8000-00AA006D2EA4}']
    property ActualSize: Integer readonly dispid 1109;
    property Attributes: Integer dispid 1114;
    property DefinedSize: Integer dispid 1103;
    property Name: WideString readonly dispid 1100;
    property Type_: DataTypeEnum dispid 1102;
    property Value: OleVariant dispid 0;
    property Precision: Byte dispid 1112;
    property NumericScale: Byte dispid 1113;
    procedure AppendChunk(Data: OleVariant); dispid 1107;
    function GetChunk(Length: Integer): OleVariant; dispid 1108;
    property OriginalValue: OleVariant readonly dispid 1104;
    property UnderlyingValue: OleVariant readonly dispid 1105;
    property DataFormat: IUnknown dispid 1115;
    property Properties: IADOProperties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: IADOField
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000569-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOField = interface(IADOField20)
    ['{00000569-0000-0010-8000-00AA006D2EA4}']
    function Get_Status: Integer; safecall;
    property Status: Integer read Get_Status;
  end;

// *********************************************************************//
// DispIntf:  IADOFieldDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000569-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOFieldDisp = dispinterface
    ['{00000569-0000-0010-8000-00AA006D2EA4}']
    property Status: Integer readonly dispid 1116;
    property ActualSize: Integer readonly dispid 1109;
    property Attributes: Integer dispid 1114;
    property DefinedSize: Integer dispid 1103;
    property Name: WideString readonly dispid 1100;
    property Type_: DataTypeEnum dispid 1102;
    property Value: OleVariant dispid 0;
    property Precision: Byte dispid 1112;
    property NumericScale: Byte dispid 1113;
    procedure AppendChunk(Data: OleVariant); dispid 1107;
    function GetChunk(Length: Integer): OleVariant; dispid 1108;
    property OriginalValue: OleVariant readonly dispid 1104;
    property UnderlyingValue: OleVariant readonly dispid 1105;
    property DataFormat: IUnknown dispid 1115;
    property Properties: IADOProperties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: IADOParameter
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000050C-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOParameter = interface(IADO)
    ['{0000050C-0000-0010-8000-00AA006D2EA4}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const pbstr: WideString); safecall;
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(pvar: OleVariant); safecall;
    function Get_Type_: DataTypeEnum; safecall;
    procedure Set_Type_(psDataType: DataTypeEnum); safecall;
    procedure Set_Direction(plParmDirection: ParameterDirectionEnum); safecall;
    function Get_Direction: ParameterDirectionEnum; safecall;
    procedure Set_Precision(pbPrecision: Byte); safecall;
    function Get_Precision: Byte; safecall;
    procedure Set_NumericScale(pbScale: Byte); safecall;
    function Get_NumericScale: Byte; safecall;
    procedure Set_Size(pl: Integer); safecall;
    function Get_Size: Integer; safecall;
    procedure AppendChunk(Val: OleVariant); safecall;
    function Get_Attributes: Integer; safecall;
    procedure Set_Attributes(plParmAttribs: Integer); safecall;
    property Name: WideString read Get_Name write Set_Name;
    property Value: OleVariant read Get_Value write Set_Value;
    property Type_: DataTypeEnum read Get_Type_ write Set_Type_;
    property Direction: ParameterDirectionEnum read Get_Direction write Set_Direction;
    property Precision: Byte read Get_Precision write Set_Precision;
    property NumericScale: Byte read Get_NumericScale write Set_NumericScale;
    property Size: Integer read Get_Size write Set_Size;
    property Attributes: Integer read Get_Attributes write Set_Attributes;
  end;

// *********************************************************************//
// DispIntf:  IADOParameterDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000050C-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOParameterDisp = dispinterface
    ['{0000050C-0000-0010-8000-00AA006D2EA4}']
    property Name: WideString dispid 1;
    property Value: OleVariant dispid 0;
    property Type_: DataTypeEnum dispid 2;
    property Direction: ParameterDirectionEnum dispid 3;
    property Precision: Byte dispid 4;
    property NumericScale: Byte dispid 5;
    property Size: Integer dispid 6;
    procedure AppendChunk(Val: OleVariant); dispid 7;
    property Attributes: Integer dispid 8;
    property Properties: IADOProperties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: IADOParameters
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000050D-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOParameters = interface(IADODynaCollection)
    ['{0000050D-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): IADOParameter; safecall;
    property Item[Index: OleVariant]: IADOParameter read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  IADOParametersDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000050D-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOParametersDisp = dispinterface
    ['{0000050D-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: IADOParameter readonly dispid 0; default;
    procedure Append(const Object_: IDispatch); dispid 1610809344;
    procedure Delete(Index: OleVariant); dispid 1610809345;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: IADOCommand
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054E-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOCommand = interface(IADOCommand15)
    ['{0000054E-0000-0010-8000-00AA006D2EA4}']
    function Get_State: Integer; safecall;
    procedure Cancel; safecall;
    property State: Integer read Get_State;
  end;

// *********************************************************************//
// DispIntf:  IADOCommandDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054E-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOCommandDisp = dispinterface
    ['{0000054E-0000-0010-8000-00AA006D2EA4}']
    property State: Integer readonly dispid 9;
    procedure Cancel; dispid 10;
    function ActiveConnection: IADOConnection; dispid 1;
    property CommandText: WideString dispid 2;
    property CommandTimeout: Integer dispid 3;
    property Prepared: WordBool dispid 4;
    function Execute(out RecordsAffected: OleVariant; var Parameters: OleVariant; Options: Integer): IADORecordset; dispid 5;
    function CreateParameter(const Name: WideString; Type_: DataTypeEnum;
                             Direction: ParameterDirectionEnum; Size: Integer; Value: OleVariant): IADOParameter; dispid 6;
    property Parameters: IADOParameters readonly dispid 0;
    property CommandType: CommandTypeEnum dispid 7;
    property Name: WideString dispid 8;
    property Properties: IADOProperties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: IADOConnectionEventsVt
// Flags:     (16) Hidden
// GUID:      {00000402-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOConnectionEventsVt = interface(IUnknown)
    ['{00000402-0000-0010-8000-00AA006D2EA4}']
    function InfoMessage(const pError: IADOError; var adStatus: EventStatusEnum;
                         const pConnection: IADOConnection): HResult; stdcall;
    function BeginTransComplete(TransactionLevel: Integer; const pError: IADOError;
                                var adStatus: EventStatusEnum; const pConnection: IADOConnection): HResult; stdcall;
    function CommitTransComplete(const pError: IADOError; var adStatus: EventStatusEnum;
                                 const pConnection: IADOConnection): HResult; stdcall;
    function RollbackTransComplete(const pError: IADOError; var adStatus: EventStatusEnum;
                                   const pConnection: IADOConnection): HResult; stdcall;
    function WillExecute(var Source: WideString; var CursorType: CursorTypeEnum;
                         var LockType: LockTypeEnum; var Options: Integer;
                         var adStatus: EventStatusEnum; const pCommand: IADOCommand;
                         const pRecordset: IADORecordset; const pConnection: IADOConnection): HResult; stdcall;
    function ExecuteComplete(RecordsAffected: Integer; const pError: IADOError;
                             var adStatus: EventStatusEnum; const pCommand: IADOCommand;
                             const pRecordset: IADORecordset; const pConnection: IADOConnection): HResult; stdcall;
    function WillConnect(var ConnectionString: WideString; var UserID: WideString;
                         var Password: WideString; var Options: Integer;
                         var adStatus: EventStatusEnum; const pConnection: IADOConnection): HResult; stdcall;
    function ConnectComplete(const pError: IADOError; var adStatus: EventStatusEnum;
                             const pConnection: IADOConnection): HResult; stdcall;
    function Disconnect(var adStatus: EventStatusEnum; const pConnection: IADOConnection): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IADORecordsetEventsVt
// Flags:     (16) Hidden
// GUID:      {00000403-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADORecordsetEventsVt = interface(IUnknown)
    ['{00000403-0000-0010-8000-00AA006D2EA4}']
    function WillChangeField(cFields: Integer; Fields: OleVariant; var adStatus: Integer;
                             const pRecordset: IADORecordset): HResult; stdcall;
    function FieldChangeComplete(cFields: Integer; Fields: OleVariant; const pError: IADOError;
                                 var adStatus: Integer; const pRecordset: IADORecordset): HResult; stdcall;
    function WillChangeRecord(adReason: EventReasonEnum; cRecords: Integer;
                              var adStatus: Integer; const pRecordset: IADORecordset): HResult; stdcall;
    function RecordChangeComplete(adReason: EventReasonEnum; cRecords: Integer;
                                  const pError: IADOError; var adStatus: Integer;
                                  const pRecordset: IADORecordset): HResult; stdcall;
    function WillChangeRecordset(adReason: EventReasonEnum; var adStatus: Integer;
                                 const pRecordset: IADORecordset): HResult; stdcall;
    function RecordsetChangeComplete(adReason: EventReasonEnum; const pError: IADOError;
                                     var adStatus: Integer; const pRecordset: IADORecordset): HResult; stdcall;
    function WillMove(adReason: EventReasonEnum; var adStatus: Integer;
                      const pRecordset: IADORecordset): HResult; stdcall;
    function MoveComplete(adReason: EventReasonEnum; const pError: IADOError;
                          var adStatus: Integer; const pRecordset: IADORecordset): HResult; stdcall;
    function EndOfRecordset(var fMoreData: WordBool; var adStatus: Integer;
                            const pRecordset: IADORecordset): HResult; stdcall;
    function FetchProgress(Progress: Integer; MaxProgress: Integer; var adStatus: Integer;
                           const pRecordset: IADORecordset): HResult; stdcall;
    function FetchComplete(const pError: IADOError; var adStatus: Integer;
                           const pRecordset: IADORecordset): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDBAsynchNotifyVt
// Flags:     (16) Hidden
// GUID:      {0C733A96-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IDBAsynchNotifyVt = interface(IUnknown)
    ['{0C733A96-2A1C-11CE-ADE5-00AA0044773D}']
        function OnLowResource(dwReserved: DWORD): HRESULT; stdcall;
        function OnProgress(const hChapter: ULONG;
                 const eOperation: DWORD;
                 const ulProgress: ULONG;
                 const ulProgressMax: ULONG;
                 const eAsynchPhase: DWORD;
                 const pwszStatusText: WideString): HRESULT; stdcall;
        function OnStop(hChapter: ULONG; eOperation: DWORD;
                 hrStatus: HRESULT;
                 pwszStatusText: WideString): HRESULT; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IADOConnectionEvents
// Flags:     (4096) Dispatchable
// GUID:      {00000400-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOConnectionEvents = dispinterface
    ['{00000400-0000-0010-8000-00AA006D2EA4}']
    procedure InfoMessage(const pError: IADOError; var adStatus: EventStatusEnum;
                          const pConnection: IADOConnection); dispid 0;
    procedure BeginTransComplete(TransactionLevel: Integer; const pError: IADOError;
                                 var adStatus: EventStatusEnum; const pConnection: IADOConnection); dispid 1;
    procedure CommitTransComplete(const pError: IADOError; var adStatus: EventStatusEnum;
                                  const pConnection: IADOConnection); dispid 3;
    procedure RollbackTransComplete(const pError: IADOError; var adStatus: EventStatusEnum;
                                    const pConnection: IADOConnection); dispid 2;
    procedure WillExecute(var Source: WideString; var CursorType: CursorTypeEnum;
                          var LockType: LockTypeEnum; var Options: Integer;
                          var adStatus: EventStatusEnum; const pCommand: IADOCommand;
                          const pRecordset: IADORecordset; const pConnection: IADOConnection); dispid 4;
    procedure ExecuteComplete(RecordsAffected: Integer; const pError: IADOError;
                              var adStatus: EventStatusEnum; const pCommand: IADOCommand;
                              const pRecordset: IADORecordset; const pConnection: IADOConnection); dispid 5;
    procedure WillConnect(var ConnectionString: WideString; var UserID: WideString;
                          var Password: WideString; var Options: Integer;
                          var adStatus: EventStatusEnum; const pConnection: IADOConnection); dispid 6;
    procedure ConnectComplete(const pError: IADOError; var adStatus: EventStatusEnum;
                              const pConnection: IADOConnection); dispid 7;
    procedure Disconnect(var adStatus: EventStatusEnum; const pConnection: IADOConnection); dispid 8;
  end;

// *********************************************************************//
// DispIntf:  IADORecordsetEvents
// Flags:     (4096) Dispatchable
// GUID:      {00000266-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADORecordsetEvents = dispinterface
    ['{00000266-0000-0010-8000-00AA006D2EA4}']
    procedure WillChangeField(cFields: Integer; Fields: OleVariant; var adStatus: EventStatusEnum;
                              const pRecordset: IADORecordset); dispid 9;
    procedure FieldChangeComplete(cFields: Integer; Fields: OleVariant; const pError: IADOError;
                                  var adStatus: EventStatusEnum; const pRecordset: IADORecordset); dispid 10;
    procedure WillChangeRecord(adReason: EventReasonEnum; cRecords: Integer;
                               var adStatus: EventStatusEnum; const pRecordset: IADORecordset); dispid 11;
    procedure RecordChangeComplete(adReason: EventReasonEnum; cRecords: Integer;
                                   const pError: IADOError; var adStatus: EventStatusEnum;
                                   const pRecordset: IADORecordset); dispid 12;
    procedure WillChangeRecordset(adReason: EventReasonEnum; var adStatus: EventStatusEnum;
                                  const pRecordset: IADORecordset); dispid 13;
    procedure RecordsetChangeComplete(adReason: EventReasonEnum; const pError: IADOError;
                                      var adStatus: EventStatusEnum; const pRecordset: IADORecordset); dispid 14;
    procedure WillMove(adReason: EventReasonEnum; var adStatus: EventStatusEnum;
                       const pRecordset: IADORecordset); dispid 15;
    procedure MoveComplete(adReason: EventReasonEnum; const pError: IADOError;
                           var adStatus: EventStatusEnum; const pRecordset: IADORecordset); dispid 16;
    procedure EndOfRecordset(var fMoreData: WordBool; var adStatus: EventStatusEnum;
                             const pRecordset: IADORecordset); dispid 17;
    procedure FetchProgress(Progress: Integer; MaxProgress: Integer; var adStatus: EventStatusEnum;
                            const pRecordset: IADORecordset); dispid 18;
    procedure FetchComplete(const pError: IADOError; var adStatus: EventStatusEnum;
                            const pRecordset: IADORecordset); dispid 19;
  end;

// *********************************************************************//
// Interface: IADOConnectionConstruction15
// Flags:     (512) Restricted
// GUID:      {00000516-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOConnectionConstruction15 = interface(IUnknown)
    ['{00000516-0000-0010-8000-00AA006D2EA4}']
    function Get_DSO(out ppDSO: IUnknown): HResult; stdcall;
    function Get_Session(out ppSession: IUnknown): HResult; stdcall;
    function WrapDSOandSession(const pDSO: IUnknown; const pSession: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IADOConnectionConstruction
// Flags:     (512) Restricted
// GUID:      {00000551-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOConnectionConstruction = interface(IADOConnectionConstruction15)
    ['{00000551-0000-0010-8000-00AA006D2EA4}']
  end;

// *********************************************************************//
// Interface: IADORecord
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {00000562-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADORecord = interface(IADO)
    ['{00000562-0000-0010-8000-00AA006D2EA4}']
    function Get_ActiveConnection: OleVariant; safecall;
    procedure _Set_ActiveConnection(const pvar: WideString); safecall;
    procedure Set_ActiveConnection(const pvar: IADOConnection); safecall;
    function Get_State: ObjectStateEnum; safecall;
    function Get_Source: OleVariant; safecall;
    procedure _Set_Source(const pvar: WideString); safecall;
    procedure Set_Source(const pvar: IDispatch); safecall;
    function Get_Mode: ConnectModeEnum; safecall;
    procedure Set_Mode(pMode: ConnectModeEnum); safecall;
    function Get_ParentURL: WideString; safecall;
    function MoveRecord(const Source: WideString; const Destination: WideString;
                        const UserName: WideString; const Password: WideString;
                        Options: MoveRecordOptionsEnum; Async: WordBool): WideString; safecall;
    function CopyRecord(const Source: WideString; const Destination: WideString;
                        const UserName: WideString; const Password: WideString;
                        Options: CopyRecordOptionsEnum; Async: WordBool): WideString; safecall;
    procedure DeleteRecord(const Source: WideString; Async: WordBool); safecall;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; Mode: ConnectModeEnum;
                   CreateOptions: RecordCreateOptionsEnum; Options: RecordOpenOptionsEnum;
                   const UserName: WideString; const Password: WideString); safecall;
    procedure Close; safecall;
    function Get_Fields: IADOFields; safecall;
    function Get_RecordType: RecordTypeEnum; safecall;
    function GetChildren: IADORecordset; safecall;
    procedure Cancel; safecall;
    property State: ObjectStateEnum read Get_State;
    property Mode: ConnectModeEnum read Get_Mode write Set_Mode;
    property ParentURL: WideString read Get_ParentURL;
    property Fields: IADOFields read Get_Fields;
    property RecordType: RecordTypeEnum read Get_RecordType;
  end;

// *********************************************************************//
// DispIntf:  IADORecordDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {00000562-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADORecordDisp = dispinterface
    ['{00000562-0000-0010-8000-00AA006D2EA4}']
    function ActiveConnection: OleVariant; dispid 1;
    property State: ObjectStateEnum readonly dispid 2;
    function Source: OleVariant; dispid 3;
    property Mode: ConnectModeEnum dispid 4;
    property ParentURL: WideString readonly dispid 5;
    function MoveRecord(const Source: WideString; const Destination: WideString;
                        const UserName: WideString; const Password: WideString;
                        Options: MoveRecordOptionsEnum; Async: WordBool): WideString; dispid 6;
    function CopyRecord(const Source: WideString; const Destination: WideString;
                        const UserName: WideString; const Password: WideString;
                        Options: CopyRecordOptionsEnum; Async: WordBool): WideString; dispid 7;
    procedure DeleteRecord(const Source: WideString; Async: WordBool); dispid 8;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; Mode: ConnectModeEnum;
                   CreateOptions: RecordCreateOptionsEnum; Options: RecordOpenOptionsEnum;
                   const UserName: WideString; const Password: WideString); dispid 9;
    procedure Close; dispid 10;
    property Fields: IADOFields readonly dispid 0;
    property RecordType: RecordTypeEnum readonly dispid 11;
    function GetChildren: IADORecordset; dispid 12;
    procedure Cancel; dispid 13;
    property Properties: IADOProperties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: IRecFields
// Flags:     (336) Hidden Dual OleAutomation
// GUID:      {00000563-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IRecFields = interface(IUnknown)
    ['{00000563-0000-0010-8000-00AA006D2EA4}']
    procedure ADOCheck; safecall;
  end;

// *********************************************************************//
// DispIntf:  IRecFieldsDisp
// Flags:     (336) Hidden Dual OleAutomation
// GUID:      {00000563-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IRecFieldsDisp = dispinterface
    ['{00000563-0000-0010-8000-00AA006D2EA4}']
    procedure ADOCheck; dispid 1;
  end;

// *********************************************************************//
// Interface: IADOStream
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {00000565-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOStream = interface(IDispatch)
    ['{00000565-0000-0010-8000-00AA006D2EA4}']
    function Get_Size: Integer; safecall;
    function Get_EOS: WordBool; safecall;
    function Get_Position: Integer; safecall;
    procedure Set_Position(pPos: Integer); safecall;
    function Get_Type_: StreamTypeEnum; safecall;
    procedure Set_Type_(ptype: StreamTypeEnum); safecall;
    function Get_LineSeparator: LineSeparatorEnum; safecall;
    procedure Set_LineSeparator(pLS: LineSeparatorEnum); safecall;
    function Get_State: ObjectStateEnum; safecall;
    function Get_Mode: ConnectModeEnum; safecall;
    procedure Set_Mode(pMode: ConnectModeEnum); safecall;
    function Get_Charset: WideString; safecall;
    procedure Set_Charset(const pbstrCharset: WideString); safecall;
    function Read(NumBytes: Integer): OleVariant; safecall;
    procedure Open(Source: OleVariant; Mode: ConnectModeEnum; Options: StreamOpenOptionsEnum; 
                   const UserName: WideString; const Password: WideString); safecall;
    procedure Close; safecall;
    procedure SkipLine; safecall;
    procedure Write(Buffer: OleVariant); safecall;
    procedure SetEOS; safecall;
    procedure CopyTo(const DestStream: IADOStream; CharNumber: Integer); safecall;
    procedure Flush; safecall;
    procedure SaveToFile(const FileName: WideString; Options: SaveOptionsEnum); safecall;
    procedure LoadFromFile(const FileName: WideString); safecall;
    function ReadText(NumChars: Integer): WideString; safecall;
    procedure WriteText(const Data: WideString; Options: StreamWriteEnum); safecall;
    procedure Cancel; safecall;
    property Size: Integer read Get_Size;
    property EOS: WordBool read Get_EOS;
    property Position: Integer read Get_Position write Set_Position;
    property Type_: StreamTypeEnum read Get_Type_ write Set_Type_;
    property LineSeparator: LineSeparatorEnum read Get_LineSeparator write Set_LineSeparator;
    property State: ObjectStateEnum read Get_State;
    property Mode: ConnectModeEnum read Get_Mode write Set_Mode;
    property Charset: WideString read Get_Charset write Set_Charset;
  end;

// *********************************************************************//
// DispIntf:  IADOStreamDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {00000565-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOStreamDisp = dispinterface
    ['{00000565-0000-0010-8000-00AA006D2EA4}']
    property Size: Integer readonly dispid 1;
    property EOS: WordBool readonly dispid 2;
    property Position: Integer dispid 3;
    property Type_: StreamTypeEnum dispid 4;
    property LineSeparator: LineSeparatorEnum dispid 5;
    property State: ObjectStateEnum readonly dispid 6;
    property Mode: ConnectModeEnum dispid 7;
    property Charset: WideString dispid 8;
    function Read(NumBytes: Integer): OleVariant; dispid 9;
    procedure Open(Source: OleVariant; Mode: ConnectModeEnum; Options: StreamOpenOptionsEnum;
                   const UserName: WideString; const Password: WideString); dispid 10;
    procedure Close; dispid 11;
    procedure SkipLine; dispid 12;
    procedure Write(Buffer: OleVariant); dispid 13;
    procedure SetEOS; dispid 14;
    procedure CopyTo(const DestStream: IADOStream; CharNumber: Integer); dispid 15;
    procedure Flush; dispid 16;
    procedure SaveToFile(const FileName: WideString; Options: SaveOptionsEnum); dispid 17;
    procedure LoadFromFile(const FileName: WideString); dispid 18;
    function ReadText(NumChars: Integer): WideString; dispid 19;
    procedure WriteText(const Data: WideString; Options: StreamWriteEnum); dispid 20;
    procedure Cancel; dispid 21;
  end;

// *********************************************************************//
// Interface: IADORecordConstruction
// Flags:     (4608) Restricted Dispatchable
// GUID:      {00000567-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADORecordConstruction = interface(IDispatch)
    ['{00000567-0000-0010-8000-00AA006D2EA4}']
    function Get_Row(out ppRow: IUnknown): HResult; stdcall;
    function Set_Row(const ppRow: IUnknown): HResult; stdcall;
    function Set_ParentRow(const Param1: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IADOStreamConstruction
// Flags:     (4608) Restricted Dispatchable
// GUID:      {00000568-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOStreamConstruction = interface(IDispatch)
    ['{00000568-0000-0010-8000-00AA006D2EA4}']
    function Get_Stream(out ppStm: IUnknown): HResult; stdcall;
    function Set_Stream(const ppStm: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IADOCommandConstruction
// Flags:     (512) Restricted
// GUID:      {00000517-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOCommandConstruction = interface(IUnknown)
    ['{00000517-0000-0010-8000-00AA006D2EA4}']
    function Get_OLEDBCommand(out ppOLEDBCommand: IUnknown): HResult; stdcall;
    function Set_OLEDBCommand(const ppOLEDBCommand: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IADORecordsetConstruction
// Flags:     (4608) Restricted Dispatchable
// GUID:      {00000283-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADORecordsetConstruction = interface(IDispatch)
    ['{00000283-0000-0010-8000-00AA006D2EA4}']
    function Get_Rowset(out ppRowset: IUnknown): HResult; stdcall;
    function Set_Rowset(const ppRowset: IUnknown): HResult; stdcall;
    function Get_Chapter(out plChapter: Integer): HResult; stdcall;
    function Set_Chapter(plChapter: Integer): HResult; stdcall;
    function Get_RowPosition(out ppRowPos: IUnknown): HResult; stdcall;
    function Set_RowPosition(const ppRowPos: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IADOField15
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000505-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOField15 = interface(IADO)
    ['{00000505-0000-0010-8000-00AA006D2EA4}']
    function Get_ActualSize: Integer; safecall;
    function Get_Attributes: Integer; safecall;
    function Get_DefinedSize: Integer; safecall;
    function Get_Name: WideString; safecall;
    function Get_Type_: DataTypeEnum; safecall;
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(pvar: OleVariant); safecall;
    function Get_Precision: Byte; safecall;
    function Get_NumericScale: Byte; safecall;
    procedure AppendChunk(Data: OleVariant); safecall;
    function GetChunk(Length: Integer): OleVariant; safecall;
    function Get_OriginalValue: OleVariant; safecall;
    function Get_UnderlyingValue: OleVariant; safecall;
    property ActualSize: Integer read Get_ActualSize;
    property Attributes: Integer read Get_Attributes;
    property DefinedSize: Integer read Get_DefinedSize;
    property Name: WideString read Get_Name;
    property Type_: DataTypeEnum read Get_Type_;
    property Value: OleVariant read Get_Value write Set_Value;
    property Precision: Byte read Get_Precision;
    property NumericScale: Byte read Get_NumericScale;
    property OriginalValue: OleVariant read Get_OriginalValue;
    property UnderlyingValue: OleVariant read Get_UnderlyingValue;
  end;

// *********************************************************************//
// DispIntf:  IADOField15Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000505-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOField15Disp = dispinterface
    ['{00000505-0000-0010-8000-00AA006D2EA4}']
    property ActualSize: Integer readonly dispid 1109;
    property Attributes: Integer readonly dispid 1114;
    property DefinedSize: Integer readonly dispid 1103;
    property Name: WideString readonly dispid 1100;
    property Type_: DataTypeEnum readonly dispid 1102;
    property Value: OleVariant dispid 0;
    property Precision: Byte readonly dispid 1112;
    property NumericScale: Byte readonly dispid 1113;
    procedure AppendChunk(Data: OleVariant); dispid 1107;
    function GetChunk(Length: Integer): OleVariant; dispid 1108;
    property OriginalValue: OleVariant readonly dispid 1104;
    property UnderlyingValue: OleVariant readonly dispid 1105;
    property Properties: IADOProperties readonly dispid 500;
  end;

  CoConnection = class
    class function Create: IADOConnection;
    class function CreateRemote(const MachineName: string): IADOConnection;
  end;

  CoRecord_ = class
    class function Create: IADORecord;
    class function CreateRemote(const MachineName: string): IADORecord;
  end;

  CoStream = class
    class function Create: IADOStream;
    class function CreateRemote(const MachineName: string): IADOStream;
  end;

  CoCommand = class
    class function Create: IADOCommand;
    class function CreateRemote(const MachineName: string): IADOCommand;
  end;

  CoRecordset = class
    class function Create: IADORecordset;
    class function CreateRemote(const MachineName: string): IADORecordset;
  end;

  CoParameter = class
    class function Create: IADOParameter;
    class function CreateRemote(const MachineName: string): IADOParameter;
  end;

implementation

uses ComObj;

class function CoConnection.Create: IADOConnection;
begin
  Result := CreateComObject(CLASS_Connection) as IADOConnection;
end;

class function CoConnection.CreateRemote(const MachineName: string): IADOConnection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Connection) as IADOConnection;
end;

class function CoRecord_.Create: IADORecord;
begin
  Result := CreateComObject(CLASS_Record) as IADORecord;
end;

class function CoRecord_.CreateRemote(const MachineName: string): IADORecord;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Record) as IADORecord;
end;

class function CoStream.Create: IADOStream;
begin
  Result := CreateComObject(CLASS_Stream) as IADOStream;
end;

class function CoStream.CreateRemote(const MachineName: string): IADOStream;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Stream) as IADOStream;
end;

class function CoCommand.Create: IADOCommand;
begin
  Result := CreateComObject(CLASS_Command) as IADOCommand;
end;

class function CoCommand.CreateRemote(const MachineName: string): IADOCommand;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Command) as IADOCommand;
end;

class function CoRecordset.Create: IADORecordset;
begin
  Result := CreateComObject(CLASS_Recordset) as IADORecordset;
end;

class function CoRecordset.CreateRemote(const MachineName: string): IADORecordset;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Recordset) as IADORecordset;
end;

class function CoParameter.Create: IADOParameter;
begin
  Result := CreateComObject(CLASS_Parameter) as IADOParameter;
end;

class function CoParameter.CreateRemote(const MachineName: string): IADOParameter;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Parameter) as IADOParameter;
end;

end.
