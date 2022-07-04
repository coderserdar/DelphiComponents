unit DaoApi;
interface
const
{ RecordsetTypeEnum }

  dbOpenTable = 1;
  dbOpenDynaset = 2;
  dbOpenSnapshot = 4;
  dbOpenForwardOnly = 8;
  dbOpenDynamic = 16;

{ EditModeEnum }

  dbEditNone = 0;
  dbEditInProgress = 1;
  dbEditAdd = 2;

{ RecordsetOptionEnum }

  dbDenyWrite = 1;
  dbDenyRead = 2;
  dbReadOnly = 4;
  dbAppendOnly = 8;
  dbInconsistent = 16;
  dbConsistent = 32;
  dbSQLPassThrough = 64;
  dbFailOnError = 128;
  dbForwardOnly = 256;
  dbSeeChanges = 512;
  dbRunAsync = 1024;
  dbExecDirect = 2048;

{ LockTypeEnum }

  dbPessimistic = 2;
  dbOptimistic = 3;
  dbOptimisticValue = 1;
  dbOptimisticBatch = 5;

{ UpdateCriteriaEnum }

  dbCriteriaKey = 1;
  dbCriteriaModValues = 2;
  dbCriteriaAllCols = 4;
  dbCriteriaTimestamp = 8;
  dbCriteriaDeleteInsert = 16;
  dbCriteriaUpdate = 32;

{ FieldAttributeEnum }

  dbFixedField = 1;
  dbVariableField = 2;
  dbAutoIncrField = 16;
  dbUpdatableField = 32;
  dbSystemField = 8192;
  dbHyperlinkField = 32768;
  dbDescending = 1;

{ DataTypeEnum }

  dbBoolean        = 1;
  dbByte           = 2;
  dbInteger        = 3;
  dbLong           = 4;
  dbAutoIncInteger = dbLong*10000;
  dbCurrency       = 5;
  dbSingle         = 6;
  dbDouble         = 7;
  dbDate           = 8;
  dbBinary         = 9;
  dbText           = 10;
  dbLongBinary     = 11;
  dbMemo           = 12;
  dbGUID           = 15;
  dbBigInt         = 16;
  dbVarBinary      = 17;
  dbChar           = 18;
  dbNumeric        = 19;
  dbDecimal        = 20;
  dbFloat          = 21;
  dbTime           = 22;
  dbTimeStamp      = 23;

  dbAttachment     = 101;
  dbComplexByte    = 102;
  dbComplexInteger = 103;
  dbComplexLong    = 104;
  dbComplexSingle  = 105;
  dbComplexDouble  = 106;
  dbComplexGUID    = 107;
  dbComplexDecimal = 108;
  dbComplexText    = 109;

  dbUnspecifyed    = 1024;



{ RelationAttributeEnum }

  dbRelationUnique = 1;
  dbRelationDontEnforce = 2;
  dbRelationInherited = 4;
  dbRelationUpdateCascade = 256;
  dbRelationDeleteCascade = 4096;
  dbRelationLeft = 16777216;
  dbRelationRight = 33554432;

{ TableDefAttributeEnum }

  dbAttachExclusive = 65536;
  dbAttachSavePWD = 131072;
  dbSystemObject = -2147483646;
  dbAttachedTable = 1073741824;
  dbAttachedODBC = 536870912;
  dbHiddenObject = 1;

{ QueryDefTypeEnum }

  dbQSelect = 0;
  dbQProcedure = 224;
  dbQAction = 240;
  dbQCrosstab = 16;
  dbQDelete = 32;
  dbQUpdate = 48;
  dbQAppend = 64;
  dbQMakeTable = 80;
  dbQDDL = 96;
  dbQSQLPassThrough = 112;
  dbQSetOperation = 128;
  dbQSPTBulk = 144;
  dbQCompound = 160;

{ QueryDefStateEnum }

  dbQPrepare = 1;
  dbQUnprepare = 2;

{ DatabaseTypeEnum }

  dbVersion10  = 1;
  dbEncrypt    = 2;
  dbDecrypt    = 4;
  dbVersion11  = 8;
  dbVersion20  = 16;
  dbVersion30  = 32;
  dbVersion40  = 64;
  dbVersion120 = 128;

{ CollatingOrderEnum }

  dbSortNeutral = 1024;
  dbSortArabic = 1025;
  dbSortCyrillic = 1049;
  dbSortCzech = 1029;
  dbSortDutch = 1043;
  dbSortGeneral = 1033;
  dbSortGreek = 1032;
  dbSortHebrew = 1037;
  dbSortHungarian = 1038;
  dbSortIcelandic = 1039;
  dbSortNorwdan = 1030;
  dbSortPDXIntl = 1033;
  dbSortPDXNor = 1030;
  dbSortPDXSwe = 1053;
  dbSortPolish = 1045;
  dbSortSpanish = 1034;
  dbSortSwedFin = 1053;
  dbSortTurkish = 1055;
  dbSortJapanese = 1041;
  dbSortChineseSimplified = 2052;
  dbSortChineseTraditional = 1028;
  dbSortKorean = 1042;
  dbSortThai = 1054;
  dbSortSlovenian = 1060;
  dbSortUndefined = -1;

{ IdleEnum }

  dbFreeLocks = 1;
  dbRefreshCache = 8;

{ PermissionEnum }

  dbSecNoAccess = 0;
  dbSecFullAccess = 1048575;
  dbSecDelete = 65536;
  dbSecReadSec = 131072;
  dbSecWriteSec = 262144;
  dbSecWriteOwner = 524288;
  dbSecDBCreate = 1;
  dbSecDBOpen = 2;
  dbSecDBExclusive = 4;
  dbSecDBAdmin = 8;
  dbSecCreate = 1;
  dbSecReadDef = 4;
  dbSecWriteDef = 65548;
  dbSecRetrieveData = 20;
  dbSecInsertData = 32;
  dbSecReplaceData = 64;
  dbSecDeleteData = 128;

{ SynchronizeTypeEnum }

  dbRepExportChanges = 1;
  dbRepImportChanges = 2;
  dbRepImpExpChanges = 4;
  dbRepSyncInternet = 16;

{ ReplicaTypeEnum }

  dbRepMakeReadOnly = 2;
  dbRepMakePartial = 1;

{ WorkspaceTypeEnum }

  dbUseODBC = 1;
  dbUseJet = 2;

{ CursorDriverEnum }

  dbUseDefaultCursor = -1;
  dbUseODBCCursor = 1;
  dbUseServerCursor = 2;
  dbUseClientBatchCursor = 3;
  dbUseNoCursor = 4;

{ DriverPromptEnum }

  dbDriverPrompt = 2;
  dbDriverNoPrompt = 1;
  dbDriverComplete = 0;
  dbDriverCompleteRequired = 3;

{ SetOptionEnum }

  dbPageTimeout = 6;
  dbLockRetry = 57;
  dbMaxBufferSize = 8;
  dbUserCommitSync = 58;
  dbImplicitCommitSync = 59;
  dbExclusiveAsyncDelay = 60;
  dbSharedAsyncDelay = 61;
  dbMaxLocksPerFile = 62;
  dbLockDelay = 63;
  dbRecycleLVs = 65;
  dbFlushTransactionTimeout = 66;

{ ParameterDirectionEnum }

  dbParamInput = 1;
  dbParamOutput = 2;
  dbParamInputOutput = 3;
  dbParamReturnValue = 4;

{ UpdateTypeEnum }

  dbUpdateBatch = 4;
  dbUpdateRegular = 1;
  dbUpdateCurrentRecord = 2;

{ RecordStatusEnum }

  dbRecordUnmodified = 0;
  dbRecordModified = 1;
  dbRecordNew = 2;
  dbRecordDeleted = 3;
  dbRecordDBDeleted = 4;

{ CommitTransOptionsEnum }

  dbForceOSFlush = 1;

{ _DAOSuppHelp }

  LogMessages = 0;
  KeepLocal = 0;
  Replicable = 0;
  ReplicableBool = 0;
  V1xNullBehavior = 0;

{ _PredefinedLanguages}
  {
  //****************************************************************************
  dbSortNeutral $00000400
  dbSortArabic $00000401
  dbSortCyrillic $00000419
  dbSortCzech $00000405
  dbSortDutch $00000413
  dbSortGeneral $00000409
  dbSortGreek $00000408
  dbSortHebrew $0000040D
  dbSortHungarian $0000040E
  dbSortIcelandic $0000040F
  dbSortNorwDan $00000406
  dbSortPDXIntl $00000409
  dbSortPDXNor $00000406
  dbSortPDXSwe $0000041D
  dbSortPolish $00000415
  dbSortSpanish $0000040A
  dbSortSwedFin $0000041D
  dbSortTurkish $0000041F
  dbSortJapanese $00000411
  dbSortChineseSimplified $00000804
  dbSortChineseTraditional $00000404
  dbSortKorean $00000412
  dbSortThai $0000041E
  dbSortSlovenian $00000424
  dbSortUndefined $FFFFFFFF
  //****************************************************************************
  }

  dbLangNeutral = ';LANGID=0x0400;CP=1024;COUNTRY=0';
  dbLangArabic = ';LANGID=0x0401;CP=1256;COUNTRY=0';
  dbLangCzech = ';LANGID=0x0405;CP=1250;COUNTRY=0';
  dbLangDutch = ';LANGID=0x0413;CP=1252;COUNTRY=0';
  dbLangGeneral = ';LANGID=0x0409;CP=1252;COUNTRY=0';
  dbLangGreek = ';LANGID=0x0408;CP=1253;COUNTRY=0';
  dbLangHebrew = ';LANGID=0x040D;CP=1255;COUNTRY=0';
  dbLangHungarian = ';LANGID=0x040E;CP=1250;COUNTRY=0';
  dbLangIcelandic = ';LANGID=0x040F;CP=1252;COUNTRY=0';
  dbLangNordic = ';LANGID=0x041D;CP=1252;COUNTRY=0';
  dbLangNorwDan = ';LANGID=0x0414;CP=1252;COUNTRY=0';
  dbLangPolish = ';LANGID=0x0415;CP=1250;COUNTRY=0';
  dbLangCyrillic = ';LANGID=0x0419;CP=1251;COUNTRY=0';
  dbLangSpanish = ';LANGID=0x040A;CP=1252;COUNTRY=0';
  dbLangSwedFin = ';LANGID=0x040B;CP=1252;COUNTRY=0';
  dbLangTurkish = ';LANGID=0x041F;CP=1254;COUNTRY=0';
  dbLangJapanese = ';LANGID=0x0411;CP=932;COUNTRY=0';
  dbLangChineseSimplified = ';LANGID=0x0804;CP=936;COUNTRY=0';
  dbLangChineseTraditional = ';LANGID=0x0404;CP=950;COUNTRY=0';
  dbLangKorean = ';LANGID=0x040C;CP=494;COUNTRY=0';
  dbLangThai = ';LANGID=0x101E;CP=874;COUNTRY=0';
  dbLangSlovenian = ';LANGID=0x0424;CP=1250;COUNTRY=0';



{ _PredefinedTableTypes}
  dBase_50_Table        = 'dBase 5.0;DATABASE=%s';     {Drive:\Path}
  dBase_III_Table       = 'dBase III;DATABASE=%s';     {Drive:\Path}
  dBase_IV_Table        = 'dBase IV;DATABASE=%s';      {Drive:\Path}
  Excel_30_Table        = 'Excel 3.0;DATABASE=%s';     {Drive:\Path\Filename.xls}
  Excel_40_Table        = 'Excel 4.0;DATABASE=%s';     {Drive:\Path\Filename.xls}
  Excel_50_Table        = 'Excel 5.0;DATABASE=%s';     {Drive:\Path\Filename.xls}
  Excel_80_Table        = 'Excel 8.0;DATABASE=%s';     {Drive:\Path\Filename.xls}
  Excel_120_BinaryTable = 'Excel 12.0;DATABASE=%s';    {Drive:\Path\Filename.xls}
  Excel_120_Table       = 'Excel 12.0 Xml;DATABASE=%s';    {Drive:\Path\Filename.xls}
  Exchange_40_Table     = 'Exchange 4.0;MAPILEVEL=$s'; {Brrrrrrrrrr}
  HTML_Import_Table     = 'HTML Import;DATABASE=%s';   {URL}
  Jet_Table             = ';DATABASE=%s';              {Drive:\Path\Filename.mdb}
  Jet_2x_Table          = ';DATABASE=%s';              {Drive:\Path\Filename.mdb}
  Jet_3x_Table          = ';DATABASE=%s';              {Drive:\Path\Filename.mdb}
  Lotus_WK1_Table       = 'Lotus WK1;DATABASE=%s';     {Drive:\Path\Filename.wk1}
  Lotus_WK3_Table       = 'Lotus WK3;DATABASE=%s';     {Drive:\Path\Filename.wk3}
  Lotus_WK4_Table       = 'Lotus WK4;DATABASE=%s';     {Drive:\Path\Filename.wk4}
  FoxPro_20_Table       = 'FoxPro 2.0;DATABASE=%s';    {Drive:\Path}
  FoxPro_25_Table       = 'FoxPro 2.5;DATABASE=%s';    {Drive:\Path}
  FoxPro_26_Table       = 'FoxPro 2.6;DATABASE=%s';    {Drive:\Path}
  FoxPro_30_Table       = 'FoxPro 3.0;DATABASE=%s';    {Drive:\Path}
  FoxPro_DBC_Table      = 'FoxPro DBC;DATABASE=%s';    {Drive:\Path\FileName.dbc}
  Paradox_3X_Table      = 'Paradox 3.X;DATABASE=%s';   {Drive:\Path}
  Paradox_4X_Table      = 'Paradox 4.X;DATABASE=%s';   {Drive:\Path}
  Paradox_5X_Table      = 'Paradox 5.X;DATABASE=%s';   {Drive:\Path}
  Paradox_7X_Table      = 'Paradox 7.X;DATABASE=%s';   {Drive:\Path}
  Text_Table            = 'Text;DATABASE=%s';          {Drive:\Path}
  ODBC_Table            = 'ODBC;DATABASE=%s;UID=%s;PWD=%s;DSN=%s';
  ODBC_Table_Prompt    =  'ODBC;';

implementation

end.
