unit DAO351Api;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ Microsoft DAO 3.51 Object Library }
{ Version 4.0 }

{ Conversion log:
  Warning: 'Property' is a reserved word. Property changed to Property_
  Warning: LanguageConstants: Modules are not supported.
  Warning: 'Object' is a reserved word. Parameter 'Object' in _DynaCollection.Append changed to 'Object_'
  Warning: 'Type' is a reserved word. Workspace.Type changed to Type_
  Warning: 'Type' is a reserved word. Parameter 'Type' in _TableDef.OpenRecordset changed to 'Type_'
  Warning: 'Type' is a reserved word. Parameter 'Type' in _TableDef.CreateField changed to 'Type_'
  Warning: 'Type' is a reserved word. Parameter 'Type' in _TableDef.CreateProperty changed to 'Type_'
  Warning: 'Type' is a reserved word. Parameter 'Type' in Database._30_OpenRecordset changed to 'Type_'
  Warning: 'Type' is a reserved word. Parameter 'Type' in Database.CreateProperty changed to 'Type_'
  Warning: 'Type' is a reserved word. Parameter 'Type' in Database.OpenRecordset changed to 'Type_'
  Warning: 'Type' is a reserved word. _QueryDef.Type changed to Type_
  Warning: 'Type' is a reserved word. Parameter 'Type' in _QueryDef._30_OpenRecordset changed to 'Type_'
  Warning: 'Type' is a reserved word. Parameter 'Type' in _QueryDef._30__OpenRecordset changed to 'Type_'
  Warning: 'Type' is a reserved word. Parameter 'Type' in _QueryDef.CreateProperty changed to 'Type_'
  Warning: 'Type' is a reserved word. Parameter 'Type' in _QueryDef.OpenRecordset changed to 'Type_'
  Warning: 'Type' is a reserved word. Parameter 'Type' in _QueryDef._OpenRecordset changed to 'Type_'
  Warning: 'Type' is a reserved word. Recordset.Type changed to Type_
  Warning: 'Type' is a reserved word. Parameter 'Type' in Recordset.OpenRecordset changed to 'Type_'
  Warning: 'Type' is a reserved word. _Field.Type changed to Type_
  Warning: 'Type' is a reserved word. Parameter 'Type' in _Field.CreateProperty changed to 'Type_'
  Warning: 'Type' is a reserved word. Parameter 'Type' in _Index.CreateField changed to 'Type_'
  Warning: 'Type' is a reserved word. Parameter 'Type' in _Index.CreateProperty changed to 'Type_'
  Warning: 'Type' is a reserved word. Parameter.Type changed to Type_
  Warning: 'Type' is a reserved word. Parameter 'Type' in _Relation.CreateField changed to 'Type_'
  Warning: 'Type' is a reserved word. Property_.Type changed to Type_
  Warning: 'Inherited' is a reserved word. Property_.Inherited changed to Inherited_
  Warning: 'Type' is a reserved word. Parameter 'Type' in Document.CreateProperty changed to 'Type_'
  Warning: 'Type' is a reserved word. Parameter 'Type' in Connection.OpenRecordset changed to 'Type_'
  Warning: Recordset.Bookmark return parameter of type PSafeArray was written as OleVariant
  Warning: Recordset.LastModified return parameter of type PSafeArray was written as OleVariant
  Warning: Recordset.CacheStart return parameter of type PSafeArray was written as OleVariant
 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_DAO: TGUID = '{00025E01-0000-0000-C000-000000000046}';

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
  dbEditChanged = 4;
  dbEditDeleted = 8;
  dbEditNew = 16;

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

  dbBoolean = 1;
  dbByte = 2;
  dbInteger = 3;
  dbLong = 4;
  dbCurrency = 5;
  dbSingle = 6;
  dbDouble = 7;
  dbDate = 8;
  dbBinary = 9;
  dbText = 10;
  dbLongBinary = 11;
  dbMemo = 12;
  dbGUID = 15;
  dbBigInt = 16;
  dbVarBinary = 17;
  dbChar = 18;
  dbNumeric = 19;
  dbDecimal = 20;
  dbFloat = 21;
  dbTime = 22;
  dbTimeStamp = 23;

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

  dbVersion10 = 1;
  dbEncrypt = 2;
  dbDecrypt = 4;
  dbVersion11 = 8;
  dbVersion20 = 16;
  dbVersion30 = 32;

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

const

{ Component class GUIDs }
  Class_DBEngine: TGUID = '{00000010-0000-0010-8000-00AA006D2EA4}';
  Class_PrivDBEngine: TGUID = '{00000011-0000-0010-8000-00AA006D2EA4}';
  Class_TableDef: TGUID = '{00000013-0000-0010-8000-00AA006D2EA4}';
  Class_QueryDef: TGUID = '{00000018-0000-0010-8000-00AA006D2EA4}';
  Class_Field: TGUID = '{00000014-0000-0010-8000-00AA006D2EA4}';
  Class_Index: TGUID = '{00000015-0000-0010-8000-00AA006D2EA4}';
  Class_User: TGUID = '{00000017-0000-0010-8000-00AA006D2EA4}';
  Class_Group: TGUID = '{00000016-0000-0010-8000-00AA006D2EA4}';
  Class_Relation: TGUID = '{00000019-0000-0010-8000-00AA006D2EA4}';

type

{ Forward declarations: Interfaces }
  _Collection = interface;
  _CollectionDisp = dispinterface;
  _DynaCollection = interface;
  _DynaCollectionDisp = dispinterface;
  _DAO = interface;
  _DAODisp = dispinterface;
  _DBEngine = interface;
  _DBEngineDisp = dispinterface;
  Error = interface;
  ErrorDisp = dispinterface;
  Errors = interface;
  ErrorsDisp = dispinterface;
  Workspace = interface;
  WorkspaceDisp = dispinterface;
  Workspaces = interface;
  WorkspacesDisp = dispinterface;
  _TableDef = interface;
  _TableDefDisp = dispinterface;
  TableDefs = interface;
  TableDefsDisp = dispinterface;
  Database = interface;
  DatabaseDisp = dispinterface;
  Databases = interface;
  DatabasesDisp = dispinterface;
  _QueryDef = interface;
  _QueryDefDisp = dispinterface;
  QueryDefs = interface;
  QueryDefsDisp = dispinterface;
  Recordset = interface;
  RecordsetDisp = dispinterface;
  Recordsets = interface;
  RecordsetsDisp = dispinterface;
  _Field = interface;
  _FieldDisp = dispinterface;
  Fields = interface;
  FieldsDisp = dispinterface;
  _Index = interface;
  _IndexDisp = dispinterface;
  Indexes = interface;
  IndexesDisp = dispinterface;
  IndexFields = interface;
  IndexFieldsDisp = dispinterface;
  Parameter = interface;
  ParameterDisp = dispinterface;
  Parameters = interface;
  ParametersDisp = dispinterface;
  _User = interface;
  _UserDisp = dispinterface;
  Users = interface;
  UsersDisp = dispinterface;
  _Group = interface;
  _GroupDisp = dispinterface;
  Groups = interface;
  GroupsDisp = dispinterface;
  _Relation = interface;
  _RelationDisp = dispinterface;
  Relations = interface;
  RelationsDisp = dispinterface;
  Property_ = interface;
  Property_Disp = dispinterface;
  Properties = interface;
  PropertiesDisp = dispinterface;
  Container = interface;
  ContainerDisp = dispinterface;
  Containers = interface;
  ContainersDisp = dispinterface;
  Document = interface;
  DocumentDisp = dispinterface;
  Documents = interface;
  DocumentsDisp = dispinterface;
  Connection = interface;
  ConnectionDisp = dispinterface;
  Connections = interface;
  ConnectionsDisp = dispinterface;

{ Forward declarations: CoClasses }
  DBEngine = _DBEngine;
  PrivDBEngine = _DBEngine;
  TableDef = _TableDef;
  QueryDef = _QueryDef;
  Field = _Field;
  Index = _Index;
  User = _User;
  Group = _Group;
  Relation = _Relation;

{ Forward declarations: Enums }
  RecordsetTypeEnum = TOleEnum;
  EditModeEnum = TOleEnum;
  RecordsetOptionEnum = TOleEnum;
  LockTypeEnum = TOleEnum;
  UpdateCriteriaEnum = TOleEnum;
  FieldAttributeEnum = TOleEnum;
  DataTypeEnum = TOleEnum;
  RelationAttributeEnum = TOleEnum;
  TableDefAttributeEnum = TOleEnum;
  QueryDefTypeEnum = TOleEnum;
  QueryDefStateEnum = TOleEnum;
  DatabaseTypeEnum = TOleEnum;
  CollatingOrderEnum = TOleEnum;
  IdleEnum = TOleEnum;
  PermissionEnum = TOleEnum;
  SynchronizeTypeEnum = TOleEnum;
  ReplicaTypeEnum = TOleEnum;
  WorkspaceTypeEnum = TOleEnum;
  CursorDriverEnum = TOleEnum;
  DriverPromptEnum = TOleEnum;
  SetOptionEnum = TOleEnum;
  ParameterDirectionEnum = TOleEnum;
  UpdateTypeEnum = TOleEnum;
  RecordStatusEnum = TOleEnum;
  CommitTransOptionsEnum = TOleEnum;
  _DAOSuppHelp = TOleEnum;

  _Collection = interface(IDispatch)
    ['{000000A0-0000-0010-8000-00AA006D2EA4}']
    function Get_Count: Smallint; safecall;
    function _NewEnum: IUnknown; safecall;
    procedure Refresh; safecall;
    property Count: Smallint read Get_Count;
  end;

{ DispInterface declaration for Dual Interface _Collection }

  _CollectionDisp = dispinterface
    ['{000000A0-0000-0010-8000-00AA006D2EA4}']
    property Count: Smallint readonly dispid 1610743808;
    procedure Refresh; dispid 1610743810;
  end;

  _DynaCollection = interface(_Collection)
    ['{000000A2-0000-0010-8000-00AA006D2EA4}']
    procedure Append(Object_: IDispatch); safecall;
    procedure Delete(const Name: WideString); safecall;
  end;

{ DispInterface declaration for Dual Interface _DynaCollection }

  _DynaCollectionDisp = dispinterface
    ['{000000A2-0000-0010-8000-00AA006D2EA4}']
    procedure Append(Object_: IDispatch); dispid 1610809344;
    procedure Delete(const Name: WideString); dispid 1610809345;
  end;

  _DAO = interface(IDispatch)
    ['{0000000A-0000-0010-8000-00AA006D2EA4}']
    function Get_Properties: Properties; safecall;
    property Properties: Properties read Get_Properties;
  end;

{ DispInterface declaration for Dual Interface _DAO }

  _DAODisp = dispinterface
    ['{0000000A-0000-0010-8000-00AA006D2EA4}']
    property Properties: Properties readonly dispid 10;
  end;

{ The Microsoft Jet database engine. }

  _DBEngine = interface(_DAO)
    ['{00000021-0000-0010-8000-00AA006D2EA4}']
    function Get_Version: WideString; safecall;
    function Get_IniPath: WideString; safecall;
    procedure Set_IniPath(const Value: WideString); safecall;
    procedure Set_DefaultUser(const Value: WideString); safecall;
    procedure Set_DefaultPassword(const Value: WideString); safecall;
    function Get_LoginTimeout: Smallint; safecall;
    procedure Set_LoginTimeout(Value: Smallint); safecall;
    function Get_Workspaces: Workspaces; safecall;
    function Get_Errors: Errors; safecall;
    procedure Idle(Action: OleVariant); safecall;
    procedure CompactDatabase(const SrcName, DstName: WideString; DstLocale, Options, SrcLocale: OleVariant); safecall;
    procedure RepairDatabase(const Name: WideString); safecall;
    procedure RegisterDatabase(const Dsn, Driver: WideString; Silent: WordBool; const Attributes: WideString); safecall;
    function _30_CreateWorkspace(const Name, UserName, Password: WideString): Workspace; safecall;
    function OpenDatabase(const Name: WideString; Options, ReadOnly, Connect: OleVariant): Database; safecall;
    function CreateDatabase(const Name, Locale: WideString; Option: OleVariant): Database; safecall;
    procedure FreeLocks; safecall;
    procedure BeginTrans; safecall;
    procedure CommitTrans(Option: Integer); safecall;
    procedure Rollback; safecall;
    procedure SetDefaultWorkspace(const Name, Password: WideString); safecall;
    procedure SetDataAccessOption(Option: Smallint; Value: OleVariant); safecall;
    function ISAMStats(StatNum: Integer; Reset: OleVariant): Integer; safecall;
    function Get_SystemDB: WideString; safecall;
    procedure Set_SystemDB(const Value: WideString); safecall;
    function CreateWorkspace(const Name, UserName, Password: WideString; UseType: OleVariant): Workspace; safecall;
    function OpenConnection(const Name: WideString; Options, ReadOnly, Connect: OleVariant): Connection; safecall;
    function Get_DefaultType: Integer; safecall;
    procedure Set_DefaultType(Value: Integer); safecall;
    procedure SetOption(Option: Integer; Value: OleVariant); safecall;
    property Version: WideString read Get_Version;
    property IniPath: WideString read Get_IniPath write Set_IniPath;
    property DefaultUser: WideString write Set_DefaultUser;
    property DefaultPassword: WideString write Set_DefaultPassword;
    property LoginTimeout: Smallint read Get_LoginTimeout write Set_LoginTimeout;
    property Workspaces: Workspaces read Get_Workspaces;
    property Errors: Errors read Get_Errors;
    property SystemDB: WideString read Get_SystemDB write Set_SystemDB;
    property DefaultType: Integer read Get_DefaultType write Set_DefaultType;
  end;

{ DispInterface declaration for Dual Interface _DBEngine }

  _DBEngineDisp = dispinterface
    ['{00000021-0000-0010-8000-00AA006D2EA4}']
    property Version: WideString readonly dispid 1610809344;
    property IniPath: WideString dispid 1610809345;
    property DefaultUser: WideString writeonly dispid 1610809347;
    property DefaultPassword: WideString writeonly dispid 1610809348;
    property LoginTimeout: Smallint dispid 1610809349;
    property Workspaces: Workspaces readonly dispid 0;
    property Errors: Errors readonly dispid 1610809352;
    procedure Idle(Action: OleVariant); dispid 1610809353;
    procedure CompactDatabase(const SrcName, DstName: WideString; DstLocale, Options, SrcLocale: OleVariant); dispid 1610809354;
    procedure RepairDatabase(const Name: WideString); dispid 1610809355;
    procedure RegisterDatabase(const Dsn, Driver: WideString; Silent: WordBool; const Attributes: WideString); dispid 1610809356;
    function OpenDatabase(const Name: WideString; Options, ReadOnly, Connect: OleVariant): Database; dispid 1610809358;
    function CreateDatabase(const Name, Locale: WideString; Option: OleVariant): Database; dispid 1610809359;
    procedure BeginTrans; dispid 1610809361;
    procedure CommitTrans(Option: Integer); dispid 1610809362;
    procedure Rollback; dispid 1610809363;
    function ISAMStats(StatNum: Integer; Reset: OleVariant): Integer; dispid 1610809366;
    property SystemDB: WideString dispid 1610809367;
    function CreateWorkspace(const Name, UserName, Password: WideString; UseType: OleVariant): Workspace; dispid 1610809369;
    function OpenConnection(const Name: WideString; Options, ReadOnly, Connect: OleVariant): Connection; dispid 1610809370;
    property DefaultType: Integer dispid 1610809371;
    procedure SetOption(Option: Integer; Value: OleVariant); dispid 1610809373;
  end;

{ Information about any errors that occurred with a DAO object. }

  Error = interface(IDispatch)
    ['{00000023-0000-0010-8000-00AA006D2EA4}']
    function Get_Number: Integer; safecall;
    function Get_Source: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_HelpContext: Integer; safecall;
    property Number: Integer read Get_Number;
    property Source: WideString read Get_Source;
    property Description: WideString read Get_Description;
    property HelpFile: WideString read Get_HelpFile;
    property HelpContext: Integer read Get_HelpContext;
  end;

{ DispInterface declaration for Dual Interface Error }

  ErrorDisp = dispinterface
    ['{00000023-0000-0010-8000-00AA006D2EA4}']
    property Number: Integer readonly dispid 1610743808;
    property Source: WideString readonly dispid 1610743809;
    property Description: WideString readonly dispid 0;
    property HelpFile: WideString readonly dispid 1610743811;
    property HelpContext: Integer readonly dispid 1610743812;
  end;

{ Collection of Error objects. }

  Errors = interface(_Collection)
    ['{00000025-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Error; safecall;
    property Item[Index: OleVariant]: Error read Get_Item; default;
  end;

{ DispInterface declaration for Dual Interface Errors }

  ErrorsDisp = dispinterface
    ['{00000025-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Error readonly dispid 0; default;
  end;

{ A session for a user. }

  Workspace = interface(_DAO)
    ['{00000039-0000-0010-8000-00AA006D2EA4}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_UserName: WideString; safecall;
    procedure Set__30_UserName(const Value: WideString); safecall;
    procedure Set__30_Password(const Value: WideString); safecall;
    function Get_IsolateODBCTrans: Smallint; safecall;
    procedure Set_IsolateODBCTrans(Value: Smallint); safecall;
    function Get_Databases: Databases; safecall;
    function Get_Users: Users; safecall;
    function Get_Groups: Groups; safecall;
    procedure BeginTrans; safecall;
    procedure CommitTrans(Options: Integer); safecall;
    procedure Close; safecall;
    procedure Rollback; safecall;
    function OpenDatabase(const Name: WideString; Options, ReadOnly, Connect: OleVariant): Database; safecall;
    function CreateDatabase(const Name, Connect: WideString; Option: OleVariant): Database; safecall;
    function CreateUser(Name, PID, Password: OleVariant): User; safecall;
    function CreateGroup(Name, PID: OleVariant): Group; safecall;
    function OpenConnection(const Name: WideString; Options, ReadOnly, Connect: OleVariant): Connection; safecall;
    function Get_LoginTimeout: Integer; safecall;
    procedure Set_LoginTimeout(Value: Integer); safecall;
    function Get_DefaultCursorDriver: Integer; safecall;
    procedure Set_DefaultCursorDriver(Value: Integer); safecall;
    function Get_hEnv: Integer; safecall;
    function Get_Type_: Integer; safecall;
    function Get_Connections: Connections; safecall;
    property Name: WideString read Get_Name write Set_Name;
    property UserName: WideString read Get_UserName;
    property _30_UserName: WideString write Set__30_UserName;
    property _30_Password: WideString write Set__30_Password;
    property IsolateODBCTrans: Smallint read Get_IsolateODBCTrans write Set_IsolateODBCTrans;
    property Databases: Databases read Get_Databases;
    property Users: Users read Get_Users;
    property Groups: Groups read Get_Groups;
    property LoginTimeout: Integer read Get_LoginTimeout write Set_LoginTimeout;
    property DefaultCursorDriver: Integer read Get_DefaultCursorDriver write Set_DefaultCursorDriver;
    property hEnv: Integer read Get_hEnv;
    property Type_: Integer read Get_Type_;
    property Connections: Connections read Get_Connections;
  end;

{ DispInterface declaration for Dual Interface Workspace }

  WorkspaceDisp = dispinterface
    ['{00000039-0000-0010-8000-00AA006D2EA4}']
    property Name: WideString dispid 1610809344;
    property UserName: WideString readonly dispid 1610809346;
    property IsolateODBCTrans: Smallint dispid 1610809349;
    property Databases: Databases readonly dispid 0;
    property Users: Users readonly dispid 1610809352;
    property Groups: Groups readonly dispid 1610809353;
    procedure BeginTrans; dispid 1610809354;
    procedure CommitTrans(Options: Integer); dispid 1610809355;
    procedure Close; dispid 1610809356;
    procedure Rollback; dispid 1610809357;
    function OpenDatabase(const Name: WideString; Options, ReadOnly, Connect: OleVariant): Database; dispid 1610809358;
    function CreateDatabase(const Name, Connect: WideString; Option: OleVariant): Database; dispid 1610809359;
    function CreateUser(Name, PID, Password: OleVariant): User; dispid 1610809360;
    function CreateGroup(Name, PID: OleVariant): Group; dispid 1610809361;
    function OpenConnection(const Name: WideString; Options, ReadOnly, Connect: OleVariant): Connection; dispid 1610809362;
    property LoginTimeout: Integer dispid 1610809363;
    property DefaultCursorDriver: Integer dispid 1610809365;
    property hEnv: Integer readonly dispid 1610809367;
    property Type_: Integer readonly dispid 1610809368;
    property Connections: Connections readonly dispid 1610809369;
  end;

{ Collection of Workspace objects. }

  Workspaces = interface(_DynaCollection)
    ['{0000003B-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Workspace; safecall;
    property Item[Index: OleVariant]: Workspace read Get_Item; default;
  end;

{ DispInterface declaration for Dual Interface Workspaces }

  WorkspacesDisp = dispinterface
    ['{0000003B-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Workspace readonly dispid 0; default;
  end;

{ A saved table definition. }

  _TableDef = interface(_DAO)
    ['{00000049-0000-0010-8000-00AA006D2EA4}']
    function Get_Attributes: Integer; safecall;
    procedure Set_Attributes(Value: Integer); safecall;
    function Get_Connect: WideString; safecall;
    procedure Set_Connect(const Value: WideString); safecall;
    function Get_DateCreated: OleVariant; safecall;
    function Get_LastUpdated: OleVariant; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_SourceTableName: WideString; safecall;
    procedure Set_SourceTableName(const Value: WideString); safecall;
    function Get_Updatable: WordBool; safecall;
    function Get_ValidationText: WideString; safecall;
    procedure Set_ValidationText(const Value: WideString); safecall;
    function Get_ValidationRule: WideString; safecall;
    procedure Set_ValidationRule(const Value: WideString); safecall;
    function Get_RecordCount: Integer; safecall;
    function Get_Fields: Fields; safecall;
    function Get_Indexes: Indexes; safecall;
    function OpenRecordset(Type_, Options: OleVariant): Recordset; safecall;
    procedure RefreshLink; safecall;
    function CreateField(Name, Type_, Size: OleVariant): Field; safecall;
    function CreateIndex(Name: OleVariant): Index; safecall;
    function CreateProperty(Name, Type_, Value, DDL: OleVariant): Property_; safecall;
    function Get_ConflictTable: WideString; safecall;
    function Get_ReplicaFilter: OleVariant; safecall;
    procedure Set_ReplicaFilter(Value: OleVariant); safecall;
    property Attributes: Integer read Get_Attributes write Set_Attributes;
    property Connect: WideString read Get_Connect write Set_Connect;
    property DateCreated: OleVariant read Get_DateCreated;
    property LastUpdated: OleVariant read Get_LastUpdated;
    property Name: WideString read Get_Name write Set_Name;
    property SourceTableName: WideString read Get_SourceTableName write Set_SourceTableName;
    property Updatable: WordBool read Get_Updatable;
    property ValidationText: WideString read Get_ValidationText write Set_ValidationText;
    property ValidationRule: WideString read Get_ValidationRule write Set_ValidationRule;
    property RecordCount: Integer read Get_RecordCount;
    property Fields: Fields read Get_Fields;
    property Indexes: Indexes read Get_Indexes;
    property ConflictTable: WideString read Get_ConflictTable;
    property ReplicaFilter: OleVariant read Get_ReplicaFilter write Set_ReplicaFilter;
  end;

{ DispInterface declaration for Dual Interface _TableDef }

  _TableDefDisp = dispinterface
    ['{00000049-0000-0010-8000-00AA006D2EA4}']
    property Attributes: Integer dispid 1610809344;
    property Connect: WideString dispid 1610809346;
    property DateCreated: OleVariant readonly dispid 1610809348;
    property LastUpdated: OleVariant readonly dispid 1610809349;
    property Name: WideString dispid 1610809350;
    property SourceTableName: WideString dispid 1610809352;
    property Updatable: WordBool readonly dispid 1610809354;
    property ValidationText: WideString dispid 1610809355;
    property ValidationRule: WideString dispid 1610809357;
    property RecordCount: Integer readonly dispid 1610809359;
    property Fields: Fields readonly dispid 0;
    property Indexes: Indexes readonly dispid 1610809361;
    function OpenRecordset(Type_, Options: OleVariant): Recordset; dispid 1610809362;
    procedure RefreshLink; dispid 1610809363;
    function CreateField(Name, Type_, Size: OleVariant): Field; dispid 1610809364;
    function CreateIndex(Name: OleVariant): Index; dispid 1610809365;
    function CreateProperty(Name, Type_, Value, DDL: OleVariant): Property_; dispid 1610809366;
    property ConflictTable: WideString readonly dispid 1610809367;
    property ReplicaFilter: OleVariant dispid 1610809368;
  end;

{ Collection of TableDef objects. }

  TableDefs = interface(_DynaCollection)
    ['{0000004B-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): TableDef; safecall;
    property Item[Index: OleVariant]: TableDef read Get_Item; default;
  end;

{ DispInterface declaration for Dual Interface TableDefs }

  TableDefsDisp = dispinterface
    ['{0000004B-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: TableDef readonly dispid 0; default;
  end;

{ An open database. }

  Database = interface(_DAO)
    ['{00000071-0000-0010-8000-00AA006D2EA4}']
    function Get_CollatingOrder: Integer; safecall;
    function Get_Connect: WideString; safecall;
    function Get_Name: WideString; safecall;
    function Get_QueryTimeout: Smallint; safecall;
    procedure Set_QueryTimeout(Value: Smallint); safecall;
    function Get_Transactions: WordBool; safecall;
    function Get_Updatable: WordBool; safecall;
    function Get_Version: WideString; safecall;
    function Get_RecordsAffected: Integer; safecall;
    function Get_TableDefs: TableDefs; safecall;
    function Get_QueryDefs: QueryDefs; safecall;
    function Get_Relations: Relations; safecall;
    function Get_Containers: Containers; safecall;
    function Get_Recordsets: Recordsets; safecall;
    procedure Close; safecall;
    procedure Execute(const Query: WideString; Options: OleVariant); safecall;
    function _30_OpenRecordset(const Name: WideString; Type_, Options: OleVariant): Recordset; safecall;
    function CreateProperty(Name, Type_, Value, DDL: OleVariant): Property_; safecall;
    function CreateRelation(Name, Table, ForeignTable, Attributes: OleVariant): Relation; safecall;
    function CreateTableDef(Name, Attributes, SourceTableName, Connect: OleVariant): TableDef; safecall;
    procedure BeginTrans; safecall;
    procedure CommitTrans(Options: Integer); safecall;
    procedure Rollback; safecall;
    function CreateDynaset(const Name: WideString; Options, Inconsistent: OleVariant): Recordset; safecall;
    function CreateQueryDef(Name, SQLText: OleVariant): QueryDef; safecall;
    function CreateSnapshot(const Source: WideString; Options: OleVariant): Recordset; safecall;
    procedure DeleteQueryDef(const Name: WideString); safecall;
    function ExecuteSQL(const SQL: WideString): Integer; safecall;
    function ListFields(const Name: WideString): Recordset; safecall;
    function ListTables: Recordset; safecall;
    function OpenQueryDef(const Name: WideString): QueryDef; safecall;
    function OpenTable(const Name: WideString; Options: OleVariant): Recordset; safecall;
    function Get_ReplicaID: WideString; safecall;
    function Get_DesignMasterID: WideString; safecall;
    procedure Set_DesignMasterID(const Value: WideString); safecall;
    procedure Synchronize(const DbPathName: WideString; ExchangeType: OleVariant); safecall;
    procedure MakeReplica(const PathName, Description: WideString; Options: OleVariant); safecall;
    procedure Set_Connect(const Value: WideString); safecall;
    procedure NewPassword(const bstrOld, bstrNew: WideString); safecall;
    function OpenRecordset(const Name: WideString; Type_, Options, LockEdit: OleVariant): Recordset; safecall;
    function Get_Connection: Connection; safecall;
    procedure PopulatePartial(const DbPathName: WideString); safecall;
    property CollatingOrder: Integer read Get_CollatingOrder;
    property Connect: WideString read Get_Connect write Set_Connect;
    property Name: WideString read Get_Name;
    property QueryTimeout: Smallint read Get_QueryTimeout write Set_QueryTimeout;
    property Transactions: WordBool read Get_Transactions;
    property Updatable: WordBool read Get_Updatable;
    property Version: WideString read Get_Version;
    property RecordsAffected: Integer read Get_RecordsAffected;
    property TableDefs: TableDefs read Get_TableDefs;
    property QueryDefs: QueryDefs read Get_QueryDefs;
    property Relations: Relations read Get_Relations;
    property Containers: Containers read Get_Containers;
    property Recordsets: Recordsets read Get_Recordsets;
    property ReplicaID: WideString read Get_ReplicaID;
    property DesignMasterID: WideString read Get_DesignMasterID write Set_DesignMasterID;
    property Connection: Connection read Get_Connection;
  end;

{ DispInterface declaration for Dual Interface Database }

  DatabaseDisp = dispinterface
    ['{00000071-0000-0010-8000-00AA006D2EA4}']
    property CollatingOrder: Integer readonly dispid 1610809344;
    property Connect: WideString dispid 1610809345;
    property Name: WideString readonly dispid 1610809346;
    property QueryTimeout: Smallint dispid 1610809347;
    property Transactions: WordBool readonly dispid 1610809349;
    property Updatable: WordBool readonly dispid 1610809350;
    property Version: WideString readonly dispid 1610809351;
    property RecordsAffected: Integer readonly dispid 1610809352;
    property TableDefs: TableDefs readonly dispid 0;
    property QueryDefs: QueryDefs readonly dispid 1610809354;
    property Relations: Relations readonly dispid 1610809355;
    property Containers: Containers readonly dispid 1610809356;
    property Recordsets: Recordsets readonly dispid 1610809357;
    procedure Close; dispid 1610809358;
    procedure Execute(const Query: WideString; Options: OleVariant); dispid 1610809359;
    function CreateProperty(Name, Type_, Value, DDL: OleVariant): Property_; dispid 1610809361;
    function CreateRelation(Name, Table, ForeignTable, Attributes: OleVariant): Relation; dispid 1610809362;
    function CreateTableDef(Name, Attributes, SourceTableName, Connect: OleVariant): TableDef; dispid 1610809363;
    function CreateQueryDef(Name, SQLText: OleVariant): QueryDef; dispid 1610809368;
    property ReplicaID: WideString readonly dispid 1610809376;
    property DesignMasterID: WideString dispid 1610809377;
    procedure Synchronize(const DbPathName: WideString; ExchangeType: OleVariant); dispid 1610809379;
    procedure MakeReplica(const PathName, Description: WideString; Options: OleVariant); dispid 1610809380;
    procedure NewPassword(const bstrOld, bstrNew: WideString); dispid 1610809382;
    function OpenRecordset(const Name: WideString; Type_, Options, LockEdit: OleVariant): Recordset; dispid 1610809383;
    property Connection: Connection readonly dispid 1610809384;
    procedure PopulatePartial(const DbPathName: WideString); dispid 1610809385;
  end;

{ Collection of Database objects. }

  Databases = interface(_Collection)
    ['{00000073-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Database; safecall;
    property Item[Index: OleVariant]: Database read Get_Item; default;
  end;

{ DispInterface declaration for Dual Interface Databases }

  DatabasesDisp = dispinterface
    ['{00000073-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Database readonly dispid 0; default;
  end;

{ A saved query definition. }

  _QueryDef = interface(_DAO)
    ['{00000079-0000-0010-8000-00AA006D2EA4}']
    function Get_DateCreated: OleVariant; safecall;
    function Get_LastUpdated: OleVariant; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_ODBCTimeout: Smallint; safecall;
    procedure Set_ODBCTimeout(Value: Smallint); safecall;
    function Get_Type_: Smallint; safecall;
    function Get_SQL: WideString; safecall;
    procedure Set_SQL(const Value: WideString); safecall;
    function Get_Updatable: WordBool; safecall;
    function Get_Connect: WideString; safecall;
    procedure Set_Connect(const Value: WideString); safecall;
    function Get_ReturnsRecords: WordBool; safecall;
    procedure Set_ReturnsRecords(Value: WordBool); safecall;
    function Get_RecordsAffected: Integer; safecall;
    function Get_Fields: Fields; safecall;
    function Get_Parameters: Parameters; safecall;
    procedure Close; safecall;
    function _30_OpenRecordset(Type_, Options: OleVariant): Recordset; safecall;
    function _30__OpenRecordset(Type_, Options: OleVariant): Recordset; safecall;
    function _Copy: QueryDef; safecall;
    procedure Execute(Options: OleVariant); safecall;
    procedure Compare(const pQdef: QueryDef; var lps: Smallint); safecall;
    function CreateDynaset(Options, Inconsistent: OleVariant): Recordset; safecall;
    function CreateSnapshot(Options: OleVariant): Recordset; safecall;
    function ListParameters: Recordset; safecall;
    function CreateProperty(Name, Type_, Value, DDL: OleVariant): Property_; safecall;
    function OpenRecordset(Type_, Options, LockEdit: OleVariant): Recordset; safecall;
    function _OpenRecordset(Type_, Options, LockEdit: OleVariant): Recordset; safecall;
    procedure Cancel; safecall;
    function Get_hStmt: Integer; safecall;
    function Get_MaxRecords: Integer; safecall;
    procedure Set_MaxRecords(Value: Integer); safecall;
    function Get_StillExecuting: WordBool; safecall;
    function Get_CacheSize: Integer; safecall;
    procedure Set_CacheSize(Value: Integer); safecall;
    function Get_Prepare: OleVariant; safecall;
    procedure Set_Prepare(Value: OleVariant); safecall;
    property DateCreated: OleVariant read Get_DateCreated;
    property LastUpdated: OleVariant read Get_LastUpdated;
    property Name: WideString read Get_Name write Set_Name;
    property ODBCTimeout: Smallint read Get_ODBCTimeout write Set_ODBCTimeout;
    property Type_: Smallint read Get_Type_;
    property SQL: WideString read Get_SQL write Set_SQL;
    property Updatable: WordBool read Get_Updatable;
    property Connect: WideString read Get_Connect write Set_Connect;
    property ReturnsRecords: WordBool read Get_ReturnsRecords write Set_ReturnsRecords;
    property RecordsAffected: Integer read Get_RecordsAffected;
    property Fields: Fields read Get_Fields;
    property Parameters: Parameters read Get_Parameters;
    property hStmt: Integer read Get_hStmt;
    property MaxRecords: Integer read Get_MaxRecords write Set_MaxRecords;
    property StillExecuting: WordBool read Get_StillExecuting;
    property CacheSize: Integer read Get_CacheSize write Set_CacheSize;
    property Prepare: OleVariant read Get_Prepare write Set_Prepare;
  end;

{ DispInterface declaration for Dual Interface _QueryDef }

  _QueryDefDisp = dispinterface
    ['{00000079-0000-0010-8000-00AA006D2EA4}']
    property DateCreated: OleVariant readonly dispid 1610809344;
    property LastUpdated: OleVariant readonly dispid 1610809345;
    property Name: WideString dispid 1610809346;
    property ODBCTimeout: Smallint dispid 1610809348;
    property Type_: Smallint readonly dispid 1610809350;
    property SQL: WideString dispid 1610809351;
    property Updatable: WordBool readonly dispid 1610809353;
    property Connect: WideString dispid 1610809354;
    property ReturnsRecords: WordBool dispid 1610809356;
    property RecordsAffected: Integer readonly dispid 1610809358;
    property Fields: Fields readonly dispid 1610809359;
    property Parameters: Parameters readonly dispid 0;
    procedure Close; dispid 1610809361;
    procedure Execute(Options: OleVariant); dispid 1610809365;
    function CreateProperty(Name, Type_, Value, DDL: OleVariant): Property_; dispid 1610809370;
    function OpenRecordset(Type_, Options, LockEdit: OleVariant): Recordset; dispid 1610809371;
    procedure Cancel; dispid 1610809373;
    property hStmt: Integer readonly dispid 1610809374;
    property MaxRecords: Integer dispid 1610809375;
    property StillExecuting: WordBool readonly dispid 1610809377;
    property CacheSize: Integer dispid 1610809378;
    property Prepare: OleVariant dispid 1610809380;
  end;

{ Collection of QueryDef objects. }

  QueryDefs = interface(_DynaCollection)
    ['{0000007B-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): QueryDef; safecall;
    property Item[Index: OleVariant]: QueryDef read Get_Item; default;
  end;

{ DispInterface declaration for Dual Interface QueryDefs }

  QueryDefsDisp = dispinterface
    ['{0000007B-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: QueryDef readonly dispid 0; default;
  end;

{ A representation of the records in a base table or the records that result from  }

  Recordset = interface(_DAO)
    ['{00000031-0000-0010-8000-00AA006D2EA4}']
    function Get_BOF: WordBool; safecall;
    function Get_Bookmark: PSafeArray; safecall;
    procedure Set_Bookmark(Value: PSafeArray); safecall;
    function Get_Bookmarkable: WordBool; safecall;
    function Get_DateCreated: OleVariant; safecall;
    function Get_EOF: WordBool; safecall;
    function Get_Filter: WideString; safecall;
    procedure Set_Filter(const Value: WideString); safecall;
    function Get_Index: WideString; safecall;
    procedure Set_Index(const Value: WideString); safecall;
    function Get_LastModified: PSafeArray; safecall;
    function Get_LastUpdated: OleVariant; safecall;
    function Get_LockEdits: WordBool; safecall;
    procedure Set_LockEdits(Value: WordBool); safecall;
    function Get_Name: WideString; safecall;
    function Get_NoMatch: WordBool; safecall;
    function Get_Sort: WideString; safecall;
    procedure Set_Sort(const Value: WideString); safecall;
    function Get_Transactions: WordBool; safecall;
    function Get_Type_: Smallint; safecall;
    function Get_RecordCount: Integer; safecall;
    function Get_Updatable: WordBool; safecall;
    function Get_Restartable: WordBool; safecall;
    function Get_ValidationText: WideString; safecall;
    function Get_ValidationRule: WideString; safecall;
    function Get_CacheStart: PSafeArray; safecall;
    procedure Set_CacheStart(Value: PSafeArray); safecall;
    function Get_CacheSize: Integer; safecall;
    procedure Set_CacheSize(Value: Integer); safecall;
    function Get_PercentPosition: Single; safecall;
    procedure Set_PercentPosition(Value: Single); safecall;
    function Get_AbsolutePosition: Integer; safecall;
    procedure Set_AbsolutePosition(Value: Integer); safecall;
    function Get_EditMode: Smallint; safecall;
    function Get_ODBCFetchCount: Integer; safecall;
    function Get_ODBCFetchDelay: Integer; safecall;
    function Get_Parent: Database; safecall;
    function Get_Fields: Fields; safecall;
    function Get_Indexes: Indexes; safecall;
    procedure _30_CancelUpdate; safecall;
    procedure AddNew; safecall;
    procedure Close; safecall;
    function OpenRecordset(Type_, Options: OleVariant): Recordset; safecall;
    procedure Delete; safecall;
    procedure Edit; safecall;
    procedure FindFirst(const Criteria: WideString); safecall;
    procedure FindLast(const Criteria: WideString); safecall;
    procedure FindNext(const Criteria: WideString); safecall;
    procedure FindPrevious(const Criteria: WideString); safecall;
    procedure MoveFirst; safecall;
    procedure _30_MoveLast; safecall;
    procedure MoveNext; safecall;
    procedure MovePrevious; safecall;
    procedure Seek(const Comparison: WideString; Key1, Key2, Key3, Key4, Key5, Key6, Key7, Key8, Key9, Key10, Key11, Key12, Key13: OleVariant); safecall;
    procedure _30_Update; safecall;
    function Clone: Recordset; safecall;
    procedure Requery(NewQueryDef: OleVariant); safecall;
    procedure Move(Rows: Integer; StartBookmark: OleVariant); safecall;
    procedure FillCache(Rows, StartBookmark: OleVariant); safecall;
    function CreateDynaset(Options, Inconsistent: OleVariant): Recordset; safecall;
    function CreateSnapshot(Options: OleVariant): Recordset; safecall;
    function CopyQueryDef: QueryDef; safecall;
    function ListFields: Recordset; safecall;
    function ListIndexes: Recordset; safecall;
    function GetRows(NumRows: OleVariant): OleVariant; safecall;
    function Get_Collect(Index: OleVariant): OleVariant; safecall;
    procedure Set_Collect(Index: OleVariant; Value: OleVariant); safecall;
    procedure Cancel; safecall;
    function NextRecordset: WordBool; safecall;
    function Get_hStmt: Integer; safecall;
    function Get_StillExecuting: WordBool; safecall;
    function Get_BatchSize: Integer; safecall;
    procedure Set_BatchSize(Value: Integer); safecall;
    function Get_BatchCollisionCount: Integer; safecall;
    function Get_BatchCollisions: OleVariant; safecall;
    function Get_Connection: Connection; safecall;
    procedure Set_Connection(Value: Connection); safecall;
    function Get_RecordStatus: Smallint; safecall;
    function Get_UpdateOptions: Integer; safecall;
    procedure Set_UpdateOptions(Value: Integer); safecall;
    procedure CancelUpdate(UpdateType: Integer); safecall;
    procedure Update(UpdateType: Integer; Force: WordBool); safecall;
    procedure MoveLast(Options: Integer); safecall;
    property BOF: WordBool read Get_BOF;
    property Bookmark: PSafeArray read Get_Bookmark write Set_Bookmark;
    property Bookmarkable: WordBool read Get_Bookmarkable;
    property DateCreated: OleVariant read Get_DateCreated;
    property EOF: WordBool read Get_EOF;
    property Filter: WideString read Get_Filter write Set_Filter;
    property Index: WideString read Get_Index write Set_Index;
    property LastModified: PSafeArray read Get_LastModified;
    property LastUpdated: OleVariant read Get_LastUpdated;
    property LockEdits: WordBool read Get_LockEdits write Set_LockEdits;
    property Name: WideString read Get_Name;
    property NoMatch: WordBool read Get_NoMatch;
    property Sort: WideString read Get_Sort write Set_Sort;
    property Transactions: WordBool read Get_Transactions;
    property Type_: Smallint read Get_Type_;
    property RecordCount: Integer read Get_RecordCount;
    property Updatable: WordBool read Get_Updatable;
    property Restartable: WordBool read Get_Restartable;
    property ValidationText: WideString read Get_ValidationText;
    property ValidationRule: WideString read Get_ValidationRule;
    property CacheStart: PSafeArray read Get_CacheStart write Set_CacheStart;
    property CacheSize: Integer read Get_CacheSize write Set_CacheSize;
    property PercentPosition: Single read Get_PercentPosition write Set_PercentPosition;
    property AbsolutePosition: Integer read Get_AbsolutePosition write Set_AbsolutePosition;
    property EditMode: Smallint read Get_EditMode;
    property ODBCFetchCount: Integer read Get_ODBCFetchCount;
    property ODBCFetchDelay: Integer read Get_ODBCFetchDelay;
    property Parent: Database read Get_Parent;
    property Fields: Fields read Get_Fields;
    property Indexes: Indexes read Get_Indexes;
    property Collect[Index: OleVariant]: OleVariant read Get_Collect write Set_Collect;
    property hStmt: Integer read Get_hStmt;
    property StillExecuting: WordBool read Get_StillExecuting;
    property BatchSize: Integer read Get_BatchSize write Set_BatchSize;
    property BatchCollisionCount: Integer read Get_BatchCollisionCount;
    property BatchCollisions: OleVariant read Get_BatchCollisions;
    property Connection: Connection read Get_Connection write Set_Connection;
    property RecordStatus: Smallint read Get_RecordStatus;
    property UpdateOptions: Integer read Get_UpdateOptions write Set_UpdateOptions;
  end;

{ DispInterface declaration for Dual Interface Recordset }

  RecordsetDisp = dispinterface
    ['{00000031-0000-0010-8000-00AA006D2EA4}']
    property BOF: WordBool readonly dispid 101;
    property Bookmark: OleVariant dispid 102;
    property Bookmarkable: WordBool readonly dispid 103;
    property DateCreated: OleVariant readonly dispid 104;
    property EOF: WordBool readonly dispid 105;
    property Filter: WideString dispid 106;
    property Index: WideString dispid 107;
    property LastModified: OleVariant readonly dispid 108;
    property LastUpdated: OleVariant readonly dispid 109;
    property LockEdits: WordBool dispid 110;
    property Name: WideString readonly dispid 111;
    property NoMatch: WordBool readonly dispid 112;
    property Sort: WideString dispid 113;
    property Transactions: WordBool readonly dispid 114;
    property Type_: Smallint readonly dispid 115;
    property RecordCount: Integer readonly dispid 116;
    property Updatable: WordBool readonly dispid 117;
    property Restartable: WordBool readonly dispid 118;
    property ValidationText: WideString readonly dispid 119;
    property ValidationRule: WideString readonly dispid 120;
    property CacheStart: OleVariant dispid 121;
    property CacheSize: Integer dispid 122;
    property PercentPosition: Single dispid 123;
    property AbsolutePosition: Integer dispid 124;
    property EditMode: Smallint readonly dispid 125;
    property ODBCFetchCount: Integer readonly dispid 126;
    property ODBCFetchDelay: Integer readonly dispid 127;
    property Parent: Database readonly dispid 128;
    property Fields: Fields readonly dispid 0;
    procedure AddNew; dispid 132;
    procedure Close; dispid 133;
    function OpenRecordset(Type_, Options: OleVariant): Recordset; dispid 134;
    procedure Delete; dispid 135;
    procedure Edit; dispid 136;
    procedure FindFirst(const Criteria: WideString); dispid 137;
    procedure FindLast(const Criteria: WideString); dispid 138;
    procedure FindNext(const Criteria: WideString); dispid 139;
    procedure FindPrevious(const Criteria: WideString); dispid 140;
    procedure MoveFirst; dispid 141;
    procedure MoveNext; dispid 143;
    procedure MovePrevious; dispid 144;
    procedure Seek(const Comparison: WideString; Key1, Key2, Key3, Key4, Key5, Key6, Key7, Key8, Key9, Key10, Key11, Key12, Key13: OleVariant); dispid 145;
    function Clone: Recordset; dispid 147;
    procedure Requery(NewQueryDef: OleVariant); dispid 148;
    procedure Move(Rows: Integer; StartBookmark: OleVariant); dispid 149;
    procedure FillCache(Rows, StartBookmark: OleVariant); dispid 150;
    function CopyQueryDef: QueryDef; dispid 153;
    function GetRows(NumRows: OleVariant): OleVariant; dispid 156;
    property Collect[Index: OleVariant]: OleVariant dispid -8;
    procedure Cancel; dispid 157;
    function NextRecordset: WordBool; dispid 158;
    property hStmt: Integer readonly dispid 159;
    property StillExecuting: WordBool readonly dispid 160;
    property BatchSize: Integer dispid 161;
    property BatchCollisionCount: Integer readonly dispid 162;
    property BatchCollisions: OleVariant readonly dispid 163;
    property Connection: Connection dispid 164;
    property RecordStatus: Smallint readonly dispid 165;
    property UpdateOptions: Integer dispid 166;
    procedure CancelUpdate(UpdateType: Integer); dispid 167;
    procedure Update(UpdateType: Integer; Force: WordBool); dispid 168;
    procedure MoveLast(Options: Integer); dispid 169;
  end;

{ Collection of Recordset objects. }

  Recordsets = interface(_Collection)
    ['{00000033-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Recordset; safecall;
    property Item[Index: OleVariant]: Recordset read Get_Item; default;
  end;

{ DispInterface declaration for Dual Interface Recordsets }

  RecordsetsDisp = dispinterface
    ['{00000033-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Recordset readonly dispid 0; default;
  end;

{ A column that is part of a TableDef, QueryDef, Index, Relation, or Recordset. }

  _Field = interface(_DAO)
    ['{00000051-0000-0010-8000-00AA006D2EA4}']
    function Get_CollatingOrder: Integer; safecall;
    function Get_Type_: Smallint; safecall;
    procedure Set_Type_(Value: Smallint); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Size: Integer; safecall;
    procedure Set_Size(Value: Integer); safecall;
    function Get_SourceField: WideString; safecall;
    function Get_SourceTable: WideString; safecall;
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(Value: OleVariant); safecall;
    function Get_Attributes: Integer; safecall;
    procedure Set_Attributes(Value: Integer); safecall;
    function Get_OrdinalPosition: Smallint; safecall;
    procedure Set_OrdinalPosition(Value: Smallint); safecall;
    function Get_ValidationText: WideString; safecall;
    procedure Set_ValidationText(const Value: WideString); safecall;
    function Get_ValidateOnSet: WordBool; safecall;
    procedure Set_ValidateOnSet(Value: WordBool); safecall;
    function Get_ValidationRule: WideString; safecall;
    procedure Set_ValidationRule(const Value: WideString); safecall;
    function Get_DefaultValue: OleVariant; safecall;
    procedure Set_DefaultValue(Value: OleVariant); safecall;
    function Get_Required: WordBool; safecall;
    procedure Set_Required(Value: WordBool); safecall;
    function Get_AllowZeroLength: WordBool; safecall;
    procedure Set_AllowZeroLength(Value: WordBool); safecall;
    function Get_DataUpdatable: WordBool; safecall;
    function Get_ForeignName: WideString; safecall;
    procedure Set_ForeignName(const Value: WideString); safecall;
    procedure AppendChunk(Val: OleVariant); safecall;
    function GetChunk(Offset, Bytes: Integer): OleVariant; safecall;
    function _30_FieldSize: Integer; safecall;
    function CreateProperty(Name, Type_, Value, DDL: OleVariant): Property_; safecall;
    function Get_CollectionIndex: Smallint; safecall;
    function Get_OriginalValue: OleVariant; safecall;
    function Get_VisibleValue: OleVariant; safecall;
    function Get_FieldSize: Integer; safecall;
    property CollatingOrder: Integer read Get_CollatingOrder;
    property Type_: Smallint read Get_Type_ write Set_Type_;
    property Name: WideString read Get_Name write Set_Name;
    property Size: Integer read Get_Size write Set_Size;
    property SourceField: WideString read Get_SourceField;
    property SourceTable: WideString read Get_SourceTable;
    property Value: OleVariant read Get_Value write Set_Value;
    property Attributes: Integer read Get_Attributes write Set_Attributes;
    property OrdinalPosition: Smallint read Get_OrdinalPosition write Set_OrdinalPosition;
    property ValidationText: WideString read Get_ValidationText write Set_ValidationText;
    property ValidateOnSet: WordBool read Get_ValidateOnSet write Set_ValidateOnSet;
    property ValidationRule: WideString read Get_ValidationRule write Set_ValidationRule;
    property DefaultValue: OleVariant read Get_DefaultValue write Set_DefaultValue;
    property Required: WordBool read Get_Required write Set_Required;
    property AllowZeroLength: WordBool read Get_AllowZeroLength write Set_AllowZeroLength;
    property DataUpdatable: WordBool read Get_DataUpdatable;
    property ForeignName: WideString read Get_ForeignName write Set_ForeignName;
    property CollectionIndex: Smallint read Get_CollectionIndex;
    property OriginalValue: OleVariant read Get_OriginalValue;
    property VisibleValue: OleVariant read Get_VisibleValue;
    property FieldSize: Integer read Get_FieldSize;
  end;

{ DispInterface declaration for Dual Interface _Field }

  _FieldDisp = dispinterface
    ['{00000051-0000-0010-8000-00AA006D2EA4}']
    property CollatingOrder: Integer readonly dispid 1610809344;
    property Type_: Smallint dispid 1610809345;
    property Name: WideString dispid 1610809347;
    property Size: Integer dispid 1610809349;
    property SourceField: WideString readonly dispid 1610809351;
    property SourceTable: WideString readonly dispid 1610809352;
    property Value: OleVariant dispid 0;
    property Attributes: Integer dispid 1610809355;
    property OrdinalPosition: Smallint dispid 1610809357;
    property ValidationText: WideString dispid 1610809359;
    property ValidateOnSet: WordBool dispid 1610809361;
    property ValidationRule: WideString dispid 1610809363;
    property DefaultValue: OleVariant dispid 1610809365;
    property Required: WordBool dispid 1610809367;
    property AllowZeroLength: WordBool dispid 1610809369;
    property DataUpdatable: WordBool readonly dispid 1610809371;
    property ForeignName: WideString dispid 1610809372;
    procedure AppendChunk(Val: OleVariant); dispid 1610809374;
    function GetChunk(Offset, Bytes: Integer): OleVariant; dispid 1610809375;
    function CreateProperty(Name, Type_, Value, DDL: OleVariant): Property_; dispid 1610809377;
    property CollectionIndex: Smallint readonly dispid 1610809378;
    property OriginalValue: OleVariant readonly dispid 1610809379;
    property VisibleValue: OleVariant readonly dispid 1610809380;
    property FieldSize: Integer readonly dispid 1610809381;
  end;

{ Collection of Field objects. }

  Fields = interface(_DynaCollection)
    ['{00000053-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Field; safecall;
    property Item[Index: OleVariant]: Field read Get_Item; default;
  end;

{ DispInterface declaration for Dual Interface Fields }

  FieldsDisp = dispinterface
    ['{00000053-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Field readonly dispid 0; default;
  end;

{ The Index object orders values and provides efficient access to a Recordset. }

  _Index = interface(_DAO)
    ['{00000059-0000-0010-8000-00AA006D2EA4}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Foreign: WordBool; safecall;
    function Get_Unique: WordBool; safecall;
    procedure Set_Unique(Value: WordBool); safecall;
    function Get_Clustered: WordBool; safecall;
    procedure Set_Clustered(Value: WordBool); safecall;
    function Get_Required: WordBool; safecall;
    procedure Set_Required(Value: WordBool); safecall;
    function Get_IgnoreNulls: WordBool; safecall;
    procedure Set_IgnoreNulls(Value: WordBool); safecall;
    function Get_Primary: WordBool; safecall;
    procedure Set_Primary(Value: WordBool); safecall;
    function Get_DistinctCount: Integer; safecall;
    function Get_Fields: OleVariant; safecall;
    procedure Set_Fields(Value: OleVariant); safecall;
    function CreateField(Name, Type_, Size: OleVariant): Field; safecall;
    function CreateProperty(Name, Type_, Value, DDL: OleVariant): Property_; safecall;
    property Name: WideString read Get_Name write Set_Name;
    property Foreign: WordBool read Get_Foreign;
    property Unique: WordBool read Get_Unique write Set_Unique;
    property Clustered: WordBool read Get_Clustered write Set_Clustered;
    property Required: WordBool read Get_Required write Set_Required;
    property IgnoreNulls: WordBool read Get_IgnoreNulls write Set_IgnoreNulls;
    property Primary: WordBool read Get_Primary write Set_Primary;
    property DistinctCount: Integer read Get_DistinctCount;
    property Fields: OleVariant read Get_Fields write Set_Fields;
  end;

{ DispInterface declaration for Dual Interface _Index }

  _IndexDisp = dispinterface
    ['{00000059-0000-0010-8000-00AA006D2EA4}']
    property Name: WideString dispid 1610809344;
    property Foreign: WordBool readonly dispid 1610809346;
    property Unique: WordBool dispid 1610809347;
    property Clustered: WordBool dispid 1610809349;
    property Required: WordBool dispid 1610809351;
    property IgnoreNulls: WordBool dispid 1610809353;
    property Primary: WordBool dispid 1610809355;
    property DistinctCount: Integer readonly dispid 1610809357;
    property Fields: OleVariant dispid 1610809358;
    function CreateField(Name, Type_, Size: OleVariant): Field; dispid 1610809360;
    function CreateProperty(Name, Type_, Value, DDL: OleVariant): Property_; dispid 1610809361;
  end;

{ Collection of Index objects. }

  Indexes = interface(_DynaCollection)
    ['{0000005B-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Index; safecall;
    property Item[Index: OleVariant]: Index read Get_Item; default;
  end;

{ DispInterface declaration for Dual Interface Indexes }

  IndexesDisp = dispinterface
    ['{0000005B-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Index readonly dispid 0; default;
  end;

  IndexFields = interface(_DynaCollection)
    ['{0000005D-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): OleVariant; safecall;
    property Item[Index: OleVariant]: OleVariant read Get_Item; default;
  end;

{ DispInterface declaration for Dual Interface IndexFields }

  IndexFieldsDisp = dispinterface
    ['{0000005D-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: OleVariant readonly dispid 0; default;
  end;

{ A parameter for a parameter query. }

  Parameter = interface(_DAO)
    ['{00000081-0000-0010-8000-00AA006D2EA4}']
    function Get_Name: WideString; safecall;
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(Value: OleVariant); safecall;
    function Get_Type_: Smallint; safecall;
    procedure Set_Type_(Value: Smallint); safecall;
    function Get_Direction: Smallint; safecall;
    procedure Set_Direction(Value: Smallint); safecall;
    property Name: WideString read Get_Name;
    property Value: OleVariant read Get_Value write Set_Value;
    property Type_: Smallint read Get_Type_ write Set_Type_;
    property Direction: Smallint read Get_Direction write Set_Direction;
  end;

{ DispInterface declaration for Dual Interface Parameter }

  ParameterDisp = dispinterface
    ['{00000081-0000-0010-8000-00AA006D2EA4}']
    property Name: WideString readonly dispid 1610809344;
    property Value: OleVariant dispid 0;
    property Type_: Smallint dispid 1610809347;
    property Direction: Smallint dispid 1610809349;
  end;

{ Collection of Parameter objects. }

  Parameters = interface(_Collection)
    ['{00000083-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Parameter; safecall;
    property Item[Index: OleVariant]: Parameter read Get_Item; default;
  end;

{ DispInterface declaration for Dual Interface Parameters }

  ParametersDisp = dispinterface
    ['{00000083-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Parameter readonly dispid 0; default;
  end;

{ A user account. }

  _User = interface(_DAO)
    ['{00000069-0000-0010-8000-00AA006D2EA4}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    procedure Set_PID(const Value: WideString); safecall;
    procedure Set_Password(const Value: WideString); safecall;
    function Get_Groups: Groups; safecall;
    procedure NewPassword(const bstrOld, bstrNew: WideString); safecall;
    function CreateGroup(Name, PID: OleVariant): Group; safecall;
    property Name: WideString read Get_Name write Set_Name;
    property PID: WideString write Set_PID;
    property Password: WideString write Set_Password;
    property Groups: Groups read Get_Groups;
  end;

{ DispInterface declaration for Dual Interface _User }

  _UserDisp = dispinterface
    ['{00000069-0000-0010-8000-00AA006D2EA4}']
    property Name: WideString dispid 1610809344;
    property PID: WideString writeonly dispid 1610809346;
    property Password: WideString writeonly dispid 1610809347;
    property Groups: Groups readonly dispid 0;
    procedure NewPassword(const bstrOld, bstrNew: WideString); dispid 1610809349;
    function CreateGroup(Name, PID: OleVariant): Group; dispid 1610809350;
  end;

{ Collection of User objects. }

  Users = interface(_DynaCollection)
    ['{0000006B-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): User; safecall;
    property Item[Index: OleVariant]: User read Get_Item; default;
  end;

{ DispInterface declaration for Dual Interface Users }

  UsersDisp = dispinterface
    ['{0000006B-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: User readonly dispid 0; default;
  end;

{ A group of user accounts. }

  _Group = interface(_DAO)
    ['{00000061-0000-0010-8000-00AA006D2EA4}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    procedure Set_PID(const Value: WideString); safecall;
    function Get_Users: Users; safecall;
    function CreateUser(Name, PID, Password: OleVariant): User; safecall;
    property Name: WideString read Get_Name write Set_Name;
    property PID: WideString write Set_PID;
    property Users: Users read Get_Users;
  end;

{ DispInterface declaration for Dual Interface _Group }

  _GroupDisp = dispinterface
    ['{00000061-0000-0010-8000-00AA006D2EA4}']
    property Name: WideString dispid 1610809344;
    property PID: WideString writeonly dispid 1610809346;
    property Users: Users readonly dispid 0;
    function CreateUser(Name, PID, Password: OleVariant): User; dispid 1610809348;
  end;

{ Collection of Group objects. }

  Groups = interface(_DynaCollection)
    ['{00000063-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Group; safecall;
    property Item[Index: OleVariant]: Group read Get_Item; default;
  end;

{ DispInterface declaration for Dual Interface Groups }

  GroupsDisp = dispinterface
    ['{00000063-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Group readonly dispid 0; default;
  end;

{ A relationship between fields in tables and queries. }

  _Relation = interface(_DAO)
    ['{00000089-0000-0010-8000-00AA006D2EA4}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Table: WideString; safecall;
    procedure Set_Table(const Value: WideString); safecall;
    function Get_ForeignTable: WideString; safecall;
    procedure Set_ForeignTable(const Value: WideString); safecall;
    function Get_Attributes: Integer; safecall;
    procedure Set_Attributes(Value: Integer); safecall;
    function Get_Fields: Fields; safecall;
    function CreateField(Name, Type_, Size: OleVariant): Field; safecall;
    function Get_PartialReplica: WordBool; safecall;
    procedure Set_PartialReplica(Value: WordBool); safecall;
    property Name: WideString read Get_Name write Set_Name;
    property Table: WideString read Get_Table write Set_Table;
    property ForeignTable: WideString read Get_ForeignTable write Set_ForeignTable;
    property Attributes: Integer read Get_Attributes write Set_Attributes;
    property Fields: Fields read Get_Fields;
    property PartialReplica: WordBool read Get_PartialReplica write Set_PartialReplica;
  end;

{ DispInterface declaration for Dual Interface _Relation }

  _RelationDisp = dispinterface
    ['{00000089-0000-0010-8000-00AA006D2EA4}']
    property Name: WideString dispid 1610809344;
    property Table: WideString dispid 1610809346;
    property ForeignTable: WideString dispid 1610809348;
    property Attributes: Integer dispid 1610809350;
    property Fields: Fields readonly dispid 0;
    function CreateField(Name, Type_, Size: OleVariant): Field; dispid 1610809353;
    property PartialReplica: WordBool dispid 1610809354;
  end;

{ Collection of Relation objects. }

  Relations = interface(_DynaCollection)
    ['{0000008B-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Relation; safecall;
    property Item[Index: OleVariant]: Relation read Get_Item; default;
  end;

{ DispInterface declaration for Dual Interface Relations }

  RelationsDisp = dispinterface
    ['{0000008B-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Relation readonly dispid 0; default;
  end;

{ A built-in or user-defined property. }

  Property_ = interface(_DAO)
    ['{00000027-0000-0010-8000-00AA006D2EA4}']
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(Value: OleVariant); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Type_: Smallint; safecall;
    procedure Set_Type_(Value: Smallint); safecall;
    function Get_Inherited_: WordBool; safecall;
    property Value: OleVariant read Get_Value write Set_Value;
    property Name: WideString read Get_Name write Set_Name;
    property Type_: Smallint read Get_Type_ write Set_Type_;
    property Inherited_: WordBool read Get_Inherited_;
  end;

{ DispInterface declaration for Dual Interface Property_ }

  Property_Disp = dispinterface
    ['{00000027-0000-0010-8000-00AA006D2EA4}']
    property Value: OleVariant dispid 0;
    property Name: WideString dispid 1610809346;
    property Type_: Smallint dispid 1610809348;
    property Inherited_: WordBool readonly dispid 1610809350;
  end;

{ Collection of Property objects. }

  Properties = interface(_DynaCollection)
    ['{00000029-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Property_; safecall;
    property Item[Index: OleVariant]: Property_ read Get_Item; default;
  end;

{ DispInterface declaration for Dual Interface Properties }

  PropertiesDisp = dispinterface
    ['{00000029-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Property_ readonly dispid 0; default;
  end;

{ Storage for information about a predefined object type. }

  Container = interface(_DAO)
    ['{00000091-0000-0010-8000-00AA006D2EA4}']
    function Get_Name: WideString; safecall;
    function Get_Owner: WideString; safecall;
    procedure Set_Owner(const Value: WideString); safecall;
    function Get_UserName: WideString; safecall;
    procedure Set_UserName(const Value: WideString); safecall;
    function Get_Permissions: Integer; safecall;
    procedure Set_Permissions(Value: Integer); safecall;
    function Get_Inherit: WordBool; safecall;
    procedure Set_Inherit(Value: WordBool); safecall;
    function Get_Documents: Documents; safecall;
    function Get_AllPermissions: Integer; safecall;
    property Name: WideString read Get_Name;
    property Owner: WideString read Get_Owner write Set_Owner;
    property UserName: WideString read Get_UserName write Set_UserName;
    property Permissions: Integer read Get_Permissions write Set_Permissions;
    property Inherit: WordBool read Get_Inherit write Set_Inherit;
    property Documents: Documents read Get_Documents;
    property AllPermissions: Integer read Get_AllPermissions;
  end;

{ DispInterface declaration for Dual Interface Container }

  ContainerDisp = dispinterface
    ['{00000091-0000-0010-8000-00AA006D2EA4}']
    property Name: WideString readonly dispid 1610809344;
    property Owner: WideString dispid 1610809345;
    property UserName: WideString dispid 1610809347;
    property Permissions: Integer dispid 1610809349;
    property Inherit: WordBool dispid 1610809351;
    property Documents: Documents readonly dispid 0;
    property AllPermissions: Integer readonly dispid 1610809354;
  end;

{ Collection of Container objects. }

  Containers = interface(_Collection)
    ['{00000093-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Container; safecall;
    property Item[Index: OleVariant]: Container read Get_Item; default;
  end;

{ DispInterface declaration for Dual Interface Containers }

  ContainersDisp = dispinterface
    ['{00000093-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Container readonly dispid 0; default;
  end;

{ Information about a saved, predefined object. }

  Document = interface(_DAO)
    ['{00000099-0000-0010-8000-00AA006D2EA4}']
    function Get_Name: WideString; safecall;
    function Get_Owner: WideString; safecall;
    procedure Set_Owner(const Value: WideString); safecall;
    function Get_Container: WideString; safecall;
    function Get_UserName: WideString; safecall;
    procedure Set_UserName(const Value: WideString); safecall;
    function Get_Permissions: Integer; safecall;
    procedure Set_Permissions(Value: Integer); safecall;
    function Get_DateCreated: OleVariant; safecall;
    function Get_LastUpdated: OleVariant; safecall;
    function Get_AllPermissions: Integer; safecall;
    function CreateProperty(Name, Type_, Value, DDL: OleVariant): Property_; safecall;
    property Name: WideString read Get_Name;
    property Owner: WideString read Get_Owner write Set_Owner;
    property Container: WideString read Get_Container;
    property UserName: WideString read Get_UserName write Set_UserName;
    property Permissions: Integer read Get_Permissions write Set_Permissions;
    property DateCreated: OleVariant read Get_DateCreated;
    property LastUpdated: OleVariant read Get_LastUpdated;
    property AllPermissions: Integer read Get_AllPermissions;
  end;

{ DispInterface declaration for Dual Interface Document }

  DocumentDisp = dispinterface
    ['{00000099-0000-0010-8000-00AA006D2EA4}']
    property Name: WideString readonly dispid 1610809344;
    property Owner: WideString dispid 1610809345;
    property Container: WideString readonly dispid 1610809347;
    property UserName: WideString dispid 1610809348;
    property Permissions: Integer dispid 1610809350;
    property DateCreated: OleVariant readonly dispid 1610809352;
    property LastUpdated: OleVariant readonly dispid 1610809353;
    property AllPermissions: Integer readonly dispid 1610809354;
    function CreateProperty(Name, Type_, Value, DDL: OleVariant): Property_; dispid 1610809355;
  end;

{ Collection of Document objects. }

  Documents = interface(_Collection)
    ['{0000009B-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Document; safecall;
    property Item[Index: OleVariant]: Document read Get_Item; default;
  end;

{ DispInterface declaration for Dual Interface Documents }

  DocumentsDisp = dispinterface
    ['{0000009B-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Document readonly dispid 0; default;
  end;

{ An open ODBCDirect connection. }

  Connection = interface(IDispatch)
    ['{00000041-0000-0010-8000-00AA006D2EA4}']
    function Get_Name: WideString; safecall;
    function Get_Connect: WideString; safecall;
    function Get_Database: Database; safecall;
    function Get_hDbc: Integer; safecall;
    function Get_QueryTimeout: Smallint; safecall;
    procedure Set_QueryTimeout(Value: Smallint); safecall;
    function Get_Transactions: WordBool; safecall;
    function Get_RecordsAffected: Integer; safecall;
    function Get_StillExecuting: WordBool; safecall;
    function Get_Updatable: WordBool; safecall;
    function Get_QueryDefs: QueryDefs; safecall;
    function Get_Recordsets: Recordsets; safecall;
    procedure Cancel; safecall;
    procedure Close; safecall;
    function CreateQueryDef(Name, SQLText: OleVariant): QueryDef; safecall;
    procedure Execute(const Query: WideString; Options: OleVariant); safecall;
    function OpenRecordset(const Name: WideString; Type_, Options, LockEdit: OleVariant): Recordset; safecall;
    property Name: WideString read Get_Name;
    property Connect: WideString read Get_Connect;
    property Database: Database read Get_Database;
    property hDbc: Integer read Get_hDbc;
    property QueryTimeout: Smallint read Get_QueryTimeout write Set_QueryTimeout;
    property Transactions: WordBool read Get_Transactions;
    property RecordsAffected: Integer read Get_RecordsAffected;
    property StillExecuting: WordBool read Get_StillExecuting;
    property Updatable: WordBool read Get_Updatable;
    property QueryDefs: QueryDefs read Get_QueryDefs;
    property Recordsets: Recordsets read Get_Recordsets;
  end;

{ DispInterface declaration for Dual Interface Connection }

  ConnectionDisp = dispinterface
    ['{00000041-0000-0010-8000-00AA006D2EA4}']
    property Name: WideString readonly dispid 1610743808;
    property Connect: WideString readonly dispid 1610743809;
    property Database: Database readonly dispid 1610743810;
    property hDbc: Integer readonly dispid 1610743811;
    property QueryTimeout: Smallint dispid 1610743812;
    property Transactions: WordBool readonly dispid 1610743814;
    property RecordsAffected: Integer readonly dispid 1610743815;
    property StillExecuting: WordBool readonly dispid 1610743816;
    property Updatable: WordBool readonly dispid 1610743817;
    property QueryDefs: QueryDefs readonly dispid 0;
    property Recordsets: Recordsets readonly dispid 1610743819;
    procedure Cancel; dispid 1610743820;
    procedure Close; dispid 1610743821;
    function CreateQueryDef(Name, SQLText: OleVariant): QueryDef; dispid 1610743822;
    procedure Execute(const Query: WideString; Options: OleVariant); dispid 1610743823;
    function OpenRecordset(const Name: WideString; Type_, Options, LockEdit: OleVariant): Recordset; dispid 1610743824;
  end;

{ Collection of Connection objects }

  Connections = interface(_Collection)
    ['{00000043-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Connection; safecall;
    property Item[Index: OleVariant]: Connection read Get_Item; default;
  end;

{ DispInterface declaration for Dual Interface Connections }

  ConnectionsDisp = dispinterface
    ['{00000043-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Connection readonly dispid 0; default;
  end;

{ The Microsoft Jet database engine. }

  CoDBEngine = class
    class function Create: _DBEngine;
    class function CreateRemote(const MachineName: string): _DBEngine;
  end;

{ DAO 3.0 DBEngine (private) }

  CoPrivDBEngine = class
    class function Create: _DBEngine;
    class function CreateRemote(const MachineName: string): _DBEngine;
  end;

{ A saved table definition. }

  CoTableDef = class
    class function Create: _TableDef;
    class function CreateRemote(const MachineName: string): _TableDef;
  end;

{ A saved query definition. }

  CoQueryDef = class
    class function Create: _QueryDef;
    class function CreateRemote(const MachineName: string): _QueryDef;
  end;

{ A column that is part of a TableDef, QueryDef, Index, Relation, or Recordset. }

  CoField = class
    class function Create: _Field;
    class function CreateRemote(const MachineName: string): _Field;
  end;

{ The Index object orders values and provides efficient access to a Recordset. }

  CoIndex = class
    class function Create: _Index;
    class function CreateRemote(const MachineName: string): _Index;
  end;

{ A user account. }

  CoUser = class
    class function Create: _User;
    class function CreateRemote(const MachineName: string): _User;
  end;

{ A group of user accounts. }

  CoGroup = class
    class function Create: _Group;
    class function CreateRemote(const MachineName: string): _Group;
  end;

{ A relationship between fields in tables and queries. }

  CoRelation = class
    class function Create: _Relation;
    class function CreateRemote(const MachineName: string): _Relation;
  end;



implementation

uses ComObj;

class function CoDBEngine.Create: _DBEngine;
begin
  Result := CreateComObject(Class_DBEngine) as _DBEngine;
end;

class function CoDBEngine.CreateRemote(const MachineName: string): _DBEngine;
begin
  Result := CreateRemoteComObject(MachineName, Class_DBEngine) as _DBEngine;
end;

class function CoPrivDBEngine.Create: _DBEngine;
begin
  Result := CreateComObject(Class_PrivDBEngine) as _DBEngine;
end;

class function CoPrivDBEngine.CreateRemote(const MachineName: string): _DBEngine;
begin
  Result := CreateRemoteComObject(MachineName, Class_PrivDBEngine) as _DBEngine;
end;

class function CoTableDef.Create: _TableDef;
begin
  Result := CreateComObject(Class_TableDef) as _TableDef;
end;

class function CoTableDef.CreateRemote(const MachineName: string): _TableDef;
begin
  Result := CreateRemoteComObject(MachineName, Class_TableDef) as _TableDef;
end;

class function CoQueryDef.Create: _QueryDef;
begin
  Result := CreateComObject(Class_QueryDef) as _QueryDef;
end;

class function CoQueryDef.CreateRemote(const MachineName: string): _QueryDef;
begin
  Result := CreateRemoteComObject(MachineName, Class_QueryDef) as _QueryDef;
end;

class function CoField.Create: _Field;
begin
  Result := CreateComObject(Class_Field) as _Field;
end;

class function CoField.CreateRemote(const MachineName: string): _Field;
begin
  Result := CreateRemoteComObject(MachineName, Class_Field) as _Field;
end;

class function CoIndex.Create: _Index;
begin
  Result := CreateComObject(Class_Index) as _Index;
end;

class function CoIndex.CreateRemote(const MachineName: string): _Index;
begin
  Result := CreateRemoteComObject(MachineName, Class_Index) as _Index;
end;

class function CoUser.Create: _User;
begin
  Result := CreateComObject(Class_User) as _User;
end;

class function CoUser.CreateRemote(const MachineName: string): _User;
begin
  Result := CreateRemoteComObject(MachineName, Class_User) as _User;
end;

class function CoGroup.Create: _Group;
begin
  Result := CreateComObject(Class_Group) as _Group;
end;

class function CoGroup.CreateRemote(const MachineName: string): _Group;
begin
  Result := CreateRemoteComObject(MachineName, Class_Group) as _Group;
end;

class function CoRelation.Create: _Relation;
begin
  Result := CreateComObject(Class_Relation) as _Relation;
end;

class function CoRelation.CreateRemote(const MachineName: string): _Relation;
begin
  Result := CreateRemoteComObject(MachineName, Class_Relation) as _Relation;
end;


end.
