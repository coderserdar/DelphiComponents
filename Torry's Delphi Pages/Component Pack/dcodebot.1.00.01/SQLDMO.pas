unit SQLDMO;

interface

uses
  Windows, ActiveX;

const
  DMOMajorVersion = 7;
  DMOMinorVersion = 0;

  LIBID_DMO: TGUID = '{10010001-E260-11CF-AE68-00AA004A34D5}';

  CLASS_Application: TGUID = '{10020100-E260-11CF-AE68-00AA004A34D5}';
  CLASS_SQLServer: TGUID = '{10020200-E260-11CF-AE68-00AA004A34D5}';
  CLASS_Database: TGUID = '{10020300-E260-11CF-AE68-00AA004A34D5}';

  IID_IVDMOStdObject: TGUID = '{10010007-E260-11CF-AE68-00AA004A34D5}';
  IID_Application: TGUID = '{10020106-E260-11CF-AE68-00AA004A34D5}';
  IID_Properties: TGUID = '{10020001-E260-11CF-AE68-00AA004A34D5}';
  IID_Property: TGUID = '{10020002-E260-11CF-AE68-00AA004A34D5}';
  IID_SQLServers: TGUID = '{10020203-E260-11CF-AE68-00AA004A34D5}';
  IID_SQLServer: TGUID = '{10020206-E260-11CF-AE68-00AA004A34D5}';
  IID_Databases: TGUID = '{10020303-E260-11CF-AE68-00AA004A34D5}';
  IID_Database: TGUID = '{10020306-E260-11CF-AE68-00AA004A34D5}';
  IID_QueryResults: TGUID = '{10022506-E260-11CF-AE68-00AA004A34D5}';

{ Constants for enum DMO_OBJECT_TYPE }

type
  DMO_OBJECT_TYPE = TOleEnum;

const
  DMOObj_Unknown = $00004000;
  DMOObj_Application = $00000000;
  DMOObj_UserDefinedDatatype = $00000001;
  DMOObj_SystemTable = $00000002;
  DMOObj_View = $00000004;
  DMOObj_UserTable = $00000008;
  DMOObj_StoredProcedure = $00000010;
  DMOObj_Default = $00000040;
  DMOObj_Rule = $00000080;
  DMOObj_Trigger = $00000100;
  DMOObj_AllDatabaseUserObjects = $000001FD;
  DMOObj_AllDatabaseObjects = $000001FF;
  DMOObj_SystemDatatype = $00001000;
  DMOObj_User = $00002000;
  DMOObj_Group = $00003000;
  DMOObj_Index = $00004000;
  DMOObj_Key = $00005000;
  DMOObj_Column = $00006000;
  DMOObj_DBObject = $00007000;
  DMOObj_DBOption = $00008000;
  DMOObj_ProcedureParameter = $00009000;
  DMOObj_Permission = $0000A000;
  DMOObj_IntegratedSecurity = $0000B000;
  DMOObj_Check = $0000C000;
  DMOObj_DRIDefault = $0000D000;
  DMOObj_Server = $00020000;
  DMOObj_Database = $00021000;
  DMOObj_BackupDevice = $00022000;
  DMOObj_Login = $00023000;
  DMOObj_Language = $00024000;
  DMOObj_RemoteServer = $00025000;
  DMOObj_RemoteLogin = $00026000;
  DMOObj_Configuration = $00027000;
  DMOObj_ConfigValue = $00028000;
  DMOObj_QueryResults = $00029000;
  DMOObj_TransactionLog = $0002A000;
  DMOObj_Registry = $0002B000;
  DMOObj_Transfer = $0002C000;
  DMOObj_Backup = $0002D000;
  DMOObj_AutoProperty = $0002E000;
  DMOObj_ServerGroup = $0002F000;
  DMOObj_RegisteredServer = $00031000;
  DMOObj_BulkCopy = $00032000;
  DMOObj_FileGroup = $00033000;
  DMOObj_DBFile = $00034000;
  DMOObj_LogFile = $00035000;
  DMOObj_ServerRole = $00036000;
  DMOObj_DatabaseRole = $00037000;
  DMOObj_Restore = $00038000;
  DMOObj_LinkedServer = $00039000;
  DMOObj_LinkedServerLogin = $00040000;
  DMOObj_FullTextCatalog = $00041000;
  DMOObj_FullTextService = $00042000;
  DMOObj_TransPublication = $00101000;
  DMOObj_TransArticle = $00102000;
  DMOObj_TransSubscription = $00103000;
  DMOObj_TransPullSubscription = $00104000;
  DMOObj_MergePublication = $00105000;
  DMOObj_MergeArticle = $00106000;
  DMOObj_MergeSubscription = $00107000;
  DMOObj_MergePullSubscription = $00108000;
  DMOObj_Replication = $00109000;
  DMOObj_Publisher = $0010A000;
  DMOObj_Subscriber = $0010B000;
  DMOObj_Distributor = $0010C000;
  DMOObj_ReplicationSecurity = $0010D000;
  DMOObj_DistributionPublisher = $0010E000;
  DMOObj_RegisteredSubscriber = $0010F000;
  DMOObj_ReplicationDatabase = $00110000;
  DMOObj_DistributionDatabase = $00111000;
  DMOObj_ReplicationTable = $00112000;
  DMOObj_ReplicationStoredProcedure = $00113000;
  DMOObj_DistributionPublication = $00114000;
  DMOObj_DistributionArticle = $00115000;
  DMOObj_DistributionSubscription = $00116000;
  DMOObj_MergeSubsetFilter = $00117000;
  DMOObj_AlertSystem = $00201000;
  DMOObj_JobServer = $00202000;
  DMOObj_Alert = $00203000;
  DMOObj_Operator = $00204000;
  DMOObj_Job = $00205000;
  DMOObj_JobStep = $00206000;
  DMOObj_TargetServer = $00207000;
  DMOObj_TargetServerGroup = $00208000;
  DMOObj_Category = $00209000;
  DMOObj_Schedule = $00210000;
  DMOObj_JobFilter = $00211000;
  DMOObj_JobHistoryFilter = $00212000;
  DMOObj_JobSchedule = $00213000;
  DMOObj_Last = $10000000;

{ Constants for enum DMO_SRVUSERPROFILE_TYPE }

type
  DMO_SRVUSERPROFILE_TYPE = TOleEnum;

const
  DMOSrvUserProf_None = $00000000;
  DMOSrvUserProf_SaLogin = $00000001;
  DMOSrvUserProf_CreateDatabase = $00000002;
  DMOSrvUserProf_CreateXP = $00000004;
  DMOSrvUserProf_AllProfileBits = $00000007;

{ Constants for enum DMO_SVCSTATUS_TYPE }

type
  DMO_SVCSTATUS_TYPE = TOleEnum;

const
  DMOSvc_Unknown = $00000000;
  DMOSvc_Running = $00000001;
  DMOSvc_Paused = $00000002;
  DMOSvc_Stopped = $00000003;
  DMOSvc_Starting = $00000004;
  DMOSvc_Stopping = $00000005;
  DMOSvc_Continuing = $00000006;
  DMOSvc_Pausing = $00000007;

{ Constants for enum DMO_STATUSINFO_TYPE }

type
  DMO_STATUSINFO_TYPE = TOleEnum;

const
  DMOStatInfo_Unknown = $00000000;
  DMOStatInfo_DatabaseStatus = $00000001;
  DMOStatInfo_DatabaseSpace = $00000002;
  DMOStatInfo_AutoVerifyConnection = $00000004;
  DMOStatInfo_All = $00000007;

{ Constants for enum DMO_EXEC_TYPE }

type
  DMO_EXEC_TYPE = TOleEnum;

const
  DMOExec_Default = $00000000;
  DMOExec_NoCommandTerm = $00000001;
  DMOExec_ContinueOnError = $00000002;
  DMOExec_NoExec = $00000004;
  DMOExec_ParseOnly = $00000008;
  DMOExec_QI_ON = $00000010;

{ Constants for enum DMO_MEDIA_TYPE }

type
  DMO_MEDIA_TYPE = TOleEnum;

const
  DMOMedia_Floppy = $00000001;
  DMOMedia_FixedDisk = $00000002;
  DMOMedia_Tape = $00000004;
  DMOMedia_CDROM = $00000008;
  DMOMedia_All = $0000000F;

{ Constants for enum DMO_OS_TYPE }

type
  DMO_OS_TYPE = TOleEnum;

const
  DMO_WIN95 = $00000001;
  DMO_WINNT = $00000002;

{ Constants for enum DMO_ROLE_TYPE }

type
  DMO_ROLE_TYPE = TOleEnum;
  
const
  DMORole_Server = $00000001;
  DMORole_Database = $00000002;
  DMORole_All = $00000003;

{ Constants for enum DMO_VER }

type
  DMO_VER = TOleEnum;
  
const
  DMOVer_Unknown = $00000000;
  DMOVer_Pre_60 = $00000001;
  DMOVer_60 = $00000002;
  DMOVer_65 = $00000004;
  DMOVer_70 = $00000008;

{ Constants for enum DMO_PACKAGE_TYPE }

type
  DMO_PACKAGE_TYPE = TOleEnum;

const
  DMO_Unknown = $00000000;
  DMO_OFFICE = $00000001;
  DMO_STANDARD = $00000002;
  DMO_ENTERPRISE = $00000003;
  DMO_MSDE = $00000004;

{ Constants for enum DMO_DBUSERPROFILE_TYPE }

type
  DMO_DBUSERPROFILE_TYPE = TOleEnum;

const
  DMODbUserProf_None = $00000000;
  DMODbUserProf_DboLogin = $00000001;
  DMODbUserProf_CreateTable = $00000002;
  DMODbUserProf_CreateView = $00000004;
  DMODbUserProf_CreateProcedure = $00000008;
  DMODbUserProf_DumpDatabase = $00000010;
  DMODbUserProf_CreateDefault = $00000020;
  DMODbUserProf_DumpTransaction = $00000040;
  DMODbUserProf_CreateRule = $00000080;
  DMODbUserProf_DumpTable = $00000100;
  DMODbUserProf_AllProfileBits = $000001FF;
  DMODbUserProf_InvalidLogin = $40000000;
  DMODbUserProf_InaccessibleDb = $80000000;
  DMODbUserProf_DbNotAvailable = $C0000000;

{ Constants for enum DMO_DBSTATUS_TYPE }

type
  DMO_DBSTATUS_TYPE = TOleEnum;
  
const
  DMODBStat_Normal = $00000000;
  DMODBStat_Loading = $00000020;
  DMODBStat_Recovering = $000000C0;
  DMODBStat_Suspect = $00000100;
  DMODBStat_Offline = $00000200;
  DMODBStat_Inaccessible = $000003E0;
  DMODBStat_EmergencyMode = $00008000;
  DMODBStat_Standby = $00000400;
  DMODBStat_All = $000087E0;

{ Constants for enum DMO_PRIVILEGE_TYPE }

type
  DMO_PRIVILEGE_TYPE = TOleEnum;

const
  DMOPriv_Unknown = $00000000;
  DMOPriv_Select = $00000001;
  DMOPriv_Insert = $00000002;
  DMOPriv_Update = $00000004;
  DMOPriv_Delete = $00000008;
  DMOPriv_Execute = $00000010;
  DMOPriv_References = $00000020;
  DMOPriv_AllObjectPrivs = $0000003F;
  DMOPriv_CreateTable = $00000080;
  DMOPriv_CreateDatabase = $00000100;
  DMOPriv_CreateView = $00000200;
  DMOPriv_CreateProcedure = $00000400;
  DMOPriv_DumpDatabase = $00000800;
  DMOPriv_CreateDefault = $00001000;
  DMOPriv_DumpTransaction = $00002000;
  DMOPriv_CreateRule = $00004000;
  DMOPriv_DumpTable = $00008000;
  DMOPriv_AllDatabasePrivs = $0000FF80;

{ Constants for enum DMO_DBCC_REPAIR_TYPE }

type
  DMO_DBCC_REPAIR_TYPE = TOleEnum;

const
  DMORepair_None = $00000000;
  DMORepair_Fast = $00000001;
  DMORepair_Rebuild = $00000002;
  DMORepair_Allow_DataLoss = $00000003;

{ Constants for enum DMO_OBJSORT_TYPE }

type
  DMO_OBJSORT_TYPE = TOleEnum;

const
  DMOObjSort_Name = $00000000;
  DMOObjSort_Type = $00000001;
  DMOObjSort_Owner = $00000002;
  DMOObjSort_Date = $00000003;

{ Constants for enum DMO_DEPENDENCY_TYPE }

type
  DMO_DEPENDENCY_TYPE = TOleEnum;
  
const
  DMODep_Parents = $00000000;
  DMODep_FullHierarchy = $00010000;
  DMODep_OrderDescending = $00020000;
  DMODep_Children = $00040000;
  DMODep_ReturnInputObject = $00080000;
  DMODep_FirstLevelOnly = $00100000;
  DMODep_DRIOnly = $00200000;
  DMODep_Valid = $003F0000;

{ Constants for enum DMO_XFRSCRIPTMODE_TYPE }

type
  DMO_XFRSCRIPTMODE_TYPE = TOleEnum;

const
  DMOXfrFile_Default = $00000001;
  DMOXfrFile_SummaryFiles = $00000001;
  DMOXfrFile_SingleFile = $00000002;
  DMOXfrFile_SingleFilePerObject = $00000004;
  DMOXfrFile_SingleSummaryFile = $00000008;

  { Constants for enum DMO_SCRIPT_TYPE }

type
  DMO_SCRIPT_TYPE = TOleEnum;
  
const
  DMOScript_Default = $00000004;
  DMOScript_Drops = $00000001;
  DMOScript_ObjectPermissions = $00000002;
  DMOScript_PrimaryObject = $00000004;
  DMOScript_ClusteredIndexes = $00000008;
  DMOScript_Triggers = $00000010;
  DMOScript_DatabasePermissions = $00000020;
  DMOScript_Permissions = $00000022;
  DMOScript_ToFileOnly = $00000040;
  DMOScript_Bindings = $00000080;
  DMOScript_AppendToFile = $00000100;
  DMOScript_NoDRI = $00000200;
  DMOScript_UDDTsToBaseType = $00000400;
  DMOScript_IncludeIfNotExists = $00001000;
  DMOScript_NonClusteredIndexes = $00002000;
  DMOScript_Indexes = $00012008;
  DMOScript_Aliases = $00004000;
  DMOScript_NoCommandTerm = $00008000;
  DMOScript_DRIIndexes = $00010000;
  DMOScript_IncludeHeaders = $00020000;
  DMOScript_OwnerQualify = $00040000;
  DMOScript_TimestampToBinary = $00080000;
  DMOScript_SortedData = $00100000;
  DMOScript_SortedDataReorg = $00200000;
  DMOScript_TransferDefault = $000670FF;
  DMOScript_DRI_NonClustered = $00400000;
  DMOScript_DRI_Clustered = $00800000;
  DMOScript_DRI_Checks = $01000000;
  DMOScript_DRI_Defaults = $02000000;
  DMOScript_DRI_UniqueKeys = $04000000;
  DMOScript_DRI_ForeignKeys = $08000000;
  DMOScript_DRI_PrimaryKey = $10000000;
  DMOScript_DRI_AllKeys = $1C000000;
  DMOScript_DRI_AllConstraints = $1F000000;
  DMOScript_DRI_All = $1FC00000;
  DMOScript_DRIWithNoCheck = $20000000;
  DMOScript_NoIdentity = $40000000;
  DMOScript_UseQuotedIdentifiers = $80000000;

{ Constants for enum DMO_SCRIPT2_TYPE }

type
  DMO_SCRIPT2_TYPE = TOleEnum;

const
  DMOScript2_Default = $00000000;
  DMOScript2_AnsiPadding = $00000001;
  DMOScript2_AnsiFile = $00000002;
  DMOScript2_UnicodeFile = $00000004;
  DMOScript2_NonStop = $00000008;
  DMOScript2_NoFG = $00000010;
  DMOScript2_MarkTriggers = $00000020;
  DMOScript2_OnlyUserTriggers = $00000040;
  DMOScript2_EncryptPWD = $00000080;
  DMOScript2_SeparateXPs = $00000100;
  DMOScript2_NoWhatIfIndexes = $00000200;
  DMOScript2_AgentNotify = $00000400;
  DMOScript2_AgentAlertJob = $00000800;
  DMOScript2_FullTextIndex = $00080000;
  DMOScript2_LoginSID = $00100000;
  DMOScript2_FullTextCat = $00200000;

{ Constants for enum DMO_SHRINK_TYPE }

type
  DMO_SHRINK_TYPE = TOleEnum;

const
  DMOShrink_Default = $00000000;
  DMOShrink_NoTruncate = $00000001;
  DMOShrink_TruncateOnly = $00000002;
  DMOShrink_EmptyFile = $00000003;

{ Constants for enum DMO_COMP_LEVEL_TYPE }

type
  DMO_COMP_LEVEL_TYPE = TOleEnum;
  
const
  DMOCompLevel_Unknown = $00000000;
  DMOCompLevel_60 = $0000003C;
  DMOCompLevel_65 = $00000041;
  DMOCompLevel_70 = $00000046;

{ Constants for enum DMO_QUERY_DATATYPE }

type
  DMO_QUERY_DATATYPE = TOleEnum;

const
  DMO_DTypeUnknown = $00000000;
  DMO_DTypeChar = $00000001;
  DMO_DTypeText = $FFFFFFFF;
  DMO_DTypeVarchar = $0000000C;
  DMO_DTypeVarBinary = $FFFFFFFD;
  DMO_DTypeBinary = $FFFFFFFE;
  DMO_DTypeImage = $FFFFFFFC;
  DMO_DTypeFloat4 = $00000007;
  DMO_DTypeFloat8 = $00000006;
  DMO_DTypeInt1 = $FFFFFFFA;
  DMO_DTypeInt2 = $00000005;
  DMO_DTypeInt4 = $00000004;
  DMO_DTypeMoney4 = $00000003;
  DMO_DTypeMoney = $00000003;
  DMO_DTypeDateTime = $FFFFFFFE;
  DMO_DTypeDateTime4 = $0000005D;
  DMO_DTypeBit = $FFFFFFF9;
  DMO_DTypeUChar = $FFFFFFF8;
  DMO_DTypeUVarchar = $FFFFFFF7;
  DMO_DTypeGUID = $FFFFFFF5;
  DMO_DTypeNText = $FFFFFFF6;

{ Constants for enum DMO_VERIFYCONN_TYPE }

type
  DMO_VERIFYCONN_TYPE = TOleEnum;

const
  DMOConn_LastState = $00000001;
  DMOConn_CurrentState = $00000002;
  DMOConn_ReconnectIfDead = $00000006;
  DMOConn_Valid = $00000007;

type
  IApplication = interface;
  IProperties = interface;
  IProperty = interface;
  ISQLServers = interface;
  ISQLServer = interface;
  IDatabases = interface;
  IDatabase = interface;
  IQueryResults = interface;

{ IVDMOStdObject }

  IVDMOStdObject = interface(IDispatch)
    ['{10010007-E260-11CF-AE68-00AA004A34D5}']
  end;

{ IVDMOStub }

  IVDMOStub = interface(IVDMOStdObject)
  end;

{ IApplication - This interface contains the following stubs:

    NameList
    ServerGroups }

  IApplication = interface(IVDMOStdObject)
    ['{10020106-E260-11CF-AE68-00AA004A34D5}']
    function GetApplication: IApplication; safecall;
    function GetParent: IVDMOStdObject; safecall;
    function GetUserData: Integer; safecall;
    procedure SetUserData(pRetVal: Integer); safecall;
    function GetTypeOf: DMO_OBJECT_TYPE; safecall;
    function GetProperties: IProperties; safecall;
    function GetName: WideString; safecall;
    function GetFullName: WideString; safecall;
    function GetServers: ISQLServers; safecall;
    function GetODBCVersionString: WideString; safecall;
    function GetVersionMajor: Integer; safecall;
    function GetVersionMinor: Integer; safecall;
    procedure Quit; safecall;
    function GetGroupRegistrationServer: WideString; safecall;
    procedure SetGroupRegistrationServer(const pRetVal: WideString); safecall;
    function GetServerGroups: IVDMOStub; safecall;
    function GetGroupRegistrationVersion: Integer; safecall;
    function GetVersionBuild: Integer; safecall;
    function GetUseCurrentUserServerGroups: WordBool; safecall;
    procedure SetUseCurrentUserServerGroups(pRetVal: WordBool); safecall;
    function ListAvailableServers: IVDMOStub; safecall;
    function GetBlockingTimeout: Integer; safecall;
    procedure SetBlockingTimeout(pRetVal: Integer); safecall;
    property Application: IApplication read GetApplication;
    property Parent: IVDMOStdObject read GetParent;
    property UserData: Integer read GetUserData write SetUserData;
    property TypeOf: DMO_OBJECT_TYPE read GetTypeOf;
    property Properties: IProperties read GetProperties;
    property Name: WideString read GetName;
    property FullName: WideString read GetFullName;
    property Servers: ISQLServers read GetServers;
    property ODBCVersionString: WideString read GetODBCVersionString;
    property VersionMajor: Integer read GetVersionMajor;
    property VersionMinor: Integer read GetVersionMinor;
    property GroupRegistrationServer: WideString read GetGroupRegistrationServer
      write SetGroupRegistrationServer;
    property ServerGroups: IVDMOStub read GetServerGroups;
    property GroupRegistrationVersion: Integer read GetGroupRegistrationVersion;
    property VersionBuild: Integer read GetVersionBuild;
    property UseCurrentUserServerGroups: WordBool read GetUseCurrentUserServerGroups
      write SetUseCurrentUserServerGroups;
    property BlockingTimeout: Integer read GetBlockingTimeout write SetBlockingTimeout;
  end;

{ IObjectList }

  IObjectList = interface(IVDMOStdObject)
    ['{10022806-E260-11CF-AE68-00AA004A34D5}']
    function GetApplication: IApplication; safecall;
    function GetParent: IVDMOStdObject; safecall;
    function GetUserData: Integer; safecall;
    procedure SetUserData(pRetVal: Integer); safecall;
    function GetTypeOf: DMO_OBJECT_TYPE; safecall;
    function Item(Index: OleVariant): IVDMOStdObject; safecall;
    function GetNewEnum: IUnknown; safecall;
    function GetCount: Integer; safecall;
    procedure Refresh; safecall;
    property Application: IApplication read GetApplication;
    property Parent: IVDMOStdObject read GetParent;
    property UserData: Integer read GetUserData write SetUserData;
    property TypeOf: DMO_OBJECT_TYPE read GetTypeOf;
    property NewEnum: IUnknown read GetNewEnum;
    property Count: Integer read GetCount;
  end;

{ IProperties }

  IProperties = interface(IVDMOStdObject)
    ['{10020001-E260-11CF-AE68-00AA004A34D5}']
    function GetApplication: IApplication; safecall;
    function GetParent: IVDMOStdObject; safecall;
    function GetUserData: Integer; safecall;
    procedure SetUserData(pRetVal: Integer); safecall;
    function GetTypeOf: DMO_OBJECT_TYPE; safecall;
    function Item(Index: OleVariant): IProperty; safecall;
    function GetNewEnum: IUnknown; safecall;
    function GetCount: Integer; safecall;
    property Application: IApplication read GetApplication;
    property Parent: IVDMOStdObject read GetParent;
    property UserData: Integer read GetUserData write SetUserData;
    property TypeOf: DMO_OBJECT_TYPE read GetTypeOf;
    property NewEnum: IUnknown read GetNewEnum;
    property Count: Integer read GetCount;
  end;

{ IProperty }

  IProperty = interface(IVDMOStdObject)
    ['{10020002-E260-11CF-AE68-00AA004A34D5}']
    function GetApplication: IApplication; safecall;
    function GetParent: IVDMOStdObject; safecall;
    function GetUserData: Integer; safecall;
    procedure SetUserData(pRetVal: Integer); safecall;
    function GetTypeOf: DMO_OBJECT_TYPE; safecall;
    function GetValue: OleVariant; safecall;
    procedure SetValue(pRetVal: OleVariant); safecall;
    function GetProperties: IProperties; safecall;
    function GetName: WideString; safecall;
    function GetKind: Integer; safecall;
    function GetGetBool: WordBool; safecall;
    function GetSetBool: WordBool; safecall;
    property Application: IApplication read GetApplication;
    property Parent: IVDMOStdObject read GetParent;
    property UserData: Integer read GetUserData write SetUserData;
    property TypeOf: DMO_OBJECT_TYPE read GetTypeOf;
    property Value: OleVariant read GetValue write SetValue;
    property Properties: IProperties read GetProperties;
    property Name: WideString read GetName;
    property Kind: Integer read GetKind;
    property GetBool: WordBool read GetGetBool;
    property SetBool: WordBool read GetSetBool;
  end;

{ ISQLServers }

  ISQLServers = interface(IVDMOStdObject)
    ['{10020203-E260-11CF-AE68-00AA004A34D5}']
    function GetApplication: IApplication; safecall;
    function GetParent: IVDMOStdObject; safecall;
    function GetUserData: Integer; safecall;
    procedure SetUserData(pRetVal: Integer); safecall;
    function GetTypeOf: DMO_OBJECT_TYPE; safecall;
    function Item(Index: OleVariant): ISQLServer; safecall;
    function GetNewEnum: IUnknown; safecall;
    function GetCount: Integer; safecall;
    function ItemByID(ID: Integer): ISQLServer; safecall;
    property Application: IApplication read GetApplication;
    property Parent: IVDMOStdObject read GetParent;
    property UserData: Integer read GetUserData write SetUserData;
    property TypeOf: DMO_OBJECT_TYPE read GetTypeOf;
    property NewEnum: IUnknown read GetNewEnum;
    property Count: Integer read GetCount;
  end;

{ ISQLServer - This interface contains the following stubs:

    BackupDevices
    Configuration
    FullTextService
    JobServer
    IntegratedSecurity
    Languages
    LinkedServers
    Logins
    NameList
    Registry
    RemoteServers
    Replication
    ServerRoles }

  ISQLServer = interface(IVDMOStdObject)
    ['{10020206-E260-11CF-AE68-00AA004A34D5}']
    function GetApplication: IApplication; safecall;
    function GetParent: IVDMOStdObject; safecall;
    function GetUserData: Integer; safecall;
    procedure SetUserData(pRetVal: Integer); safecall;
    function GetTypeOf: DMO_OBJECT_TYPE; safecall;
    function GetProperties: IProperties; safecall;
    function GetDatabases: IDatabases; safecall;
    function GetPassword: WideString; safecall;
    procedure SetPassword(const pRetVal: WideString); safecall;
    function GetName: WideString; safecall;
    procedure SetName(const pRetVal: WideString); safecall;
    function GetLogin: WideString; safecall;
    procedure SetLogin(const pRetVal: WideString); safecall;
    function GetVersionString: WideString; safecall;
    function GetBackupDevices: IVDMOStub; safecall;
    function GetVersionMajor: Integer; safecall;
    function GetVersionMinor: Integer; safecall;
    function GetCommandTerminator: WideString; safecall;
    procedure SetCommandTerminator(const pRetVal: WideString); safecall;
    function GetTrueName: WideString; safecall;
    function GetConnectionID: Integer; safecall;
    function GetTrueLogin: WideString; safecall;
    function GetIntegratedSecurity: IVDMOStub; safecall;
    function GetLanguages: IVDMOStub; safecall;
    function GetRemoteServers: IVDMOStub; safecall;
    function GetLogins: IVDMOStub; safecall;
    function GetUserProfile: DMO_SRVUSERPROFILE_TYPE; safecall;
    function GetMaxNumericPrecision: Integer; safecall;
    function GetNextDeviceNumber: Integer; safecall;
    function GetQueryTimeout: Integer; safecall;
    procedure SetQueryTimeout(pRetVal: Integer); safecall;
    function GetLoginTimeout: Integer; safecall;
    procedure SetLoginTimeout(pRetVal: Integer); safecall;
    function GetNetPacketSize: Integer; safecall;
    procedure SetNetPacketSize(pRetVal: Integer); safecall;
    function GetHostName: WideString; safecall;
    procedure SetHostName(const pRetVal: WideString); safecall;
    function GetApplicationName: WideString; safecall;
    procedure SetApplicationName(const pRetVal: WideString); safecall;
    function GetLoginSecure: WordBool; safecall;
    procedure SetLoginSecure(pRetVal: WordBool); safecall;
    function GetProcessID: Integer; safecall;
    function GetStatus: DMO_SVCSTATUS_TYPE; safecall;
    function GetRegistry: IVDMOStub; safecall;
    function GetConfiguration: IVDMOStub; safecall;
    function GetJobServer: IVDMOStub; safecall;
    function GetProcessInputBuffer(ProcessID: Integer): WideString; safecall;
    function GetProcessOutputBuffer(ProcessID: Integer): WideString; safecall;
    function GetLanguage: WideString; safecall;
    procedure SetLanguage(const pRetVal: WideString); safecall;
    function GetAutoReConnect: WordBool; safecall;
    procedure SetAutoReConnect(pRetVal: WordBool); safecall;
    function GetStatusInfoRefetchInterval(StatusInfoType: DMO_STATUSINFO_TYPE): Integer; safecall;
    procedure SetStatusInfoRefetchInterval(StatusInfoType: DMO_STATUSINFO_TYPE; pRetVal: Integer); safecall;
    function GetSaLogin: WordBool; safecall;
    function GetAnsiNulls: WordBool; safecall;
    procedure SetAnsiNulls(pRetVal: WordBool); safecall;
    procedure Connect(ServerName: OleVariant; Login: OleVariant; Password: OleVariant); safecall;
    procedure Close; safecall;
    procedure DisConnect; safecall;
    procedure KillProcess(lProcessID: Integer); safecall;
    procedure ExecuteImmediate(const Command: WideString; ExecType: DMO_EXEC_TYPE;
      Length: OleVariant); safecall;
    procedure ReConnect; safecall;
    procedure Shutdown(Wait: OleVariant); safecall;
    procedure Start(StartMode: WordBool; Server: OleVariant; Login: OleVariant; Password: OleVariant); safecall;
    procedure UnloadODSDLL(const DLLName: WideString); safecall;
    procedure KillDatabase(const DatabaseName: WideString); safecall;
    function ExecuteWithResults(const Command: WideString; Length: OleVariant): IQueryResults; safecall;
    function ListStartupProcedures: IObjectList; safecall;
    procedure BeginTransaction(TransactionName: OleVariant); safecall;
    procedure CommitTransaction(TransactionName: OleVariant); safecall;
    procedure SaveTransaction(const SavepointName: WideString); safecall;
    procedure RollbackTransaction(TransactionOrSavepointName: OleVariant); safecall;
    procedure CommandShellImmediate(const Command: WideString); safecall;
    function ReadErrorLog(LogNumber: OleVariant): IQueryResults; safecall;
    function EnumErrorLogs: IQueryResults; safecall;
    function EnumAvailableMedia(MediaType: DMO_MEDIA_TYPE): IQueryResults; safecall;
    function EnumDirectories(const PathName: WideString): IQueryResults; safecall;
    function EnumServerAttributes: IQueryResults; safecall;
    function EnumVersionInfo(Prefixes: OleVariant): IQueryResults; safecall;
    function EnumLocks(WhoByID: OleVariant): IQueryResults; safecall;
    function CommandShellWithResults(const Command: WideString): IQueryResults; safecall;
    function ReadBackupHeader(const LoadSpec: IVDMOStub): IQueryResults; safecall;
    function EnumProcesses(WhoByNameOrID: OleVariant): IQueryResults; safecall;
    procedure Pause; safecall;
    procedure Continue; safecall;
    function VerifyConnection(ReconnectIfDead: OleVariant): WordBool; safecall;
    function IsOS(lType: DMO_OS_TYPE): WordBool; safecall;
    procedure AddStartParameter(const NewParam: WideString); safecall;
    function GetNetName: WideString; safecall;
    function ExecuteWithResultsAndMessages(const Command: WideString; Length: OleVariant;
      out Messages: WideString): IQueryResults; safecall;
    function EnumLoginMappings: IQueryResults; safecall;
    function GetReplication: IVDMOStub; safecall;
    function GetEnableBcp: WordBool; safecall;
    procedure SetEnableBcp(pRetVal: WordBool); safecall;
    function GetBlockingTimeout: Integer; safecall;
    procedure SetBlockingTimeout(pRetVal: Integer); safecall;
    function GetServerRoles: IVDMOStub; safecall;
    function GetIsdbcreator: WordBool; safecall;
    function GetIsdiskadmin: WordBool; safecall;
    function GetIsprocessadmin: WordBool; safecall;
    function GetIssecurityadmin: WordBool; safecall;
    function GetIsserveradmin: WordBool; safecall;
    function GetIssetupadmin: WordBool; safecall;
    function GetIssysadmin: WordBool; safecall;
    function EnumNTDomainGroups(Domain: OleVariant): IQueryResults; safecall;
    function EnumAccountInfo(Account: OleVariant; ListAll: OleVariant): IQueryResults; safecall;
    function ListMembers(RoleType: DMO_ROLE_TYPE): IVDMOStub; safecall;
    function IsLogin(const LoginName: WideString): WordBool; safecall;
    procedure Abort; safecall;
    function DetachDB(const DBName: WideString; bCheck: WordBool): WideString; safecall;
    function AttachDB(const DBName: WideString; const DataFiles: WideString): WideString; safecall;
    function GetQuotedIdentifier: WordBool; safecall;
    procedure SetQuotedIdentifier(pRetVal: WordBool); safecall;
    function GetLinkedServers: IVDMOStub; safecall;
    procedure SetCodePageOverride(Param1: Integer); safecall;
    function GetFullTextService: IVDMOStub; safecall;
    function GetODBCPrefix: WordBool; safecall;
    procedure SetODBCPrefix(pRetVal: WordBool); safecall;
    procedure Stop; safecall;
    function PingServerVersion(ServerName: OleVariant; Login: OleVariant; Password: OleVariant): DMO_VER; safecall;
    function IsPackage: DMO_PACKAGE_TYPE; safecall;
    function GetRegionalSetting: WordBool; safecall;
    procedure SetRegionalSetting(pRetVal: WordBool); safecall;
    function GetCodePage: Integer; safecall;
    function AttachDBWithSingleFile(const DBName: WideString; const DataFile: WideString): WideString; safecall;
    function IsNTGroupMember(const NTGroup: WideString; const NTUser: WideString): WordBool; safecall;
    function GetServerTime: WideString; safecall;
    function GetTranslateChar: WordBool; safecall;
    procedure SetTranslateChar(pRetVal: WordBool); safecall;
    property Application: IApplication read GetApplication;
    property Parent: IVDMOStdObject read GetParent;
    property UserData: Integer read GetUserData write SetUserData;
    property TypeOf: DMO_OBJECT_TYPE read GetTypeOf;
    property Properties: IProperties read GetProperties;
    property Databases: IDatabases read GetDatabases;
    property Password: WideString read GetPassword write SetPassword;
    property Name: WideString read GetName write SetName;
    property Login: WideString read GetLogin write SetLogin;
    property VersionString: WideString read GetVersionString;
    property BackupDevices: IVDMOStub read GetBackupDevices;
    property VersionMajor: Integer read GetVersionMajor;
    property VersionMinor: Integer read GetVersionMinor;
    property CommandTerminator: WideString read GetCommandTerminator write SetCommandTerminator;
    property TrueName: WideString read GetTrueName;
    property ConnectionID: Integer read GetConnectionID;
    property TrueLogin: WideString read GetTrueLogin;
    property IntegratedSecurity: IVDMOStub read GetIntegratedSecurity;
    property Languages: IVDMOStub read GetLanguages;
    property RemoteServers: IVDMOStub read GetRemoteServers;
    property Logins: IVDMOStub read GetLogins;
    property UserProfile: DMO_SRVUSERPROFILE_TYPE read GetUserProfile;
    property MaxNumericPrecision: Integer read GetMaxNumericPrecision;
    property NextDeviceNumber: Integer read GetNextDeviceNumber;
    property QueryTimeout: Integer read GetQueryTimeout write SetQueryTimeout;
    property LoginTimeout: Integer read GetLoginTimeout write SetLoginTimeout;
    property NetPacketSize: Integer read GetNetPacketSize write SetNetPacketSize;
    property HostName: WideString read GetHostName write SetHostName;
    property ApplicationName: WideString read GetApplicationName write SetApplicationName;
    property LoginSecure: WordBool read GetLoginSecure write SetLoginSecure;
    property ProcessID: Integer read GetProcessID;
    property Status: DMO_SVCSTATUS_TYPE read GetStatus;
    property Registry: IVDMOStub read GetRegistry;
    property Configuration: IVDMOStub read GetConfiguration;
    property JobServer: IVDMOStub read GetJobServer;
    property ProcessInputBuffer[ProcessID: Integer]: WideString read GetProcessInputBuffer;
    property ProcessOutputBuffer[ProcessID: Integer]: WideString read GetProcessOutputBuffer;
    property Language: WideString read GetLanguage write SetLanguage;
    property AutoReConnect: WordBool read GetAutoReConnect write SetAutoReConnect;
    property StatusInfoRefetchInterval[StatusInfoType: DMO_STATUSINFO_TYPE]: Integer read GetStatusInfoRefetchInterval write SetStatusInfoRefetchInterval;
    property SaLogin: WordBool read GetSaLogin;
    property AnsiNulls: WordBool read GetAnsiNulls write SetAnsiNulls;
    property NetName: WideString read GetNetName;
    property Replication: IVDMOStub read GetReplication;
    property EnableBcp: WordBool read GetEnableBcp write SetEnableBcp;
    property BlockingTimeout: Integer read GetBlockingTimeout write SetBlockingTimeout;
    property ServerRoles: IVDMOStub read GetServerRoles;
    property Isdbcreator: WordBool read GetIsdbcreator;
    property Isdiskadmin: WordBool read GetIsdiskadmin;
    property Isprocessadmin: WordBool read GetIsprocessadmin;
    property Issecurityadmin: WordBool read GetIssecurityadmin;
    property Isserveradmin: WordBool read GetIsserveradmin;
    property Issetupadmin: WordBool read GetIssetupadmin;
    property Issysadmin: WordBool read GetIssysadmin;
    property QuotedIdentifier: WordBool read GetQuotedIdentifier write SetQuotedIdentifier;
    property LinkedServers: IVDMOStub read GetLinkedServers;
    property CodePageOverride: Integer write SetCodePageOverride;
    property FullTextService: IVDMOStub read GetFullTextService;
    property ODBCPrefix: WordBool read GetODBCPrefix write SetODBCPrefix;
    property RegionalSetting: WordBool read GetRegionalSetting write SetRegionalSetting;
    property CodePage: Integer read GetCodePage;
    property ServerTime: WideString read GetServerTime;
    property TranslateChar: WordBool read GetTranslateChar write SetTranslateChar;
  end;

{ IServerSink }

  IServerSink = interface(IUnknown)
    ['{10020209-E260-11CF-AE68-00AA004A34D5}']
    function QueryTimeout(const Message: WideString; out Continue: WordBool): HResult; stdcall;
    function ServerMessage(Severity: Integer; MessageNumber: Integer; MessageState: Integer;
      const Message: WideString): HResult; stdcall;
    function ConnectionBroken(const Message: WideString; out Retry: WordBool): HResult; stdcall;
    function RemoteLoginFailed(Severity: Integer; MessageNumber: Integer; MessageState: Integer;
      const Message: WideString): HResult; stdcall;
    function CommandSent(const Command: WideString): HResult; stdcall;
  end;

{ IDatabases }

  IDatabases = interface(IVDMOStdObject)
    ['{10020303-E260-11CF-AE68-00AA004A34D5}']
    function GetApplication: IApplication; safecall;
    function GetParent: IVDMOStdObject; safecall;
    function GetUserData: Integer; safecall;
    procedure SetUserData(pRetVal: Integer); safecall;
    function GetTypeOf: DMO_OBJECT_TYPE; safecall;
    function Item(Index: OleVariant; Owner: OleVariant): IDatabase; safecall;
    function GetNewEnum: IUnknown; safecall;
    function GetCount: Integer; safecall;
    function ItemByID(ID: Integer): IDatabase; safecall;
    procedure Add(const Database: IDatabase); safecall;
    procedure Remove(Index: OleVariant; Owner: OleVariant); safecall;
    procedure Refresh(ReleaseMemberObjects: OleVariant); safecall;
    property Application: IApplication read GetApplication;
    property Parent: IVDMOStdObject read GetParent;
    property UserData: Integer read GetUserData write SetUserData;
    property TypeOf: DMO_OBJECT_TYPE read GetTypeOf;
    property NewEnum: IUnknown read GetNewEnum;
    property Count: Integer read GetCount;
  end;

{ IDatabase - This interface contains the following stubs:

    Defaults
    DatabaseRoles
    FileGroups
    FullTextCatalogs
    Groups
    Rules
    StoredProcedures
    SystemDatatypes
    Tables
    TransactionLog
    Users
    UserDefinedDatatypes
    Views }

  IDatabase = interface(IVDMOStdObject)
    ['{10020306-E260-11CF-AE68-00AA004A34D5}']
    function GetApplication: IApplication; safecall;
    function GetParent: IVDMOStdObject; safecall;
    function GetUserData: Integer; safecall;
    procedure SetUserData(pRetVal: Integer); safecall;
    function GetTypeOf: DMO_OBJECT_TYPE; safecall;
    function GetProperties: IProperties; safecall;
    function GetName: WideString; safecall;
    procedure SetName(const pRetVal: WideString); safecall;
    function GetTables: IVDMOStub; safecall;
    function GetSystemObject: WordBool; safecall;
    function GetID: Integer; safecall;
    function GetUserProfile: DMO_DBUSERPROFILE_TYPE; safecall;
    function GetCreateForAttach: WordBool; safecall;
    procedure SetCreateForAttach(pRetVal: WordBool); safecall;
    function GetOwner: WideString; safecall;
    function GetVersion: Integer; safecall;
    function GetCreateDate: WideString; safecall;
    function GetDataSpaceUsage: Single; safecall;
    function GetUserName: WideString; safecall;
    procedure SetUserName(const pRetVal: WideString); safecall;
    function GetStatus: DMO_DBSTATUS_TYPE; safecall;
    function GetSize: Integer; safecall;
    function GetSpaceAvailable: Integer; safecall;
    function GetIndexSpaceUsage: Single; safecall;
    function GetSpaceAvailableInMB: Single; safecall;
    function GetViews: IVDMOStub; safecall;
    function GetStoredProcedures: IVDMOStub; safecall;
    function GetDefaults: IVDMOStub; safecall;
    function GetRules: IVDMOStub; safecall;
    function GetUserDefinedDatatypes: IVDMOStub; safecall;
    function GetUsers: IVDMOStub; safecall;
    function GetGroups: IVDMOStub; safecall;
    function GetSystemDatatypes: IVDMOStub; safecall;
    function GetTransactionLog: IVDMOStub; safecall;
    function GetDBOption: IVDMOStub; safecall;
    function GetDboLogin: WordBool; safecall;
    procedure Grant(Privileges: DMO_PRIVILEGE_TYPE; const GranteeNames: WideString); safecall;
    procedure Revoke(Privileges: DMO_PRIVILEGE_TYPE; const RevokeeNames: WideString); safecall;
    procedure ExecuteImmediate(const Command: WideString; ExecType: DMO_EXEC_TYPE;
      Length: OleVariant); safecall;
    function GetObjectByName(const ObjectName: WideString; ObjectType: DMO_OBJECT_TYPE;
      Owner: OleVariant): IVDMOStub; safecall;
    procedure Checkpoint; safecall;
    function CheckTables(RepairType: DMO_DBCC_REPAIR_TYPE): WideString; safecall;
    function CheckAllocations(RepairType: DMO_DBCC_REPAIR_TYPE): WideString; safecall;
    function CheckCatalog: WideString; safecall;
    function GetMemoryUsage: WideString; safecall;
    function ExecuteWithResults(const Command: WideString; Length: OleVariant): IQueryResults; safecall;
    function ListObjectPermissions(PrivilegeTypes: DMO_PRIVILEGE_TYPE): IObjectList; safecall;
    function EnumLocks(Who: OleVariant): IQueryResults; safecall;
    function ListObjects(ObjectTypes: DMO_OBJECT_TYPE; SortBy: DMO_OBJSORT_TYPE): IObjectList; safecall;
    function EnumDependencies(DependencyType: DMO_DEPENDENCY_TYPE): IQueryResults; safecall;
    procedure SetOwner(const LoginName: WideString; TransferAliases: OleVariant;
      OverrideIfAlreadyUser: OleVariant); safecall;
    function ListDatabasePermissions(PrivilegeTypes: DMO_PRIVILEGE_TYPE): IObjectList; safecall;
    procedure Remove; safecall;
    procedure RecalcSpaceUsage; safecall;
    function EnumCandidateKeys: IQueryResults; safecall;
    function IsValidKeyDatatype(const KeyColType: WideString; ReferencingColType: OleVariant): WordBool; safecall;
    function GetDatatypeByName(const TypeName: WideString): IVDMOStdObject; safecall;
    procedure Transfer(const TransferSpec: IVDMOStub); safecall;
    function ScriptTransfer(const TransferSpec: IVDMOStub;
      ScriptFileMode: DMO_XFRSCRIPTMODE_TYPE; ScriptFilePath: OleVariant): WideString; safecall;
    procedure CheckIdentityValues; safecall;
    function ExecuteWithResultsAndMessages(const Command: WideString; Length: OleVariant;
      out Messages: WideString): IQueryResults; safecall;
    function Script(ScriptType: DMO_SCRIPT_TYPE; ScriptFilePath: OleVariant;
      Script2Type: DMO_SCRIPT2_TYPE): WideString; safecall;
    function CheckTablesDataOnly: WideString; safecall;
    function CheckAllocationsDataOnly: WideString; safecall;
    procedure UpdateIndexStatistics; safecall;
    function EnumLoginMappings: IQueryResults; safecall;
    function GetPrimaryFilePath: WideString; safecall;
    function GetFileGroups: IVDMOStub; safecall;
    function GetDatabaseRoles: IVDMOStub; safecall;
    function GetPermissions: HResult; safecall;
    function GetIsdbaccessadmin: WordBool; safecall;
    function GetIsdbdatareader: WordBool; safecall;
    function GetIsdbddladmin: WordBool; safecall;
    function GetIsdbdenydatareader: WordBool; safecall;
    function GetIsdbdenydatawriter: WordBool; safecall;
    function GetIsdbbackupoperator: WordBool; safecall;
    function GetIsdbowner: WordBool; safecall;
    function GetIsdbsecurityadmin: WordBool; safecall;
    function GetIsdbdatawriter: WordBool; safecall;
    function EnumFiles: IQueryResults; safecall;
    function EnumFileGroups: IQueryResults; safecall;
    function EnumUsers(Who: OleVariant): IQueryResults; safecall;
    function EnumNTGroups(Who: OleVariant): IQueryResults; safecall;
    procedure Deny(Privileges: DMO_PRIVILEGE_TYPE; const DenyeeNames: WideString); safecall;
    function IsUser(const UserName: WideString): WordBool; safecall;
    function Generate: WideString; safecall;
    procedure Shrink(FreeSpaceInPercent: Integer; Truncate: DMO_SHRINK_TYPE); safecall;
    function CheckTextAllocsFast: WideString; safecall;
    function CheckTextAllocsFull: WideString; safecall;
    function EnumMatchingSPs(const Text: WideString; IncludeSystemSP: OleVariant): IQueryResults; safecall;
    procedure EnableFullTextCatalogs; safecall;
    procedure RemoveFullTextCatalogs; safecall;
    function FullTextIndexScript: WideString; safecall;
    function GetIsFullTextEnabled: WordBool; safecall;
    function GetFullTextCatalogs: IVDMOStub; safecall;
    procedure DisableFullTextCatalogs; safecall;
    function GetCompatibilityLevel: DMO_COMP_LEVEL_TYPE; safecall;
    procedure SetCompatibilityLevel(pRetVal: DMO_COMP_LEVEL_TYPE); safecall;
    function GetUseServerName: WideString; safecall;
    procedure SetUseServerName(const pRetVal: WideString); safecall;
    property Application: IApplication read GetApplication;
    property Parent: IVDMOStdObject read GetParent;
    property UserData: Integer read GetUserData write SetUserData;
    property TypeOf: DMO_OBJECT_TYPE read GetTypeOf;
    property Properties: IProperties read GetProperties;
    property Name: WideString read GetName write SetName;
    property Tables: IVDMOStub read GetTables;
    property SystemObject: WordBool read GetSystemObject;
    property ID: Integer read GetID;
    property UserProfile: DMO_DBUSERPROFILE_TYPE read GetUserProfile;
    property CreateForAttach: WordBool read GetCreateForAttach write SetCreateForAttach;
    property Owner: WideString read GetOwner;
    property Version: Integer read GetVersion;
    property CreateDate: WideString read GetCreateDate;
    property DataSpaceUsage: Single read GetDataSpaceUsage;
    property UserName: WideString read GetUserName write SetUserName;
    property Status: DMO_DBSTATUS_TYPE read GetStatus;
    property Size: Integer read GetSize;
    property SpaceAvailable: Integer read GetSpaceAvailable;
    property IndexSpaceUsage: Single read GetIndexSpaceUsage;
    property SpaceAvailableInMB: Single read GetSpaceAvailableInMB;
    property Views: IVDMOStub read GetViews;
    property StoredProcedures: IVDMOStub read GetStoredProcedures;
    property Defaults: IVDMOStub read GetDefaults;
    property Rules: IVDMOStub read GetRules;
    property UserDefinedDatatypes: IVDMOStub read GetUserDefinedDatatypes;
    property Users: IVDMOStub read GetUsers;
    property Groups: IVDMOStub read GetGroups;
    property SystemDatatypes: IVDMOStub read GetSystemDatatypes;
    property TransactionLog: IVDMOStub read GetTransactionLog;
    property DBOption: IVDMOStub read GetDBOption;
    property DboLogin: WordBool read GetDboLogin;
    property PrimaryFilePath: WideString read GetPrimaryFilePath;
    property FileGroups: IVDMOStub read GetFileGroups;
    property DatabaseRoles: IVDMOStub read GetDatabaseRoles;
    property Permissions: HResult read GetPermissions;
    property Isdbaccessadmin: WordBool read GetIsdbaccessadmin;
    property Isdbdatareader: WordBool read GetIsdbdatareader;
    property Isdbddladmin: WordBool read GetIsdbddladmin;
    property Isdbdenydatareader: WordBool read GetIsdbdenydatareader;
    property Isdbdenydatawriter: WordBool read GetIsdbdenydatawriter;
    property Isdbbackupoperator: WordBool read GetIsdbbackupoperator;
    property Isdbowner: WordBool read GetIsdbowner;
    property Isdbsecurityadmin: WordBool read GetIsdbsecurityadmin;
    property Isdbdatawriter: WordBool read GetIsdbdatawriter;
    property IsFullTextEnabled: WordBool read GetIsFullTextEnabled;
    property FullTextCatalogs: IVDMOStub read GetFullTextCatalogs;
    property CompatibilityLevel: DMO_COMP_LEVEL_TYPE read GetCompatibilityLevel write SetCompatibilityLevel;
    property UseServerName: WideString read GetUseServerName write SetUseServerName;
  end;

{ IQueryResults }

  IQueryResults = interface(IVDMOStdObject)
    ['{10022506-E260-11CF-AE68-00AA004A34D5}']
    function GetApplication: IApplication; safecall;
    function GetParent: IVDMOStdObject; safecall;
    function GetUserData: Integer; safecall;
    procedure SetUserData(pRetVal: Integer); safecall;
    function GetTypeOf: DMO_OBJECT_TYPE; safecall;
    function GetProperties: IProperties; safecall;
    function GetResultSets: Integer; safecall;
    function GetCurrentResultSet: Integer; safecall;
    procedure SetCurrentResultSet(pRetVal: Integer); safecall;
    function GetRows: Integer; safecall;
    function GetColumns: Integer; safecall;
    function GetColumnName(Column: Integer): WideString; safecall;
    function GetColumnType(Column: Integer): DMO_QUERY_DATATYPE; safecall;
    function GetColumnMaxLength(Column: Integer): Integer; safecall;
    function GetColumnLong(Row: Integer; Column: Integer): Integer; safecall;
    function GetColumnBool(Row: Integer; Column: Integer): WordBool; safecall;
    function GetColumnFloat(Row: Integer; Column: Integer): Single; safecall;
    function GetColumnDouble(Row: Integer; Column: Integer): Double; safecall;
    function GetColumnString(Row: Integer; Column: Integer): WideString; safecall;
    function GetRangeString(Top: OleVariant; Left: OleVariant; Bottom: OleVariant; 
      Right: OleVariant; RowDelim: OleVariant; ColDelim: OleVariant; 
      ColWidths: OleVariant): WideString; safecall;
    procedure Refresh; safecall;
    function GetColumnDate(Row: Integer; Column: Integer): TDateTime; safecall;
    function GetColumnBinary(Row: Integer; Column: Integer): PSafeArray; safecall;
    function GetColumnBinaryLength(Row: Integer; Column: Integer): Integer; safecall;
    function GetColumnGUID(Row: Integer; Column: Integer): PSafeArray; safecall;
    property Application: IApplication read GetApplication;
    property Parent: IVDMOStdObject read GetParent;
    property UserData: Integer read GetUserData write SetUserData;
    property TypeOf: DMO_OBJECT_TYPE read GetTypeOf;
    property Properties: IProperties read GetProperties;
    property ResultSets: Integer read GetResultSets;
    property CurrentResultSet: Integer read GetCurrentResultSet write SetCurrentResultSet;
    property Rows: Integer read GetRows;
    property Columns: Integer read GetColumns;
    property ColumnName[Column: Integer]: WideString read GetColumnName;
    property ColumnType[Column: Integer]: DMO_QUERY_DATATYPE read GetColumnType;
    property ColumnMaxLength[Column: Integer]: Integer read GetColumnMaxLength;
  end;

implementation

end.
