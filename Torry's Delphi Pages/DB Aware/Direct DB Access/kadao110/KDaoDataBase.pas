unit KDaoDataBase;
{$B-}
//******************************************************************************
//                           Delphi Dao Project
//                 Copyright (c) 2000-2001 by Kiril Antonov
//******************************************************************************
{$DEFINE USEDB}          //DISABLE IF YOU WANT TO USE PURE DAO WITHOUT KDaoTable
{$I KADaoCommonDirectives.pas}
//****************************************** CHANGES ***************************
// 30.05.2000 - Added a checking of database for supporting transactions
//              If database does NOT support transactions
//              NO DAO action is performed
// 08.06.2000 - Adding support for Dynamycally setting DAO Version
//
// 12.06.2000 - Fixed a login bug for ISAM databases
//
// 14.06.2000 - Added support for creating autoincrement fields
//              How to use:
//                 Use constant dbAutoIncInteger for FieldType
//              Restrictions: (This is a DAO restrictions not the component!)
//                 No retrictions when creating new table (BUT ONLY ONE AutoInc
//                    Field per table)
//                 Only ONE AutoInc Field per table
//
// 14.06.2000 - Renamed F_RefreshDefinitions to RefreshDefinitions
//
// 18.06.2000 - Fixed a bug with setting Dao Version when TKADaoDatabase is
//              created.
//              WARNING!!! INITIAL VERSION OF KADAO IS SET TO 3.5 NOW!
// 19.06.2000 - Fixed a minor bug when a database control is deleted
//              Now all tables linked to KADaoDatabase control work properly
//              when control is deleted
//
// 26.06.2000 - Added Idle method to acces DBEngine Idle
//
// 26.06.2000 - Rewrited DaoVersion and SystemDatabase properties
//
// 27.06.2000 - Rewrited EngineType property
//
// 28.06.2000 - Minor fix: now CoreWorkspace is closed each time a new workspace
//              is created
//
// 28.06.2000 - Added read only property DatabaseLanguage for information
//              purpouses. If you want a LocaleCode DatabaseLanguageInt contains
//              them
// 28.06.2000 - Added CompactAccessDatabaseEx - No comment see code
//              Seee also new Language constants in DaoApi.pas
//
// 28.06.2000 - Added CreateAccessDatabaseEx2 - Seee new Language constants
//              in DaoApi.pas
//
// 29.06.2000 - Added CheckEngines method for avoiding exceptions when checking
//              available versions of DAO
//
// 29.06.2000 - Added F_FindWorkspace method for avoiding exceptions when
//              creating a new workspace
//
// 03.07.2000  CreateTable and AddFieldsToTable rewrited
//             Still problems with creating Paradox primary index - HELP NEEDED!
// 05.07.2000 - Fixed a very rediculous bug with Version property
//              It seems that a 4 July is a day of shame for me!
//
// 17.07.2000 - Added LinkExternalTable - Now Tables of various types can be
//              linked to Access database
//              See _PredefinedTableTypes in DaoApi
//              for information about TableType parameter
//
// 20.07.2000 - Added Open and Close methods
//
// 20.07.2000 - Added OnLogin Event - same as Borland's
//
// 24.07.2000 - Added tynny support for ODBC - No more Support for ODBC in
//              future versions. Remember that DAO allows use of ODBC only if
//              it has no ISAM driver for that type of Database.
//
// 27.07.2000 - Finnaly KADao CAN make Paradox table with Indexes
//              BORLAND RESTRICTIONS WHICH APPLY HERE:
//                - First field must be Primary Index
//                - Unique Indexes can be created only using Paradox 7.X ISAM
//                  driver wich is available only with DAO 3.6
//                - All fields that are in PrimaryKey index must follow the
//                  first field
//
// 27.07.2000 - Added small code to read again List of ISAM drivers when
//              changing from DAO 3.5 to 3.6 and vice versa
//
// 30.07.2000 - Changed CreateTable to support Required field property
//
// 30.07.2000 - Changed AddFieldstoTable to support Required field property
//
// 30.07.2000 - Added new class - TKADaoTableManager wich manages creating of
//              fields and indexes like the TTable
//              See Demos for more info
// 31.07.2000 - Maybe a Problem with Access security is solved
//              A few steps are required to set SystemDatabase and login
//                 1. Set Username property to valid username
//                 2. Set Password property to valid password
//                 3. Select system database
//                 4. Set other properties and finally set connected to true
//             WARNING! It is reccomennded to set new property PrivateEngine to
//                    TRUE if you will use more then one KADaoDatabase component
//                    in your project
//
// 18.08.2000 - Fixed a bug with Username/Password protection when using global
//              Database password. With many thanks to "Joop" for reporting the
//              problem
//
// 22.08.2000 - F_FindWorkspace method moved to public methods as FindWorkspace
//
// 24.08.2000 - Added ChangeDatabasePassword method for changing DB password
//              Note: This is global database password
//
// 29.08.2000  - Added some code for QueryDefTimeOut
//
// 30.08.2000  - Added New Property UsesDynadao - True when DYNADAO is DEFINED
//
// 31.08.2000  - Added new Property DatabasePassword for Databases protected
//               with both User Level Password and Global Database Password
//               Also a login dialog and ONLogin Event are changed to support
//               this
//               When Database is MSAccess Database both Password And
//               DatabasePassword are used and DatabasePassword is used to
//               open Database otherwise a Password is used to Open Database
//
// 31.08.2000  - Changes made to CompactAccessDatabase and
//               RepairAccessDatabase to support Password (send as new parameter)
//
// 07.09.2000  - Added few fixes in Create method - Many thanks to Oliver Häger
//
// 07.09.2000  - Added new method ChooseDatabase for displaying dialogs as in
//               property editor - thanks again to Oliver Häger
//
// 07.09.2000  - Fixed a bug in CompactAccessDatabase/CompactAccessDatabaseEx
//               Many thanks to Ingmar Bode for reporting the problem
//
// 21.09.2000  - Added Params property similar to TDatabase.Params
//
// 21.09.2000  - Added CreateEmptyTable method - Creates an Empty table
//
// 22.09.2000  - Added new Property DSNFileNames (TStringList) containing
//               File Names of the DSN's which have such
//               Format is DSN=FullPathFileName
//
// 01.10.2000  - Adjusted number of calls to RecreateCore when component loads
//               Now it is called two times not 10!
//                  1. At setting workspace property
//                  2. At connecting to database
//
// 02.10.2000  - Added Additional code for DAO testing
//               Also added support for easy creation of DBEngine
//               thanks to Oliver Häger.
//
// 02.10.2000  - Added RefreshLink Method to support DAO method with
//               the same name. It can refres a link to external table
//               previously created by LinkExternalTable method
//
// 02.10.2000  - Added RegisterDatabase Method to support DAO method with
//               the same name. It is useful for creating ODBC links
//               If the database is already registered
//               in the Windows Registry the connection information is updated.
//
// 02.10.2000  - Added Support for ODBCDirect workspaces (ONLY WITH DYNADAO!)
//               Unfortunately DAO restricts enumerating of tables in
//               ODBCDirect Connections
//               To use ODBCDirect do the following
//                 1. Set DatabaseType to ODBC
//                 2. Set EngineType to dbUseODBC
//                 3. Set Database property
//                 4. Set Connected to TRUE
//                 5. In KadaoTable set TableType to DynamicTable
//                 6. In KadaoTable MANUALY type the Table name in TableName
//                    property and enclose in squire brackets []
//                 7. In KadaoTable set OpenOptions to none or only to options
//                    supported by ODBCDirect connection (for more see DAO help)
//                 8. In KadaoTable set Active to TRUE
//
// 05.10.2000  - Added Minor fixes in CheckEngines routine
//
// 09.10.2000  - Added six new methods supporting transactions at
//               DBEngine and Workspace level
//               The standart methods are at Database level
//
//                      DBEngineLevel_StartTransaction;
//                      DBEngineLevel_Commit;
//                      DBEngineLevel_Rollback;
//                      WorkspaceLevel_StartTransaction;
//                      WorkspaceLevel_Commit;
//                      WorkspaceLevel_Rollback;
//
// 17.10.2000  - Added CloseDatasets method same as TDatabase.CloseDatasets
//
//******************************************************************************
//
// 25.10.2000  - Found a bug in Rollback method-table rasies 'No current record'
//               after rollback - now fixed thanks to Sergey
//
// 25.10.2000  - RecreateCore is now Public method. It is usefull for
//               console applications
//
// 27.10.2000  - Added a small patch in TKADaoTableManager.CreateIndex to avoid
//               creation of PrimaryKeyIndex again - thanks to Leo Verdú
//
// 31.10.2000  - Fixed a bug in designtime of security - now all works fine
//
// 31.10.2000  - Added new property SaveUsername - default to true
//               When set to True login dialog shows the Username otherwise
//               Username is blank
//
// 31.10.2000  - PrivateEngine is now True by default
//
// 01.11.2000  - For VERY NOT ORIENTED people added property VersionInfo
//               No more comments!!! 
//
// 14.11.2000  - Added changes to support reading the Registry in ReadOnly mode
//
//******************************************************************************
//
// 05.12.2000  - All Error messages are moved to resourcestring so you can
//               localize your KADAO.
//               Errors between 1000 and 1999 are rezerved for KADaoDatabase
//               Errors between 2000 and 2999 are rezerved for KADaoTable
//
//******************************************************************************
//
// 05.12.2000  - Added two new methods
//                 - GoOffline - it gives opportunity to set
//                    UserName, Password, SystemDatabase, EngineType
//                    and some other parameters BEFORE call to RecreateCore
//                 - GoOnline - restores the standart database state
//               See Help for details.
//
// 12.12.2000  - Fixed small bug in handling User logging to the database
//
//******************************************************************************
//
// 07.01.2001  - Fixed small bug on CreateEmptyTable
//
//******************************************************************************
//
// 14.01.2000 - Fixed a smal problem with Registry in Delphi5 without DYNADAO
//                                                                                  
//******************************************************************************
//
// 19.01.2001 - Fixed a bug in retrieving registry data in F_Get_DBTypesList
// 19.01.2001 - Made changes to code - now exceptions are not raised on
//              Non-MSAccess databases in Refresh Definitions
//
//******************************************************************************
//
// 28.01.2001 - Added new property MDBVersionAutoDetect
//              False by default - When this property is True and DYNADAO
//              is used then KADao automatically selects DAO 3.6 if
//              MDB is Access 4.0 file. Based on Andrew Baylis Idea.
//
// 23.02.2001 - Fixed a DeleteField bug. Thanks to Simone.
//
//******************************************************************************
//
// 09.03.2001 - PrivateEngine Default value is now setto False.
//
// 11.03.2001 - Found a bug in processing PrimaryKey indexes - now fixed.
//
// 14.03.2001 - Added RepairAccessDatabaseEx method - thanks to Mark Hamilton
//
// 14.03.2001 - Added ModifyQueryDef method.
//
//******************************************************************************
//
// 28.03.2001 - Fixed a bug in the Destructor;
//
// 04.04.2001 - Fixed a bug in the Constructor - thanks to Thomas Seban for
//              reporting the problem
//              Bug appearcs when a line like
//              db:=TKADaoDatabase.Create(Nil) is executed.
//******************************************************************************
//
// 15.05.2001 - Fixed a bug in LinkExternalTable - thanks to Adam Abas
//              for reporting the problem
//
//******************************************************************************
//
// 29.05.2001 - Fixed a bug in AddFieldsToTable
//
// 29.05.2001 - Now KADaoTableManages supports creation of Primarykey indexes
//              with name different then "PrimaryKey"
//
//******************************************************************************
// 24.07.2001 - Fixed a bug in StartTransaction, Rollback and Commit
//              Bug appears only when using DAO 3.6 and not affecrs DAO 3.5
//******************************************************************************
//
// 14.08.2001 - Fixed a bug in security system
//              In special conditions users cannot set security parameters
//              BEFORE all properties are set.
//              Now after loading of the component RecreateCore is called to
//              ensure that all properies are applyed to Workspace and Engine.
//
//******************************************************************************
//
// 25.09.2001 - Added 4 new events
//                    - OnBeforeConnect
//                    - OnAfterConnect
//                    - OnBeforeDisconnect
//                    - OnAfterDisconnect
//
//
// 15.10.2001 - Added new property - SmartOpen - True by default
//              When SmartOpen is true KADAODatabase first try to find mdb
//              file with the filename specifyed in design time
//              If filename does not exists KADAODatabase tryes to find same
//              file in the program's startup folder.
//
//******************************************************************************
//
// 26.11.2001 - Added new property - DatabaseVersion - ReadOnly
//              This property gives information for the version of DAO used to
//              create opened database.
//              For example you must use DAO 3.6 to open 3.51 database
//              In this case:
//                  Version property          = 3.6
//                  DatabaseVersion  property = 3.5
//
// 27.11.2001 - Added new property - DatabaseParameters - String
//              This property can contain some additional information for
//              opening an database.
//              For example when openning an Excel file
//              DatabaseParameters can contain "HDR=NO; IMEX=1;"
//
//******************************************************************************
//
// 05.12.2001 - Added new method GetDAOEnginesInstalled:TStringList
//              It returns all installed dao versions on the target computer.
//
//******************************************************************************
//
// 28.03.2002 - Added new method RefreshDatasets
//              When called all tables connected to the database are refreshed
//              using TKadaoTable.RefreshData method.
//
//******************************************************************************
interface
Uses
DAOApi,
ComObj,
{$IFDEF DAO35}
DAO35Api,
{$ENDIF}
{$IFDEF DAO36}
DAO36Api,
{$ENDIF}
{$IFDEF DAO120}
DAO120Api,
{$ENDIF}
Windows, SysUtils, Classes, FileCtrl, DbLoginUnit, Registry, TypInfo, DaoAddOns
{$IFDEF USEDB}, DB, KADaoDummyDataset, DaoUtils{$ENDIF}
{$IFDEF D6UP}, Variants {$ENDIF};
//******************************************************* DatabaseError Messages
{$I ErrLangDB.pas}
//******************************************************************************

Const
  szUSERNAME   = 'USER NAME';
  szPASSWORD   = 'PASSWORD';
  szDBPASSWORD = 'DBPASSWORD';
Type
 TKADaoDatabase=Class;

 TDaoErrRec=Record
            ErrNo       : Integer;
            Source      : String;
            Description : String;
            HelpFile    : String;
            HelpContext : Integer;
          End;
 PDaoErrRec=^TDaoErrRec;

 TLoginEvent    = procedure(Database: TKADaoDatabase; LoginParams: TStrings) of object;
 TConnectEvent  = procedure(Database: TKADaoDatabase) of object;

 TKADaoDatabase = Class(TComponent)
       Private
         F_RuntimeLicense     : String;
         F_Database           : String;
         F_DatabaseParameters : String;
         F_SmartOpen          : Boolean;
         F_EngineType         : Integer;
         F_PrivateEngine      : Boolean;
         F_DatabaseType       : String;
         F_Workspace          : String;
         F_CollatingOrder     : String;
         F_DaoVersion         : String;
         F_ActualDaoVersion   : String;
         F_DatabaseVersion    : String;
         F_VersionInfo        : String;
         F_SystemDB           : String;
         F_Active             : Boolean;
         F_ReadOnly           : Boolean;
         F_Exclusive          : Boolean;
         F_LoginPrompt        : Boolean;
         F_AutoDetectMDB      : Boolean;
         F_Username           : String;
         F_Password           : String;
         F_DatabasePassword   : String;
         F_SaveUserName       : Boolean;
         F_MachineName        : String;
         F_QueryTimeout       : Integer;
         F_LoginDialog        : TDbLogin;
         F_TableNames         : TStringList;
         F_ActiveTableNames   : TStringList;
         F_QueryDefNames      : TStringList;
         F_DriverList         : TStringList;
         F_SystemDSNs         : TStringList;
         F_UserDSNs           : TStringList;
         F_DSNFileNames       : TStringList;
         F_Params             : TStringList;
         F_OLE_ON             : Boolean;
         F_OnLogin            : TLoginEvent;
         F_BeforeConnect      : TConnectEvent;
         F_AfterConnect       : TConnectEvent;
         F_BeforeDisconnect   : TConnectEvent;
         F_AfterDisconnect    : TConnectEvent;
         F_DynaDao            : Boolean;
         F_Offline            : Boolean;
         F_ShowSysObjects     : Boolean;

         F_TransInfo          : TStringList;
         F_TrackTransactions  : Boolean;

         F_ComponentVersion   : String;
         F_DefaultCursorDriver: Integer;
         F_UseODBCDialog      : Boolean;

         procedure F_Get_DBTypesList(List: TStrings);
         Function  F_Get_DBTypeFileExtension(DBType:String):String;
         Function  F_Get_DBTypeTableType(DBType:String):String;
         Function  F_Get_ODBCFileName(DSN:String;SystemWideDSN:Boolean):String;
         procedure F_FillDSNFileNames(List: TStrings);
         procedure F_Get_OdbcDriverList(List: TStrings);
         procedure F_Get_SystemDSNs(DSNs: TStrings);
         procedure F_Get_UserDSNs(DSNs: TStrings);
         Procedure F_Set_DaoVersion(Value : String);
         Procedure F_Set_ActualDaoVersion(Value : String);
         Procedure F_Set_DatabaseVersion(Value : String);
         Procedure F_Set_VersionInfo(Value : String);
         Procedure F_Set_Database(Value : String);
         Procedure F_Set_DatabaseParameters(Value : String);
         Function  F_Get_SystemDatabaseFromRegistry:String;
         Procedure F_Set_SystemDatabase(Value : String);
         Procedure F_Set_Workspace(Value : String);
         Function  F_Get_DatabaseType:String;
         Procedure F_Set_DatabaseType(Value : String);
         Function  F_Get_CollatingOrder:String;
         Procedure F_Set_EngineType(Value : Integer);
         Procedure F_Set_PrivateEngine(Value : Boolean);
         Procedure F_Set_ShowSysObjects(Value : Boolean);
         Procedure F_Set_UserName(Value : String);
         Procedure F_Set_Password(Value : String);
         Procedure F_Set_DatabasePassword(Value : String);
         Procedure F_Set_Exclusive(Value : Boolean);
         Procedure F_Set_LoginPrompt(Value : Boolean);
         Procedure F_Set_ReadOnly(Value : Boolean);
         Procedure F_Set_DynaDao(Value: Boolean);
         Procedure F_Set_ComponentVersion(Value: String);
         Procedure F_Set_Params(Value : TStringList);
         Procedure F_Set_DefaultCursorDriver(Value : Integer);
         Procedure F_Set_Active(Value : Boolean);
         Procedure F_Set_TrackTransactions(Value : Boolean);
         Function  F_GetTableRN(Tables:String;TableName:String):String;

       Protected
         Procedure                     CreateDBEngine(DaoVer:String);
         Procedure                     Loaded; override;
       Public
         //********************************* Public for Property Editors request
         F_DBTypesList               : TStringList;
         F_DaoVersionList            : TStringList;
         //*********************************************************************
         DatabaseLanguageInt         : Integer;
         {$IFDEF DYNADAO} //****************************************************
         CoreDBEngine                : OleVariant;
         CoreDatabase                : OleVariant;
         CoreWorkspace               : OleVariant;
         {$ENDIF}
         {$IFDEF DAO35}
         CoreDBEngine                : DAO35Api.DBEngine;
         CoreDatabase                : DAO35Api.Database;
         CoreWorkspace               : DAO35Api.Workspace;
         {$ENDIF}
         {$IFDEF DAO36}
         CoreDBEngine                : DAO36Api.DBEngine;
         CoreDatabase                : DAO36Api.Database;
         CoreWorkspace               : DAO36Api.Workspace;
         {$ENDIF}
         {$IFDEF DAO120}
         CoreDBEngine                : DAO120Api.DBEngine;
         CoreDatabase                : DAO120Api.Database;
         CoreWorkspace               : DAO120Api.Workspace;
         {$ENDIF}
         Property    Params          : TStringList Read F_Params Write F_Set_Params;
         Property    DSNFileNames    : TStringList Read F_DSNFileNames;
         Property    QueryDefNames   : TStringList Read F_QueryDefNames;
         Property    TableNames      : TStringList Read F_TableNames;
         Property    ActiveTableNames: TStringList Read F_ActiveTableNames;
         Property    DatabaseTypes   : TStringList Read F_DBTypesList;

         //********************************* Public for Property Editors request
         Function  F_ChooseDatabase  : String;
         //*********************************************************************

         {$IFDEF DYNADAO}
         Function                      CreateOleDBEngine(const ClassName: string): IDispatch;
         {$ELSE}
         Function                      CreateOleDBEngine(const ClassID: TGUID): DBEngine;
         {$ENDIF}
         Function                      CreateOleDBEngine_II(const ClassName: string): IDispatch;
         Function                      GetDAOEnginesInstalled:TStringList;
         Procedure                     CheckEngines;
         Procedure                     DetectMDB(DatabasePath:String);
         Function                      GetLastDaoError:TDaoErrRec;
         Constructor                   Create(AOwner : TComponent); override;
         Destructor                    Destroy; override;

         //****************************************************** Online/Offline
         Procedure   GoOffline;
         Procedure   GoOnline;
         //****************************************************** Transactions
         Procedure                   StartTransaction;
         Procedure                   Commit;
         Procedure                   Rollback;
         Procedure                   RollbackRefresh;
         Procedure                   AddRNToTransaction(TableName : String;RN:Integer);

         Procedure                   DBEngineLevel_StartTransaction;
         Procedure                   DBEngineLevel_Commit;
         Procedure                   DBEngineLevel_Rollback;

         Procedure                   WorkspaceLevel_StartTransaction;
         Procedure                   WorkspaceLevel_Commit;
         Procedure                   WorkspaceLevel_Rollback;

         Function                    GetTransactionCount:Integer;

         //****************************************************** Utils
         Procedure                   RepairAccessDatabase  (DatabaseName,Password:String);
         Procedure                   RepairAccessDatabaseEx(DatabaseName : String;
                                                            NewLocale    : String;
                                                            Encrypt      : Boolean;
                                                            Decrypt      : Boolean;
                                                            NewVersion   : Integer;
                                                            Password     : String);
         Procedure                   CompactAccessDatabase  (DatabaseName,Password:String);
         Procedure                   CompactAccessDatabaseEx(DatabaseName: String;
                                                             NewLocale   : String;
                                                             Encrypt     : Boolean;
                                                             Decrypt     : Boolean;
                                                             NewVersion  : Integer;
                                                             Password    : String);

         Procedure                   CreateAccessDatabase    (DatabaseName:String);
         Procedure                   CreateAccessDatabaseEx  (DatabaseName,LANGID,CP,COUNTRY,Password,Version:String;Encrypt:Boolean);
         Procedure                   CreateAccessDatabaseEx2 (DatabaseName,Language,Password,Version:String;Encrypt:Boolean);
         //****************************************************** Utils II
         Function                    ChangeDatabasePassword(OldPassword,NewPassword:String):Boolean;
         Function                    RegisterDatabase(DatabaseName, DriverName:String; Silent:Boolean; Attributes:String):Boolean;
         Function                    CreateEmptyTable(TableName:String):Boolean;
         Function                    CreateTable(TableName:String; FieldNames : Variant; FieldTypes : Variant; FieldSizes : Variant; FieldIndexes:Variant; FieldsRequired:Variant):Boolean;
         Function                    AddFieldsToTable(TableName:String; FieldNames : Variant; FieldTypes : Variant; FieldSizes : Variant; FieldIndexes:Variant; FieldsRequired:Variant):Boolean;
         Procedure                   LinkExternalTable(Database,TableName,TableType:String;TableAttributes:Integer);
         Procedure                   LinkExternalTableEx(Database,TableName,TableFileName,TableType:String;TableAttributes:Integer);
         Procedure                   RefreshLink(Database,TableName,TableType:String);

         Procedure                   RenameTable(OldTableName,NewTableName:String);
         Function                    EmptyTable(TableName:String):Boolean;
         Procedure                   DeleteTable(TableName:String);

         Function                    HasPrimaryKey(NewTable:OleVariant):Boolean;
         Function                    TablePrimaryKeyName(NewTable:OleVariant):String;
         Procedure                   DeletePrimaryKey(NewTable:OleVariant);

         Function                    CreateIndex(TableName,FieldName:String;IndexType:Integer):Boolean;
         Procedure                   RenameIndex(TableName,OldIndexName,NewIndexName:String);
         Procedure                   DeleteIndexByName(TableName,IndexName:String);
         Procedure                   DeleteIndexByFieldName(TableName,FieldName:String);

         Procedure                   RenameField(TableName,OldFieldName,NewFieldName:String);
         Procedure                   DeleteField(TableName,FieldName:String);



         Function                    CreateQueryDef(Name:String;SQL:String):Boolean;
         Procedure                   ModifyQueryDef(Name:String;SQL:String);
         Function                    GetQueryDefSQLText(Name:String):String;
         Procedure                   RenameQueryDef(OldQueryName,NewQueryName:String);
         Procedure                   DeleteQueryDef(QueryName:String);

         Function                    FindWorkspace(WS:String):Boolean;
         Procedure                   RefreshDefinitions;
         Procedure                   Idle;

         Procedure                   Open;
         Procedure                   Close;
         Procedure                   CloseDatasets;
         Procedure                   RefreshDatasets;
         Function                    ChooseDatabase: Boolean;

         Procedure                   RecreateCore;
      Published
         Property ComponentVersion     : String  Read F_ComponentVersion Write F_Set_ComponentVersion;
         Property Exclusive            : Boolean Read F_Exclusive Write F_Set_Exclusive;
         Property DatabaseLanguage     : String  Read F_Get_CollatingOrder Write F_CollatingOrder;
         Property DatabaseType         : String  Read F_Get_DatabaseType Write F_Set_DatabaseType;
         Property Database             : String  Read F_Database Write F_Set_Database;
         Property DatabaseParameters   : String  Read F_DatabaseParameters Write F_Set_DatabaseParameters;
         Property DatabaseVersionInfo  : String  Read F_DatabaseVersion   Write F_Set_DatabaseVersion;
         Property ReadOnly             : Boolean Read F_ReadOnly Write F_Set_ReadOnly;
         Property LoginPrompt          : Boolean Read F_LoginPrompt Write F_Set_LoginPrompt;
         Property UserName             : String  Read F_Username  Write F_Set_UserName;
         Property UseODBCDialog        : Boolean Read F_UseODBCDialog  Write F_UseODBCDialog;
         Property Password             : String  Read F_Password  Write F_Set_Password;
         Property DatabasePassword     : String  Read F_DatabasePassword Write F_Set_DatabasePassword;
         Property SystemDatabase       : String  Read F_SystemDB Write F_Set_SystemDatabase;
         Property SaveUserName         : Boolean Read F_SaveUserName Write F_SaveUserName;
         Property ShowSystemObjects    : Boolean Read F_ShowSysObjects Write F_Set_ShowSysObjects;
         Property SmartOpen            : Boolean Read F_SmartOpen Write F_SmartOpen;
         Property EngineType           : Integer Read F_EngineType Write F_Set_EngineType;
         Property PrivateEngine        : Boolean Read F_PrivateEngine Write F_Set_PrivateEngine;
         Property TrackTransactions    : Boolean Read F_TrackTransactions Write F_Set_TrackTransactions;
         Property UsesDynaDao          : Boolean Read F_DynaDao Write F_Set_DynaDao;
         Property Version              : String  Read F_DaoVersion Write F_Set_DaoVersion;
         Property VersionDetails       : String  Read F_ActualDaoVersion Write F_Set_ActualDaoVersion;
         Property VersionInfo          : String  Read F_VersionInfo Write F_Set_VersionInfo;
         Property Workspace            : String  Read F_Workspace Write F_Set_Workspace;
         Property DefaultCursorDriver  : Integer Read F_DefaultCursorDriver Write F_Set_DefaultCursorDriver;
         Property QueryTimeout         : Integer Read F_QueryTimeout Write F_QueryTimeout;
         Property MdbVersionAutoDetect : Boolean Read F_AutoDetectMDB Write F_AutoDetectMDB;
         Property OnLogin              : TLoginEvent   Read F_OnLogin Write F_OnLogin;
         Property OnBeforeConnect      : TConnectEvent Read F_BeforeConnect Write F_BeforeConnect;
         Property OnAfterConnect       : TConnectEvent Read F_AfterConnect Write F_AfterConnect;
         Property OnBeforeDisconnect   : TConnectEvent Read F_BeforeDisconnect Write F_BeforeDisconnect;
         Property OnAfterDisconnect    : TConnectEvent Read F_AfterDisconnect Write F_AfterDisconnect;
         Property Connected            : Boolean       Read F_Active Write F_Set_Active Default False;
      End;

{$IFDEF USEDB}
TKADaoTableManager = Class(TObject)
      Private
         F_Database      : TKADaoDatabase;
         F_DummyDataset  : TDummyDataset;
         Function          CheckStatus:Boolean;
         Procedure         StringToList(Items: String; List: TStringList);
      Public
         FieldDefs   : TFieldDefs;
         IndexDefs   : TIndexDefs;
         TableName   : String;
         Procedure   CreateTable;
         Procedure   AppendTable;
         Procedure   CreateIndex(PreservePrimaryKeys:Boolean);
         Constructor Create(Database : TKADaoDatabase);
         Destructor  Destroy;override;
      End;
{$ENDIF}

      Procedure Register;

{$IFNDEF D5UP}
var
  //   ***************************************************
  //   Defined only for Delphi3 and Delphi4
  //   Delphi5 has buildin support for EmptyParam
  //   ***************************************************
  EmptyParam : OleVariant;
  Unassigned : OleVariant;
{$ENDIF}

//*************************************************************************************************
implementation
Uses Dialogs, Forms, ODBCDialogUnit, ActiveX{$IFDEF USEDB}, KDaoTable{$ENDIF};

Const
  dbLangGeneral = ';LANGID=%s;CP=%s;COUNTRY=%s';

//******************************************************************************

{$IFNDEF USEDB}
Procedure DatabaseError(Msg:String);
Begin
  Raise Exception.Create(Msg);
End;
{$ENDIF}

function GetExeDir: String;
begin
     SetLength(Result,1001);
     GetModuleFileName(HInstance,PChar(Result),1000);
     Result := ExtractFilePath(StrPas(PChar(Result)));
end;

function GetWorkDir: String;
begin
     GetDir(0, Result);
     if Result[Length(Result)] <> '\' Then Result:=Result+'\';
end;

Function  TKADaoDatabase.GetLastDaoError:TDaoErrRec;
Begin
  Result.ErrNo         := 0;
  Result.Source        := '';
  Result.Description   := '';
  Result.HelpFile      := '';
  Result.HelpContext   := 0;
  {$IFDEF DYNADAO}
  if VarIsNull(CoreDBEngine) Then Exit;
  if VarIsEmpty(CoreDBEngine) Then Exit;
  {$ELSE}
  if CoreDBEngine=NIL Then Exit;
  {$ENDIF}
  if CoreDBEngine.Errors.Count=0 Then Exit;
  Result.ErrNo       := CoreDBEngine.Errors.Item[0].Number;
  Result.Source      := CoreDBEngine.Errors.Item[0].Source;
  Result.Description := CoreDBEngine.Errors.Item[0].Description;
  Result.HelpFile    := CoreDBEngine.Errors.Item[0].HelpFile;
  Result.HelpContext := CoreDBEngine.Errors.Item[0].HelpContext;
End;

{$IFDEF DYNADAO}
Function TKADaoDatabase.CreateOleDBEngine(const ClassName: string): IDispatch;
{$ELSE}
Function TKADaoDatabase.CreateOleDBEngine(const ClassID: TGUID): DBEngine;
{$ENDIF}
Const
  DBEngine_TGUID: TGUID = '{00000021-0000-0010-8000-00AA006D2EA4}';
Var
  LicenseClass       : IClassFactory2;
  DWReserved         : DWORD;
  LicenseString      : Widestring;
{$IFDEF DYNADAO}
  ClassID : TGUID;
Begin
  ClassID := ProgIDToClassID(ClassName);
{$ELSE}
Begin
{$ENDIF}
  //****************************************************************************
  LicenseClass := Nil;
  OleCheck(CoGetClassObject(ClassID,CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, nil, IClassFactory2, LicenseClass));
  if Assigned(LicenseClass) Then
     Begin
       SetLength(LicenseString,2000);
       DWReserved:=0;
       if F_RuntimeLicense <> '' Then
          LicenseString := F_RuntimeLicense
       Else
          LicenseClass.RequestLicKey(DWReserved,LicenseString);
       OleCheck(LicenseClass.CreateInstanceLic (nil, nil, DBEngine_TGUID, LicenseString, Result));
     End;
  //****************************************************************************
End;

Function TKADaoDatabase.CreateOleDBEngine_II(const ClassName: string): IDispatch;
Const
  DBEngine_TGUID: TGUID = '{00000021-0000-0010-8000-00AA006D2EA4}';
Var
  LicenseClass       : IClassFactory2;
  DWReserved         : DWORD;
  LicenseString      : Widestring;
  ClassID : TGUID;
Begin
  ClassID := ProgIDToClassID(ClassName);
  //****************************************************************************
  LicenseClass := Nil;
  OleCheck(CoGetClassObject(ClassID,CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, nil, IClassFactory2, LicenseClass));
  if Assigned(LicenseClass) Then
     Begin
       SetLength(LicenseString,2000);
       DWReserved:=0;
       if F_RuntimeLicense <> '' Then
          LicenseString := F_RuntimeLicense
       Else
          LicenseClass.RequestLicKey(DWReserved,LicenseString);
       OleCheck(LicenseClass.CreateInstanceLic (nil, nil, DBEngine_TGUID, LicenseString, Result));
     End;
  //****************************************************************************
End;


Procedure TKADaoDatabase.CheckEngines;
Var
 V35               : String;
 V36               : String;
 V120              : String;
 Reg               : TRegistry;
 S                 : String;
 TempDBEngine      : OleVariant;
Begin
  if F_PrivateEngine Then
    Begin
     V35  := 'DAO.PrivateDBEngine.35';
     V36  := 'DAO.PrivateDBEngine.36';
     V120 := 'DAO.PrivateDBEngine.120';
    End
 Else
    Begin
     V35  := 'DAO.DBEngine.35';
     V36  := 'DAO.DBEngine.36';
     V120 := 'DAO.DBEngine.120';
    End;

  Reg := TRegistry.Create;  
  {$IFDEF VER130} Reg.Access:=KEY_READ; {$ENDIF}
  Reg.RootKey := HKEY_CLASSES_ROOT;
  {$IFNDEF D4UP}
  if Reg.OpenKey(V35,False) then
  {$ELSE}
  if Reg.OpenKeyReadOnly(V35) then
  {$ENDIF}
     Begin
       Reg.CloseKey;
       Try
        TempDBEngine               := CreateOleDBEngine_II(V35);
        VarClear(TempDBEngine);
        F_DaoVersionList.Add('3.5');
       Except
         on E:Exception do
            Begin
              S:=E.Message;
              if Pos('80040112',S) > 0 Then
                 Begin
                   Reg.CloseKey;
                   Reg.Free;
                   DatabaseError(E1001);
                 End;
            End;
       End;
     End;
  Reg.Free;
   
  Reg := TRegistry.Create;
  {$IFDEF VER130} Reg.Access:=KEY_READ; {$ENDIF}
  Reg.RootKey := HKEY_CLASSES_ROOT;
  {$IFNDEF D4UP}
  if Reg.OpenKey(V36,False) then
  {$ELSE}
  if Reg.OpenKeyReadOnly(V36) then
  {$ENDIF}
     Begin
       Reg.CloseKey;
       Try
        TempDBEngine               := CreateOleDBEngine_II(V36);
        VarClear(TempDBEngine);
        F_DaoVersionList.Add('3.6');
       Except
         on E:Exception do
            Begin
              S:=E.Message;
              if Pos('80040112',S) > 0 Then
                 Begin
                   Reg.CloseKey;
                   Reg.Free;
                   DatabaseError(E1001);
                 End;
            End;
       End;
     End;
  Reg.Free;

  Reg := TRegistry.Create;
  {$IFDEF VER130} Reg.Access:=KEY_READ; {$ENDIF}
  Reg.RootKey := HKEY_CLASSES_ROOT;
  {$IFNDEF D4UP}
  if Reg.OpenKey(V120,False) then
  {$ELSE}
  if Reg.OpenKeyReadOnly(V120) then
  {$ENDIF}
     Begin
       Reg.CloseKey;
       Try
        TempDBEngine               := CreateOleDBEngine_II(V120);
        VarClear(TempDBEngine);
        F_DaoVersionList.Add('12.0');
       Except
         on E:Exception do
            Begin
              S:=E.Message;
              if Pos('80040112',S) > 0 Then
                 Begin
                   Reg.CloseKey;
                   Reg.Free;
                   DatabaseError(E1001);
                 End;
            End;
       End;
     End;
  Reg.Free;

  If (Not VarIsNull(TempDBEngine)) And (Not VarIsEmpty(TempDBEngine)) Then VarClear(TempDBEngine);
End;

//*************************************************************************************************
Procedure TKADaoDatabase.CreateDBEngine(DaoVer:String);
Var
  V35  : String;
  V36  : String;
  V120 : String;
Begin
 if F_PrivateEngine Then
    Begin
     V35  := 'DAO.PrivateDBEngine.35';
     V36  := 'DAO.PrivateDBEngine.36';
     V120 := 'DAO.PrivateDBEngine.120';
    End
 Else
    Begin
     V35  := 'DAO.DBEngine.35';
     V36  := 'DAO.DBEngine.36';
     V120 := 'DAO.DBEngine.120';
    End;

 {$IFDEF DYNADAO}
  F_DynaDao := True;

  if DaoVer='3.5' Then
     Begin
       Try
         CoreDBEngine               := CreateOleDBEngine(V35);
         F_DaoVersion               := '3.5';
       Except
         Try
           CoreDBEngine             := CreateOleDBEngine(V36);
           F_DaoVersion             := '3.6';
         Except
           Try
             CoreDBEngine           := CreateOleDBEngine(V120);
             F_DaoVersion           := '12.0';
           Except
             DatabaseError(E1002);
           End;
         End;
       End;
   End;

  if DaoVer='3.6' Then
     Begin
       Try
        CoreDBEngine             := CreateOleDBEngine(V36);
        F_DaoVersion             := '3.6';
       Except
         Try
           CoreDBEngine          := CreateOleDBEngine(V120);
           F_DaoVersion          := '12.0';
         Except
           DatabaseError(E1002);
         End;
       End;
   End;

  if DaoVer='12.0' Then
     Begin
       Try
         CoreDBEngine             := CreateOleDBEngine(V120);
         F_DaoVersion             := '12.0';
       Except
         DatabaseError(E1002);
       End;
   End;

  {$ELSE}
  F_DynaDao := False;
  CoreDBEngine               := Nil;
  Try
    if F_PrivateEngine Then
       CoreDBEngine          := CreateOleDBEngine(Class_PrivDBEngine)
    Else
       CoreDBEngine          := CreateOleDBEngine(Class_DBEngine);
  Except
    on E:Exception do
       Begin
         if Pos('80040112',E.Message) > 0 Then
            Begin
              DatabaseError(E1001);
            End
          Else DatabaseError(E.Message);
       End;
  End;
  {$ENDIF}
End;

Function    TKADaoDatabase.GetDAOEnginesInstalled:TStringList;
Begin
  Result := GetAllDaoEngines(F_RuntimeLicense);
End;

Constructor TKADaoDatabase.Create(AOwner : TComponent);
Var
  OLE_INIT : Integer;
  X        : Integer;
  Prop     : Pointer;
Begin
  Inherited Create(AOwner);
  //*******************************************
  F_ComponentVersion :='11.00';
  //*******************************************
  {$IFDEF DYNADAO}
  //********************************************************************* Events
  F_OnLogin            := Nil;
  F_BeforeConnect      := Nil;
  F_AfterConnect       := Nil;
  F_BeforeDisconnect   := Nil;
  F_AfterDisconnect    := Nil;
  //********************************************************************* Events
  F_RuntimeLicense              := '';
  CoreWorkspace                 := Unassigned;
  CoreDatabase                  := Unassigned;
  CoreDBEngine                  := Unassigned;
  {$ENDIF}
  F_Offline := False;
  F_OLE_ON  := False;
  OLE_INIT  := CoInitialize(NIL);
  if (OLE_INIT = S_OK) or (OLE_INIT = S_FALSE) then F_OLE_ON:= True
  else DatabaseError(E1003);
  F_PrivateEngine               := False;
  F_DaoVersionList              := TStringList.Create;
  F_DaoVersionList.Clear;
  if Assigned(Owner) Then
   Begin
    For X := 0 To Owner.ComponentCount-1 do
      Begin
       Prop := GetPropInfo(Owner.Components[X].ClassInfo, 'DaoLicence');
       if Prop <> Nil Then
           Begin
             F_RuntimeLicense := GetStrProp(Owner.Components[X], Prop);
             Break;
           End;
      End;             
   End;
  CheckEngines;
  {$IFDEF DYNADAO}
  if F_DaoVersionList.Count > 0 Then
     Begin
      if F_DaoVersionList.Strings[0]='3.5'  Then F_DaoVersion := '3.5';
      if F_DaoVersionList.Strings[0]='3.6'  Then F_DaoVersion := '3.6';
      if F_DaoVersionList.Strings[0]='12.0' Then F_DaoVersion := '12.0';
     End
  Else
     Begin
       DatabaseError(E1004);
     End;
  {$ENDIF}
  {$IFDEF DAO35}
  F_DaoVersion               := '3.5';
  {$ENDIF}
  {$IFDEF DAO36}
  F_DaoVersion               := '3.6';
  {$ENDIF}
  {$IFDEF DAO120}
  F_DaoVersion               := '12.0';
  {$ENDIF}
  //*******************************************
  CreateDBEngine(F_DaoVersion);
  //*******************************************
  F_SystemDB                    := F_Get_SystemDatabaseFromRegistry;
  if F_SystemDB <> '' Then
  CoreDBEngine.SystemDB         := F_SystemDB;
  F_Username                    := 'Admin';
  F_Password                    := '';
  F_DatabasePassword            := '';
  F_DatabaseParameters          := ''; 
  F_SaveUserName                := True;
  F_SmartOpen                   := True;
  CoreDBEngine.DefaultUser      := 'Admin';
  CoreDBEngine.DefaultPassword  := '';
  F_EngineType                  := dbUseJet;
  CoreDBEngine.DefaultType      := F_EngineType;
  F_DefaultCursorDriver         := dbUseDefaultCursor;

  //****************************************************************************
  F_Workspace                   := 'DaoWorkspace';
  //****************************************************************************

  F_ActualDaoVersion            := CoreDBEngine.Version;
  F_VersionInfo                 := '';
  F_DatabaseVersion             := '';
  if Pos( '3.5',F_ActualDaoVersion)=1  Then F_VersionInfo:='(In Access''97 mode)';
  if Pos( '3.6',F_ActualDaoVersion)=1  Then F_VersionInfo:='(In Access''2000 mode)';
  if Pos('12.0',F_ActualDaoVersion)=1  Then F_VersionInfo:='(In Access''2007 mode)';
  F_MachineName                 := '';
  F_DatabaseType                :='Access';
  F_Active                      := False;
  F_Database                    := '';
  F_ReadOnly                    := False;
  F_Exclusive                   := False;
  F_LoginPrompt                 := False;
  F_AutoDetectMDB               := False;
  F_ShowSysObjects              := False;


  F_TableNames             := TStringList.Create;
  F_ActiveTableNames       := TStringList.Create;
  F_QueryDefNames          := TStringList.Create;
  F_DBTypesList            := TStringList.Create;
  F_DriverList             := TStringList.Create;
  F_SystemDSNs             := TStringList.Create;
  F_UserDSNs               := TStringList.Create;
  F_DSNFileNames           := TStringList.Create;
  F_Params                 := TStringList.Create;

  F_QueryTimeout           := 60;

  F_TransInfo              := TStringList.Create;
  F_TrackTransactions      := True;
  F_UseODBCDialog          := True;

  F_Get_DBTypesList(F_DBTypesList);
  F_Get_OdbcDriverList(F_DriverList);
  F_Get_SystemDSNs(F_SystemDSNs);
  F_Get_UserDSNs(F_UserDSNs);
  F_FillDSNFileNames(F_DSNFileNames);
End;

Destructor  TKADaoDatabase.Destroy;
Begin
 If F_Active Then Connected := False;
 F_TableNames.Free;
 F_ActiveTableNames.Free;
 F_QueryDefNames.Free;
 F_DBTypesList.Free;
 F_DriverList.Free;
 F_SystemDSNs.Free;
 F_UserDSNs.Free;
 F_DaoVersionList.Free;
 F_DSNFileNames.Free;
 F_Params.Free;
 F_TransInfo.Free;

 {$IFDEF DYNADAO}
 If (Not VarIsNull(CoreWorkspace)) And (Not VarIsEmpty(CoreWorkspace)) Then CoreWorkspace.Close;
 VarClear(CoreDatabase);
 VarClear(CoreWorkspace);
 VarClear(CoreDBEngine);
 {$ELSE}
 if CoreWorkspace <> Nil Then CoreWorkspace.Close;
 CoreDatabase  := Nil;
 CoreWorkspace := Nil;
 CoreDBEngine  := Nil;
 {$ENDIF}
 if F_OLE_ON then CoUninitialize;
 Inherited Destroy;
End;

Procedure TKADaoDatabase.RecreateCore;
Var
  OLE_INIT     : Integer;
  TempPrivate  : Boolean;
Begin
  if F_Offline Then Exit;
 {$IFDEF DYNADAO}
   If (Not VarIsNull(CoreWorkspace)) And (Not VarIsEmpty(CoreWorkspace)) Then CoreWorkspace.Close;
   Try
     VarClear(CoreWorkspace);
   Except
   End;
   If (Not VarIsNull(CoreDBEngine))  And (Not VarIsEmpty(CoreDBEngine))  Then VarClear(CoreDBEngine);
  {$ELSE}
   If (Assigned(CoreWorkspace)) Then CoreWorkspace.Close;
   CoreWorkspace := Nil;
   CoreDBEngine  := Nil;
  {$ENDIF}
   if F_OLE_ON Then CoUninitialize;
   F_OLE_ON   := False;
   OLE_INIT   := CoInitialize(NIL);
   if (OLE_INIT = S_OK) or (OLE_INIT = S_FALSE) then F_OLE_ON:= True
   Else DatabaseError(E1003);
   //*************************************************** Borland, Microsoft ...
   TempPrivate:=True;
   if (csDesigning in ComponentState) And (F_EngineType=dbUseJet) Then
      Begin
        TempPrivate      := F_PrivateEngine;
        F_PrivateEngine  := True;
      End;
   CreateDBEngine(F_DaoVersion);
   if (csDesigning in ComponentState) And (F_EngineType=dbUseJet) Then F_PrivateEngine  := TempPrivate;
   //***************************************************************************
   CoreDBEngine.SystemDB                                   := F_SystemDB;
   F_ActualDaoVersion                                      := CoreDBEngine.Version;
   if Pos( '3.5',F_ActualDaoVersion)=1  Then F_VersionInfo := '(In Access''97 mode)';
   if Pos( '3.6',F_ActualDaoVersion)=1  Then F_VersionInfo := '(In Access''2000 mode)';
   if Pos('12.0',F_ActualDaoVersion)=1  Then F_VersionInfo := '(In Access''2007 mode)';
   CoreDBEngine.DefaultUser                                := F_Username;
   CoreDBEngine.DefaultPassword                            := F_Password;
   CoreWorkspace                                           := CoreDBEngine.CreateWorkspace(F_Workspace,F_Username,F_Password,F_EngineType);
   CoreDBEngine.Workspaces.Append(CoreWorkspace);
   if F_EngineType=dbUseODBC Then
      Begin
       CoreWorkspace.DefaultCursorDriver:=F_DefaultCursorDriver;
      End;
   F_Workspace                   := CoreWorkspace.Name;
End;

Procedure TKADaoDatabase.Loaded;
Begin
  Try
    inherited Loaded;
    if Not F_Active Then RecreateCore;
  Except
  End;
End;

Procedure TKADaoDatabase.F_Set_ComponentVersion(Value: String);
Begin
 //******************************************************************** ReadOnly
End;

Procedure TKADaoDatabase.F_Set_Params(Value : TStringList);
Begin
  F_Params.SetText(Value.GetText);
End;

Procedure TKADaoDatabase.F_Set_DefaultCursorDriver(Value : Integer);
Begin
 F_DefaultCursorDriver:=Value;
 if csLoading in ComponentState Then Exit;
 if F_Offline Then Exit;
 if F_EngineType=dbUseODBC Then
    Begin
      CoreWorkspace.DefaultCursorDriver:=F_DefaultCursorDriver;
    End;
End;

Function TKADaoDatabase.F_Get_ODBCFileName(DSN:String;SystemWideDSN:Boolean):String;
Var
  Reg : TRegistry;
Begin
  Result:='';
  Reg := TRegistry.Create;
  {$IFDEF VER130} Reg.Access:=KEY_READ; {$ENDIF}
  if SystemWideDSN Then
     Reg.RootKey := HKEY_LOCAL_MACHINE
  Else
     Reg.RootKey := HKEY_CURRENT_USER;
  {$IFNDEF D4UP}
  if Reg.OpenKey('SOFTWARE\ODBC\ODBC.INI\'+DSN,False) then
  {$ELSE}
  if Reg.OpenKeyReadOnly('SOFTWARE\ODBC\ODBC.INI\'+DSN) then
  {$ENDIF}
     Begin
       Result:=Reg.ReadString('DBQ');
       Reg.CloseKey;
     End;
  Reg.Free;
End;

procedure TKADaoDatabase.F_FillDSNFileNames(List: TStrings);
Var
  X : Integer;
  S : String;
Begin
  List.Clear;
  For X:=0 to F_UserDSNs.Count-1 do
      Begin
       S:=F_Get_ODBCFileName(F_UserDSNs.Strings[X],False);
       if Length(S) > 0 Then List.Add(F_UserDSNs.Strings[X]+'='+S);
      End;
  For X:=0 to F_SystemDSNs.Count-1 do
      Begin
       S:=F_Get_ODBCFileName(F_SystemDSNs.Strings[X],True);
       if Length(S) > 0 Then List.Add(F_SystemDSNs.Strings[X]+'='+S);
      End;
End;

procedure TKADaoDatabase.F_Get_OdbcDriverList(List: TStrings);
var
   Reg : TRegistry;
Begin
     Reg := TRegistry.Create;
     {$IFDEF VER130} Reg.Access:=KEY_READ; {$ENDIF}
     try
     Begin
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\ODBC\ODBCINST.INI\ODBC Drivers',False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\ODBC\ODBCINST.INI\ODBC Drivers') then
          {$ENDIF}
          Begin
               List.Clear;
               Reg.GetValueNames(List);
               Reg.CloseKey;
          End;
     End;
     finally
          Reg.Free;
     End;
End;

procedure TKADaoDatabase.F_Get_SystemDSNs(DSNs: TStrings);
var
  Reg: TRegistry;
begin
  DSNs.Clear;
  Reg:= TRegistry.Create;
  {$IFDEF VER130} Reg.Access:=KEY_READ; {$ENDIF}
  Reg.RootKey:= HKEY_LOCAL_MACHINE;
  {$IFNDEF D4UP}
  if Reg.OpenKey('\SOFTWARE\ODBC\odbc.ini\ODBC Data Sources', False) Then
  {$ELSE}
  if Reg.OpenKeyReadOnly('\SOFTWARE\ODBC\odbc.ini\ODBC Data Sources')  Then
  {$ENDIF}
     Begin
       Reg.GetValueNames(DSNs);
       Reg.CloseKey;
     End;
  Reg.Free;
end;

procedure TKADaoDatabase.F_Get_UserDSNs(DSNs: TStrings);
var
  Reg: TRegistry;
begin
  DSNs.Clear;
  Reg:= TRegistry.Create;
  {$IFDEF VER130} Reg.Access:=KEY_READ; {$ENDIF}
  Reg.RootKey:= HKEY_CURRENT_USER;
  {$IFNDEF D4UP}
  if Reg.OpenKey('\SOFTWARE\ODBC\odbc.ini\ODBC Data Sources', False) Then
  {$ELSE}
  if Reg.OpenKeyReadOnly('\SOFTWARE\ODBC\odbc.ini\ODBC Data Sources')  Then
  {$ENDIF}
     Begin
       Reg.GetValueNames(DSNs);
       Reg.CloseKey;
     End;
  Reg.Free;
end;

procedure TKADaoDatabase.F_Get_DBTypesList(List: TStrings);
var
   Reg : TRegistry;
Begin
     Reg := TRegistry.Create;
     {$IFDEF VER130} Reg.Access:=KEY_READ; {$ENDIF}
     try
     Begin
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          {$IFDEF DAO35}
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\Microsoft\JET\3.5\ISAM Formats',False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\JET\3.5\ISAM Formats') then
          {$ENDIF}
             Begin
               List.Clear;
               Reg.GetKeyNames(List);
               Reg.CloseKey;
             End;
          {$ENDIF}

          {$IFDEF DAO36}
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\Microsoft\JET\4.0\ISAM Formats',False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\JET\4.0\ISAM Formats') then
          {$ENDIF}
             Begin
               List.Clear;
               Reg.GetKeyNames(List);
               Reg.CloseKey;
             End;
          {$ENDIF}

          {$IFDEF DAO120}
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\Microsoft\Office\12.0\Access Connectivity Engine\ISAM Formats',False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Office\12.0\Access Connectivity Engine\ISAM Formats') then
          {$ENDIF}
             Begin
               List.Clear;
               Reg.GetKeyNames(List);
               Reg.CloseKey;
             End;
          {$ENDIF}

          {$IFDEF DYNADAO}//****************************************************
          if F_DaoVersion='3.5' then
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\Microsoft\JET\3.5\ISAM Formats',False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\JET\3.5\ISAM Formats') then
          {$ENDIF}
             Begin
               List.Clear;
               Reg.GetKeyNames(List);
               Reg.CloseKey;
             End;

          if F_DaoVersion='3.6' then
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\Microsoft\JET\4.0\ISAM Formats',False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\JET\4.0\ISAM Formats') then
          {$ENDIF}
             Begin
               List.Clear;
               Reg.GetKeyNames(List);
               Reg.CloseKey;
             End;

          if F_DaoVersion='12.0' then
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\Microsoft\Office\12.0\Access Connectivity Engine\ISAM Formats',False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Office\12.0\Access Connectivity Engine\ISAM Formats') then
          {$ENDIF}
             Begin
               List.Clear;
               Reg.GetKeyNames(List);
               Reg.CloseKey;
             End;
          {$ENDIF}
     End;
     finally
          Reg.Free;
     End;
    List.Insert(0,'ODBC');
    List.Insert(0,'Access');
End;


Function TKADaoDatabase.F_Get_DBTypeFileExtension(DBType:String):String;
var
   Reg : TRegistry;
Begin
     Reg := TRegistry.Create;
     {$IFDEF VER130} Reg.Access:=KEY_READ; {$ENDIF}
     try
     Begin
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          {$IFDEF DAO35}
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\Microsoft\JET\3.5\ISAM Formats\'+DBType,False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\JET\3.5\ISAM Formats\'+DBType) then
          {$ENDIF}
             Begin
               Result:=Reg.ReadString('ExportFilter');
               if Result='' Then Result:=Reg.ReadString('ImportFilter');
               Reg.CloseKey;
             End;
          {$ENDIF}

          {$IFDEF DAO36}
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\Microsoft\JET\4.0\ISAM Formats\'+DBType,False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\JET\4.0\ISAM Formats\'+DBType) then
          {$ENDIF}
             Begin
               Result:=Reg.ReadString('ExportFilter');
               if Result='' Then Result:=Reg.ReadString('ImportFilter');
               Reg.CloseKey;
             End;
          {$ENDIF}

          {$IFDEF DAO120}
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\Microsoft\Office\12.0\Access Connectivity Engine\ISAM Formats\'+DBType,False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Office\12.0\Access Connectivity Engine\ISAM Formats\'+DBType) then
          {$ENDIF}
             Begin
               Result:=Reg.ReadString('ExportFilter');
               if Result='' Then Result:=Reg.ReadString('ImportFilter');
               Reg.CloseKey;
             End;
          {$ENDIF}

          {$IFDEF DYNADAO}
          if F_DaoVersion='3.5' then
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\Microsoft\JET\3.5\ISAM Formats\'+DBType,False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\JET\3.5\ISAM Formats\'+DBType) then
          {$ENDIF}
             Begin
               Result:=Reg.ReadString('ExportFilter');
               if Result='' Then Result:=Reg.ReadString('ImportFilter');
               Reg.CloseKey;
             End;

          if F_DaoVersion='3.6' then
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\Microsoft\JET\4.0\ISAM Formats\'+DBType,False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\JET\4.0\ISAM Formats\'+DBType) then
          {$ENDIF}
             Begin
               Result:=Reg.ReadString('ExportFilter');
               if Result='' Then Result:=Reg.ReadString('ImportFilter');
               Reg.CloseKey;
             End;

          if F_DaoVersion='12.0' then
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\Microsoft\Office\12.0\Access Connectivity Engine\ISAM Formats\'+DBType,False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Office\12.0\Access Connectivity Engine\ISAM Formats\'+DBType) then
          {$ENDIF}
             Begin
               Result:=Reg.ReadString('ExportFilter');
               if Result='' Then Result:=Reg.ReadString('ImportFilter');
               Reg.CloseKey;
             End;
          {$ENDIF}
     End;
     finally
          Reg.Free;
     End;
End;

Function TKADaoDatabase.F_Get_DBTypeTableType(DBType:String):String;
var
   Reg : TRegistry;
   BUF  : Array[1..1000] of Byte;
Begin
     Reg := TRegistry.Create;
     {$IFDEF VER130} Reg.Access:=KEY_READ; {$ENDIF}
     try
     Begin
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          {$IFDEF DAO35}
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\Microsoft\JET\3.5\ISAM Formats\'+DBType,False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\JET\3.5\ISAM Formats\'+DBType) then
          {$ENDIF}
             Begin
               Reg.ReadBinaryData('OneTablePerFile',BUF,1000);
               Result:=IntToStr(BUF[1]);
               Reg.CloseKey;
             End;
          {$ENDIF}

          {$IFDEF DAO36}
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\Microsoft\JET\4.0\ISAM Formats\'+DBType,False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\JET\4.0\ISAM Formats\'+DBType) then
          {$ENDIF}
             Begin
               Reg.ReadBinaryData('OneTablePerFile',BUF,1000);
               Result:=IntToStr(BUF[1]);
               Reg.CloseKey;
             End;
          {$ENDIF}

          {$IFDEF DAO120}
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\Microsoft\Office\12.0\Access Connectivity Engine\ISAM Formats\'+DBType,False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Office\12.0\Access Connectivity Engine\ISAM Formats\'+DBType) then
          {$ENDIF}
             Begin
               Reg.ReadBinaryData('OneTablePerFile',BUF,1000);
               Result:=IntToStr(BUF[1]);
               Reg.CloseKey;
             End;
          {$ENDIF}

          {$IFDEF DYNADAO}
          if F_DaoVersion='3.5' then
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\Microsoft\JET\3.5\ISAM Formats\'+DBType,False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\JET\3.5\ISAM Formats\'+DBType) then
          {$ENDIF}
             Begin
               Reg.ReadBinaryData('OneTablePerFile',BUF,1000);
               Result:=IntToStr(BUF[1]);
               Reg.CloseKey;
             End;

          if F_DaoVersion='3.6' then
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\Microsoft\JET\4.0\ISAM Formats\'+DBType,False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\JET\4.0\ISAM Formats\'+DBType) then
          {$ENDIF}
             Begin
               Reg.ReadBinaryData('OneTablePerFile',BUF,1000);
               Result:=IntToStr(BUF[1]);
               Reg.CloseKey;
             End;

          if F_DaoVersion='12.0' then
          {$IFNDEF D4UP}
          if Reg.OpenKey('SOFTWARE\Microsoft\Office\12.0\Access Connectivity Engine\ISAM Formats\'+DBType,False) then
          {$ELSE}
          if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Office\12.0\Access Connectivity Engine\ISAM Formats\'+DBType) then
          {$ENDIF}
             Begin
               Reg.ReadBinaryData('OneTablePerFile',BUF,1000);
               Result:=IntToStr(BUF[1]);
               Reg.CloseKey;
             End;
          {$ENDIF}
     End;
     finally
          Reg.Free;
     End;
End;


Procedure TKADaoDatabase.DetectMDB(DatabasePath:String);
Var
  F   : File;
  S   : Array[0..160] of Char;
  NR  : Integer;
  OP  : Boolean;
Begin
 if F_DatabaseType <> 'Access' Then Exit;
 OP:=False;
 Try
  System.FileMode := 0;
  System.AssignFile(F,DatabasePath);
  System.Reset(F,1);
  OP    :=True;
  System.BlockRead(F,S,160,NR);
  if NR < 160 Then Exit;

  if F_DaoVersionList.IndexOf('3.5') <> -1 Then
     F_DaoVersion := '3.5';

  if     (S[156]='4')
     And (S[157]='.')
     And (F_DaoVersion='3.5')
     And (F_Dynadao) Then
     Begin
       F_DaoVersion := '3.6';
     End;

 if      (S[156]='4')
     And (S[157]='.')
     And (S[13]='A')
     And (S[14]='C')
     And (S[15]='E')
     And ((F_DaoVersion='3.5') or (F_DaoVersion='3.6'))
     And (F_Dynadao) Then
     Begin
       F_DaoVersion := '12.0';
     End;

 Except
   if OP Then System.CloseFile(F);
   Exit;
 End;
 if OP Then System.CloseFile(F);
End;

Procedure TKADaoDatabase.F_Set_TrackTransactions(Value : Boolean);
Begin
  if F_Active  Then DatabaseError('Cannot set TrackTransactions property when Database is connected!');
  F_TrackTransactions := Value;
End;

Procedure TKADaoDatabase.F_Set_Active(Value : Boolean);
Label START;
Var
  Pwd          : String;
  LoginParams  : TStringList;
  BadPassword  : Boolean;
  ExitDB       : Boolean;
Begin
  if (F_Active) And (Value) Then Exit;
  if (F_Database='') And (Value) Then
      Begin
       DatabaseError(E1005);
       Exit;
     End;
  if (F_DatabaseType='') And (Value) Then
      Begin
       DatabaseError(E1006);
       Exit;
     End;
  if (F_Active) And (NOT Value) Then
     Begin
       if Assigned(F_BeforeDisconnect) Then F_BeforeDisconnect(Self);
       F_TableNames.Clear;
       F_QueryDefNames.Clear;
       {$IFDEF USEDB}
       CloseDatasets;
       {$ENDIF}
       F_ActiveTableNames.Clear;
       CoreDatabase.Close;
       {$IFDEF DYNADAO}
       VarClear(CoreDatabase); 
       {$ELSE}
       CoreDatabase := Nil;
       {$ENDIF}
       F_TransInfo.Clear;
       F_Active:=False;
       if Assigned(F_AfterDisconnect) Then F_AfterDisconnect(Self);
     End;
  if (NOT F_Active) And (Value) Then
     Begin
        if Assigned(F_BeforeConnect) Then F_BeforeConnect(Self);
START:
        ExitDB      := False;
        BadPassword := False;
        if F_LoginPrompt Then
           Begin
             F_LoginDialog   := TDbLogin.CreateParented(Application.Handle);
             if F_SaveUserName Then
                F_LoginDialog.UserName.Text    := F_UserName
             Else
                F_LoginDialog.UserName.Text    := '';
             F_LoginDialog.Password.Text    := '';
             F_LoginDialog.DbPassword.Text  := '';
             F_LoginDialog.DatabaseName.Caption:=F_Database;
             F_LoginDialog.ActiveControl:=F_LoginDialog.UserName;
             if Assigned(F_OnLogin) Then
                Begin
                  LoginParams  := TStringList.Create;
                  LoginParams.Add(szUSERNAME+'='+F_Username);
                  LoginParams.Add(szPASSWORD+'='+F_Password);
                  LoginParams.Add(szDBPASSWORD+'='+F_DatabasePassword);
                  F_OnLogin(Self, LoginParams);
                  F_Username:=LoginParams.Values[szUSERNAME];
                  F_Password:=LoginParams.Values[szPASSWORD];
                  F_DatabasePassword:=LoginParams.Values[szDBPASSWORD];
                  LoginParams.Free;
                End
             Else
                Begin
                  if (F_Params.Count > 0) Then
                     Begin
                      if F_Params.IndexOfName(szUSERNAME) <> -1 Then
                         F_Username         := F_Params.Values[szUSERNAME];
                      if F_Params.IndexOfName(szPASSWORD) <> -1 Then
                         F_Password         := F_Params.Values[szPASSWORD];
                      if F_Params.IndexOfName(szDBPASSWORD) <> -1 Then
                         F_DatabasePassword := F_Params.Values[szDBPASSWORD];
                     End
                  Else
                  if F_LoginDialog.ShowModal=ID_OK Then
                     Begin
                      F_Username          := F_LoginDialog.UserName.Text;
                      F_Password          := F_LoginDialog.Password.Text;
                      F_DatabasePassword  := F_LoginDialog.DbPassword.Text;
                     End
                  Else
                     Begin
                       ShowMessage('If You not enter Username and Password You may not gain access to your data!');
                       F_Username         := '';
                       F_Password         := '';
                       F_DatabasePassword := '';
                       ExitDB:=True;
                     End;
                End;
             F_LoginDialog.Free;
           End
        Else
           Begin
             if Assigned(F_OnLogin) Then
                Begin
                  LoginParams  := TStringList.Create;
                  LoginParams.Add(szUSERNAME+'='+F_Username);
                  LoginParams.Add(szPASSWORD+'='+F_Password);
                  LoginParams.Add(szDBPASSWORD+'='+F_DatabasePassword);
                  F_OnLogin(Self, LoginParams);
                  F_Username:=LoginParams.Values[szUSERNAME];
                  F_Password:=LoginParams.Values[szPASSWORD];
                  F_DatabasePassword:=LoginParams.Values[szDBPASSWORD];
                  LoginParams.Free;
                End
             Else
                Begin
                  if (F_Params.Count > 0) Then
                     Begin
                      if F_Params.IndexOfName(szUSERNAME) <> -1 Then
                         F_Username         := F_Params.Values[szUSERNAME];
                      if F_Params.IndexOfName(szPASSWORD) <> -1 Then
                         F_Password         := F_Params.Values[szPASSWORD];
                      if F_Params.IndexOfName(szDBPASSWORD) <> -1 Then
                         F_DatabasePassword := F_Params.Values[szDBPASSWORD];
                     End
                End;
           End;
        Try                                                               
          F_Offline := False;
          if F_AutoDetectMDB Then DetectMDB(F_Database);
          RecreateCore;
        Except
          On E:Exception do
             Begin
              if F_LoginPrompt Then
                 Begin
                   if ExitDB Then Exit;
                   ShowMessage(E.Message);
                   BadPassword :=True;
                 End
              Else
                 Begin
                   Raise;
                 End;
             End;
        End;
        if BadPassword Then Goto Start;
        if (AnsiCompareText(F_DatabaseType,'Access')=0) Then
           Begin
             Pwd:=F_DatabasePassword;
             if F_SmartOpen Then
                Begin
                 if NOT FileExists(F_Database) Then
                    Begin
                      if csDesigning in ComponentState Then
                         F_Database := GetWorkDir+ExtractFileName(F_Database)
                      Else
                         F_Database := GetExeDir+ExtractFileName(F_Database);
                    End;
                End;
             if NOT FileExists(F_Database) Then DatabaseError(E1038+#13#10+F_Database);
             if F_EngineType=dbUseJet Then
                CoreDatabase := CoreWorkspace.OpenDatabase(F_Database,F_Exclusive,F_ReadOnly,Format(';UID=%s;PWD=%s;%s',[F_Username,Pwd,F_DatabaseParameters]))
             Else
                DatabaseError(E1007);
           End
        Else
           Begin
             Pwd:=F_Password;
             if AnsiCompareText(F_DatabaseType,'ODBC')=0 Then
                Begin
                  if F_EngineType=dbUseJet Then
                    Begin
                      if Pos('odbc;',AnsiLowerCase(F_Database))=1 Then
                         CoreDatabase := CoreWorkspace.OpenDatabase(F_Database,dbDriverNoPrompt,F_ReadOnly,Format('%s;%s',[F_Database,F_DatabaseParameters]))
                      Else
                         CoreDatabase := CoreWorkspace.OpenDatabase(F_Database,dbDriverNoPrompt,F_ReadOnly,Format('%s;UID=%s;PWD=%s;DSN=%s;%s',[F_DatabaseType,F_Username,Pwd,F_Database,F_DatabaseParameters]));
                    End
                 Else
                    Begin
                      {$IFDEF DYNADAO}
                        if Pos('odbc;',AnsiLowerCase(F_Database))=1 Then
                           CoreDatabase := CoreWorkspace.OpenConnection(F_Database,dbDriverNoPrompt,F_ReadOnly,Format('%s;%s',[F_Database,F_DatabaseParameters]))
                        Else
                           CoreDatabase := CoreWorkspace.OpenConnection(F_Database,dbDriverNoPrompt,F_ReadOnly,Format('%s;UID=%s;PWD=%s;DSN=%s;%s',[F_DatabaseType,F_Username,Pwd,F_Database,F_DatabaseParameters]));
                      {$ELSE}
                        DatabaseError(E1008);
                      {$ENDIF}
                    End;
                End
             Else
                Begin
                 if F_EngineType=dbUseJet Then
                    Begin
                      if (Pwd='') or (F_Username='')  Then
                         CoreDatabase := CoreWorkspace.OpenDatabase(F_Database,F_Exclusive,F_ReadOnly,Format('%s;%s',[F_DatabaseType,F_DatabaseParameters]))
                      Else
                         CoreDatabase := CoreWorkspace.OpenDatabase(F_Database,F_Exclusive,F_ReadOnly,Format('%s;UID=%s;PWD=%s;%s',[F_DatabaseType,F_Username,Pwd,F_DatabaseParameters]));
                    End
                 Else
                    DatabaseError(E1009);
                End;
           End;
        if F_QueryTimeout <> 60 Then
           Begin
             CoreDatabase.QueryTimeout:=F_QueryTimeout;
           End;
        if F_DatabaseType<>'ODBC' Then
           F_DatabaseVersion := CoreDatabase.Version;
        RefreshDefinitions;
        F_CollatingOrder:=F_Get_CollatingOrder;
        F_Active:=True;
        Idle;
        if Assigned(F_AfterConnect) Then F_AfterConnect(Self);
    End;
End;

Procedure TKADaoDatabase.Open;
Begin
 Connected := True;
End;

Procedure TKADaoDatabase.Close;
Begin
  Connected := False;
End;

Procedure TKADaoDatabase.CloseDatasets;
{$IFDEF USEDB}
Var
  X            : Integer;
  ATable       : TKADaoTable;
{$ENDIF}
Begin
{$IFDEF USEDB}
For X:=0 to F_ActiveTableNames.Count-1 do
    Begin
     ATable:=TKADaoTable(F_ActiveTableNames.Objects[X]);
     Try
      ATable.MainDatabaseShutdown := True;
      ATable.Active:=False;
     Except
     End;
    End;
{$ENDIF}
F_ActiveTableNames.Clear;
End;

Procedure TKADaoDatabase.RefreshDatasets;
{$IFDEF USEDB}
Var
  X            : Integer;
  ATable       : TKADaoTable;
{$ENDIF}
Begin
Idle;
{$IFDEF USEDB}
For X:=0 to F_ActiveTableNames.Count-1 do
    Begin
     ATable:=TKADaoTable(F_ActiveTableNames.Objects[X]);
     Try
      ATable.RefreshData;
     Except
     End;
    End;
{$ENDIF}
End;

Function TKADaoDatabase.ChooseDatabase: Boolean;
Var
   NewDB    : String;
begin
  NewDB  := F_ChooseDatabase;
  Result := NewDB <> '';
  if Result Then Database := NewDB
end;


Procedure TKADaoDatabase.RefreshDefinitions;
Var
  X: Integer;
Begin
 F_TableNames.Clear;
 F_QueryDefNames.Clear;
 //*****************************************************************************
 Try
   if F_EngineType = dbUseJet Then CoreDatabase.TableDefs.Refresh;
 Except
 End;
 //*****************************************************************************
 Try
   CoreDatabase.QueryDefs.Refresh;
 Except
 End;
 //*****************************************************************************
 Try
   if F_DatabaseType='Access' Then
   CoreDatabase.Containers.Refresh;
 Except
 End;
 //*****************************************************************************
 Try
   if F_DatabaseType='Access' Then
   CoreDatabase.Relations.Refresh;
 Except
 End;
 //*****************************************************************************
 Try
   CoreDatabase.Recordsets.Refresh;
 Except
 End;
 //*****************************************************************************
 Try
   if F_EngineType = dbUseJet Then CoreDatabase.Properties.Refresh;
 Except
 End;
 //*****************************************************************************
 Try
   CoreDBEngine.Errors.Refresh;
 Except
 End;
 //*****************************************************************************
 Try
   CoreDBEngine.Workspaces.Refresh;
 Except
 End;
 //*****************************************************************************
 Try
   CoreDBEngine.Properties.Refresh;
 Except
 End;
 //*****************************************************************************
 Try
   GoOnline;
   if F_EngineType = dbUseJet Then CoreWorkspace.Users.Refresh;
 Except
 End;
 //*****************************************************************************
 Try
   GoOnline;
   if F_EngineType = dbUseJet Then CoreWorkspace.Groups.Refresh;
 Except
 End;
 //*****************************************************************************
 Try
   GoOnline;
   CoreWorkspace.Databases.Refresh;
 Except
 End;
 //*****************************************************************************
 Try
   GoOnline;
   CoreWorkspace.Properties.Refresh;
 Except
 End;
 //*****************************************************************************
 Try
 if F_EngineType = dbUseJet Then
    Begin
      For X:=0 To CoreDatabase.TableDefs.Count-1 do
          Begin
            if F_ShowSysObjects Then
               Begin
                 F_TableNames.Add(CoreDatabase.TableDefs.Item[X].Name);
               End
            Else
               Begin
                 if (CoreDatabase.TableDefs.Item[X].Attributes And dbSystemObject) = 0 Then
                    Begin
                      F_TableNames.Add(CoreDatabase.TableDefs.Item[X].Name);
                    End;
               End;
          End;
    End;
 Except
 End;
 //*****************************************************************************
 Try
 For X:=0 To CoreDatabase.QueryDefs.Count-1 do
     Begin
      F_QueryDefNames.Add(CoreDatabase.QueryDefs.Item[X].Name);
     End;
 Except
 End;
End;

Procedure TKADaoDatabase.Idle;
Begin                                               
 CoreDBEngine.Idle(DaoApi.dbFreeLocks);
 CoreDBEngine.Idle(dbRefreshCache);
End;

Procedure TKADaoDatabase.F_Set_Database(Value : String);
Begin
  if (F_Active) Then
     Begin
       DatabaseError(E1010);
       Exit;
     End;
  F_Database:=Value;
End;

Procedure TKADaoDatabase.F_Set_DatabaseParameters(Value : String);
Begin
  if (F_Active) Then
     Begin
       DatabaseError(E1037);                                                                                 
       Exit;
     End;
  F_DatabaseParameters:=Value;
End;

Procedure TKADaoDatabase.F_Set_SystemDatabase(Value : String);
Var
  Tmp : String;
Begin
  if (F_Active) Then
     Begin
       DatabaseError(E1011);
       Exit;
     End;
  Tmp:=F_SystemDB;
  F_SystemDB:=Value;
  if F_SystemDB = '' Then F_SystemDB := F_Get_SystemDatabaseFromRegistry;
  if csLoading In ComponentState then Exit;
  //*********************** RECREATE???
  Try
   RecreateCore;
  Except
   F_SystemDB:=Tmp;
   RecreateCore;
   Raise;
  End;
End;

Procedure TKADaoDatabase.F_Set_DaoVersion(Value : String);
{$IFDEF DYNADAO}
Var
  Tmp : String;
  P   : Integer;
{$ENDIF}
Begin
{$IFDEF DYNADAO}
  if (F_Active) Then
     Begin
       DatabaseError(E1012);
       Exit;
     End;
  Tmp             := F_DaoVersion;
  P               := Pos('.',Value);
  F_DaoVersion    := Copy(Value,1,P+1);
  F_Get_DBTypesList(F_DBTypesList);
  if csLoading In ComponentState then Exit;
 //*********************** RECREATE???
 Try
   RecreateCore;
  Except
   F_DaoVersion:=Tmp;
   RecreateCore;
   Raise;
  End;
{$ELSE}
  //This property is read only for fixed DAO
{$ENDIF}
 F_ActualDaoVersion := CoreDBEngine.Version;
End;

Procedure TKADaoDatabase.F_Set_ActualDaoVersion(Value : String);
Begin
  //This property is read only
End;

Procedure TKADaoDatabase.F_Set_DatabaseVersion(Value : String);
Begin
  //This property is read only
End;



Procedure TKADaoDatabase.F_Set_VersionInfo(Value : String);
Begin
  //This property is read only
End;

Function TKADaoDatabase.F_Get_SystemDatabaseFromRegistry:String;
Var
  RS   : String;
  Reg : TRegistry;
Begin
  Result                         := '';
  RS                             := 'SOFTWARE\Microsoft\JET\3.5\Engines';
  if F_DaoVersion='3.5'  Then RS := 'SOFTWARE\Microsoft\JET\3.5\Engines';
  if F_DaoVersion='3.6'  Then RS := 'SOFTWARE\Microsoft\JET\4.0\Engines';
  if F_DaoVersion='12.0' Then RS := 'SOFTWARE\Microsoft\Office\12.0\Access Connectivity Engine\Engines';
  Reg := TRegistry.Create;
  {$IFDEF VER130} Reg.Access:=KEY_READ; {$ENDIF}
  Try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    {$IFNDEF D4UP}
    if Reg.OpenKey(RS,False) then
    {$ELSE}
    if Reg.OpenKeyReadOnly(RS) then
    {$ENDIF}
       Begin
         Result:=Reg.ReadString('SystemDB');
         Reg.CloseKey;
       End;
  Finally
    Reg.Free;
  End;
End;

Function TKADaoDatabase.FindWorkspace(WS:String):Boolean;
Var
  X : Integer;
Begin
  Result := False;
  For X :=0 to CoreDBEngine.Workspaces.Count-1 do
      Begin
       if CoreDBEngine.Workspaces.Item[X].Name=WS Then
          Begin
            Result := True;
            Exit;
          End;
      End;
End;

Procedure TKADaoDatabase.F_Set_Workspace(Value : String);
Var
  Tmp : String;
Begin
  if (F_Active) Then
     Begin
       DatabaseError(E1013);
       Exit;
     End;
  Tmp:=F_Workspace;
  F_Workspace:=Value;
  if csLoading In ComponentState then Exit;
  //*********************** RECREATE???
  Try
   RecreateCore;
  Except
   F_Workspace:=Tmp;
   RecreateCore;
   Raise;
  End;
End;


Procedure TKADaoDatabase.F_Set_DatabaseType(Value : String);
Begin
  if (F_Active) Then
     Begin
       DatabaseError(E1014);
       Exit;
     End;
  F_Database:='';
  F_DatabaseType:=Value;
End;

Function TKADaoDatabase.F_Get_CollatingOrder:String;
Var
  CO : Integer;
Begin
  Result := '';
  DatabaseLanguageInt:=0;
  if Not F_Active Then Exit;
  CO:=dbSortUndefined;
  Try
   CO := CoreDatabase.CollatingOrder;
  Except
  End;
  DatabaseLanguageInt:=CO;
  Case CO of
     dbSortGeneral	        : Result := 'General (English, French, German, Portuguese, Italian, and Modern Spanish)';
     dbSortArabic	        : Result := 'Arabic';
     dbSortChineseSimplified	: Result := 'Simplified Chinese';
     dbSortChineseTraditional	: Result := 'Traditional Chinese';
     dbSortCyrillic	        : Result := 'Bulgarian or Russian';
     dbSortCzech	        : Result := 'Czech';
     dbSortDutch	        : Result := 'Dutch';
     dbSortGreek	        : Result := 'Greek';
     dbSortHebrew	        : Result := 'Hebrew';
     dbSortHungarian	        : Result := 'Hungarian';
     dbSortIcelandic	        : Result := 'Icelandic';
     dbSortJapanese	        : Result := 'Japanese';
     dbSortKorean	        : Result := 'Korean';
     dbSortNeutral	        : Result := 'Neutral';
     dbSortNorwDan	        : Result := 'Norwegian or Danish';
     dbSortPolish	        : Result := 'Polish';
     dbSortSlovenian	        : Result := 'Slovenian';
     dbSortSpanish	        : Result := 'Spanish';
     dbSortSwedFin	        : Result := 'Swedish or Finnish';
     dbSortThai	                : Result := 'Thai';
     dbSortTurkish	        : Result := 'Turkish';
     dbSortUndefined	        : Result := 'Undefined or unknown';
  End;
  F_CollatingOrder:=Result;
End;


Procedure TKADaoDatabase.F_Set_EngineType(Value : Integer);
Var
  Tmp : Integer;
Begin
  if (F_Active) Then
     Begin
       DatabaseError(E1015);
       Exit;
     End;
  Tmp:=F_EngineType;
  F_EngineType:=Value;
  if csLoading In ComponentState then Exit;
  //*********************** RECREATE???
  Try
   RecreateCore;
  Except
   F_EngineType:=Tmp;
   RecreateCore;
   Raise;
  End;
End;

Procedure TKADaoDatabase.F_Set_PrivateEngine(Value : Boolean);
Var
  Tmp : Boolean;
Begin
  if (F_Active) Then
     Begin
       DatabaseError(E1016);
       Exit;
     End;
  Tmp:=F_PrivateEngine;
  F_PrivateEngine:=Value;
  if csLoading In ComponentState then Exit;
  //*********************** RECREATE???
  Try
   RecreateCore;
  Except
   F_PrivateEngine:=Tmp;
   RecreateCore;
   Raise;
  End;
End;

Procedure TKADaoDatabase.F_Set_ShowSysObjects(Value : Boolean);
Begin
 F_ShowSysObjects := Value;
 if F_Active Then RefreshDefinitions;
End;

Function  TKADaoDatabase.F_Get_DatabaseType:String;
Begin
  Result:=F_DatabaseType;
End;

Procedure TKADaoDatabase.F_Set_ReadOnly(Value : Boolean);
{$IFDEF USEDB}
Var
  X      : Integer;
  ATable : TKADaoTable;
 {$ENDIF}
Begin
 if (F_Active) Then
     Begin
       DatabaseError(E1017);
       Exit;
     End;
 F_ReadOnly:=Value;
 {$IFDEF USEDB}
 if F_ReadOnly Then
    Begin
     For X :=0 To F_ActiveTableNames.Count-1 do
      Begin
      ATable:=TKADaoTable(F_ActiveTableNames.Objects[X]);
      ATable.ReadOnly:=True;
     End;
    End;
 {$ENDIF}
End;

Procedure TKADaoDatabase.F_Set_DynaDao(Value: Boolean);
Begin
 //****************** READ ONLY
End;


Procedure TKADaoDatabase.F_Set_Exclusive(Value : Boolean);
Begin
 if (F_Active) Then
     Begin
       DatabaseError(E1018);
       Exit;
     End;
 F_Exclusive:=Value;
End;

Procedure TKADaoDatabase.F_Set_LoginPrompt(Value : Boolean);
Begin
 if (F_Active) Then
     Begin
       DatabaseError(E1019);
       Exit;
     End;
 F_LoginPrompt:=Value;
End;

Procedure TKADaoDatabase.F_Set_UserName(Value : String);
Begin
 if (F_Active) Then
     Begin
       DatabaseError(E1020);
       Exit;
     End;
 F_UserName:=Value;
 if csLoading in ComponentState Then Exit;
 Try
  RecreateCore;
 Except
 End;
End;

Procedure TKADaoDatabase.F_Set_Password(Value : String);
Begin
 if (F_Active) Then
     Begin
       DatabaseError(E1021);
       Exit;
     End;
 F_Password:=Value;
 if csLoading in ComponentState Then Exit;
 Try
  RecreateCore;
 Except
 End;
End;

Procedure TKADaoDatabase.F_Set_DatabasePassword(Value : String);
Begin
 if (F_Active) Then
     Begin
       DatabaseError(E1022);
       Exit;
     End;
 F_DatabasePassword:=Value;
End;

Procedure TKADaoDatabase.GoOffline;
Begin
 F_Offline := True;
End;

Procedure TKADaoDatabase.GoOnline;
Begin
 F_Offline := False;
End;

Procedure TKADaoDatabase.AddRNToTransaction(TableName : String;RN:Integer);
Var
 SL : TStringList;
 I  : Integer;
Begin
 if F_TransInfo.Count = 0 Then Exit;
 SL := TStringList.Create;
 Try
  SL.CommaText := F_TransInfo.Strings[F_TransInfo.Count-1];
  I := SL.IndexOfName(TableName);
  if I <> -1 Then
     Begin
       SL.Values[TableName] := IntToStr(RN);
     End
 Else
     Begin
      if F_TransInfo.Strings[F_TransInfo.Count-1] <> '' Then
         SL.Add(','+TableName+'='+IntToStr(RN))
      Else
         SL.Add(TableName+'='+IntToStr(RN))
     End;
 F_TransInfo.Strings[F_TransInfo.Count-1]:=SL.CommaText;
 Except
 End;
 SL.Free;
End;

Function TKADaoDatabase.F_GetTableRN(Tables:String;TableName:String):String;
Var
 SL : TStringList;
 I  : Integer;
Begin
 Result := '-1';
 SL := TStringList.Create;
 Try
  SL.CommaText := Tables;
  I := SL.IndexOfName(TableName);
  if I <> -1 Then Result := SL.Values[TableName];
 Except
 End;
 SL.Free;
End;



Procedure TKADaoDatabase.StartTransaction;
{$IFDEF USEDB}
Var
  X       : Integer;
  S       : String;
  ATable  : TKADaoTable;
{$ENDIF}
Begin
  if (NOT F_Active) Then
     Begin
       DatabaseError(E1023);
       Exit;
     End;
  CoreWorkspace.BeginTrans;
  {$IFDEF USEDB}
  if F_TrackTransactions Then
     Begin
       S:= '';
       For X := 0 To F_ActiveTableNames.Count-1 do
           Begin
             ATable:=TKADaoTable(F_ActiveTableNames.Objects[X]);
             if ATable.IsEmpty Then
                Begin
                  S := S+ATable.Name+'=-1';
                End
             Else
                Begin
                  if (ATable.Bookmarkable) And (ATable.MasterSource = Nil) Then
                     S := S+ATable.Name+'='+IntToStr(PInteger(PChar(ATable.Bookmark))^)
                  Else
                     S := S+ATable.Name+'='+IntToStr(ATable.RecNo);
                End;
             if X < F_ActiveTableNames.Count-1 Then S := S + ',';
           End;
       F_TransInfo.Add(S);
     End;
  {$ENDIF}
End;
                       
Procedure TKADaoDatabase.Commit;
Begin
 if (NOT F_Active) Then
     Begin
       DatabaseError(E1024);
       Exit;
     End;
 CoreWorkspace.CommitTrans(dbForceOSFlush);
 if F_TrackTransactions Then
    Begin
      if F_TransInfo.Count > 0 Then F_TransInfo.Delete(F_TransInfo.Count-1);
    End
End;

Procedure TKADaoDatabase.Rollback;
{$IFDEF USEDB}
Var
  X       : Integer;
  RN      : String;
  BKS     : Integer;
  PIN     : PInteger;
  ATable  : TKADaoTable;
{$ENDIF}
Begin
 CoreWorkspace.Rollback;
 {$IFDEF USEDB}
 For X := 0 to F_ActiveTableNames.Count-1 do
     Begin
      ATable:=TKADaoTable(F_ActiveTableNames.Objects[X]);
      ATable.RollbackRefresh;
      if F_TrackTransactions Then
         Begin
          if F_TransInfo.Count > 0 Then
             Begin
              RN := F_GetTableRN(F_TransInfo.Strings[F_TransInfo.Count-1],ATable.Name);
              if RN <> '-1' Then
                 Begin
                   Try
                     if NOT ATable.IsEmpty Then
                        Begin
                          if (ATable.Bookmarkable) And (ATable.MasterSource = Nil) Then
                             Begin
                               BKS := StrToInt(RN);
                               PIN := @BKS;
                               SetString(RN,PChar(PIN),4);
                               ATable.Bookmark := RN;
                             End
                          Else
                             Begin
                               ATable.RecNo := StrToInt(RN);
                             End;  
                        End;
                   Except
                   End;
                 End;
             End;
         End;
     End;
  if F_TrackTransactions Then
     Begin
      if F_TransInfo.Count > 0 Then F_TransInfo.Delete(F_TransInfo.Count-1);
     End;
 {$ENDIF}
End;

Function TKADaoDatabase.GetTransactionCount:Integer;
Begin
 Result := F_TransInfo.Count;
End;


Procedure TKADaoDatabase.RollbackRefresh;
{$IFDEF USEDB}
Var
  X       : Integer;
  ATable  : TKADaoTable;
{$ENDIF}
Begin
 {$IFDEF USEDB}
 For X :=0 To F_ActiveTableNames.Count-1 do
     Begin
      ATable:=TKADaoTable(F_ActiveTableNames.Objects[X]);
      ATable.RollbackRefresh;
     End;
 {$ENDIF}
End;



Procedure TKADaoDatabase.DBEngineLevel_StartTransaction;
Begin
 CoreDBEngine.BeginTrans;
End;

Procedure TKADaoDatabase.DBEngineLevel_Commit;
Begin
 CoreDBEngine.CommitTrans(dbForceOSFlush);
End;

Procedure TKADaoDatabase.DBEngineLevel_Rollback;
{$IFDEF USEDB}
Var
  X       : Integer;
  ATable  : TKADaoTable;
{$ENDIF}
Begin
 CoreDBEngine.Rollback;
 {$IFDEF USEDB}
 For X :=0 To F_ActiveTableNames.Count-1 do
     Begin
      ATable:=TKADaoTable(F_ActiveTableNames.Objects[X]);
      ATable.RollbackRefresh;
     End;
 {$ENDIF}
End;

Procedure TKADaoDatabase.WorkspaceLevel_StartTransaction;
Begin
 GoOnline;
 CoreWorkspace.BeginTrans;
End;

Procedure TKADaoDatabase.WorkspaceLevel_Commit;
Begin
 GoOnline;
 CoreWorkspace.CommitTrans(dbForceOSFlush);
End;

Procedure TKADaoDatabase.WorkspaceLevel_Rollback;
{$IFDEF USEDB}
Var
  X       : Integer;
  ATable  : TKADaoTable;
{$ENDIF}
Begin
 GoOnline;
 CoreWorkspace.Rollback;
 {$IFDEF USEDB}
 For X :=0 To F_ActiveTableNames.Count-1 do
     Begin
      ATable:=TKADaoTable(F_ActiveTableNames.Objects[X]);
      ATable.RollbackRefresh;
     End;
 {$ENDIF}
End;

//********************************************** WORKS ONLY ON DAO 3.5X
//                                              ON DAO 3.6 USE COMPACT DATABASE
//                                              WICH ALSO DOES REPAIR
//******************************************************************************
Procedure TKADaoDatabase.RepairAccessDatabase(DatabaseName,Password:String);
Begin
  if F_DaoVersion='3.5' Then
     CoreDBEngine.RepairDatabase(DatabaseName)
  Else
     CompactAccessDatabase(DatabaseName,Password);
End;

Procedure TKADaoDatabase.RepairAccessDatabaseEx(DatabaseName : String;
                                               NewLocale    : String;
                                               Encrypt      : Boolean;
                                               Decrypt      : Boolean;
                                               NewVersion   : Integer;
                                               Password     : String);
Begin
  if F_DaoVersion = '3.5' Then
     CoreDBEngine.RepairDatabase(DatabaseName)
  Else
     CompactAccessDatabaseEx(DatabaseName,NewLocale,Encrypt,Decrypt,NewVersion,Password);
End;

Procedure  TKADaoDatabase.CompactAccessDatabase(DatabaseName,Password:String);
Var
  TempName : Array[0..1000] of Char;
  TempPath : String;
  Name     : String;
Begin
  TempPath:=ExtractFilePath(DatabaseName);
  if TempPath='' Then TempPath:=GetCurrentDir;
  GetTempFileName(PChar(TempPath),'mdb',0,TempName);
  Name:=StrPas(TempName);
  DeleteFile(Name);
  if Password <> '' Then Password:=';pwd='+Password;
  OleVariant(CoreDBEngine).CompactDatabase(DatabaseName,Name,,,Password);
  DeleteFile(DatabaseName);
  RenameFile(Name,DatabaseName);
End;

Procedure  TKADaoDatabase.CompactAccessDatabaseEx(DatabaseName: String;
                                                  NewLocale   : String;
                                                  Encrypt     : Boolean;
                                                  Decrypt     : Boolean;
                                                  NewVersion  : Integer;
                                                  Password    : String);
Var
  TempName : Array[0..1000] of Char;
  TempPath : String;
  Name     : String;
  Options  : Integer;
Begin
  TempPath:=ExtractFilePath(DatabaseName);
  if TempPath='' Then TempPath:=GetCurrentDir;
  GetTempFileName(PChar(TempPath),'mdb',0,TempName);
  Name:=StrPas(TempName);
  DeleteFile(Name);
  Options:=0;
  if Encrypt Then Options := dbEncrypt;
  if Decrypt Then Options := dbDecrypt;
  if NewVersion <> 0 Then Options:=Options+NewVersion;
  if Password <> '' Then Password:=';pwd='+Password;
  CoreDBEngine.CompactDatabase(DatabaseName,Name,NewLocale,Options,Password);
  DeleteFile(DatabaseName);
  RenameFile(Name,DatabaseName);
End;

Procedure TKADaoDatabase.CreateAccessDatabase(DatabaseName:String);
Var
 CreateOptions : String;
Begin
 CreateOptions := Format(dbLangGeneral,['0x0409','1252','0']);
 GoOnline;
 {$IFDEF DAO35}
 CoreWorkspace.CreateDatabase(DatabaseName,CreateOptions, dbVersion30);
 {$ENDIF}
 {$IFDEF DAO36}
 CoreWorkspace.CreateDatabase(DatabaseName,CreateOptions, dbVersion40);
 {$ENDIF}
 {$IFDEF DAO120}
 CoreWorkspace.CreateDatabase(DatabaseName,CreateOptions, dbVersion120);
 {$ENDIF}
 {$IFDEF DYNADAO}
 if F_DaoVersion =  '3.5' then CoreWorkspace.CreateDatabase(DatabaseName,CreateOptions, dbVersion30);
 if F_DaoVersion =  '3.6' then CoreWorkspace.CreateDatabase(DatabaseName,CreateOptions, dbVersion40);
 if F_DaoVersion = '12.0' then CoreWorkspace.CreateDatabase(DatabaseName,CreateOptions, dbVersion120);
 {$ENDIF}
End;

Procedure TKADaoDatabase.CreateAccessDatabaseEx(DatabaseName,LANGID,CP,COUNTRY,Password,Version:String;Encrypt:Boolean);
Var
 CreateOptions:String;
Begin
 CreateOptions:=Format(dbLangGeneral,[LANGID,CP,COUNTRY]);
 if Password <> '' Then CreateOptions:=CreateOptions+';PWD='+Password;
 GoOnline;
 {$IFDEF DAO35}
 if Encrypt Then
    CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
 Else
    CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30);
 {$ENDIF}

 {$IFDEF DAO36}
  if Version='30' Then
     if Encrypt Then
        CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
     Else
        CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30)
  Else
     if Encrypt Then
        CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40 OR dbEncrypt)
     Else
        CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40);
 {$ENDIF}

 {$IFDEF DAO120}
  if Version='30' Then
     Begin
        if Encrypt Then
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
        Else
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30)
     End
  Else
  if Version='40' Then
     Begin
       if Encrypt Then
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40 OR dbEncrypt)
       Else
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40);
     End
 Else
  if Version='120' Then
     Begin
       if Encrypt Then
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion120 OR dbEncrypt)
       Else
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion120);
     End
 {$ENDIF}

 {$IFDEF DYNADAO}
 if F_DaoVersion='3.5'  Then
    Begin
       if Encrypt Then
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
       Else
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30);
    End;
 //*****************************************************************************
  if F_DaoVersion='3.6'  Then
     Begin
        if Version='30' Then
           Begin
             if Encrypt Then
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
             Else
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30)
           End
        Else
        if Version='40' Then
           Begin
             if Encrypt Then
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40 OR dbEncrypt)
             Else
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40);
           End;
     End;
   if F_DaoVersion='12.0'  Then
     Begin
        if Version='30' Then
           Begin
             if Encrypt Then
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
             Else
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30)
           End
        Else
        if Version='40' Then
           Begin
             if Encrypt Then
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40 OR dbEncrypt)
             Else
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40);
           End
        Else
        if Version='120' Then
           Begin
             if Encrypt Then
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion120 OR dbEncrypt)
             Else
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion120);
           End

     End;
 {$ENDIF}
End;

Procedure TKADaoDatabase.CreateAccessDatabaseEx2(DatabaseName,Language,Password,Version:String;Encrypt:Boolean);
Var
 CreateOptions:String;
Begin
 CreateOptions:=Language;
 if Password <> '' Then CreateOptions:=CreateOptions+';PWD='+Password;
 GoOnline;

 {$IFDEF DAO35}
 if Encrypt Then
    CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
 Else
    CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30);
 {$ENDIF}

 {$IFDEF DAO36}
  if Version='30' Then
     if Encrypt Then
        CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
     Else
        CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30)
  Else
     if Encrypt Then
        CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40 OR dbEncrypt)
     Else
        CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40);
 {$ENDIF}

 {$IFDEF DAO120}
  if Version='30' Then
     Begin
       if Encrypt Then
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
       Else
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30)
    End
  Else
  if Version='40' Then
     Begin
       if Encrypt Then
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40 OR dbEncrypt)
       Else
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40);
     End
  Else
  if Version='120' Then
     Begin
       if Encrypt Then
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion120 OR dbEncrypt)
       Else
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion120);
     End;
 {$ENDIF}

 {$IFDEF DYNADAO}
 if F_DaoVersion='3.5'  Then
    Begin
       if Encrypt Then
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
       Else
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30);
    End;
 //*****************************************************************************
  if F_DaoVersion='3.6'  Then
     Begin
        if Version='30' Then
           Begin
               if Encrypt Then
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
               Else
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30)
           End
        Else
        if Version='40' Then
           Begin
               if Encrypt Then
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40 OR dbEncrypt)
               Else
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40);
           End;
    End;
 //*****************************************************************************
 if F_DaoVersion='12.0'  Then
     Begin
        if Version='30' Then
           Begin
               if Encrypt Then
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
               Else
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30)
           End
        Else
        if Version='40' Then
           Begin
               if Encrypt Then
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40 OR dbEncrypt)
               Else
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40);
           End
        Else
        if Version='120' Then
           Begin
               if Encrypt Then
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion120 OR dbEncrypt)
               Else
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion120);
           End;
    End;
 {$ENDIF}
End;

Function TKADaoDatabase.ChangeDatabasePassword(OldPassword,NewPassword:String):Boolean;
Begin
  Result := False;
  if NOT F_Active Then DatabaseError(E1025);
  if NOt F_Exclusive Then DatabaseError(E1026);
  Try
    CoreDatabase.NewPassword(OldPassword,NewPassword);
  Except
   Exit;
  End;
  Result := True;
End;

Function TKADaoDatabase.RegisterDatabase(DatabaseName, DriverName:String; Silent:Boolean; Attributes:String):Boolean;
Begin
  Result := False;
  Try
    CoreDBEngine.RegisterDatabase(DatabaseName,DriverName,Silent,Attributes);
  Except
   Exit;
  End;
  Result := True;
End;

Procedure TKADaoDatabase.RenameTable(OldTableName,NewTableName:String);
Begin
 RefreshDefinitions;
 CoreDatabase.TableDefs.Item[OldTableName].Name:=NewTableName;
 RefreshDefinitions;
End;

Procedure TKADaoDatabase.DeleteTable(TableName:String);
Begin
 RefreshDefinitions;
 CoreDatabase.TableDefs.Delete(TableName);
 RefreshDefinitions;
End;

//******************************************************************************
//  1 = Primary index
//  2 = Unique
//  4 = NormalIndex
//******************************************************************************
Function TKADaoDatabase.HasPrimaryKey(NewTable:OleVariant):Boolean;
Var
  X:Integer;
Begin
 Result:=False;
 For X :=0 to NewTable.Indexes.Count-1 do
     Begin
       if NewTable.Indexes.Item[X].Primary Then
          Begin
            Result:=True;
            Exit;
          End;
     End;
End;

Function TKADaoDatabase.TablePrimaryKeyName(NewTable:OleVariant):String;
Var
  X:Integer;
Begin
 Result:='';
 For X :=0 to NewTable.Indexes.Count-1 do
     Begin
       if NewTable.Indexes.Item[X].Primary Then
          Begin
            Result:=NewTable.Indexes.Item[X].name;
            Exit;
          End;
     End;
End;

Procedure TKADaoDatabase.DeletePrimaryKey(NewTable:OleVariant);
Var
  X:Integer;
Begin
 For X :=0 to NewTable.Indexes.Count-1 do
     Begin
       if NewTable.Indexes.Item[X].Primary Then
          Begin
            NewTable.Indexes.Delete(NewTable.Indexes.Item[X].Name);
            Exit;
          End;
     End;
End;


Function TKADaoDatabase.CreateIndex(TableName,FieldName:String;IndexType:Integer):Boolean;
Var
  NewTable         : OleVariant;
  NewField         : OleVariant;
  NewIndex         : OleVariant;
  PrimIndex        : OleVariant;
  PrimaryKeyName   : String;
Begin
  Result:=False;
  RefreshDefinitions;
  Try
   NewTable  := CoreDatabase.TableDefs.Item[TableName];
   if Pos('paradox',AnsiLowerCase(F_DatabaseType)) > 0 Then PrimaryKeyName := TableName Else PrimaryKeyName:='PrimaryKey';
   if ((IndexType And 1) > 0) Then
      Begin
        if HasPrimaryKey(NewTable) Then DeletePrimaryKey(NewTable);
        PrimIndex          := NewTable.CreateIndex(PrimaryKeyName);
        PrimIndex.Primary  := True;
        PrimIndex.Unique   := True;
        NewField           := NewTable.CreateField(FieldName);
        PrimIndex.Fields.AppEnd(NewField);
        NewTable.Indexes.AppEnd(PrimIndex);
        if NOT ((IndexType And 2) > 0) Then IndexType:=IndexType+2;
      End;
   if ((IndexType And 2) > 0) or ((IndexType And 4) > 0) Then
      Begin
        NewIndex  := NewTable.CreateIndex(FieldName);
        if ((IndexType And 2) = 0) Then NewIndex.Unique  := False  Else  NewIndex.Unique  := True;
        NewField := NewTable.CreateField(FieldName);
        NewIndex.Fields.AppEnd(NewField);
        NewTable.Indexes.AppEnd(NewIndex);
      End;
  Except
   Exit;
  End;
  RefreshDefinitions;
  Result:=True;
End;

Procedure TKADaoDatabase.RenameIndex(TableName,OldIndexName,NewIndexName:String);
Begin
  RefreshDefinitions;
  CoreDatabase.TableDefs.Item[TableName].Indexes.Item[OldIndexName].Name:=NewIndexName;
  RefreshDefinitions;
End;

Procedure TKADaoDatabase.DeleteIndexByName(TableName,IndexName:String);
Begin
 RefreshDefinitions;
 CoreDatabase.TableDefs.Item[TableName].Indexes.Delete(IndexName);
 RefreshDefinitions;
End;

Procedure TKADaoDatabase.DeleteIndexByFieldName(TableName,FieldName:String);
Var
 X         : Integer;
 TmpName   : String;
 IndexName : String;
 NotFound  : Boolean;
Begin
 RefreshDefinitions;
 Try
  Repeat
   NotFound:=True;
   CoreDatabase.TableDefs.Refresh;
   For X:=0 To CoreDatabase.TableDefs.Item[TableName].Indexes.Count-1 do
       Begin
         TmpName:=CoreDatabase.TableDefs.Item[TableName].Indexes.Item[X].Fields.Item[0].Name;
         if TmpName=FieldName Then
            Begin
              IndexName:=CoreDatabase.TableDefs.Item[TableName].Indexes.Item[X].Name;
              DeleteIndexByName(TableName,IndexName);
              NotFound:=False;
              Break;
            End;
       End;
  Until NotFound;
 Except
 End;
 RefreshDefinitions;
End;

Procedure TKADaoDatabase.DeleteField(TableName,FieldName:String);
Var
 X,Y       : Integer;
 TmpName   : String;
 IndexName : String;
 Found     : Boolean;
Begin
 RefreshDefinitions;
 Try
  Repeat
   Found:=False;
   CoreDatabase.TableDefs.Refresh;
   For X:=0 To CoreDatabase.TableDefs.Item[TableName].Indexes.Count-1 do
       Begin
         For Y := 0 To CoreDatabase.TableDefs.Item[TableName].Indexes.Item[X].Fields.Count-1 do
             Begin
               TmpName:=CoreDatabase.TableDefs.Item[TableName].Indexes.Item[X].Fields.Item[Y].Name;
               if AnsiCompareText(TmpName,FieldName)=0 Then
                  Begin
                    IndexName:=CoreDatabase.TableDefs.Item[TableName].Indexes.Item[X].Name;
                    DeleteIndexByName(TableName,IndexName);
                    Found:=True;
                    Break;
                  End;
             End;
         if Found Then Break;
       End;
  Until NOT Found;
 Except
 End;
 CoreDatabase.TableDefs.Item[TableName].Fields.Delete(FieldName);
 RefreshDefinitions;
End;

Procedure TKADaoDatabase.RenameField(TableName,OldFieldName,NewFieldName:String);
Begin
  RefreshDefinitions;
  CoreDatabase.TableDefs.Item[TableName].Fields.Item[OldFieldName].Name:=NewFieldName;
  RefreshDefinitions;
End;

Function TKADaoDatabase.EmptyTable(TableName:String):Boolean;
Begin
  Result:=False;
  Try
    CoreDatabase.Execute('DELETE * FROM ['+TableName+'];',0);
  Except
    Exit;
  End;
  Result:=True;
End;

Function TKADaoDatabase.CreateEmptyTable(TableName:String):Boolean;
Var
  NewTable : OleVariant;
  NewField : OleVariant;
Begin
 Result:=False;
 Try
   NewTable  := OleVariant(CoreDatabase).CreateTableDef(TableName);
   NewField  := NewTable.CreateField('Temp',DAOApi.dbLong,0);
   NewTable.Fields.AppEnd(NewField);
 Except
   Exit;
 End;
 CoreDatabase.TableDefs.AppEnd(IDispatch(TVarData(NewTable).vDispatch));
 RefreshDefinitions;
 DeleteField(TableName,'Temp');
 Result:=True;
End;

//******************************************************************************
//  1 = Primary index
//  2 = Unique
//  4 = NormalIndex
//******************************************************************************
Function TKADaoDatabase.CreateTable(TableName:String; FieldNames : Variant; FieldTypes : Variant; FieldSizes : Variant; FieldIndexes:Variant; FieldsRequired:Variant):Boolean;
Var
  NewTable       : OleVariant;
  NewField       : OleVariant;
  NewIndex       : OleVariant;
  PrimIndex      : OleVariant;
  Primary        : Boolean;
  X              : Integer;
  Count          : Integer;
  AutoInc        : Boolean;
  IdxName        : String;
  PrimaryKeyName : String;
Begin
 if (NOT F_Active) Then
     Begin
       DatabaseError(E1027);
       CreateTable:=False;
       Exit;
     End;
 if TableName='' Then
    Begin
       DatabaseError(E1028);
       CreateTable:=False;
       Exit;
     End;
 Primary := False;
 NewTable:=OleVariant(CoreDatabase).CreateTableDef(TableName);
 Count:=VarArrayHighBound(FieldTypes,VarArrayDimCount(FieldTypes));
 if Pos('paradox',AnsiLowerCase(F_DatabaseType)) > 0 Then PrimaryKeyName := TableName Else PrimaryKeyName:='PrimaryKey';
 For X:=0 to Count do
     Begin
      AutoInc:=False;
      if FieldTypes[X]=dbAutoIncInteger Then
         Begin
           FieldTypes[X]:=dbLong;
           AutoInc:=True;
         End;
      NewField  := NewTable.CreateField(FieldNames[X],FieldTypes[X],FieldSizes[X]);
      NewTable.Fields.AppEnd(NewField);
      if AutoInc Then NewTable.Fields[FieldNames[X]].Attributes:=dbAutoIncrField;
      //************************************************************************
      // First Create Primary Key Indexes
      //************************************************************************
      if FieldIndexes[X] > 0 Then
         Begin
           if ((FieldIndexes[X] And 1) > 0) Then
               Begin
                 if Not Primary Then
                    Begin
                       PrimIndex          := NewTable.CreateIndex(PrimaryKeyName);
                       PrimIndex.Primary  := True;
                       PrimIndex.Unique   := True;
                       Primary:=True;
                    End;
                 NewField         := NewTable.CreateField(FieldNames[X],FieldTypes[X],FieldSizes[X]);
                 PrimIndex.Fields.AppEnd(NewField);
               End
         End;
     End;
 if Primary Then NewTable.Indexes.AppEnd(PrimIndex);
 //*****************************************************************************
 // Then create Unique and NonUnique indexes
 //*****************************************************************************
 For X:=0 to Count do
     Begin
        if (FieldIndexes[X] And 2 > 0) Or (FieldIndexes[X] And 4 > 0) Then
         Begin
           IdxName:=FieldNames[X];
           NewIndex:=NewTable.CreateIndex(IdxName);
           if ((FieldIndexes[X] And 2) > 0) Then NewIndex.Unique  := True;
           NewField  := NewTable.CreateField(FieldNames[X],FieldTypes[X],FieldSizes[X]);
           NewIndex.Fields.AppEnd(NewField);
           NewTable.Indexes.AppEnd(NewIndex);
         End;
     End;
 CoreDatabase.TableDefs.AppEnd(IDispatch(TVarData(NewTable).vDispatch));
 //*****************************************************************************
 // Then mark required fields
 //*****************************************************************************
 RefreshDefinitions;
 For X:=0 to Count do
     Begin
       if FieldsRequired[X]=1 Then
          CoreDatabase.TableDefs.Item[TableName].Fields.Item[FieldNames[X]].Required := True;
     End;
 RefreshDefinitions;
 CreateTable:=True;
End;

//******************************************************************************
//  1 = Primary index
//  2 = Unique
//  4 = NormalIndex
//******************************************************************************
Function TKADaoDatabase.AddFieldsToTable(TableName:String; FieldNames : Variant; FieldTypes : Variant; FieldSizes : Variant; FieldIndexes:Variant;  FieldsRequired:Variant):Boolean;
Var
  NewTable          : OleVariant;
  NewField          : OleVariant;
  PrimIndex         : OleVariant;
  NewIndex          : OleVariant;
  X                 : Integer;
  Count             : Integer;
  Primary           : Boolean;
  PrimaryKeyName    : String;
  IdxName           : String;
Begin
if (NOT F_Active) Then
     Begin
       DatabaseError(E1029);
       AddFieldsToTable:=False;
       Exit;
     End;
 if TableName='' Then
    Begin
       DatabaseError(E1030);
       AddFieldsToTable:=False;
       Exit;
     End;
 NewTable:=CoreDatabase.TableDefs.Item[TableName];
 //*****************************************************************************
 // Delete PrimaryKey if new Primary key is required
 //*****************************************************************************
 Primary := False;
 Count:=VarArrayHighBound(FieldTypes,VarArrayDimCount(FieldTypes));
 For X:=0 to Count do
     Begin
       if ((FieldIndexes[X] And 1) = 1) Then
          Begin
           Primary:=True;
          End;
     End;
 if Pos('paradox',AnsiLowerCase(F_DatabaseType)) > 0 Then PrimaryKeyName := TableName Else PrimaryKeyName:='PrimaryKey';
 if Primary then DeletePrimaryKey(NewTable);
 //*****************************************************************************
 Primary := False;
 For X:=0 to Count do
     Begin
      if FieldTypes[X] = dbAutoIncInteger then
         Begin
           FieldTypes[X]:= dbLong;
           NewField := NewTable.CreateField(FieldNames[X], dbLong,FieldSizes[X]);
           NewField.Attributes:= NewField.Attributes or dbAutoIncrField;
         End
      else
         Begin
           NewField  := NewTable.CreateField(FieldNames[X],FieldTypes[X],FieldSizes[X]);
         End;
      NewTable.Fields.AppEnd(NewField);
      //************************************************************************
      // First Create Primary Key Indexes
      //************************************************************************
      if FieldIndexes[X] > 0 Then
         Begin
           if ((FieldIndexes[X] And 1) = 1) Then
               Begin
                 if Not Primary Then
                    Begin
                       PrimIndex          := NewTable.CreateIndex(PrimaryKeyName);
                       PrimIndex.Primary  := True;
                       PrimIndex.Unique   := True;
                       Primary:=True;
                    End;
                 NewField         := NewTable.CreateField(FieldNames[X],FieldTypes[X],FieldSizes[X]);
                 PrimIndex.Fields.AppEnd(NewField);
               End
         End;
     End;
 if Primary Then NewTable.Indexes.AppEnd(PrimIndex);
 //*****************************************************************************
 // Then create Unique and NonUnique indexes
 //*****************************************************************************
 For X:=0 to Count do
     Begin
        if (FieldIndexes[X] And 2 > 0) Or (FieldIndexes[X] And 4 > 0) Then
         Begin
           IdxName:=FieldNames[X];
           NewIndex:=NewTable.CreateIndex(IdxName);
           if ((FieldIndexes[X] And 2) > 0) Then NewIndex.Unique  := True;
           NewField  := NewTable.CreateField(FieldNames[X],FieldTypes[X],FieldSizes[X]);
           NewIndex.Fields.AppEnd(NewField);
           NewTable.Indexes.AppEnd(NewIndex);
         End;
     End;
 RefreshDefinitions;
 //*****************************************************************************
 // Then mark required fields
 //*****************************************************************************
 For X:=0 to Count do
     Begin
       if FieldsRequired[X]=1 Then
          CoreDatabase.TableDefs.Item[TableName].Fields.Item[FieldNames[X]].Required := True;
     End;
 RefreshDefinitions;
 AddFieldsToTable:=True;
End;

//******************************************************************************
// See _PredefinedTableTypes in DaoApi for information about TableType
//******************************************************************************
Procedure TKADaoDatabase.LinkExternalTable(Database,TableName,TableType:String;TableAttributes:Integer);
Var
 NewTable : OleVariant;
 TDEFName : String;
 X, L     : Integer;
Begin
 TDEFName:=TableName;
 L := Length(TDEFName);
 For X := 1 to L do If TDEFName[X]='.' Then TDEFName[X]:='#';
 NewTable:=OleVariant(CoreDatabase).CreateTableDef(TDEFName);
 if Pos('%s',TableType) > 0 Then
    NewTable.Connect         := Format(TableType,[Database])
 Else
    NewTable.Connect         := TableType;
 if TableAttributes <> 0 Then NewTable.Attributes := TableAttributes;
 NewTable.SourceTableName := TableName;
 CoreDatabase.TableDefs.AppEnd(IDispatch(TVarData(NewTable).vDispatch));
End;

Procedure TKADaoDatabase.LinkExternalTableEx(Database,TableName,TableFileName,TableType:String;TableAttributes:Integer);
Var
 NewTable : OleVariant;
Begin
 NewTable:=OleVariant(CoreDatabase).CreateTableDef(TableName);
 if Pos('%s',TableType) > 0 Then
    NewTable.Connect         := Format(TableType,[Database])
 Else
    NewTable.Connect         := TableType;
 if TableAttributes <> 0 Then NewTable.Attributes := TableAttributes;
 NewTable.SourceTableName := TableFileName;
 CoreDatabase.TableDefs.AppEnd(IDispatch(TVarData(NewTable).vDispatch));
End;

Procedure TKADaoDatabase.RefreshLink(Database,TableName,TableType:String);
Var
 LinkedTable : OleVariant;
Begin
 LinkedTable:=OleVariant(CoreDatabase).TableDefs.Item[TableName];
 if Pos('%s',TableType) > 0 Then
    LinkedTable.Connect         := Format(TableType,[Database])
 Else
    LinkedTable.Connect         := TableType;
 LinkedTable.RefreshLink;
End;

Function TKADaoDatabase.CreateQueryDef(Name:String;SQL:String):Boolean;
Var
 {$IFDEF DYNADAO}
 Query : OleVariant;
 {$ELSE}                                                      
 Query : QueryDef;
 {$ENDIF}
Begin
 Query:=CoreDatabase.CreateQueryDef(Name,SQL);
 RefreshDefinitions;
 CreateQueryDef:=True;
End;

Procedure TKADaoDatabase.ModifyQueryDef(Name:String;SQL:String);
Begin
 RefreshDefinitions;
 CoreDatabase.QueryDefs.Item[Name].SQL:=SQL;
 RefreshDefinitions;
End;

Function TKADaoDatabase.GetQueryDefSQLText(Name:String):String;
Begin
 Try
   Result:=CoreDatabase.QueryDefs.Item[Name].SQL;
 Except
   Result:='';
 End;
End;

Procedure TKADaoDatabase.RenameQueryDef(OldQueryName,NewQueryName:String);
Begin
 RefreshDefinitions;
 CoreDatabase.QueryDefs.Item[OldQueryName].Name:=NewQueryName;
 RefreshDefinitions;
End;

Procedure TKADaoDatabase.DeleteQueryDef(QueryName:String);
Begin
 RefreshDefinitions;
 CoreDatabase.QueryDefs.Delete(QueryName);
 RefreshDefinitions;
End;

Function  TKADaoDatabase.F_ChooseDatabase: String;
var
   FileName              : String;
   Filter                : String;
   Temp                  : String;
   P                     : Integer;
   TableType             : String;
   DSN                   : String;
   DlgChooseOdbcDatabase : TODBCDialog;
   DlgChooseDatabase     : TOpenDialog;
Begin
  Result := '';
  If F_DatabaseType='' Then DatabaseError(E1031);
  If F_DatabaseType='ODBC' Then
    Begin
      DSN:=F_Database;
      F_Get_SystemDSNs(F_SystemDSNs);
      F_Get_UserDSNs(F_UserDSNs);
      Application.CreateForm(TODBCDialog,DlgChooseOdbcDatabase);
      if DlgChooseOdbcDatabase.Execute(F_SystemDSNs,F_UserDSNs,Dsn,F_UseODBCDialog) Then Result := DSN;
      DlgChooseOdbcDatabase.Free;
    End
  Else
    Begin
     DlgChooseDatabase := TOpenDialog.Create(Nil);
     FileName := Database;
     if FileName = '' then
        Begin
           DlgChooseDatabase.FileName   := '';
           if csDesigning in ComponentState Then
              DlgChooseDatabase.InitialDir := GetExeDir
           Else
              DlgChooseDatabase.InitialDir := GetExeDir;
        End
     Else
        Begin
           DlgChooseDatabase.FileName   := ExtractFileName(FileName);
           DlgChooseDatabase.InitialDir := ExtractFileDir(FileName);
        End;
     if F_DatabaseType='Access' Then
        Begin
         Filter  := '';
         if F_DaoVersion='12.0' Then
            Begin
              Filter  := Filter+'Microsoft Access 12 (*.accdb)|*.accdb';
              Filter  := Filter+'|Microsoft Access (*.mdb)|*.mdb';
            End
         Else
            Begin
              Filter  := Filter+'Microsoft Access (*.mdb)|*.mdb';
            End;
         Filter  := Filter+'|All files (*.*)|*.*';
         DlgChooseDatabase.Title:='Choose '+F_DatabaseType+' Database:';
         DlgChooseDatabase.Options:=[ofPathMustExist,ofFileMustExist,ofHideReadOnly];
         DlgChooseDatabase.Filter :=Filter;
         DlgChooseDatabase.DefaultExt:='mdb';
         if DlgChooseDatabase.Execute then Result := DlgChooseDatabase.FileName;
        End
     Else
        Begin
         Filter:=F_Get_DBTypeFileExtension(F_DatabaseType);
         TableType:=F_Get_DBTypeTableType(F_DatabaseType);
         if TableType='1' Then
            Begin
              if SelectDirectory(FileName,[],0) Then Result := FileName;
            End
         Else
            Begin
             Temp:=Filter;
             P:=Pos('(',Temp);
             if P > 0 Then
                Begin
                  Delete(Temp,1,P);
                  P:=Pos(')',Temp);
                  if P > 0 Then Temp:=Copy(Temp,1,P-1);
                  Filter:=Filter+'|'+Temp;
                End;
             Filter:=Filter+'|All files (*.*)|*.*';
             DlgChooseDatabase.Title:='Choose '+F_DatabaseType+' Database:';
             DlgChooseDatabase.Options:=[ofFileMustExist,ofPathMustExist,ofHideReadOnly];
             DlgChooseDatabase.Filter :=Filter;
             if DlgChooseDatabase.Execute then Result :=DlgChooseDatabase.FileName;
            End;
        End;
      DlgChooseDatabase.Free;
    End;
end;

//******************************************************************************
// EASY WRAPPER TO CREATE TABLES USING METHODS SIMILAR TO BORLAND'S TTABLE
//******************************************************************************

{$IFDEF USEDB}
Constructor TKADaoTableManager.Create(Database : TKADaoDatabase);
Begin
  F_Database       := Database;
  F_DummyDataset   := TDummyDataset.Create(Nil);
  IndexDefs        := TIndexDefs.Create(F_DummyDataset);
  FieldDefs        := TFieldDefs.Create(F_DummyDataset);
  TableName        := '';
End;

Destructor  TKADaoTableManager.Destroy;
Begin
  FieldDefs.Free;
  IndexDefs.Free;
  F_DummyDataset.Free;
  Inherited Destroy;
End;

Function TKADaoTableManager.CheckStatus:Boolean;
Begin
 Result := False;
 if Not Assigned(F_Database) Then DatabaseError(E1032);
 if Not (F_Database.Connected) Then DatabaseError(E1025);
 if TableName='' Then
    Begin
      DatabaseError('Missing TableName!');
      Exit;
    End;
 Result := True;
End;

Procedure TKADaoTableManager.StringToList(Items: String; List: TStringList);
var
  X: Integer;
begin
  For X:= 1 To Length(Items) Do If Items[X] = ';' Then Items[X]:= #13;
  List.Clear;
  List.Text:=Items;
  For X:= 0 To List.Count - 1 Do List[X]:= Trim(List[X]);
end;

Procedure   TKADaoTableManager.AppendTable;
Var
  FN,FT,FS,FI,FR  : Variant;
  Count           : Integer;
  X               : Integer;
  Idx             : Integer;
Begin
  if Not CheckStatus Then Exit;
  Count:=FieldDefs.Count-1;
  FN:=VarArrayCreate([0, Count], varOleStr);
  FT:=VarArrayCreate([0, Count], varInteger);
  FS:=VarArrayCreate([0, Count], varInteger);
  FI:=VarArrayCreate([0, Count], varInteger);
  FR:=VarArrayCreate([0, Count], varInteger);
  For X :=0 To Count Do
      Begin
        FN[X]:=FieldDefs.Items[X].Name;
        FT[X]:=BDEToDao(FieldDefs.Items[X].DataType);
        FS[X]:=DaoSizeToBDESize(FT[X],FieldDefs.Items[X].Size);
        if FieldDefs.Items[X].Required Then FR[X]:=1 Else FR[X]:=0;
        Idx:=0;
        FI[X]:=Idx;
      End;
  F_Database.AddFieldsToTable(TableName,FN,FT,FS,FI,FR);
  VarClear(FN); FN:=NULL;
  VarClear(FT); FT:=NULL;
  VarClear(FS); FS:=NULL;
  VarClear(FI); FI:=NULL;
  VarClear(FR); FR:=NULL;
  CreateIndex(False);
End;


Procedure  TKADaoTableManager.CreateIndex(PreservePrimaryKeys:Boolean);
Var
  Count           : Integer;
  NT,NF,FI        : OleVariant;
  X,Y             : Integer;
  PrimaryKeyName  : String;
  Primary         : Boolean;
  FieldNames      : TStringList;
  INam            : String;
Begin
  Count:=IndexDefs.Count;
  if Count=0 Then Exit;
  if Not CheckStatus Then Exit;
  FieldNames:=TStringList.Create;
  Try
   NT := F_Database.CoreDatabase.TableDefs.Item[TableName];
   Primary:=False;
   For X :=0 To Count-1 Do
      Begin
       if ixPrimary in IndexDefs[X].Options Then
          Begin
            Primary := True;
            PrimaryKeyName:=IndexDefs[X].Name;
          End;
      End;
   if Pos('paradox',AnsiLowerCase(F_Database.F_DatabaseType)) > 0 Then PrimaryKeyName := TableName;
   if Primary Then
     Begin
       if F_Database.HasPrimaryKey(NT) Then F_Database.DeletePrimaryKey(NT);
       FI:=NT.CreateIndex(PrimaryKeyName);
       FI.Primary := True;
       For X :=0 To Count-1 Do
          Begin
           if ixPrimary in IndexDefs[X].Options Then
              Begin
                 StringToList(IndexDefs[X].Fields,FieldNames);
                 For Y := 0 To FieldNames.Count-1 do
                     Begin
                       NF:=FI.CreateField(FieldNames.Strings[Y]);
                       FI.Fields.AppEnd(NF);
                     End;
              End;
          End;
       NT.Indexes.AppEnd(FI);
     End;
   For X :=0 To Count-1 Do
      Begin
       if (IndexDefs[X].Options=[])
       or (IndexDefs[X].Options=[ixPrimary,ixUnique])
       or (IndexDefs[X].Options=[ixUnique])Then
          Begin
           StringToList(IndexDefs[X].Fields,FieldNames);
           if IndexDefs[X].Name='' Then
              INam:= FieldNames.Strings[0]
           Else
              INam:=IndexDefs[X].Name;
           if (AnsiCompareText(INam,PrimaryKeyName)=0) And (Primary) Then
               Begin
                 //******************* Don't Create again PRIMARY KEY
               End
           Else
               Begin
                FI:=NT.CreateIndex(INam);
                if ixUnique in IndexDefs[X].Options Then FI.Unique := True;
                For Y := 0 To FieldNames.Count-1 do
                    Begin
                      NF:=FI.CreateField(FieldNames.Strings[Y]);
                      FI.Fields.AppEnd(NF);
                    End;
                NT.Indexes.AppEnd(FI);
               End;
          End;
      End;
   F_Database.RefreshDefinitions;
  Finally
    FieldNames.Free;
  End;
End;

Procedure   TKADaoTableManager.CreateTable;
Var
  FN,FT,FS,FI,FR  : Variant;
  Count           : Integer;
  X               : Integer;
  Idx             : Integer;
Begin
  if Not CheckStatus Then Exit;
  Count:=FieldDefs.Count-1;
  FN:=VarArrayCreate([0, Count], varOleStr);
  FT:=VarArrayCreate([0, Count], varInteger);
  FS:=VarArrayCreate([0, Count], varInteger);
  FI:=VarArrayCreate([0, Count], varInteger);
  FR:=VarArrayCreate([0, Count], varInteger);
  For X :=0 To Count Do
      Begin
        FN[X]:=FieldDefs.Items[X].Name;
        FT[X]:=BDEToDao(FieldDefs.Items[X].DataType);
        FS[X]:=DaoSizeToBDESize(FT[X],FieldDefs.Items[X].Size);
        if FieldDefs.Items[X].Required Then FR[X]:=1 Else FR[X]:=0;
        Idx:=0;
        FI[X]:=Idx;
      End;
  F_Database.CreateTable(TableName,FN,FT,FS,FI,FR);
  VarClear(FN); FN:=NULL;
  VarClear(FT); FT:=NULL;
  VarClear(FS); FS:=NULL;
  VarClear(FI); FI:=NULL;
  VarClear(FR); FR:=NULL;
  CreateIndex(False);
End;
{$ENDIF}
//******************************************************************************
procedure Register;
Begin
  RegisterComponents('KA Dao', [TKADaoDatabase]);
End;

Initialization
 {$IFNDEF D5UP}
  TVarData(Unassigned).VType := varEmpty;
  TVarData(EmptyParam).VType := varError;
  TVarData(EmptyParam).VError := $80020004;
 {$ENDIF}
End.


