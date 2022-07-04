{
   Firebird Library
   Open Source Library No Data Aware for direct access to Firebird
   Relational Database from Borland Delphi / Kylix and Freepascal
   
   File:FBLService.pas
   Copyright (c) 2004 Alessandro Batisti
   fblib@altervista.org
   http://fblib.altervista.org

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
}

{
  Note:
  works only with firebird and interbase  1.0.x superserver
  and firebird 1.5.x superserver and partially with classicserver
  in classic server version(1.5)
    property NumOfAttachments    return always 0
    property NumOfDatabases      return always 0
    property DatabaseNames       return always empty list
    property UserNames           return always empty list
    and security functions     (UserNames,ViewUser,AddUser,DeleteUser,ModifyUser)
}

{$I fbl.inc}

{
@abstract(Firebird service manager routines)
@author(Alessandro Batisti <fblib@altervista.org>)
FbLib - Firebird Library @html(<br>)
FBLDService.pas unit provides firebird service manager(backup,restore,security,gfix)
}



unit FBLService;

interface

uses
  Classes, SysUtils, ibase_h;

type
  {Service manager protocol type}
  TServiceProtocolType = (ptLocal, ptTcpIp, ptNetBeui);
  {Backup Options}
  TBackupOption = (bkpVerbose, bkpIgnoreCheckSum, bkpIgnoreLimbo, bkpMetadataOnly,
    bkpNoGarbageCollect, bkpOldDescription, bkpNoTrasportable, bkpConvert);
  TBackupOptions = set of TBackupOption;
  {Restore options}
  TRestoreOption = (resVerbose, resDeactivateIdx, resNoShadow, resNoValidity,
    resOneAtATime, resReplace, resCreate, resUseAllSpace,
    resAccessModeReadOnly, resAccessModeReadWrite);
  TRestoreOptions = set of TRestoreOption;
  {Gstat options}
  TStatOption = (stsDataPages, stsDbLog, stsHdrPages, stsIdxPages, stsSysRelations,
    stsRecordVersions);
  TStatOptions = set of TStatOption;
  {Gfix options}
  TGfixRepair = (gfrCheckDb, gfrIgnore, gfrKill, gfrMend, gfrValidate, gfrFull, gfrSweep);
  TGfixRepairs = set of TGfixRepair;
  {Event procedure occurs when service manager write output messages
   TOnWriteOutput = procedure(Sender: TObject; TextLine: string; IscAction: integer) of object;
  }
  TOnWriteOutput = procedure(Sender: TObject; TextLine: string; IscAction: integer) of
  object; //see ibase_h.pas for value of isc_action_*

  {@abstract(encapsulates properties and methods for managing firebird service manager )}
  TFBLService = class(TComponent)
  private
    FUser: string;
    FPassword: string;
    FProtocol: TServiceProtocolType;
    FHost: string;
    FTcpPort: word;
    FServiceHandle: TISC_SVC_HANDLE;
    FOnWriteOutput: TOnWriteOutput;
    FUserList: TStringList;
    FDatabaseNames: TStringList;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    function GetConnected: boolean;
    function GetUserNames: TStringList;
    function GetVersion: integer;
    function GetInfoString(Aisc_info: integer): string;
    function GetServerVersion: string;
    function GetServerImplementation: string;
    function GetServerPath: string;
    function GetServerLockPath: string;
    function GetServerMsgPath: string;
    function GetUserDBPath: string;
    function GetNumOfAttachments: integer;
    function GetNumOfDatabases: integer;
    function GetDatabaseNames: TStringList;
    procedure AddModifyUser(AIscAction: integer; const AUserName, APassword: string;
      AFirstName: string = ''; AMiddleName: string = '';
      ALastName: string = ''; AUserID: longint = 0; AGroupID: longint = 0);
    procedure WriteLineOutput(const AIscAction: integer);
    procedure CallProc32(const ADatabaseFile: string;
      AiscAction, AiscParam, AValue: longint);
    procedure CallProc8(const ADatabaseFile: string;
      AiscAction, AiscParam, AValue: longint);
  public
    {Create an istance  of a TFBLService}
    constructor Create(AOwner: TComponent); override;
    {Free up  all resources associated with this instance}
    destructor Destroy; override;
    {Connect service manager}
    procedure Connect;
    {Disconect service manager}
    procedure Disconnect;
    {Perform backup operation
    @param(ADatabaseFile : database file source)
    @param(ABackupFile : backup file destination)
    @param(AOption : Backup Options , see @link(TBackupOption))
    @longcode(#
    //Examples...
    // myService is an instance of TFBLService
    // myEvent is an event procedure TOnWriteOutput
    procedure TmyClass.myEvent(Sender: TObject; TextLine: string; IscAction: integer)
    begin
      //iscAction has value ACTION_BACKUP
      WriteLn(TextLine); //write verbose output in console of backup
    end;
    //.......
    myService.OnWriteOutput := myEvent;
    myService.User := 'sysdba';
    myService.Password := 'masterkey';
    myService.Connect;  //connect service manager
    //start backup if not specify [bkpVerbose] in options event onWriteOutput has no effect
    myService.Backup('c:\db\mydb.fdb','c:\db\mydb.gbk',[bkpVerbose])
    //....
    //Backup only metadata
    myService.Backup('c:\db\mydb.fdb','c:\db\mydb-empty.gbk',[bkpVerbose,bkpMetadataOnly])
    myService.Disconnect; //disconnect service manager
    #)}
    procedure Backup(const ADatabaseFile, ABackupFile: string;
      AOption: TBackupOptions = []);
    {Perform restore operation
    @param(ABackupFile : backup file source)
    @param(ADatabaseFile : database file destination)
    @param(AOption : RestoreOptions , see @link(TRestoreOption))
    @html(<br>) see also @link(Backup)}
    procedure Restore(const ABackupFile, ADatabaseFile: string;
      AOption: TRestoreOptions = []; APageSize: integer = 0);
    {Get firebird server log file
     @html(<br>) output is managed by @link(OnWriteOutput)}
    procedure GetLogFile;
    {corresponds to gstat command line
    @html(<br>) output is managed by @link(OnWriteOutput)
    @param(ADatabaseFile : Database file)
    @param(AOption : GStatOptions , see @link(TStatOption))}
    procedure GetStatusReports(const ADatabaseFile: string; AOption: TStatOptions = []);
    {Add an user in security db
    @param(AUserName : user name)
    @param(APassword : Password)
    @param(AFirstName : first name (optional default := ''))
    @param(AMiddleName : middle name (optional default := ''))
    @param(ALastName : last name (optional default := ''))
    @param(AUserId : user id (optional default := 0))
    @param(AGroupId : group id (optional default := 0))}
    procedure AddUser(const AUserName, APassword: string; AFirstName: string = '';
      AMiddleName: string = '';
      ALastName: string = ''; AUserID: longint = 0; AGroupID: longint = 0);
    {Modify information about an user in security db
    @html(<br>) Note : set (AFirstName,AMiddleName,ALastName) to '#' for delete value in these parameters 
    @param(AUserName : user name to modify)
    @param(APassword : Password)
    @param(AFirstName : first name (optional default := ''))
    @param(AMiddleName : middle name (optional default := ''))
    @param(ALastName : last name (optional default := ''))
    @param(AUserId : user id (optional default := 0))
    @param(AGroupId : group id (optional default := 0))}
    procedure ModifyUser(const AUserName, APassword: string;
      AFirstName: string = ''; AMiddleName: string = '';
      ALastName: string = ''; AUserID: longint = 0; AGroupID: longint = 0);
    {delete an user from security db
    @param(AUserName : user name to delete)}
    procedure DeleteUser(const AUserName: string);
    {get information about an  User from security db
     @param(AUserName : user name to view)
     @param(var AFirstName : first name)
     @param(var AMiddleName : middle name )
     @param(var ALastName: last name)
     @param(var AUserId : user id)
     @param(var AGroupId : group id )}
    procedure ViewUser(const AUserName: string;
      var AFirstName, AMiddleName, ALastName: string;
      var AUserID, AGroupId: longint);
    {corresponds to gfix -buffes n}
    procedure GFixSetPageBuffers(const ADatabaseFile: string;
      APageBufferLenght: longint);
    {corresponds to gfix -housekeeping n}
    procedure GFixSetSweepInterval(const ADatabaseFile: string;
      ASweepIntervalLenght: longint);
    {corresponds to gfix -shut -force n}
    procedure GFixSetShutDownDb(const ADatabaseFile: string; ATimeOut: longint);
    {corresponds to gfix -shut -tran n}
    procedure GFixSetShutDownDbTran(const ADatabaseFile: string; ATimeOut: longint);
    {corresponds to gfix -shut -attach n}
    procedure GFixSetShutDownDbAttach(const ADatabaseFile: string; ATimeOut: longint);
    {corresponds gfix -use full}
    procedure GFixSetReserveSpaceFull(const ADatabaseFile: string);
    {corresponds to gfix -use reserve}
    procedure GFixSetReserveSpaceRes(const ADatabaseFile: string);
    {corresponds to gfix -write async}
    procedure GFixSetWriteModeAsync(const ADatabaseFile: string);
    {corresponds to gfix -write sync}
    procedure GFixSetWriteModeSync(const ADatabaseFile: string);
    {corresponds to gfix -mode read_only}
    procedure GFixSetAccessModeReadOnly(const ADatabaseFile: string);
    {corresponds to gfix -mode read_write}
    procedure GFixSetAccessModeReadWrite(const ADatabaseFile: string);
    {corresponds to gfix -sql_dialect n}
    procedure GFixSetSqlDialect(const ADatabaseFile: string; AValue: integer);
    {corresponds gfix repair
    @param(AdatabaseFile : datatbase file)
    @param(AOption: GfixOption , see @link(TGfixRepair))}
    procedure GFixRepair(const ADatabaseFile: string; AOption: TGfixRepairs);
    {True is service manager is connected}
    property Connected: boolean read GetConnected;
    {List of users in Security db}
    property UserNames: TStringList read GetUserNames;
    {Version of the service manager}
    property Version: integer read GetVersion;
    {Version of the database server}
    property ServerVersion: string read GetServerVersion;
    {Implementation of database server ex: 'Firebird/X86/WindowsNT'}
    property ServerImplementation: string read GetServerImplementation;
    {Location of Firebird root directory}
    property ServerPath: string read GetServerPath;
    {Location of Firebird Lock Manager}
    property ServerLockPath: string read GetServerLockPath;
    {Location of Firebird message file}
    property ServerMsgPath: string read GetServerMsgPath;
    {Location of security database on the server}
    property UserDbPath: string read GetUserDbPath;
    {Number of attachments currently in use on the server (only SuperServer)}
    property NumOfAttachments: integer read GetNumOfAttachments;
    {Number of databases currently in use on the server (only SuperServer)}
    property NumOfDatabases: integer read GetNumOfDatabases;
    {List of databases currently in use on the server (only SuperServer)}
    property DatabaseNames: TStringList read GetDatabaseNames;
  published
    {The user ID you use to attach to Servive Manager normally SYSDBA}
    property User: string read FUser write FUser;
    {The password you use to attach to Servive Manager}
    property Password: string read FPassword write FPassword;
    {The protocol you use  to attach to Servive Manager : (ptLocal, ptTcpIp, ptNetBeui)}
    property Protocol: TServiceProtocolType read FProtocol write FProtocol;
    {The remote HostName where run Service Manager}
    property Host: string read FHost write FHost;
    {Tcp Port where service manager is connected (default 3050)}
    property TcpPort: word read FTcpPort write FTcpPort default 3050;
    {Occurs when a service write on output}
    property OnWriteOutput: TOnWriteOutput read FOnWriteOutput write FOnWriteOutput;
    {Occurs after service manager is connected}
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    {Occurs after service manager is disconnected} 
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
  end;

const
  {Value of IscAction in @link(TOnWriteOutput) if operation performed is backup}
  ACTION_BACKUP = isc_action_svc_backup;
  {Value of IscAction in @link(TOnWriteOutput) if operation performed is restore}
  ACTION_RESTORE = isc_action_svc_restore;
  {Value of IscAction in @link(TOnWriteOutput) if operation performed is get log}
  ACTION_GET_LOG = isc_action_svc_get_ib_log;
  {Value of IscAction in @link(TOnWriteOutput) if operation performed is gstat}
  ACTION_GET_STATS = isc_action_svc_db_stats;
  {Value of IscAction in @link(TOnWriteOutput) if operation performed is gfix}
  ACTION_REPAIR = isc_action_svc_repair;

implementation

uses FBLExcept, FBLmixf, FBLConst;

//------------------------------------------------------------------------------

constructor TFBLService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTcpPort := 3050;
  FServiceHandle := nil;
  FUserList := nil;
  FDatabaseNames := nil;
end;

//------------------------------------------------------------------------------

destructor TFBLService.Destroy;
begin
  if Connected then Disconnect;
  if Assigned(FUserList) then FUserList.Free;
  if Assigned(FDatabaseNames) then  FDatabaseNames.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TFBLService.GetConnected: boolean;
begin
  Result := (FServiceHandle <> nil);
end;

//------------------------------------------------------------------------------

procedure TFBLService.CallProc32(const ADatabaseFile: string;
  AiscAction, AiscParam, AValue: longint);
var
  SpbBuffer: PChar;
  SpbIdx: Short;
  LenBuffer: integer;
  Status_vector: ISC_STATUS_VECTOR;
begin
  if FServiceHandle = nil then
    FBLError(E_SM_NO_CON);
  SpbBuffer := nil;
  SpbIdx := 0;
  LenBuffer := 4 + Length(ADatabaseFile) + 5;
  FBLMalloc(SpbBuffer, LenBuffer);
  try
    SpbBuffer[SpbIdx] := char(AiscAction);
    Inc(SpbIdx);
    //databasefile
    SpbBuffer[SpbIdx] := char(isc_spb_dbname);
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Length(ADatabaseFile));
    Inc(spbidx);
    SpbBuffer[SpbIdx] := char(Length(ADatabaseFile) shr 8);
    Inc(SpbIdx);
    Move(ADatabaseFile[1], SpbBuffer[spbidx], Length(ADatabaseFile));
    Inc(SpbIdx, Length(ADatabaseFile));
    SpbBuffer[SpbIdx] := char(AiscParam);
    Inc(spbidx);
    SpbBuffer[SpbIdx] := char(AValue);
    Inc(spbidx);
    SpbBuffer[SpbIdx] := char(AValue shr 8);
    Inc(spbidx);
    SpbBuffer[SpbIdx] := char(AValue shr 16);
    Inc(spbidx);
    SpbBuffer[SpbIdx] := char(AValue shr 24);
    Inc(SpbIdx);
    // start service
    if isc_service_start(@Status_vector, @FServiceHandle, nil, SpbIdx,
      SpbBuffer) <> 0 then
      FBLShowError(@Status_vector);
  finally
    FBLFree(SpbBuffer);
  end;
end;

//------------------------------------------------------------------------------


procedure TFBLService.CallProc8(const ADatabaseFile: string;
  AiscAction, AiscParam, AValue: longint);
var
  SpbBuffer: PChar;
  SpbIdx: Short;
  LenBuffer: integer;
  Status_vector: ISC_STATUS_VECTOR;
begin
  if FServiceHandle = nil then
    FBLError(E_SM_NO_CON);
  SpbBuffer := nil;
  SpbIdx := 0;
  LenBuffer := 4 + Length(ADatabaseFile) + 2;
  FBLMalloc(SpbBuffer, LenBuffer);
  try
    SpbBuffer[SpbIdx] := char(AiscAction);        //isc_action_svc_properties
    Inc(SpbIdx);
    //databasefile
    SpbBuffer[SpbIdx] := char(isc_spb_dbname);
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Length(ADatabaseFile));
    Inc(spbidx);
    SpbBuffer[SpbIdx] := char(Length(ADatabaseFile) shr 8);
    Inc(SpbIdx);
    Move(ADatabaseFile[1], SpbBuffer[spbidx], Length(ADatabaseFile));
    Inc(SpbIdx, Length(ADatabaseFile));
    SpbBuffer[SpbIdx] := char(AiscParam);
    Inc(spbidx);
    SpbBuffer[SpbIdx] := char(AValue);
    Inc(spbidx);
    // start service
    if isc_service_start(@Status_vector, @FServiceHandle, nil, SpbIdx,
      SpbBuffer) <> 0 then
      FBLShowError(@Status_vector);
  finally
    FBLFree(SpbBuffer);
  end;
end;

//------------------------------------------------------------------------------

procedure TFBLService.WriteLineOutput(const AIscAction: integer);
var
  RequestInfo: char;
  BufferResult: array[0..511] of char;
  Status_vector: ISC_STATUS_VECTOR;
  LenLine: integer;
  Line: string;
begin
  RequestInfo := char(isc_info_svc_line);
  LenLine := 0;
  repeat
    if isc_service_query(@Status_vector, @FServiceHandle, nil, 0,nil,
      1, @RequestInfo, SizeOf(BufferResult),
      BufferResult) <> 0 then
      FBLShowError(@Status_vector);
    if BufferResult[0] = char(isc_info_svc_line) then
    begin
      LenLine := isc_vax_integer(@BufferResult[1], 2);
      SetLength(Line, LenLine);
      Move(BufferResult[3], Line[1], LenLine);
      if Assigned(FOnWriteOutput) then
        FOnWriteOutput(self, Line, AIscAction);
    end;
  until LenLine = 0
  end;

//------------------------------------------------------------------------------

procedure TFBLService.Connect;
var
  SpbBuffer: PChar;
  SpbIdx: Short;
  Status_vector: ISC_STATUS_VECTOR;
  ServiceName: string;
  LenBuffer: integer;
begin
  CheckFbClientLoaded;
  if FServiceHandle <> nil then
    FBLError(E_SM_ALREADY_CON);
  case FProtocol of
    ptTcpIp:
      begin
        if FTcpPort <> 3050 then
          ServiceName := Format('%s/%d:service_mgr', [FHost, FTcpPort])
        else
          ServiceName := Format('%s:service_mgr', [FHost]);
      end;
    ptNetBeui: ServiceName := '\\' + FHost + '\' + 'service_mgr';
    else  
      ServiceName := 'service_mgr';
  end;
  SpbBuffer := nil;
  LenBuffer := 4 + Length(FUser) + 2 + Length(FPassword);
  FBLMalloc(SpbBuffer, LenBuffer);
  SpbIdx := 0;
  try
    SpbBuffer[SpbIdx] := char(isc_spb_version);
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(isc_spb_current_version);
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(isc_spb_user_name);
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Length(FUser));
    Inc(SpbIdx);
    Move(FUser[1], SpbBuffer[SpbIdx], Length(FUser));
    Inc(SpbIdx, Length(FUser));
    SpbBuffer[SpbIdx] := char(isc_spb_password);
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Length(FPassword));
    Inc(SpbIdx);
    Move(FPassword[1], SpbBuffer[SpbIdx], Length(FPassword));
    Inc(SpbIdx, Length(FPassword));
    if isc_service_attach(@Status_vector, 0,PChar(ServiceName), @FServiceHandle,
      SpbIdx, SpbBuffer) <> 0 then
      FBLShowError(@Status_vector);
    if Assigned(FOnConnect) then
      FOnConnect(self);
  finally
    FBLFree(SpbBuffer);
  end;  
end;

//------------------------------------------------------------------------------

procedure TFBLService.Disconnect;
var
  Status_vector: ISC_STATUS_VECTOR;
begin
  if FServiceHandle = nil then
    FBLError(E_SM_NO_CON);
  if isc_service_detach(@Status_vector, @FServiceHandle) <> 0 then
    FBLShowError(@Status_vector);
  if Assigned(FOnDisconnect) then
    FOnDisconnect(self);
end;

//------------------------------------------------------------------------------

procedure TFBLService.Backup(const ADatabaseFile, ABackupFile: string;
  AOption: TBackupOptions = []);
var
  SpbBuffer: PChar;
  SpbIdx: Short;
  LenBuffer: integer;
  Status_vector: ISC_STATUS_VECTOR;
  Options: longint;   //bitmask of isc_spb_options
begin
  if FServiceHandle = nil then
    FBLError(E_SM_NO_CON);
  Options := 0;
  SpbBuffer := nil;
  SpbIdx := 0;
  { LenBuffer
    1 byte : isc_action_svc_backup
    1 byte : isc_spb_dbname
    2 byte : lenght DatabaseFile
    n byte : Databasefile name
    1 byte : isc_spb_bkp_file
    2 byte : lenght backupfile
    n byte : backupfile name
    1 byte : isc_spb_options
    4 byte : Options bitmask
    + 1 byte if verbose request}
  LenBuffer := 4 + Length(ADatabaseFile) + 3 + Length(ABackupFile) + 5;
  if bkpVerbose in AOption then
    Inc(LenBuffer);
  FBLMalloc(SpbBuffer, LenBuffer);
  try
    SpbBuffer[SpbIdx] := char(isc_action_svc_backup);
    Inc(SpbIdx);
    //databasefile
    SpbBuffer[SpbIdx] := char(isc_spb_dbname);
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Length(ADatabaseFile));
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Length(ADatabaseFile) shr 8);
    Inc(SpbIdx);
    Move(ADatabaseFile[1], SpbBuffer[SpbIdx], Length(ADatabaseFile));
    Inc(SpbIdx, Length(ADatabaseFile));
    // verbose result
    if bkpVerbose in AOption then
    begin
      SpbBuffer[SpbIdx] := char(isc_spb_verbose);
      Inc(SpbIdx);
    end;
    //backupfile
    SpbBuffer[SpbIdx] := char(isc_spb_bkp_file);
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Length(ABackupFile));
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Length(ABackupFile) shr 8);
    Inc(SpbIdx);
    Move(ABackupFile[1], SpbBuffer[SpbIdx], Length(ABackupFile));
    Inc(SpbIdx, Length(ABackupFile));
    //options backup
    if bkpIgnoreCheckSum in AOption then
      Options := Options or isc_spb_bkp_ignore_checksums;
    if bkpIgnoreLimbo in AOption then
      Options := Options or isc_spb_bkp_ignore_limbo;
    if bkpMetadataOnly in AOption then
      Options := Options or isc_spb_bkp_metadata_only;
    if bkpNoGarbageCollect in AOption then
      Options := Options or isc_spb_bkp_no_garbage_collect;
    if bkpOldDescription in AOption then
      Options := Options or isc_spb_bkp_old_descriptions;
    if bkpConvert in AOption then
      Options := Options or isc_spb_bkp_convert;
    SpbBuffer[SpbIdx] := char(isc_spb_options);
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Options);
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Options shr 8);
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Options shr 16);
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Options shr 24);
    Inc(SpbIdx);
    // start backup
    if isc_service_start(@Status_vector, @FServiceHandle, nil, SpbIdx,
      SpbBuffer) <> 0 then
      FBLShowError(@Status_vector);
  finally
    FBLFree(SpbBuffer);
  end;
  // if Verbose print msg
  if bkpVerbose in AOption then
    WriteLineOutput(isc_action_svc_backup);
end;

//------------------------------------------------------------------------------

procedure TFBLService.Restore(const ABackupFile, ADatabaseFile: string;
  AOption: TRestoreOptions = []; APageSize: integer = 0);
var
  SpbBuffer: PChar;
  SpbIdx: Short;
  LenBuffer: integer;
  Status_vector: ISC_STATUS_VECTOR;
  Options: longint;
begin
  if FServiceHandle = nil then
    FBLError(E_SM_NO_CON);
  if not ((resCreate in AOption) or (resReplace in AOption)) then
    FBLError(E_SM_RES_NO_ACTION);
  SpbBuffer := nil;
  Options := 0;
  SpbIdx := 0;
  { LenBuffer
    1 byte : isc_action_svc_restore
    1 byte : isc_spb_bkp_file
    2 byte : lenght BackupFile
    n byte : Backup name
    1 byte : isc_spb_bkp_dbname
    2 byte : lenght DatabaseFile
    n byte : DatabaseFile name
    1 byte : isc_spb_options
    4 byte : Options bitmask
    + 1 byte if verbose request
    + 1 byte if access mode
    + 5 byte if Pagesize > 0}
  LenBuffer := 4 + Length(ABackupFile) + 3 + Length(ADatabaseFile) + 5;
  if (resVerbose in Aoption) then
    Inc(LenBuffer);
  if (resAccessModeReadOnly in AOption) or (resAccessModeReadWrite in AOption) then
    Inc(LenBuffer);
  if APageSize > 0 then
    Inc(LenBuffer, 5);
  FBLMalloc(spbBuffer, LenBuffer);
  try
    SpbBuffer[SpbIdx] := char(isc_action_svc_restore);
    Inc(SpbIdx);
    //backupfile
    SpbBuffer[SpbIdx] := char(isc_spb_bkp_file);
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Length(ABackupFile));
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Length(ABackupFile) shr 8);
    Inc(SpbIdx);
    Move(ABackupFile[1], SpbBuffer[SpbIdx], Length(ABackupFile));
    Inc(SpbIdx, Length(ABackupFile));
    // verbose result
    if resVerbose in AOption then
    begin
      SpbBuffer[SpbIdx] := char(isc_spb_verbose);
      Inc(SpbIdx);
    end;
    //databasefile
    SpbBuffer[SpbIdx] := char(isc_spb_dbname);
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Length(ADatabaseFile));
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Length(ADatabaseFile) shr 8);
    Inc(SpbIdx);
    Move(ADatabaseFile[1], SpbBuffer[SpbIdx], Length(ADatabaseFile));
    Inc(SpbIdx, Length(ADatabaseFile));
    //options
    if resDeactivateIdx in AOption then
      Options := Options or isc_spb_res_deactivate_idx;
    if resNoShadow in AOption then
      Options := Options or isc_spb_res_no_shadow;
    if resNoValidity in AOption then
      Options := Options or isc_spb_res_no_validity;
    if resOneAtATime in AOption then
      Options := Options or isc_spb_res_one_at_a_time;
    if resReplace in AOption then
      Options := Options or isc_spb_res_replace;
    if resCreate in AOption then
      Options := Options or isc_spb_res_create;
    if resUseAllSpace in AOption then
      Options := Options or isc_spb_res_use_all_space;
    SpbBuffer[SpbIdx] := char(isc_spb_options);
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Options);
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Options shr 8);
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Options shr 16);
    Inc(SpbIdx);
    SpbBuffer[SpbIdx] := char(Options shr 24);
    Inc(SpbIdx);
    if resAccessModeReadOnly in AOption then
    begin
      if resAccessModeReadWrite in AOption then
        FBLError(E_SM_RES_PARAMS_ACCESSMODE);
      SpbBuffer[SpbIdx] := char(isc_spb_res_access_mode);
      Inc(SpbIdx);
      SpbBuffer[SpbIdx] := char(isc_spb_res_am_readonly);
    end;
    if resAccessModeReadWrite in AOption then
    begin
      if resAccessModeReadOnly in AOption then
        FBLError(E_SM_RES_PARAMS_ACCESSMODE);
      SpbBuffer[SpbIdx] := char(isc_spb_res_access_mode);
      Inc(SpbIdx);
      SpbBuffer[SpbIdx] := char(isc_spb_res_am_readwrite);
    end;
    // Page Size
    if APageSize <> 0 then
    begin
      SpbBuffer[SpbIdx] := char(isc_spb_res_page_size);
      Inc(SpbIdx);
      SpbBuffer[SpbIdx] := char(APageSize);
      Inc(SpbIdx);
      SpbBuffer[SpbIdx] := char(APageSize shr 8);
      Inc(SpbIdx);
      SpbBuffer[SpbIdx] := char(APageSize shr 16);
      Inc(SpbIdx);
      SpbBuffer[SpbIdx] := char(APageSize shr 24);
      Inc(SpbIdx);
    end;
    // start restore
    if isc_service_start(@Status_vector, @FServiceHandle, nil, SpbIdx,
      SpbBuffer) <> 0 then
      FBLShowError(@Status_vector);
  finally
    FBLFree(SpbBuffer);
  end;
  if resVerbose in AOption then
    WriteLineOutput(isc_action_svc_restore);
end;

//------------------------------------------------------------------------------

procedure TFBLService.GetLogFile;
var
  RequestInfo: char;
  Status_vector: ISC_STATUS_VECTOR;
begin
  if FServiceHandle = nil then
    FBLError(E_SM_NO_CON);
  RequestInfo := char(isc_action_svc_get_ib_log);
  if isc_service_start(@Status_vector, @FServiceHandle, nil, 1, @RequestInfo) <> 0 then
    FBLShowError(@Status_vector);
  WriteLineOutput(isc_action_svc_get_ib_log);
end;

//------------------------------------------------------------------------------

procedure TFBLService.GetStatusReports(const ADatabaseFile: string;
  AOption: TStatOptions = []);
var
  Options: longint;
begin
  Options := 0;
  if stsDataPages in AOption then
    Options := Options or isc_spb_sts_data_pages;
  if stsDbLog in AOption then
    Options := Options or isc_spb_sts_db_log;
  if stsHdrPages in AOption then
    Options := Options or isc_spb_sts_hdr_pages;
  if stsIdxPages in AOption then
    Options := Options or isc_spb_sts_idx_pages;
  if stsSysRelations in AOption then
    Options := Options or isc_spb_sts_sys_relations;
  if stsRecordVersions in AOption then
    Options := Options or isc_spb_sts_record_versions;
  CallProc32(ADatabaseFile, isc_action_svc_db_stats, isc_spb_options, Options);
  WriteLineOutput(isc_action_svc_db_stats);
end;

//------------------------------------------------------------------------------

function TFBLService.GetUserNames: TStringList;
var
  RequestInfo: char;
  BufferResult: array[0..255] of char; //tempBuffer
  Buffer: PChar;                       //Full Buffer
  Status_vector: ISC_STATUS_VECTOR;
  User: string;
  Idx, TotBuffer, PosBuffer, LenResult: integer;
  BufferError: boolean;
begin
  if FServiceHandle = nil then
    FBLError(E_SM_NO_CON);
  if FUserList = nil then FUserList := TStringList.Create;
  Result := FUserList;
  FUserList.Clear;
  RequestInfo := char(isc_action_svc_display_user);
  if isc_service_start(@Status_vector, @FServiceHandle, nil, 1, @RequestInfo) <> 0 then
    FBLShowError(@Status_vector);
  RequestInfo := char(isc_info_svc_get_users);
  Buffer := nil;
  TotBuffer := 0;
  PosBuffer := 0;
  BufferError := False;
  try
    repeat
      if isc_service_query(@Status_vector, @FServiceHandle, nil, 0,nil,
        1, @RequestInfo, SizeOf(BufferResult),
        BufferResult) <> 0 then
        FBLShowError(@Status_vector);
      if BufferResult[0] <> char(isc_info_svc_get_users) then
        Exit;              // function not supported
      LenResult := isc_vax_integer(@BufferResult[1], 2);
      if LenResult = 0 then
      begin
        Inc(TotBuffer, 1);
        ReallocMem(Buffer, TotBuffer);
        Buffer[TotBuffer - 1] := char(isc_info_end);
      end
      else
      begin
        Inc(TotBuffer, LenResult);
        ReallocMem(Buffer, TotBuffer);
        Move(BufferResult[3], Buffer[PosBuffer], LenResult);
        Inc(PosBuffer, LenResult);
      end;
    until LenResult = 0;
    idx := 0;

    while (Buffer[idx] <> char(isc_info_end)) and (not BufferError) do
    begin
      case integer(Buffer[idx]) of
        isc_spb_sec_userid:
          Inc(idx, 5);
        isc_spb_sec_groupid:
          Inc(idx, 5);
        isc_spb_sec_username:
          begin
            Inc(idx);
            LenResult := isc_vax_integer(@Buffer[Idx], 2);
            Inc(idx, 2);
            SetLength(User, LenResult);
            Move(Buffer[idx], User[1], LenResult);
            FUserList.Add(User);
            Inc(idx, LenResult);
          end;
        isc_spb_sec_groupname:
          begin
            Inc(idx);
            LenResult := isc_vax_integer(@Buffer[Idx], 2);
            Inc(idx, 2);
            Inc(idx, LenResult);
          end;
        isc_spb_sec_firstname:
          begin
            Inc(idx);
            LenResult := isc_vax_integer(@Buffer[Idx], 2);
            Inc(idx, 2);
            Inc(idx, LenResult);
          end;
        isc_spb_sec_middlename:
          begin
            Inc(idx);
            LenResult := isc_vax_integer(@Buffer[Idx], 2);
            Inc(idx, 2);
            Inc(idx, LenResult);
          end;
        isc_spb_sec_lastname:
          begin
            Inc(idx);
            LenResult := isc_vax_integer(@Buffer[Idx], 2);
            Inc(idx, 2);
            Inc(idx, LenResult);
          end;
        else
          BufferError := True;
      end;           //end case
    end;             // end while
    if BufferError then
      FBLError(E_SM_NOT_FUNC_SUPPORT);
  finally
    ReallocMem(Buffer, 0);
  end;
end;

//------------------------------------------------------------------------------

procedure TFBLService.AddModifyUser(AIscAction: integer;
  const AUserName, APassword: string; AFirstName: string = ''; AMiddleName: string = '';
  ALastName: string = ''; AUserID: longint = 0; AGroupID: longint = 0);
var
  SpbBuffer: array [0..254] of char;
  Status_vector: ISC_STATUS_VECTOR;
  SpbBufferIdx: Short;
  UserName, Password, FirstName, LastName, MiddleName: string;
begin
  if FServiceHandle = nil then
    FBLError(E_SM_NO_CON);
  if Length(AUserName) > 31 then
    UserName := Copy(AUserName, 0,31)
  else
    UserName := AUserName;
  if Length(APassword) > 8 then
    Password := Copy(APassword, 0,8)
  else
    Password := APassword;
  if Length(AFirstName) > 17 then
    FirstName := Copy(AFirstName, 0,17)
  else
    FirstName := AFirstName;
  if Length(AMiddleName) > 17 then
    MiddleName := Copy(AMiddleName, 0,17)
  else
    MiddleName := AMiddleName;
  if Length(ALastName) > 17 then
    LastName := Copy(ALastName, 0,17)
  else
    LastName := ALastName;
  SpbBufferIdx := 0;
  SpbBuffer[SpbBufferIdx] := char(AIscAction);
  Inc(SpbBufferIdx);
  //UserName
  SpbBuffer[SpbBufferIdx] := char(isc_spb_sec_username);
  Inc(SpbBufferIdx);
  SpbBuffer[SpbBufferIdx] := char(Length(UserName));
  Inc(SpbBufferIdx);
  SpbBuffer[SpbBufferIdx] := char(Length(UserName) shr 8);
  Inc(SpbBufferIdx);
  Move(UserName[1], SpbBuffer[SpbBufferIdx], Length(UserName));
  Inc(SpbBufferIdx, Length(UserName));
  //Password
  SpbBuffer[SpbBufferIdx] := char(isc_spb_sec_password);
  Inc(SpbBufferIdx);
  SpbBuffer[SpbBufferIdx] := char(Length(Password));
  Inc(SpbBufferIdx);
  SpbBuffer[SpbBufferIdx] := char(Length(Password) shr 8);
  Inc(SpbBufferIdx);
  Move(Password[1], SpbBuffer[SpbBufferIdx], Length(Password));
  Inc(SpbBufferIdx, Length(Password));
  //FirstName
  if FirstName <> '' then
  begin
    if (FirstName = '#') and (AIscAction = isc_action_svc_modify_user) then
    begin
      SpbBuffer[SpbBufferIdx] := char(isc_spb_sec_firstname);
      Inc(SpbBufferIdx);
      SpbBuffer[SpbBufferIdx] := char(0);
      Inc(SpbBufferIdx);
      SpbBuffer[SpbBufferIdx] := char(0);
      Inc(SpbBufferIdx);
    end
    else
    begin
      SpbBuffer[SpbBufferIdx] := char(isc_spb_sec_firstname);
      Inc(SpbBufferIdx);
      SpbBuffer[SpbBufferIdx] := char(Length(FirstName));
      Inc(SpbBufferIdx);
      SpbBuffer[SpbBufferIdx] := char(Length(FirstName) shr 8);
      Inc(SpbBufferIdx);
      Move(FirstName[1], SpbBuffer[SpbBufferIdx], Length(FirstName));
      Inc(SpbBufferIdx, Length(FirstName));
    end;
  end;
  //MiddleName
  if MiddleName <> '' then
  begin
    if (MiddleName = '#') and (AIscAction = isc_action_svc_modify_user) then
    begin
      SpbBuffer[SpbBufferIdx] := char(isc_spb_sec_middlename);
      Inc(SpbBufferIdx);
      SpbBuffer[SpbBufferIdx] := char(0);
      Inc(SpbBufferIdx);
      SpbBuffer[SpbBufferIdx] := char(0);
      Inc(SpbBufferIdx);
    end
    else
    begin
      SpbBuffer[SpbBufferIdx] := char(isc_spb_sec_middlename);
      Inc(SpbBufferIdx);
      SpbBuffer[SpbBufferIdx] := char(Length(MiddleName));
      Inc(SpbBufferIdx);
      SpbBuffer[SpbBufferIdx] := char(Length(MiddleName) shr 8);
      Inc(SpbBufferIdx);
      Move(MiddleName[1], SpbBuffer[SpbBufferIdx], Length(MiddleName));
      Inc(SpbBufferIdx, Length(MiddleName));
    end;
  end;

  //LastName
  if LastName <> '' then
  begin
    if (LastName = '#') and (AIscAction = isc_action_svc_modify_user) then
    begin
      SpbBuffer[SpbBufferIdx] := char(isc_spb_sec_lastname);
      Inc(SpbBufferIdx);
      SpbBuffer[SpbBufferIdx] := char(0);
      Inc(SpbBufferIdx);
      SpbBuffer[SpbBufferIdx] := char(0);
      Inc(SpbBufferIdx);
    end
    else
    begin
      SpbBuffer[SpbBufferIdx] := char(isc_spb_sec_lastname);
      Inc(SpbBufferIdx);
      SpbBuffer[SpbBufferIdx] := char(Length(LastName));
      Inc(SpbBufferIdx);
      SpbBuffer[SpbBufferIdx] := char(Length(LastName) shr 8);
      Inc(SpbBufferIdx);
      Move(LastName[1], SpbBuffer[SpbBufferIdx], Length(LastName));
      Inc(SpbBufferIdx, Length(LastName));
    end;
  end;
  
  //UserId
  if AUserId > 0 then
  begin
    SpbBuffer[SpbBufferIdx] := char(isc_spb_sec_userid);
    Inc(SpbBufferIdx);
    SpbBuffer[SpbBufferIdx] := char(AUserId);
    Inc(SpbBufferIdx);
    SpbBuffer[SpbBufferIdx] := char(AUserId shr 8);
    Inc(SpbBufferIdx);
    SpbBuffer[SpbBufferIdx] := char(AUserId shr 16);
    Inc(SpbBufferIdx);
    SpbBuffer[SpbBufferIdx] := char(AUserId shr 24);
    Inc(SpbBufferIdx);
  end;
  //GroupId
  if AGroupID > 0 then
  begin
    SpbBuffer[SpbBufferIdx] := char(isc_spb_sec_groupid);
    Inc(SpbBufferIdx);
    SpbBuffer[SpbBufferIdx] := char(AGroupID);
    Inc(SpbBufferIdx);
    SpbBuffer[SpbBufferIdx] := char(AGroupID shr 8);
    Inc(SpbBufferIdx);
    SpbBuffer[SpbBufferIdx] := char(AGroupID shr 16);
    Inc(SpbBufferIdx);
    SpbBuffer[SpbBufferIdx] := char(AGroupID shr 24);
    Inc(SpbBufferIdx);
  end;
  if isc_service_start(@Status_vector, @FServiceHandle, nil, SpbBufferIdx,
    SpbBuffer) <> 0 then
    FBLShowError(@Status_vector);
end;

//------------------------------------------------------------------------------

procedure TFBLService.AddUser(const AUserName, APassword: string;
  AFirstName: string = ''; AMiddleName: string = '';
  ALastName: string = ''; AUserID: longint = 0; AGroupID: longint = 0);
begin
  AddModifyUser(isc_action_svc_add_user, AUserName, APassword, AFirstName,
    AMiddleName, ALastName, AUserID, AGroupID);
end;

//------------------------------------------------------------------------------

procedure TFBLService.ModifyUser(const AUserName, APassword: string;
  AFirstName: string = ''; AMiddleName: string = '';
  ALastName: string = ''; AUserID: longint = 0; AGroupID: longint = 0);
begin
  AddModifyUser(isc_action_svc_modify_user, AUserName, APassword, AFirstName,
    AMiddleName, ALastName, AUserID, AGroupID);
end;

//------------------------------------------------------------------------------

procedure TFBLService.DeleteUser(const AUserName: string);
var
  SpbBuffer: array [0..127] of char;
  Status_vector: ISC_STATUS_VECTOR;
  SpbBufferIdx: Short;
  UserName: string;
begin
  if FServiceHandle = nil then
    FBLError(E_SM_NO_CON);
  if Length(AUserName) > 31 then
    UserName := Copy(AUserName, 0,31)
  else
    UserName := AUserName;
  SpbBufferIdx := 0;
  SpbBuffer[SpbBufferIdx] := char(isc_action_svc_delete_user);
  Inc(SpbBufferIdx);
  //UserName
  SpbBuffer[SpbBufferIdx] := char(isc_spb_sec_username);
  Inc(SpbBufferIdx);
  SpbBuffer[SpbBufferIdx] := char(Length(UserName));
  Inc(SpbBufferIdx);
  SpbBuffer[SpbBufferIdx] := char(Length(UserName) shr 8);
  Inc(SpbBufferIdx);
  Move(UserName[1], SpbBuffer[SpbBufferIdx], Length(UserName));
  Inc(SpbBufferIdx, Length(UserName));
  if isc_service_start(@Status_vector, @FServiceHandle, nil, SpbBufferIdx,
    SpbBuffer) <> 0 then
    FBLShowError(@Status_vector);
end;

//------------------------------------------------------------------------------

procedure TFBLService.ViewUser(const AUserName: string;
  var AFirstName, AMiddleName, ALastName: string;
  var AUserID, AGroupId: longint);
var
  SpbBuffer: array [0..254] of char;
  BufferResult: array[0..254] of char;
  Status_vector: ISC_STATUS_VECTOR;
  SpbBufferIdx: Short;
  LenData: integer;
  UserName: string;
  BufferError: boolean;
begin
  if FServiceHandle = nil then
    FBLError(E_SM_NO_CON);
  BufferError := False;
  if Length(AUserName) > 31 then
    UserName := Copy(AUserName, 0,31)
  else
    UserName := AUserName;
  SpbBufferIdx := 0;
  SpbBuffer[SpbBufferIdx] := char(isc_action_svc_display_user);
  Inc(SpbBufferIdx);
  SpbBuffer[SpbBufferIdx] := char(isc_spb_sec_username);
  Inc(SpbBufferIdx);
  SpbBuffer[SpbBufferIdx] := char(Length(UserName));
  Inc(SpbBufferIdx);
  SpbBuffer[SpbBufferIdx] := char(Length(UserName) shr 8);
  Inc(SpbBufferIdx);
  Move(UserName[1], SpbBuffer[SpbBufferIdx], Length(UserName));
  Inc(SpbBufferIdx, Length(UserName));
  if isc_service_start(@Status_vector, @FServiceHandle, nil, SpbBufferIdx,
    SpbBuffer) <> 0 then
    FBLShowError(@Status_vector);
  SpbBuffer[0] := char(isc_info_svc_get_users);

  if isc_service_query(@Status_vector, @FServiceHandle, nil, 0,nil,
    1,SpbBuffer, SizeOf(BufferResult),
    BufferResult) <> 0 then
    FBLShowError(@Status_vector);
  SpbBufferIdx := 3;
  if BufferResult[0] = char(isc_info_svc_get_users) then
  begin
    if BufferResult[3] = char(isc_info_end) then
      FBLError(E_SM_USER_NOT_EXIST, [UserName]);
    //LenBuffer := isc_vax_integer(@BufferResult[1],2);
    while (BufferResult[SpbBufferIdx] <> char(isc_info_end)) and (not BufferError) do
    begin
      case integer(BufferResult[SpbBufferIdx]) of
        isc_spb_sec_userid:
          begin
            Inc(SpbBufferIdx);
            AUserID := isc_vax_integer(@BufferResult[SpbBufferIdx], 4);
            Inc(SpbBufferIdx, 4);
          end;
        isc_spb_sec_groupid:
          begin
            Inc(SpbBufferIdx);
            AGroupID := isc_vax_integer(@BufferResult[SpbBufferIdx], 4);
            Inc(SpbBufferIdx, 4);
          end;
        isc_spb_sec_username:
          begin;
            Inc(SpbBufferIdx);
            LenData := isc_vax_integer(@BufferResult[SpbBufferIdx], 2);
            Inc(SpbBufferIdx, 2 + LenData);
          end;
        isc_spb_sec_groupname:
          begin;
            Inc(SpbBufferIdx);
            LenData := isc_vax_integer(@BufferResult[SpbBufferIdx], 2);
            Inc(SpbBufferIdx, 2 + LenData);
          end;
        isc_spb_sec_firstname:
          begin;
            Inc(SpbBufferIdx);
            LenData := isc_vax_integer(@BufferResult[SpbBufferIdx], 2);
            Inc(SpbBufferIdx, 2);
            SetLength(AFirstName, LenData);
            Move(BufferResult[SpbBufferIdx], AFirstName[1], LenData);
            Inc(SpbBufferIdx, LenData);
          end;
        isc_spb_sec_middlename:
          begin;
            Inc(SpbBufferIdx);
            LenData := isc_vax_integer(@BufferResult[SpbBufferIdx], 2);
            Inc(SpbBufferIdx, 2);
            SetLength(AMiddleName, LenData);
            Move(BufferResult[SpbBufferIdx], AMiddleName[1], LenData);
            Inc(SpbBufferIdx, LenData);
          end;
        isc_spb_sec_lastname:
          begin;
            Inc(SpbBufferIdx);
            LenData := isc_vax_integer(@BufferResult[SpbBufferIdx], 2);
            Inc(SpbBufferIdx, 2);
            SetLength(ALastName, LenData);
            Move(BufferResult[SpbBufferIdx], ALastName[1], LenData);
            Inc(SpbBufferIdx, LenData);
          end;
        else
          BufferError := True;
      end;                   // end case
    end;                     // end while
    if BufferError then
      FBLError(E_SM_NOT_FUNC_SUPPORT);
  end;
end;

//------------------------------------------------------------------------------

function TFBLService.GetVersion: integer;
var
  RequestInfo: char;
  BufferResult: array[0..31] of char;
  Status_vector: ISC_STATUS_VECTOR;
begin
  Result := 0;
  if FServiceHandle = nil then
    FBLError(E_SM_NO_CON);
  RequestInfo := char(isc_info_svc_version);
  if isc_service_query(@Status_vector, @FServiceHandle, nil, 0,nil,
    1, @RequestInfo, SizeOf(BufferResult),
    BufferResult) <> 0 then
    FBLShowError(@Status_vector);
  if BufferResult[0] = char(isc_info_svc_version) then
    Result := isc_vax_integer(@BufferResult[1], 4);
end;

//------------------------------------------------------------------------------

function TFBLService.GetInfoString(Aisc_info: integer): string;
var
  RequestInfo: char;
  BufferResult: array[0..1023] of char;
  Status_vector: ISC_STATUS_VECTOR;
  StringLength: integer;
begin
  Result := '';
  if FServiceHandle = nil then
    FBLError(E_SM_NO_CON);
  RequestInfo := char(Aisc_info);
  if isc_service_query(@Status_vector, @FServiceHandle, nil, 0,nil,
    1, @RequestInfo, SizeOf(BufferResult),
    BufferResult) <> 0 then
    FBLShowError(@Status_vector);
  if BufferResult[0] = char(Aisc_info) then
  begin
    StringLength := isc_vax_integer(@BufferResult[1], 2);
    SetLength(Result, StringLength);
    Move(BufferResult[3], Result[1], StringLength);
  end;
end;

//------------------------------------------------------------------------------

function TFBLService.GetServerVersion: string;
begin
  Result := GetInfoString(isc_info_svc_server_version);
end;

//------------------------------------------------------------------------------

function TFBLService.GetServerImplementation: string;
begin
  Result := GetInfoString(isc_info_svc_implementation);
end;

//------------------------------------------------------------------------------

function TFBLService.GetServerPath: string;
begin
  Result := GetInfoString(isc_info_svc_get_env);
end;

//------------------------------------------------------------------------------

function TFBLService.GetServerLockPath: string;
begin
  Result := GetInfoString(isc_info_svc_get_env_lock);
end;

//------------------------------------------------------------------------------

function TFBLService.GetServerMsgPath: string;
begin
  Result := GetInfoString(isc_info_svc_get_env_msg);
end;

//------------------------------------------------------------------------------

function TFBLService.GetUserDBPath: string;
begin
  Result := GetInfoString(isc_info_svc_user_dbpath);
end;

//------------------------------------------------------------------------------

function TFBLService.GetNumOfAttachments: integer;
var
  RequestInfo: char;
  BufferResult: array [0..127] of char;
  Status_vector: ISC_STATUS_VECTOR;
begin
  Result := 0;
  if FServiceHandle = nil then
    FBLError(E_SM_NO_CON);
  RequestInfo := char(isc_info_svc_svr_db_info);
  if isc_service_query(@Status_vector, @FServiceHandle, nil, 0,nil,
    1, @RequestInfo, SizeOf(BufferResult),
    BufferResult) <> 0 then
    FBLShowError(@Status_vector);
  if BufferResult[0] = char(isc_info_svc_svr_db_info) then
  begin
    if BufferResult[1] = char(isc_spb_num_att) then
      Result := isc_vax_integer(@BufferResult[2], 4);
  end;
end;

//------------------------------------------------------------------------------

function TFBLService.GetNumOfDatabases: integer;
var
  RequestInfo: char;
  BufferResult: array [0..127] of char;
  Status_vector: ISC_STATUS_VECTOR;
begin
  Result := 0;
  if FServiceHandle = nil then
    FBLError(E_SM_NO_CON);
  RequestInfo := char(isc_info_svc_svr_db_info);
  if isc_service_query(@Status_vector, @FServiceHandle, nil, 0,nil,
    1, @RequestInfo, SizeOf(BufferResult),
    BufferResult) <> 0 then
    FBLShowError(@Status_vector);
  if BufferResult[0] = char(isc_info_svc_svr_db_info) then
  begin
    if BufferResult[6] = char(isc_spb_num_db) then
      Result := isc_vax_integer(@BufferResult[7], 4);
  end;
end;

//------------------------------------------------------------------------------


function TFBLService.GetDatabaseNames: TStringList;
var
  RequestInfo: char;
  BufferResult: array[0..32767] of char;
  Status_vector: ISC_STATUS_VECTOR;
  DBName: string;
  DbNameLength, Idx: integer;
begin
  if FServiceHandle = nil then
    FBLError(E_SM_NO_CON);
  if FDatabaseNames = nil then FDatabaseNames := TStringList.Create;
  Result := FDatabaseNames;
  FDatabaseNames.Clear;
  DBName := '';
  idx := 11;
  RequestInfo := char(isc_info_svc_svr_db_info);
  if isc_service_query(@Status_vector, @FServiceHandle, nil, 0,nil,
    1, @RequestInfo, SizeOf(BufferResult),
    BufferResult) <> 0 then
    FBLShowError(@Status_vector);
  if BufferResult[0] = char(isc_info_svc_svr_db_info) then
  begin
    while BufferResult[idx] = char(isc_spb_dbname) do
    begin
      Inc(Idx);
      DbNameLength := isc_vax_integer(@BufferResult[idx], 2);
      Inc(Idx, 2);
      SetLength(DBName, DbNameLength);
      Move(BufferResult[idx], DBName[1], DbNameLength);
      Inc(Idx, DbNameLength);
      FDatabaseNames.Add(DBName);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TFBLService.GFixSetPageBuffers(const ADatabaseFile: string;
  APageBufferLenght: longint);
begin
  CallProc32(ADatabaseFile, isc_action_svc_properties, isc_spb_prp_page_buffers,
    APageBufferLenght);
end;

//------------------------------------------------------------------------------

procedure TFBLService.GFixSetSweepInterval(const ADatabaseFile: string;
  ASweepIntervalLenght: longint);
begin
  CallProc32(ADatabaseFile, isc_action_svc_properties, isc_spb_prp_sweep_interval,
    ASweepIntervalLenght);
end;

//------------------------------------------------------------------------------

procedure TFBLService.GFixSetShutDownDb(const ADatabaseFile: string; ATimeOut: longint);
begin
  CallProc32(ADatabaseFile, isc_action_svc_properties, isc_spb_prp_shutdown_db, ATimeOut);
end;

//------------------------------------------------------------------------------

procedure TFBLService.GFixSetShutDownDbTran(const ADatabaseFile: string;
  ATimeOut: longint);
begin
  CallProc32(ADatabaseFile, isc_action_svc_properties,
    isc_spb_prp_deny_new_transactions, ATimeOut);
end;

//------------------------------------------------------------------------------

procedure TFBLService.GFixSetShutDownDbAttach(const ADatabaseFile: string;
  ATimeOut: longint);
begin
  CallProc32(ADatabaseFile, isc_action_svc_properties,
    isc_spb_prp_deny_new_attachments, ATimeOut);
end;

//------------------------------------------------------------------------------

procedure TFBLService.GFixSetReserveSpaceFull(const ADatabaseFile: string);
begin
  CallProc8(ADatabaseFile, isc_action_svc_properties, isc_spb_prp_reserve_space,
    isc_spb_prp_res_use_full);
end;

//------------------------------------------------------------------------------

procedure TFBLService.GFixSetReserveSpaceRes(const ADatabaseFile: string);
begin
  CallProc8(ADatabaseFile, isc_action_svc_properties, isc_spb_prp_reserve_space,
    isc_spb_prp_res);
end;

//------------------------------------------------------------------------------

procedure TFBLService.GFixSetWriteModeAsync(const ADatabaseFile: string);
begin
  CallProc8(ADatabaseFile, isc_action_svc_properties, isc_spb_prp_write_mode,
    isc_spb_prp_wm_async);
end;

//------------------------------------------------------------------------------

procedure TFBLService.GFixSetWriteModeSync(const ADatabaseFile: string);
begin
  CallProc8(ADatabaseFile, isc_action_svc_properties, isc_spb_prp_write_mode,
    isc_spb_prp_wm_sync);
end;

//------------------------------------------------------------------------------

procedure TFBLService.GFixSetAccessModeReadOnly(const ADatabaseFile: string);
begin
  CallProc8(ADatabaseFile, isc_action_svc_properties, isc_spb_prp_access_mode,
    isc_spb_prp_am_readonly);
end;

//------------------------------------------------------------------------------

procedure TFBLService.GFixSetAccessModeReadWrite(const ADatabaseFile: string);
begin
  CallProc8(ADatabaseFile, isc_action_svc_properties, isc_spb_prp_access_mode,
    isc_spb_prp_am_readwrite);
end;

//------------------------------------------------------------------------------

procedure TFBLService.GFixSetSqlDialect(const ADatabaseFile: string; AValue: integer);
begin
  if (AValue <> 1) and (AValue <> 3) then
    FBLError(E_DB_SQLDIALECT_INVALID);
  CallProc32(ADatabaseFile, isc_action_svc_properties, isc_spb_prp_set_sql_dialect, AValue);
end;

//------------------------------------------------------------------------------

procedure TFBLService.GfixRepair(const ADatabaseFile: string; AOption: TGfixRepairs);
var
  Options: longint;   //bitmask of isc_spb_options
begin
  Options := 0;
  //gfrCheckDb,gfrIgnore,gfrKill,gfrMend,gfrValidate,gfrFull,gfrSweep
  if gfrCheckDb in AOption then
    Options := Options or isc_spb_rpr_check_db;
  if gfrIgnore in AOption then
    Options := Options or isc_spb_rpr_ignore_checksum;
  if gfrKill in AOption then
    Options := Options or isc_spb_rpr_kill_shadows;
  if gfrMend in AOption then
    Options := Options or isc_spb_rpr_mend_db;
  if gfrValidate in AOption then
    Options := Options or isc_spb_rpr_validate_db;
  if gfrFull in AOption then
    Options := Options or isc_spb_rpr_full;
  if gfrSweep in AOption then
    Options := Options or isc_spb_rpr_sweep_db;
  CallProc32(ADatabaseFile, isc_action_svc_repair, isc_spb_options, Options);
  WriteLineOutput(isc_action_svc_repair);
end;

end.
