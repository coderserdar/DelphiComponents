{$I fsdefine.inc}

Unit fsadminplug;

Interface
Uses
  Classes,
  FsLLBase,
  FsSrCfg,
  FsLLComm,
  FsLLEng,
  FsLLLog,
  Fsnetmsg,
  Fssrbase,
  fsserverclass;

{ Message constants and data structures for the plugin. }
Const
  fsnmAP_Base = fsnmUser + 40; // arbitrary but unique basenumber
  // user management
  fsnmGetUser = fsnmAP_Base;
  fsnmAddUser = fsnmAP_Base + 1;
  fsnmUpdateUser = fsnmAP_Base + 2;
  fsnmDeleteUser = fsnmAP_Base + 3;
  fsnmExistsUser = fsnmAP_Base + 4;
  fsnmUserCount = fsnmAP_Base + 5;
  // batched user management
  fsnmGetUserList = fsnmAP_Base + 10;
  fsnmSetUserList = fsnmAP_Base + 11;
  fsnmClearUserList = fsnmAP_Base + 12;
  // configuration management
  fsnmGetConfig = fsnmAP_Base + 20;
  fsnmSetConfig = fsnmAP_Base + 21;
  fsnmGetIsSecure = fsnmAP_Base + 22;
  fsnmSetIsSecure = fsnmAP_Base + 23;

  // Component install targets
  fsc_CompSrPage = 'FSSQL Utils';

Type
  // structures to hold user data
  PfsnmUser = ^TfsnmUser;
  TfsnmUser = Packed Record
    UserID: TffName;
    aLastName: TffName;
    aFirstName: TffName;
    aPwdHash: TffWord32;
    Rights: TffUserRights;
  End;

  // structure for batched UserList messages
  PfsUserList = ^TfsUserList;

  // structure for batched Configuration messages
  PfsnmConfAdmin = ^TfsnmConfAdmin;
  TfsnmConfAdmin = Packed Record
    {$IFDEF IsDelphi}
    giServerName: TffNetName;
    {$ELSE}
    giServerName: TffNetNameShr;
    {$ENDIF}
    giMaxRAM: Longint;
    giSingleUser: Boolean;
    giIPXSPX: Boolean;
    giIPXSPXLFB: Boolean;
    giTCPIP: Boolean;
    giTCPIPLFB: Boolean;
    giTCPPort: Longint;
    giUDPPortSr: Longint;
    giUDPPortCl: Longint;
    giIPXSocketSr: Longint;
    giIPXSocketCl: Longint;
    giSPXSocket: Longint;
    giIsSecure: Boolean;
    giIsSecureServer: Boolean;
    giAutoUp: Boolean;
    giAutoMini: Boolean;
    giDebugLog: Boolean;
    giEncrypt: Boolean;
    giReadOnly: Boolean;
    giLastMsgInterval: Longint;
    giKAInterval: Longint;
    giKARetries: Longint;
    giPriority: Longint;
    giTCPInterface: Longint;
    giNoAutoSaveCfg: Boolean;
    giTempStoreSize: Integer;
    giCollectEnabled: Boolean;
    giCollectFreq: Longint;
    giEnabledTrigers: Boolean;
    giEnabledProcedures: Boolean;
    giEnabledReferential: Boolean;
    giEnabledxx: Boolean;
    giMaxClients: Longint;
    giMaxDuplicateUsers: Longint;
    giSendErrorIfLoginDuplicateUser: Boolean;
    giMaxDbOpen: Longint;
    giMaxSessionOpen: Longint;
    giClearCachePerCount: LongInt;
    giCloseInactiveTablesAfterCommitOrRoolback: Boolean;
    giCloseInactiveTables: Boolean;
    giClearCacheIfUpdate: Boolean;
    giClearCache: Boolean;
    giEncryptTempStorage: Boolean;
  End;

  // == REMOTE ADMIN OBJECT DEFINITIONS ==========================================

Type
  { This is the abstract class defining the plugin's interface. }
  TFSBaseAdminPlugin = Class(TFSBasePluginEngine)
  Protected
    FUsername: TffName; // username for remote server login
    FPwdHash: TffWord32; // passwordhash for server login
    Procedure scInitialize; Override;
    Procedure scPrepareForShutdown; Override;
    Procedure scShutdown; Override;
    Procedure scStartup; Override;
    Procedure ceSetPassword(Value: TffName); Virtual;
    Function ceGetPassword: TffName; Virtual;
    Property Username: TffName Read FUsername Write FUsername;
    Property Password: TffName Read ceGetPassword Write ceSetPassword;
  Public
    Constructor Create(aOwner: TComponent); Override;
    Procedure Close; Virtual;
    Procedure Start; Virtual;
    Function GetUser(RM: PfsnmUser): TffResult; Virtual; Abstract;
    Function AddUser(RM: PfsnmUser): TffResult; Virtual; Abstract;
    Function UpdateUser(RM: PfsnmUser): TffResult; Virtual; Abstract;
    Function DeleteUser(Const UserID: TffName): TffResult; Virtual; Abstract;
    Function ExistsUser(Const UserID: TffName): TffResult; Virtual; Abstract;
    Function GetUserList(Var UL: PfsUserList): TffResult; Virtual; Abstract;
    Function SetUserList(UL: PfsUserList): TffResult; Virtual; Abstract;
    Function ClearUserList: TffResult; Virtual; Abstract;
    Function GetUserCount(Var aCount: Integer): TffResult; Virtual; Abstract;
    Function GetConfig(CM: PfsnmConfAdmin): TffResult; Virtual; Abstract;
    Function SetConfig(CM: PfsnmConfAdmin): TffResult; Virtual; Abstract;
    Function GetIsSecure(Var Status: boolean): TffResult; Virtual; Abstract;
    Function SetIsSecure(Status: boolean): TffResult; Virtual; Abstract;
  End;

Type
  { This is the class that implements the plugin's interface. }
  TFSAdminServer = Class(TFSBaseAdminPlugin)
  Protected
    FEngine: TFSServer;
  Public
    Constructor Create(aOwner: TComponent); Override;
    Function GetUser(RM: PfsnmUser): TffResult; Override;
    Function AddUser(RM: PfsnmUser): TffResult; Override;
    Function UpdateUser(RM: PfsnmUser): TffResult; Override;
    Function DeleteUser(Const UserID: TffName): TffResult; Override;
    Function ExistsUser(Const UserID: TffName): TffResult; Override;
    Function GetUserList(Var UL: PfsUserList): TffResult; Override;
    Function SetUserList(UL: PfsUserList): TffResult; Override;
    Function ClearUserList: TffResult; Override;
    Function GetUserCount(Var aCount: Integer): TffResult; Override;
    Function GetConfig(CM: PfsnmConfAdmin): TffResult; Override;
    Function SetConfig(CM: PfsnmConfAdmin): TffResult; Override;
    Function GetIsSecure(Var Status: boolean): TffResult; Override;
    Function SetIsSecure(Status: boolean): TffResult; Override;
  Published
    Property ServerEngine: TFSServer Read FEngine Write FEngine;
  End;

Type
  { This is the client-side class that implements a remote interface to the
    actual plugin engine. }
  TFSRemoteAdminServer = Class(TFSBaseAdminPlugin)
  Protected
    FClientID: TffClientID; // ClientID returned when a connection is established
    FTransport: TFSBaseTransport;
    FAutoConnect: boolean; // connect plugin at startup
    Procedure CheckPluginRequirements;
    Procedure scPrepareForShutdown; Override;
    Procedure scStartup; Override;
    Procedure ceSetTransport(aTransport: TFSBaseTransport); // set Transport property
    Function DoUserRequest(RM: PfsnmUser; MsgID: Longint): TffResult;
  Public
    Constructor Create(aOwner: TComponent); Override;
    Procedure Close; Override;
    Procedure Start; Override;
    Procedure FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp;
      Const AData: TffWord32); Override;

    Function GetUser(RM: PfsnmUser): TffResult; Override;
    Function AddUser(RM: PfsnmUser): TffResult; Override;
    Function UpdateUser(RM: PfsnmUser): TffResult; Override;
    Function DeleteUser(Const UserID: TffName): TffResult; Override;
    Function ExistsUser(Const UserID: TffName): TffResult; Override;
    Function GetUserList(Var UL: PfsUserList): TffResult; Override;
    Function SetUserList(UL: PfsUserList): TffResult; Override;
    Function ClearUserList: TffResult; Override;
    Function GetUserCount(Var aCount: Integer): TffResult; Override;
    Function GetConfig(CM: PfsnmConfAdmin): TffResult; Override;
    Function SetConfig(CM: PfsnmConfAdmin): TffResult; Override;
    Function GetIsSecure(Var Status: boolean): TffResult; Override;
    Function SetIsSecure(Status: boolean): TffResult; Override;
  Published
    Property AutoConnect: boolean Read FAutoConnect Write FAutoConnect Default True;
    Property Username: TffName Read FUsername Write FUsername;
    Property Password: TffName Read ceGetPassword Write ceSetPassword;
    // transport through which this class talks to the remote server
    Property Transport: TFSBaseTransport Read FTransport Write ceSetTransport;
  End;

Type
  { This is the plugin command handler that translates messages from the
    remote interface into method calls of the actual plugin engine. }
  TFSAdminHandler = Class(TFSBasePluginCommandHandler)
  Protected
    Function epcGetPluginEngine: TFSAdminServer;
    Procedure epcSetPluginEngine(anEngine: TFSAdminServer);
    // Called by Process for fsnmGetUser message
    Procedure nmGetUser(Var Msg: TfsDataMessage); Message fsnmGetUser;
    Procedure nmAddUser(Var Msg: TfsDataMessage); Message fsnmAddUser;
    Procedure nmUpdateUser(Var Msg: TfsDataMessage); Message fsnmUpdateUser;
    Procedure nmDeleteUser(Var Msg: TfsDataMessage); Message fsnmDeleteUser;
    Procedure nmExistsUser(Var Msg: TfsDataMessage); Message fsnmExistsUser;
    Procedure nmGetUserList(Var Msg: TfsDataMessage); Message fsnmGetUserList;
    Procedure nmSetUserList(Var Msg: TfsDataMessage); Message fsnmSetUserList;
    Procedure nmClearUserList(Var Msg: TfsDataMessage); Message fsnmClearUserList;
    Procedure nmGetUserCount(Var Msg: TfsDataMessage); Message fsnmUserCount;
    Procedure nmGetConfig(Var Msg: TfsDataMessage); Message fsnmGetConfig;
    Procedure nmSetConfig(Var Msg: TfsDataMessage); Message fsnmSetConfig;
    Procedure nmGetIsSecure(Var Msg: TfsDataMessage); Message fsnmGetIsSecure;
    Procedure nmSetIsSecure(Var Msg: TfsDataMessage); Message fsnmSetIsSecure;
    Procedure scInitialize; Override;
    Procedure scPrepareForShutdown; Override;
    Procedure scShutdown; Override;
    Procedure scStartup; Override;
  Public
    // This method is called by a command handler when it has a message that
    // may be processed by a plugin.  If the plugin handles the message,
    // set handled to True. }
    Procedure Process(Msg: PFSDataMessage; Var Handled: boolean); Override;
  Published
    Property PluginEngine: TFSAdminServer Read epcGetPluginEngine Write epcSetPluginEngine;
  End;

Procedure Register;

Implementation

Uses
  SysUtils,
  FsLLComp,
  FsLLReq,
  FsSrBDE,
  FsLLProt,
  FsHash;

Var
  ptrRequest: PfsnmUser; // global pointer for callback routines user info
  ptrUserList: PfsUserList; // global pointer for callback userlist routines
  ptrConf: PfsnmConfAdmin; // global pointer for callback config routines
  ptrBool: ^boolean; // global pointer for boolean callback results
  ptrInteger: ^Integer; // global pointer for integer callback results
  cbResult: TffResult; // global result variable for callback results

  {== Stream helper routines ====================================================}

Procedure UList2Stream(UL: PfsUserList; Var aStream: TMemoryStream; Var aLen: Longint);
// convert fsUserList referred to by UL to memorystream aStream
Var
  itmSize, usrCount, I: Integer;
  aItem: TfsnmUser;
Begin
  // write userlist to stream
  usrCount := UL^.Count;
  itmSize := SizeOf(aItem);
  // first 2 bytes of stream hold usercount
  aStream.Write(usrCount, 2);
  For I := 0 To pred(usrCount) Do
    Begin
      // copy userdata to buffer
      aItem.UserID := UL^.UserItem[I].UserID;
      aItem.aLastName := UL^.UserItem[I].LastName;
      aItem.aFirstName := UL^.UserItem[I].FirstName;
      aItem.aPwdHash := UL^.UserItem[I].PasswordHash;
      aItem.Rights := UL^.UserItem[I].Rights;
      // write buffer to stream
      aStream.Write(aItem, itmSize);
    End;
  // reset stream and calc total length
  aStream.Position := 0;
  aLen := 2 + (usrCount * itmSize);
End;

Procedure Stream2UList(aStream: TMemoryStream; Var UL: PfsUserList);
// convert memorystream aStream to a fsUserList referred to by UL
Var
  itmSize, I: Integer;
  usrCount: Word;
  aItem: TfsnmUser;
Begin
  // reset stream
  aStream.Position := 0;
  // first 2 bytes hold user count
  aStream.Read(usrCount, 2);
  usrCount := (usrCount And $FFFF); // mask 2 bytes only just to be sure
  itmSize := SizeOf(aItem);
  // read user data from stream and add to the userlist
  UL^.Empty;
  For I := 0 To pred(usrCount) Do
    Begin
      aStream.Read(aItem, itmSize);
      With aItem Do
        UL^.AddUser(TfsUserItem.Create(UserID, aLastName, aFirstName, aPwdHash, Rights));
    End;
End;

{== TFSBaseAdminPlugin =====================================================}

Constructor TFSBaseAdminPlugin.Create;
Begin
  Inherited;
  FUserName := '';
  FPwdHash := $FFFFFFFF; // unknown passwordhash
End;
{--------}

Procedure TFSBaseAdminPlugin.scInitialize;
Begin
  { Do nothing. }
End;
{--------}

Procedure TFSBaseAdminPlugin.scPrepareForShutdown;
Begin
  { Do nothing. }
End;
{--------}

Procedure TFSBaseAdminPlugin.scShutdown;
Begin
  { Do nothing. }
End;
{--------}

Procedure TFSBaseAdminPlugin.scStartup;
Begin
  { Do nothing. }
End;
{--------}

Procedure TFSBaseAdminPlugin.Start;
Begin
  { Do nothing. }
End;
{--------}

Procedure TFSBaseAdminPlugin.Close;
Begin
  { Do nothing. }
End;
{--------}

Function TFSBaseAdminPlugin.ceGetPassword: TffName;
Begin
  Result := '******'; // no password read allowed
End;
{--------}

Procedure TFSBaseAdminPlugin.ceSetPassword(Value: TffName);
// store password as passwordhash
Begin
  fPwdHash := FSCalcShStrElfHash(Value);
End;

{== TFSAdminServer ===============================================================}

Constructor TFSAdminServer.Create(aOwner: TComponent);
Begin
  Inherited;
  FEngine := Nil;
End;

Function TFSAdminServer.GetUser(RM: PfsnmUser): TffResult;
// This is where the server acts upon the request to read user data
Var
  usrItem: TfsUserItem;
  idxUser: Integer;
Begin
  If assigned(FEngine) Then
    Begin
      // get user data
      idxUser := FEngine.Configuration.UserList.UserIndex(RM^.UserID);
      If (idxUser <> -1) Then // user found
        Begin
          usrItem := FEngine.Configuration.UserList.UserItem[idxUser];
          With RM^ Do
            Begin
              UserID := usrItem.UserID;
              aLastName := usrItem.LastName;
              aFirstName := usrItem.FirstName;
              aPwdHash := usrItem.PasswordHash;
              Rights := FEngine.Configuration.UserList.UserRights[RM^.UserID];
            End;
          Result := DBIERR_NONE; // all is well
        End
      Else
        Result := DBIERR_INVALIDPASSWORD; // no such user
    End
  Else
    Result := DBIERR_UNKNOWNDRIVER; // no server component found
End;

Function TFSAdminServer.AddUser(RM: PfsnmUser): TffResult;
// This is where the server acts upon the request to add user data
Var
  idxUser: Integer;
  theList: TfsUserList;
Begin
  If assigned(Fengine) Then
    Begin
      // get user data
      idxUser := FEngine.Configuration.UserList.UserIndex(RM^.UserID);
      If (idxUser = -1) Then // user not found, OK to add
        Begin
          theList := FEngine.Configuration.UserList;
          With RM^ Do
            theList.AddUser(TfsUserItem.Create(UserID, aLastName, aFirstName, aPwdHash, Rights));
          // write out updated userlist
          Result := FEngine.SaveConfiguration;
        End
      Else
        Result := DBIERR_INVALIDPASSWORD; // user already exists
    End
  Else
    Result := DBIERR_UNKNOWNDRIVER; // no server component found
End;

Function TFSAdminServer.UpdateUser(RM: PfsnmUser): TffResult;
// This is where the server acts upon the request to read user data
Var
  idxUser: Integer;
  theList: TfsUserList;
Begin
  If Assigned(Fengine) Then
    Begin
      // get user data
      idxUser := FEngine.Configuration.UserList.UserIndex(RM^.UserID);
      If (idxUser <> -1) Then // user found
        Begin
          theList := FEngine.Configuration.UserList;
          // update existing user by removing / reinserting it
          With RM^ Do
            Begin
              theList.DeleteUser(UserID);
              theList.AddUser(TfsUserItem.Create(UserID, aLastName, aFirstName, aPwdHash, Rights));
            End;
          // write out updated userlist
          Result := FEngine.SaveConfiguration;
        End
      Else
        Result := DBIERR_INVALIDPASSWORD; // no such user
    End
  Else
    Result := DBIERR_UNKNOWNDRIVER; // no server component found
End;

Function TFSAdminServer.DeleteUser(Const UserID: TffName): TffResult;
// This is where the server acts upon the request to read user data
Var
  idxUser: Integer;
Begin
  If Assigned(Fengine) Then
    Begin
      // get user data
      idxUser := FEngine.Configuration.UserList.UserIndex(UserID);
      If (idxUser <> -1) Then // user found, OK to delete it
        Begin
          FEngine.Configuration.UserList.DeleteUser(UserID);
          // write out updated userlist
          Result := FEngine.SaveConfiguration;
        End
      Else
        Result := DBIERR_INVALIDPASSWORD; // no such user
    End
  Else
    Result := DBIERR_UNKNOWNDRIVER; // no server component found
End;

Function TFSAdminServer.ExistsUser(Const UserID: TffName): TffResult;
// This is where the server acts upon the request to check user existance
Var
  idxUser: Integer;
Begin
  If Assigned(Fengine) Then
    Begin
      // get user data
      idxUser := FEngine.Configuration.UserList.UserIndex(UserID);
      If (idxUser <> -1) Then
        Result := DBIERR_NONE // user found
      Else
        Result := DBIERR_INVALIDPASSWORD; // no such user
    End
  Else
    Result := DBIERR_UNKNOWNDRIVER; // no server component found
End;

Function TFSAdminServer.GetUserList(Var UL: PfsUserList): TffResult;
// This is where the server acts upon the request to send a userlist
Var
  I: Integer;
  theList: TfsUserList;
Begin
  If Assigned(Fengine) Then
    Begin
      theList := FEngine.Configuration.UserList;
      // copy user list
      UL^.Empty;
      For I := 0 To pred(theList.Count) Do
        UL^.AddUser(theList.UserItem[I]);
      Result := DBIERR_NONE;
    End
  Else
    Begin
      UL := Nil;
      Result := DBIERR_UNKNOWNDRIVER; // no server component found
    End;
End;

Function TFSAdminServer.SetUserList(UL: PfsUserList): TffResult;
// This is where the server acts upon the request to set a userlist
Var
  I: Integer;
  theList: TfsUserList;
Begin
  If Assigned(Fengine) Then
    Begin
      theList := FEngine.Configuration.UserList;
      // copy user list
      theList.Empty;
      For I := 0 To pred(UL^.Count) Do
        theList.AddUser(UL^.UserItem[I]);
      // write out updated userlist
      Result := FEngine.SaveConfiguration;
    End
  Else
    Result := DBIERR_UNKNOWNDRIVER; // no server component found
End;

Function TFSAdminServer.ClearUserList: TffResult;
// This is where the server acts upon the request to clear the userlist
Begin
  If Assigned(Fengine) Then
    Begin
      // clear the list
      FEngine.Configuration.UserList.Empty;
      // write out updated userlist
      Result := FEngine.SaveConfiguration;
    End
  Else
    Result := DBIERR_UNKNOWNDRIVER; // no server component found
End;

Function TFSAdminServer.GetUserCount(Var aCount: Integer): TffResult;
// This is where the server acts upon the request to return the usercount
Begin
  If Assigned(Fengine) Then
    Begin
      aCount := FEngine.Configuration.UserList.Count;
      Result := DBIERR_NONE; // all is well
    End
  Else
    Result := DBIERR_UNKNOWNDRIVER; // no server component found
End;

Function TFSAdminServer.GetConfig(CM: PfsnmConfAdmin): TffResult;
// This is where the server acts upon the request to read the server configuration
Var
  theInfo: PfsGeneralInfo;
Begin
  If Assigned(Fengine) Then
    Begin
      theInfo := FEngine.Configuration.GeneralInfo;
      // copy the infoblock to our resultstructure
      With CM^ Do
        Begin
          giServerName := theInfo^.giServerName;
          giMaxRAM := theInfo^.giMaxRAM;
          giSingleUser := theInfo^.giSingleUser;
          giIPXSPX := theInfo^.giIPXSPX;
          giIPXSPXLFB := theInfo^.giIPXSPXLFB;
          giTCPIP := theInfo^.giTCPIP;
          giTCPIPLFB := theInfo^.giTCPIPLFB;
          giTCPPort := theInfo^.giTCPPort;
          giUDPPortSr := theInfo^.giUDPPortSr;
          giUDPPortCl := theInfo^.giUDPPortCl;
          giIPXSocketSr := theInfo^.giIPXSocketSr;
          giIPXSocketCl := theInfo^.giIPXSocketCl;
          giSPXSocket := theInfo^.giSPXSocket;
          giIsSecure := theInfo^.giIsSecure;
          giIsSecureServer := theInfo^.giIsSecureServer;
          giAutoUp := theInfo^.giAutoUp;
          giAutoMini := theInfo^.giAutoMini;
          giDebugLog := theInfo^.giDebugLog;
          giEncrypt := theInfo^.giEncrypt;
          giReadOnly := theInfo^.giReadOnly;
          giLastMsgInterval := theInfo^.giLastMsgInterval;
          giKAInterval := theInfo^.giKAInterval;
          giKARetries := theInfo^.giKARetries;
          giPriority := theInfo^.giPriority;
          giTCPInterface := theInfo^.giTCPInterface;
          giNoAutoSaveCfg := theInfo^.giNoAutoSaveCfg;
          giTempStoreSize := theInfo^.giTempStoreSize;
          giCollectEnabled := theInfo^.giCollectEnabled;
          giCollectFreq := theInfo^.giCollectFreq;
          giEnabledTrigers := theInfo^.giEnabledTrigers;
          giEnabledProcedures := theInfo^.giEnabledProcedures;
          giEnabledReferential := theInfo^.giEnabledReferential;
          giEnabledxx := theInfo^.giEnabledxx;
          giMaxClients := theInfo^.giMaxClients;
          giMaxDuplicateUsers := theInfo^.giMaxDuplicateUsers;
          giSendErrorIfLoginDuplicateUser := theInfo^.giSendErrorIfLoginDuplicateUser;
          giMaxDbOpen := theInfo^.giMaxDbOpen;
          giMaxSessionOpen := theInfo^.giMaxSessionOpen;
          giClearCachePerCount := theInfo^.giClearCachePerCount;
          giCloseInactiveTablesAfterCommitOrRoolback := theInfo^.giCloseInactiveTablesAfterCommitOrRoolback;
          giCloseInactiveTables := theInfo^.giCloseInactiveTables;
          giClearCacheIfUpdate := theInfo^.giClearCacheIfUpdate;
          giClearCache := theInfo^.giClearCache;
          giEncryptTempStorage := theInfo^.giEncryptTempStorage;
        End;
      Result := DBIERR_NONE; // all is well
    End
  Else
    Result := DBIERR_UNKNOWNDRIVER; // no server component found
End;

Function TFSAdminServer.SetConfig(CM: PfsnmConfAdmin): TffResult;
// This is where the server acts upon the request to set the server configuration
Var
  theInfo: PfsGeneralInfo;
  OverrideRO: boolean;
Begin
  If Assigned(Fengine) Then
    Begin
      theInfo := FEngine.Configuration.GeneralInfo;
      With theInfo^ Do
        Begin
          giServerName := CM^.giServerName;
          giMaxRAM := CM^.giMaxRAM;
          giSingleUser := CM^.giSingleUser;
          giIPXSPX := CM^.giIPXSPX;
          giIPXSPXLFB := CM^.giIPXSPXLFB;
          giTCPIP := CM^.giTCPIP;
          giTCPIPLFB := CM^.giTCPIPLFB;
          giTCPPort := CM^.giTCPPort;
          giUDPPortSr := CM^.giUDPPortSr;
          giUDPPortCl := CM^.giUDPPortCl;
          giIPXSocketSr := CM^.giIPXSocketSr;
          giIPXSocketCl := CM^.giIPXSocketCl;
          giSPXSocket := CM^.giSPXSocket;
          giIsSecure := CM^.giIsSecure;
          giIsSecureServer := CM^.giIsSecureServer;
          giAutoUp := CM^.giAutoUp;
          giAutoMini := CM^.giAutoMini;
          giDebugLog := CM^.giDebugLog;
          giEncrypt := CM^.giEncrypt;
          giReadOnly := CM^.giReadOnly;
          giLastMsgInterval := CM^.giLastMsgInterval;
          giKAInterval := CM^.giKAInterval;
          giKARetries := CM^.giKARetries;
          giTCPInterface := CM^.giTCPInterface;
          giNoAutoSaveCfg := CM^.giNoAutoSaveCfg;
          giTempStoreSize := CM^.giTempStoreSize;
          giCollectEnabled := CM^.giCollectEnabled;
          giCollectFreq := CM^.giCollectFreq;
          giEnabledTrigers := CM^.giEnabledTrigers;
          giEnabledProcedures := CM^.giEnabledProcedures;
          giEnabledReferential := CM^.giEnabledReferential;
          giEnabledxx := CM^.giEnabledxx;
          giMaxClients := CM^.giMaxClients;
          giMaxDuplicateUsers := CM^.giMaxDuplicateUsers;
          giSendErrorIfLoginDuplicateUser := CM^.giSendErrorIfLoginDuplicateUser;
          giMaxDbOpen := CM^.giMaxDbOpen;
          giMaxSessionOpen := CM^.giMaxSessionOpen;
          giClearCachePerCount := CM^.giClearCachePerCount;
          giCloseInactiveTablesAfterCommitOrRoolback := CM^.giCloseInactiveTablesAfterCommitOrRoolback;
          giCloseInactiveTables := CM^.giCloseInactiveTables;
          giClearCacheIfUpdate := CM^.giClearCacheIfUpdate;
          giClearCache := CM^.giClearCache;
          giEncryptTempStorage := CM^.giEncryptTempStorage;

          If (CM^.giPriority < -2) Or (CM^.giPriority > 2) Then
            giPriority := 0
          Else
            giPriority := CM^.giPriority;

          // set constants for Keep Alive
          fsc_LastMsgInterval := giLastMsgInterval;
          fsc_KeepAliveInterval := giKAInterval;
          fsc_KeepAliveRetries := giKARetries;
        End;
      // write out new server configuration
      Result := FEngine.SaveConfiguration;
    End
  Else
    Result := DBIERR_UNKNOWNDRIVER; // no server component found
End;

Function TFSAdminServer.GetIsSecure(Var Status: boolean): TffResult;
// This is where the server acts upon the request to get security
Begin
  If Assigned(Fengine) Then
    Begin
      // get server security
      Status := FEngine.Configuration.GeneralInfo^.giIsSecure;
      Result := DBIERR_NONE;
    End
  Else
    Result := DBIERR_UNKNOWNDRIVER // no server component found
End;

Function TFSAdminServer.SetIsSecure(Status: boolean): TffResult;
// This is where the server acts upon the request to set security
Begin
  If Assigned(Fengine) Then
    Begin
      // set server security
      FEngine.Configuration.GeneralInfo^.giIsSecure := Status;
      // write out change, disable OverrideReadOnly
      Result := FEngine.SaveConfiguration;
    End
  Else
    Result := DBIERR_UNKNOWNDRIVER // no server component found
End;

{== TFSRemoteAdminServer =========================================================}

Constructor TFSRemoteAdminServer.Create(aOwner: TComponent);
Begin
  Inherited;
  FTransport := Nil;
  FAutoConnect := True;
End;

Procedure TFSRemoteAdminServer.ceSetTransport(aTransport: TFSBaseTransport);
Begin
  // Check to make sure the new property is different
  If FTransport = aTransport Then
    Exit;
  // Are we already connected to a transport?
  If assigned(FTransport) Then
    Begin
      // Yes.  Notify it that we are disconnecting

      FTransport.FFRemoveDependent(Self);

      FTransport := Nil; // D3 compatibility
    End;
  // Do we have a new value for the Transport property?
  If assigned(aTransport) Then
    Begin
      // Yes.  Set our internal variable and make sure the transport tells us
      // when it is freed. }
      FTransport := aTransport;

      FTransport.FFAddDependent(Self);

    End;
End;

Procedure TFSRemoteAdminServer.FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp;
  Const AData: TffWord32);
Begin
  Inherited;
  If (AOp In [ffn_Destroy, ffn_Remove]) Then
    If (AFrom = FTransport) Then
      Begin
        FTransport.FFRemoveDependent(Self);
        FTransport := Nil;
      End;
End;

Procedure TFSRemoteAdminServer.scPrepareForShutdown;
Begin
  If FAutoConnect Then
    Close;
End;

Procedure TFSRemoteAdminServer.scStartup;
Begin
  If FAutoConnect Then
    Start;
End;

Procedure TFSRemoteAdminServer.Close;
// manually shutdown client connection to server
Begin
  FTransport.TerminateConnection(FClientID, 5000);
  scSetState(fsesStopped);
End;

Procedure TFSRemoteAdminServer.Start;
// manually start plugin
Var
  Result: TffResult;
  aRights: TffUserRights;
  aSecurityEnabled: boolean;
Begin
  // connect to remote server using supplied username / password
  aRights := [];
  aSecurityEnabled:= false;
  Result := FTransport.EstablishConnection(FUsername,
    FPwdHash,
    5000, FClientID, aRights,aSecurityEnabled);
  If (Result = DBIERR_NONE) Then
    scSetState(fsesStarted)
  Else
    Raise Exception.Create('Remote Admin: connection refused.');
End;

Procedure TFSRemoteAdminServer.CheckPluginRequirements;
// test if plugin requirements for sending requests are met
Begin
  { Requirement: Transport must be assigned. }
  If Not assigned(FTransport) Then
    Raise Exception.Create('Remote Admin: Transport not assigned.');

  { Requirement: Plugin must be started. }
  If scState <> fsesStarted Then
    Raise Exception.Create('Remote Admin: Plugin not started.');
End;

Procedure cbUserRequest(msgID: Longint; ErrorCode: TffResult; replyData: pointer;
  replyDataLen: Longint; replyCookie: Longint);
// callback routine for remote user admin requests
Begin
  ptrRequest^ := TfsnmUser(replyData^);
  cbResult := ErrorCode;
End;

Function TFSRemoteAdminServer.DoUserRequest(RM: PfsnmUser; MsgID: Longint): TffResult;
// generic request handler for remote plugin
Begin
  CheckPluginRequirements;
  { Build the request. We use a fixed sized message structure here. }
  ptrRequest := RM;
  cbResult := -1; // force errorcode to catch time out
  FTransport.Request(0, FClientID, MsgID, 5000, RM, SizeOf(RM^), cbUserRequest, 0);
  Result := cbResult;
End;

// The following methods just call the generic user request handler DoRequest.
// This way we stay compatible with the local FFServerAdminPlugin

Function TFSRemoteAdminServer.GetUser(RM: PfsnmUser): TffResult;
Begin
  Result := DoUserRequest(RM, fsnmGetUser);
End;

Function TFSRemoteAdminServer.AddUser(RM: PfsnmUser): TffResult;
Begin
  Result := DoUserRequest(RM, fsnmAddUser);
End;

Function TFSRemoteAdminServer.UpdateUser(RM: PfsnmUser): TffResult;
Begin
  Result := DoUserRequest(RM, fsnmUpdateUser);
End;

Procedure cbResultOnly(msgID: Longint; errorCode: TffResult; replyData: pointer;
  replyDataLen: Longint; replyCookie: Longint);
// simple callback routine for remote requests
Begin
  cbResult := errorCode;
End;

Function TFSRemoteAdminServer.DeleteUser(Const UserID: TffName): TffResult;
Var
  MsgBuf: String;
Begin
  CheckPluginRequirements;
  cbResult := -1; // force errorcode to catch time out
  MsgBuf := UserID;
  // send request - account for trailing zero in size of string !!
  FTransport.Request(0, FClientID, fsnmDeleteUser, 5000,
    PChar(MsgBuf), SizeOf(MsgBuf) + 1, cbResultOnly, 0);
  Result := cbResult;
End;

Function TFSRemoteAdminServer.ExistsUser(Const UserID: TffName): TffResult;
Var
  MsgBuf: String;
Begin
  CheckPluginRequirements;
  cbResult := -1; // force errorcode to catch time out
  MsgBuf := UserID;
  // send request - account for trailing zero in size of string !!
  FTransport.Request(0, FClientID, fsnmExistsUser, 5000,
    PChar(MsgBuf), SizeOf(MsgBuf) + 1, cbResultOnly, 0);
  Result := cbResult;
End;

Procedure cbGetUserList(msgID: Longint; ErrorCode: TffResult; replyData: pointer;
  replyDataLen: Longint; replyCookie: Longint);
// callback routine for remote userlist requests
Var
  aStream: TMemoryStream;
Begin
  aStream := TMemoryStream.Create;
  Try
    // copy reply into memory stream
    aStream.Write(replyData^, replyDataLen);
    // convert stream to userlist
    Stream2UList(aStream, ptrUserList);
  Finally
    aStream.Free;
  End;
  cbResult := ErrorCode;
End;

Function TFSRemoteAdminServer.GetUserList(Var UL: PfsUserList): TffResult;
// request handler for remote get userlist
Begin
  CheckPluginRequirements;
  { Build the request. No request data required. }
  ptrUserList := UL;
  cbResult := -1; // force errorcode to catch time out
  FTransport.Request(0, FClientID, fsnmGetUserList, 5000, Nil, 0, cbGetUserList, 0);
  Result := cbResult;
End;

Function TFSRemoteAdminServer.SetUserList(UL: PfsUserList): TffResult;
// request handler for remote set userlist
Var
  aStream: TMemoryStream;
  aRequestLen: Longint;
Begin
  CheckPluginRequirements;
  { Build the request. Convert user data to message stream. }
  aStream := TMemoryStream.Create;
  Try
    // write userlist to stream
    UList2Stream(UL, aStream, aRequestLen);
    // send request
    cbResult := -1; // force errorcode to catch time out
    FTransport.Request(0, FClientID, fsnmSetUserList, 5000,
      aStream.Memory, aRequestLen, cbResultOnly, 0);
    Result := cbResult;
  Finally
    aStream.Free;
  End;
End;

Function TFSRemoteAdminServer.ClearUserList: TffResult;
// request handler for remote clear userlist
Begin
  CheckPluginRequirements;
  // send request
  cbResult := -1; // force errorcode to catch time out
  FTransport.Request(0, FClientID, fsnmClearUserList, 5000, Nil, 0, cbResultOnly, 0);
  Result := cbResult;
End;

Procedure cbIntResult(msgID: Longint; ErrorCode: TffResult; replyData: pointer;
  replyDataLen: Longint; replyCookie: Longint);
// callback routine for remote user requesting integer result
Begin
  ptrInteger^ := Integer(replyData^);
  cbResult := ErrorCode;
End;

Function TFSRemoteAdminServer.GetUserCount(Var aCount: Integer): TffResult;
// request handler for remote clear userlist
Begin
  CheckPluginRequirements;
  // send request
  ptrInteger := @aCount;
  cbResult := -1; // force errorcode to catch time out
  FTransport.Request(0, FClientID, fsnmUserCount, 5000,
    @aCount, SizeOf(aCount), cbIntResult, 0);
  Result := cbResult;
End;

Procedure cbConfig(msgID: Longint; errorCode: TffResult; replyData: pointer;
  replyDataLen: Longint; replyCookie: Longint);
// callback routine for remote Config administration
Begin
  cbResult := errorcode;
  ptrConf^ := TfsnmConfAdmin(replyData^);
End;

Function TFSRemoteAdminServer.GetConfig(CM: PfsnmConfAdmin): TffResult;
// request handler for remote get server configuration
Begin
  CheckPluginRequirements;
  // send request
  cbResult := -1; // force errorcode to catch time out
  ptrConf := CM; // pointer to result structure for callback
  FTransport.Request(0, FClientID, fsnmGetConfig, 5000, CM, SizeOf(CM^), cbConfig, 0);
  Result := cbResult;
End;

Function TFSRemoteAdminServer.SetConfig(CM: PfsnmConfAdmin): TffResult;
// request handler for remote set server configuration
Begin
  CheckPluginRequirements;
  // send request
  cbResult := -1; // force errorcode to catch time out
  ptrConf := CM; // pointer to result structure for callback
  FTransport.Request(0, FClientID, fsnmSetConfig, 5000, CM, SizeOf(CM^), cbConfig, 0);
  Result := cbResult;
End;

Procedure cbBoolResult(msgID: Longint; ErrorCode: TffResult; replyData: pointer;
  replyDataLen: Longint; replyCookie: Longint);
// callback routine for remote user requesting boolean result
Begin
  ptrBool^ := boolean(replyData^);
  cbResult := ErrorCode;
End;

Function TFSRemoteAdminServer.GetIsSecure(Var Status: boolean): TffResult;
// request handler for getting server security mode
Begin
  CheckPluginRequirements;
  // send request
  ptrBool := @Status;
  cbResult := -1; // force errorcode to catch time out
  FTransport.Request(0, FClientID, fsnmGetIsSecure, 5000,
    @Status, SizeOf(Status), cbBoolResult, 0);
  Result := cbResult;
End;

Function TFSRemoteAdminServer.SetIsSecure(Status: boolean): TffResult;
// request handler for setting server security mode
Begin
  CheckPluginRequirements;
  // send request
  cbResult := -1; // force errorcode to catch time out
  FTransport.Request(0, FClientID, fsnmSetIsSecure, 5000,
    @Status, SizeOf(Status), cbResultOnly, 0);
  Result := cbResult;
End;

{== TFSAdminHandler ==========================================================}

Procedure TFSAdminHandler.scInitialize;
Begin
  { Do nothing. }
End;

Procedure TFSAdminHandler.scPrepareForShutdown;
Begin
  { Do nothing. }
End;

Procedure TFSAdminHandler.scShutdown;
Begin
  { Do nothing. }
End;

Procedure TFSAdminHandler.scStartup;
Begin
  { Do nothing. }
End;

Function TFSAdminHandler.epcGetPluginEngine: TFSAdminServer;
Begin
  If Assigned(FPluginEngine) Then
    Result := TFSAdminServer(FPluginEngine)
  Else
    Result := Nil;
End;

Procedure TFSAdminHandler.epcSetPluginEngine(anEngine: TFSAdminServer);
Begin
  If assigned(FPluginEngine) Then
    Begin
      FPluginEngine.FFRemoveDependent(Self);
      FPluginEngine := Nil;
    End;
  If assigned(anEngine) Then
    Begin
      anEngine.FFAddDependent(Self);
      FPluginEngine := anEngine;
    End;
End;

Procedure TFSAdminHandler.nmGetUser(Var Msg: TfsDataMessage);
Var
  aRequest: PfsnmUser;
  reqResult: TffResult;
Begin
  aRequest := PfsnmUser(Msg.dmData);
  reqResult := TFSAdminServer(FPluginEngine).GetUser(aRequest);
  // send reply through class procedure TffBaseTransport.Reply
  TFSBaseTransport.Reply(Msg.dmRequestID, reqResult, aRequest, SizeOf(aRequest^));
End;

Procedure TFSAdminHandler.nmAddUser(Var Msg: TfsDataMessage);
Var
  aRequest: PfsnmUser;
  reqResult: TffResult;
Begin
  aRequest := PfsnmUser(Msg.dmData);
  reqResult := TFSAdminServer(FPluginEngine).AddUser(aRequest);
  // send reply through class procedure TffBaseTransport.Reply
  TFSBaseTransport.Reply(Msg.dmRequestID, reqResult, aRequest, SizeOf(aRequest^));
End;

Procedure TFSAdminHandler.nmUpdateUser(Var Msg: TfsDataMessage);
Var
  aRequest: PfsnmUser;
  reqResult: TffResult;
Begin
  aRequest := PfsnmUser(Msg.dmData);
  reqResult := TFSAdminServer(FPluginEngine).UpdateUser(aRequest);
  // send reply through class procedure TffBaseTransport.Reply
  TFSBaseTransport.Reply(Msg.dmRequestID, reqResult, aRequest, SizeOf(aRequest^));
End;

Procedure TFSAdminHandler.nmDeleteUser(Var Msg: TfsDataMessage);
Var
  aRequest: TffName;
  reqResult: TffResult;
Begin
  aRequest := PChar(Msg.dmData);
  reqResult := TFSAdminServer(FPluginEngine).DeleteUser(aRequest);
  // send reply through class procedure TffBaseTransport.Reply
  TFSBaseTransport.Reply(Msg.dmRequestID, reqResult, Nil, 0);
End;

Procedure TFSAdminHandler.nmExistsUser(Var Msg: TfsDataMessage);
Var
  aRequest: TffName;
  reqResult: TffResult;
Begin
  aRequest := PChar(Msg.dmData);
  reqResult := TFSAdminServer(FPluginEngine).ExistsUser(aRequest);
  // send reply through class procedure TffBaseTransport.Reply
  TFSBaseTransport.Reply(Msg.dmRequestID, reqResult, Nil, 0);
End;

Procedure TFSAdminHandler.nmGetUserList(Var Msg: TfsDataMessage);
Var
  aList: TfsUserList;
  pList: PfsUserList;
  aStream: TMemoryStream;
  aReplyLen: Longint;
  reqResult: TffResult;
Begin
  aList := TfsUserList.Create;
  pList := @aList;
  aStream := TMemoryStream.Create;
  Try
    // get userlist
    reqResult := TFSAdminServer(FPluginEngine).GetUserList(pList);
    // convert userlist to stream for reply
    UList2Stream(pList, aStream, aReplyLen);
    // send reply through class procedure TffBaseTransport.Reply
    TFSBaseTransport.Reply(Msg.dmRequestID, reqResult, aStream.Memory, aReplyLen);
  Finally
    aStream.Free;
    aList.Free;
  End;
End;

Procedure TFSAdminHandler.nmSetUserList(Var Msg: TfsDataMessage);
Var
  aStream: TMemoryStream;
  aList: TfsUserList;
  pList: PfsUserList;
  reqResult: TffResult;
Begin
  aStream := TMemoryStream.Create;
  aList := TfsUserList.Create;
  pList := @aList;
  Try
    // copy message data into memory stream and convert to fsUserList
    aStream.Write(Msg.dmData^, Msg.dmDataLen);
    Stream2UList(aStream, pList);
    // update server userlist with the received aList
    reqResult := TFSAdminServer(FPluginEngine).SetUserList(pList);
    // send reply through class procedure TffBaseTransport.Reply
    TFSBaseTransport.Reply(Msg.dmRequestID, reqResult, Nil, 0);
  Finally
    aStream.Free;
    aList.Free;
  End;
End;

Procedure TFSAdminHandler.nmGetConfig(Var Msg: TfsDataMessage);
Var
  aRequest: PfsnmConfAdmin;
  reqResult: TffResult;
Begin
  aRequest := PfsnmConfAdmin(Msg.dmData);
  reqResult := TFSAdminServer(FPluginEngine).GetConfig(aRequest);
  // send reply through class procedure TffBaseTransport.Reply
  TFSBaseTransport.Reply(Msg.dmRequestID, reqResult, aRequest, SizeOf(aRequest^));
End;

Procedure TFSAdminHandler.nmSetConfig(Var Msg: TfsDataMessage);
Var
  aRequest: PfsnmConfAdmin;
  reqResult: TffResult;
Begin
  aRequest := PfsnmConfAdmin(Msg.dmData);
  reqResult := TFSAdminServer(FPluginEngine).SetConfig(aRequest);
  // send reply through class procedure TffBaseTransport.Reply
  TFSBaseTransport.Reply(Msg.dmRequestID, reqResult, aRequest, SizeOf(aRequest^));
End;

Procedure TFSAdminHandler.nmClearUserList(Var Msg: TfsDataMessage);
Var
  reqResult: TffResult;
Begin
  reqResult := TFSAdminServer(FPluginEngine).ClearUserList;
  // send reply through class procedure TffBaseTransport.Reply
  TFSBaseTransport.Reply(Msg.dmRequestID, reqResult, Nil, 0);
End;

Procedure TFSAdminHandler.nmGetUserCount(Var Msg: TfsDataMessage);
Var
  aCount: Integer;
  reqResult: TffResult;
Begin
  aCount := Integer(Msg.dmData^);
  reqResult := TFSAdminServer(FPluginEngine).GetUserCount(aCount);
  // send reply through class procedure TffBaseTransport.Reply
  TFSBaseTransport.Reply(Msg.dmRequestID, reqResult, @aCount, SizeOf(aCount));
End;

Procedure TFSAdminHandler.nmGetIsSecure(Var Msg: TfsDataMessage);
Var
  aStatus: boolean;
  reqResult: TffResult;
Begin
  aStatus := boolean(Msg.dmData^);
  reqResult := TFSAdminServer(FPluginEngine).GetIsSecure(aStatus);
  // send reply through class procedure TffBaseTransport.Reply
  TFSBaseTransport.Reply(Msg.dmRequestID, reqResult, @aStatus, SizeOf(aStatus));
End;

Procedure TFSAdminHandler.nmSetIsSecure(Var Msg: TfsDataMessage);
Var
  aStatus: boolean;
  reqResult: TffResult;
Begin
  aStatus := boolean(Msg.dmData^);
  reqResult := TFSAdminServer(FPluginEngine).SetIsSecure(aStatus);
  // send reply through class procedure TffBaseTransport.Reply
  TFSBaseTransport.Reply(Msg.dmRequestID, reqResult, Nil, 0);
End;

Procedure TFSAdminHandler.Process(Msg: PFSDataMessage; Var Handled: boolean);
Begin
  // specify the messages we can handle
  Case Msg^.dmMsg Of
    fsnmGetUser,
      fsnmAddUser,
      fsnmUpdateUser,
      fsnmDeleteUser,
      fsnmExistsUser,
      fsnmGetUserList,
      fsnmSetUserList,
      fsnmClearUserList,
      fsnmUserCount,
      fsnmGetConfig,
      fsnmSetConfig,
      fsnmGetIsSecure,
      fsnmSetIsSecure: Handled := True;
    Else
      Handled := False;
  End; // case
  // call request handler through message dispatch
  // Note: message data is freed for us by caller TffBaseCommandHandler.Process
  If Handled Then
    Dispatch(Msg^);
End;

{==============================================================================}

Procedure Register;
Begin
  RegisterComponents(fsc_CompSrPage, [TFSAdminServer, TFSRemoteAdminServer, TFSAdminHandler]);
End;

End.

