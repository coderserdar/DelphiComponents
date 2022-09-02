{*********************************************************}
{*             Server Configuration Information          *}
{*********************************************************}

{$I fsdefine.inc}

Unit fssrcfg;

Interface

Uses
  Windows,
  SysUtils,
  Classes,
  fsconst,
  fsllbase,
  fsllunc,
  fssrmgr,
  fsllexcp,
  fsllprot,
  fssrintf,
  fssrbase;

Type
  PfsGeneralInfo = ^TfsGeneralInfo;
  TfsGeneralInfo = Packed Record
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

Type
  TfsAliasItem = Class( TfsUCStrListItem )
  Protected {private}
    FCheckDisk: Boolean; {!!.11}
    FPath: PffShStr;
    FPathBlob: PffShStr;
  Protected
    Function GetAlias: String; {!!.10}
    Function GetPath: String; {!!.10}
    Function GetPathBlob: String; 
  Public
    Constructor Create( Const aAlias: TffName;
      Const aPath: TffPath;
      aCheckDisk: Boolean = False;
      aPathBlob: TffPath = '' );
    Destructor Destroy; Override;

    Property Alias: String Read GetAlias; {!!.10}
    Property Path: String Read GetPath; {!!.10}
    Property PathBlob: String Read GetPathBlob;
    Property CheckSpace: Boolean {!!.11}
    Read FCheckDisk; {!!.11}
  End;

  TfsAliasList = Class( TFSSpecThreadList )
  Protected
    Function GetAliasItem( aInx: Integer ): TfsAliasItem;
    Function GetAliasPath( Const aAlias: TffName ): TffPath;
  Public
    Procedure AddAlias( aAliasItem: TfsAliasItem );
    Function AliasExists( Const aAlias: TffName ): Boolean;
    Function AliasIndex( Const aAlias: TffName ): Integer;
    Function CheckDiskSpace( Const aAlias: TffName ): Boolean; {!!.11}
    Procedure DeleteAlias( Const aAlias: TffName );

    Property AliasItem[aInx: Integer]: TfsAliasItem
    Read GetAliasItem; Default;
    Property Path[Const aAlias: TffName]: TffPath
    Read GetAliasPath;
  End;

  TfsUserItem = Class( TfsUCStrListItem )
  Protected {private}
    FFirst: PffShStr;
    FLast: PffShStr;
    FPwdHash: TffWord32;
    FRights: TffUserRights;
    FSecurityEnabled: boolean;
  Protected
    Function GetFirstName: String; {!!.10}
    Function GetLastName: String; {!!.10}
    Function GetUserID: String; {!!.10}
  Public
    Constructor Create( Const aUserID: TffName;
      Const aLastName: TffName;
      Const aFirstName: TffName;
      aPwdHash: TffWord32;
      aRights: TffUserRights);
    Destructor Destroy; Override;

    Property FirstName: String Read GetFirstName; {!!.10}
    Property LastName: String Read GetLastName; {!!.10}
    Property PasswordHash: TffWord32 Read FPwdHash;
    Property Rights: TffUserRights Read FRights;
    Property UserID: String Read GetUserID;
  End;

  TfsUserList = Class( TFSSpecObject )
  Protected {private}
    FUserList: TFSNormalList;
  Protected
    Function GetUserItem( aInx: Integer ): TfsUserItem;
    Function GetUserPwdHash( Const aUserID: TffName ): TffWord32;
    Function GetUserRights( Const aUserID: TffName ): TffUserRights;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure AddUser( aUserItem: TfsUserItem );
    Function UserExists( Const aUserID: TffName ): Boolean;
    Function UserIndex( Const aUserID: TffName ): Integer;
    Function Count: Integer;
    Procedure DeleteUser( Const aUserID: TffName );
    Procedure Empty;

    Property UserItem[aInx: Integer]: TfsUserItem Read GetUserItem; Default;
    Property PasswordHash[Const aUserID: TffName]: TffWord32 Read GetUserPwdHash;
    Property UserRights[Const aUserID: TffName]: TffUserRights Read GetUserRights;
  End;

  TfsKeyProcItem = Class( TfsUCStrListItem )
  Protected {private}
    FIndexID: Integer;
    FDLLName: PffShStr;
    FBuildKey: TffKeyBuildFunc;
    FBuildName: PffShStr;
    FCompareKey: TffKeyCompareFunc;
    FCompareName: PffShStr;
    FPath: PffShStr;
    FTable: PffShStr;

    kpiLibHandle: THandle;
  Protected
    Function GetBuildKeyName: String; {!!.10}
    Function GetCompareKeyName: String; {!!.10}
    Function GetDLLName: String; {!!.10}
    Function GetPath: String; {!!.10}
    Function GetTable: String; {!!.10}
    Function GetTableDataFileName: String; {!!.10}
  Public
    Constructor Create( Const aPath: TffPath;
      Const aTable: TfsTableName;
      aIndexID: Integer;
      Const aDLLName: TffFullFileName;
      Const aBuildName: TffName;
      Const aCompareName: TffName );
    Destructor Destroy; Override;
    Function Link: boolean;
    Procedure Unlink;

    Property DLLName: String
      Read GetDLLName; {!!.10}
    Property IndexID: Integer
      Read FIndexID;
    Property BuildKey: TffKeyBuildFunc
      Read FBuildKey;
    Property BuildKeyName: String
      Read GetBuildKeyName; {!!.10}
    Property CompareKey: TffKeyCompareFunc
      Read FCompareKey;
    Property CompareKeyName: String
      Read GetCompareKeyName; {!!.10}
    Property Path: String
      Read GetPath; {!!.10}
    Property Table: String
      Read GetTable; {!!.10}
    Property TableDataFileName: String
      Read GetTableDataFileName; {!!.10}
  End;

  TfsKeyProcList = Class( TFSSpecObject )
  Protected {private}
    FKPList: TFSNormalList;
  Protected
    Function GetKPItem( aInx: Integer ): TfsKeyProcItem;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure AddKeyProc( aKeyProcItem: TfsKeyProcItem );
    Function KeyProcExists( Const aPath: TffPath;
      Const aTable: TfsTableName;
      aIndexID: Integer )
      : Boolean;
    Function KeyProcIndex( Const aPath: TffPath;
      Const aTable: TfsTableName;
      aIndexID: Integer )
      : Integer;
    Function Count: Integer;
    Procedure DeleteKeyProc( Const aPath: TffPath;
      Const aTable: TfsTableName;
      aIndexID: Integer );
    Procedure Empty;

    Property KeyProcItem[aInx: Integer]: TfsKeyProcItem
    Read GetKPItem; Default;
  End;

  TfsServerConfiguration = Class( TFSSpecObject )
  Protected {private}
    FAliasList: TfsAliasList;
    FGeneralInfo: TfsGeneralInfo;
    FKeyProcList: TfsKeyProcList;
    FUserList: TfsUserList;
    scPadLock: TfsPadlock;
  Protected
    Function GetGeneralInfo: PfsGeneralInfo;
    Function GetServerName: String; {!!.10}
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure AddAlias( Const aAlias: TffName;
      Const aPath: TffPath;
      aCheckDisk: Boolean = False;
      aPathBlob: TffPath = '');
    Procedure AddKeyProc( Const aPath: TffPath;
      Const aTable: TfsTableName;
      aIndexID: Integer;
      Const aDLLName: TffFullFileName;
      Const aBuildName: TffName;
      Const aCompareName: TffName );
    Procedure AddUser( Const aUserID: TffName;
      Const aLastName: TffName;
      Const aFirstName: TffName;
      aPwdHash: TffWord32;
      aRights: TffUserRights);
    Procedure Lock;
    Procedure PerformDynamicLink;
    Procedure Unlock;

    Property AliasList: TfsAliasList
      Read FAliasList;
    Property GeneralInfo: PfsGeneralInfo
      Read GetGeneralInfo;
    Property KeyProcList: TfsKeyProcList
      Read FKeyProcList;
    Property ServerName: String
      Read GetServerName; {!!.10}
    Property UserList: TfsUserList
      Read FUserList;
  End;

  {---Internal helper routines---}
Function srcfgCalcKPKey( Const aTable: TffFullFileName;
  aIndex: Integer ): TffShStr;
Function srcfgAliasFromKPKey( Const BKK: TffShStr ): TffName;
Function srcfgTableFromKPKey( Const BKK: TffShStr ): TfsTableName;
Function srcfgIndexFromKPKey( Const BKK: TffShStr ): Integer;

Implementation

{== Helper routines ==================================================}

Function srcfgCalcKPKey( Const aTable: TffFullFileName;
  aIndex: Integer )
  : TffShStr;
Var
  S: String[9];
Begin
  Str( aIndex, S );
  Result := aTable;
  FFShStrAddChar( Result, '|' );
  FFShStrConcat( Result, S );
End;
{--------}

Function srcfgAliasFromKPKey( Const BKK: TffShStr ): TffName;
Var
  PosSlash: Integer;
Begin
  PosSlash := Pos( '/', BKK );
  If ( PosSlash > 0 ) Then
    Result := Copy( BKK, 1, pred( PosSlash ) )
  Else
    Result := '';
End;
{--------}

Function srcfgTableFromKPKey( Const BKK: TffShStr ): TfsTableName;
Var
  PosColon: Integer;
  PosSlash: Integer;
Begin
  PosSlash := Pos( '/', BKK );
  PosColon := Pos( ':', BKK );
  If ( PosSlash > 0 ) And ( PosColon > 0 ) Then
    Result := Copy( BKK, succ( PosSlash ), pred( PosColon - PosSlash ) )
  Else
    Result := '';
End;
{--------}

Function srcfgIndexFromKPKey( Const BKK: TffShStr ): Integer;
Var
  PosColon: Integer;
  ec: Integer;
  InxAsStr: String[9];
Begin
  PosColon := Pos( ':', BKK );
  If ( PosColon > 0 ) Then
    Begin
      InxAsStr := Copy( BKK, succ( PosColon ), 255 );
      Val( InxAsStr, Result, ec );
      If ( ec <> 0 ) Then
        Result := 0;
    End
  Else
    Result := 0;
End;
{=====================================================================}

{== TfsAliasItem =====================================================}

Constructor TfsAliasItem.Create( Const aAlias: TffName;
  Const aPath: TffPath;
  aCheckDisk: Boolean = False;
  aPathBlob: TffPath = '' );
Begin
  Inherited Create( aAlias );
  FPath := FFShStrAlloc( FFExpandUNCFileName( aPath ) );
  FPathBlob:= FFShStrAlloc( FFExpandUNCFileName( aPathBlob ) );
  FCheckDisk := aCheckDisk;
End;
{--------}

Destructor TfsAliasItem.Destroy;
Begin
  FFShStrFree( FPath );
  FFShStrFree( FPathBlob );
  Inherited Destroy;
End;
{--------}

Function TfsAliasItem.GetAlias: String; {!!.10}
Begin
  Result := KeyAsStr;
End;
{--------}

Function TfsAliasItem.GetPath: String; {!!.10}
Begin
  Result := FPath^;
End;

Function TfsAliasItem.GetPathBlob: String; 
Begin
  Result := FPathBlob^;
End;
{====================================================================}

{===TfsAliasList=====================================================}

Procedure TfsAliasList.AddAlias( aAliasItem: TfsAliasItem );
Begin
  Insert( aAliasItem );
End;
{--------}

Function TfsAliasList.AliasExists( Const aAlias: TffName ): Boolean;
Begin
  Result := Exists( aAlias );
End;
{--------}

Function TfsAliasList.AliasIndex( Const aAlias: TffName ): Integer;
Begin
  Result := Index( aAlias );
End;
{--------}
{!!.11 - New}

Function TfsAliasList.CheckDiskSpace( Const aAlias: TffName )
  : Boolean;
Var
  Position: Integer;
Begin
  Result := False;
  Position := Index( aAlias );
  If ( Position > -1 ) Then
    Result := AliasItem[Position].CheckSpace;
End;
{--------}

Procedure TfsAliasList.DeleteAlias( Const aAlias: TffName );
Begin
  Delete( aAlias );
End;
{--------}

Function TfsAliasList.GetAliasItem( aInx: Integer ): TfsAliasItem;
Begin
  Result := TfsAliasItem( fflList[aInx] );
End;
{--------}

Function TfsAliasList.GetAliasPath( Const aAlias: TffName ): TffPath;
Var
  Inx: Integer;
Begin
  Inx := Index( aAlias );
  If ( Inx = -1 ) Then
    Result := ''
  Else
    Result := TfsAliasItem( fflList[Inx] ).Path;
End;
{====================================================================}

{===TfsUserItem======================================================}

Constructor TfsUserItem.Create( Const aUserID: TffName;
  Const aLastName: TffName;
  Const aFirstName: TffName;
  aPwdHash: TffWord32;
  aRights: TffUserRights);
Begin
  Inherited Create( aUserID );
  FFirst := FFShStrAlloc( aFirstName );
  FLast := FFShStrAlloc( aLastName );
  FPwdHash := aPwdHash;
  FRights := aRights;
End;
{--------}

Destructor TfsUserItem.Destroy;
Begin
  FFShStrFree( FFirst );
  FFShStrFree( FLast );
  Inherited Destroy;
End;
{--------}

Function TfsUserItem.GetFirstName: String; {!!.10}
Begin
  Result := FFirst^;
End;
{--------}

Function TfsUserItem.GetLastName: String; {!!.10}
Begin
  Result := FLast^;
End;

{--------}

Function TfsUserItem.GetUserID: String; {!!.10}
Begin
  Result := KeyAsStr;
End;
{====================================================================}

{===TfsUserList======================================================}

Constructor TfsUserList.Create;
Begin
  Inherited Create;
  FUserList := TFSNormalList.Create;
End;
{--------}

Destructor TfsUserList.Destroy;
Begin
  FUserList.Free;
  Inherited Destroy;
End;
{--------}

Procedure TfsUserList.AddUser( aUserItem: TfsUserItem );
Begin
  FUserList.Insert( aUserItem );
End;
{--------}

Function TfsUserList.UserExists( Const aUserID: TffName ): Boolean;
Begin
  Result := FUserList.Exists( aUserID );
End;
{--------}

Function TfsUserList.UserIndex( Const aUserID: TffName ): Integer;
Begin
  Result := FUserList.Index( aUserID );
End;
{--------}

Function TfsUserList.Count: Integer;
Begin
  Result := FUserList.Count;
End;
{--------}

Procedure TfsUserList.DeleteUser( Const aUserID: TffName );
Begin
  FUserList.Delete( aUserID );
End;
{--------}

Procedure TfsUserList.Empty;
Begin
  FUserList.Empty;
End;
{--------}

Function TfsUserList.GetUserItem( aInx: Integer ): TfsUserItem;
Begin
  Result := TfsUserItem( FUserList[aInx] );
End;
{--------}

Function TfsUserList.GetUserPwdHash( Const aUserID: TffName ): TffWord32;
Var
  Inx: Integer;
Begin
  If ( aUserID = '' ) Then
    Result := 0
  Else
    Begin
      Inx := FUserList.Index( aUserID );
      If ( Inx = -1 ) Then
        Result := $FFFFFFFF
      Else
        Result := TfsUserItem( FUserList[Inx] ).PasswordHash;
    End;
End;
{--------}

Function TfsUserList.GetUserRights( Const aUserID: TffName ): TffUserRights;
Var
  Inx: Integer;
Begin
  Inx := FUserList.Index( aUserID );
  If ( Inx = -1 ) Then
    Result := []
  Else
    Result := TfsUserItem( FUserList[Inx] ).Rights;
End;
{====================================================================}

{===TfsKeyProcItem===================================================}

Constructor TfsKeyProcItem.Create( Const aPath: TffPath;
  Const aTable: TfsTableName;
  aIndexID: Integer;
  Const aDLLName: TffFullFileName;
  Const aBuildName: TffName;
  Const aCompareName: TffName );
Var
  FFN: TffFullFileName;
  UNCPath: TffPath;
Begin
  UNCPath := FFExpandUNCFileName( aPath );
  FFN := FFMakeFullFileName( UNCPath, FFMakeFileNameExt( aTable, fsc_ExtForData ) );
  Inherited Create( srcfgCalcKPKey( FFN, aIndexID ) );
  FIndexID := aIndexID;
  FPath := FFShStrAlloc( UNCPath );
  FTable := FFShStrAlloc( aTable );
  FDLLName := FFShStrAlloc( aDLLName );
  FBuildName := FFShStrAlloc( aBuildName );
  FCompareName := FFShStrAlloc( aCompareName );
End;
{--------}

Destructor TfsKeyProcItem.Destroy;
Begin
  Unlink;
  FFShStrFree( FCompareName );
  FFShStrFree( FBuildName );
  FFShStrFree( FDLLName );
  FFShStrFree( FTable );
  FFShStrFree( FPath );
  Inherited Destroy;
End;
{--------}

Function TfsKeyProcItem.GetDLLName: String; {!!.10}
Begin
  Result := FDLLName^;
End;
{--------}

Function TfsKeyProcItem.GetBuildKeyName: String; {!!.10}
Begin
  Result := FBuildName^;
End;
{--------}

Function TfsKeyProcItem.GetCompareKeyName: String; {!!.10}
Begin
  Result := FCompareName^;
End;
{--------}

Function TfsKeyProcItem.GetPath: String; {!!.10}
Begin
  Result := FPath^;
End;
{--------}

Function TfsKeyProcItem.GetTable: String; {!!.10}
Begin
  Result := FTable^;
End;
{--------}

Function TfsKeyProcItem.GetTableDataFileName: String; {!!.10}
Begin
  Result := FFMakeFullFileName( Path, FFMakeFileNameExt( Table, fsc_ExtForData ) );
End;
{--------}

Function TfsKeyProcItem.Link: Boolean;
Var
  DLLPathZ: TffStringZ;
  ProcNameZ: TffStringZ;
Begin
  Result := False;
  Unlink;
  kpiLibHandle := LoadLibrary( FFStrPCopy( DLLPathZ, DLLName ) );
  If ( kpiLibHandle <> 0 ) Then
    Begin
      @FBuildKey := GetProcAddress( kpiLibHandle, FFStrPCopy( ProcNameZ, BuildKeyName ) );
      If Assigned( FBuildKey ) Then
        Begin
          @FCompareKey := GetProcAddress( kpiLibHandle, FFStrPCopy( ProcNameZ, CompareKeyName ) );
          If Assigned( FCompareKey ) Then
            Result := True
          Else
            Unlink;
        End
      Else
        Unlink;
    End;
End;
{--------}

Procedure TfsKeyProcItem.Unlink;
Begin
  If ( kpiLibHandle <> 0 ) Then
    FreeLibrary( kpiLibHandle );
  kpiLibHandle := 0;
  FBuildKey := Nil;
  FCompareKey := Nil;
End;
{====================================================================}

{===TfsKeyProcList===================================================}

Constructor TfsKeyProcList.Create;
Begin
  Inherited Create;
  FKPList := TFSNormalList.Create;
End;
{--------}

Destructor TfsKeyProcList.Destroy;
Begin
  FKPList.Free;
  Inherited Destroy;
End;
{--------}

Procedure TfsKeyProcList.AddKeyProc( aKeyProcItem: TfsKeyProcItem );
Begin
  FKPList.Insert( aKeyProcItem );
End;
{--------}

Function TfsKeyProcList.Count: Integer;
Begin
  Result := FKPList.Count;
End;
{--------}

Procedure TfsKeyProcList.DeleteKeyProc( Const aPath: TffPath;
  Const aTable: TfsTableName;
  aIndexID: Integer );
Var
  FFN: TffFullFileName;
  KPKey: TffShStr;
Begin
  FFN := FFMakeFullFileName(
    FFExpandUNCFileName( aPath ),
    FFMakeFileNameExt( aTable, fsc_ExtForData ) );
  KPKey := srcfgCalcKPKey( FFN, aIndexID );
  FKPList.Delete( KPKey );
End;
{--------}

Procedure TfsKeyProcList.Empty;
Begin
  FKPList.Empty;
End;
{--------}

Function TfsKeyProcList.GetKPItem( aInx: Integer ): TfsKeyProcItem;
Begin
  Result := TfsKeyProcItem( FKPList[aInx] );
End;
{--------}

Function TfsKeyProcList.KeyProcExists( Const aPath: TffPath;
  Const aTable: TfsTableName;
  aIndexID: Integer ): Boolean;
Var
  FFN: TffFullFileName;
  KPKey: TffShStr;
Begin
  FFN := FFMakeFullFileName(
    FFExpandUNCFileName( aPath ),
    FFMakeFileNameExt( aTable, fsc_ExtForData ) );
  KPKey := srcfgCalcKPKey( FFN, aIndexID );
  Result := ( FKPList.Index( KPKey ) <> -1 );
End;
{--------}

Function TfsKeyProcList.KeyProcIndex( Const aPath: TffPath;
  Const aTable: TfsTableName;
  aIndexID: Integer ): Integer;
Var
  FFN: TffFullFileName;
  KPKey: TffShStr;
Begin
  FFN := FFMakeFullFileName(
    FFExpandUNCFileName( aPath ),
    FFMakeFileNameExt( aTable, fsc_ExtForData ) );
  KPKey := srcfgCalcKPKey( FFN, aIndexID );
  Result := FKPList.Index( KPKey );
End;
{=====================================================================}

{== TfsServerConfiguration ===========================================}

Constructor TfsServerConfiguration.Create;
Begin
  Inherited Create; {!!.01}

  {set up the default general info}
  With FGeneralInfo Do
    Begin
      giServerName := '';
      giMaxRAM := 10;
      giSingleUser := True;
      giIPXSPX := False;
      giIPXSPXLFB := True;
      giTCPIP := True;
      giTCPIPLFB := True;
      giIsSecure := False;
      giAutoUp := True;
      giAutoMini := True;
      giDebugLog := False;
      giEncrypt := True;
      giReadOnly := False;
      giNoAutoSaveCfg := False;
      giLastMsgInterval := fsc_LastMsgInterval;
      giKAInterval := fsc_KeepAliveInterval;
      giKARetries := fsc_KeepAliveRetries;
      giPriority := 0; {THREAD_PRIORITY_NORMAL}
      giTCPPort := FSGetTCPPort;
      giUDPPortSr := FSGetUDPPortServer;
      giUDPPortCl := FSGetUDPPortClient;
      giIPXSocketSr := FSGetIPXSocketServer;
      giIPXSocketCl := FSGetIPXSocketClient;
      giSPXSocket := FSGetSPXSocket;
      giTempStoreSize := fscl_TempStorageSize;
      giCollectEnabled := False;
      giCollectFreq := fscl_CollectionFrequency;
      giEnabledTrigers := True;
      giEnabledProcedures := True;
      giEnabledReferential := True;
      giEnabledxx := True;
      giMaxClients := 0;
      giMaxDuplicateUsers := 0;
      giSendErrorIfLoginDuplicateUser := False;
      giMaxDbOpen := 0;
      giMaxSessionOpen := 0;
      giEncryptTempStorage := false;
    End;

  {create internal items}
  scPadLock := TfsPadlock.Create;
  FAliasList := TfsAliasList.Create;
  FKeyProcList := TfsKeyProcList.Create;
  FUserList := TfsUserList.Create;
End;
{--------}

Destructor TfsServerConfiguration.Destroy;
Begin
  FUserList.Free;
  FKeyProcList.Free;
  FAliasList.Free;
  scPadLock.Free;
  Inherited Destroy; {!!.01}
End;
{--------}

Procedure TfsServerConfiguration.AddAlias( Const aAlias: TffName;
  Const aPath: TffPath;
  aCheckDisk: Boolean = False;
  aPathBlob: TffPath = '');
Var
  NewAlias: TfsAliasItem;
Begin
  { Assumption: Thread-safeness enforced at a higher level. }
  NewAlias := TfsAliasItem.Create( aAlias, aPath, aCheckDisk, aPathBlob );
  Try
    FAliasList.AddAlias( NewAlias );
  Except
    NewAlias.Free;
    Raise;
  End; {try..except}
End;
{--------}

Procedure TfsServerConfiguration.AddKeyProc( Const aPath: TffPath;
  Const aTable: TfsTableName;
  aIndexID: Integer;
  Const aDLLName: TffFullFileName;
  Const aBuildName: TffName;
  Const aCompareName: TffName );
Var
  NewKeyProc: TfsKeyProcItem;
Begin
  { Assumption: Thread-safeness enforced at a higher level. }
  NewKeyProc := TfsKeyProcItem.Create( aPath, aTable, aIndexID,
    aDLLName, aBuildName, aCompareName );
  Try
    FKeyProcList.AddKeyProc( NewKeyProc )
  Except
    NewKeyProc.Free;
    Raise;
  End; {try..except}
End;
{--------}

Procedure TfsServerConfiguration.AddUser( Const aUserID: TffName;
  Const aLastName: TffName;
  Const aFirstName: TffName;
  aPwdHash: TffWord32;
  aRights: TffUserRights);
Var
  NewUser: TfsUserItem;
Begin
  { Assumption: Thread-safeness enforced at a higher level. }
  NewUser := TfsUserItem.Create( aUserID, aLastName, aFirstName, aPwdHash, aRights);
  Try
    FUserList.AddUser( NewUser )
  Except
    NewUser.Free;
    Raise;
  End; {try..except}
End;
{--------}

Function TfsServerConfiguration.GetGeneralInfo: PfsGeneralInfo;
Begin
  Result := @FGeneralInfo;
End;
{--------}

Function TfsServerConfiguration.GetServerName: String; {!!.10}
Begin
  Result := FGeneralInfo.giServerName;
End;
{--------}

Procedure TfsServerConfiguration.Lock;
Begin
  scPadLock.Lock;
End;
{--------}

Procedure TfsServerConfiguration.PerformDynamicLink;
Var
  i: Integer;
  lTable: TffFullFileName;
  lDLL: TffFullFileName;
  lBKName: TffName;
  lCKName: TffName;
Begin
  Lock;
  Try
    For i := 0 To pred( KeyProcList.Count ) Do
      Begin
        With KeyProcList[i] Do
          Begin
            If Not Link Then
              Begin
                lTable := TableDataFileName;
                lDLL := DLLName;
                lBKName := BuildKeyName;
                lCKName := CompareKeyName;
                FSRaiseException( EfsServerException, fsStrResServer, fserrDynamicLink,
                  [lTable, IndexID, lDLL, lBKName, lCKName] )
              End;
          End;
      End;
  Finally
    Unlock;
  End; {try..finally}
End;
{--------}

Procedure TfsServerConfiguration.UnLock;
Begin
  scPadLock.Unlock;
End;
{====================================================================}
End.

