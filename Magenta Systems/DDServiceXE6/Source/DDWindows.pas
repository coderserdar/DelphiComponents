{*****************************************************************************
 Portions created by Microsoft are Copyright (c) 1997-2001 Microsoft Corporation
 New stuff Windows.pas for SvcMgr.pas
 Pascal translation by Arno Garrels <arno.garrels@gmx.de>
*****************************************************************************}

unit DDWindows;

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}
{$I DDCompilers.inc}

interface

{$IFDEF COMPILER16_UP}
uses Winapi.Windows;
{$ELSE}
uses Windows;
{$ENDIF}

const

// Winnt.h
////////////////////////////////////////////////////////////////////////
//                                                                    //
//               NT Defined Privileges                                //
//                                                                    //
////////////////////////////////////////////////////////////////////////

  SE_CREATE_TOKEN_NAME              = 'SeCreateTokenPrivilege';
  {$EXTERNALSYM SE_CREATE_TOKEN_NAME}
  SE_ASSIGNPRIMARYTOKEN_NAME        = 'SeAssignPrimaryTokenPrivilege';
  {$EXTERNALSYM SE_ASSIGNPRIMARYTOKEN_NAME}
  SE_LOCK_MEMORY_NAME               = 'SeLockMemoryPrivilege';
  {$EXTERNALSYM SE_LOCK_MEMORY_NAME}
  SE_INCREASE_QUOTA_NAME            = 'SeIncreaseQuotaPrivilege';
  {$EXTERNALSYM SE_INCREASE_QUOTA_NAME}
  SE_UNSOLICITED_INPUT_NAME         = 'SeUnsolicitedInputPrivilege';
  {$EXTERNALSYM SE_UNSOLICITED_INPUT_NAME}
  SE_MACHINE_ACCOUNT_NAME           = 'SeMachineAccountPrivilege';
  {$EXTERNALSYM SE_MACHINE_ACCOUNT_NAME}
  SE_TCB_NAME                       = 'SeTcbPrivilege';
  {$EXTERNALSYM SE_TCB_NAME}
  SE_SECURITY_NAME                  = 'SeSecurityPrivilege';
  {$EXTERNALSYM SE_SECURITY_NAME}
  SE_TAKE_OWNERSHIP_NAME            = 'SeTakeOwnershipPrivilege';
  {$EXTERNALSYM SE_TAKE_OWNERSHIP_NAME}
  SE_LOAD_DRIVER_NAME               = 'SeLoadDriverPrivilege';
  {$EXTERNALSYM SE_LOAD_DRIVER_NAME}
  SE_SYSTEM_PROFILE_NAME            = 'SeSystemProfilePrivilege';
  {$EXTERNALSYM SE_SYSTEM_PROFILE_NAME}
  SE_SYSTEMTIME_NAME                = 'SeSystemtimePrivilege';
  {$EXTERNALSYM SE_SYSTEMTIME_NAME}
  SE_PROF_SINGLE_PROCESS_NAME       = 'SeProfileSingleProcessPrivilege';
  {$EXTERNALSYM SE_PROF_SINGLE_PROCESS_NAME}
  SE_INC_BASE_PRIORITY_NAME         = 'SeIncreaseBasePriorityPrivilege';
  {$EXTERNALSYM SE_INC_BASE_PRIORITY_NAME}
  SE_CREATE_PAGEFILE_NAME           = 'SeCreatePagefilePrivilege';
  {$EXTERNALSYM SE_CREATE_PAGEFILE_NAME}
  SE_CREATE_PERMANENT_NAME          = 'SeCreatePermanentPrivilege';
  {$EXTERNALSYM SE_CREATE_PERMANENT_NAME}
  SE_BACKUP_NAME                    = 'SeBackupPrivilege';
  {$EXTERNALSYM SE_BACKUP_NAME}
  SE_RESTORE_NAME                   = 'SeRestorePrivilege';
  {$EXTERNALSYM SE_RESTORE_NAME}
  SE_SHUTDOWN_NAME                  = 'SeShutdownPrivilege';
  {$EXTERNALSYM SE_SHUTDOWN_NAME}
  SE_DEBUG_NAME                     = 'SeDebugPrivilege';
  {$EXTERNALSYM SE_DEBUG_NAME}
  SE_AUDIT_NAME                     = 'SeAuditPrivilege';
  {$EXTERNALSYM SE_AUDIT_NAME}
  SE_SYSTEM_ENVIRONMENT_NAME        = 'SeSystemEnvironmentPrivilege';
  {$EXTERNALSYM SE_SYSTEM_ENVIRONMENT_NAME}
  SE_CHANGE_NOTIFY_NAME             = 'SeChangeNotifyPrivilege';
  {$EXTERNALSYM SE_CHANGE_NOTIFY_NAME}
  SE_REMOTE_SHUTDOWN_NAME           = 'SeRemoteShutdownPrivilege';
  {$EXTERNALSYM SE_REMOTE_SHUTDOWN_NAME}
  SE_UNDOCK_NAME                    = 'SeUndockPrivilege';
  {$EXTERNALSYM SE_UNDOCK_NAME}
  SE_SYNC_AGENT_NAME                = 'SeSyncAgentPrivilege';
  {$EXTERNALSYM SE_SYNC_AGENT_NAME}
  SE_ENABLE_DELEGATION_NAME         = 'SeEnableDelegationPrivilege';
  {$EXTERNALSYM SE_ENABLE_DELEGATION_NAME}
  SE_MANAGE_VOLUME_NAME             = 'SeManageVolumePrivilege';
  {$EXTERNALSYM SE_MANAGE_VOLUME_NAME}
  SE_IMPERSONATE_NAME               = 'SeImpersonatePrivilege';
  {$EXTERNALSYM SE_IMPERSONATE_NAME}
  SE_CREATE_GLOBAL_NAME             = 'SeCreateGlobalPrivilege';
  {$EXTERNALSYM SE_CREATE_GLOBAL_NAME}
  SE_TRUSTED_CREDMAN_ACCESS_NAME    = 'SeTrustedCredManAccessPrivilege';
  {$EXTERNALSYM SE_TRUSTED_CREDMAN_ACCESS_NAME}
  SE_RELABEL_NAME                   = 'SeRelabelPrivilege';
  {$EXTERNALSYM SE_RELABEL_NAME}
  SE_INC_WORKING_SET_NAME           = 'SeIncreaseWorkingSetPrivilege';
  {$EXTERNALSYM SE_INC_WORKING_SET_NAME}
  SE_TIME_ZONE_NAME                 = 'SeTimeZonePrivilege';
  {$EXTERNALSYM SE_TIME_ZONE_NAME}
  SE_CREATE_SYMBOLIC_LINK_NAME      = 'SeCreateSymbolicLinkPrivilege';
  {$EXTERNALSYM SE_CREATE_SYMBOLIC_LINK_NAME}

// WinUser.h
// W2K
  WM_POWERBROADCAST                 = $0218;
  {$EXTERNALSYM WM_POWERBROADCAST}
  PBT_APMQUERYSUSPEND               = $0000;
  {$EXTERNALSYM PBT_APMQUERYSUSPEND}
  PBT_APMQUERYSTANDBY               = $0001;
  {$EXTERNALSYM PBT_APMQUERYSTANDBY}
  PBT_APMQUERYSUSPENDFAILED         = $0002;
  {$EXTERNALSYM PBT_APMQUERYSUSPENDFAILED}
  PBT_APMQUERYSTANDBYFAILED         = $0003;
  {$EXTERNALSYM PBT_APMQUERYSTANDBYFAILED}
  PBT_APMSUSPEND                    = $0004;
  {$EXTERNALSYM PBT_APMSUSPEND}
  PBT_APMSTANDBY                    = $0005;
  {$EXTERNALSYM PBT_APMSTANDBY}
  PBT_APMRESUMECRITICAL             = $0006; // removed in Vista an Server 2008
  {$EXTERNALSYM PBT_APMRESUMECRITICAL}
  PBT_APMRESUMESUSPEND              = $0007;
  {$EXTERNALSYM PBT_APMRESUMESUSPEND}
  PBT_APMRESUMESTANDBY              = $0008;
  {$EXTERNALSYM PBT_APMRESUMESTANDBY}
  PBTF_APMRESUMEFROMFAILURE         = $00000001;
  {$EXTERNALSYM PBTF_APMRESUMEFROMFAILURE}
  PBT_APMBATTERYLOW                 = $0009;
  {$EXTERNALSYM PBT_APMBATTERYLOW}
  PBT_APMPOWERSTATUSCHANGE          = $000A;
  {$EXTERNALSYM PBT_APMPOWERSTATUSCHANGE}
  PBT_APMOEMEVENT                   = $000B;
  {$EXTERNALSYM PBT_APMOEMEVENT}
  PBT_APMRESUMEAUTOMATIC            = $0012;
  {$EXTERNALSYM PBT_APMRESUMEAUTOMATIC}

//Vista
  PBT_POWERSETTINGCHANGE            = $8013;
  {$IFDEF SDK_HEADER_VISTA} {$EXTERNALSYM PBT_POWERSETTINGCHANGE} {$ENDIF}

type
// WinUser.h
  PPowerBroadcastSetting = ^TPowerBroadcastSetting;  //Vista
  {$IFDEF SDK_HEADER_VISTA} {$EXTERNALSYM POWERBROADCAST_SETTING} {$ENDIF}
  POWERBROADCAST_SETTING = record
    PowerSetting    : TGUID;
    DataLength      : DWORD;
    Data            : array[0..0] of AnsiChar;
  end;
  TPowerBroadcastSetting = POWERBROADCAST_SETTING;

// WinUser.h
//
// WTSSESSION_NOTIFICATION struct pointed by lParam, for WM_WTSSESSION_CHANGE//
//

  PWtsSessionNotification = ^TWtsSessionNotification; // XP
  {$IFDEF COMPILER11_UP} {$EXTERNALSYM tagWTSSESSION_NOTIFICATION} {$ENDIF}
  tagWTSSESSION_NOTIFICATION = record
    cbSize      : DWORD;
    dwSessionId : DWORD;
  end;
  {$IFDEF COMPILER11_UP} {$EXTERNALSYM WTSSESSION_NOTIFICATION}{$ENDIF}
  WTSSESSION_NOTIFICATION = tagWTSSESSION_NOTIFICATION;
  TWtsSessionNotification = tagWTSSESSION_NOTIFICATION;

const
// WinUser.h
//
// codes passed in WPARAM for WM_WTSSESSION_CHANGE // XP
// 
  WTS_CONSOLE_CONNECT               = $1;
  {$EXTERNALSYM WTS_CONSOLE_CONNECT}
  WTS_CONSOLE_DISCONNECT            = $2;
  {$EXTERNALSYM WTS_CONSOLE_DISCONNECT}
  WTS_REMOTE_CONNECT                = $3;
  {$EXTERNALSYM WTS_REMOTE_CONNECT}
  WTS_REMOTE_DISCONNECT             = $4;
  {$EXTERNALSYM WTS_REMOTE_DISCONNECT}
  WTS_SESSION_LOGON                 = $5;
  {$EXTERNALSYM WTS_SESSION_LOGON}
  WTS_SESSION_LOGOFF                = $6;
  {$EXTERNALSYM WTS_SESSION_LOGOFF}
  WTS_SESSION_LOCK                  = $7;
  {$EXTERNALSYM WTS_SESSION_LOCK}
  WTS_SESSION_UNLOCK                = $8;
  {$EXTERNALSYM WTS_SESSION_UNLOCK}
  WTS_SESSION_REMOTE_CONTROL        = $9;
  {$IFDEF COMPILER11_UP} {$EXTERNALSYM WTS_SESSION_REMOTE_CONTROL}{$ENDIF}

type
//  
// Winnt.h
//
  _TOKEN_INFORMATION_CLASS = (    
    TokenUser = 1,
    TokenGroups,
    TokenPrivileges,
    TokenOwner,
    TokenPrimaryGroup,
    TokenDefaultDacl,
    TokenSource,
    TokenType,
    TokenImpersonationLevel,
    TokenStatistics,
    TokenRestrictedSids,
    TokenSessionId,
    TokenGroupsAndPrivileges,
    TokenSessionReference,
    TokenSandBoxInert,
    TokenAuditPolicy,
{ Server 2003 }
    TokenOrigin,     
{ Vista and better }
    TokenElevationType,
    TokenLinkedToken,
    TokenElevation,
    TokenHasRestrictions,
    TokenAccessInformation,
    TokenVirtualizationAllowed,
    TokenVirtualizationEnabled,
    TokenIntegrityLevel,
    TokenUIAccess,
    TokenMandatoryPolicy,
    TokenLogonSid,
    MaxTokenInfoClass  // MaxTokenInfoClass should always be the last enum
);

 {$EXTERNALSYM _TOKEN_INFORMATION_CLASS}
  TOKEN_INFORMATION_CLASS = _TOKEN_INFORMATION_CLASS;
  {$EXTERNALSYM TOKEN_INFORMATION_CLASS}
  PTOKEN_INFORMATION_CLASS = ^TOKEN_INFORMATION_CLASS;
  {$EXTERNALSYM PTOKEN_INFORMATION_CLASS}

  TTokenInformationClass = TOKEN_INFORMATION_CLASS;
  PTokenInformationClass = PTOKEN_INFORMATION_CLASS;

function SetTokenInformation(TokenHandle: THandle;
  TokenInformationClass: TTokenInformationClass; TokenInformation: Pointer;
  TokenInformationLength: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetTokenInformation}

//
// UserEnv.h
//
//=============================================================================
//
// CreateEnvironmentBlock
//
// Returns the environment variables for the specified user.  This block
// can then be passed to CreateProcessAsUser().
//
// lpEnvironment  -  Receives a pointer to the new environment block
// hToken         -  User's token returned from LogonUser() (optional, can be NULL)
// bInherit       -  Inherit from the current process's environment block
//                   or start from a clean state.
//
// Returns:  TRUE if successful
//           FALSE if not.  Call GetLastError() for more details
//
// Note:     If hToken is NULL, the returned environment block will contain
//           system variables only.
//
//           Call DestroyEnvironmentBlock to free the buffer when finished.
//
//           If this block is passed to CreateProcessAsUser, the
//           CREATE_UNICODE_ENVIRONMENT flag must also be set.
//
//=============================================================================
// Minimum operating systems	Windows NT 4.0
function CreateEnvironmentBlock(var lpEnvironment: Pointer; hToken: THandle;
  bInherit: BOOL): BOOL; stdcall;
{$EXTERNALSYM CreateEnvironmentBlock}

//=============================================================================
//
// DestroyEnvironmentBlock
//
// Frees environment variables created by CreateEnvironmentBlock
//
// lpEnvironment  -  A pointer to the environment block
//
// Returns:  TRUE if successful
//           FALSE if not.  Call GetLastError() for more details
//
//=============================================================================
// Minimum operating systems	Windows NT 4.0
function DestroyEnvironmentBlock(lpEnvironment: Pointer): BOOL; stdcall;
{$EXTERNALSYM DestroyEnvironmentBlock}

function WTSGetActiveConsoleSessionId: DWORD;
{$EXTERNALSYM WTSGetActiveConsoleSessionId}

const
  userenv = 'userenv.dll';

implementation

function SetTokenInformation; external advapi32 name 'SetTokenInformation';
function CreateEnvironmentBlock; external userenv name 'CreateEnvironmentBlock';
function DestroyEnvironmentBlock; external userenv name 'DestroyEnvironmentBlock';

type
// WinBase.h
  TWTSGetActiveConsoleSessionId = function: DWORD; stdcall;

var
  _WTSGetActiveConsoleSessionId  : TWTSGetActiveConsoleSessionId = nil;
  hKernel32                      : THandle = 0;

function WTSGetActiveConsoleSessionId: DWORD;
begin
    if @_WTSGetActiveConsoleSessionId = nil then
    begin
        if hKernel32 = 0 then
            hKernel32 := GetModuleHandle(PChar(Kernel32));
        @_WTSGetActiveConsoleSessionId := GetProcAddress(hKernel32, 'WTSGetActiveConsoleSessionId');
        if @_WTSGetActiveConsoleSessionId = nil then
            Result := MAXDWORD
        else
            Result := _WTSGetActiveConsoleSessionId;
    end
    else
        Result := _WTSGetActiveConsoleSessionId;
end;

end.
