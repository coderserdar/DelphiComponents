{
=======================================================================

		KLIB v100
		Serious Software Made in Brazil


		home-page: www.knowhow-online.com.br (sorry, just portuguese)
		authors: Demian Lessa (demian@knowhow-online.com.br) and Leonardo Freitas

		Released under the Netscape Public License Version 1.0
	 (see license.txt)

		Unless otherwise noted, all materials provided in this release
		are copyright © 2001 by KnowHow Informatica Ltda.

=======================================================================
}

unit ukwUtils;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Windows, WinSVC, Classes, uksyUtils;

{
--------------------------------------------------------------------------------
---------------------------- Windows NT Services API ---------------------------
--------------------------------------------------------------------------------
}

type

	EKWUtils = class( EKWinAPI );

	TKServiceControl = ( scStop, scPause, scContinue, scInterrogate, scShutDown );
	TKServiceControls = set of TKServiceControl;

	TKServiceState = ( ssStopped, ssStartPending, ssStopPending,
		ssRunning, ssContinuePending, ssPausePending, ssPaused );

	TKControlAccepted = ( caStop, caPauseContinue, caShutDown );
	TKControlsAccepted = set of TKControlAccepted;

	TKManagerAccess = ( maConnect, maCreateService, maEnumerateService,
		maLock, maQueryLockStatus, maModifyBootConfig, maStandardRightsReq );
	TKManagerAccessMask = set of TKManagerAccess;

	TKServiceAccess = ( saQueryConfig, saChangeConfig, saQueryStatus,
		saEnumerateDependents, saServiceStart, saServiceStop, saPauseContinue,
		saInterrogate, saUserDefinedControl, saStandardRightsReq );
	TKServiceAccessMask = set of TKServiceAccess;

	TKServiceStart = ( ssBoot, ssSystem, ssAuto, ssDemand, ssDisabled );

	TKServiceError = ( seIgnore, seNormal, seSevere, seCritical );

	TKServiceType = ( stKernelDriver, stFileSystemDriver, stAdapter,
		stRecognizerDriver, stWin32OwnProcess, stWin32ShareProcess,
		stInteractiveProcess );

	TKServiceTypes = set of TKServiceType;

const

{ Service Start Types }
	SERVICE_BOOT_START     =   $00000000;
	SERVICE_SYSTEM_START   =   $00000001;
	SERVICE_AUTO_START     =   $00000002;
	SERVICE_DEMAND_START   =   $00000003;
	SERVICE_DISABLED       =   $00000004;

{ Service Error Types }
	SERVICE_ERROR_IGNORE      =   $00000000;
	SERVICE_ERROR_NORMAL      =   $00000001;
	SERVICE_ERROR_SEVERE      =   $00000002;
	SERVICE_ERROR_CRITICAL    =   $00000003;

{ Service Types }
	SERVICE_KERNEL_DRIVER          = $00000001;
	SERVICE_FILE_SYSTEM_DRIVER     = $00000002;
	SERVICE_ADAPTER                = $00000004;
	SERVICE_RECOGNIZER_DRIVER      = $00000008;

	SERVICE_DRIVER                 = SERVICE_KERNEL_DRIVER or
																	 SERVICE_FILE_SYSTEM_DRIVER or
																	 SERVICE_RECOGNIZER_DRIVER;

	SERVICE_WIN32_OWN_PROCESS      = $00000010;
	SERVICE_WIN32_SHARE_PROCESS    = $00000020;
	SERVICE_WIN32                  = SERVICE_WIN32_OWN_PROCESS or
																	 SERVICE_WIN32_SHARE_PROCESS;

	SERVICE_INTERACTIVE_PROCESS    = $00000100;

	SERVICE_TYPE_ALL               = SERVICE_WIN32  or
																	 SERVICE_ADAPTER or
																	 SERVICE_DRIVER  or
																	 SERVICE_INTERACTIVE_PROCESS;

	SERVICE_CONTROL: array[TKServiceControl] of DWORD =
	(
		SERVICE_CONTROL_STOP,
		SERVICE_CONTROL_PAUSE,
		SERVICE_CONTROL_CONTINUE,
		SERVICE_CONTROL_INTERROGATE,
		SERVICE_CONTROL_SHUTDOWN
	);

	SERVICE_STATE: array[TKServiceState] of DWORD =
	(
		SERVICE_STOPPED,
		SERVICE_START_PENDING,
		SERVICE_STOP_PENDING,
		SERVICE_RUNNING,
		SERVICE_CONTINUE_PENDING,
		SERVICE_PAUSE_PENDING,
		SERVICE_PAUSED
	);

	SERVICE_ACCEPT: array[TKControlAccepted] of DWORD =
	(
		SERVICE_ACCEPT_STOP,
		SERVICE_ACCEPT_PAUSE_CONTINUE,
		SERVICE_ACCEPT_SHUTDOWN
	);

	SERVICE_TYPE: array[TKServiceType] of DWORD =
	(
		SERVICE_KERNEL_DRIVER,
		SERVICE_FILE_SYSTEM_DRIVER,
		SERVICE_ADAPTER,
		SERVICE_RECOGNIZER_DRIVER,
		SERVICE_WIN32_OWN_PROCESS,
		SERVICE_WIN32_SHARE_PROCESS,
		SERVICE_INTERACTIVE_PROCESS
	);

	START_TYPE: array[TKServiceStart] of DWORD =
	(
		SERVICE_BOOT_START,
		SERVICE_SYSTEM_START,
		SERVICE_AUTO_START,
		SERVICE_DEMAND_START,
		SERVICE_DISABLED
	);

	SERVICE_ERROR: array[TKServiceError] of DWORD =
	(
		SERVICE_ERROR_IGNORE,
		SERVICE_ERROR_NORMAL,
		SERVICE_ERROR_SEVERE,
		SERVICE_ERROR_CRITICAL
	);

	MANAGER_ACCESS: array[TKManagerAccess] of DWORD =
	(
		SC_MANAGER_CONNECT,
		SC_MANAGER_CREATE_SERVICE,
		SC_MANAGER_ENUMERATE_SERVICE,
		SC_MANAGER_LOCK,
		SC_MANAGER_QUERY_LOCK_STATUS,
		SC_MANAGER_MODIFY_BOOT_CONFIG,
		STANDARD_RIGHTS_REQUIRED
	);

	SERVICE_ACCESS: array[TKServiceAccess] of DWORD =
	(
		SERVICE_QUERY_CONFIG,
		SERVICE_CHANGE_CONFIG,
		SERVICE_QUERY_STATUS,
		SERVICE_ENUMERATE_DEPENDENTS,
		SERVICE_START,
		SERVICE_STOP,
		SERVICE_PAUSE_CONTINUE,
		SERVICE_INTERROGATE,
		SERVICE_USER_DEFINED_CONTROL,
		STANDARD_RIGHTS_REQUIRED
	);

function EnumServicesStatus( hSCManager: SC_HANDLE; dwServiceType,
	dwServiceState: DWORD; plpServices: PEnumServiceStatus;
	cbBufSize: DWORD; var pcbBytesNeeded, lpServicesReturned,
	lpResumeHandle: DWORD ): BOOL; stdcall;

function EnumDependentServices( hService: SC_HANDLE; dwServiceState: DWORD;
	plpServices: PEnumServiceStatus; cbBufSize: DWORD; var pcbBytesNeeded,
	lpServicesReturned : DWORD): BOOL; stdcall;

function QueryServiceConfig( hService: SC_HANDLE;
	plpServiceConfig: PQueryServiceConfig; cbBufSize: DWORD;
	var pcbBytesNeeded: DWORD ): BOOL; stdcall;

{ ServiceError functions }

function ServiceErrorDWORD( Value: TKServiceError ): DWORD;
function ServiceErrorEnum( Value: DWORD ): TKServiceError;

{ ServiceControl functions }

function ServiceControlEnum( Value: DWORD ): TKServiceControl;
function ServiceControlSet( Value: DWORD ): TKServiceControls;
function ServiceControlDWORD( Value: TKServiceControl ): DWORD;
function ServiceControlDWORDFromSet( Value: TKServiceControls ): DWORD;

{ ServiceState functions }

function ServiceStateEnum( Value: DWORD ): TKServiceState;
function ServiceStateDWORD( Value: TKServiceState ): DWORD;

{ ControlAccept functions }

function ControlAcceptEnum( Value: DWORD ): TKControlAccepted;
function ControlAcceptSet( Value: DWORD ): TKControlsAccepted;
function ControlAcceptDWORD( Value: TKControlAccepted ): DWORD;
function ControlAcceptDWORDFromSet( Value: TKControlsAccepted ): DWORD;

{ ManagerAccess functions }

function ManagerAccessEnum( Value: DWORD ): TKManagerAccess;
function ManagerAccessSet( Value: DWORD ): TKManagerAccessMask;
function ManagerAccessDWORD( Value: TKManagerAccess ): DWORD;
function ManagerAccessDWORDFromSet( Value: TKManagerAccessMask ): DWORD;

{ ServiceAccess functions }

function ServiceAccessEnum( Value: DWORD ): TKServiceAccess;
function ServiceAccessSet( Value: DWORD ): TKServiceAccessMask;
function ServiceAccessDWORD( Value: TKServiceAccess ): DWORD;
function ServiceAccessDWORDFromSet( Value: TKServiceAccessMask ): DWORD;

{ ServiceType functions }

function ServiceTypeEnum( Value: DWORD ): TKServiceType;
function ServiceTypeSet( Value: DWORD ): TKServiceTypes;
function ServiceTypeDWORD( Value: TKServiceType ): DWORD;
function ServiceTypeDWORDFromSet( Value: TKServiceTypes ): DWORD;


{ ServiceStart functions }

function ServiceStartEnum( Value: DWORD ): TKServiceStart;
function ServiceStartDWORD( Value: TKServiceStart ): DWORD;

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

function IsWinAPI_Shareware: Boolean;

{##NI##}

{
--------------------------------------------------------------------------------
--------------------- Validating Registration Information ----------------------
--------------------------------------------------------------------------------
}

function PackageUserName: string;
function PackageCompanyName: string;
function PackageVersion: TKLibVersion;

implementation

uses
	SysUtils, uksyConsts, uksyTypes, uksyPackReg, ukwConsts, ukwPackReg;

{
--------------------------------------------------------------------------------
---------------------------- Windows NT Services API ---------------------------
--------------------------------------------------------------------------------
}

{ Functions that were incorrected imported into WinSVC.pas }

const
	advapi32 = 'advapi32.dll';

function EnumServicesStatus; external advapi32 name 'EnumServicesStatusA';
function EnumDependentServices; external advapi32 name 'EnumDependentServicesA';
function QueryServiceConfig; external advapi32 name 'QueryServiceConfigA';

{ ServiceControl functions }

function ServiceControlEnum( Value: DWORD ): TKServiceControl;
var
	i: TKServiceControl;
begin
	Result := scStop;
	for i := Low( TKServiceControl ) to High( TKServiceControl ) do
		if ( SERVICE_CONTROL[i] = Value ) then
		begin
			Result := i;
			Exit;
		end;
end;

function ServiceControlSet( Value: DWORD ): TKServiceControls;
var
	i: TKServiceControl;
begin
	Result := [];
	for i := Low( TKServiceControl ) to High( TKServiceControl ) do
		if ( ( SERVICE_CONTROL[i] and Value ) = SERVICE_CONTROL[i] ) then
			Include( Result, i );
end;

function ServiceControlDWORD( Value: TKServiceControl ): DWORD;
begin
	Result := SERVICE_CONTROL[Value];
end;

function ServiceControlDWORDFromSet( Value: TKServiceControls ): DWORD;
var
	i: TKServiceControl;
begin
	Result := 0;
	for i := Low( TKServiceControl ) to High( TKServiceControl ) do
		if ( i in Value  ) then
			Result := Result or SERVICE_CONTROL[i];
end;

{ ServiceState functions }

function ServiceStateEnum( Value: DWORD ): TKServiceState;
var
	i: TKServiceState;
begin
	Result := ssStopped;
	for i := Low( TKServiceState ) to High( TKServiceState ) do
		if ( SERVICE_STATE[i] = Value ) then
		begin
			Result := i;
			Exit;
		end;
end;

function ServiceStateDWORD( Value: TKServiceState ): DWORD;
begin
	Result := SERVICE_STATE[Value];
end;

{ ControlAccept functions }

function ControlAcceptEnum( Value: DWORD ): TKControlAccepted;
var
	i: TKControlAccepted;
begin
	Result := caStop;
	for i := Low( TKControlAccepted ) to High( TKControlAccepted ) do
		if ( SERVICE_ACCEPT[i] = Value ) then
		begin
			Result := i;
			Exit;
		end;
end;

function ControlAcceptSet( Value: DWORD ): TKControlsAccepted;
var
	i: TKControlAccepted;
begin
	Result := [];
	for i := Low( TKControlAccepted ) to High( TKControlAccepted ) do
		if ( ( SERVICE_ACCEPT[i] and Value ) = SERVICE_ACCEPT[i] ) then
			Include( Result, i );
end;

function ControlAcceptDWORD( Value: TKControlAccepted ): DWORD;
begin
	Result := SERVICE_ACCEPT[Value];
end;

function ControlAcceptDWORDFromSet( Value: TKControlsAccepted ): DWORD;
var
	i: TKControlAccepted;
begin
	Result := 0;
	for i := Low( TKControlAccepted ) to High( TKControlAccepted ) do
		if ( i in Value  ) then
			Result := Result or SERVICE_ACCEPT[i];
end;

{ ManagerAccess functions }

function ManagerAccessEnum( Value: DWORD ): TKManagerAccess;
var
	i: TKManagerAccess;
begin
	Result := maConnect;
	for i := Low( TKManagerAccess ) to High( TKManagerAccess ) do
		if ( MANAGER_ACCESS[i] = Value ) then
		begin
			Result := i;
			Exit;
		end;
end;

function ManagerAccessSet( Value: DWORD ): TKManagerAccessMask;
var
	i: TKManagerAccess;
begin
	Result := [];
	for i := Low( TKManagerAccess ) to High( TKManagerAccess ) do
		if ( ( MANAGER_ACCESS[i] and Value ) = MANAGER_ACCESS[i] ) then
			Include( Result, i );
end;

function ManagerAccessDWORD( Value: TKManagerAccess ): DWORD;
begin
	Result := MANAGER_ACCESS[Value];
end;

function ManagerAccessDWORDFromSet( Value: TKManagerAccessMask ): DWORD;
var
	i: TKManagerAccess;
begin
	Result := 0;
	for i := Low( TKManagerAccess ) to High( TKManagerAccess ) do
		if ( i in Value  ) then
			Result := Result or MANAGER_ACCESS[i];
end;

{ ServiceAccess functions }

function ServiceAccessEnum( Value: DWORD ): TKServiceAccess;
var
	i: TKServiceAccess;
begin
	Result := saQueryConfig;
	for i := Low( TKServiceAccess ) to High( TKServiceAccess ) do
		if ( SERVICE_ACCESS[i] = Value ) then
		begin
			Result := i;
			Exit;
		end;
end;

function ServiceAccessSet( Value: DWORD ): TKServiceAccessMask;
var
	i: TKServiceAccess;
begin
	Result := [];
	for i := Low( TKServiceAccess ) to High( TKServiceAccess ) do
		if ( ( SERVICE_ACCESS[i] and Value ) = SERVICE_ACCESS[i] ) then
			Include( Result, i );
end;

function ServiceAccessDWORD( Value: TKServiceAccess ): DWORD;
begin
	Result := SERVICE_ACCESS[Value];
end;

function ServiceAccessDWORDFromSet( Value: TKServiceAccessMask ): DWORD;
var
	i: TKServiceAccess;
begin
	Result := 0;
	for i := Low( TKServiceAccess ) to High( TKServiceAccess ) do
		if ( i in Value  ) then
			Result := Result or SERVICE_ACCESS[i];
end;

{ ServiceType functions }

function ServiceTypeEnum( Value: DWORD ): TKServiceType;
var
	i: TKServiceType;
begin
	Result := stKernelDriver;
	for i := Low( TKServiceType ) to High( TKServiceType ) do
		if ( SERVICE_TYPE[i] = Value ) then
		begin
			Result := i;
			Exit;
		end;
end;

function ServiceTypeSet( Value: DWORD ): TKServiceTypes;
var
	i: TKServiceType;
begin
	Result := [];
	if ( Value <> SERVICE_NO_CHANGE ) then
		for i := Low( TKServiceType ) to High( TKServiceType ) do
			if ( ( SERVICE_TYPE[i] and Value ) = SERVICE_TYPE[i] ) then
	  		Include( Result, i );
end;

function ServiceTypeDWORD( Value: TKServiceType ): DWORD;
begin
	Result := SERVICE_TYPE[Value]
end;

function ServiceTypeDWORDFromSet( Value: TKServiceTypes ): DWORD;
var
	i: TKServiceType;
begin
	Result := 0;
	if ( Value = [] ) then
	  Result := SERVICE_NO_CHANGE
	else
		for i := Low( TKServiceType ) to High( TKServiceType ) do
			if ( i in Value  ) then
				Result := Result or SERVICE_TYPE[i];
end;

{ ServiceStart functions }

function ServiceStartEnum( Value: DWORD ): TKServiceStart;
var
	i: TKServiceStart;
begin
	Result := ssBoot;
	for i := Low( TKServiceStart ) to High( TKServiceStart ) do
		if ( START_TYPE[i] = Value ) then
		begin
			Result := i;
			Exit;
		end;
end;

function ServiceStartDWORD( Value: TKServiceStart ): DWORD;
begin
	Result := START_TYPE[Value];
end;

{ ServiceError functions }

function ServiceErrorEnum( Value: DWORD ): TKServiceError;
var
	i: TKServiceError;
begin
	Result := seIgnore;
	for i := Low( TKServiceError ) to High( TKServiceError ) do
		if ( SERVICE_ERROR[i] = Value ) then
		begin
			Result := i;
			Exit;
		end;
end;

function ServiceErrorDWORD( Value: TKServiceError ): DWORD;
begin
	Result := SERVICE_ERROR[Value];
end;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type

	TSignature	 = TUserName;
	TKey				 = TUserName;

{$IFNDEF INTERNAL_VERSION}

	PKInstallInfo = ^TKInstallInfo;
	TKInstallInfo = record
		Signature: TSignature;
		Key: TKey;
	end;

const

(* KLIB100_REGISTRY_SIGNATURE = '{09536FA0-BF69-11D2-B212-00C0DFE081C4}' *)

	KnowHowInstallInfo: TKInstallInfo =
	(
{$IFDEF KLIB100}
		Signature: '{09536FA0-BF69-11D2-B212-00C0DFE081C4}'; { do not resource/const }
{$ELSE}
		Signature: *MUST GENERATE AN ERROR!*;
{$ENDIF}
		Key:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32;
	);

{$ENDIF}

function IsWinAPI_Shareware: Boolean;
begin
{$IFDEF INTERNAL_VERSION}
	Result := false;
{$ELSE}
	Result := ( not CheckRegistryInfo( GetWinAPIRegistryInfo,
		LongInt( @KnowHowInstallInfo ) - SizeOf( TKInstallInfo ) ) );
{$ENDIF}
end;

procedure RegisterWinAPIUnits;
begin
	RegisterRunningPackage( perWinAPI, $54F3DE2E ); { do not const... }
	RegisterRunningPackage( pedWinAPI, $77D4C86B );
end;

procedure UnregisterWinAPIUnits;
begin
	UnregisterRunningPackage( perWinAPI, $54F3DE2E ); { do not const... }
	UnregisterRunningPackage( pedWinAPI, $77D4C86B );
end;

{
--------------------------------------------------------------------------------
--------------------- Validating Registration Information ----------------------
--------------------------------------------------------------------------------
}

type

	PKRegistryInfo = ^TKRegistryInfo;
	TKRegistryInfo = record
		Signature: TSignature;
		Key: TKey;
		UserName: TUserName;
		Company: TCompanyName;
	end;

function PackageUserName: string;
begin
	Result := Trim( PKRegistryInfo( GetWinAPIRegistryInfo + SizeOf( TKRegistryInfo ) ).UserName );
end;

function PackageCompanyName: string;
begin
	Result := Trim( PKRegistryInfo( GetWinAPIRegistryInfo + SizeOf( TKRegistryInfo ) ).Company );
end;

function PackageVersion: TKLibVersion;
begin
	ZeroMemory( @Result, SizeOf( TKLIBVersion ) );
	Result.Release := StrToDateTime( WINAPI_VER_RELEASE_DATE );
	Result.Version := WINAPI_VER;
	Result.Reserved := WINAPI_VER_INT;
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
  RegisterWinAPIUnits;
	TestWinAPIShareWareVersion;
	CreateRegCheckerThread( perWinAPI );
end;

procedure Done;
begin
	UnregisterWinAPIUnits;
end;

initialization
	Init;

finalization
	Done;

end.
