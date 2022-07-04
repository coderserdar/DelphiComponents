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

unit uksyPackReg;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}
{
--------------------------------------------------------------------------------
------------------------ Package Registration Mechanism ------------------------
--------------------------------------------------------------------------------
}

type

	TKPackageEnum = (
{ design }
		pedSystem, pedKernel,	pedStd, pedExt,	pedWinAPI,
		pedDBCtrls, pedDB, pedExperts, pedCOM, pedBarCode,
		pedCrypto, pedDialogs, pedDBDialogs, pedForms, pedCPL, 
{ runtime }
		perSystem, perKernel,	perStd, perExt, perWinAPI,
		perDBCtrls, perDB, perExperts, perCOM, perBarCode,
		perCrypto, perDialogs, perDBDialogs, perForms, perCPL );

	TKPackageEnums = set of TKPackageEnum;

	TKPackageKey = {$IFDEF DELPHI4}Cardinal{$ELSE}Integer{$ENDIF};	

function IsPackageRunning( APackage: TKPackageEnum ): Boolean;
function CheckPackageRegistered( APackage: TKPackageEnum ): Boolean;
function CheckPackageRunning( APackage: TKPackageEnum ): Boolean;

function GetPackageName( APackage: TKPackageEnum ): string;
function GetPackageNames( APackages: TKPackageEnums ): string;

function GetRegUnitName( APackage: TKPackageEnum ): string;
function GetRegUnitNames( APackages: TKPackageEnums ): string;
function GetRegMirrorName( APackage: TKPackageEnum ): string;
function GetRegMirrorNames( APackages: TKPackageEnums ): string;

procedure RegisterKnowHowPackage( APackage: TKPackageEnum; Key: TKPackageKey );
procedure UnregisterKnowHowPackage( APackage: TKPackageEnum; Key: TKPackageKey );

procedure RegisterRunningPackage( APackage: TKPackageEnum; Key: TKPackageKey );
procedure UnregisterRunningPackage( APackage: TKPackageEnum; Key: TKPackageKey );

function CheckAnyPackagesRunning( APackages: TKPackageEnums ): Boolean;
procedure ForceAnyPackagesRunning( const AClassName: ShortString; APackages: TKPackageEnums );

function CheckAllPackagesRunning( APackages: TKPackageEnums ): Boolean;
procedure ForceAllPackagesRunning( const AClassName: ShortString; APackages: TKPackageEnums );

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestSystemShareWareVersion;
procedure CreateRegCheckerThread( APackage: TKPackageEnum );

function AnyPackage_Shareware: Boolean;
function FirstShareWare_PackageName: string;
function IsPackage_ShareWare( APackage: TKPackageEnum ): Boolean;

function CheckRegistryInfo( Data1, Data2: LongInt ): Boolean;

function IsAppRegistered( p: Pointer ): Boolean;
function MakeAppResName( const AppResName: string ): string;
function ApplicationRootDir( const BaseRegKey: string ): string;
function CheckAppRegistryKeys( const BaseRegKey, ClassName: string ): Boolean;
procedure CreateAppRegistrationKeys( const BaseRegKey, AppPath, ClassName: string );

{
--------------------------------------------------------------------------------
-------------------------------- Internal Use ----------------------------------
--------------------------------------------------------------------------------
}

resourcestring

{ Authors' Names }

	sDemian = 'Demian Lessa';
	sLeonardo = 'Leonardo Freitas';

{##NI##}

implementation

uses
	Windows, SysUtils, Classes, Registry, uksyResStr, uksyTypes, uksyConsts,
	uksyUtils, uksyRegCheck;

type

	EKSYPackReg = class( EKSystem );

{
--------------------------------------------------------------------------------
------------------------ Package Registration Mechanism ------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

{
--------------------------------------------------------------------------------
-------------------------------- Internal Use ----------------------------------
--------------------------------------------------------------------------------
}

{ Packages -- Version 1.00 }

{$IFDEF KLIB100}

const

	sKRPSystem    = 'krpSystem100.dpl';
	sKDPSystem    = 'kdpSystem100.dpl';
	sKRPKernel    = 'krpKernel100.dpl';
	sKDPKernel    = 'kdpKernel100.dpl';
	sKRPStd       = 'krpStd100.dpl';
	sKDPStd       = 'kdpStd100.dpl';
	sKRPExt       = 'krpExt100.dpl';
	sKDPExt       = 'kdpExt100.dpl';
	sKRPWinAPI    = 'krpWinAPI100.dpl';
	sKDPWinAPI    = 'kdpWinAPI100.dpl';
	sKRPDBCtrls   = 'krpDBCtrls100.dpl';
	sKDPDBCtrls   = 'kdpDBCtrls100.dpl';
	sKRPDB        = 'krpDB100.dpl';
	sKDPDB        = 'kdpDB100.dpl';
	sKRPExperts   = 'krpExperts100.dpl';
	sKDPExperts   = 'kdpExperts100.dpl';
	sKRPCOM       = 'krpCOM100.dpl';
	sKDPCOM       = 'kdpCOM100.dpl';
	sKRPBarCode   = 'krpBarCode100.dpl';
	sKDPBarCode   = 'kdpBarCode100.dpl';
	sKRPCrypto    = 'krpCrypto100.dpl';
	sKDPCrypto    = 'kdpCrypto100.dpl';
	sKRPDialogs   = 'krpDialogs100.dpl';
	sKDPDialogs   = 'kdpDialogs100.dpl';
	sKRPDBDialogs = 'krpDBDialogs100.dpl';
	sKDPDBDialogs = 'kdpDBDialogs100.dpl';
	sKRPForms     = 'krpForms100.dpl';
	sKDPForms     = 'kdpForms100.dpl';
	sKRPCPL				= 'krpCPL100.dpl';
	SKDPCPL				= 'kdpCPL100.dpl';

{ design }
	sKDPSystemConsts    = 'uksydConsts.pas';
	sKDPKernelConsts    = 'ukrdConsts.pas';
	sKDPStdConsts 			= 'uksdConsts.pas';
	sKDPExtConsts 			= 'ukedConsts.pas';
	sKDPWinAPIConsts 	  = 'ukwdConsts.pas';
	sKDPDBCtrlsConsts   = 'ukdcdConsts.pas';
	sKDPDBConsts        = 'ukdbdConsts.pas';
	sKDPExpertsConsts   = 'ukexdConsts.pas';
	sKDPCOMConsts 			= 'ukcmdConsts.pas';
	sKDPBarCodeConsts 	= 'ukbcdConsts.pas';
	sKDPCryptoConsts 	  = 'ukcydConsts.pas';
	sKDPDialogsConsts   = 'ukdgdConsts.pas';
	sKDPDBDialogsConsts = 'ukdddConsts.pas';
	sKDPFormsConsts 		= 'ukfdConsts.pas';
	sKDPCPLConsts 			= 'ukcpdConsts.pas';
{ runtime }
	sKRPSystemConsts    = 'uksyConsts.pas';
	sKRPKernelConsts 	  = 'ukrConsts.pas';
	sKRPStdConsts	 		  = 'uksConsts.pas';
	sKRPExtConsts 			= 'ukeConsts.pas';
	sKRPWinAPIConsts 	  = 'ukwConsts.pas';
	sKRPDBCtrlsConsts 	= 'ukdcConsts.pas';
	sKRPDBConsts 			  = 'ukdbConsts.pas';
	sKRPExpertsConsts 	= 'ukexConsts.pas';
	sKRPCOMConsts 			= 'ukcmConsts.pas';
	sKRPBarCodeConsts   = 'ukbcConsts.pas';
	sKRPCryptoConsts    = 'ukcyConsts.pas';
	sKRPDialogsConsts   = 'ukdgConsts.pas';
	sKRPDBDialogsConsts = 'ukddConsts.pas';
	sKRPFormsConsts 		= 'ukfConsts.pas';
	sKRPCPLConsts 			= 'ukcpConsts.pas';
{ design }
	sKDPSystemUtils    = 'uksydUtils.pas';
	sKDPKernelUtils    = 'ukrdUtils.pas';
	sKDPStdUtils 			 = 'uksdUtils.pas';
	sKDPExtUtils 			 = 'ukedUtils.pas';
	sKDPWinAPIUtils 	 = 'ukwdUtils.pas';
	sKDPDBCtrlsUtils   = 'ukdcdUtils.pas';
	sKDPDBUtils        = 'ukdbdUtils.pas';
	sKDPExpertsUtils   = 'ukexdUtils.pas';
	sKDPCOMUtils 			 = 'ukcmdUtils.pas';
	sKDPBarCodeUtils 	 = 'ukbcdUtils.pas';
	sKDPCryptoUtils 	 = 'ukcydUtils.pas';
	sKDPDialogsUtils   = 'ukdgdUtils.pas';
	sKDPDBDialogsUtils = 'ukdddUtils.pas';
	sKDPFormsUtils 		 = 'ukfdUtils.pas';
	sKDPCPLUtils       = 'ukcpdUtils.pas';
{ runtime }
	sKRPSystemUtils    = 'uksyUtils.pas';
	sKRPKernelUtils 	 = 'ukrUtils.pas';
	sKRPStdUtils	 		 = 'uksUtils.pas';
	sKRPExtUtils 			 = 'ukeUtils.pas';
	sKRPWinAPIUtils 	 = 'ukwUtils.pas';
	sKRPDBCtrlsUtils 	 = 'ukdcUtils.pas';
	sKRPDBUtils 			 = 'ukdbUtils.pas';
	sKRPExpertsUtils 	 = 'ukexUtils.pas';
	sKRPCOMUtils 			 = 'ukcmUtils.pas';
	sKRPBarCodeUtils   = 'ukbcUtils.pas';
	sKRPCryptoUtils    = 'ukcyUtils.pas';
	sKRPDialogsUtils   = 'ukdgUtils.pas';
	sKRPDBDialogsUtils = 'ukddUtils.pas';
	sKRPFormsUtils 		 = 'ukfUtils.pas';
	sKRPCPLUtils       = 'ukcpUtils.pas';

{ Current Package Version }

	PACKAGE_NAMES: array[TKPackageEnum] of string[19] =
{ design }
	(	sKDPSystem, sKDPKernel,	sKDPStd, sKDPExt,	sKDPWinAPI, sKDPDBCtrls, sKDPDB,
		sKDPExperts, sKDPCOM, sKDPBarCode, sKDPCrypto, sKDPDialogs, sKDPDBDialogs,
		sKDPForms, sKDPCPL,
{ runtime }
		sKRPSystem, sKRPKernel,	sKRPStd, sKRPExt, sKRPWinAPI, sKRPDBCtrls, sKRPDB,
		{ sKDPExperts For experts package there is only dsgn package -  }sKRPExperts,
		sKRPCOM, sKRPBarCode, sKRPCrypto, sKRPDialogs, sKRPDBDialogs, sKRPForms, sKRPCPL );

	PACKAGE_UNIT_REG_NAMES: array[TKPackageEnum] of string[15] =
{ design }
	(	sKDPSystemConsts, sKDPKernelConsts,	sKDPStdConsts, sKDPExtConsts,	sKDPWinAPIConsts,
		sKDPDBCtrlsConsts, sKDPDBConsts, sKDPExpertsConsts, sKDPCOMConsts, sKDPBarCodeConsts,
		sKDPCryptoConsts, sKDPDialogsConsts, sKDPDBDialogsConsts, sKDPFormsConsts, sKDPCPLConsts,
{ runtime }
		sKRPSystemConsts, sKRPKernelConsts,	sKRPStdConsts, sKRPExtConsts, sKRPWinAPIConsts,
		sKRPDBCtrlsConsts, sKRPDBConsts, sKRPExpertsConsts, sKRPCOMConsts, sKRPBarCodeConsts,
		sKRPCryptoConsts, sKRPDialogsConsts, sKRPDBDialogsConsts, sKRPFormsConsts, sKRPCPLConsts );

	PACKAGE_UNIT_MIRROR_NAMES: array[TKPackageEnum] of string[14] =
{ design }
	(	sKDPSystemUtils, sKDPKernelUtils,	sKDPStdUtils, sKDPExtUtils,	sKDPWinAPIUtils,
		sKDPDBCtrlsUtils, sKDPDBUtils, sKDPExpertsUtils, sKDPCOMUtils, sKDPBarCodeUtils,
		sKDPCryptoUtils, sKDPDialogsUtils, sKDPDBDialogsUtils, sKDPFormsUtils, sKDPCPLUtils,
{ runtime }
		sKRPSystemUtils, sKRPKernelUtils,	sKRPStdUtils, sKRPExtUtils, sKRPWinAPIUtils,
		sKRPDBCtrlsUtils, sKRPDBUtils, sKRPExpertsUtils, sKRPCOMUtils, sKRPBarCodeUtils,
		sKRPCryptoUtils, sKRPDialogsUtils, sKRPDBDialogsUtils, sKRPFormsUtils, sKRPCPLUtils );

{$ENDIF}

{$IFOPT Q+}
	{$DEFINE QCHECK}
	{$Q-}
{$ENDIF}
function MakeKey( const AName: ShortString ): TKPackageKey;
var
	i: TKPackageKey;
begin
	ForceTrimStr( AName );
	Result := 13;
	for i := 1 to SizeOf( ShortString ) - 1 do
		Result := Result +
			( ( Succ( i ) * i ) and Pred( i ) ) * ( ( Result + i ) or i ) +
			( Succ( TKPackageKey( Ord( AName[i mod ( Length( AName ) + 1 )] ) ) ) *
				Pred( TKPackageKey( Ord( AName[i mod ( Length( AName ) + 1 )] ) ) ) xor i );
end;
{$IFDEF QCHECK}
	{$Q+}
	{$UNDEF QCHECK}
{$ENDIF}

function CheckKey( const AName: ShortString; Key: TKPackageKey ): Boolean;
begin
	Result := ( MakeKey( AName ) ) = Key;
end;

{$IFDEF CREATE_KEY_FILES}

procedure CreateKnowHowPackageKeyFile( const FileName: string; KeyType: Byte );

const
	PACKAGE_KEY_SPACE = 25;
	PACKAGE_KEY_EXT = '.key';
	PACKAGE_KEY_PATTERN = '%-*sKey: $%.8x'#13#10;

var
	i: TKPackageEnum;
	fs: TFileStream;
	s: string;
begin
	ForceTrimStr( FileName );
	ForceDeleteFile( FileName );
	fs := TFileStream.Create( FileName, fmCreate );
	try
		for i := Low( TKPackageEnum ) to High( TKPackageEnum ) do
		begin
			if ( KeyType = 0 ) then
				s := Format( PACKAGE_KEY_PATTERN, [PACKAGE_KEY_SPACE, StrRightPad(
					GetPackageName( i ), PACKAGE_KEY_SPACE, CH_DOTMARK ),
					MakeKey( GetPackageName( i ) )] )
			else
				s := Format( PACKAGE_KEY_PATTERN, [PACKAGE_KEY_SPACE, StrRightPad(
					GetRegMirrorName( i ), PACKAGE_KEY_SPACE, CH_DOTMARK ),
					MakeKey( GetRegMirrorName( i ) )] );
			fs.WriteBuffer( Pointer( s )^, Length( s ) );
		end;
	finally
		fs.Free;
	end;
end;
{$ENDIF}

var
	PackageList: TStrings = nil;

procedure ClosePackageList;
begin
	while CheckStrings( PackageList ) do
		PackageList.Delete( PackageList.Count - 1 );
	FreeClean( PackageList );
end;

procedure PackageListInclude( const AName: string );
begin
	if ( not CheckObject( PackageList ) ) then
		PackageList := TStringList.Create;
	PackageList.Add( AName );
end;

function PackageListRemove( const AName: string ): Boolean;
var
	i : Integer;
begin
	if ( not CheckObject( PackageList ) ) then
	begin
		Result := False;
		Exit;
	end;
	i := PackageList.IndexOf( AName );
	Result := ( i <> -1 );
	if ( Result ) then
		PackageList.Delete( i );
end;

function PackageListExists( const AName: string ): Boolean;
begin
	if ( not CheckObject( PackageList ) ) then
	begin
		Result := False;
		Exit;
	end;
	Result := ( PackageList.IndexOf( AName ) <> -1 );
end;

{-------------------------------------------------------------------------------}

var
	PackageRunningList: TStrings = nil;

procedure ClosePackageRunningList;
begin
	while CheckStrings( PackageRunningList ) do
		PackageRunningList.Delete( PackageRunningList.Count - 1 );
	FreeClean( PackageRunningList );
end;

procedure PackageRunningListInclude( const AName: string );
begin
	if ( not CheckObject( PackageRunningList ) ) then
		PackageRunningList := TStringList.Create;
	PackageRunningList.Add( AName );
end;

function PackageRunningListRemove( const AName: string ): Boolean;
var
	i : Integer;
begin
	if ( not CheckObject( PackageRunningList ) ) then
	begin
		Result := False;
		Exit;
	end;
	i := PackageRunningList.IndexOf( AName );
	Result := ( i <> -1 );
	if ( Result ) then
		PackageRunningList.Delete( i );
end;

function PackageRunningListExists( const AName: string ): Boolean;
begin
	if ( not CheckObject( PackageRunningList ) ) then
	begin
		Result := False;
		Exit;
	end;
	Result := ( PackageRunningList.IndexOf( AName ) <> -1 );
end;

{---------------------------- Public Implementation ----------------------------}

function GetPackageName( APackage: TKPackageEnum ): string;
begin
	Result := {$IFDEF KLIB100}PACKAGE_NAMES[APackage]{$ELSE}''{$ENDIF};
end;

function CheckPackageRegistered( APackage: TKPackageEnum ): Boolean;
begin
	Result := PackageListExists( GetPackageName( APackage ) );
end;

procedure RegisterKnowHowPackage( APackage: TKPackageEnum; Key: TKPackageKey );
begin
	if ( not CheckKey( GetPackageName( APackage ), Key ) ) then
		RaiseExceptionFmt( EKSYPackReg, sErrRegPackInvKey, [GetPackageName( APackage )] );
	if PackageListExists( GetPackageName( APackage ) ) then
		RaiseExceptionFmt( EKSYPackReg, sErrRegPackRegistered, [GetPackageName( APackage )] );
	PackageListInclude( GetPackageName( APackage ) );
end;

procedure UnregisterKnowHowPackage( APackage: TKPackageEnum; Key: TKPackageKey );
begin
	if ( not CheckKey( GetPackageName( APackage ), Key ) ) then
		RaiseExceptionFmt( EKSYPackReg, sErrUnregPackInvKey, [GetPackageName( APackage )] );
	if CheckObject( PackageList ) then
	begin
		if ( not PackageListExists( GetPackageName( APackage ) ) ) then
			RaiseExceptionFmt( EKSYPackReg, sErrUnregPackRegistered, [GetPackageName( APackage )] );
		if ( not PackageListRemove( GetPackageName( APackage ) ) ) then
			RaiseExceptionFmt( EKSYPackReg, sErrUnregPackRegistered, [GetPackageName( APackage )] );
	end;
end;

function GetRegUnitName( APackage: TKPackageEnum ): string;
begin
	Result := {$IFDEF KLIB100}PACKAGE_UNIT_REG_NAMES[APackage]{$ELSE}''{$ENDIF};
end;

function GetRegMirrorName( APackage: TKPackageEnum ): string;
begin
	Result := {$IFDEF KLIB100}PACKAGE_UNIT_MIRROR_NAMES[APackage]{$ELSE}''{$ENDIF};
end;

function CheckPackageRunning( APackage: TKPackageEnum ): Boolean;
begin
	Result := PackageRunningListExists( GetRegMirrorName( APackage ) );
end;

function IsPackageRunning( APackage: TKPackageEnum ): Boolean;
begin
	Result := CheckAllModules( sDelphi32, [GetPackageName( APackage )] ) and
		CheckPackageRunning( APackage );
end;

procedure RegisterRunningPackage( APackage: TKPackageEnum; Key: TKPackageKey );
begin
	if ( not CheckKey( GetRegMirrorName( APackage ), Key ) ) then
		RaiseExceptionFmt( EKSYPackReg, sErrRegUnitInvKey, [GetRegMirrorName( APackage )] );
	if PackageListExists( GetRegMirrorName( APackage ) ) then
		RaiseExceptionFmt( EKSYPackReg, sErrRegUnitRegistered, [GetRegMirrorName( APackage )] );
	PackageRunningListInclude( GetRegMirrorName( APackage ) );
end;

procedure UnregisterRunningPackage( APackage: TKPackageEnum; Key: TKPackageKey );
begin
	if ( not CheckKey( GetRegMirrorName( APackage ), Key ) ) then
		RaiseExceptionFmt( EKSYPackReg, sErrUnregUnitInvKey, [GetRegMirrorName( APackage )] );
	if CheckObject( PackageRunningList ) then
	begin
		if ( not PackageRunningListExists( GetRegMirrorName( APackage ) ) ) then
			RaiseExceptionFmt( EKSYPackReg, sErrUnregUnitRegistered, [GetRegMirrorName( APackage )] );
		if ( not PackageRunningListRemove( GetRegMirrorName( APackage ) ) ) then
			RaiseExceptionFmt( EKSYPackReg, sErrUnregUnitRegistered, [GetRegMirrorName( APackage )] );
	end;
end;

function GetPackageNames( APackages: TKPackageEnums ): string;
var
	i: TKPackageEnum;
begin
	Result := '';
	for i := Low( TKPackageEnum ) to High( TKPackageEnum ) do
		if ( i in APackages ) then
			Result := Result + GetPackageName( i ) + CH_COMMA + CH_SPACE;
	if CheckStr( Result ) then
		Delete( Result, Length( Result ) - 1, 2 );
end;

function GetRegUnitNames( APackages: TKPackageEnums ): string;
var
	i: TKPackageEnum;
begin
	Result := '';
	for i := Low( TKPackageEnum ) to High( TKPackageEnum ) do
		if ( i in APackages ) then
			Result := Result + GetRegUnitName( i ) + CH_COMMA + CH_SPACE;
	if CheckStr( Result ) then
		Delete( Result, Length( Result ) - 1, 2 );
end;

function GetRegMirrorNames( APackages: TKPackageEnums ): string;
var
	i: TKPackageEnum;
begin
	Result := '';
	for i := Low( TKPackageEnum ) to High( TKPackageEnum ) do
		if ( i in APackages ) then
			Result := Result + GetRegMirrorName( i ) + CH_COMMA + CH_SPACE;
	if CheckStr( Result ) then
		Delete( Result, Length( Result ) - 1, 2 );
end;

function CheckAnyPackagesRunning( APackages: TKPackageEnums ): Boolean;
var
	i: TKPackageEnum;
begin
	Result := False;
	for i := Low( TKPackageEnum ) to High( TKPackageEnum ) do
		if ( i in APackages ) then
			Result := ( Result or CheckPackageRunning( i ) );
end;

procedure ForceAnyPackagesRunning( const AClassName: ShortString; APackages: TKPackageEnums );
begin
	if ( not CheckAnyPackagesRunning( APackages ) ) then
		RaiseExceptionFmt( EKSYPackReg, sErrClassNotReg, [AClassName,
			GetPackageNames( APackages )] );
end;

function CheckAllPackagesRunning( APackages: TKPackageEnums ): Boolean;
var
	i: TKPackageEnum;
begin
	Result := True;
	for i := Low( TKPackageEnum ) to High( TKPackageEnum ) do
		if ( i in APackages ) then
			Result := ( Result and CheckPackageRunning( i ) );
end;

procedure ForceAllPackagesRunning( const AClassName: ShortString; APackages: TKPackageEnums );
begin
	if ( not CheckAllPackagesRunning( APackages ) ) then
		RaiseExceptionFmt( EKSYPackReg, sErrClassNotReg, [AClassName,
			GetPackageNames( APackages )] );
end;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

var
	Registered: Boolean = False;
	RegCheckThread : TKRegistryCheckThread = nil;

procedure UnregisterSystemPackages;
begin
	if ( not IsSystem_Shareware ) and ( Registered ) then
	begin
		UnregisterKnowHowPackage( perSystem, $A65DE92F ); { do not const... }
		UnregisterKnowHowPackage( pedSystem, $47F3FDC3 );
		Registered := False;
	end;
end;

procedure DestroyRegCheckerThread;
begin
	if CheckObject( RegCheckThread ) then
		TerminateResgitryCheckThread( RegCheckThread );
end;

{---------------------------- Public Implementation ----------------------------}

procedure TestSystemShareWareVersion;
begin
{$IFDEF PACKREG_TESTING}
	if ( not CheckPackageRegistered( perSystem ) ) then
		RegisterKnowHowPackage( perSystem, $A65DE92F ); { do not const... }
	if ( not CheckPackageRegistered( pedSystem ) ) then
		RegisterKnowHowPackage( pedSystem, $47F3FDC3 );
	Registered := True;
{$ENDIF}
	if ( IsSystem_Shareware ) then
	begin
		if ( not CurrentDelphi32Running ) then
			FatalErrorFmt( sErrShareWare, [GetPackageName( perSystem )] );
	end
	else if ( not Registered ) then
	begin
		if ( not CheckPackageRegistered( perSystem ) ) then
			RegisterKnowHowPackage( perSystem, $A65DE92F ); { do not const... }
		if ( not CheckPackageRegistered( pedSystem ) ) then
			RegisterKnowHowPackage( pedSystem, $47F3FDC3 );
		Registered := True;
	end;
end;

function AnyPackage_Shareware: Boolean;
begin
	Result := CheckObject( RegCheckThread );
end;

function FirstShareWare_PackageName: string;
begin
	if AnyPackage_Shareware then
		Result := GetPackageName( RegCheckThread.PackageEnum )
	else
		Result := '';
end;

function IsPackage_ShareWare( APackage: TKPackageEnum ): Boolean;
begin
	Result := ( not CheckPackageRegistered( APackage ) )
end;

procedure CreateRegCheckerThread( APackage: TKPackageEnum );
begin
	if ( not CheckObject( RegCheckThread ) ) then
	begin
		if ( not ( CheckPackageRegistered( APackage ) and CheckPackageRunning( APackage ) ) ) then
{$IFDEF REG_CHECK_ENABLED}
			RegCheckThread := TKRegistryCheckThread.Create( APackage );
{$ENDIF}			
	end;
end;

function CheckRegistryInfo( Data1, Data2: LongInt ): Boolean;
type

	TSignature	   = TUserName;
	TKey				   = TUserName;
	PKRegistryInfo = ^TKRegistryInfo;
	TKRegistryInfo = record
		Signature: TSignature;
		Key: TKey;
		UserName: TUserName;
		Company: TCompanyName;
	end;

	PKInstallInfo = ^TKInstallInfo;
	TKInstallInfo = record
		Signature: TSignature;
		Key: TKey;
	end;

	function CheckInstallKey( pri: PKRegistryInfo ): Boolean;
	begin
		Result := CheckStrEqual( CH_BRACKET_OPEN + IntToHex( MakeKey( Trim( pri^.UserName ) +
			Trim( pri^.Company ) ), BITS_PER_BYTE ) + IntToHex( MakeKey( InvertString( Trim(
			pri^.UserName ) + Trim( pri^.Company ) ) ), BITS_PER_BYTE ) + CH_BRACKET_CLOSE,
			Trim( pri^.Key ) );
	end;

var
	pii: PKInstallInfo;
	pri: PKRegistryInfo;

begin
	ForcePointers( [Pointer( Data1 ), Pointer( Data2 )] );
	pri := PKRegistryInfo( Data1 + SizeOf( TKRegistryInfo ) ); { Hard Coded! }
	pii := PKInstallInfo( Data2 + SizeOf( TKInstallInfo ) ); { Hard Coded! }
	Result :=
		( CheckTrimStrs( [pri^.Signature, pri^.Key, pri^.UserName, pii^.Signature, pii^.Key] ) and
{$IFDEF KLIB100}
			CheckStrEqual( pri^.Signature, '{09536FA0-BF69-11D2-B212-00C0DFE081C4}' ) and { do not resource/const }
{$ELSE}
			*MUST GENERATE AN ERROR!*;
{$ENDIF}
			CheckStrEqual( pri^.Signature, pii^.Signature ) and
			CheckStrEqual( Trim( pri^.Key ), Trim( pii^.Key ) ) and
			CheckInstallKey( pri ) );
end;

function IsAppRegistered( p: Pointer ): Boolean;

	{$IFOPT Q+}
		{$DEFINE QCHECK2}
		{$Q-}
	{$ENDIF}
	function MakeAppKey( const AName: ShortString ): LongInt;
	var
		i: LongInt;
	begin
		ForceTrimStr( AName );
		Result := 23; { Use another magic number! }
		for i := 1 to SizeOf( ShortString ) - 1 do
			Result := Result +
				( ( Succ( i ) * i ) and Pred( i ) ) * ( ( Result + i ) or i ) +
				( Succ( Ord( AName[i mod ( Length( AName ) + 1 )] ) ) *
					Pred( Ord( AName[i mod ( Length( AName ) + 1 )] ) ) xor i );
	end;
	{$IFDEF QCHECK2}
		{$Q+}
		{$UNDEF QCHECK2}
	{$ENDIF}

type
	PKAppReg = ^TKAppReg;
	TKAppReg = record
		UserName: TUserName;
		CompanyName: TCompanyName;
		Key1: LongInt;
		Key2: LongInt;
	end;

begin
	ForcePointer( p );
	with PKAppReg( p )^ do
		Result := ( CheckTrimStr( UserName + CompanyName ) and
			( MakeAppKey( UserName + CompanyName ) = Key1 ) and
			( MakeAppKey( InvertString( UserName + CompanyName ) ) = Key2 ) );
end;

function MakeAppResName( const AppResName: string ): string;
var
	i: Integer;
begin
	ForceTrimStr( AppResName );
	Result := '';
	for i := Length( AppResName ) downto 1 do
		Result := Result + IntToHex( Ord( AppResName[i] ), 2 );
end;

function ApplicationRootDir( const BaseRegKey: string ): string;
var
	R2: TRegistry;
begin
	ForceTrimStr( BaseRegKey );
	R2 := TRegistry.Create;
	try
		R2.RootKey := HKEY_LOCAL_MACHINE;
		if ( not R2.OpenKey( BaseRegKey, True ) ) then
			RaiseException( EKSYPackReg, sErrReadRootDir );
		Result := R2.ReadString( APPLICATION_ROOT_DIR_SECTION );
	finally
		R2.Free;
	end;
end;

function CheckAppRegistryKeys( const BaseRegKey, ClassName: string ): Boolean;
var
	s: string;
	R2: TRegistry;
begin
	Result := CheckTrimStrs( [BaseRegKey, ClassName] );
	if ( not Result ) then
		Exit;
	R2 := TRegistry.Create;  { Generic AppRegistration }
	try
		if ( not R2.OpenKey( BaseRegKey, True ) ) then
			RaiseException( EKSYPackReg, sErrReadResName );
		s := R2.ReadString( APPLICATION_INTERNAL_REG_SECTION );
		Result := CheckTrimStr( s ) and CheckStrEqual( s, MakeAppResName( UpperCase( ClassName ) ) );
		if ( not Result ) then
			Exit;
		R2.RootKey := HKEY_LOCAL_MACHINE;
		if ( not R2.OpenKey( BaseRegKey, True ) ) then
			RaiseException( EKSYPackReg, sErrReadResName );
		s := R2.ReadString( APPLICATION_ROOT_DIR_SECTION );
		Result := CheckTrimStr( s );
	finally
		R2.Free;
	end;
end;

{ Actually BaseRegKey should be = APPLICATION_INTERNAL_REG_SECTION + ApplicationLikeName( '' ) }
procedure CreateAppRegistrationKeys( const BaseRegKey, AppPath, ClassName: string );
var
	R2: TRegistry;
begin
	ForceTrimStrs( [BaseRegKey, AppPath, ClassName] );
	ForcePath( AppPath );
	R2 := TRegistry.Create;  { Generic AppRegistration }
	try
		if ( not R2.OpenKey( BaseRegKey, True ) ) then
			RaiseException( EKSYPackReg, sErrWriteResName );
		R2.WriteString( APPLICATION_INTERNAL_REG_SECTION, MakeAppResName( UpperCase( ClassName ) ) );
		R2.RootKey := HKEY_LOCAL_MACHINE;
		if ( not R2.OpenKey( BaseRegKey, True ) ) then
			RaiseException( EKSYPackReg, sErrWriteResName );
		R2.WriteString( APPLICATION_ROOT_DIR_SECTION, AppPath );
	finally
		R2.Free;
	end;
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
{$IFDEF CREATE_KEY_FILES}
	CreateKnowHowPackageKeyFile( 'u:\Delphi\KLib100\Source\Include\KLIB100_Packages.key', 0 );
	CreateKnowHowPackageKeyFile( 'u:\Delphi\KLib100\Source\Include\KLIB100_Units.key', 1 );
{$ENDIF}	
end;

procedure Done;
begin
	UnregisterSystemPackages;
	ClosePackageList;
	ClosePackageRunningList;
	DestroyRegCheckerThread;
end;

initialization
	Init;

finalization
	Done;

end.
