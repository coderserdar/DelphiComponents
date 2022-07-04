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

unit ukdcUtils;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
  uksyUtils;

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

function IsDBCtrls_Shareware: Boolean;

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
	SysUtils, uksyConsts, uksyTypes, uksyPackReg, ukdcConsts, ukdcPackReg;

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

	MAXLEN_KEY = MAXLEN_USERNAME;

{$ENDIF}

function IsDBCtrls_Shareware: Boolean;
begin
{$IFDEF INTERNAL_VERSION}
	Result := false;
{$ELSE}
	Result := ( not CheckRegistryInfo( GetDBCtrlsRegistryInfo,
		LongInt( @KnowHowInstallInfo ) - SizeOf( TKInstallInfo ) ) );
{$ENDIF}
end;

procedure RegisterDBCtrlsUnits;
begin
	RegisterRunningPackage( perDBCtrls, $F17ED570 ); { do not const... }
	RegisterRunningPackage( pedDBCtrls, $92DE7CD2 );
end;

procedure UnregisterDBCtrlsUnits;
begin
	UnregisterRunningPackage( perDBCtrls, $F17ED570 ); { do not const... }
	UnregisterRunningPackage( pedDBCtrls, $92DE7CD2 );
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
	Result := Trim( PKRegistryInfo( GetDBCtrlsRegistryInfo + SizeOf( TKRegistryInfo ) ).UserName );
end;

function PackageCompanyName: string;
begin
	Result := Trim( PKRegistryInfo( GetDBCtrlsRegistryInfo + SizeOf( TKRegistryInfo ) ).Company );
end;

function PackageVersion: TKLibVersion;
begin
	ZeroMemory( @Result, SizeOf( TKLIBVersion ) );
	Result.Release := StrToDateTime( DBCTRLS_VER_RELEASE_DATE );
	Result.Version := DBCTRLS_VER;
	Result.Reserved := DBCTRLS_VER_INT;
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
  RegisterDBCtrlsUnits;
	TestDBCtrlsShareWareVersion;
	CreateRegCheckerThread( perDBCtrls );
end;

procedure Done;
begin
	UnregisterDBCtrlsUnits;
end;

initialization
	Init;

finalization
	Done;

end.
