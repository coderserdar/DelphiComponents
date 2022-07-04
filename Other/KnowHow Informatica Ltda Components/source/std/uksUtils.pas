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

unit uksUtils;

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

function IsStd_Shareware: Boolean;

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
	SysUtils, uksyTypes, uksyPackReg, uksConsts, {$IFNDEF INTERNAL_VERSION}
	uksyConsts, {$ENDIF} uksPackReg;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

type

	TSignature	 = TUserName;
	TKey				 = TUserName;

{--------------------------- Internal Implementation ---------------------------}

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

function IsStd_Shareware: Boolean;
begin
{$IFDEF INTERNAL_VERSION}
	Result := false;
{$ELSE}
	Result := ( not CheckRegistryInfo( GetStdRegistryInfo,
		LongInt( @KnowHowInstallInfo ) - SizeOf( TKInstallInfo ) ) );
{$ENDIF}
end;

procedure RegisterStdUnits;
begin
	RegisterRunningPackage( perStd, $85C3AE4E ); { do not const... }
	RegisterRunningPackage( pedStd, $4181BE43 );
end;

procedure UnregisterStdUnits;
begin
	UnregisterRunningPackage( perStd, $85C3AE4E ); { do not const... }
	UnregisterRunningPackage( pedStd, $4181BE43 );
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
	Result := Trim( PKRegistryInfo( GetStdRegistryInfo + SizeOf( TKRegistryInfo ) ).UserName );
end;

function PackageCompanyName: string;
begin
	Result := Trim( PKRegistryInfo( GetStdRegistryInfo + SizeOf( TKRegistryInfo ) ).Company );
end;

function PackageVersion: TKLibVersion;
begin
	ZeroMemory( @Result, SizeOf( TKLIBVersion ) );
	Result.Release := StrToDateTime( STD_VER_RELEASE_DATE );
	Result.Version := STD_VER;
	Result.Reserved := STD_VER_INT;
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
	RegisterStdUnits;
	TestStdShareWareVersion;
	CreateRegCheckerThread( perStd );
end;

procedure Done;
begin
  UnregisterStdUnits;  
end;

initialization
	Init;

finalization
	Done;

end.
