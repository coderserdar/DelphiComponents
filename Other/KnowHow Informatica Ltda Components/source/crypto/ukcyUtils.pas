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

unit ukcyUtils;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Classes, uksyUtils, ukcyTypes;

{
--------------------------------------------------------------------------------
------------------------- Generic Pointer Routines -----------------------------
--------------------------------------------------------------------------------
}

procedure CreateBFKey( const KeyIn; szIn: Integer; KeyOut: PBFKey );
procedure CreateStrBFKey( const KeyIn: string; KeyOut: PBFKey );

procedure StuffStream( Stream: TStream; StuffChar: Char; BlockSize: Integer );
procedure DeStuffStream( Stream: TStream; StuffChar: Char; BlockSize: Integer );

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

function IsCrypto_Shareware: Boolean;

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
	SysUtils, Consts, uksyTypes, uksyConsts, uksyPackReg, ukcyConsts, ukcyPackReg,
	ukcyBlowFish;

type

	EKCYUtils = class( EKCrypto );

{
--------------------------------------------------------------------------------
------------------------- Generic Pointer Routines -----------------------------
--------------------------------------------------------------------------------
}

procedure CreateBFKey( const KeyIn; szIn: Integer; KeyOut: PBFKey );
var
	i: Integer;
begin
	ForceReference( KeyIn );
	ForceReference( KeyOut );
	ZeroMemory( KeyOut, szIn );
	if ( szIn <= 0 ) then
		Exit;
	for i := 0 to szIn - 1 do
		KeyOut^[i] := Byte( PByteArray( @KeyIn )^[i] );
end;

procedure CreateStrBFKey( const KeyIn: string; KeyOut: PBFKey );
begin
	TKStringBlowFish.ForceStringKey( KeyIn );
	CreateBFKey( Pointer( KeyIn )^, Length( KeyIn ), KeyOut );
end;

procedure StuffStream( Stream: TStream; StuffChar: Char; BlockSize: Integer );
var
	s: string;
	iRead: Integer;
begin
	ForceObject( Stream );
	while ( Stream.Size <> Stream.Position ) do
	begin
		SetString( s, nil, BlockSize );
		iRead := Stream.Read( Pointer( s )^, BlockSize );
		if ( iRead <> BlockSize ) and ( Stream.Size <> Stream.Position ) then
			RaiseException( EKCYUtils, SReadError );
		if ( iRead < BlockSize ) then
			SetLength( s, iRead );
		ForceStr( s );
		s := StringReplace( s, StuffChar, StuffChar + StuffChar, krfAll );
		Stream.Seek( -iRead, soFromCurrent );
		Stream.WriteBuffer( Pointer( s )^, Length( s ) );
	end;
end;

procedure DeStuffStream( Stream: TStream; StuffChar: Char; BlockSize: Integer );
var
	s: string;
	iRead: Integer;
begin
	ForceObject( Stream );
	BlockSize := Min( BlockSize, KB );
	while ( Stream.Size <> Stream.Position ) do
	begin
		SetString( s, nil, BlockSize );
		iRead := Stream.Read( Pointer( s )^, BlockSize );
		if ( iRead <> BlockSize ) and ( Stream.Size <> Stream.Position ) then
			RaiseException( EKCYUtils, SReadError );
		if ( iRead < BlockSize ) then
			SetLength( s, iRead );
		ForceStr( s );
		s := StringReplace( s, StuffChar + StuffChar, StuffChar, krfAll );
		Stream.Seek( -iRead, soFromCurrent );
		Stream.WriteBuffer( Pointer( s )^, Length( s ) );
	end;
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

function IsCrypto_Shareware: Boolean;
begin
{$IFDEF INTERNAL_VERSION}
	Result := false;
{$ELSE}
	Result := ( not CheckRegistryInfo( GetCryptoRegistryInfo,
		LongInt( @KnowHowInstallInfo ) - SizeOf( TKInstallInfo ) ) );
{$ENDIF}		
end;

procedure RegisterCryptoUnits;
begin
	RegisterRunningPackage( perCrypto, $B168E831 ); { do not const... }
	RegisterRunningPackage( pedCrypto, $2734A591 );
end;

procedure UnregisterCryptoUnits;
begin
	UnregisterRunningPackage( perCrypto, $B168E831 ); { do not const... }
	UnregisterRunningPackage( pedCrypto, $2734A591 );
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
	Result := Trim( PKRegistryInfo( GetCryptoRegistryInfo + SizeOf( TKRegistryInfo ) ).UserName );
end;

function PackageCompanyName: string;
begin
	Result := Trim( PKRegistryInfo( GetCryptoRegistryInfo + SizeOf( TKRegistryInfo ) ).Company );
end;

function PackageVersion: TKLibVersion;
begin
	ZeroMemory( @Result, SizeOf( TKLIBVersion ) );
	Result.Release := StrToDateTime( CRYPTO_VER_RELEASE_DATE );
	Result.Version := CRYPTO_VER;
	Result.Reserved := CRYPTO_VER_INT;
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
	RegisterCryptoUnits;
	TestCryptoShareWareVersion;
	CreateRegCheckerThread( perCrypto );
end;

procedure Done;
begin
	UnregisterCryptoUnits;
end;

initialization
	Init;

finalization
	Done;

end.
