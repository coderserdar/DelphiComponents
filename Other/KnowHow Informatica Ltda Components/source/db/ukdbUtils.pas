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

unit ukdbUtils;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Classes, Controls, DB, DBTables, uksyUtils, ukrClasses, ukrUtils;

type

	EKDBUtils = class( EKDB );

{
--------------------------------------------------------------------------------
--------------------------- General DB Utilities -------------------------------
--------------------------------------------------------------------------------
}

function MountLinkedFieldsSQL( DataSet: TDBDataSet; ALinkedFields: TKStrings;
	const AOperator, ATablePrefix: string ): string;

function GetFieldTypeByClass( const FieldClass: ShortString ): TFieldType;
function GetFieldClassByType( FieldType: TFieldType ): ShortString;
function GetFieldValidCharsByClass( const FieldClass: ShortString ): TFieldChars;
function GetFieldDataSizeByClass( const FieldClass: ShortString ): Word;

function GetTableName( const TblName: string ): string;

{
--------------------------------------------------------------------------------
--------------------------- SQL Script Utilities -------------------------------
--------------------------------------------------------------------------------
}

procedure BuildDBSQLScript( const ASessionName, ADataBaseName: string; ss: TStrings );
procedure GetScriptSections( const Script: string; Proc: TGetStrProc );

function ValidateDML_SQL( SQL: TStrings ): Boolean;

{##NI##}

{
--------------------------------------------------------------------------------
--------------------- Field Registration Architecture --------------------------
--------------------------------------------------------------------------------
}

procedure RegisterKFields( const FieldClasses: array of TFieldClass );
procedure RegisterDictionaryControls( const ControlClasses: array of TControlClass );
function GetFieldClass( const FieldClass: ShortString ): TFieldClass;

const
	RegisterKFieldsProc: procedure( const FieldClasses: array of TFieldClass ) = nil;
	RegisterDictionaryControlsProc: procedure( const ControlClasses: array of TControlClass ) = nil;
	GetFieldClassProc: function( const FieldClass: ShortString ): TFieldClass = nil;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

function IsDB_Shareware: Boolean;

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
	SysUtils, Forms, TypInfo, uksyConsts, uksyTypes, uksyPackReg, ukrConsts,
	ukrDBUtils, ukdbResStr, ukdbConsts, ukdbPackReg, ukdbClasses, ukdbScript;

{
--------------------------------------------------------------------------------
--------------------------- General DB Utilities -------------------------------
--------------------------------------------------------------------------------
}

function MountLinkedFieldsSQL( DataSet: TDBDataSet; ALinkedFields: TKStrings;
	const AOperator, ATablePrefix: string ): string;
const
	LINKED_SQL_FLD_PATTERN: string = ( '( %s."%s"=''%s'' ) %s '#13#10 );
var
	i: Integer;
	ks: TKStrings;
	sLinkedFlds: string;
begin
	Result := '';
	ForceObjects( [DataSet, ALinkedFields] );
	if ( not CheckStrings( ALinkedFields ) ) then
		Exit;
	if ( not ( CheckStr( AOperator ) and CheckStr( ATablePrefix ) ) ) then
		RaiseException( EKDBUtils, sErrDBIInvSQLOperator );
	ks := TKStrings.Create;
	try
{ Get the names that has the linked fields. Values has the Name of the TableName
	fields linked with! }
		sLinkedFlds := MakeString( ALinkedFields, CH_LIST_TOKEN, stNames );
		ExtractStrings( sLinkedFlds, CH_LIST_TOKEN, ks );
		ks.AdjustForValues;
{ ForceFields( DataSet, ALinkedFields, EKDBIntegrity, ValidLinkFldTypes )?
	The property editor do this job }
		FillFieldValues( DataSet, ks );
		for i := 0 to ks.Count - 1 do
			Result := Result + Format( LINKED_SQL_FLD_PATTERN, [ATablePrefix,
				ks.Names[i], ks.ValuesByIndex[i], AOperator] );
	finally
		ks.Free;
	end;
	if CheckStr( Result ) then
		Delete( Result, Pos( Format( '%s '#13#10, [AOperator] ), Result ),
			Length( Format( '%s '#13#10, [AOperator] ) ) );
end;

function GetFieldTypeByClass( const FieldClass: ShortString ): TFieldType;
var
	i: Integer;
begin
	ForceTrimStr( FieldClass );
	Result := FIELDTYPE_BY_FIELDCLASS_MAP[loFldClassMap].FieldType;
	for i := ( loFldClassMap + 1 ) to hiFldClassMap do
		if CheckStrEqual( Trim( FieldClass ), FIELDTYPE_BY_FIELDCLASS_MAP[i].FieldClassName ) then
		begin
			Result := FIELDTYPE_BY_FIELDCLASS_MAP[i].FieldType;
			Exit;
		end;
end;

function GetFieldValidCharsByClass( const FieldClass: ShortString ): TFieldChars;
var
	i: Integer;
begin
	ForceTrimStr( FieldClass );
	Result := FIELD_VALIDCHARS_BY_FIELDCLASS_MAP[loFldValidCharsMap].FieldValidChars;
	for i := ( loFldValidCharsMap + 1 ) to hiFldValidCharsMap do
		if CheckStrEqual( Trim( FieldClass ), FIELD_VALIDCHARS_BY_FIELDCLASS_MAP[i].FieldClassName ) then
		begin
			Result := FIELD_VALIDCHARS_BY_FIELDCLASS_MAP[i].FieldValidChars;
			Exit;
		end;
end;

function GetFieldDataSizeByClass( const FieldClass: ShortString ): Word;
var
	i: Integer;
begin
	Result := FIELD_DATASIZE_BY_FIELDCLASS_MAP[loFldSizeMap].FieldDataSize;
	for i := (loFldSizeMap+1) to hiFldSizeMap do
		if CheckStrEqual( Trim( FieldClass ), FIELD_DATASIZE_BY_FIELDCLASS_MAP[i].FieldClassName ) then
		begin
			Result := FIELD_DATASIZE_BY_FIELDCLASS_MAP[i].FieldDataSize;
			Exit;
		end;
end;

function GetFieldClassByType( FieldType: TFieldType ): ShortString;
var
  i: Integer;
begin
  Result := '';
	for i := loFldClassMap to hiFldClassMap do
		if ( FIELDTYPE_BY_FIELDCLASS_MAP[i].FieldType = FieldType ) then
			Result := FIELDTYPE_BY_FIELDCLASS_MAP[i].FieldClassName;
end;

function GetTableName( const TblName: string ): string;
begin
	ForceTrimStr( TblName );
	Result := TblName;
	Result := StringReplace( Result, EXT_DBF, '', krfAll );
	Result := StringReplace( Result, EXT_DB, '', krfAll );
	Result := StringReplace( Result, EXT_TXT, '', krfAll );
end;

{
--------------------------------------------------------------------------------
--------------------------- SQL Script Utilities -------------------------------
--------------------------------------------------------------------------------
}

procedure BuildDBSQLScript( const ASessionName, ADataBaseName: string; ss: TStrings );
var
	SQLBuild: TKDBCustomSQLBuild;
	SQLBuildClass: TKDBCustomSQLBuildClass;
begin
	ForceTrim( [ASessionName, ADataBaseName, ss] );
	SQLBuildClass := SQLBUILD_DB_CLASS_MAP[sstLocal];
	if ( not CheckClass( SQLBuildClass ) ) then
		RaiseExceptionFmt( EKDBSQLBuild, sErrSBTypeNotImpl,
			[EnumName( Integer( sstLocal ), TypeInfo( TKSQLScriptType ) )] );
	SQLBuild := SQLBuildClass.CreateLinked( sstLocal, ASessionName, ADataBaseName );
	try
		ss.BeginUpdate;
		try
			ss.Clear;
			ss.Text:= SQLBuild.SQLScript;
		finally
			ss.EndUpdate;
		end;
	finally
		SQLBuild.Free;
	end;
end;

procedure GetScriptSections( const Script: string; Proc: TGetStrProc );
var
	iPos1,
	iPos2,
	iPos3: Integer;
	sProc,
	sScript: string;
begin
	ForceTrimStr( Script );
	ForceReference( @Proc );
	iPos1 := Pos( SQLBUILD_ENVVAR_SOS_TOKEN, Script );
	iPos2 := ( Pos( SQLBUILD_ENVVAR_EOS_TOKEN, Script ) - 2 );
	iPos3 := Pos( SQLBUILD_RUN_TOKEN_COMMAND, Script );
	sProc := '';
	sScript := Copy( Script, iPos1, iPos2-iPos1 );
  if ( iPos1 = 0 ) or ( iPos2 = 0 ) or ( iPos3 = 0 ) or ( iPos1 >= iPos2 ) or
     ( iPos3 >= iPos2 ) or ( iPos3 <= iPos1 ) then
		RaiseException( EKSQLBuild, sErrSBInvScriptToken );
	iPos1 := Pos( SQLBUILD_START_TOKEN_COMMAND, sScript ) +
	  Length( SQLBUILD_START_TOKEN_COMMAND );
  iPos3 := Pos( SQLBUILD_RUN_TOKEN_COMMAND, sScript );
  if ( iPos1 = 0 ) or ( iPos3 = 0 ) or ( iPos1 >= iPos2 ) or
     ( iPos3 >= iPos2 ) or ( iPos3 <= iPos1 ) then
		RaiseException( EKSQLBuild, sErrSBInvScriptToken );
	while ( iPos3 <> 0 ) do
	begin
		sProc := Copy( sScript, iPos1, iPos3-iPos1 );
		if ( not CheckTrimStr( sProc ) ) then
			RaiseException( EKSQLBuild, sErrSBInvScriptToken );
		sScript := Copy( sScript, ( iPos3 + Length( SQLBUILD_RUN_TOKEN_COMMAND ) ), iPos2 );
		iPos1 := Pos( SQLBUILD_START_TOKEN_COMMAND, sScript ) +
      Length( SQLBUILD_START_TOKEN_COMMAND );
    iPos3 := Pos( SQLBUILD_RUN_TOKEN_COMMAND, sScript );
    if ( iPos3 <> 0 ) and ( ( iPos1 = 0 ) or ( iPos1 >= iPos2 ) or
			 ( iPos3 >= iPos2 ) or ( iPos3 <= iPos1 ) ) then
			RaiseException( EKSQLBuild, sErrSBInvScriptToken );
		Proc( sProc );
	end;
end;

function ValidateDML_SQL( SQL: TStrings ): Boolean;
var
	i: Integer;
	s: string;
begin
	Result := False;
	if ( not CheckStrings( SQL ) ) then
	  Exit;
	s := SQL.Text;
	for i := DML_SQL_LOW_CHECK to DML_SQL_HIGH_CHECK do
		if CheckStrContains( DML_SQL_CHECK[i], s ) then
			Exit;
	Result := True;
end;

{
--------------------------------------------------------------------------------
--------------------- Field Registration Architecture --------------------------
--------------------------------------------------------------------------------
}

procedure RegisterKFields( const FieldClasses: array of TFieldClass );
begin
	if CheckReference( @RegisterKFieldsProc ) then
		RegisterKFieldsProc( FieldClasses )
	else
		RaiseException( EKDBUtils, sErrDBInvFieldRegProc );
end;

procedure RegisterDictionaryControls( const ControlClasses: array of TControlClass );
begin
	if CheckReference( @RegisterDictionaryControlsProc ) then
		RegisterDictionaryControlsProc( ControlClasses )
	else
		RaiseException( EKDBUtils, sErrDBInvControlRegProc );
end;

function GetFieldClass( const FieldClass: ShortString ): TFieldClass;
begin
  Result := nil;
	if CheckReference( @GetFieldClassProc ) then
		Result := GetFieldClassProc( FieldClass )
	else
		RaiseException( EKDBUtils, sErrDBInvGetFldClassProc );
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

function IsDB_Shareware: Boolean;
begin
{$IFDEF INTERNAL_VERSION}
	Result := false;
{$ELSE}
	Result := ( not CheckRegistryInfo( GetDBRegistryInfo,
		LongInt( @KnowHowInstallInfo ) - SizeOf( TKInstallInfo ) ) );
{$ENDIF}
end;

procedure RegisterDBUnits;
begin
	RegisterRunningPackage( perDB, $056B8CE2 ); { do not const... }
	RegisterRunningPackage( pedDB, $62E72C65 );
end;

procedure UnregisterDBUnits;
begin
	UnregisterRunningPackage( perDB, $056B8CE2 ); { do not const... }
	UnregisterRunningPackage( pedDB, $62E72C65 );
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
	Result := Trim( PKRegistryInfo( GetDBRegistryInfo + SizeOf( TKRegistryInfo ) ).UserName );
end;

function PackageCompanyName: string;
begin
	Result := Trim( PKRegistryInfo( GetDBRegistryInfo + SizeOf( TKRegistryInfo ) ).Company );
end;

function PackageVersion: TKLibVersion;
begin
	ZeroMemory( @Result, SizeOf( TKLIBVersion ) );
	Result.Release := StrToDateTime( DB_VER_RELEASE_DATE );
	Result.Version := DB_VER;
	Result.Reserved := DB_VER_INT;
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
  RegisterDBUnits;
	TestDBShareWareVersion;
	CreateRegCheckerThread( perDB );
end;

procedure Done;
begin
	UnregisterDBUnits;
end;

initialization
	Init;

finalization
	Done;

end.
