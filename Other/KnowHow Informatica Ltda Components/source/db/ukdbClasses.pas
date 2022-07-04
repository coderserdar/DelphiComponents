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

unit ukdbClasses;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Classes, DB, DBTables,uksyUtils, uksyClasses, ukrClasses, ukdbConsts;

type

	EKDBClasses = class( EKDB );

{
--------------------------------------------------------------------------------
----------------------------- Generic DB Classes -------------------------------
--------------------------------------------------------------------------------
}

	TKDBBytes = class( TKBytes )
	public
		function SaveToField( bf: TBytesField ): Boolean;
		function LoadFromField( bf: TBytesField ): Boolean;

	end;

	TDataSetClass = class of TDataSet;

{
--------------------------------------------------------------------------------
------------------------------ DBInfo Calsses ----------------------------------
--------------------------------------------------------------------------------
}

{ TKDBInfo }

	EKDBInfo = class( EKDBClasses );
	
{ TKCustomInfo, TKDBInfo, TKDictionaryInfo }

	PKFieldInfo = ^TKFieldInfo;
	TKFieldInfo = record
		Size: Integer;
		IsKey: Boolean;
		FieldID: Integer;
		Info: ShortString;
		Name: ShortString;
		Alias: ShortString;
		IsRequired: Boolean;
		DFormat: ShortString;
		EFormat: ShortString;
		Default: ShortString;
		DataType: ShortString;
		ValidChars: ShortString;
		Constraint: ShortString;
		ConstraintMsg: ShortString;
	end;

	PKIndexInfo = ^TKIndexInfo;
	TKIndexInfo = Record
		ID: Integer;
		Info: ShortString;
		Name: ShortString;
		Alias: ShortString;
		Fields: ShortString;
		Options: ShortString;
	end;

{
	PKHeaderInfo = ^TKHeaderInfo;
	TKHeaderInfo = record
		Info: ShortString;
		Version: Integer;
		DatabaseName: ShortString;
		TableCount: Integer;
		LastChange: TDateTime;
	end;
}
	PKTableInfo = ^TKTableInfo;
	TKTableInfo = record
		ID: Integer;
		Info: ShortString;
		Name: ShortString;
		Alias: ShortString;
		FieldCount: Integer;
		IndexCount: Integer;
	end;

{ TKCustomInfo }

	TKCustomInfo = class( TKCustomLinkable )
	private
		FIndexInfos: TList;
		FFieldInfos: TList;
		FTableIndex: Integer;
		FDatabaseName: string;
		FTableNames: TStrings;

		procedure ClearFields;
		procedure ClearIndexes;
		function GetDirectory: string;
		function GetFieldCount: Integer;
		function GetIndexCount: Integer;
		function GetTableCount: Integer;
		procedure SetTableIndex( Value: Integer );
		function GetTableNames( Index: Integer ): string;

	protected
		procedure GetFieldInfos; virtual; abstract;
		procedure GetIndexInfos; virtual; abstract;
		procedure LoadTableNames; virtual; abstract;
		
		procedure UpdateTableInfo; virtual;
		procedure SetDatabaseName( const Value: string ); virtual;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		function GetTableInfo: PKTableInfo; virtual;

		property Directory: string
						 read GetDirectory;
		property FieldCount: Integer
						 read GetFieldCount;
		property FieldInfos: TList
						 read FFieldInfos;
		property IndexCount: Integer
						 read GetIndexCount;
		property IndexInfos: TList
						 read FIndexInfos;
		property TableCount: Integer
						 read GetTableCount;
		property TableIndex: Integer
						 read FTableIndex write SetTableIndex;
		property TableNames[Index: Integer]: string
						 read GetTableNames;

	published
		property DatabaseName: string
						 read FDatabaseName write SetDatabaseName;

	end;

{ TKDBInfo }

	TKDBInfoRequiredFields = set of Byte;

	TKDBInfo = class( TKCustomInfo )
	private
		FTable: TTable;
		FRequiredFields: TKDBInfoRequiredFields;

		function ForceTable: Boolean;
		procedure GetRequiredFields;
		function GetTableTypeName: PChar;
		function IsFieldKey( const AField: string ): Boolean;
		function GetFieldInfo( const AName: string ): PKFieldInfo;
		function GetIndexInfo( const AName: string ): PKIndexInfo;

	protected
		procedure GetFieldInfos; override;
		procedure GetIndexInfos; override;
		procedure LoadTableNames; override;
		procedure UpdateTableInfo; override;
		procedure SetDatabaseName( const Value: string ); override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	end;

{ TKDBPrint }

	TKDBPrint = class( TKDBInfo )
	private
		FTopMargin: Integer;
		FPrintDate: Boolean;
		FPrintName: Boolean;
		FApplyIndexes: Boolean;
		FDateTimeFormat: string;

	public
		constructor Create( AOwner : TComponent ); override;

		procedure PrintTable( Index: Integer );

	published
		property ApplyIndexes: Boolean
						 read FApplyIndexes write FApplyIndexes default true;
		property DateTimeFormat: string
						 read FDateTimeFormat write FDateTimeFormat;
		property PrintDate: Boolean
						 read FPrintDate write FPrintDate default true;
		property PrintName: Boolean
						 read FPrintName write FPrintName default true;

	end;

{
--------------------------------------------------------------------------------
-------------------------- SQL Script Architecture -----------------------------
--------------------------------------------------------------------------------
}

	EKSQLBuild = class( EKDBClasses );

{ TKSQLBuildStrings }

	TKSQLBuildStrings = class( TKStrings )
	private
		function GetInfo( Index: Integer ): string;
		procedure SetInfo( Index: Integer; const Value: string );
		function GetScript: string;
		function GetScriptByName( const Name: string ): string;

	public
		destructor Destroy; override;
		procedure Clear; override;
		procedure Delete( Index: Integer ); override;
		function AddScript( const Name, Script: string ): Integer; virtual;
		function NormalizeSQL( const S: string ): string; dynamic;

		property SubScripts[Index: Integer]: string
						 read GetInfo write SetInfo;
		property ScriptByName[const Name: string]:string
						 read GetScriptByName;
		property Script: string
						 read GetScript;

	end;

{$M+}
	TKSQLScriptType = ( sstUnKnown, sstAnsi, sstMSSql, sstIntB, sstLocal );
{$M-}

	TKCustomSQLBuild = class;

{ Indicating the kind of SQL entity actualy builded with the actual SQLBuild class }
	TKSQLBuildType = ( sbtTable, sbtIndex );

	TKSQLBuildEvent = procedure( Sender: TKCustomSQLBuild;
		IteratorID, SQLStringsID: Integer; SQLBuildType: TKSQLBuildType ) of object;

{ TKCustomSQLBuild }

	TKCustomSQLBuild = class( TPersistent )
	private
		FBuildEvent: TKSQLBuildEvent;
		FSQLSType: TKSQLScriptType;
		FScript: string;
		FHeaderPattern: string;
		FFooterPattern: string;
		FTablePattern: string;
		FFieldPattern: string;
		FPrimKeyPattern: string;
		FCheckPattern: string;
		FIndexesPattern: string;
	 {FConstraintsPattern: string;
		FForeginKeyPattern: string;
		FRefIntPattern: string;}
		FFieldTypeDef: TKSQLFieldTypeDef;

		FKTables: TKSQLBuildStrings;
		FKIndexes: TKSQLBuildStrings;

		function GetFieldTypeDefs( Index: TFieldType ): TKStrFldTypDef;
		procedure SetFieldTypeDefs( Index: TFieldType; const Value: TKStrFldTypDef );

	protected
		constructor Create( ASQLType: TKSQLScriptType ); virtual;

		function BuildHeader: string; virtual; abstract;
		function BuildFooter: string; virtual; abstract;
		function BuildTable( const TableID: Integer ): string; virtual; abstract;
		function BuildField( const TableID, FieldID: Integer ): string; virtual; abstract;
		function BuildIndex( const TableID, IndexID: Integer ): string; virtual; abstract;
		function BuildPrimaryKey( const TableID: Integer ): string; virtual; abstract;
		function BuildCheckConstraints( TableID: Integer; Constraint: TCheckConstraint ): string; virtual; abstract;
		function BuildDBName: string; virtual; abstract;
		function IsPrimaryKeyIndex( const TableID, IndexID: Integer ): Boolean; virtual; abstract;
		function GetTblCount: Integer; virtual; abstract;
		function GetIdxCount( TableID: Integer ): Integer; virtual; abstract;

		procedure DoBuild( IteratorID, SQLStringsID: Integer; SQLBuiltType: TKSQLBuildType ); dynamic;
		function GetStartCommand: string; dynamic;
		function GetRunCommand: string; dynamic;
		function GetStrings( Index: Integer ): TKSQLBuildStrings; virtual;
		function GetScript: string; virtual;
		function GetScriptCleared: Boolean; virtual;
		procedure SetPattern( Index: Integer; const Value: string ); virtual;

		procedure SetSQLScriptType( Value: TKSQLScriptType ); virtual;

		property HeaderPattern: string
						 index SQLBUILD_HEADER_PATTERN read FHeaderPattern write SetPattern;
		property FooterPattern: string
						 index SQLBUILD_FOOTER_PATTERN read FFooterPattern write SetPattern;
		property TablePattern: string
						 index SQLBUILD_TABLES_PATTERN read FTablePattern write SetPattern;
		property IndexesPattern: string
						 index SQLBUILD_INDEXES_PATTERN read FIndexesPattern write SetPattern;
		property FieldPattern: string
						 index SQLBUILD_FIELDS_PATTERN read FFieldPattern write SetPattern;
		property PrimKeyPattern: string
						 index SQLBUILD_PRIMKY_PATTERN read FPrimKeyPattern write SetPattern;
		property CheckPattern: string
						 index SQLBUILD_CHECKS_PATTERN read FCheckPattern write SetPattern;

		property InternalScript: string
						 read FScript write FScript;
		property FieldTypeDef: TKSQLFieldTypeDef
						 read FFieldTypeDef write FFieldTypeDef;
		property FieldTypeDefs[Index: TFieldType]: TKStrFldTypDef
						 read GetFieldTypeDefs write SetFieldTypeDefs;
		property TableCount: Integer
						 read GetTblCount;
		property IndexCount[TableID: Integer]: Integer
						 read GetIdxCount;

		property OnBuild: TKSQLBuildEvent
						 read FBuildEvent write FBuildEvent;

	public
		destructor Destroy; override;

		procedure BuildSQLScript; virtual;
		procedure ClearSQLScript; virtual;

		procedure Assign( Source: TPersistent ); override;

		property SQLScriptType: TKSQLScriptType
						 read FSQLSType;
		property SQLScript: string
						 read GetScript;
		property ScriptCleared: Boolean
						 read GetScriptCleared;

		property Tables: TKSQLBuildStrings
						 index SQLBUILD_TABLES_PATTERN read GetStrings;
		property Indexes: TKSQLBuildStrings
						 index SQLBUILD_INDEXES_PATTERN read GetStrings;
		{constraints, foreginkeys, etc...}

	end;

	TKCustomSQLBuildClass = class of TKCustomSQLBuild;

	EKDBSQLBuild = class( EKSQLBuild );

{ TKDBCustomSQLBuild }

	TKDBCustomSQLBuild = class( TKCustomSQLBuild )
	private
		FCurIdxCount : Integer;
		FCurTblID : Integer;
		FSessionName: string;
		FDataBaseName: string;
		FTablePatterns: string;
		FSystemTables: Boolean;
		FExtensions: Boolean;
		FSession: TSession;
		FDataBase: TDataBase;
		FTable: TTable;
		FDSList: TStrings;
		FOwner: TObject;

		function GetSession: TSession;
		function GetDatabase: TDataBase;
		function GetTable( const TableName: string ): TTable;

		procedure SetInfo( Index: Integer; Value: string );
		function GetInfo( Index: Integer ): string;

	protected
		constructor Create( ASQLType: TKSQLScriptType ); override;

		function GetTblCount: Integer; override;
		function GetIdxCount( TableID: Integer ): Integer; override;

		function GetOwnerObject: TObject;

		function BuildType( Field: TFieldDef ): string; virtual; abstract;
		function BuildNullable( Field: TFieldDef ): string; virtual; abstract;
		function BuildDefault( Field: TFieldDef ): string; virtual; abstract;
		function ValidAndGetPrimKey( const TableID: Integer ): TIndexDef; virtual; abstract;
		function ValidAndGetIndex( const TableID, IndexID :Integer ): TIndexDef; virtual; abstract;

	public
		destructor Destroy; override;
		constructor CreateLinked( ASQLType: TKSQLScriptType;
			const ASessionName, ADataBaseName: string ); virtual;
		constructor CreateOwned( AOwner: TObject; ASQLType: TKSQLScriptType ); virtual;

		procedure Assign( Source: TPersistent ); override;

		property Owner: TObject
						 read GetOwnerObject;
		property ActualSession: TSession
						 read GetSession;
		property ActualDataBase: TDataBase
						 read GetDataBase;
		property ActualTable[const TableName: string]: TTable
						 read GetTable;
		property CurrentTableID: Integer
						 read FCurTblID;
		property CurrentIndexCount: Integer
						 read FCurIdxCount;

		property TableCount;
		property IndexCount;
		property FieldTypeDef;
		property FieldTypeDefs;

		property Tables;
		property Indexes;

		property DataBaseName: string
						 index 0 read GetInfo write SetInfo;
		property SessionName: string
						 index 1 read GetInfo write SetInfo;
		property TablePatterns: string
						 index 2 read GetInfo write SetInfo;

		property TablesList: TStrings
						 read FDSList;

		property SystemTables: Boolean
						 read FSystemTables write FSystemTables;
		property Extensions: Boolean
						 read FExtensions write FExtensions;

	end;

	TKDBCustomSQLBuildClass = class of TKDBCustomSQLBuild;

{ TKDBAnsiSQLBuild }

	TKDBAnsiSQLBuild = class( TKDBCustomSQLBuild )
	private
		property HeaderPattern;
		property FooterPattern;
		property TablePattern;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		procedure SetSQLScriptType( Value: TKSQLScriptType ); override;

	protected
		function ValidAndGetPrimKey( const TableID: Integer ): TIndexDef; override;
		function ValidAndGetIndex( const TableID, IndexID :Integer ): TIndexDef; override;
		function IsPrimaryKeyIndex( const TableID, IndexID: Integer ): Boolean; override;
		function BuildTable( const TableID: Integer ): string; override;
		function BuildField( const TableID, FieldID: Integer ): string; override;
		function BuildPrimaryKey( const TableID: Integer ): string; override;
		function BuildCheckConstraints( TableID: Integer; Constraint: TCheckConstraint ): string; override;
		function BuildIndex( const TableID, IndexID: Integer ): string; override;
		function BuildType( Field: TFieldDef ): string; override;
		function BuildNullable( Field: TFieldDef ): string; override;
		function BuildDefault( Field: TFieldDef ): string; override;

	public
		constructor CreateLinked( ASQLType: TKSQLScriptType;
			const ASessionName, ADataBaseName: string ); override;

{ Make public for internal package decoupling... }
		function BuildHeader: string; override;
		function BuildDBName: string; override;
		function BuildFooter: string; override;

	end;

  TKDBAnsiSQLBuildClass = class of TKDBAnsiSQLBuild;

{ TKDBLocalSQLBuild }

	TKDBLocalSQLBuild = class( TKDBAnsiSQLBuild )
	private
		property FieldPattern;
		property CheckPattern;
		property FieldTypeDef;
		property PrimKeyPattern;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		procedure SetSQLScriptType( Value: TKSQLScriptType ); override;

	protected
		function BuildField( const TableID, FieldID: Integer ): string; override;
		function BuildPrimaryKey( const TableID: Integer ): string; override;
		function BuildType( Field: TFieldDef ): string; override;
		function TableType( TableID: Integer ): TTableType; virtual;

	end;

{ TKMSSQLBuild }

	TKDBMSSQLBuild = class( TKDBAnsiSQLBuild )
	private
		property TablePattern;
		property FieldTypeDef;
		property IndexesPattern;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		procedure SetSQLScriptType( Value: TKSQLScriptType ); override;

	{$IFDEF DELPHI4}
	public
	{$ELSE}
	protected
	{$ENDIF}
		procedure BuildSQLScript; override;

	end;

{ TKDBInterBSQLBuild }

	TKDBInterBSQLBuild = class( TKDBAnsiSQLBuild )
	private
		property FieldTypeDef;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		procedure SetSQLScriptType( Value: TKSQLScriptType ); override;

	protected
		function BuildType( Field: TFieldDef ): string; override;

	end;

{ Utility Functions }

function GetSQLTypeTag( ASQLType: TKSQLScriptType ): string;

{##NI##}

function AdjustQuoteAndCRLF( const S: string ): string;

const

	SQLBUILD_DB_CLASS_MAP: array[TKSQLScriptType] of TKDBCustomSQLBuildClass =
	(
		TKDBCustomSQLBuild, { sstUnKnown }
		TKDBAnsiSQLBuild,   { sstAnsi    }
		TKDBMSSQLBuild,     { sstMSSql   }
		TKDBInterBSQLBuild, { sstIntB    }
		TKDBLocalSQLBuild   { sstLocal   }
	);

{##NI##}

implementation

uses
	SysUtils, Graphics, Controls, Forms, BDE, Printers, uksyConsts, uksyResStr,
	ukrUtils, ukrDBUtils, ukdbResStr, ukdbUtils;

{
--------------------------------------------------------------------------------
----------------------------- Generic DB Classes -------------------------------
--------------------------------------------------------------------------------
}

function TKDBBytes.SaveToField( bf: TBytesField ): Boolean;
begin
	Result := CheckPointer( BytesList ) and ( Count > 0 );
	if Result then
		bf.SetData( BytesList );
end;

function TKDBBytes.LoadFromField( bf: TBytesField ): Boolean;
var
	lSize: LongInt;
	FByteArray: PByteArray;
begin
	lSize := bf.DataSize;
	GetMem( FByteArray, lSize );
	try
		Result := bf.GetData( FByteArray );
		if Result then
			Result := AssignBytes( FByteArray, lSize );
	finally
		FreeMem( FByteArray, lSize );
	end;
end;

{
--------------------------------------------------------------------------------
------------------------------ DBInfo Classes ----------------------------------
--------------------------------------------------------------------------------
}

procedure ExtractFields( ss: TStrings; const Fields: string );
begin
	ExtractStrings( Fields, CH_LIST_TOKEN, ss );
end;

{ TKCustomInfo }

constructor TKCustomInfo.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FTableIndex := -1;
  FFieldInfos := TList.Create;
	FIndexInfos := TList.Create;
	FTableNames := TStringList.Create;
	TStringList( FTableNames ).Sorted := true;
end;

destructor TKCustomInfo.Destroy;
begin
	ClearFields;
	ClearIndexes;
	FreeClean( FFieldInfos );
	FreeClean( FIndexInfos );
	FreeClean( FTableNames );
	inherited Destroy;
end;

procedure TKCustomInfo.ClearFields;
var
	i: Integer;
begin
	for i := FFieldInfos.Count - 1 downto 0 do
		Dispose( PKFieldInfo( FFieldInfos[i] ) );
	FFieldInfos.Clear;
end;

procedure TKCustomInfo.ClearIndexes;
var
	i: Integer;
begin
	for i := FIndexInfos.Count - 1 downto 0 do
		Dispose( PKIndexInfo( FIndexInfos[i] ) );
	FIndexInfos.Clear;
end;

function TKCustomInfo.GetDirectory: string;
begin
	if ( not CheckTrimStr( DatabaseName ) ) then
		Result := ''
	else
		Result := GetDatabaseDir( DatabaseName );
end;

function TKCustomInfo.GetFieldCount: Integer;
begin
	Result := FFieldInfos.Count;
end;

function TKCustomInfo.GetIndexCount: Integer;
begin
	Result := FIndexInfos.Count;
end;

function TKCustomInfo.GetTableCount: Integer;
begin
	Result := FTableNames.Count;
end;

procedure TKCustomInfo.SetTableIndex( Value: Integer );
begin
	if ( FTableIndex = Value ) then
		Exit;
	if ( Value >= 0 ) and CheckObject( FTableNames ) and ( Value < FTableNames.Count ) then
		FTableIndex := Value
	else
		FTableIndex := -1;
	UpdateTableInfo;
end;

function TKCustomInfo.GetTableNames( Index: Integer ): string;
begin
	Result := '';
	if ( Index >= 0 ) and CheckObject( FTableNames ) and ( Index < FTableNames.Count ) then
		Result := FTableNames[Index];
end;

function TKCustomInfo.GetTableInfo: PKTableInfo;
var
	pti: PKTableInfo;
begin
	pti := New( PKTableInfo );
	try
		ZeroMemory( pti, SizeOf( TKTableInfo ) );
		with pti^ do
		begin
			ID := TableIndex;
			Info := '';
			Alias := '';
			Name := TableNames[TableIndex];
			FieldCount := GetFieldCount;
			IndexCount := GetIndexCount;
		end;
		Result := pti;
	except
		Dispose( pti );
		raise;
	end;
end;

procedure TKCustomInfo.UpdateTableInfo;
begin
	ClearFields;
	ClearIndexes;
	if ( FTableIndex <> -1 ) then
	begin
		GetFieldInfos;
		GetIndexInfos;
	end;
end;

procedure TKCustomInfo.SetDatabaseName( const Value: string );
begin
	if ( FDatabaseName <> Value ) then
	begin
		FDatabaseName := Value;
		LoadTableNames;
	end;
end;

{ TKDBInfo }

constructor TKDBInfo.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FTable := TTable.Create( nil );
end;

destructor TKDBInfo.Destroy;
begin
	FreeClean( FTable );
	inherited Destroy;
end;

function TKDBInfo.ForceTable: Boolean;
begin
	Result := CheckDataSet( FTable );
	if ( not Result ) then
		try
			FTable.Active := true;
		except
			RaiseException( EKDBInfo, sErrInvTable );
		end;
end;

procedure TKDBInfo.GetRequiredFields;
var
	ValCheckDesc: VCHKDesc;
	FCursor, VCursor: HDBICur;
	STableName: array[0..DBIMAXTBLNAMELEN - 1] of Char;
begin
	AnsiToNative( FTable.Database.Locale, FTable.TableName, STableName, SizeOf( STableName ) - 1 );
	FRequiredFields := [];
	while ( not DbiOpenFieldList( FTable.DBHandle, STableName,
		GetTableTypeName, False, FCursor ) = DBIERR_NONE ) do { Retry };
	try
		if ( DbiOpenVChkList( FTable.DBHandle, STableName, GetTableTypeName, VCursor ) = 0 ) then
		begin
			while ( DbiGetNextRecord( VCursor, dbiNoLock, @ValCheckDesc, nil ) = 0 ) do
				if ValCheckDesc.bRequired then
					Include( FRequiredFields, ValCheckDesc.iFldNum );
			DbiCloseCursor( VCursor );
		end;
	finally
		DbiCloseCursor( FCursor );
	end;
end;

function TKDBInfo.GetTableTypeName: PChar;
const
	Names: array[TTableType] of PChar =
		( szPARADOX, szPARADOX, szDBASE, {$IFDEF DELPHI4} szFOXPRO,{$ENDIF} szASCII );
var
	Extension: string;
	TableType: TTableType;
begin
	Result := nil;
	if ( not FTable.Database.IsSQLBased ) then
	begin
		TableType := FTable.TableType;
		if TableType = ttDefault then
		begin
			Extension := ExtractFileExt( FTable.TableName );
			if ( CompareText( Extension, TABLE_TYPE_MASKS[ttDBase] ) = 0 ) then
				TableType := ttDBase;
			if ( CompareText( Extension, TABLE_TYPE_MASKS[ttASCII] ) = 0 ) then
				TableType := ttASCII;
		end;
		Result := Names[TableType];
	end;
end;

function TKDBInfo.IsFieldKey( const AField: string ): Boolean;
var
	i: Integer;
	ss: TStrings;
begin
	Result := false;
	ForceTable;
	if ( not FTable.IndexDefs.Updated ) then
		FTable.IndexDefs.Update;
	ss := TStringList.Create;
	try
		with FTable, IndexDefs do
			for i := 0 to Count - 1 do
				if ( ixPrimary in Items[i].Options ) then
				begin
					ExtractFields( ss, Items[i].Fields );
					Result := Result or ( ss.IndexOf( AField ) > -1 ) or
						( ( CheckObject( FindField( AField ) ) and
							( ss.IndexOf( IntToStr( FindField( AField ).FieldNo ) ) > -1 ) ) );
					if Result then
						Exit;
				end;
	finally
		ss.Free;
	end;
end;

procedure TKDBInfo.GetFieldInfos;
var
	i: Integer;
begin
	ForceTable;
	GetRequiredFields;
	with FTable.FieldDefs do
		for i := 0 to Count - 1 do
			FFieldInfos.Add( GetFieldInfo( Items[i].Name ) );
end;

procedure TKDBInfo.GetIndexInfos;
var
	i: Integer;
begin
	ForceTable;
	with FTable.IndexDefs do
		for i := 0 to Count - 1 do
			FIndexInfos.Add( GetIndexInfo( Items[i].Name ) );
end;

procedure TKDBInfo.LoadTableNames;
var
	ssn: TSession;
	i, j: Integer;
begin
	FTableNames.Clear;
	SetTableIndex( -1 );
	for i := 0 to Sessions.Count - 1 do
	begin
		ssn := Sessions[i];
		for j := 0 to ssn.DatabaseCount - 1 do
			if ( DatabaseName = ssn.Databases[j].DatabaseName ) then
			begin
			  FTable.SessionName := ssn.SessionName;
				ssn.GetTableNames( DatabaseName, '', true, false, FTableNames );
				Exit;
			end;
	end;
end;

procedure TKDBInfo.UpdateTableInfo;
begin
	if CheckObject( FTable ) and ( TableIndex <> -1 ) then
		with FTable do
		begin
			Close;
			TableName := TableNames[TableIndex];
			Open;
			FieldDefs.Update;
			IndexDefs.Update;
		end;
	inherited UpdateTableInfo;
end;

procedure TKDBInfo.SetDatabaseName( const Value: string );
begin
	inherited SetDatabaseName( Value );
	FTable.Close;
	FTable.TableName := '';
	FTable.DatabaseName := Value;
end;

function TKDBInfo.GetFieldInfo( const AName: string ): PKFieldInfo;
var
	i: Integer;
	pfi: PKFieldInfo;
begin
	Result := nil;
	ForceTable;
	i := FTable.FieldDefs.IndexOf( AName );
	if ( i = -1 ) then
		Exit;
	pfi := New( PKFieldInfo );
	try
		ZeroMemory( pfi, SizeOf( TKFieldInfo ) );
		with pfi^ do
		begin
			Info := '';
			Alias := '';
			Name := AName;
			IsKey := IsFieldKey( Name );
			Size := FTable.FieldDefs[i].Size;
			if ( Size = 0 ) then
				Size := FTable.FieldByName( AName ).DataSize;
			FieldID := FTable.FieldDefs[i].FieldNo;
			IsRequired := ( FieldID in FRequiredFields );
			DataType := DataTypeToStr( FTable.FieldDefs[i].DataType );
			DFormat := '';
			EFormat := '';
			Default := '';
			ValidChars := '';
			Constraint := '';
			ConstraintMsg := '';
		end;
		Result := pfi;
	except
		Dispose( pfi );
		raise;
	end;
end;

function TKDBInfo.GetIndexInfo( const AName: string ): PKIndexInfo;
var
	ss: TStrings;
	i,
	j: Integer;
	pii: PKIndexInfo;
begin
	Result := nil;
	ForceTable;
	if ( not FTable.IndexDefs.Updated ) then
		FTable.IndexDefs.Update;
	i := FTable.IndexDefs.IndexOf( AName );
	if ( i = -1 ) then
		Exit;
	pii := New( PKIndexInfo );
	try
		ZeroMemory( pii, SizeOf( TKIndexInfo ) );
		with pii^ do
		begin
			ID := i;
			Name := AName;
			Info := '';
			Alias := '';
			Fields := '';
			Options := '[';
		end;
		with FTable.IndexDefs[i] do
		begin
			if ( ixPrimary in Options ) then
			begin
				if ( AName = '' ) then
					pii^.Name := 'Primary Key';
				pii^.Options := pii^.Options + 'Primary, ';
			end;
			if ( ixUnique in Options ) then
				pii^.Options := pii^.Options + 'Unique, ';
			if ( ixDescending in Options ) then
				pii^.Options := pii^.Options + 'Descending, ';
			if ( ixExpression in Options ) then
				pii^.Options := pii^.Options + 'Expression, ';
			if ( ixCaseInsensitive in Options ) then
				pii^.Options := pii^.Options + 'Case Insensitive]'
			else
				pii^.Options := pii^.Options + 'Case Sensitive]';
		end;
		ss := TStringList.Create;
		try
			with FTable do
			begin
				if ( ixExpression in IndexDefs[i].Options ) then
					ExtractFields( ss, IndexDefs[i].Expression )
				else
					ExtractFields( ss, IndexDefs[i].Fields );
				for j := 0 to Pred( FieldDefs.Count ) do
					if ( ss.IndexOf( FieldDefs[j].Name ) > -1 ) or
						 ( ss.IndexOf( IntToStr( FieldDefs[j].FieldNo ) ) > -1 ) then
						pii^.Fields := pii^.Fields + FieldDefs[j].Name + '; ';
				pii^.Fields := Copy( pii^.Fields, 1, Length( pii^.Fields ) - 2 );
			end;
		finally
			ss.Free;
		end;
		Result := pii;
	except
		Dispose( pii );
		raise;
	end;
end;

{ TKDBPrint }

const
	DELTA = 150;
	FIELD_COLS = 6;
	STRINGS_COUNT = 11;
	TEXT_HEIGHT_PATTERN = 'ABCDEFG';
	OFFSET: Array[1..FIELD_COLS] of Integer = ( 300, 600, 1900, 2400, 2800, 3400 );

	Titles: array[1..STRINGS_COUNT] of string[12] =
	( 'Item', 'Field Name', 'Data Type', 'Key?', 'Data Size', 'Required?', 'Index Name:',
		'Fields', 'Options', 'Yes', 'No' );

constructor TKDBPrint.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FTopMargin := 400;
	FPrintDate := true;
	FPrintName := true;
	FApplyIndexes := true;
	FDateTimeFormat := 'dd/mm/yyyy hh:nn';
end;

procedure TKDBPrint.PrintTable( Index: Integer );
var
	y,
	iField: Integer;
	pfi: PKFieldInfo;
	pii: PKIndexInfo;

	procedure PrintHeader;
	begin
		with Printer.Canvas do
		begin
// print table name
			if FPrintName then
			begin
				TextOut( OFFSET[1], y, FTable.TableName );
				y := y + 2 * TextHeight( TEXT_HEIGHT_PATTERN );
			end;
// print date
			if FPrintdate then
			begin
				TextOut( OFFSET[1], y, FormatDateTime( FDateTimeFormat, Now ) );
				y	:= y + 2 * TextHeight( TEXT_HEIGHT_PATTERN );
			end;
// some blank space
			y := y + 2 * TextHeight( TEXT_HEIGHT_PATTERN );
		end;
	end;

	procedure PrintFieldHeader;
	var
		i: Integer;
	begin
		with Printer.Canvas do
		begin
// print header
			Font.Style := [fsBold];
			for i := 1 to FIELD_COLS do
				TextOut( OFFSET[i], y, Titles[i] );
			Font.Style := [];
// some blank space
			y := y + 2 * TextHeight( TEXT_HEIGHT_PATTERN );
		end;
	end;

	procedure PrintField;
	const
		FIELD_ID: array[Boolean] of Integer = ( $0B, $0A );
	var
		ID: Integer;
		iDelta: Integer;
	begin
		inc( iField );
		with Printer.Canvas do
		begin
// print item number
			iDelta := ( TextWidth( Titles[1] ) - TextWidth( IntToStr( iField ) ) ) div 2;
			TextOut( OFFSET[1] + iDelta, y, IntToStr( iField ) );
// print field name
			TextOut( OFFSET[2], y, pfi^.Name );
// print field's data type
			TextOut( OFFSET[3], y, pfi^.DataType );
// print key fields
			ID := FIELD_ID[pfi^.IsKey];
			iDelta := ( TextWidth( Titles[4] ) - TextWidth( Titles[ID] ) ) div 2;
			TextOut( OFFSET[4] + iDelta, y, Titles[ID] );
// print field's data size
			iDelta := ( TextWidth( Titles[5] ) - TextWidth( IntToStr( pfi^.Size ) ) ) div 2;
			TextOut( OFFSET[5] + iDelta, y, IntToStr( pfi^.Size ) );
// print required fields
			ID := FIELD_ID[pfi^.IsRequired];
			iDelta := ( TextWidth( Titles[6] ) - TextWidth( Titles[ID] ) ) div 2;
			TextOut( OFFSET[6] + iDelta, y, Titles[ID] );
// step to next line...
			y := y + TextHeight( TEXT_HEIGHT_PATTERN );
		end;
	end;

	procedure PrintIndex;
	const
		DELTA = 60;
	begin
		with Printer.Canvas do
		begin
// some blank space
			y := y + TextHeight( TEXT_HEIGHT_PATTERN );
// print index name
			Font.Style := [fsBold];
			TextOut( OFFSET[1], y, Titles[FIELD_COLS + 1] );
			Font.Style := [];
			TextOut( OFFSET[1] + TextWidth( Titles[FIELD_COLS + 1] ) + DELTA, y, pii^.Name );
// some blank space
			y := y + ( 3 * TextHeight( TEXT_HEIGHT_PATTERN ) ) div 2;
// print fields in index
			Font.Style := [fsBold];
			TextOut( OFFSET[2] - DELTA, y, Titles[FIELD_COLS + 2] );
			Font.Style := [];
			TextOut( OFFSET[2] - DELTA + TextWidth( Titles[FIELD_COLS + 3] ) + DELTA, y, pii^.Fields );
// some blank space
			y := y + ( 3 * TextHeight( TEXT_HEIGHT_PATTERN ) ) div 2;
// print index options
			Font.Style := [fsBold];
			TextOut( OFFSET[2] - DELTA, y, Titles[FIELD_COLS + 3] );
			Font.Style := [];
			TextOut( OFFSET[2] - DELTA + TextWidth( Titles[FIELD_COLS + 3] ) + DELTA, y, pii^.Options );
// some blank space
			y := y + TextHeight( TEXT_HEIGHT_PATTERN );
		end;
	end;

var
	i: Integer;
begin
	TableIndex := Index;
	if ( TableIndex = -1 ) then
	  Exit;
	iField := 0;
	with Printer.Canvas do
	begin
		Screen.Cursor := crHourGlass;
		try
			Printer.BeginDoc;
			try
				y := FTopMargin;
				if ( FPrintName or FPrintDate ) then
					PrintHeader;
				PrintFieldHeader;
				for i := 0 to FieldCount - 1 do
				begin
					pfi := GetFieldInfo( FTable.FieldDefs[i].Name );
					try
						PrintField;
					finally
						Dispose( pfi );
					end;
				end;
				if FApplyIndexes then
				begin
					y := y + 2 * Printer.Canvas.TextHeight( TEXT_HEIGHT_PATTERN );
					for i := 0 to IndexCount - 1 do
					begin
						pii := GetIndexInfo( FTable.IndexDefs[i].Name );
						try
							PrintIndex;
						finally
							Dispose( pii );
						end;
					end;
				end;
			finally
				Printer.EndDoc;
			end
		finally
			Screen.Cursor := crDefault;
		end;
	end;
end;

{
--------------------------------------------------------------------------------
-------------------------- SQL Script Architecture -----------------------------
--------------------------------------------------------------------------------
}

{ Utility Functions }

function AdjustQuoteAndCRLF( const S: string ): string;
begin
	Result := S;
	if CheckStr( Result ) then
		Delete( Result, Length( Result ) - 2, 3 );
end;

function GetSQLTypeTag( ASQLType: TKSQLScriptType ): string;
begin
	Result := Copy( EnumName( Integer( ASQLType ), TypeInfo( TKSQLScriptType ) ), 4, MaxInt );
end;

{------------------------------ TKSQLBuildStrings ------------------------------}

destructor TKSQLBuildStrings.Destroy;
begin
	Clear;
	inherited Destroy;
end;

procedure TKSQLBuildStrings.Clear;
var
	i : Integer;
begin
	for i := 0 to Count - 1 do
		StrDispose( PChar( Objects[i] ) );
  inherited Clear;
end;

procedure TKSQLBuildStrings.Delete( Index: Integer );
begin
	StrDispose( PChar( Objects[Index] ) );
	inherited Delete( Index );
end;

function TKSQLBuildStrings.NormalizeSQL( const S: string ): string;
const
{
	Pos( Start, s ) Pos( Run, Result )

		 False            False         -> do nothing (-2)
		 False            True          -> copy from beginning to RunPos - 1
		 True             False         -> copy SQL SubScript/Script from StartPos to Length S
		 True             True          -> copy SQL SubScript/Script between tokens
}

	Filter: array[Boolean, Boolean] of ShortInt = ( ( -2, -1 ), ( 0, 1 ) );
var
	iPos1,
	iPos2: Integer;
begin
	Result := s;
	iPos1 := Pos( SQLBUILD_ENVVAR_START_TOKEN, Result );
	iPos2 := Pos( SQLBUILD_ENVVAR_RUN_TOKEN, Result );
	case Filter[( iPos1 > 0 ), ( iPos2 > 0 )] of
		-2: ;
		-1: Result := Copy( Result, 1, iPos2 - 1 );
		 0: Result := Copy( Result, iPos1 + Length( SQLBUILD_ENVVAR_START_TOKEN ),
					Length( Result ) - ( iPos1 + Length( SQLBUILD_ENVVAR_START_TOKEN ) ) );
		 1: Result := Copy( Result, iPos1 + Length( SQLBUILD_ENVVAR_START_TOKEN ),
					iPos2 - ( iPos1 + Length( SQLBUILD_ENVVAR_START_TOKEN ) ) );
	end;
end;

function TKSQLBuildStrings.GetInfo( Index: Integer ): string;
begin
	Result := PChar( LongInt( Objects[Index] ) );
end;

procedure TKSQLBuildStrings.SetInfo( Index: Integer; const Value: string );
begin
  Objects[Index] := TObject( LongInt( StrNew( PChar( Value ) ) ) );
end;

function TKSQLBuildStrings.GetScript: string;
var
  i: Integer;
begin
  Result := '';
	for i := 0 to Count - 1 do
		Result := Result + GetInfo( i );
end;

function TKSQLBuildStrings.GetScriptByName( const Name: string ): string;
var
	i: Integer;
begin
	Result := '';
	for i := 0 to Count - 1 do
		if CheckStrEqual( Names[i], Name ) then
			Result := Result + GetInfo( i );
end;

function TKSQLBuildStrings.AddScript( const Name, Script: string ): Integer;
begin
  Result := AddObject( Name, TObject( LongInt( StrNew( PChar( Script ) ) ) ) );
end;

{------------------------------ TKCustomSQLBuild -------------------------------}

{

 This base class only defines the SQL script construction definition (abstract
 methods), manges the script lists, and creates the final SQL script.

 FKIndexes:

 Names   -> TableName
 Values  -> IndexName
 Objects -> IndexScript (as string)

 FKTables:

 Strings -> TableName
 Objects -> IndexScript (as string)

}

constructor TKCustomSQLBuild.Create( ASQLType: TKSQLScriptType );
begin
	inherited Create;
	FScript := '';
	SetSQLScriptType( ASQLType );
	ZeroMemory( @FFieldTypeDef, SizeOf( FFieldTypeDef ) );
	FKTables := TKSQLBuildStrings.Create;
	FKTables.Duplicates := dupError;
	FKIndexes := TKSQLBuildStrings.Create;
	FKIndexes.Duplicates := dupError;
end;

destructor TKCustomSQLBuild.Destroy;
begin
	FScript := '';
  FKTables.Free;
	FKIndexes.Free;
	inherited Destroy;
end;

procedure TKCustomSQLBuild.Assign( Source: TPersistent );
begin
	if CheckObjectClass( Source, TKCustomSQLBuild ) then
		with ( Source as TKCustomSQLBuild ) do
		begin
			Self.OnBuild := OnBuild;
(*
			Self.HeaderPattern  := HeaderPattern;
			Self.FooterPattern  := FooterPattern;
			Self.TablePattern   := TablePattern;
			Self.FieldPattern   := FieldPattern;
			Self.PrimKeyPattern := PrimKeyPattern;
			Self.CheckPattern   := CheckPattern;
			Self.IndexesPattern := IndexesPattern;
		 {
			FConstraintsPattern: string;
			FForeginKeyPattern: string;
			FRefIntPattern: string;
			}
			Self.FieldTypeDef := FieldTypeDef;
*)
		end
	else
		inherited Assign( Source );
end;

function TKCustomSQLBuild.GetStartCommand: string;
begin
	Result := SQLBUILD_START_COMMAND;
end;

function TKCustomSQLBuild.GetRunCommand: string;
begin
	Result := SQLBUILD_RUN_COMMAND;
end;

function TKCustomSQLBuild.GetScriptCleared: Boolean;
begin
	Result := ( ( not CheckTrimStr( FScript ) ) or
		( not ( CheckStrings( FKTables ) or CheckStrings( FKIndexes ) ) ) );
end;

function TKCustomSQLBuild.GetScript: string;
begin
	if ( ScriptCleared and ( SQLScriptType <> sstUnKnown ) ) then
		BuildSQLScript;
	Result := FScript;
end;

procedure TKCustomSQLBuild.SetSQLScriptType( Value: TKSQLScriptType );
begin
 	FSQLSType := Value;
end;

procedure TKCustomSQLBuild.SetPattern( Index: Integer; const Value: string );
begin
	case Index of
		SQLBUILD_HEADER_PATTERN :
			FmtStr( FHeaderPattern, sSBInternalHeaderPattern, [GetSQLTypeTag( SQLScriptType ),
				FormatDateTime( SQL_SCRIPT_DATETIME_FORMAT, Now ), Value] );
		SQLBUILD_FOOTER_PATTERN :
			FmtStr( FFooterPattern, sSBInternalFooterPattern, [Value, FormatDateTime(
				SQL_SCRIPT_DATETIME_FORMAT, Now ), GetSQLTypeTag( SQLScriptType )] );
		SQLBUILD_TABLES_PATTERN : FTablePattern := Value;
		SQLBUILD_INDEXES_PATTERN: FIndexesPattern := Value;
		SQLBUILD_FIELDS_PATTERN : FFieldPattern := Value;
		SQLBUILD_PRIMKY_PATTERN : FPrimKeyPattern := Value;
		SQLBUILD_CHECKS_PATTERN : FCheckPattern := Value;
	end;
end;

function TKCustomSQLBuild.GetFieldTypeDefs( Index: TFieldType ): TKStrFldTypDef;
begin
  Result := FieldTypeDef[Index];
end;

procedure TKCustomSQLBuild.SetFieldTypeDefs( Index: TFieldType;
  const Value: TKStrFldTypDef );
begin
  FFieldTypeDef[Index] := Value;
end;

function TKCustomSQLBuild.GetStrings( Index: Integer ): TKSQLBuildStrings;
const
  Pattern: array[SQLBUILD_TABLES_PATTERN..SQLBUILD_INDEXES_PATTERN] of string[7] =
    ( 'tables', 'indexes' );
begin
	if ScriptCleared then
		RaiseExceptionFmt( EKSQLBuild, sErrSBInvStr, [Pattern[Index]] );
	case Index of
		SQLBUILD_TABLES_PATTERN  : Result := FKTables;
		SQLBUILD_INDEXES_PATTERN : Result := FKIndexes;
	else
		Result := nil;
	end;
end;

procedure TKCustomSQLBuild.ClearSQLScript;
begin
	FScript := '';
  FKTables.Clear;
  FKIndexes.Clear;
end;

procedure TKCustomSQLBuild.DoBuild( IteratorID, SQLStringsID: Integer;
  SQLBuiltType: TKSQLBuildType );
begin
  if Assigned( FBuildEvent ) then
    FBuildEvent( Self, IteratorID, SQLStringsID, SQLBuiltType );
end;

procedure TKCustomSQLBuild.BuildSQLScript;
var
	i,
	j,
	k,
	iPos,
	iPos2: Integer;
	sTables,
	sTblName,
	sIndexes,
	sIdxName: string;
	sl : TStrings;

{
 sl: -> Used for DoBuild for Indexes

 Names   -> TableName Index
 Values  -> IndexName Index
 Objects -> FKIndexes Index for IndexName
}

begin
	if ( not ScriptCleared ) then
		ClearSQLScript;
	FScript := BuildHeader;
	sl := TStringList.Create;
	try
		for i := 0 to TableCount - 1 do
		begin
			sTables := BuildTable( i );
			iPos := Pos( 'CREATE TABLE ', sTables );   { do not const... }
			if ( iPos <= 0 ) then
				RaiseExceptionFmt( EKSQLBuild, sErrSBInvTblPattern, [i] );
			Inc( iPos, Length( 'CREATE TABLE ' ) );
			iPos2 := Pos( '(', sTables );
			if ( iPos2 <= 0 ) then
				RaiseExceptionFmt( EKSQLBuild, sErrSBInvTblPattern, [i] );
			sTblName := Trim( Copy( sTables, iPos, iPos2 - iPos - 1 ) );
			if ( IndexCount[i] = 0 ) then
				FKIndexes.AddScript( sTblName + CH_EQUAL_TOKEN + sSBIdxEmptyName,
					Format( sSBEmptyIdx, [sTblName] ) )
			else
				for j := 0 to IndexCount[i] - 1 do
				begin
					if IsPrimaryKeyIndex( i, j ) then
						Continue;
					sIndexes := BuildIndex( i, j );
					iPos := Pos( 'INDEX ', sIndexes );
					if ( iPos <= 0 ) then
						RaiseExceptionFmt( EKSQLBuild, sErrSBInvIdxPattern, [i, sTblName, j] );
					Inc( iPos, Length( 'INDEX ' ) );
					iPos2 := Pos( ' ON', sIndexes );
					if ( iPos2 <= 0 ) then
						RaiseExceptionFmt( EKSQLBuild, sErrSBInvIdxPattern, [i, sTblName, j] );
					sIdxName := Trim( Copy( sIndexes, iPos, iPos2 - iPos ) );
					if ( not CheckStr( sIdxName ) ) then
						sIdxName := Format( 'Unamed_%d', [j] );
					k := FKIndexes.AddScript( sTblName + CH_EQUAL_TOKEN + sIdxName, sIndexes );
					sl.AddObject( IntToStr( i ) + CH_EQUAL_TOKEN + IntToStr( j ), TObject( LongInt( k ) ) );
				end;

{ variable names aren't semantical anymore }

			sIdxName := StringReplace( sTables, SQLBUILD_ENVVAR_INDEXES,
				{ '/* Indexes will be here... */' }'', krfAll );
			j := FKTables.AddScript( sTblName, sIdxName );
			DoBuild( i, j, sbtTable );
			FKTables.SubScripts[j] := StringReplace( sTables,
				SQLBUILD_ENVVAR_INDEXES, FKIndexes.ScriptByName[sTblName], krfAll );
			if ( IndexCount[i] > 0 ) then
				for j := 0 to IndexCount[i] - 1 do
				begin
					k := sl.IndexOf( IntToStr( i ) + CH_EQUAL_TOKEN + IntToStr( j ) );
					if ( k <> -1 ) then
					begin
						k := Integer( sl.Objects[k] );
						DoBuild( j, k, sbtIndex );
					end;
				end;
			for j := 0 to sl.Count - 1 do
				sl.Objects[j] := nil;
			sl.Clear;



		end;
	finally
		sl.Free;
	end;
	if ScriptCleared then
		FScript := StringReplace( FScript, SQLBUILD_ENVVAR_TABLES, sSBEmptyDB, krfAll )
	else
	begin
		FScript := StringReplace( FScript, SQLBUILD_ENVVAR_TABLES, FKTables.Script, krfAll );
		FScript := StringReplace( FScript, SQLBUILD_ENVVAR_START, GetStartCommand, krfAll );
		FScript := StringReplace( FScript, SQLBUILD_ENVVAR_RUN, GetRunCommand, krfAll );
	end;
	sTblName := Format( SQLBUILD_ENVVAR_DBNAME, [BuildDBName] );
	FScript := StringReplace( FScript, SQLBUILD_ENVVAR_DBNAME, sTblName, krfAll );
	FScript := StringReplace( FScript, SQLBUILD_ENVVAR_FOOTER, BuildFooter, krfAll );
end;

{----------------------------- TKDBCustomSQLBuild ------------------------------}

{
	This class is the base class for DataBase Script generation. It implements the
	default DataBase support and the abstract methods for database entity definitions.
}

constructor TKDBCustomSQLBuild.Create( ASQLType: TKSQLScriptType );
begin
  inherited Create( ASQLType );
  FCurIdxCount := -1;
  FCurTblID := -1;
	FSessionName  := '';
  FDataBaseName := '';
  FTablePatterns := '*.*';
	FSystemTables := False;
	FExtensions := True;
  FDSList := TStringList.Create;
  FTable := TTable.Create( nil );
  FOwner := nil;
end;

constructor TKDBCustomSQLBuild.CreateLinked( ASQLType: TKSQLScriptType;
	const ASessionName, ADataBaseName: string );
begin
	Create( ASQLType );
	if ( not CheckTrimStr( ASessionName ) ) then
    SessionName := Session.SessionName
	else
		SessionName := ASessionName;
  if ( not CheckTrimStr( ADataBaseName ) ) then
    FDataBase := nil
  else
    DataBaseName := ADataBaseName;
end;

constructor TKDBCustomSQLBuild.CreateOwned( AOwner: TObject; ASQLType: TKSQLScriptType );
begin
	CreateLinked( ASQLType, '', '' );
  FOwner := AOwner;
end;

destructor TKDBCustomSQLBuild.Destroy;
begin
	FDataBase := nil;
	FSession := nil;
	FOwner := nil;
	FTable.Close;
	FreeClean( FTable );
	FreeClean( FDSList );
	inherited Destroy;
end;

procedure TKDBCustomSQLBuild.Assign( Source: TPersistent );
begin
	inherited Assign( Source );
	if CheckObjectClass( Source, TKDBCustomSQLBuild ) then
		with ( Source as TKDBCustomSQLBuild ) do
		begin
			Self.SessionName := SessionName;
			Self.DataBaseName := DataBaseName;
			Self.TablePatterns := TablePatterns;
			Self.SystemTables := SystemTables;
			Self.Extensions := Extensions;
		end;
end;

procedure TKDBCustomSQLBuild.SetInfo( Index: Integer; Value: string );
begin
	case Index of
		0: if ( Value <> FDataBaseName ) then
			 begin
				 FDataBaseName := Value;
				 FTable.Close;
				 FTable.DatabaseName := Value;
				 FTable.TableName := '';
				 FDSList.Clear;
				 if CheckTrimStr( Value ) then
				 begin
					 FDataBase := FSession.FindDatabase( Value );
					 if ( not CheckObject( FDataBase ) ) then
					 begin
						 try
							 FDataBase := FSession.OpenDatabase( Value );
						 except
							 FDataBase := nil;
							 RaiseExceptionFmt( EKDBSQLBuild, sErrDBCInvDBName, [Value] );
						 end;
					 end
					 else
						 FDataBase.Open;
					 if ( not FDatabAse.Connected ) then
						 RaiseExceptionFmt( EKDBSQLBuild, sErrDBCInvDBOpen , [Value] );
					 FSession.GetTableNames( FDataBaseName, TablePatterns, Extensions, SystemTables,
						 FDSList );
				 end;
			 end;
		1: if ( Value <> FSessionName ) then
			 begin
				 if ( not CheckTrimStr( Value ) ) then
					 Value := Session.SessionName;
				 FSession := Sessions.FindSession( Value );
				 if ( not CheckObject( FSession ) ) then
				 begin
					 FSession := Sessions.FindSession( FSessionName );
					 RaiseExceptionFmt( EKDBSQLBuild, sErrDBCInvSName, [Value] );
				 end;
				 FSession.Open;
				 if ( not FSession.Active ) then
					 RaiseExceptionFmt( EKDBSQLBuild, sErrDBCInvSOpen , [Value] );
				 FSessionName := Value;
				 FDataBaseName := '';
         FTable.Close;
         FTable.SessionName := Value;
         FTable.DatabaseName := '';
				 FTable.TableName := '';
				 FDataBase := nil;
				 FDSList.Clear;
			 end;
		2: if ( FTablePatterns <> Value ) then
				 FTablePatterns := Value;
	end;
end;

function TKDBCustomSQLBuild.GetOwnerObject: TObject;
begin
  Result := FOwner;
end;

function TKDBCustomSQLBuild.GetInfo( Index: Integer ): string;
begin
  case Index of
    0: Result := FDataBaseName;
    1: Result := FSessionName;
    2: Result := FTablePatterns;
	else
		Result := '';
	end;
end;

function TKDBCustomSQLBuild.GetSession: TSession;
begin
	Result := FSession;
  if ( not CheckObject( Result ) ) then
		RaiseExceptionFmt( EKDBSQLBuild, sErrDBCInvSName, [sNil] )
	else if ( not Result.Active ) then
		RaiseExceptionFmt( EKDBSQLBuild, sErrDBCInvSOpen, [Result.SessionName] );
end;

function TKDBCustomSQLBuild.GetDatabase: TDataBase;
begin
	Result := FDataBase;
	if ( not CheckObject( Result ) ) then
		RaiseExceptionFmt( EKDBSQLBuild, sErrDBCInvDBName, [sNil] )
	else if ( not Result.Connected ) then
		 RaiseExceptionFmt( EKDBSQLBuild, sErrDBCInvDBOpen , [Result.DataBaseName] );
end;

function TKDBCustomSQLBuild.GetTable( const TableName: string ): TTable;
begin
  Result := nil;
	if ( not CheckStrEqual( FTable.SessionName, SessionName ) ) or
		 ( not CheckStrEqual( FTable.DatabaseName, DataBaseName ) ) then
		RaiseExceptionFmt( EKDBSQLBuild, sErrDBSBInvTblSessionDB, [TableName] )
	else
	begin
		if ( not CheckStrEqual( FTable.TableName, TableName ) ) then
		begin
			FTable.Close;
      FTable.TableName := TableName;
		end;
		Result := FTable;
  end;
end;

function TKDBCustomSQLBuild.GetTblCount: Integer;
begin
  Result := FDSList.Count;
end;

function TKDBCustomSQLBuild.GetIdxCount( TableID: Integer ): Integer;
var
	tbl: TTable;
begin
	if ( TableID <> FCurTblID ) or ( FCurIdxCount = -1 ) then
  begin
		if ( not CheckStrings( FDSList ) ) then
			Result := 0
		else
    begin
			tbl := ActualTable[TablesList[TableID]];
			ForceObject( tbl );
			tbl.IndexDefs.Update;
			Result := tbl.IndexDefs.Count;
			FCurTblID := TableID;
			FCurIdxCount := Result;
		end;
  end
	else
		Result := FCurIdxCount;
end;

{------------------------------ TKDBAnsiSQLBuild -------------------------------}

constructor TKDBAnsiSQLBuild.CreateLinked( ASQLType: TKSQLScriptType;
	const ASessionName, ADataBaseName: string );
begin
	if ( ASQLType in [sstUnKnown] ) then
		RaiseExceptionFmt( EKSQLBuild, sErrSBInvSqlScriptType, [GetSQLTypeTag( ASQLType )] );
	inherited CreateLinked( ASQLType, ASessionName, ADataBaseName );
end;

procedure TKDBAnsiSQLBuild.SetSQLScriptType( Value: TKSQLScriptType );
begin
	inherited SetSQLScriptType( Value );
	FooterPattern := '';
	HeaderPattern := SQL_ANSI_HEADER_PATTERN;
	TablePattern := SQL_ANSI_TABLE_PATTERN;
	PrimKeyPattern := SQL_ANSI_PK_PATTERN;
	FieldPattern := SQL_ANSI_FIELD_PATTERN;
	FieldTypeDef := SQL_ANSI_FIELDTYPE_PATTERN;
	CheckPattern := SQL_ANSI_CK_PATTERN;
	IndexesPattern := SQL_ANSI_INDEX_PATTERN;
end;

function TKDBAnsiSQLBuild.BuildHeader: string;
begin
	Result := Format( HeaderPattern, ['%0:s', sSQLBDBAnsiDBComment] );
end;

function TKDBAnsiSQLBuild.BuildDBName: string;
begin
	Result := ActualDataBase.DataBaseName;
end;

function TKDBAnsiSQLBuild.BuildFooter: string;
begin
	Result := FooterPattern;
end;

function TKDBAnsiSQLBuild.ValidAndGetIndex( const TableID, IndexID :Integer ): TIndexDef;
var
  idxs: TIndexDefs;
begin
	idxs := ActualTable[TablesList.Strings[TableID]].IndexDefs;
	idxs.Update;
	Result := idxs.Items[IndexID];
	if ( not CheckObject( Result ) ) then
		RaiseExceptionFmt( EKDBSQLBuild, sErrSBInvIndexID, [TablesList.Strings[TableID], IndexID] );
end;

function TKDBAnsiSQLBuild.ValidAndGetPrimKey( const TableID :Integer ): TIndexDef;
var
	idxs: TIndexDefs;
	i: Integer;
begin
	Result := nil;
	idxs := ActualTable[TablesList.Strings[TableID]].IndexDefs;
	idxs.Update;
	for i := 0 to idxs.Count - 1 do
		if ( ixPrimary in idxs.Items[i].Options ) then
		begin
			Result := idxs.Items[i];
			Exit;
		end;
end;

function TKDBAnsiSQLBuild.BuildTable( const TableID: Integer ): string;
var
	b : Boolean;
	i : Integer;
	sFlds: string;
	sChecks: string;
	sPrimKey: string;
	sTblName: string;
begin
	if ( TableID >= FDSList.Count ) then
		RaiseExceptionFmt( EKSQLBuild, sErrSBInvTbl, [TableID] );
	b := CheckTrimStr( CheckPattern );
	sFlds := '';
	sChecks := '';
	sPrimKey := '';
	Result := '';
	sTblName := FDSList.Strings[TableID];
	with ActualTable[sTblName] do
	begin
		FCurTblID := TableID;
		FCurIdxCount := -1;
		FieldDefs.Update;
		if ( TableType = ttParadox ) or
			 ( not ( CheckStrContains( EXT_DBF, TableName ) or
							 CheckStrContains( EXT_TXT, TableName ) ) ) then
			sPrimKey := BuildPrimaryKey( TableID );
		for i := 0 to FieldDefs.Count - 1 do
		begin
			sFlds := sFlds + BuildField( TableID, i );

		end;
		if b then
			for i := 0 to Constraints.Count - 1 do
			 	sChecks := sChecks + BuildCheckConstraints( TableID, Constraints.Items[i] );
	end;
	if b then
	begin
		sChecks := AdjustQuoteAndCRLF( sChecks );
		if ( not CheckStr( sChecks ) ) then
		begin
			sPrimKey := AdjustQuoteAndCRLF( sPrimKey );
			if CheckStr( sPrimKey ) then
				sPrimKey := ( sPrimKey + CH_CRLF );
		end
		else
		begin
			sChecks := sChecks + CH_CRLF;
	//	sPrimKey := AdjustQuoteAndCRLF( sPrimKey );
		end;
	end
	else
		sPrimKey := AdjustQuoteAndCRLF( sPrimKey );
	if ( not CheckStr( sPrimKey ) ) then
		sFlds := AdjustQuoteAndCRLF( sFlds );
	if ( not CheckStr( sFlds ) ) then
		RaiseExceptionFmt( EKDBSQLBuild, sErrSBInvTblFlds, [GetTableName( sTblName )] );
	Result := Format( TablePattern, [sSQLBDBAnsiComment, sTblName] );
	Result := StringReplace( Result, SQLBUILD_ENVVAR_FIELDS, sFlds, krfAll );
	Result := StringReplace( Result, SQLBUILD_ENVVAR_PRIMKEY, sPrimKey, krfAll );
	Result := StringReplace( Result, SQLBUILD_ENVVAR_CHECKS, sChecks, krfAll );
end;

function TKDBAnsiSQLBuild.BuildField( const TableID, FieldID: Integer ): string;
var
	fld: TFieldDef;
begin
{ I trust that TableID and FieldID are Valid... and ActualTable has their fieldDefs Updated... }
	fld := ActualTable[TablesList.Strings[TableID]].FieldDefs.Items[FieldID];
	ForceObject( fld );
	Result := Format( FieldPattern, [GetTableName( TablesList.Strings[TableID] ),
		fld.Name, BuildType( fld ), BuildNullAble( fld ), BuildDefault( fld )] );
end;

function TKDBAnsiSQLBuild.BuildIndex( const TableID, IndexID: Integer ): string;
const
	IdxOpt: array[0..1] of string = ( 'UNIQUE', 'DESC' );
var
	idx: TIndexDef;
	sIdxOpt: string;
begin
	sIdxOpt := '';
	idx := ValidAndGetIndex( TableID, IndexID );
	if ( ixUnique in idx.Options ) then
		sIdxOpt := sIdxOpt + IdxOpt[0];
	if ( ixDescending in idx.Options ) then
		sIdxOpt := sIdxOpt + ' ' + IdxOpt[1];
	Result := Format( IndexesPattern, [sSQLBDBAnsiComment, sIdxOpt, Idx.Name,
		TablesList.Strings[TableID], '"%0:s"."' + StringReplace( idx.Fields, ';',
		'", "%0:s"."', krfAll ) + '"'] );
	Result := Format( Result, [GetTableName( TablesList.Strings[TableID] )] );
end;

function TKDBAnsiSQLBuild.BuildPrimaryKey( const TableID: Integer ): string;
var
	idx: TIndexDef;
begin
	idx := ValidAndGetPrimKey( TableID );
	if CheckObject( Idx ) then
	begin
		Result := Format( PrimKeyPattern, [idx.Name, '"%0:s"."' + StringReplace(
			idx.Fields, CH_LIST_TOKEN, '", "%0:s"."', krfAll ) + '"'] );
		Result := Format( Result, [GetTableName( TablesList.Strings[TableID] )] );
	end
	else
		Result := '';
end;

function TKDBAnsiSQLBuild.IsPrimaryKeyIndex( const TableID, IndexID: Integer ): Boolean;
begin
	Result := ( ixPrimary in ValidAndGetIndex( TableID, IndexID ).Options );
end;

function TKDBAnsiSQLBuild.BuildType( Field: TFieldDef ): string;
const
	FloatFldTypes = [ftBCD, ftFloat, ftCurrency];
var
	fldType: TFieldType;
	iPos: Integer;
begin
	fldType := Field.DataType;
	Result := FieldTypeDef[fldType];
	if ( Pos( '%d', Result ) > 0 ) then
		if ( fldType in FloatFldTypes ) then
		begin
			if ( fldType <> ftBCD ) then
				case fldType of
					ftFloat    : Result := Format( Result, [SQL_SCRIPT_FLOAT_PRECISION, SQL_SCRIPT_FLOAT_DIGITS] );
					ftCurrency : Result := Format( Result, [SQL_SCRIPT_CURRENCY_PRECISION, SQL_SCRIPT_CURRENCY_DIGITS] );
				end
			else
				Result := Format( Result, [SQL_SCRIPT_BCD_PRECISION, SQL_SCRIPT_BCD_DIGITS] );
			iPos := Pos( '(0,', Result );
			if ( iPos > 0 ) then
			begin
				Delete( Result, iPos, 3 );
				Insert( '(%d,', Result, iPos );
				Result := Format( Result, [SQL_SCRIPT_CURRENCY_DIGITS] );
			end;
		end
		else
		begin
			Result := Format( Result, [Field.Size] );
			iPos := Pos( '(0', Result );
			if ( iPos > 0 ) then
			begin
				Delete( Result, iPos, 2 );
				Insert( '(1', Result, iPos );
			end;
		end;
end;
  
function TKDBAnsiSQLBuild.BuildCheckConstraints( TableID: Integer; Constraint: TCheckConstraint ): string;
begin
	if CheckTrimStr( Constraint.CustomConstraint ) then
		Result := Format( CheckPattern, [Constraint.DisplayName, { need revision... } 
			Constraint.CustomConstraint] )
	else
		Result := '';
end;

function TKDBAnsiSQLBuild.BuildNullable( Field: TFieldDef ): string;
const                                    {False    , True   }
	sNull: array[Boolean] of string[8] = ( 'NOT NULL', 'NULL' );
begin
	Result := sNull[Field.Required];
end;

function TKDBAnsiSQLBuild.BuildDefault( Field: TFieldDef ): string;
const
	sCharDef: array[Boolean] of string[1] = ( '', '''' );
begin
	{ there is no default expression in FieldDef! }
	Result := '';
end;

{------------------------------ TKDBLocalSQLBuild ------------------------------}

procedure TKDBLocalSQLBuild.SetSQLScriptType( Value: TKSQLScriptType );
begin
	inherited SetSQLScriptType( Value );
	CheckPattern :=	'';
	FieldPattern := SQL_LOCAL_FIELD_PATTERN;
	PrimKeyPattern := SQL_LOCAL_PK_PATTERN;
	FieldTypeDef := SQL_LOCAL_FIELDTYPE_PATTERN;
end;

function TKDBLocalSQLBuild.BuildField( const TableID, FieldID: Integer ): string;
var
	fld: TFieldDef;
begin
{ I trust that TableID and FieldID are Valid... and ActualTable has
	their fieldDefs Updated... }
	fld := ActualTable[TablesList.Strings[TableID]].FieldDefs.Items[FieldID];
	ForceObject( fld );
	Result := Format( FieldPattern, [GetTableName( TablesList.Strings[TableID] ),
		fld.Name, BuildType( fld )] );
end;

function TKDBLocalSQLBuild.TableType( TableID: Integer ): TTableType;
begin
	if CheckStrContains( EXT_DBF, TablesList[TableID] ) then
		Result := ttDBase
	else if CheckStrContains( EXT_DB, TablesList[TableID] ) then
		Result := ttParadox
	else if CheckStrContains( EXT_TXT, TablesList[TableID] ) then
		Result := ttAscii
	else
		Result := ttDefault;
end;

function TKDBLocalSQLBuild.BuildType( Field: TFieldDef ): string;
const
	FloatFldTypes = [ftBCD, ftFloat, ftCurrency];
	SpecialFldTypes = [ftString, ftVarBytes, ftFloat];
	DBaseDangerousType = [ftBCD, ftFmtMemo, ftAutoInc, ftBytes];
var
	fldType: TFieldType;
	iPos: Integer;
begin
	fldType := Field.DataType;
	Result := FieldTypeDef[fldType];
	if CheckStrContains( '%d', Result ) then
		if ( fldType in FloatFldTypes ) then
		begin
			iPos := Field.Precision;
			if ( iPos < SQL_LOCAL_DEFAULT_PRECISION ) then
				iPos := SQL_LOCAL_DEFAULT_PRECISION;
			Result := Format( Result, [iPos, SQL_LOCAL_DEFAULT_DIGITS] );
		end
		else
		begin
			Result := Format( Result, [Field.Size] );
			if CheckStrContains( '(0)', Result ) then
				Delete( Result, Pos( '(0)', Result ), 3 );
		end;
	if ( fldType in DBaseDangerousType ) and ( CurrentTableID <> -1 ) and
		 ( TableType( CurrentTableID ) = ttDBase ) then
		RaiseExceptionFmt( EKDBSQLBuild, sErrDBSBInvDBaseType, [Integer(fldType)] );
end;

function TKDBLocalSQLBuild.BuildPrimaryKey( const TableID: Integer ): string;
var
	idx: TIndexDef;
begin
	idx := ValidAndGetPrimKey( TableID );
	if CheckObject( Idx ) then
	begin
		Result := Format( PrimKeyPattern, ['"%0:s"."' + StringReplace( idx.Fields,
			';', '", "%0:s"."', krfAll ) + '"'] );
		Result := Format( Result, [GetTableName( TablesList.Strings[TableID] )] );
	end
	else
		Result := '';
end;

{------------------------------- TKDBMSSQLBuild --------------------------------}

procedure TKDBMSSQLBuild.SetSQLScriptType( Value: TKSQLScriptType );
begin
	inherited SetSQLScriptType( Value );
	HeaderPattern := SQL_MSSQL_HEADER_PATTERN;
	TablePattern := SQL_MSSQL_TABLE_PATTERN;
	IndexesPattern := SQL_MSSQL_INDEX_PATTERN;
	FieldTypeDef := SQL_MSSQL_FIELDTYPE_PATTERN;
end;

procedure TKDBMSSQLBuild.BuildSQLScript;
begin
	inherited BuildSQLScript;
	if ( not ScriptCleared ) then
	begin
		InternalScript := StringReplace( InternalScript, SQLBUILD_MSSQL_ENVVAR_RUN,
			SQLBUILD_MSSQL_RUN_COMMAND, krfAll );
		InternalScript := StringReplace( InternalScript, SQLBUILD_MSSQL_ENVVAR_QUOTED,
			SQLBUILD_MSSQL_QUOTED_TOKEN_COMMAND, krfAll );
	end;
end;

{------------------------------- TKDBInterBSQLBuild --------------------------------}

procedure TKDBInterBSQLBuild.SetSQLScriptType( Value: TKSQLScriptType );
begin
	inherited SetSQLScriptType( Value );
	FieldTypeDef := SQL_IBASE_FIELDTYPE_PATTERN;
end;

function TKDBInterBSQLBuild.BuildType( Field: TFieldDef ): string;
const
	BlobFldTypes: set of TBlobType = [ftBlob..ftTypedBinary];
var
	iSize: Integer;
	fldType: TFieldType;
begin
  fldType := Field.DataType;
	if ( fldType in BlobFldTypes ) then
	begin
		Result := FieldTypeDef[fldType];
		iSize := Field.Size;
		if ( iSize = 0 ) then
			iSize := 1;
		if ( Pos( '%d', Result ) > 0 ) then
			Result := Format( Result, [iSize, IBASE_BLOB_SUBTYPES[fldType]] );
	end
	else
		Result := inherited BuildType( Field );
end;

end.
