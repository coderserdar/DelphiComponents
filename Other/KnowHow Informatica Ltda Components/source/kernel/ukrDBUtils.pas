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

unit ukrDBUtils;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Classes, Controls, DB, DBTables, BDE, DBGrids, uksyUtils, ukrClasses,
	ukrUtils;

type

	EKRDBUtils = class( EKKernel );

{
--------------------------------------------------------------------------------
--------------------------- General DB Utilities -------------------------------
--------------------------------------------------------------------------------
}

{------------------------------ BDE API Utilities ------------------------------}

	PKSQLDesc = ^TKSQLDesc;
	TKSQLDesc = record
		szDatabase:  DBINAME;
		szTableName: DBITBLNAME;
		szFieldName: DBINAME;
	end;

function GetSQLFldDesc( Query: TQuery; List: TList ): Integer;
function GetSQLFldDescEx( const DataBaseName, ASQLText: string; List: TList ): Integer;

{--------------------------- DB Check/Force Utilities --------------------------}

function CheckSessionName( const SessionName: string ): Boolean;
function CheckDatabaseName( const DBName: string ): Boolean;
function CheckTableName( const DBName, TblName: string ): Boolean;
function CheckStoredProcName( const DBName, StrProcName: string ): Boolean;

procedure ForceSessionName( const SessionName: string );
procedure ForceDatabaseName( const DBName: string );
procedure ForceTableName( const DBName, TblName: string );
procedure ForceStoredProcName( const DBName, StrProcName: string );

function CheckDataset( Dataset: TDataset ): Boolean;
function CheckDatasetEditing( Dataset: TDataset ): Boolean;
function CheckDatasetEmpty( Dataset: TDataset ): Boolean;
function CheckDataSetReadOnly( DataSet: TDataSet ): Boolean;

procedure ForceDataset( Dataset: TDataset );
procedure ForceDatasetEditing( Dataset: TDataset );
procedure ForceDatasetEmpty( Dataset: TDataset );
procedure ForceDataSetReadOnly( DataSet: TDataSet );

function CheckDataSource( DataSource: TDataSource ): Boolean;
procedure ForceDataSource( DataSource: TDataSource );

function FindDatasetGrid( AParent: TWinControl; ADataset: TDataset ): TDBGrid;
function FindDatasetControl( AParent: TWinControl; AControlClass: TControlClass; ADataset: TDataset ): TControl;

{------------------------------ DB Info Utilities ------------------------------}

function GetDatabaseDir( const DatabaseName: string ): string;
function GetTableDir( tbl: TTable ): string;
function GetLocalShare: string;
function SetLocalShare( Value: Boolean ): Boolean;
function GetNetFileDir: string;
function SetNetFileDir( const Value: string ): Boolean;
function GetBDEInfo( const Path, Node: string ): string;
function SetBDEInfo( const Path, Node, Value: string ): Boolean;

{------------------------------ Dataset Utilities ------------------------------}

function GetTableType( const TableName: string ): TTableType; {$IFDEF DELPHI4}overload;
function GetTableType( Table: TTable ): TTableType; overload; {$ENDIF}

function IsActiveCtrlLinked( ADataset: TDataset ): Boolean;
function CheckActiveCtrlLinkedDataField( ADataset: TDataset; var DataField: string ): Boolean;
function CheckActiveDBGridLinked( ADataset: TDataset; var DataField: string ): Boolean;

{--------------------------- TTable Index Utilities ----------------------------}

function MakeIndexOptions( Options: TIndexOptions ): Byte;
function RetrieveIndexOptions( IndexOptionsSet: Byte ): TIndexOptions;
function RetrieveIndexOptionsEx( IndexOptionsSet: Byte ): string;

{------------------------------- TField Utilities ------------------------------}

type
	TFieldTypes = set of TFieldType;

const
	VALID_LINKEDFIELD_TYPES = [ftString..ftWord, ftFloat..ftBCD];
	VALID_KEYFIELD_TYPES    = [ftString..ftWord, ftFloat..ftBCD, ftDate..ftDateTime, ftAutoInc];

{ Generic }

function DataTypeToStr( dt: TFieldType ): string;
function StrToDataType( const FieldType: string ): TFieldType;

procedure FillFieldValues( ADataset: TDataset; sl: TKStrings );
function ExtractFieldName( const Fields: string; var APos: Integer ): string;

{ Field Checking }

procedure ForceFields( ADataset: TDataset; const Fields: string;
	ValidTypes: TFieldTypes );
procedure ForceFieldsEx( ADataset: TDataset; Fields: TKStrings;
	ValidTypes: TFieldTypes; SType: TKStringType );
function CheckFieldsExPos( ADataset: TDataset; Fields: TKStrings;
	ValidTypes: TFieldTypes; SType: TKStringType ): Integer;

{ Primary Key Checking }

procedure ForcePrimaryKeyFields( ADataset: TTable; const Fields: string;
	ValidTypes: TFieldTypes );
procedure ForcePrimaryKeyFieldsEx( ADataset: TTable; Fields: TKStrings;
	ValidTypes: TFieldTypes; SType: TKStringType );
function CheckPrimaryKeyFieldsExPos( ADataset: TTable; Fields: TKStrings;
	ValidTypes: TFieldTypes; SType: TKStringType ): Integer;

{ Field Bitmask Representation (via TField) }

function MakeFields( ADataset: TDBDataset; const Fields: string; IsFirst32: Boolean ): Integer;
function MakeFieldsOrder( ADataset: TDBDataset; const Fields: string ): string;
function RetrieveFields( ADataset: TDBDataset; FieldsSet: Integer; IsFirst32: Boolean ): string;

{ Field Bitmask Representation (via TFieldDef) }

function MakeFieldsEx( AFieldDefs: TFieldDefs; const Fields: string; IsFirst32: Boolean ): Integer;
function MakeFieldsOrderEx( AFieldDefs: TFieldDefs; const Fields: string ): string;
function RetrieveFieldsEx( AFieldDefs: TFieldDefs; FieldsSet: Integer; IsFirst32: Boolean ): string;

procedure FreeCopyBuffer( ds: TDataset; Buffer: Pointer );
function CopyRecord( ds: TDataset; NoUnique: Boolean ): Pointer;
procedure PasteRecord( ds: TDataset; Buffer: Pointer; Overwrite: Boolean );

const
	TABLE_TYPE_MASKS: array[TTableType] of string =
		( '', '*.DB', '*.DBF', {$IFDEF DELPHI4}'*.DBF' { for pro extension ?},
		  {$ENDIF} '*.TXT' );

implementation

uses
	SysUtils, TypInfo, Forms, uksyTypes, uksyConsts, ukrResStr, ukrConsts;

{
--------------------------------------------------------------------------------
--------------------------- General DB Utilities -------------------------------
--------------------------------------------------------------------------------
}

{------------------------------ BDE API Utilities ------------------------------}

{
	This function will return the passed TList filled with fields read from
	a	SQLStatement into allocated pointers of PKSQLDesc type. It is the
	caller's responsibility to dispose these pointers. The number of fields
	read from the statement is List.Count. The return value is the actual
	number of fields returned by the statement. These values are the same
	in about "95%" <g> of the cases. One known situation in which these
	values aren't	the same has already been found: when an unqualified
	SELECT statement performs a select on all fields of more than one
	table.

		SELECT t.*, a.* FROM orders t, customer a ( will work OK! )
		SELECT * FROM orders ( will work OK! )
		SELECT t.*, a.*, orderno FROM orders t, customer a ( will work OK! )

		SELECT * FROM orders, customer ( will NOT work OK! )

	In the above statement, the List.Count will be 9 ( fields of Orders ),
	and Result will be 19 ( fields of Orders + Customer )!

	You must provide a valid (BDE registered) DataBaseName, a valid
	SQLText, and a valid TList object ( that is cleared before processing ).

	PS: If any errors occur during the field extraction process, the list
			items will be freed	before the exception is raised.	For
			GetSQLFldDesc, you must provide an openned query.
}

{
-----------------------------------------------------------
-------------------- Extended Library ---------------------
-----------------------------------------------------------
}

{----- Copy/Paste Record Architecture -----}

type
	PDataEntry = ^TDataEntry;
	TDataEntry = Record
		Data: Pointer;
		Blob: Boolean;
		Empty: Boolean;
		Name: ShortString;
	end;
	PDataEntryArray = ^TDataEntryArray;
	TDataEntryArray = array[0..MaxListSize - 1] of PDataEntry;

function CopyRecordBuffer( ds: TDataset; NoUnique: Boolean ): Pointer;
var
	i,
	j,
	sz: Integer;
	id: TIndexDef;
	pdea: PDataEntryArray;
begin
	j := 0;
	sz := 0;
	for i := 0 to ds.FieldCount - 1 do
		if ( ds.Fields[i].FieldKind = fkData ) then
			Inc( sz );
	pdea := AllocMem( ( sz + 1 ) * SizeOf( PDataEntry ) );
	try
		for i := 0 to ds.FieldCount - 1 do
			with ds.Fields[i] do
				if ( FieldKind = fkData ) then
				begin
					pdea^[j] := AllocMem( SizeOf( TDataEntry ) );
					try
						pdea^[j]^.Blob := IsBlob;
						pdea^[j]^.Name := FieldName;
						pdea^[j]^.Empty := IsNull or ( DataType = ftAutoInc );
						if ( not pdea^[j]^.Empty ) then
						begin
							if pdea^[j]^.Blob then
							begin
								pdea^[j]^.Data := TMemoryStream.Create;
								TBlobField( ds.Fields[i] ).SaveToStream( TMemoryStream( pdea^[j]^.Data ) );
							end
							else
							begin
	{ do not copy data from fields that are part of a unique key of a TTable object }
								if ( ( ds is TTable ) and NoUnique ) then
								begin
									id := TTable( ds ).IndexDefs.GetIndexForFields( FieldName, false );
									if CheckObject( id ) then
										pdea^[j]^.Empty := ( ixPrimary in id.Options ) or ( ixUnique in id.Options );
								end;
								if ( not pdea^[j]^.Empty ) then
								begin
									GetMem( pdea^[j]^.Data, DataSize );
									GetData( pdea^[j]^.Data );
								end;
							end;
						end;
					except
						if CheckPointer( pdea^[j] ) then
						begin
							if CheckPointer( pdea^[j]^.Data ) then
							begin
								if ds.Fields[i].IsBlob then
									TMemoryStream( pdea^[j]^.Data ).Free
								else
									FreeMem( pdea^[j]^.Data, ds.Fields[i].DataSize );
							end;
							FreeMem( pdea^[j], SizeOf( TDataEntry ) );
							pdea^[j] := nil;
						end;
						raise;
					end;
					Inc( j );
				end;
	except  
		FreeCopyBuffer( ds, pdea );
		raise;
	end;
	Result := pdea;
end;

procedure PasteRecordBuffer( ds: TDataset; Buffer: Pointer; Overwrite: Boolean );
var
	i,
	sz: Integer;
	f: TField;
	pdea: PDataEntryArray;
begin
	i := 0;
	pdea := PDataEntryArray( Buffer );
	while CheckPointer( pdea^[i] ) do
		Inc( i );
	sz := i;
	if ( not ( ds.State in dsEditModes ) ) then
		ds.Insert;
	for i := 0 to sz - 1 do
		if CheckPointer( pdea^[i]^.Data ) and ( not pdea^[i]^.Empty ) then
		begin
			f := ds.FindField( pdea^[i]^.Name );
			if ( CheckObject( f ) and ( f.FieldKind = fkData ) ) then
			begin
				if ( f.IsNull or ( Overwrite and ( not f.IsNull ) ) ) then
					if f.IsBlob then
						TBlobField( f ).LoadFromStream( TMemoryStream( pdea^[i]^.Data ) )
					else
						f.SetData( pdea^[i]^.Data );
			end;
		end;
end;

function CopyRecord( ds: TDataset; NoUnique: Boolean ): Pointer;
begin
	ForceDataset( ds );
	if ( ds.State <> dsBrowse ) then
		RaiseException( EKRDBUtils, sErrCopyPasteState );
	if CheckDatasetEmpty( ds ) then
		RaiseException( EKRDBUtils, sErrCopyEmpty );
	Result := CopyRecordBuffer( ds, NoUnique );
end;

procedure PasteRecord( ds: TDataset; Buffer: Pointer; Overwrite: Boolean );
begin
	ForceDataset( ds );
	ForcePointer( Buffer );
	if ( ds.State <> dsBrowse ) then
		RaiseException( EKRDBUtils, sErrCopyPasteState );
	PasteRecordBuffer( ds, Buffer, Overwrite );
end;

procedure FreeCopyBuffer( ds: TDataset; Buffer: Pointer );
var
	i,
	sz: Integer;
	pdea: PDataEntryArray;
begin
	if ( not CheckPointer( Buffer ) ) then
		Exit;
	i := 0;
	pdea := PDataEntryArray( Buffer );
	while CheckPointer( pdea^[i] ) do
		Inc( i );
	sz := i;
	for i := sz - 1 downto 0 do
		if CheckPointer( pdea^[i] ) then
		begin
			if CheckPointer( pdea^[i]^.Data ) then
			begin
				if ds.Fields[i].IsBlob then
					TMemoryStream( pdea^[i]^.Data ).Free
				else
					FreeMem( pdea^[i]^.Data );
			end;
			FreeMem( pdea^[i], SizeOf( TDataEntry ) );
		end;
	FreeMem( Pointer( pdea ), ( sz + 1 ) * SizeOf( PDataEntry ) );
end;

{--------------------------- Internal Implementation ---------------------------}

procedure InternalGetSQLFldDesc( STMTHandle: hDBIStmt; List: TList );
var
	hCur: hDBICur;
	rslt: DBIResult;
	Descs: STMTBaseDesc;
	psd: PKSQLDesc;
begin
	hCur := nil;
	try
		DBTables.Check( DbiQGetBaseDescs( STMTHandle, hCur ) );
		try
			repeat
				rslt := DbiGetNextRecord( hCur, dbiNOLOCK, @Descs, nil );
				if ( rslt = DBIERR_NONE ) then
				begin
					psd := New( PKSQLDesc );
					ZeroMemory( psd, SizeOf( TKSQLDesc ) );
					psd^.szDatabase := Descs.szDataBase;
					psd^.szTableName := Descs.szTableName;
					psd^.szFieldName := Descs.szFieldName;
					List.Add( psd );
				end
				else
					if ( rslt <> DBIERR_EOF ) then
						DBTables.Check( rslt );
			until ( rslt <> DBIERR_NONE );
		except
			on EDBEngineError do
			begin
				while CheckList( List ) do
				begin
					Dispose( PKSQLDesc( List.Last ) );
					List.Delete( List.Count - 1 );
				end;
				raise;
			end;
		end;
	finally
		if CheckPointer( hCur ) then
			DBTables.Check( DbiCloseCursor( hCur ) );
	end;
end;

{---------------------------- Public Implementation ----------------------------}

function GetSQLFldDesc( Query: TQuery; List: TList ): Integer;
begin
	ForceTrim( [Query, List] );
	if ( not CheckTrimStr( Query.SQL.Text ) ) then
		RaiseException( EKRDBUtils, sErrInvOriginNULLSQLText );
	if ( not Query.Active ) then
		RaiseExceptionFmt( EKRDBUtils, sErrInvOriginQueryState, [Query.Name] );
	InternalGetSQLFldDesc( Query.StmtHandle, List );
	Result := Query.FieldCount;
end;

function GetSQLFldDescEx( const DataBaseName, ASQLText: string; List: TList ): Integer;
var
	qy: TQuery;
begin
	ForceObject( List );
	if ( not CheckTrimStr( ASQLText ) ) then
		RaiseException( EKRDBUtils, sErrInvOriginNULLSQLText );
	ForceDatabaseName( DataBaseName );
	qy := TQuery.Create( nil );
	try
		qy.DataBaseName := DataBaseName;
		qy.SQL.Text := ASQLText;
		qy.Open;
		InternalGetSQLFldDesc( qy.StmtHandle, List );
		Result := qy.FieldCount;
	finally
		qy.Free;
	end;
end;

{--------------------------- DB Check/Force Utilities --------------------------}

function CheckSessionName( const SessionName: string ): Boolean;
var
	sl: TStrings;
begin
	ForceTrimStr( SessionName );
	Result := CheckStrEqual( Session.SessionName, SessionName );
	if Result then
		Exit;
	sl := TStringList.Create;
	try
		Sessions.GetSessionNames( sl );
		Result := ( sl.IndexOf( SessionName ) > -1 );
	finally
		sl.Free;
	end;
end;

function CheckDatabaseName( const DBName: string ): Boolean;
var
	sl: TStrings;
begin
	ForceTrimStr( DBName );
	sl := TStringList.Create;
	try
		Session.GetDataBaseNames( sl );
		Result := ( sl.IndexOf( DBName ) > -1 );
	finally
		sl.Free;
	end;
end;

function CheckTableName( const DBName, TblName: string ): Boolean;
var
	sl: TStrings;
begin
  Result := ( CheckTrimStr( TblName ) and CheckDataBaseName( DBName ) );
  if Result then
  begin
    sl := TStringList.Create;
    try
      Session.GetTableNames( DBName, '*.*', True, True, sl );
      Result := ( sl.IndexOf( TblName ) > -1 );
    finally
      sl.Free;
    end;
  end;  
end;

function CheckStoredProcName( const DBName, StrProcName: string ): Boolean;
var
	sl: TStrings;
begin
	ForceTrimStr( StrProcName );
	ForceDataBaseName( DBName );
	sl := TStringList.Create;
	try
		Session.GetStoredProcNames( DBName, sl );
		Result := ( sl.IndexOf( StrProcName ) > -1 );
	finally
		sl.Free;
	end;
end;

procedure ForceSessionName( const SessionName: string );
begin
	if ( not CheckSessionName( SessionName ) ) then
		RaiseExceptionFmt( EKRDBUtils, sErrInvSessionName, [SessionName] );
end;

procedure ForceDatabaseName( const DBName: string );
begin
	if ( not CheckDatabaseName( DBName ) ) then
		RaiseExceptionFmt( EKRDBUtils, sErrInvDBName, [DBName] );
end;

procedure ForceTableName( const DBName, TblName: string );
begin
	if ( not CheckTableName( DBName, TblName ) ) then
		RaiseExceptionFmt( EKRDBUtils, sErrInvTblName, [DBName, TblName] );
end;                               

procedure ForceStoredProcName( const DBName, StrProcName: string );
begin
	if ( not CheckStoredProcName( DBName, StrProcName ) ) then
		RaiseExceptionFmt( EKRDBUtils, sErrInvStoredProcName, [DBName, StrProcName] );
end;

function CheckDataset( Dataset: TDataset ): Boolean;
begin
	Result := CheckObject( Dataset ) and ( Dataset.Active );
end;

procedure ForceDataset( Dataset: TDataset );
begin
	if ( not CheckDataset( Dataset ) ) then
		RaiseException( EKRDBUtils, sErrInvDataset );
end;

function CheckDatasetEditing( Dataset: TDataset ): Boolean;
begin
	Result := CheckDataset( Dataset ) and ( Dataset.State in dsEditModes );
end;

procedure ForceDatasetEditing( Dataset: TDataset );
begin
	if ( not CheckDatasetEditing( Dataset ) ) then
		RaiseException( EKRDBUtils, sErrInvDatasetEditing );
end;

function CheckDatasetEmpty( Dataset: TDataset ): Boolean;
begin
	Result := CheckDataset( Dataset ) and ( Dataset.BOF and Dataset.BOF );
end;

function CheckDataSetReadOnly( DataSet: TDataSet ): Boolean;
begin
	Result := ( CheckObject( DataSet ) and ( not DataSet.CanModify ) );
end;

procedure ForceDatasetEmpty( Dataset: TDataset );
begin
	if ( not CheckDatasetEmpty( Dataset ) ) then
		RaiseException( EKRDBUtils, sErrInvDatasetEmpty );
end;

function CheckDataSource( DataSource: TDataSource ): Boolean;
begin
	Result := CheckObject( DataSource ) and CheckObject( DataSource.Dataset );
end;

procedure ForceDataSource( DataSource: TDataSource );
begin
	if ( not CheckDataSource( DataSource ) ) then
		RaiseException( EKRDBUtils, sErrInvDataSource );
end;

procedure ForceDataSetReadOnly( DataSet: TDataSet );
begin
	if ( not CheckDataSetReadOnly( DataSet ) ) then
		RaiseException( EKRDBUtils, sErrInvDatasetReadOnly );
end;

function FindDatasetControl( AParent: TWinControl; AControlClass: TControlClass; ADataset: TDataset ): TControl;
var
	i: Cardinal;
	pi: PPropInfo;
	ds: TDataSource;
	AControl: TControl;
	AClass: TControlClass;
begin
	AClass := AControlClass;
	if ( not CheckClass( AClass ) ) then
		AClass := TControl;
	if CheckObjects( [AParent, ADataset] ) then
		for i := 0 to AParent.ControlCount - 1 do
			if CheckObjectClass( AParent.Controls[i], AClass ) then
			begin
				AControl := AParent.Controls[i];
				pi := GetPropInfo( TypeInfo( TDataSource ), 'DataSource' );
				if CheckPointer( pi ) then
				begin
					ds := TDataSource( GetOrdProp( AControl, pi ) );
					if ( CheckDataSource( ds ) ) and ( ds.Dataset = ADataset ) then
					begin
						Result := AControl;
						if ( CheckObjectClass( Result, TWinControl ) and
								 TWinControl( Result ).Focused ) then
							Exit;
					end;
				end;
			end;
	Result := nil;		
end;

function FindDatasetGrid( AParent: TWinControl; ADataset: TDataset ): TDBGrid;
begin
	Result := TDBGrid( FindDatasetControl( AParent, TDBGrid, ADataset ) );
end;

{------------------------------ DB Info Utilities ------------------------------}

function GetDatabaseDir( const DatabaseName: string ): string;
var
	sDir: string;
	vDBDesc: DBDesc;
begin
	DBTables.Check( DbiGetDataBaseDesc( PChar( DatabaseName ), @vDBDesc ) );
	sDir := Format( '%s', [vDBDesc.szPhyName] );
	Result := sDir;
end;

function GetTableDir( tbl: TTable ): string;
var
	sDir: string;
	vDBDesc: DBDesc;
begin
	DBTables.Check( DbiGetDataBaseDesc( PChar( tbl.DatabaseName ), @vDBDesc ) );
	sDir := Format( '%s\%s', [vDBDesc.szPhyName, tbl.TableName] );
	Result := sDir;
end;

function GetLocalShare: string;
begin
	Result := GetBDEInfo( '\System\Init', 'Local Share' );
end;

function SetLocalShare( Value: Boolean ): Boolean;
begin
	Result := SetBDEInfo( '\System\Init', 'Local Share', BOOL_NAME[Value] );
end;

function GetNetFileDir: string;
begin
	Result := GetBDEInfo( '\Drivers\Paradox\Init', 'Net Dir' );
end;

function SetNetFileDir( const Value: string ): Boolean;
begin
	Result := SetBDEInfo( '\Drivers\Paradox\Init', 'Net Dir', Value );
end;

function GetBDEInfo( const Path, Node: string ): string;
var
	hCur: hDbiCur;
	pDesc: CfgDesc;
begin
  ForceTrimStrs( [Path, Node] );
	Result := '';
	ZeroMemory( @pDesc, SizeOf( CfgDesc ) );
	DBTables.Check( DbiInit( nil ) );
	try
		DBTables.Check( DbiOpenCfgInfoList( nil, DbiReadOnly, cfgPersistent,
			PChar( Path ), hCur ) );
		try
			while ( DbiGetNextRecord( hCur, DbiWriteLock, @pDesc, nil ) = DBIERR_NONE ) do
				if CheckStrEqual( StrPas( pDesc.szNodeName ), Node ) then
				begin
					Result := pDesc.szValue;
					Break;
				end;
		finally
			DBTables.Check( DbiCloseCursor( hCur ) );
		end;
	finally
		DbiExit;
	end;
end;

function SetBDEInfo( const Path, Node, Value: string ): Boolean;
var
	hCur: hDbiCur;
	pDesc: CfgDesc;
begin
  ForceTrimStrs( [Path, Node] );
	Result := false;
	ZeroMemory( @pDesc, SizeOf( CfgDesc ) );
	DBTables.Check( DbiInit( nil ) );
	try
		DBTables.Check( DbiOpenCfgInfoList( nil, DbiReadWrite, cfgPersistent,
			PChar( Path ), hCur ) );
		try
			while ( DbiGetNextRecord( hCur, DbiWriteLock, @pDesc, nil ) = DBIERR_NONE ) do
				if CheckStrEqual( StrPas( pDesc.szNodeName ), Node ) then
				begin
					StrPCopy( pDesc.szValue, Value );
					DBTables.Check( DbiModifyRecord( hCur, @pDesc, true ) );
					Result := true;
					Break;
				end;
		finally
			DBTables.Check( DbiCloseCursor( hCur ) );
		end;
	finally
		DbiExit;
	end;
end;

{------------------------------ Dataset Utilities ------------------------------}

function GetTableType( const TableName: string ): TTableType;
begin
	ForceTrimStr( TableName );
	if CheckStrContains( TABLE_TYPE_MASKS[ttDBase], TableName ) then
		Result := ttDBase
	else if CheckStrContains( TABLE_TYPE_MASKS[ttParadox], TableName ) then
		Result := ttParadox
	else if CheckStrContains( TABLE_TYPE_MASKS[ttASCII], TableName ) then
		Result := ttASCII
	else
		Result := ttDefault;
end;

{$IFDEF DELPHI4}

type
	TTableHack = class( TTable );

function GetTableType( Table: TTable ): TTableType;
begin
	ForceObject( Table );
	Result := TTableHack( Table ).GetTableType;
end;

{$ENDIF}


function IsActiveCtrlLinked( ADataset: TDataset ): Boolean;
var
	ppi: PPropInfo;
	ds: TDataSource;
	ctrl: TWinControl;
begin
  ForceObject( ADataset );
	ctrl := Screen.ActiveControl;
	Result := CheckObject( ctrl );
	if Result then
	begin
		ppi := GetPropInfo( ctrl.ClassInfo, 'DataSource' );
		Result := CheckPointer( ppi );
		if Result then
		begin
			ds := TDataSource( GetOrdProp( ctrl, ppi ) );
			Result := CheckObject( ds ) and CheckObject( ds.Dataset ) and
				( ds.Dataset = ADataset );
		end;
	end;
end;

function CheckActiveCtrlLinkedDataField( ADataset: TDataset; var DataField: string ): Boolean;
var
	ppi: PPropInfo;
	ctrl: TWinControl;
begin
  DataField := '';
	Result := IsActiveCtrlLinked( ADataset );
	if Result then
	begin
		ctrl := Screen.ActiveControl;
		Result := CheckObject( ctrl );
		if Result then
		begin
			ppi := GetPropInfo( ctrl.ClassInfo, 'DataField' );
			Result := CheckPointer( ppi );
			if Result then
			begin
				DataField := GetStrProp( ctrl, ppi );
				Result := CheckStr( DataField );
				if Result then
				begin
					if ADataset.Active then
						Result := CheckObject( ADataset.FindField( DataField ) )
					else
					begin
						ADataset.FieldDefs.Update;
						Result := CheckObject( ADataset.FieldDefs.Find( DataField ) );
					end;
				end;
			end;
		end;
	end;
end;

function CheckActiveDBGridLinked( ADataset: TDataset; var DataField: string ): Boolean;
var
	ctrl: TWinControl;
begin
	DataField := '';
	Result := IsActiveCtrlLinked( ADataset );
	if Result then
	begin
		ctrl := Screen.ActiveControl;
		Result := CheckObjectClass( ctrl, TDBGrid ) and
			CheckObject( TDBGrid( ctrl ).SelectedField );
		if Result then
		begin
			DataField := TDBGrid( ctrl ).SelectedField.FieldName;
			if ADataset.Active then
				Result := CheckObject( ADataset.FindField( DataField ) )
			else
			begin
				ADataset.FieldDefs.Update;
				Result := CheckObject( ADataset.FieldDefs.Find( DataField ) );
			end;
		end;
	end;
end;

{--------------------------- TTable Index Utilities ----------------------------}

function MakeIndexOptions( Options: TIndexOptions ): Byte;
var
	i: TKIntegerSet;
begin
	i := [];
	if ( ixPrimary in Options ) then
		Include( i, Integer( ixPrimary ) );
	if ( ixUnique in Options ) then
		Include( i, Integer( ixUnique ) );
	if ( ixDescending in Options ) then
		Include( i, Integer( ixDescending ) );
	if ( ixExpression in Options ) then
		Include( i, Integer( ixExpression ) );
	if ( ixCaseInsensitive in Options ) then
		Include( i, Integer( ixCaseInsensitive ) );
	Result := Byte( Integer( i ) );
end;

function RetrieveIndexOptions( IndexOptionsSet: Byte ): TIndexOptions;
var                                                 
	iSet: TKIntegerSet;
begin
	Integer( iSet ) := Integer( IndexOptionsSet );
	Result := [];
	if ( Integer( ixPrimary ) in iSet ) then
		Include( Result, ixPrimary );
	if ( Integer( ixUnique ) in iSet ) then
		Include( Result, ixUnique );
	if ( Integer( ixDescending ) in iSet ) then
		Include( Result, ixDescending );
	if ( Integer( ixExpression ) in iSet ) then
		Include( Result, ixExpression );
	if ( Integer( ixCaseInsensitive ) in iSet ) then
		Include( Result, ixCaseInsensitive );
end;

function RetrieveIndexOptionsEx( IndexOptionsSet: Byte ): string;
var
	iSet: TKIntegerSet;
begin
	Integer( iSet ) := Integer( IndexOptionsSet );
	Result := '[';
	if ( Integer( ixPrimary ) in iSet ) then
		Result := Result + 'ixPrimary, ';
	if ( Integer( ixUnique ) in iSet ) then
		Result := Result + 'ixUnique, ';
	if ( Integer( ixDescending ) in iSet ) then
		Result := Result + 'ixDescending, ';
	if ( Integer( ixExpression ) in iSet ) then
		Result := Result + 'ixExpression, ';
	if ( Integer( ixCaseInsensitive ) in iSet ) then
		Result := Result + 'ixCaseInsensitive, ';
	if ( Length( Result ) > 1 ) and ( Result[Length( Result )-1] = ',' ) then
		Delete( Result, Length( Result )-1, 2 );
	Result := Result + ']';
end;

{------------------------------- TField Utilities ------------------------------}

{ Generic }

function DataTypeToStr( dt: TFieldType ): string;
begin
	Result := EnumName( Ord( dt ), TypeInfo( TFieldType ) );
	if CheckStr( Result ) then
		Result := Copy( Result, 3, Length( Result ) - 2 );
end;

function StrToDataType( const FieldType: string ): TFieldType;
var
	i: TFieldType;
	s1, s2: string;
begin
	ForceTrimStr( FieldType );
	s1 := 'ft' + FieldType;
	for i := Low( TFieldType ) to High( TFieldType ) do
	begin
		s2 := EnumName( Ord( i ), TypeInfo( TFieldType ) ) ;
		if ( AnsiCompareText( s1, s2 ) = 0 ) then
		begin
			Result := i;
			Exit;
		end;
	end;
	Result := ftUnknown;
end;

procedure FillFieldValues( ADataset: TDataset; sl: TKStrings );
var
	i: Integer;
begin
  ForceObjects( [ADataset, sl] );
	with sl do
		for i := 0 to Count - 1 do
			ValuesByIndex[i] := ADataset.FieldByName( Names[i] ).AsString;
end;

function ExtractFieldName( const Fields: string; var APos: Integer ): string;
var
	i: Integer;
begin
	i := APos;
	while ( i <= Length( Fields ) ) and ( Fields[i] <> CH_LIST_TOKEN ) do
		Inc( i );
	Result := Trim( Copy( Fields, APos, i - APos ) );
	if ( i <= Length( Fields ) ) and ( Fields[i] = CH_LIST_TOKEN ) then
		Inc( i );
	APos := i;
end;

{ Field Checking }

procedure ForceFields( ADataset: TDataset; const Fields: string;
	ValidTypes: TFieldTypes );
var
	sl: TKStrings;
begin
	if ( not CheckStr( Fields ) ) then
		Exit;
	sl := TKStrings.Create;
	try
		ExtractStrings( Fields, ';', sl );
		ForceFieldsEx( ADataset, sl, ValidTypes, stStrings );
	finally
		sl.Free;
	end;
end;

procedure ForceFieldsEx( ADataset: TDataset; Fields: TKStrings;
	ValidTypes: TFieldTypes; SType: TKStringType );
var
	i: Integer;
	s: string;
begin
	i := CheckFieldsExPos( ADataset, Fields, ValidTypes, SType );
	if ( i <> -1 ) then
	begin
		case SType of
			stStrings: s := Fields.Strings[i];
			stNames  : s := Fields.Names[i];
			stValues : s := Fields.ValuesByIndex[i];
		else
		  s := '';	
		end;
		RaiseExceptionFmt( EKRDBUtils, sErrInvFldName, [s, ADataset.Name] );
	end;
end;

function CheckFieldsExPos( ADataset: TDataset; Fields: TKStrings;
	ValidTypes: TFieldTypes; SType: TKStringType ): Integer;
var
	i,
	j: Integer;
	s: string;
begin
	ForceObjects( [ADataset, Fields] );
	Result := -1;
	if ( not CheckStrings( Fields ) ) then
		Exit;
	s := '';
	ADataset.FieldDefs.Update;
	for i := 0 to Fields.Count - 1 do
	begin
		case SType of
			stStrings: s := Fields.Strings[i];
			stNames  : s := Fields.Names[i];
			stValues : s := Fields.ValuesByIndex[i];
		end;
		j := ADataset.FieldDefs.IndexOf( s );
		if ( j = -1 ) or not ( ADataset.FieldDefs.Items[j].DataType in ValidTypes ) then
		begin
			Result := i;
			Exit;
		end;
	end;
end;

{ Primary Key Checking }

procedure ForcePrimaryKeyFields( ADataset: TTable; const Fields: string;
	ValidTypes: TFieldTypes );
var
	sl: TKStrings;
begin
	if ( not CheckStr( Fields ) ) then
		Exit;
	sl := TKStrings.Create;
	try
		ExtractStrings( Fields, CH_LIST_TOKEN, sl );
		ForcePrimaryKeyFieldsEx( ADataset, sl, ValidTypes, stStrings );
	finally
		sl.Free;
	end;
end;

procedure ForcePrimaryKeyFieldsEx( ADataset: TTable; Fields: TKStrings;
	ValidTypes: TFieldTypes; SType: TKStringType );
var
	i: Integer;
begin
	i := CheckPrimaryKeyFieldsExPos( ADataset, Fields, ValidTypes, SType );
	if ( i <> -1 ) then
		RaiseExceptionFmt( EKRDBUtils, sErrInvPKFldName, [Fields[i], ADataset.Name] );
end;

function CheckPrimaryKeyFieldsExPos( ADataset: TTable; Fields: TKStrings;
	ValidTypes: TFieldTypes; SType: TKStringType ): Integer;
var
	i,
	j: Integer;
	s: string;
begin
	ForceObjects( [ADataset, Fields] );
	Result := -1;
	if ( not CheckStrings( Fields ) ) then
		Exit;
	s := '';
	ADataset.FieldDefs.Update;
	ADataset.IndexDefs.Update;
	for i := 0 to Fields.Count - 1 do
	begin
		case SType of
			stStrings: s := Fields.Strings[i];
			stNames  : s := Fields.Names[i];
			stValues : s := Fields.ValuesByIndex[i];
		end;
{See the case of primary key with numbers. Try a StrToIntDef and search for ID!}
		j := ADataset.FieldDefs.IndexOf( s );
		with ADataset, IndexDefs do
			if ( j = -1 ) or
				 ( ( FindIndexForFields( s ).Options * [ixPrimary] ) = [] ) or
					 ( not ( FieldDefs.Items[j].DataType in ValidTypes ) ) then
			begin
				Result := i;
				Exit;
			end;
	end;
end;

{ Field Bitmask Representation (via TField) }

function MakeFields( ADataset: TDBDataset; const Fields: string;
	IsFirst32: Boolean ): Integer;
var
	i: TKIntegerSet;
	iPos,
	iFldNo: Integer;
	bOldActive: Boolean;
begin
	i := [];
	iPos := 1;
	bOldActive := ADataset.Active;
	try
		ADataset.Open;
		while ( iPos <= Length( Fields ) ) do
		begin
			iFldNo := ADataset.FieldByName( ExtractFieldName( Fields, iPos ) ).FieldNo - 1;
			if ( not ( iFldNo in i ) ) then
			begin
				if ( IsFirst32 and
						 ( iFldNo <= ( SETSIZE_INTEGER - 1 ) ) ) or
					 ( ( not IsFirst32 ) and
						 ( iFldNo > ( SETSIZE_INTEGER - 1 ) ) )  then
					Include( i, iFldNo )
				else if ( iFldNo > ( ( 2 * SETSIZE_INTEGER ) - 1 ) ) then
					RaiseExceptionFmt( EKRDBUtils, sErrInvFldNo, [ADataset.Name, iFldNo] );
			end;
		end;
		Result := Integer( i );
	finally
		ADataset.Active := bOldActive;
	end;
end;

function MakeFieldsOrder( ADataset: TDBDataset; const Fields: string ): string;
var
	iPos,
	iFldNo: Integer;
	bOldActive: Boolean;
begin
	ForceTrim( [ADataset, Fields] );
	Result := '';
	iPos := 1;
	bOldActive := ADataset.Active;
	try
		ADataset.Open;
		while ( iPos <= Length( Fields ) ) do
		begin
			iFldNo := ADataset.FieldByName( ExtractFieldName( Fields, iPos ) ).FieldNo - 1;
			Result := Result + IntToStr( iFldNo ) + CH_LIST_TOKEN;
		end;
		if CheckStr( Result ) then
			Delete( Result, Length( Result ), 1 );
	finally
		ADataset.Active := bOldActive;
	end;
end;

function RetrieveFields( ADataset: TDBDataset; FieldsSet: Integer;
	IsFirst32: Boolean ): string;

	function GetFieldName( i: Integer ): string;
	begin
		Result := '';
		for i := 0 to ADataset.FieldCount - 1 do
			if ( ( ADataset.Fields[i].FieldNo - 1 ) = i ) then
			begin
				Result := ADataset.Fields[i].FieldName;
				Break;
			end;
		ForceStr( Result );
	end;

var
	i: Byte;
	iSet: TKIntegerSet;
	bOldActive: Boolean;
begin
	Integer( iSet ) := FieldsSet;
	Result := '';
	bOldActive := ADataset.Active;
	try
		ADataset.Open;
		for i := 0 to SETSIZE_INTEGER - 1 do
			if ( i in iSet ) then
			begin
				if IsFirst32 then
					Result := Result + GetFieldName( i ) + CH_LIST_TOKEN
				else
					Result := Result + GetFieldName( i + SETSIZE_INTEGER ) + CH_LIST_TOKEN;
			end;
		if CheckStr( Result ) then
			Delete( Result, Length( Result ), 1 );
	finally
		ADataset.Active := bOldActive;
	end;
end;

{ Field Bitmask Representation (via TFieldDef) }

function MakeFieldsEx( AFieldDefs: TFieldDefs; const Fields: string;
	IsFirst32: Boolean ): Integer;
var
	i: TKIntegerSet;
	iPos,
	iFldNo: Integer;
begin
	ForceTrim( [AFieldDefs, Fields] );
	i := [];
	iPos := 1;
	AFieldDefs.Update;
	while ( iPos <= Length( Fields ) ) do
	begin
		iFldNo := AFieldDefs.Find( ExtractFieldName( Fields, iPos ) ).FieldNo - 1;
		if ( not ( iFldNo in i ) ) then
		begin
			if ( IsFirst32 and
					 ( iFldNo <= ( SETSIZE_INTEGER - 1 ) ) ) or
				 ( ( not IsFirst32 ) and
					 ( iFldNo > ( SETSIZE_INTEGER - 1 ) ) )  then
				Include( i, iFldNo )
			else if ( iFldNo > ( ( 2 * SETSIZE_INTEGER ) - 1 ) ) then
				RaiseExceptionFmt( EKRDBUtils, sErrInvFldDefFldNo, [iFldNo] );
		end;
	end;
	Result := Integer( i );
end;

function MakeFieldsOrderEx( AFieldDefs: TFieldDefs; const Fields: string ): string;
var
	iPos,
	iFldNo: Integer;
begin
	ForceTrim( [AFieldDefs, Fields] );
	Result := '';
	iPos := 1;
	AFieldDefs.Update;
	while ( iPos <= Length( Fields ) ) do
	begin
		iFldNo := AFieldDefs.Find( ExtractFieldName( Fields, iPos ) ).FieldNo - 1;
		Result := Result + IntToStr( iFldNo ) + CH_LIST_TOKEN;
	end;
	if CheckStr( Result ) then
		Delete( Result, Length( Result ), 1 );
end;

function RetrieveFieldsEx( AFieldDefs: TFieldDefs; FieldsSet: Integer;
	IsFirst32: Boolean ): string;

	function GetFieldName( I: Integer ): string;
	begin
		Result := '';
		for i := 0 to AFieldDefs.Count - 1 do
			if ( ( AFieldDefs.Items[i].FieldNo - 1 ) = i ) then
			begin
				Result := AFieldDefs.Items[i].Name;
				Break;
			end;
		ForceStr( Result );
	end;

var
	i: Byte;
	iSet: TKIntegerSet;
begin
  ForceObject( AFieldDefs );
	Integer( iSet ) := FieldsSet;
	Result := '';
	AFieldDefs.Update;
	for i := 0 to SETSIZE_INTEGER - 1 do
		if ( i in iSet ) then
		begin
			if IsFirst32 then
				Result := Result + GetFieldName( i ) + CH_LIST_TOKEN
			else
				Result := Result + GetFieldName( i + SETSIZE_INTEGER ) + CH_LIST_TOKEN;
		end;
	if CheckStr( Result ) then
		Delete( Result, Length( Result ), 1 );
end;

end.
