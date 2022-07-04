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

unit ukdbdClasses;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Windows, Classes, Forms, Dialogs, DB, DBTables, DsgnIntf, uksydUtils,
  uksydClasses, ukrConsts, ukrdClasses, ukdbClasses, ukdbEngines;

type

	EKDBDClasses = class( EKDDB );

{
--------------------------------------------------------------------------------
---------------------------- Component Editors ---------------------------------
--------------------------------------------------------------------------------
}

{ TKSQLSctirpEditor }

	TKSQLScriptEditor = class( TDefaultEditor )
	public
		procedure ExecuteVerb( Index: Integer ); override;
		function GetVerb( Index: Integer ): string; override;
		function GetVerbCount: Integer; override;

	end;

{ TKSQLSctirpEditorEx }

	TKSQLScriptEditorEx = class( TKSQLScriptEditor )
	public
		procedure ExecuteVerb( Index: Integer ); override;
		function GetVerb( Index: Integer ): string; override;
		function GetVerbCount: Integer; override;

	end;

{ TKDBLogManagerEditor }

	TKDBLogManagerEditor = class( TComponentEditor )
	public
		procedure ExecuteVerb( Index: Integer ); override;
		function GetVerb( Index: Integer ): string; override;
		function GetVerbCount: Integer; override;

	end;

{
--------------------------------------------------------------------------------
----------------------------- Property Editors ---------------------------------
--------------------------------------------------------------------------------
}

{------------------------------- Generic Editors -------------------------------}

{ TKKeyActionsProperty }

	TKKeyActionsProperty = class( TClassProperty )
	private
		FList: TList;

		procedure QuickSort;
		procedure ClearList;
		procedure InternalProc( Prop: TPropertyEditor );

	public
		procedure GetProperties( Proc: TGetPropEditProc ); override;
		procedure Initialize; override;
		destructor Destroy; override;

	end;

{ TKDataSetActionProperty }

	TKDataSetActionProperty = class( TIntegerProperty )
	public
		function GetAttributes: TPropertyAttributes; override;
		function GetValue: string; override;
		procedure SetValue( const Value: String ); override;
		procedure GetValues( Proc: TGetStrProc ); override;

	end;

{ TKAggregateDataSetActionsProperty }

	TKAggregateDataSetActionsProperty = class( TKDialogClassProperty )
	private
	  function GetDataSet: TDataSet;
		procedure SetDSA( const Values: TKDataSetActions );
		function GetDSA: TKDataSetActions;

	protected
		function GetAddCaption: string; virtual;
		function EditKDataSetActionsSet( var iSetValue: TKDataSetActions ): Boolean; virtual;

	public
		function GetValue: string; override;
		procedure Edit; override;
		property DSActions: TKDataSetActions
						 read GetDSA write SetDSA;

	end;

{------------------------------- TKTable Editors -------------------------------}

{ TKDataBaseNameIntegrityItemProperty }

	TKDataBaseNameIntegrityItemProperty = class( TKDataBaseNameProperty )
	protected
		function GetDataSet: TDBDataSet; override;

	end;

{ TKTableNameIntegrityItemProperty }

	TKTableNameIntegrityItemProperty = class( TKTableNameProperty )
	protected
		function GetDataSet: TDBDataSet; override;
		procedure GetValueList( List: TStrings ); override;

	end;

{ TKLinkedFieldsProperty }

	TKLinkedFieldsProperty = class( TKDialogClassProperty )
	public
		function GetAttributes: TPropertyAttributes; override;
		function GetValue: string; override;
		procedure Edit; override;

	end;

{------------------------------- TKQuery Editors -------------------------------}

{ TKQueryKeyFieldsProperty }

	TKQueryKeyFieldsProperty = class( TStringProperty )
	private
		function GetDataSet: TDataSet;
		
	public
		function GetAttributes: TPropertyAttributes; override;
		procedure Edit; override;

	end;

{----------------------------- TKSQLScript Editors -----------------------------}

{ TKQueryItemLinkToProperty }

	TKQueryItemLinkToProperty = class( TKCompNameProperty )
	protected
		function GetFilterClass: TComponentClass; override;
		function ProcessValue( GetComp_i: TPersistent; BaseComp: TComponent;
			const Value: string ): Boolean; override;

	end;

{ TKQueryItemParamsProperty }

	TKQueryItemParamsProperty = class( TPropertyEditor )
	public
		function GetValue: string; override;
		function GetAttributes: TPropertyAttributes; override;

	end;

{ TKQueryItemDatabaseNameProperty }

	TKQueryItemDatabaseNameProperty = class( TKDatabaseNameProperty )
	protected
		function GetDataSet: TDBDataSet; override;

	end;

{ TKQueryItemSessionNameProperty }

	TKQueryItemSessionNameProperty = class( TKSessionNameProperty )
	protected
		function GetDataSet: TDBDataSet; override;

	end;

{ TKSQLScriptExDatabaseNameProperty }

	TKSQLScriptExDatabaseNameProperty = class( TKDatabaseNameProperty )
	protected
		procedure GetValueList( List: TStrings ); override;

	end;

{ TKSQLScriptExSessionNameProperty }

	TKSQLScriptExSessionNameProperty = class( TKSessionNameProperty )
	protected
		procedure GetValueList( List: TStrings ); override;

	end;

  { TKDBLogEngineTableNameProperty }

	TKDBLogEngineTableNameProperty = class( TKDBStringProperty )
	protected
		procedure GetValueList( List: TStrings ); override;
		function GetDataSet: TDBDataSet; override;

  public
    procedure SetValue( const Value: string ); override;

	end;

  { TKDBLogTableNameProperty }

	TKDBLogTableNameProperty = class( TKDBStringProperty )
	protected
		procedure GetValueList( List: TStrings ); override;
		function GetDataSet: TDBDataSet; override;
    
  public
    procedure SetValue( const Value: string ); override;

	end;

implementation

uses
	SysUtils, Controls, TypInfo, uksyConsts, uksyUtils, uksydInternal, ukrUtils,
	ukrDBUtils, ukrClasses, ukrdUtils, ukdbConsts, ukdbUtils, ukdbScript,	ukdbTables,
	ukdbdConsts, ukdbfKeyFields, ukdbfLinkedFields, ukdbfDataSetActions;

{
--------------------------------------------------------------------------------
---------------------------- Component Editors ---------------------------------
--------------------------------------------------------------------------------
}

{ TKSQLScriptEditor }

procedure TKSQLScriptEditor.ExecuteVerb( Index: Integer );
var
	s: string;
begin
	s := '';
	case Index of
		0:
		begin
			s := SelectLoadFile( sSQLSFilter, sSQLSTitle, CurrentProjectPath, QUERY_ITEMS_DEFAULT_FILE_EXT );
			if CheckFile( s ) then
			begin
				Screen.Cursor := crHourGlass;
				try
					( Component as TKSQLScript ).LoadItemsFromFile( s );
				finally
					Screen.Cursor := crDefault;
				end;
				Inform( sSQLSInformLoad );
			end;
		end;
		1:
		begin
			s := SelectSaveFile( sSQLSFilter, sSQLSTitle, CurrentProjectPath, QUERY_ITEMS_DEFAULT_FILE_EXT );
			if ( ( not CheckFile( s ) ) or
					 ( CheckFile( s ) and Confirm( sSQLReplaceFile ) ) ) then
			begin
				Screen.Cursor := crHourGlass;
				try
					( Component as TKSQLScript ).SaveItemsToFile( s );
				finally
					Screen.Cursor := crDefault;
				end;
				Inform( sSQLSInformSave );
			end;
		end;
		2: { do nothing };
		3:
		begin
			if ( not ConfirmFmt( sSSEScriptPrint,
				[( Component as TKSQLScript ).QueryCollection.Count] ) ) then
				Exit;
			s := '.';
			Screen.Cursor := crHourGlass;
			try
				PrintSQLScripts( ( Component as TKSQLScript ) );
			finally
				Screen.Cursor := crDefault;
			end;
			Inform( sSQLSInformPrint );
		end;
	else
		Exit;
	end;
	if CheckStr( s ) then
		Designer.Modified;
end;

function TKSQLScriptEditor.GetVerb( Index: Integer ): string;
begin
	Result := '';
	case Index of
		0: Result := sSQLSLoad;
		1: Result := sSQLSSave;
		2: Result := sSQLSSpace;
		3: Result := sSQLSPrint;
	end;
end;

function TKSQLScriptEditor.GetVerbCount: Integer;
const
	VERB_COUNT: array[Boolean] of Integer = ( 0, SCRIPT_VERBCOUNT );
begin
	Result := VERB_COUNT[CheckCollection( ( Component as TKSQLScript ).QueryCollection )];
end;

{ TKSQLScriptEditorEx }

procedure TKSQLScriptEditorEx.ExecuteVerb( Index: Integer );

	procedure SQLEXLoad;
	var
		s: string;
	begin
		s := SelectLoadFile( sSQLSExFilter, sSQLSExTitle, CurrentProjectPath, SQL_DEFAULT_FILE_EXT );
		if CheckFile( s ) then
		begin
			Screen.Cursor := crHourGlass;
			try
				( Component as TKSQLScriptEx ).LoadScriptsFromFile( s );
			finally
				Screen.Cursor := crDefault;
			end;
			Inform( sSQLSExInformLoad );
		end;
	end;

	procedure SQLEXBuild;
	begin
		Screen.Cursor := crHourGlass;
		try
			( Component as TKSQLScriptEx ).BuildScript;
		finally
			Screen.Cursor := crDefault;
		end;
		Inform( sSQLSExBuild2 );
	end;

	procedure SQLEXCreateDB;
	var
		sl: TStrings;
		ItemIndex: Integer;
	begin
		sl := TStringList.Create;
		try
			( Component as TKSQLScriptEx ).DBSQLB.ActualSession.GetDatabaseNames( sl );
			ItemIndex := sl.IndexOf( ( Component as TKSQLScriptEx ).DataBaseName );
			if ( ItemIndex <> -1 ) then
				sl.Delete( ItemIndex );
			if ( not CheckStrings( sl ) ) then
				RaiseException( EKDBDClasses, sErrSQLEXInvAvailDBNames );
			ItemIndex := InputListDialog( sSQLSExCreateDBTitle, sSQLSExCreateDBText, 0, sl );
			if ( ItemIndex = -1 ) then
				Exit;
			Screen.Cursor := crHourGlass;
			try
				( Component as TKSQLScriptEx ).CreateDataBase( sl[ItemIndex] );
			finally
				Screen.Cursor := crDefault;
			end;
			Inform( sSQLSExDBCreated );
		finally
			sl.Free;
		end;
	end;

begin
	if ( Index <= ( inherited GetVerbCount - 1 ) ) then
	begin
		inherited ExecuteVerb( Index );
		Exit;
	end
	else
		case inherited GetVerbCount of
			0:
				case Index of
					0: SQLExLoad;
					1: SQLEXBuild;
					2: SQLEXCreateDB;
				end;
			SCRIPT_VERBCOUNT:
				case ( Index - inherited GetVerbCount ) of
					0: { do nothing };
					1: SQLExLoad;
					2: SQLEXBuild;
					3: SQLEXCreateDB;
				end;
		end;
	Designer.Modified;
end;

{Dia da alteração 22/03/1999 - não tá funcionando direito.}
function TKSQLScriptEditorEx.GetVerb( Index: Integer ): string;
begin
	Result := '';
	if ( Index <= ( inherited GetVerbCount - 1 ) ) then
		Result := inherited GetVerb( Index )
	else
		case inherited GetVerbCount of
			0:
				case Index of
					0: Result := sSQLSExLoad;
					1: Result := sSQLSExBuild;
					2: Result := sSQLSExCreateDB;
				end;
			SCRIPT_VERBCOUNT:
				case ( Index - inherited GetVerbCount ) of
					0: Result := sSQLSSpace;
					1: Result := sSQLSExLoad;
					2: Result := sSQLSExBuild;
					3: Result := sSQLSExCreateDB;
				end;
		end;
end;

function TKSQLScriptEditorEx.GetVerbCount: Integer;
begin
	Result := ( inherited GetVerbCount + SCRIPTEX_VERBCOUNT + Ord( inherited GetVerbCount > 0 ) );
end;

{ TKDBLogManagerEditor }

procedure TKDBLogManagerEditor.ExecuteVerb( Index: Integer );
var
  sFileName: string;
begin
  sFileName := '';
	case Index of
		0: { MergeFromFile }
		begin
			sFileName := SelectLoadFiles( sDBLMFilter, sDBLMMergeTitle, ApplicationPath,
        sDBLMDefExt, [ofHideReadOnly, ofPathMustExist, ofFileMustExist], nil );
			if CheckTrimStr( sFileName ) then
				( Component as TKDBLogManager ).MergeFromFile( sFileName );
		end;
		1: { LoadFromFile }
		begin
			sFileName := SelectLoadFiles( sDBLMFilter, sDBLMLoadTitle, ApplicationPath,
        sDBLMDefExt, [ofHideReadOnly, ofPathMustExist, ofFileMustExist], nil );
			if CheckTrimStr( sFileName ) then
				( Component as TKDBLogManager ).LoadFromFile( sFileName );
		end;
		2: { SaveToFile }
		begin
			sFileName := SelectSaveFiles( sDBLMFilter, sDBLMSaveTitle, ApplicationPath,
				sDBLMDefExt, [ofHideReadOnly, ofPathMustExist], nil );
			if CheckTrimStr( sFileName ) then
			begin
				if ( CheckFile( sFileName ) and ( not ConfirmFmt( sDBLMConfirmDel, [sFileName] ) ) ) then
					Exit;
				( Component as TKDBLogManager ).SaveToFile( sFileName );
			end;
		end;
	end;
	if CheckTrimStr( sFileName ) then
		Designer.Modified;
end;

function TKDBLogManagerEditor.GetVerb( Index: Integer ): string;
begin
	Result := DBLOGMANAGER_VERBS[Index];
end;

function TKDBLogManagerEditor.GetVerbCount: Integer;
begin
	Result := DBLOGMANAGER_VERBCOUNT;
end;

{
--------------------------------------------------------------------------------
----------------------------- Property Editors ---------------------------------
--------------------------------------------------------------------------------
}

{------------------------------- Generic Editors -------------------------------}

destructor TKKeyActionsProperty.Destroy;
begin
	ClearList;
	FreeClean( FList );
	inherited Destroy;
end;

procedure TKKeyActionsProperty.Initialize;
begin
	inherited Initialize;
	FList := TList.Create;
end;

procedure TKKeyActionsProperty.ClearList;
begin
	while CheckList( FList ) do
	begin
		TKPropertyEditor( FList.Last ).Free;
		FList.Delete( FList.Count - 1 );
	end;
end;

procedure TKKeyActionsProperty.InternalProc( Prop: TPropertyEditor );
begin
	FList.Add( TKPropertyEditor.CreateFrom( Prop ) );
end;

procedure TKKeyActionsProperty.QuickSort;

	procedure InternalQuickSort( iStart, iEnd: Integer );
	var
		iAfter,
		iBefore,
		iHalfIndex: Integer;
	begin
		iBefore := iStart;
		iAfter := iEnd;
		iHalfIndex := TKPropertyEditor( FList.Items[( iBefore + iAfter ) div 2] ).PropList^[0].PropInfo^.Index;
		repeat
			while ( TKPropertyEditor( FList.Items[iBefore] ).PropList^[0].PropInfo^.Index < iHalfIndex ) do
				Inc( iBefore );
			while ( iHalfIndex < TKPropertyEditor( FList.Items[iAfter] ).PropList^[0].PropInfo^.Index ) do
				Dec( iAfter );
			if ( iBefore <= iAfter ) then
			begin
				FList.Exchange( iBefore, iAfter );
				Inc( iBefore );
				Dec( iAfter );
			end;
		until ( iBefore > iAfter );
		if ( iStart < iAfter ) then
			InternalQuickSort( iStart, iAfter );
		if ( iBefore < iEnd ) then
			InternalQuickSort( iBefore, iEnd );
	end;

begin
	InternalQuickSort( 0, FList.Count - 1 );
end;

procedure TKKeyActionsProperty.GetProperties( Proc: TGetPropEditProc );
var
	i: Integer;
begin
	inherited GetProperties( InternalProc );
	ForceList( FList ); { The property editor MUST HAVE sub properties... }
	QuickSort;
	for i := 0 to FList.Count - 1 do
		Proc( TKPropertyEditor( FList.Items[i] ).Editor );
	ClearList;
end;

{ TKDataSetActionProperty }

procedure TKDataSetActionProperty.GetValues( Proc: TGetStrProc );
begin
	GetKDataSetActionValues( Proc );
end;

procedure TKDataSetActionProperty.SetValue( const Value: String );
begin
	SetOrdValue( Integer( StringToKDataSetAction( Value ) ) );
end;

function TKDataSetActionProperty.GetValue: string;
begin
	Result := KDataSetActionToString( TKDataSetAction( GetOrdValue ) );
end;

function TKDataSetActionProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [paValueList, paMultiSelect, paRevertable];
end;

{ TKAggregateDataSetActionsProperty }

function TKAggregateDataSetActionsProperty.GetDataSet: TDataSet;
begin
	Result := nil;
	if CheckObjectClass( GetComponent( 0 ), TDataSet ) then
		Result := ( GetComponent( 0 ) as TDataSet )
	else if CheckObjectClass( GetComponent( 0 ), TKQueryItem ) then
		Result := ( GetComponent( 0 ) as TKQueryItem ).Query
	else
		RaiseException( EKDBDClasses, sErrDSAPropInvClass );
end;

function TKAggregateDataSetActionsProperty.GetValue: string;
var
	i: TKDataSetAction;
begin
	Result := '[';
	for i := Low( TKDataSetAction ) to High( TKDataSetAction ) do
		if ( i in DSActions ) then
			Result := Result + dsaNames[i] + ', ';
	if ( Length( Result ) > 1 ) then
	  Delete( Result, Length( Result ) - 1, 2 );		
	Result := Result + ']';
end;

procedure TKAggregateDataSetActionsProperty.Edit;
var
	iSetValue: TKDataSetActions; 
begin
	iSetValue := DSActions;
	if EditKDataSetActionsSet( iSetValue ) then
	begin
		DSActions := iSetValue;
		Designer.Modified;
	end;
end;

function TKAggregateDataSetActionsProperty.GetAddCaption: string;
begin
	Result := GetDataSet.Name;
end;

function TKAggregateDataSetActionsProperty.EditKDataSetActionsSet(
	var iSetValue: TKDataSetActions ): Boolean;
const
	HISETVALUE: array[Boolean] of TKDataSetAction = ( dsaServerYield, dsaExecSQL );
begin
{ Derived Property Editor classes can use this method for customization pruposes }
	Result := KDataSetActionsEdit( iSetValue, GetAddCaption, dsaOpen,
		{ Choose TKQuery because it's smaller thant TKTable }
		HISETVALUE[CheckObjectClass( GetDataSet, TKQuery )] );
end;

procedure TKAggregateDataSetActionsProperty.SetDSA( const Values: TKDataSetActions );
var
	ds: TDataSet;
begin
	ds := GetDataSet;
	if ( ds is TKTable ) then
		( ds as TKTable ).DSActions := Values
	else if ( ds is TKQuery ) then
		( ds as TKQuery ).DSActions := Values
  else
		RaiseException( EKDBDClasses, sErrDSAPropInvClass );
end;

function TKAggregateDataSetActionsProperty.GetDSA: TKDataSetActions;
var
	ds: TDataSet;
begin
  Result := [];
	ds := GetDataSet;
	if ( ds is TKTable ) then
		Result :=	( ds as TKTable ).DataSetActions.DSActions
	else if ( ds is TKQuery ) then
		Result := ( ds as TKQuery ).DataSetActions.DSActions
	else
		RaiseException( EKDBDClasses, sErrDSAPropInvClass );
end;

{------------------------------- TKTable Editors -------------------------------}

{ TKDataBaseNameIntegrityItemProperty }

function TKDataBaseNameIntegrityItemProperty.GetDataSet: TDBDataSet;
begin
	Result := ( GetComponent( 0 ) as TKCustomDBIntegrityItem ).Owner.DataSet;
end;

{ TKTableNameIntegrityItemProperty }

function TKTableNameIntegrityItemProperty.GetDataSet: TDBDataSet;
begin
	Result := ( GetComponent( 0 ) as TKTableDBIntegrityItem ).Owner.DataSet;
end;

procedure TKTableNameIntegrityItemProperty.GetValueList( List: TStrings );
var
	i: Integer;
begin
	inherited GetValueList( List );
{ Exclude the owner tablename for integrity check }
	i := List.IndexOf( ( DataSet as TKTable ).TableName );
	if ( i <> -1 ) then
		List.Delete( i );
end;

{ TKLinkedFieldsProperty }

function TKLinkedFieldsProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes - [paMultiSelect];
end;

function TKLinkedFieldsProperty.GetValue: string;
var
	sl: TStrings;
begin
	Result := '';
	sl := TStrings( GetOrdValue );
	if CheckObject( sl ) then
		Result := MakeString( sl, CH_LIST_TOKEN, stNames );
	if ( not CheckStr( Result ) ) then
		Result := sLinkedFldEmpty;
end;
    
procedure TKLinkedFieldsProperty.Edit;
var
	sl: TKStrings;
	tdbi: TKTableDBIntegrityItem;
	isPrim: Boolean;
begin
	sl := TKStrings( GetOrdValue );
	tdbi := ( GetComponent( 0 ) as TKTableDBIntegrityItem );
{
	IMPORTANT:
	----------

	Rolling back a set of table operations when cached updates is true will work only
	if the linked fields defined for the TKTableDBIntegrityItem object are part of the
	primary key for the table. Since only the TKCascadeItem is the one changing the
	database, it should be possible to rollback its actions on the database: thus,
	only when the TKTableDBIntegrityItem is TKCascadeItem and ChachedUpdates is true,
	the LinkedFields property has to be part of the primary key. 
}
	isPrim := CheckObjectClass( tdbi, TKCascadeItem ) and ( tdbi.Owner.DataSet.CachedUpdates );
{
	SetOrdValue( LongInt( sl ) ); !! Don't add this line !! Just the line below instead.
	This occurs because sl is a persistent object that doesn't define any published
	property, it defines instead a hidden property for the strings values. For this reason
	we don't need to call SetOrdValue. It's not a real RTTI property!
}
	if EditLinkedFields( tdbi, sl, VALID_LINKEDFIELD_TYPES, isPrim ) then
		Modified;
end;

{------------------------------- TKQuery Editors -------------------------------}

{ TKQueryKeyFieldsProperty }

function TKQueryKeyFieldsProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [paReadOnly, paDialog] - [paMultiSelect];
end;

function TKQueryKeyFieldsProperty.GetDataSet: TDataSet;
begin
	Result := nil;
	if CheckObjectClass( GetComponent( 0 ), TDataSet ) then
		Result := ( GetComponent( 0 ) as TDataSet )
	else if CheckObjectClass( GetComponent( 0 ), TKQueryItem ) then
		Result := ( GetComponent( 0 ) as TKQueryItem ).Query
	else
		RaiseException( EKDBDClasses, sErrDSAPropInvClass );
end;

procedure TKQueryKeyFieldsProperty.Edit;
var
	kfld: string;
	ds: TKQuery;
begin
	kfld := GetStrValue;
	ds := ( GetDataSet as TKQuery );
	if EditKeyFields( ds, ds.Name, kfld, VALID_KEYFIELD_TYPES ) then
		SetStrValue( kfld );
end;

{----------------------------- TKSQLScript Editors -----------------------------}

function TKQueryItemLinkToProperty.GetFilterClass: TComponentClass;
begin
	Result := TComponent;
end;

function TKQueryItemLinkToProperty.ProcessValue( GetComp_i: TPersistent;
	BaseComp: TComponent; const Value: string ): Boolean;
begin
	Result := CheckPointer( TypInfo.GetPropInfo( BaseComp.ClassInfo, 'DataSet' ) );
end;

{ TKParamsProperty }

function TKQueryItemParamsProperty.GetValue: string;
begin
	Result := Format( '( %s )', [TParams.ClassName] );
end;

function TKQueryItemParamsProperty.GetAttributes: TPropertyAttributes;
begin
	Result := [paMultiSelect, paDialog];
end;

{ TKQueryItemDataBaseNameProperty }

function TKQueryItemDataBaseNameProperty.GetDataSet: TDBDataSet;
begin
	Result := ( GetComponent( 0 ) as TKQueryItem ).Query;
end;

{ TKQueryItemSessionNameProperty }

function TKQueryItemSessionNameProperty.GetDataSet: TDBDataSet;
begin
	Result := ( GetComponent( 0 ) as TKQueryItem ).Query;
end;

{ TKSQLScriptExDatabaseNameProperty }

procedure TKSQLScriptExDatabaseNameProperty.GetValueList( List: TStrings );
var
	ssn: string;
	ss: TSession;
begin
	ssn := ( GetComponent( 0 ) as TKSQLScriptEx ).SessionName;
	if ( not CheckTrimStr( ssn ) ) then
		ssn := Session.SessionName;
	ss := Sessions.OpenSession( ssn );
	List.Clear;
	if CheckObject( ss ) then
  	ss.GetDatabaseNames( List );
end;

{ TKSQLScriptExSessionNameProperty }

procedure TKSQLScriptExSessionNameProperty.GetValueList( List: TStrings );
begin
  Sessions.GetSessionNames( List );
end;

{ TKDBLogEngineTableNameProperty }

function TKDBLogEngineTableNameProperty.GetDataSet: TDBDataSet;
begin
  Result := ( GetComponent( 0 ) as TKDBLogEngine ).Query;
end;

procedure TKDBLogEngineTableNameProperty.GetValueList( List: TStrings );
begin
  ForceObject( List );
	with ( DataSet as TQuery ) do
		DBSession.GetTableNames( DatabaseName, '*.*', True, False, List );
end;

procedure TKDBLogEngineTableNameProperty.SetValue( const Value: string );
begin
  with ( DataSet as TQuery ) do
    if ( CheckTrimStrs( [DataBaseName, Value] ) and
      CheckTableName( DataBaseName, Value ) ) then
      WarnFmt( sWarnInvDBLogEngineTblName, [Value, DataBaseName] );
  inherited SetValue( Value );
end;

{ TKDBLogTableNameProperty }

function TKDBLogTableNameProperty.GetDataSet: TDBDataSet;
begin
  Result := ( GetComponent( 0 ) as TKDBLog ).Query;
end;

procedure TKDBLogTableNameProperty.GetValueList( List: TStrings );
begin
  ForceObject( List );
	with ( DataSet as TQuery ) do
		DBSession.GetTableNames( DatabaseName, '*.*', True, False, List );
end;

procedure TKDBLogTableNameProperty.SetValue( const Value: string );
begin
  with ( DataSet as TQuery ) do
    if ( CheckTrimStrs( [DataBaseName, Value] ) and
      ( ( ( ( GetComponent( 0 ) as TKDBLog ).DBLogType = dbltCreateInsert ) and
        CheckTableName( DataBaseName, Value ) ) or
      ( ( ( GetComponent( 0 ) as TKDBLog ).DBLogType = dbltInsert ) and
        ( not CheckTableName( DataBaseName, Value ) ) ) ) ) then
      WarnFmt( sWarnInvDBLogTblName, [Value, DataBaseName] );
  inherited SetValue( Value );
end;

end.
