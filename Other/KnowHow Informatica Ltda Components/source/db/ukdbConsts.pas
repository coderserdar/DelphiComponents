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

unit ukdbConsts;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Classes, DB, uksyConsts, ukrConsts;

const

{
--------------------------------------------------------------------------------
-------------------------- Generic DataSet Constants ---------------------------
--------------------------------------------------------------------------------
}

	DEFAULT_FLUSH_COUNT     = 5;
	DEFAULT_COMMITCACHESIZE = 5;

	EXT_DBF = '.DBF';
	EXT_DB  = '.DB';
	EXT_TXT = '.TXT';

	SQL_DEFAULT_FILE_EXT = '.sql';

{ Delayed DBError for process errors during char code messages }

	KM_DELAYED_DBERROR = KM_USER + $03;

	KM_OTRAN_END   = KM_USER + $04;
	KM_OTRAN_START = KM_USER + $05;
	KM_OTRAN_INC   = KM_USER + $06;
	KM_OTRAN_DEC   = KM_USER + $07;
	KM_OTRAN_ERROR = KM_USER + $08;

{$IFDEF KLIB100}
	DB_VER = '1.00';
	DB_VER_INT = 100;
	DB_VER_RELEASE_DATE = '25/07/1999 01:00:00';
{$ELSE}
	DB_VER = '?.??';
	DB_VER_INT = 0;
	DB_VER_RELEASE_DATE = '01/01/1900 00:00:00';
{$ENDIF}
	
{
--------------------------------------------------------------------------------
------------------------- DataSet Auditory Constants ---------------------------
--------------------------------------------------------------------------------
}

{ Audit Message for DataSetAuditor }

	KA_DATASET = KM_USER + $02;

{ Audit Events for DataSetAuditor }

	aeLogData    = $01;
	aeNotifyData = $02;

	K_HALF_MONTH = 15;
	K_100KB 		 = 100 * KB;

{ DataSet actions for each posible dataset event (after"s" and others, not before"s") }

	dsaUnknown			 = $00;
	dsaOpen				   = $01;
	dsaClose				 = $02;
	dsaCancel			   = $03;
	dsaPost				   = $04;
	dsaScroll			   = $05;
	dsaInsert			   = $06;
	dsaEdit				   = $07;
	dsaDelete			   = $08;
	dsaCalcFields	   = $09;
	dsaFilterRecord  = $0A; {must study a way to hook this action - see TBDEDataSet.CachedUpdateCallBack!}
	dsaNewRecord		 = $0B;
	dsaUpdateRecord  = $0C;
	dsaDeleteError	 = $0D;
	dsaEditError 	   = $0E;
	dsaPostError		 = $0F;
	dsaUpdateError	 = $10; {must study a way to hook this action - see TBDEDataSet.CachedUpdateCallBack!}
	dsaServerYield	 = $11; {must study a way to hook this action - see TBDEDataSet.YieldCallBack!}
	dsaCommitCache   = $12; {only for TKQuery protected Method}
	dsaExecSQL			 = $13;

	dsaDefActions = [dsaOpen, dsaClose, dsaCancel, dsaPost, dsaInsert, dsaEdit,
		dsaDelete, dsaNewRecord, dsaDeleteError, dsaPostError, dsaEditError];

	dsaAll       = [dsaOpen..dsaPostError] - [dsaFilterRecord];
	dsaEditModes = [dsaCancel..dsaDelete];
	dsaError     = [dsaDeleteError..dsaPostError];
	dsaNone      = [];

	dsaNames: array[dsaUnknown..dsaExecSQL] of string[15] =
	(
		'dsaUnknown',
		'dsaOpen',
		'dsaClose',
		'dsaCancel',
		'dsaPost',
		'dsaScroll',
		'dsaInsert',
		'dsaEdit',
		'dsaDelete',
		'dsaCalcFields',
		'dsaFilterRecord',
		'dsaNewRecord',
		'dsaUpdateRecord',
		'dsaDeleteError',
		'dsaEditError',
		'dsaPostError',
		'dsaUpdateError',
		'dsaServerYield',
		'dsaCommitCache',
		'dsaExecSQL'
	);

{ Map entries for the TKDataSetActions }

	dsaMapEntry: array[dsaUnKnown..dsaExecSQL] of TIdentMapEntry =
	(
		( Value: dsaUnknown		  ; Name: 'dsaUnknown' ),
		( Value: dsaOpen				; Name: 'dsaOpen' ),
		( Value: dsaClose			  ; Name: 'dsaClose' ),
		( Value: dsaCancel			; Name: 'dsaCancel' ),
		( Value: dsaPost				; Name: 'dsaPost' ),
		( Value: dsaScroll			; Name: 'dsaScroll' ),
		( Value: dsaInsert			; Name: 'dsaInsert' ),
		( Value: dsaEdit				; Name: 'dsaEdit' ),
		( Value: dsaDelete			; Name: 'dsaDelete' ),
		( Value: dsaCalcFields	; Name: 'dsaCalcFields' ),
		( Value: dsaFilterRecord; Name: 'dsaFilterRecord' ),
		( Value: dsaNewRecord	  ; Name: 'dsaNewRecord' ),
		( Value: dsaUpdateRecord; Name: 'dsaUpdateRecord' ),
		( Value: dsaDeleteError ; Name: 'dsaDeleteError' ),
		( Value: dsaEditError   ; Name: 'dsaEditError' ),
		( Value: dsaPostError	  ; Name: 'dsaPostError' ),
		( Value: dsaUpdateError ; Name: 'dsaUpdateError' ),
		( Value: dsaServerYield ; Name: 'dsaServerYield' ),
		( Value: dsaCommitCache ; Name: 'dsaCommitCache' ),
		( Value: dsaExecSQL     ; Name: 'dsaExecSQL' )
	);

{ Link Events for DataSetAuditor }

	leBeforePrepareDataSetLog = $01;
	leAfterPrepareDataSetLog  = $02;
	leBeforeDataSetLog 				= $03;
	leAfterDataSetLog  				= $04;

{ DB Auditory Log Extensions }

	DSL_TABLE_LOGEXT = '.ktl';
	DSL_QUERY_LOGEXT = '.kql';

  KDLE_TABLE_REPORT_EXTENTION = '.ktr';
  KDLE_QUERY_REPORT_EXTENTION = '.kqr';

  MAXSIZE_INTEGER_STR = 10; { Length( IntToStr( MaxInt ) ) } 

	DSA_CLASSNAME_FIELD   = 'Dataset.ClassName';
	DSA_NAME_FIELD        = 'Dataset.Name';
	DSA_OWNER_NAME_FIELD  = 'Dataset.OwnerName';
	DSA_CURACTION_FIELD   = 'Dataset.CurrentAction';
	DSA_DBNAME_FIELD      = 'Dataset.DataBaseName';
	DSA_AUDITGRPIDX_FIELD = 'Dataset.AuditGroupIndex';
	DSA_ADDINFO_FIELD     = 'Dataset.AddInfo';
	TBLA_TABLENAME_FIELD  = 'Table.TableName';
	TBLA_IDXFLDNAME_FIELD = 'Table.IndexFldName';
	QUYA_KEYFIELDS_FIELD  = 'Query.KeyFields';
	DSA_ADDINFO_FIELD_LEN = SizeOf( ShortString ) - 1;
	QUYA_KEYFIELDS_FIELD_LEN = DSA_ADDINFO_FIELD_LEN;

{
--------------------------------------------------------------------------------
------------------------------- Field Mappings ---------------------------------
--------------------------------------------------------------------------------
}

type

	TKFieldTypeByClass = record
		FieldClassName: ShortString;
		FieldType     : TFieldType;
	end;

	TKFieldValidCharsByClass = record
		FieldClassName : ShortString;
		FieldValidChars: TFieldChars;
	end;

	TKFieldDataSizeByClass = record
		FieldClassName : ShortString;
		FieldDataSize  : Word;
	end;

const

	DEFAULT_KFIELDCLASS_MAP: array[TFieldType] of TFieldClass =
	(
		nil,                { ftUnknown }
		TStringField,       { ftString }
		TSmallintField,     { ftSmallint }
		TIntegerField,      { ftInteger }
		TWordField,         { ftWord }
		TBooleanField,      { ftBoolean }
		TFloatField,        { ftFloat }
		TCurrencyField,     { ftCurrency }
		TBCDField,          { ftBCD }
		TDateField,         { ftDate }
		TTimeField,         { ftTime }
		TDateTimeField,     { ftDateTime }
		TBytesField,        { ftBytes }
		TVarBytesField,     { ftVarBytes }
		TAutoIncField,      { ftAutoInc }
		TBlobField,         { ftBlob }
		TMemoField,         { ftMemo }
		TGraphicField,      { ftGraphic }
		TBlobField,         { ftFmtMemo }
		TBlobField,         { ftParadoxOle }
		TBlobField,         { ftDBaseOle }
		TBlobField,         { ftTypedBinary }
		nil                 { ftCursor }
		{$IFDEF DELPHI4},
		TStringField,       { ftFixedChar }
		nil, { TWideStringField } { ftWideString }
		TLargeIntField,     { ftLargeInt }
		TADTField,          { ftADT }
		TArrayField,        { ftArray }
		TReferenceField,    { ftReference }
		TDataSetField       { ftDataSet }
		{$ENDIF}  
	);

	loFldClassMap = 0;
	hiFldClassMap = 22;

	FIELDTYPE_BY_FIELDCLASS_MAP:
		array[loFldClassMap..hiFldClassMap] of TKFieldTypeByClass =
	(
		( FieldClassName: '';	      				FieldType: ftUnknown  ),
		( FieldClassName: 'TField';					FieldType: ftUnknown  ),
		( FieldClassName: 'TStringField';		FieldType: ftString 	),
		( FieldClassName: 'TSmallintField';	FieldType: ftSmallint ),
		( FieldClassName: 'TIntegerField';	FieldType: ftInteger  ),
		( FieldClassName: 'TWordField';			FieldType: ftWord			),
		( FieldClassName: 'TBooleanField';	FieldType: ftBoolean	),
		( FieldClassName: 'TFloatField';		FieldType: ftFloat		),
		( FieldClassName: 'TCurrencyField';	FieldType: ftCurrency ),
		( FieldClassName: 'TBCDField';			FieldType: ftBCD			),
		( FieldClassName: 'TDateField';			FieldType: ftDate			),
		( FieldClassName: 'TTimeField';			FieldType: ftTime   	),
		( FieldClassName: 'TDateTimeField';	FieldType: ftDateTime ),
		( FieldClassName: 'TBytesField';		FieldType: ftBytes 		),
		( FieldClassName: 'TVarBytesField';	FieldType: ftVarBytes	),
		( FieldClassName: 'TAutoIncField';	FieldType: ftAutoInc	),
		( FieldClassName: 'TBlobField';			FieldType: ftBlob 		),
		( FieldClassName: 'TMemoField';	    FieldType: ftMemo     ),
		( FieldClassName: 'TGraphicField';	FieldType: ftGraphic  ),
		( FieldClassName: 'TBlobField';			FieldType: ftFmtMemo  ),
		( FieldClassName: 'TBlobField';			FieldType: ftParadoxOLE ),
		( FieldClassName: 'TBlobField';			FieldType: ftDBaseOLE ),
		( FieldClassName: 'TBlobField';			FieldType: ftTypedBinary )
	);

	loFldValidCharsMap = 0;
	hiFldValidCharsMap = 18;

	FIELD_VALIDCHARS_BY_FIELDCLASS_MAP:
		array[loFldValidCharsMap..hiFldValidCharsMap] of TKFieldValidCharsByClass =
	(
		( FieldClassName: '';	      				FieldValidChars: [] ),
		( FieldClassName: 'TField';					FieldValidChars: [#0..#255] ),
		( FieldClassName: 'TStringField';		FieldValidChars: [#0..#255] ),
		( FieldClassName: 'TSmallintField';	FieldValidChars: ['+', '-', '0'..'9'] ),
		( FieldClassName: 'TIntegerField';	FieldValidChars: ['+', '-', '0'..'9'] ),
		( FieldClassName: 'TWordField';			FieldValidChars: ['+', '-', '0'..'9'] ),
		( FieldClassName: 'TBooleanField';	FieldValidChars: [#0..#255] ),
		( FieldClassName: 'TFloatField';    FieldValidChars: ['+', '-', '0'..'9', 'E', 'e'] ),
		( FieldClassName: 'TCurrencyField';	FieldValidChars: ['+', '-', '0'..'9', 'E', 'e'] ),
		( FieldClassName: 'TBCDField';			FieldValidChars: ['+', '-', '0'..'9'] ),
		( FieldClassName: 'TDateField';			FieldValidChars: [#0..#255] ),
		( FieldClassName: 'TTimeField';			FieldValidChars: [#0..#255] ),
		( FieldClassName: 'TDateTimeField';	FieldValidChars: [#0..#255] ),
		( FieldClassName: 'TBytesField';		FieldValidChars: [#0..#255] ),
		( FieldClassName: 'TVarBytesField';	FieldValidChars: [#0..#255] ),
		( FieldClassName: 'TAutoIncField';	FieldValidChars: ['+', '-', '0'..'9'] ),
		( FieldClassName: 'TBlobField';			FieldValidChars: [#0..#255] ),
		( FieldClassName: 'TMemoField';	    FieldValidChars: [#0..#255] ),
		( FieldClassName: 'TGraphicField';	FieldValidChars: [#0..#255] ) );

{ Take a look at TFieldClass constructors }

	INVALID_FLD_SIZE  = 0;
	STRING_FLD_SIZE   = 20; {from 1..8192(dsMaxStringSize)}
	BYTES_FLD_SIZE    = 16; {from 1..??}
	VARBYTES_FLD_SIZE = 16; {from 1..??}
	BCD_FLD_SIZE      = 4;  {from 1..32}

{ Take a look at TFieldClass GetDataSize }
	WORD_FLD_SIZE     = SizeOf( Word );       { Fixed Field Sizes }
	SMALLINT_FLD_SIZE = SizeOf( SmallInt );
	INTEGER_FLD_SIZE  = SizeOf( Integer );
	AUTOINC_FLD_SIZE  = INTEGER_FLD_SIZE;
	FLOAT_FLD_SIZE    = SizeOf( Double );
	CURRENCY_FLD_SIZE = FLOAT_FLD_SIZE;
	BOOLEAN_FLD_SIZE  = SizeOf( WordBool );
	DATETIME_FLD_SIZE = SizeOf( TDateTime );
	DATE_FLD_SIZE     = INTEGER_FLD_SIZE;
	TIME_FLD_SIZE     = INTEGER_FLD_SIZE;

{ Suggested initial values }
	BYNARY_FLD_SIZE   = 16;      							{ From 0..?? }
	BLOB_FLD_SIZE     = INVALID_FLD_SIZE;
	MEMO_FLD_SIZE     = INVALID_FLD_SIZE;
	GRAPHIC_FLD_SIZE  = INVALID_FLD_SIZE;

	loFldSizeMap = 0;
	HiFldSizeMap = 18;

	FIELD_DATASIZE_BY_FIELDCLASS_MAP:
		array[loFldSizeMap..HiFldSizeMap] of TKFieldDataSizeByClass =
	(
		( FieldClassName: '';	      				FieldDataSize: INVALID_FLD_SIZE ),
		( FieldClassName: 'TField';					FieldDataSize: INVALID_FLD_SIZE ),
		( FieldClassName: 'TStringField';		FieldDataSize: STRING_FLD_SIZE ),
		( FieldClassName: 'TSmallintField';	FieldDataSize: SMALLINT_FLD_SIZE ),
		( FieldClassName: 'TIntegerField';	FieldDataSize: INTEGER_FLD_SIZE ),
		( FieldClassName: 'TWordField';			FieldDataSize: WORD_FLD_SIZE ),
		( FieldClassName: 'TBooleanField';	FieldDataSize: BOOLEAN_FLD_SIZE ),
		( FieldClassName: 'TFloatField';    FieldDataSize: FLOAT_FLD_SIZE ),
		( FieldClassName: 'TCurrencyField';	FieldDataSize: CURRENCY_FLD_SIZE ),
		( FieldClassName: 'TBCDField';			FieldDataSize: BCD_FLD_SIZE ),
		( FieldClassName: 'TDateField';			FieldDataSize: DATE_FLD_SIZE ),
		( FieldClassName: 'TTimeField';			FieldDataSize: TIME_FLD_SIZE ),
		( FieldClassName: 'TDateTimeField';	FieldDataSize: DATETIME_FLD_SIZE ),
		( FieldClassName: 'TBytesField';		FieldDataSize: BYTES_FLD_SIZE ),
		( FieldClassName: 'TVarBytesField';	FieldDataSize: VARBYTES_FLD_SIZE ),
		( FieldClassName: 'TAutoIncField';	FieldDataSize: AUTOINC_FLD_SIZE ),
		( FieldClassName: 'TBlobField';			FieldDataSize: BLOB_FLD_SIZE ),
		( FieldClassName: 'TMemoField';	    FieldDataSize: MEMO_FLD_SIZE ),
		( FieldClassName: 'TGraphicField';	FieldDataSize: GRAPHIC_FLD_SIZE )
	);

{
--------------------------------------------------------------------------------
------------------------ InterBase BlobType Constants --------------------------
--------------------------------------------------------------------------------
}

{ types less than zero are reserved for customer use }

	ISC_BLOB_UNTYPED  = 0;

{ internal subtypes }

	ISC_BLOB_TEXT     = 1;
	ISC_BLOB_BLR      = 2;
	ISC_BLOB_ACL      = 3;
	ISC_BLOB_RANGES   = 4;
	ISC_BLOB_SUMMARY  = 5;
	ISC_BLOB_FORMAT   = 6;
	ISC_BLOB_TRA      = 7;
	ISC_BLOB_EXTFILE  = 8;

{ the range 20-30 is reserved for DBASE and Paradox types }

	ISC_BLOB_FORMATTED_MEMO = 20;
	ISC_BLOB_PARADOX_ORE    = 21;
	ISC_BLOB_GRAPHIC        = 22;
	ISC_BLOB_DBASE_OLE      = 23;
	ISC_BLOB_TYPED_BINARY   = 24;

	IBASE_BLOB_SUBTYPES : array[TBlobType] of ShortInt =
	(
		ISC_BLOB_UNTYPED,
		ISC_BLOB_TEXT,
		ISC_BLOB_GRAPHIC,
		ISC_BLOB_FORMATTED_MEMO,
		ISC_BLOB_PARADOX_ORE,
		ISC_BLOB_DBASE_OLE,
		ISC_BLOB_TYPED_BINARY
	);

{
--------------------------------------------------------------------------------
--------------------------- SQL Script Constants -------------------------------
--------------------------------------------------------------------------------
}

{------------------------------ Generic Constants ------------------------------}

	SQL_SCRIPT_CURRENCY_PRECISION = 15;
	SQL_SCRIPT_CURRENCY_DIGITS = 2;
	SQL_SCRIPT_FLOAT_PRECISION = SQL_SCRIPT_CURRENCY_PRECISION;
	SQL_SCRIPT_FLOAT_DIGITS = SQL_SCRIPT_CURRENCY_DIGITS;
	SQL_SCRIPT_BCD_PRECISION = SQL_SCRIPT_CURRENCY_PRECISION;
	SQL_SCRIPT_BCD_DIGITS = SQL_SCRIPT_CURRENCY_DIGITS;


{ Generic SQL BUILD Constants }

	SQL_SCRIPT_DATETIME_FORMAT = 'dd/mm/yyyy hh:nn:ss';
	SQL_SCRIPT_MAX_FLDTYP_DEF  = 20;

{ DB LOCAL SQL BUILD Constants }

	SQL_LOCAL_DEFAULT_DIGITS = 2;

{ DBF/DB tables must have precision field at least 2 less DIGITS field.}

	SQL_LOCAL_DEFAULT_PRECISION = SQL_LOCAL_DEFAULT_DIGITS + 2;

{------------------------------ Pattern Constants ------------------------------}

	SQLBUILD_HEADER_PATTERN  = 0;
	SQLBUILD_FOOTER_PATTERN  = 1;
	SQLBUILD_TABLES_PATTERN  = 2;
	SQLBUILD_INDEXES_PATTERN = 3;
	SQLBUILD_FIELDS_PATTERN  = 4;
	SQLBUILD_PRIMKY_PATTERN  = 5;
	SQLBUILD_CHECKS_PATTERN  = 6;

{------------------------------ Command Constants ------------------------------}

{ General Commands }

	SQLBUILD_RUN_COMMAND 				 = 'RUN';
	SQLBUILD_RUN_TOKEN_COMMAND	 = '/*' + SQLBUILD_RUN_COMMAND + '*/'#13#10#13#10;
	SQLBUILD_START_COMMAND 			 = 'START';
	SQLBUILD_START_TOKEN_COMMAND = '/*' + SQLBUILD_START_COMMAND + '*/'#13#10;

{ MSSQL Specific Commands }

	SQLBUILD_MSSQL_QUOTED_COMMAND 		  = 'on';
	SQLBUILD_MSSQL_QUOTED_TOKEN_COMMAND = 'set quoted_identifier '+ SQLBUILD_MSSQL_QUOTED_COMMAND;
	SQLBUILD_MSSQL_RUN_COMMAND 					= 'GO';
	SQLBUILD_MSSQL_RUN_TOKEN_COMMAND 		= SQLBUILD_MSSQL_RUN_COMMAND;

{---------------------------- Environment Variables ----------------------------}

{ General Environment Variables }

	SQLBUILD_ENVVAR_HEADER  = '$HEADER$';
	SQLBUILD_ENVVAR_TABLES  = '$TABLES$';
	SQLBUILD_ENVVAR_INDEXES = '$INDEXES$';
	SQLBUILD_ENVVAR_FOOTER  = '$FOOTER$';
	SQLBUILD_ENVVAR_RUN 		= '$GO$';
	SQLBUILD_ENVVAR_START 	= '$START$';
	SQLBUILD_ENVVAR_DBNAME 	= '$DBNAME %0:s $';
	SQLBUILD_ENVVAR_SOS  		= '$SOS$'; { Start of Any Script type }
	SQLBUILD_ENVVAR_EOS  		= '$EOS$'; { End of Any Script type }
	SQLBUILD_ENVVAR_SOTS 		= '$SOTS$'; { Start of Any Table Script type }
	SQLBUILD_ENVVAR_EOTS 		= '$EOTS$'; { End of Any Table Script type }

{ Who builds the fields list should put correct CRLF pairs }
	SQLBUILD_ENVVAR_FIELDS 	= '$FIELDS$';

{ Who builds the primary key entry should put correct CRLF pairs and tab }
	SQLBUILD_ENVVAR_PRIMKEY = '$PRIMKEY$';

{ Who builds the checks list should put correct CRLF pairs and tab }
	SQLBUILD_ENVVAR_CHECKS 	= '$CHECKS$';

{ MSSQL Specific Environment Variables }

	SQLBUILD_MSSQL_ENVVAR_QUOTED = 'set quoted_identifier $QUOTED$';
	SQLBUILD_MSSQL_ENVVAR_RUN 	 = '$MSSQLRUN$';

{------------------------ Environment Variables Tokens -------------------------}

	SQLBUILD_ENVVAR_START_TOKEN  = '/*$START$*/'#13#10;
	SQLBUILD_ENVVAR_RUN_TOKEN 	 = '/*$GO$*/'#13#10#13#10;
	SQLBUILD_ENVVAR_DBNAME_TOKEN = '/*$DBNAME ';
	SQLBUILD_ENVVAR_SOS_TOKEN 	 = '/*' + SQLBUILD_ENVVAR_SOS + '*/';
	SQLBUILD_ENVVAR_EOS_TOKEN    = '/*' + SQLBUILD_ENVVAR_EOS + '*/';

{----------------------------- Definition Checking -----------------------------}

	SQL_DDL_ALTER  = 'ALTER';
	SQL_DDL_CREATE = 'CREATE';
	SQL_DDL_DROP   = 'DROP';

  SQL_DDL_DB		 = 'DB';
	SQL_DDL_VIEW   = 'VIEW';
	SQL_DDL_TABLE  = 'TABLE';
	SQL_DDL_INDEX  = 'INDEX';
	SQL_DDL_PK		 = 'PRIMARY KEY';
	SQL_DDL_FK     = 'FOREIGN KEY';
	SQL_DDL_CONSTRAINT = 'CONSTRAINT';

	SQL_DML_DELETE = 'DELETE';
	SQL_DML_INSERT = 'INSERT';
	SQL_DML_UPDATE = 'UPDATE';
	SQL_DML_UNION  = 'UNION';
	SQL_DML_SELECT = 'SELECT';

	DML_SQL_LOW_CHECK = 0;
	DML_SQL_HIGH_CHECK = 5;

	DML_SQL_CHECK: array[DML_SQL_LOW_CHECK..DML_SQL_HIGH_CHECK] of string[6] =
	(
		SQL_DDL_ALTER,
		SQL_DDL_CREATE,
		SQL_DDL_DROP,
		SQL_DML_DELETE,
		SQL_DML_INSERT,
		SQL_DML_UPDATE
	);

{
--------------------------------------------------------------------------------
---------------------------- SQL Script Patterns -------------------------------
--------------------------------------------------------------------------------
}

type

	TKStrFldTypDef = string[SQL_SCRIPT_MAX_FLDTYP_DEF];
	TKSQLFieldTypeDef = array[TFieldType] of TKStrFldTypDef;

{------------------------- ANSI SQL Script Patterns ----------------------------}

const

	SQL_ANSI_HEADER_PATTERN =
		'/*' + SQLBUILD_ENVVAR_SOS + '*/'#13#10#13#10 + {Start of Script snipet}
		'/*' + SQLBUILD_ENVVAR_DBNAME + '*/'#13#10#13#10 +
		'/* %1:s */'#13#10#13#10 + {Comments for database i}
		SQLBUILD_ENVVAR_TABLES + {Tables list}
		'/*' + SQLBUILD_ENVVAR_EOS + '*/'#13#10 +
		SQLBUILD_ENVVAR_FOOTER;{Script Footer}{End of Script snipet}

	SQL_ANSI_TABLE_PATTERN =
		'/*' + SQLBUILD_ENVVAR_START + '*/'#13#10 +
		'/* %0:s */'#13#10 + {Comments for table i}
		'CREATE TABLE "%1:s" ( '#13#10 + {Table Name}
		SQLBUILD_ENVVAR_FIELDS +
		SQLBUILD_ENVVAR_PRIMKEY +
		SQLBUILD_ENVVAR_CHECKS +
		#13#10#9')'#13#10 +
		'/*' + SQLBUILD_ENVVAR_RUN + '*/'#13#10#13#10 +
		SQLBUILD_ENVVAR_INDEXES;{Index List for Table i}

	SQL_ANSI_INDEX_PATTERN =
		'/*' + SQLBUILD_ENVVAR_START + '*/'#13#10+
		'/* %0:s */'#13#10 + {Comments for index j of table i}
		'CREATE %1:s INDEX %2:s ON "%3:s" ( %4:s )'#13#10 +
		'/*' + SQLBUILD_ENVVAR_RUN + '*/'#13#10#13#10;

	SQL_ANSI_FIELD_PATTERN =
		{ FldName FldType Null|Not Null [Default ( Default Expression )], }
		#9'"%0:s"."%1:s" %2:s %3:s %4:s,'#13#10;
  
	SQL_ANSI_PK_PATTERN =
		{ PRIMARY KEY PkName ( Fld1, Fld2, ... , FldN ), }
		#9'CONSTRAINT "%0:s" PRIMARY KEY ( %1:s ),'#13#10;

	SQL_ANSI_CK_PATTERN =
		{ CHECK CkName ( ConstraintExpression ), }
		#9'CHECK "%1:s" ( %2:s ),'#13#10;

	SQL_ANSI_FIELD_DEFAULT_PATTERN =
		{ DEFAULT( 'xxxx' ) or DEFAULT( xxx ) }
		'DEFAULT ( %0:s%1:s%0:s )';

{
 Non Exact floats » NUMERIC(p,s).
 Exact floats/BCD » DECIMAL(p,s).
}

	SQL_ANSI_FIELDTYPE_PATTERN: TKSQLFieldTypeDef =
	(
		'ERROR!',         { ftUnknown }
		'CHAR(%d)',       { ftString }
		'SMALLINT',       { ftSmallint }
		'INTEGER',        { ftInteger }
		'INTEGER',        { ftWord }
		'BIT',            { ftBoolean }
		'NUMERIC(%d, %d)',{ ftFloat }
		'DECIMAL(%d, %d)',{ ftCurrency }
		'DECIMAL(%d, %d)',{ ftBCD }
		'DATE',           { ftDate }
		'TIME',           { ftTime }
		'TIMESTAMP',      { ftDateTime }
		'BYTES(%d)',      { ftBytes }
		'BYTES(%d)',	  	{ ftVarBytes }
		'INTEGER',        { ftAutoInc }
		'VARBYTES(%d)',	  { ftBlob }
		'VARBYTES(%d)',  	{ ftMemo }
		'VARBYTES(%d)',  	{ ftGraphic }
		'VARBYTES(%d)',   { ftFmtMemo }
		'VARBYTES(%d)',	  { ftParadoxOle }
		'VARBYTES(%d)',	  { ftDBaseOle }
		'VARBYTES(%d)',  	{ ftTypedBinary }
		'ERROR!'	  	    { ftCursor }

		{$IFDEF DELPHI4},  // not yet implemented for d4...
		'ERROR!',         { ftFixedChar }
		'ERROR!', { TWideStringField } { ftWideString }
		'ERROR!',         { ftLargeInt }
		'ERROR!',         { ftADT }
		'ERROR!',  				{ ftArray }
		'ERROR!',					{ ftReference }
		'ERROR!'					{ ftDataSet }
		{$ENDIF}

	);

{------------------------ LOCAL SQL Script Patterns ----------------------------}

	SQL_LOCAL_FIELD_PATTERN =
		{ FldName FldType, }
		#9'"%0:s"."%1:s" %2:s,'#13#10;

	SQL_LOCAL_PK_PATTERN =
		{ PRIMARY KEY PkName ( Fld1, Fld2, ... , FldN ), }
		#9'PRIMARY KEY ( %s ),'#13#10;

	SQL_LOCAL_FIELDTYPE_PATTERN: TKSQLFieldTypeDef =
	(
		'ERROR!',         { ftUnknown }
		'CHAR(%d)',       { ftString }
		'SMALLINT',       { ftSmallint }
		'INTEGER',        { ftInteger }
		'INTEGER',        { ftWord }
		'BOOLEAN',        { ftBoolean }
		'NUMERIC(%d, %d)',{ ftFloat }
		'MONEY',          { ftCurrency }
		'DECIMAL(%d, %d)',{ ftBCD }
		'DATE',           { ftDate }
		'TIME',           { ftTime }
		'DATE',           { ftDateTime }
		'BYTES(%d)',      { ftBytes }
		'BYTES(%d)',			{ ftVarBytes }
		'AUTOINC',        { ftAutoInc }
		'BLOB(%d, 2)',	  { ftBlob }          { Paradox Blob SubTypes }
		'BLOB(%d, 1)',	  { ftMemo }
		'BLOB(%d, 5)',	  { ftGraphic }
		'BLOB(%d, 3)',    { ftFmtMemo }
		'BLOB(%d, 4)',	  { ftParadoxOle }
		'BLOB(%d, 4)',	  { ftDBaseOle }
		'BLOB(%d, 2)',	  { ftTypedBinary }
		'ERROR!'	  	  	{ ftCursor }

		{$IFDEF DELPHI4},  // not yet implemented for d4...
		'ERROR!',         { ftFixedChar }
		'ERROR!', { TWideStringField } { ftWideString }
		'ERROR!',         { ftLargeInt }
		'ERROR!',         { ftADT }
		'ERROR!',  				{ ftArray }
		'ERROR!',					{ ftReference }
		'ERROR!'					{ ftDataSet }
		{$ENDIF}
	);

{-------------------------- MSSQL SQL Script Patterns --------------------------}

	SQL_MSSQL_HEADER_PATTERN =
		'/*' + SQLBUILD_ENVVAR_SOS + '*/'#13#10#13#10 + {Start of Script snipet}
		'/*' + SQLBUILD_ENVVAR_DBNAME + '*/'#13#10#13#10 +
		'/* %1:s */'#13#10#13#10 + {Comments for database i}
		SQLBUILD_MSSQL_ENVVAR_QUOTED + #13#10#13#10 +
		SQLBUILD_ENVVAR_TABLES + {Tables list}
		'/*' + SQLBUILD_ENVVAR_EOS + '*/'#13#10 +
		SQLBUILD_ENVVAR_FOOTER;{Script Footer}{End of Script snipet}

	SQL_MSSQL_TABLE_PATTERN =
		'/*' + SQLBUILD_ENVVAR_START + '*/'#13#10 +
		'/* %0:s */'#13#10 + {Comments for table i}
		'CREATE TABLE "%1:s" ( '#13#10 + {Table Name}
		SQLBUILD_ENVVAR_FIELDS +
		SQLBUILD_ENVVAR_PRIMKEY +
		SQLBUILD_ENVVAR_CHECKS +
		#13#10#9')'#13#10 +
		'/*' + SQLBUILD_ENVVAR_RUN + '*/' + #13#10 +
		SQLBUILD_MSSQL_ENVVAR_RUN + #13#10#13#10 +
		SQLBUILD_ENVVAR_INDEXES;{Index List for Table i}

	SQL_MSSQL_INDEX_PATTERN =
		'/*' + SQLBUILD_ENVVAR_START + '*/'#13#10 +
		'/* %0:s */'#13#10 + {Comments for index j of table i}
		'CREATE %1:s INDEX %2:s ON "%3:s" ( %4:s )'#13#10 +  {Fields list for index}
		'/*' + SQLBUILD_ENVVAR_RUN + '*/'#13#10 +
		SQLBUILD_MSSQL_ENVVAR_RUN + #13#10#13#10;

	SQL_MSSQL_FIELDTYPE_PATTERN: TKSQLFieldTypeDef =
	(
		'ERROR!',         { ftUnknown }
		'CHAR(%d)',       { ftString }
		'SMALLINT',       { ftSmallint }
		'INT',            { ftInteger }
		'INT',            { ftWord }
		'BIT',            { ftBoolean }
		'NUMERIC(%d, %d)',{ ftFloat }
		'MONEY',          { ftCurrency }
		'DECIMAL(%d, %d)',{ ftBCD }
		'DATETIME',       { ftDate }
		'DATETIME',       { ftTime }
		'DATETIME',       { ftDateTime }
		'BINARY(%d)',     { ftBytes }
		'VARBINARY(%d)',	{ ftVarBytes }
		'INT IDENTITY',   { ftAutoInc }
		'VARBINARY(%d)',	{ ftBlob }
		'TEXT',						{ ftMemo }
		'IMAGE',					{ ftGraphic }
		'VARBINARY(%d)',  { ftFmtMemo }
		'VARBINARY(%d)',	{ ftParadoxOle }
		'VARBINARY(%d)',	{ ftDBaseOle }
		'VARBINARY(%d)',	{ ftTypedBinary }
		'ERROR!'	  	  	{ ftCursor }

		{$IFDEF DELPHI4},  // not yet implemented for d4...
		'ERROR!',         { ftFixedChar }
		'ERROR!', { TWideStringField } { ftWideString }
		'ERROR!',         { ftLargeInt }
		'ERROR!',         { ftADT }
		'ERROR!',  				{ ftArray }
		'ERROR!',					{ ftReference }
		'ERROR!'					{ ftDataSet }
		{$ENDIF}

	);

{------------------------ INTERBASE SQL Script Patterns ------------------------}

	SQL_IBASE_FIELDTYPE_PATTERN: TKSQLFieldTypeDef =
	(
		'ERROR!',         { ftUnknown }
		'%s',             { ftString }
		'SMALLINT',       { ftSmallint }
		'INTEGER',        { ftInteger }
		'INTEGER',        { ftWord }
		'BIT',            { ftBoolean }
		'%s',             { ftFloat }
		'DECIMAL(%d, %d)',{ ftCurrency }
		'DECIMAL(%d, %d)',{ ftBCD }
		'DATE',           { ftDate }
		'DATE',           { ftTime }        { Also includes time information }
		'DATE',           { ftDateTime }
		'BYTES(%d)',      { ftBytes }
		'BYTES(%d)',			{ ftVarBytes }
		'INTEGER',        { ftAutoInc }
		'BLOB(%d, %d)',	  { ftBlob }        { InterBase Blob SubTypes }
		'BLOB(%d, %d)',	  { ftMemo }
		'BLOB(%d, %d)',	  { ftGraphic }
		'BLOB(%d, %d)',   { ftFmtMemo }
		'BLOB(%d, %d)',	  { ftParadoxOle }
		'BLOB(%d, %d)',	  { ftDBaseOle }
		'BLOB(%d, %d)',	  { ftTypedBinary }
		'ERROR!'	  	  	{ ftCursor }

		{$IFDEF DELPHI4},  // not yet implemented for d4...
		'ERROR!',         { ftFixedChar }
		'ERROR!', { TWideStringField } { ftWideString }
		'ERROR!',         { ftLargeInt }
		'ERROR!',         { ftADT }
		'ERROR!',  				{ ftArray }
		'ERROR!',					{ ftReference }
		'ERROR!'					{ ftDataSet }
		{$ENDIF}

	);

{------------------------- ORACLE SQL Script Patterns -------------------------}


{ DBLogEngine }

	sDBLogEngineCreateTable = 'CREATE TABLE "%s" ('#13#10 +'%s )';
	sDBLogEngineDropTable = 'DROP TABLE "%s"';
	sDBLogEngineInsertIntoTbl = 'INSERT INTO "%s" ( %s )'#13#10'VALUES ( %s )';
	{ F_XXX - Fixed Fields; V_XXX - Virtual Fields; C_XXX - Custom Fields }
	sDBLogEngineCrtTblFieldsSQL = '"%s" CHAR(%d), '#13#10;
	sDBLogEngineCrtTblTextFieldsSQL = '"%s" TEXT, '#13#10;
	sDBLogEngineCrtTblFieldsLocal = '"%s"."%s" CHAR(%d), '#13#10;
	sDBLogEngineCrtTblBlobFieldsLocal = '"%s"."%s" BLOB(50,1), '#13#10;

	DBLOG_ITEMS_ATTRCOUNT = 3;
	DBLOG_GROUP_NULL = COLLECTION_ITEM_GROUP_NULL;
	DBLOG_GROUP_ALL  = COLLECTION_ITEM_GROUP_ALL;

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

function GetDBRegistryInfo: LongInt;

{##NI##}

implementation

uses
	SysUtils, uksyTypes;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type
	TSignature	 = TUserName;
	TKey				 = TUserName;

	TKRegistryInfo = record
		Signature: TSignature;
		Key: TKey;
		UserName: TUserName;
		Company: TCompanyName;
	end;

const

	KnowHowRegistryInfo: TKRegistryInfo =
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
		UserName:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32;
		Company:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32;
	);

{---------------------------- Public Implementation ----------------------------}
	
function GetDBRegistryInfo: LongInt;
begin
	Result := LongInt( @KnowHowRegistryInfo ) - SizeOf( TKRegistryInfo );
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

const
	ID_FLOAT    = 7;
	ID_CURRENCY = 8;
	ID_BCD			= 9;

procedure Init;
begin
//	Include( FIELD_VALIDCHARS_BY_FIELDCLASS_MAP[ID_FLOAT].FieldValidChars, DecimalSeparator );
//	Include( FIELD_VALIDCHARS_BY_FIELDCLASS_MAP[ID_CURRENCY].FieldValidChars, DecimalSeparator );
//	Include( FIELD_VALIDCHARS_BY_FIELDCLASS_MAP[ID_BCD].FieldValidChars, DecimalSeparator );
end;

procedure Done;
begin
end;

initialization
	Init;

finalization
	Done;

end.
