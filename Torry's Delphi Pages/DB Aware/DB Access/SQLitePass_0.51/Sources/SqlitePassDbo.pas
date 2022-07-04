{ The unit SqlitePassDbo defines all the interface definitions for
  the SqlitePass Database Objects Project. The implementations are located
  in SqlitePassEngine.inc, SqlitePassDatabase.inc, SqlitePassDatabaseParts.inc,
  SqlitePassDataset.inc, SqlitePassSqlStmts.inc and SqlitePassRecordset.inc files.


  ---------------------------------------------------------------------------

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  ---------------------------------------------------------------------------

    @Author(Luc DAVID 2006-2010 luckylazarus@free.fr)
    @Author(http://source.online.free.fr )

    Major changes are indicated in the \Documentation\Changes.pdf file
    @created(2006)
    @lastmod(09.03.2010)

  --------------------------------------------------------------------------- }


unit SqlitePassDbo;
{ Turns on/off debug output to Debuger Event Log}
{.$DEFINE DEBUG_SQLITEPASS}
{.$DEFINE DEBUG_SQLStmt}

{$i SqlitePassDbo.inc}


Interface

uses
 Classes, SysUtils, Db, Forms, Dialogs, Controls, Extctrls,
 Math, {Keep with FPC despite the compiler tips}
 SqlitePassConst,
 SqlitePassErrorLang,
 SqlitePassApi_v3,
 SqlitePassUtils,

{$IFDEF HasFmtBcd}
  FmtBcd,
{$ENDIF}

{$IFDEF DEBUG_SQLITEPASS}
  SqlitePassDebug,
{$ENDIF}

{$IFDEF FPC}
 Variants, LCLIntf, LCLProc, DbConst, FileUtil,
{$ELSE}
  {$IFDEF Delphi6}
   Variants,
  {$ENDIF}
  {$IFDEF Delphi2009}
   {pointermath on}
   Variants, AnsiStrings,
  {$ENDIF}
  DbConsts,
  DBCommon,
{$ENDIF}

{$IFDEF UseTNTComponents}
 TntDB,
{$ENDIF}

{$IFDEF WIN32}
 Windows;
{$ELSE}
 Dynlibs;
{$ENDIF}


Type
 { forward declarations }
 TSqlitePassEngine = class;
 TSqlitePassDatabase = Class;
 TSqlitePassDataset = class;
 TSqlitePassDatasets = class;
 TSqlitePassTableDef = class;
 TSqlitePassTableDefs = class;
 TSqlitePassDatabaseIndexDefs = class;
 TSqlitePassQueryDefs = class;
 TSqlitePassViews = class;
 TSqlitePassTriggers = class;
 TSqlitePassTableFieldDef = class;
 TSqlitePassTableFieldDefs = class;
 TSqlitePassIndexColumns = class;
 TSqlitePassIndex = class;
 TSqlitePassDatasetIndex = class;
 TSqlitePassTranslator = class;
 TSqlitePassDatabasesAttached = class;
 TSqlitePassSelectStmtFieldDefs = class;
 TSqlitePassFieldFilterExp = class;
 TSqlitePassFieldFilter = class;
 TSqlitePassFieldFilters = class;
 TSqlitePassSelectStmtTableDef = class;
 TSqlitePassSelectStmtTableDefs = class;
 TSqlitePassRecordset = class;
 TSqlitePassSQLStmtDefs = class;
 TSqlitePassSelectStmt = class;
 TSqlitePassSQLStmt = class;
 TSqlitePassSQLTokenizer = class;
 TSqlitePassDatabaseDataTypeOptions = class;
 TSqlitePassInMemoryIndex = class;
 TSqlitePassInMemoryIndexes = class;

 TMidRecordBuffer = record
                    FieldIsNull: Boolean;
                    FieldBuffer: PRecBuffer;
                    end;
 TMidRecordBuffers = Array of TMidRecordBuffer;

 TSqlitePassVerboseLevel = vlSilent..vlLogAndShow;
 TSqlitePassAvailableOperations = set of (aoRead, aoInsert, aoUpdate, aoDelete);
 TSqlitePassSQLChanges = set of (scSelect, scWhere, scGroupBy, scHaving, scOrderBy, scLimit, scRefresh);
 TSqlitePassDatabaseObjectTypes =(dboAll, dboTableDefs, dboQueryDefs, dboIndexDefs, dboViews, dboTriggers);
 TSqlitePassDatabaseType = (dbtSqlitePass, dbtKexi, dbtSqliteAdmin, dbtSqlite4Fpc, dbtSqliteExpert, dbtUnknown);
 TSqlitePassDatabaseState = set of (dbsInternalOpen, dbsRefreshingDefinitions);
 TSqlitePassPostType = (ptInsert, ptUpdate, ptDelete);
 TSqlitePassQuoteStyle = (qsDoubleQuote, qsBracket, qsGraveQuote, qsNone);

 { Filtering }
 TSqlitePassFilterChanges = set of (fcSQLFilter, fcDirectFilters, fcLowerLimit, fcUpperLimit);
 TSqlitePassFilterComparisonOperators = (CmpUnknown, CmpEqual, CmpNotEqual, CmpGreater, CmpGreateOrEqual,
                                         CmpLesserOrEqual, CmpLesser, CmpNull, CmpNotNull);
 TSqlitePassFilterLogicalOperators = (opNone, opAnd, opOr);
 TSqlitePassFilterMode = (fmDirect, fmSQL, fmSQLDirect);

 { Sorting }
 TSqlitePassSortMode = (smDirect, smSQL);
 TSqlitePassWriteMode = (wmDirect, wmPostponed);

 { }
 TSqlitePassExecCallBack = function(acolumns: Integer; aColumnValues, aColumnNames:ppAnsiChar):integer of object;
 PSqlitePassExecCallBack = ^TSqlitePassExecCallBack;
 TSqlitePassGetPragmaCallback = function(UserData: Pointer; ColumnCount: Integer; ColumnValues, ColumnNames: PPointer): Integer; cdecl;

 { Record State }
 TSqlitePassRecordStateType = 0..7;
 TSqlitePassRecordStateValue = 0..1;
 TSqlitePassRecordState =  Byte;
 PSqlitePassRecordState = ^TSqlitePassRecordState;

 { Note : Check SqlitePassConst.SqlitePassSQLTokenTypeAsString if TSqlitePassSQLTokenType is modified }
 TSqlitePassSQLTokenType= (ttKeyword, ttIdentifier, ttDatabaseIdentifier, ttTableIdentifier, ttFieldIdentifier, ttFunctionIdentifier,
                           ttStar, ttNumber, ttDateTime, ttSingleQuote, ttDoubleQuote, ttGraveQuote, ttHash, ttPercent, ttBraceOpen, ttBraceClose, ttSquareOpen, ttSquareClose,
                           ttRoundOpen, ttRoundClose, ttWhitespace, ttComment, ttComma, ttColon, ttSemiColon, ttEqual, ttNotEqual,
                           ttGreater, ttGreaterOrEqual, ttLesser, ttLesserOrEqual, ttNull, ttNotNull, ttPlus, ttMinus, ttAnd, ttOr, ttDot, ttSlash,
                           ttSeparator, ttEOF, ttUnknown);

 { TVersionInfo }
 TSqlitePassDatabaseVersionInfo = Class(TPersistent)
     Private
         FComponent   : String;
         FDatabase    : TSqlitePassDatabase;
         FPackage     : String;
         FSchema      : Integer;
         FUserTag     : Integer;
         function GetFDbSchema: Integer;
         function GetFSqliteLibrary: String;
         function GetFSqliteLibraryNumber: Integer;
         function GetFSqliteLibrarySourceId: String;
         function GetFUserTag: Integer;
         procedure SetFPackage(const Value: String);
         procedure SetFDbSchema(const Value: Integer);
         procedure SetFSqliteLibrary(const Value: String);
         procedure SetFSqliteLibraryNumber(const Value: Integer);
         procedure SetFSqliteLibrarySourceId(const Value: String);
         procedure SetFUserTag(const Value: Integer);
         procedure SetFComponent(const Value: String);
     Public
         constructor Create(Database: TSqlitePassDatabase);
         destructor Destroy; override;
     Published
         { This property returns the SqlitePass Database 
         component version }
         Property Component     : String  Read FComponent       Write SetFComponent;
         
         { This property returns the internal database schema
           version as described in the Sqlite help }
         Property Schema        : Integer Read GetFDbSchema     Write SetFDbSchema;
         
         { This property returns the SqlitePass database version }
         Property Package       : String  Read FPackage         Write SetFPackage;

         { This property returns the current sqlite library version used by the components
          (sqlitepass3.dll, sqlitepass3.so or an alternative library. The function is provided
          for DLL only (windows)
          @seealso(TSqlitePassDatabase.SQLiteLibrary)}
         Property SqliteLibrary : String  Read GetFSqliteLibrary Write SetFSqliteLibrary;

         { This property returns the current sqlite library version number used by the components
          (sqlitepass3.dll, sqlitepass3.so or an alternative library.
          @seealso(TSqlitePassDatabase.SQLiteLibrary)}
         Property SqliteLibraryNumber : Integer  Read GetFSqliteLibraryNumber Write SetFSqliteLibraryNumber;

         { This property returns the current sqlite library Source Id number used by the components
          (sqlitepass3.dll, sqlitepass3.so or an alternative library.
          @seealso(TSqlitePassDatabase.SQLiteLibrary)}
         Property SqliteSourceId : String  Read GetFSqliteLibrarySourceId Write SetFSqliteLibrarySourceId;

         { This property returns the internal usertag version
           as described in the Sqlite help }
         Property UserTag        : Integer Read GetFUserTag      Write SetFUserTag;
     end;

 TSqlitePassAutoVacuumType       = (avNone, avFull, avIncremental);
 TSqlitePassEncoding             = (UTF8, UTF16, UTF16le, UTF16be);
 TSqlitePassJournalMode          = (jmDelete, jmTruncate, jmPersist, jmMemory, jmOff);
 TSqlitePassLockingMode          = (lmNormal, lmExclusive);
 TSqlitePassPageSize             = 512..32768;
 TSqlitePassRecordsCacheCapacity = 1..MaxInt;
 TSqlitePassSynchronous          = (syncOff, syncNormal, syncFull);
 TSqlitePassSystemEncoding       = (sysANSI, sysUTF8, sysUTF16);
 TSqlitePassTempStore            = (tsDefault, tsFile, tsMemory);
 TSqlitePassUnicodeEncoding      = (ueAuto, ueUTF8, ueUTF16, ueRawText);
 TSqlitePassApplyMode            = (amOverwriteDatabaseFileSettings,
                                    amAutoVacuum,
                                    amCacheSize,
                                    amCaseSensitiveLike,
                                    amCountChanges,
                                    amDefaultCacheSize,
                                    amFullColumnNames,
                                    amForeignKeys,
                                    amJournalMode,
                                    amJournalSizeLimit,
                                    amLockingMode,
                                    amMaxPageCount,
                                    amPageSize,
                                    amRecursiveTriggers,
                                    amSecureDelete,
                                    amSynchronous,
                                    amTemporaryStorage,
                                    amTemporaryStorageDir);
 TSqlitePassApplyModes           = set of TSqlitePassApplyMode;

 { TSqlitePassDatabaseOptions }
 TSqlitePassDatabaseOptions = Class(TPersistent)
    Private
         FDatabase: TSqlitePassDatabase;
         { Private fields in order to set default db setting on TSqlitePassDatabaseOptions.create }
         FApplyMode: TSqlitePassApplyModes;
         FAutoVacuum: TSqlitePassAutoVacuumType;
         FCacheSize: Integer;
         FCaseSensitiveLike: Boolean; { Needed to set to False as default since CaseSensitive Status cannot be retreive from the Db }
         FCountChanges: Boolean;
         FDefaultCacheSize: Integer;
         FEmptyResultCallBack: Boolean;
         FEncoding: TSqlitePassEncoding;
         FFullColumnNames: Boolean;
         FForeignKeys: Boolean;
         FJournalMode: TSqlitePassJournalMode;
         FJournalSizeLimit: Integer;
         FLockingMode: TSqlitePassLockingMode;
         FMaxPageCount: Integer;
         FPageSize: TSqlitePassPageSize;
         FRecursiveTriggers: Boolean;
         FSecureDelete: Boolean;
         FSynchronous: TSqlitePassSynchronous;
         FTemporaryStorage: TSqlitePassTempStore;
         FTemporaryStorageDir: String;
         { other private fields }
         FLogErrors: Boolean;
         FQuoteStyle: TSqlitePassQuoteStyle;
         FQuoteOpen: Char;
         FQuoteClose: Char;
         function GetFAutoVacuum: TSqlitePassAutoVacuumType;
         function GetFCacheSize: Integer;
         function GetFCountChanges: Boolean;
         function GetFDefaultCacheSize: Integer;
         function GetFEncoding: TSqlitePassEncoding;
         function GetFFullColumnNames: Boolean;
         function GetFForeignKeys: Boolean;
         function GetFJournalMode: TSqlitePassJournalMode;
         function GetFJournalSizeLimit: Integer;
         function GetFLockingMode: TSqlitePassLockingMode;
         function GetFMaxPageCount: Integer;
         function GetFPageSize: TSqlitePassPageSize;
         function GetFRecursiveTriggers: Boolean;
         function GetFSecureDelete: Boolean;
         function GetFSynchronous: TSqlitePassSynchronous;
         function GetFTemporaryStorage: TSqlitePassTempStore;
         function GetFTemporaryStorageDir: String;
         procedure SetFAutoVacuum(const Value: TSqlitePassAutoVacuumType);
         procedure SetFCacheSize(const Value: Integer);
         procedure SetFCaseSensitiveLike(const Value: Boolean);
         procedure SetFCountChanges(const Value: Boolean);
         procedure SetFDefaultCacheSize(const Value: Integer);
         procedure SetFEncoding(const Value: TSqlitePassEncoding);
         procedure SetFJournalMode(const Value: TSqlitePassJournalMode);
         procedure SetFJournalSizeLimit(const Value: Integer);
         procedure SetFForeignKeys(const Value: Boolean);
         procedure SetFFullColumnNames(const Value: Boolean);
         procedure SetFLockingMode(Const Value: TSqlitePassLockingMode);
         procedure SetFMaxPageCount(Const Value: Integer);
         procedure SetFPageSize(Const Value: TSqlitePassPageSize);
         procedure SetFQuoteStyle(const Value: TSqlitePassQuoteStyle);
         procedure SetFRecursiveTriggers(const Value: Boolean);
         procedure SetFSecureDelete(const Value: Boolean);
         procedure SetFSynchronous(const Value: TSqlitePassSynchronous);
         procedure SetFTemporaryStorage(const Value: TSqlitePassTempStore);
         procedure SetFTemporaryStorageDir(const Value: String);
         function CheckPageSize(Value: TSqlitePassPageSize): TSqlitePassPageSize;
    Public
         constructor Create(Database: TSqlitePassDatabase);
         destructor Destroy; override;
         Function GetEncodingAsString(Encoding: TSqlitePassEncoding): String;

         { Writes the options settings to the database file }
         Procedure Apply;
    Published
         { This property sets how the options are applied to the database file.
         @br
         @bold(When database is already connected), any options change is directly applied to the database file.
         @br
         @bold(When database is not connected), the changes are only applied when you open the database (Connected = True),
         depending on the ApplyMode setting :
         if amOverwriteDatabaseFileSettings is True,
         then the the ApplyMode selected options are applied to the database file with the values defined in object inspector
         (designtime) or in your *.dfm or *.lfm file (runtime).
         @br
         if amOverwriteDatabaseFileSettings is False, your customized settings will discarded and overwritten with
         the current database file settings.
          }
         Property ApplyMode: TSqlitePassApplyModes Read FApplyMode Write FApplyMode;

         { This property gets or sets the database autovacum
           pragma as described in the Sqlite help }
         Property AutoVacuum: TSqlitePassAutoVacuumType Read GetFAutoVacuum Write SetFAutoVacuum;
         
         { This property gets or sets the database CacheSize
           pragma as described in the Sqlite help }
         Property CacheSize: Integer Read GetFCacheSize Write SetFCacheSize;
         
         { This property gets or sets the database CaseSensitiveLike
           pragma as described in the Sqlite help }
         Property CaseSensitiveLike: Boolean Read FCaseSensitiveLike Write SetFCaseSensitiveLike;

         { This property gets or sets the database CountChanges
           pragma as described in the Sqlite help }
         Property CountChanges: Boolean Read GetFCountChanges Write SetFCountChanges;

         { This property gets or sets the database DefaultCacheSize
           pragma as described in the Sqlite help }
         Property DefaultCacheSize: Integer Read GetFDefaultCacheSize Write SetFDefaultCacheSize;

         { This property is readonly =. It gets the database Encoding
           as described in the Sqlite help - the database encoding can only be set if
           the database has not already been created }
         Property Encoding: TSqlitePassEncoding Read GetFEncoding Write SetFEncoding;

         { This property gets or sets the database FullColumnNames
           pragma as described in the Sqlite help }
         Property ForeignKeys: Boolean Read GetFForeignKeys Write SetFForeignKeys;

         { This property gets or sets the database FullColumnNames
           pragma as described in the Sqlite help }
         Property FullColumnNames: Boolean Read GetFFullColumnNames Write SetFFullColumnNames;

         { This property gets or sets the database Journal mode
           pragma as described in the Sqlite help }
         Property JournalMode: TSqlitePassJournalMode Read GetFJournalMode Write SetFJournalMode;

         { This property gets or sets the database Journal mode
           property as described in the Sqlite help }
         Property JournalSizeLimit: Integer Read GetFJournalSizeLimit Write SetFJournalSizeLimit;

         { This property gets or sets the database Journal mode
           pragma as described in the Sqlite help }
         Property LockingMode: TSqlitePassLockingMode Read GetFLockingMode Write SetFLockingMode;

         { When set to True, Errors are logged to the
           TSqlitePassDatabase.DatabaseError.ErrorList }
         Property LogErrors: Boolean Read FLogErrors Write FLogErrors;

         { This property gets or sets the Maximum database Pages Count
           pragma as described in the Sqlite help }
         Property MaxPageCount: Integer Read GetFMaxPageCount Write SetFMaxPageCount;

         { This property gets or sets the database PageSize
           pragma as described in the Sqlite help }
         Property PageSize: TSqlitePassPageSize Read GetFPageSize Write SetFPageSize;

         { This property gets or sets the database default
           quote style used to surround identifiers in sql statements.
           It can take one of the following values :
           @br
    	   @unorderedlist(
           @item qsDoubleQuote for "
           @item qsBracket for [ and ]
           @item sqGraveQuote for `)
           }
         Property QuoteStyle: TSqlitePassQuoteStyle Read FQuoteStyle Write SetFQuoteStyle;

         { This property gets or sets the database Recursive Triggers
           pragma as described in the Sqlite help }
         Property RecursiveTriggers: Boolean Read GetFRecursiveTriggers Write SetFRecursiveTriggers;

         { This property gets or sets the database Secure Delete
           pragma as described in the Sqlite help }
         Property SecureDelete: Boolean Read GetFSecureDelete Write SetFSecureDelete;

         { This property gets or sets the database Synchronous
           pragma as described in the Sqlite help }
         Property Synchronous: TSqlitePassSynchronous Read GetFSynchronous Write SetFSynchronous;

         { This property gets or sets the database TemporaryStorage
           pragma as described in the Sqlite help }
         Property TemporaryStorage: TSqlitePassTempStore Read GetFTemporaryStorage Write SetFTemporaryStorage;

         { This property gets or sets the database TemporaryStorage
           pragma as described in the Sqlite help }
         Property TemporaryStorageDir: String Read GetFTemporaryStorageDir Write SetFTemporaryStorageDir;

    end;

 { Datatypes detection and conversion }
 TSqlitePassBooleanStorage = (asInteger, asText);
 TSqlitePassDateTimeStorage = (dtsDateTime, dtsJulian, dtsText, dtsUnix, dtsMac);
 TSqlitePassDateStorage = TSqlitePassBooleanStorage;
 TSqlitePassTimeStorage = TSqlitePassDateStorage;
 TSqlitePassDataTypeDetectionMode = (dmTypeName, dmDbSpecific, dmCustom, dmForceStr);
 TSqlitePassDataTypeMappingMode = (mmExact, mmExactNoCase, mmPartial, mmPartialNoCase, mmAll);
 TSqlitePassDataTypeStorageLoadOptions = set of (loDefaultProperties, loCustomProperties, loTranslationRules, loCustomFieldDefs);
 TSqlitePassDataTypeStorageSaveOptions = set of (soCustomProperties, soTranslationRules, soCustomFieldDefs);

 { TSqlitePassFieldTypeTranslationRule.
   Database FieldTypes mapping to Pascal FieldTypes }
 TSqlitePassFieldTypeTranslationRule = Class
 Private
 FDatatypeName: String;
 FMappingMode: TSqlitePassDataTypeMappingMode;
 FFieldType: TFieldType;
 Public
 Property DatatypeName: String Read FDatatypeName Write FDatatypeName;
 Property MappingMode: TSqlitePassDataTypeMappingMode Read FMappingMode Write FMappingMode;
 Property FieldType: TFieldType Read FFieldType Write FFieldType;
 end;

 { TSqlitePassFieldTypesGenericTranslationRules }
 TSqlitePassFieldTypesGenericTranslationRules = Class(TSqlitePassObjectList)
  Private
    FDataTypeOptions: TSqlitePassDatabaseDataTypeOptions;
  Protected
    Function GetItem(Index: Integer): TSqlitePassFieldTypeTranslationRule;
    Procedure SetItem(Index: Integer; FieldTypeTranslationRule: TSqlitePassFieldTypeTranslationRule);
  Public
    Constructor Create(DataTypeOptions: TSqlitePassDatabaseDataTypeOptions);
    Destructor Destroy; override;
    Function Add(FieldTypeTranslationRule: TSqlitePassFieldTypeTranslationRule): Integer;
    Procedure ClearAndFreeItems;
    Property Items[Index: Integer]: TSqlitePassFieldTypeTranslationRule read GetItem write SetItem; default;
  end;

 { TSqlitePassFieldTypesDefaultTranslationRules }
 TSqlitePassFieldTypesDefaultTranslationRules = Class(TSqlitePassFieldTypesGenericTranslationRules)
 Private
   Procedure InitDefaultTranslationRules;
 end;

 { TSqlitePassFieldTypesTranslationRules }

 TSqlitePassFieldTypesTranslationRules = Class(TSqlitePassFieldTypesGenericTranslationRules)
 Public
   Procedure Assign(Source: TSqlitePassFieldTypesGenericTranslationRules);
   { Scans the FieldTypesDefaultTranslationRules to verify if the requested translation rule
     was customized }
   Procedure LoadDefaultTranslationRules;
   Function IsCustomized(Item: TSqlitePassFieldTypeTranslationRule): Boolean;
   { Try to get the field datatype from then Custom Translation Rules. If no match is found,
     then try with the default field datatypes associated with the current database type }
   Procedure SetFieldDefDataType(FieldDef: TSqlitePassTableFieldDef);
 end;


 { TSqlitePassCustomFieldDef
   is used by TSqlitePassDatabaseDataTypeOptions when you need to define
   your own fieldtype translations }
 TSqlitePassCustomFieldDef = Class
 Private
 FFieldName: String;
 FFieldType: TFieldType;
 FFieldSize: Integer;
 FFieldPrecision: Integer;
 FTableName: String;
 Public
 Property TableName: String Read FTableName Write FTableName;
 Property FieldName: String Read FFieldName Write FFieldName;
 Property FieldType: TFieldType Read FFieldType Write FFieldType;
 Property FieldSize: Integer Read FFieldSize Write FFieldSize;
 Property FieldPrecision: Integer Read FFieldPrecision Write FFieldPrecision;
 end;


 { TSqlitePassCustomFieldDefs }
 TSqlitePassCustomFieldDefs = Class(TSqlitePassObjectList)
  Protected
    Function GetItem(Index: Integer): TSqlitePassCustomFieldDef;
    Procedure SetItem(Index: Integer; CustomFieldDef: TSqlitePassCustomFieldDef);
  Public
    Constructor Create;
    Destructor Destroy; override;
    Function Add(CustomFieldDef: TSqlitePassCustomFieldDef): Integer;
    procedure ClearAndFreeItems;
    Property Items[Index: Integer]: TSqlitePassCustomFieldDef read GetItem write SetItem; default;
  end;


 {
  The TSqlitePassDatabaseDataTypeOptions is used by the TSqlitePassDatabase.DatatypeOptions property and
  gives you the opportunity to set custom or default behaviors for a given database.
  It lets you also define how fields.datatype will be retrieved and translated
  into pascal datatypes using TranslationRules and CustomFieldDefs.}
  TSqlitePassDatabaseDataTypeOptions = Class(TPersistent)
      Private
         {Year-Month-Day}
         FDateFormatPattern : Array[0..2, 0..1] of Word;
         {Hour-Minute-Sec-Msec}
         FTimeFormatPattern : Array[0..3, 0..1] of Word;
         {Year-Month-Day-Hour-Minute-Sec-Msec}
         FDateTimeFormatPattern : Array[0..6, 0..1] of Word;
         {True Values - False Values}
         FBooleanTrueValues    : TStringList;
         FBooleanFalseValues   : TStringList;
         FBooleanFormat        : String;
         FBooleanStorage       : TSqlitePassBooleanStorage;
         FCustomFieldDefs      : TSqlitePassCustomFieldDefs;
         FDatabase             : TSqlitePassDatabase;
         { Date Delta }
         FDateDelta            : Integer;
         FDateFormat           : String;
         FDateStorage          : TSqlitePassDateStorage;
         FDateTimeDelta        : Integer;
         FDateTimeFormat       : String;
         FDateTimeStorage      : TSqlitePassDateTimeStorage;
         FDecimalSeparator     : Char;
         FDefaultFieldType     : TFieldType;
         FDetectionMode        : TSqlitePassDataTypeDetectionMode;
         FFieldTypesDefaultTranslationRules: TSqlitePassFieldTypesDefaultTranslationRules;
         FFieldTypesTranslationRules       : TSqlitePassFieldTypesTranslationRules;
         FLoadOptions          : TSqlitePassDataTypeStorageLoadOptions;
         FSaveOptions          : TSqlitePassDataTypeStorageSaveOptions;
         FUnicodeEncoding      : TSqlitePassUnicodeEncoding;
         FTimeFormat           : String;
         FTimeStorage          : TSqlitePassTimeStorage;
         FStorageVersion       : String;
         procedure FindSubString(Chr: Char; LowerStr: String; Var StartPos, EndPos: Word);
         procedure SetFDateFormat(const Value: String);
         procedure SetFTimeFormat(const Value: String);
         procedure SetFDateTimeFormat(const Value: String);
         procedure SetFBooleanFormat(const Value: String);
         procedure SetFUnicodeEncoding(const Value: TSqlitePassUnicodeEncoding);
         procedure ReadTranslationRules(Reader: TReader);
         procedure WriteTranslationRules(Writer: TWriter);
         procedure ReadCustomFieldDefs(Reader: TReader);
         procedure WriteCustomFieldDefs(Writer: TWriter);
        Protected
         procedure DefineProperties(Filer: TFiler); override;
        Public
          { ----- Constructor / Destructor ----- }
         constructor Create(Database: TSqlitePassDatabase);
         destructor Destroy; override;
         { --- }
         procedure ApplyCustomFieldDefs;
         procedure SetDefaultPropertiesValues;
         Procedure LoadFromDatabase(_LoadOptions: TSqlitePassDataTypeStorageLoadOptions = []);
         Procedure SaveToDatabase(_SaveOptions: TSqlitePassDataTypeStorageSaveOptions = []);
         Property Database: TSqlitePassDatabase Read FDatabase;
       Published
         { ----- Properties ----- }
           { a String used as a Boolean formating template.
           The BooleanFormat is only active when the BooleanStorage property is set to "asText".
           In this case it is used to read or write boolean values stored as text to the database
           BooleanFormat is separated into two sections :
           The left part represents the text statements used for True,
           a separator '-'
           The right part represents the text statements used for False

           @italic(
           Usualy BooleanFormat takes one of the following value :
           'Yes,True,On,1,-No,False,Off,0')

           When BooleanFormat is set to "asText"
           the first value of the "True" section ('Yes' in the example)
           and the first value of the "False" sections ('No' in the example)
           are used as text representation to write boolean value to database}
         Property BooleanFormat        : String Read FBooleanFormat Write SetFBooleanFormat;

         {BooleanStorage determines how the Boolean values are stored in the database.
          It can take one of the following values :
          asInteger : the field value is stored as an integer.
          asText : the field value is stored as formated text, using the BooleanFormat property setting}
         Property BooleanStorage       : TSqlitePassBooleanStorage Read FBooleanStorage Write FBooleanStorage;

         {CustomFieldDefs : A collection of custom fielddefs.

         A custom fielddef can be used to overwrite the fielddefs translation
         already done by the DetectionMode and the TranslationRules properties.

         A custom fielddef is made of :
           A TableName and a FieldName to identify the field
           A FieldType: TfieldType (ftinteger, ftstring...etc)
           A FieldSize
           A FieldPrecision

         This property can be very usefull when a fielddef cannot be built on the datatype.
         @italic(For example, you created a table with two BLOB fields declared with the
         same datatype (CLOB) in the table create statement.
         If you want to store MemoText in the first blob and store graphic data in the second, you just
         need to create two custom fielddefs using the designtime editor
         or the sqliteToolbox program and store them in your database using LoadOptions and SaveOptions.)

         At design time, a specific editor lets you define the translation rules.}
         Property CustomFieldDefs      : TSqlitePassCustomFieldDefs Read FCustomFieldDefs Write FCustomFieldDefs;

         { a String used as a Date formating template.
           The DateFormat is only active when the DateStorage property is set to asText.
           In this case it is used to read or write date values stored as text to the database
           DateFormat accepts @bold(only Uppercases) to match Day (D), Month (M), Year (Y)
           Any other character is treated as literal and is included is the formated String

           @italic(
           usualy DateFormat takes one of the following value :
           DD-MM-YYYY
           YYYY-MM-DD
           DD/MM/YYYY
           YYYY/MM/DD)
            }
         Property DateFormat           : String Read FDateFormat Write SetFDateFormat;

         {DateStorage determines how the Date values are stored in the database.
          It can take one of the following values :
          asInteger : the field value is stored as an integer.
          asText : the field value is stored as formated text, using the DateFormat property setting}
         Property DateStorage          : TSqlitePassDateStorage Read FDateStorage Write FDateStorage;

         { a String used as a DateTime formating template.
           The DateTimeFormat is only active when the DateTimeStorage property is set to "asText".
           In this case it is used to read or write datetime value stored as text to the database
           DateFormat accepts @bold(only Uppercases for the Date part and only lowercases for the Time part)
           to match Days (D), Months (M), Years (Y), Hours (h), Minutes (m), Secondes (s) and Msecs (z)
           Any other character is treated as literal and is included is the formated String

           @italic(
           usualy DateFormat takes one of the following value :
           DD-MM-YYYY hh:mm:ss
           DD/MM/YYYY hh:mm:ss
           DD-MM-YYYY hh:mm:ss:zzz
           DD/MM/YYYY hh:mm:ss:zzz)
           Note : Kexi databases format DateTime as DD-MM-YYYYThh:mm:ss
           )
            }
         Property DateTimeFormat       : String Read FDateTimeFormat Write SetFDateTimeFormat;

         {DateTimeStorage determines how the DateTime values are stored in the database.
          It can take one of the following values :
          dtsDateTime : the field value is stored as a double value.
          dtsJulian : the field value is stored as a double value converted to a julian date.
          dtsText : the field value is stored as formated text, using the DateTimeFormat property setting
          dtsUnix : the field value is stored as a int64 value converted to an Unix date.
          dtsMac : the field value is stored as a int64 value converted to an Apple Mac date.}
         Property DateTimeStorage      : TSqlitePassDateTimeStorage Read FDateTimeStorage Write FDateTimeStorage;

         { Decimal separator : not used}
         Property DecimalSeparator     : Char Read FDecimalSeparator Write FDecimalSeparator;

         { Determines the default field type used when field type translations fails.
           It is usually set to ftUnknow or ftString}
         Property DefaultFieldType     : TFieldType Read FDefaultFieldType Write FDefaultFieldType;

         {The DetectionMode controls how the TsqlitePassDatabase component will behave when it tries to retrieve the database fields definition.
         It can have on of the following values :

         @bold(dmTypeName)@br
         FieldTypes are retrieved from the initial SQL CREATE statement of the table for example :
         'CREATE TABLE cars_names (id INTEGER PRIMARY KEY, companycode Integer, name Text(200))'
         Then the database uses the TranslationRules (a collection of translation Rule) to match the datatypes names found in the database table,  for example 'integer' with a pascal datatype (ftInteger in this example).

         @bold(dmDbSpecific)@br
         Field.datatypes are set directly by sqlitepass, depending on the Database.Databasetype value.
         Note : This can only be used with the 'dbtKexi' Databasetype.

         @bold(dmCustom)@br
         Field.datatypes are first preset directly by sqlitepass, depending on the Database.Databasetype value.
         If the first match fails, it uses the TranslationRules if you defined some
         Finally, it fires the OnDataTypeConversion event letting you modified directly the Field.Datatype Of course, the OnDataTypeConversion Event must be assigned...

         @bold(dmForceStr)@br
         Converts any Datatype to ftString;}
         Property DetectionMode        : TSqlitePassDataTypeDetectionMode Read FDetectionMode Write FDetectionMode;

         {if SoManual is not selected (default behavior) your optional settings are retrieved directly from
          a special table named "SQLitePass__DbSettings" every time you open a database

           The LoadOptions property determines which parts of the database DataTypeOptions are retrieved from the table
           It can take one or more of the following values :
           soProperties : It retrieves your settings for
                        BooleanExtension;
                        BooleanStorage;
                        DateFormat;
                        DateStorage;
                        DateTimeFormat;
                        DateTimeStorage;
                        DecimalSeparator;
                        DefaultFieldType;
                        DetectionMode;
                        TimeFormat;
                        TimeStorage;

           soTranslationRules : It retrieves and fills the TranslationRules collection.
           soCustomFieldDefs : It retrieves and fills the CustomFieldDefs collection.

           soManual : The selected parts are not retrieved automatically.
           You will then have to call LoadFromDatabase to load your database settings.
           }
         Property LoadOptions          : TSqlitePassDataTypeStorageLoadOptions Read FLoadOptions Write FLoadOptions;

         {if SoManual is not selected (default behavior) your optional settings are stored directly to
          a special table named "SQLitePass__DbSettings" every time you close a database.

           The SaveOptions property determines which parts of the database DataTypeOptions are saved from the table
           It can take one or more of the following values :
           soProperties : It saves your settings for
                        BooleanExtension;
                        BooleanStorage;
                        DateFormat;
                        DateStorage;
                        DateTimeFormat;
                        DateTimeStorage;
                        DecimalSeparator;
                        DefaultFieldType;
                        DetectionMode;
                        TimeFormat;
                        TimeStorage;

           soTranslationRules : It saves the the TranslationRules collection to the database.
           soCustomFieldDefs : It saves the CustomFieldDefs collection to the database.

           soManual : The selected parts are not saved automatically.
           You will then have to call SaveToDatabase to save your database settings.
           }
         Property SaveOptions          : TSqlitePassDataTypeStorageSaveOptions Read FSaveOptions Write FSaveOptions;

         {Unicode UTF8 and UTF16 support :
          The components are now 'unicode friendly' with UTF8 and UTF16 support for SQL statements,
          Table names, field names, field Data...etc.

          The property UnicodeEncoding sets how text, strings or memo are
          retrieved from the database, encoded, displayed and eventually put back to the database.
          @br
          This property is independant of the Database.Options.Encoding property which is readonly :
          you can read or write strings as UTF16 from an UTF8 Database and vice-versa. // TODO : To be tested...
          @br
          UnicodeEncoding can take one of the following value (ueAuto, ueUTF8, ueUTF16, ueRawText).
          UnicodeEncoding doesn't really change the way text is encoded. It simply overwrite the field
          definition for a given table column, changing the original ftString to ftWideString, ftMemo to
          ftWideMemo or vice-versa.
          @br
          Whatever delphi or fpc version you use (unless you set UnicodeEncoding to ueRawText),
          SqlitePassDatabase uses the following rules :
          @unorderedlist(
          @item A ftString field is ALWAYS encoded as UTF8,
          @item A ftMemo field is ALWAYS encoded as UTF8,
          @item A ftWideString field is ALWAYS encoded as UTF16,
          @item A ftWideMemo field is ALWAYS encoded as UTF16.)
          @br
          The fields conversions applies only for dynamically created fields (not the
          ones already set at design time).
          If a custom conversion rule is already set for a given table field, it will always
          have the priority for the final field definition. @seealso(TSqlitePassDatabaseDataTypeOptions.CustomFieldDefs).
          @br
          The UnicodeEncoding default value is ueAuto.
          @unorderedlist(
          @item ueAuto: For Delphi prior 2009 and fpc = ueUTF8 ; ueUTF16 for Delphi 2009 and up
          @item ueUtf8: ftWideString fields are converted to ftString, ftWideMemo are converted to ftMemo,
          @item ueUtf16: ftString fields are converted to ftWideString, ftMemo are converted to ftWideMemo,
          @item ueRawText: no change is done to the fieldDefs, no encoding is done when retrieving data.)
          @br
          @br
          Note :
          Delphi, prior Delphi 2009, doesn't handle Unicode in classic vcl. In order to write Unicode applications you have to
          use third part libraries like tnt unicode controls or utf8-vcl (freeware). The delphi 4 demo program uses utf8-vcl which is not
          very reliable (memory leaks and pointers crashes) but good enougth to test the database components.

          Delphi 2009 offers native Unicode support as UTF16.
          Lazarus-fpc offers native Unicode support as UTF8 : it is a nice alternative to Delphi prior Delphi 2009 to build unicode applications.
          }
         Property UnicodeEncoding       : TSqlitePassUnicodeEncoding Read FUnicodeEncoding Write SetFUnicodeEncoding;

         { a String used as a Time formating template.
           The TimeFormat is only active when the DateStorage property is set to "asText".
           In this case it is used to read or write Time values stored as text to the database
           DateFormat accepts @bold(only lowercases) to match Hours (h), Minutes (m), Secondes (s) and Msecs (z)
           Any other character is treated as literal and is included is the formated String
           @italic(
           usualy TimeFormat takes one of the following value :
           hh:mm:ss
           hh:mm:ss:zzz
           hh-mm-ss
           hh-mm-ss-zzz)}
         Property TimeFormat           : String Read FTimeFormat Write SetFTimeFormat;

         {TimeStorage dtermines how the Time values are stored in the database.
          It can take one of the following values :
          asInteger : the field value is stored as an integer.
          asText : the field value is stored as formated text, using the TimeFormat property setting}
         Property TimeStorage          : TSqlitePassTimeStorage Read FTimeStorage Write FTimeStorage;

         {TranslationRules : A collection of translation rules.
         TranslationRules are stored in *.DFM or *.LFM file but can also be stored directly in the database using
         the LoadOptions and SaveOptions properties.

         A translation rule is made of :
           A datatype name (whatever you want depending on how fields datatypes are named inside the database or the table create statement)
           A matching rule (mmExact, mmExactNoCase, mmPartial, mmPartialNoCase, mmAll);
           A resulting datatype (ft... pascal fieldtype)


         The matching rule can be set for each translation rule and can be one of the following values:

         @bold(mmExact)
         the datatype found in the database table definition must match exactly
         the rule datatype name. It is case sensitive.

         @italic(For example if the 'Int' name is found in the database table definition
         then it will not match the ('Integer',mmExact,ftInteger) rule thus ftUnknown
         will be returned)

         @bold(mmExactNoCase)
         Same as mmExact but not case sensitive

         @bold(mmPartial)
         the datatype found in the database table definition can partially match the rule datatype name.
         It is case sensitive.
         @italic(For example if the 'Int' name is found in the database table definition then
         it will match the ('Integer',mmExact,ftInteger) rule and will return ftInteger)

         @bold(mmPartialNoCase)
         Same as mmPartial but not case sensitive

         @bold(mmAll (Default))
         Tries to determine the datatype using mmExact.
         If it fails then another try is done using mmPartial
         If the matching rule failed, the DefaultFieldType is returned (ftUnknown as default).

         At design time, a specific editor lets you define the translation rules.}
         Property TranslationRules     : TSqlitePassFieldTypesTranslationRules Read FFieldTypesTranslationRules Write FFieldTypesTranslationRules;
      end;

 { }
 TSqlitePassDatabaseError = class
 Private
   FDatabase: TSqlitePassDatabase;
   FErrorCount: Integer;
   FErrorStringList: TStringList;
   FLastErrorCode: Integer;
   FLastErrorMsg: String;
   FLastErrorTime: TDateTime;
   procedure LogError(Msg: String; const Args: array of const; const ErrorCode: Integer = -1; Component: TComponent = nil);
   procedure RaiseException(const Msg: String; const ErrorCode: Integer = -1;  Component: TComponent = nil; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow);
   procedure RaiseExceptionFmt(const Msg: String; const Args: array of const; const ErrorCode: Integer = -1; Component: TComponent = nil; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow);
   function  ShowError(const Msg: String; DlgType: TMsgDlgType = mtWarning; DlgButtons: TMsgDlgButtons = [mbOk]; HelpCtx: Longint = 0): Word;
 Public
   { ----- Constructor / Destructor ----- }
   Constructor Create(Database: TSqlitePassDatabase);
   Destructor Destroy; override;
   { ----- Procedures / Functions ----- }
   procedure Clear;
   procedure GetLastError(var ErrorCode: Integer;var Msg: String;var Time: TDateTime);
   procedure SaveToFile(FileName: String);
   property ErrorCount: Integer Read FErrorCount;
   property ErrorList: TStringList Read FErrorStringList;
  end;


 { TSqlitePassDatabase Events }
 TConnectEvent = procedure(Database: TSqlitePassDatabase) of object;
 TDataTypeConversion = procedure (Database: TSqlitePassDatabase; TableDef: TSqlitePassTableDef;
                                  FiedDef: TSqlitePassTableFieldDef) of object;

 {The TSqlitePassDatabase component is the main link between your
  application and the sqlite library.
  It currently supports sqlite engine version 3.xx and gives you direct acces to
  a wide range of sqlite databases created using database management progams like kexi,
  sqlite administrator, sqliteToolbox...etc.
  With proper setting, you can also use it to read and write to your own custom sqlite databases.
  }
 TSqlitePassDatabase = Class(TComponent)
       Private
         {$IFDEF DEBUG_SQLITEPASS}
         FDebugger: TSqlitePassDebugger;
         {$ENDIF}
         FConnected          : Boolean;
         FWaitingForConnection: Boolean;
         FCollatingOrder     : String;
         FDatabase           : String;
         FDatabaseError      : TSqlitePassDatabaseError;
         FDatasets           : TSqlitePassDatasets;
         FTableDefs          : TSqlitePassTableDefs;
         FIndexDefs          : TSqlitePassDatabaseIndexDefs;
         FQueryDefs          : TSqlitePassQueryDefs;
         FViews              : TSqlitePassViews;
         FTriggers           : TSqlitePassTriggers;
         FSystemEncoding     : TSqlitePassSystemEncoding;
         FSQLStmtDefs        : TSqlitePassSQLStmtDefs;
         FTempRecordset      : TSqlitePassRecordset;
         { Temporary List used by RefreshDefinition to get Attached Databases info }
         FDatabasesList      : TList;
         { AttachedDatabases Object List }
         FDatabases          : TSqlitePassDatabasesAttached;
         FDatabaseType       : TSqlitePassDatabaseType;
         FDataTypeOptions    : TSqlitePassDatabaseDataTypeOptions;
         FEngine             : TSqlitePassEngine;
         FTranslator         : TSqlitePassTranslator;
         FOptions            : TSqlitePassDatabaseOptions;
         FQueryTimeout       : Integer;
         FReadOnly           : Boolean;
         FShowSysObjects     : Boolean;
         FState              : TSqlitePassDatabaseState;
         FVersionInfo        : TSqlitePassDatabaseVersionInfo;
         FSQLiteLibrary      : String;

         { ----- Events ---- }
         FAfterConnect       : TConnectEvent;
         FAfterDisconnect    : TConnectEvent;
         FBeforeConnect      : TConnectEvent;
         FBeforeDisconnect   : TConnectEvent;
         FDataTypeConversion : TDataTypeConversion;

         { Screen utility }
         FSavedScreenCursor  :TCursor;

         { Private Functions and Procedures }
         function  CheckCanOpen(VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow): Boolean;
         function  CheckCanChangePropertyValue(PropertyPrivateName, PropertyPublicName: String): Boolean;
         function  DatabaseTypeFromFileName(FileName: String): TSqlitePassDatabaseType;
         Procedure SetFDatabase(Value : String);
         procedure SetFDatabaseType(const Value: TSqlitePassDatabaseType);
         function  GetFCollatingOrder:String;
         function  GetFConnected: Boolean;
         Procedure SetFShowSysObjects(Value : Boolean);
         Procedure SetFSQLiteLibrary(Value : String);
         Procedure SetFReadOnly(Value : Boolean);
         Procedure SetFConnected(Value : Boolean);
         function  GetFEngine: TSqlitePassEngine;
         procedure ClearDefinitions;
         procedure OpenAttachedDatabases;
         procedure CloseAttachedDatabases;
         procedure ShowBusyScreenCursor;
         procedure RestoreScreenCursor;

         { ------ Pragma ------ }
         procedure SetPragma(PragmaName:String; value:Boolean);overload;
         procedure SetPragma(PragmaName:String; value:Integer);overload;
         procedure SetPragma(PragmaName, value:String);overload;
         function  GetStrPragma(PragmaName:String):String;
         function  GetIntPragma(PragmaName:String):Integer;
       Protected
         Procedure Loaded; override;
       Public
         {$IFDEF DEBUG_SQLITEPASS}
         Property Debugger: TSqlitePassDebugger read FDebugger write FDebugger;
         {$ENDIF}

         { ----- Constructor / Destructor ----- }

         Constructor Create(AOwner : TComponent); override;
         Destructor Destroy; override;

         {Same as Connected := True}
         Procedure Open;

         {Same as Connected := False }
         Procedure Close;

         { Compacts the database content and free unused space }
         Function Compact: Integer;

         { Checks the database integrity and returns the results messages
           in MsgList  }
         procedure CheckIntegrity(MsgList: TStringList; MaxErrorCount: Integer = 100);

         { Creates a new database }
         Function CreateDatabase(DbName: String; DbType: TSqlitePassDatabaseType;
                                 DbEncoding: TSqlitePassEncoding = UTF8; PageSize: TSqlitePassPageSize = 4096;
                                 AutoVacuum: TSqlitePassAutoVacuumType = avNone): Boolean;

         { Deletes a database file. Use with caution... }
         Function DeleteDatabase(Const DbName: String): Boolean;

         {CreateFunc References a new scalar User Defined function in the Sqlite Engine API
          As an Example, you can use an IsOdd function to filter the records
          returned by a query :
          @br
          1 - Reference the IsOdd function in Sqlite Engine
          procedure TMainForm.Button1Click(Sender: TObject);
          begin
            db.CreateFunction('IsOdd',1,@IsOdd, nil);
          end;
          @br
          2 - Execute a query that uses the IsOdd function
          for instance " Select  IntegerField from  "IntegerTable" where  IsOdd(IntegerField)>0;"
          @be
          3 - The IsOdd Function is called every time SQLite processes a
          row returned by the query
          @br
          4 - Now, the Query returns Only Odd Values...
          @br
          Note : The function must be declared 'cdecl'. Please, have a look at SQLite online help
          for further information on UDF Functions.
          @br
          @br
          Procedure IsOdd implementation (From SqliteToolBox) :
          @br
          procedure IsOdd(Context: Pointer; nArgs: Integer; Args: ppSqliteValue); cdecl;
          var
          TempRes, Res:integer;
          begin
            // We get the initial value of the current row returned by the query
            TempRes := StrToIntDef(SqlitePassApi_v3.SqliteDbv3_value_text(Args^),0);
            // We do something with TempRes
            if (TempRes mod 2) > 0
               then Res := TempRes
               else Res := 0;
            // We send back the changed value to Sqlite
            SqlitePassApi_v3.SqliteDbv3_result_int(Context,Res);
          end;

          }
         Function CreateFunction(FuncName: String; ArgCount: ShortInt; Func: TFuncHandler; UserData: Pointer; DefaultEncoding: Byte = SQLITE_ANY): Boolean;

        { CreateAggFunc References a new Aggregate User Defined function in the Sqlite Engine API
          As an Example, you can use an SumOdd function to get the Sum of Odd Values :
          @br
          1 - Reference the SumOdd function in Sqlite Engine
          Unit...
          var
          SumOddResult: Integer;

          procedure TMainForm.Button1Click(Sender: TObject);
          begin
            SumOddResult := 0;
            db.CreateAggFunction('SumOdd', 1, @SumOddStep, @SumOddFinal, @SumOddResult);
          end;
          @br
          2 - Execute a query that uses the SumOdd function
          for instance " Select SumOdd(IntegerField) From IntegerTable;"
          @be
          3 - The SumOdd Function is called every time SQLite processes a
          row returned by the query
          @br
          4 - Now, the Query returns the sum of odd Values...
          @br
          Note : The function must be declared 'cdecl'.
          Please, have a look at SQLite online help
          for further information on UDF Functions.
          @br
          @br
          Procedure SumOdd implementation :
          @br
          procedure SumOddStep(Context: Pointer; nArgs: Integer; Args: ppSqliteValue); cdecl;
          var
          TempRes, Res:integer;
          begin
          // We get the initial value of the current row returned by the query
          TempRes := StrToIntDef(SqlitePassApi_v3.SqliteDbv3_value_text(Args^),0);
          // We do something with TempRes
          if (TempRes mod 2) > 0
           then SumOddResult := SumOddResult + TempRes;
          end;

          procedure SumOddFinal(Context: Pointer); cdecl;
          begin
          // Send back the final result to SQLite Engine
          SqlitePassApi_v3.SqliteDbv3_result_int(Context,SumOddResult);
          // Reset the value for another query
          SumOddResult := 0;
          end;
          }
         Function CreateAggFunction(FuncName: String; ArgCount: ShortInt; FuncStep: TFuncHandler; FuncFinal: TFuncFinalizer; UserData: Pointer; DefaultEncoding: Byte = SQLITE_ANY): Boolean;

         { Remove a previously referenced User Defined Function }
         Function DeleteFunction(FuncName: String): Boolean;

         {Enables to attach one or several foreign databases to the current one.
          The attached databases must be compatible with the current one
          (the databases must have been created with the same database manager application).
          Once a database is attached, its content becomes available as part of the current database.
          Then you can access to tables, queries... as if they were part of the main database.
          Usage : AttachDatabase(DatabasePath: String);}
         Function AttachDatabase(FAttachedDatabase, DatabaseAlias: String): Boolean;

         {Detaches a previously attached database.}
         Function DetachDatabase(DatabaseAlias: String): Boolean;

         { Updates the database definition content (TableDefs, QueryDefs...) from file }
         procedure RefreshDefinitions;

         { Returns True if the Table is a system table }
         Function IsSystemTable(TableName: String): Boolean;
         { ----- Properties ----- }

         { A utility object to log errors }
         Property DatabaseError: TSqlitePassDatabaseError Read FDatabaseError;

         { Internal object to access the sqlite engine }
         Property Engine: TSqlitePassEngine Read GetFEngine;

         { translation object used to get or set data that depend on
           the DatabaseType property }
         Property Translator: TSqlitePassTranslator Read FTranslator;

         { A collection of attached database to the current one }
         property Databases: TSqlitePassDatabasesAttached Read FDatabases;

         { A collection of all the datasets that use the current database component }
         Property Datasets: TSqlitePassDatasets Read FDatasets Write FDatasets;

         {The IndexDefs property is a collection that
          gives you access to the indexes definitions stored in the database.}
         Property IndexDefs: TSqlitePassDatabaseIndexDefs  Read FIndexDefs Write FIndexDefs;

         {The QueryDefs property is a collection that
          gives you access to the queries definitons stored in the database.}
         property QueryDefs: TSqlitePassQueryDefs Read FQueryDefs Write FQueryDefs;

         {SystemEncoding lets you know how pascal compiler handles unicode strings.

         @bold(sysUTF8) :
         For Delphi, prior Delphi 2009, and Lazarus, Database.SystemEncoding default value is sysUTF8.

         @bold(sysUTF16) :
         For Delphi 2009 and up : Database.SystemEncoding default value is sysUTF16.

         SystemEncoding show how the SQL statements sent to the database engine have
         to be encoded according to the compiler setting.
         You shouldn't have to care to much about this since the component will handle this for you.}
         property SystemEncoding: TSqlitePassSystemEncoding Read FSystemEncoding;

         property SQLStmtDefs:  TSqlitePassSQLStmtDefs Read FSQLStmtDefs Write FSQLStmtDefs;

         {The TableDefs property is a collection that
          gives you access to the tables definitons stored in the database.}
         property TableDefs: TSqlitePassTableDefs Read FTableDefs Write FTableDefs;

         {The ViewDefs property is a collection that
         gives you access to the views definitons stored in the database.}
         property Views: TSqlitePassViews Read FViews Write FViews;

         {The TriggerDefs property is a collection that
         gives you access to the Triggers definitons stored in the database.}
         property Triggers: TSqlitePassTriggers Read FTriggers Write FTriggers;
       Published
         { ----- Properties ----- }

         {Set this property to True to connect the database defined in the database property.
          Set it to False to disconnect the database and all the datasets associated with it.
          You can also verify if a database is connected using :
          @code(if MyDatabase.Connected then...)}
         Property Connected            : Boolean Read GetFConnected        Write SetFConnected Default False;

         {Represents the physical database file you want to connect. At design time, shows up a file selection dialog box.}
         Property Database             : String  Read FDatabase            Write SetFDatabase;


         {Represents the database type detected from the database file extension you are using.
          (*.kexi for kexi database for example).
          Once the database type is recognised, the TsqlitePassDatabase component sets this property and creates
          an internal translator to take care of the database specifications.
          You can also define this property by yourself if the file extension doesn't match the correct database type.}
         Property DatabaseType         : TSqlitePassDatabaseType Read FDatabaseType Write FDatabaseType;


         {One of the main difficulties when working with Sqlite databases is to detect
          and translate properly the fields.datatypes since sqlite datatype are not formely defined.
          SqlitePass implements several ways to define a datatype.
          The Database.DatatypeOptions property gives you the opportunity to set custom or
          default behaviors for a given database and lets you also define how fields.datatype
          will be retrieved and translated into pascal datatypes using TranslationRules and CustomFieldDefs}
         Property DatatypeOptions      : TSqlitePassDatabaseDataTypeOptions Read FDatatypeOptions Write FDatatypeOptions;

         {Represents the database optional settings.}
         Property Options              : TSqlitePassDatabaseOptions Read FOptions Write FOptions;


         Property QueryTimeout         : Integer Read FQueryTimeout        Write FQueryTimeout;

         { Same as classic dataset behavior }
         Property ReadOnly             : Boolean Read FReadOnly            Write SetFReadOnly default False;

         { }
         Property ShowSystemObjects    : Boolean Read FShowSysObjects      Write SetFShowSysObjects;


         {Represents an alternative library file to be used instead of the default one.
          By default, sqlitepassDatabase tries to use the sqlitepass3.dll or libsqlitepass3.so file
          located in the system path directory (..\windows\system32\ or ..\user\lib for example).
          Enter a complete library file path to use a different library.
          From version 0.28 TsqlitePassDatabase needs the sqlite library compiled
          with the ENABLE_METADATA precompiler directive.
          These libraries are available from http://source.online.free.fr
          or you can compile your own following the tutorial available on the same internet site.}
         Property SQLiteLibrary        : String  Read FSQLiteLibrary       Write SetFSQLiteLibrary;

         Property VersionInfo          : TSqlitePassDatabaseVersionInfo  Read FVersionInfo   Write FVersionInfo;

          { ----- Events ----- }
         Property OnAfterConnect       : TConnectEvent Read FAfterConnect        Write FAfterConnect;
         Property OnAfterDisconnect    : TConnectEvent Read FAfterDisconnect     Write FAfterDisconnect;
         Property OnBeforeConnect      : TConnectEvent Read FBeforeConnect       Write FBeforeConnect;
         Property OnBeforeDisconnect   : TConnectEvent Read FBeforeDisconnect    Write FBeforeDisconnect;
         Property OnDataTypeConversion : TDataTypeConversion Read FDataTypeConversion Write FDataTypeConversion;
      end;

{ Internal function to compare buffers value when sorting or indexing fields }
TSqlitePassCompareColumnValue = function(Const Recordset: TSqlitePassRecordset;
                                         Const FieldBufferValue1, FieldBufferValue2: Pointer): Integer;

TSqlitePassInMemoryIndexInfo = Record
     Field: TField;
     FieldValueOffset: Integer; { Field offset Position in memory buffer }
     Ascending: Boolean; { 'ASC' = True }
     ValueComparator: TSqlitePassCompareColumnValue;
     end;

TSqlitePassInMemoryIndexInfos = Array of TSqlitePassInMemoryIndexInfo;


 { TSqlitePassGenericFieldDef
   is a generic class used to store information on fields presents in the database tables.
   This class is used by two descendants : TSqlitePassTableFieldDef and  TSqlitePassSelectStmtFieldDef }
 TSqlitePassGenericFieldDef = Class
  Private
   FNoConstraints: Boolean;
   FAutoInc: Boolean;
   FUnique: Boolean;
   FPrimaryKey: Boolean;
   FForeignKey: Boolean;
   FNotNull: Boolean;
   FNotEmpty: Boolean; { only legal for String-like and blob fields }
   FIndexed: Boolean;  { Indexed by sqlite engine ? }
   FDatabaseOrigineNameIndex: Integer;
   FTableOrigineNameIndex: Integer;
   FFieldName: String;
   FDisplayName: String;
   FSize: Word;
   FPrecision: Integer;
   FNativeDataType: String;
   FNativeDataTypeCode: Integer;
   FDataType: TFieldType;
   FDefaultValue: String;
   FTableFieldNo: Integer;
   FRecordFieldIndex: Integer;
   FHint: String;
   procedure Assign(Source: TSqlitePassGenericFieldDef);
   procedure SetFDataType(Value: TFieldType); virtual;
   procedure SetFFieldName(Value: String); virtual;
  Public
   Property AutoInc: Boolean Read FAutoInc;
   { write is enabled to force a DataType conversion }
   Property DataType: TFieldType Read FDataType Write SetFDataType;
   Property DefaultValue: String Read FDefaultValue;
   { write is enabled to enable changing default display name }
   Property DisplayName: String Read FDisplayName Write FDisplayName;
   Property FieldName: String Read FFieldName;
   Property TableFieldNo: Integer Read FTableFieldNo;
   Property ForeignKey: Boolean Read FForeignKey;
   { write is enabled to enable changing default hint message }
   Property Hint: String Read FHint Write FHint;
   Property Indexed: Boolean Read FIndexed;
   Property NativeDataType: String Read FNativeDataType;
   Property NativeDataTypeCode: Integer Read FNativeDataTypeCode Write FNativeDataTypeCode;
   Property NotEmpty: Boolean Read FNotEmpty;
   Property NoConstraints: Boolean Read FNoConstraints;
   Property NotNull: Boolean Read FNotNull;
   Property Precision: Integer Read FPrecision;
   Property PrimaryKey: Boolean Read FPrimaryKey;
   { The internal FieldIndex(Field Position in TSqlitePassRecordset Record) }
   Property RecordFieldIndex: Integer Read FRecordFieldIndex;
   Property Size: Word Read FSize;
   Property Unique: Boolean Read FUnique;
  end;

 { TSqlitePassTableFieldDef
   is mainly used to get information on fields presents in the database tables and
   to build a static schema of the database }
TSqlitePassTableFieldDef = Class(TSqlitePassGenericFieldDef)
  Private
  FFieldDefs: TSqlitePassTableFieldDefs;
  function GetFFieldFullName: String;
  Public
  Constructor Create(Owner: TSqlitePassTableFieldDefs);
  Destructor Destroy; override;
  procedure Assign(Source: TSqlitePassTableFieldDef);
  Property FieldFullName: String Read GetFFieldFullName;
  Property FieldDefs: TSqlitePassTableFieldDefs Read FFieldDefs;
  end;

{ TSqlitePassSelectStmtFieldDef
  is used to store information on fields returned by a SELECT Statement database.
  The fields definitions are mainly retrieved from the database schema.
  @br
  @seealso(TSqlitePassTableFieldDef)
  @br
  Setting the field Datatype property will also define how the field will interact with
  the database for reading, writing, sorting and filtering.
  }
TSqlitePassBufferToSqliteValue = procedure (Const Recordset: TSqlitePassRecordset;
                                            Const RecordBuffer: PRecBuffer;
                                            Const RecordFieldIndex: Integer;
                                            Const PreparedStmt: Pointer;
                                            Const PreparedStmtFieldNo: Integer);

TSqlitePassSelectStmtFieldDef = Class(TSqlitePassGenericFieldDef)
  Private
  FFieldDefs: TSqlitePassSelectStmtFieldDefs;
  FFieldUpdateStmt: String;
  BindValue: TSqlitePassBufferToSqliteValue;
  function GetFFieldFullName: String;
  procedure SetFDataType(Value: TFieldType); override;
  procedure SetFFieldName(Value: String); override;
  Public
  Constructor Create(Owner: TSqlitePassSelectStmtFieldDefs);
  Destructor Destroy; override;
  procedure Assign(Source: TSqlitePassTableFieldDef);
  Property FieldFullName: String Read GetFFieldFullName;
  Property FieldDefs: TSqlitePassSelectStmtFieldDefs Read FFieldDefs;
  end;

{ TSqlitePassTableFieldDefs
  is a collection of TSqlitePassTableFieldDef }
TSqlitePassTableFieldDefs = class(TSqlitePassObjectList)
  private
  FTableDef: TSqlitePassTableDef;
  Protected
  procedure ClearAndFreeItems;
  function GetItem(Index: Integer): TSqlitePassTableFieldDef;
  procedure SetItem(Index: Integer; const Value: TSqlitePassTableFieldDef);
  public
  Constructor Create(Owner: TSqlitePassTableDef);
  Destructor Destroy; override;
  procedure Assign(Source: TSqlitePassTableFieldDefs);
  function FieldDefByName(const Value: String): TSqlitePassTableFieldDef;
  function FindFieldDef(const Value: String): TSqlitePassTableFieldDef;
  property Count;
  property Items[Index: Integer]: TSqlitePassTableFieldDef read GetItem write SetItem; default;
  property TableDef: TSqlitePassTableDef read FTableDef;
  end;

{ TSqlitePassSelectStmtFieldDefs
  is a collection of TSqlitePassSelectStmtFieldDef}
TSqlitePassSelectStmtFieldDefs = class(TSqlitePassObjectList)
  private
  FTableDef: TSqlitePassSelectStmtTableDef;
  function GetItem(Index: Integer): TSqlitePassSelectStmtFieldDef;
  procedure SetItem(Index: Integer; const Value: TSqlitePassSelectStmtFieldDef);
  procedure ClearAndFreeItems;
  public
  Constructor Create(Owner: TSqlitePassSelectStmtTableDef);
  Destructor Destroy; override;
  function FieldDefByName(const Value: String): TSqlitePassSelectStmtFieldDef;
  function FindFieldDef(const Value: String): TSqlitePassSelectStmtFieldDef;
  procedure Assign(Source: TSqlitePassTableFieldDefs);
  property Count;
  property Items[Index: Integer]: TSqlitePassSelectStmtFieldDef read GetItem write SetItem; default;
  property TableDef: TSqlitePassSelectStmtTableDef read FTableDef;
  end;


TFieldFilterExpAcceptValueFunc = function(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean;

TSqlitePassFieldFilterExp = Class
  private
  { Owner }
  FFieldFilter: TSqlitePassFieldFilter;
  FLogicalOperator: TSqlitePassFilterLogicalOperators;
  FComparisonOperator: TSqlitePassFilterComparisonOperators;
  FValueMem: Pointer; { Pointer to original GetMem memory address }
  FValue: Pointer;
  FTextValue: String;
  AcceptValue: TFieldFilterExpAcceptValueFunc;
  { Binary search comparison }
  IsEqual: TFieldFilterExpAcceptValueFunc;
  IsLesser: TFieldFilterExpAcceptValueFunc;
  procedure BinarySetResultRecordsRanges(var RangeStart, RangeStop: Integer);
  function ComparisonOpAsText: String;
  function GetExpressionText: String;
  function LogicalOpAsText: String;
  { --- }
  procedure SetFComparisonOperator(Value: TSqlitePassFilterComparisonOperators);
  procedure SetFValue(const StrValue: String);
  public
  constructor Create(Owner: TSqlitePassFieldFilter);
  destructor Destroy; override;
  property ComparisonOperator: TSqlitePassFilterComparisonOperators Read FComparisonOperator Write SetFComparisonOperator;
  Property ExpressionText: String Read GetExpressionText;
  property LogicalOperator: TSqlitePassFilterLogicalOperators Read FLogicalOperator Write FLogicalOperator;
  property Value: String Read FTextValue Write SetFValue;
  end;

TSqlitePassFieldFilter = class(TSqlitePassObjectList)
  Private
  FFieldFilters: TSqlitePassFieldFilters;
  FField: TField;
  { InMemory Index if one is ready and available }
  FInMemoryIndex: TSqlitePassInMemoryIndex;
  { Index of the Keyfield field used to fill the filter value (for lookupfields) }
  FKeyFieldIndex: Integer;
  { Can the Filter value be set directly from a record buffer (for lookupfields) ? }
  FCanGetValueFromRecordBuffer: Boolean;
  { Indicates wether or not a filter is active on the field value }
  FFiltered: Boolean;
  procedure SetKeyField(Const KeyField: TField);
  procedure NotifyFilterChanged;
  function AcceptValue(Const FieldValue: Pointer):Boolean;
  function GetFilterText: String;
  function GetHasFilterExpression: Boolean;
  procedure ClearAndFreeItems;
  procedure SetFFiltered(Value: Boolean);
  Protected
  function GetItem(Index: Integer): TSqlitePassFieldFilterExp;
  procedure SetItem(Index: Integer; const Value: TSqlitePassFieldFilterExp);
  public
  constructor Create(FieldFilters: TSqlitePassFieldFilters; Field: TField);
  destructor Destroy; override;
  procedure AddFilterExpression(LogicalOperator:TSqlitePassFilterLogicalOperators; ComparisonOperator: TSqlitePassFilterComparisonOperators; FilterValue: String);
  procedure ClearFilter;
  property Count;
  property Field: TField Read FField;
  property FilterText: String Read GetFilterText;
  Property Filtered: Boolean Read FFiltered Write SetFFiltered;
  { Indicates whether or not a filter is defined on the field value }
  Property HasFilterExpression: Boolean Read GetHasFilterExpression;
  property Items[Index: Integer]: TSqlitePassFieldFilterExp read GetItem write SetItem; default;
  end;


{ A list of defined Filters matching Fields List }
TSqlitePassFieldFilters = class(TSqlitePassObjectList)
  Private
  FDataset: TSqlitePassDataset;
  { --- }
  FActiveFilters: TList;
  FCanDoBinaryScan: Boolean;
  FFilterText: String;
  FInMemoryIndexedFilter: TSqlitePassFieldFilter;
  FMoveState: TGetResult;
  FOptions: TLocateOptions;
  FRangeStart: Integer;
  FRangeStop: Integer;
  FResultRecordsList: TSqlitePassIntegerList;
  FResultRecordsListCurrentRecordIndex: Integer;
  FResultRecordsListCurrentIndex: Integer;
  FResultRecordsMaxCount: Integer;
  FUpdating: Boolean;
  { Send an event when filters changed }
  FOnChange: TNotifyEvent;
  procedure NotifyFiltersChanged;
  procedure ClearAndFreeItems;
  function GetFFiltered: Boolean;
  procedure LinearScanRecords;
  procedure InitBinaryScan;
  procedure BinaryScanRecords;
  Function MoveToRecord: Boolean;
  Procedure ResetResultRecords;
  Protected
  function GetItem(Index: Integer): TSqlitePassFieldFilter;
  procedure SetItem(Index: Integer; const Value: TSqlitePassFieldFilter);
  procedure InitFilters;
  public
  constructor Create(Dataset: TSqlitePassDataset; OnChange: TNotifyEvent);
  destructor Destroy; override;
  function AcceptRecord(Const RecordBuffer: PRecBuffer): Boolean;
  procedure Assign(Source: TSqlitePassFieldFilters);
  procedure InitActiveFilters;
  function FilterByField(const Value: TField): TSqlitePassFieldFilter;
  function FilterByFieldName(const Value: String): TSqlitePassFieldFilter;
  function GetFilterText(FilteredOnly: Boolean = False): String;
  procedure ParseFilterText(const Value: String); overload;
  procedure ParseFilterText(const KeyFields: String; const KeyValues: Variant; FilterOptions: TLocateOptions); overload;
  { Scan the Recordset and fill FResultRecordsList with matching records }
  procedure ScanRecords;
  { Navigate Through Results }
  function First: Boolean;
  function Next: Boolean;
  function Prior: Boolean;
  function Last: Boolean;
  { --- }
  procedure EnableFilters;
  procedure DisableFilters;
  procedure ClearFilters;
  procedure BeginUpdate;
  procedure EndUpdate;
  { Properties }
  property Count;
  Property Filtered: Boolean Read GetFFiltered;
  Property FilterText: String Read FFilterText;
  property Items[Index: Integer]: TSqlitePassFieldFilter read GetItem write SetItem; default;
  property MoveState: TGetResult Read FMoveState;
  property Options: TLocateOptions Read FOptions Write FOptions;
  property Results: TSqlitePassIntegerList Read FResultRecordsList;
  end;


TSqlitePassInMemoryIndex = class
  Private
  FIndexes: TSqlitePassInMemoryIndexes;
  FField: TField;
  FIndexed:Boolean; { In memory indexed ? }
  FIndexInfos: TSqlitePassInMemoryIndexInfos;
  FIndexedRecords: TList;
  procedure SetFIndexed(Const Value: Boolean);
  function IsReady: Boolean;
  public
  constructor Create(Indexes: TSqlitePassInMemoryIndexes; Field: TField);
  destructor Destroy; override;
  procedure BuildIndex;
  procedure ClearIndex;
  property Field: TField Read FField;
  Property Indexed: Boolean Read FIndexed Write SetFIndexed;
  end;

TSqlitePassInMemoryIndexes = class(TSqlitePassObjectList)
  Private
  FDataset: TSqlitePassDataset;
  procedure ClearAndFreeItems;
  procedure SetSelectedIndexes;
  Protected
  function GetItem(Index: Integer): TSqlitePassInMemoryIndex;
  procedure SetItem(Index: Integer; const Value: TSqlitePassInMemoryIndex);
  public
  constructor Create(Dataset: TSqlitePassDataset);
  destructor Destroy; override;
  procedure BuildIndexes;
  procedure ClearIndexes;
  function IndexByField(const Value: TField): TSqlitePassInMemoryIndex;
  function IndexByFieldName(const Value: String): TSqlitePassInMemoryIndex;
  procedure InitIndexes;
  function GetIndexedFilter(Filters: TSqlitePassFieldFilters): TSqlitePassFieldFilter;
  property Count;
  property Items[Index: Integer]: TSqlitePassInMemoryIndex read GetItem write SetItem; default;
  end;


{ TSqliteAttachedDatabases : Maintain a list of all attached databases }

TSqlitePassDatabaseAttached = Class(TObject)
       Private
         FAliasName          : String;
         FDatabase           : String;
         FDatabasesList      : TSqlitePassDatabasesAttached;
       Public
         { ----- Constructor / Destructor ----- }
         Constructor Create(Databases: TSqlitePassDatabasesAttached);
         Destructor Destroy; override;
         { ----- Properties ----- }
         Property AliasName: String Read FAliasName;
         Property Database: String  Read FDatabase Write FDatabase;
      end;

TSqlitePassDatabasesAttached = Class(TSqlitePassObjectList)
  Private
   FDatabase: TSqlitePassDatabase;
  Protected
    Function GetItem(Index: Integer): TSqlitePassDatabaseAttached;
    Procedure SetItem(Index: Integer; Database: TSqlitePassDatabaseAttached);
    procedure DetachAll;
  public
    constructor Create(Owner: TSqlitePassDatabase);
    destructor Destroy; override;
    function DatabaseByName(Const Name: String): TSqlitePassDatabaseAttached;
    function FindDatabase(const Value: String): TSqlitePassDatabaseAttached;
    property Items[Index: Integer]: TSqlitePassDatabaseAttached read GetItem write SetItem; default;
    property Count;
  end;


{ TableDefs }

TSqlitePassGenericTableDef = class
  private
  FAttachedDatabase: TSqlitePassDatabaseAttached;
  FAliasName: String;
  FCanModify: Boolean;
  FDatabaseOrigineNameIndex: Integer;
  FOnConflict: String;
  FSql: String;
  FTableName: String;
  FTemporary: Boolean;
  function GetAttached: Boolean;
  function GetTableFullName: String;
  Public
  procedure Assign(Source: TSqlitePassGenericTableDef; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  Property AliasName: String Read FAliasName;
  property Attached: Boolean Read GetAttached;
  Property CanModify: Boolean Read FCanModify;
  Property OnConlict: String Read FOnConflict;
  property Sql: String Read FSql;
  property TableName: String Read FTableName;
  property TableFullName: String Read GetTableFullName;
  Property Temporary: Boolean Read FTemporary;
  end;

TSqlitePassTableDef = class(TSqlitePassGenericTableDef)
  private
  { Owner }
  FTableDefs: TSqlitePassTableDefs;
  { FieldDefs }
  FFieldDefs: TSqlitePassTableFieldDefs;
  Public
  Constructor Create(Owner: TSqlitePassTableDefs);
  Destructor Destroy; override;
  procedure Assign(Source: TSqlitePassTableDef; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  procedure CopyTable(NewTableName: String; TempTable: Boolean = False; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  procedure DeleteTable(Silent: Boolean = False);
  procedure RenameAs(NewName: String);
  procedure Reindex;
  property FieldDefs: TSqlitePassTableFieldDefs Read FFieldDefs;
  end;

{ TSqlitePassSelectStmtTableDef :
  When a SQL select statement is executed, a TSqlitePassSelectStmtTableDef is generated
  for each table used in the SQL statement.
  Each TSqlitePassSelectStmtTableDef stores information on one table in order to know
  how to insert, update or delete records.

  for an insert statement, we need to know the :
      name of the table  (from TSqlitePassGenericTableDef.TableName or FullTableName properties)
      fields names (from the fieldDefs property)
      fields values to be inserted (from the recordset)
      fields Datatype (from the fieldDefs property)
      fields BufferSize (from the fieldDefs property)

  for an update statement, we need to know the  :
      if the record was modified (at least one field was modified) from the recordset
      name of the table
      rowid number of the record (from the recordset)
      fields values to be inserted (from the recordset and translate it to )
      fields index
      fields Datatype
      fields BufferSize

  for a delete statement, we need to know the
      name of the table
      rowid number of the record
      }

TSqlitePassSelectStmtTableDef = class(TSqlitePassGenericTableDef)
  private
  { Owner }
  FTableDefs: TSqlitePassSelectStmtTableDefs;
  { Prepared sql statements to speed up operations }
  FInsertStmt: Pointer;
  FUpdateStmt: Pointer;
  FDeleteStmt: Pointer;
  FInsertStmtText: String;
  FUpdateStmtText: String;
  FDeleteStmtText: String;
  { InternalPrimaryKey }
  FLastInsertedRowId: Int64;
  { Internal TableDef Position in TableDefs Collection }
  FTableDefNo: Integer;
  { A FieldDefs 'sub'-collection representing the 'AutoInc' fields included in the SQL Select Statement
   in order to speed up things in TSqlitePassDataset.UpdateAutoIncValues }
  FAutoIncFieldDefs: TSqlitePassSelectStmtFieldDefs;
  { Available operations on the table }
  FAvailableOperations: TSqlitePassAvailableOperations;
  { FieldDefs }
  FFieldDefs: TSqlitePassSelectStmtFieldDefs;
  Procedure UpdateAutoIncValues(Const RecordBuffer: PRecBuffer);
  public
  Constructor Create(Owner: TSqlitePassSelectStmtTableDefs);
  Destructor Destroy; override;
  procedure Assign(Source: TSqlitePassTableDef);
  procedure PrepareInsertDeleteSQLStmts;
  procedure UnprepareSQLStmts;
  procedure PostInsert(Const RecordBuffer: PRecBuffer);
  procedure PostUpdate(Const RowId: Int64; Const RecordBuffer: PRecBuffer);
  procedure PostDelete(Const RowId: Int64; Const RecordBuffer: PRecBuffer);
  property AvailableOperations: TSqlitePassAvailableOperations Read FAvailableOperations;
  property FieldDefs: TSqlitePassSelectStmtFieldDefs Read FFieldDefs;
  end;

TSqlitePassTableDefs = class(TSqlitePassObjectList)
  private
  FDatabase: TSqlitePassDatabase;
  Protected
  procedure ClearAndFreeItems;
  function GetItem(Index: Integer): TSqlitePassTableDef;
  procedure SetItem(Index: Integer; const Value: TSqlitePassTableDef);
  public
  constructor Create(Owner: TSqlitePassDatabase);
  destructor Destroy; override;
  procedure Assign(Source: TSqlitePassTableDefs; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  procedure AddTableDefs(Source: TSqlitePassTableDefs; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  procedure CopyTable(Const TableName, NewTableName: String; TempTable: Boolean = False; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  procedure CreateTable(Const Sql: String);
  procedure DeleteTable(Const TableName: String; Silent: Boolean = False);
  procedure EmptyTable(Const TableName: String);
  procedure RenameAs(Const TableName, NewName: String);
  procedure Reindex(Const TableName: String);
  function TableByName(const Value: String): TSqlitePassTableDef;
  function FindTable(const Value: String): TSqlitePassTableDef;
  function FindFieldDef(Const TableName, FieldName: String): TSqlitePassTableFieldDef;
  function FindFieldTableName(Const FieldName: String; TableNames: TStringList): String;
  property Count;
  procedure Refresh;
  property Items[Index: Integer]: TSqlitePassTableDef read GetItem write SetItem; default;
  end;


{ TSqlitePassSelectStmtTableDefs :
  represents a collection of TSqlitePassSelectStmtTableDef.
  This collection is used to insert, update or delete records from a SQL select returning
  fields from several tables.
  }
TSqlitePassSelectStmtTableDefs = class(TSqlitePassObjectList)
  private
  FSQLStmt: TSqlitePassSelectStmt;
  Protected
  procedure ClearAndFreeItems;
  function GetItem(Index: Integer): TSqlitePassSelectStmtTableDef;
  procedure SetItem(Index: Integer; const Value: TSqlitePassSelectStmtTableDef);
  public
  constructor Create(Owner: TSqlitePassSelectStmt);
  destructor Destroy; override;
  function FindTable(const Value: String): TSqlitePassSelectStmtTableDef;
  property Count;
  property Items[Index: Integer]: TSqlitePassSelectStmtTableDef read GetItem write SetItem; default;
  end;


  { QueryDefs }

TSqlitePassQueryDef = class
  private
  { Owner }
  FQueryDefs: TSqlitePassQueryDefs;
  FAttachedDatabase: TSqlitePassDatabaseAttached;
  FQueryName: String;
  FSql: String;
  FFieldDefs: TFieldDefs;
  FParams: Tparams;
  function GetAttached: Boolean;
  function GetQueryFullName: String;
  public
  Constructor Create(Owner: TSqlitePassQueryDefs);
  Destructor Destroy; override;
  procedure Assign(Source: TSqlitePassQueryDef; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  property Attached: Boolean Read GetAttached;
  procedure DeleteQuery;
  procedure RenameAs(NewName: String);
  property QueryName: String Read FQueryName;
  property QueryFullName: String Read GetQueryFullName;
  property Sql: String Read FSql;
  property Params: TParams Read FParams;
  property Fields: TFieldDefs Read FFieldDefs;
  end;

TSqlitePassQueryDefs = class(TSqlitePassObjectList)
  private
  FDatabase: TSqlitePassDatabase;
  FAttachedDatabase: TSqlitePassDatabaseAttached;
  protected
  procedure ClearAndFreeItems;
  function GetItem(Index: Integer): TSqlitePassQueryDef;
  procedure SetItem(Index: Integer; const Value: TSqlitePassQueryDef);
  public
  constructor Create(Owner: TSqlitePassDatabase; AttachedDb: TSqlitePassDatabaseAttached = nil);
  destructor Destroy; override;
  procedure Assign(Source: TSqlitePassQueryDefs; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  procedure AddQueryDefs(Source: TSqlitePassQueryDefs; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  procedure CreateQuery(Sql: String);
  procedure DeleteQuery(QueryName: String);
  function QueryByName(const Value: String): TSqlitePassQueryDef;
  function FindQuery(const Value: String): TSqlitePassQueryDef;
  property Count;
  procedure Refresh;
  property Items[Index: Integer]: TSqlitePassQueryDef read GetItem write SetItem; default;
  end;


{ Database indexes management (managed by sqlite engine) }

TSqlitePassIndexDirection = (cidAscending, cidDescending, cidUnknown);

TSqlitePassIndexColumn = class
 public
 Position: Integer;
 CollatingSequence: String;
 ColumnName: String;
 Direction: TSqlitePassIndexDirection;
 procedure Assign(const Source: TSqlitePassIndexColumn);
 end;

TSqlitePassIndexColumns = class(TSqlitePassObjectList)
  private
  FIndex: TSqlitePassIndex;
  Protected
  procedure ClearAndFreeItems;
  function GetItem(Index: Integer): TSqlitePassIndexColumn;
  procedure SetItem(Index: Integer; const Value: TSqlitePassIndexColumn);
  public
  constructor Create(Owner: TSqlitePassIndex);
  destructor Destroy; override;
  procedure Assign(const Source: TSqlitePassIndexColumns);
  procedure AddColumns(Source: TSqlitePassIndexColumns);
  function ColumnByName(const Value: String): TSqlitePassIndexColumn;
  function FindColumn(const Value: String): TSqlitePassIndexColumn;
  property Items[Index: Integer]: TSqlitePassIndexColumn read GetItem write SetItem; default;
  property Count;
  end;

TSqlitePassIndex = class
 private
  FAttachedDatabase: TSqlitePassDatabaseAttached;
  FTableName: String;
  FIndexName: String;
  FOverwriteIfExists: Boolean;
  FColumns : TSqlitePassIndexColumns;
  FSql: String;
  FUnique : Boolean;
  function GetIndexFullName: String;
  function GetAttached: Boolean;
  procedure SetFIndexName(value: String);
 public
  constructor Create;
  Destructor Destroy; override;
  procedure Assign(Source: TSqlitePassIndex; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  property Attached: Boolean Read GetAttached;
  property Columns: TSqlitePassIndexColumns read FColumns write FColumns;
  property IndexName: String Read FIndexName Write SetFIndexName;
  property IndexFullName: String Read GetIndexFullName;
  property Sql: String Read FSql;
  property TableName: String Read FTableName Write FTableName;
  property Unique: Boolean read FUnique write FUnique;
 end;

TSqlitePassDatabaseIndex = class(TSqlitePassIndex)
 private
  FIndexDefs: TSqlitePassDatabaseIndexDefs;
  public
  constructor Create(Owner: TSqlitePassDatabaseIndexDefs);
  Destructor Destroy; override;
  procedure Reindex;
  procedure DeleteIndex;
  end;

TSqlitePassDatabaseIndexDefs = class(TSqlitePassObjectList)
  private
  FDatabase: TSqlitePassDatabase;
  Protected
  procedure ClearAndFreeItems;
  function GetItem(Index: Integer): TSqlitePassIndex;
  procedure SetItem(Index: Integer; const Value: TSqlitePassIndex);
  public
  constructor Create(Owner: TSqlitePassDatabase);
  destructor Destroy; override;
  procedure Assign(Source: TSqlitePassDatabaseIndexDefs; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  procedure AddIndexes(Source: TSqlitePassDatabaseIndexDefs; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  function IndexByName(const Value: String): TSqlitePassIndex;
  function FindIndex(const Value: String): TSqlitePassIndex;
  procedure CreateIndex(Sql: String); overload;
  procedure CreateIndex(Index: TSqlitePassDatasetIndex); overload;
  procedure DeleteIndex(IndexName: String);
  procedure Reindex;
  procedure Refresh;
  procedure RenameIndex(OldIndexName, NewIndexName: String);
  property Items[Index: Integer]: TSqlitePassIndex read GetItem write SetItem; default;
  property Count;
  end;


{ Views }

TSqlitePassView = class
 private
 FAttachedDatabase: TSqlitePassDatabaseAttached;
 FViews: TSqlitePassViews;
 FSqlCreateStmt: String;
 FSqlSelectStmt: String;
 FViewName: String;
 FTemp: Boolean;
 function GetAttached: Boolean;
 function GetViewFullName: String;
 public
 Constructor Create(Owner: TSqlitePassViews);
 Destructor Destroy; override;
 procedure Assign(Source: TSqlitePassView; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
 property Attached: Boolean Read GetAttached;
 procedure DeleteView;
 property SqlCreateStmt: String Read FSqlCreateStmt;
 property SqlSelectStmt: String Read FSqlSelectStmt;
 property ViewName: String Read FViewName;
 property ViewFullName: String Read GetViewFullName;
 end;


{ TSqlitePassViews }

TSqlitePassViews = class(TSqlitePassObjectList)
  private
  FDatabase: TSqlitePassDatabase;
  Protected
  procedure ClearAndFreeItems;
  function GetItem(Index: Integer): TSqlitePassView;
  procedure SetItem(Index: Integer; const Value: TSqlitePassView);
  public
  constructor Create(Owner: TSqlitePassDatabase);
  destructor Destroy; override;
  procedure Assign(Source: TSqlitePassViews; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  procedure AddViews(Source: TSqlitePassViews; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  procedure CreateView(Const ViewName, SQL: String; Const Temp: Boolean = False; Const Silent: Boolean = True);
  procedure DeleteView(ViewName: String);
  function ViewByName(const Value: String): TSqlitePassView;
  function FindView(const Value: String): TSqlitePassView;
  property Count;
  procedure Refresh;
  property Items[Index: Integer]: TSqlitePassView read GetItem write SetItem; default;
  end;


{ Triggers }

TSqlitePassTrigger = class
 private
  FAttachedDatabase: TSqlitePassDatabaseAttached;
  FTriggers: TSqlitePassTriggers;
  FSql: String;
  FTemporary: Boolean;
  FTriggerName: String;
  function GetAttached: Boolean;
  function GetTriggerFullName: String;
 public
  constructor Create(Owner: TSqlitePassTriggers);
  Destructor Destroy; override;
  procedure Assign(Source: TSqlitePassTrigger; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  procedure DeleteTrigger;
  property Attached: Boolean Read GetAttached;
  property Sql: String Read FSql;
  property Temporary: Boolean Read FTemporary Write FTemporary;
  property TriggerName: String Read FTriggerName Write FTriggerName;
  property TriggerFullName: String Read GetTriggerFullName;
 end;

TSqlitePassTriggers = class(TSqlitePassObjectList)
  private
  FDatabase: TSqlitePassDatabase;
  Protected
  procedure ClearAndFreeItems;
  Function GetItem(Index: Integer): TSqlitePassTrigger;
  Procedure SetItem(Index: Integer; Value: TSqlitePassTrigger);
  public
  constructor Create(Owner: TSqlitePassDatabase);
  destructor Destroy; override;
  procedure Assign(Source: TSqlitePassTriggers; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  procedure AddTriggers(Source: TSqlitePassTriggers; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  procedure CreateTrigger(Sql: String);
  procedure DeleteTrigger(TriggerName: String);
  function TriggerByName(const Value: String): TSqlitePassTrigger;
  function FindTrigger(const Value: String): TSqlitePassTrigger;
  property Items[Index: Integer]: TSqlitePassTrigger read GetItem write SetItem; default;
  property Count;
  procedure Refresh;
  end;


{TSqlitePassDatasets : Maintain a list of all datasets connected to the database}

TSqlitePassDatasets = Class(TSqlitePassObjectList)
  Private
    FDatabase: TSqlitePassDatabase;
  Protected
    Function GetItem(Index: Integer): TSqlitePassDataset;
    Procedure SetItem(Index: Integer; Dataset: TSqlitePassDataset);
  public
    constructor Create(Owner: TSqlitePassDatabase);
    destructor Destroy; override;
    function DatasetByName(Const Name: String): TSqlitePassDataset;
    function FindDataset(const Value: String): TSqlitePassDataset;
    property Items[Index: Integer]: TSqlitePassDataset read GetItem write SetItem; default;
    property Count;
    Procedure Close;
  end;

  TSqlitePassDatasetType = (dtTable, dtQuery, dtView, dtSQLSelect, dtSQLDirect, dtUnknown);
 { Forward declaration }
 TSqlitePassDatasetIndexDefs=class;

 TSqlitePassDatasetVersionInfo = Class(TPersistent)
     Private
       FPackage     : String;
       FComponent   : String;
       procedure SetFComponent(const Value: String);
       procedure SetFPackage(const Value: String);
     Published
      constructor Create;
      Property Component     : String  Read FComponent       Write SetFComponent;
      Property Package       : String  Read FPackage         Write SetFPackage;
     end;




{ TSqlitePassDataset }

{ Events }
TSqlitePassDatasetExportProgressEvent = procedure(Current, Total:Integer) of object;
TSqlitePassDatasetFilterRecordEvent   = procedure(DataSet: TDataSet; var Accept: Boolean) of object;
TSqlitePassDatasetImportProgressEvent = procedure(Current:Integer) of object;
//TSqlitePassDatasetGetFieldDataEvent = procedure(Field: TField; Const NativeFieldData: String; FieldDataBuffer: Pointer; var Handled: Boolean) of object;
//TSqlitePassDatasetSetFieldDataEvent = Procedure(Field: TField; Const FieldDataBuffer: Pointer; Out NativeFieldData: String; var Handled: Boolean) of object;


{The TSqlitePassDataset is a link between your application and the database content.
 TSqlitePassDataset enables you to access tables, queries, views or even to create direct SQL queries to read and write data from/to your database.
 It supports almost any kind of fields, including blobs, memo, datetime...etc, and has extended capabilities to quickly @link(TSqlitePassDataset.SortedBy sort),
  @link(TSqlitePassDataset.Filter filter) records (even on calculated or lookup fields).
  @link(TSqlitePassDataset.Locate Locate) and @Link(TSqlitePassDataset.Lookup Lookup) is also implemented with some additional methods.}
TSqlitePassDataset = class(TDataSet)
private
    { Memory Data Cache }
    FRecordset              : TSqlitePassRecordset;
    FParamCheck             : Boolean;
    FParams                 : TParams;
    FDatasetName            : String;
    FDatasetFullName        : String;
    FDatasetType            : TSqlitePassDatasetType;
    FDatabase               : TSqlitePassDatabase;
    { Locate }
    FLocateSmartRefresh     : Boolean; { if True, Locate found record list is automatically updated }
    FLocateFieldFilters       : TSqlitePassFieldFilters;
    { Calc Fields }
    FCalcFieldsList          : TList; { a sublist of Fields to hold Calcfields}
    FCalcDisplayedRecordsOnly: Boolean;
    { Lookup Fields }
    FLookupFieldsList            : TList; { a sublist of Fields to hold Lookupfields}
    FLookupFieldsKeyFieldFilters : TSqlitePassFieldFilters;
    FLookupFieldsDisplayedRecordsOnly: Boolean;
    { Lookup }
    FLookUpCache            : Boolean;
    FLookUpResultFields     : TList; { List of Result fields used to return the lookup result values }
    FLookUpSmartRefresh     : Boolean; { if True, Lookup recordset is automatically updated }
    FLookupKeyFieldFilters  : TSqlitePassFieldFilters;
    { Filtering }
    FFilterMode             : TSqlitePassFilterMode;
    FFilterChanges          : TSqlitePassFilterChanges;
    { Filtering Master - Detail }
    FMasterLink             : TMasterDataLink;
    FMasterFields           : String;
    FDetailFields           : TList;
    FMasterAutoActivate     : Boolean;
    { Filtering - SQL }
    FFilterSQLStmt          : String;
    FPreviousFilterSQLStmt  : String;
    { Filtering - Direct (in memory) }
    FFieldFilters           : TSqlitePassFieldFilters;
    FFilterDirectStmt       : String;
    FPreviousFilterDirectStmt: String;
    { Filtering - Range filter }
    FFilterRecordLowerLimit : Integer;
    FFilterRecordUpperLimit : Integer;
    { ----- }
    FRecordsCacheCapacity   : TSqlitePassRecordsCacheCapacity; { Number of records that can fit in each memory bloc alLookupd to store the fields data }
    FReadOnly               : Boolean;
    { SQL Stmt }
    FSQL                    : TStringList; { The original SQL statement }
    FSQLSelectStmt          : TSqlitePassSelectStmt; { Parsed SQL Statement to add filtering, sorting... capabilities }
    FSQLChanges             : TSqlitePassSQLChanges; { Set to indicate that FSQLSelectStmt needs to be updated }
    FRefetchRows            : Boolean; { Flag to indicate we have to refetch rows from database }
    { Sorting }
    FSorted                 : Boolean; { Flag to indicate if TSqlitePassDataset is sorted }
    FSortedBy               : String;  { OrderBy SQL Stmt }
    FSortedFields           : TSqlitePassInMemoryIndexInfos;   { A list of fields to be sorted }
    FSortMode               : TSqlitePassSortMode; { Flag to indicate if the sort uses SQL or QuickSort method. Calc and Lookup Fields can be sorted only when smInternal is selected}
    { Indexing - Internal index used with Locate Binary search }
    FInMemoryIndexed        : Boolean; { Flag to indicate if TSqlitePassDataset is indexed using InMemory indexes}
    FInMemoryIndexedBy      : String;  { IndexedBy Stmt }
    FInMemoryIndexes        : TSqlitePassInMemoryIndexes;   { A Collection of InMemory indexes }
    { ----- }
    FWriteMode              : TSqlitePassWriteMode;
    { ----- }
    FInInternalOpen         : Boolean; { Flag to indicate TSqlitePassDataset is executing InternalOpen }
    FDatabaseAutoActivate   : Boolean; { Flag to indicate whether or not TSqlitePassDataset will automatically open his associated Database }
    FVersionInfo            : TSqlitePassDatasetVersionInfo;
    FIndexDefs              : TSqlitePassDatasetIndexDefs;

    { Additional Events }
//    FOnExportProgress       : TSqlitePassDatasetExportProgressEvent;
//    FOnImportProgress       : TSqlitePassDatasetImportProgressEvent;
    FOnFilterRecord         : TSqlitePassDatasetFilterRecordEvent;

    { --- Functions and Procedures }
    Function  GetActiveRecord: Integer;
    Procedure UpdateFSQL(Sender: TObject);
    Procedure UpdateFSQLStmt;
//    procedure UpdateAutoIncValues;
    Procedure SetFDatasetType;
    procedure SetFSQL(const Value: TStringList);
    Function  GetFDatabase:TSqlitePassDatabase;
    Procedure SetFDatabase(Value:TSqlitePassDatabase);
    Procedure SetFDatasetName(const Value:String);
    Procedure SetFReadOnly(const Value:Boolean);
    Function  GetFReadOnly:Boolean;
    Procedure SetFFilterRecordLowerLimit(const Value: Integer);
    Procedure SetFFilterRecordUpperLimit(const Value: Integer);
    procedure SetFSortedBy(const Value: String);
    procedure SetFSorted(const Value: Boolean);
    procedure SetFInMemoryIndexedBy(const Value: String);
    procedure SetFInMemoryIndexed(const Value: Boolean);
    function  GetUniqueFieldName(FieldDef: TSqlitePassSelectStmtFieldDef): String;
    procedure SetFWriteMode(Const Value: TSqlitePassWriteMode);

    { ----- Opening ----- }
    function CheckCanOpen(VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow): Boolean;
    procedure MapFieldsToMemoryFieldBuffers;
     { ----- Sorting ----- }
    procedure GetSortedFields(Const SortedByStmt: String; Var SortedFieldsArray: TSqlitePassInMemoryIndexInfos);
    function GetValueComparator(Datatype: TFieldType; Const Ascending: Boolean): TSqlitePassCompareColumnValue;
    procedure SortRecords;
    procedure SetFSortMode(const Value: TSqlitePassSortMode);
    { ----- Calculated fields ----- }
    procedure SetFCalcDisplayedRecordsOnly(Const Value: Boolean);
    procedure FillCalcFields(Const RecordIndexStart, RecordIndexStop: Integer); { Stores the calculated fields value directly in the dataset to enable sorting on calc fields }
    { ----- Filtering ----- }
    procedure OnFieldFiltersChange(Sender: TObject);
    Procedure CheckRefreshFilteredRecords;
    { ----- Master Detail Filtering ----- }
    function  GetMasterDataSource: TDataSource;
    procedure SetMasterDataSource(Value: TDataSource);
    procedure MasterChanged(Sender: TObject);
    procedure MasterDisabled(Sender: TObject);
    function  GetMasterFields: String;
    procedure SetMasterFields(const Value: String);
    procedure ProcessMasterDetailFields;
    { ----- Locate ----- }
    function LocateRecords: Boolean;
    function GetLocateMoveState: TGetResult;
    { ----- LookupFields----- }
    procedure InitFieldLookupKeyFieldFilter(Const Field: TField);
    procedure SetFLookupFieldsDisplayedRecordsOnly(Const Value: Boolean);
    function GetLookupFieldBuffer(Const Field: TField; Const LookupFieldIndex: Integer): PRecBuffer;
    procedure FillLookupFields(Const RecordIndexStart, RecordIndexStop: Integer); { Stores the lookup fields value directly in the dataset to enable sorting on Lookup fields }
    { ----- Lookup ----- }
    function GetLookUpMoveState: TGetResult;
    function GetLookUpResultValues(Const LookupOk: Boolean): Variant;
    procedure LookUpRecords;
    Procedure ClearLookup;
    procedure RefreshLookup;
    procedure ActivateLookUpCache;
    procedure SetFLookUpCache(const Value: Boolean);
protected
    { ------ ------- }
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    { ----- Params ----- }
    Procedure SetParamsList(Value: TParams);
    Procedure UpdateParamsList;
    Procedure WriteParamData(Writer: TWriter);
    Function  GetParamsCount: Integer;
    Procedure DefineProperties(Filer: TFiler); override;
    Procedure ReadParamData(Reader: TReader);
    { ----- Override methods from TDataset ----- }
    Procedure Loaded; override;
    Procedure Notification(AComponent: TComponent; Operation: TOperation);Override;
    Function AllocRecordBuffer: PRecBuffer; override;
    Function GetRecord(Buffer: PRecBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override; //Ok v1.00
    Function GetRecordSize: Word; override;
    Function IsCursorOpen: Boolean; override;
    {$IFNDEF HasFmtBcd} { BCD fields support for D4 }
    Function BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean; override;
    Function CurrToBCD(const Curr: Currency; BCD: Pointer; Precision, Decimals: Integer): Boolean; override;
    {$ENDIF}
    { ----- ----- }
    Procedure ClearCalcFields(Buffer: PRecBuffer); override;
    Procedure FreeRecordBuffer(var Buffer: PRecBuffer); override;
    Procedure InternalOpen; override;
    Procedure InternalClose; override;
    Procedure InternalInitFieldDefs; override;
    Procedure InternalClearFieldDefs;
    Procedure InternalInitIndexDefs;
    Procedure InternalInitRecord(Buffer: PRecBuffer); override;
    Procedure InternalRefresh; override;
    Procedure UpdateInternalFieldsInfos;
    Procedure UpdateIndexDefs; override;
    Function  GetCanModify: Boolean; override;

    { Fetch data from physical Database }
    Procedure RefreshActive;

    { ----- Navigation and Editing ----- }
    Procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    Procedure InternalCancel; override;
    Procedure InternalDelete; override;
    Procedure InternalEdit; override;
    Procedure InternalFirst;override;
    procedure InternalInsert; override;
    Procedure InternalLast;override;
    Procedure InternalPost; override;
    Procedure InternalSetToRecord(Buffer: PRecBuffer); override;
    { ----- Bookmarks ----- }
    Procedure InternalGotoBookmark(ABookmark: Pointer); override;
    // Procedure InternalMoveToBookmark(Bookmark: Pointer);
    Function  GetBookmarkFlag(Buffer: PRecBuffer): TBookmarkFlag; override; {From TDataset}
    Procedure SetBookmarkFlag(Buffer: PRecBuffer; Value: TBookmarkFlag); override; {From TDataset}
    { Function  GetBookmarkStr: TBookmarkStr; override; -> Keep the inherited }
    { Procedure SetBookmarkStr(const Value: TBookmarkStr); override; -> Keep the inherited }
    {$IFNDEF Delphi2009}
    Procedure GetBookmarkData(Buffer: pAnsiChar; Data: Pointer); override;
    Procedure SetBookmarkData(Buffer: pAnsiChar; Data: Pointer); override;
    {$ENDIF}
    { ----- Filtering ----- }
    Procedure RefreshFilteredRecords;
    function  FindRecord(Restart, GoForward: Boolean): Boolean; override;
    procedure DoOnNewRecord; override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterOptions(Value: TFilterOptions); override;
    procedure SetFilterText(Const Value: String); override;
    procedure ProcessFilterText;
    { ----- Miscalleous ----- }
    Procedure InternalHandleException; override;
    { ----- Records infos ----- }
    Function GetRecordCount: Integer; override;
    Function GetRecNo: Integer; override;
    Procedure SetRecNo(Value: Integer); override;
    { ----- Blobs ----- }
    procedure AllocateBLOBPointers(Buffer: pAnsiChar);
    procedure FreeBlobPointers(Buffer: pAnsiChar);
    procedure FreeRecordPointers(Buffer: pAnsiChar);
  public

    { ----- Constructor - Destructor ----- }
    Constructor Create(AOwner: TComponent); override;
    Destructor  Destroy; override;
    { ----- Bookmarks ----- }
    {Call the BookmarkValide method to check if a given bookmark is still valid
     This function tests bookmarks for validity and indicate when a bookmark is valid by returning True.}
    Function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    {Compares two bookmark and returns
     @br
     @unorderedlist(
     @item -1 if Bookmark1 < Bookmark2
     @item  0 if Bookmark1 = Bookmark2
     @item  1 if Bookmark1 > Bookmark2)}
    Function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    { ----- Get or Set Field Data ----- }
    { GetFieldData Puts the data for field 'Field' from the active buffer into Buffer.
      This is called whenever a field value is demanded by a TField, so it must be
      efficient.
      the TDataset class doesn’t know anything about how our record buffer is organized.
      The next question then becomes that if the TDataset class doesn’t know about our
      record structure, how it does it pull out field values from this structure?
      How it does it put them back in when the user is editing a record?
      The answer is that it uses the GetFieldData and SetFieldData methods which a
      custom dataset class must override.
      These methods are called by the Tdataset.fields when it needs to retrieve
      or set a specific field value from the current record buffer.
      GetFieldData is prototyped as follows:

      function TDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;

      The Field parameter is the field for which the value needs to be retrieved
      We send back the needed value in the buffer.
      Returns False if a null value was retrieved }
    Function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    {seealso(GetFieldData)}
    Procedure SetFieldData(Field: TField; Buffer: Pointer);override;
    { -----  ----- }
    { Override the TDataset.GetFieldList method to accept quoted field names }
    procedure GetFieldList(List: TList; const FieldNames: string);
    { -----  ----- }
    Procedure Refresh;
    { ----- Apply or Cancel records modification if WriteMode is wmPostpone ----- }
    Procedure ApplyChanges;
    Procedure EmptyTable;

    { A list of In-Memory Indexes }
    Property Indexes: TSqlitePassInMemoryIndexes Read FInMemoryIndexes Write FInMemoryIndexes;

    { A list of internal filters applying to a record }
    Property Filters: TSqlitePassFieldFilters Read FFieldFilters Write FFieldFilters;

    { ----- Locate ----- }
    { The Locate function works as the classic one, with the same parameters, but has extended features
      to provide navigation in Located records with LocateFirst, LocateNext, LocatePrior, LocateLast, LocateRecordCount}
    function Locate(const KeyFields: String; const KeyValues: Variant; Options: TLocateOptions): Boolean; overload; override;

    function Locate(const LocateStmt: String; Options: TLocateOptions): Boolean; reintroduce; overload; 

    procedure RefreshLocate;

    Property LocateFilters: TSqlitePassFieldFilters read FLocateFieldFilters write FLocateFieldFilters;

    {Returns the state of the last move when locating a record}
    Property LocateMoveState: TGetResult read GetLocateMoveState;

    {Moves to the first record matching Locate parameters}
    function LocateFirst: Boolean;

    {Moves to the next record matching Locate parameters.}
    function LocateNext:  Boolean;

    {Moves to the prior record matching Locate parameters}
    function LocatePrior: Boolean;

    {Moves to the last record matching Locate parameters}
    function LocateLast:  Boolean;

    { ----- Lookup ----- }

    { Implementation of the classic TDataset.Lookup function }
    function Lookup(const KeyFields: String; const KeyValues: Variant; const ResultFields: String): Variant; override;

    { LookupEx function is a extension of classic lookup function in order to provide
      'multiple rows' lookup results. It has got two syntaxes :
      The first syntaxe works as quite the sames parameters as the TDataset.Lookup classic one,
      but has extended features to provide navigation in LookedUp records
      with LookupFirst, LookupNext, LookupPrior, LookupLast.
      Additional Parameters : LookUpResultMaxRecordCount is used to limit the number of records
      returned by the Lookup function in order to improve speed with large tables.
      Note : Strings case sensitive lookup is set by the lookupFilters.Options property
      }
    function LookupEx(const KeyFields: String; const KeyValues: Variant; const ResultFields: String;
                      const LookUpResultMaxRecordCount: Integer = 0): Variant; overload;

   { The second LookupEx function takes a complete Lookup Statement as first parameter. The others parameters
     remains the same }
    function LookupEx(const LookupStmt: String; const ResultFields: String;
                      const LookUpResultMaxRecordCount: integer = 0): Variant; overload;

    { Collection of lookup Filters }
    Property LookupFilters: TSqlitePassFieldFilters read FLookupKeyFieldFilters write FLookupKeyFieldFilters;

    {Returns the state of the last lookup move}
    Property LookupMoveState: TGetResult read GetLookupMoveState;

    {Moves to the first record matching lookup parameters}
    function LookupFirst(var LookupResult: Variant): Boolean;

    {Moves to the next record matching lookup parameters}
    function LookupNext(var LookupResult: Variant): Boolean;

    {Moves to the prior record matching lookup parameters}
    function LookupPrior(var LookupResult: Variant): Boolean;

    {Moves to the last record matching lookup parameters}
    function LookUpLast(var LookupResult: Variant): Boolean;

    { ----- Blobs ----- }

    { Classic TDataset behavior }
    Function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    { Classic TDataset behavior }
    procedure CloseBlob(Field: TField); override;

    { ----- Indexing ----- }

    { Indexes the dataset in memory, using the fields and index order defined in the IndexedBy property}
     Procedure Index;

    { ----- Sorting ----- }

    { Sorts the dataset using the fields and sort order defined in the SortedBy property}
    Procedure Sort;

    { -----  Properties ----- }

    { To be properly implemented }
    Property ParamCount: Integer read GetParamsCount;

    {This property is read only. It gives you information about the currently selected dataset and can be one of the following values :

     @bold(dtUnkown) :
     The dataset type could not be recognized or the DatasetName property is empty.

     @bold(dtTable) :
     The dataset is a table.

     @bold(dtQuery) :
     The dataset is a query.

     @bold(dtView) :
     The dataset is a view.

     @bold(dtSqlDirect) :
     The SQL property has been modified or you entered a new SQL query. When the SQL text is changed, the DatasetName will automatically be set to '' assuming that the DatasetName and SQL text don't match anymore.}
    Property DatasetType: TSqlitePassDatasetType Read FDatasetType;

    {Can take one of the following values :
     @br
     @unorderedlist(
     @item smDirect - Default
     @item smSQL)
     @seealso(SortedBy)}
    Property SortMode: TSqlitePassSortMode Read FSortMode Write SetFSortMode;

    {The internal schema of the current SQL statement. It gives you access to advanced information - read only}
    Property SQLSelectStmt: TSqlitePassSelectStmt Read FSQLSelectStmt;

  published
    { Enable or Disable the  persistent lookupfields }
    Property CalcDisplayedRecordsOnly: Boolean Read FCalcDisplayedRecordsOnly Write SetFCalcDisplayedRecordsOnly;

    {Selects the TsqlitePassDatabase component you want to depend on.}
    Property Database: TSqlitePassDatabase Read GetFDatabase Write SetFDatabase;

    {Classic table MasterFields behavior.

     At design time, a dialog will let you create or modify the relation between MasterFields and DetailFields.

     A relation is defined like this : MasterFieldName=DetailFieldName
     If you want set several relations, they must be separated by a ';'
     MasterFieldName1=DetailFieldName1;MasterFieldName2=DetailFieldName2}
    property MasterFields: String read GetMasterFields write SetMasterFields;

    {Classic table MasterSource behavior.}
    property MasterSource: TDataSource read GetMasterDataSource write SetMasterDataSource;

    { When set to TRUE, the dataset will try to automatically open the master dataource if necessary }
    property MasterSourceAutoActivate: Boolean read FMasterAutoActivate write FMasterAutoActivate;

    {Once you are connected to a database, enters a table name or a
     query name. At design time, a dialog will let you choose your dataset
     among all the available database datasets.}
    Property DatasetName: String Read FDatasetName Write SetFDatasetName;


    {Can take one of the following values :
     @br
     @unorderedlist(
     @item fmDirect
     @item fmSQL
     @item fmSQLDirect - Default)
     @seealso(Filter)}
     Property FilterMode: TSqlitePassFilterMode Read FFilterMode Write FFilterMode;

    {FilterRecordLowerLimit is the lower limit of the range filter.
     If greater than 1, the -nth first records will not be retrieved.
     In other words, if FilterLowerLimit = 4, the fifth record will be the first one retrieved from the query.}
    Property FilterRecordLowerLimit: Integer Read FFilterRecordLowerLimit Write SetFFilterRecordLowerLimit;

    {FilterRecordUpperLimit is the upper limit of the range filter.

    If greater than 1, the -nth first records will be retrieved.
    In other words, if FilterUpperLimit = 4 and FilterLowerLimit = 0 then
    only the four first records will be retrieved from the query.

    If lesser than 0, the -nth last records will be retrieved.
    In other words, if FilterUpperLimit = -9 and FilterLowerLimit = 0 then
    only the nine last records will be retrieved from the query.}
    Property FilterRecordUpperLimit: Integer Read FFilterRecordUpperLimit Write setFFilterRecordUpperLimit;


    {Determines whether or not the IndexedBy property is activated.}
    Property Indexed: Boolean Read FInMemoryIndexed Write SetFInMemoryIndexed;

    {The IndexedBy property lets you define an internal index used to speed up
     locate, lookup operations.
     When a proper index is set, the dataset is directly Indexed in memory, using quicksort.
     and a fast binary search routine is used to find the requested records.
     The IndexedBy property always takes a SQL 'ORDER BY' clause but without the
     'ORDER BY' expression at the begining
    @br
    @italic(Example : 'car_names ASC, car_types DESC'.)
    Field names with space must be quoted (see @link(TSqlitePassDatabaseOptions.QuoteStyle)).
    @br

    @br
    At design time, a dialog will let you create or modify the index.
    }
    Property IndexedBy: String Read FInMemoryIndexedBy Write SetFInMemoryIndexedBy;

    { When set to TRUE, Located records list is updated every time data is refetched from database.
      If FALSE, the located records list is not synchronized : Located records are lost when data is refetched}
    Property LocateSmartRefresh: Boolean Read FLocateSmartRefresh Write FLocateSmartRefresh;

    { Activate or disactivate the LookUpCache }
    Property LookUpCache: Boolean Read FLookUpCache Write SetFLookUpCache;

    { Enable or Disable the  persistent lookupfields. You should turn on thos option if you need to sort
      lookupfields}
    Property LookUpDisplayedRecordsOnly: Boolean Read FLookUpFieldsDisplayedRecordsOnly Write SetFLookUpFieldsDisplayedRecordsOnly;

    { Automatically Resfreshes the Lookup Results when dataset is changed or refetch from database }
    Property LookUpSmartRefresh: Boolean Read FLookUpSmartRefresh Write FLookUpSmartRefresh;

    {SQL represents the SQL statement used to retrieve data from the database.

    For tables, it will automatically be set to :

    @code(SELECT * FROM TableName;)
    if all fields need to be retrieved from the table, or to :

    @code(SELECT field1, field2... FROM TableName;)
    if only some fields need to be retrieved from the table.

    For queries, it will reflect the query SQL statement.
    You can also directly write your own SQL statement to fit your needs or to interact directly with the database.
    In this case, the datasetname propery will be set to '' (empty) and the datasetType will be set to dtDirectSql.}
    Property SQL: TStringList Read FSQL Write SetFSQL;

    {Determines whether or not the IndexedBy property is activated.}
    Property Sorted: Boolean Read FSorted Write SetFSorted;

    {The IndexedBy property always takes a SQL 'ORDER BY' clause but without the
     'ORDER BY' expression at the begining
    @br
    @italic(Example : 'car_names ASC, car_types DESC'.)
    Field names with space must be quoted (see @link(TSqlitePassDatabaseOptions.QuoteStyle)).
    @br
    It has two different sort modes :
    @br
    When the SortMode property is set to smDirect, (this is the default value)
    the dataset is directly Indexed in memory, using quicksort. This is fast and handy to sort dates, times, and calculated fields since
    you don't have to think about date or time format.
    @br
    When the SortMode property is set to smSQL,
    the dataset is Indexed using a SQL ORDER BY statement. Calculated fields cannot be Indexed this way.
    @br
    At design time, a dialog will let you create or modify the sort order.
    }
    Property SortedBy: String Read FSortedBy Write SetFSortedBy;

    {Set the number of records that can fit within one memory bloc before the need to alLookup a new records memory bloc.
    Using a larger RecordsCacheCapacity with large tables or queries should improve loading speed }
    Property RecordsCacheCapacity: TSqlitePassRecordsCacheCapacity Read FRecordsCacheCapacity Write FRecordsCacheCapacity;

    { Not used - TODO }
    Property Params: TParams read FParams Write SetParamsList Stored False;

    {The IndexDefs property gives you access to the indexes definitions for the selected table.
    Indexes are only available if the DatasetType is a 'dtTable' type.}
    Property IndexDefs: TSqlitePassDatasetIndexDefs  Read FIndexDefs Write FIndexDefs;

    { Classic Dataset behavior }
    Property ReadOnly: Boolean Read GetFReadOnly Write SetFReadOnly default False;

    {When set to True, opens automatically the database if needed.}
    Property DatabaseAutoActivate: Boolean Read FDatabaseAutoActivate  Write FDatabaseAutoActivate;

    {Returns the dataset component version number }
    Property VersionInfo: TSqlitePassDatasetVersionInfo Read FVersionInfo Write FVersionInfo;

    { Not used - TODO }
    Property ParamCheck: Boolean Read FParamCheck Write FParamCheck;

    { need to write doc... }
    Property WriteMode: TSqlitePassWriteMode Read FWriteMode Write SetFWriteMode;

    { Additionnal Events }
//    Property OnExportProgress: TSqlitePassDatasetExportProgressEvent Read FOnExportProgress Write FOnExportProgress;
//    Property OnImportProgress: TSqlitePassDatasetImportProgressEvent Read FOnImportProgress Write FOnImportProgress;
    Property OnFilterRecord  : TSqlitePassDatasetFilterRecordEvent read FOnFilterRecord write FOnFilterRecord;

    { --- }
    Property BeforeOpen;
    Property AfterOpen;
    Property BeforeClose;
    Property AfterClose;
    Property BeforeInsert;
    Property AfterInsert;
    Property BeforeEdit;
    Property AfterEdit;
    Property BeforePost;
    Property AfterPost;
    Property BeforeCancel;
    Property AfterCancel;
    Property BeforeDelete;
    Property AfterDelete;
    Property BeforeScroll;
    Property AfterScroll;
    Property OnCalcFields;
    Property OnDeleteError;
    Property OnEditError;
    Property OnNewRecord;
    Property OnPostError;
    { --- }
    { Classic dataset behavior }
    Property AutoCalcFields;

    {Classic dataset behavior.
     When set to True, opens the dataset and displays data if dataware components are linked to the datasource.
     When set to False, closes the dataset and frees the memory used to store dataset records.}
    Property Active;

    {This dataset allows many kinds of filtering features that will be applied in this priority to the retrieved data :
     @orderedlist(
     @item Original SQL statement using a WHERE or a LIMIT clause to filter records.
     @item Additional WHERE clause defined in the 1st part of the Filter property - when FilterMode in [fmSQL, fmSQLDirect].
     @item FilterRecordLowerLimit applies after the SQL statement is executed.
     @item FilterRecordUpperLimit applies after the SQL statement is executed.
     @item Additional filter defined in the 2nd part of the Filter property - when FilterMode is in [fmDirect, fmSQLDirect].
     @Item Master - Detail relationship.)

    As described, the filter statement itsef can be divided in two parts that are activated depending on the FilterMode property.
    [1st part = SQL syntax][;][2nd part = Direct syntax]

    The goal of the 2nd part is to provide filtering on @bold(calculated fields)

    @bold(The 1st part :)
    follows the classic dataset filter behavior and takes a SQL WHERE clause but without the WHERE word at the begining. You can also use wildcard characters as discribed in the Sqlite help.
       @italic(Example #1 : country = 'France')
       @italic(Example #2 : customer like '%cur%')
    @br
    @br
    The ';' semi-colon is used to separate the 1st and the 2nd part
    @br
    @br
    @bold(The 2nd part :)
    it uses a custom syntax close to SQL but limited
       For numeric fields :
           ComparisonOp : '=, <>, >, >=, <=, <, Null, NotNull'
           Value = numeric or empty (with Null and NotNull)
           Example 1 :
           MyDataset.Filter := MyField1 > 1 and < 10 or = 20; MyField7 <> 5; MyField3 Null;

           Example 2 :
           '=' operator has a special syntax supporting multiple values separated with comma
           MyDataset.Filter := MyField1 = 1, 10, 20, 30; MyField2 <> 5; MyField7 NotNull;

       For Boolean fields
           ComparisonOp : '='
           Value : True or False

           Example 1 :
           MyDataset.Filter := MyField1 = True; MyField2 = False;

       For Text fields (including Memo)
           ComparisonOp : '=, <>, >, >=, <=, <, Null, NotNull'
           '=' and '<>' accepts '%' or '*' wildcard char.
           You can use *YourText or %YourText* or YourText% as TextPattern

           Example 1 :
           MyDataset.Filter := MyField1 = %Pasc%; MyField3 NotNull;

       For Date, Time, DateTime fields
           ComparisonOp : '=, <>, >, >=, <=, <, Null, NotNull'
           Value = numeric or empty (with Null and NotNull)
           DateFormat : #DD-MM-YYYY#
           TimeFormat : #hh:mm:ss[.zzz]#
           DateTimeFormat : #DD-MM-YYYY hh:mm:ss[.zzz]#

           Note : '#' can be replaced by single or double quote

           Example 1 :
           MyDataset.Filter := MyField1 = #22-02-2008#; MyField3 NotNull; }
    Property Filter;

    {Determines whether or not the different filters are activated.
     The TsqlitePassDataset component can handle three filter levels
     that will be applied in this priority order :

     1 : MasterFields/DetailFields property
     2 : Filter property
     3 : RecordLowerLimit/RecordUpperLimit properties}
    Property Filtered;

    {foCaseInsensitive and foNoPartialCompare applies both to SQL and Internal Filters.
     You can also override the general setting, if you change the
     LocateFilters.Options, LookupFilters.Options,
     LookupFieldFilters.Options or Filters.Options properties}
    Property FilterOptions;
  End;

{ ----- Blob fields stream ----- }

TSqlitePassBlobStream = class(TMemoryStream)
  private
    FActiveRecIndex : Integer;
    FField          : TBlobField;
    FDataSet        : TSqlitePassDataset;
    FOpened         : Boolean;
    FModified       : Boolean;
    FRecBlobStream  : TMemoryStream;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    Function Read(var Buffer; Count: Longint): Longint; override;
    Function Write(const Buffer; Count: Longint): Longint; override;
  End;

 TSqlitePassDatasetIndexDefs = class(TSqlitePassObjectList)
  private
  FDataset: TSqlitePassDataset;
  Protected
  function GetItem(Index: Integer): TSqlitePassIndex;
  procedure SetItem(Index: Integer; const Value: TSqlitePassIndex);
  public
  constructor Create(Owner: TSqlitePassDataset);
  destructor Destroy; override;
  procedure RefreshFromCache;
  property Items[Index: Integer]: TSqlitePassIndex read GetItem write SetItem; default;
  property Count;
  end;

 TSqlitePassDatasetIndex = class(TSqlitePassIndex)
 private
  FIndexDefs: TSqlitePassDatasetIndexDefs;
  public
  constructor Create(Owner: TSqlitePassDatasetIndexDefs);
  destructor Destroy; override;
  procedure Reindex;
  procedure DeleteIndex;
  end;

 TSqlitePassTranslator = class
  Private
    { Objects Reference }
    FDatabase: TSqlitePassDatabase;
    FFieldTypesTranslationRules: TSqlitePassFieldTypesTranslationRules;

     { Conversion functions from a SQLite Db Value to it's internal MemRecord storage }
    procedure IntegerToWordBool(Const IntegerValue: Integer; out Value: WordBool);
    procedure BooleanTextToWordBool(Const StrValue: String; out Value: WordBool);
    procedure DateTextToInteger(Const StrValue: String;
                                Const YearStart, YearLength, MonthStart, MonthLength, DayStart, DayLength: Word;
                                out Value: Integer); overload;
    procedure DateTextToInteger(Const StrValue: String; out Value: Integer); overload;
    procedure TimeTextToInteger(Const StrValue: String;
                                Const HourStart, HourLength, MinStart, MinLength, SecStart, SecLength, MSecStart, MSecLength: Word;
                                out Value: Integer); overload;
    procedure TimeTextToInteger(Const StrValue: String; out Value: Integer); overload;
    procedure JulianDateTimeToDouble(Const DoubleValue: Double; out Value: Double);
    procedure DateTimeTextToDouble(Const StrValue: String; out Value: Double);
    procedure UnixDateTimeToDouble(Const Int64Value: Int64; out Value: Double);
    procedure MacDateTimeToDouble(Const Int64Value: Int64; out Value: Double);

     { Conversion functions from an internal MemRecord storage to a SQLite Db Value }
    procedure WordBoolToInteger(Const Value: WordBool; out IntegerValue: Integer);
    procedure WordBoolToBooleanText(Const Value: WordBool; out StrValue: String);
    procedure IntegerToDateText(Const Value: Integer;
                               Const DatePattern: String;
                               Const YearStart, YearLength, MonthStart, MonthLength, DayStart, DayLength: Word;
                               out StrValue: String); overload;
    procedure IntegerToDateText(Const Value: Integer; out StrValue: String); overload;
    procedure IntegerToTimeText(Const Value: Integer;
                               Const TimePattern: String;
                               Const HourStart, HourLength, MinStart, MinLength, SecStart, SecLength, MSecStart, MSecLength: Word;
                               out StrValue: String); overload;
    procedure IntegerToTimeText(Const Value: Integer; out StrValue: String); overload;
    procedure DoubleToJulianDateTime(Const Value: Double; out DoubleValue: Double);
    procedure DoubleToDateTimeText(Const Value: Double; out StrValue: String);
    procedure DoubleToUnixDateTime(Const Value: Double; out Int64Value: Int64);
    procedure DoubleToMacDateTime(Const Value: Double; out Int64Value: Int64);

    { Strings conversion functions }
    procedure SystemEncodingToUTF8(Const StrValue: String;  Out UTF8String: UTF8String);
    procedure UTF8ToSystemEncoding(Const UTF8String: String; Out StrValue: String);

    { Informations on databases parts }
    procedure GetDatabaseTableDefs(TableDefs: TSqlitePassTableDefs); virtual;
    procedure GetDatabaseTableFieldDefs(TableDef: TSqlitePassTableDef; FieldDefsRecordset: TSqlitePassRecordset); virtual;
    procedure GetDatabaseIndexDefs(IndexDefs: TSqlitePassDatabaseIndexDefs); virtual;
    procedure GetDatabaseQueryDefs(QueryDefs: TSqlitePassQueryDefs); virtual;
    procedure GetDatabaseViews(Views: TSqlitePassViews); virtual;
    procedure GetDatabaseTriggers(Triggers: TSqlitePassTriggers); virtual;
    function IsSystemTable(TableName: String): Boolean; virtual;

   { Field Datatype Conversion }
    procedure ConvertNativeFieldTypeToPascalFieldType(FieldDef: TSqlitePassTableFieldDef);
    procedure GetFieldDefSize(FieldDef: TSqlitePassTableFieldDef);

    { Properties }
    property FieldTypesTranslationRules: TSqlitePassFieldTypesTranslationRules Read FFieldTypesTranslationRules;

  public
    { --- Constructor / Destructor --- }
    Constructor Create(Owner: TSqlitePassDatabase);
    Destructor Destroy;override;
    { Returns a DateTime String based on TSqlitePassDatabaseDataTypeOptions.DateTimeFormat }
    function DateTimeToStr(Value: TDateTime): String;

    { Returns a Date String based on TSqlitePassDatabaseDataTypeOptions.DateFormat }
    function DateToStr(Value: TDateTime): String;

   { Returns a Time String based on TSqlitePassDatabaseDataTypeOptions.TimeFormat }
    function TimeToStr(Value: TDateTime): String;
  end;


  { Specific translator for Kexi databases }

  TSqlitePassTranslator_Kexi = class(TSqlitePassTranslator)
  public
   { Field Datatype Conversion }
    procedure ConvertNativeFieldTypeToPascalFieldType(FieldDef: TSqlitePassTableFieldDef);
    function GetNativeFieldTypeName(FieldType: Word): String;

    { Gets information on database parts }
    Procedure GetDatabaseTableFieldDefs(TableDef: TSqlitePassTableDef; FieldDefsRecordset: TSqlitePassRecordset); override;
    procedure GetDatabaseQueryDefs(QueryDefs: TSqlitePassQueryDefs); override;

    { Utilities }
    function IsSystemTable(TableName: String): Boolean; override;
  end;

  { Empty declaration : nothing to override }
  TSqlitePassTranslator_SqliteExpert = class(TSqlitePassTranslator)
  end;

  { Empty declaration : nothing to override }
  TSqlitePassTranslator_SqliteAdmin = class(TSqlitePassTranslator)

  end;

StmtType = (stCreate, stCreateTable, stSelect, stInsert, stUpdate, stDelete, stUnknown);
TSqlitePassSQLTokenizerOptions =  set of (toClean, toUpperCase);
TSqlitePassSQLTokenizerCleanOptions =  set of TSqlitePassSQLTokenType;

TSqlitePassSQLToken = class
 public
 {$IFDEF DEBUG_SQLStmt}
  Id: Integer;
 {$ENDIF}

 { the SQL statement }
 Text: UTF16WideString;
 { the token type }
 TokenType: TSqlitePassSQLTokenType;
 { NestingLevel }
 NestingLevel: Integer;
 function TokenTypeAsText: String;
 Constructor Create;
 end;

TSqlitePassSQLTokens = class(TSqlitePassObjectList)
  private
  function GetItem(Index: Integer): TSqlitePassSQLToken;
  procedure SetItem(Index: Integer; const Value: TSqlitePassSQLToken);
  procedure ClearAndFreeItems;
  public
  destructor Destroy; override;
  property Count;
  property Items[Index: Integer]: TSqlitePassSQLToken read GetItem write SetItem; default;
  end;

{The TSqlitePassSQLSections is an utility object used to :
 Split a SQL statement into a list of String for each SQL Keyword }
TSqlitePassSQLSections = class(TStringList)
  public
  function  GetSectionText(Const SectionName: String): String;
  procedure SetSectionText(Const SectionName, Text: String);
  procedure SetSectionTextFromTokenizer(Const SectionName: String; Tokenizer: TSqlitePassSQLTokenizer);
  procedure ReplaceSectionText(Const SectionName, NewStmt: String);
  { Replace a given SQLSection Text by the one in the SQLSections Parameter
    if no SectionName is set, all the Strings are copied }
  procedure ReplaceSectionTextFromSQLSections(SQLSections: TSqlitePassSQLSections; Const SectionName: String);
  procedure ClearSectionText(Const SectionName, NewStmt: String);
  procedure SplitSQLStmtIntoSections(Const SQLStmt: String; Const Keywords: Array of String);
  function  GetSQLStmtFromSections: String;
  end;

{The TSqlitePassTokenizer is an utility object used to :
 Split a SQL statement or a piece of SQL statement
 into a list of tokens (TSqlitePassSQLToken)

 Move to the First, Prior, Next, Last Token
 Lookup a specific token by text or type
 Replace a specific token Located before an existing token
 Remove a specific token Located after an existing token
 Insert a new token before an existing token
 Insert a new token after an existing token
 Set the String to be tokenized and rebuild a String representation from the tokens (Text property)...}
TSqlitePassSQLTokenizer = class
 private
 FText: UTF16WideString;
 { The current Token index }
 FCurrentTokenIndex: Integer;
 { Array to keep track of the last Char that changed the NestingLevel
   It is a simple stack that holds ['(','['... ]}
 FNestingCharStack: Array[0..255] of WideChar;
 { Used to fill the Token.NestingLevel Property when parsing }
 FNestingLevel: Integer;
 { A list of the tokens }
 FTokens: TSqlitePassSQLTokens;
 function GetFCount: Integer;
 function GetFCurrentToken: TSqlitePassSQLToken;
 function GetFEOF: Boolean;
 function GetFText: String;
 procedure SetFText(const Value: String);
 procedure Clean(Options: TSqlitePassSQLTokenizerCleanOptions);
 public
 constructor Create;
 destructor Destroy; override;
 function First: Boolean;
 function Prior: Boolean; overload;
 function Prior(TokenType: TSqlitePassSQLTokenType): Boolean; overload;
 function Next: Boolean; overload;
 function Next(TokenType: TSqlitePassSQLTokenType): Boolean; overload;
 function Last: Boolean;
 procedure Replace(OldText, NewText: String);
 function Locate(TokenText: String): Boolean; overload;
 function Locate(TokenType: TSqlitePassSQLTokenType): Boolean; overload;
 procedure InsertBefore(TokenText, InsertedStmt: String);
 procedure InsertAfter(TokenText, InsertedStmt: String);
 procedure RemoveBefore(TokenText: String);
 Procedure RemoveAfter(TokenText: String);
 procedure Remove(TokenText: String);
 procedure Tokenize(Options: TSqlitePassSQLTokenizerOptions = []);
 function GetTextAfter(TokenType: TSqlitePassSQLTokenType): String;
 function GetTextBefore(TokenType: TSqlitePassSQLTokenType): String;
 { Sets the original String to be tokenized or builds and returns a String from the tokens }
 property Text: String Read GetFText Write SetFText;
 { Gives access to the currently selected token }
 property Token: TSqlitePassSQLToken Read GetFCurrentToken;
 property TokenIndex: Integer Read FCurrentTokenIndex;
 property EOF: Boolean Read GetFEOF;
 property Count: Integer Read GetFCount;
 end;


TSqlitePassSQLStmtDef = class
 private
  FAttachedDatabase: TSqlitePassDatabaseAttached;
  FSQLStmtDefs: TSqlitePassSQLStmtDefs;
  { The SQL statement that can be modified, using the section spliter or the tokenizer }
  FSQLText: String;
  { the Statement name }
  FSQLStmtName: String;
  function GetAttached: Boolean;
  function GetSQLStmtFullName: String;
  { Returns the statement type }
  function  GetStmtType: StmtType;
 protected
 public
  constructor Create(SQLStmts: TSqlitePassSqlStmtDefs);
  destructor Destroy; override;
  procedure Assign(Source: TSqlitePassSQLStmtDef; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  procedure DeleteSQLStmt;
  { Properties }
  property Attached: Boolean Read GetAttached;
  property SQL: String Read FSQLText Write FSQLText;
  property SQLStmtName: String Read FSQLStmtName Write FSQLStmtName;
  property SQLStmtFullName: String Read GetSQLStmtFullName;
  property SQLType: StmtType Read GetStmtType;  
 end;

 { SQL statements collection }
 TSqlitePassSQLStmtDefs = class(TSqlitePassObjectList)
  private
  FDatabase: TSqlitePassDatabase;
  procedure ClearAndFreeItems;
  function GetItem(Index: Integer): TSqlitePassSQLStmtDef;
  procedure SetItem(Index: Integer; const Value: TSqlitePassSQLStmtDef);
  procedure UpdateSQLStmtTable(Const UpdateType: Integer; Const SQLStmtName: String; Const NewSQLStmtName: String; Const SQLStmt: TStringList);
  public
  constructor Create(Owner: TSqlitePassDatabase);
  destructor Destroy; override;
  procedure Assign(Source: TSqlitePassSQLStmtDefs; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  procedure AddSQLStmts(Source: TSqlitePassSQLStmtDefs; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
  procedure CreateSQLStmt(Const SQLStmtName: String; Const SQLStmt: TStringList);
  procedure DeleteSQLStmt(Const SQLStmtName: String);
  function FindSQLStmt(const Value: String): TSqlitePassSQLStmtDef;
  procedure RenameSQLStmt(Const OldName, NewName: String);
  function SQLStmtByName(const Value: String): TSqlitePassSQLStmtDef;
  property Count;
  procedure Refresh;
  property Items[Index: Integer]: TSqlitePassSQLStmtDef read GetItem write SetItem; default;
  end;

TSqlitePassSQLStmt = class
 private
  FDatabase: TSqlitePassDatabase;
  { The original SQL statement }
  FOriginalSQLText: String;
  { A TStringList used to split the original SQL Statement into Sections }
  FOriginalSQLSections: TSqlitePassSQLSections;
  { The SQL statement that can be modified, using the section spliter or the tokenizer }
  FSQLText: String;
  { A TStringList used to split the SQL Statement into Sections }
  FSQLSections: TSqlitePassSQLSections;
  { An Array of the SQL Keywords allowed in the SQL Statement
    used to split the statement in sections }
  FStmtKeywords: array of String;
  { A Tokenizer to split the statement in tokens }
  FTokenizer: TSqlitePassSQLTokenizer;
  { The operations that can be performed by the SQL Statement }
  FAvailableOperations: TSqlitePassAvailableOperations;
  { Statement Handle returned by sqlite engine }
  FStmtHandle: Pointer;
  { the Statement name }
  FStmtName: String;
  { Returns the statement type }
  function  GetStmtType: StmtType;
  function  GetFSQLText: String;
  procedure SetFSQLText(Value: String);
 protected
 public
  constructor Create(Database: TSqlitePassDatabase); overload;
  destructor Destroy; override;
  { }
  procedure Prepare; virtual;
  procedure UnPrepare;

  procedure QuoteString(Var S: String);
  procedure UnquoteString(Var S: String; All: Boolean = True);
  procedure RestoreOriginalStmt;
  { Properties }
  property AvailableOperations: TSqlitePassAvailableOperations Read FAvailableOperations;
  property StmtHandle: Pointer Read FStmtHandle;
  property StmtName: String Read FStmtName Write FStmtName;
  property SQLSections: TSqlitePassSQLSections Read FSQLSections Write FSQLSections;
  property SQL: String Read GetFSQLText Write SetFSQLText;
  property OriginalSQL: String Read FOriginalSQLText;
  property OriginalSQLSections: TSqlitePassSQLSections Read FOriginalSQLSections;
  property SQLType: StmtType Read GetStmtType;
  property Tokenizer: TSqlitePassSQLTokenizer Read FTokenizer Write FTokenizer;
 end;

 TSqlitePassSelectStmt = class(TSqlitePassSQLStmt)
 private
  { The dataset owner of the SQL Select statement }
  FDataset: TSqlitePassDataset;
  { An internal String used to add primary keys to the original SQL Select Statement }
  FPrimaryKeyStmt: String;
  { Number of updatable Tables }
  FPrimaryKeyCount: Integer;
  { A TableDefs collection representing the tables included in the SQL Select Statement }
  FTableDefs: TSqlitePassSelectStmtTableDefs;
  { A FieldDefs collection representing the fields included in the SQL Select Statement }
  FFieldDefs: TSqlitePassSelectStmtFieldDefs;
  { Internal Flag to indicate wether or not a 'given' field is requested several times in query }
//  FHasDuplicatedFields: Boolean;
 protected
  procedure AddPrimaryKeys;
  procedure FillTableDefsFromDatabaseSchema(Const TableName, FieldName: String; RecFieldIndex: Integer);
  procedure FillTableDefsFromRecordsetSchema(RecFieldIndex: Integer);
  procedure PrepareBindValueDispacher;
  procedure PrepareSQLStmts;
  procedure UnprepareSQLStmts;
 public
  constructor Create(Dataset: TSqlitePassDataset);
  destructor Destroy; override;
  { Buid an internal representation of the SQL Statement }
  procedure BuildSchema;
  { Prepare the statement and send it to the engine }
  procedure Prepare; override;
  { Release the prepared statement from the engine }
  procedure UnPrepare;
  procedure Post(Const PostType: TSqlitePassPostType; Const RecordBuffer: PRecBuffer);
  {}
  property FieldDefs: TSqlitePassSelectStmtFieldDefs Read FFieldDefs;
  property TableDefs: TSqlitePassSelectStmtTableDefs Read FTableDefs;
 end;


{ TSqlitePassFieldAccessor }

 TSqlitePassSqliteValueToBuffer = procedure(Const FRecordset: TSqlitePassRecordset;
                                  Const Buffer: PRecBuffer;
                                  Const PreparedStmt: Pointer;
                                  Const ColumnIndex: Integer);


 TSqlitePassDataReader = record
   BufferSize: Integer;
   Load: TSqlitePassSqliteValueToBuffer;
   end;

{ TSqlitePassRecordset }
{ Internal cache holding the records returned by a table/query
  It is designed to provide data to TSqlitePassDataset }
TSqlitePassRecordset = Class
 Private
  FDatabase: TSqlitePassDatabase;
  FDataset: TSqlitePassDataset;

  FDataReader: Array of TSqlitePassDataReader;

  { a list of pointers to the memory blocs used to store records values
  One Field  = [  FieldStatus (Byte to store NullValue flag, Modified flag)
                + FieldValue]

  One record = [  BookmarkFlag
                + BookMarkData
                + RecordState
                + [Rowid_1..Rowid_n] (One column per table involved in query)
                + [FieldsValues_1..FieldsValues_n] (Fields stored in db)
                + [Calc or Lookup FieldsValues_1..Calc or Lookup FieldsValues_n]}

  { List keeping track of Blocs of memory alLookupd to store data }
  FRecordsMemoryBlocs: TList;
  FRecordsUsedInMemoryBloc:Integer;
  FRecordMemoryBlocCapacity: Integer;

  { a list of pointers on records }
  FRecords: PRecBufferList;
  FRecordsCapacity: Integer;
  FRecordsCount: Integer;
  FRecycledRecords: TSqlitePassList;

  { a list of strings : Strings are not stored directly in the records, only the
    ItemIndex to access the string item }
  FAnsiStrings: TSqlitePassAnsiStringList;
  FRecycledAnsiStrings: TSqlitePassIntegerList;

  { a list of strings : WideStrings are not stored directly in the records, only the
    ItemIndex to access the Widestring item }
  FWideStrings: TSqlitePassWideStringList;
  FRecycledWideStrings: TSqlitePassIntegerList;

  { a TList of TMemoryStream (necessary to free blobs) }
  FMemoryStreams: TList;
  FRecycledMemoryStreams: TSqlitePassIntegerList;

  { --- Record Offsets --- }
  { Offset between record start and BookMarkData Value }
  FBookMarkDataOffset: Word;
  { Offset between record start and RecordState Value }
  FRecordStateOffset: Word;
  { Offset between record start and RowIds Values
   (one rowid per table returned by the sql statement)}
  FRowIdsOffset: Word;
  { Offset between record start and Fields Values }
  FFieldsValuesOffset: Word;

  { Total number of columns returned by the by the query }
  FColumnsCount: Integer;

  { Number of Data Fields (Number of columns - (n * TableCount) to substract the RowId column)
    returned by the query }
  FDataFieldsCount: Integer;

  { Number of Calc or Lookup Fields }
  FCalcFieldsCount: Cardinal;

  { Number of Fields (Number of columns - (n * TableCount) to substract the RowId column)
    returned by the query, including Calc and Lookup fields stored in record buffer }
  FFieldsCount: Cardinal;

  { Number of tables concerned by the sql statement
    The FRowidTablesCount is used to store the record rowid for each table}
  FRowidTablesCount: Integer;

  {Array of Offsets between record start and Fields Value buffer}
  FInternalFieldsValueOffset: Array of Integer;

  { Size necessary to holds the fields values, or pAnsiChars for strings..blobs,
    including Calculated - Lookup fields for one record }
  FRecordSize: Integer;

  { Boolean flag set to True when one of the records is inserted, edited, deleted  }
  FRecordsChanged: Boolean;
  FActive: Boolean;
  FActiveRecordOldValues: PRecBuffer;
  FOldActiveRecordIndex: Integer;

  FCurrentRecordIndex: Integer;
  MoveState: TGetResult;

  FDirectPost: Boolean;

  { --- Procedures / Functions --- }

  procedure PrepareDataReader(FDataset: TDataset);

  { --- Records --- }
  { Compute and get the size of a record }
  Function SetFRecordSize: Integer;
  { Set the records list capacity }
  procedure SetFRecordsCapacity(Const NewCapacity: Integer);

  function GetFieldSize(Const DataType: TFieldType): Integer;
  procedure GetRecords(Const Sql: String);
  procedure BuildAllRecordsList;
  Procedure ExchangeRecords(Const RecordIndex1, RecordIndex2: Integer);
  Procedure CopyRecord(Const Source, Dest: PRecBuffer);
  function GetActiveRecordIndex: Integer;

  { --- Read and Write Data in Record Buffer --- }

  { Read and Write a record BookMarkFlag }
  Function GetBookMarkFlag(Const RecordIndex: Integer): TBookMarkFlag;
  Procedure SetBookMarkFlag(Const RecordIndex: Integer; Const Value: TBookMarkFlag);

  { Read and Write a record BookMarkData }
  Function GetBookMarkData(Const RecordIndex: Integer): Cardinal;
  Procedure SetBookMarkData(Const RecordIndex: Integer; Const Value: Cardinal);

  { Read and Write a record state }
  Function GetRecordState(Const RecordBuffer: PRecBuffer; Const RecordStateType: TSqlitePassRecordStateType;
                          Const Value: TSqlitePassRecordStateValue):  Boolean; overload;
  Function GetRecordState(Const RecordIndex: Integer; Const RecordStateType: TSqlitePassRecordStateType;
                          Const Value: TSqlitePassRecordStateValue):  Boolean; overload;
  Procedure SetRecordState(Const RecordBuffer: PRecBuffer; Const RecordStateType: TSqlitePassRecordStateType;
                           Const Value: TSqlitePassRecordStateValue); overload;
  Procedure SetRecordState(Const RecordIndex: Integer; Const RecordStateType: TSqlitePassRecordStateType;
                           Const Value: TSqlitePassRecordStateValue); overload;
  Procedure ResetRecordState(Const RecordBuffer: PRecBuffer);

  Procedure DisableRemainingRecords;

  { Read and Write a record rowid }
  Function GetRowId(Const RecordBuffer: PRecBuffer; Const TableNo: Word): Int64; overload;
  Function GetRowId(Const RecordIndex: Integer; Const TableNo: Word): Int64; overload;
  Procedure SetRowId(Const RecordBuffer: PRecBuffer; Const TableNo: Word; Const Value: Int64); overload;
  Procedure SetRowId(Const RecordIndex: Integer; Const TableNo: Word; Const Value: Int64); overload;

  { Returns a pointer (PRecBuffer) to the field value inside a record }
  Function GetFieldValueBuffer(Const RecordIndex, FieldIndex: Integer): PRecBuffer; overload;
  Function GetFieldValueBuffer(Const RecordBuffer: PRecBuffer; Const FieldIndex: Integer): PRecBuffer; overload;

  { Returns the AnsiString value for FieldIndex }
  Function GetFieldAsAnsiString(Const FieldValueBuffer: PRecBuffer): AnsiString; overload;
  Function GetFieldAsAnsiString(Const RecordIndex, FieldIndex: Integer): AnsiString; overload;
  Procedure SetFieldAsAnsiString(Const FieldValueBuffer: PRecBuffer; Const Value: AnsiString); overload;
  Procedure SetFieldAsAnsiString(Const RecordIndex, FieldIndex: Integer; Const Value: AnsiString); overload;

  { Returns the Widestring value for FieldIndex }
  Function GetFieldAsWideString(Const FieldValueBuffer: PRecBuffer): UTF16WideString; overload;
  Function GetFieldAsWideString(Const RecordIndex, FieldIndex: Integer): UTF16WideString; overload;
  Procedure SetFieldAsWideString(Const FieldValueBuffer: PRecBuffer; Const Value: UTF16WideString); overload;
  Procedure SetFieldAsWideString(Const RecordIndex, FieldIndex: Integer; Const Value: UTF16WideString); overload;

  { Returns the Blob - MemoryStream value for FieldIndex }
  Function GetFieldAsBlob(Const RecordIndex, FieldIndex: Integer): TMemoryStream; overload;
  Function GetFieldAsBlob(Const RecordBuffer: PRecBuffer; Const FieldIndex: Integer): TMemoryStream; overload;
  Function GetFieldAsBlob(Const FieldBuffer: PRecBuffer): TMemoryStream; overload;

  { Returns Variant for FieldIndex }
  Function GetFieldAsVariant(Const RecordIndex, FieldIndex: Integer; Const Field: TField): Variant;

  { Read and Write a Field 'Null' Boolean Flag }
  Function FieldIsNull(Const RecordIndex, FieldIndex: Integer): Boolean; overload;
  Function FieldIsNull(Const RecordBuffer: PRecBuffer; Const FieldIndex: Integer): Boolean; overload;
  Function FieldIsNull(Const FieldValueBuffer: PRecBuffer): Boolean; overload;
  Procedure SetFieldNullValue(Const RecordIndex, FieldIndex: Integer; Const Value: Boolean); overload;
  Procedure SetFieldNullValue(Const FieldValueBuffer: PRecBuffer; Const Value: Boolean); overload;

  { Set all record fields to null }
  Procedure SetFieldsNullValue(Const RecordIndex: Integer);

  { Read and Write a record 'Modified' Boolean Flag }
  Function FieldWasModified(Const RecordIndex, FieldIndex: Integer): Boolean; overload;
  Function FieldWasModified(Const RecordBuffer: PRecBuffer; Const FieldIndex: Integer): Boolean; overload;
  Function FieldWasModified(Const FieldValueBuffer: PRecBuffer): Boolean; overload;
  Procedure SetFieldWasModifiedValue(Const RecordIndex, FieldIndex: Integer; Const Value: Boolean); overload;
  Procedure SetFieldWasModifiedValue(Const FieldValueBuffer: PRecBuffer; Const Value: Boolean); overload;

  { Reset all the fields states of a record to the default state, but keep the
   FieldIsNull flag to the current state }  
  Procedure ResetFieldsState(Const RecordIndex: Integer); overload;
  procedure ResetFieldsState(Const RecordBuffer: PRecBuffer); overload;

  { Used when no dataset is assigned - Set or Get a Generic String (Ansi or Unicode
    based on system encoding }
  Function GetTextItem(Const RecordIndex, FieldIndex: Integer): String;
  Procedure SetTextItem(Const RecordIndex, FieldIndex: Integer; Const Value: String);
  Procedure AddTextItem(Const RecordIndex, FieldIndex: Integer; Const Value: String);

  { Filtering }
  Procedure ShowAllRecords;
  Procedure ShowFilteredRecords;

  { Indexing (For Binary search and sorting) }
  Procedure BuildIndexFromRecordsList(InMemoryIndex: TSqlitePassInMemoryIndex; Const Ascending: Boolean);
  Procedure BuildRecordsListFromIndex(InMemoryIndex: TSqlitePassInMemoryIndex; Const Ascending: Boolean);

  { Sorting }
  Procedure SortRecords(var SortedFields: TSqlitePassInMemoryIndexInfos; RecordBufferList: PRecBufferList);

  { --- Managing Records --- }
  procedure _AddRecord;
  function AddRecord: Integer;
  function AddTempRecord: Integer;
  procedure ClearRecord(Index: Integer);
  Procedure InsertRecord(Index: Integer);
  Procedure DeleteRecord(Index: Integer);
  procedure PostRecord;
  procedure Cancel;

  function _AddMemoryStream(Const FieldValueBuffer: PRecBuffer): Integer; overload;
  function AddMemoryStream(Const FieldValueBuffer: PRecBuffer): Integer; overload;
  function AddMemoryStream(Const RecordIndex, FieldIndex: Integer): Integer; overload;
  procedure ClearMemoryStream(Const FieldValueBuffer: PRecBuffer); overload;
  procedure ClearMemoryStream(Const RecordIndex, FieldIndex: Integer); overload;

  Procedure _AddAnsiString(Const FieldValueBuffer: PRecBuffer; Const StrValue: AnsiString);
  procedure AddAnsiString(Const FieldValueBuffer: PRecBuffer; Const StrValue: AnsiString); overload;
  procedure AddAnsiString(Const RecordIndex, FieldIndex: Integer; Const StrValue: AnsiString); overload;
  procedure ClearAnsiString(Const FieldValueBuffer: PRecBuffer); overload;
  procedure ClearAnsiString(Const RecordIndex, FieldIndex: Integer); overload;

  procedure _AddWideString(Const FieldValueBuffer: PRecBuffer; Const StrValue: UTF16WideString); overload;
  procedure AddWideString(Const FieldValueBuffer: PRecBuffer; Const StrValue: UTF16WideString); overload;
  procedure AddWideString(Const RecordIndex, FieldIndex: Integer; Const StrValue: UTF16WideString); overload;
  procedure ClearWideString(Const FieldValueBuffer: PRecBuffer); overload;
  procedure ClearWideString(Const RecordIndex, FieldIndex: Integer); overload;

  { --- Active record old values storage / restore when editing --- }
  Procedure SaveActiveRecordOldValues;
  Procedure RestoreActiveRecordOldValues;
  Procedure ClearActiveRecordOldValues;

  { --- Opening / Closing --- }
  procedure Open(Const Sql: String; Const TablesCount: Word = 0);
  Procedure Close; overload;

  { Clear Memory }
  procedure ClearMemory;

  { --- Navigating --- }
  Procedure First;
  Procedure Prior;
  Procedure Next;
  Procedure Last;
  Procedure MoveBy(Offset: Integer);
  procedure GotoRecord(RecBufferBookmark: PRecBuffer);

  { --- Properties --- }
  Property Active: Boolean Read FActive;
  Property RecordsCount: Integer read FRecordsCount;

  { CurrentRecordIndex is zero based }
  Property ActiveRecordIndex: Integer Read GetActiveRecordIndex;
  Property CurrentRecordIndex: Integer Read FCurrentRecordIndex Write FCurrentRecordIndex;
  Property ItemsTextValue[Const RecordIndex, FieldIndex: Integer]: String read GetTextItem write SetTextItem;

  Public
  { --- Constructor / Destructor --- }
  constructor Create(Dataset: TSqlitePassDataset); overload;
  constructor Create(Database: TSqlitePassDatabase); overload;
  Destructor Destroy; override;
  end;


 TSqlitePassTransaction = class
 Private
  FLevel: Integer;
  FEngine: TSqlitePassEngine;
  FInternalTransaction: Boolean;
 Public
  Constructor Create(Owner: TSqlitePassEngine);
  Destructor Destroy; override;
  Procedure CommitInternalTransaction;
  Procedure RollbackInternalTransaction;
  Procedure StartInternalTransaction;
  Procedure ResetTransactions(CommitTransaction: Boolean);

  {Ends the transaction and write data to the database.}
  Procedure Commit;
  Procedure CommitAll;

  {Ends the transaction and discards any change made to the database.}
  Procedure Rollback;
  Procedure RollbackAll;

  {Starts a new transaction unless one is already started.}
  Procedure Start;
  Property  Level: Integer Read FLevel Write FLevel;
 end;

 TSqlitePassEngine = Class
  Private
    FDatabase: TSqlitePassDatabase;
    {$IFDEF DEBUG_SQLITEPASS}
    FPreparedStmts: TStringList;
    {$ENDIF}
    FConnectionHandle: Pointer;
    FElapsedTime: Double;
    FLibraryFile: String;
    FLibraryHandle: Integer;
    FLibraryLoaded: Boolean;
    FStartTime:Integer;
    FStopTime: Integer;
    FQueryTimeOut: Integer;
    FTransaction: TSqlitePassTransaction;
  Public
    Constructor Create(Database: TSqlitePassDatabase);
    Destructor Destroy; override;
    Property Connection: Pointer read FConnectionHandle;
    Property QueryTimeout: Integer Read FQueryTimeout Write FQueryTimeout;

    { Chrono utility }
    Procedure StartChrono;
    Procedure StopChrono;
    Property ElapsedTime: Double read FElapsedTime;

    { ------------ Library ----------------- }
    Function LoadSqliteLibrary(LibraryFile: String): Boolean;

    { ------------- Database ---------------- }
    procedure OpenDatabase(Const FullName: String; Const LibraryFile: String = DefaultSQLiteLibrary);
    procedure CloseDatabase;

    { ----------- Transactions ------------- }

    {Transactions.
    TODO :  Rework on nested named transactions.
    Sqlite engine supports only one active transaction at the same time.
    In other words, you can't use nested transactions.
    The TSqlitePassTransaction object will automatically handle this,
    so any attempt to start a transaction while one is running will have no effect.
    Transactions are really helpfull to speed up and secure data operations.
    You should use them as often as you can.
    If no transaction is active, the TSqlitePassDatabase will always try to start
    a new transaction before writing to the database and commit it.
    This could be time consuming if you need to update or add many records at the
    same time and you should proceed like this :
    @code(
    Database.Transaction.Start;
    ...
    Your code here to update or create records;
    ...
    Database.Transaction.Commit;)}
    Property Transaction: TSqlitePassTransaction Read FTransaction Write FTransaction;
    { ------ SQL Statements ------ }
    function PrepareStmt(var Stmt: pointer; const sql: String; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow): Boolean;
    function UnprepareStmt(var Stmt: pointer): Boolean;
    Procedure FinalizePendingStmts;
    { ------ SQL Exec ------ }
    function ExecSQL(Const Sql: String; CallBackFunc: TSqlitePassExecCallBack = nil): Integer;
    procedure ExecQueryUTF8(Const Sql: UTF8String; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow);
    procedure ExecQueryUTF16(Const Sql: UTF16WideString; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow);
    procedure ExecQuery(Const Sql: String; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow);
    function GetChangesCount: Integer;
    function GetTotalChangesCount: Integer;
    function GetLastInsertRowId: Int64;
    function CheckResult(Const i: Integer; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow): Integer;
    { ------ User Defined Functions ----- }
    function CreateFunction(FuncName: String; ArgCount: ShortInt; DefaultEncoding: Byte;
                            UserData: Pointer; xFunc, xStep: TFuncHandler; xFinal: TFuncFinalizer): Boolean;

   end;

{ *** Conversion functions from an internal MemRecord storage to a SQLite Db Value *** }
{ Translates a value from internal buffer to a sqlite value.
  Then binds this value to parameter in INSERT - UPDATE - DELETE SQL statement
  StmtFieldNo is 1 based, FieldIndex is Zero based }

procedure BindDummyToSqliteValue
          (Const Recordset: TSqlitePassRecordset;
           Const RecordBuffer: PRecBuffer;
           Const RecordFieldIndex: Integer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}


procedure BindWordBoolToSqliteValue
          (Const Recordset: TSqlitePassRecordset;
           Const RecordBuffer: PRecBuffer;
           Const RecordFieldIndex: Integer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure BindWordBoolToSqliteValueAsText
          (Const Recordset: TSqlitePassRecordset;
           Const RecordBuffer: PRecBuffer;
           Const RecordFieldIndex: Integer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure BindIntegerToSqliteValue
          (Const Recordset: TSqlitePassRecordset;
           Const RecordBuffer: PRecBuffer;
           Const RecordFieldIndex: Integer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure BindInt64ToSqliteValue
          (Const Recordset: TSqlitePassRecordset;
           Const RecordBuffer: PRecBuffer;
           Const RecordFieldIndex: Integer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure BindDoubleToSqliteValue
          (Const Recordset: TSqlitePassRecordset;
           Const RecordBuffer: PRecBuffer;
           Const RecordFieldIndex: Integer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure BindCurrencyToSqliteValue
          (Const Recordset: TSqlitePassRecordset;
           Const RecordBuffer: PRecBuffer;
           Const RecordFieldIndex: Integer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure BindDateToSqliteValueAsText
          (Const Recordset: TSqlitePassRecordset;
           Const RecordBuffer: PRecBuffer;
           Const RecordFieldIndex: Integer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure BindTimeToSqliteValueAsText
          (Const Recordset: TSqlitePassRecordset;
           Const RecordBuffer: PRecBuffer;
           Const RecordFieldIndex: Integer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure BindDateTimeToSqliteValueAsText
          (Const Recordset: TSqlitePassRecordset;
           Const RecordBuffer: PRecBuffer;
           Const RecordFieldIndex: Integer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure BindDateTimeToSqliteValueAsJulianDateTime
          (Const Recordset: TSqlitePassRecordset;
           Const RecordBuffer: PRecBuffer;
           Const RecordFieldIndex: Integer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure BindDateTimeToSqliteValueAsUnixDateTime
          (Const Recordset: TSqlitePassRecordset;
           Const RecordBuffer: PRecBuffer;
           Const RecordFieldIndex: Integer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure BindDateTimeToSqliteValueAsMacDateTime
          (Const Recordset: TSqlitePassRecordset;
           Const RecordBuffer: PRecBuffer;
           Const RecordFieldIndex: Integer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure BindAnsiStringToSqliteValueAsUTF8
          (Const Recordset: TSqlitePassRecordset;
           Const RecordBuffer: PRecBuffer;
           Const RecordFieldIndex: Integer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure BindUTF8StringToSqliteValueAsUTF8
          (Const Recordset: TSqlitePassRecordset;
           Const RecordBuffer: PRecBuffer;
           Const RecordFieldIndex: Integer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure BindUTF8StringToSqliteValueAsUTF16
          (Const Recordset: TSqlitePassRecordset;
           Const RecordBuffer: PRecBuffer;
           Const RecordFieldIndex: Integer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure BindWideStringToSqliteValueAsUTF16
          (Const Recordset: TSqlitePassRecordset;
           Const RecordBuffer: PRecBuffer;
           Const RecordFieldIndex: Integer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure BindBlobToSqliteValue
          (Const Recordset: TSqlitePassRecordset;
           Const RecordBuffer: PRecBuffer;
           Const RecordFieldIndex: Integer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}


{ Data Reader - Translate functions }

procedure SqliteValueToDummy
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure SqliteValueToWordBool
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure SqliteValueTextToWordBool
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure SqliteValueToInteger
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure SqliteValueToInt64
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure SqliteValueToDouble
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure SqliteValueToCurrency
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure SqliteValueTextToDate
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure SqliteValueTextToTime
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure SqliteValueDateTimeTextToDouble
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure SqliteValueJulianDateTimeToDouble
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure SqliteValueUnixDateTimeToDouble
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure SqliteValueMacDateTimeToDouble
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure SqliteValueToRawString
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure SqliteValueToAnsiString
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}


procedure SqliteValueToUTF8String
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure SqliteValueToRawWideString
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure SqliteValueToUTF16WideString
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}

procedure SqliteValueToBlob
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}

{ Filter functions }
function AcceptDummyValue(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}

function AcceptValueCmpNull(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptValueCmpNotNull(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}

function AcceptIntegerValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptIntegerValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptIntegerValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptIntegerValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptIntegerValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptIntegerValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}

function AcceptInt64ValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptInt64ValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptInt64ValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean;{$IFDEF HasInline} inline; {$ENDIF}
function AcceptInt64ValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptInt64ValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptInt64ValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}

function AcceptDoubleValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptDoubleValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptDoubleValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptDoubleValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptDoubleValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptDoubleValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}

function AcceptCurrencyValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptCurrencyValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptCurrencyValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptCurrencyValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptCurrencyValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptCurrencyValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}

function AcceptWordBoolValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptWordBoolValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}

{ Case Sensitive }
function AcceptAnsiStringCsValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptAnsiStringCsValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptAnsiStringCsValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptAnsiStringCsValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptAnsiStringCsValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptAnsiStringCsValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptAnsiStringCsValueCmpStartWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptAnsiStringCsValueCmpEndWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptAnsiStringCsValueCmpAnywhere(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}

{ Not Case Sensitive }
function AcceptAnsiStringValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptAnsiStringValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptAnsiStringValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptAnsiStringValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptAnsiStringValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptAnsiStringValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptAnsiStringValueCmpStartWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptAnsiStringValueCmpEndWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptAnsiStringValueCmpAnywhere(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}

{ Case Sensitive }
function AcceptWideStringCsValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptWideStringCsValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptWideStringCsValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptWideStringCsValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptWideStringCsValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptWideStringCsValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptWideStringCsValueCmpStartWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptWideStringCsValueCmpEndWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptWideStringCsValueCmpAnywhere(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}

{ Not Case Sensitive }
function AcceptWideStringValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptWideStringValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptWideStringValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptWideStringValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptWideStringValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptWideStringValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptWideStringValueCmpStartWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptWideStringValueCmpEndWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptWideStringValueCmpAnywhere(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}


{ Memo }
function GetMemoValue(Const Recordset: TSqlitePassRecordset; Const FieldValue: Pointer): AnsiString; {$IFDEF HasInline} inline; {$ENDIF}

{ Memo - Not Case Sensitive }

{ Case Sensitive }
function AcceptMemoCsValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptMemoCsValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptMemoCsValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptMemoCsValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptMemoCsValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptMemoCsValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptMemoCsValueCmpStartWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptMemoCsValueCmpEndWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptMemoCsValueCmpAnywhere(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}

{ Not Case Sensitive }
function AcceptMemoValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptMemoValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptMemoValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptMemoValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptMemoValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptMemoValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptMemoValueCmpStartWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptMemoValueCmpEndWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
function AcceptMemoValueCmpAnywhere(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}

//procedure InsertionSort(RecordBufferList: PRecBufferList; var MidRecordBuffers: TMidRecordBuffers; Const Lo, Hi: Integer);
function CompareRecord(Recordset: TSqlitePassRecordset; var MidRecordBuffers: TMidRecordBuffers; var SortedFields: TSqlitePassInMemoryIndexInfos; RecordBuffer: PRecBuffer): Integer;{$IFDEF HasInline} inline; {$ENDIF}
procedure InsertionSort(Recordset: TSqlitePassRecordset; RecordBufferList: PRecBufferList; var MidRecordBuffers: TMidRecordBuffers; var SortedFields: TSqlitePassInMemoryIndexInfos;Const Lo, Hi: Integer);{$IFDEF HasInline} inline; {$ENDIF}

{ Sort Comparison functions }
function CompareDummyValue(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}

{ Sort Comparison functions - Desc }
function CompareWordBoolValueDesc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
function CompareIntegerValueDesc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
function CompareInt64ValueDesc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
function CompareDoubleValueDesc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
function CompareCurrencyValueDesc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
function CompareAnsiStringValueDesc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
function CompareWideStringValueDesc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
function CompareMemoValueDesc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}

{ Sort Comparison functions - Asc
  We could call the above *Desc functions and multiplicate the result by -1, but the code is faster this way }
function CompareWordBoolValueAsc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
function CompareIntegerValueAsc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
function CompareInt64ValueAsc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
function CompareDoubleValueAsc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
function CompareCurrencyValueAsc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
function CompareAnsiStringValueAsc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
function CompareWideStringValueAsc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
function CompareMemoValueAsc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}

implementation
{$i SqlitePassEngine.inc}
{$i SqlitePassDatabase.inc}
{$i SqlitePassDatabaseParts.inc}
{$i SqlitePassDataset.inc}
{$i SqlitePassSqlStmts.inc}
{$i SqlitePassRecordset.inc}
end.
