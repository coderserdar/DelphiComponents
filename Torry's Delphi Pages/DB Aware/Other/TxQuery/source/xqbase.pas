{**************************************************************************}
{   TxQuery DataSet                                                        }
{                                                                          }
{   The contents of this file are subject to the Mozilla Public License    }
{   Version 1.1 (the "License"); you may not use this file except in       }
{   compliance with the License. You may obtain a copy of the License at   }
{   http://www.mozilla.org/MPL/                                            }
{                                                                          }
{   Software distributed under the License is distributed on an "AS IS"    }
{   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the}
{   License for the specific language governing rights and limitations     }
{   under the License.                                                     }
{                                                                          }
{   The Original Code is xqbase.pas                                        }
{                                                                          }
{   The Initial Developer of the Original Code is Alfonso Moreno.          }
{   Portions created by Alfonso Moreno are Copyright (C) Alfonso Moreno.   }
{   All Rights Reserved.                                                   }
{                                                                          }
{   Alfonso Moreno (Hermosillo, Sonora, Mexico)                            }
{   email: luisarvayo@yahoo.com                                            }
{     url: http://www.ezsoft.com                                           }
{          http://www.sigmap.com/txquery.htm                               }
{                                                                          }
{   Contributor(s): Chee-Yang, CHAU (Malaysia) <cychau@gmail.com>          }
{                   Sherlyn CHEW (Malaysia)                                }
{              url: http://code.google.com/p/txquery/                      }
{                   http://groups.google.com/group/txquery                 }
{                                                                          }
{**************************************************************************}

Unit xqbase;

{$I XQ_FLAG.INC}
Interface

Uses
  SysUtils, Windows, Classes, Dialogs, Db,
  xqmiscel, SparsArr, Qbaseexpr, QExprYacc
{$IFDEF WITHBDE}
  , DBTables, bde
{$ENDIF}
  ;

Const
  SQuote = ['''', '"'];
  NBoolean: Array[Boolean] Of String = ( 'FALSE', 'TRUE' );

Type
  {-------------------------------------------------------------------------------}
  {                          Main exception                                       }
  {-------------------------------------------------------------------------------}

  ExQueryError = Class( Exception );

  {-------------------------------------------------------------------------------}
  {                          Some base types needed                               }
  {-------------------------------------------------------------------------------}

  PFloat = ^Double;
  PInteger = ^Integer;
  PWordBool = ^WordBool;

  {-------------------------------------------------------------------------------}
  {                          Forward declarations                                 }
  {-------------------------------------------------------------------------------}
  TCreateFields = Class;
  TColumnList = Class;
  TTableList = Class;
  TOrderByList = Class;
  TUpdateList = Class;
  TWhereOptimizeList = Class;
  TCreateTableList = Class;
  TInsertList = Class;
  TSrtFields = Class;
  TxqSortList = Class;
  TAggregateList = Class;
  TMemMapFile = Class;

  {-------------------------------------------------------------------------------}
  {                          Declare enumerations                                 }
  {-------------------------------------------------------------------------------}

  TRelOperator = ( roNone, roAnd, roOr );

  TAggregateKind = ( akSUM,
                     akAVG,
                     akSTDEV,
                     akMIN,
                     akMAX,
                     akCOUNT );

  TRelationalOperator = ( ropBETWEEN,
                          ropGT,
                          ropGE,
                          ropLT,
                          ropLE,
                          ropNEQ );

  TSubQueryKind = ( skAny,
                    skAll );

  TSQLStatement = ( ssSelect,
                    ssUpdate,
                    ssDelete,
                    ssInsert,
                    ssUnion,
                    ssCreateTable,
                    ssAlterTable,
                    ssCreateIndex,
                    ssDropTable,
                    ssDropIndex,
                    ssPackTable,
                    ssZapTable,
                    ssReindexTable );

  {TMemorizeJoin = ( mjNone,
                    mjUsingMemory,
                    mjUsingFile );  }

  {-------------------------------------------------------------------------------}
  {                  Defines TAggregateItem                                       }
  {-------------------------------------------------------------------------------}

  TAggregateItem = Class
  Private
    FAggregateList: TAggregateList; { belongs to }
    FAggregateStr: String; { the expression as it is issued in the SQL statement }
    FColIndex: Integer; { the index in the ColumnList where this aggregate is temporary evaluated }
    FAggregate: TAggregateKind; { used if ColumnKind = ckAggregate                          }
    FIsDistinctAg: Boolean; { syntax is SELECT COUNT(distinct pricelist) FROM customer }
    FSparseList: TAggSparseList; { a sparse array for aggregates values in every record }
  Public
    Constructor Create( AggregateList: TAggregateList );
    Destructor Destroy; Override;
    Property AggregateStr: String Read FAggregateStr Write FAggregateStr;
    Property ColIndex: Integer Read FColIndex Write FColIndex;
    Property Aggregate: TAggregateKind Read FAggregate Write FAggregate;
    Property IsDistinctAg: Boolean Read FIsDistinctAg Write FIsDistinctAg;
    Property SparseList: TAggSparseList Read FSparseList Write FSparseList;
  End;

  {-------------------------------------------------------------------------------}
  {                  Defines TAggregateList                                       }
  {-------------------------------------------------------------------------------}

  TAggregateList = Class
  Private
    FItems: TList;
    Function GetCount: Integer;
    Function GetItem( Index: Integer ): TAggregateItem;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function Add: TAggregateItem;
    Procedure Clear;
    Procedure Delete( Index: Integer );
    Procedure Assign( AggregateList: TAggregateList );
    Property Count: Integer Read GetCount;
    Property Items[Index: Integer]: TAggregateItem Read GetItem; Default;
  End;

  {-------------------------------------------------------------------------------}
  {                  SELECT section data                                          }
  {-------------------------------------------------------------------------------}

  TColumnItem = Class
  Private
    FColumnList: TColumnList; { the column list that this column item belongs to }
    FColumnExpr: String; { the expression in this column }
    FAsAlias: String; { column Name (used later in where, group by, etc), default is FieldName   }
    { also is used as title in browse                                          }
    FIsAsExplicit: Boolean; { explicity defined in SQL (ex.: SELECT Sales+Bonus As TotalSales FROM...) }
    FResolver: TExprParser; { object used to evaluate ColumnExpr                                       }
    FAutoFree: Boolean; { Auto free FResolver class                                                }
    FIsTemporaryCol: Boolean; { Is a column used for some calculations (temporarily)                     }
    FCastType: Word; { column must be casted to this type (CAST expr AS DATE)                   }
    FCastLen: Word; { only used if casting to CHAR(n) n=CastLen                                }
    FAggregateList: TAggregateList; { the list of aggregates for this column: SUM(expression) / SUM(expression) }
    FSubQueryList: TList;
      { the list of subqueries for this column (SELECT AMOUNTPAID FROM custno WHERE custno=1000) / (SELECT AMOUNTPAID FROM custno WHERE custno=2000) }
  Public
    Constructor Create( ColumnList: TColumnList );
    Destructor Destroy; Override;

    Property ColumnExpr: String Read FColumnExpr Write FColumnExpr;
    Property AsAlias: String Read FAsAlias Write FAsAlias;
    Property IsAsExplicit: Boolean Read FIsAsExplicit Write FIsAsExplicit;
    Property IsTemporaryCol: Boolean Read FIsTemporaryCol Write FIsTemporaryCol;
    Property CastType: Word Read FCastType Write FCastType;
    Property CastLen: Word Read FCastLen Write FCastLen;
    Property Resolver: TExprParser Read FResolver Write FResolver;
    Property AutoFree: Boolean Read FAutoFree Write FAutoFree;
    Property AggregateList: TAggregateList Read FAggregateList Write FAggregateList;
    Property SubqueryList: TList Read FSubQueryList Write FSubQueryList;
  End;

  TColumnList = Class
  Private
    FItems: TList;
    Function GetCount: Integer;
    Function GetItem( Index: Integer ): TColumnItem;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function Add: TColumnItem;
    Procedure Clear;
    Procedure Delete( Index: Integer );
    Procedure DeleteAggregate( RecNo: Integer );
    Procedure SortAggregateWithList( SortList: TxqSortList );
    Property Count: Integer Read GetCount;
    Property Items[Index: Integer]: TColumnItem Read GetItem; Default;
  End;

  {-------------------------------------------------------------------------------}
  {                  FROM section data                                            }
  {-------------------------------------------------------------------------------}

  TTableItem = Class
  Private
    FTableList: TTableList;
    FTableName: String;
    FAlias: String;
    FDataSet: TDataSet; { the attached dataset }
    FIsFullPath: Boolean;
    { for using in syntax like: SELECT * FROM subquery1, a, subquery2, ... etc}
    FNumSubquery: Integer;
  Public
    Constructor Create( TableList: TTableList );

    Property TableName: String Read FTableName Write FTableName;
    Property Alias: String Read FAlias Write FAlias;
    Property DataSet: TDataSet Read FDataSet Write FDataSet;
    Property IsFullPath: Boolean Read FIsFullPath Write FIsFullPath;
    Property NumSubquery: Integer read FNumSubquery write FNumSubquery;
  End;

  TTableList = Class
    FItems: TList;
    Function GetCount: Integer;
    Function GetItem( Index: Integer ): TTableItem;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function Add: TTableItem;
    Procedure Clear;
    Procedure Delete( Index: Integer );
    Function IndexOFDataSet( DataSet: TDataSet ): Integer;
    Function IndexOFTableName( Const tableName: String ): Integer; // 1.56 fix
    Function IndexOFAlias( Const Alias: String ): Integer; // 1.56 fix

    Property Count: Integer Read GetCount;
    Property Items[Index: Integer]: TTableItem Read GetItem; Default;
  End;

  {-------------------------------------------------------------------------------}
  {   ORDER BY section data - used in ORDER BY and GROUP BY                       }
  {-------------------------------------------------------------------------------}

  TOrderByItem = Class
  Private
    FOrderByList: TOrderByList;
    FColIndex: Integer;
    FAlias: String; { field name used to order                                   }
    FDesc: Boolean; { Descending? default = false = Ascending;                   }
  Public
    Constructor Create( OrderByList: TOrderByList );
    Property ColIndex: Integer Read FColIndex Write FColIndex;
    Property Alias: String Read FAlias Write FAlias;
    Property Desc: Boolean Read FDesc Write FDesc;
  End;

  TOrderByList = Class
    FItems: TList;
    Function GetCount: Integer;
    Function GetItem( Index: Integer ): TOrderByItem;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function Add: TOrderByItem;
    Procedure Clear;
    Procedure Delete( Index: Integer );
    Property Count: Integer Read GetCount;
    Property Items[Index: Integer]: TOrderByItem Read GetItem; Default;
  End;

  {-------------------------------------------------------------------------------}
  {                  UPDATE statement section data                                }
  {-------------------------------------------------------------------------------}

  TUpdateItem = Class
  Private
    FUpdateList: TUpdateList;
    FColName: String;
    FColExpr: String;
    FResolver: TExprParser;
    FField: TField;
  Public
    Constructor Create( UpdateList: TUpdateList );
    Destructor Destroy; Override;

    Property ColName: String Read FColName Write FColName;
    Property ColExpr: String Read FColExpr Write FColExpr;
    Property Resolver: TExprParser Read FResolver Write FResolver;
    Property Field: TField Read FField Write FField;
  End;

  TUpdateList = Class
    FItems: TList;
    { the following can be one of the following supported syntax only:
      0 = RW_UPDATE table_identifier RW_SET list_update_columns where_clause end_statement
      1 = RW_UPDATE table_identifier RW_SET list_update_columns _EQ subquery end_statement }
    FSyntaxUsed: Integer;
    Function GetCount: Integer;
    Function GetItem( Index: Integer ): TUpdateItem;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function Add: TUpdateItem;
    Procedure Clear;
    Procedure Delete( Index: Integer );
    Property SyntaxUsed: Integer read FSyntaxUsed write FSyntaxUsed;
    Property Count: Integer Read GetCount;
    Property Items[Index: Integer]: TUpdateItem Read GetItem; Default;
  End;

  {-------------------------------------------------------------------------------}
  {                  WHERE optimization section data                              }
  {-------------------------------------------------------------------------------}

  TWhereOptimizeItem = Class
  Private
    FWhereOptimizeList: TWhereOptimizeList;
    FDataSet: TDataSet;
    FFieldNames: String;
    FRangeStart: String;
    FRangeEnd: String;
    FRelOperator: TRelationalOperator;
    FCanOptimize: Boolean; { Can optimize the result set generation with this config. }
  Public
    Constructor Create( WhereOptimizeList: TWhereOptimizeList );

    Property DataSet: TDataSet Read FDataSet Write FDataSet;
    Property FieldNames: String Read FFieldNames Write FFieldNames;
    Property RangeStart: String Read FRangeStart Write FRangeStart;
    Property RangeEnd: String Read FRangeEnd Write FRangeEnd;
    Property RelOperator: TRelationalOperator Read FRelOperator Write FRelOperator;
    Property CanOptimize: Boolean Read FCanOptimize Write FCanOptimize;
  End;

  TWhereOptimizeList = Class
    FItems: TList;
    Function GetCount: Integer;
    Function GetItem( Index: Integer ): TWhereOptimizeItem;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Assign( OptimizeList: TWhereOptimizeList );
    Function Add: TWhereOptimizeItem;
    Procedure Clear;
    Procedure Delete( Index: Integer );
    Property Count: Integer Read GetCount;
    Property Items[Index: Integer]: TWhereOptimizeItem Read GetItem; Default;
  End;

  {-------------------------------------------------------------------------------}
  {                  CREATE TABLE section data                                    }
  {-------------------------------------------------------------------------------}

  TCreateField = Class
  Private
    FCreateFields: TCreateFields;
    FFieldName: String;
    FFieldType: Integer;
    FScale: Integer;
    FPrecision: Integer;
    FSize: Integer;
    FBlobType: Integer;
    FMustDrop: Boolean; // used only in DROP TABLE
  Public
    Constructor Create( CreateFields: TCreateFields );
    Property FieldName: String Read FFieldName Write FFieldName;
    Property FieldType: Integer Read FFieldType Write FFieldType;
    Property Scale: Integer Read FScale Write FScale;
    Property Precision: Integer Read FPrecision Write FPrecision;
    Property Size: Integer Read FSize Write FSize;
    Property BlobType: Integer Read FBlobType Write FBlobType;
    Property MustDrop: Boolean Read FMustDrop Write FMustDrop; // used only in DROP TABLE
  End;

  TCreateFields = Class
  Private
    FList: TList;
    Function Get( Index: Integer ): TCreateField;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Clear;
    Procedure AddField( Const AName: String; AFieldType, AScale, APrecision,
      ASize, ABlobType: Integer; AMustDrop: Boolean );
    Function Count: Integer;
    Property Items[Index: Integer]: TCreateField Read Get; Default;
  End;

  TCreateTableItem = Class
  Private
    FCreateTableList: TCreateTableList;
    FFields: TCreateFields;
    FTableName: String;
    FPrimaryKey: TStringList;
  Public
    Constructor Create( CreateTableList: TCreateTableList );
    Destructor Destroy; Override;
    Function FieldCount: Integer;
    Property Fields: TCreateFields Read FFields;
    Property TableName: String Read FTableName Write FTableName;
    Property PrimaryKey: TStringList Read FPrimaryKey;
  End;

  TCreateTableList = Class
    FItems: TList;
    Function GetCount: Integer;
    Function GetItem( Index: Integer ): TCreateTableItem;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function Add: TCreateTableItem;
    Procedure Clear;
    Procedure Delete( Index: Integer );
    Property Count: Integer Read GetCount;
    Property Items[Index: Integer]: TCreateTableItem Read GetItem; Default;
  End;

  {-------------------------------------------------------------------------------}
  {                  INSERT INTO section data                                     }
  {-------------------------------------------------------------------------------}

  TInsertItem = Class
  Private
    FInsertList: TInsertList;
    FTableName: String;
    FIsFullPath: Boolean;
    FFieldNames: TStringList;
    FExprList: TStringList;
    FResolverList: TList;
    FDataSet: TDataSet;
  Public
    Constructor Create( InsertList: TInsertList );
    Destructor Destroy; Override;

    Property TableName: String Read FTableName Write FTableName;
    Property IsFullPath: Boolean Read FIsFullPath Write FIsFullPath;
    Property FieldNames: TStringList Read FFieldNames;
    Property ExprList: TStringList Read FExprList;
    Property DataSet: TDataSet Read FDataSet Write FDataSet;
    Property ResolverList: TList Read FResolverList;
  End;

  TInsertList = Class
    FItems: TList;
    Function GetCount: Integer;
    Function GetItem( Index: Integer ): TInsertItem;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function Add: TInsertItem;
    Procedure Clear;
    Procedure Delete( Index: Integer );
    Property Count: Integer Read GetCount;
    Property Items[Index: Integer]: TInsertItem Read GetItem; Default;
  End;

  {-------------------------------------------------------------------------------}
  {                  TSrtField to sort with variable type columns                 }
  {-------------------------------------------------------------------------------}

  TSrtField = Class( TObject )
  Private
    FFields: TSrtFields;
    FDataType: TExprType;
    FDataSize: Integer;
    FDesc: Boolean;
    FBufferOffset: Integer;
    Function GetData( Buffer: Pointer ): Boolean;
    Procedure SetData( Buffer: Pointer );
  Protected
    Function GetAsString: String; Virtual; Abstract;
    Procedure SetAsString( Const Value: String ); Virtual; Abstract;
    Function GetAsFloat: double; Virtual; Abstract;
    Procedure SetAsFloat( Value: double ); Virtual; Abstract;
    Function GetAsInteger: Longint; Virtual; Abstract;
    Procedure SetAsInteger( Value: Longint ); Virtual; Abstract;
    Function GetAsBoolean: Boolean; Virtual; Abstract;
    Procedure SetAsBoolean( Value: Boolean ); Virtual; Abstract;
    Procedure SetDataType( Value: TExprType );
  Public
    Constructor Create( Fields: TSrtFields ); Virtual;

    Property DataType: TExprType Read FDataType Write SetDataType;
    Property DataSize: Integer Read FDataSize Write FDataSize;
    Property Desc: Boolean Read FDesc Write FDesc;
    Property BufferOffset: Integer Read FBufferOffset Write FBufferOffset;

    Property AsString: String Read GetAsString Write SetAsString;
    Property AsFloat: Double Read GetAsFloat Write SetAsFloat;
    Property AsInteger: Longint Read GetAsInteger Write SetAsInteger;
    Property AsBoolean: Boolean Read GetAsBoolean Write SetAsBoolean;
  End;

  {-------------------------------------------------------------------------------}
  {                  TSrtStringField                                              }
  {-------------------------------------------------------------------------------}

  TSrtStringField = Class( TSrtField )
  Private
    Function GetValue( Var Value: String ): Boolean;
  Protected
    Function GetAsString: String; Override;
    Procedure SetAsString( Const Value: String ); Override;
    Function GetAsFloat: double; Override;
    Procedure SetAsFloat( Value: double ); Override;
    Function GetAsInteger: Longint; Override;
    Procedure SetAsInteger( Value: Longint ); Override;
    Function GetAsBoolean: Boolean; Override;
    Procedure SetAsBoolean( Value: Boolean ); Override;
  Public
    Constructor Create( Fields: TSrtFields ); Override;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TsrtFloatField                                        }
  {-------------------------------------------------------------------------------}

  TSrtFloatField = Class( TSrtField )
  Protected
    Function GetAsString: String; Override;
    Procedure SetAsString( Const Value: String ); Override;
    Function GetAsFloat: double; Override;
    Procedure SetAsFloat( Value: double ); Override;
    Function GetAsInteger: Longint; Override;
    Procedure SetAsInteger( Value: Longint ); Override;
    Function GetAsBoolean: Boolean; Override;
    Procedure SetAsBoolean( Value: Boolean ); Override;
  Public
    Constructor Create( Fields: TSrtFields ); Override;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TsrtIntegerField                                      }
  {-------------------------------------------------------------------------------}

  TSrtIntegerField = Class( TSrtField )
  Protected
    Function GetAsString: String; Override;
    Procedure SetAsString( Const Value: String ); Override;
    Function GetAsInteger: Longint; Override;
    Procedure SetAsInteger( Value: Longint ); Override;
    Function GetAsFloat: double; Override;
    Procedure SetAsFloat( Value: double ); Override;
    Function GetAsBoolean: Boolean; Override;
    Procedure SetAsBoolean( Value: Boolean ); Override;
  Public
    Constructor Create( Fields: TSrtFields ); Override;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TSrtBooleanField                                      }
  {-------------------------------------------------------------------------------}

  TSrtBooleanField = Class( TSrtField )
  Protected
    Function GetAsString: String; Override;
    Procedure SetAsString( Const Value: String ); Override;
    Function GetAsBoolean: Boolean; Override;
    Procedure SetAsBoolean( Value: Boolean ); Override;
    Function GetAsInteger: Longint; Override;
    Procedure SetAsInteger( Value: Longint ); Override;
    Function GetAsFloat: double; Override;
    Procedure SetAsFloat( Value: double ); Override;
  Public
    Constructor Create( Fields: TSrtFields ); Override;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TSrtFields                                            }
  {-------------------------------------------------------------------------------}

  TSrtFields = Class
    FSortList: TxqSortList;
    FItems: TList;
    Function GetCount: Integer;
    Function GetItem( Index: Integer ): TSrtField;
  Public
    Constructor Create( SortList: TxqSortList );
    Destructor Destroy; Override;
    Function Add( DataType: TExprType ): TSrtField;
    Procedure Clear;

    Property Count: Integer Read GetCount;
    Property Items[Index: Integer]: TSrtField Read GetItem; Default;
    Property SortList: TxqSortList Read FSortList;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TxqSortList                                           }
  {-------------------------------------------------------------------------------}
  TxqSortList = Class( TObject )
  Private
    FFields: TSrtFields;
    FRecNo: Integer;
    FRecordBufferSize: Integer;
    FUsingBookmark: Boolean;
    FBookmarkedDataset: TDataset;
    FSelected: TList;
    FBofCrack: Boolean;
    FEofCrack: Boolean;
    FFilterRecno: Integer;

    function ActiveBuffer: PAnsiChar; virtual; abstract;
    Function DoCompare( N: Integer; Const KeyValue: Variant ): Integer;
    Function Find( Const KeyValue: Variant; Var Index: Integer ): Boolean;
    procedure SetBookmarkedDataset(const Value: TDataset);
  Protected
    Function GetFieldData( Field: TSrtField; Buffer: Pointer ): Boolean; Virtual; Abstract;
    Procedure SetFieldData( Field: TSrtField; Buffer: Pointer ); Virtual; Abstract;
    Procedure SetRecno( Value: Integer );
    Function GetRecno: Integer;
    Procedure SetSourceRecno( Value: Integer ); Virtual; Abstract;
    Function GetSourceRecno: Integer; Virtual; Abstract;
    Function GetRecordCount: Integer; Virtual; Abstract;
    {$if RtlVersion >= 20}
    function GetSourceBookmark: TBookmark; virtual; abstract; { patched by ccy }
    procedure SetSourceBookmark(const Value: TBookmark); virtual; abstract; { patched by ccy }
    {$ifend}
  Public
    Constructor Create( UsingBookmark: Boolean );
    Destructor Destroy; Override;
    Procedure AddField( pDataType: TExprType; pDataSize: Integer;
      pDescending: Boolean );
    Procedure Insert; Virtual; Abstract;
    Procedure Sort;
    Procedure Exchange( Recno1, Recno2: Integer ); Virtual; Abstract;
    Procedure Clear; Virtual;
    Function IsEqual( Recno1, Recno2: Integer ): Boolean;
    Procedure Filter( Const KeyValue: Variant );
    Procedure First;
    Procedure Next;
    Function Eof: Boolean;
    Function Bof: Boolean;

    Property Count: Integer Read GetRecordCount;
    Property Recno: Integer Read GetRecno Write SetRecno;
    Property FilterRecno: Integer Read FFilterRecno Write FFilterRecno;
    Property SourceRecno: Integer Read GetSourceRecno Write SetSourceRecno;
    {$if RtlVersion >= 20}
    property SourceBookmark: TBookmark read GetSourceBookmark write
        SetSourceBookmark; { patched by ccy }
    {$ifend}
    Property Fields: TSrtFields Read FFields;
    property BookmarkedDataset: TDataset read FBookmarkedDataset write
        SetBookmarkedDataset; { patched by ccy }
    Property UsingBookmark: Boolean read FUsingBookmark write FUsingBookmark;
  End;

  TMemSortList = Class( TxqSortList )
  Private
    FBufferList: TList;
    function ActiveBuffer: PAnsiChar; override;
  Protected
    Function GetFieldData( Field: TSrtField; Buffer: Pointer ): Boolean; Override;
    Procedure SetFieldData( Field: TSrtField; Buffer: Pointer ); Override;
    Function GetRecordCount: Integer; Override;
    Procedure SetSourceRecno( Value: Integer ); Override;
    Function GetSourceRecno: Integer; Override;
    {$if RtlVersion >= 20}
    function GetSourceBookmark: TBookmark; override;
    procedure SetSourceBookmark(const Value: TBookmark); override;
    {$ifend}
  Public
    Constructor Create( UsingBookmark: Boolean );
    Destructor Destroy; Override;
    Procedure Insert; Override;
    Procedure Exchange( Recno1, Recno2: Integer ); Override;
    Procedure Clear; Override;
  End;

  TFileSortList = Class( TxqSortList )
  Private
    FBufferList: TList;
    FMemMapFile: TMemMapFile;
    FTmpFile: String;
    FBuffer: PAnsiChar;
    function ActiveBuffer: PAnsiChar; override;
  Protected
    Function GetFieldData( Field: TSrtField; Buffer: Pointer ): Boolean; Override;
    Procedure SetFieldData( Field: TSrtField; Buffer: Pointer ); Override;
    Function GetRecordCount: Integer; Override;
    Procedure SetSourceRecno( Value: Integer ); Override;
    Function GetSourceRecno: Integer; Override;
    {$if RtlVersion >= 20}
    function GetSourceBookmark: TBookmark; override;
    procedure SetSourceBookmark(const Value: TBookmark); override;
    {$ifend}
  Public
    Constructor Create( UsingBookmark: Boolean; MapFileSize: Longint );
    Destructor Destroy; Override;
    Procedure Insert; Override;
    Procedure Exchange( Recno1, Recno2: Integer ); Override;
    Procedure Clear; Override;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TUserDefinedRange                                     }
  {-------------------------------------------------------------------------------}

  { this class is used for handling this kind of syntax:
    SELECT * FROM MOVES SET RANGE FROM 1000 TO 3000 USING INDEX "CUSTNO_INDEX" ;

    and is implemented only in certain situations where it is possible to
    optimize by end-user .
  }

  TUserDefinedRange = Class
  Private
    FForFields: TStrings;
    FStartValues: TStrings;
    FEndValues: TStrings;
    FUsingIndex: String;
    FStartResolvers: TList;
    FEndResolvers: TList;
    Procedure ClearResolvers;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Property ForFields: TStrings Read FForFields;
    Property StartValues: TStrings Read FStartValues;
    Property EndValues: TStrings Read FEndValues;
    Property UsingIndex: String Read FUsingIndex Write FUsingIndex;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TMemMapFile                                           }
  {-------------------------------------------------------------------------------}

  TMemMapFile = Class( TObject )
  Private
    FFileName: String;
    FSize: Longint;
    FFileSize: Longint;
    FFileMode: Integer;
    FFileHandle: Integer;
    FMapHandle: Integer;
    FData: PChar;
    FMapNow: Boolean;
    FPosition: Longint;
    FVirtualSize: Longint;

    Procedure AllocFileHandle;
    Procedure AllocFileMapping;
    Procedure AllocFileView;
    Function GetSize: Longint;
  Public
    Constructor Create( FileName: String; FileMode: integer;
      Size: integer; MapNow: Boolean ); Virtual;
    Destructor Destroy; Override;
    Procedure FreeMapping;
    Procedure Read( Var Buffer; Count: Longint );
    Procedure Write( Const Buffer; Count: Longint );
    Procedure Seek( Offset: Longint; Origin: Word );

    Property Data: PChar Read FData;
    Property Size: Longint Read GetSize;
    Property VirtualSize: Longint Read FVirtualSize;
    Property Position: Longint Read FPosition;
    Property FileName: String Read FFileName;
    Property FileHandle: Integer Read FFileHandle;
    Property MapHandle: Integer Read FMapHandle;
  End;

  {---------------------------------------------------------------------------}
  {                  Define TParamsAsFieldsItem                               }
  {---------------------------------------------------------------------------}

  TParamsAsFieldsItem = Class( TCollectionItem )
  Private
    FName: string;
    FValue: string;
    Procedure SetName( const Value: string );
    Procedure SetValue( Const Value: String );
  Protected
    Function GetDisplayName: String; Override;
  Public
    Procedure Assign( Source: TPersistent ); Override;
  Published
    Property Name: String Read FName Write SetName;
    Property Value: String Read FValue Write SetValue;
  End;

  {----------------------------------------------------------------------------}
  {                  Define TParamsAsFields                                    }
  {----------------------------------------------------------------------------}

  TParamsAsFields = Class( TOwnedCollection )
  Private
    Function GetItem( Index: Integer ): TParamsAsFieldsItem;
    Procedure SetItem( Index: Integer; Value: TParamsAsFieldsItem );
  Public
    Constructor Create( AOwner: TPersistent );
    Function Add: TParamsAsFieldsItem;
    function ParamByName(const Value: string): TParamsAsFieldsItem;
    Property Items[Index: Integer]: TParamsAsFieldsItem Read GetItem Write SetItem; Default;
  End;

  { TIntegerList class }

  TxqIntegerList = Class
  Private
    FList: TList;
    Function Get( Index: Integer ): Integer;
    Procedure Put( Index: Integer; Value: Integer );
    Function GetCapacity: Integer;
    Procedure SetCapacity( Value: Integer );
    Function GetCount: Integer;
    Procedure SetCount( Value: Integer );
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Assign( AList: TxqIntegerList );
    Function Add( Item: Integer ): Integer;
    Procedure Clear;
    Procedure Delete( Index: Integer );
    Procedure Insert( Index: Integer; Value: Integer );
    Function IndexofValue( Item: Integer ): Integer;
    Procedure Sort;
    Function Find(Value: Integer; var Index: Integer): Boolean;
    procedure LoadFromStream( Stream: TStream );
    Procedure SaveToStream( Stream: TStream );
    Procedure LoadFromFile( const FileName: string );
    Procedure SaveToFile( const FileName: string );
    Procedure Reindex;

    Property Items[Index: Integer]: Integer Read Get Write Put; Default;
    Property Capacity: Integer Read GetCapacity Write SetCapacity;
    Property Count: Integer Read GetCount Write SetCount;
  End;


Implementation

Uses
  xquery, xqconsts
{$IFDEF LEVEL6}
  , Variants
{$ENDIF}
  ;

{-------------------------------------------------------------------------------}
{                  Implement TColumnItem                                        }
{-------------------------------------------------------------------------------}

Constructor TColumnItem.Create( ColumnList: TColumnList );
Begin
  Inherited Create;
  FColumnList := ColumnList;
  FAggregateList := TAggregateList.Create;
  FSubQueryList := TList.Create;
  FAutoFree := True;
End;

Destructor TColumnItem.Destroy;
Var
  i: Integer;
Begin
  FAggregateList.Free;
  If FAutoFree And Assigned( FResolver ) Then
    FResolver.Free;
  For i := 0 To FSubQueryList.Count - 1 Do
    TSqlAnalizer( FSubQueryList[i] ).Free;
  FSubQueryList.Free;
  Inherited Destroy;
End;

{-------------------------------------------------------------------------------}
{                  Implement TColumnList                                        }
{-------------------------------------------------------------------------------}

Constructor TColumnList.Create;
Begin
  Inherited Create;
  FItems := TList.Create;
End;

Destructor TColumnList.Destroy;
Begin
  Clear;
  FItems.Free;
  Inherited Destroy;
End;

Function TColumnList.GetCount;
Begin
  Result := FItems.Count;
End;

Function TColumnList.GetItem( Index: Integer ): TColumnItem;
Begin
  Result := FItems[Index];
End;

Function TColumnList.Add: TColumnItem;
Begin
  Result := TColumnItem.Create( Self );
  FItems.Add( Result );
End;

Procedure TColumnList.Clear;
Var
  I: Integer;
Begin
  For I := 0 To FItems.Count - 1 Do
    TColumnItem( FItems[I] ).Free;
  FItems.Clear;
End;

Procedure TColumnList.Delete( Index: Integer );
Begin
  TColumnItem( FItems[Index] ).Free;
  FItems.Delete( Index );
End;

Procedure TColumnList.DeleteAggregate( RecNo: Integer );
Var
  I, J: Integer;
Begin
  For I := 0 To FItems.Count - 1 Do
    With TColumnItem( FItems[I] ) Do
      For J := 0 To AggregateList.Count - 1 Do
        AggregateList[J].SparseList.Delete( RecNo );
End;

Procedure TColumnList.SortAggregateWithList( SortList: TxqSortList );
Var
  I, J, K,
    Index: Integer;
  SparseList: TAggSparseList;
Begin
  For I := 0 To FItems.Count - 1 Do
    With TColumnItem( FItems[I] ) Do
      For J := 0 To AggregateList.Count - 1 Do
      Begin
        { if this columns contains aggregate functions, the value for every
          record is saved on TColumnItem(FItems[I]).AggregateList[J].SparseList.Values[Index]
          where J is the No. of aggregate (several aggregates accepted on every column)
          and Index is the number of record on the result set}
        SparseList := TAggSparseList.Create( 1000 );
        For K := 1 To SortList.Count Do
        Begin
          SortList.Recno := K;
          Index := SortList.SourceRecno;
          If AggregateList[J].SparseList.HasData(Index) Then
          Begin
            SparseList.Values[K] := AggregateList[J].SparseList.Values[Index];
            SparseList.Count[K] := AggregateList[J].SparseList.Count[Index];
          End;
        End;
        AggregateList[J].SparseList.Free;
        AggregateList[J].SparseList := SparseList;
      End;
End;

{-------------------------------------------------------------------------------}
{                  Implement TTableItem                                         }
{-------------------------------------------------------------------------------}

Constructor TTableItem.Create( TableList: TTableList );
Begin
  Inherited Create;
  FTableList := TableList;
  FNumSubquery:= -1;  // means no subquery defined for this
End;

{-------------------------------------------------------------------------------}
{                  Implement TTableList                                         }
{-------------------------------------------------------------------------------}

Constructor TTableList.Create;
Begin
  Inherited Create;
  FItems := TList.Create;
End;

Destructor TTableList.Destroy;
Begin
  Clear;
  FItems.Free;
  Inherited Destroy;
End;

Function TTableList.GetCount;
Begin
  Result := FItems.Count;
End;

Function TTableList.GetItem( Index: Integer ): TTableItem;
Begin
  Result := FItems[Index];
End;

Function TTableList.Add: TTableItem;
Begin
  Result := TTableItem.Create( Self );
  FItems.Add( Result );
End;

Procedure TTableList.Clear;
Var
  I: Integer;
Begin
  For I := 0 To FItems.Count - 1 Do
    TTableItem( FItems[I] ).Free;
  FItems.Clear;
End;

Procedure TTableList.Delete( Index: Integer );
Begin
  TTableItem( FItems[Index] ).Free;
  FItems.Delete( Index );
End;

Function TTableList.IndexOFDataSet( DataSet: TDataSet ): Integer;
Var
  Idx: Integer;
Begin
  Result := -1;
  For Idx := 0 To FItems.Count - 1 Do
    If TTableItem( FItems[Idx] ).DataSet = DataSet Then
    Begin
      Result := Idx;
      Exit;
    End;
End;

Function TTableList.IndexOFTableName( Const tableName: String ): Integer; // 1.56 fix
Var
  Idx: Integer;
Begin
  Result := -1;
  For Idx := 0 To FItems.Count - 1 Do
    If AnsiCompareText( TTableItem( FItems[Idx] ).TableName, TableName ) = 0 Then
    Begin
      Result := Idx;
      Exit;
    End;
End;

Function TTableList.IndexOFAlias( Const Alias: String ): Integer; // 1.56 fix
Var
  Idx: Integer;
Begin
  Result := -1;
  For Idx := 0 To FItems.Count - 1 Do
    If AnsiCompareText( TTableItem( FItems[Idx] ).Alias, Alias ) = 0 Then
    Begin
      Result := Idx;
      Exit;
    End;
End;

{-------------------------------------------------------------------------------}
{                  Implement TOrderByItem                                       }
{-------------------------------------------------------------------------------}

Constructor TOrderByItem.Create( OrderByList: TOrderByList );
Begin
  Inherited Create;
  FOrderByList := OrderByList;
End;

{-------------------------------------------------------------------------------}
{                  Implement TOrderByList                                       }
{-------------------------------------------------------------------------------}

Constructor TOrderByList.Create;
Begin
  Inherited Create;
  FItems := TList.Create;
End;

Destructor TOrderByList.Destroy;
Begin
  Clear;
  FItems.Free;
  Inherited Destroy;
End;

Function TOrderByList.GetCount;
Begin
  Result := FItems.Count;
End;

Function TOrderByList.GetItem( Index: Integer ): TOrderByItem;
Begin
  Result := FItems[Index];
End;

Function TOrderByList.Add: TOrderByItem;
Begin
  Result := TOrderByItem.Create( Self );
  FItems.Add( Result );
End;

Procedure TOrderByList.Clear;
Var
  I: Integer;
Begin
  For I := 0 To FItems.Count - 1 Do
    TOrderByItem( FItems[I] ).Free;
  FItems.Clear;
End;

Procedure TOrderByList.Delete( Index: Integer );
Begin
  TOrderByItem( FItems[Index] ).Free;
  FItems.Delete( Index );
End;

{-------------------------------------------------------------------------------}
{                  Implement TUpdateItem                                        }
{-------------------------------------------------------------------------------}

Constructor TUpdateItem.Create( UpdateList: TUpdateList );
Begin
  Inherited Create;
  FUpdateList := UpdateList;
End;

Destructor TUpdateItem.Destroy;
Begin
  If Assigned( FResolver ) Then
    FResolver.Free;
  Inherited Destroy;
End;

{-------------------------------------------------------------------------------}
{                  Implement TUpdateList                                        }
{-------------------------------------------------------------------------------}

Function TUpdateList.GetCount;
Begin
  Result := FItems.Count;
End;

Function TUpdateList.GetItem( Index: Integer ): TUpdateItem;
Begin
  Result := FItems[Index];
End;

Constructor TUpdateList.Create;
Begin
  Inherited Create;
  FItems := TList.Create;
End;

Destructor TUpdateList.Destroy;
Begin
  Clear;
  FItems.Free;
  Inherited Destroy;
End;

Function TUpdateList.Add: TUpdateItem;
Begin
  Result := TUpdateItem.Create( Self );
  FItems.Add( Result );
End;

Procedure TUpdateList.Clear;
Var
  I: Integer;
Begin
  For I := 0 To FItems.Count - 1 Do
    TUpdateItem( FItems[I] ).Free;
  FItems.Clear;
End;

Procedure TUpdateList.Delete( Index: Integer );
Begin
  TUpdateItem( FItems[Index] ).Free;
  FItems.Delete( Index );
End;

{-------------------------------------------------------------------------------}
{                  Implement TWhereOptimizeItem                                 }
{-------------------------------------------------------------------------------}

Constructor TWhereOptimizeItem.Create( WhereOptimizeList: TWhereOptimizeList );
Begin
  Inherited Create;
  FWhereOptimizeList := WhereOptimizeList;
End;

{-------------------------------------------------------------------------------}
{                  Implement TWhereOptimizeList                                 }
{-------------------------------------------------------------------------------}

Function TWhereOptimizeList.GetCount;
Begin
  Result := FItems.Count;
End;

Function TWhereOptimizeList.GetItem( Index: Integer ): TWhereOptimizeItem;
Begin
  Result := FItems[Index];
End;

Constructor TWhereOptimizeList.Create;
Begin
  Inherited Create;
  FItems := TList.Create;
End;

Destructor TWhereOptimizeList.Destroy;
Begin
  Clear;
  FItems.Free;
  Inherited Destroy;
End;

Procedure TWhereOptimizeList.Assign( OptimizeList: TWhereOptimizeList );
Var
  Item: TWhereOptimizeItem;
  I: Integer;
Begin
  Clear;
  For I := 0 To OptimizeList.Count - 1 Do
  Begin
    Item := Self.Add;
    With OptimizeList[I] Do
    Begin
      Item.DataSet := Dataset;
      Item.FieldNames := Fieldnames;
      Item.RangeStart := Rangestart;
      Item.Rangeend := Rangeend;
      Item.RelOperator := Reloperator;
      Item.CanOptimize := Canoptimize;
    End;
  End;
End;

Function TWhereOptimizeList.Add: TWhereOptimizeItem;
Begin
  Result := TWhereOptimizeItem.Create( Self );
  FItems.Add( Result );
End;

Procedure TWhereOptimizeList.Clear;
Var
  I: Integer;
Begin
  For I := 0 To FItems.Count - 1 Do
    TWhereOptimizeItem( FItems[I] ).Free;
  FItems.Clear;
End;

Procedure TWhereOptimizeList.Delete( Index: Integer );
Begin
  TWhereOptimizeItem( FItems[Index] ).Free;
  FItems.Delete( Index );
End;

{-------------------------------------------------------------------------------}
{                  Implement TCreateField                                       }
{-------------------------------------------------------------------------------}

Constructor TCreateField.Create( CreateFields: TCreateFields );
Begin
  Inherited Create;
  FCreateFields := CreateFields;
End;

{-------------------------------------------------------------------------------}
{                  Implement TCreateFields                                      }
{-------------------------------------------------------------------------------}

Constructor TCreateFields.Create;
Begin
  Inherited Create;
  FList := TList.Create;
End;

Destructor TCreateFields.Destroy;
Begin
  Clear;
  FList.Free;
  Inherited;
End;

Procedure TCreateFields.AddField( Const AName: String;
  AFieldType,
  AScale,
  APrecision,
  ASize,
  ABlobType: Integer;
  AMustDrop: Boolean );
Var
  NewField: TCreateField;
Begin
  NewField := TCreateField.Create( Self );
  With NewField Do
  Begin
    FieldName := AName;
    FieldType := AFieldType;
    Scale := AScale;
    Precision := APrecision;
    Size := ASize;
    BlobType := ABlobType;
    MustDrop := AMustDrop;
  End;
  FList.Add( NewField );
End;

Procedure TCreateFields.Clear;
Var
  I: Integer;
Begin
  For I := 0 To FList.Count - 1 Do
    TCreateField( FList[I] ).Free;
  FList.Clear;
End;

Function TCreateFields.Get( Index: Integer ): TCreateField;
Begin
  Result := Nil;
  If ( Index < 0 ) Or ( Index > FList.Count - 1 ) Then
    exit;
  Result := TCreateField( FList[Index] );
End;

Function TCreateFields.Count: Integer;
Begin
  Result := FList.Count;
End;

{-------------------------------------------------------------------------------}
{                  Implement TCreateTableItem                                   }
{-------------------------------------------------------------------------------}

Constructor TCreateTableItem.Create( CreateTableList: TCreateTableList );
Begin
  Inherited Create;
  FCreateTableList := CreateTableList;
  FFields := TCreateFields.Create;
  FPrimaryKey := TStringList.Create;
End;

Destructor TCreateTableItem.Destroy;
Begin
  FFields.Free;
  FPrimaryKey.Free;
  Inherited Destroy;
End;

Function TCreateTableItem.FieldCount: Integer;
Begin
  Result := FFields.Count;
End;

{-------------------------------------------------------------------------------}
{                  Implement TCreateTableList                                   }
{-------------------------------------------------------------------------------}

Constructor TCreateTableList.Create;
Begin
  Inherited Create;
  FItems := TList.Create;
End;

Destructor TCreateTableList.Destroy;
Begin
  Clear;
  FItems.Free;
  Inherited Destroy;
End;

Function TCreateTableList.GetCount;
Begin
  Result := FItems.Count;
End;

Function TCreateTableList.GetItem( Index: Integer ): TCreateTableItem;
Begin
  Result := FItems[Index];
End;

Function TCreateTableList.Add: TCreateTableItem;
Begin
  Result := TCreateTableItem.Create( Self );
  FItems.Add( Result );
End;

Procedure TCreateTableList.Clear;
Var
  I: Integer;
Begin
  For I := 0 To FItems.Count - 1 Do
    TCreateTableItem( FItems[I] ).Free;
  FItems.Clear;
End;

Procedure TCreateTableList.Delete( Index: Integer );
Begin
  TCreateTableItem( FItems[Index] ).Free;
  FItems.Delete( Index );
End;

{-------------------------------------------------------------------------------}
{                  Implement TInsertItem                                        }
{-------------------------------------------------------------------------------}

Constructor TInsertItem.Create( InsertList: TInsertList );
Begin
  Inherited Create;
  FInsertList := InsertList;
  FFieldNames := TStringList.Create;
  FExprList := TStringList.Create;
  FResolverList := TList.Create;
End;

Destructor TInsertItem.Destroy;
Var
  I: Integer;
  Resolver: TExprParser;
Begin
  FFieldNames.Free;
  FExprList.Free;
  For I := 0 To FResolverList.Count - 1 Do
  Begin
    Resolver := TExprParser( FResolverList[I] );
    If Assigned( Resolver ) Then
      Resolver.Free;
  End;
  FResolverList.Free;
  Inherited Destroy;
End;

{-------------------------------------------------------------------------------}
{                  Implement TInsertList                                        }
{-------------------------------------------------------------------------------}

Constructor TInsertList.Create;
Begin
  Inherited Create;
  FItems := TList.Create;
End;

Destructor TInsertList.Destroy;
Begin
  Clear;
  FItems.Free;
  Inherited Destroy;
End;

Function TInsertList.GetCount;
Begin
  Result := FItems.Count;
End;

Function TInsertList.GetItem( Index: Integer ): TInsertItem;
Begin
  Result := FItems[Index];
End;

Function TInsertList.Add: TInsertItem;
Begin
  Result := TInsertItem.Create( Self );
  FItems.Add( Result );
End;

Procedure TInsertList.Clear;
Var
  I: Integer;
Begin
  For I := 0 To FItems.Count - 1 Do
    TInsertItem( FItems[I] ).Free;
  FItems.Clear;
End;

Procedure TInsertList.Delete( Index: Integer );
Begin
  TInsertItem( FItems[Index] ).Free;
  FItems.Delete( Index );
End;

{-------------------------------------------------------------------------------}
{                  implements TSrtField                                         }
{-------------------------------------------------------------------------------}

Constructor TSrtField.Create( Fields: TSrtFields );
Begin
  Inherited Create;
  FFields := Fields;
End;

Function TSrtField.GetData( Buffer: Pointer ): Boolean;
Begin
  Result := FFields.FSortList.GetFieldData( Self, Buffer );
End;

Procedure TSrtField.SetData( Buffer: Pointer );
Begin
  FFields.FSortList.SetFieldData( Self, Buffer );
End;

Procedure TSrtField.SetDataType( Value: TExprType );
Begin
  FDataType := Value;
End;

{-------------------------------------------------------------------------------}
{                  implements TSrtStringField                                   }
{-------------------------------------------------------------------------------}

Constructor TSrtStringField.Create( Fields: TSrtFields );
Begin
  Inherited Create( Fields );
  SetDataType( ttString );
End;

Function TSrtStringField.GetValue( Var Value: String ): Boolean;
Var
  Buffer: Array[0..dsMaxStringSize] Of Char;
Begin
  Result := GetData( @Buffer );
  If Result Then
    Value := Buffer;
End;

Function TSrtStringField.GetAsString: String;
Begin
  If Not GetValue( Result ) Then
    Result := '';
End;

Procedure TSrtStringField.SetAsString( Const Value: String );
Var
  Buffer: Array[0..dsMaxStringSize] Of Char;
  L: Integer;
Begin
  FillChar( Buffer, FDataSize, 0 );
  L := Length( Value );
  StrLCopy( Buffer, PChar( Value ), L );
  SetData( @Buffer );
End;

Function TSrtStringField.GetAsFloat: double;
Begin
  Result := 0;
End;

Procedure TSrtStringField.SetAsFloat( Value: double );
Begin
End;

Function TSrtStringField.GetAsInteger: Longint;
Begin
  Result := 0;
End;

Procedure TSrtStringField.SetAsInteger( Value: Longint );
Begin
End;

Function TSrtStringField.GetAsBoolean: Boolean;
Begin
  Result := False;
End;

Procedure TSrtStringField.SetAsBoolean( Value: Boolean );
Begin
End;

{-------------------------------------------------------------------------------}
{                  implements TSrtFloatField                                        }
{-------------------------------------------------------------------------------}

Constructor TSrtFloatField.Create( Fields: TSrtFields );
Begin
  Inherited Create( Fields );
  SetDataType( ttFloat );
End;

Function TSrtFloatField.GetAsFloat: double;
Begin
  If Not GetData( @Result ) Then
    Result := 0;
End;

Procedure TSrtFloatField.SetAsFloat( Value: double );
Begin
  SetData( @Value );
End;

Function TSrtFloatField.GetAsString: String;
Var
  F: Double;
Begin
  If GetData( @F ) Then
    Result := FloatToStr( F )
  Else
    Result := '';
End;

Procedure TSrtFloatField.SetAsString( Const Value: String );
Var
  F: Extended;
Begin
  If Value = '' Then
    SetAsFloat( 0 )
  Else
  Begin
    If Not TextToFloat( PChar( Value ), F, fvExtended ) Then
      EXQueryError.CreateFmt( SIsInvalidFloatValue, [Value] );
    SetAsFloat( F );
  End;
End;

Function TSrtFloatField.GetAsInteger: Longint;
Begin
  Result := 0;
End;

Procedure TSrtFloatField.SetAsInteger( Value: Longint );
Begin
End;

Function TSrtFloatField.GetAsBoolean: Boolean;
Begin
  Result := False;
End;

Procedure TSrtFloatField.SetAsBoolean( Value: Boolean );
Begin
End;

{-------------------------------------------------------------------------------}
{                  implements TsrtIntegerField                                      }
{-------------------------------------------------------------------------------}

Constructor TSrtIntegerField.Create( Fields: TSrtFields );
Begin
  Inherited Create( Fields );
  SetDataType( ttInteger );
End;

Function TSrtIntegerField.GetAsInteger: Longint;
Begin
  If Not GetData( @Result ) Then
    Result := 0;
End;

Procedure TSrtIntegerField.SetAsInteger( Value: Longint );
Begin
  SetData( @Value );
End;

Function TSrtIntegerField.GetAsString: String;
Var
  L: Longint;
Begin
  If GetData( @L ) Then
    Result := IntToStr(L)  { patched by ccy }
  Else
    Result := '';
End;

Procedure TSrtIntegerField.SetAsString( Const Value: String );
Var
  E: Integer;
  L: Longint;
Begin
  Val( Value, L, E );
  If E <> 0 Then
    EXQueryError.CreateFmt( SIsInvalidIntegerValue, [Value] );
  SetAsInteger( L );
End;

Function TSrtIntegerField.GetAsFloat: double;
Begin
  Result := 0;
End;

Procedure TSrtIntegerField.SetAsFloat( Value: double );
Begin
End;

Function TSrtIntegerField.GetAsBoolean: Boolean;
Begin
  Result := False;
End;

Procedure TSrtIntegerField.SetAsBoolean( Value: Boolean );
Begin
End;

{-------------------------------------------------------------------------------}
{                  implements TSrtBooleanField                                      }
{-------------------------------------------------------------------------------}

Constructor TSrtBooleanField.Create( Fields: TSrtFields );
Begin
  Inherited Create( Fields );
  SetDataType( ttBoolean );
End;

Function TSrtBooleanField.GetAsBoolean: Boolean;
Var
  B: WordBool;
Begin
  If GetData( @B ) Then
    Result := B
  Else
    Result := False;
End;

Procedure TSrtBooleanField.SetAsBoolean( Value: Boolean );
Var
  B: WordBool;
Begin
  If Value Then
    Word( B ) := 1
  Else
    Word( B ) := 0;
  SetData( @B );
End;

Function TSrtBooleanField.GetAsString: String;
Var
  B: WordBool;
Begin
  If GetData( @B ) Then
    Result := Copy( xqbase.NBoolean[B], 1, 1 )
  Else
    Result := '';
End;

Procedure TSrtBooleanField.SetAsString( Const Value: String );
Var
  L: Integer;
Begin
  L := Length( Value );
  If L = 0 Then
  Begin
    SetAsBoolean( False );
  End
  Else
  Begin
    If AnsiCompareText( Value, Copy( xqbase.NBoolean[False], 1, L ) ) = 0 Then
      SetAsBoolean( False )
    Else If AnsiCompareText( Value, Copy( xqbase.NBoolean[True], 1, L ) ) = 0 Then
      SetAsBoolean( True )
    Else
      EXQueryError.CreateFmt( SIsInvalidBoolValue, [Value] );
  End;
End;

Function TSrtBooleanField.GetAsInteger: Longint;
Begin
  Result := 0;
End;

Procedure TSrtBooleanField.SetAsInteger( Value: Longint );
Begin
End;

Function TSrtBooleanField.GetAsFloat: double;
Begin
  Result := 0;
End;

Procedure TSrtBooleanField.SetAsFloat( Value: double );
Begin
End;

{-------------------------------------------------------------------------------}
{                  implements TSrtFields                                            }
{-------------------------------------------------------------------------------}

Constructor TSrtFields.Create( SortList: TxqSortList );
Begin
  Inherited Create;
  FSortList := SortList;
  FItems := TList.Create;
End;

Destructor TSrtFields.Destroy;
Begin
  Clear;
  FItems.Free;
  Inherited Destroy;
End;

Function TSrtFields.GetCount: Integer;
Begin
  Result := FItems.Count;
End;

Function TSrtFields.GetItem( Index: Integer ): TSrtField;
Begin
  Result := FItems[Index];
End;

Function TSrtFields.Add( DataType: TExprType ): TSrtField;
Begin
  Result := Nil;
  Case DataType Of
    ttString: Result := TSrtStringField.Create( Self );
    ttFloat: Result := TSrtFloatField.Create( Self );
    ttInteger: Result := TSrtIntegerField.Create( Self );
    ttBoolean: Result := TSrtBooleanField.Create( Self );
  End;
  FItems.Add( Result );
End;

Procedure TSrtFields.Clear;
Var
  I: Integer;
Begin
  For I := 0 To FItems.Count - 1 Do
    TSrtField( FItems[I] ).Free;
  FItems.Clear;
End;

{-------------------------------------------------------------------------------}
{                  Define TxqSortList                                           }
{-------------------------------------------------------------------------------}

Constructor TxqSortList.Create( UsingBookmark: Boolean );
Begin
  Inherited Create;
  FFields := TSrtFields.Create( Self );
  FRecNo := -1;
  FRecordBufferSize := SizeOf( Integer ); { first data is the SourceRecNo property }
  FUsingBookmark := UsingBookmark;
End;

Destructor TxqSortList.Destroy;
Begin
  Clear();
  FreeAndNil(FFields);
  If Assigned( FSelected ) Then
    FreeAndNil(FSelected);
  Inherited;
End;

Procedure TxqSortList.Clear;
Var
  I: Integer;
begin
  If FUsingBookmark And Assigned( FBookmarkedDataset ) Then
  Begin
    For I := 1 To GetRecordCount Do
    Begin
      SetRecno( I );
      FBookmarkedDataset.FreeBookmark( TBookmark( SourceRecno ) );
    End;
  End;
  FBookmarkedDataset:= nil;
end;

Procedure TxqSortList.SetRecno( Value: Integer );
Begin
  If ( Value < 1 ) Or ( Value > GetRecordCount ) Then
    Raise EXQueryError.Create( SRecnoInvalid );
  FRecNo := Value;
End;

Function TxqSortList.GetRecno: Integer;
Begin
  Result := FRecNo;
End;

Procedure TxqSortList.AddField( pDataType: TExprType;
  pDataSize: Integer; pDescending: Boolean );
Begin
  With FFields.Add( pDataType ) Do
  Begin
    BufferOffset := FRecordBufferSize;
    DataType := pDataType;
    Case DataType Of
      ttString: DataSize := (pDataSize + 1) * SizeOf(Char);  { patched by ccy}
      ttFloat: DataSize := SizeOf( Double );
      ttInteger: DataSize := SizeOf( Integer );
      ttBoolean: DataSize := SizeOf( WordBool );
    End;
    Desc := pDescending;
    Inc( FRecordBufferSize, DataSize );
  End;
End;

Function TxqSortList.IsEqual( Recno1, Recno2: Integer ): Boolean;
Var
  Buffer: PAnsiChar;
  Buffer1: PAnsiChar;
  Buffer2: PAnsiChar;
Begin
  SetRecno( Recno1 );
  Buffer := ActiveBuffer;
  GetMem( Buffer1, FRecordBufferSize );
  Move( Buffer^, Buffer1^, FRecordBufferSize );

  SetRecno( Recno2 );
  Buffer := ActiveBuffer;
  GetMem( Buffer2, FRecordBufferSize );
  Move( Buffer^, Buffer2^, FRecordBufferSize );

  { the first SizeOf(Integer) bytes is the source recno and always is different }
  Result := Comparemem( ( Buffer1 + SizeOf( Integer ) ),
    ( Buffer2 + SizeOf( Integer ) ), FRecordBufferSize - SizeOf( Integer ) );

  FreeMem( Buffer1, FRecordBufferSize );
  FreeMem( Buffer2, FRecordBufferSize );

End;

Function TxqSortList.DoCompare( N: Integer; Const KeyValue: Variant ): Integer;
{ returns -1, 0 or 1 for a<b, a=b, a>b}
Var
  DataType: TExprType;
  s: String;
  f: Double;
  i: Integer;
  b, cb: Boolean;
  CompareValue: Variant;
Begin
  Result := 0;
  SetRecno( N );
  DataType := FFields[0].DataType;
  If VarIsNull( KeyValue ) Then
  Begin
    { solo por si se ofrece, se prueba tambien al recibir un valor NULL }
    Case DataType Of
      ttString: CompareValue := '';
      ttFloat, ttInteger: CompareValue := 0;
      ttBoolean: CompareValue := False ;
    End;
  End Else
    CompareValue := KeyValue;
  Case DataType Of
    ttString:
      Begin
        s := FFields[0].AsString;
        If s = CompareValue Then
        Begin
          Result := 0;
          Exit;
        End;
        If FFields[0].Desc Then
        Begin
          If s < CompareValue Then
            Result := 1
          Else
            Result := -1;
        End
        Else
        Begin
          If s < CompareValue Then
            Result := -1
          Else
            Result := 1;
        End;
      End;
    ttFloat:
      Begin
        f := FFields[0].AsFloat;
        If f = CompareValue Then
        Begin
          Result := 0;
          Exit;
        End;
        If FFields[0].Desc Then
        Begin
          If f < CompareValue Then
            Result := 1
          Else
            Result := -1;
        End
        Else
        Begin
          If f < CompareValue Then
            Result := -1
          Else
            Result := 1;
        End;
      End;
    ttInteger:
      Begin
        i := FFields[0].AsInteger;
        If i = CompareValue Then
        Begin
          Result := 0;
          Exit;
        End;
        If FFields[0].Desc Then
        Begin
          If i < CompareValue Then
            Result := 1
          Else
            Result := -1;
        End
        Else
        Begin
          If i < CompareValue Then
            Result := -1
          Else
            Result := 1;
        End;
      End;
    ttBoolean:
      Begin
        b := FFields[0].AsBoolean;
        cb := CompareValue;
        If ord( b ) = ord( cb ) Then
        Begin
          Result := 0;
          Exit;
        End;
        If FFields[0].Desc Then
        Begin
          If Ord( b ) < Ord( cb ) Then
            Result := 1
          Else
            Result := -1;
        End
        Else
        Begin
          If Ord( b ) < Ord( cb ) Then
            Result := -1
          Else
            Result := 1;
        End;
      End;
  End;
End;

Function TxqSortList.Find( Const KeyValue: Variant; Var Index: Integer ): Boolean;
Var
  L, H, I, C: Integer;
Begin
  Result := False;
  L := 1;
  H := GetRecordCount;
  While L <= H Do
  Begin
    I := ( L + H ) Shr 1;
    C := DoCompare( I, KeyValue );
    If C < 0 Then
      L := I + 1
    Else
    Begin
      H := I - 1;
      If C = 0 Then
      Begin
        Result := True;
        //if Duplicates <> dupAccept then L := I;
      End;
    End;
  End;
  Index := L;
End;

{ this methods filter only by the first data column of the sort }

Procedure TxqSortList.Filter( Const KeyValue: Variant );
Var
  I, Index: Integer;
Begin
  If FSelected = Nil Then
    FSelected := TList.Create
  Else
    FSelected.Clear;
  { the first value must be on the database }
  If Self.Find( KeyValue, Index ) Then
  Begin
    For I := Index To GetRecordCount Do
      If DoCompare( I, KeyValue ) = 0 Then
        FSelected.Add( Pointer( I ) )
      Else
        Break;
  End;
  FFilterRecno := -1;
End;

Procedure TxqSortList.First;
Begin
  If FSelected = Nil Then Exit;
  If FSelected.Count > 0 Then
  Begin
    FFilterRecno := 0;
    FBofCrack := false;
    FEofCrack := false;
    SetRecno( Longint( FSelected[FFilterRecno] ) );
  End
  Else
  Begin
    FBofCrack := true;
    FEofCrack := true;
  End
End;

Procedure TxqSortList.Next;
Begin
  If FSelected = Nil Then Exit;
  If FSelected.Count > 0 Then
  Begin
    If FFilterRecno < FSelected.Count - 1 Then
    Begin
      Inc( FFilterRecno );
      FBofCrack := false;
      FEofCrack := false;
    End
    Else
    Begin
      FFilterRecno := FSelected.Count - 1;
      FBofCrack := false;
      FEofCrack := true;
    End;
    SetRecno( Longint( FSelected[FFilterRecno] ) );
  End
  Else
  Begin
    FBofCrack := true;
    FEofCrack := true;
  End
End;

Function TxqSortList.Eof: Boolean;
Begin
  result := FEofCrack;
End;

Function TxqSortList.Bof: Boolean;
Begin
  result := FBofCrack;
End;

type
  TDataSetAccess = class(TDataSet);

procedure TxqSortList.SetBookmarkedDataset(const Value: TDataset);
begin
  FBookmarkedDataset := Value;
  fRecordBufferSize := TDataSetAccess(FBookmarkedDataset).BookmarkSize;
end;

Procedure TxqSortList.Sort;
Var
  I, Idx: Integer;
  Index: Integer;
  Pivot: Integer;
  DataType: TExprType;
  IsDesc: Boolean;
  TempL, TempR: String;

  Function SortCompare_S( Recno: Integer; Const Value: String ): Integer;
  Var
    s: String;
  Begin
    SetRecno( Recno );
    s := FFields[Idx].AsString;
    If s = Value Then
    Begin
      Result := 0;
      Exit;
    End;
    If IsDesc Then
    Begin
      If s < Value Then
        Result := 1
      Else
        Result := -1;
    End
    Else
    Begin
      If s < Value Then
        Result := -1
      Else
        Result := 1;
    End;
  End;

  Function SortCompare_F( Recno: Integer; Const Value: Double ): Integer;
  Var
    f: Double;
  Begin
    SetRecno( Recno );
    f := FFields[Idx].AsFloat;
    If f = Value Then
    Begin
      Result := 0;
      Exit;
    End;
    If IsDesc Then
    Begin
      If f < Value Then
        Result := 1
      Else
        Result := -1;
    End
    Else
    Begin
      If f < Value Then
        Result := -1
      Else
        Result := 1;
    End;
  End;

  Function SortCompare_I( Recno: Integer; Value: Integer ): Integer;
  Var
    i: Integer;
  Begin
    SetRecno( Recno );
    i := FFields[Idx].AsInteger;
    If i = Value Then
    Begin
      Result := 0;
      Exit;
    End;
    If IsDesc Then
    Begin
      If i < Value Then
        Result := 1
      Else
        Result := -1;
    End
    Else
    Begin
      If i < Value Then
        Result := -1
      Else
        Result := 1;
    End;
  End;

  Function SortCompare_B( Recno: Integer; Value: Boolean ): Integer;
  Var
    b: Boolean;
  Begin
    SetRecno( Recno );
    b := FFields[Idx].AsBoolean;
    If Ord( b ) = Ord( Value ) Then
    Begin
      Result := 0;
      Exit;
    End;
    If IsDesc Then
    Begin
      If Ord( b ) < Ord( Value ) Then
        Result := 1
      Else
        Result := -1;
    End
    Else
    Begin
      If Ord( b ) < Ord( Value ) Then
        Result := -1
      Else
        Result := 1;
    End;
  End;

  Procedure QuickSort( L, R: Integer );
  Var
    I, J, P: Integer;
    s1: String;
    f1: Double;
    i1: Integer;
    b1: Boolean;
  Begin
    Repeat
      I := L;
      J := R;
      P := ( L + R ) Shr 1;
      SetRecno( P );
      f1 := 0;
      i1 := 0;
      b1 := False;
      Case DataType Of
        ttString: s1 := FFields[Idx].AsString;
        ttFloat: f1 := FFields[Idx].AsFloat;
        ttInteger: i1 := FFields[Idx].AsInteger;
        ttBoolean: b1 := FFields[Idx].AsBoolean;
      End;
      Repeat
        Case DataType Of
          ttString:
            Begin
              While SortCompare_S( I, s1 ) < 0 Do
                Inc( I );
            End;
          ttFloat:
            Begin
              While SortCompare_F( I, f1 ) < 0 Do
                Inc( I );
            End;
          ttInteger:
            Begin
              While SortCompare_I( I, i1 ) < 0 Do
                Inc( I );
            End;
          ttBoolean:
            Begin
              While SortCompare_B( I, b1 ) < 0 Do
                Inc( I );
            End;
        End;

        Case DataType Of
          ttString:
            Begin
              While SortCompare_S( J, s1 ) > 0 Do
                Dec( J );
            End;
          ttFloat:
            Begin
              While SortCompare_F( J, f1 ) > 0 Do
                Dec( J );
            End;
          ttInteger:
            Begin
              While SortCompare_I( J, i1 ) > 0 Do
                Dec( J );
            End;
          ttBoolean:
            Begin
              While SortCompare_B( J, b1 ) > 0 Do
                Dec( J );
            End;
        End;
        If I <= J Then
        Begin
          Exchange( I, J );
          Inc( I );
          Dec( J );
        End;
      Until I > J;
      If L < J Then
        QuickSort( L, J );
      L := I;
    Until I >= R;
  End;

Begin
  If ( FFields.Count = 0 ) Or ( GetRecordCount = 0 ) Then Exit;
  Idx := 0;
  DataType := FFields[0].DataType;
  IsDesc := FFields[0].Desc;
  QuickSort( 1, GetRecordCount );
  For Idx := 1 To FFields.Count - 1 Do
  Begin
    SetRecno( 1 );
    DataType := FFields[Idx].DataType;
    IsDesc := FFields[Idx].Desc;
    Index := 1;
    Pivot := 1;
    TempL := '';
    For I := 0 To Idx - 1 Do
      TempL := TempL + FFields[I].AsString;
    While Index <= GetRecordCount Do
    Begin
      SetRecno( Index );
      TempR := '';
      For I := 0 To Idx - 1 Do
        TempR := TempR + FFields[I].AsString;
      If TempL <> TempR Then
      Begin
        If Index - 1 > Pivot Then
          QuickSort( Pivot, Index - 1 );

        Pivot := Index;
        SetRecno( Pivot );
        TempL := TempR;
        Index := Pivot - 1;
      End;
      Inc( Index );
    End;
    If ( ( Index - 1 ) <= GetRecordCount ) And ( Index - 1 > Pivot ) Then
      QuickSort( Pivot, Index - 1 );
  End;
End;

{-------------------------------------------------------------------------------}
{                  implements TMemSortList                                      }
{-------------------------------------------------------------------------------}

Constructor TMemSortList.Create( UsingBookmark: Boolean );
Begin
  Inherited Create( UsingBookmark );
  FBufferList := TList.Create;
End;

Destructor TMemSortList.Destroy;
Begin
  Clear();
  FreeAndNil(FBufferList);
  Inherited Destroy;
End;

Procedure TMemSortList.Clear;
Var
  I: Integer;
  Buffer: PChar;
Begin
  inherited Clear();
  if(FBufferList<>nil)then
  begin
    For I := 0 To FBufferList.Count - 1 Do
    Begin
      Buffer := FBufferList[I];
      // Start Modified by CCY: Fixed memory leak. Free the Bookmark allocated
//      if fUsingBookMark then
//        Move( ( Buffer + 0 )^, L, SizeOf(L));
      // End Modified by CCY

      FreeMem( Buffer, FRecordBufferSize );
    End;
    FBufferList.Clear();
  end;
  if(FFields<>nil) then FFields.Clear();
  FRecordBufferSize := SizeOf( Integer );
  FRecNo := -1;
End;

function TMemSortList.ActiveBuffer: PAnsiChar;
Begin
  Result := Nil;
  If ( FRecNo < 1 ) Or ( FRecNo > FBufferList.Count ) Then Exit;
  Result := FBufferList[FRecNo - 1];
End;

Function TMemSortList.GetFieldData( Field: TSrtField; Buffer: Pointer ): Boolean;
Var
  RecBuf: PAnsiChar;
Begin
  Result := False;
  RecBuf := ActiveBuffer;
  If RecBuf = Nil Then  Exit;
  Move( ( RecBuf + Field.BufferOffset )^, Buffer^, Field.DataSize );
  Result := True;
End;

Procedure TMemSortList.SetFieldData( Field: TSrtField; Buffer: Pointer );
Var
  RecBuf: PAnsiChar;
Begin
  RecBuf := ActiveBuffer;
  If ( RecBuf = Nil ) Or ( Buffer = Nil ) Then Exit;
  Move( Buffer^, ( RecBuf + Field.BufferOffset )^, Field.DataSize );
End;

Procedure TMemSortList.Insert;
Var
  Buffer: PChar;
Begin
  GetMem( Buffer, FRecordBufferSize );
  FillChar( Buffer^, FRecordBufferSize, 0 );
  FBufferList.Add( Buffer );
  FRecNo := FBufferList.Count;
End;

Function TMemSortList.GetRecordCount: Integer;
Begin
  Result := FBufferList.Count;
End;

Procedure TMemSortList.Exchange( Recno1, Recno2: Integer );
Begin
  FBufferList.Exchange( Recno1 - 1, Recno2 - 1 );
End;

{$if RtlVersion >= 20}
function TMemSortList.GetSourceBookmark: TBookmark;
Var
  Buffer: PAnsiChar;
Begin
  Result := nil;
  If ( fRecNo < 1 ) Or ( fRecNo > GetRecordCount ) Then
    Exit;
  Buffer := PAnsiChar( fBufferList[fRecNo - 1] );
  SetLength(Result, 20);
  Move( ( Buffer + 0 )^, Result[0], 20 );
End;
{$ifend}

Function TMemSortList.GetSourceRecno: Integer;
Var
  Buffer: PChar;
Begin
  Result := 0;
  If ( FRecNo < 1 ) Or ( FRecNo > GetRecordCount ) Then Exit;
  Buffer := PChar( FBufferList[FRecNo - 1] );
  Move( ( Buffer + 0 )^, Result, SizeOf( Integer ) );
End;

{$if RtlVersion >= 20}
procedure TMemSortList.SetSourceBookmark(const Value: TBookmark);
Var
  Buffer: PAnsiChar;
Begin
  If ( fRecNo < 1 ) Or ( fRecNo > GetRecordCount ) Then Exit;
  Buffer := PAnsiChar( fBufferList[fRecNo - 1] );
  Move( Value[0], ( Buffer + 0 )^, 20 );
End;
{$ifend}

Procedure TMemSortList.SetSourceRecno( Value: Integer );
Var
  Buffer: PChar;
Begin
  If ( FRecNo < 1 ) Or ( FRecNo > GetRecordCount ) Then Exit;
  Buffer := PChar( FBufferList[FRecNo - 1] );
  Move( Value, ( Buffer + 0 )^, SizeOf( Integer ) );
End;

{-------------------------------------------------------------------------------}
{                  implements TFileSortList                                     }
{-------------------------------------------------------------------------------}

Constructor TFileSortList.Create( UsingBookmark: Boolean; MapFileSize: Longint );
Begin
  Inherited Create( UsingBookmark );
  FBufferList := TList.Create;
  FTmpFile := GetTemporaryFileName( '~xq' );
  FMemMapFile := TMemMapFile.Create( FTmpFile, fmCreate, MapFileSize, True );
End;

Destructor TFileSortList.Destroy;
Begin
  Clear;
  FreeObject( FMemMapFile );
  SysUtils.DeleteFile( FTmpFile );
  FBufferList.Free;
  If Assigned( FBuffer ) Then
    FreeMem( FBuffer, FRecordBufferSize );
  Inherited Destroy;
End;

Procedure TFileSortList.Clear;
Begin
  FMemMapFile.Seek( 0, 0 );
  FBufferList.Clear;
  FFields.Clear;
  FRecordBufferSize := SizeOf( Integer );
  FRecNo := -1;
End;

function TFileSortList.ActiveBuffer: PAnsiChar;
Begin
  Result := Nil;
  If ( FRecNo < 1 ) Or ( FRecNo > FBufferList.Count ) Then
    Exit;
  If Not Assigned( FBuffer ) Then
    GetMem( FBuffer, FRecordBufferSize );
  FMemMapFile.Seek( Longint( FBufferList[FRecNo - 1] ), 0 );
  FMemMapFile.Read( FBuffer^, FRecordBufferSize );
  Result := FBuffer;
End;

Function TFileSortList.GetFieldData( Field: TSrtField; Buffer: Pointer ): Boolean;
Var
  RecBuf: PAnsiChar;
Begin
  Result := False;
  RecBuf := ActiveBuffer;
  If RecBuf = Nil Then
    Exit;
  Move( ( RecBuf + Field.BufferOffset )^, Buffer^, Field.DataSize );
  Result := True;
End;

Procedure TFileSortList.SetFieldData( Field: TSrtField; Buffer: Pointer );
Var
  RecBuf: PAnsiChar;
Begin
  RecBuf := ActiveBuffer;
  If RecBuf = Nil Then
    Exit;
  Move( Buffer^, ( RecBuf + Field.BufferOffset )^, Field.DataSize );
  FMemMapFile.Seek( Longint( FBufferList[Recno - 1] ), 0 );
  FMemMapFile.Write( RecBuf^, FRecordBufferSize );
End;

Procedure TFileSortList.Insert;
Var
  Offset: Integer;
Begin
  If Not Assigned( FBuffer ) Then
    GetMem( FBuffer, FRecordBufferSize );
  FillChar( FBuffer^, FRecordBufferSize, 0 );
  Offset := FMemMapFile.VirtualSize;
  FMemMapFile.Seek( Offset, 0 );
  FMemMapFile.Write( FBuffer^, FRecordBufferSize );
  FBufferList.Add( Pointer( Offset ) ); { the address in temp file is saved }
  FRecNo := FBufferList.Count;
End;

Function TFileSortList.GetRecordCount: Integer;
Begin
  Result := FBufferList.Count;
End;

Procedure TFileSortList.Exchange( Recno1, Recno2: Integer );
Begin
  FBufferList.Exchange( Recno1 - 1, Recno2 - 1 );
End;

{$if RtlVersion >= 20}
function TFileSortList.GetSourceBookmark: TBookmark;
begin
  Result := nil;
end;
{$ifend}

Procedure TFileSortList.SetSourceRecno( Value: Integer );
Begin
  If ( FRecNo < 1 ) Or ( FRecNo > GetRecordCount ) Then
    Exit;
  FMemMapFile.Seek( Longint( FBufferList[FRecNo - 1] ), 0 );
  FMemMapFile.Write( Value, SizeOf( Integer ) );
End;

Function TFileSortList.GetSourceRecno: Integer;
Var
  RecBuf: PAnsiChar;
Begin
  Result := -1;
  RecBuf := ActiveBuffer;
  If RecBuf = Nil Then
    Exit;
  Move( ( RecBuf + 0 )^, Result, SizeOf( Integer ) );
End;

{$if RtlVersion >= 20}
procedure TFileSortList.SetSourceBookmark(const Value: TBookmark);
begin

end;
{$ifend}

{-------------------------------------------------------------------------------}
{ Implementation of TAggregateItem                                              }
{-------------------------------------------------------------------------------}

Constructor TAggregateItem.Create( AggregateList: TAggregateList );
Begin
  Inherited Create;
  FAggregateList := AggregateList;

  FSparseList := TAggSparseList.Create( 1000 );
End;

Destructor TAggregateItem.Destroy;
Begin
  FSparseList.Free;
  Inherited Destroy;
End;

{-------------------------------------------------------------------------------}
{ Implementation of TAggregateList                                              }
{-------------------------------------------------------------------------------}

Constructor TAggregateList.Create;
Begin
  Inherited Create;
  FItems := TList.Create;
End;

Destructor TAggregateList.Destroy;
Begin
  Clear;
  FItems.Free;
  Inherited Destroy;
End;

Function TAggregateList.GetCount;
Begin
  Result := FItems.Count;
End;

Function TAggregateList.GetItem( Index: Integer ): TAggregateItem;
Begin
  Result := FItems[Index];
End;

Function TAggregateList.Add: TAggregateItem;
Begin
  Result := TAggregateItem.Create( Self );
  FItems.Add( Result );
End;

Procedure TAggregateList.Clear;
Var
  I: Integer;
Begin
  For I := 0 To FItems.Count - 1 Do
    TAggregateItem( FItems[I] ).Free;
  FItems.Clear;
End;

Procedure TAggregateList.Delete( Index: Integer );
Begin
  TAggregateItem( FItems[Index] ).Free;
  FItems.Delete( Index );
End;

Procedure TAggregateList.Assign( AggregateList: TAggregateList );
Var
  I: Integer;
  Aggr: TAggregateItem;
Begin
  Clear;
  For I := 0 To AggregateList.Count - 1 Do
  Begin
    Aggr := AggregateList[I];
    With Self.Add Do
    Begin
      AggregateStr := Aggr.AggregateStr;
      ColIndex := Aggr.ColIndex;
      Aggregate := Aggr.Aggregate;
      IsDistinctAg := Aggr.IsDistinctAg;
    End;
  End;
End;

{-------------------------------------------------------------------------------}
{  Implements TMemMapFile                                                       }
{-------------------------------------------------------------------------------}

Constructor TMemMapFile.Create( FileName: String; FileMode: integer;
  Size: integer; MapNow: Boolean );
{ Creates Memory Mapped view of FileName file.
  FileName: Full pathname of file.
  FileMode: Use fmXXX constants.
  Size: size of memory map.  Pass zero as the size to use the
        file's own size.
}
Begin

  { Initialize private fields }
  FMapNow := MapNow;
  FFileName := FileName;
  FFileMode := FileMode;

  AllocFileHandle; // Obtain a file handle of the disk file.
  { Assume file is < 2 gig  }

  FFileSize := GetFileSize( FFileHandle, Nil );
  FSize := Size;

  Try
    AllocFileMapping; // Get the file mapping object handle.
  Except
    On ExQueryError Do
    Begin
      CloseHandle( FFileHandle ); // close file handle on error
      FFileHandle := 0; // set handle back to 0 for clean up
      Raise; // re-raise exception
    End;
  End;
  If FMapNow Then
    AllocFileView; // Map the view of the file
End;

Destructor TMemMapFile.Destroy;
Begin

  If FFileHandle <> 0 Then
    CloseHandle( FFileHandle ); // Release file handle.

  { Release file mapping object handle }
  If FMapHandle <> 0 Then
    CloseHandle( FMapHandle );

  FreeMapping; { Unmap the file mapping view . }
  Inherited Destroy;
End;

Procedure TMemMapFile.FreeMapping;
{ This method unmaps the view of the file from this process's address space }
Begin
  If FData <> Nil Then
  Begin
    UnmapViewOfFile( FData );
    FData := Nil;
  End;
End;

Function TMemMapFile.GetSize: Longint;
Begin
  If FSize <> 0 Then
    Result := FSize
  Else
    Result := FFileSize;
End;

Procedure TMemMapFile.AllocFileHandle;
{ creates or opens disk file before creating memory mapped file }
Begin
  If FFileMode = fmCreate Then
    FFileHandle := FileCreate( FFileName )
  Else
    FFileHandle := FileOpen( FFileName, FFileMode );

  If FFileHandle < 0 Then
    Raise ExQueryError.Create( SFailOpenFile );
End;

Procedure TMemMapFile.AllocFileMapping;
Var
  ProtAttr: DWORD;
Begin
  If FFileMode = fmOpenRead Then // obtain correct protection attribute
    ProtAttr := Page_ReadOnly
  Else
    ProtAttr := Page_ReadWrite;
  { attempt to create file mapping of disk file.
    Raise exception on error. }
  FMapHandle := CreateFileMapping( FFileHandle, Nil, ProtAttr,
    0, FSize, Nil );
  If FMapHandle = 0 Then
    Raise ExQueryError.Create( SFailCreateMapping );
End;

Procedure TMemMapFile.AllocFileView;
Var
  Access: Longint;
Begin
  If FFileMode = fmOpenRead Then // obtain correct file mode
    Access := File_Map_Read
  Else
    Access := File_Map_All_Access;
  FData := MapViewOfFile( FMapHandle, Access, 0, 0, FSize );
  If FData = Nil Then
    Raise ExQueryError.Create( SFailMapView );
End;

Procedure TMemMapFile.Read( Var Buffer; Count: Longint );
Begin
  If FPosition + Count > GetSize Then
    Raise ExQueryError.Create( SBeyondEOF );
  Move( ( FData + FPosition )^, Buffer, Count );
  Inc( FPosition, Count );
End;

Procedure TMemMapFile.Write( Const Buffer; Count: Longint );
Begin
  Move( Buffer, ( FData + FPosition )^, Count );
  Inc( FPosition, Count );
  FVirtualSize := IMax( FPosition, FVirtualSize );
End;

Procedure TMemMapFile.Seek( Offset: Longint; Origin: Word );
Begin
  FPosition := Offset; // only from beginning supported (Origin = 0)
End;

{ TUserDefinedRange }

Constructor TUserDefinedRange.Create;
Begin
  Inherited Create;
  FForFields := TStringList.Create;
  FStartValues := TStringList.Create;
  FEndValues := TStringList.Create;
  FStartResolvers := TList.create;
  FEndResolvers := TList.create;
End;

Destructor TUserDefinedRange.Destroy;
Begin
  FForFields.Free;
  FStartValues.Free;
  FEndValues.Free;
  ClearResolvers;
  FStartResolvers.free;
  FEndResolvers.free;
  Inherited Destroy;
End;

Procedure TUserDefinedRange.ClearResolvers;
Var
  I: Integer;
Begin
  For I := 0 To FStartResolvers.Count - 1 Do
    TExprParser( FStartResolvers[I] ).Free;
  FStartResolvers.Clear;
  For I := 0 To FEndResolvers.Count - 1 Do
    TExprParser( FEndResolvers[I] ).Free;
  FEndResolvers.Clear;
End;

{ TParamsAsFieldsItem }

procedure TParamsAsFieldsItem.Assign(Source: TPersistent);
begin
  If Source Is TParamsAsFieldsItem Then
  Begin
    FName := TParamsAsFieldsItem( Source ).Name;
    FValue := TParamsAsFieldsItem( Source ).Value;
  End
  Else
    Inherited Assign( Source );
end;

procedure TParamsAsFieldsItem.SetName(const Value: string);
begin
  { check if param already exists }
  if (Collection as TParamsAsFields).ParamByName(Value) <> Nil then
    Raise EXQueryError.Create( SDupParamsAsFields );
  FName:= value;
end;

procedure TParamsAsFieldsItem.SetValue(const Value: String);
begin
  FValue:= Value;
end;

Function TParamsAsFieldsItem.GetDisplayName: String;
begin
  if (Length(Name) <> 0) Or (Length(Value) <> 0) then
    Result:= Name + ' - ' + Value
  else
    Result:= inherited GetDisplayName;
end;

{ TParamsAsFields }

constructor TParamsAsFields.Create(AOwner: TPersistent);
begin
  Inherited Create( AOwner, TParamsAsFieldsItem );
end;

function TParamsAsFields.Add: TParamsAsFieldsItem;
begin
  Result := TParamsAsFieldsItem( Inherited Add );
end;

function TParamsAsFields.GetItem(Index: Integer): TParamsAsFieldsItem;
begin
  Result := TParamsAsFieldsItem( Inherited GetItem( Index ) );
end;

procedure TParamsAsFields.SetItem(Index: Integer; Value: TParamsAsFieldsItem);
begin
  Inherited SetItem( Index, Value );
end;

function TParamsAsFields.ParamByName(const Value: string): TParamsAsFieldsItem;
var
  I: Integer;
begin
  Result:= Nil;
  for I:= 0 to Count - 1 do
    if GetItem( I ).Name = Value then
    begin
      Result:= GetItem( I );
      Exit;
    end;
end;



{ TxqIntegerList }

Constructor TxqIntegerList.Create;
Begin
  Inherited Create;
  FList := TList.Create;
End;

Destructor TxqIntegerList.Destroy;
Begin
  FList.Free;
  Inherited;
End;

Procedure TxqIntegerList.Assign( AList: TxqIntegerList );
{$IFNDEF LEVEL6}
Var
  I: Integer;
{$ENDIF}
begin
{$IFDEF LEVEL6}
  FList.Assign( AList.FList );
{$ELSE}
  FList.Clear;
  For I:= 0 to AList.Count-1 do
    FList.Add( AList.FList[I] );
{$ENDIF}
end;

Function TxqIntegerList.Add( Item: Integer ): Integer;
Begin
  result := FList.Add( Pointer( Item ) );
End;

Procedure TxqIntegerList.Clear;
Begin
  FList.Clear;
End;

Procedure TxqIntegerList.Delete( Index: Integer );
Begin
  FList.Delete( Index );
End;

Function TxqIntegerList.GetCount: Integer;
Begin
  result := FList.Count;
End;

Procedure TxqIntegerList.SetCount( Value: Integer );
Begin
  FList.Count := Value;
End;

Function TxqIntegerList.Get( Index: Integer ): Integer;
Begin
  result := Longint( FList[Index] );
End;

Procedure TxqIntegerList.Insert( Index, Value: Integer );
Begin
  FList.Insert( Index, Pointer( Value ) );
End;

Procedure TxqIntegerList.Put( Index, Value: Integer );
Begin
  FList[Index] := Pointer( Value );
End;

Function TxqIntegerList.IndexofValue( Item: Integer ): Integer;
Begin
  Result := FList.IndexOf( Pointer( item ) );
End;

Function TxqIntegerList.GetCapacity: Integer;
Begin
  result := FList.Capacity;
End;

Procedure TxqIntegerList.SetCapacity( Value: Integer );
Begin
  FList.Capacity := Value;
End;

Procedure TxqIntegerList.Sort;

  Procedure QuickSort( L, R: Integer );
  Var
    I, J: Integer;
    P, T: Integer;
  Begin
    Repeat
      I := L;
      J := R;
      P := Longint( FList[( L + R ) Shr 1] );
      Repeat
        While Longint( FList[I] ) < P Do
          Inc( I );
        While Longint( FList[J] ) > P Do
          Dec( J );
        If I <= J Then
        Begin
          T := Longint( FList[I] );
          FList[I] := Pointer( Longint( FList[J] ) );
          FList[J] := Pointer( T );
          Inc( I );
          Dec( J );
        End;
      Until I > J;
      If L < J Then
        QuickSort( L, J );
      L := I;
    Until I >= R;
  End;

Begin
  If FList.Count > 1 Then
    QuickSort( 0, FList.Count - 1 );
End;

procedure TxqIntegerList.LoadFromStream( Stream: TStream );
var
  I, N, Value: Integer;
Begin
  FList.Clear;
  with Stream do
  begin
    Read(N,SizeOf(N));
    for I:= 1 to N do
    Begin
      Read( Value, SizeOf(Value));
      FList.Add( Pointer(Value) );
    End;
  end;
End;

Procedure TxqIntegerList.SaveToStream( Stream: TStream );
var
  I, N, Value: Integer;
Begin
  N:= FList.Count;
  with Stream do
  begin
    Write(N,SizeOf(N));
    for I:= 0 to FList.Count-1 do
    Begin
      Value:= Longint( FList[I] );
      Write( Value, SizeOf(Value));
    End;
  end;
End;

Procedure TxqIntegerList.LoadFromFile( const FileName: string );
var
  s: TStream;
Begin
  if Not FileExists( FileName ) then Exit;
  s:= TFileStream.Create( FileName, fmOpenRead or fmShareDenyNone );
  Try
    LoadFromStream( s );
  Finally
    s.Free;
  End;
End;

Procedure TxqIntegerList.SaveToFile( const FileName: string );
var
  s: TStream;
Begin
  s:= TFileStream.Create( FileName, fmCreate );
  Try
    SaveToStream( s );
  Finally
    s.Free;
  End;
End;

function TxqIntegerList.Find(Value: Integer; var Index: Integer): Boolean;
var
  nLow: Integer;
  nHigh: Integer;
  nCheckPos: Integer;
begin
  nLow:= 0;
  nHigh:= FList.Count-1;
  Index := -1;
  Result:=False;
  // keep searching until found or
  // no more items to search
  while nLow <= nHigh do
  begin
     nCheckPos := (nLow + nHigh) div 2;
     if Longint(FList[nCheckPos]) < Value then       // less than
        nHigh := nCheckPos - 1
     else if Longint(FList[nCheckPos]) > Value then  // greater than
        nLow := nCheckPos + 1
      else                                  // equal to
      begin
        Index:= nCheckPos;
        Result:= true;
        Exit;
      end;
  end;
end;

procedure TxqIntegerList.Reindex;
Var
  I, n, Last: Integer;
begin
  { this method is only used in conjunction with a TEzVector Parts property }
  If FList.Count = 0 Then Exit;
  { it is needed to reindex with repeated values }
  n := 0;
  Last := Longint( FList.Items[0] );
  I := 0;
  While I <= FList.Count - 1 Do
  Begin
    If Last <> Longint( FList.Items[I] ) Then
    Begin
      Inc( n );
      Last := Longint( FList.Items[I] );
    End;
    FList.Items[I] := Pointer( n );
    Inc( I );
  End;
end;


End.
