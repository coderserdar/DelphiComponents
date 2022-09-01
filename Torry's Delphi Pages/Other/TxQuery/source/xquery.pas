{**************************************************************************}
{   TxQuery DataSet                                                        }
{                                                                          }
{   Copyright (C) <1999-2003> of                                           }
{   Alfonso Moreno (Hermosillo, Sonora, Mexico)                            }
{   email: luisarvayo@yahoo.com                                            }
{     url: http://www.ezsoft.com                                           }
{          http://www.sigmap.com/txquery.htm                               }
{                                                                          }
{   Open Source patch review (2009) with permission from Alfonso Moreno by }
{   Chee-Yang CHAU and Sherlyn CHEW (Klang, Selangor, Malaysia)            }
{   email: cychau@gmail.com                                                }
{   url: http://code.google.com/p/txquery/                                 }
{        http://groups.google.com/group/txquery                            }
{                                                                          }
{   This program is free software: you can redistribute it and/or modify   }
{   it under the terms of the GNU General Public License as published by   }
{   the Free Software Foundation, either version 3 of the License, or      }
{   (at your option) any later version.                                    }
{                                                                          }
{   This program is distributed in the hope that it will be useful,        }
{   but WITHOUT ANY WARRANTY; without even the implied warranty of         }
{   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          }
{   GNU General Public License for more details.                           }
{                                                                          }
{   You should have received a copy of the GNU General Public License      }
{   along with this program.  If not, see <http://www.gnu.org/licenses/>.  }
{                                                                          }
{**************************************************************************}

Unit xquery;

{$I XQ_FLAG.INC}
Interface

Uses
  SysUtils, Windows, Classes, Controls, Forms, Db,
  XQMiscel, xqbase, Qlexlib, Qyacclib, Qbaseexpr, QExprYacc, XQJoins
{$IFDEF LEVEL3}
  , DBTables
{$ENDIF}
{$IFDEF LEVEL6}
  , Variants
{$ENDIF}
{$if RTLVersion >= 20}
  , Generics.Collections
{$ifend}
  ;

Type

  {-------------------------------------------------------------------------------}
  {                  Define forward declarations and class definitions            }
  {-------------------------------------------------------------------------------}

  TResultSet = Class;
  TxqFields = Class;
  TCustomxQuery = Class;
  TDataSetClass = Class Of TDataSet;
  TSqlAnalizer = Class;

  {-------------------------------------------------------------------------------}
  {                  Define TxqField                                              }
  {-------------------------------------------------------------------------------}

  TxqField = Class
  Private
    FFields: TxqFields; { the list of fields that this field belongs to                                  }
    FFieldName: String; { the column name sample: Customer.Addr1                                         }
    FAlias: String; { the alias of the field: Customer.Addr1 main_address                            }
    FDataType: TExprType; { the data type (ttstring, ttFloat, ttInteger, ttBoolean)                        }
    FDataSize: Word; { calculated datasize for ttstring (used in TxQuery)                             }
    FBufferOffset: Integer; { offset in the list of buffers for every record                                 }
    FFieldOffset: Integer; { Offset in the buffer in TxQuery dataset                                        }
    FReadOnly: Boolean; { = true, means that comes from single field, False = expression or joined field }
    FSourceField: TField; { Field that originated this column, =nil if it is an expression                 }
    FCastType: Word; { field must be casted to this type on creation                                  }
    FCastLen: Word; { field must be of this len if CastType = RW_CHAR                                }
    FUseDisplayLabel: Boolean; { true = use labels from SrcField.DisplayLabel for column alias                   }
    FFieldNo: Integer; { the number of the field, in base 1                                             }
    Function GetData(Buffer: Pointer): Boolean;
    Procedure SetData(Buffer: Pointer);
    Function GetColWidth: Integer;
    Function GetIsNull: Boolean;
    { LAS : 5/JUN/2003 }
    Procedure SetIsNull;
  Protected
    Function GetAsVariant: Variant; Virtual;
    Procedure SetAsVariant(const Value: Variant);
    Procedure SetVarValue(const Value: Variant); Virtual; Abstract;
    Function GetAsstring: String; Virtual;
    Procedure SetAsstring(Const Value: String); Virtual;
    Function GetAsFloat: double; Virtual;
    Procedure SetAsFloat(Value: double); Virtual;
    Function GetAsInteger: Longint; Virtual;
    Procedure SetAsInteger(Value: Longint); Virtual;
    Function GetAsBoolean: Boolean; Virtual;
    Procedure SetAsBoolean(Value: Boolean); Virtual;
    Procedure SetDataType(Value: TExprType);
  Public
    Constructor Create(Fields: TxqFields; FieldNo: Integer); Virtual;
    Procedure Clear; Virtual; Abstract;

    Property FieldName: String Read FFieldName Write FFieldName;
    Property Alias: String Read FAlias Write FAlias;
    Property FieldNo: Integer Read FFieldNo;
    Property DataType: TExprType Read FDataType Write FDataType;
    Property DataSize: Word Read FDataSize Write FDataSize;
    Property ReadOnly: Boolean Read FReadOnly Write FReadOnly;
    Property FieldOffset: Integer Read FFieldOffset Write FFieldOffset;
    Property SourceField: TField Read FSourceField Write FSourceField;
    Property CastType: Word Read FCastType Write FCastType;
    Property CastLen: Word Read FCastLen Write FCastLen;
    Property ColWidth: Integer Read GetColWidth;
    Property BufferOffset: Integer Read FBufferOffset Write FBufferOffset;
    Property UseDisplayLabel: Boolean Read FUseDisplayLabel Write FUseDisplayLabel;

    Property AsVariant: Variant Read GetAsVariant Write SetAsVariant;
    Property Asstring: String Read GetAsstring Write SetAsstring;
    Property AsFloat: double Read GetAsFloat Write SetAsFloat;
    Property AsInteger: Longint Read GetAsInteger Write SetAsInteger;
    Property AsBoolean: Boolean Read GetAsBoolean Write SetAsBoolean;
    Property IsNull: Boolean Read GetIsNull;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TxqStringField                                        }
  {-------------------------------------------------------------------------------}

  TxqStringField = Class(TxqField)
  Private
    Function GetValue(Var Value: String): Boolean;
  Protected
    Function GetAsVariant: Variant; Override;
    Procedure SetVarValue(const Value: Variant); Override;
    Function GetAsstring: String; Override;
    Procedure SetAsstring(Const Value: String); Override;
    Function GetAsFloat: double; Override;
    Procedure SetAsFloat(Value: double); Override;
    Function GetAsInteger: Longint; Override;
    Procedure SetAsInteger(Value: Longint); Override;
    Function GetAsBoolean: Boolean; Override;
    Procedure SetAsBoolean(Value: Boolean); Override;
  Public
    Constructor Create(Fields: TxqFields; FieldNo: Integer); Override;
    Procedure Clear; Override;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TxqFloatField                                         }
  {-------------------------------------------------------------------------------}

  TxqFloatField = Class(TxqField)
  Private
  Protected
    Function GetAsVariant: Variant; Override;
    Procedure SetVarValue(const Value: Variant); Override;
    Function GetAsFloat: double; Override;
    Function GetAsInteger: Longint; Override;
    Function GetAsstring: String; Override;
    Procedure SetAsFloat(Value: double); Override;
    Procedure SetAsInteger(Value: Longint); Override;
    Procedure SetAsstring(Const Value: String); Override;
  Public
    Constructor Create(Fields: TxqFields; FieldNo: Integer); Override;
    Procedure Clear; Override;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TxqIntegerField                                       }
  {-------------------------------------------------------------------------------}

  TxqIntegerField = Class(TxqField)
  Private
  Protected
    Function GetAsVariant: Variant; Override;
    Procedure SetVarValue(const Value: Variant); Override;
    Function GetAsFloat: double; Override;
    Function GetAsInteger: Longint; Override;
    Function GetAsstring: String; Override;
    Procedure SetAsFloat(Value: double); Override;
    Procedure SetAsInteger(Value: Longint); Override;
    Procedure SetAsstring(Const Value: String); Override;
  Public
    Constructor Create(Fields: TxqFields; FieldNo: Integer); Override;
    Procedure Clear; Override;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TxqBooleanField                                       }
  {-------------------------------------------------------------------------------}

  TxqBooleanField = Class(TxqField)
  Private
  Protected
    Function GetAsVariant: Variant; Override;
    Procedure SetVarValue(const Value: Variant); Override;
    Function GetAsBoolean: Boolean; Override;
    Function GetAsstring: String; Override;
    Procedure SetAsBoolean(Value: Boolean); Override;
    Procedure SetAsstring(Const Value: String); Override;
  Public
    Constructor Create(Fields: TxqFields; FieldNo: Integer); Override;
    Procedure Clear; Override;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TxqFields                                             }
  {-------------------------------------------------------------------------------}

  TxqFields = Class
    FResultSet: TResultSet;
    FItems: TList;
    Function GetCount: Integer;
    Function GetItem(Index: Integer): TxqField;
  Public
    Constructor Create(ResultSet: TResultSet);
    Destructor Destroy; Override;
    Function Add(DataType: TExprType): TxqField;
    Procedure Clear;
    Procedure Delete(Index: Integer);
    Function FindField(Const FieldName: String): TxqField;

    Property Count: Integer Read GetCount;
    Property Items[Index: Integer]: TxqField Read GetItem; Default;
    Property ResultSet: TResultSet Read FResultSet;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define abstract TResultSet                                   }
  {-------------------------------------------------------------------------------}

  TResultSet = Class
  Private
    FFields: TxqFields;
    FRecNo: Integer;
    FRecordBufferSize: Integer;
    FSourceDataSet: TDataSet;
    FIsSequenced: Boolean;
    FBookmarkSize: integer;                         { patched by ccy }
    procedure CheckRecordBufferSize(DataSet: TDataSet); { patched by ccy }
  Protected
    Function GetFieldData(Field: TxqField; Buffer: Pointer): Boolean; Virtual;
    Procedure SetFieldData(Field: TxqField; Buffer: Pointer); Virtual; Abstract;
    Function GetIsNull(Field: TxqField): Boolean; Virtual;
    { LAS : 5/JUN/2003 }
    Procedure SetIsNull(Field: TxqField); Virtual;
    Procedure SetRecno(Value: Integer);
    Function GetRecno: Integer;
    Function GetRecordCount: Integer; Virtual;
    Procedure SortWithList(SortList: TxqSortList); Virtual; Abstract;
    Procedure ClearBufferList; Virtual; Abstract;
    procedure SetSourceDataSet(DataSet: TDataSet); { patched by ccy }
  Public
    { methods }
    Constructor Create;
    Destructor Destroy; Override;
    Procedure AddField(Const pFieldName, pAlias: String; pDataType: TExprType;
      pDataSize: Integer; pField: TField; pReadOnly: Boolean;
      pCastType: Integer; pCastLen: Integer; pUseDisplayLabel: Boolean);
    Procedure Insert; Virtual; Abstract;
    Procedure Delete; Virtual; Abstract;
    Function FindField(Const FieldName: String): TxqField;
    Function FieldByName(Const FieldName: String): TxqField;
    Procedure Clear; Virtual;
    Procedure SaveToText(Const FileName: String); // debuggin purposes
    Procedure SetSourceBookmark(Bookmark: TBookmark); Virtual; Abstract;
    Function GetSourceBookmark: TBookmark; Virtual; Abstract;
    Procedure FreeSourceBookmark; Virtual; Abstract;

    { properties }
    Property IsSequenced: Boolean Read FIsSequenced Write FIsSequenced;
    Property SourceDataSet: TDataSet Read FSourceDataSet Write SetSourceDataSet; { patched by ccy }
    Property Recno: Integer Read GetRecno Write SetRecno;
    Property RecordCount: Integer Read GetRecordCount;
    Property Fields: TxqFields Read FFields;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TMemResultSet                                         }
  {-------------------------------------------------------------------------------}

  TxBuffer = {$if RtlVersion <= 18.5}PAnsiChar{$else}TBytes{$ifend}; { patched by ccy }

  TMemResultSet = Class(TResultSet)
  Private
    FBufferList: TList{$if RtlVersion>=20}<TxBuffer>{$ifend}; { patched by ccy }
    function ActiveBuffer: TxBuffer; { patched by ccy }
  Protected
    Function GetFieldData(Field: TxqField; Buffer: Pointer): Boolean; Override;
    Procedure SetFieldData(Field: TxqField; Buffer: Pointer); Override;
    Function GetIsNull(Field: TxqField): Boolean; Override;
    { LAS : 5/JUN/2003 }
    Procedure SetIsNull(Field: TxqField); Override;
    Function GetRecordCount: Integer; Override;
    Procedure SortWithList(SortList: TxqSortList); Override;
    Procedure ClearBufferList; Override;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Insert; Override;
    Procedure Delete; Override;
    Procedure Clear; Override;
    Procedure SetSourceBookmark(Bookmark: TBookmark); Override;
    Function GetSourceBookmark: TBookmark; Override;
    Procedure FreeSourceBookmark; Override;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TFileResultSet                                        }
  {-------------------------------------------------------------------------------}

  TFileResultSet = Class(TResultSet)
  Private
    FBufferList: TList;
    FMemMapFile: TMemMapFile;
    FTmpFile: String;
    FBuffer: PChar;
    Function ActiveBuffer: PChar;
  Protected
    Function GetFieldData(Field: TxqField; Buffer: Pointer): Boolean; Override;
    Procedure SetFieldData(Field: TxqField; Buffer: Pointer); Override;
    Function GetIsNull(Field: TxqField): Boolean; Override;
    { LAS : 5/JUN/2003 }
    Procedure SetIsNull(Field: TxqField); Override;
    Function GetRecordCount: Integer; Override;
    Procedure SortWithList(SortList: TxqSortList); Override;
    Procedure ClearBufferList; Override;
  Public
    Constructor Create(MapFileSize: Longint);
    Destructor Destroy; Override;
    Procedure Insert; Override;
    Procedure Delete; Override;
    Procedure Clear; Override;
    Procedure SetSourceBookmark(Bookmark: TBookmark); Override;
    Function GetSourceBookmark: TBookmark; Override;
    Procedure FreeSourceBookmark; Override;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TSqlAnalizer                                          }
  {-------------------------------------------------------------------------------}

  TSqlAnalizer = Class(TObject)
  Private
    FParentAnalizer: TSqlAnalizer; { the parent analizer if this is a subquery       }
    FResultSet: TResultSet; { the Result Set                                  }
    FParams: TParams; { Params passed from TxQuery                      }
    FStatement: TSqlStatement; { The statement: SELECT, UPDATE, etc.             }
    FxQuery: TCustomxQuery; { Linked to this TxQuery                          }
    FDefDataSet: TDataSet; { the default TDataSet                            }
    FColumnList: TColumnList; { SELECT                                          }
    FIsDistinct: Boolean; { syntax: SELECT DISTINCT                         }
    FTableList: TTableList; { FROM                                            }
    FJoinList: TJoinOnList; { JOIN ON                                         }
    FLJoinCandidateList: TStringList; { candidates for converting to join               }
    FRJoinCandidateList: TStringList; {                                                 }
    FWhereStr: String; { WHERE clause expression                         }
    FIsJoinInWhere: Boolean; { JOINing in a where clause                       }
    FJoinInWhereExpres: String;
    FJoinInWhereResolver: TExprParser;
    FMainWhereResolver: TExprParser; { WHERE expression resolver class                 }
    FWhereOptimizeList: TWhereOptimizeList; { used for optimizing the Result set generation}
    FOrderByList: TOrderByList; { ORDER BY list                                   }
    FGroupByList: TOrderByList; { GROUP BY list                                   }
    FHavingCol: Integer; { HAVING predicate                                }
    FSubQueryList: TList; { Subqueries in the expression                    }
    FSubQueryKindList: TList; {                                                 }
    FDoSelectAll: Boolean; { syntax: SELECT * FROM...                        }
    FTableAllFields: TStringList; { syntax: SELECT customer.* FROM customer;        }
    FUpdateColumnList: TUpdateList; { UPDATE statement                                }
    FInsertList: TInsertList; { INSERT statement                                }
    FCreateTableList: TCreateTableList; { CREATE TABLE statement                          }
    FAlterTableList: TCreateTableList; { ALTER TABLE statement                          }
    FIndexUnique: Boolean; { CREATE INDEX, DROP TABLE, DROP INDEX statements }
    FIndexDescending: Boolean; {                                                 }
    FIndexColumnList: TStringList; {                                                 }
    FIndexName: String; {                                                 }
    FIndexTable: String; {                                                 }
    FPivotStr: String; { syntax: TRANSFORM...PIVOT                       }
    FPivotInList: TStringList;
    FTransformColumnList: TColumnList; { LAS 25:07:2000 }
    FTransfBaseColumns: Integer;
    FTransfGroupedColumns: Integer;
    FTransfResultSet: TResultSet; { used in the TRANSFORM...PIVOT}
    FUnionAnalizer: TSqlAnalizer; { syntax select_statement UNION second_select_statement }
    FWhereFilter: String; { used to check if a WHERE clause can be filtered }
    FSubqueryInPivotPredicate: Boolean; { syntax: 	transform count(it.AutoId)
    select 'No. of Items' as Items from pe, it
    where  (pe.Title=it.ItDateM)
    and (pe.ReportId=:REPID)
       pivot pe.Title in (select pe.Title by pe order by pe.Title)
  }
    FIntoTable: String; { select * from customer INTO newcust}
    FUserDefinedRange: TUserDefinedRange;
    FWhereContainsOnlyBasicFields: Boolean;
    FTopNInSelect: Integer;
    FTopNInGroupBy: Integer;

    { parameters for lex / yacc follows }
    FParser: TCustomParser;
    FLexer: TCustomLexer;
    FInputStream: TMemoryStream;
    FOutputStream: TMemoryStream;
    FErrorStream: TMemoryStream;

    Function StripFs(Const Ident: String): String;
    Function IsValidFieldName(Const FieldName: String; CheckInPrimary: Boolean): Boolean;
    Function GetRealTableAlias(Const tn: String): String;
    Function CreateSortList(UsingBookmark: Boolean): TxqSortList;
    Procedure SafeCreateResultSet;
    Procedure CreateResultSet;
    Procedure InitializeResultSet;
    Function HasDistinctAggregates: Boolean;
    { define the procedure for the Result set creation}
    Procedure DoJoinInWhere;
    Procedure DoInsertStatement;
    Procedure DoUpdateRecord;
    Procedure DoGroupBy;
    Procedure DoOrderBy(TheOrderList: TOrderByList);
    Procedure DoTransform;
    Procedure DoUnion;
    Procedure FreeSubQueries(Var SubQ: TSqlAnalizer);
    Procedure DoIntoTableOperation;
    Procedure DoMassiveUpdates;
  Protected
    Function GetResultSet: TResultSet;
    Procedure SetResultSet(Value: TResultSet);
  Public
    Constructor Create(ParentAnalizer: TSqlAnalizer; XQuery: TCustomxQuery);
    Destructor Destroy; Override;
    Procedure ClearSQL;
    Function ReplaceParams(const SQL: string): String;
    Function HasAggregates: Boolean;
    Function FindDataSetByName(Const Name: String): TDataSet;
    Function FindFieldByName(Const FieldName: String): TField;
    Function QualifiedField(Const FieldName: String; UseAlias: Boolean): String;
    Function CheckIntegrity: Boolean;
    Procedure doSelect;
    Procedure doExecSQL;
    Procedure AddFieldIfNot(Const fieldName: String);
    { methds mainly used internally }
    Procedure AddThisRecord(Dataset: TDataset);
    Function GetRealTableName(Const tn: String; Var Index: Integer): String;

    Property xQuery: TCustomxQuery Read FxQuery Write FxQuery;
    Property Params: TParams Read FParams;
    Property Statement: TSqlStatement Read FStatement Write FStatement;
    Property DefDataSet: TDataSet Read FDefDataSet Write FDefDataSet;
    Property ColumnList: TColumnList Read FColumnList;
    Property IsDistinct: Boolean Read FIsDistinct Write FIsDistinct;
    Property TableList: TTableList Read FTableList;
    Property JoinList: TJoinOnList Read FJoinList;
    Property LJoinCandidateList: TStringList Read FLJoinCandidateList;
    Property RJoinCandidateList: TStringList Read FRJoinCandidateList;
    Property WhereStr: String Read FWhereStr Write FWhereStr;
    Property WhereOptimizeList: TWhereOptimizeList Read FWhereOptimizeList;
    Property OrderByList: TOrderByList Read FOrderByList;
    Property GroupByList: TOrderByList Read FGroupByList;
    Property HavingCol: Integer Read FHavingCol Write FHavingCol;
    Property SubQueryList: TList Read FSubQueryList;
    Property SubQueryKindList: TList Read FSubQueryKindList;
    Property doSelectAll: Boolean Read FDoSelectAll Write FDoSelectAll;
    Property TableAllFields: TStringList Read FTableAllFields;
    Property InsertList: TInsertList Read FInsertList;
    Property UpdateColumnList: TUpdateList Read FUpdateColumnList;
    Property CreateTableList: TCreateTableList Read FCreateTableList Write FCreateTableList;
    Property AlterTableList: TCreateTableList Read FAlterTableList Write FAlterTableList;
    Property IndexUnique: Boolean Read FIndexUnique Write FIndexUnique;
    Property IndexDescending: Boolean Read FIndexDescending Write FIndexDescending;
    Property IndexColumnList: TStringList Read FIndexColumnList Write FIndexColumnList;
    Property IndexName: String Read FIndexName Write FIndexName;
    Property IndexTable: String Read FIndexTable Write FIndexTable;
    Property ResultSet: TResultSet Read GetResultSet Write SetResultSet;
    Property PivotStr: String Read FPivotStr Write FPivotStr;
    Property PivotInList: TStringList Read FPivotInList;
    Property TransformColumnList: TColumnList Read FTransformColumnList;
    Property UnionAnalizer: TSqlAnalizer Read FUnionAnalizer Write FUnionAnalizer;
    Property ParentAnalizer: TSqlAnalizer Read FParentAnalizer Write FParentAnalizer;
    Property SubqueryInPivotPredicate: Boolean Read FSubqueryInPivotPredicate Write FSubqueryInPivotPredicate;
    Property IntoTable: String Read FIntoTable Write FIntoTable;
    Property UserDefinedRange: TUserDefinedRange Read FUserDefinedRange;
    Property WhereContainsOnlyBasicFields: Boolean Read FWhereContainsOnlyBasicFields;
    Property MainWhereResolver: TExprParser Read FMainWhereResolver;
    Property TopNInSelect: Integer read FTopNInSelect write FTopNInSelect;
    Property TopNInGroupBy: Integer read FTopNInGroupBy write FTopNInGroupBy;

    { lex / yacc information }
    Property Parser: TCustomParser Read FParser;
    Property Lexer: TCustomLexer Read FLexer;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TDataSetItem                                          }
  {-------------------------------------------------------------------------------}

  TxDataSetItem = Class(TCollectionItem)
  Private
    FDataSet: TDataSet;
    FAlias: String;
    FTemporal: Boolean;
    Procedure SetDataSet(Value: TDataSet);
    Procedure SetAlias(Const Value: String);
  Protected
    Function GetDisplayName: String; Override;
    Function GetCaption: String;
  Public
    Procedure Assign(Source: TPersistent); Override;
    Property Temporal: boolean Read FTemporal Write FTemporal;
  Published
    Property Alias: String Read FAlias Write SetAlias;
    Property DataSet: TDataSet Read FDataSet Write SetDataSet;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TxDataSets                                            }
  {-------------------------------------------------------------------------------}

  TxDataSets = Class(TOwnedCollection)
  Private
    FxQuery: TCustomxQuery;
    FDataSetClass: TDataSetClass;
    Function GetItem(Index: Integer): TxDataSetItem;
    Procedure SetItem(Index: Integer; Value: TxDataSetItem);
  Public
    Constructor Create(AOwner: TPersistent);
    Function IndexOFAlias(Const Alias: String): Integer; { LAS : 05-30-2000 }
    Function Add: TxDataSetItem;
    Property Items[Index: Integer]: TxDataSetItem Read GetItem Write SetItem; Default;
    Property DataSetClass: TDataSetClass Read FDataSetClass Write FDataSetClass;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define Events in TCustomxQuery                               }
  {-------------------------------------------------------------------------------}

  TOptimizeMethod = (omNone, omSetFilter, omSetRange);

  TUDFCheckEvent = Procedure(Sender: tobject;
    Const Identifier: String; Params: TParameterList;
    Var DataType: TExprType; Var MaxLen: Integer;
    Var Accept: Boolean) Of Object;

  TUDFSolveEvent = Procedure(Sender: tobject;
    Const Identifier: String; Params: TParameterList;
    Var Value: variant) Of Object;

  TIndexNeededForEvent = Procedure(Sender: tobject;
    DataSet: TDataSet;
    Const FieldNames: String;
    ActivateIndex: Boolean;
    IsJoining: Boolean;
    Var Accept: Boolean) Of Object;

  TSetRangeEvent = Procedure(Sender: tobject;
    RelOperator: TRelationalOperator;
    DataSet: TDataSet;
    Const FieldNames, StartValues, EndValues: String;
    IsJoining: Boolean) Of Object;

  TCancelRangeEvent = Procedure(Sender: tobject;
    DataSet: TDataSet;
    IsJoining: Boolean) Of Object;

  TSetFilterEvent = Procedure(Sender: tobject;
    DataSet: TDataSet;
    Const Filter: String;
    IsJoining: Boolean;
    Var Handled: Boolean) Of Object;

  TCancelFilterEvent = Procedure(Sender: Tobject;
    DataSet: TDataSet; IsJoining: Boolean) Of Object;

  TBlobNeededEvent = Procedure(Sender: tobject;
    DataSet: TDataSet;
    Var Accept: Boolean) Of Object;

  TxProgressStatus = (psXStart, psXProgress, psXEnd);

  TxProgressEvent = Procedure(Sender: tobject;
    Status: TXProgressStatus;
    Min, Max, Position: Integer) Of Object;

  TCreateTableEvent = Procedure(Sender: tobject;
    CreateTable: TCreateTableItem) Of Object;

  TCreateIndexEvent = Procedure(Sender: tobject;
    Unique, Descending: Boolean;
    Const TableName, IndexName: String;
    ColumnExprList: TStringList) Of Object;

  TDropTableEvent = Procedure(Sender: TObject;
    Const TableName: String) Of Object;

  TDropIndexEvent = Procedure(Sender: TObject;
    Const TableName, IndexName: String) Of Object;

  TSyntaxErrorEvent = Procedure(Sender: tobject;
    Const ErrorMsg, OffendingText: String;
    LineNum, ColNum, TextLen: Integer) Of Object;

  TCancelQueryEvent = Procedure(Sender: TObject;
    Var Cancel: Boolean) Of Object;

  TResolveDatasetEvent = Procedure(Sender: TObject;
    Const Filename: String;
    Var ATableName: String;
    Var Dataset: TDataset) Of Object;

  TQueryScriptErrorEvent = Procedure(Sender: TObject;
    E: Exception;
    Var AbortScript: Boolean) Of Object;

  TQueryFieldNameEvent = Procedure(Sender: TObject;
    FieldIndex: Integer;
    Var FieldName: String) Of Object;

  TSetUserRangeEvent = Procedure(Sender: TObject;
    Dataset: TDataset;
    Const UsingIndex: String;
    ForFields, StartValues, EndValues: TStrings) Of Object;

  TCancelUserRangeEvent = Procedure(Sender: TObject;
    Dataset: TDataset) Of Object;

  {-------------------------------------------------------------------------------}
  {                  Define Bookmark information                                  }
  {-------------------------------------------------------------------------------}

  PRecInfo = ^TRecInfo;
  TRecInfo = Record
    RecordNo: Integer;
    BookmarkFlag: TBookmarkFlag;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TCustomxQuery dataset                                 }
  {-------------------------------------------------------------------------------}

  {$if RTLVersion <= 18.5}TRecordBuffer = PChar; {$ifend}

  TCustomxQuery = Class(TDataSet)
  Private
    { Data }
    FDataSets: TxDataSets; {  list of queried datasets               }
    FAllSequenced: Boolean; {  Means all datasets accepts RecNo prop. }
    FResultSet: TResultSet; {  the abstract Result set                }
    FRecordCount: Integer; {  current number of record               }
    FRecordSize: Integer; {  the size of the actual data            }
    FRecordBufferSize: Integer; {  data + housekeeping(TRecInfo)          }
    FRecordInfoOffset: Integer; {  offset of RecInfo in record buffer     }
    FRecNo: Integer; {  current record(0 to FRecordCount - 1)  }
    BofCrack: Integer; {  before the first record(crack)         }
    EofCrack: Integer; {  after the last record(crack)           }
    StartCalculated: Integer;
    FIsOpen: Boolean;
    FReadOnly: Boolean;
    FSQL: Tstrings;
    FFilterBuffer: TRecordBuffer;
    FFilterExpr: TExprParser;
    FPrepared: Boolean;
    FParams: TParams;
    FParamsAsFields: TParamsAsFields;
    FParamCheck: Boolean;
    FDataLink: TDataLink;
    FAutoDisableControls: Boolean;
    FUseDisplayLabel: Boolean;
    FInMemResultSet: Boolean;
    FDateFormat: String;
    FDisabledDataSets: TList;
    FMapFileSize: Longint; { Temporary file max size in bytes     }
    { script section }
    FScriptStatementType: TSQLStatement;
    FScriptIsRunning: Boolean;
    FSQLScript: Tstrings;
    FShowWaitCursor: Boolean;
    FResultSetIsDefined: Boolean;
    FRowsAffected: Integer;
    FWhereOptimizeMethod: TOptimizeMethod;
    FRefFields: TStrings;
    FWithDummies: Boolean;
    FActiveDataSetEvents: Boolean;           // Nonn

    { events }
    FOnUDFCheck: TUDFCheckEvent;
    FOnUDFSolve: TUDFSolveEvent;
    FOnProgress: TXProgressEvent;
    FOnIndexNeededFor: TIndexNeededForEvent;
    FOnSetRange: TSetRangeEvent;
    FOnCancelRange: TCancelRangeEvent;
    FOnBlobNeeded: TBlobNeededEvent;
    FOnBeforeQuery: TNotifyEvent;
    FOnAfterQuery: TNotifyEvent;
    FOnCreateTable: TCreateTableEvent;
    FOnAlterTable: TCreateTableEvent;
    FOnCreateIndex: TCreateIndexEvent;
    FOnDropTable: TDropTableEvent;
    FOnDropIndex: TDropIndexEvent;
    FOnSyntaxError: TSyntaxErrorEvent;
    FOnSetFilter: TSetFilterEvent;
    FOnCancelFilter: TCancelFilterEvent;
    FOnCancelQuery: TCancelQueryEvent;
    FOnResolveDataset: TResolveDatasetEvent;
    FOnScriptError: TQueryScriptErrorEvent;
    FOnQueryFieldName: TQueryFieldNameEvent;
    FOnSetUserRange: TSetUserRangeEvent;
    FOnCancelUserRange: TCancelUserRangeEvent;

    Procedure SetParamsAsFields(Value: TParamsAsFields);
    Function GetFieldSize(FieldType: TFieldType; Size: longint): longint;
    function GetActiveRecordBuffer: TRecordBuffer;
    function FilterRecord(Buffer: TRecordBuffer): Boolean;
    Procedure SetQuery(Value: Tstrings);
    Procedure SetSQLScript(Value: Tstrings);
    Function GetAbout: String;
    Procedure SetAbout(Const Value: String);
    Procedure SetFilterData(Const Text: String);
    Procedure QueryChanged(Sender: tobject);
    Function GetPrepared: Boolean;
    Procedure SetPrepare(Value: Boolean);
    Function GetParamsCount: Word;
    Procedure SetParamsList(Value: TParams);
    Procedure SetDataSource(Value: TDataSource);
    Procedure RefreshParams;
    Procedure SetParamsFromDataSet;
{$IFDEF level4}
    Procedure ReadParamData(Reader: TReader);
    Procedure writeParamData(writer: Twriter);
{$ENDIF}
    Procedure FixDummiesForQuerying(Var Filter: String);
    Procedure ClearTempDatasets;
    Function LocateRecord(Const KeyFields: String; Const KeyValues: Variant;
      Options: TLocateOptions): Integer;
    Procedure SetDataSets(Value: TxDatasets);
  Protected
    Procedure InternalEdit; Override; // editing
    Procedure SetFieldData(Field: TField; Buffer: Pointer); Override; // editing
    Procedure InternalRefresh; Override;
    Function GetDataSource: TDataSource; Override;
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;

    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; doCheck: Boolean):
        TGetResult; override;
    Function GetRecordSize: Word; Override;

    Function BCDToCurr(BCD: Pointer; Var Curr: Currency): Boolean;
{$IFNDEF LEVEL5} Override;
{$ENDIF}
    Function CurrToBCD(Const Curr: Currency; BCD: Pointer; Precision, Decimals:
      Integer): Boolean;
{$IFNDEF LEVEL5} Override;
{$ENDIF}

    Procedure InternalClose; Override;
    Procedure InternalFirst; Override;
    Procedure InternalGotoBookmark(Bookmark: Pointer); Override;
    Function InternalBookmarkValid(Bookmark: Pointer): boolean;

    Procedure InternalInitFieldDefs; Override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    Procedure InternalLast; Override;
    Procedure InternalOpen; Override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    Function IsCursorOpen: Boolean; Override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
        override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    Function GetCanModify: Boolean; Override;
    procedure ClearCalcFields(Buffer: TRecordBuffer); override;
    Function GetRecordCount: Integer; Override;
    Procedure SetRecNo(Value: Integer); Override;
    Function GetRecNo: Integer; Override;
    Procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); Override;
    Procedure InternalDelete; Override;
    procedure InternalInsert; Override;
    Procedure InternalHandleException; Override;
    Procedure InternalPost; Override;
    Procedure Notification(AComponent: TComponent; Operation: toperation); Override;
    Procedure SetFilterText(Const Value: String); Override;
    Procedure SetFiltered(Value: Boolean); Override;
    Procedure FixDummiesForFilter(Var Filter: String); Dynamic;
{$IFDEF level4}
    Procedure DefineProperties(Filer: TFiler); Override;
{$ENDIF}

{$IFDEF level3}
    Function GetFieldData(Field: TField; Buffer: Pointer): Boolean; Override;
{$ENDIF}

    Procedure PopulateDatasets(TableList: TTableList); Virtual;
    Procedure DisposeDatasets; Virtual; { LAS : 05-30-2000 }
    Procedure PackTable(TableList: TTableList); Virtual;
    Procedure ZapTable(TableList: TTableList); Virtual;
    Procedure ReindexTable(TableList: TTableList); Virtual;

    procedure _ReadRecord(Buffer: TRecordBuffer; IntRecNum: Integer);

    { properties }
    Property AllSequenced: Boolean Read FAllSequenced Write FAllSequenced;

    Property InMemResultSet: Boolean Read FInMemResultSet Write FInMemResultSet Default True;
    Property MapFileSize: Longint Read FMapFileSize Write FMapFileSize Default 2000000;

  Public
    { methods }
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;

{$IFDEF LEVEL4}
    Function GetFieldData(Field: TField; Buffer: Pointer): Boolean; Override;
    Procedure SetBlockReadSize(Value: Integer); Override;
{$ENDIF}
    Function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; Override;
    Function IsSequenced: Boolean; Override;
    Function IsDataSetDisabled(DataSet: TDataSet): Boolean;
    Function Locate(Const KeyFields: String;
      Const KeyValues: Variant;
      Options: TLocateOptions): Boolean; Override;
    Function Lookup(Const KeyFields: String;
      Const KeyValues: Variant;
      Const ResultFields: String): Variant; Override;
    Function BookmarkValid(Bookmark: TBookmark): boolean; Override;
    Function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; Override;

    Function Find(Const Expression: String): Boolean;
    Function ResultSetIsSequenced: Boolean;
    Procedure ExecSQL;
    Procedure Disconnect;
    Procedure Prepare;
    Procedure UnPrepare;
    Function ParamByName(Const Value: String): TParam;

    { not so common procedures }
    Procedure AddDataSet(DataSet: TDataSet; Const Alias: String);
    Procedure ClearDatasets;
    Function DataSetByName(Const Name: String): TDataSet;
    Function SourceDataSet: TDataSet;
    Procedure WriteToTextFile(Const FileName: String;
      FieldDelimChar, TxtSeparator: Char; IsCSV: Boolean; FieldNames: TStringList);
    Procedure ExecSQLScript;
    Procedure SortByColumns(Const Columns: Array Of integer; Descending: Boolean);
    Function SourceBookmark: TBookmark;

    { properties }
    Property DataSets: TxDataSets Read FDataSets Write SetDataSets;
    Property DisabledDataSets: TList read FDisabledDataSets;  { mainly used internally}
    Property ResultSet: TResultSet Read FResultSet;           { mainly used internally}
    Property ParamCount: Word Read GetParamsCount;
    Property Prepared: Boolean Read GetPrepared Write SetPrepare;
    Property ReadOnly: Boolean Read FReadOnly Write FReadOnly Default False;
    Property RecNo: Integer Read GetRecNo Write SetRecNo;
    { scripts }
    Property ScriptStatementType: TSQLStatement Read FScriptStatementType Write FScriptStatementType;
    Property ScriptIsRunning: Boolean Read FScriptIsRunning Write FScriptIsRunning;
    Property ResultSetIsDefined: Boolean Read FResultSetIsDefined Write FResultSetIsDefined;
    Property RowsAffected: Integer Read FRowsAffected Write FRowsAffected;
    Property RefFields: TStrings Read FRefFields;
    Property WithDummies: Boolean read FWithDummies write FWithDummies;

    { new events }
    Property OnIndexNeededFor: TIndexNeededForEvent Read FOnIndexNeededFor Write FOnIndexNeededFor;
    Property OnSetRange: TSetRangeEvent Read FOnSetRange Write FOnSetRange;
    Property OnCancelRange: TCancelRangeEvent Read FOnCancelRange Write FOnCancelRange;
    Property OnBlobNeeded: TBlobNeededEvent Read FOnBlobNeeded Write FOnBlobNeeded;
    Property OnCreateTable: TCreateTableEvent Read FOnCreateTable Write FOnCreateTable;
    Property OnAlterTable: TCreateTableEvent Read FOnAlterTable Write FOnAlterTable;
    Property OnCreateIndex: TCreateIndexEvent Read FOnCreateIndex Write FOnCreateIndex;
    Property OnDropTable: TDropTableEvent Read FOnDropTable Write FOnDropTable;
    Property OnDropIndex: TDropIndexEvent Read FOnDropIndex Write FOnDropIndex;
    Property OnSetFilter: TSetFilterEvent Read FOnSetFilter Write FOnSetFilter;
    Property OnCancelFilter: TCancelFilterEvent Read FOnCancelFilter Write FOnCancelFilter;

  Published
    Property DataSource: TDataSource Read GetDataSource Write SetDataSource;
    Property SQL: Tstrings Read FSQL Write SetQuery;
    Property SQLScript: Tstrings Read FSQLScript Write SetSQLScript;
    Property Params: TParams Read FParams Write SetParamsList Stored False;
    Property ParamCheck: Boolean Read FParamCheck Write FParamCheck Default True;
    Property About: String Read GetAbout Write SetAbout;
    Property AutoDisableControls: Boolean Read FAutoDisableControls Write FAutoDisableControls Default True;
    Property UseDisplayLabel: Boolean Read FUseDisplayLabel Write FUseDisplayLabel Default False;
    Property DateFormat: String Read FDateFormat Write FDateFormat;
    Property ShowWaitCursor: Boolean Read FShowWaitCursor Write FShowWaitCursor Default true;
    Property WhereOptimizeMethod: TOptimizeMethod Read FWhereOptimizeMethod Write FWhereOptimizeMethod Default omSetFilter;
    Property ParamsAsFields: TParamsAsFields read FParamsAsFields write SetParamsAsFields;
    Property ActiveDataSetEvents: Boolean Read FActiveDataSetEvents Write FActiveDataSetEvents Default True;   // Nonn
    { inherited properties }
    Property Active;
    Property Filter;
    Property Filtered;

    { new events }
    Property OnUDFCheck: TUDFCheckEvent Read FOnUDFCheck Write FOnUDFCheck;
    Property OnUDFSolve: TUDFSolveEvent Read FOnUDFSolve Write FOnUDFSolve;
    Property OnProgress: TXProgressEvent Read FOnProgress Write FOnProgress;
    Property OnBeforeQuery: TNotifyEvent Read FOnBeforeQuery Write FOnBeforeQuery;
    Property OnAfterQuery: TNotifyEvent Read FOnAfterQuery Write FOnAfterQuery;
    Property OnSyntaxError: TSyntaxErrorEvent Read FOnSyntaxError Write FOnSyntaxError;
    Property OnCancelQuery: TCancelQueryEvent Read FOnCancelQuery Write FOnCancelQuery;
    Property OnResolveDataset: TResolveDatasetEvent Read FOnResolveDataset Write FOnResolveDataset;
    Property OnScriptError: TQueryScriptErrorEvent Read FOnScriptError Write FOnScriptError;
    Property OnQueryFieldName: TQueryFieldNameEvent Read FOnQueryFieldName Write FOnQueryFieldName;
    Property OnSetUserRange: TSetUserRangeEvent Read FOnSetUserRange Write FOnSetUserRange;
    Property OnCancelUserRange: TCancelUserRangeEvent Read FOnCancelUserRange Write FOnCancelUserRange;

    { inherited events }
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
    Property OnFilterRecord;
    Property OnNewRecord;
    Property OnPostError;
  End;

  {-------------------------------------------------------------------------------}
  {                  Define TxQuery                                               }
  {-------------------------------------------------------------------------------}

  TxQuery = Class(TCustomXQuery)
  Published
    { properties }
    Property DataSets;

    { events }
    Property OnIndexNeededFor;
    Property OnSetRange;
    Property OnCancelRange;
    Property OnBlobNeeded;
    Property OnCreateTable;
    Property OnAlterTable;
    Property OnCreateIndex;
    Property OnDropTable;
    Property OnDropIndex;
    Property OnSetFilter;
    Property OnCancelFilter;
  End;

{$IFDEF XQDEMO}
Procedure ShowAbout;
{$ENDIF}

{$IFDEF DELPHI3}
Const
  ftNonTextTypes = [ftBytes, ftvarBytes, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor];
{$ENDIF}

Implementation

{ zip file password: A01DB36C-7D13-4331-825A-11C5F7D83225
  ftp://www.sigmap.com
  user name: sources
  password: zigumini
}

Uses
{$IFDEF XQDEMO}
  DemoReg,
{$ENDIF}
  xqLex, xqYacc, xqConsts, CnvStrUtils;

Function VarTypeToExprType(Const Value: Variant): TExprType;
Begin
  Case VarType(Value) Of
    varSingle, varDouble, varCurrency, varDate: Result := ttFloat;
    varSmallint, varByte, varInteger: Result := ttInteger;
    varBoolean: Result := ttBoolean;
  Else
    Result := ttstring;
  End;
End;

{-------------------------------------------------------------------------------}
{                  Define as a demo section                                     }
{-------------------------------------------------------------------------------}

{$IFDEF xqdemo}
Var
  IsFirstTime: Boolean = True;

Procedure ShowAbout;
Begin
  With TfrmRegister.Create(Nil) Do
  Begin
    Try
      ShowModal;
    Finally
      free;
    End;
  End;
End;
{$ENDIF}

{-------------------------------------------------------------------------------}
{                  Implements TxqField                                          }
{-------------------------------------------------------------------------------}

Constructor TxqField.Create(Fields: TxqFields; FieldNo: Integer);
Begin
  Inherited Create;
  FFields := Fields;
  FFieldNo := FieldNo;
End;

Function TxqField.GetData(Buffer: Pointer): Boolean;
Begin
  Result := FFields.ResultSet.GetFieldData(Self, Buffer);
End;

Procedure TxqField.SetData(Buffer: Pointer);
Begin
  FFields.ResultSet.SetFieldData(Self, Buffer);
End;

Procedure TxqField.SetDataType(Value: TExprType);
Begin
  FDataType := Value;
End;

Function TxqField.GetIsNull: Boolean;
Begin
  Result := FFields.ResultSet.GetIsNull(Self);
End;

{ LAS : 5/JUN/2003 }
Procedure TxqField.SetIsNull;
Begin
  FFields.ResultSet.SetIsNull(Self);
End;

Function TxqField.GetAsBoolean: Boolean;
Begin
  Raise ExQueryError.Create(SReadBooleanField);
End;

Function TxqField.GetAsFloat: double;
Begin
  Raise ExQueryError.Create(SReadFloatField);
End;

Function TxqField.GetAsInteger: Longint;
Begin
  Raise ExQueryError.Create(SReadIntegerField);
End;

Function TxqField.GetAsstring: String;
Begin
  Raise ExQueryError.Create(SReadstringField);
End;

Procedure TxqField.SetAsstring(Const Value: String);
Begin
  Raise ExQueryError.Create(SwritestringField);
End;

Function TxqField.GetAsVariant: Variant;
Begin
  Raise ExQueryError.Create(SReadstringField);
End;

Procedure TxqField.SetAsVariant(Const Value: Variant);
Begin
  If VarIsNull(Value) Then
    Clear
  Else
    SetVarValue(Value);
End;

Procedure TxqField.SetAsFloat(Value: double);
Begin
  Raise ExQueryError.Create(SwriteFloatField);
End;

Procedure TxqField.SetAsInteger(Value: Longint);
Begin
  Raise ExQueryError.Create(SwriteIntegerField);
End;

Procedure TxqField.SetAsBoolean(Value: Boolean);
Begin
  Raise ExQueryError.Create(SwriteBooleanField);
End;

Function TxqField.GetColWidth: Integer;
Begin
  If Assigned(FSourceField) Then
  Begin
    If FSourceField.DataType In [ftstring{$IFDEF LEVEL4}, ftFixedChar,
    ftWidestring{$ENDIF}
{$IFDEF LEVEL5}, ftGUID{$ENDIF}] Then
      Result := FSourceField.Size
    Else
      Result := FSourceField.DataSize;
  End
  Else
    Result := FDataSize;
End;

{-------------------------------------------------------------------------------}
{                  Implements TxqStringField                                    }
{-------------------------------------------------------------------------------}

Constructor TxqStringField.Create(Fields: TxqFields; FieldNo: Integer);
Begin
  Inherited Create(Fields, FieldNo);
  SetDataType(ttstring);
End;

Procedure TxqStringField.Clear;
Begin
  SetAsstring('');
  SetIsNull;
End;

Function TxqStringField.GetValue(Var Value: String): Boolean; { patched by ccy }
Var
  Buffer: Array[0..dsMaxstringSize] Of Char;
  AnsiBuffer: Array[0..dsMaxstringSize] Of AnsiChar;
  A: AnsiString;                                     
Begin
  if (SourceField = nil) or (SourceField.DataType = ftString) or (SourceField.DataType = ftMemo) then begin
    Result := GetData(@AnsiBuffer);
    If Result Then begin
      A := AnsiBuffer;
      Value := String(A);
    end;
  end else if (SourceField.DataType = ftWideString) or (SourceField.DataType = ftWideMemo) then begin
    Result := GetData(@Buffer);
    If Result Then
      Value := Buffer;
  end else
    Assert(False, 'Unsupported data type in procedure TxqStringField.SetAsstring');
End;

Function TxqStringField.GetAsstring: String;
Begin
  If Not GetValue(Result) Then
    Result := '';
End;

Function TxqStringField.GetAsFloat: double;
Begin
  Result := StrToFloat(GetAsstring);
End;

Function TxqStringField.GetAsVariant: Variant;
Var
  S: String;
Begin
  If GetValue(S) Then Result:= S Else Result:= Null;
End;

Procedure TxqStringField.SetVarValue(const Value: Variant);
Begin
  SetAsString(Value);
End;

Function TxqStringField.GetAsInteger: Longint;
Begin
  Result := StrToInt(GetAsstring);
End;

Function TxqStringField.GetAsBoolean: Boolean;
Var
  S: String;
Begin
  S := GetAsstring;
  Result := (Length(S) > 0) And CharInSet(S[1], ['T', 't', 'Y', 'y']);
End;

Procedure TxqStringField.SetAsstring(Const Value: String); { patched by ccy }
Var
  Buffer: Array[0..dsMaxstringSize] Of Char;
  AnsiBuffer: Array[0..dsMaxstringSize] Of AnsiChar;
  L: Integer;
  A: AnsiString;
Begin
  if (SourceField = nil) or (SourceField.DataType = ftString) or (SourceField.DataType = ftMemo) then begin
    A := AnsiString(Value);
    FillChar(AnsiBuffer, FDataSize, 0);
    L := Length(A);
    StrLCopy(AnsiBuffer, PAnsiChar(A), L);
    SetData(@AnsiBuffer);
  end else if (SourceField.DataType = ftWideString) or (SourceField.DataType = ftWideMemo) then begin
    FillChar(Buffer, FDataSize, 0);
    L := Length(Value);
    Move(Value[1], Buffer, L * SizeOf(WideChar));
    SetData(@Buffer);
  end else
    Assert(False, 'Unsupported data type in procedure TxqStringField.SetAsstring');
End;

Procedure TxqStringField.SetAsFloat(Value: double);
Begin
  SetAsstring(FloatToStr(Value));
End;

Procedure TxqStringField.SetAsInteger(Value: Longint);
Begin
  SetAsstring(IntToStr(Value));
End;

Procedure TxqStringField.SetAsBoolean(Value: Boolean);
Begin
  SetAsstring(Copy(xqbase.NBoolean[Value], 1, 1));
End;

{-------------------------------------------------------------------------------}
{                  Implements TxqFloatField                                     }
{-------------------------------------------------------------------------------}

Constructor TxqFloatField.Create(Fields: TxqFields; FieldNo: Integer);
Begin
  Inherited Create(Fields, FieldNo);
  SetDataType(ttFloat);
End;

Procedure TxqFloatField.Clear;
Begin
  SetAsFloat(0);
  { LAS : 5/JUN/2003 }
  SetIsNull;
End;

Function TxqFloatField.GetAsVariant: Variant;
Var
  d: Double;
Begin
  If GetData(@d) Then Result:= d Else Result:= Null;
End;

Procedure TxqFloatField.SetVarValue(const Value: Variant);
Begin
  SetAsFloat(Value);
End;

Function TxqFloatField.GetAsFloat: double;
Begin
  If Not GetData(@Result) Then
    Result := 0;
End;

Function TxqFloatField.GetAsInteger: Longint;
Begin
  Result := Longint(Round(GetAsFloat));
End;

Function TxqFloatField.GetAsstring: String;
Var
  F: double;
Begin
  If GetData(@F) Then
    Result := FloatToStr(F)
  Else
    Result := '';
End;

Procedure TxqFloatField.SetAsFloat(Value: double);
Begin
  SetData(@Value);
End;

Procedure TxqFloatField.SetAsInteger(Value: Longint);
Begin
  SetAsFloat(Value);
End;

Procedure TxqFloatField.SetAsstring(Const Value: String);
Var
  F: Extended;
Begin
  If Value = '' Then
    Clear
  Else
  Begin
    If Not TextToFloat(PChar(Value), F, fvExtended) Then
      ExQueryError.CreateFmt(SIsInvalidFloatValue, [Value]);
    SetAsFloat(F);
  End;
End;

{-------------------------------------------------------------------------------}
{                  Implements TxqIntegerField                                   }
{-------------------------------------------------------------------------------}

Constructor TxqIntegerField.Create(Fields: TxqFields; FieldNo: Integer);
Begin
  Inherited Create(Fields, FieldNo);
  SetDataType(ttInteger);
End;

Procedure TxqIntegerField.Clear;
Begin
  SetAsInteger(0);
  { LAS : 5/JUN/2003 }
  SetIsNull;
End;

Function TxqIntegerField.GetAsVariant: Variant;
Var
  v: integer;
Begin
  If GetData(@v) Then Result:= v Else Result:= Null;
End;

Procedure TxqIntegerField.SetVarValue(const Value: Variant);
Begin
  SetAsInteger(Value);
End;

Function TxqIntegerField.GetAsFloat: double;
Begin
  Result := GetAsInteger;
End;

Function TxqIntegerField.GetAsInteger: Longint;
Begin
  If Not GetData(@Result) Then
    Result := 0;
End;

Function TxqIntegerField.GetAsstring: String;
Var
  L: Longint;
Begin
  If GetData(@L) Then
    Result := IntToStr(L)  { patched by ccy }
  Else
    Result := '';
End;

Procedure TxqIntegerField.SetAsFloat(Value: double);
Begin
  SetAsInteger(Integer(Round(Value)));
End;

Procedure TxqIntegerField.SetAsInteger(Value: Longint);
Begin
  SetData(@Value);
End;

Procedure TxqIntegerField.SetAsstring(Const Value: String);
Var
  E: Integer;
  L: Longint;
Begin
  Val(Value, L, E);
  If E <> 0 Then
    ExQueryError.CreateFmt(SIsInvalidIntegerValue, [Value]);
  SetAsInteger(L);
End;

{-------------------------------------------------------------------------------}
{                  Implements TxqBooleanField                                   }
{-------------------------------------------------------------------------------}

Constructor TxqBooleanField.Create(Fields: TxqFields; FieldNo: Integer);
Begin
  Inherited Create(Fields, FieldNo);
  SetDataType(ttBoolean);
End;

Procedure TxqBooleanField.Clear;
Begin
  SetAsBoolean(False);
  { LAS : 5/JUN/2003 }
  SetIsNull;
End;

Function TxqBooleanField.GetAsVariant: Variant;
Var
  b: Boolean;
Begin
  If GetData(@b) Then Result:= b Else Result:= Null;
End;

Procedure TxqBooleanField.SetVarValue(const Value: Variant);
Begin
  SetAsBoolean(Value);
End;

Function TxqBooleanField.GetAsBoolean: Boolean;
Var
  B: WordBool;
Begin
  If GetData(@B) Then
    Result := B
  Else
    Result := False;
End;

Function TxqBooleanField.GetAsstring: String;
Var
  B: WordBool;
Begin
  If GetData(@B) Then
    Result := Copy(xqbase.NBoolean[B], 1, 1)
  Else
    Result := '';
End;

Procedure TxqBooleanField.SetAsBoolean(Value: Boolean);
Var
  B: WordBool;
Begin
  If Value Then
    Word(B) := 1
  Else
    Word(B) := 0;
  SetData(@B);
End;

Procedure TxqBooleanField.SetAsstring(Const Value: String);
Var
  L: Integer;
Begin
  L := Length(Value);
  If L = 0 Then
  Begin
    SetAsBoolean(False);
  End
  Else
  Begin
    If AnsiCompareText(Value, Copy(xqbase.NBoolean[False], 1, L)) = 0 Then
      SetAsBoolean(False)
    Else If AnsiCompareText(Value, Copy(xqbase.NBoolean[True], 1, L)) = 0 Then
      SetAsBoolean(True)
    Else
      ExQueryError.CreateFmt(SIsInvalidBoolValue, [Value]);
  End;
End;

{ TxqFields }

Constructor TxqFields.Create(ResultSet: TResultSet);
Begin
  Inherited Create;
  FResultSet := ResultSet;
  FItems := TList.Create;
End;

Destructor TxqFields.Destroy;
Begin
  Clear;
  FItems.Free;
  Inherited Destroy;
End;

Function TxqFields.FindField(Const FieldName: String): TxqField;
Var
  I: Integer;
Begin
  For I := 0 To FItems.Count - 1 Do
  Begin
    Result := FItems[I];
    If (AnsiCompareText(Result.FFieldName, FieldName) = 0) Or
      (AnsiCompareText(Result.FAlias, FieldName) = 0) Then
      Exit;
  End;
  Result := Nil;
End;

Function TxqFields.GetCount;
Begin
  Result := FItems.Count;
End;

Function TxqFields.GetItem(Index: Integer): TxqField;
Begin
  Result := FItems[Index];
End;

Function TxqFields.Add(DataType: TExprType): TxqField;
Begin
  Result := Nil;
  Case DataType Of
    ttstring: Result := TxqStringField.Create(Self, FItems.Count + 1);
    ttFloat: Result := TxqFloatField.Create(Self, FItems.Count + 1);
    ttInteger: Result := TxqIntegerField.Create(Self, FItems.Count + 1);
    ttBoolean: Result := TxqBooleanField.Create(Self, FItems.Count + 1);
    //ttUnknown: Result:= nil;            { error here if returned nil !}
  End;
  FItems.Add(Result);
End;

Procedure TxqFields.Clear;
Var
  I: Integer;
Begin
  For I := 0 To FItems.Count - 1 Do
    TxqField(FItems[I]).Free;
  FItems.Clear;
End;

Procedure TxqFields.Delete(Index: Integer);
Begin
  TxqField(FItems[Index]).Free;
  FItems.Delete(Index);
End;

{-------------------------------------------------------------------------------}
{                  Implements TResultSet                                        }
{-------------------------------------------------------------------------------}

Constructor TResultSet.Create;
Begin
  Inherited Create;
  FFields := TxqFields.Create(Self);
  FRecNo := -1;
  { first sizeof(TBookmark) bytes is the SourceBookmark for every table}
  {$if RTLVersion <= 18.5}fRecordBufferSize := SizeOf(Integer);{$ifend}  { patched by ccy }
End;

Destructor TResultSet.Destroy;
Begin
  FFields.Free;
  Inherited Destroy;
End;

Procedure TResultSet.Clear;
Begin
  FFields.Clear;
End;

Function TResultSet.FindField(Const FieldName: String): TxqField;
Begin
  Result := FFields.FindField(FieldName);
End;

Function TResultSet.FieldByName(Const FieldName: String): TxqField;
Begin
  Result := FindField(FieldName);
  If Result = Nil Then
    ExQueryError.CreateFmt(SFieldNotFound, [FieldName]);
End;

Function TResultSet.GetFieldData(Field: TxqField; Buffer: Pointer): Boolean;
Begin
  Result := False;
End;

Function TResultSet.GetIsNull(Field: TxqField): Boolean;
Begin
  Result := True;
End;

{ LAS : 5/JUN/2003 }
Procedure TResultSet.SetIsNull(Field: TxqField);
Begin
End;

Procedure TResultSet.AddField(Const pFieldName, pAlias: String;
  pDataType: TExprType;
  pDataSize: Integer;
  pField: TField;
  pReadOnly: Boolean;
  pCastType: Integer;
  pCastLen: Integer;
  pUseDisplayLabel: Boolean);
Begin
  if Assigned(pField) then                     { patched by ccy }
    CheckRecordBufferSize(pField.DataSet);     { patched by ccy }
  With FFields.Add(pDataType) Do
  Begin
    FieldName := pFieldName;
    Alias := pAlias;
    DataType := pDataType;
    SourceField := pField;
    ReadOnly := pReadOnly;
    CastType := pCastType;
    CastLen := pCastLen;
    UseDisplayLabel := pUseDisplayLabel;
    Case CastType Of
      RW_CHAR:
        begin
        DataType := ttstring;
        if pCastType <> 0 then
          pDataSize:= pCastLen;
        end;
      RW_INTEGER: DataType := ttInteger;
      RW_BOOLEAN: DataType := ttBoolean;
      RW_DATE, RW_DATETIME, RW_TIME, RW_FLOAT, RW_MONEY: DataType := ttFloat;
    End;
    { Calculate the position in the record buffer }
    BufferOffset := FRecordBufferSize;
    Case DataType Of
      ttstring:
        Begin
          DataSize := pDataSize;
          Inc(FRecordBufferSize, DataSize + (DataSize Mod SizeOf(Word)) + Sizeof(WordBool));
            // last boolean is the Null indicator
        End;
      ttFloat:
        Begin
          Inc(FRecordBufferSize, SizeOf(Double) + Sizeof(WordBool));
          DataSize := SizeOf(Double);
        End;
      ttInteger:
        Begin
          Inc(FRecordBufferSize, SizeOf(Integer) + Sizeof(WordBool));
          DataSize := SizeOf(Integer);
        End;
      ttBoolean:
        Begin
          Inc(FRecordBufferSize, SizeOf(WordBool) + Sizeof(WordBool));
          DataSize := SizeOf(WordBool);
        End;
    End;
  End;
End;

type TDataSetAccess = class(TDataSet);

procedure TResultSet.CheckRecordBufferSize(DataSet: TDataSet);
begin
  if Assigned(DataSet) and (fRecordBufferSize = 0) then begin
    FBookmarkSize := TDataSetAccess(DataSet).BookmarkSize;
    fRecordBufferSize := FBookmarkSize;
  end;
end;

Function TResultSet.GetRecno: Integer;
Begin
  Result := FRecNo;
End;

Procedure TResultSet.SetRecno(Value: Integer);
Begin
  If FRecNo = Value Then Exit;
  If (Value < 1) Or (Value > GetRecordCount) Then
    Raise ExQueryError.Create(SRecnoInvalid);
  FRecNo := Value;
End;

Function TResultSet.GetRecordCount: Longint;
Begin
  Result := 0;
End;

Procedure TResultSet.SaveToText(Const FileName: String); // debugging purposes
Var
  I, J: Integer;
  f: TStringList;
  s: String;
Begin
  f := TStringList.Create;
  s := '';
  For J := 0 To Fields.Count - 1 Do
    s := s + Format('%12d', [J]);
  f.Add(s);
  For I := 1 To RecordCount Do
  Begin
    Recno := I;
    s := '';
    For J := 0 To Fields.Count - 1 Do
      s := s + Format('%12s', [Fields[J].Asstring]);
    f.Add(s);
  End;
  f.SaveToFile(FileName);
  f.free;
End;

procedure TResultSet.SetSourceDataSet(DataSet: TDataSet);
begin
  fSourceDataSet := DataSet;
  CheckRecordBufferSize(DataSet);
end;

{-------------------------------------------------------------------------------}
{                  Implements TMemResultSet                                     }
{-------------------------------------------------------------------------------}

Constructor TMemResultSet.Create;
Begin
  Inherited Create;
  fBufferList := TList{$if RtlVersion>=20}<TxBuffer>{$ifend}.Create; { patched by ccy }
End;

Destructor TMemResultSet.Destroy;
Begin
  Clear;
  FBufferList.Free;
  Inherited Destroy;
End;

Procedure TMemResultSet.Clear;
Begin
  Inherited Clear;
  ClearBufferList;
End;

Procedure TMemResultSet.SortWithList(SortList: TxqSortList);
Var
  vTempList: TList{$if RtlVersion>=20}<TxBuffer>{$ifend}; { patched by ccy }
  I: Integer;
Begin
  vTempList := TList{$if RtlVersion>=20}<TBookmark>{$ifend}.Create; { patched by ccy }
  For I := 1 To SortList.Count Do
  Begin
    SortList.Recno := I;
    vTempList.Add(FBufferList[SortList.SourceRecno - 1]);
  End;
  FBufferList.Free;
  FBufferList := vTempList;
End;

Procedure TMemResultSet.ClearBufferList;
{$if RTLVersion <= 18.5}
Var
  I: Integer;
  Buffer: PChar;
  Bookmark: TBookmark;
{$ifend}
Begin
  {$if RTLVersion <= 18.5}
  For I := 0 To FBufferList.Count - 1 Do
  Begin
    Buffer := FBufferList[I];
    { free the bookmark }
    Move((Buffer + 0)^, Bookmark, SizeOf(TBookmark));
    If (Longint(Bookmark) > 0) And (FSourceDataSet <> Nil) Then
      //Not (csDestroying in FSourceDataset.ComponentState) Then
    Begin
      // due to the previous test, it is needed first to destroy the TxQuery than others
      FSourceDataSet.FreeBookmark(Bookmark);
    End;
    FreeMem(Buffer, FRecordBufferSize);
  End;
  {$ifend}
  FBufferList.Clear;
End;

function TMemResultSet.ActiveBuffer: TxBuffer; { patched by ccy }
Begin
  Result := Nil;
  If (FRecNo < 1) Or (FRecNo > FBufferList.Count) Then
    Exit;
  Result := FBufferList[FRecNo - 1];
End;

Function TMemResultSet.GetFieldData(Field: TxqField; Buffer: Pointer): Boolean;
Var
  RecBuf: TxBuffer; { patched by ccy }
Begin
  Result := False;
  If GetIsNull(Field) Then Exit;
  RecBuf := ActiveBuffer;
  If RecBuf = Nil Then Exit;
  {$if RTLVersion <= 18.5}
  Move((RecBuf + Field.BufferOffset + SizeOf(WordBool))^, Buffer^, Field.DataSize);
  {$else}
  Move(RecBuf[Field.BufferOffset + SizeOf(WordBool)], Buffer^, Field.DataSize); { patched by ccy }
  {$ifend}
  Result := True;
End;

Function TMemResultSet.GetIsNull(Field: TxqField): Boolean;
Var
  RecBuf: TxBuffer; { patched by ccy }
  HasValue: WordBool;
Begin
  Result := False;
  RecBuf := ActiveBuffer;
  If RecBuf = Nil Then Exit;
  {$if RTLVersion <= 18.5}
  Move((RecBuf + Field.BufferOffset)^, HasValue, SizeOf(WordBool));
  {$else}
  Move(RecBuf[Field.BufferOffset], HasValue, SizeOf(WordBool)); { patched by ccy }
  {$ifend}
  Result := Not HasValue;
End;

{ LAS: 5/JUN/2003}
Procedure TMemResultSet.SetIsNull(Field: TxqField);
Var
  RecBuf: TxBuffer; { patched by ccy }
  HasValue: WordBool;
Begin
  RecBuf := ActiveBuffer;
  If RecBuf = Nil Then Exit;
  HasValue:= False;
  {$if RTLVersion <= 18.5}
  Move(HasValue, (RecBuf + Field.BufferOffset)^, SizeOf(WordBool));
  {$else}
  Move(HasValue, RecBuf[Field.BufferOffset], SizeOf(WordBool)); { patched by ccy }
  {$ifend}
End;

Procedure TMemResultSet.SetFieldData(Field: TxqField; Buffer: Pointer);
Var
  RecBuf: TxBuffer; { patched by ccy }
  HasValue: WordBool;
Begin
  RecBuf := ActiveBuffer;
  If (RecBuf = Nil) Or (Buffer = Nil) Then Exit;
  {$if RTLVersion <= 18.5}
  Move(Buffer^, (RecBuf + Field.BufferOffset + SizeOf(WordBool))^, Field.DataSize);
  {$else}
  Move(Buffer^, RecBuf[Field.BufferOffset + SizeOf(WordBool)], Field.DataSize); { patched by ccy }
  {$ifend}
  HasValue := True;
  {$if RTLVersion <= 18.5}
  Move(HasValue, (RecBuf + Field.BufferOffset)^, SizeOf(WordBool));
  {$else}
  Move(HasValue, RecBuf[Field.BufferOffset], SizeOf(WordBool)); { patched by ccy }
  {$ifend}
End;

Function TMemResultSet.GetRecordCount;
Begin
  Result := FBufferList.Count;
End;

Procedure TMemResultSet.SetSourceBookmark(Bookmark: TBookmark);
Var
  Buffer: TBookmark; { patched by ccy }
Begin
  If (FRecNo < 1) Or (FRecNo > GetRecordCount) Then Exit;
  { first delete any previous bookmark set }
  FreeSourceBookmark;
  Buffer := FBufferList[FRecNo - 1];
  {$if RTLVersion <= 18.5}
  Move(Bookmark, Buffer^, SizeOf(TBookmark));
  {$else}
  Move(Bookmark[0], Buffer[0], Length(Bookmark)); { patched by ccy }
  {$ifend}
End;

Function TMemResultSet.GetSourceBookmark: TBookmark;
{$if RtlVersion <= 18.5}
Var
  Buffer: TBookmark; { patched by ccy }
{$ifend}
Begin
  {$if RtlVersion <= 18.5}
  Result := Nil;
  If (FRecNo < 1) Or (FRecNo > GetRecordCount) Then Exit;
  Buffer := FBufferList[FRecNo - 1];
  Move(Buffer^, Result, SizeOf(TBookmark));
  {$else}
  Result := Nil;
  If (fRecNo < 1) Or (fRecNo > GetRecordCount) Then Exit;
  SetLength(Result, FBookmarkSize);
  Move(fBufferList[fRecNo - 1][0], Result[0], FBookmarkSize);
  {$ifend}
End;

Procedure TMemResultSet.FreeSourceBookmark;
Var
  Buffer: TBookmark; { patched by ccy }
  {$if RtlVersion <= 18.5}Bookmark: TBookmark;{$ifend}
Begin
  If (FRecNo < 1) Or (FRecNo > GetRecordCount) Then Exit;
  Buffer := FBufferList[FRecNo - 1];
  {$if RtlVersion <= 18.5}
  Move(Buffer^, Bookmark, SizeOf(TBookmark));
  If (Longint(Bookmark) > 0) And (FSourceDataSet <> Nil) Then
  Begin
    FSourceDataSet.FreeBookmark(Bookmark);
  End;
  Bookmark := Nil;
  Move(Bookmark, Buffer^, SizeOf(TBookmark));
  {$else}
  FillChar(Buffer[0], FBookmarkSize, 0); { patched by ccy }
  {$ifend}
End;

Procedure TMemResultSet.Insert;
Var
  Buffer: TxBuffer; { patched by ccy }
Begin
  {$if RtlVersion <= 18.5}
  GetMem(Buffer, FRecordBufferSize);
  FillChar(Buffer^, FRecordBufferSize, 0);
  {$else}
  SetLength(Buffer, fRecordBufferSize); { patched by ccy }
  FillChar(Buffer[0], FRecordBufferSize, 0); { patched by ccy }
  {$ifend}
  FBufferList.Add(Buffer);
  FRecNo := FBufferList.Count;
End;

Procedure TMemResultSet.Delete;
Var
  Buffer: TxBuffer; { patched by ccy }
Begin
  If (FRecNo < 1) Or (RecNo > GetRecordCount) Then Exit;
  FreeSourceBookmark;
  Buffer := FBufferList[FRecNo - 1];
  {$if RtlVersion <= 18.5}FreeMem(Buffer, FRecordBufferSize);{$ifend} { patched by ccy }
  FBufferList.Delete(FRecNo - 1);
  If FRecNo > GetRecordCount Then
    FRecNo := GetRecordCount;
End;

{-------------------------------------------------------------------------------}
{                  Implements TFileResultSet                                    }
{-------------------------------------------------------------------------------}

Constructor TFileResultSet.Create(MapFileSize: Longint);
Begin
  Inherited Create;
  FBufferList := TList.Create;
  { auxiliary files }
  FTmpFile := GetTemporaryFileName('~xq');
  FMemMapFile := TMemMapFile.Create(FTmpFile, fmCreate, MapFileSize, True);
End;

Destructor TFileResultSet.Destroy;
Begin
  If Assigned(FBuffer) Then
    FreeMem(FBuffer, FRecordBufferSize);
  Clear;
  FBufferList.Free;
  Inherited Destroy;
End;

Procedure TFileResultSet.Clear;
Var
  I: Integer;
  Bookmark: TBookmark;
Begin
  Inherited Clear;
  { free the bookmarks }
  For I := 0 To FBufferList.Count - 1 Do
  Begin
    FMemMapFile.Seek(Longint(FBufferList[I]), 0);
    FMemMapFile.Read(Bookmark, SizeOf(TBookmark));
    If (Longint(Bookmark) > 0) And (FSourceDataSet <> Nil) Then
    Begin
      FSourceDataSet.FreeBookmark(Bookmark);
    End;
  End;
  FreeObject(FMemMapFile);
  SysUtils.DeleteFile(FTmpFile);
  FBufferList.Clear;
End;

Procedure TFileResultSet.SortWithList(SortList: TxqSortList);
Var
  vTempList: TList;
  I: Integer;
Begin
  vTempList := TList.Create;
  For I := 1 To SortList.Count Do
  Begin
    SortList.Recno := I;
    vTempList.Add(FBufferList[SortList.SourceRecno - 1]);
  End;
  FBufferList.Free;
  FBufferList := vTempList;
End;

Function TFileResultSet.ActiveBuffer: PChar;
Begin
  Result := Nil;
  If (FRecNo < 1) Or (FRecNo > FBufferList.Count) Then
    Exit;
  If Not Assigned(FBuffer) Then
    GetMem(FBuffer, FRecordBufferSize);
  FMemMapFile.Seek(Longint(FBufferList[FRecNo - 1]), 0);
  FMemMapFile.Read(FBuffer^, FRecordBufferSize);
  Result := FBuffer;
End;

Function TFileResultSet.GetFieldData(Field: TxqField; Buffer: Pointer): Boolean;
Var
  RecBuf: PChar;
Begin
  Result := False;
  If GetIsNull(Field) Then Exit;
  RecBuf := ActiveBuffer;
  If RecBuf = Nil Then
    Exit;
  Move((RecBuf + Field.BufferOffset + Sizeof(WordBool))^, Buffer^, Field.DataSize);
  Result := True;
End;

Function TFileResultSet.GetIsNull(Field: TxqField): Boolean;
Var
  RecBuf: PChar;
  HasValue: WordBool;
Begin
  Result := False;
  RecBuf := ActiveBuffer;
  If RecBuf = Nil Then
    Exit;
  Move((RecBuf + Field.BufferOffset)^, HasValue, SizeOf(WordBool));
  Result := Not HasValue;
End;

{ LAS : 5/JUN/2003 }
Procedure TFileResultSet.SetIsNull(Field: TxqField);
Var
  RecBuf: PChar;
  HasValue: WordBool;
Begin
  RecBuf := ActiveBuffer;
  If RecBuf = Nil Then Exit;
  HasValue:= False;
  Move(HasValue, (RecBuf + Field.BufferOffset)^, SizeOf(WordBool));
End;

Procedure TFileResultSet.SetFieldData(Field: TxqField; Buffer: Pointer);
Var
  RecBuf: PChar;
  HasValue: WordBool;
Begin
  RecBuf := ActiveBuffer;
  If RecBuf = Nil Then
    Exit;
  Move(Buffer^, (RecBuf + Field.BufferOffset + Sizeof(Boolean))^, Field.DataSize);
  HasValue := True;
  Move(HasValue, (RecBuf + Field.BufferOffset)^, SizeOf(WordBool));
  FMemMapFile.Seek(Longint(FBufferList[Recno - 1]), 0);
  FMemMapFile.Write(RecBuf^, FRecordBufferSize);
End;

Function TFileResultSet.GetRecordCount;
Begin
  Result := FBufferList.Count;
End;

Procedure TFileResultSet.SetSourceBookmark(Bookmark: TBookmark);
Begin
  If (FRecNo < 1) Or (FRecNo > GetRecordCount) Then  Exit;
  { first delete any previous bookmark set }
  FreeSourceBookmark;
  FMemMapFile.Seek(Longint(FBufferList[FRecNo - 1]), 0);
  FMemMapFile.Read(Bookmark, SizeOf(TBookmark));
End;

Function TFileResultSet.GetSourceBookmark: TBookmark;
Begin
  Result := Nil;
  If (FRecNo < 1) Or (FRecNo > GetRecordCount) Then
    Exit;
  FMemMapFile.Seek(Longint(FBufferList[FRecNo - 1]), 0);
  FMemMapFile.Read(Result, SizeOf(TBookmark));
End;

Procedure TFileResultSet.FreeSourceBookmark;
Var
  Bookmark: TBookmark;
Begin
  If (FRecNo < 1) Or (FRecNo > GetRecordCount) Then Exit;
  FMemMapFile.Seek(Longint(FBufferList[FRecNo - 1]), 0);
  FMemMapFile.Read(Bookmark, SizeOf(TBookmark));
  If (Longint(Bookmark) > 0) And (FSourceDataSet <> Nil) Then
  Begin
    FSourceDataSet.FreeBookmark(Bookmark);
  End;
  Bookmark := Nil;
  FMemMapFile.Seek(Longint(FBufferList[FRecNo - 1]), 0);
  FMemMapFile.Write(Bookmark, SizeOf(TBookmark));
End;

Procedure TFileResultSet.Insert;
Var
  Offset: Integer;
Begin
  If Not Assigned(FBuffer) Then
    GetMem(FBuffer, FRecordBufferSize);
  FillChar(FBuffer^, FRecordBufferSize, 0);
  Offset := FMemMapFile.VirtualSize;
  FMemMapFile.Seek(Offset, 0);
  FMemMapFile.Write(FBuffer^, FRecordBufferSize);
  FBufferList.Add(Pointer(Offset)); { the address in temp file is saved }
  FRecNo := FBufferList.Count;
End;

Procedure TFileResultSet.Delete;
Begin
  If (FRecNo < 1) Or (RecNo > GetRecordCount) Then Exit;
  FreeSourceBookmark;
  FBufferList.Delete(FRecNo - 1);
  If FRecNo > GetRecordCount Then
    FRecNo := GetRecordCount;
End;

Procedure TFileResultSet.ClearBufferList;
Begin
End;

{-------------------------------------------------------------------------------}
{                  Implements TSqlAnalizer                                      }
{-------------------------------------------------------------------------------}

Constructor TSqlAnalizer.Create(ParentAnalizer: TSqlAnalizer; XQuery: TCustomxQuery);
Var
  Temps: String;
Begin
  Inherited Create;
  FParentAnalizer := ParentAnalizer;
  FxQuery := XQuery;
  FParams := TParams.Create{$IFNDEF level3}(FxQuery){$ENDIF};
  FColumnList := TColumnList.Create;
  FTableList := TTableList.Create;
  FOrderByList := TOrderByList.Create;
  FGroupByList := TOrderByList.Create;
  FJoinList := TJoinOnList.Create(Self);
  FLJoinCandidateList := TStringList.Create;
  FRJoinCandidateList := TStringList.Create;
  FTableAllFields := TStringList.Create;
  FInsertList := TInsertList.Create;
  FUpdateColumnList := TUpdateList.Create;
  FWhereOptimizeList := TWhereOptimizeList.Create;
  FCreateTableList := TCreateTableList.Create;
  FAlterTableList := TCreateTableList.Create;
  FIndexColumnList := TStringList.Create;
  FTransformColumnList := TColumnList.Create;
  FPivotInList := TStringList.Create;
  FHavingCol := -1;
  FSubQueryList := TList.Create; { LAS: 01-08-00 }
  FSubQueryKindList := TList.Create;
  FUserDefinedRange := TUserDefinedRange.Create;

  { create lex / yacc information }
  Temps := FxQuery.SQL.Text;
  FInputStream := TMemoryStream.Create;
  FInputStream.WriteBuffer(Pointer(Temps)^, Length(Temps) * SizeOf(Char)); { patched by ccy }
  FInputStream.Seek(0, 0);
  FOutputStream := TMemoryStream.Create;
  FErrorStream := TMemoryStream.Create;
  FLexer := TxqLexer.Create;
  FLexer.yyinput := FInputStream;
  FLexer.yyoutput := FOutputStream;
  FLexer.yyerrorfile := FErrorStream;
  (FLexer As TxqLexer).DateFormat := FxQuery.DateFormat;
  FParser := TxqParser.Create(Self); { parser and Analizer linked }
  FParser.yyLexer := FLexer; { link lexer and parser }

End;

Destructor TSqlAnalizer.Destroy;
Begin
  ClearSQL;
  FColumnList.Free;
  FTableList.Free;
  FOrderByList.Free;
  FGroupByList.Free;
  FSubQueryList.Free;
  FSubQueryKindList.Free;
  //if FSubQueryList.Count > 0 then
  //   FreeSubQueries(fSubQuery);
  If Assigned(FResultSet) Then
    FResultSet.Free;
  FTableAllFields.Free;
  FJoinList.Free;
  FLJoinCandidateList.Free;
  FRJoinCandidateList.Free;
  FUpdateColumnList.Free;
  FInsertList.Free;
  FWhereOptimizeList.Free;
  FParams.Free;
  FCreateTableList.Free;
  FAlterTableList.Free;
  FIndexColumnList.Free;
  FTransformColumnList.Free;
  FPivotInList.Free;
  FUserDefinedRange.Free;
  If Assigned(FUnionAnalizer) Then
    FreeObject(FUnionAnalizer);
  If Assigned(FTransfResultSet) Then
    FreeObject(FTransfResultSet);

  { free lex / yacc information }
  FParser.free;
  FInputStream.free;
  FOutputStream.free;
  FErrorStream.free;
  FLexer.free;

  Inherited Destroy;
End;

Function TSqlAnalizer.CreateSortList(UsingBookmark: Boolean): TxqSortList;
Begin
  If FxQuery.InMemResultSet Then
    Result := TMemSortList.Create(UsingBookmark)
  Else
    Result := TFileSortList.Create(UsingBookmark, FxQuery.FMapFileSize);
End;

{ recursively free subqueries }

Procedure TSqlAnalizer.FreeSubQueries(Var SubQ: TSqlAnalizer);
Var
  i: integer;
  Analizer: TSqlAnalizer;
Begin
  For i := 0 To SubQ.FSubQueryList.Count - 1 Do
  Begin
    Analizer := TSqlAnalizer(SubQ.FSubQueryList[i]);
    FreeSubQueries(Analizer);
  End;
  FreeObject(SubQ);
End;

Procedure TSqlAnalizer.ClearSQL;
Var
  TmpAnalizer: TSqlAnalizer;
  I: integer;
Begin
  ColumnList.Clear;
  TableList.Clear;
  OrderByList.Clear;
  GroupByList.Clear;
  JoinList.Clear;
  UpdateColumnList.Clear;
  WhereOptimizeList.Clear;
  CreateTableList.Clear;
  For I := 0 To FSubQueryList.Count - 1 Do
  Begin
    TmpAnalizer := FSubQueryList[I];
    TmpAnalizer.ClearSQL;
    FreeSubqueries(TmpAnalizer);
  End;
  FSubQueryList.Clear;
  FSubQueryKindList.Clear;
  If Assigned(ResultSet) Then
    ResultSet.Clear;
End;

Procedure TSqlAnalizer.DoJoinInWhere;

  Procedure RecursiveJoin(Start: Integer);
  Var
    TmpDataSet: TDataSet;
    bm: TBookmark;
  Begin
    TmpDataSet := FTableList[Start + 1].DataSet;
    TmpDataSet.First;
    While Not TmpDataSet.EOF Do
    Begin
      If Start < FTableList.Count - 2 Then
      Begin
        { call recursively }
        bm := TmpDataset.GetBookmark;
        Try
          RecursiveJoin(Start + 1)
        Finally
          TmpDataset.GotoBookmark(bm);
          TmpDataset.FreeBookmark(bm);
        End;
      End
      Else
      Begin
        If FJoinInWhereResolver.Expression.AsBoolean Then
          AddThisRecord(FDefDataSet);
      End;

      { next record }
      TmpDataSet.Next;
    End;
  End;
Begin
  { recursively joining }
  if FTableList.Count > 1 then
    RecursiveJoin(0);
End;

{ --- Add this record to the Result set --- }

Procedure TSqlAnalizer.AddThisRecord(Dataset: TDataset);
Var
  J, K, N: Integer;
  vField: TxqField;
  vColumn: TColumnItem;
  vAnalizer: TSqlAnalizer;
  vS: String;
Begin
  ResultSet.Insert;

  { Dataset can optionally support RecNo property  }
  ResultSet.SetSourceBookmark(Dataset.GetBookmark);

  { add all fields }
  For J := 0 To FColumnList.Count - 1 Do
  Begin
    vColumn := ColumnList[J];

    { this column will be evaluated in DoGroupBy method }
    If vColumn.AggregateList.Count > 0 Then
      Continue;

    If vColumn.SubqueryList.Count > 0 Then
    Begin
      { a subquery in the SELECT clause must be evaluated here }
      vS := vColumn.ColumnExpr;
      For K := 0 To vColumn.SubqueryList.Count - 1 Do
      Begin
        vAnalizer := TSqlAnalizer(vColumn.SubqueryList[K]);
        N := vAnalizer.ResultSet.RecordCount;
        If N = 0 then
        begin
          Replacestring(vS, Format('{Subquery %d}', [K]), '""');
        end else
        begin
          If ResultSet.RecNo > N Then
            vAnalizer.ResultSet.RecNo := N
          Else
            vAnalizer.ResultSet.RecNo := ResultSet.RecNo;
          { only the first field is meaningful }
          Replacestring(vS, Format('{Subquery %d}', [K]), vAnalizer.ResultSet.Fields[0].Asstring);
        end;
        { due to this, you cannot combine an aggregate function with a subquery in the SELECT
         columns, example:
         SELECT custno, SUM(amountpaid) *
            (SELECT COUNT(*) FROM customer WHERE custno BETWEEN 1000 AND 2000) FROM customer;

         is not valid, but you can do this instead:
         SELECT custno, (SELECT SUM(AmountPaid) FROM Customer) *
            (SELECT COUNT(*) FROM customer WHERE custno BETWEEN 1000 AND 2000) FROM customer;

         }
      End;
      vColumn.Resolver.ParseExpression(vS);
    End;

    vField := ResultSet.Fields[J];
    With vColumn Do
    Begin
      If Assigned(vField.SourceField) And (vField.SourceField Is TBlobField) Then
      Begin
        { Instead of actual data, this will point to the original recno
          that have the blob field of the dataset }
        If Not vField.SourceField.IsNull Then
        Begin
          If Not Self.FxQuery.IsDataSetDisabled(vField.SourceField.DataSet) Then
          Begin
            { actually, this information is no longer used }
            ResultSet.Fields[J].AsInteger := {GetRecordNumber(} vField.SourceField.DataSet.Recno {)};
          End;
        End;
      End Else If Not Resolver.Expression.IsNull Then
        Case Resolver.Expression.ExprType Of
          ttstring:
            vField.Asstring := Resolver.Expression.Asstring;
          ttFloat:
            vField.AsFloat := Resolver.Expression.AsFloat;
          ttInteger:
            vField.AsInteger := Resolver.Expression.AsInteger;
          ttBoolean:
            vField.AsBoolean := Resolver.Expression.AsBoolean;
        End;
    End;
  End;
End;

{ INSERT statement }

Procedure TSqlAnalizer.DoInsertStatement;
Var
  I, J, K, L: Integer;
  Resolver: TExprParser;
  InsertItem: TInsertItem;
  TmpAnalizer: TSqlAnalizer;
  TmpField: TField;
Begin
  For L := 0 To FInsertList.Count - 1 Do
  Begin
    InsertItem := FInsertList[L];
    If FSubQueryList.Count > 0 Then
    Begin
      For K := 0 To FSubQueryList.Count - 1 Do
      Begin
        TmpAnalizer := FSubQueryList[K];
        For I := 1 To TmpAnalizer.ResultSet.RecordCount Do
        Begin
          TmpAnalizer.ResultSet.RecNo := I;
          If InsertItem.FieldNames.Count = 0 Then
          Begin
            { insertion on all fields }
            InsertItem.DataSet.Insert;
            Try
              For J := 0 To InsertItem.DataSet.FieldCount - 1 Do
              Begin
                If J <= TmpAnalizer.ResultSet.FFields.Count - 1 Then
                Begin
                  With InsertItem.DataSet.Fields[J] Do
                  Begin
                    If DataType In ftNonTextTypes Then
                    Begin
                      TmpField := TmpAnalizer.ResultSet.Fields[J].SourceField;
                      If Assigned(TmpField) And (TmpField.DataType In ftNonTextTypes) And
                         Not (TmpField.IsNull) Then
                        Assign(TmpField);
                      //Asstring := TmpAnalizer.ResultSet.Fields[J].Asstring;
                    End
                    Else
                    Begin
                      if not TmpAnalizer.ResultSet.Fields[J].IsNull then
                      begin
                        Case Field2ExprType(DataType) Of
                          ttstring:
                            InsertItem.DataSet.Fields[J].Asstring := TmpAnalizer.ResultSet.Fields[J].Asstring;
                          ttFloat:
                            InsertItem.DataSet.Fields[J].AsFloat := TmpAnalizer.ResultSet.Fields[J].AsFloat;
                          ttInteger:
                            InsertItem.DataSet.Fields[J].AsInteger := TmpAnalizer.ResultSet.Fields[J].AsInteger;
                          ttBoolean:
                            InsertItem.DataSet.Fields[J].AsBoolean := TmpAnalizer.ResultSet.Fields[J].AsBoolean;
                        End;
                      End;
                    End;
                  End;
                End
                Else
                  Break;
              End;
              Inc(FxQuery.FRowsAffected);
              { InsertItem.DataSet.Fields[J].Value := Null; }
            Except
              InsertItem.DataSet.Cancel;
              Raise;
            End;
            InsertItem.DataSet.Post;
          End
          Else
          Begin
            // insertion on specific fields
            InsertItem.DataSet.Insert;
            Try
              For J := 0 To InsertItem.FieldNames.Count - 1 Do
                With InsertItem.DataSet.FieldByName(InsertItem.FieldNames[J]) Do
                  If DataType In ftNonTextTypes Then
                  Begin
                    TmpField :=
                      TmpAnalizer.ResultSet.FieldByName(InsertItem.FieldNames[J]).SourceField;
                    If Assigned(TmpField) And (TmpField.DataType In ftNonTextTypes) And
                      Not TmpField.IsNull And Not (AnsiCompareText(InsertItem.ExprList[J], 'NULL') = 0) Then
                      Assign(TmpField);
                    //Asstring := TmpAnalizer.ResultSet.FieldByName(InsertItem.FieldNames[J]).Asstring
                  End
                  Else If Not TmpAnalizer.ResultSet.Fields[J].IsNull Then
                  Begin
                    Case Field2ExprType(DataType) Of
                      ttstring:
                        Asstring :=
                          TmpAnalizer.ResultSet.Fields[J].Asstring;
                      ttFloat:
                        AsFloat :=
                          TmpAnalizer.ResultSet.Fields[J].AsFloat;
                      ttInteger:
                        AsInteger :=
                          TmpAnalizer.ResultSet.Fields[J].AsInteger;
                      ttBoolean:
                        AsBoolean :=
                          TmpAnalizer.ResultSet.Fields[J].AsBoolean;
                    End;
                  End;
              Inc(FxQuery.FRowsAffected);
              { for j := 0 to InsertItem.DataSet.FieldCount - 1 do
                 if InsertItem.FieldNames.IndexOf(Insert.DataSet.Fields[J].FieldName) < 0 then
                    InsertItem.DataSet.Fields[J].Value := Null; }
            Except
              InsertItem.DataSet.Cancel;
              Raise;
            End;
            InsertItem.DataSet.Post;
          End;
        End;
      End;
    End
    Else
    Begin

      { only one record will be inserted }
      If InsertItem.FieldNames.Count = 0 Then
      Begin
        { insertion on all fields }
        InsertItem.DataSet.Insert;
        Try
          For I := 0 To InsertItem.DataSet.FieldCount - 1 Do
            If I <= InsertItem.ResolverList.Count - 1 Then
            Begin
              With InsertItem.DataSet.Fields[I] Do
              Begin
                Resolver := TExprParser(InsertItem.ResolverList[I]);
                If Resolver = Nil Then Continue;
                If (Datatype In ftNonTextTypes) And Not Resolver.Expression.IsNull Then
                  Asstring := HexToString (Resolver.Expression.Asstring)
                  // Encode data in HEX text, probably there's a better way to do this with more code changes
                Else If Not Resolver.Expression.IsNull Then
                Begin
                  Case Field2ExprType(DataType) Of
                    ttstring:
                      Asstring := Resolver.Expression.Asstring;
                    ttFloat:
                      AsFloat := Resolver.Expression.AsFloat;
                    ttInteger:
                      AsInteger := Resolver.Expression.AsInteger;
                    ttBoolean:
                      AsBoolean := Resolver.Expression.AsBoolean;
                  End;
                End;
              End;
            End
            Else
              Break;
          Inc(FxQuery.FRowsAffected);
          { InsertItem.DataSet.Fields[i].Value := Null; }
        Except
          InsertItem.DataSet.Cancel;
          Raise;
        End;
        InsertItem.DataSet.Post;
      End
      Else
      Begin
        { insertion on specific fields }
        InsertItem.DataSet.Insert;
        Try
          For I := 0 To InsertItem.FieldNames.Count - 1 Do
          Begin
            With InsertItem.DataSet.FieldByName(InsertItem.FieldNames[I]) Do
            Begin
              Resolver := TExprParser(InsertItem.ResolverList[I]);
              If Resolver = Nil Then
                Continue;
              If (DataType In ftNonTextTypes) And Not Resolver.Expression.IsNull Then
                Asstring := HexToString(Resolver.Expression.Asstring)
              Else If Not Resolver.Expression.IsNull Then
              Begin
                Case Field2ExprType(DataType) Of
                  ttstring:
                    Asstring := Resolver.Expression.Asstring;
                  ttFloat:
                    AsFloat := Resolver.Expression.AsFloat;
                  ttInteger:
                    AsInteger := Resolver.Expression.AsInteger;
                  ttBoolean:
                    AsBoolean := Resolver.Expression.AsBoolean;
                End;
              End;
            End;
          End;
          Inc(FxQuery.FRowsAffected);
          { set to null all other fields }
          { for i := 0 to InsertItem.DataSet.FieldCount - 1 do
             if InsertItem.FieldNames.IndexOf(InsertItem.DataSet.Fields[i].FieldName) < 0 then
                InsertItem.DataSet.Fields[J].Value := Null; }
        Except
          InsertItem.DataSet.Cancel;
          Raise;
        End;
        InsertItem.DataSet.Post;
      End;
    End;
  End;
End;

{ UPDATE statement }

Procedure TSqlAnalizer.DoMassiveUpdates;
Var
  I, Recno: Integer;
  Subq: TSqlAnalizer;
  xqField: TxqField;
Begin
  { this method is called internally when syntax is like:
    UPDATE olditem SET description, name = (SELECT description, name FROM
    payitem WHERE payitem.itemcode = olditem.itemcode;)}
  Subq := TSqlAnalizer(FSubQueryList[0]);
  FDefDataSet.First;
  Recno := 1;
  While Not FDefDataSet.Eof Do
  Begin
    Subq.ResultSet.Recno := Recno;
    FDefDataSet.Edit;
    For I := 0 To UpdateColumnList.Count - 1 Do
      With UpdateColumnList[I] Do
      Begin
        xqField := Subq.ResultSet.Fields[I];
        If Not xqField.IsNull Then
          Case Field2Exprtype(Field.DataType) Of
            ttstring: Field.Asstring := xqField.AsString;
            ttFloat: Field.AsFloat := xqField.AsFloat;
            ttInteger: Field.AsInteger := xqField.AsInteger;
            ttBoolean: Field.AsBoolean := xqField.AsBoolean;
          End;
      End;
    FDefDataSet.Post;
    Inc(FxQuery.FRowsAffected);

    FDefDataSet.Next;
    Inc(Recno);
  End;
End;

Procedure TSqlAnalizer.DoUpdateRecord;
Var
  I: Integer;
Begin
  FDefDataSet.Edit;
  For I := 0 To UpdateColumnList.Count - 1 Do
    With UpdateColumnList[I] Do
    Begin
      if Length(ColExpr) > 0 then
      Begin
        If Not Resolver.Expression.IsNull Then
          Case Field2Exprtype(Field.DataType) Of
            ttstring: Field.Asstring := Resolver.Expression.Asstring;
            ttFloat: Field.AsFloat := Resolver.Expression.AsFloat;
            ttInteger: Field.AsInteger := Resolver.Expression.AsInteger;
            ttBoolean: Field.AsBoolean := Resolver.Expression.AsBoolean;
          End;
      End Else
      Begin
        Field.Clear;
      End;
    End;
  FDefDataSet.Post;
  Inc(FxQuery.FRowsAffected);
End;

{ GROUP BY clause }

Procedure TSqlAnalizer.DoGroupBy;
Var
  I, J, K, L, n: Integer;
  Idx: Integer;
  vIndex: Integer;
  vPivot: Integer;
  vS: String;
  vColumn: TColumnItem;
  vTempExpr: TExprParser;
  vDiff: Double;
  vSortList: TxqSortList;
  ThisGroupCount: Integer;
  vValue: Variant;
  vIsNull: Boolean;
  DValue: Double;
  ResultSetHasRecords: Boolean;
  B: TBookmark; { pathced by ccy }
Begin
  // GROUP BY clause
  If Not( (ResultSet.RecordCount > 0) And
     ((FGroupByList.Count > 0) Or HasAggregates) ) Then Exit;

  if ResultSet.RecordCount=0 Then
  Begin
    ResultSetHasRecords:= False;
    ResultSet.Insert();
  End Else
    ResultSetHasRecords:= True;

  ResultSet.IsSequenced := False;
  vSortList := CreateSortList(False);
  Try
    // syntax: SELECT COUNT(DISTINCT pricelist) FROM customer;
    If HasDistinctAggregates Then
    Begin
      { create all the needed fields }
      For J := 0 To FColumnList.Count - 1 Do
      Begin
        vColumn := FColumnList[J];
        For K := 0 To vColumn.AggregateList.Count - 1 Do
        Begin
          If Not (vColumn.AggregateList[K].IsDistinctAg) Then
            Continue;
          L := vColumn.AggregateList[K].ColIndex;
          vSortList.AddField(ResultSet.Fields[L].DataType,
            ResultSet.Fields[L].ColWidth, False);
        End;
      End;
      For I := 1 To ResultSet.RecordCount Do
      Begin
        ResultSet.RecNo := I;
        Idx := 0;
        For J := 0 To FColumnList.Count - 1 Do
        Begin
          vColumn := FColumnList[J];
          For K := 0 To vColumn.AggregateList.Count - 1 Do
          Begin
            If Not (vColumn.AggregateList[K].IsDistinctAg) Then
              Continue;
            { This is the temporary column where the aggregate was defined }
            vSortList.Insert;
            vSortList.SourceRecno := I;
            L := vColumn.AggregateList[K].ColIndex;
            If Not ResultSet.Fields[L].IsNull Then
              Case ResultSet.Fields[L].DataType Of
                ttstring:
                  vSortList.Fields[Idx].Asstring := ResultSet.Fields[L].Asstring;
                ttFloat:
                  vSortList.Fields[Idx].AsFloat := ResultSet.Fields[L].AsFloat;
                ttInteger:
                  vSortList.Fields[Idx].AsInteger := ResultSet.Fields[L].AsInteger;
                ttBoolean:
                  vSortList.Fields[Idx].AsBoolean := ResultSet.Fields[L].AsBoolean;
              End;
            Inc(Idx);
          End;
        End;
      End;
      vSortList.Sort;

      { mark records that must be deleted }
      For I := vSortList.Count Downto 2 Do
      Begin
        If vSortList.IsEqual(I, I - 1) Then
        Begin
          vSortList.Recno := I;
          ResultSet.RecNo := vSortList.SourceRecno;
          {$if RtlVersion <= 18.5}
          ResultSet.SetSourceBookmark(TBookmark(-2));
          {$else}
          ResultSet.SetSourceBookmark(TBookmark.Create($FF, $FE)); { pathced by ccy }
          {$ifend}
        End;
      End;

      { now, delete the records }
      For I := ResultSet.RecordCount Downto 1 Do
      Begin
        ResultSet.RecNo := I;
        {$if RtlVersion <= 18.5}
        If Longint(ResultSet.GetSourceBookmark) = -2 Then
        {$else}
        B := ResultSet.GetSourceBookmark; { pathced by ccy }
        If (Length(B) > 2) and (B[0] = $FF) and (B[1] = $FE) Then { pathced by ccy }
        {$ifend}
        Begin
          ResultSet.Delete;
          FColumnList.DeleteAggregate(I);
        End;
      End;
      vSortList.Clear;
    End; { end of processing DISTINCT aggregate columns }

    { now the real grouping :
      create the group-by fields }
    For J := 0 To FGroupByList.Count - 1 Do
      With FGroupByList[J] Do
        vSortList.AddField(ResultSet.Fields[ColIndex].DataType,
          ResultSet.Fields[ColIndex].ColWidth, False);

    { An additional field in the sort for ordering by recno also }
    //vSortList.AddField(ttInteger, 0, False);

    For I := 1 To ResultSet.RecordCount Do
    Begin
      ResultSet.RecNo := I;
      vSortList.Insert;
      vSortList.SourceRecno := I;
      For J := 0 To FGroupByList.Count - 1 Do
        With FGroupByList[J] Do
          If Not ResultSet.Fields[ColIndex].IsNull Then
            Case ResultSet.Fields[ColIndex].DataType Of
              ttstring:
                vSortList.Fields[J].Asstring := ResultSet.Fields[ColIndex].Asstring;
              ttFloat:
                vSortList.Fields[J].AsFloat := ResultSet.Fields[ColIndex].AsFloat;
              ttInteger:
                vSortList.Fields[J].AsInteger := ResultSet.Fields[ColIndex].AsInteger;
              ttBoolean:
                vSortList.Fields[J].AsBoolean := ResultSet.Fields[ColIndex].AsBoolean;
            End;
      //vSortList.Fields[FGroupByList.Count].AsInteger:= I;
    End;
    vSortList.Sort;

    // once sorted, group the records
    vIndex := 1;
    vPivot := 1;
    vValue := 0;

    ThisGroupCount:= Self.FTopNInGroupBy;

    // all but pivots are marked with negative number
    vSortList.Recno := 1;
    vSortList.SourceRecno := -vSortList.SourceRecno;
    While vIndex <= ResultSet.RecordCount Do
    Begin
      If vSortList.IsEqual(vPivot, vIndex) Then
      Begin
        If (FGroupByList.Count > 0) And (ThisGroupCount > 0) then
        begin
          vSortList.Recno := vIndex;
          If vSortList.SourceRecno > 0 then
            vSortList.SourceRecno := -vSortList.SourceRecno;
          Dec(ThisGroupCount);
        end;
        For I := 0 To FColumnList.Count - 1 Do
        Begin
          vColumn := FColumnList[I];
          For J := 0 To vColumn.AggregateList.Count - 1 Do
          Begin
            { set position to current record and get the values in there }
            If Not (vColumn.AggregateList[J].Aggregate = akCOUNT) Then
            Begin
              { it is in [akSUM, akAVG, akSTDEV, akMIN, akMAX] }
              vSortList.RecNo := vIndex;
              ResultSet.RecNo := Abs(vSortList.SourceRecno);
              vValue := ResultSet.Fields[vColumn.AggregateList[J].ColIndex].AsVariant;
            End;
            vIsNull := ResultSet.Fields[vColumn.AggregateList[J].ColIndex].IsNull;

            If (Not vIsNull) Or (vColumn.AggregateList[J].Aggregate=akCount) Then
            Begin
              { Set position to the pivot record for this group }
              vSortList.Recno := vPivot;
              n := Abs(vSortList.SourceRecno);
              With vColumn.AggregateList[J] Do
              Begin
                Case Aggregate Of
                  akSUM, akAVG, akSTDEV:
                    Begin
                      { Warning: a SUM,AVG,STDEV for string cannot be calculated }
                      If ResultSetHasRecords Then
                      Begin
                        SparseList.Values[n] := SparseList.Values[n] + vValue;
                        If Aggregate = akSTDEV Then { LAS: 05-30-00 }
                          SparseList.SqrValues[n] := SparseList.SqrValues[n] + Sqr(vValue);
                      End;
                    End;
                  akMIN:
                    If ResultSetHasRecords Then
                    Begin
                      If SparseList.Count[n] = 0 Then
                        SparseList.Values[n] := vValue
                      Else
                        SparseList.Values[n] := xqmiscel.VarMin(vValue, SparseList.Values[n]);
                    End;
                  akMAX:
                    If ResultSetHasRecords Then
                    Begin
                      If SparseList.Count[n] = 0 Then
                        SparseList.Values[n] := vValue
                      Else
                        SparseList.Values[n] := xqmiscel.VarMax(vValue, SparseList.Values[n]);
                    End;
                End;
                { always is calculated due to that it is used in AVG,MIN,MAX,COUNT }
                If ResultSetHasRecords Then
                  SparseList.Count[n] := SparseList.Count[n] + 1;
              End;
            End;
          End;
        End;
      End
      Else
      Begin
        vPivot := vIndex;
        { mark pivot with a negative number }
        vSortList.Recno := vPivot;
        vSortList.SourceRecno := -vSortList.SourceRecno;
        vIndex := Pred(vPivot);
        ThisGroupCount:= Self.FTopNInGroupBy;
      End;
      Inc(vIndex);
    End;

    { calculate AVG aggregate and pass Results to normal field area }
    For I := 1 To vSortList.Count Do
    Begin
      vSortList.Recno := I;
      K := vSortList.SourceRecno;
      If K < 0 Then
      Begin
        n := Abs(K);
        ResultSet.RecNo := n;
        For J := 0 To FResultSet.Fields.Count - 1 Do
        Begin
          vColumn := FColumnList[J];
          If (J = FHavingCol) Or (vColumn.AggregateList.Count = 0) Then
            Continue;
          vS := vColumn.ColumnExpr;
          For K := 0 To vColumn.AggregateList.Count - 1 Do
          Begin
            With vColumn.AggregateList[K] Do
            Begin
              If Aggregate = akAVG Then
              Begin
                If ResultSetHasRecords Then
                Begin
                  If SparseList.Count[n] > 0 Then
                    SparseList.Values[n] := SparseList.Values[n] / SparseList.Count[n]
                  Else
                    SparseList.Values[n] := 0;
                End;
              End;
              If Aggregate = akSTDEV Then
              Begin
                If ResultSetHasRecords Then
                Begin
                  If SparseList.Count[n] > 1 Then
                  Begin
                    vDiff := SparseList.SqrValues[n] - Sqr(SparseList.Values[n]) / SparseList.Count[n];
                    If vDiff < 0 Then
                      vDiff := 0;
                    SparseList.Values[n] := Sqrt(vDiff / (SparseList.Count[n] - 1))
                  End
                  Else
                    SparseList.Values[n] := 0;
                End;
              End;
              { replace expressions like (Aggregate 1) with their real value}
              If Aggregate = akCOUNT Then
              Begin
                Replacestring(vS, Format('{Aggregate %d}', [K]), IntToStr(SparseList.Count[n]));
              End
              Else
              Begin
                If VarType(SparseList.Values[n]) = varString Then
                  Replacestring(vS, Format('{Aggregate %d}', [K]), '"'+SparseList.Values[n]+'"')
                Else
                Begin
                  DValue:= SparseList.Values[n];
                  Replacestring(vS, Format('{Aggregate %d}', [K]), FloatToStr(DValue));
                End;
              End;
              Replacestring(vS, DecimalSeparator, '.');
            End;
          End;
          vColumn.Resolver.ParseExpression(vS);
          { now set the temporary aggregate calculated value in the working value }
          With vColumn.Resolver.Expression Do
            If Not IsNull Then
              Case ExprType Of
                ttstring: ResultSet.Fields[J].Asstring := Asstring;
                ttFloat: ResultSet.Fields[J].AsFloat := AsFloat;
                ttInteger: ResultSet.Fields[J].AsInteger := AsInteger;
                ttBoolean: ResultSet.Fields[J].AsBoolean := AsBoolean;
              End;
        End;
      End;
    End;

    { first, mark all records that must be deleted }
    For I := 1 To vSortList.Count Do
    Begin
      vSortList.Recno := I;
      K := vSortList.SourceRecno;
      If K > 0 Then
      Begin
        ResultSet.RecNo := K;
        {$if RtlVersion <= 18.5}
        ResultSet.SetSourceBookmark(TBookmark(-2)); { sourcebookmark = -2 is used to delete later }
        {$else}
        ResultSet.SetSourceBookmark(TBookmark.Create($FF, $FE)); { sourcebookmark = -2 is used to delete later } { pathced by ccy }
        {$ifend}
      End;
    End;

    { now delete not needed records }
    For I := ResultSet.RecordCount Downto 1 Do
    Begin
      ResultSet.RecNo := I;
      {$if RtlVersion <= 18.5}
      If Longint(ResultSet.GetSourceBookmark) = -2 Then
      {$else}
      B := ResultSet.GetSourceBookmark; { pathced by ccy }
      If (Length(B) > 2) and (B[0] = $FF) and (B[1] = $FE) Then { pathced by ccy }
      {$ifend}
      Begin
        ResultSet.Delete;
        FColumnList.DeleteAggregate(I);
      End;
    End;

    { HAVING predicate }
    If FHavingCol >= 0 Then
    Begin
      vColumn := FColumnList[FHavingCol];
      vTempExpr := TExprParser.Create(Self, FDefDataSet);
      Try
        For I := ResultSet.RecordCount Downto 1 Do
        Begin
          ResultSet.RecNo := I;
          vs := vColumn.ColumnExpr;
          For K := 0 To vColumn.AggregateList.Count - 1 Do
          Begin
            With vColumn.AggregateList[K] Do
            Begin
              If Aggregate = akAVG Then
              Begin
                If ResultSetHasRecords Then
                Begin
                  If SparseList.Count[I] > 0 Then
                    SparseList.Values[I] := SparseList.Values[I] / SparseList.Count[I]
                  Else
                    SparseList.Values[I] := 0;
                End;
              End;
              { replace expressions like (Aggregate 1) with its real value}
              If Aggregate = akCOUNT Then
              Begin
                Replacestring(vS, Format('{Aggregate %d}', [K]), IntToStr(SparseList.Count[I]));
              End
              Else
              Begin
                DValue:= SparseList.Values[I];
                Replacestring(vS, Format('{Aggregate %d}', [K]), FloatToStr(DValue));
              End;
              Replacestring(vS, DecimalSeparator, '.');
            End;
          End;
          vTempExpr.ParseExpression(vS);
          If vTempExpr.Expression.ExprType <> ttBoolean Then
            Raise ExQueryError.Create(SExprNotBoolean);
          If Not vTempExpr.Expression.AsBoolean Then
          Begin
            ResultSet.Delete; { delete this record }
            FColumnList.DeleteAggregate(I);
          End;
        End;
      Finally
        vTempExpr.Free;
      End;
    End;

    { now sort the Result set }
    vSortList.Clear;
    { create the fields }
    For J := 0 To FGroupByList.Count - 1 Do
      With FGroupByList[J] Do
        vSortList.AddField(ResultSet.Fields[ColIndex].DataType,
          ResultSet.Fields[ColIndex].ColWidth, False);

    For I := 1 To ResultSet.RecordCount Do
    Begin
      ResultSet.RecNo := I;
      vSortList.Insert;
      vSortList.SourceRecno := I;
      For J := 0 To FGroupByList.Count - 1 Do
        With FGroupByList[J] Do
          If Not ResultSet.Fields[ColIndex].IsNull Then
            Case ResultSet.Fields[ColIndex].DataType Of
              ttstring:
                vSortList.Fields[J].Asstring := ResultSet.Fields[ColIndex].Asstring;
              ttFloat:
                vSortList.Fields[J].AsFloat := ResultSet.Fields[ColIndex].AsFloat;
              ttInteger:
                vSortList.Fields[J].AsInteger := ResultSet.Fields[ColIndex].AsInteger;
              ttBoolean:
                vSortList.Fields[J].AsBoolean := ResultSet.Fields[ColIndex].AsBoolean;
            End;
    End;
    vSortList.Sort;
    ResultSet.SortWithList(vSortList);
    FColumnList.SortAggregateWithList(vSortList);
  Finally
    vSortList.Free;
  End;

End;

{ ORDER BY clause }

Procedure TSqlAnalizer.DoOrderBy(TheOrderList: TOrderByList);
Var
  I, J: Integer;
  vSortList: TxqSortList;
Begin
  If (ResultSet.RecordCount > 0) And (TheOrderList.Count > 0) Then
  Begin
    vSortList := CreateSortList(False);
    For I := 0 To TheOrderList.Count - 1 Do
      With TheOrderList[I] Do
        vSortList.AddField(ResultSet.Fields[ColIndex].DataType,
          ResultSet.Fields[ColIndex].ColWidth, Desc);
    Try
      For I := 1 To ResultSet.RecordCount Do
      Begin
        ResultSet.RecNo := I;
        vSortList.Insert;
        vSortList.SourceRecNo := I;
        For J := 0 To TheOrderList.Count - 1 Do
          With TheOrderList[J] Do
            If Not ResultSet.Fields[ColIndex].IsNull Then
              Case ResultSet.Fields[ColIndex].DataType Of
                ttstring:
                  vSortList.Fields[J].Asstring := ResultSet.Fields[ColIndex].Asstring;
                ttFloat:
                  vSortList.Fields[J].AsFloat := ResultSet.Fields[ColIndex].AsFloat;
                ttInteger:
                  vSortList.Fields[J].AsInteger := ResultSet.Fields[ColIndex].AsInteger;
                ttBoolean:
                  vSortList.Fields[J].AsBoolean := ResultSet.Fields[ColIndex].AsBoolean;
              End;
      End;
      vSortList.Sort;
      { now, sort the Result set }
      ResultSet.SortWithList(vSortList);
      { and the aggregates }
      FColumnList.SortAggregateWithList(vSortList);
    Finally
      vSortList.Free;
    End;
  End;
End;

Procedure TSqlAnalizer.DoUnion;
Var
  I: Integer;
  J: Integer;
  N: Integer;
Begin
  N := IMin(ResultSet.Fields.Count, FUnionAnalizer.ResultSet.Fields.Count);
  For I := 1 To FUnionAnalizer.ResultSet.RecordCount Do
  Begin
    FUnionAnalizer.ResultSet.RecNo := I;
    ResultSet.Insert;
    ResultSet.SetSourceBookmark(FUnionAnalizer.ResultSet.SourceDataset.GetBookmark);
    For J := 0 To N - 1 Do
      ResultSet.Fields[J].Asstring := FUnionAnalizer.ResultSet.Fields[J].Asstring;
  End;
End;

Procedure TSqlAnalizer.DoTransform;
Const
  MAX_AGGREGATES = 30;
Var
  vTempResultSet: TResultSet;
  I, J, vNumCol,
    vTransfAggCount,
    vCounter: Integer;
  vTemps: String;
  vIndex: Integer;
  vTempType: TExprType;
  vPivotValue: String;
  vDiff: Double;
  vPivotIndex: Integer;
  vTempField: TxqField;
  vSortList,
    vBaseSortList: TMemSortList;
  Idx: Array[0..MAX_AGGREGATES - 1] Of Integer;
  vColumn: Array[0..MAX_AGGREGATES - 1] Of TColumnItem;
  vValue,
    vSqrValue,
    vCount,
    vMin,
    vMax: Array[0..MAX_AGGREGATES - 1] Of Double;
  IsNullValue: Array[0..MAX_AGGREGATES - 1] Of Boolean;
  Analizer: TSqlAnalizer;

  Procedure Transf_CreateSelectValues( Index: Integer );
  Var
    I: Integer;
  Begin
    vSortList.RecNo := Index;
    For I := 0 To FTransfGroupedColumns - 1 Do
      With ResultSet.Fields[I] Do
        If Not IsNull Then
          Case DataType Of
            ttstring: vSortList.Fields[I].Asstring := Asstring;
            ttFloat: vSortList.Fields[I].AsFloat := AsFloat;
            ttInteger: vSortList.Fields[I].AsInteger := AsInteger;
            ttBoolean: vSortList.Fields[I].AsBoolean := AsBoolean;
          End;

    vBaseSortList.RecNo := Index;
    For I := 0 To FTransfGroupedColumns - 2 Do
      With ResultSet.Fields[I] Do
        If Not IsNull Then
          Case DataType Of
            ttstring: vBaseSortList.Fields[I].Asstring := Asstring;
            ttFloat: vBaseSortList.Fields[I].AsFloat := AsFloat;
            ttInteger: vBaseSortList.Fields[I].AsInteger := AsInteger;
            ttBoolean: vBaseSortList.Fields[I].AsBoolean := AsBoolean;
          End;
  End;

  Procedure Trans_AddNewRecord;
  Var
    J: Integer;
  Begin
    FTransfResultSet.RecNo := vCounter + 1;
    Inc( vCounter );
    { insert a new record in the new Result set }
    vTempResultSet.Insert;
    { add the select columns }
    For J := 0 To FTransfBaseColumns - 1 Do
      If Not FTransfResultSet.Fields[J].IsNull Then
        vTempResultSet.Fields[J].Asstring := FTransfResultSet.Fields[J].Asstring;
  End;

Begin
  { TRANSFORM...PIVOT section}
  If ( ResultSet.RecordCount = 0 ) Or ( Length( FPivotStr ) = 0 ) Then
    Exit;

  //ResultSet.SaveToText('c:\xquery\demos\RichardGrossman\Resultset.txt');
  //FTransfResultSet.SaveToText('c:\xquery\demos\RichardGrossman\transfrs.txt');

  ResultSet.IsSequenced := False;
  vSortList := TMemSortList.Create( False );
  vBaseSortList := TMemSortList.Create( False );
  { create the comparison fields
    used TMemSortList to do the comparison }
  For I := 0 To FTransfGroupedColumns - 1 Do
    vSortList.AddField( ResultSet.Fields[I].DataType, ResultSet.Fields[I].ColWidth, False );
  { create the base comparison fields
    used TMemSortList to do the comparison it is one less than previous sort}
  For I := 0 To FTransfGroupedColumns - 2 Do
    vBaseSortList.AddField( ResultSet.Fields[I].DataType, ResultSet.Fields[I].ColWidth, False );

  { Add two dummy records for comparing purposes only }
  vSortList.Insert;
  vSortList.Insert;

  vBaseSortList.Insert;
  vBaseSortList.Insert;

  { always create a memory Result set}
  vTempResultSet := TMemResultSet.Create;
  { create the original SELECT columns }
  For I := 0 To FTransfBaseColumns - 1 Do
  Begin
    vTempField := FTransfResultSet.Fields[I];
    With vTempField Do
      vTempResultSet.AddField( FieldName,
        FieldName,
        DataType,
        DataSize,
        SourceField,
        ReadOnly,
        CastType,
        CastLen,
        UseDisplayLabel );

  End;
  If FSubqueryInPivotPredicate Then
  Begin
    // the Result set is always the last Result set in the SQL statement
    Analizer := TSqlAnalizer( FSubQueryList[FSubQueryList.Count - 1] );
    For I := 1 To Analizer.ResultSet.RecordCount Do
    Begin
      Analizer.ResultSet.Recno := I;
      FPivotInList.Add( Analizer.ResultSet.Fields[0].Asstring );
    End;
  End
  Else If FPivotInList.Count = 0 Then
  Begin
    { Dynamics column count:
      must search all the Result set to found how many more columns to add }
    ResultSet.RecNo := 1;
    vTemps := ResultSet.Fields[FTransfBaseColumns].Asstring;
    FPivotInList.Add( vTemps );
    For vIndex := 2 To ResultSet.RecordCount Do
    Begin
      ResultSet.RecNo := vIndex;
      vTemps := ResultSet.Fields[FTransfBaseColumns].Asstring;
      If FPivotInList.IndexOf( vTemps ) = -1 Then
        FPivotInList.Add( vTemps );
    End;
    { by default, the column is sorted }
    FPivotInList.Sort;
  End;
  { Add the fixed number of columns }
  vTransfAggCount := FTransformColumnList.Count;
  For I := 0 To FPivotInList.Count - 1 Do
    For J := 0 To vTransfAggCount - 1 Do
    Begin
      vColumn[J] := FColumnList[FTransfBaseColumns + 1 + J];
      If vColumn[J].AggregateList[0].Aggregate In [akSUM, akAVG, akSTDEV, akMIN, akMAX] Then { LAS: 05-30-00 }
        vTempType := ttFloat
      Else
        vTempType := ttInteger; // the COUNT(*) aggregate
      vTempResultSet.AddField(
        FPivotInList[I], FPivotInList[I],
        vTempType,
        0,
        Nil,
        True,
        vColumn[J].CastType,
        vColumn[J].CastLen,
        False );
    End;
  For J := 0 To vTransfAggCount - 1 Do
    vColumn[J] := FColumnList[FTransfBaseColumns + 1 + J];

  ResultSet.RecNo := 1;
  vCounter := 0;
  { create the SELECT columns value for comparing }
  Transf_CreateSelectValues( 1 );
  { Get the acumulated first value }
  For J := 0 To vTransfAggCount - 1 Do
  Begin
    If vColumn[J].AggregateList[0].Aggregate = akCOUNT Then
      vValue[J] := 1 { it is a COUNT(*) }
    Else
    Begin
      Idx[J] := vColumn[J].AggregateList[0].ColIndex;
      IsNullValue[J] := ResultSet.Fields[Idx[J]].IsNull;
      vValue[J] := ResultSet.Fields[Idx[J]].AsFloat; { it is an aggregate }
      vMin[J] := vValue[J];
      vMax[J] := vValue[J];
      vCount[J] := 1; { used for obtaining the average }
      vSqrValue[J] := Sqr( vValue[J] );
    End;
  End;
  { get the value for the pivot column }
  vPivotValue := ResultSet.Fields[FTransfBaseColumns].Asstring;
  vPivotIndex := FPivotInList.IndexOf( vPivotValue );

  { the column where this aggregate goes into the Result set }
  vNumCol := FTransfBaseColumns + ( vPivotIndex * vTransfAggCount ); // + J;

  { generate a new record in the new Result set }
  Trans_AddNewRecord;

  { Iterated on all the records }
  For vIndex := 2 To ResultSet.RecordCount Do
  Begin
    ResultSet.RecNo := vIndex;
    { it is a change in SELECT columns ? }
    Transf_CreateSelectValues( 2 );
    If Not vSortList.IsEqual( 1, 2 ) Then
    Begin
      { save the last value }
      If vPivotIndex >= 0 Then
      Begin
        For J := 0 To vTransfAggCount - 1 Do
          Case vColumn[J].AggregateList[0].Aggregate Of
            akCount, akSUM:
              If Not IsNullValue[J] Then
                vTempResultSet.Fields[vNumCol + J].AsFloat := vValue[J];
            akAVG, akSTDEV: { LAS: 05-30-00 }
              If Not IsNullValue[J] Then
                If vCount[J] > 0 Then
                Begin
                  If vColumn[J].AggregateList[0].Aggregate = akAVG Then
                    vTempResultSet.Fields[vNumCol + J].AsFloat := vValue[J] / vCount[J]
                  Else
                  Begin
                    vDiff := ( vSqrValue[J] - Sqr( vValue[J] ) / vCount[J] );
                    If vDiff < 0 Then
                      vDiff := 0;
                    vTempResultSet.Fields[vNumCol + J].AsFloat := Sqrt( vDiff / ( vCount[J] - 1 ) );
                  End;
                End
                Else
                  vTempResultSet.Fields[vNumCol + J].AsFloat := 0;
            akMIN:
              If Not IsNullValue[J] Then
                vTempResultSet.Fields[vNumCol + J].AsFloat := vMin[J];
            akMAX:
              If Not IsNullValue[J] Then
                vTempResultSet.Fields[vNumCol + J].AsFloat := vMax[J];
          End;
      End;
      { Generate a new record only if select columns changed }
      If Not vBaseSortList.IsEqual( 1, 2 ) Then
        Trans_AddNewRecord
      Else
        Inc( vCounter );

      { initialize the accumulated }
      For J := 0 To vTransfAggCount - 1 Do
      Begin
        If vColumn[J].AggregateList[0].Aggregate = akCOUNT Then
          vValue[J] := 0 { it is a COUNT(*) }
        Else
        Begin
          //Idx := vColumn.AggregateList[0].ColIndex;
          vValue[J] := 0; //ResultSet.Fields[Idx].AsFloat; { it is an aggregate }
          vCount[J] := 0;
          vSqrValue[J] := 0;
        End;
        IsNullValue[J] := False;
      End;
      Transf_CreateSelectValues( 1 );
      vPivotValue := ResultSet.Fields[FTransfBaseColumns].Asstring;
      vPivotIndex := FPivotInList.IndexOf( vPivotValue );
      vNumCol := FTransfBaseColumns + ( vPivotIndex * vTransfAggCount );
    End;
    { it is a change in the pivot column ? }
    If vPivotValue <> ResultSet.Fields[FTransfBaseColumns].Asstring Then
    Begin
      { save the last value }
      If vPivotIndex >= 0 Then
      Begin
        For J := 0 To vTransfAggCount - 1 Do
          Case vColumn[J].AggregateList[0].Aggregate Of
            akCount, akSUM:
              If Not IsNullValue[J] Then
                vTempResultSet.Fields[vNumCol + J].AsFloat := vValue[J];
            akAVG, akSTDEV: { LAS: 05-30-00 }
              If Not IsNullValue[J] Then
                If vCount[J] > 0 Then
                Begin
                  If vColumn[J].AggregateList[0].Aggregate = akAVG Then
                  Begin
                    vTempResultSet.Fields[vNumCol + J].AsFloat := vValue[J] / vCount[J]
                  End
                  Else
                  Begin
                    vDiff := ( vSqrValue[J] - Sqr( vValue[J] ) / vCount[J] );
                    If vDiff < 0 Then
                      vDiff := 0;
                    vTempResultSet.Fields[vNumCol + J].AsFloat := Sqrt( vDiff / ( vCount[J] - 1 ) );
                  End;
                End
                Else
                  vTempResultSet.Fields[vNumCol + J].AsFloat := 0;
            akMIN:
              If Not IsNullValue[J] Then
                vTempResultSet.Fields[vNumCol + J].AsFloat := vMin[J];
            akMAX:
              If Not IsNullValue[J] Then
                vTempResultSet.Fields[vNumCol + J].AsFloat := vMax[J];
          End;
      End;
      { initialize the accumulated }
      For J := 0 To vTransfAggCount - 1 Do
      Begin
        If vColumn[J].AggregateList[0].Aggregate = akCOUNT Then
          vValue[J] := 1 { it is a COUNT(*) }
        Else
        Begin
          Idx[J] := vColumn[J].AggregateList[0].ColIndex;
          IsNullValue[J] := ResultSet.Fields[Idx[J]].IsNull;
          vValue[J] := ResultSet.Fields[Idx[J]].AsFloat; { it is an aggregate }
          vCount[J] := 1;
          vSqrValue[J] := Sqr( vValue[J] );
        End;
      End;
      vPivotValue := ResultSet.Fields[FTransfBaseColumns].Asstring;
      vPivotIndex := FPivotInList.IndexOf( vPivotValue );
      vNumCol := FTransfBaseColumns + ( vPivotIndex * vTransfAggCount );
    End
    Else
    Begin
      For J := 0 To vTransfAggCount - 1 Do
        If vColumn[J].AggregateList[0].Aggregate = akCOUNT Then
          vValue[J] := vValue[J] + 1
        Else
        Begin
          Idx[J] := vColumn[J].AggregateList[0].ColIndex;
          IsNullValue[J] := IsNullValue[J] Or ResultSet.Fields[Idx[J]].IsNull;
          vValue[J] := vValue[J] + ResultSet.Fields[Idx[J]].AsFloat;
          vSqrValue[J] := vSqrValue[J] + Sqr( ResultSet.Fields[Idx[J]].AsFloat );
          If vCount[J] = 0 Then
          Begin
            vMin[J] := ResultSet.Fields[Idx[J]].AsFloat;
            vMax[J] := ResultSet.Fields[Idx[J]].AsFloat;
          End
          Else
          Begin
            vMin[J] := Min( ResultSet.Fields[Idx[J]].AsFloat, vMin[J] );
            vMax[J] := Max( ResultSet.Fields[Idx[J]].AsFloat, vMax[J] );
          End;
          vCount[J] := vCount[J] + 1;
        End;
    End;
  End;
  { assign the start value to the corresponding column }
  If vPivotIndex >= 0 Then
  Begin
    vNumCol := FTransfBaseColumns + ( vPivotIndex * vTransfAggCount );
    For J := 0 To vTransfAggCount - 1 Do
      Case vColumn[J].AggregateList[0].Aggregate Of
        akCount, akSUM:
          If Not IsNullValue[J] Then
            vTempResultSet.Fields[vNumCol + J].AsFloat := vValue[J];
        akAVG, akSTDEV: { LAS: 05-30-00 }
          If Not IsNullValue[J] Then
            If vCount[J] > 0 Then
            Begin
              If vColumn[J].AggregateList[0].Aggregate = akAVG Then
              Begin
                vTempResultSet.Fields[vNumCol + J].AsFloat :=
                  vValue[J] / vCount[J]
              End
              Else
              Begin
                vDiff := ( vSqrValue[J] - Sqr( vValue[J] ) / vCount[J] );
                If vDiff < 0 Then
                  vDiff := 0;
                vTempResultSet.Fields[vNumCol + J].AsFloat :=
                  Sqrt( vDiff / ( vCount[J] - 1 ) );
              End;
            End
            Else
              vTempResultSet.Fields[vNumCol + J].AsFloat := 0;
        akMIN:
          If Not IsNullValue[J] Then
            vTempResultSet.Fields[vNumCol + J].AsFloat := vMin[J];
        akMAX:
          If Not IsNullValue[J] Then
            vTempResultSet.Fields[vNumCol + J].AsFloat := vMax[J];
      End;
  End;

  { assign the new Result set }
  FResultSet.Free;
  FResultSet := vTempResultSet;
  vSortList.Free; { free temporary sort list used for comparisons }
  vBaseSortList.Free;
  FreeObject( FTransfResultSet );
End;

Procedure TSqlAnalizer.CreateResultSet;
Var
  Column            : TColumnItem;
  Index             : Integer;
  I, J, K, L,
  n, JS             : Integer;
  ps                : Integer;
  Pivot             : Integer;
  IntValue          : Integer;
  NumAccepted       : Integer;
  MustAccept        : Integer;
  NumAsc            : Integer;
  NumDesc           : Integer;
  ProgressMin       : Integer;
  ProgressMax       : Integer;
  ProgressPosition  : Integer;
  TmpPosition       : Integer;
  Range, Progress   : Integer;
  Position          : Integer;
  StartOptimize     : Integer;
  EndOptimize       : Integer;
  TmpDataSet        : TDataSet;
  EventWasCalled    : Boolean;
  TmpDeleted        : Boolean;
  Pass              : Boolean;
  Accepted          : Boolean;
  TempBool          : Boolean;
  Succeed           : Boolean;
  FilterActive      : Boolean;
  CheckExpr,
  SubqExpr,
  TempExpr          : TExprParser;
  Value             : Double;
  TempS             : String;
  Lasts             : String;
  S                 : String;
  TableName         : String;
  TempList          : TList;
  OldCursor         : TCursor;
  ReadOnly          : Boolean;
  Cancel            : Boolean;
  Canceled          : Boolean;
  F                 : TField;
  WhereOptimize     : TWhereOptimizeItem;
  SortList          : TxqSortList;
  TempAnalizer      : TSqlAnalizer;
  TempField         : TxqField;
  TempResultSet     : TResultSet;
  OptimizeList      : TWhereOptimizeList;
  TmpAnalizer       : TSqlAnalizer;
  IsDone,
  IsUnionStatement  : Boolean;
  ExprType          : TExprType;
  B                 : TBookmark; { patched by ccy }
  iSize             : Integer; { patched by ccy }

  Procedure RecursiveSolveSubqueries(n: integer; Const s: String);
  Var
    j, Count  : integer;
    Analizer  : TSqlAnalizer;
    tmp       : String;
  Begin
    { warning! a combination of any and all is not supported with multiple
      subqueries, example:
      select distinct adr_id as id from amr where
      (adr_id in any(select adr_id from amr where mer_id = 5)) and
      (adr_id in all(select adr_id from amr where mer_id = 6))
    }
    Analizer := TSqlAnalizer(FSubQueryList[n]);
    For j := 1 To Analizer.ResultSet.RecordCount Do
    Begin
      Analizer.ResultSet.RecNo := j;
      tmp := s;
      With Analizer.ResultSet Do
        Case Fields[0].DataType Of
          ttstring:
            Replacestring(tmp,
              Format('(Subquery %d)', [n]),
              AddCorrectStrDelim(Fields[0].Asstring));
          ttFloat, ttInteger, ttBoolean:
            Replacestring(tmp,
              Format('(Subquery %d)', [n]),
              Fields[0].Asstring);
        End;
      Count := FSubQueryList.Count - 1;
      If FSubqueryInPivotPredicate Then
        Dec(Count);
      If (n + 1) <= Count Then
      Begin
        RecursiveSolveSubqueries(n + 1, tmp);
        If IsDone Then
          Exit;
      End
      Else
      Begin
        // the end of the list, then evaluate the expression
        SubqExpr.ParseExpression(tmp);
        If SubqExpr.Expression.ExprType <> ttBoolean Then
          Raise ExQueryError.Create(SExprNotBoolean);
        If SubqExpr.Expression.AsBoolean = true Then
        Begin
          Inc(NumAccepted);
          If TSubqueryKind(FSubQueryKindList[n]) = skAny Then
          Begin
            IsDone := True;
            Break;
          End;
        End
        Else If TSubqueryKind(FSubQueryKindList[n]) = skAll Then
        Begin
          IsDone := True;
          Break;
        End;
      End;
    End;
  End;

Begin
  TmpDataSet := Nil;
  WhereOptimize := Nil;
  ProgressPosition := 0;
  ProgressMin := 0;
  ProgressMax := 0;

  For I := 0 To FSubQueryList.Count - 1 Do
    TSqlAnalizer(FSubQueryList[I]).CreateResultSet; { exception will be raised on error }

  { LAS: 4/jun/2003 }
  IsUnionStatement := (FStatement = ssUnion) ;
  if IsUnionStatement then
    FStatement:= ssSelect;

  If Not CheckIntegrity Then Exit;

  { first DataSet listed in SQL is the DataSet that will be joined to other
    DataSets (if join was defined in sql) }
  If Not (FStatement = ssInsert) Then
    FDefDataSet := FTableList[0].DataSet
  Else
    FDefDataSet := FInsertList[0].DataSet;

  { SELECT: Create all columns expressions }
  Try
    n := FColumnList.Count - 1; { don't move this }
    For I := 0 To n Do
    Begin
      Column := FColumnList[I];
      Column.Resolver := TExprParser.Create(Self, FDefDataSet);
      If Column.AggregateList.Count = 0 Then
      Begin
        If Column.SubqueryList.Count = 0 Then
        begin
          Column.Resolver.ParseExpression(Column.ColumnExpr)
        End Else
        Begin
          { First create a dummy expression in order to detect the column type }
          S := Column.ColumnExpr;
          For J := 0 To Column.SubqueryList.Count - 1 Do
          Begin
            TempAnalizer := TSqlAnalizer(Column.SubqueryList[J]);
            { create the Result set ! }
            TempAnalizer.CreateResultSet;
            Case TempAnalizer.ResultSet.Fields[0].DataType Of
              ttstring: Replacestring(S, Format('{Subquery %d}', [J]), '" "');
              ttFloat: Replacestring(S, Format('{Subquery %d}', [J]), '1.0');
              ttInteger: Replacestring(S, Format('{Subquery %d}', [J]), '1');
              ttBoolean:
                Replacestring(S, Format('{Subquery %d}', [J]), 'True');
            End;
          End;
          Column.Resolver.ParseExpression(S);
        End;
      End
      Else
      Begin
        { First create a dummy expression in order to detect the column type }
        S := Column.ColumnExpr;
        For J := 0 To Column.AggregateList.Count - 1 Do
          Replacestring(S, Format('{Aggregate %d}', [J]), Column.AggregateList[j].AggregateStr);
        Column.Resolver.ParseExpression(S);
        { For all the aggregates in this column, add a new temporary column }
        For J := 0 To Column.AggregateList.Count - 1 Do
        Begin
          With FColumnList.Add Do
          Begin
            ColumnExpr := Column.AggregateList[J].AggregateStr;
            IsTemporaryCol := (Length(FPivotStr) = 0); {TRANSFORM...PIVOT special case}
            Resolver := TExprParser.Create(Self, FDefDataSet);
            Resolver.ParseExpression(ColumnExpr)
          End;
          Column.AggregateList[J].ColIndex := FColumnList.Count - 1;
        End;
      End;
    End;
  Except
    For I := 0 To FColumnList.Count - 1 Do
      With FColumnList[I] Do
        If Assigned(Resolver) Then
        Begin
          Resolver.Free;
          Resolver := Nil;
        End;
    Raise;
  End;

  { UPDATE: create all columns expressions }
  Try
    If (FUpdateColumnList.Count > 0) And (FUpdateColumnList.SyntaxUsed = 0) Then
      For I := 0 To FUpdateColumnList.Count - 1 Do
      Begin
        With FUpdateColumnList[I] Do
        Begin
          if Length(ColExpr) > 0 Then
          Begin
            Resolver := TExprParser.Create(Self, FDefDataSet);
            Resolver.ParseExpression(ColExpr);
          End;
        End;
      End;
  Except
    For I := 0 To FUpdateColumnList.Count - 1 Do
      With FUpdateColumnList[I] Do
        If Assigned(Resolver) Then
        Begin
          Resolver.Free;
          Resolver := Nil;
        End;
    Raise;
  End;

  { INSERT : create all values list }
  If FStatement = ssInsert Then
  Begin
    For L := 0 To FInsertList.Count - 1 Do
    Begin
      For I := 0 To FInsertList[L].ExprList.Count - 1 Do
      Begin
        If AnsiCompareText(FInsertList[L].ExprList[I], 'NULL') = 0 Then
        Begin
          FInsertList[L].ResolverList.Add(Nil);
          Continue;
        End;
        TempExpr := TExprParser.Create(Self, FInsertList[L].DataSet);
        Try
          TempExpr.ParseExpression(FInsertList[L].ExprList[I]);
          FInsertList[L].ResolverList.Add(TempExpr);
        Except
          TempExpr.Free;
          Raise;
        End;
      End;
    End;
  End;

  { set a filter on this dataset }
  FilterActive := False;
  If (FxQuery.WhereOptimizeMethod = omSetFilter) And
     (Length(FWhereFilter) > 0) And Assigned(FxQuery.OnSetFilter) Then
  Begin
    { you must catch the exception in the event handler in order to
      continue processing the query }
    FxQuery.OnSetFilter(FxQuery, FDefDataSet, FWhereFilter, False, FilterActive);
    If FilterActive Then
    Begin
      FWhereOptimizeList.Clear; { assumed the filter expression is OK }
      FWhereStr := ''; { where expression canceled also }
    End;
  End;

  OptimizeList := FWhereOptimizeList;

  { for to use in subqueries }
  SubqExpr := TExprParser.Create(Self, FDefDataSet);

  { Create the apropriate TResultSet descendant }
  InitializeResultSet;

  { used after Result set is created }
  ResultSet.SourceDataSet := FDefDataSet;

  { SELECT: Create Result set fields }
  For I := 0 To FColumnList.Count - 1 Do
  Begin
    Column := FColumnList[I];
    S := Column.ColumnExpr;
    F := Nil;
    ReadOnly := True;
    if Column.Resolver.Expression = nil then Continue;
    ExprType := Column.Resolver.Expression.ExprType;
    If (Column.CastType = 0) And (Column.SubqueryList.Count = 0) Then
    Begin
      If Column.AggregateList.Count > 0 Then
      Begin
        For J := 0 To Column.AggregateList.Count - 1 Do
          Replacestring(S, Format('{Aggregate %d}', [J]), Column.AggregateList[J].AggregateStr);
      End;
      For J := 0 To FTableList.Count - 1 Do
      Begin
        TmpDataSet := FTableList[J].DataSet;
        CheckExpr := TExprParser.Create(Self, TmpDataSet);
        Try
          Try
            { is field defined in expression valid for the table ? }
            If CheckExpr.CheckExpression(S) Then
            Begin
              With CheckExpr.CheckData Do
              Begin
                If (RefCount = 1) And Not HasMoreFunctions And
                  (CheckExpr.CheckData.Field <> Nil) Then
                Begin
                  F := CheckExpr.CheckData.Field;
                  If CheckExpr.Expression.ExprType = ttBoolean Then
                  Begin
                    If F.DataType <> ftBoolean Then
                      F := Nil;
                  End;
                  {case vCheckExpr.Expression.ExprType of
                    ttstring: ;
                    ttFloat: ;
                    ttInteger: ;
                    ttBoolean:
                      if vF.DataType <> ftBoolean then vF:= nil;
                  end;}
                End;
              End;
              If TmpDataSet = FDefDataSet Then
                ReadOnly := False; { means that can later be modified }
              Break;
            End;
          Except
            { ignore exception (not bad because I'm only checking expression) }
          End;
        Finally
          CheckExpr.Free;
        End;
      End;
    End;

    If Column.Resolver.Expression.ExprType = ttstring Then
    Begin
      CheckExpr := TExprParser.Create(Self, TmpDataSet);
      Try
        CheckExpr.ParseExpression(S);
        iSize := SizeOf(AnsiChar);                                { patched by ccy }
        if Assigned(CheckExpr.CheckData.Field) and (CheckExpr.CheckData.Field.DataType = ftWideString) then { patched by ccy }
          iSize := SizeOf(WideChar);                              { patched by ccy }
        n := CheckExpr.Expression.MaxLen + 1 * iSize;             { patched by ccy }
      Finally
        CheckExpr.Free;
      End;
    End
    Else
      n := 0;
    With Column Do
      ResultSet.AddField(AsAlias, ColumnExpr, ExprType, n, F, ReadOnly,
        CastType, CastLen, (Not IsAsExplicit) And FxQuery.UseDisplayLabel);
  End;

  { pre-create WhereExpr }
  FMainWhereResolver := TExprParser.Create(Self, FDefDataSet);

  { pre-create FJoinInWhereResolver}
  If FIsJoinInWhere Then
    FJoinInWhereResolver := TExprParser.Create(Self, FDefDataSet); { used in FJoinInWhereExpres }

  If FxQuery.ShowWaitCursor Then
  Begin
    OldCursor := Screen.Cursor;
    Screen.Cursor := crSQLWait;
  End;
  Try

    { INSERT statement }
    If FStatement = ssInsert Then
    Begin
      DoInsertStatement;
      Exit;
    End;

    If (FStatement = ssUpdate) And (FUpdateColumnList.SyntaxUsed = 1) Then
    Begin
      DoMassiveUpdates;
      Exit;
    End;

    { pre-evaluate where expression (if possible) }
    If (FSubQueryList.Count = 0) And (Length(FWhereStr) > 0) Then
    Begin
      FMainWhereResolver.ParseExpression(FWhereStr);
      If FMainWhereResolver.Expression.ExprType <> ttBoolean Then
        Raise ExQueryError.Create(SExprNotBoolean);
    End;

    If FIsJoinInWhere Then
    Begin
      FJoinInWhereResolver.ParseExpression(FJoinInWhereExpres);
      If FJoinInWhereResolver.Expression.ExprType <> ttBoolean Then
        Raise ExQueryError.Create(SExprNotBoolean);
    End;

    ResultSet.IsSequenced := (FStatement = ssSelect);

    { Check WHERE clause sections that can be optimized by using indexes
      JOINing has precedence over WHERE statement
      WHERE statement has precedence over an ORDER BY }

    EventWasCalled := False;
    StartOptimize := 0;
    EndOptimize := 0;

    { end-user defined a range ? }
    If Not (FStatement = ssInsert) And
      (UserDefinedRange.StartValues.Count > 0) And Assigned(FxQuery.OnSetUserRange) Then
    Begin
      FxQuery.OnSetUserRange(FxQuery, FDefDataSet, UserDefinedRange.UsingIndex,
        UserDefinedRange.ForFields, UserDefinedRange.StartValues,
        UserDefinedRange.EndValues);
    End;

    If (FxQuery.WhereOptimizeMethod = omSetRange) And
       (FStatement In [ssSelect, ssUpdate]) And
       (FJoinList.Count = 0) And (FWhereOptimizeList.Count > 0) And
       Assigned(FxQuery.OnIndexNeededFor) And Assigned(FxQuery.OnSetRange) Then
    Begin
      If FWhereOptimizeList.Count <> 1 Then
        { cannot optimize when there is more than one possible optimization options }
        FWhereOptimizeList.Clear;
      If FWhereOptimizeList.Count > 0 Then
      Begin
        With FWhereOptimizeList[0] Do { TWhereOptimize}
        Begin
          If CanOptimize Then
          Begin
            If Not ((AnsiPos('(Subquery', RangeStart) > 0) Or
              (AnsiPos('(Subquery', RangeEnd) > 0)) Then
            Begin
              TempBool := CanOptimize;
              FxQuery.OnIndexNeededFor(FxQuery, DataSet, FieldNames, False, False, TempBool);
              CanOptimize := TempBool;
              If CanOptimize Then
              Begin
                EventWasCalled := True;
              End;
            End;
          End;
        End;
      End;
    End Else If (FxQuery.WhereOptimizeMethod = omSetRange) And
      (FStatement In [ssSelect, ssUpdate]) And (FJoinList.Count = 0) And
      (FOrderByList.Count > 0) And Assigned(FxQuery.OnIndexNeededFor) Then
    Begin
      { Check if the ordering can be optimized }
      TempS := '';
      NumAsc := 0;
      NumDesc := 0;
      For I := 0 To FOrderByList.Count - 1 Do
      Begin
        With FOrderByList[I] Do { TOrderByItem }
        Begin
          If Desc Then
            Inc(NumDesc)
          Else
            Inc(NumAsc);
          Column := FColumnList[ColIndex];
        End;
        If I < FOrderByList.Count - 1 Then
          TempS := TempS + Column.AsAlias + ';'
        Else
          TempS := TempS + Column.AsAlias;
      End;
      If NumAsc = FOrderByList.Count Then { only ascending sort for now }
      Begin
        { ascending homogeneous sort (not supported descending for optimizing) }
        Accepted := False;
        FxQuery.OnIndexNeededFor(FxQuery, FDefDataSet, TempS, True, False, Accepted);
        If Accepted Then
        Begin
          WhereOptimizeList.Clear; { cannot optimize both: where and ordering (for now) }
          OrderByList.Clear;
        End;
      End;
      TempS := '';
    End;

    { progress status }
    Position := 1;
    If Assigned(FxQuery.FOnProgress) Then
    Begin
      ProgressMin := 1;
      ProgressPosition := 1;
      ProgressMax := FDefDataSet.RecordCount;
      FxQuery.FOnProgress(FxQuery, psXStart, 1, FDefDataSet.RecordCount, 1);
    End;

    Canceled := False;
    { now generate all rows }
    For I := StartOptimize To EndOptimize Do
    Begin
      If (xQuery.WhereOptimizeMethod = omSetRange) And EventWasCalled Then
      Begin
        { Request to user to set a range to optimize }
        WhereOptimize := FWhereOptimizeList[I];
        With WhereOptimize Do { TWhereOptimizeItem }
        Begin
          If CanOptimize Then
          Begin
            TempBool := CanOptimize;
            FxQuery.OnIndexNeededFor(FxQuery,
              DataSet,
              FieldNames,
              True,
              False,
              TempBool);
            CanOptimize := TempBool;
            If CanOptimize Then
              FxQuery.OnSetRange(FxQuery,
                RelOperator,
                DataSet,
                FieldNames,
                RangeStart,
                RangeEnd,
                False);
          End;
        End;
      End;

      FDefDataSet.First;
      While Not FDefDataSet.EOF Do
      Begin
        If FJoinList.Count > 0 Then
        Begin
          { join records }
          If (FMainWhereResolver.Expression <> Nil) And FWhereContainsOnlyBasicFields Then
          Begin
            If FMainWhereResolver.Expression.AsBoolean Then
              FJoinList.DoJoinOn;
          End
          Else
            FJoinList.DoJoinOn;
        End
        Else If FIsJoinInWhere Then
          DoJoinInWhere
        Else
        Begin
          { WHERE clause without JOIN ON clause }
          Accepted := True;
          If ((FSubqueryInPivotPredicate = true) And
            (FSubQueryList.Count > 1)) Or ((FSubqueryInPivotPredicate = False)
            And (FSubQueryList.Count > 0)) Then { is a WHERE with subqueries? }
          Begin
            NumAccepted := 0;
            IsDone := False;
            RecursiveSolveSubqueries(0, FWhereStr);

            Case TSubqueryKind(FSubQueryKindList[0]) Of
              skAny:
                Accepted := (NumAccepted > 0);
              skAll:
                Begin
                  MustAccept := 0;
                  N := FSubQueryList.Count - 1;
                  If FSubqueryInPivotPredicate Then
                    Dec(N);
                  For j := 0 To N Do
                  Begin
                    TmpAnalizer := TSqlAnalizer(FSubQueryList[j]);
                    Inc(MustAccept, TmpAnalizer.ResultSet.RecordCount);
                  End;
                  Accepted := (NumAccepted = MustAccept);
                End;
            End;
          End
          Else If Length(FWhereStr) > 0 Then
          Begin
            { process WHERE clause with normal expression (without subquery) }
            {if (FTableList.Count = 2) and (FJoinList.Count = 0) then begin
               // special case
               AuxDataset := FTableList[1].DataSet;
               AuxDataset.First;
               while not AuxDataset.Eof do begin

               end;
            end else }
            Accepted := FMainWhereResolver.Expression.AsBoolean;
          End;
          If Accepted Then
          Begin
            Case FStatement Of
              { DataSet optionally must support RecNo property  }
              ssSelect:
                AddThisRecord(DefDataSet);
              ssUpdate:
                DoUpdateRecord;
              ssDelete:
                Begin
                  FDefDataSet.Delete;
                  Inc(FxQuery.FRowsAffected);
                End;
            End;
          End;
        End;

        If Not (Accepted And (FStatement = ssDelete)) Then
          FDefDataSet.Next; { delete moves to the next record automatically }

        If Assigned(FxQuery.OnProgress) Then
        Begin
          Inc(Position);
          Range := ProgressMax - ProgressMin;
          TmpPosition := MulDiv(ProgressPosition, 100, Range);
          Progress := MulDiv(Position, 100, Range);
          If Progress > TmpPosition + 5 Then
          Begin
            ProgressPosition := Position;
            FxQuery.OnProgress(FxQuery, psXProgress, 0, 0, Position);
          End;
        End;
        If Assigned(FxQuery.OnCancelQuery) Then
        Begin
          Cancel := False;
          FxQuery.OnCancelQuery(FxQuery, Cancel);
          If Cancel Then
          Begin
            Canceled := True;
            Break;
          End;
        End;
      End;

      { cancel ranges (dataset is back to its normal state) }
      If (FxQuery.WhereOptimizeMethod = omSetRange) And EventWasCalled
        And WhereOptimize.CanOptimize And Assigned(FxQuery.OnCancelRange) Then
        FxQuery.OnCancelRange(FxQuery, WhereOptimize.DataSet, False);

      If Canceled Then
        Break;

    End;

    { cancel filters }
    If (FxQuery.WhereOptimizeMethod = omSetFilter) And
      FilterActive And Assigned(FxQuery.OnCancelFilter) Then
      FxQuery.OnCancelFilter(FxQuery, FDefDataSet, False);

    { cancel user defined range }
    If Not (FStatement = ssInsert) And (UserDefinedRange.StartValues.Count > 0)
      And Assigned(FxQuery.OnCancelUserRange) Then
      FxQuery.OnCancelUserRange(FxQuery, FDefDataSet);

    If Canceled Then Exit;

    If Length(FPivotStr) = 0 Then
    Begin
      // GROUP BY execute
      DoGroupBy;
      // ORDER BY... execute
      DoOrderBy(Self.FOrderByList);
    End Else
    Begin
      { ORDER BY... execute }
      if Self.fOrderByList.Count = 0 then  { patched by ccy }
        DoOrderBy(Self.fGroupByList)       { patched by ccy }
      else
        DoOrderBy(Self.fOrderByList);

      { copy the Result set to a temporary memory Result set }
      FTransfResultSet := TMemResultSet.Create;
      { first, create the original SELECT columns }
      For J := 0 To ResultSet.Fields.Count - 1 Do
      Begin
        TempField := ResultSet.Fields[J];
        With TempField Do
          FTransfResultSet.AddField(FieldName,
            FieldName,
            DataType,
            DataSize,
            SourceField,
            ReadOnly,
            CastType,
            CastLen,
            UseDisplayLabel);

      End;
      { now copy from one Result set to another }
      For J := 1 To ResultSet.RecordCount Do
      Begin
        ResultSet.RecNo := J;
        { NOTE: due to this, it is not allowed to include blob/memo fields
          in the Result set when using TRANSFORM...PIVOT (obviously)

          NOTE: also, there is no possible to have a SELECT with DISTINCT because
          it is calculated later in this same method. }
        FTransfResultSet.Insert;
        For K := 0 To ResultSet.Fields.Count - 1 Do
          If Not ResultSet.Fields[K].IsNull Then
            FTransfResultSet.Fields[K].Asstring := ResultSet.Fields[K].Asstring;
      End;
      { now a little trick to group the transform Result set }
      TempResultSet := ResultSet;
      ResultSet := FTransfResultSet;
      Try
        DoGroupBy;
        DoOrderBy(Self.FOrderByList);
      Finally
        { restore }
        ResultSet := TempResultSet;
      End;
      { explain to what follows: in TRANSFORM, the group by is changed to an
        ORDER BY }
      With FGroupByList.Add Do
        ColIndex := FTransfBaseColumns;
      DoOrderBy( FGroupByList );
    End;

    { SELECT DISTINCT... syntax }
    If FIsDistinct Then
    Begin
      SortList := CreateSortList(False);
      Try
        For J := 0 To FColumnList.Count - 1 Do
          SortList.AddField(ResultSet.Fields[J].DataType,
            ResultSet.Fields[J].ColWidth, False);
        For I := 1 To ResultSet.RecordCount Do
        Begin
          ResultSet.RecNo := I;
          SortList.Insert;
          SortList.SourceRecno := I;
          For J := 0 To FColumnList.Count - 1 Do
          Begin
            If Not ResultSet.Fields[J].IsNull Then
            Begin
              Case ResultSet.Fields[J].DataType Of
                ttstring:
                  SortList.Fields[J].Asstring := ResultSet.Fields[J].Asstring;
                ttFloat:
                  SortList.Fields[J].AsFloat := ResultSet.Fields[J].AsFloat;
                ttInteger:
                  SortList.Fields[J].AsInteger := ResultSet.Fields[J].AsInteger;
                ttBoolean:
                  SortList.Fields[J].AsBoolean := ResultSet.Fields[J].AsBoolean;
              End;
            End;
          End;
        End;

        SortList.Sort;

        { mark records that must be deleted }
        For I := SortList.Count Downto 2 Do
          If SortList.IsEqual(I, I - 1) Then
          Begin
            SortList.Recno := I;
            ResultSet.RecNo := SortList.SourceRecno;
            {$if RtlVersion <= 18.5}
            ResultSet.SetSourceBookmark(TBookmark(-2));
            {$else}
            ResultSet.SetSourceBookmark(TBookmark.Create($FF, $FE)); { patched by ccy }
            {$ifend}
          End;

        { now, delete the records }
        For I := ResultSet.RecordCount Downto 1 Do
        Begin
          ResultSet.RecNo := I;
          {$if RtlVersion <= 18.5}
          If Longint(ResultSet.GetSourceBookmark) = -2 Then
          {$else}
          B := ResultSet.GetSourceBookmark;
          If (Length(B) > 2) and (B[0] = $FF) and (B[1] = $FE) Then { patched by ccy }
          {$ifend}
          Begin
            ResultSet.Delete;
            FColumnList.DeleteAggregate(I);
          End;
        End;
      Finally
        SortList.Free;
      End;
    End;

    { Now the TOP N stuff. Ex.: SELECT TOP 10 FROM Customer ORDER BY Custno }
    If (FTopNInGroupBy = 0) And  { Cannot mix TOP N in select and in Group By }
       (FTopNInSelect > 0) And (ResultSet.RecordCount > FTopNInSelect) then
    begin
      For I := ResultSet.RecordCount Downto (FTopNInSelect + 1) Do
      Begin
        ResultSet.Recno := I;
        ResultSet.Delete;
      End;
    end;

    { Now delete temporary column from the Result set
      temporary columns are used temporarily having columns and aggregate columns }
    For I := FColumnList.Count - 1 Downto 0 Do
      If FColumnList[I].IsTemporaryCol Then
      Begin
        ResultSet.Fields.Delete(I);
        FColumnList.Delete(I);
      End;

    { TRANSFORM...PIVOT statement }
    DoTransform;

  Finally
    FJoinList.Clear;
    FMainWhereResolver.Free;
    If FIsJoinInWhere Then
      FJoinInWhereResolver.Free;
    SubqExpr.free;
    If Assigned(FxQuery.FOnProgress) Then
      FxQuery.FOnProgress(FxQuery, psXEnd, 0, 0, 0);
    If FxQuery.ShowWaitCursor Then
    Begin
      Screen.Cursor := OldCursor;
    End;
  End;

  { LAS: 4/jun/2003 }
  if IsUnionStatement And Assigned(UnionAnalizer) Then
  Begin
    UnionAnalizer.Statement := ssSelect;
    UnionAnalizer.CreateResultSet;
    DoUnion;
  End;
End;

Function TSqlAnalizer.FindFieldByName(Const FieldName: String): TField;
Var
  I: Integer;
  F: TField;
Begin
  Result := Nil;
  For I := 0 To FTableList.Count - 1 Do
  Begin
    F := FTableList[I].DataSet.FindField(FieldName);
    If Assigned(F) Then
    Begin
      Result := F;
      Break;
    End;
  End;
End;

{function TSqlAnalizer.GetFieldName(const value: string): string;
var
  p: Integer;
begin
  p:= Pos('.', value);
  if p=0 then
    Result:= value
  else
    Result:= Copy(value,p+1,Length(value));
end; }

Function TSqlAnalizer.GetRealTableName(Const tn: String; Var Index: Integer): String;
Var
  I: Integer;
Begin
  Result := tn;
  Index:= -1;
  For I := 0 To FTableList.Count - 1 Do
    With FTableList[I] Do
      If (AnsiCompareText(TableName, tn) = 0) Then
      begin
        Index:= I;
        Exit;
      end Else If (AnsiCompareText(Alias, tn) = 0) Then
      Begin
        Result := TableName;
        Index:= I;
        Exit;
      End;
End;

Function TSqlAnalizer.GetRealTableAlias(Const tn: String): String;
Var
  I: Integer;
Begin
  Result := tn;
  For I := 0 To FTableList.Count - 1 Do
    With FTableList[I] Do
      If (AnsiCompareText(TableName, tn) = 0) Then
      Begin
        Result := Alias;
        Exit;
      End
      Else If (AnsiCompareText(Alias, tn) = 0) Then
      Begin
        Exit;
      End;
End;

Function TSqlAnalizer.IsValidFieldName(Const FieldName: String;
  CheckInPrimary: Boolean): Boolean;
Var
  Alias: String;
  FldNam: String;
  P: Integer;
  Dataset: TDataset;
Begin
  Result := False;
  P := AnsiPos('.', FieldName);
  If P = 0 Then
  Begin
    Alias := '';
    FldNam := FieldName;
  End
  Else
  Begin
    Alias := Copy(FieldName, 1, P - 1);
    FldNam := Copy(FieldName, P + 1, Length(FieldName));
  End;
  If CheckInPrimary Then
  Begin
    If Length(Alias) > 0 Then
    Begin
      If (AnsiComparetext(FTableList[0].TableName, Alias) = 0) Or
         (AnsiComparetext(FTableList[0].Alias, Alias) = 0) Then
        Dataset := FDefDataSet
      Else
        Dataset := Nil;
    End
    Else
      Dataset := FDefDataSet;
  End
  Else
  Begin
    If Length(Alias) > 0 Then
      Dataset := FindDatasetByName(Alias)
    Else
      Dataset := FDefDataSet;
  End;
  If Dataset = Nil Then Exit;
  Result := Dataset.FindField(FldNam) <> Nil;
End;

Function TSqlAnalizer.QualifiedField(Const FieldName: String; UseAlias: Boolean): String;
Var
  I, P, J, K, Index: Integer;
  F: TField;
  TableItem: TTableItem;
  S, tname, fName: String;
  FieldFound: Boolean;
  Found: Boolean;

  Function FindColumnExplicitAlias(Const fldname: String): Integer;
  Var
    I: Integer;
  Begin
    Result := -1;
    For I := 0 To FColumnList.Count - 1 Do
      With FColumnList[I] Do
      Begin
        If IsAsExplicit And
          (AnsiCompareText(Format('\f"%s"', [fldname]), ColumnExpr) <> 0) And
          (AnsiCompareText(fldname, AsAlias) = 0) Then
        Begin
          Result := I;
          Exit;
        End;
      End;
  End;

Begin
  Result := FieldName;
  If Length(Trim(FieldName)) = 0 Then Exit;
  P := AnsiPos('\f"', Result);
  While (p > 0) Do
  Begin
    K := p + 3;
    FieldFound := False;
    While K <= Length(Result) Do
    Begin
      If Result[K] = '"' Then
      Begin
        FieldFound := True;
        s := Copy(Result, p + 3, K - (p + 3));
        J := AnsiPos('.', s);
        If J = 0 Then
        Begin
          Found := False;
          Index := FindColumnExplicitAlias(s);
          { if one column alias exists with same fieldname then use original field expression }
          If (Index >= 0) And
            (AnsiPos('{Aggregate', FColumnList[Index].ColumnExpr) = 0) Then
          Begin
            Result:= StringReplace(Result, Format('\f"%s"', [s]),
              FColumnList[Index].ColumnExpr, [rfReplaceAll, rfIgnoreCase]);
            Found := True;
          End
          Else
          Begin
            For I := 0 To FTableList.Count - 1 Do
            Begin
              TableItem := FTableList[I];
              F := TableItem.DataSet.FindField(TrimSquareBrackets(s));
              If Assigned(F) Then
              Begin
                If UseAlias Then
                  tname := TableItem.Alias
                Else
                  tname := TableItem.TableName;
                Replacestring(Result, Format('\f"%s"', [s]), Format('%s.%s', [tname, s]));
                Found := True;
                break;
              End;
            End;
          End;
          If Not Found Then
            Raise ExQueryError.CreateFmt(SFieldNotFound, [s]);
        End
        Else
        Begin
          tname := GetRealTableName(Copy(s, 1, J - 1), Index);
          fName := Copy(s, J + 1, Length(s));
          Replacestring(Result,
            Format('\f"%s"', [s]),
            Format('%s.%s', [tname, fName]));
        End;
        break;
      End;
      Inc(K);
    End;
    If Not FieldFound Then
      Break;
    p := AnsiPos('\f"', Result);
  End;

  Result:= ReplaceParams(Result);
End;

Function TSqlAnalizer.StripFs(Const Ident: String): String;
Var
  P, K: Integer;
  FieldFound: Boolean;
Begin
  Result := Ident;
  If Length(Trim(Ident)) = 0 Then Exit;
  P := AnsiPos('\f"', Result);
  While p > 0 Do
  Begin
    K := p + 3;
    FieldFound := False;
    While K <= Length(Result) Do
    Begin
      If Result[K] = '"' Then
      Begin
        FieldFound := True;
        Result := Copy(Result, p + 3, K - (p + 3));
        Break;
      End;
      Inc(K);
    End;
    If Not FieldFound Then Break;
    p := AnsiPos('\f"', Result);
  End;
End;

{ some code here borrowed from Db.TParam.ParseSQL }
Function TSqlAnalizer.ReplaceParams(const SQL: string): String;
var
  List: TParams;
  I, DblQuote, Quote: Integer;
  ParamValue: string;
  Param: TParam;
begin
  List := TParams.Create(Nil);
  try
    Result:= List.ParseSQL(SQL, True); // Result:= StrPas(PChar(List.ParseSQL(SQL, True))); {patched by ccy}
    for I:= 0 to List.Count - 1 do
    begin
      Param:= xQuery.ParamByName(List[I].Name);
      if Param <> Nil then
      begin
        case Param.DataType of
          ftBlob:
            ParamValue := #34 + StringToHex(Param.AsString) + #34;
          ftString, ftWideString: { patched by ccy }
            begin
              ParamValue:= Param.Asstring;
              DblQuote:= AnsiPos(#34, ParamValue);
              Quote:= AnsiPos(#39, ParamValue);
              if (Quote > 0) and (DblQuote = 0) then
                ParamValue:= #34 + ParamValue + #34
              else if (DblQuote >= 0) and (Quote = 0) then
                ParamValue:= #39 + ParamValue + #39
              else
                ParamValue:= #39 + ParamValue + #39;
            end;
          ftFloat, ftCurrency, ftBCD, ftAutoInc, ftSmallInt, ftInteger, ftWord, ftFmtBcd : { patched by ccy }
            ParamValue:= Param.Asstring;
          ftDate, ftTime, ftDateTime:
            ParamValue:= FloatToStr(Param.AsFloat);
          ftBoolean:
            ParamValue:= xqbase.NBoolean[Param.AsBoolean];
        end;
        Result:= StringReplace(Result, '?', ParamValue, [rfIgnoreCase]);
      end;
    end;
  finally
    List.free;
  end;
end;

Function TSqlAnalizer.CheckIntegrity: Boolean;
Var
  I, J, K, L, vp1, tmp: Integer;
  NumAccepted: Integer;
  Column: TColumnItem;
  TmpWhereStr: String;
  s, AFieldName, temp: String;
  CheckExpres: String;
  tablnam, lrt, rrt: String;
  filename: String;
  Found, Al, Ar: Boolean;
  GroupBy: TOrderByItem;
  OrderBy: TOrderByItem;
  LCheckExpr: TExprParser;
  RCheckExpr: TExprParser;
  TempExpr: TExprParser;
  JoinOn: TJoinOnItem;
  F: TField;
  WhereOptimize: TWhereOptimizeItem;
  ReferencedDataSets: TReferencedDataSetList;
  LeftDataset: TDataSet;
  RightDataset: TDataSet;
  Idx1, Idx2: Integer;
  OptimizeList: TWhereOptimizeList;
  TmpDataset: TDataset;
  TmpAnalizer: TSqlAnalizer;
  GroupOrder: TOrderByItem;
  ADataset: TDataset;
  DatasetItem: TxDataSetItem;
  FromxQuery: TxQuery;
  JoinOnItem: TJoinOnItem;
  LeftTable, RightTable,
  LeftField, RightField: String;
  LeftTableAlias, RightTableAlias: String;
  FoundAggregate: Boolean;
  TempList: TStringList;
  tempLeft: TStringList;
  tempRight: TStringList;

  Function DetectSimpleField(Const fieldnam: String; Var TheTable, TheField: String): Boolean;
  Var
    ds : TDataset;
    sf, tn, fn : String;
    k : integer;
  Begin
    Result := False;
    sf := fieldnam;
    k := Pos('.', sf);
    If k > 0 Then
    Begin
      tn := copy(sf, 1, k - 1);
      fn := TrimSquareBrackets(copy(sf, k + 1, Length(sf)));

      ds := Self.FindDatasetByName(tn);
      If Assigned(ds) Then
      Begin
        If ds.FindField(fn) <> Nil Then
        Begin
          TheTable := tn;
          TheField := fn;
          Result := true;
        End;
      End;
    End
    Else
    Begin
      If FindFieldByName(sf) <> Nil Then
        Result := true;
    End;
  End;

  Procedure AddColumn(Const ColText, ColAlias: String);
  Begin
    With Self.FColumnList.Add Do
    Begin
      ColumnExpr := ColText;
      AsAlias := ColAlias;
    End;
  End;

  Procedure AddAllReferences(pTableItem: TTableItem);
  Var
    K: Integer;
    TmpStr1, TmpStr2: String;
  Begin
    { add native table fields }
    With pTableItem.DataSet Do
      For K := 0 To FieldCount - 1 Do
        If Fields[K].Visible Then
        Begin
          TmpStr1 := AddSquareBrackets(pTableItem.TableName);
          TmpStr2 := AddSquareBrackets(Fields[K].FieldName);
          AddColumn(Format('%s.%s', [TmpStr1, TmpStr2]),
            pTableItem.TableName + '.' + Fields[K].FieldName);
        End;
  End;

  Function DoReplaceCandidates(const AReplaceStr, ALeftTable, ALeftField, ARightTable, ARightField: string): string;
  begin
    Result:= StringReplace(AReplaceStr,
      ALeftTable + '.' + ALeftField + ' = ' + ARightTable + '.' + ARightField, '0 = 0', [rfReplaceAll, rfIgnoreCase]);
    Result:= StringReplace(Result,
      ALeftTable + '.[' + ALeftField + '] = ' + ARightTable + '.[' + ARightField + ']', '0 = 0', [rfReplaceAll, rfIgnoreCase]);
    // now in reverse order
    Result:= StringReplace(Result,
      ARightTable + '.' + ARightField + ' = ' + ALeftTable + '.' + ALeftField, '0 = 0', [rfReplaceAll, rfIgnoreCase]);
    Result:= StringReplace(Result,
      ARightTable + '.[' + ARightField + '] = ' + ALeftTable + '.[' + ALeftField + ']', '0 = 0', [rfReplaceAll, rfIgnoreCase]);
  end;

Begin

  If Not Assigned(FxQuery) Then
    Raise ExQueryError.Create(SXQueryNotDefined);

  { Populate the list of dataset (if will be dinamically created) }
  FxQuery.PopulateDatasets(FTableList);

  { syntax: select * from (select * from Customer where CustNo Between 1000 And 3000) c
    where c.CustNo Between 1500 and 2500; }
  For I := FTableList.Count - 1 Downto 0 Do
  begin
    With FTableList[I] Do
    begin
      If NumSubquery >= 0 then
      begin
        FromxQuery := TxQuery.Create(Nil);
        FromxQuery.Name := FTableList[I].Alias;

        With FxQuery.FDataSets.Add Do
        Begin
          FDataSet := FromxQuery;
          FAlias := FTableList[I].Alias;
          FTemporal := True;
        End;
        FromxQuery.FResultSet := TSqlAnalizer(FSubQueryList[NumSubquery]).ResultSet;
        TSqlAnalizer(FSubQueryList[NumSubquery]).ResultSet := Nil;
        TSqlAnalizer(FSubQueryList[NumSubquery]).Free;
        FSubQueryList.Delete(NumSubquery);
        FromxQuery.FResultSetIsDefined := true;
        FromxQuery.Open;
      end;
    end;
  end;

  { check that all tables in FROM clause exists }
  If Not (FStatement = ssInsert) And (FTableList.Count = 0) Then
    Raise ExQueryError.Create(SWrongTableNumber);

  // reference the dataset from the table names
  For I := 0 To FTableList.Count - 1 Do
    With FTableList[I] Do
    Begin
      If Not IsFullPath Then
      Begin
        DataSet := FxQuery.DataSetByName(TrimSquareBrackets(TableName));
        If (DataSet = Nil) Then
          Raise ExQueryError.CreateFmt(SWrongDataSetName, [TableName]);
      End
      Else
      Begin
        // we need to add this dataset temporary
        If Assigned(FxQuery.FOnResolveDataset) Then
        Begin
          //filename:= TableName;
          tablnam := Alias;
          FxQuery.FOnResolveDataset(FxQuery, TableName, tablnam, ADataset);
          If Length(tablnam) > 0 Then
            TableName := tablnam;
          If Assigned(ADataSet) Then
          Begin
            // add as a temporary dataset
            DataSetItem := FxQuery.DataSets.Add;
            With DataSetItem Do
            Begin
              Dataset := ADataset;
              Alias := FTableList[I].Alias;
              Temporal := True;
            End;
            FTableList[I].Dataset := ADataset;
          End
          Else
            Raise ExQueryError.CreateFmt(SWrongDataSetName, [TableName])
        End
        Else
          Raise ExQueryError.CreateFmt(SWrongDataSetName, [TableName]);
      End;

      { data set must be opened }
      If Not DataSet.Active Then
      Begin
        // try to open
        Try
          DataSet.Open; { LAS : 05-30-2000 }
        Except
          Raise ExQueryError.CreateFmt(SDataSetNotOpened, [TableName]);
        End;
      End;
    End;

  If FStatement = ssInsert Then
  Begin
    For I := 0 To FInsertList.Count - 1 Do
      With FInsertList[I] Do
      Begin
        If IsFullPath Then
        Begin
          If Assigned(FxQuery.FOnResolveDataset) Then
          Begin
            tablnam := TableName;
            FxQuery.FOnResolveDataset(FxQuery, TableName, tablnam, ADataset);
            If Length(tablnam) > 0 Then
              TableName := tablnam;
            If Assigned(ADataSet) Then
            Begin
              // add as a temporary dataset
              DataSetItem := FxQuery.DataSets.Add;
              With DataSetItem Do
              Begin
                Dataset := ADataset;
                Alias := TableName;
                Temporal := True;
              End;
              FInsertList[I].Dataset := ADataset;
            End
            Else
              Raise ExQueryError.CreateFmt(SWrongDataSetName, [TableName])
          End
          Else
            Raise ExQueryError.CreateFmt(SWrongDataSetName, [TableName]);
        End;
      End;
    For I := 0 To FInsertList.Count - 1 Do
      With FInsertList[I] Do
      Begin
        DataSet := FxQuery.DataSetByName(TableName);
        If DataSet = Nil Then
          Raise ExQueryError.CreateFmt(SWrongDataSetName, [TableName]);
        { CRVG : Added for compatibility with VDB stuff }
        { data set must be opened }
        If Not DataSet.Active Then
        Begin
          // try to open
          Try
            DataSet.Open; { LAS : 05-30-2000 }
          Except
            Raise ExQueryError.CreateFmt(SDataSetNotOpened, [TableName]);
          End;
        End; { data set must be opened }
        If Not DataSet.Active Then
          Raise ExQueryError.CreateFmt(SDataSetNotOpened, [TableName]);
      End;
  End;

  { set column field to qualified fields }
  For I := 0 To FColumnList.Count - 1 Do
    With FColumnList[I] Do
    Begin
      If Length(AsAlias) = 0 Then
        AsAlias := ColumnExpr;
      s:= QualifiedField(ColumnExpr, True);
      FxQuery.FixDummiesForQuerying(s);
      FxQuery.FixDummiesForFilter(s);
      ColumnExpr:= s;
      For J := 0 To AggregateList.Count - 1 Do
      Begin
        AggregateList[J].AggregateStr := QualifiedField(AggregateList[J].AggregateStr, True);
      End;
    End;

  { UPDATE: set update columns to qualified fields and check for fields existence }
  For I := 0 To FUpdateColumnList.Count - 1 Do
    With FUpdateColumnList[I] Do
    Begin
      if Length(ColExpr) > 0 Then
      Begin
        s := QualifiedField(ColExpr, True);
        FxQuery.FixDummiesForQuerying(s);
        FxQuery.FixDummiesForFilter(s);
        ColExpr := s;
      End;
      Field := Self.FindFieldByName(ColName);
      If Field = Nil Then
        Raise ExQueryError.CreateFmt(SWrongFieldName, [ColName]);
    End;

  { INSERT: check that fields in FieldNames exist in every dataset }
  If FStatement = ssInsert Then
  Begin
    For L := 0 To FInsertList.Count - 1 Do
      For I := 0 To FInsertList[L].FieldNames.Count - 1 Do
      Begin
        For K := 0 To FInsertList[L].ExprList.Count - 1 Do
        Begin
          s := FInsertList[L].ExprList[I];
          FxQuery.FixDummiesForQuerying(s);
          FxQuery.FixDummiesForFilter(s);
          FInsertList[L].ExprList[I] := s;
        End;
        F := FInsertList[L].DataSet.FindField(FInsertList[L].FieldNames[I]);
        If F = Nil Then
        Begin
          Raise ExQueryError.CreateFmt(SInsertWrongFieldName,
            [FInsertList[L].FieldNames[I]]);
        End;
      End;
  End;

  { check for duplicate table names }
  (*for I:= 0 to FTableList.Count - 1 do
    with FTableList[I] do
    begin
      for K:= 0 to FTableList.Count - 1 do
        if I <> K then
        begin
          if DataSet = FTableList[K].DataSet then
            raise ExQueryError.CreateFmt(SDuplicateDataSets, [TableName]);
        end;
    end; *)

  { syntax SELECT * FROM; }
  If FDoSelectAll Then
    For I := 0 To FTableList.Count - 1 Do
      AddAllReferences(FTableList[I]);

  { syntax SELECT Customer.* FROM }
  For I := 0 To FTableAllFields.Count - 1 Do
    For K := 0 To FTableList.Count - 1 Do
      With FTableList[K] Do
        If (AnsiCompareText(TableName, FTableAllFields[I]) = 0) Or
           (AnsiCompareText(Alias, FTableAllFields[I]) = 0) Then
        Begin
          AddAllReferences(FTableList[K]);
          Break;
        End;

  { check subquery }
  For I := 0 To FSubQueryList.Count - 1 Do
  Begin
    TmpAnalizer := TSqlAnalizer(FSubQueryList[I]);
    With TmpAnalizer Do
    Begin
      If FTableList.Count <> 1 Then
        Raise ExQueryError.Create(SSubqueryWrongTables);
      If (Self.FStatement <> ssInsert) And (FColumnList.Count <> 1) Then
        Raise ExQueryError.Create(SSubqueryWrongCols);
    End;
  End;
  For i := 0 To FSubQueryKindList.Count - 2 Do
    If TSubqueryKind(FSubQueryKindList[i]) <> TSubqueryKind(FSubQueryKindList[i + 1]) Then
      Raise ExQueryError.Create(SSubqueryKindWrong);

  { check if join candidates defined in WHERE clause are valids }
  For I := 0 To FLJoinCandidateList.Count - 1 Do
  Begin
    CheckExpres := FLJoinCandidateList[I];
    FxQuery.FixDummiesForFilter(CheckExpres);
    FLJoinCandidateList[I] := CheckExpres;

    CheckExpres := FRJoinCandidateList[I];
    FxQuery.FixDummiesForFilter(CheckExpres);
    FRJoinCandidateList[I] := CheckExpres;
  End;

  { fix some custom implementation data like date, booleans, etc. }
  FWhereStr := QualifiedField(FWhereStr, True);
  FxQuery.FixDummiesForQuerying(FWhereStr);
  FxQuery.FixDummiesForQuerying(FWhereStr);

  for I:= 0 to UserDefinedRange.StartValues.Count-1 do
  begin
    UserDefinedRange.StartValues[I]:=
      ReplaceParams(UserDefinedRange.StartValues[I]);
    UserDefinedRange.EndValues[I]:=
      ReplaceParams(UserDefinedRange.EndValues[I]);
  end;

  // check if candidates for join in where clause are valid
  ReferencedDataSets := TReferencedDataSetList.Create;
  Try
    If (FStatement = ssSelect) And (FJoinList.Count = 0) Then
    Begin
{.$ifdef false}
      // first, reorder the join candidate list
      TempList:= TStringList.Create;
      Try
        For I := 0 To FLJoinCandidateList.Count - 1 Do
        Begin

          FLJoinCandidateList[i] := UpperCase(QualifiedField(FLJoinCandidateList[i], True));
          FRJoinCandidateList[i] := UpperCase(QualifiedField(FRJoinCandidateList[i], True));

          { change the coded field to a workable form }
          Al := DetectSimpleField(FLJoinCandidateList[i], LeftTable, LeftField);
          Ar := DetectSimpleField(FRJoinCandidateList[i], RightTable, RightField);

          LeftTable := GetRealTableName(LeftTable, Idx1);
          RightTable := GetRealTableName(RightTable, Idx2);

          If Idx1 <= Idx2 then
            TempList.AddObject(Format('%.3d%.3d', [Idx1, I] ), TObject(i+1))
          else
            TempList.AddObject(Format('%.3d%.3d', [Idx2, I] ), TObject(-(i+1)));
        end;
        // order numerically based on index of left tables
        TempList.Sort;
        tempLeft:= TStringList.Create;
        tempRight:= TStringList.Create;
        for I:= 0 to TempList.Count-1 do
        begin
          temp:= TempList[I];
          K:= Longint(TempList.Objects[I]);
          If K < 0 then
          begin
            // the right is first
            K:= Abs(K) - 1;
            tempLeft.Add( FRJoinCandidateList[K] );
            tempRight.Add( FLJoinCandidateList[K] );
          end else
          begin
            // the left is first
            tempLeft.Add( FLJoinCandidateList[K-1] );
            tempRight.Add( FRJoinCandidateList[K-1] );
          end;
        end;
        FLJoinCandidateList.Free; FLJoinCandidateList:= tempLeft;
        FRJoinCandidateList.Free; FRJoinCandidateList:= tempRight;
      Finally
        TempList.Free;
      End;
{.$endif}
      // try to find joins in WHERE clause
      TmpWhereStr := FWhereStr;
      NumAccepted := 0;
      For I := 0 To FLJoinCandidateList.Count - 1 Do
      Begin

        FLJoinCandidateList[i] := UpperCase(QualifiedField(FLJoinCandidateList[i], True));
        FRJoinCandidateList[i] := UpperCase(QualifiedField(FRJoinCandidateList[i], True));

        { change the coded field to a workable form }
        Al := DetectSimpleField(FLJoinCandidateList[i], LeftTable, LeftField);
        Ar := DetectSimpleField(FRJoinCandidateList[i], RightTable, RightField);

        LeftTableAlias := GetRealTableAlias(LeftTable);
        RightTableAlias := GetRealTableAlias(RightTable);

        LeftTable := GetRealTableName(LeftTable, Idx1);
        RightTable := GetRealTableName(RightTable, Idx2);

        If (Length(LeftTable) = 0) Or (Length(LeftField) = 0) Or
           (Length(RightTable) = 0) Or (Length(RightField) = 0) Then Continue;

        { create dummy expression for checking fields of left and right expressions }
        LCheckExpr := TExprParser.Create(Self, FTableList[0].DataSet);
        RCheckExpr := TExprParser.Create(Self, FTableList[0].DataSet);
        Try
          { check left expression }
          ReferencedDataSets.Clear;
          LCheckExpr.ReferencedDataSets := ReferencedDataSets;
          { exists parameters? }
          If AnsiPos('(Subquery', FLJoinCandidateList[I]) > 0 Then Continue;
          LCheckExpr.CheckExpression(FLJoinCandidateList[I]);
          If ReferencedDataSets.Count <> 1 Then Continue;

          { check right expression }
          ReferencedDataSets.Clear;
          RCheckExpr.ReferencedDataSets := ReferencedDataSets;
          { exists parameters? }
          If AnsiPos('(Subquery', FRJoinCandidateList[I]) > 0 Then Continue;
          RCheckExpr.CheckExpression(FRJoinCandidateList[I]);
          If ReferencedDataSets.Count <> 1 Then Continue;

          With FJoinList.Add Do
          Begin
            JoinAction := jkLeftInnerJoin;
            JoinExpression := FLJoinCandidateList[I] + ' = ' + FRJoinCandidateList[I];
            LeftRefTest := LeftTable + '.' + LeftField;
            RightRefTest := RightTable + '.' + RightField;

            TmpWhereStr:= DoReplaceCandidates(TmpWhereStr, LeftTable, LeftField, RightTable, RightField);
            // Now in reverse order

            TmpWhereStr:= DoReplaceCandidates(TmpWhereStr, LeftTableAlias, LeftField, RightTableAlias, RightField);
          End ;

          {p1:= AnsiPos(UpperCase(FLJoinCandidateList[I]), UpperCase(TmpWhereStr));
          if p1 > 0 then
            TmpWhereStr:= Copy(TmpWhereStr, 1, p1 - 1) + '0' +
                           Copy(TmpWhereStr, p1 + Length(FLJoinCandidateList[I]), Length(TmpWhereStr));

          p1:= AnsiPos(UpperCase(FRJoinCandidateList[I]), UpperCase(TmpWhereStr));
          if p1 > 0 then
            TmpWhereStr:= Copy(TmpWhereStr, 1, p1 - 1) + '0' +
                           Copy(TmpWhereStr, p1 + Length(FRJoinCandidateList[I]), Length(TmpWhereStr)); }

          Inc(NumAccepted);
        Finally
          LCheckExpr.Free;
          RCheckExpr.Free;
        End;
      End;
      If NumAccepted > 0 Then
      Begin
        ReplaceString(TmpWhereStr, 'AND (0 = 0)', '');
        ReplaceString(TmpWhereStr, '(0 = 0) AND', '');
        If FJoinList.Count >= FTableList.Count Then
        Begin
          FIsJoinInWhere := True;
          FJoinInWhereExpres := FWhereStr;
          FWhereStr := '';
          FJoinList.Clear;
          FWhereOptimizeList.Clear;
        End
        Else
        Begin
          FWhereStr := TmpWhereStr;
        End;
      End;
    End;

    { check if WHERE clause includes only fields from primary table }
    If Not (FStatement = ssInsert) Then
      FDefDataSet := FTableList[0].DataSet
    Else
      FDefDataSet := FInsertList[0].DataSet;
    { where contiene solo campos sencillos de una o mas tablas }
    FWhereContainsOnlyBasicFields := true;
    If (Length(FWhereStr) > 0) And (AnsiPos('(subquery', Lowercase(FWhereStr)) = 0) Then
    Begin
      s := FWhereStr;
      FxQuery.FixDummiesForQuerying(s);
      FxQuery.FixDummiesForFilter(s);
      TempExpr := TExprParser.Create(Self, FDefDataSet);
      Try
        TempExpr.ParseExpression(s);
        For I := 0 To TempExpr.IdReferences.Count - 1 Do
          If Not IsValidFieldName(TempExpr.IdReferences[I], TRUE) Then
          Begin
            FWhereContainsOnlyBasicFields := False;
            break;
          End;
      Finally
        TempExpr.Free;
      End;
    End;

    { where expression }
    If (Length(FWhereStr) > 0) And FWhereContainsOnlyBasicFields And
       (FxQuery.WhereOptimizeMethod = omSetFilter) And Assigned(FxQuery.OnSetFilter) Then
    Begin
      FWhereFilter := QualifiedField(FWhereStr, true);
      If FStatement = ssInsert Then
      Begin
        ReplaceString(FWhereFilter, FInsertList[0].TableName + '.', '');
        ReplaceString(FWhereFilter, FInsertList[0].TableName + '.', '');
      End
      Else
      Begin
        for I:= 0 to FTableList.Count-1 do
        begin
          ReplaceString(FWhereFilter, FTableList[I].TableName + '.', '');
          ReplaceString(FWhereFilter, FTableList[I].Alias + '.', '');
        end;
      End;
    End
    Else
      FWhereFilter := '';

    { Add temporary columns for fields not included in the WHERE clause
      and only if JOINing }
    If (FJoinList.Count > 0) And Not FWhereContainsOnlyBasicFields Then
    Begin
      TempExpr := TExprParser.Create(Self, FTableList[0].DataSet);
      Try
        { parse the expression for detecting how many references to fields
          exists in WHERE clause. References will be stored in
          TempExpr.IdReferences: TStringList property
        }
        TempExpr.ParseExpression(FWhereStr);
        { now compare against the column list }
        For I := 0 To TempExpr.IdReferences.Count - 1 Do
        Begin

          If Not IsValidFieldName(TempExpr.IdReferences[I], False) Then Continue;

          Found := False;
          For J := 0 To FColumnList.Count - 1 Do
            If AnsiCompareText(FColumnList[J].ColumnExpr,
              QualifiedField('\f"' + QualifiedFieldAddSquareBrackets(TempExpr.IdReferences[I]) + '"', True)) = 0 Then
            Begin
              Found := true;
              break;
            End;
          If Not Found Then
            With FColumnList.Add Do
            Begin
              ColumnExpr := QualifiedField('\f"' + QualifiedFieldAddSquareBrackets(TempExpr.IdReferences[I]) + '"', False);
              AsAlias := QualifiedFieldAddSquareBrackets(TempExpr.IdReferences[I]);
              If Length(AsAlias) = 0 Then
                AsAlias := ColumnExpr;
              IsAsExplicit := False;
              IsTemporaryCol := True;
              // other values are irrelevant...
            End;
        End;
      Finally
        TempExpr.Free;
      End;
    End;

    For I:= 0 to FJoinList.Count-1 do
    begin
      with FJoinList[I] do
      begin
        lrt:= UpperCase( LeftRefTest );
        rrt:= UpperCase( RightRefTest );

        If AnsiPos('[DUMMY].', lrt) > 0 Then
        Begin
          AFieldName:= Copy( lrt, 9, Length(lrt));
          // find this fieldname on all tables
          for J:= 0 to TableList.Count-1 do
            with TableList[J] do
              If DataSet.FindField(AFieldName) <> Nil Then
              Begin
                temp:= lrt;
                lrt:= StringReplace(lrt, '[DUMMY]', TableList[J].TableName, [rfReplaceAll, rfIgnoreCase]);
                JOINExpression:= StringReplace(JOINExpression, temp, lrt, [rfReplaceAll, rfIgnoreCase]);
                LeftRefTest:= lrt;
                Break;
              End;
        End;
        If AnsiPos('[DUMMY].', rrt) > 0 Then
        Begin
          AFieldName:= Copy( rrt, 9, Length(rrt));
          // find this fieldname on all tables
          for J:= 0 to TableList.Count-1 do
            with TableList[J] do
              If DataSet.FindField(AFieldName) <> Nil Then
              Begin
                temp:= rrt;
                rrt:= StringReplace(rrt, '[DUMMY]', TableList[J].TableName, [rfReplaceAll, rfIgnoreCase]);
                JOINExpression:= StringReplace(JOINExpression, temp, rrt, [rfReplaceAll, rfIgnoreCase]);
                RightRefTest:= rrt;
                Break;
              End;
        End;
      end;
    end;

    { Prepare for JOINing }

    FJoinList.PrepareJoin;

  Finally
    ReferencedDatasets.Free;
  End;

  If Not FIsJoinInWhere And (FTableList.Count > 1) And (FJoinList.Count = 0) Then
    Raise ExQueryError.Create(SWrongJoin);

  (* if NOT(FWhereContainsOnlyBasicFields and
     (FxQuery.WhereOptimizeMethod=omSetFilter) and
     (AnsiPos('(Subquery)', FWhereStr)=0)) then
  begin
    FWhereOptimizeList.Clear; { no possible optimizations in WHERE when JOINing }
    FWhereFilter:= '';        { no filters possible }
  end; *)
  If (Length(FWhereFilter) > 0) And (FSubQueryList.Count > 0) Then
    FWhereFilter := '';

  OptimizeList := FWhereOptimizeList;

  { now check for the where optimization list }
  For I := 0 To OptimizeList.Count - 1 Do
  Begin
    WhereOptimize := OptimizeList[I];
    With WhereOptimize Do
    Begin
      CanOptimize := False; { first try to optimize the where statement }
      s := QualifiedField(FieldNames, True);
      LCheckExpr := TExprParser.Create(Self, FTableList[0].DataSet);
      Try
        If (Pos(':', s) > 0) Or (Not LCheckExpr.CheckExpression(s)) Then
          Continue;
        CanOptimize := True;
        If LCheckExpr.CheckData.FieldCount > 0 Then
        Begin
          DataSet := LCheckExpr.CheckData.Fields[1].DataSet;
          FieldNames := '';
          For J := 1 To LCheckExpr.CheckData.FieldCount Do
            If J < LCheckExpr.CheckData.FieldCount Then
              FieldNames := FieldNames + LCheckExpr.CheckData.Fields[J].FieldName + ';'
            Else
              FieldNames := FieldNames + LCheckExpr.CheckData.Fields[J].FieldName;
        End;
      Finally
        LCheckExpr.Free;
      End;
    End;
  End;

  { Check GROUP BY clause }
  For I := 0 To FGroupByList.Count - 1 Do
  Begin
    GroupBy := FGroupByList[I];
    { For columns defined with a name in the GROUP BY clause, we must found
      the indef of the column it is referring to }
    If GroupBy.ColIndex < 0 Then
    Begin
      s := GroupBy.Alias;
      GroupBy.Alias := QualifiedField(s, True);
      { find column index
       find this field in the list of fields }
      Found := False;
      For K := 0 To FColumnList.Count - 1 Do
      Begin
        Column := FColumnList[K];
        If (AnsiCompareText(Column.ColumnExpr, GroupBy.Alias) = 0) Or
           (AnsiCompareText(Column.ColumnExpr, QualifiedField(s, False)) = 0) Or
           (Column.IsAsExplicit And (AnsiCompareText(Column.AsAlias, GroupBy.Alias) = 0))  Then
        Begin
          GroupBy.ColIndex := K;
          Found := True;
          Break;
        End;
      End;
      If Not Found Then
        Raise ExQueryError.Create(SGroupByIncongruent);
    End
    Else
    Begin
      If (GroupBy.ColIndex < 0) Or (GroupBy.ColIndex > FColumnList.Count - 1) Then
        Raise ExQueryError.Create(SGroupByWrongNum);
    End;
  End;

  { check ORDER BY clause }
  For I := 0 To FOrderByList.Count - 1 Do
  Begin
    OrderBy := FOrderByList[I];
    If OrderBy.ColIndex < 0 Then
    Begin
      s := OrderBy.Alias;
      temp:= StripFs(s);
      // check if alias corresponds to an aggregate field
      FoundAggregate:= False;
      For K := 0 To FColumnList.Count - 1 Do
        With FColumnList[K] Do
        Begin
          If IsAsExplicit And
            (AnsiPos('{Aggregate', ColumnExpr) > 0) And
            (AnsiCompareText(temp, AsAlias) = 0) Then
          Begin
            OrderBy.ColIndex := K;
            FoundAggregate:= True;
            Break;
          End;
        End;
      If Not FoundAggregate Then
      begin
        OrderBy.Alias := QualifiedField(s, True);
        { find column index. find this field in the list of fields }
        Found := False;
        For K := 0 To FColumnList.Count - 1 Do
        Begin
          Column := FColumnList[K];
          If (AnsiCompareText(Column.ColumnExpr, OrderBy.Alias) = 0) Or
             (AnsiCompareText(Column.ColumnExpr, QualifiedField(s, False)) = 0)  Or
             (Column.IsAsExplicit And (AnsiCompareText(Column.AsAlias, OrderBy.Alias) = 0))  Then
          Begin
            OrderBy.ColIndex := K;
            Found := True;
            Break;
          End;
        End;
        If Not Found Then
          Raise ExQueryError.CreateFmt(SFieldNotFound, [OrderBy.Alias]);
      end;
    End
    Else
    Begin
      If (OrderBy.ColIndex < 0) Or (OrderBy.ColIndex > FColumnList.Count - 1) Then
        Raise ExQueryError.Create(SWrongIndexField);
    End;
  End;

  { TRANSFORM: check that number of columns is same as columns in group by
    and in ORDER BY }
  If Length(FPivotStr) > 0 Then
  Begin

    If Not (FColumnList.Count >= FGroupByList.Count) Then
      Raise ExQueryError.Create(STransfColumnsMismatch);
    { check that column expressions in SELECT be the same as in GROUP BY}
    For I := 0 To FGroupByList.Count - 1 Do
      If Not (FGroupByList[I].ColIndex = I) Then
        Raise ExQueryError.Create(STransfWrongColumnGroup);

    If FOrderByList.Count > 0 Then
    Begin
      If Not (FColumnList.Count >= FOrderByList.Count) Then
        Raise ExQueryError.Create(STransfOrderByMismatch);
      { check that column expressions in SELECT be the same as in ORDER BY}
      For I := 0 To FOrderByList.Count - 1 Do
        If Not (FOrderByList[I].ColIndex = I) Then
          Raise ExQueryError.Create(STransfWrongColumnOrder);
    End;
    FTransfBaseColumns := FColumnList.Count;
    FTransfGroupedColumns := FGroupByList.Count + 1;
    { add the pivot expression as the next column }
    With FColumnList.Add Do
    Begin
      ColumnExpr := QualifiedField(FPivotStr, False);
      CastType := 0;
      CastLen := 1;
      AsAlias := ColumnExpr;
      IsAsExplicit := False;
    End;
    { also, add this column as the GROUP BY column }
    If Not HasAggregates Then   // tiene Agregates adicionales en clausual SELECT ?
    Begin
      GroupOrder := FGroupByList.Add;
      GroupOrder.Alias := '';
      GroupOrder.ColIndex := FColumnList.Count - 1;
    End;

    { ... and also add this column as the ORDER BY column}
    If Not HasAggregates And (FOrderByList.Count > 0) Then
    Begin
      GroupOrder := FOrderByList.Add;
      GroupOrder.Alias := '';
      GroupOrder.ColIndex := FColumnList.Count - 1;
    End;

    { now add the aggregate functions defined in TRANSFORM clause as the last column}
    For I := 0 To FTransformColumnList.Count - 1 Do
    Begin
      Column := FTransformColumnList[I];
      With FColumnList.Add Do
      Begin
        ColumnExpr := QualifiedField(Column.ColumnExpr, False);
        AsAlias := Column.AsAlias;
        If Length(AsAlias) = 0 Then
          AsAlias := ColumnExpr;
        IsAsExplicit := Column.IsAsExplicit;
        AggregateList.Assign(Column.AggregateList);
        For J := 0 To AggregateList.Count - 1 Do
          AggregateList[J].AggregateStr := QualifiedField(AggregateList[J].AggregateStr, False);
        IsTemporaryCol := Column.IsTemporaryCol;
        CastType := Column.CastType;
        CastLen := Column.CastLen;
        { other values are irrelevant }
      End;
    End;
  End;

  Result := True;
End;

Function TSqlAnalizer.FindDataSetByName(Const Name: String): TDataSet;
Var
  vI: Integer;
Begin
  Result := Nil;
  For vI := 0 To FTableList.Count - 1 Do
    With FTableList[vI] Do
      If (AnsiCompareText(TrimSquareBrackets(TableName), TrimSquareBrackets(Name)) = 0) Or
        (AnsiCompareText(TrimSquareBrackets(Alias), TrimSquareBrackets(Name)) = 0) Then
      Begin
        Result := DataSet;
        Exit;
      End;
End;

Procedure TSqlAnalizer.DoIntoTableOperation;
Var
  DestTable, SourceTable: TDataset;
  SourceField, DestField: TField;
  I, J: Integer;
Begin
  If Length(FIntoTable) = 0 Then Exit;
  // find the referenced dataset
  DestTable := Nil;
  With FxQuery Do
    For I := 0 To Datasets.Count - 1 Do
      If AnsiCompareText(Datasets[I].Alias, FIntoTable) = 0 Then
      Begin
        DestTable := Datasets[I].Dataset;
        Break;
      End;
  If DestTable = Nil Then
    Raise ExQueryError.Create(SWrongIntoTable);
  SourceField := Nil;
  // find one valid field
  For J := 0 To ResultSet.Fields.Count - 1 Do
  Begin
    SourceField := ResultSet.Fields[J].SourceField;
    If SourceField <> Nil Then
      break;
  End;
  If SourceField = Nil Then Exit;
  SourceTable := SourceField.Dataset;
  For I := 1 To ResultSet.RecordCount Do
  Begin
    ResultSet.Recno := I;
    // position in the correct record in the source dataset
    SourceTable.GotoBookmark(ResultSet.GetSourceBookmark);
    { case fIntoAction of
      iaForAppend: DestTable.Insert;
      iaForCopy: DestTable.Edit;
    end; }
    DestTable.Insert;
    For J := 0 To ResultSet.Fields.Count - 1 Do
    Begin
      SourceField := ResultSet.Fields[J].SourceField;
      If SourceField = Nil Then Continue;
      DestField := DestTable.FindField(SourceField.FieldName);
      If DestField = Nil Then Continue;
      DestField.Assign(SourceField);
    End;
    DestTable.Post;
  End;
End;

Procedure TSqlAnalizer.SafeCreateResultSet;
Type
  TDataSetEventRec = Record
    BeforeOpen: TDataSetNotifyEvent;
    AfterOpen: TDataSetNotifyEvent;
    BeforeClose: TDataSetNotifyEvent;
    AfterClose: TDataSetNotifyEvent;
    BeforeInsert: TDataSetNotifyEvent;
    AfterInsert: TDataSetNotifyEvent;
    BeforeEdit: TDataSetNotifyEvent;
    AfterEdit: TDataSetNotifyEvent;
    BeforePost: TDataSetNotifyEvent;
    AfterPost: TDataSetNotifyEvent;
    BeforeCancel: TDataSetNotifyEvent;
    AfterCancel: TDataSetNotifyEvent;
    BeforeDelete: TDataSetNotifyEvent;
    AfterDelete: TDataSetNotifyEvent;
    BeforeRefresh: TDataSetNotifyEvent;
    AfterRefresh: TDataSetNotifyEvent;
    BeforeScroll: TDataSetNotifyEvent;
    AfterScroll: TDataSetNotifyEvent;
    OnNewRecord: TDataSetNotifyEvent;
    // FOnCalcFields: TDataSetNotifyEvent;
    // FOnEditError: TDataSetErrorEvent;
    // FOnPostError: TDataSetErrorEvent;
    // FOnDeleteError: TDataSetErrorEvent;
    // FOnFilterRecord: TFilterRecordEvent;
  End;
  TFieldEventRec = Record
    OnChange: TFieldNotifyEvent;
    OnGetText: TFieldGetTextEvent;
    OnSetText: TFieldSetTextEvent;
    OnValidate: TFieldNotifyEvent;
  End;
Var
  BookmarkList: {$if RTLVersion >= 20}TList<TBookmark>{$else}TList{$ifend};
  bm: TBookmark;
  I: Integer;
  DisabledState: array of boolean;
  DataSetState: array of TDataSetState;         // Nonn ...
  DataSetFields: array of array of Variant;
  DataSetEvents: array of TDataSetEventRec;
  FieldEvents: array of array of TFieldEventRec;
  J: Integer;
  MaxNumFields: Integer;
  SFS: TSaveFormatSettings;

  Procedure SaveFieldEvents(ADsIndex, AFldIndex: Integer; AField: TField);
  Begin
    FieldEvents[ADSIndex][AFldIndex].OnChange := AField.OnChange;
    FieldEvents[ADSIndex][AFldIndex].OnGetText := AField.OnGetText;
    FieldEvents[ADSIndex][AFldIndex].OnSetText := AField.OnSetText;
    FieldEvents[ADSIndex][AFldIndex].OnValidate := AField.OnValidate;
    AField.OnChange := nil;
    AField.OnGetText := nil;
    AField.OnSetText := nil;
    AField.OnValidate := nil;
  End;

  Procedure RestoreFieldEvents(ADSIndex, AFldIndex: Integer; AField: TField);
  Begin
    AField.OnChange := FieldEvents[ADSIndex][AFldIndex].OnChange;
    AField.OnGetText := FieldEvents[ADSIndex][AFldIndex].OnGetText;
    AField.OnSetText := FieldEvents[ADSIndex][AFldIndex].OnSetText;
    AField.OnValidate := FieldEvents[ADSIndex][AFldIndex].OnValidate;
  End;

  Procedure SaveDataSetEvents(AIndex: Integer; ADataSet: TDataSet);
  Var
    J: Integer;
  Begin
    If Assigned(ADataSet) Then
    Begin
      For J := 0 To ADataSet.FieldCount - 1 Do
        SaveFieldEvents(AIndex, J, ADataSet.Fields[J]);

      DataSetEvents[AIndex].BeforeOpen := ADataSet.BeforeOpen;
      ADataSet.BeforeOpen := nil;
      DataSetEvents[AIndex].AfterOpen := ADataSet.AfterOpen;
      ADataSet.AfterOpen := nil;

      DataSetEvents[AIndex].BeforeClose := ADataSet.BeforeClose;
      ADataSet.BeforeClose := nil;
      DataSetEvents[AIndex].AfterClose := ADataSet.AfterClose;
      ADataSet.AfterClose := nil;

      DataSetEvents[AIndex].BeforeInsert := ADataSet.BeforeInsert;
      ADataSet.BeforeInsert := nil;
      DataSetEvents[AIndex].AfterInsert := ADataSet.AfterInsert;
      ADataSet.AfterInsert := nil;

      DataSetEvents[AIndex].BeforeEdit := ADataSet.BeforeEdit;
      ADataSet.BeforeEdit := nil;
      DataSetEvents[AIndex].AfterEdit := ADataSet.AfterEdit;
      ADataSet.AfterEdit := nil;

      DataSetEvents[AIndex].BeforePost := ADataSet.BeforePost;
      ADataSet.BeforePost := nil;
      DataSetEvents[AIndex].AfterPost := ADataSet.AfterPost;
      ADataSet.AfterPost := nil;

      DataSetEvents[AIndex].BeforeCancel := ADataSet.BeforeCancel;
      ADataSet.BeforeCancel := nil;
      DataSetEvents[AIndex].AfterCancel := ADataSet.AfterCancel;
      ADataSet.AfterCancel := nil;

      DataSetEvents[AIndex].BeforeDelete := ADataSet.BeforeDelete;
      ADataSet.BeforeDelete := nil;
      DataSetEvents[AIndex].AfterDelete := ADataSet.AfterDelete;
      ADataSet.AfterDelete := nil;

      DataSetEvents[AIndex].BeforeRefresh := ADataSet.BeforeRefresh;
      ADataSet.BeforeRefresh := nil;
      DataSetEvents[AIndex].AfterRefresh := ADataSet.AfterRefresh;
      ADataSet.AfterRefresh := nil;

      DataSetEvents[AIndex].BeforeScroll := ADataSet.BeforeScroll;
      ADataSet.BeforeScroll := nil;
      DataSetEvents[AIndex].AfterScroll := ADataSet.AfterScroll;
      ADataSet.AfterScroll := nil;

      DataSetEvents[AIndex].OnNewRecord := ADataSet.OnNewRecord;
      ADataSet.OnNewRecord := nil;
    End;
  End;

  Procedure RestoreDataSetEvents(AIndex: Integer; ADataSet: TDataSet);
  Var
    J: Integer;
  Begin
    If Assigned(ADataSet) Then
    Begin
      For J := 0 To ADataSet.FieldCount - 1 Do
        RestoreFieldEvents(AIndex, J, ADataSet.Fields[J]);

      ADataSet.BeforeOpen := DataSetEvents[AIndex].BeforeOpen;
      ADataSet.AfterOpen := DataSetEvents[AIndex].AfterOpen;

      ADataSet.BeforeClose := DataSetEvents[AIndex].BeforeClose;
      ADataSet.AfterClose := DataSetEvents[AIndex].AfterClose;

      ADataSet.BeforeInsert := DataSetEvents[AIndex].BeforeInsert;
      ADataSet.AfterInsert := DataSetEvents[AIndex].AfterInsert;

      ADataSet.BeforeEdit := DataSetEvents[AIndex].BeforeEdit;
      ADataSet.AfterEdit := DataSetEvents[AIndex].AfterEdit;

      ADataSet.BeforePost := DataSetEvents[AIndex].BeforePost;
      ADataSet.AfterPost := DataSetEvents[AIndex].AfterPost;

      ADataSet.BeforeCancel := DataSetEvents[AIndex].BeforeCancel;
      ADataSet.AfterCancel := DataSetEvents[AIndex].AfterCancel;

      ADataSet.BeforeDelete := DataSetEvents[AIndex].BeforeDelete;
      ADataSet.AfterDelete := DataSetEvents[AIndex].AfterDelete;

      ADataSet.BeforeRefresh := DataSetEvents[AIndex].BeforeRefresh;
      ADataSet.AfterRefresh := DataSetEvents[AIndex].AfterRefresh;

      ADataSet.BeforeScroll := DataSetEvents[AIndex].BeforeScroll;
      ADataSet.AfterScroll := DataSetEvents[AIndex].AfterScroll;

      ADataSet.OnNewRecord := DataSetEvents[AIndex].OnNewRecord;
    End;
  End;                                                             // ... Nonn

Begin
  If Assigned(FxQuery.FOnBeforeQuery) Then
    FxQuery.FOnBeforeQuery(Self);
  SFS := SaveFormatSettings;                                       // Nonn
  BookmarkList := {$if RTLVersion >= 20}TList<TBookmark>{$else}TList{$ifend}.Create;
  SetLength( DisabledState, FxQuery.DataSets.Count );
  SetLength( DataSetState, FxQuery.DataSets.Count );
  SetLength( DataSetEvents, FxQuery.DataSets.Count );
  MaxNumFields := 0;
  For I := 0 To FxQuery.DataSets.Count - 1 Do
    With FxQuery.DataSets[I] Do
      If Assigned(DataSet) And (DataSet.Active) Then
      begin
        If DataSet.FieldCount > MaxNumFields then
          MaxNumFields := DataSet.FieldCount;
      end;
  If MaxNumFields > 0 then
  Begin
    SetLength( DataSetFields, FxQuery.DataSets.Count, MaxNumFields );
    SetLength( FieldEvents, FxQuery.DataSets.Count, MaxNumFields );
  End;
  Try
    //If FxQuery.FAutoDisableControls Then
      { Save position of the datasets.
        We must always do the following because the dataset need to be repositioned
        to first record before querying, otherwise incorrect results must arise,
        so AutoDisableControls property must not exist
      }
      For I := 0 To FxQuery.DataSets.Count - 1 Do
        With FxQuery.DataSets[I] Do
        Begin

          If Not FxQuery.ActiveDataSetEvents Then                  // Nonn
            SaveDataSetEvents(I, DataSet);

          If Assigned(DataSet) And (DataSet.Active) Then
          Begin
            DisabledState[I]:= DataSet.ControlsDisabled;
            If Not DisabledState[I] Then
              DataSet.DisableControls;
            // save bookmark
            BookmarkList.Add(DataSet.GetBookmark);
            // save state
            DataSetState[I] := DataSet.State;                       // Nonn ...
            If DataSet.State In [dsEdit, dsInsert] Then
            Begin
              // save field values
              For J := 0 To DataSet.FieldCount - 1 Do
                DataSetFields[I][J] := DataSet.Fields[J].AsVariant;

              DataSet.Cancel;
            End;                                                   // ... Nonn

            DataSet.First;    // must be set to first record for querying correctly
          End
          Else
          Begin
            BookmarkList.Add(Nil);
            DataSetState[I] := dsInactive;                        // Nonn
            DisabledState[I]:= False;
          End;
        End;

    CreateResultSet;

    DoIntoTableOperation;

    If {FxQuery.FAutoDisableControls And} (BookmarkList.Count > 0) Then
    Begin { LAS : 05-30-2000 }
      { restore position of the datasets }
      SysUtils.GetFormatSettings; {Dirk Orlet 2000-09 Reset to windows settings}
      For I := 0 To FxQuery.DataSets.Count - 1 Do
        With FxQuery.DataSets[I] Do
        Begin
          If Assigned(DataSet) And (DataSet.Active) And
            (I <= BookmarkList.Count - 1) Then
          Begin
            bm := TBookmark(BookmarkList[I]);
            If Assigned(bm) And DataSet.BookmarkValid(bm) And
              (DataSet.RecordCount > 0) Then
              try
                DataSet.GotoBookmark(bm);
              except
                // Workaround for bookmark not found error
                on EDatabaseError do { } // Ignore errors if failed going to bookmarked record
              end;

            // restore state                              // Nonn ...
            If DataSetState[I] In [dsEdit, dsInsert] Then
            Begin
              Case DataSetState[I] Of
                dsEdit:
                  DataSet.Edit;
                dsInsert:
                  DataSet.Insert;
              End;

              // restore field values
              For J := 0 To DataSet.FieldCount - 1 Do
                DataSet.Fields[J].AsVariant := DataSetFields[I][J];
            End;                                          // ... Nonn

            If Assigned(bm) Then
              DataSet.FreeBookmark(bm);
            If Not DisabledState[I] Then
              DataSet.EnableControls;
          End;

          If Not FxQuery.ActiveDataSetEvents Then        // Nonn
            RestoreDataSetEvents(I, DataSet);

        End;
    End;
  Finally
    BookmarkList.Free;
    RestoreFormatSettings(SFS);                          // Nonn
    If Assigned(FxQuery.FOnAfterQuery) Then
      FxQuery.FOnAfterQuery(Self);

    Finalize(DataSetEvents);
    Finalize(DisabledState);
    Finalize(DataSetState);
    If MaxNumFields > 0 then
    begin
      Finalize(DataSetFields);
      Finalize(FieldEvents);
    end;
  End;
End;

Procedure TSqlAnalizer.DoSelect;
Begin
  If Not (FStatement in [ssSelect, ssUnion]) Then
    Raise ExQueryError.Create(SIsNotValidInSelect);
  SafeCreateResultSet;
End;

Procedure TSqlAnalizer.AddFieldIfNot(Const fieldName: String);
Begin
  If FxQuery.RefFields.IndexOf(fieldName) < 0 Then
    FxQuery.RefFields.Add(fieldName);
End;

Procedure TSqlAnalizer.DoExecSQL;
Var
  I: Integer;
  TmpThousandSeparator: Char; { LAS : 05-30-2000 }
  TmpDecimalSeparator: Char; { LAS : 05-30-2000 }
Begin

  If FStatement = ssSelect Then
    Raise ExQueryError.Create(SIsNotValidInExecSQL);

  If FStatement = ssCreateTable Then { CREATE TABLE }
  Begin

    If Assigned(FxQuery.OnCreateTable) Then
    Begin
      For I := 0 To FCreateTableList.Count - 1 Do
        FxQuery.OnCreateTable(FxQuery, FCreateTableList[I]);
    End;
    Exit;

  End
  Else If FStatement = ssAlterTable Then { ALTER TABLE }
  Begin

    If Assigned(FxQuery.OnAlterTable) Then
    Begin
      For I := 0 To FAlterTableList.Count - 1 Do
        FxQuery.OnAlterTable(FxQuery, FAlterTableList[I]);
    End;
    Exit;

  End
  Else If FStatement = ssCreateIndex Then { CREATE INDEX }
  Begin

    If Assigned(FxQuery.OnCreateIndex) Then
      FxQuery.OnCreateIndex(FxQuery, IndexUnique, IndexDescending, IndexTable,
        IndexName,
        IndexColumnList);
    Exit;

  End
  Else If FStatement = ssDropTable Then { DROP TABLE }
  Begin

    If Assigned(FxQuery.OnDropTable) Then
      FxQuery.OnDropTable(FxQuery, FIndexTable);
    Exit;

  End
  Else If FStatement = ssDropIndex Then { DROP INDEX }
  Begin

    If Assigned(FxQuery.OnDropIndex) Then
      FxQuery.OnDropIndex(FxQuery, FIndexTable, FIndexName);
    Exit;

  End
  Else If FStatement = ssPackTable Then { PACK TABLE }
  Begin
    FxQuery.PackTable(FTableList);
    Exit;
  End
  Else If FStatement = ssZapTable Then { ZAP TABLE }
  Begin
    FxQuery.ZapTable(FTableList);
    Exit;
  End
  Else If FStatement = ssReindexTable Then { REINDEX TABLE }
  Begin
    FxQuery.ReindexTable(FTableList);
    Exit;
  End;
  TmpThousandSeparator := ThousandSeparator; { LAS : 05-30-2000 }
  TmpDecimalSeparator := DecimalSeparator; { LAS : 05-30-2000 }
  ThousandSeparator := ','; { LAS : 05-30-2000 }
  DecimalSeparator := '.'; { LAS : 05-30-2000 }
  FxQuery.FRowsAffected := 0;
  Try
    SafeCreateResultSet;
  Finally
    ThousandSeparator := TmpThousandSeparator; { LAS : 05-30-2000 }
    DecimalSeparator := TmpDecimalSeparator; { LAS : 05-30-2000 }
  End;
End;

Function TSqlAnalizer.HasAggregates: Boolean;
Var
  I: Integer;
Begin
  Result := False;
  For I := 0 To FColumnList.Count - 1 Do
  Begin
    With FColumnList[I] Do
    Begin
      If AggregateList.Count > 0 Then
      Begin
        Result := True;
        Exit;
      End;
    End;
  End;
End;

Function TSqlAnalizer.HasDistinctAggregates: Boolean;
Var
  I, K: Integer;
Begin
  Result := False;
  For I := 0 To FColumnList.Count - 1 Do
  Begin
    With FColumnList[I] Do
    Begin
      For K := 0 To AggregateList.Count - 1 Do
      Begin
        If AggregateList[K].IsDistinctAg Then
        Begin
          Result := True;
          Exit;
        End;
      End;
    End;
  End;
End;

Procedure TSqlAnalizer.InitializeResultSet;
//var
  //I: Integer;
  //Accept: Boolean;
Begin
  If Assigned(FResultSet) Then
    FResultSet.Free;
{$IFDEF False}
  Accept := False;
  If Accept { next release }
  And (FStatement = ssSelect) { select statement }
  And FxQuery.FAllSequenced { all sequenced property}
  And (TableList.Count = 1) { no JOINing }
  And (FSubQueryList.Count = 0) { no subqueries }
  And (Length(FPivotStr) = 0) { the command is not TRANSFORM...PIVOT }
  And (FParentAnalizer = Nil) { don't have parent analizer }
  And Not HasAggregates { not aggregates }
  And Not HasDistinctAggregates { not COUNT(DISTINCT country) }
  And (Length(FWhereStr) = 0) { no where clause }
  And (FWhereOptimizeList.Count = 0) { no optimizations } Then
  Begin
    FResultSet := TSeqResultSet.Create;
    With TSeqResultSet(FResultSet) Do
    Begin
      For I := 0 To ColumnList.Count - 1 Do
      Begin
        fResolverList.Add(ColumnList[I].Resolver);
        ColumnList[I].AutoFree := False;
      End;
    End;
  End
  Else
{$ENDIF}
  Begin
    If FxQuery.InMemResultSet Then
      FResultSet := TMemResultSet.Create
    Else
      FResultSet := TFileResultSet.Create(FxQuery.FMapFileSize);
  End;
End;

Function TSqlAnalizer.GetResultSet: TResultSet;
Begin
  Result := FResultSet;
End;

Procedure TSqlAnalizer.SetResultSet(Value: TResultSet);
Begin
  FResultSet := Value;
End;

{-------------------------------------------------------------------------------}
{                  Implements TxDataSetItem                                     }
{-------------------------------------------------------------------------------}

Procedure TxDataSetItem.Assign(Source: TPersistent);
Begin
  If Source Is TxDataSetItem Then
  Begin
    FAlias := TxDataSetItem(Source).Alias;
    FDataSet := TxDataSetItem(Source).DataSet;
  End
  Else
    Inherited Assign(Source);
End;

Function TxDataSetItem.GetDisplayName: String;
Begin
  Result := GetCaption;
  If Result = '' Then
    Result := Inherited GetDisplayName;
End;

Function TxDataSetItem.GetCaption: String;
Begin
  If FDataSet <> Nil Then
    Result := FDataSet.Name + ' (' + FAlias + ')'
  Else
    Result := '(not defined)';
End;

Procedure TxDataSetItem.SetDataSet(Value: TDataSet);
Begin
  If Value = TxDataSets(Collection).FxQuery Then
    Raise exception.Create(SCircularReference);

  If Not (Value Is TxDataSets(Collection).DataSetClass) Then
    Raise exception.CreateFmt(SDataSetUnexpected,
      [TxDataSets(Collection).DataSetClass.ClassName]);

  FDataSet := Value;

  If Collection Is TxDataSets Then
    Value.FreeNotification(TxDataSets(Collection).FxQuery);

  If (FAlias = '') And Assigned(Value) Then
    FAlias := Value.Name;
End;

Procedure TxDataSetItem.SetAlias(Const Value: String);
Begin
  If TxDataSets(Collection).IndexOfAlias(Value) >= 0 Then
    Raise ExQueryError.Create(SDuplicateAlias);
  FAlias := Value;
End;

{-------------------------------------------------------------------------------}
{                  Implements TxDataSets                                        }
{-------------------------------------------------------------------------------}

Constructor TxDataSets.Create(AOwner: TPersistent);
Begin
  Inherited Create(AOwner, TxDataSetItem);
  FxQuery := AOwner As TCustomxQuery;
  FDataSetClass := TDataSet;
End;

Function TxDataSets.GetItem(Index: Integer): TxDataSetItem;
Begin
  Result := TxDataSetItem(Inherited GetItem(Index));
End;

Procedure TxDataSets.SetItem(Index: Integer; Value: TxDataSetItem);
Begin
  Inherited SetItem(Index, Value);
End;

Function TxDataSets.IndexOFAlias(Const Alias: String): Integer;
Var
  I: Integer;
Begin
  Result := -1;
  For I := 0 To Count - 1 Do
    If AnsiCompareText(TxDataSetItem(Inherited GetItem(I)).Alias, Alias) = 0 Then
    Begin
      Result := I;
      Exit;
    End;
End;

{function TxDataSets.GetOwner: TPersistent;
begin
  Result:= FxQuery;
end;}

Function TxDataSets.Add: TxDataSetItem;
Begin
  Result := TxDataSetItem(Inherited Add);
End;


{-------------------------------------------------------------------------------}
{          Define and implements TxqBlobStream                                  }
{-------------------------------------------------------------------------------}

Type

  TxqBlobStream = Class(TMemoryStream)
  Private
    FField: TBlobField;
    FDataSet: TCustomxQuery;
    FIndex: Integer;
    Procedure ReadBlobData;
  Public
    Constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    Destructor Destroy; Override;
  End;

Constructor TxqBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
Begin
  Inherited Create;
  FField := Field;
  FIndex := FField.Index;
  FDataSet := FField.DataSet As TCustomxQuery;
  If Mode = bmRead Then
    ReadBlobData;
End;

{MED: 09-07-00, added- caused memory leak big time!}

Destructor TxqBlobStream.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TxqBlobStream.ReadBlobData;
Var
{$IFDEF False}
  RecNo: Integer;
  Accepted: Boolean;
{$ENDIF}
  Field: TxqField;
Begin
  If (FDataSet.ResultSet.RecordCount=0) or (FDataSet.RecNo < 1) Then Exit;
  FDataSet.ResultSet.RecNo := FDataSet.RecNo;
  Field := FDataSet.ResultSet.Fields[FIndex];
{$IFDEF False}
  RecNo := FDataSet.ResultSet.Fields[FIndex].AsInteger;
  If RecNo < 1 Then
  Begin
    If Assigned(FDataSet.OnBlobNeeded) Then
    Begin
      Accepted := False;
      FDataSet.OnBlobNeeded(FDataSet, Field.SourceField.DataSet, Accepted);
      If Not Accepted Then
      begin
        Exit;
      end;
    End
    Else
    begin
      Exit;
    end;
  End
  Else
{$ENDIF}
  Begin
    { position in the original record }
    Field.SourceField.Dataset.GotoBookmark(FDataSet.ResultSet.GetSourceBookmark);
    //SetRecordNumber(Field.SourceField.DataSet, RecNo);
  End;

  (Field.SourceField As TBlobField).SaveToStream(Self);
  Self.Position := 0;
End;

{-------------------------------------------------------------------------------}
{                  Defines and implements TxQueryDataLink                       }
{-------------------------------------------------------------------------------}

Type
  TxQueryDataLink = Class({$IFDEF level3}TDataLink{$ELSE}TDetailDataLink{$ENDIF})
  Private
    FxQuery: TCustomxQuery;
  Protected
    Procedure ActiveChanged; Override;
    Procedure RecordChanged(Field: TField); Override;
{$IFNDEF LEVEL3}
    Function GetDetailDataSet: TDataSet; Override;
{$ENDIF}
    Procedure CheckBrowseMode; Override;
  Public
    Constructor Create(AxQuery: TCustomxQuery);
  End;

Constructor TxQueryDataLink.Create(AxQuery: TCustomxQuery);
Begin
  Inherited Create;
  FxQuery := AxQuery;
End;

Procedure TxQueryDataLink.ActiveChanged;
Begin
  If FxQuery.Active Then
    FxQuery.RefreshParams;
End;

{$IFNDEF LEVEL3}
Function TxQueryDataLink.GetDetailDataSet: TDataSet;
Begin
  Result := FxQuery;
End;
{$ENDIF}

Procedure TxQueryDataLink.RecordChanged(Field: TField);
Begin
  If (Field = Nil) And FxQuery.Active Then
    FxQuery.RefreshParams;
End;

Procedure TxQueryDataLink.CheckBrowseMode;
Begin
  If FxQuery.Active Then
    FxQuery.CheckBrowseMode;
End;

{-------------------------------------------------------------------------------}
{                  Implements TCustomxQuery                                     }
{-------------------------------------------------------------------------------}

Constructor TCustomxQuery.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FReadOnly := False; // editing
  FDataSets := TxDataSets.Create(Self);
  FSQL := TStringList.Create;
  FSQLScript := TStringList.Create;
  TStringList(SQL).OnChange := QueryChanged;
  FDataLink := TxQueryDataLink.Create(Self);
  FParams := TParams.Create{$IFNDEF LEVEL3}(Self){$ENDIF};
  FParamCheck := True;
  FAutoDisableControls := True;
  FInMemResultSet := True;
  FDateFormat := ''; //ShortDateFormat;   { empty string means to use ShortDateFormat global var }
  FDisabledDataSets := TList.Create;
  FMapFileSize := 2000000;
  FShowWaitCursor := True;
  FWhereOptimizeMethod := omSetFilter;
  FRefFields := TStringList.create;
  FParamsAsFields:= TParamsAsFields.Create(Self);
  FActiveDataSetEvents := True;           // Nonn
End;

Destructor TCustomxQuery.Destroy;
Begin
  Disconnect;
  FSQL.Free;
  FSQLScript.Free;
  FFilterExpr.Free;
  FParams.Free;
  FDataLink.Free;
  FDisabledDataSets.Free;
  { I am not sure if I must move up because some customer reported memory leak
    when its placed above }
  ClearTempDatasets;
  FDataSets.Free;
  FRefFields.Free;
  FParamsAsFields.Free;
  Inherited Destroy;
End;

Function TCustomxQuery.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
Begin
  Result := TxqBlobStream.Create(Field As TBlobField, Mode);
End;

procedure TCustomxQuery._ReadRecord(Buffer: TRecordBuffer; IntRecNum: Integer);
Var
  I: Integer;
  vField: TxqField;
  vs: String;
  Avs: AnsiString; { patched by ccy }
  Wvs: WideString; { patched by ccy }
  vf: double;
  vn: Integer;
  vb: WordBool;
  vTimeStamp: TTimeStamp;
  vData: TDateTimeRec;
  vFieldType: TFieldType;
  FldDef: TFieldDef;
  b: Byte;
  si: SmallInt;
Begin
  FResultSet.RecNo := IntRecNum + 1;
  FillChar(Buffer^, FRecordSize, #0);
  For I := 0 To FResultSet.Fields.Count - 1 Do
  Begin
    vField := FResultSet.Fields[I];
    Case vField.DataType Of
      ttstring: { patched by ccy }
        Begin
          vs := vField.Asstring;
          If Length(vs) > 0 Then begin
            FldDef := FieldDefs[I];
            if (FieldDefs[I].DataType = ftString) or (FieldDefs[I].DataType = ftMemo) then begin
              Avs := AnsiString(vs);
              Move(Avs[1], (Buffer + vField.FFieldOffset)^, IMin(FldDef.Size, Length(Avs)));
            end else if (FieldDefs[I].DataType = ftWideString) or (FieldDefs[I].DataType = ftWideMemo) then begin
              Wvs := vs;
              Move(Wvs[1], (Buffer + vField.FFieldOffset)^, IMin(FldDef.Size, Length(Wvs)) * SizeOf(WideChar));
            end else
              Assert(False, 'Unsupported data type in procedure TxqStringField.SetAsstring');
          end;
        End;
      ttFloat:
        Begin
          vf := vField.AsFloat;
          vFieldType := ftUnknown;
          If Assigned(vField.SourceField) Then
          Begin
            vFieldType := vField.SourceField.DataType;
            {$IFDEF LEVEL6}
            If vFieldType=ftTimeStamp then
              vFieldType:= ftDateTime;
            {$ENDIF}
          End
          Else If vField.CastType > 0 Then
          Begin
            Case vField.CastType Of
              RW_DATE: vFieldType := ftDate;
              RW_TIME: vFieldType := ftTime;
              RW_DATETIME: vFieldType := ftDateTime;
            End;
          End;
          If vFieldType In [{ftDate, ftTime,} ftDateTime] Then
          Begin
            If vf <> 0 Then
            Begin
              vTimeStamp := DateTimeToTimeStamp(vf);
              {Case vFieldType Of
                ftDate: vData.Date := vTimeStamp.Date;
                ftTime: vData.Time := vTimeStamp.Time;
              Else }
                vData.DateTime := TimeStampToMSecs(vTimeStamp);
              //End;
              Move(vData, (Buffer + vField.FFieldOffset)^, SizeOf(TDateTimeRec))
            End
            Else
              Move(vf, (Buffer + vField.FFieldOffset)^, SizeOf(vf))
          End
          Else
            Move(vf, (Buffer + vField.FFieldOffset)^, SizeOf(Double));
        End;
      ttInteger:
        Begin
          vn := vField.AsInteger;
          if Assigned(vField.SourceField) then
          begin
            if vField.SourceField.DataSize = 1 then
            begin
              b:= vn;
              Move(b, (Buffer + vField.FFieldOffset)^, 1);
            end else if vField.SourceField.DataSize = 2 then
            begin
              si:= vn;
              Move(si, (Buffer + vField.FFieldOffset)^, 2);
            end else
              Move(vn, (Buffer + vField.FFieldOffset)^, vField.SourceField.DataSize);
          end else
            Move(vn, (Buffer + vField.FFieldOffset)^, SizeOf(Integer));
        End;
      ttBoolean:
        Begin
          vb := vField.AsBoolean;
          Move(vb, (Buffer + vField.FFieldOffset)^, SizeOf(WordBool));
        End;
    End;
  End;
End;

Procedure TCustomxQuery.SetQuery(Value: Tstrings);
Begin
  If SQL.Text <> Value.Text Then
  Begin
    Disconnect;
    SQL.BeginUpdate;
    Try
      SQL.Assign(Value);
    Finally
      SQL.EndUpdate;
    End;
  End;
End;

Procedure TCustomxQuery.SetSQLScript(Value: Tstrings);
Begin
  If SQLScript.Text <> Value.Text Then
  Begin
    SQLScript.BeginUpdate;
    Try
      SQLScript.Assign(Value);
    Finally
      SQLScript.EndUpdate;
    End;
  End;
End;

Procedure TCustomxQuery.AddDataSet(DataSet: TDataSet; Const Alias: String);
Var
  Item: TxDataSetItem;
  s: String;
Begin
  If Not Assigned(DataSet) Then
    Exit;
  If Not (DataSet Is TDataSetClass) Then
    Raise ExQueryError.CreateFmt(SDataSetUnexpected, [TDataSetClass.ClassName]);
  If FDataSets.IndexOFAlias(Alias) >= 0 Then
    Raise ExQueryError.Create(SDuplicateAlias);
  Item := FDataSets.Add;
  s := Alias;
  If Length(s) = 0 Then
    s := DataSet.Name;
  Item.Alias := s;
  Item.DataSet := DataSet;
End;

Procedure TCustomxQuery.ClearDatasets;
Begin
  FDataSets.Clear;
End;

Function TCustomxQuery.GetPrepared: Boolean;
Begin
  Result := FPrepared;
End;

Procedure TCustomxQuery.SetPrepare(Value: Boolean);
Begin
  If Value Then
    Prepare
  Else
    UnPrepare;
End;

Procedure TCustomxQuery.ClearTempDatasets;
Var
  I: Integer;
  found: boolean;
  Item: TxDataSetItem;
  D: TDataset;
Begin
  D:= Nil;
  Repeat
    found := False;
    For I := FDataSets.Count - 1 Downto 0 Do
    Begin
      Item := FDataSets[I];
      If Item.Temporal And Assigned(Item.Dataset) Then
      Begin
        D := Item.Dataset;
        D.Free;
        found := true;
        break;
      End;
    End;
    If found Then
      { confirm that the item was also deleted}
      For I := 0 To FDataSets.Count - 1 Do
      Begin
        Item := FDataSets[I];
        If Item.Dataset = D Then
        Begin
          Item.Free;
          Break;
        End;
      End;
  Until Not found;
End;

Procedure TCustomxQuery.InternalOpen;
Var
  Analizer: TSqlAnalizer;
  Field: TxqField;
  ErrLine, ErrCol: Integer;
  ErrMsg, Errtxt: String;
  I, Offset: Integer;
  TmpThousandSeparator: Char; { LAS : 05-30-2000 }
  TmpDecimalSeparator: Char; { LAS : 05-30-2000 }
  iSize: integer; { patched by ccy }
Begin

  ClearTempDatasets;

{$IFDEF XQDEMO}
  If IsFirstTime Or
    (Not (csDesigning In ComponentState) And Not IsDelphiRunning) Then
  Begin
    IsFirstTime := False;
    If Not (csDesigning In ComponentState) Then
      ShowAbout;
    If Not IsDelphiRunning Then
      Raise exception.Create(SDelphiIsNotRunning);
  End;
{$ENDIF}

  FRowsAffected := 0;

  ClearTempDatasets;
  If Not FResultSetIsDefined Then
  Begin
    If Length(Trim(FSQL.Text)) = 0 Then
      Raise ExQueryError.Create(SSQLIsEmpty);

    If Assigned(FResultSet) Then
      FreeObject(FResultSet);

    { Clear the list of referenced fields on the SQL }
    TmpThousandSeparator := ThousandSeparator;
    TmpDecimalSeparator := DecimalSeparator;
    ThousandSeparator := ',';
    DecimalSeparator := '.';

    If FDataLink.DataSource <> Nil Then
      SetParamsFromDataSet;

    Analizer := TSqlAnalizer.Create(Nil, Self);
    Try
      If Analizer.parser.yyparse = 1 Then
      Begin
        ErrLine := Analizer.lexer.yylineno;
        ErrCol := Analizer.lexer.yycolno - Analizer.Lexer.yyTextLen - 1;
        ErrMsg := Analizer.parser.yyerrormsg;
        Analizer.lexer.getyytext(ErrTxt);
        If Assigned(FOnSyntaxError) Then
        Begin
          FOnSyntaxError(Self, ErrMsg, ErrTxt, ErrLine, ErrCol, Length(ErrTxt));
          Abort;
        End
        Else
          { if not raised an error, will raise here }
          Raise ExQueryError.CreateFmt(SSyntaxErrorMsg, [ErrMsg, ErrLine, ErrCol, ErrTxt]);
      End;
      Analizer.DoSelect; { normal SELECT statement }
      FResultSet := Analizer.ResultSet;
      Analizer.ResultSet := Nil;
    Finally
      FreeObject(Analizer);
      (*if Found then
        FSQL.Text:= SavedSQLText; *)

      ThousandSeparator := TmpThousandSeparator; { LAS : 05-30-2000 }
      DecimalSeparator := TmpDecimalSeparator; { LAS : 05-30-2000 }
    End;
  End;

  { calculate the offset for every field }
  Offset := 0;
  For I := 0 To FResultSet.FFields.Count - 1 Do
  Begin
    Field := FResultSet.FFields[I];
    With Field Do
    Begin
      Case DataType Of
        ttstring: begin { patched by ccy }
                    iSize := SizeOf(AnsiChar);
                    if Assigned(SourceField) and (SourceField.DataType = ftWideString) then
                      iSize := SizeOf(WideChar);
                    DataSize := (ColWidth + 1) * iSize;
                  end;
        ttFloat: DataSize := SizeOf(Double);
        ttInteger:
          begin
          if Assigned(Field.SourceField) then
            DataSize := Field.SourceField.DataSize
          else
            DataSize := SizeOf(Integer);
          end;
        ttBoolean: DataSize := SizeOf(WordBool);
      End;
      FFieldOffset := Offset;
      Inc(Offset, DataSize);
    End;
  End;


  InternalInitFieldDefs;

  If DefaultFields Then
    CreateFields;

  { Assign the display labels as column names }
  For I := 0 To FResultSet.Fields.Count - 1 Do
  Begin
    Field := FResultSet.Fields[I];
    If Assigned(Field.SourceField) And Field.UseDisplayLabel Then
      Fields[I].DisplayLabel := Field.SourceField.DisplayLabel;
    If Assigned(Field.SourceField) And (Field.SourceField Is TNumericField) Then
      (Fields[I] as TNumericField).DisplayFormat := (Field.SourceField As TNumericField).DisplayFormat;
    If Assigned(Field.SourceField) And (Field.SourceField Is TDateTimeField) Then
      (Fields[I] as TDateTimeField).DisplayFormat := (Field.SourceField As TDateTimeField).DisplayFormat;
  End;

  BindFields(True);

  If Assigned(FResultSet) Then
    FRecordCount := FResultSet.RecordCount;

  BofCrack := -1;
  EofCrack := FRecordCount;
  FRecNo := -1;
  FRecordInfoOffset := FRecordSize;
  StartCalculated := FRecordSize + SizeOf(TRecInfo);
  FRecordBufferSize := StartCalculated + CalcFieldsSize;
  BookmarkSize := SizeOf(Integer);

  { now create the filter expression if exists }
  If FFilterExpr <> Nil Then
    FreeObject(FFilterExpr);
  If Filtered And (Length(Filter) > 0) Then
  Begin
    FFilterExpr := TExprParser.Create(Nil, Self);
    Try
      FFilterExpr.ParseExpression(Filter);
    Except
      FreeObject(FFilterExpr);
      Raise;
    End;
  End;

  FIsOpen := Assigned(FResultSet);

End;

Function TCustomxQuery.GetFieldSize(FieldType: TFieldType; Size: longint): longint;
Begin
  Case FieldType Of
{$IFNDEF LEVEL3}
    ftFixedChar, ftWideString,
{$ENDIF}
    ftString: Result := Size + 1;
    ftSmallInt: Result := SizeOf(SmallInt);
    ftInteger: Result := SizeOf(Integer);
{$IFNDEF LEVEL3}
    ftLargeInt: Result := SizeOf(Int64);
{$ENDIF}
    ftWord: Result := SizeOf(Word);
    ftBoolean: Result := SizeOf(WordBool);
    ftFloat: Result := SizeOf(Double);
    ftCurrency: Result := SizeOf(Double);
    ftDate: Result := SizeOf(TDateTimeRec);
    ftTime: Result := SizeOf(TDateTimeRec);
    ftDateTime: Result := SizeOf(TDateTimeRec);
    ftAutoInc: Result := SizeOf(Integer);
    ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary,
      ftBytes, ftVarBytes: Result := SizeOf(Pointer);
    ftBCD: Result := 34;
  Else
    Result := 0;
  End;
End;

Procedure TCustomxQuery.InternalInitFieldDefs;
Var
  I, J, Numtry: Integer;
  Field: TxqField;
  DataType: TFieldType;
  Size: Word;
  FieldName, Test: String;
  Found: Boolean;
  FldDef: TFieldDef;
Begin
  FRecordSize := 0;
  FieldDefs.Clear;
  If Not Assigned(FResultSet) Then Exit;
  If DefaultFields Then
  Begin
    For I := 0 To FResultSet.Fields.Count - 1 Do
    Begin
      Field := FResultSet.FFields[I];
      FieldName := TrimSquareBrackets(Field.FieldName);
      If Length(FieldName) = 0 Then
        FieldName := 'NULL';
      If Assigned(FOnQueryFieldName) Then
        FOnQueryFieldName(Self, I, FieldName);
      Test := FieldName;
      J := AnsiPos('.', Test);
      If J > 0 Then
      begin
        Test := TrimSquareBrackets(Copy(Test, J + 1, Length(Test)));
      end;
      Numtry := 0;
      Repeat
        Found := False;
        For J := 0 To FieldDefs.Count - 1 Do
          If AnsiCompareText(FieldDefs[J].Name, Test) = 0 Then
          Begin
            Found := True;
            Break;
          End;
        If Found Then
        Begin
          Inc(Numtry);
          Test := FieldName + '_' + IntToStr(Numtry);
          J := AnsiPos('.', Test);
          If J > 0 Then
            Test := Copy(Test, J + 1, Length(Test));
        End;
      Until Not Found;
      FieldName := Test;
      Size := 0;
      Case Field.DataType Of
        ttString:
          Begin
            DataType := {$if RTLVersion <= 18.5}ftString{$else}ftString{$ifend}; { patched by ccy }; { this fixes bug with ftWidestring }
            if Assigned(Field.SourceField) then
              DataType := Field.SourceField.DataType; { patched by ccy }
            Size := Field.DataSize;
          End;
        ttFloat:
          DataType := ftFloat;
        ttInteger:
          DataType := ftInteger;
        ttBoolean:
          DataType := ftBoolean;
      Else
        DataType := ftUnknown;
      End;
      If Assigned(Field.SourceField) Then
      Begin
        DataType := Field.SourceField.DataType;
        If ((DataType = ftFloat) And (TFloatField(Field.SourceField).Currency)) Or
           (DataType = ftBCD) Then
          DataType := ftCurrency;
        {$IFDEF LEVEL6}
        If (DataType = ftFMTBcd) Then
        begin
          DataType := ftFloat;
          Size:= 0;
        end;
        {$ENDIF}
        If DataType In [ftString{$IFDEF LEVEL4}, ftFixedChar, ftWidestring{$ENDIF}
          {$IFDEF LEVEL5}, ftGUID{$ENDIF}] Then
        Begin
          DataType := Field.SourceField.DataType; { patched by ccy }; { this fixes bug with ftWidestring }
          Size := Field.SourceField.Size;  { patched by ccy }
          If Size = 0 Then
            Size := 1;
        End;
        {$IFDEF LEVEL6}
        If DataType = ftTimeStamp then
          DataType:= ftDateTime;
        {$ENDIF}
        (* if DataType in ftNonTextTypes then
           Size := 0; *)
        If DataType In [ftAutoInc] Then
          Size := 0;
      End
      Else If Field.CastType > 0 Then
      Begin
        Case Field.CastType Of
          RW_CHAR:
            Begin
              DataType := ftstring;
              Size := Field.CastLen;
            End;
          RW_INTEGER:
            Begin
              DataType := ftInteger;
              Size := 0;
            End;
          RW_BOOLEAN:
            Begin
              DataType := ftBoolean;
              Size := 0;
            End;
          RW_DATE:
            Begin
              DataType := ftDate;
              Size := 0;
            End;
          RW_TIME:
            Begin
              DataType := ftTime;
              Size := 0;
            End;
          RW_DATETIME:
            Begin
              DataType := ftDateTime;
              Size := 0;
            End;
          RW_FLOAT:
            Begin
              DataType := ftFloat;
              Size := 0;
            End;
          RW_MONEY:
            Begin
              DataType := ftCurrency;
              Size := 0;
            End;
        End;
      End;
      FieldDefs.Add(FieldName, DataType, Size, False);
      FldDef := FieldDefs[FieldDefs.Count - 1];
      If (DataType = ftBCD) And Assigned(Field.SourceField) Then
      Begin
        FldDef.Size := TBCDField(Field.SourceField).Size;
        FldDef.Precision := TBCDField(Field.SourceField).Precision;
      End;
      Inc(FRecordSize, Field.DataSize);
    End;
  End
  Else
  Begin
    For i := 0 To FieldCount - 1 Do
    Begin
      If Fields[i].FieldKind = fkData Then
      Begin
        Field := FResultSet.FFields[I];
        FieldDefs.Add(Fields[i].FieldName, Fields[i].DataType, Fields[i].Size, Fields[i].Required);
        FldDef := FieldDefs[FieldDefs.Count - 1];
        If Fields[i].DataType = ftBCD Then
        Begin
          FldDef.Size := TBCDField(Field.SourceField).Size;
          FldDef.Precision := TBCDField(Field.SourceField).Precision;
        End;
        Inc(FRecordSize, GetFieldSize(Fields[i].DataType, Fields[i].Size));
      End;
    End;
  End;
End;

Procedure TCustomxQuery.InternalClose;
Begin
  BindFields(False);

  If DefaultFields Then
    DestroyFields;

  If Assigned(FResultSet) Then
    FreeObject(FResultSet);
  DisposeDatasets; { LAS : 05-30-2000 }
  ClearTempDatasets;

  FIsOpen := False;
End;

Function TCustomxQuery.IsCursorOpen: Boolean;
Begin
  Result := FIsOpen;
End;

Function TCustomxQuery.GetCanModify: Boolean;
Begin
  Result := Not FReadOnly;
End;

Procedure TCustomxQuery.InternalGotoBookmark(Bookmark: Pointer);
Begin
  If InternalBookmarkValid(Bookmark) Then
  Begin
    FRecNo := PInteger(Bookmark)^;
    FResultSet.Recno := FRecNo + 1;
  End
  Else
    Raise ExQueryError.CreateFmt(SBookmarkNotFound, [(PInteger(Bookmark)^)]);
End;

procedure TCustomxQuery.InternalSetToRecord(Buffer: TRecordBuffer);
(*var
  ReqBookmark: Integer;*)
Begin
  FRecNo := PRecInfo(Buffer + FRecordInfoOffset).RecordNo;
  FResultSet.Recno := FRecNo + 1;
  (*ReqBookmark := PRecInfo(Buffer + FRecordInfoOffset).RecordNo;
  InternalGotoBookmark(@ReqBookmark);*)
End;

function TCustomxQuery.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
Begin
  Result := PRecInfo(Buffer + FRecordInfoOffset).BookmarkFlag;
End;

procedure TCustomxQuery.SetBookmarkFlag(Buffer: TRecordBuffer; Value:
    TBookmarkFlag);
Begin
  PRecInfo(Buffer + FRecordInfoOffset).BookmarkFlag := Value;
End;

Function TCustomxQuery.BookmarkValid(Bookmark: TBookmark): boolean;
Begin
  Result := InternalBookmarkValid(Bookmark);
End;

Function TCustomxQuery.InternalBookmarkValid(Bookmark: Pointer): boolean;
Begin
  Result := (PInteger(Bookmark)^ >= 0) And (PInteger(Bookmark)^ <= FRecordCount - 1);
End;

Function TCustomxQuery.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
Var
  b1, b2: integer;
Begin
  If Bookmark1 = Nil Then
    b1 := 0
  Else
    b1 := PInteger(Bookmark1)^;
  If Bookmark2 = Nil Then
    b2 := 0
  Else
    b2 := PInteger(Bookmark2)^;
  If b1 = b2 Then
    Result := 0
  Else If b1 < b2 Then
    Result := -1
  Else
    Result := 1;
End;

procedure TCustomxQuery.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
Begin
  PInteger(Data)^ := PRecInfo(Buffer + FRecordInfoOffset).RecordNo;
End;

procedure TCustomxQuery.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
Begin
  PRecInfo(Buffer + FRecordInfoOffset).RecordNo := PInteger(Data)^;
End;

Procedure TCustomxQuery.InternalFirst;
Begin
  FRecNo := -1;
  If FResultSet.RecordCount > 0 Then
    FResultSet.Recno := 1;
End;

Procedure TCustomxQuery.InternalLast;
Begin
  EofCrack := FRecordCount;
  FRecNo := EofCrack;
  If FRecNo >= 0 Then
    FResultSet.Recno := FRecNo;
End;

Function TCustomxQuery.GetRecordCount: Longint;
Var
  SaveState: TDataSetState;
  SavePosition: integer;
  TempBuffer: TRecordBuffer;
Begin
  CheckActive;
  If Not Filtered Then
    Result := FRecordCount
  Else
  Begin
    Result := 0;
    SaveState := SetTempState(dsBrowse);
    SavePosition := FRecNo;
    Try
      TempBuffer := AllocRecordBuffer;
      InternalFirst;
      While GetRecord(TempBuffer, gmNext, True) = grOk Do
        Inc(Result);
    Finally
      RestoreState(SaveState);
      FRecNo := SavePosition;
      FreeRecordBuffer(TempBuffer);
    End;
  End;
End;

Function TCustomxQuery.SourceBookmark: TBookmark;
Begin
  Result := Nil;
  If Not FResultSet.IsSequenced Then Exit;
  CheckActive;
  UpdateCursorPos;
  FResultSet.RecNo := FRecNo + 1;
  Result := FResultSet.GetSourceBookmark;
End;

Function TCustomxQuery.GetRecNo: Longint;
Var
  SaveState: TDataSetState;
  SavePosition: integer;
  TempBuffer: TRecordBuffer;
Begin
  CheckActive;
  UpdateCursorPos;
  If Not Filtered Then
  Begin
    If FRecNo < 0 Then
      Result := 1
    Else
      Result := FRecNo + 1;
  End
  Else
  Begin
    Result := 0;
    SaveState := SetTempState(dsBrowse);
    SavePosition := FRecNo;
    Try
      TempBuffer := AllocRecordBuffer;
      InternalFirst;
      Repeat
        If GetRecord(TempBuffer, gmNext, True) = grOk Then
          Inc(Result);
      Until PRecInfo(TempBuffer + FRecordInfoOffset).RecordNo = SavePosition;
    Finally
      RestoreState(SaveState);
      FRecNo := SavePosition;
      FreeRecordBuffer(TempBuffer);
    End;
  End;
End;

Procedure TCustomxQuery.SetRecNo(Value: Integer);
Var
  SaveState: TDataSetState;
  SavePosition: integer;
  TempBuffer: TRecordBuffer;
Begin
  CheckBrowseMode;
  If Not Filtered Then
  Begin
    If (Value >= 1) And (Value <= FRecordCount) Then
    Begin
      FRecNo := Value - 1;
      FResultSet.Recno := FRecNo + 1;
      Resync([]);
      doAfterScroll;
    End;
  End
  Else
  Begin
    SaveState := SetTempState(dsBrowse);
    SavePosition := FRecNo;
    Try
      TempBuffer := AllocRecordBuffer;
      InternalFirst;
      Repeat
        Begin
          If GetRecord(TempBuffer, gmNext, True) = grOk Then
            Dec(Value)
          Else
          Begin
            FRecNo := SavePosition;
            break;
          End;
        End;
      Until Value = 0;
      doAfterScroll;
    Finally
      RestoreState(SaveState);
      FreeRecordBuffer(TempBuffer);
    End;
  End;
End;

Function TCustomxQuery.GetRecordSize: Word;
Begin
  Result := FRecordSize + SizeOf(TRecInfo) + CalcFieldsSize;
End;

procedure TCustomxQuery.InternalInitRecord(Buffer: TRecordBuffer);
Begin
  FillChar(Buffer^, FRecordBufferSize, #0);
End;

function TCustomxQuery.AllocRecordBuffer: TRecordBuffer;
Begin
  Result := AllocMem(FRecordBufferSize);
End;

procedure TCustomxQuery.FreeRecordBuffer(var Buffer: TRecordBuffer);
Begin
  FreeMem(Buffer);
End;

function TCustomxQuery.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
    doCheck: Boolean): TGetResult;
Var
  Acceptable: Boolean;
Begin
  Result := grOk;
  Acceptable := False;
  Repeat
    Case GetMode Of
      gmCurrent:
        Begin
          If FRecNo >= FRecordCount Then
          Begin
            Result := grEOF;
          End
          Else If FRecNo < 0 Then
          Begin
            Result := grBOF;
          End
          Else
            Result := grOk;
        End;
      gmNext:
        Begin
          If FRecNo < FRecordCount - 1 Then
          Begin
            Inc(FRecNo);
            Result := grOK;
          End
          Else
          Begin
            Result := grEOF;
          End;
        End;
      gmPrior:
        Begin
          If FRecNo > 0 Then
          Begin
            Dec(FRecNo);
            Result := grOK;
          End
          Else
          Begin
            Result := grBOF;
          End;
        End;
    End;
    { fill record data area of buffer }
    If Result = grOK Then
    Begin
      _ReadRecord(Buffer, FRecNo);
      With PRecInfo(Buffer + FRecordInfoOffset)^ Do
      Begin
        BookmarkFlag := bfCurrent;
        RecordNo := FRecNo;
      End;
      ClearCalcFields(Buffer);
      GetCalcFields(Buffer);
      Acceptable := FilterRecord(Buffer);
      If (GetMode = gmCurrent) And Not Acceptable Then
        Result := grError;
    End;
    If (Result = grError) And doCheck Then
      Raise ExQueryError.Create(SGetRecordInvalid);
  Until (Result <> grOk) Or Acceptable;
End;

function TCustomxQuery.FilterRecord(Buffer: TRecordBuffer): Boolean;
Var
  SaveState: TDatasetState;
Begin
  Result := True;
  If Not Filtered Then Exit;
  If Not (Assigned(FFilterExpr) Or Assigned(OnFilterRecord)) Then Exit;
  SaveState := SetTempState(dsFilter);
  FFilterBuffer := Buffer;
  If Assigned(OnFilterRecord) Then
    OnFilterRecord(Self, Result);
  If Assigned(FFilterExpr) And Result Then
    Result := FFilterExpr.Expression.AsBoolean;
  RestoreState(SaveState);
End;

procedure TCustomxQuery.ClearCalcFields(Buffer: TRecordBuffer);
Begin
  If CalcFieldsSize > 0 Then
    FillChar(Buffer[StartCalculated], CalcFieldsSize, 0);
End;

Function TCustomxQuery.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
Var
  vFieldOffset: Integer;
  vxqField: TxqField;
  RecBuffer: TRecordBuffer;
  c: Currency;
  d: Double;
  i: integer;
  IsBlank: Boolean;
Begin
  Result := False;
  If Field.FieldNo < 1 Then Exit;

  { IMPORTANT:
    It's very important not to access Recno property inside this method because
    this causes stack overflow when this TDataset is filtered }
  If Not Filtered And (Recno <> FResultSet.Recno) And (Recno > 0) And
     (Recno < FResultSet.RecordCount) Then
    FResultSet.Recno := Recno;
  IsBlank := FResultSet.Fields[Field.FieldNo - 1].IsNull;
  If Buffer = Nil Then
  Begin
    //Dataset checks if field is null by passing a nil buffer
    //Return true if it is not null
    Result := Not IsBlank;
    Exit;
  End;
  RecBuffer := GetActiveRecordBuffer;
  If Not FIsOpen Or (RecBuffer = Nil) Then Exit;
  If Field.FieldKind In [fkCalculated, fkLookup] Then
  Begin
    inc(RecBuffer, StartCalculated + Field.Offset);
    If Not ((Byte(RecBuffer[0]) = 0) Or (Buffer = Nil)) Then
      CopyMemory(Buffer, @RecBuffer[1], Field.DataSize);
  End
  Else
  Begin
    vxqField := FResultSet.Fields[Field.FieldNo - 1];
    vFieldOffset := vxqField.FFieldOffset;
    If Assigned(Buffer) Then
    Begin
      If (Field.DataType = ftBCD) And (vxqField.DataType = ttInteger) Then
      Begin
        Move((RecBuffer + vFieldOffset)^, i, SizeOf(integer));
        c := i;
        Move(c, Buffer^, SizeOf(double));
      End
      Else If (Field.DataType = ftBCD) Then
      Begin
        Move((RecBuffer + vFieldOffset)^, d, SizeOf(Double));
        c := d;
        Move(c, Buffer^, SizeOf(double));
      End
      Else
      Begin
         if Field.DataType in [ftDate, ftTime] Then
         Begin
           Move((RecBuffer + vFieldOffset)^, d, SizeOf(d));
           if Field.DataType = ftDate then
             Integer(Buffer^) := DateTimeToTimeStamp(d).Date
           else
             Integer(Buffer^) := DateTimeToTimeStamp(d).Time;
         End Else
           Move((RecBuffer + vFieldOffset)^, Buffer^, Field.DataSize);
      End;
      { empty date problem fixed by Stephan Trapp 05.01.2000 }
      If (vxqField.SourceField <> Nil) And
         (vxqField.SourceField.DataType In [ftDate, ftTime, ftDateTime]) Then
      Begin
        If Double(Buffer^) = 0 Then { 693594 = Delphi1-Zero-Date }
        Begin
          Result := False;
          Exit;
        End;
      End;
    End
    Else
    Begin
      If Field.DataType = ftBoolean Then
      Begin
        Move((RecBuffer + vFieldOffset)^, Buffer, Field.DataSize);
      End;
    End;
  End;
  Result := Not IsBlank;
End;

Procedure TCustomxQuery.InternalAddRecord(Buffer: Pointer; Append: Boolean);
Begin
  FResultSet.Insert;
  FRecordCount := FResultSet.RecordCount;
  UpdateCursorPos;
  if Append then
    InternalLast();
End;

Procedure TCustomxQuery.InternalDelete;
Begin
  FResultSet.RecNo := Recno;
  FResultSet.Delete;
  FRecordCount := FResultSet.RecordCount;
  UpdateCursorPos;
End;

procedure TCustomxQuery.InternalInsert;
begin
  FResultSet.Insert;
  FRecordCount := FResultSet.RecordCount;
  UpdateCursorPos;
end;

Procedure TCustomxQuery.InternalHandleException;
Begin
  Application.HandleException(Self);
End;

Procedure TCustomxQuery.InternalPost;
Begin
End;

Function TCustomxQuery.DataSetByName(Const Name: String): TDataSet;
Var
  Index: Integer;
  Item: TxDataSetItem;
Begin
  Result := Nil;
  For Index := 0 To FDataSets.Count - 1 Do
  Begin
    Item := FDataSets[Index];
    If Assigned(Item.DataSet) And
      (AnsiCompareText(Item.Alias, TrimSquareBrackets(Name)) = 0) Then
    Begin
      Result := Item.DataSet;
      Exit;
    End;
  End;
End;

Procedure TCustomxQuery.Notification(AComponent: TComponent; Operation: TOperation);
Var
  I: Integer;
  Item: TxDataSetItem;
  Found: Boolean;
Begin
  Inherited Notification(AComponent, Operation);
  If (Operation = opRemove) And Assigned(FDataSets) Then
    Repeat
      Found := False;
      For I := 0 To FDataSets.Count - 1 Do
      Begin
        Item := FDataSets[I];
        If AComponent = Item.DataSet Then
        Begin
          Item.Free;
          Found := True;
          Break;
        End;
      End;
    Until Not Found;
End;

function TCustomxQuery.GetActiveRecordBuffer: TRecordBuffer;
Begin
  Case State Of
    dsBrowse:
      If IsEmpty Then
        Result := Nil
      Else
        Result := ActiveBuffer;
    dsCalcFields: Result := CalcBuffer;
    dsFilter: Result := FFilterBuffer;
    dsEdit, dsInsert: Result := ActiveBuffer;
    dsNewValue, dsOldValue, dsCurValue: Result := ActiveBuffer;
{$IFDEF level5}
    dsBlockRead: Result := ActiveBuffer;
{$ENDIF}
  Else
    Result := Nil;
  End;
End;

Function TCustomxQuery.SourceDataSet: TDataSet;
Begin
  Result := FResultSet.SourceDataSet;
End;

Function TCustomxQuery.ResultSetIsSequenced: Boolean;
Begin
  Result := FResultSet.IsSequenced;
End;

Procedure TCustomxQuery.ExecSQL;
Var
  Analizer: TSqlAnalizer;
  ErrLine, ErrCol: Integer;
  ErrMsg, ErrTxt: String;
Begin

  ClearTempDatasets;

{$IFDEF XQDEMO}
  If Not (csDesigning In ComponentState) And Not IsDelphiRunning Then
    Raise exception.Create(SDelphiIsNotRunning);
{$ENDIF}

  If Length(Trim(FSQL.Text)) = 0 Then
    Raise ExQueryError.Create(SSQLIsEmpty);

  If Assigned(FResultSet) Then
    FreeObject(FResultSet);

  { clear the list of referenced fields }
  FRowsAffected := 0;
  Analizer := TSqlAnalizer.Create(Nil, Self);
  Try
    If Analizer.parser.yyparse = 1 Then
    Begin
      ErrLine := Analizer.lexer.yylineno;
      ErrCol := Analizer.lexer.yycolno - Analizer.lexer.yyTextLen - 1;
      ErrMsg := Analizer.parser.yyerrormsg;
      Analizer.lexer.getyytext(ErrTxt);
      If Assigned(FOnSyntaxError) Then
      Begin
        FOnSyntaxError(Self, ErrMsg, ErrTxt, ErrLine, ErrCol, Length(ErrTxt));
        Abort;
      End
      Else
        { if not raised an error, will raise here }
        Raise ExQueryError.CreateFmt(SSyntaxErrorMsg, [ErrMsg, ErrLine, ErrCol, ErrTxt]);
    End;
    Analizer.DoExecSQL;
  Finally
    FreeObject(Analizer);
  End;
End;

Function TCustomxQuery.BCDToCurr(BCD: Pointer; Var Curr: Currency): Boolean;
Begin
  Move(BCD^, Curr, SizeOf(Currency));
  Result := True;
End;

Function TCustomxQuery.CurrToBCD(Const Curr: Currency; BCD: Pointer; Precision,
  Decimals: Integer): Boolean;
Begin
  Move(Curr, BCD^, SizeOf(Currency));
  Result := True;
End;

{$IFDEF LEVEL4}

Procedure TCustomxQuery.SetBlockReadSize(Value: Integer);
{$IFNDEF LEVEL5}
Var
  DoNext: Boolean;
{$ENDIF}
Begin
  If Value <> BlockReadSize Then
  Begin
    If (Value > 0) Or (Value < -1) Then
    Begin
      Inherited;
      BlockReadNext;
    End
    Else
    Begin
{$IFNDEF LEVEL5}
      doNext := Value = -1;
{$ENDIF}
      Value := 0;
      Inherited;

{$IFNDEF LEVEL5}
      If doNext Then
        Next
      Else
      Begin
{$ENDIF}
        CursorPosChanged;
        Resync([]);
{$IFNDEF LEVEL5}
      End;
{$ENDIF}
    End;
  End;
End;
{$ENDIF}

Procedure TCustomxQuery.SetFiltered(Value: Boolean);
Begin
  If FIsOpen Then
  Begin
    CheckBrowseMode;
    If Filtered <> Value Then
    Begin
      Inherited SetFiltered(Value);
      If Value Then
        SetFilterData(Filter);
    End;
    First;
  End
  Else
    Inherited SetFiltered(Value);
End;

Procedure TCustomxQuery.SetFilterData(Const Text: String);
Begin
  If FIsOpen And Filtered Then
  Begin
    CheckBrowseMode;
    If Assigned(FFilterExpr) Then
      FreeObject(FFilterExpr);
    If Length(Text) > 0 Then
    Begin
      FFilterExpr := TExprParser.Create(Nil, Self);
      Try
        FFilterExpr.ParseExpression(Text);
      Except
        FreeObject(FFilterExpr);
        Raise;
      End;
    End;
    First;
  End;
  Inherited SetFilterText(Text);
End;

Procedure TCustomxQuery.SetFilterText(Const Value: String);
Begin
  SetFilterData(Value);
End;

Function TCustomxQuery.IsSequenced: Boolean;
Begin
  Result := Not Filtered;
End;

Function TCustomxQuery.GetAbout: String;
Begin
  Result := SXQUERY_ABOUT;
End;

Procedure TCustomxQuery.SetAbout(Const Value: String);
Begin
// nothing here
End;

Function TCustomxQuery.Find(Const Expression: String): Boolean;
Var
  FindExpr: TExprParser;
  SavedRecno: Integer;
Begin
  Result := False;
  CheckActive;
  If Length(Expression) = 0 Then Exit;

  FindExpr := TExprParser.Create(Nil, Self);
  Try
    FindExpr.ParseExpression(Expression);
  Except
    On E: exception Do
    Begin
      FindExpr.Free;
      Raise;
    End;
  End;
  SavedRecNo := RecNo;
  DisableControls;
  Try
    First;
    While Not EOF Do
    Begin
      If FindExpr.Expression.AsBoolean Then
      Begin
        Result := True;
        Break;
      End;
      Next;
    End;
  Finally
    If Not Result Then
      RecNo := SavedRecNo;
    FindExpr.Free;
    EnableControls;
  End;
End;

{ LAS: 15/JUN/2003 : 1.80 }
Procedure TCustomxQuery.QueryChanged(Sender: tobject);
var
  List: TParams;
Begin
  If Not (csReading In ComponentState) Then
  Begin
    Disconnect;
    If ParamCheck Or (csDesigning In ComponentState) Then
    begin
      List := TParams.Create(Self);
      try
        List.ParseSQL(SQL.Text, True);
        List.AssignValues(FParams);
        FParams.Clear;
        FParams.Assign(List);
      finally
        List.Free;
      end;
    end;
    DataEvent(dePropertyChange, 0);
  End Else
    FParams.ParseSQL(SQL.Text, False);
End;

Procedure TCustomxQuery.Disconnect;
Begin
  Close;
  UnPrepare;
End;

Procedure TCustomxQuery.Prepare;
Begin
  { future use }
End;

Procedure TCustomxQuery.UnPrepare;
Begin
  { future use }
End;

{$IFDEF level4}

Procedure TCustomxQuery.DefineProperties(Filer: TFiler);
Begin
  Inherited DefineProperties(Filer);
  Filer.Defineproperty('ParamData', ReadParamData, writeParamData, FParams.Count > 0);
End;

Procedure TCustomxQuery.ReadParamData(Reader: TReader);
Begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
End;

Procedure TCustomxQuery.WriteParamData(writer: Twriter);
Begin
  writer.writeCollection(FParams);
End;
{$ENDIF}

Function TCustomxQuery.GetParamsCount: Word;
Begin
  Result := FParams.Count;
End;

Function TCustomxQuery.ParamByName(Const Value: String): TParam;
Begin
  Result := FParams.ParamByName(Value);
End;

Procedure TCustomxQuery.SetParamsList(Value: TParams);
Begin
  FParams.AssignValues(Value);
End;

Procedure TCustomxQuery.SetParamsFromDataSet;
Var
  I: Integer;
  DataSet: TDataSet;
  Field: TField;
Begin
  If FDataLink.DataSource <> Nil Then
  Begin
    DataSet := FDataLink.DataSource.DataSet;
    If DataSet <> Nil Then
    Begin
      DataSet.FieldDefs.Update;
      For I := 0 To FParams.Count - 1 Do
        With FParams[I] Do
          If Not Bound Then
          Begin
            Field := DataSet.FindField(Name);
            If Field <> Nil Then
            Begin
              AssignField(Field);
              Bound := False;
            End;
          End;
    End;
  End;
End;

{ this method is called when the TxQuery is linked to a datasource }

Procedure TCustomxQuery.RefreshParams;
Var
  DataSet: TDataSet;
Begin
  DisableControls;
  Try
    If FDataLink.DataSource <> Nil Then
    Begin
      DataSet := FDataLink.DataSource.DataSet;
      If DataSet <> Nil Then
        If DataSet.Active And Not(DataSet.State In [dsInsert, dsSetKey]) Then
        Begin
          Close;
          Open;
        End;
    End;
  Finally
    EnableControls;
  End;
End;

Procedure TCustomxQuery.SetDataSource(Value: TDataSource);
Begin
  If IsLinkedTo(Value) Then
    Raise ExQueryError.Create(SCircularDataLink);
  FDataLink.DataSource := Value;
End;

Function TCustomxQuery.GetDataSource: TDataSource;
Begin
  Result := FDataLink.DataSource;
End;

Function TCustomxQuery.IsDataSetDisabled(DataSet: TDataSet): Boolean;
Begin
  Result := False;
  If FDisabledDataSets.Count = 0 Then
    Exit;
  Result := FDisabledDataSets.IndexOf(DataSet) >= 0;
End;

Procedure TCustomxQuery.FixDummiesForQuerying(Var Filter: String);
Var
  Ps: Integer;
  I: Integer;
Begin
  If Length(Filter) = 0 Then Exit;
  { this method called in the WHERE clause is a filter in
    order to fix some flags:
    - Only working flag now is the date in the format: 'DummyDate(32445.6566)'}
  Ps := AnsiPos('DummyDate(', Filter);
  While Ps > 0 Do
  Begin
    { by default, the date is left as it is but in descendant classes
      the date can be changed to meet the dataset filter implementation}
    For I := Ps + 10 To Length(Filter) Do
      If Filter[I] = ')' Then
      Begin
        System.Delete(Filter, I, 1);
        System.Delete(Filter, Ps, 10);
        Break;
      End;
    Ps := AnsiPos('DummyDate(', Filter);
  End;
End;

Procedure TCustomxQuery.FixDummiesForFilter(Var Filter: String);
Begin
  FixDummiesForQuerying(Filter); { fix the dates}
  Replacestring(Filter, 'DummyBoolean(True)', 'True');
  Replacestring(Filter, 'DummyBoolean(False)', 'False');
End;

Procedure TCustomxQuery.InternalRefresh;
Begin
  InternalClose;
  InternalOpen;
End;

{-------------------------------------------------------------------------------}
{ This procedure writes a TDataSet to a text file                               }
{-------------------------------------------------------------------------------}

Procedure TCustomxQuery.WriteToTextFile(Const FileName: String;
  FieldDelimChar, TxtSeparator: Char; IsCSV: Boolean;
  FieldNames: TStringList);
Const
  CSVquote = '"';

Var
  FTarget: TextFile;
  Idx: Integer;

  Function CSVquotedStr(Const Str: String): String;
  Var
    Idx: integer;
    Changed: boolean;
  Begin
    Changed := False;
    Result := Str;
    For Idx := Length(Result) Downto 1 Do
      If (Result[Idx] = CSVquote) Then
      Begin
        System.Insert(CSVquote, Result, Idx);
        Changed := true;
      End;

    If Changed Then
      Result := CSVquote + Result + CSVquote;
  End;

  Procedure WritefieldNames;
  Var
    Idx: integer;
    Fld: String;
    F: TField;
  Begin
    For Idx := 0 To FieldNames.Count - 1 Do
    Begin
      F := Self.FindField(FieldNames[idx]);
      If Not (F.DataType In ftNonTextTypes) Then
      Begin
        Fld := FieldNames[Idx];

        If (FieldDelimChar <> #0) Then
          Fld := FieldDelimChar + Fld + FieldDelimChar
        Else
        Begin
          If IsCSV Then
            Fld := CSVquotedStr(Fld);
        End;

        If Idx < FieldNames.Count - 1 Then
          System.Write(FTarget, Fld + TxtSeparator)
        Else
          System.Write(FTarget, Fld)
      End;
    End;

    System.WriteLn(FTarget);
  End;

  Procedure WriteField(FieldNum: integer);
  Var
    Data: String;
    F: TField;
  Begin
    F := Self.FindField(FieldNames[FieldNum]);
    If Not (F.DataType In ftNonTextTypes) Then
    Begin
      Data := Trim(F.Asstring);

      If FieldDelimChar <> #0 Then
        Data := FieldDelimChar + Data + FieldDelimChar
      Else
      Begin
        If IsCSV Then
          Data := CSVquotedStr(Data);
      End;

      If FieldNum < FieldNames.Count - 1 Then
        System.Write(FTarget, Data + TxtSeparator)
      Else
        System.Write(FTarget, Data);
    End;
  End;

  Procedure WriteRecords;
  Var
    Idx: integer;
  Begin
    Self.DisableControls;
    Self.First;
    While Not Self.Eof Do
    Begin
      For Idx := 0 To FieldNames.Count - 1 Do
        WriteField(Idx);
      System.WriteLn(FTarget);
      Self.Next;
    End;
    Self.EnableControls;
  End;

Begin
  AssignFile(FTarget, FileName);
  Rewrite(FTarget);

  If FieldNames.Count = 0 Then
  Begin
    { fill with all fields }
    Self.FieldDefs.Update;
    For Idx := 0 To Self.FieldCount - 1 Do
      If Not (Self.Fields[Idx].dataType In ftNonTextTypes) Then
        FieldNames.Add(Self.Fields[Idx].FieldName);
  End;
  WritefieldNames;
  Writerecords;

  CloseFile(FTarget);
End;

Procedure TCustomxQuery.PopulateDatasets(TableList: TTableList); { LAS : 05-30-2000 }
Begin
  // nothing to do here (this is for descendants only)
End;

Procedure TCustomxQuery.DisposeDatasets; { LAS : 05-30-2000 }
Begin
  // nothing to do here (this is for descendants only)
End;

Procedure TCustomxQuery.ExecSQLScript;
Var
  Analizer: TSqlAnalizer;
  P, ErrLine, ErrCol: Integer;
  ErrMsg, ErrTxt: String;
  AbortScript: boolean;
  s, ns: String;
  SaveState: Boolean;
  SaveCursor: TCursor;
Begin
  SaveCursor := crDefault;

  s := Trim(FSQLScript.Text);
  If Length(s) = 0 Then
    Raise ExQueryError.Create(SSQLIsEmpty);

{$IFDEF XQDEMO}
  If Not (csDesigning In ComponentState) And Not IsDelphiRunning Then
    Raise exception.Create(SDelphiIsNotRunning);
{$ENDIF}

  If FShowWaitCursor Then
  Begin
    SaveCursor := Screen.Cursor;
    Screen.Cursor := crSQLWait;
  End;
  SaveState := FShowWaitCursor;
  FShowWaitCursor := False;
  FScriptIsRunning := true;
  Try
    Repeat
      If Assigned(FResultSet) Then
        FreeObject(FResultSet);
      P := AnsiPos(';', s);
      If P > 0 Then
      Begin
        ns := Copy(s, 1, p);
        System.Delete(s, 1, P);
      End
      Else
        ns := Trim(s);
      If Length(ns) > 0 Then
      Begin
        FSQL.Text := ns;
        Analizer := TSqlAnalizer.Create(Nil, Self);
        Try
          If Analizer.parser.yyparse = 1 Then
          Begin
            ErrLine := Analizer.lexer.yylineno;
            ErrCol := Analizer.lexer.yycolno - Analizer.lexer.yyTextLen - 1;
            ErrMsg := Analizer.parser.yyerrormsg;
            Analizer.lexer.getyytext (ErrTxt);
            If Assigned(FOnSyntaxError) Then
            Begin
              FOnSyntaxError(Self, ErrMsg, ErrTxt, ErrLine, ErrCol,
                Length(ErrTxt));
              Abort;
            End
            Else
              { if not raised an error, will raise here }
              Raise ExQueryError.CreateFmt(SSyntaxErrorMsg, [ErrMsg, ErrLine,
                ErrCol, ErrTxt]);
          End;
          FScriptStatementType := Analizer.FStatement;
          Try
            Analizer.DoExecSQL;
          Except
            On E: Exception Do
              If Assigned(FOnScriptError) Then
              Begin
                AbortScript := False;
                FOnScriptError(Self, E, AbortScript);
                If AbortScript Then
                  exit;
              End
              Else
                Raise;
          End;
        Finally
          FreeObject(Analizer);
        End;
      End
      Else
        Break;
    Until False;
  Finally
    FScriptIsRunning := False;
    FShowWaitCursor := SaveState;
    If FShowWaitCursor Then
    Begin
      Screen.Cursor := SaveCursor;
    End;
  End;
End;

Procedure TCustomxQuery.PackTable(TableList: TTableList);
Begin
  // nothing to do here
End;

Procedure TCustomxQuery.ZapTable(TableList: TTableList);
Begin
  // nothing to do here
End;

Procedure TCustomxQuery.ReindexTable(TableList: TTableList);
Begin
  // nothing to do here
End;

Function TCustomxQuery.LocateRecord(Const KeyFields: String; Const KeyValues: Variant;
  Options: TLocateOptions): Integer;
Var
  //fieldlist: TStringList;
  ss, cs, ts: String;
  sd, cd: double;
  si, ci: integer;
  sb, cb: boolean;
  xqField: TxqField;
  I, J, rnum: integer;
  Fields: TList;
  FieldCount, MatchCount: Integer;
  IsEqual, Found: Boolean;
Begin
  CheckActive;
  CheckBrowseMode;
  IsEqual := False;
  Found := False;
  Result := 0;
  rnum := 0;
  Fields := TList.Create;
  Try
    GetFieldList(Fields, KeyFields);
    FieldCount := Fields.Count;
    For I := 1 To FResultSet.RecordCount Do
    Begin
      rnum := I;
      FResultSet.Recno := rnum;
      MatchCount := 0;
      For J := 0 To FieldCount - 1 Do
      Begin
        xqField := FResultSet.Fields[TField(Fields[J]).FieldNo - 1];
        Case xqField.DataType Of
          ttString:
            Begin
              ss := xqField.AsString;
              If FieldCount = 1 Then
                cs := KeyValues
              Else
                cs := KeyValues[J];
              If loPartialKey In Options Then
                ts := Copy(ss, 1, Length(cs))
              Else
                ts := ss;
              If loCaseInsensitive In Options Then
                IsEqual := AnsiCompareText(ts, cs) = 0
              Else
                IsEqual := AnsiCompareStr(ts, cs) = 0;
            End;
          ttFloat:
            Begin
              sd := xqField.AsFloat;
              If FieldCount = 1 Then
                cd := KeyValues
              Else
                cd := KeyValues[J];
              IsEqual := sd = cd;
            End;
          ttInteger:
            Begin
              si := xqField.AsInteger;
              If FieldCount = 1 Then
                ci := KeyValues
              Else
                ci := KeyValues[J];
              IsEqual := si = ci;
            End;
          ttBoolean:
            Begin
              sb := xqField.AsBoolean;
              If FieldCount = 1 Then
                cb := KeyValues
              Else
                cb := KeyValues[J];
              IsEqual := sb = cb;
            End;
        End;
        If IsEqual Then
          Inc(MatchCount);
      End;
      Found := MatchCount = FieldCount;
      If Found Then
        Break;
    End;
    If Found Then
    Begin
      Result := rnum;
    End;
  Finally
    Fields.Free;
  End;
End;

Function TCustomxQuery.Locate(Const KeyFields: String;
  Const KeyValues: Variant; Options: TLocateOptions): Boolean;
Var
  TmpRecno: Integer;
Begin
  DoBeforeScroll;
  TmpRecno := LocateRecord(KeyFields, KeyValues, Options);
  Result := TmpRecno > 0;
  If Result Then
  Begin
    Recno := TmpRecno;
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  End;
End;

Function TCustomxQuery.Lookup(Const KeyFields: String;
  Const KeyValues: Variant; Const ResultFields: String): Variant;
Var
  SaveRecno, TmpRecno: Integer;
Begin
  SaveRecno := Self.Recno;
  //DoBeforeScroll;
  TmpRecno := LocateRecord(KeyFields, KeyValues, []);
  If TmpRecno > 0 Then
  Begin
    Recno := TmpRecno;
    Resync([rmExact, rmCenter]);
    //DoAfterScroll;
    Result := Null;
    SetTempState(dsCalcFields);
    Try
      CalculateFields(TempBuffer);
      FResultSet.Recno := TmpRecno;
      Result := FieldValues[ResultFields];
    Finally
      RestoreState(dsBrowse);
    End;
    Recno := SaveRecno;
    Resync([rmExact, rmCenter]);
  End;
End;

Procedure TCustomxQuery.InternalEdit; // editing
Begin
  Inherited InternalEdit;
  If GetActiveRecordBuffer <> Nil Then
    InternalSetToRecord(GetActiveRecordBuffer);
End;

Procedure TCustomxQuery.SetFieldData(Field: TField; Buffer: Pointer); // editing
Var
  Offset: Integer;
  RecBuffer: TRecordBuffer;
  TempDouble: Double;
  Data: TDateTimeRec;
  ts: TTimeStamp;
  TempBool: WordBool;
  xqField: TxqField;
Begin
  If Not Active Then Exit;
  RecBuffer := GetActiveRecordBuffer;
  If (RecBuffer = Nil) Or (Buffer = Nil) Then Exit;
  If (Field.FieldKind = fkCalculated) Or (Field.FieldKind = fkLookup) Then
  Begin
    Inc(RecBuffer, StartCalculated + Field.Offset);
    Boolean(RecBuffer[0]) := (Buffer <> Nil);
    //if Boolean(RecBuffer[0]) then
    CopyMemory(@RecBuffer[1], Buffer, Field.DataSize);
  End
  Else
  Begin
    xqField := FResultSet.Fields[Field.FieldNo - 1];
    FResultSet.Recno := RecNo;  //FRecNo + 1;
    Offset := xqField.FFieldOffset;
    Case Field.DataType Of
      ftInteger:
        Begin
          Move(Integer(Buffer^), (RecBuffer + Offset)^, sizeof(Integer));
          FResultSet.Fields[Field.FieldNo - 1].AsInteger := Integer(Buffer^);
        End;
      ftBoolean:
        Begin
          Move(WordBool(Buffer^), TempBool, sizeof(WordBool));
          Move(TempBool, (RecBuffer + Offset)^, sizeof(WordBool));
          FResultSet.Fields[Field.FieldNo - 1].AsBoolean := WordBool(Buffer^);
        End;
      ftString:
        Begin
          StrLCopy(PChar(RecBuffer + Offset), Buffer, StrLen(PChar(Buffer)));
          FResultSet.Fields[Field.FieldNo - 1].AsString := PChar(Buffer);
        End;
      ftDate, ftTime:
        Begin
          if Field.DataType = ftDate then
          Begin
            ts.Time:=0;
            ts.Date:=Integer(Buffer^);
          End Else
          Begin
            ts.Time:=Integer(Buffer^);
            ts.Date:=Trunc(Now);
          End;
          TempDouble:= TimeStampToDateTime(ts);
          Move(TempDouble, (RecBuffer + Offset)^, Sizeof(Double));
          FResultSet.Fields[Field.FieldNo - 1].AsFloat := TempDouble;
        End;
      ftDateTime:
        Begin
          Data := TDateTimeRec(Buffer^);
          ts := MSecsToTimeStamp(Data.DateTime);
          TempDouble := TimeStampToDateTime(ts);
          Move(TempDouble, (RecBuffer + Offset)^, sizeof(TempDouble));
          FResultSet.Fields[Field.FieldNo - 1].AsFloat := TempDouble;
        End;
      ftFloat, ftCurrency:
        Begin
          Move(Double(Buffer^), (RecBuffer + Offset)^, sizeof(Double));
          FResultSet.Fields[Field.FieldNo - 1].AsFloat := Double(Buffer^);
        End;
    End;
  End;
  If Not (State In [dsCalcFields, dsFilter, dsNewValue]) Then
    DataEvent(deFieldChange, Longint(Field));
End;

Procedure TCustomxQuery.SortByColumns(Const Columns: Array Of integer;
  Descending: Boolean);
Var
  I, J, ColIndex: Integer;
  SortList: TxqSortList;

  Function LocalCreateSortList: TxqSortList;
  Begin
    If FInMemResultSet Then
      Result := TMemSortList.Create(False)
    Else
      Result := TFileSortList.Create(False, FMapFileSize);
  End;

Begin
  CheckActive;
  SortList := LocalCreateSortList;
  Try
    For I := Low(Columns) To High(Columns) Do
    Begin
      ColIndex := Columns[I];
      If (ColIndex < 0) Or (ColIndex > ResultSet.Fields.Count - 1) Then
        Continue;
      SortList.AddField(ResultSet.Fields[ColIndex].DataType,
        ResultSet.Fields[ColIndex].ColWidth, Descending);
    End;
    For I := 1 To ResultSet.RecordCount Do
    Begin
      ResultSet.RecNo := I;
      SortList.Insert;
      SortList.SourceRecNo := I;
      For J := Low(Columns) To High(Columns) Do
      Begin
        ColIndex := Columns[J];
        Case ResultSet.Fields[ColIndex].DataType Of
          ttstring:
            SortList.Fields[J].Asstring := ResultSet.Fields[ColIndex].Asstring;
          ttFloat:
            SortList.Fields[J].AsFloat := ResultSet.Fields[ColIndex].AsFloat;
          ttInteger:
            SortList.Fields[J].AsInteger := ResultSet.Fields[ColIndex].AsInteger;
          ttBoolean:
            SortList.Fields[J].AsBoolean := ResultSet.Fields[ColIndex].AsBoolean;
        End;
      End;
    End;
    SortList.Sort;
    { now, sort the Result set }
    ResultSet.SortWithList(SortList);
    { now, position to the first record }
    First;
  Finally
    SortList.Free;
  End;
End;

Procedure TCustomxQuery.SetParamsAsFields(Value: TParamsAsFields);
Begin
  FParamsAsFields.Assign(Value);
End;

Procedure TCustomxQuery.SetDataSets(Value: TxDatasets);
Begin
  FDataSets.Assign(Value);
End;

End.
