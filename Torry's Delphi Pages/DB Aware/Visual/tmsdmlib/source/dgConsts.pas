{***************************************************************************}
{ TMS Data Modeler Library for Delphi & C++Builder                          }
{ version 1.7                                                               }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 1997 - 2010                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such.                             }
{***************************************************************************}

unit dgConsts;

interface

type
  {TIndexType contains the type of the index:
   itNone: index is not unique
   itUnique: index is unique (exclusive)}
  TIndexType = (itNone, itUnique);

  {TIndexOrder specifies the order of index, ascending or descending}
  TIndexOrder = (ioAscending, ioDescending);

  {TIndexFieldOrder specifies the order of field in index, ascending or descending}
  TIndexFieldOrder = (ioAsc, ioDesc);

  {TUpdateMethod specifies the update method of the relationship
   umNone: not used
   umRestrict: parent cannot be updated if there are childs
   umCascade: when parent is updated, childs are updated
   umSetNull: when parent is updated, childs are set to null
   umSetDefault: when parent is updated, childs are set to default value}
  TUpdateMethod = (umNone, umRestrict, umCascade, umSetNull, umSetDefault);

  {TDeleteMethod specifies the delete method of the relationship
   dmNone: not used
   dmRestrict: parent cannot be deleted if there are childs
   dmCascade: when parent is deleted, childs are deleted
   dmSetNull: when parent is deleted, childs are set to null
   dmSetDefault: when parent is deleted, childs are set to default value}
  TDeleteMethod = (dmNone, dmRestrict, dmCascade, dmSetNull, dmSetDefault);

  {Specifies the type of the relationship.
  ryUndefined: not used
  ryIdentifying: child fields belong to primary key of child table
  ryNonIdentifying: child fields does not necessarily belongs to primary key of child table}
  TGDAORelationshipType = (ryUndefined, ryIdentifying, ryNonIdentifying);

  {Specifies the "native" data type of the TGDAODataType object. The "native" data type
  is just a generic type for the physical data type. It can be used to retrieve a normalized
  information about the type of TGDAODataType.}
  TNativeDataType = (naUnknown, naInteger, naFloat, naString, naBoolean,
                     naDateTime, naMemo, naBlob, naComputed);

  {Specifies the "native" sub data type of the TGDAODataType object. The "native" sub data type
  is just a generic sub type for the physical data type. It can be used to retrieve a normalized
  information about the type/sub type of TGDAODataType.}
  TNativeSubType  = (    stUnknown,
                      // naInteger
                         stInteger, stLongInt, stCounter, stSmallInt, stTinyInt, stLongCounter,
                         stSmallCounter, stTinyCounter, stTinyWord, stSmallWord, stWord, stMediumInt,
                         stMediumCounter, 
                      // naString
                         stString, stChar, stNChar, stNVarChar, stSysName, stRowID, stURowID,
                         stTinyText, stCIChar,
                      // naBoolean
                         stBoolean, stBit,
                      // naDateTime
                         stDateTime, stSmallDateTime, stTime, stDate, stYear, stLongDateTime,
                      // naMemo
                         stMemo, stNText, stMediumText, stLongText,
                      // naBlob
                         stBlob, stBinary, stImage, stVariant,stTimeStamp, stVarBinary, stGUID,
                         stBFile, stClob, stNClob, stRaw, stLongRaw, stLongBlob, stMediumBlob,
                         stSerial, stEnum, stSet, stTinyBlob, stXML,
                      // naFloat
                         stFloat, stDecimal, stNumericXY, stMoney, stReal, stSmallMoney,
                         stFloatCounter, stSingle, stDouble, stExtended, stFloatX,
                         stIntervalYear, stIntervalYearToMonth,
                         stIntervalMonth,
                         stIntervalDay, stIntervalDayToHour, stIntervalDayToMinute, stIntervalDayToSecond, stIntervalDayToMSecond,
                         stIntervalHour, stIntervalHourToMinute, stIntervalHourToSecond, stIntervalHourToMSecond,
                         stIntervalMinute, stIntervalMinuteToSecond, stIntervalMinuteToMSecond,
                         stIntervalSecond, stIntervalSecondToMSecond,
                         stIntervalMSecond,
                         stTimeStampLocalZone, stTimeStampTimeZone,
                      // naComputed
                         stComputed
  );

  {Specifies restrictions over fields (TGDAOField.Restriction property) when project is open for editing:
   frNone: field is fully editable
   frPartialReadOnly: some field properties are editable
   frReadOnly: field is read only}
  TFieldRestriction  = (frNone, frPartialReadOnly, frReadOnly);

  {Specifies restrictions over objects (TGDAOObject.Restriction property) when project is open for editing:
   orNone: object is visible and editable
   orReadOnly: object is visible and read only
   orHidden: object is not visible}
  TObjectRestriction = (orNone, orReadOnly, orHidden);

  {Specifies restrictions over tables (TGDAOTable.Restriction property) when project is open for editing:
   trNone: table is visible and editable
   trReadOnly: table is visible and read only
   trHidden: table is not visible}
  TTableRestriction  = (trNone, trReadOnly, trHidden);

  {Specifies the type of TGDAOCategory object.
  ctNone: not used
  ctProcedure: Stored Procedures
  ctView: Views
  ctSequence: Sequences/Generators
  ctFunction: Functions}
  TGDAOCategoryType = (ctNone, ctProcedure, ctView, ctSequence, ctFunction);

  {Specifies the database vendor type to which the database dictionary model is targeted to.
  fdbUnknown: Not used
  fdbSqlServer2000: Microsoft SQL Server 2000
  fdbSqlServer2005: Microsoft SQL Server 2005
  fdbFirebird2: Firebird 2.x
  fdbAbsoluteDB: Absolute Database from ComponentAce
  fdbSqlServer2008: Microsoft SQL Server 2008
  fdbNexusDB3: NexusDB V3
  fdbOracle10g: Oracle 10g
  fdbMySQL51: MySQL 5.1
  fdbSqlAzure: Microsoft SQL Azure
  fdbElevateDB: ElevateDB}
  TFixedDBType =
    (fdbUnknown,
     fdbSqlServer2000,
     fdbSqlServer2005,
     fdbFirebird2,
     fdbAbsoluteDB,
     fdbSqlServer2008,
     fdbNexusDB3,
     fdbOracle10g,
     fdbMySQL51,
     fdbSqlAzure,
     fdbElevateDB);

const
  {contains the unique string id for the database type. This string id is saved in the
   Project file (.dgp) to identify the database type}
  FixedDBTypeID: array[TFixedDBType] of string =
    ('none',
     'mssql2000',
     'mssql2005',
     'firebird2',
     'absolutedb',
     'mssql2008',
     'nexusdb3',
     'oracle10g',
     'mysql51',
     'mssqlazure',
     'elevatedb');

implementation

end.

