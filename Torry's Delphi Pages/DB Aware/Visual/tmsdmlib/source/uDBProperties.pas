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

unit uDBProperties;

interface

uses
  Classes, SysUtils, uGDAO, dgConsts;

type
  {TDBProperties is a class that holds some useful class methods. You do not need
   to instantiate an object of this class} 
  TDBProperties = class
  private
    class procedure AllocObjectCategories(ADatabase: TGDAODatabase);
  public
    {FillDataTypesObject method fills the ADataTypes collection with all the data types
     related to the database type specified by ADatabaseID. }
    class procedure FillDataTypesObject(ADatabaseID: string; ADataTypes: TGDAODataTypes);
    {GetFixedDatabaseType returns a TFixedDBType value which corresponds to the database ID
    specified by ADatabaseID}
    class function GetFixedDatabaseType(ADatabaseID: string): TFixedDBType;
    {LoadAll method initialize the TGDAODatabase specified by ADatabase.}
    class procedure LoadAll(ADatabase: TGDAODatabase);
    {CopyDictionary performs a copy of all information held by ASource to a TGDAODatabase object
    specified by ATarget}
    class procedure CopyDictionary(ASource, ATarget: TGDAODatabase);
  end;

implementation

uses
  Contnrs;

{ TDBProperties }

class procedure TDBProperties.AllocObjectCategories(ADatabase: TGDAODatabase);
var
  ValidCats: TObjectList;

  function AddNewCategory(AType: TGDAOCategoryType; ANameS, ANameP: String; ACreate, ADrop: String): TGDAOCategory;
  var
    ACat: TGDAOCategory;
  begin
    ACat := ADatabase.Categories.FindByType(AType);
    if ACat = nil then
      ACat := ADatabase.Categories.Add(AType, '', '', '', '');

    ACat.CategoryNameS := ANameS;
    ACat.CategoryNameP := ANameP;
    ACat.CreateTemplate := ACreate;
    ACat.DropTemplate := ADrop;
    ValidCats.Add(ACat);

    result := ACat;
  end;

var
  c: integer;
begin
  with ADatabase do
  begin
    ValidCats := TObjectList.Create(false);
    try
      case GetFixedDatabaseType(DatabaseTypeID) of
        fdbSqlServer2000, fdbSqlServer2005, fdbSqlServer2008, fdbSqlAzure:
          begin
            AddNewCategory(ctProcedure,
              'procedure',
              'Procedures',
              '',
              '');
            AddNewCategory(ctView,
              'view',
              'Views',
              '',
              '');
          end;
        fdbFirebird2:
          begin
            AddNewCategory(ctProcedure,
              'procedure',
              'Procedures',
              '',
              '');
            AddNewCategory(ctView,
              'view',
              'Views',
              '',
              '');
            with AddNewCategory(ctSequence,
              'generator',
              'Generators',
              '', '') do
            begin
              PropDefs.Clear;
              PropDefs.Add(SProp_SequenceSeed, pdtInteger, 0);
            end;
          end;
        fdbAbsoluteDB:
          begin
            //No categories for AbsoluteDB
          end;
        fdbNexusDB3:
          begin
            AddNewCategory(ctProcedure,
              'procedure',
              'Procedures',
              '',
              '');
            AddNewCategory(ctView,
              'view',
              'Views',
              '',
              '');
          end;
        fdbOracle10g:
          begin
            AddNewCategory(ctProcedure,
              'procedure',
              'Procedures',
              '',
              '');
            AddNewCategory(ctFunction,
              'function',
              'Functions',
              '',
              '');
            with AddNewCategory(ctSequence,
              'sequence',
              'Sequences',
              '',
              '') do
            begin
              PropDefs.Clear;
              PropDefs.Add(SProp_SequenceSeed, pdtInteger, 0);
            end;
            AddNewCategory(ctView,
              'view',
              'Views',
              '',
              '');
          end;
        fdbMySQL51:
          begin
            AddNewCategory(ctProcedure,
              'procedure',
              'Procedures',
              '',
              '');
            AddNewCategory(ctFunction,
              'function',
              'Functions',
              '',
              '');
            AddNewCategory(ctView,
              'view',
              'Views',
              '',
              '');
          end;
        fdbElevateDB:
          begin
            AddNewCategory(ctProcedure,
              'procedure',
              'Procedures',
              '',
              '');
            AddNewCategory(ctFunction,
              'function',
              'Functions',
              '',
              '');
            AddNewCategory(ctView,
              'view',
              'Views',
              '',
              '');
          end;
      end;

      {Clear invalid categories}
      c := 0;
      while c < Categories.Count do
        if ValidCats.IndexOf(Categories[c]) = -1 then
          Categories[c].Free
        else
          inc(c);
    finally
       ValidCats.Free;
    end;
  end;
end;

class procedure TDBProperties.CopyDictionary(ASource, ATarget: TGDAODatabase);
begin
  ATarget.Assign(ASource);
  LoadAll(ATarget);
end;

class procedure TDBProperties.FillDataTypesObject(ADatabaseID: string; ADataTypes: TGDAODataTypes);
begin
  with ADataTypes do
  begin
    Clear;
    case GetFixedDatabaseType(ADatabaseID) of
      fdbFirebird2:
        begin
          Add('Bigint', 'BIGINT', false, false, naInteger, stLongInt);
          Add('Blob', 'BLOB SUB_TYPE %p% SEGMENT SIZE %s%', true, true, naBlob, stBlob ).SetSizeSettings(80, 0, true, 1, 32767);
          Add('Blob text', 'BLOB SUB_TYPE TEXT SEGMENT SIZE %s%', true, false, naMemo, stMemo ).SetSizeSettings(80, 0, true, 1, 32767);
          Add('Char', 'CHAR(%s%)', true, false, naString, stChar).SetSizeSettings(10, 0, true, 1, 32765);
          Add('Computed', '', false, false, naComputed, stComputed).Computed := true;
          Add('Date', 'DATE', false, false, naDateTime, stDateTime );
          Add('Decimal', 'DECIMAL(%s%,%p%)', true, true, naFloat, stDecimal).SetSizeSettings(10, 3, true, 0, 18);
          Add('Double precision', 'DOUBLE PRECISION', false, false, naFloat, stDouble );
          Add('Float', 'FLOAT', false,  false,  naFloat, stFloat );
          Add('Integer', 'INTEGER', false, false, naInteger, stInteger);
          Add('Numeric', 'NUMERIC(%s%,%p%)', true, true, naFloat, stNumericXY ).SetSizeSettings(10, 3, true, 0, 18);;
          Add('Smallint', 'SMALLINT', false, false,  naInteger, stSmallInt );
          Add('Time', 'TIME', false, false, naDateTime, stTime );
          Add('Timestamp', 'TIMESTAMP', false, false, naDateTime, stTimeStamp );
          Add('Varchar', 'VARCHAR(%s%)', true, false, naString, stString ).SetSizeSettings(50, 0, true, 1, 32765);

          Add('NChar', 'NCHAR(%s%)', true, false, naString, stNChar ).SetSizeSettings(10, 0, true, 1, 32765);
          Add('NChar Varying', 'NCHAR VARYING(%s%)', true, false, naString, stNVarChar ).SetSizeSettings(50, 0, true, 1, 32765);

          {Add('Int64', 'INT64', False, False, naInteger, stInt64);}
        end;
      fdbSqlServer2000, fdbSqlServer2005, fdbSqlServer2008, fdbSqlAzure:
        begin
          Add('Bigint', 'BIGINT', false, false, naInteger, stLongint );
          with Add('Bigint (identity)', 'BIGINT IDENTITY(%e%,%i%)', false, false, naInteger, stLongCounter, true, true, true) do
            ForeignDataTypeName := 'Bigint';

          Add('Binary', 'BINARY(%s%)', true, false, naBlob, stBinary).SetSizeSettings(50, 0, true, 1, 8000);
          Add('Bit', 'BIT', false, false, naBoolean, stBoolean);
          Add('Char', 'CHAR(%s%)', true, false, naString, stChar).SetSizeSettings(10, 0, true, 1, 8000);
          Add('Computed', '', false, false, naComputed, stComputed).Computed := true;
          Add('Datetime', 'DATETIME', false, false, naDateTime, stDateTime);
          Add('Decimal', 'DECIMAL(%s%,%p%)', true, true, naFloat, stDecimal).SetSizeSettings(18, 0, true, 1, 38);
          with Add('Decimal (identity)', 'DECIMAL(%s%,%p%) IDENTITY(%e%,%i%)', true, true, naFloat, stCounter, true, true, true) do
          begin
            ForeignDataTypeName := 'Decimal';
            SetSizeSettings(18, 0, true, 1, 38)
          end;

          //Add('Empty', '', false, false, naUnknown, stUnknown );
          Add('Float', 'FLOAT', false, false, naFloat, stFloat);
          Add('Image', 'IMAGE', false, false, naBlob, stBlob);
          Add('Int', 'INTEGER', false, false, naInteger, stInteger);
          with Add('Int (identity)', 'INTEGER IDENTITY(%e%,%i%)', false, false, naInteger, stCounter, true, true, true) do
            ForeignDataTypeName := 'Int';
          Add('Money', 'MONEY', false, false, naFloat, stMoney);
          Add('NChar', 'NCHAR(%s%)', true, false, naString, stNChar).SetSizeSettings(10, 0, true, 1, 4000);
          Add('NText', 'NTEXT', false, false, naMemo, stNText);
          Add('Numeric', 'NUMERIC(%s%,%p%)', true, true, naFloat, stNumericXY).SetSizeSettings(18, 0, true, 1, 38);
          with Add('Numeric (identity)', 'NUMERIC(%s%,%p%) IDENTITY(%e%,%i%)', true, true, naFloat, stFloatCounter, true, true, true ) do
          begin
            ForeignDataTypeName := 'Numeric';
            SetSizeSettings(18, 0, true, 1, 38)
          end;

          Add('NVarChar', 'NVARCHAR(%s%)', true, false, naString, stNVarChar).SetSizeSettings(50, 0, true, 1, 4000);
          Add('Real', 'REAL', false, false, naFloat, stReal);
          Add('SmallDateTime', 'SMALLDATETIME', false, false, naDateTime, stSmallDateTime);
          Add('SmallInt', 'SMALLINT', false, false, naInteger, stSmallInt);
          with Add('SmallInt (identity)', 'SMALLINT IDENTITY(%e%,%i%)', false, false, naInteger, stSmallCounter, true, true, true) do
            ForeignDataTypeName := 'SmallInt';

          Add('SmallMoney', 'SMALLMONEY', false, false, naFloat, stSmallMoney);
          Add('Sql_Variant', 'SQL_VARIANT', false, false, naBlob, stVariant );
          Add('SysName', 'sysname', false, false, naString, stSysName );
          Add('Text', 'TEXT' , false, false, naMemo, stMemo);
          Add('TimeStamp', 'TIMESTAMP', false, false, naDateTime, stTimeStamp);
          Add('TinyInt', 'TINYINT', false, false, naInteger, stTinyInt);
          with Add('TinyInt (identity)', 'TINYINT IDENTITY(%e%,%i%)', false, false, naInteger, stTinyCounter, true, true, true) do
            ForeignDataTypeName := 'TinyInt';

          Add('UniqueIdentifier', 'UNIQUEIDENTIFIER', false, false, naBlob, stGUID);
          Add('VarBinary', 'VARBINARY(%s%)', true, false, naBlob, stVarBinary).SetSizeSettings(50, 0, true, 1, 8000);
          Add('VarChar', 'VARCHAR(%s%)', true, false, naString, stString).SetSizeSettings(50, 0, true, 1, 8000);
          if GetFixedDatabaseType(ADatabaseID) in [fdbSqlServer2005, fdbSqlServer2008, fdbSqlAzure] then
          begin
            Add('NVarChar(MAX)', 'NVARCHAR(MAX)', false, false, naMemo, stNText);
            Add('VarBinary(MAX)', 'VARBINARY(MAX)', false, false, naBlob, stVarBinary);
            Add('VarChar(MAX)', 'VARCHAR(MAX)', false, false, naString, stString);
            Add('XML', 'XML', False, False, naBlob, stXML);
          end;

          if GetFixedDatabaseType(ADatabaseID) in [fdbSqlServer2008, fdbSqlAzure] then
          begin
            Add('Date', 'DATE', false, false, naDateTime, stDate);
            Add('Datetime2', 'DATETIME2(%s%)', true, false, naDateTime, stLongDateTime).SetSizeSettings(7, 0, true, 0, 7);
            Add('Datetimeoffset', 'DATETIMEOFFSET(%s%)', true, false, naDateTime, stLongDateTime).SetSizeSettings(7, 0, true, 0, 7);
            Add('Geography', 'GEOGRAPHY', false, false, naBlob, stRaw);
            Add('Geometry', 'GEOMETRY', false, false, naBlob, stRaw);
            Add('Hierarchyid', 'HIERARCHYID', false, false, naBlob, stRaw);
            Add('Time', 'TIME(%s%)', true, false, naDateTime, stTime).SetSizeSettings(7, 0, true, 0, 7);
          end;
        end;
      fdbAbsoluteDB:
        begin
          Add('Autoinc', 'AUTOINC(INTEGER, INITIALVALUE %e%, INCREMENT %i%)', false, false, naInteger, stCounter, true, true, true);
          Add('AutoincShortInt', 'AUTOINC(SHORTINT, INITIALVALUE %e%, INCREMENT %i%)', false, false, naInteger, stSmallCounter, true, true, true);
          Add('AutoincSmallInt', 'AUTOINC(SMALLINT, INITIALVALUE %e%, INCREMENT %i%)', false, false, naInteger, stSmallCounter, true, true, true);
          Add('AutoincInteger', 'AUTOINC(INTEGER, INITIALVALUE %e%, INCREMENT %i%)', false, false, naInteger, stCounter, true, true, true);
          Add('AutoincLargeInt', 'AUTOINC(LARGEINT, INITIALVALUE %e%, INCREMENT %i%)', false, false, naInteger, stLongCounter, true, true, true);
          Add('AutoincByte', 'AUTOINC(BYTE, INITIALVALUE %e%, INCREMENT %i%)', false, false, naInteger, stTinyCounter, true, true, true);
          Add('AutoincWord', 'AUTOINC(WORD, INITIALVALUE %e%, INCREMENT %i%)', false, false, naInteger, stSmallCounter, true, true, true);
          Add('AutoincCardinal', 'AUTOINC(CARDINAL, INITIALVALUE %e%, INCREMENT %i%)', false, false, naInteger, stSmallCounter, true, true, true);
          Add('Blob', 'BLOB', false, false, naBlob, stBlob);
          Add('Byte', 'BYTE', false, false, naInteger, stTinyWord);
          Add('Bytes', 'BYTES(%s%)', true, false, naBlob, stVarBinary).SetSizeSettings(50, 0, true, 1, 65500);
          Add('Cardinal', 'CARDINAL', false, false, naInteger, stSmallInt);
          Add('Char', 'CHAR(%s%)', true, false, naString, stChar).SetSizeSettings(50, 0, true, 1, 65500);
          Add('Currency', 'CURRENCY', false, false, naFloat, stMoney);
          Add('Date', 'DATE', false, false, naDateTime, stDate);
          Add('Datetime', 'DATETIME', false, false, naDateTime, stDateTime);
          Add('Extended', 'EXTENDED', false, false, naFloat, stExtended);
          Add('Float', 'FLOAT', false, false, naFloat, stFloat);
          Add('FormattedMemo', 'FORMATTEDMEMO' , false, false, naMemo, stMemo);
          Add('Graphic', 'GRAPHIC' , false, false, naBlob, stBlob);
          Add('Guid', 'GUID' , false, false, naBlob, stGuid);
          Add('Integer', 'INTEGER', false, false, naInteger, stInteger);
          Add('LargeInt', 'LARGEINT', false, false, naInteger, stLongInt);
          Add('Logical', 'LOGICAL', false, false, naBoolean, stBoolean);
          Add('Memo', 'MEMO' , false, false, naMemo, stMemo);
          Add('Single', 'SINGLE', false, false, naFloat, stSingle);
          Add('SmallInt', 'SMALLINT', false, false, naInteger, stSmallInt);
          Add('String', 'STRING(%s%)', true, false, naString, stString).SetSizeSettings(50, 0, true, 1, 65500);
          Add('Time', 'TIME', false, false, naDateTime, stTime);
          Add('TimeStamp', 'TIMESTAMP', false, false, naDateTime, stTimeStamp);
          Add('Varbytes', 'VARBYTES', false, false, naBlob, stBlob);
          Add('WideMemo', 'WIDEMEMO', false, false, naMemo, stNText);
          Add('WideChar', 'WIDECHAR(%s%)', true, false, naString, stNChar).SetSizeSettings(50, 0, true, 1, 65500);
          Add('WideString', 'WIDESTRING(%s%)', true, false, naString, stNVarChar).SetSizeSettings(50, 0, true, 1, 65500);
          Add('Word', 'WORD', false, false, naInteger, stSmallWord);
        end;
      fdbNexusDB3:
        begin
          Add('Autoinc', 'AUTOINC(%e%, %i%)', false, false, naInteger, stCounter, true, true, true);
          Add('Bigint', 'BIGINT', false, false, naInteger, stLongInt);
          Add('Blob', 'BLOB', false, false, naBlob, stBlob);
          Add('Boolean', 'BOOLEAN', false, false, naBoolean, stBoolean);
          Add('Byte', 'BYTE', false, false, naInteger, stTinyWord);
          Add('Bytearray', 'BYTEARRAY(%s%)', true, false, naBlob, stVarBinary).SetSizeSettings(50, 0, true, 1, 65536);
          Add('Clob', 'CLOB' , false, false, naMemo, stMemo);
          Add('Date', 'DATE', false, false, naDateTime, stDate);
          Add('Dword', 'DWORD', false, false, naInteger, stWord);
          Add('Extended', 'EXTENDED', false, false, naFloat, stExtended);
          Add('Float', 'FLOAT(%s%)', true, false, naFloat, stFloat).SetSizeSettings(18, 0, true, 0, 18);
          Add('Guid', 'GUID' , false, false, naBlob, stGuid);
          Add('Image', 'IMAGE', false, false, naBlob, stImage);
          Add('Integer', 'INTEGER', false, false, naInteger, stInteger);
          Add('Money', 'MONEY', false, false, naFloat, stMoney);
          Add('Nclob', 'NCLOB', false, false, naMemo, stNText);
          Add('Nsinglechar', 'NSINGLECHAR', false, false, naString, stNChar);
          Add('Numeric', 'NUMERIC(%s%,%p%)', true, true, naFloat, stNumericXY).SetSizeSettings(20, 4, true, 1, 38);
          Add('Nvarchar', 'NVARCHAR(%s%)', true, false, naString, stNVarChar).SetSizeSettings(50, 0, true, 1, 32767);
          Add('Real', 'REAL', false, false, naFloat, stDouble);
          Add('Recrev', 'RECREV' , false, false, naBlob, stRaw);
          Add('Shortint', 'SHORTINT', false, false, naInteger, stTinyInt);
          Add('Shortstring', 'SHORTSTRING(%s%)', true, false, naString, stTinyText).SetSizeSettings(50, 0, true, 1, 255);
          Add('Singlechar', 'SINGLECHAR', false, false, naString, stChar);
          Add('Smallint', 'SMALLINT', false, false, naInteger, stSmallInt);
          Add('Time', 'TIME', false, false, naDateTime, stTime);
          Add('Timestamp', 'TIMESTAMP', false, false, naDateTime, stTimeStamp);
          Add('Varchar', 'VARCHAR(%s%)', true, false, naString, stString).SetSizeSettings(50, 0, true, 1, 8192);
          Add('Word', 'WORD', false, false, naInteger, stSmallWord);
        end;
      fdbOracle10g:
        begin
          // Oracle built-in datatypes
          Add('Bfile', 'BFILE', false, false, naBlob, stBFile);
          Add('Binary_Double', 'BINARY_DOUBLE', False, False, naFloat, stDouble);
          Add('Binary_Float', 'BINARY_FLOAT', False, False, naFloat, stFloat);
          Add('Blob', 'BLOB', false, false, naBlob, stBlob);
          Add('Char', 'CHAR(%s%)', true, false, naString, stChar);
          Add('Clob', 'CLOB', false, false, naMemo, stMemo);
          Add('Date', 'DATE', false, false, naDateTime, stDateTime);
          Add('Interval day to second', 'INTERVAL DAY (%s%) TO SECOND (%p%)', true, true, naFloat, stIntervalDay);
          Add('Interval year to month', 'INTERVAL YEAR(%s%) TO MONTH', true, false, naFloat, stIntervalYear);
          Add('Long', 'LONG', false, false, naInteger, stLongInt);
          Add('Long raw', 'LONG RAW', false, false, naBlob, stLongRaw);
          Add('NChar', 'NCHAR(%s%)', true, false, naString, stNChar);
          Add('NClob', 'NCLOB', false, false, naBlob, stNClob);
          Add('Number', 'NUMBER(%s%, %p%)', true, true, naFloat, stNumericXY);
          Add('Number (floating point)', 'NUMBER', false, false, naFloat, stFloat);
          Add('NVarchar2', 'NVARCHAR2(%s%)', true, false, naString, stNVarChar);
          Add('Raw', 'RAW(%s%)', true, false, naBlob, stRaw);
          Add('Rowid', 'ROWID', false, false, naString, stRowID);
          Add('Timestamp', 'TIMESTAMP(%s%)', true, false, naDateTime, stTimeStamp);
          Add('Timestamp with local time zone', 'TIMESTAMP(%s%) WITH LOCAL TIME ZONE', true, false, naFloat, stTimeStampLocalZone);
          Add('Timestamp with time zone', 'TIMESTAMP(%s%) WITH TIME ZONE', true, false, naFloat, stTimeStampTimeZone);
          Add('Urowid', 'UROWID(%s%)', true, false, naString, stURowID);
          Add('Varchar2', 'VARCHAR2(%s%)', true, false, naString, stString);

          // ANSI datatypes
          Add('Float', 'FLOAT(%s)', true, false, naFloat, stFloatX);
          Add('Double precision', 'DOUBLE PRECISION', false, false, naFloat, stDouble);
          Add('Numeric', 'NUMERIC(%s%, %p%)', true, true, naFloat, stNumericXY);
          Add('Decimal', 'DECIMAL(%s%, %p%)', true, true, naFloat, stDecimal);
          Add('Integer', 'INTEGER', false, false, naInteger, stInteger);
          Add('Smallint', 'SMALLINT', false, false, naInteger, stSmallint);
          Add('Real', 'REAL', false, false, naFloat, stReal);
        end;
      fdbMySQL51:
        begin
          Add('BigInt', 'BIGINT', False, False, naInteger, stLongInt);
          Add('BigInt (autoincrement)', 'BIGINT AUTO_INCREMENT', False, False, naInteger, stLongCounter, True).ForeignDataTypeName := 'BigInt';
          Add('Int', 'INT', False, False, naInteger, stInteger);
          Add('Int (autoincrement)', 'INT AUTO_INCREMENT', False, False, naInteger, stCounter, True).ForeignDataTypeName := 'Int';
          Add('MediumInt', 'MEDIUMINT', False, False, naInteger, stMediumInt);
          Add('MediumInt (autoincrement)', 'MEDIUMINT AUTO_INCREMENT', False, False, naInteger, stMediumCounter, True).ForeignDataTypeName := 'MediumImt';
          Add('SmallInt', 'SMALLINT', False, False, naInteger, stSmallInt);
          Add('SmallInt (autoincrement)', 'SMALLINT AUTO_INCREMENT', False, False, naInteger, stSmallCounter, True).ForeignDataTypeName := 'SmallInt';
          Add('TinyInt', 'TINYINT', False, False, naInteger, stTinyInt);
          Add('TinyInt (autoincrement)', 'TINYINT AUTO_INCREMENT', False, False, naInteger, stTinyCounter, True).ForeignDataTypeName := 'TinyInt';

          Add('Bit', 'BIT', False, False, naInteger, stBit);
          Add('Decimal', 'DECIMAL(%s%,%p%)', True, True, naFloat, stDecimal);
          Add('Double precision', 'DOUBLE PRECISION', False, False, naFloat, stDouble);
          Add('Float', 'FLOAT(%s%)', True, False, naFloat, stFloat);
          Add('Numeric', 'NUMERIC(%s%,%p%)', True, True, naFloat, stNumericXY);
          Add('Real', 'REAL', False, False, naFloat, stReal);

          Add('Date', 'DATE', False, False, naDateTime, stDate);
          Add('DateTime', 'DATETIME', False, False, naDateTime, stDateTime);
          Add('Time', 'TIME', False, False, naDateTime, stTime);
          Add('TimeStamp', 'TIMESTAMP', False, False, naDateTime, stTimeStamp);
          Add('Year', 'YEAR', False, False, naDateTime, stYear);

          Add('Binary', 'BINARY(%s%)', True, False, naBlob, stBinary);
          Add('Blob', 'BLOB', False, False, naBlob, stBlob);
          Add('Char', 'CHAR(%s%)', True, False, naString, stChar);
          Add('LongBlob', 'LONGBLOB', False, False, naBlob, stLongBlob);
          Add('LongText', 'LONGTEXT', False, False, naMemo, stLongText);
          Add('MediumBlob', 'MEDIUMBLOB', False, False, naBlob, stMediumBlob);
          Add('MediumText', 'MEDIUMTEXT', False, False, naMemo, stMediumText);
          Add('Text', 'TEXT', False, False, naMemo, stMemo);
          Add('TinyBlob', 'TINYBLOB', False, False, naBlob, stTinyBlob);
          Add('TinyText', 'TINYTEXT', False, False, naMemo, stTinyText);
          Add('VarBinary', 'VARBINARY(%s%)', True, False, naBlob, stVarBinary);
          Add('VarChar', 'VARCHAR(%s%)', True, False, naString, stString);
          Add('Computed', '', false, false, naComputed, stComputed).Computed := true;
        end;
      fdbElevateDB:
        begin
          Add('Integer', 'INTEGER', false, false, naInteger, stInteger);
          Add('SmallInt', 'SMALLINT', false, false, naInteger, stSmallInt);
          Add('BigInt', 'BIGINT', false, false, naInteger, stLongInt);
          Add('Numeric', 'NUMERIC(%s%,%p%)', true, true, naFloat, stNumericXY);
          Add('Decimal', 'DECIMAL(%s%,%p%)', true, true, naFloat, stDecimal);
          Add('Double precision', 'DOUBLE PRECISION', false, false, naFloat, stDouble);
          Add('Float', 'FLOAT', false, false, naFloat, stFloat);

          Add('Char', 'CHAR(%s%)', true, false, naString, stChar);
          Add('VarChar', 'VARCHAR(%s%)', true, false, naString, stString);
          Add('Guid', 'GUID', false, false, naBlob, stGUID);
          Add('Clob', 'CLOB', false, false, naBlob, stClob);
          Add('Byte', 'BYTE(%s%)', true, false, naBlob, stBinary);
          Add('VarByte', 'VARBYTE(%s%)', true, false, naBlob, stVarBinary);
          Add('Blob', 'BLOB', false, false, naBlob, stBlob);

          Add('Date', 'DATE', false, false, naDateTime, stDate);
          Add('Time', 'TIME', false, false, naDateTime, stTime);
          Add('TimeStamp', 'TIMESTAMP', false, false, naDateTime, stTimeStamp);

          Add('Boolean', 'BOOLEAN', false, false, naBoolean, stBoolean);

          Add('Interval year', 'INTERVAL YEAR', false, false, naFloat, stIntervalYear);
          Add('Interval year to month', 'INTERVAL YEAR TO MONTH', false, false, naFloat, stIntervalYearToMonth);
          Add('Interval month', 'INTERVAL MONTH', false, false, naFloat, stIntervalMonth);
          Add('Interval day', 'INTERVAL DAY', false, false, naFloat, stIntervalDay);
          Add('Interval day to hour', 'INTERVAL DAY TO HOUR', false, false, naFloat, stIntervalDayToHour);
          Add('Interval day to minute', 'INTERVAL DAY TO MINUTE', false, false, naFloat, stIntervalDayToMinute);
          Add('Interval day to msecond', 'INTERVAL DAY TO MSECOND', false, false, naFloat, stIntervalDayToMSecond);
          Add('Interval day to second', 'INTERVAL DAY TO SECOND', false, false, naFloat, stIntervalDayToSecond);
          Add('Interval hour', 'INTERVAL HOUR', false, false, naFloat, stIntervalHour);
          Add('Interval hour to minute', 'INTERVAL HOUR TO MINUTE', false, false, naFloat, stIntervalHourToMinute);
          Add('Interval hour to msecond', 'INTERVAL HOUR TO MSECOND', false, false, naFloat, stIntervalHourToMSecond);
          Add('Interval hour to second', 'INTERVAL HOUR TO SECOND', false, false, naFloat, stIntervalHourToSecond);
          Add('Interval minute', 'INTERVAL MINUTE', false, false, naFloat, stIntervalMinute);
          Add('Interval minute to msecond', 'INTERVAL MINUTE TO MSECOND', false, false, naFloat, stIntervalMinuteToMSecond);
          Add('Interval minute to second', 'INTERVAL MINUTE TO SECOND', false, false, naFloat, stIntervalMinuteToSecond);
          Add('Interval second', 'INTERVAL SECOND', false, false, naFloat, stIntervalSecond);
          Add('Interval second to msecond', 'INTERVAL SECOND TO MSECOND', false, false, naFloat, stIntervalSecondToMSecond);
          Add('Interval msecond', 'INTERVAL MSECOND', false, false, naFloat, stIntervalMSecond);

          Add('Computed', '', false, false, naComputed, stComputed).Computed := true;
          Add('Identity (always)', 'INTEGER GENERATED ALWAYS AS IDENTITY (START WITH %e%, INCREMENT BY %i%)', false, false,
            naInteger, stCounter, true, true, true).ForeignDataTypeName := 'Integer';
          Add('Identity (default)', 'INTEGER GENERATED BY DEFAULT AS IDENTITY (START WITH %e%, INCREMENT BY %i%)', false, false,
            naInteger, stCounter, true, true, true).ForeignDataTypeName := 'Integer';
        end;
    end;
  end;
end;

class function TDBProperties.GetFixedDatabaseType(ADatabaseID: string): TFixedDBType;
begin
  for result := Low(TFixedDBType) to High(TFixedDBType) do
    if FixedDBTypeID[result] = ADatabaseID then
      exit;
  result := fdbUnknown;
end;

class procedure TDBProperties.LoadAll(ADatabase: TGDAODatabase);
begin
  AllocObjectCategories(ADatabase);
end;

end.

