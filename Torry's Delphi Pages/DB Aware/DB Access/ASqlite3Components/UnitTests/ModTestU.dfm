object modTest: TmodTest
  OldCreateOrder = False
  Left = 202
  Top = 200
  Height = 455
  Width = 592
  object DbTest: TASQLite3DB
    Database = ':memory:'
    DefaultExt = '.sqb3'
    Version = '3.3.5'
    DriverDLL = 'sqlite3.dll'
    Connected = False
    MustExist = False
    ASQLiteInlineSQL = InlineCreateTables
    ExecuteInlineSQL = True
    Left = 96
    Top = 56
  end
  object InlineCreateTables: TASQLite3InlineSQL
    SQL.Strings = (
      'CREATE TABLE "Testing" ('
      '"FPKey" INTEGER  NOT NULL PRIMARY KEY AUTOINCREMENT,'
      '"FInt" INTEGER  NULL,'
      '"FFloat" FLOAT  NULL,'
      '"FBool" BOOLEAN  NULL,'
      '"FDate" DATE  NULL,'
      '"FDateTime" DateTime  NULL,'
      '"FText" VARCHAR(256)  NULL'
      ');'
      ''
      'CREATE TABLE TestBlobs'
      '('
      '"FPKey" INTEGER  NOT NULL PRIMARY KEY AUTOINCREMENT,'
      '"FBlob" MEMO  NULL'
      ');'
      ''
      'CREATE TABLE TestMaster'
      '('
      ' "FAMILYCODE" INTEGER NOT NULL,'
      ' "FATHER" VARCHAR(30) NOT NULL,'
      ' "MOTHER" VARCHAR(30) NOT NULL,'
      ' PRIMARY KEY ("FAMILYCODE")'
      ');'
      ''
      'CREATE TABLE TestDetail'
      '('
      ' "FAMILYCODE" INTEGER NOT NULL,'
      ' "CHILD" VARCHAR(30) NOT NULL,'
      ' PRIMARY KEY("FAMILYCODE", "CHILD")'
      ');')
    Left = 240
    Top = 64
  end
  object InlineCreateData: TASQLite3InlineSQL
    SQL.Strings = (
      
        'INSERT INTO Testing (FPKey, FInt, FFloat, FBool, FDate, FDateTim' +
        'e, FText)'
      
        '       VALUES (1, 12345, 123.45, '#39'True'#39', '#39'2006-4-1'#39', '#39'2006-04-01' +
        ' 23:30:00'#39', '#39'This is text!'#39');'
      ''
      'INSERT INTO Testing (FPKey)'
      '       VALUES (2);'
      ''
      'INSERT INTO TestMaster VALUES (1, '#39'john'#39', '#39'mary'#39');'
      'INSERT INTO TestMaster VALUES (2, '#39'peter'#39', '#39'jane'#39');'
      'INSERT INTO TestMaster VALUES (3, '#39'wim'#39', '#39'pascal'#39');'
      'INSERT INTO TestMaster VALUES (4, '#39'evert'#39', '#39'tonia'#39');'
      ''
      'INSERT INTO TestDetail VALUES (1, '#39'jane'#39');'
      'INSERT INTO TestDetail VALUES (2, '#39'irene'#39');'
      'INSERT INTO TestDetail VALUES (2, '#39'hjalmar'#39');'
      'INSERT INTO TestDetail VALUES (3, '#39'diana'#39');'
      'INSERT INTO TestDetail VALUES (3, '#39'bert'#39');'
      'INSERT INTO TestDetail VALUES (3, '#39'ernie'#39');'
      'INSERT INTO TestDetail VALUES (4, '#39'jane'#39');'
      'INSERT INTO TestDetail VALUES (4, '#39'oprah'#39');'
      'INSERT INTO TestDetail VALUES (4, '#39'anastacia'#39');'
      'INSERT INTO TestDetail VALUES (4, '#39'jim'#39');'
      '')
    Left = 240
    Top = 128
  end
  object sqlTemp: TASQLite3Query
    AutoCommit = False
    SQLiteDateFormat = True
    Connection = DbTest
    MaxResults = 0
    StartResult = 0
    TypeLess = False
    SQLCursor = True
    ReadOnly = False
    UniDirectional = False
    RawSQL = False
    Left = 240
    Top = 184
  end
  object tblTemp: TASQLite3Table
    AutoCommit = False
    SQLiteDateFormat = True
    Connection = DbTest
    MaxResults = 0
    StartResult = 0
    TypeLess = False
    SQLCursor = True
    ReadOnly = False
    UniDirectional = False
    PrimaryAutoInc = False
    Left = 248
    Top = 248
  end
  object tblCalc_: TASQLite3Table
    AutoCommit = False
    SQLiteDateFormat = True
    Connection = DbTest
    MaxResults = 0
    StartResult = 0
    TypeLess = False
    SQLCursor = True
    ReadOnly = False
    UniDirectional = False
    OnCalcFields = tblCalc_CalcFields
    TableName = 'Testing'
    PrimaryAutoInc = False
    Left = 248
    Top = 312
    object tblCalc_FPKey: TIntegerField
      FieldName = 'FPKey'
    end
    object tblCalc_FInt: TIntegerField
      FieldName = 'FInt'
    end
    object tblCalc_FText: TStringField
      FieldName = 'FText'
      Size = 256
    end
    object tblCalc_CalcInt: TIntegerField
      FieldKind = fkCalculated
      FieldName = 'CalcInt'
      Calculated = True
    end
    object tblCalc_CalcString: TStringField
      FieldKind = fkCalculated
      FieldName = 'CalcString'
      Size = 50
      Calculated = True
    end
  end
  object tblMaster: TASQLite3Table
    AutoCommit = False
    SQLiteDateFormat = True
    Connection = DbTest
    MaxResults = 0
    StartResult = 0
    TypeLess = False
    SQLCursor = True
    ReadOnly = False
    UniDirectional = False
    TableName = 'TestMaster'
    PrimaryAutoInc = False
    Left = 376
    Top = 64
  end
  object TBLDetail: TASQLite3Table
    AutoCommit = False
    SQLiteDateFormat = True
    Connection = DbTest
    MaxResults = 0
    StartResult = 0
    TypeLess = False
    MasterFields = 'FAMILYCODE=FAMILYCODE;'
    MasterSource = DSMaster
    SQLCursor = True
    ReadOnly = False
    UniDirectional = False
    TableName = 'TestDetail'
    PrimaryAutoInc = False
    Left = 376
    Top = 120
  end
  object DSMaster: TDataSource
    DataSet = tblMaster
    Left = 416
    Top = 64
  end
  object tblOutOfOrder_: TASQLite3Table
    AutoCommit = False
    SQLiteDateFormat = True
    Connection = DbTest
    MaxResults = 0
    StartResult = 0
    TypeLess = False
    SQLCursor = True
    ReadOnly = False
    UniDirectional = False
    TableName = 'TestBlobs'
    PrimaryAutoInc = False
    Left = 384
    Top = 216
    object tblOutOfOrder_FBlob: TMemoField
      FieldName = 'FBlob'
      BlobType = ftMemo
    end
    object tblOutOfOrder_FPKey: TIntegerField
      FieldName = 'FPKey'
    end
  end
  object sqlOutOfOrder_: TASQLite3Query
    AutoCommit = False
    SQLiteDateFormat = True
    Connection = DbTest
    MaxResults = 0
    StartResult = 0
    TypeLess = False
    SQLCursor = True
    ReadOnly = False
    UniDirectional = False
    RawSQL = False
    SQL.Strings = (
      'select * from TestBlobs')
    Left = 384
    Top = 288
    object sqlOutOfOrder_FBlob: TMemoField
      FieldName = 'FBlob'
      BlobType = ftMemo
    end
    object sqlOutOfOrder_FPKey: TIntegerField
      FieldName = 'FPKey'
    end
  end
end
