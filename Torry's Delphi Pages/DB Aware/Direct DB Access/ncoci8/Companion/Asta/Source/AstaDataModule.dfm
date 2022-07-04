object AstaOracleDataModule: TAstaOracleDataModule
  OldCreateOrder = True
  OnCreate = AstaOracleDataModuleCreate
  Left = 285
  Top = 161
  Height = 479
  Width = 741
  object AstaProvider1: TAstaProvider
    Dataset = EmpQuery
    UpdateTableName = 'SCOTT.EMP'
    PrimeFields.Strings = (
      'EMPNO')
    RefetchOnInsert.Strings = (
      'HIREDATE'
      'SAL')
    BeforeInsert = AstaProvider1BeforeInsert
    Left = 232
    Top = 104
  end
  object MainSession: TOCIDatabase
    InitModes = [dmThreaded]
    UserName = 'demo'
    Password = 'demo'
    Left = 32
    Top = 8
  end
  object SelectQuery: TOCIQuery
    Left = 136
    Top = 56
  end
  object ExecQuery: TOCIQuery
    Left = 136
    Top = 112
  end
  object qrySpName: TOCIQuery
    FetchParams.RowsetSize = 50
    FetchParams.FetchExact = -1
    SQL.Strings = (
      'Select owner,object_name,object_type from sys.all_objects'
      'where object_type in ('#39'FUNCTION'#39', '#39'PROCEDURE'#39', '#39'PACKAGE'#39')')
    Left = 32
    Top = 56
  end
  object qryFields: TOCIQuery
    Left = 88
    Top = 296
  end
  object qryMeta: TOCIQuery
    Left = 32
    Top = 152
  end
  object EmpQuery: TOCIQuery
    SQL.Strings = (
      'select D.*, cursor('
      
        '    select E.* from EMPLOYEE E where E.DEPARTMENT_ID = D.DEPARTM' +
        'ENT_ID'
      ') emps'
      'from DEPARTMENT D'
      'order by D.NAME')
    Left = 232
    Top = 56
  end
  object spSpColumn: TOCIStoredProc
    Left = 32
    Top = 104
  end
  object ExecProc: TOCIStoredProc
    Left = 136
    Top = 168
  end
  object qryTables: TOCIQuery
    SQL.Strings = (
      'Select Owner,Table_Name from all_Tables')
    Left = 32
    Top = 200
  end
  object qryViews: TOCIQuery
    SQL.Strings = (
      'Select Owner, Table_Name from all_views')
    Left = 32
    Top = 248
  end
  object qryIndexes: TOCIQuery
    Params = <
      item
        OName = ':TABLENAME'
        OParamType = odIn
      end
      item
        OName = ':OWNER'
        OParamType = odIn
      end>
    SQL.Strings = (
      'SELECT index_name, uniqueness '
      'FROM all_indexes'
      'WHERE table_name = :TableName AND table_owner = :Owner')
    Left = 32
    Top = 296
  end
  object qryPKFields: TOCIQuery
    Params = <
      item
        OName = ':OWNER'
        OParamType = odIn
      end
      item
        OName = ':TABLENAME'
        OParamType = odIn
      end>
    SQL.Strings = (
      'SELECT column_name'
      'FROM all_ind_columns'
      'WHERE (index_owner, index_name) in ('
      '  SELECT owner, index_name'
      '  FROM all_indexes'
      
        '  WHERE table_owner = :Owner and table_name = :TableName and uni' +
        'queness = '#39'UNIQUE'#39' AND ROWNUM <= 1'
      ')'
      'ORDER BY column_position')
    Left = 144
    Top = 296
  end
end
