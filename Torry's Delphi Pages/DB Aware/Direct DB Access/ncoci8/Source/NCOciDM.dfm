object OCIDM: TOCIDM
  OldCreateOrder = True
  Left = 285
  Top = 161
  Height = 479
  Width = 741
  object qrSelObjs: TOCIQuery
    Macros = <
      item
        Value = Null
        Name = 'system'
      end>
    SQL.Strings = (
      
        'select /*+ALL_ROWS*/ Decode(owner, USER, '#39#39', OWNER || '#39'.'#39') || OB' +
        'JECT_NAME as object_name '
      'from all_objects '
      'where object_type in ('#39'TABLE'#39', '#39'VIEW'#39') &system'
      'union all'
      
        'select /*+ALL_ROWS*/ Decode(owner, '#39'PUBLIC'#39', '#39#39', USER, '#39#39', OWNER' +
        ' || '#39'.'#39') || SYNONYM_NAME as object_name  '
      'from all_synonyms'
      'where (table_owner, table_name) in ('
      #9'select owner, object_name'
      #9'from all_objects'
      #9'where object_type in ('#39'TABLE'#39', '#39'VIEW'#39')  &system'
      ')'
      'order by 1')
    Left = 23
    Top = 8
  end
  object qrPacks: TOCIQuery
    Macros = <
      item
        Value = Null
        Name = 'system'
      end>
    SQL.Strings = (
      
        'select /*+ALL_ROWS*/ Decode(owner, USER, '#39#39', OWNER || '#39'.'#39') || OB' +
        'JECT_NAME as object_name'
      'from all_objects'
      'where object_type in ('#39'PACKAGE'#39') &system'
      'union all'
      
        'select /*+ALL_ROWS*/ Decode(owner, '#39'PUBLIC'#39', '#39#39', USER, '#39#39', OWNER' +
        ' || '#39'.'#39') || SYNONYM_NAME as object_name'
      'from all_synonyms'
      'where (table_owner, table_name) in ('
      #9'select owner, object_name'
      #9'from all_objects'
      #9'where object_type in ('#39'PACKAGE'#39') &system'
      ')'
      'order by 1')
    Left = 87
    Top = 8
  end
  object qrPackProcs: TOCIQuery
    Params = <
      item
        OName = ':O'
        ODataType = otString
        OParamType = odIn
        ODataSize = 255
      end
      item
        OName = ':P'
        ODataType = otString
        OParamType = odIn
        ODataSize = 255
      end>
    SQL.Strings = (
      'select /*+ALL_ROWS*/ distinct object_name'
      'from ('
      '    select /*+PUSH_SUBQ*/ object_name'
      '    from all_arguments'
      '    where owner = nvl(:O, user) and package_name = :P'
      '    union all'
      '    select /*+PUSH_SUBQ*/ object_name'
      '    from all_arguments'
      '    where (owner, package_name) in ('
      '        select table_owner, table_name'
      '        from all_synonyms'
      '        where (owner in ('#39'PUBLIC'#39', user) or owner = :O) and'
      '              synonym_name = :P'
      '    )'
      ')')
    Left = 143
    Top = 8
  end
  object qrProcs: TOCIQuery
    Macros = <
      item
        Value = Null
        Name = 'system'
      end>
    SQL.Strings = (
      
        'select /*+ALL_ROWS*/ Decode(owner, USER, '#39#39', OWNER || '#39'.'#39') || OB' +
        'JECT_NAME as object_name'
      'from all_objects'
      'where object_type in ('#39'FUNCTION'#39', '#39'PROCEDURE'#39') &system'
      'union all'
      
        'select /*+ALL_ROWS*/ Decode(owner, '#39'PUBLIC'#39', '#39#39', USER, '#39#39', OWNER' +
        ' || '#39'.'#39') || SYNONYM_NAME as object_name'
      'from all_synonyms'
      'where (table_owner, table_name) in ('
      #9'select owner, object_name'
      #9'from all_objects'
      #9'where object_type in ('#39'FUNCTION'#39', '#39'PROCEDURE'#39') &system'
      ')'
      'order by 1')
    Left = 207
    Top = 8
  end
  object qrSeqs: TOCIQuery
    Macros = <
      item
        Value = Null
        Name = 'system'
      end>
    SQL.Strings = (
      
        'select /*+ALL_ROWS*/ Decode(owner, USER, '#39#39', OWNER || '#39'.'#39') || OB' +
        'JECT_NAME as object_name '
      'from all_objects '
      'where object_type in ('#39'SEQUENCE'#39') &system'
      'union all'
      
        'select /*+ALL_ROWS*/ Decode(owner, '#39'PUBLIC'#39', '#39#39', USER, '#39#39', OWNER' +
        ' || '#39'.'#39') || SYNONYM_NAME as object_name  '
      'from all_synonyms'
      'where (table_owner, table_name) in ('
      #9'select owner, object_name'
      #9'from all_objects'
      #9'where object_type in ('#39'SEQUENCE'#39')  &system'
      ')'
      'order by 1')
    Left = 263
    Top = 8
  end
  object qrTabDefaults: TOCIQuery
    Params = <
      item
        OName = ':OWNER'
        OParamType = odIn
      end
      item
        OName = ':TABNAME'
        OParamType = odIn
      end>
    SQL.Strings = (
      'select column_name, data_default '
      'from all_tab_columns '
      
        'where data_default is not null and owner = NVL(:OWNER, USER) and' +
        ' table_name = :TABNAME')
    Left = 24
    Top = 56
  end
  object qrTabCons: TOCIQuery
    Params = <
      item
        OName = ':OWNER'
        OParamType = odIn
      end
      item
        OName = ':TABNAME'
        OParamType = odIn
      end>
    SQL.Strings = (
      'select constraint_name, search_condition'
      'from all_constraints co'
      
        'where co.owner = NVL(:OWNER, USER) and co.table_name = :TABNAME ' +
        'and co.constraint_type = '#39'C'#39)
    Left = 88
    Top = 56
  end
end
