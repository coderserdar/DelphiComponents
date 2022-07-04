object NCOciPLSQLGenSetupFrm: TNCOciPLSQLGenSetupFrm
  Left = 309
  Top = 274
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'NCOCI8 PL/SQL Wrapper Objects Generator'
  ClientHeight = 433
  ClientWidth = 550
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 550
    Height = 393
    ActivePage = TabSheet1
    Align = alTop
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Package'
      object Bevel1: TBevel
        Left = 0
        Top = 0
        Width = 542
        Height = 47
        Align = alTop
        Shape = bsBottomLine
      end
      object Label1: TLabel
        Left = 8
        Top = 16
        Width = 72
        Height = 13
        Caption = 'Package name'
      end
      object Splitter1: TSplitter
        Left = 0
        Top = 217
        Width = 542
        Height = 3
        Cursor = crVSplit
        Align = alTop
      end
      object Bevel2: TBevel
        Left = 0
        Top = 47
        Width = 542
        Height = 3
        Align = alTop
        Shape = bsSpacer
      end
      object dblckPackage: TDBLookupComboBox
        Left = 88
        Top = 13
        Width = 233
        Height = 21
        KeyField = 'OWNER||'#39'.'#39'||OBJECT_NAME'
        ListField = 'OWNER||'#39'.'#39'||OBJECT_NAME'
        ListSource = dsPackages
        TabOrder = 0
        OnClick = dblckPackageExit
      end
      object dbgProcs: TDBGrid
        Left = 0
        Top = 50
        Width = 542
        Height = 167
        Align = alTop
        DataSource = dsPackProcs
        Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgMultiSelect]
        ReadOnly = True
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object DBGrid2: TDBGrid
        Left = 0
        Top = 220
        Width = 542
        Height = 145
        Align = alClient
        DataSource = dsPackProcParams
        Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
        ReadOnly = True
        TabOrder = 2
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object btnSelectAll: TButton
        Left = 461
        Top = 12
        Width = 75
        Height = 25
        Caption = 'Select All'
        TabOrder = 3
        OnClick = btnSelectAllClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Options'
      ImageIndex = 2
      object Bevel3: TBevel
        Left = 0
        Top = 0
        Width = 542
        Height = 64
        Align = alTop
        Shape = bsSpacer
      end
      object Label2: TLabel
        Left = 8
        Top = 10
        Width = 101
        Height = 13
        Caption = 'Base '#39'Package'#39' class'
      end
      object Label3: TLabel
        Left = 8
        Top = 34
        Width = 93
        Height = 13
        Caption = 'Base '#39'Record'#39' class'
      end
      object Label4: TLabel
        Left = 264
        Top = 10
        Width = 85
        Height = 13
        Caption = 'Base '#39'Table'#39' class'
      end
      object edtParPackClass: TEdit
        Left = 128
        Top = 8
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'TOCICustomPackage'
      end
      object edtParRecClass: TEdit
        Left = 128
        Top = 32
        Width = 121
        Height = 21
        TabOrder = 1
        Text = 'TOCIPLSQLRecord'
      end
      object edtParTabClass: TEdit
        Left = 392
        Top = 8
        Width = 121
        Height = 21
        TabOrder = 2
        Text = 'TOCIPLSQLTable'
      end
      object cbSkipUnsupProcs: TCheckBox
        Left = 262
        Top = 34
        Width = 143
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Skip unsupp. procedures'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object pnlProc: TPanel
        Tag = 5
        Left = 0
        Top = 260
        Width = 542
        Height = 49
        Align = alTop
        TabOrder = 8
        object Label5: TLabel
          Left = 8
          Top = 25
          Width = 60
          Height = 13
          Caption = 'Name format'
        end
        object Label6: TLabel
          Left = 148
          Top = 25
          Width = 24
          Height = 13
          Caption = 'Case'
        end
        object Label7: TLabel
          Left = 6
          Top = 6
          Width = 65
          Height = 13
          Caption = 'Procedures'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Edit1: TEdit
          Left = 76
          Top = 23
          Width = 65
          Height = 21
          TabOrder = 0
        end
        object ComboBox1: TComboBox
          Left = 180
          Top = 23
          Width = 105
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          Items.Strings = (
            'UPPER CASE'
            'lower case'
            'Pretty Case')
        end
        object CheckBox1: TCheckBox
          Left = 293
          Top = 25
          Width = 57
          Height = 17
          Caption = 'Trim '#39'_'#39
          TabOrder = 2
        end
        object CheckBox2: TCheckBox
          Left = 356
          Top = 25
          Width = 137
          Height = 17
          Caption = 'Trim non Delphi chars'
          TabOrder = 3
        end
      end
      object pnlPar: TPanel
        Tag = 4
        Left = 0
        Top = 309
        Width = 542
        Height = 49
        Align = alTop
        TabOrder = 9
        object Label8: TLabel
          Left = 8
          Top = 25
          Width = 60
          Height = 13
          Caption = 'Name format'
        end
        object Label9: TLabel
          Left = 148
          Top = 25
          Width = 24
          Height = 13
          Caption = 'Case'
        end
        object Label10: TLabel
          Left = 6
          Top = 6
          Width = 64
          Height = 13
          Caption = 'Parameters'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Edit2: TEdit
          Left = 76
          Top = 23
          Width = 65
          Height = 21
          TabOrder = 0
        end
        object ComboBox2: TComboBox
          Left = 180
          Top = 23
          Width = 105
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          Items.Strings = (
            'UPPER CASE'
            'lower case'
            'Pretty Case')
        end
        object CheckBox3: TCheckBox
          Left = 293
          Top = 25
          Width = 57
          Height = 17
          Caption = 'Trim '#39'_'#39
          TabOrder = 2
        end
        object CheckBox4: TCheckBox
          Left = 356
          Top = 25
          Width = 137
          Height = 17
          Caption = 'Trim non Delphi chars'
          TabOrder = 3
        end
      end
      object pnlPack: TPanel
        Tag = 3
        Left = 0
        Top = 162
        Width = 542
        Height = 49
        Align = alTop
        TabOrder = 6
        object Label11: TLabel
          Left = 8
          Top = 25
          Width = 60
          Height = 13
          Caption = 'Name format'
        end
        object Label12: TLabel
          Left = 148
          Top = 25
          Width = 24
          Height = 13
          Caption = 'Case'
        end
        object Label13: TLabel
          Left = 6
          Top = 6
          Width = 85
          Height = 13
          Caption = 'Package Class'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Edit3: TEdit
          Left = 76
          Top = 23
          Width = 65
          Height = 21
          TabOrder = 0
        end
        object ComboBox3: TComboBox
          Left = 180
          Top = 23
          Width = 105
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          Items.Strings = (
            'UPPER CASE'
            'lower case'
            'Pretty Case')
        end
        object CheckBox5: TCheckBox
          Left = 293
          Top = 25
          Width = 57
          Height = 17
          Caption = 'Trim '#39'_'#39
          TabOrder = 2
        end
        object CheckBox6: TCheckBox
          Left = 356
          Top = 25
          Width = 137
          Height = 17
          Caption = 'Trim non Delphi chars'
          TabOrder = 3
        end
      end
      object pnlTab: TPanel
        Tag = 2
        Left = 0
        Top = 113
        Width = 542
        Height = 49
        Align = alTop
        TabOrder = 5
        object Label14: TLabel
          Left = 8
          Top = 25
          Width = 60
          Height = 13
          Caption = 'Name format'
        end
        object Label15: TLabel
          Left = 148
          Top = 25
          Width = 24
          Height = 13
          Caption = 'Case'
        end
        object Label16: TLabel
          Left = 6
          Top = 6
          Width = 80
          Height = 13
          Caption = 'Table Classes'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Edit4: TEdit
          Left = 76
          Top = 23
          Width = 65
          Height = 21
          TabOrder = 0
        end
        object ComboBox4: TComboBox
          Left = 180
          Top = 23
          Width = 105
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          Items.Strings = (
            'UPPER CASE'
            'lower case'
            'Pretty Case')
        end
        object CheckBox7: TCheckBox
          Left = 293
          Top = 25
          Width = 57
          Height = 17
          Caption = 'Trim '#39'_'#39
          TabOrder = 2
        end
        object CheckBox8: TCheckBox
          Left = 356
          Top = 25
          Width = 137
          Height = 17
          Caption = 'Trim non Delphi chars'
          TabOrder = 3
        end
      end
      object pnlRec: TPanel
        Tag = 1
        Left = 0
        Top = 64
        Width = 542
        Height = 49
        Align = alTop
        TabOrder = 4
        object Label17: TLabel
          Left = 8
          Top = 25
          Width = 60
          Height = 13
          Caption = 'Name format'
        end
        object Label18: TLabel
          Left = 148
          Top = 25
          Width = 24
          Height = 13
          Caption = 'Case'
        end
        object Label19: TLabel
          Left = 6
          Top = 6
          Width = 89
          Height = 13
          Caption = 'Record Classes'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Edit5: TEdit
          Left = 76
          Top = 23
          Width = 65
          Height = 21
          TabOrder = 0
        end
        object ComboBox5: TComboBox
          Left = 180
          Top = 23
          Width = 105
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          Items.Strings = (
            'UPPER CASE'
            'lower case'
            'Pretty Case')
        end
        object CheckBox9: TCheckBox
          Left = 293
          Top = 25
          Width = 57
          Height = 17
          Caption = 'Trim '#39'_'#39
          TabOrder = 2
        end
        object CheckBox10: TCheckBox
          Left = 356
          Top = 25
          Width = 137
          Height = 17
          Caption = 'Trim non Delphi chars'
          TabOrder = 3
        end
      end
      object pnlField: TPanel
        Left = 0
        Top = 211
        Width = 542
        Height = 49
        Align = alTop
        TabOrder = 7
        object Label20: TLabel
          Left = 8
          Top = 25
          Width = 60
          Height = 13
          Caption = 'Name format'
        end
        object Label21: TLabel
          Left = 148
          Top = 25
          Width = 24
          Height = 13
          Caption = 'Case'
        end
        object Label22: TLabel
          Left = 6
          Top = 6
          Width = 34
          Height = 13
          Caption = 'Fields'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Edit6: TEdit
          Left = 76
          Top = 23
          Width = 65
          Height = 21
          TabOrder = 0
        end
        object ComboBox6: TComboBox
          Left = 180
          Top = 23
          Width = 105
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          Items.Strings = (
            'UPPER CASE'
            'lower case'
            'Pretty Case')
        end
        object CheckBox11: TCheckBox
          Left = 293
          Top = 25
          Width = 57
          Height = 17
          Caption = 'Trim '#39'_'#39
          TabOrder = 2
        end
        object CheckBox12: TCheckBox
          Left = 356
          Top = 25
          Width = 137
          Height = 17
          Caption = 'Trim non Delphi chars'
          TabOrder = 3
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'File'
      ImageIndex = 2
      object Label23: TLabel
        Left = 8
        Top = 16
        Width = 64
        Height = 13
        Caption = 'Unit file name'
      end
      object SpeedButton1: TSpeedButton
        Left = 306
        Top = 14
        Width = 23
        Height = 22
        Flat = True
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000CE0E0000C40E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
          77777777777777777777000000000007777700333333333077770B0333333333
          07770FB03333333330770BFB0333333333070FBFB000000000000BFBFBFBFB07
          77770FBFBFBFBF0777770BFB0000000777777000777777770007777777777777
          7007777777770777070777777777700077777777777777777777}
        OnClick = SpeedButton1Click
      end
      object Label24: TLabel
        Left = 8
        Top = 49
        Width = 48
        Height = 13
        Caption = 'Unit name'
      end
      object edtUnitFileName: TEdit
        Left = 88
        Top = 14
        Width = 217
        Height = 21
        TabOrder = 0
        OnChange = edtUnitFileNameExit
      end
      object edtUnitName: TEdit
        Left = 88
        Top = 46
        Width = 217
        Height = 21
        TabOrder = 1
      end
    end
  end
  object BitBtn1: TBitBtn
    Left = 458
    Top = 402
    Width = 83
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object btnGenerate: TBitBtn
    Left = 368
    Top = 402
    Width = 83
    Height = 25
    Caption = 'Generate'
    Default = True
    TabOrder = 2
    OnClick = btnGenerateClick
    Glyph.Data = {
      42010000424D4201000000000000760000002800000011000000110000000100
      040000000000CC00000000000000000000001000000010000000000000000000
      BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
      DDDDD0000000DDDDDDDDDDDDDDDDD0000000DDDDDDDD0000DDDDD0000000DDDD
      DDD066660DDDD0000000DDDDDD06666660DDD000000000000006666660DDD000
      0000D0444406666660DDD0000000DD0444066666600DD0000000DDD044406666
      0B0DD0000000DDDD04000000BF0DD0000000DDDDD0D0FBFBFB0DD0000000DDDD
      DDD0BFBFBF0DD0000000DDDDDDD0FBFBFB0DD0000000DDDDDDD0BFBFBF0DD000
      0000DDDDDDD00000000DD0000000DDDDDDDDDDDDDDDDD0000000DDDDDDDDDDDD
      DDDDD0000000}
  end
  object qryPackProcs: TOCIQuery
    Params = <
      item
        OName = ':OWN'
        ODataType = otString
        OParamType = odIn
        ODataSize = 30
      end
      item
        OName = ':PACK'
        ODataType = otString
        OParamType = odIn
        ODataSize = 30
      end>
    DatabaseName = 'NCOCI8GenDB'
    SQL.Strings = (
      'SELECT distinct object_name, overload'
      'FROM all_arguments '
      'WHERE owner = :OWN AND package_name = :PACK'
      'ORDER BY object_name, to_number(overload) ')
    Left = 280
    Top = 152
    object qryPackProcsOBJECT_NAME: TStringField
      DisplayLabel = 'Procedure name'
      DisplayWidth = 71
      FieldName = 'OBJECT_NAME'
      Origin = 'OBJECT_NAME'
      Size = 30
    end
    object qryPackProcsOVERLOAD: TStringField
      DisplayLabel = 'Overload index'
      DisplayWidth = 17
      FieldName = 'OVERLOAD'
      Origin = 'OVERLOAD'
      Size = 40
    end
  end
  object dsPackProcs: TDataSource
    DataSet = qryPackProcs
    Left = 312
    Top = 152
  end
  object qryPackProcParams: TOCIQuery
    Params = <
      item
        OName = ':OWN'
        ODataType = otString
        OParamType = odIn
        ODataSize = 30
      end
      item
        OName = ':PACK'
        ODataType = otString
        OParamType = odIn
        ODataSize = 30
      end
      item
        OName = ':OBJECT_NAME'
        ODataType = otString
        OParamType = odIn
        ODataSize = 30
      end
      item
        OName = ':OVERLOAD'
        ODataType = otString
        OParamType = odIn
        ODataSize = 40
      end>
    DatabaseName = 'NCOCI8GenDB'
    SQL.Strings = (
      'select position, argument_name,'
      '    decode(argument_name, null, '#39'<result>'#39', in_out) as in_out,'
      #9'decode(data_type,'
      #9#9#39'PL/SQL TABLE'#39', '#39'<tab> '#39' ||'
      #9#9#9'decode(type_owner, null, '#39#39', type_owner || '#39'.'#39') ||'
      #9#9#9'decode(type_name, null, '#39#39', type_name || '#39'.'#39') ||'
      #9#9#9'decode(type_subname, null, '#39'<unnamed>'#39', type_subname),'
      #9#9#39'PL/SQL RECORD'#39', '#39'<rec> '#39' ||'
      #9#9#9'decode(type_owner, null, '#39#39', type_owner || '#39'.'#39') ||'
      #9#9#9'decode(type_name, null, '#39#39', type_name || '#39'.'#39') ||'
      #9#9#9'decode(type_subname, null, '#39'<unnamed>'#39', type_subname),'
      #9#9'pls_type) as data_type'
      'from all_arguments'
      
        'where owner = :OWN and package_name = :PACK and object_name = :O' +
        'BJECT_NAME and'
      '    (:OVERLOAD is not null and overload = :OVERLOAD or'
      '     :OVERLOAD is null and overload is null) and data_level = 0'
      'order by "SEQUENCE"')
    DataSource = dsPackProcs
    Left = 280
    Top = 184
    object qryPackProcParamsPOSITION: TFloatField
      DisplayLabel = 'Pos'
      DisplayWidth = 12
      FieldName = 'POSITION'
      Origin = 'POSITION'
      Required = True
    end
    object qryPackProcParamsARGUMENT_NAME: TStringField
      DisplayLabel = 'Argument name'
      DisplayWidth = 32
      FieldName = 'ARGUMENT_NAME'
      Origin = 'ARGUMENT_NAME'
      Size = 30
    end
    object qryPackProcParamsIN_OUT: TStringField
      DisplayLabel = 'In/Out'
      DisplayWidth = 11
      FieldName = 'IN_OUT'
      Origin = 'IN_OUT'
      Size = 9
    end
    object qryPackProcParamsDATA_TYPE: TStringField
      DisplayLabel = 'Data type'
      DisplayWidth = 47
      FieldName = 'DATA_TYPE'
      Origin = 'DATA_TYPE'
      Size = 30
    end
  end
  object dsPackProcParams: TDataSource
    DataSet = qryPackProcParams
    Left = 312
    Top = 184
  end
  object qryPackages: TOCIQuery
    DatabaseName = 'NCOCI8GenDB'
    SQL.Strings = (
      'SELECT owner, object_name, owner || '#39'.'#39' || object_name '
      'FROM all_objects '
      'WHERE object_type = '#39'PACKAGE'#39)
    Left = 280
    Top = 112
  end
  object dsPackages: TDataSource
    DataSet = qryPackages
    Left = 312
    Top = 112
  end
  object dbGen: TOCIDatabase
    DatabaseName = 'NCOCI8GenDB'
    UserName = 'demo'
    Password = 'demo'
    Left = 116
    Top = 192
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'pas'
    Filter = 'Delphi unit (*.pas)|*.pas|Any file (*.*)|*.*'
    Left = 340
    Top = 32
  end
end
