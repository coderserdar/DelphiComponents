object Form1: TForm1
  Left = 361
  Top = 227
  Width = 643
  Height = 363
  Caption = 'MSSQL TableEdit Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object split1: TSplitter
    Left = 317
    Top = 63
    Height = 247
  end
  object p1: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 63
    Align = alTop
    TabOrder = 0
    DesignSize = (
      635
      63)
    object sh_connection_status: TShape
      Left = 6
      Top = 4
      Width = 13
      Height = 53
      Brush.Color = clSilver
    end
    object btn_connect: TButton
      Left = 22
      Top = 33
      Width = 75
      Height = 25
      Caption = 'connect ...'
      TabOrder = 2
      OnClick = btn_connectClick
    end
    object btn_disconnect: TButton
      Left = 103
      Top = 32
      Width = 105
      Height = 25
      Caption = 'disconnect'
      TabOrder = 3
      OnClick = btn_disconnectClick
    end
    object cbx_connection_string: TComboBox
      Left = 137
      Top = 8
      Width = 496
      Height = 20
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Courier New'
      Font.Style = []
      ItemHeight = 12
      ItemIndex = 1
      ParentFont = False
      TabOrder = 1
      Text =
        '#1  2005 Native local:: DIRECT  #2  VendorLib=sqlncli.dll   #3  ' +
        'SERVER=?;Trusted_Connection=Yes;coCatPrefix=DATABASE;coTrimChar=' +
        '1'
      Items.Strings = (
        '?'

          '#1  2005 Native local:: DIRECT  #2  VendorLib=sqlncli.dll   #3  ' +
          'SERVER=?;Trusted_Connection=Yes;coCatPrefix=DATABASE;coTrimChar=' +
          '1'

          '#1  2005 Native pipe :: DIRECT  #2  VendorLib=sqlncli.dll   #3  ' +
          'SERVER=\\127.0.0.1\pipe\MSSQL$SQLEXPRESS2005\sql\query;Trusted_C' +
          'onnection=Yes;DATABASE=dbxoodbc;coCatPrefix=DATABASE;coTrimChar=' +
          '1'

          '#1  2000/2005   pipe :: DIRECT  #2  VendorLib=sqlsrv32.dll  #3  ' +
          'SERVER=\\127.0.0.1\pipe\MSSQL$SQLEXPRESS2005\sql\query;Trusted_C' +
          'onnection=Yes;DATABASE=dbxoodbc;coCatPrefix=DATABASE;coTrimChar=' +
          '1'

          '#1  2005 Native tcp  :: DIRECT  #2  VendorLib=sqlncli.dll   #3  ' +
          'SERVER=127.0.0.1\SQLEXPRESS2005;Trusted_Connection=Yes;DATABASE=' +
          'dbxoodbc;coCatPrefix=DATABASE;coTrimChar=1'

          '#1  2000/2005   tcp  :: DIRECT  #2  VendorLib=sqlsrv32.dll  #3  ' +
          'SERVER=127.0.0.1\SQLEXPRESS2005;Trusted_Connection=Yes;DATABASE=' +
          'dbxoodbc;coCatPrefix=DATABASE;coTrimChar=1'

          '#1  2000        tcp  :: DIRECT  #2  VendorLib=sqlsrv32.dll  #3  ' +
          'SERVER=127.0.0.1;Trusted_Connection=Yes;DATABASE=dbxoodbc;coCatP' +
          'refix=DATABASE;coTrimChar=1'

          '#1  2000        pipe :: DIRECT  #2  VendorLib=sqlsrv32.dll  #3  ' +
          'SERVER=\\.\pipe\sql\query;Trusted_Connection=Yes;DATABASE=dbxood' +
          'bc;coCatPrefix=DATABASE;coTrimChar=1')
    end
    object txt1: TStaticText
      Left = 30
      Top = 8
      Width = 101
      Height = 17
      Caption = 'Connection Strings: '
      TabOrder = 0
    end
    object SUID: TStaticText
      Left = 214
      Top = 35
      Width = 26
      Height = 17
      Caption = 'UID:'
      TabOrder = 4
    end
    object EUID: TEdit
      Left = 239
      Top = 35
      Width = 121
      Height = 21
      TabOrder = 5
    end
    object SPWD: TStaticText
      Left = 366
      Top = 35
      Width = 31
      Height = 17
      Caption = 'PWD:'
      TabOrder = 6
    end
    object EPWD: TEdit
      Left = 397
      Top = 35
      Width = 121
      Height = 21
      PasswordChar = '*'
      TabOrder = 7
    end
  end
  object statBar1: TStatusBar
    Left = 0
    Top = 310
    Width = 635
    Height = 19
    Panels = <>
  end
  object p2: TPanel
    Left = 0
    Top = 63
    Width = 317
    Height = 247
    Align = alLeft
    TabOrder = 2
    object mem_sql: TMemo
      Left = 1
      Top = 69
      Width = 315
      Height = 177
      Align = alClient
      Lines.Strings = (
        'select * from dbx_test_short')
      ScrollBars = ssBoth
      TabOrder = 0
      WantTabs = True
    end
    object p4: TPanel
      Left = 1
      Top = 1
      Width = 315
      Height = 35
      Align = alTop
      TabOrder = 1
      object btn_open: TButton
        Left = 5
        Top = 4
        Width = 91
        Height = 25
        Caption = 'open'
        TabOrder = 0
        OnClick = btn_openClick
      end
      object btn_apply: TButton
        Left = 102
        Top = 4
        Width = 105
        Height = 25
        Caption = 'apply changes'
        TabOrder = 1
        OnClick = btn_applyClick
      end
      object btn_close: TButton
        Left = 213
        Top = 4
        Width = 94
        Height = 25
        Caption = 'close'
        TabOrder = 2
        OnClick = btn_closeClick
      end
    end
    object p7: TPanel
      Left = 1
      Top = 36
      Width = 315
      Height = 33
      Align = alTop
      TabOrder = 2
      DesignSize = (
        315
        33)
      object cbx_query: TComboBox
        Left = 5
        Top = 6
        Width = 305
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbx_queryChange
        Items.Strings = (
          'select * from dbx_test_short'
          'select * from dbx_test_char'
          'select * from dbx_test_varchar'
          'select * from dbx_test_nchar'
          'select * from dbx_test_nvarchar'
          'select * from dbx_test_nchar2'
          'select * from dbx_test_long')
      end
    end
  end
  object p3: TPanel
    Left = 320
    Top = 63
    Width = 315
    Height = 247
    Align = alClient
    TabOrder = 3
    object PC: TPageControl
      Left = 1
      Top = 1
      Width = 313
      Height = 208
      ActivePage = ts_grid
      Align = alClient
      TabOrder = 1
      object ts_grid: TTabSheet
        Caption = 'DBGRID'
        object DBGrid: TDBGrid
          Left = 0
          Top = 0
          Width = 305
          Height = 180
          Align = alClient
          DataSource = DataSource
          TabOrder = 0
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'Tahoma'
          TitleFont.Style = []
        end
      end
      object ts_blob: TTabSheet
        Caption = 'BLOB'
        ImageIndex = 1
        object p6: TPanel
          Left = 0
          Top = 0
          Width = 305
          Height = 41
          Align = alTop
          TabOrder = 0
          object cbx_fields: TComboBox
            Left = 48
            Top = 8
            Width = 213
            Height = 21
            Style = csDropDownList
            DropDownCount = 17
            ItemHeight = 0
            TabOrder = 0
            OnChange = cbx_fieldsChange
            OnKeyDown = cbx_fieldsKeyDown
          end
          object StaticText1: TStaticText
            Left = 8
            Top = 8
            Width = 30
            Height = 17
            Caption = 'Field:'
            TabOrder = 1
          end
        end
        object db_memo: TDBMemo
          Left = 0
          Top = 41
          Width = 305
          Height = 139
          Align = alClient
          DataSource = DataSource
          TabOrder = 1
        end
      end
    end
    object p5: TPanel
      Left = 1
      Top = 209
      Width = 313
      Height = 37
      Align = alBottom
      TabOrder = 0
      object DBNavigator: TDBNavigator
        Left = 2
        Top = 6
        Width = 240
        Height = 25
        DataSource = DataSource
        TabOrder = 0
      end
    end
  end
  object SQLConnection: TSQLConnection
    DriverName = '@'
    GetDriverFunc = 'getSQLDriverODBCW'
    LibraryName = 'dbxoodbc.dll'
    LoginPrompt = False
    Params.Strings = (
      '@ TransIsolation=ReadCommited'
      'Database=?'
      'User_Name='
      'Password='
      'RowsetSize=20'
      'BlobSize=-1'
      'Trim Char=True'

        'Custom String=coConnectionString=Trusted_Connection=Yes;SERVER=1' +
        '27.0.0.1\SQLEXPRESS2005;DATABASE=dbxoodbc;LANGUAGE='#1088#1091#1089#1089#1082#1080#1081';coCat' +
        'Prefix=DATABASE')
    VendorLib = 'sqlncli.dll'
    AfterConnect = SQLConnectionAfterConnect
    AfterDisconnect = SQLConnectionAfterDisconnect
    Left = 582
    Top = 70
  end
  object SQLDataSet: TSQLDataSet
    CommandText =
      'SELECT'#13#10'  id,'#13#10'  f_binary,'#13#10'  f_bit,'#13#10'  f_char_1,'#13#10'  f_char_3,'#13#10 +
      '  f_datetime,'#13#10'  f_decimal_19_5,'#13#10'  f_float,'#13#10'  f_image,'#13#10'  f_in' +
      't,'#13#10'  f_money,'#13#10'  f_nchar_1,'#13#10'  f_nchar_3,'#13#10'  f_ntext,'#13#10'  f_nume' +
      'ric_19_5,'#13#10'  f_nvarchar,'#13#10'  f_real,'#13#10'  f_smalldatetime,'#13#10'  f_sma' +
      'llint,'#13#10'  f_smallmoney,'#13#10'  --f_sql_variant,'#13#10'  f_sysname,'#13#10'  f_t' +
      'ext,'#13#10'  f_timestamp,'#13#10'  f_tinyint,'#13#10'  f_uniqueidentifier,'#13#10'  f_v' +
      'arbinary,'#13#10'  f_varchar,'#13#10'  --f_xml,'#13#10'  f_sysname_sys'#13#10'FROM'#13#10'  db' +
      'o.table_field_test1'
    MaxBlobSize = -1
    Params = <>
    SQLConnection = SQLConnection
    Left = 246
    Top = 110
  end
  object DSP: TDataSetProvider
    DataSet = SQLDataSet
    UpdateMode = upWhereKeyOnly
    OnUpdateError = DSPUpdateError
    Left = 246
    Top = 158
  end
  object CDS: TClientDataSet
    Aggregates = <>
    Params = <>
    ProviderName = 'DSP'
    AfterOpen = CDSAfterOpen
    BeforeClose = CDSBeforeClose
    BeforeApplyUpdates = CDSBeforeApplyUpdates
    Left = 248
    Top = 206
  end
  object DataSource: TDataSource
    DataSet = CDS
    Left = 248
    Top = 258
  end
end
