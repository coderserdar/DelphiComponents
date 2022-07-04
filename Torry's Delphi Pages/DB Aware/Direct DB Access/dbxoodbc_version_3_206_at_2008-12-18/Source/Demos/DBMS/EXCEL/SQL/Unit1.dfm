object Form1: TForm1
  Left = 281
  Top = 188
  Width = 786
  Height = 637
  Caption = 'dbxoodbx:: demo dbx3 access to Excel'
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    778
    603)
  PixelsPerInch = 96
  TextHeight = 13
  object sh1: TShape
    Left = 8
    Top = 7
    Width = 17
    Height = 17
    Brush.Color = clGray
  end
  object btn_connect: TButton
    Left = 8
    Top = 104
    Width = 75
    Height = 25
    Caption = 'ReConnect'
    TabOrder = 6
    OnClick = btn_connectClick
  end
  object btn_open_query: TButton
    Left = 89
    Top = 139
    Width = 75
    Height = 25
    Caption = 'Open Query'
    TabOrder = 9
    OnClick = btn_open_queryClick
  end
  object mem_sql_text: TMemo
    Left = 251
    Top = 28
    Width = 523
    Height = 101
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'SELECT * FROM `customer$`')
    ScrollBars = ssBoth
    TabOrder = 5
  end
  object btn_query_exec: TButton
    Left = 170
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Exec Query'
    TabOrder = 8
    OnClick = btn_query_execClick
  end
  object btn_cds_open: TButton
    Left = 89
    Top = 351
    Width = 75
    Height = 25
    Caption = 'Open CDS'
    TabOrder = 16
    OnClick = btn_cds_openClick
  end
  object dbnav1: TDBNavigator
    Left = 174
    Top = 559
    Width = 240
    Height = 25
    DataSource = DataSource
    Anchors = [akLeft, akBottom]
    TabOrder = 20
  end
  object btn_cds_close: TButton
    Left = 89
    Top = 382
    Width = 75
    Height = 25
    Caption = 'Close CDS'
    TabOrder = 17
    OnClick = btn_cds_closeClick
  end
  object btn_open_close: TButton
    Left = 89
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Close Query'
    TabOrder = 10
    OnClick = btn_open_closeClick
  end
  object btn_disconnect: TButton
    Left = 89
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 7
    OnClick = btn_disconnectClick
  end
  object btn_cds_apply: TButton
    Left = 89
    Top = 413
    Width = 75
    Height = 25
    Caption = 'Apply CDS'
    TabOrder = 18
    OnClick = btn_cds_applyClick
  end
  object chk_unicode_dbx: TCheckBox
    Left = 39
    Top = 7
    Width = 146
    Height = 17
    Caption = 'Unicode DBX ( DBX 3.0 )'
    Checked = True
    Enabled = False
    State = cbChecked
    TabOrder = 0
  end
  object chk_ansi_string: TCheckBox
    Left = 39
    Top = 51
    Width = 134
    Height = 17
    Caption = 'Ansi String Fields'
    TabOrder = 2
  end
  object chk_unicode_odbc: TCheckBox
    Left = 39
    Top = 28
    Width = 146
    Height = 17
    Caption = 'Unicode ODBC API'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object PC: TPageControl
    Left = 170
    Top = 333
    Width = 603
    Height = 216
    ActivePage = ts_grid
    TabOrder = 19
    object ts_grid: TTabSheet
      Caption = 'GRID'
      object grd1: TDBGrid
        Left = 0
        Top = 0
        Width = 595
        Height = 188
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
      object p1: TPanel
        Left = 0
        Top = 0
        Width = 595
        Height = 41
        Align = alTop
        TabOrder = 0
        object cbx_fields: TComboBox
          Left = 44
          Top = 8
          Width = 227
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = cbx_fieldsChange
        end
        object txt1: TStaticText
          Left = 6
          Top = 12
          Width = 35
          Height = 17
          Caption = 'Fields:'
          TabOrder = 1
        end
      end
      object db_memo: TDBMemo
        Left = 0
        Top = 41
        Width = 595
        Height = 147
        Align = alClient
        DataSource = DataSource
        TabOrder = 1
      end
    end
  end
  object cbx_query: TComboBox
    Left = 251
    Top = 3
    Width = 523
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 47
    ItemHeight = 13
    TabOrder = 4
    OnChange = cbx_queryChange
    Items.Strings = (
      '-- work: press "Open CDS" or "Open Query"'
      'select * from `country$`'
      'select * from `custonly$`'
      'select * from `customer$`'
      'select * from `employeer$`'
      'select * from `events$`'
      ''
      '-- not work: press "Exec Query"'
      'CREATE TABLE `sheet_x$` (`FLD_ID` INTEGER, `FLD_NAME` TEXT);'
      '-- work'
      'CREATE TABLE `sheet_x` (`FLD_ID` INTEGER, `FLD_NAME` TEXT);'
      ''
      
        '-- RECOMENDATION: after creating table need access to it with us' +
        'age sufix $'
      ''
      'insert into sheet_x(FLD_ID, FLD_NAME) values(1, '#39'A'#39')'
      'insert into `sheet_x`(`FLD_ID`, `FLD_NAME`) values(2, '#39'B'#39')'
      ''
      'insert into `sheet_x$`(FLD_ID, FLD_NAME) values(11, '#39'C'#39')'
      'insert into `sheet_x$`(FLD_ID, FLD_NAME) values(12, '#39'D'#39')'
      ''
      '-- not work'
      'insert into sheet_x(FLD_ID, FLD_NAME) values(3, '#39'A'#39')'
      '-- work'
      'insert into `sheet_x$`(FLD_ID, FLD_NAME) values(3, '#39'A'#39')'
      '-- not work'
      'delete from `sheet_x`  where  FLD_ID = 2 '
      'delete from `sheet_x$`  where  `FLD_ID` = 12'
      ''
      '-- work'
      '-- 2 rows: press "Open CDS"'
      'select * from `sheet_x`'
      '-- 5 rows'
      'select * from `sheet_x$`'
      ''
      'DROP TABLE sheet_x'
      ''
      '-- 5 rows (2 rows is empty)'
      'select * from `sheet_x$`'
      
        '-- change emty field; press  apply updates; press open CDS - 5 f' +
        'illed rows'
      '-- press reconnect; press open CDS - 4 rows'
      ''
      '-- not work, but clear all data'
      'DROP TABLE `sheet_x$`'
      ''
      '-- empty rows'
      'select * from `sheet_x$`')
  end
  object pc_log: TPageControl
    Left = 170
    Top = 139
    Width = 604
    Height = 190
    ActivePage = sh_mem_log
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 15
    object sh_mem_log: TTabSheet
      Caption = 'Log: Query'
      object mem_log: TMemo
        Left = 0
        Top = 0
        Width = 596
        Height = 162
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object sh_sql_monitor: TTabSheet
      Caption = 'Log: SQLMonitor'
      ImageIndex = 1
      object mem_sql_monitor: TMemo
        Left = 0
        Top = 0
        Width = 596
        Height = 162
        Align = alClient
        Color = clBlack
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clLime
        Font.Height = -13
        Font.Name = 'Lucida Console'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object btn_open_table: TButton
    Left = 36
    Top = 208
    Width = 128
    Height = 25
    Caption = 'Open Table `country$`'
    TabOrder = 11
    OnClick = btn_open_tableClick
  end
  object btn_get_table_list: TButton
    Left = 36
    Top = 240
    Width = 128
    Height = 25
    Caption = 'Get Table List'
    TabOrder = 12
    OnClick = btn_get_table_listClick
  end
  object chk_direct_odbc: TCheckBox
    Left = 39
    Top = 71
    Width = 134
    Height = 17
    Caption = 'Direct ODBC'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object btn_clear_log_query: TButton
    Left = 39
    Top = 271
    Width = 125
    Height = 25
    Caption = 'Clear: Log Query'
    TabOrder = 13
    OnClick = btn_clear_log_query_Click
  end
  object btn_clear_log_sqlmonitor: TButton
    Left = 39
    Top = 302
    Width = 125
    Height = 25
    Caption = 'Clear: SQLMonitor'
    TabOrder = 14
    OnClick = btn_clear_log_sqlmonitor_Click
  end
  object SQLConnection: TSQLConnection
    DriverName = 'DbxMSJetExcel'
    GetDriverFunc = 'getSQLDriverODBC'
    LibraryName = 'dbxoodbc.dll'
    LoginPrompt = False
    Params.Strings = (
      'DbxMSJetExcel TransIsolation=ReadCommited'
      
        'Database=DRIVER={Microsoft Excel Driver (*.xls)};DBQ=..\dbdemos.' +
        'xls;DefaultDir=..\;DriverId=790;MaxBufferSize=2048;PageTimeout=1' +
        '7'
      'User_Name='
      'Password='
      'RowsetSize=20'
      'BlobSize=-1'
      'Trim Char=True'
      'Custom String=coLockMode=-1;coCatPrefix=DBQ')
    TableScope = [tsTable]
    VendorLib = 'odbcjt32.dll;odbc32.dll'
    AfterConnect = SQLConnectionAfterConnect
    AfterDisconnect = SQLConnectionAfterDisconnect
    Left = 148
    Top = 28
  end
  object SQLQuery: TSQLQuery
    MaxBlobSize = -1
    Params = <>
    SQLConnection = SQLConnection
    Left = 30
    Top = 114
  end
  object CDS: TClientDataSet
    Aggregates = <>
    Params = <>
    ProviderName = 'DSP'
    AfterOpen = CDSAfterOpen
    BeforeClose = CDSBeforeClose
    Left = 30
    Top = 306
  end
  object DSP: TDataSetProvider
    DataSet = SQLQuery
    Left = 32
    Top = 164
  end
  object DataSource: TDataSource
    DataSet = CDS
    Left = 32
    Top = 372
  end
  object SQLTable: TSQLTable
    GetMetadata = False
    MaxBlobSize = -1
    SQLConnection = SQLConnection
    TableName = 'country$'
    Left = 44
    Top = 248
  end
end
