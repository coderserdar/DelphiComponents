object Form1: TForm1
  Left = 277
  Top = 188
  Width = 786
  Height = 615
  Caption = 'dbxoodbx:: demo dbx access to DBF'
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
    581)
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
    Left = 138
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
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'select * from table_demo')
    ParentFont = False
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
    Top = 552
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
    Left = 138
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
    Width = 604
    Height = 216
    ActivePage = ts_grid
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 19
    object ts_grid: TTabSheet
      Caption = 'GRID'
      object grd1: TDBGrid
        Left = 0
        Top = 0
        Width = 596
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
        Width = 596
        Height = 41
        Align = alTop
        TabOrder = 0
        object cbx_fields: TComboBox
          Left = 44
          Top = 8
          Width = 227
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
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
        Width = 596
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
      'select * from table_demo'
      'select * from '#1090#1072#1073#1083#1080#1094#1072'_'#1087#1088#1080#1084#1077#1088)
  end
  object pc_log: TPageControl
    Left = 216
    Top = 139
    Width = 558
    Height = 190
    ActivePage = sh_mem_log
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 15
    object sh_mem_log: TTabSheet
      Caption = 'Log: Query'
      object mem_log: TMemo
        Left = 0
        Top = 0
        Width = 550
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
        Width = 550
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
    Left = 8
    Top = 208
    Width = 205
    Height = 25
    Caption = 'Open SQL: ..\DB\sql_text_unicode.txt'
    TabOrder = 11
    OnClick = btn_open_tableClick
  end
  object btn_get_table_list: TButton
    Left = 8
    Top = 240
    Width = 205
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
    Left = 8
    Top = 271
    Width = 205
    Height = 25
    Caption = 'Clear: Log Query'
    TabOrder = 13
    OnClick = btn_clear_log_query_Click
  end
  object btn_clear_log_sqlmonitor: TButton
    Left = 8
    Top = 302
    Width = 205
    Height = 25
    Caption = 'Clear: SQLMonitor'
    TabOrder = 14
    OnClick = btn_clear_log_sqlmonitor_Click
  end
  object SQLConnection: TSQLConnection
    DriverName = 'DbxMSJetDBase'
    GetDriverFunc = 'getSQLDriverODBCAW'
    LibraryName = 'dbxoodbc.dll'
    LoginPrompt = False
    Params.Strings = (
      'DbxMSJetDBase TransIsolation=ReadCommited'
      
        'Database=DRIVER={Microsoft dBase Driver (*.dbf)};DefaultDir=C:\P' +
        'rojects\#CVS\dbxoodbc\devel\source\Demos\DBMS\DBF\DB;DriverId=53' +
        '3;MaxBufferSize=4096;MaxScanRows=16;PageTimeout=17;SafeTransacti' +
        'ons=1;Statistics=0;Threads=3;UserCommitSync=Yes'
      'RowsetSize=20'
      'BlobSize=-1'
      'Trim Char=True'
      'Custom String=coLockMode=-1;coCatPrefix=DefaultDir')
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
    SQL.Strings = (
      'select * from '#1090#1072#1073#1083#1080#1094#1072'_'#1087#1088#1080#1084#1077#1088)
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
    MaxBlobSize = -1
    SQLConnection = SQLConnection
    TableName = 'table_demo'
    Left = 44
    Top = 248
  end
end
