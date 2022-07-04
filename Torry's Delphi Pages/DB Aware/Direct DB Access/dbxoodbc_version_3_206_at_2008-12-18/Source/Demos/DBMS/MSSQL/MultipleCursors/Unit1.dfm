object Form1: TForm1
  Left = 376
  Top = 270
  Caption = 'MSSQLMultipleCursors:: Demo'
  ClientHeight = 546
  ClientWidth = 655
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object p1: TPanel
    Left = 0
    Top = 0
    Width = 655
    Height = 85
    Align = alTop
    TabOrder = 0
    OnResize = p1Resize
    object sh_connection_status: TShape
      Left = 16
      Top = 54
      Width = 21
      Height = 21
      Brush.Color = clSilver
    end
    object txt1: TStaticText
      Left = 8
      Top = 8
      Width = 93
      Height = 17
      Caption = 'Connection String:'
      TabOrder = 0
    end
    object btn_sqlserver_connect: TButton
      Left = 44
      Top = 52
      Width = 75
      Height = 25
      Caption = 'Connect'
      TabOrder = 2
      OnClick = btn_sqlserver_connectClick
    end
    object btn_sqlserver_disconnect: TButton
      Left = 128
      Top = 52
      Width = 75
      Height = 25
      Caption = 'Disconnect'
      TabOrder = 3
      OnClick = btn_sqlserver_disconnectClick
    end
    object cbx_connection_string: TComboBox
      Left = 8
      Top = 22
      Width = 343
      Height = 21
      ItemHeight = 13
      ItemIndex = 6
      TabOrder = 1
      Text = 'Trusted_Connection=Yes;DATABASE=dbxoodbc;SERVER=localhost'
      Items.Strings = (
        'Trusted_Connection=Yes'
        'Trusted_Connection=Yes;;coMapInt64ToBCD=1'
        'Trusted_Connection=Yes;;coEnableUnicode=0'
        'Trusted_Connection=Yes;DATABASE=tempdb'
        'UID=user1;PWD=pwd1;DATABASE=tempdb'
        '?'
        'UID=user1;PWD=pwd1;DATABASE=dbxoodbc;SERVER=mssql-host'
        'Trusted_Connection=Yes;DATABASE=dbxoodbc;SERVER=localhost'

          'Trusted_Connection=Yes;DATABASE=dbxoodbc;SERVER=localhost;coMapI' +
          'nt64ToBCD=1')
    end
  end
  object PC: TPageControl
    Left = 0
    Top = 85
    Width = 655
    Height = 461
    ActivePage = tbsh_cds
    Align = alClient
    TabOrder = 1
    object tbsh_schema: TTabSheet
      Caption = 'Schema for testing'
      object mem_schema_make: TMemo
        Left = 0
        Top = 41
        Width = 647
        Height = 262
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        Lines.Strings = (
          '-- Create table: table 1'
          'create table table1'
          '('
          '  tab1_id   integer not null,'
          '  tab1_name varchar(64)'
          ')'
          ';'
          ''
          '-- Create/Recreate primary, unique and foreign key constraints'
          'alter table table1'
          '  add constraint pk_tab1_id primary key (tab1_id);'
          ''

            'insert into table1 (tab1_id, tab1_name) values (1, '#39'Hello table1' +
            '. # 1'#39');'

            'insert into table1 (tab1_id, tab1_name) values (2, '#39'Hello table1' +
            '. # 2'#39');'
          ''
          ''
          '-- Create table: table 2'
          'create table table2'
          '('
          '  tab2_id   integer not null,'
          '  tab2_name varchar(192)'
          ')'
          ';'
          ''
          '-- Create/Recreate primary, unique and foreign key constraints'
          'alter table table2'
          '  add constraint pk_tab2_id primary key (tab2_id);'
          ''

            'insert into table2 (tab2_id, tab2_name) values (2, '#39'Hello table2' +
            '. # 1'#39');'

            'insert into table2 (tab2_id, tab2_name) values (3, '#39'Hello table2' +
            '. # 2'#39');'
          ''
          ''
          '-- Create table: table 3'
          'create table table3'
          '('
          '  tab3_id   integer not null,'
          '  tab3_name varchar(138),'
          '  tab3_desc varchar(138)'
          ')'
          ';'
          ''
          '-- Create/Recreate primary, unique and foreign key constraints'
          'alter table table3'
          '  add constraint pk_tab3_id primary key (tab3_id);'
          ''

            'insert into table3 (tab3_id, tab3_name, tab3_desc) values (3, '#39'H' +
            'ello table3. # 1'#39', '#39'Desc 1'#39');'

            'insert into table3 (tab3_id, tab3_name, tab3_desc) values (4, '#39'H' +
            'ello table3. # 2'#39', '#39'Desc 2'#39');'

            'insert into table3 (tab3_id, tab3_name, tab3_desc) values (5, '#39'H' +
            'ello table3. # 3'#39', '#39'Desc 3'#39');')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 1
      end
      object p4: TPanel
        Left = 0
        Top = 0
        Width = 647
        Height = 41
        Align = alTop
        TabOrder = 0
        object txt2: TStaticText
          Left = 8
          Top = 8
          Width = 73
          Height = 17
          Caption = 'Create tables:'
          TabOrder = 0
        end
        object btn_schema_make: TButton
          Left = 110
          Top = 8
          Width = 200
          Height = 25
          Caption = 'Execute Script: create tables'
          TabOrder = 1
          OnClick = btn_schema_makeClick
        end
      end
      object p5: TPanel
        Left = 0
        Top = 303
        Width = 647
        Height = 41
        Align = alBottom
        TabOrder = 2
        object txt3: TStaticText
          Left = 8
          Top = 8
          Width = 63
          Height = 17
          Caption = 'Drop tables:'
          TabOrder = 0
        end
        object btn_schema_drop: TButton
          Left = 110
          Top = 8
          Width = 200
          Height = 25
          Caption = 'Execute Script: drop tables'
          TabOrder = 1
          OnClick = btn_schema_dropClick
        end
      end
      object mem_schema_drop: TMemo
        Left = 0
        Top = 344
        Width = 647
        Height = 89
        Align = alBottom
        Lines.Strings = (
          'drop table table1;'
          'drop table table2;'
          'drop table table3;')
        ScrollBars = ssBoth
        TabOrder = 3
      end
    end
    object tbsh_cursors: TTabSheet
      Caption = 'SQL Query Text'
      ImageIndex = 1
      object mem_query_cursors: TMemo
        Left = 0
        Top = 41
        Width = 647
        Height = 392
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        Lines.Strings = (
          'select * from table1;'
          'select * from table2;'
          'select * from table3;')
        ParentFont = False
        TabOrder = 1
      end
      object p2: TPanel
        Left = 0
        Top = 0
        Width = 647
        Height = 41
        Align = alTop
        TabOrder = 0
        object btn_cursors_open: TButton
          Left = 8
          Top = 8
          Width = 101
          Height = 25
          Caption = 'Open Cursors'
          TabOrder = 0
          OnClick = btn_cursors_openClick
        end
        object btn_cds_open: TButton
          Left = 115
          Top = 8
          Width = 101
          Height = 25
          Caption = 'Open CDS'
          TabOrder = 1
          OnClick = btn_cds_reopenClick
        end
      end
    end
    object tbsh_results: TTabSheet
      Caption = 'SQL Query Data/Cursors'
      ImageIndex = 2
      object p3: TPanel
        Left = 0
        Top = 0
        Width = 647
        Height = 41
        Align = alTop
        TabOrder = 0
        object btn_cursors_nextrecordset: TButton
          Left = 8
          Top = 8
          Width = 101
          Height = 25
          Caption = 'Next Recordset'
          TabOrder = 0
          OnClick = btn_cursors_nextrecordsetClick
        end
        object btn_cursors_reopen: TButton
          Left = 116
          Top = 8
          Width = 101
          Height = 25
          Caption = 'ReOpen Cursors'
          TabOrder = 1
          OnClick = btn_cursors_reopenClick
        end
        object btn_cursors_close: TButton
          Left = 224
          Top = 8
          Width = 101
          Height = 25
          Caption = 'Close Cursors'
          TabOrder = 2
          OnClick = btn_cursors_closeClick
        end
      end
      object mem_cursors_log: TMemo
        Left = 0
        Top = 41
        Width = 647
        Height = 392
        Align = alClient
        TabOrder = 1
      end
    end
    object tbsh_cds: TTabSheet
      Caption = 'ClientDataSet'
      ImageIndex = 3
      object p6: TPanel
        Left = 0
        Top = 0
        Width = 647
        Height = 41
        Align = alTop
        TabOrder = 0
        object sh2: TShape
          Left = 447
          Top = 23
          Width = 77
          Height = 8
          Brush.Color = clSilver
          Shape = stRoundRect
        end
        object btn_cds_nextrecordset: TButton
          Left = 116
          Top = 8
          Width = 101
          Height = 25
          Caption = 'Next Recordset'
          TabOrder = 1
          OnClick = btn_cds_nextrecordsetClick
        end
        object btn_cds_reopen: TButton
          Left = 9
          Top = 10
          Width = 101
          Height = 25
          Caption = 'ReOpen CDS'
          TabOrder = 0
          OnClick = btn_cds_reopenClick
        end
        object btn_cds_close: TButton
          Left = 223
          Top = 8
          Width = 101
          Height = 25
          Caption = 'Close CDS'
          TabOrder = 2
          OnClick = btn_cds_closeClick
        end
        object btn_cds_apply_changes: TButton
          Left = 330
          Top = 8
          Width = 101
          Height = 25
          Caption = 'Apply Changes'
          TabOrder = 3
          OnClick = btn_cds_apply_changesClick
        end
        object st_applying: TStaticText
          Left = 448
          Top = 6
          Width = 81
          Height = 17
          Caption = 'applying status:'
          TabOrder = 4
        end
      end
      object grd1: TDBGrid
        Left = 0
        Top = 41
        Width = 647
        Height = 355
        Align = alClient
        DataSource = DataSource_midas
        Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgConfirmDelete, dgCancelOnExit]
        TabOrder = 1
        TitleFont.Charset = RUSSIAN_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      object p7: TPanel
        Left = 0
        Top = 396
        Width = 647
        Height = 37
        Align = alBottom
        BevelOuter = bvLowered
        TabOrder = 2
        object dbnav1: TDBNavigator
          Left = 4
          Top = 6
          Width = 240
          Height = 25
          DataSource = DataSource_midas
          TabOrder = 0
        end
      end
    end
  end
  object SQLConnection: TSQLConnection
    DriverName = 'DbxSQLServerW'
    GetDriverFunc = 'getSQLDriverODBCW'
    LibraryName = 'dbxoodbc.dll'
    LoginPrompt = False
    Params.Strings = (
      'DbxSQLServer TransIsolation=ReadCommited'
      'Database=?'
      'User_Name=user'
      'Password=password'
      'RowsetSize=20'
      'BlobSize=-1'
      'Trim Char=True'

        'Custom String=coConnectionString=DATABASE=dbxoodbc;Trusted_Conne' +
        'ction=Yes;SERVER=127.0.0.1;coLockMode=17;coCatPrefix=DATABASE'
      'Prepare SQL=False')
    VendorLib = 'sqlsrv32.dll'
    AfterConnect = SQLConnectionAfterConnect
    AfterDisconnect = SQLConnectionAfterDisconnect
    BeforeDisconnect = SQLConnectionBeforeDisconnect
    Left = 372
    Top = 12
  end
  object SQLQuery: TSQLQuery
    AfterClose = SQLQueryAfterClose
    MaxBlobSize = -1
    Params = <>
    SQL.Strings = (
      'select * from table_fields_test')
    SQLConnection = SQLConnection
    Left = 448
    Top = 12
  end
  object SQLQuerySchema: TSQLQuery
    MaxBlobSize = -1
    ParamCheck = False
    Params = <>
    SQLConnection = SQLConnection
    Left = 516
    Top = 12
  end
  object SQLQuery_midas: TSQLQuery
    MaxBlobSize = -1
    Params = <>
    SQLConnection = SQLConnection
    Left = 532
    Top = 184
  end
  object DataSource_midas: TDataSource
    DataSet = CDS
    Left = 540
    Top = 349
  end
  object CDS: TClientDataSet
    Aggregates = <>
    Params = <>
    ProviderName = 'DSP'
    AfterOpen = CDSAfterOpen
    BeforeClose = CDSBeforeClose
    Left = 536
    Top = 293
  end
  object DSP: TDataSetProvider
    DataSet = SQLQuery_midas
    Left = 532
    Top = 241
  end
  object timer_sh2: TTimer
    Enabled = False
    Interval = 100
    OnTimer = timer_sh2Timer
    Left = 544
    Top = 116
  end
end
