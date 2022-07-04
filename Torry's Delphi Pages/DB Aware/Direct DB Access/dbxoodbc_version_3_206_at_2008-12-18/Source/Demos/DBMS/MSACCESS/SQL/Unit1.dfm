object Form1: TForm1
  Left = 347
  Top = 173
  Width = 647
  Height = 582
  Caption = 'dbxoodbx:: demo dbx3 access to MSAccess'
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
    639
    548)
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
    Top = 77
    Width = 75
    Height = 25
    Caption = 'ReConnect'
    TabOrder = 5
    OnClick = btn_connectClick
  end
  object btn_open_query: TButton
    Left = 89
    Top = 108
    Width = 75
    Height = 25
    Caption = 'Open Query'
    TabOrder = 8
    OnClick = btn_open_queryClick
  end
  object mem_sql_text: TMemo
    Left = 251
    Top = 28
    Width = 384
    Height = 78
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'select * from dbx_test_long')
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object btn_query_exec: TButton
    Left = 170
    Top = 77
    Width = 75
    Height = 25
    Caption = 'Exec Query'
    TabOrder = 7
    OnClick = btn_query_execClick
  end
  object btn_cds_open: TButton
    Left = 89
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Open CDS'
    TabOrder = 11
    OnClick = btn_cds_openClick
  end
  object dbnav1: TDBNavigator
    Left = 174
    Top = 504
    Width = 240
    Height = 25
    DataSource = DataSource
    Anchors = [akLeft, akBottom]
    TabOrder = 15
  end
  object btn_cds_close: TButton
    Left = 89
    Top = 335
    Width = 75
    Height = 25
    Caption = 'Close CDS'
    TabOrder = 12
    OnClick = btn_cds_closeClick
  end
  object btn_open_close: TButton
    Left = 89
    Top = 190
    Width = 75
    Height = 25
    Caption = 'Close Query'
    TabOrder = 9
    OnClick = btn_open_closeClick
  end
  object btn_disconnect: TButton
    Left = 89
    Top = 77
    Width = 75
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 6
    OnClick = btn_disconnectClick
  end
  object btn_cds_apply: TButton
    Left = 89
    Top = 366
    Width = 75
    Height = 25
    Caption = 'Apply CDS'
    TabOrder = 13
    OnClick = btn_cds_applyClick
  end
  object chk_unicode_dbx: TCheckBox
    Left = 39
    Top = 7
    Width = 146
    Height = 17
    Caption = 'Unicode DBX ( DBX 3.0 )'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object chk_ansi_string: TCheckBox
    Left = 39
    Top = 51
    Width = 134
    Height = 17
    Caption = 'Ansi String'
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
    Top = 283
    Width = 465
    Height = 218
    ActivePage = ts_grid
    TabOrder = 14
    object ts_grid: TTabSheet
      Caption = 'GRID'
      object grd1: TDBGrid
        Left = 0
        Top = 0
        Width = 457
        Height = 190
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
        Width = 457
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
        Width = 457
        Height = 149
        Align = alClient
        DataSource = DataSource
        TabOrder = 1
      end
    end
  end
  object cbx_query: TComboBox
    Left = 251
    Top = 3
    Width = 384
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 3
    OnChange = cbx_queryChange
    Items.Strings = (
      'select * from customer;'
      'select * from dbx_test_char;'
      'select * from dbx_test_varchar;'
      'select * from dbx_test_short;'
      'select * from dbx_test_long;')
  end
  object pc_log: TPageControl
    Left = 170
    Top = 108
    Width = 465
    Height = 175
    ActivePage = sh_sql_monitor
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 10
    object sh_mem_log: TTabSheet
      Caption = 'Log: Query'
      object mem_log: TMemo
        Left = 0
        Top = 0
        Width = 457
        Height = 147
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
        Width = 457
        Height = 147
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Lucida Console'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object SQLConnection: TSQLConnection
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
    Top = 204
  end
  object DataSource: TDataSource
    DataSet = CDS
    Left = 32
    Top = 372
  end
end
