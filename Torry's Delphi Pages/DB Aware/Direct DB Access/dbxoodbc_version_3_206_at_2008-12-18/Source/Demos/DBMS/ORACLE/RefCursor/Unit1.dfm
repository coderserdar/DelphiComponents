object Form1: TForm1
  Left = 247
  Top = 211
  Caption = 'dbxoodbc demo: oracle: RefCursor'
  ClientHeight = 299
  ClientWidth = 530
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 530
    Height = 123
    Align = alTop
    TabOrder = 0
    object StaticText1: TStaticText
      Left = 9
      Top = 12
      Width = 62
      Height = 17
      Caption = 'DBX Driver: '
      TabOrder = 0
    end
    object cmbDriver: TComboBox
      Left = 77
      Top = 8
      Width = 114
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = 'OpenODBC'
      OnChange = cmbDriverChange
      Items.Strings = (
        'OpenODBC'
        'Oracle'
        'CoreLab')
    end
    object btnTest: TButton
      Left = 5
      Top = 38
      Width = 74
      Height = 25
      Caption = 'Test'
      TabOrder = 2
      OnClick = btnTestClick
    end
    object btnOpenStoredProc: TButton
      Left = 81
      Top = 38
      Width = 108
      Height = 25
      Caption = 'Open Stored Proc'
      TabOrder = 3
      OnClick = btnOpenStoredProcClick
    end
    object EStoredProcName: TRadioGroup
      Left = 195
      Top = 2
      Width = 331
      Height = 116
      Caption = 'StoredProc.StoredProcName'
      ItemIndex = 0
      Items.Strings = (
        'test_proc_refcursor_a'
        '"TEST_PROC_REFCURSOR_A"'
        '"Test_Proc_RefCursor_A"'
        'pkg_test_dbxoodbc_01.test_proc_refcursor_b'
        'pkg_test_dbxoodbc_01."TEST_PROC_REFCURSOR_B"'
        'pkg_test_dbxoodbc_01."Test_Proc_RefCursor_C"')
      TabOrder = 4
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 264
    Width = 530
    Height = 35
    Align = alBottom
    TabOrder = 1
    object btnOpen: TButton
      Left = 5
      Top = 5
      Width = 75
      Height = 25
      Caption = 'Open'
      TabOrder = 0
      OnClick = btnOpenClick
    end
    object btnNextRecordSet: TButton
      Left = 81
      Top = 5
      Width = 104
      Height = 25
      Caption = 'NextRecordSet'
      TabOrder = 1
      OnClick = btnNextRecordSetClick
    end
    object btnClose: TButton
      Left = 186
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 2
      OnClick = btnCloseClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 123
    Width = 530
    Height = 141
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'GRID'
      object DBGrid1: TDBGrid
        Left = 0
        Top = 0
        Width = 522
        Height = 113
        Align = alClient
        DataSource = DataSource1
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'LOG'
      ImageIndex = 1
      object memLog: TMemo
        Left = 0
        Top = 0
        Width = 522
        Height = 113
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Lucida Console'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object SQLConnection1: TSQLConnection
    DriverName = 'Oracle'
    GetDriverFunc = 'getSQLDriverORACLE'
    LibraryName = 'dbxora.dll'
    LoginPrompt = False
    Params.Strings = (
      'Database=TNS_ORA_DEMO'
      'User_Name=scott'
      'Password=tiger')
    VendorLib = 'oci.dll'
    Left = 80
    Top = 80
  end
  object SQLStoredProc1: TSQLStoredProc
    MaxBlobSize = -1
    Params = <>
    SQLConnection = SQLConnection1
    Left = 148
    Top = 76
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 188
    Top = 140
  end
  object DataSetProvider1: TDataSetProvider
    DataSet = SQLStoredProc1
    Left = 64
    Top = 140
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    ProviderName = 'DataSetProvider1'
    Left = 128
    Top = 172
  end
  object SQLQuery1: TSQLQuery
    MaxBlobSize = -1
    Params = <>
    SQL.Strings = (
      'select 1 s from dual')
    SQLConnection = SQLConnection1
    Left = 364
    Top = 136
  end
end
