object Form1: TForm1
  Left = 416
  Top = 317
  Width = 623
  Height = 402
  Caption = 'Demo: DbxOOdbc "MS SQL" Connect'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    615
    368)
  PixelsPerInch = 96
  TextHeight = 13
  object LSRV: TLabel
    Left = 20
    Top = 8
    Width = 69
    Height = 13
    Caption = 'SERVER NAME'
  end
  object LUSER: TLabel
    Left = 20
    Top = 60
    Width = 26
    Height = 13
    Caption = 'USER'
  end
  object LPWD: TLabel
    Left = 20
    Top = 88
    Width = 51
    Height = 13
    Caption = 'PASWORD'
  end
  object LDNS: TLabel
    Left = 20
    Top = 113
    Width = 20
    Height = 13
    Caption = 'DNS'
  end
  object LAdd: TLabel
    Left = 20
    Top = 137
    Width = 89
    Height = 13
    Caption = 'Additional options:'
  end
  object LDB: TLabel
    Left = 19
    Top = 31
    Width = 52
    Height = 13
    Caption = 'DATABASE'
  end
  object sh1: TShape
    Left = 20
    Top = 165
    Width = 24
    Height = 21
    Brush.Color = clGray
  end
  object Grid: TDBGrid
    Left = 8
    Top = 200
    Width = 360
    Height = 160
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource
    TabOrder = 16
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object ESRV1: TEdit
    Left = 100
    Top = 8
    Width = 120
    Height = 21
    TabOrder = 0
    Visible = False
  end
  object EUSER: TEdit
    Left = 100
    Top = 56
    Width = 199
    Height = 21
    TabOrder = 7
  end
  object EPWD: TEdit
    Left = 100
    Top = 79
    Width = 199
    Height = 21
    TabOrder = 9
  end
  object BConnect: TButton
    Left = 60
    Top = 165
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 13
    OnClick = BConnectClick
  end
  object BDisconnect: TButton
    Left = 148
    Top = 165
    Width = 85
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 14
    OnClick = BDisconnectClick
  end
  object EDNS: TEdit
    Left = 100
    Top = 106
    Width = 199
    Height = 21
    TabOrder = 10
  end
  object CDirectOdbc: TCheckBox
    Left = 312
    Top = 8
    Width = 85
    Height = 17
    Caption = 'Direct ODBC'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object BSPExec: TButton
    Left = 288
    Top = 165
    Width = 213
    Height = 25
    Caption = 'Execute Stored Procedure'
    TabOrder = 15
    Visible = False
    OnClick = BSPExecClick
  end
  object EAdditional: TEdit
    Left = 113
    Top = 131
    Width = 472
    Height = 21
    TabOrder = 12
    Text = 'coTrimChar=True;coLockMode=60'
  end
  object EDB: TEdit
    Left = 100
    Top = 30
    Width = 199
    Height = 21
    TabOrder = 4
  end
  object COSAuthentication: TCheckBox
    Left = 312
    Top = 59
    Width = 129
    Height = 17
    Caption = 'OS Authentication'
    Checked = True
    State = cbChecked
    TabOrder = 8
  end
  object CUnicodeDriver: TCheckBox
    Left = 403
    Top = 10
    Width = 158
    Height = 17
    Caption = 'Use Dbx3 (Unicode Driver)'
    TabOrder = 3
  end
  object CUnicodeODBCAPI: TCheckBox
    Left = 312
    Top = 34
    Width = 117
    Height = 17
    Caption = 'Unicode ODBC API'
    TabOrder = 5
  end
  object CAnsiFields: TCheckBox
    Left = 428
    Top = 34
    Width = 85
    Height = 17
    Caption = 'Ansi Fields'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object cbx_server_version: TComboBox
    Left = 423
    Top = 78
    Width = 73
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 1
    TabOrder = 11
    Text = '2005'
    Items.Strings = (
      '2007'
      '2005'
      '2000')
  end
  object txt1: TStaticText
    Left = 312
    Top = 80
    Width = 100
    Height = 17
    Caption = 'SQL Server Version:'
    TabOrder = 17
  end
  object ESRV: TComboBox
    Left = 100
    Top = 8
    Width = 200
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    Text = '127.0.0.1\SQLEXPRESS2005'
    Items.Strings = (
      '127.0.0.1'
      '127.0.0.1\SQLEXPRESS2005'
      '127.0.0.1\SQLEXPRESS')
  end
  object SQLConnection: TSQLConnection
    LoginPrompt = False
    AfterConnect = SQLConnectionAfterConnect
    AfterDisconnect = SQLConnectionAfterDisconnect
    Left = 532
    Top = 76
  end
  object SQLStoredProc: TSQLStoredProc
    MaxBlobSize = -1
    ParamCheck = False
    Params = <
      item
        DataType = ftCursor
        Name = 'Result'
        ParamType = ptResult
      end
      item
        DataType = ftDate
        Name = 'pOnDate'
        ParamType = ptInput
        Value = Null
      end
      item
        DataType = ftInteger
        Precision = 9
        Name = 'pREPORT_ID'
        ParamType = ptInput
        Size = 9
        Value = 0
      end
      item
        DataType = ftString
        Name = 'pUSER_NAME'
        ParamType = ptInput
        Value = #39#39
      end
      item
        DataType = ftInteger
        Precision = 9
        Name = 'pSHOW_SYS_USERS'
        ParamType = ptInput
        Size = 9
        Value = 1
      end>
    SQLConnection = SQLConnection
    StoredProcName = 'REPORTS_GET_LOG'
    Left = 532
    Top = 164
  end
  object DataSource: TDataSource
    DataSet = CDS
    Left = 540
    Top = 316
  end
  object DSProv: TDataSetProvider
    DataSet = SQLStoredProc
    Constraints = False
    UpdateMode = upWhereKeyOnly
    Left = 536
    Top = 216
  end
  object CDS: TClientDataSet
    Aggregates = <>
    PacketRecords = 100
    Params = <>
    ProviderName = 'DataSetProvider1'
    Left = 540
    Top = 268
  end
end
