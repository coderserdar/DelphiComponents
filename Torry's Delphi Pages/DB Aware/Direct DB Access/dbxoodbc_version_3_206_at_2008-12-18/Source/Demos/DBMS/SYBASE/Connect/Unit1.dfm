object Form1: TForm1
  Left = 415
  Top = 322
  Width = 606
  Height = 402
  Caption = 'Demo: DbxOOdbc Sybase Connect'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    598
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
  object Grid: TDBGrid
    Left = 8
    Top = 200
    Width = 360
    Height = 160
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource
    TabOrder = 11
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object ESRV: TEdit
    Left = 100
    Top = 8
    Width = 120
    Height = 21
    TabOrder = 0
    Text = 'PSI'
  end
  object EUSER: TEdit
    Left = 100
    Top = 56
    Width = 120
    Height = 21
    TabOrder = 2
    Text = 'PSIREAD'
  end
  object EPWD: TEdit
    Left = 100
    Top = 80
    Width = 120
    Height = 21
    TabOrder = 3
    Text = 'HAUSER'
  end
  object BConnect: TButton
    Left = 28
    Top = 165
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 7
    OnClick = BConnectClick
  end
  object BDisconnect: TButton
    Left = 116
    Top = 165
    Width = 85
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 8
    OnClick = BDisconnectClick
  end
  object EDNS: TEdit
    Left = 100
    Top = 106
    Width = 120
    Height = 21
    TabOrder = 4
  end
  object CDirectOdbc: TCheckBox
    Left = 232
    Top = 109
    Width = 109
    Height = 17
    Caption = 'Direct ODBC'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object BSPExec: TButton
    Left = 288
    Top = 165
    Width = 213
    Height = 25
    Caption = 'Execute Stored Procedure'
    TabOrder = 9
    Visible = False
    OnClick = BSPExecClick
  end
  object EAdditional: TEdit
    Left = 113
    Top = 131
    Width = 472
    Height = 21
    TabOrder = 10
    Text = 'coTrimChar=True;coLockMode=60'
  end
  object EDB: TEdit
    Left = 100
    Top = 32
    Width = 120
    Height = 21
    TabOrder = 1
    Text = 'PSI'
  end
  object RServerType: TRadioGroup
    Left = 232
    Top = 7
    Width = 225
    Height = 82
    Caption = 'Server Type'
    ItemIndex = 0
    Items.Strings = (
      'Sybase System 11'
      'Adaptive Server Anywhere 8'
      'Adaptive Server Anywhere 7')
    TabOrder = 5
  end
  object SQLConnection: TSQLConnection
    LoginPrompt = False
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
