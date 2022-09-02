object frmClient: TfrmClient
  Left = 391
  Top = 272
  Width = 466
  Height = 223
  Caption = 'FSSQL Remote Administration Client'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnUsers: TButton
    Left = 262
    Top = 14
    Width = 185
    Height = 49
    Caption = 'Users'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = btnUsersClick
  end
  object btnConf: TButton
    Left = 262
    Top = 70
    Width = 185
    Height = 49
    Caption = 'Configuration'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = btnConfClick
  end
  object btnSingle: TButton
    Left = 262
    Top = 126
    Width = 185
    Height = 49
    Caption = 'One2one'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = btnSingleClick
  end
  object GroupBox1: TGroupBox
    Left = 12
    Top = 8
    Width = 231
    Height = 167
    Caption = ' DB Login '
    TabOrder = 3
    object Label1: TLabel
      Left = 24
      Top = 56
      Width = 51
      Height = 13
      Caption = 'Username:'
    end
    object Label2: TLabel
      Left = 31
      Top = 91
      Width = 44
      Height = 13
      Caption = 'Pasword:'
    end
    object edtName: TEdit
      Left = 88
      Top = 52
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object edtPasswd: TEdit
      Left = 88
      Top = 88
      Width = 121
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
  end
  object RemoteAdmin: TFSRemoteAdminServer
    EventLog = EventLog
    AutoConnect = False
    Password = '******'
    Transport = Transport
    Left = 50
    Top = 132
  end
  object Transport: TFSParamConnect
    EventLog = EventLog
    CommandHandler = Handler
    ServerName = 'data@127.0.0.1'
    Protocol = ptTCPIP
    Left = 104
    Top = 136
  end
  object Monitor: TFSMonitor
    EventLog = EventLog
    ServerEngine = ServerEngine
    Left = 181
    Top = 136
  end
  object Handler: TFSHandler
    EventLog = EventLog
    Left = 50
    Top = 26
  end
  object ServerEngine: TFSRemoteServer
    EventLog = EventLog
    Transport = Transport
    Left = 130
    Top = 28
  end
  object EventLog: TFSEventLog
    Left = 190
    Top = 32
  end
end
