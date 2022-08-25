object SrvForm: TSrvForm
  Left = 272
  Top = 135
  Caption = 'Server'
  ClientHeight = 171
  ClientWidth = 303
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ClientListBox: TListBox
    Left = 0
    Top = 41
    Width = 303
    Height = 130
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 303
    Height = 41
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object PortEdit: TEdit
      Left = 48
      Top = 8
      Width = 73
      Height = 21
      TabOrder = 0
      Text = 'PortEdit'
    end
    object RestartButton: TButton
      Left = 128
      Top = 8
      Width = 57
      Height = 21
      Caption = '&Restart'
      Default = True
      TabOrder = 1
      OnClick = RestartButtonClick
    end
  end
  object SrvSocket: TWSocket
    LineMode = False
    LineLimit = 65536
    LineEnd = #13#10
    LineEcho = False
    LineEdit = False
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalPort = '0'
    MultiThreaded = False
    MultiCast = False
    MultiCastIpTTL = 1
    FlushTimeout = 60
    SendFlags = wsSendNormal
    LingerOnOff = wsLingerOn
    LingerTimeout = 0
    KeepAliveOnOff = wsKeepAliveOff
    KeepAliveTime = 0
    KeepAliveInterval = 0
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    LastError = 0
    ReuseAddr = False
    ComponentOptions = []
    ListenBacklog = 5
    ReqVerLow = 2
    ReqVerHigh = 2
    OnSessionAvailable = SrvSocketSessionAvailable
    Left = 32
    Top = 56
  end
  object DataTable: TTable
    DatabaseName = 'i:\cours97\delphi\reseau'
    IndexName = 'NOM'
    TableName = 'clients.dbf'
    Left = 96
    Top = 56
  end
end
